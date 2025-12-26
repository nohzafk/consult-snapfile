use crate::fd;
use crate::matcher::FuzzyMatcher;
use crate::protocol::MatchResult;
use crate::watcher::Watcher;
use anyhow::Result;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};
use tokio::sync::Mutex;
use tokio_util::sync::CancellationToken;

/// Cache entry for a directory's file list
#[derive(Clone)]
struct CacheEntry {
    files: Vec<String>,
    timestamp: u64,
    last_access: Instant,
}

/// Tracks fd subprocess execution state
#[derive(Default)]
struct FdState {
    in_progress: HashMap<PathBuf, bool>,
    last_invalidation: HashMap<PathBuf, u64>,
}

/// Search engine with caching and file watching
pub struct SearchEngine {
    cache: Arc<Mutex<HashMap<PathBuf, CacheEntry>>>,
    fd_state: Arc<Mutex<FdState>>,
    watcher: Arc<Watcher>,
    max_cache_size: usize,
}

impl SearchEngine {
    pub fn new(max_cache_size: usize) -> Self {
        Self {
            cache: Arc::new(Mutex::new(HashMap::new())),
            fd_state: Arc::new(Mutex::new(FdState::default())),
            watcher: Arc::new(Watcher::new(10)),
            max_cache_size,
        }
    }

    /// Execute a search query
    /// Returns None if cancelled, Some((results, elapsed_ms)) if completed
    pub async fn search(
        &self,
        query: &str,
        cwd: PathBuf,
        max_results: usize,
        token: &CancellationToken,
    ) -> Result<Option<(Vec<MatchResult>, u64)>> {
        let start = Instant::now();

        // Check cancellation before cache lookup
        if token.is_cancelled() {
            return Ok(None);
        }

        // Check cache first
        let files = if let Some(cached_files) = self.get_cache(&cwd).await {
            cached_files
        } else {
            // Check cancellation before spawning fd
            if token.is_cancelled() {
                return Ok(None);
            }
            // Cache miss - spawn fd
            match self.fetch_files(&cwd, max_results, token).await {
                Ok(Some(files)) => files,
                Ok(None) => return Ok(None), // Cancelled
                Err(e) => return Err(e),
            }
        };

        // Check cancellation before matching
        if token.is_cancelled() {
            return Ok(None);
        }

        // Fuzzy match against files
        // Create a new matcher per search to avoid mutex contention
        let mut matcher = FuzzyMatcher::new();
        let results = matcher.match_list(query, &files, max_results);

        let elapsed_ms = start.elapsed().as_millis() as u64;
        Ok(Some((results, elapsed_ms)))
    }

    /// Get files from cache if available
    async fn get_cache(&self, cwd: &PathBuf) -> Option<Vec<String>> {
        let mut cache = self.cache.lock().await;
        if let Some(entry) = cache.get_mut(cwd) {
            entry.last_access = Instant::now();
            Some(entry.files.clone())
        } else {
            None
        }
    }

    /// Fetch files from fd and cache them
    /// Returns None if cancelled
    async fn fetch_files(
        &self,
        cwd: &PathBuf,
        max_results: usize,
        token: &CancellationToken,
    ) -> Result<Option<Vec<String>>> {
        // Check if fd already in progress for this cwd
        {
            let fd_state = self.fd_state.lock().await;
            if fd_state.in_progress.get(cwd).copied().unwrap_or(false) {
                // Another request is already fetching, use empty list for now
                return Ok(Some(vec![]));
            }
        }

        // Mark fd as in progress
        {
            let mut fd_state = self.fd_state.lock().await;
            fd_state.in_progress.insert(cwd.clone(), true);
        }

        let fd_start_time = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis() as u64;

        // Start file watcher
        self.start_watching(cwd).await;

        // Spawn fd with cancellation support
        let files = match fd::find_files(cwd, max_results, token).await {
            Ok(Some(files)) => files,
            Ok(None) => {
                // Cancelled - mark fd as finished and return
                let mut fd_state = self.fd_state.lock().await;
                fd_state.in_progress.insert(cwd.clone(), false);
                return Ok(None);
            }
            Err(e) => {
                // Error - mark fd as finished and propagate
                let mut fd_state = self.fd_state.lock().await;
                fd_state.in_progress.insert(cwd.clone(), false);
                return Err(e);
            }
        };

        // Mark fd as finished
        {
            let mut fd_state = self.fd_state.lock().await;
            fd_state.in_progress.insert(cwd.clone(), false);
        }

        // Cache the results (with race protection)
        self.set_cache(cwd, files.clone(), fd_start_time).await;

        Ok(Some(files))
    }

    /// Set cache with race condition protection
    async fn set_cache(&self, cwd: &PathBuf, files: Vec<String>, fd_start_time: u64) {
        let mut cache = self.cache.lock().await;
        let fd_state = self.fd_state.lock().await;

        // Check if directory was invalidated after fd started
        if let Some(&last_invalidation) = fd_state.last_invalidation.get(cwd) {
            if last_invalidation > fd_start_time {
                // Stale results, don't cache
                return;
            }
        }

        // LRU eviction if at capacity
        if cache.len() >= self.max_cache_size && !cache.contains_key(cwd) {
            if let Some(oldest_path) = cache
                .iter()
                .min_by_key(|(_, entry)| entry.last_access)
                .map(|(p, _)| p.clone())
            {
                cache.remove(&oldest_path);
            }
        }

        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis() as u64;

        cache.insert(
            cwd.clone(),
            CacheEntry {
                files,
                timestamp,
                last_access: Instant::now(),
            },
        );
    }

    /// Start watching a directory for file changes
    async fn start_watching(&self, cwd: &PathBuf) {
        let cwd = cwd.clone();
        let cache = self.cache.clone();
        let fd_state = self.fd_state.clone();

        let mut rx = match self.watcher.watch(&cwd).await {
            Ok(rx) => rx,
            Err(_) => return, // Silently fail if can't watch
        };

        tokio::spawn(async move {
            let mut last_event: Option<Instant> = None;
            let debounce_delay = Duration::from_millis(100);

            while rx.recv().await.is_some() {
                // Debounce rapid events
                let now = Instant::now();
                if let Some(last_time) = last_event {
                    if now.duration_since(last_time) < debounce_delay {
                        continue;
                    }
                }
                last_event = Some(now);

                // Mark as invalidated
                let timestamp = SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .unwrap()
                    .as_millis() as u64;

                {
                    let mut state = fd_state.lock().await;
                    state.last_invalidation.insert(cwd.clone(), timestamp);
                }

                // Invalidate cache
                let mut cache_guard = cache.lock().await;
                cache_guard.remove(&cwd);
            }
        });
    }

    /// Invalidate cache for a directory (for testing)
    pub async fn invalidate_cache(&self, cwd: &PathBuf) {
        let mut cache = self.cache.lock().await;
        cache.remove(cwd);
    }

    /// Get cache statistics
    pub async fn cache_stats(&self) -> (usize, usize) {
        let cache = self.cache.lock().await;
        (cache.len(), self.max_cache_size)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_search_with_cache() {
        let engine = SearchEngine::new(10);
        let cwd = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
        let token = CancellationToken::new();

        // First search (cache miss)
        let result1 = engine.search("src", cwd.clone(), 10, &token).await.unwrap();
        let (results1, _elapsed1) = result1.expect("search should not be cancelled");
        assert!(!results1.is_empty());

        // Second search (cache hit - should be faster)
        let result2 = engine.search("test", cwd, 10, &token).await.unwrap();
        let (results2, _elapsed2) = result2.expect("search should not be cancelled");
        assert!(!results2.is_empty());
        // Cache hit should be significantly faster (but not guaranteed in tests)
    }

    #[tokio::test]
    async fn test_cache_invalidation() {
        let engine = SearchEngine::new(10);
        let cwd = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
        let token = CancellationToken::new();

        // Populate cache
        engine.search("src", cwd.clone(), 10, &token).await.unwrap();
        assert_eq!(engine.cache_stats().await.0, 1);

        // Invalidate
        engine.invalidate_cache(&cwd).await;
        assert_eq!(engine.cache_stats().await.0, 0);
    }

    #[tokio::test]
    async fn test_search_cancellation() {
        let engine = SearchEngine::new(10);
        let cwd = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
        let token = CancellationToken::new();

        // Cancel before search
        token.cancel();

        let result = engine.search("src", cwd, 10, &token).await.unwrap();
        assert!(result.is_none(), "cancelled search should return None");
    }
}
