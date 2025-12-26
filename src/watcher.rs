use anyhow::Result;
use notify::{Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher as NotifyWatcher};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::mpsc;
use tokio::sync::Mutex;

/// File system watcher for cache invalidation
pub struct Watcher {
    watchers: Arc<Mutex<HashMap<PathBuf, (RecommendedWatcher, Instant)>>>,
    max_watchers: usize,
}

impl Watcher {
    pub fn new(max_watchers: usize) -> Self {
        Self {
            watchers: Arc::new(Mutex::new(HashMap::new())),
            max_watchers,
        }
    }

    /// Watch a directory for file changes (rename events only)
    /// Returns a channel that receives invalidation events
    pub async fn watch(&self, path: &Path) -> Result<mpsc::UnboundedReceiver<PathBuf>> {
        let path = path.to_path_buf();
        let mut watchers = self.watchers.lock().await;

        // If already watching, return None
        if watchers.contains_key(&path) {
            // Update last access time
            if let Some((_, last_access)) = watchers.get_mut(&path) {
                *last_access = Instant::now();
            }
            // Return a dummy channel since we're already watching
            let (tx, rx) = mpsc::unbounded_channel();
            drop(tx); // Close immediately
            return Ok(rx);
        }

        // Evict oldest watcher if at capacity (LRU)
        if watchers.len() >= self.max_watchers {
            if let Some(oldest_path) = watchers
                .iter()
                .min_by_key(|(_, (_, last_access))| last_access)
                .map(|(p, _)| p.clone())
            {
                watchers.remove(&oldest_path);
            }
        }

        let (tx, rx) = mpsc::unbounded_channel();
        let path_clone = path.clone();

        // Create watcher with debouncing
        let mut watcher = notify::recommended_watcher(move |res: Result<Event, _>| {
            if let Ok(event) = res {
                // Only care about rename events (create/delete/rename)
                match event.kind {
                    EventKind::Create(_) | EventKind::Remove(_) | EventKind::Modify(_) => {
                        // Send invalidation event (ignore send errors if receiver dropped)
                        let _ = tx.send(path_clone.clone());
                    }
                    _ => {}
                }
            }
        })?;

        watcher.watch(&path, RecursiveMode::Recursive)?;
        watchers.insert(path, (watcher, Instant::now()));

        Ok(rx)
    }

    /// Stop watching a directory
    pub async fn unwatch(&self, path: &Path) {
        let mut watchers = self.watchers.lock().await;
        watchers.remove(path);
    }

    /// Get number of active watchers
    pub async fn count(&self) -> usize {
        self.watchers.lock().await.len()
    }
}

/// Debouncer to coalesce rapid file system events
pub struct Debouncer {
    last_event: Arc<Mutex<Option<Instant>>>,
    delay: Duration,
}

impl Debouncer {
    pub fn new(delay_ms: u64) -> Self {
        Self {
            last_event: Arc::new(Mutex::new(None)),
            delay: Duration::from_millis(delay_ms),
        }
    }

    /// Check if enough time has passed since last event
    pub async fn should_trigger(&self) -> bool {
        let mut last = self.last_event.lock().await;
        let now = Instant::now();

        if let Some(last_time) = *last {
            if now.duration_since(last_time) < self.delay {
                return false;
            }
        }

        *last = Some(now);
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[tokio::test]
    async fn test_watch_directory() {
        let temp_dir = TempDir::new().unwrap();
        let watcher = Watcher::new(10);

        let mut rx = watcher.watch(temp_dir.path()).await.unwrap();

        // Create a file
        fs::write(temp_dir.path().join("test.txt"), "hello").unwrap();

        // Should receive invalidation event
        tokio::time::timeout(Duration::from_secs(1), rx.recv())
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn test_debouncer() {
        let debouncer = Debouncer::new(100);

        assert!(debouncer.should_trigger().await);
        assert!(!debouncer.should_trigger().await);

        tokio::time::sleep(Duration::from_millis(150)).await;
        assert!(debouncer.should_trigger().await);
    }
}
