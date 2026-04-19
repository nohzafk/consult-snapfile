use crate::protocol::SearchMode;
use anyhow::{Context, Result};
use ignore::WalkBuilder;
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use tokio_util::sync::CancellationToken;

/// Find project entries in a directory using the ignore crate.
/// Respects .gitignore and other ignore files.
/// Returns None if cancelled.
pub async fn find_entries(
    cwd: &Path,
    mode: SearchMode,
    max_results: usize,
    token: &CancellationToken,
) -> Result<Option<Vec<String>>> {
    // Validate directory exists
    if !cwd.exists() {
        anyhow::bail!("Directory does not exist: {}", cwd.display());
    }
    if !cwd.is_dir() {
        anyhow::bail!("Path is not a directory: {}", cwd.display());
    }

    // Check cancellation before walking
    if token.is_cancelled() {
        return Ok(None);
    }

    // Clone what we need for the blocking task
    let cwd = cwd.to_path_buf();
    let cancelled = Arc::new(AtomicBool::new(false));
    let cancelled_clone = cancelled.clone();
    let token_clone = token.clone();

    // Spawn cancellation monitor
    let cancel_handle = tokio::spawn(async move {
        token_clone.cancelled().await;
        cancelled_clone.store(true, Ordering::SeqCst);
    });

    // Run blocking walk in a separate thread
    // Collect more files for cache reusability (10x max_results)
    let max_files = max_results.saturating_mul(10);
    let result = tokio::task::spawn_blocking(move || {
        let mut entries = Vec::new();

        let walker = WalkBuilder::new(&cwd)
            .hidden(false) // Include hidden files (like .config)
            .follow_links(true) // Follow symbolic links
            .git_ignore(true) // Respect .gitignore
            .git_global(true) // Respect global gitignore
            .git_exclude(true) // Respect .git/info/exclude
            .ignore(true) // Respect .ignore files
            .parents(true) // Check parent directories for ignore files
            .filter_entry(|entry| {
                // Skip .git directory (not in .gitignore but should be excluded)
                let is_git_dir = entry.file_name().to_str().map_or(false, |name| name == ".git");
                !is_git_dir
            })
            .build();

        for entry in walker {
            // Check cancellation periodically
            if cancelled.load(Ordering::SeqCst) {
                return None;
            }

            if let Ok(entry) = entry {
                if let Some(file_type) = entry.file_type() {
                    if let Ok(rel_path) = entry.path().strip_prefix(&cwd) {
                        if rel_path.as_os_str().is_empty() {
                            continue;
                        }

                        let path_str = rel_path.to_string_lossy().to_string();
                        if path_str.contains(".git/") {
                            continue;
                        }

                        let candidate = match mode {
                            SearchMode::Files if file_type.is_file() => Some(path_str),
                            SearchMode::Dirs if file_type.is_dir() => Some(format!("{}/", path_str)),
                            SearchMode::Paths if file_type.is_file() => Some(path_str),
                            SearchMode::Paths if file_type.is_dir() => Some(format!("{}/", path_str)),
                            _ => None,
                        };

                        if let Some(candidate) = candidate {
                            entries.push(candidate);

                            if entries.len() >= max_files {
                                break;
                            }
                        }
                    }
                }
            }
        }

        Some(entries)
    })
    .await
    .context("Failed to walk directory")?;

    // Cancel the monitor task
    cancel_handle.abort();

    Ok(result)
}

pub async fn find_files(
    cwd: &Path,
    max_results: usize,
    token: &CancellationToken,
) -> Result<Option<Vec<String>>> {
    find_entries(cwd, SearchMode::Files, max_results, token).await
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[tokio::test]
    async fn test_find_files_in_project() {
        let cwd = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let token = CancellationToken::new();
        let files = find_files(&cwd, 100, &token).await.unwrap();
        assert!(files.is_some());
        let files = files.unwrap();
        assert!(!files.is_empty());
        // Should find Cargo.toml
        assert!(files.iter().any(|f| f == "Cargo.toml"));
    }

    #[tokio::test]
    async fn test_invalid_directory() {
        let token = CancellationToken::new();
        let result = find_files(Path::new("/nonexistent"), 100, &token).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_find_files_cancelled() {
        let cwd = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let token = CancellationToken::new();
        token.cancel();
        let result = find_files(&cwd, 100, &token).await.unwrap();
        assert!(result.is_none(), "cancelled find should return None");
    }

    #[tokio::test]
    async fn test_respects_gitignore() {
        let cwd = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let token = CancellationToken::new();
        let files = find_files(&cwd, 1000, &token).await.unwrap().unwrap();
        // Should not include target directory files (gitignored)
        assert!(!files.iter().any(|f| f.starts_with("target/")));
        // Should not include .git directory
        assert!(!files.iter().any(|f| f.starts_with(".git/") || f.contains("/.git/")));
    }

    #[tokio::test]
    async fn test_find_directories() {
        let cwd = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        let token = CancellationToken::new();
        let dirs = find_entries(&cwd, SearchMode::Dirs, 100, &token)
            .await
            .unwrap()
            .unwrap();
        assert!(dirs.iter().any(|entry| entry == "src/"));
        assert!(dirs.iter().all(|entry| entry.ends_with('/')));
    }
}
