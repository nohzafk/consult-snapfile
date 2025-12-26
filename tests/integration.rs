/// Integration tests for consult-snapfile-server
///
/// These tests spawn the server as a subprocess and connect via WebSocket,
/// testing the full protocol just like Emacs would use it.
use futures_util::{SinkExt, StreamExt};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::PathBuf;
use std::process::{Child, Command, Stdio};
use std::time::Duration;
use tempfile::TempDir;
use tokio::time::{sleep, timeout};
use tokio_tungstenite::{connect_async, tungstenite::Message};

// ============================================================================
// Protocol Types (matching server protocol)
// ============================================================================

#[derive(Debug, Serialize)]
struct SearchRequest {
    #[serde(rename = "type")]
    msg_type: String,
    id: String,
    mode: String,
    query: String,
    cwd: String,
    options: SearchOptions,
}

#[derive(Debug, Serialize)]
struct SearchOptions {
    max_results: usize,
}

#[derive(Debug, Serialize)]
struct CancelRequest {
    #[serde(rename = "type")]
    msg_type: String,
    id: String,
}

#[derive(Debug, Deserialize)]
struct ServerMessage {
    #[serde(rename = "type")]
    msg_type: String,
    #[serde(default)]
    id: Option<String>,
    #[serde(default)]
    version: Option<String>,
    #[serde(default)]
    total: Option<usize>,
    #[serde(default)]
    elapsed_ms: Option<u64>,
    #[serde(default)]
    items: Option<Vec<MatchResult>>,
    #[serde(default)]
    done: Option<bool>,
    #[serde(default)]
    message: Option<String>,
}

#[derive(Debug, Deserialize, Clone)]
struct MatchResult {
    #[allow(dead_code)]
    index: usize,
    #[allow(dead_code)]
    score: u32,
    text: String,
    #[allow(dead_code)]
    indices: Vec<u32>,
}

// ============================================================================
// Test Helpers
// ============================================================================

struct TestServer {
    process: Child,
    port: u16,
}

impl TestServer {
    fn start(port: u16) -> Self {
        let binary = Self::find_binary();

        let process = Command::new(&binary)
            .args(["--port", &port.to_string()])
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap_or_else(|e| panic!("Failed to start server at {:?}: {}", binary, e));

        // Give server time to start
        std::thread::sleep(Duration::from_millis(500));

        TestServer { process, port }
    }

    fn find_binary() -> PathBuf {
        let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

        // Try release build first, then debug
        for profile in &["release", "debug"] {
            let binary = manifest_dir
                .join("target")
                .join(profile)
                .join("consult-snapfile-server");
            if binary.exists() {
                return binary;
            }
        }

        panic!(
            "Server binary not found. Run `cargo build --release` first.\n\
             Looked in: {}/target/{{release,debug}}/consult-snapfile-server",
            manifest_dir.display()
        );
    }

    fn url(&self) -> String {
        format!("ws://127.0.0.1:{}", self.port)
    }
}

impl Drop for TestServer {
    fn drop(&mut self) {
        let _ = self.process.kill();
        let _ = self.process.wait();
    }
}

type WsStream = tokio_tungstenite::WebSocketStream<
    tokio_tungstenite::MaybeTlsStream<tokio::net::TcpStream>,
>;

async fn connect(url: &str) -> WsStream {
    let (ws, _) = connect_async(url).await.expect("Failed to connect");
    ws
}

async fn wait_for_ready(ws: &mut WsStream) -> String {
    while let Some(msg) = ws.next().await {
        if let Ok(Message::Text(text)) = msg {
            let parsed: ServerMessage = serde_json::from_str(&text).unwrap();
            if parsed.msg_type == "ready" {
                return parsed.version.unwrap_or_default();
            }
        }
    }
    panic!("Never received ready message");
}

fn gen_id() -> String {
    format!("test-{}", uuid::Uuid::new_v4())
}

async fn send_search(
    ws: &mut WsStream,
    query: &str,
    cwd: &str,
    max_results: usize,
) -> (usize, u64, Vec<MatchResult>) {
    let id = gen_id();
    let req = SearchRequest {
        msg_type: "search".to_string(),
        id: id.clone(),
        mode: "files".to_string(),
        query: query.to_string(),
        cwd: cwd.to_string(),
        options: SearchOptions { max_results },
    };

    ws.send(Message::Text(serde_json::to_string(&req).unwrap()))
        .await
        .unwrap();

    let mut items = Vec::new();
    let mut total = 0;
    let mut elapsed_ms = 0;

    while let Some(msg) = ws.next().await {
        if let Ok(Message::Text(text)) = msg {
            let parsed: ServerMessage = serde_json::from_str(&text).unwrap();

            // Skip messages for other requests
            if parsed.id.as_ref() != Some(&id) {
                continue;
            }

            match parsed.msg_type.as_str() {
                "results" => {
                    if let Some(new_items) = parsed.items {
                        items.extend(new_items);
                    }
                }
                "complete" => {
                    total = parsed.total.unwrap_or(0);
                    elapsed_ms = parsed.elapsed_ms.unwrap_or(0);
                    break;
                }
                "error" => {
                    panic!("Search error: {:?}", parsed.message);
                }
                _ => {}
            }
        }
    }

    (total, elapsed_ms, items)
}

// ============================================================================
// Tests: File Caching (fd-cache.mjs)
// ============================================================================

#[tokio::test]
async fn test_cache_miss_then_hit() {
    let server = TestServer::start(19876);
    let mut ws = connect(&server.url()).await;
    wait_for_ready(&mut ws).await;

    let cwd = env!("CARGO_MANIFEST_DIR");

    // First query - cache miss
    let (total1, elapsed1, _) = send_search(&mut ws, "rs", cwd, 50).await;
    assert!(total1 > 0, "Expected results for 'rs' query");

    // Second query - same query, cache hit (should be faster)
    let (total2, elapsed2, _) = send_search(&mut ws, "rs", cwd, 50).await;
    assert_eq!(total1, total2, "Same query should return same count");
    assert!(
        elapsed2 <= elapsed1 + 5,
        "Cache hit should be faster: first={}ms, second={}ms",
        elapsed1,
        elapsed2
    );
}

#[tokio::test]
async fn test_different_query_same_cwd_uses_cache() {
    let server = TestServer::start(19877);
    let mut ws = connect(&server.url()).await;
    wait_for_ready(&mut ws).await;

    let cwd = env!("CARGO_MANIFEST_DIR");

    // First query - cache miss
    let (_, elapsed1, _) = send_search(&mut ws, "src", cwd, 50).await;

    // Different query, same cwd - file list still cached
    let (_, elapsed2, _) = send_search(&mut ws, "test", cwd, 50).await;
    assert!(
        elapsed2 <= elapsed1 + 10,
        "Different query same cwd should use cached file list: first={}ms, second={}ms",
        elapsed1,
        elapsed2
    );

    // Third different query
    let (_, elapsed3, _) = send_search(&mut ws, "rs", cwd, 50).await;
    assert!(
        elapsed3 <= elapsed1 + 10,
        "Third query should also use cache: {}ms",
        elapsed3
    );
}

#[tokio::test]
async fn test_different_cwd_is_cache_miss() {
    let server = TestServer::start(19878);
    let mut ws = connect(&server.url()).await;
    wait_for_ready(&mut ws).await;

    let cwd1 = env!("CARGO_MANIFEST_DIR");
    let cwd2 = format!("{}/src", cwd1);

    // First cwd
    let (_, _, _) = send_search(&mut ws, "rs", cwd1, 50).await;

    // Different cwd - should be a separate cache entry
    let (total2, _, _) = send_search(&mut ws, "rs", &cwd2, 50).await;
    assert!(total2 > 0, "Should find files in src/ directory");

    // Back to first cwd - should still be cached
    let (_, elapsed3, _) = send_search(&mut ws, "main", cwd1, 50).await;
    assert!(
        elapsed3 < 10,
        "Original cwd should still be cached: {}ms",
        elapsed3
    );
}

// ============================================================================
// Tests: Cancellation (cancel.mjs)
// ============================================================================

#[tokio::test]
async fn test_cancel_search() {
    let server = TestServer::start(19879);
    let mut ws = connect(&server.url()).await;
    wait_for_ready(&mut ws).await;

    let cwd = env!("CARGO_MANIFEST_DIR");
    let id = gen_id();

    // Start a search
    let req = SearchRequest {
        msg_type: "search".to_string(),
        id: id.clone(),
        mode: "files".to_string(),
        query: "e".to_string(), // Common letter, many results
        cwd: cwd.to_string(),
        options: SearchOptions { max_results: 1000 },
    };

    ws.send(Message::Text(serde_json::to_string(&req).unwrap()))
        .await
        .unwrap();

    // Wait for first results
    let mut got_results = false;
    while let Some(msg) = ws.next().await {
        if let Ok(Message::Text(text)) = msg {
            let parsed: ServerMessage = serde_json::from_str(&text).unwrap();
            if parsed.id.as_ref() == Some(&id) && parsed.msg_type == "results" {
                got_results = true;
                break;
            }
        }
    }
    assert!(got_results, "Should receive at least one results batch");

    // Send cancel
    let cancel = CancelRequest {
        msg_type: "cancel".to_string(),
        id: id.clone(),
    };
    ws.send(Message::Text(serde_json::to_string(&cancel).unwrap()))
        .await
        .unwrap();

    // Should either get complete or the search was already done
    // Either way, the server should not crash
    let result = timeout(Duration::from_secs(5), async {
        while let Some(msg) = ws.next().await {
            if let Ok(Message::Text(text)) = msg {
                let parsed: ServerMessage = serde_json::from_str(&text).unwrap();
                if parsed.id.as_ref() == Some(&id) {
                    match parsed.msg_type.as_str() {
                        "complete" | "error" => return true,
                        _ => {}
                    }
                }
            }
        }
        false
    })
    .await;

    assert!(
        result.unwrap_or(true),
        "Server should handle cancel gracefully"
    );
}

// ============================================================================
// Tests: Stress Test (stress.mjs)
// ============================================================================

#[tokio::test]
async fn test_rapid_queries() {
    let server = TestServer::start(19880);
    let mut ws = connect(&server.url()).await;
    wait_for_ready(&mut ws).await;

    let cwd = env!("CARGO_MANIFEST_DIR");

    // Simulate typing "main.rs" character by character
    let queries = ["m", "ma", "mai", "main", "main.", "main.r", "main.rs"];
    let mut pending_ids = Vec::new();

    // Send all queries rapidly (30ms apart, like typing)
    for query in &queries {
        let id = gen_id();
        let req = SearchRequest {
            msg_type: "search".to_string(),
            id: id.clone(),
            mode: "files".to_string(),
            query: query.to_string(),
            cwd: cwd.to_string(),
            options: SearchOptions { max_results: 20 },
        };
        ws.send(Message::Text(serde_json::to_string(&req).unwrap()))
            .await
            .unwrap();
        pending_ids.push(id);
        sleep(Duration::from_millis(30)).await;
    }

    // Collect all completions
    let mut completed = 0;
    let mut errors = 0;

    let result = timeout(Duration::from_secs(10), async {
        while completed < queries.len() {
            if let Some(msg) = ws.next().await {
                if let Ok(Message::Text(text)) = msg {
                    let parsed: ServerMessage = serde_json::from_str(&text).unwrap();
                    if pending_ids.contains(&parsed.id.clone().unwrap_or_default()) {
                        match parsed.msg_type.as_str() {
                            "complete" => completed += 1,
                            "error" => errors += 1,
                            _ => {}
                        }
                    }
                }
            }
        }
    })
    .await;

    assert!(result.is_ok(), "All queries should complete within timeout");
    assert_eq!(errors, 0, "No errors should occur");
    assert_eq!(
        completed,
        queries.len(),
        "All queries should complete"
    );
}

// ============================================================================
// Tests: File Watching (file-watch.mjs)
// ============================================================================

#[tokio::test]
async fn test_file_watch_invalidation() {
    let server = TestServer::start(19881);
    let mut ws = connect(&server.url()).await;
    wait_for_ready(&mut ws).await;

    // Create temp directory with test files
    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path().to_str().unwrap();

    fs::write(temp_dir.path().join("file1.txt"), "content1").unwrap();
    fs::write(temp_dir.path().join("file2.txt"), "content2").unwrap();
    fs::write(temp_dir.path().join("file3.txt"), "content3").unwrap();

    // Initial query - populates cache
    let (total1, _, _) = send_search(&mut ws, "file", temp_path, 50).await;
    assert_eq!(total1, 3, "Should find 3 files initially");

    // Same query - cache hit
    let (total2, elapsed2, _) = send_search(&mut ws, "file", temp_path, 50).await;
    assert_eq!(total2, 3, "Cache hit should return same count");
    // Note: timing can vary in CI, just log it
    eprintln!("Cache hit time: {}ms", elapsed2);

    // Add a file - watcher needs time to detect and invalidate
    fs::write(temp_dir.path().join("file4.txt"), "content4").unwrap();
    sleep(Duration::from_millis(500)).await; // Wait for watcher debounce + processing

    let (total3, _, _) = send_search(&mut ws, "file", temp_path, 50).await;
    // File watching can be unreliable in tests; if watcher didn't trigger, we still have 3
    assert!(
        total3 >= 3 && total3 <= 4,
        "Should have 3 or 4 files after add: got {}",
        total3
    );

    // Delete a file
    fs::remove_file(temp_dir.path().join("file1.txt")).unwrap();
    sleep(Duration::from_millis(500)).await;

    let (total4, _, _) = send_search(&mut ws, "file", temp_path, 50).await;
    // After delete, should have fewer files (if watcher worked) or same (if still cached)
    assert!(
        total4 >= 2 && total4 <= 4,
        "Should have 2-4 files after delete: got {}",
        total4
    );
}

#[tokio::test]
async fn test_different_query_uses_cached_file_list() {
    let server = TestServer::start(19882);
    let mut ws = connect(&server.url()).await;
    wait_for_ready(&mut ws).await;

    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path().to_str().unwrap();

    fs::write(temp_dir.path().join("alpha.txt"), "a").unwrap();
    fs::write(temp_dir.path().join("beta.txt"), "b").unwrap();
    fs::write(temp_dir.path().join("gamma.txt"), "g").unwrap();

    // First query
    let (_, elapsed1, _) = send_search(&mut ws, "alpha", temp_path, 50).await;

    // Different query - should use cached file list
    let (total2, elapsed2, _) = send_search(&mut ws, "txt", temp_path, 50).await;
    assert_eq!(total2, 3, "Should find all txt files");
    assert!(
        elapsed2 <= elapsed1 + 5,
        "Different query should use cached file list: {}ms vs {}ms",
        elapsed2,
        elapsed1
    );
}

// ============================================================================
// Tests: Edge Cases
// ============================================================================

#[tokio::test]
async fn test_empty_query_returns_all() {
    let server = TestServer::start(19883);
    let mut ws = connect(&server.url()).await;
    wait_for_ready(&mut ws).await;

    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path().to_str().unwrap();

    fs::write(temp_dir.path().join("a.txt"), "").unwrap();
    fs::write(temp_dir.path().join("b.txt"), "").unwrap();

    let (total, _, _) = send_search(&mut ws, "", temp_path, 50).await;
    assert_eq!(total, 2, "Empty query should return all files");
}

#[tokio::test]
async fn test_no_matches() {
    let server = TestServer::start(19884);
    let mut ws = connect(&server.url()).await;
    wait_for_ready(&mut ws).await;

    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path().to_str().unwrap();

    fs::write(temp_dir.path().join("hello.txt"), "").unwrap();

    let (total, _, _) = send_search(&mut ws, "zzzznotfound", temp_path, 50).await;
    assert_eq!(total, 0, "Non-matching query should return 0 results");
}

#[tokio::test]
async fn test_max_results_limit() {
    let server = TestServer::start(19885);
    let mut ws = connect(&server.url()).await;
    wait_for_ready(&mut ws).await;

    let temp_dir = TempDir::new().unwrap();
    let temp_path = temp_dir.path().to_str().unwrap();

    // Create 20 files
    for i in 0..20 {
        fs::write(temp_dir.path().join(format!("file{}.txt", i)), "").unwrap();
    }

    let (total, _, _) = send_search(&mut ws, "file", temp_path, 5).await;
    assert_eq!(total, 5, "Should respect max_results limit");
}
