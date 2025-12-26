use crate::protocol::{Request, Response};
use crate::search::SearchEngine;
use anyhow::Result;
use futures_util::{SinkExt, StreamExt};
use std::collections::HashMap;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::net::{TcpListener, TcpStream};
use tokio::sync::Mutex;
use tokio_tungstenite::{accept_async, tungstenite::Message};
use tokio_util::sync::CancellationToken;

/// WebSocket server for consult-snapfile
pub struct Server {
    addr: SocketAddr,
    search_engine: Arc<SearchEngine>,
}

impl Server {
    pub fn new(host: &str, port: u16, max_cache_size: usize) -> Result<Self> {
        let addr = format!("{}:{}", host, port).parse()?;
        Ok(Self {
            addr,
            search_engine: Arc::new(SearchEngine::new(max_cache_size)),
        })
    }

    pub async fn run(&self) -> Result<()> {
        let listener = TcpListener::bind(&self.addr).await?;
        println!(
            "consult-snapfile server listening on ws://{}",
            self.addr
        );

        loop {
            match listener.accept().await {
                Ok((stream, peer_addr)) => {
                    let engine = self.search_engine.clone();
                    tokio::spawn(async move {
                        if let Err(e) = handle_connection(stream, peer_addr, engine).await {
                            eprintln!("Connection error from {}: {}", peer_addr, e);
                        }
                    });
                }
                Err(e) => {
                    eprintln!("Failed to accept connection: {}", e);
                }
            }
        }
    }
}

async fn handle_connection(
    stream: TcpStream,
    peer_addr: SocketAddr,
    engine: Arc<SearchEngine>,
) -> Result<()> {
    let ws_stream = accept_async(stream).await?;
    let (write, mut read) = ws_stream.split();
    let write = Arc::new(Mutex::new(write));

    let conn_id = uuid::Uuid::new_v4().to_string();
    println!("Client connected: {} ({})", peer_addr, &conn_id[..8]);

    // Send ready message
    let ready = Response::Ready {
        version: env!("CARGO_PKG_VERSION").to_string(),
    };
    let ready_json = serde_json::to_string(&ready)?;
    write.lock().await.send(Message::Text(ready_json)).await?;

    // Active requests with cancellation tokens
    let active_requests: Arc<Mutex<HashMap<String, CancellationToken>>> =
        Arc::new(Mutex::new(HashMap::new()));

    while let Some(msg) = read.next().await {
        let msg = match msg {
            Ok(m) => m,
            Err(e) => {
                eprintln!("WebSocket error: {}", e);
                break;
            }
        };

        if let Message::Text(text) = msg {
            let engine = engine.clone();
            let active_requests = active_requests.clone();
            let write = write.clone();

            // Parse request first to handle Cancel immediately
            let request: Request = match serde_json::from_str(&text) {
                Ok(r) => r,
                Err(e) => {
                    let error = Response::Error {
                        id: None,
                        message: format!("Invalid request: {}", e),
                    };
                    if let Ok(error_json) = serde_json::to_string(&error) {
                        let _ = write.lock().await.send(Message::Text(error_json)).await;
                    }
                    continue;
                }
            };

            match request {
                Request::Search {
                    id,
                    mode,
                    query,
                    cwd,
                    options,
                } => {
                    // Create cancellation token for this request
                    let token = CancellationToken::new();
                    {
                        let mut active = active_requests.lock().await;
                        active.insert(id.clone(), token.clone());
                    }

                    // Spawn search task so we can continue processing messages
                    tokio::spawn(async move {
                        let result = handle_search(
                            id.clone(),
                            mode,
                            query,
                            cwd,
                            options.max_results,
                            engine,
                            token.clone(),
                            write,
                        )
                        .await;

                        if let Err(e) = result {
                            eprintln!("Search error for {}: {}", id, e);
                        }

                        // Remove from active requests
                        let mut active = active_requests.lock().await;
                        active.remove(&id);
                    });
                }
                Request::Cancel { id } => {
                    // Cancel the request if it's still active
                    let active = active_requests.lock().await;
                    if let Some(token) = active.get(&id) {
                        token.cancel();
                    }
                }
            }
        }
    }

    // Cancel all active requests on disconnect
    {
        let active = active_requests.lock().await;
        for (_, token) in active.iter() {
            token.cancel();
        }
    }

    println!("Client disconnected: {}", peer_addr);
    Ok(())
}

type WebSocketWriter = Arc<
    Mutex<
        futures_util::stream::SplitSink<tokio_tungstenite::WebSocketStream<TcpStream>, Message>,
    >,
>;

async fn handle_search(
    id: String,
    _mode: String,
    query: String,
    cwd: String,
    max_results: usize,
    engine: Arc<SearchEngine>,
    token: CancellationToken,
    write: WebSocketWriter,
) -> Result<()> {
    let cwd_path = PathBuf::from(cwd);

    // Check cancellation before starting
    if token.is_cancelled() {
        return Ok(());
    }

    match engine.search(&query, cwd_path, max_results, &token).await {
        Ok(Some((results, elapsed_ms))) => {
            // Send results
            let response = Response::Results {
                id: id.clone(),
                source: "files".to_string(),
                items: results.clone(),
                done: true,
            };
            let json = serde_json::to_string(&response)?;
            write.lock().await.send(Message::Text(json)).await?;

            // Send complete
            let complete = Response::Complete {
                id: id.clone(),
                total: results.len(),
                elapsed_ms,
            };
            let complete_json = serde_json::to_string(&complete)?;
            write.lock().await.send(Message::Text(complete_json)).await?;
        }
        Ok(None) => {
            // Cancelled - no response needed
        }
        Err(e) => {
            // Only send error if not cancelled
            if !token.is_cancelled() {
                let error = Response::Error {
                    id: Some(id.clone()),
                    message: format!("{}", e),
                };
                let error_json = serde_json::to_string(&error)?;
                write.lock().await.send(Message::Text(error_json)).await?;
            }
        }
    }

    Ok(())
}
