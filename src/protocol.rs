use serde::{Deserialize, Serialize};

/// Incoming request from Emacs client
#[derive(Debug, Deserialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum Request {
    #[serde(rename = "search")]
    Search {
        id: String,
        mode: String,
        query: String,
        cwd: String,
        options: SearchOptions,
    },
    #[serde(rename = "cancel")]
    Cancel { id: String },
}

#[derive(Debug, Deserialize)]
pub struct SearchOptions {
    #[serde(default = "default_max_results")]
    pub max_results: usize,
}

fn default_max_results() -> usize {
    100
}

/// Outgoing response to Emacs client
#[derive(Debug, Serialize)]
#[serde(tag = "type", rename_all = "lowercase")]
pub enum Response {
    #[serde(rename = "ready")]
    Ready { version: String },

    #[serde(rename = "results")]
    Results {
        id: String,
        source: String,
        items: Vec<MatchResult>,
        done: bool,
    },

    #[serde(rename = "complete")]
    Complete {
        id: String,
        total: usize,
        elapsed_ms: u64,
    },

    #[serde(rename = "error")]
    Error {
        #[serde(skip_serializing_if = "Option::is_none")]
        id: Option<String>,
        message: String,
    },
}

/// A single fuzzy match result
#[derive(Debug, Serialize, Clone)]
pub struct MatchResult {
    pub index: usize,
    pub score: u32,
    pub text: String,
    pub indices: Vec<u32>,
}
