mod fd;
mod matcher;
mod protocol;
mod search;
mod server;
mod watcher;

use anyhow::Result;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "consult-snapfile-server")]
#[command(version, about = "Fast file search server for Emacs consult", long_about = None)]
struct Args {
    /// Host to bind to
    #[arg(long, default_value = "127.0.0.1")]
    host: String,

    /// Port to listen on
    #[arg(long, default_value_t = 9876)]
    port: u16,

    /// Maximum number of directories to cache
    #[arg(long, default_value_t = 10)]
    max_cache: usize,
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();

    let server = server::Server::new(&args.host, args.port, args.max_cache)?;
    server.run().await?;

    Ok(())
}
