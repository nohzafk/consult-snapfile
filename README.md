# consult-snapfile

**Instant file finding for Emacs, powered by a Rust server with caching and nucleo fuzzy matching.**

```text
First query:     ~60ms  ████████░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  (file scan + cache)
Cached queries:  <1ms   ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░  (instant)
                        ├──────────────────────────────────────────────────┤
                        0ms                    Human perception: 100ms →

Tested on a project with 4000+ files.
```

After the first query, all subsequent searches complete in sub-millisecond time - results appear before you finish typing.

## Features

- **Server-side caching** - File lists cached per project, making repeated searches instant (<1ms)
- **Auto-start server** - Server starts automatically on first use, no manual setup required
- **File watching** - Cache auto-invalidates when files are added or deleted
- **Nucleo fuzzy matching** - FZF-style scoring from [Helix editor](https://github.com/helix-editor/nucleo)
- **Match highlighting** - Matched characters highlighted in the completion UI
- **Async streaming** - Results stream in as you type via WebSocket with request cancellation
- **Self-contained** - No runtime dependency on `fd`; uses the [ignore](https://crates.io/crates/ignore) crate directly

### Why not consult-fd?

Nothing wrong with `consult-fd` — it works great. But I wanted faster file search.

With `consult-fd`, results appear **after you stop typing** because it spawns `fd` as a subprocess with debouncing to avoid rapid process spawning.

With `consult-snapfile`, results stream in **while you type**. We use the [`ignore`](https://crates.io/crates/ignore) crate directly (the same library `fd` uses internally), giving us:

- Same `.gitignore` behavior as fd
- Parallel directory walking
- No subprocess overhead
- No runtime dependency

In short: **consult-snapfile is faster**.

## Architecture

```text
┌─────────────────────────────────────────────────────────┐
│                     Emacs Client                        │
│  ┌───────────────────────────────────────────────────┐  │
│  │              consult-snapfile                     │  │
│  │         (consult async source)                    │  │
│  └───────────────────────┬───────────────────────────┘  │
│                          │ WebSocket                    │
└──────────────────────────┼──────────────────────────────┘
                           │
┌──────────────────────────┴──────────────────────────────┐
│                    Rust Server                          │
│  ┌─────────────────────────────────────────────────┐    │
│  │              File Cache (per project)           │    │
│  │         + File Watcher (auto-invalidate)        │    │
│  └──────────────────────┬──────────────────────────┘    │
│                         │                               │
│  ┌──────────────┐  ┌────┴─────┐                         │
│  │    ignore    │  │  nucleo  │                         │
│  │ (file walk)  │  │ (fuzzy)  │                         │
│  └──────────────┘  └──────────┘                         │
└─────────────────────────────────────────────────────────┘
```

## Requirements

- Emacs 28.1+ with [consult](https://github.com/minad/consult) and [websocket](https://github.com/ahyatt/emacs-websocket)
- Rust 1.70+ (for building the server)

## Installation

### 1. Install Rust

```bash
# macOS
brew install rust

# Ubuntu/Debian
sudo apt install cargo
```

### 2. Build the server

```bash
git clone https://github.com/nohzafk/consult-snapfile
cd consult-snapfile
./build.sh
```

This creates the binary at `target/release/consult-snapfile-server`.

### 3. Configure Emacs

**With use-package:**

```elisp
;; Install dependencies from MELPA
(use-package websocket :ensure t)
(use-package consult :ensure t)

(use-package consult-snapfile
  :load-path "/path/to/consult-snapfile/emacs"
  :after (consult websocket)
  ;; Optional: bind to your preferred key
  :bind ("C-c s f" . consult-snapfile))
```

**Without use-package:**

```elisp
;; Ensure websocket and consult are installed from MELPA first
(add-to-list 'load-path "/path/to/consult-snapfile/emacs")
(require 'consult-snapfile)
```

The server auto-starts on first use. It looks for the binary in:

1. Your PATH
2. `target/release/` (relative to the repo)
3. `target/debug/` (for development)

## Usage

### Find files

```text
M-x consult-snapfile
```

The server starts automatically on first use. Type to fuzzy search - results stream in as you type with nucleo scoring and match highlighting.

## Configuration

```elisp
;; Server settings
(setq consult-snapfile-server-port 9876)
(setq consult-snapfile-server-host "127.0.0.1")

;; Max results to return
(setq consult-snapfile-max-results 100)
```

## Development

### Running tests

```bash
./test.sh      # Run local tests (Rust unit + integration)
./test.sh ci   # Run full CI via act (requires Docker)
```

### GitHub Actions

Tests run automatically on push/PR. To run locally with [act](https://github.com/nektos/act):

```bash
brew install act  # macOS
./test.sh ci      # Run all CI jobs locally
```

### Project Structure

```text
consult-snapfile/
├── Cargo.toml              # Rust project config
├── src/                    # Rust server source
│   ├── main.rs             # Entry point + CLI
│   ├── server.rs           # WebSocket server
│   ├── search.rs           # Search engine + caching
│   ├── matcher.rs          # Nucleo fuzzy matching
│   ├── fd.rs               # File discovery (ignore crate)
│   ├── watcher.rs          # File watching
│   └── protocol.rs         # JSON protocol types
├── tests/                  # Rust integration tests
│   └── integration.rs
├── emacs/                  # Emacs package
│   ├── consult-snapfile.el
│   ├── consult-snapfile-server.el
│   ├── consult-snapfile-history.el
│   └── tests/              # Emacs tests
├── build.sh                # Build script
└── test.sh                 # Test runner
```

## Protocol

WebSocket JSON messages between Emacs and server:

### Request (Emacs → Server)

```json
{
  "type": "search",
  "id": "uuid",
  "mode": "files",
  "query": "main",
  "cwd": "/path/to/project",
  "options": {"max_results": 100}
}
```

### Response (Server → Emacs)

```json
{"type": "ready", "version": "0.1.0"}
{"type": "results", "id": "uuid", "items": [{"text": "src/main.rs", "score": 128, "indices": [4,5,6,7]}]}
{"type": "complete", "id": "uuid", "total": 10, "elapsed_ms": 0}
```

## License

MIT
