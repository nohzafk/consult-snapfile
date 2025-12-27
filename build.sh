#!/bin/bash
# Build script for consult-snapfile

set -e

cd "$(dirname "$0")"
PROJECT_ROOT=$(pwd)

echo "=== consult-snapfile Build ==="
echo ""

# Check dependencies
echo "Checking dependencies..."

check_cmd() {
    if ! command -v "$1" &> /dev/null; then
        echo "ERROR: $1 is not installed"
        exit 1
    fi
    echo "  ✓ $1"
}

check_cmd cargo

echo ""
echo "All dependencies found!"
echo ""

# Build Rust server
echo "Building Rust server..."
cargo build --release

echo ""
echo "=== Build Complete ==="
echo ""
echo "Binary location:"
echo "  $PROJECT_ROOT/target/release/consult-snapfile-server"
echo ""
echo "To start the server:"
echo "  ./target/release/consult-snapfile-server"
echo ""
echo "To use in Emacs, add to your config:"
echo "  (add-to-list 'load-path \"$PROJECT_ROOT/emacs\")"
echo "  (require 'consult-snapfile)"
echo ""
