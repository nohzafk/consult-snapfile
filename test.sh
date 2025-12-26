#!/bin/bash
# Run tests for consult-snapfile
#
# Usage:
#   ./test.sh       # Run local tests (Rust unit + integration)
#   ./test.sh ci    # Run full CI via act (requires Docker)

set -e

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$PROJECT_DIR"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

run_local_tests() {
    echo -e "${YELLOW}╔══════════════════════════════════════════╗${NC}"
    echo -e "${YELLOW}║   Local Tests                            ║${NC}"
    echo -e "${YELLOW}╚══════════════════════════════════════════╝${NC}\n"

    local passed=0
    local failed=0

    run_test() {
        local name="$1"
        shift
        echo -e "${BLUE}━━━ $name ━━━${NC}"
        if "$@"; then
            echo -e "${GREEN}✓ $name passed${NC}\n"
            passed=$((passed + 1))
        else
            echo -e "${RED}✗ $name failed${NC}\n"
            failed=$((failed + 1))
        fi
    }

    # Build release binary (needed for integration tests)
    echo -e "${BLUE}━━━ Building release binary ━━━${NC}"
    cargo build --release 2>&1 | tail -5
    echo ""

    run_test "Rust Tests" bash -c "cargo test -- --test-threads=1 2>&1 | tail -30"

    echo -e "${YELLOW}╔══════════════════════════════════════════╗${NC}"
    echo -e "${YELLOW}║   Summary                                ║${NC}"
    echo -e "${YELLOW}╚══════════════════════════════════════════╝${NC}"
    echo -e "  ${GREEN}Passed: $passed${NC}"
    echo -e "  ${RED}Failed: $failed${NC}"

    return $failed
}

run_ci() {
    if ! command -v act &> /dev/null; then
        echo "Error: 'act' is not installed"
        echo ""
        echo "Install with:"
        echo "  macOS:  brew install act"
        echo "  Linux:  curl -s https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash"
        echo ""
        echo "See: https://github.com/nektos/act"
        exit 1
    fi

    if ! docker info &> /dev/null; then
        echo "Error: Docker is not running"
        echo "Please start Docker and try again"
        exit 1
    fi

    local log_file="/tmp/act-ci-$$.log"
    echo "Running all CI jobs via act..."
    echo "Log file: $log_file"
    echo ""

    # Run act and capture output
    act 2>&1 | tee "$log_file"
    local exit_code=${PIPESTATUS[0]}

    # Show error summary at the end
    echo ""
    echo -e "${YELLOW}╔══════════════════════════════════════════╗${NC}"
    echo -e "${YELLOW}║   CI Summary                             ║${NC}"
    echo -e "${YELLOW}╚══════════════════════════════════════════╝${NC}"

    # Extract job results
    echo ""
    echo "Job Results:"
    grep -E "(Job succeeded|Job .* failed)" "$log_file" | while read -r line; do
        if echo "$line" | grep -q "succeeded"; then
            echo -e "  ${GREEN}✓${NC} $line"
        else
            echo -e "  ${RED}✗${NC} $line"
        fi
    done

    # Show errors if any
    if grep -q "failed\|Error\|✗" "$log_file"; then
        echo ""
        echo -e "${RED}Errors found:${NC}"
        grep -B 5 "✗\|Error:\|failed" "$log_file" | grep -E "✗|Error:|Test.*failed|=== Results:" | head -20
    fi

    echo ""
    echo "Full log: $log_file"
    return $exit_code
}

case "${1:-local}" in
    ci)
        run_ci
        ;;
    local|"")
        run_local_tests
        ;;
    --help|-h)
        echo "Usage: $0 [command]"
        echo ""
        echo "Commands:"
        echo "  (none)   Run local tests (Rust unit + integration)"
        echo "  ci       Run full CI via act (requires Docker)"
        echo "  --help   Show this help"
        ;;
    *)
        echo "Unknown command: $1"
        echo "Run '$0 --help' for usage"
        exit 1
        ;;
esac
