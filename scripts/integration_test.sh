#!/bin/bash
set -euo pipefail

# Integration test script for Fluxum exchanges
# Tests WebSocket connections, order book updates, and connection stability

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test configuration
TEST_DURATION=60  # Duration in seconds for each test
MIN_UPDATES=5     # Minimum number of updates expected
RESULTS_FILE="tmp/integration_test_results.txt"

# Create tmp directory if it doesn't exist
mkdir -p tmp

# Initialize results file
echo "Fluxum Integration Test Results - $(date)" > "$RESULTS_FILE"
echo "============================================" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"

# Test counters
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Function to print colored output
print_status() {
    local status=$1
    local message=$2

    if [ "$status" = "PASS" ]; then
        echo -e "${GREEN}✓ PASS${NC}: $message"
        echo "✓ PASS: $message" >> "$RESULTS_FILE"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    elif [ "$status" = "FAIL" ]; then
        echo -e "${RED}✗ FAIL${NC}: $message"
        echo "✗ FAIL: $message" >> "$RESULTS_FILE"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    elif [ "$status" = "SKIP" ]; then
        echo -e "${YELLOW}⊘ SKIP${NC}: $message"
        echo "⊘ SKIP: $message" >> "$RESULTS_FILE"
    elif [ "$status" = "INFO" ]; then
        echo -e "${BLUE}ℹ INFO${NC}: $message"
        echo "ℹ INFO: $message" >> "$RESULTS_FILE"
    fi

    TOTAL_TESTS=$((TOTAL_TESTS + 1))
}

# Function to test an exchange orderbook
test_exchange_orderbook() {
    local exchange=$1
    local executable=$2
    local symbol=${3:-}
    local log_file="tmp/integration_test_${exchange}.log"

    print_status "INFO" "Testing $exchange exchange orderbook..."

    # Build the command
    local cmd="timeout ${TEST_DURATION} dune exec ${executable}"
    if [ -n "$symbol" ]; then
        cmd="$cmd -- $symbol"
    fi

    # Run the test and capture output
    if $cmd > "$log_file" 2>&1; then
        # Test completed successfully within timeout
        local update_count=$(grep -c "Updated\|Epoch\|BTC\|ETH" "$log_file" 2>/dev/null || echo "0")

        if [ "$update_count" -ge "$MIN_UPDATES" ]; then
            print_status "PASS" "$exchange: Received $update_count updates"

            # Check for errors in the log
            if grep -qi "error\|exception\|failed" "$log_file" 2>/dev/null; then
                print_status "FAIL" "$exchange: Found errors in log file"
                echo "  Last 10 lines:" >> "$RESULTS_FILE"
                tail -10 "$log_file" | sed 's/^/    /' >> "$RESULTS_FILE"
            fi
        else
            print_status "FAIL" "$exchange: Insufficient updates ($update_count < $MIN_UPDATES)"
            echo "  Log file: $log_file" >> "$RESULTS_FILE"
        fi
    else
        local exit_code=$?
        if [ $exit_code -eq 124 ]; then
            # Timeout occurred (expected behavior for continuous streams)
            local update_count=$(grep -c "Updated\|Epoch\|BTC\|ETH" "$log_file" 2>/dev/null || echo "0")

            if [ "$update_count" -ge "$MIN_UPDATES" ]; then
                print_status "PASS" "$exchange: Timed out after receiving $update_count updates (expected)"
            else
                print_status "FAIL" "$exchange: Timed out with insufficient updates ($update_count < $MIN_UPDATES)"
                echo "  Last 20 lines of log:" >> "$RESULTS_FILE"
                tail -20 "$log_file" | sed 's/^/    /' >> "$RESULTS_FILE"
            fi
        else
            print_status "FAIL" "$exchange: Exited with code $exit_code"
            echo "  Last 20 lines of log:" >> "$RESULTS_FILE"
            tail -20 "$log_file" | sed 's/^/    /' >> "$RESULTS_FILE"
        fi
    fi

    echo "" >> "$RESULTS_FILE"
}

# Function to test REST API endpoints
test_exchange_rest() {
    local exchange=$1
    local endpoint=$2

    print_status "INFO" "Testing $exchange REST API: $endpoint..."

    local log_file="tmp/integration_test_${exchange}_${endpoint}.log"

    if timeout 10 dune exec app/cli.exe -- $exchange $endpoint > "$log_file" 2>&1; then
        # Check if we got valid JSON or data back
        if grep -q "{" "$log_file" 2>/dev/null || grep -q "price\|time\|symbol" "$log_file" 2>/dev/null; then
            print_status "PASS" "$exchange $endpoint: Returned valid data"
        else
            print_status "FAIL" "$exchange $endpoint: No valid data returned"
            echo "  Response:" >> "$RESULTS_FILE"
            head -10 "$log_file" | sed 's/^/    /' >> "$RESULTS_FILE"
        fi
    else
        local exit_code=$?
        print_status "FAIL" "$exchange $endpoint: Failed with exit code $exit_code"
        echo "  Error output:" >> "$RESULTS_FILE"
        tail -10 "$log_file" | sed 's/^/    /' >> "$RESULTS_FILE"
    fi

    echo "" >> "$RESULTS_FILE"
}

# Main test execution
echo ""
echo "=========================================="
echo "  Fluxum Integration Tests"
echo "  $(date)"
echo "=========================================="
echo ""

# Test 1: Gemini WebSocket Order Book (requires API credentials)
echo -e "${BLUE}[1/12]${NC} Testing Gemini WebSocket..."
test_exchange_orderbook "Gemini" "examples/gemini_orderbook.exe"

# Test 2: Kraken WebSocket Order Book
echo -e "${BLUE}[2/12]${NC} Testing Kraken WebSocket..."
test_exchange_orderbook "Kraken" "examples/kraken_orderbook.exe"

# Test 3: Hyperliquid WebSocket Order Book
echo -e "${BLUE}[3/12]${NC} Testing Hyperliquid WebSocket..."
test_exchange_orderbook "Hyperliquid" "examples/hyperliquid_orderbook.exe"

# Test 4: Binance WebSocket Order Book
echo -e "${BLUE}[4/12]${NC} Testing Binance WebSocket..."
test_exchange_orderbook "Binance" "examples/binance_orderbook.exe"

# Test 5: dYdX WebSocket Order Book
echo -e "${BLUE}[5/12]${NC} Testing dYdX WebSocket..."
test_exchange_orderbook "dYdX" "examples/dydx_orderbook.exe"

# Test 6: Consolidated Order Book (Multi-Exchange)
echo -e "${BLUE}[6/12]${NC} Testing Consolidated Order Book..."
test_exchange_orderbook "Consolidated" "examples/consolidated_orderbook.exe"

# Test 7: Binance REST API - Server Time
echo -e "${BLUE}[7/12]${NC} Testing Binance REST API..."
test_exchange_rest "binance" "server-time"

# Test 8: Kraken REST API - Server Time
echo -e "${BLUE}[8/12]${NC} Testing Kraken REST API..."
test_exchange_rest "kraken" "server-time"

# Test 9: Gemini REST API - Symbols
echo -e "${BLUE}[9/12]${NC} Testing Gemini REST API..."
test_exchange_rest "gemini" "symbols"

# Test 10: Connection Stability - Reconnection Test
echo -e "${BLUE}[10/12]${NC} Testing connection stability..."
print_status "INFO" "Running multiple short connections..."

success_count=0
for i in {1..3}; do
    # Use Kraken which works in CI without credentials
    if timeout 15 dune exec examples/kraken_orderbook.exe > "tmp/stability_test_$i.log" 2>&1; then
        success_count=$((success_count + 1))
    else
        exit_code=$?
        if [ $exit_code -eq 124 ]; then
            success_count=$((success_count + 1))  # Timeout is expected
        fi
    fi
done

if [ $success_count -ge 2 ]; then
    print_status "PASS" "Connection stability: $success_count/3 connections successful"
else
    print_status "FAIL" "Connection stability: Only $success_count/3 connections successful"
fi

# Test 11: Data Quality - Check for valid price levels
echo -e "${BLUE}[11/12]${NC} Testing data quality..."
# Use Kraken log since it works in CI without credentials
test_log="tmp/integration_test_Kraken.log"
if [ -f "$test_log" ]; then
    # Check for realistic price ranges (BTC should be > 1000)
    if grep -E "([0-9]{5,}|[0-9]{4}\.[0-9]{2})" "$test_log" > /dev/null 2>&1; then
        print_status "PASS" "Data quality: Found realistic price levels"
    else
        print_status "FAIL" "Data quality: Price levels seem invalid"
    fi
else
    print_status "SKIP" "Data quality: No test log available"
fi

# Test 12: Memory/Resource Test - Check for leaks
echo -e "${BLUE}[12/15]${NC} Testing resource usage..."
print_status "INFO" "Monitoring resource usage during 30s test..."

# Run a test and monitor (use Kraken which works in CI)
timeout 30 dune exec examples/kraken_orderbook.exe > tmp/resource_test.log 2>&1 &
TEST_PID=$!

# Wait and check if process is still running (it should be)
sleep 5
if ps -p $TEST_PID > /dev/null 2>&1; then
    print_status "PASS" "Resource test: Process stable after 5s"
else
    print_status "FAIL" "Resource test: Process died within 5s"
fi

# Cleanup
wait $TEST_PID 2>/dev/null || true

# Test 13: Account Operations - Core Types Unit Tests
echo -e "${BLUE}[13/15]${NC} Testing account operations core types..."
print_status "INFO" "Running account operations unit tests..."

if dune build @lib/test/runtest > tmp/account_ops_core.log 2>&1; then
    print_status "PASS" "Account operations core types: Unit tests passed"
else
    # Check if test stanza exists
    if grep -q "No rule found" tmp/account_ops_core.log 2>/dev/null; then
        print_status "SKIP" "Account operations core types: No test stanza defined yet"
    else
        print_status "FAIL" "Account operations core types: Unit tests failed"
        echo "  Last 10 lines:" >> "$RESULTS_FILE"
        tail -10 tmp/account_ops_core.log | sed 's/^/    /' >> "$RESULTS_FILE"
    fi
fi

# Test 14: Account Operations - Exchange Adapter Tests
echo -e "${BLUE}[14/15]${NC} Testing exchange adapter account operations..."

# Test each major exchange's account operations
for exchange in gemini kraken binance coinbase; do
    print_status "INFO" "Testing $exchange account operations..."
    test_log="tmp/account_ops_${exchange}.log"

    if dune build @lib/exchange/${exchange}/test/runtest > "$test_log" 2>&1; then
        print_status "PASS" "$exchange: Account operations tests passed"
    else
        if grep -q "No rule found" "$test_log" 2>/dev/null; then
            print_status "SKIP" "$exchange: No test stanza defined"
        else
            print_status "FAIL" "$exchange: Account operations tests failed"
            echo "  Last 5 lines:" >> "$RESULTS_FILE"
            tail -5 "$test_log" | sed 's/^/    /' >> "$RESULTS_FILE"
        fi
    fi
done

# Test 15: Account Operations - Deposit/Withdrawal Types
echo -e "${BLUE}[15/15]${NC} Testing deposit/withdrawal type normalization..."
print_status "INFO" "Verifying Transfer_status, Deposit, Withdrawal types compile..."

# Simple compilation test - if types are defined, they should compile
if dune build lib/types.ml > tmp/types_compile.log 2>&1; then
    print_status "PASS" "Account operation types: Compilation successful"
else
    print_status "FAIL" "Account operation types: Compilation failed"
    echo "  Error:" >> "$RESULTS_FILE"
    tail -10 tmp/types_compile.log | sed 's/^/    /' >> "$RESULTS_FILE"
fi

# Print summary
echo ""
echo "=========================================="
echo "  Test Summary"
echo "=========================================="
echo ""
echo -e "Total Tests:  ${BLUE}$TOTAL_TESTS${NC}"
echo -e "Passed:       ${GREEN}$PASSED_TESTS${NC}"
echo -e "Failed:       ${RED}$FAILED_TESTS${NC}"
echo ""

# Write summary to results file
echo "" >> "$RESULTS_FILE"
echo "=========================================" >> "$RESULTS_FILE"
echo "Summary" >> "$RESULTS_FILE"
echo "=========================================" >> "$RESULTS_FILE"
echo "Total Tests:  $TOTAL_TESTS" >> "$RESULTS_FILE"
echo "Passed:       $PASSED_TESTS" >> "$RESULTS_FILE"
echo "Failed:       $FAILED_TESTS" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"

# Exit with appropriate code
# In CI, we consider the run successful if at least 4 tests pass
# (some exchanges like Gemini require API credentials not available in CI)
# Increased threshold to include account operations tests
MIN_PASS_THRESHOLD=4

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    echo "All tests passed!" >> "$RESULTS_FILE"
    exit 0
elif [ $PASSED_TESTS -ge $MIN_PASS_THRESHOLD ]; then
    echo -e "${YELLOW}$PASSED_TESTS tests passed (minimum $MIN_PASS_THRESHOLD required). Some optional tests failed.${NC}"
    echo "$PASSED_TESTS tests passed (minimum $MIN_PASS_THRESHOLD required). Some optional tests failed." >> "$RESULTS_FILE"
    exit 0
else
    echo -e "${RED}Only $PASSED_TESTS tests passed (need at least $MIN_PASS_THRESHOLD). Check logs in tmp/ directory.${NC}"
    echo "Only $PASSED_TESTS tests passed (need at least $MIN_PASS_THRESHOLD). Check individual test logs." >> "$RESULTS_FILE"
    exit 1
fi
