# Integration Tests

This directory contains integration test scripts for the Fluxum cryptocurrency exchange library.

## integration_test.sh

Comprehensive integration test script that validates:

- **WebSocket Connections**: Tests live WebSocket connections to multiple exchanges (Gemini, Kraken, Hyperliquid)
- **Order Book Updates**: Verifies order book data is received and updated correctly
- **REST API Endpoints**: Tests REST API endpoints for server time, symbols, etc.
- **Connection Stability**: Tests multiple successive connections to ensure reliability
- **Data Quality**: Validates that received price levels are realistic
- **Resource Usage**: Monitors for memory leaks and crashes

### Usage

Run locally:
```bash
./scripts/integration_test.sh
```

The script will:
1. Test each exchange's WebSocket order book for 60 seconds
2. Verify minimum number of updates received (5+)
3. Test REST API endpoints with 10-second timeout
4. Run stability tests with multiple connections
5. Generate detailed logs in `tmp/` directory
6. Create summary report in `tmp/integration_test_results.txt`

### Configuration

Edit these variables in the script to adjust test parameters:

- `TEST_DURATION`: Duration in seconds for each WebSocket test (default: 60)
- `MIN_UPDATES`: Minimum number of updates expected (default: 5)

### Test Output

The script produces:
- Colored console output with test progress
- Individual log files per exchange: `tmp/integration_test_<exchange>.log`
- Summary report: `tmp/integration_test_results.txt`

### Exit Codes

- `0`: All tests passed
- `1`: One or more tests failed

## CI/CD Integration

The integration test runs automatically:

- **Daily**: Every day at 2 AM UTC via GitHub Actions scheduled workflow
- **Manual**: Can be triggered manually from GitHub Actions "Run workflow" button

View results in GitHub Actions artifacts (retained for 7 days).

## Test Coverage

| Exchange     | WebSocket | REST API | Notes                    |
|--------------|-----------|----------|--------------------------|
| Gemini       | ✓         | ✓        | Public endpoints only    |
| Kraken       | ✓         | ✓        | Public endpoints only    |
| Hyperliquid  | ✓         | ✗        | WebSocket only           |
| Binance      | ✗         | ✓        | REST API only            |
| Consolidated | ✓         | ✗        | Multi-exchange order book|

## Extending Tests

To add a new exchange test:

1. Add a new test case in the main execution section
2. Use `test_exchange_orderbook` for WebSocket tests
3. Use `test_exchange_rest` for REST API tests

Example:
```bash
test_exchange_orderbook "MyExchange" "examples/myexchange_orderbook.exe"
test_exchange_rest "myexchange" "server-time"
```

## Troubleshooting

If tests fail:

1. Check individual log files in `tmp/` directory
2. Look for error messages in `tmp/integration_test_results.txt`
3. Verify network connectivity to exchange endpoints
4. Check exchange API status (some may be down or rate-limiting)

For network-related failures, the CI workflow uses `continue-on-error: false` to fail the build, ensuring issues are caught early.
