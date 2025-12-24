#!/bin/bash
# WebSocket diagnostic script to distinguish rate limiting vs protocol issues

echo "=== Kraken WebSocket Diagnostic ==="
echo ""

echo "1. Testing basic connectivity to ws.kraken.com..."
ping -c 2 ws.kraken.com 2>&1 | grep -E "(packets transmitted|ttl=)" || echo "Ping failed or filtered"
echo ""

echo "2. Testing HTTPS access (should work)..."
curl -s -o /dev/null -w "HTTP Status: %{http_code}\n" https://api.kraken.com/0/public/Time
echo ""

echo "3. Checking our recent CF-RAY values (different = processed, same = cached block)..."
echo "Most recent CF-RAYs from our tests:"
echo "- 9b2d90710ed442bc-EWR (most recent)"
echo "- 9b2d8a1b38074241-EWR"
echo "- 9b2d895cdc8c3ee5-EWR"
echo "- 9b2d8327adf33ee5-EWR"
echo "All different! This means each request reaches Cloudflare (not IP-banned)"
echo ""

echo "4. Cloudflare 403 + Content-Length: 151 typically means:"
echo "   a) Rate limiting (most likely given our ~10-15 connection attempts)"
echo "   b) Geographic blocking (unlikely, same IP works for REST API)"
echo "   c) Missing required headers (possible but we simplified headers)"
echo "   d) WAF rule triggered (possible if User-Agent or pattern detected)"
echo ""

echo "5. To confirm it's rate limiting vs protocol issue:"
echo "   RATE LIMITING: Wait 10-15 minutes, try again. Should work."
echo "   PROTOCOL ISSUE: Would fail consistently even after waiting."
echo ""

echo "Next steps:"
echo "- Wait 10-15 minutes for potential rate limit to clear"
echo "- Try one single connection attempt"
echo "- If still fails after waiting, investigate protocol/headers further"
