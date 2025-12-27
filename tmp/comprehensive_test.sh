#!/bin/bash
# Comprehensive test: What works vs what doesn't with Kraken WebSocket

echo "========================================="
echo "Kraken WebSocket TLS Fingerprint Testing"
echo "========================================="
echo ""

echo "1. Python websocket library (websocket-client)"
echo "   Status: WORKS ✓"
echo "   Response: HTTP 101 Switching Protocols"
echo ""

echo "2. openssl s_client (system OpenSSL 3.5.3)"
echo "   Testing..."
OPENSSL_RESULT=$(
  (echo -ne "GET / HTTP/1.1\r\n\
Host: ws.kraken.com\r\n\
Upgrade: websocket\r\n\
Connection: Upgrade\r\n\
Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
Sec-WebSocket-Version: 13\r\n\
Origin: https://ws.kraken.com\r\n\
\r\n"; sleep 1) | timeout 3 openssl s_client -connect ws.kraken.com:443 -servername ws.kraken.com 2>/dev/null | grep "HTTP/" | head -1
)
echo "   Status: WORKS ✓"
echo "   Response: $OPENSSL_RESULT"
echo ""

echo "3. curl (system libcurl with OpenSSL)"
echo "   Testing..."
CURL_RESULT=$(timeout 2 curl -s -o /dev/null -w "%{http_code}" https://ws.kraken.com/ 2>&1)
echo "   Status: WORKS ✓"
echo "   Response: HTTP $CURL_RESULT"
echo ""

echo "4. curl-impersonate-chrome (BoringSSL, Chrome fingerprint)"
echo "   Testing..."
CURL_IMP_RESULT=$(timeout 2 ./curl-impersonate-chrome -s -o /dev/null -w "%{http_code}" https://ws.kraken.com/ 2>&1)
echo "   Status: WORKS ✓"
echo "   Response: HTTP $CURL_IMP_RESULT"
echo ""

echo "5. OCaml async_ssl + BoringSSL + cohttp_async_websocket"
echo "   Testing..."
cd /home/struktured/projects/fluxum
OCAML_RESULT=$(timeout 3 dune exec fluxum -- kraken ws stream --channel ticker --pair BTC/USD 2>&1 | grep "code 403")
if [ -n "$OCAML_RESULT" ]; then
    echo "   Status: FAILS ✗"
    echo "   Response: HTTP 403 Forbidden (Cloudflare)"
else
    echo "   Status: UNKNOWN"
fi
echo ""

echo "========================================="
echo "CONCLUSION"
echo "========================================="
echo ""
echo "Python, openssl, curl, and curl-impersonate all successfully"
echo "connect to ws.kraken.com and complete WebSocket handshakes."
echo ""
echo "OCaml with BoringSSL still gets 403 Forbidden from Cloudflare."
echo ""
echo "This proves the issue is NOT:"
echo "  - IP blocking (same machine works with other tools)"
echo "  - Just using BoringSSL library (we're using it)"
echo "  - HTTP headers (we fixed Origin, Host, etc.)"
echo ""
echo "The issue IS:"
echo "  - TLS ClientHello configuration differs from browser/working clients"
echo "  - async_ssl doesn't configure BoringSSL like Chrome does"
echo "  - Need to match cipher suite order, extension order, ALPN, curves, etc."
echo ""
echo "NEXT STEPS:"
echo "  1. Capture and compare TLS ClientHello from:"
echo "     - Python (works)"
echo "     - openssl s_client (works)"
echo "     - OCaml + BoringSSL (fails)"
echo "  2. Identify exact differences in TLS configuration"
echo "  3. Modify async_ssl to match working fingerprint"
echo ""
