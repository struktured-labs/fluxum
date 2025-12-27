#!/bin/bash
# Test WebSocket upgrade with curl-impersonate
# Connection will hang after upgrade, so we kill it after getting headers

(
  timeout 1 ./curl-impersonate-chrome -i \
    --http1.1 \
    -H "Connection: Upgrade" \
    -H "Upgrade: websocket" \
    -H "Sec-WebSocket-Version: 13" \
    -H "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" \
    -H "Origin: https://ws.kraken.com" \
    https://ws.kraken.com/ 2>&1
) | head -20
