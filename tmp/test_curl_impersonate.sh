#!/bin/bash
# Test curl-impersonate against Kraken WebSocket

./curl-impersonate-chrome \
  --http1.1 \
  -H "Connection: Upgrade" \
  -H "Upgrade: websocket" \
  -H "Sec-WebSocket-Version: 13" \
  -H "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" \
  -H "Origin: https://ws.kraken.com" \
  -v \
  https://ws.kraken.com/
