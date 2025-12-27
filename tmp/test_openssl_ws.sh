#!/bin/bash
# Test WebSocket handshake using openssl s_client

(echo -ne "GET / HTTP/1.1\r\n\
Host: ws.kraken.com\r\n\
Upgrade: websocket\r\n\
Connection: Upgrade\r\n\
Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\
Sec-WebSocket-Version: 13\r\n\
Origin: https://ws.kraken.com\r\n\
\r\n"; sleep 2) | timeout 5 openssl s_client -connect ws.kraken.com:443 -servername ws.kraken.com 2>/dev/null
