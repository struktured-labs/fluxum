#!/usr/bin/env python3
"""
Analyze Python's SSL/TLS configuration to see what it sends in ClientHello
"""

import ssl
import socket
import sys

# Create SSL context like websocket-client does
context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
context.check_hostname = True
context.verify_mode = ssl.CERT_REQUIRED
context.load_default_certs()

print("=== Python SSL Configuration ===")
print(f"Protocol: {context.protocol}")
print(f"Verify mode: {context.verify_mode}")
print(f"Check hostname: {context.check_hostname}")
print()

print("=== Supported Ciphers ===")
ciphers = context.get_ciphers()
print(f"Total cipher suites: {len(ciphers)}")
for i, cipher in enumerate(ciphers[:20]):  # Show first 20
    print(f"{i+1}. {cipher['name']} (0x{cipher['id']:04X})")
if len(ciphers) > 20:
    print(f"... and {len(ciphers) - 20} more")
print()

# Get OpenSSL version
print("=== OpenSSL Info ===")
print(f"Version: {ssl.OPENSSL_VERSION}")
print(f"Version number: {hex(ssl.OPENSSL_VERSION_NUMBER)}")
print()

# Try to get more details by connecting
print("=== Attempting connection to see negotiated parameters ===")
try:
    sock = socket.create_connection(("ws.kraken.com", 443), timeout=5)
    ssock = context.wrap_socket(sock, server_hostname="ws.kraken.com")

    print(f"Connected successfully!")
    print(f"Protocol version: {ssock.version()}")
    print(f"Cipher: {ssock.cipher()}")
    print(f"Compression: {ssock.compression()}")

    # Get certificate info
    cert = ssock.getpeercert()
    print(f"Server cert subject: {cert.get('subject')}")

    ssock.close()
except Exception as e:
    print(f"Connection failed: {e}")
