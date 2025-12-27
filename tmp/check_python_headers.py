#!/usr/bin/env python3
"""Check what headers Python websocket-client sends"""

import websocket
import sys

# Enable debug to see headers
websocket.enableTrace(True)

try:
    ws = websocket.create_connection("wss://ws.kraken.com/")
    print("\n=== CONNECTION SUCCESSFUL ===")
    ws.close()
except Exception as e:
    print(f"\n=== ERROR: {e} ===")
