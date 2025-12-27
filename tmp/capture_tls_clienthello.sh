#!/bin/bash
# Capture TLS ClientHello packets from Python and OCaml implementations
# This will help us identify exactly what's different in the TLS fingerprint

PCAP_DIR="/home/struktured/projects/fluxum/tmp"

echo "========================================="
echo "Capturing TLS ClientHello Packets"
echo "========================================="
echo ""

# Capture Python TLS ClientHello
echo "1. Capturing Python WebSocket TLS handshake..."
sudo timeout 5 tcpdump -i any -w "${PCAP_DIR}/python_tls.pcap" \
  'host ws.kraken.com and tcp port 443' 2>/dev/null &
TCPDUMP_PID=$!
sleep 1

# Run Python WebSocket client
python3 /tmp/kraken_test_debug.py > /dev/null 2>&1 &
PYTHON_PID=$!
sleep 2
kill $PYTHON_PID 2>/dev/null
wait $PYTHON_PID 2>/dev/null

sudo kill $TCPDUMP_PID 2>/dev/null
wait $TCPDUMP_PID 2>/dev/null
echo "   Saved to: ${PCAP_DIR}/python_tls.pcap"
echo ""

# Capture OCaml TLS ClientHello
echo "2. Capturing OCaml+BoringSSL TLS handshake..."
sudo timeout 5 tcpdump -i any -w "${PCAP_DIR}/ocaml_tls.pcap" \
  'host ws.kraken.com and tcp port 443' 2>/dev/null &
TCPDUMP_PID=$!
sleep 1

# Run OCaml WebSocket client
cd /home/struktured/projects/fluxum
timeout 3 dune exec fluxum -- kraken ws stream --channel ticker --pair BTC/USD > /dev/null 2>&1 &
OCAML_PID=$!
sleep 2
kill $OCAML_PID 2>/dev/null
wait $OCAML_PID 2>/dev/null

sudo kill $TCPDUMP_PID 2>/dev/null
wait $TCPDUMP_PID 2>/dev/null
echo "   Saved to: ${PCAP_DIR}/ocaml_tls.pcap"
echo ""

echo "========================================="
echo "Analyzing ClientHello with tshark"
echo "========================================="
echo ""

echo "Python ClientHello:"
echo "-------------------"
tshark -r "${PCAP_DIR}/python_tls.pcap" -Y "ssl.handshake.type == 1" -V 2>/dev/null | \
  grep -A 200 "Handshake Protocol: Client Hello" | head -100

echo ""
echo "OCaml ClientHello:"
echo "-------------------"
tshark -r "${PCAP_DIR}/ocaml_tls.pcap" -Y "ssl.handshake.type == 1" -V 2>/dev/null | \
  grep -A 200 "Handshake Protocol: Client Hello" | head -100
