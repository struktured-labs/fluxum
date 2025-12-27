#!/bin/bash
# Capture TLS ClientHello as hex dump for analysis

echo "========================================="
echo "Python TLS ClientHello Capture"
echo "========================================="

# Capture Python connection
sudo timeout 3 tcpdump -i any -s 0 -x 'host ws.kraken.com and tcp[((tcp[12:1] & 0xf0) >> 2):1] = 0x16' 2>/dev/null &
TCPDUMP_PID=$!
sleep 1

python3 /tmp/kraken_test_debug.py > /dev/null 2>&1 &
PYTHON_PID=$!
sleep 1
kill $PYTHON_PID 2>/dev/null
wait $PYTHON_PID 2>/dev/null

sleep 1
sudo kill $TCPDUMP_PID 2>/dev/null
wait $TCPDUMP_PID 2>/dev/null

echo ""
echo "========================================="
echo "OCaml TLS ClientHello Capture"
echo "========================================="

# Capture OCaml connection
sudo timeout 3 tcpdump -i any -s 0 -x 'host ws.kraken.com and tcp[((tcp[12:1] & 0xf0) >> 2):1] = 0x16' 2>/dev/null &
TCPDUMP_PID=$!
sleep 1

cd /home/struktured/projects/fluxum
timeout 2 dune exec fluxum -- kraken ws stream --channel ticker --pair BTC/USD > /dev/null 2>&1 &
OCAML_PID=$!
sleep 1
kill $OCAML_PID 2>/dev/null
wait $OCAML_PID 2>/dev/null

sleep 1
sudo kill $TCPDUMP_PID 2>/dev/null
wait $TCPDUMP_PID 2>/dev/null
