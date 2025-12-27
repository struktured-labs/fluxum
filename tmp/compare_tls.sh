#!/bin/bash
# Compare TLS ClientHello between Python and OCaml

echo "=== Python TLS ClientHello ==="
sudo timeout 3 tcpdump -i any -s 0 -X 'host ws.kraken.com and tcp[((tcp[12:1] & 0xf0) >> 2):1] = 0x16' &
TCPDUMP_PID=$!
sleep 0.5

python3 /tmp/kraken_test_debug.py > /dev/null 2>&1 &
PYTHON_PID=$!
sleep 2

kill $PYTHON_PID 2>/dev/null
sudo kill $TCPDUMP_PID 2>/dev/null
wait

echo ""
echo "=== OCaml TLS ClientHello ==="
sudo timeout 3 tcpdump -i any -s 0 -X 'host ws.kraken.com and tcp[((tcp[12:1] & 0xf0) >> 2):1] = 0x16' &
TCPDUMP_PID=$!
sleep 0.5

cd /home/struktured/projects/fluxum
timeout 2 dune exec fluxum -- kraken ws stream --channel ticker --pair BTC/USD > /dev/null 2>&1 &
OCAML_PID=$!
sleep 2

kill $OCAML_PID 2>/dev/null
sudo kill $TCPDUMP_PID 2>/dev/null
wait
