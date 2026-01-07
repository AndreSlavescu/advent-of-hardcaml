#!/bin/bash
set -e
cd "$(dirname "$0")"

eval $(opam env)

export TT_WIDTH=16
export TT_HEIGHT=16
OUT_DIR="tinytapeout_output"
WORK_DIR="/tmp/tt_day04_$$"

echo "TinyTapeOut with ${TT_WIDTH} by ${TT_HEIGHT} grid:"
echo

echo "generating verilog..."
echo
cd hardcaml
DAY04_WIDTH=$TT_WIDTH DAY04_HEIGHT=$TT_HEIGHT dune exec ./bin/generate.exe > /tmp/day04_tiny.v
cd - > /dev/null

mkdir -p "$OUT_DIR" "$WORK_DIR"
cp tinytapeout/src/tt_um_day04.v "$WORK_DIR/"
cp /tmp/day04_tiny.v "$WORK_DIR/day04.v"

echo "running yosys synthesis..."
echo
cd "$WORK_DIR"
yosys -q -p "
read_verilog tt_um_day04.v day04.v
hierarchy -top tt_um_day04
proc; opt; fsm; opt; memory; opt
techmap; opt
tee -o stats.txt stat
"
cd - > /dev/null

cp "$WORK_DIR/stats.txt" "$OUT_DIR/"

echo ""
echo "synthesis results:"
echo
cat "$OUT_DIR/stats.txt"
echo ""
echo "output: $OUT_DIR/stats.txt"
rm -rf "$WORK_DIR"
