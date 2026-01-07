# AoC 2025 Day 4 problem TinyTapeout

TinyTapeout-compatible wrapper for the hardcaml submission for the AoC 2025 Day 4 solution.

## Files

- `src/tt_um_day04.v` - TinyTapeout wrapper module
- `config.json` - OpenLane configuration
- `verify.py` - Verilator lint check
- `Makefile` - Build targets

## Pin Mapping

| Pin | Direction | Signal |
|-----|-----------|--------|
| ui_in[7:0] | in | in_char which takes ASCII input |
| uio_in[0] | in | part2 select |
| uio_in[1] | in | in_valid |
| uio_in[2] | in | start |
| uio_in[3] | in | read_result |
| uo_out[0] | out | in_ready |
| uo_out[1] | out | out_valid |
| uo_out[7:2] | out | result_byte[5:0] |
| uio_out[1:0] | out | result_byte[7:6] |

## Usage

From the day04 directory:

```bash
# produces synthesis stats for a grid of 16 * 16
./run_tinytapeout.sh

# lint
cd tinytapeout && make verify
```

## Grid Size

Default generates a 16 * 16 mini version. Edit `TT_WIDTH` and `TT_HEIGHT` in `run_tinytapeout.sh` to change, or run the script with new values like so:

```bash
TT_WIDTH=16 TT_HEIGHT=16 ./run_tinytapeout.sh
```

Since the 137 * 137 grid is too large for TinyTapeout, with roughly 47M cells. The 16 * 16 version demonstrates the architecture for a scale that can be synthesized.
