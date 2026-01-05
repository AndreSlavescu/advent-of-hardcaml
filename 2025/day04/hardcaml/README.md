## AoC 2025 Day 4 problem HDL

Hardcaml solution for day 4.

### Install

Since this repo doesn’t assume OCaml tooling is installed. The Jane Street template recommends
using OxCaml, but you're free to also use stock OCaml if you prefer. Install the OCaml + Hardcaml toolchain to run this project by visiting the following references:

- `https://oxcaml.org/get-oxcaml/`
- `https://raw.githubusercontent.com/janestreet/hardcaml_template_project/with-extensions/README.md`

opam installs:

```bash
opam install -y hardcaml hardcaml_waveterm hardcaml_step_testbench ppx_hardcaml
opam install -y core core_unix ppx_jane rope re dune
```

### Build

```bash
cd 2025/day04/hardcaml
dune build bin/generate.exe
```

### Simulating the RTL

```bash
dune exec ./bin/sim.exe -- --part1 < input.txt
dune exec ./bin/sim.exe -- --part2 < input.txt
```

### Generate Verilog

```bash
dune exec ./bin/generate.exe
```

### Verilator Simulation

First make sure you have verilator installed.

```bash
cd 2025/day04/hardcaml/verilator
dune exec ../bin/generate.exe > day04.v
verilator --cc day04.v --exe sim_main.cpp -O3 --build -j 4 -Wno-COMBDLY
./obj_dir/Vday04 --part2 < ../../day4_input.txt
```

### Scale Test

Compare C++ reference implementation against Verilator simulation at various grid scales:

```bash
cd 2025/day04
python3 scale_test.py --part2 --scales 1 5 10
```

### Direct Bash Script

```bash
cd 2025/day04
./run_rtl.sh --part1 # part 1 (RTL sim only)
./run_rtl.sh --part2 # part 2 (RTL sim only)
./run_rtl.sh --part1 --scale-test # part 1 with scale test
./run_rtl.sh --part2 --scale-test # part 2 with scale test
```