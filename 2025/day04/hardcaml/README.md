## AoC 2025 Day 4 (Printing Department) — Hardcaml starter

This is a small Hardcaml project scaffold for Day 4. It includes:

- The **C++ solver** under `../cpp/` is treated as the *golden model* for correctness.
- An **RTL module** (`src/day04_rtl.ml`) implementing **Part 1 + Part 2** with a realistic ready/valid interface.
- A **Verilog generator** (`bin/generate.ml`) following Jane Street’s Hardcaml template conventions.

### Install the OCaml + Hardcaml toolchain

This repo doesn’t assume OCaml tooling is installed. The Jane Street template recommends
using OxCaml, but you can also use stock OCaml if you prefer.

See:
- `https://oxcaml.org/get-oxcaml/`
- `https://raw.githubusercontent.com/janestreet/hardcaml_template_project/with-extensions/README.md`

Typical opam installs (non-interactive):

```bash
opam install -y hardcaml hardcaml_waveterm hardcaml_step_testbench ppx_hardcaml
opam install -y core core_unix ppx_jane rope re dune
```

### Build

From this directory:

```bash
dune build bin/generate.exe
```

### Simulate the RTL

This drives the RTL with ready/valid and prints the answer:

```bash
dune exec ./bin/sim.exe -- --part1 < input.txt
dune exec ./bin/sim.exe -- --part2 < input.txt
```

### Generate Verilog

```bash
dune exec ./bin/generate.exe
```


