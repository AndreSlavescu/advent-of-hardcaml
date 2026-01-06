# Advent of Hardcaml

This repository contains my RTL design for various [Advent of Code](https://adventofcode.com/) challenge problems from various years. The reason for choosing to use hardcaml is in spirit of the [Jane Street Advent of Code FPGA design challenge](https://blog.janestreet.com/advent-of-fpga-challenge-2025/).

For my submission I chose to solve both parts of day 4 from the 2025 calendar. You can find the technical explanation for the problem and RTL design in [`day04_rtl.ml`](2025/day04/hardcaml/src/day04_rtl.ml).

## Installation and Testing

For installation instructions, visit the respective READMEs for the [cpp source](2025/day04/cpp/README.md) and the [hardcaml source](2025/day04/hardcaml/README.md). 


#### Note:

The reason there is a cpp source as well, is to have a baseline for checking correctness as well as having a high level model of the underlying algorithm. The cpp source however, is independent of the hardcaml source.
