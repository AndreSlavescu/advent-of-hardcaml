## AoC 2025 Day 4 (Printing Department) — C++ reference

### Build

From repo root:

```bash
./2025/build.sh
```

Or build just this file:

```bash
g++ -O2 -std=c++20 -Wall -Wextra -Wpedantic -o 2025/day04/cpp/day04 \
  2025/day04/cpp/main.cpp \
  2025/day04/cpp/day04_common.cpp \
  2025/day04/cpp/day04_part1.cpp \
  2025/day04/cpp/day04_part2.cpp
```

### Run

```bash
./2025/day04/cpp/day04 --part1 < input.txt
```

### Part 2

Part 2 removes accessible rolls in batches until no more can be removed (a 4-core peel).

```bash
./2025/day04/cpp/day04 --part2 < input.txt
```


