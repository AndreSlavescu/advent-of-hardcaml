#!/bin/bash

usage() {
  echo "Usage: $0 --part1|--part2" 1>&2
}

MODE="$1"
if [[ "${MODE}" != "--part1" && "${MODE}" != "--part2" ]]; then
  usage
  exit 2
fi

echo "build started."

DAY04_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CPP_DIR="${DAY04_DIR}/cpp"
CPP_BIN="${CPP_DIR}/day04"
INPUT_TXT="${DAY04_DIR}/day4_input.txt"

cd "${CPP_DIR}"

g++ -O3 -std=c++20 -Wall -o "${CPP_BIN}" \
  "main.cpp" \
  "day04_common.cpp" \
  "day04_part1.cpp" \
  "day04_part2.cpp"

echo "build finished: ${CPP_BIN}"
echo "running ${MODE} on ${INPUT_TXT}"
"${CPP_BIN}" "${MODE}" < "${INPUT_TXT}"
