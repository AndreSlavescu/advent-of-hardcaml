#!/bin/bash

usage() {
  echo "Usage: $0 --part1|--part2 [--scale-test]" 1>&2
}

if [[ $# -lt 1 || $# -gt 2 ]]; then
  usage
  exit 2
fi

MODE="$1"
if [[ "${MODE}" != "--part1" && "${MODE}" != "--part2" ]]; then
  usage
  exit 2
fi

SCALE_TEST="0"
if [[ $# -eq 2 ]]; then
  if [[ "$2" == "--scale-test" ]]; then
    SCALE_TEST="1"
  else
    usage
    exit 2
  fi
fi

DAY04_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_TXT="${DAY04_DIR}/day4_input.txt"
HC_DIR="${DAY04_DIR}/hardcaml"

if ! command -v opam >/dev/null 2>&1; then
  echo "Error: opam not found in PATH. Install OCaml tooling first, see hardcaml/README.md for further instructions on how to do so." 1>&2
  exit 1
fi

if opam switch list -s 2>/dev/null | grep -qx '5.2.0'; then
  eval "$(opam env --switch=5.2.0)"
else
  eval "$(opam env)"
fi

cd "${HC_DIR}"

echo "build started."
dune build bin/sim.exe >/dev/null
echo "build finished."

if [[ "${SCALE_TEST}" == "1" ]]; then
  # Ensure C++ is built once (run_cpp_source.sh always builds).
  bash "${DAY04_DIR}/run_cpp_source.sh" "${MODE}" >/dev/null
  chmod +x "${DAY04_DIR}/scale_test.py" || true
  python3 "${DAY04_DIR}/scale_test.py" "${MODE}"
  exit $?
fi

EXPECTED="$(bash "${DAY04_DIR}/run_cpp_source.sh" "${MODE}" | tail -n 1)"

echo "running ${MODE} on ${INPUT_TXT}"
ACTUAL="$(dune exec ./bin/sim.exe -- "${MODE}" < "${INPUT_TXT}" | tail -n 1)"

if [[ "${ACTUAL}" != "${EXPECTED}" ]]; then
  echo "Mismatch!" 1>&2
  echo "  expected: ${EXPECTED}" 1>&2
  echo "  got: ${ACTUAL}" 1>&2
  exit 1
fi

echo "${ACTUAL}"
