#!/bin/bash

MODE="$1"
SCALE_TEST="${2:-}"

DAY04_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
INPUT_TXT="${DAY04_DIR}/day4_input.txt"
HC_DIR="${DAY04_DIR}/hardcaml"

if opam switch list -s 2>/dev/null | grep -qx '5.2.0'; then
  eval "$(opam env --switch=5.2.0)"
else
  eval "$(opam env)"
fi

cd "${HC_DIR}"

echo "build started."
dune build bin/sim.exe >/dev/null
echo "build finished."

if [[ "${SCALE_TEST}" == "--scale-test" ]]; then
  bash "${DAY04_DIR}/run_cpp_source.sh" "${MODE}" >/dev/null
  python3 "${DAY04_DIR}/scale_test.py" "${MODE}"
  exit $?
fi

EXPECTED="$(bash "${DAY04_DIR}/run_cpp_source.sh" "${MODE}" | tail -n 1)"

echo "running ${MODE} on ${INPUT_TXT}"
ACTUAL="$(dune exec ./bin/sim.exe -- "${MODE}" < "${INPUT_TXT}" | tail -n 1)"

if [[ "${ACTUAL}" != "${EXPECTED}" ]]; then
  echo "Mismatch: expected ${EXPECTED}, got ${ACTUAL}" 1>&2
  exit 1
fi

echo "${ACTUAL}"
