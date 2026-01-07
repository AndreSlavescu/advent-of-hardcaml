#!/usr/bin/env python3
import subprocess
import sys
from pathlib import Path

def main():
    tt_dir = Path(__file__).parent
    src_dir = tt_dir / "src"
    hc_verilog = tt_dir.parent / "hardcaml" / "verilator" / "day04.v"

    if not hc_verilog.exists():
        print(f"ERROR: {hc_verilog} not found")
        return 1

    src_dir.mkdir(exist_ok=True)
    (src_dir / "day04.v").write_bytes(hc_verilog.read_bytes())

    wrapper = src_dir / "tt_um_day04.v"
    if not wrapper.exists():
        print(f"ERROR: {wrapper} not found")
        return 1

    result = subprocess.run(
        ["verilator", "--lint-only",
         "-Wno-COMBDLY", "-Wno-UNUSEDSIGNAL", "-Wno-MULTITOP", "-Wno-DECLFILENAME",
         "--top-module", "tt_um_day04",
         str(wrapper), str(src_dir / "day04.v")],
        capture_output=True, text=True
    )

    if result.returncode != 0:
        print("LINT FAILED:")
        print(result.stderr)
        return 1

    print("OK: lint passed")
    return 0

if __name__ == "__main__":
    sys.exit(main())
