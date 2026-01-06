#!/usr/bin/env python3

import argparse
import os
import subprocess
import time
from pathlib import Path

def get_opam_env():
    result = subprocess.run(
        ["opam", "env"],
        capture_output=True,
        text=True,
    )
    env = dict(os.environ)
    for line in result.stdout.splitlines():
        if "=" in line and "; export" in line:
            assignment = line.split("; export")[0]
            key, val = assignment.split("=", 1)
            env[key] = val.strip("'\"")
    return env

def run_cmd_stream(cmd, block_bytes: bytes, repeats: int, env=None) -> int:
    p = subprocess.Popen(
        cmd,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=False,
        env=env,
    )

    for _ in range(repeats):
        p.stdin.write(block_bytes)
    p.stdin.close()

    out = p.stdout.read()
    p.wait()

    lines = [ln.strip() for ln in out.decode(errors="replace").splitlines() if ln.strip()]
    return int(lines[-1])

def build_verilator(hc_dir: Path, env) -> Path:
    verilator_dir = hc_dir / "verilator"
    verilator_dir.mkdir(exist_ok=True)

    subprocess.run(
        ["dune", "exec", "./bin/generate.exe"],
        cwd=hc_dir,
        stdout=open(verilator_dir / "day04.v", "w"),
        env=env,
        capture_output=False,
    )

    subprocess.run(
        ["verilator", "--cc", "day04.v", "--exe", "sim_main.cpp", "-O3", "--build", "-j", "4", "-Wno-COMBDLY"],
        cwd=verilator_dir,
        capture_output=True,
    )

    return verilator_dir / "obj_dir" / "Vday04"

def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--part1", action="store_true")
    ap.add_argument("--part2", action="store_true")
    ap.add_argument("--scales", nargs="*", type=int, default=[1, 5, 10])
    args = ap.parse_args()

    mode = "--part1" if args.part1 else "--part2"

    day04_dir = Path(__file__).resolve().parent
    hc_dir = day04_dir / "hardcaml"
    input_txt = day04_dir / "day4_input.txt"
    base_lines = [ln.rstrip("\r") for ln in input_txt.read_text().splitlines() if ln.strip()]
    base_h = len(base_lines)
    base_w = len(base_lines[0])

    cpp_bin = day04_dir / "cpp" / "day04"
    opam_env = get_opam_env()

    print(f"mode: {mode}")
    print()
    print("H,W,scale,cpp_seconds,hdl_seconds,output_matches")

    rtl_max_cells = 5_000_000

    for scale in args.scales:
        if scale <= 0:
            continue

        h = base_h * scale
        w = base_w * scale
        cells = h * w

        block_lines = [(ln * scale) for ln in base_lines]
        block_bytes = ("\n".join(block_lines) + "\n").encode()

        t0 = time.perf_counter()
        cpp_out = run_cmd_stream([str(cpp_bin), mode], block_bytes, repeats=scale)
        t1 = time.perf_counter()

        if cells <= rtl_max_cells:
            env = dict(opam_env, DAY04_WIDTH=str(w), DAY04_HEIGHT=str(h))

            verilator_bin = build_verilator(hc_dir, env)

            t2 = time.perf_counter()
            verilator_out = run_cmd_stream([str(verilator_bin), mode], block_bytes, repeats=scale)
            t3 = time.perf_counter()

            ok = "yes" if cpp_out == verilator_out else "no"
            print(f"{h},{w},{scale},{t1 - t0:.6f},{t3 - t2:.6f},{ok}")
        else:
            print(f"{h},{w},{scale},{t1 - t0:.6f},n/a,rtl_skipped")

    return 0

if __name__ == "__main__":
    raise SystemExit(main())
