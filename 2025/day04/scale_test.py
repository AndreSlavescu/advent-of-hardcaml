#!/usr/bin/env python3

import argparse
import os
import subprocess
import time
from pathlib import Path

def run_cmd_stream(cmd, block_bytes: bytes, repeats: int, env=None) -> int:
    p = subprocess.Popen(
        cmd,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=False,
        env=env,
    )
    assert p.stdin is not None
    assert p.stdout is not None
    assert p.stderr is not None

    for _ in range(repeats):
        p.stdin.write(block_bytes)
    p.stdin.close()

    out = p.stdout.read()
    err = p.stderr.read()
    rc = p.wait()
    if rc != 0:
        raise RuntimeError(
            f"Command failed (rc={rc}): {' '.join(cmd)}\n"
            f"--- stderr ---\n{err.decode(errors='replace')}\n"
            f"--- stdout ---\n{out.decode(errors='replace')}\n"
        )

    lines = [ln.strip() for ln in out.decode(errors="replace").splitlines() if ln.strip()]
    if not lines:
        raise RuntimeError(f"No output from: {' '.join(cmd)}")
    return int(lines[-1])

def main() -> int:
    ap = argparse.ArgumentParser(
        description="Day 4 scale test (tile input by N in each dimension; no files written)"
    )
    ap.add_argument("--part1", action="store_true")
    ap.add_argument("--part2", action="store_true")
    ap.add_argument("mode", nargs="?", help="Optional: pass --part1 or --part2 as positional arg")
    ap.add_argument(
        "--scales",
        nargs="*",
        type=int,
        default=[1, 10, 100],
    )
    args = ap.parse_args()

    if args.mode in ("--part1", "--part2"):
        args.part1 = args.part1 or (args.mode == "--part1")
        args.part2 = args.part2 or (args.mode == "--part2")

    if args.part1 == args.part2:
        ap.error("Choose exactly one: --part1 or --part2")

    mode = "--part1" if args.part1 else "--part2"

    day04_dir = Path(__file__).resolve().parent
    input_txt = day04_dir / "day4_input.txt"
    if not input_txt.exists():
        raise SystemExit(f"Input not found: {input_txt}")
    base_lines = [ln.rstrip("\r") for ln in input_txt.read_text().splitlines() if ln.strip()]
    if not base_lines:
        raise SystemExit("Empty input")
    base_h = len(base_lines)
    base_w = len(base_lines[0])
    if any(len(ln) != base_w for ln in base_lines):
        raise SystemExit("Non-rectangular base input")

    cpp_bin = day04_dir / "cpp" / "day04"
    sim_bin = day04_dir / "hardcaml" / "_build" / "default" / "bin" / "sim.exe"

    if not cpp_bin.exists():
        raise SystemExit(f"C++ binary not found (build it first): {cpp_bin}")
    if not sim_bin.exists():
        raise SystemExit(f"RTL sim binary not found (build it first): {sim_bin}")

    print(f"mode: {mode}")
    print()
    print("H,W,scale,cpp_seconds,rtl_seconds,ok")

    base_block = ("\n".join(base_lines) + "\n").encode()
    cpp_base = run_cmd_stream([str(cpp_bin), mode], base_block, repeats=1)
    rtl_env_base = dict(os.environ, DAY04_WIDTH=str(base_w), DAY04_HEIGHT=str(base_h))
    rtl_base = run_cmd_stream([str(sim_bin), mode], base_block, repeats=1, env=rtl_env_base)
    if cpp_base != rtl_base:
        raise SystemExit(f"Baseline mismatch (H={base_h},W={base_w}): cpp={cpp_base} rtl={rtl_base}")

    # Hard limit for RTL simulation to avoid huge memory allocations in Cyclesim/sim.exe.
    rtl_max_cells = 5_000_000

    for scale in args.scales:
        if scale <= 0:
            continue

        t0 = time.perf_counter()
        block_lines = [(ln * scale) for ln in base_lines]
        block_bytes = ("\n".join(block_lines) + "\n").encode()
        cpp_out = run_cmd_stream([str(cpp_bin), mode], block_bytes, repeats=scale)
        t1 = time.perf_counter()

        h = base_h * scale
        w = base_w * scale

        cells = h * w
        if cells <= rtl_max_cells:
            t2 = time.perf_counter()
            rtl_env = dict(os.environ, DAY04_WIDTH=str(w), DAY04_HEIGHT=str(h))
            rtl_out = run_cmd_stream([str(sim_bin), mode], block_bytes, repeats=scale, env=rtl_env)
            t3 = time.perf_counter()
            ok = "yes" if cpp_out == rtl_out else "no"
            print(f"{h},{w},{scale},{t1 - t0:.6f},{t3 - t2:.6f},{ok}")
        else:
            print(f"{h},{w},{scale},{t1 - t0:.6f},n/a,rtl_skipped")

    return 0

if __name__ == "__main__":
    raise SystemExit(main())

