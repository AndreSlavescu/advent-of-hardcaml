#!/usr/bin/env python3

import argparse
import json
import os
import shutil
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


def run_tinytapeout_synth(day04_dir: Path, h: int, w: int) -> dict:
    """Run TinyTapeout/OpenLane synthesis and return metrics."""
    tt_dir = day04_dir / "tinytapeout"
    src_dir = tt_dir / "src"
    src_dir.mkdir(exist_ok=True)

    # Copy generated Verilog
    hc_verilog = day04_dir / "hardcaml" / "verilator" / "day04.v"
    shutil.copy(hc_verilog, src_dir / "day04.v")

    # Update config with design dimensions in name for tracking
    config_path = tt_dir / "config.json"
    with open(config_path) as f:
        config = json.load(f)

    # Scale die area based on design size (rough heuristic)
    scale_factor = min((h * w) / (140 * 140), 4.0)  # Cap at 4x base
    new_height = int(225 * (1 + scale_factor * 0.5))
    new_width = int(160 * (1 + scale_factor * 0.5))
    config["DIE_AREA"] = f"0 0 {new_width} {new_height}"

    with open(config_path, "w") as f:
        json.dump(config, f, indent=4)

    # Run OpenLane synthesis via docker
    t0 = time.perf_counter_ns()
    result = subprocess.run(
        [
            "docker", "run", "--rm",
            "-v", f"{tt_dir}:/work",
            "-v", f"{os.environ.get('PDK_ROOT', os.path.expanduser('~/.volare'))}:/pdk",
            "-e", "PDK_ROOT=/pdk",
            "-e", "PDK=sky130A",
            "-w", "/work",
            "ghcr.io/efabless/openlane2:latest",
            "openlane", "config.json", "--flow", "Classic"
        ],
        cwd=tt_dir,
        capture_output=True,
        text=True,
    )
    t1 = time.perf_counter_ns()

    synth_time = (t1 - t0) / 1e9

    # Find GDS output
    gds_files = list(tt_dir.glob("runs/*/final/gds/*.gds"))
    gds_path = gds_files[0] if gds_files else None

    # Parse synthesis stats if available
    stat_files = list(tt_dir.glob("runs/*/reports/synthesis/*synthesis.stat.rpt"))
    cell_count = 0
    if stat_files:
        stat_content = stat_files[0].read_text()
        for line in stat_content.splitlines():
            if "Number of cells:" in line:
                cell_count = int(line.split(":")[-1].strip())
                break

    return {
        "synth_seconds": synth_time,
        "gds_path": str(gds_path) if gds_path else None,
        "cell_count": cell_count,
        "success": result.returncode == 0,
        "die_area": f"{new_width}x{new_height}",
    }

def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--part1", action="store_true")
    ap.add_argument("--part2", action="store_true")
    ap.add_argument("--scales", nargs="*", type=int, default=[1, 5, 10, 20, 50])
    ap.add_argument("--tinytapeout", action="store_true", help="Run TinyTapeout/OpenLane synthesis")
    ap.add_argument("--gds", action="store_true", help="Generate GDS layout (implies --tinytapeout)")
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

    # TinyTapeout synthesis mode
    if args.tinytapeout or args.gds:
        print("TinyTapeout Synthesis Results")
        print("=" * 60)
        print()
        print("H,W,scale,synth_seconds,cell_count,die_area,gds_path")

        for scale in args.scales:
            if scale <= 0:
                continue

            h = base_h * scale
            w = base_w * scale

            # Generate Verilog first
            env = dict(opam_env, DAY04_WIDTH=str(w), DAY04_HEIGHT=str(h))
            build_verilator(hc_dir, env)

            # Run TinyTapeout synthesis
            result = run_tinytapeout_synth(day04_dir, h, w)

            status = "OK" if result["success"] else "FAIL"
            print(f"{h},{w},{scale},{result['synth_seconds']:.1f},{result['cell_count']},{result['die_area']},{status}")

            if result["gds_path"]:
                print(f"  -> GDS: {result['gds_path']}")

        return 0

    # Standard simulation comparison mode
    print(f"mode: {mode}")
    print()
    print("H,W,scale,cpp_seconds,hdl_seconds,output_matches")

    for scale in args.scales:
        if scale <= 0:
            continue

        h = base_h * scale
        w = base_w * scale

        block_lines = [(ln * scale) for ln in base_lines]
        block_bytes = ("\n".join(block_lines) + "\n").encode()

        t0 = time.perf_counter_ns()
        cpp_out = run_cmd_stream([str(cpp_bin), mode], block_bytes, repeats=scale)
        t1 = time.perf_counter_ns()

        env = dict(opam_env, DAY04_WIDTH=str(w), DAY04_HEIGHT=str(h))
        verilator_bin = build_verilator(hc_dir, env)

        t2 = time.perf_counter_ns()
        verilator_out = run_cmd_stream([str(verilator_bin), mode], block_bytes, repeats=scale)
        t3 = time.perf_counter_ns()

        ok = "yes" if cpp_out == verilator_out else "no"
        print(f"{h},{w},{scale},{(t1 - t0) / 1e9:.6f},{(t3 - t2) / 1e9:.6f},{ok}")

    return 0

if __name__ == "__main__":
    raise SystemExit(main())
