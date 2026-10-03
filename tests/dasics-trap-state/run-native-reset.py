#!/usr/bin/env python3
"""Simulate the actual async-parent CSR harness emitted by production CIRCT lowering."""

import argparse
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import socket
import subprocess
import sys


def now():
    return datetime.now(timezone.utc).isoformat()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--batch", type=Path, required=True)
    parser.add_argument("--source-id", required=True)
    parser.add_argument("--tool-source", type=Path, required=True)
    parser.add_argument("--attempt", required=True)
    parser.add_argument("--enabled", choices=("true",), required=True)
    parser.add_argument("--jobs", type=int, default=32)
    args = parser.parse_args()
    if socket.gethostname() != "cmy-zgclab-eda":
        parser.error("Only cmy-zgclab-eda is authorized")
    if not 1 <= args.jobs <= 44:
        parser.error("The approved total host concurrency limit is 44")
    for value in (args.source_id, args.attempt):
        if not value or not all(c.isalnum() or c in "-_" for c in value):
            parser.error("Identifiers must contain only alphanumeric, dash or underscore")
    batch = args.batch.resolve(strict=True)
    source = batch / "inputs" / args.source_id
    root = batch / "tests" / args.attempt
    report_root = batch / "native-reset-reports"
    report_root.mkdir(exist_ok=True)
    report_path = report_root / (args.attempt + ".json")
    if report_path.exists() or root.exists():
        parser.error("An attempt must be new; previous evidence is never overwritten")
    record = {"status": "RUNNING", "started": now(), "host": socket.gethostname(),
              "pid": os.getpid(), "enabled": args.enabled == "true", "steps": [],
              "source_manifest": str(batch / "inputs" / (args.source_id + "-manifest.json")),
              "scope": "Actual wrapper/NewCSR async-parent reset behavior; no full processor execution or timing signoff"}

    def save():
        report_path.write_text(json.dumps(record, indent=2) + "\n")

    def execute(name, argv, cwd, log=None):
        row = {"name": name, "argv": argv, "cwd": str(cwd), "started": now()}
        record["steps"].append(row)
        save()
        environment = dict(os.environ, PATH="/usr/bin:/bin", LC_ALL="C", TZ="UTC",
                           MAKEFLAGS=f"-j{args.jobs} VK_PCH_I_FAST= VK_PCH_I_SLOW=")
        child = subprocess.Popen(argv, cwd=cwd, env=environment,
                                 stdout=log, stderr=subprocess.STDOUT)
        row["pid"] = child.pid
        save()
        rc = child.wait()
        row.update(finished=now(), exit_code=rc, process_reaped=True)
        save()
        return rc

    rc = 1
    save()
    try:
        generation = ["/usr/bin/python3", "-B", str(source / "tests/dasics-trap-state/run-local.py"),
                      "run", "--batch", str(batch), "--source-root", str(source),
                      "--source-manifest", str(batch / "inputs" / (args.source_id + "-manifest.json")),
                      "--tool-source", str(args.tool_source.resolve(strict=True)),
                      "--attempt", args.attempt, "--suite", "reset-rtl-on", "--jobs", str(args.jobs)]
        rc = execute("circt-generation", generation, batch)
        if rc == 0:
            rtl = root / "rtl"
            verilog = sorted(str(path) for path in rtl.rglob("*")
                             if path.is_file() and path.suffix in (".sv", ".v"))
            if not verilog:
                raise RuntimeError("CIRCT produced no Verilog sources")
            build = root / "native-build"
            command = ["/usr/bin/verilator", "--cc", "--exe", "--build", "--assert", "-Wno-fatal",
                       "--top-module", "FDIAsyncParentResetHarness", "--Mdir", str(build),
                       "-j", str(args.jobs), "-CFLAGS", "-std=c++17 -O2", "-o", "fdi-reset",
                       *verilog, str(source / "tests/dasics-reference/async-parent-reset.cpp")]
            with (root / "logs/native-build.log").open("x") as log:
                rc = execute("verilator-build", command, rtl, log)
        if rc == 0:
            with (root / "logs/native-run.log").open("x") as log:
                rc = execute("native-reset-test", [str(root / "native-build/fdi-reset"), args.enabled], root, log)
            if rc == 0:
                prefix = "FDI_NATIVE_RESET_RESULT "
                results = [json.loads(line[len(prefix):])
                           for line in (root / "logs/native-run.log").read_text().splitlines()
                           if line.startswith(prefix)]
                if len(results) != 1 or results[0].get("status") != "PASS":
                    raise RuntimeError("The reset test did not emit exactly one successful result")
                expected = {"enabled": args.enabled == "true", "views": 52,
                            "owners": 51 if args.enabled == "true" else 0,
                            "effects": 53 if args.enabled == "true" else 0}
                if any(results[0].get(key) != value for key, value in expected.items()):
                    raise RuntimeError("The test result does not match the requested configuration")
                record["result"] = results[0]
    except Exception as error:
        record["error"] = repr(error)
        rc = 1
    record.update(status="PASS" if rc == 0 else "FAIL", finished=now(), exit_code=rc)
    save()
    print(json.dumps({"native_reset_report": str(report_path), "status": record["status"], "exit_code": rc}), flush=True)
    return rc


if __name__ == "__main__":
    sys.exit(main())
