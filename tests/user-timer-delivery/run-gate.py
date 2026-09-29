#!/usr/bin/env python3
"""Run one isolated offline gate and retain its command, inputs and fresh results."""

import argparse
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time
import xml.etree.ElementTree as ET


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--run-root", required=True, type=Path)
    parser.add_argument("--name", required=True)
    parser.add_argument("--expect-tests", type=int)
    parser.add_argument("--configuration", choices=("enabled", "disabled"), default="enabled")
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    if not args.name or any(c not in "abcdefghijklmnopqrstuvwxyz0123456789-_" for c in args.name):
        parser.error("name must contain only lowercase letters, digits, hyphen or underscore")
    command = args.command[1:] if args.command[:1] == ["--"] else args.command
    if not command:
        parser.error("a Mill target is required after --")
    source_root = Path(__file__).resolve().parents[2]
    run_root = args.run_root.resolve()
    gate_root = run_root / "gates" / args.name
    gate_root.mkdir(parents=True, exist_ok=False)
    # Production TopMain publishes its auxiliary files relative to the working directory.
    (gate_root / "build").mkdir()
    scope = run_root / "scoped-build"
    scope.mkdir(exist_ok=True)
    build_file = source_root / "tests/user-timer-delivery/build.sc"
    shutil.copy2(build_file, scope / "build.sc")
    inputs = gate_root / "inputs"
    inputs.mkdir()
    shutil.copytree(source_root / "src", inputs / "src")
    shutil.copytree(source_root / "tests", inputs / "tests")
    env = os.environ.copy()
    selected_env = {
        "MILL_VERSION": "0.12.3",
        "JAVA_HOME": "/usr/lib/jvm/java-11-openjdk-amd64",
        "PATH": "/usr/lib/jvm/java-11-openjdk-amd64/bin:" + env.get("PATH", "/usr/bin:/bin"),
        "JAVA_TOOL_OPTIONS": "-Xmx40G -Xss256m",
        "COURSIER_MODE": "offline",
        "COURSIER_REPOSITORIES": "file:///home/zengsiyuan/.cache/coursier/v1/https/repo1.maven.org/maven2",
        "COURSIER_CACHE": str(run_root / "cache"),
        "MILL_OUTPUT_DIR": str(run_root / "mill-output"),
        "UIT02_SOURCE_ROOT": str(source_root),
        "UIT02_RUN_ROOT": str(gate_root),
        "UIT04_RUN_ROOT": str(gate_root),
        "UIT04_CONFIGURATION": args.configuration,
        "CHISEL_FIRTOOL_PATH": "/home/zengsiyuan/.cache/llvm-firtool/1.62.1/bin",
        "MAKEFLAGS": "VK_PCH_I_FAST= VK_PCH_I_SLOW=",
    }
    env.update(selected_env)
    argv = ["/usr/bin/mill", "--no-server", "-j", str(os.cpu_count()), *command]
    started = time.time()
    record = {"argv": argv, "cwd": str(scope), "environment": selected_env,
              "started": datetime.now(timezone.utc).isoformat(),
              "expected_tests": args.expect_tests}
    (gate_root / "command.json").write_text(json.dumps(record, indent=2) + "\n")
    print(f"Starting {args.name}: {gate_root}", flush=True)
    with (gate_root / "run.log").open("w") as log:
        result = subprocess.run(["/usr/bin/time", "-v", "-o", str(gate_root / "resources.txt"),
                                 *argv], cwd=scope, env=env, stdout=log, stderr=subprocess.STDOUT)
    (gate_root / "exit-code").write_text(str(result.returncode) + "\n")
    record.update({"exit_code": result.returncode, "elapsed_seconds": time.time() - started,
                   "finished": datetime.now(timezone.utc).isoformat()})
    failures = []
    if result.returncode:
        failures.append(f"command exited {result.returncode}")
    if args.expect_tests is not None:
        report = run_root / "mill-output/production/test/testOnly.dest/test-report.xml"
        if not report.is_file() or report.stat().st_mtime < started:
            failures.append("fresh JUnit XML is missing")
        else:
            shutil.copy2(report, gate_root / "test-report.xml")
            tree = ET.parse(report)
            cases = list(tree.iter("testcase"))
            record["tests"] = [{"suite": case.get("classname"), "name": case.get("name")} for case in cases]
            if len(cases) != args.expect_tests or not cases:
                failures.append(f"expected {args.expect_tests} tests, found {len(cases)}")
            for tag in ("failure", "error", "skipped"):
                count = sum(1 for _ in tree.iter(tag))
                record[tag] = count
                if count:
                    failures.append(f"JUnit contains {count} {tag} elements")
    record["failures"] = failures
    record["status"] = "FAIL" if failures else "PASS"
    (gate_root / "result.json").write_text(json.dumps(record, indent=2) + "\n")
    print(json.dumps({"gate": args.name, "status": record["status"], "failures": failures,
                      "elapsed_seconds": record["elapsed_seconds"]}), flush=True)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
