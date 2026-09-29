#!/usr/bin/env python3
"""Run the complete delivery suite with one feature configuration per test JVM."""

import argparse
from datetime import datetime, timezone
import json
from pathlib import Path
import subprocess
import sys
import time
import xml.etree.ElementTree as ET


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--run-root", required=True, type=Path)
    parser.add_argument("--name", required=True)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args()
    if not args.name or any(c not in "abcdefghijklmnopqrstuvwxyz0123456789-_" for c in args.name):
        parser.error("name must contain only lowercase letters, digits, hyphen or underscore")
    run_root = args.run_root.resolve()
    runner = Path(__file__).resolve().with_name("run-gate.py")
    variants = (("enabled", 21), ("disabled", 1))
    commands = []
    for configuration, expected in variants:
        gate_name = f"{args.name}-{configuration}"
        commands.append([
            sys.executable, str(runner), "--run-root", str(run_root), "--name", gate_name,
            "--configuration", configuration, "--expect-tests", str(expected), "--",
            "production.test.testOnly", "xiangshan.backend.fu.UserTimerDeliveryTest",
        ])
    if args.dry_run:
        print(json.dumps({"commands": commands, "expected_tests": 22, "executed_tests": 0}, indent=2))
        return 0
    if not runner.is_file():
        parser.error("run-suite.py must be installed beside the canonical run-gate.py")
    suite_root = run_root / "suites" / args.name
    if suite_root.exists():
        parser.error(f"suite evidence directory already exists: {suite_root}")
    for configuration, _ in variants:
        gate_root = run_root / "gates" / f"{args.name}-{configuration}"
        if gate_root.exists():
            parser.error(f"gate evidence directory already exists: {gate_root}")
    suite_root.mkdir(parents=True)
    record = {
        "commands": commands, "expected_tests": 22,
        "started": datetime.now(timezone.utc).isoformat(),
        "configuration_isolation": "Separate run-gate invocations and test JVMs; shared Scala disk cache",
    }
    (suite_root / "command.json").write_text(json.dumps(record, indent=2) + "\n")
    started = time.monotonic()
    failures = []
    gates = []
    aggregate = ET.Element("testsuites")
    actual_cases = []
    for (configuration, expected), command in zip(variants, commands):
        gate_root = run_root / "gates" / f"{args.name}-{configuration}"
        print(f"Running {configuration} configuration: {expected} expected tests", flush=True)
        result = subprocess.run(command)
        gate = {"configuration": configuration, "path": str(gate_root), "exit_code": result.returncode}
        result_file = gate_root / "result.json"
        if result_file.is_file():
            try:
                gate["result"] = json.loads(result_file.read_text())
            except json.JSONDecodeError as error:
                failures.append(f"{configuration}: invalid gate result: {error}")
        else:
            failures.append(f"{configuration}: gate result is missing")
        if result.returncode or gate.get("result", {}).get("status") != "PASS":
            failures.append(f"{configuration}: canonical gate failed")
        # The canonical gate copies only the XML from this fresh invocation into its unique directory.
        report = gate_root / "test-report.xml"
        cases = []
        if report.is_file():
            try:
                tree = ET.parse(report)
                root = tree.getroot()
                cases = list(root.iter("testcase"))
                if root.tag == "testsuite":
                    aggregate.append(root)
                else:
                    for suite in root.findall("testsuite"):
                        aggregate.append(suite)
            except ET.ParseError as error:
                failures.append(f"{configuration}: invalid gate XML: {error}")
        else:
            failures.append(f"{configuration}: fresh gate XML is missing")
        gate["actual_tests"] = len(cases)
        if len(cases) != expected:
            failures.append(f"{configuration}: expected {expected} tests, found {len(cases)}")
        actual_cases.extend(cases)
        gates.append(gate)
    names = [(case.get("classname"), case.get("name")) for case in actual_cases]
    if len(names) != len(set(names)):
        failures.append("duplicate test identities across configuration reports")
    if len(actual_cases) != 22:
        failures.append(f"expected 22 total tests, found {len(actual_cases)}")
    counts = {tag: sum(1 for case in actual_cases for _ in case.iter(tag))
              for tag in ("failure", "error", "skipped")}
    for tag, count in counts.items():
        if count:
            failures.append(f"aggregate XML contains {count} {tag} elements")
    aggregate.set("tests", str(len(actual_cases)))
    aggregate.set("failures", str(counts["failure"]))
    aggregate.set("errors", str(counts["error"]))
    aggregate.set("skipped", str(counts["skipped"]))
    ET.ElementTree(aggregate).write(suite_root / "test-report.xml", encoding="utf-8", xml_declaration=True)
    record.update({
        "gates": gates, "actual_tests": len(actual_cases),
        "passed_tests": sum(all(case.find(tag) is None for tag in counts) for case in actual_cases),
        "failure": counts["failure"], "error": counts["error"], "skipped": counts["skipped"],
        "failures": failures, "status": "FAIL" if failures else "PASS",
        "elapsed_seconds": time.monotonic() - started, "finished": datetime.now(timezone.utc).isoformat(),
    })
    (suite_root / "result.json").write_text(json.dumps(record, indent=2) + "\n")
    print(json.dumps({"suite": str(suite_root), "status": record["status"],
                      "actual_tests": record["actual_tests"], "failures": failures}), flush=True)
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
