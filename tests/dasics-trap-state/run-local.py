#!/usr/bin/env python3
"""Run trap-state validation using an existing frozen source tree and offline tools."""

import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import shutil
import socket
import subprocess
import sys
import time


def now():
    return datetime.now(timezone.utc).isoformat()


def save(path, value):
    path.write_text(json.dumps(value, indent=2) + "\n")


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def run(args):
    source = args.source_root
    for name in ("candidate-revision", "candidate-dirty", "tests/dasics-trap-state/build.sc"):
        if not (source / name).is_file():
            raise ValueError(f"Required frozen input is absent: {source / name}")
    if not args.source_manifest.is_file():
        raise ValueError("An existing frozen source manifest is required")
    for name in ("mill", "maven", "java-runtime"):
        path = args.tool_source / name
        if not path.is_dir() or not any(path.iterdir()):
            raise ValueError(f"Required offline tool input is absent: {path}")

    root = args.batch / "tests" / args.attempt
    root.mkdir(parents=True)
    for name in ("logs", "tmp", "scope", "commands", "build"):
        (root / name).mkdir()
    shutil.copy2(source / "tests/dasics-trap-state/build.sc", root / "scope/build.sc")
    java_runtime = args.batch / "tools/java-runtime"
    java_runtime.mkdir(parents=True, exist_ok=True)
    for jar in (args.tool_source / "java-runtime").glob("*.jar"):
        target = java_runtime / jar.name
        if not target.exists():
            shutil.copy2(jar, target)

    env = os.environ.copy()
    settings = {
        "JAVA_HOME": "/usr/lib/jvm/java-11-openjdk-amd64",
        "PATH": "/usr/lib/jvm/java-11-openjdk-amd64/bin:" + str(source / "src/main/resources") + ":/usr/bin:/bin",
        "MILL_VERSION": "0.12.3", "MILL_FINAL_DOWNLOAD_FOLDER": str(args.tool_source / "mill"),
        "MILL_OUTPUT_DIR": str(args.batch / "mill-output"), "COURSIER_MODE": "offline",
        "COURSIER_REPOSITORIES": "file://" + str(args.tool_source / "maven"),
        "COURSIER_CACHE": str(args.batch / "cache"),
        "CHISEL_FIRTOOL_PATH": "/home/zengsiyuan/.cache/llvm-firtool/1.62.1/bin",
        "JAVA_TOOL_OPTIONS": f"-Xmx16G -Xss32m -XX:-UsePerfData -XX:ActiveProcessorCount={args.jobs} "
                             f"-Djava.io.tmpdir={root / 'tmp'} -Duser.home={java_runtime}",
        "E02_SOURCE_ROOT": str(source), "E02_RUN_ROOT": str(root), "DASICS_JOBS": str(args.jobs),
        "TMPDIR": str(root / "tmp"), "MAKEFLAGS": f"-j{args.jobs} VK_PCH_I_FAST= VK_PCH_I_SLOW=",
        "LC_ALL": "C", "TZ": "UTC", "GIT_CONFIG_GLOBAL": "/dev/null", "GIT_CONFIG_NOSYSTEM": "1",
    }
    env.update(settings)
    argv = ["/usr/bin/mill", "--no-server", "--home", str(java_runtime), "-j", str(args.jobs)]
    if args.suite.startswith("backend-"):
        enabled = args.suite == "backend-on"
        argv += ["backend.runMain", "top.C05BackendElaboration", "--config", "FpgaDefaultConfig",
                 "--has-fdi", "true" if enabled else "false", "--fpga-platform", "--disable-perf",
                 "--disable-alwaysdb", "--target-dir", str(root / "rtl"), "--target", "systemverilog",
                 "--split-verilog"]
    elif args.suite == "reset-rtl-on":
        (root / "rtl").mkdir()
        argv += ["production.test.runMain", "xiangshan.backend.fu.FDIAsyncParentResetMain", "true",
                 "--target-dir", str(root / "rtl"), "--dump-fir", "--target", "systemverilog",
                 "--split-verilog", "--full-stacktrace"]
    elif args.suite == "compile":
        argv += ["production.test.compile"]
    else:
        suites = {
            "trap": ["*FDITrapStateTest"],
            "composed": ["*FDISelectorTrapTest"],
            "selector": ["*FDIExceptionRecordTest"],
            "access": ["*FDICSRIntegrationTest"],
            "distribution": ["*FDICSRDistributionTest"],
            "uit": ["*UserTimerCSRIntegrationTest", "*UserTimerEntryReturnTest"],
        }
        argv += ["production.test.testOnly", *suites[args.suite]]

    start_wall = time.time()
    record = {"started": now(), "host": socket.gethostname(), "pid": os.getpid(), "suite": args.suite,
              "argv": argv, "cwd": str(root / "scope"), "environment": settings,
              "source_root": str(source), "source_manifest": str(args.source_manifest),
              "source_manifest_sha256": digest(args.source_manifest),
              "driver_sha256": digest(Path(__file__)), "log": str(root / "logs/test.log"),
              "process_reaped": False,
              "scope": "Production CSR trap tests, affected regressions or Backend generation; no processor execution"}
    save(root / "commands/test-started.json", record)
    print(json.dumps(record), flush=True)
    rc = 1
    try:
        with (root / "logs/test.log").open("x") as log:
            with subprocess.Popen(argv, cwd=root / "scope", env=env,
                                  stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True) as child:
                record["child_pid"] = child.pid
                save(root / "commands/test-started.json", record)
                for line in child.stdout:
                    log.write(line)
                    log.flush()
                    print(line, end="", flush=True)
                rc = child.wait()
                record["process_reaped"] = True
    except Exception as error:
        record["error"] = str(error)
    report = args.batch / "mill-output/production/test/testOnly.dest/test-report.xml"
    if args.suite in ("trap", "composed", "selector", "uit", "access", "distribution") and report.is_file() and report.stat().st_mtime >= start_wall:
        shutil.copy2(report, root / "test-report.xml")
    record.update(finished=now(), exit_code=rc, status="PASS" if rc == 0 else "FAIL")
    save(root / "commands/test.json", record)
    (root / "exit-code").write_text(str(rc) + "\n")
    print(json.dumps({"finished": record["finished"], "attempt": args.attempt, "exit_code": rc}), flush=True)
    return rc


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("run",))
    parser.add_argument("--batch", type=Path, required=True)
    parser.add_argument("--source-root", type=Path, default=os.environ.get("E02_SOURCE_ROOT"),
                        help="Existing frozen input tree; defaults to E02_SOURCE_ROOT")
    parser.add_argument("--source-manifest", type=Path, required=True)
    parser.add_argument("--tool-source", type=Path, required=True)
    parser.add_argument("--attempt", required=True)
    parser.add_argument("--suite", choices=("compile", "trap", "composed", "selector", "uit", "access",
                                           "distribution", "reset-rtl-on", "backend-on", "backend-off"),
                        required=True)
    parser.add_argument("--jobs", type=int, default=12)
    args = parser.parse_args()
    if args.source_root is None:
        parser.error("Set --source-root or E02_SOURCE_ROOT to an already frozen source tree")
    args.batch = args.batch.resolve()
    args.source_root = args.source_root.resolve(strict=True)
    args.source_manifest = args.source_manifest.resolve(strict=True)
    args.tool_source = args.tool_source.resolve(strict=True)
    if not args.source_root.is_relative_to(args.batch / "inputs"):
        parser.error("Source input must be an existing snapshot under the approved batch inputs directory")
    if not args.attempt or not all(c.isalnum() or c in "-_" for c in args.attempt):
        parser.error("Attempt identifiers must contain only alphanumeric, dash or underscore")
    if not 1 <= args.jobs <= 44:
        parser.error("The approved total host concurrency limit is 44")
    if socket.gethostname() != "cmy-zgclab-eda":
        parser.error("This task is approved only on cmy-zgclab-eda")
    sys.exit(run(args))
