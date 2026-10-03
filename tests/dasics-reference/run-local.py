#!/usr/bin/env python3
"""Run FDI observer tests and RTL generation; use run-native-reset.py for reset behavior."""

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


def prepare(args):
    source = args.batch / "inputs" / args.source_id
    source.mkdir(parents=True)
    paths = [
        "src/main", "macros/src", "rocket-chip/src/main",
        "rocket-chip/macros/src/main/scala", "rocket-chip/cde/cde/src",
        "rocket-chip/hardfloat/hardfloat/src/main", "utility/src/main",
        "huancun/src/main", "coupledL2/src/main", "openLLC/src/main",
        "openLLC/openNCB/src/main", "yunsuan/src/main", "fudian/src/main",
        "difftest/src/main", "ChiselAIA/src/main",
        "src/test/scala/top/SimTop.scala",
        "src/test/scala/top/SimMMIO.scala",
        "src/test/scala/top/DasicsDevelopmentMain.scala",
        "src/test/scala/xiangshan/backend/fu/UserTimerBaremetalMain.scala",
        "src/test/scala/xiangshan/backend/fu/UserTimerReferenceObserver.scala",
        "src/test/scala/xiangshan/backend/fu/UserTimerReferenceMain.scala",
        "src/test/scala/xiangshan/backend/fu/UserTimerCycleObservations.scala",
        "src/test/scala/xiangshan/backend/fu/UserTimerCycleMain.scala",
        "src/test/scala/xiangshan/backend/fu/UserTimerReferenceObserverTest.scala",
        "src/test/scala/xiangshan/backend/fu/FDICSRIntegrationTest.scala",
        "src/test/scala/xiangshan/backend/fu/FDIReferenceObserver.scala",
        "src/test/scala/xiangshan/backend/fu/FDIReferenceMain.scala",
        "src/test/scala/xiangshan/backend/fu/FDIReferenceObserverTest.scala",
        "src/test/scala/xiangshan/backend/fu/FDIAsyncParentResetHarness.scala",
        "src/test/scala/xiangshan/backend/fu/FDIAsyncParentResetMain.scala",
        "tests/dasics-reference/async-parent-reset.cpp",
        "tests/dasics-reference/run-native-reset.py",
        "tests/dasics-reference/build.sc",
        "tests/dasics-reference/run-local.py",
    ]
    manifest = []
    for relative in paths:
        original = (args.difftest_source_root / "src/main"
                    if relative == "difftest/src/main" and args.difftest_source_root is not None
                    else args.source_root / relative)
        files = sorted(p for p in original.rglob("*") if p.is_file()) if original.is_dir() else [original]
        for path in files:
            rel = Path(relative) / path.relative_to(original) if original.is_dir() else Path(relative)
            target = source / rel
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(path, target)
            row = {"path": str(rel), "sha256": digest(target), "bytes": target.stat().st_size}
            if relative == "difftest/src/main" and args.difftest_source_root is not None:
                row["selected_dependency_source"] = str(path)
            if path.is_symlink():
                row.update(original_symlink=os.readlink(path), resolved_source=str(path.resolve()))
            manifest.append(row)
    revision = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=args.source_root, text=True).strip()
    status = subprocess.check_output(["git", "status", "--porcelain=v1"], cwd=args.source_root,
                                     text=True, env=dict(os.environ, GIT_OPTIONAL_LOCKS="0"))
    (source / "candidate-revision").write_text(revision + "\n")
    (source / "candidate-dirty").write_text(("1" if status else "0") + "\n")
    save(args.batch / "inputs" / f"{args.source_id}-manifest.json", {
        "captured": now(), "source": str(args.source_root), "snapshot": str(source),
        "revision": revision, "dirty": bool(status), "status": status, "files": manifest,
        "source_lock_status": "DEVELOPMENT_SNAPSHOT_NOT_SOURCE_LOCK_PASS",
        "scope": "Production Scala dependencies and passive FDI observation sources",
        "difftest_source_root": str(args.difftest_source_root) if args.difftest_source_root else None,
    })
    print(f"Saved {len(manifest)} input files in {source}", flush=True)


def run(args):
    source = args.batch / "inputs" / args.source_id
    manifest = args.batch / "inputs" / f"{args.source_id}-manifest.json"
    if not manifest.is_file():
        raise ValueError("Prepare an independent input snapshot before running")
    root = args.batch / "tests" / args.attempt
    root.mkdir(parents=True)
    for name in ("logs", "tmp", "scope", "commands", "build"):
        (root / name).mkdir()
    # The Scala fork writes FileRegisters relative to its cwd; DiffTest uses
    # NOOP_HOME/build/generated-src. Both destinations belong to this attempt.
    (root / "build/generated-src").mkdir()
    (root / "rtl").mkdir()
    if root == source or root.is_relative_to(source):
        raise ValueError("Generated outputs must not be placed inside frozen source inputs")
    shutil.copy2(source / "tests/dasics-reference/build.sc", root / "scope/build.sc")
    java_runtime = args.batch / "tools/java-runtime"
    java_runtime.mkdir(parents=True, exist_ok=True)
    for jar in (args.tool_source / "java-runtime").glob("*.jar"):
        target = java_runtime / jar.name
        if not target.exists():
            shutil.copy2(jar, target)
    for path in (args.tool_source / "mill", args.tool_source / "maven", java_runtime):
        if not path.is_dir() or not any(path.iterdir()):
            raise ValueError(f"Required offline tool input is absent: {path}")
    env = os.environ.copy()
    settings = {
        "JAVA_HOME": "/usr/lib/jvm/java-11-openjdk-amd64",
        "PATH": f"/usr/lib/jvm/java-11-openjdk-amd64/bin:{source / 'src/main/resources'}:/usr/bin:/bin",
        "MILL_VERSION": "0.12.3", "MILL_FINAL_DOWNLOAD_FOLDER": str(args.tool_source / "mill"),
        "MILL_OUTPUT_DIR": str(args.batch / "mill-output"), "COURSIER_MODE": "offline",
        "COURSIER_REPOSITORIES": "file://" + str(args.tool_source / "maven"),
        "COURSIER_CACHE": str(args.batch / "cache"),
        "CHISEL_FIRTOOL_PATH": "/home/zengsiyuan/.cache/llvm-firtool/1.62.1/bin",
        "JAVA_TOOL_OPTIONS": f"-Xmx16G -Xss32m -XX:-UsePerfData -XX:ActiveProcessorCount={args.jobs} "
                             f"-Djava.io.tmpdir={root / 'tmp'} -Duser.home={java_runtime}",
        "FDI_SOURCE_ROOT": str(source), "FDI_RUN_ROOT": str(root), "DASICS_JOBS": str(args.jobs), "FDI_PROTOCOL_VERSION": "65536",
        "NOOP_HOME": str(root), "GIT_CEILING_DIRECTORIES": str(root),
        "TMPDIR": str(root / "tmp"), "MAKEFLAGS": f"-j{args.jobs} VK_PCH_I_FAST= VK_PCH_I_SLOW=",
        "LC_ALL": "C", "TZ": "UTC", "GIT_CONFIG_GLOBAL": "/dev/null", "GIT_CONFIG_NOSYSTEM": "1",
    }
    env.update(settings)
    save(root / "commands/generation-paths.json", {
        "frozen_source": str(source), "fork_working_directory": str(root),
        "noop_home": settings["NOOP_HOME"], "rtl": str(root / "rtl"),
        "file_registers": str(root / "build"),
        "difftest_generated_source": str(root / "build/generated-src"),
        "diffstate_header": str(root / "build/generated-src/diffstate.h"),
        "source_outputs_allowed": False,
    })
    argv = ["/usr/bin/mill", "--no-server", "--home", str(java_runtime), "-j", str(args.jobs)]
    if args.suite.startswith("reset-rtl-"):
        argv += ["production.test.runMain", "xiangshan.backend.fu.FDIAsyncParentResetMain",
                 "true" if args.suite == "reset-rtl-on" else "false",
                 "--target-dir", str(root / "rtl"), "--dump-fir", "--target", "systemverilog",
                 "--split-verilog", "--full-stacktrace"]
    elif args.suite.startswith("simtop-"):
        enabled = args.suite == "simtop-on"
        argv += ["production.test.runMain", "xiangshan.backend.fu.FDIReferenceMain",
                 "--config", "FpgaDefaultConfig", "--num-cores", "1",
                 "--l2-cache-size", "256", "--l3-cache-size", "768", "--enable-difftest",
                 "--has-fdi", "true" if enabled else "false",
                 "--disable-perf", "--disable-alwaysdb", "--target-dir", str(root / "rtl"),
                 "--dump-fir", "--target", "systemverilog", "--split-verilog",
                 "--firtool-opt", "--repl-seq-mem --repl-seq-mem-file=SimTop.sv.conf",
                 "--firtool-opt", "-O=release --disable-annotation-unknown "
                 "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none",
                 "--full-stacktrace"]
    elif args.suite == "compile":
        argv += ["production.test.compile"]
    else:
        suite = {"observer": "*FDIReferenceObserverTest", "uit": "*UserTimerReferenceObserverTest"}[args.suite]
        argv += ["production.test.testOnly", suite]
    start_wall = time.time()
    record = {"started": now(), "host": socket.gethostname(), "pid": os.getpid(), "suite": args.suite,
              "argv": argv, "cwd": str(root / "scope"), "environment": settings,
              "source_manifest": str(manifest), "source_manifest_sha256": digest(manifest),
              "driver_sha256": digest(Path(__file__)), "log": str(root / "logs/test.log"),
              "scope": "Passive FDI producer: real wrapper and shared UID observation or real SimTop elaboration; no processor execution"}
    save(root / "commands/test-started.json", record)
    print(json.dumps(record), flush=True)
    rc = 1
    try:
        with (root / "logs/test.log").open("x") as log:
            child = subprocess.Popen(argv, cwd=root / "scope", env=env,
                                     stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
            record["child_pid"] = child.pid
            save(root / "commands/test-started.json", record)
            for line in child.stdout:
                log.write(line)
                log.flush()
                print(line, end="", flush=True)
            rc = child.wait()
    except Exception as error:
        record["error"] = str(error)
    report = args.batch / "mill-output/production/test/testOnly.dest/test-report.xml"
    if report.is_file() and report.stat().st_mtime >= start_wall:
        shutil.copy2(report, root / "test-report.xml")
    record.update(finished=now(), exit_code=rc, status="PASS" if rc == 0 else "FAIL")
    save(root / "commands/test.json", record)
    (root / "exit-code").write_text(str(rc) + "\n")
    print(json.dumps({"finished": record["finished"], "attempt": args.attempt, "exit_code": rc}), flush=True)
    return rc


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("prepare", "run"))
    parser.add_argument("--batch", type=Path, required=True)
    parser.add_argument("--source-root", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--tool-source", type=Path)
    parser.add_argument("--difftest-source-root", type=Path,
                        help="Use an already frozen DiffTest dependency while another task edits that repository")
    parser.add_argument("--source-id", default="source-r01")
    parser.add_argument("--attempt", default="observer-r01")
    parser.add_argument("--suite", choices=("compile", "observer", "uit", "reset-rtl-on", "reset-rtl-off", "simtop-on", "simtop-off"), default="observer")
    parser.add_argument("--jobs", type=int, default=32)
    args = parser.parse_args()
    args.batch = args.batch.resolve()
    args.source_root = args.source_root.resolve()
    if args.difftest_source_root is not None:
        args.difftest_source_root = args.difftest_source_root.resolve(strict=True)
    if args.tool_source is not None:
        args.tool_source = args.tool_source.resolve()
    for value in (args.source_id, args.attempt):
        if not value or not all(c.isalnum() or c in "-_" for c in value):
            parser.error("Source and attempt identifiers must contain only alphanumeric, dash or underscore")
    if not 1 <= args.jobs <= 44:
        parser.error("The approved total host concurrency limit is 44")
    if socket.gethostname() != "cmy-zgclab-eda":
        parser.error("This task is approved only on cmy-zgclab-eda")
    if args.action == "run" and args.tool_source is None:
        parser.error("run requires an explicit existing offline --tool-source")
    if args.action == "prepare":
        prepare(args)
    else:
        sys.exit(run(args))
