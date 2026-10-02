#!/usr/bin/env python3
"""Run the local special-register tests with isolated, recorded inputs."""

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
        "src/test/scala/xiangshan/backend/fu/FDIMainCfgBankTest.scala",
        "src/test/scala/xiangshan/backend/fu/FDIBoundRegisterBankTest.scala",
        "src/test/scala/xiangshan/backend/fu/FDISpecialRegisterBankTest.scala",
        "tests/dasics-special-registers/build.sc",
        "tests/dasics-special-registers/run-local.py",
    ]
    manifest = []
    for relative in paths:
        original = args.source_root / relative
        files = sorted(p for p in original.rglob("*") if p.is_file()) if original.is_dir() else [original]
        for path in files:
            rel = path.relative_to(args.source_root)
            target = source / rel
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(path, target)
            row = {"path": str(rel), "sha256": digest(target), "bytes": target.stat().st_size}
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
        "scope": "Production Scala dependencies and selected local register tests",
    })
    print(f"Saved {len(manifest)} input files in {source}", flush=True)


def run(args):
    source = args.batch / "inputs" / args.source_id
    manifest = args.batch / "inputs" / f"{args.source_id}-manifest.json"
    if not manifest.is_file():
        raise ValueError("Prepare an independent input snapshot before running")
    root = args.batch / "tests" / args.attempt
    root.mkdir(parents=True)
    for name in ("logs", "tmp", "scope", "commands"):
        (root / name).mkdir()
    shutil.copy2(source / "tests/dasics-special-registers/build.sc", root / "scope/build.sc")
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
        "PATH": "/usr/lib/jvm/java-11-openjdk-amd64/bin:/usr/bin:/bin",
        "MILL_VERSION": "0.12.3", "MILL_FINAL_DOWNLOAD_FOLDER": str(args.tool_source / "mill"),
        "MILL_OUTPUT_DIR": str(args.batch / "mill-output"), "COURSIER_MODE": "offline",
        "COURSIER_REPOSITORIES": "file://" + str(args.tool_source / "maven"),
        "COURSIER_CACHE": str(args.batch / "cache"),
        "CHISEL_FIRTOOL_PATH": "/home/zengsiyuan/.cache/llvm-firtool/1.62.1/bin",
        "JAVA_TOOL_OPTIONS": f"-Xmx16G -Xss32m -XX:-UsePerfData -XX:ActiveProcessorCount={args.jobs} "
                             f"-Djava.io.tmpdir={root / 'tmp'} -Duser.home={java_runtime}",
        "C04_SOURCE_ROOT": str(source), "C04_RUN_ROOT": str(root), "DASICS_JOBS": str(args.jobs),
        "TMPDIR": str(root / "tmp"), "MAKEFLAGS": f"-j{args.jobs} VK_PCH_I_FAST= VK_PCH_I_SLOW=",
        "LC_ALL": "C", "TZ": "UTC", "GIT_CONFIG_GLOBAL": "/dev/null", "GIT_CONFIG_NOSYSTEM": "1",
    }
    env.update(settings)
    suite = {"c04": "*FDISpecialRegisterBankTest", "c03": "*FDIBoundRegisterBankTest"}[args.suite]
    argv = ["/usr/bin/mill", "--no-server", "--home", str(java_runtime), "-j", str(args.jobs),
            "production.test.testOnly", suite]
    start_wall = time.time()
    record = {"started": now(), "host": socket.gethostname(), "pid": os.getpid(), "suite": args.suite,
              "argv": argv, "cwd": str(root / "scope"), "environment": settings,
              "source_manifest": str(manifest), "source_manifest_sha256": digest(manifest),
              "driver_sha256": digest(Path(__file__)), "log": str(root / "logs/test.log"),
              "scope": "Local native CSR instances and test dispatch, no production NewCSR integration"}
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
    parser.add_argument("--source-id", default="source-r01")
    parser.add_argument("--attempt", default="c04-r01")
    parser.add_argument("--suite", choices=("c04", "c03"), default="c04")
    parser.add_argument("--jobs", type=int, default=12)
    args = parser.parse_args()
    args.batch = args.batch.resolve()
    args.source_root = args.source_root.resolve()
    if args.tool_source is not None:
        args.tool_source = args.tool_source.resolve()
    for value in (args.source_id, args.attempt):
        if not value or not all(c.isalnum() or c in "-_" for c in value):
            parser.error("Source and attempt identifiers must contain only alphanumeric, dash or underscore")
    if not 1 <= args.jobs <= 12:
        parser.error("The approved local concurrency limit is 12")
    if socket.gethostname() != "cmy-zgclab-eda":
        parser.error("This task is approved only on cmy-zgclab-eda")
    if args.action == "run" and args.tool_source is None:
        parser.error("run requires an explicit existing offline --tool-source")
    if args.action == "prepare":
        prepare(args)
    else:
        sys.exit(run(args))
