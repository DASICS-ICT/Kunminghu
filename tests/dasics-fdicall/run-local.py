#!/usr/bin/env python3
"""Run production call decode and immediate transport tests with isolated, recorded inputs."""

import argparse
from datetime import datetime, timezone
import hashlib
import io
import tarfile
import json
import os
import re
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


# Shared production files are changed only through the reviewed patch.
PRODUCTION = (
    "src/main/scala/xiangshan/package.scala",
    "src/main/scala/xiangshan/backend/decode/Instructions.scala",
    "src/main/scala/xiangshan/backend/decode/DecodeUnit.scala",
    "src/main/scala/xiangshan/backend/issue/ImmExtractor.scala",
    "src/main/scala/xiangshan/backend/fu/FuConfig.scala",
)
OVERLAYS = (
    "src/test/scala/xiangshan/backend/fu/FDICallDecodeTest.scala",
    "tests/dasics-fdicall/build.sc",
    "tests/dasics-fdicall/run-local.py",
)


def prepare(args):
    source = args.batch / "inputs" / args.source_id
    source.mkdir(parents=True)
    revision = subprocess.check_output(
        ["git", "rev-parse", args.baseline + "^{commit}"], cwd=args.source_root, text=True).strip()
    files = {}
    repositories = {}

    def archive(prefix, commit, paths):
        repository = args.source_root / prefix
        payload = subprocess.check_output(["git", "archive", commit, "--", *paths], cwd=repository)
        with tarfile.open(fileobj=io.BytesIO(payload)) as stream:
            for entry in stream.getmembers():
                if entry.isdir():
                    continue
                if not entry.isfile():
                    raise ValueError(f"Unsupported archived source type: {prefix}/{entry.name}")
                relative = Path(prefix) / entry.name
                destination = source / relative
                destination.parent.mkdir(parents=True, exist_ok=True)
                destination.write_bytes(stream.extractfile(entry).read())
                destination.chmod(entry.mode)
                files[str(relative)] = {"origin": "committed", "repository": str(repository), "revision": commit}
        repositories[prefix or "."] = commit

    archive("", revision, ["src/main", "macros/src",
        "src/test/scala/top/C05BackendElaboration.scala",
        "src/test/scala/xiangshan/backend/fu/UserTimerDecodeTest.scala"])
    dependencies = [
        ("rocket-chip", "", ["src/main", "macros/src/main/scala"]),
        ("rocket-chip/cde", "rocket-chip", ["cde/src"]),
        ("rocket-chip/hardfloat", "rocket-chip", ["hardfloat/src/main"]),
        ("utility", "", ["src/main"]), ("huancun", "", ["src/main"]),
        ("coupledL2", "", ["src/main"]), ("openLLC", "", ["src/main"]),
        ("openLLC/openNCB", "openLLC", ["src/main"]),
        ("yunsuan", "", ["src/main"]), ("fudian", "", ["src/main"]),
        ("difftest", "", ["src/main"]), ("ChiselAIA", "", ["src/main"]),
    ]
    for prefix, parent, paths in dependencies:
        parent_revision = repositories[parent or "."]
        component = str(Path(prefix).relative_to(parent or "."))
        commit = subprocess.check_output(["git", "rev-parse", parent_revision + ":" + component],
                                         cwd=args.source_root / parent, text=True).strip()
        archive(prefix, commit, paths)
    payload = args.hardware_patch.read_text()
    patch_files = re.findall(r"^\+\+\+ b/(.+)$", payload, re.M)
    if set(patch_files) != set(PRODUCTION) or len(patch_files) != len(PRODUCTION):
        raise ValueError("Production patch must change exactly the call decode allowlist")
    before = {relative: digest(source / relative) for relative in PRODUCTION}
    # Exclude the enclosing product repository while applying to the exported tree.
    patch_env = dict(os.environ, GIT_CEILING_DIRECTORIES=str(source.parent))
    subprocess.run(["git", "apply", "--check", str(args.hardware_patch)],
                   cwd=source, env=patch_env, check=True)
    subprocess.run(["git", "apply", str(args.hardware_patch)],
                   cwd=source, env=patch_env, check=True)
    patch_copy = args.batch / "inputs" / f"{args.source_id}-hardware.patch"
    shutil.copy2(args.hardware_patch, patch_copy)
    for relative in PRODUCTION:
        hunks = payload.split("+++ b/" + relative + "\n", 1)[1].split("\n--- a/", 1)[0]
        files[relative].update(origin="committed-plus-task-patch", baseline_sha256=before[relative],
            patch=str(patch_copy), patch_sha256=digest(patch_copy),
            hunk_headers=re.findall(r"^@@.*@@.*$", hunks, re.M))
    for relative in OVERLAYS:
        destination = source / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(args.source_root / relative, destination)
        files[relative] = {"origin": "task-owned-overlay", "source": str(args.source_root / relative)}
    manifest = []
    for relative, provenance in sorted(files.items()):
        destination = source / relative
        manifest.append(dict(path=relative, sha256=digest(destination), bytes=destination.stat().st_size, **provenance))
    (source / "candidate-revision").write_text(revision + "\n")
    (source / "candidate-dirty").write_text("1\n")
    save(args.batch / "inputs" / f"{args.source_id}-manifest.json", {
        "captured": now(), "source": str(args.source_root), "snapshot": str(source),
        "revision": revision, "dirty": True, "repositories": repositories,
        "files": manifest, "overlays": OVERLAYS,
        "source_lock_status": "DEVELOPMENT_SNAPSHOT_NOT_SOURCE_LOCK_PASS",
        "scope": "Committed hardware baseline and gitlinked dependencies with only reviewed call decode hunks and task-owned tests",
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
    (root / "build/generated-src").mkdir()
    (root / "rtl").mkdir()
    if root == source or root.is_relative_to(source):
        raise ValueError("Generated outputs must not be placed inside frozen source inputs")
    shutil.copy2(source / "tests/dasics-fdicall/build.sc", root / "scope/build.sc")
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
        "PATH": "/usr/lib/jvm/java-11-openjdk-amd64/bin:" + str(source / "src/main/resources") + ":/usr/bin:/bin",
        "MILL_VERSION": "0.12.3", "MILL_FINAL_DOWNLOAD_FOLDER": str(args.tool_source / "mill"),
        "MILL_OUTPUT_DIR": str(args.batch / "mill-output"), "COURSIER_MODE": "offline",
        "COURSIER_REPOSITORIES": "file://" + str(args.tool_source / "maven"),
        "COURSIER_CACHE": str(args.batch / "cache"),
        "CHISEL_FIRTOOL_PATH": "/home/zengsiyuan/.cache/llvm-firtool/1.62.1/bin",
        "JAVA_TOOL_OPTIONS": f"-Xmx16G -Xss32m -XX:-UsePerfData -XX:ActiveProcessorCount={args.jobs} "
                             f"-Djava.io.tmpdir={root / 'tmp'} -Duser.home={java_runtime}",
        "F01_SOURCE_ROOT": str(source), "F01_RUN_ROOT": str(root), "DASICS_JOBS": str(args.jobs),
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
        "espresso": str(source / "src/main/resources/espresso"),
        "source_outputs_allowed": False,
    })
    suite = {"decode": "*FDICallDecodeTest", "uit": "*UserTimerDecodeTest"}.get(args.suite)
    argv = ["/usr/bin/mill", "--no-server", "--home", str(java_runtime), "-j", str(args.jobs)]
    if args.suite.startswith("backend-"):
        enabled = args.suite == "backend-on"
        argv += ["backend.runMain", "top.C05BackendElaboration", "--config", "FpgaDefaultConfig",
                 "--has-fdi", "true" if enabled else "false", "--fpga-platform", "--disable-perf",
                 "--disable-alwaysdb", "--target-dir", str(root / "rtl"), "--dump-fir",
                 "--target", "systemverilog", "--split-verilog",
                 "--firtool-opt", "--repl-seq-mem --repl-seq-mem-file=Backend.sv.conf",
                 "--firtool-opt", "-O=release --disable-annotation-unknown "
                 "--lowering-options=explicitBitcast,disallowLocalVariables,disallowPortDeclSharing,locationInfoStyle=none",
                 "--full-stacktrace"]
    else:
        argv += ["production.test.compile"] if args.suite == "compile" else ["production.test.testOnly", suite]
    start_wall = time.time()
    record = {"started": now(), "host": socket.gethostname(), "pid": os.getpid(), "suite": args.suite,
              "argv": argv, "cwd": str(root / "scope"), "environment": settings,
              "source_manifest": str(manifest), "source_manifest_sha256": digest(manifest),
              "driver_sha256": digest(Path(__file__)), "log": str(root / "logs/test.log"),
              "scope": "Production call decode tests or Backend immediate transport elaboration; no processor execution"}
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
    parser.add_argument("--baseline", required=True, help="Committed hardware baseline before this task")
    parser.add_argument("--hardware-patch", type=Path, help="Reviewed production-only patch for prepare")
    parser.add_argument("--source-id", default="source-r01")
    parser.add_argument("--attempt", default="decode-r01")
    parser.add_argument("--suite", choices=("compile", "decode", "uit", "backend-on", "backend-off"), default="decode")
    parser.add_argument("--jobs", type=int, default=32)
    args = parser.parse_args()
    args.batch = args.batch.resolve()
    args.source_root = args.source_root.resolve()
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
        if args.hardware_patch is None:
            parser.error("prepare requires --hardware-patch; shared live files are never copied")
        args.hardware_patch = args.hardware_patch.resolve(strict=True)
        prepare(args)
    else:
        sys.exit(run(args))
