#!/usr/bin/env python3
"""Freeze critical-recovery inputs and run isolated local or Backend validation."""

import argparse
from datetime import datetime, timezone
import hashlib
import io
import json
import os
from pathlib import Path, PurePosixPath
import re
import shutil
import signal
import socket
import subprocess
import sys
import tarfile
import time
import xml.etree.ElementTree as ET


TASK_DIRECTORY = "tests/dasics-critical-recovery"
BASELINE = "ceae6bb45dd888f8ec7c16145be6890c62faceb4"
PRODUCTION_OVERLAYS = (
    "src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala",
    "src/main/scala/xiangshan/backend/fu/NewCSR/InterruptFilter.scala",
)
TEST_OVERLAYS = (
    "src/test/scala/xiangshan/backend/fu/FDICriticalRecoveryLocalHarness.scala",
    "src/test/scala/xiangshan/backend/fu/FDICriticalRecoveryLocalTest.scala",
    "src/test/scala/xiangshan/backend/fu/FDICriticalRecoveryBackendHarness.scala",
    "src/test/scala/xiangshan/backend/fu/FDICriticalRecoveryBackendTest.scala",
)
COMMITTED_SUPPORT = ("src/test/scala/xiangshan/backend/fu/UserTimerDeliveryHarness.scala",)
SUITE_SOURCES = {
    "local": TEST_OVERLAYS[:2] + COMMITTED_SUPPORT,
    "backend": TEST_OVERLAYS[2:] + COMMITTED_SUPPORT,
    "elaborate-off": (TEST_OVERLAYS[2],) + COMMITTED_SUPPORT,
}
TEST_CASES = {
    "local": (
        "xiangshan.backend.fu.FDICriticalRecoveryLocalTest",
        "Critical recovery public CSR boundaries should preserve owned effects and native controls",
    ),
    "backend": (
        "xiangshan.backend.fu.FDICriticalRecoveryBackendTest",
        "Owned critical Debug recovery through the real Backend should retain old effects and recover from owned critical sources",
    ),
}
OFF_MAIN = "xiangshan.backend.fu.FDICriticalRecoveryOffMain"
DEPENDENCIES = (
    ("rocket-chip", "", ("src/main", "macros/src/main/scala")),
    ("rocket-chip/cde", "rocket-chip", ("cde/src",)),
    ("rocket-chip/hardfloat", "rocket-chip", ("hardfloat/src/main",)),
    ("utility", "", ("src/main",)),
    ("huancun", "", ("src/main",)),
    ("coupledL2", "", ("src/main",)),
    ("openLLC", "", ("src/main",)),
    ("openLLC/openNCB", "openLLC", ("src/main",)),
    ("yunsuan", "", ("src/main",)),
    ("fudian", "", ("src/main",)),
    ("difftest", "", ("src/main",)),
    ("ChiselAIA", "", ("src/main",)),
)


def now():
    return datetime.now(timezone.utc).isoformat()


def save(path, value):
    path.write_text(json.dumps(value, indent=2) + "\n")


def relative_path(value):
    path = PurePosixPath(value)
    if path.is_absolute() or not path.parts or ".." in path.parts or str(path) != value:
        raise ValueError(f"Expected a normalized relative file path: {value}")
    return value


def prepare(args):
    revision = subprocess.check_output(
        ["git", "rev-parse", args.baseline + "^{commit}"], cwd=args.source_root, text=True).strip()
    if revision != BASELINE:
        raise ValueError("Critical preparation requires the approved committed F05 parent")
    release_bytes = args.overlay_manifest.read_bytes()
    release = json.loads(release_bytes)
    if release.get("status") != "RELEASED" or release.get("baseline") != revision:
        raise ValueError("The released overlay manifest must identify the exact baseline commit")
    support = [relative_path(path) for path in release["committed_test_sources"]]
    if len(set(support)) != len(support) or any(
            not path.startswith("src/test/") or not path.endswith(".scala") for path in support):
        raise ValueError("Committed test support must be a distinct explicit list of Scala test files")
    if support != list(COMMITTED_SUPPORT):
        raise ValueError("Critical support must be exactly the committed UserTimerDeliveryHarness")
    overlays = {}
    for entry in release["files"]:
        relative = relative_path(entry["path"])
        if relative in overlays or relative in support:
            raise ValueError(f"Duplicate or overlapping released source: {relative}")
        if not (relative in PRODUCTION_OVERLAYS or relative in TEST_OVERLAYS or
                relative in (f"{TASK_DIRECTORY}/build.sc", f"{TASK_DIRECTORY}/run-local.py")):
            raise ValueError(f"Source is outside the critical overlay boundary: {relative}")
        origin = Path(entry["source"])
        if not origin.is_absolute() or not origin.is_file():
            raise ValueError(f"Released source must name an existing absolute file: {origin}")
        payload = origin.read_bytes()
        checksum = hashlib.sha256(payload).hexdigest()
        if checksum != entry["sha256"]:
            raise ValueError(f"Released source content changed: {origin}")
        overlays[relative] = (payload, origin.stat().st_mode & 0o777, dict(
            path=relative, origin="released-overlay", source=str(origin),
            sha256=checksum, bytes=len(payload)))
    if not set(PRODUCTION_OVERLAYS).issubset(overlays):
        raise ValueError("Release both explicit critical production inputs before preparation")
    for name in ("build.sc", "run-local.py"):
        if f"{TASK_DIRECTORY}/{name}" not in overlays:
            raise ValueError(f"The release must explicitly include the critical runner input: {name}")

    source = args.batch / "inputs" / args.source_id
    manifest = args.batch / "inputs" / f"{args.source_id}-manifest.json"
    if manifest.exists():
        raise ValueError(f"Source manifest already exists: {manifest}")
    source.mkdir(parents=True)
    files = {}
    repositories = {}

    def archive(prefix, commit, paths):
        repository = args.source_root / prefix
        payload = subprocess.check_output(["git", "archive", commit, "--", *paths], cwd=repository)
        with tarfile.open(fileobj=io.BytesIO(payload)) as stream:
            for entry in stream:
                if entry.isdir():
                    continue
                if not entry.isfile():
                    raise ValueError(f"Unsupported committed source type: {prefix}/{entry.name}")
                relative = relative_path(str(PurePosixPath(prefix) / entry.name))
                destination = source / relative
                destination.parent.mkdir(parents=True, exist_ok=True)
                destination.write_bytes(stream.extractfile(entry).read())
                destination.chmod(entry.mode)
                files[relative] = dict(path=relative, origin="committed", repository=str(repository),
                                       revision=commit, bytes=entry.size)
        repositories[prefix or "."] = commit

    archive("", revision, ["src/main", "macros/src", *support])
    for prefix, parent, paths in DEPENDENCIES:
        parent_revision = repositories[parent or "."]
        component = str(PurePosixPath(prefix).relative_to(parent or "."))
        link = subprocess.check_output(["git", "ls-tree", parent_revision, "--", component],
                                       cwd=args.source_root / parent, text=True).strip()
        identity, path = link.split("\t", 1)
        mode, kind, commit = identity.split()
        if mode != "160000" or kind != "commit" or path != component:
            raise ValueError(f"The committed dependency is not the expected gitlink: {prefix}")
        archive(prefix, commit, paths)
    for relative, (payload, mode, record) in overlays.items():
        destination = source / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes(payload)
        destination.chmod(mode)
        files[relative] = record
    (source / "candidate-revision").write_text(revision + "\n")
    (source / "candidate-dirty").write_text("1\n")
    (source / "critical-overlay-release.json").write_bytes(release_bytes)
    save(manifest, {
        "status": "PREPARED", "captured": now(), "snapshot": str(source),
        "source": str(args.source_root), "revision": revision, "dirty": True,
        "overlay_manifest": str(args.overlay_manifest),
        "overlay_manifest_sha256": hashlib.sha256(release_bytes).hexdigest(),
        "committed_test_sources": support, "repositories": repositories,
        "overlays": list(overlays), "files": [files[path] for path in sorted(files)],
        "source_lock_status": "DEVELOPMENT_SNAPSHOT_NOT_SOURCE_LOCK_PASS",
        "identity_policy": "Committed archives identified by revision/gitlinks; released overlays checked once",
    })
    print(f"Saved {len(files)} source records in {source}", flush=True)


def junit_result(report, suite):
    if not report.is_file():
        return {"valid": False, "reason": "No fresh JUnit report"}
    try:
        root = ET.parse(report).getroot()
        cases = list(root.iter("testcase"))
        bad_cases = [case.get("name") for case in cases
                     if any(case.find(kind) is not None for kind in ("failure", "error", "skipped"))]
        suite_errors = any(int(suite.get(key, "0")) != 0 for suite in root.iter("testsuite")
                           for key in ("failures", "errors", "skipped"))
        result = {"valid": bool(cases) and not bad_cases and not suite_errors,
                  "tests": len(cases), "failed_or_skipped": bad_cases, "suite_errors": suite_errors}
        expected = [TEST_CASES[suite]]
        actual = sorted((case.get("classname", ""), case.get("name", "")) for case in cases)
        result.update(expected_cases=expected, actual_cases=actual, exact_cases_match=actual == expected)
        result["valid"] = result["valid"] and len(cases) == 1 and actual == expected
        return result
    except (ET.ParseError, ValueError) as error:
        return {"valid": False, "reason": str(error)}


def run(args):
    if socket.gethostname() != "cmy-zgclab-eda":
        raise ValueError("Critical execution is approved only on cmy-zgclab-eda")
    if not os.environ.get("TMUX") or not os.environ.get("TMUX_PANE"):
        raise ValueError("The root scheduler must launch critical validation in the approved named tmux session")
    session = subprocess.check_output(
        ["tmux", "display-message", "-p", "-t", os.environ["TMUX_PANE"], "#S"], text=True).strip()
    if session != args.tmux_session:
        raise ValueError(f"Expected tmux session {args.tmux_session}, got {session}")
    source = args.batch / "inputs" / args.source_id
    manifest = args.batch / "inputs" / f"{args.source_id}-manifest.json"
    prepared = json.loads(manifest.read_text())
    if prepared.get("status") != "PREPARED" or Path(prepared["snapshot"]).resolve() != source:
        raise ValueError("Prepare a completed independent input snapshot before running")
    if prepared.get("revision") != BASELINE:
        raise ValueError("The frozen input must retain the approved F05 parent")
    if Path(__file__).resolve() != (source / TASK_DIRECTORY / "run-local.py").resolve():
        raise ValueError("Run the driver from the prepared immutable source")
    files = {entry["path"]: entry for entry in prepared["files"]}
    for relative in SUITE_SOURCES[args.suite]:
        if relative not in files or not (source / relative).is_file():
            raise ValueError(f"Missing released suite input: {relative}")
    driver_digest = files[f"{TASK_DIRECTORY}/run-local.py"]["sha256"]
    for directory in ("mill", "maven", "java-runtime"):
        path = args.tool_source / directory
        if not path.is_dir() or not any(path.iterdir()):
            raise ValueError(f"Required offline tool input is absent: {path}")
    root = args.batch / "tests" / args.attempt
    root.mkdir(parents=True)
    for name in ("logs", "tmp", "scope", "commands", "build/generated-src", "rtl"):
        (root / name).mkdir(parents=True)
    shutil.copy2(source / TASK_DIRECTORY / "build.sc", root / "scope/build.sc")
    # Each group has independent mutable build state; the scheduler serializes within a group.
    group = "local" if args.suite == "local" else "backend"
    group_root = args.batch / "critical-groups" / group
    java_runtime = group_root / "tools/java-runtime"
    java_runtime.mkdir(parents=True, exist_ok=True)
    for jar in (args.tool_source / "java-runtime").glob("*.jar"):
        target = java_runtime / jar.name
        if not target.exists():
            shutil.copy2(jar, target)
    generation = args.suite == "elaborate-off"
    backend = args.suite == "backend"
    settings = {
        "JAVA_HOME": "/usr/lib/jvm/java-11-openjdk-amd64",
        "PATH": "/usr/lib/jvm/java-11-openjdk-amd64/bin:" + str(source / "src/main/resources") + ":/usr/bin:/bin",
        "MILL_VERSION": "0.12.3", "MILL_FINAL_DOWNLOAD_FOLDER": str(args.tool_source / "mill"),
        "MILL_OUTPUT_DIR": str(group_root / "mill-output"), "COURSIER_MODE": "offline",
        "COURSIER_REPOSITORIES": "file://" + str(args.tool_source / "maven"),
        "COURSIER_CACHE": str(group_root / "cache"),
        "CHISEL_FIRTOOL_PATH": "/home/zengsiyuan/.cache/llvm-firtool/1.62.1/bin",
        "JAVA_TOOL_OPTIONS": f"-Xmx{args.mill_heap} -Xss32m -XX:-UsePerfData -XX:ActiveProcessorCount={args.jobs} "
                             f"-Djava.io.tmpdir={root / 'tmp'} -Duser.home={java_runtime}",
        "CRITICAL_SOURCE_ROOT": str(source), "CRITICAL_RUN_ROOT": str(root),
        "CRITICAL_SUITE": args.suite, "CRITICAL_FDI_ENABLED": args.enabled,
        "CRITICAL_TEST_HEAP": args.test_heap or ("48G" if generation else "40G" if backend else "12G"),
        "CRITICAL_TEST_STACK": args.test_stack or ("256m" if generation or backend else "64m"),
        "DASICS_JOBS": str(args.jobs), "NOOP_HOME": str(root),
        "GIT_CEILING_DIRECTORIES": str(root), "TMPDIR": str(root / "tmp"),
        "MAKEFLAGS": f"-j{args.jobs} VK_PCH_I_FAST= VK_PCH_I_SLOW=",
        "LC_ALL": "C", "TZ": "UTC", "GIT_CONFIG_GLOBAL": "/dev/null", "GIT_CONFIG_NOSYSTEM": "1",
    }
    env = dict(os.environ, **settings)
    argv = ["/usr/bin/mill", "--no-server", "--home", str(java_runtime), "-j", str(args.jobs)]
    if generation:
        argv += ["off.runMain", OFF_MAIN]
    else:
        argv += ["production.test.testOnly", TEST_CASES[args.suite][0]]
    record = {
        "started": now(), "host": socket.gethostname(), "pid": os.getpid(), "suite": args.suite,
        "enabled": args.enabled, "tmux_session": session, "argv": argv, "cwd": str(root / "scope"),
        "environment": settings, "source_manifest": str(manifest), "source_prepared_at": prepared["captured"],
        "driver_sha256": driver_digest, "log": str(root / "logs/test.log"), "process_reaped": False,
        "scope": "Actual feature-off Backend elaboration only; generated structure review remains separate" if generation else
                 "Actual enabled Backend critical recovery with one model and released programs; no full-core or physical timing claim" if backend else
                 "Actual public CSR/HU local controls in the selected feature state; no whole-Backend or physical timing claim",
        "feature_selection": "Local accepts explicit on/off; native Backend is on only; elaborate-off is off only",
        "cache_group": group, "cache_root": str(group_root),
        "cache_concurrency": "Local and Backend groups are isolated; root serializes each group and enforces the44-worker host total",
        "identity_policy": "Release payload identities checked once during prepare; subsequent runs use the immutable manifest and frozen driver path",
    }
    if generation:
        record["generation_fork_args"] = ["-Xmx" + settings["CRITICAL_TEST_HEAP"],
                                           "-Xss" + settings["CRITICAL_TEST_STACK"]]
    save(root / "commands/test-started.json", record)
    print(json.dumps(record), flush=True)
    start_wall = time.time()
    child = None
    rc = None

    def interrupted(signum, frame):
        raise InterruptedError(f"Runner received signal {signum}")

    previous_handlers = {signum: signal.signal(signum, interrupted) for signum in (signal.SIGINT, signal.SIGTERM)}
    try:
        with (root / "logs/test.log").open("x") as log:
            log.write(json.dumps({"started": record["started"], "argv": argv}) + "\n")
            log.flush()
            child = subprocess.Popen(argv, cwd=root / "scope", env=env, start_new_session=True,
                                     stdout=log, stderr=subprocess.STDOUT)
            record["child_pid"] = child.pid
            save(root / "commands/test-started.json", record)
            rc = child.wait()
            record["process_reaped"] = True
    except (Exception, KeyboardInterrupt) as error:
        record["error"] = repr(error)
        if child is not None:
            # Only the process group created by this attempt belongs to this runner.
            for signum in previous_handlers:
                signal.signal(signum, signal.SIG_IGN)
            try:
                os.killpg(child.pid, signal.SIGTERM)
            except ProcessLookupError:
                pass
            try:
                rc = child.wait(timeout=30)
            except subprocess.TimeoutExpired:
                os.killpg(child.pid, signal.SIGKILL)
                rc = child.wait()
            record["process_reaped"] = True
    finally:
        for signum, handler in previous_handlers.items():
            signal.signal(signum, handler)
    # Persist the actual child outcome before parsing reports or inventorying generated files.
    record.update(process_finished=now(), exit_code=rc, raw_exit_code=rc)
    save(root / "commands/process-exit.json", record)
    (root / "exit-code").write_text(str(rc) + "\n")
    if generation:
        # Attempt outputs are unique; retain partial artifacts even when generation fails.
        artifacts = []
        for directory in ("rtl", "build"):
            for path in sorted((root / directory).rglob("*")):
                if path.is_file():
                    artifacts.append({"path": str(path.relative_to(root)), "bytes": path.stat().st_size})
        systemverilog = [entry for entry in artifacts if entry["path"].endswith(".sv") and entry["bytes"] > 0]
        firrtl = [entry for entry in artifacts if entry["path"].endswith(".fir") and entry["bytes"] > 0]
        inventory = root / "commands/generation-artifacts.json"
        save(inventory, {"captured": now(), "root": str(root), "files": artifacts,
                         "scope": "Generated RTL/FIRRTL and registered support files; no structural acceptance claim"})
        record["generation"] = {"valid": bool(systemverilog),
            "artifact_manifest": str(inventory), "files": len(artifacts),
            "systemverilog_files": len(systemverilog), "firrtl_files": len(firrtl),
            "acceptance": "Actual off entry exit and nonempty SystemVerilog outputs; optional FIRRTL retained, structure review separate"}
    else:
        report = group_root / "mill-output/production/test/testOnly.dest/test-report.xml"
        copied_report = root / "test-report.xml"
        if report.is_file() and report.stat().st_mtime >= start_wall:
            shutil.copy2(report, copied_report)
        record["junit"] = junit_result(copied_report, args.suite)
    passed = (rc == 0 and record["process_reaped"] and "error" not in record and
              record.get("junit", {"valid": True})["valid"] and record.get("generation", {"valid": True})["valid"])
    runner_rc = 0 if passed else (rc if rc is not None and rc > 0 else 1)
    record.update(finished=now(), exit_code=rc, raw_exit_code=rc, runner_exit_code=runner_rc, status="PASS" if passed else "FAIL")
    save(root / "commands/test.json", record)
    with (root / "logs/test.log").open("a") as log:
        log.write(json.dumps({"finished": record["finished"], "exit_code": rc, "runner_exit_code": runner_rc}) + "\n")
    print(json.dumps({"finished": record["finished"], "attempt": args.attempt,
                      "exit_code": rc, "runner_exit_code": runner_rc}), flush=True)
    return runner_rc


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("prepare", "run"))
    parser.add_argument("--batch", type=Path, required=True)
    parser.add_argument("--source-root", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--baseline", help="Explicit committed hardware parent for prepare")
    parser.add_argument("--overlay-manifest", type=Path, help="Root-released file sources and content identities")
    parser.add_argument("--tool-source", type=Path)
    parser.add_argument("--source-id", required=True)
    parser.add_argument("--attempt")
    parser.add_argument("--suite", choices=("local", "backend", "elaborate-off"))
    parser.add_argument("--enabled", choices=("true", "false"))
    parser.add_argument("--tmux-session", help="Named session owned and launched by the root scheduler")
    parser.add_argument("--jobs", type=int)
    parser.add_argument("--mill-heap", default="16G")
    parser.add_argument("--test-heap")
    parser.add_argument("--test-stack")
    args = parser.parse_args()
    for name in ("batch", "source_root", "tool_source", "overlay_manifest"):
        value = getattr(args, name)
        if value is not None:
            setattr(args, name, value.resolve())
    for value in (args.source_id, args.attempt):
        if value is not None and re.fullmatch(r"[A-Za-z0-9_-]+", value) is None:
            parser.error("Source and attempt identifiers must contain only alphanumeric, dash or underscore")
    for value in (args.mill_heap, args.test_heap, args.test_stack):
        if value is not None and re.fullmatch(r"[1-9][0-9]*[kKmMgG]", value) is None:
            parser.error("JVM heap and stack values must be positive sizes with K, M or G units")
    if args.jobs is None:
        args.jobs = 12 if args.suite == "local" else 32
    if not 1 <= args.jobs <= 44:
        parser.error("The approved total host concurrency limit is 44")
    if args.action == "prepare":
        if not args.baseline or args.overlay_manifest is None:
            parser.error("prepare requires --baseline and --overlay-manifest")
        prepare(args)
        return 0
    if any(value is None for value in (args.tool_source, args.tmux_session, args.attempt, args.suite, args.enabled)):
        parser.error("run requires --tool-source, --tmux-session, --attempt, --suite and --enabled")
    if args.suite == "backend" and args.enabled != "true":
        parser.error("backend runs the approved on model; use elaborate-off for the off Backend")
    if args.suite == "elaborate-off" and args.enabled != "false":
        parser.error("elaborate-off requires --enabled false")
    return run(args)


if __name__ == "__main__":
    sys.exit(main())
