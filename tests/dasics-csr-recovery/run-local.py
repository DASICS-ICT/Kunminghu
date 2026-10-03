#!/usr/bin/env python3
"""Run CSR recovery tests from the validated C06 input with recorded provenance."""

import argparse
from datetime import datetime, timezone
import hashlib
import io
import json
import os
from pathlib import Path
import re
import shutil
import socket
import subprocess
import sys
import tarfile
import time
import xml.etree.ElementTree as ET


D01_HARDWARE = "96c379c00b2bd36e83b9a21fdb10227906569e68"
D01_DIFFTEST = "05a6f054387b4fff3ce9406bfe4b7ee2998bfbe6"
# Production files are inherited or read from these commits, never live overlays.
OVERLAYS = (
    "src/test/scala/xiangshan/backend/fu/FDICSRRecoveryTest.scala",
    "src/test/scala/xiangshan/backend/fu/FDICSRRecoveryBoundaryTest.scala",
    "src/test/scala/xiangshan/backend/fu/FDICSRRecoveryObserverTest.scala",
    "src/test/scala/xiangshan/backend/fu/FDICSRRecoveryPermissionTest.scala",
    "src/test/scala/xiangshan/backend/fu/FDICSRRecoveryTrapTest.scala",
    "src/test/scala/xiangshan/backend/fu/FDICSRRecoveryBackendHarness.scala",
    "src/test/scala/xiangshan/backend/fu/FDICSRRecoveryBackendTest.scala",
    "tests/dasics-csr-recovery/build.sc",
    "tests/dasics-csr-recovery/run-local.py",
)
D01_SOURCES = tuple("src/test/scala/xiangshan/backend/fu/" + name for name in (
    "FDIReferenceObserver.scala", "FDIReferenceObserverTest.scala", "FDIReferenceMain.scala",
    "FDIAsyncParentResetHarness.scala", "FDIAsyncParentResetMain.scala",
    "UserTimerReferenceObserver.scala", "UserTimerReferenceObserverTest.scala",
    "UserTimerReferenceMain.scala",
    "UserTimerBaremetalMain.scala", "UserTimerDeliveryHarness.scala",
))
NEGATIVE_MARKERS = (
    "C07_MISSING_FLUSH_PIPE_WITH_ACCEPTED_WRITE",
    "C07_MISSING_FLUSH_PIPE_AFTER_BACKPRESSURE",
)


def now():
    return datetime.now(timezone.utc).isoformat()


def save(path, value):
    path.write_text(json.dumps(value, indent=2) + "\n")


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def prepare(args):
    previous = args.source_root.parents[2] / "build/dasics/c06-csr-distribution/20261003-r01"
    parent_source = previous / "inputs/source-r04"
    parent_manifest = previous / "inputs/source-r04-manifest.json"
    validation = previous / "validation-summary-r04.json"
    validated = json.loads(validation.read_text())
    if validated.get("source") != "source-r04" or validated.get("status") != "PASS_REGRESSION_AND_OBSERVED_GENERATION":
        raise ValueError("The approved C06 source-r04 validation receipt is required")
    inherited = json.loads(parent_manifest.read_text())
    if Path(inherited["snapshot"]).resolve() != parent_source.resolve():
        raise ValueError("C06 manifest does not identify the approved parent snapshot")
    for relative in OVERLAYS:
        if not (args.source_root / relative).is_file():
            raise ValueError(f"Required task input is absent: {relative}")
    source = args.batch / "inputs" / args.source_id
    shutil.copytree(parent_source, source)
    files = {entry["path"]: entry for entry in inherited["files"]}
    additions = []

    def archive(repository, commit, paths, prefix=""):
        payload = subprocess.check_output(["git", "archive", commit, "--", *paths], cwd=repository)
        with tarfile.open(fileobj=io.BytesIO(payload)) as stream:
            for entry in stream.getmembers():
                if entry.isdir():
                    continue
                if not entry.isfile():
                    raise ValueError(f"Unsupported committed source type: {entry.name}")
                relative = str(Path(prefix) / entry.name)
                destination = source / relative
                destination.parent.mkdir(parents=True, exist_ok=True)
                destination.write_bytes(stream.extractfile(entry).read())
                destination.chmod(entry.mode)
                files[relative] = dict(path=relative, sha256=digest(destination),
                    bytes=destination.stat().st_size, origin="committed-D01",
                    repository=str(repository), revision=commit)
                additions.append(relative)

    archive(args.source_root, D01_HARDWARE, D01_SOURCES)
    archive(args.source_root / "difftest", D01_DIFFTEST, ["src/main/scala"], "difftest")
    for relative in OVERLAYS:
        destination = source / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(args.source_root / relative, destination)
        files[relative] = dict(path=relative, sha256=digest(destination),
            bytes=destination.stat().st_size, origin="task-owned-test-or-runner-overlay",
            source=str(args.source_root / relative))
    repositories = dict(inherited["repositories"])
    repositories["difftest"] = D01_DIFFTEST
    save(args.batch / "inputs" / f"{args.source_id}-manifest.json", {
        "captured": now(), "snapshot": str(source), "revision": inherited["revision"],
        "dirty": True, "parent_snapshot": str(parent_source), "parent_manifest": str(parent_manifest),
        "parent_manifest_sha256": digest(parent_manifest), "parent_validation": str(validation),
        "repositories": repositories, "committed_additions": additions,
        "committed_hardware_addition_revision": D01_HARDWARE,
        "overlays": OVERLAYS, "files": [files[path] for path in sorted(files)],
        "source_lock_status": "DEVELOPMENT_SNAPSHOT_NOT_SOURCE_LOCK_PASS",
        "inherited_hash_policy": "Reuse the validated parent manifest; hash only added or replaced files",
        "scope": "C06 source-r04 production retained; committed D01 observers and DiffTest Scala; C07 test and runner only",
    })
    print(f"Saved {len(files)} input records in {source}", flush=True)


def classify_negative(report, log, rc):
    result = {"classification": "UNEXPECTED_FAILURE", "actual_exit_code": rc, "expected_markers": list(NEGATIVE_MARKERS)}
    if rc == 0:
        return dict(result, classification="UNEXPECTED_PASS")
    if rc < 0 or not report.is_file():
        return dict(result, reason="No completed ScalaTest report with an ordinary nonzero exit")
    try:
        tree = ET.parse(report).getroot()
    except ET.ParseError as error:
        return dict(result, reason=str(error))
    cases = list(tree.iter("testcase"))
    if len(cases) != 2 or any(tree.get(key) != value for key, value in (
        ("tests", "2"), ("failures", "2"), ("errors", "0"), ("skipped", "0"))):
        return dict(result, reason="The negative report must contain exactly two tests and two failures")
    failures = []
    matched = set()
    for case in cases:
        if case.get("classname") != "xiangshan.backend.fu.FDICSRRecoveryTest" or "c07-negative-response" not in case.get("name", ""):
            return dict(result, reason="The report contains a test outside the negative selector")
        if case.find("error") is not None or case.find("skipped") is not None:
            return dict(result, reason="A selected test was aborted or skipped")
        case_failures = case.findall("failure")
        if len(case_failures) != 1:
            return dict(result, reason="Each negative test must have exactly one failure")
        for failure in case_failures:
            message = failure.get("message", "") + " " + " ".join(failure.itertext())
            markers = [marker for marker in NEGATIVE_MARKERS if marker in message]
            if len(markers) != 1:
                return dict(result, reason="A test failed for a reason outside the expected regression")
            matched.update(markers)
            failures.append(dict(name=case.get("name"), markers=markers))
    if matched != set(NEGATIVE_MARKERS) or list(tree.iter("error")):
        return dict(result, reason="Both distinct regression failures are required without suite errors")
    output = log.read_text()
    if not all(marker in output for marker in matched):
        return dict(result, reason="The test log does not corroborate the assertion markers")
    # Mill prefixes child stdout with a numeric task ID; only SGR colors are optional.
    lines = [re.sub(r"^\[[0-9]+\] ", "", re.sub(r"\x1b\[[0-9;]*m", "", line), count=1)
             for line in output.splitlines()]
    writes = [line for line in lines if line.startswith("C07_PROVED_C1_WRITE ")]
    stalled = [line for line in lines if line.startswith("C07_PROVED_STALLED_RESPONSE ")]
    write_pattern = (r"C07_PROVED_C1_WRITE address=0x8b1 rob=4 writes=1 distributions=1 "
                     r"owner=0xf123456789abcdef stalled=(true|false) flushPipe=(true|false)")
    stalled_pattern = (r"C07_PROVED_STALLED_RESPONSE address=0x8b1 rob=4 writes=1 distributions=1 "
                       r"responses=1 firstFlush=(true|false) heldFlush=(true|false),(true|false),(true|false) "
                       r"acceptedFlush=(true|false)")
    if len(writes) != 2 or len(stalled) != 1 or not all(re.fullmatch(write_pattern, line) for line in writes) or not re.fullmatch(stalled_pattern, stalled[0]):
        return dict(result, reason="The two applied-write proofs and one stalled-response proof are required")
    if sum("stalled=false " in line for line in writes) != 1 or sum("stalled=true " in line for line in writes) != 1 or "responses=1 " not in stalled[0]:
        return dict(result, reason="The proof lines do not establish both immediate and stalled transactions")
    return dict(result, classification="EXPECTED_C07_REGRESSION", failed_tests=failures,
        matched_markers=sorted(matched), proof_lines=writes + stalled)


def run(args):
    source = args.batch / "inputs" / args.source_id
    manifest = args.batch / "inputs" / f"{args.source_id}-manifest.json"
    if not manifest.is_file():
        raise ValueError("Prepare an independent input snapshot before running")
    if not os.environ.get("TMUX"):
        raise ValueError("The root scheduler must run this command inside the approved named tmux session")
    session = subprocess.check_output(["tmux", "display-message", "-p", "#S"], text=True).strip()
    if session != args.tmux_session:
        raise ValueError(f"Expected tmux session {args.tmux_session}, got {session}")
    root = args.batch / "tests" / args.attempt
    root.mkdir(parents=True)
    for name in ("logs", "tmp", "scope", "commands", "build/generated-src", "rtl"):
        (root / name).mkdir(parents=True)
    shutil.copy2(source / "tests/dasics-csr-recovery/build.sc", root / "scope/build.sc")
    java_runtime = args.batch / "tools/java-runtime"
    java_runtime.mkdir(parents=True, exist_ok=True)
    for jar in (args.tool_source / "java-runtime").glob("*.jar"):
        target = java_runtime / jar.name
        if not target.exists():
            shutil.copy2(jar, target)
    for path in (args.tool_source / "mill", args.tool_source / "maven", java_runtime):
        if not path.is_dir() or not any(path.iterdir()):
            raise ValueError(f"Required offline tool input is absent: {path}")
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
        "C07_SOURCE_ROOT": str(source), "C07_RUN_ROOT": str(root), "C07_SUITE": args.suite,
        "C07_BACKEND_SCENARIO": {"backend-minimal": "minimal", "backend-trace": "trace"}.get(args.suite, "full"),
        "DASICS_JOBS": str(args.jobs),
        "NOOP_HOME": str(root), "GIT_CEILING_DIRECTORIES": str(root), "TMPDIR": str(root / "tmp"),
        "MAKEFLAGS": f"-j{args.jobs} VK_PCH_I_FAST= VK_PCH_I_SLOW=",
        "LC_ALL": "C", "TZ": "UTC", "GIT_CONFIG_GLOBAL": "/dev/null", "GIT_CONFIG_NOSYSTEM": "1",
    }
    env = dict(os.environ, **settings)
    argv = ["/usr/bin/mill", "--no-server", "--home", str(java_runtime), "-j", str(args.jobs)]
    if args.suite == "compile":
        argv += ["production.test.compile"]
    else:
        suites = {
            "negative": ["*FDICSRRecoveryTest"],
            "response": ["*FDICSRRecoveryTest"],
            "boundary": ["*FDICSRRecoveryBoundaryTest"],
            "local": ["*FDICSRRecoveryTest", "*FDICSRRecoveryBoundaryTest"],
            "access": ["*FDICSRIntegrationTest"],
            "distribution": ["*FDICSRDistributionTest"],
            "observer": ["*FDICSRRecoveryObserverTest"],
            "permission": ["*FDICSRRecoveryPermissionTest"],
            "reset": ["*FDICSRRecoveryBoundaryTest"],
            "satp": ["*FDICSRRecoveryBoundaryTest"],
            "trap": ["*FDICSRRecoveryTrapTest"],
            "backend": ["*FDICSRRecoveryBackendTest"],
            "backend-minimal": ["*FDICSRRecoveryBackendTest"],
            "backend-trace": ["*FDICSRRecoveryBackendTest"],
        }
        argv += ["production.test.testOnly", *suites[args.suite]]
        if args.suite == "negative":
            argv += ["--", "-z", "c07-negative-response"]
        elif args.suite == "reset":
            argv += ["--", "-z", "c07-reset-effect"]
        elif args.suite == "satp":
            argv += ["--", "-z", "c07-satp-control"]
    start_wall = time.time()
    record = {"started": now(), "host": socket.gethostname(), "pid": os.getpid(), "suite": args.suite,
        "tmux_session": session, "argv": argv, "cwd": str(root / "scope"), "environment": settings,
        "source_manifest": str(manifest), "source_manifest_sha256": digest(manifest),
        "driver_sha256": digest(Path(__file__)), "log": str(root / "logs/test.log"),
        "scope": "Production Backend/FTQ fixture execution with transport mirrors; no real ICache/LSU or physical timing"
                 if args.suite.startswith("backend") else
                 "Production wrapper or passive-observer CSR recovery tests; no processor execution or physical timing"}
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
            record["process_reaped"] = True
    except Exception as error:
        record["error"] = repr(error)
    report = args.batch / "mill-output/production/test/testOnly.dest/test-report.xml"
    copied_report = root / "test-report.xml"
    if report.is_file() and report.stat().st_mtime >= start_wall:
        shutil.copy2(report, copied_report)
    record.update(finished=now(), exit_code=rc, status="PASS" if rc == 0 else "FAIL")
    if args.suite == "negative":
        result = classify_negative(copied_report, root / "logs/test.log", rc)
        if "error" in record:
            result.update(classification="UNEXPECTED_FAILURE", execution_error=record["error"])
        save(root / "commands/negative-result.json", result)
        record["negative_classification"] = result["classification"]
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
    parser.add_argument("--attempt", default="negative-r01")
    parser.add_argument("--suite", choices=("compile", "negative", "response", "boundary", "local", "access", "distribution", "observer", "permission", "reset", "satp", "trap", "backend", "backend-minimal", "backend-trace"), default="local")
    parser.add_argument("--tmux-session", help="Named session owned and launched by the root scheduler")
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
    if args.action == "run" and (args.tool_source is None or not args.tmux_session):
        parser.error("run requires existing offline --tool-source and root-owned --tmux-session")
    if args.action == "prepare":
        prepare(args)
    else:
        sys.exit(run(args))
