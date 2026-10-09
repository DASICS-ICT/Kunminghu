"""Freeze current candidates and record the approved RTL validation suites."""

import argparse
from collections import Counter
from datetime import datetime, timezone
import fcntl
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


DEPENDENCIES = (
    ("rocket-chip", "", ("src/main", "macros/src/main/scala")),
    ("rocket-chip/cde", "rocket-chip", ("cde/src",)),
    ("rocket-chip/hardfloat", "rocket-chip", ("hardfloat/src/main",)),
    ("utility", "", ("src/main",)), ("huancun", "", ("src/main",)),
    ("coupledL2", "", ("src/main",)), ("openLLC", "", ("src/main",)),
    ("openLLC/openNCB", "openLLC", ("src/main",)),
    ("yunsuan", "", ("src/main",)), ("fudian", "", ("src/main",)),
    ("difftest", "", ("src/main",)), ("ChiselAIA", "", ("src/main",)),
)
HELPER = "tests/dasics-validation/runner.py"
EXCLUDED = (
    "src/test/scala/xiangshan/backend/fu/FDIControlContextHarness.scala",
    "src/test/scala/xiangshan/backend/fu/FDIControlContextTest.scala",
)


def now():
    return datetime.now(timezone.utc).isoformat()


def save(path, value):
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_text(json.dumps(value, indent=2) + "\n")
    temporary.replace(path)


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def relative_path(value):
    path = PurePosixPath(value)
    if path.is_absolute() or not path.parts or ".." in path.parts or str(path) != value:
        raise ValueError(f"Expected a normalized relative path: {value}")
    return value


def prepare(args, profile, directory):
    revision = subprocess.check_output(
        ["git", "rev-parse", args.baseline + "^{commit}"], cwd=args.source_root, text=True).strip()
    head = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=args.source_root, text=True).strip()
    if revision != head:
        raise ValueError("Prepare must use the current committed hardware parent, not an older validation baseline")
    release_bytes = args.overlay_manifest.read_bytes()
    release = json.loads(release_bytes)
    if release.get("status") != "RELEASED" or release.get("baseline") != revision:
        raise ValueError("A root-released overlay manifest must identify the current parent")
    support = [relative_path(path) for path in release["committed_test_sources"]]
    tests = set(profile["test_sources"])
    if len(set(support)) != len(support) or not set(support).issubset(tests):
        raise ValueError("Committed test support must be an explicit subset of this suite's Scala sources")
    driver_files = {HELPER, *(directory + "/" + name for name in ("build.sc", "run-local.py", "suites.json"))}
    overlays = {}
    for entry in release["files"]:
        path = relative_path(entry["path"])
        if path in overlays or path in support or path in EXCLUDED:
            raise ValueError(f"Duplicate, overlapping or excluded input: {path}")
        allowed = path in tests or path in driver_files or path.startswith(("src/main/", "macros/src/"))
        if not allowed:
            raise ValueError(f"Input outside the approved production/test boundary: {path}")
        origin = Path(entry["source"])
        if not origin.is_absolute() or not origin.is_file():
            raise ValueError(f"Released input must name an absolute existing file: {origin}")
        payload = origin.read_bytes()
        checksum = hashlib.sha256(payload).hexdigest()
        if checksum != entry["sha256"]:
            raise ValueError(f"Released input changed: {origin}")
        if path == "src/main/scala/xiangshan/backend/fu/NewCSR/NewCSR.scala" and not entry.get("ownership_note"):
            raise ValueError("A NewCSR overlay must explicitly account for the pre-existing unrelated comment")
        overlays[path] = (payload, origin.stat().st_mode & 0o777, dict(entry, origin="released-overlay"))
    if not driver_files.issubset(overlays):
        raise ValueError("Release must contain the exact driver and profile")
    captured = set(support) | set(overlays)
    available = [name for name, suite in profile["suites"].items() if set(suite["sources"]).issubset(captured)]
    if not available:
        raise ValueError("Release must contain every source of at least one complete suite")
    test_sources = sorted({path for name in available for path in profile["suites"][name]["sources"]})
    for path in driver_files:
        if overlays[path][0] != (args.source_root / path).read_bytes():
            raise ValueError(f"Invoke prepare using the released driver/profile revision: {path}")
    source = args.batch / "inputs" / args.source_id
    manifest = args.batch / "inputs" / f"{args.source_id}-manifest.json"
    if source.exists() or manifest.exists():
        raise ValueError("Never replace an existing frozen source or manifest")
    source.mkdir(parents=True)
    files, repositories = {}, {}

    def archive(prefix, commit, paths):
        repository = args.source_root / prefix
        payload = subprocess.check_output(["git", "archive", commit, "--", *paths], cwd=repository)
        with tarfile.open(fileobj=io.BytesIO(payload)) as stream:
            for entry in stream:
                if entry.isdir():
                    continue
                if not entry.isfile():
                    raise ValueError(f"Unsupported source type: {prefix}/{entry.name}")
                relative = relative_path(str(PurePosixPath(prefix) / entry.name))
                target = source / relative
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_bytes(stream.extractfile(entry).read())
                target.chmod(entry.mode)
                files[relative] = dict(path=relative, origin="committed", repository=str(repository),
                                       revision=commit, bytes=entry.size)
        repositories[prefix or "."] = commit

    archive("", revision, ["src/main", "macros/src", *support])
    for prefix, parent, paths in DEPENDENCIES:
        component = str(PurePosixPath(prefix).relative_to(parent or "."))
        link = subprocess.check_output(
            ["git", "ls-tree", repositories[parent or "."], "--", component],
            cwd=args.source_root / parent, text=True).strip()
        identity, path = link.split("\t", 1)
        mode, kind, commit = identity.split()
        if mode != "160000" or kind != "commit" or path != component:
            raise ValueError(f"Expected a committed dependency gitlink: {prefix}")
        archive(prefix, commit, paths)
    for path, (payload, mode, record) in overlays.items():
        target = source / path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(payload)
        target.chmod(mode)
        files[path] = dict(record, bytes=len(payload))
    (source / "candidate-revision").write_text(revision + "\n")
    (source / "candidate-dirty").write_text("1\n")
    (source / "released-inputs.json").write_bytes(release_bytes)
    live_status = subprocess.check_output(
        ["git", "status", "--porcelain=v1", "--untracked-files=all"], cwd=args.source_root, text=True)
    save(manifest, dict(status="PREPARED", captured=now(), snapshot=str(source),
         source=str(args.source_root), revision=revision, dirty=True, profile=directory + "/suites.json",
         overlay_manifest=str(args.overlay_manifest), overlay_manifest_sha256=hashlib.sha256(release_bytes).hexdigest(),
         committed_test_sources=support, test_sources=test_sources, available_suites=available, repositories=repositories,
         files=[files[path] for path in sorted(files)], live_status_at_capture=live_status.splitlines(),
         excluded_inputs=list(EXCLUDED),
         ownership="Production is current HEAD plus released files only; unlisted live edits are not captured",
         source_lock_status="DEVELOPMENT_SNAPSHOT_NOT_SOURCE_LOCK_PASS"))
    print(json.dumps({"status": "PREPARED", "source": str(source), "manifest": str(manifest)}), flush=True)


def junit_result(report, expected):
    if not report.is_file():
        return {"valid": False, "reason": "No fresh JUnit report"}
    try:
        root = ET.parse(report).getroot()
        cases = list(root.iter("testcase"))
        actual = Counter((case.get("classname", ""), case.get("name", "")) for case in cases)
        wanted = Counter(tuple(case) for case in expected)
        failures = [case.get("name") for case in cases
                    if any(case.find(kind) is not None for kind in ("failure", "error", "skipped"))]
        errors = any(int(suite.get(key, "0")) != 0 for suite in root.iter("testsuite")
                     for key in ("failures", "errors", "skipped"))
        return dict(valid=bool(cases) and actual == wanted and not failures and not errors,
                    tests=len(cases), expected_cases=expected, actual_cases=list(actual.elements()),
                    failed_or_skipped=failures, suite_errors=errors, exact_cases_match=actual == wanted)
    except (ET.ParseError, ValueError) as error:
        return {"valid": False, "reason": str(error)}


def run(args, profile, directory):
    if socket.gethostname() != "cmy-zgclab-eda":
        raise ValueError("RTL compilation and simulation are approved only on cmy-zgclab-eda")
    if not os.environ.get("TMUX") or not os.environ.get("TMUX_PANE"):
        raise ValueError("The root scheduler must launch this attempt in the named tmux session")
    session = subprocess.check_output(
        ["tmux", "display-message", "-p", "-t", os.environ["TMUX_PANE"], "#S"], text=True).strip()
    if session != args.tmux_session:
        raise ValueError(f"Expected tmux session {args.tmux_session}, got {session}")
    source = args.batch / "inputs" / args.source_id
    manifest = args.batch / "inputs" / f"{args.source_id}-manifest.json"
    prepared = json.loads(manifest.read_text())
    if prepared.get("status") != "PREPARED" or Path(prepared["snapshot"]).resolve() != source:
        raise ValueError("Prepare an independent frozen input snapshot before running")
    if Path(__file__).resolve() != source / HELPER or prepared["profile"] != directory + "/suites.json":
        raise ValueError("Run the frozen task driver from the prepared snapshot")
    if args.suite != "compile" and args.suite not in prepared["available_suites"]:
        raise ValueError("Requested suite was not completely captured in this candidate")
    for name in ("mill", "maven", "java-runtime"):
        tool = args.tool_source / name
        if not tool.is_dir() or not any(tool.iterdir()):
            raise ValueError(f"Missing captured offline tool input: {tool}")
    cache = args.batch / "cache" / args.source_id
    cache.mkdir(parents=True, exist_ok=True)
    # Shared Mill outputs are safe only while this process holds the source-specific lock.
    lock = (cache / "run.lock").open("a")
    try:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        raise ValueError("Another attempt is already using this frozen source's Mill output")
    try:
        return run_locked(args, profile, directory, source, manifest, prepared, cache, session)
    finally:
        lock.close()


def run_locked(args, profile, directory, source, manifest, prepared, cache, session):
    root = args.batch / "tests" / args.attempt
    root.mkdir(parents=True)
    for name in ("logs", "tmp", "commands", "build/generated-src", "rtl"):
        (root / name).mkdir(parents=True)
    scope = cache / "scope"
    scope.mkdir(exist_ok=True)
    build_file = scope / "build.sc"
    if build_file.exists() and build_file.read_bytes() != (source / directory / "build.sc").read_bytes():
        raise ValueError("An existing source cache contains a different scoped build")
    if not build_file.exists():
        shutil.copy2(source / directory / "build.sc", build_file)
    java_runtime = cache / "java-runtime"
    java_runtime.mkdir(exist_ok=True)
    jars = sorted((args.tool_source / "java-runtime").glob("*.jar"))
    if not jars:
        raise ValueError("The offline Java runtime bootstrap jar is missing")
    for jar in jars:
        if not (java_runtime / jar.name).exists():
            shutil.copy2(jar, java_runtime / jar.name)
    settings = {
        "JAVA_HOME": "/usr/lib/jvm/java-11-openjdk-amd64",
        "PATH": "/usr/lib/jvm/java-11-openjdk-amd64/bin:" + str(source / "src/main/resources") + ":/usr/bin:/bin",
        "MILL_VERSION": "0.12.3", "MILL_FINAL_DOWNLOAD_FOLDER": str(args.tool_source / "mill"),
        "MILL_OUTPUT_DIR": str(cache / "mill-output"), "COURSIER_MODE": "offline",
        "COURSIER_REPOSITORIES": "file://" + str(args.tool_source / "maven"),
        "COURSIER_CACHE": str(cache / "coursier"),
        "CHISEL_FIRTOOL_PATH": "/home/zengsiyuan/.cache/llvm-firtool/1.62.1/bin",
        "JAVA_TOOL_OPTIONS": f"-Xmx{args.mill_heap} -Xss32m -XX:-UsePerfData -XX:ActiveProcessorCount={args.jobs} "
                             f"-Djava.io.tmpdir={root / 'tmp'} -Duser.home={java_runtime}",
        "DASICS_SOURCE_ROOT": str(source), "DASICS_RUN_ROOT": str(root),
        "DASICS_PROFILE": directory + "/suites.json", "DASICS_JOBS": str(args.jobs),
        "DASICS_TEST_SOURCES": json.dumps(prepared["test_sources"]),
        "DASICS_TEST_HEAP": args.test_heap, "DASICS_TEST_STACK": args.test_stack,
        "L01_FDI_ENABLED": args.enabled, "FDI_CONTROL_ENABLED": args.enabled,
        "FDI_CONTROL_RUN_ROOT": str(root), "FDI_CONTROL_TASKS": args.control_tasks,
        "NOOP_HOME": str(root), "BUILD_DIR": str(root / "build"),
        "GIT_CEILING_DIRECTORIES": str(root), "TMPDIR": str(root / "tmp"),
        "MAKEFLAGS": f"-j{args.jobs} VK_PCH_I_FAST= VK_PCH_I_SLOW=",
        "LC_ALL": "C", "TZ": "UTC", "GIT_CONFIG_GLOBAL": "/dev/null", "GIT_CONFIG_NOSYSTEM": "1",
    }
    if directory == "tests/dasics-load-misalign":
        settings.update(L04_FDI_ENABLED=args.enabled, L04_SCENARIO=args.scenario)
    if directory == "tests/dasics-control-completion":
        settings.update(C08_FDI_ENABLED=args.enabled, C08_FRONTEND_SCENARIO="full")
    argv = ["/usr/bin/mill", "--no-server", "--home", str(java_runtime), "-j", str(args.jobs)]
    if args.suite == "compile":
        argv += ["production.test.compile"]
    else:
        argv += ["production.test.testOnly", profile["suites"][args.suite]["class"]]
    record = dict(started=now(), host=socket.gethostname(), pid=os.getpid(), tmux_session=session,
                  suite=args.suite, enabled=args.enabled, control_tasks=args.control_tasks, scenario=args.scenario,
                  argv=argv, cwd=str(scope), environment=settings, source_manifest=str(manifest),
                  source_manifest_sha256=digest(manifest), driver_sha256=digest(Path(__file__)),
                  log=str(root / "logs/test.log"), process_reaped=False,
                  scope="Compilation only" if args.suite == "compile" else profile["suites"][args.suite]["scope"],
                  cache_policy="Source-specific locked Scala cache; native model sharing occurs only inside the Scala test")
    save(root / "commands/test-started.json", record)
    print(json.dumps({"event": "attempt-started", "attempt": args.attempt, "log": record["log"]}), flush=True)
    start = time.time_ns()
    child, rc = None, None

    def interrupted(signum, frame):
        raise InterruptedError(f"Runner received signal {signum}")

    handlers = {sig: signal.signal(sig, interrupted) for sig in (signal.SIGINT, signal.SIGTERM)}
    try:
        with (root / "logs/test.log").open("x") as log:
            log.write(json.dumps({"started": record["started"], "argv": argv}) + "\n")
            log.flush()
            child = subprocess.Popen(argv, cwd=scope, env=dict({key: value for key, value in os.environ.items() if not key.startswith("GIT_")}, **settings),
                                     stdout=log, stderr=subprocess.STDOUT, start_new_session=True)
            record["child_pid"] = child.pid
            save(root / "commands/test-started.json", record)
            rc = child.wait()
            record["process_reaped"] = True
    except (Exception, KeyboardInterrupt) as error:
        record["error"] = repr(error)
        if child is not None:
            for sig in handlers:
                signal.signal(sig, signal.SIG_IGN)
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
        for sig, handler in handlers.items():
            signal.signal(sig, handler)
    record.update(finished=now(), exit_code=rc)
    save(root / "commands/process-result.json", record)
    (root / "actual-exit-code").write_text(str(rc) + "\n")
    if args.suite != "compile":
        report = cache / "mill-output/production/test/testOnly.dest/test-report.xml"
        copied = root / "test-report.xml"
        if report.is_file() and report.stat().st_mtime_ns >= start:
            shutil.copy2(report, copied)
        record["junit"] = junit_result(copied, profile["suites"][args.suite]["expected_cases"])
    passed = rc == 0 and record["process_reaped"] and "error" not in record and record.get("junit", {"valid": True})["valid"]
    result = 0 if passed else (rc if rc is not None and rc > 0 else 1)
    record.update(status="PASS" if passed else "FAIL", runner_exit_code=result)
    save(root / "commands/test.json", record)
    (root / "exit-code").write_text(str(result) + "\n")
    with (root / "logs/test.log").open("a") as log:
        log.write(json.dumps({"finished": record["finished"], "exit_code": rc, "runner_exit_code": result}) + "\n")
    print(json.dumps({"event": "attempt-finished", "attempt": args.attempt,
                      "exit_code": rc, "runner_exit_code": result}), flush=True)
    return result


def main(task_directory):
    directory = str(task_directory.relative_to(task_directory.parents[1]))
    profile = json.loads((task_directory / "suites.json").read_text())
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("action", choices=("prepare", "run"))
    parser.add_argument("--batch", type=Path, required=True)
    parser.add_argument("--source-root", type=Path, default=task_directory.parents[1])
    parser.add_argument("--baseline")
    parser.add_argument("--overlay-manifest", type=Path)
    parser.add_argument("--tool-source", type=Path)
    parser.add_argument("--source-id", required=True)
    parser.add_argument("--attempt")
    parser.add_argument("--suite", choices=("compile", *profile["suites"]))
    parser.add_argument("--enabled", choices=("true", "false"))
    parser.add_argument("--tmux-session")
    parser.add_argument("--jobs", type=int, default=32)
    parser.add_argument("--mill-heap", default="16G")
    parser.add_argument("--test-heap", default="40G")
    parser.add_argument("--test-stack", default="256m")
    parser.add_argument("--control-tasks", default="f02")
    parser.add_argument("--scenario", choices=profile.get("scenarios", ()), default=profile.get("default_scenario"),
                        help="Explicit L04 misaligned-load scenario")
    args = parser.parse_args()
    for name in ("batch", "source_root", "overlay_manifest", "tool_source"):
        value = getattr(args, name)
        if value is not None:
            setattr(args, name, value.resolve())
    for value in (args.source_id, args.attempt):
        if value is not None and re.fullmatch(r"[A-Za-z0-9_-]+", value) is None:
            parser.error("Source and attempt identifiers must contain only alphanumeric, dash or underscore")
    for value in (args.mill_heap, args.test_heap, args.test_stack):
        if re.fullmatch(r"[1-9][0-9]*[kKmMgG]", value) is None:
            parser.error("JVM heap and stack values must be positive sizes with K, M or G units")
    if not 1 <= args.jobs <= 44:
        parser.error("The approved total host concurrency limit is 44")
    tasks = args.control_tasks.split(",")
    if not tasks or len(tasks) != len(set(tasks)) or not set(tasks).issubset({"f02"}):
        parser.error("--control-tasks must be a distinct comma-separated list of f02")
    if args.action == "prepare":
        if not args.baseline or args.overlay_manifest is None:
            parser.error("prepare requires --baseline and --overlay-manifest")
        prepare(args, profile, directory)
        return 0
    if any(value is None for value in (args.tool_source, args.tmux_session, args.attempt, args.suite, args.enabled)):
        parser.error("run requires --tool-source, --tmux-session, --attempt, --suite and --enabled")
    return run(args, profile, directory)
