#!/usr/bin/env python3
"""Exercise the actual scoped Mill fork settings without compiling production RTL."""

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


PROFILES = ("dasics-load-replay", "dasics-control-completion", "dasics-load-misalign")
ENVIRONMENT_KEYS = (
    "DASICS_RUN_ROOT", "DASICS_JOBS", "DASICS_TEST_HEAP", "DASICS_TEST_STACK",
    "L01_FDI_ENABLED", "FDI_CONTROL_ENABLED", "FDI_CONTROL_RUN_ROOT", "FDI_CONTROL_TASKS",
    "L04_FDI_ENABLED", "L04_SCENARIO", "NOOP_HOME", "BUILD_DIR", "TMPDIR", "MAKEFLAGS",
)
JAVA_SOURCE = '''import java.nio.file.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public final class RuntimeProbe {
    public static void main(String[] args) throws Exception {
        List<String> rows = new ArrayList<>();
        rows.add("cwd=" + Paths.get("").toRealPath());
        for (String key : args) {
            String value = key.startsWith("prop:")
                ? System.getProperty(key.substring(5)) : System.getenv(key);
            rows.add(key + "=" + (value == null ? "<unset>" : value));
        }
        Files.write(Paths.get("child.txt"), rows, StandardCharsets.UTF_8);
        System.out.println("RUNTIME_CHILD_PASS");
    }
}
'''


def now():
    return datetime.now(timezone.utc).isoformat()


def save(path, value):
    path.write_text(json.dumps(value, indent=2) + "\n")


def extract_fork_settings(build):
    runtime = build.split("    def runtimeEnvironment = T.input {", 1)[1].split("    override def sources", 1)[0]
    runtime = "    def runtimeEnvironment = T.input {" + runtime
    forks = build.split("    override def forkArgs = T {", 1)[1].split("\n  }\n}", 1)[0]
    forks = "    override def forkArgs = T {" + forks
    forks = forks.replace("    override def testSandboxWorkingDir = false\n", "")
    return runtime, forks


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, required=True)
    parser.add_argument("--tool-source", type=Path, required=True)
    parser.add_argument("--tmux-session", required=True)
    parser.add_argument("--profile", choices=PROFILES, action="append", help="Check only the selected existing profile")
    args = parser.parse_args()
    root, tools = args.root.resolve(), args.tool_source.resolve()
    if socket.gethostname() != "cmy-zgclab-eda" or not os.environ.get("TMUX_PANE"):
        parser.error("Run this local tool gate in its root-owned named tmux session")
    session = subprocess.check_output(
        ["tmux", "display-message", "-p", "-t", os.environ["TMUX_PANE"], "#S"], text=True).strip()
    if session != args.tmux_session:
        parser.error("The actual tmux session does not match --tmux-session")
    for name in ("mill", "maven", "java-runtime"):
        if not (tools / name).is_dir() or not any((tools / name).iterdir()):
            parser.error("Missing offline tools: " + name)
    root.mkdir(parents=True)
    shutil.copy2(Path(__file__), root / "check-runtime.py")
    home = root / "java-runtime"
    home.mkdir()
    for jar in (tools / "java-runtime").glob("*.jar"):
        shutil.copy2(jar, home / jar.name)
    summary = dict(started=now(), host=socket.gethostname(), tmux_session=session,
                   scope="Small Java child through exact scoped Mill fork methods; no production RTL", profiles=[])
    save(root / "result.json", dict(summary, status="RUNNING"))
    task_root = Path(__file__).resolve().parents[1]
    try:
        for profile in args.profile or PROFILES:
            path = root / profile
            path.mkdir()
            build_file = task_root / profile / "build.sc"
            build = build_file.read_text()
            (path / "source-build.sc").write_text(build)
            runtime, forks = extract_fork_settings(build)
            scope = path / "scope"
            scope.mkdir()
            (scope / "src").mkdir()
            (scope / "src/RuntimeProbe.java").write_text(JAVA_SOURCE)
            (scope / "build.sc").write_text(
                'import mill._\nimport mill.scalalib._\n\nobject probe extends JavaModule {\n'
                '  override def sources = T.sources(Seq(PathRef(os.Path(' + json.dumps(str(scope / "src")) + '))))\n'
                + runtime + forks + '\n}\n')
            profile_record = dict(profile=profile, build_sha256=hashlib.sha256(build_file.read_bytes()).hexdigest(), runs=[])
            summary["profiles"].append(profile_record)
            for index, (enabled, scenario, tasks, jobs) in enumerate((
                    ("true", "allowed", "f02", "2"),
                    ("false", "boundaries", "f03", "3"),
                    ("false", "denied-high-page-pf", "f02,f03,f04", "4")), start=1):
                attempt = path / f"attempt-{index}"
                attempt.mkdir()
                (attempt / "tmp").mkdir()
                settings = {
                    "JAVA_HOME": "/usr/lib/jvm/java-11-openjdk-amd64",
                    "PATH": "/usr/lib/jvm/java-11-openjdk-amd64/bin:/usr/bin:/bin",
                    "MILL_VERSION": "0.12.3", "MILL_FINAL_DOWNLOAD_FOLDER": str(tools / "mill"),
                    "MILL_OUTPUT_DIR": str(path / "mill-output"), "COURSIER_MODE": "offline",
                    "COURSIER_REPOSITORIES": "file://" + str(tools / "maven"),
                    "COURSIER_CACHE": str(path / "coursier"),
                    "JAVA_TOOL_OPTIONS": f"-Xmx1G -XX:-UsePerfData -XX:ActiveProcessorCount={jobs} -Duser.home={home}",
                    "DASICS_RUN_ROOT": str(attempt), "DASICS_JOBS": jobs,
                    "DASICS_TEST_HEAP": "256M", "DASICS_TEST_STACK": "2M", "DASICS_TEST_SOURCES": "[]",
                    "L01_FDI_ENABLED": enabled, "FDI_CONTROL_ENABLED": enabled,
                    "FDI_CONTROL_RUN_ROOT": str(attempt), "FDI_CONTROL_TASKS": tasks,
                    "L04_FDI_ENABLED": enabled, "L04_SCENARIO": scenario,
                    "NOOP_HOME": str(attempt), "BUILD_DIR": str(attempt / "build"), "TMPDIR": str(attempt / "tmp"),
                    "GIT_CEILING_DIRECTORIES": str(attempt), "LC_ALL": "C", "TZ": "UTC",
                }
                properties = ["prop:l04.runRoot"] if profile == "dasics-load-misalign" else ["prop:l01.runRoot", "prop:f02.runRoot"]
                environment_keys = list(ENVIRONMENT_KEYS)
                if profile == "dasics-control-completion":
                    settings.update(C08_FDI_ENABLED=enabled, C08_FRONTEND_SCENARIO="minimal" if index == 1 else "full")
                    properties.append("prop:c08.runRoot")
                    environment_keys += ["C08_FDI_ENABLED", "C08_FRONTEND_SCENARIO"]
                arguments = [*environment_keys, *properties]
                command = ["/usr/bin/mill", "--no-server", "--home", str(home), "-j", jobs,
                           "probe.runMain", "RuntimeProbe", *arguments]
                record = dict(started=now(), command=command, cwd=str(scope), environment=settings)
                save(attempt / "started.json", record)
                with (attempt / "test.log").open("x") as log:
                    log.write(json.dumps({"started": record["started"]}) + "\n")
                    log.flush()
                    result = subprocess.run(command, cwd=scope, env=dict(os.environ, **settings),
                                            stdout=log, stderr=subprocess.STDOUT)
                    record.update(finished=now(), exit_code=result.returncode)
                    log.write(json.dumps({"finished": record["finished"], "exit_code": result.returncode}) + "\n")
                (attempt / "exit-code").write_text(str(result.returncode) + "\n")
                save(attempt / "result.json", record)
                if result.returncode != 0:
                    raise ValueError(f"Actual Mill invocation failed: {attempt}")
                actual = dict(line.split("=", 1) for line in (attempt / "child.txt").read_text().splitlines())
                expected = {key: settings[key] for key in environment_keys if key != "MAKEFLAGS"}
                expected.update({key: str(attempt) for key in properties})
                expected.update(cwd=str(attempt), MAKEFLAGS=f"-j{jobs} VK_PCH_I_FAST= VK_PCH_I_SLOW=")
                record.update(actual=actual, expected=expected, status="PASS" if actual == expected else "FAIL")
                save(attempt / "result.json", record)
                profile_record["runs"].append(record)
                save(root / "result.json", dict(summary, status="RUNNING"))
                if actual != expected:
                    raise ValueError(f"The real child saw stale or missing fork settings: {attempt}")
        summary.update(status="PASS", finished=now(), exit_code=0)
    except Exception as error:
        summary.update(status="FAIL", finished=now(), exit_code=1, error=repr(error))
    save(root / "result.json", summary)
    (root / "exit-code").write_text(str(summary["exit_code"]) + "\n")
    print(json.dumps({"status": summary["status"], "result": str(root / "result.json")}), flush=True)
    return summary["exit_code"]


if __name__ == "__main__":
    sys.exit(main())
