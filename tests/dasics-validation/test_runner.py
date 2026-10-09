"""Check that incomplete or unrelated evidence cannot pass the RTL runner."""

import importlib.util
from contextlib import redirect_stderr
import io
import json
from pathlib import Path
from tempfile import TemporaryDirectory
from types import SimpleNamespace
import unittest
from unittest.mock import patch


spec = importlib.util.spec_from_file_location("rtl_runner", Path(__file__).with_name("runner.py"))
runner = importlib.util.module_from_spec(spec)
spec.loader.exec_module(runner)


class EvidenceChecks(unittest.TestCase):
    def setUp(self):
        self.tmp = TemporaryDirectory(prefix="dasics-runner-check-")
        self.addCleanup(self.tmp.cleanup)
        self.report = Path(self.tmp.name) / "report.xml"
        self.expected = [["example.Test", "must execute the approved case"]]
        self.case = '<testcase classname="example.Test" name="must execute the approved case"/>'

    def result(self, content):
        self.report.write_text(content)
        return runner.junit_result(self.report, self.expected)

    def test_exact_fresh_case_is_accepted(self):
        self.assertTrue(self.result('<testsuite tests="1">' + self.case + '</testsuite>')["valid"])

    def test_missing_report_is_rejected(self):
        self.assertFalse(runner.junit_result(self.report, self.expected)["valid"])

    def test_duplicate_expected_case_is_rejected(self):
        self.assertFalse(self.result('<testsuite>' + self.case * 2 + '</testsuite>')["valid"])

    def test_unrelated_case_is_rejected(self):
        self.assertFalse(self.result('<testsuite>' + self.case.replace('example.Test', 'other.Test') + '</testsuite>')["valid"])

    def test_skipped_and_failed_cases_are_rejected(self):
        for tag in ('skipped', 'failure', 'error'):
            with self.subTest(tag=tag):
                case = self.case.replace('/>', '><' + tag + '/></testcase>')
                self.assertFalse(self.result('<testsuite>' + case + '</testsuite>')["valid"])

    def test_suite_error_is_rejected_without_case_error(self):
        self.assertFalse(self.result('<testsuite errors="1">' + self.case + '</testsuite>')["valid"])

    def test_malformed_xml_is_rejected(self):
        self.assertFalse(self.result('<testsuite>')["valid"])

    def test_source_paths_cannot_escape_snapshot(self):
        for value in ('/absolute', '../outside', 'a/../../outside', 'a//b', ''):
            with self.subTest(value=value), self.assertRaises(ValueError):
                runner.relative_path(value)

    def test_misaligned_scenario_reaches_run_without_starting_tools(self):
        directory = Path(self.tmp.name) / "tests/dasics-load-misalign"
        directory.mkdir(parents=True)
        (directory / "suites.json").write_text(json.dumps({
            "suites": {"local": {}}, "scenarios": ["allowed", "denied-high-page-pf"],
            "default_scenario": "allowed"}))
        command = ["runner", "run", "--batch", self.tmp.name, "--source-id", "candidate",
                   "--attempt", "allowed-on", "--suite", "local", "--enabled", "true",
                   "--tool-source", self.tmp.name, "--tmux-session", "example"]
        for supplied, expected in (([], "allowed"), (["--scenario", "denied-high-page-pf"], "denied-high-page-pf")):
            with self.subTest(expected=expected), patch.object(runner.sys, "argv", command + supplied), \
                    patch.object(runner, "run", return_value=0) as run:
                self.assertEqual(runner.main(directory), 0)
                self.assertEqual(run.call_args.args[0].scenario, expected)
                self.assertEqual(run.call_args.args[0].enabled, "true")

    def test_unrecognized_or_unrelated_scenario_is_rejected(self):
        base = Path(self.tmp.name) / "tests"
        for task, scenario in (("dasics-load-misalign", "typo"), ("dasics-load-replay", "allowed")):
            directory = base / task
            directory.mkdir(parents=True)
            profile = {"suites": {"local": {}}}
            if task == "dasics-load-misalign":
                profile.update(scenarios=["allowed"], default_scenario="allowed")
            (directory / "suites.json").write_text(json.dumps(profile))
            command = ["runner", "run", "--batch", self.tmp.name, "--source-id", "candidate", "--scenario", scenario]
            with self.subTest(task=task), patch.object(runner.sys, "argv", command), \
                    patch.object(runner, "run") as run, redirect_stderr(io.StringIO()):
                with self.assertRaises(SystemExit) as rejected:
                    runner.main(base / task)
                self.assertEqual(rejected.exception.code, 2)
                run.assert_not_called()

    def test_old_baseline_is_rejected_before_capture(self):
        args = SimpleNamespace(baseline="previous-validation", source_root=Path(self.tmp.name))
        with patch.object(runner.subprocess, "check_output", side_effect=['old-parent\n', 'current-parent\n']):
            with self.assertRaisesRegex(ValueError, 'current committed hardware parent'):
                runner.prepare(args, {}, "tests/dasics-load-replay")
        self.assertFalse((Path(self.tmp.name) / 'inputs').exists())


if __name__ == "__main__":
    unittest.main()
