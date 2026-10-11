#!/usr/bin/env python3
"""Export unchanged generated RTL from a finished test for OOC analysis."""

import argparse
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import re
import sys


TOPS = {
    "backend": "FDIFullControlHarness",
    "frontend": "FDITrustMetadataFrontendHarness",
    "load-misalign": "FDILoadMisalignHarness",
}
MARKERS = ("fdiNotTrusted", "fdiRawRVC", "fdiInstr", "fdiTrapInst")
# Ignore comments and strings: only Verilog identifiers establish field presence.
NON_CODE = re.compile(r'//[^\n]*|/\*.*?\*/|"(?:\\.|[^"\\])*"', re.S)
IDENTIFIER = re.compile(r"[A-Za-z_$][A-Za-z0-9_$]*|\\[^\s]+")
MODULE = re.compile(r"\bmodule\s+(?:(?:automatic|static)\s+)?([A-Za-z_$][\w$]*)\b")
SYNTHESIS_FIXTURES = {
    "backend": {"sources": {"src/test/scala/xiangshan/backend/fu/" + name for name in
                            ("UserTimerDeliveryHarness.scala", "FDIFullControlHarness.scala")},
                "suite": "control", "parameters": {"parameter_factory": "UserTimerDeliveryParameters"}},
    "load-misalign": {"sources": {"src/test/scala/xiangshan/mem/FDILoadMisalignHarness.scala"},
        "suite": "local", "parameters": {"parameter_factory": "FDILoadMisalignTest configuration",
            "parameter_args": ["--config", "FpgaDefaultConfig", "--num-cores", "1",
                "--l2-cache-size", "256", "--l3-cache-size", "768", "--fpga-platform",
                "--disable-always-basic-diff", "--disable-perf", "--disable-alwaysdb"],
            "vector_length_bits": 128, "load_to_load_forwarding": False}},
}
RUNNER = "tests/dasics-validation/runner.py"


def require(condition, message):
    if not condition:
        raise ValueError(message)


def sha256(data):
    return hashlib.sha256(data).hexdigest()


def source_provenance(run, feature):
    command_path = run / "commands/test.json"
    command = json.loads(command_path.read_bytes())
    receipt = run / "actual-exit-code"
    legacy = not receipt.is_file()
    receipt = run / "exit-code" if legacy else receipt
    rc = int(receipt.read_text().strip())
    require(command.get("finished") and command.get("process_reaped") is True and
            command.get("exit_code") == rc, "source run has no matching completed process receipt")
    require(not legacy or command.get("runner_exit_code") == rc,
            "legacy source run exit codes disagree")
    require(command.get("enabled") == ("true" if feature == "on" else "false"),
            "source run feature disagrees with the export identity")
    path = Path(command["source_manifest"]).resolve(strict=True)
    data = path.read_bytes()
    require(sha256(data) == command.get("source_manifest_sha256"), "source manifest checksum mismatch")
    return command, json.loads(data), {"run": str(run), "source_test_exit_code": rc,
        "exit_code_file": str(receipt), "source_manifest": str(path), "source_manifest_sha256": sha256(data)}


def equivalent_sources(emit_manifest, sim_manifest, extra=(), fixture="backend"):
    required = SYNTHESIS_FIXTURES[fixture]["sources"] | set(extra)
    def selected(manifest):
        paths = [f["path"] for f in manifest["files"]]
        require(len(set(paths)) == len(paths), "duplicate source manifest paths")
        result = {p for p in paths if "/src/main/" in "/" + p or p in required}
        require(required <= result and len(result) > len(required), "incomplete production/fixture manifest")
        return result
    paths = selected(emit_manifest)
    require(paths == selected(sim_manifest), "production/fixture source path sets disagree")
    roots = [Path(m["snapshot"]).resolve(strict=True) for m in (emit_manifest, sim_manifest)]
    for relative in sorted(paths):
        files = [(root / relative).resolve(strict=True) for root in roots]
        require(all(p.is_relative_to(root) for p, root in zip(files, roots)), "source path escapes snapshot")
        require(files[0].read_bytes() == files[1].read_bytes(), f"production/fixture source differs: {relative}")
    return len(paths)


def synthesis_relation(run, simulation, feature, fixture="backend"):
    # Runner overlays may differ; production inputs and selected fixture definitions must not.
    emit_command, emit_manifest, emit = source_provenance(run, feature)
    sim_command, sim_manifest, sim = source_provenance(simulation, feature)
    require(emit_command.get("suite") == "timing-emit" and
            sim_command.get("suite") == SYNTHESIS_FIXTURES[fixture]["suite"],
            "source runs must be a timing emission and its original fixture simulation")
    require(emit["source_test_exit_code"] == 0 and emit_command.get("runner_exit_code") == 0 and
            emit_command.get("status") == "PASS", "synthesis-view emission did not pass")
    tool = emit_command["environment"].get("CHISEL_FIRTOOL_PATH")
    require(tool and tool == sim_command["environment"].get("CHISEL_FIRTOOL_PATH"),
            "simulation and synthesis view use different FIRTool directories")
    count = equivalent_sources(emit_manifest, sim_manifest, fixture=fixture)
    return {"method": "Exact byte comparison of all production inputs and selected fixture files",
            "compared_files": count, "emit": emit, "simulation": sim, "firtool_directory": tool,
            "relation": "Same frozen production and fixture sources; not byte-identical simulation RTL"}


def candidate_relation(run, candidate_path, feature, fixture="backend"):
    command, emit_manifest, emit = source_provenance(run, feature)
    require(command.get("suite") == "timing-emit" and emit["source_test_exit_code"] == 0 and
            command.get("runner_exit_code") == 0 and command.get("status") == "PASS",
            "synthesis-view emission did not pass")
    candidate_path = candidate_path.resolve(strict=True)
    data = candidate_path.read_bytes()
    candidate = json.loads(data)
    require(candidate.get("status") == "PREPARED" and
            SYNTHESIS_FIXTURES[fixture]["suite"] in candidate.get("available_suites", []),
            "candidate is not a frozen functional source for the requested fixture")
    count = equivalent_sources(emit_manifest, candidate, (RUNNER,), fixture)
    tool = command["environment"].get("CHISEL_FIRTOOL_PATH")
    runner = (Path(candidate["snapshot"]) / RUNNER).read_text()
    fixed_tools = re.findall(r'["\']CHISEL_FIRTOOL_PATH["\']\s*:\s*["\']([^"\']+)["\']', runner)
    require(tool and fixed_tools == [tool], "frozen runner does not bind the observed emit FIRTool directory")
    return {"method": "Exact production, fixture and frozen neutral-runner byte comparison",
        "compared_files": count, "emit": emit, "firtool_directory": tool,
        "simulation": {"run": None, "source_test_exit_code": None, "exit_code_file": None,
            "source_manifest": str(candidate_path), "source_manifest_sha256": sha256(data),
            "snapshot": str(Path(candidate["snapshot"]).resolve()), "functional_status": "PENDING",
            "planned_feature": feature, "observed_functional_feature": None,
            "planned_parameter_factory": SYNTHESIS_FIXTURES[fixture]["parameters"]["parameter_factory"],
            "planned_parameter_metadata": SYNTHESIS_FIXTURES[fixture]["parameters"],
            "planned_firtool_directory": tool},
        "observed_emit_feature": feature,
        "relation": "Frozen functional candidate only; no completed fixture simulation is claimed",
        "followup": "Append a completed functional-run acceptance record without replacing this PENDING record; changed production or fixture sources require a new emit and timing review"}


def export(args):
    run = args.run_directory.resolve(strict=True)
    receipt = run / "actual-exit-code"
    completion = None
    completion_bytes = None
    if not receipt.is_file():
        require(args.fixture == "frontend", "run has no actual-exit-code; refusing unfinished run")
        completion = run / "commands/test.json"
        completion_bytes = completion.read_bytes()
        details = json.loads(completion_bytes)
        require(details.get("finished") and details.get("process_reaped") is True and
                details.get("exit_code") == details.get("runner_exit_code") == 0 and
                details.get("status") == "PASS", "legacy frontend run is not successfully finished")
        receipt = run / "exit-code"
    receipt_bytes = receipt.read_bytes()
    test_rc = int(receipt_bytes.decode().strip())
    require(completion is None or test_rc == 0, "legacy frontend receipt exit codes disagree")
    top = TOPS[args.fixture]
    view_path = run / "synthesis-view.json"
    synthesis = view_path.is_file()
    view_bytes = view_path.read_bytes() if synthesis else None
    view = json.loads(view_bytes) if synthesis else None
    relation = None
    if synthesis:
        require(args.fixture in SYNTHESIS_FIXTURES and test_rc == 0 and
                (args.simulation_run_directory or args.simulation_source_manifest),
                "synthesis-view export requires a supported fixture, successful emit, and a simulation binding")
        relation = (synthesis_relation(run, args.simulation_run_directory.resolve(strict=True), args.feature, args.fixture)
                    if args.simulation_run_directory else
                    candidate_relation(run, args.simulation_source_manifest, args.feature, args.fixture))
        expected = {"schema_version": 1, "status": "SYNTHESIS_VIEW_EMITTED", "top": top,
            "feature": args.feature,
            "generator": "top.Generator / top.XiangShanStage", "chisel_version": "6.6.0",
            "verification_transform": "xiangshan.transforms.PrintControl / RemoveAssertAnnotation",
            "removed_statement_kinds": ["firrtl.ir.Stop", "firrtl.ir.Verification"],
            "firtool_options": ["-disable-annotation-unknown"], "simulation_rtl_modified": False,
            "simulation_assertions_disabled": False, "firtool_directory": relation["firtool_directory"],
            "chisel_args": ["--target", "systemverilog", "--split-verilog", "--target-dir",
                            str(run / "synthesis-rtl"), "--remove-assert"]}
        expected.update(SYNTHESIS_FIXTURES[args.fixture]["parameters"])
        require(all(view.get(k) == v for k, v in expected.items()), "unexpected synthesis-view emission contract")
    else:
        require(args.fixture != "load-misalign", "load-misalign export requires a standard synthesis view")
        require(args.simulation_run_directory is None and args.simulation_source_manifest is None,
                "simulation binding arguments require a synthesis view")
    native = not synthesis and (run / "compiled/primary-sources").is_dir()
    require(synthesis or native or args.fixture == "frontend", "backend export requires native primary-sources")
    primary = (run / ("synthesis-rtl" if synthesis else "compiled/primary-sources" if native else
                     f"frontend-full-{args.feature}")).resolve(strict=True)
    require(primary.is_relative_to(run), "source directory must remain inside the finished run")
    filelist = primary / "filelist.f" if synthesis else run / "compiled/support-artifacts/filelist.f" if native else None
    filelist_bytes = filelist.read_bytes() if filelist else None
    resources = primary / "firrtl_black_box_resource_files.f"
    resource_bytes = resources.read_bytes() if synthesis and resources.is_file() else None
    output = args.output.absolute()
    require(not output.exists() and not output.is_symlink(), "output must be a new directory")
    require(not output.resolve().is_relative_to(run), "output must be outside the source run")
    require(not relation or not output.resolve().is_relative_to(Path(
            relation["simulation"]["run"] or relation["simulation"]["snapshot"])),
            "output must be outside the simulation run or frozen candidate")

    paths = []
    source_lines = filelist_bytes.decode().splitlines() if filelist else [f"{top}.sv"]
    if resource_bytes is not None:
        source_lines += resource_bytes.decode().splitlines()
    for line in source_lines:
        if not line.strip():
            continue
        relative = Path(line.strip())
        if synthesis and relative.is_absolute():
            relative = relative.relative_to(primary)
        require(not relative.is_absolute() and ".." not in relative.parts,
                f"filelist path escapes primary-sources: {line!r}")
        require(relative.suffix in ((".sv", ".v") if synthesis else (".sv",)) and
                not any(c.isspace() for c in str(relative)),
                f"unsupported filelist entry: {line!r}")
        source = primary / relative
        require(source.resolve(strict=True).is_relative_to(primary) and source.is_file(),
                f"filelist source is not contained in primary-sources: {line!r}")
        if not synthesis or relative not in paths:
            paths.append(relative)
    require(paths and len(set(paths)) == len(paths), "filelist is empty or has duplicate paths")
    actual = {p.relative_to(primary) for p in primary.rglob("*")
              if p.is_file() and (native or p.suffix in (".sv", ".v"))}
    require(set(paths) == actual,
            f"filelist does not exactly cover primary-sources: "
            f"unlisted={sorted(map(str, actual - set(paths)))}, "
            f"missing={sorted(map(str, set(paths) - actual))}")
    if synthesis:
        require(set(view["rtl_files"]) == {p.as_posix() for p in paths} and
                len(view["rtl_files"]) == len(paths), "emission record and RTL file lists disagree")

    top_files = []
    marker_files = {marker: [] for marker in MARKERS}
    records = []
    for relative in paths:
        data = (primary / relative).read_bytes()
        code = NON_CODE.sub(" ", data.decode())
        if not native and not synthesis:
            require(not re.search(r"(?m)^\s*`include\b|\$(?:readmemh|readmemb)\b", code),
                    "monolithic frontend RTL references an unsupported external asset")
        top_files.extend([str(relative)] * MODULE.findall(code).count(top))
        identifiers = set(IDENTIFIER.findall(code))
        for marker in MARKERS:
            if any(marker in identifier for identifier in identifiers):
                marker_files[marker].append(str(relative))
        records.append({"path": f"rtl/{relative.as_posix()}",
                        "sha256": sha256(data), "bytes": len(data)})
    require(len(top_files) == 1, f"expected one definition of {top}, found {top_files}")
    if args.fixture != "load-misalign":
        require(bool(marker_files["fdiNotTrusted"]) == (args.feature == "on"),
                "fdiNotTrusted identifiers disagree with requested feature identity")
        if args.snapshot == "after" and args.feature == "on":
            needed = MARKERS[1:] if args.fixture == "backend" else ("fdiRawRVC",)
            for marker in needed:
                require(marker_files[marker], f"after-on RTL is missing {marker} identifiers")
        else:
            require(not any(marker_files[m] for m in MARKERS[1:]),
                    "before/off RTL contains fields belonging to the after-on implementation")

    output.mkdir(parents=True)
    for relative, record in zip(paths, records):
        data = (primary / relative).read_bytes()
        require(sha256(data) == record["sha256"], f"source changed during export: {relative}")
        destination = output / record["path"]
        destination.parent.mkdir(parents=True, exist_ok=True)
        destination.write_bytes(data)
    identity = {"fixture": args.fixture, "feature": args.feature,
                "snapshot": args.snapshot, "top": top}
    (output / "identity.tsv").write_text("".join(f"{k}\t{v}\n" for k, v in identity.items()))
    (output / "sources.f").write_text("".join(f'{r["path"]}\n' for r in records))
    require(receipt.read_bytes() == receipt_bytes and
            (completion is None or completion.read_bytes() == completion_bytes) and
            (not synthesis or view_path.read_bytes() == view_bytes) and
            (resource_bytes is None or resources.read_bytes() == resource_bytes) and
            (filelist is None or filelist.read_bytes() == filelist_bytes),
            "source completion receipt or filelist changed during export")
    manifest = {
        "schema_version": 1, **identity,
        "created_utc": datetime.now(timezone.utc).isoformat(),
        "source_run": str(run), "source_rtl_directory": str(primary),
        "source_primary_sources": str(primary) if native else None,
        "source_layout": "xiangshan-synthesis-view" if synthesis else
                         "native-primary-sources" if native else "chiseltest-monolithic",
        "source_filelist": str(filelist) if filelist else None,
        "source_filelist_sha256": sha256(filelist_bytes) if filelist else None,
        "source_exit_code_file": str(receipt), "source_test_exit_code": test_rc,
        "source_exit_code_sha256": sha256(receipt_bytes),
        "source_completion_record": str(completion) if completion else None,
        "source_completion_sha256": sha256(completion_bytes) if completion else None,
        "rtl_modified": False,
        "excluded": ["compiled/generated-sources"] if native else ["non-Verilog chiseltest artifacts"],
        "scope": "RTL provenance export only; no functional or timing pass is asserted.",
        "top_source": top_files[0], "marker_source_files": marker_files,
        "file_count": len(records), "files": records,
    }
    if synthesis:
        manifest.update(input_view="verification-removed-synthesis", synthesis_view=view,
            rtl_modified_scope="Byte-preserving export of the emitted synthesis view",
            synthesis_view_sha256=sha256(view_bytes), source_emit_exit_code=test_rc,
            source_test_exit_code=relation["simulation"]["source_test_exit_code"],
            source_simulation_exit_code_file=relation["simulation"]["exit_code_file"],
            simulation_run=relation["simulation"]["run"], simulation_assertions_disabled=False,
            simulation_relation=relation, excluded=["non-Verilog synthesis emission artifacts"],
            source_resource_filelist=str(resources) if resource_bytes is not None else None,
            source_resource_filelist_sha256=sha256(resource_bytes) if resource_bytes is not None else None)
        if args.simulation_source_manifest:
            manifest.update(functional_status="PENDING",
                            simulation_source_manifest=relation["simulation"]["source_manifest"])
    (output / "manifest.json").write_text(json.dumps(manifest, indent=2) + "\n")
    checksum_paths = [r["path"] for r in records] + ["sources.f", "identity.tsv", "manifest.json"]
    checksums = []
    for name in sorted(checksum_paths):
        checksums.append(f"{sha256((output / name).read_bytes())}  {name}\n")
    (output / "SHA256SUMS").write_text("".join(checksums))
    print(f"Exported {len(records)} unchanged RTL sources to {output}; "
          f"source test exit code: {manifest['source_test_exit_code']}" +
          (f"; source emit exit code: {test_rc}" if synthesis else ""))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--run-directory", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--fixture", choices=TOPS, required=True)
    parser.add_argument("--feature", choices=("on", "off"), required=True)
    parser.add_argument("--snapshot", choices=("before", "after"), required=True)
    simulation = parser.add_mutually_exclusive_group()
    simulation.add_argument("--simulation-run-directory", type=Path)
    simulation.add_argument("--simulation-source-manifest", type=Path)
    args = parser.parse_args()
    try:
        export(args)
    except (OSError, ValueError) as error:
        parser.exit(1, f"error: {error}\n")


if __name__ == "__main__":
    sys.exit(main())
