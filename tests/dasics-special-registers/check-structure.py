#!/usr/bin/env python3
"""Inspect emitted local-test state and connectivity, without a timing claim."""

from pathlib import Path
import hashlib
import json
import re
import sys
from collections import Counter


def analyze(path, combined):
    text = re.sub(r'//[^\n]*', '', path.read_text())
    modules = dict(re.findall(r'module (\w+)\((.*?)endmodule', text, re.S))
    states = {}
    for name, body in modules.items():
        rows = [(hi, lo, field) for hi, lo, field in re.findall(
            r'(?m)^\s*reg\s+(?:\[(\d+):(\d+)\]\s+)?(\w+)\s*;', body)
            if not field.startswith('_RAND_')]
        states[name] = {
            'declarations': [(field, int(hi)-int(lo)+1 if hi else 1) for hi, lo, field in rows],
            'bits': sum(int(hi)-int(lo)+1 if hi else 1 for hi, lo, _ in rows),
        }
    harness_name = 'FDIBoundMapTestHarness' if combined else 'FDISpecialMapTestHarness'
    harness = modules[harness_name]
    instances = re.findall(r'(?m)^\s*(\w+)\s+(\w+)\s*\(\s*\.clock', harness)
    types = Counter(kind for kind, name in instances)
    expected = {'FDIMainCallEntryModule': 3, 'FDIFReasonModule': 1}
    if combined:
        expected.update(FDISMainBoundLoModule=44, FDILibCfgModule=1, FDIJumpCfgModule=1, FDIMainCfgModule=1)
    assert types == expected, types
    assert len(instances) == len({name for kind, name in instances}) == (51 if combined else 4)
    assert states[harness_name]['bits'] == 0
    assert states['FDIMainCallEntryModule']['declarations'] == [('reg_ALL', 64)]
    assert states['FDIFReasonModule']['declarations'] == [('reg_REASON', 3)]
    pc = modules['FDIMainCallEntryModule']
    reason = modules['FDIFReasonModule']
    assert re.search(r'reg_ALL <= w_wdata;', pc)
    assert re.search(r'if \(reset\) begin\s+reg_ALL <= 64\x27h0;\s+end else if \(w_wen\)', pc)
    assert re.search(r'wire \[2:0\] wdata_REASON = w_wdata\[2:0\];', reason)
    assert re.search(r'reg_REASON <= wdata_REASON;', reason)
    assert re.search(r'if \(reset\) begin\s+reg_REASON <= 3\x27h0;\s+end else if \(w_wen\)', reason)
    assert re.search(r'assign rdata = \{61\x27h0,rdataFields_REASON\};', reason)
    if combined:
        assert states['FDISMainBoundLoModule']['bits'] == 61
        assert states['FDILibCfgModule']['bits'] == 48
        assert states['FDIJumpCfgModule']['bits'] == 4
        assert states['FDIMainCfgModule']['bits'] == 11
    architectural = sum(states[kind]['bits'] for kind, name in instances)
    assert architectural == (2942 if combined else 195)
    count = 52 if combined else 4
    rsel = re.findall(r'wire\s+(rsel_\d+) = io_readAddress == 12\x27h([0-9a-f]+);', harness)
    wsel = re.findall(r'wire\s+(wsel_\d+) = io_write_bits_address == 12\x27h([0-9a-f]+);', harness)
    assert len(rsel) == len(wsel) == count
    assert [address for name, address in rsel] == [address for name, address in wsel]
    assert len({address for name, address in rsel}) == count
    assert {'8b0', '8b1', '8b2', '8b3'} <= {address for name, address in rsel}
    enables = re.findall(r'assign (\w+_w(?:AliasUMainCfg)?_wen) = io_writeApplied & (wsel_\d+);', harness)
    assert len(enables) == count and len({name for name, select in enables}) == count
    definitions = dict(re.findall(
        r'(?m)^\s*(?:wire\s+(?:\[\d+:\d+\]\s+)?|assign\s+)(\w+)\s*=\s*([^;]+);', harness))

    def depth(name, seen=frozenset()):
        assert name not in seen, 'Combinational dependency cycle'
        if name not in definitions:
            return 0
        dependencies = set(re.findall(r'\b\w+\b', definitions[name])) & definitions.keys()
        return 1 + max((depth(item, seen | {name}) for item in dependencies), default=0)

    or_nodes = sum(name.startswith('_io_readData_T') and ' | ' in expr
                   for name, expr in definitions.items())
    assert or_nodes == count - 1
    return {
        'file': str(path), 'sha256': hashlib.sha256(path.read_bytes()).hexdigest(),
        'architectural_instances': len(instances), 'architectural_bits': architectural,
        'module_types': dict(types), 'states_by_module': states, 'instances': instances,
        'read_decode_entries': rsel, 'write_decode_entries': wsel,
        'effect_gate_fanout_to_native_ports': len(enables), 'read_mux_or_assignments': or_nodes,
        'read_address_to_output_assignment_depth': depth('io_readData'),
        'read_path': '12-bit equality, selected 64-bit word, emitted OR reduction, readEnable gate',
        'write_path': 'Test decode and effect gate, native wen, synchronous field update',
        'limitations': 'Emitted assignment depth is not physical gate depth or delay; no production NewCSR timing evidence',
    }


if __name__ == '__main__':
    root = Path(sys.argv[1]).resolve()
    designs = [
        analyze(root/'rtl-special-registers/FDISpecialMapTestHarness.sv', False),
        analyze(root/'rtl-special-combined-csr-adapter/FDIBoundCSRTestAdapter.sv', True),
    ]
    result = {'status': 'PASS', 'scope': 'Generated local RTL state and connectivity',
              'physical_timing_validated': False, 'designs': designs}
    (root/'structure-result.json').write_text(json.dumps(result, indent=2)+'\n')
    print(json.dumps({'status': 'PASS', 'designs': [{key: row[key] for key in (
        'architectural_instances', 'architectural_bits', 'effect_gate_fanout_to_native_ports',
        'read_mux_or_assignments', 'read_address_to_output_assignment_depth')} for row in designs]}, indent=2))
