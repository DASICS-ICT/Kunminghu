#!/usr/bin/env python3
"""Independent replay of raw special-register DUT observations; never imports DUT/oracle code."""
from pathlib import Path
import csv, json, hashlib, sys
from collections import Counter

BOUNDS=[0xbc5,0xbc6,0x9e2,0x9e3,0x880,
      0x890,0x891,0x892,0x893,0x894,0x895,0x896,0x897,
      0x898,0x899,0x89a,0x89b,0x89c,0x89d,0x89e,0x89f,
      0x8a0,0x8a1,0x8a2,0x8a3,0x8a4,0x8a5,0x8a6,0x8a7,
      0x8a8,0x8a9,0x8aa,0x8ab,0x8ac,0x8ad,0x8ae,0x8af,
      0x8c0,0x8c1,0x8c2,0x8c3,0x8c4,0x8c5,0x8c6,0x8c7,0x8c8]
ADDR=[0x8b0,0x8b1,0x8b2,0x8b3]
FULL=(1<<64)-1
MASK={a:0xfffffffffffffff8 for a in BOUNDS}
MASK.update({0x8b0:FULL,0x8b1:FULL,0x8b2:FULL,0x8b3:7,0x880:0xbbbbbbbbbbbbbbbb,0x8c8:0x0001000100010001,0xbc4:0x7ff,0x9e1:0x7c2})
def boolean(s):
    assert s in ('true','false'),s
    return s=='true'
def hexint(s): return int(s,16)
def owner(a): return 0xbc4 if a==0x9e1 else a
def view(state,a): return state.get(owner(a),0)&MASK.get(a,0)
def apply(state,a,data):
    old=state[owner(a)]
    state[owner(a)]=(old&~MASK[a])|(data&MASK[a])
def check_snapshot(row,key,state,order):
    actual=[hexint(s) for s in row[key].split(':')]
    expected=[view(state,a) for a in order]
    assert actual==expected,(row['cycle'],row['category'],key,actual,expected)

def replay(root):
    categories=Counter()
    events=Counter()
    state={a:0 for a in ADDR}
    direct_cycles=direct_writes=0
    with (root/'special-trace.csv').open() as f:
        for row in csv.DictReader(f):
            assert int(row['cycle'])==direct_cycles
            check_snapshot(row,'before',state,ADDR)
            a,ra=hexint(row['address']),hexint(row['readAddress'])
            stopped=boolean(row['reset']) or boolean(row['cancel'])
            applied=boolean(row['valid']) and a in ADDR and not stopped
            assert boolean(row['readHit'])==(ra in ADDR)
            assert boolean(row['writeApplied'])==applied
            assert hexint(row['readData'])==(view(state,ra) if boolean(row['readEnable']) else 0)
            assert hexint(row['rmwData'])==view(state,ra)
            before=state.copy()
            if boolean(row['reset']): state={a:0 for a in ADDR}
            elif applied: apply(state,a,hexint(row['data']))
            if applied:
                direct_writes+=1
                events['direct_same_value_write' if before==state else 'direct_changed_write']+=1
            check_snapshot(row,'after',state,ADDR)
            direct_cycles+=1
            categories[row['category']]+=1
    order=BOUNDS+[0xbc4,0x9e1]+ADDR
    state={a:0 for a in BOUNDS+[0xbc4]+ADDR}
    phase=0
    pending=None
    accepted=responses=cancelled=csr_writes=csr_cycles=0
    immediates=set()
    with (root/'csr-trace.csv').open() as f:
        for row in csv.DictReader(f):
            assert int(row['cycle'])==csr_cycles
            assert int(row['phase'])==phase
            check_snapshot(row,'before',state,order)
            rst,cancel=boolean(row['reset']),boolean(row['cancel'])
            stopped=rst or cancel
            ready=boolean(row['respReady'])
            rqready=phase==0 and not stopped
            rsvalid=phase!=0 and not stopped
            assert boolean(row['reqReady'])==rqready
            assert boolean(row['respValid'])==rsvalid
            applied=phase==1 and not stopped and pending['write']
            assert boolean(row['writeApplied'])==bool(applied)
            if pending:
                for key,expected in [('pendingTag',pending['tag']),('pendingAddress',pending['addr']),
                                     ('pendingOld',pending['old']),('savedAddress',pending['addr'])]:
                    value=int(row[key]) if key=='pendingTag' else hexint(row[key])
                    assert value==expected,(csr_cycles,key,value,expected)
                for key,expected in [('pendingRead',pending['read']),('pendingWrite',pending['write']),
                                     ('pendingRejected',not pending['permit']),('savedPermit',pending['permit'])]:
                    assert boolean(row[key])==expected,(csr_cycles,key)
                if pending['op'] in (1,2,3,5,6,7):
                    assert hexint(row['savedData'])==pending['final'],(csr_cycles,'savedData')
            else: assert row['pendingTag']==''
            before=state.copy()
            if stopped:
                if pending: cancelled+=1
                events[f'{"reset" if rst else "cancel"}_phase_{phase}']+=1
                if rst: state={a:0 for a in BOUNDS+[0xbc4]+ADDR}
                phase,pending=0,None
            elif rqready and boolean(row['reqValid']):
                a=hexint(row['reqAddress'])
                op,enc,rd=int(row['reqOperation']),int(row['reqEncoding']),int(row['reqRd'])
                read=op not in (1,5) or rd!=0
                write=op in (1,5) or enc!=0
                permit=op in (1,2,3,5,6,7) and a in order and (not read or boolean(row['readAllowed'])) and (not write or boolean(row['writeAllowed']))
                old=view(state,a)
                operand=enc if op in (5,6,7) else (hexint(row['reqOperand']) if enc else 0)
                final=operand if op in (1,5) else (old|operand if op in (2,6) else old&~operand&FULL)
                pending={'tag':int(row['reqTag']),'addr':a,'op':op,'old':old if permit and read else 0,
                         'read':permit and read,'write':permit and write,'permit':permit,'final':final}
                phase=1
                accepted+=1
                if not permit: events['rejected']+=1
                elif not write: events['pure_read']+=1
                if op in (5,6,7): immediates.add((a,op,enc))
            elif phase==1:
                if applied:
                    apply(state,pending['addr'],pending['final'])
                    csr_writes+=1
                    events['csr_same_value_write' if before==state else 'csr_changed_write']+=1
                if ready:
                    phase,pending=0,None
                    responses+=1
                else: phase=2
            elif rsvalid and ready:
                phase,pending=0,None
                responses+=1
            if rsvalid and not ready: events['response_hold']+=1
            if boolean(row['reqValid']) and not rqready and not stopped: events['request_stall']+=1
            assert accepted==responses+cancelled+bool(pending)
            check_snapshot(row,'after',state,order)
            categories[row['category']]+=1
            csr_cycles+=1
    assert not pending and phase==0 and all(v==0 for v in state.values())
    required={(a,op,enc) for a in ADDR for op in (5,6,7) for enc in (0,1,7,8,31)}
    assert required <= immediates
    matrix=len(required)
    counts={'directCycles':direct_cycles,'directWrites':direct_writes,'csrCycles':csr_cycles,'csrWrites':csr_writes,
            'accepted':accepted,'responses':responses,'cancelled':cancelled,'remainingInflight':0,'c04DirectedImmediateCases':matrix}
    result=json.loads((root/'result.json').read_text())
    for key,value in dict(counts,categories=dict(categories),events=dict(events)).items():
        assert result[key]==value,(key,result[key],value)
    return dict(status='PASS',counts=counts,categories=dict(categories),events=dict(events),
                traces=[{'path':str(root/n),'sha256':hashlib.sha256((root/n).read_bytes()).hexdigest()} for n in ['special-trace.csv','csr-trace.csv']],
                scope='Independent replay of observed local DUT state and transaction traces; no production wrapper/permission/retirement claim')

if __name__=='__main__':
    root=Path(sys.argv[1]).resolve()
    result=replay(root)
    (root/'replay-result.json').write_text(json.dumps(result,indent=2)+'\n')
    print(json.dumps({'status':result['status'],'counts':result['counts']},indent=2))
