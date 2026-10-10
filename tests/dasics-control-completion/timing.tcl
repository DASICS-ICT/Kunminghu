# SPDX-License-Identifier: MulanPSL-2.0

# The two existing fixtures preserve real frontend and backend pipeline boundaries.
# Zero external delays supply a local 30 ns budget, not product arrival times.
set checkpoint ""
if {$argc == 4 && [lindex $argv 0] eq "--checkpoint"} {
    set checkpoint [file normalize [lindex $argv 1]]
    set argv [lrange $argv 2 end]
}
if {[llength $argv] != 2} {
    error "usage: timing.tcl ?--checkpoint <saved-synth.dcp>? <export-timing-rtl-bundle> <new-output-directory>"
}
lassign $argv bundle output_directory
set bundle [file normalize $bundle]
set output_directory [file normalize $output_directory]

proc read_text {path} {
    set channel [open $path r]
    fconfigure $channel -translation binary -encoding binary
    set value [read $channel]
    close $channel
    return $value
}
proc write_text {path value} {
    set channel [open $path w]
    puts $channel $value
    close $channel
}
set identity [dict create]
foreach row [split [string trim [read_text [file join $bundle identity.tsv]]] \n] {
    set fields [split $row \t]
    if {[llength $fields] != 2 || [dict exists $identity [lindex $fields 0]]} {
        error "invalid or duplicate timing bundle identity row"
    }
    dict set identity {*}$fields
}
foreach key {fixture feature snapshot top} {
    if {![dict exists $identity $key]} { error "missing timing identity: $key" }
    set $key [dict get $identity $key]
}
set tops [dict create frontend FDITrustMetadataFrontendHarness backend FDIFullControlHarness]
if {![dict exists $tops $fixture] || $top ne [dict get $tops $fixture] ||
        $feature ni {on off} || $snapshot ni {before after}} {
    error "unexpected fixture, feature, snapshot or top identity"
}
if {[file exists $output_directory]} { error "output directory already exists: $output_directory" }
set sources [list]
foreach relative [split [string trim [read_text [file join $bundle sources.f]]] \n] {
    if {![regexp {^rtl/[A-Za-z0-9_.-]+[.](sv|v)$} $relative]} {
        error "unexpected exported RTL filename: $relative"
    }
    set source [file join $bundle $relative]
    if {![file isfile $source]} { error "exported RTL is unavailable: $source" }
    lappend sources $source
}
if {[llength $sources] == 0 || [llength [lsort -unique $sources]] != [llength $sources]} {
    error "empty or duplicate RTL file list"
}
file mkdir $output_directory
file copy [file join $bundle manifest.json] [file join $output_directory input-manifest.json]
file copy [file join $bundle identity.tsv] [file join $output_directory identity.tsv]
set_param general.maxThreads 32
set constraints [file join $output_directory local.xdc]
write_text $constraints {
create_clock -name clock -period 30.000 -waveform {0.000 15.000} [get_ports clock]
set_input_delay -clock clock 0.000 [get_ports -filter {DIRECTION == IN && NAME != clock}]
set_output_delay -clock clock 0.000 [all_outputs]
}
if {$checkpoint ne ""} {
    set origin [file dirname $checkpoint]
    if {![file isfile $checkpoint] ||
            [read_text [file join $origin input-manifest.json]] ne [read_text [file join $bundle manifest.json]] ||
            [read_text [file join $origin local.xdc]] ne [read_text $constraints]} {
        error "checkpoint provenance or boundary constraints do not match the exported bundle"
    }
    open_checkpoint $checkpoint
    write_text [file join $output_directory checkpoint-input.txt] $checkpoint
} else {
    create_project -in_memory -part xcvu19p-fsva3824-2-e
    # SYNTHESIS excludes generated assertions and initialization without editing RTL.
    read_verilog -sv $sources
    read_xdc $constraints
    synth_design -top $top -part xcvu19p-fsva3824-2-e \
        -mode out_of_context -flatten_hierarchy none -verilog_define SYNTHESIS
    # Keep the successful netlist even if a later focused report detects a coverage gap.
    write_checkpoint [file join $output_directory synth.dcp]
}
set clocks [get_clocks -quiet]
if {[get_property TOP [current_design]] ne $top ||
        [get_property PART [current_design]] ne "xcvu19p-fsva3824-2-e" ||
        [llength $clocks] != 1 || [get_property PERIOD $clocks] != 30.000} {
    error "unexpected device or clock contract"
}

proc instances {reference} {
    set result [list]
    set pattern [format {^%s(_[0-9]+)?$} $reference]
    foreach cell [get_cells -quiet -hierarchical -filter \
            "REF_NAME =~ ${reference}* || ORIG_REF_NAME =~ ${reference}*"] {
        foreach property {REF_NAME ORIG_REF_NAME} {
            if {[regexp $pattern [get_property $property $cell]]} {
                lappend result $cell
                break
            }
        }
    }
    return [lsort -unique $result]
}
proc owned {objects owners {pattern *}} {
    set result [list]
    foreach owner $owners {
        set result [concat $result [filter $objects "NAME =~ $owner/$pattern"]]
    }
    return [lsort -unique $result]
}
proc owned_regex {objects owners pattern} {
    set result [list]
    # Vivado's NAME =~ filter does not implement Tcl character-class matching.
    # Apply the exact numeric field pattern to real owner-relative register names.
    foreach owner $owners {
        foreach object [owned $objects [list $owner]] {
            set relative [string range [get_property NAME $object] [expr {[string length $owner] + 1}] end]
            if {[regexp -- [format {^%s$} $pattern] $relative]} { lappend result $object }
        }
    }
    return [lsort -unique $result]
}
proc interface_pins {owners pattern} {
    set result [list]
    foreach owner $owners {
        set result [concat $result [get_pins -quiet -of_objects [get_cells $owner] \
            -filter "NAME =~ $owner/$pattern"]]
    }
    return [lsort -unique $result]
}
proc leaf_drivers {pins} {
    if {[llength $pins] == 0} { return [list] }
    set nets [get_nets -quiet -segments -of_objects $pins]
    if {[llength $nets] == 0} { return [list] }
    return [lsort -unique [get_pins -quiet -leaf -of_objects $nets -filter {DIRECTION == OUT}]]
}
proc data_pins {cells} {
    if {[llength $cells] == 0} { return [list] }
    return [get_pins -quiet -of_objects $cells -filter {REF_PIN_NAME == D}]
}
proc register_bits {cells} {
    set bits [dict create]
    foreach cell $cells {
        if {![regexp {\[([0-9]+)\]$} [get_property NAME $cell] unused bit] || [dict exists $bits $bit]} {
            error "ambiguous raw instruction register bit: $cell"
        }
        dict set bits $bit $cell
    }
    return $bits
}
proc state_group {name objects} {
    global inventory metrics failures
    dict set metrics ${name}_count [llength $objects]
    foreach object $objects { append inventory "\n$name\t$object" }
    if {[llength $objects] == 0} { lappend failures "missing required state or interface: $name" }
    return $objects
}
proc path_report {name from to {through __NONE__}} {
    global output_directory paths failures
    set info "startpoints=[llength $from]\nendpoints=[llength $to]"
    set options [list]
    if {$through ne "__NONE__"} {
        append info "\nthrough=[llength $through]"
        write_text [file join $output_directory ${name}-through.txt] [join $through \n]
        if {[llength $through] == 0} {
            lappend failures "$name has no required through point"
            write_text [file join $output_directory ${name}-coverage.txt] "$info\nstatus=MISSING_THROUGH"
            append paths "\n$name\tNA\tNA\tNA\tNA"
            return
        }
        set options [list -through $through]
    }
    if {[llength $from] == 0 || [llength $to] == 0} {
        lappend failures "$name has no startpoints or endpoints"
        write_text [file join $output_directory ${name}-coverage.txt] "$info\nstatus=MISSING_ENDPOINTS"
        append paths "\n$name\tNA\tNA\tNA\tNA"
        return
    }
    set values [list]
    foreach delay {max min} kind {setup hold} {
        set found [get_timing_paths -quiet -from $from -to $to {*}$options -delay_type $delay -max_paths 20]
        if {[llength $found] == 0} {
            lappend failures "$name has no $kind path"
            write_text [file join $output_directory ${name}-coverage.txt] "$info\nstatus=MISSING_${kind}_PATH"
            append paths "\n$name\tNA\tNA\tNA\tNA"
            return
        }
        report_timing -of_objects $found -path_type full_clock_expanded -input_pins \
            -significant_digits 3 -file [file join $output_directory ${name}-${kind}.rpt]
        set worst [lindex $found 0]
        lappend values [get_property SLACK $worst]
        if {$delay eq "max"} {
            lappend values [get_property DATAPATH_DELAY $worst] [get_property LOGIC_LEVELS $worst]
        }
        append info "\n${kind}_paths=[llength $found]"
    }
    foreach value $values {
        if {![string is double -strict $value] || ![expr {abs($value) < Inf}]} {
            lappend failures "$name has a nonfinite timing metric"
        }
    }
    append paths "\n$name\t[join $values \t]"
    write_text [file join $output_directory ${name}-coverage.txt] "$info\nstatus=REPORTED"
}

set registers [all_registers -edge_triggered]
set latches [all_registers -level_sensitive]
set clock_gate_latches [owned $latches [instances ClockGate]]
set unexpected_latches [list]
foreach latch $latches { if {$latch ni $clock_gate_latches} { lappend unexpected_latches $latch } }
set blackboxes [get_cells -quiet -hierarchical -filter {IS_BLACKBOX == 1}]
set failures [list]
set inventory "category\tobject"
set paths "category\tsetup_slack_ns\tdata_delay_ns\tlogic_levels\thold_slack_ns"
set metrics [dict merge $identity [dict create tool_version [version -short] \
    part xcvu19p-fsva3824-2-e period_ns 30.000 input_delay_ns 0.000 output_delay_ns 0.000 \
    estimated_pre_route 1 reused_checkpoint [expr {$checkpoint ne ""}] \
    registers [llength $registers] latches [llength $latches] \
    clock_gate_latches [llength $clock_gate_latches] unexpected_latches [llength $unexpected_latches] \
    blackboxes [llength $blackboxes]]]
foreach latch $clock_gate_latches { append inventory "\nproduction_clock_gate_latch\t$latch" }
foreach latch $unexpected_latches { append inventory "\nunexpected_latch\t$latch" }
foreach {metric pattern} {lut_primitives LUT* ff_primitives FD* carry8_primitives CARRY8} {
    dict set metrics $metric [llength [get_cells -quiet -hierarchical -filter "IS_PRIMITIVE == 1 && REF_NAME =~ $pattern"]]
}
set required_modules [expr {$fixture eq "frontend" ?
    {NewIFU IBuffer} : {Backend CtrlBlock Rob ExceptionGen CSR NewCSR MtvalModule StvalModule}}]
foreach reference $required_modules {
    set cells [instances $reference]
    dict set metrics ${reference}_instances [llength $cells]
    foreach cell $cells { append inventory "\n${reference}_instance\t$cell" }
    if {[llength $cells] != 1} { lappend failures "expected one real $reference, found [llength $cells]" }
}

report_utilization -file [file join $output_directory utilization.rpt]
report_utilization -hierarchical -file [file join $output_directory utilization-hierarchy.rpt]
report_high_fanout_nets -timing -max_nets 50 -file [file join $output_directory high-fanout.rpt]
set checks [check_timing -verbose -return_string]
write_text [file join $output_directory check-timing.rpt] $checks
report_timing_summary -delay_type min_max -check_timing_verbose -report_unconstrained \
    -max_paths 20 -significant_digits 3 -file [file join $output_directory timing-summary.rpt]
path_report register-to-register $registers $registers
path_report input-to-register [all_inputs] $registers
path_report register-to-output $registers [all_outputs]

set added_state [filter $registers {NAME =~ *fdiRawRVC* || NAME =~ *fdiInstr* || NAME =~ *fdiTrapInst*}]
dict set metrics added_encoding_registers [llength $added_state]
if {($snapshot eq "before" || $feature eq "off") && [llength $added_state] != 0} {
    lappend failures "encoding transport state exists in a before/off fixture"
}
if {$fixture eq "frontend"} {
    set ifus [instances NewIFU]
    set ibuffers [instances IBuffer]
    set ifu_state [owned $registers $ifus]
    set ibuffer_state [owned $registers $ibuffers]
    path_report ifu-to-ibuffer $ifu_state $ibuffer_state
    path_report ibuffer-to-decode $ibuffer_state [get_ports -quiet io_decode_*]
    if {$snapshot eq "after" && $feature eq "on"} {
        set raw_ifu [state_group ifu_raw_instruction \
            [owned_regex $registers $ifus {f3_instr_[0-9]+_reg(\[[0-9]+\])?}]]
        set raw_mmio [state_group ifu_mmio_instruction \
            [owned_regex $registers $ifus {f3_mmio_data_[0-9]+_reg(\[[0-9]+\])?}]]
        set raw_bank [state_group ibuffer_raw_bank [owned $registers $ibuffers ibuf_*fdiRawRVC*]]
        set raw_output [state_group ibuffer_raw_output [owned $registers $ibuffers outputEntries_*fdiRawRVC*]]
        set raw_pins [interface_pins $ifus io_toIbuffer_bits_fdiRawRVC*]
        path_report cache-to-ifu-raw [get_ports -quiet io_cacheResponse_bits_data*] $raw_ifu
        path_report ifu-raw-to-bank $raw_ifu $raw_bank $raw_pins
        path_report mmio-raw-to-bank $raw_mmio $raw_bank $raw_pins
        path_report ifu-raw-bypass $raw_ifu $raw_output $raw_pins
        path_report bank-to-output-raw $raw_bank $raw_output
        path_report ibuffer-control-to-raw $ibuffer_state $raw_bank
        path_report raw-output-to-decode $raw_output [get_ports -quiet io_decode_*_bits_fdiRawRVC*]
    }
} else {
    set exception_modules [instances ExceptionGen]
    set robs [instances Rob]
    set csr_modules [instances NewCSR]
    set exception_state [owned $registers $exception_modules]
    set rob_state [list]
    foreach item [owned $registers $robs] {
        if {$item ni $exception_state} { lappend rob_state $item }
    }
    set mtval_state [state_group mtval [owned $registers [instances MtvalModule]]]
    set stval_state [state_group stval [owned $registers [instances StvalModule]]]
    set trap_pins [interface_pins $csr_modules io_trapInst_bits*]
    path_report exception-selection $exception_state $exception_state
    path_report exception-to-rob $exception_state $rob_state
    path_report trap-encoding-to-mtval $registers $mtval_state $trap_pins
    path_report trap-encoding-to-stval $registers $stval_state $trap_pins
    if {$snapshot eq "after" && $feature eq "on"} {
        set backend_state [owned $registers [instances Backend]]
        set raw16 [state_group backend_raw16 [filter $backend_state {NAME =~ *fdiRawRVC*}]]
        set raw32 [state_group backend_issued_raw32 [filter $backend_state {NAME =~ *fdiInstr*}]]
        set writeback_raw [state_group control_writeback_raw [owned $registers \
            [instances CtrlBlock] delayedNotFlushedWriteBack*fdiTrapInst_bits*]]
        set s0 [state_group exception_raw_s0 [owned $registers $exception_modules s0_out_bits*fdiTrapInst_bits*]]
        set s1 [state_group exception_raw_s1 [owned $registers $exception_modules s1_out_bits*fdiTrapInst_bits*]]
        set current [state_group exception_raw_current [owned $registers $exception_modules current_fdiTrapInst_bits*]]
        set s0_valid [state_group exception_raw_valid_s0 [owned $registers $exception_modules s0_out_bits*fdiTrapInst_valid*]]
        set rob_raw [state_group rob_trap_raw [owned $registers $robs io_exception_bits_fdiTrapInst*bits*]]
        set rob_valid [state_group rob_trap_raw_valid [owned $registers $robs io_exception_bits_fdiTrapInst*valid*]]
        path_report raw16-to-issued-raw32 $raw16 $raw32
        set lanes [dict create]
        foreach reference {JumpUnit BranchUnit} {
            set units [instances $reference]
            state_group ${reference}_instances $units
            foreach unit $units {
                if {![regexp {/inner_intExuBlock/exus_([0-9]+)/[^/]+$} $unit unused lane] ||
                        [dict exists $lanes $lane $reference]} {
                    lappend failures "ambiguous control execution lane: $unit"
                    continue
                }
                dict set lanes $lane $reference $unit
            }
        }
        if {[dict size $lanes] != 3} { lappend failures "the frozen configuration requires three control execution lanes" }
        set connectivity "lane\tfield\tbit\tfrom\tto\tresult"
        set bit_paths "lane\tbit\tfrom\tto\tdestination_driver\tsetup_slack_ns\tsetup_data_delay_ns\tsetup_logic_levels\thold_slack_ns\thold_data_delay_ns\thold_logic_levels\tstatus"
        # Jump and Branch copy the same raw word without a register. ExeUnit's
        # output-valid mux qualifies each bit before writeback. Synthesis removes
        # the FU payload ports, so require each lane's matching bit-to-bit timing
        # path and retain separate producer-qualification paths for both FUs.
        dict for {lane units} $lanes {
            set issue [state_group lane_${lane}_issued_raw [owned_regex $registers [instances Backend] \
                [format {^inner_bypassNetwork2intExuBlock_%d/data_fdiInstr_reg\[[0-9]+\]$} $lane]]]
            set wb [state_group lane_${lane}_writeback_raw [owned_regex $registers [instances CtrlBlock] \
                [format {^delayedNotFlushedWriteBack_delayed_bits_r_%d_fdiTrapInst_bits_reg\[[0-9]+\]$} $lane]]]
            # For these producers fdiTrapInst.valid equals exceptionVec.II in RTL;
            # the netlist retains the II register and removes the duplicate FF.
            set valid [state_group lane_${lane}_writeback_valid_II [owned_regex $registers [instances CtrlBlock] \
                [format {^delayedNotFlushedWriteBack_delayed_bits_r_%d_exceptionVec_2_reg$} $lane]]]
            if {[llength $issue] != 32 || [llength $wb] != 32 || [llength $valid] != 1} {
                lappend failures "lane $lane lacks its own 32-bit data and one-bit validity storage"
            } else {
                set issue_bits [register_bits $issue]
                set wb_bits [register_bits $wb]
                for {set bit 0} {$bit < 32} {incr bit} {
                    if {![dict exists $issue_bits $bit] || ![dict exists $wb_bits $bit]} {
                        lappend failures "lane $lane raw bit $bit is absent"
                        continue
                    }
                    set source [get_pins -quiet -of_objects [list [dict get $issue_bits $bit]] -filter {REF_PIN_NAME == Q}]
                    set destination [data_pins [list [dict get $wb_bits $bit]]]
                    set actual [leaf_drivers $destination]
                    set connected [expr {[llength $source] == 1 && [llength $destination] == 1}]
                    set values [list]
                    foreach delay {max min} kind {setup hold} {
                        set found [list]
                        if {[llength $source] == 1 && [llength $destination] == 1} {
                            set found [get_timing_paths -quiet -from $source -to $destination -delay_type $delay -max_paths 1]
                        }
                        if {[llength $found] != 1} {
                            set connected 0
                            lappend values NA NA NA
                            lappend failures "lane $lane raw bit $bit has no matching $kind path"
                        } else {
                            foreach property {SLACK DATAPATH_DELAY LOGIC_LEVELS} {
                                set value [get_property $property [lindex $found 0]]
                                lappend values $value
                                if {![string is double -strict $value] || ![expr {abs($value) < Inf}]} {
                                    set connected 0
                                    lappend failures "lane $lane raw bit $bit has nonfinite $kind $property"
                                }
                            }
                        }
                    }
                    append connectivity "\n$lane\traw\t$bit\t$source\t$destination\t$connected"
                    append bit_paths "\n$lane\t$bit\t$source\t$destination\t$actual\t[join $values \t]\t[expr {$connected ? {REPORTED} : {MISSING}}]"
                }
            }
            path_report lane-${lane}-raw-to-writeback $issue $wb
            path_report lane-${lane}-writeback-raw-to-exception $wb $s0
            path_report lane-${lane}-writeback-valid-to-exception $valid $s0_valid
            foreach reference {JumpUnit BranchUnit} {
                if {![dict exists $units $reference]} {
                    lappend failures "lane $lane is missing $reference"
                    continue
                }
                set unit [dict get $units $reference]
                set producer [leaf_drivers [interface_pins [list $unit] io_out_bits_ctrl_exceptionVec_2]]
                set starts [list]
                if {[llength $producer]} { set starts [all_fanin -flat -startpoints_only -to $producer] }
                set destination [data_pins $valid]
                set ends [list]
                if {[llength $producer]} { set ends [all_fanout -flat -endpoints_only -from $producer] }
                set connected [expr {[llength $destination] == 1 && [lindex $destination 0] in $ends}]
                append connectivity "\n$lane\t${reference}-valid\t0\t$producer\t$destination\t$connected"
                if {!$connected} { lappend failures "lane $lane $reference validity does not reach its own writeback II register" }
                path_report lane-${lane}-${reference}-valid-to-writeback $starts $valid $producer
            }
        }
        write_text [file join $output_directory raw-lane-connectivity.tsv] $connectivity
        write_text [file join $output_directory raw-bit-paths.tsv] $bit_paths
        path_report exception-raw-s0-to-s1 $s0 $s1
        path_report exception-raw-s1-to-current $s1 $current
        path_report exception-control-to-raw $exception_state [concat $s1 $current]
        path_report current-raw-to-rob $current $rob_raw
        set delay_modules [get_cells -quiet -hierarchical -filter {NAME =~ */csr_io_csrio_exception_delay}]
        if {[llength $delay_modules] != 1} { lappend failures "missing unique production CSR exception delay" }
        # ExeUnit transports the complete exception through an existing two-cycle
        # DelayN before CSR selects dynamic-II instruction bits for TVAL.
        set d0_bits [state_group csr_exception_delay0_bits [owned $registers $delay_modules REG_bits_fdiTrapInst_bits*]]
        set d0_valid [state_group csr_exception_delay0_valid [owned $registers $delay_modules REG_bits_fdiTrapInst_valid*]]
        set d1_bits [state_group csr_exception_delay1_bits [owned $registers $delay_modules REG_1_bits_fdiTrapInst_bits*]]
        set d1_valid [state_group csr_exception_delay1_valid [owned $registers $delay_modules REG_1_bits_fdiTrapInst_valid*]]
        path_report rob-raw-to-csr-delay0 $rob_raw $d0_bits
        path_report rob-raw-valid-to-csr-delay0 $rob_valid $d0_valid
        path_report csr-raw-delay0-to-delay1 $d0_bits $d1_bits
        path_report csr-valid-delay0-to-delay1 $d0_valid $d1_valid
        set trap_drivers [leaf_drivers $trap_pins]
        foreach {bank bank_state} [list mtval $mtval_state stval $stval_state] {
            path_report csr-delay1-raw-to-${bank} $d1_bits $bank_state $trap_drivers
            path_report csr-delay1-valid-to-${bank} $d1_valid $bank_state $trap_drivers
        }
    }
}

set counts [dict create]
foreach line [split $checks \n] {
    if {[regexp {^[0-9]+[.][[:space:]]+checking[[:space:]]+([a-z_]+)[[:space:]]+\(([0-9]+)\)} \
            $line match category count]} { dict set counts $category $count }
}
foreach category {loops latch_loops no_clock unconstrained_internal_endpoints} {
    if {![dict exists $counts $category]} {
        lappend failures "missing check_timing category: $category"
    } else {
        dict set metrics $category [dict get $counts $category]
        if {[dict get $counts $category] != 0} { lappend failures "check_timing $category is nonzero" }
    }
}
if {[llength $unexpected_latches] || [llength $blackboxes]} {
    lappend failures "unexpected latches or unresolved black boxes remain"
}
set summary "metric\tvalue"
dict for {metric value} $metrics { append summary "\n$metric\t$value" }
write_text [file join $output_directory summary.tsv] $summary
write_text [file join $output_directory state-inventory.tsv] $inventory
write_text [file join $output_directory paths.tsv] $paths
write_text [file join $output_directory coverage-failures.txt] [join $failures \n]
if {[llength $failures]} { error "timing coverage is incomplete: [join $failures {; }]" }
write_text [file join $output_directory analysis-status.txt] \
    "ANALYSIS_COMPLETE\nEstimated pre-placement timing only. Read setup, hold and coverage reports; this is not timing PASS."
close_project
