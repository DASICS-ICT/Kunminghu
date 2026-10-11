# SPDX-License-Identifier: MulanPSL-2.0

# Analyze the actual load, split, translation and PMP fixture before placement.
# A 30 ns clock and zero boundary delays provide a local full-cycle budget;
# they do not describe arrival times in MemBlock or routed product timing.
# Inputs must use the synthesis emission policy; SYNTHESIS only excludes guarded code.
if {$argc != 4} {
    error "usage: timing.tcl <RTL-directory> <new-output-directory> <on|off> <before|after>"
}
lassign $argv source output_directory feature snapshot
set source [file normalize $source]
set output_directory [file normalize $output_directory]
if {$feature ni {on off} || $snapshot ni {before after}} {
    error "feature must be on/off and snapshot must be before/after"
}
if {![file isfile [file join $source FDILoadMisalignHarness.sv]]} {
    error "the production load/misalign fixture RTL is unavailable: $source"
}
if {[file exists $output_directory]} { error "output directory already exists: $output_directory" }
file mkdir $output_directory
set_param general.maxThreads 32

proc write_text {path value} {
    set channel [open $path w]
    puts $channel $value
    close $channel
}

proc instances {reference scope} {
    set result [list]
    set pattern [format {^%s(_[0-9]+)?$} $reference]
    foreach cell [get_cells -quiet -hierarchical -filter "NAME =~ $scope"] {
        foreach property {REF_NAME ORIG_REF_NAME} {
            if {[regexp $pattern [get_property $property $cell]]} {
                lappend result $cell
                break
            }
        }
    }
    return [lsort -unique $result]
}

proc owned_registers {registers owners} {
    set result [list]
    foreach owner $owners {
        set result [concat $result [filter $registers "NAME =~ $owner/*"]]
    }
    return [lsort -unique $result]
}

proc named_registers {registers owners pattern} {
    set result [list]
    foreach owner $owners {
        foreach object [owned_registers $registers [list $owner]] {
            set relative [string range [get_property NAME $object] [expr {[string length $owner] + 1}] end]
            if {[regexp -- $pattern $relative]} { lappend result $object }
        }
    }
    return [lsort -unique $result]
}

proc without_objects {collection excluded} {
    set result [list]
    foreach item $collection {
        if {$item ni $excluded} { lappend result $item }
    }
    return $result
}

proc path_reports {directory category from to {through __NONE__}} {
    set coverage "startpoint_count=[llength $from]\nendpoint_count=[llength $to]"
    set through_options [list]
    if {$through ne "__NONE__"} {
        append coverage "\nthrough_count=[llength $through]"
        if {[llength $through] == 0} {
            write_text [file join $directory ${category}-coverage.txt] "$coverage\nstatus=NO_THROUGH_OBJECTS"
            return [list NA NA NA NA]
        }
        write_text [file join $directory ${category}-through.txt] [join $through "\n"]
        set through_options [list -through $through]
    }
    if {[llength $from] == 0 || [llength $to] == 0} {
        write_text [file join $directory ${category}-coverage.txt] "$coverage\nstatus=NO_MATCHING_OBJECTS"
        return [list NA NA NA NA]
    }
    set metrics [list]
    foreach delay_type {max min} report_type {setup hold} {
        set paths [get_timing_paths -quiet -from $from -to $to {*}$through_options \
            -delay_type $delay_type -max_paths 20]
        if {[llength $paths] == 0} {
            write_text [file join $directory ${category}-coverage.txt] "$coverage\nstatus=NO_${report_type}_PATH"
            return [list NA NA NA NA]
        }
        report_timing -of_objects $paths -path_type full_clock_expanded \
            -input_pins -significant_digits 3 -file [file join $directory ${category}-${report_type}.rpt]
        set worst [lindex $paths 0]
        lappend metrics [get_property SLACK $worst]
        if {$delay_type eq "max"} {
            lappend metrics [get_property DATAPATH_DELAY $worst] [get_property LOGIC_LEVELS $worst]
        }
        append coverage "\n${report_type}_path_count=[llength $paths]"
    }
    foreach metric $metrics {
        if {![string is double -strict $metric] || ![expr {abs($metric) < Inf}]} {
            write_text [file join $directory ${category}-coverage.txt] "$coverage\nstatus=NONFINITE_TIMING_METRIC"
            return [list NA NA NA NA]
        }
    }
    write_text [file join $directory ${category}-coverage.txt] "$coverage\nstatus=REPORTED"
    return $metrics
}

set constraints [file join $output_directory local.xdc]
write_text $constraints {
create_clock -name clock -period 30.000 -waveform {0.000 15.000} [get_ports clock]
set_input_delay -clock clock 0.000 [get_ports -filter {DIRECTION == IN && NAME != clock}]
set_output_delay -clock clock 0.000 [all_outputs]
}
set sources [lsort [concat [glob -nocomplain -directory $source *.sv] \
    [glob -nocomplain -directory $source *.v]]]
write_text [file join $output_directory source-files.txt] [join $sources "\n"]
create_project -in_memory -part xcvu19p-fsva3824-2-e
read_verilog -sv $sources
read_xdc $constraints
# Retain production hierarchy so the local path reports identify real owners.
synth_design -top FDILoadMisalignHarness -part xcvu19p-fsva3824-2-e \
    -mode out_of_context -flatten_hierarchy none -verilog_define SYNTHESIS
# Save successful synthesis before focused endpoint reporting can fail.
write_checkpoint [file join $output_directory synth.dcp]

set clocks [get_clocks]
if {[get_property TOP [current_design]] ne "FDILoadMisalignHarness" ||
        [get_property PART [current_design]] ne "xcvu19p-fsva3824-2-e" ||
        [llength $clocks] != 1 || [get_property PERIOD $clocks] != 30.000} {
    error "the local device or clock contract does not match the approved product"
}
set registers [all_registers -edge_triggered]
set latches [all_registers -level_sensitive]
set blackboxes [get_cells -quiet -hierarchical -filter {IS_BLACKBOX == 1}]
set metrics [dict create feature $feature snapshot $snapshot tool_version [version -short] \
    part xcvu19p-fsva3824-2-e clock_period_ns 30.000 input_delay_ns 0.000 output_delay_ns 0.000 \
    estimated_pre_route 1 hierarchy_preserved 1 registers [llength $registers] \
    latches [llength $latches] blackboxes [llength $blackboxes]]
set owners [dict create]
set modules_by_owner [dict create]
set inventory "category\tobject"
set failures [list]
# These are the fixture's actual owners. Other same-type children, such as a
# future TLB-internal checker, must not impersonate its public PMP service.
foreach {category reference scope} {
        load LoadUnit load split LoadMisalignBuffer split translation TLBNonBlock tlb
        protection PMPChecker protection replay LoadQueueReplay replay
        exceptions LqExceptionBuffer exceptions uncache LoadQueueUncache uncache
        permission FDIPermissionChecker load/*} {
    set modules [instances $reference $scope]
    set state [owned_registers $registers $modules]
    dict set modules_by_owner $category $modules
    dict set owners $category $state
    dict set metrics ${category}_instances [llength $modules]
    dict set metrics ${category}_registers [llength $state]
    foreach item $modules {
        append inventory "\n${category}_instance\t$item"
        append inventory "\n${category}_reference\t[get_property REF_NAME $item]"
        append inventory "\n${category}_original_reference\t[get_property ORIG_REF_NAME $item]"
    }
    foreach item $state { append inventory "\n${category}_register\t$item" }
    set expected [expr {$category eq "permission" ? $feature eq "on" : 1}]
    if {[llength $modules] != $expected} {
        lappend failures "expected $expected real $reference instance(s) at $scope, found [llength $modules]"
    }
    if {$expected && [llength $state] == 0} {
        lappend failures "the $category production owner has no retained registers"
    }
}
dict set metrics all_pmp_checker_instances [llength [instances PMPChecker *]]
# Permission is nested inside LoadUnit. Disjoint endpoint sets prevent an
# internal checker path from masquerading as a checker-to-pipeline path.
dict set owners load [without_objects [dict get $owners load] [dict get $owners permission]]
dict set metrics load_pipeline_registers [llength [dict get $owners load]]
foreach {metric pattern} {lut_primitives LUT* ff_primitives FD* carry8_primitives CARRY8} {
    dict set metrics $metric [llength [get_cells -quiet -hierarchical -filter "IS_PRIMITIVE == 1 && REF_NAME =~ $pattern"]]
}
write_text [file join $output_directory state-registers.tsv] $inventory
report_utilization -file [file join $output_directory utilization.rpt]
report_utilization -hierarchical -file [file join $output_directory utilization-hierarchy.rpt]
report_high_fanout_nets -timing -max_nets 50 -file [file join $output_directory high-fanout.rpt]
set checks [check_timing -verbose -return_string]
write_text [file join $output_directory check-timing.rpt] $checks
report_timing_summary -delay_type min_max -check_timing_verbose -report_unconstrained \
    -max_paths 20 -significant_digits 3 -file [file join $output_directory timing-summary.rpt]

set paths "category\tsetup_slack_ns\tdata_delay_ns\tlogic_levels\thold_slack_ns"
set path_groups [list \
    register-to-register $registers $registers \
    input-to-register [all_inputs] $registers \
    register-to-output $registers [all_outputs] \
    input-to-output [all_inputs] [all_outputs]]
foreach {from to} {
        load split split load translation load load translation protection load
        load protection load replay replay load load exceptions load uncache} {
    lappend path_groups ${from}-to-${to} [dict get $owners $from] [dict get $owners $to]
}
# Both paired variants contain the same real uncached consumer. Its first
# register boundary must retain request validity, address, and fault metadata;
# a path to an unrelated queue register cannot substitute for these captures.
foreach {category pattern} {
        valid {^s2_valid_REG_reg$}
        address {^s2_req_0_paddr_reg\[[0-9]+\]$}
        exception {^s2_req_0_uop_exceptionVec_[0-9]+_reg$}} {
    set state [named_registers $registers [dict get $modules_by_owner uncache] $pattern]
    dict set metrics uncache_capture_${category}_registers [llength $state]
    foreach item $state { append inventory "\nuncache_capture_${category}\t$item" }
    if {[llength $state] == 0 || ($category eq "valid" && [llength $state] != 1)} {
        lappend failures "missing or ambiguous real uncached $category capture state"
    }
    # Constrain the data pins: a capture-enable path must not stand in for the
    # address or exception payload captured by these registers.
    set capture_pins [list]
    if {[llength $state]} {
        set capture_pins [get_pins -quiet -of_objects $state -filter {REF_PIN_NAME == D}]
    }
    if {[llength $capture_pins] != [llength $state]} {
        lappend failures "uncached $category capture registers lack unique data pins"
    }
    lappend path_groups load-to-uncache-capture-${category} [dict get $owners load] $capture_pins
}
write_text [file join $output_directory state-registers.tsv] $inventory
if {$feature eq "on"} {
    foreach {from to} {load permission permission load} {
        lappend path_groups ${from}-to-${to} [dict get $owners $from] [dict get $owners $to]
    }
}
foreach {category from to} $path_groups {
    set result [path_reports $output_directory $category $from $to]
    append paths "\n$category\t[join $result \t]"
    if {[lindex $result 0] eq "NA"} {
        lappend failures "missing required production path: $category"
    }
}
if {$feature eq "on"} {
    set outcome_pins [list]
    foreach owner [dict get $modules_by_owner permission] {
        set outcome_pins [concat $outcome_pins [get_pins -quiet \
            -of_objects [get_cells $owner] -filter {DIRECTION == OUT && NAME =~ */io_resp_bits_outcome*}]]
    }
    # If denial becomes constant or disconnected, unrelated retained metadata
    # must not make the permission coverage appear complete.
    set result [path_reports $output_directory permission-outcome-to-load \
        [dict get $owners permission] [dict get $owners load] $outcome_pins]
    append paths "\npermission-outcome-to-load\t[join $result \t]"
    if {[lindex $result 0] eq "NA"} {
        lappend failures "missing required permission outcome consumption path"
    }
}

# Additional groups identify the new production cones at their existing register
# boundaries. They supplement the common paired-fixture groups without changing
# their endpoint sets or constraints.
if {$snapshot eq "after"} {
    proc capture_data_pins {cells} {
        if {[llength $cells] == 0} { return [list] }
        return [get_pins -quiet -of_objects $cells -filter {REF_PIN_NAME == D}]
    }
    proc focused_state {name category pattern} {
        global registers modules_by_owner metrics inventory failures
        set state [named_registers $registers [dict get $modules_by_owner $category] $pattern]
        dict set metrics ${name}_registers [llength $state]
        foreach item $state { append inventory "\n$name\t$item" }
        if {[llength $state] == 0} { lappend failures "missing required after state: $name" }
        return $state
    }
    proc module_pins {category pattern} {
        global modules_by_owner
        set result [list]
        foreach owner [dict get $modules_by_owner $category] {
            foreach pin [get_pins -quiet -of_objects [get_cells $owner]] {
                set relative [string range [get_property NAME $pin] [expr {[string length $owner] + 1}] end]
                if {[regexp -- $pattern $relative]} { lappend result $pin }
            }
        }
        return [lsort -unique $result]
    }
    proc focused_path {name from to {through __NONE__}} {
        global output_directory paths failures
        set result [path_reports $output_directory $name $from $to $through]
        append paths "\n$name\t[join $result \t]"
        if {[lindex $result 0] eq "NA"} { lappend failures "missing required after path: $name" }
    }
    proc focused_capture {name from cells {through __NONE__}} {
        focused_path $name $from [capture_data_pins $cells] $through
    }

    set split_address [focused_state saved-original-fullva split {^req_fullva_reg\[[0-9]+\]$}]
    set high_address [focused_state high-fragment-fullva split {^splitLoadReqs_1_fullva_reg\[[0-9]+\]$}]
    set terminal_address [focused_state terminal-fullva split {^terminalFullva_reg\[[0-9]+\]$}]
    set split_state [focused_state split-control split {^(bufferState|req_valid|curPtr|unSentLoads)_reg(\[[0-9]+\])?$}]
    set split_owner [focused_state split-owner split {^req_uop_(robIdx_(flag|value)|lqIdx_(flag|value)|uopIdx)_reg(\[[0-9]+\])?$}]
    set split_exception [focused_state split-final-exception split {^exceptionVec_[0-9]+_reg$}]
    set split_qualification [focused_state split-address-qualification split {^req_(vaNeedExt|isvec|uop_fuOpType)_reg(\[[0-9]+\])?$}]

    for {set stage 1} {$stage <= 3} {incr stage} {
        set prefix [expr {$stage == 3 ? "s3_in" : "s${stage}_in_r"}]
        foreach {field pattern} {
                fullva {fullva_reg\[[0-9]+\]}
                extension {vaNeedExt_reg}
                terminal {hasException_reg}
                owner {uop_(robIdx_(flag|value)|lqIdx_(flag|value)|uopIdx)_reg(\[[0-9]+\])?}
                exception {uop_exceptionVec_[0-9]+_reg}} {
            set pipe($stage,$field) [focused_state stage-${stage}-${field} load [format {^%s_%s$} $prefix $pattern]]
        }
        if {$feature eq "on"} {
            set pipe($stage,mode) [focused_state stage-${stage}-mode load [format {^%s_fdiLoadMode_reg\[[0-9]+\]$} $prefix]]
        }
    }
    set s1_valid [focused_state reserved-s1-valid load {^s1_valid_reg$}]
    set s3_uncache [focused_state stage-3-uncache-qualification load {^s3_in_(isFrmMisAlignBuf|isvec|nc|mmio|uop_fuOpType)_reg(\[[0-9]+\])?$}]
    set tlb_direct [focused_state translated-fullva-direct translation {^responseFullvaDirect_0_reg$}]
    set tlb_prefault [focused_state translated-fullva-prefault translation {^io_requestor_0_resp_bits_excp_0_(pf|af)_ld_REG_reg$}]
    # Vivado can merge req_out.fullva with the identical response register bank.
    # Resolve the response port's actual registered drivers instead of assuming
    # that both duplicate declaration names survive synthesis.
    set response_pins [module_pins translation {^io_requestor_0_resp_bits_fullva\[[0-9]+\]$}]
    set response_registers [list]
    if {[llength $response_pins]} {
        set nets [get_nets -quiet -segments -of_objects $response_pins]
        if {[llength $nets]} {
            set drivers [get_pins -quiet -leaf -of_objects $nets -filter {DIRECTION == OUT}]
            if {[llength $drivers]} {
                foreach cell [get_cells -quiet -of_objects $drivers] {
                    if {$cell in [dict get $owners translation]} { lappend response_registers $cell }
                }
            }
        }
    }
    set response_registers [lsort -unique $response_registers]
    dict set metrics translated_fullva_driver_registers [llength $response_registers]
    foreach item $response_registers { append inventory "\ntranslated-fullva-driver\t$item" }
    if {[llength $response_registers] == 0} { lappend failures "TLB fullva response has no actual registered driver" }
    set tlb_kind [module_pins translation {^io_requestor_0_req_bits_fullvaKind\[[0-9]+\]$}]
    set tlb_address [module_pins translation {^io_requestor_0_req_bits_fullva\[[0-9]+\]$}]
    set live_sources [concat [all_inputs] $registers]
    focused_capture address-kind-to-tlb-fullva $live_sources $response_registers $tlb_kind
    focused_capture address-kind-to-tlb-direct $live_sources $tlb_direct $tlb_kind
    focused_capture address-kind-to-prefault $live_sources $tlb_prefault $tlb_kind
    focused_capture fullva-to-tlb-response $live_sources $response_registers $tlb_address
    focused_capture tlb-fullva-to-load $response_registers $pipe(2,fullva)
    focused_capture tlb-direct-to-load $tlb_direct $pipe(2,extension)
    focused_capture original-to-high-fullva $split_address $high_address
    focused_capture split-qualification-to-high-fullva $split_qualification $high_address

    focused_capture response-owner-to-mab-state $pipe(3,owner) $split_state
    focused_capture saved-owner-to-mab-state $split_owner $split_state
    focused_capture response-exception-to-mab-selection $pipe(3,exception) $split_exception
    focused_capture original-to-terminal-fullva $split_address $terminal_address
    if {$feature eq "on"} {
        # Only the deferred probe can select a response fault address; the shared
        # native uncached terminal always retains the original starting address.
        focused_capture probe-response-to-terminal-fullva $pipe(3,fullva) $terminal_address
    }
    focused_capture terminal-ack-to-mab-release $pipe(3,terminal) $split_state
    focused_capture mab-reservation-to-s1-valid $split_state $s1_valid
    focused_capture mab-terminal-marker-to-s1 $split_state $pipe(1,terminal)
    focused_capture terminal-fullva-to-s1 $terminal_address $pipe(1,fullva)
    focused_capture terminal-exception-to-s1 $split_exception $pipe(1,exception)
    foreach {stage next} {1 2 2 3} {
        foreach field {fullva extension terminal exception owner} {
            focused_capture stage-${stage}-to-${next}-${field} $pipe($stage,$field) $pipe($next,$field)
        }
        if {$feature eq "on"} {
            focused_capture stage-${stage}-to-${next}-mode $pipe($stage,mode) $pipe($next,mode)
        }
    }
    # These public outputs are dynamic in the actual fixture. Disabled L2L and
    # unconnected external data services are not represented as timed paths.
    foreach {name sources port} [list \
            terminal-state-to-cache-request $split_state io_cacheRequest_valid \
            terminal-state-to-tlb-request $split_state io_phases_tlbRequest_valid \
            terminal-s1-to-sq-forward $pipe(1,terminal) io_phases_sqForward \
            terminal-s1-to-sbuffer-forward $pipe(1,terminal) io_phases_sbufferForward \
            terminal-s3-to-result-valid $pipe(3,terminal) io_result_valid \
            terminal-s3-to-integer-write $pipe(3,terminal) io_result_bits_uop_rfWen \
            terminal-s3-to-floating-write $pipe(3,terminal) io_result_bits_uop_fpWen] {
        focused_path $name $sources [get_ports -quiet $port]
    }
    if {$feature eq "on"} {
        set defer [focused_state deferred-denial load {^s3_fdiDefer_reg$}]
        set late_eligibility [focused_state late-error-eligibility load {^s3_error_eligible_last_REG_reg$}]
        set saved_exception [focused_state saved-original-exception split {^req_uop_exceptionVec_[0-9]+_reg$}]
        focused_capture permission-to-deferred-denial [dict get $owners permission] $defer
        focused_capture permission-to-late-error-eligibility [dict get $owners permission] $late_eligibility
        focused_capture deferred-denial-to-mab $defer $saved_exception
        focused_path probe-mode-to-sq-forward $pipe(1,mode) [get_ports -quiet io_phases_sqForward]
    }
    set exception_address [focused_state exception-address-capture exceptions {^s2_req_0_fullva_reg\[[0-9]+\]$}]
    set exception_payload [focused_state exception-payload-capture exceptions {^s2_req_0_uop_exceptionVec_[0-9]+_reg$}]
    focused_capture terminal-address-to-exception-buffer $pipe(3,fullva) $exception_address
    focused_capture terminal-payload-to-exception-buffer $pipe(3,exception) $exception_payload
    set uncache_valid [focused_state uncache-accepted-valid uncache {^s2_valid_REG_reg$}]
    set uncache_exception [focused_state uncache-accepted-exception uncache {^s2_req_0_uop_exceptionVec_[0-9]+_reg$}]
    focused_capture native-uncache-to-admission $s3_uncache $uncache_valid
    focused_capture terminal-to-uncache-admission $pipe(3,terminal) $uncache_valid
    set entries [instances UncacheEntry uncache/*]
    if {[llength $entries] != 4} { lappend failures "expected four real uncached entry owners" }
    set entry_addresses [list]
    set entry_control [list]
    foreach entry $entries {
        set valid [named_registers $registers [list $entry] {^req_valid_reg$}]
        if {[llength $valid] != 1} { lappend failures "uncached entry lacks its valid register: $entry" }
        focused_capture uncache-exception-to-[file tail $entry] $uncache_exception $valid
        set entry_addresses [concat $entry_addresses [named_registers $registers [list $entry] {^req_paddr_reg\[[0-9]+\]$}]]
        set entry_control [concat $entry_control [named_registers $registers [list $entry] {^(uncacheState|req_valid)_reg(\[[0-9]+\])?$}]]
    }
    set request_valid [focused_state uncache-request-valid uncache {^pipelineReg/pipeline_reg_valid_reg$}]
    set request_address [focused_state uncache-request-address uncache {^pipelineReg/io_out_bits_r_addr_reg\[[0-9]+\]$}]
    focused_capture uncache-entry-to-request-valid $entry_control $request_valid
    focused_capture uncache-entry-to-request-address $entry_addresses $request_address
    write_text [file join $output_directory state-registers.tsv] $inventory
}

write_text [file join $output_directory paths.tsv] $paths

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
if {[llength $latches] || [llength $blackboxes]} {
    lappend failures "unresolved black boxes or latches remain in the synthesized fixture"
}
set summary "metric\tvalue"
dict for {metric value} $metrics { append summary "\n$metric\t$value" }
write_text [file join $output_directory summary.tsv] $summary
write_text [file join $output_directory coverage-failures.txt] [join $failures "\n"]
if {[llength $failures]} { error "local timing coverage is incomplete: [join $failures {; }]" }
write_text [file join $output_directory analysis-status.txt] \
    "ANALYSIS_COMPLETE\nEstimated pre-placement timing only. Read setup, hold and coverage reports; this is not timing PASS."
close_project
