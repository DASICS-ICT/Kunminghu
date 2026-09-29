# SPDX-License-Identifier: MulanPSL-2.0

# Compare the same production Backend/Ftq fixture in both feature configurations.
# Zero external delays give a local cycle budget, not product arrival times.
# The read-only observation ports and unplaced clocks are part of this fixture.
set focused_only [expr {$argc == 4 && [lindex $argv 0] eq "--focused-checkpoints"}]
set use_checkpoints [expr {$argc == 4 && ([lindex $argv 0] eq "--checkpoints" || $focused_only)}]
if {$use_checkpoints} { set argv [lrange $argv 1 end] }
if {[llength $argv] != 3} {
    error "usage: timing.tcl ?--checkpoints|--focused-checkpoints? <disabled-RTL-directory-or-DCP> <enabled-RTL-directory-or-DCP> <new-output-directory>"
}
set sources [list [file normalize [lindex $argv 0]] [file normalize [lindex $argv 1]]]
set output_root [file normalize [lindex $argv 2]]
foreach source $sources {
    if {$use_checkpoints} {
        if {![file isfile $source]} { error "Synthesis checkpoint is unavailable: $source" }
    } else {
        if {![file isdirectory $source]} { error "RTL directory is unavailable: $source" }
        if {![file isfile [file join $source UserTimerDeliveryHarness.sv]]} {
            error "production delivery fixture RTL is unavailable: $source"
        }
    }
}
if {[file exists $output_root]} { error "output directory already exists: $output_root" }
file mkdir $output_root
set_param general.maxThreads 32

proc write_text {path value} {
    set channel [open $path w]
    puts $channel $value
    close $channel
}

proc instances {reference} {
    return [get_cells -quiet -hierarchical -filter \
        "REF_NAME == $reference || ORIG_REF_NAME == $reference"]
}

proc owned_registers {registers owners} {
    set result [list]
    foreach owner $owners {
        set result [concat $result [filter $registers "NAME =~ $owner/*"]]
    }
    return [lsort -unique $result]
}

proc remove_objects {collection remove} {
    set result [list]
    set excluded [dict create]
    foreach object $remove { dict set excluded $object 1 }
    foreach object $collection {
        if {![dict exists $excluded $object]} { lappend result $object }
    }
    return $result
}

proc named_registers {registers owners prefixes} {
    set result [list]
    foreach owner $owners {
        foreach prefix $prefixes {
            set result [concat $result [filter $registers "NAME =~ $owner/$prefix*"]]
        }
    }
    return [lsort -unique $result]
}

proc interface_pins {owners port_name} {
    set result [list]
    foreach owner $owners {
        set result [concat $result [get_pins -quiet $owner/$port_name]]
    }
    return [lsort -unique $result]
}

proc leaf_drivers {pins} {
    if {[llength $pins] == 0} { return [list] }
    set nets [get_nets -quiet -segments -of_objects $pins]
    if {[llength $nets] == 0} { return [list] }
    return [lsort -unique [get_pins -quiet -leaf -of_objects $nets -filter {DIRECTION == OUT}]]
}

proc check_report_paths {path expected} {
    set channel [open $path r]
    set contents [read $channel]
    close $channel
    set actual [regexp -all -line -- {^Slack \(} $contents]
    if {$actual != $expected} {
        error "Timing report omitted retrieved paths: $path expected=$expected actual=$actual"
    }
}

proc path_reports {directory category from to {through_groups {}}} {
    set info "startpoint_count=[llength $from]\nendpoint_count=[llength $to]"
    set through_options [list]
    set index 0
    foreach group $through_groups {
        append info "\nthrough_${index}_count=[llength $group]"
        write_text [file join $directory ${category}-through-${index}.txt] [join $group "\n"]
        if {[llength $group] == 0} {
            write_text [file join $directory ${category}-coverage.txt] "$info\nstatus=NO_THROUGH_OBJECTS"
            return [list NA NA NA NA]
        }
        lappend through_options -through $group
        incr index
    }
    if {[llength $from] == 0 || [llength $to] == 0} {
        write_text [file join $directory ${category}-coverage.txt] "$info\nstatus=NO_MATCHING_OBJECTS"
        return [list NA NA NA NA]
    }
    # Report the retrieved paths directly; repeating from/to scans is costly on the full fixture.
    set max_paths [get_timing_paths -quiet -from $from -to $to {*}$through_options -delay_type max -max_paths 20]
    if {[llength $max_paths] == 0} {
        write_text [file join $directory ${category}-coverage.txt] "$info\nstatus=NO_TIMING_PATH"
        return [list NA NA NA NA]
    }
    report_timing -of_objects $max_paths \
        -path_type full_clock_expanded -input_pins -significant_digits 3 \
        -file [file join $directory ${category}-setup.rpt]
    check_report_paths [file join $directory ${category}-setup.rpt] [llength $max_paths]
    set setup [lindex $max_paths 0]
    set setup_metrics [list [get_property SLACK $setup] [get_property DATAPATH_DELAY $setup] \
        [get_property LOGIC_LEVELS $setup]]
    set min_paths [get_timing_paths -quiet -from $from -to $to {*}$through_options -delay_type min -max_paths 20]
    if {[llength $min_paths] == 0} {
        write_text [file join $directory ${category}-coverage.txt] "$info\nstatus=NO_TIMING_PATH"
        return [list NA NA NA NA]
    }
    report_timing -of_objects $min_paths \
        -path_type full_clock_expanded -input_pins -significant_digits 3 \
        -file [file join $directory ${category}-hold.rpt]
    check_report_paths [file join $directory ${category}-hold.rpt] [llength $min_paths]
    write_text [file join $directory ${category}-coverage.txt] \
        "$info\nsetup_path_count=[llength $max_paths]\nhold_path_count=[llength $min_paths]\nstatus=REPORTED"
    set hold [lindex $min_paths 0]
    return [concat $setup_metrics [list [get_property SLACK $hold]]]
}

set summary "configuration\tmetric\tvalue"
set path_summary "configuration\tpath_category\tsetup_slack_ns\tdata_delay_ns\tlogic_levels\thold_slack_ns"
foreach enabled {0 1} source $sources {
    set variant [expr {$enabled ? "on" : "off"}]
    if {$focused_only && !$enabled} {
        # OFF already completed all checks and reports before ON synthesis started.
        set prior_root [file dirname [file dirname $source]]
        set off_metrics [dict create]
        foreach {filename variable} {summary.tsv summary paths.tsv path_summary} {
            set channel [open [file join $prior_root $filename] r]
            set contents [read $channel]
            close $channel
            set rows 0
            foreach line [split $contents "\n"] {
                if {[string first "off\t" $line] == 0} {
                    append $variable "\n$line"
                    incr rows
                    if {$filename eq "summary.tsv"} {
                        lassign [split $line "\t"] configuration metric value
                        dict set off_metrics $metric $value
                    }
                }
            }
            if {$filename eq "summary.tsv" && $rows == 0} {
                error "Completed OFF metrics are unavailable: $prior_root/$filename"
            }
        }
        foreach {metric expected} {timer_instances 0 user_bank_instances 0 user_event_registers 0
                unexpected_latches 0 blackboxes 0 filter_instances 1 rob_instances 1
                control_instances 1 csr_instances 1 ftq_instances 1 period_ns 30.000
                input_delay_ns 0.000 output_delay_ns 0.000} {
            if {![dict exists $off_metrics $metric] || [dict get $off_metrics $metric] != $expected} {
                error "Completed OFF metric is inconsistent: $metric"
            }
        }
        file mkdir [file join $output_root off]
        write_text [file join $output_root off reused-report-directory.txt] [file dirname $source]
        continue
    }
    if {$enabled && !$use_checkpoints} {
        set trace_source [file join $source TraceBuffer.sv]
        if {![file isfile $trace_source]} { error "Enabled RTL is missing the real TraceBuffer" }
        set channel [open $trace_source r]
        set trace_text [read $channel]
        close $channel
        if {[string first "io_out_blockCommitNext" $trace_text] < 0} {
            error "Enabled RTL predates the trace-capacity completion guard"
        }
    }
    set directory [file join $output_root $variant]
    file mkdir $directory
    set constraints [file join $directory local.xdc]
    write_text $constraints {
create_clock -name clock -period 30.000 -waveform {0.000 15.000} [get_ports clock]
set_input_delay -clock clock 0.000 [get_ports -filter {DIRECTION == IN && NAME != clock}]
set_output_delay -clock clock 0.000 [all_outputs]
}
    if {$use_checkpoints} {
        # Retain the checkpoint's original constraints; reporting must not change the timing model.
        open_checkpoint $source
        write_text [file join $directory checkpoint-input.txt] $source
    } else {
        create_project -in_memory -part xcvu19p-fsva3824-2-e
        set system_verilog [lsort [glob -nocomplain -directory $source *.sv]]
        set verilog [lsort [glob -nocomplain -directory $source *.v]]
        write_text [file join $directory rtl-inputs.txt] [join [concat $system_verilog $verilog] "\n"]
        read_verilog -sv $system_verilog
        if {[llength $verilog] != 0} { read_verilog $verilog }
        read_xdc $constraints
        synth_design -top UserTimerDeliveryHarness -part xcvu19p-fsva3824-2-e \
            -mode out_of_context -flatten_hierarchy none
        # Preserve successful synthesis before any report selection or coverage check can fail.
        write_checkpoint [file join $directory synth.dcp]
    }
    if {[get_property PART [current_design]] ne "xcvu19p-fsva3824-2-e"} {
        error "$variant has an unexpected target part"
    }
    set clocks [get_clocks -quiet]
    if {[llength $clocks] != 1 || [get_property PERIOD $clocks] != 30.000} {
        error "$variant does not have the expected single 30 ns clock"
    }
    set registers [all_registers -edge_triggered]
    set latches [all_registers -level_sensitive]
    set clock_gates [instances ClockGate]
    set clock_gate_latches [owned_registers $latches $clock_gates]
    set unexpected_latches [remove_objects $latches $clock_gate_latches]
    set blackboxes [get_cells -quiet -hierarchical -filter {IS_BLACKBOX == 1}]
    set timers [instances UserTimer]
    set banks [get_cells -quiet -hierarchical -regexp {^.*/userTimerCSRMods_[0-6]$}]
    set backends [instances Backend]
    set filters [instances InterruptFilter]
    set robs [instances Rob]
    set controls [instances CtrlBlock]
    set csrs [instances NewCSR]
    set ftqs [instances Ftq]
    set traces [instances Trace]
    set trace_buffers [instances TraceBuffer]
    set required_coverage_failures [list]
    set metrics [dict create registers [llength $registers] latches [llength $latches] \
        clock_gate_latches [llength $clock_gate_latches] unexpected_latches [llength $unexpected_latches] \
        blackboxes [llength $blackboxes] timer_instances [llength $timers] \
        user_bank_instances [llength $banks] filter_instances [llength $filters] \
        rob_instances [llength $robs] control_instances [llength $controls] \
        csr_instances [llength $csrs] ftq_instances [llength $ftqs] \
        period_ns 30.000 input_delay_ns 0.000 output_delay_ns 0.000 estimated_pre_route 1 \
        input_is_checkpoint $use_checkpoints]
    foreach {metric pattern} {lut_primitives LUT* ff_primitives FD* carry8_primitives CARRY8} {
        dict set metrics $metric [llength [get_cells -quiet -hierarchical -filter "REF_NAME =~ $pattern"]]
    }
    if {!$focused_only} {
        report_utilization -file [file join $directory utilization.rpt]
        report_utilization -hierarchical -file [file join $directory utilization-hierarchy.rpt]
    }
    set latch_inventory "category\tcell"
    foreach cell $clock_gate_latches { append latch_inventory "\nproduction_clock_gate\t$cell" }
    foreach cell $unexpected_latches { append latch_inventory "\nunexpected\t$cell" }
    write_text [file join $directory latches.tsv] $latch_inventory
    if {$focused_only} {
        # Reuse completed global reports from the checkpoint's original directory.
        set global_directory [file dirname $source]
        foreach report {utilization.rpt utilization-hierarchy.rpt timing-summary.rpt check-timing.rpt high-fanout.rpt} {
            if {![file isfile [file join $global_directory $report]]} {
                error "Completed global report is unavailable: $global_directory/$report"
            }
        }
        write_text [file join $directory global-report-directory.txt] $global_directory
    } else {
        report_timing_summary -delay_type min_max -check_timing_verbose \
            -report_unconstrained -max_paths 20 -significant_digits 3 \
            -file [file join $directory timing-summary.rpt]
        write_text [file join $directory check-timing.rpt] [check_timing -verbose -return_string]
        report_high_fanout_nets -timing -max_nets 100 -file [file join $directory high-fanout.rpt]
    }

    set filter_registers [owned_registers $registers $filters]
    set rob_registers [owned_registers $registers $robs]
    set control_registers [owned_registers $registers $controls]
    set csr_registers [owned_registers $registers $csrs]
    set ftq_registers [owned_registers $registers $ftqs]
    set timer_registers [owned_registers $registers $timers]
    set bank_registers [owned_registers $registers $banks]
    set csr_control_registers [remove_objects $csr_registers [concat $filter_registers $timer_registers]]
    set control_without_rob [remove_objects $control_registers $rob_registers]
    set named_hu_registers [filter $registers \
        {NAME =~ *hu* || NAME =~ *Hu* || NAME =~ *userInHandler* || NAME =~ *userEntry* || NAME =~ *userReturn*}]
    # Only real production state may bound event paths; harness counters remain in total area.
    set hu_registers [owned_registers $named_hu_registers [concat $backends $ftqs]]
    set excluded_observer_registers [remove_objects $named_hu_registers $hu_registers]
    write_text [file join $directory excluded-observer-event-registers.txt] \
        [join $excluded_observer_registers "\n"]
    dict set metrics excluded_observer_event_registers [llength $excluded_observer_registers]
    # Control's terminal record and PC qualifiers are feature-owned despite generic field names.
    set control_event_registers [named_registers $registers $controls \
        {savedSatpMode savedPc pcReady requestSent terminalPending terminal aheadSent superseded externalTargetConsumed}]
    set filter_event_registers [named_registers $registers $filters {candidateStages}]
    set rob_event_registers [named_registers $registers $robs {interruptDescriptorReg candidateValid}]
    set csr_delivery_registers [named_registers $registers $csrs \
        {deliveredInterrupt nmiInFlight deferredCriticalDebug criticalDebugInFlight}]
    set hu_registers [lsort -unique [concat $hu_registers $control_event_registers \
        $filter_event_registers $rob_event_registers $csr_delivery_registers]]
    set trace_stage_registers [named_registers $registers $traces {s1_out_blocks}]
    set trace_queue_registers [named_registers $registers $trace_buffers {enqPtr deqPtr blockCommit}]
    set trace_capacity_registers [lsort -unique [concat $trace_stage_registers $trace_queue_registers]]
    set handler_registers [named_registers $registers $csrs {userInHandler}]
    set state_inventory "category\tregister"
    foreach {category members} [list filter $filter_registers rob $rob_registers \
            control $control_without_rob csr $csr_control_registers ftq $ftq_registers \
            timer $timer_registers bank $bank_registers user_event $hu_registers \
            trace_capacity $trace_capacity_registers] {
        dict set metrics ${category}_registers [llength $members]
        foreach member [lsort $members] { append state_inventory "\n$category\t$member" }
    }
    write_text [file join $directory state-registers.tsv] $state_inventory
    if {$enabled} {
      # Global min/max coverage is already in timing-summary.rpt; keep focused production paths.
      foreach {category from to} [list \
            csr-to-filter $csr_control_registers $filter_registers \
            filter-to-rob $filter_registers $rob_registers \
            csr-to-rob $csr_control_registers $rob_registers \
            rob-to-csr $rob_registers $csr_control_registers \
            control-to-csr $control_without_rob $csr_control_registers \
            csr-to-control $csr_control_registers $control_without_rob \
            control-to-ftq $control_without_rob $ftq_registers] {
        set result [path_reports $directory $category $from $to]
        append path_summary "\n$variant\t$category\t[join $result \t]"
      }
    }
    if {$enabled} {
        foreach {category from to} [list \
                timer-to-filter $timer_registers $filter_registers \
                timer-to-event $timer_registers $hu_registers \
                csr-to-event $csr_control_registers $hu_registers \
                filter-to-event $filter_registers $hu_registers \
                event-to-filter $hu_registers $filter_registers \
                event-to-event $hu_registers $hu_registers \
                event-to-bank $hu_registers $bank_registers \
                event-to-timer $hu_registers $timer_registers \
                event-to-ftq $hu_registers $ftq_registers] {
            set result [path_reports $directory $category $from $to]
            append path_summary "\n$variant\t$category\t[join $result \t]"
        }
        # Driver pins keep through-points on the common internal nets before bank/observer fanout.
        # Requiring the lookahead driver distinguishes the new capacity path from registered blocking.
        set lookahead_pins [interface_pins $trace_buffers io_out_blockCommitNext]
        set ready_pins [interface_pins $csrs io_huEntry_completion_ready]
        set effect_pins [interface_pins $csrs io_status_userEntryEffect]
        set lookahead_drivers [leaf_drivers $lookahead_pins]
        set ready_drivers [leaf_drivers $ready_pins]
        set effect_drivers [leaf_drivers $effect_pins]
        set trace_inputs [get_ports -quiet {io_traceEnable io_traceStall}]
        set ready_output [get_ports -quiet io_huCompletionReady]
        set entry_through [list $lookahead_drivers $ready_drivers $effect_drivers]
        set ready_through [list $lookahead_drivers $ready_drivers]
        foreach {category from to through} [list \
                trace-lookahead-state-to-ready $trace_capacity_registers $ready_output $ready_through \
                trace-lookahead-input-to-ready $trace_inputs $ready_output $ready_through \
                trace-lookahead-state-to-bank $trace_capacity_registers $bank_registers $entry_through \
                trace-lookahead-input-to-bank $trace_inputs $bank_registers $entry_through \
                trace-lookahead-state-to-handler $trace_capacity_registers $handler_registers $entry_through \
                trace-lookahead-state-to-timer $trace_capacity_registers $timer_registers $entry_through \
                trace-lookahead-state-to-terminal $trace_capacity_registers $control_event_registers $ready_through] {
            set result [path_reports $directory $category $from $to $through]
            append path_summary "\n$variant\t$category\t[join $result \t]"
            if {[lindex $result 0] eq "NA" || [lindex $result 3] eq "NA"} {
                lappend required_coverage_failures $category
            }
        }
        write_text [file join $directory trace-capacity-coverage.txt] \
            "trace_instances=[join $traces ,]\ntrace_buffer_instances=[join $trace_buffers ,]\nlookahead_pins=[join $lookahead_pins ,]\nready_pins=[join $ready_pins ,]\neffect_pins=[join $effect_pins ,]\nmissing_paths=[join $required_coverage_failures ,]"
    }
    dict for {metric value} $metrics { append summary "\n$variant\t$metric\t$value" }
    write_text [file join $output_root summary.tsv] $summary
    write_text [file join $output_root paths.tsv] $path_summary

    foreach {module owners} [list Backend $backends InterruptFilter $filters Rob $robs CtrlBlock $controls NewCSR $csrs Ftq $ftqs] {
        if {[llength $owners] != 1} { error "$variant requires one real $module; found [llength $owners]" }
    }
    if {$enabled && ([llength $timers] != 1 || [llength $banks] != 7)} {
        error "enabled fixture did not preserve the single timer and seven user banks"
    }
    if {!$enabled && ([llength $timers] != 0 || [llength $banks] != 0 || [llength $hu_registers] != 0)} {
        error "disabled fixture contains added timer, bank or event state"
    }
    if {[llength $unexpected_latches] != 0 || [llength $blackboxes] != 0} {
        error "$variant contains unexpected latches or blackboxes; inspect saved reports"
    }
    if {[llength $required_coverage_failures] != 0} {
        error "$variant is missing required trace-capacity timing paths: $required_coverage_failures"
    }
    close_project
}
write_text [file join $output_root analysis-status.txt] \
    "ANALYSIS_COMPLETE\nEstimated pre-placement timing only. Read setup, hold and coverage reports; this is not timing PASS."
