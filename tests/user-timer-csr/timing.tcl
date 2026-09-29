# SPDX-License-Identifier: MulanPSL-2.0

# Analyze the generated production CSR wrapper in both feature configurations.
# This is pre-placement estimated timing, with zero external I/O delay budgets.
# It is not whole-core timing closure or board/signoff evidence.
set use_checkpoints [expr {$argc == 4 && [lindex $argv 0] eq "--checkpoints"}]
if {$use_checkpoints} { set argv [lrange $argv 1 end] }
if {[llength $argv] != 3} {
    error "usage: timing.tcl ?--checkpoints? <disabled.sv-or-dcp> <enabled.sv-or-dcp> <new-output-dir>"
}
set sources [list [file normalize [lindex $argv 0]] [file normalize [lindex $argv 1]]]
set output_root [file normalize [lindex $argv 2]]
foreach source $sources {
    if {![file isfile $source]} { error "analysis input is unavailable: $source" }
}
if {[file exists $output_root]} { error "output directory already exists: $output_root" }
file mkdir $output_root
set_param general.maxThreads 32

proc write_text {path text} {
    set channel [open $path w]
    puts $channel $text
    close $channel
}

proc path_report {directory name from to} {
    if {[llength $from] == 0 || [llength $to] == 0} {
        write_text [file join $directory ${name}.rpt] "No matching startpoints or endpoints."
        write_text [file join $directory ${name}-hold.rpt] "No matching startpoints or endpoints."
        return NA
    }
    set paths [get_timing_paths -from $from -to $to -delay_type max -max_paths 1]
    if {[llength $paths] == 0} {
        write_text [file join $directory ${name}.rpt] "No timing path exists between these objects."
        write_text [file join $directory ${name}-hold.rpt] "No timing path exists between these objects."
        return NA
    }
    # Separate path limits keep small hold slacks from displacing setup paths.
    report_timing -from $from -to $to -delay_type max -max_paths 20 \
        -path_type full_clock_expanded -input_pins -significant_digits 3 \
        -file [file join $directory ${name}.rpt]
    report_timing -from $from -to $to -delay_type min -max_paths 20 \
        -path_type full_clock_expanded -input_pins -significant_digits 3 \
        -file [file join $directory ${name}-hold.rpt]
    return [get_property SLACK [lindex $paths 0]]
}

set summary "configuration\tmetric\tvalue"
set results [dict create]
foreach enabled {0 1} source $sources {
    set variant [expr {$enabled ? "on" : "off"}]
    set directory [file join $output_root $variant]
    file mkdir $directory
    # The period comes from s2c19p-full-rvv-f33333 / F33.333. Boundary delays
    # deliberately model a full-cycle local budget, not real core arrival times.
    set constraints [file join $directory local.xdc]
    write_text $constraints {
create_clock -name clock -period 30.000 -waveform {0.000 15.000} [get_ports clock]
set_input_delay -clock clock 0.000 [get_ports -filter {DIRECTION == IN && NAME != clock}]
set_output_delay -clock clock 0.000 [all_outputs]
}
    if {$use_checkpoints} {
        # Re-evaluate saved local synthesis artifacts without changing the netlist.
        open_checkpoint $source
    } else {
        create_project -in_memory -part xcvu19p-fsva3824-2-e
        read_verilog -sv $source
        read_xdc $constraints
        # Preserve the actual timer and wrapper boundaries for attributable paths.
        synth_design -top UserTimerCSRHarness -part xcvu19p-fsva3824-2-e \
            -mode out_of_context -flatten_hierarchy none
    }
    if {[get_property PART [current_design]] ne "xcvu19p-fsva3824-2-e"} {
        error "$variant has an unexpected target part"
    }
    if {[llength [get_clocks -quiet]] == 0} { read_xdc -unmanaged $constraints }
    set clocks [get_clocks]
    if {[llength $clocks] != 1 || [get_property PERIOD $clocks] != 30.000} {
        error "$variant does not have the expected single 30 ns clock"
    }

    set registers [all_registers -edge_triggered]
    set latches [all_registers -level_sensitive]
    set blackboxes [get_cells -quiet -hierarchical -filter {IS_BLACKBOX == 1}]
    set timers [get_cells -quiet -hierarchical \
        -filter {REF_NAME == UserTimer || ORIG_REF_NAME == UserTimer}]
    # FIRRTL may share module definitions with an existing CSR. The seven
    # independent instances, not their deduplicated REF_NAMEs, own bank state.
    set banks [get_cells -quiet -hierarchical -regexp {^.*/userTimerCSRMods_[^/]+$}]
    set metrics [dict create input_file $source input_is_checkpoint $use_checkpoints \
        part xcvu19p-fsva3824-2-e period_ns 30.000 \
        external_input_delay_ns 0.000 external_output_delay_ns 0.000 \
        estimated_pre_route 1 hierarchy_preserved 1 \
        registers [llength $registers] latches [llength $latches] \
        blackboxes [llength $blackboxes] timers [llength $timers] bank_modules [llength $banks]]
    set bank_register_count 0
    foreach index {0 1 2 3 4 5 6} {
        set instance_name csr/csrMod/userTimerCSRMods_$index
        set instances [get_cells -quiet $instance_name]
        set bank_registers [filter $registers "NAME =~ $instance_name/*"]
        set register_count [llength $bank_registers]
        dict set metrics bank_${index}_instances [llength $instances]
        dict set metrics bank_${index}_registers $register_count
        incr bank_register_count $register_count
    }
    dict set metrics bank_registers $bank_register_count
    foreach {metric pattern} {lut_primitives LUT* ff_primitives FD* carry8_primitives CARRY8} {
        dict set metrics $metric [llength [get_cells -quiet -hierarchical -filter "REF_NAME =~ $pattern"]]
    }
    report_utilization -file [file join $directory utilization.rpt]
    report_utilization -hierarchical -file [file join $directory utilization-hierarchy.rpt]
    set checks [check_timing -verbose -return_string]
    write_text [file join $directory check-timing.rpt] $checks
    report_timing_summary -delay_type min_max -check_timing_verbose \
        -report_unconstrained -max_paths 20 -significant_digits 3 \
        -file [file join $directory timing-summary.rpt]
    foreach {name from to} [list \
            register-to-register $registers $registers \
            input-to-register [all_inputs] $registers \
            register-to-output $registers [all_outputs] \
            input-to-output [all_inputs] [all_outputs]] {
        dict set metrics ${name}_setup_slack_ns [path_report $directory $name $from $to]
    }
    if {$enabled && [llength $timers] == 1} {
        set timer_registers [filter $registers "NAME =~ [lindex $timers 0]/*"]
        set external_registers [filter $registers "NAME !~ [lindex $timers 0]/*"]
        set writeback_registers [filter $registers {NAME =~ */wdataReg_reg*}]
        dict set metrics timer_registers [llength $timer_registers]
        dict set metrics timer_internal_setup_slack_ns [path_report $directory timer-internal \
            $timer_registers $timer_registers]
        dict set metrics timer_to_csr_setup_slack_ns [path_report $directory timer-to-csr \
            $timer_registers $external_registers]
        dict set metrics csr_to_timer_setup_slack_ns [path_report $directory csr-to-timer \
            [concat [all_inputs] $external_registers] $timer_registers]
        dict set metrics timer_to_output_setup_slack_ns [path_report $directory timer-to-output \
            $timer_registers [all_outputs]]
        dict set metrics timer_to_writeback_setup_slack_ns [path_report $directory timer-to-writeback \
            $timer_registers $writeback_registers]
    }
    # Missing check categories must not look like a clean local timing result.
    set counts [dict create]
    foreach line [split $checks \n] {
        if {[regexp {^[0-9]+[.][[:space:]]+checking[[:space:]]+([a-z_]+)[[:space:]]+\(([0-9]+)\)} \
                $line match category count]} { dict set counts $category $count }
    }
    foreach category {loops latch_loops no_clock unconstrained_internal_endpoints} {
        if {![dict exists $counts $category]} { error "missing check_timing category: $category" }
        dict set metrics $category [dict get $counts $category]
    }
    dict for {metric value} $metrics { append summary "\n$variant\t$metric\t$value" }
    write_text [file join $output_root summary.tsv] $summary
    dict set results $variant $metrics
    if {!$use_checkpoints} { write_checkpoint [file join $directory synth.dcp] }
    if {[llength $latches] || [llength $blackboxes]} {
        error "$variant contains latches or unresolved black boxes; inspect reports"
    }
    if {[llength $timers] != $enabled || [llength $banks] != 7 * $enabled} {
        error "$variant timer/bank instance count does not match the feature setting"
    }
    foreach index {0 1 2 3 4 5 6} expected_bits {2 1 62 64 63 64 64} {
        if {[dict get $metrics bank_${index}_instances] != $enabled ||
                [dict get $metrics bank_${index}_registers] != $expected_bits * $enabled} {
            error "$variant bank instance $index does not have the expected independent state"
        }
    }
    if {$enabled && [dict get $metrics timer_registers] != 65} {
        error "enabled timer does not contain the expected 64-bit count and pending register"
    }
    if {$enabled} {
        foreach metric {timer_internal_setup_slack_ns csr_to_timer_setup_slack_ns timer_to_writeback_setup_slack_ns} {
            set slack [dict get $metrics $metric]
            if {![string is double -strict $slack] || ![expr {abs($slack) < Inf}]} {
                error "enabled timer has no finite timing result for $metric"
            }
        }
    }
    foreach category {loops latch_loops no_clock unconstrained_internal_endpoints} {
        if {[dict get $counts $category] != 0} { error "$variant failed check_timing $category" }
    }
    if {[dict get $metrics register-to-register_setup_slack_ns] eq "NA"} {
        error "$variant has no analyzable register-to-register timing path"
    }
    close_project
}
foreach metric {registers lut_primitives ff_primitives carry8_primitives} {
    append summary "\ndelta\t$metric\t[expr {[dict get $results on $metric] - [dict get $results off $metric]}]"
}
write_text [file join $output_root summary.tsv] $summary
puts "UIT02_LOCAL_TIMING_ANALYSIS=COMPLETE pre_route=1 period_ns=30.000 io_delay_ns=0.000 output=$output_root"
