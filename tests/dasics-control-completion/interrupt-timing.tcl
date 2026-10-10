# SPDX-License-Identifier: MulanPSL-2.0

# Read an existing Backend checkpoint and report the real M/HS candidate cones.
# No synthesis, constraint modification, placement or routing is performed here.
if {$argc != 4} {
    error "usage: interrupt-timing.tcl <before|after> <synth.dcp> <RTL-export-bundle> <new-output-directory>"
}
lassign $argv revision checkpoint bundle output_directory
set checkpoint [file normalize $checkpoint]
set bundle [file normalize $bundle]
set output_directory [file normalize $output_directory]
if {$revision ni {before after}} { error "interrupt revision must be before/after" }
if {[file exists $output_directory]} { error "output directory already exists: $output_directory" }

proc irq_read {path} {
    set stream [open $path r]
    fconfigure $stream -translation binary -encoding binary
    set value [read $stream]
    close $stream
    return $value
}
proc irq_write {path value} {
    set stream [open $path w]
    puts $stream $value
    close $stream
}
set identity [dict create]
foreach row [split [string trim [irq_read [file join $bundle identity.tsv]]] \n] {
    set fields [split $row \t]
    if {[llength $fields] != 2 || [dict exists $identity [lindex $fields 0]]} {
        error "invalid exported identity"
    }
    dict set identity {*}$fields
}
if {[dict get $identity fixture] ne "backend" ||
        [dict get $identity top] ne "FDIFullControlHarness" ||
        [dict get $identity feature] ni {on off}} {
    error "interrupt timing requires the real Backend fixture"
}
set feature [dict get $identity feature]
set origin [file dirname $checkpoint]
set expected_constraints {
create_clock -name clock -period 30.000 -waveform {0.000 15.000} [get_ports clock]
set_input_delay -clock clock 0.000 [get_ports -filter {DIRECTION == IN && NAME != clock}]
set_output_delay -clock clock 0.000 [all_outputs]
}
if {![file isfile $checkpoint] ||
        [irq_read [file join $origin input-manifest.json]] ne [irq_read [file join $bundle manifest.json]] ||
        [irq_read [file join $origin local.xdc]] ne "${expected_constraints}\n"} {
    error "checkpoint source identity or original constraints do not match"
}
file mkdir $output_directory
file copy [file join $bundle manifest.json] [file join $output_directory input-manifest.json]
file copy [file join $bundle identity.tsv] [file join $output_directory identity.tsv]
file copy [file join $origin local.xdc] [file join $output_directory local.xdc]
irq_write [file join $output_directory checkpoint-input.txt] $checkpoint
set_param general.maxThreads 32
open_checkpoint $checkpoint
set clocks [get_clocks -quiet]
if {[get_property TOP [current_design]] ne "FDIFullControlHarness" ||
        [get_property PART [current_design]] ne "xcvu19p-fsva3824-2-e" ||
        [llength $clocks] != 1 || [get_property PERIOD $clocks] != 30.000} {
    error "unexpected checkpoint top, part or clock"
}

proc irq_modules {reference} {
    set result [list]
    foreach cell [get_cells -quiet -hierarchical -filter \
            "REF_NAME =~ ${reference}* || ORIG_REF_NAME =~ ${reference}*"] {
        foreach property {REF_NAME ORIG_REF_NAME} {
            if {[regexp [format {^%s(_[0-9]+)?$} $reference] [get_property $property $cell]]} {
                lappend result $cell
                break
            }
        }
    }
    return [lsort -unique $result]
}
proc irq_owned {objects owner pattern} {
    set result [list]
    foreach object $objects {
        set name [get_property NAME $object]
        if {[string first "$owner/" $name] == 0 &&
                [regexp -- $pattern [string range $name [expr {[string length $owner] + 1}] end]]} {
            lappend result $object
        }
    }
    return $result
}
proc irq_interface {owner pattern} {
    return [irq_owned [get_pins -quiet -of_objects [get_cells $owner]] $owner $pattern]
}
proc irq_drivers {pins} {
    if {[llength $pins] == 0} { return [list] }
    set nets [get_nets -quiet -segments -of_objects $pins]
    if {[llength $nets] == 0} { return [list] }
    return [lsort -unique [get_pins -quiet -leaf -of_objects $nets -filter {DIRECTION == OUT}]]
}
proc irq_topi_readback {drivers csr} {
    if {[llength $drivers] == 0} { return [list] }
    # TOPI IID occupies CSR read-data bits 16..21 for the implemented IRQ causes.
    # Keep only real readback D pins reached by these drivers, excluding observers.
    set endpoints [all_fanout -flat -endpoints_only -from $drivers]
    return [lsort -unique [irq_owned $endpoints $csr \
        {^io_out_bits_rData_r_reg\[(1[6-9]|2[01])\]/D$}]]
}
proc irq_group {name objects {required 1}} {
    global inventory metrics failures
    dict set metrics ${name}_count [llength $objects]
    foreach object $objects { append inventory "\n$name\t$object" }
    if {$required && [llength $objects] == 0} { lappend failures "missing required object group: $name" }
    return $objects
}
proc irq_path {name from to {through_groups {}}} {
    global output_directory paths failures
    set info "startpoints=[llength $from]\nendpoints=[llength $to]"
    set options [list]
    set incomplete [expr {[llength $from] == 0 || [llength $to] == 0}]
    set index 0
    foreach group $through_groups {
        append info "\nthrough_${index}=[llength $group]"
        irq_write [file join $output_directory ${name}-through-${index}.txt] [join $group \n]
        if {[llength $group] == 0} { set incomplete 1 }
        # Each group is mandatory and ordered. One flattened list would mean OR.
        lappend options -through $group
        incr index
    }
    if {$incomplete} {
        lappend failures "$name has missing endpoints or through points"
        append paths "\n$name\tNA\tNA\tNA\tNA"
        irq_write [file join $output_directory ${name}-coverage.txt] "$info\nstatus=MISSING_OBJECTS"
        return
    }
    set values [list]
    foreach delay {max min} kind {setup hold} {
        set found [get_timing_paths -quiet -from $from -to $to {*}$options -delay_type $delay -max_paths 10]
        if {[llength $found] == 0} {
            lappend failures "$name has no $kind path"
            append paths "\n$name\tNA\tNA\tNA\tNA"
            irq_write [file join $output_directory ${name}-coverage.txt] "$info\nstatus=MISSING_${kind}_PATH"
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
            lappend failures "$name has a nonfinite timing value"
        }
    }
    append paths "\n$name\t[join $values \t]"
    irq_write [file join $output_directory ${name}-coverage.txt] "$info\nstatus=REPORTED"
}

set filters [irq_modules InterruptFilter]
set csrs [irq_modules NewCSR]
if {[llength $filters] != 1 || [llength $csrs] != 1} {
    error "expected one real InterruptFilter inside one real NewCSR"
}
set intr [lindex $filters 0]
set csr [lindex $csrs 0]
if {[string first "$csr/" $intr] != 0} { error "InterruptFilter is not owned by NewCSR" }
set registers [all_registers -edge_triggered]
set intr_state [irq_owned $registers $intr {.*}]
set outside_filter [list]
foreach cell $registers {
    if {$cell ni $intr_state} { lappend outside_filter $cell }
}
# Include the Backend's registered platform IRQ inputs as well as CSR banks.
# Excluding the filter's registered priorities is essential for the immediate cone.
set live_sources [concat [all_inputs] $outside_filter]
set failures [list]
set inventory "category\tobject"
set paths "category\tsetup_slack_ns\tdata_delay_ns\tlogic_levels\thold_slack_ns"
set metrics [dict merge $identity [dict create interrupt_revision $revision \
    checkpoint $checkpoint estimated_pre_route 1 period_ns 30.000 \
    input_delay_ns 0.000 output_delay_ns 0.000 InterruptFilter $intr NewCSR $csr]]
set m_valid [irq_group m_gather_valid [irq_owned $registers $intr {^mtopiIsNotZero_reg(\[[0-9]+\])?$}] \
    [expr {$revision eq "after"}]]
set hs_valid [irq_group hs_gather_valid [irq_owned $registers $intr {^stopiIsNotZero_reg(\[[0-9]+\])?$}] \
    [expr {$revision eq "after"}]]
if {$revision eq "before" && ([llength $m_valid] || [llength $hs_valid])} {
    lappend failures "a before checkpoint already contains the new gather-valid registers"
}
dict set metrics baseline_validity_combinational [expr {$revision eq "before"}]
set m_prio [irq_group m_priority [irq_owned $registers $intr {^mipriosReg_[0-9]+_.*}]]
set hs_prio [irq_group hs_priority [irq_owned $registers $intr {^hsipriosReg_[0-9]+_.*}]]
set m_gather [irq_group m_gather_inputs [irq_interface $intr {^io_in_(mip|mie|mideleg)(_|\[|$)}]]
set hs_gather [irq_group hs_gather_inputs [irq_interface $intr {^io_in_(sip|sie|hip|hie|hideleg)(_|\[|$)}]]
set m_iid [irq_group m_topi_iid [irq_interface $intr {^io_out_mtopi_IID(\[|$)}]]
set hs_iid [irq_group hs_topi_iid [irq_interface $intr {^io_out_stopi_IID(\[|$)}]]
set m_topi_drivers [irq_group m_topi_leaf_drivers [irq_drivers $m_iid]]
set hs_topi_drivers [irq_group hs_topi_leaf_drivers [irq_drivers $hs_iid]]
set m_readback [irq_group m_topi_csr_readback [irq_topi_readback $m_topi_drivers $csr]]
set hs_readback [irq_group hs_topi_csr_readback [irq_topi_readback $hs_topi_drivers $csr]]
foreach {mode readback} [list m $m_readback hs $hs_readback] {
    if {[llength $readback] != 6} {
        lappend failures "$mode TOPI readback requires all six CSR data D pins 16..21; found [llength $readback]"
    }
}
set stages [irq_owned $registers $intr {^candidateStages_[0-9]+_.*}]
if {$feature eq "on"} {
    set raw_cause [irq_group raw_candidate_cause [irq_owned $registers $intr {^candidateStages_0_bits_cause_reg(\[[0-9]+\])?$}]]
    set raw_valid [irq_group raw_candidate_valid [irq_owned $registers $intr {^candidateStages_0_valid_reg$}]]
    for {set stage 0} {$stage < 6} {incr stage} {
        set cause($stage) [irq_group candidate_${stage}_cause [irq_owned $registers $intr \
            [format {^candidateStages_%d_bits_cause_reg(\[[0-9]+\])?$} $stage]]]
        set valid($stage) [irq_group candidate_${stage}_valid [irq_owned $registers $intr \
            [format {^candidateStages_%d_valid_reg$} $stage]]]
    }
    for {set stage 0} {$stage < 5} {incr stage} {
        set next [expr {$stage + 1}]
        irq_path candidate-${stage}-to-${next}-cause $cause($stage) $cause($next)
        irq_path candidate-${stage}-to-${next}-valid $valid($stage) $valid($next)
    }
} else {
    if {[llength $stages]} { lappend failures "HasFDI-off retains HU candidate stages" }
    set raw_cause [irq_group normal_interrupt_capture [irq_owned $registers $intr {^intrVecReg_reg(\[[0-9]+\])?$}]]
}
# Synthesis can duplicate the IID cone for raw selection and CSR readback.
# The exported TOPI driver's readback path and the direct raw path are distinct checks.
irq_path m-priority-through-topi-to-csr-readback $m_prio $m_readback [list $m_topi_drivers]
irq_path hs-priority-through-topi-to-csr-readback $hs_prio $hs_readback [list $hs_topi_drivers]
irq_path m-priority-direct-to-raw-cause $m_prio $raw_cause
irq_path hs-priority-direct-to-raw-cause $hs_prio $raw_cause
if {$revision eq "after"} {
    irq_path m-gather-to-valid $live_sources $m_valid [list $m_gather]
    irq_path hs-gather-to-valid $live_sources $hs_valid [list $hs_gather]
    irq_path m-valid-through-topi-to-csr-readback $m_valid $m_readback [list $m_topi_drivers]
    irq_path hs-valid-through-topi-to-csr-readback $hs_valid $hs_readback [list $hs_topi_drivers]
    irq_path m-valid-direct-to-raw-cause $m_valid $raw_cause
    irq_path hs-valid-direct-to-raw-cause $hs_valid $raw_cause
    if {$feature eq "on"} {
        irq_path m-valid-direct-to-raw-valid $m_valid $raw_valid
        irq_path hs-valid-direct-to-raw-valid $hs_valid $raw_valid
        set higher [irq_group higher_priority_drivers [irq_drivers [irq_interface $intr {^io_out_higherPriority$}]]]
        set kill [irq_group hu_kill_drivers [irq_drivers [irq_interface $intr {^io_in_huCandidateKill$}]]]
        set exported_kill [irq_group csr_hu_kill_drivers [irq_drivers [irq_interface $csr {^io_huCandidateKill$}]]]
        if {[lsort $kill] ne [lsort $exported_kill]} {
            lappend failures "filter HU kill is not driven by the exported NewCSR qualification cone"
        }
        foreach {mode gather} [list m $m_gather hs $hs_gather] {
            # A new raw HU candidate already checks higherPending directly. Its
            # duplicate kill test may be optimized away; retained candidates
            # require the complete NewCSR qualification feedback cone.
            irq_path ${mode}-immediate-higher-raw-valid $live_sources $valid(0) [list $gather $higher]
            irq_path ${mode}-immediate-higher-kill-stage5 $live_sources $valid(5) [list $gather $higher $kill]
        }
    }
}

set checks [check_timing -verbose -return_string]
irq_write [file join $output_directory check-timing.rpt] $checks
set counts [dict create]
foreach line [split $checks \n] {
    if {[regexp {^[0-9]+[.][[:space:]]+checking[[:space:]]+([a-z_]+)[[:space:]]+\(([0-9]+)\)} \
            $line match category count]} { dict set counts $category $count }
}
foreach category {loops latch_loops no_clock unconstrained_internal_endpoints} {
    if {![dict exists $counts $category] || [dict get $counts $category] != 0} {
        lappend failures "missing or nonzero check_timing category: $category"
    }
}
set summary "metric\tvalue"
dict for {key value} $metrics { append summary "\n$key\t$value" }
irq_write [file join $output_directory summary.tsv] $summary
irq_write [file join $output_directory state-inventory.tsv] $inventory
irq_write [file join $output_directory paths.tsv] $paths
irq_write [file join $output_directory coverage-failures.txt] [join $failures \n]
if {[llength $failures]} { error "interrupt timing coverage is incomplete: [join $failures {; }]" }
irq_write [file join $output_directory analysis-status.txt] \
    "ANALYSIS_COMPLETE\nFocused unplaced interrupt timing only; negative setup/hold values remain reportable and this is not physical timing PASS."
close_project
