# SPDX-License-Identifier: MulanPSL-2.0

# Measure the real store-address, queue and byte-publication fixture. The 30 ns
# clock and zero I/O delays define a local budget, not integrated arrival times.
if {$argc != 4} {
    error "usage: timing.tcl <RTL-directory> <new-output-directory> <on|off> <before|after>"
}
lassign $argv source output_directory feature snapshot
set source [file normalize $source]
set output_directory [file normalize $output_directory]
if {$feature ni {on off} || $snapshot ni {before after}} { error "invalid feature or snapshot" }
if {![file isfile [file join $source FDIStoreMisalignHarness.sv]]} {
    error "the real store-misalignment fixture RTL is unavailable"
}
if {[file exists $output_directory]} { error "output directory already exists" }
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
proc owned {objects owners} {
    set result [list]
    foreach owner $owners { set result [concat $result [filter $objects "NAME =~ $owner/*"]] }
    return [lsort -unique $result]
}
proc subtract {objects excluded} {
    set result [list]
    foreach object $objects { if {$object ni $excluded} { lappend result $object } }
    return $result
}
proc interface_pins {owners pattern} {
    set result [list]
    foreach owner $owners {
        foreach pin [get_pins -quiet -of_objects [get_cells $owner]] {
            set relative [string range [get_property NAME $pin] [expr {[string length $owner] + 1}] end]
            if {[regexp -- $pattern $relative]} { lappend result $pin }
        }
    }
    return [lsort -unique $result]
}
proc ports_matching {pattern direction} {
    set result [list]
    foreach port [get_ports -quiet -filter "DIRECTION == $direction"] {
        if {[regexp -- $pattern [get_property NAME $port]]} { lappend result $port }
    }
    return $result
}
proc data_pins {cells} {
    if {[llength $cells] == 0} { return [list] }
    return [get_pins -quiet -of_objects $cells -filter {REF_PIN_NAME == D}]
}
proc path_report {name from to {through __NONE__} {endpoint_role COMMON}} {
    global output_directory paths failures
    set info "startpoints=[llength $from]\nendpoints=[llength $to]\nendpoint_role=$endpoint_role"
    write_text [file join $output_directory ${name}-endpoints.txt] [join $to \n]
    set options [list]
    if {$through ne "__NONE__"} {
        append info "\nthrough=[llength $through]"
        write_text [file join $output_directory ${name}-through.txt] [join $through \n]
        if {[llength $through] == 0} {
            lappend failures "$name has no required through point"
            append paths "\n$name\tNA\tNA\tNA\tNA"
            write_text [file join $output_directory ${name}-coverage.txt] "$info\nstatus=MISSING_THROUGH"
            return
        }
        set options [list -through $through]
    }
    if {[llength $from] == 0 || [llength $to] == 0} {
        lappend failures "$name has no startpoints or endpoints"
        append paths "\n$name\tNA\tNA\tNA\tNA"
        write_text [file join $output_directory ${name}-coverage.txt] "$info\nstatus=MISSING_ENDPOINTS"
        return
    }
    set values [list]
    foreach delay {max min} kind {setup hold} {
        set found [get_timing_paths -quiet -from $from -to $to {*}$options -delay_type $delay -max_paths 20]
        if {[llength $found] == 0} {
            lappend failures "$name has no $kind path"
            append paths "\n$name\tNA\tNA\tNA\tNA"
            write_text [file join $output_directory ${name}-coverage.txt] "$info\nstatus=MISSING_${kind}_PATH"
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

set constraints [file join $output_directory local.xdc]
write_text $constraints {
create_clock -name clock -period 30.000 -waveform {0.000 15.000} [get_ports clock]
set_input_delay -clock clock 0.000 [get_ports -filter {DIRECTION == IN && NAME != clock}]
set_output_delay -clock clock 0.000 [all_outputs]
}
set sources [lsort [concat [glob -nocomplain -directory $source *.sv] [glob -nocomplain -directory $source *.v]]]
write_text [file join $output_directory source-files.txt] [join $sources \n]
create_project -in_memory -part xcvu19p-fsva3824-2-e
read_verilog -sv $sources
read_xdc $constraints
synth_design -top FDIStoreMisalignHarness -part xcvu19p-fsva3824-2-e \
    -mode out_of_context -flatten_hierarchy none -verilog_define SYNTHESIS
# Keep the successfully synthesized netlist even when a later coverage check fails.
write_checkpoint [file join $output_directory synth.dcp]
set clocks [get_clocks -quiet]
if {[get_property TOP [current_design]] ne "FDIStoreMisalignHarness" ||
        [get_property PART [current_design]] ne "xcvu19p-fsva3824-2-e" ||
        [llength $clocks] != 1 || [get_property PERIOD $clocks] != 30.000} {
    error "unexpected device, top or clock contract"
}

set registers [all_registers -edge_triggered]
set latches [all_registers -level_sensitive]
set blackboxes [get_cells -quiet -hierarchical -filter {IS_BLACKBOX == 1}]
set failures [list]
set inventory "category\tobject"
set paths "category\tsetup_slack_ns\tdata_delay_ns\tlogic_levels\thold_slack_ns"
set metrics [dict create feature $feature snapshot $snapshot tool_version [version -short] \
    part xcvu19p-fsva3824-2-e period_ns 30.000 input_delay_ns 0.000 output_delay_ns 0.000 \
    estimated_pre_route 1 registers [llength $registers] latches [llength $latches] blackboxes [llength $blackboxes]]
set modules [dict create]
set state [dict create]
foreach {category reference scope} {
        sta0 StoreUnit stores_0 sta1 StoreUnit stores_1
        translation TLBNonBlock translation pmp0 PMPChecker protection_0 pmp1 PMPChecker protection_1
        split StoreMisalignBuffer split queue StoreQueue queue
        sq_data SQDataModule queue/dataModule sq_paddr SQAddrModule queue/paddrModule
        sq_vaddr SQAddrModule queue/vaddrModule publication DatamoduleResultBuffer queue/dataBuffer
        exceptions StoreExceptionBuffer queue/exceptionBuffer
        sbuffer Sbuffer buffer sbuffer_data SbufferData buffer/dataModule
        permission0 FDIPermissionChecker stores_0/* permission1 FDIPermissionChecker stores_1/*} {
    set owners [instances $reference $scope]
    set cells [owned $registers $owners]
    dict set modules $category $owners
    dict set state $category $cells
    dict set metrics ${category}_instances [llength $owners]
    dict set metrics ${category}_registers [llength $cells]
    set expected [expr {[string match permission* $category] ? $feature eq "on" : 1}]
    if {[llength $owners] != $expected || ($expected && [llength $cells] == 0)} {
        lappend failures "expected $expected real $reference owner(s) with retained state at $scope"
    }
    foreach owner $owners { append inventory "\n${category}_module\t$owner" }
    foreach cell $cells { append inventory "\n${category}_register\t$cell" }
}
# STD is a zero-latency MemExeUnit/Std wire path. It may have no retained cells;
# its real input-to-SQ data and acceptance paths are required below instead.
dict set metrics combinational_std_modules [llength [instances MemExeUnit dataUnits*]]
dict set metrics all_pmp_checker_instances [llength [instances PMPChecker *]]
foreach lane {0 1} {
    dict set state sta$lane [subtract [dict get $state sta$lane] [dict get $state permission$lane]]
}
dict set state queue_control [subtract [dict get $state queue] [concat \
    [dict get $state sq_data] [dict get $state sq_paddr] [dict get $state sq_vaddr] \
    [dict get $state publication] [dict get $state exceptions]]]
dict set state sbuffer_control [subtract [dict get $state sbuffer] [dict get $state sbuffer_data]]
foreach {metric pattern} {lut_primitives LUT* ff_primitives FD* carry8_primitives CARRY8} {
    dict set metrics $metric [llength [get_cells -quiet -hierarchical -filter "IS_PRIMITIVE == 1 && REF_NAME =~ $pattern"]]
}
write_text [file join $output_directory state-inventory.tsv] $inventory
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
path_report input-to-output [all_inputs] [all_outputs]
foreach lane {0 1} {
    set sta [dict get $state sta$lane]
    set address_sources [concat [all_inputs] [dict get $state split]]
    set tlb_request [interface_pins [dict get $modules sta$lane] {^io_tlb_req_bits_vaddr(\[[0-9]+\])?$}]
    path_report sta${lane}-address-to-translation $address_sources [data_pins [dict get $state translation]] $tlb_request
    path_report translation-to-sta${lane} [dict get $state translation] [data_pins $sta]
    path_report translation-to-pmp${lane} [dict get $state translation] [data_pins [dict get $state pmp$lane]]
    path_report pmp${lane}-to-sta${lane} [dict get $state pmp$lane] [data_pins $sta]
    path_report sta${lane}-to-split $sta [data_pins [dict get $state split]]
    path_report sta${lane}-to-queue-control $sta [data_pins [dict get $state queue_control]]
    path_report sta${lane}-to-exception $sta [data_pins [dict get $state exceptions]]
    set supplement [interface_pins [dict get $modules queue] \
        [format {^io_storeAddrInRe_%d_(updateAddrValid|hasException)$} $lane]]
    path_report sta${lane}-supplement-to-queue [concat $sta [dict get $state pmp$lane]] \
        [data_pins [dict get $state queue_control]] $supplement
    set std_data [ports_matching [format {^io_std_%d_bits_src_0(\[[0-9]+\])?$} $lane] IN]
    set std_capture [interface_pins [dict get $modules queue] \
        [format {^io_storeDataIn_%d_bits_data(\[[0-9]+\])?$} $lane]]
    path_report std${lane}-data-to-sq $std_data [data_pins [dict get $state sq_data]] $std_capture
    path_report std${lane}-valid-to-sq [get_ports -quiet io_std_${lane}_valid] [data_pins [dict get $state queue_control]]
    if {$feature eq "on"} {
        set permission [dict get $state permission$lane]
        set outcome [interface_pins [dict get $modules permission$lane] {^io_resp_bits_outcome(\[[0-9]+\])?$}]
        path_report sta${lane}-to-permission $sta [data_pins $permission]
        path_report permission${lane}-outcome-to-sta $permission [data_pins $sta] $outcome
        path_report permission${lane}-outcome-to-queue $permission [data_pins [dict get $state queue_control]] $outcome
    }
}
path_report split-to-designated-sta [dict get $state split] [data_pins [dict get $state sta0]]
path_report queue-to-split [dict get $state queue_control] [data_pins [dict get $state split]]
set cross_page [interface_pins [dict get $modules split] {^io_sqControl_toStoreQueue_crossPageCanDeq$}]
path_report split-cross-page-to-publication [dict get $state split] [data_pins [dict get $state publication]] $cross_page
foreach category {queue_control sq_data sq_paddr sq_vaddr} {
    path_report ${category}-to-publication [dict get $state $category] [data_pins [dict get $state publication]]
}
path_report publication-to-sbuffer-control [dict get $state publication] [data_pins [dict get $state sbuffer_control]]
path_report sbuffer-backpressure-to-publication [dict get $state sbuffer_control] [data_pins [dict get $state publication]]
foreach lane {0 1} {
    set data [interface_pins [dict get $modules sbuffer_data] \
        [format {^io_writeReq_%d_bits_data(\[[0-9]+\])?$} $lane]]
    set qualification [interface_pins [dict get $modules sbuffer_data] [format {^io_writeReq_%d_valid$} $lane]]
    path_report publication${lane}-data-to-sbuffer [dict get $state publication] \
        [data_pins [dict get $state sbuffer_data]] $data
    path_report publication${lane}-qualified-write [concat [dict get $state publication] [dict get $state sbuffer_control]] \
        [data_pins [dict get $state sbuffer_data]] $qualification
}
path_report sbuffer-to-cache-request [dict get $state sbuffer] [ports_matching {^io_cacheWrite_req_(valid|bits_.*)$} OUT]
path_report queue-to-uncache-request [dict get $state queue_control] [get_ports -quiet io_uncache_req_valid]

# Keep common paired groups unchanged. New payload captures end at D pins;
# qualification gates explicitly allow the synthesized D/CE/reset controls.
if {$snapshot eq "after"} {
    proc focused_state {name category pattern} {
        global registers modules state metrics inventory failures
        set result [list]
        foreach owner [dict get $modules $category] {
            foreach cell [owned $registers [list $owner]] {
                set relative [string range [get_property NAME $cell] [expr {[string length $owner] + 1}] end]
                if {[regexp -- $pattern $relative]} { lappend result $cell }
            }
        }
        set result [lsort -unique $result]
        dict set metrics ${name}_registers [llength $result]
        foreach cell $result { append inventory "\n$name\t$cell" }
        if {[llength $result] == 0} { lappend failures "missing required after state: $name" }
        return $result
    }
    proc focused_capture {name from cells {through __NONE__}} {
        path_report $name $from [data_pins $cells] $through PAYLOAD_D
    }
    proc focused_control {name from cells {through __NONE__}} {
        set pins [list]
        if {[llength $cells]} {
            set pins [get_pins -quiet -of_objects $cells -filter \
                {REF_PIN_NAME == D || REF_PIN_NAME == CE || REF_PIN_NAME == R || REF_PIN_NAME == S || REF_PIN_NAME == CLR || REF_PIN_NAME == PRE}]
        }
        path_report $name $from $pins $through CONTROL_D_CE_R_S
    }
    set original [focused_state after-original-address split {^req_fullva_reg\[[0-9]+\]$}]
    set high [focused_state after-high-address split {^splitStoreReqs_1_fullva_reg\[[0-9]+\]$}]
    set terminal_address [focused_state after-terminal-address split {^terminalFullva_reg\[[0-9]+\]$}]
    set terminal_format [focused_state after-terminal-format split {^terminalVaNeedExt_reg$}]
    set split_owner [focused_state after-split-owner split {^req_uop_(robIdx_(flag|value)|sqIdx_(flag|value)|uopIdx)_reg(\[[0-9]+\])?$}]
    set split_control [focused_state after-split-control split {^(bufferState|req_valid|curPtr|unSentStores)_reg(\[[0-9]+\])?$}]
    set split_exception [focused_state after-split-exception split {^exceptionVec_[0-9]+_reg$}]
    set original_through [interface_pins [concat [dict get $modules sta0] [dict get $modules sta1]] \
        {^io_misalign_enq_req_bits_fullva\[[0-9]+\]$}]
    focused_capture after-original-address-capture [concat [dict get $state sta0] [dict get $state sta1] \
        [dict get $state translation]] $original $original_through
    focused_capture after-original-to-high-address $original $high
    focused_capture after-original-to-terminal-address $original $terminal_address

    # Only STA0 consumes split requests; STA1 retains the ordinary full-access path.
    foreach stage {1 2} {
        foreach {field pattern} {
                address {fullva_reg\[[0-9]+\]}
                format {vaNeedExt_reg}
                terminal {hasException_reg}
                owner {uop_(robIdx_(flag|value)|sqIdx_(flag|value)|uopIdx)_reg(\[[0-9]+\])?}
                exception {uop_exceptionVec_[0-9]+_reg}} {
            set pipe($stage,$field) [focused_state after-sta0-s${stage}-${field} sta0 \
                [format {^s%d_in_%s$} $stage $pattern]]
        }
        if {$feature eq "on"} {
            set pipe($stage,probe) [focused_state after-sta0-s${stage}-probe sta0 \
                [format {^s%d_in_fdiStoreProbe_reg$} $stage]]
        }
    }
    focused_capture after-high-address-to-sta0 $high $pipe(1,address)
    focused_capture after-terminal-address-to-sta0 $terminal_address $pipe(1,address)
    focused_capture after-terminal-format-to-sta0 $terminal_format $pipe(1,format)
    focused_capture after-terminal-exception-to-sta0 $split_exception $pipe(1,exception)
    focused_capture after-terminal-marker-to-sta0 $split_control $pipe(1,terminal)
    foreach field {address format terminal owner exception} {
        focused_capture after-sta0-s1-to-s2-${field} $pipe(1,$field) $pipe(2,$field)
    }
    focused_capture after-response-address-to-terminal $pipe(2,address) $terminal_address
    focused_capture after-response-format-to-terminal $pipe(2,format) $terminal_format
    focused_capture after-response-exception-to-selection $pipe(2,exception) $split_exception
    focused_control after-response-owner-to-split-state $pipe(2,owner) $split_control
    focused_control after-saved-owner-to-split-state $split_owner $split_control
    focused_control after-terminal-ack-to-split-state $pipe(2,terminal) $split_control
    focused_control after-redirect-to-split-state [ports_matching {^io_redirect_(valid|bits_.*)$} IN] $split_control

    foreach lane {0 1} {
        # Identical full-address banks may merge. Both names refer to the same
        # accepted translation payload; no unrelated TLB state substitutes here.
        set response [focused_state after-tlb${lane}-address translation \
            [format {^(req_out_%d_fullva|io_requestor_%d_resp_bits_fullva_r)_reg\[[0-9]+\]$} $lane $lane]]
        set direct [focused_state after-tlb${lane}-format translation [format {^responseFullvaDirect_%d_reg$} $lane]]
        set prefault [focused_state after-tlb${lane}-prefault translation \
            [format {^io_requestor_%d_resp_bits_excp_0_(pf|af)_st_REG_reg$} $lane]]
        set address [interface_pins [dict get $modules translation] \
            [format {^io_requestor_%d_req_bits_fullva\[[0-9]+\]$} $lane]]
        set kind [interface_pins [dict get $modules translation] \
            [format {^io_requestor_%d_req_bits_fullvaKind\[[0-9]+\]$} $lane]]
        set address_capture [focused_state after-sta${lane}-response-address sta$lane {^s2_in_fullva_reg\[[0-9]+\]$}]
        set format_capture [focused_state after-sta${lane}-response-format sta$lane {^s2_in_vaNeedExt_reg$}]
        set sources [concat [all_inputs] [dict get $state split]]
        focused_capture after-address-to-tlb${lane} $sources $response $address
        focused_capture after-address-to-tlb${lane}-prefault $sources $prefault $address
        focused_capture after-kind-to-tlb${lane}-format $sources $direct $kind
        focused_capture after-tlb${lane}-address-to-sta $response $address_capture
        focused_capture after-tlb${lane}-format-to-sta $direct $format_capture
        if {$lane == 0} {
            # Only the designated split lane can present Effective addresses.
            # Raw versus Scalar changes the other lane's format, not its PMM value.
            focused_capture after-kind-to-tlb${lane}-address $sources $response $kind
            focused_capture after-kind-to-tlb${lane}-prefault $sources $prefault $kind
            set admission [interface_pins [dict get $modules translation] {^io_requestor_0_req_valid$}]
            focused_control after-terminal-state-to-tlb-admission $split_control [concat $response $direct] $admission
        }
    }
    path_report after-terminal-state-to-cache-request $split_control [get_ports -quiet io_metadata_0_req_valid] __NONE__ OUTPUT_CONTROL

    set sq_terminal [focused_state after-sq-terminal-marker queue {^stWbTerminalReg_reg$}]
    set sq_address [focused_state after-sq-terminal-address queue {^terminalVA_reg\[[0-9]+\]$}]
    set sq_exception [focused_state after-sq-terminal-exception queue {^terminalExceptions_[0-9]+_reg$}]
    # Existing scalar-owner registers can be merged with the identical terminal capture.
    set sq_owner [focused_state after-sq-terminal-owner queue \
        {^(terminal(Sq|Rob)_(flag|value)|terminalUop|replenishOwnerLive_(sq_(flag|value)|rob_(flag|value)|uopIdx)|stWbIndexReg)_reg(\[[0-9]+\])?$}]
    set sq_wait [focused_state after-sq-wait queue {^waitStoreS2_[0-9]+_reg$}]
    set sq_fault [focused_state after-sq-fault queue {^hasException_[0-9]+_reg$}]
    set sq_class [focused_state after-sq-memory-classification queue {^(pending|nc|mmio)_[0-9]+_reg$}]
    set sq_pending [focused_state after-sq-mmio-pending queue {^pending_[0-9]+_reg$}]
    set sq_nc [focused_state after-sq-nc-classification queue {^nc_[0-9]+_reg$}]
    set sq_commit [focused_state after-sq-commit queue {^committed_[0-9]+_reg$}]
    # Both emitted feature views name the native RegNext MMIO admission REG_6.
    # It is the first capture; mmioState consumes that result one cycle later.
    set mmio_admission [focused_state after-mmio-admission queue {^REG_6_reg$}]
    set mmio_state [focused_state after-mmio-state queue {^mmioState_reg(\[[0-9]+\])?$}]
    set nc_state [focused_state after-nc-state queue {^(ncState|FSM_sequential_ncState)_reg(\[[0-9]+\])?$}]
    set publication_control [focused_state after-publication-control publication {^(valids_[01]|data_[01]_vecValid)_reg$}]
    foreach {field cells} [list terminal $sq_terminal address $sq_address exception $sq_exception owner $sq_owner] {
        focused_capture after-terminal-${field}-to-sq $pipe(1,$field) $cells
    }
    set supplement [interface_pins [dict get $modules queue] {^io_storeAddrInRe_0_(updateAddrValid|hasException)$}]
    focused_control after-terminal-supplement-to-sq $pipe(2,terminal) [concat $sq_wait $sq_fault $sq_class] $supplement
    focused_control after-sq-owner-match-to-completion [concat $sq_owner $sq_address $sq_exception] [concat $sq_wait $sq_fault $sq_class]
    focused_control after-sq-wait-to-commit $sq_wait $sq_commit
    focused_control after-sq-commit-to-publication $sq_commit $publication_control
    focused_capture after-sq-fault-to-publication $sq_fault $publication_control
    focused_capture after-sq-fault-to-mmio-admission $sq_fault $mmio_admission
    focused_capture after-sq-pending-to-mmio-admission $sq_pending $mmio_admission
    focused_control after-mmio-admission-to-state $mmio_admission $mmio_state
    focused_control after-sq-nc-to-state $sq_nc $nc_state
    path_report after-mmio-state-to-request $mmio_state [get_ports -quiet io_uncache_req_valid] __NONE__ OUTPUT_CONTROL
    path_report after-nc-state-to-request $nc_state [get_ports -quiet io_uncache_req_valid] __NONE__ OUTPUT_CONTROL
    set exception_address [focused_state after-exception-address exceptions {^s2_req_2_fullva_reg\[[0-9]+\]$}]
    set exception_format [focused_state after-exception-format exceptions {^s2_req_2_vaNeedExt_reg$}]
    set exception_valid [focused_state after-exception-qualification exceptions {^s2_valid_REG_2_reg$}]
    focused_capture after-terminal-address-to-exception $pipe(2,address) $exception_address
    focused_capture after-terminal-format-to-exception $pipe(2,format) $exception_format
    focused_capture after-terminal-exception-to-qualification $pipe(2,exception) $exception_valid

    if {$feature eq "on"} {
        set deferred [focused_state after-deferred-owner split {^req_fdiStoreProbe_reg$}]
        set split_probe [focused_state after-split-probe split {^splitStoreReqs_[01]_fdiStoreProbe_reg$}]
        set decision [interface_pins [concat [dict get $modules sta0] [dict get $modules sta1]] \
            {^io_misalign_enq_deferToProbe$}]
        focused_capture after-permission-defer-to-smb [concat [dict get $state sta0] [dict get $state sta1] \
            [dict get $state permission0] [dict get $state permission1]] $deferred $decision
        # Include the saved owner's dedicated physical copy only for this data capture.
        set deferred_capture_sources [focused_state after-deferred-probe-capture-source split {^req_fdiStoreProbe_reg(_rep)?$}]
        focused_capture after-deferred-owner-to-probe $deferred_capture_sources $split_probe
        focused_capture after-probe-to-sta0 $split_probe $pipe(1,probe)
        focused_capture after-probe-s1-to-s2 $pipe(1,probe) $pipe(2,probe)
        set primary [focused_state after-sq-primary-accepted queue {^storeAddrSeenReg_reg$}]
        set primary_valid [interface_pins [dict get $modules queue] {^io_storeAddrIn_0_valid$}]
        focused_capture after-probe-to-primary-suppression $pipe(1,probe) $primary $primary_valid
        focused_control after-probe-to-supplement-suppression $pipe(2,probe) [concat $sq_wait $sq_fault $sq_class] $supplement
        path_report after-probe-to-cache-request $split_probe [get_ports -quiet io_metadata_0_req_valid] __NONE__ OUTPUT_CONTROL
        focused_control after-deferred-owner-to-split-state $deferred $split_control
    }
    write_text [file join $output_directory state-inventory.tsv] $inventory
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
if {[llength $latches] || [llength $blackboxes]} { lappend failures "latches or unresolved blackboxes remain" }
set summary "metric\tvalue"
dict for {metric value} $metrics { append summary "\n$metric\t$value" }
write_text [file join $output_directory summary.tsv] $summary
write_text [file join $output_directory paths.tsv] $paths
write_text [file join $output_directory coverage-failures.txt] [join $failures \n]
if {[llength $failures]} { error "store timing coverage is incomplete: [join $failures {; }]" }
write_text [file join $output_directory analysis-status.txt] \
    "ANALYSIS_COMPLETE\nEstimated pre-placement store timing only. Preserve setup and hold violations; this is not physical timing PASS."
close_project
