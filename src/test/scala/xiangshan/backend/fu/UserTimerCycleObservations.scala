// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import freechips.rocketchip.rocket.CSRs
import xiangshan.backend.fu.NewCSR.UserTimerCSRAddress
import xiangshan.backend.fu.NewCSR.CSREvents.{InterruptDescriptor, InterruptEventIdentity}
import xiangshan.backend.fu.wrapper.CSR

/** Read-only pre-edge sources for the independent cycle checker; no DUT inputs are driven. */
object UserTimerCycleObservations {
  def attach(sim: top.SimTop): Unit = {
    val core = sim.l_soc.core_with_l2.head.core
    val rob = core.backend.inner.ctrlBlock.rob.module
    val csrExu = core.backend.inner.intExuBlock.get.exus.find(_.exuParams.hasCSR).get.module
    val csr = csrExu.funcUnits.collectFirst { case unit: CSR => unit }.get
    val bank = csr.csrMod
    val timer = bank.userTimer.get
    val filter = bank.intrMod
    val memory = core.memBlock.inner.module
    require(core.coreParams.HasFDI && csr.cfg.ckAlwaysEn)

    // This counter has the same clock, reset and initial value as the retained raw logger
    // and reference observer. A row samples the values consumed at its numbered edge.
    val timerClock = observe(timer.clock)
    withClock(timerClock) {
      val cycle = RegInit(0.U(64.W))
      cycle := cycle + 1.U
      def emit(record: String, fields: (String, Bits)*): Unit = {
        val format = s"UIT07_DUT record=$record cycle=%d" + fields.map(x => s" ${x._1}=0x%x").mkString + "\n"
        printf(Printable.pack(format, (Seq(cycle) ++ fields.map(_._2)): _*))
      }
      // Eager arguments place every bore at module scope, including conditional records.
      def emitWhen(enable: Bool, record: String, fields: (String, Bits)*): Unit = {
        when(enable) { emit(record, fields: _*) }
      }
      val timerReset = observe(timer.reset).asBool
      val redirect = observe(rob.io.redirect)
      val csrFlush = observe(csr.io.flush)
      val input = observe(bank.io.in.bits)
      // Bit order is part of the observation format, independent of map iteration order.
      val userWrites = UserTimerCSRAddress.all.map(a => observe(bank.userTimerCSRMap(a)._1.wen))
      val qualificationAddresses = Seq(UserTimerCSRAddress.utimer, UserTimerCSRAddress.ustatus,
        UserTimerCSRAddress.uie, CSRs.mstateen0, CSRs.sstateen0, CSRs.dcsr, CSRs.mnstatus)
      val qualificationWrites = qualificationAddresses.map(a => observe(bank.csrRwMap(a)._1.wen))
      emit("SOURCE", "reset" -> timerReset, "rob_size" -> rob.RobSize.U,
        "mstateen0" -> observe(bank.mstateen0.rdata),
        "sstateen0" -> observe(bank.sstateen0.rdata),
        "sstateenc" -> observe(bank.sstateen0.rdataFields.C).asBool,
        "step" -> observe(bank.dcsr.regOut.STEP).asBool,
        "stepie" -> observe(bank.dcsr.regOut.STEPIE).asBool,
        "nmie" -> observe(bank.mnstatus.regOut.NMIE).asBool,
        "satp" -> observe(bank.satp.rdata),
        "user_write_mask" -> VecInit(userWrites).asUInt, "user_write_data" -> input.wdata,
        "qualification_c1" -> VecInit(qualificationWrites).asUInt,
        "rob_redirect_valid" -> redirect.valid,
        "rob_redirect_rob" -> redirect.bits.robIdx.value,
        "rob_redirect_robflag" -> redirect.bits.robIdx.flag,
        "rob_redirect_level" -> redirect.bits.level,
        "csr_flush" -> csrFlush.valid, "csr_flush_rob" -> csrFlush.bits.robIdx.value,
        "csr_flush_robflag" -> csrFlush.bits.robIdx.flag, "csr_flush_level" -> csrFlush.bits.level,
        "csr_redirect_flush" -> input.redirectFlush,
        "csr_in_valid" -> observe(bank.io.in.valid), "csr_in_ready" -> observe(bank.io.in.ready),
        "permit_ii" -> observe(bank.permitMod.io.out.EX_II),
        "permit_vi" -> observe(bank.permitMod.io.out.EX_VI),
        "has_trap" -> observe(bank.hasTrap),
        "debug_entry" -> observe(bank.trapEntryDEvent.valid),
        "mn_entry" -> observe(bank.trapEntryMNEvent.valid),
        "double_trap_mn" -> observe(bank.dbltrpToMN))

      // Bit i belongs to the real filter register i (0 is nearest the source).
      // Causes and IIDs use 8-bit and 12-bit little-indexed lanes respectively.
      val stages = observe(filter.candidateStages.get)
      def stageMask(select: Int => Bool): UInt = VecInit(stages.indices.map(select)).asUInt
      // Descriptor format: cause[7:0], debug, critical, NMI, virtual, HS, VS, HU,
      // interrupt[15], then IID[27:16]. Do not depend on Bundle packing order.
      def descriptor(event: InterruptDescriptor): UInt = Cat(event.hvictlIID, event.isInterrupt,
        event.irToHU, event.irToVS, event.irToHS, event.virtualInterruptIsHvictlInject,
        event.nmi, event.criticalDebug, event.debug, event.cause)
      val selected = observe(filter.io.out.candidate.get)
      emit("PIPE", "valid" -> stageMask(i => stages(i).valid),
        "hu" -> stageMask(i => stages(i).bits.irToHU),
        "debug" -> stageMask(i => stages(i).bits.debug),
        "critical" -> stageMask(i => stages(i).bits.criticalDebug),
        "nmi" -> stageMask(i => stages(i).bits.nmi),
        "virtual" -> stageMask(i => stages(i).bits.virtualInterruptIsHvictlInject),
        "hs" -> stageMask(i => stages(i).bits.irToHS),
        "vs" -> stageMask(i => stages(i).bits.irToVS),
        "interrupt" -> stageMask(i => stages(i).bits.isInterrupt),
        "causes" -> Cat(stages.reverse.map(_.bits.cause)),
        "iids" -> Cat(stages.reverse.map(_.bits.hvictlIID)),
        "hu_candidate" -> observe(filter.io.in.huCandidate.get),
        "hu_kill" -> observe(filter.io.in.huCandidateKill.get),
        "hu_claim" -> observe(filter.io.in.huClaim.get),
        "higher" -> observe(filter.io.out.higherPriority.get),
        "critical_inflight" -> observe(filter.io.in.criticalDebugInFlight.get),
        "critical_claim" -> observe(filter.io.in.criticalDebugClaim.get),
        "nmi_inflight_valid" -> observe(filter.io.in.nmiInFlight.get.valid),
        "nmi_inflight_cause" -> observe(filter.io.in.nmiInFlight.get.bits),
        "nmi_claim_valid" -> observe(filter.io.in.nmiClaim.get.valid),
        "nmi_claim_cause" -> observe(filter.io.in.nmiClaim.get.bits),
        "selected_valid" -> selected.valid, "selected_hu" -> selected.bits.irToHU,
        "selected_cause" -> selected.bits.cause, "selected_descriptor" -> descriptor(selected.bits),
        "mip" -> observe(filter.io.in.mip).asUInt, "mie" -> observe(filter.io.in.mie).asUInt,
        "mideleg" -> observe(filter.io.in.mideleg).asUInt,
        "sip" -> observe(filter.io.in.sip).asUInt, "sie" -> observe(filter.io.in.sie).asUInt,
        "hip" -> observe(filter.io.in.hip).asUInt, "hie" -> observe(filter.io.in.hie).asUInt,
        "hideleg" -> observe(filter.io.in.hideleg).asUInt,
        "vsip" -> observe(filter.io.in.vsip).asUInt, "vsie" -> observe(filter.io.in.vsie).asUInt,
        "hvictl" -> observe(filter.io.in.hvictl).asUInt,
        "d_intr" -> observe(filter.io.in.debugIntr),
        "critical_debug" -> observe(filter.io.in.criticalDebug.get),
        "nmi_input" -> observe(filter.io.in.nmi), "nmi_vec" -> observe(filter.io.in.nmiVec),
        "mstatus_mie" -> observe(filter.io.in.mstatusMIE),
        "sstatus_sie" -> observe(filter.io.in.sstatusSIE),
        "vsstatus_sie" -> observe(filter.io.in.vsstatusSIE))

      val head = observe(rob.io.debugRobHead)
      val headInfo = observe(rob.deqPtrEntry)
      val robCandidate = observe(rob.interruptDescriptorReg.get)
      val robDelivery = rob.io.csr.userTimerDelivery.get
      val incoming = observe(robDelivery.candidate)
      emit("ROB_GATE", "rob" -> observe(rob.io.robDeqPtr.value),
        "robflag" -> observe(rob.io.robDeqPtr.flag), "pc" -> head.pc, "instr" -> head.instr,
        "type" -> headInfo.commitType, "mmio" -> headInfo.mmio, "vls" -> headInfo.isVls,
        "valid" -> headInfo.commit_v, "sealed" -> headInfo.huGroupSealed.get,
        "writebacked" -> headInfo.commit_w, "safe" -> headInfo.interrupt_safe,
        "candidate_valid" -> observe(rob.intrBitSetReg), "candidate_hu" -> robCandidate.irToHU,
        "descriptor" -> descriptor(robCandidate),
        "candidate_cause" -> robCandidate.cause, "selected_hu" -> observe(rob.selectedHU),
        "incoming_valid" -> incoming.valid,
        "incoming_hu" -> incoming.bits.irToHU,
        "incoming_descriptor" -> descriptor(incoming.bits),
        "candidate_kill" -> observe(robDelivery.candidateKill),
        "can_accept" -> observe(rob.io.huCanAccept.get), "idle" -> (observe(rob.state) === 0.U),
        "wait_forward" -> observe(rob.hasWaitForward), "flushed" -> observe(rob.deqHasFlushed),
        "need_flush" -> headInfo.needFlush, "exception" -> observe(rob.deqHasException),
        "flush_pipe" -> observe(rob.isFlushPipe), "last_flush" -> observe(rob.lastCycleFlush),
        "mispred_block" -> observe(rob.misPredBlock), "wfi" -> observe(rob.hasWFI),
        "deq_flush_block" -> observe(rob.deqFlushBlock), "critical" -> observe(rob.criticalErrorState),
        "trace_block" -> observe(rob.traceBlock), "accept_blocked" -> observe(rob.huAcceptanceBlocked),
        "interrupt_claim" -> observe(rob.interruptClaim), "accepted" -> observe(rob.huAccepted),
        "pendingld" -> observe(rob.io.lsq.pendingld), "pendingst" -> observe(rob.io.lsq.pendingst))

      // The bank port includes cancellation injected by the wrapper after its delayed
      // redirect. The retained raw CANCEL row describes the controller-side port.
      val entry = bank.io.huEntry.get
      val bankCancel = observe(entry.cancel)
      emit("DELIVERY", "reserve_valid" -> observe(entry.reserve.valid), "reserve_ready" -> observe(entry.reserve.ready),
        "request_valid" -> observe(entry.request.valid), "request_ready" -> observe(entry.request.ready),
        "complete_valid" -> observe(entry.completion.valid), "complete_ready" -> observe(entry.completion.ready),
        "cancel_valid" -> bankCancel.valid, "release" -> observe(entry.release),
        "effect_locked" -> observe(entry.effectLocked), "canceled" -> observe(entry.canceled))
      def identity(event: InterruptEventIdentity): Seq[(String, Bits)] = Seq(
        "rob" -> event.robIdx.value, "robflag" -> event.robIdx.flag,
        "ftq" -> event.ftqIdx.value, "ftqflag" -> event.ftqIdx.flag,
        "offset" -> event.ftqOffset, "rvc" -> event.isRVC,
        "cause" -> event.interrupt.cause, "hu" -> event.interrupt.irToHU,
        "interrupt" -> event.interrupt.isInterrupt)
      emitWhen(!timerReset && bankCancel.valid, "BANK_CANCEL", identity(bankCancel.bits.event): _*)
      emitWhen(!timerReset && redirect.valid, "ROB_REDIRECT",
        "rob" -> redirect.bits.robIdx.value, "robflag" -> redirect.bits.robIdx.flag,
        "level" -> redirect.bits.level, "pc" -> redirect.bits.cfiUpdate.pc,
        "target" -> redirect.bits.cfiUpdate.target, "mispred" -> redirect.bits.cfiUpdate.isMisPred,
        "hu" -> redirect.bits.isHUTimer.get, "interrupt" -> redirect.bits.interrupt)
      val trapUpdate = observe(bank.trapEntryMEvent.valid) || observe(bank.trapEntryHSEvent.valid)
      val afterTrap = RegNext(trapUpdate, false.B)
      emitWhen(!timerReset && (trapUpdate || afterTrap), "TRAP_STATE",
        "effect" -> trapUpdate, "after" -> afterTrap,
        "mepc" -> observe(bank.mepc.rdata), "mcause" -> observe(bank.mcause.rdata),
        "mtval" -> observe(bank.mtval.rdata), "sepc" -> observe(bank.sepc.rdata),
        "scause" -> observe(bank.scause.rdata), "stval" -> observe(bank.stval.rdata))

      // A ROB stall does not establish memory execution. These handshakes bound actual
      // scalar load and atomic ownership; replay and flush must retain the same ROB identity.
      val memoryRedirect = observe(memory.redirect)
      emitWhen(!timerReset && memoryRedirect.valid, "MEM_REDIRECT",
        "rob" -> memoryRedirect.bits.robIdx.value, "robflag" -> memoryRedirect.bits.robIdx.flag,
        "level" -> memoryRedirect.bits.level)
      val atomic = memory.atomicsUnit
      val atomicInput = observe(atomic.io.in.bits)
      val atomicOutput = observe(atomic.io.out.bits)
      emitWhen(!timerReset && observe(atomic.io.in.valid), "AMO_IN",
        "ready" -> observe(atomic.io.in.ready), "rob" -> atomicInput.uop.robIdx.value,
        "robflag" -> atomicInput.uop.robIdx.flag, "pc" -> atomicInput.uop.pc,
        "op" -> atomicInput.uop.fuOpType, "uop" -> atomicInput.uop.uopIdx)
      emitWhen(!timerReset && observe(atomic.io.out.valid), "AMO_OUT",
        "ready" -> observe(atomic.io.out.ready), "rob" -> atomicOutput.uop.robIdx.value,
        "robflag" -> atomicOutput.uop.robIdx.flag, "pc" -> atomicOutput.uop.pc,
        "data" -> atomicOutput.data, "exception" -> atomicOutput.uop.exceptionVec.asUInt)
      memory.loadUnits.zipWithIndex.foreach { case (load, lane) =>
        val loadInput = observe(load.io.ldin.bits)
        val loadOutput = observe(load.io.ldout.bits)
        emitWhen(!timerReset && observe(load.io.ldin.valid), "LOAD_IN",
          "lane" -> lane.U, "ready" -> observe(load.io.ldin.ready),
          "rob" -> loadInput.uop.robIdx.value, "robflag" -> loadInput.uop.robIdx.flag,
          "pc" -> loadInput.uop.pc, "op" -> loadInput.uop.fuOpType,
          "uop" -> loadInput.uop.uopIdx)
        emitWhen(!timerReset && observe(load.io.ldout.valid), "LOAD_OUT",
          "lane" -> lane.U, "ready" -> observe(load.io.ldout.ready),
          "rob" -> loadOutput.uop.robIdx.value, "robflag" -> loadOutput.uop.robIdx.flag,
          "pc" -> loadOutput.uop.pc, "data" -> loadOutput.data,
          "exception" -> loadOutput.uop.exceptionVec.asUInt)
      }
    }
  }
}
