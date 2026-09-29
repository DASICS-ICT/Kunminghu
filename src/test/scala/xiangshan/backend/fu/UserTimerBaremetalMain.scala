// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import freechips.rocketchip.diplomacy.DisableMonitors
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, Constantin, FileRegisters}
import xiangshan.{CommitType, DebugOptionsKey, Redirect, XSTileKey}
import xiangshan.backend.fu.NewCSR.UserTimerCSRAddress
import xiangshan.backend.fu.NewCSR.CSREvents.InterruptEventIdentity
import xiangshan.backend.fu.wrapper.CSR

// The production simulation platform owns all execution, memory and peripheral paths.
// Every additional connection is a read-only observation; no probe drives DUT state.
class UserTimerBaremetalSimTop(implicit p: Parameters) extends top.SimTop {
  override def desiredName: String = "SimTop"
  require(l_soc.core_with_l2.size == 1)
  require(debugOpts.EnableDifftest && !debugOpts.FPGAPlatform)

  private val core = l_soc.core_with_l2.head.core
  require(core.coreParams.HasUserTimerInterrupt && core.coreParams.HasVPU && core.coreParams.VLEN == 128)
  private val backend = core.backend.module
  private val ctrl = core.backend.inner.ctrlBlock.module
  private val rob = core.backend.inner.ctrlBlock.rob.module
  private val ftq = core.frontend.inner.module.ftq
  private val ifu = core.frontend.inner.module.ifu
  private val csrExu = core.backend.inner.intExuBlock.get.exus.find(_.exuParams.hasCSR).get.module
  private val csr = csrExu.funcUnits.collectFirst { case unit: CSR => unit }.get
  require(csr.cfg.ckAlwaysEn)
  private val csrMod = csr.csrMod
  private val userTimer = csrMod.userTimer.get

  // The observer advances on the timer's actual clock, including pre-release reset
  // cycles. Each row describes values immediately before that numbered rising edge.
  withClock(observe(userTimer.clock)) {
    val cycle = RegInit(0.U(64.W))
    cycle := cycle + 1.U
    val timerReset = observe(userTimer.reset).asBool
    val mode = observe(csrMod.io.status.privState.PRVM).asUInt
    val virtualMode = observe(csrMod.io.status.privState.V).asUInt
    val debugMode = observe(csrMod.io.status.debugMode)
    val handler = observe(csrMod.io.status.userInHandler.get)
    val entry = observe(csrMod.io.status.userEntryEffect.get)
    val uret = observe(csrMod.io.status.userReturnEffect.get)

    def emit(record: String, fields: (String, Bits)*): Unit = {
      val format = s"UIT05_DUT record=$record cycle=%d" + fields.map(x => s" ${x._1}=0x%x").mkString + "\n"
      printf(Printable.pack(format, (Seq(cycle) ++ fields.map(_._2)): _*))
    }
    // Eager arguments create every probe at module scope. Only the printf is
    // conditional: BoringUtils appends its secret connections at module scope.
    def emitWhen(enable: Bool, record: String, fields: (String, Bits)*): Unit = {
      when(!timerReset && enable) { emit(record, fields: _*) }
    }
    def bank(address: Int): UInt = observe(csrMod.userTimerCSROutMap(address))
    def identity(event: InterruptEventIdentity): Seq[(String, Bits)] = Seq(
      "rob" -> event.robIdx.value, "robflag" -> event.robIdx.flag,
      "ftq" -> event.ftqIdx.value, "ftqflag" -> event.ftqIdx.flag,
      "offset" -> event.ftqOffset, "rvc" -> event.isRVC,
      "cause" -> event.interrupt.cause, "hu" -> event.interrupt.irToHU,
      "interrupt" -> event.interrupt.isInterrupt)

    emit("T", "reset" -> timerReset, "csrreset" -> observe(csrMod.reset).asBool,
      "priv" -> mode, "v" -> virtualMode, "debug" -> debugMode, "handler" -> handler,
      "rem" -> observe(userTimer.io.remaining), "pending" -> observe(userTimer.io.pending),
      "tick" -> observe(userTimer.io.tickEnable), "write" -> observe(userTimer.io.write.valid),
      "wdata" -> observe(userTimer.io.write.bits), "consume" -> observe(userTimer.io.consume),
      "entry" -> entry, "uret" -> uret,
      "ustatus" -> bank(UserTimerCSRAddress.ustatus), "uie" -> bank(UserTimerCSRAddress.uie),
      "utvec" -> bank(UserTimerCSRAddress.utvec), "uepc" -> bank(UserTimerCSRAddress.uepc),
      "ucause" -> bank(UserTimerCSRAddress.ucause), "utval" -> bank(UserTimerCSRAddress.utval),
      "uscratch" -> bank(UserTimerCSRAddress.uscratch), "uip" -> bank(UserTimerCSRAddress.uip))

    locally {
      emitWhen(simMMIO.io.uart.out.valid, "UART", "ch" -> simMMIO.io.uart.out.ch)
      val csrIn = observe(csrMod.io.in.bits)
      emitWhen(observe(csrMod.io.in.valid) && observe(csrMod.io.in.ready),
          "CSR", "addr" -> csrIn.addr, "wen" -> csrIn.wen, "ren" -> csrIn.ren,
          "legalwen" -> observe(csrMod.permitMod.io.out.hasLegalWen), "op" -> csrIn.op,
          "wdata" -> csrIn.wdata, "src" -> csrIn.src, "flush" -> csrIn.redirectFlush,
          "uret" -> csrIn.uret, "mret" -> csrIn.mret, "sret" -> csrIn.sret,
          "rob" -> observe(csr.io.in.bits.ctrl.robIdx.value),
          "robflag" -> observe(csr.io.in.bits.ctrl.robIdx.flag), "priv" -> mode)
      val out = observe(csrMod.io.out.bits)
      emitWhen(observe(csrMod.io.out.valid) && observe(csrMod.io.out.ready),
          "CSR_OUT", "rdata" -> out.rData, "ii" -> out.EX_II, "vi" -> out.EX_VI,
          "target_update" -> out.targetPcUpdate, "target" -> out.targetPc.pc,
          "uret_redirect" -> out.userReturnRedirect, "uret_target" -> out.userReturnTarget.pc,
          "uret_effect" -> uret, "rob" -> observe(csr.io.out.bits.ctrl.robIdx.value),
          "robflag" -> observe(csr.io.out.bits.ctrl.robIdx.flag))

      val delivery = ctrl.io.robio.csr.userTimerDelivery.get
      val accepted = observe(delivery.accepted)
      emitWhen(accepted.valid, "ACCEPT", identity(accepted.bits): _*)
      val head = observe(rob.io.debugRobHead)
      emitWhen(accepted.valid || observe(userTimer.io.pending),
          "HEAD", "rob" -> observe(rob.io.robDeqPtr.value),
          "robflag" -> observe(rob.io.robDeqPtr.flag), "pc" -> head.pc, "instr" -> head.instr,
          "ftq" -> head.ftqPtr.value, "ftqflag" -> head.ftqPtr.flag, "offset" -> head.ftqOffset,
          "valid" -> observe(rob.deqPtrEntryValid), "safe" -> observe(rob.deqPtrEntry.interrupt_safe),
          "sealed" -> observe(rob.deqPtrEntry.huGroupSealed.get),
          "writebacked" -> observe(rob.deqPtrEntry.commit_w))
      val port = csr.huEntry.get
      emitWhen(observe(port.reserve.valid),
          "RESERVE", (identity(observe(port.reserve.bits)) :+ ("ready" -> observe(port.reserve.ready))): _*)
      val controlRequest = observe(delivery.entry.request.bits)
      emitWhen(observe(delivery.entry.request.valid),
          "CONTROL_REQ", (identity(controlRequest.event) ++ Seq("pc" -> controlRequest.pc,
          "ready" -> observe(delivery.entry.request.ready))): _*)
      val csrRequest = observe(port.request.bits)
      emitWhen(observe(port.request.valid),
          "CSR_REQ", (identity(csrRequest.event) ++ Seq("pc" -> csrRequest.pc, "ready" -> observe(port.request.ready))): _*)
      val done = observe(port.completion.bits)
      emitWhen(observe(port.completion.valid),
          "COMPLETE", (identity(done.event) ++ Seq("target" -> done.target.pc,
          "outcome" -> done.outcome, "ready" -> observe(port.completion.ready),
          "iaf" -> done.target.raiseIAF, "ipf" -> done.target.raiseIPF,
          "igpf" -> done.target.raiseIGPF)): _*)
      emitWhen(observe(port.cancel.valid), "CANCEL", identity(observe(port.cancel.bits.event)): _*)
      emitWhen(observe(port.release), "RELEASE")

      // The FTQ write path is independent of the CSR recovery-PC transport. An
      // offline scoreboard can rebuild A1 from the accepted FTQ index and offset.
      emitWhen(observe(ftq.io.toBackend.pc_mem_wen),
          "FTQ_WRITE", "ftq" -> observe(ftq.io.toBackend.pc_mem_waddr),
          "start" -> observe(ftq.io.toBackend.pc_mem_wdata.startAddr))
      backend.io.frontend.toFtq.ftqIdxAhead.zipWithIndex.foreach { case (ahead, slot) =>
        emitWhen(observe(ahead.valid),
            "AHEAD", "slot" -> slot.U, "ftq" -> observe(ahead.bits.value),
            "ftqflag" -> observe(ahead.bits.flag))
      }
      def redirect(record: String, valid: Bool, bits: Redirect): Unit = {
        emitWhen(valid, record, "rob" -> bits.robIdx.value, "robflag" -> bits.robIdx.flag,
            "ftq" -> bits.ftqIdx.value, "ftqflag" -> bits.ftqIdx.flag, "offset" -> bits.ftqOffset,
            "hu" -> bits.isHUTimer.get, "rvc" -> bits.isRVC, "level" -> bits.level,
            "interrupt" -> bits.interrupt, "pc" -> bits.cfiUpdate.pc,
            "target" -> bits.cfiUpdate.target, "fulltarget" -> bits.fullTarget,
            "iaf" -> bits.cfiUpdate.backendIAF, "ipf" -> bits.cfiUpdate.backendIPF,
            "igpf" -> bits.cfiUpdate.backendIGPF)
      }
      redirect("ROB_FLUSH", observe(rob.io.flushOut.valid), observe(rob.io.flushOut.bits))
      redirect("REDIRECT", observe(backend.io.frontend.toFtq.redirect.valid), observe(backend.io.frontend.toFtq.redirect.bits))
      redirect("FTQ_REDIRECT", observe(ftq.io.toIfu.redirect.valid), observe(ftq.io.toIfu.redirect.bits))
      val fetch = observe(ftq.io.toIfu.req.bits)
      emitWhen(observe(ftq.io.toIfu.req.valid) && observe(ftq.io.toIfu.req.ready),
          "FETCH", "ftq" -> fetch.ftqIdx.value, "ftqflag" -> fetch.ftqIdx.flag,
          "start" -> fetch.startAddr, "next" -> fetch.nextStartAddr)
      // Backend instructions have already passed the production RVC expander.
      // Retain the fetched bytes as well so image identity is independently checkable.
      val ifuEnqueueMask = observe(ifu.io.toIbuffer.bits.enqEnable)
      ifu.io.toIbuffer.bits.instrs.indices.foreach { slot =>
        val raw = Mux(observe(ifu.f3_req_is_mmio), observe(ifu.mmioRVCExpander.io.in), observe(ifu.f3_instr(slot)))
        emitWhen(observe(ifu.io.toIbuffer.valid) && observe(ifu.io.toIbuffer.ready) && ifuEnqueueMask(slot),
            "IFU", "slot" -> slot.U, "pc" -> observe(ifu.io.toIbuffer.bits.pc(slot)),
            "raw" -> raw, "instr" -> observe(ifu.io.toIbuffer.bits.instrs(slot)),
            "ftq" -> observe(ifu.io.toIbuffer.bits.ftqPtr.value),
            "ftqflag" -> observe(ifu.io.toIbuffer.bits.ftqPtr.flag),
            "offset" -> observe(ifu.io.toIbuffer.bits.ftqOffset(slot).bits),
            "rvc" -> observe(ifu.io.toIbuffer.bits.pd(slot).isRVC),
            "mmio" -> observe(ifu.f3_req_is_mmio))
      }
      backend.io.frontend.cfVec.zipWithIndex.foreach { case (cf, slot) =>
        val inst = observe(cf.bits)
        emitWhen(observe(cf.valid) && observe(cf.ready),
            "FRONTEND", "slot" -> slot.U, "pc" -> inst.pc, "instr" -> inst.instr,
            "ftq" -> inst.ftqPtr.value, "ftqflag" -> inst.ftqPtr.flag,
            "offset" -> inst.ftqOffset, "rvc" -> inst.pd.isRVC)
      }
      rob.io.enq.req.zipWithIndex.foreach { case (req, slot) =>
        val inst = observe(req.bits)
        emitWhen(observe(req.valid) && observe(rob.io.enq.canAccept),
            "ENQ", "slot" -> slot.U, "rob" -> inst.robIdx.value, "robflag" -> inst.robIdx.flag,
            "ftq" -> inst.ftqPtr.value, "ftqflag" -> inst.ftqPtr.flag, "offset" -> inst.ftqOffset,
            "pc" -> inst.pc, "instr" -> inst.instr, "rvc" -> inst.preDecodeInfo.isRVC,
            "first" -> inst.firstUop, "last" -> inst.lastUop, "uop" -> inst.uopIdx,
            "size" -> inst.instrSize, "fused" -> CommitType.isFused(inst.commitType),
            "ldest" -> inst.ldest, "pdest" -> inst.pdest, "rfwen" -> inst.rfWen,
            "eliminated" -> inst.eliminatedMove)
      }
      rob.io.commits.commitValid.zipWithIndex.foreach { case (valid, slot) =>
        // info.ftqOffset identifies the last instruction of a compressed ROB
        // group; commitDebugUop retains its first instruction and exact PC.
        val info = observe(rob.io.commits.info(slot))
        val inst = observe(rob.commitDebugUop(slot))
        val ptr = observe(rob.io.commits.robIdx(slot))
        emitWhen(observe(rob.io.commits.isCommit) && observe(valid),
            "RETIRE", "slot" -> slot.U, "rob" -> ptr.value, "robflag" -> ptr.flag,
            "ftq" -> info.ftqIdx.value, "ftqflag" -> info.ftqIdx.flag, "offset" -> info.ftqOffset,
            "pc" -> inst.pc, "instr" -> inst.instr, "rvc" -> info.isRVC,
            "size" -> info.instrSize, "fused" -> CommitType.isFused(info.commitType),
            "type" -> info.commitType, "ldest" -> info.debug_ldest.get,
            "pdest" -> info.debug_pdest.get, "rfwen" -> info.rfWen,
            "eliminated" -> inst.eliminatedMove, "data" -> observe(rob.wdata(slot)),
            "priv" -> mode, "handler" -> handler)
      }

      val mEntry = observe(csrMod.trapEntryMEvent.valid)
      val hsEntry = observe(csrMod.trapEntryHSEvent.valid)
      val vsEntry = observe(csrMod.trapEntryVSEvent.valid)
      val mnEntry = observe(csrMod.trapEntryMNEvent.valid)
      val dEntry = observe(csrMod.trapEntryDEvent.valid)
      emitWhen(observe(csrMod.hasTrap) || mEntry || hsEntry || vsEntry || mnEntry || dEntry,
          "TRAP", "ordinary" -> observe(csrMod.hasTrap), "interrupt" -> observe(csrMod.trapIsInterrupt),
          "pc" -> observe(csrMod.trapPC), "vector" -> observe(csrMod.trapVec).asUInt,
          "m" -> mEntry, "hs" -> hsEntry, "vs" -> vsEntry, "mn" -> mnEntry, "debug" -> dEntry,
          "priv" -> mode, "v" -> virtualMode)
      val trapBankUpdated = RegNext(mEntry || hsEntry, false.B)
      emitWhen(mEntry || hsEntry || trapBankUpdated,
          "TRAP_BANK", "mepc" -> observe(csrMod.mepc.rdata), "mcause" -> observe(csrMod.mcause.rdata),
          "mtval" -> observe(csrMod.mtval.rdata), "sepc" -> observe(csrMod.sepc.rdata),
          "scause" -> observe(csrMod.scause.rdata), "priv" -> mode)
    }
  }
}

object UserTimerBaremetalMain extends App {
  val (base, firrtlOpts, firtoolOpts) = top.ArgParser.parse(args)
  val config = base.alterPartial {
    case XSTileKey => base(XSTileKey).map(_.copy(HasUserTimerInterrupt = true))
  }
  require(config(XSTileKey).size == 1)
  val options = config(DebugOptionsKey)
  require(!options.FPGAPlatform && options.EnableDifftest)
  Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
  ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
  top.Generator.execute(firrtlOpts, DisableMonitors(p => new UserTimerBaremetalSimTop()(p))(config), firtoolOpts)
  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write(fileDir = "./build")
}
