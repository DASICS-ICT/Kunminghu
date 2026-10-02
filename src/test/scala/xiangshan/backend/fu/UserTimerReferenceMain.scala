// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import difftest._
import freechips.rocketchip.diplomacy.DisableMonitors
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, Constantin, FileRegisters, SignExt, ZeroExt}
import xiangshan.{CommitType, DebugOptionsKey, XSTileKey}
import xiangshan.backend.fu.NewCSR.UserTimerCSRAddress
import xiangshan.backend.fu.wrapper.CSR

/** Both feature settings observe the same production execution and retirement interfaces. */
object UserTimerReferenceObservations {
 def attach(sim: top.SimTop): Unit = {
  val core = sim.l_soc.core_with_l2.head.core
  val rob = core.backend.inner.ctrlBlock.rob.module
  val csrExu = core.backend.inner.intExuBlock.get.exus.find(_.exuParams.hasCSR).get.module
  val csr = csrExu.funcUnits.collectFirst { case unit: CSR => unit }.get
  val bank = csr.csrMod
  val enabled = core.coreParams.HasFDI
  require(csr.cfg.ckAlwaysEn)

  // Every bore is created at module scope, outside conditionals and DUT reset control.
  val coreClock = observe(bank.clock)
  val coreReset = observe(bank.reset).asBool
  withClock(coreClock) {
    val observer = Module(new UserTimerReferenceObserver()(bank.p))
    val in = observer.io
    in.enabled := enabled.B
    in.coreReset := coreReset
    in.flush := observe(rob.io.redirect)
    val canAccept = observe(rob.io.enq.canAccept)
    val redirectValid = observe(rob.io.redirect.valid)
    rob.io.enq.req.indices.foreach { lane =>
      val source = observe(rob.io.enq.req(lane))
      val allocated = observe(rob.io.enq.resp(lane))
      in.allocate(lane).valid := source.valid && source.bits.firstUop && canAccept && !redirectValid
      in.allocate(lane).bits.ptr := allocated
      in.allocate(lane).bits.pc := ZeroExt(source.bits.pc, 64)
      in.allocate(lane).bits.instr := source.bits.instr
      if (!enabled) {
        when(source.valid && canAccept && !coreReset) {
          printf(p"UIT05_DUT record=ENQ cycle=${in.status.cycle} slot=0x${Hexadecimal(lane.U)} rob=0x${Hexadecimal(source.bits.robIdx.value)} robflag=0x${Hexadecimal(source.bits.robIdx.flag)} pc=0x${Hexadecimal(source.bits.pc)} instr=0x${Hexadecimal(source.bits.instr)} first=0x${Hexadecimal(source.bits.firstUop)}\n")
        }
      }
      when(in.allocate(lane).valid && !coreReset) {
        assert(allocated === source.bits.robIdx, "UIT06 ROB allocation disagrees with input identity")
        printf(p"UIT06_DUT record=ALLOC cycle=${in.status.cycle} slot=0x${Hexadecimal(lane.U)} rob=0x${Hexadecimal(allocated.value)} robflag=0x${Hexadecimal(allocated.flag)} pc=0x${Hexadecimal(source.bits.pc)} instr=0x${Hexadecimal(source.bits.instr)}\n")
      }
    }
    val committing = observe(rob.io.commits.isCommit)
    rob.io.commits.commitValid.indices.foreach { lane =>
      val valid = observe(rob.io.commits.commitValid(lane))
      val info = observe(rob.io.commits.info(lane))
      in.retire(lane).valid := committing && valid
      in.retire(lane).bits.ptr := observe(rob.io.commits.robIdx(lane))
      in.retire(lane).bits.count := info.instrSize +& CommitType.isFused(info.commitType).asUInt
      in.retire(lane).bits.isTerminal := observe(rob.commitDebugUop(lane).isXSTrap)
    }
    val csrInput = observe(bank.io.in.bits)
    val csrInputValid = observe(bank.io.in.valid)
    val csrReady = observe(bank.io.in.ready)
    val csrControl = observe(csr.io.in.bits.ctrl)
    val csrOutput = observe(bank.io.out.bits)
    val csrOutputValid = observe(bank.io.out.valid)
    val csrOutputReady = observe(bank.io.out.ready)
    val csrOutputControl = observe(csr.io.out.bits.ctrl)
    val permitII = observe(bank.permitMod.io.out.EX_II)
    val permitVI = observe(bank.permitMod.io.out.EX_VI)
    val userAddress = UserTimerCSRAddress.all.map(a => csrInput.addr === a.U).reduce(_ || _)
    if (!enabled) {
      when(csrInputValid && csrReady && !coreReset) {
        printf(p"UIT05_DUT record=CSR cycle=${in.status.cycle} addr=0x${Hexadecimal(csrInput.addr)} rob=0x${Hexadecimal(csrControl.robIdx.value)} robflag=0x${Hexadecimal(csrControl.robIdx.flag)} ren=0x${Hexadecimal(csrInput.ren)} wen=0x${Hexadecimal(csrInput.wen)} uret=0x${Hexadecimal(csrInput.uret)} flush=0x${Hexadecimal(csrInput.redirectFlush)}\n")
      }
    }
    in.request.valid := csrInputValid && csrReady && !csrInput.redirectFlush &&
      ((userAddress && (csrInput.ren || csrInput.wen)) || csrInput.uret)
    in.request.bits.ptr := csrControl.robIdx
    in.request.bits.csr := csrInput.addr
    in.request.bits.funct3 := csrControl.fuOpType(2, 0)
    in.request.bits.read := csrInput.ren
    in.request.bits.write := csrInput.wen
    in.request.bits.legal := enabled.B && !permitII && !permitVI
    in.request.bits.isUret := csrInput.uret
    in.request.bits.oldValue := csrOutput.regOut
    in.request.bits.pending := bank.userTimer.map(t => observe(t.io.pending)).getOrElse(false.B)
    in.response.valid := csrOutputValid && csrOutputReady
    in.response.bits.ptr := csrOutputControl.robIdx
    in.response.bits.rdata := csrOutput.rData
    in.response.bits.illegal := csrOutput.EX_II
    in.response.bits.virtualFault := csrOutput.EX_VI
    in.response.bits.returnEffect := bank.io.status.userReturnEffect.map(observe(_)).getOrElse(false.B)
    in.response.bits.target := csrOutput.userReturnTarget.pc
    in.softwareReady := csrReady
    val ordinaryTrap = observe(bank.hasTrap)
    val debugEntry = observe(bank.trapEntryDEvent.valid)
    in.returnCancel := ordinaryTrap || debugEntry
    val writes = UserTimerCSRAddress.all.map(a => bank.userTimerCSRMap.get(a).map(pair => observe(pair._1.wen)).getOrElse(false.B))
    in.writeValid := writes.reduce(_ || _)
    in.writeAddress := Mux1H(writes, UserTimerCSRAddress.all.map(_.U(12.W)))
    in.writeData := csrInput.wdata
    when(!coreReset) { assert(PopCount(writes) <= 1.U, "UIT06 multiple user CSR write owners") }

    val exceptionBoundary = observe(rob.exceptionHappen)
    val headPtr = observe(rob.io.robDeqPtr)
    val isInterrupt = observe(rob.intrEnable)
    val huAccepted = observe(rob.huAccepted)
    in.boundary.valid := exceptionBoundary
    in.boundary.bits.ptr := headPtr
    in.boundary.bits.isInterrupt := isInterrupt
    in.boundary.bits.isHU := huAccepted
    val huEffect = bank.io.status.userEntryEffect.map(observe(_)).getOrElse(false.B)
    in.architecturalTrap := ordinaryTrap || huEffect
    in.huEffect := huEffect
    in.huRelease := csr.huEntry.map(port => observe(port.release)).getOrElse(false.B)
    val huRequestPC = csr.huEntry.map(port => observe(port.request.bits.pc)).getOrElse(0.U(64.W))
    val huRequestFire = csr.huEntry.map(port => observe(port.request.valid) && observe(port.request.ready)).getOrElse(false.B)
    val huPC = RegEnable(huRequestPC, huRequestFire)
    in.trapPC := Mux(huEffect, huPC, observe(bank.trapPC))
    in.trapTarget := csrOutput.targetPc.pc

    def userCSR(address: Int): UInt = bank.userTimerCSROutMap.get(address).map(observe(_)).getOrElse(0.U(64.W))
    in.bank.ustatus := userCSR(UserTimerCSRAddress.ustatus)
    in.bank.uie := userCSR(UserTimerCSRAddress.uie)
    in.bank.utvec := userCSR(UserTimerCSRAddress.utvec)
    in.bank.uscratch := userCSR(UserTimerCSRAddress.uscratch)
    in.bank.uepc := userCSR(UserTimerCSRAddress.uepc)
    in.bank.ucause := userCSR(UserTimerCSRAddress.ucause)
    in.bank.utval := userCSR(UserTimerCSRAddress.utval)
    in.bank.inHandler := bank.io.status.userInHandler.map(observe(_)).getOrElse(false.B)
    in.bank.mstateen0 := observe(bank.mstateen0.rdata)
    in.bank.sstateen0 := observe(bank.sstateen0.rdata)
    in.bank.privilegeMode := observe(bank.io.status.privState.PRVM).asUInt
    in.bank.virtMode := observe(bank.io.status.privState.V).asUInt
    in.bank.debugMode := observe(bank.io.status.debugMode)

    in.csrState.privilegeMode := in.bank.privilegeMode
    in.csrState.mstatus := observe(bank.mstatus.rdata).asUInt
    in.csrState.sstatus := observe(bank.mstatus.sstatus).asUInt
    in.csrState.mepc := observe(bank.mepc.rdata).asUInt
    in.csrState.sepc := observe(bank.sepc.rdata).asUInt
    in.csrState.mtval := observe(bank.mtval.rdata).asUInt
    in.csrState.stval := observe(bank.stval.rdata).asUInt
    in.csrState.mtvec := observe(bank.mtvec.rdata).asUInt
    in.csrState.stvec := observe(bank.stvec.rdata).asUInt
    in.csrState.mcause := observe(bank.mcause.rdata).asUInt
    in.csrState.scause := observe(bank.scause.rdata).asUInt
    in.csrState.satp := observe(bank.satp.rdata).asUInt
    in.csrState.mip := observe(bank.mip.rdata).asUInt
    in.csrState.mie := observe(bank.mie.rdata).asUInt
    in.csrState.mscratch := observe(bank.mscratch.rdata).asUInt
    in.csrState.sscratch := observe(bank.sscratch.rdata).asUInt
    in.csrState.mideleg := observe(bank.mideleg.rdata).asUInt
    in.csrState.medeleg := observe(bank.medeleg.rdata).asUInt
    in.hcsrState.virtMode := in.bank.virtMode
    in.hcsrState.mtval2 := observe(bank.mtval2.rdata).asUInt
    in.hcsrState.mtinst := observe(bank.mtinst.rdata).asUInt
    in.hcsrState.hstatus := observe(bank.hstatus.rdata).asUInt
    in.hcsrState.hideleg := observe(bank.hideleg.rdata).asUInt
    in.hcsrState.hedeleg := observe(bank.hedeleg.rdata).asUInt
    in.hcsrState.hcounteren := observe(bank.hcounteren.rdata).asUInt
    in.hcsrState.htval := observe(bank.htval.rdata).asUInt
    in.hcsrState.htinst := observe(bank.htinst.rdata).asUInt
    in.hcsrState.hgatp := observe(bank.hgatp.rdata).asUInt
    in.hcsrState.vsstatus := observe(bank.vsstatus.rdata).asUInt
    in.hcsrState.vstvec := observe(bank.vstvec.rdata).asUInt
    in.hcsrState.vsepc := observe(bank.vsepc.rdata).asUInt
    in.hcsrState.vscause := observe(bank.vscause.rdata).asUInt
    in.hcsrState.vstval := observe(bank.vstval.rdata).asUInt
    in.hcsrState.vsatp := observe(bank.vsatp.rdata).asUInt
    in.hcsrState.vsscratch := observe(bank.vsscratch.rdata).asUInt

    val hartId = observe(rob.io.hartId)
    def connectPayload(target: Bundle, source: Bundle): Unit = {
      source.elements.foreach { case (name, data) => target.elements(name) := data }
    }
    val access = DifftestModule(new DiffUserTimerAccess, delay = 0)
    access.coreid := hartId
    connectPayload(access, in.access)
    val response = DifftestModule(new DiffUserTimerResponse, delay = 0)
    response.coreid := hartId
    connectPayload(response, in.completed)
    val snapshot = DifftestModule(new DiffUserTimerSnapshot, delay = 0)
    snapshot.coreid := hartId
    connectPayload(snapshot, in.snapshot)
    in.canceled.indices.foreach { slot =>
      val canceled = DifftestModule(new DiffUserTimerCancel, delay = 0)
      canceled.coreid := hartId
      canceled.index := slot.U
      connectPayload(canceled, in.canceled(slot))
    }
    val status = DifftestModule(new DiffUserTimerClock, delay = 0)
    status.coreid := hartId
    connectPayload(status, in.status)
    val terminal = DifftestModule(new DiffUserTimerTerminal, delay = 0)
    terminal.coreid := hartId
    connectPayload(terminal, in.terminal)
    when(in.terminal.valid) {
      printf(p"UIT06_DUT record=TERMINAL cycle=${in.terminal.cycle} epoch=0x${Hexadecimal(in.terminal.epoch)} uid=0x${Hexadecimal(in.terminal.uid)} after_count=0x${Hexadecimal(in.terminal.afterCount)} pc=0x${Hexadecimal(in.terminal.pc)}\n")
    }
    in.committed.indices.foreach { lane =>
      val committed = DifftestModule(new DiffUserTimerRetire, delay = 3)
      committed.coreid := hartId
      committed.index := lane.U
      connectPayload(committed, in.committed(lane))
    }
    val uartModule = sim.l_simMMIO.uart.module
    val uart = DifftestModule(new DiffUserTimerUart, delay = 0)
    uart.coreid := hartId
    uart.epoch := in.status.epoch
    uart.cycle := in.status.cycle
    uart.valid := observe(uartModule.io.extra.get.out.valid) && !coreReset
    uart.address := observe(uartModule.waddr)
    val uartData = observe(uartModule.in.w.bits.data)
    val uartMask = observe(uartModule.in.w.bits.strb)
    uart.data := uartData >> (uart.address(2, 0) << 3)
    uart.length := PopCount(uartMask)
  }
 }
}

/** Retains the independent bare-metal logger while adding typed reference observations. */
class UserTimerReferenceSimTop(implicit val referenceParameters: Parameters)
  extends UserTimerBaremetalSimTop()(referenceParameters) {
  override def desiredName: String = "SimTop"
  override protected def addDifftestObservations(): Unit = UserTimerReferenceObservations.attach(this)
}

/** Disabled reference candidates retain the real core plus raw retirement/UART evidence. */
class UserTimerReferenceDisabledSimTop(implicit val referenceParameters: Parameters)
  extends top.SimTop()(referenceParameters) {
  override def desiredName: String = "SimTop"
  override protected def addDifftestObservations(): Unit = UserTimerReferenceObservations.attach(this)
  private val core = l_soc.core_with_l2.head.core
  private val rob = core.backend.inner.ctrlBlock.rob.module
  private val coreClock = observe(rob.clock)
  private val coreReset = observe(rob.reset).asBool
  withClock(coreClock) {
    val cycle = RegInit(0.U(64.W))
    cycle := cycle + 1.U
    val uartValid = simMMIO.io.uart.out.valid
    val uartByte = simMMIO.io.uart.out.ch
    when(uartValid && !coreReset) {
      printf(p"UIT05_DUT record=UART cycle=${cycle} ch=0x${Hexadecimal(uartByte)}\n")
    }
    val committing = observe(rob.io.commits.isCommit)
    rob.io.commits.commitValid.indices.foreach { lane =>
      val valid = observe(rob.io.commits.commitValid(lane))
      val info = observe(rob.io.commits.info(lane))
      val inst = observe(rob.commitDebugUop(lane))
      val ptr = observe(rob.io.commits.robIdx(lane))
      when(committing && valid && !coreReset) {
        printf(p"UIT05_DUT record=RETIRE cycle=${cycle} slot=0x${Hexadecimal(lane.U)} rob=0x${Hexadecimal(ptr.value)} robflag=0x${Hexadecimal(ptr.flag)} pc=0x${Hexadecimal(inst.pc)} instr=0x${Hexadecimal(inst.instr)} size=0x${Hexadecimal(info.instrSize)} fused=0x${Hexadecimal(CommitType.isFused(info.commitType))}\n")
      }
    }
  }
}

object UserTimerReferenceMain extends App {
  require(sys.env.get("UIT06_PROTOCOL_VERSION").contains("1"), "UIT06_PROTOCOL_VERSION must be 1")
  // Preserve this legacy fixture input as an explicit mapping to the sole production feature.
  val enabled = sys.env.get("UIT06_USER_TIMER") match {
    case Some("1") => true
    case Some("0") => false
    case _ => throw new IllegalArgumentException("UIT06_USER_TIMER must be 0 or 1")
  }
  val (config, firrtlOpts, firtoolOpts) = top.ArgParser.parse(args ++ Array("--has-fdi", enabled.toString))
  require(config(XSTileKey).size == 1)
  val options = config(DebugOptionsKey)
  require(!options.FPGAPlatform && options.EnableDifftest)
  Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
  ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
  top.Generator.execute(firrtlOpts, DisableMonitors(p =>
    if (enabled) new UserTimerReferenceSimTop()(p) else new UserTimerReferenceDisabledSimTop()(p))(config), firtoolOpts)
  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write(fileDir = "./build")
}
