// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.decode.{DecodeUnit, Imm_Z}
import xiangshan.backend.fu.NewCSR.CSREvents._
import xiangshan.backend.fu.wrapper.CSR
import xiangshan.backend.rob.RobPtr

// These leaf observations never drive state or alter the public CSR protocol.
class FDICriticalRecoveryLocalProbe(implicit p: Parameters) extends CSR(FuConfig.CsrCfg) {
  val observed = IO(Output(new Bundle {
    val mode = UInt(2.W)
    val virtualMode = Bool()
    val debugMode = Bool()
    val debugEffect = Bool()
    val critical = Bool()
    val dpc = UInt(64.W)
    val dcsr = UInt(64.W)
    val scratch = UInt(64.W)
    val scratchWrite = Bool()
    val returnPC = UInt(64.W)
    val returnPCWrite = Bool()
    val handler = Bool()
    val entryEffect = Bool()
    val uepc = UInt(64.W)
    val uip = UInt(64.W)
    val targetValid = Bool()
    val target = new TargetPCBundle
  }))
  observed.mode := csrMod.io.status.privState.PRVM.asUInt
  observed.virtualMode := csrMod.io.status.privState.V.asUInt.asBool
  observed.debugMode := csrMod.io.status.debugMode
  observed.debugEffect := observe(csrMod.trapEntryDEvent.valid)
  observed.critical := observe(csrMod.criticalErrorState)
  observed.dpc := observe(csrMod.dpc.rdata)
  observed.dcsr := observe(csrMod.dcsr.rdata)
  observed.scratch := observe(csrMod.csrOutMap(0x340))
  observed.scratchWrite := observe(csrMod.csrRwMap(0x340)._1.wen)
  observed.returnPC := csrMod.csrOutMap.get(0x8b1).map(observe(_)).getOrElse(0.U)
  observed.returnPCWrite := csrMod.csrRwMap.get(0x8b1).map(x => observe(x._1.wen)).getOrElse(false.B)
  observed.handler := csrMod.io.status.userInHandler.getOrElse(false.B)
  observed.entryEffect := csrMod.io.status.userEntryEffect.getOrElse(false.B)
  observed.uepc := csrMod.csrOutMap.get(0x041).map(observe(_)).getOrElse(0.U)
  observed.uip := csrMod.csrOutMap.get(0x044).map(observe(_)).getOrElse(0.U)
  observed.targetValid := csrMod.io.out.bits.targetPcUpdate
  observed.target := csrMod.io.out.bits.targetPc
}

// The test is the environment at existing request, trap and HU ports. Accepted
// identities are supplied explicitly; this fixture does not model ROB retirement.
class FDICriticalRecoveryLocalHarness(implicit val p: Parameters) extends Module with HasXSParameter {
  val io = IO(new Bundle {
    val request = Flipped(Decoupled(new Bundle {
      val instruction = UInt(32.W)
      val source = UInt(64.W)
      val owner = new InterruptEventIdentity
    }))
    val response = Decoupled(new Bundle {
      val data = UInt(64.W)
      val illegal = Bool()
      val virtualIllegal = Bool()
      val rob = new RobPtr
      val flushPipe = Bool()
    })
    val critical = Input(Bool())
    val halt = Input(Bool())
    val accepted = Input(Valid(new InterruptEventIdentity))
    val trap = Input(Valid(new Bundle {
      val pc = UInt(64.W)
      val singleStep = Bool()
      val isInterrupt = Bool()
      val event = new InterruptEventIdentity
    }))
    val hu = new HUEntryPort
    val candidate = Output(Valid(new InterruptDescriptor))
    val candidateKill = Output(Bool())
    val nativeInterrupt = Output(Bool())
    val criticalCommitBlock = Output(Bool())
    val distribution = Output(new DistributedCSRIO)
  })
  val csr = Module(new FDICriticalRecoveryLocalProbe)
  val observed = IO(Output(chiselTypeOf(csr.observed)))
  observed := csr.observed

  def idleInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(idleInputs)
    case vector: Vec[_] => vector.foreach(idleInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  idleInputs(csr.io)
  val decoder = Module(new DecodeUnit)
  idleInputs(decoder.io)
  decoder.io.enq.ctrlFlow.instr := io.request.bits.instruction
  csr.io.in.valid := io.request.valid
  csr.io.in.bits.ctrl.fuOpType := decoder.io.deq.decodedInst.fuOpType
  csr.io.in.bits.ctrl.robIdx := io.request.bits.owner.robIdx
  csr.io.in.bits.ctrl.ftqIdx.get := io.request.bits.owner.ftqIdx
  csr.io.in.bits.ctrl.ftqOffset.get := io.request.bits.owner.ftqOffset
  csr.io.in.bits.data.imm := Imm_Z().minBitsFromInstr(io.request.bits.instruction)
  csr.io.in.bits.data.src(0) := io.request.bits.source
  io.request.ready := csr.io.in.ready
  csr.io.out.ready := io.response.ready
  io.response.valid := csr.io.out.valid
  io.response.bits.data := csr.io.out.bits.res.data
  io.response.bits.illegal := csr.io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr)
  io.response.bits.virtualIllegal := csr.io.out.bits.ctrl.exceptionVec.get(ExceptionNO.virtualInstr)
  io.response.bits.rob := csr.io.out.bits.ctrl.robIdx
  io.response.bits.flushPipe := csr.io.out.bits.ctrl.flushPipe.get

  csr.io.csrin.get.criticalErrorState := io.critical
  csr.io.csrio.get.externalInterrupt.debug := io.halt
  csr.io.csrio.get.exception.valid := io.trap.valid
  csr.io.csrio.get.exception.bits.pc := io.trap.bits.pc
  csr.io.csrio.get.exception.bits.singleStep := io.trap.bits.singleStep
  csr.io.csrio.get.exception.bits.isInterrupt := io.trap.bits.isInterrupt
  csr.io.csrio.get.exception.bits.interruptEvent.foreach(_ := io.trap.bits.event)
  io.distribution := csr.io.csrio.get.customCtrl.distribute_csr
  io.nativeInterrupt := csr.io.csrio.get.interrupt
  io.criticalCommitBlock := csr.io.csrio.get.criticalErrorState
  io.candidate := 0.U.asTypeOf(io.candidate)
  io.candidateKill := false.B
  io.hu.reserve.ready := false.B
  io.hu.request.ready := false.B
  io.hu.completion.valid := false.B
  io.hu.completion.bits := 0.U.asTypeOf(io.hu.completion.bits)
  io.hu.effectLocked := false.B
  io.hu.canceled := false.B
  io.hu.satpMode := 0.U
  csr.io.csrio.get.userTimerDelivery.foreach { delivery =>
    delivery.accepted := io.accepted
    delivery.entry <> io.hu
    io.candidate := delivery.candidate
    io.candidateKill := delivery.candidateKill
    // The environment must retain the reservation identity until target release.
    val owned = RegInit(false.B)
    val owner = RegEnable(io.hu.reserve.bits, io.hu.reserve.fire)
    when(io.hu.reserve.fire) {
      assert(!owned)
      assert(io.accepted.valid && io.accepted.bits.asUInt === io.hu.reserve.bits.asUInt)
      owned := true.B
    }
    when(io.hu.request.valid) { assert(owned && io.hu.request.bits.event.asUInt === owner.asUInt) }
    when(io.hu.completion.valid) { assert(owned && io.hu.completion.bits.event.asUInt === owner.asUInt) }
    when(io.hu.release) { assert(owned); owned := false.B }
  }
}
