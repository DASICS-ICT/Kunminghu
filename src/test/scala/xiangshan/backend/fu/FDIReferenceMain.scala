// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import difftest._
import freechips.rocketchip.diplomacy.DisableMonitors
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, Constantin, FileRegisters, SignExt}
import xiangshan.{DebugOptionsKey, ExceptionNO, JumpOpType, XSTileKey}
import xiangshan.backend.fu.wrapper.{CSR, JumpUnit}

object FDIReferenceObservations {
  // Jump data.pc is the FTQ block base. Reconstruct the instruction address
  // from the same accepted input, independently of the computed link effect.
  def callSourcePC(input: FuncUnitInput): UInt =
    SignExt(input.data.pc.get, 64) + (input.ctrl.ftqOffset.get << input.instOffsetBits)

  /** Observe the actual CSR producer and the already allocated UIT identities. */
  def connect(csr: CSR, identities: UserTimerReferenceObserver,
    jumps: Seq[JumpUnit] = Seq.empty): FDIReferenceObserver = {
    val bank = csr.csrMod
    require(bank.coreParams.HasFDI, "FDI producer requires the enabled production bank")
    require(FDIReferenceProjection.addresses.forall(bank.csrOutMap.contains),
      "FDI producer is missing a raw architectural view")
    require(FDIReferenceProjection.addresses.forall(bank.csrRwMap.contains),
      "FDI producer is missing an architectural write owner")
    val observer = Module(new FDIReferenceObserver()(bank.p))
    val in = observer.io
    val request = observe(bank.io.in.bits)
    val requestFire = observe(bank.io.in.valid) && observe(bank.io.in.ready)
    val control = observe(csr.io.in.bits.ctrl)
    val response = observe(bank.io.out.bits)
    val responseValid = observe(bank.io.out.valid)
    val ptr = control.robIdx
    val ids = observe(identities.ids)
    val flags = observe(identities.flags)
    val live = observe(identities.live)
    val instructions = observe(identities.instructions)
    in.coreReset := identities.io.status.coreReset
    in.epoch := identities.io.status.epoch
    in.cycle := identities.io.status.cycle
    in.flush := identities.io.flush
    in.retired := identities.io.committed
    in.boundary := identities.io.boundary
    in.request.valid := requestFire && !request.redirectFlush &&
      (request.ren || request.wen) &&
      FDIReferenceProjection.addresses.map(a => request.addr === a.U).reduce(_ || _)
    in.request.bits.ptr := ptr
    in.request.bits.uid := ids(ptr.value)
    in.request.bits.live := live(ptr.value) && flags(ptr.value) === ptr.flag
    in.request.bits.instruction := instructions(ptr.value)
    in.request.bits.pc := request.sourcePc
    in.request.bits.address := request.addr
    in.request.bits.read := request.ren
    in.request.bits.write := request.wen
    in.request.bits.permitted := !observe(bank.permitMod.io.out.EX_II) &&
      !observe(bank.permitMod.io.out.EX_VI)
    in.responseValid := responseValid
    in.responseIllegal := response.EX_II
    in.responseVirtual := response.EX_VI
    for ((address, index) <- FDIReferenceProjection.addresses.zipWithIndex) {
      // Production csrOutMap contains raw regOut (and the specified UMainCfg view).
      // Never normalize the observed values before handing them to the checker.
      in.bank(index) := observe(bank.csrOutMap(address))
    }
    in.writes := VecInit(FDIReferenceProjection.addresses.map(address =>
      observe(bank.csrRwMap(address)._1.wen))).asUInt

    in.call := 0.U.asTypeOf(in.call)
    // NewCSR accepts this sink in the same cycle whenever its reset is low.
    // The following bank snapshot independently verifies the owner's update.
    in.callWrite := observe(bank.io.fdiCallReturnPC.get.valid) && !observe(bank.reset).asBool
    in.callIllegal := false.B
    if (jumps.nonEmpty) {
      // Capture the accepted Jump transaction, not the implicit-effect pulse:
      // a missing effect must leave an observable comparison obligation.
      val calls = jumps.map { jump =>
        val input = observe(jump.io.in.bits)
        val output = observe(jump.io.out.bits)
        val operation = input.ctrl.fuOpType
        val accepted = observe(jump.io.out.valid) && observe(jump.io.out.ready) &&
          (operation === JumpOpType.fdicallJ || operation === JumpOpType.fdicallJR)
        val call = Wire(new FDIReferenceRequest()(bank.p))
        val ptr = input.ctrl.robIdx
        val illegal = output.ctrl.exceptionVec.get(ExceptionNO.illegalInstr)
        call.ptr := ptr
        call.uid := ids(ptr.value)
        call.live := live(ptr.value) && flags(ptr.value) === ptr.flag
        call.instruction := instructions(ptr.value)
        call.pc := callSourcePC(input)
        call.address := "h8b1".U
        call.read := false.B
        call.write := true.B
        call.permitted := !illegal
        (accepted, call, illegal)
      }
      val accepted = calls.map(_._1)
      in.call.valid := VecInit(accepted).asUInt.orR
      in.call.bits := Mux1H(accepted, calls.map(_._2))
      in.callIllegal := Mux1H(accepted, calls.map(_._3))
      when(!in.coreReset) {
        assert(PopCount(VecInit(accepted)) <= 1.U, "FDI observation saw simultaneous call acceptances")
      }
    }

    val architecturalTrap = identities.io.architecturalTrap
    val trapInstruction = RegEnable(Mux(identities.io.huEffect, 0.U,
      observe(bank.io.fromRob.trap.bits.instr)), architecturalTrap)
    val event = identities.io.snapshot
    in.trap.valid := event.valid && event.kind === 3.U
    in.trap.bits.epoch := event.epoch
    in.trap.bits.uid := event.uid
    in.trap.bits.robIdx := event.robIdx
    in.trap.bits.robFlag := event.robFlag
    in.trap.bits.cycle := event.cycle
    in.trap.bits.pc := event.pc
    in.trap.bits.instruction := trapInstruction
    in.trap.bits.eventSeq := event.eventSeq
    in.trap.bits.beforeCount := event.beforeCount
    observer
  }

  def attach(csr: CSR, identities: UserTimerReferenceObserver, hartId: UInt,
    jumps: Seq[JumpUnit] = Seq.empty): Unit = {
    if (csr.csrMod.coreParams.HasFDI) {
      val observer = connect(csr, identities, jumps)
      for (lane <- 0 until 4) {
        val packet = DifftestModule(new DiffFDIObservation, delay = 0)
        packet.coreid := hartId
        packet.index := lane.U
        observer.io.packets(lane).elements.foreach { case (name, data) =>
          packet.elements(name) := data
        }
      }
    }
  }
}

/** The same real core/UID observer supplies both independent reference channels. */
class FDIReferenceSimTop(implicit val referenceParameters: Parameters) extends top.SimTop()(referenceParameters) {
  override def desiredName: String = "SimTop"
  override protected def addDifftestObservations(): Unit = {
    val jumps = l_soc.core_with_l2.head.core.backend.inner.intExuBlock.get.exus.flatMap(
      _.module.funcUnits.collect { case jump: JumpUnit => jump })
    require(jumps.nonEmpty, "FDI full-core observations require the real Jump units")
    UserTimerReferenceObservations.attach(this,
      (csr, identities, hartId) => FDIReferenceObservations.attach(csr, identities, hartId, jumps))
  }
}

object FDIReferenceMain extends App {
  require(sys.env.get("FDI_PROTOCOL_VERSION").contains("65536"), "FDI_PROTOCOL_VERSION must be 65536")
  val (config, firrtlOpts, firtoolOpts) = top.ArgParser.parse(args)
  require(config(XSTileKey).size == 1, "FDI reference supports one cold-start hart")
  val options = config(DebugOptionsKey)
  require(!options.FPGAPlatform && options.EnableDifftest)
  Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
  ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
  top.Generator.execute(firrtlOpts, DisableMonitors(p =>
    if (p(XSTileKey).head.HasFDI) new FDIReferenceSimTop()(p) else new top.SimTop()(p))(config), firtoolOpts)
  ChiselDB.addToFileRegisters
  Constantin.addToFileRegisters
  FileRegisters.write(fileDir = "./build")
}
