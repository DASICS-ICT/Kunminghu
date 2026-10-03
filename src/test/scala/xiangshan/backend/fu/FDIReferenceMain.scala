// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import difftest._
import freechips.rocketchip.diplomacy.DisableMonitors
import org.chipsalliance.cde.config.Parameters
import utility.{ChiselDB, Constantin, FileRegisters}
import xiangshan.{DebugOptionsKey, XSTileKey}
import xiangshan.backend.fu.wrapper.CSR

object FDIReferenceObservations {
  /** Observe the actual CSR producer and the already allocated UIT identities. */
  def connect(csr: CSR, identities: UserTimerReferenceObserver): FDIReferenceObserver = {
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

  def attach(csr: CSR, identities: UserTimerReferenceObserver, hartId: UInt): Unit = {
    if (csr.csrMod.coreParams.HasFDI) {
      val observer = connect(csr, identities)
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
  override protected def addDifftestObservations(): Unit =
    UserTimerReferenceObservations.attach(this, FDIReferenceObservations.attach)
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
