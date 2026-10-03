// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import difftest.{FDIObservation, UserTimerRetire}
import org.chipsalliance.cde.config.Parameters
import xiangshan.{Redirect, XSBundle, XSModule}
import xiangshan.backend.rob.RobPtr

object FDIReferenceProjection {
  // Fixed machine-ABI order, including the original-bit-position UMainCfg alias.
  val addresses: Seq[Int] = Seq(0xbc4, 0x9e1, 0xbc5, 0xbc6, 0x9e2, 0x9e3, 0x880) ++
    (0x890 to 0x8af) ++ Seq(0x8b0, 0x8b1, 0x8b2, 0x8b3, 0x8c8) ++ (0x8c0 to 0x8c7)
  require(addresses.size == 52 && addresses.distinct.size == 52)
  val reset = 1
  val pre = 2
  val post = 3
  val trap = 4
  val cancel = 5
}

class FDIReferenceRequest(implicit p: Parameters) extends XSBundle {
  val ptr = new RobPtr
  val uid = UInt(64.W)
  val live = Bool()
  val instruction = UInt(32.W)
  val pc = UInt(64.W)
  val address = UInt(12.W)
  val read = Bool()
  val write = Bool()
  val permitted = Bool()
}

class FDIReferenceTrap extends Bundle {
  val epoch = UInt(64.W)
  val uid = UInt(64.W)
  val robIdx = UInt(16.W)
  val robFlag = Bool()
  val cycle = UInt(64.W)
  val pc = UInt(64.W)
  val instruction = UInt(32.W)
  val eventSeq = UInt(64.W)
  val beforeCount = UInt(64.W)
}

/** Passive simulation observer. Identities come from the existing ROB observer. */
class FDIReferenceObserver(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val coreReset = Input(Bool())
    val epoch = Input(UInt(64.W))
    val cycle = Input(UInt(64.W))
    val bank = Input(Vec(52, UInt(64.W)))
    val request = Input(Valid(new FDIReferenceRequest))
    val responseValid = Input(Bool())
    val responseIllegal = Input(Bool())
    val responseVirtual = Input(Bool())
    val writes = Input(UInt(52.W))
    val flush = Input(Valid(new Redirect))
    val retired = Input(Vec(CommitWidth, new UserTimerRetire))
    val boundary = Input(Valid(new UserTimerReferenceBoundary))
    val trap = Input(Valid(new FDIReferenceTrap))
    val packets = Output(Vec(4, new FDIObservation))
  })
  io.packets.foreach(_ := 0.U.asTypeOf(new FDIObservation))
  private val active = HasFDI.B && !io.coreReset && !reset.asBool
  private val resetSeen = RegInit(false.B)
  private val resetWords = Reg(Vec(52, UInt(64.W)))
  private val resetNow = active && !resetSeen
  when(resetSeen && io.coreReset) { assert(false.B, "FDI observer does not support warm reset") }
  when(resetNow) {
    assert(!io.request.valid && !io.writes.orR, "FDI reset proof must precede guest access")
    resetSeen := true.B
    resetWords := io.bank
    io.bank.foreach(word => assert(word === 0.U, "FDI natural reset state is polluted"))
  }
  // The captured proof is never refreshed from a later all-zero architectural state.
  dontTouch(resetWords)

  private val request = io.request.bits
  private val requestKilled = request.ptr.needFlush(io.flush)
  private val accepted = active && resetSeen && io.request.valid && !requestKilled
  private val ownerLive = RegInit(false.B)
  private val owner = Reg(new FDIObservation)
  private val ownerPtr = Reg(new RobPtr)
  private val ownerEffect = RegInit(false.B)
  private val ownerIllegal = RegInit(false.B)
  private val ownerVirtual = RegInit(false.B)
  private val ownerBoundary = RegInit(false.B)
  private val ownerInterrupt = RegInit(false.B)
  private val ownerRetired = io.retired.map(r => r.valid && r.uid === owner.uid).reduce(_ || _)
  private val ownerKilled = active && ownerLive && ownerPtr.needFlush(io.flush)
  private val boundaryNow = io.boundary.valid && io.boundary.bits.ptr === ownerPtr
  private val isBoundary = ownerBoundary || boundaryNow
  private val boundaryInterrupt = Mux(boundaryNow, io.boundary.bits.isInterrupt, ownerInterrupt)

  private val pre = Wire(new FDIObservation)
  pre := 0.U.asTypeOf(pre)
  pre.valid := accepted
  pre.kind := FDIReferenceProjection.pre.U
  pre.epoch := io.epoch
  pre.uid := request.uid
  pre.robIdx := request.ptr.value
  pre.robFlag := request.ptr.flag
  pre.cycle := io.cycle
  pre.pc := request.pc
  pre.instruction := request.instruction
  pre.csrAddress := request.address
  pre.readNeeded := request.read
  pre.writeNeeded := request.write
  pre.permitted := request.permitted
  pre.words := io.bank
  io.packets(0) := pre
  when(resetNow) {
    io.packets(0) := 0.U.asTypeOf(pre)
    io.packets(0).valid := true.B
    io.packets(0).kind := FDIReferenceProjection.reset.U
    io.packets(0).epoch := io.epoch
    io.packets(0).cycle := io.cycle
    io.packets(0).words := io.bank
  }

  // All six FDI CSR forms retain waitForward/blockBackward in DecodeUnit.
  // NewDispatch admits them into an empty ROB; Rob blocks younger enqueue
  // until that owner leaves. One retained owner therefore covers every cancel.
  when(accepted) {
    assert(request.live && request.uid =/= 0.U, "FDI acceptance has no real live ROB identity")
    assert(!ownerLive || ownerRetired, "FDI serialized CSR owner was overwritten")
    ownerLive := true.B
    owner := pre
    ownerPtr := request.ptr
    ownerEffect := false.B
    ownerIllegal := false.B
    ownerVirtual := false.B
    ownerBoundary := false.B
  }
  when(active && ownerLive && boundaryNow) {
    ownerBoundary := true.B
    ownerInterrupt := io.boundary.bits.isInterrupt
  }
  when(active && ownerLive && ownerRetired) {
    assert(!ownerKilled, "FDI owner retired and canceled on the same edge")
    when(!accepted) { ownerLive := false.B }
    for (retired <- io.retired) {
      when(retired.valid && retired.uid === owner.uid) {
        assert(retired.robIdx === owner.robIdx && retired.robFlag === owner.robFlag,
          "FDI retirement changed ROB identity")
      }
    }
  }

  // C0 acceptance creates a C1 obligation even if the production write is absent.
  private val c1Valid = RegNext(accepted, false.B)
  private val c1 = RegEnable(pre, accepted)
  private val c1Ptr = RegEnable(request.ptr, accepted)
  private val c1Killed = c1Ptr.needFlush(io.flush)
  private val c2Valid = RegNext(active && c1Valid && !c1Killed, false.B)
  private val c2 = RegEnable(c1, c1Valid)
  private val c2Write = RegEnable(io.writes.orR, c1Valid)
  private val c2Illegal = RegEnable(io.responseIllegal, c1Valid)
  private val c2Virtual = RegEnable(io.responseVirtual, c1Valid)
  when(active && c1Valid) {
    assert(ownerLive && owner.uid === c1.uid, "FDI effect stage changed owner")
    when(!c1Killed) { assert(io.responseValid, "FDI accepted CSR has no C1 response") }
    ownerIllegal := io.responseIllegal
    ownerVirtual := io.responseVirtual
  }
  when(active && io.writes.orR) {
    assert(PopCount(io.writes) === 1.U, "FDI write has multiple address owners")
    assert(c1Valid && !c1Killed && ownerLive && c1.permitted && c1.writeNeeded,
      "FDI architectural write has no legal accepted owner")
    val writeAddress = Mux1H(io.writes.asBools, FDIReferenceProjection.addresses.map(_.U(12.W)))
    assert(writeAddress === c1.csrAddress, "FDI architectural write changed address")
    ownerEffect := true.B
  }
  io.packets(1) := c2
  io.packets(1).valid := active && c2Valid
  io.packets(1).kind := FDIReferenceProjection.post.U
  io.packets(1).cycle := io.cycle
  io.packets(1).actualWrite := c2Write
  io.packets(1).responseIllegal := c2Illegal
  io.packets(1).responseVirtual := c2Virtual
  io.packets(1).words := io.bank

  // Cancellation carries the actual bank at the redirect, including faults whose
  // C2 snapshot already exists. The host retains synchronous faults until trap.
  io.packets(3) := owner
  io.packets(3).valid := ownerKilled
  io.packets(3).kind := FDIReferenceProjection.cancel.U
  io.packets(3).cycle := io.cycle
  io.packets(3).actualWrite := ownerEffect || io.writes.orR
  io.packets(3).responseIllegal := Mux(c1Valid, io.responseIllegal, ownerIllegal)
  io.packets(3).responseVirtual := Mux(c1Valid, io.responseVirtual, ownerVirtual)
  io.packets(3).cancelReason := Mux(isBoundary, Mux(boundaryInterrupt, 3.U, 2.U), 1.U)
  io.packets(3).words := io.bank
  when(ownerKilled) {
    assert(!ownerEffect && !io.writes.orR, "FDI applied architectural write was discarded")
    ownerLive := false.B
  }

  private val trap = io.trap.bits
  io.packets(2).valid := active && io.trap.valid
  io.packets(2).kind := FDIReferenceProjection.trap.U
  io.packets(2).epoch := trap.epoch
  io.packets(2).uid := trap.uid
  io.packets(2).robIdx := trap.robIdx
  io.packets(2).robFlag := trap.robFlag
  io.packets(2).cycle := trap.cycle
  io.packets(2).pc := trap.pc
  io.packets(2).instruction := trap.instruction
  io.packets(2).eventSeq := trap.eventSeq
  io.packets(2).beforeCount := trap.beforeCount
  io.packets(2).words := io.bank
  when(active && io.trap.valid) {
    assert(resetSeen && trap.epoch === io.epoch && trap.uid =/= 0.U && trap.eventSeq =/= 0.U && trap.cycle === io.cycle,
      "FDI trap snapshot has no real architectural event identity")
  }
}
