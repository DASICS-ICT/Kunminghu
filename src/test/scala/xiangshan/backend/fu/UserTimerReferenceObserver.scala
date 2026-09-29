// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import difftest._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{Redirect, XSBundle, XSModule}
import xiangshan.backend.rob.RobPtr

class UserTimerReferenceAllocation(implicit p: Parameters) extends XSBundle {
  val ptr = new RobPtr
  val pc = UInt(64.W)
  val instr = UInt(32.W)
}

class UserTimerReferenceCommit(implicit p: Parameters) extends XSBundle {
  val ptr = new RobPtr
  val count = UInt(8.W)
  val isTerminal = Bool()
}

class UserTimerReferenceRequest(implicit p: Parameters) extends XSBundle {
  val ptr = new RobPtr
  val csr = UInt(12.W)
  val funct3 = UInt(3.W)
  val read = Bool()
  val write = Bool()
  val legal = Bool()
  val isUret = Bool()
  val oldValue = UInt(64.W)
  val pending = Bool()
}

class UserTimerReferenceCompletion(implicit p: Parameters) extends XSBundle {
  val ptr = new RobPtr
  val rdata = UInt(64.W)
  val illegal = Bool()
  val virtualFault = Bool()
  val returnEffect = Bool()
  val target = UInt(64.W)
}

class UserTimerReferenceBoundary(implicit p: Parameters) extends XSBundle {
  val ptr = new RobPtr
  val isInterrupt = Bool()
  val isHU = Bool()
}

/** A simulation-only sink. Its inputs are observations, never ready/valid feedback. */
class UserTimerReferenceObserver(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val enabled = Input(Bool())
    val coreReset = Input(Bool())
    val allocate = Input(Vec(RenameWidth, Valid(new UserTimerReferenceAllocation)))
    val retire = Input(Vec(CommitWidth, Valid(new UserTimerReferenceCommit)))
    val flush = Input(Valid(new Redirect))
    val request = Input(Valid(new UserTimerReferenceRequest))
    val response = Input(Valid(new UserTimerReferenceCompletion))
    val softwareReady = Input(Bool())
    val returnCancel = Input(Bool())
    val writeValid = Input(Bool())
    val writeAddress = Input(UInt(12.W))
    val writeData = Input(UInt(64.W))
    val boundary = Input(Valid(new UserTimerReferenceBoundary))
    val architecturalTrap = Input(Bool())
    val huEffect = Input(Bool())
    val huRelease = Input(Bool())
    val trapPC = Input(UInt(64.W))
    val trapTarget = Input(UInt(64.W))
    val bank = Input(new UserTimerBank)
    val csrState = Input(new CSRState)
    val hcsrState = Input(new HCSRState)
    val access = Output(new UserTimerAccess)
    val completed = Output(new UserTimerResponse)
    val snapshot = Output(new UserTimerSnapshot)
    val committed = Output(Vec(CommitWidth, new UserTimerRetire))
    val canceled = Output(Vec(RobSize, new UserTimerCancel))
    val status = Output(new UserTimerClock)
    val terminal = Output(new UserTimerTerminal)
  })

  // A fresh host run owns epoch one. A second core reset is rejected rather than resumed.
  val cycle = RegInit(0.U(64.W))
  val released = RegInit(false.B)
  cycle := cycle + 1.U
  when(!io.coreReset) { released := true.B }
  when(released && io.coreReset) { assert(false.B, "UIT06 warm reset is unsupported") }
  io.status.epoch := 1.U
  io.status.cycle := cycle
  io.status.coreReset := io.coreReset || reset.asBool
  io.status.enabled := io.enabled

  val live = RegInit(VecInit(Seq.fill(RobSize)(false.B)))
  val flags = Reg(Vec(RobSize, Bool()))
  val ids = Reg(Vec(RobSize, UInt(64.W)))
  val pcs = Reg(Vec(RobSize, UInt(64.W)))
  val instructions = Reg(Vec(RobSize, UInt(32.W)))
  val accessed = RegInit(VecInit(Seq.fill(RobSize)(false.B)))
  val effected = RegInit(VecInit(Seq.fill(RobSize)(false.B)))
  val nextId = RegInit(1.U(64.W))
  val retiredCount = RegInit(0.U(64.W))
  def matches(ptr: RobPtr): Bool = live(ptr.value) && flags(ptr.value) === ptr.flag

  val boundaryLive = RegInit(false.B)
  val boundaryId = Reg(UInt(64.W))
  val boundaryPtr = Reg(new RobPtr)
  val boundaryCount = Reg(UInt(64.W))
  val boundaryInterrupt = Reg(Bool())
  val boundaryHU = Reg(Bool())
  val eventSequence = RegInit(0.U(64.W))
  when(io.boundary.valid && !io.coreReset) {
    assert(matches(io.boundary.bits.ptr), "UIT06 boundary has no live allocation")
    assert(!boundaryLive, "UIT06 architectural boundary overwritten")
    assert(!io.retire.map(_.valid).reduce(_ || _), "UIT06 boundary overlaps retirement")
    boundaryLive := true.B
    boundaryId := ids(io.boundary.bits.ptr.value)
    boundaryPtr := io.boundary.bits.ptr
    boundaryCount := retiredCount
    boundaryInterrupt := io.boundary.bits.isInterrupt
    boundaryHU := io.boundary.bits.isHU
  }
  when(io.architecturalTrap && !io.coreReset) {
    assert(boundaryLive, "UIT06 trap has no saved ROB boundary")
    assert(boundaryHU === io.huEffect, "UIT06 trap type changed after acceptance")
    assert(eventSequence =/= ~0.U(64.W), "UIT06 event sequence overflow")
    eventSequence := eventSequence + 1.U
    boundaryLive := false.B
  }
  when(io.huRelease && boundaryLive && boundaryHU) { boundaryLive := false.B }

  val killed = Wire(Vec(RobSize, Bool()))
  val killAccess = Wire(Vec(RobSize, Bool()))
  for (slot <- 0 until RobSize) {
    val ptr = Wire(new RobPtr)
    ptr.value := slot.U
    ptr.flag := flags(slot)
    killed(slot) := live(slot) && ptr.needFlush(io.flush)
    killAccess(slot) := killed(slot) && accessed(slot)
    when(killed(slot)) {
      live(slot) := false.B
      accessed(slot) := false.B
      assert(!effected(slot), "UIT06 an applied CSR or URET effect was discarded")
    }
  }
  // Pure CSR reads can execute speculatively and several may be canceled together.
  // Each slot publishes its old allocation identity at the actual redirect edge.
  for (slot <- 0 until RobSize) {
    val canceled = io.canceled(slot)
    val isBoundary = boundaryLive && ids(slot) === boundaryId
    canceled := 0.U.asTypeOf(canceled)
    canceled.valid := killAccess(slot) && !io.coreReset
    canceled.epoch := 1.U
    canceled.uid := ids(slot)
    canceled.cycle := cycle
    canceled.reason := Mux(isBoundary, Mux(boundaryInterrupt, 3.U, 2.U), 1.U)
    canceled.hadEffect := effected(slot)
  }

  val retirementAmounts = io.retire.map(r => Mux(r.valid, r.bits.count, 0.U))
  val retirementTotal = retirementAmounts.reduce(_ +& _)
  for (lane <- 0 until CommitWidth) {
    val source = io.retire(lane)
    val prefix = retirementAmounts.take(lane).foldLeft(0.U(64.W))(_ +& _)
    val out = io.committed(lane)
    out := 0.U.asTypeOf(out)
    out.valid := source.valid && !io.coreReset
    out.epoch := 1.U
    out.uid := ids(source.bits.ptr.value)
    out.robIdx := source.bits.ptr.value
    out.robFlag := source.bits.ptr.flag
    out.beforeCount := retiredCount + prefix
    out.afterCount := retiredCount + prefix + source.bits.count
    out.cycle := cycle
    when(out.valid) {
      assert(matches(source.bits.ptr), "UIT06 retirement has no live allocation")
      assert(source.bits.count =/= 0.U, "UIT06 zero-sized retirement")
      assert(!killed(source.bits.ptr.value), "UIT06 retirement overlaps cancellation")
      live(source.bits.ptr.value) := false.B
      accessed(source.bits.ptr.value) := false.B
    }
  }
  when(!io.coreReset) { retiredCount := retiredCount + retirementTotal }
  // The raw trap channel has no commit delay. Its marker carries the exact retiring
  // identity so the host can wait for that instruction's delayed comparison frame.
  val terminalValids = io.retire.map(r => r.valid && r.bits.isTerminal)
  when(!io.coreReset) { assert(PopCount(terminalValids) <= 1.U, "UIT06 multiple terminal instructions") }
  io.terminal := 0.U.asTypeOf(io.terminal)
  io.terminal.valid := terminalValids.reduce(_ || _) && !io.coreReset
  io.terminal.epoch := 1.U
  io.terminal.uid := Mux1H(terminalValids, io.committed.map(_.uid))
  io.terminal.cycle := cycle
  io.terminal.afterCount := Mux1H(terminalValids, io.committed.map(_.afterCount))
  io.terminal.pc := Mux1H(terminalValids, io.retire.map(r => pcs(r.bits.ptr.value)))
  when(io.terminal.valid) {
    assert(PopCount(io.retire.map(_.valid)) === 1.U, "UIT06 terminal retirement is not exclusive")
    assert(Mux1H(terminalValids, io.retire.map(_.bits.count)) === 1.U,
      "UIT06 terminal instruction was compressed or fused")
  }

  val allocationCount = PopCount(io.allocate.map(_.valid))
  when(allocationCount =/= 0.U && !io.coreReset) {
    assert(nextId <= ~0.U(64.W) - allocationCount, "UIT06 instruction sequence overflow")
    nextId := nextId + allocationCount
  }
  for (lane <- 0 until RenameWidth) {
    val source = io.allocate(lane)
    val index = source.bits.ptr.value
    when(source.valid && !io.coreReset) {
      val retiring = io.retire.map(r => r.valid && r.bits.ptr.value === index).reduce(_ || _)
      assert(!live(index) || killed(index) || retiring, "UIT06 live ROB allocation overwritten")
      assert(!io.flush.valid, "UIT06 allocation during ROB redirect")
      live(index) := true.B
      flags(index) := source.bits.ptr.flag
      ids(index) := nextId + PopCount(io.allocate.take(lane).map(_.valid))
      pcs(index) := source.bits.pc
      instructions(index) := source.bits.instr
      accessed(index) := false.B
      effected(index) := false.B
    }
  }

  val request = io.request.bits
  // ROB observes a redirect before its registered copy reaches the CSR unit.
  // A raw CSR handshake on that edge can already belong to a discarded instruction.
  val requestKilled = request.ptr.needFlush(io.flush)
  val requestFire = io.request.valid && !io.coreReset && !requestKilled
  when(io.request.valid && !io.coreReset) {
    assert(matches(request.ptr), "UIT06 raw CSR request has no live allocation")
    when(requestKilled) {
      printf(p"UIT06_DUT record=DISCARD cycle=${cycle} epoch=0x1 uid=0x${Hexadecimal(ids(request.ptr.value))} rob=0x${Hexadecimal(request.ptr.value)} robflag=0x${Hexadecimal(request.ptr.flag)} stage=0x0 reason=0x1\n")
    }
  }
  val ownerValid = RegInit(false.B)
  val ownerId = Reg(UInt(64.W))
  val ownerPtr = Reg(new RobPtr)
  val ownerRequest = Reg(new UserTimerReferenceRequest)
  val ownerPC = Reg(UInt(64.W))
  val ownerCanceled = ownerValid && ownerPtr.needFlush(io.flush)
  io.access := 0.U.asTypeOf(io.access)
  io.access.valid := requestFire
  io.access.epoch := 1.U
  io.access.uid := ids(request.ptr.value)
  io.access.robIdx := request.ptr.value
  io.access.robFlag := request.ptr.flag
  io.access.cycle := cycle
  io.access.pc := pcs(request.ptr.value)
  io.access.instr := instructions(request.ptr.value)
  io.access.csr := request.csr
  io.access.funct3 := request.funct3
  io.access.legal := request.legal
  io.access.needRead := request.read
  io.access.isUret := request.isUret
  io.access.pre := io.bank
  val sampled = request.legal && request.read && !request.isUret
  io.access.sampleKind := Mux(sampled && request.csr === "h800".U, 1.U,
    Mux(sampled && request.csr === "h044".U, 2.U, 0.U))
  io.access.sampleValue := Mux(io.access.sampleKind === 2.U, request.pending.asUInt,
    Mux(io.access.sampleKind === 1.U, request.oldValue, 0.U))
  when(requestFire) {
    assert(matches(request.ptr), "UIT06 CSR request has no live allocation")
    assert(!accessed(request.ptr.value), "UIT06 duplicate CSR acceptance")
    assert(!ownerValid, "UIT06 CSR response owner overwritten")
    assert(!request.ptr.needFlush(io.flush), "UIT06 accepted a flushed CSR")
    ownerValid := true.B
    ownerId := ids(request.ptr.value)
    ownerPtr := request.ptr
    ownerRequest := request
    ownerPC := pcs(request.ptr.value)
    accessed(request.ptr.value) := true.B
  }
  when(io.response.valid && !io.coreReset) {
    // Non-user CSR responses are not part of this channel.
    when(ownerValid) {
      assert(io.response.bits.ptr === ownerPtr, "UIT06 response changed CSR identity")
      when(ownerCanceled) {
        printf(p"UIT06_DUT record=DISCARD cycle=${cycle} epoch=0x1 uid=0x${Hexadecimal(ownerId)} rob=0x${Hexadecimal(ownerPtr.value)} robflag=0x${Hexadecimal(ownerPtr.flag)} stage=0x1 reason=0x2\n")
      }
      when(ownerRequest.isUret && !ownerCanceled) {
        assert(io.response.bits.returnEffect === (!io.response.bits.illegal && !io.response.bits.virtualFault),
          "UIT06 successful URET response omitted its effect")
      }
      ownerValid := false.B
    }
  }
  when(ownerCanceled) { ownerValid := false.B }
  when(io.returnCancel && ownerValid && ownerRequest.isUret && !ownerCanceled && !io.coreReset) {
    // The accepted program excludes asynchronous debug/critical return takeover.
    assert(false.B, "UIT06 asynchronous URET cancellation is unsupported")
  }
  when(ownerValid && io.softwareReady && !requestFire && !io.coreReset) {
    assert(false.B, "UIT06 CSR owner disappeared without response or cancellation")
  }
  io.completed := 0.U.asTypeOf(io.completed)
  io.completed.valid := io.response.valid && ownerValid && !ownerCanceled && !io.coreReset
  io.completed.epoch := 1.U
  io.completed.uid := ownerId
  io.completed.cycle := cycle
  io.completed.rdata := io.response.bits.rdata
  io.completed.illegal := io.response.bits.illegal
  io.completed.virtualFault := io.response.bits.virtualFault

  // The expected C2 record is driven by acceptance, so a missing DUT write remains visible.
  val c1Valid = RegNext(requestFire && !request.isUret, false.B)
  val c1Id = RegEnable(io.access.uid, requestFire)
  val c1PC = RegEnable(io.access.pc, requestFire)
  val c1Ptr = RegEnable(request.ptr, requestFire)
  val c1Killed = c1Ptr.needFlush(io.flush)
  val c2Valid = RegNext(c1Valid && !c1Killed && !io.coreReset, false.B)
  val c2Id = RegEnable(c1Id, c1Valid)
  val c2PC = RegEnable(c1PC, c1Valid)
  val c2Ptr = RegEnable(c1Ptr, c1Valid)
  val c2Write = RegEnable(io.writeValid, c1Valid)
  val c2Data = RegEnable(io.writeData, c1Valid)
  when(io.writeValid && !io.coreReset) {
    assert(c1Valid && ownerValid && !ownerRequest.isUret && ownerRequest.legal && ownerRequest.write,
      "UIT06 user CSR write has no legal accepted owner")
    assert(io.writeAddress === ownerRequest.csr, "UIT06 user CSR write changed address")
    assert(!c1Killed, "UIT06 user CSR write survived cancellation")
    effected(ownerPtr.value) := true.B
  }
  when(io.response.bits.returnEffect && !io.coreReset) {
    assert(io.completed.valid, "UIT06 URET effect has no live response owner")
    effected(ownerPtr.value) := true.B
  }

  val returnDone = io.completed.valid && ownerRequest.isUret
  val returnSnapshotValid = RegNext(returnDone, false.B)
  val returnId = RegEnable(ownerId, returnDone)
  val returnPC = RegEnable(ownerPC, returnDone)
  val returnPtr = RegEnable(ownerPtr, returnDone)
  val returnTarget = RegEnable(io.response.bits.target, returnDone)
  val trapSnapshotValid = RegNext(io.architecturalTrap && !io.coreReset, false.B)
  val trapId = RegEnable(boundaryId, io.architecturalTrap)
  val trapPtr = RegEnable(boundaryPtr, io.architecturalTrap)
  val trapCount = RegEnable(boundaryCount, io.architecturalTrap)
  val trapSequence = RegEnable(eventSequence + 1.U, io.architecturalTrap)
  val trapPC = RegEnable(io.trapPC, io.architecturalTrap)
  val trapTarget = RegEnable(io.trapTarget, io.architecturalTrap)
  val c2Cancellation = io.canceled(c2Ptr.value)
  val csrSnapshotValid = c2Valid && !(c2Cancellation.valid && c2Cancellation.uid === c2Id && c2Cancellation.reason =/= 2.U)
  val snapshotValids = Seq(csrSnapshotValid, returnSnapshotValid, trapSnapshotValid)
  when(!io.coreReset) {
    assert(PopCount(snapshotValids) <= 1.U, "UIT06 snapshot boundaries overlap")
  }
  io.snapshot := 0.U.asTypeOf(io.snapshot)
  io.snapshot.valid := snapshotValids.reduce(_ || _) && !io.coreReset
  io.snapshot.epoch := 1.U
  io.snapshot.uid := Mux1H(snapshotValids, Seq(c2Id, returnId, trapId))
  io.snapshot.robIdx := Mux1H(snapshotValids, Seq(c2Ptr.value, returnPtr.value, trapPtr.value))
  io.snapshot.robFlag := Mux1H(snapshotValids, Seq(c2Ptr.flag, returnPtr.flag, trapPtr.flag))
  io.snapshot.cycle := cycle
  io.snapshot.kind := Mux1H(snapshotValids, Seq(1.U, 2.U, 3.U))
  io.snapshot.pc := Mux1H(snapshotValids, Seq(c2PC, returnPC, trapPC))
  io.snapshot.target := Mux(trapSnapshotValid, trapTarget, Mux(returnSnapshotValid, returnTarget, 0.U))
  io.snapshot.eventSeq := Mux(trapSnapshotValid, trapSequence, 0.U)
  io.snapshot.beforeCount := Mux(trapSnapshotValid, trapCount, 0.U)
  io.snapshot.writeSeen := csrSnapshotValid && c2Write
  io.snapshot.writeData := Mux(csrSnapshotValid, c2Data, 0.U)
  io.snapshot.post := io.bank
  io.snapshot.csrState := io.csrState
  io.snapshot.hcsrState := io.hcsrState

  when(io.coreReset) {
    live.foreach(_ := false.B)
    accessed.foreach(_ := false.B)
    effected.foreach(_ := false.B)
    ownerValid := false.B
    boundaryLive := false.B
    c1Valid := false.B
    c2Valid := false.B
    returnSnapshotValid := false.B
    trapSnapshotValid := false.B
    retiredCount := 0.U
    nextId := 1.U
    eventSequence := 0.U
  }
}
