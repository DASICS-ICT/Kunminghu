// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu

import chisel3._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.backend.rob.RobPtr

class UserTimerReferenceObserverTest extends AnyFlatSpec with ChiselScalatestTester {
  private val base = new top.DefaultConfig
  private implicit val parameters: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head.copy(HasUserTimerInterrupt = true)
  }

  private def clear(data: Data): Unit = data match {
    case value: Bool => value.poke(false.B)
    case value: UInt => value.poke(0.U)
    case value: SInt => value.poke(0.S)
    case value: Vec[_] => value.foreach(clear)
    case value: Record => value.elements.values.foreach(clear)
    case _ => throw new IllegalArgumentException(s"Unsupported observation input $data")
  }
  private def pointer(ptr: RobPtr, index: Int, flag: Boolean = false): Unit = {
    ptr.value.poke(index.U)
    ptr.flag.poke(flag.B)
  }
  private def initialize(dut: UserTimerReferenceObserver): Unit = {
    dut.io.enabled.poke(true.B)
    Seq(dut.io.allocate, dut.io.retire, dut.io.flush, dut.io.request, dut.io.response,
      dut.io.boundary, dut.io.bank, dut.io.csrState, dut.io.hcsrState).foreach(clear)
    dut.io.coreReset.poke(true.B)
    dut.io.softwareReady.poke(true.B)
    dut.io.returnCancel.poke(false.B)
    dut.io.writeValid.poke(false.B)
    dut.io.writeAddress.poke(0.U)
    dut.io.writeData.poke(0.U)
    dut.io.architecturalTrap.poke(false.B)
    dut.io.huEffect.poke(false.B)
    dut.io.huRelease.poke(false.B)
    dut.io.trapPC.poke(0.U)
    dut.io.trapTarget.poke(0.U)
    dut.clock.step(2)
    dut.io.coreReset.poke(false.B)
  }
  private def allocate(dut: UserTimerReferenceObserver, index: Int, instr: BigInt, pc: BigInt = 0x80001000L): Unit = {
    val input = dut.io.allocate(0)
    input.valid.poke(true.B)
    pointer(input.bits.ptr, index)
    input.bits.pc.poke(pc.U)
    input.bits.instr.poke(instr.U)
    dut.clock.step()
    input.valid.poke(false.B)
  }
  private def retire(dut: UserTimerReferenceObserver, index: Int, uid: Int): Unit = {
    dut.io.retire(0).valid.poke(true.B)
    pointer(dut.io.retire(0).bits.ptr, index)
    dut.io.retire(0).bits.count.poke(1.U)
    dut.io.committed(0).valid.expect(true.B)
    dut.io.committed(0).uid.expect(uid.U)
    dut.clock.step()
    dut.io.retire(0).valid.poke(false.B)
  }

  behavior of "User timer reference observations"

  it should "preserve execution samples across delayed responses and emit post snapshots even when a write is missing" in {
    test(new UserTimerReferenceObserver).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("observer-samples"))) { dut =>
      initialize(dut)
      var uid = 0
      for (funct3 <- Seq(1, 2, 3, 5, 6, 7); rd <- Seq(0, 5); rs1 <- Seq(0, 6)) {
        uid += 1
        val csr = if (uid % 2 == 0) 0x44 else 0x800
        val instruction = (BigInt(csr) << 20) | (rs1 << 15) | (funct3 << 12) | (rd << 7) | 0x73
        allocate(dut, 0, instruction)
        val rw = (funct3 & 3) == 1
        val read = !rw || rd != 0
        val write = rw || rs1 != 0
        val request = dut.io.request
        request.valid.poke(true.B)
        pointer(request.bits.ptr, 0)
        request.bits.csr.poke(csr.U)
        request.bits.funct3.poke(funct3.U)
        request.bits.read.poke(read.B)
        request.bits.write.poke(write.B)
        request.bits.legal.poke(true.B)
        request.bits.isUret.poke(false.B)
        request.bits.oldValue.poke((1000 + uid).U)
        request.bits.pending.poke((uid % 4 == 0).B)
        dut.io.access.uid.expect(uid.U)
        dut.io.access.robIdx.expect(0.U)
        dut.io.access.robFlag.expect(false.B)
        dut.io.access.instr.expect(instruction.U)
        dut.io.access.funct3.expect(funct3.U)
        dut.io.access.sampleKind.expect((if (!read) 0 else if (csr == 0x800) 1 else 2).U)
        dut.io.access.sampleValue.expect((if (!read) 0 else if (csr == 0x800) 1000 + uid else if (uid % 4 == 0) 1 else 0).U)
        dut.clock.step()
        request.valid.poke(false.B)
        // Live input changes must not replace the accepted owner.
        request.bits.csr.poke(0x40.U)
        request.bits.oldValue.poke(0.U)
        pointer(request.bits.ptr, 7)
        dut.io.softwareReady.poke(false.B)
        // Deliberately omit a requested write: its C2 comparison record must still exist.
        dut.io.writeValid.poke(false.B)
        dut.clock.step()
        dut.io.snapshot.valid.expect(true.B)
        dut.io.snapshot.uid.expect(uid.U)
        dut.io.snapshot.writeSeen.expect(false.B)
        dut.io.snapshot.kind.expect(1.U)
        dut.clock.step(3)
        dut.io.snapshot.valid.expect(false.B)
        dut.io.response.valid.poke(true.B)
        pointer(dut.io.response.bits.ptr, 0)
        dut.io.response.bits.rdata.poke((1000 + uid).U)
        dut.io.completed.valid.expect(true.B)
        dut.io.completed.uid.expect(uid.U)
        dut.clock.step()
        dut.io.response.valid.poke(false.B)
        dut.io.softwareReady.poke(true.B)
        dut.io.completed.valid.expect(false.B)
        retire(dut, 0, uid)
      }
    }
  }

  it should "keep a previous write snapshot separate from a new acceptance and return effects" in {
    test(new UserTimerReferenceObserver).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("observer-snapshots"))) { dut =>
      initialize(dut)
      allocate(dut, 0, BigInt("040092f3", 16))
      allocate(dut, 1, BigInt("044022f3", 16))
      dut.io.request.valid.poke(true.B)
      pointer(dut.io.request.bits.ptr, 0)
      dut.io.request.bits.csr.poke(0x40.U)
      dut.io.request.bits.funct3.poke(1.U)
      dut.io.request.bits.read.poke(true.B)
      dut.io.request.bits.write.poke(true.B)
      dut.io.request.bits.legal.poke(true.B)
      dut.clock.step()
      dut.io.request.valid.poke(false.B)
      dut.io.softwareReady.poke(false.B)
      dut.io.writeValid.poke(true.B)
      dut.io.writeAddress.poke(0x40.U)
      dut.io.writeData.poke(0x3456.U)
      dut.io.response.valid.poke(true.B)
      pointer(dut.io.response.bits.ptr, 0)
      dut.clock.step()
      dut.io.writeValid.poke(false.B)
      dut.io.response.valid.poke(false.B)
      dut.io.softwareReady.poke(true.B)
      dut.io.bank.uscratch.poke(0x3456.U)
      dut.io.request.valid.poke(true.B)
      pointer(dut.io.request.bits.ptr, 1)
      dut.io.request.bits.csr.poke(0x44.U)
      dut.io.request.bits.funct3.poke(2.U)
      dut.io.request.bits.write.poke(false.B)
      dut.io.request.bits.pending.poke(true.B)
      dut.io.snapshot.valid.expect(true.B)
      dut.io.snapshot.uid.expect(1.U)
      dut.io.snapshot.robIdx.expect(0.U)
      dut.io.snapshot.post.uscratch.expect(0x3456.U)
      dut.io.snapshot.writeSeen.expect(true.B)
      dut.io.access.valid.expect(true.B)
      dut.io.access.uid.expect(2.U)
      dut.io.access.robIdx.expect(1.U)
      dut.io.access.pre.uscratch.expect(0x3456.U)
      dut.io.access.sampleValue.expect(1.U)
      dut.clock.step()
      dut.io.request.valid.poke(false.B)
      dut.io.softwareReady.poke(false.B)
      dut.io.response.valid.poke(true.B)
      pointer(dut.io.response.bits.ptr, 1)
      dut.clock.step()
      dut.io.response.valid.poke(false.B)
      dut.io.softwareReady.poke(true.B)
      dut.io.snapshot.uid.expect(2.U)
      dut.io.snapshot.robIdx.expect(1.U)
      retire(dut, 0, 1)
      retire(dut, 1, 2)

      allocate(dut, 0, BigInt("00200073", 16))
      dut.io.request.valid.poke(true.B)
      pointer(dut.io.request.bits.ptr, 0)
      dut.io.request.bits.isUret.poke(true.B)
      dut.io.request.bits.read.poke(false.B)
      dut.io.request.bits.csr.poke(2.U)
      dut.io.request.bits.funct3.poke(0.U)
      dut.io.bank.inHandler.poke(1.U)
      dut.clock.step()
      dut.io.request.valid.poke(false.B)
      dut.io.softwareReady.poke(false.B)
      dut.clock.step(5)
      dut.io.snapshot.valid.expect(false.B)
      dut.io.response.valid.poke(true.B)
      pointer(dut.io.response.bits.ptr, 0)
      dut.io.response.bits.returnEffect.poke(true.B)
      dut.io.response.bits.target.poke(0x80001236L.U)
      dut.clock.step()
      dut.io.response.valid.poke(false.B)
      dut.io.response.bits.returnEffect.poke(false.B)
      dut.io.softwareReady.poke(true.B)
      dut.io.bank.inHandler.poke(0.U)
      dut.io.bank.ustatus.poke(0x11.U)
      dut.io.snapshot.valid.expect(true.B)
      dut.io.snapshot.uid.expect(3.U)
      dut.io.snapshot.kind.expect(2.U)
      dut.io.snapshot.target.expect(0x80001236L.U)
      dut.io.snapshot.post.inHandler.expect(0.U)
      retire(dut, 0, 3)
    }
  }

  it should "preserve the HU boundary through pointer cancellation and reuse" in {
    test(new UserTimerReferenceObserver).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("observer-boundary"))) { dut =>
      initialize(dut)
      allocate(dut, 0, BigInt("00000013", 16), 0x80001236L)
      dut.io.boundary.valid.poke(true.B)
      pointer(dut.io.boundary.bits.ptr, 0)
      dut.io.boundary.bits.isInterrupt.poke(true.B)
      dut.io.boundary.bits.isHU.poke(true.B)
      dut.clock.step()
      dut.io.boundary.valid.poke(false.B)
      dut.io.flush.valid.poke(true.B)
      pointer(dut.io.flush.bits.robIdx, 0)
      dut.io.flush.bits.level.poke(1.U)
      dut.clock.step()
      dut.io.flush.valid.poke(false.B)
      dut.io.architecturalTrap.poke(true.B)
      dut.io.huEffect.poke(true.B)
      dut.io.trapPC.poke(0x80001236L.U)
      dut.io.trapTarget.poke(0x80002000L.U)
      dut.clock.step()
      dut.io.architecturalTrap.poke(false.B)
      dut.io.huEffect.poke(false.B)
      dut.io.bank.uepc.poke(0x80001236L.U)
      dut.io.bank.ucause.poke(BigInt("8000000000000004", 16).U)
      dut.io.bank.inHandler.poke(1.U)
      dut.io.snapshot.valid.expect(true.B)
      dut.io.snapshot.kind.expect(3.U)
      dut.io.snapshot.uid.expect(1.U)
      dut.io.snapshot.beforeCount.expect(0.U)
      dut.io.snapshot.eventSeq.expect(1.U)
      allocate(dut, 0, BigInt("00000013", 16), 0x80002000L)
      retire(dut, 0, 2)
    }
  }

  it should "retain a faulting CSR identity and distinguish it from an uneffected redirect cancellation" in {
    test(new UserTimerReferenceObserver).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("observer-fault"))) { dut =>
      initialize(dut)
      allocate(dut, 0, BigInt("800022f3", 16))
      dut.io.request.valid.poke(true.B)
      pointer(dut.io.request.bits.ptr, 0)
      dut.io.request.bits.csr.poke(0x800.U)
      dut.io.request.bits.funct3.poke(2.U)
      dut.io.request.bits.read.poke(true.B)
      dut.io.request.bits.legal.poke(false.B)
      dut.io.request.bits.oldValue.poke(999.U)
      dut.io.access.sampleKind.expect(0.U)
      dut.io.access.sampleValue.expect(0.U)
      dut.clock.step()
      dut.io.request.valid.poke(false.B)
      dut.io.softwareReady.poke(false.B)
      dut.io.response.valid.poke(true.B)
      pointer(dut.io.response.bits.ptr, 0)
      dut.io.response.bits.illegal.poke(true.B)
      dut.clock.step()
      dut.io.response.valid.poke(false.B)
      dut.io.response.bits.illegal.poke(false.B)
      dut.io.softwareReady.poke(true.B)
      dut.io.snapshot.valid.expect(true.B)
      dut.io.snapshot.uid.expect(1.U)
      dut.io.snapshot.writeSeen.expect(false.B)
      dut.io.boundary.valid.poke(true.B)
      pointer(dut.io.boundary.bits.ptr, 0)
      dut.clock.step()
      dut.io.boundary.valid.poke(false.B)
      dut.io.flush.valid.poke(true.B)
      pointer(dut.io.flush.bits.robIdx, 0)
      dut.io.flush.bits.level.poke(1.U)
      dut.io.canceled(0).valid.expect(true.B)
      dut.io.canceled(0).uid.expect(1.U)
      dut.io.canceled(0).reason.expect(2.U)
      dut.io.canceled(0).hadEffect.expect(false.B)
      dut.clock.step()
      dut.io.flush.valid.poke(false.B)
      dut.io.architecturalTrap.poke(true.B)
      dut.io.trapPC.poke(0x80001000L.U)
      dut.io.trapTarget.poke(0x80003000L.U)
      dut.clock.step()
      dut.io.architecturalTrap.poke(false.B)
      dut.io.csrState.mepc.poke(0x80001000L.U)
      dut.io.snapshot.valid.expect(true.B)
      dut.io.snapshot.kind.expect(3.U)
      dut.io.snapshot.uid.expect(1.U)
      dut.io.snapshot.csrState.mepc.expect(0x80001000L.U)

      allocate(dut, 0, BigInt("800022f3", 16))
      dut.io.request.valid.poke(true.B)
      dut.io.request.bits.legal.poke(true.B)
      dut.clock.step()
      dut.io.request.valid.poke(false.B)
      dut.io.softwareReady.poke(false.B)
      dut.io.flush.valid.poke(true.B)
      dut.io.canceled(0).valid.expect(true.B)
      dut.io.canceled(0).uid.expect(2.U)
      dut.io.canceled(0).reason.expect(1.U)
      dut.clock.step()
      dut.io.flush.valid.poke(false.B)
      dut.io.softwareReady.poke(true.B)
      dut.io.snapshot.valid.expect(false.B)
      dut.io.completed.valid.expect(false.B)
      dut.clock.step()
    }
  }

  it should "bind the undelayed terminal marker to the final retiring UID and expanded instruction count" in {
    test(new UserTimerReferenceObserver).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("observer-terminal"))) { dut =>
      initialize(dut)
      allocate(dut, 0, BigInt("00000013", 16), 0x80001000L)
      allocate(dut, 1, BigInt("0000006b", 16), 0x8000100cL)
      dut.io.retire(0).valid.poke(true.B)
      pointer(dut.io.retire(0).bits.ptr, 0)
      dut.io.retire(0).bits.count.poke(3.U)
      dut.io.committed(0).afterCount.expect(3.U)
      dut.io.terminal.valid.expect(false.B)
      dut.clock.step()
      clear(dut.io.retire)
      dut.io.retire(0).valid.poke(true.B)
      pointer(dut.io.retire(0).bits.ptr, 1)
      dut.io.retire(0).bits.count.poke(1.U)
      dut.io.retire(0).bits.isTerminal.poke(true.B)
      dut.io.committed(0).beforeCount.expect(3.U)
      dut.io.terminal.valid.expect(true.B)
      dut.io.terminal.uid.expect(2.U)
      dut.io.terminal.afterCount.expect(4.U)
      dut.io.terminal.pc.expect(0x8000100cL.U)
      dut.clock.step()
      clear(dut.io.retire)
      dut.io.terminal.valid.expect(false.B)
    }
  }

  it should "discard a new raw CSR read on the earlier ROB redirect while canceling the completed previous read" in {
    test(new UserTimerReferenceObserver).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("observer-redirect-accept"))) { dut =>
      initialize(dut)
      allocate(dut, 0, BigInt("00000063", 16), 0x800166aaL)
      allocate(dut, 1, BigInt("041027f3", 16), 0x800166aeL)
      allocate(dut, 2, BigInt("042027f3", 16), 0x800166baL)
      dut.io.request.valid.poke(true.B)
      pointer(dut.io.request.bits.ptr, 1)
      dut.io.request.bits.csr.poke(0x41.U)
      dut.io.request.bits.funct3.poke(2.U)
      dut.io.request.bits.read.poke(true.B)
      dut.io.request.bits.legal.poke(true.B)
      dut.io.access.uid.expect(2.U)
      dut.clock.step()
      dut.io.request.valid.poke(false.B)
      dut.io.softwareReady.poke(false.B)
      dut.io.response.valid.poke(true.B)
      pointer(dut.io.response.bits.ptr, 1)
      dut.io.response.bits.rdata.poke(0x80010b5aL.U)
      dut.io.completed.valid.expect(true.B)
      dut.clock.step()
      dut.io.response.valid.poke(false.B)
      dut.io.softwareReady.poke(true.B)
      dut.io.request.valid.poke(true.B)
      pointer(dut.io.request.bits.ptr, 2)
      dut.io.request.bits.csr.poke(0x42.U)
      dut.io.flush.valid.poke(true.B)
      pointer(dut.io.flush.bits.robIdx, 0)
      dut.io.flush.bits.level.poke(0.U)
      dut.io.access.valid.expect(false.B)
      dut.io.snapshot.valid.expect(false.B)
      dut.io.canceled(1).valid.expect(true.B)
      dut.io.canceled(1).uid.expect(2.U)
      dut.io.canceled(1).reason.expect(1.U)
      dut.io.canceled(2).valid.expect(false.B)
      dut.clock.step()
      dut.io.request.valid.poke(false.B)
      dut.io.flush.valid.poke(false.B)
      dut.io.canceled.foreach(_.valid.expect(false.B))
      dut.io.snapshot.valid.expect(false.B)
      dut.clock.step(2)
      dut.io.snapshot.valid.expect(false.B)
      dut.io.completed.valid.expect(false.B)
    }
  }

  it should "cancel several speculative reads on one edge including a returning read and preserve reused identities" in {
    test(new UserTimerReferenceObserver).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("observer-redirect-batch"))) { dut =>
      initialize(dut)
      allocate(dut, 0, BigInt("00000063", 16))
      for (index <- 1 to 3) {
        allocate(dut, index, BigInt("800022f3", 16), 0x80001004L + index * 4)
      }
      for (index <- 1 to 3) {
        dut.io.request.valid.poke(true.B)
        pointer(dut.io.request.bits.ptr, index)
        dut.io.request.bits.csr.poke(0x800.U)
        dut.io.request.bits.funct3.poke(2.U)
        dut.io.request.bits.read.poke(true.B)
        dut.io.request.bits.legal.poke(true.B)
        dut.io.request.bits.oldValue.poke((100 + index).U)
        dut.io.access.uid.expect((index + 1).U)
        dut.clock.step()
        dut.io.request.valid.poke(false.B)
        dut.io.softwareReady.poke(false.B)
        dut.io.response.valid.poke(true.B)
        pointer(dut.io.response.bits.ptr, index)
        dut.io.response.bits.rdata.poke((100 + index).U)
        if (index == 3) {
          dut.io.flush.valid.poke(true.B)
          pointer(dut.io.flush.bits.robIdx, 0)
          dut.io.flush.bits.level.poke(0.U)
          dut.io.completed.valid.expect(false.B)
          for (killed <- 1 to 3) {
            dut.io.canceled(killed).valid.expect(true.B)
            dut.io.canceled(killed).uid.expect((killed + 1).U)
            dut.io.canceled(killed).reason.expect(1.U)
            dut.io.canceled(killed).hadEffect.expect(false.B)
          }
          dut.io.canceled(0).valid.expect(false.B)
        } else dut.io.completed.valid.expect(true.B)
        dut.clock.step()
        dut.io.response.valid.poke(false.B)
        dut.io.flush.valid.poke(false.B)
        dut.io.softwareReady.poke(true.B)
      }
      dut.io.canceled.foreach(_.valid.expect(false.B))
      dut.io.snapshot.valid.expect(false.B)
      allocate(dut, 2, BigInt("800022f3", 16), 0x8000100cL)
      dut.io.request.valid.poke(true.B)
      pointer(dut.io.request.bits.ptr, 2)
      dut.io.request.bits.oldValue.poke(7.U)
      dut.io.access.uid.expect(5.U)
      dut.io.access.sampleValue.expect(7.U)
      dut.clock.step()
      dut.io.request.valid.poke(false.B)
      dut.io.softwareReady.poke(false.B)
      dut.io.response.valid.poke(true.B)
      pointer(dut.io.response.bits.ptr, 2)
      dut.clock.step()
      dut.io.response.valid.poke(false.B)
      dut.io.softwareReady.poke(true.B)
      dut.io.snapshot.uid.expect(5.U)
      retire(dut, 0, 1)
      retire(dut, 2, 5)
    }
  }

  for (effect <- Seq("write", "return")) {
    it should s"reject an unregistered $effect effect after a raw request was already killed at ROB" in {
      assertThrows[ChiselAssertionError] {
        test(new UserTimerReferenceObserver).withAnnotations(Seq(
          VerilatorBackendAnnotation, TargetDirAnnotation(s"observer-orphan-$effect"))) { dut =>
          initialize(dut)
          allocate(dut, 0, BigInt("00000063", 16))
          allocate(dut, 1, if (effect == "write") BigInt("04009073", 16) else BigInt("00200073", 16))
          dut.io.request.valid.poke(true.B)
          pointer(dut.io.request.bits.ptr, 1)
          dut.io.request.bits.csr.poke((if (effect == "write") 0x40 else 2).U)
          dut.io.request.bits.funct3.poke((if (effect == "write") 1 else 0).U)
          dut.io.request.bits.write.poke((effect == "write").B)
          dut.io.request.bits.isUret.poke((effect == "return").B)
          dut.io.request.bits.legal.poke(true.B)
          dut.io.flush.valid.poke(true.B)
          pointer(dut.io.flush.bits.robIdx, 0)
          dut.io.flush.bits.level.poke(0.U)
          dut.io.access.valid.expect(false.B)
          dut.clock.step()
          dut.io.request.valid.poke(false.B)
          dut.io.flush.valid.poke(false.B)
          dut.io.softwareReady.poke(false.B)
          if (effect == "write") {
            dut.io.writeValid.poke(true.B)
            dut.io.writeAddress.poke(0x40.U)
            dut.io.writeData.poke(0x123.U)
          } else {
            dut.io.response.valid.poke(true.B)
            pointer(dut.io.response.bits.ptr, 1)
            dut.io.response.bits.returnEffect.poke(true.B)
          }
          dut.clock.step()
        }
      }
    }
  }
}
