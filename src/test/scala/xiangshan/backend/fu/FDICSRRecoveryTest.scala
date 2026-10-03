// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._

// Observe the production response without altering its ready/valid contract.
class FDICSRRecoveryHarness(implicit p: Parameters) extends FDICSRDistributionHarness {
  val recovery = IO(new Bundle {
    val requestFlag = Input(Bool())
    val flushFlag = Input(Bool())
    val flushAfter = Input(Bool())
    val responseFlushPipe = Output(Bool())
    val responseFlag = Output(Bool())
    val responseFire = Output(Bool())
  })
  csr.io.in.bits.ctrl.robIdx.flag := recovery.requestFlag
  csr.io.flush.bits.robIdx.flag := recovery.flushFlag
  csr.io.flush.bits.level := Mux(recovery.flushAfter, RedirectLevel.flushAfter, RedirectLevel.flush)
  recovery.responseFlushPipe := csr.io.out.bits.ctrl.flushPipe.get
  recovery.responseFlag := csr.io.out.bits.ctrl.robIdx.flag
  recovery.responseFire := csr.io.out.fire
}

class FDICSRRecoveryTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDI software CSR recovery"

  private def run(name: String)(body: FDICSRRecoveryHarness => Unit): Unit = {
    val root = java.nio.file.Paths.get(sys.props("c07.runRoot")).toRealPath()
    require(java.nio.file.Paths.get("").toRealPath() == root)
    val base = new top.DefaultConfig
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = true)
      case DebugOptionsKey => base(DebugOptionsKey).copy(FPGAPlatform = true,
        EnableDifftest = false, AlwaysBasicDiff = false, EnablePerfDebug = false,
        EnableChiselDB = false, AlwaysBasicDB = false)
    }
    test(new FDICSRRecoveryHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation(name))) { dut =>
      dut.io.request.valid.poke(false.B)
      drive(dut, 0x340, 2, 0, 1, 0, 4)
      dut.io.response.ready.poke(true.B)
      dut.io.flush.poke(false.B)
      dut.io.flushRob.poke(4.U)
      dut.io.trap.poke(false.B)
      dut.recovery.requestFlag.poke(false.B)
      dut.recovery.flushFlag.poke(false.B)
      dut.recovery.flushAfter.poke(false.B)
      dut.reset.poke(true.B)
      dut.clock.step(5)
      dut.reset.poke(false.B)
      dut.clock.step()
      dut.io.request.ready.expect(true.B)
      dut.io.response.valid.expect(false.B)
      dut.io.writes.expect(0.U)
      body(dut)
    }
  }

  private def drive(dut: FDICSRRecoveryHarness, address: Int, function: Int,
    rs1: Int, rd: Int, operand: BigInt, rob: Int): Unit = {
    val instruction = (BigInt(address) << 20) | (BigInt(rs1) << 15) |
      (BigInt(function) << 12) | (BigInt(rd) << 7) | 0x73
    dut.io.request.bits.instruction.poke(instruction.U)
    dut.io.request.bits.operation.poke((8 | function).U)
    dut.io.request.bits.operand.poke(operand.U)
    dut.io.request.bits.basePc.poke(0x1000.U)
    dut.io.request.bits.offset.poke(0.U)
    dut.io.request.bits.rob.poke(rob.U)
  }

  private val finalWord = BigInt("f123456789abcdef", 16)
  // Independent indices in the existing observation ABI, not DUT-generated maps.
  private val returnPcView = 40
  private val returnPcControl = 11

  private def acceptWrite(dut: FDICSRRecoveryHarness, stalled: Boolean): Boolean = {
    drive(dut, 0x8b1, 1, 1, 1, finalWord, 4)
    dut.io.request.valid.poke(true.B)
    dut.io.response.ready.poke((!stalled).B)
    dut.io.request.ready.expect(true.B)
    dut.io.writes.expect(0.U)
    dut.clock.step()

    // A different offered request cannot replace the still-owned single slot.
    drive(dut, 0x8b0, 1, 2, 2, BigInt(0x7777), 7)
    dut.io.request.ready.expect(false.B)
    dut.io.response.valid.expect(true.B)
    dut.io.response.bits.illegal.expect(false.B)
    dut.io.response.bits.virtualIllegal.expect(false.B)
    dut.io.response.bits.rob.expect(4.U)
    dut.recovery.responseFlag.expect(false.B)
    dut.io.response.bits.data.expect(0.U)
    dut.io.writes.expect((BigInt(1) << returnPcView).U)
    dut.observed.bus.w.valid.expect(true.B)
    dut.observed.bus.w.bits.addr.expect(0x8b1.U)
    dut.observed.bus.w.bits.data.expect(finalWord.U)
    val responseFlush = dut.recovery.responseFlushPipe.peek().litToBoolean
    dut.recovery.responseFire.expect((!stalled).B)
    dut.clock.step()
    dut.io.request.valid.poke(false.B)
    dut.io.state(returnPcView).expect(finalWord.U)
    dut.observed.control(returnPcControl).expect(finalWord.U)
    dut.io.writes.expect(0.U)
    dut.observed.bus.w.valid.expect(false.B)
    println(s"C07_PROVED_C1_WRITE address=0x8b1 rob=4 writes=1 distributions=1 " +
      s"owner=0x${finalWord.toString(16)} stalled=$stalled flushPipe=$responseFlush")
    responseFlush
  }

  it should "c07-negative-response include flushPipe on the first legal write response" in {
    run("c07-response-immediate") { dut =>
      val responseFlush = acceptWrite(dut, stalled = false)
      dut.io.response.valid.expect(false.B)
      assert(responseFlush, "C07_MISSING_FLUSH_PIPE_WITH_ACCEPTED_WRITE")
      println("C07_RESPONSE_IMMEDIATE_PASS writes=1 distributions=1 responses=1")
    }
  }

  it should "c07-negative-response retain flushPipe after the write pulse under backpressure" in {
    run("c07-response-stalled") { dut =>
      val firstFlush = acceptWrite(dut, stalled = true)
      val heldFlush = (0 until 3).map { _ =>
        dut.io.request.ready.expect(false.B)
        dut.io.response.valid.expect(true.B)
        dut.io.response.bits.illegal.expect(false.B)
        dut.io.response.bits.virtualIllegal.expect(false.B)
        dut.io.response.bits.rob.expect(4.U)
        dut.io.response.bits.data.expect(0.U)
        dut.recovery.responseFlag.expect(false.B)
        dut.recovery.responseFire.expect(false.B)
        dut.io.writes.expect(0.U)
        dut.observed.bus.w.valid.expect(false.B)
        dut.io.state(returnPcView).expect(finalWord.U)
        val flush = dut.recovery.responseFlushPipe.peek().litToBoolean
        dut.clock.step()
        flush
      }
      dut.io.response.ready.poke(true.B)
      dut.io.request.ready.expect(false.B)
      dut.recovery.responseFire.expect(true.B)
      val acceptedFlush = dut.recovery.responseFlushPipe.peek().litToBoolean
      dut.clock.step()
      dut.io.response.valid.expect(false.B)
      dut.io.request.ready.expect(true.B)
      dut.io.writes.expect(0.U)
      dut.observed.bus.w.valid.expect(false.B)
      println(s"C07_PROVED_STALLED_RESPONSE address=0x8b1 rob=4 writes=1 distributions=1 " +
        s"responses=1 firstFlush=$firstFlush heldFlush=${heldFlush.mkString(",")} acceptedFlush=$acceptedFlush")
      assert(heldFlush.forall(identity) && acceptedFlush,
        "C07_MISSING_FLUSH_PIPE_AFTER_BACKPRESSURE")
      assert(firstFlush, "C07_MISSING_FLUSH_PIPE_WITH_ACCEPTED_WRITE")
      println("C07_RESPONSE_STALLED_PASS writes=1 distributions=1 responses=1")
    }
  }
}
