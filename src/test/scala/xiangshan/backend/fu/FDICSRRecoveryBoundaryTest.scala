// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._

class FDICSRRecoveryBoundaryTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDI software write response ownership"

  private val addresses = Seq(0xbc4, 0xbc5, 0xbc6, 0x9e1, 0x9e2, 0x9e3, 0x880) ++
    (0x890 to 0x8af) ++ (0x8b0 to 0x8b3) ++ (0x8c0 to 0x8c8)
  private val ones = (BigInt(1) << 64) - 1
  private val masks = addresses.map { address => address -> (address match {
    case 0xbc4 => BigInt(0x7ff)
    case 0x9e1 => BigInt(0x7c2)
    case 0x880 => BigInt("bbbbbbbbbbbbbbbb", 16)
    case 0x8c8 => BigInt("0001000100010001", 16)
    case 0x8b3 => BigInt(7)
    case 0x8b0 | 0x8b1 | 0x8b2 => ones
    case _ => ones ^ 7
  }) }.toMap
  private def backing(address: Int): Int = if (address == 0x9e1) 0xbc4 else address

  private def run(name: String, enabled: Boolean)(body: Driver => Unit): Unit = {
    val root = java.nio.file.Paths.get(sys.props("c07.runRoot")).toRealPath()
    require(java.nio.file.Paths.get("").toRealPath() == root)
    val base = new top.DefaultConfig
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled)
      case DebugOptionsKey => base(DebugOptionsKey).copy(FPGAPlatform = true,
        EnableDifftest = false, AlwaysBasicDiff = false, EnablePerfDebug = false,
        EnableChiselDB = false, AlwaysBasicDB = false)
    }
    test(new FDICSRRecoveryHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation(name))) { dut =>
      val driver = new Driver(dut, enabled)
      driver.initialize()
      body(driver)
      println(s"C07_BOUNDARY_PASS group=$name requests=${driver.requests} effects=${driver.effects} " +
        s"responses=${driver.responses} flushResponses=${driver.flushResponses}")
    }
  }

  private case class Redirect(rob: Int, flag: Boolean = false, after: Boolean = false)

  private class Driver(val dut: FDICSRRecoveryHarness, enabled: Boolean) {
    private val bank = scala.collection.mutable.Map.from(addresses.map(backing).distinct.map(_ -> BigInt(0)))
    var requests = 0
    var effects = 0
    var responses = 0
    var flushResponses = 0

    private def checkBank(): Unit = addresses.zipWithIndex.foreach { case (address, index) =>
      dut.io.state(index).expect((if (enabled) bank(backing(address)) & masks(address) else BigInt(0)).U)
    }

    private def drive(address: Int, function: Int, source: Int, rd: Int, operand: BigInt,
      rob: Int, flag: Boolean): Unit = {
      val instruction = (BigInt(address) << 20) | (BigInt(source) << 15) |
        (BigInt(function) << 12) | (BigInt(rd) << 7) | 0x73
      dut.io.request.bits.instruction.poke(instruction.U)
      dut.io.request.bits.operation.poke((8 | function).U)
      dut.io.request.bits.operand.poke(operand.U)
      dut.io.request.bits.basePc.poke(0x1000.U)
      dut.io.request.bits.offset.poke(0.U)
      dut.io.request.bits.rob.poke(rob.U)
      dut.recovery.requestFlag.poke(flag.B)
    }

    private def redirect(event: Option[Redirect]): Unit = {
      dut.io.flush.poke(event.nonEmpty.B)
      dut.io.flushRob.poke(event.map(_.rob).getOrElse(0).U)
      dut.recovery.flushFlag.poke(event.exists(_.flag).B)
      dut.recovery.flushAfter.poke(event.exists(_.after).B)
    }

    def initialize(): Unit = {
      dut.io.request.valid.poke(false.B)
      drive(0x340, 2, 0, 1, 0, 4, false)
      dut.io.response.ready.poke(true.B)
      dut.io.trap.poke(false.B)
      redirect(None)
      dut.reset.poke(true.B)
      dut.clock.step(5)
      dut.reset.poke(false.B)
      dut.clock.step()
      checkBank()
    }

    def resetWriteAt(delayAfterAcceptance: Int): Unit = {
      require(enabled && delayAfterAcceptance >= 0 && delayAfterAcceptance <= 2)
      dut.io.request.ready.expect(true.B)
      drive(0xbc4, 1, 1, 1, 3, 4, false)
      dut.io.request.valid.poke(true.B)
      dut.io.response.ready.poke(false.B)
      redirect(None)
      dut.clock.step()
      requests += 1
      dut.io.request.valid.poke(false.B)
      dut.io.response.valid.expect(true.B)
      dut.recovery.responseFlushPipe.expect(true.B)
      dut.io.writes.expect(1.U)
      dut.observed.bus.w.valid.expect(true.B)
      for (elapsed <- 0 until delayAfterAcceptance) {
        dut.io.response.valid.expect(true.B)
        dut.recovery.responseFlushPipe.expect(true.B)
        dut.io.writes.expect((if (elapsed == 0) 1 else 0).U)
        dut.observed.bus.w.valid.expect((elapsed == 0).B)
        dut.clock.step()
        if (elapsed == 0) { bank(0xbc4) = 3; effects += 1 }
        checkBank()
      }
      dut.observed.frontendInput.w.valid.expect((delayAfterAcceptance == 2).B)
      dut.observed.memoryInput.w.valid.expect((delayAfterAcceptance == 2).B)
      // Reset is present at the selected effect/transport edge while the response is unaccepted.
      dut.reset.poke(true.B)
      dut.io.writes.expect(0.U)
      dut.observed.bus.w.valid.expect(false.B)
      dut.recovery.responseFlushPipe.expect(false.B)
      dut.clock.step()
      bank.keys.foreach(key => bank(key) = 0)
      dut.reset.poke(false.B)
      dut.io.response.ready.poke(true.B)
      for (_ <- 0 until 3) {
        dut.io.response.valid.expect(false.B)
        dut.recovery.responseFlushPipe.expect(false.B)
        dut.io.request.ready.expect(true.B)
        dut.io.writes.expect(0.U)
        dut.observed.bus.w.valid.expect(false.B)
        dut.observed.frontendInput.w.valid.expect(false.B)
        dut.observed.memoryInput.w.valid.expect(false.B)
        Seq(dut.observed.control, dut.observed.frontend, dut.observed.memory)
          .foreach(_.foreach(_.expect(0.U)))
        checkBank()
        dut.clock.step()
      }
      access(0xbc4, function = 2, source = 0, stalls = 2)
      access(0x8b1, operand = 0x55, stalls = 2)
      println(s"C07_RESET_EDGE_PASS resetCycle=C${delayAfterAcceptance + 1} " +
        s"writeAppliedBeforeReset=${delayAfterAcceptance > 0} pendingCleared=true freshReadFlush=false freshWriteFlush=true")
    }

    def satpFlushControl(): Unit = {
      require(enabled)
      dut.io.request.ready.expect(true.B)
      drive(0x180, 1, 1, 1, 0, 4, false)
      dut.io.request.valid.poke(true.B)
      dut.io.response.ready.poke(true.B)
      redirect(None)
      dut.clock.step()
      requests += 1
      // The unchanged ordinary CSR path consumes its live address in C1.
      dut.io.request.valid.poke(false.B)
      dut.io.request.ready.expect(false.B)
      dut.io.response.valid.expect(true.B)
      dut.io.response.bits.illegal.expect(false.B)
      dut.io.response.bits.virtualIllegal.expect(false.B)
      dut.recovery.responseFlushPipe.expect(true.B)
      dut.recovery.responseFire.expect(true.B)
      dut.io.writes.expect(0.U)
      dut.observed.bus.w.valid.expect(true.B)
      dut.observed.bus.w.bits.addr.expect(0x180.U)
      dut.observed.bus.w.bits.data.expect(0.U)
      dut.clock.step()
      responses += 1
      flushResponses += 1
      for (_ <- 0 until 3) {
        dut.io.response.valid.expect(false.B)
        dut.io.writes.expect(0.U)
        dut.observed.bus.w.valid.expect(false.B)
        checkBank()
        dut.clock.step()
      }
      println("C07_SATP_CONTROL_PASS ordinaryWrite=true ordinaryFlush=true fdiWrite=false")
    }

    def access(address: Int, function: Int = 1, source: Int = 1, rd: Int = 1,
      operand: BigInt = 0, stalls: Int = 0, rob: Int = 4, flag: Boolean = false,
      before: Option[Redirect] = None, atEffect: Option[Redirect] = None,
      killedBefore: Boolean = false, killedAtEffect: Boolean = false,
      afterEffect: Boolean = false, resetHeld: Boolean = false, settleCycles: Int = 2): Unit = {
      require(!afterEffect || stalls > 0)
      require(!resetHeld || stalls > 0)
      val implemented = masks.contains(address)
      val illegal = implemented && !enabled
      val write = (function & 3) == 1 || source != 0
      val old = if (enabled && implemented) bank(backing(address)) & masks(address) else BigInt(0)
      val src = if (function >= 5) BigInt(source) else operand
      val raw = (function & 3) match {
        case 1 => src
        case 2 => old | src
        case 3 => old & (ones ^ src)
      }
      val finalWord = if (!implemented) BigInt(0) else if (address == 0x9e1) {
        (bank(0xbc4) & 0x3d) | (raw & masks(address))
      } else raw & masks(address)
      val effect = enabled && implemented && write && !killedBefore && !killedAtEffect
      val validResponse = !killedBefore && !killedAtEffect

      dut.io.request.ready.expect(true.B)
      drive(address, function, source, rd, operand, rob, flag)
      dut.io.request.valid.poke(true.B)
      dut.io.response.ready.poke((stalls == 0).B)
      redirect(before)
      dut.clock.step()
      requests += 1
      // Inactive and blocked input bits must not replace this response's owner.
      drive(0x8b0, 1, 2, 2, BigInt(0x1234), 7, !flag)
      dut.io.request.valid.poke(false.B)
      redirect(atEffect)
      dut.io.response.valid.expect(validResponse.B)
      if (validResponse) {
        dut.io.request.ready.expect(false.B)
        dut.io.response.bits.rob.expect(rob.U)
        dut.recovery.responseFlag.expect(flag.B)
        dut.io.response.bits.illegal.expect(illegal.B)
        dut.io.response.bits.virtualIllegal.expect(false.B)
        if (!illegal && implemented) dut.io.response.bits.data.expect(old.U)
        dut.recovery.responseFlushPipe.expect(effect.B)
      }
      dut.io.writes.expect((if (effect) BigInt(1) << addresses.indexOf(address) else BigInt(0)).U)
      dut.observed.bus.w.valid.expect(effect.B)
      if (effect) {
        dut.observed.bus.w.bits.addr.expect(address.U)
        dut.observed.bus.w.bits.data.expect(finalWord.U)
      }
      dut.clock.step()
      if (effect) {
        bank(backing(address)) = finalWord
        effects += 1
      }
      redirect(None)
      checkBank()

      if (stalls > 0 && validResponse && afterEffect) {
        // This local protocol injection checks no rollback, not a reachable ROB cancellation.
        redirect(Some(Redirect(rob, flag)))
        dut.io.response.valid.expect(false.B)
        dut.clock.step()
        redirect(None)
      } else if (stalls > 0 && validResponse && resetHeld) {
        dut.reset.poke(true.B)
        dut.clock.step()
        bank.keys.foreach(key => bank(key) = 0)
        dut.reset.poke(false.B)
        dut.io.response.valid.expect(false.B)
        checkBank()
      } else if (stalls > 0 && validResponse) {
        for (index <- 0 until stalls) {
          drive(0x8b0 + index % 3, 1, 2, 2, BigInt(index + 1), 7 + index, !flag)
          dut.io.request.valid.poke(true.B)
          dut.io.request.ready.expect(false.B)
          dut.io.response.valid.expect(true.B)
          dut.io.response.bits.rob.expect(rob.U)
          dut.recovery.responseFlag.expect(flag.B)
          dut.io.response.bits.illegal.expect(illegal.B)
          if (!illegal && implemented) dut.io.response.bits.data.expect(old.U)
          dut.recovery.responseFlushPipe.expect(effect.B)
          dut.io.writes.expect(0.U)
          dut.observed.bus.w.valid.expect(false.B)
          dut.clock.step()
          checkBank()
        }
        dut.io.request.valid.poke(false.B)
        dut.io.response.ready.poke(true.B)
        dut.io.request.ready.expect(false.B)
        dut.recovery.responseFire.expect(true.B)
        dut.recovery.responseFlushPipe.expect(effect.B)
        dut.clock.step()
      }
      if (validResponse && !afterEffect && !resetHeld) {
        responses += 1
        if (effect) flushResponses += 1
      }
      dut.io.response.ready.poke(true.B)
      dut.io.response.valid.expect(false.B)
      dut.io.request.ready.expect(true.B)
      for (_ <- 0 until settleCycles) {
        dut.io.writes.expect(0.U)
        dut.observed.bus.w.valid.expect(false.B)
        dut.clock.step()
        checkBank()
      }
    }
  }

  it should "preserve every software write event and isolate canceled or subsequent responses" in {
    run("c07-boundary-on", enabled = true) { driver =>
      for (address <- addresses) {
        driver.access(address, operand = ones)
        driver.access(address, operand = ones, stalls = 2)
        for (function <- Seq(2, 3)) {
          driver.access(address, function = function, source = 3, operand = 0, stalls = 2)
          driver.access(address, function = function, source = 0, operand = ones, stalls = 1)
        }
        driver.access(address, function = 5, source = 0, rd = 0, stalls = 1)
        for (function <- Seq(6, 7)) {
          driver.access(address, function = function, source = 1, rd = 0, stalls = 2)
          driver.access(address, function = function, source = 0, stalls = 1)
        }
        driver.access(address, source = 0, rd = 0, operand = 0)
      }
      driver.access(0x8b1, operand = ones, before = Some(Redirect(4)), killedBefore = true)
      driver.access(0x8b1, operand = ones, atEffect = Some(Redirect(3)), killedAtEffect = true)
      driver.access(0x8b1, operand = ones, atEffect = Some(Redirect(4)), killedAtEffect = true)
      driver.access(0x8b1, operand = ones, atEffect = Some(Redirect(4, after = true)), stalls = 2)
      driver.access(0x8b1, operand = ones, atEffect = Some(Redirect(7)), stalls = 2)
      driver.access(0x8b1, operand = 0x1357, rob = 1, flag = true,
        atEffect = Some(Redirect(driver.dut.RobSize - 2)), killedAtEffect = true)
      driver.access(0x8b1, operand = 0x2468, stalls = 2, afterEffect = true)
      driver.access(0x8b1, function = 2, source = 0, stalls = 2)
      driver.access(0x8b1, operand = 0x3579, stalls = 2, resetHeld = true)
      driver.access(0x8b1, function = 2, source = 0, stalls = 2)
      driver.access(0x8b1, operand = 0x468a, stalls = 2)
      driver.access(0x340, function = 2, source = 0, stalls = 2)
      // A new C0 follows the prior C1 response; the response edge itself cannot replace the slot.
      driver.access(0x8b0, operand = 0x1111, rob = 1, settleCycles = 0)
      driver.access(0x8b1, operand = 0x2222, rob = 2, settleCycles = 0)
      driver.access(0x8b1, function = 2, source = 0, rob = 3, settleCycles = 0)
      driver.access(0x8b2, operand = 0x3333, rob = 4)
    }
  }

  it should "omit FDI write events and flush responses in the disabled configuration" in {
    run("c07-boundary-off", enabled = false) { driver =>
      for (address <- addresses) {
        driver.access(address, operand = ones, stalls = 2)
        driver.access(address, function = 2, source = 0, stalls = 1)
      }
      assert(driver.effects == 0 && driver.flushResponses == 0)
    }
  }

  it should "c07-reset-effect clear the response obligation at the write edge and both transport positions" in {
    run("c07-reset-stages", enabled = true) { driver =>
      for (delay <- 0 to 2) driver.resetWriteAt(delay)
    }
  }

  it should "c07-satp-control preserve the ordinary CSR flush path between FDI responses" in {
    run("c07-satp-control", enabled = true) { driver =>
      driver.access(0x8b1, operand = 0x1234, stalls = 2)
      driver.satpFlushControl()
      driver.access(0x8b1, function = 2, source = 0, stalls = 2)
      driver.access(0x8b1, operand = 0x5678, stalls = 2)
    }
  }
}
