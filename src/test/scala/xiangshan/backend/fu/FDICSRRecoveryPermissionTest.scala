// SPDX-License-Identifier: MulanPSL-2.0
package xiangshan.backend.fu

import chisel3._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan._

class FDICSRRecoveryPermissionTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDI recovery permission boundaries"

  it should "c07-permission isolate encoded state-enable trust and guest rejection from a held write flush" in {
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
      VerilatorBackendAnnotation, TargetDirAnnotation("c07-permission-enabled"))) { dut =>
      val word = BigInt("f123456789abcdef", 16)
      // Independent observation indices for only the setup writes used below.
      val indices = Map(0xbc4 -> 0, 0xbc5 -> 1, 0xbc6 -> 2, 0x9e2 -> 4, 0x9e3 -> 5, 0x8b1 -> 40)
      val expected = Array.fill[BigInt](52)(0)
      var cycles = 0
      var writes = 0
      var distributions = 0
      var responses = 0
      var expectedWrites = 0
      var denied = 0
      var transactions = 0
      var lastResponseCycle = -1
      var privilege = 3
      var virtualMode = false

      def drive(address: Int, function: Int, rs1: Int, operand: BigInt,
        rob: Int, flag: Boolean, pc: BigInt): Unit = {
        val instruction = (BigInt(address) << 20) | (BigInt(rs1) << 15) |
          (BigInt(function) << 12) | (BigInt(1) << 7) | 0x73
        dut.io.request.bits.instruction.poke(instruction.U)
        dut.io.request.bits.operation.poke((8 | function).U)
        dut.io.request.bits.operand.poke(operand.U)
        dut.io.request.bits.rob.poke(rob.U)
        dut.recovery.requestFlag.poke(flag.B)
        dut.io.request.bits.basePc.poke((pc & ((BigInt(1) << 50) - 1) & ~BigInt(31)).U)
        dut.io.request.bits.offset.poke(((pc & 31) >> 1).U)
      }

      def edge(ordinaryToken: Option[(Int, BigInt)] = None): Unit = {
        val fdiWrite = dut.io.writes.peek().litValue != 0
        // The shared bus also carries ordinary CSR writes. Permit only the
        // explicitly expected setup token; all other pulses require an FDI effect.
        ordinaryToken match {
          case Some((address, value)) =>
            dut.io.writes.expect(0.U)
            dut.observed.bus.w.valid.expect(true.B)
            dut.observed.bus.w.bits.addr.expect(address.U)
            dut.observed.bus.w.bits.data.expect(value.U)
          case None =>
            dut.observed.bus.w.valid.expect(fdiWrite.B)
            if (fdiWrite) { distributions += 1 }
        }
        if (fdiWrite) { writes += 1 }
        if (dut.recovery.responseFire.peek().litToBoolean) {
          responses += 1
          lastResponseCycle = cycles
        }
        dut.clock.step()
        cycles += 1
      }

      def noWrite(): Unit = {
        dut.io.writes.expect(0.U)
        dut.observed.bus.w.valid.expect(false.B)
      }

      def checkBank(): Unit = {
        dut.io.state.zipWithIndex.foreach { case (value, index) => value.expect(expected(index).U) }
        dut.observed.control(11).expect(expected(40).U)
      }

      def reset(): Unit = {
        dut.io.request.valid.poke(false.B)
        drive(0x340, 2, 0, 0, 4, false, 0x1000)
        dut.io.response.ready.poke(true.B)
        dut.io.flush.poke(false.B)
        dut.io.flushRob.poke(4.U)
        dut.io.trap.poke(false.B)
        dut.recovery.flushFlag.poke(false.B)
        dut.recovery.flushAfter.poke(false.B)
        dut.reset.poke(true.B)
        dut.clock.step(5)
        dut.reset.poke(false.B)
        expected.indices.foreach(expected(_) = 0)
        privilege = 3
        virtualMode = false
        edge()
        dut.io.mode.expect(3.U)
        dut.io.virtualMode.expect(false.B)
        dut.io.request.ready.expect(true.B)
        dut.io.response.valid.expect(false.B)
        noWrite()
        checkBank()
      }

      def ordinaryWrite(address: Int, value: BigInt): Unit = {
        dut.io.request.ready.expect(true.B)
        drive(address, 1, 1, value, 8, false, 0x1000)
        dut.io.request.valid.poke(true.B)
        dut.io.response.ready.poke(true.B)
        noWrite()
        edge()
        // Ordinary CSR writes retain their live address through C1, as in the existing mode driver.
        dut.io.request.valid.poke(false.B)
        dut.io.response.valid.expect(true.B)
        dut.io.response.bits.illegal.expect(false.B)
        dut.io.response.bits.virtualIllegal.expect(false.B)
        edge(ordinaryToken = Some(address -> value))
        transactions += 1
        dut.io.response.valid.expect(false.B)
        noWrite()
        checkBank()
      }

      def enterMode(nextPrivilege: Int, nextVirtual: Boolean): Unit = {
        // Architectural MPP/MPV, mepc and mret select the source domain; no state is forced.
        ordinaryWrite(0x300, (BigInt(nextPrivilege) << 11) |
          (if (nextVirtual) BigInt(1) << 39 else BigInt(0)))
        ordinaryWrite(0x341, 0x1000)
        dut.io.request.ready.expect(true.B)
        drive(0x302, 0, 0, 0, 12, false, 0x1000)
        dut.io.request.bits.instruction.poke("h30200073".U)
        dut.io.request.bits.operation.poke(CSROpType.jmp)
        dut.io.request.valid.poke(true.B)
        noWrite()
        edge()
        dut.io.request.valid.poke(false.B)
        dut.io.response.valid.expect(true.B)
        dut.io.response.bits.illegal.expect(false.B)
        dut.io.response.bits.virtualIllegal.expect(false.B)
        noWrite()
        edge()
        transactions += 1
        privilege = nextPrivilege
        virtualMode = nextVirtual
        dut.io.mode.expect(privilege.U)
        dut.io.virtualMode.expect(virtualMode.B)
        dut.io.response.valid.expect(false.B)
        noWrite()
        checkBank()
      }

      def fdi(address: Int, write: Boolean, value: BigInt, illegal: Boolean,
        pc: BigInt = 0x1006, stalls: Int = 0): Unit = {
        val rob = 16 + transactions % 16
        val flag = transactions % 2 != 0
        val old = expected(indices(address))
        val effect = write && !illegal
        val finalWord = if (address == 0xbc4) value & 0x7ff
          else if (address == 0x8b1) value else value & ~BigInt(7)
        dut.io.request.ready.expect(true.B)
        drive(address, if (write) 1 else 2, if (write) 1 else 0, value, rob, flag, pc)
        dut.io.request.valid.poke(true.B)
        dut.io.response.ready.poke((stalls == 0).B)
        dut.io.pc.expect(pc.U)
        noWrite()
        edge()
        // Only accepted FDI transactions are challenged with different unaccepted live inputs.
        dut.io.request.valid.poke((stalls > 0).B)
        if (stalls > 0) { drive(0x8b0, 1, 1, 0x5555, 7, !flag, 0x3002) }
        val responseData = dut.io.response.bits.data.peek().litValue
        def response(): Unit = {
          dut.io.mode.expect(privilege.U)
          dut.io.virtualMode.expect(virtualMode.B)
          dut.io.request.ready.expect(false.B)
          dut.io.response.valid.expect(true.B)
          dut.io.response.bits.illegal.expect(illegal.B)
          dut.io.response.bits.virtualIllegal.expect(false.B)
          dut.io.response.bits.rob.expect(rob.U)
          dut.io.response.bits.data.expect(responseData.U)
          if (!illegal) { dut.io.response.bits.data.expect(old.U) }
          dut.recovery.responseFlag.expect(flag.B)
          dut.recovery.responseFlushPipe.expect(effect.B)
        }
        response()
        dut.recovery.responseFire.expect((stalls == 0).B)
        if (effect) {
          dut.io.writes.expect((BigInt(1) << indices(address)).U)
          dut.observed.bus.w.valid.expect(true.B)
          dut.observed.bus.w.bits.addr.expect(address.U)
          dut.observed.bus.w.bits.data.expect(finalWord.U)
        } else { noWrite(); checkBank() }
        edge()
        if (effect) {
          expectedWrites += 1
          expected(indices(address)) = finalWord
          if (address == 0xbc4) { expected(3) = finalWord & 0x7c2 }
        }
        noWrite()
        checkBank()
        for (_ <- 0 until stalls) {
          response()
          dut.recovery.responseFire.expect(false.B)
          noWrite()
          checkBank()
          edge()
        }
        if (stalls > 0) {
          dut.io.response.ready.poke(true.B)
          response()
          dut.recovery.responseFire.expect(true.B)
          noWrite()
          checkBank()
          edge()
        }
        dut.io.request.valid.poke(false.B)
        transactions += 1
        if (illegal) { denied += 1 }
        dut.io.response.valid.expect(false.B)
        dut.io.request.ready.expect(true.B)
        noWrite()
        checkBank()
        assert(writes == expectedWrites && distributions == expectedWrites)
      }

      case class Scenario(name: String, mode: Int, guest: Boolean, machineC: Boolean,
        supervisorC: Boolean, untrusted: Boolean = false, direct: Boolean = false)
      val scenarios = Seq(
        Scenario("hs-direct-encoded", 1, false, true, true, direct = true),
        Scenario("hs-machine-state-disabled", 1, false, false, true),
        Scenario("hu-machine-state-disabled", 0, false, false, true),
        Scenario("hu-supervisor-state-disabled", 0, false, true, false),
        Scenario("hs-untrusted-source", 1, false, true, true, untrusted = true),
        Scenario("hu-untrusted-source", 0, false, true, true, untrusted = true),
        Scenario("vs-host-fdi", 1, true, true, true),
        Scenario("vu-host-fdi", 0, true, true, true))

      for (scenario <- scenarios) {
        // This wrapper has no D01 observer; each independent scenario starts from its real reset ports.
        reset()
        ordinaryWrite(0x30c, (BigInt(1) << 63) | (if (scenario.machineC) BigInt(1) else BigInt(0)))
        if (scenario.mode == 0) {
          ordinaryWrite(0x10c, if (scenario.supervisorC) BigInt(1) else BigInt(0))
        }
        if (scenario.guest) {
          // H.C=0 would make the generic custom-CSR gate report VI; host FDI must retain II priority.
          ordinaryWrite(0x60c, BigInt(1) << 63)
        }
        if (scenario.untrusted) {
          fdi(0xbc5, true, 0x1000, false)
          fdi(0xbc6, true, if (scenario.mode == 1) 0x1100 else 0x1200, false)
          fdi(0x9e2, true, 0x1000, false)
          fdi(0x9e3, true, if (scenario.mode == 0) 0x1100 else 0x1200, false)
          fdi(0xbc4, true, if (scenario.mode == 1) 1 else 2, false)
        }
        if (!scenario.direct) { fdi(0x8b1, true, word, false) }
        enterMode(scenario.mode, scenario.guest)
        if (scenario.direct) {
          // M.C=1 and sEnable=0 make this HS write legal, including the held response.
          fdi(0x8b1, true, word, false, stalls = 3)
        }
        val previousResponse = lastResponseCycle
        val target = if (scenario.direct) 0xbc4 else 0x8b1
        val sourcePc = if (scenario.untrusted) BigInt(0x1100) else BigInt(0x1006)
        // The boundary source lies outside its own enabled bounds but inside the other domain's bounds.
        for (write <- Seq(true, false)) {
          if (scenario.direct && write) {
            assert(cycles == previousResponse + 1, "A non-FDI transaction or idle cycle separated the direct requests")
            println(s"C07_PERMISSION_DIRECT previousResponse=$previousResponse deniedAcceptance=$cycles gap=1")
          }
          fdi(target, write, if (write) BigInt(0x7ff) else BigInt(0), true, sourcePc, stalls = 3)
          println(s"C07_PERMISSION_DENIED scenario=${scenario.name} privilege=${scenario.mode} " +
            s"guest=${scenario.guest} pc=0x${sourcePc.toString(16)} address=0x${target.toHexString} write=$write " +
            "illegal=true virtualException=false flushPipe=false writes=0 distributions=0 bankUnchanged=true")
        }
      }
      edge()
      assert(denied == 16 && expectedWrites == 18 && transactions == 72)
      assert(writes == expectedWrites && distributions == expectedWrites && responses == transactions)
      noWrite()
      checkBank()
      println(s"C07_PERMISSION_PASS cycles=$cycles enabled=true scenarios=${scenarios.size} denied=$denied " +
        s"writes=$writes distributions=$distributions responses=$responses directPendingBoundary=true")
    }
  }
}
