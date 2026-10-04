// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import freechips.rocketchip.diplomacy.LazyModule
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import xiangshan._

class FDIEcallPolicyTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "The real ECALL policy consumer and trap pipeline"
  private val enabled = sys.env.getOrElse("F05_FDI_ENABLED",
    throw new IllegalArgumentException("F05_FDI_ENABLED must be explicit")).toBoolean
  private val root = Paths.get(sys.props("f05.runRoot")).toRealPath()
  require(Paths.get("").toRealPath() == root)
  private val ecall = BigInt(0x73)
  private val modes = Seq(("M", 3, false, 11), ("HS", 1, false, 9), ("HU", 0, false, 8),
    ("VS", 1, true, 10), ("VU", 0, true, 8))

  private def parameters(): Parameters = {
    val (base, _, _) = top.ArgParser.parse(Array("--config", "FpgaDefaultConfig", "--num-cores", "1",
      "--l2-cache-size", "256", "--l3-cache-size", "768", "--fpga-platform",
      "--disable-always-basic-diff", "--disable-perf", "--disable-alwaysdb"))
    val p = base.alterPartial { case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled) }
    val options = p(DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    require(p(XSCoreParamsKey).HasVPU && p(XSCoreParamsKey).VLEN == 128)
    p
  }
  private def cause(privilege: Int, virtual: Boolean, active: Boolean, tag: Boolean, closed: Boolean): Int = {
    if (!enabled || privilege == 3 || !active) {
      if (privilege == 3) 11 else if (privilege == 1) { if (virtual) 10 else 9 } else 8
    } else if (virtual) 2
    else if (tag && !closed) { if (privilege == 1) 25 else 24 }
    else if (privilege == 1) 9 else 8
  }
  private def zero(data: Data): Unit = data match {
    case b: Bool => b.poke(false.B)
    case u: UInt => u.poke(0.U)
    case s: SInt => s.poke(0.S)
    case v: Vec[_] => v.foreach(zero)
    case r: Record => r.elements.values.foreach(zero)
    case other => throw new IllegalArgumentException(other.getClass.getName)
  }

  private class Driver(val io: FDIEcallPolicyIO, tick: () => Unit, resetPin: Boolean => Unit,
    val exu: Boolean)(implicit p: Parameters) {
    var cycles = 0
    var transactions = 0
    var traps = 0
    var hardwareEffects = 0
    var softwareEffects = 0
    var transportEvents = 0
    var faultRecords = 0
    var accepted = 0
    var returned = 0
    var resetting = true
    var noEcallEffects = false
    val events = mutable.ArrayBuffer.empty[String]
    private case class ExpectedFault(word: BigInt, operation: Int, flag: Boolean, ftq: Int, offset: Int)
    private var expectedFault: Option[ExpectedFault] = None
    private def bool(b: Bool): Boolean = b.peek().litToBoolean
    private def value(u: UInt): BigInt = u.peek().litValue
    def raw(address: Int): BigInt = if (FDICSRTestAddresses.all.contains(address))
      value(io.state(FDICSRTestAddresses.all.indexOf(address)))
      else value(io.nativeState(FDIEcallPolicyAddresses.native.indexOf(address)))
    def mark(name: String): Unit = events += s"{\"cycle\":$cycles,\"event\":\"case\",\"name\":\"$name\"}"
    def edge(): Unit = {
      if (!resetting) {
        io.wrapperFire.expect(bool(io.bankFire).B)
        if (exu) io.resetClockEnable.expect(false.B)
        if (bool(io.request.valid) && bool(io.request.ready)) accepted += 1
        if (bool(io.response.valid) && bool(io.response.ready)) returned += 1
        if (bool(io.hardwareReason)) {
          hardwareEffects += 1
          events += s"{\"cycle\":$cycles,\"event\":\"reason-effect\",\"reason\":${value(io.hardwareReasonValue)}}"
        }
        if (bool(io.softwareReason)) softwareEffects += 1
        if (bool(io.transportedTrap)) transportEvents += 1
        if (noEcallEffects) {
          io.writes.expect(0.U)
          io.softwareReason.expect(false.B)
          io.hardwareReason.expect(false.B)
          io.fdiDistribution.expect(false.B)
          io.distribution.w.valid.expect(false.B)
        }
        if (bool(io.faultInstruction.valid)) {
          faultRecords += 1
          expectedFault.foreach { fault =>
            // Decode only the literal source instruction fields, independently of Imm_Z.
            val immediate = (((fault.word >> 7) & 31) << 17) |
              (((fault.word >> 15) & 31) << 12) | ((fault.word >> 20) & 0xfff)
            io.faultInstruction.bits.operation.expect(fault.operation.U)
            io.faultInstruction.bits.imm.expect(immediate.U)
            io.faultInstruction.bits.ftq.flag.expect(fault.flag.B)
            io.faultInstruction.bits.ftq.value.expect(fault.ftq.U)
            io.faultInstruction.bits.offset.expect(fault.offset.U)
          }
        }
      }
      tick()
      cycles += 1
    }
    def idle(count: Int): Unit = for (_ <- 0 until count) edge()
    def reset(): Unit = {
      resetting = true
      noEcallEffects = false
      expectedFault = None
      zero(io.request.bits); io.request.valid.poke(false.B)
      io.response.ready.poke(true.B)
      zero(io.redirect); zero(io.trap)
      io.trap.bits.trigger.poke(TriggerAction.None)
      io.useSelected.poke(false.B); io.trackResponse.poke(false.B); zero(io.competitor)
      io.nmiSource.poke(false.B); io.acceptNmi.poke(false.B); io.useSavedNmi.poke(false.B)
      io.monitorReset.poke(true.B)
      resetPin(true)
      edge()
      io.monitorReset.poke(false.B)
      idle(4)
      if (exu) io.gatedResetEdges.expect(4.U)
      resetPin(false)
      resetting = false
      edge()
      io.mode.expect(3.U); io.virtualMode.expect(false.B)
      io.response.valid.expect(false.B)
      io.savedInstruction.valid.expect(false.B)
    }
    def drive(word: BigInt, operation: Int = 16, operand: BigInt = 0, tag: Boolean = false,
      rob: Int = 12, flag: Boolean = false, ftq: Int = 7, offset: Int = 3, basePc: BigInt = 0x80001200L): Unit = {
      io.request.bits.instruction.poke(word.U)
      io.request.bits.operation.poke(operation.U)
      io.request.bits.operand.poke(operand.U)
      io.request.bits.notTrusted.poke(tag.B)
      io.request.bits.rob.value.poke(rob.U)
      io.request.bits.rob.flag.poke(flag.B)
      io.request.bits.ftq.value.poke(ftq.U)
      io.request.bits.ftq.flag.poke(flag.B)
      io.request.bits.offset.poke(offset.U)
      io.request.bits.basePc.poke(basePc.U)
      io.request.bits.pdest.poke(5.U)
    }
    def checkResponse(vector: BigInt, rob: Int, flag: Boolean, pdest: Int = 5): Unit = {
      io.response.valid.expect(true.B)
      io.response.bits.vector.expect(vector.U)
      io.response.bits.rob.value.expect(rob.U); io.response.bits.rob.flag.expect(flag.B)
      io.response.bits.pdest.expect(pdest.U)
      if ((vector & ((BigInt(1) << 24) | (BigInt(1) << 25))) != 0) {
        io.response.bits.tval.expect(0.U); io.response.bits.reason.expect(1.U)
      }
    }
    def waitResponse(): Unit = {
      var waited = 0
      while (!bool(io.response.valid) && waited < 20) { edge(); waited += 1 }
      io.response.valid.expect(true.B)
    }
    def transact(word: BigInt, operation: Int, operand: BigInt = 0, tag: Boolean = false,
      vector: BigInt = 0, stalls: Int = 0, poison: Boolean = false,
      rob: Int = 12, flag: Boolean = false, ftq: Int = 7, offset: Int = 3,
      basePc: BigInt = 0x80001200L, track: Boolean = false): BigInt = {
      io.request.ready.expect(true.B)
      val before = returned
      drive(word, operation, operand, tag, rob, flag, ftq, offset, basePc)
      expectedFault = if (vector == 4) Some(ExpectedFault(word, operation, flag, ftq, offset)) else None
      io.trackResponse.poke(track.B)
      io.request.valid.poke(true.B)
      io.response.ready.poke(false.B)
      edge()
      transactions += 1
      io.request.valid.poke(false.B)
      if (!exu) io.response.valid.expect(true.B)
      waitResponse()
      checkResponse(vector, rob, flag)
      val result = value(io.response.bits.data)
      if (word == ecall) io.response.bits.flushPipe.expect(false.B)
      for (index <- 0 until stalls) {
        if (poison) {
          drive(if (index % 2 == 0) BigInt("00100073", 16) else ecall,
            tag = !tag, rob = (rob + 1) % p(XSCoreParamsKey).RobSize,
            flag = !flag, ftq = (ftq + 1) % p(XSCoreParamsKey).FtqSize, offset = (offset + 1) % 16,
            basePc = 0x90000000L + index * 64)
          io.request.valid.poke(true.B)
          io.request.ready.expect(false.B)
        }
        checkResponse(vector, rob, flag)
        io.response.bits.data.expect(result.U)
        edge()
      }
      io.request.valid.poke(false.B)
      io.response.ready.poke(true.B)
      checkResponse(vector, rob, flag)
      edge()
      io.trackResponse.poke(false.B)
      io.response.valid.expect(false.B)
      assert(returned == before + 1)
      if (vector == 4) {
        io.savedInstruction.valid.expect(true.B)
        io.savedInstruction.bits.expect(word.U)
        io.savedFtq.flag.expect(flag.B); io.savedFtq.value.expect(ftq.U); io.savedOffset.expect(offset.U)
      }
      events += s"{\"cycle\":$cycles,\"event\":\"response\",\"instruction\":$word," +
        s"\"pc\":${basePc + 2 * offset},\"rob\":$rob,\"rob_flag\":$flag,\"ftq\":$ftq," +
        s"\"ftq_flag\":$flag,\"offset\":$offset,\"not_trusted\":$tag,\"vector\":$vector}"
      result
    }
    def write(address: Int, data: BigInt): Unit = {
      val word = (BigInt(address) << 20) | (1 << 15) | (1 << 12) | (1 << 7) | 0x73
      transact(word, 9, data)
    }
    def setup(cfg: Int = 0, delegated: BigInt = 0): Unit = {
      write(0x300, 0)
      write(0x305, 0x80000100L); write(0x105, 0x80000200L)
      write(0x302, delegated); write(0x602, 0)
      if (enabled) {
        write(0x30c, (BigInt(1) << 63) | 1)
        write(0x60c, (BigInt(1) << 63) | 1)
        write(0x10c, 1)
        write(0x8b0, 0x12345678); write(0x8b1, 0x22345678); write(0x8b2, 0x32345678); write(0x8b3, 7)
        write(0xbc5, 0x80000000L); write(0xbc6, 0x80010000L)
        write(0x9e2, 0x80000000L); write(0x9e3, 0x80010000L)
        write(0xbc4, cfg)
      }
      idle(3)
    }
    def mode(privilege: Int, virtual: Boolean = false, extra: BigInt = 0): Unit = {
      io.mode.expect(3.U); io.virtualMode.expect(false.B)
      write(0x300, (BigInt(privilege) << 11) | (if (virtual) BigInt(1) << 39 else BigInt(0)) | extra)
      write(0x341, 0x80001206L)
      transact(BigInt("30200073", 16), 16)
      io.mode.expect(privilege.U); io.virtualMode.expect(virtual.B)
      idle(2)
    }
    def snapshot(): Seq[BigInt] = io.state.map(value)
    def unchanged(before: Seq[BigInt]): Unit = io.state.zip(before).foreach { case (actual, old) => actual.expect(old.U) }
    def ecallAccess(expected: Int, tag: Boolean, stalls: Int = 0, track: Boolean = false,
      rob: Int = 12, flag: Boolean = false, ftq: Int = 7, offset: Int = 3): Unit = {
      val before = snapshot()
      val effects = hardwareEffects
      noEcallEffects = true
      transact(ecall, 16, tag = tag, vector = BigInt(1) << expected, stalls = stalls, poison = stalls > 0,
        track = track, rob = rob, flag = flag, ftq = ftq, offset = offset)
      unchanged(before)
      assert(hardwareEffects == effects)
      noEcallEffects = false
    }
    def redirect(rob: Int, flag: Boolean, itself: Boolean,
      ftq: Int = 7, ftqFlag: Boolean = false, offset: Int = 3): Unit = {
      zero(io.redirect.bits)
      io.redirect.bits.robIdx.value.poke(rob.U); io.redirect.bits.robIdx.flag.poke(flag.B)
      io.redirect.bits.ftqIdx.value.poke(ftq.U)
      io.redirect.bits.ftqIdx.flag.poke(ftqFlag.B)
      io.redirect.bits.ftqOffset.poke(offset.U)
      io.redirect.bits.level.poke((if (itself) 1 else 0).U)
      io.redirect.valid.poke(true.B)
      edge()
      io.redirect.valid.poke(false.B)
    }
    def waitSelected(vector: BigInt, rob: Int, flag: Boolean = false): Unit = {
      var waited = 0
      while (!bool(io.selected.valid) && waited < 12) { edge(); waited += 1 }
      io.selected.valid.expect(true.B)
      io.selected.bits.vector.expect(vector.U)
      io.selected.bits.rob.value.expect(rob.U); io.selected.bits.rob.flag.expect(flag.B)
    }
    def acceptTrap(pc: BigInt, expectedCause: Int, delegated: Boolean, tval: BigInt,
      newReason: Boolean, debug: Boolean = false, nmi: Boolean = false,
      expectState: Boolean = true, instruction: BigInt = ecall): Unit = {
      val oldReason = if (enabled) raw(0x8b3) else BigInt(0)
      val beforeEffects = hardwareEffects
      val beforeTransport = transportEvents
      zero(io.trap.bits)
      io.trap.bits.pc.poke(pc.U)
      io.trap.bits.instr.poke(instruction.U)
      io.trap.bits.trigger.poke(TriggerAction.None)
      io.trap.bits.singleStep.poke(debug.B)
      io.trap.bits.isInterrupt.poke(nmi.B)
      io.useSavedNmi.poke(nmi.B)
      io.useSelected.poke(true.B)
      io.trap.valid.poke(true.B)
      edge()
      traps += 1
      io.trap.valid.poke(false.B)
      io.trap.bits.pc.poke(0x90007776L.U)
      io.trap.bits.instr.poke(BigInt("00100073", 16).U)
      idle(5)
      io.useSavedNmi.poke(false.B)
      io.useSelected.poke(false.B)
      assert(transportEvents == beforeTransport + 1)
      assert(hardwareEffects == beforeEffects + (if (newReason) 1 else 0))
      assert(raw(0x8b3) == (if (newReason) BigInt(1) else oldReason))
      if (expectState) {
        val epc = if (delegated) 0x141 else 0x341
        assert(raw(epc) == pc && raw(epc + 1) == expectedCause && raw(epc + 2) == tval)
        if (expectedCause == 2) {
          assert(raw(0x34a) == 0 && raw(0x64a) == 0)
          io.savedInstruction.valid.expect(false.B)
        }
      }
      events += s"{\"cycle\":$cycles,\"event\":\"trap-effect\",\"source_pc\":$pc," +
        s"\"instruction\":$instruction,\"cause\":$expectedCause,\"delegated\":$delegated," +
        s"\"tval\":$tval,\"reason_effect\":$newReason}"
      idle(3)
      assert(hardwareEffects == beforeEffects + (if (newReason) 1 else 0))
    }
    def cancelHeldGuestEcall(): Unit = {
      io.mode.expect(1.U); io.virtualMode.expect(true.B)
      val beforeState = snapshot()
      val beforeReturned = returned
      val beforeFaults = faultRecords
      val beforeEffects = hardwareEffects
      drive(ecall, tag = true, rob = 12, flag = true, ftq = 9, offset = 5)
      expectedFault = Some(ExpectedFault(ecall, 16, true, 9, 5))
      io.trackResponse.poke(true.B)
      io.response.ready.poke(false.B)
      io.request.valid.poke(true.B)
      noEcallEffects = true
      edge()
      transactions += 1
      io.request.valid.poke(false.B)
      waitResponse()
      for (_ <- 0 until 3) { checkResponse(4, 12, true); edge() }
      assert(faultRecords > beforeFaults)
      io.savedInstruction.valid.expect(true.B); io.savedInstruction.bits.expect(ecall.U)
      io.savedFtq.flag.expect(true.B); io.savedFtq.value.expect(9.U); io.savedOffset.expect(5.U)
      // The older instruction shares this FTQ entry. Its smaller offset satisfies
      // TrapInstInfo.needFlush; matching ROB cancellation alone is not that protocol.
      redirect(11, true, itself = false, ftq = 9, ftqFlag = true, offset = 3)
      io.response.valid.expect(false.B)
      io.savedInstruction.valid.expect(false.B)
      io.trackResponse.poke(false.B)
      io.response.ready.poke(true.B)
      idle(4)
      io.response.valid.expect(false.B)
      io.savedInstruction.valid.expect(false.B)
      io.selected.valid.expect(false.B)
      io.mode.expect(1.U); io.virtualMode.expect(true.B)
      assert(returned == beforeReturned && hardwareEffects == beforeEffects)
      unchanged(beforeState)
      noEcallEffects = false
      events += s"{\"cycle\":$cycles,\"event\":\"guest-ii-cancel-cleared\"," +
        "\"instruction\":115,\"rob\":12,\"rob_flag\":true,\"ftq\":9,\"ftq_flag\":true," +
        "\"offset\":5,\"redirect_offset\":3,\"response_fires\":0,\"trap_inst_valid\":false}"
    }
    def claimNmi(): Unit = {
      io.nmiSource.poke(true.B); edge(); io.nmiSource.poke(false.B)
      var waited = 0
      while (!bool(io.nmiCandidate) && waited < 12) { edge(); waited += 1 }
      io.nmiCandidate.expect(true.B)
      io.acceptNmi.poke(true.B); edge(); io.acceptNmi.poke(false.B)
    }
    def save(name: String): Unit = Files.write(root.resolve(name),
      events.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
  }

  it should "classify actual ECALL inputs and preserve their single-slot response identity" in {
    implicit val p: Parameters = parameters()
    test(new FDIEcallPolicyHarness).withAnnotations(Seq(VerilatorBackendAnnotation,
      TargetDirAnnotation(s"ecall-wrapper-${if (enabled) "on" else "off"}"))) { dut =>
      val d = new Driver(dut.io, () => dut.clock.step(), value => dut.reset.poke(value.B), exu = false)
      try {
        for ((name, privilege, virtual, _) <- modes; active <- Seq(false, true);
          tag <- Seq(false, true); closed <- Seq(false, true)) {
          d.mark(s"matrix-$name-enable-$active-tag-$tag-close-$closed")
          d.reset()
          val enableBit = if (privilege == 1) 0 else 1
          val closeBit = if (privilege == 1) 2 else 6
          val cfg = (if (active) 1 << enableBit else 0) | (if (closed) 1 << closeBit else 0)
          d.setup(cfg)
          if (privilege != 3) d.mode(privilege, virtual)
          d.ecallAccess(cause(privilege, virtual, active, tag, closed), tag, stalls = if (tag) 3 else 0)
        }
        for (privilege <- Seq(0, 1); nuisance <- Seq(1 << 10, 0x3b8, if (privilege == 0) 5 else 0x42)) {
          d.mark(s"unrelated-config-$privilege-$nuisance")
          d.reset(); d.setup((if (privilege == 0) 2 else 1) | nuisance)
          d.mode(privilege)
          val effectiveClosed = (nuisance & (if (privilege == 0) 0x40 else 4)) != 0
          d.ecallAccess(cause(privilege, false, true, true, effectiveClosed), tag = true)
        }
        for (machine <- Seq(false, true); supervisor <- Seq(false, true); privilege <- Seq(0, 1)) {
          d.mark(s"stateen-does-not-disable-$machine-$supervisor-$privilege")
          d.reset(); d.setup(if (privilege == 0) 2 else 1)
          if (enabled) {
            d.write(0x30c, (BigInt(1) << 63) | (if (machine) 1 else 0))
            d.write(0x10c, if (supervisor) 1 else 0)
          }
          d.mode(privilege)
          d.ecallAccess(cause(privilege, false, true, true, false), tag = true)
        }
        d.mark("machine-data-translation-is-not-source")
        d.reset(); d.setup(3)
        d.write(0x300, (BigInt(1) << 17) | (BigInt(1) << 39))
        d.io.mode.expect(3.U); d.io.virtualMode.expect(false.B)
        d.ecallAccess(11, tag = true)
        d.mark("sret-establishes-real-user-source")
        d.reset(); d.setup(2); d.mode(1)
        d.write(0x100, 0); d.write(0x141, 0x80001206L)
        d.transact(BigInt("10200073", 16), 16)
        dut.io.mode.expect(0.U); dut.io.virtualMode.expect(false.B)
        d.ecallAccess(cause(0, false, true, true, false), tag = true)
        d.mark("continuous-tag-transactions-and-non-ecall")
        d.reset(); d.setup(2); d.mode(0)
        for (tag <- Seq(true, false, true, false)) d.ecallAccess(cause(0, false, true, tag, false), tag)
        d.transact(BigInt("00100073", 16), 16, vector = BigInt(1) << 3)
        d.ecallAccess(cause(0, false, true, true, false), tag = true)
        d.transact(BigInt("7ff020f3", 16), 10, vector = BigInt(1) << 2)
        d.ecallAccess(cause(0, false, true, false, false), tag = false)

        for ((label, redirectRob, redirectFlag, itself, killed) <- Seq(
          ("same", 12, false, true, true), ("older", 11, false, false, true),
          ("younger", 13, false, true, false), ("same-after", 12, false, false, false),
          ("wrap-older", p(XSCoreParamsKey).RobSize - 1, false, false, true))) {
          d.mark(s"held-response-redirect-$label")
          d.reset(); d.setup(2); d.mode(0)
          val flag = label == "wrap-older"
          val rob = if (flag) 0 else 12
          val vector = BigInt(1) << cause(0, false, true, true, false)
          val prior = d.snapshot()
          d.noEcallEffects = true
          d.drive(ecall, tag = true, rob = rob, flag = flag)
          dut.io.request.valid.poke(true.B); dut.io.response.ready.poke(false.B)
          d.edge(); dut.io.request.valid.poke(false.B)
          d.waitResponse(); d.checkResponse(vector, rob, flag)
          d.redirect(redirectRob, redirectFlag, itself)
          if (killed) {
            dut.io.response.valid.expect(false.B)
            d.idle(3)
          } else {
            d.checkResponse(vector, rob, flag)
            dut.io.response.ready.poke(true.B); d.edge()
          }
          d.unchanged(prior)
          d.noEcallEffects = false
          dut.io.response.ready.poke(true.B)
          d.ecallAccess(cause(0, false, true, false, false), tag = false)
        }
        d.mark("invalid-live-input-and-reset-no-late-response")
        d.reset(); d.setup(2); d.mode(0)
        d.drive(ecall, tag = true); dut.io.request.valid.poke(false.B)
        d.idle(4); dut.io.response.valid.expect(false.B)
        dut.io.request.valid.poke(true.B); dut.io.response.ready.poke(false.B); d.edge()
        dut.io.request.valid.poke(false.B); d.waitResponse(); d.reset()
        d.idle(4); dut.io.response.valid.expect(false.B)
        d.ecallAccess(11, tag = true)
        d.mark("c0-matching-flush-cancels-without-late-ecall")
        d.reset(); d.setup(2); d.mode(0)
        val beforeC0 = d.snapshot()
        d.drive(ecall, tag = true)
        dut.io.request.valid.poke(true.B)
        d.noEcallEffects = true
        d.redirect(12, false, itself = true)
        dut.io.request.valid.poke(false.B)
        dut.io.response.valid.expect(false.B)
        d.idle(4); d.unchanged(beforeC0)
        d.noEcallEffects = false
        d.ecallAccess(cause(0, false, true, false, false), tag = false)
        println(s"F05_ECALL_WRAPPER_PASS enabled=$enabled cycles=${d.cycles} transactions=${d.transactions}")
      } finally d.save(s"wrapper-${if (enabled) "on" else "off"}-events.jsonl")
    }
  }

  it should "transport actual ECALL exceptions through the production selector and trap entry" in {
    implicit val p: Parameters = parameters()
    test(LazyModule(new FDIEcallPolicyTrapHarness).module).withAnnotations(Seq(VerilatorBackendAnnotation,
      TargetDirAnnotation(s"ecall-trap-${if (enabled) "on" else "off"}"))) { dut =>
      val d = new Driver(dut.io, () => dut.clock.step(), value => dut.reset.poke(value.B), exu = true)
      try {
        for ((name, privilege, virtual, normalCause) <- modes; active <- Seq(false, true);
          delegated <- Seq(false, true) if privilege != 3 || !delegated) {
          d.mark(s"trap-$name-enable-$active-delegated-$delegated")
          d.reset()
          val expected = cause(privilege, virtual, active, true, false)
          val cfg = if (!active) 0 else if (privilege == 1) 1 else 2
          d.setup(cfg, if (delegated) BigInt(1) << expected else BigInt(0))
          if (privilege != 3) d.mode(privilege, virtual)
          val ftq = if (virtual) p(XSCoreParamsKey).FtqSize - 1 else 7
          d.ecallAccess(expected, tag = true, stalls = 4, track = true, flag = virtual, ftq = ftq, offset = 5)
          d.waitSelected(BigInt(1) << expected, 12, virtual)
          if (expected == 2) {
            dut.io.savedInstruction.valid.expect(true.B); dut.io.savedInstruction.bits.expect(ecall.U)
            dut.io.savedFtq.flag.expect(virtual.B); dut.io.savedFtq.value.expect(ftq.U); dut.io.savedOffset.expect(5.U)
          }
          d.acceptTrap(0x8000120aL, expected, delegated, if (expected == 2) ecall else BigInt(0),
            newReason = expected == 24 || expected == 25)
        }
        if (enabled) {
          d.mark("guest-ii-cancel-and-continuous-distinct-trap-instructions")
          d.reset(); d.setup(1, BigInt(1) << 2); d.mode(1, virtual = true)
          d.cancelHeldGuestEcall()
          val reasonBeforeGuestSequence = d.hardwareEffects
          // Machine-level CSR reads are ordinary II in VS. No reset or mode
          // override separates the cancelled ECALL from these distinct faults.
          for ((word, rob, ftq, offset, pc) <- Seq(
            (BigInt("300021f3", 16), 13, 10, 7, BigInt("80002000", 16)),
            (BigInt("30502273", 16), 14, 11, 1, BigInt("80003000", 16)))) {
            dut.io.mode.expect(1.U); dut.io.virtualMode.expect(true.B)
            dut.io.savedInstruction.valid.expect(false.B)
            val before = d.snapshot()
            d.noEcallEffects = true
            d.transact(word, 10, vector = 4, stalls = 3, poison = true,
              rob = rob, flag = true, ftq = ftq, offset = offset, basePc = pc, track = true)
            d.unchanged(before)
            d.noEcallEffects = false
            d.waitSelected(4, rob, flag = true)
            dut.io.savedInstruction.bits.expect(word.U)
            dut.io.savedFtq.flag.expect(true.B); dut.io.savedFtq.value.expect(ftq.U)
            dut.io.savedOffset.expect(offset.U)
            d.acceptTrap(pc + 2 * offset, 2, delegated = true, tval = word,
              newReason = false, instruction = word)
            dut.io.savedInstruction.valid.expect(false.B)
            dut.io.mode.expect(1.U); dut.io.virtualMode.expect(false.B)
            assert(d.raw(0x8b3) == 7 && d.hardwareEffects == reasonBeforeGuestSequence)
            if (rob == 13) {
              // The HS trap saved SPV/SPP; real SRET returns to the original VS.
              d.transact(BigInt("10200073", 16), 16)
              dut.io.mode.expect(1.U); dut.io.virtualMode.expect(true.B)
              dut.io.savedInstruction.valid.expect(false.B)
            }
          }

          d.mark("selected-denial-cancelled-before-trap")
          d.reset(); d.setup(2); d.mode(0)
          d.ecallAccess(24, tag = true, track = true)
          d.waitSelected(BigInt(1) << 24, 12)
          val before = d.hardwareEffects
          d.redirect(12, false, itself = true); d.idle(6)
          dut.io.selected.valid.expect(false.B); assert(d.hardwareEffects == before && d.raw(0x8b3) == 7)

          d.mark("older-standard-error-wins-without-reason-effect")
          d.reset(); d.setup(2); d.mode(0)
          dut.io.competitor.bits.rob.value.poke(10.U); dut.io.competitor.bits.rob.flag.poke(false.B)
          dut.io.competitor.bits.vector.poke((BigInt(1) << 13).U); dut.io.competitor.valid.poke(true.B)
          d.edge(); dut.io.competitor.valid.poke(false.B)
          d.ecallAccess(24, tag = true, track = true)
          d.waitSelected(BigInt(1) << 13, 10)
          d.acceptTrap(0x80001004L, 13, delegated = false, 0, newReason = false)

          d.mark("debug-claims-actual-ecall-candidate")
          d.reset(); d.setup(2); d.mode(0)
          d.ecallAccess(24, tag = true, track = true); d.waitSelected(BigInt(1) << 24, 12)
          d.acceptTrap(0x80001206L, 24, delegated = false, 0, newReason = false, debug = true, expectState = false)
          dut.io.debugMode.expect(true.B)

          d.mark("nmi-claims-actual-ecall-candidate")
          d.reset(); d.setup(2); d.mode(0)
          d.ecallAccess(24, tag = true, track = true); d.waitSelected(BigInt(1) << 24, 12)
          d.claimNmi()
          d.acceptTrap(0x80001206L, 31, delegated = false, 0, newReason = false, nmi = true, expectState = false)
          assert(d.raw(0x741) == 0x80001206L && d.raw(0x742) == ((BigInt(1) << 63) | 31))

          d.mark("supervisor-double-trap-changes-final-cause")
          d.reset(); d.setup(1, BigInt(1) << 25)
          d.write(0x30a, BigInt(1) << 59); d.mode(1)
          d.write(0x100, BigInt(1) << 24)
          d.ecallAccess(25, tag = true, track = true); d.waitSelected(BigInt(1) << 25, 12)
          d.acceptTrap(0x80001206L, 16, delegated = false, 0, newReason = false)
          assert(d.raw(0x34b) == 25)
        }
        println(s"F05_ECALL_TRAP_PASS enabled=$enabled cycles=${d.cycles} traps=${d.traps} " +
          s"hardwareEffects=${d.hardwareEffects} selector=production exu=production trapInst=production")
      } finally d.save(s"trap-${if (enabled) "on" else "off"}-events.jsonl")
    }
  }
}
