// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.simulator.PeekPokeAPI._
import io.circe.Json

object F02ControlPrograms {
  private val mask64 = (BigInt(1) << 64) - 1
  private val sentinel = BigInt("5a937bd1", 16)
  private val originalReturn = BigInt(0x7700)
  private val initial = Seq(0x8b0 -> BigInt(0x7800), 0x8b1 -> originalReturn,
    0x8b2 -> BigInt(0x7900), 0x8b3 -> BigInt(6))
  private def j(offset: Int): BigInt = {
    require(offset % 2 == 0 && offset >= -(1 << 22) && offset < (1 << 22))
    val imm = BigInt(offset) & ((BigInt(1) << 23) - 1)
    (((imm >> 22) & 1) << 31) | (((imm >> 21) & 1) << 7) |
      (((imm >> 15) & 63) << 15) | (((imm >> 11) & 15) << 8) |
      (((imm >> 1) & 1023) << 21) | 0x0b
  }
  private def jr(rd: Int, rs: Int, offset: Int): BigInt =
    (BigInt(offset & 0xfff) << 20) | (BigInt(rs) << 15) | (BigInt(rd) << 7) | 0x100b
  private def call(word: BigInt, target: BigInt, tag: Boolean = false,
    predicted: Boolean = true): FDIControlInstruction =
    FDIControlInstruction(word, tag = tag, branchType = if ((word & 0x7000) == 0) 2 else 3,
      predictedTaken = predicted, predictedTarget = Some(target))
  private def checkCall(d: FDIFullControlDriver, source: FDIControlSource, target: BigInt,
    rd: Int, allowed: Boolean): Unit = {
    if (allowed) {
      val link = (source.pc + 4) & mask64
      d.expectRetired(source, if (rd == 0) None else Some(link))
      if (rd == 0) d.expectNoWrite(source)
      assert(source.outputs.size == 1 && source.outputs.head.target == target)
      assert(source.outputs.head.exception == 0 && source.outputs.head.redirect)
      assert(source.callCycles.size == 1 && source.callCycles.head < source.retired)
      assert(source.waitForward && source.blockBackward)
      d.expectSpecial(0x8b1, link)
      assert(source.writebacks.nonEmpty, "The call must complete the real WB path")
    } else {
      // The selected shared-reference contract retains the original illegal
      // instruction in TVAL, including runtime source-permission rejection.
      d.expectTrap(source, 2, source.instruction.bits,
        reason = if (d.enabled) 6 else 0)
      d.expectNoWrite(source)
      assert(source.callCycles.isEmpty && source.outputs.forall(out => !out.callEvent && !out.redirect && !out.rfWen))
      if (d.enabled) d.expectSpecial(0x8b1, originalReturn)
    }
    if (d.enabled) {
      d.expectSpecial(0x8b0, 0x7800); d.expectSpecial(0x8b2, 0x7900); d.expectSpecial(0x8b3, 6)
    }
  }

  private def rawInstructionOwnership(d: FDIFullControlDriver): Unit = {
    d.caseName = "dynamic-ii-consecutive-calls-without-reset"
    d.setup(mainCfg = 1, csrs = initial, registers = Seq(9 -> BigInt(0x5001)))
    val first = d.execute(0x1806, call(j(64), 0x1846))
    checkCall(d, first, 0x1846, 1, allowed = false)
    // Resume through architectural MRET so a stale first raw word cannot be
    // hidden by a test reset before the second independent dynamic failure.
    d.writeCsr(0x300, 0)
    d.writeCsr(0x341, 0x1906)
    d.expectRetired(d.execute(0x8100, FDIControlInstruction(BigInt("30200073", 16))))
    d.dut.io.mode.expect(0.U)
    val second = d.execute(0x1906, call(jr(10, 9, 5), 0x5006))
    checkCall(d, second, 0x5006, 10, allowed = false)
    assert(first.instruction.bits != second.instruction.bits)
    d.record("f02-consecutive-dynamic-ii", "first_raw" -> Json.fromBigInt(first.instruction.bits),
      "second_raw" -> Json.fromBigInt(second.instruction.bits))

    for (decodeOlder <- Seq(false, true)) {
      d.caseName = s"dynamic-ii-and-decode-ii-older-$decodeOlder"
      d.setup(mainCfg = 1, csrs = initial)
      val dynamic = call(j(64), if (decodeOlder) 0x1848 else 0x1844)
      val decoded = FDIControlInstruction(BigInt("ffffffff", 16))
      val words = if (decodeOlder) Seq(decoded, dynamic) else Seq(dynamic, decoded)
      val pair = d.issueBatch(0x1804, words)
      d.until("oldest decode or dynamic II reaches the real trap")(pair.head.trap.nonEmpty)
      d.idle(24)
      d.expectTrap(pair.head, 2, pair.head.instruction.bits, reason = 6)
      pair.foreach(d.expectNoWrite)
      assert(pair(1).retired < 0 && pair(1).trap.isEmpty && pair.forall(_.callCycles.isEmpty))
      d.expectSpecial(0x8b1, originalReturn)
      d.record("f02-dynamic-decode-age", "decode_older" -> Json.fromBoolean(decodeOlder),
        "selected_raw" -> Json.fromBigInt(pair.head.instruction.bits))
    }

    // Not-taken predictions admit two ordinary guest branches together. Both
    // actually take, producing separate runtime II payloads without the
    // serial FDICALL constraint or an invalid predicted-taken fetch packet.
    val guestBranch = FDIControlInstruction(BigInt("04000063", 16), tag = true, branchType = 1)
    val guestBranch2 = FDIControlInstruction(BigInt("08000063", 16), tag = true, branchType = 1)
    // A read of the privileged mcycle CSR is an ordinary runtime CSR II. Its
    // read-only decode removes the serial waits, allowing an actual FU race.
    val csrFault = FDIControlInstruction(BigInt("b0002573", 16), tag = true)
    val csrDestinationSentinel = BigInt("316f5a29", 16)
    for (order <- Seq("dynamic-dynamic", "dynamic-csr", "csr-dynamic")) {
      d.caseName = s"raw-ii-age-$order"
      d.setup(virtual = true, mainCfg = 2, csrs = initial,
        registers = Seq(10 -> csrDestinationSentinel))
      val words = order match {
        case "dynamic-dynamic" => Seq(guestBranch, guestBranch2)
        case "dynamic-csr" => Seq(guestBranch, csrFault)
        case "csr-dynamic" => Seq(csrFault, guestBranch)
      }
      val pair = d.issueBatch(0x1804, words)
      d.until("oldest runtime II reaches the real trap")(pair.head.trap.nonEmpty)
      d.idle(24)
      d.expectTrap(pair.head, 2, pair.head.instruction.bits, reason = 6)
      pair.foreach { source =>
        assert(source.allocated >= 0, "Both raw instruction owners must reach the actual ROB")
        val arrivals = source.exceptionIngress.filter(_.eligible).toSeq
        assert(arrivals.nonEmpty && arrivals.forall(arrival => arrival.exception == 4 &&
          arrival.cycle < pair.head.trap.get.cycle),
          "Both II sources must reach the native age selector before the precise trap")
        if (source.instruction.bits == csrFault.bits) {
          assert(source.csrRequests.nonEmpty, "The CSR fault must execute through the real CSR request path")
          assert(source.csrResults.size == 1 && source.csrResults.head.exception == 4 &&
            source.csrResults.head.pdest == source.pdest,
            "The real CSR response must report II for this exact physical destination and ROB owner")
          assert(source.csrWriteRequests.isEmpty, "The mcycle read must not request a software CSR write")
          assert(arrivals.forall(_.raw.isEmpty), "An ordinary CSR fault must not acquire a dynamic control-flow raw word")
          // Existing ordinary CSR reads can write a speculative physical
          // destination before their precise exception. Recovery must discard
          // that mapping; the counter itself continues its natural counting.
          assert(source.canceled >= 0 && source.retired < 0 && source.writeCycles.forall(_ <= source.canceled),
            "A faulting or canceled CSR cannot retire or write after cancellation")
        } else {
          d.expectNoWrite(source)
          assert(source.outputs.nonEmpty && source.outputs.forall(output => output.exception == 4 &&
            !output.rfWen && !output.redirect && !output.callEvent),
            "The competing dynamic II must be produced by a real BranchUnit")
          assert(arrivals.forall(_.raw.contains(source.instruction.bits)),
            "Dynamic branch raw metadata must retain the original source instruction")
        }
      }
      assert(pair(1).retired < 0 && pair(1).trap.isEmpty && pair(1).canceled >= 0)
      d.expectSpecial(0x8b1, originalReturn)
      d.expectSpecial(0x8b3, 6)
      // readReg issues ADDI with a nonzero immediate through the real backend;
      // it does not consult a software register shadow or an eliminated move.
      val restoredDestination = d.readReg(10)
      assert(restoredDestination == csrDestinationSentinel,
        "The precise trap must restore the architectural x10 mapping")
      d.record("f02-runtime-ii-age", "order" -> Json.fromString(order),
        "older_raw" -> Json.fromBigInt(pair.head.instruction.bits),
        "younger_raw" -> Json.fromBigInt(pair(1).instruction.bits),
        "architectural_x10" -> Json.fromBigInt(restoredDestination),
        "expected_x10" -> Json.fromBigInt(csrDestinationSentinel),
        "exception_ingress_counts" -> Json.arr(pair.map(source =>
          Json.fromInt(source.exceptionIngress.count(_.eligible))): _*))
    }
  }

  def run(d: FDIFullControlDriver): Unit = {
    val contexts = Seq(
      ("hu", 0, false, BigInt(2), false, true),
      ("hs", 1, false, BigInt(1), false, true),
      ("hu-disabled", 0, false, BigInt(1), false, false),
      ("hs-disabled", 1, false, BigInt(2), false, false),
      ("hu-untrusted", 0, false, BigInt(2), true, false),
      ("hs-untrusted", 1, false, BigInt(1), true, false),
      ("machine", 3, false, BigInt(3), false, false),
      ("vu", 0, true, BigInt(3), true, false),
      ("vs", 1, true, BigInt(3), true, false))
    for ((name, mode, virtual, cfg, tag, permitted) <- contexts;
      (form, rd, word) <- Seq(("j", 1, j(64)), ("jr", 10, jr(10, 9, 1)),
        ("jr-x0", 0, jr(0, 9, 1)), ("jr-old-source", 9, jr(9, 9, 1)))) {
      d.caseName = s"$name-$form"
      val pc = if (tag) BigInt(0x4006) else BigInt(0x1806)
      val target = if (form == "j") pc + 64 else BigInt(0x5002)
      d.setup(mode = mode, virtual = virtual, mainCfg = cfg,
        csrs = if (d.enabled) initial else Seq.empty,
        registers = Seq(1 -> sentinel, 10 -> sentinel, 9 -> BigInt(0x5001)))
      d.record("f02-expect", "name" -> Json.fromString(d.caseName),
        "allowed" -> Json.fromBoolean(d.enabled && permitted), "target" -> Json.fromBigInt(target),
        "link" -> Json.fromBigInt(pc + 4), "raw" -> Json.fromBigInt(word))
      val source = d.execute(pc, call(word, target, tag))
      checkCall(d, source, target, rd, d.enabled && permitted)
      d.record("f02-source-case", "name" -> Json.fromString(d.caseName),
        "allowed" -> Json.fromBoolean(d.enabled && permitted))
    }
    if (!d.enabled) return

    for (mode <- Seq(0, 1); stateen <- Seq(false, true); closed <- Seq(false, true)) {
      d.caseName = s"implicit-mode-$mode-stateen-$stateen-close-$closed"
      val cfg = (if (mode == 0) BigInt(2) else BigInt(1)) |
        (if (closed) BigInt(1) << (if (mode == 0) 9 else 5) else BigInt(0))
      d.setup(mode = mode, mainCfg = cfg,
        csrs = initial ++ (if (stateen) Seq.empty else Seq(0x30c -> BigInt(0))))
      val source = d.execute(0x1806, call(j(64), 0x1846))
      checkCall(d, source, 0x1846, 1, allowed = true)
      d.record("f02-implicit-permission", "name" -> Json.fromString(d.caseName))
    }
    for (offset <- Seq(-4194304, -2048, -2, 0, 2, 2046, 4194302); predicted <- Seq(false, true)) {
      d.caseName = s"j-offset-$offset-predicted-$predicted"
      d.setup(csrs = initial)
      val target = (BigInt(0x1806) + offset) & mask64
      val instruction = call(j(offset), target, predicted = predicted)
      val source = d.execute(0x1806, instruction)
      checkCall(d, source, target, 1, allowed = true)
      // These nearby legal targets share the source's FTB FIT region. The
      // extreme offsets remain arithmetic cases; their truncated FTB encoding
      // must not be counted as a correctly predicted full target.
      val checkPrediction = offset >= -2048 && offset <= 2046
      val expectedMispredicted = !instruction.predictedTaken || instruction.predictedTarget.get != target
      if (checkPrediction) assert(source.outputs.head.mispredicted == expectedMispredicted,
        s"${d.caseName}: actual Jump prediction outcome does not match the planned target")
      d.record("f02-arithmetic", "offset" -> Json.fromInt(offset), "predicted" -> Json.fromBoolean(predicted),
        "prediction_checked" -> Json.fromBoolean(checkPrediction),
        "expected_mispredicted" -> (if (checkPrediction) Json.fromBoolean(expectedMispredicted) else Json.Null),
        "observed_mispredicted" -> Json.fromBoolean(source.outputs.head.mispredicted))
    }

    d.caseName = "consecutive-call-return-and-software-update"
    d.setup(csrs = initial)
    for (pc <- Seq(BigInt(0x1806), BigInt(0x1806), BigInt(0x1826))) {
      val target = pc + 8192
      val source = d.execute(pc, call(j(8192), target))
      checkCall(d, source, target, 1, allowed = true)
      val read = d.execute(pc + 4, FDIControlInstruction((BigInt(0x8b1) << 20) | 0x2573))
      d.expectRetired(read, Some(pc + 4))
      val ret = d.execute(target, FDIControlInstruction(0x8067, tag = true,
        branchType = 3, predictedTaken = true, predictedTarget = Some(pc + 4)))
      d.expectRetired(ret); d.expectNoWrite(ret)
      assert(ret.outputs.size == 1 && ret.outputs.head.target == pc + 4 && ret.outputs.head.exception == 0)
      assert(ret.allocated > source.callCycles.head && source.retired < ret.allocated)
      d.expectSpecial(0x8b1, pc + 4)
      d.writeCsr(0x8b1, originalReturn)
      d.expectSpecial(0x8b1, originalReturn)
    }
    d.record("f02-owner-and-mirror-lifecycle")

    d.caseName = "ordinary-csr-and-call-share-a-frontend-packet"
    d.setup(csrs = initial, registers = Seq(11 -> BigInt(21)))
    val serialPair = d.issueBatch(0x1804, Seq(
      FDIControlInstruction((BigInt(0x040) << 20) | (BigInt(11) << 15) | 0x1073),
      call(j(64), 0x1848)))
    d.until("ordinary CSR and following call both retire")(serialPair.forall(_.retired >= 0))
    d.idle(16)
    checkCall(d, serialPair(1), 0x1848, 1, allowed = true)
    assert(serialPair.head.retired < serialPair(1).allocated)
    assert(d.uint(d.dut.io.userScratch) == 21)
    d.record("f02-ordinary-csr-call-order")

    d.caseName = "two-calls-share-a-frontend-packet"
    d.setup(csrs = initial)
    val calls = d.issueBatch(0x1804, Seq(call(j(4), 0x1808), call(j(64), 0x1848)))
    d.until("two serialized calls retire")(calls.forall(_.retired >= 0))
    d.idle(16)
    calls.foreach { source =>
      d.expectRetired(source, Some(source.pc + 4))
      assert(source.callCycles.size == 1)
    }
    assert(calls.head.retired < calls(1).allocated && calls.head.callCycles.head < calls(1).callCycles.head)
    d.expectSpecial(0x8b1, 0x180c)
    d.record("f02-two-call-order")

    for (kind <- Seq("irq", "nmi", "debug", "hu")) {
      d.caseName = s"pending-$kind-between-effect-and-retirement"
      val interruptConfig = kind match {
        case "irq" => Seq(0x304 -> (BigInt(1) << 7))
        case "hu" => Seq(0x005 -> BigInt(0x2000), 0x000 -> BigInt(1),
          0x004 -> BigInt(16), 0x800 -> BigInt(3000))
        case _ => Seq.empty
      }
      d.setup(csrs = initial ++ interruptConfig)
      val io = d.dut.io
      io.traceEnable.poke(true.B); io.traceStall.poke(true.B)
      d.until("native trace stall", 16)(d.bool(io.traceBlocked))
      val source = d.issue(0x1806, call(j(64), 0x1846))
      d.until("call effect and WB while retirement stalls")(source.callCycles.nonEmpty && source.writebacks.nonEmpty)
      assert(source.retired < 0 && d.bool(d.dut.control.waitForward))
      val beforeAccepted = d.interruptAcceptances.size
      val beforeEntries = d.entries.size
      kind match {
        case "irq" => io.interrupts.mtip.poke(true.B)
        case "nmi" => io.interrupts.nmi.nmi_31.poke(true.B)
        case "debug" => io.interrupts.debug.poke(true.B)
        case "hu" => d.until("native user timer becomes pending", 5000)(d.bool(io.timerPending))
      }
      d.until("real filtered interrupt candidate", 128)(d.bool(io.candidate.valid))
      val candidate = io.candidate.bits
      assert(d.bool(candidate.isInterrupt) && !d.bool(candidate.criticalDebug))
      assert(!d.bool(candidate.irToHS) && !d.bool(candidate.irToVS))
      assert(d.bool(candidate.debug) == (kind == "debug") && d.bool(candidate.nmi) == (kind == "nmi") &&
        d.bool(candidate.irToHU) == (kind == "hu"), "The pending candidate has the wrong interrupt class")
      if (kind != "debug") {
        val expected = if (kind == "irq") 7 else if (kind == "nmi") 31 else 4
        assert(d.uint(candidate.cause) == expected, "The pending candidate has the wrong cause")
      }
      d.idle(20)
      assert(source.retired < 0 && source.canceled < 0 && source.callCycles.size == 1)
      assert(d.interruptAcceptances.size == beforeAccepted && d.entries.size == beforeEntries,
        "A pending interrupt consumed a call whose ReturnPC already changed")
      d.expectSpecial(0x8b1, 0x180a)
      val releasedAt = d.cycle
      io.traceStall.poke(false.B)
      d.until("call retirement after trace release")(source.retired >= 0)
      d.idle(12)
      val next = d.issue(0x1846, FDIControlInstruction(0x13))
      d.until("interrupt entry at a later owner", 3000) {
        d.entries.size > beforeEntries
      }
      io.interrupts.mtip.poke(false.B); io.interrupts.nmi.nmi_31.poke(false.B); io.interrupts.debug.poke(false.B)
      d.idle(20)
      assert(source.callCycles.size == 1 && source.canceled < 0 && source.trap.isEmpty)
      d.expectSpecial(0x8b1, 0x180a)
      d.expectSpecial(0x8b3, 6)
      val accepted = d.interruptAcceptances.drop(beforeAccepted).toSeq
      val delivered = d.entries.drop(beforeEntries).toSeq
      assert(accepted.size == 1 && delivered.size == 1,
        "Each pending interrupt must have exactly one acceptance and one architectural entry")
      val identity = accepted.head
      val entry = delivered.head
      assert(identity.source == next.id && next.rob.contains(identity.rob) && identity.fetch == next.fetch && !identity.isRVC,
        "The released interrupt must own the actual later instruction, not the completed call")
      assert(identity.cycle > source.retired && identity.cycle >= releasedAt && identity.isInterrupt)
      assert(!identity.criticalDebug && !identity.hs && !identity.vs)
      assert(identity.debug == (kind == "debug") && identity.nmi == (kind == "nmi") && identity.hu == (kind == "hu"))
      assert(entry.accepted.contains(identity) && entry.cycle > identity.cycle && entry.epc == next.pc && entry.reason == 6)
      val interruptBit = BigInt(1) << 63
      kind match {
        case "irq" =>
          assert(identity.cause == 7 && entry.kind == "m" && entry.cause == (interruptBit | 7) && entry.tval == 0)
          assert(d.uint(io.mcause) == (interruptBit | 7) && d.uint(io.mepc) == next.pc && d.uint(io.mtval) == 0)
          assert(next.trap.exists(t => t.cause == (interruptBit | 7) && t.epc == next.pc && t.reason == 6))
        case "nmi" =>
          assert(identity.cause == 31 && entry.kind == "nmi" && entry.cause == (interruptBit | 31))
          assert(d.uint(io.mncause) == (interruptBit | 31) && d.uint(io.mnepc) == next.pc)
        case "debug" =>
          assert(entry.kind == "debug" && entry.cause == 3 && d.bool(io.debugMode))
          assert(d.uint(io.dpc) == next.pc && ((d.uint(io.dcsr) >> 6) & 7) == 3)
        case "hu" =>
          assert(identity.cause == 4 && entry.kind == "hu" && entry.cause == (interruptBit | 4) && entry.tval == 0)
          assert(d.uint(io.ucause) == (interruptBit | 4) && d.uint(io.uepc) == next.pc && d.uint(io.utval) == 0)
          assert(d.bool(io.handler), "The HU entry must establish actual handler ownership")
      }
      d.record("f02-pending-event", "kind" -> Json.fromString(kind),
        "call_retired" -> Json.fromInt(source.retired), "released_at" -> Json.fromInt(releasedAt),
        "accepted_at" -> Json.fromInt(identity.cycle), "entry_at" -> Json.fromInt(entry.cycle),
        "entry_kind" -> Json.fromString(entry.kind), "cause" -> Json.fromBigInt(entry.cause),
        "later_pc" -> Json.fromBigInt(next.pc), "later_source" -> Json.fromInt(next.id))
    }

    for (fault <- Seq(1, 12)) {
      d.caseName = s"legal-call-then-target-fetch-$fault"
      d.setup(csrs = initial)
      val source = d.execute(0x1806, call(j(64), 0x1846))
      checkCall(d, source, 0x1846, 1, allowed = true)
      val target = d.execute(0x1846, FDIControlInstruction(0x13, fetchException = 1 << fault))
      d.expectTrap(target, fault, 0x1846, reason = 6)
      d.expectSpecial(0x8b1, 0x180a)
      d.record("f02-target-fetch-fault", "cause" -> Json.fromInt(fault))
    }
    for (fault <- Seq(1, 12)) {
      d.caseName = s"source-fetch-$fault-prevents-call-execution"
      d.setup(csrs = initial)
      val source = d.execute(0x1806, call(j(64), 0x1846).copy(fetchException = 1 << fault))
      d.expectTrap(source, fault, 0x1806, reason = 6)
      d.expectNoWrite(source)
      assert(source.outputs.isEmpty && source.callCycles.isEmpty)
      d.expectSpecial(0x8b1, originalReturn)
      d.record("f02-source-fetch-fault", "cause" -> Json.fromInt(fault))
    }
    d.caseName = "older-configuration-cancels-unallocated-call"
    d.setup(csrs = initial, registers = Seq(9 -> BigInt(0x5000)))
    val pair = d.issueBatch(0x1804, Seq(FDIControlInstruction((BigInt(0x8b3) << 20) | 0x35073),
      call(jr(10, 9, 0), 0x5000)))
    d.until("serialized configuration recovery discards call")(pair.head.retired >= 0 && pair(1).canceled >= 0)
    d.idle(32); d.expectNoWrite(pair(1)); assert(pair(1).callCycles.isEmpty && pair(1).outputs.isEmpty)
    d.expectSpecial(0x8b1, originalReturn)
    d.writeCsr(0x8b3, 6)
    val retry = d.execute(0x1808, call(jr(10, 9, 0), 0x5000))
    checkCall(d, retry, 0x5000, 10, allowed = true)
    d.record("f02-canceled-call-retry")
    rawInstructionOwnership(d)
  }
}
