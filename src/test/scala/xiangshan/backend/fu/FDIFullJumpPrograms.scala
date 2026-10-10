// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import io.circe.Json
import scala.collection.mutable

object F03ControlPrograms {
  private val mask64 = (BigInt(1) << 64) - 1
  private val sentinel = BigInt("6a5a39c7", 16)
  private val sourcePc = BigInt(0x4006)
  private val specials = Seq(0x8b0 -> BigInt(0x7100), 0x8b1 -> BigInt(0x7200),
    0x8b2 -> BigInt(0x7300))

  // The external frontend service derives tags from the program's source
  // context. An enabled guest never acquires trust from host MainBounds.
  private def sourceTag(pc: BigInt, mode: Int, virtual: Boolean, cfg: BigInt,
    trustedLo: BigInt = 0x1000, trustedHi: BigInt = 0x3000): Boolean =
    mode != 3 && cfg.testBit(if (mode == 1) 0 else 1) &&
      (virtual || pc < trustedLo || pc >= trustedHi)

  private def sext(value: BigInt, bits: Int): BigInt =
    if (value.testBit(bits - 1)) value - (BigInt(1) << bits) else value
  private def bit(value: BigInt, index: Int): BigInt = (value >> index) & 1
  private def field(value: BigInt, lo: Int, size: Int): BigInt =
    (value >> lo) & ((BigInt(1) << size) - 1)

  private def jal(rd: Int, offset: Int): BigInt = {
    require(offset % 2 == 0 && offset >= -(1 << 20) && offset < (1 << 20))
    val imm = BigInt(offset) & ((BigInt(1) << 21) - 1)
    (bit(imm, 20) << 31) | (field(imm, 1, 10) << 21) |
      (bit(imm, 11) << 20) | (field(imm, 12, 8) << 12) | (BigInt(rd) << 7) | 0x6f
  }
  private def jalr(rd: Int, rs1: Int, offset: Int): BigInt = {
    require(offset >= -2048 && offset <= 2047)
    ((BigInt(offset) & 0xfff) << 20) | (BigInt(rs1) << 15) | (BigInt(rd) << 7) | 0x67
  }
  private def cj(offset: Int): BigInt = {
    require(offset % 2 == 0 && offset >= -2048 && offset <= 2046)
    val imm = BigInt(offset) & 0xfff
    BigInt(0xa001) | (bit(imm, 11) << 12) | (bit(imm, 4) << 11) |
      (field(imm, 8, 2) << 9) | (bit(imm, 10) << 8) | (bit(imm, 6) << 7) |
      (bit(imm, 7) << 6) | (field(imm, 1, 3) << 3) | (bit(imm, 5) << 2)
  }
  private def cjr(rs1: Int, link: Boolean): BigInt = {
    require(rs1 > 0 && rs1 < 32)
    BigInt(if (link) 0x9002 else 0x8002) | (BigInt(rs1) << 7)
  }

  // This decoder reads the original instruction and pre-execution register
  // values. No expanded instruction, FU target, or policy output is an oracle.
  private case class Jump(target: BigInt, rd: Int, bytes: Int, branchType: Int)
  private def decode(bits: BigInt, pc: BigInt, registers: Map[Int, BigInt]): Jump = {
    def old(index: Int): BigInt = if (index == 0) BigInt(0) else registers(index)
    if ((bits & 3) != 3) {
      if (field(bits, 13, 3) == 5) {
        val imm = (bit(bits, 12) << 11) | (bit(bits, 11) << 4) |
          (field(bits, 9, 2) << 8) | (bit(bits, 8) << 10) | (bit(bits, 7) << 6) |
          (bit(bits, 6) << 7) | (field(bits, 3, 3) << 1) | (bit(bits, 2) << 5)
        Jump((pc + sext(imm, 12)) & mask64, 0, 2, 2)
      } else {
        require((bits & 0xe07f) == 0x8002)
        Jump(old(field(bits, 7, 5).toInt) & (mask64 ^ 1),
          if (bit(bits, 12) == 1) 1 else 0, 2, 3)
      }
    } else if ((bits & 0x7f) == 0x6f) {
      val imm = (bit(bits, 31) << 20) | (field(bits, 21, 10) << 1) |
        (bit(bits, 20) << 11) | (field(bits, 12, 8) << 12)
      Jump((pc + sext(imm, 21)) & mask64, field(bits, 7, 5).toInt, 4, 2)
    } else {
      require((bits & 0x707f) == 0x67)
      Jump((old(field(bits, 15, 5).toInt) + sext(field(bits, 20, 12), 12)) & (mask64 ^ 1),
        field(bits, 7, 5).toInt, 4, 3)
    }
  }

  private case class Sample(name: String, bits: BigInt, pc: BigInt = sourcePc,
    registers: Map[Int, BigInt] = Map(9 -> BigInt(0x5000)), csrs: Seq[(Int, BigInt)] = Seq.empty,
    mode: Int = 0, virtual: Boolean = false, cfg: BigInt = 2,
    predictedTaken: Boolean = true, predictedTarget: Option[BigInt] = None,
    expectedCause: Option[Int] = Some(24))

  private def bound(slot: Int, lo: BigInt, hi: BigInt, valid: Boolean = true): Seq[(Int, BigInt)] =
    Seq((0x8c0 + 2 * slot) -> lo, (0x8c1 + 2 * slot) -> hi,
      0x8c8 -> (if (valid) BigInt(1) << (16 * slot) else BigInt(0)))

  def run(driver: FDIFullControlDriver): Unit = {
    if (!driver.enabled) {
      runDisabled(driver)
      return
    }
    val cases = mutable.ArrayBuffer.empty[Sample]
    val basic = Sample("jalr-denied", jalr(10, 9, 0))
    val encodings = Seq(
      ("jal-link-positive", jal(10, 62), Map.empty[Int, BigInt]),
      ("jal-x0-negative", jal(0, -70), Map.empty[Int, BigInt]),
      ("jal-maximum", jal(1, (1 << 20) - 2), Map.empty[Int, BigInt]),
      ("jal-minimum", jal(5, -(1 << 20)), Map.empty[Int, BigInt]),
      ("jalr-link", jalr(10, 9, 0), Map(9 -> BigInt(0x5000))),
      ("jalr-x0", jalr(0, 9, 0), Map(9 -> BigInt(0x5000))),
      ("ret", jalr(0, 1, 0), Map(1 -> BigInt(0x5000))),
      ("jalr-rd-equals-rs1", jalr(9, 9, -1), Map(9 -> BigInt(0x5003))),
      ("jalr-sign-minimum", jalr(10, 9, -2048), Map(9 -> BigInt(0x5801))),
      ("jalr-sign-maximum", jalr(10, 9, 2047), Map(9 -> BigInt(0x4801))),
      ("c-j-positive", cj(62), Map.empty[Int, BigInt]),
      ("c-j-negative", cj(-70), Map.empty[Int, BigInt]),
      ("c-j-minimum", cj(-2048), Map.empty[Int, BigInt]),
      ("c-j-maximum", cj(2046), Map.empty[Int, BigInt]),
      ("c-jr", cjr(9, link = false), Map(9 -> BigInt(0x5001))),
      ("c-jalr", cjr(9, link = true), Map(9 -> BigInt(0x5001))),
      ("c-jalr-old-ra", cjr(1, link = true), Map(1 -> BigInt(0x5001))))
    for ((name, instruction, registers) <- encodings; allowed <- Seq(false, true)) {
      val decoded = decode(instruction, sourcePc, registers)
      cases += Sample(s"$name-${if (allowed) "allowed" else "denied"}", instruction,
        registers = registers, csrs = if (allowed) Seq(0x8b0 -> decoded.target) else Seq.empty,
        expectedCause = if (allowed) None else Some(24))
    }
    for (slot <- 0 until 4; (name, target, lo, hi, valid, allowed) <- Seq(
      ("lower", 0x5000, 0x5000, 0x5010, true, true),
      ("last-halfword", 0x500e, 0x5000, 0x5010, true, true),
      ("upper", 0x5010, 0x5000, 0x5010, true, false),
      ("below", 0x4ffe, 0x5000, 0x5010, true, false),
      ("invalid", 0x5000, 0x5000, 0x5010, false, false),
      ("empty", 0x5000, 0x5000, 0x5000, true, false),
      ("reversed", 0x5000, 0x5010, 0x5000, true, false))) {
      cases += basic.copy(name = s"range-$slot-$name", registers = Map(9 -> BigInt(target)),
        csrs = bound(slot, lo, hi, valid), expectedCause = if (allowed) None else Some(24))
    }
    for (address <- 0x8b0 to 0x8b2; exact <- Seq(false, true)) {
      cases += basic.copy(name = s"special-$address-${if (exact) "exact" else "odd-mismatch"}",
        csrs = Seq(address -> BigInt(if (exact) 0x5000 else 0x5001)),
        expectedCause = if (exact) None else Some(24))
    }
    cases += basic.copy(name = "zero-special-disabled", registers = Map(9 -> BigInt(0)),
      csrs = (0x8b0 to 0x8b2).map(_ -> BigInt(0)))
    cases += basic.copy(name = "zero-explicit-bound", registers = Map(9 -> BigInt(0)),
      csrs = bound(3, 0, 8), expectedCause = None)
    for (bitIndex <- Seq(39, 48, 50, 63); allow <- Seq(false, true)) {
      val target = (BigInt(1) << bitIndex) | 0x5000
      cases += basic.copy(name = s"full-target-bit-$bitIndex-${if (allow) "allowed" else "denied"}",
        registers = Map(9 -> target), csrs = bound(0, if (allow) target else BigInt(0x5000),
          if (allow) target + 8 else BigInt(0x5008)), expectedCause = if (allow) None else Some(24))
    }
    cases += basic.copy(name = "jalr-addition-wrap", bits = jalr(10, 9, 2),
      registers = Map(9 -> mask64), csrs = bound(0, 0, 8), expectedCause = None)
    cases += basic.copy(name = "jalr-negative-wrap", bits = jalr(10, 9, -2),
      registers = Map(9 -> BigInt(0)))
    val highPc = BigInt("ffff800000004006", 16)
    cases += basic.copy(name = "canonical-high-source-denied", bits = jal(10, 58),
      pc = highPc, registers = Map.empty, csrs = Seq(0x180 -> (BigInt(9) << 60)))
    cases += basic.copy(name = "canonical-high-source-allowed", bits = jal(10, 58), pc = highPc,
      registers = Map.empty, csrs = Seq(0x180 -> (BigInt(9) << 60), 0x8b0 -> (highPc + 58)), expectedCause = None)
    for (mode <- Seq(0, 1)) {
      val enable = if (mode == 0) BigInt(2) else BigInt(1)
      val close = if (mode == 0) BigInt(1) << 9 else BigInt(1) << 5
      cases += basic.copy(name = s"mode-$mode-enabled", mode = mode, cfg = enable,
        expectedCause = Some(if (mode == 0) 24 else 25))
      cases += basic.copy(name = s"mode-$mode-delegated", mode = mode, cfg = enable,
        csrs = Seq(0x302 -> (BigInt(1) << (if (mode == 0) 24 else 25))),
        expectedCause = Some(if (mode == 0) 24 else 25))
      cases += basic.copy(name = s"mode-$mode-other-enable-only", mode = mode,
        cfg = if (mode == 0) 1 else 2, expectedCause = None)
      cases += basic.copy(name = s"mode-$mode-close", mode = mode, cfg = enable | close, expectedCause = None)
      cases += basic.copy(name = s"mode-$mode-trusted", pc = 0x1806, mode = mode,
        cfg = enable, expectedCause = None)
      for (trusted <- Seq(false, true); closed <- Seq(false, true); rawAllowed <- Seq(false, true)) {
        cases += basic.copy(name = s"guest-$mode-trusted-$trusted-close-$closed-raw-$rawAllowed",
          pc = if (trusted) BigInt(0x1806) else sourcePc,
          mode = mode, virtual = true, cfg = enable | (if (closed) close else BigInt(0)),
          csrs = if (rawAllowed) Seq(0x8b0 -> BigInt(0x5000)) else Seq.empty,
          expectedCause = Some(2))
      }
      cases += basic.copy(name = s"guest-$mode-disabled", mode = mode, virtual = true,
        cfg = 0, expectedCause = None)
      cases += basic.copy(name = s"guest-$mode-compressed-jalr", bits = cjr(9, link = true),
        mode = mode, virtual = true, cfg = enable, expectedCause = Some(2))
    }
    cases += basic.copy(name = "machine-bypass", mode = 3, cfg = 3, expectedCause = None)
    for (predictTaken <- Seq(false, true); predictCorrect <- Seq(false, true); allow <- Seq(false, true)) {
      cases += basic.copy(name = s"prediction-taken-$predictTaken-correct-$predictCorrect-allow-$allow",
        predictedTaken = predictTaken, predictedTarget = Some(if (predictCorrect) 0x5000 else 0x6000),
        csrs = if (allow) Seq(0x8b0 -> BigInt(0x5000)) else Seq.empty,
        expectedCause = if (allow) None else Some(24))
    }

    for (sample <- cases) {
      val registers = Map(1 -> sentinel, 5 -> sentinel, 9 -> sentinel, 10 -> sentinel) ++ sample.registers
      val decoded = decode(sample.bits, sample.pc, registers)
      val tag = sourceTag(sample.pc, sample.mode, sample.virtual, sample.cfg)
      // The dedicated prediction cases use valid low targets with permission;
      // their recovery decision depends only on the planned prediction.
      val expectedMisPred = Option.when(sample.name.startsWith("prediction-") && sample.expectedCause.isEmpty) {
        !sample.predictedTaken || sample.predictedTarget.getOrElse(decoded.target) != decoded.target
      }
      val config = (specials ++ Seq(0x8b3 -> BigInt(6)) ++ sample.csrs).toMap.toSeq.sortBy(_._1)
      driver.caseName = s"f03-${sample.name}"
      driver.setup(mode = sample.mode, virtual = sample.virtual, mainCfg = sample.cfg,
        trustedLo = 0x1000, trustedHi = 0x3000, csrs = config, registers = registers.toSeq.sortBy(_._1))
      driver.record("f03-expect", "instruction" -> Json.fromBigInt(sample.bits),
        "source_pc" -> Json.fromBigInt(sample.pc), "resolved_target" -> Json.fromBigInt(decoded.target),
        "source_tag" -> Json.fromBoolean(tag),
        "source_mode" -> Json.fromInt(sample.mode), "source_virtual" -> Json.fromBoolean(sample.virtual),
        "main_cfg" -> Json.fromBigInt(sample.cfg), "main_lo" -> Json.fromInt(0x1000), "main_hi" -> Json.fromInt(0x3000),
        "mispredicted" -> expectedMisPred.map(Json.fromBoolean).getOrElse(Json.Null),
        "cause" -> sample.expectedCause.map(Json.fromInt).getOrElse(Json.Null),
        "tval" -> sample.expectedCause.map(cause => Json.fromBigInt(
          if (cause == 2) sample.bits else decoded.target)).getOrElse(Json.Null),
        "reason" -> Json.fromInt(if (sample.expectedCause.exists(_ != 2)) 4 else 6),
        "initial_gprs" -> Json.fromFields(registers.toSeq.map { case (index, value) => index.toString -> Json.fromBigInt(value) }),
        "initial_csrs" -> Json.fromFields(config.map { case (address, value) => address.toHexString -> Json.fromBigInt(value) }))
      val source = driver.execute(sample.pc, FDIControlInstruction(sample.bits, tag = tag,
        bytes = decoded.bytes, branchType = decoded.branchType, predictedTaken = sample.predictedTaken,
        predictedTarget = sample.predictedTarget.orElse(Some(decoded.target))))
      val outputs = source.outputs.filter(_.kind == "jump")
      assert(outputs.size == 1, s"${sample.name}: the source must have one live Jump result")
      val output = outputs.head
      assert(output.target == decoded.target, s"${sample.name}: the resolved target lost source arithmetic")
      assert(!output.callEvent, s"${sample.name}: an ordinary jump changed the call owner")
      expectedMisPred.foreach { expected =>
        assert(output.mispredicted == expected, s"${sample.name}: prediction recovery disagrees with the planned target and direction")
      }
      sample.expectedCause match {
        case Some(cause) =>
          driver.expectTrap(source, cause, if (cause == 2) sample.bits else decoded.target,
            reason = if (cause == 2) 6 else 4)
          driver.expectNoWrite(source)
          assert(!output.rfWen && !output.redirect, s"${sample.name}: a rejected jump had a normal effect")
          if (decoded.rd != 0) {
            val read = driver.execute(0x1100, FDIControlInstruction(
              (BigInt(1) << 20) | (BigInt(decoded.rd) << 15) | (BigInt(28) << 7) | 0x13))
            driver.expectRetired(read, Some((registers(decoded.rd) + 1) & mask64))
          }
        case None =>
          driver.expectRetired(source, if (decoded.rd == 0) None else Some((sample.pc + decoded.bytes) & mask64))
          assert(output.rfWen == (decoded.rd != 0), s"${sample.name}: link write enable disagrees with rd")
          assert(output.exception == 0 && output.redirect, s"${sample.name}: an allowed jump was rejected")
          assert(output.data == ((sample.pc + decoded.bytes) & mask64), s"${sample.name}: wrong link length")
          if (decoded.rd == 0) driver.expectNoWrite(source)
          driver.expectSpecial(0x8b3, 6)
      }
      for (address <- 0x8b0 to 0x8b2) driver.expectSpecial(address, config.toMap.apply(address))
      driver.record("f03-case", "name" -> Json.fromString(sample.name),
        "instruction" -> Json.fromBigInt(sample.bits), "source_pc" -> Json.fromBigInt(sample.pc),
        "expected_target" -> Json.fromBigInt(decoded.target), "bytes" -> Json.fromInt(decoded.bytes),
        "expected_cause" -> sample.expectedCause.map(Json.fromInt).getOrElse(Json.Null))
    }
    configurationAndCancellation(driver)
    fetchFaultsAndAuipc(driver)
  }

  private def configurationAndCancellation(driver: FDIFullControlDriver): Unit = {
    for (configurationFlush <- Seq(false, true)) {
      driver.caseName = s"f03-cancel-configuration-$configurationFlush"
      driver.setup(mainCfg = 2, trustedHi = 0x1808,
        csrs = specials ++ Seq(0x8b3 -> BigInt(6)) ++ bound(0, 0x5000, 0x5010, configurationFlush),
        registers = Seq(9 -> BigInt(0x5000), 10 -> sentinel, 12 -> BigInt(0)))
      val older = if (configurationFlush) {
        FDIControlInstruction((BigInt(0x8c8) << 20) | (BigInt(12) << 15) | 0x1073)
      } else {
        // BEQ x0,x0,+128 resolves before the younger rejected JALR can commit.
        FDIControlInstruction(BigInt("08000063", 16), branchType = 1, predictedTaken = false)
      }
      val jump = FDIControlInstruction(jalr(10, 9, 0),
        tag = sourceTag(0x1808, 0, false, 2, trustedHi = 0x1808),
        branchType = 3, predictedTaken = true, predictedTarget = Some(0x5000))
      val sources = driver.issueBatch(0x1804, Seq(older, jump))
      driver.until("older recovery cancels the younger jump", 2000) {
        sources.head.retired >= 0 && sources(1).canceled >= 0
      }
      driver.idle(32)
      assert(sources(1).retired < 0 && sources(1).trap.isEmpty,
        "A canceled jump reached the architectural retirement or trap boundary")
      assert(sources(1).outputs.forall(_.cycle <= sources(1).canceled),
        "A canceled jump generated a late live FU result")
      assert(sources(1).writeCycles.forall(_ <= sources(1).canceled),
        "A canceled jump generated a late physical register write")
      if (configurationFlush) driver.expectNoWrite(sources(1))
      val read = driver.execute(0x1200, FDIControlInstruction(
        (BigInt(1) << 20) | (BigInt(10) << 15) | (BigInt(28) << 7) | 0x13))
      driver.expectRetired(read, Some(sentinel + 1))
      driver.expectSpecial(0x8b3, 6)
      val retried = driver.execute(0x1808, jump)
      driver.expectTrap(retried, 24, 0x5000, reason = 4)
      driver.expectNoWrite(retried)
      driver.record("f03-cancel-reexecute", "configuration_flush" -> Json.fromBoolean(configurationFlush),
        "canceled_cycle" -> Json.fromInt(sources(1).canceled), "reexecute_pc" -> Json.fromInt(0x1808))
    }
  }

  private def fetchFaultsAndAuipc(driver: FDIFullControlDriver): Unit = {
    for (sourceFault <- Seq(false, true); cause <- Seq(1, 12)) {
      driver.caseName = s"f03-fetch-source-$sourceFault-cause-$cause"
      driver.setup(csrs = specials ++ Seq(0x8b3 -> BigInt(6), 0x8b0 -> BigInt(0x5000)),
        registers = Seq(9 -> BigInt(0x5000), 10 -> sentinel))
      val source = driver.execute(sourcePc, FDIControlInstruction(jalr(10, 9, 0),
        tag = sourceTag(sourcePc, 0, false, 2),
        branchType = 3, predictedTaken = true, predictedTarget = Some(0x5000),
        fetchException = if (sourceFault) 1 << cause else 0))
      if (sourceFault) {
        driver.expectTrap(source, cause, sourcePc, reason = 6)
        driver.expectNoWrite(source)
      } else {
        driver.expectRetired(source, Some(sourcePc + 4))
        val target = driver.execute(0x5000, FDIControlInstruction(0x13,
          tag = sourceTag(0x5000, 0, false, 2), fetchException = 1 << cause))
        driver.expectTrap(target, cause, 0x5000, reason = 6)
        driver.expectNoWrite(target)
      }
      driver.record("f03-fetch-fault", "source_fault" -> Json.fromBoolean(sourceFault),
        "cause" -> Json.fromInt(cause))
    }
    for ((mode, virtual) <- Seq(0 -> false, 1 -> false, 0 -> true, 1 -> true)) {
      driver.caseName = s"f03-auipc-$mode-$virtual"
      driver.setup(mode = mode, virtual = virtual, mainCfg = 3, csrs = Seq(0x8b3 -> BigInt(6)))
      val source = driver.execute(sourcePc, FDIControlInstruction(BigInt("12345517", 16),
        tag = sourceTag(sourcePc, mode, virtual, 3)))
      driver.expectRetired(source, Some(sourcePc + BigInt("12345000", 16)))
      driver.expectSpecial(0x8b3, 6)
      assert(source.outputs.forall(output => !output.redirect && !output.callEvent && output.exception == 0),
        "AUIPC was treated as a protected jump")
      driver.record("f03-auipc", "mode" -> Json.fromInt(mode), "virtual" -> Json.fromBoolean(virtual))
    }
  }

  private def runDisabled(driver: FDIFullControlDriver): Unit = {
    val encodings = Seq(jal(10, 62), jal(0, -70), jalr(10, 9, 1), jalr(9, 9, -1),
      cj(62), cj(-70), cjr(9, link = false), cjr(9, link = true), cjr(1, link = true))
    for ((bits, index) <- encodings.zipWithIndex; predicted <- Seq(false, true)) {
      val registers = Map(1 -> BigInt(0x5001), 9 -> BigInt(0x5001), 10 -> sentinel)
      val expected = decode(bits, sourcePc, registers)
      driver.caseName = s"f03-off-$index-predicted-$predicted"
      driver.setup(registers = registers.toSeq.sortBy(_._1))
      val source = driver.execute(sourcePc, FDIControlInstruction(bits, tag = false, bytes = expected.bytes,
        branchType = expected.branchType, predictedTaken = predicted, predictedTarget = Some(expected.target)))
      driver.expectRetired(source, if (expected.rd == 0) None else Some(sourcePc + expected.bytes))
      if (expected.rd == 0) driver.expectNoWrite(source)
      val outputs = source.outputs.filter(_.kind == "jump")
      assert(outputs.size == 1 && outputs.head.target == expected.target && outputs.head.exception == 0)
      assert(outputs.head.rfWen == (expected.rd != 0) && outputs.head.redirect && !outputs.head.callEvent)
      driver.record("f03-off-case", "instruction" -> Json.fromBigInt(bits),
        "bytes" -> Json.fromInt(expected.bytes), "expected_target" -> Json.fromBigInt(expected.target))
    }
  }
}
