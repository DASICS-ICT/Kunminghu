// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import io.circe.Json
import scala.collection.mutable

object F04ControlPrograms {
  private val mask64 = (BigInt(1) << 64) - 1
  private val sourcePc = BigInt(0x4006)
  private val specials = Seq(0x8b0 -> BigInt(0x7100), 0x8b1 -> BigInt(0x7200),
    0x8b2 -> BigInt(0x7300))
  // The external frontend service derives tags from the program's source
  // context. An enabled guest never acquires trust from host MainBounds.
  private def sourceTag(pc: BigInt, mode: Int, virtual: Boolean, cfg: BigInt,
    trustedLo: BigInt = 0x1000, trustedHi: BigInt = 0x3000): Boolean =
    mode != 3 && cfg.testBit(if (mode == 1) 0 else 1) &&
      (virtual || pc < trustedLo || pc >= trustedHi)

  private def bit(value: BigInt, index: Int): BigInt = (value >> index) & 1
  private def field(value: BigInt, lo: Int, size: Int): BigInt =
    (value >> lo) & ((BigInt(1) << size) - 1)
  private def signed(value: BigInt, width: Int): BigInt =
    if (value.testBit(width - 1)) value - (BigInt(1) << width) else value

  private def branch(funct3: Int, rs1: Int, rs2: Int, offset: Int): BigInt = {
    require(Set(0, 1, 4, 5, 6, 7)(funct3))
    require(offset % 2 == 0 && offset >= -4096 && offset <= 4094)
    val imm = BigInt(offset) & 0x1fff
    (bit(imm, 12) << 31) | (field(imm, 5, 6) << 25) | (BigInt(rs2) << 20) |
      (BigInt(rs1) << 15) | (BigInt(funct3) << 12) | (field(imm, 1, 4) << 8) |
      (bit(imm, 11) << 7) | 0x63
  }
  private def compressed(nonzero: Boolean, rs1: Int, offset: Int): BigInt = {
    require(rs1 >= 8 && rs1 <= 15 && offset % 2 == 0 && offset >= -256 && offset <= 254)
    val imm = BigInt(offset) & 0x1ff
    BigInt(if (nonzero) 0xe001 else 0xc001) | (bit(imm, 8) << 12) |
      (field(imm, 3, 2) << 10) | (BigInt(rs1 - 8) << 7) | (field(imm, 6, 2) << 5) |
      (field(imm, 1, 2) << 3) | (bit(imm, 5) << 2)
  }

  private case class Decoded(target: BigInt, next: BigInt, taken: Boolean, bytes: Int)
  // Architectural comparisons are evaluated from raw instruction fields and
  // the input register map, independently of BranchModule and its outputs.
  private def decode(bits: BigInt, pc: BigInt, registers: Map[Int, BigInt]): Decoded = {
    def old(index: Int): BigInt = if (index == 0) BigInt(0) else registers(index) & mask64
    if ((bits & 3) != 3) {
      val op = field(bits, 13, 3).toInt
      require(op == 6 || op == 7)
      val imm = (bit(bits, 12) << 8) | (field(bits, 10, 2) << 3) |
        (field(bits, 5, 2) << 6) | (field(bits, 3, 2) << 1) | (bit(bits, 2) << 5)
      val value = old(field(bits, 7, 3).toInt + 8)
      val taken = if (op == 6) value == 0 else value != 0
      val target = (pc + signed(imm, 9)) & mask64
      Decoded(target, if (taken) target else (pc + 2) & mask64, taken, 2)
    } else {
      require((bits & 0x7f) == 0x63)
      val left = old(field(bits, 15, 5).toInt)
      val right = old(field(bits, 20, 5).toInt)
      val taken = field(bits, 12, 3).toInt match {
        case 0 => left == right
        case 1 => left != right
        case 4 => signed(left, 64) < signed(right, 64)
        case 5 => signed(left, 64) >= signed(right, 64)
        case 6 => left < right
        case 7 => left >= right
        case other => throw new IllegalArgumentException(s"Invalid branch function $other")
      }
      val imm = (bit(bits, 31) << 12) | (field(bits, 25, 6) << 5) |
        (field(bits, 8, 4) << 1) | (bit(bits, 7) << 11)
      val target = (pc + signed(imm, 13)) & mask64
      Decoded(target, if (taken) target else (pc + 4) & mask64, taken, 4)
    }
  }

  private case class Sample(name: String, bits: BigInt = branch(0, 8, 9, 58),
    registers: Map[Int, BigInt] = Map(8 -> BigInt(5), 9 -> BigInt(5)), pc: BigInt = sourcePc,
    csrs: Seq[(Int, BigInt)] = Seq.empty, mode: Int = 0, virtual: Boolean = false,
    cfg: BigInt = 2, prediction: Boolean = true,
    expectedTaken: Boolean = true, expectedCause: Option[Int] = Some(24))

  private def bound(slot: Int, lo: BigInt, hi: BigInt, valid: Boolean = true): Seq[(Int, BigInt)] =
    Seq((0x8c0 + 2 * slot) -> lo, (0x8c1 + 2 * slot) -> hi,
      0x8c8 -> (if (valid) BigInt(1) << (16 * slot) else BigInt(0)))

  def run(driver: FDIFullControlDriver): Unit = {
    if (!driver.enabled) {
      runDisabled(driver)
      return
    }
    val cases = mutable.ArrayBuffer.empty[Sample]
    val negative = mask64
    val comparisons = Seq(
      ("beq", 0, BigInt(7), BigInt(7), BigInt(8)),
      ("bne", 1, BigInt(7), BigInt(8), BigInt(7)),
      ("blt-signed", 4, negative, BigInt(0), negative),
      ("bge-signed", 5, BigInt(0), negative, BigInt(1)),
      ("bltu-unsigned", 6, BigInt(0), negative, BigInt(0)),
      ("bgeu-unsigned", 7, negative, BigInt(0), negative))
    for ((name, funct3, left, takenRight, notTakenRight) <- comparisons;
      taken <- Seq(false, true); predicted <- Seq(false, true); allowedTarget <- Seq(false, true)) {
      val right = if (taken) takenRight else notTakenRight
      val actualLeft = if (!taken && funct3 == 7) BigInt(0) else left
      val bits = branch(funct3, 8, 9, 58)
      cases += Sample(s"$name-taken-$taken-predicted-$predicted-target-$allowedTarget", bits,
        registers = Map(8 -> actualLeft, 9 -> right), prediction = predicted, expectedTaken = taken,
        csrs = if (allowedTarget) Seq(0x8b0 -> (sourcePc + 58)) else Seq.empty,
        expectedCause = if (taken && !allowedTarget) Some(24) else None)
    }
    for (nonzero <- Seq(false, true); taken <- Seq(false, true); predicted <- Seq(false, true);
      allowedTarget <- Seq(false, true)) {
      val offset = if (nonzero) -70 else 58
      val value = if (taken == nonzero) BigInt(1) else BigInt(0)
      cases += Sample(s"c-b${if (nonzero) "nez" else "eqz"}-taken-$taken-predicted-$predicted-target-$allowedTarget",
        compressed(nonzero, 15, offset), registers = Map(15 -> value), expectedTaken = taken,
        prediction = predicted, csrs = if (allowedTarget) Seq(0x8b0 -> (sourcePc + offset)) else Seq.empty,
        expectedCause = if (taken && !allowedTarget) Some(24) else None)
    }
    for (offset <- Seq(-4096, 4094)) {
      cases += Sample(s"branch-offset-$offset", bits = branch(0, 8, 9, offset))
    }
    for (offset <- Seq(-256, 254); nonzero <- Seq(false, true)) {
      cases += Sample(s"compressed-offset-$offset-nonzero-$nonzero", compressed(nonzero, 8, offset),
        registers = Map(8 -> BigInt(if (nonzero) 1 else 0)))
    }
    for (slot <- 0 until 4; (name, offset, lo, hi, valid, allowed) <- Seq(
      ("lower", 58, 0x4040, 0x4050, true, true),
      ("last-halfword", 72, 0x4040, 0x4050, true, true),
      ("upper", 74, 0x4040, 0x4050, true, false),
      ("below", 56, 0x4040, 0x4050, true, false),
      ("invalid", 58, 0x4040, 0x4050, false, false),
      ("empty", 58, 0x4040, 0x4040, true, false),
      ("reversed", 58, 0x4050, 0x4040, true, false))) {
      cases += Sample(s"range-$slot-$name", bits = branch(0, 8, 9, offset),
        csrs = bound(slot, lo, hi, valid), expectedCause = if (allowed) None else Some(24))
    }
    for (address <- 0x8b0 to 0x8b2; exact <- Seq(false, true)) {
      cases += Sample(s"special-$address-exact-$exact", csrs = Seq(address -> BigInt(if (exact) 0x4040 else 0x4041)),
        expectedCause = if (exact) None else Some(24))
    }
    cases += Sample("zero-special-disabled", pc = 0x40, bits = branch(0, 8, 9, -64),
      csrs = (0x8b0 to 0x8b2).map(_ -> BigInt(0)))
    cases += Sample("zero-explicit-range", pc = 0x40, bits = branch(0, 8, 9, -64),
      csrs = bound(2, 0, 8), expectedCause = None)
    val highPc = BigInt("ffff800000004006", 16)
    for (allow <- Seq(false, true)) {
      cases += Sample(s"canonical-high-source-allow-$allow", pc = highPc,
        csrs = Seq(0x180 -> (BigInt(9) << 60)) ++ bound(0, if (allow) highPc + 58 else BigInt(0x4040),
          if (allow) highPc + 74 else BigInt(0x4050)), expectedCause = if (allow) None else Some(24))
    }
    for (mode <- Seq(0, 1); taken <- Seq(false, true)) {
      val registers = Map(8 -> BigInt(5), 9 -> BigInt(if (taken) 5 else 6))
      val enable = if (mode == 0) BigInt(2) else BigInt(1)
      val close = if (mode == 0) BigInt(1) << 9 else BigInt(1) << 5
      val base = Sample(s"mode-$mode-taken-$taken", registers = registers,
        mode = mode, cfg = enable, expectedTaken = taken,
        expectedCause = if (taken) Some(if (mode == 0) 24 else 25) else None)
      cases += base
      cases += base.copy(name = s"mode-$mode-delegated-taken-$taken",
        csrs = Seq(0x302 -> (BigInt(1) << (if (mode == 0) 24 else 25))))
      cases += base.copy(name = s"mode-$mode-other-enable-taken-$taken",
        cfg = if (mode == 0) 1 else 2, expectedCause = None)
      cases += base.copy(name = s"mode-$mode-close-taken-$taken", cfg = enable | close, expectedCause = None)
      cases += base.copy(name = s"mode-$mode-trusted-taken-$taken", pc = 0x1806,
        expectedCause = None)
      for (trusted <- Seq(false, true); closed <- Seq(false, true); rawAllowed <- Seq(false, true)) {
        val guestPc = if (trusted) BigInt(0x1806) else sourcePc
        cases += base.copy(name = s"guest-$mode-taken-$taken-trusted-$trusted-close-$closed-raw-$rawAllowed",
          pc = guestPc, virtual = true, cfg = enable | (if (closed) close else BigInt(0)),
          csrs = if (rawAllowed) Seq(0x8b0 -> (guestPc + 58)) else Seq.empty,
          expectedCause = if (taken) Some(2) else None)
      }
      cases += base.copy(name = s"guest-$mode-disabled-taken-$taken", virtual = true,
        cfg = 0, expectedCause = None)
      for (nonzero <- Seq(false, true)) {
        cases += base.copy(name = s"guest-$mode-compressed-nonzero-$nonzero-taken-$taken",
          bits = compressed(nonzero, 8, 58), registers = Map(8 -> BigInt(if (taken == nonzero) 1 else 0)),
          virtual = true, expectedCause = if (taken) Some(2) else None)
      }
    }
    cases += Sample("machine-bypass", mode = 3, cfg = 3, expectedCause = None)

    for (sample <- cases) {
      val expected = decode(sample.bits, sample.pc, sample.registers)
      assert(expected.taken == sample.expectedTaken, s"${sample.name}: raw encoding does not produce the planned condition")
      val tag = sourceTag(sample.pc, sample.mode, sample.virtual, sample.cfg)
      val config = (specials ++ Seq(0x8b3 -> BigInt(6)) ++ sample.csrs).toMap.toSeq.sortBy(_._1)
      driver.caseName = s"f04-${sample.name}"
      driver.setup(mode = sample.mode, virtual = sample.virtual, mainCfg = sample.cfg,
        trustedLo = 0x1000, trustedHi = 0x3000, csrs = config, registers = sample.registers.toSeq.sortBy(_._1))
      driver.record("f04-expect", "instruction" -> Json.fromBigInt(sample.bits),
        "source_pc" -> Json.fromBigInt(sample.pc), "taken" -> Json.fromBoolean(expected.taken),
        "source_tag" -> Json.fromBoolean(tag),
        "source_mode" -> Json.fromInt(sample.mode), "source_virtual" -> Json.fromBoolean(sample.virtual),
        "main_cfg" -> Json.fromBigInt(sample.cfg), "main_lo" -> Json.fromInt(0x1000), "main_hi" -> Json.fromInt(0x3000),
        "resolved_target" -> Json.fromBigInt(expected.target), "next_pc" -> Json.fromBigInt(expected.next),
        "cause" -> sample.expectedCause.map(Json.fromInt).getOrElse(Json.Null),
        "tval" -> sample.expectedCause.map(cause => Json.fromBigInt(
          if (cause == 2) sample.bits else expected.target)).getOrElse(Json.Null),
        "reason" -> Json.fromInt(if (sample.expectedCause.exists(_ != 2)) 4 else 6),
        "initial_gprs" -> Json.fromFields(sample.registers.toSeq.map { case (index, value) => index.toString -> Json.fromBigInt(value) }),
        "initial_csrs" -> Json.fromFields(config.map { case (address, value) => address.toHexString -> Json.fromBigInt(value) }))
      val source = driver.execute(sample.pc, FDIControlInstruction(sample.bits, tag = tag,
        bytes = expected.bytes, branchType = 1, predictedTaken = sample.prediction,
        predictedTarget = Some(expected.target)))
      val outputs = source.outputs.filter(_.kind == "branch")
      assert(outputs.size == 1, s"${sample.name}: the source must have one live Branch result")
      val output = outputs.head
      assert(output.target == expected.next, s"${sample.name}: target or instruction length is incorrect")
      assert(!output.rfWen && !output.callEvent, s"${sample.name}: branch changed an integer or call owner")
      driver.expectNoWrite(source)
      sample.expectedCause match {
        case Some(cause) =>
          driver.expectTrap(source, cause, if (cause == 2) sample.bits else expected.target,
            reason = if (cause == 2) 6 else 4)
          assert(!output.redirect, s"${sample.name}: a rejected branch redirected normally")
        case None =>
          driver.expectRetired(source)
          assert(output.exception == 0, s"${sample.name}: a permitted branch produced an exception")
          assert(output.redirect == (sample.prediction != expected.taken),
            s"${sample.name}: prediction recovery changed with permission checks")
          driver.expectSpecial(0x8b3, 6)
      }
      for (address <- 0x8b0 to 0x8b2) driver.expectSpecial(address, config.toMap.apply(address))
      driver.record("f04-case", "name" -> Json.fromString(sample.name),
        "instruction" -> Json.fromBigInt(sample.bits), "source_pc" -> Json.fromBigInt(sample.pc),
        "taken" -> Json.fromBoolean(expected.taken), "expected_target" -> Json.fromBigInt(expected.target),
        "expected_next" -> Json.fromBigInt(expected.next), "bytes" -> Json.fromInt(expected.bytes),
        "expected_cause" -> sample.expectedCause.map(Json.fromInt).getOrElse(Json.Null))
    }
    configurationAndCancellation(driver)
    fetchFaults(driver)
  }

  private def configurationAndCancellation(driver: FDIFullControlDriver): Unit = {
    for (configurationFlush <- Seq(false, true)) {
      driver.caseName = s"f04-cancel-configuration-$configurationFlush"
      driver.setup(mainCfg = 2, trustedHi = 0x1808,
        csrs = specials ++ Seq(0x8b3 -> BigInt(6)) ++ bound(0, 0x1888, 0x1890, configurationFlush),
        registers = Seq(8 -> BigInt(1), 9 -> BigInt(1), 12 -> BigInt(0)))
      val older = if (configurationFlush) {
        FDIControlInstruction((BigInt(0x8c8) << 20) | (BigInt(12) << 15) | 0x1073)
      } else {
        FDIControlInstruction(branch(0, 0, 0, 128), branchType = 1, predictedTaken = false)
      }
      val younger = FDIControlInstruction(branch(0, 8, 9, 128),
        tag = sourceTag(0x1808, 0, false, 2, trustedHi = 0x1808),
        branchType = 1, predictedTaken = true, predictedTarget = Some(0x1888))
      val sources = driver.issueBatch(0x1804, Seq(older, younger))
      driver.until("older recovery cancels the younger branch", 2000) {
        sources.head.retired >= 0 && sources(1).canceled >= 0
      }
      driver.idle(32)
      assert(sources(1).retired < 0 && sources(1).trap.isEmpty,
        "A canceled branch reached architectural retirement or the trap owner")
      assert(sources(1).outputs.forall(_.cycle <= sources(1).canceled),
        "A canceled branch generated a late live FU result")
      driver.expectNoWrite(sources(1))
      driver.expectSpecial(0x8b3, 6)
      val retried = driver.execute(0x1808, younger)
      driver.expectTrap(retried, 24, 0x1888, reason = 4)
      driver.expectNoWrite(retried)
      driver.record("f04-cancel-reexecute", "configuration_flush" -> Json.fromBoolean(configurationFlush),
        "canceled_cycle" -> Json.fromInt(sources(1).canceled), "reexecute_pc" -> Json.fromInt(0x1808))
    }
  }

  private def fetchFaults(driver: FDIFullControlDriver): Unit = {
    for (sourceFault <- Seq(false, true); cause <- Seq(1, 12)) {
      driver.caseName = s"f04-fetch-source-$sourceFault-cause-$cause"
      driver.setup(csrs = Seq(0x8b3 -> BigInt(6), 0x8b0 -> BigInt(0x4040)),
        registers = Seq(8 -> BigInt(1), 9 -> BigInt(1)))
      val source = driver.execute(sourcePc, FDIControlInstruction(branch(0, 8, 9, 58),
        tag = sourceTag(sourcePc, 0, false, 2),
        branchType = 1, predictedTaken = true, predictedTarget = Some(0x4040),
        fetchException = if (sourceFault) 1 << cause else 0))
      if (sourceFault) driver.expectTrap(source, cause, sourcePc, reason = 6)
      else {
        driver.expectRetired(source)
        val target = driver.execute(0x4040, FDIControlInstruction(0x13,
          tag = sourceTag(0x4040, 0, false, 2), fetchException = 1 << cause))
        driver.expectTrap(target, cause, 0x4040, reason = 6)
        driver.expectNoWrite(target)
      }
      driver.expectNoWrite(source)
      driver.record("f04-fetch-fault", "source_fault" -> Json.fromBoolean(sourceFault),
        "cause" -> Json.fromInt(cause))
    }
  }

  private def runDisabled(driver: FDIFullControlDriver): Unit = {
    val encodings = Seq(0, 1, 4, 5, 6, 7).map(branch(_, 8, 9, 58)) ++
      Seq(compressed(false, 8, -70), compressed(true, 8, 58))
    for ((bits, index) <- encodings.zipWithIndex; left <- Seq(BigInt(0), mask64);
      predicted <- Seq(false, true)) {
      val registers = Map(8 -> left, 9 -> BigInt(0))
      val expected = decode(bits, sourcePc, registers)
      driver.caseName = s"f04-off-$index-left-$left-predicted-$predicted"
      driver.setup(registers = registers.toSeq.sortBy(_._1))
      val source = driver.execute(sourcePc, FDIControlInstruction(bits, tag = false,
        bytes = expected.bytes, branchType = 1, predictedTaken = predicted, predictedTarget = Some(expected.target)))
      driver.expectRetired(source)
      driver.expectNoWrite(source)
      val outputs = source.outputs.filter(_.kind == "branch")
      assert(outputs.size == 1 && outputs.head.target == expected.next && outputs.head.exception == 0)
      assert(!outputs.head.rfWen && !outputs.head.callEvent && outputs.head.redirect == (predicted != expected.taken))
      driver.record("f04-off-case", "instruction" -> Json.fromBigInt(bits),
        "bytes" -> Json.fromInt(expected.bytes), "taken" -> Json.fromBoolean(expected.taken),
        "expected_next" -> Json.fromBigInt(expected.next))
    }
  }
}
