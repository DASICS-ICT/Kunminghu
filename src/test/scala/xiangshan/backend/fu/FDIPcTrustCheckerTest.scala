// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.util.Random

// chiseltest requires Module; the production checker has no clock or reset.
class FDIPcTrustCheckerHarness extends Module {
  val io = IO(new FDIPcTrustCheckerIO)
  private val checker = Module(new FDIPcTrustChecker)
  io <> checker.io
}

class FDIPcTrustCheckerTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDIPcTrustChecker"

  it should "classify a complete instruction start PC using its source mode" in {
    val runRoot = Paths.get(sys.props.getOrElse(
      "p03.runRoot",
      throw new IllegalArgumentException("Set p03.runRoot to an absolute build output directory")
    ))
    require(runRoot.isAbsolute, "p03.runRoot must be absolute")
    require(Paths.get("").toRealPath() == runRoot.toRealPath(),
      "The test working directory must be p03.runRoot")

    val maxAddress = (BigInt(1) << 64) - 1
    val signBit = BigInt(1) << 63
    val seed = 0x503033L
    val randomSamples = 4096

    case class Input(
      pc: BigInt = 0x1003,
      sourcePrivilege: Int = 0,
      sourceVirtual: Boolean = false,
      uEnable: Boolean = true,
      sEnable: Boolean = true,
      uBoundLo: BigInt = 0x1003,
      uBoundHi: BigInt = 0x1013,
      sBoundLo: BigInt = 0x2005,
      sBoundHi: BigInt = 0x2015
    )
    case class Sample(category: String, label: String, input: Input, expected: Option[Boolean])

    // Mathematical displacement avoids a fixed-width endpoint calculation.
    // Mode classes are an explicit table, independent of RTL mode decoding.
    val modeClasses = Vector("host-user", "guest-user", "host-supervisor", "guest-supervisor",
      "invalid", "invalid", "machine", "invalid")
    def inRange(pc: BigInt, lo: BigInt, hi: BigInt): Boolean = {
      val displacement = pc - lo
      val span = hi - lo
      displacement >= 0 && displacement < span
    }
    def sourceIndex(input: Input): Int = input.sourcePrivilege * 2 + (if (input.sourceVirtual) 1 else 0)
    def oracle(input: Input): Boolean = modeClasses(sourceIndex(input)) match {
      case "host-user" => input.uEnable && !inRange(input.pc, input.uBoundLo, input.uBoundHi)
      case "host-supervisor" => input.sEnable && !inRange(input.pc, input.sBoundLo, input.sBoundHi)
      case "guest-user" => input.uEnable
      case "guest-supervisor" => input.sEnable
      case "machine" => false
      case "invalid" => true
    }

    val samples = mutable.ArrayBuffer.empty[Sample]
    def directed(category: String, label: String, input: Input, expected: Boolean): Unit =
      samples += Sample(category, label, input, Some(expected))

    val matrixPcs = Vector(
      (BigInt(0x1003), true, false),
      (BigInt(0x2005), false, true),
      (BigInt(0x3000), false, false)
    )
    for (uEnable <- Seq(false, true); sEnable <- Seq(false, true);
      (pc, userHit, supervisorHit) <- matrixPcs) {
      // Entries are ordered HU, VU, HS, VS, reserved, reserved+V, M, M+V.
      val expected = Vector(uEnable && !userHit, uEnable, sEnable && !supervisorHit, sEnable,
        true, true, false, true)
      for (mode <- expected.indices) {
        directed("mode_enable_matrix", s"mode=$mode uEnable=$uEnable sEnable=$sEnable pc=$pc",
          Input(pc = pc, sourcePrivilege = mode / 2, sourceVirtual = mode % 2 != 0,
            uEnable = uEnable, sEnable = sEnable), expected(mode))
      }
    }

    for (privilege <- Seq(0, 1)) {
      def geometry(category: String, label: String, pc: BigInt, lo: BigInt, hi: BigInt,
        expected: Boolean): Unit = {
        // The unselected bank authorizes no PC, so selecting it cannot hide a miss.
        val base = Input(pc = pc, sourcePrivilege = privilege,
          uBoundLo = maxAddress, uBoundHi = 0, sBoundLo = maxAddress, sBoundHi = 0)
        val input = if (privilege == 0) base.copy(uBoundLo = lo, uBoundHi = hi)
          else base.copy(sBoundLo = lo, sBoundHi = hi)
        directed(category, s"privilege=$privilege $label", input, expected)
      }
      geometry("boundaries", "lower bound is trusted", 0x1003, 0x1003, 0x1013, false)
      geometry("boundaries", "last byte below upper bound is trusted", 0x1012, 0x1003, 0x1013, false)
      geometry("boundaries", "upper bound is untrusted", 0x1013, 0x1003, 0x1013, true)
      geometry("boundaries", "PC below lower bound is untrusted", 0x1002, 0x1003, 0x1013, true)
      geometry("boundaries", "empty interval is untrusted", 0x1003, 0x1003, 0x1003, true)
      geometry("boundaries", "reversed interval is untrusted", 0x1003, 0x1013, 0x1003, true)
      geometry("boundaries", "zero PC in explicit range", 0, 0, 1, false)
      geometry("boundaries", "all-zero bounds authorize nothing", 0, 0, 0, true)
      for (lowBits <- 0 until 8) {
        val pc = BigInt(0x1000 + lowBits)
        geometry("exact_low_bits", s"low bits=$lowBits in a single-byte range", pc, pc, pc + 1, false)
        geometry("exact_low_bits", s"low bits=$lowBits immediately below a single-byte range",
          pc, pc + 1, pc + 2, true)
        geometry("exact_low_bits", s"low bits=$lowBits at the excluded upper bound",
          pc + 1, pc, pc + 1, true)
      }
      geometry("unsigned_addresses", "PC above sign bit with low lower bound", signBit + 3, 0,
        signBit + 4, false)
      geometry("unsigned_addresses", "PC below sign bit with high lower bound", signBit - 1,
        signBit + 3, signBit + 4, true)
      geometry("unsigned_addresses", "interval crosses sign bit", signBit, signBit - 1,
        signBit + 1, false)
      geometry("maximum_address", "highest representable exclusive bound", maxAddress - 1,
        maxAddress - 1, maxAddress, false)
      geometry("maximum_address", "maximum PC cannot be below a representable upper bound",
        maxAddress, 0, maxAddress, true)
      geometry("maximum_address", "maximum PC does not wrap into zero range", maxAddress, 0, 8, true)
      for (bit <- 3 until 64) {
        val low = BigInt(3)
        val high = low + (BigInt(1) << bit)
        geometry("address_high_bits", s"PC bit $bit cannot alias low range", high, low, low + 1, true)
        geometry("address_high_bits", s"PC bit $bit preserved in matching range", high,
          high, high + 1, false)
        geometry("address_high_bits", s"range bit $bit cannot alias low PC", low,
          high, high + 1, true)
      }
    }

    for (privilege <- Seq(0, 1); virtual <- Seq(false, true); pc <- Seq(BigInt(0), maxAddress)) {
      directed("disabled_invalid_bounds", "disabled protection ignores malformed selected bounds",
        Input(pc = pc, sourcePrivilege = privilege, sourceVirtual = virtual,
          uEnable = false, sEnable = false, uBoundLo = maxAddress, uBoundHi = 0,
          sBoundLo = maxAddress, sBoundHi = 0), false)
    }
    directed("continuous_inputs", "A host user begins inside U range", Input(), false)
    directed("continuous_inputs", "B changes only the source to host supervisor",
      Input(sourcePrivilege = 1), true)
    directed("continuous_inputs", "C supervisor protection is immediately disabled",
      Input(sourcePrivilege = 1, sEnable = false), false)
    directed("continuous_inputs", "D enabled guest cannot borrow matching host range",
      Input(sourceVirtual = true), true)
    directed("continuous_inputs", "E disabled guest protection bypasses classification",
      Input(sourceVirtual = true, uEnable = false), false)
    directed("continuous_inputs", "F legal machine mode is trusted",
      Input(sourcePrivilege = 3), false)
    directed("continuous_inputs", "G virtual machine encoding is invalid even when disabled",
      Input(sourcePrivilege = 3, sourceVirtual = true, uEnable = false, sEnable = false), true)
    directed("continuous_inputs", "H user upper endpoint is immediately excluded",
      Input(pc = 0x1013), true)

    val random = new Random(seed)
    val anchors = Vector(BigInt(0), BigInt(0x1003), signBit - 32, signBit + 3, maxAddress - 63)
    for (index <- 0 until randomSamples) {
      val base = Input(pc = BigInt(64, random), sourcePrivilege = random.nextInt(4),
        sourceVirtual = random.nextBoolean(), uEnable = random.nextBoolean(), sEnable = random.nextBoolean(),
        uBoundLo = BigInt(64, random), uBoundHi = BigInt(64, random),
        sBoundLo = BigInt(64, random), sBoundHi = BigInt(64, random))
      val lo = anchors(random.nextInt(anchors.size))
      val hi = lo + 32
      val nearby = Vector((lo - 1).max(BigInt(0)), lo, lo + 1, hi - 1, hi, hi + 1)
      val (category, input) = index % 4 match {
        case 0 => "uniform" -> base
        case 1 => "boundary_neighborhood" -> base.copy(pc = nearby(random.nextInt(nearby.size)),
          uBoundLo = lo, uBoundHi = hi, sBoundLo = lo, sBoundHi = hi)
        case 2 =>
          val userSelected = random.nextBoolean()
          val inside = lo + random.nextInt(32)
          val banks = base.copy(pc = inside, uBoundLo = lo, uBoundHi = hi, sBoundLo = hi, sBoundHi = hi + 1)
          "bank_selection" -> (if (userSelected) banks else banks.copy(
            uBoundLo = hi, uBoundHi = hi + 1, sBoundLo = lo, sBoundHi = hi))
        case _ => "extreme_addresses" -> base.copy(pc = if (random.nextBoolean()) 0 else maxAddress,
          uBoundLo = 0, uBoundHi = maxAddress, sBoundLo = maxAddress, sBoundHi = 0)
      }
      samples += Sample(category, s"random index=$index", input, None)
    }

    // Reject an inconsistent explicit expectation before consulting the DUT.
    for ((sample, index) <- samples.zipWithIndex) {
      val input = sample.input
      require(Seq(input.pc, input.uBoundLo, input.uBoundHi, input.sBoundLo, input.sBoundHi)
        .forall(value => value >= 0 && value <= maxAddress))
      require(input.sourcePrivilege >= 0 && input.sourcePrivilege < 4)
      sample.expected.foreach { expected =>
        withClue(s"${sample.category} ${sample.label} case=$index input=$input: ") {
          assert(oracle(input) == expected)
        }
      }
    }

    def countsJson(counts: Array[Array[Int]]): String = counts.indices.map { index =>
      s""""$index":{"trusted":${counts(index)(0)},"notTrusted":${counts(index)(1)}}"""
    }.mkString("{", ",", "}")
    class Coverage {
      var trusted = 0
      var notTrusted = 0
      val categories = mutable.Map.empty[String, Array[Int]]
      val sources = Array.fill(8, 2)(0)
      val enableCombinations = Array.fill(4, 2)(0)
      val modeEnableMatrix = Array.fill(32, 2)(0)
      def add(sample: Sample, expected: Boolean): Unit = {
        val result = if (expected) 1 else 0
        if (expected) notTrusted += 1 else trusted += 1
        val input = sample.input
        val enables = (if (input.uEnable) 2 else 0) + (if (input.sEnable) 1 else 0)
        categories.getOrElseUpdate(sample.category, Array(0, 0))(result) += 1
        sources(sourceIndex(input))(result) += 1
        enableCombinations(enables)(result) += 1
        modeEnableMatrix(sourceIndex(input) * 4 + enables)(result) += 1
      }
      def json: String = {
        val categoryJson = categories.toSeq.sortBy(_._1).map { case (name, counts) =>
          s""""$name":{"trusted":${counts(0)},"notTrusted":${counts(1)}}"""
        }.mkString("{", ",", "}")
        s"""{"cases":${trusted + notTrusted},"trusted":$trusted,"notTrusted":$notTrusted,"categories":$categoryJson,"sourceMode":${countsJson(sources)},"enableCombinations":${countsJson(enableCombinations)},"modeEnableMatrix":${countsJson(modeEnableMatrix)}}"""
      }
    }

    test(new FDIPcTrustCheckerHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      TargetDirAnnotation("rtl-simulation")
    )) { dut =>
      val directedCoverage = new Coverage
      val randomCoverage = new Coverage
      for ((sample, index) <- samples.zipWithIndex) {
        val input = sample.input
        dut.io.pc.poke(input.pc.U(64.W))
        dut.io.sourcePrivilege.poke(input.sourcePrivilege.U(2.W))
        dut.io.sourceVirtual.poke(input.sourceVirtual.B)
        dut.io.uEnable.poke(input.uEnable.B)
        dut.io.sEnable.poke(input.sEnable.B)
        dut.io.uBoundLo.poke(input.uBoundLo.U(64.W))
        dut.io.uBoundHi.poke(input.uBoundHi.U(64.W))
        dut.io.sBoundLo.poke(input.sBoundLo.U(64.W))
        dut.io.sBoundHi.poke(input.sBoundHi.U(64.W))
        val expected = oracle(input)
        // Observe every replacement input without advancing a clock.
        val actual = dut.io.notTrusted.peek().litToBoolean
        withClue(s"${sample.category} ${sample.label} seed=$seed case=$index input=$input expected=$expected actual=$actual: ") {
          dut.io.notTrusted.expect(expected.B)
        }
        (if (sample.expected.nonEmpty) directedCoverage else randomCoverage).add(sample, expected)
      }
      val requiredCategories = Set("mode_enable_matrix", "boundaries", "exact_low_bits", "unsigned_addresses",
        "maximum_address", "address_high_bits", "disabled_invalid_bounds", "continuous_inputs")
      assert(directedCoverage.categories.keySet == requiredCategories)
      assert(directedCoverage.modeEnableMatrix.forall(_.sum > 0))
      assert(directedCoverage.trusted > 0 && directedCoverage.notTrusted > 0)
      assert(randomCoverage.trusted + randomCoverage.notTrusted == randomSamples)
      assert(randomCoverage.trusted > 0 && randomCoverage.notTrusted > 0)
      assert(randomCoverage.categories.size == 4 && randomCoverage.categories.values.forall(_.sum == 1024))
      val trusted = directedCoverage.trusted + randomCoverage.trusted
      val notTrusted = directedCoverage.notTrusted + randomCoverage.notTrusted
      val result = s"""{"status":"PASS","backend":"verilator","cases":${samples.size},"directedCases":${directedCoverage.trusted + directedCoverage.notTrusted},"randomCases":$randomSamples,"seed":$seed,"trusted":$trusted,"notTrusted":$notTrusted,"directed":${directedCoverage.json},"random":${randomCoverage.json}}"""
      Files.write(runRoot.resolve("result.json"), (result + "\n").getBytes(StandardCharsets.UTF_8))
      println(s"FDIPcTrustChecker RTL PASS: $result")
    }
  }
}
