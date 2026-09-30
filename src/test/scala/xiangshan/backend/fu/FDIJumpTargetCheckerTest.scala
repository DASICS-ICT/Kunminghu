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

// The harness supplies the Module interface required by chiseltest.
class FDIJumpTargetCheckerHarness extends Module {
  val io = IO(new FDIJumpTargetCheckerIO)
  private val checker = Module(new FDIJumpTargetChecker)
  io <> checker.io
}

class FDIJumpTargetCheckerTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDIJumpTargetChecker"

  it should "authorize exact targets through a valid range or a nonzero special target" in {
    val runRoot = Paths.get(sys.props.getOrElse(
      "p06.runRoot",
      throw new IllegalArgumentException("Set p06.runRoot to an absolute build output directory")
    ))
    require(runRoot.isAbsolute, "p06.runRoot must be absolute")
    require(Paths.get("").toRealPath() == runRoot.toRealPath(),
      "The test working directory must be p06.runRoot")

    val maxAddress = (BigInt(1) << 64) - 1
    val signBit = BigInt(1) << 63
    val seed = 0x503036L
    val randomSamples = 4096

    case class Entry(boundLo: BigInt = 0x1003, boundHi: BigInt = 0x1023,
      entryValid: Boolean = true)
    def bank(entries: (Int, Entry)*): Vector[Entry] = {
      require(entries.map(_._1).distinct.size == entries.size)
      entries.foldLeft(Vector.fill(4)(Entry(entryValid = false))) {
        case (result, (index, entry)) => result.updated(index, entry)
      }
    }
    case class Access(target: BigInt = 0x1003, entries: Vector[Entry] = bank(),
      specialTargets: Vector[BigInt] = Vector.fill(3)(BigInt(0)))
    case class Sample(category: String, label: String, input: Access,
      expected: Option[Boolean], uniqueSource: Option[Int] = None)

    // Mathematical interval membership uses unbounded integers. Special targets
    // form a separate set with zero removed; no hardware helper is called.
    def oracleSources(input: Access): Vector[Int] = {
      val intervals = input.entries.zipWithIndex.collect {
        case (entry, index) if entry.entryValid &&
          (input.target - entry.boundLo).signum >= 0 &&
          (entry.boundHi - input.target).signum > 0 => index
      }
      val special = input.specialTargets.zipWithIndex.collect {
        case (address, index) if Set(address).diff(Set(BigInt(0))).contains(input.target) => 4 + index
      }
      intervals ++ special
    }

    val samples = mutable.ArrayBuffer.empty[Sample]
    def directed(category: String, label: String, input: Access, expected: Boolean,
      uniqueSource: Option[Int] = None): Unit =
      samples += Sample(category, label, input, Some(expected), uniqueSource)

    directed("all_disabled", "all intervals invalid and all special targets zero", Access(), false)
    directed("all_disabled", "zero does not match disabled special targets", Access(target = 0), false)
    for (position <- 0 until 4) {
      val input = Access(entries = bank(position -> Entry()))
      directed("single_range", s"only range $position authorizes", input, true, Some(position))
      directed("single_range", s"range $position becomes invalid",
        input.copy(entries = bank(position -> Entry(entryValid = false))), false)
      for ((label, target, lo, hi, expected) <- Vector(
        ("lower bound included", BigInt(0x1003), BigInt(0x1003), BigInt(0x1023), true),
        ("last target before upper bound", BigInt(0x1022), BigInt(0x1003), BigInt(0x1023), true),
        ("upper bound excluded", BigInt(0x1023), BigInt(0x1003), BigInt(0x1023), false),
        ("target below lower bound", BigInt(0x1002), BigInt(0x1003), BigInt(0x1023), false),
        ("empty interval", BigInt(0x1003), BigInt(0x1003), BigInt(0x1003), false),
        ("reversed interval", BigInt(0x1003), BigInt(0x1023), BigInt(0x1003), false),
        ("zero permitted by explicit range", BigInt(0), BigInt(0), BigInt(8), true),
        ("one-byte interval", BigInt(0x1005), BigInt(0x1005), BigInt(0x1006), true)
      )) {
        directed("range_geometry", s"range $position $label",
          Access(target = target, entries = bank(position -> Entry(lo, hi))), expected)
      }
    }
    directed("multiple_ranges", "first and last ranges authorize",
      Access(entries = bank(0 -> Entry(), 3 -> Entry())), true)
    directed("multiple_ranges", "all four ranges authorize",
      Access(entries = Vector.fill(4)(Entry())), true)
    directed("multiple_ranges", "three rejecting valid ranges cannot mask the last match",
      Access(entries = Vector.fill(3)(Entry(boundHi = 0x1003)) :+ Entry()), true, Some(3))

    for (low <- 0 until 8) {
      val target = BigInt(0x1000 + low)
      directed("precise_low_bits", s"exact one-byte range at low bits $low",
        Access(target = target, entries = bank(2 -> Entry(target, target + 1))), true, Some(2))
      directed("precise_low_bits", s"lower bound one byte above low bits $low",
        Access(target = target, entries = bank(2 -> Entry(target + 1, target + 9))), false)
      directed("precise_low_bits", s"upper bound equals target at low bits $low",
        Access(target = target, entries = bank(2 -> Entry(target - 1, target))), false)
    }
    for (bit <- 13 until 64) {
      val low = BigInt(0x1003)
      val high = low + (BigInt(1) << bit)
      directed("range_high_bits", s"target bit $bit cannot alias low range",
        Access(target = high, entries = bank(1 -> Entry())), false)
      directed("range_high_bits", s"target and range retain bit $bit",
        Access(target = high, entries = bank(1 -> Entry(high, high + 32))), true)
      directed("range_high_bits", s"low target cannot alias range bit $bit",
        Access(target = low, entries = bank(1 -> Entry(high, high + 32))), false)
    }
    for ((label, target, lo, hi, expected) <- Vector(
      ("range crosses sign boundary", signBit, signBit - 1, signBit + 1, true),
      ("high target above low lower bound", signBit + 8, BigInt(0), signBit + 16, true),
      ("low target below high lower bound", BigInt(8), signBit, signBit + 16, false),
      ("high reversed interval cannot authorize", signBit + 8, signBit, BigInt(16), false),
      ("largest representable range includes preceding address", maxAddress - 1, BigInt(0), maxAddress, true),
      ("maximum address excluded by largest upper bound", maxAddress, BigInt(0), maxAddress, false),
      ("maximum empty range", maxAddress, maxAddress, maxAddress, false)
    )) {
      directed("unsigned_extremes", label, Access(target = target, entries = bank(3 -> Entry(lo, hi))), expected)
    }

    for (special <- 0 until 3) {
      def only(value: BigInt): Vector[BigInt] = Vector.fill(3)(BigInt(0)).updated(special, value)
      for (target <- Vector(BigInt(1), BigInt(0x1003), signBit, maxAddress)) {
        directed("single_special", s"special target $special authorizes $target",
          Access(target = target, specialTargets = only(target)), true, Some(4 + special))
        directed("single_special", s"special target $special rejects a different target",
          Access(target = target ^ 1, specialTargets = only(target)), false)
      }
      directed("zero_special", s"zero disables special target $special",
        Access(target = 0, specialTargets = only(0)), false)
      directed("zero_special", s"nonzero special target $special cannot authorize zero",
        Access(target = 0, specialTargets = only(maxAddress)), false)
      directed("zero_special", s"zero special target $special does not override an explicit zero range",
        Access(target = 0, entries = bank(special -> Entry(0, 8)), specialTargets = only(0)), true)
      // Every address bit participates in special-target equality, including low bits.
      for (bit <- 0 until 64) {
        val target = BigInt(0x1003)
        val changed = target ^ (BigInt(1) << bit)
        directed("special_address_bits", s"special $special rejects differing target bit $bit",
          Access(target = changed, specialTargets = only(target)), false)
        directed("special_address_bits", s"special $special rejects differing stored bit $bit",
          Access(target = target, specialTargets = only(changed)), false)
        directed("special_address_bits", s"special $special retains matching bit $bit",
          Access(target = changed, specialTargets = only(changed)), true, Some(4 + special))
      }
    }
    directed("combined_sources", "three special targets all match",
      Access(specialTargets = Vector.fill(3)(BigInt(0x1003))), true)
    directed("combined_sources", "all seven sources authorize",
      Access(entries = Vector.fill(4)(Entry()), specialTargets = Vector.fill(3)(BigInt(0x1003))), true)
    directed("combined_sources", "special target authorizes despite empty and reversed ranges",
      Access(entries = Vector(Entry(0, 0), Entry(32, 16), Entry(0x1003, 0x1003), Entry(0x2000, 0)),
        specialTargets = Vector(0, 0x1003, 0).map(BigInt(_))), true, Some(5))

    val orderInputs = Vector(
      (Access(entries = bank(2 -> Entry())), true),
      (Access(entries = bank(0 -> Entry(), 3 -> Entry())), true),
      (Access(entries = bank(1 -> Entry(boundLo = 0x1004))), false),
      (Access(specialTargets = Vector(0, 0, 0x1003).map(BigInt(_))), true)
    )
    for (((input, expected), index) <- orderInputs.zipWithIndex;
      (entries, permutation) <- input.entries.permutations.zipWithIndex) {
      directed("entry_order", s"input $index permutation $permutation", input.copy(entries = entries), expected)
    }

    directed("continuous_inputs", "A range authorizes", Access(entries = bank(0 -> Entry())), true)
    directed("continuous_inputs", "B range removed", Access(), false)
    directed("continuous_inputs", "C special target authorizes maximum address",
      Access(target = maxAddress, specialTargets = Vector(maxAddress, 0, 0)), true)
    directed("continuous_inputs", "D target differs with unchanged special target",
      Access(target = maxAddress - 1, specialTargets = Vector(maxAddress, 0, 0)), false)
    directed("continuous_inputs", "E zero allowed by explicit range",
      Access(target = 0, entries = bank(3 -> Entry(0, 8))), true)
    directed("continuous_inputs", "F zero rejected after range invalidation", Access(target = 0), false)

    val random = new Random(seed)
    def randomEntry(): Entry = Entry(BigInt(64, random), BigInt(64, random), random.nextBoolean())
    val anchors = Vector(BigInt(0), BigInt(0x1003), signBit - 32, signBit + 3, maxAddress - 63)
    for (index <- 0 until randomSamples) {
      val uniform = Access(BigInt(64, random), Vector.fill(4)(randomEntry()), Vector.fill(3)(BigInt(64, random)))
      val position = random.nextInt(4)
      val special = random.nextInt(3)
      val start = BigInt(64, random) % (maxAddress - 128)
      val (category, input) = index % 8 match {
        case 0 => "uniform" -> uniform
        case 1 =>
          val lo = anchors(random.nextInt(anchors.size))
          val hi = lo + 32
          val targets = Vector((lo - 1).max(BigInt(0)), lo, lo + 1, hi - 1, hi, hi + 1)
          "boundary_neighborhood" -> uniform.copy(target = targets(random.nextInt(targets.size)),
            entries = bank(position -> Entry(lo, hi, random.nextBoolean())))
        case 2 => "single_range" -> Access(target = start + random.nextInt(32),
          entries = bank(position -> Entry(start, start + 32)))
        case 3 =>
          val target = BigInt(64, random).max(BigInt(1))
          "special_match" -> Access(target = target,
            specialTargets = Vector.fill(3)(BigInt(0)).updated(special, target))
        case 4 => "zero_disabled" -> Access(target = 0,
          entries = Vector.fill(4)(randomEntry().copy(entryValid = false)))
        case 5 =>
          val target = BigInt(64, random).max(BigInt(1))
          "special_bit_mismatch" -> Access(target = target ^ (BigInt(1) << random.nextInt(64)),
            specialTargets = Vector.fill(3)(BigInt(0)).updated(special, target))
        case 6 => "entry_permutation" -> uniform.copy(entries = random.shuffle(uniform.entries))
        case _ => "maximum_neighborhood" -> Access(target = maxAddress - random.nextInt(4),
          entries = bank(position -> Entry(0, maxAddress)),
          specialTargets = Vector.fill(3)(BigInt(0)).updated(special, if (random.nextBoolean()) maxAddress else BigInt(0)))
      }
      samples += Sample(category, s"random index=$index", input, None)
    }

    // Explicit expected outcomes are checked before observing the implementation.
    for ((sample, index) <- samples.zipWithIndex) {
      val input = sample.input
      require(input.entries.size == 4 && input.specialTargets.size == 3)
      require((Vector(input.target) ++ input.specialTargets ++
        input.entries.flatMap(e => Vector(e.boundLo, e.boundHi))).forall(a => a >= 0 && a <= maxAddress))
      val sources = oracleSources(input)
      val diagnostic = s"${sample.category} ${sample.label} seed=$seed case=$index input=$input"
      sample.expected.foreach { expected =>
        withClue(s"$diagnostic explicit=$expected oracleSources=$sources: ") { assert(sources.nonEmpty == expected) }
      }
      sample.uniqueSource.foreach { source =>
        withClue(s"$diagnostic uniqueSource=$source oracleSources=$sources: ") { assert(sources == Vector(source)) }
      }
    }

    class Coverage {
      var allow = 0
      var deny = 0
      val categories = mutable.Map.empty[String, Array[Int]]
      val uniqueSources = Array.fill(7)(0)
      val sourceCounts = Array.fill(8)(0)
      def add(sample: Sample, sources: Vector[Int]): Unit = {
        val outcome = if (sources.nonEmpty) 1 else 0
        if (sources.nonEmpty) allow += 1 else deny += 1
        categories.getOrElseUpdate(sample.category, Array(0, 0))(outcome) += 1
        sourceCounts(sources.size) += 1
        if (sources.size == 1) uniqueSources(sources.head) += 1
      }
      def json: String = {
        val categoryJson = categories.toSeq.sortBy(_._1).map { case (name, counts) =>
          s""""$name":{"allow":${counts(1)},"deny":${counts(0)}}"""
        }.mkString("{", ",", "}")
        s"""{"cases":${allow + deny},"allow":$allow,"deny":$deny,"categories":$categoryJson,"uniqueSources":${uniqueSources.mkString("[", ",", "]")},"authorizingSourceCount":${sourceCounts.mkString("[", ",", "]")}}"""
      }
    }

    test(new FDIJumpTargetCheckerHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      TargetDirAnnotation("rtl-simulation")
    )) { dut =>
      val directedCoverage = new Coverage
      val randomCoverage = new Coverage
      for ((sample, index) <- samples.zipWithIndex) {
        val input = sample.input
        dut.io.target.poke(input.target.U(64.W))
        for ((entry, position) <- input.entries.zipWithIndex) {
          dut.io.entries(position).boundLo.poke(entry.boundLo.U(64.W))
          dut.io.entries(position).boundHi.poke(entry.boundHi.U(64.W))
          dut.io.entries(position).entryValid.poke(entry.entryValid.B)
        }
        dut.io.mainCallEntry.poke(input.specialTargets(0).U(64.W))
        dut.io.returnPC.poke(input.specialTargets(1).U(64.W))
        dut.io.activeZoneReturnPC.poke(input.specialTargets(2).U(64.W))
        val sources = oracleSources(input)
        // No clock edge is advanced; the output must reflect all current inputs.
        val actual = dut.io.allow.peek().litToBoolean
        withClue(s"${sample.category} ${sample.label} seed=$seed case=$index input=$input oracleSources=$sources actual=$actual: ") {
          dut.io.allow.expect(sources.nonEmpty.B)
        }
        (if (sample.expected.nonEmpty) directedCoverage else randomCoverage).add(sample, sources)
      }
      val requiredCategories = Set("all_disabled", "single_range", "range_geometry", "multiple_ranges",
        "precise_low_bits", "range_high_bits", "unsigned_extremes", "single_special", "zero_special",
        "special_address_bits", "combined_sources", "entry_order", "continuous_inputs")
      assert(directedCoverage.categories.keySet == requiredCategories)
      assert(directedCoverage.uniqueSources.forall(_ > 0))
      assert(directedCoverage.sourceCounts(0) > 0 && directedCoverage.sourceCounts(7) > 0)
      assert(randomCoverage.allow + randomCoverage.deny == randomSamples)
      assert(randomCoverage.allow > 0 && randomCoverage.deny > 0)
      assert(randomCoverage.categories.size == 8 && randomCoverage.categories.values.forall(_.sum == 512))
      val allowed = directedCoverage.allow + randomCoverage.allow
      val denied = directedCoverage.deny + randomCoverage.deny
      val result = s"""{"status":"PASS","backend":"verilator","cases":${samples.size},"directedCases":${directedCoverage.allow + directedCoverage.deny},"randomCases":$randomSamples,"seed":$seed,"allow":$allowed,"deny":$denied,"directed":${directedCoverage.json},"random":${randomCoverage.json}}"""
      Files.write(runRoot.resolve("result.json"), (result + "\n").getBytes(StandardCharsets.UTF_8))
      println(s"FDIJumpTargetChecker RTL PASS: $result")
    }
  }
}
