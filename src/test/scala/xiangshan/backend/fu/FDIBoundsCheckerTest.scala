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

// chiseltest requires Module; the production aggregator has no clock or reset.
class FDIBoundsCheckerHarness extends Module {
  val io = IO(new FDIBoundsCheckerIO)
  private val checker = Module(new FDIBoundsChecker)
  io <> checker.io
}

class FDIBoundsCheckerTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDIBoundsChecker"

  it should "require one complete authorization among sixteen current entries" in {
    val runRoot = Paths.get(sys.props.getOrElse(
      "p02.runRoot",
      throw new IllegalArgumentException("Set p02.runRoot to an absolute build output directory")
    ))
    require(runRoot.isAbsolute, "p02.runRoot must be absolute")
    require(Paths.get("").toRealPath() == runRoot.toRealPath(),
      "The test working directory must be p02.runRoot")

    val maxAddress = (BigInt(1) << 64) - 1
    val signBit = BigInt(1) << 63
    val seed = 0x503032L
    val randomSamples = 4096

    case class Entry(
      boundLo: BigInt = 0x1000,
      boundHi: BigInt = 0x1040,
      entryValid: Boolean = true,
      readAllowed: Boolean = true,
      writeAllowed: Boolean = true
    )
    def bank(entries: (Int, Entry)*): Vector[Entry] = {
      require(entries.map(_._1).distinct.size == entries.size)
      entries.foldLeft(Vector.fill(16)(Entry(entryValid = false))) {
        case (result, (index, entry)) => result.updated(index, entry)
      }
    }
    case class Access(
      address: BigInt = 0x1008,
      sizeLog2: Int = 0,
      operation: Int = 0,
      entries: Vector[Entry] = bank()
    )
    case class Sample(
      category: String,
      label: String,
      input: Access,
      expected: Option[Boolean],
      expectedSingleHit: Option[Int] = None
    )

    // Byte enumeration uses unbounded mathematical addresses. A permission and
    // every byte must belong to the same entry before that entry can authorize.
    val byteCounts = Vector(1, 2, 4, 8, 16)
    val permissionTable = Map(
      (false, false) -> Set.empty[Int],
      (true, false) -> Set(0),
      (false, true) -> Set(1),
      (true, true) -> Set(0, 1, 2)
    )
    def oracleHits(input: Access): Vector[Int] = {
      if (!byteCounts.indices.contains(input.sizeLog2)) Vector.empty
      else input.entries.indices.filter { index =>
        val entry = input.entries(index)
        entry.entryValid &&
          permissionTable((entry.readAllowed, entry.writeAllowed)).contains(input.operation) &&
          (0 until byteCounts(input.sizeLog2)).forall { offset =>
            val byteAddress = input.address + BigInt(offset)
            byteAddress <= maxAddress && byteAddress >= entry.boundLo && byteAddress < entry.boundHi
          }
      }.toVector
    }

    val samples = mutable.ArrayBuffer.empty[Sample]
    def directed(category: String, label: String, input: Access, expected: Boolean,
      singleHit: Option[Int] = None): Unit =
      samples += Sample(category, label, input, Some(expected), singleHit)

    for (size <- 0 to 4; operation <- 0 to 2) {
      directed("all_invalid", "all ranges and permissions match but all entries are invalid",
        Access(sizeLog2 = size, operation = operation), false)
      for (position <- 0 until 16) {
        val only = Entry(boundLo = 0x1003, readAllowed = operation != 1, writeAllowed = operation != 0)
        val input = Access(address = 0x1003, sizeLog2 = size, operation = operation,
          entries = bank(position -> only))
        directed("single_hit", s"only position $position authorizes", input, true, Some(position))
        val missingPermission = if (operation == 1) only.copy(writeAllowed = false)
          else only.copy(readAllowed = false)
        directed("single_hit_permission_denied", s"position $position lacks a required permission",
          input.copy(entries = bank(position -> missingPermission)), false)
      }
      directed("multiple_hits", "first and last entries both authorize",
        Access(sizeLog2 = size, operation = operation, entries = bank(0 -> Entry(), 15 -> Entry())), true)
      directed("multiple_hits", "all sixteen entries authorize",
        Access(sizeLog2 = size, operation = operation, entries = Vector.fill(16)(Entry())), true)
      val rejectingPrefix = Vector.fill(15)(Entry(boundHi = 0x1008)) :+ Entry()
      directed("multiple_hits", "earlier valid entries deny and the last entry authorizes",
        Access(sizeLog2 = size, operation = operation, entries = rejectingPrefix), true, Some(15))
    }

    for (position <- 0 until 16; permissions <- 0 until 4; operation <- 0 until 4) {
      val read = (permissions & 1) != 0
      val write = (permissions & 2) != 0
      val expected = operation match {
        case 0 => read
        case 1 => write
        case 2 => read && write
        case _ => false
      }
      directed("control_permissions", s"permission matrix at position $position",
        Access(sizeLog2 = position % 5, operation = operation,
          entries = bank(position -> Entry(readAllowed = read, writeAllowed = write))), expected)
    }

    for (size <- 0 to 4; operation <- 0 to 2) {
      val bytes = BigInt(1) << size
      val base = Access(address = 0x1000, sizeLog2 = size, operation = operation,
        entries = bank(7 -> Entry(boundHi = 0x1020)))
      def geometry(category: String, label: String, address: BigInt, lo: BigInt, hi: BigInt,
        expected: Boolean): Unit = directed(category, label,
        base.copy(address = address, entries = bank(7 -> Entry(boundLo = lo, boundHi = hi))), expected)

      geometry("boundaries", "lower bound included", 0x1000, 0x1000, 0x1020, true)
      geometry("boundaries", "last byte immediately below upper bound", BigInt(0x1020) - bytes,
        0x1000, 0x1020, true)
      geometry("boundaries", "last byte equals upper bound", BigInt(0x1020) - bytes + 1,
        0x1000, 0x1020, false)
      geometry("boundaries", "start equals upper bound", 0x1020, 0x1000, 0x1020, false)
      geometry("boundaries", "start below lower bound", 0x0fff, 0x1000, 0x1020, false)
      geometry("boundaries", "empty interval", 0x1000, 0x1000, 0x1000, false)
      geometry("boundaries", "reversed interval", 0x1000, 0x1020, 0x1000, false)
      geometry("boundaries", "address zero", 0, 0, 32, true)
      geometry("unaligned", "unaligned complete access", 0x1001, 0x1000, 0x1020, true)
      geometry("unaligned", "lower bound must not be rounded down", 0x1002, 0x1003, 0x1023, false)
      geometry("unaligned", "upper bound must not be rounded down", BigInt(0x1013) - bytes,
        0x1003, 0x1013, true)
      geometry("unaligned", "unaligned upper bound excludes its own byte", BigInt(0x1013) - bytes + 1,
        0x1003, 0x1013, false)
      geometry("unsigned_addresses", "address above sign bit with a low lower bound",
        signBit + 16, 0, signBit + 64, true)
      geometry("unsigned_addresses", "access across sign bit", signBit - 1,
        signBit - 32, signBit + 32, true)
      geometry("unsigned_addresses", "high address in high range", signBit + 0x1000,
        signBit + 0x1000, signBit + 0x1020, true)
      geometry("overflow", "highest exclusive bound permits preceding bytes", maxAddress - bytes,
        0, maxAddress, true)
      geometry("overflow", "maximum byte cannot be below any representable exclusive bound",
        maxAddress - bytes + 1, 0, maxAddress, false)
      geometry("overflow", "maximum start address", maxAddress, 0, maxAddress, false)
      if (size > 0) {
        geometry("overflow", "mathematical final byte overflows", maxAddress - bytes + 2,
          0, maxAddress, false)
        geometry("overflow", "wrapped endpoint cannot match a low range", maxAddress - bytes + 2,
          0, 32, false)
      }
    }

    // Exercise each high address bit without depending only on the sign bit.
    for (bit <- 13 until 64) {
      val low = BigInt(0x1003)
      val high = low + (BigInt(1) << bit)
      val base = Access(address = high, sizeLog2 = bit % 5, entries = bank(11 -> Entry(boundLo = low)))
      directed("address_high_bits", s"address bit $bit must not alias a low entry", base, false)
      directed("address_high_bits", s"address bit $bit is retained in its matching entry",
        base.copy(entries = bank(11 -> Entry(boundLo = high, boundHi = high + 32))), true)
      directed("address_high_bits", s"low address must not alias entry bit $bit",
        base.copy(address = low, entries = bank(11 -> Entry(boundLo = high, boundHi = high + 32))), false)
    }

    for (size <- 1 to 4; operation <- 0 to 2) {
      val start = BigInt(0x1008)
      val bytes = BigInt(1) << size
      val adjacent = Access(address = start, sizeLog2 = size, operation = operation, entries = bank(
        0 -> Entry(boundLo = start - 8, boundHi = start + bytes / 2),
        15 -> Entry(boundLo = start + bytes / 2, boundHi = start + bytes + 8)))
      directed("adjacent_splice", "adjacent entries jointly cover but neither fully covers", adjacent, false)
      directed("complete_overrides_splice", "complete entry alongside adjacent fragments",
        adjacent.copy(entries = adjacent.entries.updated(6, Entry(boundLo = start, boundHi = start + bytes))), true)
      if (size >= 2) {
        val overlap = adjacent.copy(entries = bank(
          0 -> Entry(boundLo = start - 8, boundHi = start + bytes - 1),
          15 -> Entry(boundLo = start + 1, boundHi = start + bytes + 8)))
        directed("overlapping_splice", "overlapping entries jointly cover but neither fully covers", overlap, false)
        directed("complete_overrides_splice", "complete entry alongside overlapping fragments",
          overlap.copy(entries = overlap.entries.updated(6, Entry(boundLo = start, boundHi = start + bytes))), true)
      }
    }
    val specifiedOverlap = Access(sizeLog2 = 4, entries = bank(
      0 -> Entry(boundLo = 0x1000, boundHi = 0x1012),
      15 -> Entry(boundLo = 0x1010, boundHi = 0x1020)))
    directed("overlapping_splice", "sixteen-byte access across the specified overlapping ranges",
      specifiedOverlap, false)

    for (size <- 0 to 4) {
      val split = Access(sizeLog2 = size, entries = bank(
        0 -> Entry(writeAllowed = false), 15 -> Entry(readAllowed = false)))
      directed("rw_splice", "same-range read-only and write-only entries permit read", split, true)
      directed("rw_splice", "same-range read-only and write-only entries permit write",
        split.copy(operation = 1), true)
      directed("rw_splice", "same-range permissions cannot be combined for read-write",
        split.copy(operation = 2), false)
      directed("complete_overrides_splice", "one complete read-write entry authorizes",
        split.copy(operation = 2, entries = split.entries.updated(8, Entry())), true)
      directed("rw_splice", "full-range read permission cannot combine with out-of-range read-write permission",
        split.copy(operation = 2, entries = split.entries.updated(9, Entry(boundLo = 0x1009))), false)
    }

    for (size <- 5 to 7; operation <- 0 until 4) {
      directed("illegal_encoding", "illegal length with all entries fully enabled",
        Access(sizeLog2 = size, operation = operation, entries = Vector.fill(16)(Entry())), false)
    }
    for (size <- 0 to 4) {
      directed("illegal_encoding", "reserved operation with all entries fully enabled",
        Access(sizeLog2 = size, operation = 3, entries = Vector.fill(16)(Entry())), false)
    }

    val orderRandom = new Random(seed)
    val orderInputs = Vector(
      (Access(entries = bank(3 -> Entry())), true),
      (Access(entries = bank(1 -> Entry(), 12 -> Entry())), true),
      (specifiedOverlap, false),
      (Access(operation = 2, entries = bank(2 -> Entry(writeAllowed = false),
        14 -> Entry(readAllowed = false))), false),
      (Access(), false)
    )
    for (((input, expected), index) <- orderInputs.zipWithIndex) {
      val permutations = Vector("original" -> input.entries, "reverse" -> input.entries.reverse) ++
        Vector(1, 7, 15).map { shift =>
          s"rotate-$shift" -> (input.entries.drop(shift) ++ input.entries.take(shift))
        } :+ ("seeded-shuffle" -> orderRandom.shuffle(input.entries))
      for ((name, entries) <- permutations) {
        directed("entry_order", s"input $index $name", input.copy(entries = entries), expected)
      }
    }

    directed("continuous_inputs", "A allows a one-byte read at the first entry",
      Access(entries = bank(0 -> Entry(writeAllowed = false))), true)
    directed("continuous_inputs", "B denies a sixteen-byte read-write permission splice",
      Access(address = 0x1011, sizeLog2 = 4, operation = 2,
        entries = bank(4 -> Entry(writeAllowed = false), 9 -> Entry(readAllowed = false))), false)
    directed("continuous_inputs", "C allows an eight-byte write at the last entry",
      Access(address = 0x1021, sizeLog2 = 3, operation = 1,
        entries = bank(15 -> Entry(readAllowed = false))), true)
    directed("continuous_inputs", "D immediately invalidates every entry",
      Access(address = 0x1021, sizeLog2 = 3, operation = 1), false)

    val random = new Random(seed)
    def randomEntry(): Entry = Entry(boundLo = BigInt(64, random), boundHi = BigInt(64, random),
      entryValid = random.nextBoolean(), readAllowed = random.nextBoolean(), writeAllowed = random.nextBoolean())
    val anchors = Vector(BigInt(0), BigInt(0x1003), signBit - 32, signBit + 3, maxAddress - 63)
    for (index <- 0 until randomSamples) {
      val uniform = Access(address = BigInt(64, random), sizeLog2 = random.nextInt(8),
        operation = random.nextInt(4), entries = Vector.fill(16)(randomEntry()))
      val position = random.nextInt(16)
      val other = (position + 1 + random.nextInt(15)) % 16
      val start = BigInt(64, random) % (maxAddress - 128)
      val size = 1 + random.nextInt(4)
      val bytes = BigInt(1) << size
      val adjacent = Access(address = start + 8, sizeLog2 = size, operation = random.nextInt(3), entries = bank(
        position -> Entry(boundLo = start, boundHi = start + 8 + bytes / 2),
        other -> Entry(boundLo = start + 8 + bytes / 2, boundHi = start + 64)))
      val complete = uniform.copy(address = start + random.nextInt(16), sizeLog2 = random.nextInt(5),
        operation = random.nextInt(3), entries = uniform.entries.updated(position, Entry(boundLo = start, boundHi = start + 64)))
      val split = Access(address = start + 8, sizeLog2 = random.nextInt(5), operation = 2, entries = bank(
        position -> Entry(boundLo = start, boundHi = start + 64, writeAllowed = false),
        other -> Entry(boundLo = start, boundHi = start + 64, readAllowed = false)))
      val (category, input) = index % 8 match {
        case 0 => "uniform" -> uniform
        case 1 =>
          val lo = anchors(random.nextInt(anchors.size))
          val hi = lo + 32
          val addresses = Vector((lo - 1).max(BigInt(0)), lo, lo + 1, hi - 16, hi - 8, hi - 1, hi, hi + 1)
          "boundary_neighborhood" -> uniform.copy(address = addresses(random.nextInt(addresses.size)),
            entries = bank(position -> randomEntry().copy(boundLo = lo, boundHi = hi)))
        case 2 => "complete_authorization" -> complete
        case 3 => "adjacent_splice" -> adjacent
        case 4 =>
          val overlapSize = 2 + random.nextInt(3)
          val overlapBytes = BigInt(1) << overlapSize
          "overlapping_splice" -> adjacent.copy(sizeLog2 = overlapSize, entries = bank(
            position -> Entry(boundLo = start, boundHi = start + 8 + overlapBytes - 1),
            other -> Entry(boundLo = start + 9, boundHi = start + 64)))
        case 5 => "rw_splice" -> split
        case 6 => "overflow_neighborhood" -> uniform.copy(address = maxAddress - random.nextInt(17),
          entries = bank(position -> Entry(boundLo = 0, boundHi = maxAddress)))
        case _ =>
          val base = if (random.nextBoolean()) complete else split
          "entry_permutation" -> base.copy(entries = random.shuffle(base.entries))
      }
      samples += Sample(category, s"random index=$index", input, None)
    }

    // Validate explicit examples independently before observing any RTL result.
    for ((sample, index) <- samples.zipWithIndex) {
      val input = sample.input
      require(input.entries.size == 16)
      require((input.address +: input.entries.flatMap(e => Seq(e.boundLo, e.boundHi)))
        .forall(a => a >= 0 && a <= maxAddress))
      require(input.sizeLog2 >= 0 && input.sizeLog2 < 8 && input.operation >= 0 && input.operation < 4)
      val hits = oracleHits(input)
      val diagnostic = s"${sample.category} ${sample.label} seed=$seed case=$index input=$input"
      sample.expected.foreach { expected =>
        withClue(s"$diagnostic explicit=$expected oracleHits=$hits: ") { assert(hits.nonEmpty == expected) }
      }
      sample.expectedSingleHit.foreach { position =>
        withClue(s"$diagnostic expectedSingleHit=$position oracleHits=$hits: ") { assert(hits == Vector(position)) }
      }
    }

    def countsJson(counts: Array[Array[Int]]): String = counts.indices.map { index =>
      s""""$index":{"allow":${counts(index)(1)},"deny":${counts(index)(0)}}"""
    }.mkString("{", ",", "}")
    class Coverage {
      var allow = 0
      var deny = 0
      val categories = mutable.Map.empty[String, Array[Int]]
      val sizes = Array.fill(8, 2)(0)
      val operations = Array.fill(4, 2)(0)
      val uniqueHitPositions = Array.fill(16, 2)(0)
      val hitCounts = Array.fill(17, 2)(0)
      def add(sample: Sample, hits: Vector[Int]): Unit = {
        val outcome = if (hits.nonEmpty) 1 else 0
        if (hits.nonEmpty) allow += 1 else deny += 1
        categories.getOrElseUpdate(sample.category, Array(0, 0))(outcome) += 1
        sizes(sample.input.sizeLog2)(outcome) += 1
        operations(sample.input.operation)(outcome) += 1
        hitCounts(hits.size)(outcome) += 1
        if (hits.size == 1) uniqueHitPositions(hits.head)(outcome) += 1
      }
      def json: String = {
        val categoryJson = categories.toSeq.sortBy(_._1).map { case (name, counts) =>
          s""""$name":{"allow":${counts(1)},"deny":${counts(0)}}"""
        }.mkString("{", ",", "}")
        s"""{"cases":${allow + deny},"allow":$allow,"deny":$deny,"categories":$categoryJson,"sizeLog2":${countsJson(sizes)},"operation":${countsJson(operations)},"uniqueHitPosition":${countsJson(uniqueHitPositions)},"authorizingEntryCount":${countsJson(hitCounts)}}"""
      }
    }

    test(new FDIBoundsCheckerHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      TargetDirAnnotation("rtl-simulation")
    )) { dut =>
      val directedCoverage = new Coverage
      val randomCoverage = new Coverage
      for ((sample, index) <- samples.zipWithIndex) {
        val input = sample.input
        dut.io.address.poke(input.address.U(64.W))
        dut.io.sizeLog2.poke(input.sizeLog2.U(3.W))
        dut.io.operation.poke(input.operation.U(2.W))
        for ((entry, position) <- input.entries.zipWithIndex) {
          dut.io.entries(position).boundLo.poke(entry.boundLo.U(64.W))
          dut.io.entries(position).boundHi.poke(entry.boundHi.U(64.W))
          dut.io.entries(position).entryValid.poke(entry.entryValid.B)
          dut.io.entries(position).readAllowed.poke(entry.readAllowed.B)
          dut.io.entries(position).writeAllowed.poke(entry.writeAllowed.B)
        }
        val hits = oracleHits(input)
        // No clock edge is advanced; observation must reflect this entire input.
        val actual = dut.io.allow.peek().litToBoolean
        val diagnostic = s"${sample.category} ${sample.label} seed=$seed case=$index input=$input"
        withClue(s"$diagnostic oracleHits=$hits expected=${hits.nonEmpty} actual=$actual: ") {
          dut.io.allow.expect(hits.nonEmpty.B)
        }
        (if (sample.expected.nonEmpty) directedCoverage else randomCoverage).add(sample, hits)
      }
      val requiredDirectedCategories = Set("all_invalid", "single_hit", "single_hit_permission_denied",
        "multiple_hits", "control_permissions", "boundaries", "unaligned", "unsigned_addresses", "overflow",
        "address_high_bits", "adjacent_splice", "overlapping_splice", "complete_overrides_splice", "rw_splice",
        "illegal_encoding", "entry_order", "continuous_inputs")
      assert(directedCoverage.categories.keySet == requiredDirectedCategories)
      assert(directedCoverage.uniqueHitPositions.forall(_(1) > 0))
      assert(directedCoverage.sizes.forall(_.sum > 0) && directedCoverage.operations.forall(_.sum > 0))
      assert(randomCoverage.allow + randomCoverage.deny == randomSamples)
      assert(randomCoverage.allow > 0 && randomCoverage.deny > 0)
      assert(randomCoverage.categories.size == 8 && randomCoverage.categories.values.forall(_.sum == 512))
      val allowed = directedCoverage.allow + randomCoverage.allow
      val denied = directedCoverage.deny + randomCoverage.deny
      val result = s"""{"status":"PASS","backend":"verilator","cases":${samples.size},"directedCases":${directedCoverage.allow + directedCoverage.deny},"randomCases":$randomSamples,"seed":$seed,"allow":$allowed,"deny":$denied,"directed":${directedCoverage.json},"random":${randomCoverage.json}}"""
      Files.write(runRoot.resolve("result.json"), (result + "\n").getBytes(StandardCharsets.UTF_8))
      println(s"FDIBoundsChecker RTL PASS: $result")
    }
  }
}
