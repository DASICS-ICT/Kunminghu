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

class FDIPermissionPolicyHarness extends Module {
  val io = IO(new FDIPermissionPolicyIO)
  private val policy = Module(new FDIPermissionPolicy)
  io <> policy.io
}

// Translation context is deliberately separate from the originating privilege.
// This harness tests that distinction, not instruction decode or an LSU path.
class FDIPermissionPolicyBoundsHarness extends Module {
  val io = IO(new Bundle {
    val address = Input(UInt(64.W))
    val sizeLog2 = Input(UInt(3.W))
    val operation = Input(UInt(2.W))
    val entries = Input(Vec(16, new FDIBoundEntry))
    val sourcePrivilege = Input(UInt(2.W))
    val sourceVirtual = Input(Bool())
    val translationPrivilege = Input(UInt(2.W))
    val translationVirtual = Input(Bool())
    val notTrusted = Input(Bool())
    val config = Input(new FDIPolicyConfig)
    val rawAllow = Output(Bool())
    val outcome = Output(UInt(2.W))
    val reason = Output(UInt(3.W))
  })
  private val bounds = Module(new FDIBoundsChecker)
  bounds.io.address := io.address
  bounds.io.sizeLog2 := io.sizeLog2
  bounds.io.operation := io.operation
  bounds.io.entries := io.entries
  private val policy = Module(new FDIPermissionPolicy)
  policy.io.rawAllow := bounds.io.allow
  policy.io.sourcePrivilege := io.sourcePrivilege
  policy.io.sourceVirtual := io.sourceVirtual
  policy.io.notTrusted := io.notTrusted
  // The harness uses only legal memory operations, whose three values correspond.
  policy.io.checkKind := io.operation
  policy.io.config := io.config
  io.rawAllow := bounds.io.allow
  io.outcome := policy.io.outcome
  io.reason := policy.io.reason
}

class FDIPermissionPolicyTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDIPermissionPolicy"

  it should "apply the complete source policy and classify local errors without translation aliases" in {
    val runRoot = Paths.get(sys.props.getOrElse("p04.runRoot",
      throw new IllegalArgumentException("Set p04.runRoot to an absolute build output directory")))
    require(runRoot.isAbsolute, "p04.runRoot must be absolute")
    require(Paths.get("").toRealPath() == runRoot.toRealPath(),
      "The test working directory must be p04.runRoot")

    case class Input(source: Int = 0, virtual: Boolean = false, notTrusted: Boolean = true,
      kind: Int = 0, raw: Boolean = false, config: Int = 3)
    case class Result(outcome: Int, reason: Int)
    val allow = Result(0, 0)
    val illegalGuest = Result(2, 0)
    val invalid = Result(3, 0)

    // Rows list disabled/trusted, disabled/untrusted, enabled/trusted,
    // enabled/untrusted. Literal values are independent of DUT constants.
    val sourceTable = Map(
      (0, false) -> Vector("bypass", "bypass", "bypass", "check"),
      (1, false) -> Vector("bypass", "bypass", "bypass", "check"),
      (2, false) -> Vector("invalid", "invalid", "invalid", "invalid"),
      (3, false) -> Vector("bypass", "bypass", "bypass", "bypass"),
      (0, true) -> Vector("bypass", "bypass", "guest", "guest"),
      (1, true) -> Vector("bypass", "bypass", "guest", "guest"),
      (2, true) -> Vector("invalid", "invalid", "invalid", "invalid"),
      (3, true) -> Vector("invalid", "invalid", "invalid", "invalid")
    )
    // Each class selects a close bit, a denial reason, and whether raw permission applies.
    val kindTable = Map(0 -> (0, 2, true), 1 -> (1, 3, true), 2 -> (1, 3, true),
      3 -> (2, 4, true), 4 -> (3, 1, false))
    def bit(value: Int, index: Int): Boolean = (value & (1 << index)) != 0
    def oracle(input: Input): Result = kindTable.get(input.kind) match {
      case None => invalid
      case Some((closeIndex, reason, usesRaw)) =>
        val mode = if (input.source == 1) 1 else 0
        val row = (if (bit(input.config, mode)) 2 else 0) + (if (input.notTrusted) 1 else 0)
        sourceTable((input.source, input.virtual))(row) match {
          case "invalid" => invalid
          case "guest" => illegalGuest
          case "bypass" => allow
          case "check" =>
            val closed = bit(input.config, 2 + mode * 4 + closeIndex)
            if (closed || (usesRaw && input.raw)) allow else Result(1, reason)
        }
    }

    def pokeConfig(port: FDIPolicyConfig, bits: Int): Unit = {
      val fields = Seq(port.uEnable, port.sEnable, port.uCloseRead, port.uCloseWrite,
        port.uCloseJump, port.uCloseEcall, port.sCloseRead, port.sCloseWrite,
        port.sCloseJump, port.sCloseEcall)
      fields.zipWithIndex.foreach { case (field, index) => field.poke(bit(bits, index).B) }
    }

    class Coverage {
      var cases = 0
      val outcomes = Array.fill(4)(0)
      val reasons = Array.fill(5)(0)
      val kinds = Array.fill(8)(0)
      val modes = Array.fill(8)(0)
      val categories = mutable.Map.empty[String, Int]
      def add(input: Input, result: Result, category: String): Unit = {
        cases += 1
        outcomes(result.outcome) += 1
        reasons(result.reason) += 1
        kinds(input.kind) += 1
        modes(input.source * 2 + (if (input.virtual) 1 else 0)) += 1
        categories(category) = categories.getOrElse(category, 0) + 1
      }
      def json: String = {
        val categoriesJson = categories.toSeq.sortBy(_._1).map { case (name, count) =>
          s""""$name":$count"""
        }.mkString("{", ",", "}")
        s"""{"cases":$cases,"outcomes":${outcomes.mkString("[", ",", "]")},"reasons":${reasons.mkString("[", ",", "]")},"kinds":${kinds.mkString("[", ",", "]")},"modes":${modes.mkString("[", ",", "]")},"categories":$categoriesJson}"""
      }
    }
    val exhaustive = new Coverage
    val directed = new Coverage
    val integration = new Coverage
    val seed = 0x503034L
    val explicit = Vector(
      ("invalid_source", Input(source = 2, notTrusted = false, raw = true, config = 1023), invalid),
      ("invalid_source", Input(source = 3, virtual = true, config = 0), invalid),
      ("invalid_kind", Input(source = 3, kind = 7, raw = true, config = 0), invalid),
      ("machine_bypass", Input(source = 3, kind = 4), allow),
      ("guest_disabled", Input(virtual = true, config = 0), allow),
      ("guest_priority", Input(virtual = true, notTrusted = false, raw = true, config = 1023), illegalGuest),
      ("guest_priority", Input(source = 1, virtual = true, notTrusted = false, config = 1023), illegalGuest),
      ("trusted_bypass", Input(notTrusted = false, kind = 4), allow),
      ("rw_close_read", Input(kind = 2, config = 7), Result(1, 3)),
      ("rw_close_write", Input(kind = 2, config = 11), allow),
      ("ecall_raw_ignored", Input(kind = 4, raw = true), Result(1, 1)),
      ("ecall_close", Input(kind = 4, config = 35), allow),
      ("jump_denied", Input(kind = 3), Result(1, 4)),
      ("jump_raw", Input(kind = 3, raw = true), allow),
      ("other_mode_close", Input(config = 1023 & ~60), Result(1, 2)),
      ("other_mode_enable", Input(config = 2), allow),
      ("supervisor_close", Input(source = 1, config = 67), allow),
      ("supervisor_independent", Input(source = 1, config = 7), Result(1, 2))
    )
    explicit.foreach { case (category, input, expected) =>
      withClue(s"$category input=$input: ") { assert(oracle(input) == expected) }
    }

    test(new FDIPermissionPolicyHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("rtl-policy")
    )) { dut =>
      def check(input: Input, coverage: Coverage, category: String): Unit = {
        dut.io.sourcePrivilege.poke(input.source.U(2.W))
        dut.io.sourceVirtual.poke(input.virtual.B)
        dut.io.notTrusted.poke(input.notTrusted.B)
        dut.io.checkKind.poke(input.kind.U(3.W))
        dut.io.rawAllow.poke(input.raw.B)
        val expected = oracle(input)
        withClue(s"$category input=$input expected=$expected: ") {
          dut.io.outcome.expect(expected.outcome.U(2.W))
          dut.io.reason.expect(expected.reason.U(3.W))
        }
        coverage.add(input, expected, category)
      }
      for ((category, input, _) <- explicit) {
        pokeConfig(dut.io.config, input.config)
        check(input, directed, category)
      }
      // The seed changes adjacency, not the complete set of tested control values.
      for (config <- new Random(seed).shuffle((0 until 1024).toVector)) {
        pokeConfig(dut.io.config, config)
        for (source <- 0 until 4; virtual <- Seq(false, true); notTrusted <- Seq(false, true);
          kind <- 0 until 8; raw <- Seq(false, true)) {
          check(Input(source, virtual, notTrusted, kind, raw, config), exhaustive, "complete_control_space")
        }
      }
    }

    case class Entry(lo: BigInt = 0x1000, hi: BigInt = 0x1040,
      valid: Boolean = true, read: Boolean = true, write: Boolean = true)
    def bank(entries: (Int, Entry)*): Vector[Entry] = entries.foldLeft(Vector.fill(16)(Entry(valid = false))) {
      case (result, (index, entry)) => result.updated(index, entry)
    }
    case class Access(category: String, address: BigInt, size: Int, operation: Int,
      entries: Vector[Entry], expectedRaw: Boolean)
    val high = (BigInt(1) << 63) + 0x1000
    val accesses = (for (operation <- 0 until 3) yield Vector(
      Access("complete_entry", 0x1008, 4, operation, bank(15 -> Entry()), true),
      Access("missing_entry", 0x1008, 0, operation, bank(), false),
      Access("rw_no_splice", 0x1008, 0, operation,
        bank(0 -> Entry(write = false), 15 -> Entry(read = false)), operation != 2),
      Access("range_no_splice", 0x1008, 4, operation,
        bank(0 -> Entry(hi = 0x1010), 15 -> Entry(lo = 0x1010)), false),
      Access("full_address", high + 8, 3, operation, bank(7 -> Entry(lo = high, hi = high + 64)), true),
      Access("address_no_alias", high + 8, 3, operation, bank(7 -> Entry()), false),
      Access("exclusive_end", 0x1040, 0, operation, bank(8 -> Entry()), false)
    )).flatten
    def boundsOracle(access: Access): Boolean = access.entries.exists { entry =>
      val permissions = (if (entry.read) Set(0) else Set.empty[Int]) ++
        (if (entry.write) Set(1) else Set.empty[Int])
      val required = Vector(Set(0), Set(1), Set(0, 1))(access.operation)
      entry.valid && required.subsetOf(permissions) &&
        (0 until (1 << access.size)).forall { offset =>
          val address = access.address + offset
          address < (BigInt(1) << 64) && address >= entry.lo && address < entry.hi
        }
    }
    accesses.foreach { access => assert(boundsOracle(access) == access.expectedRaw, access.toString) }

    test(new FDIPermissionPolicyBoundsHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation, TargetDirAnnotation("rtl-bounds-policy")
    )) { dut =>
      for (access <- accesses; source <- Seq(0, 1);
        translation <- Seq((0, false), (1, false), (0, true), (1, true));
        config <- Seq(3, 3 | (1 << (2 + source * 4)), 3 | (1 << (3 + source * 4)))) {
        dut.io.address.poke(access.address.U(64.W))
        dut.io.sizeLog2.poke(access.size.U(3.W))
        dut.io.operation.poke(access.operation.U(2.W))
        for ((entry, index) <- access.entries.zipWithIndex) {
          dut.io.entries(index).boundLo.poke(entry.lo.U(64.W))
          dut.io.entries(index).boundHi.poke(entry.hi.U(64.W))
          dut.io.entries(index).entryValid.poke(entry.valid.B)
          dut.io.entries(index).readAllowed.poke(entry.read.B)
          dut.io.entries(index).writeAllowed.poke(entry.write.B)
        }
        dut.io.sourcePrivilege.poke(source.U(2.W))
        dut.io.sourceVirtual.poke(false.B)
        dut.io.translationPrivilege.poke(translation._1.U(2.W))
        dut.io.translationVirtual.poke(translation._2.B)
        dut.io.notTrusted.poke(true.B)
        pokeConfig(dut.io.config, config)
        val input = Input(source = source, kind = access.operation, raw = boundsOracle(access), config = config)
        val expected = oracle(input)
        withClue(s"$access source=$source translation=$translation config=$config expected=$expected: ") {
          dut.io.rawAllow.expect(access.expectedRaw.B)
          dut.io.outcome.expect(expected.outcome.U(2.W))
          dut.io.reason.expect(expected.reason.U(3.W))
        }
        integration.add(input, expected, access.category)
      }
    }
    assert(exhaustive.cases == 262144)
    assert(exhaustive.outcomes.forall(_ > 0) && exhaustive.reasons.forall(_ > 0))
    assert(exhaustive.kinds.forall(_ == 32768) && exhaustive.modes.forall(_ == 32768))
    assert(integration.cases == 504 && integration.categories.size == 7)
    assert(integration.outcomes(0) > 0 && integration.outcomes(1) > 0)
    val total = directed.cases + exhaustive.cases + integration.cases
    val result = s"""{"status":"PASS","backend":"verilator","cases":$total,"seed":$seed,"seedPurpose":"exhaustive_config_order","directed":${directed.json},"exhaustive":${exhaustive.json},"integration":${integration.json}}"""
    Files.write(runRoot.resolve("result.json"), (result + "\n").getBytes(StandardCharsets.UTF_8))
    println(s"FDIPermissionPolicy RTL PASS: $result")
  }
}
