// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import org.scalatest.flatspec.AnyFlatSpec

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.util.Random

// chiseltest requires Module; the production primitive has no clock or reset.
class FDIBoundCheckerHarness extends Module {
  val io = IO(new FDIBoundCheckerIO)
  private val checker = Module(new FDIBoundChecker)
  io <> checker.io
}

class FDIBoundCheckerTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDIBoundChecker"

  it should "authorize every byte using one entry and only the current inputs" in {
    val runRoot = Paths.get(sys.props.getOrElse(
      "p01.runRoot",
      throw new IllegalArgumentException("Set p01.runRoot to an absolute build output directory")
    ))
    require(runRoot.isAbsolute, "p01.runRoot must be absolute")
    require(Paths.get("").toRealPath() == runRoot.toRealPath(),
      "The test working directory must be p01.runRoot")

    val maxAddress = (BigInt(1) << 64) - 1
    val signBit = BigInt(1) << 63
    val seed = 0x503031L
    val randomSamples = 4096

    case class Access(
      address: BigInt = 0x1000,
      boundLo: BigInt = 0x1000,
      boundHi: BigInt = 0x1020,
      sizeLog2: Int = 0,
      operation: Int = 0,
      entryValid: Boolean = true,
      readAllowed: Boolean = true,
      writeAllowed: Boolean = true
    )

    // Enumerate mathematical byte addresses instead of reproducing the DUT's
    // fixed-width endpoint arithmetic. Permissions use an independent truth table.
    val byteCounts = Vector(1, 2, 4, 8, 16)
    val permissionTable = Map(
      (false, false) -> Set.empty[Int],
      (true, false) -> Set(0),
      (false, true) -> Set(1),
      (true, true) -> Set(0, 1, 2)
    )
    def oracle(input: Access): Boolean = {
      input.sizeLog2 <= 4 && input.entryValid &&
        permissionTable((input.readAllowed, input.writeAllowed)).contains(input.operation) &&
        (0 until byteCounts(input.sizeLog2)).forall { offset =>
          val byteAddress = input.address + BigInt(offset)
          byteAddress <= maxAddress && byteAddress >= input.boundLo && byteAddress < input.boundHi
        }
    }

    test(new FDIBoundCheckerHarness).withAnnotations(Seq(
      VerilatorBackendAnnotation,
      TargetDirAnnotation("rtl-simulation")
    )) { dut =>
      var groups = 0
      var cases = 0
      var directedCases = 0
      var randomizedCases = 0
      var allowed = 0
      var denied = 0
      var randomAllowed = 0
      var randomDenied = 0
      val sizeCounts = Array.fill(8, 2)(0)
      val operationCounts = Array.fill(4, 2)(0)

      def group(body: => Unit): Unit = {
        groups += 1
        body
      }

      def check(label: String, input: Access, expected: Option[Boolean]): Unit = {
        require(Seq(input.address, input.boundLo, input.boundHi).forall(a => a >= 0 && a <= maxAddress))
        require(input.sizeLog2 >= 0 && input.sizeLog2 < 8 && input.operation >= 0 && input.operation < 4)
        val prediction = oracle(input)
        val diagnostic = s"$label seed=$seed case=$cases input=$input"
        expected.foreach { value =>
          withClue(s"$diagnostic explicit=$value oracle=$prediction: ") {
            assert(prediction == value)
          }
        }
        dut.io.address.poke(input.address.U(64.W))
        dut.io.boundLo.poke(input.boundLo.U(64.W))
        dut.io.boundHi.poke(input.boundHi.U(64.W))
        dut.io.sizeLog2.poke(input.sizeLog2.U(3.W))
        dut.io.operation.poke(input.operation.U(2.W))
        dut.io.entryValid.poke(input.entryValid.B)
        dut.io.readAllowed.poke(input.readAllowed.B)
        dut.io.writeAllowed.poke(input.writeAllowed.B)
        // No clock edge is advanced: each observation must follow the current inputs.
        val actual = dut.io.allow.peek().litToBoolean
        withClue(s"$diagnostic expected=$prediction actual=$actual: ") {
          dut.io.allow.expect(prediction.B)
        }
        cases += 1
        if (expected.nonEmpty) directedCases += 1 else randomizedCases += 1
        if (prediction) allowed += 1 else denied += 1
        if (expected.isEmpty) {
          if (prediction) randomAllowed += 1 else randomDenied += 1
        }
        val outcome = if (prediction) 1 else 0
        sizeCounts(input.sizeLog2)(outcome) += 1
        operationCounts(input.operation)(outcome) += 1
      }

      def directed(label: String, input: Access, expected: Boolean): Unit =
        check(label, input, Some(expected))

      for (size <- 0 to 4) group {
        val bytes = BigInt(1) << size
        val base = Access(sizeLog2 = size)
        directed("lower bound included", base, true)
        directed("last byte below upper bound", base.copy(address = base.boundHi - bytes), true)
        directed("last byte equals upper bound", base.copy(address = base.boundHi - bytes + 1), false)
        directed("start equals upper bound", base.copy(address = base.boundHi), false)
        directed("start below lower bound", base.copy(address = base.boundLo - 1), false)
        directed("unaligned complete access", base.copy(address = base.boundLo + 1), true)
        directed("empty interval", base.copy(boundHi = base.boundLo), false)
        directed("reversed interval", base.copy(boundLo = base.boundHi, boundHi = base.boundLo), false)
        directed("address zero", base.copy(address = 0, boundLo = 0, boundHi = 32), true)
        directed("unsigned address above sign bit",
          base.copy(address = signBit + 16, boundLo = 0, boundHi = signBit + 64), true)
        directed("range crosses sign bit",
          base.copy(address = signBit - 1, boundLo = signBit - 32, boundHi = signBit + 32), true)
        directed("high address in high interval",
          base.copy(address = signBit + 0x1000, boundLo = signBit + 0x1000, boundHi = signBit + 0x1020), true)
        directed("high address does not alias low interval", base.copy(address = signBit + 0x1000), false)
        directed("low address does not alias high interval",
          base.copy(boundLo = signBit + 0x1000, boundHi = signBit + 0x1020), false)
        directed("lower bound is not rounded down",
          base.copy(address = 0x1002, boundLo = 0x1003, boundHi = 0x1023), false)
        directed("upper bound is not rounded down",
          base.copy(address = BigInt(0x1013) - bytes, boundLo = 0x1003, boundHi = 0x1013), true)
        directed("unaligned upper bound excludes its own byte",
          base.copy(address = BigInt(0x1013) - bytes + 1, boundLo = 0x1003, boundHi = 0x1013), false)
        directed("highest representable exclusive bound permits its preceding bytes",
          base.copy(address = maxAddress - bytes, boundLo = 0, boundHi = maxAddress), true)
        directed("last byte at maximum does not overflow but is outside every bound",
          base.copy(address = maxAddress - bytes + 1, boundLo = 0, boundHi = maxAddress), false)
        if (size > 0) {
          directed("last byte overflows full address width",
            base.copy(address = maxAddress - bytes + 2, boundLo = 0, boundHi = maxAddress), false)
          directed("wrapped endpoint must not match a low interval",
            base.copy(address = maxAddress - bytes + 2, boundLo = 0, boundHi = 32), false)
        }
      }

      // Each geometry exercises all 256 combinations of the encoded controls.
      val geometries = Seq(
        ("inside", Access(), 4),
        ("seven bytes remain", Access(address = 0x1019), 2),
        ("below lower bound", Access(address = 0x0fff), -1),
        ("empty interval", Access(boundHi = 0x1000), -1)
      )
      for ((label, geometry, largestPermittedSize) <- geometries) group {
        for (size <- 0 until 8; operation <- 0 until 4;
             valid <- Seq(false, true); permissions <- 0 until 4) {
          val read = (permissions & 1) != 0
          val write = (permissions & 2) != 0
          val permissionExpected = operation match {
            case 0 => read
            case 1 => write
            case 2 => read && write
            case _ => false
          }
          val expected = valid && permissionExpected && size <= largestPermittedSize
          directed(s"control matrix $label", geometry.copy(sizeLog2 = size, operation = operation,
            entryValid = valid, readAllowed = read, writeAllowed = write), expected)
        }
      }

      group {
        val boundary = Access(boundHi = 0x1010)
        directed("eight bytes ending at the boundary", boundary.copy(address = 0x1008, sizeLog2 = 3), true)
        directed("eight bytes crossing the boundary", boundary.copy(address = 0x1009, sizeLog2 = 3), false)
        directed("unaligned eight bytes inside", boundary.copy(address = 0x1001, sizeLog2 = 3), true)
        directed("sixteen bytes fill the interval", boundary.copy(sizeLog2 = 4), true)
      }

      group {
        directed("continuous A read allowed", Access(operation = 0, writeAllowed = false), true)
        directed("continuous B read-write needs both permissions",
          Access(address = 0x1010, sizeLog2 = 4, operation = 2, writeAllowed = false), false)
        directed("continuous C write allowed",
          Access(address = 0x1018, sizeLog2 = 3, operation = 1, readAllowed = false), true)
      }

      group {
        val random = new Random(seed)
        val anchors = Vector(BigInt(0), BigInt(0x1000), signBit - 32, signBit, maxAddress - 63)
        for (index <- 0 until randomSamples) {
          val uniform = Access(address = BigInt(64, random), boundLo = BigInt(64, random),
            boundHi = BigInt(64, random), sizeLog2 = random.nextInt(8), operation = random.nextInt(4),
            entryValid = random.nextBoolean(), readAllowed = random.nextBoolean(), writeAllowed = random.nextBoolean())
          val input = index % 4 match {
            case 0 => uniform
            case 1 =>
              val lo = anchors(random.nextInt(anchors.size))
              val hi = lo + 32
              val addresses = Vector((lo - 1).max(BigInt(0)), lo, lo + 1, hi - 16, hi - 8, hi - 1, hi, hi + 1)
              uniform.copy(address = addresses(random.nextInt(addresses.size)), boundLo = lo, boundHi = hi)
            case 2 =>
              val lo = BigInt(64, random) % (maxAddress - 64)
              uniform.copy(address = lo + random.nextInt(16), boundLo = lo, boundHi = lo + 32,
                sizeLog2 = random.nextInt(5), operation = random.nextInt(3),
                entryValid = true, readAllowed = true, writeAllowed = true)
            case _ =>
              uniform.copy(address = maxAddress - random.nextInt(17), boundLo = 0, boundHi = maxAddress,
                operation = random.nextInt(3), entryValid = true, readAllowed = true, writeAllowed = true)
          }
          check(s"random index=$index", input, None)
        }
      }

      assert(randomizedCases == randomSamples && randomAllowed > 0 && randomDenied > 0)
      def countsJson(counts: Array[Array[Int]]): String = counts.indices.map { index =>
        s""""$index":{"allow":${counts(index)(1)},"deny":${counts(index)(0)}}"""
      }.mkString("{", ",", "}")
      val result = s"""{"status":"PASS","backend":"verilator","groups":$groups,"cases":$cases,"directedCases":$directedCases,"randomCases":$randomizedCases,"seed":$seed,"allow":$allowed,"deny":$denied,"randomAllow":$randomAllowed,"randomDeny":$randomDenied,"sizeLog2":${countsJson(sizeCounts)},"operation":${countsJson(operationCounts)}}"""
      Files.write(runRoot.resolve("result.json"), (result + "\n").getBytes(StandardCharsets.UTF_8))
      println(s"FDIBoundChecker RTL PASS: $result")
    }
  }
}
