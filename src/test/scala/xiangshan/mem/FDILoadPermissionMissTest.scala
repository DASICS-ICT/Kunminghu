// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.mem

import chisel3._
import chisel3.simulator.PeekPokeAPI._
import chisel3.simulator.{ChiselSimulation, ChiselWorkspace}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import svsim.{CommonCompilationSettings, Workspace}
import svsim.verilator.Backend
import xiangshan._
import xiangshan.backend.fu.{FuType, UserTimerDeliveryParameters}

class FDILoadPermissionMissTest extends AnyFlatSpec {
  behavior of "FDI load permission at the production cache miss admission boundary"

  private val enabled = sys.env.getOrElse("L01_FDI_ENABLED", "true").toBoolean
  private val baseAddress = BigInt("80001000", 16)
  private val blockBytes = 64
  private case class Request(serial: Int, address: BigInt) {
    val lastByte: BigInt = address + 7
    val allowed: Boolean = !enabled || (baseAddress <= address && lastByte < baseAddress + 8)
    val block: BigInt = address & ~BigInt(blockBytes - 1)
  }

  private class Driver(val dut: FDILoadPermissionMissHarness) {
    val events = mutable.ArrayBuffer.empty[String]
    val totals = mutable.Map.empty[String, Int].withDefaultValue(0)
    val admitted = mutable.Map.empty[BigInt, Int]
    val acquired = mutable.Set.empty[BigInt]
    var cycles = 0
    private var current: Option[Request] = None
    private var expectedPrimary = false
    private var actualEntry: Option[Int] = None
    private val counts = mutable.Map.empty[String, Int].withDefaultValue(0)

    private def bool(value: Bool): Boolean = value.peek().litToBoolean
    private def uint(value: UInt): BigInt = value.peek().litValue
    private def zero(data: Data): Unit = data match {
      case value: Bool => value.poke(false.B)
      case value: UInt => value.poke(0.U)
      case value: SInt => value.poke(0.S)
      case value: Vec[_] => value.foreach(zero)
      case value: Record => value.elements.values.foreach(zero)
      case other => throw new IllegalArgumentException(s"Unsupported input ${other.getClass.getName}")
    }
    private def record(kind: String, detail: String = ""): Unit = {
      val request = current.getOrElse(throw new AssertionError(s"Unexpected $kind at cycle $cycles"))
      counts(kind) += 1
      totals(kind) += 1
      events += s"{\"cycle\":$cycles,\"event\":\"$kind\",\"serial\":${request.serial}," +
        s"\"address\":${request.address},\"expected_allowed\":${request.allowed}" +
        (if (detail.isEmpty) "}" else "," + detail + "}")
    }

    private def edge(): Unit = {
      // Outstanding cold misses retain one configuration throughout the trace.
      dut.io.config.foreach { config =>
        config.policy.uEnable.expect(true.B)
        config.entries(0).boundLo.expect(baseAddress.U)
        config.entries(0).boundHi.expect((baseAddress + 8).U)
        config.entries(0).entryValid.expect(true.B)
        config.entries(0).readAllowed.expect(true.B)
      }
      if (bool(dut.io.request.valid) && bool(dut.io.request.ready)) record("issue")
      if (bool(dut.io.cacheRequest.valid)) {
        val request = current.get
        dut.io.cacheRequest.bits.vaddr.expect(request.address.U)
        dut.io.cacheRequest.bits.debug_robIdx.expect(request.serial.U)
        record("cache-request")
      }
      if (bool(dut.io.permissionRequest.valid)) {
        val request = current.get
        dut.io.permissionRequest.bits.address.expect(request.address.U)
        dut.io.permissionRequest.bits.sizeLog2.expect(3.U)
        dut.io.permissionRequest.bits.operation.expect(0.U)
        dut.io.permissionRequest.bits.sourcePrivilege.expect(0.U)
        dut.io.permissionRequest.bits.sourceVirtual.expect(false.B)
        dut.io.permissionRequest.bits.notTrusted.expect(true.B)
        record("permission-request")
      }
      if (bool(dut.io.permissionResponse.valid)) {
        val request = current.get
        dut.io.permissionResponse.bits.request.address.expect(request.address.U)
        dut.io.permissionResponse.bits.outcome.expect((if (request.allowed) 0 else 1).U)
        dut.io.permissionResponse.bits.reason.expect((if (request.allowed) 0 else 2).U)
        record("permission-response")
      }
      if (bool(dut.io.rawMiss.valid)) {
        val request = current.get
        dut.io.rawMiss.bits.addr.expect(request.block.U)
        dut.io.rawMiss.bits.vaddr.expect(request.address.U)
        dut.io.rawMissRob.expect(request.serial.U)
        dut.io.rawMiss.bits.source.expect(0.U)
        dut.io.rawMiss.bits.cancel.expect((!request.allowed).B)
        dut.io.cacheKill.expect((!request.allowed).B)
        assert(bool(dut.io.rawMissReady), "This trace must offer a genuinely available MSHR path")
        if (admitted.contains(request.block)) {
          assert(bool(dut.io.secondaryReady(admitted(request.block))), "Existing entry must be merge eligible")
        } else {
          assert(dut.io.primaryReady.exists(bool), "An empty entry must be allocation eligible")
        }
        record("raw-miss", s"\"cancel\":${bool(dut.io.rawMiss.bits.cancel)}")
      }
      dut.io.primaryAccept.zipWithIndex.foreach { case (port, entry) =>
        if (bool(port)) {
          val request = current.get
          assert(request.allowed && expectedPrimary, "A denied or merging load allocated an MSHR")
          assert(actualEntry.isEmpty)
          actualEntry = Some(entry)
          record("primary-accept", s"\"entry\":$entry")
        }
      }
      dut.io.secondaryAccept.zipWithIndex.foreach { case (port, entry) =>
        if (bool(port)) {
          val request = current.get
          assert(request.allowed && !expectedPrimary, "A denied or new-line load merged into an MSHR")
          assert(admitted.get(request.block).contains(entry))
          actualEntry = Some(entry)
          record("secondary-accept", s"\"entry\":$entry")
        }
      }
      if (bool(dut.io.allocation) || bool(dut.io.merge)) {
        val request = current.get
        assert(request.allowed, "A denied request changed the actual MissQueue pipeline register")
        dut.io.updateAddress.expect(request.block.U)
        assert(actualEntry.contains(uint(dut.io.updateEntry).toInt))
        dut.io.allocation.expect(expectedPrimary.B)
        dut.io.merge.expect((!expectedPrimary).B)
        record(if (expectedPrimary) "entry-allocation" else "entry-merge",
          s"\"entry\":${uint(dut.io.updateEntry)}")
      }
      if (bool(dut.io.acquireFire)) {
        val request = current.get
        assert(request.allowed && expectedPrimary, "Denied or secondary access issued a new acquire")
        dut.io.acquire.bits.address.expect(request.block.U)
        dut.io.acquire.bits.opcode.expect(6.U)
        dut.io.acquire.bits.size.expect(6.U)
        assert(actualEntry.contains(uint(dut.io.acquire.bits.source).toInt))
        assert(acquired.add(request.block), "A line emitted a duplicate acquire")
        record("mem-acquire", s"\"source\":${uint(dut.io.acquire.bits.source)}")
      }
      if (bool(dut.io.completion.valid)) {
        val request = current.get
        assert(!request.allowed, "An unrefilled cold miss must not produce a data completion")
        dut.io.completion.bits.uop.robIdx.value.expect(request.serial.U)
        dut.io.completion.bits.uop.rfWen.expect(false.B)
        dut.io.completion.bits.uop.fpWen.expect(false.B)
        dut.io.completion.bits.data.expect(0.U)
        dut.io.completion.bits.uop.exceptionVec(24).expect(true.B)
        dut.io.completion.bits.uop.fdiException.foreach { fault =>
          fault.tval.expect(request.address.U)
          fault.reason.expect(2.U)
        }
        record("denied-completion")
      }
      if (bool(dut.io.queueUpdate.valid)) {
        dut.io.queueUpdate.bits.uop.robIdx.value.expect(current.get.serial.U)
        record("queue-update")
      }
      dut.clock.step()
      cycles += 1
    }

    def reset(): Unit = {
      dut.io.request.valid.poke(false.B)
      zero(dut.io.request.bits)
      dut.io.config.foreach { config =>
        zero(config)
        config.sourcePrivilege.poke(0.U)
        config.sourceVirtual.poke(false.B)
        config.policy.uEnable.poke(true.B)
        config.entries(0).boundLo.poke(baseAddress.U)
        config.entries(0).boundHi.poke((baseAddress + 8).U)
        config.entries(0).entryValid.poke(true.B)
        config.entries(0).readAllowed.poke(true.B)
      }
      dut.io.acquireReady.poke(true.B)
      dut.reset.poke(true.B)
      dut.clock.step(6)
      dut.reset.poke(false.B)
      for (_ <- 0 until 4) edge()
      dut.io.entryValid.foreach(_.expect(false.B))
    }

    def run(request: Request): Unit = {
      current = Some(request)
      expectedPrimary = !admitted.contains(request.block)
      actualEntry = None
      counts.clear()
      events += s"{\"cycle\":$cycles,\"event\":\"input\",\"serial\":${request.serial}," +
        s"\"address\":${request.address},\"read_allowed\":true," +
        s"\"bound_lo\":$baseAddress,\"bound_hi\":${baseAddress + 8},\"size_bytes\":8," +
        s"\"expected_allowed\":${request.allowed},\"existing_line\":${!expectedPrimary}}"
      zero(dut.io.request.bits)
      dut.io.request.bits.src(0).poke(request.address.U)
      dut.io.request.bits.uop.fuType.poke(FuType.ldu.U)
      dut.io.request.bits.uop.fuOpType.poke(3.U)
      dut.io.request.bits.uop.rfWen.poke(true.B)
      dut.io.request.bits.uop.pdest.poke((request.serial + 1).U)
      dut.io.request.bits.uop.robIdx.value.poke(request.serial.U)
      dut.io.request.bits.uop.lqIdx.value.poke(request.serial.U)
      dut.io.request.bits.uop.fdiNotTrusted.foreach(_.poke(true.B))
      dut.io.request.bits.isFirstIssue.poke(true.B)
      dut.io.request.valid.poke(true.B)
      var wait = 0
      while (!bool(dut.io.request.ready) && wait < 16) { edge(); wait += 1 }
      assert(bool(dut.io.request.ready), "LoadUnit failed to accept the bounded request")
      edge()
      dut.io.request.valid.poke(false.B)
      for (_ <- 0 until 16) edge()

      assert(counts("issue") == 1 && counts("cache-request") == 1 && counts("raw-miss") == 1,
        s"The original request must reach the real cache miss boundary once: $counts")
      assert(counts("permission-request") == (if (enabled) 1 else 0))
      assert(counts("permission-response") == (if (enabled) 1 else 0))
      assert(counts("primary-accept") == (if (request.allowed && expectedPrimary) 1 else 0))
      assert(counts("secondary-accept") == (if (request.allowed && !expectedPrimary) 1 else 0))
      assert(counts("entry-allocation") == counts("primary-accept"))
      assert(counts("entry-merge") == counts("secondary-accept"))
      assert(counts("mem-acquire") == counts("primary-accept"))
      assert(counts("denied-completion") == (if (request.allowed) 0 else 1))
      assert(counts("queue-update") == 1)
      if (request.allowed && expectedPrimary) admitted(request.block) = actualEntry.get
      val actual = dut.io.entryValid.indices.filter(index => bool(dut.io.entryValid(index)))
        .map(index => uint(dut.io.entryAddress(index)) -> index).toMap
      assert(actual == admitted.toMap, s"Real entry state differs from independently admitted lines: $actual / $admitted")
      assert(acquired == admitted.keySet, "Every allocated line must issue one real acquire")
      dut.io.acquire.valid.expect(false.B)
    }

    def minimal(): Unit = {
      run(Request(1, baseAddress + 8))
      run(Request(2, baseAddress))
      run(Request(3, baseAddress + 16))
      run(Request(4, baseAddress))
      run(Request(5, baseAddress + blockBytes))
      if (enabled) {
        assert(totals("primary-accept") == 1 && totals("secondary-accept") == 1)
        assert(totals("mem-acquire") == 1 && totals("denied-completion") == 3)
      } else {
        assert(totals("primary-accept") == 2 && totals("secondary-accept") == 3)
        assert(totals("mem-acquire") == 2 && totals("denied-completion") == 0)
      }
    }
  }

  it should "reject both allocation and merge while an allowed cold load issues a real acquire" in {
    val root = Paths.get(sys.props("l01.runRoot")).toRealPath()
    require(Paths.get("").toRealPath() == root, "Run from the dedicated L01 evidence directory")
    val path = root.resolve(if (enabled) "miss-enabled" else "miss-disabled")
    require(!Files.exists(path), s"Evidence directory already exists: $path")
    Files.createDirectory(path)
    implicit val p: Parameters = UserTimerDeliveryParameters(enabled)
    val options = p(DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    val workspace = new Workspace(path.resolve("compiled").toString)
    workspace.reset()
    val module = workspace.elaborateGeneratedModule(() => new FDILoadPermissionMissHarness)
    workspace.generateAdditionalSources()
    val common = CommonCompilationSettings(availableParallelism =
      CommonCompilationSettings.AvailableParallelism.UpTo(Runtime.getRuntime.availableProcessors()))
    val settings = Backend.CompilationSettings(disabledWarnings = Seq("WIDTH", "STMTDLY"),
      disableFatalExitOnWarnings = true, enableAllAssertions = true)
    val simulation = workspace.compile(Backend.initializeFromProcessEnvironment())("verilator", common, settings, None, false)
    simulation.runElaboratedModule(module) { elaborated =>
      val driver = new Driver(elaborated.wrapped)
      try {
        driver.reset()
        driver.minimal()
        println(s"FDI load permission miss PASS enabled=$enabled cycles=${driver.cycles} " +
          s"counts=${driver.totals.toSeq.sortBy(_._1).mkString(",")} " +
          "fixture=production-LoadUnit-LoadPipe-MissQueue tlb=identity-service arrays=cold " +
          "grant=delayed scope=admission-and-acquire")
      } finally {
        Files.write(path.resolve("miss-events.jsonl"), driver.events.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
      }
    }
  }
}
