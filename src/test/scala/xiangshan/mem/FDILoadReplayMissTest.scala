// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.mem

import chisel3._
import chisel3.simulator.PeekPokeAPI._
import chisel3.simulator.{ChiselSimulation, ChiselWorkspace}
import coupledL2.IsKeywordKey
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import svsim.{CommonCompilationSettings, Workspace}
import svsim.verilator.Backend
import xiangshan._
import xiangshan.backend.fu.{FuType, UserTimerDeliveryParameters}

class FDILoadReplayMissTest extends AnyFlatSpec {
  behavior of "FDI load replay through real miss grants"

  private val enabled = sys.env.getOrElse("L01_FDI_ENABLED", "true").toBoolean
  private val base = BigInt("80001000", 16)
  private case class Request(serial: Int, address: BigInt, accessFault: Boolean = false) {
    val block: BigInt = address & ~BigInt(63)
    val destination: Int = serial + 32
  }
  private def word(address: BigInt): BigInt = BigInt("1234567800000000", 16) ^ (address >> 3)

  private class Driver(val dut: FDILoadReplayMissHarness) {
    val events = mutable.ArrayBuffer.empty[String]
    val totals = mutable.Map.empty[String, Int].withDefaultValue(0)
    var cycles = 0
    private var scenario = "reset"
    private var boundBytes = 512
    private val requests = mutable.Map.empty[Int, Request]
    private val terminal = mutable.Map.empty[Int, Int].withDefaultValue(0)
    private val attempts = mutable.Map.empty[Int, Int].withDefaultValue(0)
    private val queued = mutable.Map.empty[Int, Int].withDefaultValue(0)
    private val canceled = mutable.Set.empty[Int]
    private val acquired = mutable.Map.empty[BigInt, (Int, Boolean)]
    private val grants = mutable.Map.empty[Int, Int].withDefaultValue(0)
    private val refills = mutable.Map.empty[Int, Int].withDefaultValue(0)
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
    private def allowed(request: Request): Boolean = !enabled ||
      (base <= request.address && request.address + 8 <= base + boundBytes)
    private def record(kind: String, fields: String = ""): Unit = {
      counts(kind) += 1
      totals(kind) += 1
      events += s"{\"cycle\":$cycles,\"scenario\":\"$scenario\",\"event\":\"$kind\"" +
        (if (fields.isEmpty) "}" else "," + fields + "}")
    }
    private def lookup(serial: Int): Request = requests.getOrElse(serial,
      throw new AssertionError(s"Unknown ROB $serial at cycle $cycles in $scenario"))

    private def edge(): Unit = {
      if (bool(dut.io.request.valid) && bool(dut.io.request.ready)) {
        val serial = uint(dut.io.request.bits.uop.robIdx.value).toInt
        lookup(serial)
        record("issue", s"\"rob\":$serial")
      }
      if (bool(dut.io.allocation) || bool(dut.io.merge)) {
        record(if (bool(dut.io.allocation)) "allocation" else "merge",
          s"\"mshrid\":${uint(dut.io.updateEntry)},\"block\":${uint(dut.io.updateAddress)}")
      }
      if (bool(dut.io.acquire.valid)) {
        val block = uint(dut.io.acquire.bits.address)
        assert(requests.values.exists(r => allowed(r) && r.block == block), "Denied line emitted an acquire")
        assert(!acquired.contains(block), s"Duplicate acquire for $block")
        val source = uint(dut.io.acquire.bits.source).toInt
        val keyword = dut.io.acquire.bits.echo.lift(IsKeywordKey).exists(bool)
        acquired(block) = (source, keyword)
        record("acquire", s"\"mshrid\":$source,\"block\":$block,\"keyword\":$keyword")
      }
      if (bool(dut.io.grant.valid) && bool(dut.io.grant.ready)) {
        val source = uint(dut.io.grant.bits.source).toInt
        assert(acquired.values.exists(_._1 == source), "Grant without an accepted acquire")
        grants(source) += 1
        assert(grants(source) <= 2, "More than two beats for one acquire")
        record("grant", s"\"mshrid\":$source,\"beat\":${grants(source) - 1}," +
          s"\"corrupt\":${bool(dut.io.grant.bits.corrupt)},\"data\":${uint(dut.io.grant.bits.data)}")
      }
      if (bool(dut.io.refill.valid)) {
        val source = uint(dut.io.refill.bits.id).toInt
        refills(source) += 1
        assert(refills(source) <= grants(source), "Refill did not follow a real grant")
        record("refill", s"\"mshrid\":$source,\"address\":${uint(dut.io.refill.bits.addr)}," +
          s"\"error\":${bool(dut.io.refill.bits.error)},\"last\":${bool(dut.io.refill.bits.refill_done)}")
      }
      if (bool(dut.io.grantAck)) record("grant-ack")
      if (bool(dut.io.queueUpdate.valid)) {
        val serial = uint(dut.io.queueUpdate.bits.uop.robIdx.value).toInt
        val request = lookup(serial)
        if (enabled) dut.io.queueUpdate.bits.fullva.expect(request.address.U)
        dut.io.queueUpdate.bits.vaddr.expect((request.address & ((BigInt(1) << dut.io.queueUpdate.bits.vaddr.getWidth) - 1)).U)
        queued(serial) += 1
        record("queue-update", s"\"rob\":$serial,\"expected_va\":${request.address}," +
          s"\"fullva\":${if (enabled) uint(dut.io.queueUpdate.bits.fullva).toString else "null"}," +
          s"\"mshrid\":${uint(dut.io.queueUpdate.bits.rep_info.mshr_id)}," +
          s"\"cause\":${dut.io.queueUpdate.bits.rep_info.cause.map(bool).mkString("\"", ",", "\"")}")
      }
      if (bool(dut.io.replayRequest.valid)) {
        val serial = uint(dut.io.replayRequest.bits.uop.robIdx.value).toInt
        val request = lookup(serial)
        assert(!canceled(serial), "Canceled load was replayed")
        assert(allowed(request), "Denied load allocated a replay")
        // Full-width replay storage is specific to the compiled FDI path.
        if (enabled) dut.io.replayRequest.bits.fullva.expect(request.address.U)
        dut.io.replayRequest.bits.vaddr.expect((request.address & ((BigInt(1) << dut.io.replayRequest.bits.vaddr.getWidth) - 1)).U)
        dut.io.replayRequest.bits.uop.pdest.expect(request.destination.U)
        dut.io.replayRequest.bits.mshrid.expect(acquired(request.block)._1.U)
        attempts(serial) += 1
        record("replay", s"\"rob\":$serial,\"expected_va\":${request.address}," +
          s"\"fullva\":${if (enabled) uint(dut.io.replayRequest.bits.fullva).toString else "null"}," +
          s"\"mshrid\":${uint(dut.io.replayRequest.bits.mshrid)}")
      }
      if (bool(dut.io.permissionRequest.valid)) {
        val address = uint(dut.io.permissionRequest.bits.address)
        assert(requests.values.exists(_.address == address), "Permission checked another transaction")
        record("permission-request", s"\"fullva\":$address")
      }
      if (bool(dut.io.permissionResponse.valid)) {
        val address = uint(dut.io.permissionResponse.bits.request.address)
        val request = requests.values.find(_.address == address).get
        dut.io.permissionResponse.bits.outcome.expect((if (allowed(request)) 0 else 1).U)
        record("permission-response", s"\"fullva\":$address")
      }
      if (bool(dut.io.forwardRequest.valid)) {
        val address = uint(dut.io.forwardRequest.bits.paddr)
        val request = requests.values.find(_.address == address).get
        dut.io.forwardRequest.bits.mshrid.expect(acquired(request.block)._1.U)
        record("forward-request", s"\"mshrid\":${uint(dut.io.forwardRequest.bits.mshrid)},\"paddr\":$address")
      }
      if (bool(dut.io.s2Forward.valid) && bool(dut.io.s2Forward.resultValid)) {
        val serial = uint(dut.io.s2Forward.rob).toInt
        val request = lookup(serial)
        if (enabled) dut.io.s2Forward.fullva.expect(request.address.U)
        if (bool(dut.io.s2Forward.dchannel)) record("forward-d", s"\"rob\":$serial")
        if (bool(dut.io.s2Forward.mshr)) record("forward-mshr", s"\"rob\":$serial")
      }
      if (bool(dut.io.completion.valid)) {
        val serial = uint(dut.io.completion.bits.uop.robIdx.value).toInt
        val request = lookup(serial)
        assert(!canceled(serial), "Canceled load produced a terminal completion")
        terminal(serial) += 1
        assert(terminal(serial) == 1, "Load completed more than once")
        dut.io.completion.bits.uop.pdest.expect(request.destination.U)
        val fdiFault = !allowed(request)
        val exceptions = dut.io.completion.bits.uop.exceptionVec.indices
          .filter(i => bool(dut.io.completion.bits.uop.exceptionVec(i))).toSet
        val expected = if (fdiFault) Set(24) else if (request.accessFault) Set(5) else Set.empty[Int]
        record("completion", s"\"rob\":$serial,\"data\":${uint(dut.io.completion.bits.data)}," +
          s"\"exceptions\":[${exceptions.toSeq.sorted.mkString(",")}]")
        assert(exceptions == expected, s"$scenario ROB $serial exception $exceptions != $expected")
        if (fdiFault) {
          dut.io.completion.bits.uop.rfWen.expect(false.B)
          dut.io.completion.bits.uop.fpWen.expect(false.B)
          dut.io.completion.bits.data.expect(0.U)
          dut.io.completion.bits.uop.fdiException.foreach { fault =>
            fault.tval.expect(request.address.U)
            fault.reason.expect(2.U)
          }
        } else if (!request.accessFault) {
          dut.io.completion.bits.data.expect(word(request.address).U)
          dut.io.completion.bits.uop.rfWen.expect(true.B)
        }
      }
      dut.clock.step()
      cycles += 1
    }
    private def waitFor(condition: => Boolean, limit: Int, why: String): Unit = {
      var elapsed = 0
      while (!condition && elapsed < limit) { edge(); elapsed += 1 }
      assert(condition, s"$scenario timeout waiting for $why after $elapsed cycles")
    }
    private def idle(count: Int): Unit = (0 until count).foreach(_ => edge())

    private def reset(name: String, bytes: Int = 512): Unit = {
      scenario = name
      boundBytes = bytes
      requests.clear(); terminal.clear(); attempts.clear(); queued.clear(); canceled.clear()
      acquired.clear(); grants.clear(); refills.clear(); counts.clear()
      dut.io.request.valid.poke(false.B)
      zero(dut.io.request.bits)
      zero(dut.io.redirect)
      dut.io.acquireReady.poke(true.B)
      dut.io.grant.valid.poke(false.B)
      zero(dut.io.grant.bits)
      zero(dut.io.hint)
      dut.io.finishEnable.poke(false.B)
      dut.io.config.foreach { config =>
        zero(config)
        config.policy.uEnable.poke(true.B)
        config.entries(0).boundLo.poke(base.U)
        config.entries(0).boundHi.poke((base + bytes).U)
        config.entries(0).entryValid.poke(true.B)
        config.entries(0).readAllowed.poke(true.B)
      }
      dut.reset.poke(true.B)
      dut.clock.step(6)
      cycles += 6
      dut.reset.poke(false.B)
      idle(6)
      dut.io.replayAllocated.expect(0.U)
      dut.io.entryValid.foreach(_.expect(false.B))
      record("scenario")
    }

    private def issue(request: Request): Unit = {
      assert(!requests.contains(request.serial))
      requests(request.serial) = request
      record("input", s"\"rob\":${request.serial},\"fullva\":${request.address}," +
        s"\"allowed\":${allowed(request)},\"access_fault\":${request.accessFault}")
      zero(dut.io.request.bits)
      dut.io.request.bits.src(0).poke(request.address.U)
      dut.io.request.bits.uop.fuType.poke(FuType.ldu.U)
      dut.io.request.bits.uop.fuOpType.poke(3.U)
      dut.io.request.bits.uop.rfWen.poke(true.B)
      dut.io.request.bits.uop.pdest.poke(request.destination.U)
      dut.io.request.bits.uop.robIdx.value.poke(request.serial.U)
      dut.io.request.bits.uop.lqIdx.value.poke(request.serial.U)
      dut.io.request.bits.uop.fdiNotTrusted.foreach(_.poke(true.B))
      dut.io.request.bits.isFirstIssue.poke(true.B)
      dut.io.request.valid.poke(true.B)
      waitFor(bool(dut.io.request.ready), 24, "issue ready")
      edge()
      dut.io.request.valid.poke(false.B)
      waitFor(queued(request.serial) > 0, 32, "initial queue write")
      idle(4)
    }

    private def grantBeat(block: BigInt, beat: Int, corrupt: Boolean = false): Unit = {
      val (source, keyword) = acquired(block)
      val physicalBeat = beat ^ (if (keyword) 1 else 0)
      val data = (0 until 4).map(i => word(block + physicalBeat * 32 + i * 8) << (i * 64)).reduce(_ | _)
      zero(dut.io.grant.bits)
      dut.io.grant.bits.opcode.poke(5.U)
      dut.io.grant.bits.param.poke(0.U)
      dut.io.grant.bits.size.poke(6.U)
      dut.io.grant.bits.source.poke(source.U)
      dut.io.grant.bits.data.poke(data.U)
      dut.io.grant.bits.corrupt.poke(corrupt.B)
      dut.io.grant.bits.echo.lift(IsKeywordKey).foreach(_.poke(keyword.B))
      dut.io.grant.valid.poke(true.B)
      waitFor(bool(dut.io.grant.ready), 16, "grant ready")
      edge()
      dut.io.grant.valid.poke(false.B)
      dut.io.grant.bits.corrupt.poke(false.B)
    }
    private def refill(block: BigInt, corrupt: Boolean = false): Unit = {
      grantBeat(block, 0, corrupt)
      grantBeat(block, 1, corrupt)
    }
    private def finish(): Unit = {
      waitFor(requests.keys.forall(serial => canceled(serial) || terminal(serial) == 1), 96, "terminal loads")
      idle(12)
      dut.io.replayAllocated.expect(0.U)
      requests.values.foreach { request =>
        if (!canceled(request.serial) && allowed(request)) {
          assert(attempts(request.serial) >= 1, "Cold load never traversed the real replay queue")
        }
      }
      acquired.values.foreach { case (source, _) =>
        assert(grants(source) == 2 && refills(source) == 2, "Incomplete grant/refill lifecycle")
      }
      dut.io.finishEnable.poke(true.B)
      waitFor(!dut.io.entryValid.exists(bool), 32, "MSHR release")
      idle(8)
      record("scenario-pass")
    }

    def run(): Unit = {
      reset("denied-new-and-merge", bytes = 8)
      issue(Request(1, base + 16))
      issue(Request(2, base))
      // No grant or hint can release the older miss during this denied issue.
      // Compare only this window; later legal replay may merge into the MSHR.
      val beforeDenied = Seq("allocation", "merge", "acquire").map(counts)
      val entriesBeforeDenied = dut.io.entryValid.map(bool).toVector
      val acquiredBeforeDenied = acquired.toMap
      issue(Request(3, base + 24))
      if (enabled) {
        assert(Seq("allocation", "merge", "acquire").map(counts) == beforeDenied,
          "Denied load allocated, merged or acquired while an older miss was pending")
        assert(dut.io.entryValid.map(bool).toVector == entriesBeforeDenied,
          "Denied load changed MSHR occupancy")
        assert(acquired.toMap == acquiredBeforeDenied, "Denied load changed accepted acquires")
      }
      refill(base)
      finish()
      assert(counts("allocation") == 1 && counts("acquire") == 1)
      if (!enabled) assert(counts("merge") >= 2)

      reset("independent-lines-reverse-refill")
      issue(Request(4, base))
      issue(Request(5, base + 64))
      assert(acquired(base)._1 != acquired(base + 64)._1)
      refill(base + 64)
      idle(16)
      assert(terminal(5) == 1 && terminal(4) == 0, "One MSHR woke another line")
      refill(base)
      finish()
      assert(counts("forward-mshr") >= 2)

      reset("same-line-merge-keyword")
      issue(Request(6, base + 32))
      issue(Request(7, base + 40))
      refill(base)
      finish()
      assert(counts("acquire") == 1 && counts("merge") >= 1)

      reset("cancel-younger-keep-older")
      issue(Request(8, base))
      issue(Request(9, base + 64))
      canceled += 9
      dut.io.redirect.valid.poke(true.B)
      dut.io.redirect.bits.robIdx.value.poke(8.U)
      dut.io.redirect.bits.level.poke(RedirectLevel.flushAfter)
      edge()
      dut.io.redirect.valid.poke(false.B)
      refill(base + 64)
      refill(base)
      finish()
      assert(terminal(9) == 0 && terminal(8) == 1)

      reset("corrupt-mshr")
      issue(Request(10, base, accessFault = true))
      refill(base, corrupt = true)
      finish()
      assert(counts("forward-mshr") > 0)

      // The early wakeup is an ordinary L2 hint. GrantData is offered at the
      // resulting S1 forward query, before this real MSHR stores the first beat.
      // Different adjacent-beat errors expose metadata skew independently of data.
      Seq((false, false), (true, false), (false, true)).zipWithIndex.foreach {
        case ((firstCorrupt, secondCorrupt), index) =>
          reset(s"dchannel-corrupt-$firstCorrupt-$secondCorrupt")
          val request = Request(11 + index, base, accessFault = firstCorrupt)
          issue(request)
          val (source, keyword) = acquired(base)
          dut.io.hint.valid.poke(true.B)
          dut.io.hint.bits.sourceId.poke(source.U)
          dut.io.hint.bits.isKeyword.poke(keyword.B)
          edge()
          dut.io.hint.valid.poke(false.B)
          waitFor(bool(dut.io.forwardRequest.valid), 16, "hint-woken S1 forward query")
          dut.io.forwardRequest.bits.mshrid.expect(source.U)
          grantBeat(base, 0, firstCorrupt)
          grantBeat(base, 1, secondCorrupt)
          finish()
          assert(counts("forward-d") == 1, "Trace did not consume the real D-channel beat")
      }
    }
  }

  it should "preserve permission and transaction identity through grants, forwarding and cancellation" in {
    val root = Paths.get(sys.props("l01.runRoot")).toRealPath()
    require(Paths.get("").toRealPath() == root, "Run from the dedicated L02 evidence directory")
    val path = root.resolve(if (enabled) "replay-miss-enabled" else "replay-miss-disabled")
    require(!Files.exists(path), s"Evidence directory already exists: $path")
    Files.createDirectory(path)
    implicit val p: Parameters = UserTimerDeliveryParameters(enabled)
    val options = p(DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    val workspace = new Workspace(path.resolve("compiled").toString)
    workspace.reset()
    val module = workspace.elaborateGeneratedModule(() => new FDILoadReplayMissHarness)
    workspace.generateAdditionalSources()
    val common = CommonCompilationSettings(availableParallelism =
      CommonCompilationSettings.AvailableParallelism.UpTo(Runtime.getRuntime.availableProcessors()))
    val settings = Backend.CompilationSettings(disabledWarnings = Seq("WIDTH", "STMTDLY"),
      disableFatalExitOnWarnings = true, enableAllAssertions = true)
    val simulation = workspace.compile(Backend.initializeFromProcessEnvironment())("verilator", common, settings, None, false)
    simulation.runElaboratedModule(module) { elaborated =>
      val driver = new Driver(elaborated.wrapped)
      try {
        driver.run()
        println(s"FDI load replay miss PASS enabled=$enabled cycles=${driver.cycles} " +
          s"counts=${driver.totals.toSeq.sortBy(_._1).mkString(",")} " +
          "fixture=production-LoadUnit-LoadPipe-MissQueue-LoadQueueReplay tlb=identity-service arrays=cold " +
          "grant=real-TileLink-data scope=grant-forward-replay-terminal")
      } finally {
        Files.write(path.resolve("replay-miss-events.jsonl"),
          driver.events.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
      }
    }
  }
}
