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
import xiangshan.backend.fu.FuType
import xiangshan.mem.Bundles.LsPipelineBundle

class FDIStoreMisalignRevokeTest extends AnyFlatSpec {
  behavior of "StoreMisalignBuffer accepted-owner revocation"

  private case class Source(name: String, address: BigInt, pc: BigInt, rob: Int, sq: Int, ftq: Int)
  private val a = Source("A", 0x1ffd, 0x1008, 12, 12, 3)
  private val b = Source("B", 0x200d, 0x1000, 10, 10, 2)
  private val following = Source("following", 0x300d, 0x100c, 14, 14, 4)
  private case class Sample(splitValid: Boolean, splitFire: Boolean, writebackValid: Boolean,
                            writebackFire: Boolean, full: Boolean)

  private def parameters(): Parameters = {
    val (base, _, _) = top.ArgParser.parse(Array(
      "--config", "FpgaDefaultConfig", "--num-cores", "1",
      "--l2-cache-size", "256", "--l3-cache-size", "768",
      "--fpga-platform", "--disable-always-basic-diff", "--disable-perf", "--disable-alwaysdb"))
    val core = base(XSTileKey).head.copy(HasFDI = true)
    require(core.HasVPU && core.VLEN == 128)
    base.alterPartial { case XSCoreParamsKey => core }
  }

  private class Driver(dut: StoreMisalignBuffer, label: String, events: mutable.ArrayBuffer[String]) {
    require(dut.io.enq.length == 2, "The reproducer targets the production two-STA configuration")
    private var cycle = 0
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

    private def driveSource(dst: LsPipelineBundle, source: Source): Unit = {
      zero(dst)
      dst.vaddr.poke(source.address.U)
      dst.fullva.poke(source.address.U)
      dst.paddr.poke(source.address.U)
      dst.mask.poke(0xff.U)
      dst.vecActive.poke(true.B)
      dst.isMisalign.poke(true.B)
      dst.uop.pc.poke(source.pc.U)
      dst.uop.instr.poke(0x00113023.U) // sd x1, 0(x2)
      dst.uop.fuType.poke(FuType.stu.U)
      dst.uop.fuOpType.poke(3.U)
      dst.uop.robIdx.flag.poke(false.B)
      dst.uop.robIdx.value.poke(source.rob.U)
      dst.uop.sqIdx.flag.poke(false.B)
      dst.uop.sqIdx.value.poke(source.sq.U)
      dst.uop.ftqPtr.flag.poke(false.B)
      dst.uop.ftqPtr.value.poke(source.ftq.U)
      dst.uop.ftqOffset.poke(0.U)
      dst.uop.firstUop.poke(true.B)
      dst.uop.lastUop.poke(true.B)
      dst.uop.fdiNotTrusted.foreach(_.poke(true.B))
    }

    private def ownerFields(value: LsPipelineBundle): String =
      s"\"va\":${uint(value.vaddr)},\"fullva\":${uint(value.fullva)}," +
        s"\"rob_flag\":${bool(value.uop.robIdx.flag)},\"rob\":${uint(value.uop.robIdx.value)}," +
        s"\"sq_flag\":${bool(value.uop.sqIdx.flag)},\"sq\":${uint(value.uop.sqIdx.value)}," +
        s"\"ftq_flag\":${bool(value.uop.ftqPtr.flag)},\"ftq\":${uint(value.uop.ftqPtr.value)}," +
        s"\"ftq_offset\":${uint(value.uop.ftqOffset)},\"uop\":${uint(value.uop.uopIdx)}," +
        s"\"operation\":${uint(value.uop.fuOpType)},\"mask\":${uint(value.mask)}"

    private def edge(phase: String): Sample = {
      val split = dut.io.splitStoreReq
      val wb = dut.io.writeBack
      val sample = Sample(bool(split.valid), bool(split.valid) && bool(split.ready),
        bool(wb.valid), bool(wb.valid) && bool(wb.ready), bool(dut.io.full))
      val inputs = dut.io.enq.zipWithIndex.map { case (port, index) =>
        val valid = bool(port.req.valid)
        s"{\"port\":$index,\"valid\":$valid,\"ready\":${bool(port.req.ready)}," +
          s"\"fire\":${valid && bool(port.req.ready)},\"revoke\":${bool(port.revoke)}," +
          ownerFields(port.req.bits) + "}"
      }.mkString("[", ",", "]")
      events += s"{\"case\":\"$label\",\"cycle\":$cycle,\"phase\":\"$phase\",\"enq\":$inputs," +
        s"\"split_valid\":${sample.splitValid},\"split_ready\":${bool(split.ready)}," +
        s"\"split_fire\":${sample.splitFire},\"full\":${sample.full}," +
        s"\"response_valid\":${bool(dut.io.splitStoreResp.valid)}," +
        s"\"writeback_valid\":${sample.writebackValid},\"writeback_fire\":${sample.writebackFire}" +
        (if (sample.splitValid) ",\"split\":{" + ownerFields(split.bits) + "}" else "") +
        (if (sample.writebackValid) s",\"writeback_rob\":${uint(wb.bits.uop.robIdx.value)}" else "") + "}"
      dut.clock.step()
      cycle += 1
      sample
    }

    private def noOutput(sample: Sample, phase: String): Unit = {
      assert(!sample.splitValid && !sample.writebackValid, s"Unexpected output during $label/$phase")
    }

    def reset(): Unit = {
      dut.io.redirect.valid.poke(false.B)
      zero(dut.io.redirect.bits)
      dut.io.enq.foreach { port =>
        port.req.valid.poke(false.B)
        zero(port.req.bits)
        port.revoke.poke(false.B)
      }
      dut.io.rob.lcommit.poke(0.U)
      dut.io.rob.scommit.poke(0.U)
      dut.io.rob.pendingMMIOld.poke(false.B)
      dut.io.rob.pendingld.poke(false.B)
      dut.io.rob.pendingst.poke(false.B)
      dut.io.rob.pendingVst.poke(false.B)
      dut.io.rob.commit.poke(false.B)
      zero(dut.io.rob.pendingPtr)
      zero(dut.io.rob.pendingPtrNext)
      dut.io.splitStoreReq.ready.poke(true.B)
      dut.io.splitStoreResp.valid.poke(false.B)
      zero(dut.io.splitStoreResp.bits)
      dut.io.writeBack.ready.poke(true.B)
      dut.io.vecWriteBack.foreach(_.ready.poke(true.B))
      dut.io.storeOutValid.poke(false.B)
      dut.io.storeVecOutValid.poke(false.B)
      zero(dut.io.sqControl.toStoreMisalignBuffer)
      dut.reset.poke(true.B)
      dut.clock.step(3)
      dut.reset.poke(false.B)
      noOutput(edge("reset-released"), "reset-released")
      dut.io.full.expect(false.B)
    }

    private def accept(source: Source, port: Int): Unit = {
      driveSource(dut.io.enq(port).req.bits, source)
      dut.io.enq(port).req.valid.poke(true.B)
      dut.io.enq(port).req.ready.expect(true.B)
      noOutput(edge(s"accept-${source.name}-port-$port"), "accept")
      dut.io.enq(port).req.valid.poke(false.B)
      dut.io.full.expect(true.B)
    }

    def replace(firstPort: Int, deny: Boolean): Unit = {
      val secondPort = 1 - firstPort
      accept(a, firstPort)
      // This is exactly the S2 cycle following the accepted original request.
      dut.io.enq(firstPort).revoke.poke(false.B)
      noOutput(edge("A-S2-allow"), "A-S2-allow")
      (0 until 2).foreach { _ =>
        noOutput(edge("A-cross-page-awaiting-ROB"), "A-awaiting-ROB")
        dut.io.full.expect(true.B)
      }
      accept(b, secondPort)
      dut.io.enq(secondPort).revoke.poke(deny.B)
      noOutput(edge(if (deny) "B-S2-revoke" else "B-S2-allow"), "B-S2")
      dut.io.enq(secondPort).revoke.poke(false.B)
    }

    private def checkOwner(value: LsPipelineBundle, source: Source): Unit = {
      value.uop.robIdx.flag.expect(false.B)
      value.uop.robIdx.value.expect(source.rob.U)
      value.uop.sqIdx.flag.expect(false.B)
      value.uop.sqIdx.value.expect(source.sq.U)
      value.uop.ftqPtr.flag.expect(false.B)
      value.uop.ftqPtr.value.expect(source.ftq.U)
      value.uop.ftqOffset.expect(0.U)
      value.uop.uopIdx.expect(0.U)
    }

    // These aligned SD fragments are fixed by the stimulus's offset 5 and size 8.
    // Expectations never use the DUT output address, mask or permission state.
    private def fragment(source: Source, index: Int): (BigInt, BigInt) = {
      require(index == 0 || index == 1)
      if (index == 0) (source.address - 5, BigInt(0xff00))
      else (source.address + 3, BigInt(0x00ff))
    }

    def completeAllowed(source: Source): Unit = {
      val responses = mutable.Queue.empty[(Int, Int)]
      var requests = 0
      var completions = 0
      var finished = false
      var elapsed = 0
      while (!finished && elapsed < 96) {
        dut.io.splitStoreResp.valid.poke(false.B)
        if (responses.headOption.exists(_._1 == cycle)) {
          val (_, index) = responses.dequeue()
          val (address, mask) = fragment(source, index)
          driveSource(dut.io.splitStoreResp.bits, source)
          dut.io.splitStoreResp.bits.vaddr.poke(address.U)
          dut.io.splitStoreResp.bits.paddr.poke(address.U)
          dut.io.splitStoreResp.bits.mask.poke(mask.U)
          dut.io.splitStoreResp.bits.isMisalign.poke(false.B)
          dut.io.splitStoreResp.valid.poke(true.B)
        }
        if (bool(dut.io.splitStoreReq.valid)) {
          assert(requests < 2, s"Duplicate fragment for $label/${source.name}")
          checkOwner(dut.io.splitStoreReq.bits, source)
          val (address, mask) = fragment(source, requests)
          dut.io.splitStoreReq.bits.vaddr.expect(address.U)
          dut.io.splitStoreReq.bits.mask.expect(mask.U)
          dut.io.splitStoreReq.bits.uop.fuOpType.expect(3.U)
          dut.io.splitStoreReq.bits.isFinalSplit.expect((requests == 1).B)
          dut.io.splitStoreReq.ready.expect(true.B)
          responses.enqueue((cycle + 2, requests))
          requests += 1
        }
        if (bool(dut.io.writeBack.valid)) {
          dut.io.writeBack.bits.uop.robIdx.flag.expect(false.B)
          dut.io.writeBack.bits.uop.robIdx.value.expect(source.rob.U)
          dut.io.writeBack.bits.uop.sqIdx.value.expect(source.sq.U)
          completions += 1
        }
        edge(s"service-${source.name}")
        elapsed += 1
        finished = completions == 1 && !bool(dut.io.full)
      }
      dut.io.splitStoreResp.valid.poke(false.B)
      assert(finished && requests == 2 && completions == 1 && responses.isEmpty,
        s"Allowed replacement did not complete exactly once: $label/${source.name} requests=$requests wb=$completions")
      noOutput(edge("allowed-complete"), "allowed-complete")
    }

    def deniedWindowAndCleanup(secondPort: Int): Int = {
      var requests = 0
      var writebacks = 0
      (0 until 12).foreach { _ =>
        if (bool(dut.io.splitStoreReq.valid)) checkOwner(dut.io.splitStoreReq.bits, b)
        val sample = edge("denied-observation-window")
        if (sample.splitValid) requests += 1
        if (sample.writebackValid) writebacks += 1
      }
      // An exception redirect removes the denied instruction through its real public port.
      // It occurs after the fixed observation window, never before the S2 revoke.
      dut.io.redirect.bits.robIdx.flag.poke(false.B)
      dut.io.redirect.bits.robIdx.value.poke(b.rob.U)
      dut.io.redirect.bits.level.poke(RedirectLevel.flush)
      dut.io.redirect.valid.poke(true.B)
      edge("denied-exception-redirect")
      dut.io.redirect.valid.poke(false.B)
      dut.io.full.expect(false.B)
      noOutput(edge("denied-cleanup-complete"), "denied-cleanup-complete")
      assert(writebacks == 0, s"Denied owner wrote back without its exception path: $label")
      accept(following, secondPort)
      noOutput(edge("following-S2-allow"), "following-S2-allow")
      completeAllowed(following)
      events += s"{\"case\":\"$label\",\"event\":\"denied-summary\",\"observed_split_requests\":$requests," +
        "\"expected_split_requests\":0,\"following_completed\":true}"
      requests
    }

    // Boundary control records describe driven public inputs; edge() separately
    // records the actual request handshakes and downstream outputs at that edge.
    private def boundaryEdge(phase: String, resetActive: Boolean = false): Sample = {
      events += s"{\"case\":\"$label\",\"cycle\":$cycle,\"event\":\"boundary-control\"," +
        s"\"phase\":\"$phase\",\"reset_driven\":$resetActive," +
        s"\"redirect_valid\":${bool(dut.io.redirect.valid)}," +
        s"\"redirect_rob_flag\":${bool(dut.io.redirect.bits.robIdx.flag)}," +
        s"\"redirect_rob\":${uint(dut.io.redirect.bits.robIdx.value)}," +
        s"\"redirect_level\":${uint(dut.io.redirect.bits.level)}}"
      edge(phase)
    }

    private def holdCrossPageA(firstPort: Int): Unit = {
      accept(a, firstPort)
      dut.io.enq(firstPort).revoke.poke(false.B)
      noOutput(boundaryEdge("A-S2-allow-before-boundary"), "A-S2-before-boundary")
      (0 until 2).foreach { _ =>
        noOutput(boundaryEdge("A-held-before-boundary"), "A-held-before-boundary")
        dut.io.full.expect(true.B)
      }
    }

    private def driveRedirect(rob: Int, itself: Boolean): Unit = {
      zero(dut.io.redirect.bits)
      dut.io.redirect.bits.robIdx.flag.poke(false.B)
      dut.io.redirect.bits.robIdx.value.poke(rob.U)
      dut.io.redirect.bits.level.poke((if (itself) 1 else 0).U)
      dut.io.redirect.valid.poke(true.B)
    }

    private def quietBoundary(phase: String, occupied: Boolean): Unit = {
      (0 until 12).foreach { _ =>
        dut.io.full.expect(occupied.B)
        noOutput(boundaryEdge(phase), phase)
      }
      dut.io.full.expect(occupied.B)
    }

    private def freshAfterBoundary(port: Int): Unit = {
      dut.io.full.expect(false.B)
      accept(following, port)
      dut.io.enq(port).revoke.poke(false.B)
      noOutput(boundaryEdge("fresh-S2-allow-after-boundary"), "fresh-S2-after-boundary")
      completeAllowed(following)
      events += s"{\"case\":\"$label\",\"event\":\"boundary-complete\"," +
        "\"fresh_rob\":14,\"fresh_fragments\":2,\"fresh_writebacks\":1,\"empty\":true}"
    }

    def killingRedirectAtReplacement(firstPort: Int): Unit = {
      val secondPort = 1 - firstPort
      holdCrossPageA(firstPort)
      driveSource(dut.io.enq(secondPort).req.bits, b)
      dut.io.enq(secondPort).req.valid.poke(true.B)
      // ROB9 flushAfter kills both B10 and the occupied A12.
      driveRedirect(9, itself = false)
      dut.io.enq(secondPort).req.ready.expect(false.B)
      noOutput(boundaryEdge("older-redirect-rejects-B-replacement"), "older-redirect-replacement")
      dut.io.enq(secondPort).req.valid.poke(false.B)
      dut.io.redirect.valid.poke(false.B)
      dut.io.full.expect(false.B)
      dut.io.enq(secondPort).revoke.poke(true.B)
      noOutput(boundaryEdge("canceled-B-next-cycle-revoke"), "canceled-B-revoke")
      dut.io.enq(secondPort).revoke.poke(false.B)
      quietBoundary("killed-replacement-has-no-late-output", occupied = false)
      freshAfterBoundary(secondPort)
    }

    def oldOwnerOnlyRedirect(firstPort: Int, deny: Boolean): Unit = {
      val secondPort = 1 - firstPort
      holdCrossPageA(firstPort)
      driveSource(dut.io.enq(secondPort).req.bits, b)
      dut.io.enq(secondPort).req.valid.poke(true.B)
      // ROB11 flushAfter kills A12 while the older B10 replacement survives.
      driveRedirect(11, itself = false)
      dut.io.enq(secondPort).req.ready.expect(true.B)
      noOutput(boundaryEdge("A-only-redirect-accepts-B-replacement"), "A-only-redirect-replacement")
      dut.io.enq(secondPort).req.valid.poke(false.B)
      dut.io.redirect.valid.poke(false.B)
      dut.io.full.expect(true.B)
      dut.io.enq(secondPort).revoke.poke(deny.B)
      noOutput(boundaryEdge(if (deny) "surviving-B-S2-revoke" else "surviving-B-S2-allow"), "surviving-B-S2")
      dut.io.enq(secondPort).revoke.poke(false.B)
      if (deny) {
        quietBoundary("surviving-revoked-B-has-no-output", occupied = true)
        driveRedirect(b.rob, itself = true)
        noOutput(boundaryEdge("surviving-denied-B-exception-cleanup"), "surviving-B-cleanup")
        dut.io.redirect.valid.poke(false.B)
        dut.io.full.expect(false.B)
        noOutput(boundaryEdge("surviving-denied-B-cleanup-complete"), "surviving-B-empty")
      } else {
        // The allowed counterpart proves the retained owner is B, not an empty
        // or stale A slot that could also satisfy a negative-output check.
        completeAllowed(b)
      }
      freshAfterBoundary(secondPort)
    }

    def revokeAndFlushSelf(firstPort: Int): Unit = {
      val secondPort = 1 - firstPort
      holdCrossPageA(firstPort)
      accept(b, secondPort)
      dut.io.enq(secondPort).revoke.poke(true.B)
      driveRedirect(b.rob, itself = true)
      noOutput(boundaryEdge("B-S2-revoke-and-flush-self"), "B-S2-revoke-flush")
      dut.io.enq(secondPort).revoke.poke(false.B)
      dut.io.redirect.valid.poke(false.B)
      dut.io.full.expect(false.B)
      quietBoundary("B-flush-self-has-no-late-output", occupied = false)
      freshAfterBoundary(secondPort)
    }

    def resetAtReplacementBoundary(firstPort: Int, atAcceptance: Boolean): Unit = {
      val secondPort = 1 - firstPort
      holdCrossPageA(firstPort)
      if (atAcceptance) {
        driveSource(dut.io.enq(secondPort).req.bits, b)
        dut.io.enq(secondPort).req.valid.poke(true.B)
        dut.reset.poke(true.B)
        // Raw ready/fire during reset does not create a live accepted owner.
        noOutput(boundaryEdge("reset-at-B-replacement-opportunity", resetActive = true), "reset-B-opportunity")
        dut.io.enq(secondPort).req.valid.poke(false.B)
        dut.io.enq(secondPort).revoke.poke(true.B)
        noOutput(boundaryEdge("reset-held-at-B-next-cycle-revoke", resetActive = true), "reset-B-next-revoke")
      } else {
        accept(b, secondPort)
        dut.io.enq(secondPort).revoke.poke(true.B)
        dut.reset.poke(true.B)
        noOutput(boundaryEdge("reset-at-accepted-B-S2-revoke", resetActive = true), "reset-B-S2")
        dut.io.enq(secondPort).revoke.poke(false.B)
        noOutput(boundaryEdge("reset-held-after-B-S2", resetActive = true), "reset-after-B-S2")
      }
      dut.io.enq(secondPort).req.valid.poke(false.B)
      dut.io.enq(secondPort).revoke.poke(false.B)
      dut.reset.poke(false.B)
      dut.io.full.expect(false.B)
      quietBoundary("post-reset-old-owner-has-no-output", occupied = false)
      freshAfterBoundary(secondPort)
    }
  }

  private def simulate(label: String)(body: (StoreMisalignBuffer, mutable.ArrayBuffer[String]) => Unit): Unit = {
    val root = Paths.get(sys.props("l03.runRoot")).toRealPath()
    require(Paths.get("").toRealPath() == root, "Run from the dedicated L03 evidence directory")
    val path = root.resolve(s"misalign-revoke-$label")
    require(!Files.exists(path), s"Evidence directory already exists: $path")
    Files.createDirectory(path)
    implicit val p: Parameters = parameters()
    val options = p(DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    val workspace = new Workspace(path.resolve("compiled").toString)
    workspace.reset()
    val module = workspace.elaborateGeneratedModule(() => new StoreMisalignBuffer)
    workspace.generateAdditionalSources()
    val common = CommonCompilationSettings(availableParallelism =
      CommonCompilationSettings.AvailableParallelism.UpTo(Runtime.getRuntime.availableProcessors()))
    val settings = Backend.CompilationSettings(disabledWarnings = Seq("WIDTH", "STMTDLY"),
      disableFatalExitOnWarnings = true, enableAllAssertions = true)
    val simulation = workspace.compile(Backend.initializeFromProcessEnvironment())("verilator", common, settings, None, false)
    val events = mutable.ArrayBuffer.empty[String]
    try {
      simulation.runElaboratedModule(module) { elaborated => body(elaborated.wrapped, events) }
    } finally {
      Files.write(path.resolve("events.jsonl"), events.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
    }
  }

  it should "complete allowed older replacements through both real enqueue port orders" in {
    simulate("control") { (dut, events) =>
      for (firstPort <- 0 until 2) {
        val driver = new Driver(dut, s"allow-$firstPort-${1 - firstPort}", events)
        driver.reset()
        driver.replace(firstPort, deny = false)
        driver.completeAllowed(b)
      }
      println("Store misalign allowed replacement control PASS portOrders=2 fragmentsPerOwner=2")
    }
  }

  it should "emit no downstream fragment for a replacing owner revoked in its next cycle" in {
    simulate("denied") { (dut, events) =>
      val observed = (0 until 2).map { firstPort =>
        val driver = new Driver(dut, s"deny-$firstPort-${1 - firstPort}", events)
        driver.reset()
        driver.replace(firstPort, deny = true)
        driver.deniedWindowAndCleanup(1 - firstPort)
      }
      // Later redirect cleanup cannot excuse an earlier request from a revoked owner.
      assert(observed.forall(_ == 0), s"Revoked replacement emitted downstream requests: portOrders=$observed; expected all zero")
      println("Store misalign revoked replacement PASS portOrders=2 downstreamRequests=0 cleanupAndFollowing=2")
    }
  }

  it should "suppress a replacement killed by an older same-cycle redirect" in {
    simulate("boundary-kill-replacement") { (dut, events) =>
      for (firstPort <- 0 until 2) {
        val driver = new Driver(dut, s"kill-replacement-$firstPort-${1 - firstPort}", events)
        driver.reset()
        driver.killingRedirectAtReplacement(firstPort)
      }
      println("Store misalign replacement cancellation PASS portOrders=2 canceledDownstreamRequests=0 freshCompletions=2")
    }
  }

  it should "retain the older replacement and its revoke when redirect kills only the old owner" in {
    simulate("boundary-kill-old-owner") { (dut, events) =>
      for (firstPort <- 0 until 2; deny <- Seq(true, false)) {
        val driver = new Driver(dut, s"kill-A-keep-B-$firstPort-${1 - firstPort}-deny-$deny", events)
        driver.reset()
        driver.oldOwnerOnlyRedirect(firstPort, deny)
      }
      println("Store misalign surviving replacement PASS portOrders=2 deniedCases=2 allowedControls=2 freshCompletions=4")
    }
  }

  it should "clear a replacing owner when its revoke and flush-self coincide" in {
    simulate("boundary-revoke-flush-self") { (dut, events) =>
      for (firstPort <- 0 until 2) {
        val driver = new Driver(dut, s"revoke-flush-self-$firstPort-${1 - firstPort}", events)
        driver.reset()
        driver.revokeAndFlushSelf(firstPort)
      }
      println("Store misalign revoke and flush-self PASS portOrders=2 canceledDownstreamRequests=0 freshCompletions=2")
    }
  }

  it should "discard pre-reset replacement ownership at the acceptance and revoke boundaries" in {
    simulate("boundary-reset") { (dut, events) =>
      for (firstPort <- 0 until 2; atAcceptance <- Seq(true, false)) {
        val driver = new Driver(dut, s"reset-boundary-$firstPort-${1 - firstPort}-acceptance-$atAcceptance", events)
        driver.reset()
        driver.resetAtReplacementBoundary(firstPort, atAcceptance)
      }
      println("Store misalign replacement reset PASS portOrders=2 resetPositions=2 staleDownstreamRequests=0 freshCompletions=4")
    }
  }
}
