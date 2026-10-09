// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.mem

import chisel3._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import xiangshan._
import xiangshan.backend.fu.FuType

class FDILoadReplayTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Ordinary scalar retries across production load lanes"
  private val enabled = sys.env.getOrElse("L01_FDI_ENABLED",
    throw new IllegalArgumentException("L01_FDI_ENABLED must be explicit")).toBoolean
  private val wordMask = (BigInt(1) << 64) - 1
  private val negative = BigInt("fffffffffff08000", 16)
  private def zero(data: Data): Unit = data match {
    case value: Bool => value.poke(false.B)
    case value: UInt => value.poke(0.U)
    case value: SInt => value.poke(0.S)
    case value: Vec[_] => value.foreach(zero)
    case value: Record => value.elements.values.foreach(zero)
    case other => throw new IllegalArgumentException(s"Unsupported input: ${other.getClass}")
  }
  private def uint(data: UInt): BigInt = data.peek().litValue
  private def bool(data: Bool): Boolean = data.peek().litToBoolean

  private case class Source(serial: Int, rob: Int, flag: Boolean, lq: Int, address: BigInt,
    retries: Int, fp: Boolean = false, denied: Boolean = false, canceled: Boolean = false) {
    val data: BigInt = (BigInt("915e24a7c063bd80", 16) ^ (BigInt(serial + 1) * 0x1020305)) & wordMask
    val sqData: BigInt = data ^ BigInt("1b3d5f7193b5d7f9", 16)
    val sbData: BigInt = data ^ BigInt("e4c2a0866442200e", 16)
    val merged: BigInt = (data & BigInt("ffffffff00000000", 16)) |
      (sbData & BigInt("00000000ffff0000", 16)) | (sqData & 0xffff)
    val key: (Int, Boolean) = (rob, flag)
  }
  private case class Attempt(source: Source, lane: Int, accepted: Int, id: BigInt, ordinal: Int,
    var killed: Boolean = false)

  it should "retain transaction identity and complete each surviving load exactly once" in {
    val root = Paths.get(sys.props.getOrElse("l01.runRoot", ".")).toAbsolutePath
    Files.createDirectories(root)
    require(Paths.get("").toRealPath() == root.toRealPath(), "Run from the dedicated replay directory")
    val (base, _, _) = top.ArgParser.parse(Array("--config", "FpgaDefaultConfig", "--num-cores", "1",
      "--l2-cache-size", "256", "--l3-cache-size", "768", "--fpga-platform",
      "--disable-always-basic-diff", "--disable-perf", "--disable-alwaysdb"))
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled)
    }
    val options = p(DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    test(new FDILoadReplayHarness).withAnnotations(Seq(VerilatorBackendAnnotation,
      TargetDirAnnotation(s"load-replay-${if (enabled) "on" else "off"}"))) { dut =>
      val events = mutable.ArrayBuffer.empty[String]
      var cycle = 0
      var completedCases = 0
      def event(name: String, kind: String, details: String = ""): Unit =
        events += s"{\"cycle\":$cycle,\"case\":\"$name\",\"event\":\"$kind\"${if (details.isEmpty) "" else "," + details}}"
      def initialize(privilege: Int): Unit = {
        dut.io.redirect.valid.poke(false.B); zero(dut.io.redirect.bits)
        dut.io.tlbHint.valid.poke(false.B); zero(dut.io.tlbHint.bits)
        dut.io.rarFull.poke(false.B); dut.io.rawFull.poke(false.B)
        dut.io.sqEmpty.poke(true.B); dut.io.storeAddressReady.poke(true.B)
        dut.io.config.foreach { config =>
          zero(config)
          config.policy.uEnable.poke(true.B); config.policy.sEnable.poke(true.B)
          config.sourcePrivilege.poke(privilege.U)
          Seq((BigInt(0x8000), BigInt(0x8800)), (negative, negative + 0x800)).zipWithIndex.foreach {
            case ((lo, hi), index) =>
              config.entries(index).entryValid.poke(true.B)
              config.entries(index).readAllowed.poke(true.B)
              config.entries(index).boundLo.poke(lo.U); config.entries(index).boundHi.poke(hi.U)
          }
        }
        dut.io.lanes.foreach { lane =>
          lane.request.valid.poke(false.B); zero(lane.request.bits)
          lane.result.ready.poke(true.B); lane.cacheReady.poke(true.B)
          lane.response.valid.poke(false.B); zero(lane.response.bits)
          lane.bankConflict.poke(false.B); lane.cacheNack.poke(false.B)
          lane.wpuFailure.poke(false.B)
          lane.tlbMiss.poke(false.B); lane.tlbPageFault.poke(false.B)
          lane.nuke.valid.poke(false.B); zero(lane.nuke.bits)
          lane.rawReady.poke(true.B); lane.rarReady.poke(true.B)
          Seq(lane.sq, lane.sbuffer).foreach { port =>
            zero(port.forwardMaskFast); zero(port.forwardMask); zero(port.forwardData)
            port.dataInvalid.poke(false.B); port.matchInvalid.poke(false.B); port.addrInvalid.poke(false.B)
          }
          lane.sq.dataInvalidFast.poke(false.B)
          zero(lane.sq.dataInvalidSqIdx); zero(lane.sq.addrInvalidSqIdx)
        }
        dut.reset.poke(true.B); dut.clock.step(10); cycle += 10
        dut.reset.poke(false.B); dut.clock.step(20); cycle += 20
      }

      def run(name: String, count: Int, retries: Int, privilege: Int = 0,
        fast: Boolean = false, backpressure: Boolean = false, cancel: Boolean = false,
        reuse: Boolean = false, wrap: Boolean = false): Unit = {
        initialize(privilege)
        val begin = cycle
        val sources = (0 until count).map { index =>
          val absoluteRob = (if (wrap) dut.RobSize - 3 else 2) + index
          val denied = !cancel && !reuse && index == count - 1
          val address = if (denied) (negative & ((BigInt(1) << dut.VAddrBits) - 1))
            else if (index % 3 == 1) negative + 16 * index else BigInt(0x8000 + 16 * index)
          Source(index, absoluteRob % dut.RobSize, absoluteRob >= dut.RobSize,
            index % dut.VirtualLoadQueueSize, address, retries + index % 3,
            fp = index % 2 == 1, denied = denied, canceled = cancel && index == count - 1)
        }
        val byKey = sources.map(source => source.key -> source).toMap
        val issued = mutable.Set.empty[(Int, Boolean)]
        val completed = mutable.Set.empty[(Int, Boolean)]
        val attempts = mutable.ArrayBuffer.empty[Attempt]
        val permissionRequests = mutable.Map.empty[(Int, Boolean), Int].withDefaultValue(0)
        val permissionResponses = mutable.Map.empty[(Int, Boolean), Int].withDefaultValue(0)
        val pendingPermissions = Array.fill[Option[Source]](3)(None)
        val held = Array.fill[Option[(Source, BigInt)]](3)(None)
        val allocatedBefore = mutable.Set.empty[Int]
        val reusedSlots = mutable.Set.empty[Int]
        var previousMask = BigInt(0)
        var offered = 0
        var redirectSent = false
        var redirectCycle = -1
        var moved = 0
        var queueAttempts = 0
        var fastAttempts = 0
        var fastFallbacks = 0
        var heldCycles = 0
        var finishedAt = -1
        val expectedCompletions = sources.count(!_.canceled)
        def identity(bundle: xiangshan.mem.Bundles.LsPipelineBundle, source: Source): Unit = {
          bundle.uop.robIdx.value.expect(source.rob.U); bundle.uop.robIdx.flag.expect(source.flag.B)
          bundle.uop.lqIdx.value.expect(source.lq.U)
          bundle.uop.pdest.expect((source.serial % 24 + 8).U)
          bundle.uop.fuOpType.expect(3.U)
          bundle.uop.fdiNotTrusted.foreach(_.expect(true.B))
          bundle.vaddr.expect((source.address & ((BigInt(1) << dut.VAddrBits) - 1)).U)
          if (enabled) {
            bundle.fullva.expect(source.address.U)
            bundle.fdiSourcePrivilege.get.expect(privilege.U)
            bundle.fdiSourceVirtual.get.expect(false.B)
          }
        }
        def sourceOf(rob: UInt, flag: Bool): Source = byKey.getOrElse((uint(rob).toInt, bool(flag)),
          throw new AssertionError(s"Unknown transaction in $name"))
        def permissionTag(source: Source): BigInt = {
          val uop = dut.io.lanes.head.request.bits.uop
          val rob = BigInt(source.rob) | (if (source.flag) BigInt(1) << uop.robIdx.value.getWidth else BigInt(0))
          (rob << (uop.lqIdx.value.getWidth + 1 + uop.uopIdx.getWidth)) |
            (BigInt(source.lq) << uop.uopIdx.getWidth)
        }
        while ((finishedAt < 0 || cycle < finishedAt + 35) && cycle < begin + 16000) {
          dut.io.redirect.valid.poke(false.B)
          val cancelReady = cancel && !redirectSent && issued.size == sources.size &&
            sources.forall(source => attempts.count(_.source == source) >= 2) && uint(dut.io.allocated) > 0
          if (cancelReady) {
            val survivor = sources(sources.size - 2)
            dut.io.redirect.valid.poke(true.B)
            dut.io.redirect.bits.robIdx.value.poke(survivor.rob.U)
            dut.io.redirect.bits.robIdx.flag.poke(survivor.flag.B)
            dut.io.redirect.bits.level.poke(RedirectLevel.flushAfter)
            redirectSent = true; redirectCycle = cycle
            event(name, "redirect", s"\"victim\":${sources.last.rob},\"survivor\":${survivor.rob}")
          }
          dut.io.lanes.zipWithIndex.foreach { case (lane, index) =>
            lane.request.valid.poke(false.B)
            val fallback = fast && attempts.exists(a => a.lane == index && a.accepted + 3 == cycle && a.ordinal == 0)
            lane.cacheReady.poke((!fallback && !(backpressure && (cycle - begin) % 17 < 5)).B)
            lane.response.valid.poke(false.B); zero(lane.response.bits)
            lane.bankConflict.poke(false.B); lane.cacheNack.poke(false.B)
            lane.sq.dataInvalidFast.poke(false.B); lane.sq.dataInvalid.poke(false.B)
            lane.sq.addrInvalid.poke(false.B)
            Seq(lane.sq, lane.sbuffer).foreach { port =>
              zero(port.forwardMaskFast); zero(port.forwardMask); zero(port.forwardData)
            }
            val s1 = attempts.find(a => a.lane == index && a.accepted + 1 == cycle)
            s1.foreach { attempt =>
              val needsRetry = attempt.ordinal < attempt.source.retries
              lane.sq.dataInvalidFast.poke((needsRetry && !(fast && attempt.ordinal == 0)).B)
              if (!needsRetry) {
                val offset = (attempt.source.address & 15).toInt
                (0 until 2).foreach(byte => lane.sq.forwardMaskFast(offset + byte).poke(true.B))
                (2 until 4).foreach(byte => lane.sbuffer.forwardMaskFast(offset + byte).poke(true.B))
              }
              attempt.killed = bool(lane.cacheS1Kill)
            }
            attempts.find(a => a.lane == index && a.accepted + 2 == cycle && !a.killed).foreach { attempt =>
              lane.response.valid.poke(true.B); lane.response.bits.id.poke(attempt.id.U)
              lane.response.bits.data.poke((attempt.source.data | (attempt.source.data << 64)).U)
              val needsRetry = attempt.ordinal < attempt.source.retries
              lane.bankConflict.poke((fast && attempt.ordinal == 0).B)
              lane.sq.dataInvalid.poke((needsRetry && !(fast && attempt.ordinal == 0)).B)
              if (!needsRetry) {
                val offset = (attempt.source.address & 15).toInt
                (0 until 2).foreach(byte => lane.sq.forwardMask(offset + byte).poke(true.B))
                (2 until 4).foreach(byte => lane.sbuffer.forwardMask(offset + byte).poke(true.B))
                (0 until 16).foreach { byte =>
                  lane.sq.forwardData(byte).poke(((attempt.source.sqData >> (8 * (byte % 8))) & 255).U)
                  lane.sbuffer.forwardData(byte).poke(((attempt.source.sbData >> (8 * (byte % 8))) & 255).U)
                }
              }
            }
            attempts.find(a => a.lane == index && a.accepted + 3 == cycle && !a.killed).foreach { attempt =>
              lane.response.bits.data_delayed.poke((attempt.source.data | (attempt.source.data << 64)).U)
            }
          }

          // Fresh instructions intentionally enter only lane two. Queue slot ownership
          // determines their later production replay lane, independently of this source.
          if (offered < sources.size && (!reuse || issued.size == completed.size)) {
            val source = sources(offered)
            val request = dut.io.lanes(2).request
            zero(request.bits)
            request.bits.src(0).poke(source.address.U)
            request.bits.uop.robIdx.value.poke(source.rob.U); request.bits.uop.robIdx.flag.poke(source.flag.B)
            request.bits.uop.lqIdx.value.poke(source.lq.U)
            request.bits.uop.pdest.poke((source.serial % 24 + 8).U)
            request.bits.uop.fuType.poke(FuType.ldu.U); request.bits.uop.fuOpType.poke(3.U)
            request.bits.uop.rfWen.poke((!source.fp).B); request.bits.uop.fpWen.poke(source.fp.B)
            request.bits.uop.firstUop.poke(true.B); request.bits.uop.lastUop.poke(true.B)
            request.bits.uop.fdiNotTrusted.foreach(_.poke(true.B))
            request.bits.isFirstIssue.poke(true.B); request.valid.poke(true.B)
            if (bool(request.ready)) {
              assert(issued.add(source.key)); offered += 1
              event(name, "issue", s"\"rob\":${source.rob},\"flag\":${source.flag},\"fullva\":${source.address}")
            }
          }

          dut.io.lanes.zipWithIndex.foreach { case (lane, index) =>
            if (bool(lane.cacheRequest.valid)) {
              val source = sourceOf(lane.cacheRob, lane.cacheRobFlag)
              assert(issued(source.key), "Cache accepted an instruction before its public issue")
              if (source.canceled && redirectSent) assert(cycle == redirectCycle, "Killed instruction was reissued")
              val ordinal = attempts.count(_.source == source)
              attempts += Attempt(source, index, cycle, uint(lane.cacheRequest.bits.id), ordinal)
              event(name, "attempt", s"\"rob\":${source.rob},\"lane\":$index,\"ordinal\":$ordinal")
            }
            if (bool(lane.replay.valid)) {
              val source = sourceOf(lane.replay.bits.uop.robIdx.value, lane.replay.bits.uop.robIdx.flag)
              identity(lane.replay.bits, source)
              held(index).foreach { case (old, slot) =>
                if (!redirectSent || cycle != redirectCycle) {
                  assert(source == old, "Blocked replay changed its owner")
                  lane.replay.bits.schedIndex.expect(slot.U)
                }
              }
              if (bool(lane.replayReady)) {
                queueAttempts += 1
                if (index != 2) moved += 1
                held(index) = None
                event(name, "queue-replay", s"\"rob\":${source.rob},\"lane\":$index,\"slot\":${uint(lane.replay.bits.schedIndex)}," +
                  s"\"fullva\":${if (enabled) uint(lane.replay.bits.fullva).toString else "null"},\"expected_va\":${source.address}")
              } else {
                heldCycles += 1; held(index) = Some(source -> uint(lane.replay.bits.schedIndex))
              }
            } else held(index) = None
            if (bool(lane.fastReplay.valid)) {
              val source = sourceOf(lane.fastReplay.bits.uop.robIdx.value, lane.fastReplay.bits.uop.robIdx.flag)
              identity(lane.fastReplay.bits, source)
              if (bool(lane.fastReady)) fastAttempts += 1
              else if (bool(lane.queueWrite.valid)) {
                lane.queueWrite.bits.uop.robIdx.value.expect(source.rob.U)
                assert(lane.queueWrite.bits.rep_info.cause.exists(bool), "Fast fallback lost its replay cause")
                fastFallbacks += 1
                event(name, "fast-fallback", s"\"rob\":${source.rob},\"lane\":$index")
              }
            }
            if (bool(lane.queueWrite.valid)) {
              val source = sourceOf(lane.queueWrite.bits.uop.robIdx.value, lane.queueWrite.bits.uop.robIdx.flag)
              identity(lane.queueWrite.bits, source)
            }
            if (bool(lane.permissionResponse.valid)) {
              val source = pendingPermissions(index).getOrElse(throw new AssertionError("Unowned permission response"))
              lane.permissionResponse.bits.request.address.expect(source.address.U)
              lane.permissionResponse.bits.request.tag.expect(permissionTag(source).U)
              lane.permissionResponse.bits.request.sourcePrivilege.expect(privilege.U)
              lane.permissionResponse.bits.request.sourceVirtual.expect(false.B)
              lane.permissionResponse.bits.outcome.expect((if (source.denied) 1 else 0).U)
              permissionResponses(source.key) += 1; pendingPermissions(index) = None
            }
            if (bool(lane.permissionRequest.valid)) {
              val source = sourceOf(lane.stage1.bits.uop.robIdx.value, lane.stage1.bits.uop.robIdx.flag)
              assert(pendingPermissions(index).isEmpty)
              lane.permissionRequest.bits.address.expect(source.address.U)
              lane.permissionRequest.bits.tag.expect(permissionTag(source).U)
              lane.permissionRequest.bits.sourcePrivilege.expect(privilege.U)
              lane.permissionRequest.bits.sourceVirtual.expect(false.B)
              lane.permissionRequest.bits.notTrusted.expect(true.B)
              lane.permissionRequest.bits.sizeLog2.expect(3.U)
              permissionRequests(source.key) += 1; pendingPermissions(index) = Some(source)
            }
            if (bool(lane.result.valid)) {
              val source = sourceOf(lane.result.bits.uop.robIdx.value, lane.result.bits.uop.robIdx.flag)
              assert(!source.canceled, "Canceled instruction produced a terminal result")
              assert(completed.add(source.key), "Instruction produced a second terminal result")
              val denied = enabled && source.denied
              lane.result.bits.uop.lqIdx.value.expect(source.lq.U)
              lane.result.bits.uop.pdest.expect((source.serial % 24 + 8).U)
              lane.result.bits.data.expect((if (denied) BigInt(0) else source.merged).U)
              lane.result.bits.uop.rfWen.expect((!denied && !source.fp).B)
              lane.result.bits.uop.fpWen.expect((!denied && source.fp).B)
              lane.result.bits.uop.exceptionVec.zipWithIndex.foreach { case (bit, number) =>
                bit.expect((denied && number == (if (privilege == 0) 24 else 25)).B)
              }
              if (denied) {
                lane.result.bits.uop.fdiException.get.tval.expect(source.address.U)
                lane.result.bits.uop.fdiException.get.reason.expect(2.U)
              }
              event(name, "terminal", s"\"rob\":${source.rob},\"denied\":$denied,\"data\":${uint(lane.result.bits.data)}")
            }
            if (bool(lane.stage2.valid) && bool(lane.blocked)) lane.fastWakeup.expect(false.B)
          }
          val mask = uint(dut.io.allocationMask)
          for (slot <- 0 until dut.LoadQueueReplaySize if mask.testBit(slot) && !previousMask.testBit(slot)) {
            if (!allocatedBefore.add(slot)) reusedSlots += slot
          }
          previousMask = mask
          if (finishedAt < 0 && completed.size == expectedCompletions && uint(dut.io.allocated) == 0 &&
              (!cancel || redirectSent)) finishedAt = cycle
          dut.clock.step(); cycle += 1
        }
        assert(finishedAt >= 0, s"$name timed out: issued=${issued.size}, completed=${completed.size}, queue=${uint(dut.io.allocated)}")
        assert(issued.size == count && completed.size == expectedCompletions)
        assert(queueAttempts > 0 && moved > 0, "No real queue-driven cross-lane retry occurred")
        if (fast) assert(fastFallbacks > 0, "Fast retry never fell back through the real queue")
        if (backpressure) assert(heldCycles > 0, "Replay never encountered public cache backpressure")
        if (reuse) assert(reusedSlots.nonEmpty, "No freed replay slot was reused")
        assert(permissionRequests == permissionResponses && pendingPermissions.forall(_.isEmpty))
        sources.filterNot(_.canceled).foreach { source =>
          val expectedAttempts = if (enabled && source.denied) 1 else source.retries + 1
          assert(attempts.count(_.source == source) == expectedAttempts,
            s"Unexpected attempt count for ${source.serial} in $name")
          assert(permissionRequests(source.key) == (if (enabled) expectedAttempts else 0))
        }
        dut.io.allocated.expect(0.U)
        dut.io.lanes.foreach { lane => lane.result.valid.expect(false.B); lane.replay.valid.expect(false.B) }
        completedCases += 1
        event(name, "complete", s"\"instructions\":$count,\"terminals\":${completed.size},\"queue_replays\":$queueAttempts," +
          s"\"migrations\":$moved,\"fast_replays\":$fastAttempts,\"fast_fallbacks\":$fastFallbacks,\"held_cycles\":$heldCycles,\"reused_slots\":${reusedSlots.size}")
      }

      // These services expose one retry cause at a time. They retain the original
      // production queue, pipeline arbitration and permission response ownership.
      def runService(name: String, kind: String, denied: Boolean = false,
        pageFault: Boolean = false): Unit = {
        initialize(0)
        val expectedCause = Map("rar" -> LoadReplayCauses.C_RAR,
          "raw" -> LoadReplayCauses.C_RAW, "ambiguity" -> LoadReplayCauses.C_MA,
          "mshr-nack" -> LoadReplayCauses.C_DR, "wpu" -> LoadReplayCauses.C_WF,
          "nuke" -> LoadReplayCauses.C_NK, "tlb" -> LoadReplayCauses.C_TM)(kind)
        val fastFallback = Set("mshr-nack", "wpu", "nuke")(kind)
        val waitForService = Set("rar", "raw", "ambiguity", "tlb")(kind)
        val address = if (denied) negative & ((BigInt(1) << dut.VAddrBits) - 1) else negative + 0x60
        val source = Source(0, 10, false, 2, address, 1, denied = denied)
        val begin = cycle
        val attempts = mutable.ArrayBuffer.empty[Attempt]
        val pendingPermissions = Array.fill(3)(false)
        var issued = false
        var firstQueueCycle = -1
        var terminalCycle = -1
        var terminalCount = 0
        var queueReplays = 0
        var retryWrites = 0
        var fastFallbacks = 0
        var permissionRequests = 0
        var permissionResponses = 0
        var wrongHintSent = false
        var matchingHintSent = false
        def identity(bundle: xiangshan.mem.Bundles.LsPipelineBundle): Unit = {
          bundle.uop.robIdx.value.expect(source.rob.U); bundle.uop.robIdx.flag.expect(false.B)
          bundle.uop.lqIdx.value.expect(source.lq.U)
          bundle.uop.sqIdx.value.expect(2.U)
          bundle.uop.pdest.expect(8.U)
          bundle.uop.fuOpType.expect(3.U)
          bundle.uop.fdiNotTrusted.foreach(_.expect(true.B))
          bundle.vaddr.expect((source.address & ((BigInt(1) << dut.VAddrBits) - 1)).U)
          if (enabled) {
            bundle.fullva.expect(source.address.U)
            bundle.fdiSourcePrivilege.get.expect(0.U)
            bundle.fdiSourceVirtual.get.expect(false.B)
          }
        }
        val uop = dut.io.lanes.head.request.bits.uop
        val permissionTag = (BigInt(source.rob) <<
          (uop.lqIdx.value.getWidth + 1 + uop.uopIdx.getWidth)) |
          (BigInt(source.lq) << uop.uopIdx.getWidth)
        while ((terminalCycle < 0 || cycle < terminalCycle + 35) && cycle < begin + 400) {
          val released = firstQueueCycle >= 0 && cycle >= firstQueueCycle + 12
          dut.io.rarFull.poke((kind == "rar" && !released).B)
          dut.io.rawFull.poke((kind == "raw" && !released).B)
          dut.io.sqEmpty.poke((kind != "ambiguity").B)
          dut.io.storeAddressReady.poke((kind != "ambiguity" || released).B)
          dut.io.tlbHint.valid.poke(false.B)
          if (kind == "tlb" && firstQueueCycle >= 0) {
            // Every request is assigned filter ID zero by the translation service.
            // An unrelated hint must not unblock it; the matching hint does.
            if (cycle == firstQueueCycle + 3 || cycle == firstQueueCycle + 12) {
              val matching = cycle == firstQueueCycle + 12
              dut.io.tlbHint.valid.poke(true.B)
              dut.io.tlbHint.bits.id.poke((if (matching) 0 else 1).U)
              dut.io.tlbHint.bits.replay_all.poke(false.B)
              if (matching) matchingHintSent = true else wrongHintSent = true
              event(name, "translation-hint", s"\"matching\":$matching")
            }
          }
          if (waitForService && firstQueueCycle >= 0 && !released) {
            assert(attempts.size == 1 && queueReplays == 0,
              s"$name escaped its blocking condition before the service became ready")
            dut.io.lanes.foreach(_.replay.valid.expect(false.B))
            dut.io.allocated.expect(1.U)
          }
          if (waitForService && firstQueueCycle >= 0 && cycle == firstQueueCycle + 12)
            event(name, "service-release", s"\"cause\":$expectedCause")

          dut.io.lanes.zipWithIndex.foreach { case (lane, index) =>
            lane.request.valid.poke(false.B)
            val firstS3 = attempts.exists(a => a.lane == index && a.ordinal == 0 && a.accepted + 3 == cycle)
            lane.cacheReady.poke((!fastFallback || !firstS3).B)
            lane.response.valid.poke(false.B); zero(lane.response.bits)
            lane.bankConflict.poke(false.B); lane.cacheNack.poke(false.B)
            lane.wpuFailure.poke(false.B)
            lane.rawReady.poke(true.B); lane.rarReady.poke(true.B)
            lane.tlbMiss.poke(false.B); lane.tlbPageFault.poke(false.B)
            lane.nuke.valid.poke(false.B); zero(lane.nuke.bits)
            Seq(lane.sq, lane.sbuffer).foreach { port =>
              zero(port.forwardMaskFast); zero(port.forwardMask); zero(port.forwardData)
              port.dataInvalid.poke(false.B); port.addrInvalid.poke(false.B)
              port.matchInvalid.poke(false.B)
            }
            lane.sq.dataInvalidFast.poke(false.B)
            zero(lane.sq.dataInvalidSqIdx); zero(lane.sq.addrInvalidSqIdx)
            attempts.find(a => a.lane == index && a.accepted + 1 == cycle).foreach { attempt =>
              if (kind == "tlb") {
                lane.tlbMiss.poke((attempt.ordinal == 0).B)
                lane.tlbPageFault.poke((attempt.ordinal > 0 && pageFault).B)
              }
              // Cache cancellation on miss/PF does not cancel the load owner.
              // It still reaches the queue or terminal exception below.
              attempt.killed = bool(lane.cacheS1Kill)
            }
            attempts.find(a => a.lane == index && a.accepted + 2 == cycle).foreach { attempt =>
              if (!attempt.killed) {
                lane.response.valid.poke(true.B); lane.response.bits.id.poke(attempt.id.U)
                lane.response.bits.data.poke((source.data | (source.data << 64)).U)
              }
              if (attempt.ordinal == 0) kind match {
                case "rar" => lane.rarReady.poke(false.B)
                case "raw" => lane.rawReady.poke(false.B)
                case "ambiguity" =>
                  lane.sq.addrInvalid.poke(true.B)
                  lane.sq.addrInvalidSqIdx.value.poke(2.U)
                case "mshr-nack" =>
                  lane.cacheNack.poke(true.B)
                  lane.response.bits.miss.poke(true.B)
                case "wpu" => lane.wpuFailure.poke(true.B)
                case "nuke" =>
                  lane.nuke.valid.poke(true.B)
                  lane.nuke.bits.robIdx.value.poke((source.rob - 1).U)
                  lane.nuke.bits.paddr.poke((source.address & ((BigInt(1) << dut.PAddrBits) - 1)).U)
                  lane.nuke.bits.mask.poke((BigInt(255) << (source.address.toInt & 15)).U)
                case "tlb" =>
              }
            }
            attempts.find(a => a.lane == index && a.accepted + 3 == cycle && !a.killed).foreach { _ =>
              lane.response.bits.data_delayed.poke((source.data | (source.data << 64)).U)
            }
          }
          if (!issued) {
            val request = dut.io.lanes(2).request
            zero(request.bits)
            request.bits.src(0).poke(source.address.U)
            request.bits.uop.robIdx.value.poke(source.rob.U)
            request.bits.uop.lqIdx.value.poke(source.lq.U)
            request.bits.uop.sqIdx.value.poke(2.U)
            request.bits.uop.pdest.poke(8.U)
            request.bits.uop.fuType.poke(FuType.ldu.U); request.bits.uop.fuOpType.poke(3.U)
            request.bits.uop.rfWen.poke(true.B)
            request.bits.uop.firstUop.poke(true.B); request.bits.uop.lastUop.poke(true.B)
            request.bits.uop.storeSetHit.poke((kind == "ambiguity").B)
            request.bits.uop.fdiNotTrusted.foreach(_.poke(true.B))
            request.bits.isFirstIssue.poke(true.B); request.valid.poke(true.B)
            if (bool(request.ready)) {
              issued = true
              event(name, "issue", s"\"rob\":${source.rob},\"fullva\":${source.address},\"cause\":$expectedCause")
            }
          }
          dut.io.lanes.zipWithIndex.foreach { case (lane, index) =>
            if (bool(lane.cacheRequest.valid)) {
              lane.cacheRob.expect(source.rob.U); lane.cacheRobFlag.expect(false.B)
              assert(issued && attempts.size < 2, s"$name accepted an unexpected extra attempt")
              attempts += Attempt(source, index, cycle, uint(lane.cacheRequest.bits.id), attempts.size)
              event(name, "attempt", s"\"rob\":${source.rob},\"lane\":$index,\"ordinal\":${attempts.size - 1}")
            }
            if (bool(lane.fastReplay.valid)) {
              identity(lane.fastReplay.bits)
              assert(fastFallback, s"$name unexpectedly selected the fast path")
              lane.fastReplay.bits.rep_info.cause(expectedCause).expect(true.B)
              lane.fastReady.expect(false.B)
              lane.queueWrite.valid.expect(true.B)
              fastFallbacks += 1
              event(name, "fast-fallback", s"\"cause\":$expectedCause,\"lane\":$index")
            }
            if (bool(lane.queueWrite.valid)) {
              identity(lane.queueWrite.bits)
              val causes = lane.queueWrite.bits.rep_info.cause.indices
                .filter(i => bool(lane.queueWrite.bits.rep_info.cause(i))).toSet
              if (causes.nonEmpty) {
                assert(causes == Set(expectedCause), s"$name selected $causes instead of $expectedCause")
                assert(firstQueueCycle < 0, "A service retry was enqueued twice")
                firstQueueCycle = cycle; retryWrites += 1
                event(name, "retry-enqueue", s"\"rob\":${source.rob},\"fullva\":${source.address},\"cause\":$expectedCause")
              }
            }
            if (bool(lane.replay.valid)) {
              identity(lane.replay.bits)
              if (bool(lane.replayReady)) {
                assert(!waitForService || released, "A blocked owner replayed before its wakeup")
                queueReplays += 1
                event(name, "queue-replay", s"\"rob\":${source.rob},\"lane\":$index," +
                  s"\"fullva\":${if (enabled) uint(lane.replay.bits.fullva).toString else "null"},\"expected_va\":${source.address}")
              }
            }
            if (bool(lane.permissionResponse.valid)) {
              assert(pendingPermissions(index), "Unowned permission response in service trace")
              lane.permissionResponse.bits.request.address.expect(source.address.U)
              lane.permissionResponse.bits.request.tag.expect(permissionTag.U)
              lane.permissionResponse.bits.outcome.expect((if (denied) 1 else 0).U)
              pendingPermissions(index) = false; permissionResponses += 1
            }
            if (bool(lane.permissionRequest.valid)) {
              assert(!pendingPermissions(index))
              identity(lane.stage1.bits)
              lane.permissionRequest.bits.address.expect(source.address.U)
              lane.permissionRequest.bits.tag.expect(permissionTag.U)
              pendingPermissions(index) = true; permissionRequests += 1
            }
            if (bool(lane.result.valid)) {
              val fdiDenied = enabled && denied
              val expectedExceptions = (if (fdiDenied) Set(24) else Set.empty[Int]) ++
                (if (pageFault) Set(13) else Set.empty[Int])
              lane.result.bits.uop.robIdx.value.expect(source.rob.U)
              lane.result.bits.uop.lqIdx.value.expect(source.lq.U)
              lane.result.bits.uop.pdest.expect(8.U)
              lane.result.bits.uop.exceptionVec.zipWithIndex.foreach { case (bit, number) =>
                bit.expect(expectedExceptions(number).B)
              }
              if (fdiDenied) {
                lane.result.bits.uop.rfWen.expect(false.B); lane.result.bits.uop.fpWen.expect(false.B)
                lane.result.bits.data.expect(0.U)
                lane.result.bits.uop.fdiException.get.tval.expect(source.address.U)
                lane.result.bits.uop.fdiException.get.reason.expect(2.U)
              } else if (!pageFault) {
                lane.result.bits.uop.rfWen.expect(true.B)
                lane.result.bits.data.expect(source.data.U)
              }
              terminalCount += 1
              assert(terminalCount == 1 && attempts.size == 2, s"$name completed before retry or more than once")
              terminalCycle = cycle
              event(name, "terminal", s"\"rob\":${source.rob},\"exceptions\":[${expectedExceptions.toSeq.sorted.mkString(",")}]")
            }
          }
          dut.clock.step(); cycle += 1
        }
        assert(terminalCount == 1 && retryWrites == 1 && queueReplays == 1 && attempts.size == 2,
          s"$name failed its exact retry lifecycle: terminal=$terminalCount enqueue=$retryWrites replay=$queueReplays attempts=${attempts.size}")
        assert(fastFallbacks == (if (fastFallback) 1 else 0))
        assert(permissionRequests == (if (enabled) 2 else 0) && permissionRequests == permissionResponses)
        assert(pendingPermissions.forall(!_))
        if (kind == "tlb") assert(wrongHintSent && matchingHintSent)
        dut.io.allocated.expect(0.U)
        dut.io.lanes.foreach { lane => lane.result.valid.expect(false.B); lane.replay.valid.expect(false.B) }
        completedCases += 1
        event(name, "complete", s"\"cause\":$expectedCause,\"attempts\":2,\"terminals\":1," +
          s"\"blocked_cycles\":${if (waitForService) 12 else 0},\"fast_fallbacks\":$fastFallbacks")
      }

      try {
        run("concurrent-hu-fullva", 9, 2, backpressure = true)
        run("concurrent-hs-fullva", 6, 3, privilege = 1, backpressure = true)
        run("fast-retry-queue-fallback", 6, 2, fast = true)
        run("long-forward-retry", 4, 20, backpressure = true)
        run("selective-redirect-wrap", 4, 8, cancel = true, wrap = true)
        run("queue-slot-reuse", dut.LoadQueueReplaySize + 9, 1, reuse = true)
        Seq("rar", "raw", "ambiguity", "mshr-nack", "wpu", "nuke").foreach { kind =>
          runService(s"service-$kind-retry", kind)
        }
        runService("service-tlb-allow", "tlb")
        runService("service-tlb-deny", "tlb", denied = true)
        runService("service-tlb-page-fault", "tlb", pageFault = true)
        runService("service-tlb-deny-page-fault", "tlb", denied = true, pageFault = true)
        println(s"L02_REPLAY_PASS enabled=$enabled cases=$completedCases cycles=$cycle actual_lanes=3 actual_replay=true")
      } finally {
        Files.write(root.resolve("load-replay-events.jsonl"), (events.mkString("\n") + "\n").getBytes(StandardCharsets.UTF_8))
      }
    }
  }
}
