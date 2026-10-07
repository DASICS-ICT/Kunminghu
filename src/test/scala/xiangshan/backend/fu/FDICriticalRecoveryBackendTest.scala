// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.PeekPokeAPI._
import chisel3.simulator.{ChiselSimulation, ChiselWorkspace}
import io.circe.Json
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import svsim.{CommonCompilationSettings, Workspace}
import svsim.verilator.Backend
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.SqPtr
import xiangshan.cache.MemoryOpConstants
import xiangshan.frontend.{BranchPredictionBundle, BrType, FtqPtr}

class FDICriticalRecoveryBackendTest extends AnyFlatSpec {
  behavior of "Owned critical Debug recovery through the real Backend"

  private val threshold = (1 << 21) - 1
  private val scratchWrite = BigInt("04061073", 16)
  private val directJump = BigInt("008000ef", 16)
  private case class Ptr(flag: Boolean, value: Int)
  private case class Fetch(pointer: Ptr, offset: Int)
  private def num(x: BigInt): Json = Json.fromBigInt(x)
  private def num(x: Int): Json = Json.fromInt(x)
  private def flag(x: Boolean): Json = Json.fromBoolean(x)
  private def str(x: String): Json = Json.fromString(x)

  private class Source(val id: Int, val pc: BigInt, val instruction: BigInt, val fetch: Fetch) {
    var accepted = -1
    var acceptedMode = -1
    var acceptedVirtual = false
    var allocated = -1
    var rob: Option[Ptr] = None
    var retired = -1
    var canceled = -1
    var renamed = -1
    var removedBeforeAllocation = -1
    var csrAccepts = 0
    var csrEffects = 0
    var storeIssues = 0
    var storeCompletions = 0
    var storeDataIssues = 0
    var storeS0Fires = 0
    var storeS1Fires = 0
    var storeS2Fires = 0
    var annulledStoreIssues = 0
    var annulledStoreDataIssues = 0
    var annulledTranslations = 0
    var annulledWalks = 0
    var queueCanceled = -1
    var queueReleased = -1
    var sq: Option[Ptr] = None
    var buffered = -1
    var cacheOffered = -1
    var cacheWritten = -1
    var cacheAcknowledged = -1
    var waitForward = false
    var blockBackward = false
    val jumpInputs = mutable.ArrayBuffer.empty[(Int, Int, Boolean, Boolean)]
    val jumpOutputs = mutable.ArrayBuffer.empty[Int]
    val annulledJumpComputations = mutable.ArrayBuffer.empty[Int]
    val robWritebackArrivals = mutable.ArrayBuffer.empty[Int]
    val annulledRobWritebackArrivals = mutable.ArrayBuffer.empty[Int]
    val recoveryTargets = mutable.ArrayBuffer.empty[(Int, BigInt)]
    def json: Json = Json.obj("id" -> num(id), "pc" -> num(pc), "instruction" -> num(instruction),
      "accepted" -> num(accepted), "accepted_mode" -> num(acceptedMode),
      "accepted_virtual" -> flag(acceptedVirtual), "allocated" -> num(allocated),
      "retired" -> num(retired), "canceled" -> num(canceled), "renamed" -> num(renamed),
      "removed_before_allocation" -> num(removedBeforeAllocation), "csr_accepts" -> num(csrAccepts),
      "csr_effects" -> num(csrEffects), "store_issues" -> num(storeIssues),
      "store_completions" -> num(storeCompletions), "store_data_issues" -> num(storeDataIssues),
      "store_s0_fires" -> num(storeS0Fires), "store_s1_fires" -> num(storeS1Fires), "store_s2_fires" -> num(storeS2Fires),
      "annulled_store_inputs" -> num(annulledStoreIssues), "annulled_store_data_inputs" -> num(annulledStoreDataIssues),
      "annulled_translation_requests" -> num(annulledTranslations), "annulled_walk_requests" -> num(annulledWalks),
      "queue_cancel_cycle" -> num(queueCanceled), "queue_release_cycle" -> num(queueReleased),
      "buffered_cycle" -> num(buffered), "cache_offer_cycle" -> num(cacheOffered),
      "cache_write_cycle" -> num(cacheWritten), "cache_ack_cycle" -> num(cacheAcknowledged),
      "wait_forward" -> flag(waitForward), "block_backward" -> flag(blockBackward),
      "jump_inputs" -> Json.arr(jumpInputs.map { case (cycle, mode, virtual, debug) =>
        Json.obj("cycle" -> num(cycle), "mode" -> num(mode), "virtual" -> flag(virtual), "debug" -> flag(debug))
      }.toSeq: _*), "jump_output_cycles" -> Json.arr(jumpOutputs.map(num).toSeq: _*),
      "annulled_jump_compute_cycles" -> Json.arr(annulledJumpComputations.map(num).toSeq: _*),
      "rob_writeback_arrival_cycles" -> Json.arr(robWritebackArrivals.map(num).toSeq: _*),
      "annulled_rob_writeback_arrival_cycles" -> Json.arr(annulledRobWritebackArrivals.map(num).toSeq: _*),
      "recovery_targets" -> Json.arr(recoveryTargets.map { case (cycle, target) =>
        Json.obj("cycle" -> num(cycle), "target" -> num(target))
      }.toSeq: _*))
  }
  private case class Packet(base: BigInt, pointer: Ptr, sources: Seq[Source])

  private class Driver(val dut: FDICriticalRecoveryBackendHarness, path: Path) {
    val events = Files.newBufferedWriter(path.resolve("critical-events.jsonl"), StandardCharsets.UTF_8)
    val batches = Files.newBufferedWriter(path.resolve("idle-batches.jsonl"), StandardCharsets.UTF_8)
    val sources = mutable.ArrayBuffer.empty[Source]
    val byFetch = mutable.Map.empty[Fetch, Source]
    // Each driver owns one reset-delimited program. No fresh instructions enter
    // after critical recovery; canceled owners remain tracked until the drain
    // completes, and the next program starts with a new driver and DUT reset.
    val byRob = mutable.Map.empty[Ptr, Source]
    val fetched = mutable.Set.empty[Ptr]
    val redirects = mutable.Set.empty[Fetch]
    val frontendRedirects = mutable.Map.empty[Fetch, BigInt]
    val ifuRedirects = mutable.Map.empty[Fetch, BigInt]
    val acceptedCritical = mutable.ArrayBuffer.empty[(Int, Source)]
    val debugEffects = mutable.ArrayBuffer.empty[Int]
    val storePlan = mutable.Map.empty[BigInt, (BigInt, BigInt)]
    val bySq = mutable.Map.empty[Ptr, Source]
    val metadataReplies = Seq.fill(dut.memory.metadata.length)(mutable.Queue.empty[Int])
    val cacheReplies = mutable.Queue.empty[(Int, BigInt, Seq[Int])]
    val ptwPending = mutable.Map.empty[BigInt, (Int, BigInt)]
    val ptwResponded = mutable.Set.empty[BigInt]
    // The nonblocking TLB request register supplies the next cycle's PTW owner.
    val translationRequests = mutable.Map.empty[Int, (Int, Source, Boolean)]
    val actualBytes = mutable.Map.empty[BigInt, Int]
    var heldOffer: Option[(BigInt, BigInt, BigInt, BigInt)] = None
    var heldOldSnapshot: Option[(BigInt, BigInt, BigInt, BigInt)] = None
    var allowWaitingPtw = false
    var ptwResponses = 0
    var criticalExpected = false
    var expectedSynchronousTrap = false
    val baseAddress = BigInt("80000040", 16)
    val waitingAddress = baseAddress + 0x10000
    val youngAddress = baseAddress + 0x20000
    val legalWrites = mutable.ArrayBuffer.empty[Int]
    val lanes = dut.io.instruction +: dut.io.instructionTail.toSeq
    var cycle = 0
    var phase = "setup"
    var ordinaryPc = BigInt(0x1000)
    var lastState = Json.Null
    var overflow = -1
    var criticalCycle = -1
    var modeChange = -1
    var debugEffect = -1
    var naturalTrialExecuted = false

    var diagnostic = Seq.empty[Source]
    var lastOutcome = "DRIVER_OR_PROTOCOL_FAILURE"

    def bool(x: Bool): Boolean = x.peek().litToBoolean
    def uint(x: UInt): BigInt = x.peek().litValue
    def ptr(x: FtqPtr): Ptr = Ptr(bool(x.flag), uint(x.value).toInt)
    def ptr(x: SqPtr): Ptr = Ptr(bool(x.flag), uint(x.value).toInt)
    def ptr(x: RobPtr): Ptr = Ptr(bool(x.flag), uint(x.value).toInt)
    def fetch(x: FtqPtr, offset: UInt): Fetch = Fetch(ptr(x), uint(offset).toInt)
    def zero(x: Data): Unit = x match {
      case b: Bool => b.poke(false.B)
      case u: UInt => u.poke(0.U)
      case s: SInt => s.poke(0.S)
      case v: Vec[_] => v.foreach(zero)
      case r: Record => r.elements.values.foreach(zero)
      case other => throw new IllegalArgumentException(s"Unsupported input ${other.getClass.getName}")
    }
    def zeroInputs(x: Data): Unit = x match {
      case v: Vec[_] => v.foreach(zeroInputs)
      case r: Record => r.elements.values.foreach(zeroInputs)
      case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => zero(leaf)
      case _ =>
    }
    def setPtr(x: FtqPtr, value: Ptr): Unit = {
      x.flag.poke(value.flag.B); x.value.poke(value.value.U)
    }
    def record(kind: String, fields: (String, Json)*): Unit = {
      val value = Json.fromFields(Seq("cycle" -> num(cycle), "phase" -> str(phase), "event" -> str(kind)) ++ fields)
      events.write(value.noSpaces); events.newLine(); events.flush()
    }
    def state: Json = Json.obj(
      "mode" -> num(uint(dut.io.mode)), "virtual" -> flag(bool(dut.io.virtualMode)),
      "debug" -> flag(bool(dut.io.debugMode)), "source_mode" -> num(uint(dut.monitor.sourcePrivilege)),
      "source_virtual" -> flag(bool(dut.monitor.sourceVirtual)),
      "rob_empty" -> flag(bool(dut.monitor.robEmpty)), "wait_forward" -> flag(bool(dut.monitor.waitForward)),
      "block_backward" -> flag(bool(dut.monitor.blockBackward)),
      "hu_outstanding" -> flag(bool(dut.monitor.huOutstanding)), "hu_deferred" -> flag(bool(dut.io.criticalDebugDeferred)),
      "has_trap" -> flag(bool(dut.monitor.hasTrap)), "overflow" -> flag(bool(dut.io.watchdogOverflow)),
      "rob_error" -> flag(bool(dut.io.robWatchdogError)), "control_error" -> flag(bool(dut.io.controlWatchdogError)),
      "csr_critical_input" -> flag(bool(dut.io.csrCriticalInput)), "critical" -> flag(bool(dut.io.criticalErrorState)),
      "debug_effect" -> flag(bool(dut.io.debugEntry)), "critical_debug" -> flag(bool(dut.monitor.criticalDebug)),
      "csr_target_update" -> flag(bool(dut.monitor.targetUpdate)), "csr_target" -> num(uint(dut.monitor.target)))
    def sourceFields(s: Source): Seq[(String, Json)] = Seq("source" -> num(s.id), "pc" -> num(s.pc),
      "ftq_flag" -> flag(s.fetch.pointer.flag), "ftq" -> num(s.fetch.pointer.value), "offset" -> num(s.fetch.offset))

    def memoryOwner(rob: RobPtr, sq: SqPtr): Source = {
      val source = byRob.getOrElse(ptr(rob), throw new AssertionError("A Store has no actual ROB allocation"))
      assert(storePlan.contains(source.pc), "An unplanned source reached the native Store engine")
      val identity = ptr(sq)
      assert(source.sq.isEmpty || source.sq.contains(identity), "A Store retry changed its full SQ identity")
      source.sq = Some(identity); bySq(identity) = source
      source
    }
    def killedBy(owner: Ptr, valid: Bool, redirect: Redirect): Boolean = {
      if (!bool(valid)) false
      else {
        val target = ptr(redirect.robIdx)
        val size = dut.RobSize
        val distance = (owner.value + (if (owner.flag) size else 0) - target.value -
          (if (target.flag) size else 0) + 2 * size) % (2 * size)
        (distance > 0 && distance < size) || (distance == 0 && uint(redirect.level) == 1)
      }
    }
    def serviceMemory(): Unit = {
      val previousTranslations = translationRequests.toMap
      dut.memory.ptw.resp.valid.poke(false.B)
      ptwPending.toSeq.sortBy(_._1).find { case (vpn, (due, _)) =>
        due <= cycle && (vpn == (baseAddress >> 12) || allowWaitingPtw)
      }.foreach { case (vpn, (_, sq)) =>
        assert(!ptwResponded(vpn), "A translation response was delivered more than once")
        val response = dut.memory.ptw.resp.bits
        zero(response)
        response.memidx.is_st.poke(true.B); response.memidx.idx.poke(sq.U)
        val sector = response.s1.pteidx.length
        val index = (vpn % sector).toInt
        response.s2xlate.poke(0.U)
        response.s1.entry.tag.poke((vpn / sector).U)
        response.s1.entry.level.foreach(_.poke(0.U))
        response.s1.entry.ppn.poke((vpn / sector).U)
        response.s1.entry.v.poke(true.B)
        response.s1.entry.perm.foreach { perm =>
          perm.r.poke(true.B); perm.w.poke(true.B); perm.a.poke(true.B)
          perm.d.poke(true.B); perm.u.poke(true.B)
        }
        response.s1.addr_low.poke(index.U)
        response.s1.valididx.zipWithIndex.foreach { case (bit, i) => bit.poke((i == index).B) }
        response.s1.pteidx.zipWithIndex.foreach { case (bit, i) => bit.poke((i == index).B) }
        response.s1.ppn_low.zipWithIndex.foreach { case (ppn, i) => ppn.poke((if (i == index) vpn % sector else BigInt(0)).U) }
        dut.memory.ptw.resp.valid.poke(true.B); dut.memory.ptw.resp.ready.expect(true.B)
        ptwPending.remove(vpn); ptwResponded += vpn; ptwResponses += 1
        record("ptw-response", "vpn" -> num(vpn), "sq" -> num(sq))
      }
      for ((port, lane) <- dut.memory.metadata.zipWithIndex) {
        port.resp.valid.poke(metadataReplies(lane).headOption.contains(cycle).B)
        if (metadataReplies(lane).headOption.contains(cycle)) metadataReplies(lane).dequeue()
      }
      dut.memory.cacheWrite.main_pipe_hit_resp.valid.poke(false.B)
      if (cacheReplies.headOption.exists(_._1 == cycle)) {
        val (_, id, owners) = cacheReplies.dequeue()
        dut.memory.cacheWrite.main_pipe_hit_resp.valid.poke(true.B)
        dut.memory.cacheWrite.main_pipe_hit_resp.bits.id.poke(id.U)
        owners.foreach { identity =>
          val source = sources(identity)
          assert(source.cacheWritten >= 0 && source.cacheAcknowledged < 0)
          source.cacheAcknowledged = cycle
        }
        record("cache-ack", "id" -> num(id), "sources" -> Json.arr(owners.map(num): _*))
      }
      for ((issue, lane) <- dut.memory.issue.zipWithIndex if bool(issue.valid)) {
        val source = memoryOwner(issue.bits.uop.robIdx, issue.bits.uop.sqIdx)
        assert(source.retired < 0, "A retired Store arrived at STA again")
        val storePhase = dut.memory.phases(lane)
        val nativeKill = killedBy(source.rob.get, storePhase.redirect.valid, storePhase.redirect.bits)
        if (source.canceled >= 0) {
          assert(nativeKill, "A canceled STA input has no same-cycle native memory redirect")
          storePhase.s0Kill.expect(true.B); storePhase.s0Fire.expect(false.B)
          source.annulledStoreIssues += 1
        }
        if (bool(storePhase.s0Fire)) {
          assert(source.canceled < 0)
          source.storeS0Fires += 1
        }
        assert(source.fetch == fetch(issue.bits.uop.ftqPtr, issue.bits.uop.ftqOffset))
        dut.memory.sourcePrivilege.expect(source.acceptedMode.U)
        dut.memory.sourceVirtual.expect(source.acceptedVirtual.B)
        dut.io.debugMode.expect(false.B)
        issue.bits.src(0).expect(storePlan(source.pc)._1.U)
        issue.bits.uop.fuOpType.expect(3.U)
        issue.bits.uop.fdiNotTrusted.foreach(_.expect(false.B))
        source.storeIssues += 1
        if (source.storeIssues <= 2 || phase != "natural-watchdog-wait")
          record("store-address-input", (sourceFields(source) ++ Seq("lane" -> num(lane), "sq" -> num(source.sq.get.value),
            "s0_fire" -> flag(bool(storePhase.s0Fire)), "native_memory_kill" -> flag(nativeKill))): _*)
      }
      for ((issue, lane) <- dut.memory.dataIssue.zipWithIndex if bool(issue.valid)) {
        val source = memoryOwner(issue.bits.uop.robIdx, issue.bits.uop.sqIdx)
        assert(source.retired < 0 && source.storeDataIssues == 0)
        val redirect = dut.memory.phases.head.redirect
        val nativeKill = killedBy(source.rob.get, redirect.valid, redirect.bits)
        if (source.canceled >= 0) {
          assert(nativeKill, "A canceled STD input has no same-cycle native memory redirect")
          source.annulledStoreDataIssues += 1
        }
        dut.memory.sourcePrivilege.expect(source.acceptedMode.U)
        dut.memory.sourceVirtual.expect(source.acceptedVirtual.B)
        dut.io.debugMode.expect(false.B)
        issue.bits.src(0).expect(storePlan(source.pc)._2.U)
        source.storeDataIssues += 1
        record("store-data-input", (sourceFields(source) ++ Seq("lane" -> num(lane),
          "native_memory_kill" -> flag(nativeKill))): _*)
      }
      for ((phase, lane) <- dut.memory.phases.zipWithIndex) {
        for ((valid, fire, kill, rob, sq, stage) <- Seq(
          (phase.s1Valid, phase.s1Fire, phase.s1Kill, phase.s1Rob, phase.s1Sq, 1),
          (phase.s2Valid, phase.s2Fire, phase.s2Kill, phase.s2Rob, phase.s2Sq, 2)) if bool(valid)) {
          val source = memoryOwner(rob, sq)
          if (source.canceled >= 0) {
            assert(killedBy(source.rob.get, phase.redirect.valid, phase.redirect.bits),
              "A canceled Store stage has no matching native memory redirect")
            kill.expect(true.B); fire.expect(false.B)
          }
          if (bool(fire)) {
            assert(source.canceled < 0 && source.retired < 0)
            if (stage == 1) source.storeS1Fires += 1 else source.storeS2Fires += 1
            record("store-stage-advance", (sourceFields(source) ++ Seq("lane" -> num(lane), "stage" -> num(stage))): _*)
          }
        }
        if (bool(phase.translation.valid)) {
          val request = phase.translation.bits
          val source = byRob.getOrElse(ptr(request.debug.robIdx), throw new AssertionError("TLB request lost its source ROB"))
          assert(source.sq.exists(_.value == uint(request.memidx.idx).toInt))
          request.fullva.expect(storePlan(source.pc)._1.U)
          dut.memory.sourcePrivilege.expect(source.acceptedMode.U)
          dut.memory.sourceVirtual.expect(source.acceptedVirtual.B)
          dut.io.debugMode.expect(false.B)
          val annulled = source.canceled >= 0
          if (annulled) {
            assert(killedBy(source.rob.get, phase.redirect.valid, phase.redirect.bits))
            phase.s0Kill.expect(true.B); phase.s0Fire.expect(false.B)
            source.annulledTranslations += 1
            record("annulled-translation-request", (sourceFields(source) :+ ("lane" -> num(lane))): _*)
          }
          translationRequests(lane) = ((cycle, source, annulled))
        }
      }
      for (completion <- dut.memory.completion if bool(completion.valid)) {
        val source = memoryOwner(completion.bits.uop.robIdx, completion.bits.uop.sqIdx)
        assert(source.canceled < 0 && source.storeCompletions == 0)
        completion.bits.uop.exceptionVec.foreach(_.expect(false.B))
        source.storeCompletions += 1
        record("store-completion", sourceFields(source): _*)
      }
      for ((port, lane) <- dut.memory.metadata.zipWithIndex if bool(port.req.valid) && bool(port.req.ready)) {
        dut.memory.issue(lane).valid.expect(true.B)
        dut.memory.phases(lane).s0Fire.expect(true.B)
        val source = memoryOwner(dut.memory.issue(lane).bits.uop.robIdx, dut.memory.issue(lane).bits.uop.sqIdx)
        assert(source.canceled < 0, "A canceled Store made an effective metadata request")
        port.req.bits.vaddr.expect(storePlan(source.pc)._1.U)
        port.req.bits.cmd.expect(MemoryOpConstants.M_PFW)
        if (storePlan(source.pc)._1 != waitingAddress || allowWaitingPtw) metadataReplies(lane).enqueue(cycle + 2)
      }
      for ((request, lane) <- dut.memory.ptw.req.zipWithIndex if bool(request.valid) && bool(request.ready)) {
        request.bits.memidx.is_st.expect(true.B); request.bits.s2xlate.expect(0.U)
        val vpn = uint(request.bits.vpn)
        val sq = uint(request.bits.memidx.idx)
        val source = bySq.values.find(s => s.sq.exists(_.value == sq.toInt) && (storePlan(s.pc)._1 >> 12) == vpn).getOrElse(
          throw new AssertionError("PTW request has no issued SQ owner"))
        if (source.canceled >= 0) {
          // Native StoreUnit emits a TLB lookup even when S0 is killed. A walk
          // from exactly that registered lookup is speculative service traffic,
          // never permission to advance the canceled Store or publish its data.
          assert(previousTranslations.get(lane).exists { case (accepted, owner, annulled) =>
            accepted + 1 == cycle && (owner eq source) && annulled
          }, "Canceled PTW request has no exact preceding annulled TLB transaction")
          source.annulledWalks += 1
          record("annulled-page-walk-request", (sourceFields(source) ++ Seq("lane" -> num(lane), "vpn" -> num(vpn), "sq" -> num(sq))): _*)
        } else {
          assert(Set(baseAddress >> 12, waitingAddress >> 12).contains(vpn), "An unplanned effective Store requested translation")
          if (!ptwResponded(vpn) && !ptwPending.contains(vpn)) {
            ptwPending(vpn) = ((cycle + 12, sq))
            record("ptw-request", (sourceFields(source) ++ Seq("vpn" -> num(vpn), "sq" -> num(sq))): _*)
          }
        }
      }
      for (source <- sources if source.canceled >= 0 && source.sq.nonEmpty) {
        val slot = dut.memory.queueSlots(source.sq.get.value)
        val sameOwner = bool(slot.allocated) && source.rob.contains(ptr(slot.rob))
        if (sameOwner) {
          slot.committed.expect(false.B)
          if (bool(slot.cancel) && source.queueCanceled < 0) {
            source.queueCanceled = cycle
            record("store-queue-cancel", sourceFields(source): _*)
          }
        } else if (source.queueCanceled >= 0 && source.queueReleased < 0) {
          source.queueReleased = cycle
          record("store-queue-released", sourceFields(source): _*)
        }
      }
      for (publication <- dut.memory.publication if bool(publication.fire)) {
        val source = bySq.getOrElse(ptr(publication.sq), throw new AssertionError("SQ buffer transfer lost its original owner"))
        assert(source.canceled < 0 && source.buffered < 0 && source.storeCompletions == 1)
        // Native SQ commitment permits internal buffering at the completed head
        // before the final ROB retirement pulse. External cache publication below
        // still requires the real retirement of every contributing Store.
        val (address, data) = storePlan(source.pc)
        val line = uint(publication.bits.addr)
        val shift = (address - line).toInt
        assert(shift >= 0 && shift + 8 <= publication.bits.mask.getWidth)
        publication.bits.mask.expect((BigInt(255) << shift).U)
        assert(((uint(publication.bits.data) >> (8 * shift)) & ((BigInt(1) << 64) - 1)) == data)
        publication.bits.vecValid.expect(true.B)
        source.buffered = cycle
        record("store-buffer-transfer", (sourceFields(source) :+ ("retired_cycle" -> num(source.retired))): _*)
      }
      val request = dut.memory.cacheWrite.req
      if (bool(request.valid)) {
        val line = uint(request.bits.addr)
        val contributing = sources.filter { source =>
          source.buffered >= 0 && storePlan.get(source.pc).exists { case (address, _) => address / 64 == line / 64 }
        }
        contributing.foreach { source =>
          assert(source.retired >= 0 || dut.io.robRetire.exists(p => bool(p.valid) && source.rob.contains(ptr(p.bits))),
            "A cache Store offer preceded its architectural retirement")
          assert(source.canceled < 0)
        }
        val expected = contributing.flatMap { source =>
          val (address, data) = storePlan(source.pc)
          (0 until 8).map(i => (address + i) -> ((data >> (8 * i)) & 255).toInt)
        }.toMap
        assert(expected.nonEmpty && line % 64 == 0)
        val mask = expected.keys.foldLeft(BigInt(0))((m, byte) => m.setBit((byte - line).toInt))
        request.bits.cmd.expect(MemoryOpConstants.M_XWR); request.bits.mask.expect(mask.U)
        expected.foreach { case (byte, value) => assert(((uint(request.bits.data) >> (8 * (byte - line).toInt)) & 255).toInt == value) }
        val dataMask = (0 until request.bits.mask.getWidth).filter(mask.testBit)
          .foldLeft(BigInt(0))((value, byte) => value | (BigInt(255) << (8 * byte)))
        val offer = (line, uint(request.bits.id), mask, uint(request.bits.data) & dataMask)
        heldOffer.foreach(previous => assert(previous == offer, "Backpressure changed the accepted Store effect"))
        if (heldOffer.isEmpty) {
          contributing.foreach { source =>
            assert(source.cacheOffered < 0, "A Store exposed a duplicate external cache offer")
            source.cacheOffered = cycle
          }
          record("cache-offer", "address" -> num(line), "id" -> num(offer._2), "mask" -> num(mask),
            "sources" -> Json.arr(contributing.map(source => num(source.id)).toSeq: _*))
        }
        heldOffer = Some(offer)
        if (bool(request.ready)) {
          contributing.foreach { source =>
            assert(source.cacheWritten < 0)
            source.cacheWritten = cycle
          }
          expected.foreach { case (byte, value) =>
            assert(!actualBytes.contains(byte), "An external Store effect was duplicated")
            actualBytes(byte) = value
          }
          val owners = contributing.map(_.id).toSeq
          cacheReplies.enqueue((cycle + 3, offer._2, owners)); heldOffer = None
          record("cache-write", "address" -> num(line), "id" -> num(offer._2), "mask" -> num(mask),
            "sources" -> Json.arr(owners.map(num): _*))
        }
      } else assert(heldOffer.isEmpty, "An unaccepted cache offer disappeared")
    }

    def edge(): Unit = {
      val current = state
      if (current != lastState) { record("state", "signals" -> current); lastState = current }
      if (bool(dut.io.accepted.valid) && bool(dut.io.accepted.bits.interrupt.criticalDebug)) {
        val e = dut.io.accepted.bits
        val owner = byRob.getOrElse(ptr(e.robIdx), throw new AssertionError("Critical claim lacks real ROB allocation"))
        assert(owner.fetch == fetch(e.ftqIdx, e.ftqOffset), "Critical claim changed the allocated FTQ identity")
        assert(owner.retired < 0 && owner.canceled < 0)
        dut.io.robHeadInterruptSafe.expect(true.B)
        dut.monitor.waitForward.expect(false.B)
        acceptedCritical += ((cycle, owner))
        record("critical-claim", sourceFields(owner): _*)
      }
      if (criticalExpected) {
        if (overflow < 0 && bool(dut.io.watchdogOverflow)) overflow = cycle
        if (criticalCycle < 0 && bool(dut.io.criticalErrorState)) criticalCycle = cycle
        if (modeChange < 0 && bool(dut.io.debugMode)) modeChange = cycle
        if (bool(dut.io.debugEntry)) {
          debugEffects += cycle
          if (debugEffect < 0) debugEffect = cycle
          assert(acceptedCritical.size == 1, "An asynchronous Debug effect requires exactly one accepted critical owner")
          record("owned-debug-effect", sourceFields(acceptedCritical.head._2): _*)
        }
      }
      serviceMemory()
      if (bool(dut.io.bpu.resp.valid) && bool(dut.io.bpu.resp.ready)) {
        assert(!bool(dut.io.backendRedirect.valid) && !bool(dut.io.frontendRedirect.valid),
          "A surviving BPU response must follow the actual FTQ redirect consumption")
        record("bpu-response-accepted", "base_pc" -> num(uint(dut.io.bpu.resp.bits.s1.pc(3))),
          "ftq_flag" -> flag(ptr(dut.io.ftqNext).flag), "ftq" -> num(ptr(dut.io.ftqNext).value))
      }
      if (bool(dut.io.fetchRequest.valid) && bool(dut.io.fetchReady)) {
        val pointer = ptr(dut.io.fetchRequest.bits.ftqIdx)
        fetched += pointer
        record("ftq-fetch-accepted", "base_pc" -> num(uint(dut.io.fetchRequest.bits.startAddr)),
          "ftq_flag" -> flag(pointer.flag), "ftq" -> num(pointer.value))
      }
      dut.monitor.renamed.foreach { port =>
        if (bool(port.valid)) {
          val value = port.bits
          val s = byFetch.getOrElse(fetch(value.ftqIdx, value.ftqOffset),
            throw new AssertionError("Rename source has no planned frontend transaction"))
          value.instr.expect(s.instruction.U); value.pc.expect(s.pc.U); value.tag.expect(false.B)
          if (s.renamed < 0) s.renamed = cycle
          s.waitForward = bool(value.waitForward); s.blockBackward = bool(value.blockBackward)
          record("rename", (sourceFields(s) ++ Seq("rob_flag" -> flag(ptr(value.robIdx).flag),
            "rob" -> num(ptr(value.robIdx).value), "tag" -> flag(bool(value.tag)),
            "pdest" -> num(uint(value.pdest)), "operation" -> num(uint(value.operation)),
            "wait_forward" -> flag(s.waitForward), "block_backward" -> flag(s.blockBackward))): _*)
        }
      }
      dut.io.robEnq.zipWithIndex.foreach { case (port, lane) =>
        if (bool(dut.monitor.robAllocation(lane))) {
          port.valid.expect(true.B); port.bits.first.expect(true.B)
          val s = byFetch.getOrElse(fetch(port.bits.ftqIdx, port.bits.ftqOffset),
            throw new AssertionError("ROB allocation has no original frontend transaction"))
          val identity = ptr(port.bits.robIdx)
          assert(s.allocated < 0 && s.rob.isEmpty, "A source acquired a second ROB allocation")
          s.allocated = cycle
          s.rob = Some(identity); byRob(identity) = s
          record("allocate", (sourceFields(s) ++ Seq("rob_flag" -> flag(s.rob.get.flag), "rob" -> num(s.rob.get.value))): _*)
        }
      }
      if (bool(dut.monitor.redirect.valid)) {
        val r = dut.monitor.redirect.bits
        val index = ptr(r.robIdx)
        val size = dut.RobSize
        val raw = index.value + (if (index.flag) size else 0)
        val self = uint(r.level) == 1
        sources.filter(s => s.allocated >= 0 && s.retired < 0 && s.canceled < 0).foreach { s =>
          val x = s.rob.get
          val distance = (x.value + (if (x.flag) size else 0) - raw + 2 * size) % (2 * size)
          if ((distance > 0 && distance < size) || (distance == 0 && self)) {
            s.canceled = cycle; record("cancel", sourceFields(s): _*)
          }
        }
        dut.monitor.dispatchPending.zipWithIndex.foreach { case (port, i) =>
          if (bool(port.valid)) {
            val source = byFetch(fetch(port.bits.ftqIdx, port.bits.ftqOffset))
            if (source.allocated < 0) {
              val x = ptr(port.bits.robIdx)
              val distance = (x.value + (if (x.flag) size else 0) - raw + 2 * size) % (2 * size)
              if ((distance > 0 && distance < size) || (distance == 0 && self)) {
                assert(!bool(dut.monitor.dispatchReady(i)), "A canceled pending source was accepted concurrently")
                source.removedBeforeAllocation = cycle
                record("rename-only-cancel", sourceFields(source): _*)
              }
            }
          }
        }
        val redirectSource = byRob.get(index)
        if (bool(dut.monitor.frontendCancel)) redirectSource.foreach { owner =>
          sources.filter(s => s.accepted >= 0 && s.allocated < 0 && s.removedBeforeAllocation < 0 && s.id > owner.id)
            .foreach { source =>
              source.removedBeforeAllocation = cycle
              record("frontend-only-cancel", sourceFields(source): _*)
            }
        }
        record("internal-redirect", "rob_flag" -> flag(index.flag), "rob" -> num(index.value),
          "flush_self" -> flag(self), "target" -> num(uint(r.cfiUpdate.target)))
      }
      dut.monitor.jumps.zipWithIndex.foreach { case (port, i) =>
        if (bool(port.inValid) || bool(port.outValid)) {
          val s = byFetch.getOrElse(fetch(port.ftqIdx, port.ftqOffset),
            throw new AssertionError("Jump has no original frontend transaction"))
          assert(s.rob.contains(ptr(port.robIdx)), "Jump changed source ROB identity")
          port.tag.expect(false.B)
          val inFire = bool(port.inValid) && bool(port.inReady)
          val outFire = bool(port.outValid) && bool(port.outReady)
          if (inFire && s.acceptedMode == 0) {
            dut.monitor.sourcePrivilege.expect(0.U); dut.monitor.sourceVirtual.expect(false.B)
            dut.io.debugMode.expect(false.B)
            if (s.canceled >= 0) {
              // ROB/Control recovery is one stage ahead of the execution flush.
              // A zero-latency JAL can compute in HU on that recovery edge; only
              // an explicitly matching native cancellation may annul its result.
              val controlKill = killedBy(s.rob.get, dut.monitor.redirect.valid, dut.monitor.redirect.bits)
              val executionKill = killedBy(s.rob.get, port.flush.valid, port.flush.bits)
              assert(controlKill || executionKill, "A canceled JAL issued without a matching native recovery")
              s.annulledJumpComputations += cycle
              record("annulled-jump-computation", (sourceFields(s) ++ Seq(
                "control_kill" -> flag(controlKill), "execution_kill" -> flag(executionKill),
                "raw_in_fire" -> flag(inFire), "raw_out_fire" -> flag(outFire))): _*)
            }
          }
          if (inFire) s.jumpInputs += ((cycle, uint(dut.monitor.sourcePrivilege).toInt,
            bool(dut.monitor.sourceVirtual), bool(dut.io.debugMode)))
          if (outFire) s.jumpOutputs += cycle
          record("jump", (sourceFields(s) ++ Seq("unit" -> num(i), "in_valid" -> flag(bool(port.inValid)),
            "in_ready" -> flag(bool(port.inReady)), "in_fire" -> flag(inFire), "out_fire" -> flag(outFire),
            "out_valid" -> flag(bool(port.outValid)), "out_ready" -> flag(bool(port.outReady)),
            "tag" -> flag(bool(port.tag)), "rob_flag" -> flag(ptr(port.robIdx).flag), "rob" -> num(ptr(port.robIdx).value),
            "pdest" -> num(uint(port.pdest)), "operation" -> num(uint(port.operation)),
            "base_pc" -> num(uint(port.basePc)), "result" -> num(uint(port.result)), "rf_wen" -> flag(bool(port.rfWen)),
            "exceptions" -> num(uint(port.exception)), "redirect_valid" -> flag(bool(port.redirect.valid)),
            "target" -> num(uint(port.redirect.bits.fullTarget)),
            "mispredicted" -> flag(bool(port.redirect.bits.cfiUpdate.isMisPred)), "context" -> current)): _*)
          if (phase == "ordinary-control" && s.instruction == directJump && outFire) {
            port.result.expect((s.pc + 4).U); port.redirect.bits.fullTarget.expect((s.pc + 8).U)
            port.exception.expect(0.U)
            port.redirect.bits.cfiUpdate.isMisPred.expect(false.B)
          }
        }
      }
      dut.monitor.robWritebackArrival.foreach { port =>
        if (bool(port.valid)) {
          val source = byRob.getOrElse(ptr(port.bits), throw new AssertionError("Native ROB writeback arrival lost its source"))
          assert(source.removedBeforeAllocation < 0)
          source.robWritebackArrivals += cycle
          val annulled = source.canceled >= 0
          if (annulled) {
            // This registered arrival may predate a newly visible ROB redirect.
            // Only a matching native redirect on this exact edge can annul it.
            assert(killedBy(source.rob.get, dut.monitor.redirect.valid, dut.monitor.redirect.bits),
              "A canceled source reached ROB writeback without a current matching redirect")
            source.annulledRobWritebackArrivals += cycle
          }
          record("rob-writeback-arrival", (sourceFields(source) :+ ("annulled_by_current_redirect" -> flag(annulled))): _*)
        }
      }
      if (bool(dut.io.csrRequest.valid)) {
        val owner = byRob.getOrElse(ptr(dut.io.csrRequestRob), throw new AssertionError("CSR acceptance lacks an allocated owner"))
        owner.csrAccepts += 1
        assert(owner.csrAccepts == 1 && owner.canceled < 0)
        dut.monitor.csrSourcePc.expect(owner.pc.U)
        record("csr-accept", (sourceFields(owner) ++ Seq("address" -> num(uint(dut.io.csrRequest.bits)), "context" -> current)): _*)
      }
      if (bool(dut.io.legalCSRWrite.valid)) {
        legalWrites += uint(dut.io.legalCSRWrite.bits).toInt
        val owner = sources.reverse.find(s => s.pc == uint(dut.monitor.csrSourcePc)).get
        owner.csrEffects += 1
        assert(owner.csrEffects == 1)
        record("csr-effect", (sourceFields(owner) :+ ("address" -> num(uint(dut.io.legalCSRWrite.bits)))): _*)
      }
      if (bool(dut.io.csrResponse.valid)) record("csr-response", "data" -> num(uint(dut.io.csrResponse.bits)))
      dut.io.robRetire.foreach { port =>
        if (bool(port.valid)) {
          val s = byRob.getOrElse(ptr(port.bits), throw new AssertionError("Retirement lacks allocated identity"))
          assert(s.retired < 0 && s.canceled < 0, "A source retired twice or after cancellation")
          if (storePlan.contains(s.pc)) assert(s.storeCompletions == 1)
          if (criticalExpected && s.acceptedMode == 0) {
            dut.io.mode.expect(0.U); dut.io.debugMode.expect(false.B)
          }
          s.retired = cycle; record("retire", (sourceFields(s) :+ ("context" -> current)): _*)
        }
      }
      if (bool(dut.io.backendRedirect.valid)) {
        val r = dut.io.backendRedirect.bits
        val identity = fetch(r.ftqIdx, r.ftqOffset)
        redirects += identity
        byFetch.getOrElse(identity, throw new AssertionError("FTQ recovery lost its original source"))
          .recoveryTargets += ((cycle, uint(r.cfiUpdate.target)))
        record("backend-to-ftq-redirect", "target" -> num(uint(r.cfiUpdate.target)), "ftq" -> num(uint(r.ftqIdx.value)),
          "offset" -> num(uint(r.ftqOffset)), "level" -> num(uint(r.level)))
      }
      if (bool(dut.io.ifuRedirect.valid)) {
        val r = dut.io.ifuRedirect.bits
        ifuRedirects(fetch(r.ftqIdx, r.ftqOffset)) = uint(r.cfiUpdate.target)
      }
      if (bool(dut.io.frontendRedirect.valid)) {
        val redirect = dut.io.frontendRedirect.bits
        val identity = fetch(redirect.ftqIdx, redirect.ftqOffset)
        val target = uint(redirect.cfiUpdate.target)
        frontendRedirects(identity) = target
        record("ftq-redirect", "target" -> num(target), "ftq_flag" -> flag(identity.pointer.flag),
          "ftq" -> num(identity.pointer.value), "offset" -> num(identity.offset))
      }
      if (bool(dut.io.robException.valid)) {
        record("rob-exception", "interrupt" -> flag(bool(dut.io.robException.bits.isInterrupt)),
          "pc" -> num(uint(dut.io.robException.bits.pc)))
        if (!expectedSynchronousTrap)
          assert(bool(dut.io.robException.bits.isInterrupt), "Unexpected synchronous source exception")
      }
      dut.clock.step(); cycle += 1
    }
    def idle(n: Int): Unit = (0 until n).foreach(_ => edge())
    def until(label: String, maximum: Int = 2000)(condition: => Boolean): Unit = {
      var count = 0
      while (!condition && count < maximum) { edge(); count += 1 }
      assert(condition, s"Timed out: $label at cycle $cycle")
    }
    def reset(): Unit = {
      zeroInputs(dut.io); zeroInputs(dut.memory)
      dut.memory.metadata.foreach(_.req.ready.poke(true.B))
      dut.memory.ptw.req.foreach(_.ready.poke(true.B))
      dut.memory.cacheWrite.req.ready.poke(false.B)
      lanes.foreach { lane => lane.valid.poke(false.B); zero(lane.bits) }
      dut.io.bpu.resp.valid.poke(false.B); zero(dut.io.bpu.resp.bits)
      zero(dut.io.predecode); zero(dut.io.interrupts)
      dut.io.fetchReady.poke(true.B); dut.io.traceEnable.poke(false.B); dut.io.traceStall.poke(false.B)
      dut.reset.poke(true.B); dut.clock.step(10)
      dut.reset.poke(false.B); idle(400)
      dut.io.mode.expect(3.U); dut.io.debugMode.expect(false.B)
      dut.io.csrSiblingReady.foreach(_.expect(true.B))
    }
    def prediction(stage: BranchPredictionBundle, packet: Packet): Unit = {
      zero(stage)
      stage.pc.foreach(_.poke(packet.base.U)); stage.valid.foreach(_.poke(true.B))
      setPtr(stage.ftq_idx, packet.pointer)
      packet.sources.find(_.instruction == directJump).foreach { jump =>
        stage.full_pred.foreach { pred =>
          pred.hit.poke(true.B); pred.slot_valids.last.poke(true.B)
          pred.targets.last.poke((jump.pc + 8).U); pred.offsets.last.poke(jump.fetch.offset.U)
          pred.is_jal.poke(true.B); pred.is_call.poke(true.B)
          pred.fallThroughAddr.poke((jump.pc + 4).U)
        }
      }
    }
    def prepare(pc: BigInt, instructions: Seq[BigInt]): Packet = {
      assert(!bool(dut.io.backendRedirect.valid) && !bool(dut.io.frontendRedirect.valid),
        "Consume the matching FTQ redirect before planning its recovered BPU packet")
      val pointer = ptr(dut.io.ftqNext)
      val entryBytes = dut.io.predecode.bits.pd.length * 2
      val base = pc - pc % entryBytes
      val planned = instructions.zipWithIndex.map { case (instruction, i) =>
        val address = pc + i * 4
        val id = Fetch(pointer, ((address - base) / 2).toInt)
        require(id.offset < dut.io.predecode.bits.pd.length)
        val s = new Source(sources.size, address, instruction, id)
        sources += s; byFetch(id) = s; s
      }
      val packet = Packet(base, pointer, planned)
      record("fetch-prepare", "source_pc" -> num(pc), "base_pc" -> num(base),
        "ftq_flag" -> flag(pointer.flag), "ftq" -> num(pointer.value))
      fetched -= pointer
      zero(dut.io.bpu.resp.bits); prediction(dut.io.bpu.resp.bits.s1, packet)
      packet.sources.find(_.instruction == directJump).foreach { jump =>
        val entry = dut.io.bpu.resp.bits.last_stage_ftb_entry
        entry.valid.poke(true.B); entry.isCall.poke(true.B)
        entry.tailSlot.valid.poke(true.B); entry.tailSlot.offset.poke(jump.fetch.offset.U)
        entry.tailSlot.lower.poke(((jump.pc + 8) >> 1).U)
        entry.tailSlot.tarStat.poke(0.U)
        entry.pftAddr.poke((((jump.pc + 4) >> 1) & ((BigInt(1) << entry.pftAddr.getWidth) - 1)).U)
      }
      dut.io.bpu.resp.valid.poke(true.B)
      until("BPU readiness")(bool(dut.io.bpu.resp.ready)); edge()
      dut.io.bpu.resp.valid.poke(false.B)
      zero(dut.io.bpu.resp.bits.s1); prediction(dut.io.bpu.resp.bits.s2, packet); edge()
      zero(dut.io.bpu.resp.bits.s2); prediction(dut.io.bpu.resp.bits.s3, packet); edge()
      zero(dut.io.bpu.resp.bits.s3)
      until("FTQ fetch request")(fetched(pointer))
      packet
    }
    def send(packet: Packet): Unit = {
      zero(dut.io.predecode.bits); setPtr(dut.io.predecode.bits.ftqIdx, packet.pointer)
      dut.io.predecode.bits.pc.zipWithIndex.foreach { case (pc, i) => pc.poke((packet.base + 2 * i).U) }
      packet.sources.foreach { s =>
        val pd = dut.io.predecode.bits.pd(s.fetch.offset)
        pd.valid.poke(true.B); pd.brType.poke(if (s.instruction == directJump) BrType.jal else BrType.notCFI)
        pd.isCall.poke((s.instruction == directJump).B)
        dut.io.predecode.bits.instrRange(s.fetch.offset).poke(true.B)
      }
      dut.io.predecode.bits.ftqOffset.poke(packet.sources.last.fetch.offset.U)
      dut.io.predecode.bits.target.poke((packet.sources.last.pc + (if (packet.sources.last.instruction == directJump) 8 else 4)).U)
      packet.sources.find(_.instruction == directJump).foreach { jump =>
        // The fixed source word 0x008000ef encodes JAL x1, +8.
        dut.io.predecode.bits.cfiOffset.valid.poke(true.B)
        dut.io.predecode.bits.cfiOffset.bits.poke(jump.fetch.offset.U)
        dut.io.predecode.bits.jalTarget.poke((jump.pc + 8).U)
      }
      dut.io.predecode.valid.poke(true.B); edge(); dut.io.predecode.valid.poke(false.B)
      var consumed = 0
      var waited = 0
      while (consumed < packet.sources.size && waited < 2000) {
        lanes.zipWithIndex.foreach { case (lane, slot) =>
          val index = consumed + slot
          lane.valid.poke((index < packet.sources.size).B); zero(lane.bits)
          if (index < packet.sources.size) {
            val s = packet.sources(index)
            lane.bits.instr.poke(s.instruction.U); lane.bits.pc.poke(s.pc.U)
            lane.bits.trigger.poke(TriggerAction.None); lane.bits.fdiNotTrusted.foreach(_.poke(false.B))
            lane.bits.pd.valid.poke(true.B)
            lane.bits.pd.brType.poke(if (s.instruction == directJump) BrType.jal else BrType.notCFI)
            lane.bits.pd.isCall.poke((s.instruction == directJump).B)
            lane.bits.pred_taken.poke((s.instruction == directJump).B)
            setPtr(lane.bits.ftqPtr, packet.pointer); lane.bits.ftqOffset.poke(s.fetch.offset.U)
            lane.bits.isLastInFtqEntry.poke((index == packet.sources.size - 1).B)
          }
        }
        val available = (packet.sources.size - consumed).min(lanes.size)
        val accepted = (0 until available).takeWhile(i => bool(lanes(i).ready)).size
        assert((0 until available).count(i => bool(lanes(i).ready)) == accepted, "Frontend acceptance is not ordered")
        packet.sources.slice(consumed, consumed + accepted).foreach { s =>
          s.accepted = cycle; s.acceptedMode = uint(dut.io.mode).toInt; s.acceptedVirtual = bool(dut.io.virtualMode)
          record("frontend-accept", (sourceFields(s) ++ Seq("instruction" -> num(s.instruction),
            "expected_tag" -> flag(false), "mode" -> num(s.acceptedMode), "virtual" -> flag(s.acceptedVirtual))): _*)
        }
        edge(); consumed += accepted; waited += 1
      }
      lanes.foreach { lane => lane.valid.poke(false.B); zero(lane.bits) }
      assert(consumed == packet.sources.size, "Frontend packet was never accepted")
    }
    def execute(instruction: BigInt): Source = {
      val packet = prepare(ordinaryPc, Seq(instruction)); ordinaryPc += 4
      send(packet)
      val s = packet.sources.head
      until("instruction retirement or its recovery redirect")(s.retired >= 0 || redirects(s.fetch))
      if (redirects(s.fetch)) until("matching IFU and BPU recovery consumed") {
        ifuRedirects.contains(s.fetch) && frontendRedirects.contains(s.fetch)
      }
      idle(12); s
    }
    def addi(rd: Int, rs1: Int, imm: Int): BigInt =
      (BigInt(imm & 0xfff) << 20) | (BigInt(rs1) << 15) | (BigInt(rd) << 7) | 0x13
    def write(address: Int, value: Int): Unit = {
      val high = (value + 0x800) >> 12
      val low = value - (high << 12)
      if (high != 0) { execute((BigInt(high) << 12) | 0xb7); if (low != 0) execute(addi(1, 1, low)) }
      else execute(addi(1, 0, low))
      val before = legalWrites.size
      execute((BigInt(address) << 20) | (BigInt(1) << 15) | 0x1073)
      assert(legalWrites.drop(before).toSeq == Seq(address), s"CSR 0x${address.toHexString} did not write exactly once")
    }
    def setup(): Unit = {
      dut.io.interrupts.debug.poke(true.B)
      until("selected external halt request")(bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.debug))
      val halt = prepare(0x1104, Seq(BigInt(0x13))); send(halt)
      until("real debug entry and target") {
        bool(dut.io.debugMode) && frontendRedirects.get(halt.sources.head.fetch).contains(dut.debugEntryAddress) &&
          ifuRedirects.get(halt.sources.head.fetch).contains(dut.debugEntryAddress)
      }
      dut.io.dpc.expect(0x1104.U)
      dut.io.interrupts.debug.poke(false.B)
      ordinaryPc = dut.debugEntryAddress
      write(0x30c, 1); write(0x10c, 1)
      write(0x000, 0); write(0x004, 0)
      write(0x9e2, 0x1000); write(0x9e3, 0x3000); write(0xbc4, 2)
      write(0x3b0, 0x40000000); write(0x3a0, 15)
      // Sv39 is configured before either Store. Only PTW replies are withheld later.
      execute(addi(1, 0, 1)); execute((BigInt(63) << 20) | (BigInt(1) << 15) | 0x1093)
      execute((BigInt(0x180) << 20) | (BigInt(1) << 15) | 0x1073)
      write(0x300, 0); write(0x7b0, 0x80000); write(0x7b1, 0x1200)
      execute(BigInt("7b200073", 16))
      dut.io.mode.expect(0.U); dut.io.virtualMode.expect(false.B); dut.io.debugMode.expect(false.B)
      assert(uint(dut.io.dcsr).testBit(19), "Legal DRET must preserve CETRIG")
      dut.io.timerPending.expect(false.B); dut.io.criticalErrorState.expect(false.B)
      assert(uint(dut.io.cumulativeHuReservations) == 0)
      until("setup ROB drains")(bool(dut.monitor.robEmpty))
      record("setup-complete", "dcsr" -> num(uint(dut.io.dcsr)), "mode" -> num(uint(dut.io.mode)))
    }
    def idleCounters: Seq[BigInt] = Seq(uint(dut.monitor.accepts), uint(dut.monitor.allocations),
      uint(dut.monitor.jumpInputs), uint(dut.monitor.jumpOutputs), uint(dut.io.cumulativeRobRetireCycles),
      uint(dut.io.cumulativeDebugEntries), uint(dut.io.cumulativeBackendRedirects), uint(dut.io.cumulativeHuReservations))

    def store(base: Int, data: Int): BigInt =
      (BigInt(data) << 20) | (BigInt(base) << 15) | (BigInt(3) << 12) | 0x23
    def initializeRegisters(): Unit = {
      ordinaryPc = 0x1200
      execute(addi(8, 0, 1)); execute((BigInt(31) << 20) | (BigInt(8) << 15) | (BigInt(8) << 7) | 0x1013)
      execute(addi(8, 8, 64))
      execute((BigInt(0x10) << 12) | (BigInt(9) << 7) | 0x37)
      execute((BigInt(9) << 20) | (BigInt(8) << 15) | (BigInt(9) << 7) | 0x33)
      execute((BigInt(0x20) << 12) | (BigInt(13) << 7) | 0x37)
      execute((BigInt(13) << 20) | (BigInt(8) << 15) | (BigInt(13) << 7) | 0x33)
      execute(addi(10, 0, 0x5a5)); execute(addi(11, 0, 0x6b6)); execute(addi(12, 0, 0x321))
    }
    def checkHeldOld(): Unit = {
      val snapshot = heldOldSnapshot.get
      val request = dut.memory.cacheWrite.req
      request.valid.expect(true.B); request.ready.expect(false.B)
      val dataMask = (0 until request.bits.mask.getWidth).filter(snapshot._3.testBit)
        .foldLeft(BigInt(0))((value, byte) => value | (BigInt(255) << (8 * byte)))
      val actual = (uint(request.bits.addr), uint(request.bits.id), uint(request.bits.mask), uint(request.bits.data) & dataMask)
      assert(actual == snapshot, "The retired Store offer changed before public cache acceptance")
    }
    def run(): Unit = {
      reset(); setup(); initializeRegisters()
      phase = "ordinary-control"
      val control = prepare(0x1400, Seq(scratchWrite, directJump)); send(control)
      until("ordinary CSR and JAL retirement")(control.sources.forall(_.retired >= 0))
      assert(control.sources.head.csrAccepts == 1 && control.sources.head.csrEffects == 1)
      dut.io.userScratch.expect(0x321.U)
      assert(control.sources.last.jumpInputs.size == 1 && control.sources.last.jumpOutputs.size == 1)
      assert(control.sources.forall(_.canceled < 0))
      idle(16)
      execute(addi(12, 0, 0x7c7))
      phase = "retired-store"
      storePlan(0x1500) = (baseAddress, BigInt(0x5a5))
      val oldPacket = prepare(0x1500, Seq(store(8, 10))); send(oldPacket)
      val old = oldPacket.sources.head
      until("real translated Store retirement and internal SQ transfer")(old.retired >= 0 && old.buffered >= 0)
      dut.memory.forceFlush.poke(true.B)
      until("held old Sbuffer cache offer")(heldOffer.nonEmpty)
      heldOldSnapshot = heldOffer
      assert(actualBytes.isEmpty && old.storeCompletions == 1 && ptwResponses == 1)
      assert(old.retired >= 0 && old.cacheOffered >= old.retired && old.cacheWritten < 0)
      dut.memory.forceFlush.poke(false.B)
      checkHeldOld()

      phase = "unsafe-store-preparation"
      storePlan(0x1800) = (waitingAddress, BigInt(0x6b6))
      storePlan(0x1810) = (youngAddress, BigInt(0x5a5))
      val trial = prepare(0x1800, Seq(store(9, 11), scratchWrite, directJump)); send(trial)
      val young = prepare(0x1810, Seq(store(13, 10))); send(young)
      diagnostic = trial.sources ++ young.sources
      val waiting = trial.sources.head
      until("actual unsafe Store head with withheld public PTW") {
        waiting.rob.nonEmpty && bool(dut.io.robHead.valid) && ptr(dut.io.robHead.robIdx) == waiting.rob.get &&
          !bool(dut.io.robHead.interruptSafe) && ptwPending.contains(waitingAddress >> 12)
      }
      assert(waiting.storeIssues > 0 && waiting.storeCompletions == 0 && waiting.retired < 0)
      idle(24)
      checkHeldOld()
      record("unsafe-head-ready", sourceFields(waiting): _*)
      naturalTrialExecuted = true
      phase = "natural-watchdog-wait"
      // Miss replays use the native IQ feedback path. Their metadata prefetches
      // are killed at S1; no cache hit response is required while PTW is withheld.
      dut.memory.metadata.foreach(_.resp.valid.poke(false.B))
      metadataReplies.foreach(_.clear())
      dut.memory.ptw.resp.valid.poke(false.B)
      while (uint(dut.io.robCommitStuck) < threshold - 64) {
        dut.io.mode.expect(0.U); dut.io.debugMode.expect(false.B); dut.io.criticalErrorState.expect(false.B)
        dut.io.robHead.interruptSafe.expect(false.B)
        assert(ptr(dut.io.robHead.robIdx) == waiting.rob.get)
        checkHeldOld()
        val before = idleCounters ++ Seq(uint(dut.memory.cumulativeCompletions), uint(dut.memory.cumulativePublications))
        val count = uint(dut.io.robCommitStuck)
        val amount = (BigInt(threshold - 64) - count).min(65536).toInt
        val start = System.nanoTime()
        dut.clock.step(amount); cycle += amount
        val after = idleCounters ++ Seq(uint(dut.memory.cumulativeCompletions), uint(dut.memory.cumulativePublications))
        assert(after == before, "An architectural effect occurred during the withheld service interval")
        dut.io.robCommitStuck.expect((count + amount).U)
        val entry = Json.obj("cycle" -> num(cycle), "clocks" -> num(amount),
          "seconds" -> Json.fromDoubleOrNull((System.nanoTime() - start).toDouble / 1e9), "watchdog" -> num(count + amount))
        batches.write(entry.noSpaces); batches.newLine(); batches.flush()
        println(s"Critical recovery natural wait cycle=$cycle watchdog=${count + amount}")
      }
      criticalExpected = true; phase = "unsafe-head-critical-pending"
      until("real default watchdog overflow and CSR pending", 256)(overflow >= 0 && bool(dut.io.criticalErrorState))
      idle(32)
      dut.io.mode.expect(0.U); dut.io.debugMode.expect(false.B)
      dut.io.robHead.interruptSafe.expect(false.B)
      assert(ptr(dut.io.robHead.robIdx) == waiting.rob.get && waiting.retired < 0 && waiting.canceled < 0)
      assert(acceptedCritical.isEmpty && debugEffects.isEmpty && waiting.storeCompletions == 0)
      checkHeldOld()
      record("unsafe-head-preserved", sourceFields(waiting): _*)

      phase = "release-original-ptw"
      allowWaitingPtw = true
      until("Store completion, safe critical owner and both FTQ recoveries", 4000) {
        acceptedCritical.size == 1 && debugEffects.size == 1 && bool(dut.io.debugMode) &&
          frontendRedirects.get(acceptedCritical.head._2.fetch).contains(dut.debugEntryAddress) &&
          ifuRedirects.get(acceptedCritical.head._2.fetch).contains(dut.debugEntryAddress)
      }
      val owner = acceptedCritical.head._2
      dut.io.dpc.expect(owner.pc.U)
      assert(owner.recoveryTargets.map(_._2).toSeq == Seq(dut.debugEntryAddress),
        "The accepted critical owner must deliver exactly one Debug recovery target")
      assert(((uint(dut.io.dcsr) >> 6) & 7) == 7, "Critical owner must record Other")
      assert(waiting.retired >= 0 && waiting.retired < debugEffect && waiting.storeCompletions == 1)
      assert(ptwResponses == 2)
      val serial = trial.sources(1)
      if (serial.csrEffects != 0) {
        assert(serial.csrAccepts == 1 && serial.csrEffects == 1 && serial.retired >= 0 && serial.retired < debugEffect)
        dut.io.userScratch.expect(0x7c7.U)
      } else assert(serial.canceled >= 0 || serial.removedBeforeAllocation >= 0)
      diagnostic.foreach { source =>
        assert(source.retired >= 0 || source.canceled >= 0 || source.removedBeforeAllocation >= 0,
          s"Old source ${source.pc.toString(16)} survived without a terminal owner")
        assert(source.jumpInputs.forall { case (_, mode, virtual, debug) => mode == 0 && !virtual && !debug })
        if (source.canceled >= 0 || source.removedBeforeAllocation >= 0) {
          assert(source.retired < 0 && source.buffered < 0)
          assert(source.robWritebackArrivals.forall(c => c < source.canceled || source.annulledRobWritebackArrivals.contains(c)))
          if (source ne owner) assert(source.recoveryTargets.isEmpty)
        }
      }
      val canceledStore = young.sources.head
      assert(canceledStore.storeIssues == canceledStore.annulledStoreIssues)
      assert(canceledStore.storeDataIssues == canceledStore.annulledStoreDataIssues)
      assert(canceledStore.storeS0Fires == 0 && canceledStore.storeS1Fires == 0 && canceledStore.storeS2Fires == 0)
      assert(canceledStore.storeCompletions == 0 && canceledStore.buffered < 0 && canceledStore.retired < 0)
      assert(canceledStore.queueCanceled >= 0 && canceledStore.queueReleased > canceledStore.queueCanceled)
      checkHeldOld()
      phase = "preserved-store-drain"
      dut.memory.forceFlush.poke(true.B); dut.memory.cacheWrite.req.ready.poke(true.B)
      until("both legitimate Store effects acknowledged and drained", 3000) {
        bool(dut.memory.queueEmpty) && bool(dut.memory.bufferEmpty) && cacheReplies.isEmpty && actualBytes.size == 16
      }
      for ((address, data) <- Seq(baseAddress -> BigInt(0x5a5), waitingAddress -> BigInt(0x6b6)); byte <- 0 until 8)
        assert(actualBytes(address + byte) == ((data >> (8 * byte)) & 255).toInt)
      Seq(old, waiting).foreach { source =>
        assert(source.retired >= 0 && source.cacheOffered >= source.retired)
        assert(source.cacheWritten >= source.cacheOffered && source.cacheAcknowledged > source.cacheWritten)
      }
      assert(acceptedCritical.size == 1 && debugEffects.size == 1)
      assert(owner.recoveryTargets.map(_._2).toSeq == Seq(dut.debugEntryAddress))
      assert(uint(dut.io.cumulativeHuReservations) == 0)
      lastOutcome = "OWNED_CRITICAL_RECOVERY_PASS"
      record("critical-recovery-complete", "sources" -> Json.arr(diagnostic.map(_.json): _*))
    }
    def runLocalDoubleTrap(): Unit = {
      reset(); phase = "local-double-trap-setup"
      dut.io.interrupts.debug.poke(true.B)
      until("ordinary halt candidate")(bool(dut.io.candidate.valid) && bool(dut.io.candidate.bits.debug))
      val halt = prepare(0x1104, Seq(BigInt(0x13))); send(halt)
      until("ordinary Debug full recovery") {
        bool(dut.io.debugMode) && frontendRedirects.get(halt.sources.head.fetch).contains(dut.debugEntryAddress) &&
          ifuRedirects.get(halt.sources.head.fetch).contains(dut.debugEntryAddress)
      }
      assert(((uint(dut.io.dcsr) >> 6) & 7) == 3)
      dut.io.interrupts.debug.poke(false.B); ordinaryPc = dut.debugEntryAddress
      write(0x7b0, 0x80003); write(0x7b1, 0x2200)
      // mnstatus.NMIE clears through its legal CSR write, not an internal poke.
      write(0x744, 0)
      execute(BigInt("7b200073", 16))
      dut.io.mode.expect(3.U); dut.io.debugMode.expect(false.B)
      assert(!uint(dut.io.mnstatus).testBit(3))
      phase = "local-double-trap"
      expectedSynchronousTrap = true
      val before = uint(dut.io.cumulativeDebugEntries)
      val trap = prepare(0x2200, Seq(BigInt("00000073", 16))); diagnostic = trap.sources; send(trap)
      until("local critical target uses original trap slot") {
        bool(dut.io.debugMode) && frontendRedirects.get(trap.sources.head.fetch).contains(dut.debugEntryAddress) &&
          ifuRedirects.get(trap.sources.head.fetch).contains(dut.debugEntryAddress)
      }
      dut.io.dpc.expect(0x2200.U)
      assert(((uint(dut.io.dcsr) >> 6) & 7) == 7)
      dut.io.criticalErrorState.expect(true.B)
      idle(32)
      assert(uint(dut.io.cumulativeDebugEntries) == before + 1)
      assert(trap.sources.head.canceled >= 0 && trap.sources.head.retired < 0)
      assert(acceptedCritical.isEmpty, "Local trap feedback must not create a fresh asynchronous owner")
      lastOutcome = "OWNED_LOCAL_DOUBLE_TRAP_PASS"
    }
    def runResetControl(): Unit = {
      reset(); phase = "reset-control"
      ordinaryPc = 0x1000
      execute(addi(1, 0, 0x321))
      val source = execute((BigInt(0x340) << 20) | (BigInt(1) << 15) | 0x1073)
      diagnostic = Seq(source)
      assert(source.csrAccepts == 1 && source.csrEffects == 1 && source.retired >= 0)
      dut.io.criticalErrorState.expect(false.B); dut.io.debugMode.expect(false.B)
      assert(uint(dut.io.cumulativeDebugEntries) == 0 && uint(dut.io.cumulativeHuReservations) == 0)
      lastOutcome = "ORDINARY_RESET_CONTROL_PASS"
    }
    def finish(error: Option[Throwable]): Unit = {
      val result = Json.obj("status" -> str(if (error.nonEmpty) "FAIL" else "PASS"),
        "classification" -> str(lastOutcome), "cycle" -> num(cycle), "overflow_cycle" -> num(overflow),
        "critical_cycle" -> num(criticalCycle), "debug_effect_cycle" -> num(debugEffect), "mode_change_cycle" -> num(modeChange),
        "natural_trial_executed" -> flag(naturalTrialExecuted), "default_threshold" -> num(threshold),
        "critical_claims" -> num(acceptedCritical.size), "debug_effects" -> num(debugEffects.size),
        "ptw_responses" -> num(ptwResponses), "external_bytes" -> num(actualBytes.size),
        "sources" -> Json.arr(diagnostic.map(_.json): _*),
        "error" -> error.map(x => str(x.toString)).getOrElse(Json.Null),
        "scope" -> str("Real Backend/FTQ recovery with native cached scalar Stores and public PTW/cache services; no MMIO, NC or AMO lifecycle."))
      Files.write(path.resolve("critical-recovery-result.json"), (result.spaces2 + "\n").getBytes(StandardCharsets.UTF_8))
      events.close(); batches.close()
    }
  }

  it should "retain old effects and recover from owned critical sources" in {
    val root = Paths.get(sys.env("CRITICAL_RUN_ROOT")).toRealPath()
    require(sys.env("CRITICAL_FDI_ENABLED").toBoolean, "The native critical Backend model is enabled")
    require(Paths.get("").toRealPath() == root, "Run from the dedicated F02 evidence directory")
    val path = root.resolve("critical-backend")
    require(!Files.exists(path), "Never replace a previous critical recovery attempt")
    Files.createDirectory(path)
    implicit val p: Parameters = UserTimerDeliveryParameters(enabled = true)
    val options = p(DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    val workspace = new Workspace(path.resolve("compiled").toString)
    workspace.reset()
    val module = workspace.elaborateGeneratedModule(() => new FDICriticalRecoveryBackendHarness)
    workspace.generateAdditionalSources()
    val common = CommonCompilationSettings(availableParallelism =
      CommonCompilationSettings.AvailableParallelism.UpTo(Runtime.getRuntime.availableProcessors()))
    val settings = Backend.CompilationSettings(disabledWarnings = Seq("WIDTH", "STMTDLY"),
      disableFatalExitOnWarnings = true, enableAllAssertions = true)
    val simulation = workspace.compile(Backend.initializeFromProcessEnvironment())("verilator", common, settings, None, false)
    simulation.runElaboratedModule(module) { elaborated =>
      Seq("local-double-trap", "watchdog", "reset-control").foreach { name =>
        val directory = path.resolve(name); Files.createDirectory(directory)
        val driver = new Driver(elaborated.wrapped, directory)
        var failure: Option[Throwable] = None
        try {
          if (name == "watchdog") driver.run()
          else if (name == "local-double-trap") driver.runLocalDoubleTrap()
          else driver.runResetControl()
        } catch { case error: Throwable => failure = Some(error); throw error }
        finally driver.finish(failure)
      }
    }
  }
}
