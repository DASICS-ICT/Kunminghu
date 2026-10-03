// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.frontend

import chisel3._
import chiseltest._
import firrtl2.options.TargetDirAnnotation
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import xiangshan._

class FDITrustMetadataFrontendTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "FDI trust metadata in the real frontend"

  private val enabled = sys.env.getOrElse("C08_FDI_ENABLED",
    throw new IllegalArgumentException("C08_FDI_ENABLED must be explicit")).toBoolean
  private val scenario = sys.env.getOrElse("C08_FRONTEND_SCENARIO", "minimal")
  require(Set("minimal", "full").contains(scenario))

  private case class Context(privilege: Int, virtual: Boolean = false,
    sEnable: Boolean = false, uEnable: Boolean = false,
    sLo: BigInt = 0, sHi: BigInt = 0, uLo: BigInt = 0, uHi: BigInt = 0,
    sv39: Boolean = false, sv48: Boolean = false) {
    require(!(sv39 && sv48))
    def fullPC(pc: BigInt): BigInt = {
      val width = if (sv39) 39 else if (sv48) 48 else 64
      val mask = (BigInt(1) << width) - 1
      val low = pc & mask
      (if (width < 64 && low.testBit(width - 1)) low | (FDITrustInstructionImage.wordMask ^ mask)
       else low) & FDITrustInstructionImage.wordMask
    }
    def notTrusted(pc: BigInt): Boolean = {
      val address = fullPC(pc)
      if (privilege == 2 || (privilege == 3 && virtual)) true
      else if (privilege == 3) false
      else {
        val active = if (privilege == 1) sEnable else uEnable
        val lo = if (privilege == 1) sLo else uLo
        val hi = if (privilege == 1) sHi else uHi
        active && (virtual || !(lo < hi && lo <= address && address < hi))
      }
    }
  }
  private case class Instruction(pc: BigInt, word: BigInt, bytes: Int,
    exception: Int = 0, crossPage: Boolean = false, backendException: Boolean = false) {
    // The compressed image deliberately uses only C.NOP, whose expansion is architectural.
    def decoded: BigInt = if (bytes == 2) { require(word == 1); BigInt(0x13) } else word
  }
  private case class Mmio(physical: BigInt, translated: BigInt = 0x92000,
    tlbException: Int = 0, pmpFault: Boolean = false, responseDelay: Int = 2)
  private case class Pointer(flag: Boolean, value: Int)
  private class Fetch(val ptr: Pointer, val start: BigInt, val end: BigInt,
    val instructions: Seq[Instruction], val data: BigInt,
    val lineExceptions: Seq[Int] = Seq(0, 0), val mmio: Option[Mmio] = None,
    val backendException: Boolean = false) {
    var accepted = -1
    var sampled = -1
    var received = -1
    var context: Option[Context] = None
    var consumed = 0
    var cancelled = false
    var uncacheRequests = 0
    var uncacheResponses = 0
    var tlbRequests = 0
    var tlbResponses = 0
    var pmpChecks = 0
    var writebacks = 0
  }
  private case class Expected(fetch: Fetch, instruction: Instruction, enqueued: Int, tag: Boolean)

  private class Driver(val dut: FDITrustMetadataFrontendHarness) {
    var cycles = 0
    var requests = 0
    var responses = 0
    var f2Transfers = 0
    var packets = 0
    var delivered = 0
    var cacheHold = false
    var heldFetchCycles = 0
    var heldDecodeCycles = 0
    var bypassPackets = 0
    var storedPackets = 0
    var enqueueWraps = 0
    var dequeueWraps = 0
    var cancellations = 0
    val events = mutable.ArrayBuffer.empty[String]
    val cases = mutable.ArrayBuffer.empty[String]
    private var context = Context(3)
    private var configuration = 0
    private var offered: Option[Fetch] = None
    private val cache = mutable.Queue.empty[Fetch]
    private val flights = mutable.Map.empty[Pointer, Fetch]
    private val expected = mutable.Queue.empty[Expected]
    private var activeMmio: Option[Fetch] = None
    private var retainedOnRedirect: Option[Fetch] = None
    private val uncacheReplies = mutable.Queue.empty[(Fetch, Int, Int)]
    private val tlbReplies = mutable.Queue.empty[(Fetch, Int)]

    private def bool(value: Bool): Boolean = value.peek().litToBoolean
    private def uint(value: UInt): BigInt = value.peek().litValue
    private def pointer(value: FtqPtr): Pointer = Pointer(bool(value.flag), uint(value.value).toInt)
    private def zero(data: Data): Unit = data match {
      case value: Bool => value.poke(false.B)
      case value: UInt => value.poke(0.U)
      case value: SInt => value.poke(0.S)
      case vector: Vec[_] => vector.foreach(zero)
      case record: Record => record.elements.values.foreach(zero)
      case other => throw new IllegalArgumentException(s"Unsupported fixture field ${other.getClass.getName}")
    }
    private def event(kind: String, fetch: Fetch, detail: String = ""): Unit = {
      events += s"{\"cycle\":$cycles,\"event\":\"$kind\",\"start_pc\":${fetch.start}," +
        s"\"ftq\":${fetch.ptr.value},\"ftq_flag\":${fetch.ptr.flag}${if (detail.isEmpty) "" else "," + detail}}"
    }
    def mark(name: String): Unit = {
      assert(!cases.contains(name))
      cases += name
      events += s"{\"cycle\":$cycles,\"event\":\"case\",\"name\":\"$name\"}"
    }
    def waitFor(condition: => Boolean, limit: Int = 160): Unit = {
      var wait = 0
      while (!condition && wait < limit) { edge(); wait += 1 }
      assert(condition, s"Timed out at cycle $cycles")
    }
    def idle(count: Int): Unit = for (_ <- 0 until count) edge()
    def pending: Int = expected.size
    def f2Valid: Boolean = bool(dut.io.f2.valid)
    def full: Boolean = bool(dut.io.bufferFull)
    def stopped: Boolean = bool(dut.io.cacheStop)
    def f3Valid: Boolean = bool(dut.io.f3Valid)
    private def cancel(): Unit = {
      flights.values.filter(f => !f.cancelled && f.consumed < f.instructions.size &&
        !retainedOnRedirect.contains(f)).foreach { fetch =>
        fetch.cancelled = true
        cancellations += 1
        event("cancel", fetch, s"\"sampled\":${fetch.sampled >= 0},\"buffered\":${fetch.received >= 0}")
      }
      cache.clear()
      uncacheReplies.clear()
      tlbReplies.clear()
      activeMmio = retainedOnRedirect
    }
    def configure(next: Context): Unit = {
      context = next
      configuration += 1
      events += s"{\"cycle\":$cycles,\"event\":\"configuration\",\"configuration\":$configuration," +
        s"\"privilege\":${next.privilege},\"virtual\":${next.virtual}," +
        s"\"u_enable\":${next.uEnable},\"s_enable\":${next.sEnable}," +
        s"\"u_lo\":${next.uLo},\"u_hi\":${next.uHi},\"s_lo\":${next.sLo},\"s_hi\":${next.sHi}," +
        s"\"sv39\":${next.sv39},\"sv48\":${next.sv48}}"
      dut.io.config.foreach { value =>
        value.sourcePrivilege.poke(next.privilege.U)
        value.sourceVirtual.poke(next.virtual.B)
        value.sEnable.poke(next.sEnable.B)
        value.uEnable.poke(next.uEnable.B)
        value.sBoundLo.poke(next.sLo.U)
        value.sBoundHi.poke(next.sHi.U)
        value.uBoundLo.poke(next.uLo.U)
        value.uBoundHi.poke(next.uHi.U)
        value.sv39.poke(next.sv39.B)
        value.sv48.poke(next.sv48.B)
      }
    }
    def consume(ready: Boolean): Unit = {
      dut.io.decodeCanAccept.poke(ready.B)
      dut.io.decode.foreach(_.ready.poke(ready.B))
    }
    def reset(): Unit = {
      retainedOnRedirect = None
      cancel()
      expected.clear()
      flights.clear()
      offered = None
      cacheHold = false
      zero(dut.io.ftq.req.bits)
      dut.io.ftq.req.valid.poke(false.B)
      zero(dut.io.ftq.redirect)
      zero(dut.io.ftq.topdown_redirect)
      zero(dut.io.ftq.flushFromBpu)
      dut.io.cacheReady.poke(false.B)
      zero(dut.io.cacheResponse)
      dut.io.uncache.toUncache.ready.poke(false.B)
      dut.io.uncache.fromUncache.valid.poke(false.B)
      zero(dut.io.uncache.fromUncache.bits)
      dut.io.tlb.req.ready.poke(false.B)
      dut.io.tlb.resp.valid.poke(false.B)
      zero(dut.io.tlb.resp.bits)
      zero(dut.io.pmp.resp)
      zero(dut.io.commits)
      dut.io.mmioLastCommit.poke(false.B)
      consume(true)
      configure(Context(3))
      dut.reset.poke(true.B)
      dut.clock.step(5)
      cycles += 5
      dut.reset.poke(false.B)
      dut.io.cacheReady.poke(true.B)
      edge()
      dut.io.decode.foreach(_.valid.expect(false.B))
    }

    private def driveCache(): Option[Fetch] = {
      dut.io.cacheResponse.valid.poke(false.B)
      val next = cache.headOption.filter(fetch => !cacheHold && cycles >= fetch.accepted + 2 &&
        !bool(dut.io.cacheStop) && bool(dut.io.f2.valid) && !bool(dut.io.ftq.redirect.valid))
      next.foreach { fetch =>
        assert(pointer(dut.io.f2.request.ftqIdx) == fetch.ptr,
          "The external cache queue and actual F2 identity disagree")
        val response = dut.io.cacheResponse.bits
        zero(response)
        response.doubleline.poke(((fetch.start & 63) >= 32).B)
        response.vaddr(0).poke(fetch.start.U)
        response.vaddr(1).poke(((fetch.start & ~BigInt(63)) + 64).U)
        response.paddr(0).poke(fetch.mmio.map(_.physical).getOrElse(fetch.start).U)
        response.paddr(1).poke(((fetch.start & ~BigInt(63)) + 64).U)
        response.data.poke(fetch.data.U)
        response.backendException.poke(fetch.backendException.B)
        for (line <- 0 until 2) {
          response.exception(line).poke(fetch.lineExceptions(line).U)
          response.pmp_mmio(line).poke(fetch.mmio.isDefined.B)
        }
        dut.io.cacheResponse.valid.poke(true.B)
      }
      next
    }

    private def driveServices(): Unit = {
      dut.io.uncache.toUncache.ready.poke(true.B)
      dut.io.uncache.fromUncache.valid.poke(false.B)
      uncacheReplies.headOption.filter(_._3 <= cycles).foreach { case (fetch, ordinal, _) =>
        val instruction = fetch.instructions.head
        val spec = fetch.mmio.get
        val resend = instruction.bytes == 4 && (spec.physical & 7) == 6
        val payload = if (ordinal == 1) instruction.word >> 16
          else if (resend) instruction.word & 0xffff else instruction.word
        dut.io.uncache.fromUncache.bits.data.poke(payload.U)
        dut.io.uncache.fromUncache.bits.corrupt.poke(false.B)
        dut.io.uncache.fromUncache.valid.poke(true.B)
      }
      dut.io.tlb.req.ready.poke(true.B)
      dut.io.tlb.resp.valid.poke(false.B)
      tlbReplies.headOption.filter(_._2 <= cycles).foreach { case (fetch, _) =>
        val spec = fetch.mmio.get
        zero(dut.io.tlb.resp.bits)
        dut.io.tlb.resp.bits.paddr(0).poke(spec.translated.U)
        dut.io.tlb.resp.bits.excp(0).pf.instr.poke((spec.tlbException == 1).B)
        dut.io.tlb.resp.bits.excp(0).gpf.instr.poke((spec.tlbException == 2).B)
        dut.io.tlb.resp.bits.excp(0).af.instr.poke((spec.tlbException == 3).B)
        dut.io.tlb.resp.valid.poke(true.B)
      }
      // PMP has a combinational response, sampled in its one request cycle.
      zero(dut.io.pmp.resp)
      activeMmio.foreach { fetch =>
        dut.io.pmp.resp.mmio.poke(true.B)
        dut.io.pmp.resp.instr.poke(fetch.mmio.get.pmpFault.B)
      }
    }

    private def checkFetch(fetch: Fetch): Unit = {
      val value = dut.io.fetch.bits
      assert(!fetch.cancelled && fetch.sampled >= 0 && fetch.received < 0)
      val slots = fetch.instructions.map(i => ((i.pc - fetch.start) / 2).toInt)
      val mask = slots.foldLeft(BigInt(0))((sum, slot) => sum | (BigInt(1) << slot))
      value.enqEnable.expect(mask.U)
      for ((instruction, slot) <- fetch.instructions.zip(slots)) {
        value.pc(slot).expect(instruction.pc.U)
        value.instrs(slot).expect(instruction.decoded.U)
        value.pd(slot).valid.expect(true.B)
        value.pd(slot).isRVC.expect((instruction.bytes == 2).B)
        value.ftqOffset(slot).bits.expect(slot.U)
        value.ftqOffset(slot).valid.expect(false.B)
        value.exceptionType(slot).expect(instruction.exception.U)
        value.crossPageIPFFix(slot).expect(instruction.crossPage.B)
        value.illegalInstr(slot).expect(false.B)
        value.backendException(slot).expect(instruction.backendException.B)
        value.triggered(slot).expect(TriggerAction.None)
        value.isLastInFtqEntry(slot).expect((instruction == fetch.instructions.last).B)
        val tag = enabled && fetch.context.get.notTrusted(instruction.pc)
        value.fdiNotTrusted.foreach(_(slot).expect(tag.B))
      }
    }

    private def checkDecode(value: CtrlFlow, item: Expected): Unit = {
      assert(!item.fetch.cancelled)
      val instruction = item.instruction
      assert(cycles > item.enqueued, "The registered IBuffer output bypassed its capture edge")
      value.pc.expect(instruction.pc.U)
      value.instr.expect(instruction.decoded.U)
      value.ftqPtr.value.expect(item.fetch.ptr.value.U)
      value.ftqPtr.flag.expect(item.fetch.ptr.flag.B)
      value.ftqOffset.expect(((instruction.pc - item.fetch.start) / 2).U)
      value.pd.isRVC.expect((instruction.bytes == 2).B)
      for ((bit, index) <- value.exceptionVec.zipWithIndex) {
        val expectedException = (instruction.exception == 1 && index == ExceptionNO.instrPageFault) ||
          (instruction.exception == 2 && index == ExceptionNO.instrGuestPageFault) ||
          (instruction.exception == 3 && index == ExceptionNO.instrAccessFault)
        bit.expect(expectedException.B)
      }
      value.crossPageIPFFix.expect(instruction.crossPage.B)
      value.backendException.expect(instruction.backendException.B)
      value.trigger.expect(TriggerAction.None)
      value.isLastInFtqEntry.expect((instruction == item.fetch.instructions.last).B)
      value.fdiNotTrusted.foreach(_.expect(item.tag.B))
    }

    def edge(): Unit = {
      if (bool(dut.io.ftq.redirect.valid)) cancel()
      val flushingBuffer = bool(dut.io.bufferFlush)
      if (flushingBuffer) {
        assert(!bool(dut.io.decodeCanAccept), "The redirect driver must block downstream consumption")
        expected.clear()
      }
      driveServices()
      val response = driveCache()
      if (bool(dut.io.ftq.req.valid) && bool(dut.io.ftq.req.ready)) {
        val fetch = offered.getOrElse(throw new AssertionError("Request fire has no independent description"))
        flights.get(fetch.ptr).foreach(old => assert(old.cancelled || old.consumed == old.instructions.size))
        assert(bool(dut.io.cacheReady))
        fetch.accepted = cycles
        flights(fetch.ptr) = fetch
        cache.enqueue(fetch)
        requests += 1
        event("request", fetch)
      }
      if (bool(dut.io.f2.fire) && !bool(dut.io.f2.flush)) {
        val fetch = flights(pointer(dut.io.f2.request.ftqIdx))
        assert(!fetch.cancelled)
        dut.io.f2.request.startAddr.expect(fetch.start.U)
        dut.io.f2.request.nextStartAddr.expect(fetch.end.U)
        assert(fetch.sampled < 0 && response.contains(fetch), "F2 consumed another request's cache response")
        fetch.sampled = cycles
        fetch.context = Some(context)
        if (fetch.mmio.nonEmpty) {
          assert(activeMmio.isEmpty)
          activeMmio = Some(fetch)
        }
        f2Transfers += 1
        event("f2-capture", fetch, s"\"configuration\":$configuration," +
          s"\"privilege\":${context.privilege},\"virtual\":${context.virtual}," +
          s"\"u_enable\":${context.uEnable},\"s_enable\":${context.sEnable}")
      }
      response.foreach { fetch =>
        assert(bool(dut.io.f2.fire) && !bool(dut.io.f2.flush),
          "The valid cache response was not consumed by its F2 transaction")
        assert(cache.dequeue() eq fetch)
        responses += 1
      }
      if (bool(dut.io.fetch.valid)) {
        val fetch = flights(pointer(dut.io.fetch.bits.ftqPtr))
        checkFetch(fetch)
        if (bool(dut.io.fetchReady) && !flushingBuffer) {
          fetch.instructions.foreach { instruction =>
            expected.enqueue(Expected(fetch, instruction, cycles, enabled && fetch.context.get.notTrusted(instruction.pc)))
          }
          fetch.received = cycles
          packets += 1
          if (bool(dut.io.ibufferState.bypass)) bypassPackets += 1 else storedPackets += 1
          event("ifu-ibuffer", fetch, s"\"instructions\":${fetch.instructions.size}," +
            s"\"bypass\":${bool(dut.io.ibufferState.bypass)}")
        } else if (!bool(dut.io.fetchReady)) heldFetchCycles += 1
      }
      if (!flushingBuffer && !bool(dut.io.ftq.redirect.valid)) {
        val validOutputs = dut.io.decode.filter(lane => bool(lane.valid))
        assert(validOutputs.size <= expected.size, "IBuffer produced an unowned instruction")
        for ((lane, item) <- validOutputs.zip(expected.toSeq)) checkDecode(lane.bits, item)
        for (lane <- validOutputs if bool(lane.ready)) {
          val item = expected.dequeue()
          delivered += 1
          item.fetch.consumed += 1
          event("ibuffer-consume", item.fetch,
            s"\"pc\":${item.instruction.pc},\"instruction\":${item.instruction.decoded}," +
              s"\"not_trusted\":${item.tag},\"exception\":${item.instruction.exception}," +
              s"\"cross_page\":${item.instruction.crossPage}")
        }
        if (validOutputs.nonEmpty && !bool(dut.io.decodeCanAccept)) heldDecodeCycles += 1
      }
      if (bool(dut.io.uncache.toUncache.valid) && bool(dut.io.uncache.toUncache.ready)) {
        val fetch = activeMmio.getOrElse(throw new AssertionError("Unowned Uncache request"))
        val spec = fetch.mmio.get
        val ordinal = fetch.uncacheRequests
        assert(ordinal < 2)
        dut.io.uncache.toUncache.bits.addr.expect((if (ordinal == 0) spec.physical else spec.translated).U)
        uncacheReplies.enqueue((fetch, ordinal, cycles + 1 + spec.responseDelay))
        fetch.uncacheRequests += 1
        event("uncache-request", fetch, s"\"ordinal\":$ordinal")
      }
      if (bool(dut.io.uncache.fromUncache.valid) && bool(dut.io.uncache.fromUncache.ready)) {
        val (fetch, ordinal, _) = uncacheReplies.dequeue()
        fetch.uncacheResponses += 1
        event("uncache-response", fetch, s"\"ordinal\":$ordinal")
      }
      if (bool(dut.io.tlb.req.valid) && bool(dut.io.tlb.req.ready)) {
        val fetch = activeMmio.getOrElse(throw new AssertionError("Unowned instruction TLB request"))
        assert(fetch.tlbRequests == 0 && fetch.uncacheResponses == 1)
        dut.io.tlb.req.bits.vaddr.expect((fetch.start + 2).U)
        dut.io.tlb.req.bits.cmd.expect(2.U)
        tlbReplies.enqueue((fetch, cycles + 2))
        fetch.tlbRequests += 1
        event("tlb-request", fetch, s"\"vaddr\":${fetch.start + 2}")
      }
      if (bool(dut.io.tlb.resp.valid) && bool(dut.io.tlb.resp.ready)) {
        val (fetch, _) = tlbReplies.dequeue()
        fetch.tlbResponses += 1
        event("tlb-response", fetch, s"\"exception\":${fetch.mmio.get.tlbException}")
      }
      if (bool(dut.io.pmp.req.valid)) {
        val fetch = activeMmio.getOrElse(throw new AssertionError("Unowned PMP request"))
        assert(fetch.pmpChecks == 0 && fetch.tlbResponses == 1)
        dut.io.pmp.req.bits.addr.expect(fetch.mmio.get.translated.U)
        fetch.pmpChecks += 1
        event("pmp-check", fetch, s"\"fault\":${fetch.mmio.get.pmpFault}")
      }
      activeMmio.foreach { fetch =>
        if (bool(dut.io.predecode.valid) && pointer(dut.io.predecode.bits.ftqIdx) == fetch.ptr) {
          dut.io.predecode.bits.misOffset.valid.expect(true.B)
          dut.io.predecode.bits.misOffset.bits.expect(0.U)
          dut.io.predecode.bits.target.expect((fetch.start + fetch.instructions.head.bytes).U)
          fetch.writebacks += 1
          event("mmio-writeback", fetch, s"\"target\":${uint(dut.io.predecode.bits.target)}")
        }
      }
      val buffer = dut.io.ibufferState
      def enqPosition: Int = uint(buffer.enqIndex).toInt + (if (bool(buffer.enqFlag)) dut.IBufSize else 0)
      def deqPosition: Int = uint(buffer.deqIndex).toInt + (if (bool(buffer.deqFlag)) dut.IBufSize else 0)
      val beforeEnq = enqPosition
      val beforeDeq = deqPosition
      val entriesBefore = uint(buffer.entries).toInt
      val actualEnqueue = uint(buffer.enqueueCount).toInt
      val enqueueOwner = if (actualEnqueue > 0) Some(pointer(dut.io.fetch.bits.ftqPtr)) else None
      dut.clock.step()
      cycles += 1
      // Compare the actual pointers around this edge. A flush/reset transition is
      // never carried into the next cycle as a synthetic natural wrap.
      if (flushingBuffer) {
        assert(enqPosition == 0 && deqPosition == 0)
      } else {
        val afterEnq = enqPosition
        val afterDeq = deqPosition
        val enqueueDelta = (afterEnq - beforeEnq + 2 * dut.IBufSize) % (2 * dut.IBufSize)
        val dequeueDelta = (afterDeq - beforeDeq + 2 * dut.IBufSize) % (2 * dut.IBufSize)
        assert(enqueueDelta == actualEnqueue)
        assert(dequeueDelta <= dut.DecodeWidth && dequeueDelta <= entriesBefore)
        if (enqueueDelta > 0) {
          val owner = enqueueOwner.get
          events += s"{\"cycle\":$cycles,\"event\":\"ibuffer-bank-enqueue\",\"count\":$enqueueDelta," +
            s"\"ftq\":${owner.value},\"ftq_flag\":${owner.flag},\"before\":$beforeEnq,\"after\":$afterEnq}"
        }
        if (dequeueDelta > 0) {
          events += s"{\"cycle\":$cycles,\"event\":\"ibuffer-bank-dequeue\",\"count\":$dequeueDelta," +
            s"\"entries_before\":$entriesBefore,\"before\":$beforeDeq,\"after\":$afterDeq}"
        }
        if (beforeEnq / dut.IBufSize != afterEnq / dut.IBufSize) {
          assert(enqueueDelta > 0)
          enqueueWraps += 1
          events += s"{\"cycle\":$cycles,\"event\":\"ibuffer-enqueue-wrap\",\"count\":$enqueueDelta}"
        }
        if (beforeDeq / dut.IBufSize != afterDeq / dut.IBufSize) {
          assert(dequeueDelta > 0)
          dequeueWraps += 1
          events += s"{\"cycle\":$cycles,\"event\":\"ibuffer-dequeue-wrap\",\"count\":$dequeueDelta}"
        }
      }
    }
    def offer(fetch: Fetch): Unit = {
      assert(offered.isEmpty)
      offered = Some(fetch)
      val request = dut.io.ftq.req.bits
      zero(request)
      request.startAddr.poke(fetch.start.U)
      request.nextlineStart.poke(((fetch.start & ~BigInt(63)) + 64).U)
      request.nextStartAddr.poke(fetch.end.U)
      request.ftqIdx.value.poke(fetch.ptr.value.U)
      request.ftqIdx.flag.poke(fetch.ptr.flag.B)
      dut.io.ftq.req.valid.poke(true.B)
    }
    def finishOffer(fetch: Fetch): Unit = {
      var wait = 0
      while (fetch.accepted < 0 && wait < 40) { edge(); wait += 1 }
      assert(fetch.accepted >= 0)
      dut.io.ftq.req.valid.poke(false.B)
      offered = None
    }
    def enqueue(fetch: Fetch): Unit = { offer(fetch); finishOffer(fetch) }
    def squashAtF0(fetch: Fetch, stage2: Boolean): Unit = {
      assert(cache.isEmpty && expected.isEmpty && activeMmio.isEmpty)
      offer(fetch)
      val flush = if (stage2) dut.io.ftq.flushFromBpu.s2 else dut.io.ftq.flushFromBpu.s3
      flush.bits.value.poke(fetch.ptr.value.U)
      flush.bits.flag.poke(fetch.ptr.flag.B)
      flush.valid.poke(true.B)
      finishOffer(fetch)
      flush.valid.poke(false.B)
      assert(cache.dequeue() eq fetch)
      fetch.cancelled = true
      cancellations += 1
      event("bpu-cancel", fetch, s"\"stage\":${if (stage2) 2 else 3}")
      idle(6)
      assert(fetch.sampled < 0 && fetch.received < 0 && fetch.consumed == 0)
      assert(!f2Valid && !f3Valid, "A BPU-cancelled request remained in the real IFU pipeline")
      dut.io.decode.foreach(_.valid.expect(false.B))
      event("bpu-cancel-pipeline-empty", fetch)
    }
    def redirect(owner: Fetch): Unit = {
      assert(offered.isEmpty)
      retainedOnRedirect = None
      consume(false)
      zero(dut.io.ftq.redirect.bits)
      dut.io.ftq.redirect.bits.ftqIdx.value.poke(owner.ptr.value.U)
      dut.io.ftq.redirect.bits.ftqIdx.flag.poke(owner.ptr.flag.B)
      dut.io.ftq.redirect.bits.level.poke(RedirectLevel.flush)
      dut.io.ftq.redirect.valid.poke(true.B)
      edge()
      dut.io.ftq.redirect.valid.poke(false.B)
      dut.io.bufferFlush.expect(true.B)
      edge()
      idle(3)
      assert(expected.isEmpty && cache.isEmpty && !f3Valid)
      dut.io.decode.foreach(_.valid.expect(false.B))
      consume(true)
    }
    def retainMmioAcrossRedirect(owner: Fetch): Unit = {
      assert(activeMmio.contains(owner) && owner.uncacheRequests == 0 && expected.isEmpty)
      retainedOnRedirect = Some(owner)
      consume(false)
      zero(dut.io.ftq.redirect.bits)
      dut.io.ftq.redirect.bits.ftqIdx.value.poke(owner.ptr.value.U)
      dut.io.ftq.redirect.bits.ftqIdx.flag.poke(owner.ptr.flag.B)
      dut.io.ftq.redirect.bits.level.poke(RedirectLevel.flushAfter)
      dut.io.ftq.redirect.valid.poke(true.B)
      edge()
      dut.io.ftq.redirect.valid.poke(false.B)
      idle(4)
      assert(f3Valid && !owner.cancelled && owner.received < 0)
      event("mmio-retained-after-redirect", owner)
      retainedOnRedirect = None
      consume(true)
    }
    def previousCommit(value: Boolean): Unit = dut.io.mmioLastCommit.poke(value.B)
    def commit(fetch: Fetch, wrongFlag: Boolean = false, offset: Int = 0): Unit = {
      assert(fetch.mmio.nonEmpty && fetch.consumed == fetch.instructions.size)
      val value = dut.io.commits(0)
      zero(value.bits)
      value.bits.ftqIdx.value.poke(fetch.ptr.value.U)
      value.bits.ftqIdx.flag.poke((fetch.ptr.flag ^ wrongFlag).B)
      value.bits.ftqOffset.poke(offset.U)
      value.valid.poke(true.B)
      edge()
      value.valid.poke(false.B)
      idle(3)
      if (wrongFlag || offset != 0) {
        assert(f3Valid && activeMmio.contains(fetch))
      } else {
        assert(!f3Valid)
        activeMmio = None
      }
    }
    def drain(count: Int): Unit = {
      var wait = 0
      while (delivered < count && wait < 300) { edge(); wait += 1 }
      assert(delivered == count && expected.isEmpty && cache.isEmpty)
      for (_ <- 0 until 5) edge()
    }
  }

  private def runFull(driver: Driver, dut: FDITrustMetadataFrontendHarness): Unit = {
    require(dut.PredictWidth == 16 && dut.VAddrBits >= 48)
    var serial = 1
    def packet(start: BigInt, instructions: Seq[Instruction], end: BigInt,
      image: Map[BigInt, Int] = Map.empty, exceptions: Seq[Int] = Seq(0, 0),
      mmio: Option[Mmio] = None, backendException: Boolean = false): Fetch = {
      val ptr = Pointer((serial / dut.FtqSize) % 2 != 0, serial % dut.FtqSize)
      serial += 1
      val bytes = if (image.nonEmpty) image else
        FDITrustInstructionImage.bytes(instructions.map(i => (i.pc, i.word, i.bytes)))
      new Fetch(ptr, start, end, instructions,
        FDITrustInstructionImage.cacheWord(start, bytes, 64, dut.PredictWidth), exceptions, mmio, backendException)
    }
    def rvi(start: BigInt): Fetch = packet(start,
      (0 until 8).map(i => Instruction(start + 4 * i, FDITrustInstructionImage.addi(5, i + 1), 4)), start + 32)
    def deliver(fetch: Fetch): Unit = {
      val goal = driver.delivered + fetch.instructions.size
      driver.enqueue(fetch)
      driver.drain(goal)
    }
    def uniform(name: String, cfg: Context, answer: Boolean): Unit = {
      driver.mark(name)
      val fetch = rvi(0x4000 + serial * 64)
      assert(fetch.instructions.forall(i => cfg.notTrusted(i.pc) == answer), "Independent contract check failed")
      driver.configure(cfg)
      deliver(fetch)
    }

    for (privilege <- Seq(0, 1); virtual <- Seq(false, true); active <- Seq(false, true)) {
      val cfg = Context(privilege, virtual, sEnable = if (privilege == 1) active else !active,
        uEnable = if (privilege == 0) active else !active,
        sLo = 0, sHi = 0x100000, uLo = 0, uHi = 0x100000)
      uniform(s"source-$privilege-virtual-$virtual-enable-$active", cfg, active && virtual)
    }
    uniform("machine-bypass", Context(3, sEnable = true, uEnable = true), false)
    uniform("machine-virtual-fail-closed", Context(3, virtual = true), true)
    uniform("reserved-source-fail-closed", Context(2), true)
    uniform("reserved-virtual-source-fail-closed", Context(2, virtual = true), true)
    uniform("empty-supervisor-window", Context(1, sEnable = true, sLo = 0x4000, sHi = 0x4000), true)
    uniform("reversed-user-window", Context(0, uEnable = true, uLo = 0x8000, uHi = 0x4000), true)

    val signed39 = (BigInt(1) << 38) + 0x1000
    val signed48 = (BigInt(1) << 47) + 0x1000
    for ((name, pc, mode39, mode48) <- Seq(
      ("bare-zero-extension", signed48, false, false),
      ("sv39-sign-extension", signed39, true, false),
      ("sv48-sign-extension", signed48, false, true))) {
      driver.mark(name)
      val full = if (mode39) BigInt("ffffffc000001000", 16)
        else if (mode48) BigInt("ffff800000001000", 16) else pc
      val cfg = Context(1, sEnable = true, sLo = full, sHi = full + 16, sv39 = mode39, sv48 = mode48)
      assert(cfg.fullPC(pc) == full)
      assert((0 until 8).map(i => cfg.notTrusted(pc + i * 4)) == Seq.fill(4)(false) ++ Seq.fill(4)(true))
      driver.configure(cfg)
      deliver(rvi(pc))
    }

    driver.mark("mixed-rvc-rvi-bank-and-line-boundaries")
    val mixedStart = BigInt(0x10f8)
    var mixedPC = mixedStart
    val mixed = Seq(2, 4, 2, 4, 4, 2, 4, 2, 4, 4).zipWithIndex.map { case (size, index) =>
      val item = Instruction(mixedPC, if (size == 2) BigInt(1) else FDITrustInstructionImage.addi(6, index + 1), size)
      mixedPC += size
      item
    }
    driver.configure(Context(0, uEnable = true, uLo = mixedStart + 6, uHi = mixedStart + 22))
    deliver(packet(mixedStart, mixed, mixedStart + 32))

    driver.mark("rvc-every-slot-and-halfword-window-endpoints")
    val rvcStart = BigInt(0x1200)
    driver.configure(Context(0, uEnable = true, uLo = rvcStart + 8, uHi = rvcStart + 20))
    deliver(packet(rvcStart, (0 until 16).map(i => Instruction(rvcStart + 2 * i, 1, 2)), rvcStart + 32))
    for (slot <- 0 until 15) {
      driver.mark(s"rvi-start-slot-$slot")
      val start = BigInt(0x1400) + slot * 64
      val instructions = (0 until 16).filter(_ != slot + 1).map { index =>
        Instruction(start + 2 * index, if (index == slot) FDITrustInstructionImage.addi(2, slot + 1) else BigInt(1),
          if (index == slot) 4 else 2)
      }
      driver.configure(Context(1, sEnable = true, sLo = start + 2 * slot, sHi = start + 2 * slot + 2))
      deliver(packet(start, instructions, start + 32))
    }

    driver.mark("last-half-rvi-original-start-and-next-packet-mask")
    val halfStart = BigInt(0x2fe0)
    val first = (0 until 15).map(i => Instruction(halfStart + 2 * i, 1, 2)) :+
      Instruction(halfStart + 30, FDITrustInstructionImage.addi(1, 1), 4)
    val second = (1 until 16).map(i => Instruction(halfStart + 32 + 2 * i, 1, 2))
    val halfImage = FDITrustInstructionImage.bytes((first ++ second).map(i => (i.pc, i.word, i.bytes)))
    val firstFetch = packet(halfStart, first, halfStart + 32, halfImage)
    val secondFetch = packet(halfStart + 32, second, halfStart + 64, halfImage)
    val halfGoal = driver.delivered + first.size + second.size
    driver.configure(Context(1, sEnable = true, sLo = halfStart, sHi = halfStart + 32))
    driver.enqueue(firstFetch)
    driver.enqueue(secondFetch)
    driver.drain(halfGoal)

    driver.mark("redirect-clears-last-half-mask")
    val halfKillStart = BigInt(0x30fe0)
    val halfKillInstructions = first.map(i => i.copy(pc = i.pc - halfStart + halfKillStart))
    val halfKill = packet(halfKillStart, halfKillInstructions, halfKillStart + 32)
    driver.consume(false)
    driver.enqueue(halfKill)
    driver.waitFor(halfKill.received >= 0)
    driver.redirect(halfKill)
    val afterHalfStart = BigInt(0x32000)
    deliver(packet(afterHalfStart, (0 until 16).map(i => Instruction(afterHalfStart + 2 * i, 1, 2)),
      afterHalfStart + 32))

    driver.mark("request-backpressure-and-f2-config-capture")
    val waiting = rvi(0x6000)
    driver.configure(Context(3))
    driver.dut.io.cacheReady.poke(false.B)
    driver.offer(waiting)
    driver.idle(4)
    assert(waiting.accepted < 0)
    driver.dut.io.cacheReady.poke(true.B)
    driver.cacheHold = true
    driver.finishOffer(waiting)
    driver.waitFor(driver.f2Valid)
    driver.idle(3)
    assert(waiting.sampled < 0)
    driver.configure(Context(1, sEnable = true))
    driver.consume(false)
    driver.cacheHold = false
    driver.waitFor(waiting.sampled >= 0)
    driver.configure(Context(3))
    driver.idle(7)
    assert(waiting.received >= 0 && waiting.consumed == 0)
    driver.consume(true)
    driver.drain(driver.delivered + waiting.instructions.size)

    driver.mark("partial-output-fill-and-compaction")
    driver.consume(false)
    val shortStart = BigInt(0x6800)
    val shortA = packet(shortStart, (0 until 3).map(i => Instruction(shortStart + 2 * i, 1, 2)), shortStart + 6)
    val shortB = packet(shortStart + 6, (0 until 2).map(i => Instruction(shortStart + 6 + 2 * i, 1, 2)), shortStart + 10)
    val shortGoal = driver.delivered + 5
    driver.configure(Context(1, sEnable = true, sLo = shortStart, sHi = shortStart + 6))
    driver.enqueue(shortA)
    driver.waitFor(shortA.received >= 0)
    driver.idle(3)
    driver.enqueue(shortB)
    driver.waitFor(shortB.received >= 0)
    driver.idle(5)
    driver.consume(true)
    driver.drain(shortGoal)

    def fillHeld(base: BigInt): Seq[Fetch] = {
      driver.consume(false)
      val held = mutable.ArrayBuffer.empty[Fetch]
      var index = 0
      while (!driver.full && index < dut.IBufSize / 8 + 4) {
        val fetch = rvi(base + index * 64)
        held += fetch
        driver.enqueue(fetch)
        driver.waitFor(fetch.received >= 0)
        driver.idle(1)
        index += 1
      }
      assert(driver.full)
      for (_ <- 0 until 2) {
        val fetch = rvi(base + index * 64)
        held += fetch
        driver.enqueue(fetch)
        index += 1
      }
      driver.idle(5)
      assert(driver.stopped && driver.f3Valid && held.last.sampled < 0)
      assert(held(held.size - 2).sampled >= 0 && held(held.size - 2).received < 0)
      held.toSeq
    }
    driver.mark("ibuffer-full-f3-hold-f2-stop-and-release")
    driver.configure(Context(1, sEnable = true))
    val held = fillHeld(0x8000)
    val holdGoal = driver.delivered + held.map(_.instructions.size).sum
    val priorHold = driver.heldFetchCycles
    for (index <- 0 until 7) {
      driver.configure(if (index % 2 == 0) Context(3) else Context(0, uEnable = true))
      driver.edge()
    }
    assert(driver.heldFetchCycles >= priorHold + 7)
    driver.configure(Context(3))
    driver.consume(true)
    driver.drain(holdGoal)

    driver.mark("ibuffer-ring-wrap-continuous-transactions")
    val enqueueWrapsBefore = driver.enqueueWraps
    val dequeueWrapsBefore = driver.dequeueWraps
    val wrapGoal = driver.delivered + (dut.IBufSize / 8 * 3 + 4) * 8
    for (index <- 0 until dut.IBufSize / 8 * 3 + 4) {
      driver.configure(if (index % 2 == 0) Context(3) else Context(1, sEnable = true))
      driver.enqueue(rvi(0xa000 + index * 64))
    }
    driver.drain(wrapGoal)
    assert(driver.enqueueWraps - enqueueWrapsBefore >= 2 && driver.dequeueWraps - dequeueWrapsBefore >= 2)

    driver.mark("redirect-cancels-buffer-f3-and-f2")
    driver.configure(Context(1, sEnable = true))
    val cancelled = fillHeld(0xc000)
    val beforeCancel = driver.delivered
    driver.redirect(cancelled.head)
    assert(driver.delivered == beforeCancel && cancelled.forall(_.cancelled))
    driver.configure(Context(3))
    deliver(rvi(0xe000))

    for (stage2 <- Seq(true, false)) {
      driver.mark(s"bpu-stage-${if (stage2) 2 else 3}-f0-cancel")
      driver.squashAtF0(rvi(if (stage2) 0xe040 else 0xe080), stage2)
      // No reset or backend redirect separates the cancellation from this progress proof.
      deliver(rvi(if (stage2) 0xe060 else 0xe0a0))
    }

    driver.mark("reset-clears-pending-f2")
    driver.cacheHold = true
    val resetF2 = rvi(0xe100)
    driver.enqueue(resetF2)
    driver.waitFor(driver.f2Valid)
    driver.reset()
    assert(resetF2.cancelled && resetF2.sampled < 0)
    deliver(rvi(0xe200))
    driver.mark("reset-clears-buffered-output-and-held-f3")
    val resetBuffered = fillHeld(0x33000)
    driver.reset()
    assert(resetBuffered.forall(f => f.cancelled && f.consumed == 0))
    deliver(rvi(0xe400))

    for (exception <- 1 to 3) {
      driver.mark(s"cache-cross-page-exception-$exception")
      val start = BigInt(0x11ffe) + (exception - 1) * 0x2000
      val instructions = Seq(Instruction(start, FDITrustInstructionImage.addi(1, 1), 4,
        exception = exception, crossPage = true)) ++
        (0 until 14).map(i => Instruction(start + 4 + 2 * i, 1, 2, exception = exception))
      driver.configure(Context(1, sEnable = true, sLo = start, sHi = start + 2))
      val fetch = packet(start, instructions, start + 32, exceptions = Seq(0, exception))
      deliver(fetch)
      driver.redirect(fetch)
    }
    driver.mark("backend-exception-metadata")
    val backStart = BigInt(0x18000)
    val backInstructions = (0 until 8).map(i => Instruction(backStart + i * 4,
      FDITrustInstructionImage.addi(1, i + 1), 4, backendException = i == 0))
    val backFetch = packet(backStart, backInstructions, backStart + 32, backendException = true)
    deliver(backFetch)
    driver.redirect(backFetch)

    def mmioCase(name: String, start: BigInt, compressed: Boolean = false,
      tlbException: Int = 0, pmpFault: Boolean = false, waitPrevious: Boolean = false,
      retainedRedirect: Boolean = false): Unit = {
      driver.mark(name)
      val crossing = (start & 7) == 6 && !compressed
      val fault = if (tlbException != 0) tlbException else if (pmpFault) 3 else 0
      val word = if (compressed) BigInt(1) else if (fault != 0) BigInt(0x93) else BigInt(0x00100093)
      val instruction = Instruction(start, word, if (compressed) 2 else 4,
        exception = fault, crossPage = crossing && fault != 0)
      val fetch = packet(start, Seq(instruction), start + 32,
        mmio = Some(Mmio(start + 0x80000, tlbException = tlbException, pmpFault = pmpFault)))
      driver.configure(Context(1, sEnable = true, sLo = start, sHi = start + 2))
      driver.previousCommit(!waitPrevious)
      driver.enqueue(fetch)
      driver.waitFor(fetch.sampled >= 0)
      driver.configure(Context(0, uEnable = true))
      if (waitPrevious) {
        driver.idle(6)
        assert(fetch.uncacheRequests == 0 && fetch.received < 0)
        val previous = (fetch.ptr.value + dut.FtqSize - 1) % dut.FtqSize
        driver.dut.io.mmioCommitPointer.value.expect(previous.U)
        driver.dut.io.mmioCommitPointer.flag.expect((fetch.ptr.flag ^ (fetch.ptr.value == 0)).B)
        if (retainedRedirect) driver.retainMmioAcrossRedirect(fetch)
        driver.previousCommit(true)
      }
      driver.drain(driver.delivered + 1)
      val expectedRequests = if (crossing && fault == 0) 2 else 1
      assert(fetch.uncacheRequests == expectedRequests && fetch.uncacheResponses == expectedRequests)
      assert(fetch.tlbRequests == (if (crossing) 1 else 0) && fetch.tlbResponses == fetch.tlbRequests)
      assert(fetch.pmpChecks == (if (crossing && tlbException == 0) 1 else 0))
      assert(fetch.writebacks == (if (fault == 0 && !retainedRedirect) 1 else 0))
      if (fault != 0) driver.redirect(fetch)
      else {
        driver.commit(fetch, wrongFlag = true)
        driver.commit(fetch, offset = 1)
        driver.commit(fetch)
      }
    }
    mmioCase("mmio-rvi-previous-and-own-commit-gates", 0x20000, waitPrevious = true)
    mmioCase("mmio-rvc-original-tag", 0x20040, compressed = true)
    mmioCase("mmio-preserved-same-owner-flush-after", 0x20080, waitPrevious = true, retainedRedirect = true)
    mmioCase("mmio-page-end-resend-original-owner", 0x21ffe)
    mmioCase("mmio-page-end-tlb-pf", 0x23ffe, tlbException = 1)
    mmioCase("mmio-page-end-tlb-gpf", 0x25ffe, tlbException = 2)
    mmioCase("mmio-page-end-pmp-af", 0x27ffe, pmpFault = true)
    driver.mark("mmio-reset-with-outstanding-external-response")
    val resetMmioStart = BigInt(0x29000)
    val resetMmio = packet(resetMmioStart, Seq(Instruction(resetMmioStart, 0x00100093, 4)),
      resetMmioStart + 32, mmio = Some(Mmio(resetMmioStart + 0x80000, responseDelay = 20)))
    driver.configure(Context(1, sEnable = true))
    driver.previousCommit(true)
    driver.enqueue(resetMmio)
    driver.waitFor(resetMmio.uncacheRequests == 1)
    assert(resetMmio.uncacheResponses == 0 && resetMmio.received < 0)
    driver.reset()
    assert(resetMmio.cancelled)
    deliver(rvi(0x2a000))
    assert(driver.heldFetchCycles > 0 && driver.heldDecodeCycles > 0)
    assert(driver.bypassPackets > 0 && driver.storedPackets > 0 && driver.cancellations > 0)
    assert(driver.pending == 0)
    assert(driver.cases.size == 57)
  }

  it should "carry independently classified raw instructions through the production IFU and IBuffer" in {
    val root = Paths.get(sys.props("c08.runRoot")).toRealPath()
    require(Paths.get("").toRealPath() == root)
    val (base, _, _) = top.ArgParser.parse(Array("--config", "FpgaDefaultConfig", "--num-cores", "1",
      "--l2-cache-size", "256", "--l3-cache-size", "768", "--fpga-platform",
      "--disable-always-basic-diff", "--disable-perf", "--disable-alwaysdb"))
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled)
    }
    val options = p(DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    require(p(XSCoreParamsKey).HasVPU && p(XSCoreParamsKey).VLEN == 128)
    test(new FDITrustMetadataFrontendHarness).withAnnotations(Seq(VerilatorBackendAnnotation,
      TargetDirAnnotation(s"frontend-$scenario-${if (enabled) "on" else "off"}"))) { dut =>
      val driver = new Driver(dut)
      try {
        driver.reset()
        val start = BigInt(0x1000)
        val count = dut.PredictWidth / 2
        val instructions = (0 until count).map(i => Instruction(start + 4 * i, FDITrustInstructionImage.addi(5, i + 1), 4))
        val image = FDITrustInstructionImage.bytes(instructions.map(i => (i.pc, i.word, i.bytes)))
        val data = FDITrustInstructionImage.cacheWord(start, image, 64, dut.PredictWidth)
        driver.configure(Context(1, sEnable = true, sLo = start, sHi = start + 16))
        driver.enqueue(new Fetch(Pointer(false, 0), start, start + 2 * dut.PredictWidth, instructions, data))
        driver.drain(count)
        assert(driver.requests == 1 && driver.responses == 1 && driver.f2Transfers == 1 && driver.packets == 1)
        if (scenario == "full") runFull(driver, dut)
        println(s"C08_FRONTEND_PASS scenario=$scenario enabled=$enabled cycles=${driver.cycles} " +
          s"requests=${driver.requests} f2=${driver.f2Transfers} packets=${driver.packets} instructions=${driver.delivered} " +
          s"cases=${driver.cases.size} " +
          s"fetchHeld=${driver.heldFetchCycles} decodeHeld=${driver.heldDecodeCycles} " +
          s"bypass=${driver.bypassPackets} stored=${driver.storedPackets} " +
          s"enqWraps=${driver.enqueueWraps} deqWraps=${driver.dequeueWraps} cancelled=${driver.cancellations} " +
          "ifu=production ibuffer=production cache=external-protocol-fixture")
      } finally {
        Files.write(root.resolve("frontend-events.jsonl"), driver.events.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
      }
    }
  }
}
