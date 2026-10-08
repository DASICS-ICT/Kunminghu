// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.mem

import chisel3._
import chisel3.reflect.DataMirror
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
import xiangshan.backend.Bundles.DynInst
import xiangshan.backend.fu.FuType
import xiangshan.cache.MemoryOpConstants

class FDIStorePermissionTest extends AnyFlatSpec {
  behavior of "Ordinary store permission through real store publication"

  private val enabled = sys.env.getOrElse("L03_FDI_ENABLED",
    throw new IllegalArgumentException("L03_FDI_ENABLED must be explicit")).toBoolean
  private val scenario = sys.env.getOrElse("L03_SCENARIO", "minimal")
  require(Set("minimal", "full", "remaining").contains(scenario), "Choose an explicit supported store scenario")

  private def parameters(): Parameters = {
    val (base, _, _) = top.ArgParser.parse(Array(
      "--config", "FpgaDefaultConfig", "--num-cores", "1",
      "--l2-cache-size", "256", "--l3-cache-size", "768",
      "--fpga-platform", "--disable-always-basic-diff", "--disable-perf", "--disable-alwaysdb"))
    val core = base(XSTileKey).head.copy(HasFDI = enabled)
    require(core.HasVPU && core.VLEN == 128)
    base.alterPartial { case XSCoreParamsKey => core }
  }

  private case class Source(name: String, address: BigInt, data: BigInt, rob: Int, lane: Int) {
    // The oracle uses the complete original SD access, independently of any DUT decision.
    val denied: Boolean = enabled && !(address >= 0x8000 && address + 8 <= 0x8008)
  }
  private val sources = Seq(
    Source("allowed", 0x8000, BigInt("0123456789abcdef", 16), 4, 0),
    Source("outside-bound", 0x8040, BigInt("fedcba9876543210", 16), 5, 1),
    Source("following", 0x8000, BigInt("a1b2c3d4e5f6071", 16), 6, 0))
  private case class Pointer(flag: Boolean, value: Int)

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
  private def zeroInputs(data: Data): Unit = data match {
    case value: Vec[_] => value.foreach(zeroInputs)
    case value: Record => value.elements.values.foreach(zeroInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => zero(leaf)
    case _ =>
  }
  private def values(data: Data): Vector[BigInt] = data match {
    case value: UInt => Vector(uint(value))
    case value: SInt => Vector(value.peek().litValue)
    case value: Vec[_] => value.toVector.flatMap(values)
    case value: Record => value.elements.values.toVector.flatMap(values)
    case other => throw new IllegalArgumentException(s"Unsupported observation ${other.getClass.getName}")
  }
  private def exceptions(value: Vec[Bool]): Set[Int] = value.zipWithIndex.collect {
    case (bit, index) if bool(bit) => index
  }.toSet
  private def escaped(value: String): String = value.flatMap {
    case '\\' => "\\\\"
    case '"' => "\\\""
    case '\n' => "\\n"
    case '\r' => "\\r"
    case c => c.toString
  }

  private class Driver(dut: FDIStorePermissionHarness, events: mutable.ArrayBuffer[String]) {
    private var cycle = 0
    private var active: Option[(Source, Pointer)] = None
    private var issueCycle = -1
    private var permissionTag: Option[BigInt] = None
    private var permissionRequests = 0
    private var permissionResponses = 0
    private var wbCount = 0
    private var dataWbCount = 0
    private var publications = 0
    private var emptyTokens = 0
    private var writeCount = 0
    private var dequeues = 0
    private val metadataResponses = Seq.fill(dut.io.metadata.length)(mutable.Queue.empty[Int])
    private val cacheResponses = mutable.Queue.empty[(Int, BigInt)]
    private val expectedBytes = mutable.Map.empty[BigInt, Int]
    private val sentLines = mutable.Set.empty[BigInt]
    private val memory = mutable.Map.empty[BigInt, Int]
    private val sentinel = 0x5a
    private var deniedSnapshot: Option[Vector[BigInt]] = None

    private def log(kind: String, fields: String = ""): Unit =
      events += s"{\"cycle\":$cycle,\"event\":\"$kind\"${if (fields.isEmpty) "" else "," + fields}}"
    private def sourceFields(source: Source, pointer: Pointer): String =
      s"\"source\":\"${source.name}\",\"rob\":${source.rob},\"sq_flag\":${pointer.flag},\"sq\":${pointer.value}"
    private def stateSnapshot(): Vector[BigInt] =
      dut.io.bufferState.toVector.map(s => if (bool(s.state_valid)) BigInt(1) else BigInt(0)) ++
        values(dut.io.bufferPhysicalTags) ++ values(dut.io.bufferVirtualTags) ++
        values(dut.io.bufferData) ++ values(dut.io.bufferMask)
    private def checkOwner(uop: DynInst, source: Source, pointer: Pointer): Unit = {
      uop.robIdx.flag.expect(false.B)
      uop.robIdx.value.expect(source.rob.U)
      uop.sqIdx.flag.expect(pointer.flag.B)
      uop.sqIdx.value.expect(pointer.value.U)
      uop.uopIdx.expect(0.U)
    }
    private def driveOwner(uop: DynInst, source: Source, pointer: Pointer): Unit = {
      zero(uop)
      uop.instr.poke(0x00113023.U)
      uop.pc.poke((0x1000 + 4 * source.rob).U)
      uop.fuType.poke(FuType.stu.U)
      uop.fuOpType.poke(3.U)
      uop.robIdx.flag.poke(false.B)
      uop.robIdx.value.poke(source.rob.U)
      uop.sqIdx.flag.poke(pointer.flag.B)
      uop.sqIdx.value.poke(pointer.value.U)
      uop.ftqPtr.value.poke(1.U)
      uop.ftqOffset.poke(0.U)
      uop.firstUop.poke(true.B)
      uop.lastUop.poke(true.B)
      uop.numLsElem.poke(1.U)
      uop.fdiNotTrusted.foreach(_.poke(true.B))
    }

    private def tick(phaseName: String): Unit = {
      for ((port, lane) <- dut.io.metadata.zipWithIndex) {
        port.resp.valid.poke(false.B)
        if (metadataResponses(lane).headOption.contains(cycle)) {
          metadataResponses(lane).dequeue()
          port.resp.valid.poke(true.B)
          log("metadata-response", s"\"lane\":$lane")
        }
      }
      dut.io.cacheWrite.main_pipe_hit_resp.valid.poke(false.B)
      if (cacheResponses.headOption.exists(_._1 == cycle)) {
        val (_, id) = cacheResponses.dequeue()
        dut.io.cacheWrite.main_pipe_hit_resp.bits.id.poke(id.U)
        dut.io.cacheWrite.main_pipe_hit_resp.valid.poke(true.B)
        log("cache-ack", s"\"id\":$id")
      }
      // Bare translation makes the original source address the physical address.
      // A previously allowed line may remain pending while the sink is blocked.
      if (bool(dut.io.cacheWrite.req.valid)) {
        active.filter(_._1.denied).foreach { case (source, pointer) =>
          val lineBytes = dut.io.cacheWrite.req.bits.mask.getWidth
          val deniedLine = source.address - source.address % lineBytes
          val requestedLine = uint(dut.io.cacheWrite.req.bits.addr)
          log("cache-request-during-denial", sourceFields(source, pointer) +
            s",\"address\":$requestedLine,\"denied_line\":$deniedLine,\"ready\":${bool(dut.io.cacheWrite.req.ready)}")
          assert(requestedLine != deniedLine, "Denied store offered a cache request even without sink acceptance")
        }
      }
      assert(!bool(dut.io.uncache.req.valid), "Cacheable ordinary stores must not publish an uncache request")
      assert(!bool(dut.io.splitRequest.valid), "Aligned SD must not create a misalignment fragment")
      dut.io.ptw.req.foreach(p => assert(!bool(p.valid), "Bare translation must not request a page-table walk"))
      for ((port, lane) <- dut.io.metadata.zipWithIndex) {
        if (bool(port.req.valid) && bool(port.req.ready)) {
          port.req.bits.cmd.expect(MemoryOpConstants.M_PFW)
          val source = active.get._1
          port.req.bits.vaddr.expect(source.address.U)
          assert(source.lane == lane, "Metadata request appeared on the wrong store lane")
          metadataResponses(lane).enqueue(cycle + 2)
          log("metadata-request", s"\"lane\":$lane,\"address\":${source.address}")
        }
      }
      for ((phase, lane) <- dut.io.phases.zipWithIndex) {
        if (bool(phase.s0)) {
          assert(active.exists(_._1.lane == lane))
          assert(issueCycle < 0, "A store was accepted twice")
          issueCycle = cycle
          log("store-S0", s"\"lane\":$lane")
        }
        if (bool(phase.primary.valid)) {
          val (source, pointer) = active.get
          checkOwner(phase.primary.bits.uop, source, pointer)
          phase.primary.bits.fullva.expect(source.address.U)
          assert(cycle == issueCycle + 1, "Bare store S1 must follow its own S0")
          log("store-S1", sourceFields(source, pointer))
        }
        if (bool(phase.permissionRequest.valid)) {
          val (source, pointer) = active.get
          val request = phase.permissionRequest.bits
          request.address.expect(source.address.U)
          request.sizeLog2.expect(3.U)
          request.operation.expect(1.U)
          request.sourcePrivilege.expect(0.U)
          request.sourceVirtual.expect(false.B)
          request.notTrusted.expect(true.B)
          assert(cycle == issueCycle + 1)
          permissionTag = Some(uint(request.tag))
          permissionRequests += 1
          log("permission-request", sourceFields(source, pointer) + s",\"tag\":${uint(request.tag)}")
        }
        if (bool(phase.permissionResponse.valid)) {
          val (source, pointer) = active.get
          val response = phase.permissionResponse.bits
          assert(permissionTag.contains(uint(response.request.tag)), "Permission response changed transaction identity")
          response.request.address.expect(source.address.U)
          response.outcome.expect((if (source.denied) 1 else 0).U)
          response.reason.expect((if (source.denied) 3 else 0).U)
          phase.permissionConsumed.expect(true.B)
          assert(cycle == issueCycle + 2)
          permissionResponses += 1
          log("permission-response", sourceFields(source, pointer) + s",\"denied\":${source.denied}")
        }
        if (bool(phase.s2)) {
          val (source, pointer) = active.get
          checkOwner(phase.supplement.uop, source, pointer)
          phase.supplement.fullva.expect(source.address.U)
          phase.pmpFault.expect(false.B)
          assert(cycle == issueCycle + 2)
          log("store-S2", sourceFields(source, pointer))
        }
        if (bool(dut.io.dataWb(lane).valid)) {
          val (source, pointer) = active.get
          dut.io.dataWb(lane).bits.uop.robIdx.value.expect(source.rob.U)
          dut.io.dataWb(lane).bits.uop.sqIdx.flag.expect(pointer.flag.B)
          dut.io.dataWb(lane).bits.uop.sqIdx.value.expect(pointer.value.U)
          dut.io.dataWb(lane).bits.data.expect(source.data.U)
          dataWbCount += 1
          log("store-data-writeback", sourceFields(source, pointer))
        }
        if (bool(dut.io.wb(lane).valid)) {
          val (source, pointer) = active.get
          val uop = dut.io.wb(lane).bits.uop
          checkOwner(uop, source, pointer)
          val actual = exceptions(uop.exceptionVec)
          assert(actual == (if (source.denied) Set(24) else Set.empty[Int]), s"Unexpected store exceptions $actual")
          if (source.denied) {
            uop.fdiException.get.tval.expect(source.address.U)
            uop.fdiException.get.reason.expect(3.U)
          }
          wbCount += 1
          log("store-writeback", sourceFields(source, pointer) + s",\"exceptions\":[${actual.toSeq.sorted.mkString(",")}]")
        }
      }
      for ((publication, lane) <- dut.io.publication.zipWithIndex) {
        if (bool(publication.fire)) {
          val (source, pointer) = active.get
          publication.bits.addr.expect(source.address.U)
          publication.bits.vecValid.expect((!source.denied).B)
          publication.bits.sqNeedDeq.expect(true.B)
          if (source.denied) emptyTokens += 1 else {
            publication.bits.mask.expect(0xff.U)
            assert((uint(publication.bits.data) & ((BigInt(1) << 64) - 1)) == source.data)
            for (byte <- 0 until 8) expectedBytes(source.address + byte) = ((source.data >> (8 * byte)) & 255).toInt
            publications += 1
          }
          log("sbuffer-publication", sourceFields(source, pointer) +
            s",\"lane\":$lane,\"vec_valid\":${bool(publication.bits.vecValid)},\"mask\":${uint(publication.bits.mask)}")
        }
        val write = dut.io.writes(lane)
        assert(bool(write.valid) == (bool(publication.fire) && bool(publication.bits.vecValid)),
          "Sbuffer byte write must correspond exactly to a nonempty accepted publication")
        if (bool(write.valid)) {
          val source = active.get._1
          assert(!source.denied)
          write.bits.mask.expect(0xff.U)
          assert((uint(write.bits.data) & ((BigInt(1) << 64) - 1)) == source.data)
          assert(uint(write.bits.wvec).bitCount == 1, "A word update must target exactly one Sbuffer line")
          writeCount += 1
          log("sbuffer-data-write", s"\"lane\":$lane,\"wvec\":${uint(write.bits.wvec)},\"mask\":${uint(write.bits.mask)}")
        }
      }
      val dequeue = uint(dut.io.queueDequeue).toInt
      if (dequeue != 0) {
        dequeues += dequeue
        log("sq-dequeue", s"\"count\":$dequeue")
      }
      if (bool(dut.io.cacheWrite.req.valid) && bool(dut.io.cacheWrite.req.ready)) {
        assert(bool(dut.io.flushBuffer), "Minimal stores must publish to cache only during the requested flush")
        val request = dut.io.cacheWrite.req.bits
        request.cmd.expect(MemoryOpConstants.M_XWR)
        val address = uint(request.addr)
        val lineBytes = request.mask.getWidth
        assert(address % lineBytes == 0)
        assert(!sentLines(address), "A cache line was sent twice without a replay")
        val expected = expectedBytes.filter { case (byte, _) => byte >= address && byte < address + lineBytes }
        assert(expected.nonEmpty, s"Unexpected cache line $address")
        val expectedMask = expected.keys.foldLeft(BigInt(0))((mask, byte) => mask.setBit((byte - address).toInt))
        request.mask.expect(expectedMask.U)
        for ((byte, value) <- expected) {
          val actual = ((uint(request.data) >> (8 * (byte - address).toInt)) & 255).toInt
          assert(actual == value, s"Cache publication corrupted byte $byte")
          memory(byte) = actual
        }
        sentLines += address
        cacheResponses.enqueue((cycle + 3, uint(request.id)))
        log("cache-line-write", s"\"address\":$address,\"mask\":${uint(request.mask)},\"id\":${uint(request.id)}")
      }
      deniedSnapshot.foreach(snapshot => assert(stateSnapshot() == snapshot,
        s"Denied store changed Sbuffer payload or ownership in $phaseName before cycle $cycle"))
      log("cycle", s"\"phase\":\"$phaseName\"")
      dut.clock.step()
      cycle += 1
      deniedSnapshot.foreach(snapshot => assert(stateSnapshot() == snapshot,
        s"Denied store changed Sbuffer payload or ownership in $phaseName after cycle $cycle"))
    }

    private def until(label: String, limit: Int = 128)(done: => Boolean): Unit = {
      var elapsed = 0
      while (!done && elapsed < limit) { tick(label); elapsed += 1 }
      assert(done, s"Timed out waiting for $label at cycle $cycle")
    }

    def reset(): Unit = {
      zeroInputs(dut.io)
      dut.io.enq.lqCanAccept.poke(true.B)
      dut.io.metadata.foreach(_.req.ready.poke(true.B))
      dut.io.ptw.req.foreach(_.ready.poke(true.B))
      dut.io.uncache.req.ready.poke(true.B)
      dut.io.cacheWrite.req.ready.poke(false.B)
      dut.io.pmpEnvironment.cmode.poke(true.B)
      dut.io.pmpEnvironment.mode.poke(0.U)
      for (entries <- Seq(dut.io.pmpEnvironment.pmp, dut.io.pmpEnvironment.pma)) {
        val first = entries.head
        first.cfg.a.poke(1.U)
        first.addr.poke(0x40000.U)
        first.cfg.r.poke(true.B)
        first.cfg.w.poke(true.B)
        first.cfg.x.poke(true.B)
      }
      dut.io.pmpEnvironment.pma.head.cfg.c.poke(true.B)
      dut.io.config.foreach { config =>
        config.sourcePrivilege.poke(0.U)
        config.sourceVirtual.poke(false.B)
        config.policy.uEnable.poke(true.B)
        config.entries.head.boundLo.poke(0x8000.U)
        config.entries.head.boundHi.poke(0x8008.U)
        config.entries.head.entryValid.poke(true.B)
        config.entries.head.readAllowed.poke(true.B)
        config.entries.head.writeAllowed.poke(true.B)
      }
      dut.reset.poke(true.B)
      dut.clock.step(4)
      dut.reset.poke(false.B)
      // Translation and queue reset sweeps finish before the first accepted instruction.
      (0 until 64).foreach(_ => tick("reset-settle"))
      dut.io.queueEmpty.expect(true.B)
      dut.io.bufferEmpty.expect(true.B)
    }

    private def checkBufferedBytes(): Unit = {
      val actual = mutable.Map.empty[BigInt, Int]
      for (line <- dut.io.bufferState.indices if bool(dut.io.bufferState(line).state_valid)) {
        val words = dut.io.bufferData(line)
        val lineBytes = words.length * words.head.length
        val base = uint(dut.io.bufferPhysicalTags(line)) * lineBytes
        for (word <- words.indices; byte <- words(word).indices if bool(dut.io.bufferMask(line)(word)(byte))) {
          val address = base + word * words(word).length + byte
          assert(!actual.contains(address), "The minimal non-evicting sequence must use one copy of each buffered byte")
          actual(address) = uint(words(word)(byte)).toInt
        }
      }
      assert(actual.toMap == expectedBytes.toMap, s"Sbuffer byte contents disagree: actual=$actual expected=$expectedBytes")
    }

    private def runSource(source: Source, expectedPointer: Option[Pointer] = None): Unit = {
      until("sq-can-accept") { bool(dut.io.enq.canAccept) }
      val pointer = Pointer(bool(dut.io.enq.resp.head.flag), uint(dut.io.enq.resp.head.value).toInt)
      expectedPointer.foreach(expected => assert(pointer == expected, "SQ pointer did not advance through the full flag/value ring"))
      active = Some((source, pointer))
      issueCycle = -1
      permissionTag = None
      val oldWb = wbCount
      val oldDataWb = dataWbCount
      val oldRequests = permissionRequests
      val oldResponses = permissionResponses
      val oldPublications = publications
      val oldEmpty = emptyTokens
      if (source.denied) deniedSnapshot = Some(stateSnapshot())
      driveOwner(dut.io.enq.req.head.bits, source, pointer)
      dut.io.enq.needAlloc.head.poke(true.B)
      dut.io.enq.req.head.valid.poke(true.B)
      log("allocate", sourceFields(source, pointer))
      tick("allocate")
      dut.io.enq.needAlloc.head.poke(false.B)
      dut.io.enq.req.head.valid.poke(false.B)
      val sta = dut.io.sta(source.lane)
      val std = dut.io.std(source.lane)
      zero(sta.bits); zero(std.bits)
      driveOwner(sta.bits.uop, source, pointer)
      driveOwner(std.bits.uop, source, pointer)
      sta.bits.src(0).poke(source.address.U)
      sta.bits.src(1).poke(source.data.U)
      sta.bits.isFirstIssue.poke(true.B)
      std.bits.src(0).poke(source.data.U)
      std.bits.isFirstIssue.poke(true.B)
      sta.valid.poke(true.B)
      std.valid.poke(true.B)
      var staAccepted = false
      var stdAccepted = false
      var elapsed = 0
      while ((!staAccepted || !stdAccepted) && elapsed < 32) {
        val takeSta = !staAccepted && bool(sta.ready)
        val takeStd = !stdAccepted && bool(std.ready)
        tick("issue")
        if (takeSta) { staAccepted = true; sta.valid.poke(false.B) }
        if (takeStd) { stdAccepted = true; std.valid.poke(false.B) }
        elapsed += 1
      }
      assert(staAccepted && stdAccepted, "The real STA and STD must accept the allocated source")
      val slot = dut.io.slots(pointer.value)
      until("final-store-payload") { !bool(slot.waitS2) && wbCount == oldWb + 1 }
      slot.allocated.expect(true.B)
      slot.rob.flag.expect(false.B)
      slot.rob.value.expect(source.rob.U)
      slot.sq.flag.expect(pointer.flag.B)
      slot.sq.value.expect(pointer.value.U)
      slot.addressReady.expect(true.B)
      slot.dataReady.expect(true.B)
      slot.hasException.expect(source.denied.B)
      slot.committed.expect(false.B)
      slot.pending.expect(false.B)
      slot.nc.expect(false.B)
      slot.mmio.expect(false.B)
      assert(exceptions(slot.exceptions) == (if (source.denied) Set(24) else Set.empty[Int]))
      if (source.denied) {
        slot.fdiException.get.tval.expect(source.address.U)
        slot.fdiException.get.reason.expect(3.U)
        dut.io.exceptionAddress.expect(source.address.U)
      }
      assert(wbCount == oldWb + 1 && dataWbCount == oldDataWb + 1)
      assert(permissionRequests - oldRequests == (if (enabled) 1 else 0))
      assert(permissionResponses - oldResponses == (if (enabled) 1 else 0))
      log("sq-final-payload", sourceFields(source, pointer) + s",\"denied\":${source.denied}")
      // pendingPtr lets the real SQ release its final empty exception token. It is
      // not an assertion of architectural retirement: denied stores have scommit=0.
      dut.io.rob.pendingPtr.value.poke(source.rob.U)
      dut.io.rob.pendingPtrNext.value.poke(source.rob.U)
      dut.io.rob.scommit.poke((if (source.denied) 0 else 1).U)
      tick("rob-head-release")
      dut.io.rob.scommit.poke(0.U)
      until("sq-release-and-publication") {
        !bool(slot.allocated) && bool(dut.io.queueEmpty) &&
          (if (source.denied) emptyTokens == oldEmpty + 1 else publications == oldPublications + 1)
      }
      // The data module applies the accepted write two cycles after publication.
      (0 until 4).foreach(_ => tick("publication-settle"))
      assert(wbCount == oldWb + 1 && dataWbCount == oldDataWb + 1)
      if (source.denied) {
        assert(publications == oldPublications && emptyTokens == oldEmpty + 1)
        log("denied-buffer-unchanged", sourceFields(source, pointer))
      } else assert(emptyTokens == oldEmpty && publications == oldPublications + 1)
      deniedSnapshot = None
      checkBufferedBytes()
      active = None
    }

    def runWrap(): Unit = {
      reset()
      val count = 2 * dut.io.slots.length + 2
      for (index <- 0 until count) {
        val source = Source(s"wrap-$index", if (index % 2 == 0) 0x8000 else 0x8040,
          BigInt("132435465768798a", 16) ^ BigInt(index), 4 + index, index % 2)
        val pointer = Pointer((index / dut.io.slots.length) % 2 != 0, index % dut.io.slots.length)
        runSource(source, Some(pointer))
      }
      assert(dequeues == count && writeCount + emptyTokens == count)
      dut.io.flushBuffer.poke(true.B); dut.io.cacheWrite.req.ready.poke(true.B)
      until("wrapped queue cache flush", 256) { bool(dut.io.flushDone) && bool(dut.io.bufferEmpty) && cacheResponses.isEmpty }
      dut.io.flushBuffer.poke(false.B)
      (0 until 4).foreach(_ => tick("wrap-flush-settle"))
      assert(memory.toMap == expectedBytes.toMap)
      assert(metadataResponses.forall(_.isEmpty))
      log("full-pointer-wrap-pass", s"\"sources\":$count,\"queue_entries\":${dut.io.slots.length},\"wraps\":2,\"data_writes\":$writeCount,\"empty_tokens\":$emptyTokens")
    }

    def run(): Unit = {
      reset()
      sources.foreach(source => runSource(source))
      assert(publications == (if (enabled) 2 else 3))
      assert(writeCount == publications && emptyTokens == (if (enabled) 1 else 0))
      assert(dequeues == 3, s"Each allocated SQ entry must be physically released once, got $dequeues")
      dut.io.flushBuffer.poke(true.B)
      dut.io.cacheWrite.req.ready.poke(true.B)
      until("flush-buffer", 256) { bool(dut.io.flushDone) && bool(dut.io.bufferEmpty) && cacheResponses.isEmpty }
      dut.io.flushBuffer.poke(false.B)
      (0 until 4).foreach(_ => tick("flush-settle"))
      assert(sentLines.toSet == (if (enabled) Set(BigInt(0x8000)) else Set(BigInt(0x8000), BigInt(0x8040))))
      assert(memory.toMap == expectedBytes.toMap)
      for (address <- BigInt(0x7ff0) until BigInt(0x8060)) {
        val actual = memory.getOrElse(address, sentinel)
        val expected = expectedBytes.getOrElse(address, sentinel)
        assert(actual == expected, s"Memory or untouched sentinel changed at $address")
      }
      assert(metadataResponses.forall(_.isEmpty))
      log("summary", s"\"enabled\":$enabled,\"sources\":3,\"publications\":$publications," +
        s"\"empty_tokens\":$emptyTokens,\"data_writes\":$writeCount,\"cache_lines\":${sentLines.size}," +
        "\"nc_mmio_validated\":false,\"backend_trap_validated\":false,\"full_matrix_validated\":false")
      println(s"Store permission minimal PASS enabled=$enabled sources=3 cycles=$cycle publications=$publications emptyTokens=$emptyTokens cacheLines=${sentLines.size}")
    }
  }


  private case class MatrixAccess(address: BigInt, operation: Int = 3, floating: Boolean = false,
    lane: Int = 0, issueAt: Int = 0, dataAt: Int = 0, trusted: Boolean = false,
    immediate: Int = 0, data: BigInt = BigInt("a3b4c5d6e7f80192", 16)) {
    val size: Int = 1 << operation
  }
  private case class MatrixRegion(lo: BigInt, hi: BigInt, valid: Boolean = true, writable: Boolean = true)
  private case class MatrixCase(name: String, accesses: Seq[MatrixAccess], privilege: Int = 0,
    virtual: Boolean = false, active: Boolean = true, closeWrite: Boolean = false,
    lo: BigInt = 0x8000, hi: BigInt = 0x8100, valid: Boolean = true,
    writable: Boolean = true, pmm: Int = 0, pmpFault: Boolean = false,
    satp: Int = 0, pageFault: Boolean = false, guestPageFault: Boolean = false,
    pbmt: Int = 0, trigger: Int = 0, misalign: Boolean = false,
    cancelAfter: Option[Int] = None, resetOwned: Boolean = false, pressure: Boolean = false, robBase: Int = 4,
    regions: Seq[MatrixRegion] = Seq.empty, bareAddressFault: Boolean = false) {
    // Existing cases keep their single entry; explicit regions never splice partial permissions.
    def configuredRegions: Seq[MatrixRegion] = if (regions.nonEmpty) regions else Seq(MatrixRegion(lo, hi, valid, writable))
    private val mask64 = (BigInt(1) << 64) - 1
    def effective(access: MatrixAccess): BigInt = {
      val raw = (access.address + access.immediate) & mask64
      val keep = if (pmm == 2) 57 else if (pmm == 3) 48 else 64
      val stripped = raw & ((BigInt(1) << keep) - 1)
      if (satp != 0 && keep < 64 && stripped.testBit(keep - 1))
        stripped | (mask64 ^ ((BigInt(1) << keep) - 1)) else stripped
    }
    def outcome(access: MatrixAccess): Int = {
      if (!enabled || privilege == 3 || !active) 0
      else if (virtual) 2
      else if (access.trusted || closeWrite) 0
      else {
        val first = effective(access)
        val last = first + access.size - 1
        if (last <= mask64 && configuredRegions.exists(region => region.valid && region.writable &&
          region.lo < region.hi && region.lo <= first && last < region.hi)) 0 else 1
      }
    }
    def expectedExceptions(access: MatrixAccess): Set[Int] = {
      val fdi = outcome(access) match {
        case 1 => Set(if (privilege == 0) 24 else 25)
        case 2 => Set(2)
        case _ => Set.empty[Int]
      }
      fdi ++ (if (pmpFault || bareAddressFault) Set(7) else Set.empty[Int]) ++
        (if (pageFault) Set(15) else Set.empty[Int]) ++
        (if (guestPageFault) Set(23) else Set.empty[Int]) ++
        (if (trigger == 1) Set(3) else Set.empty[Int])
    }
  }

  // Policy cases hold configuration stable until every original owner is released.
  private def matrixCases: Seq[MatrixCase] = {
    val both = Seq(MatrixAccess(0x8000, lane = 0), MatrixAccess(0x8040, lane = 1))
    val absent = MatrixCase("bounds-disabled", Seq(MatrixAccess(0x8000)), valid = false)
    Seq(
      MatrixCase("integer-widths-dual-consecutive", Seq(
        MatrixAccess(0x8003, operation = 0, lane = 0, issueAt = 0, dataAt = -2),
        MatrixAccess(0x8012, operation = 1, lane = 1, issueAt = 0, dataAt = 3),
        MatrixAccess(0x8024, operation = 2, lane = 0, issueAt = 1, dataAt = -1),
        MatrixAccess(0x8038, operation = 3, lane = 1, issueAt = 1, dataAt = 4))),
      MatrixCase("floating-shared-store-path", Seq(
        MatrixAccess(0x8084, operation = 2, floating = true, lane = 0),
        MatrixAccess(0x80a8, operation = 3, floating = true, lane = 1))),
      MatrixCase("dual-allow-deny", both, hi = 0x8008),
      MatrixCase("dual-deny-allow", both.reverse.zipWithIndex.map { case (a, lane) => a.copy(lane = lane) }, hi = 0x8008),
      absent,
      absent.copy(name = "entry-no-write", valid = true, writable = false),
      absent.copy(name = "supervisor-denied", privilege = 1),
      absent.copy(name = "machine-bypass", privilege = 3),
      absent.copy(name = "disabled-user-policy", active = false),
      absent.copy(name = "disabled-supervisor-policy", privilege = 1, active = false),
      absent.copy(name = "trusted-user", accesses = Seq(MatrixAccess(0x8000, trusted = true))),
      absent.copy(name = "trusted-supervisor", privilege = 1, accesses = Seq(MatrixAccess(0x8000, trusted = true))),
      absent.copy(name = "close-user-write", closeWrite = true),
      absent.copy(name = "close-supervisor-write", privilege = 1, closeWrite = true),
      absent.copy(name = "guest-user-active", virtual = true),
      absent.copy(name = "guest-supervisor-active", virtual = true, privilege = 1),
      absent.copy(name = "guest-user-inactive", virtual = true, active = false),
      absent.copy(name = "guest-supervisor-inactive", virtual = true, privilege = 1, active = false),
      MatrixCase("high-bound-no-alias", Seq(MatrixAccess(0x8000)), lo = BigInt("100008000", 16), hi = BigInt("100008100", 16)),
      MatrixCase("bare-pmm7", Seq(MatrixAccess((BigInt(0x55) << 57) | 0x8000)), pmm = 2),
      MatrixCase("bare-pmm16", Seq(MatrixAccess((BigInt(0xface) << 48) | 0x8000)), pmm = 3),
      MatrixCase("rv64-address-wrap", Seq(MatrixAccess((BigInt(1) << 64) - 4, immediate = 4)), lo = 0, hi = 8),
      MatrixCase("pmp-store-access-fault", Seq(MatrixAccess(0x8000)), pmpFault = true),
      absent.copy(name = "pmp-and-user-denial", pmpFault = true),
      absent.copy(name = "pmp-and-supervisor-denial", pmpFault = true, privilege = 1))
  }

  private def translationCases: Seq[MatrixCase] = {
    val negative = BigInt("ffffffffffff8000", 16)
    val base = MatrixCase("sv39-refill-allow", Seq(MatrixAccess(0x8000)), satp = 8)
    val common = Seq(base, base.copy(name = "sv39-refill-deny", valid = false),
      base.copy(name = "sv48-refill-allow", satp = 9),
      base.copy(name = "sv48-refill-deny", satp = 9, valid = false),
      base.copy(name = "sv39-store-page-fault", pageFault = true),
      base.copy(name = "sv39-page-fault-denied", pageFault = true, valid = false),
      base.copy(name = "sv48-page-fault-denied", satp = 9, pageFault = true, valid = false),
      base.copy(name = "guest-stage2-fault", satp = 0, guestPageFault = true, virtual = true),
      base.copy(name = "guest-stage2-disabled-fault", satp = 0, guestPageFault = true, virtual = true, active = false),
      base.copy(name = "sv39-high-canonical", accesses = Seq(MatrixAccess(negative)), lo = negative, hi = negative + 8),
      base.copy(name = "sv39-pmm16-sign-extension", accesses = Seq(MatrixAccess((BigInt(0xa55a) << 48) | (negative & ((BigInt(1) << 48) - 1)))),
        pmm = 3, lo = negative, hi = negative + 8),
      base.copy(name = "sv48-pmm7-sign-extension", accesses = Seq(MatrixAccess((BigInt(0x55) << 57) | (negative & ((BigInt(1) << 57) - 1)))),
        pmm = 2, satp = 9, lo = negative, hi = negative + 8),
      MatrixCase("store-address-breakpoint", Seq(MatrixAccess(0x8000)), trigger = 1),
      MatrixCase("store-address-debug", Seq(MatrixAccess(0x8000)), trigger = 2),
      MatrixCase("breakpoint-with-denial", Seq(MatrixAccess(0x8000)), trigger = 1, valid = false),
      MatrixCase("debug-with-denial", Seq(MatrixAccess(0x8000)), trigger = 2, valid = false))
    val fdiOnly = Seq(1, 2).map(kind => base.copy(name = s"pbmt-$kind-denied-no-uncache", pbmt = kind, valid = false)) ++
      Seq(base.copy(name = "denied-cross16-no-split", accesses = Seq(MatrixAccess(0x800d)), valid = false, misalign = true),
        base.copy(name = "denied-crosspage-no-split", accesses = Seq(MatrixAccess(0x8ffd)), valid = false, misalign = true))
    common ++ (if (enabled) fdiOnly else Seq.empty)
  }

  private def cancellationCases: Seq[MatrixCase] = Seq(
    MatrixCase("cancel-young-A-retain-old-B", Seq(MatrixAccess(0x8000, issueAt = 1, dataAt = -1),
      MatrixAccess(0x8040, issueAt = 0, dataAt = -2)), hi = 0x8008, cancelAfter = Some(4)),
    MatrixCase("retain-old-A-cancel-young-B", Seq(MatrixAccess(0x8000, issueAt = 0, dataAt = -2),
      MatrixAccess(0x8040, issueAt = 1, dataAt = -1)), hi = 0x8008, cancelAfter = Some(4)),
    MatrixCase("cancel-A-and-B", Seq(MatrixAccess(0x8000, issueAt = 0, dataAt = -2),
      MatrixAccess(0x8040, issueAt = 1, dataAt = -1)), hi = 0x8008, cancelAfter = Some(3)),
    MatrixCase("reset-while-permission-owned", Seq(MatrixAccess(0x8000)), resetOwned = true))

  private def remainingCases: Seq[MatrixCase] = {
    val adjacent = Seq(MatrixRegion(0x8000, 0x8008), MatrixRegion(0x8008, 0x8010))
    val pair = Seq(MatrixAccess(0x8000, lane = 0, data = BigInt("0123456789abcdef", 16)),
      MatrixAccess(0x8008, lane = 1, data = BigInt("fedcba9876543210", 16)))
    val ordinary = Seq(
      MatrixCase("valid-empty-bound", Seq(MatrixAccess(0x8000)), lo = 0x8000, hi = 0x8000),
      MatrixCase("valid-reversed-bound", Seq(MatrixAccess(0x8000)), lo = 0x8010, hi = 0x8000),
      MatrixCase("second-entry-whole-access", Seq(MatrixAccess(0x8008)), regions = adjacent),
      MatrixCase("access-last-byte-overflow", Seq(MatrixAccess((BigInt(1) << 64) - 4)),
        lo = 0, hi = (BigInt(1) << 64) - 8, bareAddressFault = true),
      MatrixCase("same-line-dual-allow-deny", pair, lo = 0x8000, hi = 0x8008),
      MatrixCase("same-line-dual-deny-allow", pair.reverse.zipWithIndex.map { case (access, lane) =>
        access.copy(lane = lane)
      }, lo = 0x8000, hi = 0x8008),
      MatrixCase("same-line-dual-deny-deny", pair, lo = 0x8010, hi = 0x8018))
    // These original misaligned requests are rejected before any successful split behavior is required.
    val rejectedOnly = Seq(
      MatrixCase("adjacent-write-entries-no-splice", Seq(MatrixAccess(0x8004)), regions = adjacent, misalign = true),
      MatrixCase("lower-bound-partial-overlap", Seq(MatrixAccess(0x8004)), lo = 0x8008, hi = 0x8010, misalign = true))
    ordinary ++ (if (enabled) rejectedOnly else Seq.empty)
  }

  private class MatrixDriver(dut: FDIStorePermissionHarness, events: mutable.ArrayBuffer[String], trial: MatrixCase, initializeState: Boolean = true, expectedFirst: Option[Pointer] = None) {
    private case class Entry(index: Int, sq: Pointer) {
      val access = trial.accesses(index)
      val rob: Int = trial.robBase + index
      // PTW transport preserves the raw request tag; PMM fullva remains the permission address.
      val transportVpn: BigInt = (((access.address + access.immediate) & ((BigInt(1) << 64) - 1)) &
        ((BigInt(1) << dut.VAddrBits) - 1)) >> 12
      val va: BigInt = trial.effective(access)
      val pa: BigInt = if (trial.satp != 0 || trial.guestPageFault) BigInt(0x90000) + (va & 4095) else va
      val expected: Set[Int] = trial.expectedExceptions(access)
      val blocked: Boolean = expected.nonEmpty || trial.trigger == 2
      val canceled: Boolean = trial.resetOwned || trial.cancelAfter.exists(rob > _)
      var attempts: Int = 0
      var acceptedAt: Int = -1
    }
    private var cycle = 0
    private var entries = Vector.empty[Entry]
    private val accepted = mutable.Set.empty[Int]
    private val dataAccepted = mutable.Set.empty[Int]
    private val complete = mutable.Set.empty[Int]
    private val published = mutable.Set.empty[Int]
    private val cleared = mutable.Set.empty[Int]
    private val permission = mutable.Map.empty[BigInt, (Entry, Int)]
    private val requests = mutable.Set.empty[Int]
    private val responses = mutable.Set.empty[Int]
    private val metadata = Seq.fill(dut.io.metadata.length)(mutable.Queue.empty[Int])
    private val acknowledgements = mutable.Queue.empty[(Int, BigInt)]
    private val expectedBytes = mutable.Map.empty[BigInt, Int]
    private val actualBytes = mutable.Map.empty[BigInt, Int]
    private val cacheLines = mutable.Set.empty[BigInt]
    private var dataWrites = 0
    private var emptyTokens = 0
    private var deqCount = 0
    private var snapshot: Option[Vector[BigInt]] = None
    private var ptwDue = -1
    private var refillAt = -1
    private var retryAt = -1
    private var misses = 0
    private var ptwRequests = 0
    private var ptwResponses = 0
    private var issueStart = -1
    private var resetDone = false
    private var resetForbidden = false
    private var blockedPublications = 0
    private var dualPublicationCycles = 0
    private var heldCache: Option[Vector[BigInt]] = None
    private var heldPublications = Vector.empty[(Pointer, Vector[BigInt])]
    private val translated = trial.satp != 0 || trial.guestPageFault
    private def event(kind: String, fields: String = ""): Unit =
      events += s"{\"case\":\"${trial.name}\",\"cycle\":$cycle,\"event\":\"$kind\"${if (fields.isEmpty) "" else "," + fields}}"
    private def state(): Vector[BigInt] =
      dut.io.bufferState.toVector.map(s => if (bool(s.state_valid)) BigInt(1) else BigInt(0)) ++
        values(dut.io.bufferPhysicalTags) ++ values(dut.io.bufferVirtualTags) ++
        values(dut.io.bufferData) ++ values(dut.io.bufferMask)
    private def find(uop: DynInst): Entry = {
      val source = entries.find(e => e.rob == uint(uop.robIdx.value) && !bool(uop.robIdx.flag) &&
        e.sq.value == uint(uop.sqIdx.value) && e.sq.flag == bool(uop.sqIdx.flag))
        .getOrElse(throw new AssertionError("Result does not belong to an allocated full ROB/SQ identity"))
      uop.uopIdx.expect(0.U)
      source
    }
    private def driveOwner(uop: DynInst, entry: Entry): Unit = {
      zero(uop)
      val access = entry.access
      val imm = access.immediate & 4095
      val instruction = (BigInt(imm >> 5) << 25) | (BigInt(1) << 20) | (BigInt(2) << 15) |
        (BigInt(access.operation) << 12) | (BigInt(imm & 31) << 7) | (if (access.floating) 0x27 else 0x23)
      uop.instr.poke(instruction.U)
      uop.pc.poke((0x2000 + entry.index * 4).U)
      uop.fuType.poke(FuType.stu.U)
      uop.fuOpType.poke(access.operation.U)
      uop.imm.poke(imm.U)
      uop.robIdx.value.poke(entry.rob.U)
      uop.sqIdx.flag.poke(entry.sq.flag.B); uop.sqIdx.value.poke(entry.sq.value.U)
      uop.ftqPtr.value.poke(2.U); uop.ftqOffset.poke(((entry.index * 2) % (1 << uop.ftqOffset.getWidth)).U)
      uop.firstUop.poke(true.B); uop.lastUop.poke(true.B); uop.numLsElem.poke(1.U)
      uop.fdiNotTrusted.foreach(_.poke((!access.trusted).B))
    }
    private def edge(): Unit = {
      for ((port, lane) <- dut.io.metadata.zipWithIndex) {
        port.resp.valid.poke(metadata(lane).headOption.contains(cycle).B)
        if (metadata(lane).headOption.contains(cycle)) metadata(lane).dequeue()
      }
      dut.io.cacheWrite.main_pipe_hit_resp.valid.poke(false.B)
      if (acknowledgements.headOption.exists(_._1 == cycle)) {
        val (_, id) = acknowledgements.dequeue()
        dut.io.cacheWrite.main_pipe_hit_resp.valid.poke(true.B)
        dut.io.cacheWrite.main_pipe_hit_resp.bits.id.poke(id.U)
        event("cache-ack", s"\"id\":$id")
      }
      assert(!bool(dut.io.uncache.req.valid), "The cacheable matrix must not offer an uncache data write")
      assert(!bool(dut.io.splitRequest.valid), "A blocked original access must not emit any split-store request")
      dut.io.ptw.resp.valid.poke(false.B)
      if (cycle == ptwDue) {
        val entry = entries.head
        val response = dut.io.ptw.resp.bits
        val vpn = entry.transportVpn
        zero(response)
        response.memidx.is_st.poke(true.B); response.memidx.idx.poke(entry.sq.value.U)
        if (trial.guestPageFault) {
          response.s2xlate.poke(2.U)
          response.s2.entry.tag.poke(vpn.U); response.s2.entry.level.foreach(_.poke(0.U))
          response.s2.entry.ppn.poke(0x90.U); response.s2.entry.v.poke(false.B)
          response.s2.entry.perm.foreach { perm =>
            perm.r.poke(true.B); perm.w.poke(true.B); perm.a.poke(true.B); perm.d.poke(true.B)
          }
          response.s2.gpf.poke(true.B)
        } else {
          val sector = response.s1.pteidx.length
          val index = (vpn % sector).toInt
          response.s2xlate.poke(0.U)
          response.s1.entry.tag.poke((vpn / sector).U)
          response.s1.entry.level.foreach(_.poke(0.U))
          response.s1.entry.ppn.poke((BigInt(0x90) / sector).U)
          response.s1.entry.v.poke((!trial.pageFault).B)
          response.s1.entry.pbmt.poke(trial.pbmt.U)
          response.s1.entry.perm.foreach { perm =>
            perm.r.poke(true.B); perm.w.poke(true.B); perm.a.poke(true.B); perm.d.poke(true.B); perm.u.poke(true.B)
          }
          response.s1.addr_low.poke(index.U); response.s1.pf.poke(trial.pageFault.B)
          response.s1.valididx.zipWithIndex.foreach { case (bit, i) => bit.poke((i == index).B) }
          response.s1.pteidx.zipWithIndex.foreach { case (bit, i) => bit.poke((i == index).B) }
          response.s1.ppn_low.foreach(_.poke(0.U))
        }
        dut.io.ptw.resp.valid.poke(true.B); dut.io.ptw.resp.ready.expect(true.B)
        refillAt = cycle; retryAt = cycle + 6; ptwResponses += 1
        event("ptw-refill", s"\"vpn\":$vpn,\"page_fault\":${trial.pageFault},\"guest_page_fault\":${trial.guestPageFault},\"pbmt\":${trial.pbmt}")
      }
      for (request <- dut.io.ptw.req if bool(request.valid) && bool(request.ready)) {
        assert(translated && entries.size == 1)
        val entry = entries.head
        val vpn = entry.transportVpn
        request.bits.vpn.expect(vpn.U); request.bits.memidx.is_st.expect(true.B)
        request.bits.memidx.idx.expect(entry.sq.value.U)
        request.bits.s2xlate.expect((if (trial.guestPageFault) 2 else 0).U)
        ptwRequests += 1
        if (ptwDue < 0) ptwDue = cycle + 12
        event("ptw-request", s"\"vpn\":$vpn,\"sq\":${entry.sq.value}")
      }
      for ((port, lane) <- dut.io.sta.zipWithIndex if bool(port.valid) && bool(port.ready)) {
        val entry = find(port.bits.uop)
        if (entry.attempts == 0) assert(accepted.add(entry.index))
        else assert(translated && entry.attempts == 1 && refillAt >= 0 && cycle > refillAt)
        entry.attempts += 1; entry.acceptedAt = cycle
        event("store-address-accepted", s"\"source\":${entry.index},\"lane\":$lane,\"rob\":${entry.rob},\"sq\":${entry.sq.value},\"sq_flag\":${entry.sq.flag},\"effective_va\":${entry.va}")
      }
      for ((port, lane) <- dut.io.std.zipWithIndex if bool(port.valid) && bool(port.ready)) {
        val entry = find(port.bits.uop)
        assert(dataAccepted.add(entry.index))
        event("store-data-accepted", s"\"source\":${entry.index},\"lane\":$lane")
      }
      for ((port, lane) <- dut.io.metadata.zipWithIndex) {
        if (bool(port.req.valid) && bool(port.req.ready)) {
          dut.io.sta(lane).valid.expect(true.B)
          val entry = find(dut.io.sta(lane).bits.uop)
          port.req.bits.cmd.expect(MemoryOpConstants.M_PFW)
          metadata(lane).enqueue(cycle + 2)
          event("metadata-query", s"\"source\":${entry.index},\"lane\":$lane")
        }
        val phase = dut.io.phases(lane)
        if (bool(phase.permissionResponse.valid)) {
          val response = phase.permissionResponse.bits
          val (entry, acceptedAt) = permission.remove(uint(response.request.tag)).getOrElse(
            throw new AssertionError("Permission response has no accepted descriptor"))
          assert(cycle == acceptedAt + 1)
          response.request.address.expect(entry.va.U)
          response.outcome.expect(trial.outcome(entry.access).U)
          response.reason.expect((if (trial.outcome(entry.access) == 1) 3 else 0).U)
          phase.permissionConsumed.expect(true.B)
          assert(responses.add(entry.index))
          event("permission-response", s"\"source\":${entry.index},\"outcome\":${trial.outcome(entry.access)}")
        }
        if (bool(phase.tlbResponse.valid) && bool(phase.primary.valid) && !bool(phase.tlbResponse.bits.miss)) {
          val entry = find(phase.primary.bits.uop)
          phase.tlbResponse.bits.fullva.expect(entry.va.U)
          phase.tlbResponse.bits.pbmt.head.expect(trial.pbmt.U)
          phase.tlbResponse.bits.excp.head.pf.st.expect(trial.pageFault.B)
          phase.tlbResponse.bits.excp.head.gpf.st.expect(trial.guestPageFault.B)
          if (trial.bareAddressFault) {
            phase.tlbResponse.bits.excp.head.af.st.expect(true.B)
            phase.tlbResponse.bits.excp.head.vaNeedExt.expect(false.B)
          }
          if (translated && !trial.pageFault && !trial.guestPageFault) phase.tlbResponse.bits.paddr.head.expect(entry.pa.U)
          event("tlb-hit", s"\"source\":${entry.index},\"fullva\":${entry.va},\"pbmt\":${trial.pbmt}")
        }
        if (bool(phase.permissionRequest.valid)) {
          val entry = find(phase.primary.bits.uop)
          val request = phase.permissionRequest.bits
          request.address.expect(entry.va.U)
          request.sizeLog2.expect(entry.access.operation.U)
          request.operation.expect(1.U)
          request.sourcePrivilege.expect(trial.privilege.U)
          request.sourceVirtual.expect(trial.virtual.B)
          request.notTrusted.expect((!entry.access.trusted).B)
          assert(requests.add(entry.index))
          assert(!permission.contains(uint(request.tag)))
          permission(uint(request.tag)) = (entry, cycle)
          event("permission-request", s"\"source\":${entry.index},\"address\":${entry.va}")
        }
        if (bool(dut.io.feedback(lane).valid) && !bool(dut.io.feedback(lane).bits.hit)) {
          assert(translated && refillAt < 0 && complete.isEmpty && permission.isEmpty)
          val entry = entries.head
          dut.io.feedback(lane).bits.robIdx.value.expect(entry.rob.U)
          dut.io.feedback(lane).bits.sqIdx.value.expect(entry.sq.value.U)
          misses += 1
          event("tlb-miss-feedback", s"\"source\":${entry.index}")
        }
        if (bool(phase.s2)) {
          val entry = find(phase.supplement.uop)
          phase.supplement.fullva.expect(entry.va.U)
          if (!trial.pageFault && !trial.guestPageFault && !trial.bareAddressFault) phase.pmpFault.expect(trial.pmpFault.B)
          phase.supplement.hasException.expect(entry.blocked.B)
        }
        if (bool(dut.io.wb(lane).valid)) {
          val entry = find(dut.io.wb(lane).bits.uop)
          assert(!entry.canceled && !resetForbidden, "Canceled/reset owner produced a writeback")
          assert(complete.add(entry.index))
          assert(exceptions(dut.io.wb(lane).bits.uop.exceptionVec) == entry.expected)
          dut.io.wb(lane).bits.uop.trigger.expect((if (trial.trigger == 2) 1 else if (trial.trigger == 1) 0 else 15).U)
          if (trial.outcome(entry.access) == 1) {
            dut.io.wb(lane).bits.uop.fdiException.get.tval.expect(entry.va.U)
            dut.io.wb(lane).bits.uop.fdiException.get.reason.expect(3.U)
          }
          event("store-writeback", s"\"source\":${entry.index},\"exceptions\":[${entry.expected.toSeq.sorted.mkString(",")}]")
        }
        if (bool(dut.io.dataWb(lane).valid)) {
          val entry = find(dut.io.dataWb(lane).bits.uop)
          dut.io.dataWb(lane).bits.data.expect(entry.access.data.U)
        }
      }
      val offeredPublications = dut.io.publication.filter(port => bool(port.valid)).map { port =>
        val owner = Pointer(bool(port.sq.flag), uint(port.sq.value).toInt)
        val payload = Vector(uint(port.bits.addr), uint(port.bits.vaddr), uint(port.bits.mask), uint(port.bits.data),
          if (bool(port.bits.vecValid)) BigInt(1) else BigInt(0),
          if (bool(port.bits.sqNeedDeq)) BigInt(1) else BigInt(0))
        owner -> payload
      }.toVector
      assert(offeredPublications.map(_._1).distinct.size == offeredPublications.size,
        "One SQ owner was offered on multiple publication lanes")
      val acceptedPrefix = dut.io.publication.count(port => bool(port.fire))
      for ((port, lane) <- dut.io.publication.zipWithIndex) {
        port.valid.expect((lane < offeredPublications.size).B)
        port.fire.expect((lane < acceptedPrefix).B)
      }
      // An accepted prefix advances the output head, so the held suffix may move to lower lanes.
      assert(offeredPublications.take(heldPublications.size) == heldPublications,
        "Sbuffer backpressure lost, reordered or changed an unaccepted SQ owner")
      heldPublications = offeredPublications.drop(acceptedPrefix)
      blockedPublications += heldPublications.size
      if (scenario == "remaining" && trial.accesses.size == 2 && acceptedPrefix == 2) {
        dualPublicationCycles += 1
        val owners = dut.io.publication.map(port => s"{\"sq_flag\":${bool(port.sq.flag)},\"sq\":${uint(port.sq.value)},\"vec_valid\":${bool(port.bits.vecValid)}}")
        event("dual-publication", s"\"owners\":[${owners.mkString(",")}]")
      }
      for ((port, lane) <- dut.io.publication.zipWithIndex) {
        if (bool(port.fire)) {
          val entry = entries.find(e => e.sq.value == uint(port.sq.value) && e.sq.flag == bool(port.sq.flag)).getOrElse(
            throw new AssertionError("Publication does not belong to an allocated full SQ pointer"))
          assert(!entry.canceled && !resetForbidden, "Canceled/reset owner reached Sbuffer")
          if (!trial.misalign && !trial.bareAddressFault) port.bits.addr.expect(entry.pa.U)
          port.bits.vecValid.expect((!entry.blocked).B)
          port.bits.sqNeedDeq.expect(true.B)
          assert(published.add(entry.index))
          if (entry.blocked) emptyTokens += 1 else {
            val offset = (entry.pa & 15).toInt
            port.bits.mask.expect((((BigInt(1) << entry.access.size) - 1) << offset).U)
            for (byte <- 0 until entry.access.size) {
              val expected = ((entry.access.data >> (8 * byte)) & 255).toInt
              val actual = ((uint(port.bits.data) >> (8 * (offset + byte))) & 255).toInt
              assert(actual == expected, s"Byte corruption for source ${entry.index}")
              expectedBytes(entry.pa + byte) = expected
            }
          }
          event("publication", s"\"source\":${entry.index},\"lane\":$lane,\"vec_valid\":${bool(port.bits.vecValid)},\"mask\":${uint(port.bits.mask)}")
        }
        val write = dut.io.writes(lane)
        assert(bool(write.valid) == (bool(port.fire) && bool(port.bits.vecValid)))
        if (bool(write.valid)) {
          assert(uint(write.bits.wvec).bitCount == 1)
          write.bits.mask.expect(uint(port.bits.mask).U)
          write.bits.data.expect(uint(port.bits.data).U)
          dataWrites += 1
        }
      }
      deqCount += uint(dut.io.queueDequeue).toInt
      for (entry <- entries if accepted(entry.index) && !bool(dut.io.slots(entry.sq.value).allocated)) cleared += entry.index
      val request = dut.io.cacheWrite.req
      heldCache.foreach { previous =>
        request.valid.expect(true.B); assert(values(request.bits) == previous, "Cache backpressure changed the held request")
      }
      heldCache = if (bool(request.valid) && !bool(request.ready)) Some(values(request.bits)) else None
      if (bool(request.valid)) {
        val lineBytes = request.bits.mask.getWidth
        val offered = uint(request.bits.addr)
        val allowedLines = entries.filter(e => !e.blocked && !e.canceled).map(entry => entry.pa - entry.pa % lineBytes).toSet
        assert(allowedLines(offered), "A blocked-only cache line became a visible request under backpressure")
        event("cache-request", s"\"address\":$offered,\"ready\":${bool(request.ready)}")
      }
      if (bool(request.valid) && bool(request.ready)) {
        val address = uint(request.bits.addr)
        val lineBytes = request.bits.mask.getWidth
        request.bits.cmd.expect(MemoryOpConstants.M_XWR)
        assert(cacheLines.add(address))
        val bytes = expectedBytes.filter { case (byte, _) => byte >= address && byte < address + lineBytes }
        assert(bytes.nonEmpty)
        val mask = bytes.keys.foldLeft(BigInt(0))((result, byte) => result.setBit((byte - address).toInt))
        request.bits.mask.expect(mask.U)
        for ((byte, expected) <- bytes) {
          val actual = ((uint(request.bits.data) >> (8 * (byte - address).toInt)) & 255).toInt
          assert(actual == expected)
          actualBytes(byte) = actual
        }
        acknowledgements.enqueue((cycle + 3, uint(request.bits.id)))
      }
      snapshot.foreach(before => assert(state() == before, "Blocked-only batch changed Sbuffer state before the clock edge"))
      dut.clock.step(); cycle += 1
      snapshot.foreach(before => assert(state() == before, "Blocked-only batch changed Sbuffer data/mask/tag/valid"))
    }
    private def until(label: String, limit: Int = 160)(done: => Boolean): Unit = {
      val stop = cycle + limit
      while (!done && cycle < stop) edge()
      assert(done, s"${trial.name}: timeout waiting for $label at cycle $cycle")
    }
    private def idle(count: Int): Unit = (0 until count).foreach(_ => edge())
    private def initialize(): Unit = {
      zeroInputs(dut.io)
      dut.io.enq.lqCanAccept.poke(true.B)
      dut.io.metadata.foreach(_.req.ready.poke(true.B))
      dut.io.ptw.req.foreach(_.ready.poke(true.B))
      dut.io.uncache.req.ready.poke(true.B)
      dut.io.tlbCsr.priv.imode.poke(trial.privilege.U)
      dut.io.tlbCsr.priv.dmode.poke(0.U)
      dut.io.tlbCsr.priv.virt.poke(trial.guestPageFault.B)
      dut.io.tlbCsr.satp.mode.poke(trial.satp.U)
      dut.io.tlbCsr.mPBMTE.poke((trial.pbmt != 0).B)
      dut.io.tlbCsr.hgatp.mode.poke((if (trial.guestPageFault) 8 else 0).U)
      dut.io.tlbCsr.pmm.senvcfg.poke(trial.pmm.U)
      dut.io.csrCtrl.hd_misalign_st_enable.poke(trial.misalign.B)
      if (trial.trigger != 0) {
        dut.io.fromCsrTrigger.tEnableVec.head.poke(true.B)
        dut.io.fromCsrTrigger.triggerCanRaiseBpExp.poke(true.B)
        val trigger = dut.io.fromCsrTrigger.tdataVec.head
        trigger.store.poke(true.B); trigger.matchType.poke(0.U)
        trigger.tdata2.poke(trial.accesses.head.address.U)
        trigger.action.poke((if (trial.trigger == 2) 1 else 0).U)
      }
      dut.io.pmpEnvironment.cmode.poke(true.B)
      dut.io.pmpEnvironment.mode.poke(0.U)
      for (bank <- Seq(dut.io.pmpEnvironment.pmp, dut.io.pmpEnvironment.pma)) {
        bank.head.cfg.a.poke(1.U); bank.head.addr.poke(0x40000.U)
        bank.head.cfg.r.poke(true.B); bank.head.cfg.x.poke(true.B); bank.head.cfg.w.poke(true.B)
      }
      dut.io.pmpEnvironment.pmp.head.cfg.w.poke((!trial.pmpFault).B)
      dut.io.pmpEnvironment.pma.head.cfg.c.poke(true.B)
      dut.io.config.foreach { config =>
        config.sourcePrivilege.poke(trial.privilege.U); config.sourceVirtual.poke(trial.virtual.B)
        config.policy.uEnable.poke(trial.active.B); config.policy.sEnable.poke(trial.active.B)
        config.policy.uCloseWrite.poke(trial.closeWrite.B); config.policy.sCloseWrite.poke(trial.closeWrite.B)
        require(trial.configuredRegions.size <= config.entries.length)
        trial.configuredRegions.zip(config.entries).foreach { case (region, entry) =>
          entry.boundLo.poke(region.lo.U); entry.boundHi.poke(region.hi.U)
          entry.entryValid.poke(region.valid.B); entry.writeAllowed.poke(region.writable.B)
        }
      }
      dut.reset.poke(true.B); dut.clock.step(4)
      dut.reset.poke(false.B); idle(64)
      dut.io.queueEmpty.expect(true.B); dut.io.bufferEmpty.expect(true.B)
    }
    def run(): Unit = {
      if (initializeState) initialize()
      else {
        zero(dut.io.redirect); dut.io.sta.foreach(_.valid.poke(false.B)); dut.io.std.foreach(_.valid.poke(false.B))
        dut.io.cacheWrite.req.ready.poke(false.B)
        dut.io.queueEmpty.expect(true.B); dut.io.bufferEmpty.expect(true.B)
      }
      event("begin", s"\"sources\":${trial.accesses.size},\"source_privilege\":${trial.privilege},\"source_virtual\":${trial.virtual}")
      if (scenario == "remaining") {
        assert(trial.configuredRegions.forall(region => (region.lo & 7) == 0 && (region.hi & 7) == 0))
        val bounds = trial.configuredRegions.map(region => s"{\"lo\":${region.lo},\"hi\":${region.hi},\"valid\":${region.valid},\"write\":${region.writable}}")
        event("original-bound-entries", s"\"entries\":[${bounds.mkString(",")}]")
        trial.accesses.zipWithIndex.foreach { case (access, index) =>
          val first = trial.effective(access)
          val last = first + access.size - 1
          if (trial.bareAddressFault) assert(access.immediate == 0 && first == (BigInt(1) << 64) - 4 && last == (BigInt(1) << 64) + 3)
          event("original-access-range", s"\"source\":$index,\"address\":$first,\"size\":${access.size},\"last_65\":$last," +
            s"\"overflow\":${last >= (BigInt(1) << 64)},\"expected_outcome\":${trial.outcome(access)}")
        }
      }
      require(trial.accesses.size <= dut.io.slots.length - dut.io.enq.req.length)
      for (group <- trial.accesses.indices.grouped(dut.io.enq.req.length)) {
        until("SQ allocation capacity") { bool(dut.io.enq.canAccept) }
        for ((_, lane) <- group.zipWithIndex) {
          zero(dut.io.enq.req(lane).bits); dut.io.enq.req(lane).bits.numLsElem.poke(1.U)
          dut.io.enq.needAlloc(lane).poke(true.B)
        }
        val added = group.zipWithIndex.map { case (index, lane) =>
          val response = dut.io.enq.resp(lane)
          val pointer = Pointer(bool(response.flag), uint(response.value).toInt)
          if (index == 0) expectedFirst.foreach(expected => assert(pointer == expected, "Recovery did not reuse the expected full SQ slot"))
          Entry(index, pointer)
        }.toVector
        entries ++= added
        for ((entry, lane) <- added.zipWithIndex) {
          driveOwner(dut.io.enq.req(lane).bits, entry); dut.io.enq.req(lane).valid.poke(true.B)
        }
        edge()
        dut.io.enq.req.foreach(_.valid.poke(false.B)); dut.io.enq.needAlloc.foreach(_.poke(false.B))
      }
      if (entries.forall(_.blocked)) snapshot = Some(state())
      val start = cycle + 2
      issueStart = start
      val last = start + (entries.map(e => math.max(e.access.issueAt, e.access.dataAt)).max max
        (if (trial.cancelAfter.isDefined || trial.resetOwned) 2 else 0))
      while (cycle <= last) {
        dut.io.sta.foreach(_.valid.poke(false.B)); dut.io.std.foreach(_.valid.poke(false.B))
        for (entry <- entries) {
          val access = entry.access
          if (cycle == start + access.issueAt) {
            val port = dut.io.sta(access.lane)
            assert(!bool(port.valid), "Two sources compete for one STA in the test schedule")
            zero(port.bits); driveOwner(port.bits.uop, entry)
            port.bits.src(0).poke(access.address.U); port.bits.src(1).poke(access.data.U)
            port.bits.isFirstIssue.poke(true.B); port.valid.poke(true.B); port.ready.expect(true.B)
          }
          if (cycle == start + access.dataAt) {
            val port = dut.io.std(access.lane)
            assert(!bool(port.valid), "Two sources compete for one STD in the test schedule")
            zero(port.bits); driveOwner(port.bits.uop, entry)
            port.bits.src(0).poke(access.data.U)
            port.bits.isFirstIssue.poke(true.B); port.valid.poke(true.B); port.ready.expect(true.B)
          }
        }
        if (cycle == start + 2 && trial.cancelAfter.isDefined) {
          dut.io.redirect.valid.poke(true.B)
          dut.io.redirect.bits.robIdx.value.poke(trial.cancelAfter.get.U)
          dut.io.redirect.bits.level.poke(0.U)
          event("redirect", s"\"after_rob\":${trial.cancelAfter.get}")
        }
        if (cycle == start + 2 && trial.resetOwned) {
          snapshot = None
          dut.reset.poke(true.B); dut.clock.step(4); cycle += 4
          dut.reset.poke(false.B); permission.clear(); metadata.foreach(_.clear()); resetDone = true; resetForbidden = true
          event("reset-owned")
        } else edge()
        dut.io.redirect.valid.poke(false.B)
      }
      dut.io.sta.foreach(_.valid.poke(false.B)); dut.io.std.foreach(_.valid.poke(false.B))
      if (translated) {
        until("real miss and accepted PTW refill") { refillAt >= 0 && misses > 0 }
        until("PTW storage settle") { cycle >= retryAt }
        val entry = entries.head
        val port = dut.io.sta(entry.access.lane)
        zero(port.bits); driveOwner(port.bits.uop, entry)
        port.bits.src(0).poke(entry.access.address.U); port.bits.src(1).poke(entry.access.data.U)
        port.bits.isFirstIssue.poke(false.B); port.valid.poke(true.B); port.ready.expect(true.B)
        edge(); port.valid.poke(false.B)
      }
      if (trial.resetOwned) idle(64)
      val survivors = entries.filterNot(_.canceled)
      until("all final store completions and STD") {
        complete.size == survivors.size && dataAccepted.size == entries.size &&
          survivors.forall(e => !bool(dut.io.slots(e.sq.value).waitS2) && bool(dut.io.slots(e.sq.value).dataReady))
      }
      for (entry <- survivors) {
        val slot = dut.io.slots(entry.sq.value)
        slot.allocated.expect(true.B); slot.committed.expect(false.B)
        slot.rob.value.expect(entry.rob.U); slot.rob.flag.expect(false.B)
        slot.sq.value.expect(entry.sq.value.U); slot.sq.flag.expect(entry.sq.flag.B)
        slot.hasException.expect(entry.blocked.B)
        slot.pending.expect(false.B); slot.mmio.expect(false.B); slot.nc.expect(false.B)
        // Off preserves the original queue's narrower payload responsibility.
        if (enabled) assert(exceptions(slot.exceptions) == entry.expected)
        if (trial.outcome(entry.access) == 1) {
          slot.fdiException.get.tval.expect(entry.va.U); slot.fdiException.get.reason.expect(3.U)
        }
        event("final-sq-payload", s"\"source\":${entry.index},\"blocked\":${entry.blocked}")
      }
      assert(accepted.size == entries.size && dataAccepted.size == entries.size)
      val requestedOwners = entries.filter(e => !e.canceled || e.access.issueAt == 0).map(_.index).toSet
      assert(requests == (if (enabled) requestedOwners else Set.empty[Int]))
      assert((requests == responses || trial.resetOwned) && permission.isEmpty)
      if (survivors.nonEmpty) {
        dut.io.rob.pendingPtr.value.poke(survivors.last.rob.U)
        dut.io.rob.pendingPtrNext.value.poke(survivors.last.rob.U)
      }
      // The public head advance releases empty exception tokens without claiming retirement.
      dut.io.rob.scommit.poke(0.U)
      if (trial.pressure) {
        until("real Sbuffer/dataBuffer backpressure", 256) { blockedPublications >= 12 }
        assert(!bool(dut.io.queueEmpty) && actualBytes.isEmpty)
        event("publication-backpressure", s"\"held_cycles\":$blockedPublications")
        dut.io.cacheWrite.req.ready.poke(true.B)
      }
      until("all SQ entries released", 512) { bool(dut.io.queueEmpty) && published.size == survivors.size && cleared.size == entries.size }
      idle(4)
      assert(deqCount == survivors.size)
      assert(dataWrites == survivors.count(e => !e.blocked) && emptyTokens == survivors.count(_.blocked))
      for (entry <- entries.filter(_.canceled)) {
        assert(!complete(entry.index) && !published(entry.index))
        dut.io.slots(entry.sq.value).allocated.expect(false.B)
      }
      snapshot = None
      dut.io.flushBuffer.poke(true.B); dut.io.cacheWrite.req.ready.poke(true.B)
      until("actual cache flush") { bool(dut.io.flushDone) && bool(dut.io.bufferEmpty) && acknowledgements.isEmpty }
      dut.io.flushBuffer.poke(false.B); idle(4)
      val independent = survivors.filterNot(_.blocked).flatMap(e =>
        (0 until e.access.size).map(byte => (e.pa + byte) -> ((e.access.data >> (8 * byte)) & 255).toInt)).toMap
      assert(expectedBytes.toMap == independent && actualBytes.toMap == independent)
      assert(metadata.forall(_.isEmpty))
      if (translated) assert(ptwRequests > 0 && ptwResponses == 1 && misses > 0 && entries.head.attempts == 2)
      if (trial.resetOwned) assert(resetDone)
      if (scenario == "remaining" && trial.accesses.size == 2) {
        assert(entries.map(_.acceptedAt).distinct.size == 1 && entries.map(_.access.lane).toSet == Set(0, 1))
        assert(dualPublicationCycles == 1, "The same-line sources must exercise both actual publication ports together")
      }
      event("batch-case-pass", s"\"sources\":${entries.size},\"data_writes\":$dataWrites,\"empty_tokens\":$emptyTokens,\"cache_lines\":${cacheLines.size}")
    }
  }

  it should "publish allowed bytes, discard the denied token, and make subsequent progress" in {
    val root = Paths.get(sys.props("l03.runRoot")).toRealPath()
    require(Paths.get("").toRealPath() == root, "Run from the dedicated store-permission evidence directory")
    val path = root.resolve(s"store-permission-$scenario-$enabled")
    require(!Files.exists(path), s"Evidence directory already exists: $path")
    Files.createDirectory(path)
    implicit val p: Parameters = parameters()
    val options = p(DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    val workspace = new Workspace(path.resolve("compiled").toString)
    workspace.reset()
    val common = CommonCompilationSettings(availableParallelism =
      CommonCompilationSettings.AvailableParallelism.UpTo(Runtime.getRuntime.availableProcessors()))
    val settings = Backend.CompilationSettings(disabledWarnings = Seq("WIDTH", "STMTDLY"),
      disableFatalExitOnWarnings = true, enableAllAssertions = true)
    val events = mutable.ArrayBuffer.empty[String]
    try {
      val module = workspace.elaborateGeneratedModule(() => new FDIStorePermissionHarness)
      workspace.generateAdditionalSources()
      val simulation = workspace.compile(Backend.initializeFromProcessEnvironment())("verilator", common, settings, None, false)
      simulation.runElaboratedModule(module) { elaborated =>
        if (scenario == "minimal") new Driver(elaborated.wrapped, events).run()
        else if (scenario == "remaining") {
          val cases = remainingCases
          cases.foreach(trial => new MatrixDriver(elaborated.wrapped, events, trial).run())
          events += s"{\"event\":\"remaining-summary\",\"enabled\":$enabled,\"cases\":${cases.size},\"same_line_dual_cases\":3,\"backend_trap_validated\":false}"
          println(s"Store permission remaining matrix PASS enabled=$enabled cases=${cases.size}")
        } else {
          val dut = elaborated.wrapped
          val policy = matrixCases
          val translation = translationCases
          val cancellation = cancellationCases
          (policy ++ translation).foreach(trial => new MatrixDriver(dut, events, trial).run())
          cancellation.foreach { trial =>
            new MatrixDriver(dut, events, trial).run()
            // Keep the same live configuration and reset epoch while reusing the recovered SQ slot.
            new MatrixDriver(dut, events, trial.copy(name = trial.name + "-following", accesses = Seq(MatrixAccess(0x8000)),
              cancelAfter = None, resetOwned = false, robBase = 8), initializeState = false,
              expectedFirst = Some(Pointer(false, if (trial.resetOwned || trial.cancelAfter.contains(3)) 0 else 1))).run()
          }
          val count = dut.io.bufferState.length + 2 * dut.io.enq.req.length + 4
          val accesses = (0 until count).map { index =>
            val outside = index % 5 == 4
            MatrixAccess((if (outside) BigInt(0x18000) else BigInt(0x8000)) + index * 64,
              lane = index % 2, issueAt = index / 2, dataAt = index / 2,
              data = BigInt("ab21324354657687", 16) ^ BigInt(index))
          }
          new MatrixDriver(dut, events, MatrixCase("real-sbuffer-and-data-buffer-pressure", accesses,
            hi = 0x10000, pressure = true)).run()
          new Driver(dut, events).runWrap()
          val countCases = policy.size + translation.size + 2 * cancellation.size + 2
          events += s"{\"event\":\"matrix-summary\",\"cases\":$countCases,\"policy_cases\":${policy.size},\"translation_and_standard_cases\":${translation.size},\"cancel_and_following_cases\":${2 * cancellation.size},\"pressure_cases\":1,\"pointer_wrap_cases\":1,\"backend_trap_validated\":false}"
          println(s"Store permission local matrix PASS enabled=$enabled cases=$countCases")
        }
      }
    } catch {
      case failure: Throwable =>
        events += s"{\"event\":\"failure\",\"message\":\"${escaped(Option(failure.getMessage).getOrElse(failure.getClass.getName))}\"}"
        throw failure
    } finally {
      Files.write(path.resolve("events.jsonl"), events.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
    }
  }
}
