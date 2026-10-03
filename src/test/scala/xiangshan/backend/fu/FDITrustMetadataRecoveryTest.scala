// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

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
import xiangshan.backend.rob.RobPtr
import xiangshan.frontend.{BranchPredictionBundle, FDITrustInstructionImage, FtqPtr}

class FDITrustMetadataRecoveryTest extends AnyFlatSpec {
  behavior of "FDI configuration recovery into the actual instruction frontend"

  private val scenario = sys.env.getOrElse("C08_RECOVERY_SCENARIO", "minimal")
  require(Set("minimal", "full").contains(scenario))
  private val allBits = FDITrustInstructionImage.wordMask
  private val frontendAddresses = Seq(0xbc4, 0xbc5, 0xbc6, 0x9e2, 0x9e3)
  private case class Pointer(flag: Boolean, value: BigInt)
  private case class Context(mode: Int, virtual: Boolean, satp: Int,
    main: BigInt, sLo: BigInt, sHi: BigInt, uLo: BigInt, uHi: BigInt) {
    def tag(pc: BigInt): Boolean = {
      val width = if (mode != 3 && satp == 8) 39 else if (mode != 3 && satp == 9) 48 else 64
      val mask = (BigInt(1) << width) - 1
      val low = pc & mask
      val address = if (width < 64 && low.testBit(width - 1)) low | (allBits ^ mask) else low
      val enabled = if (mode == 1) main.testBit(0) else main.testBit(1)
      val lo = if (mode == 1) sLo else uLo
      val hi = if (mode == 1) sHi else uHi
      if (mode == 3 && !virtual) false
      else if (mode == 2 || mode == 3) true
      else enabled && (virtual || !(lo < hi && lo <= address && address < hi))
    }
  }
  private class Packet(val sequence: Int, val generation: Int, val pointer: Pointer,
    val start: BigInt, val nextline: BigInt, val end: BigInt, val accepted: Int, val data: BigInt) {
    var sampled = -1
    var buffered = -1
    var context: Option[Context] = None
    var canceled = false
    var consumed = 0
    val instructions: Seq[(BigInt, Int)] = (0 until 16 by 2)
      .map(offset => (start + 2 * offset, offset)).filter(_._1 < end)
  }
  private case class Token(packet: Packet, pc: BigInt, offset: Int, instruction: BigInt, tag: Boolean)
  private class Access(val pc: BigInt, val address: Int, val value: BigInt,
    val expectedWord: BigInt, val sourceMode: Int, val write: Boolean = true) {
    var uid = BigInt(0)
    var pointer: Option[Pointer] = None
    var source: Option[Token] = None
    var c0 = -1
    var effect = -1
    var mirror = -1
    var robRedirect = -1
    var ifuRedirect = -1
    var bufferFlush = -1
    var firstF2 = -1
    var firstF3 = -1
    var firstBackend = -1
    var retirement = -1
    var retireCount = 0
    var recoveredSequence = -1
  }

  private class Driver(val dut: FDITrustMetadataRecoveryHarness) {
    var cycles = 0
    var generation = 0
    var sequence = 0
    var sourceMode = 3
    var sourceVirtual = false
    var satpMode = 0
    var bufferCancellations = 0
    var f2Cancellations = 0
    var f3Cancellations = 0
    var pendingBlockedCycles = 0
    val events = mutable.ArrayBuffer.empty[String]
    val accesses = mutable.ArrayBuffer.empty[Access]
    val memory = mutable.Map.empty[BigInt, BigInt]
    val packets = mutable.ArrayBuffer.empty[Packet]
    val accepted = mutable.ArrayBuffer.empty[Token]
    val canceled = mutable.Set.empty[(Int, BigInt)]
    private val cache = mutable.Queue.empty[Packet]
    private val expected = mutable.Queue.empty[Token]
    private val owner = mutable.Map.from(frontendAddresses.map(_ -> BigInt(0)))
    private val mirror = mutable.Map.from(frontendAddresses.map(_ -> BigInt(0)))
    private var transport = Vector.fill[Option[(Int, BigInt)]](2)(None)
    private var recovery: Option[Access] = None
    private var previousRedirect = false
    private var previousGeneration = 0
    private var redirectTarget: Option[BigInt] = None
    private var expectedReturn: Option[(BigInt, Int, Boolean)] = None
    private var architecturalMode = (3, false)
    private var architecturalSatp = 0
    private var modePipeline = Vector.fill(2)((3, false))
    private var satpPipeline = Vector.fill(2)(0)
    private var programPC = BigInt(0x2000)
    private val retired = mutable.Set.empty[Pointer]
    private val allocation = mutable.Map.empty[(Int, BigInt), Pointer]
    private var floodAccess: Option[Access] = None
    private var floodOlder: Option[Pointer] = None
    private var floodRelease = -1
    private var floodOlderRetirement = -1

    def bool(value: Bool): Boolean = value.peek().litToBoolean
    def uint(value: UInt): BigInt = value.peek().litValue
    def ptr(value: FtqPtr): Pointer = Pointer(bool(value.flag), uint(value.value))
    def ptr(value: RobPtr): Pointer = Pointer(bool(value.flag), uint(value.value))
    def zero(data: Data): Unit = data match {
      case value: Bool => value.poke(false.B)
      case value: UInt => value.poke(0.U)
      case value: SInt => value.poke(0.S)
      case vector: Vec[_] => vector.foreach(zero)
      case record: Record => record.elements.values.foreach(zero)
      case other => throw new IllegalArgumentException(s"Unsupported stimulus ${other.getClass.getName}")
    }
    private def record(event: String, fields: String = ""): Unit =
      events += s"{\"cycle\":$cycles,\"event\":\"$event\"${if (fields.isEmpty) "" else "," + fields}}"
    private def identity(packet: Packet): String =
      s"\"sequence\":${packet.sequence},\"generation\":${packet.generation},\"start_pc\":${packet.start}," +
        s"\"nextline_start\":${packet.nextline},\"ftq_flag\":${packet.pointer.flag},\"ftq\":${packet.pointer.value}"
    private def instruction(pc: BigInt): BigInt = memory.getOrElse(pc, BigInt(0x13))
    private def context: Context = Context(sourceMode, sourceVirtual, satpMode,
      mirror(0xbc4), mirror(0xbc5), mirror(0xbc6), mirror(0x9e2), mirror(0x9e3))
    private def backing(address: Int): Int = if (address == 0x9e1) 0xbc4 else address
    private def finalWord(address: Int, value: BigInt): BigInt = address match {
      case 0xbc4 => value & 0x7ff
      case 0x9e1 => (owner(0xbc4) & (allBits ^ BigInt(0x7c2))) | (value & 0x7c2)
      case _ => value & (allBits ^ BigInt(7))
    }
    private def active(pointer: Pointer): Packet = packets.reverseIterator.find(p =>
      p.pointer == pointer && !p.canceled).getOrElse(throw new AssertionError(s"No live fetch $pointer"))
    private def checkConfig(): Unit = {
      frontendAddresses.zipWithIndex.foreach { case (address, index) =>
        dut.recovery.frontend(index).expect(mirror(address).U)
      }
      val value = dut.frontend.config
      value.sourcePrivilege.expect(sourceMode.U)
      value.sourceVirtual.expect(sourceVirtual.B)
      value.sEnable.expect(mirror(0xbc4).testBit(0).B)
      value.uEnable.expect(mirror(0xbc4).testBit(1).B)
      value.sBoundLo.expect(mirror(0xbc5).U)
      value.sBoundHi.expect(mirror(0xbc6).U)
      value.uBoundLo.expect(mirror(0x9e2).U)
      value.uBoundHi.expect(mirror(0x9e3).U)
      value.sv39.expect((sourceMode != 3 && satpMode == 8).B)
      value.sv48.expect((sourceMode != 3 && satpMode == 9).B)
    }
    private def cacheData(start: BigInt): BigInt = {
      val aligned = start & ~BigInt(63)
      val image = FDITrustInstructionImage.bytes((0 until 32).map(i => {
        val pc = aligned + 4 * i
        (pc, instruction(pc), 4)
      }))
      FDITrustInstructionImage.cacheWord(start, image, 64, 16)
    }
    private def driveCache(): Option[Packet] = {
      dut.frontend.cacheResponse.valid.poke(false.B)
      val response = cache.headOption.filter(packet =>
        cycles >= packet.accepted + 2 && !bool(dut.frontend.cacheStop) &&
        bool(dut.frontend.f2.valid) && !bool(dut.frontend.f2.flush))
      response.foreach { packet =>
        assert(ptr(dut.frontend.f2.request.ftqIdx) == packet.pointer)
        assert(uint(dut.frontend.f2.request.startAddr) == packet.start)
        assert(uint(dut.frontend.f2.request.nextlineStart) == packet.nextline)
        val data = dut.frontend.cacheResponse.bits
        zero(data)
        data.doubleline.poke(((packet.start & 63) >= 32).B)
        data.vaddr(0).poke(packet.start.U)
        // FTQ preserves the request's byte offset in nextlineStart. This is
        // the original request address plus one line, not an aligned line base.
        data.vaddr(1).poke(packet.nextline.U)
        data.paddr(0).poke(packet.start.U)
        data.paddr(1).poke(packet.nextline.U)
        data.data.poke(packet.data.U)
        dut.frontend.cacheResponse.valid.poke(true.B)
        dut.frontend.f2.fire.expect(true.B)
      }
      response
    }

    def edge(): Unit = {
      val response = driveCache()
      val actualMode = (uint(dut.io.mode).toInt, bool(dut.io.virtualMode))
      if (actualMode != architecturalMode) {
        val planned = expectedReturn.getOrElse(throw new AssertionError("Unplanned architectural mode change"))
        assert(actualMode == ((planned._2, planned._3)))
        architecturalMode = actualMode
        record("architectural-mode-effect", s"\"mode\":${actualMode._1},\"virtual\":${actualMode._2}")
      }
      assert(!bool(dut.io.robException.valid), s"Unexpected exception at cycle $cycles")
      dut.frontend.cacheRequestFire.expect(bool(dut.frontend.requestFire).B)
      // Configuration and mode may be in flight before a redirect, but every
      // actual F2 sample must match the independently planned architectural state.
      frontendAddresses.zipWithIndex.foreach { case (address, index) =>
        dut.recovery.frontend(index).expect(mirror(address).U)
      }
      dut.frontend.bufferFlush.expect(previousRedirect.B)
      if (bool(dut.frontend.decodeBlocked)) {
        pendingBlockedCycles += 1
        dut.frontend.decodeReady.foreach(_.expect(false.B))
      }
      if (floodOlder.nonEmpty && floodRelease < 0) {
        dut.io.traceBlocked.expect(true.B)
        dut.io.robHead.valid.expect(true.B)
        assert(ptr(dut.io.robHead.robIdx) == floodOlder.get)
        dut.io.robRetire.foreach(_.valid.expect(false.B))
        dut.recovery.effects.expect(0.U)
        dut.recovery.bus.w.valid.expect(false.B)
        assert(floodAccess.forall(_.c0 < 0), "CSR waitForward must retain the older instruction boundary")
      }
      if (bool(dut.frontend.bufferFlush)) {
        assert(previousGeneration < generation)
        expected.foreach(token => canceled += ((token.packet.sequence, token.pc)))
        val bankEntries = uint(dut.frontend.bufferEntries).toInt
        val outputEntries = dut.frontend.decode.count(port => bool(port.valid))
        bufferCancellations += bankEntries + outputEntries
        record("ibuffer-flush", s"\"old_generation\":$previousGeneration,\"tokens\":${expected.size}," +
          s"\"hardware_bank_entries\":$bankEntries,\"hardware_output_entries\":$outputEntries")
        expected.clear()
        recovery.filter(_.bufferFlush < 0).foreach(_.bufferFlush = cycles)
      }

      dut.recovery.packets.foreach { packet =>
        if (bool(packet.valid) && uint(packet.kind) == 2) {
          val pc = uint(packet.pc)
          val access = accesses.find(_.pc == pc).getOrElse(
            throw new AssertionError(s"Unplanned configuration request at $pc"))
          assert(access.c0 < 0 && uint(packet.uid) != 0)
          access.c0 = cycles
          access.uid = uint(packet.uid)
          access.pointer = Some(Pointer(bool(packet.robFlag), uint(packet.robIdx)))
          access.source = accepted.reverseIterator.find(_.pc == pc)
          assert(access.source.nonEmpty)
          if (floodAccess.contains(access)) {
            assert(floodRelease >= 0 && floodOlderRetirement > floodRelease && cycles > floodOlderRetirement,
              "The CSR may reach C0 only after the trace-blocked older instruction actually retires")
          }
          packet.csrAddress.expect(access.address.U)
          packet.permitted.expect(true.B)
          packet.writeNeeded.expect(access.write.B)
          dut.io.mode.expect(access.sourceMode.U)
          record("configuration-c0", s"\"pc\":$pc,\"uid\":${access.uid},\"address\":${access.address}," +
            s"\"source_sequence\":${access.source.get.packet.sequence}")
        }
      }
      if (bool(dut.recovery.request.valid) && uint(dut.recovery.request.bits.address) == 0x180) {
        val request = dut.recovery.request.bits
        val access = accesses.find(_.pc == uint(request.pc)).getOrElse(
          throw new AssertionError("Unplanned SATP access"))
        assert(access.c0 < 0 && access.address == 0x180)
        access.c0 = cycles
        access.uid = uint(request.uid)
        access.pointer = Some(ptr(request.robIdx))
        access.source = accepted.reverseIterator.find(_.pc == access.pc)
        assert(access.uid != 0 && access.source.nonEmpty)
        record("satp-c0", s"\"pc\":${access.pc},\"uid\":${access.uid}")
      }
      val bus = dut.recovery.bus.w
      val busToken = if (bool(bus.valid)) Some(uint(bus.bits.addr).toInt -> uint(bus.bits.data)) else None
      busToken.foreach { case (address, data) =>
        if (frontendAddresses.contains(backing(address)) || address == 0x180) {
          val access = accesses.find(a => a.write && a.c0 + 1 == cycles && a.address == address).getOrElse(
            throw new AssertionError(s"Configuration effect without its source at cycle $cycles"))
          assert(data == access.expectedWord)
          access.effect = cycles
          record("configuration-effect", s"\"pc\":${access.pc},\"uid\":${access.uid}," +
            s"\"address\":$address,\"word\":$data")
        }
      }
      val mirrorInput = dut.frontend.mirrorInput.w
      mirrorInput.valid.expect(transport.head.nonEmpty.B)
      transport.head.foreach { case (address, data) =>
        mirrorInput.bits.addr.expect(address.U)
        mirrorInput.bits.data.expect(data.U)
        accesses.find(a => a.address == address && a.effect + 2 == cycles).foreach { access =>
          access.mirror = cycles + 1
          record("frontend-mirror-input", s"\"pc\":${access.pc},\"uid\":${access.uid},\"word\":$data")
        }
      }
      if (bool(dut.recovery.robRedirect.valid)) {
        val pointer = ptr(dut.recovery.robRedirect.bits.robIdx)
        accesses.find(a => a.pointer.contains(pointer) && a.robRedirect < 0 && a.write).foreach { access =>
          access.robRedirect = cycles
          dut.recovery.robRedirect.bits.level.expect(RedirectLevel.flushAfter)
        }
      }
      val redirect = bool(dut.io.backendRedirect.valid)
      if (redirect) {
        val target = uint(dut.io.backendRedirect.bits.cfiUpdate.target)
        val pointer = ptr(dut.io.backendRedirect.bits.robIdx)
        val configuration = accesses.find(a => a.pointer.contains(pointer) && a.ifuRedirect < 0 && a.write)
        configuration match {
          case Some(access) =>
            assert(access.ifuRedirect < 0 && access.robRedirect >= 0)
            assert(target == access.pc + 4, "Configuration recovery must resume at source PC + 4")
            assert(access.mirror >= 0 && cycles >= access.mirror)
            access.ifuRedirect = cycles
            recovery = Some(access)
          case None =>
            val planned = expectedReturn.getOrElse(throw new AssertionError(s"Unplanned redirect to $target"))
            assert(target == planned._1)
            assert(architecturalMode == ((planned._2, planned._3)))
            expectedReturn = None
        }
        dut.io.ifuRedirect.valid.expect(true.B)
        dut.frontend.f2.flush.expect(true.B)
        dut.frontend.f3.flush.expect(true.B)
        dut.frontend.decodeReady.foreach(_.expect(false.B))
        if (bool(dut.frontend.f2.valid)) {
          val packet = active(ptr(dut.frontend.f2.request.ftqIdx))
          assert(packet.generation == generation)
          f2Cancellations += 1
          record("hardware-f2-cancel", identity(packet))
        }
        if (bool(dut.frontend.f3.valid)) {
          val packet = active(ptr(dut.frontend.f3.request.ftqIdx))
          assert(packet.generation == generation)
          f3Cancellations += 1
          record("hardware-f3-cancel", identity(packet))
        }
        dut.frontend.decode.filter(port => bool(port.valid)).foreach { port =>
          val packet = active(ptr(port.bits.ftqPtr))
          assert(packet.generation == generation)
          record("hardware-ibuffer-output-cancel", identity(packet) + s",\"pc\":${uint(port.bits.pc)}")
        }
        previousGeneration = generation
        packets.filter(p => !p.canceled && p.generation == generation).foreach { packet =>
          packet.canceled = true
          packet.instructions.foreach { case (pc, _) => canceled += ((packet.sequence, pc)) }
          record("old-fetch-cancel", identity(packet))
        }
        generation += 1
        cache.clear()
        redirectTarget = Some(target)
        record("ifu-flush", s"\"target\":$target,\"generation\":$generation," +
          s"\"f2_valid\":${bool(dut.frontend.f2.valid)},\"f3_valid\":${bool(dut.frontend.f3.valid)}," +
          s"\"buffer_entries\":${uint(dut.frontend.bufferEntries)}")
      }

      if (bool(dut.frontend.requestFire) && !redirect) {
        val request = dut.frontend.request.bits
        val start = uint(request.startAddr)
        val nextline = uint(request.nextlineStart)
        val end = uint(request.nextStartAddr)
        assert(!bool(request.ftqOffset.valid), "These images contain no predicted taken branch")
        assert(end > start && end <= start + 32)
        assert(nextline == start + 64, "The pinned FTQ request retains the starting byte offset in its next-line address")
        redirectTarget.foreach { target =>
          assert(start == target, "The first actual refetch did not use the backend target")
          redirectTarget = None
        }
        sequence += 1
        val packet = new Packet(sequence, generation, ptr(request.ftqIdx), start, nextline, end, cycles, cacheData(start))
        packets += packet
        cache.enqueue(packet)
        record("ftq-cache-accept", identity(packet))
      }
      if (bool(dut.frontend.f2.fire) && !bool(dut.frontend.f2.flush)) {
        val packet = active(ptr(dut.frontend.f2.request.ftqIdx))
        assert(response.contains(packet) && packet.sampled < 0)
        checkConfig()
        packet.context = Some(context)
        packet.sampled = cycles
        for (slot <- 0 until 16) {
          dut.frontend.f2.tags(slot).expect(context.tag(packet.start + 2 * slot).B)
        }
        assert(cache.dequeue() eq packet)
        recovery.filter(a => a.firstF2 < 0 && packet.generation > a.source.get.packet.generation).foreach { access =>
          assert(packet.start == access.pc + 4 && cycles >= access.mirror)
          assert(frontendAddresses.forall(address => mirror(address) == owner(address)))
          access.firstF2 = cycles
          access.recoveredSequence = packet.sequence
        }
        record("real-f2-sample", identity(packet) + s",\"mode\":$sourceMode,\"virtual\":$sourceVirtual," +
          s"\"maincfg\":${mirror(0xbc4)},\"first_tag\":${context.tag(packet.start)}")
      }
      if (bool(dut.frontend.fetch.valid) && bool(dut.frontend.fetchReady) && !redirect) {
        val fetched = dut.frontend.fetch.bits
        val packet = active(ptr(fetched.ftqPtr))
        assert(packet.sampled >= 0 && packet.buffered < 0)
        packet.buffered = cycles
        val slots = (0 until 16).filter(i => uint(fetched.valid).testBit(i) && uint(fetched.enqEnable).testBit(i))
        assert(slots == packet.instructions.map(_._2), s"IFU instruction range changed for ${identity(packet)}")
        slots.foreach { slot =>
          val pc = packet.start + 2 * slot
          val tag = packet.context.get.tag(pc)
          fetched.pc(slot).expect(pc.U)
          fetched.instrs(slot).expect(instruction(pc).U)
          fetched.fdiNotTrusted.get(slot).expect(tag.B)
          expected.enqueue(Token(packet, pc, slot, instruction(pc), tag))
        }
        recovery.filter(_.recoveredSequence == packet.sequence).foreach(_.firstF3 = cycles)
        record("real-f3-to-ibuffer", identity(packet) + s",\"instructions\":${slots.size}")
      }
      dut.frontend.decode.zipWithIndex.foreach { case (port, lane) =>
        if (bool(port.valid) && bool(dut.frontend.decodeReady(lane))) {
          assert(!redirect && !bool(dut.frontend.bufferFlush) && !bool(dut.frontend.decodeBlocked))
          assert(expected.nonEmpty, "Backend accepted an instruction without a real IFU transfer")
          val token = expected.dequeue()
          assert(!canceled((token.packet.sequence, token.pc)))
          port.bits.pc.expect(token.pc.U)
          port.bits.instr.expect(token.instruction.U)
          port.bits.ftqOffset.expect(token.offset.U)
          assert(ptr(port.bits.ftqPtr) == token.packet.pointer)
          port.bits.fdiNotTrusted.get.expect(token.tag.B)
          port.bits.exceptionVec.foreach(_.expect(false.B))
          token.packet.consumed += 1
          accepted += token
          recovery.filter(a => a.recoveredSequence == token.packet.sequence && a.firstBackend < 0).foreach { access =>
            assert(token.pc == access.pc + 4 && access.firstF3 >= 0)
            access.firstBackend = cycles
          }
          record("real-ibuffer-backend-accept", identity(token.packet) +
            s",\"pc\":${token.pc},\"instruction\":${token.instruction},\"offset\":${token.offset},\"tag\":${token.tag}")
        }
      }
      dut.io.robEnq.foreach { port =>
        if (bool(port.valid) && bool(port.bits.first)) {
          val pointer = ptr(port.bits.ftqIdx)
          val offset = uint(port.bits.ftqOffset).toInt
          val token = accepted.reverseIterator.find(t => t.packet.pointer == pointer && t.offset == offset)
            .getOrElse(throw new AssertionError("ROB allocation has no original frontend source"))
          assert(!canceled((token.packet.sequence, token.pc)), "Canceled source reached ROB allocation")
          allocation((token.packet.sequence, token.pc)) = ptr(port.bits.robIdx)
          retired -= ptr(port.bits.robIdx)
        }
      }
      dut.recovery.retired.foreach { port =>
        if (bool(port.valid)) {
          val pointer = Pointer(bool(port.robFlag), uint(port.robIdx))
          retired += pointer
          accesses.find(_.uid == uint(port.uid)).foreach { access =>
            assert(access.retireCount == 0 && (!access.write || access.robRedirect >= 0))
            access.retireCount += 1
            access.retirement = cycles
            record("writer-retirement", s"\"pc\":${access.pc},\"uid\":${access.uid}")
          }
        }
      }
      dut.io.robRetire.foreach { port =>
        if (bool(port.valid) && floodOlder.contains(ptr(port.bits))) {
          assert(floodRelease >= 0 && cycles > floodRelease && floodOlderRetirement < 0)
          floodOlderRetirement = cycles
          record("flood-older-retirement", s"\"release_cycle\":$floodRelease," +
            s"\"rob_flag\":${floodOlder.get.flag},\"rob\":${floodOlder.get.value}")
        }
      }
      transport.head.foreach { case (address, data) =>
        if (mirror.contains(backing(address))) mirror(backing(address)) = data
      }
      transport = transport.tail :+ busToken
      // These heads describe the next pre-edge sample after two hardware registers.
      // SATP's current write effect becomes architectural only after this shift.
      modePipeline = modePipeline.tail :+ actualMode
      sourceMode = modePipeline.head._1
      sourceVirtual = modePipeline.head._2
      satpPipeline = satpPipeline.tail :+ architecturalSatp
      satpMode = satpPipeline.head
      busToken.foreach { case (address, data) =>
        if (owner.contains(backing(address))) owner(backing(address)) = data
        if (address == 0x180) architecturalSatp = ((data >> 60) & 15).toInt
      }
      previousRedirect = redirect
      dut.clock.step()
      cycles += 1
    }

    def idle(count: Int): Unit = (0 until count).foreach(_ => edge())
    def until(description: String, maximum: Int = 2000)(condition: => Boolean): Unit = {
      var count = 0
      while (!condition && count < maximum) { edge(); count += 1 }
      assert(condition, s"Timed out waiting for $description at cycle $cycles")
    }
    def reset(): Unit = {
      (Seq(dut.io.instruction) ++ dut.io.instructionTail.toSeq).foreach { lane =>
        lane.valid.poke(false.B)
        zero(lane.bits)
      }
      zero(dut.io.bpu.resp.bits)
      dut.io.bpu.resp.valid.poke(false.B)
      zero(dut.io.predecode)
      zero(dut.io.interrupts)
      dut.io.fetchReady.poke(false.B)
      dut.io.traceEnable.poke(false.B)
      dut.io.traceStall.poke(false.B)
      dut.frontend.cacheReady.poke(true.B)
      zero(dut.frontend.cacheResponse)
      dut.reset.poke(true.B)
      dut.clock.step(10)
      dut.reset.poke(false.B)
      idle(400)
      dut.io.mode.expect(3.U)
      checkConfig()
    }
    private def prediction(stage: BranchPredictionBundle, pc: BigInt, pointer: Pointer): Unit = {
      zero(stage)
      stage.pc.foreach(_.poke(pc.U))
      stage.valid.foreach(_.poke(true.B))
      stage.ftq_idx.flag.poke(pointer.flag.B)
      stage.ftq_idx.value.poke(pointer.value.U)
    }
    def predict(pc: BigInt): Unit = {
      val pointer = ptr(dut.io.ftqNext)
      zero(dut.io.bpu.resp.bits)
      prediction(dut.io.bpu.resp.bits.s1, pc, pointer)
      dut.io.bpu.resp.valid.poke(true.B)
      until("BPU acceptance")(bool(dut.io.bpu.resp.ready))
      edge()
      dut.io.bpu.resp.valid.poke(false.B)
      zero(dut.io.bpu.resp.bits.s1)
      prediction(dut.io.bpu.resp.bits.s2, pc, pointer)
      edge()
      zero(dut.io.bpu.resp.bits.s2)
      prediction(dut.io.bpu.resp.bits.s3, pc, pointer)
      edge()
      zero(dut.io.bpu.resp.bits.s3)
    }
    private def finishWrite(access: Access): Access = {
      val pc = access.pc
      until("writer's actual frontend redirect")(access.ifuRedirect >= 0)
      idle(4)
      if (!packets.exists(p => p.generation == generation && p.start == pc + 4)) predict(pc + 4)
      until("first refetched instruction and unique writer retirement") {
        access.firstBackend >= 0 && access.retireCount == 1
      }
      assert(access.effect == access.c0 + 1 && access.mirror == access.effect + 3)
      assert(access.bufferFlush == access.ifuRedirect + 1)
      assert(access.firstF2 > access.ifuRedirect && access.firstF3 > access.firstF2)
      assert(access.firstBackend > access.firstF3 && access.retirement > access.robRedirect)
      record("configuration-recovery-summary", s"\"pc\":$pc,\"uid\":${access.uid}," +
        s"\"address\":${access.address},\"word\":${access.expectedWord},\"c0\":${access.c0}," +
        s"\"effect\":${access.effect},\"mirror\":${access.mirror},\"rob_redirect\":${access.robRedirect}," +
        s"\"ifu_flush\":${access.ifuRedirect},\"ibuffer_flush\":${access.bufferFlush}," +
        s"\"first_f2\":${access.firstF2},\"first_f3\":${access.firstF3}," +
        s"\"first_backend\":${access.firstBackend},\"retire\":${access.retirement},\"retire_count\":${access.retireCount}")
      drain()
      recovery = None
      access
    }
    def write(pc: BigInt, address: Int, value: Int, flood: Boolean = false): Access = {
      require(value >= 0 && value < 32)
      val access = new Access(pc, address, value, finalWord(address, value), sourceMode)
      accesses += access
      memory(pc) = (BigInt(address) << 20) | (BigInt(value) << 15) | (BigInt(5) << 12) | 0x73
      if (flood) {
        // Hold an older ROB entry through the external trace interface.
        // Its presence, rather than the trace stall alone, blocks CSR waitForward.
        dut.io.traceEnable.poke(true.B)
        dut.io.traceStall.poke(true.B)
        until("registered external trace backpressure", 16)(bool(dut.io.traceBlocked))
        val olderPC = pc - 64
        memory(olderPC) = addi(5, 0, 1)
        predict(olderPC)
        until("older instruction at the writeback-complete ROB head", 500) {
          bool(dut.io.robHead.valid) && bool(dut.io.robHead.writebacked) &&
            allocation.exists { case ((source, sourcePC), pointer) =>
              sourcePC == olderPC && packets.exists(_.sequence == source) && pointer == ptr(dut.io.robHead.robIdx)
            }
        }
        floodOlder = Some(ptr(dut.io.robHead.robIdx))
        floodAccess = Some(access)
        record("flood-older-held", s"\"pc\":$olderPC,\"rob_flag\":${floodOlder.get.flag}," +
          s"\"rob\":${floodOlder.get.value},\"writer_pc\":$pc")
        predict(pc)
        (1 to 12).foreach(i => predict(pc + 32 * i))
        until("actual old F2/F3/IBuffer occupancy before CSR acceptance", 500) {
          bool(dut.io.traceBlocked) && bool(dut.io.robHead.valid) && bool(dut.io.robHead.writebacked) &&
            ptr(dut.io.robHead.robIdx) == floodOlder.get && access.c0 < 0 &&
            uint(dut.frontend.bufferEntries) > 0 && dut.frontend.decode.exists(port => bool(port.valid)) &&
            bool(dut.frontend.f3.valid) && !bool(dut.frontend.fetchReady) &&
            bool(dut.frontend.f2.valid) && bool(dut.frontend.cacheStop)
        }
        assert(access.effect < 0 && floodOlderRetirement < 0)
        assert(uint(dut.frontend.f2.request.startAddr) > pc && uint(dut.frontend.f3.request.startAddr) > pc)
        dut.frontend.decode.filter(port => bool(port.valid)).foreach(port => assert(uint(port.bits.pc) > pc))
        floodRelease = cycles
        record("flood-trace-release", s"\"writer_pc\":$pc,\"bank_entries\":${uint(dut.frontend.bufferEntries)}," +
          s"\"output_entries\":${dut.frontend.decode.count(port => bool(port.valid))}," +
          s"\"f2_pc\":${uint(dut.frontend.f2.request.startAddr)}," +
          s"\"f3_pc\":${uint(dut.frontend.f3.request.startAddr)},\"writer_c0\":${access.c0}")
        dut.io.traceStall.poke(false.B)
      } else predict(pc)
      val completed = finishWrite(access)
      if (flood) {
        dut.io.traceEnable.poke(false.B)
        floodOlder = None
        floodAccess = None
      }
      completed
    }
    private def drain(): Unit = {
      until("instruction frontend and ROB drain") {
        cache.isEmpty && expected.isEmpty && !bool(dut.frontend.f2.valid) &&
          !bool(dut.frontend.f3.valid) && dut.frontend.decode.forall(port => !bool(port.valid)) &&
          !bool(dut.io.robHead.valid)
      }
      idle(12)
    }
    private def reserve(instructions: Seq[BigInt]): BigInt = {
      require(instructions.nonEmpty && instructions.size <= 8)
      val pc = programPC
      instructions.zipWithIndex.foreach { case (word, i) =>
        assert(!memory.contains(pc + 4 * i))
        memory(pc + 4 * i) = word
      }
      programPC += 64
      pc
    }
    private def execute(instructions: Seq[BigInt]): BigInt = {
      val pc = reserve(instructions)
      val before = accepted.size
      predict(pc)
      until("raw instruction packet accepted by backend") {
        accepted.drop(before).exists(_.pc == pc + 4 * (instructions.size - 1))
      }
      drain()
      pc
    }
    private def addi(rd: Int, rs: Int, immediate: Int): BigInt =
      (BigInt(immediate & 0xfff) << 20) | (BigInt(rs) << 15) | (BigInt(rd) << 7) | 0x13
    private def loadX1(value: BigInt): Unit = {
      require(value >= 0 && value <= allBits)
      val bytes = (7 to 0 by -1).map(i => ((value >> (8 * i)) & 255).toInt).dropWhile(_ == 0)
      val words = if (bytes.isEmpty) Seq(addi(1, 0, 0)) else {
        Seq(addi(1, 0, bytes.head)) ++ bytes.tail.flatMap { byte =>
          Seq(BigInt(0x00809093)) ++ Option.when(byte != 0)(addi(1, 1, byte)).toSeq
        }
      }
      words.grouped(8).foreach(group => execute(group))
    }
    private def csrWrite(address: Int): BigInt =
      (BigInt(address) << 20) | (BigInt(1) << 15) | (BigInt(1) << 12) | 0x73
    private def ordinaryCSR(address: Int, value: BigInt): Unit = {
      loadX1(value)
      execute(Seq(csrWrite(address)))
    }
    private def writeValue(address: Int, value: BigInt): Access = {
      loadX1(value)
      val pc = reserve(Seq(csrWrite(address)))
      val word = if (address == 0x180) value else finalWord(address, value)
      val access = new Access(pc, address, value, word, sourceMode)
      accesses += access
      predict(pc)
      finishWrite(access)
    }
    private def readAlias(): Unit = {
      val before = generation
      val words = owner.toMap
      val raw = (BigInt(0x9e1) << 20) | (BigInt(2) << 12) | (BigInt(3) << 7) | 0x73
      val pc = reserve(Seq(raw))
      val access = new Access(pc, 0x9e1, 0, owner(0xbc4), sourceMode, write = false)
      accesses += access
      predict(pc)
      until("pure-read unique retirement")(access.retireCount == 1)
      drain()
      assert(access.effect < 0 && access.ifuRedirect < 0 && generation == before && owner.toMap == words)
      record("pure-read-no-recovery", s"\"pc\":$pc,\"uid\":${access.uid}")
    }
    private def enter(target: BigInt, mode: Int, instruction: BigInt): Unit = {
      val before = generation
      val pc = reserve(Seq(instruction))
      expectedReturn = Some((target, mode, false))
      predict(pc)
      until("architectural return redirect")(generation > before)
      idle(4)
      if (!packets.exists(p => p.generation == generation && p.start == target)) predict(target)
      until("first actual instruction in the returned source mode") {
        accepted.exists(t => t.packet.generation == generation && t.pc == target)
      }
      dut.io.mode.expect(mode.U)
      dut.io.virtualMode.expect(false.B)
      checkConfig()
      val first = accepted.find(t => t.packet.generation == generation && t.pc == target).get
      assert(first.packet.context.get.mode == mode)
      record("return-first-source", identity(first.packet) +
        s",\"pc\":$target,\"mode\":$mode,\"tag\":${first.tag},\"maincfg\":${mirror(0xbc4)}")
      drain()
    }
    def fullMatrix(): Unit = {
      assert(bufferCancellations > 0 && f2Cancellations > 0 && f3Cancellations > 0,
        "The flood must create real old work in IBuffer, F2 and F3 before cancellation")
      assert(pendingBlockedCycles > 0)
      write(0x1400, 0xbc4, 3)
      writeValue(0xbc5, 0x8000)
      writeValue(0xbc6, 0x9000)
      writeValue(0xbc6, 0xa000)
      writeValue(0x9e2, 0xc000)
      writeValue(0x9e3, 0xd000)
      ordinaryCSR(0x30c, 1)
      ordinaryCSR(0x300, BigInt(1) << 11)
      ordinaryCSR(0x341, 0x9ff0)
      enter(0x9ff0, 1, BigInt(0x30200073))
      val supervisor = accepted.filter(t => t.packet.generation == generation && t.pc >= 0x9ff0 && t.pc < 0xa010)
      assert(supervisor.exists(t => t.pc < 0xa000 && !t.tag))
      assert(supervisor.exists(t => t.pc >= 0xa000 && t.tag),
        "HS first packet must discriminate the updated S upper bound and enable")
      programPC = 0x8100
      writeValue(0x9e1, 0)
      assert(owner(0xbc4) == 1, "U-view writes must preserve the S enable bit")
      writeValue(0x9e1, 2)
      writeValue(0x9e1, 2)
      writeValue(0x9e2, 0xc000)
      writeValue(0x9e3, 0xc010)
      readAlias()
      writeValue(0x180, BigInt(8) << 60)
      assert(satpMode == 8)
      writeValue(0x180, BigInt(9) << 60)
      assert(satpMode == 9)
      writeValue(0x180, 0)
      ordinaryCSR(0x100, 0)
      ordinaryCSR(0x141, 0xc000)
      enter(0xc000, 0, BigInt(0x10200073))
      val user = accepted.filter(t => t.packet.generation == generation && t.pc >= 0xc000 && t.pc < 0xc020)
      assert(user.exists(t => t.pc < 0xc010 && !t.tag))
      assert(user.exists(t => t.pc >= 0xc010 && t.tag), "HU first packet must sample the updated half-open range")
      record("full-matrix-summary", s"\"writes\":${accesses.count(_.write)}," +
        s"\"reads\":${accesses.count(a => !a.write)},\"canceled_ibuffer_entries\":$bufferCancellations," +
        s"\"canceled_f2\":$f2Cancellations,\"canceled_f3\":$f3Cancellations," +
        s"\"pending_blocked_cycles\":$pendingBlockedCycles")
    }
  }

  it should "sample the distributed configuration after the real writer's ROB and frontend recovery" in {
    val root = Paths.get(sys.props("c08.runRoot")).toRealPath()
    require(Paths.get("").toRealPath() == root, "Run from the dedicated C08 evidence directory")
    val path = root.resolve("recovery")
    require(!Files.exists(path), s"Evidence directory already exists: $path")
    Files.createDirectory(path)
    implicit val p: Parameters = UserTimerDeliveryParameters(enabled = true)
    val options = p(DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    val workspace = new Workspace(path.resolve("compiled").toString)
    workspace.reset()
    val module = workspace.elaborateGeneratedModule(() => new FDITrustMetadataRecoveryHarness)
    workspace.generateAdditionalSources()
    val common = CommonCompilationSettings(availableParallelism =
      CommonCompilationSettings.AvailableParallelism.UpTo(Runtime.getRuntime.availableProcessors()))
    val settings = Backend.CompilationSettings(disabledWarnings = Seq("WIDTH", "STMTDLY"),
      disableFatalExitOnWarnings = true, enableAllAssertions = true)
    val simulation = workspace.compile(Backend.initializeFromProcessEnvironment())(
      "verilator", common, settings, None, false)
    simulation.runElaboratedModule(module) { elaborated =>
      val driver = new Driver(elaborated.wrapped)
      try {
        driver.reset()
        driver.write(0x1000, 0xbc4, 3, flood = scenario == "full")
        if (scenario == "full") driver.fullMatrix()
        driver.idle(24)
        assert(driver.accesses.forall(_.retireCount == 1))
        println(s"FDI actual frontend recovery PASS scenario=$scenario cycles=${driver.cycles} " +
          s"accesses=${driver.accesses.size} backend=production ftq=production ifu=production ibuffer=production " +
          "cache=external-byte-service bpu=external-predictions")
      } finally {
        Files.write(path.resolve("recovery-events.jsonl"),
          driver.events.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
      }
    }
  }
}
