// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

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
import xiangshan.backend.rob.RobPtr
import xiangshan.cache.MemoryOpConstants
import xiangshan.frontend.{BranchPredictionBundle, FtqPtr}
import xiangshan.mem.SqPtr

class FDIStorePermissionBackendTest extends AnyFlatSpec {
  behavior of "Store permission and precise traps through the production Backend"

  private val enabled = sys.env.getOrElse("L03_FDI_ENABLED",
    throw new IllegalArgumentException("L03_FDI_ENABLED must be explicit")).toBoolean
  private val scenario = sys.env.getOrElse("L03_SCENARIO", "backend-minimal")
  require(Set("minimal", "backend-minimal", "full", "source-mode").contains(scenario), "Choose a supported Backend scenario")
  private val baseAddress = BigInt("80000000", 16)
  private val wordMask = (BigInt(1) << 64) - 1
  private case class Pointer(flag: Boolean, value: BigInt)
  private case class Owner(rob: Pointer, sq: Pointer)
  private case class Source(pc: BigInt, address: BigInt, data: BigInt, privilege: Int, denied: Boolean,
    notTrusted: Boolean = true, expectedBoundHi: Option[BigInt] = None,
    virtual: Boolean = false, outcomeOverride: Option[Int] = None,
    standardFaults: Set[Int] = Set.empty, pmpFault: Boolean = false) {
    def outcome: Int = outcomeOverride.getOrElse(if (denied) 1 else 0)
    def faults: Set[Int] = standardFaults ++ (outcome match {
      case 1 => Set(if (privilege == 1) 25 else 24)
      case 2 => Set(2)
      case _ => Set.empty[Int]
    })
    def blocked: Boolean = faults.nonEmpty
  }
  private val minimumSources = Seq(
    Source(0x4000, baseAddress, 0x5a5, 0, denied = false),
    Source(0x4004, baseAddress + 64, 0x6b6, 0, denied = enabled),
    Source(0x6000, baseAddress + 8, 0x5a5, if (enabled) 3 else 0, denied = false))
  private val recoverySources = Seq(
    Source(0x4ffc, baseAddress + 64, 0x5a5, 0, denied = false, expectedBoundHi = Some(baseAddress + 72)),
    Source(0x5008, baseAddress + 64, 0x6b6, 0, denied = enabled, expectedBoundHi = Some(baseAddress + 8)))

  private case class TrapCase(name: String, privilege: Int, virtual: Boolean = false,
    delegated: Boolean = false, pmpFault: Boolean = false) {
    val source = Source(0x4000, baseAddress + 64, 0x6b6, privilege, denied = enabled && !virtual,
      expectedBoundHi = Some(baseAddress + 8), virtual = virtual,
      outcomeOverride = Some(if (!enabled) 0 else if (virtual) 2 else 1),
      standardFaults = if (pmpFault) Set(7) else Set.empty[Int], pmpFault = pmpFault)
    val finalCause: Int = if (pmpFault) 7 else if (!enabled) -1 else if (virtual) 2 else 25
    val supervisorEntry: Boolean = enabled && delegated
    // A legal decoded Store has no native TrapInstMod capture when P05 later produces II.
    val finalTval: BigInt = if (finalCause == 2) BigInt(0) else source.address
    val finalReason: Int = if (finalCause == 25) 3 else 4
    val reasonEffects: Int = if (enabled && finalCause == 25) 1 else 0
  }

  private case class SourceModeCase(dataVirtual: Boolean) {
    val name: String = if (dataVirtual) "machine-mprv-vu" else "machine-mprv-u"
    val source = Source(0x4000, baseAddress + 64, 0x6b6, privilege = 3, denied = false,
      expectedBoundHi = Some(baseAddress + 8))
    // RV64 mstatus changes data translation only: MPRV=1, MPP=U, MPV as selected.
    val status: BigInt = BigInt(0x20000) | (if (dataVirtual) BigInt(1) << 39 else BigInt(0))
    val statusMask: BigInt = BigInt(0x21800) | (BigInt(1) << 39)
  }

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
  private def addi(rd: Int, rs: Int, immediate: Int): BigInt =
    (BigInt(immediate & 0xfff) << 20) | (BigInt(rs) << 15) | (BigInt(rd) << 7) | 0x13
  private def slli(rd: Int, rs: Int, shift: Int): BigInt =
    (BigInt(shift) << 20) | (BigInt(rs) << 15) | (BigInt(1) << 12) | (BigInt(rd) << 7) | 0x13
  private def lui(rd: Int, upper: Int): BigInt = (BigInt(upper) << 12) | (BigInt(rd) << 7) | 0x37
  private def csrWrite(address: Int, rs: Int): BigInt =
    (BigInt(address) << 20) | (BigInt(rs) << 15) | (BigInt(1) << 12) | 0x73
  private def csrImmediate(address: Int, value: Int): BigInt =
    (BigInt(address) << 20) | (BigInt(value) << 15) | (BigInt(5) << 12) | 0x73
  private def store(rs: Int, offset: Int): BigInt =
    (BigInt((offset >> 5) & 127) << 25) | (BigInt(rs) << 20) | (BigInt(1) << 15) |
      (BigInt(3) << 12) | (BigInt(offset & 31) << 7) | 0x23

  private class Driver(dut: FDIStorePermissionBackendHarness, events: mutable.ArrayBuffer[String], recovery: Boolean = false,
    trapCase: Option[TrapCase] = None, sourceModeCase: Option[SourceModeCase] = None) {
    require(Seq(recovery, trapCase.nonEmpty, sourceModeCase.nonEmpty).count(value => value) <= 1)
    private val sources = sourceModeCase.map(trial => Seq(trial.source))
      .orElse(trapCase.map(trial => Seq(trial.source))).getOrElse(if (recovery) recoverySources else minimumSources)
    private val sourceByPc = sources.map(source => source.pc -> source).toMap
    private val deniedPc = trapCase.map(_.source.pc).getOrElse(BigInt(if (recovery) 0x5008 else 0x4004))
    var cycle = 0
    private var csrResponses = 0
    private var traps = 0
    private var supervisorTraps = 0
    private var reasonEffects = 0
    private var robFaults = 0
    private var cancellations = 0
    private var dequeues = 0
    private var emptyTokens = 0
    private var byteWrites = 0
    private val fetches = mutable.Map.empty[Pointer, BigInt]
    // Entries become recovery witnesses only after edge() consumes the observed FTQ redirect.
    private val frontendRedirects = mutable.Map.empty[(Pointer, BigInt), BigInt]
    private val sourceLocations = mutable.Map.empty[(Pointer, BigInt), Source]
    private val sourceRobs = mutable.Map.empty[Pointer, Source]
    private val sourceOwners = mutable.Map.empty[Owner, Source]
    private val issued = mutable.Set.empty[BigInt]
    private val addressOwners = mutable.Map.empty[BigInt, Owner]
    private val dataOwners = mutable.Map.empty[BigInt, Owner]
    private val dataIssued = mutable.Set.empty[BigInt]
    private val completed = mutable.Set.empty[BigInt]
    private val finalSlots = mutable.Set.empty[BigInt]
    private val retired = mutable.Set.empty[BigInt]
    private val published = mutable.Set.empty[BigInt]
    private val permissionRequests = mutable.Set.empty[BigInt]
    private val permissionResponses = mutable.Set.empty[BigInt]
    private val pendingPermissions = mutable.Map.empty[BigInt, (Source, Int)]
    private val metadataReplies = Seq.fill(dut.memory.metadata.length)(mutable.Queue.empty[Int])
    private val cacheReplies = mutable.Queue.empty[(Int, BigInt)]
    private val expectedBytes = mutable.Map.empty[BigInt, Int]
    private val actualBytes = mutable.Map.empty[BigInt, Int]
    private val sentLines = mutable.Set.empty[BigInt]
    private var deniedTrapOwner: Option[Owner] = None
    private var oldYoungLocation: Option[(Pointer, BigInt)] = None
    private var oldYoungHeld = false
    private var oldYoungCanceled = false
    private var oldYoungCleared = false
    private var recoveryReplay = false
    private var tighteningWriteCycle = -1
    private var oldRetireCycle = -1
    private var heldCacheEffect: Option[(BigInt, BigInt, Map[BigInt, Int])] = None
    private var cacheWrites = 0
    private var sourceModeStatusWrites = 0
    private var sourceModeActive = false

    private def pointer(value: RobPtr): Pointer = Pointer(bool(value.flag), uint(value.value))
    private def pointer(value: SqPtr): Pointer = Pointer(bool(value.flag), uint(value.value))
    private def pointer(value: FtqPtr): Pointer = Pointer(bool(value.flag), uint(value.value))
    private def owner(uop: DynInst): Owner = Owner(pointer(uop.robIdx), pointer(uop.sqIdx))
    private def setPointer(value: FtqPtr, from: Pointer): Unit = {
      value.flag.poke(from.flag.B); value.value.poke(from.value.U)
    }
    private def event(kind: String, fields: String = ""): Unit = {
      val group = sourceModeCase.map(_.name).orElse(trapCase.map(_.name))
        .getOrElse(if (recovery) "configuration-recovery" else "minimum")
      events += s"{\"case\":\"$group\",\"cycle\":$cycle,\"event\":\"$kind\"${if (fields.isEmpty) "" else "," + fields}}"
    }
    private def identity(key: Owner, source: Source): String =
      s"\"pc\":${source.pc},\"rob_flag\":${key.rob.flag},\"rob\":${key.rob.value}," +
        s"\"sq_flag\":${key.sq.flag},\"sq\":${key.sq.value}"
    private def associate(uop: DynInst): (Owner, Source) = {
      val key = owner(uop)
      val source = sourceRobs.getOrElse(key.rob, throw new AssertionError(s"Issue without a known renamed source: $key"))
      sourceOwners.get(key).foreach(existing => assert(existing == source))
      sourceOwners(key) = source
      (key, source)
    }
    private def known(uop: DynInst): (Owner, Source) = {
      val key = owner(uop)
      (key, sourceOwners.getOrElse(key, throw new AssertionError(s"Result changed its full ROB/SQ owner: $key")))
    }

    private def checkSourceMode(): Unit = sourceModeCase.foreach { trial =>
      dut.io.mode.expect(3.U); dut.io.virtualMode.expect(false.B)
      dut.memory.sourcePrivilege.expect(3.U); dut.memory.sourceVirtual.expect(false.B)
      dut.memory.dataPrivilege.expect(0.U); dut.memory.dataVirtual.expect(trial.dataVirtual.B)
      assert((uint(dut.memory.machineStatus) & trial.statusMask) == trial.status)
    }

    private def edge(): Unit = {
      for ((port, lane) <- dut.memory.metadata.zipWithIndex) {
        port.resp.valid.poke(false.B)
        if (metadataReplies(lane).headOption.contains(cycle)) {
          metadataReplies(lane).dequeue()
          port.resp.valid.poke(true.B)
          event("metadata-hit-response", s"\"lane\":$lane")
        }
      }
      dut.memory.cacheWrite.main_pipe_hit_resp.valid.poke(false.B)
      if (cacheReplies.headOption.exists(_._1 == cycle)) {
        val (_, id) = cacheReplies.dequeue()
        dut.memory.cacheWrite.main_pipe_hit_resp.valid.poke(true.B)
        dut.memory.cacheWrite.main_pipe_hit_resp.bits.id.poke(id.U)
        event("cache-write-ack", s"\"id\":$id")
      }
      if (bool(dut.io.bpu.resp.valid) && bool(dut.io.bpu.resp.ready))
        assert(!bool(dut.io.backendRedirect.valid) && !bool(dut.io.frontendRedirect.valid),
          "A surviving BPU response must follow the actual FTQ redirect consumption")
      if (bool(dut.io.fetchRequest.valid) && bool(dut.io.fetchReady) && !bool(dut.io.ifuRedirect.valid))
        fetches(pointer(dut.io.fetchRequest.bits.ftqIdx)) = uint(dut.io.fetchRequest.bits.startAddr)
      if (bool(dut.io.csrResponse.valid)) csrResponses += 1
      if (sourceModeCase.nonEmpty && bool(dut.io.legalCSRWrite.valid) && uint(dut.io.legalCSRWrite.bits) == 0x300) {
        dut.memory.csrSourcePc.expect(0x1400.U)
        dut.io.mode.expect(3.U); dut.io.virtualMode.expect(false.B)
        sourceModeStatusWrites += 1
        event("actual-mstatus-write", "\"pc\":5120")
      }
      if (sourceModeActive) checkSourceMode()
      if (bool(dut.io.frontendRedirect.valid)) {
        val redirect = dut.io.frontendRedirect.bits
        val ptr = pointer(redirect.ftqIdx)
        val offset = uint(redirect.ftqOffset)
        val target = uint(redirect.cfiUpdate.target)
        frontendRedirects((ptr, offset)) = target
        event("ftq-redirect", s"\"ftq_flag\":${ptr.flag},\"ftq\":${ptr.value},\"offset\":$offset,\"target\":$target")
      }
      if (recovery && enabled) {
        val oldPresent = dut.memory.dispatchPending.zipWithIndex.filter { case (port, _) =>
          bool(port.valid) && oldYoungLocation.contains((pointer(port.bits.ftqPtr), uint(port.bits.ftqOffset)))
        }
        oldPresent.foreach { case (port, lane) =>
          assert(!bool(dut.memory.dispatchReady(lane)), "Old younger Store crossed the serializing CSR writer")
          assert((uint(port.bits.instr) & 127) == 0x23)
          oldYoungHeld = true
          val location = oldYoungLocation.get
          event("old-young-held", s"\"pc\":20488,\"rob_flag\":${bool(port.bits.robIdx.flag)},\"rob\":${uint(port.bits.robIdx.value)}," +
            s"\"ftq_flag\":${location._1.flag},\"ftq\":${location._1.value},\"offset\":${location._2},\"lane\":$lane")
        }
        if (oldYoungCanceled && !recoveryReplay) {
          assert(oldPresent.isEmpty, "CSR recovery did not clear the old younger Store")
          oldYoungCleared = true
        }
        if (bool(dut.memory.dispatchRedirect.valid) && tighteningWriteCycle >= 0 && !oldYoungCanceled) {
          assert(oldYoungHeld && oldPresent.nonEmpty, "CSR recovery must cancel a real pending old Store")
          val expectedLocation = oldYoungLocation.get
          assert(pointer(dut.memory.dispatchRedirect.bits.ftqIdx) == expectedLocation._1)
          dut.memory.dispatchRedirect.bits.ftqOffset.expect(2.U)
          oldYoungCanceled = true
          event("old-young-canceled-by-csr", s"\"ftq_flag\":${expectedLocation._1.flag},\"ftq\":${expectedLocation._1.value}," +
            s"\"writer_offset\":2,\"old_store_offset\":${expectedLocation._2}")
        }
        if (bool(dut.io.legalCSRWrite.valid) && uint(dut.io.legalCSRWrite.bits) == 0x891 &&
          uint(dut.memory.csrSourcePc) == 0x5004) {
          assert(oldRetireCycle >= 0 && oldRetireCycle < cycle && published.contains(BigInt(0x4ffc)))
          dut.memory.bufferEmpty.expect(false.B)
          assert(cacheWrites == 0)
          tighteningWriteCycle = cycle
          event("trusted-bound-tightening", s"\"pc\":20484,\"old_retired_cycle\":$oldRetireCycle")
        }
      }
      if (recovery || trapCase.nonEmpty || sourceModeCase.nonEmpty) {
        dut.io.criticalErrorState.expect(false.B)
        dut.io.debugEntry.expect(false.B); dut.io.mnEntry.expect(false.B); dut.io.vsEntry.expect(false.B)
      }
      if (bool(dut.memory.reasonEffect)) {
        assert(sourceModeCase.isEmpty, "A machine-source Store changed FReason")
        reasonEffects += 1
        assert(enabled && (bool(dut.io.mEntry) || bool(dut.io.hsEntry)), "FReason effect has no actual native trap event")
        trapCase.foreach(trial => assert(trial.reasonEffects == 1, "A standard or guest-II winner wrote FReason"))
        event("actual-freason-effect")
      }
      if (bool(dut.io.mEntry)) {
        assert(sourceModeCase.isEmpty, "A machine-source Store caused a trap")
        traps += 1
        if (trapCase.nonEmpty) {
          val trial = trapCase.get
          assert(trial.source.blocked && !trial.supervisorEntry)
        } else assert(enabled, "Feature-disabled ordinary stores must not trap")
        assert(completed.contains(deniedPc), "A trap must follow the faulting store's real completion")
        assert(!retired.contains(deniedPc), "A faulting store must not retire")
        event("machine-trap")
      }
      if (bool(dut.io.hsEntry)) {
        supervisorTraps += 1
        assert(trapCase.exists(_.supervisorEntry), "Unexpected supervisor trap entry")
        assert(completed.contains(deniedPc) && !retired.contains(deniedPc))
        event("supervisor-trap")
      }
      if (bool(dut.io.robException.valid)) {
        assert(sourceModeCase.isEmpty, "A machine-source Store reached the ROB exception path")
        val fault = dut.io.robException.bits
        if (trapCase.nonEmpty) {
          val trial = trapCase.get
          assert(exceptions(fault.exceptionVec) == trial.source.faults)
          if (trial.virtual && enabled) dut.memory.trapInstructionValid.expect(false.B)
          if (trial.source.outcome == 1) {
            fault.fdiException.get.tval.expect(trial.source.address.U)
            fault.fdiException.get.reason.expect(3.U)
          }
        } else {
          assert(enabled && exceptions(fault.exceptionVec) == Set(24), "Unexpected ROB exception in the store minimum")
          fault.fdiException.get.tval.expect((baseAddress + 64).U)
          fault.fdiException.get.reason.expect(3.U)
        }
        fault.pc.expect(deniedPc.U)
        fault.isInterrupt.expect(false.B)
        robFaults += 1
        event("rob-store-exception", s"\"pc\":$deniedPc,\"vector\":[${exceptions(fault.exceptionVec).toSeq.sorted.mkString(",")}]")
      }
      dut.memory.rename.foreach { entry =>
        if (bool(entry.valid)) {
          sourceLocations.get((pointer(entry.bits.ftqPtr), uint(entry.bits.ftqOffset))).foreach { source =>
            // A scalar store's address and data micro-operations share one ROB identity.
            assert((uint(entry.bits.instr) & 127) == 0x23, "A store location renamed an unrelated instruction")
            val rob = pointer(entry.bits.robIdx)
            if (!recovery && trapCase.isEmpty && sourceModeCase.isEmpty) {
              sourceRobs.get(rob).foreach(previous => assert(previous == source, "ROB identity aliases two live stores"))
              sourceRobs(rob) = source
            }
            event("store-rename", s"\"pc\":${source.pc},\"rob_flag\":${rob.flag},\"rob\":${rob.value}")
          }
        }
      }
      if (recovery || trapCase.nonEmpty || sourceModeCase.nonEmpty) dut.io.robEnq.foreach { port =>
        if (bool(port.valid)) {
          val location = (pointer(port.bits.ftqIdx), uint(port.bits.ftqOffset))
          assert(!enabled || !oldYoungLocation.contains(location), "Old younger Store allocated ROB past the CSR writer")
          sourceLocations.get(location).foreach { source =>
            val rob = pointer(port.bits.robIdx)
            sourceRobs.get(rob).foreach(previous => assert(previous == source, "ROB identity aliases two live stores"))
            sourceRobs(rob) = source
            event("store-rob-allocation", s"\"pc\":${source.pc},\"rob_flag\":${rob.flag},\"rob\":${rob.value}," +
              s"\"ftq_flag\":${location._1.flag},\"ftq\":${location._1.value},\"offset\":${location._2}")
          }
        }
      }
      dut.io.robRetire.foreach { port =>
        if (bool(port.valid)) sourceRobs.get(pointer(port.bits)).foreach { source =>
          assert(!source.blocked, "Faulting store was reported as architecturally retired")
          assert(completed.contains(source.pc), "Store retirement preceded its actual completion")
          assert(retired.add(source.pc), "A store source retired twice")
          if (recovery && source.pc == 0x4ffc) oldRetireCycle = cycle
          event("store-retire", s"\"pc\":${source.pc}")
        }
      }
      for ((issue, lane) <- dut.memory.issue.zipWithIndex) {
        if (bool(issue.valid)) {
          val (key, source) = associate(issue.bits.uop)
          if (recovery && enabled && source.pc == 0x5008) assert(recoveryReplay, "Old younger Store reached STA")
          val location = (pointer(issue.bits.uop.ftqPtr), uint(issue.bits.uop.ftqOffset))
          assert(sourceLocations.get(location).contains(source), "STA FTQ identity disagrees with the independently supplied instruction")
          assert(issued.add(source.pc), "Unexpected STA retry in the cache-hit minimum")
          addressOwners(source.pc) = key
          dataOwners.get(source.pc).foreach(previous => assert(previous == key, "STA and STD disagree on their SQ allocation"))
          issue.bits.uop.fuOpType.expect(3.U)
          issue.bits.src(0).expect(baseAddress.U)
          val immediate = uint(issue.bits.uop.imm) & 4095
          assert(((uint(issue.bits.src(0)) + immediate) & wordMask) == source.address)
          issue.bits.uop.fdiNotTrusted.foreach(_.expect(source.notTrusted.B))
          sourceModeCase.foreach { trial =>
            assert(sourceModeActive && sourceModeStatusWrites == 1)
            checkSourceMode()
            event("store-source-data-context", identity(key, source) +
              s",\"source_privilege\":3,\"source_virtual\":false,\"data_privilege\":0,\"data_virtual\":${trial.dataVirtual}," +
              s"\"mstatus\":${uint(dut.memory.machineStatus)}")
          }
          if (source.denied) deniedTrapOwner = Some(key)
          event("store-address-issue", identity(key, source) + s",\"lane\":$lane,\"address\":${source.address}")
        }
      }
      for ((issue, lane) <- dut.memory.dataIssue.zipWithIndex if bool(issue.valid)) {
        val (key, source) = associate(issue.bits.uop)
        if (recovery && enabled && source.pc == 0x5008) assert(recoveryReplay, "Old younger Store reached STD")
        assert(dataIssued.add(source.pc), "Unexpected STD retry in the minimum")
        dataOwners(source.pc) = key
        addressOwners.get(source.pc).foreach(previous => assert(previous == key, "STD and STA disagree on their SQ allocation"))
        issue.bits.src(0).expect(source.data.U)
        event("store-data-issue", identity(key, source) + s",\"lane\":$lane,\"data\":${source.data}")
      }
      for ((port, lane) <- dut.memory.metadata.zipWithIndex) {
        if (bool(port.req.valid) && bool(port.req.ready)) {
          assert(bool(dut.memory.issue(lane).valid), "Cache metadata must belong to this lane's actual accepted STA")
          val (_, source) = known(dut.memory.issue(lane).bits.uop)
          port.req.bits.cmd.expect(MemoryOpConstants.M_PFW)
          port.req.bits.vaddr.expect(source.address.U)
          metadataReplies(lane).enqueue(cycle + 2)
          event("metadata-request", s"\"pc\":${source.pc},\"lane\":$lane")
        }
        val phase = dut.memory.phases(lane)
        if (bool(phase.permissionRequest.valid)) {
          val (key, source) = known(phase.primary.bits.uop)
          phase.primary.valid.expect(true.B)
          val request = phase.permissionRequest.bits
          request.address.expect(source.address.U)
          request.sourcePrivilege.expect(source.privilege.U)
          request.sourceVirtual.expect(source.virtual.B)
          request.notTrusted.expect(source.notTrusted.B)
          source.expectedBoundHi.foreach(value => dut.memory.mirror(3).expect(value.U))
          request.sizeLog2.expect(3.U)
          request.operation.expect(1.U)
          assert(permissionRequests.add(source.pc), "Duplicate store permission request")
          assert(!pendingPermissions.contains(uint(request.tag)))
          pendingPermissions(uint(request.tag)) = (source, cycle)
          event("permission-request", identity(key, source) + s",\"tag\":${uint(request.tag)}")
        }
        if (bool(phase.permissionResponse.valid)) {
          val response = phase.permissionResponse.bits
          val (source, accepted) = pendingPermissions.remove(uint(response.request.tag)).getOrElse(
            throw new AssertionError("Permission response has no accepted transaction"))
          assert(cycle == accepted + 1)
          response.request.address.expect(source.address.U)
          response.outcome.expect(source.outcome.U)
          response.reason.expect((if (source.denied) 3 else 0).U)
          phase.permissionConsumed.expect(true.B)
          assert(permissionResponses.add(source.pc))
          event("permission-response", s"\"pc\":${source.pc},\"outcome\":${source.outcome},\"denied\":${source.denied}")
        }
        if (bool(phase.s2)) {
          val (key, source) = known(phase.supplement.uop)
          phase.pmpFault.expect(source.pmpFault.B)
          phase.supplement.fullva.expect(source.address.U)
          phase.supplement.hasException.expect(source.blocked.B)
          event("store-final-stage", identity(key, source))
        }
        val completion = dut.memory.completion(lane)
        if (bool(completion.valid)) {
          val (key, source) = known(completion.bits.uop)
          assert(completed.add(source.pc), "Duplicate store address completion")
          val actual = exceptions(completion.bits.uop.exceptionVec)
          assert(actual == source.faults, s"Unexpected store exception $actual")
          if (source.denied) {
            completion.bits.uop.fdiException.get.tval.expect(source.address.U)
            completion.bits.uop.fdiException.get.reason.expect(3.U)
          }
          event("store-completion", identity(key, source) + s",\"denied\":${source.denied},\"blocked\":${source.blocked}," +
            s"\"exceptions\":[${source.faults.toSeq.sorted.mkString(",")}]")
        }
      }
      for ((slot, index) <- dut.memory.slots.zipWithIndex if bool(slot.allocated) && !bool(slot.waitS2)) {
        val key = Owner(pointer(slot.rob), pointer(slot.sq))
        sourceOwners.get(key).foreach { source =>
          if (finalSlots.add(source.pc)) {
            assert(key.sq.value == index, "SQ payload moved to a different slot")
            slot.hasException.expect(source.blocked.B)
            slot.mmio.expect(false.B); slot.nc.expect(false.B); slot.pending.expect(false.B)
            if (enabled || trapCase.isEmpty) assert(exceptions(slot.exceptions) == source.faults)
            if (source.denied) {
              slot.fdiException.get.tval.expect(source.address.U)
              slot.fdiException.get.reason.expect(3.U)
            }
            event("sq-final-payload", identity(key, source))
          }
        }
      }
      for ((port, lane) <- dut.memory.publication.zipWithIndex) {
        if (bool(port.fire)) {
          val address = uint(port.bits.addr)
          val source = if (recovery) {
            val sq = pointer(dut.memory.publicationSq(lane))
            val matches = sourceOwners.iterator.filter(_._1.sq == sq).map(_._2).toSeq
            assert(matches.size == 1, "Published SQ owner is ambiguous or unknown")
            val knownSource = matches.head
            assert(knownSource.address == address)
            knownSource
          } else sources.find(_.address == address).getOrElse(throw new AssertionError(s"Unexpected SQ publication address $address"))
          if (bool(port.bits.vecValid)) {
            assert(!source.blocked, "Faulting store published real bytes")
            assert(published.add(source.pc), "A scalar store published twice")
            val wordBase = address & ~BigInt(15)
            val expectedMask = BigInt(255) << (source.address - wordBase).toInt
            port.bits.mask.expect(expectedMask.U)
            for (byte <- 0 until 8) {
              val expected = ((source.data >> (8 * byte)) & 255).toInt
              val offset = (source.address - wordBase).toInt + byte
              assert(((uint(port.bits.data) >> (8 * offset)) & 255).toInt == expected)
              expectedBytes(source.address + byte) = expected
            }
          } else {
            assert(source.blocked, "An allowed scalar store became an empty publication token")
            emptyTokens += 1
          }
          event("sbuffer-publication", s"\"pc\":${source.pc},\"lane\":$lane,\"vec_valid\":${bool(port.bits.vecValid)},\"mask\":${uint(port.bits.mask)}")
        }
        val write = dut.memory.writes(lane)
        assert(bool(write.valid) == (bool(port.fire) && bool(port.bits.vecValid)))
        if (bool(write.valid)) {
          write.bits.mask.expect(uint(port.bits.mask).U)
          write.bits.data.expect(uint(port.bits.data).U)
          assert(uint(write.bits.wvec).bitCount == 1)
          byteWrites += 1
          event("sbuffer-data-write", s"\"lane\":$lane,\"mask\":${uint(write.bits.mask)}")
        }
      }
      // The count is retained between redirects; its production event qualifies it.
      val cancel = if (bool(dut.memory.queueCancelEvent)) uint(dut.memory.queueCancel).toInt else 0
      val dequeue = uint(dut.memory.queueDequeue).toInt
      cancellations += cancel; dequeues += dequeue
      if (cancel != 0 || dequeue != 0) event("sq-release", s"\"cancel\":$cancel,\"dequeue\":$dequeue")
      assert(!bool(dut.memory.uncache.req.valid), "The bounded DRAM scenario must not create an uncache request")
      dut.memory.ptw.req.foreach(port => assert(!bool(port.valid), "Bare addresses must not request a page-table walk"))
      val request = dut.memory.cacheWrite.req
      if (recovery) {
        if (bool(request.valid)) {
          val address = uint(request.bits.addr)
          val id = uint(request.bits.id)
          request.bits.cmd.expect(MemoryOpConstants.M_XWR)
          require(request.bits.mask.getWidth == 64)
          assert(address % 64 == 0)
          if (heldCacheEffect.isEmpty) {
            val priorEffects = expectedBytes.filter { case (byte, _) => byte >= address && byte < address + 64 }.toMap
            assert(priorEffects.nonEmpty, "Cache line has no original authorized Store effect")
            heldCacheEffect = Some((address, id, priorEffects))
          }
          val (heldAddress, heldId, effect) = heldCacheEffect.get
          assert(address == heldAddress && id == heldId, "Cache backpressure changed request identity")
          val mask = effect.keys.foldLeft(BigInt(0))((bits, byte) => bits.setBit((byte - address).toInt))
          request.bits.mask.expect(mask.U)
          for ((byte, expected) <- effect)
            assert(((uint(request.bits.data) >> (8 * (byte - address).toInt)) & 255).toInt == expected)
          event("authorized-cache-offer", s"\"address\":$address,\"mask\":$mask,\"ready\":${bool(request.ready)}")
          if (bool(request.ready)) {
            effect.foreach { case (byte, value) => actualBytes(byte) = value }
            cacheWrites += 1; sentLines += address
            cacheReplies.enqueue((cycle + 3, id)); heldCacheEffect = None
            event("cache-line-write", s"\"address\":$address,\"mask\":$mask,\"id\":$id")
          }
        } else assert(heldCacheEffect.isEmpty, "An unaccepted authorized cache request disappeared")
      } else {
      if (bool(request.valid)) {
        assert(!trapCase.exists(_.source.blocked), "A faulting-only Store case exposed a cache write request")
        // Backpressure cannot excuse a request exposing a faulting-only cache line.
        val deniedLines = sources.filter(_.blocked).map(_.address & ~BigInt(63)).toSet --
          sources.filterNot(_.blocked).map(_.address & ~BigInt(63)).toSet
        require(request.bits.mask.getWidth == 64, "The fixture uses 64-byte cache lines")
        assert(!deniedLines.contains(uint(request.bits.addr)),
          "Denied store exposed a cache request before acceptance")
        event("cache-line-request", s"\"address\":${uint(request.bits.addr)},\"ready\":${bool(request.ready)}")
      }
      if (bool(request.valid) && bool(request.ready)) {
        request.bits.cmd.expect(MemoryOpConstants.M_XWR)
        val address = uint(request.bits.addr)
        val lineBytes = request.bits.mask.getWidth
        assert(address % lineBytes == 0 && sentLines.add(address), "Unexpected duplicate or unaligned cache publication")
        val expected = expectedBytes.filter { case (byte, _) => byte >= address && byte < address + lineBytes }
        assert(expected.nonEmpty, "Cache publication contains no independently allowed bytes")
        val mask = expected.keys.foldLeft(BigInt(0))((result, byte) => result.setBit((byte - address).toInt))
        request.bits.mask.expect(mask.U)
        for ((byte, value) <- expected) {
          val actual = ((uint(request.bits.data) >> (8 * (byte - address).toInt)) & 255).toInt
          assert(actual == value, s"Cache byte mismatch at $byte")
          actualBytes(byte) = actual
        }
        cacheReplies.enqueue((cycle + 3, uint(request.bits.id)))
        event("cache-line-write", s"\"address\":$address,\"mask\":$mask,\"id\":${uint(request.bits.id)}")
      }
      }
      dut.clock.step()
      cycle += 1
    }

    private def until(label: String, limit: Int = 2000)(condition: => Boolean): Unit = {
      val stop = cycle + limit
      while (!condition && cycle < stop) edge()
      assert(condition, s"Timed out: $label at cycle $cycle")
    }
    private def idle(count: Int): Unit = (0 until count).foreach(_ => edge())
    private def prediction(stage: BranchPredictionBundle, pc: BigInt, ptr: Pointer): Unit = {
      zero(stage); stage.pc.foreach(_.poke(pc.U)); stage.valid.foreach(_.poke(true.B)); setPointer(stage.ftq_idx, ptr)
    }
    private def packet(pc: BigInt, instructions: Seq[BigInt], tag: Boolean = false): Pointer = {
      val lanes = Seq(dut.io.instruction) ++ dut.io.instructionTail.toSeq
      require(instructions.nonEmpty && instructions.size <= lanes.size)
      assert(!bool(dut.io.backendRedirect.valid) && !bool(dut.io.frontendRedirect.valid),
        "Consume the matching FTQ redirect before planning its recovered BPU packet")
      val ptr = pointer(dut.io.ftqNext)
      if (recovery && pc == 0x5000 && enabled) oldYoungLocation = Some((ptr, BigInt(4)))
      fetches -= ptr
      frontendRedirects.keys.filter(_._1 == ptr).toList.foreach(frontendRedirects.remove)
      sourceLocations.keys.filter(_._1 == ptr).toList.foreach(sourceLocations.remove)
      instructions.indices.foreach { index => sourceByPc.get(pc + 4 * index).foreach { source =>
        sourceLocations((ptr, BigInt(2 * index))) = source
      }}
      zero(dut.io.bpu.resp.bits)
      prediction(dut.io.bpu.resp.bits.s1, pc, ptr)
      dut.io.bpu.resp.valid.poke(true.B)
      until("BPU ready")(bool(dut.io.bpu.resp.ready)); edge()
      dut.io.bpu.resp.valid.poke(false.B)
      zero(dut.io.bpu.resp.bits.s1); prediction(dut.io.bpu.resp.bits.s2, pc, ptr); edge()
      zero(dut.io.bpu.resp.bits.s2); prediction(dut.io.bpu.resp.bits.s3, pc, ptr); edge()
      zero(dut.io.bpu.resp.bits.s3)
      until("actual FTQ fetch")(fetches.contains(ptr))
      assert(fetches(ptr) == pc)
      zero(dut.io.predecode.bits); setPointer(dut.io.predecode.bits.ftqIdx, ptr)
      dut.io.predecode.bits.pc.zipWithIndex.foreach { case (value, index) => value.poke((pc + 2 * index).U) }
      instructions.indices.foreach { index =>
        dut.io.predecode.bits.pd(2 * index).valid.poke(true.B)
        dut.io.predecode.bits.instrRange(2 * index).poke(true.B)
      }
      dut.io.predecode.bits.ftqOffset.poke((2 * (instructions.size - 1)).U)
      dut.io.predecode.bits.target.poke((pc + 4 * instructions.size).U)
      dut.io.predecode.valid.poke(true.B); edge(); dut.io.predecode.valid.poke(false.B)
      var consumed = 0
      val stop = cycle + 2000
      while (consumed < instructions.size && cycle < stop) {
        lanes.zipWithIndex.foreach { case (lane, slot) =>
          val index = consumed + slot
          zero(lane.bits); lane.valid.poke((index < instructions.size).B)
          if (index < instructions.size) {
            lane.bits.instr.poke(instructions(index).U); lane.bits.pc.poke((pc + 4 * index).U)
            lane.bits.pd.valid.poke(true.B); lane.bits.trigger.poke(TriggerAction.None)
            lane.bits.fdiNotTrusted.foreach(_.poke(sourceByPc.get(pc + 4 * index).map(_.notTrusted).getOrElse(tag).B)); setPointer(lane.bits.ftqPtr, ptr)
            lane.bits.ftqOffset.poke((2 * index).U)
            lane.bits.isLastInFtqEntry.poke((index == instructions.size - 1).B)
          }
        }
        val count = (0 until instructions.size - consumed).takeWhile(index => bool(lanes(index).ready)).size
        edge(); consumed += count
      }
      assert(consumed == instructions.size, "The complete supplied packet must reach the real decode input")
      lanes.foreach(_.valid.poke(false.B))
      ptr
    }
    private def csr(pc: BigInt, instruction: BigInt, flush: Boolean = false): Unit = {
      val before = csrResponses
      val ptr = packet(pc, Seq(instruction))
      until("actual CSR response")(csrResponses > before)
      if (flush) until("configuration FTQ recovery") {
        frontendRedirects.get((ptr, BigInt(0))).contains(pc + 4)
      } else idle(16)
    }
    private def reset(): Unit = {
      zeroInputs(dut.io); zeroInputs(dut.memory)
      dut.io.fetchReady.poke(true.B)
      dut.memory.metadata.foreach(_.req.ready.poke(true.B))
      dut.memory.ptw.req.foreach(_.ready.poke(true.B))
      dut.memory.uncache.req.ready.poke(true.B)
      dut.memory.cacheWrite.req.ready.poke(false.B)
      dut.reset.poke(true.B); dut.clock.step(10)
      dut.reset.poke(false.B); dut.clock.step(400); cycle = 410
      dut.io.mode.expect(3.U)
    }

    def runRecovery(): Unit = {
      require(recovery)
      reset()
      packet(0x1000, Seq(addi(1, 0, 1), slli(1, 1, 31), addi(2, 1, 72), addi(3, 0, 9)))
      idle(40)
      packet(0x1040, Seq(addi(5, 0, 1), slli(5, 5, 30), addi(6, 0, 15), lui(4, 5)))
      idle(40)
      packet(0x1080, Seq(addi(4, 4, -4), lui(7, 6), addi(9, 0, 0x5a5), addi(10, 0, 0x6b6)))
      idle(40)
      packet(0x10c0, Seq(addi(12, 1, 8), lui(13, 5), addi(14, 13, 8)))
      idle(40)
      csr(0x1100, csrWrite(0x3b0, 5))
      csr(0x1140, csrWrite(0x3a0, 6))
      if (enabled) {
        csr(0x1200, csrWrite(0x890, 1), flush = true)
        csr(0x1240, csrWrite(0x891, 2), flush = true)
        csr(0x1280, csrWrite(0x880, 3), flush = true)
        csr(0x12c0, csrWrite(0x9e2, 13), flush = true)
        csr(0x1300, csrWrite(0x9e3, 14), flush = true)
        csr(0x1340, csrImmediate(0xbc4, 2), flush = true)
        csr(0x1380, csrImmediate(0x8b3, 4), flush = true)
        dut.memory.mirror(3).expect((baseAddress + 72).U)
        dut.memory.reason.expect(4.U)
      }
      csr(0x1400, csrWrite(0x305, 7))
      csr(0x1440, csrWrite(0x341, 4))
      csr(0x1480, csrWrite(0x300, 0))
      assert(traps == 0)
      val mretPtr = packet(0x1500, Seq(BigInt(0x30200073)))
      until("HU entry for store reconfiguration") {
        uint(dut.io.mode) == 0 && frontendRedirects.get((mretPtr, BigInt(0))).contains(BigInt(0x4ffc))
      }
      packet(0x4ffc, Seq(store(9, 64)), tag = true)
      until("real old-store retirement and Sbuffer ownership") {
        retired.contains(BigInt(0x4ffc)) && published.contains(BigInt(0x4ffc)) && bool(dut.memory.queueEmpty)
      }
      idle(4)
      dut.memory.bufferEmpty.expect(false.B)
      assert(oldRetireCycle >= 0 && cacheWrites == 0 && actualBytes.isEmpty)
      assert(completed == Set(BigInt(0x4ffc)) && finalSlots == Set(BigInt(0x4ffc)))
      assert(pendingPermissions.isEmpty && metadataReplies.forall(_.isEmpty))
      dut.memory.slots.foreach(_.allocated.expect(false.B))
      // The retired Sbuffer effect survives; only drained SQ/ROB identity associations end here.
      sourceRobs.clear(); sourceOwners.clear()
      sourceLocations.keys.filter(key => sourceLocations(key).pc == 0x4ffc).toList.foreach(sourceLocations.remove)
      dut.memory.forceFlush.poke(true.B)
      until("real old cache request held under backpressure") { heldCacheEffect.nonEmpty }
      event("old-retired-effect-pending", s"\"pc\":20476,\"address\":${baseAddress + 64},\"data\":1445")
      val beforeCsr = csrResponses
      val instruction = if (enabled) csrWrite(0x891, 12) else addi(0, 0, 0)
      val writerPtr = packet(0x5000, Seq(addi(0, 0, 0), instruction, store(10, 64)))
      if (enabled) {
        until("real trusted CSR response") { csrResponses > beforeCsr }
        until("bound-write recovery clears the old younger Store") {
          oldYoungCanceled && oldYoungCleared &&
            frontendRedirects.get((writerPtr, BigInt(2))).contains(BigInt(0x5008)) &&
            uint(dut.memory.mirror(3)) == baseAddress + 8
        }
        assert(oldYoungHeld && tighteningWriteCycle > oldRetireCycle)
        dut.memory.reason.expect(4.U)
        assert(!issued.contains(BigInt(0x5008)) && !dataIssued.contains(BigInt(0x5008)))
        assert(!completed.contains(BigInt(0x5008)) && !retired.contains(BigInt(0x5008)))
        assert(!published.contains(BigInt(0x5008)) && cacheWrites == 0)
        assert(sourceRobs.isEmpty && sourceOwners.isEmpty)
        dut.memory.slots.foreach(_.allocated.expect(false.B))
        oldYoungLocation.foreach(sourceLocations.remove)
        recoveryReplay = true
        val replayPtr = packet(0x5008, Seq(store(10, 64)), tag = true)
        until("recovered store's precise denial") {
          traps == 1 && frontendRedirects.get((replayPtr, BigInt(0))).contains(BigInt(0x6000))
        }
        dut.io.mode.expect(3.U); dut.io.mcause.expect(24.U)
        dut.io.mepc.expect(0x5008.U); dut.io.mtval.expect((baseAddress + 64).U)
        dut.memory.reason.expect(3.U)
        assert(!retired.contains(BigInt(0x5008)) && !published.contains(BigInt(0x5008)))
        event("recovered-store-denied", s"\"pc\":20488,\"cause\":24,\"tval\":${baseAddress + 64},\"reason\":3")
      } else {
        until("ordinary disabled-state control and young Store progress") {
          retired.contains(BigInt(0x5008)) && published.contains(BigInt(0x5008))
        }
        assert(traps == 0 && robFaults == 0)
      }
      until("reconfiguration SQ drain") { bool(dut.memory.queueEmpty) && pendingPermissions.isEmpty }
      idle(4)
      assert(issued.toSet == sourceByPc.keySet && dataIssued.toSet == sourceByPc.keySet)
      assert(completed.toSet == sourceByPc.keySet && finalSlots.toSet == sourceByPc.keySet)
      assert(addressOwners.toMap == dataOwners.toMap)
      assert(published.toSet == sources.filterNot(_.denied).map(_.pc).toSet)
      assert(retired.toSet == sources.filterNot(_.denied).map(_.pc).toSet)
      assert(byteWrites == published.size)
      assert(permissionRequests.toSet == (if (enabled) sourceByPc.keySet else Set.empty[BigInt]))
      assert(permissionResponses == permissionRequests)
      assert(cacheWrites == 0 && heldCacheEffect.nonEmpty)
      dut.memory.cacheWrite.req.ready.poke(true.B)
      until("old authorized cache effect drains after reconfiguration", 3000) {
        bool(dut.memory.flushDone) && bool(dut.memory.bufferEmpty) && cacheReplies.isEmpty && heldCacheEffect.isEmpty
      }
      dut.memory.forceFlush.poke(false.B); idle(4)
      val independentBytes = sources.filterNot(_.denied).flatMap { source =>
        (0 until 8).map(byte => (source.address + byte) -> ((source.data >> (8 * byte)) & 255).toInt)
      }.toMap
      assert(expectedBytes.toMap == independentBytes && actualBytes.toMap == independentBytes)
      assert(sentLines.toSet == Set(baseAddress + 64) && cacheWrites >= 1)
      for (address <- baseAddress + 48 until baseAddress + 96)
        assert(actualBytes.getOrElse(address, 0x5a) == independentBytes.getOrElse(address, 0x5a))
      assert(metadataReplies.forall(_.isEmpty))
      event("reconfiguration-summary", s"\"enabled\":$enabled,\"sources\":2,\"traps\":$traps,\"cache_writes\":$cacheWrites," +
        s"\"old_retired_preserved\":true,\"old_young_cancel_observed\":$oldYoungCanceled,\"new_mirror_checked\":$enabled," +
        "\"critical_debug_validated\":false,\"hs_guest_standard_traps_validated\":false")
      println(s"Store permission Backend reconfiguration PASS enabled=$enabled cycles=$cycle")
    }

    def runTrap(): Unit = {
      val trial = trapCase.get
      val source = trial.source
      reset()
      event("architectural-store-case", s"\"source_privilege\":${trial.privilege},\"source_virtual\":${trial.virtual}," +
        s"\"expected_vector\":[${source.faults.toSeq.sorted.mkString(",")}],\"expected_cause\":${trial.finalCause}")
      packet(0x1000, Seq(addi(1, 0, 1), slli(1, 1, 31), addi(2, 1, 8), addi(3, 0, 9)))
      idle(40)
      packet(0x1040, Seq(addi(5, 0, 1), slli(5, 5, 30),
        addi(6, 0, if (trial.pmpFault) 13 else 15), lui(4, 4)))
      idle(40)
      val modeValue = if (trial.virtual || trial.privilege == 1) 1 else 0
      val modeShift = if (trial.virtual) 39 else if (trial.privilege == 1) 11 else 0
      packet(0x1080, Seq(lui(7, 6), addi(10, 0, 0x6b6), addi(12, 0, modeValue), slli(12, 12, modeShift)))
      idle(40)
      if (trial.delegated && enabled) {
        packet(0x10c0, Seq(addi(13, 0, 1), slli(13, 13, 25)))
        idle(40)
      }
      csr(0x1100, csrWrite(0x3b0, 5))
      csr(0x1140, csrWrite(0x3a0, 6))
      if (enabled) {
        csr(0x1200, csrWrite(0x890, 1), flush = true)
        csr(0x1240, csrWrite(0x891, 2), flush = true)
        csr(0x1280, csrWrite(0x880, 3), flush = true)
        csr(0x12c0, csrImmediate(0xbc4, if (trial.privilege == 1) 1 else 2), flush = true)
        csr(0x1300, csrImmediate(0x8b3, 4), flush = true)
        dut.memory.mirror(3).expect((baseAddress + 8).U)
      }
      csr(0x1400, csrWrite(0x305, 7))
      csr(0x1440, csrWrite(0x105, 7))
      if (trial.delegated && enabled) csr(0x1480, csrWrite(0x302, 13))
      csr(0x14c0, csrWrite(0x341, 4))
      csr(0x1500, csrWrite(0x300, 12))
      assert(traps == 0 && supervisorTraps == 0 && robFaults == 0 && reasonEffects == 0)
      dut.memory.trapInstructionValid.expect(false.B)
      val mretPtr = packet(0x1540, Seq(BigInt(0x30200073)))
      until("actual Store source mode and FTQ recovery") {
        uint(dut.io.mode) == trial.privilege && bool(dut.io.virtualMode) == trial.virtual &&
          frontendRedirects.get((mretPtr, BigInt(0))).contains(source.pc)
      }
      dut.memory.reason.expect((if (enabled) 4 else 0).U)
      val storePtr = packet(source.pc, Seq(store(10, 64)), tag = true)
      until("real Store address completion") { completed(source.pc) }
      if (source.blocked) {
        until("precise final Store trap and source-specific FTQ recovery") {
          traps + supervisorTraps == 1 &&
            frontendRedirects.get((storePtr, BigInt(0))).contains(BigInt(0x6000))
        }
        if (trial.supervisorEntry) {
          dut.io.mode.expect(1.U); dut.io.virtualMode.expect(false.B)
          dut.io.scause.expect(trial.finalCause.U); dut.io.sepc.expect(source.pc.U)
          dut.memory.supervisorTval.expect(trial.finalTval.U)
        } else {
          dut.io.mode.expect(3.U); dut.io.virtualMode.expect(false.B)
          dut.io.mcause.expect(trial.finalCause.U); dut.io.mepc.expect(source.pc.U)
          dut.io.mtval.expect(trial.finalTval.U)
        }
        assert(!retired(source.pc) && !published(source.pc))
        event("precise-store-trap-state", s"\"cause\":${trial.finalCause},\"epc\":${source.pc}," +
          s"\"tval\":${trial.finalTval},\"supervisor\":${trial.supervisorEntry}")
      } else until("ordinary off Store retirement and byte publication") {
        retired(source.pc) && published(source.pc)
      }
      until("actual Store queue cleanup") { bool(dut.memory.queueEmpty) && pendingPermissions.isEmpty }
      idle(8)
      dut.memory.slots.foreach(_.allocated.expect(false.B))
      assert(issued.toSet == Set(source.pc) && dataIssued.toSet == Set(source.pc))
      assert(completed.toSet == Set(source.pc) && finalSlots.toSet == Set(source.pc))
      assert(addressOwners.toMap == dataOwners.toMap)
      assert(retired.toSet == (if (source.blocked) Set.empty[BigInt] else Set(source.pc)))
      assert(published.toSet == retired.toSet && byteWrites == published.size)
      assert(permissionRequests.toSet == (if (enabled) Set(source.pc) else Set.empty[BigInt]))
      assert(permissionResponses == permissionRequests)
      assert(traps + supervisorTraps == (if (source.blocked) 1 else 0))
      assert(supervisorTraps == (if (trial.supervisorEntry) 1 else 0))
      assert(robFaults == (if (source.blocked) 1 else 0) && reasonEffects == trial.reasonEffects)
      dut.memory.reason.expect((if (enabled) trial.finalReason else 0).U)
      dut.memory.forceFlush.poke(true.B); dut.memory.cacheWrite.req.ready.poke(true.B)
      until("actual case-end Sbuffer and cache ACK drain") {
        bool(dut.memory.flushDone) && bool(dut.memory.bufferEmpty) && cacheReplies.isEmpty
      }
      dut.memory.forceFlush.poke(false.B); idle(4)
      val independentBytes = if (source.blocked) Map.empty[BigInt, Int] else
        (0 until 8).map(byte => (source.address + byte) -> ((source.data >> (8 * byte)) & 255).toInt).toMap
      assert(expectedBytes.toMap == independentBytes && actualBytes.toMap == independentBytes)
      for (address <- baseAddress + 48 until baseAddress + 96)
        assert(actualBytes.getOrElse(address, 0x5a) == independentBytes.getOrElse(address, 0x5a))
      assert(sentLines.toSet == (if (source.blocked) Set.empty[BigInt] else Set(baseAddress + 64)))
      assert(metadataReplies.forall(_.isEmpty) && reasonEffects == trial.reasonEffects)
      event("architectural-store-summary", s"\"enabled\":$enabled,\"trap_count\":${traps + supervisorTraps}," +
        s"\"supervisor_traps\":$supervisorTraps,\"reason_effects\":$reasonEffects,\"byte_writes\":$byteWrites," +
        s"\"sq_cancel\":$cancellations,\"sq_dequeue\":$dequeues,\"critical_debug_validated\":false")
      println(s"Store permission Backend ${trial.name} PASS enabled=$enabled cycles=$cycle")
    }

    def runSourceMode(): Unit = {
      val trial = sourceModeCase.get
      val source = trial.source
      reset()
      packet(0x1000, Seq(addi(1, 0, 1), slli(1, 1, 31), addi(2, 1, 8), addi(3, 0, 9)))
      idle(40)
      packet(0x1040, Seq(addi(5, 0, 1), slli(5, 5, 30), addi(6, 0, 15), addi(10, 0, 0x6b6)))
      idle(40)
      // Build MPV at bit 39 and MPRV at bit 17 through ordinary integer instructions.
      packet(0x1080, Seq(addi(12, 0, if (trial.dataVirtual) 1 else 0), slli(12, 12, 22),
        addi(12, 12, 1), slli(12, 12, 17)))
      idle(40)
      csr(0x1100, csrWrite(0x3b0, 5))
      csr(0x1140, csrWrite(0x3a0, 6))
      if (enabled) {
        csr(0x1200, csrWrite(0x890, 1), flush = true)
        csr(0x1240, csrWrite(0x891, 2), flush = true)
        csr(0x1280, csrWrite(0x880, 3), flush = true)
        csr(0x12c0, csrImmediate(0xbc4, 3), flush = true)
        csr(0x1300, csrImmediate(0x8b3, 4), flush = true)
        dut.memory.mirror(0).expect(3.U); dut.memory.mirror(1).expect(9.U)
        dut.memory.mirror(2).expect(baseAddress.U); dut.memory.mirror(3).expect((baseAddress + 8).U)
      }
      csr(0x1400, csrWrite(0x300, 12))
      assert(sourceModeStatusWrites == 1)
      checkSourceMode()
      sourceModeActive = true
      event("machine-source-context-ready", s"\"source_privilege\":3,\"source_virtual\":false," +
        s"\"data_privilege\":0,\"data_virtual\":${trial.dataVirtual},\"mstatus\":${uint(dut.memory.machineStatus)}")
      // SATP/VSATP/HGATP remain at natural Bare; this checks context selection, not page walks.
      packet(source.pc, Seq(store(10, 64)), tag = true)
      until("machine-source Store retirement, publication and SQ cleanup") {
        retired(source.pc) && published(source.pc) && bool(dut.memory.queueEmpty) && pendingPermissions.isEmpty
      }
      idle(8)
      dut.memory.slots.foreach(_.allocated.expect(false.B))
      val onlySource = Set(source.pc)
      assert(sourceRobs.size == 1 && sourceOwners.size == 1)
      assert(issued.toSet == onlySource && dataIssued.toSet == onlySource)
      assert(addressOwners.toMap == dataOwners.toMap)
      assert(completed.toSet == onlySource && finalSlots.toSet == onlySource)
      assert(retired.toSet == onlySource && published.toSet == onlySource && byteWrites == 1)
      assert(permissionRequests.toSet == (if (enabled) onlySource else Set.empty[BigInt]))
      assert(permissionResponses == permissionRequests)
      assert(traps == 0 && supervisorTraps == 0 && robFaults == 0 && reasonEffects == 0)
      assert(cancellations == 0 && dequeues == 1 && emptyTokens == 0)
      dut.memory.reason.expect((if (enabled) 4 else 0).U)
      dut.memory.forceFlush.poke(true.B); dut.memory.cacheWrite.req.ready.poke(true.B)
      until("machine-source Store Sbuffer flush and cache ACK") {
        bool(dut.memory.flushDone) && bool(dut.memory.bufferEmpty) && cacheReplies.isEmpty
      }
      dut.memory.forceFlush.poke(false.B); idle(4)
      val independentBytes = (0 until 8).map { byte =>
        (source.address + byte) -> ((source.data >> (8 * byte)) & 255).toInt
      }.toMap
      assert(expectedBytes.toMap == independentBytes && actualBytes.toMap == independentBytes)
      for (address <- baseAddress + 48 until baseAddress + 96)
        assert(actualBytes.getOrElse(address, 0x5a) == independentBytes.getOrElse(address, 0x5a))
      assert(sentLines.toSet == Set(baseAddress + 64) && metadataReplies.forall(_.isEmpty))
      checkSourceMode()
      event("architectural-source-mode-summary", s"\"enabled\":$enabled,\"source_privilege\":3," +
        s"\"source_virtual\":false,\"data_privilege\":0,\"data_virtual\":${trial.dataVirtual}," +
        s"\"mstatus_writes\":$sourceModeStatusWrites,\"stores\":1,\"traps\":0,\"reason_effects\":0," +
        s"\"retired\":1,\"byte_writes\":$byteWrites,\"sq_dequeues\":$dequeues," +
        "\"guest_page_walk_validated\":false,\"critical_debug_validated\":false")
      println(s"Store permission Backend ${trial.name} PASS enabled=$enabled cycles=$cycle")
    }

    def run(): Unit = {
      reset()
      // ADDI/SLLI deliberately constructs zero-extended DRAM and PMP bounds in RV64.
      packet(0x1000, Seq(addi(1, 0, 1), slli(1, 1, 31), addi(2, 1, 8), addi(3, 0, 9)))
      idle(40)
      packet(0x1040, Seq(addi(5, 0, 1), slli(5, 5, 30), addi(6, 0, 15), lui(4, 4)))
      idle(40)
      packet(0x1080, Seq(lui(7, 6), addi(9, 0, 0x5a5), addi(10, 0, 0x6b6)))
      idle(40)
      csr(0x1100, csrWrite(0x3b0, 5))
      csr(0x1140, csrWrite(0x3a0, 6))
      if (enabled) {
        csr(0x1200, csrWrite(0x890, 1), flush = true)
        csr(0x1240, csrWrite(0x891, 2), flush = true)
        csr(0x1280, csrWrite(0x880, 3), flush = true)
        csr(0x12c0, csrImmediate(0xbc4, 2), flush = true)
        csr(0x1300, csrImmediate(0x8b3, 4), flush = true)
        dut.memory.mirror(0).expect(2.U)
        dut.memory.mirror(1).expect(9.U)
        dut.memory.mirror(2).expect(baseAddress.U)
        dut.memory.mirror(3).expect((baseAddress + 8).U)
        dut.memory.reason.expect(4.U)
      }
      csr(0x1400, csrWrite(0x305, 7))
      csr(0x1440, csrWrite(0x341, 4))
      csr(0x1480, csrWrite(0x300, 0))
      assert(traps == 0, "Setup must complete without a trap")
      val mretPtr = packet(0x1500, Seq(BigInt(0x30200073)))
      until("architectural HU entry and FTQ recovery") {
        uint(dut.io.mode) == 0 && frontendRedirects.get((mretPtr, BigInt(0))).contains(BigInt(0x4000))
      }
      val storePtr = packet(0x4000, Seq(store(9, 0), store(10, 64)), tag = true)
      if (enabled) {
        until("precise store trap and FTQ recovery") {
          traps == 1 && frontendRedirects.get((storePtr, BigInt(2))).contains(BigInt(0x6000))
        }
        dut.io.mode.expect(3.U)
        dut.io.mcause.expect(24.U)
        dut.io.mepc.expect(0x4004.U)
        dut.io.mtval.expect((baseAddress + 64).U)
        dut.memory.reason.expect(3.U)
        assert(deniedTrapOwner.nonEmpty && !retired.contains(BigInt(0x4004)))
        event("precise-trap-state", s"\"cause\":24,\"epc\":16388,\"tval\":${baseAddress + 64},\"reason\":3")
      } else until("feature-disabled ordinary store retire")(retired.contains(BigInt(0x4000)) && retired.contains(BigInt(0x4004)))
      until("initial SQ drain")(bool(dut.memory.queueEmpty))
      assert(completed.contains(BigInt(0x4000)) && completed.contains(BigInt(0x4004)))
      assert(finalSlots.contains(BigInt(0x4000)) && finalSlots.contains(BigInt(0x4004)))
      assert(published.contains(BigInt(0x4000)))
      assert(published.contains(BigInt(0x4004)) == !enabled)
      idle(16)
      val initialSources = Set(BigInt(0x4000), BigInt(0x4004))
      assert(initialSources.subsetOf(completed.toSet) && initialSources.subsetOf(finalSlots.toSet))
      assert(initialSources.subsetOf(issued.toSet) && initialSources.subsetOf(dataIssued.toSet))
      assert(retired.contains(BigInt(0x4000)))
      assert(retired.contains(BigInt(0x4004)) == !enabled)
      assert(pendingPermissions.isEmpty && metadataReplies.forall(_.isEmpty))
      assert(permissionRequests == permissionResponses)
      dut.memory.queueEmpty.expect(true.B)
      dut.memory.slots.foreach(_.allocated.expect(false.B))
      assert(sourceRobs.values.forall(source => initialSources.contains(source.pc)))
      assert(sourceOwners.values.forall(source => initialSources.contains(source.pc)))
      // A completed trap may recycle the raw ROB/SQ pointers. Live associations
      // end only after the real queue has drained; per-source history is retained.
      sourceRobs.clear()
      sourceOwners.clear()
      sourceLocations.keys.filter(key => initialSources.contains(sourceLocations(key).pc)).toList
        .foreach(sourceLocations.remove)
      event("initial-owner-group-released", "\"completed_sources\":2,\"sq_empty\":true")
      packet(0x6000, Seq(store(9, 8)), tag = true)
      until("following store retires and leaves SQ") {
        retired.contains(BigInt(0x6000)) && published.contains(BigInt(0x6000)) && bool(dut.memory.queueEmpty)
      }
      idle(4)
      assert(traps == (if (enabled) 1 else 0))
      assert(robFaults == (if (enabled) 1 else 0))
      assert(addressOwners.toMap == dataOwners.toMap)
      assert(addressOwners.keySet.toSet == sourceByPc.keySet && dataOwners.keySet.toSet == sourceByPc.keySet)
      assert(issued.toSet == sourceByPc.keySet && dataIssued.toSet == sourceByPc.keySet)
      assert(completed.toSet == sourceByPc.keySet && finalSlots.toSet == sourceByPc.keySet)
      assert(published.toSet == sources.filterNot(_.denied).map(_.pc).toSet)
      assert(byteWrites == published.size)
      assert(permissionRequests.toSet == (if (enabled) sourceByPc.keySet else Set.empty[BigInt]))
      assert(permissionResponses == permissionRequests && pendingPermissions.isEmpty)
      dut.memory.forceFlush.poke(true.B)
      dut.memory.cacheWrite.req.ready.poke(true.B)
      until("real Sbuffer flush") { bool(dut.memory.flushDone) && bool(dut.memory.bufferEmpty) && cacheReplies.isEmpty }
      dut.memory.forceFlush.poke(false.B)
      idle(4)
      val independentBytes = sources.filterNot(_.denied).flatMap { source =>
        (0 until 8).map(byte => (source.address + byte) -> ((source.data >> (8 * byte)) & 255).toInt)
      }.toMap
      assert(expectedBytes.toMap == independentBytes && actualBytes.toMap == independentBytes)
      for (address <- baseAddress - 16 until baseAddress + 96)
        assert(actualBytes.getOrElse(address, 0x5a) == independentBytes.getOrElse(address, 0x5a),
          s"Untouched memory sentinel changed at $address")
      assert(sentLines.toSet == (if (enabled) Set(baseAddress) else Set(baseAddress, baseAddress + 64)))
      assert(metadataReplies.forall(_.isEmpty))
      event("summary", s"\"enabled\":$enabled,\"sources\":3,\"traps\":$traps,\"published\":${published.size}," +
        s"\"empty_tokens\":$emptyTokens,\"sq_cancellations\":$cancellations,\"sq_dequeues\":$dequeues," +
        s"\"cache_lines\":${sentLines.size},\"ifu_validated\":false," +
        "\"nc_mmio_validated\":false,\"full_matrix_validated\":false,\"reconfiguration_overlap_validated\":false")
      println(s"Store permission Backend minimum PASS enabled=$enabled cycles=$cycle stores=3 traps=$traps cacheLines=${sentLines.size}")
    }
  }

  it should "retain exact store faults through ROB and publish only allowed bytes" in {
    val root = Paths.get(sys.props("l03.runRoot")).toRealPath()
    require(Paths.get("").toRealPath() == root, "Run from the dedicated store-permission evidence directory")
    val path = root.resolve(s"store-backend-${if (scenario == "full" || scenario == "source-mode") scenario else "minimal"}-$enabled")
    require(!Files.exists(path), s"Evidence directory already exists: $path")
    Files.createDirectory(path)
    implicit val p: Parameters = UserTimerDeliveryParameters(enabled)
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
      val module = workspace.elaborateGeneratedModule(() => new FDIStorePermissionBackendHarness)
      workspace.generateAdditionalSources()
      val simulation = workspace.compile(Backend.initializeFromProcessEnvironment())("verilator", common, settings, None, false)
      simulation.runElaboratedModule(module) { elaborated =>
        if (scenario == "full") {
          new Driver(elaborated.wrapped, events, recovery = true).runRecovery()
          val trapCases = Seq(TrapCase("hs-delegated", privilege = 1, delegated = true),
            TrapCase("guest-illegal", privilege = 0, virtual = true),
            TrapCase("pmp-standard-winner", privilege = 0, pmpFault = true))
          trapCases.foreach(trial => new Driver(elaborated.wrapped, events, trapCase = Some(trial)).runTrap())
          events += s"{\"event\":\"backend-full-summary\",\"enabled\":$enabled,\"groups\":4,\"critical_debug_validated\":false}"
        } else if (scenario == "source-mode") {
          Seq(false, true).foreach { dataVirtual =>
            new Driver(elaborated.wrapped, events, sourceModeCase = Some(SourceModeCase(dataVirtual))).runSourceMode()
          }
          events += s"{\"event\":\"backend-source-mode-summary\",\"enabled\":$enabled,\"groups\":2," +
            "\"guest_page_walk_validated\":false,\"critical_debug_validated\":false}"
        } else new Driver(elaborated.wrapped, events).run()
      }
    } catch {
      case failure: Throwable =>
        events += s"{\"event\":\"failure\",\"message\":\"${escaped(Option(failure.getMessage).getOrElse(failure.getClass.getName))}\"}"
        throw failure
    } finally {
      Files.write(path.resolve("store-backend-events.jsonl"), events.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
    }
  }
}
