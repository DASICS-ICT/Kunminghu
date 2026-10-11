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
import xiangshan.mem.Bundles.LsPipelineBundle

class FDILoadMisalignTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Scalar misaligned loads through production splitting and translation"

  private val enabled = sys.env.getOrElse("L04_FDI_ENABLED",
    throw new IllegalArgumentException("L04_FDI_ENABLED must be explicit")).toBoolean
  private val scenario = sys.env.getOrElse("L04_SCENARIO", "allowed")
  private val wordMask = (BigInt(1) << 64) - 1
  // Software encodings of the internal wire contract; no Chisel constants are constructed here.
  private val normalMode = BigInt(0)
  private val probeMode = BigInt(1)
  private val terminalMode = BigInt(2)

  private def zero(data: Data): Unit = data match {
    case value: Bool => value.poke(false.B)
    case value: UInt => value.poke(0.U)
    case value: SInt => value.poke(0.S)
    case value: Vec[_] => value.foreach(zero)
    case value: Record => value.elements.values.foreach(zero)
    case other => throw new IllegalArgumentException(s"Unsupported input ${other.getClass.getName}")
  }
  private def uint(data: UInt): BigInt = data.peek().litValue
  private def bool(data: Bool): Boolean = data.peek().litToBoolean
  private def loadMode(mode: Option[UInt]): BigInt = mode.map(uint).getOrElse(normalMode)
  private def isTerminal(payload: LsPipelineBundle): Boolean = {
    val selectedException = bool(payload.hasException)
    val fromMisalign = bool(payload.isFrmMisAlignBuf)
    assert(!selectedException || fromMisalign, "Selected terminal exception lost its MAB origin")
    val terminal = fromMisalign && selectedException
    payload.fdiLoadMode.foreach { field =>
      val mode = uint(field)
      assert(Set(normalMode, probeMode, terminalMode)(mode), "Undefined load mode")
      assert((mode == terminalMode) == terminal, "Terminal mode and native exception marker disagree")
    }
    terminal
  }
  private def faults(data: Vec[Bool]): Set[Int] =
    data.zipWithIndex.collect { case (bit, index) if bool(bit) => index }.toSet

  // The byte oracle is independent of the DUT's splitting, masks and merge result.
  private def byteAt(address: BigInt): BigInt = ((address * 37 + 0x53) ^ (address >> 4)) & 255
  private def bytesAt(address: BigInt, count: Int): BigInt =
    (0 until count).foldLeft(BigInt(0)) { case (value, offset) => value | (byteAt(address + offset) << (8 * offset)) }
  private case class Region(lo: BigInt, hi: BigInt)
  private case class Trial(name: String, address: BigInt = 0x800d, operation: Int = 3,
    fp: Boolean = false, translated: Boolean = false, denied: Boolean = false,
    highPageFault: Boolean = false, highAccessFault: Boolean = false, highNoncache: Boolean = false,
    contendWriteback: Boolean = false, cancelAt: String = "none", pmm: Int = 0,
    regions: Seq[Region] = Seq.empty, nackLow: Int = 0, nackHigh: Int = 0,
    lowPtwDelay: Int = 10, highPtwDelay: Int = 10, evictLowTranslation: Boolean = false,
    stallFragment: Boolean = false, concurrent: Boolean = false, nextAddress: BigInt = 0x810d,
    requireNextQueueReplay: Boolean = false, translationMode: Int = 8,
    guestDataContext: Boolean = false) {
    require(Set("none", "low", "high", "wakeup", "writeback", "probe", "terminal").contains(cancelAt))
    require(Set(8, 9)(translationMode))
    require(!guestDataContext || translated && requireNextQueueReplay && translationMode == 8 && pmm == 0)
    require(!concurrent || cancelAt == "none")
    require(!evictLowTranslation || translated)
    require(!contendWriteback || translated && highPageFault && !fp && operation == 3)
    require(cancelAt != "writeback" || contendWriteback)
    require(!requireNextQueueReplay || concurrent && translated && !contendWriteback && pmm == 0)
    require(!highNoncache || translated && !highPageFault && !highAccessFault)
    val size: Int = 1 << (operation & 3)
    val effective: BigInt = {
      val keep = if (pmm == 2) 57 else if (pmm == 3) 48 else 64
      val mask = (BigInt(1) << keep) - 1
      val low = address & mask
      if (translated && keep != 64 && low.testBit(keep - 1)) low | (wordMask ^ mask) else low
    }
    val highPage: BigInt = (effective >> 12) + 1
    val faultAddress: BigInt = (highPage << 12) & wordMask
    val crossesPage: Boolean = (effective >> 12) != ((effective + size - 1) >> 12)
    // Normalize the original pointer once, add its portion offset, then check the selected VA width.
    val canonicalWidth: Int = if (translationMode == 9) 48 else 39
    val highCanonicalFault: Boolean = translated && crossesPage &&
      (faultAddress >> canonicalWidth) !=
        (if (faultAddress.testBit(canonicalWidth - 1)) (BigInt(1) << (64 - canonicalWidth)) - 1 else BigInt(0))
    val bounds: Seq[Region] = if (regions.nonEmpty) regions else Seq(Region(0x8000,
      if (denied) BigInt(0x9000) else BigInt(0xa000)))
    // One entry must contain the entire access; a union of entries is not an authorization.
    val fdiDenied: Boolean = enabled && !guestDataContext && !bounds.exists(region => region.lo <= effective &&
      effective + size - 1 < region.hi && effective + size - 1 <= wordMask)
    val standardCause: Option[Int] = if (highNoncache) Some(4)
      else if (highPageFault || highCanonicalFault) Some(13)
      else if (highAccessFault) Some(5) else None
    val expectedCause: Option[Int] = standardCause.orElse(if (fdiDenied) Some(24) else None)
    val expectedAddress: BigInt = if (standardCause.nonEmpty && !highNoncache) faultAddress else effective
    def value(at: BigInt = effective): BigInt = {
      val bits = size * 8
      val raw = bytesAt(at, size)
      val mask = (BigInt(1) << bits) - 1
      if (fp && bits < 64 || !fp && operation < 4 && bits < 64 && raw.testBit(bits - 1)) raw | (wordMask ^ mask)
      else raw
    }
  }
  private case class CacheAttempt(cycle: Int, address: BigInt, id: BigInt, rob: Int,
    fragment: String, var killed: Boolean = false)
  private case class PtwReply(due: Int, vpn: BigInt, lq: BigInt)

  private val allowed = Seq(
    Trial("within16-half", address = 0x8001, operation = 1),
    Trial("within16-word", address = 0x8003, operation = 2),
    Trial("within16-double", address = 0x8001),
    Trial("cross16-half", address = 0x800f, operation = 1),
    Trial("cross16-unsigned-half", address = 0x800f, operation = 5)) ++
    (13 to 15).map(offset => Trial(s"cross16-word-$offset", address = 0x8000 + offset, operation = 2)) ++
    (9 to 15).map(offset => Trial(s"cross16-double-$offset", address = 0x8000 + offset)) ++ Seq(
      Trial("cross16-unsigned-word", address = 0x800e, operation = 6),
      Trial("cross16-floating-word", address = 0x800d, operation = 2, fp = true),
      Trial("cross16-floating-double", address = 0x800b, fp = true),
      Trial("cross-page-allowed", address = 0x8ffd, translated = true))
  private val boundaries = Seq(
    Trial("high-page-pf", address = 0x8ffd, translated = true, highPageFault = true),
    Trial("high-page-af", address = 0x8ffd, translated = true, highAccessFault = true),
    Trial("high-page-nc", address = 0x8ffd, translated = true, highNoncache = true),
    Trial("denied-cross-page", address = 0x8ffd, translated = true, denied = true),
    Trial("denied-high-page-pf", address = 0x8ffd, translated = true, denied = true, highPageFault = true),
    Trial("denied-high-page-af", address = 0x8ffd, translated = true, denied = true, highAccessFault = true),
    Trial("floating-high-page-pf", address = 0x8ffd, fp = true, translated = true, highPageFault = true),
    Trial("writeback-stall", address = 0x8ffd, translated = true, highPageFault = true, contendWriteback = true),
    Trial("flush-high-response", cancelAt = "high"),
    Trial("flush-wakeup-response", cancelAt = "wakeup"))
  private val negativePage = BigInt("ffffffc000008000", 16)
  private val sv39PositiveLimit = BigInt(1) << 38
  private val addressCases = Seq(
    Trial("bare-pmm7-split", address = (BigInt(0x55) << 57) | 0x800d, pmm = 2),
    Trial("bare-pmm16-split", address = (BigInt(0xabcd) << 48) | 0x800d, pmm = 3),
    Trial("sv39-high-split", translated = true, concurrent = true, nackLow = 3,
      nextAddress = negativePage + 0xffd, requireNextQueueReplay = true,
      regions = Seq(Region(0x8000, 0xa000), Region(negativePage, negativePage + 0x2000))),
    Trial("sv39-high-page-pf", address = negativePage + 0xffd, translated = true,
      regions = Seq(Region(negativePage, negativePage + 0x2000)), highPageFault = true),
    Trial("sv39-high-denied-page-pf", address = negativePage + 0xffd, translated = true,
      regions = Seq(Region(negativePage, negativePage + 0x1000)), highPageFault = true),
    Trial("sv39-pmm16-split", address = (BigInt(0xabcd) << 48) | ((negativePage + 0xffd) & ((BigInt(1) << 48) - 1)),
      pmm = 3, translated = true, regions = Seq(Region(negativePage, negativePage + 0x2000))),
    Trial("adjacent-bounds-not-spliced", regions = Seq(Region(0x8000, 0x8010), Region(0x8010, 0x8020))),
    Trial("overlapping-bounds-not-spliced", regions = Seq(Region(0x8000, 0x8012), Region(0x8010, 0x8020))),
    Trial("cross-page-bounds-not-spliced", address = 0x8ffd, translated = true,
      regions = Seq(Region(0x8000, 0x9000), Region(0x9000, 0xa000))),
    Trial("sv39-high-noncanonical-allowed", address = sv39PositiveLimit - 3, translated = true,
      regions = Seq(Region(sv39PositiveLimit - 0x1000, sv39PositiveLimit + 0x1000))),
    Trial("sv39-high-noncanonical-denied", address = sv39PositiveLimit - 3, translated = true,
      regions = Seq(Region(sv39PositiveLimit - 0x1000, sv39PositiveLimit))))
  private val retryCases = Seq(
    Trial("low-fragment-nack", nackLow = 2),
    Trial("high-fragment-nack", nackHigh = 2),
    Trial("both-fragments-nack", nackLow = 1, nackHigh = 1),
    Trial("low-fragment-tlb-retry", address = 0x8ffd, translated = true,
      evictLowTranslation = true, lowPtwDelay = 30),
    Trial("high-fragment-tlb-retry", address = 0x8ffd, translated = true, highPtwDelay = 50),
    Trial("denied-high-probe-tlb-retry", address = 0x8ffd, translated = true, denied = true, highPtwDelay = 50))
  private val competitionCases = Seq(
    Trial("fragment-request-backpressure", stallFragment = true),
    Trial("mab-full-two-owners", concurrent = true, nackLow = 3),
    Trial("mab-full-writeback-stall", address = 0x8ffd, translated = true, highPageFault = true,
      concurrent = true, nackLow = 3, contendWriteback = true),
    Trial("flush-low-response-new-owner", cancelAt = "low"),
    Trial("flush-high-nack-new-owner", cancelAt = "high", nackHigh = 1),
    Trial("flush-pending-exception-new-owner", address = 0x8ffd, translated = true, highPageFault = true,
      contendWriteback = true, cancelAt = "writeback"))
  private val pmm16PositiveLimit = BigInt(1) << 47
  private val pmm16BoundaryPointer = (BigInt(0xabcd) << 48) | (pmm16PositiveLimit - 3)
  private val supplementalCommon = Seq(
    Trial("pmm16-sv48-boundary-allowed", address = pmm16BoundaryPointer, translated = true,
      translationMode = 9, pmm = 3,
      regions = Seq(Region(pmm16PositiveLimit - 0x1000, pmm16PositiveLimit + 0x1000))),
    Trial("pmm16-sv48-boundary-denied", address = pmm16BoundaryPointer, translated = true,
      translationMode = 9, pmm = 3,
      regions = Seq(Region(pmm16PositiveLimit - 0x1000, pmm16PositiveLimit))),
    Trial("machine-origin-guest-data-replay", translated = true, concurrent = true, nackLow = 3,
      nextAddress = negativePage + 0xffd, requireNextQueueReplay = true, guestDataContext = true,
      regions = Seq(Region(0x8000, 0xa000), Region(negativePage, negativePage + 0x2000))),
    Trial("flush-native-terminal-new-owner", address = 0x8ffd, translated = true,
      highNoncache = true, cancelAt = "terminal"))
  private val supplementalFdi = Seq(
    Trial("flush-probe-new-owner", address = 0x8ffd, translated = true, denied = true, cancelAt = "probe"),
    Trial("flush-fdi-terminal-new-owner", address = 0x8ffd, translated = true, denied = true, cancelAt = "terminal"))
  private val supplemental = supplementalCommon ++ (if (enabled) supplementalFdi else Seq.empty)
  private val groups = Map("allowed" -> allowed, "boundaries" -> boundaries, "addresses" -> addressCases,
    "retries" -> retryCases, "competition" -> competitionCases, "supplemental" -> supplemental)
  // Existing all/group selectors retain their original 52 cases; additions have a separate entry.
  private val allCases = allowed ++ boundaries ++ addressCases ++ retryCases ++ competitionCases
  private val selectableCases = allCases ++ supplemental
  require(groups.contains(scenario) || scenario == "all" || selectableCases.exists(_.name == scenario),
    s"Unknown or feature-inapplicable L04_SCENARIO: $scenario")

  private def run(dut: FDILoadMisalignHarness, trial: Trial, events: mutable.ArrayBuffer[String]): Unit = {
    var cycle = 0
    val primaryRob = 10
    val nextRob = 12
    val nextAddress = trial.nextAddress
    val warmOwner = 8
    val warmAddress = (nextAddress & ~BigInt(0xfff)) + 0x80
    val backgroundOwners = if (trial.contendWriteback) (32 until 48).toVector else Vector.empty[Int]
    val backgroundAddresses = backgroundOwners.map(rob => rob -> (BigInt(0x8800) + (rob - 32) * 8)).toMap
    require(backgroundOwners.isEmpty || dut.RobSize > backgroundOwners.last &&
      dut.VirtualLoadQueueSize > backgroundOwners.last &&
      (BigInt(1) << dut.io.request.bits.uop.pdest.getWidth) > backgroundOwners.last + 1)
    def rawOwnerAddress(rob: Int): BigInt = if (rob == primaryRob) trial.address
      else if (rob == nextRob) nextAddress
      else if (trial.requireNextQueueReplay && rob == warmOwner) warmAddress
      else backgroundAddresses(rob)
    val vaddrMask = (BigInt(1) << dut.io.phases.tlbRequest.bits.vaddr.getWidth) - 1
    val vpnMask = (BigInt(1) << dut.io.ptw.req.head.bits.vpn.getWidth) - 1
    def vpnOf(address: BigInt): BigInt = (address >> 12) & vpnMask
    // The existing TLB service key is the truncated request vaddr. PMM normalizes
    // fullva independently, so a tagged request key is distinct from its full-VA oracle.
    def ownerIndexAddress(rob: Int): BigInt = rawOwnerAddress(rob) & vaddrMask
    val primaryLowVpn = vpnOf(ownerIndexAddress(primaryRob))
    val primaryHighVpn = (primaryLowVpn + 1) & vpnMask
    val nextLowVpn = vpnOf(ownerIndexAddress(nextRob))
    val nextHighVpn = (nextLowVpn + 1) & vpnMask
    val serviceVpns = Set(primaryLowVpn, primaryHighVpn) ++
      (if (trial.requireNextQueueReplay) Set(nextLowVpn, nextHighVpn) else Set.empty[BigInt])
    def ownerAddress(rob: Int): BigInt = if (rob == primaryRob) trial.effective else rawOwnerAddress(rob)
    // The guest control retains its native extension-needed representation. Host cases remain full-width.
    def savedAddress(rob: Int): BigInt = if (trial.guestDataContext && !enabled)
      ownerIndexAddress(rob) else ownerAddress(rob)
    val canceled = trial.cancelAt != "none"
    val expectsProbe = trial.fdiDenied && trial.crossesPage
    val expectsTerminal = expectsProbe || trial.highNoncache
    val internalCancellation = trial.cancelAt == "probe" || trial.cancelAt == "terminal"
    val attempts = mutable.ArrayBuffer.empty[CacheAttempt]
    val pendingPtw = mutable.ArrayBuffer.empty[PtwReply]
    val scheduledVpns = mutable.Set.empty[BigInt]
    val requestedVpns = mutable.Set.empty[BigInt]
    val deniedHighTranslations = mutable.Set.empty[Int]
    val hintCycles = mutable.Set.empty[Int]
    val issued = mutable.Set.empty[Int]
    val completed = mutable.Set.empty[Int]
    val terminalRows = mutable.Map.empty[Int, Int].withDefaultValue(0)
    var noncacheFragmentResponses = 0
    var noncacheTerminalRows = 0
    var uncacheEnqueues = 0
    var uncacheRequests = 0
    var uncachePeakAllocated = BigInt(0)
    var cancelCycle = -1
    var highRequests = 0
    var wakeupRequests = 0
    var lateResponses = 0
    var latePtwResponses = 0
    var cancellationRequestCycle = -1
    var guestTlbRequests = 0
    var guestTlbResponses = 0
    var guestSplitRequests = 0
    val guestResponseDue = mutable.Map.empty[Int, (Int, BigInt)]
    var contentionStreamStarted = false
    var contentionCycles = 0
    var heldIssue: Option[Int] = None
    var completedBeforeRedirect = Set.empty[Int]
    var killedOwners = Set.empty[Int]
    var heldException: Option[(Boolean, BigInt, Boolean, BigInt, BigInt, BigInt, BigInt, Set[Int])] = None
    val splitCompletions = mutable.Map.empty[Int, Int].withDefaultValue(0)
    var finalCycle = -1
    var firstEnqueueCycle = -1
    var primaryQueueReplays = 0
    var nextMisalignNackSeen = false
    var nextQueueReplaySeen = false
    var nextReplayEnqueueSeen = false
    var canonicalHighKills = 0
    var canonicalHighDataReplies = 0
    var splitStallCycles = 0
    var fullReplayRows = 0
    var lowTranslationEvicted = false
    var heldFragment: Option[(BigInt, BigInt, BigInt, BigInt, Boolean, Boolean, Boolean, Boolean)] = None
    var terminalRequests = 0
    var terminalAcks = 0
    var terminalLsqRows = 0
    var terminalRequestCycle = -1
    val injectedNacks = mutable.Map("low" -> 0, "high" -> 0)
    val fragmentCacheAttempts = mutable.Map("low" -> 0, "high" -> 0)
    val fragmentTlbRetries = mutable.Map("low" -> 0, "high" -> 0)

    def event(kind: String, detail: String = ""): Unit = events +=
      s"{\"case\":\"${trial.name}\",\"cycle\":$cycle,\"event\":\"$kind\"${if (detail.isEmpty) "" else "," + detail}}"

    zero(dut.io.request.bits); dut.io.request.valid.poke(false.B)
    zero(dut.io.tlbCsr); zero(dut.io.csrCtrl); zero(dut.io.pmpEnvironment)
    zero(dut.io.sfence); zero(dut.io.redirect.bits); dut.io.redirect.valid.poke(false.B)
    zero(dut.io.ptw.resp.bits); dut.io.ptw.resp.valid.poke(false.B)
    dut.io.ptw.req.foreach(_.ready.poke(true.B))
    zero(dut.io.tlbHint.bits); dut.io.tlbHint.valid.poke(false.B)
    zero(dut.io.cacheResponse.bits); dut.io.cacheResponse.valid.poke(false.B)
    dut.io.cacheReady.poke(true.B); dut.io.cacheBankConflict.poke(false.B); dut.io.cacheNack.poke(false.B)
    dut.io.result.ready.poke(true.B)
    dut.io.csrCtrl.hd_misalign_ld_enable.poke(true.B)
    dut.io.csrCtrl.cache_error_enable.poke(true.B)
    dut.io.tlbCsr.priv.imode.poke((if (trial.guestDataContext) 3 else 0).U)
    dut.io.tlbCsr.priv.dmode.poke((if (trial.translated) 0 else 3).U)
    dut.io.tlbCsr.priv.virt.poke(trial.guestDataContext.B)
    dut.io.tlbCsr.satp.mode.poke((if (trial.translated) trial.translationMode else 0).U)
    dut.io.tlbCsr.vsatp.mode.poke((if (trial.guestDataContext) 8 else 0).U)
    dut.io.tlbCsr.hgatp.mode.poke(0.U)
    dut.io.tlbCsr.mPBMTE.poke(trial.highNoncache.B)
    dut.io.tlbCsr.pmm.mseccfg.poke(trial.pmm.U)
    dut.io.tlbCsr.pmm.senvcfg.poke(trial.pmm.U)
    dut.io.pmpEnvironment.cmode.poke(true.B)
    dut.io.pmpEnvironment.mode.poke(0.U)
    for (entries <- Seq(dut.io.pmpEnvironment.pmp, dut.io.pmpEnvironment.pma)) {
      val first = entries.head
      first.cfg.a.poke(1.U); first.addr.poke(0x40000.U)
      first.cfg.r.poke(true.B); first.cfg.w.poke(true.B); first.cfg.x.poke(true.B)
    }
    dut.io.pmpEnvironment.pma.head.cfg.c.poke(true.B)
    dut.io.config.foreach { config =>
      zero(config)
      config.sourcePrivilege.poke((if (trial.guestDataContext) 3 else 0).U)
      config.policy.uEnable.poke(true.B)
      for ((region, index) <- trial.bounds.zipWithIndex) {
        config.entries(index).boundLo.poke(region.lo.U)
        config.entries(index).boundHi.poke(region.hi.U)
        config.entries(index).entryValid.poke(true.B)
        config.entries(index).readAllowed.poke(true.B)
      }
    }
    dut.reset.poke(true.B); dut.clock.step(10)
    dut.reset.poke(false.B); dut.clock.step(20)
    event("begin", s"\"address\":${trial.address},\"effective\":${trial.effective},\"bytes\":${trial.size},\"enabled\":$enabled")

    def offer(rob: Int, address: BigInt): Unit = {
      zero(dut.io.request.bits)
      dut.io.request.bits.src(0).poke(address.U)
      val uop = dut.io.request.bits.uop
      uop.pc.poke((0x4000 + 4 * rob).U)
      uop.robIdx.value.poke(rob.U); uop.lqIdx.value.poke(rob.U)
      uop.pdest.poke((rob + 1).U)
      uop.fuType.poke(FuType.ldu.U); uop.fuOpType.poke(trial.operation.U)
      uop.rfWen.poke((!trial.fp).B); uop.fpWen.poke(trial.fp.B)
      uop.firstUop.poke(true.B); uop.lastUop.poke(true.B)
      uop.fdiNotTrusted.foreach(_.poke(true.B))
      dut.io.request.bits.isFirstIssue.poke(true.B)
      dut.io.request.valid.poke(true.B)
    }

    while (cycle < 1200 && (finalCycle < 0 || cycle < finalCycle + 16)) {
      dut.io.request.valid.poke(false.B)
      val holdFragment = firstEnqueueCycle >= 0 && cycle > firstEnqueueCycle &&
        cycle <= firstEnqueueCycle + (if (trial.evictLowTranslation) dut.ldtlbParams.fenceDelay + 4 else 7) &&
        (trial.stallFragment || trial.evictLowTranslation)
      dut.io.cacheReady.poke((!holdFragment).B)
      dut.io.sfence.valid.poke(false.B)
      if (trial.evictLowTranslation && !lowTranslationEvicted && firstEnqueueCycle >= 0) {
        dut.io.sfence.valid.poke(true.B)
        dut.io.sfence.bits.rs1.poke(true.B); dut.io.sfence.bits.rs2.poke(true.B)
        scheduledVpns.clear()
        lowTranslationEvicted = true
        event("evict-low-translation")
      }
      dut.io.redirect.valid.poke((cycle == cancelCycle).B)
      if (cycle == cancelCycle) {
        dut.io.redirect.bits.robIdx.value.poke(primaryRob.U)
        dut.io.redirect.bits.level.poke(RedirectLevel.flush)
        completedBeforeRedirect = completed.toSet
        killedOwners = issued.toSet -- completedBeforeRedirect
        heldIssue = None
        heldException = None
        heldFragment = None
        event("redirect", s"\"rob\":$primaryRob")
      }
      zero(dut.io.cacheResponse.bits); dut.io.cacheResponse.valid.poke(false.B)
      dut.io.cacheNack.poke(false.B)
      dut.io.ptw.resp.valid.poke(false.B)
      dut.io.tlbHint.valid.poke(hintCycles(cycle).B)
      dut.io.tlbHint.bits.replay_all.poke(true.B)

      attempts.find(_.cycle + 1 == cycle).foreach { attempt =>
        attempt.killed = bool(dut.io.cacheS1Kill)
        if (trial.highCanonicalFault && attempt.rob == primaryRob && vpnOf(attempt.address) == primaryHighVpn) {
          assert(attempt.killed, "Noncanonical high-fragment lookup was not killed in its S1 cycle")
          canonicalHighKills += 1
          event("canonical-high-s1-kill", s"\"rob\":${attempt.rob},\"request_cycle\":${attempt.cycle}")
        }
      }
      attempts.find(_.cycle + 2 == cycle).foreach { attempt =>
        // A canceled cache transaction may still return; only the pipeline may discard it.
        if (!attempt.killed || canceled && cancelCycle >= 0 && cycle >= cancelCycle) {
          dut.io.cacheResponse.valid.poke(true.B)
          if (trial.highCanonicalFault && attempt.rob == primaryRob && vpnOf(attempt.address) == primaryHighVpn) {
            canonicalHighDataReplies += 1
          }
          dut.io.cacheResponse.bits.id.poke(attempt.id.U)
          val targetNacks = if (attempt.fragment == "low") trial.nackLow else trial.nackHigh
          if (attempt.rob == primaryRob && injectedNacks.contains(attempt.fragment) &&
            injectedNacks(attempt.fragment) < targetNacks) {
            dut.io.cacheNack.poke(true.B)
            injectedNacks(attempt.fragment) += 1
            event("cache-nack", s"\"fragment\":\"${attempt.fragment}\",\"rob\":${attempt.rob}")
          }
          dut.io.cacheResponse.bits.data.poke(bytesAt(attempt.address & ~BigInt(15), 16).U)
          if (canceled && cancelCycle >= 0 && cycle >= cancelCycle && attempt.rob == primaryRob) {
            lateResponses += 1
            event("late-cache-response", s"\"rob\":${attempt.rob}")
          }
        }
      }
      attempts.find(_.cycle + 3 == cycle).foreach { attempt =>
        dut.io.cacheResponse.bits.data_delayed.poke(bytesAt(attempt.address & ~BigInt(15), 16).U)
      }

      pendingPtw.find(_.due == cycle).foreach { reply =>
        zero(dut.io.ptw.resp.bits)
        val response = dut.io.ptw.resp.bits
        response.s2xlate.poke((if (trial.guestDataContext) 1 else 0).U)
        val sector = response.s1.pteidx.length
        val index = (reply.vpn % sector).toInt
        val high = reply.vpn == primaryHighVpn
        val pf = high && trial.highPageFault
        // A discontiguous high-page PPN makes only that page fail the real PMP checker.
        val ppn = if (high && trial.highAccessFault) BigInt(0x200)
          else if (trial.highCanonicalFault) BigInt(if (high) 9 else 8)
          else (reply.vpn & 0xff)
        if (trial.highCanonicalFault) {
          assert(!pf && !trial.highAccessFault && ppn >= 8 && ppn <= 9,
            "Canonical-address cases require readable PTEs and reachable physical pages")
        }
        response.memidx.is_ld.poke(true.B); response.memidx.idx.poke(reply.lq.U)
        response.s1.entry.tag.poke((reply.vpn / sector).U)
        if (trial.guestDataContext) {
          response.s1.entry.asid.poke(0.U)
          val guestVmid = response.s1.entry.vmid.getOrElse(
            throw new IllegalArgumentException("Guest data-context fixture requires the PTW stage-1 VMID field"))
          guestVmid.poke(0.U)
        }
        response.s1.entry.level.foreach(_.poke(0.U))
        response.s1.entry.ppn.poke((ppn / sector).U)
        response.s1.entry.v.poke((!pf).B)
        val pbmt = if (trial.highNoncache && high) 1 else 0
        response.s1.entry.pbmt.poke(pbmt.U)
        response.s1.entry.perm.foreach { perm =>
          perm.r.poke(true.B); perm.a.poke(true.B); perm.d.poke(true.B); perm.u.poke(true.B)
        }
        response.s1.addr_low.poke(index.U); response.s1.pf.poke(pf.B)
        response.s1.valididx.zipWithIndex.foreach { case (bit, i) => bit.poke((i == index).B) }
        response.s1.pteidx.zipWithIndex.foreach { case (bit, i) => bit.poke((i == index).B) }
        response.s1.ppn_low.zipWithIndex.foreach { case (low, i) => low.poke((if (i == index) ppn % sector else BigInt(i)).U) }
        dut.io.ptw.resp.valid.poke(true.B)
        assert(bool(dut.io.ptw.resp.ready), "PTW service reply was not accepted")
        if (internalCancellation && cancelCycle >= 0 && cycle >= cancelCycle && reply.lq == primaryRob) {
          latePtwResponses += 1
          lateResponses += 1
          event("late-ptw-response", s"\"rob\":$primaryRob,\"vpn\":${reply.vpn}")
        }
        if (trial.contendWriteback && high && pf) {
          contentionStreamStarted = true
          event("ordinary-stream-start")
        }
        hintCycles += cycle + 4
        event("ptw-response", s"\"vpn\":${reply.vpn},\"ppn\":$ppn,\"pf\":$pf,\"pbmt\":$pbmt")
      }

      // The configured scalar Backend always accepts this final port. Contention
      // is created by real ordinary LoadUnit results winning the internal MAB mux.
      dut.io.result.ready.expect(true.B)
      if (heldIssue.isEmpty) {
        heldIssue = if (trial.requireNextQueueReplay && !issued(warmOwner)) Some(warmOwner)
          else if (trial.requireNextQueueReplay && !completed(warmOwner)) None
          else if (!issued(primaryRob)) Some(primaryRob)
          else if (canceled && cancelCycle >= 0 && cycle > cancelCycle && !issued(nextRob)) Some(nextRob)
          else if (trial.contendWriteback && contentionStreamStarted && cancelCycle < 0)
            backgroundOwners.find(rob => !issued(rob))
              .orElse(if (trial.concurrent && !issued(nextRob) && bool(dut.io.phases.splitFull)) Some(nextRob) else None)
          else if (trial.concurrent && !issued(nextRob) && bool(dut.io.phases.splitFull)) Some(nextRob)
          else None
      }
      heldIssue.foreach(rob => offer(rob, rawOwnerAddress(rob)))
      if (bool(dut.io.request.valid) && bool(dut.io.request.ready)) {
        val rob = uint(dut.io.request.bits.uop.robIdx.value).toInt
        assert(issued.add(rob), "Duplicate source issue")
        heldIssue = None
        event("issue", s"\"rob\":$rob")
      }
      if (bool(dut.io.cacheRequest.valid)) {
        val address = uint(dut.io.cacheRequest.bits.vaddr)
        val rob = uint(dut.io.phases.s0Rob).toInt
        if (trial.fdiDenied && rob == primaryRob) {
          assert(vpnOf(address) != primaryHighVpn, "Denied high fragment issued a cache data request")
        }
        val fromSplit = bool(dut.io.phases.s0FromMisalign)
        if (trial.fdiDenied && rob == primaryRob) {
          assert(!fromSplit, "Denied owner issued a fragment cache data request")
        }
        // A split fragment can fast-replay without a new request from the MAB.
        // Classify each accepted cache attempt by its saved owner and block address.
        val highBlock = (ownerIndexAddress(rob) & ~BigInt(15)) + 16
        val fragment = if (!fromSplit) "original" else if (address >= highBlock) "high" else "low"
        if (rob == primaryRob && fromSplit) fragmentCacheAttempts(fragment) += 1
        attempts += CacheAttempt(cycle, address, uint(dut.io.cacheRequest.bits.id), rob, fragment)
        event("cache-request", s"\"rob\":$rob,\"address\":$address")
      }
      if (trial.fdiDenied && bool(dut.io.phases.tlbRequest.valid) &&
        vpnOf(uint(dut.io.phases.tlbRequest.bits.vaddr)) == primaryHighVpn) {
        deniedHighTranslations += cycle
        dut.io.phases.ordinaryWakeup.expect(false.B)
        event("denied-high-translation", s"\"address\":${uint(dut.io.phases.tlbRequest.bits.vaddr)}")
      }
      if (deniedHighTranslations(cycle - 1)) {
        dut.io.phases.sqForward.expect(false.B)
        dut.io.phases.sbufferForward.expect(false.B)
        dut.io.phases.mshrForward.expect(false.B)
        dut.io.phases.uncacheForward.expect(false.B)
      }
      if (deniedHighTranslations(cycle - 2)) dut.io.phases.fastWakeup.expect(false.B)
      if (deniedHighTranslations(cycle - 3)) dut.io.phases.l2lForward.expect(false.B)
      if (trial.guestDataContext) {
        guestResponseDue.remove(cycle).foreach { case (rob, expectedFullva) =>
          val response = dut.io.phases.tlbResponse
          response.valid.expect(true.B)
          response.bits.memidx.idx.expect(rob.U)
          response.bits.fullva.expect(expectedFullva.U)
          response.bits.excp.foreach { exception =>
            exception.vaNeedExt.expect(true.B)
            if (!bool(response.bits.miss)) {
              exception.pf.ld.expect(false.B); exception.gpf.ld.expect(false.B); exception.af.ld.expect(false.B)
            }
          }
          guestTlbResponses += 1
          event("guest-data-tlb-response", s"\"rob\":$rob,\"fullva\":$expectedFullva,\"va_need_ext\":true")
        }
        val request = dut.io.phases.tlbRequest
        if (bool(request.valid) && !bool(request.bits.no_translate)) {
          val rob = uint(request.bits.memidx.idx).toInt
          val fromSplit = bool(dut.io.phases.s0FromMisalign)
          val first = bool(request.bits.debug.isFirstIssue)
          val expectedFullva = if (fromSplit || !first) savedAddress(rob) else ownerAddress(rob)
          request.bits.fullva.expect(expectedFullva.U)
          request.bits.fullvaKind.expect((if (fromSplit) 0 else 2).U)
          request.bits.checkfullva.expect((!fromSplit && first).B)
          assert(!guestResponseDue.contains(cycle + 1), "Two guest transactions shared one TLB response slot")
          guestResponseDue(cycle + 1) = (rob, expectedFullva)
          guestTlbRequests += 1
          event("guest-data-tlb-request", s"\"rob\":$rob,\"fullva\":$expectedFullva,\"from_mab\":$fromSplit,\"first\":$first")
        }
      }
      for ((request, lane) <- dut.io.ptw.req.zipWithIndex if bool(request.valid) && bool(request.ready)) {
        val vpn = uint(request.bits.vpn)
        assert(trial.translated && lane == 0, "Unexpected PTW source")
        if (trial.guestDataContext) request.bits.s2xlate.expect(1.U)
        assert(serviceVpns(vpn), "PTW request escaped the accepted source pages")
        requestedVpns += vpn
        val delay = if (vpn == primaryHighVpn) trial.highPtwDelay else trial.lowPtwDelay
        if (scheduledVpns.add(vpn)) pendingPtw += PtwReply(cycle + delay, vpn, uint(request.bits.memidx.idx))
        event("ptw-request", s"\"vpn\":$vpn")
      }
      if (bool(dut.io.phases.replay.valid)) {
        val replay = dut.io.phases.replay.bits
        val rob = uint(replay.uop.robIdx.value).toInt
        assert(issued(rob), "Replay has no accepted source owner")
        replay.hasException.expect(false.B)
        replay.vaddr.expect(ownerIndexAddress(rob).U)
        if (enabled) replay.fullva.expect(ownerAddress(rob).U)
        if (rob == primaryRob) primaryQueueReplays += 1
        if (trial.requireNextQueueReplay && rob == nextRob) {
          assert(nextMisalignNackSeen, "Negative owner replayed without the required MAB-full rejection")
          nextQueueReplaySeen = true
        }
        val fullva = if (enabled) uint(replay.fullva).toString else "null"
        event("queue-replay", s"\"rob\":$rob,\"index_vaddr\":${uint(replay.vaddr)},\"fullva\":$fullva,\"expected_fullva\":${ownerAddress(rob)}")
      }
      if (bool(dut.io.phases.splitEnqueue.valid)) {
        val enqueue = dut.io.phases.splitEnqueue.bits
        val rob = uint(enqueue.uop.robIdx.value).toInt
        enqueue.fullva.expect(savedAddress(rob).U)
        enqueue.hasException.expect(false.B)
        enqueue.vaNeedExt.expect(trial.guestDataContext.B)
        if (trial.requireNextQueueReplay && rob == nextRob) {
          assert(nextMisalignNackSeen && nextQueueReplaySeen && bool(enqueue.isLoadReplay),
            "Negative owner did not enter the MAB from its witnessed ordinary queue replay")
          nextReplayEnqueueSeen = true
        }
        if (firstEnqueueCycle < 0) firstEnqueueCycle = cycle
        event("mab-enqueue", s"\"rob\":$rob,\"fullva\":${uint(enqueue.fullva)},\"expected_fullva\":${savedAddress(rob)},\"from_queue_replay\":${bool(enqueue.isLoadReplay)},\"from_fast_replay\":${bool(enqueue.isFastReplay)},\"primary_queue_replays\":$primaryQueueReplays")
      }
      val splitRequest = dut.io.phases.splitRequest
      def fragmentPayload = (uint(splitRequest.bits.uop.robIdx.value), uint(splitRequest.bits.vaddr),
        uint(splitRequest.bits.fullva), loadMode(splitRequest.bits.fdiLoadMode),
        bool(splitRequest.bits.hasException), bool(splitRequest.bits.isFrmMisAlignBuf),
        bool(splitRequest.bits.isFinalSplit), bool(splitRequest.bits.misalignNeedWakeUp))
      heldFragment.foreach { previous =>
        assert(bool(splitRequest.valid) || bool(dut.io.phases.splitBackpressured), "Stalled fragment request disappeared")
        assert(fragmentPayload == previous, "Stalled fragment changed owner or address")
      }
      heldFragment = if (bool(dut.io.phases.splitBackpressured)) Some(fragmentPayload) else None
      if (bool(dut.io.phases.splitBackpressured)) splitStallCycles += 1
      if (bool(splitRequest.valid)) {
        val rob = uint(splitRequest.bits.uop.robIdx.value).toInt
        assert(issued(rob), "Fragment has no accepted source")
        val mode = loadMode(splitRequest.bits.fdiLoadMode)
        val terminal = isTerminal(splitRequest.bits)
        val probe = mode == probeMode
        val wakeup = bool(splitRequest.bits.misalignNeedWakeUp)
        val high = bool(splitRequest.bits.isFinalSplit) && !wakeup
        // A terminal carries a selected final fault address, not a LOW data address.
        // Ordinary HIGH addresses are independently derived from the saved owner.
        val expectedFullva = if (terminal) trial.expectedAddress
          else if (trial.guestDataContext) savedAddress(rob)
          else if (high) (ownerAddress(rob) + 16 - (ownerAddress(rob) & 15)) & wordMask
          else ownerAddress(rob)
        splitRequest.bits.fullva.expect(expectedFullva.U)
        splitRequest.bits.vaNeedExt.expect(trial.guestDataContext.B)
        dut.io.phases.splitSavedAddress.expect(savedAddress(rob).U)
        if (trial.guestDataContext) guestSplitRequests += 1
        if (terminal) {
          assert(expectsTerminal && rob == primaryRob && terminalRequests == 0,
            "Unexpected or duplicate terminal request")
          assert(!high && !wakeup, "Terminal request was encoded as a data fragment or wakeup")
          splitRequest.bits.vaddr.expect((trial.expectedAddress & vaddrMask).U)
          splitRequest.bits.isMisalign.expect(false.B)
          splitRequest.bits.data.expect(0.U)
          val uop = splitRequest.bits.uop
          uop.robIdx.flag.expect(false.B); uop.robIdx.value.expect(primaryRob.U)
          uop.lqIdx.flag.expect(false.B); uop.lqIdx.value.expect(primaryRob.U)
          uop.uopIdx.expect(0.U); uop.pc.expect((0x4000 + 4 * primaryRob).U)
          uop.pdest.expect((primaryRob + 1).U)
          uop.rfWen.expect(false.B); uop.fpWen.expect(false.B)
          val cause = trial.expectedCause.get
          val candidates = faults(uop.exceptionVec)
          val permittedCandidates = Set(cause) ++ (if (trial.fdiDenied) Set(24) else Set.empty[Int])
          assert(candidates(cause) && (candidates -- permittedCandidates).isEmpty,
            "Terminal request did not preserve the independently selected fault")
          if (cause == 24) {
            uop.fdiException.get.tval.expect(trial.effective.U)
            uop.fdiException.get.reason.expect(2.U)
          }
          dut.io.cacheRequest.valid.expect(false.B)
          dut.io.phases.tlbRequest.valid.expect(false.B)
          terminalRequests += 1
          terminalRequestCycle = cycle
          if (trial.cancelAt == "terminal" && cancelCycle < 0) {
            cancellationRequestCycle = cycle
            cancelCycle = cycle + 2
          }
          event("terminal-request", s"\"rob\":$rob,\"fullva\":$expectedFullva,\"cause\":$cause")
        } else {
          if (probe) {
            assert(expectsProbe && rob == primaryRob && high && !wakeup,
              "Probe did not belong to the denied owner's high fragment")
            dut.io.cacheRequest.valid.expect(false.B)
            if (trial.cancelAt == "probe" && cancelCycle < 0) {
              cancellationRequestCycle = cycle
              cancelCycle = cycle + 2
            }
          }
          val fragmentLine = uint(splitRequest.bits.vaddr) & ~BigInt(15)
          val ownerLine = ownerIndexAddress(rob) & ~BigInt(15)
          assert(fragmentLine == ownerLine || fragmentLine == ownerLine + 16,
            "Fragment address escaped its original owner's two data blocks")
          if (rob == primaryRob) {
            if (high) highRequests += 1
            if (wakeup) wakeupRequests += 1
            if (cancelCycle < 0 && (trial.cancelAt == "low" && !high && !wakeup ||
              trial.cancelAt == "high" && high || trial.cancelAt == "wakeup" && wakeup)) {
              cancelCycle = cycle + 1
            }
          }
        }
        event("split-request", s"\"rob\":$rob,\"address\":${uint(splitRequest.bits.vaddr)},\"fullva\":${uint(splitRequest.bits.fullva)},\"expected_fullva\":$expectedFullva,\"saved_owner_fullva\":${savedAddress(rob)},\"mode\":$mode,\"high\":$high,\"wakeup\":$wakeup")
      }
      if (bool(dut.io.phases.splitResponse.valid)) {
        val response = dut.io.phases.splitResponse.bits
        val terminal = isTerminal(response)
        if (terminal) {
          assert(expectsTerminal && terminalRequests == 1 && terminalAcks == 0 && cycle > terminalRequestCycle,
            "Terminal acknowledgement has no unique accepted request")
          response.uop.robIdx.flag.expect(false.B); response.uop.robIdx.value.expect(primaryRob.U)
          response.uop.lqIdx.flag.expect(false.B); response.uop.lqIdx.value.expect(primaryRob.U)
          response.uop.uopIdx.expect(0.U)
          response.uop.pc.expect((0x4000 + 4 * primaryRob).U)
          response.uop.pdest.expect((primaryRob + 1).U)
          response.uop.rfWen.expect(false.B); response.uop.fpWen.expect(false.B)
          response.fullva.expect(trial.expectedAddress.U)
          response.vaNeedExt.expect(false.B)
          response.isFinalSplit.expect(false.B); response.misalignNeedWakeUp.expect(false.B)
          assert(!response.rep_info.cause.exists(bool), "Terminal acknowledgement requested replay")
          dut.io.result.valid.expect(true.B); dut.io.result.ready.expect(true.B)
          dut.io.result.bits.uop.robIdx.value.expect(primaryRob.U)
          dut.io.phases.splitWriteback.valid.expect(false.B)
          terminalAcks += 1
          event("terminal-ack", s"\"rob\":$primaryRob,\"fullva\":${uint(response.fullva)}")
        }
        if (trial.highNoncache && !terminal && uint(response.uop.robIdx.value) == primaryRob &&
          bool(response.isFinalSplit) && !bool(response.rep_info.tlb_miss) && bool(response.nc)) {
          noncacheFragmentResponses += 1
          assert(!faults(response.uop.exceptionVec).exists(Set(5, 13)),
            "Noncache fixture accidentally substituted a physical or page fault")
          event("noncache-high-fragment", s"\"rob\":$primaryRob,\"fullva\":${uint(response.fullva)}")
        }
        if (!terminal && uint(response.uop.robIdx.value) == primaryRob && bool(response.rep_info.tlb_miss)) {
          val fragment = if (bool(response.isFinalSplit)) "high" else "low"
          fragmentTlbRetries(fragment) += 1
          event("split-tlb-retry", s"\"fragment\":\"$fragment\"")
        }
        if (canceled && cancelCycle >= 0 && cycle >= cancelCycle && uint(response.uop.robIdx.value) == primaryRob) {
          lateResponses += 1
          event("late-split-response", s"\"wakeup\":${bool(response.misalignNeedWakeUp)}")
        }
      }
      if (bool(dut.io.phases.permissionRequest.valid)) {
        val request = dut.io.phases.permissionRequest.bits
        val shift = dut.io.request.bits.uop.lqIdx.getWidth + dut.io.request.bits.uop.uopIdx.getWidth
        val rob = (uint(request.tag) >> shift).toInt
        assert(issued(rob), "Permission request has no accepted owner")
        request.address.expect(ownerAddress(rob).U)
        dut.io.phases.permissionRequest.bits.sizeLog2.expect((trial.operation & 3).U)
      }
      if (bool(dut.io.phases.queueWrite.valid)) {
        val row = dut.io.phases.queueWrite.bits
        val rob = uint(row.uop.robIdx.value).toInt
        if (rob == nextRob && bool(row.rep_info.misalign_nack)) {
          if (trial.requireNextQueueReplay) {
            assert(bool(dut.io.phases.splitFull) && uint(dut.io.phases.splitOwner) == primaryRob,
              "Required misalign_nack did not originate from the first owner occupying MAB")
            nextMisalignNackSeen = true
          }
          fullReplayRows += 1
          event("mab-full-replay", s"\"rob\":$rob")
        }
        // Hardware reduction helpers are elaboration-only; observe the existing cause leaves.
        val requiresReplay = row.rep_info.cause.exists(bool)
        if (internalCancellation && cancelCycle >= 0 && cycle >= cancelCycle && rob == primaryRob) {
          assert(!bool(row.updateAddrValid) || requiresReplay,
            "Canceled internal transaction published a terminal LSQ row")
        }
        if (isTerminal(row)) {
          assert(expectsTerminal && rob == primaryRob && terminalRequests == 1,
            "Terminal LSQ record has no accepted terminal owner")
          row.fullva.expect(trial.expectedAddress.U)
          row.updateAddrValid.expect(true.B)
          assert(!requiresReplay, "Terminal LSQ record was marked for retry")
          terminalLsqRows += 1
        }
        if (bool(row.updateAddrValid) && !requiresReplay) {
          terminalRows(rob) += 1
          if (trial.highNoncache && rob == primaryRob && faults(row.uop.exceptionVec) == Set(4)) {
            noncacheTerminalRows += 1
            row.fullva.expect(trial.effective.U)
            row.vaNeedExt.expect(false.B)
          }
          event("terminal-row", s"\"rob\":$rob,\"address\":${uint(row.fullva)},\"exceptions\":[${faults(row.uop.exceptionVec).toSeq.sorted.mkString(",")}]")
        }
      }

      if (trial.highNoncache || internalCancellation) {
        if (bool(dut.io.phases.uncacheEnqueue)) {
          uncacheEnqueues += 1
          event("uncache-enqueue")
        }
        uncachePeakAllocated = uncachePeakAllocated.max(uint(dut.io.phases.uncacheAllocated))
        if (bool(dut.io.phases.uncacheRequest)) {
          uncacheRequests += 1
          event("uncache-data-request", s"\"address\":${uint(dut.io.phases.uncacheRequestAddress)}")
        }
      }

      def pendingPayload = (bool(dut.io.phases.splitSavedRob.flag), uint(dut.io.phases.splitSavedRob.value),
        bool(dut.io.phases.splitSavedLq.flag), uint(dut.io.phases.splitSavedLq.value),
        uint(dut.io.phases.splitSavedDestination), uint(dut.io.phases.splitSavedPc),
        uint(dut.io.phases.splitSavedAddress), faults(dut.io.phases.splitSavedException))
      if (trial.contendWriteback && cycle != cancelCycle) {
        if (cancelCycle >= 0 && cycle > cancelCycle && bool(dut.io.phases.splitExceptionPending)) {
          assert(uint(dut.io.phases.splitSavedRob.value) != primaryRob,
            "Canceled exceptional owner remained pending after redirect")
        }
        heldException.foreach { previous =>
          assert(bool(dut.io.phases.splitExceptionPending), "Pending MAB exception disappeared without fire or redirect")
          assert(pendingPayload == previous, "Pending MAB exception changed owner or payload")
        }
        if (bool(dut.io.phases.splitExceptionPending)) {
          assert(pendingPayload == (false, BigInt(primaryRob), false, BigInt(primaryRob),
            BigInt(primaryRob + 1), BigInt(0x4000 + 4 * primaryRob), trial.effective, Set(13)),
            "MAB did not retain the original exceptional owner")
          heldException = Some(pendingPayload)
          if (bool(dut.io.phases.normalWriteback.valid) && !bool(dut.io.phases.splitWriteback.valid)) {
            val normalRob = uint(dut.io.phases.normalWriteback.bits).toInt
            assert(backgroundOwners.contains(normalRob), "Exception contention did not come from an ordinary background load")
            dut.io.phases.splitWritebackReady.expect(false.B)
            contentionCycles += 1
            event("exception-writeback-contention", s"\"normal_rob\":$normalRob,\"mab_rob\":$primaryRob")
            if (trial.cancelAt == "writeback" && cancelCycle < 0) cancelCycle = cycle + 1
          }
        }
      }
      if (bool(dut.io.phases.splitWriteback.valid)) {
        val rob = uint(dut.io.phases.splitWriteback.bits.uop.robIdx.value).toInt
        assert(!(canceled && rob == primaryRob), "Canceled MAB owner published a result")
        splitCompletions(rob) += 1
        heldException = None
        event("mab-writeback-fire", s"\"rob\":$rob")
      }
      val wb = dut.io.result
      if (bool(wb.valid)) {
        val rob = uint(wb.bits.uop.robIdx.value).toInt
        assert(issued(rob), "Writeback has no accepted source")
        assert(!(canceled && rob == primaryRob), "Canceled source published a writeback")
        assert(!(cycle > cancelCycle && cancelCycle >= 0 && killedOwners(rob)),
          "Killed source published a result after redirect")
        val redirectCycleResult = trial.cancelAt == "writeback" && cycle == cancelCycle && killedOwners(rob)
        if (redirectCycleResult) {
          // The native scalar interface may still expose the current S3 result
          // during redirect; that sample belongs to the canceled transaction.
          event("redirect-cycle-writeback", s"\"rob\":$rob")
        } else {
          val expected = if (rob == primaryRob) trial.expectedCause else None
          val actual = faults(wb.bits.uop.exceptionVec)
          expected match {
            case Some(cause) =>
              assert(actual(cause), s"${trial.name}: missing required standard/FDI cause $cause in $actual")
              val permittedCandidates = Set(cause) ++ (if (trial.fdiDenied) Set(24) else Set.empty[Int])
              assert((actual -- permittedCandidates).isEmpty, s"Unexpected exception candidates: $actual")
              wb.bits.uop.rfWen.expect(false.B); wb.bits.uop.fpWen.expect(false.B)
              if (cause == 24) {
                wb.bits.uop.fdiException.get.tval.expect(trial.effective.U)
                wb.bits.uop.fdiException.get.reason.expect(2.U)
              }
            case None =>
              assert(actual.isEmpty, s"Allowed source produced exceptions $actual")
              wb.bits.data.expect(trial.value(ownerAddress(rob)).U)
              wb.bits.uop.rfWen.expect((!trial.fp).B); wb.bits.uop.fpWen.expect(trial.fp.B)
          }
          if (bool(wb.ready)) {
            assert(completed.add(rob), "Duplicate accepted load result")
            val expected = if (canceled) completedBeforeRedirect + nextRob
              else Set(primaryRob) ++ (if (trial.concurrent) Set(nextRob) else Set.empty[Int]) ++ backgroundOwners ++
                (if (trial.requireNextQueueReplay) Set(warmOwner) else Set.empty[Int])
            if (completed.toSet == expected) finalCycle = cycle
            event("writeback", s"\"rob\":$rob,\"data\":${uint(wb.bits.data)},\"exceptions\":[${actual.toSeq.sorted.mkString(",")}]")
          }
        }
      }
      dut.clock.step(); cycle += 1
    }

    if (trial.highNoncache) {
      event("noncache-summary", s"\"high_fragments\":$noncacheFragmentResponses,\"exception_rows\":$noncacheTerminalRows,\"uncache_enqueues\":$uncacheEnqueues,\"uncache_requests\":$uncacheRequests,\"peak_allocated\":$uncachePeakAllocated")
      assert(noncacheFragmentResponses > 0, "No real resolved PBMT-NC high fragment was observed")
    }
    val expectedRob = if (canceled) nextRob else primaryRob
    val expectedOwners = if (canceled) completedBeforeRedirect + nextRob
      else Set(primaryRob) ++ (if (trial.concurrent) Set(nextRob) else Set.empty[Int]) ++ backgroundOwners ++
                (if (trial.requireNextQueueReplay) Set(warmOwner) else Set.empty[Int])
    assert(completed == expectedOwners, s"${trial.name}: incomplete sources $completed")
    expectedOwners.foreach { rob =>
      assert(terminalRows(rob) == 1, s"Expected one terminal LSQ record for $rob, saw ${terminalRows(rob)}")
    }
    dut.io.phases.splitFull.expect(false.B)
    dut.io.phases.replayAllocated.expect(0.U)
    dut.io.result.valid.expect(false.B)
    if (trial.translated && trial.crossesPage && !trial.highCanonicalFault) {
      assert(requestedVpns(primaryHighVpn), "High page never reached real translation")
    }
    if (trial.highCanonicalFault) {
      assert(canonicalHighDataReplies == 0, "Noncanonical high fragment consumed cache data")
      val highAttempts = attempts.count(attempt => attempt.rob == primaryRob && vpnOf(attempt.address) == primaryHighVpn)
      assert(canonicalHighKills == highAttempts, "A noncanonical high-fragment lookup escaped matched S1 cancellation")
    }
    if (trial.requireNextQueueReplay) {
      assert(nextMisalignNackSeen && nextQueueReplaySeen && nextReplayEnqueueSeen,
        "MAB-full to ordinary replay to negative-owner MAB capture was not exercised")
      assert(requestedVpns(nextHighVpn), "Negative owner's high page never reached translation")
    }
    if (internalCancellation) {
      assert(cancellationRequestCycle >= 0 && cancelCycle == cancellationRequestCycle + 2,
        "Internal cancellation was not bound to the accepted request's S2 cycle")
      val expectedRequests = if (trial.cancelAt == "terminal") 1 else 0
      assert(terminalRequests == expectedRequests && terminalAcks == 0 && terminalLsqRows == 0 &&
        terminalRows(primaryRob) == 0, "Canceled internal owner left a terminal completion")
      assert(uncacheEnqueues == 0 && uncacheRequests == 0 && uncachePeakAllocated == 0,
        "Canceled internal owner escaped to the uncached data path")
      if (trial.cancelAt == "probe") assert(latePtwResponses > 0,
        "Probe cancellation did not overlap a real late PTW response")
    } else if (expectsTerminal && !canceled) {
      assert(terminalRequests == 1 && terminalAcks == 1 && terminalLsqRows == 1,
        "Selected MAB exception did not produce one terminal request, LSQ record and acknowledgement")
    } else {
      assert(terminalRequests == 0 && terminalAcks == 0 && terminalLsqRows == 0,
        "Ordinary access unexpectedly used a terminal transaction")
    }
    if (trial.expectedCause.nonEmpty && !canceled) {
      dut.io.phases.exceptionValid.expect(true.B)
      dut.io.phases.exceptionOwner.expect(primaryRob.U)
      dut.io.phases.exceptionAddress.expect(trial.expectedAddress.U)
    }
    if (trial.highNoncache) {
      assert(noncacheTerminalRows == (if (canceled) 0 else 1),
        "Noncache misalignment published the wrong number of same-owner standard exception rows")
      assert(uncacheEnqueues == 0 && uncacheRequests == 0 && uncachePeakAllocated == 0,
        "Noncache misalignment allocated or issued target-data work")
    }
    if (trial.contendWriteback) {
      assert(contentionStreamStarted && contentionCycles > 0, "No real ordinary-load/MAB exception contention occurred")
      if (!canceled) {
        assert(backgroundOwners.forall(completed), "The ordinary contention stream did not drain")
        assert(splitCompletions(primaryRob) == 1, "Exceptional MAB owner did not complete exactly once")
      } else {
        assert(splitCompletions(primaryRob) == 0 && splitCompletions(nextRob) == 1,
          "Canceled exception or replacement owner violated MAB completion ownership")
      }
    }
    if (canceled) {
      if (bool(dut.io.phases.exceptionValid)) {
        assert(uint(dut.io.phases.exceptionOwner) != primaryRob, "Canceled owner survived in the exception buffer")
      }
      assert(cancelCycle >= 0 && (lateResponses > 0 || trial.cancelAt == "writeback" || trial.cancelAt == "terminal"),
        "Cancellation never overlapped a late response or a pending exception")
      if (trial.cancelAt != "low") assert(highRequests > 0)
      if (trial.cancelAt == "wakeup") assert(wakeupRequests == 1)
      assert(injectedNacks("low") == trial.nackLow && injectedNacks("high") == trial.nackHigh,
        "Canceled cache response did not exercise the requested late nack")
    }
    if (!canceled) {
      assert(injectedNacks("low") == trial.nackLow && injectedNacks("high") == trial.nackHigh,
        s"Requested cache retry was not exercised: $injectedNacks")
      if (trial.nackLow > 0) assert(fragmentCacheAttempts("low") > trial.nackLow)
      if (trial.nackHigh > 0) assert(fragmentCacheAttempts("high") > trial.nackHigh)
    }
    if (trial.guestDataContext) {
      assert(guestTlbRequests > 0 && guestTlbResponses == guestTlbRequests && guestSplitRequests > 0,
        "Guest data-context preservation did not reach the real translation and MAB paths")
    }
    if (trial.evictLowTranslation) assert(lowTranslationEvicted && fragmentTlbRetries("low") > 0,
      "Low fragment never retried a real TLB miss after eviction")
    if (trial.highPtwDelay > 10) assert(fragmentTlbRetries("high") > 1,
      "High fragment did not retain its owner across delayed translation")
    if (trial.stallFragment) assert(splitStallCycles >= 3, "Fragment request backpressure was not exercised")
    if (trial.concurrent) assert(fullReplayRows > 0, "Second owner never encountered a full MAB")
    event("complete", s"\"cycles\":$cycle,\"terminal_rows\":${terminalRows(expectedRob)},\"primary_queue_replays\":$primaryQueueReplays")
    println(s"L04_CASE_PASS name=${trial.name} enabled=$enabled cycles=$cycle")
  }

  it should "preserve whole-access permission and complete the original owner exactly once" in {
    val root = Paths.get(sys.props.getOrElse("l04.runRoot", ".")).toAbsolutePath
    Files.createDirectories(root)
    require(Paths.get("").toRealPath() == root.toRealPath(), "Run from the dedicated misaligned-load test directory")
    val (base, _, _) = top.ArgParser.parse(Array("--config", "FpgaDefaultConfig", "--num-cores", "1",
      "--l2-cache-size", "256", "--l3-cache-size", "768", "--fpga-platform",
      "--disable-always-basic-diff", "--disable-perf", "--disable-alwaysdb"))
    implicit val p: Parameters = base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasFDI = enabled)
    }
    require(p(XSCoreParamsKey).HasVPU && p(XSCoreParamsKey).VLEN == 128)
    require(!p(XSCoreParamsKey).EnableLoadToLoadForward)
    val options = p(DebugOptionsKey)
    utility.Constantin.init(options.EnableConstantin && !options.FPGAPlatform)
    utility.ChiselDB.init(options.EnableChiselDB && !options.FPGAPlatform)
    // Exercise the uncached regression before the remaining independently reset cases.
    val completeMatrix = allCases.filter(_.highNoncache) ++ allCases.filterNot(_.highNoncache)
    val selected = if (scenario == "all") completeMatrix else groups.getOrElse(scenario, selectableCases.filter(_.name == scenario))
    val events = mutable.ArrayBuffer.empty[String]
    try {
      test(new FDILoadMisalignHarness).withAnnotations(Seq(VerilatorBackendAnnotation,
        TargetDirAnnotation(s"load-misalign-$scenario-${if (enabled) "on" else "off"}"))) { dut =>
        selected.foreach(trial => run(dut, trial, events))
      }
      println(s"L04_MISALIGN_PASS enabled=$enabled scenario=$scenario cases=${selected.size}")
    } finally {
      Files.write(root.resolve("load-misalign-events.jsonl"), events.mkString("", "\n", "\n").getBytes(StandardCharsets.UTF_8))
    }
  }
}
