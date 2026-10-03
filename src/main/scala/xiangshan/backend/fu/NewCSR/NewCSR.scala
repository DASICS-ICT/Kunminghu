package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import difftest._
import freechips.rocketchip.rocket.CSRs
import org.chipsalliance.cde.config.Parameters
import top.{ArgParser, Generator}
import utility._
import utils.OptionWrapper
import xiangshan.backend.fu.NewCSR.CSRBundles.{CSRCustomState, PrivState, RobCommitCSR}
import xiangshan.backend.fu.NewCSR.CSRDefines._
import xiangshan.backend.fu.NewCSR.CSREnumTypeImplicitCast._
import xiangshan.backend.fu.NewCSR.CSREvents.{CSREvents, DretEventSinkBundle, EventUpdatePrivStateOutput, MNretEventSinkBundle, MretEventSinkBundle, SretEventSinkBundle, SretEventSDTSinkBundle,  TargetPCBundle, TrapEntryDEventSinkBundle, TrapEntryEventInput, TrapEntryHSEventSinkBundle, TrapEntryMEventSinkBundle, TrapEntryMNEventSinkBundle, TrapEntryVSEventSinkBundle}
import xiangshan.backend.fu.fpu.Bundles.Frm
import xiangshan.backend.fu.vector.Bundles.{Vl, Vstart, Vxrm, Vxsat}
import xiangshan.backend.fu.wrapper.CSRToDecode
import xiangshan.backend.rob.RobPtr
import xiangshan._
import xiangshan.backend.fu.PerfCounterIO
import xiangshan.backend.fu.util.CSRConst
import xiangshan.ExceptionNO._
import xiangshan.backend.trace._

import scala.collection.immutable.SeqMap

object CSRConfig {
  final val ASIDLEN = 16 // the length of ASID of XS implementation

  final val ASIDMAX = 16 // the max value of ASIDLEN defined by spec

  final val HIIDWidth = 12 // support Hvictl[27:16](IID)

  final val VMIDLEN = 14 // the length of VMID of XS implementation

  final val VMIDMAX = 14 // the max value of VMIDLEN defined by spec

  final val VaddrMaxWidth = 48 + 2 // support Sv39/Sv48/Sv39x4/Sv48x4

  final val InstWidth = 32

  final val XLEN = 64 // Todo: use XSParams

  final val VLEN = 128

  // Since we need macro to compute the width of CSR field, the input of macro should be the value that can be computed
  // at compile time. The log2Up function cannot be used as meta-programming function, so we use litral value here
  // log2Up(128 + 1), hold 0~128
  final val VlWidth = 8

  final val PAddrWidth = 48

  final val AddrWidthInPage = 12

  final val PMPAddrWidth = 48

  final val PMPOffBits = 2

  final val PMPAddrBits = PMPAddrWidth - PMPOffBits

  // perf
  final val perfCntNum = 29       // in Spec

  final val EXT_SSTC = true

  final val EXT_DBLTRP = true

  final val PPNLength = 44
  // TODO: as current test not support clean mdt , we set mstatus->mdt = 0 to allow exception in m-mode
  final val mdtInit = 0

}

class NewCSRInput(implicit p: Parameters) extends Bundle {
  val wen = Bool()
  val ren = Bool()
  val op = UInt(2.W)
  val addr = UInt(12.W)
  val src = UInt(64.W)
  val wdata = UInt(64.W)
  // Full start PC of the accepted CSR instruction, not an exception or fetch PC.
  val sourcePc = UInt(64.W)
  val mnret = Input(Bool())
  val mret = Input(Bool())
  val sret = Input(Bool())
  val dret = Input(Bool())
  val uret = Input(Bool())
  val redirectFlush = Input(Bool())
}

class NewCSROutput(implicit p: Parameters) extends Bundle {
  val EX_II = Bool()
  val EX_VI = Bool()
  val flushPipe = Bool()
  val rData = UInt(64.W)
  val targetPcUpdate = Bool()
  val targetPc = new TargetPCBundle
  val userReturnTarget = new TargetPCBundle
  val userReturnRedirect = Bool()
  val regOut = UInt(64.W)
  // perf
  val isPerfCnt = Bool()
}

class NewCSR(implicit val p: Parameters) extends Module
  with HasXSParameter
  with MachineLevel
  with SupervisorLevel
  with HypervisorLevel
  with VirtualSupervisorLevel
  with Unprivileged
  with UserTimerCSRs
  with CSRAIA
  with HasExternalInterruptBundle
  with HasNonMaskableIRPBundle
  with CSREvents
  with DebugLevel
  with CSRCustom
  with CSRPMP
  with CSRPMA
  with HasCriticalErrors
  with IpIeAliasConnect
{

  import CSRConfig._
  import xiangshan.backend.fu.NewCSR.CSREvents.{HUEntryPort, HUEntryOutcome, InterruptDescriptor, InterruptEventIdentity, UserTrapEventInput, TrapEntryHUEventSink, UretEventSink}

  val io = IO(new Bundle {
    val fromTop = Input(new Bundle {
      val hartId = UInt(hartIdLen.W)
      val clintTime = Input(ValidIO(UInt(64.W)))
      val l2FlushDone = Input(Bool())
      val criticalErrorState = Input(Bool())
    })
    val in = Flipped(DecoupledIO(new NewCSRInput))
    val huEntry = Option.when(HasFDI)(new HUEntryPort)
    val interruptCandidate = Option.when(HasFDI)(Output(Valid(new InterruptDescriptor)))
    val huCandidateKill = Option.when(HasFDI)(Output(Bool()))
    val acceptedInterrupt = Option.when(HasFDI)(Input(Valid(new InterruptEventIdentity)))
    val trapInst = Input(ValidIO(UInt(InstWidth.W)))
    val fromMem = Input(new Bundle {
      val excpVA  = UInt(XLEN.W)
      val excpGPA = UInt(XLEN.W)
      val excpIsForVSnonLeafPTE = Bool()
    })
    val fromRob = Input(new Bundle {
      val trap = ValidIO(new Bundle {
        val pc = UInt(VaddrMaxWidth.W)
        val pcGPA = UInt(PAddrBitsMax.W)
        val instr = UInt(InstWidth.W)
        val trapVec = UInt(64.W)
        val fdiException = Option.when(HasFDI)(new xiangshan.backend.FDIExceptionRecord)
        val isFetchBkpt = Bool()
        val singleStep = Bool()
        val trigger = TriggerAction()
        val crossPageIPFFix = Bool()
        val isInterrupt = Bool()
        val isHls = Bool()
        val isFetchMalAddr = Bool()
        val isForVSnonLeafPTE = Bool()
        val interruptEvent = Option.when(HasFDI)(new InterruptEventIdentity)
      })
      val commit = Input(new RobCommitCSR)
      val robDeqPtr = Input(new RobPtr)
    })

    val fromVecExcpMod = Input(new Bundle {
      val busy = Bool()
    })

    val perf = Input(new PerfCounterIO)

    /** Output should be a DecoupledIO, since now CSR writing to integer register file might be blocked (by arbiter) */
    val out = DecoupledIO(new NewCSROutput)
    val status = Output(new Bundle {
      val userInHandler = Option.when(HasFDI)(Bool())
      val userEntryEffect = Option.when(HasFDI)(Bool())
      val userReturnEffect = Option.when(HasFDI)(Bool())
      val userTargetIdentity = Option.when(HasFDI)(Valid(new InterruptEventIdentity))
      val privState = new PrivState
      val interrupt = Bool()
      val wfiEvent = Bool()
      // fp
      val fpState = new Bundle {
        val off = Bool()
        val frm = Frm()
      }
      // vec
      val vecState = new Bundle {
        val vstart = Vstart()
        val vxsat = Vxsat()
        val vxrm = Vxrm()
        val vcsr = UInt(XLEN.W)
        val vl = Vl()
        val vtype = UInt(XLEN.W)
        val vlenb = UInt(XLEN.W)
        val off = Bool()
      }
      // debug
      val debugMode = Bool()
      val singleStepFlag = Bool()
      // trigger
      val frontendTrigger = new FrontendTdataDistributeIO()
      val memTrigger = new MemTdataDistributeIO()
      // Instruction fetch address translation type
      val instrAddrTransType = new AddrTransType
      // trace
      val traceCSR = Output(new TraceCSR)
      // custom
      val custom = new CSRCustomState
      val criticalErrorState = Bool()
    })
    // tlb
    val tlb = Output(new Bundle {
      val satpASIDChanged = Bool()
      val vsatpASIDChanged = Bool()
      val hgatpVMIDChanged = Bool()
      val satp = new SatpBundle
      val vsatp = new SatpBundle
      val hgatp = new HgatpBundle
      val mbmc = new MbmcBundle
      val mxr = Bool()
      val sum = Bool()
      val vmxr = Bool()
      val vsum = Bool()
      val spvp = Bool()
      val imode = UInt(2.W)
      val dmode = UInt(2.W)
      val dvirt = Bool()
      val mPBMTE = Bool()
      val hPBMTE = Bool()
      val pmm = new Bundle {
        val mseccfg = UInt(2.W)
        val menvcfg = UInt(2.W)
        val henvcfg = UInt(2.W)
        val hstatus = UInt(2.W)
        val senvcfg = UInt(2.W)
      }
    })

    val toDecode = new CSRToDecode

    val fetchMalTval = Input(UInt(XLEN.W))

    val distributedWenLegal = Output(Bool())
    val distributedFDI = Option.when(HasFDI)(Output(new DistributedCSRIO))
  })

  val toAIA   = IO(Output(new CSRToAIABundle))
  val fromAIA = IO(Flipped(Output(new AIAToCSRBundle)))

  dontTouch(toAIA)
  dontTouch(fromAIA)
  dontTouch(io.fromTop.clintTime)

  /* Alias of input valid/ready */
  // The HU slot excludes software execution, including legacy live-valid consumers.
  private val huBusy = if (HasFDI) Some(RegInit(false.B)) else None
  // The reservation survives PC transport and terminal backpressure. Only the
  // controller's release, after redirect ownership ends, reopens software issue.
  private val huPcValid = Option.when(HasFDI)(RegInit(false.B))
  private val huFinished = Option.when(HasFDI)(RegInit(false.B))
  private val huCanceled = Option.when(HasFDI)(RegInit(false.B))
  private val huDebugPending = Option.when(HasFDI)(RegInit(false.B))
  private val huTrapFinished = Option.when(HasFDI)(RegInit(false.B))
  private val huSaved = Option.when(HasFDI)(Reg(new UserTrapEventInput))
  private val huIdentity = Option.when(HasFDI)(Reg(new InterruptEventIdentity))
  private val huTrapTarget = Option.when(HasFDI)(Reg(new TargetPCBundle))
  // A critical error observed after a terminal effect needs a new precise ROB
  // boundary; it must not reuse the completed HU transaction's recovery PC.
  private val deferredCriticalDebug = Option.when(HasFDI)(RegInit(false.B))
  private val criticalDebugInFlight = Option.when(HasFDI)(RegInit(false.B))
  private val huEligible = WireDefault(false.B)
  private val huCandidateInvalidate = WireDefault(false.B)
  private val huClaim = WireDefault(false.B)
  private val huTakeDebug = WireDefault(false.B)
  private val huCancelNow = WireDefault(false.B)
  private val userReturnPending = if (HasFDI) Some(RegInit(false.B)) else None
  private val userReturnLegal = if (HasFDI) Some(RegInit(false.B)) else None
  private val userEventCancel = WireDefault(false.B)
  private val softwareSlotReady = Wire(Bool())
  val valid = if (HasFDI) io.in.valid && softwareSlotReady else io.in.valid

  /* Alias of input signals */
  val wen   = io.in.bits.wen && valid
  val addr  = io.in.bits.addr
  val wdata = io.in.bits.wdata

  val ren   = io.in.bits.ren && valid
  val raddr = io.in.bits.addr

  // flush
  val redirectFlush = io.in.bits.redirectFlush

  private val deliveredInterrupt = io.fromRob.trap.bits.interruptEvent.map(_.interrupt)
  private val hasHURequest = io.fromRob.trap.valid && io.fromRob.trap.bits.isInterrupt &&
    deliveredInterrupt.map(_.irToHU).getOrElse(false.B)
  // An accepted HU descriptor owns a separate transaction and is never also an
  // ordinary trap, even while its exact PC uses the common exception transport.
  val hasTrap = io.fromRob.trap.valid && !hasHURequest
  val trapVec = io.fromRob.trap.bits.trapVec
  val trapPC = io.fromRob.trap.bits.pc
  val trapPCGPA = io.fromRob.trap.bits.pcGPA
  val trapIsInterrupt = io.fromRob.trap.bits.isInterrupt
  val trapIsCrossPageIPF = io.fromRob.trap.bits.crossPageIPFFix
  val trigger = io.fromRob.trap.bits.trigger
  val singleStep = io.fromRob.trap.bits.singleStep
  val trapIsHls = io.fromRob.trap.bits.isHls
  val trapIsFetchMalAddr = io.fromRob.trap.bits.isFetchMalAddr
  val trapIsFetchBkpt = io.fromRob.trap.bits.isFetchBkpt
  val trapIsForVSnonLeafPTE = io.fromRob.trap.bits.isForVSnonLeafPTE

  // debug_intrrupt
  val debugIntrEnable = RegInit(true.B) // debug interrupt will be handle only when debugIntrEnable
  val debugIntr = platformIRP.debugIP && debugIntrEnable

  // CSR Privilege State
  val PRVM = RegInit(PrivMode(1, 0), PrivMode.M)
  val V = RegInit(VirtMode(0), VirtMode.Off)
  val debugMode = RegInit(false.B)
  private val nextV = WireInit(VirtMode(0), VirtMode.Off)
  V := nextV
  // dcsr stopcount 
  val debugModeStopCountNext = debugMode && dcsr.regOut.STOPCOUNT
  val debugModeStopTimeNext  = debugMode && dcsr.regOut.STOPTIME
  val debugModeStopCount = RegNext(debugModeStopCountNext)
  val unprivCountUpdate  = !debugModeStopCount && debugModeStopCountNext

  val criticalErrorStateInCSR = Wire(Bool())
  val criticalErrorState = RegEnable(true.B, false.B, io.fromTop.criticalErrorState || criticalErrorStateInCSR)

  private val privState = Wire(new PrivState)
  privState.PRVM := PRVM
  privState.V := V

  private val isModeM              = privState.isModeM
  private val (isModeHS, isModeHU) = (privState.isModeHS, privState.isModeHU)
  private val (isModeVS, isModeVU) = (privState.isModeVS, privState.isModeVU)

  userTimer.foreach { timer =>
    // Entry/return change eligibility only after their effect edge; masks do not freeze counting.
    timer.io.tickEnable := isModeHU && !debugMode && !userInHandler.get
  }

  val permitMod = Module(new CSRPermitModule)
  val sstcIRGen = Module(new SstcInterruptGen)
  val commidIdMod = Module(new CommitIDModule(40))

  val gitCommitSHA = WireInit(commidIdMod.io.commitID)
  val gitDirty     = WireInit(commidIdMod.io.dirty)
  dontTouch(gitCommitSHA)
  dontTouch(gitDirty)

  private val wenLegal = permitMod.io.out.hasLegalWen

  val legalSret  = permitMod.io.out.hasLegalSret
  val legalMret  = permitMod.io.out.hasLegalMret
  val legalMNret = permitMod.io.out.hasLegalMNret
  val legalDret  = permitMod.io.out.hasLegalDret

  private val userTimerAddressHit = if (HasFDI) {
    UserTimerCSRAddress.all.map(address => addr === address.U).reduce(_ || _)
  } else false.B
  // Bank requests have their own accepted write selection. Do not dispatch their delayed
  // write pulse using a subsequent live address through any legacy CSR consumer.
  private val fdiMainCfg = Option.when(HasFDI)(withReset(reset.asBool)(new FDIMainCfgBank))
  private val fdiBounds = Option.when(HasFDI)(withReset(reset.asBool)(new FDIBoundRegisterBank))
  private val fdiSpecial = Option.when(HasFDI)(withReset(reset.asBool)(new FDISpecialRegisterBank))
  private val fdiRwEntries = fdiMainCfg.toSeq.flatMap(_.csrRwMap.toSeq) ++
    fdiBounds.toSeq.flatMap(_.csrRwMap.toSeq) ++ fdiSpecial.toSeq.flatMap(_.csrRwMap.toSeq)
  private val fdiOutEntries = fdiMainCfg.toSeq.flatMap(_.csrOutMap.toSeq) ++
    fdiBounds.toSeq.flatMap(_.csrOutMap.toSeq) ++ fdiSpecial.toSeq.flatMap(_.csrOutMap.toSeq)
  require(fdiRwEntries.map(_._1).distinct.size == fdiRwEntries.size,
    "FDI CSR groups must have unique address views")
  require(fdiOutEntries.map(_._1) == fdiRwEntries.map(_._1))
  private val fdiCSRMap = SeqMap.from(fdiRwEntries)
  private val fdiCSROutMap = SeqMap.from(fdiOutEntries)
  private val fdiAddressHit = fdiCSRMap.keys.map(address => addr === address.U).foldLeft(false.B)(_ || _)
  private val wenLegalReg = GatedValidRegNext(wenLegal && !userTimerAddressHit && !fdiAddressHit)

  var csrRwMap: SeqMap[Int, (CSRAddrWriteBundle[_], UInt)] =
    machineLevelCSRMap ++
    supervisorLevelCSRMap ++
    hypervisorCSRMap ++
    virtualSupervisorCSRMap ++
    unprivilegedCSRMap ++
    debugCSRMap ++
    aiaCSRMap ++
    customCSRMap ++
    pmpCSRMap ++
    pmaCSRMap

  require((csrRwMap.keySet intersect userTimerCSRMap.keySet).isEmpty,
    "User timer CSR addresses overlap an implemented CSR")
  csrRwMap ++= userTimerCSRMap
  require((csrRwMap.keySet intersect fdiCSRMap.keySet).isEmpty,
    "FDI CSR addresses overlap an implemented CSR")
  csrRwMap ++= fdiCSRMap

  val csrMods: Seq[CSRModule[_]] =
    machineLevelCSRMods ++
    supervisorLevelCSRMods ++
    hypervisorCSRMods ++
    virtualSupervisorCSRMods ++
    unprivilegedCSRMods ++
    debugCSRMods ++
    aiaCSRMods ++
    customCSRMods ++
    pmpCSRMods ++
    pmaCSRMods ++
    userTimerCSRMods ++
    fdiMainCfg.toSeq.flatMap(_.csrMods) ++
    fdiBounds.toSeq.flatMap(_.csrMods) ++
    fdiSpecial.toSeq.flatMap(_.csrMods)

  var csrOutMap: SeqMap[Int, UInt] =
    machineLevelCSROutMap ++
    supervisorLevelCSROutMap ++
    hypervisorCSROutMap ++
    virtualSupervisorCSROutMap ++
    unprivilegedCSROutMap ++
    debugCSROutMap ++
    aiaCSROutMap ++
    customCSROutMap ++
    pmpCSROutMap ++
    pmaCSROutMap

  require((csrOutMap.keySet intersect userTimerCSROutMap.keySet).isEmpty,
    "User timer CSR addresses overlap an implemented CSR readout")
  csrOutMap ++= userTimerCSROutMap
  require((csrOutMap.keySet intersect fdiCSROutMap.keySet).isEmpty,
    "FDI CSR addresses overlap an implemented CSR readout")
  csrOutMap ++= fdiCSROutMap

  // interrupt
  val nmip = RegInit(new NonMaskableIRPendingBundle, (new NonMaskableIRPendingBundle).init)
  private val nmiInFlight = Option.when(HasFDI)(RegInit(false.B))
  private val nmiInFlightCause = Option.when(HasFDI)(Reg(UInt(8.W)))
  private val nmiClaim = WireDefault(false.B)
  private val nmiClaimCause = WireDefault(0.U(8.W))
  if (HasFDI) {
    val accepted = io.acceptedInterrupt.get
    nmiClaim := accepted.valid && accepted.bits.interrupt.nmi && !accepted.bits.interrupt.debug &&
      !accepted.bits.interrupt.irToHU
    nmiClaimCause := accepted.bits.interrupt.cause
    when(nmiClaim) {
      nmiInFlight.get := true.B
      nmiInFlightCause.get := nmiClaimCause
    }.elsewhen(hasTrap && trapIsInterrupt && deliveredInterrupt.get.nmi &&
      deliveredInterrupt.get.cause === nmiInFlightCause.get) {
      nmiInFlight.get := false.B
    }
    // Pending belongs to the source until its accepted descriptor actually enters
    // MN. A coincident new assertion remains pending after that acknowledgement.
    val nmiEffect = trapEntryMNEvent.valid && hasTrap && trapIsInterrupt &&
      deliveredInterrupt.get.nmi && !deliveredInterrupt.get.debug && !deliveredInterrupt.get.irToHU
    when(nonMaskableIRP.NMI_43) {
      nmip.NMI_43 := true.B
    }.elsewhen(nmiEffect && deliveredInterrupt.get.cause === NonMaskableIRNO.NMI_43.U) {
      nmip.NMI_43 := false.B
    }
    when(nonMaskableIRP.NMI_31) {
      nmip.NMI_31 := true.B
    }.elsewhen(nmiEffect && deliveredInterrupt.get.cause === NonMaskableIRNO.NMI_31.U) {
      nmip.NMI_31 := false.B
    }
  } else {
    when(nonMaskableIRP.NMI_43) { nmip.NMI_43 := true.B }
    when(nonMaskableIRP.NMI_31) { nmip.NMI_31 := true.B }
  }

  val intrMod = Module(new InterruptFilter(HasFDI))
  if (HasFDI) {
    intrMod.io.in.huCandidate.get := huEligible
    intrMod.io.in.huCandidateKill.get := huCandidateInvalidate
    intrMod.io.in.huClaim.get := huClaim
    intrMod.io.in.nmiInFlight.get.valid := nmiInFlight.get
    intrMod.io.in.nmiInFlight.get.bits := nmiInFlightCause.get
    intrMod.io.in.nmiClaim.get.valid := nmiClaim
    intrMod.io.in.nmiClaim.get.bits := nmiClaimCause
    intrMod.io.in.criticalDebug.get := deferredCriticalDebug.get
    val criticalClaim = io.acceptedInterrupt.get.valid && io.acceptedInterrupt.get.bits.interrupt.criticalDebug
    intrMod.io.in.criticalDebugInFlight.get := criticalDebugInFlight.get
    intrMod.io.in.criticalDebugClaim.get := criticalClaim
    when(criticalClaim) {
      criticalDebugInFlight.get := true.B
    }.elsewhen(hasTrap && trapIsInterrupt && deliveredInterrupt.get.criticalDebug) {
      criticalDebugInFlight.get := false.B
    }
    io.interruptCandidate.get := intrMod.io.out.candidate.get
    io.huCandidateKill.get := huCandidateInvalidate
  }
  intrMod.io.in.privState := privState
  intrMod.io.in.mstatusMIE := mstatus.regOut.MIE.asBool
  intrMod.io.in.sstatusSIE := mstatus.regOut.SIE.asBool
  intrMod.io.in.vsstatusSIE := vsstatus.regOut.SIE.asBool
  intrMod.io.in.mip := mip.rdataFields
  intrMod.io.in.mie := mie.regOut
  intrMod.io.in.mideleg := mideleg.regOut
  intrMod.io.in.sip := sip.regOut
  intrMod.io.in.sie := sie.regOut
  intrMod.io.in.hip := hip.regOut
  intrMod.io.in.hie := hie.regOut
  intrMod.io.in.hideleg := hideleg.regOut
  intrMod.io.in.vsip := vsip.regOut
  intrMod.io.in.vsie := vsie.regOut
  intrMod.io.in.hvictl := hvictl.regOut
  intrMod.io.in.hstatus := hstatus.regOut
  intrMod.io.in.mtopei := mtopei.regOut
  intrMod.io.in.stopei := stopei.regOut
  intrMod.io.in.vstopei := vstopei.regOut
  intrMod.io.in.hviprio1 := hviprio1.regOut
  intrMod.io.in.hviprio2 := hviprio2.regOut
  intrMod.io.in.miprios := Cat(miregiprios.map(_.rdata).reverse)
  intrMod.io.in.hsiprios := Cat(siregiprios.map(_.rdata).reverse)
  intrMod.io.in.mnstatusNMIE := mnstatus.regOut.NMIE.asBool
  val selectableNmi = if (HasFDI) {
    nmip.asUInt & ~Mux(nmiInFlight.get, UIntToOH(nmiInFlightCause.get, 64), 0.U(64.W))
  } else nmip.asUInt
  intrMod.io.in.nmi := selectableNmi.orR
  intrMod.io.in.nmiVec := selectableNmi
  intrMod.io.in.debugMode := debugMode
  intrMod.io.in.debugIntr := debugIntr || (deferredCriticalDebug.getOrElse(false.B) &&
    !criticalDebugInFlight.getOrElse(false.B) && criticalErrorState && dcsr.regOut.CETRIG.asBool)
  intrMod.io.in.dcsr      := dcsr.regOut
  intrMod.io.in.platform.meip := platformIRP.MEIP
  intrMod.io.in.platform.seip := platformIRP.SEIP
  intrMod.io.in.fromAIA.meip := fromAIA.meip
  intrMod.io.in.fromAIA.seip := fromAIA.seip

  if (!HasFDI) {
    when(intrMod.io.out.nmi && intrMod.io.out.interruptVec.valid) {
      nmip.NMI_31 := nmip.NMI_31 & !UIntToOH(intrMod.io.out.interruptVec.bits, 64)(NonMaskableIRNO.NMI_31)
      nmip.NMI_43 := nmip.NMI_43 & !UIntToOH(intrMod.io.out.interruptVec.bits, 64)(NonMaskableIRNO.NMI_43)
    }
  }
  val legacyCapture = intrMod.io.out.interruptVec.valid &&
    !intrMod.io.out.candidate.map(_.bits.irToHU).getOrElse(false.B)
  val observedIntrVec = RegEnable(intrMod.io.out.interruptVec.bits, 0.U, legacyCapture)
  val observedDebug = RegEnable(intrMod.io.out.debug, false.B, legacyCapture)
  val observedNmi = RegEnable(intrMod.io.out.nmi, false.B, legacyCapture)
  val observedInject = RegEnable(intrMod.io.out.virtualInterruptIsHvictlInject, false.B, legacyCapture)
  val observedToHS = RegEnable(intrMod.io.out.irToHS, false.B, legacyCapture)
  val observedToVS = RegEnable(intrMod.io.out.irToVS, false.B, legacyCapture)
  val useAcceptedInterrupt = HasFDI.B && hasTrap && trapIsInterrupt
  val intrVec = if (HasFDI) Mux(useAcceptedInterrupt, deliveredInterrupt.get.cause, 0.U) else observedIntrVec
  val debug = if (HasFDI) useAcceptedInterrupt && deliveredInterrupt.get.debug else observedDebug
  val nmi = if (HasFDI) useAcceptedInterrupt && deliveredInterrupt.get.nmi else observedNmi
  val virtualInterruptIsHvictlInject = Mux(useAcceptedInterrupt,
    deliveredInterrupt.map(_.virtualInterruptIsHvictlInject).getOrElse(false.B), if (HasFDI) false.B else observedInject)
  val irToHS = Mux(useAcceptedInterrupt, deliveredInterrupt.map(_.irToHS).getOrElse(false.B), if (HasFDI) false.B else observedToHS)
  val irToVS = Mux(useAcceptedInterrupt, deliveredInterrupt.map(_.irToVS).getOrElse(false.B), if (HasFDI) false.B else observedToVS)

  val trapHandleMod = Module(new TrapHandleModule)

  trapHandleMod.io.in.trapInfo.valid := hasTrap
  trapHandleMod.io.in.trapInfo.bits.trapVec := trapVec.asUInt
  trapHandleMod.io.in.trapInfo.bits.nmi := nmi
  trapHandleMod.io.in.trapInfo.bits.intrVec := intrVec
  trapHandleMod.io.in.trapInfo.bits.isInterrupt := trapIsInterrupt
  trapHandleMod.io.in.trapInfo.bits.irToHS := irToHS
  trapHandleMod.io.in.trapInfo.bits.irToVS := irToVS
  trapHandleMod.io.in.privState := privState
  trapHandleMod.io.in.mstatus  := mstatus.regOut
  trapHandleMod.io.in.vsstatus := vsstatus.regOut
  trapHandleMod.io.in.mnstatus := mnstatus.regOut
  trapHandleMod.io.in.mideleg  := mideleg.regOut
  trapHandleMod.io.in.medeleg  := medeleg.regOut
  trapHandleMod.io.in.hideleg  := hideleg.regOut
  trapHandleMod.io.in.hedeleg  := hedeleg.regOut
  trapHandleMod.io.in.mvien := mvien.regOut
  trapHandleMod.io.in.hvien := hvien.regOut
  trapHandleMod.io.in.mtvec := mtvec.regOut
  trapHandleMod.io.in.stvec := stvec.regOut
  trapHandleMod.io.in.vstvec := vstvec.regOut
  trapHandleMod.io.in.virtualInterruptIsHvictlInject := virtualInterruptIsHvictlInject
  trapHandleMod.io.in.trapInfo.bits.singleStep  := hasTrap && !trapIsInterrupt && singleStep

  val entryPrivState = trapHandleMod.io.out.entryPrivState
  val entryDebugMode = WireInit(false.B)
  val dbltrpToMN     = trapHandleMod.io.out.dbltrpToMN
  val hasDTExcp      = trapHandleMod.io.out.hasDTExcp

  // PMP
  val pmpEntryMod = Module(new PMPEntryHandleModule)
  pmpEntryMod.io.in.pmpCfg  := pmpcfgs.map(_.regOut.asInstanceOf[PMPCfgBundle])
  pmpEntryMod.io.in.pmpAddr := pmpaddr.map(_.regOut.asInstanceOf[PMPAddrBundle])
  pmpEntryMod.io.in.ren   := ren
  pmpEntryMod.io.in.wen   := wenLegalReg
  pmpEntryMod.io.in.addr  := addr
  pmpEntryMod.io.in.wdata := wdata

  // PMA
  val pmaEntryMod = Module(new PMAEntryHandleModule)
  pmaEntryMod.io.in.pmaCfg  := pmacfgs.map(_.regOut.asInstanceOf[PMACfgBundle])
  pmaEntryMod.io.in.ren   := ren
  pmaEntryMod.io.in.wen   := wenLegalReg
  pmaEntryMod.io.in.addr  := addr
  pmaEntryMod.io.in.wdata := wdata

  // Todo: all wen and wdata of CSRModule assigned in this for loop
  for ((id, (wBundle, _)) <- csrRwMap) {
    if (HasFDI && (UserTimerCSRAddress.all.contains(id) || fdiCSRMap.contains(id))) {
      // The wrapper captures final RMW data on fire. Keep this address selection aligned
      // with that data for the following write edge, including bubbles and output stalls.
      val acceptedWrite = RegNext(io.in.fire && wenLegal && addr === id.U && !redirectFlush, false.B)
      wBundle.wen := acceptedWrite && !redirectFlush &&
        (if (fdiCSRMap.contains(id)) !reset.asBool else true.B)
      wBundle.wdata := wdata
    } else if (vsMapS.contains(id)) {
      // VS access CSR by S: privState.isModeVS && addrMappedToVS === sMapVS(id).U
      wBundle.wen := wenLegalReg && ((isModeVS && addr === vsMapS(id).U) || (!isModeVS && addr === id.U))
      wBundle.wdata := wdata
    } else if (sMapVS.contains(id)) {
      wBundle.wen := wenLegalReg && !isModeVS && addr === id.U
      wBundle.wdata := wdata
    } else {
      wBundle.wen := wenLegalReg && addr === id.U
      wBundle.wdata := wdata
    }
  }

  private val fdiSoftwareWriteEffect = WireDefault(false.B)

  if (HasFDI) {
    // Normalize once at the accepted write effect. The same final word drives
    // its native owner and the distribution bus; mirrors perform no CSR RMW.
    val owners = fdiMainCfg.get.csrMods ++ fdiBounds.get.csrMods ++ fdiSpecial.get.csrMods
    val ownerWords = owners.map { owner =>
      val fields = Wire(chiselTypeOf(owner.regOut.asInstanceOf[CSRBundle]))
      fields := wdata
      owner.addr -> fields.asUInt
    }.toMap
    val uFields = Wire(new FDIUMainCfgBundle)
    uFields := wdata
    val uFinal = Wire(new FDIMainCfgBundle)
    uFinal := fdiMainCfg.get.mainCfg.regOut
    // No other MainCfg writer can intervene between the accepted request and
    // this effect. Hidden fields come from the sole architectural owner.
    for ((name, field) <- uFields.elements) {
      uFinal.elements(name) := field
    }
    val finalWords = ownerWords + (FDIMainCfgAddress.uMainCfg -> uFinal.asUInt)
    require(finalWords.keySet == fdiCSRMap.keySet)
    val effects = fdiCSRMap.toSeq.map { case (address, (port, _)) => address -> port.wen }
    val finalData = Mux1H(effects.map { case (address, effect) => effect -> finalWords(address) })
    val distribution = io.distributedFDI.get.w
    fdiSoftwareWriteEffect := effects.map(_._2).reduce(_ || _)
    distribution.valid := fdiSoftwareWriteEffect
    distribution.bits.addr := Mux1H(effects.map { case (address, effect) => effect -> address.U(12.W) })
    distribution.bits.data := finalData
    fdiCSRMap.values.foreach { case (port, _) => port.wdata := finalData }
    assert(PopCount(VecInit(effects.map(_._2))) <= 1.U,
      "Only one accepted FDI write may update and distribute each cycle")
  }

  private val writeFpLegal  = permitMod.io.out.hasLegalWriteFcsr
  private val writeVecLegal = permitMod.io.out.hasLegalWriteVcsr

  permitMod.io.in.csrAccess.ren := ren && valid
  permitMod.io.in.csrAccess.wen := wen
  permitMod.io.in.csrAccess.addr := addr

  permitMod.io.in.privState := privState
  permitMod.io.in.debugMode := debugMode
  permitMod.io.in.userHandler := userInHandler.getOrElse(false.B)
  permitMod.io.in.userTimerEnabled := HasFDI.B
  permitMod.io.in.fdiSelected := fdiAddressHit
  permitMod.io.in.fdiNotTrusted := false.B
  if (HasFDI) {
    val trust = Module(new xiangshan.backend.fu.FDIPcTrustChecker)
    trust.io.pc := io.in.bits.sourcePc
    trust.io.sourcePrivilege := privState.PRVM.asUInt
    trust.io.sourceVirtual := privState.isVirtual
    trust.io.sEnable := fdiMainCfg.get.mainCfg.regOut.sEnable.asBool
    trust.io.uEnable := fdiMainCfg.get.mainCfg.regOut.uEnable.asBool
    trust.io.sBoundLo := fdiBounds.get.sMainBoundLo.regOut.asUInt
    trust.io.sBoundHi := fdiBounds.get.sMainBoundHi.regOut.asUInt
    trust.io.uBoundLo := fdiBounds.get.uMainBoundLo.regOut.asUInt
    trust.io.uBoundHi := fdiBounds.get.uMainBoundHi.regOut.asUInt
    permitMod.io.in.fdiNotTrusted := trust.io.notTrusted
  }
  permitMod.io.in.xRet.uret := io.in.bits.uret && valid

  permitMod.io.in.xRet.mnret := io.in.bits.mnret && valid
  permitMod.io.in.xRet.mret  := io.in.bits.mret  && valid
  permitMod.io.in.xRet.sret  := io.in.bits.sret  && valid
  permitMod.io.in.xRet.dret  := io.in.bits.dret  && valid

  permitMod.io.in.status.tsr := mstatus.regOut.TSR.asBool
  permitMod.io.in.status.vtsr := hstatus.regOut.VTSR.asBool

  permitMod.io.in.status.tvm  := mstatus.regOut.TVM.asBool
  permitMod.io.in.status.vtvm := hstatus.regOut.VTVM.asBool

  permitMod.io.in.status.vgein := hstatus.regOut.VGEIN.asUInt

  permitMod.io.in.xcounteren.mcounteren := mcounteren.rdata
  permitMod.io.in.xcounteren.hcounteren := hcounteren.rdata
  permitMod.io.in.xcounteren.scounteren := scounteren.rdata

  permitMod.io.in.xstateen.mstateen0 := mstateen0.rdata
  permitMod.io.in.xstateen.hstateen0 := hstateen0.rdata
  permitMod.io.in.xstateen.sstateen0 := sstateen0.rdata

  permitMod.io.in.xenvcfg.menvcfg := menvcfg.rdata
  permitMod.io.in.xenvcfg.henvcfg := henvcfg.rdata

  permitMod.io.in.status.mstatusFSOff  :=  mstatus.regOut.FS === ContextStatus.Off
  permitMod.io.in.status.mstatusVSOff  :=  mstatus.regOut.VS === ContextStatus.Off
  permitMod.io.in.status.vsstatusFSOff := vsstatus.regOut.FS === ContextStatus.Off
  permitMod.io.in.status.vsstatusVSOff := vsstatus.regOut.VS === ContextStatus.Off

  permitMod.io.in.aia.miselectIsIllegal  := miselect.isIllegal
  permitMod.io.in.aia.siselectIsIllegal  := siselect.isIllegal
  permitMod.io.in.aia.vsiselectIsIllegal := vsiselect.isIllegal
  permitMod.io.in.aia.siselect := siselect.rdata
  permitMod.io.in.aia.vsiselect := vsiselect.rdata
  permitMod.io.in.aia.mvienSEIE := mvien.regOut.SEIE.asBool
  permitMod.io.in.aia.hvictlVTI := hvictl.regOut.VTI.asBool

  sstcIRGen.i.stime.valid := time.updated
  sstcIRGen.i.stime.bits  := time.stime
  sstcIRGen.i.vstime.valid := time.updated
  sstcIRGen.i.vstime.bits  := time.vstime
  sstcIRGen.i.stimecmp.wen := GatedValidRegNext(stimecmp.w.wen)
  sstcIRGen.i.stimecmp.rdata  := stimecmp.rdata
  sstcIRGen.i.vstimecmp.wen   := GatedValidRegNext(vstimecmp.w.wen)
  sstcIRGen.i.vstimecmp.rdata := vstimecmp.rdata
  sstcIRGen.i.menvcfg.wen   := GatedValidRegNext(menvcfg.w.wen)
  sstcIRGen.i.menvcfg.STCE  := menvcfg.regOut.STCE.asBool
  sstcIRGen.i.henvcfg.wen   := GatedValidRegNext(henvcfg.w.wen)
  sstcIRGen.i.henvcfg.STCE  := henvcfg.regOut.STCE.asBool
  sstcIRGen.i.htimedeltaWen := GatedValidRegNext(htimedelta.w.wen)

  miregiprios.foreach { mod =>
    mod.w.wen := mireg.w.wen && (miselect.regOut.ALL.asUInt === mod.addr.U)
    mod.w.wdata := wdata
  }

  siregiprios.foreach { mod =>
    mod.w.wen := sireg.w.wen && (siselect.regOut.ALL.asUInt === mod.addr.U)
    mod.w.wdata := wdata
  }

  iregiprios.foreach { mod =>
    mod match {
      case m: HasIeBundle =>
        m.mie := mie.regOut
        m.sie := sie.regOut
      case _ =>
    }
  }

  mhartid.hartid := this.io.fromTop.hartId

  pmpcfgs.zipWithIndex.foreach { case (mod, i) =>
    mod.w.wen   := wenLegalReg && (addr === (CSRs.pmpcfg0 + i / 8 * 2).U)
    mod.w.wdata := pmpEntryMod.io.out.pmpCfgWData(8*((i%8)+1)-1,8*(i%8))
  }

  pmpaddr.zipWithIndex.foreach { case (mod, i) =>
    mod.w.wen   := wenLegalReg && (addr === (CSRs.pmpaddr0 + i).U)
    mod.w.wdata := pmpEntryMod.io.out.pmpAddrWData(i)
  }

  pmacfgs.zipWithIndex.foreach { case (mod, i) =>
    mod.w.wen   := wenLegalReg && (addr === (CSRConst.PmacfgBase + i / 8 * 2).U)
    mod.w.wdata := pmaEntryMod.io.out.pmaCfgWdata(8*((i%8)+1)-1,8*(i%8))
  }

  csrMods.foreach { mod =>
    mod match {
      case m: HypervisorBundle =>
        m.hstatus := hstatus.regOut
      case _ =>
    }
    mod match {
      case m: VirtualSupervisorBundle =>
        m.v := V.asUInt.asBool
        m.hgatp := hgatp.regOut
      case _ =>
    }
    mod match {
      case m: HasMachineDelegBundle =>
        m.mideleg := mideleg.regOut
        m.medeleg := medeleg.regOut
      case _ =>
    }
    mod match {
      case m: HasMachineCounterControlBundle =>
        m.mcountinhibit := mcountinhibit.regOut
      case _ =>
    }
    mod match {
      case m: HasExternalInterruptBundle =>
        m.platformIRP := this.platformIRP
        m.platformIRP.STIP  := sstcIRGen.o.STIP
        m.platformIRP.VSTIP := sstcIRGen.o.VSTIP
      case _ =>
    }
    mod match {
      case m: HasRobCommitBundle =>
        // Todo: move RegNext from ROB to CSR
        m.robCommit.instNum := io.fromRob.commit.instNum
        m.robCommit.fflags  := RegNextWithEnable(io.fromRob.commit.fflags)
        m.robCommit.fsDirty := GatedValidRegNext(io.fromRob.commit.fsDirty)
        m.robCommit.vsDirty := GatedValidRegNext(io.fromRob.commit.vsDirty)
        m.robCommit.vxsat   := RegNextWithEnable(io.fromRob.commit.vxsat)
        m.robCommit.vtype   := RegNextWithEnable(io.fromRob.commit.vtype)
        m.robCommit.vl      := RegNext          (io.fromRob.commit.vl)
        m.robCommit.vstart  := RegNextWithEnable(io.fromRob.commit.vstart)
        m.writeFCSR         := writeFpLegal
        m.writeVCSR         := writeVecLegal
        m.isVirtMode        := V.asUInt.asBool
      case _ =>
    }
    mod match {
      case m: TrapEntryHUEventSink => m.trapToHU := trapEntryHUEvent.get.out
      case _ =>
    }
    mod match {
      case m: UretEventSink => m.retFromU := uretEvent.get.out
      case _ =>
    }
    mod match {
      case m: TrapEntryDEventSinkBundle =>
        m.trapToD := trapEntryDEvent.out
      case _ =>
    }
    mod match {
      case m: TrapEntryMEventSinkBundle =>
        m.trapToM := trapEntryMEvent.out
      case _ =>
    }
    mod match {
      case m: TrapEntryMNEventSinkBundle =>
        m.trapToMN := trapEntryMNEvent.out
      case _ =>
    }
    mod match {
      case m: TrapEntryHSEventSinkBundle =>
        m.trapToHS := trapEntryHSEvent.out
      case _ =>
    }
    mod match {
      case m: TrapEntryVSEventSinkBundle =>
        m.trapToVS := trapEntryVSEvent.out
      case _ =>
    }
    mod match {
      case m: MretEventSinkBundle =>
        m.retFromM := mretEvent.out
      case _ =>
    }
    mod match {
      case m: MNretEventSinkBundle =>
        m.retFromMN := mnretEvent.out
      case _ =>
    }
    mod match {
      case m: SretEventSinkBundle =>
        m.retFromS := sretEvent.out
      case _ =>
    }
    mod match {
      case m: SretEventSDTSinkBundle =>
        m.retFromSSDT := sretEvent.outSDT
      case _ =>
    }
    mod match {
      case m: DretEventSinkBundle =>
        m.retFromD := dretEvent.out
      case _ =>
    }
    mod match {
      case m: HasAIABundle =>
        m.aiaToCSR.rdata.valid := fromAIA.rdata.valid
        m.aiaToCSR.rdata.bits.data := fromAIA.rdata.bits.data
        m.aiaToCSR.rdata.bits.illegal := fromAIA.rdata.bits.illegal
        m.aiaToCSR.meip    := fromAIA.meip
        m.aiaToCSR.seip    := fromAIA.seip
        m.aiaToCSR.vseip   := fromAIA.vseip
        m.aiaToCSR.mtopei  := fromAIA.mtopei
        m.aiaToCSR.stopei  := fromAIA.stopei
        m.aiaToCSR.vstopei := fromAIA.vstopei
      case _ =>
    }
    mod match {
      case m: HasInterruptFilterSink =>
        m.topIR.mtopi  := intrMod.io.out.mtopi
        m.topIR.stopi  := intrMod.io.out.stopi
        m.topIR.vstopi := intrMod.io.out.vstopi
      case _ =>
    }
    mod match {
      case m: HasPMPAddrSink =>
        m.addrRData := pmpEntryMod.io.out.pmpAddrRData
      case _ =>
    }
    mod match {
      case m: HasPMAAddrSink =>
        m.addrRData := pmaEntryMod.io.out.pmaAddrRData
      case _ =>
    }
    mod match {
      case m: HasMHPMSink =>
        // cycle from mcycle
        m.mHPM.cycle := mcycle.rdata
        // time from clint
        m.mHPM.time  := io.fromTop.clintTime
        // instret from minstret
        m.mHPM.instret := minstret.rdata
        // VS-Mode or VU-Mode
        m.v := privState.isVirtual
        m.nextV := nextV.isOneOf(VirtMode.On)
        m.htimedelta := htimedelta.rdata
        m.mHPM.hpmcounters.zip(mhpmcounters).map{
          case(counter, mcounter) => counter := mcounter.rdata
        }
      case _ =>
    }
    mod match {
      case m: HasMachineEnvBundle =>
        m.menvcfg := menvcfg.regOut
      case _ =>
    }
    mod match {
      case m: HasHypervisorEnvBundle =>
        m.menvcfg := menvcfg.regOut
      case _ =>
    }
    mod match {
      case m: HasVirtualSupervisorEnvBundle =>
        m.henvcfg := henvcfg.regOut
        m.menvcfg := menvcfg.regOut
      case _ =>
    }
    mod match {
      case m: HasIpIeBundle =>
        m.mideleg := mideleg.regOut
        m.mip := mip.rdata
        m.mie := mie.regOut
        m.mvip := mvip.regOut
        m.mvien := mvien.regOut
        m.hideleg := hideleg.regOut
        m.hip := hip.regOut
        m.hie := hie.regOut
        m.hvien := hvien.regOut
        m.hvip := hvip.regOut
        m.sip := sip.regOut
        m.sie := sie.regOut
        m.vsip := vsip.regOut
        m.vsie := vsie.regOut
        m.hgeip := hgeip.regOut
        m.hgeie := hgeie.regOut
        m.hstatusVGEIN := hstatus.regOut.VGEIN
      case _ =>
    }
    mod match {
      case m: HasMhpmeventOfBundle =>
        m.ofVec := VecInit(mhpmevents.map{ event =>
          val mhpmevent = Wire(new MhpmeventBundle)
          mhpmevent := event.rdata
          mhpmevent.OF.asBool
        }).asUInt
        m.privState := privState
        m.mcounteren := mcounteren.rdata
        m.hcounteren := hcounteren.rdata
      case _ =>
    }
    mod match {
      case m: HasStateen0Bundle =>
        m.fromMstateen0 := mstateen0.regOut
        m.fromHstateen0 := hstateen0.regOut
        m.privState     := privState
      case _ =>
    }
    mod match {
      case m: HasDebugStopBundle =>
        m.debugModeStopCount := debugModeStopCount
        m.debugModeStopTime  := debugModeStopTimeNext
        m.unprivCountUpdate  := unprivCountUpdate
      case _ =>
    }
    mod match {
      case m: HasNmipBundle =>
        m.nmip := nmip.asUInt.orR
      case _ =>
    }
    mod match {
      case m: HasMachineFlushL2Bundle =>
        m.l2FlushDone := io.fromTop.l2FlushDone
      case _ =>
    }
  }

  csrMods.foreach { mod =>
    println(s"${mod.modName}: ")
    println(mod.dumpFields)
  }

  trapEntryMNEvent.valid  := ((hasTrap && nmi) || dbltrpToMN) && !entryDebugMode && !debugMode && mnstatus.regOut.NMIE
  trapEntryMEvent .valid  := hasTrap && entryPrivState.isModeM && !dbltrpToMN && !entryDebugMode && !debugMode && !nmi && mnstatus.regOut.NMIE
  trapEntryHSEvent.valid  := hasTrap && entryPrivState.isModeHS && !entryDebugMode && !debugMode && mnstatus.regOut.NMIE
  trapEntryVSEvent.valid  := hasTrap && entryPrivState.isModeVS && !entryDebugMode && !debugMode && mnstatus.regOut.NMIE

  if (HasFDI) {
    // Use the actual cause write events: double-trap and debug routing may
    // replace or suppress the synchronous cause selected by TrapHandleModule.
    val committedFDITrap = Seq(trapEntryMEvent.out.mcause, trapEntryHSEvent.out.scause).map { event =>
      event.valid && !event.bits.Interrupt.asBool &&
        (event.bits.ExceptionCode.asUInt === dasicsU.U || event.bits.ExceptionCode.asUInt === dasicsS.U)
    }.reduce(_ || _)
    fdiSpecial.get.fReason.trapReason.valid := committedFDITrap
    fdiSpecial.get.fReason.trapReason.bits.REASON := io.fromRob.trap.bits.fdiException.get.reason
  }

  Seq(trapEntryMEvent, trapEntryMNEvent, trapEntryHSEvent, trapEntryVSEvent, trapEntryDEvent).foreach { eMod =>
    eMod.in match {
      case in: TrapEntryEventInput =>
        in.causeNO := trapHandleMod.io.out.causeNO
        in.fdiException.foreach(_ := io.fromRob.trap.bits.fdiException.get)
        in.trapPc := trapPC
        in.trapPcGPA := trapPCGPA // only used by trapEntryMEvent & trapEntryHSEvent
        in.trapInst := io.trapInst
        in.fetchMalTval := io.fetchMalTval
        in.isCrossPageIPF := trapIsCrossPageIPF
        in.isHls := trapIsHls
        in.isFetchMalAddr := trapIsFetchMalAddr
        in.isFetchBkpt := trapIsFetchBkpt
        in.trapIsForVSnonLeafPTE := trapIsForVSnonLeafPTE
        in.hasDTExcp := hasDTExcp

        in.iMode.PRVM := PRVM
        in.iMode.V := V
        // when NMIE is zero, force to behave as MPRV is zero
        in.dMode.PRVM := Mux(mstatus.regOut.MPRV.asBool && mnstatus.regOut.NMIE.asBool, mstatus.regOut.MPP, PRVM)
        in.dMode.V := V.asUInt.asBool || mstatus.regOut.MPRV && mnstatus.regOut.NMIE.asBool && (mstatus.regOut.MPP =/= PrivMode.M) && mstatus.regOut.MPV

        in.privState := privState
        in.mstatus := mstatus.regOut
        in.hstatus := hstatus.regOut
        in.sstatus := mstatus.sstatus
        in.vsstatus := vsstatus.regOut
        in.pcFromXtvec := trapHandleMod.io.out.pcFromXtvec

        in.menvcfg := menvcfg.regOut
        in.henvcfg := henvcfg.regOut

        in.satp  := satp.regOut
        in.vsatp := vsatp.regOut
        in.hgatp := hgatp.regOut
        if (HasBitmapCheck) {
          in.mbmc := mbmc.get.regOut
        } else {
          in.mbmc := DontCare
        }

        in.memExceptionVAddr := io.fromMem.excpVA
        in.memExceptionGPAddr := io.fromMem.excpGPA
        in.memExceptionIsForVSnonLeafPTE := io.fromMem.excpIsForVSnonLeafPTE

        in.virtualInterruptIsHvictlInject := virtualInterruptIsHvictlInject
        in.hvictlIID := Mux(useAcceptedInterrupt,
          deliveredInterrupt.map(_.hvictlIID).getOrElse(0.U), hvictl.regOut.IID.asUInt)
    }
  }

  mnretEvent.valid := legalMNret
  mnretEvent.in match {
    case in =>
      in.mstatus := mstatus.regOut
      in.vsstatus := vsstatus.regOut
      in.mnepc   := mnepc.regOut
      in.mnstatus:= mnstatus.regOut
      in.satp := satp.regOut
      in.vsatp := vsatp.regOut
      in.hgatp := hgatp.regOut
  }

  mretEvent.valid := legalMret
  mretEvent.in match {
    case in =>
      in.mstatus := mstatus.regOut
      in.vsstatus := vsstatus.regOut
      in.mepc := mepc.regOut
      in.satp := satp.regOut
      in.vsatp := vsatp.regOut
      in.hgatp := hgatp.regOut
  }

  sretEvent.valid := legalSret
  sretEvent.in match {
    case in =>
      in.privState := privState
      in.mstatus := mstatus.regOut
      in.hstatus := hstatus.regOut
      in.vsstatus := vsstatus.regOut
      in.sepc := sepc.regOut
      in.vsepc := vsepc.regOut
      in.satp := satp.regOut
      in.vsatp := vsatp.regOut
      in.hgatp := hgatp.regOut
  }

  dretEvent.valid := legalDret
  dretEvent.in match {
    case in =>
      in.dcsr := dcsr.regOut
      in.dpc  := dpc.regOut
      in.mstatus := mstatus.regOut
      in.vsstatus := vsstatus.regOut
      in.satp := satp.regOut
      in.vsatp := vsatp.regOut
      in.hgatp := hgatp.regOut
  }

  PRVM := MuxCase(
    PRVM,
    events.filter(_.out.isInstanceOf[EventUpdatePrivStateOutput]).map {
      x => x.out match {
        case xx: EventUpdatePrivStateOutput => (xx.privState.valid -> xx.privState.bits.PRVM)
      }
    }
  )

  nextV := MuxCase(
    V,
    events.filter(_.out.isInstanceOf[EventUpdatePrivStateOutput]).map {
      x => x.out match {
        case xx: EventUpdatePrivStateOutput => (xx.privState.valid -> xx.privState.bits.V)
      }
    }
  )

  debugMode := MuxCase(
    debugMode,
    Seq(
      dretEvent.out.debugMode.valid -> dretEvent.out.debugMode.bits,
      trapEntryDEvent.out.debugMode.valid -> trapEntryDEvent.out.debugMode.bits
    )
  )

  debugIntrEnable := MuxCase(
    debugIntrEnable,
    Seq(
      dretEvent.out.debugIntrEnable.valid -> dretEvent.out.debugIntrEnable.bits,
      trapEntryDEvent.out.debugIntrEnable.valid -> trapEntryDEvent.out.debugIntrEnable.bits
    )
  )

  // perf
  val addrInPerfCnt = (wenLegal || ren) && (
    (addr >= CSRs.mcycle.U) && (addr <= CSRs.mhpmcounter31.U) ||
    (addr >= CSRs.cycle.U) && (addr <= CSRs.hpmcounter31.U)
  )

  val resetSatp = WireInit(false.B)
  // flush
  if (HasBitmapCheck) {
    resetSatp := Cat(Seq(satp, vsatp, hgatp, mbmc.get).map(_.addr.U === addr)).orR && wenLegalReg // write to satp will cause the pipeline be flushed
  } else {
    resetSatp := Cat(Seq(satp, vsatp, hgatp).map(_.addr.U === addr)).orR && wenLegalReg // write to satp will cause the pipeline be flushed
  }

  val floatStatusOnOff = mstatus.w.wen && (
    mstatus.w.wdataFields.FS === ContextStatus.Off && mstatus.regOut.FS =/= ContextStatus.Off ||
    mstatus.w.wdataFields.FS =/= ContextStatus.Off && mstatus.regOut.FS === ContextStatus.Off
  ) || mstatus.wAliasSstatus.wen && (
    mstatus.wAliasSstatus.wdataFields.FS === ContextStatus.Off && mstatus.regOut.FS =/= ContextStatus.Off ||
    mstatus.wAliasSstatus.wdataFields.FS =/= ContextStatus.Off && mstatus.regOut.FS === ContextStatus.Off
  ) || vsstatus.w.wen && (
    vsstatus.w.wdataFields.FS === ContextStatus.Off && vsstatus.regOut.FS =/= ContextStatus.Off ||
    vsstatus.w.wdataFields.FS =/= ContextStatus.Off && vsstatus.regOut.FS === ContextStatus.Off
  )

  val vectorStatusOnOff = mstatus.w.wen && (
    mstatus.w.wdataFields.VS === ContextStatus.Off && mstatus.regOut.VS =/= ContextStatus.Off ||
    mstatus.w.wdataFields.VS =/= ContextStatus.Off && mstatus.regOut.VS === ContextStatus.Off
  ) || mstatus.wAliasSstatus.wen && (
    mstatus.wAliasSstatus.wdataFields.VS === ContextStatus.Off && mstatus.regOut.VS =/= ContextStatus.Off ||
    mstatus.wAliasSstatus.wdataFields.VS =/= ContextStatus.Off && mstatus.regOut.VS === ContextStatus.Off
  ) || vsstatus.w.wen && (
    vsstatus.w.wdataFields.VS === ContextStatus.Off && vsstatus.regOut.VS =/= ContextStatus.Off ||
    vsstatus.w.wdataFields.VS =/= ContextStatus.Off && vsstatus.regOut.VS === ContextStatus.Off
  )

  val triggerFrontendChange = Wire(Bool())

  val vstartChange = vstart.w.wen && (
    vstart.w.wdata === 0.U && vstart.regOut.vstart.asUInt =/= 0.U ||
    vstart.w.wdata =/= 0.U && vstart.regOut.vstart.asUInt === 0.U
  )

  // flush pipe when write frm and data > 4 or write fcsr and data[7:5] > 4 or write frm/fcsr and frm is reserved
  val frmIsReserved = fcsr.frm(2) && fcsr.frm(1, 0).orR
  val frmWdataReserved = fcsr.wAliasFfm.wdata(2) && fcsr.wAliasFfm.wdata(1, 0).orR
  val fcsrWdataReserved = fcsr.w.wdata(7) && fcsr.w.wdata(6, 5).orR
  val frmChange = fcsr.wAliasFfm.wen && (!frmIsReserved && frmWdataReserved || frmIsReserved && !frmWdataReserved) ||
    fcsr.w.wen && (!frmIsReserved && fcsrWdataReserved || frmIsReserved && !fcsrWdataReserved)

  private val fdiWriteNeedsFlush = if (HasFDI) {
    // The C1 software effect is single-cycle, while its response may remain stalled.
    // Cancellation or acceptance clears ownership before another event can be saved.
    val pending = RegInit(false.B)
    when(redirectFlush || io.in.fire || io.out.fire) {
      pending := false.B
    }.elsewhen(fdiSoftwareWriteEffect) {
      pending := true.B
    }
    (fdiSoftwareWriteEffect || pending) && !redirectFlush && !reset.asBool
  } else false.B

  val flushPipe = resetSatp ||
    triggerFrontendChange || floatStatusOnOff || vectorStatusOnOff ||
    vstartChange || frmChange || fdiWriteNeedsFlush

  /**
   * Look up id in vsMapS and sMapVS.
   * If id is in vsMapS, use vsMapS(id) when under VS mode,
   *                         id under other modes
   * Else If id is in sMapVS, use 0 when under VS mode,
   *                              id under modes except VS
   * Else, use id as read address
   * Use read address to look up rdata in csrRwMap
   */
  private val rdata = Mux1H(csrRwMap.map { case (id, (_, rdata)) =>
    if (vsMapS.contains(id)) {
      ((isModeVS && addr === vsMapS(id).U) || !isModeVS && addr === id.U) -> rdata
    } else if (sMapVS.contains(id)) {
      (!isModeVS && addr === id.U) -> rdata
    } else {
      (raddr === id.U) -> rdata
    }
  })

  private val rwMask = 0xc00
  private val csrOutMapFilter = csrOutMap.filter { case (id, _) => (id & rwMask) != rwMask }

  private val regOut = Mux1H(csrOutMapFilter.map { case (id, regOut) =>
    if (vsMapS.contains(id)) {
      ((isModeVS && addr === vsMapS(id).U) || !isModeVS && addr === id.U) -> regOut
    } else if (sMapVS.contains(id)) {
      (!isModeVS && addr === id.U) -> regOut
    } else {
      (raddr === id.U) -> regOut
    }
  })

  private val needTargetUpdate = mnretEvent.out.targetPc.valid || mretEvent.out.targetPc.valid || sretEvent.out.targetPc.valid || dretEvent.out.targetPc.valid ||
    trapEntryMEvent.out.targetPc.valid || trapEntryMNEvent.out.targetPc.valid || trapEntryHSEvent.out.targetPc.valid || trapEntryVSEvent.out.targetPc.valid || trapEntryDEvent.out.targetPc.valid ||
    userEntryEffect || userReturnEffect

  private val noCSRIllegal = (ren || wen) && Cat(csrRwMap.keys.toSeq.sorted.map(csrAddr => !(addr === csrAddr.U))).andR

  private val noCSRIllegalReg = RegEnable(noCSRIllegal, ren || wen)

  private val s_idle :: s_waitIMSIC :: s_finish :: Nil = Enum(3)

  /** the state machine of newCSR module */
  private val state = RegInit(s_idle)
  /** the next state of newCSR */
  private val stateNext = WireInit(state)
  state := stateNext

  /**
   * Asynchronous access operation of CSR. Check whether an access is asynchronous when read/write-enable is high.
   * AIA registers are designed to be access asynchronously, so newCSR will wait for response.
   **/
  private val asyncAccess = (wen || ren) && !(permitMod.io.out.EX_II || permitMod.io.out.EX_VI) && (
    mireg.addr.U === addr && miselect.inIMSICRange ||
    sireg.addr.U === addr && ((!V.asUInt.asBool && siselect.inIMSICRange) || (V.asUInt.asBool && vsiselect.inIMSICRange)) ||
    vsireg.addr.U === addr && vsiselect.inIMSICRange
  )

  /** State machine of newCSR */
  switch(state) {
    is(s_idle) {
      when(valid && redirectFlush) {
        stateNext := s_idle
      }.elsewhen(valid && asyncAccess) {
        stateNext := s_waitIMSIC
      }.elsewhen(valid) {
        stateNext := s_finish
      }
    }
    is(s_waitIMSIC) {
      when(redirectFlush) {
        stateNext := s_idle
      }.elsewhen(fromAIA.rdata.valid) {
        when(io.out.ready) {
          stateNext := s_idle
        }.otherwise {
          stateNext := s_finish
        }
      }
    }
    is(s_finish) {
      when(redirectFlush || io.out.ready) {
        stateNext := s_idle
      }
    }
  }


  // Todo: check IMSIC EX_II and EX_VI
  private val imsicIllegal = fromAIA.rdata.valid && fromAIA.rdata.bits.illegal
  private val imsic_EX_II = imsicIllegal && !V.asUInt.asBool
  private val imsic_EX_VI = imsicIllegal && V.asUInt.asBool

  /** Set io.in.ready when state machine is ready to receive a new request synchronously */
  softwareSlotReady := state === s_idle && !huBusy.getOrElse(false.B)
  io.in.ready := softwareSlotReady

  /**
   * Valid signal of newCSR output.
   * When in IDLE state, when input_valid is high, we set it.
   * When in waitIMSIC state, and the next state is IDLE, we set it.
   **/

  /** Data that have been read before,and should be stored because output not fired */
  val normalCSRValid = state === s_idle && valid && !asyncAccess
  val waitIMSICValid = state === s_waitIMSIC && fromAIA.rdata.valid

  io.out.valid := (waitIMSICValid || state === s_finish) && !redirectFlush && !userEventCancel
  io.out.bits.userReturnTarget := 0.U.asTypeOf(new TargetPCBundle)
  io.out.bits.userReturnRedirect := false.B

  if (HasFDI) {
    val port = io.huEntry.get
    val hu = trapEntryHUEvent.get
    val ret = uretEvent.get
    val retSaved = Reg(new UserTrapEventInput)
    hu.in := huSaved.get
    ret.in := retSaved
    val higherEvent = hasTrap || entryDebugMode || dbltrpToMN
    userEventCancel := userReturnPending.get && (higherEvent || reset.asBool)
    val status = userTimerCSROutMap(UserTimerCSRAddress.ustatus)
    huEligible := isModeHU && !debugMode && !userInHandler.get &&
      userTimer.get.io.pending && status(0) && userTimerCSROutMap(UserTimerCSRAddress.uie)(4) &&
      mstateen0.regOut.C.asBool && sstateen0.rdataFields.C.asBool &&
      !dcsr.regOut.STEP.asBool && mnstatus.regOut.NMIE.asBool

    val qualificationAddresses = Seq(UserTimerCSRAddress.utimer, UserTimerCSRAddress.ustatus,
      UserTimerCSRAddress.uie, CSRs.mstateen0, CSRs.sstateen0, CSRs.dcsr, CSRs.mnstatus)
    val qualificationWriteC0 = io.in.fire && wenLegal && !redirectFlush &&
      qualificationAddresses.map(address => addr === address.U).reduce(_ || _)
    val qualificationWriteC1 = qualificationAddresses.map(address => csrRwMap(address)._1.wen).reduce(_ || _)
    huCandidateInvalidate := !huEligible || qualificationWriteC0 || qualificationWriteC1 ||
      huBusy.get || higherEvent || intrMod.io.out.higherPriority.get || reset.asBool
    val sameRequest = port.request.bits.event.asUInt === huIdentity.get.asUInt
    huCancelNow := port.cancel.valid && huBusy.get && !huFinished.get && !huTrapFinished.get &&
      port.cancel.bits.event.asUInt === huIdentity.get.asUInt
    val replay = huCanceled.get || huCancelNow

    port.reserve.ready := softwareSlotReady && !io.in.valid && !huCandidateInvalidate && !port.cancel.valid
    huClaim := port.reserve.fire
    // Once reserved, PC arrival no longer depends on live pending, masks, mode,
    // or a cancellation. Replay and critical debug both still need that exact PC.
    port.request.ready := huBusy.get && !huPcValid.get && !huFinished.get && sameRequest && !reset.asBool
    port.satpMode := Mux(huBusy.get, huSaved.get.satp.MODE.asUInt, satp.regOut.MODE.asUInt)
    port.completion.valid := huBusy.get && huPcValid.get && !huFinished.get && !higherEvent && !reset.asBool &&
      (!huDebugPending.get || huTrapFinished.get || replay)
    port.completion.bits.event := huIdentity.get
    port.completion.bits.outcome := Mux(huTrapFinished.get, HUEntryOutcome.trap,
      Mux(replay, HUEntryOutcome.replay, HUEntryOutcome.enter))
    val replayTarget = WireDefault(0.U.asTypeOf(new TargetPCBundle))
    replayTarget.pc := huSaved.get.pc
    port.completion.bits.target := Mux(huTrapFinished.get, huTrapTarget.get,
      Mux(replay, replayTarget, hu.out.targetPc.bits))
    port.canceled := huBusy.get && !huFinished.get && !huTrapFinished.get &&
      (replay || higherEvent || huDebugPending.get || reset.asBool)
    port.effectLocked := huTrapFinished.get || huFinished.get
    userEntryEffect := port.completion.fire && port.completion.bits.outcome === HUEntryOutcome.enter
    hu.valid := userEntryEffect

    when(port.reserve.fire) {
      huBusy.get := true.B
      huIdentity.get := port.reserve.bits
      huSaved.get.target := userTimerCSROutMap(UserTimerCSRAddress.utvec)
      huSaved.get.savedEnable := status(0)
      huSaved.get.satp := satp.regOut
      huPcValid.get := false.B
      huFinished.get := false.B
      huCanceled.get := false.B
      huDebugPending.get := false.B
      huTrapFinished.get := false.B
    }
    when(port.request.fire) {
      huPcValid.get := true.B
      huSaved.get.pc := port.request.bits.pc
    }
    when(huBusy.get && !huFinished.get) {
      when(huCancelNow || hasTrap) { huCanceled.get := true.B }
      when(criticalErrorState && dcsr.regOut.CETRIG.asBool && !debugMode) {
        huDebugPending.get := true.B
      }
      when(needTargetUpdate && higherEvent && !userEntryEffect && !userReturnEffect) {
        huTrapTarget.get := io.out.bits.targetPc
        huTrapFinished.get := true.B
      }
    }
    when(port.completion.fire) { huFinished.get := true.B }
    when(port.release) {
      assert(huBusy.get && (huFinished.get || port.completion.fire))
      huBusy.get := false.B
      huPcValid.get := false.B
      huFinished.get := false.B
      huCanceled.get := false.B
      huDebugPending.get := false.B
      huTrapFinished.get := false.B
    }
    when(port.reserve.fire) {
      assert(port.reserve.bits.interrupt.irToHU && port.reserve.bits.interrupt.isInterrupt &&
        port.reserve.bits.interrupt.cause === 4.U)
    }
    when(port.request.valid && huBusy.get) { assert(sameRequest) }
    when(userEntryEffect) { assert(huPcValid.get && !huCanceled.get && !huTrapFinished.get) }

    when(io.in.fire) {
      userReturnPending.get := io.in.bits.uret && !redirectFlush && !higherEvent
      userReturnLegal.get := permitMod.io.out.hasLegalUret
      retSaved.pc := 0.U
      retSaved.target := userTimerCSROutMap(UserTimerCSRAddress.uepc)
      retSaved.savedEnable := status(4)
      retSaved.satp := satp.regOut
    }.elsewhen(io.out.fire || redirectFlush || userEventCancel) {
      userReturnPending.get := false.B
    }
    when(userEventCancel || (valid && io.in.bits.uret && higherEvent)) {
      stateNext := s_idle
    }
    userReturnEffect := io.out.fire && userReturnPending.get && userReturnLegal.get
    ret.valid := userReturnEffect
    io.out.bits.userReturnTarget := ret.out.targetPc.bits
    io.out.bits.userReturnRedirect := userReturnPending.get && userReturnLegal.get && !userEventCancel && !redirectFlush
    assert(!(userEntryEffect && userReturnEffect))
    when(userEntryEffect || userReturnEffect) {
      assert(!higherEvent && !userTimerCSRMap.values.map(_._1.wen).reduce(_ || _))
    }
  }
  io.out.bits.EX_II := DataHoldBypass(Mux1H(Seq(
    normalCSRValid -> (permitMod.io.out.EX_II || noCSRIllegal),
    waitIMSICValid -> imsic_EX_II,
  )), false.B, normalCSRValid || waitIMSICValid)
  io.out.bits.EX_VI := DataHoldBypass(Mux1H(Seq(
    normalCSRValid -> permitMod.io.out.EX_VI,
    waitIMSICValid -> imsic_EX_VI,
  )), false.B, normalCSRValid || waitIMSICValid)
  io.out.bits.flushPipe := flushPipe

  /** Prepare read data for output */
  io.out.bits.rData := DataHoldBypass(
    Mux1H(Seq(
      io.in.fire -> rdata,
      fromAIA.rdata.valid -> fromAIA.rdata.bits.data
    )), 0.U(64.W), io.in.fire || fromAIA.rdata.valid)
  io.out.bits.regOut := regOut
  io.out.bits.targetPc := DataHoldBypass(
    Mux(trapEntryDEvent.out.targetPc.valid,
      trapEntryDEvent.out.targetPc.bits,
      Mux1H(Seq(
        mnretEvent.out.targetPc.valid -> mnretEvent.out.targetPc.bits,
        mretEvent.out.targetPc.valid  -> mretEvent.out.targetPc.bits,
        sretEvent.out.targetPc.valid  -> sretEvent.out.targetPc.bits,
        dretEvent.out.targetPc.valid  -> dretEvent.out.targetPc.bits,
        trapEntryMEvent.out.targetPc.valid -> trapEntryMEvent.out.targetPc.bits,
        trapEntryMNEvent.out.targetPc.valid -> trapEntryMNEvent.out.targetPc.bits,
        trapEntryHSEvent.out.targetPc.valid -> trapEntryHSEvent.out.targetPc.bits,
        trapEntryVSEvent.out.targetPc.valid -> trapEntryVSEvent.out.targetPc.bits) ++
        trapEntryHUEvent.toSeq.map(e => e.out.targetPc.valid -> e.out.targetPc.bits) ++
        uretEvent.toSeq.map(e => e.out.targetPc.valid -> e.out.targetPc.bits)
      )
    ),
  needTargetUpdate)
  io.out.bits.targetPcUpdate := needTargetUpdate
  io.out.bits.isPerfCnt := DataHoldBypass(addrInPerfCnt, false.B, io.in.fire)

  io.status.userInHandler.foreach(_ := userInHandler.get)
  io.status.userEntryEffect.foreach(_ := userEntryEffect)
  io.status.userReturnEffect.foreach(_ := userReturnEffect)
  io.status.userTargetIdentity.foreach { identity =>
    identity.valid := userEntryEffect || huTakeDebug
    identity.bits := huIdentity.get
  }
  io.status.privState := privState
  io.status.fpState.frm := fcsr.frm
  io.status.fpState.off := mstatus.regOut.FS === ContextStatus.Off
  io.status.vecState.vstart := vstart.rdata.asUInt
  io.status.vecState.vxsat := vcsr.vxsat
  io.status.vecState.vxrm := vcsr.vxrm
  io.status.vecState.vcsr := vcsr.rdata.asUInt
  io.status.vecState.vl := vl.rdata.asUInt
  io.status.vecState.vtype := vtype.rdata.asUInt // Todo: check correct
  io.status.vecState.vlenb := vlenb.rdata.asUInt
  io.status.vecState.off := mstatus.regOut.VS === ContextStatus.Off
  io.status.interrupt := intrMod.io.out.interruptVec.valid
  io.status.wfiEvent := debugIntr || (mie.rdata.asUInt & mip.rdata.asUInt).orR
  io.status.debugMode := debugMode
  io.status.singleStepFlag := !debugMode && dcsr.regOut.STEP

  /**
   * debug_begin
   */
  val tdata1Selected = Wire(new Tdata1Bundle)
  tdata1Selected := tdata1.rdata
  val dmodeInSelectedTrigger = tdata1Selected.DMODE.asBool
  val triggerCanWrite = dmodeInSelectedTrigger && debugMode || !dmodeInSelectedTrigger
  val tdata1Update  = tdata1.w.wen && triggerCanWrite
  val tdata2Update  = tdata2.w.wen && triggerCanWrite
  val tdata1Vec = tdata1RegVec.map{ mod => {
    val tdata1Wire = Wire(new Tdata1Bundle)
    tdata1Wire := mod.rdata
    tdata1Wire
  }}

  val triggerCanRaiseBpExp = !(privState.isModeM && !mstatus.regOut.MIE ||
    medeleg.regOut.EX_BP && privState.isModeHS && !mstatus.sstatus.SIE ||
    medeleg.regOut.EX_BP && hedeleg.regOut.EX_BP && privState.isModeVS && !vsstatus.regOut.SIE)

  val debugMod = Module(new Debug)
  debugMod.io.in.trapInfo.valid            := hasTrap
  debugMod.io.in.trapInfo.bits.trapVec     := trapVec.asUInt
  debugMod.io.in.trapInfo.bits.isDebugIntr := debug
  debugMod.io.in.trapInfo.bits.isInterrupt := trapIsInterrupt
  debugMod.io.in.trapInfo.bits.trigger     := trigger
  debugMod.io.in.trapInfo.bits.singleStep  := singleStep
  val acceptedCriticalDebug = useAcceptedInterrupt &&
    deliveredInterrupt.map(_.criticalDebug).getOrElse(false.B)
  debugMod.io.in.trapInfo.bits.criticalErrorState := criticalErrorState &&
    (!deferredCriticalDebug.getOrElse(false.B) || acceptedCriticalDebug)
  debugMod.io.in.privState                 := privState
  debugMod.io.in.debugMode                 := debugMode
  debugMod.io.in.dcsr                      := dcsr.regOut
  debugMod.io.in.tselect                   := tselect.regOut
  debugMod.io.in.tdata1Vec                 := tdata1Vec
  debugMod.io.in.tdata1Selected            := tdata1.rdata
  debugMod.io.in.tdata2Selected            := tdata2.rdata
  debugMod.io.in.tdata1Update              := tdata1Update
  debugMod.io.in.tdata2Update              := tdata2Update
  debugMod.io.in.tdata1Wdata               := wdata
  debugMod.io.in.triggerCanRaiseBpExp      := triggerCanRaiseBpExp

  if (HasFDI) {
    when(huBusy.get && (huFinished.get || huTrapFinished.get) &&
      criticalErrorState && dcsr.regOut.CETRIG.asBool && !debugMode) {
      deferredCriticalDebug.get := true.B
    }.elsewhen(entryDebugMode && acceptedCriticalDebug) {
      deferredCriticalDebug.get := false.B
    }
    huTakeDebug := huBusy.get && !huFinished.get && huPcValid.get && !huTrapFinished.get &&
      !huCanceled.get && !huCancelNow && !hasTrap &&
      (huDebugPending.get || debugMod.io.out.criticalErrorStateEnterDebug) && !debugMode
    // Critical debug takes ownership immediately, but its architectural entry waits
    // for the reserved instruction's real PC. Other accepted traps keep their PC.
    entryDebugMode := !debugMode && Mux(huBusy.get && !hasTrap, huTakeDebug, debugMod.io.out.hasDebugTrap)
    when(huTakeDebug) {
      trapEntryDEvent.in.trapPc := huSaved.get.pc
    }
  } else {
    entryDebugMode := debugMod.io.out.hasDebugTrap && !debugMode
  }

  trapEntryDEvent.valid                           := entryDebugMode
  trapEntryDEvent.in.hasDebugIntr                 := debugMod.io.out.hasDebugIntr && !acceptedCriticalDebug
  trapEntryDEvent.in.debugMode                    := debugMode
  trapEntryDEvent.in.hasTrap                      := hasTrap
  trapEntryDEvent.in.hasSingleStep                := debugMod.io.out.hasSingleStep
  trapEntryDEvent.in.triggerEnterDebugMode        := debugMod.io.out.triggerEnterDebugMode
  trapEntryDEvent.in.hasDebugEbreakException      := debugMod.io.out.hasDebugEbreakException
  trapEntryDEvent.in.breakPoint                   := debugMod.io.out.breakPoint
  trapEntryDEvent.in.criticalErrorStateEnterDebug := debugMod.io.out.criticalErrorStateEnterDebug || acceptedCriticalDebug

  for(idx <- 0 until TriggerNum) {
    val tdata1Pre = Wire(new Tdata1Bundle)
    val mcontrol6Pre = Wire(new Mcontrol6)
    tdata1Pre := (if (idx > 0) tdata1RegVec(idx - 1) else tdata1RegVec(idx)).rdata.asUInt
    mcontrol6Pre := tdata1Pre.DATA.asUInt
    val canWriteDmode = WireInit(false.B)
    canWriteDmode := (if(idx > 0) (Mux(mcontrol6Pre.CHAIN.asBool, tdata1Pre.DMODE.asBool && tdata1Pre.TYPE.isLegal, true.B)) && debugMode else debugMode).asBool
    tdata1RegVec(idx) match {
      case m: HasTriggerBundle =>
        m.canWriteDmode := canWriteDmode
        m.chainable := debugMod.io.out.newTriggerChainIsLegal
      case _ =>
    }
  }

  tdata1RegVec.zip(tdata2RegVec).zipWithIndex.map { case ((mod1, mod2), idx) => {
    mod1.w.wen    := tdata1Update && (tselect.rdata === idx.U)
    mod1.w.wdata  := wdata
    mod2.w.wen    := tdata2Update && (tselect.rdata === idx.U)
    mod2.w.wdata  := wdata
  }}

  triggerFrontendChange := debugMod.io.out.triggerFrontendChange

  io.status.frontendTrigger := debugMod.io.out.frontendTrigger
  io.status.memTrigger      := debugMod.io.out.memTrigger
  /**
   * debug_end
   */

  // trace
  val privForTrace = Mux(debugMode,
    Priv.D,
    Mux1H(
      Seq(privState.isModeM, privState.isModeHS, privState.isModeVS, privState.isModeHU, privState.isModeVU),
      Seq(Priv.M,            Priv.HS,            Priv.VS,            Priv.HU,            Priv.VU)
    )
  )
  val xret = legalDret || legalMNret || legalMret || legalSret || userReturnEffect
  val currentPriv = privForTrace
  val lastPriv = RegEnable(privForTrace, Priv.M, (xret || hasTrap || userEntryEffect || huTakeDebug))

  io.status.traceCSR.lastPriv       := lastPriv
  io.status.traceCSR.currentPriv    := privForTrace
  val traceCause = Mux1H(Seq(
    privState.isModeM -> mcause.rdata,
    privState.isModeHS -> scause.rdata,
    privState.isModeVS -> vscause.rdata) ++
    userTimerCSROutMap.get(UserTimerCSRAddress.ucause).toSeq.map(data => isModeHU -> data))
  val traceTval = Mux1H(Seq(
    privState.isModeM -> mtval.rdata,
    privState.isModeHS -> stval.rdata,
    privState.isModeVS -> vstval.rdata) ++
    userTimerCSROutMap.get(UserTimerCSRAddress.utval).toSeq.map(data => isModeHU -> data))
  io.status.traceCSR.cause := Mux(userEntryEffect, "h8000000000000004".U, traceCause)
  io.status.traceCSR.tval := Mux(userEntryEffect, 0.U, traceTval)
  
  /**
   * perf_begin
   * perf number: 29 (frontend 8, ctrlblock 8, memblock 8, huancun 5)
   */
  val csrevents = mhpmevents.slice(24, 29).map(_.rdata)

  val hcEvents = Wire(Vec(numPCntHc * coreParams.L2NBanks, new PerfEvent))
  for (i <- 0 until numPCntHc * coreParams.L2NBanks) {
    hcEvents(i) := io.perf.perfEventsHc(i)
  }

  val hpmHc = HPerfMonitor(csrevents, hcEvents)
  val allPerfEvents = io.perf.perfEventsFrontend ++
    io.perf.perfEventsBackend ++
    io.perf.perfEventsLsu ++
    hpmHc.getPerf

  val countingEn        = RegInit(0.U.asTypeOf(Vec(perfCntNum, Bool())))
  val ofFromPerfCntVec  = Wire(Vec(perfCntNum, Bool()))
  val lcofiReqVec       = Wire(Vec(perfCntNum, Bool()))
  
  for(i <- 0 until perfCntNum) {
    mhpmcounters(i) match {
      case m: HasPerfCounterBundle =>
        m.countingEn        := countingEn(i)
        m.perf              := allPerfEvents(i)
        ofFromPerfCntVec(i) := m.toMhpmeventOF
      case _ =>
    }

    mhpmevents(i) match {
      case m: HasOfFromPerfCntBundle =>
        m.ofFromPerfCnt := ofFromPerfCntVec(i)
      case _ =>
    }
    
    val mhpmevent = Wire(new MhpmeventBundle)
    mhpmevent := mhpmevents(i).rdata
    lcofiReqVec(i) := ofFromPerfCntVec(i) && !mhpmevent.OF.asBool

    countingEn(i) := (privState.isModeM && !mhpmevent.MINH) ||
      (privState.isModeHS && !mhpmevent.SINH)  ||
      (privState.isModeHU && !mhpmevent.UINH)  ||
      (privState.isModeVS && !mhpmevent.VSINH) ||
      (privState.isModeVU && !mhpmevent.VUINH)
  }

  val lcofiReq = lcofiReqVec.asUInt.orR
  mip match {
    case m: HasLocalInterruptReqBundle =>
      m.lcofiReq := lcofiReq
    case _ =>
  }
  /**
   * perf_end
   */

  /**
   * [[io.status.custom]] connection
   */
  io.status.custom.pf_ctrl.l1I_pf_enable           := spfctl.regOut.L1I_PF_ENABLE.asBool
  io.status.custom.pf_ctrl.l2_pf_enable            := spfctl.regOut.L2_PF_ENABLE.asBool
  io.status.custom.pf_ctrl.l1D_pf_enable           := spfctl.regOut.L1D_PF_ENABLE.asBool
  io.status.custom.pf_ctrl.l1D_pf_train_on_hit     := spfctl.regOut.L1D_PF_TRAIN_ON_HIT.asBool
  io.status.custom.pf_ctrl.l1D_pf_enable_agt       := spfctl.regOut.L1D_PF_ENABLE_AGT.asBool
  io.status.custom.pf_ctrl.l1D_pf_enable_pht       := spfctl.regOut.L1D_PF_ENABLE_PHT.asBool
  io.status.custom.pf_ctrl.l1D_pf_active_threshold := spfctl.regOut.L1D_PF_ACTIVE_THRESHOLD.asUInt
  io.status.custom.pf_ctrl.l1D_pf_active_stride    := spfctl.regOut.L1D_PF_ACTIVE_STRIDE.asUInt
  io.status.custom.pf_ctrl.l1D_pf_enable_stride    := spfctl.regOut.L1D_PF_ENABLE_STRIDE.asBool
  io.status.custom.pf_ctrl.l2_pf_store_only        := spfctl.regOut.L2_PF_STORE_ONLY.asBool
  io.status.custom.pf_ctrl.l2_pf_recv_enable       := spfctl.regOut.L2_PF_RECV_ENABLE.asBool
  io.status.custom.pf_ctrl.l2_pf_pbop_enable       := spfctl.regOut.L2_PF_PBOP_ENABLE.asBool
  io.status.custom.pf_ctrl.l2_pf_vbop_enable       := spfctl.regOut.L2_PF_VBOP_ENABLE.asBool
  io.status.custom.pf_ctrl.l2_pf_tp_enable         := spfctl.regOut.L2_PF_TP_ENABLE.asBool

  io.status.custom.lvpred_disable          := slvpredctl.regOut.LVPRED_DISABLE.asBool
  io.status.custom.no_spec_load            := slvpredctl.regOut.NO_SPEC_LOAD.asBool
  io.status.custom.storeset_wait_store     := slvpredctl.regOut.STORESET_WAIT_STORE.asBool
  io.status.custom.storeset_no_fast_wakeup := slvpredctl.regOut.STORESET_NO_FAST_WAKEUP.asBool
  io.status.custom.lvpred_timeout          := slvpredctl.regOut.LVPRED_TIMEOUT.asUInt

  io.status.custom.bp_ctrl.ubtb_enable     := sbpctl.regOut.UBTB_ENABLE .asBool
  io.status.custom.bp_ctrl.btb_enable      := sbpctl.regOut.BTB_ENABLE  .asBool
  io.status.custom.bp_ctrl.bim_enable      := sbpctl.regOut.BIM_ENABLE  .asBool
  io.status.custom.bp_ctrl.tage_enable     := sbpctl.regOut.TAGE_ENABLE .asBool
  io.status.custom.bp_ctrl.sc_enable       := sbpctl.regOut.SC_ENABLE   .asBool
  io.status.custom.bp_ctrl.ras_enable      := sbpctl.regOut.RAS_ENABLE  .asBool
  io.status.custom.bp_ctrl.loop_enable     := sbpctl.regOut.LOOP_ENABLE .asBool

  io.status.custom.sbuffer_threshold                := smblockctl.regOut.SBUFFER_THRESHOLD.asUInt
  io.status.custom.ldld_vio_check_enable            := smblockctl.regOut.LDLD_VIO_CHECK_ENABLE.asBool
  io.status.custom.soft_prefetch_enable             := smblockctl.regOut.SOFT_PREFETCH_ENABLE.asBool
  io.status.custom.cache_error_enable               := smblockctl.regOut.CACHE_ERROR_ENABLE.asBool
  io.status.custom.uncache_write_outstanding_enable := smblockctl.regOut.UNCACHE_WRITE_OUTSTANDING_ENABLE.asBool
  io.status.custom.hd_misalign_st_enable            := smblockctl.regOut.HD_MISALIGN_ST_ENABLE.asBool
  io.status.custom.hd_misalign_ld_enable            := smblockctl.regOut.HD_MISALIGN_LD_ENABLE.asBool

  io.status.custom.fusion_enable           := srnctl.regOut.FUSION_ENABLE.asBool
  io.status.custom.wfi_enable              := srnctl.regOut.WFI_ENABLE.asBool && (!io.status.singleStepFlag) && !debugMode

  io.status.custom.power_down_enable := mcorepwr.regOut.POWER_DOWN_ENABLE.asBool

  io.status.custom.flush_l2_enable := mflushpwr.regOut.FLUSH_L2_ENABLE.asBool

  io.status.instrAddrTransType.bare := privState.isModeM ||
    (!privState.isVirtual && satp.regOut.MODE === SatpMode.Bare) ||
    (privState.isVirtual && vsatp.regOut.MODE === SatpMode.Bare && hgatp.regOut.MODE === HgatpMode.Bare)
  io.status.instrAddrTransType.sv39 := !privState.isModeM && !privState.isVirtual && satp.regOut.MODE === SatpMode.Sv39 ||
    privState.isVirtual && vsatp.regOut.MODE === SatpMode.Sv39
  io.status.instrAddrTransType.sv48 := !privState.isModeM && !privState.isVirtual && satp.regOut.MODE === SatpMode.Sv48 ||
    privState.isVirtual && vsatp.regOut.MODE === SatpMode.Sv48
  io.status.instrAddrTransType.sv39x4 := privState.isVirtual && vsatp.regOut.MODE === SatpMode.Bare && hgatp.regOut.MODE === HgatpMode.Sv39x4
  io.status.instrAddrTransType.sv48x4 := privState.isVirtual && vsatp.regOut.MODE === SatpMode.Bare && hgatp.regOut.MODE === HgatpMode.Sv48x4
  assert(PopCount(io.status.instrAddrTransType.asUInt) === 1.U, "Exactly one inst trans type should be asserted")

  // A bank read must not become an indirect AIA request when the live address changes.
  private val csrAccess = wenLegalReg || RegNext(ren && !userTimerAddressHit && !fdiAddressHit)

  private val imsicAddrValid =
    csrAccess &&  addr === CSRs.mireg.U &&  miselect.inIMSICRange ||
    csrAccess &&  addr === CSRs.sireg.U && !isModeVS && siselect.inIMSICRange ||
    csrAccess && (addr === CSRs.sireg.U &&  isModeVS || addr === CSRs.vsireg.U) && vsiselect.inIMSICRange

  private val imsicAddr = Mux1H(Seq(
    (csrAccess &&  addr === CSRs.mireg.U) -> miselect.rdata,
    (csrAccess &&  addr === CSRs.sireg.U && !isModeVS) -> siselect.rdata,
    (csrAccess && (addr === CSRs.sireg.U &&  isModeVS || addr === CSRs.vsireg.U)) -> vsiselect.rdata,
  ))

  private val imsicAddrPrivState = Mux1H(Seq(
    (csrAccess &&  addr === CSRs.mireg.U) -> PrivState.ModeM,
    (csrAccess &&  addr === CSRs.sireg.U && !isModeVS) -> PrivState.ModeHS,
    (csrAccess && (addr === CSRs.sireg.U &&  isModeVS || addr === CSRs.vsireg.U)) -> PrivState.ModeVS,
  ))

  private val imsicWdataValid =
    mireg.w.wen  && miselect.inIMSICRange ||
    sireg.w.wen  && siselect.inIMSICRange ||
    vsireg.w.wen && vsiselect.inIMSICRange

  toAIA.addr.valid     := imsicAddrValid
  toAIA.addr.bits.addr := imsicAddr
  toAIA.addr.bits.prvm := imsicAddrPrivState.PRVM
  toAIA.addr.bits.v    := imsicAddrPrivState.V

  toAIA.wdata.valid := imsicWdataValid
  toAIA.wdata.bits.op := RegNext(io.in.bits.op)
  toAIA.wdata.bits.data := RegNext(io.in.bits.src)
  toAIA.vgein := hstatus.regOut.VGEIN.asUInt
  toAIA.mClaim  := mtopei.w.wen
  toAIA.sClaim  := stopei.w.wen
  toAIA.vsClaim := vstopei.w.wen

  // tlb
  io.tlb.satpASIDChanged  := GatedValidRegNext(satp.w.wen  && satp .regOut.ASID =/=  satp.w.wdataFields.ASID)
  io.tlb.vsatpASIDChanged := GatedValidRegNext(vsatp.w.wen && vsatp.regOut.ASID =/= vsatp.w.wdataFields.ASID)
  io.tlb.hgatpVMIDChanged := GatedValidRegNext(hgatp.w.wen && hgatp.regOut.VMID =/= hgatp.w.wdataFields.VMID)
  io.tlb.satp := satp.rdata
  io.tlb.vsatp := vsatp.rdata
  io.tlb.hgatp := hgatp.rdata
  if (HasBitmapCheck) {
    io.tlb.mbmc := mbmc.get.rdata
  } else {
    io.tlb.mbmc := DontCare
  }
  io.tlb.mxr  :=  mstatus.regOut.MXR.asBool
  io.tlb.sum  :=  mstatus.regOut.SUM.asBool
  io.tlb.vmxr := vsstatus.regOut.MXR.asBool
  io.tlb.vsum := vsstatus.regOut.SUM.asBool
  io.tlb.spvp :=  hstatus.regOut.SPVP.asBool

  io.tlb.imode := PRVM.asUInt
  // when NMIE is zero, force to behave as MPRV is zero
  io.tlb.dmode := Mux(
    (debugMode && dcsr.regOut.MPRVEN || !debugMode) && mstatus.regOut.MPRV && mnstatus.regOut.NMIE,
    mstatus.regOut.MPP.asUInt,
    PRVM.asUInt
  )
  io.tlb.dvirt := Mux(
    (debugMode && dcsr.regOut.MPRVEN || !debugMode) && mstatus.regOut.MPRV && mnstatus.regOut.NMIE && mstatus.regOut.MPP =/= PrivMode.M,
    mstatus.regOut.MPV.asUInt,
    V.asUInt
  )
  io.tlb.mPBMTE := RegNext(menvcfg.regOut.PBMTE.asBool)
  io.tlb.hPBMTE := RegNext(henvcfg.regOut.PBMTE.asBool)
  io.tlb.pmm.mseccfg := RegNext(mseccfg.regOut.PMM.asUInt)
  io.tlb.pmm.menvcfg := RegNext(menvcfg.regOut.PMM.asUInt)
  io.tlb.pmm.henvcfg := RegNext(henvcfg.regOut.PMM.asUInt)
  io.tlb.pmm.hstatus := RegNext(hstatus.regOut.HUPMM.asUInt)
  io.tlb.pmm.senvcfg := RegNext(senvcfg.regOut.PMM.asUInt)

  io.toDecode.illegalInst.sfenceVMA  := isModeHS && mstatus.regOut.TVM  || isModeHU
  io.toDecode.virtualInst.sfenceVMA  := isModeVS && hstatus.regOut.VTVM || isModeVU
  io.toDecode.illegalInst.sfencePart := isModeHU
  io.toDecode.virtualInst.sfencePart := isModeVU
  io.toDecode.illegalInst.hfenceGVMA := isModeHS && mstatus.regOut.TVM || isModeHU
  io.toDecode.illegalInst.hfenceVVMA := isModeHU
  io.toDecode.virtualInst.hfence     := isModeVS || isModeVU
  io.toDecode.illegalInst.hlsv       := isModeHU && !hstatus.regOut.HU
  io.toDecode.virtualInst.hlsv       := isModeVS || isModeVU
  io.toDecode.illegalInst.fsIsOff    := mstatus.regOut.FS === ContextStatus.Off || (isModeVS || isModeVU) && vsstatus.regOut.FS === ContextStatus.Off
  io.toDecode.illegalInst.vsIsOff    := mstatus.regOut.VS === ContextStatus.Off || (isModeVS || isModeVU) && vsstatus.regOut.VS === ContextStatus.Off
  io.toDecode.illegalInst.wfi        := isModeHU || !isModeM && mstatus.regOut.TW
  io.toDecode.virtualInst.wfi        := isModeVS && !mstatus.regOut.TW && hstatus.regOut.VTW || isModeVU && !mstatus.regOut.TW
  io.toDecode.illegalInst.wrs_nto    := !isModeM && mstatus.regOut.TW
  io.toDecode.virtualInst.wrs_nto    := privState.V && !mstatus.regOut.TW && hstatus.regOut.VTW
  io.toDecode.illegalInst.frm        := frmIsReserved
  // Ref: The RISC-V Instruction Set Manual Volume I - 20.5. Control and Status Register State
  io.toDecode.illegalInst.cboZ       := !isModeM && !menvcfg.regOut.CBZE || isModeHU && !senvcfg.regOut.CBZE
  io.toDecode.virtualInst.cboZ       := menvcfg.regOut.CBZE && (
    isModeVS && !henvcfg.regOut.CBZE ||
    isModeVU && !(henvcfg.regOut.CBZE && senvcfg.regOut.CBZE)
  )
  io.toDecode.illegalInst.cboCF      := !isModeM && !menvcfg.regOut.CBCFE || isModeHU && !senvcfg.regOut.CBCFE
  io.toDecode.virtualInst.cboCF      := menvcfg.regOut.CBCFE && (
    isModeVS && !henvcfg.regOut.CBCFE ||
    isModeVU && !(henvcfg.regOut.CBCFE && senvcfg.regOut.CBCFE)
  )
  io.toDecode.illegalInst.cboI       :=
    !isModeM && menvcfg.regOut.CBIE === EnvCBIE.Off ||
    isModeHU && senvcfg.regOut.CBIE === EnvCBIE.Off
  io.toDecode.virtualInst.cboI       := menvcfg.regOut.CBIE =/= EnvCBIE.Off && (
    isModeVS && henvcfg.regOut.CBIE === EnvCBIE.Off ||
    isModeVU &&(henvcfg.regOut.CBIE === EnvCBIE.Off || senvcfg.regOut.CBIE === EnvCBIE.Off)
  )
  io.toDecode.special.cboI2F := !io.toDecode.illegalInst.cboI && !io.toDecode.virtualInst.cboI && (
    menvcfg.regOut.CBIE === EnvCBIE.Flush && !isModeM ||
    senvcfg.regOut.CBIE === EnvCBIE.Flush && (isModeHU || isModeVU) ||
    henvcfg.regOut.CBIE === EnvCBIE.Flush && (isModeVS || isModeVU)
  )

  io.distributedWenLegal := wenLegalReg && !noCSRIllegalReg
  io.status.criticalErrorState := criticalErrorState && !dcsr.regOut.CETRIG.asBool

  val criticalErrors = Seq(
    ("csr_dbltrp_inMN", !mnstatus.regOut.NMIE && hasTrap && !entryDebugMode),
  )
  criticalErrorStateInCSR := criticalErrors.map(criticalError => criticalError._2).reduce(_ || _).asBool
  generateCriticalErrors()

  // Always instantiate basic difftest modules.
  if (env.AlwaysBasicDiff || env.EnableDifftest) {
    // Delay trap passed to difftest until VecExcpMod is not busy
    val architecturalTrap = hasTrap || userEntryEffect
    val pendingTrap = RegInit(false.B)
    when (architecturalTrap) {
      pendingTrap := true.B
    }.elsewhen (!io.fromVecExcpMod.busy) {
      pendingTrap := false.B
    }

    val hartId = io.fromTop.hartId
    val trapValid = pendingTrap && !io.fromVecExcpMod.busy
    val acceptedHvictlIID = Mux(useAcceptedInterrupt,
      deliveredInterrupt.map(_.hvictlIID).getOrElse(0.U), hvictl.regOut.IID.asUInt)
    val trapNO = Mux(userEntryEffect, 4.U, Mux(virtualInterruptIsHvictlInject && hasTrap,
      acceptedHvictlIID, trapHandleMod.io.out.causeNO.ExceptionCode.asUInt))
    val interrupt = userEntryEffect || trapHandleMod.io.out.causeNO.Interrupt.asBool
    val hasNMI = nmi && hasTrap
    val interruptNO = Mux(interrupt, trapNO, 0.U)
    val exceptionNO = Mux(!interrupt, trapNO, 0.U)
    val isSv39: Bool =
      (isModeHS || isModeHU) &&  satp.regOut.MODE === SatpMode.Sv39 ||
      (isModeVS || isModeVU) && vsatp.regOut.MODE === SatpMode.Sv39
    val isSv48: Bool =
      (isModeHS || isModeHU) &&  satp.regOut.MODE === SatpMode.Sv48 ||
      (isModeVS || isModeVU) && vsatp.regOut.MODE === SatpMode.Sv48
    val isBare = !isSv39 && !isSv48
    val sv39PC = SignExt(trapPC.take(39), XLEN)
    val sv48PC = SignExt(trapPC.take(48), XLEN)
    val barePC = ZeroExt(trapPC.take(PAddrBits), XLEN)
    // When enable virtual memory, the higher bit should fill with the msb of address of Sv39/Sv48/Sv57
    val regularExceptionPC = Mux1H(Seq(
      isSv39 -> sv39PC,
      isSv48 -> sv48PC,
      isBare -> barePC,
    ))
    val exceptionPC = Mux(userEntryEffect, huSaved.map(_.pc).getOrElse(0.U), regularExceptionPC)

    val diffArchEvent = DifftestModule(new DiffArchEvent, delay = 3, dontCare = true)
    diffArchEvent.coreid := hartId
    diffArchEvent.valid := trapValid
    diffArchEvent.interrupt := RegEnable(interruptNO, architecturalTrap)
    diffArchEvent.exception := RegEnable(exceptionNO, architecturalTrap)
    diffArchEvent.exceptionPC := RegEnable(exceptionPC, architecturalTrap)
    diffArchEvent.hasNMI := RegEnable(hasNMI, architecturalTrap)
    diffArchEvent.virtualInterruptIsHvictlInject := RegNext(virtualInterruptIsHvictlInject && hasTrap)
    diffArchEvent.irToHS := RegEnable(irToHS, architecturalTrap)
    diffArchEvent.irToVS := RegEnable(irToVS, architecturalTrap)
    // The simulation sequence pairs each delivered trap with its post-effect snapshot.
    // Production configurations retain constants and no additional event counter.
    if (env.EnableDifftest && !env.FPGAPlatform) {
      val eventSequence = RegInit(0.U(64.W))
      when(architecturalTrap) { eventSequence := eventSequence + 1.U }
      diffArchEvent.irToHU := RegEnable(userEntryEffect, false.B, architecturalTrap)
      diffArchEvent.eventSeq := RegEnable(eventSequence + 1.U, 0.U(64.W), architecturalTrap)
    } else {
      diffArchEvent.irToHU := false.B
      diffArchEvent.eventSeq := 0.U
    }
    if (env.EnableDifftest) {
      diffArchEvent.exceptionInst := RegEnable(Mux(userEntryEffect, 0.U, io.fromRob.trap.bits.instr), architecturalTrap)
    }

    val diffCriticalErrorEvent = DifftestModule(new DiffCriticalErrorEvent, delay = 4, dontCare = true)
    diffCriticalErrorEvent.valid := io.status.criticalErrorState && trapValid
    diffCriticalErrorEvent.coreid := hartId
    diffCriticalErrorEvent.criticalError := io.status.criticalErrorState

    val diffCSRState = DifftestModule(new DiffCSRState)
    diffCSRState.coreid         := hartId
    diffCSRState.privilegeMode  := privState.PRVM.asUInt
    diffCSRState.mstatus        := mstatus.rdata.asUInt
    diffCSRState.sstatus        := mstatus.sstatus.asUInt
    diffCSRState.mepc           := mepc.rdata.asUInt
    diffCSRState.sepc           := sepc.rdata.asUInt
    diffCSRState.mtval          := mtval.rdata.asUInt
    diffCSRState.stval          := stval.rdata.asUInt
    diffCSRState.mtvec          := mtvec.rdata.asUInt
    diffCSRState.stvec          := stvec.rdata.asUInt
    diffCSRState.mcause         := mcause.rdata.asUInt
    diffCSRState.scause         := scause.rdata.asUInt
    diffCSRState.satp           := satp.rdata.asUInt
    diffCSRState.mip            := mip.rdata.asUInt
    diffCSRState.mie            := mie.rdata.asUInt
    diffCSRState.mscratch       := mscratch.rdata.asUInt
    diffCSRState.sscratch       := sscratch.rdata.asUInt
    diffCSRState.mideleg        := mideleg.rdata.asUInt
    diffCSRState.medeleg        := medeleg.rdata.asUInt

    val diffDebugMode = DifftestModule(new DiffDebugMode)
    diffDebugMode.coreid    := hartId
    diffDebugMode.debugMode := debugMode
    diffDebugMode.dcsr      := dcsr.rdata.asUInt
    diffDebugMode.dpc       := dpc.rdata.asUInt
    diffDebugMode.dscratch0 := dscratch0.rdata.asUInt
    diffDebugMode.dscratch1 := dscratch1.rdata.asUInt

    val diffTriggerCSRState = DifftestModule(new DiffTriggerCSRState)
    diffTriggerCSRState.coreid    := hartId
    diffTriggerCSRState.tselect   := tselect.rdata
    diffTriggerCSRState.tdata1    := tdata1.rdata
    diffTriggerCSRState.tinfo     := tinfo.rdata

    val diffVecCSRState = DifftestModule(new DiffVecCSRState)
    diffVecCSRState.coreid := hartId
    diffVecCSRState.vstart := vstart.rdata.asUInt
    diffVecCSRState.vxsat := vcsr.vxsat.asUInt
    diffVecCSRState.vxrm := vcsr.vxrm.asUInt
    diffVecCSRState.vcsr := vcsr.rdata.asUInt
    diffVecCSRState.vl := RegNext(io.fromRob.commit.vl)
    diffVecCSRState.vtype := vtype.rdata.asUInt
    diffVecCSRState.vlenb := vlenb.rdata.asUInt

    val diffFpCSRState = DifftestModule(new DiffFpCSRState)
    diffFpCSRState.coreid := hartId
    diffFpCSRState.fcsr := fcsr.rdata.asUInt

    val diffHCSRState = DifftestModule(new DiffHCSRState)
    diffHCSRState.coreid      := hartId
    diffHCSRState.virtMode    := privState.V.asBool
    diffHCSRState.mtval2      := mtval2.rdata.asUInt
    diffHCSRState.mtinst      := mtinst.rdata.asUInt
    diffHCSRState.hstatus     := hstatus.rdata.asUInt
    diffHCSRState.hideleg     := hideleg.rdata.asUInt
    diffHCSRState.hedeleg     := hedeleg.rdata.asUInt
    diffHCSRState.hcounteren  := hcounteren.rdata.asUInt
    diffHCSRState.htval       := htval.rdata.asUInt
    diffHCSRState.htinst      := htinst.rdata.asUInt
    diffHCSRState.hgatp       := hgatp.rdata.asUInt
    diffHCSRState.vsstatus    := vsstatus.rdata.asUInt
    diffHCSRState.vstvec      := vstvec.rdata.asUInt
    diffHCSRState.vsepc       := vsepc.rdata.asUInt
    diffHCSRState.vscause     := vscause.rdata.asUInt
    diffHCSRState.vstval      := vstval.rdata.asUInt
    diffHCSRState.vsatp       := vsatp.rdata.asUInt
    diffHCSRState.vsscratch   := vsscratch.rdata.asUInt

    val platformIRPMeipChange = !platformIRP.MEIP &&  RegNext(platformIRP.MEIP) || platformIRP.MEIP && !RegNext(platformIRP.MEIP)
    val platformIRPMtipChange = !platformIRP.MTIP &&  RegNext(platformIRP.MTIP) || platformIRP.MTIP && !RegNext(platformIRP.MTIP)
    val platformIRPMsipChange = !platformIRP.MSIP &&  RegNext(platformIRP.MSIP) || platformIRP.MSIP && !RegNext(platformIRP.MSIP)
    val platformIRPSeipChange = !platformIRP.SEIP &&  RegNext(platformIRP.SEIP) || platformIRP.SEIP && !RegNext(platformIRP.SEIP)
    val platformIRPStipChange = !sstcIRGen.o.STIP &&  RegNext(sstcIRGen.o.STIP) || sstcIRGen.o.STIP && !RegNext(sstcIRGen.o.STIP)
    val platformIRPVseipChange = !platformIRP.VSEIP &&  RegNext(platformIRP.VSEIP) ||
                                  platformIRP.VSEIP && !RegNext(platformIRP.VSEIP) ||
                                 !hgeip.rdata.asUInt(hstatus.regOut.VGEIN.asUInt) &&  RegNext(hgeip.rdata.asUInt(hstatus.regOut.VGEIN.asUInt)) ||
                                  hgeip.rdata.asUInt(hstatus.regOut.VGEIN.asUInt) && !RegNext(hgeip.rdata.asUInt(hstatus.regOut.VGEIN.asUInt))
    val platformIRPVstipChange = !sstcIRGen.o.VSTIP && RegNext(sstcIRGen.o.VSTIP) || sstcIRGen.o.VSTIP && !RegNext(sstcIRGen.o.VSTIP)
    val fromAIAMeipChange = !fromAIA.meip && RegNext(fromAIA.meip) || fromAIA.meip && !RegNext(fromAIA.meip)
    val fromAIASeipChange = !fromAIA.seip && RegNext(fromAIA.seip) || fromAIA.seip && !RegNext(fromAIA.seip)
    val lcofiReqChange = !lcofiReq && RegNext(lcofiReq) || lcofiReq && !RegNext(lcofiReq)

    val diffNonRegInterruptPendingEvent = DifftestModule(new DiffNonRegInterruptPendingEvent)
    diffNonRegInterruptPendingEvent.coreid           := hartId
    diffNonRegInterruptPendingEvent.valid            := (platformIRPMeipChange || platformIRPMtipChange || platformIRPMsipChange ||
                                                        platformIRPSeipChange || platformIRPStipChange ||
                                                        platformIRPVseipChange || platformIRPVstipChange ||
                                                        fromAIAMeipChange || fromAIASeipChange ||
                                                        lcofiReqChange || RegNext(reset.asBool)) & !reset.asBool
    diffNonRegInterruptPendingEvent.platformIRPMeip  := platformIRP.MEIP
    diffNonRegInterruptPendingEvent.platformIRPMtip  := platformIRP.MTIP
    diffNonRegInterruptPendingEvent.platformIRPMsip  := platformIRP.MSIP
    diffNonRegInterruptPendingEvent.platformIRPSeip  := platformIRP.SEIP
    diffNonRegInterruptPendingEvent.platformIRPStip  := sstcIRGen.o.STIP
    diffNonRegInterruptPendingEvent.platformIRPVseip := platformIRP.VSEIP || hgeip.rdata.asUInt(hstatus.regOut.VGEIN.asUInt)
    diffNonRegInterruptPendingEvent.platformIRPVstip := sstcIRGen.o.VSTIP
    diffNonRegInterruptPendingEvent.fromAIAMeip := fromAIA.meip
    diffNonRegInterruptPendingEvent.fromAIASeip := fromAIA.seip
    diffNonRegInterruptPendingEvent.localCounterOverflowInterruptReq  := mip.regOut.LCOFIP.asBool

    val diffMhpmeventOverflowEvent = DifftestModule(new DiffMhpmeventOverflowEvent)
    diffMhpmeventOverflowEvent.coreid := hartId
    diffMhpmeventOverflowEvent.valid  := Cat(mhpmevents.zipWithIndex.map{ case (event, i) =>
      !ofFromPerfCntVec(i) && RegNext(ofFromPerfCntVec(i)) || ofFromPerfCntVec(i) && !RegNext(ofFromPerfCntVec(i))
    }).orR
    diffMhpmeventOverflowEvent.mhpmeventOverflow := VecInit(mhpmevents.map(_.regOut.asInstanceOf[MhpmeventBundle].OF.asBool)).asUInt

    val mtopeiChange = RegNext(fromAIA.mtopei.asUInt) =/= fromAIA.mtopei.asUInt
    val stopeiChange = RegNext(fromAIA.stopei.asUInt) =/= fromAIA.stopei.asUInt
    val vstopeiChange = RegNext(hstatus.regOut.VGEIN.asUInt) =/= hstatus.regOut.VGEIN.asUInt
    val hgeipChange = RegNext(fromAIA.vseip) =/= fromAIA.vseip

    val diffSyncAIAEvent = DifftestModule(new DiffSyncAIAEvent)
    diffSyncAIAEvent.coreid := hartId
    diffSyncAIAEvent.valid := mtopeiChange || stopeiChange || vstopeiChange || hgeipChange
    diffSyncAIAEvent.mtopei := mtopei.rdata
    diffSyncAIAEvent.stopei := stopei.rdata
    diffSyncAIAEvent.vstopei := vstopei.rdata
    diffSyncAIAEvent.hgeip := hgeip.rdata

    val diffCustomMflushpwr = DifftestModule(new DiffSyncCustomMflushpwrEvent)
    diffCustomMflushpwr.coreid := hartId
    diffCustomMflushpwr.valid := RegNext(io.fromTop.l2FlushDone) =/= io.fromTop.l2FlushDone
    diffCustomMflushpwr.l2FlushDone := io.fromTop.l2FlushDone
  }
}

trait IpIeAliasConnect {
  self: NewCSR with MachineLevel with SupervisorLevel with VirtualSupervisorLevel with HypervisorLevel =>

  mip.fromMvip  := mvip.toMip
  mip.fromSip   := sip.toMip
  mip.fromVSip  := vsip.toMip
  mvip.fromMip  := mip.toMvip
  mvip.fromSip  := sip.toMvip
  mvip.fromVSip := vsip.toMvip
  hvip.fromMip  := mip.toHvip
  hvip.fromHip  := hip.toHvip
  hvip.fromVSip := vsip.toHvip

  mie.fromHie  := hie.toMie
  mie.fromSie  := sie.toMie
  mie.fromVSie := vsie.toMie
  sie.fromVSie := vsie.toSie
}

object NewCSRMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(
    args :+ "--disable-always-basic-diff" :+ "--dump-fir" :+ "--fpga-platform" :+ "--target" :+ "verilog")

  val defaultConfig = config.alterPartial({
    // Get XSCoreParams and pass it to the "small module"
    case XSCoreParamsKey => config(XSTileKey).head
  })

  Generator.execute(
    firrtlOpts :+ "--full-stacktrace" :+ "--target-dir" :+ "backend",
    new NewCSR()(defaultConfig),
    firtoolOpts
  )

  println("done")
}
