// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import org.chipsalliance.cde.config.Parameters
import utility.DelayN
import xiangshan._
import xiangshan.backend.fu.NewCSR.{FDIBoundRegisterAddress, FDIMainCfgAddress, FDIMainCfgBundle}
import xiangshan.backend.fu.NewCSR.CSRDefines.{PrivMode, SatpMode}
import xiangshan.frontend._
import xiangshan.frontend.icache.ICacheMainPipeResp

// Reuse the production Backend/FTQ and their passive shared-UID observer. The
// inherited synthetic CtrlFlow/predecode inputs are disconnected below: all
// instructions reaching the backend come from the actual IFU and IBuffer.
class FDITrustMetadataRecoveryHarness(implicit p: Parameters) extends FDICSRRecoveryBackendHarness {
  val ifu = withReset(reset.asAsyncReset) { Module(new NewIFU) }
  val ibuffer = withReset(reset.asAsyncReset) { Module(new IBuffer) }
  val frontend = IO(new Bundle {
    val cacheReady = Input(Bool())
    val cacheResponse = Flipped(Valid(new ICacheMainPipeResp))
    val cacheStop = Output(Bool())
    val request = Output(Valid(new FetchRequestBundle))
    val requestFire = Output(Bool())
    val cacheRequestFire = Output(Bool())
    val config = Output(new FDIFrontendConfig)
    val mirrorInput = Output(new DistributedCSRIO)
    val fetch = Output(Valid(new FetchToIBuffer))
    val fetchReady = Output(Bool())
    val bufferFlush = Output(Bool())
    val bufferEntries = Output(UInt(log2Ceil(IBufSize + 1).W))
    val decodeBlocked = Output(Bool())
    val decodeCanAccept = Output(Bool())
    val decode = Output(Vec(DecodeWidth, Valid(new CtrlFlow)))
    val decodeReady = Output(Vec(DecodeWidth, Bool()))
    val f2 = Output(new Bundle {
      val valid = Bool()
      val fire = Bool()
      val flush = Bool()
      val request = new FetchRequestBundle
      val tags = Vec(PredictWidth, Bool())
    })
    val f3 = Output(new Bundle {
      val valid = Bool()
      val flush = Bool()
      val request = new FetchRequestBundle
    })
  })

  idleInputs(ifu.io)
  idleInputs(ibuffer.io)
  ifu.io.ftqInter.fromFtq <> ftq.io.toIfu
  ftq.io.toIfu.req.ready := ifu.io.ftqInter.fromFtq.req.ready && frontend.cacheReady
  ftq.io.toICache.req.ready := ifu.io.ftqInter.fromFtq.req.ready && frontend.cacheReady
  ftq.io.fromIfu <> ifu.io.ftqInter.toFtq
  ftq.io.mmioCommitRead <> ifu.io.mmioCommitRead
  ifu.io.icacheInter.icacheReady := frontend.cacheReady
  ifu.io.icacheInter.resp := frontend.cacheResponse
  ifu.io.rob_commits := backend.io.frontend.toFtq.rob_commits
  backend.io.frontend.fromIfu := ifu.io.toBackend
  ibuffer.io.in <> ifu.io.toIbuffer
  backend.io.frontend.cfVec <> ibuffer.io.out
  backend.io.frontend.stallReason <> ibuffer.io.stallReason
  ibuffer.io.decodeCanAccept := backend.io.frontend.canAccept
  val bufferFlush = RegNext(backend.io.frontend.toFtq.redirect.valid, false.B)
  ibuffer.io.flush := bufferFlush

  // Match Frontend's two-cycle CSR/mode transport. The inherited frontend
  // mirror is a real FDICSRMirror driven by this same distributed CSR source.
  val tlbCsr = DelayN(backend.io.mem.tlbCsr, 2)
  val csrCtrl = DelayN(backend.io.mem.csrCtrl, 2)
  val mainCfg = Wire(new FDIMainCfgBundle)
  mainCfg := frontendMirror.word(FDIMainCfgAddress.sMainCfg)
  val config = ifu.io.fdiConfig.get
  config.sourcePrivilege := tlbCsr.priv.imode
  config.sourceVirtual := csrCtrl.virtMode
  config.uEnable := mainCfg.uEnable.asBool
  config.sEnable := mainCfg.sEnable.asBool
  config.uBoundLo := frontendMirror.word(FDIBoundRegisterAddress.uMainBoundLo)
  config.uBoundHi := frontendMirror.word(FDIBoundRegisterAddress.uMainBoundHi)
  config.sBoundLo := frontendMirror.word(FDIBoundRegisterAddress.sMainBoundLo)
  config.sBoundHi := frontendMirror.word(FDIBoundRegisterAddress.sMainBoundHi)
  val sourceIsMachine = tlbCsr.priv.imode === PrivMode.M.asUInt
  config.sv39 := (!sourceIsMachine && !csrCtrl.virtMode && tlbCsr.satp.mode === SatpMode.Sv39.asUInt) ||
    (csrCtrl.virtMode && tlbCsr.vsatp.mode === SatpMode.Sv39.asUInt)
  config.sv48 := (!sourceIsMachine && !csrCtrl.virtMode && tlbCsr.satp.mode === SatpMode.Sv48.asUInt) ||
    (csrCtrl.virtMode && tlbCsr.vsatp.mode === SatpMode.Sv48.asUInt)
  ifu.io.frontendTrigger := csrCtrl.frontend_trigger
  ifu.io.csr_fsIsOff := csrCtrl.fsIsOff

  frontend.request.valid := ftq.io.toIfu.req.valid
  frontend.request.bits := ftq.io.toIfu.req.bits
  frontend.requestFire := ftq.io.toIfu.req.fire
  frontend.cacheRequestFire := ftq.io.toICache.req.fire
  frontend.cacheStop := ifu.io.icacheStop
  frontend.config := config
  frontend.mirrorInput := frontendMirror.io.distribute
  frontend.fetch.valid := ifu.io.toIbuffer.valid
  frontend.fetch.bits := ifu.io.toIbuffer.bits
  frontend.fetchReady := ifu.io.toIbuffer.ready
  frontend.bufferFlush := bufferFlush
  frontend.bufferEntries := observe(ibuffer.numValid)
  // Decode's redirect input includes CtrlBlock's registered pendingRedirect
  // interval; observing it does not modify readiness or cancellation.
  frontend.decodeBlocked := observe(ctrl.decode.io.redirect)
  frontend.decodeCanAccept := backend.io.frontend.canAccept
  ibuffer.io.out.zipWithIndex.foreach { case (port, lane) =>
    frontend.decode(lane).valid := port.valid
    frontend.decode(lane).bits := port.bits
    frontend.decodeReady(lane) := port.ready
  }
  frontend.f2.valid := observe(ifu.f2_valid)
  frontend.f2.fire := observe(ifu.f2_fire)
  frontend.f2.flush := observe(ifu.f2_flush)
  frontend.f2.request := observe(ifu.f2_ftq_req)
  frontend.f2.tags := observe(ifu.f2_fdiNotTrusted.get)
  frontend.f3.valid := observe(ifu.f3_valid)
  frontend.f3.flush := observe(ifu.f3_flush)
  frontend.f3.request := observe(ifu.f3_ftq_req)
}
