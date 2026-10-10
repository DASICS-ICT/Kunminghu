// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu

import chisel3._
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.NewCSR.CSRDefines.{PrivMode, VirtMode}

// CSR values enter through their architectural bit positions. The fixture has
// no priority, candidate-delay, or acknowledgement logic of its own.
class InterruptFilterAlignmentInputs extends Bundle {
  val mip = UInt(64.W)
  val mie = UInt(64.W)
  val mideleg = UInt(64.W)
  val sip = UInt(64.W)
  val sie = UInt(64.W)
  val hip = UInt(64.W)
  val hie = UInt(64.W)
  val hideleg = UInt(64.W)
  val miprios = UInt(512.W)
  val hsiprios = UInt(512.W)
  val mode = UInt(2.W)
  val virtualMode = Bool()
  val mieGlobal = Bool()
  val sieGlobal = Bool()
  val debug = Bool()
  val debugMode = Bool()
  val singleStep = Bool()
  val stepIE = Bool()
  val nmi = Bool()
  val nmiVector = UInt(64.W)
  val nmie = Bool()
  val hu = Bool()
  val huKill = Bool()
  val huClaim = Bool()
  val critical = Bool()
  val ceTrigger = Bool()
  val criticalInFlight = Bool()
  val criticalClaim = Bool()
  val nmiInFlightValid = Bool()
  val nmiInFlightCause = UInt(8.W)
  val nmiClaimValid = Bool()
  val nmiClaimCause = UInt(8.W)
}

class InterruptFilterAlignmentHarness(val enabled: Boolean) extends Module {
  val filter = Module(new InterruptFilter(enabled))
  val io = IO(new Bundle {
    val in = Input(new InterruptFilterAlignmentInputs)
    val out = Output(chiselTypeOf(filter.io.out))
    val mtopiIID = Output(UInt(12.W))
    val mtopiPriority = Output(UInt(8.W))
    val stopiIID = Output(UInt(12.W))
    val stopiPriority = Output(UInt(8.W))
  })
  // Typed CSR defaults preserve legal XLEN encodings for inactive VS inputs.
  filter.io.in.elements.values.foreach {
    case bundle: CSRBundle => bundle := bundle.cloneType.init
    case input => input := 0.U.asTypeOf(input)
  }
  filter.io.in.mip := io.in.mip
  filter.io.in.mie := io.in.mie
  filter.io.in.mideleg := io.in.mideleg
  filter.io.in.sip := io.in.sip
  filter.io.in.sie := io.in.sie
  filter.io.in.hip := io.in.hip
  filter.io.in.hie := io.in.hie
  filter.io.in.hideleg := io.in.hideleg
  filter.io.in.miprios := io.in.miprios
  filter.io.in.hsiprios := io.in.hsiprios
  filter.io.in.privState.PRVM := PrivMode(io.in.mode)
  filter.io.in.privState.V := VirtMode(io.in.virtualMode.asUInt)
  filter.io.in.mstatusMIE := io.in.mieGlobal
  filter.io.in.sstatusSIE := io.in.sieGlobal
  filter.io.in.debugIntr := io.in.debug
  filter.io.in.debugMode := io.in.debugMode
  filter.io.in.dcsr := ((io.in.ceTrigger.asUInt << 19) |
    (io.in.stepIE.asUInt << 11) | (io.in.singleStep.asUInt << 2)).pad(32)
  filter.io.in.nmi := io.in.nmi
  filter.io.in.nmiVec := io.in.nmiVector
  filter.io.in.mnstatusNMIE := io.in.nmie
  filter.io.in.huCandidate.foreach(_ := io.in.hu)
  filter.io.in.huCandidateKill.foreach(_ := io.in.huKill)
  filter.io.in.huClaim.foreach(_ := io.in.huClaim)
  filter.io.in.criticalDebug.foreach(_ := io.in.critical)
  filter.io.in.criticalDebugInFlight.foreach(_ := io.in.criticalInFlight)
  filter.io.in.criticalDebugClaim.foreach(_ := io.in.criticalClaim)
  filter.io.in.nmiInFlight.foreach { event =>
    event.valid := io.in.nmiInFlightValid
    event.bits := io.in.nmiInFlightCause
  }
  filter.io.in.nmiClaim.foreach { event =>
    event.valid := io.in.nmiClaimValid
    event.bits := io.in.nmiClaimCause
  }
  io.out := filter.io.out
  io.mtopiIID := filter.io.out.mtopi.IID.asUInt
  io.mtopiPriority := filter.io.out.mtopi.IPRIO.asUInt
  io.stopiIID := filter.io.out.stopi.IID.asUInt
  io.stopiPriority := filter.io.out.stopi.IPRIO.asUInt
}
