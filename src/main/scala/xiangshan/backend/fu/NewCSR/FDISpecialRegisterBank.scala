// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util.Valid
import org.chipsalliance.cde.config.Parameters
import scala.collection.immutable.SeqMap
import xiangshan.backend.fu.NewCSR.CSRBundles.FieldInitBundle
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRRWField => RW}

object FDISpecialRegisterAddress {
  val mainCallEntry = 0x8b0
  val returnPC = 0x8b1
  val activeZoneReturnPC = 0x8b2
  val fReason = 0x8b3
}

// Every three-bit software value is valid, including reserved hardware reasons.
// Absent upper fields are read as zero and ignore writes through CSRBundle.
class FDIFReasonBundle extends CSRBundle {
  val REASON = RW(2, 0).withReset(0.U)
}

class FDIFReasonModule(implicit p: Parameters)
  extends CSRModule("FDIFReason", new FDIFReasonBundle) with RequireSyncReset {
  // This event is a committed M/HS DASICS trap, not a speculative candidate.
  // CSR dispatch serialization makes software and hardware effects exclusive.
  val trapReason = IO(Input(Valid(new FDIFReasonBundle)))
  reg.REASON.addOtherUpdate(trapReason.valid, trapReason.bits.REASON)
  reconnectReg()
  assert(!(w.wen && trapReason.valid), "FReason software and trap effects must be exclusive")
  when(trapReason.valid) {
    assert(trapReason.bits.REASON.asUInt >= 1.U && trapReason.bits.REASON.asUInt <= 4.U,
      "A committed DASICS trap must have a hardware-defined reason")
  }
}

// Native instances created in the caller's Module context. Each CSRModule is
// the sole owner of its backing; this group adds no dispatch or request state.
// The caller supplies one authorized, cancellation-qualified write effect and
// the same transaction's final write value, or an exclusive precise trap event.
class FDISpecialRegisterBank(implicit p: Parameters) {
  // FieldInitBundle retains all 64 bits, including bit zero, with reset zero.
  // Stored zero and unaligned values have no special behavior in this group.
  val mainCallEntry = Module(new CSRModule("FDIMainCallEntry", new FieldInitBundle) with RequireSyncReset).setAddr(FDISpecialRegisterAddress.mainCallEntry)
  val returnPC = Module(new CSRModule("FDIReturnPC", new FieldInitBundle) with RequireSyncReset).setAddr(FDISpecialRegisterAddress.returnPC)
  val activeZoneReturnPC = Module(new CSRModule("FDIActiveZoneReturnPC", new FieldInitBundle) with RequireSyncReset).setAddr(FDISpecialRegisterAddress.activeZoneReturnPC)
  val fReason = Module(new FDIFReasonModule).setAddr(FDISpecialRegisterAddress.fReason)
  // Standalone software-only users do not produce architectural trap events.
  fReason.trapReason.valid := false.B
  fReason.trapReason.bits := 0.U.asTypeOf(fReason.trapReason.bits)

  val csrMods: Seq[CSRModule[_]] = Seq(
    mainCallEntry,
    returnPC,
    activeZoneReturnPC,
    fReason
  )
  require(csrMods.size == 4)
  require(csrMods.map(_.addr).distinct.size == csrMods.size, "Duplicate special register CSR address")
  val csrRwMap: SeqMap[Int, (CSRAddrWriteBundle[_], UInt)] = SeqMap.from(
    csrMods.map(csr => csr.addr -> (csr.w, csr.rdata))
  )
  val csrOutMap: SeqMap[Int, UInt] = SeqMap.from(
    csrMods.map(csr => csr.addr -> csr.regOut.asInstanceOf[CSRBundle].asUInt)
  )
}
