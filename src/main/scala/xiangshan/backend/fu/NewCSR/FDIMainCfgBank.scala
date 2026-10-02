// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu.NewCSR

import chisel3._
import scala.collection.immutable.SeqMap
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRRWField => RW}

object FDIMainCfgAddress {
  val sMainCfg = 0xbc4
  val uMainCfg = 0x9e1
}

class FDIMainCfgBundle extends CSRBundle {
  val sEnable             = RW(0).withReset(0.U)
  val uEnable             = RW(1).withReset(0.U)
  val sCloseEcall         = RW(2).withReset(0.U)
  val sCloseWrite         = RW(3).withReset(0.U)
  val sCloseRead          = RW(4).withReset(0.U)
  val sCloseJump          = RW(5).withReset(0.U)
  val uCloseEcall         = RW(6).withReset(0.U)
  val uCloseWrite         = RW(7).withReset(0.U)
  val uCloseRead          = RW(8).withReset(0.U)
  val uCloseJump          = RW(9).withReset(0.U)
  val uCloseUserInterrupt = RW(10).withReset(0.U)
}

// The alias retains architectural bit positions and contains no hidden S fields.
class FDIUMainCfgBundle extends CSRBundle {
  val uEnable             = RW(1).withReset(0.U)
  val uCloseEcall         = RW(6).withReset(0.U)
  val uCloseWrite         = RW(7).withReset(0.U)
  val uCloseRead          = RW(8).withReset(0.U)
  val uCloseJump          = RW(9).withReset(0.U)
  val uCloseUserInterrupt = RW(10).withReset(0.U)
}

class FDIMainCfgModule(implicit override val p: Parameters)
  extends CSRModule("FDIMainCfg", new FDIMainCfgBundle) with RequireSyncReset {
  val wAliasUMainCfg = IO(Input(new CSRAddrWriteBundle(new FDIUMainCfgBundle)))
  val uRdata = IO(Output(UInt(64.W)))

  // Both ports consume final CSR write data. Only the inherited reg owns state;
  // an alias write leaves every field absent from the alias bundle unchanged.
  for ((name, field) <- wAliasUMainCfg.wdataFields.elements) {
    val aliasField = field.asInstanceOf[CSREnumType]
    reg.elements(name).asInstanceOf[CSREnumType].addOtherUpdate(
      wAliasUMainCfg.wen && aliasField.isLegal,
      aliasField
    )
  }
  reconnectReg()

  // CSRModule combines updates with Mux1H, so callers must select one view.
  assert(!(w.wen && wAliasUMainCfg.wen), "MainCfg writes must select one view")

  private val uFields = Wire(new FDIUMainCfgBundle)
  uFields := regOut.asUInt
  uRdata := uFields.asUInt
}

// An instance group in the caller's Module context, following the native CSR
// maps. Only FDIMainCfgModule owns architectural state. This group adds neither
// an address bus nor read/write dispatch; production integration belongs to C05.
class FDIMainCfgBank(implicit p: Parameters) {
  val mainCfg = Module(new FDIMainCfgModule).setAddr(FDIMainCfgAddress.sMainCfg)
  val csrMods: Seq[CSRModule[_]] = Seq(mainCfg)
  require(FDIMainCfgAddress.sMainCfg != FDIMainCfgAddress.uMainCfg)

  val csrRwMap: SeqMap[Int, (CSRAddrWriteBundle[_], UInt)] = SeqMap(
    FDIMainCfgAddress.sMainCfg -> (mainCfg.w, mainCfg.rdata),
    FDIMainCfgAddress.uMainCfg -> (mainCfg.wAliasUMainCfg, mainCfg.uRdata)
  )
  val csrOutMap: SeqMap[Int, UInt] = SeqMap(
    FDIMainCfgAddress.sMainCfg -> mainCfg.regOut.asUInt,
    FDIMainCfgAddress.uMainCfg -> mainCfg.uRdata
  )
}
