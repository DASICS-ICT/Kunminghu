// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util.Valid
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

class FDIMainCfgWrite extends Bundle {
  val address = UInt(12.W)
  val data = UInt(64.W)
}

class FDIMainCfgBank(implicit p: Parameters) extends Module with RequireSyncReset {
  val io = IO(new Bundle {
    val readAddress = Input(UInt(12.W))
    // Read enable and write valid are already authorized by the caller.
    val readEnable = Input(Bool())
    val readHit = Output(Bool())
    val readData = Output(UInt(64.W))
    val write = Flipped(Valid(new FDIMainCfgWrite))
    val writeCancel = Input(Bool())
    // Internal state views are combinational and do not authorize CSR access.
    val sView = Output(UInt(64.W))
    val uView = Output(UInt(64.W))
    val writeApplied = Output(Bool())
  })

  private val mainCfg = Module(new FDIMainCfgModule)
  private val readS = io.readAddress === FDIMainCfgAddress.sMainCfg.U(12.W)
  private val readU = io.readAddress === FDIMainCfgAddress.uMainCfg.U(12.W)
  private val writeS = io.write.bits.address === FDIMainCfgAddress.sMainCfg.U(12.W)
  private val writeU = io.write.bits.address === FDIMainCfgAddress.uMainCfg.U(12.W)

  // One valid pulse describes one edge's write, including a same-value write.
  // Address, data, permission and cancellation must belong to that transaction;
  // this bank neither queues requests nor repeats the upstream RW/RS/RC operation.
  io.writeApplied := io.write.valid && (writeS || writeU) && !io.writeCancel && !reset.asBool
  mainCfg.w.wen := io.writeApplied && writeS
  mainCfg.w.wdata := io.write.bits.data
  mainCfg.wAliasUMainCfg.wen := io.writeApplied && writeU
  mainCfg.wAliasUMainCfg.wdata := io.write.bits.data

  io.sView := mainCfg.rdata
  io.uView := mainCfg.uRdata
  io.readHit := readS || readU
  io.readData := Mux(io.readEnable && io.readHit, Mux(readS, mainCfg.rdata, mainCfg.uRdata), 0.U)
}
