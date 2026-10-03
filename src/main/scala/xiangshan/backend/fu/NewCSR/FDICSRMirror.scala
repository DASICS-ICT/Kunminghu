// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.DistributedCSRIO

// Each block retains only the configuration consumed by its protection path.
sealed trait FDIMirrorClient {
  val name: String
  val addresses: Seq[Int]
}

object FDIMirrorClient {
  case object Frontend extends FDIMirrorClient {
    val name = "Frontend"
    val addresses = Seq(FDIMainCfgAddress.sMainCfg,
      FDIBoundRegisterAddress.sMainBoundLo, FDIBoundRegisterAddress.sMainBoundHi,
      FDIBoundRegisterAddress.uMainBoundLo, FDIBoundRegisterAddress.uMainBoundHi)
  }
  case object ControlFlow extends FDIMirrorClient {
    val name = "ControlFlow"
    val addresses = Seq(FDIMainCfgAddress.sMainCfg, FDIBoundRegisterAddress.jumpCfg) ++
      (0 until 8).map(FDIBoundRegisterAddress.jumpBoundLo0 + _) ++
      Seq(FDISpecialRegisterAddress.mainCallEntry, FDISpecialRegisterAddress.returnPC,
        FDISpecialRegisterAddress.activeZoneReturnPC)
  }
  case object Memory extends FDIMirrorClient {
    val name = "Memory"
    val addresses = Seq(FDIMainCfgAddress.sMainCfg, FDIBoundRegisterAddress.libCfg) ++
      (0 until 32).map(FDIBoundRegisterAddress.libBoundLo0 + _)
  }
}

class FDICSRMirror(val client: FDIMirrorClient)(implicit p: Parameters)
  extends Module with RequireSyncReset {
  override def desiredName: String = s"FDI${client.name}CSRMirror"
  require(client.addresses.distinct.size == client.addresses.size)
  require(!client.addresses.contains(FDIMainCfgAddress.uMainCfg))
  val io = IO(new Bundle {
    val distribute = Flipped(new DistributedCSRIO)
    val state = Output(Vec(client.addresses.size, UInt(64.W)))
  })
  private val state = RegInit(VecInit(Seq.fill(client.addresses.size)(0.U(64.W))))
  private val write = io.distribute.w
  for ((address, index) <- client.addresses.zipWithIndex) {
    val hit = if (address == FDIMainCfgAddress.sMainCfg) {
      write.bits.addr === FDIMainCfgAddress.sMainCfg.U ||
        write.bits.addr === FDIMainCfgAddress.uMainCfg.U
    } else write.bits.addr === address.U
    when(write.valid && hit) {
      // The owner supplies the complete legal final word, including MainCfg's
      // hidden fields on U-view writes. A mirror never repeats masks or RMW.
      state(index) := write.bits.data
    }
  }
  io.state := state

  def word(address: Int): UInt = {
    require(client.addresses.contains(address), "The block does not consume this configuration word")
    io.state(client.addresses.indexOf(address))
  }
}
