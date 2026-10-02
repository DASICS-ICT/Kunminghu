// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.backend.fu.NewCSR

import chisel3._
import org.chipsalliance.cde.config.Parameters
import scala.collection.immutable.SeqMap
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRRWField => RW}

object FDIBoundRegisterAddress {
  val sMainBoundLo = 0xbc5
  val sMainBoundHi = 0xbc6
  val uMainBoundLo = 0x9e2
  val uMainBoundHi = 0x9e3
  val libCfg = 0x880
  val libBoundLo0 = 0x890
  val libBoundHi0 = 0x891
  val libBoundLo1 = 0x892
  val libBoundHi1 = 0x893
  val libBoundLo2 = 0x894
  val libBoundHi2 = 0x895
  val libBoundLo3 = 0x896
  val libBoundHi3 = 0x897
  val libBoundLo4 = 0x898
  val libBoundHi4 = 0x899
  val libBoundLo5 = 0x89a
  val libBoundHi5 = 0x89b
  val libBoundLo6 = 0x89c
  val libBoundHi6 = 0x89d
  val libBoundLo7 = 0x89e
  val libBoundHi7 = 0x89f
  val libBoundLo8 = 0x8a0
  val libBoundHi8 = 0x8a1
  val libBoundLo9 = 0x8a2
  val libBoundHi9 = 0x8a3
  val libBoundLo10 = 0x8a4
  val libBoundHi10 = 0x8a5
  val libBoundLo11 = 0x8a6
  val libBoundHi11 = 0x8a7
  val libBoundLo12 = 0x8a8
  val libBoundHi12 = 0x8a9
  val libBoundLo13 = 0x8aa
  val libBoundHi13 = 0x8ab
  val libBoundLo14 = 0x8ac
  val libBoundHi14 = 0x8ad
  val libBoundLo15 = 0x8ae
  val libBoundHi15 = 0x8af
  val jumpBoundLo0 = 0x8c0
  val jumpBoundHi0 = 0x8c1
  val jumpBoundLo1 = 0x8c2
  val jumpBoundHi1 = 0x8c3
  val jumpBoundLo2 = 0x8c4
  val jumpBoundHi2 = 0x8c5
  val jumpBoundLo3 = 0x8c6
  val jumpBoundHi3 = 0x8c7
  val jumpCfg = 0x8c8
}

// Complete unsigned RV64 address. CSRBundle preserves bits 63:3 in their
// architectural positions and supplies zero for the absent low three bits.
class FDIBoundRegisterBundle extends CSRBundle {
  val ADDRESS = RW(63, 3).withReset(0.U)
}

class FDILibCfgBundle extends CSRBundle {
  val W0 = RW(0).withReset(0.U)
  val R0 = RW(1).withReset(0.U)
  val V0 = RW(3).withReset(0.U)
  val W1 = RW(4).withReset(0.U)
  val R1 = RW(5).withReset(0.U)
  val V1 = RW(7).withReset(0.U)
  val W2 = RW(8).withReset(0.U)
  val R2 = RW(9).withReset(0.U)
  val V2 = RW(11).withReset(0.U)
  val W3 = RW(12).withReset(0.U)
  val R3 = RW(13).withReset(0.U)
  val V3 = RW(15).withReset(0.U)
  val W4 = RW(16).withReset(0.U)
  val R4 = RW(17).withReset(0.U)
  val V4 = RW(19).withReset(0.U)
  val W5 = RW(20).withReset(0.U)
  val R5 = RW(21).withReset(0.U)
  val V5 = RW(23).withReset(0.U)
  val W6 = RW(24).withReset(0.U)
  val R6 = RW(25).withReset(0.U)
  val V6 = RW(27).withReset(0.U)
  val W7 = RW(28).withReset(0.U)
  val R7 = RW(29).withReset(0.U)
  val V7 = RW(31).withReset(0.U)
  val W8 = RW(32).withReset(0.U)
  val R8 = RW(33).withReset(0.U)
  val V8 = RW(35).withReset(0.U)
  val W9 = RW(36).withReset(0.U)
  val R9 = RW(37).withReset(0.U)
  val V9 = RW(39).withReset(0.U)
  val W10 = RW(40).withReset(0.U)
  val R10 = RW(41).withReset(0.U)
  val V10 = RW(43).withReset(0.U)
  val W11 = RW(44).withReset(0.U)
  val R11 = RW(45).withReset(0.U)
  val V11 = RW(47).withReset(0.U)
  val W12 = RW(48).withReset(0.U)
  val R12 = RW(49).withReset(0.U)
  val V12 = RW(51).withReset(0.U)
  val W13 = RW(52).withReset(0.U)
  val R13 = RW(53).withReset(0.U)
  val V13 = RW(55).withReset(0.U)
  val W14 = RW(56).withReset(0.U)
  val R14 = RW(57).withReset(0.U)
  val V14 = RW(59).withReset(0.U)
  val W15 = RW(60).withReset(0.U)
  val R15 = RW(61).withReset(0.U)
  val V15 = RW(63).withReset(0.U)
}

class FDIJumpCfgBundle extends CSRBundle {
  val V0 = RW(0).withReset(0.U)
  val V1 = RW(16).withReset(0.U)
  val V2 = RW(32).withReset(0.U)
  val V3 = RW(48).withReset(0.U)
}

// Native CSR instance group, instantiated in a caller's Module context.
// Each CSRModule owns exactly one backing; maps expose its native ports.
// No range checks, aliases, second state copies, dispatch, or request pipeline.
// The eventual production caller must gate creation with the real HasFDI and
// supply unified access permission and one aligned final-write effect per CSR.
class FDIBoundRegisterBank(implicit p: Parameters) {
  val sMainBoundLo = Module(new CSRModule("FDISMainBoundLo", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.sMainBoundLo)
  val sMainBoundHi = Module(new CSRModule("FDISMainBoundHi", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.sMainBoundHi)
  val uMainBoundLo = Module(new CSRModule("FDIUMainBoundLo", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.uMainBoundLo)
  val uMainBoundHi = Module(new CSRModule("FDIUMainBoundHi", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.uMainBoundHi)
  val libCfg = Module(new CSRModule("FDILibCfg", new FDILibCfgBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libCfg)
  val libBoundLo0 = Module(new CSRModule("FDILibBoundLo0", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundLo0)
  val libBoundHi0 = Module(new CSRModule("FDILibBoundHi0", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundHi0)
  val libBoundLo1 = Module(new CSRModule("FDILibBoundLo1", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundLo1)
  val libBoundHi1 = Module(new CSRModule("FDILibBoundHi1", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundHi1)
  val libBoundLo2 = Module(new CSRModule("FDILibBoundLo2", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundLo2)
  val libBoundHi2 = Module(new CSRModule("FDILibBoundHi2", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundHi2)
  val libBoundLo3 = Module(new CSRModule("FDILibBoundLo3", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundLo3)
  val libBoundHi3 = Module(new CSRModule("FDILibBoundHi3", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundHi3)
  val libBoundLo4 = Module(new CSRModule("FDILibBoundLo4", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundLo4)
  val libBoundHi4 = Module(new CSRModule("FDILibBoundHi4", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundHi4)
  val libBoundLo5 = Module(new CSRModule("FDILibBoundLo5", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundLo5)
  val libBoundHi5 = Module(new CSRModule("FDILibBoundHi5", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundHi5)
  val libBoundLo6 = Module(new CSRModule("FDILibBoundLo6", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundLo6)
  val libBoundHi6 = Module(new CSRModule("FDILibBoundHi6", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundHi6)
  val libBoundLo7 = Module(new CSRModule("FDILibBoundLo7", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundLo7)
  val libBoundHi7 = Module(new CSRModule("FDILibBoundHi7", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundHi7)
  val libBoundLo8 = Module(new CSRModule("FDILibBoundLo8", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundLo8)
  val libBoundHi8 = Module(new CSRModule("FDILibBoundHi8", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundHi8)
  val libBoundLo9 = Module(new CSRModule("FDILibBoundLo9", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundLo9)
  val libBoundHi9 = Module(new CSRModule("FDILibBoundHi9", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundHi9)
  val libBoundLo10 = Module(new CSRModule("FDILibBoundLo10", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundLo10)
  val libBoundHi10 = Module(new CSRModule("FDILibBoundHi10", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundHi10)
  val libBoundLo11 = Module(new CSRModule("FDILibBoundLo11", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundLo11)
  val libBoundHi11 = Module(new CSRModule("FDILibBoundHi11", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundHi11)
  val libBoundLo12 = Module(new CSRModule("FDILibBoundLo12", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundLo12)
  val libBoundHi12 = Module(new CSRModule("FDILibBoundHi12", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundHi12)
  val libBoundLo13 = Module(new CSRModule("FDILibBoundLo13", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundLo13)
  val libBoundHi13 = Module(new CSRModule("FDILibBoundHi13", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundHi13)
  val libBoundLo14 = Module(new CSRModule("FDILibBoundLo14", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundLo14)
  val libBoundHi14 = Module(new CSRModule("FDILibBoundHi14", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundHi14)
  val libBoundLo15 = Module(new CSRModule("FDILibBoundLo15", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundLo15)
  val libBoundHi15 = Module(new CSRModule("FDILibBoundHi15", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.libBoundHi15)
  val jumpBoundLo0 = Module(new CSRModule("FDIJumpBoundLo0", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.jumpBoundLo0)
  val jumpBoundHi0 = Module(new CSRModule("FDIJumpBoundHi0", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.jumpBoundHi0)
  val jumpBoundLo1 = Module(new CSRModule("FDIJumpBoundLo1", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.jumpBoundLo1)
  val jumpBoundHi1 = Module(new CSRModule("FDIJumpBoundHi1", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.jumpBoundHi1)
  val jumpBoundLo2 = Module(new CSRModule("FDIJumpBoundLo2", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.jumpBoundLo2)
  val jumpBoundHi2 = Module(new CSRModule("FDIJumpBoundHi2", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.jumpBoundHi2)
  val jumpBoundLo3 = Module(new CSRModule("FDIJumpBoundLo3", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.jumpBoundLo3)
  val jumpBoundHi3 = Module(new CSRModule("FDIJumpBoundHi3", new FDIBoundRegisterBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.jumpBoundHi3)
  val jumpCfg = Module(new CSRModule("FDIJumpCfg", new FDIJumpCfgBundle) with RequireSyncReset).setAddr(FDIBoundRegisterAddress.jumpCfg)

  val csrMods: Seq[CSRModule[_]] = Seq(
    sMainBoundLo,
    sMainBoundHi,
    uMainBoundLo,
    uMainBoundHi,
    libCfg,
    libBoundLo0,
    libBoundHi0,
    libBoundLo1,
    libBoundHi1,
    libBoundLo2,
    libBoundHi2,
    libBoundLo3,
    libBoundHi3,
    libBoundLo4,
    libBoundHi4,
    libBoundLo5,
    libBoundHi5,
    libBoundLo6,
    libBoundHi6,
    libBoundLo7,
    libBoundHi7,
    libBoundLo8,
    libBoundHi8,
    libBoundLo9,
    libBoundHi9,
    libBoundLo10,
    libBoundHi10,
    libBoundLo11,
    libBoundHi11,
    libBoundLo12,
    libBoundHi12,
    libBoundLo13,
    libBoundHi13,
    libBoundLo14,
    libBoundHi14,
    libBoundLo15,
    libBoundHi15,
    jumpBoundLo0,
    jumpBoundHi0,
    jumpBoundLo1,
    jumpBoundHi1,
    jumpBoundLo2,
    jumpBoundHi2,
    jumpBoundLo3,
    jumpBoundHi3,
    jumpCfg
  )
  require(csrMods.size == 46)
  require(csrMods.map(_.addr).distinct.size == csrMods.size, "Duplicate C03 CSR address")
  val csrRwMap: SeqMap[Int, (CSRAddrWriteBundle[_], UInt)] = SeqMap.from(
    csrMods.map(csr => csr.addr -> (csr.w, csr.rdata))
  )
  val csrOutMap: SeqMap[Int, UInt] = SeqMap.from(
    csrMods.map(csr => csr.addr -> csr.regOut.asInstanceOf[CSRBundle].asUInt)
  )
}
