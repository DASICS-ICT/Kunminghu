package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.CSRs
import utility.GatedValidRegNext
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRRWField => RW, CSRWARLField => WARL}
import xiangshan.backend.fu.NewCSR.CSRFunc._
import xiangshan.backend.fu.vector.Bundles._
import xiangshan.backend.fu.NewCSR.CSRConfig._
import xiangshan.backend.fu.fpu.Bundles.{Fflags, Frm}
import xiangshan.backend.fu.NewCSR.CSREnumTypeImplicitCast._
import xiangshan.backend.fu.DasicsConst
import scala.collection.immutable.SeqMap

trait CSRDasics { self: NewCSR with DasicsConst =>

  // Dasics U CSR, modeM || modeS configurable
  val dumcfg = Module(new CSRModule("DasicsUMainCfg", new DasicsUMainCfgBundle))
  .setAddr(DasicsUMainCfg)
  val dumboundlo = Module(new CSRModule("DasicsUMainBoundLo", new DasicsCommonBundle))
  .setAddr(DasicsUMainBoundLo)
  val dumboundhi = Module(new CSRModule("DasicsUMainBoundHi", new DasicsCommonBundle))
  .setAddr(DasicsUMainBoundHi)
  
  // Dasics config CSR -> modeM || modeS || (modeU && Trusted) configurable
  // TODO: implement parameterized libcfg bundle
  val dlcfg = Module(new CSRModule("DasicsLibCfg", new DasicsCommonBundle))
  .setAddr(DasicsLibCfgBase)

  val dlbound = (0 until NumDasicsMemBounds*2).map{i =>
    val module = Module(new CSRModule(s"DasicsLibBound$i", new DasicsBoundBundle)).setAddr(DasicsLibBoundBase+i)
    module 
  }

  val dmaincall = Module(new CSRModule("DasicsMainCall", new DasicsCommonBundle))
  .setAddr(DasicsMainCall)
  val dretpc = Module(new CSRModule("DasicsReturnPc", new DasicsCommonBundle))
  .setAddr(DasicsReturnPc)

  val djcfg = Module(new CSRModule("DasicsJmpCfg",new DasicsJmpCfgBundle))
  .setAddr(DasicsJmpCfgBase)
  val djbound = (0 until NumDasicsJmpBounds*2).map{ i =>
    val module = Module(new CSRModule(s"DasicsJmpBound$i", new DasicsBoundBundle)).setAddr(DasicsJmpBoundBase+i)
    module 
  }

  val dasicsCfgMods = Seq(
      dumcfg,
      dumboundlo,
      dumboundhi,
      dlcfg,
      djcfg,
      dmaincall,
      dretpc,
    )

  val dasicsCSRMods = dasicsCfgMods ++ dlbound ++ djbound

  val dasicsCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_ <: CSRBundle], UInt)] = SeqMap.from(
    dasicsCSRMods.map(csr => (csr.addr -> (csr.w -> csr.rdata))).iterator
  )

  val dasicsCSROutMap: SeqMap[Int, UInt] = SeqMap.from(
    dasicsCSRMods.map(csr => (csr.addr -> csr.regOut.asInstanceOf[CSRBundle].asUInt)).iterator
  )

}

class DasicsUMainCfgBundle extends CSRBundle {
    val CUFT = RW(5).withReset(false.B)
    val CULT = RW(4).withReset(false.B)
    val CUST = RW(3).withReset(false.B)
    val CUET = RW(2).withReset(false.B)
    val UENA = RW(1).withReset(false.B)
}


class DasicsBoundBundle extends CSRBundle with DasicsConst {
  // for mask
  val bound = RW(63, DasicsGrainBit).withReset(0.U)
}

class DasicsCommonBundle extends CSRBundle with DasicsConst {
  // only for reset
  val value = RW(63, 0).withReset(0.U)
}

class DasicsJmpCfgBundle extends CSRBundle with DasicsConst {
  // for mask
  // TODO: parameterize it
    val v0 = RW(0).withReset(false.B)
    val v1 = RW(16).withReset(false.B)
    val v2 = RW(32).withReset(false.B)
    val v3 = RW(48).withReset(false.B)
}


