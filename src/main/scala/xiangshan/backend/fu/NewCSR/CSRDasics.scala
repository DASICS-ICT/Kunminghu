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

  // Dasics S CSR -> modeM configurable
//   val dsmcfg = Module(new CSRModule("DasicsSMainCfg", new DasicsSMainCfgBundle))
//   .setAddr(0xBC0)
//   val dsmboundlo = Module(new CSRModule("DasicsSMainBoundLo", new DasicsSMainBoundLoBundle))
//   .setAddr(0xBC2)
//   val dsmboundhi = Module(new CSRModule("DasicsSMainBoundHi", new DasicsSMainBoundHiBundle))
//   .setAddr(0xBC3)

  // Dasics U CSR, modeM || modeHS configurable
  val dumcfg = Module(new CSRModule("DasicsUMainCfg", new DasicsUMainCfgBundle))
  .setAddr(DasicsUMainCfg)
  val dumboundlo = Module(new CSRModule("DasicsUMainBoundLo"))
  .setAddr(DasicsUMainBoundLo)
  val dumboundhi = Module(new CSRModule("DasicsUMainBoundHi"))
  .setAddr(DasicsUMainBoundHi)
  
  // Dasics config CSR -> modeM || modeHS || (modeHU && Trusted) configurable
  val dlcfg = Module(new CSRModule("DasicsLibCfg", new DasicsLibCfgBundle))
  .setAddr(DasicsLibCfgBase)

  val dlbound = (0 until NumDasicsMemBounds*2).map{i =>
    val module = Module(new CSRModule(s"DasicsLibBound$i")).setAddr(DasicsLibBoundBase+i)
    module 
  }

  val dmaincall = Module(new CSRModule("DasicsMainCall"))
  .setAddr(DasicsMainCall)
  val dretpc = Module(new CSRModule("DasicsReturnPc"))
  .setAddr(DasicsReturnPc)
  val dretpcfz = Module(new CSRModule("DasicsActiveZoneReturnPc"))
  .setAddr(DasicsActiveZoneReturnPc)
  val dfreason = Module(new CSRModule("DasicsFReason"))
  .setAddr(DasicsFReason)

  val djcfg = Module(new CSRModule("DasicsJmpCfg", new DasicsJmpCfgBundle))
  .setAddr(DasicsJmpCfgBase)
  val djbound = (0 until NumDasicsJmpBounds*2).map{ i =>
    val module = Module(new CSRModule(s"DasicsJmpBound$i")).setAddr(DasicsJmpBoundBase+i)
    module 
  }

  val dasicsCSRMods = Seq(
    dumcfg,
    dumboundlo,
    dumboundhi,
    dlcfg,
    djcfg,
    dmaincall,
    dretpc,
    dretpcfz,
    dfreason,
  )
  for (i <- 0 until NumDasicsMemBounds*2) dasicsCSRMods += dlbound(i)
  for (i <- 0 until NumDasicsJmpBounds*2) dasicsCSRMods += djbound(i)

  val dasicsCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_ <: CSRBundle], UInt)] = SeqMap.from(
    dasicsCSRMods.map(csr => (csr.addr -> (csr.w -> csr.rdata))).iterator
  )

  val dasicsCSROutMap: SeqMap[Int, UInt] = SeqMap.from(
    dasicsCSRMods.map(csr => (csr.addr -> csr.regOut.asInstanceOf[CSRBundle].asUInt)).iterator
  )

}

// class DasicsSMainCfgBundle extends CSRBundle {
//     val CSFT = RW(9).withReset(false.B)
//     val CSLT = RW(8).withReset(false.B)
//     val CSST = RW(7).withReset(false.B)
//     val CSET = RW(6).withReset(false.B)
//     val CUFT = RW(5).withReset(false.B)
//     val CULT = RW(4).withReset(false.B)
//     val CUST = RW(3).withReset(false.B)
//     val CUET = RW(2).withReset(false.B)
//     val UENA = RW(1).withReset(false.B)
//     val SENA = RW(0).withReset(false.B)
// }

class DasicsUMainCfgBundle extends CSRBundle {
    val CUFT = RW(5).withReset(false.B)
    val CULT = RW(4).withReset(false.B)
    val CUST = RW(3).withReset(false.B)
    val CUET = RW(2).withReset(false.B)
    val UENA = RW(1).withReset(false.B)
}

class DasicsLibCfgBundle extends CSRBundle with DasicsConst{
    val cfg = (0 until NumDasicsMemBounds).map{
        i => new Bundle {
            val v = RW(4*i+3).withReset(false.B)
            val u = RW(4*i+2).withReset(false.B)
            val r = RW(4*i+1).withReset(false.B)
            val w = RW(4*i  ).withReset(false.B)
        }
    }
}

class DasicsJmpCfgBundle extends CSRBundle with DasicsConst{
    val cfg = (0 until NumDasicsJmpBounds).map{
        i => new Bundle {
            val v = RW(16*i  ).withReset(false.B)
        }
    }
}

