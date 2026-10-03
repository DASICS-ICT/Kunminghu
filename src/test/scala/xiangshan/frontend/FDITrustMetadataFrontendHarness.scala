// SPDX-License-Identifier: MulanPSL-2.0

package xiangshan.frontend

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.util._
import chisel3.util.experimental.BoringUtils.{bore => observe}
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.cache.mmu.TlbRequestIO
import xiangshan.frontend.icache.{ICacheMainPipeResp, ICachePMPBundle}

// Shared by the local frontend test and the actual Backend/FTQ recovery fixture.
// The image describes bytes at architectural instruction addresses, not DUT tags.
object FDITrustInstructionImage {
  val wordMask: BigInt = (BigInt(1) << 64) - 1

  def addi(rd: Int, immediate: Int): BigInt =
    (BigInt(immediate & 0xfff) << 20) | (BigInt(rd) << 7) | 0x13

  def bytes(instructions: Seq[(BigInt, BigInt, Int)]): Map[BigInt, Int] = {
    val result = scala.collection.mutable.Map.empty[BigInt, Int]
    for ((pc, instruction, size) <- instructions; index <- 0 until size) {
      require(size == 2 || size == 4)
      val address = (pc + index) & wordMask
      require(!result.contains(address), s"Instruction bytes overlap at $address")
      result(address) = ((instruction >> (8 * index)) & 255).toInt
    }
    result.toMap
  }

  // ICache returns selected 8-byte banks in circular line positions. IFU repeats
  // this one word and cuts from startAddr's halfword offset; it does not take two lines.
  def cacheWord(start: BigInt, image: Map[BigInt, Int], blockBytes: Int, predictWidth: Int): BigInt = {
    require(blockBytes % 8 == 0)
    val bankStart = start & ~BigInt(7)
    val neededBytes = (start - bankStart).toInt + 2 * (predictWidth + 1)
    val banks = (neededBytes + 7) / 8
    (0 until banks * 8).foldLeft(BigInt(0)) { (word, offset) =>
      val address = (bankStart + offset) & wordMask
      val byte = image.getOrElse(address, 0)
      word | (BigInt(byte) << (8 * (address % blockBytes).toInt))
    }
  }
}

class FDITrustMetadataFrontendHarness(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val ftq = Flipped(new FtqToIfuIO)
    val cacheReady = Input(Bool())
    val cacheResponse = Flipped(Valid(new ICacheMainPipeResp))
    val cacheStop = Output(Bool())
    val config = Option.when(HasFDI)(Input(new FDIFrontendConfig))
    val decodeCanAccept = Input(Bool())
    val decode = Vec(DecodeWidth, Decoupled(new CtrlFlow))
    val uncache = new UncacheInterface
    val tlb = new TlbRequestIO
    val pmp = new ICachePMPBundle
    val commits = Input(Vec(CommitWidth, Valid(new RobCommitInfo)))
    val mmioLastCommit = Input(Bool())
    val mmioCommitPointer = Output(new FtqPtr)
    val predecode = Output(Valid(new PredecodeWritebackBundle))
    val fetch = Output(Valid(new FetchToIBuffer))
    val fetchReady = Output(Bool())
    val bufferFlush = Output(Bool())
    val bufferFull = Output(Bool())
    val ibufferState = Output(new Bundle {
      val bypass = Bool()
      val entries = UInt(log2Ceil(IBufSize + 1).W)
      val enqueueCount = UInt(log2Ceil(PredictWidth + 1).W)
      val enqIndex = UInt(log2Ceil(IBufSize).W)
      val deqIndex = UInt(log2Ceil(IBufSize).W)
      val enqFlag = Bool()
      val deqFlag = Bool()
    })
    val f2 = Output(new Bundle {
      val valid = Bool()
      val fire = Bool()
      val flush = Bool()
      val request = new FetchRequestBundle
    })
    val f3Valid = Output(Bool())
    val f3Flush = Output(Bool())
  })

  // Real frontend children inherit the asynchronous core reset; no state is forced.
  val ifu = withReset(reset.asAsyncReset) { Module(new NewIFU) }
  val ibuffer = withReset(reset.asAsyncReset) { Module(new IBuffer) }

  def idleInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(idleInputs)
    case vector: Vec[_] => vector.foreach(idleInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  idleInputs(ifu.io)
  idleInputs(ibuffer.io)
  ifu.io.ftqInter.fromFtq <> io.ftq
  ifu.io.icacheInter.icacheReady := io.cacheReady
  ifu.io.icacheInter.resp := io.cacheResponse
  ifu.io.fdiConfig.foreach(_ := io.config.get)
  ifu.io.uncacheInter <> io.uncache
  ifu.io.iTLBInter <> io.tlb
  ifu.io.pmp <> io.pmp
  ifu.io.rob_commits := io.commits
  ifu.io.mmioCommitRead.mmioLastCommit := io.mmioLastCommit
  io.mmioCommitPointer := ifu.io.mmioCommitRead.mmioFtqPtr
  ibuffer.io.in <> ifu.io.toIbuffer
  ibuffer.io.out <> io.decode
  ibuffer.io.decodeCanAccept := io.decodeCanAccept
  // Frontend.needFlush has this register boundary on backend redirect.
  val bufferFlush = RegNext(io.ftq.redirect.valid, false.B)
  ibuffer.io.flush := bufferFlush
  io.bufferFlush := bufferFlush
  io.bufferFull := ibuffer.io.full
  io.ibufferState.bypass := observe(ibuffer.useBypass)
  io.ibufferState.entries := observe(ibuffer.numValid)
  io.ibufferState.enqueueCount := observe(ibuffer.numEnq)
  io.ibufferState.enqIndex := observe(ibuffer.enqPtr.value)
  io.ibufferState.deqIndex := observe(ibuffer.deqPtr.value)
  io.ibufferState.enqFlag := observe(ibuffer.enqPtr.flag)
  io.ibufferState.deqFlag := observe(ibuffer.deqPtr.flag)
  io.cacheStop := ifu.io.icacheStop
  io.predecode := ifu.io.ftqInter.toFtq.pdWb
  io.fetch.valid := ifu.io.toIbuffer.valid
  io.fetch.bits := ifu.io.toIbuffer.bits
  io.fetchReady := ifu.io.toIbuffer.ready
  io.f2.valid := observe(ifu.f2_valid)
  io.f2.fire := observe(ifu.f2_fire)
  io.f2.flush := observe(ifu.f2_flush)
  io.f2.request := observe(ifu.f2_ftq_req)
  io.f3Valid := observe(ifu.f3_valid)
  io.f3Flush := observe(ifu.f3_flush)

  require(ifu.io.fdiConfig.isDefined == HasFDI)
  require(ifu.io.toIbuffer.bits.fdiNotTrusted.isDefined == HasFDI)
  require(ibuffer.io.out.forall(_.bits.fdiNotTrusted.isDefined == HasFDI))
}
