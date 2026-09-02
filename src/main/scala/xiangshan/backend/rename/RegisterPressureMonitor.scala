package xiangshan.backend.rename

import chisel3._
import chisel3.util._

object RegisterPressureCSR {
  final val ControlAddress = 0x800
  final val IndexAddress = 0x801
  final val InfoAddress = 0xcc0
  final val DataAddress = 0xcc1

  final val StartAll = 1
  final val StartUser = 2
  final val Freeze = 3
  final val Clear = 4

  final val PoolCount = 5
  final val HistogramBins = 32
  final val CounterWidth = 48
  final val OccupancyWidth = 9
  final val ReadIndexWidth = 9
  final val CommitLogicalIndexWidth = 6

  final val PoolBlockBase = 0x20
  final val PoolBlockSize = 0x40
  final val HistogramBase = 0x10

  final val AbiMajor = 1
  final val AbiMinor = 0
  final val AbiInfo: BigInt =
    (BigInt(0x4b4d4850L) << 32) |
      (BigInt(AbiMajor) << 24) |
      (BigInt(AbiMinor) << 16) |
      (BigInt(PoolCount) << 8) |
      BigInt(HistogramBins)

  final val IntegerPool = 0
  final val FloatingPointPool = 1
  final val VectorPool = 2
  final val VectorMaskPool = 3
  final val VectorLengthPool = 4

  def poolBase(pool: Int): Int = PoolBlockBase + pool * PoolBlockSize
}

class RegisterPressureCSRWrite extends Bundle {
  val addr = UInt(12.W)
  val data = UInt(64.W)
}

class RegisterPressureCommitEvent extends Bundle {
  val valid = Bool()
  val logicalDestination = UInt(RegisterPressureCSR.CommitLogicalIndexWidth.W)
  val integerWrite = Bool()
  val floatingPointWrite = Bool()
  val vectorWrite = Bool()
  val vectorMaskWrite = Bool()
  val vectorLengthWrite = Bool()
}

class RegisterPressureMonitor(
  capacities: Seq[Int],
  resetOccupancies: Seq[Int],
  histogramShifts: Seq[Int],
  commitWidth: Int,
) extends Module {
  import RegisterPressureCSR._

  require(capacities.length == PoolCount)
  require(resetOccupancies.length == PoolCount)
  require(histogramShifts.length == PoolCount)
  require(capacities.forall(capacity => capacity > 0 && capacity < (1 << OccupancyWidth)))
  require(resetOccupancies.zip(capacities).forall { case (baseline, capacity) =>
    baseline >= 0 && baseline <= capacity
  })
  require(histogramShifts.forall(shift => shift >= 0 && shift < OccupancyWidth))
  require(commitWidth > 0)

  val io = IO(new Bundle {
    val currentOccupancy = Input(Vec(PoolCount, UInt(OccupancyWidth.W)))
    val pressureStall = Input(Vec(PoolCount, Bool()))
    val samplePrivilegeAllowed = Input(Bool())
    val redirect = Input(Bool())
    val walk = Input(Bool())
    val commits = Input(Vec(commitWidth, new RegisterPressureCommitEvent))
    val csrWrite = Input(Valid(new RegisterPressureCSRWrite))
    val selectedData = Output(UInt(64.W))
  })

  private val active = RegInit(false.B)
  private val userOnly = RegInit(false.B)
  private val frozen = RegInit(false.B)
  private val hasSamples = RegInit(false.B)
  private val readIndex = RegInit(0.U(ReadIndexWidth.W))

  private val sampleCycles = RegInit(0.U(64.W))
  private val redirectCount = RegInit(0.U(64.W))
  private val walkCycles = RegInit(0.U(64.W))

  private val endOccupancy = RegInit(VecInit(Seq.fill(PoolCount)(0.U(OccupancyWidth.W))))
  private val lastSampleOccupancy = RegInit(VecInit(Seq.fill(PoolCount)(0.U(OccupancyWidth.W))))
  private val peakOccupancy = RegInit(VecInit(Seq.fill(PoolCount)(0.U(OccupancyWidth.W))))
  private val firstPeakCycle = RegInit(VecInit(Seq.fill(PoolCount)(0.U(64.W))))
  private val occupancySum = RegInit(VecInit(Seq.fill(PoolCount)(0.U(64.W))))
  private val tail90Cycles = RegInit(VecInit(Seq.fill(PoolCount)(0.U(CounterWidth.W))))
  private val tail95Cycles = RegInit(VecInit(Seq.fill(PoolCount)(0.U(CounterWidth.W))))
  private val tail99Cycles = RegInit(VecInit(Seq.fill(PoolCount)(0.U(CounterWidth.W))))
  private val pressureStallCycles = RegInit(VecInit(Seq.fill(PoolCount)(0.U(CounterWidth.W))))
  private val committedDestinationWrites = RegInit(VecInit(Seq.fill(PoolCount)(0.U(CounterWidth.W))))
  private val histograms = Seq.fill(PoolCount)(
    RegInit(VecInit(Seq.fill(HistogramBins)(0.U(CounterWidth.W))))
  )

  private val integerDestinationMask = RegInit(0.U(32.W))
  private val floatingPointDestinationMask = RegInit(0.U(32.W))
  private val vectorDestinationMask = RegInit(0.U(32.W))
  private val internalFloatingPointDestinationMask = RegInit(0.U(2.W))
  private val internalVectorDestinationMask = RegInit(0.U(15.W))
  private val vectorLengthWritten = RegInit(false.B)

  private val controlWrite = io.csrWrite.valid && io.csrWrite.bits.addr === ControlAddress.U
  private val command = io.csrWrite.bits.data(7, 0)
  private val startAll = controlWrite && command === StartAll.U
  private val startUser = controlWrite && command === StartUser.U
  private val freeze = controlWrite && command === Freeze.U
  private val clear = controlWrite && command === Clear.U

  when(io.csrWrite.valid && io.csrWrite.bits.addr === IndexAddress.U) {
    readIndex := io.csrWrite.bits.data(ReadIndexWidth - 1, 0)
  }

  private val sampleThisCycle = active && (!userOnly || io.samplePrivilegeAllowed)

  private def saturatingCounterAdd(counter: UInt, increment: UInt): UInt = {
    val extended = counter +& increment
    Mux(extended(CounterWidth), Fill(CounterWidth, 1.U), extended(CounterWidth - 1, 0))
  }

  private def clearStatistics(): Unit = {
    hasSamples := false.B
    sampleCycles := 0.U
    redirectCount := 0.U
    walkCycles := 0.U
    endOccupancy.foreach(_ := 0.U)
    lastSampleOccupancy.foreach(_ := 0.U)
    peakOccupancy.foreach(_ := 0.U)
    firstPeakCycle.foreach(_ := 0.U)
    occupancySum.foreach(_ := 0.U)
    tail90Cycles.foreach(_ := 0.U)
    tail95Cycles.foreach(_ := 0.U)
    tail99Cycles.foreach(_ := 0.U)
    pressureStallCycles.foreach(_ := 0.U)
    committedDestinationWrites.foreach(_ := 0.U)
    histograms.foreach(_.foreach(_ := 0.U))
    integerDestinationMask := 0.U
    floatingPointDestinationMask := 0.U
    vectorDestinationMask := 0.U
    internalFloatingPointDestinationMask := 0.U
    internalVectorDestinationMask := 0.U
    vectorLengthWritten := false.B
  }

  when(startAll || startUser || clear) {
    clearStatistics()
    active := startAll || startUser
    userOnly := startUser
    frozen := false.B
  }.elsewhen(freeze) {
    active := false.B
    frozen := true.B
    endOccupancy := Mux(hasSamples, lastSampleOccupancy, io.currentOccupancy)
  }.elsewhen(sampleThisCycle) {
    hasSamples := true.B
    sampleCycles := sampleCycles + 1.U
    when(io.redirect) {
      redirectCount := redirectCount + 1.U
    }
    when(io.walk) {
      walkCycles := walkCycles + 1.U
    }

    for (pool <- 0 until PoolCount) {
      val occupancy = io.currentOccupancy(pool)
      val shiftedBin = occupancy >> histogramShifts(pool)
      val histogramBin = Mux(
        shiftedBin >= (HistogramBins - 1).U,
        (HistogramBins - 1).U,
        shiftedBin,
      )(log2Ceil(HistogramBins) - 1, 0)

      lastSampleOccupancy(pool) := occupancy
      occupancySum(pool) := occupancySum(pool) + occupancy
      histograms(pool)(histogramBin) := saturatingCounterAdd(histograms(pool)(histogramBin), 1.U)
      when(occupancy > peakOccupancy(pool)) {
        peakOccupancy(pool) := occupancy
        firstPeakCycle(pool) := sampleCycles
      }
      when(occupancy >= ((capacities(pool) * 90 + 99) / 100).U) {
        tail90Cycles(pool) := saturatingCounterAdd(tail90Cycles(pool), 1.U)
      }
      when(occupancy >= ((capacities(pool) * 95 + 99) / 100).U) {
        tail95Cycles(pool) := saturatingCounterAdd(tail95Cycles(pool), 1.U)
      }
      when(occupancy >= ((capacities(pool) * 99 + 99) / 100).U) {
        tail99Cycles(pool) := saturatingCounterAdd(tail99Cycles(pool), 1.U)
      }
      when(io.pressureStall(pool)) {
        pressureStallCycles(pool) := saturatingCounterAdd(pressureStallCycles(pool), 1.U)
      }
    }

    val integerWrites = PopCount(io.commits.map(commit => commit.valid && commit.integerWrite))
    val floatingPointWrites = PopCount(io.commits.map(commit => commit.valid && commit.floatingPointWrite))
    val vectorWrites = PopCount(io.commits.map(commit => commit.valid && commit.vectorWrite))
    val vectorMaskWrites = PopCount(io.commits.map(commit => commit.valid && commit.vectorMaskWrite))
    val vectorLengthWrites = PopCount(io.commits.map(commit => commit.valid && commit.vectorLengthWrite))
    committedDestinationWrites(IntegerPool) :=
      saturatingCounterAdd(committedDestinationWrites(IntegerPool), integerWrites)
    committedDestinationWrites(FloatingPointPool) :=
      saturatingCounterAdd(committedDestinationWrites(FloatingPointPool), floatingPointWrites)
    committedDestinationWrites(VectorPool) :=
      saturatingCounterAdd(committedDestinationWrites(VectorPool), vectorWrites)
    committedDestinationWrites(VectorMaskPool) :=
      saturatingCounterAdd(committedDestinationWrites(VectorMaskPool), vectorMaskWrites)
    committedDestinationWrites(VectorLengthPool) :=
      saturatingCounterAdd(committedDestinationWrites(VectorLengthPool), vectorLengthWrites)

    val integerUpdates = VecInit(io.commits.map(commit =>
      Mux(
        commit.valid && commit.integerWrite &&
          commit.logicalDestination =/= 0.U && commit.logicalDestination < 32.U,
        UIntToOH(commit.logicalDestination, 32),
        0.U(32.W),
      )
    )).reduce(_ | _)
    val floatingPointUpdates = VecInit(io.commits.map(commit =>
      Mux(
        commit.valid && commit.floatingPointWrite && commit.logicalDestination < 32.U,
        UIntToOH(commit.logicalDestination, 32),
        0.U(32.W),
      )
    )).reduce(_ | _)
    val vectorUpdates = VecInit(io.commits.map(commit =>
      Mux(
        commit.valid && commit.vectorWrite && commit.logicalDestination > 0.U && commit.logicalDestination < 32.U,
        UIntToOH(commit.logicalDestination, 32),
        Mux(commit.valid && commit.vectorMaskWrite, 1.U(32.W), 0.U(32.W)),
      )
    )).reduce(_ | _)
    val internalFloatingPointUpdates = VecInit((32 until 34).map(logicalIndex =>
      io.commits.map(commit =>
        commit.valid && commit.floatingPointWrite && commit.logicalDestination === logicalIndex.U
      ).reduce(_ || _)
    )).asUInt
    val internalVectorUpdates = VecInit((32 until 47).map(logicalIndex =>
      io.commits.map(commit =>
        commit.valid && commit.vectorWrite && commit.logicalDestination === logicalIndex.U
      ).reduce(_ || _)
    )).asUInt

    integerDestinationMask := integerDestinationMask | integerUpdates
    floatingPointDestinationMask := floatingPointDestinationMask | floatingPointUpdates
    vectorDestinationMask := vectorDestinationMask | vectorUpdates
    internalFloatingPointDestinationMask := internalFloatingPointDestinationMask | internalFloatingPointUpdates
    internalVectorDestinationMask := internalVectorDestinationMask | internalVectorUpdates
    vectorLengthWritten := vectorLengthWritten || io.commits.map(commit =>
      commit.valid && commit.vectorLengthWrite
    ).reduce(_ || _)
  }

  for (pool <- 0 until PoolCount) {
    assert(io.currentOccupancy(pool) <= capacities(pool).U)
  }

  private def packedConstants(values: Seq[Int], width: Int): UInt = {
    Cat(values.reverse.map(value => value.U(width.W)))
  }

  private val capacityMetadata = packedConstants(capacities, OccupancyWidth)
  private val resetOccupancyMetadata = packedConstants(resetOccupancies, OccupancyWidth)
  private val histogramMetadata = Cat(
    CounterWidth.U(8.W),
    packedConstants(histogramShifts, 4),
  )
  private val internalDestinationMasks = Cat(
    0.U(46.W),
    vectorLengthWritten,
    internalVectorDestinationMask,
    internalFloatingPointDestinationMask,
  )

  private val globalDataTable = VecInit(Seq(
    AbiInfo.U(64.W),
    Cat(0.U(60.W), hasSamples, frozen, userOnly, active),
    sampleCycles,
    redirectCount,
    walkCycles,
    integerDestinationMask.pad(64),
    floatingPointDestinationMask.pad(64),
    vectorDestinationMask.pad(64),
    internalDestinationMasks,
    capacityMetadata.pad(64),
    resetOccupancyMetadata.pad(64),
    histogramMetadata.pad(64),
  ) ++ Seq.fill(4)(0.U(64.W)))

  // Keep the global decoder as an indexed table so FPGA synthesis cannot prune
  // it while retaining only the independently indexed physical-pool decoder.
  private val globalData = WireDefault(0.U(64.W))
  when(!readIndex(ReadIndexWidth - 1, 4).orR) {
    globalData := globalDataTable(readIndex(3, 0))
  }
  dontTouch(globalData)

  private val poolOffset = (readIndex - PoolBlockBase.U)(5, 0)
  private val poolNumber = ((readIndex - PoolBlockBase.U) >> log2Ceil(PoolBlockSize))(2, 0)

  private def poolMetadata(pool: Int): UInt = {
    val threshold90 = (capacities(pool) * 90 + 99) / 100
    val threshold95 = (capacities(pool) * 95 + 99) / 100
    val threshold99 = (capacities(pool) * 99 + 99) / 100
    Cat(
      0.U(15.W),
      threshold99.U(OccupancyWidth.W),
      threshold95.U(OccupancyWidth.W),
      threshold90.U(OccupancyWidth.W),
      histogramShifts(pool).U(4.W),
      resetOccupancies(pool).U(OccupancyWidth.W),
      capacities(pool).U(OccupancyWidth.W),
    )
  }

  private def selectedPoolData(pool: Int): UInt = {
    val histogramSelected = poolOffset >= HistogramBase.U &&
      poolOffset < (HistogramBase + HistogramBins).U
    val histogramData = Mux(
      histogramSelected,
      histograms(pool)((poolOffset - HistogramBase.U)(log2Ceil(HistogramBins) - 1, 0)),
      0.U,
    )
    MuxLookup(poolOffset, histogramData)(Seq(
      0x00.U -> io.currentOccupancy(pool),
      0x01.U -> endOccupancy(pool),
      0x02.U -> peakOccupancy(pool),
      0x03.U -> occupancySum(pool),
      0x04.U -> firstPeakCycle(pool),
      0x05.U -> tail90Cycles(pool),
      0x06.U -> tail95Cycles(pool),
      0x07.U -> tail99Cycles(pool),
      0x08.U -> pressureStallCycles(pool),
      0x09.U -> committedDestinationWrites(pool),
      0x0a.U -> poolMetadata(pool),
    ))
  }

  private val inPoolBlock = readIndex >= PoolBlockBase.U &&
    readIndex < (PoolBlockBase + PoolCount * PoolBlockSize).U
  private val poolData = MuxLookup(poolNumber, 0.U(64.W))(
    (0 until PoolCount).map(pool => pool.U -> selectedPoolData(pool))
  )

  io.selectedData := RegNext(Mux(inPoolBlock, poolData, globalData), 0.U)
}
