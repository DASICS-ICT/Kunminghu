package xiangshan.backend.rename

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import xiangshan.backend.fu.NewCSR.CSROoORead

class RegisterPressureMonitorTest extends AnyFlatSpec with ChiselScalatestTester {
  import RegisterPressureCSR._

  private val capacities = Seq(10, 20, 63, 8, 1)
  private val resetOccupancies = Seq(5, 5, 32, 1, 1)
  private val histogramShifts = Seq.fill(PoolCount)(0)
  private val commitWidth = 4

  private def monitor = new RegisterPressureMonitor(
    capacities = capacities,
    resetOccupancies = resetOccupancies,
    histogramShifts = histogramShifts,
    commitWidth = commitWidth,
  )

  private def clearCommits(dut: RegisterPressureMonitor): Unit = {
    dut.io.commits.foreach { commit =>
      commit.valid.poke(false.B)
      commit.logicalDestination.poke(0.U)
      commit.integerWrite.poke(false.B)
      commit.floatingPointWrite.poke(false.B)
      commit.vectorWrite.poke(false.B)
      commit.vectorMaskWrite.poke(false.B)
      commit.vectorLengthWrite.poke(false.B)
    }
  }

  private def initialize(dut: RegisterPressureMonitor): Unit = {
    setOccupancies(dut, Seq.fill(PoolCount)(0))
    setStalls(dut, Seq.fill(PoolCount)(false))
    dut.io.samplePrivilegeAllowed.poke(false.B)
    dut.io.redirect.poke(false.B)
    dut.io.walk.poke(false.B)
    clearCommits(dut)
    dut.io.csrWrite.valid.poke(false.B)
    dut.io.csrWrite.bits.addr.poke(0.U)
    dut.io.csrWrite.bits.data.poke(0.U)
  }

  private def setOccupancies(dut: RegisterPressureMonitor, values: Seq[Int]): Unit = {
    require(values.length == PoolCount)
    dut.io.currentOccupancy.zip(values).foreach { case (occupancy, value) =>
      occupancy.poke(value.U)
    }
  }

  private def setStalls(dut: RegisterPressureMonitor, values: Seq[Boolean]): Unit = {
    require(values.length == PoolCount)
    dut.io.pressureStall.zip(values).foreach { case (stall, value) =>
      stall.poke(value.B)
    }
  }

  private def writeCsr(dut: RegisterPressureMonitor, address: Int, value: BigInt): Unit = {
    dut.io.csrWrite.valid.poke(true.B)
    dut.io.csrWrite.bits.addr.poke(address.U)
    dut.io.csrWrite.bits.data.poke(value.U)
    dut.clock.step()
    dut.io.csrWrite.valid.poke(false.B)
  }

  private def command(dut: RegisterPressureMonitor, value: Int): Unit = {
    writeCsr(dut, ControlAddress, value)
  }

  private def select(dut: RegisterPressureMonitor, index: Int): Unit = {
    writeCsr(dut, IndexAddress, index)
  }

  private def expectSelected(dut: RegisterPressureMonitor, index: Int, expected: BigInt): Unit = {
    select(dut, index)
    dut.clock.step()
    dut.io.selectedData.expect(expected.U)
  }

  private def prepareSample(
    dut: RegisterPressureMonitor,
    occupancies: Seq[Int],
    stalls: Seq[Boolean] = Seq.fill(PoolCount)(false),
    redirect: Boolean = false,
    walk: Boolean = false,
  ): Unit = {
    dut.io.csrWrite.valid.poke(false.B)
    setOccupancies(dut, occupancies)
    setStalls(dut, stalls)
    dut.io.redirect.poke(redirect.B)
    dut.io.walk.poke(walk.B)
    clearCommits(dut)
  }

  private def setCommit(
    dut: RegisterPressureMonitor,
    slot: Int,
    valid: Boolean,
    logicalDestination: Int,
    integerWrite: Boolean = false,
    floatingPointWrite: Boolean = false,
    vectorWrite: Boolean = false,
    vectorMaskWrite: Boolean = false,
    vectorLengthWrite: Boolean = false,
  ): Unit = {
    val commit = dut.io.commits(slot)
    commit.valid.poke(valid.B)
    commit.logicalDestination.poke(logicalDestination.U)
    commit.integerWrite.poke(integerWrite.B)
    commit.floatingPointWrite.poke(floatingPointWrite.B)
    commit.vectorWrite.poke(vectorWrite.B)
    commit.vectorMaskWrite.poke(vectorMaskWrite.B)
    commit.vectorLengthWrite.poke(vectorLengthWrite.B)
  }

  private def poolIndex(pool: Int, offset: Int): Int = poolBase(pool) + offset

  private def pack(values: Seq[Int], width: Int): BigInt = {
    values.zipWithIndex.map { case (value, index) => BigInt(value) << (index * width) }.sum
  }

  behavior of "RegisterPressureMonitor"

  it should "serialize indexed data reads behind older CSR writes" in {
    assert(CSROoORead.waitForwardInOrderCsrReadList.contains(DataAddress))
  }

  it should "decode all global entries through the indexed table" in {
    test(monitor) { dut =>
      initialize(dut)

      val expected = Seq(
        AbiInfo,
        BigInt(0),
        BigInt(0),
        BigInt(0),
        BigInt(0),
        BigInt(0),
        BigInt(0),
        BigInt(0),
        BigInt(0),
        pack(capacities, OccupancyWidth),
        pack(resetOccupancies, OccupancyWidth),
        BigInt(CounterWidth) << 20,
      ) ++ Seq.fill(4)(BigInt(0))

      expected.zipWithIndex.foreach { case (value, index) =>
        expectSelected(dut, index, value)
      }
    }
  }

  it should "honor all and user-only control, freeze, clear, and repeated starts" in {
    test(monitor) { dut =>
      initialize(dut)

      command(dut, StartAll)

      prepareSample(dut, Seq(1, 2, 3, 1, 1))
      setCommit(dut, slot = 0, valid = true, logicalDestination = 1, integerWrite = true)
      dut.clock.step()

      setOccupancies(dut, Seq(7, 8, 9, 2, 0))
      command(dut, Freeze)
      expectSelected(dut, 0x001, 0xc)
      expectSelected(dut, 0x002, 1)
      expectSelected(dut, poolIndex(IntegerPool, 0x01), 1)
      expectSelected(dut, 0x005, BigInt(1) << 1)

      prepareSample(
        dut,
        occupancies = Seq(10, 20, 63, 8, 1),
        stalls = Seq.fill(PoolCount)(true),
        redirect = true,
        walk = true,
      )
      setCommit(dut, slot = 0, valid = true, logicalDestination = 2, integerWrite = true)
      dut.clock.step()
      expectSelected(dut, 0x002, 1)
      expectSelected(dut, 0x003, 0)
      expectSelected(dut, 0x005, BigInt(1) << 1)

      command(dut, StartUser)
      expectSelected(dut, 0x002, 0)

      prepareSample(dut, Seq(8, 16, 32, 4, 1), redirect = true, walk = true)
      setCommit(dut, slot = 0, valid = true, logicalDestination = 2, integerWrite = true)
      dut.clock.step()
      expectSelected(dut, 0x002, 0)
      expectSelected(dut, 0x005, 0)
      expectSelected(dut, 0x001, 0x3)

      dut.io.samplePrivilegeAllowed.poke(true.B)
      prepareSample(dut, Seq(6, 12, 24, 3, 1))
      setCommit(dut, slot = 0, valid = true, logicalDestination = 3, integerWrite = true)
      dut.clock.step()
      setOccupancies(dut, Seq(5, 10, 20, 2, 0))
      command(dut, Freeze)
      expectSelected(dut, 0x001, 0xe)
      expectSelected(dut, 0x002, 1)
      expectSelected(dut, 0x005, BigInt(1) << 3)
      expectSelected(dut, poolIndex(IntegerPool, 0x09), 1)

      command(dut, Clear)
      expectSelected(dut, 0x001, 0)
      expectSelected(dut, 0x002, 0)
      expectSelected(dut, poolIndex(IntegerPool, 0x01), 0)

      command(dut, StartAll)
      setOccupancies(dut, Seq(5, 10, 20, 2, 0))
      command(dut, Freeze)
      expectSelected(dut, 0x001, 0x4)
      expectSelected(dut, 0x002, 0)
      expectSelected(dut, poolIndex(IntegerPool, 0x01), 5)

      command(dut, Clear)
      expectSelected(dut, poolIndex(IntegerPool, 0x01), 0)
      expectSelected(dut, 0x00c, 0)
      expectSelected(dut, 0x010, 0)
      expectSelected(dut, 0x01f, 0)
      expectSelected(dut, poolIndex(IntegerPool, 0x0f), 0)
      expectSelected(dut, 0x160, 0)
      expectSelected(dut, 0x1ff, 0)
    }
  }

  it should "accumulate peaks, sums, tails, histograms, stalls, redirects, and walks" in {
    test(monitor) { dut =>
      initialize(dut)
      command(dut, StartAll)

      prepareSample(
        dut,
        occupancies = Seq(8, 17, 30, 1, 0),
        stalls = Seq(false, true, false, false, false),
        walk = true,
      )
      dut.clock.step()
      prepareSample(
        dut,
        occupancies = Seq(9, 18, 31, 2, 1),
        stalls = Seq(true, false, false, false, false),
        redirect = true,
      )
      dut.clock.step()
      prepareSample(
        dut,
        occupancies = Seq(10, 19, 32, 3, 1),
        stalls = Seq(true, true, false, false, false),
        walk = true,
      )
      dut.clock.step()
      prepareSample(
        dut,
        occupancies = Seq(10, 20, 63, 4, 0),
        stalls = Seq(false, true, false, false, false),
        redirect = true,
        walk = true,
      )
      dut.clock.step()

      setOccupancies(dut, Seq(7, 16, 29, 5, 0))
      command(dut, Freeze)

      expectSelected(dut, 0x002, 4)
      expectSelected(dut, 0x003, 2)
      expectSelected(dut, 0x004, 3)

      expectSelected(dut, poolIndex(IntegerPool, 0x01), 10)
      expectSelected(dut, poolIndex(IntegerPool, 0x02), 10)
      expectSelected(dut, poolIndex(IntegerPool, 0x03), 37)
      expectSelected(dut, poolIndex(IntegerPool, 0x04), 2)
      expectSelected(dut, poolIndex(IntegerPool, 0x05), 3)
      expectSelected(dut, poolIndex(IntegerPool, 0x06), 2)
      expectSelected(dut, poolIndex(IntegerPool, 0x07), 2)
      expectSelected(dut, poolIndex(IntegerPool, 0x08), 2)

      expectSelected(dut, poolIndex(FloatingPointPool, 0x01), 20)
      expectSelected(dut, poolIndex(FloatingPointPool, 0x02), 20)
      expectSelected(dut, poolIndex(FloatingPointPool, 0x03), 74)
      expectSelected(dut, poolIndex(FloatingPointPool, 0x04), 3)
      expectSelected(dut, poolIndex(FloatingPointPool, 0x05), 3)
      expectSelected(dut, poolIndex(FloatingPointPool, 0x06), 2)
      expectSelected(dut, poolIndex(FloatingPointPool, 0x07), 1)
      expectSelected(dut, poolIndex(FloatingPointPool, 0x08), 3)

      expectSelected(dut, poolIndex(VectorPool, 0x02), 63)
      expectSelected(dut, poolIndex(VectorPool, 0x03), 156)
      expectSelected(dut, poolIndex(VectorPool, 0x04), 3)
      expectSelected(dut, poolIndex(VectorPool, HistogramBase + 30), 1)
      expectSelected(dut, poolIndex(VectorPool, HistogramBase + 31), 3)
    }
  }

  it should "track architectural and internal committed destinations for all pools" in {
    test(monitor) { dut =>
      initialize(dut)
      command(dut, StartAll)

      prepareSample(dut, Seq(1, 2, 3, 1, 1))
      setCommit(dut, slot = 0, valid = true, logicalDestination = 1, integerWrite = true)
      setCommit(dut, slot = 1, valid = true, logicalDestination = 31, floatingPointWrite = true)
      setCommit(dut, slot = 2, valid = true, logicalDestination = 0, vectorMaskWrite = true)
      setCommit(dut, slot = 3, valid = true, logicalDestination = 32, vectorWrite = true)
      dut.clock.step()

      prepareSample(dut, Seq(1, 2, 3, 1, 1))
      setCommit(dut, slot = 0, valid = true, logicalDestination = 32, floatingPointWrite = true)
      setCommit(dut, slot = 1, valid = true, logicalDestination = 31, vectorWrite = true)
      setCommit(dut, slot = 2, valid = true, logicalDestination = 46, vectorWrite = true)
      setCommit(dut, slot = 3, valid = true, logicalDestination = 0, vectorLengthWrite = true)
      dut.clock.step()

      prepareSample(dut, Seq(1, 2, 3, 1, 1))
      setCommit(dut, slot = 0, valid = true, logicalDestination = 33, floatingPointWrite = true)
      setCommit(dut, slot = 2, valid = true, logicalDestination = 0, integerWrite = true)
      setCommit(
        dut,
        slot = 1,
        valid = false,
        logicalDestination = 2,
        integerWrite = true,
        floatingPointWrite = true,
        vectorWrite = true,
        vectorMaskWrite = true,
        vectorLengthWrite = true,
      )
      dut.clock.step()

      command(dut, Freeze)

      expectSelected(dut, 0x005, BigInt(1) << 1)
      expectSelected(dut, 0x006, BigInt(1) << 31)
      expectSelected(dut, 0x007, (BigInt(1) << 31) | 1)
      expectSelected(
        dut,
        0x008,
        (BigInt(1) << 17) | (BigInt(1) << 16) | (BigInt(1) << 2) | 3,
      )

      expectSelected(dut, poolIndex(IntegerPool, 0x09), 2)
      expectSelected(dut, poolIndex(FloatingPointPool, 0x09), 3)
      expectSelected(dut, poolIndex(VectorPool, 0x09), 3)
      expectSelected(dut, poolIndex(VectorMaskPool, 0x09), 1)
      expectSelected(dut, poolIndex(VectorLengthPool, 0x09), 1)
    }
  }
}
