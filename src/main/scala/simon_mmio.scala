
import chisel3._
import chisel3.utils._

case class SimonParameters(address: BigInt)

trait SimonModule extends HasRegMap {
  implicit val p: Parameters
  def params: SimonParameters

  private val REG_SCONF = 0x00
  private val REG_KEY1 = 0x08
  private val REG_KEY2 = 0x10
  private val REG_DATA1 = 0x18
  private val REG_DATA2 = 0x20

  val regKeyH = Reg(UInt(64.W))
  val regKeyL = Reg(UInt(64.W))
  val regData1 = Reg(UInt(64.W))
  val regData2 = Reg(UInt(64.W))
  val regWSconf = RegInit(0.U(64.W))
  val regRSconf = Wire(UInt(64.W))
  regRSconf := Cat(0.U((64-6).W), regWSconf(2, 0))

  // internal stuff
  val kExpStart = RegInit(false.B)

  def readSConf(ready: Bool): (Bool, UInt) = {
    (true.B, regRSconf)
  }

  def writeSConf(valid: Bool, bits: UInt): Bool = {
    when (valid) { regWSconf := bits }
    true.B
  }

  def readKey(ready: Bool): (Bool, UInt) = {
    (true.B, 0.U)
  }

  def writeKeyH(valid: Bool, bits: UInt): Bool = {
    when (valid && !core.io.kExpBusy) {
      regKeyH := bits
      kExpStart := true.B
    }
    true.B
  }

  def writeKeyL(valid: Bool, bits: UInt): Bool = {
    when (valid && !core.io.kExpBusy) { regKeyL := bits  }
    true.B
  }

  def writeData1(valid: Bool, bits: UInt): Bool = {
    when (valid && core.io.dInReady) { regData1 := bits  }
    true.B
  }

  def writeData2(valid: Bool, bits: UInt): Bool = {
    when (valid && core.io.dInReady) {
      regData2 := bits
      core.io.dInValid := true.B
    }
    true.B
  }

  def readData1(ready: Bool): (Bool, UInt) = {
    (true.B, core.io.data1Out)
  }

  def readData2(ready: Bool): (Bool, UInt) = {
    (true.B, core.io.data2Out)
  }

  val core = Module(new SimonCore(64))
  core.io.keyH := regKeyH
  core.io.keyL := regKeyL
  core.io.data1In := regData1
  core.io.data2In := regData2
  core.io.sMode := regWSconf(0)
  core.io.dEncDec := regWSconf(1)
  core.io.rSingle := regWSconf(2)
  core.io.kValid := kExpStart

  regmap(
    0x00 -> Seq(RegField(64, readSConf, writeSConf)),
    0x08 -> Seq(RegField(64, readKey, writeKeyL)),
    0x10 -> Seq(RegField(64, readKey, writeKeyH)),
    0x18 -> Seq(RegField(64, readData1, writeData2)),
    0x20 -> Seq(RegField(64, readData2, writeData2))
  )
}
