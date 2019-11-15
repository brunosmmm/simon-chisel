package SimonAcc

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.subsystem.BaseSubsystem
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.UIntIsOneOf

case class SimonParams(address: BigInt, beatBytes: Int)

trait SimonBundle extends Bundle {
}

trait SimonModule extends HasRegMap {
  implicit val p: Parameters
  def params: SimonParams

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

  // internal stuff
  val dataValid = RegInit(false.B)
  val kExpStart = RegInit(false.B)

  val core = Module(new SimonCore(64, 128))
  core.io.keyH := regKeyH
  core.io.keyL := regKeyL
  core.io.data1In := regData1
  core.io.data2In := regData2
  core.io.sMode := regWSconf(0)
  core.io.dEncDec := regWSconf(1)
  core.io.rSingle := regWSconf(2)
  core.io.kValid := kExpStart
  core.io.dInValid := dataValid

  val regRSconf = Wire(UInt(64.W))
  regRSconf := Cat(0.U((64-5).W), core.io.dInReady, core.io.kExpDone, regWSconf(2, 0))

  // self-clearing bit?
  when (dataValid) {
    dataValid := false.B
  }

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
    when (valid && core.io.kReady) {
      regKeyH := bits
      kExpStart := true.B
    }
    true.B
  }

  def writeKeyL(valid: Bool, bits: UInt): Bool = {
    when (valid && core.io.kReady) { regKeyL := bits  }
    true.B
  }

  def writeData1(valid: Bool, bits: UInt): Bool = {
    when (valid && core.io.dInReady) { regData1 := bits  }
    true.B
  }

  // how can I make bits that clear themselves? not having an explicit clock is detrimental
  def writeData2(valid: Bool, bits: UInt): Bool = {
    when (valid && core.io.dInReady) {
      regData2 := bits
      dataValid := true.B
    }
    true.B
  }

  def readData1(ready: Bool): (Bool, UInt) = {
    (true.B, core.io.data1Out)
  }

  def readData2(ready: Bool): (Bool, UInt) = {
    (true.B, core.io.data2Out)
  }

  regmap(
    0x00 -> Seq(RegField(64, readSConf(_), writeSConf(_,_))),
    0x08 -> Seq(RegField(64, readKey(_), writeKeyL(_,_))),
    0x10 -> Seq(RegField(64, readKey(_), writeKeyH(_,_))),
    0x18 -> Seq(RegField(64, readData1(_), writeData2(_,_))),
    0x20 -> Seq(RegField(64, readData2(_), writeData2(_,_)))
  )
}

class SimonTL(c: SimonParams)(implicit p: Parameters)
  extends TLRegisterRouter(
    c.address, "simon", Seq("esl,simon"),
    beatBytes = c.beatBytes)(
      new TLRegBundle(c, _) with SimonBundle)(
      new TLRegModule(c, _, _) with SimonModule)

trait HasPeripherySimonTL { this: BaseSubsystem =>
  implicit val p: Parameters

  private val address = 0x1000000
  private val portName = "simon"

  val simon = LazyModule(new SimonTL(
    SimonParams(address, pbus.beatBytes))(p))

  pbus.toVariableWidthSlave(Some(portName)) { simon.node }
}

trait HasPeripherySimonTLModuleImp extends LazyModuleImp {
  implicit val p: Parameters
  val outer: HasPeripherySimonTL
}
