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

case class SimonParams(address: BigInt, beatBytes: Int, regSize: Int = 64)

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

  val SIMON_ID_1 = 0x53494d4f
  val SIMON_ID_2 = 0x4e313238

  val regKeyH = Reg(UInt(64.W))
  val regKeyL = Reg(UInt(64.W))
  val regData1 = Reg(UInt(64.W))
  val regData2 = Reg(UInt(64.W))
  val regWSconf = RegInit(0.U(64.W))

  // internal stuff
  val dataValid = RegInit(false.B)
  val kExpStart = RegInit(false.B)
  val wroteData1 = RegInit(false.B)

  val core = Module(new SimonCore(64, 128, false))
  core.io.keyH := regKeyH
  core.io.keyL := regKeyL
  core.io.data1In := regData1
  core.io.data2In := regData2
  core.io.sMode := regWSconf(0)
  core.io.dEncDec := regWSconf(1)
  core.io.rSingle := regWSconf(2)
  core.io.kValid := kExpStart
  core.io.dInValid := dataValid

  val regRSconf = Wire(UInt(params.regSize.W))
  regRSconf := Cat(0.U((params.regSize-5).W), core.io.dInReady, core.io.kExpDone, regWSconf(2, 0))

  // self-clearing bit?
  when (dataValid) {
    dataValid := false.B
  }

  when (kExpStart) {
    kExpStart := false.B
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
    when (valid && core.io.dInReady) {
      regData1 := bits
      wroteData1 := true.B
    }
    true.B
  }

  // how can I make bits that clear themselves? not having an explicit clock is detrimental
  def writeData2(valid: Bool, bits: UInt): Bool = {
    when (valid && core.io.dInReady) {
      // update data register 1 value when re-starting
      when (!wroteData1) {
        regData1 := core.io.data1Out
      }.otherwise {
        wroteData1 := false.B
      }
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

  def readID(ready: Bool): (Bool, UInt) = {
    (true.B, Cat(SIMON_ID_1.U(32.W), SIMON_ID_2.U(32.W)))
  }

  def ignoreWrite(valid: Bool, bits: UInt): Bool = {
    true.B
  }

  if (params.regSize == 64) {
    regmap(
      0x00 -> Seq(RegField(64, readSConf(_), writeSConf(_,_))),
      0x08 -> Seq(RegField(64, readKey(_), writeKeyL(_,_))),
      0x10 -> Seq(RegField(64, readKey(_), writeKeyH(_,_))),
      0x18 -> Seq(RegField(64, readData1(_), writeData1(_,_))),
      0x20 -> Seq(RegField(64, readData2(_), writeData2(_,_))),
      0x28 -> Seq(RegField(64, readID(_), ignoreWrite(_,_)))
    )
  } else {
    if (params.regSize == 32) {

      def readData1L(ready: Bool): (Bool, UInt) = {
        (true.B, core.io.data1Out(31, 0))
      }

      def readData1H(ready: Bool): (Bool, UInt) = {
        (true.B, core.io.data1Out(63, 32))
      }

      def readData2L(ready: Bool): (Bool, UInt) = {
        (true.B, core.io.data2Out(31, 0))
      }

      def readData2H(ready: Bool): (Bool, UInt) = {
        (true.B, core.io.data2Out(63, 32))
      }

      def readIDL(ready: Bool): (Bool, UInt) = {
        (true.B, SIMON_ID_1.U(32.W))
      }

      def readIDH(ready: Bool): (Bool, UInt) = {
        (true.B, SIMON_ID_2.U(32.W))
      }

      def writeKeyHL(valid: Bool, bits: UInt): Bool = {
        when (valid && core.io.kReady) {
          regKeyH := Cat(regKeyH(63, 32), bits)
        }
        true.B
      }

      def writeKeyHH(valid: Bool, bits: UInt): Bool = {
        when (valid && core.io.kReady) {
          regKeyH := Cat(bits, regKeyH(31, 0))
          kExpStart := true.B
        }
        true.B
      }

      def writeKeyLL(valid: Bool, bits: UInt): Bool = {
        when (valid && core.io.kReady) { regKeyL := Cat(regKeyL(63, 32), bits)  }
        true.B
      }

      def writeKeyLH(valid: Bool, bits: UInt): Bool = {
        when (valid && core.io.kReady) { regKeyL := Cat(bits, regKeyL(31, 0))  }
        true.B
      }

      def writeData1L(valid: Bool, bits: UInt): Bool = {
        when (valid && core.io.dInReady) {
          regData1 := Cat(regData1(63, 32), bits)
        }
        true.B
      }

      def writeData1H(valid: Bool, bits: UInt): Bool = {
        when (valid && core.io.dInReady) {
          regData1 := Cat(bits, regData1(31, 0))
          wroteData1 := true.B
        }
        true.B
      }

      def writeData2L(valid: Bool, bits: UInt): Bool = {
        when (valid && core.io.dInReady) {
          // update data register 1 value when re-starting
          regData2 := Cat(regData2(63, 32), bits)
        }
        true.B
      }

      def writeData2H(valid: Bool, bits: UInt): Bool = {
        when (valid && core.io.dInReady) {
          // update data register 1 value when re-starting
          when (!wroteData1) {
            regData1 := core.io.data1Out
          }.otherwise {
            wroteData1 := false.B
          }
          regData2 := Cat(bits, regData2(31, 0))
          dataValid := true.B
        }
        true.B
      }

      regmap(
        0x00 -> Seq(RegField(32, readSConf(_), writeSConf(_,_))),
        0x04 -> Seq(RegField(32, readKey(_), writeKeyLL(_,_))),
        0x08 -> Seq(RegField(32, readKey(_), writeKeyLH(_,_))),
        0x10 -> Seq(RegField(32, readKey(_), writeKeyHL(_,_))),
        0x14 -> Seq(RegField(32, readKey(_), writeKeyHH(_,_))),
        0x18 -> Seq(RegField(32, readData1L(_), writeData1L(_,_))),
        0x20 -> Seq(RegField(32, readData1H(_), writeData1H(_,_))),
        0x24 -> Seq(RegField(32, readData2L(_), writeData2L(_,_))),
        0x28 -> Seq(RegField(32, readData2H(_), writeData2H(_,_))),
        0x30 -> Seq(RegField(32, readIDL(_), ignoreWrite(_,_))),
        0x34 -> Seq(RegField(32, readIDH(_), ignoreWrite(_,_)))
      )
    }
  }
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
    SimonParams(address, pbus.beatBytes, 32))(p))

  pbus.toVariableWidthSlave(Some(portName)) { simon.node }
}

trait HasPeripherySimonTLModuleImp extends LazyModuleImp {
  implicit val p: Parameters
  val outer: HasPeripherySimonTL
}
