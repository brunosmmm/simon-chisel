
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
  val regWSconf = Reg(Uint(64.W))
  val regRSconf = Wire(UInt(64.W))
  regRSconf := Cat(0.U((64-6).W), )

  val core = Module(new SimonCore(64))
  core.io.keyH := regKeyH
  core.io.keyL := regKeyL
  core.io.data1In := regData1
  core.io.data2In := regData2
  core.io.sMode := regWSconf(0)
  core.io.dEncDec := regWSconf(1)
  core.io.rSingle := regWSconf(2)
}
