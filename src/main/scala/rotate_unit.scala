package SimonAcc

import chisel3._
import chisel3.util._

class RotateUnit(width: Int, amount: Int, left: Boolean) extends Module {
  val io = IO(
    new Bundle {
      val i_value = Input(UInt(width.W))
      val o_value = Output(UInt(width.W))
    })

  when(left.B) {
    io.o_value := Cat(io.i_value(width-amount-1, 0), io.i_value(width-1, width-amount))
  }.otherwise {
    io.o_value := Cat(io.i_value(amount-1, 0), io.i_value(width-1, amount))
  }
}
