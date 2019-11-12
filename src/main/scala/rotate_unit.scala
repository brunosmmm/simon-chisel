package SimonAcc

import chisel3._

class RotateUnit(width: Int, amount: Int, left: Boolean) extends Module {
  val io = IO(
    new Bundle {
      val i_value = Input(UInt(width.W))
      val o_value = Output(UInt(width.W))
    })

  when(left) {
    io.o_value := Cat(i_value(width-amount-1, 0), i_value(width-1, width-amount))
  }.otherwise {
    io.o_value := Cat(i_value(amount-1, 0), i_value(width-1, amount))
  }
}
