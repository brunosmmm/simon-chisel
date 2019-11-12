package SimonAcc

import chisel3._
import chisel3.util._

class RotateRight(width: Int, amount: Int) extends Module {
  val io = IO(
    new Bundle {
      val i_value = Input(UInt(width.W))
      val o_value = Output(UInt(width.W))
    })

    io.o_value := Cat(io.i_value(amount-1, 0), io.i_value(width-1, amount))
}

class RotateLeft(width: Int, amount: Int) extends Module {
  val io = IO(
    new Bundle {
      val i_value = Input(UInt(width.W))
      val o_value = Output(UInt(width.W))
    })

    io.o_value := Cat(io.i_value(width-amount-1, 0), io.i_value(width-1, width-amount))
}
