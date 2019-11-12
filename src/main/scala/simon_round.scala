package SimonAcc

import chisel3._
import chisel3.util._

class SimonRound(maxWordWidth: Int) extends Module {
  val io = IO(
    new Bundle {
      val mode = Input(Bool())
      val encDec = Input(Bool())
      val block1In = Input(UInt(maxWordWidth.W))
      val block2In = Input(UInt(maxWordWidth.W))
      val roundKey = Input(UInt(maxWordWidth.W))
      val iValid = Input(Bool())
      val block1Out = Output(UInt(maxWordWidth.W))
      val block2Out = Output(UInt(maxWordWidth.W))
      val oValid = Output(Bool())
      val iReady = Output(Bool())
    })

  private val SIMON_64_128_WORD_SIZE = 32
  private val SIMON_128_128_WORD_SIZE = 64

  val rl1_64in = RegInit(0.U(SIMON_64_128_WORD_SIZE.W))
  val rl1_64out = Wire(UInt(SIMON_64_128_WORD_SIZE.W))
  val rl2_64in = RegInit(0.U(SIMON_64_128_WORD_SIZE.W))
  val rl2_64out = Wire(UInt(SIMON_64_128_WORD_SIZE.W))
  val rl8_64in = RegInit(0.U(SIMON_64_128_WORD_SIZE.W))
  val rl8_64out = Wire(UInt(SIMON_64_128_WORD_SIZE.W))

  val rl1_128in = RegInit(0.U(SIMON_128_128_WORD_SIZE.W))
  val rl1_128out = Wire(UInt(SIMON_128_128_WORD_SIZE.W))
  val rl2_128in = RegInit(0.U(SIMON_128_128_WORD_SIZE.W))
  val rl2_128out = Wire(UInt(SIMON_128_128_WORD_SIZE.W))
  val rl8_128in = RegInit(0.U(SIMON_128_128_WORD_SIZE.W))
  val rl8_128out = Wire(UInt(SIMON_128_128_WORD_SIZE.W))

  val rl_1_64 = Module(new RotateLeft(SIMON_64_128_WORD_SIZE, 1))
  val rl_2_64 = Module(new RotateLeft(SIMON_64_128_WORD_SIZE, 2))
  val rl_8_64 = Module(new RotateLeft(SIMON_64_128_WORD_SIZE, 8))

  rl_1_64.io.i_value := rl1_64in
  rl1_64out := rl_1_64.io.o_value

  rl_2_64.io.i_value := rl2_64in
  rl2_64out := rl_2_64.io.o_value

  rl_8_64.io.i_value := rl8_64in
  rl8_64out := rl_8_64.io.o_value

  val rl_1_128 = Module(new RotateLeft(SIMON_128_128_WORD_SIZE, 1))
  val rl_2_128 = Module(new RotateLeft(SIMON_128_128_WORD_SIZE, 2))
  val rl_8_128 = Module(new RotateLeft(SIMON_128_128_WORD_SIZE, 8))

  rl_1_128.io.i_value := rl1_128in
  rl1_128out := rl_1_128.io.o_value

  rl_2_128.io.i_value := rl2_128in
  rl2_128out := rl_2_128.io.o_value

  rl_8_128.io.i_value := rl8_128in
  rl8_128out := rl_8_128.io.o_value

  val out = Wire(UInt(maxWordWidth.W))
  val xIn = Wire(UInt(maxWordWidth.W))
  val yIn = Wire(UInt(maxWordWidth.W))
  val tmp = RegInit(0.U(maxWordWidth.W))
  val busy = RegInit(false.B)
  val encDecMode = RegInit(true.B)
  val simonMode = RegInit(false.B)

  val output1 = RegInit(0.U(maxWordWidth.W))
  val output2 = RegInit(0.U(maxWordWidth.W))
  val isReady = RegInit(true.B)
  val isValid = RegInit(false.B)
  io.block1Out := output1
  io.block2Out := output2
  io.iReady := isReady
  io.oValid := isValid

  when (io.encDec) {
    xIn := io.block1In
    yIn := io.block2In
  }.otherwise {
    xIn := io.block2In
    yIn := io.block1In
  }

  when (simonMode) {
    out := Cat(0.U(32.W), (rl1_64out & rl8_64out) ^ rl2_64out) ^ tmp
  }.otherwise {
    out := (rl1_128out & rl8_128out) ^ rl2_128out ^ tmp
  }

  // actual round process
  when (io.iValid && !busy) {
    isReady := false.B
    busy := true.B
    encDecMode := io.encDec
    simonMode := io.mode
    tmp := io.roundKey ^ yIn

    when(!io.mode) {
      rl1_64in := xIn(31, 0)
      rl2_64in := xIn(31, 0)
      rl8_64in := xIn(31, 0)
    }.otherwise {
      rl1_128in := xIn
      rl2_128in := xIn
      rl8_128in := xIn
    }

    when (io.encDec) {
      output2 := io.block1In
    }.otherwise {
      output1 := io.block2In
    }
  }.otherwise {
    when (busy) {
      isValid := true.B
      isReady := true.B
      busy := false.B
      when (encDecMode) {
        output1 := out
      }.otherwise {
        output2 := out
      }
    }.otherwise {
      isValid := false.B
    }
  }

}

object SimonRoundDriver extends App {
  chisel3.Driver.execute(args, () => new SimonRound(64))
}
