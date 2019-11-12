package SimonAcc

import chisel3._
import chisel3.util._

class SimonKeyExpander(maxRounds: Int, maxWordWidth: Int, keyWidth: Int) extends Module {
  val io = IO(
    new Bundle {
      val mode = Input(Bool())
      val key = Input(UInt(keyWidth.W))
      val kReady = Output(Bool())
      val kValid = Input(Bool())
      val expValid = Output(Bool())
      val expanded = Output(Vec(maxRounds, UInt(maxWordWidth.W)))
    })

  // constants - don't know where to put them as common definitions
  val SIMON_64_128_ROUNDS = 44
  val SIMON_64_128_WORD_SIZE = 32
  val SIMON_64_128_KEY_WORDS = 4
  val SIMON_128_128_ROUNDS = 68
  val SIMON_128_128_WORD_SIZE = 64
  val SIMON_128_128_KEY_WORDS = 2
  val Z_64_128 = "b0011110000101100111001010001001000000111101001100011010111011011".U
  val Z_128_128 = "b0011001101101001111110001000010100011001001011000000111011110101".U

  // connections to rotate units
  val rr1_64in = RegInit(0.asUInt(SIMON_64_128_WORD_SIZE.W))
  val rr1_64out = Wire(UInt(SIMON_64_128_WORD_SIZE.W))
  val rr3_64in = RegInit(0.asUInt(SIMON_64_128_WORD_SIZE.W))
  val rr3_64out = Wire(UInt(SIMON_64_128_WORD_SIZE.W))

  val rr1_128in = RegInit(0.asUInt(SIMON_128_128_WORD_SIZE.W))
  val rr1_128out = Wire(UInt(SIMON_128_128_WORD_SIZE.W))
  val rr3_128in = RegInit(0.asUInt(SIMON_128_128_WORD_SIZE.W))
  val rr3_128out = Wire(UInt(SIMON_128_128_WORD_SIZE.W))

  val rr_1_64 = Module(new RotateUnit(SIMON_64_128_WORD_SIZE, 1, false))
  rr_1_64.io.i_value := rr1_64in
  rr1_64out := rr_1_64.io.o_value

  val rr_3_64 = Module(new RotateUnit(SIMON_64_128_WORD_SIZE, 3, false))
  rr_3_64.io.i_value := rr3_64in
  rr3_64out := rr_3_64.io.o_value

  val rr_1_128 = Module(new RotateUnit(SIMON_128_128_WORD_SIZE, 1, false))
  rr_1_128.io.i_value := rr1_128in
  rr1_128out := rr_1_128.io.o_value

  val rr_3_128 = Module(new RotateUnit(SIMON_128_128_WORD_SIZE, 3, false))
  rr_3_128.io.i_value := rr3_128in
  rr3_128out := rr_3_128.io.o_value

  // states
  val kexpIdle ::  kexpExp1 :: kexpExp2 :: kexpDone :: Nil = Enum(4)
  val kexpState = RegInit(kexpIdle)

  // pending round key expansion
  val kexpPending = Reg(UInt(16.W))

  // mode
  val kexpMode = Reg(Bool())

  // expanded round keys
  val xKey = RegInit(VecInit(Seq.fill(maxRounds)(0.U(maxWordWidth.W)))) // awful
  io.expanded := xKey

  // generate outputs
  io.expValid := (kexpState === kexpDone)
  io.kReady := (kexpState === kexpIdle)

  // asynchronously calculate values
  val xKey64Value = Wire(UInt(maxWordWidth.W))
  val xKey128Value = Wire(UInt(maxWordWidth.W))
  xKey64Value := Cat(0.U(32.W), ~xKey(SIMON_64_128_ROUNDS.U-kexpPending)(31, 0) ^ rr1_64out ^ rr1_64in ^
                       Cat(0.U(31.W), Z_64_128((SIMON_64_128_ROUNDS.U-kexpPending-SIMON_64_128_KEY_WORDS.U)%62.U)) ^ 3.U)
  xKey128Value := ~xKey(SIMON_128_128_ROUNDS.U-kexpPending)(31, 0) ^ rr1_128out ^ rr1_128in ^
                        Cat(0.U(31.W), Z_128_128((SIMON_128_128_ROUNDS.U-kexpPending-SIMON_128_128_KEY_WORDS.U)%62.U)) ^ 3.U

  switch(kexpState) {
    is (kexpIdle) {
      when (io.kValid) {
        kexpMode := io.mode
        kexpState := kexpExp1
        when (!io.mode) {
          xKey(0) := Cat(0.U(32.W), io.key(31, 0))
          xKey(1) := Cat(0.U(32.W), io.key(63, 32))
          xKey(2) := Cat(0.U(32.W), io.key(95, 64))
          xKey(3) := Cat(0.U(32.W), io.key(127, 96))
          kexpPending := SIMON_64_128_ROUNDS.U - SIMON_64_128_KEY_WORDS.U
          rr3_64in := io.key(127, 96)
        }.otherwise {
          xKey(0) := io.key(63, 0)
          xKey(1) := io.key(127, 64)
          kexpPending := SIMON_128_128_ROUNDS.U - SIMON_128_128_KEY_WORDS.U
          rr3_128in := io.key(127, 64)
        }
      }
    }
    is (kexpExp1) {
      when (kexpPending === 0.U) {
        kexpState := kexpDone
      }.otherwise {
        kexpState := kexpExp2
        when (!kexpMode) {
          rr1_64in := rr3_64out ^ xKey(SIMON_64_128_ROUNDS.U-kexpPending-3.U)(31, 0)
        }.otherwise {
          rr1_128in := rr3_128out ^ xKey(SIMON_128_128_ROUNDS.U-kexpPending-3.U)
        }
      }
    }
    is (kexpExp2) {
      kexpState := kexpExp1
      kexpPending := kexpPending - 1.U
      when (!kexpMode) {
        xKey(SIMON_64_128_ROUNDS.U-kexpPending) := xKey64Value
        rr3_64in := xKey64Value(31, 0)
      }.otherwise {
        xKey(SIMON_128_128_ROUNDS.U-kexpPending) := xKey128Value
        rr3_128in := xKey128Value
      }
    }
    is (kexpDone) {
      when (io.kValid) {
        kexpState := kexpIdle
      }
    }
  }
}

