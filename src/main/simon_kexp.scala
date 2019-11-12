import chisel3._
import chisel3.util._

class SimonKeyExpander(maxRounds: Int, maxWordWidth: Int, keyWidth: Int) extends Module {
  val io = IO(
    new Bundle {
      val mode = Input(Bool())
      val key = Input(keyWidth.W)
      val kReady = Output(Bool())
      val kValid = Input(Bool())
      val expValid = Output(Bool())
      val expanded = Output(Vec(maxRounds, UInt(maxWordWidth.W)))
    })

  // constants - don't know where to put them as common definitions
  val SIMON_64_128_ROUNDS = 44.U
  val SIMON_64_128_WORD_SIZE = 32.U
  val SIMON_64_128_KEY_WORDS = 4.U
  val SIMON_128_128_ROUNDS = 68.U
  val SIMON_128_128_WORD_SIZE = 64.U
  val SIMON_128_128_KEY_WORDS = 2.U
  val Z_64_128 = "b0011110000101100111001010001001000000111101001100011010111011011".U
  val Z_128_128 = "b0011001101101001111110001000010100011001001011000000111011110101".U

  // connections to rotate units
  val rr1_64in = Reg(UInt(SIMON_64_128_WORD_SIZE.W))
  val rr1_64out = Wire(UInt(SIMON_64_128_WORD_SIZE.W))
  val rr3_64in = Reg(UInt(SIMON_64_128_WORD_SIZE.W))
  val rr3_64out = Wire(UInt(SIMON_64_128_WORD_SIZE.W))

  val rr1_128in = Reg(UInt(SIMON_128_128_WORD_SIZE.W))
  val rr1_128out = Wire(UInt(SIMON_128_128_WORD_SIZE.W))
  val rr3_128in = Reg(UInt(SIMON_128_128_WORD_SIZE.W))
  val rr3_128out = Wire(UInt(SIMON_128_128_WORD_SIZE.W))

  val rr_1_64 = Module(new RotateUnit(SIMON_64_128_WORD_SIZE, 1, false.B))
  rr_1_64.io.i_value := rr1_64in
  rr1_64out := rr_1_64.io.o_value

  val rr_3_64 = Module(new RotateUnit(SIMON_64_128_WORD_SIZE, 3, false.B))
  rr_3_64.io.i_value := rr3_64in
  rr3_64out := rr_3_64.io.o_value

  val rr_1_128 = Module(new RotateUnit(SIMON_128_128_WORD_SIZE, 1, false.B))
  rr_1_128.io.i_value := rr1_128in
  rr1_128out := rr_1_128.io.o_value

  val rr_3_128 = Module(new RotateUnit(SIMON_128_128_WORD_SIZE, 3, false.B))
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
  val xKey = RegInit(Vec(maxRounds, 0.U(maxWordWidth.W)))
  io.expanded := xKey

  // generate outputs
  io.expValid := (kexpState === kexpDone)
  io.kReady := (kexpState == kexpIdle)

  // asynchronously calculate values
  val xKey64Value = Wire(Uint(maxWordWidth.W))
  val xKey128Value = Wire(Uint(maxWordWidth.W))
  xKey64Value := Cat(0.U(32.W), ~xKey(SIMON_64_128_ROUNDS-kexpPending)(31, 0) ^ rr1_64out ^ rr1_64in ^
                       Cat(0.U(31.W), Z_64_128((SIMON_64_128_ROUNDS-kexpPending-SIMON_64_128_KEY_WORDS)%62)) ^ 3)
  xKey128Value := Cat(0.U(32.W), ~xKey(SIMON_128_128_ROUNDS-kexpPending)(31, 0) ^ rr1_128out ^ rr1_128in ^
                        Cat(0.U(31.W), Z_128_128((SIMON_128_128_ROUNDS-kexpPending-SIMON_128_128_KEY_WORDS)%62)) ^ 3)

  switch(state) {
    is (kexpIdle) {
      when (kValid) {
        kexpMode := mode
        kexpState := kexpExp1
        when (!mode) {
          xKey(0) := Cat(0.U(32.W), key(31, 0))
          xKey(1) := Cat(0.U(32.W), key(63, 32))
          xKey(2) := Cat(0.U(32.W), key(95, 64))
          xKey(3) := Cat(0.U(32.W), key(127, 96))
          kexpPending := SIMON_64_128_ROUNDS - SIMON_64_128_KEY_WORDS
          rr3_64in := key(127, 96)
        }.otherwise {
          xKey(0) := key(63, 0)
          xKey(1) := key(127, 64)
          kexpPending := SIMON_128_128_ROUNDS - SIMON_128_128_KEY_WORDS
          rr3_128in := key(127, 64)
        }
      }
    }
    is (kexpExp1) {
      when (kexpPending === 0) {
        kexpState := kexpDone
      }.otherwise {
        kexpState := kexpExp2
        when (!kexpMode) {
          rr1_64in := rr3_64out ^ xKey(SIMON_64_128_ROUNDS-kexpPending-3)(31, 0)
        }.otherwise {
          rr1_128in := rr3_128out ^ xKey(SIMON_128_128_ROUNDS-kexpPending-3)
        }
      }
    }
    is (kexpExp2) {
      kexpState := kexpExp1
      kexpPending := kexpPending - 1
      when (!kexpMode) {
        xKey(SIMON_64_128_ROUNDS-kexpPending) := xKey64Value
        rr3_64in := xKey64Value(31, 0)
      }.otherwise {
        xKey(SIMON_128_128_ROUNDS-kexpPending) := xKey128Value
        rr3_128in := xKey128Value
      }
    }
    is (kexpDone) {
      when (kValid) {
        kexpState := kexpIdle
      }
    }
  }
}
