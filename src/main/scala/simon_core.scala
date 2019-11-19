package SimonAcc

import chisel3._
import chisel3.util._

class SimonCore(registerWidth: Int, keyWidth: Int) extends Module {
  val io = IO(
    new Bundle {
      val keyL = Input(UInt(registerWidth.W))
      val keyH = Input(UInt(registerWidth.W))
      val kValid = Input(Bool())
      val kExpDone = Output(Bool())
      val kReady = Output(Bool())
      val sMode = Input(Bool())
      val data1In = Input(UInt(registerWidth.W))
      val data2In = Input(UInt(registerWidth.W))
      val data1Out = Output(UInt(registerWidth.W))
      val data2Out = Output(UInt(registerWidth.W))
      val dInReady = Output(Bool())
      val dInValid = Input(Bool())
      val dOutValid = Output(Bool())
      val dEncDec = Input(Bool())
      val rSingle = Input(Bool())
    })

  private val SIMON_64_128_ROUNDS = 44
  private val SIMON_128_128_ROUNDS = 68

  // configuration and status register broken down into bits
  val sconfMode = RegInit(false.B)
  val sconfEncDec = RegInit(false.B)
  val sconfSingle = RegInit(false.B)

  // status portion
  val sconfReady = Wire(Bool())
  val sconfBusy = Wire(Bool())

  // output flags
  io.dInReady := sconfReady

  // key registers
  val keyRegL = RegInit(0.U(registerWidth.W))
  val keyRegH = RegInit(0.U(registerWidth.W))

  val keyValue = Wire(UInt((2*registerWidth).W))
  keyValue := Cat(keyRegH, keyRegL)

  // data registers
  val dataReg1 = RegInit(0.U(registerWidth.W))
  val dataReg2 = RegInit(0.U(registerWidth.W))

  // key expander
  val kExp = Module(new SimonKeyExpander(SIMON_128_128_ROUNDS, registerWidth, keyWidth))

  // round computer
  val sRound = Module(new SimonRound(64))

  // internal states
  val kExpDone = RegInit(false.B)
  val rBusy = RegInit(false.B)
  val rStart = RegInit(false.B)
  val pendingRounds = RegInit(0.U(68.W))
  val roundCounter = RegInit(0.U(68.W))
  val roundKey = RegInit(0.U(64.W))
  val roundIValid = RegInit(false.B)
  val expKValid = RegInit(false.B)
  io.kExpDone := kExpDone
  io.dOutValid := ~rBusy && sRound.io.oValid
  sconfReady := ~sconfBusy && kExpDone
  io.kReady := kExp.io.kReady && ~sconfBusy

  when (rBusy) {
    io.data1Out := 0.U
    io.data2Out := 0.U
  }.otherwise {
    io.data1Out := dataReg1
    io.data2Out := dataReg2
  }

  // busy flag logic
  sconfBusy := expKValid || rBusy || !kExp.io.kReady || !sRound.io.iReady

  // connect other signals
  kExp.io.mode := sconfMode
  kExp.io.key := keyValue
  kExp.io.kValid := expKValid
  sRound.io.mode := sconfMode
  sRound.io.encDec := sconfEncDec
  sRound.io.block1In := dataReg1
  sRound.io.block2In := dataReg2
  sRound.io.roundKey := roundKey
  sRound.io.iValid := roundIValid

  // trigger key expansion
  when(!expKValid && io.kValid && kExp.io.kReady) {
    expKValid := true.B
    kExpDone := false.B
    keyRegH := io.keyH
    keyRegL := io.keyL
    sconfMode := io.sMode
  }.otherwise {
    when (expKValid && kExp.io.expValid) {
      // key expansion finished,
      // deassert key expansion signal
      expKValid := false.B
      kExpDone := true.B
    }
  }

  // start round
  when (!expKValid && !rBusy && !rStart && io.dInValid) {
    when(sconfEncDec^io.dEncDec) {
      // reset round counter
      roundCounter := 0.U
    }
    sconfEncDec := io.dEncDec
    sconfSingle := io.rSingle
    dataReg1 := io.data1In
    dataReg2 := io.data2In
    rStart := true.B
  }

  // perform round computations
  when (rStart) {
    when (sRound.io.oValid) {
      dataReg1 := sRound.io.block1Out
      dataReg2 := sRound.io.block2Out
      when (pendingRounds === 0.U) {
        rBusy := false.B
        roundIValid := false.B
        rStart := false.B
        when (!sconfMode) {
          when (roundCounter === (SIMON_64_128_ROUNDS - 1).U) {
            roundCounter := 0.U
          }.otherwise {
            roundCounter := roundCounter + 1.U
          }
        }.otherwise {
          when (roundCounter === (SIMON_128_128_ROUNDS - 1).U) {
            roundCounter := 0.U
          }.otherwise {
            roundCounter := roundCounter + 1.U
          }
        }
      }.otherwise {
        pendingRounds := pendingRounds - 1.U
        roundIValid := true.B
      }
    }.otherwise {
      roundIValid := true.B
      rBusy := true.B
      when (sconfEncDec) {
        roundKey := kExp.io.expanded(roundCounter)
      }.otherwise {
        when (!sconfMode) {
          roundKey := kExp.io.expanded(SIMON_64_128_ROUNDS.U - roundCounter - 1.U)
        }.otherwise {
          roundKey := kExp.io.expanded(SIMON_128_128_ROUNDS.U - roundCounter - 1.U)
        }
      }
      when (sconfSingle) {
        pendingRounds := 0.U
      }.otherwise {
        when (!sconfMode) {
          pendingRounds := SIMON_64_128_ROUNDS.U
        }.otherwise {
          pendingRounds := SIMON_128_128_ROUNDS.U
        }
      }
    }
  }
}

object SimonCoreDriver extends App {
  chisel3.Driver.execute(args, () => new SimonCore(64, 128))
}
