package SimonAcc

import chisel3._
import chisel3.util._

class SimonCore(registerWidth: Int) extends Module {
  val io = IO(
    new Bundle {
      val addr = Input(UInt(6.W))
      val iData = Input(UInt(registerWidth.W))
      val oData = Output(UInt(registerWidth.W))
      val rd = Input(Bool())
      val wr = Input(Bool())
    })

  private val REG_SCONF = 0x00
  private val REG_KEY1 = 0x08
  private val REG_KEY2 = 0x10
  private val REG_DATA1 = 0x18
  private val REG_DATA2 = 0x20
  private val SIMON_64_128_ROUNDS = 44
  private val SIMON_128_128_ROUNDS = 68

  // configuration and status register broken down into bits
  val sconfMode = RegInit(false.B)
  val sconfEncDec = RegInit(false.B)
  val sconfSingle = RegInit(false.B)

  // status portion
  val sconfReady = RegInit(false.B)
  val sconfBusy = Wire(Bool())

  // register is a wire
  val sconfReg = Wire(UInt(registerWidth.W))
  sconfReg := Cat(0.U((registerWidth-6).W), sconfBusy, sconfReady, 0.U(1.W), sconfSingle, sconfEncDec, sconfMode)

  // key registers
  val keyRegL = RegInit(0.U(registerWidth.W))
  val keyRegH = RegInit(0.U(registerWidth.W))

  val keyValue = Wire(UInt((2*registerWidth).W))
  keyValue := Cat(keyRegH, keyRegL)

  // data registers
  val dataReg1 = RegInit(0.U(registerWidth.W))
  val dataReg2 = RegInit(0.U(registerWidth.W))

  // key expander
  val kExp = Module(new SimonKeyExpander(SIMON_128_128_ROUNDS, 64, 128))

  // round computer
  val sRound = Module(new SimonRound(64))

  // internal states
  val kBusy = RegInit(false.B)
  val rBusy = RegInit(false.B)
  val ready = RegInit(false.B)
  val rStart = RegInit(false.B)
  val pendingRounds = RegInit(0.U(68.W))
  val roundCounter = RegInit(0.U(68.W))
  val roundKey = RegInit(0.U(64.W))
  val roundIValid = RegInit(false.B)
  val expKValid = RegInit(false.B)

  // busy flag logic
  sconfBusy := kBusy || rBusy || !kExp.io.kReady || !sRound.io.iReady

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

  // register write and round start logic
  when(!kBusy && io.wr) {
    expKValid := false.B
    switch (io.addr) {
      is (REG_SCONF.U) {
        sconfMode := io.iData(0)
        sconfEncDec := io.iData(1)
        sconfSingle := io.iData(2)
      }
      is (REG_KEY1.U) {
        keyRegL := io.iData
        ready := false.B
      }
      is (REG_KEY2.U) {
        keyRegH := io.iData
        ready := false.B
        expKValid := true.B
        kBusy := true.B
      }
      is (REG_DATA1.U) {
        dataReg1 := io.iData
        when (!sconfMode) {
          rStart := true.B
        }
      }
      is (REG_DATA2.U) {
        dataReg2 := io.iData
        when (sconfMode) {
          rStart := true.B
        }
      }
    }
  }

  // read logic
  when (io.rd) {
    io.oData := 0.U
    switch (io.addr) {
      is (REG_SCONF.U) {
        io.oData := sconfReg
      }
      is (REG_DATA1.U) {
        io.oData := dataReg1
      }
      is (REG_DATA2.U) {
        io.oData := dataReg2
      }
    }
  }

  // perform key expansion and round computations
  when (ready && rStart) {
    when (sRound.io.oValid) {
      when (pendingRounds === 0.U) {
        dataReg1 := sRound.io.block1Out
        dataReg2 := sRound.io.block2Out
        rBusy := false.B
        roundIValid := false.B
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
          roundKey := kExp.io.expanded(SIMON_64_128_ROUNDS.U - roundCounter)
        }.otherwise {
          roundKey := kExp.io.expanded(SIMON_128_128_ROUNDS.U - roundCounter)
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
  chisel3.Driver.execute(args, () => new SimonCore(64))
}
