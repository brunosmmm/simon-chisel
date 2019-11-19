package SimonAcc

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.subsystem.{BaseSubsystem, CacheBlockBytes}
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, IdRange}
import testchipip.TLHelper

class SimonToosly(opcodes: OpcodeSet)
    (implicit p: Parameters) extends LazyRoCC(opcodes) {
  override lazy val module = new SimonTooslyModule(this)
  val memCtl = LazyModule(new SimonTooslyMemModule)
  tlNode := memCtl.node
}

class SimonTooslyMemModule(implicit p: Parameters) extends LazyModule {
  lazy val module = new SimonTooslyMemModuleImp(this)
  val node = TLHelper.makeClientNode(
    name = "simon-toosly", sourceId = IdRange(0, 1))
}

class SimonTooslyMemModuleImp(outer: SimonTooslyMemModule)(implicit p: Parameters) extends LazyModuleImp(outer)
    with HasCoreParameters {

  val (mem, edge) = outer.node.out(0)
}

class SimonTooslyModule(outer: SimonToosly)
    extends LazyRoCCModuleImp(outer) {
  // The parts of the command are as follows
  // inst - the parts of the instruction itself
  //   opcode
  //   rd - destination register number
  //   rs1 - first source register number
  //   rs2 - second source register number
  //   funct
  //   xd - is the destination register being used?
  //   xs1 - is the first source register being used?
  //   xs2 - is the second source register being used?
  // rs1 - the value of source register 1
  // rs2 - the value of source register 2
  val core = Module(new SimonCore(64, 128))

  // memory controller
  val memCtl = outer.memCtl.module

  // custom functions
  private val FUNC_INIT = 0
  private val FUNC_ENC = 1
  private val FUNC_DEC = 2

  private val SIMON_FUNCT_MODE_OFFSET = 0
  private val SIMON_FUNCT_OP_OFFSET = 2
  private val SIMON_FUNCT_MODE_MASK = 0x3
  private val SIMON_FUNCT_OP_MASK = 0xC
  private val SIMON_64_128_ROUNDS = 44
  private val SIMON_128_128_ROUNDS = 68

  val operation = (io.cmd.bits.inst.funct & SIMON_FUNCT_OP_MASK.U) >> SIMON_FUNCT_OP_OFFSET
  val mode = io.cmd.bits.inst.funct & SIMON_FUNCT_MODE_MASK.U
  val kBusy = RegInit(false.B)
  val rBusy = RegInit(false.B)
  val respData = RegInit(0.U(64.W))

  val coreKeyH = RegInit(0.U(64.W))
  val coreKeyL = RegInit(0.U(64.W))
  val coreKeyValid = RegInit(false.B)
  val coreEncDec = RegInit(true.B)
  val coreDataValid = RegInit(false.B)
  val coreData1 = RegInit(0.U(64.W))
  val coreData2 = RegInit(0.U(64.W))

  // support only 64/128 mode
  core.io.sMode := false.B
  core.io.keyH := coreKeyH
  core.io.keyL := coreKeyL
  core.io.dEncDec := coreEncDec
  core.io.data1In := coreData1
  core.io.data2In := coreData2
  core.io.dInValid := coreDataValid
  // all rounds performed by controller
  core.io.rSingle := false.B
  core.io.kValid := coreKeyValid

  val responseValid = RegInit(false.B)
  val responsePending = RegInit(false.B)
  val stallResponse = Wire(Bool())
  val wantsResponse = RegInit(false.B)
  val responseDest = RegInit(0.U(5.W))
  val lastOperation = RegInit(0.U(2.W))
  stallResponse := io.cmd.bits.inst.xd && !io.resp.ready

  // auto clear
  when (coreKeyValid) {
    coreKeyValid := false.B
  }

  when (coreDataValid) {
    coreDataValid := false.B
  }

  when (responseValid) {
    responseValid := false.B
  }

  when (responsePending) {
    when (wantsResponse && io.resp.ready) {
      responseValid := true.B
      responsePending := false.B
    }
  }

  // other
  io.interrupt := false.B
  io.resp.bits.rd := responseDest
  io.resp.bits.data := respData
  io.resp.valid := responseValid
  io.busy := kBusy || rBusy
  io.cmd.ready := !kBusy && !rBusy && !stallResponse

  // receive instructions
  when (io.cmd.fire()) {
    wantsResponse := io.cmd.bits.inst.xd
    responseDest := io.cmd.bits.inst.rd
    lastOperation := operation
    switch (operation) {
      is (FUNC_INIT.U) {
        // initialize
        coreKeyH := io.cmd.bits.rs2
        coreKeyL := io.cmd.bits.rs1
        coreKeyValid := true.B
        kBusy := true.B
        respData := 0.U
      }
      is (FUNC_ENC.U) {
        coreEncDec := true.B
        coreData1 := io.cmd.bits.rs1(31,0)
        coreData2 := io.cmd.bits.rs1(63, 32)
        coreDataValid := true.B
        rBusy := true.B
      }
      is (FUNC_DEC.U) {
        coreEncDec := false.B
        coreData1 := io.cmd.bits.rs1(31,0)
        coreData2 := io.cmd.bits.rs1(63, 32)
        coreDataValid := true.B
        rBusy := true.B
      }
    }
  }

  when (kBusy) {
    when (core.io.kExpDone) {
      kBusy := false.B
      wantsResponse := false.B
      when (wantsResponse && io.resp.ready) {
        responseValid := true.B
      }.otherwise {
        when (wantsResponse) {
          responsePending := true.B
        }
      }
    }
  }

  when (rBusy) {
    when (core.io.dOutValid) {
      rBusy := false.B
      wantsResponse := false.B
      when (wantsResponse && io.resp.ready) {
        responseValid := true.B
      }.otherwise {
        when (wantsResponse) {
          responsePending := true.B
        }
      }
      // respData := Cat(core.io.data2Out(31, 0), core.io.data1Out(31, 0))
      respData := 0.U
    }
  }
}