package SimonAcc

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.subsystem.{BaseSubsystem, CacheBlockBytes}
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
// TODO fix imports

class SimonToosly(opcodes: OpcodeSet)
    (implicit p: Parameters) extends LazyRoCC(opcodes=opcodes, nPTWPorts=1) {
  override lazy val module = new SimonTooslyModule(this)
  val memCtl = LazyModule(new SimonTooslyMemModule)
  tlNode := memCtl.node
}

class SimonTooslyModule(outer: SimonToosly)
    extends LazyRoCCModuleImp(outer) {
  // simon core
  val core = Module(new SimonCore(64, 128))
  // memory controller
  val memCtl = outer.memCtl.module

  val memWr = RegInit(false.B)
  val memRd = RegInit(false.B)

  // custom values
  // FIXME how to put common values in package in scala?
  private val FUNC_INIT = 0
  private val FUNC_ENC = 1
  private val FUNC_DEC = 2
  private val FUNC_CLOAD = 3
  private val FUNC_CSTOR = 4

  private val SIMON_FUNCT_MODE_OFFSET = 0
  private val SIMON_FUNCT_OP_OFFSET = 2
  private val SIMON_FUNCT_MODE_MASK = 0x3
  private val SIMON_FUNCT_OP_MASK = 0x1C
  private val SIMON_64_128_ROUNDS = 44
  private val SIMON_128_128_ROUNDS = 68

  val operation = (io.cmd.bits.inst.funct & SIMON_FUNCT_OP_MASK.U) >> SIMON_FUNCT_OP_OFFSET
  val mode = io.cmd.bits.inst.funct & SIMON_FUNCT_MODE_MASK.U
  val kBusy = RegInit(false.B)
  val rBusy = RegInit(false.B)
  val mBusy = Wire(Bool())
  val respData = RegInit(0.U(64.W))
  mBusy := !memCtl.io.ready

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
  stallResponse := io.cmd.bits.inst.xd && !io.resp.ready

  // handle encryption/decryption over memory region
  val accBusy = RegInit(false.B)
  val storePending = RegInit(false.B)
  val loadPending = RegInit(false.B)
  val storeAddr = RegInit(0.U(64.W))
  val loadAddr = RegInit(0.U(64.W))
  val storeWord = RegInit(0.U(64.W))
  val pendingWordCount = RegInit(0.U(64.W))
  val startAddr = RegInit(0.U(64.W))
  val startEncDec = RegInit(false.B)
  val memRdAck = RegInit(false.B)
  val coreOutAck = RegInit(false.B)
  val dontStore = RegInit(false.B)

  memCtl.io.addr := Mux(memWr, storeAddr, loadAddr)
  memCtl.io.wr := memWr
  memCtl.io.rd := memRd
  memCtl.io.dataIn := storeWord

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
  io.busy := kBusy || rBusy || mBusy || startEncDec
  io.cmd.ready := !kBusy && !rBusy && !startEncDec && !mBusy && !stallResponse

  // receive instructions
  when (io.cmd.fire()) {
    wantsResponse := io.cmd.bits.inst.xd
    responseDest := io.cmd.bits.inst.rd
    dontStore := false.B
    switch (operation) {
      is (FUNC_INIT.U) {
        // initialize & perform key expansion
        coreKeyH := io.cmd.bits.rs2
        coreKeyL := io.cmd.bits.rs1
        coreKeyValid := true.B
        kBusy := true.B
        respData := 0.U
      }
      is (FUNC_ENC.U) {
        // encrypt a memory region
        coreEncDec := true.B
        startAddr := io.cmd.bits.rs1
        when (io.cmd.bits.rs2 === 0.U) {
          // exception?
        }.otherwise {
          pendingWordCount := io.cmd.bits.rs2 - 1.U
          startEncDec := true.B
          loadPending := true.B
        }
        loadAddr := io.cmd.bits.rs1
      }
      is (FUNC_DEC.U) {
        // decrypt a memory region
        coreEncDec := false.B
        startAddr := io.cmd.bits.rs1
        when (io.cmd.bits.rs2 === 0.U) {
          // exception?
        }.otherwise {
          pendingWordCount := io.cmd.bits.rs2 - 1.U
          startEncDec := true.B
          loadPending := true.B
        }
        loadAddr := io.cmd.bits.rs1
      }
      is (FUNC_CLOAD.U) {
        // load and decrypt from memory
        coreEncDec := false.B
        startAddr := io.cmd.bits.rs1
        pendingWordCount := 0.U
        startEncDec := true.B
        loadPending := true.B
        loadAddr := io.cmd.bits.rs1
        // prevent store mechanism from starting up automatically
        dontStore := true.B
      }
      is (FUNC_CSTOR.U) {
        // encrypt & store to memory
        coreEncDec := true.B
        startAddr := io.cmd.bits.rs1
        pendingWordCount := 0.U
        coreData1 := io.cmd.bits.rs2(31, 0)
        coreData2 := io.cmd.bits.rs2(63, 32)
        startEncDec := true.B
        loadAddr := io.cmd.bits.rs1
        // start immediately
        coreDataValid := true.B
        rBusy := true.B
      }
    }
  }

  // automatically deassert start flag
  when (startEncDec) {
    when (pendingWordCount === 0.U && !loadPending && !storePending && !rBusy) {
      startEncDec := false.B
    }
  }

  // detect key expansion end and flag as ready
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

  // detect encryption/decryption rounds end and perform store to memory if necessary
  when (rBusy) {
    when (core.io.dOutValid && !coreOutAck) {
      rBusy := false.B
      wantsResponse := false.B
      when (wantsResponse && io.resp.ready) {
        responseValid := true.B
      }.otherwise {
        when (wantsResponse) {
          responsePending := true.B
        }
      }
      // store to memory pending
      when (!dontStore) {
        storePending := true.B
        // store to same address
        storeAddr := loadAddr
        storeWord := Cat(core.io.data2Out(31, 0), core.io.data1Out(31, 0))
        respData := 0.U
      }.otherwise {
        respData := Cat(core.io.data2Out(31,0), core.io.data1Out(31, 0))
      }
    }
  }

  // perform actual store
  when (storePending) {
    when (memCtl.io.ready) {
      storePending := false.B
      memWr := true.B

      // prepare next load automatically if needed
      when (pendingWordCount > 0.U) {
        loadPending := true.B
        loadAddr := loadAddr + 1.U
        pendingWordCount := pendingWordCount - 1.U
      }
    }
  }

  // deassert memory controller signals
  when (memWr) {
    memWr := false.B
  }

  when (memRd) {
    memRd := false.B
  }

  // manage double handshake flags for memory controller
  when (memCtl.io.rdValid) {
    memRdAck := true.B
  }.otherwise {
    memRdAck := false.B
  }

  when (core.io.dOutValid) {
    coreOutAck := true.B
  }.otherwise {
    coreOutAck := false.B
  }

  // manage memory load mechanism
  when (loadPending && !storePending && !memWr) {
    when (memCtl.io.ready) {
      memRd := true.B
      when (memCtl.io.rdValid && !memRdAck) {
        // read done
        memRd := false.B
        coreData1 := memCtl.io.dataOut(31, 0)
        coreData2 := memCtl.io.dataOut(63, 32)
        // perform next rounds
        rBusy := true.B
        coreDataValid := true.B
        loadPending := false.B
      }
    }
  }
}
