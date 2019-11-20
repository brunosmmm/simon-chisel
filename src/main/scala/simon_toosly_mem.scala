
package SimonAcc

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile._
import freechips.rocketchip.subsystem.{BaseSubsystem, CacheBlockBytes}
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket.{TLBPTWIO}
import freechips.rocketchip.tilelink._

class SimonTooslyMemModule(implicit p: Parameters) extends LazyModule {
  lazy val module = new SimonTooslyMemModuleImp(this)
  val node = TLClientNode(Seq(TLClientPortParameters(Seq(TLClientParameters("simon-toosly")))))
}

// simple memory controller
class SimonTooslyMemModuleImp(outer: SimonTooslyMemModule)(implicit p: Parameters) extends LazyModuleImp(outer)
    with HasCoreParameters {
  val io = IO(new Bundle
    {
      val wr = Input(Bool())
      val rd = Input(Bool())
      val rdValid = Output(Bool())
      val wrDone = Output(Bool())
      val ready = Output(Bool())
      val addr = Input(UInt(64.W))
      val dataIn = Input(UInt(64.W))
      val dataOut = Output(UInt(64.W))
      val ptw = new TLBPTWIO
    })

  val (mem, edge) = outer.node.out(0)
  val state_idle :: state_request_rd :: state_request_wr :: state_response_rd :: state_response_wr :: Nil = Enum(5)
  val state = RegInit(state_idle)

  io.ready := state === state_idle && mem.a.ready

  mem.a.valid := (state === state_request_rd) || (state === state_request_wr)
  mem.d.ready := (state === state_response_rd) || (state === state_response_wr)

  val rdDone = RegInit(false.B)
  io.rdValid := rdDone

  val wrDone = RegInit(false.B)
  io.wrDone := wrDone

  val readData = RegInit(0.U(64.W))
  io.dataOut := readData

  // interface a sends requests
  // interface d receives response
  switch (state) {
    is (state_idle) {
      when (io.wr && !io.rd && mem.a.ready) {
        wrDone := false.B
        state := state_request_wr
        mem.a.bits := edge.Put(
          fromSource = 0.U,
          toAddress = io.addr,
          lgSize = 3.U,
          data = io.dataIn
        )._2
      }
      when (io.rd && !io.wr && mem.a.ready) {
        rdDone := false.B
        state := state_request_rd
        mem.a.bits := edge.Get(
          fromSource = 0.U,
          toAddress = io.addr,
          lgSize = 3.U
        )._2
      }
    }
    is (state_request_rd) {
      state := state_response_rd
    }
    is (state_request_wr) {
      state := state_response_wr
    }
    is (state_response_rd) {
      when (mem.d.fire()) {
        rdDone := true.B
        readData := mem.d.bits.data
        state := state_idle
      }
    }
    is (state_response_wr) {
      when (mem.d.fire()) {
        state := state_idle
        wrDone := true.B
      }
    }
  }
}
