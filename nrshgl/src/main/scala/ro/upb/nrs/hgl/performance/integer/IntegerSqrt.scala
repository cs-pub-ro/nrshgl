package ro.upb.nrs.hgl.performance

import chisel3._

class ZSqrt(size: Int) extends Module {
    val io = IO(new Bundle {
        val op = Input(SInt((2 * size).W))
        val loadingOp = Input(Bool())
        val outputReady = Output(Bool())
        val q = Output(UInt(size.W))
        val r = Output(UInt((size + 1).W))
        val notANumber = Output(Bool())
    })

    /*when(io.op(size - 1)) {
        io.notANumber := true.B
        io.q := 0.U
        io.r := 0.U
        io.outputReady := true.B
    } .otherwise {
        val nSqrt = Module(new NaturalSqrt(size))
        nSqrt.io.op := io.op.asUInt
        nSqrt.io.loadingOp := io.loadingOp
        io.outputReady := nSqrt.io.outputValid
        io.q := nSqrt.io.q
        io.r := nSqrt.io.r
        io.notANumber := false.B
    }*/
} 
