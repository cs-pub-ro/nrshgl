package ro.upb.nrs.hgl.performance

import chisel3._

class ZAdd(size: Int) extends Module{
    val io = IO(new Bundle {
        val op1 = Input(SInt(size.W))
        val op2 = Input(SInt(size.W))
        val res = Output(SInt(size.W))
    })

    io.res := io.op1 + io.op2
}
