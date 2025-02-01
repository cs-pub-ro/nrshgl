package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.experimental.FixedPoint

class FixedPointQAdd(size: Int, fsize: Int) extends Module {
    val io = IO(new Bundle {
        val op1 = Input(FixedPoint(size.W, fsize.BP))
        val op2 = Input(FixedPoint(size.W, fsize.BP))
        val res = Output(FixedPoint(size.W, fsize.BP))
    })

    io.res := io.op1 + io.op2
}
