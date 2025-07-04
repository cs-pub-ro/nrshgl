package ro.upb.nrs.hgl.performance

import chisel3._

class NaturalSub(size: Int) extends Module {
    val io = IO(new Bundle{
		val op1 = Input(UInt(size.W))
		val op2 = Input(UInt(size.W))
		val res = Output(UInt(size.W))
	})

	io.res := io.op1 - io.op2
}
