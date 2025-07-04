package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.util._

class FloatPIO(esize: Int, fsize: Int) extends Bundle {
	val in = Input(Bits((1 + esize + fsize).W))
	val sign = Output(Bool())
	var exponent = Output(UInt(esize.W))
	var mantissa = Output(UInt(fsize.W))
	var notANo = Output(Bool())
	var negativeZero = Output(Bool())
	var positiveZero = Output(Bool())
	var hiddenBit = Output(UInt(1.W))
}


class Decode(esize: Int, fsize: Int) extends Module {
	val io = IO(new FloatPIO(esize, fsize))

	// from input bits, sign represent the first charachter
	io.sign := io.in(esize + fsize)

	// the next 8 bits represent the exponent
	io.exponent := io.in >> fsize

	// the rest represent the mantissa
	io.mantissa := io.in

	// set control signals: notANo, negativeInf, positiveInf, negativeZero, positiveZero, denormal, hiddenBit 
	io.hiddenBit := 1.U(1.W)

	// special numbers verification
	/*when(io.exponent === ((1.U << esize) - 1.U) && io.mantissa =/= 0.U(fsize.W)) {
		io.notANo := true.B
	} .elsewhen(io.exponent === ((1.U << esize) - 1.U) && io.mantissa === 0.U(fsize.W)) {
		io.notANo := true.B
	} .elsewhen(io.exponent === 0.U && io.mantissa =/= 0.U(fsize.W)) {
		io.denormal := true.B
		io.hiddenBit := 0.U(1.W)
	} .elsewhen (io.exponent === 0.U && io.mantissa === 0.U(fsize.W)) {
		when(io.sign === 1.U) {
			io.negativeZero := true.B
		} .otherwise {
			io.positiveZero := true.B
		}
	}*/
	
	io.notANo := Mux(io.exponent === ((1.U << esize.U) - 1.U), true.B, false.B)
	io.positiveZero := Mux((io.exponent === 0.U) && (io.mantissa === 0.U) && (io.sign === false.B), true.B, false.B)
	io.negativeZero := Mux((io.exponent === 0.U) && (io.mantissa === 0.U) && (io.sign === true.B), true.B, false.B)
}

class Encode(esize: Int, fsize: Int) extends Module {
	val io = IO(new Bundle {
		val sign = Input(Bool())
		val exponent = Input(UInt(esize.W))
		val mantissa = Input(UInt(fsize.W))
		val out = Output(Bits((1 + esize + fsize).W))
	})

	io.out := io.sign ## io.exponent ## io.mantissa
}

class Rounding(esize: Int, fsize: Int) extends Module {
	val io = IO(new Bundle {
		val noBeforeRounding = Input(UInt((1 + esize + fsize).W))
		val l = Input(Bool())
		val g = Input(Bool())
		val r = Input(Bool())
		val s = Input(Bool())
		val roundingMode = Input(UInt(3.W))
		var noAfterRounding = Output(UInt((1 + esize + fsize).W))
	})

	when(io.roundingMode === 0.U) {
		io.noAfterRounding := Mux(((io.g && (io.l || io.r || io.s)) === true.B), io.noBeforeRounding + 1.U, io.noBeforeRounding)
	} .elsewhen(io.roundingMode === 1.U) {
		io.noAfterRounding := io.noBeforeRounding
	} .elsewhen(io.roundingMode === 2.U) {
		io.noAfterRounding := Mux(((io.noBeforeRounding(esize + fsize) && (io.g || io.r || io.s)) === true.B), io.noBeforeRounding + 1.U, io.noBeforeRounding)
	} .elsewhen(io.roundingMode === 3.U) {
		io.noAfterRounding := Mux((((io.noBeforeRounding(esize + fsize) === false.B) && (io.g || io.r || io.s)) === true.B), io.noBeforeRounding + 1.U, io.noBeforeRounding)
	} .elsewhen(io.roundingMode === 4.U) {
		io.noAfterRounding := Mux((io.g || io.r || io.s) === true.B, io.noBeforeRounding + 1.U, io.noBeforeRounding)
	} .otherwise {
		io.noAfterRounding := io.noBeforeRounding
	}

}