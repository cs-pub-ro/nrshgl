package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.util._

class FloatPMulIO(esize: Int, fsize: Int) extends Bundle {
	val op1 = Input(Bits((1 + esize + fsize).W))
	val op2 = Input(Bits((1 + esize + fsize).W))
	val sign = Output(Bool())
	val normalizedExponent = Output(UInt(esize.W))
	val normalizedMantissa = Output(UInt(fsize.W))
	val res = Output(Bits((1 + esize + fsize).W))
}


class FloatingPointMultiply(esize: Int, fsize: Int) extends Module {
	val io = IO(new FloatPMulIO(esize, fsize))

	// instantiate the module to decode the operands
	val decoder1 = Module(new Decode(esize, fsize))
	val decoder2 = Module(new Decode(esize, fsize))
	val encoder = Module(new Encode(esize, fsize))

	// instantiate the chosen representations for FP
	val decodedOp1 = Wire(new FloatPIO(esize, fsize))
	val decodedOp2 = Wire(new FloatPIO(esize, fsize))

	val bias = (1.U << (esize - 1)) - 1.U

	// decode the raw bits that represents the operands
	decoder1.io.in := io.op1
	decoder2.io.in := io.op2

	decodedOp1 := decoder1.io
	decodedOp2 := decoder2.io
	
	// https://en.wikipedia.org/wiki/Signed_zero - the sign rules for division and multiplication are kept even if an operand is positive zero or negative zero
	when(decodedOp1.positiveZero || decodedOp1.negativeZero) {
		io.sign := decodedOp1.sign ^ decodedOp2.sign
		io.normalizedExponent := decodedOp1.exponent
		io.normalizedMantissa := decodedOp1.mantissa
		encoder.io.sign := io.sign
		encoder.io.exponent := io.normalizedExponent
		encoder.io.mantissa := io.normalizedMantissa
		io.res := encoder.io.out
	} .elsewhen(decodedOp2.positiveZero || decodedOp2.positiveZero) {
		io.sign := decodedOp1.sign ^ decodedOp2.sign
		io.normalizedExponent := decodedOp2.exponent
		io.normalizedMantissa := decodedOp2.mantissa
		encoder.io.sign := io.sign
		encoder.io.exponent := io.normalizedExponent
		encoder.io.mantissa := io.normalizedMantissa
		io.res := encoder.io.out
	} .elsewhen(decodedOp1.notANo || decodedOp2.notANo) {
		io.sign := decodedOp1.sign ^ decodedOp2.sign
		io.normalizedExponent := (1.U << esize) - 1.U
		io.normalizedMantissa := 0.U
		encoder.io.sign := io.sign
		encoder.io.exponent := io.normalizedExponent
		encoder.io.mantissa := io.normalizedMantissa
		io.res := encoder.io.out
	} .otherwise {
		// compute the sign of the result
		io.sign := decodedOp1.sign ^ decodedOp2.sign

		// res_exp = op1_exp + op2_exp - bias
		val exponent = Wire(UInt(esize.W))
		exponent := decodedOp1.exponent + decodedOp2.exponent - bias.asUInt

		// res_mantissa = op1_mantissa * op2_mantissa
		val mantissa1 = Wire(UInt((fsize + 1).W))
		val mantissa2 = Wire(UInt((fsize + 1).W))
		val mantissa3 = Wire(UInt((2 * (fsize + 1)).W))

		mantissa1 := decodedOp1.hiddenBit ## decodedOp1.mantissa
		mantissa2 := decodedOp2.hiddenBit ## decodedOp2.mantissa
		mantissa3 := mantissa1 * mantissa2

		val is1 = Wire(Bool())
		is1 := (mantissa3(2 * fsize + 1) === true.B)

		io.normalizedMantissa := Mux(is1, mantissa3(2 * fsize + 1, fsize + 1), mantissa3(2 * fsize + 1, fsize))
		io.normalizedExponent := Mux(is1, exponent + 1.U, exponent)

		val restBits = Wire(UInt((fsize + 1).W))
		restBits := Mux(is1, mantissa3(fsize, 0), mantissa3(fsize - 1, 0) ## false.B)

		val g = restBits(fsize)
		val r = restBits(fsize - 1)
		val s = (restBits(fsize - 2, 0)).orR
		val l = io.normalizedMantissa(0)

		encoder.io.sign := io.sign
		encoder.io.exponent := io.normalizedExponent
		encoder.io.mantissa := io.normalizedMantissa

		val rounder = Module(new Rounding(esize, fsize))
		rounder.io.g := g
		rounder.io.l := l
		rounder.io.r := r
		rounder.io.s := s
		rounder.io.noBeforeRounding := encoder.io.out
		rounder.io.roundingMode := 0.U

		val rounderRes = Wire(UInt((1 + esize + fsize).W))
		rounderRes := rounder.io.noAfterRounding

		io.res := rounderRes
	}
}