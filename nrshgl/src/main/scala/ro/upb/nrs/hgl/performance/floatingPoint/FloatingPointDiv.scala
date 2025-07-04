package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.util._

class FloatPDivIO(esize: Int, fsize: Int) extends Bundle {
	val op1 = Input(Bits((1 + esize + fsize).W))
	val op2 = Input(Bits((1 + esize + fsize).W))
	val sign = Output(Bool())
	val normalizedExponent = Output(UInt(esize.W))
	val normalizedMantissa = Output(UInt(fsize.W))
	val res = Output(Bits((1 + esize + fsize).W))
}

class FloatingPointDiv(esize: Int, fsize: Int) extends Module {
	val io = IO(new FloatPDivIO(esize, fsize))

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

	when((decodedOp1.notANo === true.B) || (decodedOp2.notANo === true.B) || (decodedOp2.positiveZero === true.B) || (decodedOp2.negativeZero === true.B)) {
		io.sign := decodedOp1.sign ^ decodedOp2.sign 
		io.normalizedExponent := (1.U << esize) - 1.U
		io.normalizedMantissa := 0.U(fsize.W)
		encoder.io.sign := io.sign
		encoder.io.exponent := io.normalizedExponent
		encoder.io.mantissa := io.normalizedMantissa
		io.res := encoder.io.out
	} .elsewhen(decodedOp1.positiveZero || decodedOp1.negativeZero) {
		io.sign := decodedOp1.sign ^ decodedOp2.sign
		io.normalizedExponent := decodedOp1.exponent
		io.normalizedMantissa := decodedOp1.mantissa
		encoder.io.sign := io.sign
		encoder.io.exponent := io.normalizedExponent
		encoder.io.mantissa := io.normalizedMantissa
		io.res := encoder.io.out
	} .otherwise {
		// sgn = op1.sgn ^ op2.sgn
		io.sign := decodedOp1.sign ^ decodedOp2.sign

		// exponent = op1.exponent - op2.exponent + bias
		val exponent = Wire(UInt(esize.W))
		exponent := decodedOp1.exponent - decodedOp2.exponent + bias.asUInt

		val concat1 = Wire(UInt((2 * fsize + 4).W))
		val concat2 = Wire(UInt((2 * fsize + 4).W))

		//TODO shift op1 mantisa left with fraction size 

		concat1 := Cat(decodedOp1.hiddenBit, decodedOp1.mantissa) << (fsize + 3)
		concat2 := Cat(decodedOp2.hiddenBit, decodedOp2.mantissa)

		printf("%d %d\n", concat1, concat2)

		val quotient = Wire(UInt((2 * fsize + 4).W))
		val remainder = Wire(UInt((2 * fsize + 4).W))

		quotient := concat1 / concat2
		remainder := concat1 % concat2
			
		printf("%d %d\n", quotient, remainder)

		/*quotient := 0.U((2 * fsize + 4).W)
		remainder := 0.U((2 * fsize + 4).W)*/

		io.normalizedMantissa := (Mux((quotient(fsize + 3) === false.B), quotient << 1, quotient))(fsize + 3, 3)

		io.normalizedExponent := Mux((quotient(fsize + 3) === false.B), exponent - 1.U, exponent)

		val l = Mux((quotient(fsize + 3) === false.B), quotient(2), quotient(3))
		val g = Mux((quotient(fsize + 3) === false.B), quotient(1), quotient(2))
		val r = Mux((quotient(fsize + 3) === false.B), quotient(0), quotient(1))
		val s = Mux((quotient(fsize + 3) === false.B), remainder.orR, (quotient(0) ## remainder).orR)

		encoder.io.sign := io.sign
		encoder.io.exponent := io.normalizedExponent
		encoder.io.mantissa := io.normalizedMantissa

		printf("%d %d\n", io.normalizedExponent, io.normalizedMantissa)

		val rounder = Module(new Rounding(esize, fsize))
		rounder.io.g := g
		rounder.io.l := l
		rounder.io.r := r
		rounder.io.s := s
		rounder.io.noBeforeRounding := encoder.io.out
		rounder.io.roundingMode := 4.U
			
		val rounderRes = Wire(UInt((1 + esize + fsize).W))
		rounderRes := rounder.io.noAfterRounding

		io.res := rounderRes
	}
}
