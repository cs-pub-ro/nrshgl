package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.util._

class FloatPAddSubIO(esize: Int, fsize: Int) extends Bundle {
	val op1 = Input(Bits((1 + esize + fsize).W))
	val op2 = Input(Bits((1 + esize + fsize).W))
    val opSel = Input(Bool())
	val sign = Output(Bool())
	val normalizedExponent = Output(UInt(esize.W))
	val normalizedMantissa = Output(UInt(fsize.W))
	val res = Output(Bits((1 + esize + fsize).W))
}

class FloatingPointAddSub(esize: Int, fsize: Int) extends Module {
    val io = IO(new FloatPAddSubIO(esize, fsize))
    
    // instantiate the module to decode the operands and to encode the result
	val decoder1 = Module(new Decode(esize, fsize))
	val decoder2 = Module(new Decode(esize, fsize))
	val encoder = Module(new Encode(esize, fsize))

	// instantiate the chosen representations for FP
	val decodedOp1 = Wire(new FloatPIO(esize, fsize))
	val decodedOp2 = Wire(new FloatPIO(esize, fsize))

	decoder1.io.in := io.op1
	decoder2.io.in := io.op2

	decodedOp1 := decoder1.io
	decodedOp2 := decoder2.io

    val equalOps = Wire(Bool())
    equalOps := false.B

    when (decodedOp1.notANo || decodedOp2.notANo) {
        io.sign := false.B
        io.normalizedExponent := (1.U << esize.U) - 1.U
        io.normalizedMantissa := 0.U
        encoder.io.sign := io.sign
        encoder.io.exponent := io.normalizedExponent
        encoder.io.mantissa := io.normalizedMantissa
        io.res := encoder.io.out
    } .elsewhen((decodedOp1.positiveZero || decodedOp1.negativeZero) && (io.opSel === false.B)) {
        io.sign := decodedOp2.sign
        io.normalizedExponent := decodedOp2.sign
        io.normalizedMantissa := decodedOp2.mantissa
        encoder.io.sign := io.sign
        encoder.io.exponent := io.normalizedExponent
        encoder.io.mantissa := io.normalizedMantissa
        io.res := encoder.io.out
    } .elsewhen((decodedOp2.positiveZero || decodedOp2.negativeZero) && (io.opSel === false.B)) {
        io.sign := decodedOp1.sign
        io.normalizedExponent := decodedOp1.exponent
        io.normalizedMantissa := decodedOp1.mantissa
        encoder.io.sign := io.sign
        encoder.io.exponent := io.normalizedExponent
        encoder.io.mantissa := io.normalizedMantissa
        io.res := encoder.io.out
    } .elsewhen((decodedOp1.positiveZero || decodedOp1.negativeZero) && (io.opSel === true.B)) {
        io.sign := !decodedOp2.sign
        io.normalizedExponent := decodedOp2.exponent
        io.normalizedMantissa := decodedOp2.mantissa
        encoder.io.sign := io.sign
        encoder.io.exponent := io.normalizedExponent
        encoder.io.mantissa := io.normalizedMantissa
        io.res := encoder.io.out
    } .elsewhen((decodedOp2.positiveZero || decodedOp2.negativeZero) && (io.opSel === true.B)) {
        io.sign := decodedOp1.sign
        io.normalizedExponent := decodedOp1.exponent
        io.normalizedMantissa := decodedOp1.mantissa
        encoder.io.sign := io.sign
        encoder.io.exponent := io.normalizedExponent
        encoder.io.mantissa := io.normalizedMantissa
        io.res := encoder.io.out
    } .elsewhen((decodedOp1.sign ## decodedOp1.exponent ## decodedOp1.mantissa) === (decodedOp2.sign ## decodedOp2.exponent ## decodedOp2.mantissa) && (io.opSel === true.B)) {
        io.sign := false.B
        io.normalizedExponent := 0.U(esize.W)
        io.normalizedMantissa := 0.U(esize.W)
        encoder.io.sign := io.sign
        encoder.io.exponent := io.normalizedExponent
        encoder.io.mantissa := io.normalizedMantissa
        io.res := encoder.io.out
    } .otherwise {
        val diffSign = !(decodedOp1.sign === decodedOp2.sign)
        val op = io.opSel
        val gtOperand = ((decodedOp1.exponent ## decodedOp1.mantissa) >= (decodedOp2.exponent ## decodedOp2.mantissa))

        val op1 = false.B ## Mux(gtOperand, decodedOp1.exponent ## decodedOp1.mantissa, decodedOp2.exponent ## decodedOp2.mantissa)
        val op2 = false.B ## Mux(gtOperand, decodedOp2.exponent ## decodedOp2.mantissa, decodedOp1.exponent ## decodedOp1.mantissa)

        val op2Neg = diffSign ^ op

        io.sign := (op2Neg & (!gtOperand)) ^ decodedOp1.sign

        val mantissa1 = Wire(UInt((2 * fsize + 5).W))
        val mantissa2 = Wire(UInt((2 * fsize + 5).W))
        val mantissa3 = Wire(UInt((2 * fsize + 5).W))

        val possibleExponent = Wire(UInt(esize.W))
        possibleExponent := op1(esize + fsize - 1, fsize)

        val normalizedExponent = Wire(UInt(esize.W))

        val concat1 = Wire(UInt((fsize + 1).W))
        concat1 := decodedOp1.hiddenBit ## op1(fsize - 1, 0)

        mantissa1 := false.B ## Cat(decodedOp1.hiddenBit ## op1(fsize - 1, 0), 0.U((fsize + 3).W))
            
        val possibleMantissa2 = false.B ## ((decodedOp2.hiddenBit ## op2(fsize - 1, 0)) ## 0.U((fsize + 3).W)) >> (op1(esize + fsize - 1, fsize) - op2(esize + fsize - 1, fsize))

        mantissa2 := Mux(op2Neg, ~possibleMantissa2 + 1.U, possibleMantissa2)

        mantissa3 := mantissa1 +& mantissa2

        val normalizedMantissa = Wire(UInt((2 * fsize + 4).W))

        when(mantissa3(2 * fsize + 4) === true.B) {
            normalizedExponent := possibleExponent + 1.U
            normalizedMantissa := (mantissa3 >> 1)
        } .elsewhen(mantissa3(2 * fsize + 3) === true.B) {
            normalizedExponent := possibleExponent
            normalizedMantissa := mantissa3
        } .otherwise {
            val positionsToShift = Mux(mantissa3 === 0.U, 0.U, PriorityEncoder(Reverse(mantissa3(2 * fsize + 3, 0))))
            normalizedMantissa := (mantissa3 << positionsToShift)
            normalizedExponent := possibleExponent - positionsToShift
        }

        io.normalizedExponent := Mux(equalOps, 0.U(esize.W), normalizedExponent)
        io.normalizedMantissa := Mux(equalOps, 0.U(fsize.W), normalizedMantissa(2 * fsize + 2, fsize + 3))

        val l = io.normalizedMantissa(0)
        val g = normalizedMantissa(fsize + 2)
        val r = normalizedMantissa(fsize + 1)
        val s = normalizedMantissa(fsize, 0).orR

        encoder.io.sign := io.sign  
        encoder.io.exponent := io.normalizedExponent
        encoder.io.mantissa := io.normalizedMantissa

        val rounder = Module(new Rounding(esize, fsize))
        rounder.io.g := g
        rounder.io.r := r
        rounder.io.s := s
        rounder.io.l := l
        rounder.io.noBeforeRounding := encoder.io.out
        rounder.io.roundingMode := 4.U

        io.res := rounder.io.noAfterRounding
    }
}
