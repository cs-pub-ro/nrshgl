package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.util._

class FloatPRoot(esize: Int, fsize: Int) extends Bundle {
	val in = Input(Bits((1 + esize + fsize).W))
	val sign = Output(Bool())
	val exponent = Output(UInt(esize.W))
	val mantissa = Output(UInt(fsize.W))
	val normalizedExponent = Output(UInt(esize.W))
	val normalizedMantissa = Output(UInt(fsize.W))
	var notANo = Output(Bool())
	var negativeInf = Output(Bool())
	var positiveInf = Output(Bool())
	var negativeZero = Output(Bool())
	var positiveZero = Output(Bool())
	var denormal = Output(Bool())
	var hiddenBit = Output(UInt(1.W))
	var outputReady = Output(Bool())
}

/*
class FloatingPointSqrt(esize: Int, fsize: Int) extends Module {
	val io = IO(new FloatPRoot(esize, fsize))

	io.sign := false.B
	io.exponent := 0.U
	io.mantissa := 0.U
	io.normalizedExponent := 0.U
	io.normalizedMantissa := 0.U
	io.notANo := false.B
	io.negativeInf := false.B
	io.positiveInf := false.B
	io.negativeZero := false.B
	io.positiveZero := false.B
	io.denormal := false.B
	io.hiddenBit := 1.U(1.W)
	io.outputReady := false.B

	// init the decoder module
	val decoder = Module(new Decode(esize, fsize))

	// init the chosen representation for FP
	val decodedOp = Wire(new FloatP(esize, fsize))

	// decode the raw bits that represent the input
	decoder.io.in := io.in

	decodedOp := decoder.io

	// special values and common case
	when(decodedOp.sign || decodedOp.notANo) {
		io.notANo := true.B
	} .elsewhen(decodedOp.positiveZero || decodedOp.negativeZero) {
		io.exponent := decodedOp.exponent
		io.mantissa := decodedOp.mantissa
		io.normalizedExponent := decodedOp.exponent
		io.normalizedMantissa := decodedOp.mantissa
		io.notANo := decodedOp.notANo
		io.negativeZero := decodedOp.negativeZero
		io.positiveZero := decodedOp.positiveZero
		io.denormal := decodedOp.denormal
		io.hiddenBit := decodedOp.hiddenBit
	} .otherwise {
		val auxExponent = Wire(Bits(8.W))
		auxExponent := decodedOp.exponent

		val auxMantissa = Wire(Bits(25.W))

		when(auxExponent(0) === false.B) {
			auxMantissa := 0.U(1.W) ## decodedOp.hiddenBit ## decodedOp.mantissa
		} .otherwise {
			auxMantissa := (decodedOp.hiddenBit ## decodedOp.mantissa) << 1
		}

		val bias = 127

		io.exponent := decodedOp.exponent >> 1


		val sqrtModule = Module(new UIntSqrt(esize, fsize))
		sqrtModule.io.input := 0.U(1.W) ## auxMantissa
		sqrtModule.io.inputReady := true.B
	}
}

//TODO:  without clk
class UIntSqrt(size: Int) extends Module {
	val io = IO(new Bundle {
		val input = Input(UInt(26.W))
		val inputReady = Input(Bool())
		val remainder = Output(UInt(15.W))
		val output = Output(UInt(13.W))
		val outputReady = Output(Bool())
	})

	val regR = RegInit(0.S(15.W))
	val regQ = RegInit(0.U(13.W))
	val regD = RegInit(0.U(26.W))
	val regCounter = RegInit(0.U(13.W))

	val nextR = Wire(UInt(15.W))
	val diff = Wire(SInt(15.W))
	val restoredR = Wire(SInt(15.W))

	// concatenate the next 2 bits from D to the partialR
	nextR := (regR(13, 0) << 2) | regD(25 - 2 * regCounter.litValue, 25 - 2 * (regCounter.litValue + 1))

	// diff = nextRemainder - (current_q ## 01) 
	diff := nextR.asSInt - ((regQ.zext << 2) | 1.S(2.W))

	// restoredRemainder = nextRemainder + (current_q ## 11)
	restoredR := nextR.asSInt + ((regQ.zext << 2) | 3.S(2.W))

	when(io.inputReady) {
		regD := io.input
		regQ := 0.U
		regR := 0.U
		regCounter := 0.U
	} .otherwise {
		when(regR(14) === false.B) {
			regR := diff
			when(diff(14) === true.B) {
				regQ := (regQ << 1) | 1.U(1.W)
			} .otherwise {
				regQ := regQ << 1
			}
		} .otherwise {
			regR := restoredR
			when(restoredR(14) === false.B) {
				regQ := (regQ << 1) | 1.U(1.W)
			} .otherwise {
				regQ := regQ << 1
			}
		}

		regCounter := regCounter + 1.U
	}

	printf("%d\n", regCounter)

	io.output := regQ
	io.outputReady := (regCounter === 13.U)
	io.remainder := regR
}*/