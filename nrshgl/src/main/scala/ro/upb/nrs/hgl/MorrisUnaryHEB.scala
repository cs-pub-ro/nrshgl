package ro.upb.nrs.hgl

import chisel3._
import chisel3.util._

case class MorrisUnaryHEB(
    size: Int,
    rounding : RoundingType,
    softwareDebug : Boolean
) extends Bundle with HNumberRepresentationSystem[MorrisUnaryHEB] with FloatingPointTrait {
    require(size >= 3)

	val value : UInt = Output(UInt((size).W))
    val internalExponentSize = MorrisUnaryHEB.internalExponentSize(size)
    val internalFractionSize = MorrisUnaryHEB.internalFractionSize(size)

    def floatingPointValue : FloatingPoint = {
		val decoder = Module(
            new DecoderMorrisUnaryHEB(
                size,
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
		decoder.io.binary := value
        val out = Wire(new FloatingPoint(
            internalExponentSize,
            internalFractionSize,
            softwareDebug
            )
        )
        out := decoder.io.result
		out
    }

	override def +(that: MorrisUnaryHEB): MorrisUnaryHEB = {
		val encode = Module(
            new EncoderMorrisUnaryHEB(
                size,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue + that.floatingPointValue
		val out = Wire(MorrisUnaryHEB(size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def -(that: MorrisUnaryHEB): MorrisUnaryHEB = {
		val encode = Module(
            new EncoderMorrisUnaryHEB(
                size,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue - that.floatingPointValue
		val out = Wire(MorrisUnaryHEB(size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def *(that: MorrisUnaryHEB): MorrisUnaryHEB = {
		val encode = Module(
            new EncoderMorrisUnaryHEB(
                size,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue * that.floatingPointValue
		val out = Wire(MorrisUnaryHEB(size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def /(that: MorrisUnaryHEB): MorrisUnaryHEB = {
		val encode = Module(
            new EncoderMorrisUnaryHEB(
                size,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue / that.floatingPointValue
		val out = Wire(MorrisUnaryHEB(size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}
    
    
	override def sqrt: MorrisUnaryHEB = {
		val encode = Module(
            new EncoderMorrisUnaryHEB(
                size,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue.sqrt
		val out = Wire(MorrisUnaryHEB(size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

    def getSize : Int = this.size
    def getInternalExponentSize : Int = this.internalExponentSize
    def getInternalFractionSize : Int = this.internalFractionSize
    def getDecoderFloatingPoint(
        floatingPointExponentSize : Int,
        floatingPointFractionSize : Int,
        floatingPointSize : Int,
        softwareDebug : Boolean
    ) : DecoderFloatingPoint = {
        new DecoderMorrisUnaryHEB(
            floatingPointSize,
            floatingPointExponentSize,
            floatingPointFractionSize,
            softwareDebug
        )
    }
    def getEncoderFloatingPoint(
        floatingPointExponentSize : Int,
        floatingPointFractionSize : Int,
        floatingPointSize : Int,
        softwareDebug : Boolean
    ) : EncoderFloatingPoint = {
        new EncoderMorrisUnaryHEB(
            floatingPointSize,
            Some(this.rounding),
            floatingPointExponentSize,
            floatingPointFractionSize,
            softwareDebug
        )
    }
    
    def getOptimalAccumulatorSize : BigInt = MorrisUnaryHEB.accumulatorSize(size)
    def getOptimalAccumulatorFractionSize : BigInt = MorrisUnaryHEB.accumulatorFractionSize(size)
}

object MorrisUnaryHEB {
	def apply(  size: Int,
                rounding : RoundingType = RoundEven,
                softwareDebug : Boolean = false
            ) : MorrisUnaryHEB = new MorrisUnaryHEB(size, rounding, softwareDebug)
    def minimumExponent(size: Int) : BigInt = -MorrisUnaryHEB.maximumExponent(size)
    def maximumExponent(size: Int) : BigInt = BigInt(1) << (size-3)
    def internalExponentSize(size: Int) : Int = {
        // subnormalExponent - fracton_size is the minimum possible exponent
        // -1 for the number between 0 and the minimum value
        val maximumExponentAbsoluteValue = MorrisUnaryHEB.maximumExponent(size).max(MorrisUnaryHEB.minimumExponent(size))
        log2Ceil(maximumExponentAbsoluteValue + 1)
    }
    def internalFractionSize(size: Int) : Int = size-3
    def accumulatorFractionSize(size: Int) : BigInt = {
        2 * (MorrisUnaryHEB.minimumExponent(size)).abs
    }
    def accumulatorSize(size: Int) : BigInt = {
        MorrisUnaryHEB.accumulatorFractionSize(size) + //afs
        2 * MorrisUnaryHEB.maximumExponent(size) + //max exp
        1 +//sign
        1 //overflow
    }
}


class DecoderMorrisUnaryHEB(
    size: Int,
    internalExponentSize : Int,
    internalFractionSize : Int,
    softwareDebug : Boolean = false
    ) extends DecoderFloatingPoint(
        internalExponentSize,
        internalFractionSize,
        size,
        softwareDebug
    ) {
    val minimumInternalFractionSize = MorrisUnaryHEB.internalFractionSize(size)
    val minimumInternalExponentSize = MorrisUnaryHEB.internalExponentSize(size)
    val floatingPoint = Wire(new FloatingPoint(
            minimumInternalExponentSize,
            minimumInternalFractionSize,
            softwareDebug
        )
    )
    floatingPoint.sign := io.binary(size-1)
    floatingPoint.zero := io.binary(size-1, 0) === Fill(size, 0.U(1.W))
    floatingPoint.nan := io.binary(size-1) & (io.binary(size-2, 0) === Fill(size-1, 0.U(1.W)))
    floatingPoint.inf := floatingPoint.nan
    floatingPoint.underflow := false.B
    floatingPoint.overflow := false.B
    floatingPoint.restBits := 0.U(3.W)
    
    val decodeBits = Wire(UInt((size-1).W))
    decodeBits := io.binary(size-2, 0)
    val regimeFirstBit = Wire(Bool())
    regimeFirstBit := decodeBits(size-2)
    val consecutiveValues = Wire(UInt(log2Ceil(size).W))
    consecutiveValues := Mux(
                                regimeFirstBit,
                                PriorityEncoder(Reverse(~decodeBits)),
                                PriorityEncoder(Reverse(decodeBits))
                            )
    val regimeSize = Wire(UInt(log2Ceil(size+1).W))
    regimeSize := consecutiveValues + 1.U
    
    if(softwareDebug) {
        printf("[DecoderMorrisUnaryHEB] regimeFirstBit DEC: %d, regimeFirstBit HEX %x\n",
        regimeFirstBit, regimeFirstBit)
        printf("[DecoderMorrisUnaryHEB] consecutiveValues DEC: %d, consecutiveValues HEX %x\n",
        consecutiveValues, consecutiveValues)
        printf("[DecoderMorrisUnaryHEB] regimeSize DEC: %d, regimeSize HEX %x\n",
        regimeSize, regimeSize)
    }
    val exponentSign = Wire(Bool())
    exponentSign := !regimeFirstBit
    val minusOneExponentSize = Wire(Bool())
    minusOneExponentSize := (decodeBits(size-2, size-3) === 2.U)
    val exponentSize = Wire(UInt((log2Ceil(minimumInternalExponentSize+1)).W))
    when(minusOneExponentSize) {
        exponentSize := 0.U
    } .otherwise {
        when(exponentSign) {
            exponentSize := consecutiveValues - 1.U
        } .otherwise {
            exponentSize := Mux(
                                    decodeBits === Fill(size-1, 1.U(1.W)), //special case when maximum value
                                    consecutiveValues - 1.U,
                                    consecutiveValues - 2.U
                                )
        }
    }
    if(softwareDebug) {
        printf("[DecoderMorrisUnaryHEB] exponentSign DEC: %d, exponentSign HEX %x\n",
        exponentSign, exponentSign)
        printf("[DecoderMorrisUnaryHEB] minusOneExponentSize DEC: %d, minusOneExponentSize HEX %x\n",
        minusOneExponentSize, minusOneExponentSize)
        printf("[DecoderMorrisUnaryHEB] exponentSize DEC: %d, exponentSize HEX %x\n",
        exponentSize, exponentSize)
    }
    val fractionSize = Wire(UInt(log2Ceil(size+1).W))
    fractionSize := Mux(
                            regimeSize >= (size - 1).U - exponentSize,
                            0.U,
                            (size - 1).U - exponentSize - regimeSize
                        )
    val mantissaBits = Wire(UInt((minimumInternalFractionSize+1).W))
    mantissaBits := Mux(
                                fractionSize === 0.U,
                                0.U,
                                io.binary << (minimumInternalFractionSize.U - fractionSize)
                            ) | (1.U(1.W) ## 0.U(minimumInternalFractionSize.W))
    floatingPoint.mantissa := mantissaBits
    if(softwareDebug) {
        printf("[DecoderMorrisUnaryHEB] fractionSize DEC: %d, fractionSize HEX %x\n",
        fractionSize, fractionSize)
        printf("[DecoderMorrisUnaryHEB] mantissaBits DEC: %d, mantissaBits HEX %x\n",
        mantissaBits, mantissaBits)
    }

    val binaryExponentSize = Wire(UInt(log2Ceil(size).W))
    binaryExponentSize := Mux(
                                regimeSize >= (size - 1).U,
                                0.U,
                                Mux(
                                    regimeSize > (size - 1).U - exponentSize,
                                    (size - 1).U - regimeSize,
                                    exponentSize
                                )
                            )
    val exponentMask = Wire(UInt(size.W))
    exponentMask := Fill(size, 1.U(1.W)) >> (regimeSize + 1.U)
    val binaryExponent = Wire(UInt(minimumInternalExponentSize.W))
    binaryExponent := Mux(minusOneExponentSize,
                            0.U,
                            Mux(
                                binaryExponentSize === 0.U,
                                0.U, //special ccase for negative exponents
                                ( (Mux(exponentSign, ~io.binary, io.binary) & exponentMask) >> (fractionSize) ) << (exponentSize - binaryExponentSize)
                            ) | (1.U << exponentSize)
                        )
    
    floatingPoint.exponentSign := exponentSign
    floatingPoint.exponentAbsoluteValue := binaryExponent
    when(binaryExponent === 0.U) {
        floatingPoint.exponentSign := false.B
    }

    val convertingModule = Module(new FloatingPointConverter(
            minimumInternalExponentSize,
            minimumInternalFractionSize,
            internalExponentSize,
            internalFractionSize,
            softwareDebug
        )
    )
    convertingModule.io.source := floatingPoint
    io.result := convertingModule.io.destination

    if(softwareDebug) {
        printf("[DecoderMorrisUnaryHEB] floatingPoint.sign DEC: %d, floatingPoint.zero DEC: %d, floatingPoint.inf DEC: %d, floatingPoint.nan DEC: %d, floatingPoint.underflow DEC: %d, floatingPoint.overflow DEC: %d, \n",
        floatingPoint.sign, floatingPoint.zero, floatingPoint.inf, floatingPoint.nan, floatingPoint.underflow, floatingPoint.overflow)
        printf("[DecoderMorrisUnaryHEB] floatingPoint.exponentSign DEC: %d, floatingPoint.exponentSign HEX %x\n",
        floatingPoint.exponentSign, floatingPoint.exponentSign)
        printf("[DecoderMorrisUnaryHEB] floatingPoint.exponentAbsoluteValue DEC: %d, floatingPoint.exponentAbsoluteValue HEX %x\n",
        floatingPoint.exponentAbsoluteValue, floatingPoint.exponentAbsoluteValue)
        printf("[DecoderMorrisUnaryHEB] floatingPoint.mantissa DEC: %d, floatingPoint.mantissa HEX %x\n",
        floatingPoint.mantissa, floatingPoint.mantissa)
        printf("[DecoderMorrisUnaryHEB] floatingPoint.restBits DEC: %d, floatingPoint.restBits HEX %x\n",
        floatingPoint.restBits, floatingPoint.restBits)
        printf("[DecoderMorrisUnaryHEB] mantissaBits DEC: %d, mantissaBits HEX %x\n",
        mantissaBits, mantissaBits)
    }
}


class EncoderMorrisUnaryHEB(
    size: Int,
    rounding : Some[RoundingType],
    internalExponentSize : Int,
    internalFractionSize : Int,
    softwareDebug : Boolean = false
    ) extends EncoderFloatingPoint(
        internalExponentSize,
        internalFractionSize,
        size,
        rounding,
        softwareDebug
    ) {
    val minimumInternalFractionSize = MorrisUnaryHEB.internalFractionSize(size)
    val minimumInternalExponentSize = MorrisUnaryHEB.internalExponentSize(size)
    val minimumExponentValue = (MorrisUnaryHEB.minimumExponent(size)).abs
    val maximumExponentValue = (MorrisUnaryHEB.maximumExponent(size)).abs
    io.binary := 0.U
    val floatingPoint = Wire(new FloatingPoint(
            minimumInternalExponentSize,
            minimumInternalFractionSize,
            softwareDebug
        )
    )
    
    val convertingModule = Module(new FloatingPointConverter(
            internalExponentSize,
            internalFractionSize,
            minimumInternalExponentSize,
            minimumInternalFractionSize,
            softwareDebug
        )
    )
    convertingModule.io.source := io.floatingPoint
    floatingPoint := convertingModule.io.destination
    
    val overflow = Wire(Bool())
    overflow := (!floatingPoint.exponentSign & (floatingPoint.exponentAbsoluteValue > maximumExponentValue.U)) ||
                floatingPoint.overflow
    val underflow = Wire(Bool())
    underflow := (floatingPoint.exponentSign & (floatingPoint.exponentAbsoluteValue > minimumExponentValue.U)) || floatingPoint.underflow
    if(softwareDebug) printf("[EncoderMorrisUnaryHEB] overflow DEC: %d, underflow DEC %d\n", overflow, underflow)

    when(floatingPoint.nan || floatingPoint.inf) {
        io.binary := 1.U(1.W) ## Fill(size-1, 0.U(1.W))
    } .elsewhen(floatingPoint.zero) {
        io.binary := Fill(size, 0.U(1.W))
    } .elsewhen(overflow) {
        io.binary := 1.U(1.W) ## Fill(size-1, 0.U(1.W))
    } .elsewhen(underflow) {
        io.binary := Fill(size, 0.U(1.W))
    } .otherwise {
        val exponentMSBZero = Wire(UInt((log2Ceil(minimumInternalExponentSize+2)).W))
        exponentMSBZero := PriorityEncoder(Reverse(floatingPoint.exponentAbsoluteValue))
        val exponentSize = Wire(UInt((log2Ceil(minimumInternalExponentSize+1)).W))
        val minusOneExponentSize = Wire(Bool())
        minusOneExponentSize := floatingPoint.exponentAbsoluteValue === 0.U
        exponentSize := minimumInternalExponentSize.U -& exponentMSBZero -& 1.U
        val regimeAbsoluteValue = Wire(UInt(log2Ceil(size+1).W))
        regimeAbsoluteValue := exponentSize +& 1.U
        val regimeFirstBit = Wire(Bool())
        regimeFirstBit := !floatingPoint.exponentSign
        val consecutiveValues = Wire(UInt(log2Ceil(size).W))
        when(minusOneExponentSize) {
            exponentSize := 0.U
            consecutiveValues := 1.U
        } .otherwise {
            when(floatingPoint.exponentSign) {
                consecutiveValues := regimeAbsoluteValue
            } .otherwise {
                consecutiveValues := regimeAbsoluteValue +& 1.U
            }
        }
        val regimeSize = Wire(UInt(log2Ceil(size+1).W))
        regimeSize := consecutiveValues +& 1.U
        if(softwareDebug) {
            printf("[EncoderMorrisUnaryHEB] regimeAbsoluteValue DEC: %d, regimeAbsoluteValue HEX %x\n", regimeAbsoluteValue, regimeAbsoluteValue)
            printf("[EncoderMorrisUnaryHEB] regimeFirstBit DEC: %d, regimeFirstBit HEX %x\n", regimeFirstBit, regimeFirstBit)
            printf("[EncoderMorrisUnaryHEB] consecutiveValues DEC: %d, consecutiveValues HEX %x\n", consecutiveValues, consecutiveValues)
            printf("[EncoderMorrisUnaryHEB] regimeSize DEC: %d, regimeSize HEX %x\n", regimeSize, regimeSize)
        }
        val fractionSize = Wire(UInt(log2Ceil(size+1).W))
        fractionSize := Mux(
                                regimeSize >= (size - 1).U - exponentSize,
                                0.U,
                                (size - 1).U - exponentSize - regimeSize
                            )
        if(softwareDebug) printf("[EncoderMorrisUnaryHEB] fractionSize DEC: %d, fractionSize HEX %x\n", fractionSize, fractionSize)
        val mantissaBits = Wire(UInt(minimumInternalFractionSize.W))
        mantissaBits := floatingPoint.mantissa(minimumInternalFractionSize-1, 0) >> (minimumInternalFractionSize.U - fractionSize)
        val binaryExponentSize = Wire(UInt(log2Ceil(size).W))
        binaryExponentSize := Mux(
                                    regimeSize >= (size - 1).U,
                                    0.U,
                                    Mux(
                                        regimeSize > (size - 1).U - exponentSize,
                                        (size - 1).U - regimeSize,
                                        exponentSize
                                    )
                                )
        val exponentBits = Wire(UInt((size-1).W))
        val exponentMask = Wire(UInt(minimumInternalExponentSize.W))
        exponentMask := Fill(minimumInternalExponentSize, 1.U(1.W)) >> (minimumInternalExponentSize.U - exponentSize)
        // delete MSB & ~(1.U << binaryExponentSize)
        // for negative exponent sign there is a need to negate TODO
        when(floatingPoint.exponentSign) {
            exponentBits := ((~floatingPoint.exponentAbsoluteValue & exponentMask) >> (exponentSize - binaryExponentSize) ) << fractionSize
        } .otherwise {
            exponentBits := ((floatingPoint.exponentAbsoluteValue & exponentMask) >> (exponentSize - binaryExponentSize) ) << fractionSize
        }
        if(softwareDebug) {
            printf("[EncoderMorrisUnaryHEB] exponentMask DEC: %d, exponentMask HEX %x\n", exponentMask, exponentMask)
            printf("[EncoderMorrisUnaryHEB] floatingPoint.exponentAbsoluteValue DEC: %d, floatingPoint.exponentAbsoluteValue HEX %x\n", floatingPoint.exponentAbsoluteValue, floatingPoint.exponentAbsoluteValue)
            printf("[EncoderMorrisUnaryHEB] exponentBits DEC: %d, exponentBits HEX %x\n", exponentBits, exponentBits)
            printf("[EncoderMorrisUnaryHEB] binaryExponentSize DEC: %d, binaryExponentSize HEX %x\n", binaryExponentSize, binaryExponentSize)
        }
        val regimeBits = Wire(UInt((size-1).W))
        regimeBits := Mux(
                            regimeFirstBit,
                            Mux(consecutiveValues === (size-1).U, //special case maximum value
                                Fill(size-1, 1.U(1.W)),
                                ( ((Fill(size, 1.U(1.W)) ^ 3.U) << (binaryExponentSize + fractionSize))(size - 1, 0) >> 1 )
                            ),
                            ( (1.U << (binaryExponentSize + fractionSize)) )
                        )
        if(softwareDebug) printf("[EncoderMorrisUnaryHEB] fractionSize DEC: %d, fractionSize HEX %x\n", fractionSize, fractionSize)
        val partialBinary = Wire(UInt(size.W))
        //rounding
		val roundingModule = Module(new FloatingPointRounding(softwareDebug))
        roundingModule.io.sign := floatingPoint.sign
        roundingModule.io.l := partialBinary(0)
        roundingModule.io.g := floatingPoint.restBits(2)
        roundingModule.io.r := floatingPoint.restBits(1)
        roundingModule.io.s := floatingPoint.restBits(0)
        io.roundingType match {
            case Some(r) => roundingModule.io.rounding := r
            case None => roundingModule.io.rounding := RoundingType.toUInt(rounding.get)
        }
        partialBinary := (floatingPoint.sign ## Fill(size-1, 0.U(1.W))) | regimeBits | exponentBits | mantissaBits

        when(((exponentSize - binaryExponentSize) >= 1.U) && floatingPoint.exponentSign && (binaryExponentSize >= 1.U)) {
            io.binary := partialBinary -& roundingModule.io.addOne //if the last bits are exponent and the exponent is negative you have to substract one
        } .elsewhen( (partialBinary(size-2,0) === Fill(size-1, 1.U(1.W))) && (roundingModule.io.addOne === 1.U)) {
            io.binary := 1.U(1.W) ## Fill(size-1, 0.U(1.W))
        } .otherwise {
            io.binary := partialBinary +& roundingModule.io.addOne
        }

        val exponent = Wire(UInt(minimumInternalExponentSize.W))
        exponent := floatingPoint.exponentAbsoluteValue
        when(minimumInternalFractionSize.U > fractionSize) {//ussualy happens
            when( (exponentSize - binaryExponentSize) >= 2.U) {
                //when g and r are exponent bits 
                when(exponentSize >= 2.U) {
                    roundingModule.io.g := (exponent >> (exponentSize - 1.U - binaryExponentSize))(0)
                    roundingModule.io.r := (exponent >> (exponentSize - 2.U - binaryExponentSize))(0)
                    // calcualte s from the other exponent bits, mantisa bits and any other rest bits
                    //s := (exponent & (Fill(exponentSize-2, 1.U(1.W)) >> binaryExponentSize)).orR || floatingPoint.mantissa(internalFractionSize-1, 0).orR || floatingPoint.restBits.orR
                    when(exponentSize > 2.U) {
                        roundingModule.io.s := !((exponent & ((1.U<<((exponentSize - 2.U - binaryExponentSize)))-1.U)) === 0.U) ||
                                                floatingPoint.mantissa(minimumInternalFractionSize-1, 0).orR ||
                                                floatingPoint.restBits.orR
                    } .otherwise {
                        roundingModule.io.s := floatingPoint.mantissa(minimumInternalFractionSize-1, 0).orR ||
                                                floatingPoint.restBits.orR
                    }
                } .otherwise {
                    //case imposible
                    roundingModule.io.g := false.B
                    roundingModule.io.r := false.B
                    roundingModule.io.s := false.B
                }
            } .elsewhen( (exponentSize - binaryExponentSize) === 1.U) {
                // when g is exponent bits and rest are fraction
                when(exponentSize >= 1.U) {
                    roundingModule.io.g := exponent(0)
                } .otherwise {
                    //case imposible
                    roundingModule.io.g := false.B
                }
                roundingModule.io.r := floatingPoint.mantissa(minimumInternalFractionSize-1)
                roundingModule.io.s := floatingPoint.mantissa(minimumInternalFractionSize-1, 0).orR ||
                                        floatingPoint.restBits.orR
            } .otherwise {
                when(fractionSize === (minimumInternalFractionSize-1).U) {
                    roundingModule.io.g := floatingPoint.mantissa(0)
                    roundingModule.io.r := floatingPoint.restBits(2)
                    roundingModule.io.s := floatingPoint.restBits(1) | floatingPoint.restBits(0)
                } .elsewhen(fractionSize === (minimumInternalFractionSize-2).U) {
                    roundingModule.io.g := floatingPoint.mantissa(1)
                    roundingModule.io.r := floatingPoint.mantissa(0)
                    roundingModule.io.s := floatingPoint.restBits.orR
                } .elsewhen(fractionSize === (minimumInternalFractionSize-3).U) {
                    roundingModule.io.g := floatingPoint.mantissa(2)
                    roundingModule.io.r := floatingPoint.mantissa(1)
                    roundingModule.io.s := floatingPoint.mantissa(0) | floatingPoint.restBits.orR
                } .elsewhen(fractionSize < (minimumInternalFractionSize-3).U) {
                    //when g r and s are fraction bits
                    roundingModule.io.g := ( floatingPoint.mantissa & (1.U << ((minimumInternalFractionSize-1).U - fractionSize)) ).orR
                    roundingModule.io.r := ( floatingPoint.mantissa & (1.U << ((minimumInternalFractionSize-2).U - fractionSize)) ).orR
                    //i don't understand why -1 math says -2
                    roundingModule.io.s := ( floatingPoint.mantissa & ((1.U << ((minimumInternalFractionSize-2).U - fractionSize))-1.U) ).orR ||
                        floatingPoint.restBits.orR
                } .otherwise {
                    printf("[EncoderMorrisUnaryHEB][ERROR] fractionSize DEC: %d, fractionSize HEX %x\n", fractionSize, fractionSize)
                }
            }
        }
        
        if(softwareDebug) {
            printf("[EncoderMorrisUnaryHEB] partialBinary DEC: %d, partialBinary HEX %x\n", partialBinary, partialBinary)
            printf("[EncoderMorrisUnaryHEB] regimeBits DEC: %d, regimeBits HEX %x\n", regimeBits, regimeBits)
            printf("[EncoderMorrisUnaryHEB] exponentBits DEC: %d, exponentBits HEX %x\n", exponentBits, exponentBits)
            printf("[EncoderMorrisUnaryHEB] mantissaBits DEC: %d, mantissaBits HEX %x\n", mantissaBits, mantissaBits)
            printf("[EncoderMorrisUnaryHEB] partialBinary DEC: %d, partialBinary HEX %x\n", partialBinary, partialBinary)
        }
    }
    if(softwareDebug) printf("[EncoderMorrisUnaryHEB] io.binary DEC: %d, io.binary HEX %x\n", io.binary, io.binary)
}