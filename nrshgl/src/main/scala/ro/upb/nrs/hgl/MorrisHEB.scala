package ro.upb.nrs.hgl

import chisel3._
import chisel3.util._

case class MorrisHEB(
    gSize: Int,
    size: Int,
    rounding : RoundingType,
    softwareDebug : Boolean
) extends Bundle with HNumberRepresentationSystem[MorrisHEB] with FloatingPointTrait {
    require(gSize >= 1)
    require(size >= 3)
    require((size-gSize) >= 3)

    val internalExponentSize = MorrisHEB.internalExponentSize(gSize, size)
    val internalFractionSize = MorrisHEB.internalFractionSize(gSize, size)
	val value : UInt = Output(UInt((size).W))

    def floatingPointValue : FloatingPoint = {
		val decoder = Module(
            new DecoderMorrisHEB(
                gSize,
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

	override def +(that: MorrisHEB): MorrisHEB = {
		val encode = Module(
            new EncoderMorrisHEB(
                gSize,
                size,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue + that.floatingPointValue
		val out = Wire(MorrisHEB(gSize, size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def -(that: MorrisHEB): MorrisHEB = {
		val encode = Module(
            new EncoderMorrisHEB(
                gSize,
                size,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue - that.floatingPointValue
		val out = Wire(MorrisHEB(gSize, size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def *(that: MorrisHEB): MorrisHEB = {
		val encode = Module(
            new EncoderMorrisHEB(
                gSize,
                size,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue * that.floatingPointValue
		val out = Wire(MorrisHEB(gSize, size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def /(that: MorrisHEB): MorrisHEB = {
		val encode = Module(
            new EncoderMorrisHEB(
                gSize,
                size,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue / that.floatingPointValue
		val out = Wire(MorrisHEB(gSize, size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}
    
    
	override def sqrt: MorrisHEB = {
		val encode = Module(
            new EncoderMorrisHEB(
                gSize,
                size,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue.sqrt
		val out = Wire(MorrisHEB(gSize, size, rounding, softwareDebug))
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
        new DecoderMorrisHEB(
            this.gSize,
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
        new EncoderMorrisHEB(
            this.gSize,
            floatingPointSize,
            Some(this.rounding),
            floatingPointExponentSize,
            floatingPointFractionSize,
            softwareDebug
        )
    }
    
    def getOptimalAccumulatorSize : BigInt = MorrisHEB.accumulatorSize(gSize, size)
    def getOptimalAccumulatorFractionSize : BigInt = MorrisHEB.accumulatorFractionSize(gSize, size)
}

object MorrisHEB {
	def apply(
        gSize: Int,
        size: Int,
        rounding : RoundingType = RoundZero,
        softwareDebug : Boolean = false
    ) : MorrisHEB = new MorrisHEB(gSize, size, rounding, softwareDebug)
    def minimumExponent(gSize: Int, size: Int) : BigInt = -MorrisHEB.maximumExponent(gSize, size)
    def maximumExponent(gSize: Int, size: Int) : BigInt = {
        val maxExponentSize = (1 << gSize) - 2 // G=g_value+1 , max g_value = 2^g_size - 1
        val remainingBitsSize = size - 1 - gSize - 1 // fraction sign, g_size and exponent sign
        (if(maxExponentSize <= remainingBitsSize) {
            ((BigInt(1)<<maxExponentSize) - 1)
        } else {
            ( ((BigInt(1)<<remainingBitsSize) - 1) << (maxExponentSize - remainingBitsSize) )
        }) + (BigInt(1) << maxExponentSize)
        
    }
    def internalExponentSize(gSize: Int, size: Int) : Int = {
        // subnormalExponent - fracton_size is the minimum possible exponent
        // -1 for the number between 0 and the minimum value
        val maximumExponentValue = MorrisHEB.maximumExponent(gSize, size)
        log2Ceil(maximumExponentValue + 1)
    }
    
    def internalFractionSize(gSize: Int, size: Int) : Int = size-2-gSize
    def accumulatorFractionSize(gSize: Int, size: Int) : BigInt = {
        val maxExponentSize = (1 << gSize) - 2
        val fractionSize = BigInt(0).max(size - 1 - gSize - 1 - maxExponentSize)
        2 * (MorrisHEB.minimumExponent(gSize, size)).abs +
        2 * fractionSize
    }
    def accumulatorSize(gSize: Int, size: Int) : BigInt = {
        MorrisHEB.accumulatorFractionSize(gSize, size) + //afs
        2 * MorrisHEB.maximumExponent(gSize, size) + //max exp
        1 +//sign
        1 //overflow
    }
}


class DecoderMorrisHEB(
    gSize: Int,
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
    val minimumInternalFractionSize = MorrisHEB.internalFractionSize(gSize, size)
    val minimumInternalExponentSize = MorrisHEB.internalExponentSize(gSize, size)
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

    val gValue = Wire(UInt(gSize.W))
    gValue := io.binary(size-2, size-1-gSize)
    val exponentSign = Wire(Bool())
    exponentSign := io.binary(size-2-gSize)
    val minusOneExponentSize = Wire(Bool())
    minusOneExponentSize := (gValue === 0.U)
    val exponentSize = Wire(UInt((log2Ceil(minimumInternalExponentSize+1)).W))
    when(minusOneExponentSize) {
        exponentSize := 0.U
    } .otherwise {
        exponentSize := gValue -& 1.U
    }
    val fractionSize = Wire(UInt(log2Ceil(size+1).W))
    fractionSize := Mux(
                            exponentSize >= (size - 2 - gSize).U,
                            0.U,
                            (size - 2 - gSize).U - exponentSize
                        )
    val mantissaBits = Wire(UInt((minimumInternalFractionSize+1).W))
    mantissaBits := Mux(
                                fractionSize === 0.U,
                                0.U,
                                io.binary << (minimumInternalFractionSize.U - fractionSize)
                            ) | (1.U(1.W) ## 0.U(minimumInternalFractionSize.W))
    floatingPoint.mantissa := mantissaBits

    val binaryExponentSize = Wire(UInt(log2Ceil(minimumInternalExponentSize+1).W))
    binaryExponentSize := Mux(
                                exponentSize <= (size - 2 - gSize).U,
                                exponentSize,
                                (size - 2 - gSize).U
                            )
    val binaryExponent = Wire(UInt(minimumInternalExponentSize.W))
    binaryExponent := Mux(minusOneExponentSize,
                            0.U,
                            Mux(
                                binaryExponentSize === 0.U,
                                0.U,
                                ( io.binary(size-3-gSize,0) >> (fractionSize) ) << (exponentSize - binaryExponentSize)
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
        printf("[DecoderMorrisHEB] floatingPoint.sign DEC: %d, floatingPoint.zero DEC: %d, floatingPoint.inf DEC: %d, floatingPoint.nan DEC: %d, floatingPoint.underflow DEC: %d, floatingPoint.overflow DEC: %d, \n",
        floatingPoint.sign, floatingPoint.zero, floatingPoint.inf, floatingPoint.nan, floatingPoint.underflow, floatingPoint.overflow)
        printf("[DecoderMorrisHEB] floatingPoint.exponentSign DEC: %d, floatingPoint.exponentSign HEX %x\n",
        floatingPoint.exponentSign, floatingPoint.exponentSign)
        printf("[DecoderMorrisHEB] floatingPoint.exponentAbsoluteValue DEC: %d, floatingPoint.exponentAbsoluteValue HEX %x\n",
        floatingPoint.exponentAbsoluteValue, floatingPoint.exponentAbsoluteValue)
        printf("[DecoderMorrisHEB] floatingPoint.mantissa DEC: %d, floatingPoint.mantissa HEX %x\n",
        floatingPoint.mantissa, floatingPoint.mantissa)
        printf("[DecoderMorrisHEB] floatingPoint.restBits DEC: %d, floatingPoint.restBits HEX %x\n",
        floatingPoint.restBits, floatingPoint.restBits)
        printf("[DecoderMorrisHEB] gValue DEC: %d, gValue HEX %x\n",
        gValue, gValue)
        printf("[DecoderMorrisHEB] minusOneExponentSize DEC: %d, minusOneExponentSize HEX %x\n",
        minusOneExponentSize, minusOneExponentSize)
        printf("[DecoderMorrisHEB] fractionSize DEC: %d, fractionSize HEX %x\n",
        fractionSize, fractionSize)
        printf("[DecoderMorrisHEB] mantissaBits DEC: %d, mantissaBits HEX %x\n",
        mantissaBits, mantissaBits)
        printf("[DecoderMorrisHEB] exponentSize DEC: %d, exponentSize HEX %x\n",
        exponentSize, exponentSize)
        printf("[DecoderMorrisHEB] binaryExponentSize DEC: %d, binaryExponentSize HEX %x\n",
        binaryExponentSize, binaryExponentSize)
        printf("[DecoderMorrisHEB] binaryExponent DEC: %d, binaryExponent HEX %x\n",
        binaryExponent, binaryExponent)
    }
}


class EncoderMorrisHEB(
    gSize: Int,
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
    val minimumInternalFractionSize = MorrisHEB.internalFractionSize(gSize, size)
    val minimumInternalExponentSize = MorrisHEB.internalExponentSize(gSize, size)
    val minimumExponentValue = (MorrisHEB.minimumExponent(gSize, size)).abs
    val maximumExponentValue = (MorrisHEB.maximumExponent(gSize, size)).abs
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
    if(softwareDebug) printf("[EncoderMorrisHEB] overflow DEC: %d, underflow DEC %d\n", overflow, underflow)

    when(floatingPoint.nan || floatingPoint.inf) {
        io.binary := 1.U(1.W) ## Fill(size-1, 0.U(1.W))
    } .elsewhen(overflow) {
        io.binary := 1.U(1.W) ## Fill(size-1, 0.U(1.W))
    } .elsewhen(floatingPoint.zero) {
        io.binary := Fill(size, 0.U(1.W))
    } .elsewhen(underflow) {
        io.binary := Fill(size, 0.U(1.W))
    } .otherwise {
        val exponentMSBZero = Wire(UInt((log2Ceil(gSize+2)).W))
        exponentMSBZero := PriorityEncoder(Reverse(floatingPoint.exponentAbsoluteValue))
        val exponentSize = Wire(UInt((gSize+1).W))
        val minusOneExponentSize = Wire(Bool())
        minusOneExponentSize := floatingPoint.exponentAbsoluteValue === 0.U
        exponentSize := minimumInternalExponentSize.U -& exponentMSBZero -& 1.U
        val gValue = Wire(UInt(gSize.W))
        gValue := exponentSize +& 1.U
        when(minusOneExponentSize) {
            exponentSize := 0.U
            gValue := 0.U
        }
        val fractionSize = Wire(UInt(log2Ceil(size+1).W))
        fractionSize := Mux(
                                exponentSize >= (size - 2 - gSize).U,
                                0.U,
                                (size - 2 - gSize).U - exponentSize
                            )
        val mantissaBits = Wire(UInt(minimumInternalFractionSize.W))
        mantissaBits := floatingPoint.mantissa(minimumInternalFractionSize-1, 0) >> (minimumInternalFractionSize.U - fractionSize)
        val binaryExponentSize = Wire(UInt(log2Ceil(size).W))
        binaryExponentSize := Mux(
                                    exponentSize <= (size - 2 - gSize).U,
                                    exponentSize,
                                    (size - 2 - gSize).U
                                )
        val exponentBits = Wire(UInt((size-1).W))
        // delete MSB & ~(1.U << binaryExponentSize)
        exponentBits := ((floatingPoint.exponentAbsoluteValue >> (exponentSize - binaryExponentSize)) & ~(1.U << binaryExponentSize)) << fractionSize
        val partialBinary = Wire(UInt(size.W))
        partialBinary := (floatingPoint.sign ## gValue ## floatingPoint.exponentSign ## Fill(size-2-gSize, 0.U(1.W))) | exponentBits | mantissaBits
        
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
        io.binary := partialBinary +& roundingModule.io.addOne

        val exponent = Wire(UInt(minimumInternalExponentSize.W))
        exponent := floatingPoint.exponentAbsoluteValue

        when(minimumInternalFractionSize.U > fractionSize) {//ussualy happens
            when( (exponentSize - binaryExponentSize) >= 2.U) {
                //when g and r are exponent bits 
                when(exponentSize >= 2.U) {
                    roundingModule.io.g := (exponent >> (exponentSize - 1.U - binaryExponentSize))(0)
                    roundingModule.io.r := (exponent >> (exponentSize - 2.U - binaryExponentSize))(0)
                    // calcualte s from the other exponent bits, mantisa bits and any other rest bits
                    //s := (exponent & (Fill(exponentSize-2, 1.U(1.W)) >> binaryExponentSize)).orR || floatingPoint.mantissa(minimumInternalFractionSize-1, 0).orR || floatingPoint.restBits.orR
                    when(exponentSize > 2.U) {
                        roundingModule.io.s := !((exponent & ((1.U<<((exponentSize - 2.U - binaryExponentSize)))-1.U)) === 0.U) ||
                                                floatingPoint.mantissa(minimumInternalFractionSize-1, 0).orR || floatingPoint.restBits.orR
                    } .otherwise {
                        roundingModule.io.s := floatingPoint.mantissa(minimumInternalFractionSize-1, 0).orR || floatingPoint.restBits.orR
                    }
                } .otherwise {
                    //case imposible
                    roundingModule.io.g := false.B
                    roundingModule.io.r := false.B
                    roundingModule.io.s := false.B
                    printf("[EncoderMorrisHEB][ERROR] binaryExponentSize DEC: %d, binaryExponentSize HEX %x\n", binaryExponentSize, binaryExponentSize)
                }
            } .elsewhen( (exponentSize - binaryExponentSize) === 1.U) {
                // when g is exponent bits and rest are fraction
                when(exponentSize >= 1.U) {
                    roundingModule.io.g := exponent(0)
                } .otherwise {
                    //case imposible
                    roundingModule.io.g := false.B
                    printf("[EncoderMorrisHEB][ERROR] binaryExponentSize DEC: %d, binaryExponentSize HEX %x\n", binaryExponentSize, binaryExponentSize)
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
                    printf("[EncoderMorrisHEB][ERROR] fractionSize DEC: %d, fractionSize HEX %x\n", fractionSize, fractionSize)
                }
            }

        
            if(softwareDebug) {
                printf("[EncoderMorrisHEB] floatingPoint.exponentSign DEC: %d, floatingPoint.exponentSign HEX %x\n", floatingPoint.exponentSign, floatingPoint.exponentSign)
                printf("[EncoderMorrisHEB] floatingPoint.exponentAbsoluteValue DEC: %d, floatingPoint.exponentAbsoluteValue HEX %x\n", floatingPoint.exponentAbsoluteValue, floatingPoint.exponentAbsoluteValue)
                printf("[EncoderMorrisHEB] exponentBits DEC: %d, exponentBits HEX %x\n", exponentBits, exponentBits)
            }
        }
    }
    if(softwareDebug) printf("[EncoderMorrisHEB] io.binary DEC: %d, io.binary HEX %x\n", io.binary, io.binary)
}