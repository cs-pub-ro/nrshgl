package ro.upb.nrs.hgl

import chisel3._
import chisel3.util._

case class Morris(
    gSize: Int,
    size: Int,
    rounding : RoundingType,
    softwareDebug : Boolean
) extends Bundle with HNumberRepresentationSystem[Morris] with FloatingPointTrait {
    require(gSize >= 1)
    require(size >= 3)
    require((size-gSize) >= 3)

    val internalExponentSize = Morris.internalExponentSize(gSize, size)
    val internalFractionSize = Morris.internalFractionSize(gSize, size)
	val value : UInt = Output(UInt((size).W))

    def floatingPointValue : FloatingPoint = {
		val decoder = Module(
            new DecoderMorris(
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

	override def +(that: Morris): Morris = {
		val encode = Module(
            new EncoderMorris(
                gSize,
                size,
                rounding,
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue + that.floatingPointValue
		val out = Wire(Morris(gSize, size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def -(that: Morris): Morris = {
		val encode = Module(
            new EncoderMorris(
                gSize,
                size,
                rounding,
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue - that.floatingPointValue
		val out = Wire(Morris(gSize, size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def *(that: Morris): Morris = {
		val encode = Module(
            new EncoderMorris(
                gSize,
                size,
                rounding,
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue * that.floatingPointValue
		val out = Wire(Morris(gSize, size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def /(that: Morris): Morris = {
		val encode = Module(
            new EncoderMorris(
                gSize,
                size,
                rounding,
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue / that.floatingPointValue
		val out = Wire(Morris(gSize, size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}


	override def sqrt: Morris = {
		val encode = Module(
            new EncoderMorris(
                gSize,
                size,
                rounding,
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue.sqrt
		val out = Wire(Morris(gSize, size, rounding, softwareDebug))
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
        new DecoderMorris(
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
        new EncoderMorris(
            this.gSize,
            floatingPointSize,
            this.rounding,
            floatingPointExponentSize,
            floatingPointFractionSize,
            softwareDebug
        )
    }
    
    def getOptimalAccumulatorSize : BigInt = Morris.accumulatorSize(gSize, size)
    def getOptimalAccumulatorFractionSize : BigInt = Morris.accumulatorFractionSize(gSize, size)
}

object Morris {
	def apply(
        gSize: Int,
        size: Int,
        rounding : RoundingType = RoundZero,
        softwareDebug : Boolean = false
    ) : Morris = new Morris(gSize, size, rounding, softwareDebug)
    def minimumExponent(gSize: Int, size: Int) : BigInt = -Morris.maximumExponent(gSize, size)
    def maximumExponent(gSize: Int, size: Int) : BigInt = {
        val maxExponentSize = 1 << gSize // G=g_value+1 , max g_value = 2^g_size - 1
        val remainingBitsSize = size - 1 - gSize - 1 // fraction sign, g_size and exponent sign
        if(maxExponentSize <= remainingBitsSize) {
            ((BigInt(1)<<maxExponentSize) - 1)
        } else {
            ( ((BigInt(1)<<remainingBitsSize) - 1) << (maxExponentSize - remainingBitsSize) )
        }
        
    }
    def internalExponentSize(gSize: Int, size: Int) : Int = {
        val maximumExponentValue = Morris.maximumExponent(gSize, size)
        log2Ceil(maximumExponentValue + 1)
    }
    def accumulatorFractionSize(gSize: Int, size: Int) : BigInt = {
        val maxExponentSize = 1 << gSize
        val fractionSize = BigInt(0).max(size - 1 - gSize - 1 - maxExponentSize)
        2 * (Morris.minimumExponent(gSize, size)).abs +
        2 * fractionSize
    }
    def accumulatorSize(gSize: Int, size: Int) : BigInt = {
        Morris.accumulatorFractionSize(gSize, size) + //afs
        2 * Morris.maximumExponent(gSize, size) + //max exp
        1 +//sign
        1 //overflow
    }
    
    def internalFractionSize(gSize: Int, size: Int) : Int = size-3-gSize //1 sign 1 exponent sign 1 from (g+1 exponent size)
}


class DecoderMorris(
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
    val minimumInternalFractionSize = Morris.internalFractionSize(gSize, size)
    val minimumInternalExponentSize = Morris.internalExponentSize(gSize, size)
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
    val exponentSize = Wire(UInt((gSize+1).W))
    exponentSize := gValue +& 1.U
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

    val binaryExponentSize = Wire(UInt(log2Ceil(size).W))
    binaryExponentSize := Mux(
                                exponentSize <= (size - 2 - gSize).U,
                                exponentSize,
                                (size - 2 - gSize).U
                            )

    val binaryExponent = Wire(UInt(Morris.internalExponentSize(gSize, size).W))
    binaryExponent := Mux(
                            binaryExponentSize === 0.U,
                            0.U,
                            ( io.binary(size-3-gSize,0) >> (fractionSize) ) << (exponentSize - binaryExponentSize)
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
        printf("[DecoderMorris] floatingPoint.sign DEC: %d, floatingPoint.zero DEC: %d, floatingPoint.inf DEC: %d, floatingPoint.nan DEC: %d, floatingPoint.underflow DEC: %d, floatingPoint.overflow DEC: %d, \n",
        floatingPoint.sign, floatingPoint.zero, floatingPoint.inf, floatingPoint.nan, floatingPoint.underflow, floatingPoint.overflow)
        printf("[DecoderMorris] floatingPoint.exponentSign DEC: %d, floatingPoint.exponentSign HEX %x\n",
        floatingPoint.exponentSign, floatingPoint.exponentSign)
        printf("[DecoderMorris] floatingPoint.exponentAbsoluteValue DEC: %d, floatingPoint.exponentAbsoluteValue HEX %x\n",
        floatingPoint.exponentAbsoluteValue, floatingPoint.exponentAbsoluteValue)
        printf("[DecoderMorris] floatingPoint.mantissa DEC: %d, floatingPoint.mantissa HEX %x\n",
        floatingPoint.mantissa, floatingPoint.mantissa)
        printf("[DecoderMorris] floatingPoint.restBits DEC: %d, floatingPoint.restBits HEX %x\n",
        floatingPoint.restBits, floatingPoint.restBits)
        printf("[DecoderMorris] fractionSize DEC: %d, fractionSize HEX %x\n",
        fractionSize, fractionSize)
        printf("[DecoderMorris] mantissaBits DEC: %d, mantissaBits HEX %x\n",
        mantissaBits, mantissaBits)
        printf("[DecoderMorris] binaryExponentSize DEC: %d, binaryExponentSize HEX %x\n",
        binaryExponentSize, binaryExponentSize)
        printf("[DecoderMorris] binaryExponent DEC: %d, binaryExponent HEX %x\n",
        binaryExponent, binaryExponent)
    }
}


class EncoderMorris(
    gSize: Int,
    size: Int,
    rounding : RoundingType,
    internalExponentSize : Int,
    internalFractionSize : Int,
    softwareDebug : Boolean = false
    ) extends EncoderFloatingPoint(
        internalExponentSize,
        internalFractionSize,
        size,
        softwareDebug
    ) {
    val minimumInternalFractionSize = Morris.internalFractionSize(gSize, size)
    val minimumInternalExponentSize = Morris.internalExponentSize(gSize, size)
    val minimumExponentValue = (Morris.minimumExponent(gSize, size)).abs
    val maximumExponentValue = (Morris.maximumExponent(gSize, size)).abs
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
    underflow := (floatingPoint.exponentSign & (floatingPoint.exponentAbsoluteValue > minimumExponentValue.U)) ||
                floatingPoint.underflow
    if(softwareDebug) printf("[EncoderMorris] overflow DEC: %d, underflow DEC %d\n", overflow, underflow)

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
        exponentSize := minimumInternalExponentSize.U - exponentMSBZero
        val gValue = Wire(UInt(gSize.W))
        gValue := exponentSize -& 1.U
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
        exponentBits := (floatingPoint.exponentAbsoluteValue >> (exponentSize - binaryExponentSize)) << fractionSize
        val partialBinary = Wire(UInt(size.W))
        partialBinary := (floatingPoint.sign ## gValue ## floatingPoint.exponentSign ## Fill(size-2-gSize, 0.U(1.W))) | exponentBits | mantissaBits
        
        //rounding
		val roundingModule = Module(new FloatingPointRounding(rounding, softwareDebug))
        roundingModule.io.sign := floatingPoint.sign
        roundingModule.io.l := partialBinary(0)
        roundingModule.io.g := floatingPoint.restBits(2)
        roundingModule.io.r := floatingPoint.restBits(1)
        roundingModule.io.s := floatingPoint.restBits(0)
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
                        roundingModule.io.s := floatingPoint.mantissa(minimumInternalFractionSize-1, 0).orR ||
                        floatingPoint.restBits.orR
                    }
                } .otherwise {
                    //case imposible
                    roundingModule.io.g := false.B
                    roundingModule.io.r := false.B
                    roundingModule.io.s := false.B
                    printf("[EncoderMorris][ERROR] binaryExponentSize DEC: %d, binaryExponentSize HEX %x\n", binaryExponentSize, binaryExponentSize)
                }
            } .elsewhen( (exponentSize - binaryExponentSize) === 1.U) {
                // when g is exponent bits and rest are fraction
                when(exponentSize >= 1.U) {
                    roundingModule.io.g := exponent(0)
                } .otherwise {
                    //case imposible
                    roundingModule.io.g := false.B
                    printf("[EncoderMorris][ERROR] binaryExponentSize DEC: %d, binaryExponentSize HEX %x\n", binaryExponentSize, binaryExponentSize)
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
                    printf("[EncoderMorris][ERROR] fractionSize DEC: %d, fractionSize HEX %x\n", fractionSize, fractionSize)
                }
            }
        }
        
        if(softwareDebug) {
            printf("[EncoderMorris] floatingPoint.exponentSign DEC: %d, floatingPoint.exponentSign HEX %x\n", floatingPoint.exponentSign, floatingPoint.exponentSign)
            printf("[EncoderMorris] floatingPoint.exponentAbsoluteValue DEC: %d, floatingPoint.exponentAbsoluteValue HEX %x\n", floatingPoint.exponentAbsoluteValue, floatingPoint.exponentAbsoluteValue)
            printf("[EncoderMorris] exponentBits DEC: %d, exponentBits HEX %x\n", exponentBits, exponentBits)
        }

    }
    if(softwareDebug) printf("[EncoderMorris] io.binary DEC: %d, io.binary HEX %x\n", io.binary, io.binary)
}