package ro.upb.nrs.hgl

import chisel3._
import chisel3.util._

case class IEEE754(
    exponentSize: Int,
    fractionSize: Int,
    rounding : RoundingType,
    softwareDebug : Boolean
) extends Bundle with HNumberRepresentationSystem[IEEE754] with FloatingPointTrait {
    require(exponentSize >= 2)
    require(fractionSize >= 1)

    val size = exponentSize + fractionSize + 1
    val internalExponentSize = IEEE754.internalExponentSize(exponentSize, fractionSize)
    val internalFractionSize = IEEE754.internalFractionSize(exponentSize, fractionSize)
	val value : UInt = Output(UInt(size.W))

    def floatingPointValue : FloatingPoint = {
		val decoder = Module(
            new DecoderIEEE754(
                exponentSize,
                fractionSize,
                internalExponentSize,
                internalFractionSize,
                size,
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

	override def +(that: IEEE754): IEEE754 = {
		val encode = Module(
            new EncoderIEEE754(
                exponentSize,
                fractionSize,
                rounding,
                internalExponentSize,
                internalFractionSize,
                size,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue + that.floatingPointValue
		val out = Wire(IEEE754(exponentSize, fractionSize, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def -(that: IEEE754): IEEE754 = {
		val encode = Module(
            new EncoderIEEE754(
                exponentSize,
                fractionSize,
                rounding,
                internalExponentSize,
                internalFractionSize,
                size,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue - that.floatingPointValue
		val out = Wire(IEEE754(exponentSize, fractionSize, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def *(that: IEEE754): IEEE754 = {
		val encode = Module(
            new EncoderIEEE754(
                exponentSize,
                fractionSize,
                rounding,
                internalExponentSize,
                internalFractionSize,
                size,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue * that.floatingPointValue
		val out = Wire(IEEE754(exponentSize, fractionSize, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def /(that: IEEE754): IEEE754 = {
		val encode = Module(
            new EncoderIEEE754(
                exponentSize,
                fractionSize,
                rounding,
                internalExponentSize,
                internalFractionSize,
                size,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue / that.floatingPointValue
		val out = Wire(IEEE754(exponentSize, fractionSize, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

    
	override def sqrt: IEEE754 = {
		val encode = Module(
            new EncoderIEEE754(
                exponentSize,
                fractionSize,
                rounding,
                internalExponentSize,
                internalFractionSize,
                size,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue.sqrt
		val out = Wire(IEEE754(exponentSize, fractionSize, rounding, softwareDebug))
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
        new DecoderIEEE754(
            this.exponentSize,
            this.fractionSize,
            floatingPointExponentSize,
            floatingPointFractionSize,
            floatingPointSize,
            softwareDebug
        )
    }
    def getEncoderFloatingPoint(
        floatingPointExponentSize : Int,
        floatingPointFractionSize : Int,
        floatingPointSize : Int,
        softwareDebug : Boolean
    ) : EncoderFloatingPoint = {
        new EncoderIEEE754(
            this.exponentSize,
            this.fractionSize,
            this.rounding,
            floatingPointExponentSize,
            floatingPointFractionSize,
            floatingPointSize,
            softwareDebug
        )
    }
    
    def getOptimalAccumulatorSize : BigInt = IEEE754.accumulatorSize(exponentSize, fractionSize)
    def getOptimalAccumulatorFractionSize : BigInt = IEEE754.accumulatorFractionSize(exponentSize, fractionSize)
}

object IEEE754 {
	def apply(exponentSize: Int,
                fractionSize: Int,
                rounding : RoundingType = RoundEven,
                softwareDebug : Boolean = false
            ) : IEEE754 = new IEEE754(exponentSize, fractionSize, rounding, softwareDebug)
    def bias(exponentSize: Int) : BigInt = (BigInt(1) << (exponentSize - 1)) - 1
    def subnormalExponent(exponentSize: Int) : BigInt = -IEEE754.bias(exponentSize) + 1
    def minimumExponent(exponentSize: Int, fractionSize: Int) : BigInt = IEEE754.subnormalExponent(exponentSize) - fractionSize - 1
    def maximumExponent(exponentSize: Int, fractionSize: Int) : BigInt = IEEE754.bias(exponentSize) + 1
    def internalExponentSize(exponentSize: Int, fractionSize: Int) : Int = {
        // subnormalExponent - fracton_size is the minimum possible exponent
        // -1 for the number between 0 and the minimum value
        val maximumExponentAbsoluteValue = IEEE754.minimumExponent(exponentSize, fractionSize).abs.max(
                                            IEEE754.maximumExponent(exponentSize, fractionSize))
        log2Ceil(maximumExponentAbsoluteValue + 1)
    }
    def accumulatorFractionSize(exponentSize: Int, fractionSize: Int) : BigInt = {
        2 * (IEEE754.minimumExponent(exponentSize, fractionSize)+1).abs
    }
    def accumulatorSize(exponentSize: Int, fractionSize: Int) : BigInt = {
        IEEE754.accumulatorFractionSize(exponentSize, fractionSize) + //afs
        2 * (IEEE754.maximumExponent(exponentSize, fractionSize)-1) + //max exp for possible 1 minus 1
        1 +//sign
        1 //overflow
    }
    def internalFractionSize(exponentSize: Int, fractionSize: Int) : Int = fractionSize
}


class DecoderIEEE754(
    exponentSize : Int,
    fractionSize : Int,
    internalExponentSize : Int,
    internalFractionSize : Int,
    size : Int,
    softwareDebug : Boolean = false
    ) extends DecoderFloatingPoint(
        internalExponentSize,
        internalFractionSize,
        size,
        softwareDebug
    ) {
    val minimumInternalFractionSize = IEEE754.internalFractionSize(exponentSize, fractionSize)
    val minimumInternalExponentSize = IEEE754.internalExponentSize(exponentSize, fractionSize)
    val floatingPoint = Wire(new FloatingPoint(
            minimumInternalExponentSize,
            minimumInternalFractionSize,
            softwareDebug
        )
    )
    floatingPoint.sign := io.binary(size-1)
    floatingPoint.zero := io.binary( (size - 2), 0) === Fill(size-1, 0.U(1.W))
    floatingPoint.underflow := false.B
    floatingPoint.overflow := false.B
    floatingPoint.restBits := 0.U(3.W)
    val exponentBits = Wire(UInt(exponentSize.W))
    exponentBits := io.binary(exponentSize + fractionSize, fractionSize)
    val mantissaBits = Wire(UInt(fractionSize.W))
    mantissaBits := io.binary(fractionSize - 1, 0)
    floatingPoint.inf := (exponentBits === Fill(exponentSize, 1.U(1.W))) && (mantissaBits === Fill(fractionSize, 0.U(1.W)))
    floatingPoint.nan := (exponentBits === Fill(exponentSize, 1.U(1.W))) && (mantissaBits =/= Fill(fractionSize, 0.U(1.W)))
    val exponentSign = Wire(Bool())
    val bias = IEEE754.bias(exponentSize)
    exponentSign := exponentBits < bias.U
    val subnormal = Wire(Bool())
    subnormal := (exponentBits === Fill(exponentSize, 0.U(1.W)) && !floatingPoint.zero)
    val exponentAbsoluteValue = Wire(UInt(minimumInternalExponentSize.W))
    floatingPoint.exponentSign := exponentSign
    floatingPoint.exponentAbsoluteValue := exponentAbsoluteValue
    val mantissa = Wire(UInt((fractionSize + 1).W))
    mantissa := 1.U(1.W) ## mantissaBits
    val subnormalExponentAbsoluteValue = Wire(UInt(minimumInternalExponentSize.W))
    subnormalExponentAbsoluteValue := (-IEEE754.subnormalExponent(exponentSize)).U
    when(subnormal) {
        val shiftLeft = Wire(UInt((log2Ceil(fractionSize) + 1).W))
        shiftLeft := PriorityEncoder(Reverse(0.U(1.W) ## mantissaBits))
        exponentAbsoluteValue := subnormalExponentAbsoluteValue +& shiftLeft
        floatingPoint.exponentSign := true.B
        mantissa := mantissaBits << shiftLeft
    } .otherwise {
        exponentAbsoluteValue := Mux(exponentSign, bias.U - exponentBits, exponentBits - bias.U)
    }
    floatingPoint.mantissa := mantissa

    
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
        printf("[DecoderIEEE754] floatingPoint.sign DEC: %d, floatingPoint.zero DEC: %d, floatingPoint.inf DEC: %d, floatingPoint.nan DEC: %d, floatingPoint.underflow DEC: %d, floatingPoint.overflow DEC: %d, \n",
        floatingPoint.sign, floatingPoint.zero, floatingPoint.inf, floatingPoint.nan, floatingPoint.underflow, floatingPoint.overflow)
        printf("[DecoderIEEE754] floatingPoint.exponentSign DEC: %d, floatingPoint.exponentSign HEX %x\n",
        floatingPoint.exponentSign, floatingPoint.exponentSign)
        printf("[DecoderIEEE754] floatingPoint.exponentAbsoluteValue DEC: %d, floatingPoint.exponentAbsoluteValue HEX %x\n",
        floatingPoint.exponentAbsoluteValue, floatingPoint.exponentAbsoluteValue)
        printf("[DecoderIEEE754] floatingPoint.mantissa DEC: %d, floatingPoint.mantissa HEX %x\n",
        floatingPoint.mantissa, floatingPoint.mantissa)
        printf("[DecoderIEEE754] floatingPoint.restBits DEC: %d, floatingPoint.restBits HEX %x\n",
        floatingPoint.restBits, floatingPoint.restBits)
        printf("[DecoderIEEE754] exponentBits DEC: %d, exponentBits HEX %x\n",
        exponentBits, exponentBits)
        printf("[DecoderIEEE754] mantissaBits DEC: %d, mantissaBits HEX %x\n",
        mantissaBits, mantissaBits)
    }
}


class EncoderIEEE754(
    exponentSize: Int,
    fractionSize: Int,
    rounding : RoundingType,
    internalExponentSize : Int,
    internalFractionSize : Int,
    size : Int,
    softwareDebug : Boolean = false
    ) extends EncoderFloatingPoint(
        internalExponentSize,
        internalFractionSize,
        size,
        softwareDebug
    ) {
    val minimumInternalFractionSize = IEEE754.internalFractionSize(exponentSize, fractionSize)
    val minimumInternalExponentSize = IEEE754.internalExponentSize(exponentSize, fractionSize)
    val bias = IEEE754.bias(exponentSize)
    val minimumExponentValue = (IEEE754.minimumExponent(exponentSize, fractionSize)).abs
    val maximumExponentValue = (IEEE754.maximumExponent(exponentSize, fractionSize)).abs
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
    overflow := (!floatingPoint.exponentSign & (floatingPoint.exponentAbsoluteValue >= maximumExponentValue.U)) ||
                floatingPoint.overflow
    val underflow = Wire(Bool())
    underflow := (floatingPoint.exponentSign & (floatingPoint.exponentAbsoluteValue > minimumExponentValue.U)) || floatingPoint.underflow
    if(softwareDebug) printf("[EncoderIEEE754] overflow DEC: %d, underflow DEC %d\n", overflow, underflow)
    val subnormal = Wire(Bool())
    subnormal := (floatingPoint.exponentSign & (floatingPoint.exponentAbsoluteValue > ((IEEE754.subnormalExponent(exponentSize)).abs).U))

    when(floatingPoint.nan) {
        io.binary := 1.U(1.W) ## Fill(exponentSize, 1.U(1.W)) ## 1.U(1.W) ## Fill(fractionSize-1, 0.U(1.W))
    } .elsewhen(floatingPoint.inf || overflow) {
        io.binary := floatingPoint.sign ## Fill(exponentSize, 1.U(1.W)) ## Fill(fractionSize, 0.U(1.W))
    } .elsewhen(floatingPoint.zero || underflow) {
        io.binary := floatingPoint.sign ## Fill(size-1, 0.U(1.W))
    } .otherwise {
        val exponentBits = Wire(UInt(exponentSize.W))
        exponentBits := Mux(
            floatingPoint.exponentSign,
            bias.U -& floatingPoint.exponentAbsoluteValue,
            floatingPoint.exponentAbsoluteValue +& bias.U
        )
        if(softwareDebug) {
            printf("[EncoderIEEE754] floatingPoint.exponentSign DEC: %d, floatingPoint.exponentSign HEX %x\n", floatingPoint.exponentSign, floatingPoint.exponentSign)
            printf("[EncoderIEEE754] floatingPoint.exponentAbsoluteValue DEC: %d, floatingPoint.exponentAbsoluteValue HEX %x\n", floatingPoint.exponentAbsoluteValue, floatingPoint.exponentAbsoluteValue)
            printf("[EncoderIEEE754] exponentBits DEC: %d, exponentBits HEX %x\n", exponentBits, exponentBits)
        }
        val partialBinary = Wire(UInt(size.W))
        partialBinary := floatingPoint.sign ## exponentBits ## floatingPoint.mantissa(fractionSize-1, 0)
        if(softwareDebug) printf("[EncoderIEEE754] partialBinary DEC: %d, partialBinary HEX %x\n", partialBinary, partialBinary)
        
        //rounding
		val roundingModule = Module(new FloatingPointRounding(rounding, softwareDebug))
        roundingModule.io.sign := floatingPoint.sign
        roundingModule.io.l := partialBinary(0)
        roundingModule.io.g := floatingPoint.restBits(2)
        roundingModule.io.r := floatingPoint.restBits(1)
        roundingModule.io.s := floatingPoint.restBits(0)
        io.binary := partialBinary +& roundingModule.io.addOne

        /*
        special subnormal case
        */
        when(subnormal) {
            exponentBits := Fill(exponentSize, 0.U(1.W))
            val mantissaBits = Wire(UInt(fractionSize.W))
            val shiftRight = Wire(UInt((log2Ceil(fractionSize) + 1).W))
            shiftRight := floatingPoint.exponentAbsoluteValue -& (IEEE754.subnormalExponent(exponentSize).abs).U
            mantissaBits := (floatingPoint.mantissa >> shiftRight)(fractionSize-1, 0)
            partialBinary := floatingPoint.sign ## exponentBits ## mantissaBits
            when(shiftRight === 1.U) {
                roundingModule.io.g := floatingPoint.mantissa(0)
                roundingModule.io.r := floatingPoint.restBits(2)
                roundingModule.io.s := floatingPoint.restBits(1) | floatingPoint.restBits(0)
            } .elsewhen(shiftRight === 2.U) {
                roundingModule.io.g := floatingPoint.mantissa(1)
                roundingModule.io.r := floatingPoint.mantissa(0)
                roundingModule.io.s := floatingPoint.restBits.orR
            } .elsewhen(shiftRight === 3.U) {
                roundingModule.io.g := floatingPoint.mantissa(2)
                roundingModule.io.r := floatingPoint.mantissa(1)
                roundingModule.io.s := floatingPoint.mantissa(0) | floatingPoint.restBits.orR
            } .elsewhen(shiftRight > 3.U) {
                roundingModule.io.g := (floatingPoint.mantissa >> (shiftRight - 1.U))(0)
                roundingModule.io.r := (floatingPoint.mantissa >> (shiftRight - 2.U))(0)
                roundingModule.io.s := (floatingPoint.mantissa & ((1.U << (shiftRight - 2.U))-1.U)).orR | floatingPoint.restBits.orR
            } .otherwise {
                printf("[EncoderIEEE754][ERROR] shiftRight DEC: %d, shiftRight HEX %x\n", shiftRight, shiftRight)
            }
            
            if(softwareDebug) {
                printf("[EncoderIEEE754] shiftRight DEC: %d, shiftRight HEX %x\n", shiftRight, shiftRight)
                printf("[EncoderIEEE754] mantissaBits DEC: %d, mantissaBits HEX %x\n", mantissaBits, mantissaBits)
            }
        }
    }
    if(softwareDebug) printf("[EncoderIEEE754] io.binary DEC: %d, io.binary HEX %x\n", io.binary, io.binary)
}