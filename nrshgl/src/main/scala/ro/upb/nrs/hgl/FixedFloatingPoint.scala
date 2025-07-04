package ro.upb.nrs.hgl

import chisel3._
import chisel3.util._

case class FixedFloatingPoint(
    exponentSize: Int,
    fractionSize: Int,
    rounding : RoundingType,
    softwareDebug : Boolean
) extends Bundle with HNumberRepresentationSystem[FixedFloatingPoint] with FloatingPointTrait {
    require(exponentSize >= 2)
    require(fractionSize >= 1)

    val size = exponentSize + fractionSize + 1
    val internalExponentSize = FixedFloatingPoint.internalExponentSize(exponentSize, fractionSize)
    val internalFractionSize = FixedFloatingPoint.internalFractionSize(exponentSize, fractionSize)
	val value : UInt = Output(UInt(size.W))

    def floatingPointValue : FloatingPoint = {
		val decoder = Module(
            new DecoderFixedFloatingPoint(
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

	override def +(that: FixedFloatingPoint): FixedFloatingPoint = {
		val encode = Module(
            new EncoderFixedFloatingPoint(
                exponentSize,
                fractionSize,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                size,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue + that.floatingPointValue
		val out = Wire(FixedFloatingPoint(exponentSize, fractionSize, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def -(that: FixedFloatingPoint): FixedFloatingPoint = {
		val encode = Module(
            new EncoderFixedFloatingPoint(
                exponentSize,
                fractionSize,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                size,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue - that.floatingPointValue
		val out = Wire(FixedFloatingPoint(exponentSize, fractionSize, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def *(that: FixedFloatingPoint): FixedFloatingPoint = {
		val encode = Module(
            new EncoderFixedFloatingPoint(
                exponentSize,
                fractionSize,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                size,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue * that.floatingPointValue
		val out = Wire(FixedFloatingPoint(exponentSize, fractionSize, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def /(that: FixedFloatingPoint): FixedFloatingPoint = {
		val encode = Module(
            new EncoderFixedFloatingPoint(
                exponentSize,
                fractionSize,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                size,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue / that.floatingPointValue
		val out = Wire(FixedFloatingPoint(exponentSize, fractionSize, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}
    
	override def sqrt: FixedFloatingPoint = {
		val encode = Module(
            new EncoderFixedFloatingPoint(
                exponentSize,
                fractionSize,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                size,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue.sqrt
		val out = Wire(FixedFloatingPoint(exponentSize, fractionSize, rounding, softwareDebug))
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
        new DecoderFixedFloatingPoint(
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
        new EncoderFixedFloatingPoint(
            this.exponentSize,
            this.fractionSize,
            Some(this.rounding),
            floatingPointExponentSize,
            floatingPointFractionSize,
            floatingPointSize,
            softwareDebug
        )
    }
    
    def getOptimalAccumulatorSize : BigInt = FixedFloatingPoint.accumulatorSize(exponentSize, fractionSize)
    def getOptimalAccumulatorFractionSize : BigInt = FixedFloatingPoint.accumulatorFractionSize(exponentSize, fractionSize)
}

object FixedFloatingPoint {
	def apply(exponentSize: Int,
                fractionSize: Int,
                rounding : RoundingType = RoundEven,
                softwareDebug : Boolean = false
            ) : FixedFloatingPoint = {
        new FixedFloatingPoint(exponentSize, fractionSize, rounding, softwareDebug)
    }
    def bias(exponentSize: Int) : BigInt = (BigInt(1) << (exponentSize - 1)) - 1
    def minimumExponent(exponentSize: Int, fractionSize: Int) : BigInt = -FixedFloatingPoint.bias(exponentSize)
    def maximumExponent(exponentSize: Int, fractionSize: Int) : BigInt = FixedFloatingPoint.bias(exponentSize) + 1
    def internalExponentSize(exponentSize: Int, fractionSize: Int) : Int = {
        val maximumExponentAbsoluteValue = FixedFloatingPoint.minimumExponent(exponentSize, fractionSize).abs.max(
                                            FixedFloatingPoint.maximumExponent(exponentSize, fractionSize))
        log2Ceil(maximumExponentAbsoluteValue.abs + 1)
    }
    def accumulatorFractionSize(exponentSize: Int, fractionSize: Int) : BigInt = {
        2 * FixedFloatingPoint.minimumExponent(exponentSize, fractionSize).abs + 2 * fractionSize
    }
    def accumulatorSize(exponentSize: Int, fractionSize: Int) : BigInt = {
        FixedFloatingPoint.accumulatorFractionSize(exponentSize, fractionSize) + //afs
        2 * FixedFloatingPoint.maximumExponent(exponentSize, fractionSize) + //max exp
        1 + //sign
        1 //overflow
    }
    def internalFractionSize(exponentSize: Int, fractionSize: Int) : Int = fractionSize
}


class DecoderFixedFloatingPoint(
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
    require(internalFractionSize >= fractionSize)
    val minimumInternalFractionSize = FixedFloatingPoint.internalFractionSize(exponentSize, fractionSize)
    val minimumInternalExponentSize = FixedFloatingPoint.internalExponentSize(exponentSize, fractionSize)
    val floatingPoint = Wire(new FloatingPoint(
            minimumInternalExponentSize,
            minimumInternalFractionSize,
            softwareDebug
        )
    )

    floatingPoint.sign := io.binary(size-1)
    floatingPoint.zero := io.binary( (size - 2), 0) === Fill(size-1, 0.U(1.W))
    floatingPoint.inf := io.binary( (size - 2), 0) === Fill(size-1, 1.U(1.W))
    //does not have nan in binary is more an error
    floatingPoint.nan := false.B
    floatingPoint.underflow := false.B
    floatingPoint.overflow := false.B
    floatingPoint.restBits := 0.U(3.W)
    val exponentBits = Wire(UInt(exponentSize.W))
    exponentBits := io.binary(exponentSize + fractionSize, fractionSize)
    val exponentSign = Wire(Bool())
    val bias = FixedFloatingPoint.bias(exponentSize)
    exponentSign := exponentBits < bias.U
    val exponentAbsoluteValue = Wire(UInt(minimumInternalExponentSize.W))
    exponentAbsoluteValue := Mux(exponentSign, bias.U - exponentBits, exponentBits - bias.U)
    floatingPoint.exponentSign := exponentSign
    floatingPoint.exponentAbsoluteValue := exponentAbsoluteValue
    val mantissaBits = Wire(UInt(fractionSize.W))
    mantissaBits := io.binary(fractionSize - 1, 0)
    floatingPoint.mantissa := 1.U(1.W) ## mantissaBits

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
        printf("[DecoderFixedFloatingPoint] floatingPoint.sign DEC: %d, floatingPoint.zero DEC: %d, floatingPoint.inf DEC: %d, floatingPoint.nan DEC: %d, floatingPoint.underflow DEC: %d, floatingPoint.overflow DEC: %d, \n",
        floatingPoint.sign, floatingPoint.zero, floatingPoint.inf, floatingPoint.nan, floatingPoint.underflow, floatingPoint.overflow)
        printf("[DecoderFixedFloatingPoint] floatingPoint.exponentSign DEC: %d, floatingPoint.exponentSign HEX %x\n",
        floatingPoint.exponentSign, floatingPoint.exponentSign)
        printf("[DecoderFixedFloatingPoint] floatingPoint.exponentAbsoluteValue DEC: %d, floatingPoint.exponentAbsoluteValue HEX %x\n",
        floatingPoint.exponentAbsoluteValue, floatingPoint.exponentAbsoluteValue)
        printf("[DecoderFixedFloatingPoint] floatingPoint.mantissa DEC: %d, floatingPoint.mantissa HEX %x\n",
        floatingPoint.mantissa, floatingPoint.mantissa)
        printf("[DecoderFixedFloatingPoint] floatingPoint.restBits DEC: %d, floatingPoint.restBits HEX %x\n",
        floatingPoint.restBits, floatingPoint.restBits)
        printf("[DecoderFixedFloatingPoint] exponentBits DEC: %d, exponentBits HEX %x\n",
        exponentBits, exponentBits)
        printf("[DecoderFixedFloatingPoint] mantissaBits DEC: %d, mantissaBits HEX %x\n",
        mantissaBits, mantissaBits)
        printf("[DecoderFixedFloatingPoint] io.binary DEC: %d, io.binary HEX %x\n",
        io.binary, io.binary)
    }
}


class EncoderFixedFloatingPoint(
    exponentSize: Int,
    fractionSize: Int,
    rounding : Option[RoundingType],
    internalExponentSize : Int,
    internalFractionSize : Int,
    size : Int,
    softwareDebug : Boolean = false
    ) extends EncoderFloatingPoint(
        internalExponentSize,
        internalFractionSize,
        size,
        rounding,
        softwareDebug
    ) {
    val minimumInternalFractionSize = FixedFloatingPoint.internalFractionSize(exponentSize, fractionSize)
    val minimumInternalExponentSize = FixedFloatingPoint.internalExponentSize(exponentSize, fractionSize)
    val bias = FixedFloatingPoint.bias(exponentSize)
    val minimumExponentValue = (FixedFloatingPoint.minimumExponent(exponentSize, fractionSize)).abs
    val maximumExponentValue = (FixedFloatingPoint.maximumExponent(exponentSize, fractionSize)).abs
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
                floatingPoint.overflow ||
                (!floatingPoint.exponentSign & (floatingPoint.exponentAbsoluteValue === maximumExponentValue.U) & (floatingPoint.mantissa >= Fill(fractionSize+1, 1.U(1.W)) ) ) 
    val underflow = Wire(Bool())
    underflow := (floatingPoint.exponentSign & (floatingPoint.exponentAbsoluteValue > minimumExponentValue.U)) || floatingPoint.underflow
    if(softwareDebug) printf("[EncoderFixedFloatingPoint] overflow DEC: %d, underflow DEC %d\n", overflow, underflow)

    when(floatingPoint.nan) {
        io.binary := 1.U ## Fill(size-1, 1.U(1.W))
    } .elsewhen(floatingPoint.inf || overflow) {
        io.binary := floatingPoint.sign ## Fill(size-1, 1.U(1.W))
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
            printf("[EncoderFixedFloatingPoint] floatingPoint.exponentSign DEC: %d, floatingPoint.exponentSign HEX %x\n", floatingPoint.exponentSign, floatingPoint.exponentSign)
            printf("[EncoderFixedFloatingPoint] floatingPoint.exponentAbsoluteValue DEC: %d, floatingPoint.exponentAbsoluteValue HEX %x\n", floatingPoint.exponentAbsoluteValue, floatingPoint.exponentAbsoluteValue)
            printf("[EncoderFixedFloatingPoint] exponentBits DEC: %d, exponentBits HEX %x\n", exponentBits, exponentBits)
        }
        val partialBinary = Wire(UInt(size.W))
        partialBinary := floatingPoint.sign ## exponentBits ## (floatingPoint.mantissa(fractionSize - 1, 0))
        if(softwareDebug) printf("[EncoderFixedFloatingPoint] partialBinary DEC: %d, partialBinary HEX %x\n", partialBinary, partialBinary)
        
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
        io.binary := partialBinary + roundingModule.io.addOne
    }
    if(softwareDebug) printf("[EncoderFixedFloatingPoint] io.binary DEC: %d, io.binary HEX %x\n", io.binary, io.binary)
}