package ro.upb.nrs.hgl

import chisel3._
import chisel3.util._

case class Posit(
    exponentSize: Int,
    size: Int,
    rounding : RoundingType,
    softwareDebug : Boolean
) extends Bundle with HNumberRepresentationSystem[Posit] with FloatingPointTrait {
    require(exponentSize >= 0)
    require(size >= 3)

	val value : UInt = Output(UInt((size).W))
    val internalExponentSize = Posit.internalExponentSize(exponentSize, size)
    val internalFractionSize = Posit.internalFractionSize(exponentSize, size)

    def floatingPointValue : FloatingPoint = {
		val decoder = Module(
            new DecoderPosit(
                exponentSize,
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

	override def +(that: Posit): Posit = {
		val encode = Module(
            new EncoderPosit(
                exponentSize,
                size,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue + that.floatingPointValue
		val out = Wire(Posit(exponentSize, size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def -(that: Posit): Posit = {
		val encode = Module(
            new EncoderPosit(
                exponentSize,
                size,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue - that.floatingPointValue
		val out = Wire(Posit(exponentSize, size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def *(that: Posit): Posit = {
		val encode = Module(
            new EncoderPosit(
                exponentSize,
                size,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue * that.floatingPointValue
		val out = Wire(Posit(exponentSize, size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def /(that: Posit): Posit = {
		val encode = Module(
            new EncoderPosit(
                exponentSize,
                size,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue / that.floatingPointValue
		val out = Wire(Posit(exponentSize, size, rounding, softwareDebug))
		out.value := encode.io.binary
		out
	}

	override def sqrt: Posit = {
		val encode = Module(
            new EncoderPosit(
                exponentSize,
                size,
                Some(rounding),
                internalExponentSize,
                internalFractionSize,
                softwareDebug
            )
        )
        encode.io.floatingPoint := this.floatingPointValue.sqrt
		val out = Wire(Posit(exponentSize, size, rounding, softwareDebug))
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
        new DecoderPosit(
            this.exponentSize,
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
        new EncoderPosit(
            this.exponentSize,
            floatingPointSize,
            Some(this.rounding),
            floatingPointExponentSize,
            floatingPointFractionSize,
            softwareDebug
        )
    }
    def getOptimalAccumulatorSize : BigInt = Posit.accumulatorSize(exponentSize, size)
    def getOptimalAccumulatorFractionSize : BigInt = Posit.accumulatorFractionSize(exponentSize, size)
    def getStandardAccumulatorSize : BigInt = BigInt(2).pow(
        log2Ceil(
            this.getOptimalAccumulatorSize - 1 //-1 overflow
        )
    )
}

object Posit {
	def apply(exponentSize: Int,
                size: Int,
                rounding : RoundingType = RoundEven,
                softwareDebug : Boolean = false
            ) : Posit = new Posit(exponentSize, size, rounding, softwareDebug)
    def minimumExponent(exponentSize: Int, size: Int) : BigInt = -(size - 2) * (BigInt(1) << exponentSize)
    def maximumExponent(exponentSize: Int, size: Int) : BigInt = (size - 2) * (BigInt(1) << exponentSize)
    def internalExponentSize(exponentSize: Int, size: Int) : Int = {
        val minimumExponentValue = Posit.minimumExponent(exponentSize, size)
        log2Ceil(minimumExponentValue.abs + 1)
    }
    def internalFractionSize(exponentSize: Int, size: Int) : Int = size-3-exponentSize
    def accumulatorFractionSize(exponentSize: Int, size: Int) : BigInt = {
        2 * (Posit.minimumExponent(exponentSize, size)).abs
    }
    def accumulatorSize(exponentSize: Int, size: Int) : BigInt = {
        Posit.accumulatorFractionSize(exponentSize, size) + //afs
        2 * Posit.maximumExponent(exponentSize, size) + //max exp
        1 +//sign
        1 //overflow
    }
}


class DecoderPosit(
    exponentSize: Int,
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
    val minimumInternalFractionSize = Posit.internalFractionSize(exponentSize, size)
    val minimumInternalExponentSize = Posit.internalExponentSize(exponentSize, size)
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
    /*
    sign is positive you decode de rest of the bits
    sign is negative you decode de two's complement of the rest of the bits
    two's complement of x is ~x plus 1
    */
    val decodeBits = Wire(UInt((size-1).W))
    decodeBits := Mux(
                            floatingPoint.sign,
                            ~io.binary( (size - 2), 0) + 1.U,
                            io.binary( (size - 2), 0)
                        )
    if(softwareDebug) {
        printf("[DecoderPosit] decodeBits DEC: %d, decodeBits HEX %x\n",
        decodeBits, decodeBits)
    }
    /*
    The next value is needed for decode is regime. It is represented by the number of bits
    with the same value after sign and with the last one with a opposit value
    Example : s_rrrrrr_~r_XXXXXXX
    ~r = 1 - r
    number of bits with the same value is the number of r
    s can be considered zero (because we do two complement if it is 1)
    s_rrrrrr_~r_XXXXXXX -> 0_rrrrrr_~r_XXXXXXX
    Example 0_0000_1_XXX -> number_of_same_bit_value = 4
    Example 0_111_0_XXXX -> number_of_same_bit_value = 3
    if the first bit is zero than regime (k) is -number_of_same_bit_value
    else if the first bit is one than regime is number_of_same_bit_value - 1
    Example 0_0000_1_XXX -> number_of_same_bit_value = 4, regime = -4
    Example 0_111_0_XXXX -> number_of_same_bit_value = 3, regime = 2
    Also number of bits occupied by the regime is number_of_same_bit_value + 1
    also called regime size
    We have some special case:
    Maximum value possible 0_111_1111 -> no ~r at the end we consider that is a hidden one
    Zero value 0_000_0000 -> zero treated separated

    Find the number of continous zeroes or ones in bits.
    If the first bit is one. Negate all the bits, reverse the bits and find the index of the first one.
    If the first bit is zero. Reverse the bits and find the index of the first one.
    */
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
    /*
    Size occupied by the regime is the number of the same value bit
    plus one (the bit that has a different value)
    */
    if(softwareDebug) {
        printf("[DecoderPosit] regimeFirstBit DEC: %d, regimeFirstBit HEX %x\n",
        regimeFirstBit, regimeFirstBit)
        printf("[DecoderPosit] consecutiveValues DEC: %d, consecutiveValues HEX %x\n",
        consecutiveValues, consecutiveValues)
        printf("[DecoderPosit] regimeSize DEC: %d, regimeSize HEX %x\n",
        regimeSize, regimeSize)
    }
    /*
    calculate the number of fraction bits
    size - 1 (sign bit) - regime_size - max_exponent_size
    if the value is less than zero or zero than is zero
    */
    val fractionSize = Wire(UInt(log2Ceil(size+1).W))
    fractionSize := Mux(
                            regimeSize >= (size - 1 - exponentSize).U,
                            0.U,
                            (size - 1 - exponentSize).U - regimeSize
                        )
    val mantissaBits = Wire(UInt((minimumInternalFractionSize+1).W))
    mantissaBits := Mux(
                                fractionSize === 0.U,
                                0.U,
                                decodeBits << (minimumInternalFractionSize.U - fractionSize)
                            ) | (1.U(1.W) ## 0.U(minimumInternalFractionSize.W))
    floatingPoint.mantissa := mantissaBits
    if(softwareDebug) {
        printf("[DecoderPosit] fractionSize DEC: %d, fractionSize HEX %x\n",
        fractionSize, fractionSize)
        printf("[DecoderPosit] mantissaBits DEC: %d, mantissaBits HEX %x\n",
        mantissaBits, mantissaBits)
    }
    /*
    After regime bits are the exponent bits they are ussualy
    exponent size bits, but in some special case the real binary size
    can be less that exponent size. Than the bits represent the MSB of
    a exponent of an exponent_size bits (rest of the bits are considered 0 that 
    are not in the binary representation)
    Eg: size = 8 es =2
    0_111_0110 regime = 2 binary_exponent = 11 exponent = 11 = 3
    0_111_1011 regime = 3 binary_exponent = 11 exponent = 11 = 3
    0_111_1101 regime = 4 binary_exponent = 1 exponent = 10 = 2
    0_111_1110 regime = 5 binary_exponent = 0 exponent = 00 = 0

    To calculate binary_exponent_size we have to look on how many bits are remaining
    after we take sign bit and regime bits, but we can not take more than exponent_size
    bits. formula will be:
    binary_exponent_size = max(0, min(size - 1 (sign bit) - regime_size, max_exponent_size))
    exponent will be the next binary_exponent_size bits from bits to decode. At the end
    we need to add bits of zero with shifting left the bits remaining to make and expoennt
    of exponent_size bits:
    exponent = binary_exponent << (exponent_size - binary_exponent_size)
    */
    /*
    Calculate the possible exponent size
    max(0, min(size - 1 (sign bit) - regime_size, max_exponent_size))
    */
    val binaryExponentSize = Wire(UInt(log2Ceil(size).W))
    binaryExponentSize := Mux(
                                regimeSize >= (size - 1).U,
                                0.U,
                                Mux(
                                    regimeSize > (size - 1 - exponentSize).U,
                                    (size - 1).U - regimeSize,
                                    exponentSize.U
                                )
                            )
    /*
    If the thexponent size is zero than the exponent is zero,
    otherwise we sift right with fraction_size to eliminate the fraction bits
    and with a mask only for exponent_size
    */
    val binaryExponent = Wire(UInt(exponentSize.W))
    binaryExponent := Mux(
                            binaryExponentSize === 0.U,
                            0.U,
                            ( decodeBits >> (fractionSize) ) << (exponentSize.U - binaryExponentSize)
                        )
    /*
    the final exponent is (2^exponent_size) * regime + exponent =
    = regime << exponent_size + binary_exponent
    */
    val regimeAbsoluteValue = Wire(UInt(log2Ceil(size+1).W))
    regimeAbsoluteValue := Mux(regimeFirstBit,
                                Mux(
                                    decodeBits === Fill(size-1, 1.U(1.W)), //special case when maximum value
                                    consecutiveValues,
                                    consecutiveValues - 1.U
                                ),
                                consecutiveValues)
    floatingPoint.exponentSign := !regimeFirstBit
    floatingPoint.exponentAbsoluteValue := Mux(floatingPoint.exponentSign,
                                        (regimeAbsoluteValue << exponentSize) -& binaryExponent,
                                        (regimeAbsoluteValue << exponentSize) +& binaryExponent
                                        )
    if(softwareDebug) {
        printf("[DecoderPosit] binaryExponentSize DEC: %d, binaryExponentSize HEX %x\n",
        binaryExponentSize, binaryExponentSize)
        printf("[DecoderPosit] binaryExponent DEC: %d, binaryExponent HEX %x\n",
        binaryExponent, binaryExponent)
        printf("[DecoderPosit] regimeAbsoluteValue DEC: %d, regimeAbsoluteValue HEX %x\n",
        regimeAbsoluteValue, regimeAbsoluteValue)
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
        printf("[DecoderPosit] floatingPoint.sign DEC: %d, floatingPoint.zero DEC: %d, floatingPoint.inf DEC: %d, floatingPoint.nan DEC: %d, floatingPoint.underflow DEC: %d, floatingPoint.overflow DEC: %d, \n",
        floatingPoint.sign, floatingPoint.zero, floatingPoint.inf, floatingPoint.nan, floatingPoint.underflow, floatingPoint.overflow)
        printf("[DecoderPosit] floatingPoint.exponentSign DEC: %d, floatingPoint.exponentSign HEX %x\n",
        floatingPoint.exponentSign, floatingPoint.exponentSign)
        printf("[DecoderPosit] floatingPoint.exponentAbsoluteValue DEC: %d, floatingPoint.exponentAbsoluteValue HEX %x\n",
        floatingPoint.exponentAbsoluteValue, floatingPoint.exponentAbsoluteValue)
        printf("[DecoderPosit] floatingPoint.mantissa DEC: %d, floatingPoint.mantissa HEX %x\n",
        floatingPoint.mantissa, floatingPoint.mantissa)
        printf("[DecoderPosit] floatingPoint.restBits DEC: %d, floatingPoint.restBits HEX %x\n",
        floatingPoint.restBits, floatingPoint.restBits)
        printf("[DecoderPosit] mantissaBits DEC: %d, mantissaBits HEX %x\n",
        mantissaBits, mantissaBits)
    }
}


class EncoderPosit(
    exponentSize: Int,
    size: Int,
    rounding : Option[RoundingType],
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
    val minimumInternalFractionSize = Posit.internalFractionSize(exponentSize, size)
    val minimumInternalExponentSize = Posit.internalExponentSize(exponentSize, size)
    val minimumExponentValue = (Posit.minimumExponent(exponentSize, size)).abs
    val maximumExponentValue = (Posit.maximumExponent(exponentSize, size)).abs
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
                floatingPoint.overflow  ||
                (!floatingPoint.exponentSign & ((floatingPoint.exponentAbsoluteValue === maximumExponentValue.U) & (floatingPoint.mantissa >= (1.U(1.W) ## 0.U(minimumInternalFractionSize.W))) ))
    val underflow = Wire(Bool())
    underflow := (floatingPoint.exponentSign & (floatingPoint.exponentAbsoluteValue > minimumExponentValue.U)) || floatingPoint.underflow
    if(softwareDebug) printf("[EncoderPosit] overflow DEC: %d, underflow DEC %d\n", overflow, underflow)

    when(floatingPoint.nan || floatingPoint.inf) {
        io.binary := 1.U(1.W) ## Fill(size-1, 0.U(1.W))
    } .elsewhen(overflow) {
        io.binary := Mux(floatingPoint.sign, 1.U(1.W) ## Fill(size-2, 0.U(1.W)) ## 1.U(1.W), 0.U(1.W) ## Fill(size-1, 1.U(1.W)))
    } .elsewhen(floatingPoint.zero) {
        io.binary := Fill(size, 0.U(1.W))
    } .elsewhen(underflow) {
        io.binary := Mux(floatingPoint.sign, Fill(size, 1.U(1.W)), Fill(size-1, 0.U(1.W)) ## 1.U(1.W))
    } .otherwise {
        val regimeAbsoluteValue = Wire(UInt(log2Ceil(size+1).W))
        regimeAbsoluteValue := Mux(floatingPoint.exponentSign,
                                    (floatingPoint.exponentAbsoluteValue - 1.U)(Posit.internalExponentSize(exponentSize, size)-1, exponentSize) + 1.U,
                                    floatingPoint.exponentAbsoluteValue(Posit.internalExponentSize(exponentSize, size)-1, exponentSize)
                                )
        val regimeFirstBit = Wire(Bool())
        regimeFirstBit := !floatingPoint.exponentSign
        val consecutiveValues = Wire(UInt(log2Ceil(size).W))
        consecutiveValues := Mux(
                                    regimeFirstBit,
                                    regimeAbsoluteValue + 1.U,
                                    regimeAbsoluteValue
                                )
        val regimeSize = Wire(UInt(log2Ceil(size+1).W))
        regimeSize := consecutiveValues + 1.U
        if(softwareDebug) {
            printf("[EncoderPosit] regimeAbsoluteValue DEC: %d, regimeAbsoluteValue HEX %x\n", regimeAbsoluteValue, regimeAbsoluteValue)
            printf("[EncoderPosit] regimeFirstBit DEC: %d, regimeFirstBit HEX %x\n", regimeFirstBit, regimeFirstBit)
            printf("[EncoderPosit] consecutiveValues DEC: %d, consecutiveValues HEX %x\n", consecutiveValues, consecutiveValues)
            printf("[EncoderPosit] regimeSize DEC: %d, regimeSize HEX %x\n", regimeSize, regimeSize)
        }
        val binaryExponentSize = Wire(UInt(log2Ceil(size).W))
        binaryExponentSize := Mux(
                                    regimeSize >= (size - 1).U,
                                    0.U,
                                    Mux(
                                        regimeSize > (size - 1 - exponentSize).U,
                                        (size - 1).U - regimeSize,
                                        exponentSize.U
                                    )
                                )
        val exponent = Wire(UInt(exponentSize.W))
        if(exponentSize >= 1) {
            exponent := Mux(floatingPoint.exponentSign,
                            (1.U(1.W) ## 0.U(exponentSize.W)) - floatingPoint.exponentAbsoluteValue(exponentSize-1, 0),
                            floatingPoint.exponentAbsoluteValue(exponentSize-1, 0)
                        )
        } else {
            exponent := 0.U
        }
        if(softwareDebug) {
            printf("[EncoderPosit] exponent DEC: %d, exponent HEX %x\n", exponent, exponent)
            printf("[EncoderPosit] binaryExponentSize DEC: %d, binaryExponentSize HEX %x\n", binaryExponentSize, binaryExponentSize)
        }
        val fractionSize = Wire(UInt(log2Ceil(size+1).W))
        fractionSize := Mux(
                                regimeSize >= (size - 1 - exponentSize).U,
                                0.U,
                                (size - 1 - exponentSize).U - regimeSize
                            )
        if(softwareDebug) {
            printf("[EncoderPosit] fractionSize DEC: %d, fractionSize HEX %x\n", fractionSize, fractionSize)
            printf("[EncoderPosit] minimumInternalFractionSize DEC: %d, minimumInternalFractionSize HEX %x\n", minimumInternalFractionSize.U, minimumInternalFractionSize.U)
        }
        val mantissaBits = Wire(UInt(minimumInternalFractionSize.W))
        mantissaBits := floatingPoint.mantissa(minimumInternalFractionSize-1, 0) >> (minimumInternalFractionSize.U - fractionSize)
        val exponentBits = Wire(UInt((size-1).W))
        exponentBits := (exponent >> (exponentSize.U - binaryExponentSize)) << fractionSize
        val regimeBits = Wire(UInt((size-1).W))
        regimeBits := Mux(
                            regimeFirstBit,
                            ( ((Fill(size, 1.U(1.W)) ^ 3.U) << (binaryExponentSize + fractionSize))(size - 1, 0) >> 1 ),
                            ( (1.U << (binaryExponentSize + fractionSize)) )
                        )
        val absolutePartialBinary = Wire(UInt(size.W))
        absolutePartialBinary := regimeBits | exponentBits | mantissaBits
        val partialBinary = Wire(UInt(size.W))
        
        //rounding
		val roundingModule = Module(new FloatingPointRounding(softwareDebug))
        roundingModule.io.sign := floatingPoint.sign
        roundingModule.io.l := absolutePartialBinary(0)
        roundingModule.io.g := floatingPoint.restBits(2)
        roundingModule.io.r := floatingPoint.restBits(1)
        roundingModule.io.s := floatingPoint.restBits(0)
        partialBinary := absolutePartialBinary +& roundingModule.io.addOne
        io.roundingType match {
            case Some(r) => roundingModule.io.rounding := r
            case None => roundingModule.io.rounding := RoundingType.toUInt(rounding.get)
        }
        io.binary := Mux(floatingPoint.sign, ~partialBinary +& 1.U, partialBinary)
        
        when(minimumInternalFractionSize.U > fractionSize) {//ussualy happens
            when( (exponentSize.U - binaryExponentSize) >= 2.U) {
                //when g and r are exponent bits 
                if(exponentSize >= 2) {
                    roundingModule.io.g := (exponent >> ((exponentSize - 1).U - binaryExponentSize))(0)
                    roundingModule.io.r := (exponent >> ((exponentSize - 2).U - binaryExponentSize))(0)
                    // calcualte s from the other exponent bits, mantisa bits and any other rest bits
                    //s := (exponent & (Fill(exponentSize-2, 1.U(1.W)) >> binaryExponentSize)).orR || floatingPoint.mantissa(minimumInternalFractionSize-1, 0).orR || floatingPoint.restBits.orR
                    if(exponentSize > 2) {
                        val exponentMask = Wire(UInt((exponentSize-2).W))
                        exponentMask := Fill((exponentSize-2), 1.U(1.W)) >> binaryExponentSize
                        roundingModule.io.s := (exponent & exponentMask).orR ||
                                                floatingPoint.mantissa(minimumInternalFractionSize - 1, 0).orR ||
                                                floatingPoint.restBits.orR
                    } else {
                        roundingModule.io.s := floatingPoint.mantissa(minimumInternalFractionSize - 1, 0).orR ||
                                                floatingPoint.restBits.orR
                    }
                } else {
                    //case imposible
                    roundingModule.io.g := false.B
                    roundingModule.io.r := false.B
                    roundingModule.io.s := false.B
                    printf("[EncoderPosit][ERROR] binaryExponentSize DEC: %d, binaryExponentSize HEX %x\n", binaryExponentSize, binaryExponentSize)
                }
            } .elsewhen( (exponentSize.U - binaryExponentSize) === 1.U) {
                // when g is exponent bits and rest are fraction
                if(exponentSize >= 1) {
                    roundingModule.io.g := exponent(0)
                } else {
                    //case imposible
                    roundingModule.io.g := false.B
                    printf("[EncoderPosit][ERROR] binaryExponentSize DEC: %d, binaryExponentSize HEX %x\n", binaryExponentSize, binaryExponentSize)
                }
                roundingModule.io.r := floatingPoint.mantissa(minimumInternalFractionSize - 1)
                roundingModule.io.s := floatingPoint.mantissa(minimumInternalFractionSize - 1, 0).orR ||
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
                    printf("[EncoderPosit][ERROR] fractionSize DEC: %d, fractionSize HEX %x\n", fractionSize, fractionSize)
                }
            }
        }

        if(softwareDebug) {
            printf("[EncoderPosit] regimeBits DEC: %d, regimeBits HEX %x\n", regimeBits, regimeBits)
            printf("[EncoderPosit] exponentBits DEC: %d, exponentBits HEX %x\n", exponentBits, exponentBits)
            printf("[EncoderPosit] mantissaBits DEC: %d, mantissaBits HEX %x\n", mantissaBits, mantissaBits)
            printf("[EncoderPosit] absolutePartialBinary DEC: %d, absolutePartialBinary HEX %x\n", absolutePartialBinary, absolutePartialBinary)
            printf("[EncoderPosit] partialBinary DEC: %d, partialBinary HEX %x\n", partialBinary, partialBinary)
        }
    }
    if(softwareDebug) printf("[EncoderPosit] io.binary DEC: %d, io.binary HEX %x\n", io.binary, io.binary)
}