package ro.upb.nrs.hgl

import chisel3._
import chisel3.util._


trait FloatingPointTrait {
    def getSize : Int
    def getInternalExponentSize : Int
    def getInternalFractionSize : Int
    def getDecoderFloatingPoint(
        floatingPointExponentSize : Int,
        floatingPointFractionSize : Int,
        floatingPointSize : Int,
        softwareDebug : Boolean
    ) : DecoderFloatingPoint
    def getEncoderFloatingPoint(
        floatingPointExponentSize : Int,
        floatingPointFractionSize : Int,
        floatingPointSize : Int,
        softwareDebug : Boolean
    ) : EncoderFloatingPoint
    def getOptimalAccumulatorSize : BigInt
    def getOptimalAccumulatorFractionSize : BigInt
    def getAccumulatorSize : Int = this.getOptimalAccumulatorSize.min(2048).toInt
    def getAccumulatorFractionSize : Int = this.getOptimalAccumulatorFractionSize.min(1024).toInt
}
/*
We consider floatingPoint(es, fs)
(-1)^sign * 2^exponent * ( mantisa / 2^fractionSize )
mantisa / 2^fractionSize in [1, 2)
es, fs are fixed
mantisa has the hiddent bit that is always 1
there are 2 special numbers:
(+-infinity)
Zero - 0
NaN
*/
class FloatingPoint(exponentSize: Int, fractionSize: Int, softwareDebug : Boolean = false) extends Bundle {
    //sign
	val sign = Bool()
    //is zero ?
	val zero = Bool()
    //is inf ?
	val inf = Bool()
    //is a nan ?
	val nan = Bool()
    //is a underflow ?
	val underflow = Bool()
    //is a overflow ?
	val overflow = Bool()
    //exponent sign
	val exponentSign = Bool()
    //exponent absolute value 
	val exponentAbsoluteValue = UInt(exponentSize.W)
    // mantisa with  fractionSize bits + 1 hidden bit
	val mantissa = UInt((fractionSize + 1).W)
    // rest bits
    val restBits = UInt(3.W) //g,r,s

    def +(that : FloatingPoint) : FloatingPoint = {
        val adder = Module(new FloatingPointAddition(exponentSize, fractionSize, softwareDebug))
        val subtracter = Module(new FloatingPointSubtraction(exponentSize, fractionSize, softwareDebug))
        val comparator = Module(new FloatingPointComparator(exponentSize, fractionSize, softwareDebug))
		val result = Wire(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        result := adder.io.result
        adder.io.operand1 := this
        adder.io.operand2 := that
        subtracter.io.operand1 := this
        subtracter.io.operand2 := that
        comparator.io.operand1 := this
        comparator.io.operand2 := that
        when(this.sign === that.sign) {
            when((comparator.io.g ^ this.sign) | comparator.io.e) { //this>=that in absolute value
                adder.io.operand1 := this
                adder.io.operand2 := that
                if(softwareDebug) printf("[FloatingPoint] this + that")
            } .otherwise {
                adder.io.operand1 := that
                adder.io.operand2 := this
                if(softwareDebug) printf("[FloatingPoint] that + this")
            }
        } .otherwise {
            //this - (-that)
            comparator.io.operand2 := -that
            when((comparator.io.g ^ this.sign) | comparator.io.e) { //this >= -that in absolute value
                subtracter.io.operand1 := this
                subtracter.io.operand2 := -that
                result := subtracter.io.result
                if(softwareDebug) printf("[FloatingPoint] this - (-that)")
            } .otherwise {
                subtracter.io.operand1 := -that
                subtracter.io.operand2 := this
                result := -subtracter.io.result
                if(softwareDebug) printf("[FloatingPoint] -( (-that) - this)")
            }
        }
        result
    }
    
    def -(that : FloatingPoint) : FloatingPoint = {
        this + (-that)
    }

    
    def *(that : FloatingPoint) : FloatingPoint = {
        val multiplier = Module(new FloatingPointMultiplication(exponentSize, fractionSize, softwareDebug))
		val result = Wire(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        multiplier.io.operand1 := this
        multiplier.io.operand2 := that
        result := multiplier.io.result
        result
    }

    
    def fm(that : FloatingPoint, accumulatorSize : Int, accumulatorFractionSize : Int) : Accumulator = {
        val fusedMultiplier = Module(
            new FloatingPointFusedMultiplication(
                exponentSize,
                fractionSize,
                accumulatorSize,
                accumulatorFractionSize,
                softwareDebug
            )
        )
		val result = Wire(new Accumulator(accumulatorSize, accumulatorFractionSize, softwareDebug))
        fusedMultiplier.io.operand1 := this
        fusedMultiplier.io.operand2 := that
        result := fusedMultiplier.io.result
        result
    }

    def /(that : FloatingPoint) : FloatingPoint = {
        val divider = Module(new FloatingPointDivider(exponentSize, fractionSize, softwareDebug))
		val result = Wire(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        divider.io.operand1 := this
        divider.io.operand2 := that
        result := divider.io.result
        result
    }

    def sqrt : FloatingPoint = {
        val squareRoot = Module(new FloatingPointSquareRoot(exponentSize, fractionSize, softwareDebug))
		val result = Wire(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        squareRoot.io.operand := this
        result := squareRoot.io.result
        result
    }

    //logical operations
    def >(that : FloatingPoint) : Bool = {
        val comparator = Module(new FloatingPointComparator(exponentSize, fractionSize, softwareDebug))
        comparator.io.operand1 := this
        comparator.io.operand2 := that
        comparator.io.g
    }

    def <(that : FloatingPoint) : Bool = {
        val comparator = Module(new FloatingPointComparator(exponentSize, fractionSize, softwareDebug))
        comparator.io.operand1 := this
        comparator.io.operand2 := that
        comparator.io.l
    }

    def ==(that : FloatingPoint) : Bool = {
        val comparator = Module(new FloatingPointComparator(exponentSize, fractionSize, softwareDebug))
        comparator.io.operand1 := this
        comparator.io.operand2 := that
        comparator.io.e
    }
    def >=(that : FloatingPoint) : Bool = (this > that) | (this == that)
    def <=(that : FloatingPoint) : Bool = (this < that) | (this == that)

    def unary_- : FloatingPoint = {
		val result = Wire(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        result.sign := !this.sign
        result.zero := this.zero
        result.inf := this.inf
        result.nan := this.nan
        result.underflow := this.underflow
        result.overflow := this.overflow
        result.exponentSign := this.exponentSign
        result.exponentAbsoluteValue := this.exponentAbsoluteValue
        result.mantissa := this.mantissa
        result.restBits := this.restBits
        result
    }
}


class DecoderFloatingPoint(exponentSize: Int, fractionSize: Int, size : Int, softwareDebug : Boolean = false) extends Module {
    val io = IO(new Bundle {
        val binary = Input(UInt(size.W))
        val result = Output(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
    })
    if(softwareDebug) printf("[DecoderFloatingPoint] io.binary DEC: %d, io.binary HEX %x\n",
    io.binary, io.binary)
    io.result.restBits := 0.U
}


class EncoderFloatingPoint(exponentSize: Int, fractionSize: Int, size : Int, rounding: Option[RoundingType], softwareDebug : Boolean = false) extends Module {
    val io = IO(new Bundle {
        val binary = Output(UInt(size.W))
        val floatingPoint = Input(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        val roundingType = if (rounding.isEmpty) Some(Input(UInt(3.W))) else None
    })

    if(softwareDebug) {
        printf("[EncoderFloatingPoint] io.floatingPoint.sign DEC: %d, io.floatingPoint.zero DEC: %d, io.floatingPoint.inf DEC: %d, io.floatingPoint.nan DEC: %d, io.floatingPoint.underflow DEC: %d, io.result.overflow DEC: %d, \n",
        io.floatingPoint.sign, io.floatingPoint.zero, io.floatingPoint.inf, io.floatingPoint.nan, io.floatingPoint.underflow, io.floatingPoint.overflow) 
        printf("[EncoderFloatingPoint] io.floatingPoint.exponentSign DEC: %d, io.floatingPoint.exponentAbsoluteValue DEC %d\n",
        io.floatingPoint.exponentSign, io.floatingPoint.exponentAbsoluteValue)
        printf("[EncoderFloatingPoint] io.floatingPoint.mantissa DEC: %d, io.floatingPoint.mantissa HEX %x\n",
        io.floatingPoint.mantissa, io.floatingPoint.mantissa)
        printf("[EncoderFloatingPoint] io.floatingPoint.restBits DEC: %d, io.floatingPoint.restBits HEX %x\n",
        io.floatingPoint.restBits, io.floatingPoint.restBits)
    }
}


class FloatingPointAddition(exponentSize: Int, fractionSize: Int, softwareDebug : Boolean = false) extends Module {
    val io = IO(new Bundle {
        val operand1 = Input(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        val operand2 = Input(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        val result = Output(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
    })

    // operand1 and operand2 have the same sign
    // operand1 absolute value is bigger than operand2
    io.result.sign := io.operand1.sign
    io.result.zero := false.B
    io.result.inf := false.B
    io.result.nan := false.B
    io.result.underflow := false.B
    io.result.overflow := false.B
    io.result.exponentSign := false.B
    io.result.exponentAbsoluteValue := 0.U
    io.result.mantissa := 0.U
    io.result.restBits := 0.U

    when(io.operand1.nan || io.operand2.nan) { //NAN + x = NAN
        io.result.nan := true.B
    } .elsewhen(io.operand1.inf || io.operand2.inf) { // INF + x = INF
        io.result.inf := true.B
    } .elsewhen(io.operand1.zero && io.operand2.zero) { // 0 + x = x // 0 when negative
        if(softwareDebug) printf("[FloatingPointAddition] both zero")
        io.result.sign := io.operand1.sign & io.operand2.sign
        io.result.zero := true.B
    } .elsewhen(io.operand1.zero) { // 0 + x = x // 0 when negative
        if(softwareDebug) printf("[FloatingPointAddition] first operand zero")
        io.result := io.operand2
    } .elsewhen(io.operand2.zero) { // x + 0 = x
        if(softwareDebug) printf("[FloatingPointAddition] second operand zero")
        io.result := io.operand1
    } .otherwise { // x + y
        /*
        (-1)^sign * 2^exponent1 * ( mantissa1 / 2^fractionSize ) + (-1)^sign * 2^exponent2 * ( mantissa2 / 2^fractionSize ) =
        = (-1)^sign * 2^exponent1 * ( ( mantissa1 + 2^( exponent2 - exponent1 ) * mantissa2 ) / 2^fractionSize ) =
        = (-1)^sign * 2^exponent1 * ( ( mantissa1 + mantissa2 / 2^( exponent1 - exponent2 ) ) / 2^fractionSize ) =
        = (-1)^sign * 2^exponent1 * ( ( mantissa1 + ( mantissa2 >> ( exponent1 - exponent2 ) ) ) / 2^fractionSize ) = 
        = (-1)^sign * 2^exponent1 * ( ( mantissa1 + ( mantissa2 >> exponentDifference ) ) / 2^fractionSize ) = 
        */
        val exponentDifference = Wire(UInt((exponentSize + 1).W))
        exponentDifference := Mux(
            io.operand1.exponentSign,
            io.operand2.exponentAbsoluteValue -& io.operand1.exponentAbsoluteValue,
            Mux(
                io.operand2.exponentSign,
                io.operand1.exponentAbsoluteValue +& io.operand2.exponentAbsoluteValue,
                io.operand1.exponentAbsoluteValue -& io.operand2.exponentAbsoluteValue
            )
        )
        io.result.exponentSign :=  io.operand1.exponentSign
        io.result.exponentAbsoluteValue := io.operand1.exponentAbsoluteValue
        /*
        +1 hidden bit
        +1 carry bit
        +3 g r s
        */
        val internalMnatissa = Wire(UInt((fractionSize + 1 + 1 + 3).W))
        val restBits = Wire(UInt((fractionSize+1).W))

        restBits := io.operand2.mantissa
        when(exponentDifference > (fractionSize + 3).U) {
            internalMnatissa := io.operand1.mantissa << 3
        } .otherwise {
            val shift = Wire(UInt((log2Ceil(fractionSize + 3) + 1).W))
            shift := exponentDifference
            internalMnatissa := (io.operand1.mantissa << 3) +& ( (io.operand2.mantissa << 3) >> shift)
            restBits := io.operand2.mantissa << ((fractionSize + 1 + 3).U - shift)
            if(softwareDebug) {
                printf("[FloatingPointAddition] (io.operand1.mantissa << 3) DEC: %d, (io.operand1.mantissa << 3) HEX %x\n",
                (io.operand1.mantissa << 3), (io.operand1.mantissa << 3))
                printf("[FloatingPointAddition] ( (io.operand2.mantissa << 3) >> shift) DEC: %d, ( (io.operand2.mantissa << 3) >> shift) HEX %x\n",
                ( (io.operand2.mantissa << 3) >> shift), ( (io.operand2.mantissa << 3) >> shift))
                printf("[FloatingPointAddition] sum DEC: %d, sum HEX %x\n",
                (io.operand1.mantissa << 3) +& ( (io.operand2.mantissa << 3) >> shift), (io.operand1.mantissa << 3) +& ( (io.operand2.mantissa << 3) >> shift))
            }
        }
        
        if(softwareDebug) {
            printf("[FloatingPointAddition] exponentDifference DEC: %d, exponentDifference HEX %x\n",
            exponentDifference, exponentDifference)
            printf("[FloatingPointAddition] internalMnatissa DEC: %d, internalMnatissa HEX %x\n",
            internalMnatissa, internalMnatissa)
            printf("[FloatingPointAddition] restBits DEC: %d, restBits HEX %x\n",
            restBits, restBits)
            printf("[FloatingPointAddition] mantissa overflow DEC: %d, mantissa overflow HEX %x\n",
            internalMnatissa(fractionSize+4), internalMnatissa(fractionSize+4))
        }

        
        when(internalMnatissa(fractionSize+4)) {//mantissa overflow
            when(io.operand1.exponentSign) {
                when(io.operand1.exponentAbsoluteValue === 1.U) {
                    io.result.exponentSign := false.B
                    io.result.exponentAbsoluteValue := 0.U
                } .otherwise {
                    io.result.exponentAbsoluteValue := io.operand1.exponentAbsoluteValue -& 1.U
                }
            } .otherwise {
                io.result.exponentAbsoluteValue := io.operand1.exponentAbsoluteValue +& 1.U
            }
            io.result.mantissa := internalMnatissa(fractionSize+4, 4)
            io.result.restBits := internalMnatissa(3, 1) | internalMnatissa(0) | restBits.orR
            io.result.underflow := false.B //no underflow because you can have only overflow and than mantissa can only increase
            io.result.overflow := !io.result.exponentSign & io.operand1.exponentAbsoluteValue.andR
        } .otherwise {
            io.result.mantissa := internalMnatissa(fractionSize+3, 3)
            io.result.restBits := internalMnatissa(2, 0) | restBits.orR
        }
    }
}


class FloatingPointSubtraction(exponentSize: Int, fractionSize: Int, softwareDebug : Boolean = false) extends Module {
    val io = IO(new Bundle {
        val operand1 = Input(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        val operand2 = Input(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        val result = Output(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
    })

    // operand1 and operand2 have the same sign
    // operand1 absolute value is bigger than operand2
    io.result.sign := io.operand1.sign
    io.result.zero := false.B
    io.result.inf := false.B
    io.result.nan := false.B
    io.result.underflow := false.B
    io.result.overflow := false.B
    io.result.exponentSign := false.B
    io.result.exponentAbsoluteValue := 0.U
    io.result.mantissa := 0.U
    io.result.restBits := 0.U

    when(io.operand1.nan || io.operand2.nan) { //NAN - x = x - NAN = NAN
        io.result.nan := true.B
    } .elsewhen(io.operand1.inf && io.operand2.inf) { // INF - INF = NAN
        io.result.nan := true.B
    } .elsewhen(io.operand1.inf) { // INF - x = INF //same sign we can not have operand 2 INF and operand 1 x
        io.result.inf := true.B
    } .elsewhen(io.operand1.zero && io.operand2.zero) { // 0 - 0 = 0 // TODO difference between -0 and +0
        io.result.sign := io.operand1.sign & !io.operand2.sign
        io.result.zero := true.B
    } .elsewhen(io.operand1.zero) { // 0 - x = x //imposible because 0 can be bigger than anything //only if x is 0
        io.result.sign := !io.operand2.sign
        io.result.zero := io.operand2.zero
        io.result.inf := io.operand2.inf
        io.result.nan := io.operand2.nan
        io.result.underflow := io.operand2.underflow
        io.result.overflow := io.operand2.overflow
        io.result.exponentSign := io.operand2.exponentSign
        io.result.exponentAbsoluteValue := io.operand2.exponentAbsoluteValue
        io.result.mantissa := io.operand2.mantissa
        io.result.restBits := io.operand2.restBits
    } .elsewhen(io.operand2.zero) { // x - 0 = x
        io.result := io.operand1
    } .otherwise { // x + y
        /*
        (-1)^sign * 2^exponent1 * ( mantissa1 / 2^fractionSize ) - (-1)^sign * 2^exponent2 * ( mantissa2 / 2^fractionSize ) =
        = (-1)^sign * 2^exponent1 * ( ( mantissa1 - 2^( exponent2 - exponent1 ) * mantissa2 ) / 2^fractionSize ) =
        = (-1)^sign * 2^exponent1 * ( ( mantissa1 - mantissa2 / 2^( exponent1 - exponent2 ) ) / 2^fractionSize ) =
        = (-1)^sign * 2^exponent1 * ( ( mantissa1 - ( mantissa2 >> ( exponent1 - exponent2 ) ) ) / 2^fractionSize ) =
        = (-1)^sign * 2^exponent1 * ( ( mantissa1 - ( mantissa2 >> exponentDifference ) ) / 2^fractionSize )
        */
        val exponentDifference = Wire(UInt((exponentSize + 1).W))
        exponentDifference := Mux(
            io.operand1.exponentSign,
            io.operand2.exponentAbsoluteValue -& io.operand1.exponentAbsoluteValue,
            Mux(
                io.operand2.exponentSign,
                io.operand1.exponentAbsoluteValue +& io.operand2.exponentAbsoluteValue,
                io.operand1.exponentAbsoluteValue -& io.operand2.exponentAbsoluteValue
            )
        )
        io.result.exponentSign :=  io.operand1.exponentSign
        io.result.exponentAbsoluteValue := io.operand1.exponentAbsoluteValue

        /*
        +1 hidden bit
        + fractionSize  worst case scenario
        +3 g r s in rost case scenario
        */
        val temporalMantissa = Wire(UInt((fractionSize + 1 + fractionSize + 3).W))
        val internalMantissa = Wire(UInt((fractionSize + 1 + fractionSize + 3).W))
        val restBits = Wire(UInt(1.W))

        io.result.restBits := 0.U
        when(exponentDifference > (2 * fractionSize + 3).U) {
            temporalMantissa := (io.operand1.mantissa << (fractionSize + 3))
            restBits := 1.U
        } .otherwise {
            val shift = Wire(UInt((log2Ceil(2 * fractionSize + 3) + 1).W))
            shift := exponentDifference
            temporalMantissa := (io.operand1.mantissa << (fractionSize + 3)) - ( (io.operand2.mantissa << (fractionSize + 3)) >> shift)
            restBits := (io.operand2.mantissa << ((2*fractionSize + 1 + 3).U - shift))(fractionSize, 0).orR
            if(softwareDebug) {
                printf("[FloatingPointSubtraction]  (io.operand1.mantissa << (fractionSize + 3)) DEC: %d,  (io.operand1.mantissa << (fractionSize + 3)) HEX %x\n",
                (io.operand1.mantissa << (fractionSize + 3)),  (io.operand1.mantissa << (fractionSize + 3)))
                printf("[FloatingPointSubtraction] ( (io.operand2.mantissa << (fractionSize + 3)) >> shift) DEC: %d, ( (io.operand2.mantissa << (fractionSize + 3)) >> shift) HEX %x\n",
                ( (io.operand2.mantissa << (fractionSize + 3)) >> shift), ( (io.operand2.mantissa << (fractionSize + 3)) >> shift))
                printf("[FloatingPointSubtraction] sum DEC: %d, sum HEX %x\n",
                (io.operand1.mantissa << (fractionSize + 3)) - ( (io.operand2.mantissa << (fractionSize + 3)) >> shift), (io.operand1.mantissa << (fractionSize + 3)) - ( (io.operand2.mantissa << (fractionSize + 3)) >> shift))
            }
        }

        when(restBits(0)) {
            internalMantissa := temporalMantissa - 1.U
        } .otherwise {
            internalMantissa := temporalMantissa
        }

        if(softwareDebug) {
            printf("[FloatingPointSubtraction] exponentDifference DEC: %d, exponentDifference HEX %x\n",
            exponentDifference, exponentDifference)
            printf("[FloatingPointSubtraction] temporalMantissa DEC: %d, temporalMantissa HEX %x\n",
            temporalMantissa, temporalMantissa)
            printf("[FloatingPointSubtraction] internalMantissa DEC: %d, internalMantissa HEX %x\n",
            internalMantissa, internalMantissa)
            printf("[FloatingPointSubtraction] restBits DEC: %d, restBits HEX %x\n",
            restBits, restBits)
            printf("[FloatingPointSubtraction] normal DEC: %d, normal HEX %x\n",
            internalMantissa(2*fractionSize+3), internalMantissa(2*fractionSize+3))
        }
        
        when(internalMantissa(2*fractionSize+3)) {//mantissa normal
            io.result.mantissa := internalMantissa(2*fractionSize + 3, fractionSize+3)
            io.result.restBits := internalMantissa(fractionSize + 2, fractionSize) | internalMantissa(fractionSize - 1, 0).orR
            //todo caz special rezultatul e zero
        } .elsewhen(internalMantissa.orR === 0.U) {
            io.result.restBits := 0.U
            io.result.zero := true.B
            io.result.sign := false.B
        } .otherwise { //mantissa underflow
            //we find the hidden bit
            //
            val shiftLeft = Wire(UInt((log2Ceil(fractionSize) + 1).W))
            shiftLeft := PriorityEncoder(Reverse(internalMantissa))
            val normalisedMantissa = Wire(UInt((fractionSize + 1 + fractionSize + 3).W))
            normalisedMantissa := internalMantissa << shiftLeft
            if(softwareDebug) {
                printf("[FloatingPointSubtraction] shiftLeft DEC: %d, shiftLeft HEX %x\n",
                shiftLeft, shiftLeft)
                printf("[FloatingPointSubtraction] normalisedMantissa DEC: %d, normalisedMantissa HEX %x\n",
                normalisedMantissa, normalisedMantissa)
            }
            io.result.mantissa := normalisedMantissa(2*fractionSize + 3, fractionSize+3)
            io.result.restBits := normalisedMantissa(fractionSize + 2, fractionSize) | normalisedMantissa(fractionSize - 1, 0).orR
            //for restbit 1 is going on this todo> maybe test

            //exponent sign
            val exponentSign = Wire(Bool())
            //exponent basolute value + 1 for overflow
            val maximum_exponentSize = exponentSize.max(log2Ceil(fractionSize + 1) + 1) + 1
            val exponentAbsoluteValue = Wire(UInt(maximum_exponentSize.W))
            exponentSign := io.operand1.exponentSign
            exponentAbsoluteValue := io.operand1.exponentAbsoluteValue
            when(io.operand1.exponentSign) {//negative exponent
                exponentSign := io.operand1.exponentSign
                exponentAbsoluteValue := io.operand1.exponentAbsoluteValue +& shiftLeft
            } .otherwise {
                when(io.operand1.exponentAbsoluteValue >= shiftLeft) {
                    exponentAbsoluteValue := io.operand1.exponentAbsoluteValue -& shiftLeft
                } .otherwise {
                    exponentSign := true.B
                    exponentAbsoluteValue := shiftLeft -& io.operand1.exponentAbsoluteValue
                }
            }

            io.result.exponentSign := exponentSign
            io.result.exponentAbsoluteValue := exponentAbsoluteValue(exponentSize - 1, 0)

            when(io.result.exponentAbsoluteValue === 0.U) {// 0 exponent
                io.result.exponentSign := false.B
            }

            when(exponentAbsoluteValue(maximum_exponentSize - 1, exponentSize).orR) {// underflow or overflow
                io.result.underflow := exponentSign
                io.result.overflow := !exponentSign
            }
        }
    }
}

class FloatingPointMultiplication(exponentSize: Int, fractionSize: Int, softwareDebug : Boolean = false) extends Module {
    val io = IO(new Bundle {
        val operand1 = Input(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        val operand2 = Input(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        val result = Output(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
    })
    io.result.sign := io.operand1.sign ^ io.operand2.sign
    io.result.zero := false.B
    io.result.inf := false.B
    io.result.nan := false.B
    io.result.underflow := false.B
    io.result.overflow := false.B
    io.result.exponentSign := false.B
    io.result.exponentAbsoluteValue := 0.U
    io.result.mantissa := 0.U
    io.result.restBits := 0.U

    when(io.operand1.nan || io.operand2.nan) {
        io.result.nan := true.B
    } .elsewhen(io.operand1.inf || io.operand2.inf) {
        io.result.inf := true.B
        when(io.operand1.zero || io.operand2.zero) {
            io.result.nan := true.B
        }
    } .elsewhen(io.operand1.zero || io.operand2.zero) {
        io.result.zero := true.B
    } .otherwise {
        val mantissaMultiplication = Wire(UInt((2*fractionSize + 2).W))
        mantissaMultiplication := io.operand1.mantissa * io.operand2.mantissa
        val mantissaOverflow = Wire(Bool())
        mantissaOverflow := mantissaMultiplication(2*fractionSize + 1)
        io.result.mantissa := Mux(mantissaOverflow,
                                    mantissaMultiplication(2*fractionSize+1, fractionSize+1),
                                    mantissaMultiplication(2*fractionSize, fractionSize)
                                )
        if (fractionSize >= 4) {
            io.result.restBits := Mux(mantissaOverflow,
                    mantissaMultiplication(fractionSize, fractionSize-2) | mantissaMultiplication(fractionSize-3, 0).orR,
                    mantissaMultiplication(fractionSize-1, fractionSize-3) | mantissaMultiplication(fractionSize-4, 0).orR
                )
        } else {
            if(fractionSize == 3) 
                io.result.restBits := Mux(mantissaOverflow,
                    mantissaMultiplication(fractionSize, 1) | mantissaMultiplication(0),
                    mantissaMultiplication(fractionSize-1, 0)
                )
            else
                io.result.restBits := Mux(mantissaOverflow,
                    mantissaMultiplication(fractionSize, 0) << (2 - fractionSize),
                    mantissaMultiplication(fractionSize-1, 0) << (3 - fractionSize)
                )
        }
        if(softwareDebug) {
            printf("[FloatingPointMultiplication] mantissaMultiplication DEC: %d, mantissaMultiplication HEX %x\n",
            mantissaMultiplication, mantissaMultiplication)
            printf("[FloatingPointMultiplication] mantissaOverflow DEC: %d, mantissaOverflow HEX %x\n",
            mantissaOverflow, mantissaOverflow)
            printf("[FloatingPointMultiplication] io.result.mantissa DEC: %d, io.result.mantissa HEX %x\n",
            io.result.mantissa, io.result.mantissa)
            printf("[FloatingPointMultiplication] io.result.restBits DEC: %d, io.result.restBits HEX %x\n",
            io.result.restBits, io.result.restBits)
        }
        //exponent sign
        val exponentSign = Wire(Bool())
        //exponent basolute value + 1 for overflow
        val maximum_exponentSize = exponentSize.max(log2Ceil(fractionSize + 1) + 1) + 1
        val exponentAbsoluteValue = Wire(UInt(maximum_exponentSize.W))
        exponentSign := io.operand1.exponentSign
        exponentAbsoluteValue := io.operand1.exponentAbsoluteValue
        when(io.operand1.exponentSign === io.operand2.exponentSign) {
            exponentSign := io.operand1.exponentSign
            when(io.operand1.exponentSign) {
                exponentAbsoluteValue := io.operand1.exponentAbsoluteValue +& io.operand2.exponentAbsoluteValue -& Mux(mantissaOverflow, 1.U, 0.U)
            } .otherwise {
                exponentAbsoluteValue := io.operand1.exponentAbsoluteValue +& io.operand2.exponentAbsoluteValue +& Mux(mantissaOverflow, 1.U, 0.U)
            }
        } .otherwise {
            when(io.operand1.exponentSign) {
                when(io.operand2.exponentAbsoluteValue >= io.operand1.exponentAbsoluteValue) {
                    exponentSign := false.B
                    exponentAbsoluteValue := io.operand2.exponentAbsoluteValue -& io.operand1.exponentAbsoluteValue +& Mux(mantissaOverflow, 1.U, 0.U)
                } .otherwise {
                    exponentAbsoluteValue := io.operand1.exponentAbsoluteValue -& io.operand2.exponentAbsoluteValue -& Mux(mantissaOverflow, 1.U, 0.U)
                }
            } .otherwise {
                when(io.operand1.exponentAbsoluteValue >= io.operand2.exponentAbsoluteValue) {
                    exponentAbsoluteValue := io.operand1.exponentAbsoluteValue -& io.operand2.exponentAbsoluteValue +& Mux(mantissaOverflow, 1.U, 0.U)
                } .otherwise {
                    exponentSign := true.B
                    exponentAbsoluteValue := io.operand2.exponentAbsoluteValue -& io.operand1.exponentAbsoluteValue -& Mux(mantissaOverflow, 1.U, 0.U)
                }
            }
        }

        io.result.exponentSign := exponentSign
        io.result.exponentAbsoluteValue := exponentAbsoluteValue(exponentSize - 1, 0)

        when(io.result.exponentAbsoluteValue === 0.U) {// 0 exponent
            io.result.exponentSign := false.B
        }

        when(exponentAbsoluteValue(maximum_exponentSize - 1, exponentSize).orR) {// underflow or overflow
            io.result.underflow := exponentSign
            io.result.overflow := !exponentSign
        }
        if(softwareDebug) {
            printf("[FloatingPointMultiplication] maximum_exponentSize DEC: %d, maximum_exponentSize HEX %x\n",
            maximum_exponentSize.U, maximum_exponentSize.U)
            printf("[FloatingPointMultiplication] exponentSign DEC: %d, exponentSign HEX %x\n",
            exponentSign, exponentSign)
            printf("[FloatingPointMultiplication] exponentAbsoluteValue DEC: %d, exponentAbsoluteValue HEX %x\n",
            exponentAbsoluteValue, exponentAbsoluteValue)
            printf("[FloatingPointMultiplication] exponentAbsoluteValue(maximum_exponentSize - 1, exponentSize) DEC: %d, exponentAbsoluteValue(maximum_exponentSize - 1, exponentSize) HEX %x\n",
            exponentAbsoluteValue(maximum_exponentSize - 1, exponentSize), exponentAbsoluteValue(maximum_exponentSize - 1, exponentSize))
        }
    }
}


class FloatingPointDivider(exponentSize: Int, fractionSize: Int, softwareDebug : Boolean = false) extends Module {
    val io = IO(new Bundle {
        val operand1 = Input(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        val operand2 = Input(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        val result = Output(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
    })
    io.result.sign := io.operand1.sign ^ io.operand2.sign
    io.result.zero := false.B
    io.result.inf := false.B
    io.result.nan := false.B
    io.result.underflow := false.B
    io.result.overflow := false.B
    io.result.exponentSign := false.B
    io.result.exponentAbsoluteValue := 0.U
    io.result.mantissa := 0.U
    io.result.restBits := 0.U

    when(io.operand1.nan || io.operand2.nan) { // NAN /x = x / NAN = NAN/NAN = NAN
        io.result.nan := true.B
    } .elsewhen(io.operand1.inf && io.operand2.inf) { //INF/INF =NAN
        io.result.nan := true.B
    } .elsewhen(io.operand1.inf) { //INF/x =INF
        io.result.inf := true.B
    } .elsewhen(io.operand2.inf) { //x/INF=0
        io.result.zero := true.B
    } .elsewhen(io.operand1.zero && io.operand2.zero) { //0/0 =NAN
        io.result.nan := true.B
    }  .elsewhen(io.operand1.zero) { //0/x=0
        io.result.zero := true.B
    } .elsewhen(io.operand2.zero) { //x/0=INF
        io.result.inf := true.B
    } .otherwise {
        // fractionSize + 1 the result
        // +1 underflow
        // +3 g r s
        val mantissaDivision = Wire(UInt((fractionSize + 1 + 1 + 3).W))
        // result on fs + 5 -> we divide by fs + 1 so we need to divide 2*fs+6
        mantissaDivision := (io.operand1.mantissa << (fractionSize + 4)) / io.operand2.mantissa
        val mantissaRemainder = Wire(Bool())
        mantissaRemainder := ((io.operand1.mantissa << (fractionSize + 4)) % io.operand2.mantissa).orR
        val mantissaUnderflow = Wire(Bool())
        mantissaUnderflow := !mantissaDivision(fractionSize + 4)
        
        io.result.mantissa := Mux(mantissaUnderflow,
                                    mantissaDivision(fractionSize + 3, 3),
                                    mantissaDivision(fractionSize + 4, 4)
                                )
        io.result.restBits := Mux(mantissaUnderflow,
                                    mantissaDivision(2, 0) | mantissaRemainder,
                                    mantissaDivision(3, 1) | mantissaDivision(0) | mantissaRemainder
                                )
        if(softwareDebug) {
            printf("[FloatingPointDivider] (io.operand1.mantissa << (fractionSize + 4)) DEC: %d, (io.operand1.mantissa << (fractionSize + 4)) HEX %x\n",
            (io.operand1.mantissa << (fractionSize + 4)), (io.operand1.mantissa << (fractionSize + 4)))
            printf("[FloatingPointDivider] (io.operand1.mantissa << (fractionSize + 5)) / io.operand1.mantissa DEC: %d, (io.operand1.mantissa << (fractionSize + 5)) / io.operand1.mantissa HEX %x\n",
            (io.operand1.mantissa << (fractionSize + 4)) / io.operand2.mantissa, (io.operand1.mantissa << (fractionSize + 4)) / io.operand2.mantissa)
            printf("[FloatingPointDivider] mantissaDivision DEC: %d, mantissaDivision HEX %x\n",
            mantissaDivision, mantissaDivision)
            printf("[FloatingPointDivider] mantissaRemainder DEC: %d, mantissaRemainder HEX %x\n",
            mantissaRemainder, mantissaRemainder)
            printf("[FloatingPointDivider] mantissaUnderflow DEC: %d, mantissaUnderflow HEX %x\n",
            mantissaUnderflow, mantissaUnderflow)
            printf("[FloatingPointDivider] io.result.mantissa DEC: %d, io.result.mantissa HEX %x\n",
            io.result.mantissa, io.result.mantissa)
        }
        //exponent sign
        val exponentSign = Wire(Bool())
        //exponent basolute value + 1 for overflow
        val maximum_exponentSize = exponentSize.max(log2Ceil(fractionSize + 1) + 1) + 1
        val exponentAbsoluteValue = Wire(UInt(maximum_exponentSize.W))
        exponentSign := io.operand1.exponentSign
        exponentAbsoluteValue := io.operand1.exponentAbsoluteValue
        when(io.operand1.exponentSign === io.operand2.exponentSign) {
            when(io.operand1.exponentSign) {
                when(io.operand1.exponentAbsoluteValue >= io.operand2.exponentAbsoluteValue) {
                    exponentAbsoluteValue := io.operand1.exponentAbsoluteValue -& io.operand2.exponentAbsoluteValue +& Mux(mantissaUnderflow, 1.U, 0.U)
                } .otherwise {
                    exponentSign := false.B
                    exponentAbsoluteValue := io.operand2.exponentAbsoluteValue -& io.operand1.exponentAbsoluteValue -& Mux(mantissaUnderflow, 1.U, 0.U)
                }
            } .otherwise {
                when(io.operand1.exponentAbsoluteValue > io.operand2.exponentAbsoluteValue) {
                    exponentAbsoluteValue := io.operand1.exponentAbsoluteValue -& io.operand2.exponentAbsoluteValue -& Mux(mantissaUnderflow, 1.U, 0.U)
                } .otherwise {
                    exponentSign := true.B
                    exponentAbsoluteValue := io.operand2.exponentAbsoluteValue -& io.operand1.exponentAbsoluteValue +& Mux(mantissaUnderflow, 1.U, 0.U)
                }
            }
        } .otherwise {
            when(io.operand1.exponentSign) {
                exponentAbsoluteValue := io.operand1.exponentAbsoluteValue +& io.operand2.exponentAbsoluteValue +& Mux(mantissaUnderflow, 1.U, 0.U)
            } .otherwise {
                when( (io.operand1.exponentAbsoluteValue === 0.U) & (io.operand2.exponentAbsoluteValue === 0.U) ) { //special case udnerflow and both exponent 0
                    exponentSign := true.B
                    exponentAbsoluteValue := Mux(mantissaUnderflow, 1.U, 0.U)
                } .otherwise {
                    exponentAbsoluteValue := io.operand1.exponentAbsoluteValue +& io.operand2.exponentAbsoluteValue -& Mux(mantissaUnderflow, 1.U, 0.U)
                }
            }
        }

        io.result.exponentSign := exponentSign
        io.result.exponentAbsoluteValue := exponentAbsoluteValue(exponentSize - 1, 0)

        when(io.result.exponentAbsoluteValue === 0.U) {// 0 exponent
            io.result.exponentSign := false.B
        }

        when(exponentAbsoluteValue(maximum_exponentSize - 1, exponentSize).orR) {// underflow or overflow
            io.result.underflow := exponentSign
            io.result.overflow := !exponentSign
        }
        if(softwareDebug) {
            printf("[FloatingPointDivider] maximum_exponentSize DEC: %d, maximum_exponentSize HEX %x\n",
            maximum_exponentSize.U, maximum_exponentSize.U)
            printf("[FloatingPointDivider] exponentSign DEC: %d, exponentSign HEX %x\n",
            exponentSign, exponentSign)
            printf("[FloatingPointDivider] exponentAbsoluteValue DEC: %d, exponentAbsoluteValue HEX %x\n",
            exponentAbsoluteValue, exponentAbsoluteValue)
            printf("[FloatingPointDivider] exponentAbsoluteValue(maximum_exponentSize - 1, exponentSize) DEC: %d, exponentAbsoluteValue(maximum_exponentSize - 1, exponentSize) HEX %x\n",
            exponentAbsoluteValue(maximum_exponentSize - 1, exponentSize), exponentAbsoluteValue(maximum_exponentSize - 1, exponentSize))
        }
    }
}



class FloatingPointSquareRoot(exponentSize: Int, fractionSize: Int, softwareDebug : Boolean = false) extends Module {
    val io = IO(new Bundle {
        val operand = Input(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        val result = Output(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
    })
    io.result.sign := false.B
    io.result.zero := false.B
    io.result.inf := false.B
    io.result.nan := false.B
    io.result.underflow := false.B
    io.result.overflow := false.B
    io.result.exponentSign := false.B
    io.result.exponentAbsoluteValue := 0.U
    io.result.mantissa := 0.U
    io.result.restBits := 0.U

    when(io.operand.nan || (io.operand.sign & !io.operand.zero)) { // SQRT(NaN) and SQRT(X<0)
        io.result.nan := true.B
    } .elsewhen(io.operand.zero) { //SQRT(-0)=-0
        io.result := io.operand
    } .elsewhen(io.operand.inf) { //SQRT(INF) = INF
        io.result.inf := true.B
    } .otherwise {
        // 2* (fractionSize) -> to get fraction size bits
        // + 1 hidden bit
        // + 1 exponent oabsolute value odd
        // + 2 * 2 = 4 for g r
        val mantissaSQRT = Wire(UInt((2 * fractionSize + 1 + 1 + 4).W))
        // result on fs + 5 -> we divide by fs + 1 so we need to divide 2*fs+6
        mantissaSQRT := (io.operand.mantissa << (fractionSize + 4)) << io.operand.exponentAbsoluteValue(0)
        val mantissaSQRTResult = Wire(UInt((2 * fractionSize + 1 + 1 + 4).W))
        val mantissaSQRTRemainder = Wire(Bool())

        val sqrtModule = Module(new NQRT(2 * fractionSize + 1 + 1 + 4, 2))
        sqrtModule.io.op := mantissaSQRT
        mantissaSQRTResult := sqrtModule.io.q
        mantissaSQRTRemainder := sqrtModule.io.r.orR
        // hard to explain but is not such thing as overflow
        
        io.result.mantissa := mantissaSQRTResult(fractionSize + 2, 2)
        io.result.restBits := mantissaSQRTResult(1,0) ## mantissaSQRTRemainder

        if(softwareDebug) {
            printf("[FloatingPointSquareRoot] io.operand.mantissa DEC: %d, io.operand.mantissa HEX %x\n",
            io.operand.mantissa, io.operand.mantissa)
            printf("[FloatingPointSquareRoot] mantissaSQRT DEC: %d, mantissaSQRT HEX %x\n",
            mantissaSQRT, mantissaSQRT)
            printf("[FloatingPointSquareRoot] mantissaSQRTResult DEC: %d, mantissaSQRTResult HEX %x\n",
            mantissaSQRTResult, mantissaSQRTResult)
            printf("[FloatingPointSquareRoot] mantissaRemainder DEC: %d, mantissaRemainder HEX %x\n",
            mantissaSQRTRemainder, mantissaSQRTRemainder)
            printf("[FloatingPointSquareRoot] io.result.mantissa DEC: %d, io.result.mantissa HEX %x\n",
            io.result.mantissa, io.result.mantissa)
        }
        //exponent sign
        val exponentSign = Wire(Bool())
        val exponentAbsoluteValue = Wire(UInt(exponentSize.W))
        exponentSign := io.operand.exponentSign
        exponentAbsoluteValue := (io.operand.exponentAbsoluteValue +& Mux(io.operand.exponentSign, 1.U, 0.U)) >> 1
        io.result.exponentSign := exponentSign
        io.result.exponentAbsoluteValue := exponentAbsoluteValue(exponentSize - 1, 0)

        when(io.result.exponentAbsoluteValue === 0.U) {// 0 exponent
            io.result.exponentSign := false.B
        }
        if(softwareDebug) {
            printf("[FloatingPointSquareRoot] exponentSign DEC: %d, exponentSign HEX %x\n",
            exponentSign, exponentSign)
            printf("[FloatingPointSquareRoot]  io.operand.exponentAbsoluteValue DEC: %d,  io.operand.exponentAbsoluteValue HEX %x\n",
             io.operand.exponentAbsoluteValue,  io.operand.exponentAbsoluteValue)
            printf("[FloatingPointSquareRoot] exponentAbsoluteValue DEC: %d, exponentAbsoluteValue HEX %x\n",
            exponentAbsoluteValue, exponentAbsoluteValue)
        }
    }
}


class FloatingPointComparator(exponentSize: Int, fractionSize: Int, softwareDebug : Boolean = false) extends Module {
    val io = IO(new Bundle {
        val operand1 = Input(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        val operand2 = Input(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        val g = Output(Bool())
        val ge = Output(Bool())
        val e = Output(Bool())
        val l = Output(Bool())
        val le = Output(Bool())
    })
    io.g := false.B
    io.e := false.B
    io.l := false.B
    io.ge := io.g | io.e
    io.le := io.l | io.e

    when(io.operand1.nan || io.operand2.nan) { //NaN logi operation -> false
        io.g := false.B
        io.e := false.B
        io.l := false.B
    } .elsewhen(io.operand1.zero & io.operand2.zero) {//0==0
        io.g := false.B
        io.e := true.B
        io.l := false.B
    } .elsewhen(io.operand1.zero) {//0>
        io.g := io.operand2.sign
        io.e := false.B
        io.l := !io.operand2.sign
    } .elsewhen(io.operand2.zero) {//<0
        if(softwareDebug) printf("[FloatingPointComparator] Operand 2 zero\n")
        io.g := !io.operand1.sign
        io.e := false.B
        io.l := io.operand1.sign
    } .elsewhen(io.operand1.sign && !io.operand2.sign) {
        io.g := false.B
        io.e := false.B
        io.l := true.B
    } .elsewhen(!io.operand1.sign && io.operand2.sign) {
        io.g := true.B
        io.e := false.B
        io.l := false.B
    } .elsewhen(io.operand1.inf && io.operand2.inf) {
        io.g := false.B
        io.e := false.B
        io.l := false.B
    } .elsewhen(io.operand1.inf) {
        io.g := !io.operand1.sign
        io.e := false.B
        io.l := io.operand1.sign
    } .elsewhen(io.operand2.inf) {
        io.g := io.operand2.sign
        io.e := false.B
        io.l := !io.operand2.sign
    }  .otherwise {
        val biggerExponent = Wire(Bool())
        biggerExponent := (!io.operand1.exponentSign & io.operand2.exponentSign) ||
                          (io.operand1.exponentSign & io.operand2.exponentSign & (io.operand1.exponentAbsoluteValue < io.operand2.exponentAbsoluteValue) ) ||
                          (!io.operand1.exponentSign & !io.operand2.exponentSign & (io.operand1.exponentAbsoluteValue > io.operand2.exponentAbsoluteValue) )
        val equalExponent = Wire(Bool())
        equalExponent := (io.operand1.exponentSign === io.operand2.exponentSign) & (io.operand1.exponentAbsoluteValue === io.operand2.exponentAbsoluteValue)
        val biggerMantissa = Wire(Bool())
        biggerMantissa := io.operand1.mantissa > io.operand2.mantissa
        val equalMantissa = Wire(Bool())
        equalMantissa := io.operand1.mantissa === io.operand2.mantissa
        if(softwareDebug) {
            printf("[FloatingPointComparator] biggerExponent DEC: %d, equalExponent DEC: %d, biggerMantissa DEC: %d, equalMantissa DEC: %d,\n",
            biggerExponent, equalExponent, biggerMantissa, equalMantissa)
        }
        io.g := (biggerExponent | (equalExponent & biggerMantissa)) ^ io.operand1.sign
        io.e := equalExponent & equalMantissa
        io.l := !io.g & !io.e
    }
}

class FloatingPointRounding(softwareDebug : Boolean = false) extends Module {
    val io = IO(new Bundle {
        val addOne = Output(Bool())
        val l = Input(Bool())
        val g = Input(Bool())
        val r = Input(Bool())
        val s = Input(Bool())
        val sign = Input(Bool())
        val rounding = Input(UInt(3.W))
    })

    when(io.rounding === 0.U) {
        io.addOne := Mux(io.g&(io.r|io.s|io.l), true.B, false.B)
    } .elsewhen(io.rounding === 1.U) {
        io.addOne := 0.U
    } .elsewhen(io.rounding === 2.U) {
        io.addOne := Mux(io.sign & (io.g|io.r|io.s), true.B, false.B) 
    } .elsewhen(io.rounding === 3.U) {
        io.addOne := Mux(!io.sign & (io.g|io.r|io.s), true.B, false.B)
    } .elsewhen(io.rounding === 4.U) {
        io.addOne := Mux(io.g|io.r|io.s, true.B, false.B)
    } .otherwise {
        io.addOne := 0.U
    }
    if(softwareDebug) printf("[FloatingPointRounding] l DEC: %d, g DEC: %d, r DEC: %d, s DEC: %d, addOne DEC: %d\n", io.l, io.g, io.r, io.s, io.addOne)
}


class FloatingPointConverter(
    sourceExponentSize: Int,
    sourceFractionSize: Int,
    destinationExponentSize: Int,
    destinationFractionSize: Int,
    softwareDebug : Boolean = false
    ) extends Module {
    val io = IO(new Bundle {
        val source = Input(new FloatingPoint(sourceExponentSize, sourceFractionSize, softwareDebug))
        val destination = Output(new FloatingPoint(destinationExponentSize, destinationFractionSize, softwareDebug))
    })
    if((sourceExponentSize == destinationExponentSize) && (sourceFractionSize == destinationFractionSize)) {
        io.destination := io.source
    } else {
        io.destination.sign := io.source.sign
        if(sourceExponentSize == destinationExponentSize) {
            io.destination.nan := io.source.nan
            io.destination.zero := io.source.zero
            io.destination.inf := io.source.inf
            io.destination.overflow := io.source.overflow
            io.destination.underflow := io.source.underflow
            io.destination.exponentSign := io.source.exponentSign
            io.destination.exponentAbsoluteValue := io.source.exponentAbsoluteValue
        } else if (sourceExponentSize < destinationExponentSize) {
            //we don't know how was the underflow or overflow
            io.destination.nan := io.source.nan | io.source.overflow | io.source.underflow
            io.destination.zero := io.source.zero
            io.destination.inf := io.source.inf
            io.destination.overflow := false.B
            io.destination.underflow := false.B
            io.destination.exponentSign := io.source.exponentSign
            io.destination.exponentAbsoluteValue := io.source.exponentAbsoluteValue
        } else {
            io.destination.nan := io.source.nan
            io.destination.zero := io.source.zero
            io.destination.inf := io.source.inf
            io.destination.overflow := io.source.overflow |
                (
                    !io.source.exponentSign &
                    io.source.exponentAbsoluteValue(sourceExponentSize-1,destinationExponentSize).orR
                )
            io.destination.underflow := io.source.underflow |
                (
                    io.source.exponentSign &
                    io.source.exponentAbsoluteValue(sourceExponentSize-1,destinationExponentSize).orR
                )
            io.destination.exponentSign := io.source.exponentSign
            io.destination.exponentAbsoluteValue := io.source.exponentAbsoluteValue(destinationExponentSize-1, 0)
        }
        if(sourceFractionSize == destinationFractionSize) {
            io.destination.mantissa := io.source.mantissa
            io.destination.restBits := io.source.restBits
        } else if (sourceFractionSize > destinationFractionSize) {
            val differenceFractionSize = sourceFractionSize - destinationFractionSize
            io.destination.mantissa := io.source.mantissa(sourceFractionSize, differenceFractionSize)
            if(differenceFractionSize == 1) {
                io.destination.restBits := io.source.mantissa(0) ## io.source.restBits(2) ## (io.source.restBits(1) | io.source.restBits(0))
            } else if(differenceFractionSize == 2) {
                io.destination.restBits := io.source.mantissa(1) ## io.source.mantissa(0) ## io.source.restBits.orR
            } else { //differenceFractionSize >= 3
                io.destination.restBits := io.source.mantissa(differenceFractionSize - 1) ## io.source.mantissa(differenceFractionSize - 2) ## (io.source.mantissa(differenceFractionSize - 3, 0).orR | io.source.restBits.orR)
            }
        } else {
            val differenceFractionSize = destinationFractionSize - sourceFractionSize
            if(differenceFractionSize == 1) {
                io.destination.mantissa := io.source.mantissa(sourceFractionSize, 0) ## io.source.restBits(2)
                io.destination.restBits := io.source.restBits(1) ## 0.U(1.W) ## io.source.restBits(0)
            } else if(differenceFractionSize == 2) {
                io.destination.mantissa := io.source.mantissa(sourceFractionSize, 0) ## io.source.restBits(2) ## io.source.restBits(1)
                io.destination.restBits := 0.U(2.W) ## io.source.restBits(0)
            } else { //differenceFractionSize >= 3
                io.destination.mantissa := io.source.mantissa(sourceFractionSize, 0) ## io.source.restBits(2) ## io.source.restBits(1) ## 0.U((differenceFractionSize-2).W)
                io.destination.restBits := 0.U(2.W) ## io.source.restBits(0)
            }
        }

    }
    if(softwareDebug) {
        //stats
        printf("[FloatingPointConverter] sourceExponentSize DEC: %d, sourceExponentSize HEX %x\n",
        sourceExponentSize.U, sourceExponentSize.U)
        printf("[FloatingPointConverter] destinationExponentSize DEC: %d, destinationExponentSize HEX %x\n",
        destinationExponentSize.U, destinationExponentSize.U)
        printf("[FloatingPointConverter] sourceFractionSize DEC: %d, sourceFractionSize HEX %x\n",
        sourceFractionSize.U, sourceFractionSize.U)
        printf("[FloatingPointConverter] destinationFractionSize DEC: %d, destinationFractionSize HEX %x\n",
        destinationFractionSize.U, destinationFractionSize.U)
        //source
        printf("[FloatingPointConverter] io.source.sign DEC: %d, io.source.sign HEX %x\n",
        io.source.sign, io.source.sign)
        printf("[FloatingPointConverter] io.source.nan DEC: %d, io.source.nan HEX %x\n",
        io.source.nan, io.source.nan)
        printf("[FloatingPointConverter] io.source.zero DEC: %d, io.source.zero HEX %x\n",
        io.source.zero, io.source.zero)
        printf("[FloatingPointConverter] io.source.inf DEC: %d, io.source.inf HEX %x\n",
        io.source.inf, io.source.inf)
        printf("[FloatingPointConverter] io.source.overflow DEC: %d, io.source.overflow HEX %x\n",
        io.source.overflow, io.source.overflow)
        printf("[FloatingPointConverter] io.source.underflow DEC: %d, io.source.underflow HEX %x\n",
        io.source.underflow, io.source.underflow)
        printf("[FloatingPointConverter] io.source.exponentSign DEC: %d, io.source.exponentSign HEX %x\n",
        io.source.exponentSign, io.source.exponentSign)
        printf("[FloatingPointConverter] io.source.exponentAbsoluteValue DEC: %d, io.source.exponentAbsoluteValue HEX %x\n",
        io.source.exponentAbsoluteValue, io.source.exponentAbsoluteValue)
        printf("[FloatingPointConverter] io.source.mantissa DEC: %d, io.source.mantissa HEX %x\n",
        io.source.mantissa, io.source.mantissa)
        printf("[FloatingPointConverter] io.source.restBits DEC: %d, io.source.restBits HEX %x\n",
        io.source.restBits, io.source.restBits)
        //destination
        printf("[FloatingPointConverter] io.destination.sign DEC: %d, io.destination.sign HEX %x\n",
        io.destination.sign, io.destination.sign)
        printf("[FloatingPointConverter] io.destination.nan DEC: %d, io.destination.nan HEX %x\n",
        io.destination.nan, io.destination.nan)
        printf("[FloatingPointConverter] io.destination.zero DEC: %d, io.destination.zero HEX %x\n",
        io.destination.zero, io.destination.zero)
        printf("[FloatingPointConverter] io.destination.inf DEC: %d, io.destination.inf HEX %x\n",
        io.destination.inf, io.destination.inf)
        printf("[FloatingPointConverter] io.destination.overflow DEC: %d, io.destination.overflow HEX %x\n",
        io.destination.overflow, io.destination.overflow)
        printf("[FloatingPointConverter] io.destination.underflow DEC: %d, io.destination.underflow HEX %x\n",
        io.destination.underflow, io.destination.underflow)
        printf("[FloatingPointConverter] io.destination.exponentSign DEC: %d, io.destination.exponentSign HEX %x\n",
        io.destination.exponentSign, io.destination.exponentSign)
        printf("[FloatingPointConverter] io.destination.exponentAbsoluteValue DEC: %d, io.destination.exponentAbsoluteValue HEX %x\n",
        io.destination.exponentAbsoluteValue, io.destination.exponentAbsoluteValue)
        printf("[FloatingPointConverter] io.destination.mantissa DEC: %d, io.destination.mantissa HEX %x\n",
        io.destination.mantissa, io.destination.mantissa)
        printf("[FloatingPointConverter] io.destination.restBits DEC: %d, io.destination.restBits HEX %x\n",
        io.destination.restBits, io.destination.restBits)
    }
}



class FloatingPointFusedMultiplication(
    exponentSize: Int,
    fractionSize: Int,
    accumulatorSize : Int,
    accumulatorFractionSize : Int,
    softwareDebug : Boolean = false
) extends Module {
    val io = IO(new Bundle {
        val operand1 = Input(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        val operand2 = Input(new FloatingPoint(exponentSize, fractionSize, softwareDebug))
        val result = Output(new Accumulator(accumulatorSize, accumulatorFractionSize, softwareDebug))
    })
    io.result.value := 0.U
    io.result.underflow := false.B
    io.result.overflow := false.B

    when(io.operand1.nan || io.operand2.nan) {
        io.result.underflow := true.B
        io.result.overflow := true.B
    } .elsewhen(io.operand1.inf || io.operand2.inf) {
        io.result.underflow := io.operand1.sign ^ io.operand2.sign
        io.result.overflow := !io.result.underflow
        when(io.operand1.zero || io.operand2.zero) {
            io.result.underflow := true.B
            io.result.overflow := true.B
        }
    } .elsewhen(io.operand1.zero || io.operand2.zero) {
        io.result.value := 0.U
    } .otherwise {
        val mantissaMultiplication = Wire(UInt((2*fractionSize + 2).W))
        mantissaMultiplication := io.operand1.mantissa * io.operand2.mantissa
        val multiplicationFractionSize = 2*fractionSize
        val mantissaOverflow = Wire(Bool())
        mantissaOverflow := mantissaMultiplication(2*fractionSize + 1)

        if(softwareDebug) {
            printf("[FloatingPointFusedMultiplication] mantissaMultiplication DEC: %d, mantissaMultiplication HEX %x\n",
            mantissaMultiplication, mantissaMultiplication)
            printf("[FloatingPointFusedMultiplication] mantissaOverflow DEC: %d, mantissaOverflow HEX %x\n",
            mantissaOverflow, mantissaOverflow)
        }
        //exponent sign
        val exponentSign = Wire(Bool())
        //exponent basolute value + 1 for overflow
        val maximum_exponentSize = exponentSize.max(log2Ceil(fractionSize + 1) + 1) + 1
        val exponentAbsoluteValue = Wire(UInt(maximum_exponentSize.W))
        exponentSign := io.operand1.exponentSign
        exponentAbsoluteValue := io.operand1.exponentAbsoluteValue
        when(io.operand1.exponentSign === io.operand2.exponentSign) {
            exponentSign := io.operand1.exponentSign
            when(io.operand1.exponentSign) {
                exponentAbsoluteValue := io.operand1.exponentAbsoluteValue +& io.operand2.exponentAbsoluteValue
            } .otherwise {
                exponentAbsoluteValue := io.operand1.exponentAbsoluteValue +& io.operand2.exponentAbsoluteValue
            }
        } .otherwise {
            when(io.operand1.exponentSign) {
                when(io.operand2.exponentAbsoluteValue >= io.operand1.exponentAbsoluteValue) {
                    exponentSign := false.B
                    exponentAbsoluteValue := io.operand2.exponentAbsoluteValue -& io.operand1.exponentAbsoluteValue
                } .otherwise {
                    exponentAbsoluteValue := io.operand1.exponentAbsoluteValue -& io.operand2.exponentAbsoluteValue
                }
            } .otherwise {
                when(io.operand1.exponentAbsoluteValue >= io.operand2.exponentAbsoluteValue) {
                    exponentAbsoluteValue := io.operand1.exponentAbsoluteValue -& io.operand2.exponentAbsoluteValue
                } .otherwise {
                    exponentSign := true.B
                    exponentAbsoluteValue := io.operand2.exponentAbsoluteValue -& io.operand1.exponentAbsoluteValue
                }
            }
        }
        when(exponentAbsoluteValue === 0.U) {// 0 exponent
            exponentSign := false.B
        }

        val mantissaBits = Wire(UInt(accumulatorSize.W))
        if(multiplicationFractionSize <= accumulatorFractionSize)
            mantissaBits := mantissaMultiplication << (accumulatorFractionSize - multiplicationFractionSize) //going to right binary point
        else
            mantissaBits := mantissaMultiplication >> (multiplicationFractionSize - accumulatorFractionSize) //going to right binary point
            
        val shiftedBits = Wire(UInt(accumulatorSize.W))
        shiftedBits := Mux(
            exponentSign,
            mantissaBits >> exponentAbsoluteValue,
            mantissaBits << exponentAbsoluteValue
        )
        io.result.value := Mux(io.operand1.sign ^ io.operand2.sign, ~shiftedBits+1.U, shiftedBits)
        io.result.overflow := !exponentSign & (exponentAbsoluteValue >= ((accumulatorSize - 1 - accumulatorFractionSize).U -& Mux(mantissaOverflow, 1.U, 0.U)))

        if(softwareDebug) {
            printf("[FloatingPointFusedMultiplication] maximum_exponentSize DEC: %d, maximum_exponentSize HEX %x\n",
            maximum_exponentSize.U, maximum_exponentSize.U)
            printf("[FloatingPointFusedMultiplication] exponentSign DEC: %d, exponentSign HEX %x\n",
            exponentSign, exponentSign)
            printf("[FloatingPointFusedMultiplication] exponentAbsoluteValue DEC: %d, exponentAbsoluteValue HEX %x\n",
            exponentAbsoluteValue, exponentAbsoluteValue)
            printf("[FloatingPointFusedMultiplication] mantissaBits DEC: %d, mantissaBits HEX %x\n",
            mantissaBits, mantissaBits)
            printf("[FloatingPointFusedMultiplication] shiftedBits DEC: %d, shiftedBits HEX %x\n",
            shiftedBits, shiftedBits)
        }
    }
}

class IntegerToFloatingPoint(
    integerSize: Int,
    exponentSize: Int,
    fractionSize: Int,
    softwareDebug: Boolean = false,
) extends Module {
    val io = IO(new Bundle {
        val integer = Input(UInt(integerSize.W))
        val sign = Input(Bool())
        val result = Output(new FloatingPoint(exponentSize, fractionSize))
    })

    io.result.sign := Mux(io.sign, io.integer(integerSize - 1), false.B)
    io.result.zero := false.B
    io.result.inf := false.B
    io.result.nan := false.B
    io.result.underflow := false.B
    io.result.overflow := false.B
    io.result.exponentSign := false.B
    io.result.exponentAbsoluteValue := 0.U
    io.result.mantissa := 0.U
    io.result.restBits := 0.U
    
    when(io.integer === 0.U) {
        io.result.zero := true.B
    }.otherwise {
        val absoluteValue = Mux(io.sign && io.integer(integerSize - 1), 
                               (~io.integer).asUInt + 1.U, 
                               io.integer)

        val leadingOne = PriorityEncoder(Reverse(absoluteValue))
        val msbPosition = integerSize.U - 1.U - leadingOne

        val normalizedValue = absoluteValue << (integerSize.U - 1.U - msbPosition)

        io.result.exponentAbsoluteValue := msbPosition

        if (fractionSize + 1 >= integerSize) {
            io.result.mantissa := Cat(1.U(1.W), normalizedValue(integerSize-2, 0), 0.U((fractionSize + 1 - integerSize).W))
        } else {
            io.result.mantissa := Cat(1.U(1.W), normalizedValue(integerSize-2, integerSize-fractionSize-1))
            val roundPosition = integerSize - fractionSize - 2
            if (roundPosition >= 0) {
                val g = normalizedValue(roundPosition)
                val r = if (roundPosition > 0) normalizedValue(roundPosition-1) else false.B
                val s = if (roundPosition > 1) normalizedValue(roundPosition-2, 0).orR else false.B
                io.result.restBits := Cat(g, r, s)
            }
        }

        if (softwareDebug) {
            printf("[IntegerToFloatingPoint] input integer: %d\n", io.integer)
            printf("[IntegerToFloatingPoint] sign: %d\n", io.result.sign)
            printf("[IntegerToFloatingPoint] exponent: %d\n", io.result.exponentAbsoluteValue)
            printf("[IntegerToFloatingPoint] mantissa: 0x%x\n", io.result.mantissa)
            printf("[IntegerToFloatingPoint] restBits: 0x%x\n", io.result.restBits)
        }
    }
}

class FloatingPointToInteger(
    integerSize: Int,
    exponentSize: Int,
    fractionSize: Int,
    softwareDebug: Boolean = false,
) extends Module {
    val io = IO(new Bundle {
        val floatingPoint = Input(new FloatingPoint(exponentSize, fractionSize))
        val roundingType = Input(UInt(3.W))
        val integer = Output(UInt(integerSize.W))
    })

    io.integer := 1.U

    when(io.floatingPoint.nan || io.floatingPoint.inf) {
        io.integer := (1.U << (integerSize - 1))
    } .elsewhen(io.floatingPoint.zero) {
        io.integer := 0.U
    } .otherwise {
        val absoluteExponent = io.floatingPoint.exponentAbsoluteValue

        when(absoluteExponent >= integerSize.U - 2.U && !io.floatingPoint.exponentSign) {
            io.integer := Mux(io.floatingPoint.sign, 
                              (1.U << (integerSize - 1)), 
                              Fill(integerSize - 1, 1.U))
        } .elsewhen(io.floatingPoint.exponentSign) {

            val shiftedMantissa = io.floatingPoint.mantissa >> absoluteExponent
            
            val roundingLogic = Module(new FloatingPointRounding(softwareDebug))
            roundingLogic.io.l := shiftedMantissa(io.floatingPoint.mantissa.getWidth - 1)
            roundingLogic.io.g := shiftedMantissa(io.floatingPoint.mantissa.getWidth - 2)
            roundingLogic.io.r := shiftedMantissa(io.floatingPoint.mantissa.getWidth - 3)
            roundingLogic.io.s := shiftedMantissa(io.floatingPoint.mantissa.getWidth - 4, 0).orR
            roundingLogic.io.sign := io.floatingPoint.sign
            roundingLogic.io.rounding := io.roundingType
            
            val intResult = Mux(roundingLogic.io.addOne, 1.U, 0.U)
            
            io.integer := Mux(io.floatingPoint.sign,
                             ~(intResult.pad(integerSize)) + 1.U,
                             intResult.pad(integerSize))
        } .otherwise {
            val shiftAmount = Mux(absoluteExponent >= integerSize.U, 
                         integerSize.U - 1.U,
                         absoluteExponent)

            val shiftedMantissa = io.floatingPoint.mantissa << shiftAmount

            val integerPart = shiftedMantissa >> fractionSize.U

            val roundingLogic = Module(new FloatingPointRounding(softwareDebug))

            roundingLogic.io.l := integerPart(0)
            roundingLogic.io.g := shiftedMantissa(fractionSize - 1)
            roundingLogic.io.r := shiftedMantissa(fractionSize - 2)
            roundingLogic.io.s := shiftedMantissa(fractionSize - 3, 0).orR
            roundingLogic.io.sign := io.floatingPoint.sign
            roundingLogic.io.rounding := io.roundingType

            val roundedInteger = Mux(roundingLogic.io.addOne, 
                                    integerPart + 1.U, 
                                    integerPart)

            val resultInteger = Mux(io.floatingPoint.sign,
                                ~roundedInteger + 1.U,
                                roundedInteger)

            io.integer := resultInteger(integerSize - 1, 0)

            if (softwareDebug) {
                printf("[FloatingPointToInteger] shiftAmount: %d\n", shiftAmount)
                printf("[FloatingPointToInteger] shiftedMantissa: 0x%x\n", shiftedMantissa)
                printf("[FloatingPointToInteger] integerPart: 0x%x\n", integerPart)
                printf("[FloatingPointToInteger] roundedInteger: 0x%x\n", roundedInteger)
            }
        }

        if (softwareDebug) {
            printf("[FloatingPointToInteger] absoluteExponent: %d\n", absoluteExponent)
        }
    }

    if (softwareDebug) {
        printf("[FloatingPointToInteger] input floating point: 0x%x\n", io.floatingPoint.asUInt)
        printf("[FloatingPointToInteger] sign: %d\n", io.floatingPoint.sign)
        printf("[FloatingPointToInteger] exponent: %d\n", io.floatingPoint.exponentAbsoluteValue)
        printf("[FloatingPointToInteger] mantissa: 0x%x\n", io.floatingPoint.mantissa)
        printf("[FloatingPointToInteger] integer output: 0x%x\n", io.integer)
    }
}