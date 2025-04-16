package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.util._

/*
We consider Fractional
(-1)^sign * numerator / denominator
size_numerator and size_denominator
total size = size_numerator + size_denominator + 1
there are 2 special numbers:
NAR - not a real (+-infinity) 1 / 0
Zero - 0 numerator = 0 / 1
*/
class NormalFractional(size_numerator: Int, size_denominator: Int) extends Bundle {
    //binary representation
	val binary = Bits((size_numerator + size_denominator + 1).W)
    //sign
	val sign = Bool()
    //numerator
	val numerator = UInt(size_numerator.W)
    //denominator
	val denominator = UInt(size_denominator.W)
    //is a nar ?
	val nar = Bool()
    //is zero ?
	val zero = Bool()
}

object NormalFractional {
    def apply(size : Int) : NormalFractional = if (size %  2 == 0) new NormalFractional(size/2 - 1, size/2) else new NormalFractional(size/2, size/2)
    def apply(size_numerator: Int, size_denominator: Int) : NormalFractional = new NormalFractional(size_numerator, size_denominator)
}

// IO for decoding and encoding
class NormalFractionalIO(size_numerator: Int, size_denominator: Int) extends Bundle {
    val binary = Input(Bits((size_numerator + size_denominator + 1).W))
    val fractional = Output(NormalFractional(size_numerator, size_denominator))
}

object NormalFractionalIO {
    def apply(size : Int) : NormalFractionalIO = if (size %  2 == 0) new NormalFractionalIO(size/2 - 1, size/2) else new NormalFractionalIO(size/2, size/2)
    def apply(size_numerator: Int, size_denominator: Int) : NormalFractionalIO = new NormalFractionalIO(size_numerator, size_denominator)
}


class InternalFractionalIO(size_numerator: Int, size_denominator: Int) extends Bundle { 
    val max_size = if(size_numerator > size_denominator) size_numerator else size_denominator
    val binary = Output(Bits((size_numerator + size_denominator + 1).W))
    val fractional = Input(NormalFractional(2 * max_size, 2 * max_size))
}

object InternalFractionalIO {
    def apply(size : Int) : InternalFractionalIO = if (size %  2 == 0) new InternalFractionalIO(size/2 - 1, size/2) else new InternalFractionalIO(size/2, size/2)
    def apply(size_numerator: Int, size_denominator: Int) : InternalFractionalIO = new InternalFractionalIO(size_numerator, size_denominator)
}


class DecodeFractional(size_numerator: Int, size_denominator: Int) extends Module {
	val io = IO(NormalFractionalIO(size_numerator, size_denominator))

    io.fractional.binary :=  io.binary
    io.fractional.sign := io.fractional.binary(size_numerator + size_denominator)
    //printf("[DECODE] io.fractional.binary: %d, io.fractional.sign: %d\n", io.fractional.binary, io.fractional.sign.asUInt)

    io.fractional.numerator := io.fractional.binary(size_numerator + size_denominator - 1, size_denominator)
    io.fractional.denominator := Mux(io.fractional.numerator === 0.U, 1.U, io.fractional.binary(size_denominator - 1, 0))
    //printf("[DECODE] io.fractional.numerator: %d, io.fractional.denominator: %d\n", io.fractional.numerator, io.fractional.denominator)

    //verivy the special binary pattern
    io.fractional.nar := io.fractional.binary(size_denominator - 1, 0) === 0.U
    io.fractional.zero := !io.fractional.nar && (io.fractional.numerator === 0.U)
    //printf("[DECODE] io.nar: %d, io.zero: %d\n", io.fractional.nar.asUInt, io.fractional.zero.asUInt)
}

class NormaliseEncodeFractional(size_numerator: Int, size_denominator: Int) extends Module {
	val io = IO(InternalFractionalIO(size_numerator, size_denominator))

    val max_size = if(size_numerator > size_denominator) size_numerator else size_denominator
    //gcd
    val internal_gcd = Module(new GCD(2 * max_size, 20))
    internal_gcd.io.value1 := io.fractional.numerator
    internal_gcd.io.value2 := io.fractional.denominator
    //printf("[ENCODE] io.fractional.numerator: %d, io.fractional.denominator: %d\n", io.fractional.numerator, io.fractional.denominator)

    //internal possible numerator and denominator
    val possible_numerator = Wire( UInt( (2 * max_size).W ) )
    val possible_denominator = Wire(UInt( (2 * max_size).W ))
    possible_numerator := io.fractional.numerator / internal_gcd.io.gcd
    possible_denominator := io.fractional.denominator / internal_gcd.io.gcd
    //printf("[ENCODE] possible_numerator: %d, possible_denominator: %d, internal_gcd.io.gcd: %d\n", possible_numerator, possible_denominator, internal_gcd.io.gcd)

    val numeratorFirstOne =  Wire( UInt( (log2Ceil(2 * max_size)).W ) )
    val denominatorFirstOne =  Wire( UInt( (log2Ceil(2 * max_size)).W ) )
    numeratorFirstOne := PriorityEncoder( Reverse(possible_numerator) )
    denominatorFirstOne := PriorityEncoder( Reverse(possible_denominator) )
    //printf("[ENCODE] numeratorFirstOne: %d, denominatorFirstOne: %d\n", numeratorFirstOne, denominatorFirstOne)

    val numeratorShiftRight = Wire( UInt( (log2Ceil(2 * max_size)).W ) )
    val denominatorShiftRight = Wire( UInt( (log2Ceil(2 * max_size)).W ) )
    numeratorShiftRight := Mux(numeratorFirstOne >= (2 * max_size - size_numerator).U, 0.U, (2 * max_size - size_numerator).U - numeratorFirstOne)
    denominatorShiftRight := Mux(denominatorFirstOne >= (2 * max_size - size_denominator).U, 0.U, (2 * max_size - size_denominator).U - denominatorFirstOne)
    //printf("[ENCODE] numeratorShiftRight: %d, denominatorShiftRight: %d\n", numeratorShiftRight, denominatorShiftRight)

    val maxShiftRight = Wire( UInt( (log2Ceil(2 * max_size)).W ) )
    maxShiftRight := Mux(numeratorShiftRight >= denominatorShiftRight, numeratorShiftRight, denominatorShiftRight)
    //printf("[ENCODE] maxShiftRight: %d\n", maxShiftRight)

    val possible_binary_numerator = Wire(UInt( (size_numerator + size_denominator + 1).W ))
    val binary_numerator = Wire(UInt( (size_numerator + size_denominator + 1).W ))
    val binary_denominator = Wire(UInt( (2 * max_size).W ))
    possible_binary_numerator := possible_numerator >> maxShiftRight
    binary_numerator := possible_binary_numerator << size_denominator
    binary_denominator := possible_denominator >> maxShiftRight
    //printf("[ENCODE] possible_binary_numerator: %d, binary_numerator: %x, binary_denominator: %x\n", possible_binary_numerator, binary_numerator, binary_denominator)

    io.binary := Mux(
                        io.fractional.nar | (binary_denominator === 0.U),
                        1.U << size_denominator,
                        Mux(
                            io.fractional.zero | (binary_numerator === 0.U),
                            1.U,
                            ( (io.fractional.sign << (size_numerator + size_denominator)) | binary_numerator | binary_denominator )
                        )
                    )
    //printf("[ENCODE] io.binary: %d\n", io.binary)
}



class DecodeNormaliseEncodeFractional(size_numerator: Int, size_denominator: Int) extends Module {
	val io = IO(new Bundle {
            val a = Input(UInt((size_numerator + size_denominator + 1).W))
            val b = Output(UInt((size_numerator + size_denominator + 1).W))
        }
    )
    
    val f1 = Module(new DecodeFractional(size_numerator, size_denominator))
    val f2 = Module(new NormaliseEncodeFractional(size_numerator, size_denominator))
    f1.io.binary := io.a
    f2.io.fractional.sign := f1.io.fractional.sign
    f2.io.fractional.numerator := f1.io.fractional.numerator
    f2.io.fractional.denominator := f1.io.fractional.denominator
    f2.io.fractional.nar := f1.io.fractional.nar
    f2.io.fractional.zero := f1.io.fractional.zero
    f2.io.fractional.binary := f1.io.fractional.binary
    io.b := f2.io.binary
}