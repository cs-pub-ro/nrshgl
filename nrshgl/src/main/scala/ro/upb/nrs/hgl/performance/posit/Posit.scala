package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.util._

/*
We consider Posit
(-1)^sign * 2^( (2^exponent_size)^regime + binary_exponent ) * ( mantisa / 2^fraction_size )
(-1)^sign * 2^exponent * ( mantisa / 2^fraction_size )
mantisa / 2^fraction_size in [1, 2)
size, exponent_size are fixed
mantisa has the hiddent bit that is always 1
there are 2 special numbers in posit:
NAR - not a real (+-infinity)
Zero - 0
*/
class NormalPosit(exponent_size: Int, size: Int) extends Bundle {
    //binary representation
	val binary = (Bits(size.W))
    //sign
	val sign = Bool()
    //exponent value as  (2^exponent_size)^regime + binary_exponent 
	val exponent = SInt((log2Ceil(size) + 1 + exponent_size).W)
    // mantisa with hidden bit fraction + (1 << fraction_size)
	val mantissa = UInt((size - 2).W)
    // fraction size
	val fraction_size = UInt(log2Ceil(size).W)
    //is a nar ?
	val nar = Bool()
    //is zero ?
	val zero = Bool()
}

// IO for decoding and encoding
class NormalPositIO(exponent_size: Int, size: Int) extends Bundle {
    val binary = Input(Bits(size.W))
    val posit = Output(new NormalPosit(exponent_size, size))
}


class DecodePosit(exponent_size: Int, size: Int) extends Module {
	val io = IO(new NormalPositIO(exponent_size, size))

    // binary into posit binary
    io.posit.binary := io.binary
    /*
    sign is the MSB of binary
    */
    io.posit.sign := io.posit.binary(size-1)

    /*
    rest of the bits
    the rest of bits are set for decoding
    If the sign is positive than we have to decode the LSB (size - 1) bits of ~binary + 1
    else we decode LSB (size - 1) bits of binary
    if(sign) decode ~binary + 1
    else decode binary


    */
    val bits_to_decode = Wire(UInt((size-1).W))
    
    /*
    sign is positive you decode de rest of the bits
    sign is negative you decode de two's complement of the rest of the bits
    two's complement of x is ~x plus 1
    */
    bits_to_decode := Mux(
                            io.posit.sign,
                            ~io.posit.binary( (size - 2), 0) + 1.U,
                            io.posit.binary( (size - 2), 0)
                        )
    
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
    val number_of_same_bit_value = Wire(UInt(log2Ceil(size).W))
    number_of_same_bit_value := Mux(
                                        bits_to_decode((size-2)),
                                        PriorityEncoder(Reverse(~bits_to_decode)),
                                        PriorityEncoder(Reverse(bits_to_decode))
                                    )
    //printf("[DECODE] io.sign: %d, bits_to_decode: %d, number_of_same_bit_value: %d\n", io.posit.sign.asUInt, bits_to_decode, number_of_same_bit_value)
    
    /*
    If the first bit was one than is a pozitive regime with the value:
    the number of consecutive ones minus one
    If the frist bit was zero than is a negative regime with the value:
    minus the number of consecutive zeros
    */
    val regime = Wire(SInt((log2Ceil(size) + 1).W))
    regime := Mux(
                    bits_to_decode((size-2)),
                    //if r is 1
                    Mux(
                        bits_to_decode(0)===1.U && number_of_same_bit_value===(size-2).U, //if all bits are 1. Eg: 0_111_1111
                        number_of_same_bit_value.zext,
                        number_of_same_bit_value.zext - 1.S //else number_of_same_bit_value - 1
                        ),
                    //if r is 0
                    Mux(
                        bits_to_decode(0)===0.U && number_of_same_bit_value===(size-2).U, //if all bits are 0. Eg: 0_000_0000
                        -number_of_same_bit_value.zext - 1.S,
                        -number_of_same_bit_value.zext // else -number_of_same_bit_value
                    )
                )
    

    val regime_size = Wire(UInt(log2Ceil(size).W))
    /*
    Size occupied by the regime is the number of the same value bit
    plus one (the bit that has a different value)
    */
    regime_size := number_of_same_bit_value + 1.U
    //printf("[DECODE] regime: %d, regime_size: %d\n", regime, regime_size)



    /*
    fraction size is the remaining bits after taking sign bit regime bits and exponent_size bits
    if positive otherwise zero. We calculated fraction size first because we can better take
    exponent bits of bits to decode. Mantisa will be the fraction bits plus the hidden bit 1
    as the MSB bit. done using 1 << fraction_size
    */
    /*
    calculate the number of fraction bits
    size - 1 (sign bit) - regime_size - max_exponent_size
    if the value is less than zero or zero than is zero
    */
    io.posit.fraction_size := Mux(
                                    regime_size >= (size - 1 - exponent_size).U,
                                    0.U,
                                    (size - 1 - exponent_size).U - regime_size
                                )
    /*
    If the fraction size is zero than the fraction value is zero
    else is the the least significant fraction_size bits
    */
    io.posit.mantissa := Mux(
                                io.posit.fraction_size === 0.U,
                                0.U,
                                ( (Fill(size, 1.U(1.W)) >> (size.U-io.posit.fraction_size)) & bits_to_decode)
                            ) | (1.U << io.posit.fraction_size)
    //printf("[DECODE] io.fraction_size: %d, io.mantissa: %d\n", io.posit.fraction_size, io.posit.mantissa)

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

    To calculate binary_exponent_size we have to look on how mant bits are remaining
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
    val binary_exponent_size = Wire(UInt(log2Ceil(size).W))
    binary_exponent_size := Mux(
                                regime_size >= (size - 1).U,
                                0.U,
                                Mux(
                                    regime_size > (size - 1 - exponent_size).U,
                                    (size - 1).U - regime_size,
                                    exponent_size.U
                                )
                            )

    /*
    If the thexponent size is zero than the exponent is zero,
    otherwise we sift right with fraction_size to eliminate the fraction bits
    and with a mask only for exponent_size
    */
    val binary_exponent = Wire(UInt(exponent_size.W))
    binary_exponent := Mux(
                            binary_exponent_size === 0.U,
                            0.U,
                            ( bits_to_decode >> (io.posit.fraction_size) ) & ( Fill(size, 1.U(1.W)) >> (size.U - binary_exponent_size) ) 
                        )
    //printf("[DECODE] binary_exponent_size: %d, binary_exponent: %d\n", binary_exponent_size, binary_exponent)

    /*
    the final exponent is (2^exponent_size) * regime + exponent =
    = regime << exponent_size + binary_exponent << (exponent_size - binary_exponent_size)
    */
    io.posit.exponent := (binary_exponent << (exponent_size.U - binary_exponent_size)).zext + (regime << exponent_size)
    //printf("[DECODE] exponent %d\n", io.posit.exponent)

    //verivy the special binary pattern
    io.posit.nar := io.posit.binary === (1.U << (size - 1))
    io.posit.zero := io.posit.binary === 0.U
    //printf("[DECODE] io.nar: %d, io.zero: %d\n", io.posit.nar.asUInt, io.posit.zero.asUInt)
}

class DecodeNormalisePosit(exponent_size: Int, size: Int) extends Module {
	val io = IO(new Bundle {
            val a = Input(UInt(size.W))
            val b = Output(UInt(size.W))
        }
    )
    
  val f1 = Module(new DecodePosit(exponent_size, size))
  val f2 = Module(new NormalisePosit(exponent_size, size))
  f1.io.binary := io.a
  f2.io.posit.sign := f1.io.posit.sign
  f2.io.posit.exponent := f1.io.posit.exponent
  f2.io.posit.mantissa := f1.io.posit.mantissa
  f2.io.posit.fraction_size := f1.io.posit.fraction_size
  f2.io.posit.nar := f1.io.posit.nar
  f2.io.posit.zero := f1.io.posit.zero
  f2.io.posit.restBits := false.B
  io.b := f2.io.binary
}