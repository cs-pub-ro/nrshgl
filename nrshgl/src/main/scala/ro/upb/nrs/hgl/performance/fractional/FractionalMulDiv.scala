package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.util._

/*
The module has 
as input
two posit numbers in binary representation
and op_sel operator selection 0 for mul and 1 for div
as ouput
the result fractional number as binary representation

mul 
  (a / b) * (c / d) = ( a * c ) / (b * d)
div
  (a / b) / (c / d) = ( a * d ) / (b * c)

*/
class FractionalMulDiv(size_numerator: Int, size_denominator: Int) extends Module {
    val io = IO(new Bundle {
        val op1 = Input(UInt((size_numerator + size_denominator + 1).W))
        val op2 = Input(UInt((size_numerator + size_denominator + 1).W))
        val op_sel = Input(Bool())
        val res = Output(UInt((size_numerator + size_denominator + 1).W))
    })
    val max_size = if(size_numerator > size_denominator) size_numerator else size_denominator
    /*
    Two decoder for input
    and one normaliser and encoder for ouput
    */
    val decode_op1 = Module(new DecodeFractional(size_numerator, size_denominator))
    val decode_op2 = Module(new DecodeFractional(size_numerator, size_denominator))
    val encode_res = Module(new NormaliseEncodeFractional(size_numerator, size_denominator))

    /*
    decode de inputs
    */
    decode_op1.io.binary := io.op1
    decode_op2.io.binary := io.op2


    //sign
    encode_res.io.fractional.sign :=  decode_op1.io.fractional.sign ^ decode_op2.io.fractional.sign
    //printf("[MULDIV] encode_res.io.fractional.sign: %d\n", encode_res.io.fractional.sign)
    /*
    mul 
        (a / b) * (c / d) = ( a * c ) / (b * d)
    div
        (a / b) / (c / d) = ( a * d ) / (b * c)
    */
    encode_res.io.fractional.numerator :=  Mux(
                                                !io.op_sel,
                                                decode_op1.io.fractional.numerator * decode_op2.io.fractional.numerator,
                                                decode_op1.io.fractional.numerator * decode_op2.io.fractional.denominator
                                            )
    encode_res.io.fractional.denominator := Mux(
                                                !io.op_sel,
                                                decode_op1.io.fractional.denominator * decode_op2.io.fractional.denominator,
                                                decode_op1.io.fractional.denominator * decode_op2.io.fractional.numerator
                                            )
    //printf("[MULDIV] encode_res.io.fractional.numerator: %d, encode_res.io.fractional.denominator: %d\n", encode_res.io.fractional.numerator, encode_res.io.fractional.denominator)
    encode_res.io.fractional.nar := Mux(
                                            !io.op_sel,
                                            decode_op1.io.fractional.nar | decode_op2.io.fractional.nar,
                                            decode_op1.io.fractional.nar | decode_op2.io.fractional.zero
                                        )
    encode_res.io.fractional.zero := Mux(
                                            !io.op_sel,
                                            decode_op1.io.fractional.zero | decode_op2.io.fractional.zero,
                                            decode_op1.io.fractional.zero | decode_op2.io.fractional.nar
                                        )
    //printf("[MULDIV] encode_res.io.fractional.nar: %d, encode_res.io.fractional.zero: %d\n", encode_res.io.fractional.nar, encode_res.io.fractional.zero)
    
    encode_res.io.fractional.binary := 0.U

    io.res := encode_res.io.binary
}
