package ro.upb.nrs.hgl.performance

import chisel3._

  /*
  (-1)^sign1 * 2^exponent1 * ( mantisa1 / 2^fraction_size1 ) * (-1)^sign2 * 2^exponent2 * ( mantisa2 / 2^fraction_size2 ) =
  = (-1)^(sign1 xor sign2) * 2^(exponent1 + exponent2) *  (mantisa1 * mantisa2) / 2^(fraction_size1 + fraction_size2 )
  */
class PositMul(exponent_size: Int, size: Int) extends Module {
    val io = IO(new Bundle {
        val op1 = Input(UInt(size.W))
        val op2 = Input(UInt(size.W))
        val res = Output(UInt(size.W))
    })

    /*
    Two decoder for input
    and one normaliser and encoder for ouput
    */
    val decode_op1 = Module(new DecodePosit(exponent_size, size))
    val decode_op2 = Module(new DecodePosit(exponent_size, size))
    val encode_res = Module(new NormalisePosit(exponent_size, size))
    decode_op1.io.binary := io.op1
    decode_op2.io.binary := io.op2

    encode_res.io.posit.restBits :=  false.B
    //sign1 xor sign2
    encode_res.io.posit.sign :=  decode_op1.io.posit.sign ^ decode_op2.io.posit.sign
    //exponent1 + exponent2
    encode_res.io.posit.exponent :=  decode_op1.io.posit.exponent +& decode_op2.io.posit.exponent
    //mantisa1 * mantisa2
    encode_res.io.posit.mantissa :=  decode_op1.io.posit.mantissa * decode_op2.io.posit.mantissa
    //fraction_size1 + fraction_size2
    encode_res.io.posit.fraction_size :=  decode_op1.io.posit.fraction_size +& decode_op2.io.posit.fraction_size
    // nar * a or b * nar 
    encode_res.io.posit.nar :=  decode_op1.io.posit.nar | decode_op2.io.posit.nar
    // a * 0 or 0 * a
    encode_res.io.posit.zero :=  decode_op1.io.posit.zero | decode_op2.io.posit.zero

    io.res := encode_res.io.binary
}
