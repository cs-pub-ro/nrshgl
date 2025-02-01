package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.util._

/*
The module has 
as input
two posit numbers in binary representation
and op_sel operator selection 0 for add and 1 for sub
as ouput
the result posit number as binary representation

x.signum is cosider -1 for x < 0 and 1 for x > 1, for x == 0 we consider 1 even
if the theoritical value is 0

|x| = absolute value of x

op1 = first operand considered
op2 = second operand considered
We need to respect the next proprietry op1 >= op2 and op1,op2 >=0
res = resutl and will have the sign calculate separate
op_neg = if the second operand must be negated ussualy when we do subtraction

res = a op_sel b = |a| *  a.signum op_sel  |b| * b.signum
if(a.signum == b.signum)
    res = (|a| op_sel |b|) * a.signum
    if(|a| >= |b|)
        res = (|a| op_sel |b|) * a.signum
        op1 = |a|
        op2 = |b|
        op_neg = op_sel
        res.sign = a.sign
    else
        If(op_sel == 0) //+
            res = (|b| + |a|) * a.signum
            op1 = |b|
            op2 = |a|
            op_neg = false
            res.sign = a.sign
        else //-
            res = (|b| - |a|) * -a.signum
            op1 = |b|
            op2 = |a|
            op_neg = true
            res.sign = -a.sign
else // b.signum = -a.signum
    res = (|a| op_sel -|b|) * a.signum
    if(|a| >= |b|)
        res = (|a| op_sel -|b|) * a.signum
        op1 = |a|
        op2 = |b|
        res.sign = a.sign
        If(op_sel == 0) //+
            res = (|a| - |b|) * a.signum
            op_neg = true
        else //-
            res = (|a| + |a|) * a.signum
            op_neg = false
    else 
        If(op_sel == 0) //+
            res = (|b| - |a|) * -a.signum
            op1 = |b|
            op2 = |a|
            op_neg = true
            res.sign = -a.sign
        else //-
            res = (|b| + |a|) * a.signum
            op1 = |b|
            op2 = |a|
            op_neg = false
            res.sign = a.sign

we can consider
abs_ge = |a| >= |b|
sign_equal = a.signum == b.signum

we separate op_neg res.sign and op1 and op2
op1 = if(abs_ge) |a| else |b|
op2 = if(abs_ge) |b| else |a|
if(sign_equal)
    if(abs_ge)
        op_neg = op_sel
        res.sign = a.sign
    else
        If(op_sel == 0) //+
            op_neg = false // op_sel
            res.sign = a.sign
        else //-
            op_neg = true // op_sel
            res.sign = -a.sign
else
    if(abs_ge)
        res.sign = a.sign
        If(op_sel == 0) //+
            op_neg = true // !op_sel
        else //-
            op_neg = false // !op_sel
    else 
        If(op_sel == 0) //+
            op_neg = true // !op_sel
            res.sign = -a.sign
        else //-
            op_neg = false // !op_sel
            res.sign = a.sign

we see that when sign_equal is true than op_neg = op_sel otherwise op_neg=!op_sel so
op_neg = !sign_equal ^ op_sel

majority of cases res.sign = a.sign, but when abs_ge is false and op_neg is true
res.sign = (op_neg & !abs_ge) ^ a.sign
logical because when we ave op_neg we do a substraction of absolute values
and when abs_ge is false it means that we change the sign of the result.

the operation next
exponent1 >= exponent2
(-1)^sign * 2^exponent1 * ( mantisa1 / 2^fraction_size1 ) op_sel (-1)^sign * 2^exponent2 * ( mantisa2 / 2^fraction_size2 ) =
= (-1)^sign * 2^exponent1 * ( (mantisa1 * 2^fraction_size2) / 2^(fraction_size1 + fraction_size2) ) op_sel (-1)^sign * 2^exponent2 * ( (mantisa2 * 2^fraction_size1) / 2^(fraction_size2 + fraction_size1) ) =
= (-1)^sign * 2^exponent1 * ( (mantisa1 * 2^fraction_size2) op_sel 2^(exponent2 - exponent1) * (mantisa2 * 2^fraction_size1) ) / 2^(fraction_size2 + fraction_size1) =
= (-1)^sign * 2^exponent1 * ( (mantisa1 << fraction_size2) op_sel ( (mantisa2 << fraction_size1) / 2^(exponent1 - exponent2) ) ) / 2^(fraction_size2 + fraction_size1) =
= (-1)^sign * 2^exponent1 * ( (mantisa1 << fraction_size2) op_sel ( (mantisa2 << fraction_size1) >> (exponent1 - exponent2) ) ) / 2^(fraction_size2 + fraction_size1) =
= res.sign * 2^exponent1 * ( (mantisa1 << fraction_size2) + mantissa3 ) / 2^(fraction_size2 + fraction_size1) =
op_neg = true -> mantissa3 = ~( (mantisa2 << fraction_size1) >> (exponent1 - exponent2) ) + 1
         false -> mantissa3 = ( (mantisa2 << fraction_size1) >> (exponent1 - exponent2) )

*/
class PositAddSub(exponent_size: Int, size: Int) extends Module {
    val io = IO(new Bundle {
        val op1 = Input(UInt(size.W))
        val op2 = Input(UInt(size.W))
        val op_sel = Input(Bool())
        val res = Output(UInt(size.W))
    })

    /*
    Two decoder for input
    and one normaliser and encoder for ouput
    */
    val decode_op1 = Module(new DecodePosit(exponent_size, size))
    val decode_op2 = Module(new DecodePosit(exponent_size, size))
    val encode_res = Module(new NormalisePosit(exponent_size, size))

    /*
    decode de inputs
    */
    decode_op1.io.binary := io.op1
    decode_op2.io.binary := io.op2

    //sign equal
    val sign_equal = Wire(Bool())
    sign_equal := !(decode_op1.io.posit.sign ^ decode_op2.io.posit.sign) //== 0 if != 1
    /*
    abs_ge if a.exponent > b.exponent or if a.exponent=b.exponent and a.mantisa>=b.mantisa
    */
    val abs_ge = Wire(Bool())
    abs_ge := (decode_op1.io.posit.exponent > decode_op2.io.posit.exponent) || ( (decode_op1.io.posit.exponent > decode_op2.io.posit.exponent) && (decode_op1.io.posit.mantissa >= decode_op2.io.posit.mantissa) )
    /*
    op_neg = !sign_equal ^ op_sel
    */
    val op_neg = Wire(Bool())
    op_neg := !sign_equal ^ io.op_sel

    //printf("[ADDSUB] sign_equal: %d, abs_ge: %d, op_neg: %d\n", sign_equal, abs_ge, op_neg)

    /*
    op1 = if(abs_ge) |a| else |b|
    op2 = if(abs_ge) |b| else |a|
    */
    val operand_1 = Mux(abs_ge, decode_op1.io.posit, decode_op2.io.posit)
    val operand_2 = Mux(abs_ge, decode_op2.io.posit, decode_op1.io.posit)
    //printf("[ADDSUB] operand_1.exponent: %d, operand_2.exponent: %d\n", operand_1.exponent, operand_2.exponent)

    //res.sign = (op_neg & !abs_ge) ^ a.sign
    encode_res.io.posit.sign :=  (op_neg & !abs_ge ) ^ decode_op1.io.posit.sign
    //printf("[ADDSUB] encode_res.io.posit.sign: %d\n", encode_res.io.posit.sign)
    // res.sign * 2^exponent1 * ( (mantisa1 << fraction_size2) + mantissa3 ) / 2^(fraction_size2 + fraction_size1) =
    encode_res.io.posit.exponent :=  operand_1.exponent

    /*
    res.sign * 2^exponent1 * ( (mantisa1 << fraction_size2) + mantissa3 ) / 2^(fraction_size2 + fraction_size1) =
    op_neg = true -> mantissa3 = ~( (mantisa2 << fraction_size1) >> (exponent1 - exponent2) ) + 1
         false -> mantissa3 = ( (mantisa2 << fraction_size1) >> (exponent1 - exponent2) )

    for the internal posit we use internal register of double size and one extra bit for overflow
    */
    //mantissa1 = mantisa1 << fraction_size2
    val mantissa1 = Wire(UInt((2 * size + 1).W))
    //mantissa2_partial = (mantisa2 << fraction_size1)
    val mantissa2_partial = Wire(UInt((2 * size + 1).W))
    //mantissa2 = mantissa2_partial >> (exponent1 - exponent2)
    val mantissa2 = Wire(UInt((2 * size + 1).W))
    //mantisa1 = mantisa1 << (2 * size - 2 - fraction_size2)
    mantissa1 := ( operand_1.mantissa << ( (2 * size -2).U - operand_1.fraction_size ) )
    //mantissa2_partial = mantisa2 << (2 * size - 2 - fraction_size1)
    mantissa2_partial := ( operand_2.mantissa << ( (2 * size -2).U - operand_2.fraction_size ) )
    //mantissa2 = mantissa2_partial >> (exponent1 - exponent2)
    mantissa2 := mantissa2_partial >> (operand_1.exponent - operand_2.exponent).asUInt
    /*
    are the bits that were posibble taken outside by the restbits
    */
    encode_res.io.posit.restBits :=  !( (mantissa2_partial & ( ( 1.U << (operand_1.exponent - operand_2.exponent).asUInt ) - 1.U)) === 0.U )
    //printf("[ADDSUB] mantissa1: %x, mantissa2_partial: %x, mantissa2: %x\n", mantissa1, mantissa2_partial, mantissa2)

    /*
    op_neg = true -> mantissa1 + ~mantissa2 + 1  // mantissa1 - mantissa2
         false ->  mantissa1 + mantissa2

    */
    encode_res.io.posit.mantissa :=  Mux(op_neg, mantissa1 + ~mantissa2 + 1.U, mantissa1 + mantissa2)
    //printf("[ADDSUB] mantissa1 + mantisa2: %x, mantissa1 + ~mantissa2 + 1.U: %x\n", mantissa1 + mantissa2, mantissa1 + ~mantissa2 + 1.U)
    encode_res.io.posit.fraction_size :=  (2*size-2).U
    // nar op_sel a or a op_sel nar
    encode_res.io.posit.nar :=  decode_op1.io.posit.nar | decode_op2.io.posit.nar
    // zero op_sel zero
    encode_res.io.posit.zero :=  decode_op1.io.posit.zero & decode_op2.io.posit.zero

    io.res := encode_res.io.binary
}
