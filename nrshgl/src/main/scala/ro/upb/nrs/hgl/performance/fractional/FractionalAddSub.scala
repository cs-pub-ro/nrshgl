package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.util._

/*
The module has 
as input
two fractional numbers in binary representation
and op_sel operator selection 0 for add and 1 for sub
as ouput
the result fractional number as binary representation

For fractional consider as (-1)^sign * numerator / denominator
x1 op_sel x2 = (-1)^sign1 * numerator1 / denominator1 opsel (-1)^sign2 * numerator2 / denominator2 =
= ( (-1)^sign1 * numerator1 * denominator2 op_sel (-1)^sign2 * numerator2 * denominator1 ) / ( denominator1 * denominator2 )

next we considered only numerator
( (-1)^sign1 * numerator1 * denominator2 op_sel (-1)^sign2 * numerator2 * denominator1 ) =
= ( (-1)^sign1 * element_1 op_sel (-1)^sign2 * element2 )
= x1 op_sel x2

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
op1 >= op2
( (-1)^sign1 * numerator1 * denominator2 op_sel (-1)^sign2 * numerator2 * denominator1 )
( (-1)^sign1 * element1 op_sel (-1)^sign2 * element2 )

op_neg = true -> element3 = element1 - element2
         false -> element3 = element1 + element2

*/
class FractionalAddSub(size_numerator: Int, size_denominator: Int) extends Module {
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

    //denominator1 * denominator2
    encode_res.io.fractional.denominator := decode_op1.io.fractional.denominator * decode_op2.io.fractional.denominator

    //sign equal
    val sign_equal = Wire(Bool())
    sign_equal := !(decode_op1.io.fractional.sign ^ decode_op2.io.fractional.sign) //== 0 if != 1
    /*
    element1 = numerator1 * denominator2
    element2 = numerator2 * denominator1
    */
	val element1 = Wire(UInt((2 * max_size).W))
	val element2 = Wire(UInt((2 * max_size).W))
    element1 := decode_op1.io.fractional.numerator * decode_op2.io.fractional.denominator
    element2 := decode_op2.io.fractional.numerator * decode_op1.io.fractional.denominator
    //printf("[ADDSUB] element1: %d, element2: %d, encode_res.io.fractional.denominator: %d\n", element1, element2, encode_res.io.fractional.denominator)

    /*
    abs_ge 
    */
    val abs_ge = Wire(Bool())
    abs_ge := (element1 >= element2)
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
    val operand_1 = Mux(abs_ge, element1, element2)
    val operand_2 = Mux(abs_ge, element2, element1)
    //printf("[ADDSUB] operand_1: %d, operand_2: %d\n", operand_1, operand_2)

    //res.sign = (op_neg & !abs_ge) ^ a.sign
    encode_res.io.fractional.sign :=  (op_neg & !abs_ge ) ^ decode_op1.io.fractional.sign
    //printf("[ADDSUB] encode_res.io.fractional.sign: %d\n", encode_res.io.fractional.sign)
    

    /*

    op_neg = true -> element3 = element1 - element2 = element1 + ~element2 + 1
            false -> element3 = element1 + element2
    */
    encode_res.io.fractional.numerator :=  Mux(op_neg, operand_1 + ~operand_2 + 1.U, operand_1 + operand_2)
    // nar op_sel a or a op_sel nar
    encode_res.io.fractional.nar :=  decode_op1.io.fractional.nar | decode_op2.io.fractional.nar
    // zero op_sel zero
    encode_res.io.fractional.zero :=  decode_op1.io.fractional.zero & decode_op2.io.fractional.zero
    //printf("[ADDSUB] encode_res.io.fractional.numerator: %d, encode_res.io.fractional.nar: %d, encode_res.io.fractional.zero: %d\n", encode_res.io.fractional.numerator, encode_res.io.fractional.nar, encode_res.io.fractional.zero)

    encode_res.io.fractional.binary := 0.U

    io.res := encode_res.io.binary
}
