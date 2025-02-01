package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.experimental.FixedPoint

/*
  (value1 / 2^fraction_size) / (value2 / 2^fraction_size) =
  = (value1 / value2) =
  = ( (value1 / value2) * 2^fraction_size) / 2^fraction_size
  = ( ( (value1 * 2^fraction_size) / value2) ) / 2^fraction_size
  = ( ( (value1 << fraction_size) / value2) ) / 2^fraction_size
  */
class FixedPointQDiv(size: Int, fsize: Int) extends Module {
    val io = IO(new Bundle {
        val op1 = Input(FixedPoint(size.W, fsize.BP))
        val op2 = Input(FixedPoint(size.W, fsize.BP))
        val res = Output(FixedPoint(size.W, fsize.BP))
    })

    val op1_sign = Wire(UInt(1.W))
    op1_sign := io.op1.asUInt(size - 1)
    val op1_abs = Wire(UInt((size - 1).W))
    op1_abs := Mux(op1_sign === 1.U, ~io.op1.asUInt + 1.U(1.W), io.op1.asUInt)
    printf("[DIV] op1: %d, op1_sign: %d, op1_abs: %d\n", io.op1.asUInt, op1_sign, op1_abs)
    val op2_sign = Wire(UInt(1.W))
    op2_sign := io.op2.asUInt(size - 1)
    val op2_abs = Wire(UInt((size - 1).W))
    op2_abs := Mux(op2_sign === 1.U, ~io.op2.asUInt + 1.U(1.W), io.op2.asUInt)
    printf("[DIV] op2: %d, op2_sign: %d, op2_abs: %d\n", io.op2.asUInt, op2_sign, op2_abs)
    // take 2 extra bits for g and r bits -> size - 1 + 2 -> size + 1
    val div_result = Wire(UInt((size + 1).W))
    div_result := (op1_abs << (fsize + 2)) / op2_abs
    // remainder for s bit
    val remainder_result = Wire(UInt((size + 1).W))
    remainder_result := (op1_abs << (fsize + 2)) % op2_abs
    //eliminate extra bits
    val result_abs = Wire(UInt((size - 1).W))
    result_abs := div_result >> 2
    printf("[DIV] div_result: %d, remainder_result: %d, result_abs: %d\n", div_result, remainder_result, result_abs)
    val result_sign = Wire(UInt(1.W))
    //result sign
    result_sign := op1_sign ^ op2_sign
    val result_value = Wire(UInt(size.W))
    //result value if the sign is set (negative number) ~abs + 1 else abs
    result_value := Mux(result_sign === 1.U, ~(0.U(1.W) ## result_abs) + 1.U(size.W), result_abs)
    printf("[DIV] result_value: %d, result_sign: %d, result_abs: %d, ~result_abs: %d \n", result_value, result_sign, result_abs, ~(0.U(1.W) ## result_abs))
    //calculate the l,g,r,s rounding bits
    val l = Wire(UInt(1.W))
    l := result_value(0)
    val g = Wire(UInt(1.W))
    g := div_result(1)
    val r = Wire(UInt(1.W))
    r := div_result(0)
    val s = Wire(UInt(1.W))
    s := Mux(remainder_result === 0.U, 0.U, 1.U)
    printf("[DIV] l: %d, g: %d, r: %d, s: %d\n", l, g, r, s)
    //return the result TODO: maybe the value after rounding
    io.res := result_value.asFixedPoint(fsize.BP)
}
