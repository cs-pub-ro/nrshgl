package ro.upb.nrs.hgl

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.ChiselScalatestTester
import chisel3.util._

import ro.upb.nrs.sl
import ro.upb.nrs.benchmark


class TestSingleGFPU extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "test single operation on GFPU"
    val exponentSize = 4
    val fractionSize = 3
    val size = exponentSize + fractionSize + 1
    val nrss : List[FloatingPointTrait] = List(
        FixedFloatingPoint(exponentSize, fractionSize, RoundEven, false),
        IEEE754(exponentSize, fractionSize, RoundEven, false)
    )
    val testNrss : List[ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem] = List(
        ro.upb.nrs.sl.FixedFloatingPoint(0.0d, exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven),
        ro.upb.nrs.sl.IEEE754(0.0d, exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)
    )

    it should "add 2 values" in {
        test(new GFPU(
                        size,
                        nrss,
                        false
            )) { c =>
            val op1Type : Int = 0
            val op2Type : Int = 1
            val resultType : Int = 1
            val operationType : Int = 0
            val a : Int = 0
            val b : Int = 119
            val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(a).binaryEncode))
            val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, testNrss(op1Type))
            val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(b).binaryEncode))
            val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, testNrss(op2Type))
            c.io.operand1Value.poke(a.U(size.W))
            c.io.operand1Type.poke(op1Type.U(log2Ceil(nrss.length).W))
            c.io.operand2Value.poke(b.U(size.W))
            c.io.operand2Type.poke(op2Type.U(log2Ceil(nrss.length).W))
            c.io.resultType.poke(resultType.U(log2Ceil(nrss.length).W))
            c.io.operationType.poke(operationType.U(2.W))
			c.clock.step()
            val result : ro.upb.nrs.sl.RationalNumber_B = operationType match {
                case 0 => (value1_nrs.toRationalNumber + value2_nrs.toRationalNumber)
                case 1 => (value1_nrs.toRationalNumber - value2_nrs.toRationalNumber)
                case 2 => (value1_nrs.toRationalNumber * value2_nrs.toRationalNumber)
                case 3 => (value1_nrs.toRationalNumber / value2_nrs.toRationalNumber)
                case _ => value1_nrs.toRationalNumber
            }
            val result_nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromRationalNumber(result, testNrss(resultType)) match {
                case that : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem => that
                case _ => throw new Error("not a fixed precision number")
            }
            println("Value1 " + value1_nrs)
            println("Value2 " + value2_nrs)
            println("Result " + (value1_nrs.toRationalNumber + value2_nrs.toRationalNumber))
            println("Result " + result_nrs)
            c.io.resultValue.expect(("b" + result_nrs.toBinaryString).U(size.W))
        }
    }
}


class TestExhaustiveGFPU extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "exahustive testing of the GFPU"
    val exponentSize = 4
    val fractionSize = 3
    val size = exponentSize + fractionSize + 1
    val nrss : List[FloatingPointTrait] = List(
        FixedFloatingPoint(exponentSize, fractionSize, RoundEven, false),
        IEEE754(exponentSize, fractionSize, RoundEven, false)
    )
    val testNrss : List[ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem] = List(
        ro.upb.nrs.sl.FixedFloatingPoint(0.0d, exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven),
        ro.upb.nrs.sl.IEEE754(0.0d, exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)
    )
    val nops = 3


    it should "add 2 values" in {
        test(new GFPU(
                        size,
                        nrss,
                        false
            )) { c =>
            for(operationType <- 0 until nops) {
                for(op1Type <- 0 until nrss.length) {
                    for(op2Type <- 0 until nrss.length) {
                        for(resultType <- 0 until nrss.length) {
                            for( operand1 <- 0 until (1<<size)) {
                                for( operand2 <- 0 until (1<<size)) {
                                    val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(operand1).binaryEncode))
                                    val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, testNrss(op1Type))
                                    val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(operand2).binaryEncode))
                                    val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, testNrss(op2Type))
                                    c.io.operand1Value.poke(operand1.U(size.W))
                                    c.io.operand1Type.poke(op1Type.U(log2Ceil(nrss.length).W))
                                    c.io.operand2Value.poke(operand2.U(size.W))
                                    c.io.operand2Type.poke(op2Type.U(log2Ceil(nrss.length).W))
                                    c.io.resultType.poke(resultType.U(log2Ceil(nrss.length).W))
                                    c.io.operationType.poke(operationType.U(2.W))
                                    c.clock.step()
                                    val result : ro.upb.nrs.sl.RationalNumber_B = operationType match {
                                        case 0 => (value1_nrs.toRationalNumber + value2_nrs.toRationalNumber)
                                        case 1 => (value1_nrs.toRationalNumber - value2_nrs.toRationalNumber)
                                        case 2 => (value1_nrs.toRationalNumber * value2_nrs.toRationalNumber)
                                        case 3 => (value1_nrs.toRationalNumber / value2_nrs.toRationalNumber)
                                        case _ => value1_nrs.toRationalNumber
                                    }
                                    val result_nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromRationalNumber(result, testNrss(resultType)) match {
                                        case that : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem => that
                                        case _ => throw new Error("not a fixed precision number")
                                    }
                                    println("Value1 " + value1_nrs)
                                    println("Value2 " + value2_nrs)
                                    println("Result " + (value1_nrs.toRationalNumber + value2_nrs.toRationalNumber))
                                    println("Result " + result_nrs)
                                    c.io.resultValue.expect(("b" + result_nrs.toBinaryString).U(size.W))

                                }
                            }
                        }
                    }
                }
            }
        }
    }
}