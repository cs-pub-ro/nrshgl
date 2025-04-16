package ro.upb.nrs.hgl

import chisel3._
import chisel3.tester._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.ChiselScalatestTester
import chisel3.util._

import ro.upb.nrs.sl
import ro.upb.nrs.benchmark


class TestSingleTFPU extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "test single operation on TFPU"
    val exponentSize = 4
    val fractionSize = 3
    val size = exponentSize + fractionSize + 1
    val nrs : FloatingPointTrait = FixedFloatingPoint(exponentSize, fractionSize, RoundEven, false)
    val testNrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.FixedFloatingPoint(0.0d, exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)

    it should "add 2 values" in {
        test(new TFPU(
                        nrs,
                        false
            )) { c =>
            val operationType : Int = 0
            val a : Int = 64
            val b : Int = 64
            val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(a).binaryEncode))
            val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, testNrs)
            val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(b).binaryEncode))
            val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, testNrs)
            c.io.operand1Value.poke(a.U(size.W))
            c.io.operand2Value.poke(b.U(size.W))
            c.io.operationType.poke(operationType.U(2.W))
			c.clock.step()
            val result : ro.upb.nrs.sl.NumberRepresentationSystem = operationType match {
                case 0 => (value1_nrs + value2_nrs)
                case 1 => (value1_nrs - value2_nrs)
                case 2 => (value1_nrs * value2_nrs)
                case 3 => (value1_nrs / value2_nrs)
                case _ => value1_nrs
            }
            val result_nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = result match {
                case that : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem => that
                case _ => throw new Error("not a fixed precision number")
            }
            println("Value1 " + value1_nrs)
            println("Value2 " + value2_nrs)
            println("Result " + result)
            println("Result " + result_nrs)
            c.io.resultValue.expect(("b" + result_nrs.toBinaryString).U(size.W))
        }
    }
}


class TestExhaustiveTFPU extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "exahustive testing of the TFPU"
    val exponentSize = 4
    val fractionSize = 3
    val size = exponentSize + fractionSize + 1
    val nrs : FloatingPointTrait = FixedFloatingPoint(exponentSize, fractionSize, RoundEven, false)
    val testNrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.FixedFloatingPoint(0.0d, exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)
    val nops = 3


    it should "add 2 values" in {
        test(new TFPU(
                        nrs,
                        false
            )) { c =>
            for(operationType <- 0 until nops) {
                for( operand1 <- 0 until (1<<size)) {
                    for( operand2 <- 0 until (1<<size)) {
                        val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(operand1).binaryEncode))
                        val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, testNrs)
                        val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(operand2).binaryEncode))
                        val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, testNrs)
                        c.io.operand1Value.poke(operand1.U(size.W))
                        c.io.operand2Value.poke(operand2.U(size.W))
                        c.io.operationType.poke(operationType.U(2.W))
                        c.clock.step()
                        val result : ro.upb.nrs.sl.NumberRepresentationSystem = operationType match {
                            case 0 => (value1_nrs + value2_nrs)
                            case 1 => (value1_nrs - value2_nrs)
                            case 2 => (value1_nrs * value2_nrs)
                            case 3 => (value1_nrs / value2_nrs)
                            case _ => value1_nrs
                        }
                        val result_nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = result match {
                            case that : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem => that
                            case _ => throw new Error("not a fixed precision number")
                        }
                        println("Value1 " + value1_nrs)
                        println("Value2 " + value2_nrs)
                        println("Result " + (value1_nrs + value2_nrs))
                        println("Result " + result_nrs)
                        c.io.resultValue.expect(("b" + result_nrs.toBinaryString).U(size.W))

                    }
                }
            }
        }
    }
}