package ro.upb.nrs.hgl

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.ChiselScalatestTester

import ro.upb.nrs.sl
import ro.upb.nrs.benchmark


class TestAdditionSingleFixedFloatingPoint extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Addition on FixedFloatingPoint"
    val exponentSize = 4
    val fractionSize = 3
    val size = exponentSize + fractionSize + 1
    it should "add 2 values" in {
        test(new AdditionOperationModule(() => FixedFloatingPoint(exponentSize, fractionSize, RoundEven, true))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.FixedFloatingPoint(0.0d, exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)
            val a : Int = 0
            val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(a).binaryEncode))
            val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
            val b : Int = 119
            val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(b).binaryEncode))
            val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
            c.io.operand1.value.poke(a.U(size.W))
            c.io.operand2.value.poke(b.U(size.W))
			c.clock.step()
            val result_nrs : ro.upb.nrs.sl.FixedFloatingPoint_B = (value1_nrs + value2_nrs) match {
                case that : ro.upb.nrs.sl.FixedFloatingPoint_B => that
                case _ => ro.upb.nrs.sl.FixedFloatingPoint_NR(exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)
            }
            println("Value1 " + value1_nrs)
            println("Value2 " + value2_nrs)
            println("Result " + (value1_nrs + value2_nrs))
            println("Result " + result_nrs)
            c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
        }
    }
}

class TestAdditionExhaustiveFixedFloatingPoint extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Addition on FixedFloatingPoint"
    val exponentSize = 4
    val fractionSize = 3
    val size = exponentSize + fractionSize + 1

    it should "exhausting verification +/-" in {
        test(new AdditionOperationModule(() => FixedFloatingPoint(exponentSize, fractionSize, RoundEven, false))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.FixedFloatingPoint(0.0d, exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)
			for( i <- 0 until (1<<size)) {//
				for( j <- 0 until (1<<size)) {
                    val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(i).binaryEncode))
                    val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
                    val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(j).binaryEncode))
                    val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
                    c.io.operand1.value.poke(i.U(size.W))
                    c.io.operand2.value.poke(j.U(size.W))
                    c.clock.step()
                    val result_nrs : ro.upb.nrs.sl.FixedFloatingPoint_B = (value1_nrs + value2_nrs) match {
                        case that : ro.upb.nrs.sl.FixedFloatingPoint_B => that
                        case _ => ro.upb.nrs.sl.FixedFloatingPoint_NR(exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)
                    }
                    //println("i: " + i + " j: " + j)
                    c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
				}
			}
        }
    }
}


class TestMultiplicationSingleFixedFloatingPoint extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Multiplication on FixedFloatingPoint"
    val exponentSize = 4
    val fractionSize = 3
    val size = exponentSize + fractionSize + 1

    it should "multiply 2 values" in {
        test(new MultiplicationOperationModule(() => FixedFloatingPoint(exponentSize, fractionSize, RoundEven, true))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.FixedFloatingPoint(0.0d, exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)
            val a : Int = 114
            val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(a).binaryEncode))
            val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
            val b : Int = 125
            val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(b).binaryEncode))
            val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
            c.io.operand1.value.poke(a.U(size.W))
            c.io.operand2.value.poke(b.U(size.W))
			c.clock.step()
            val result_nrs : ro.upb.nrs.sl.FixedFloatingPoint_B = (value1_nrs * value2_nrs) match {
                case that : ro.upb.nrs.sl.FixedFloatingPoint_B => that
                case _ => ro.upb.nrs.sl.FixedFloatingPoint_NR(exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)
            }
            println("Value1 " + value1_nrs)
            println("Value2 " + value2_nrs)
            println("Result " + (value1_nrs * value2_nrs))
            println("Result " + result_nrs)
            c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
        }
    }
    
}


class TestMultiplicationExhaustiveFixedFloatingPoint extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Multiplication on FixedFloatingPoint"
    val exponentSize = 4
    val fractionSize = 3
    val size = exponentSize + fractionSize + 1

    it should "exhausting verification *" in {
        test(new MultiplicationOperationModule(() => FixedFloatingPoint(exponentSize, fractionSize, RoundEven, false))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.FixedFloatingPoint(0.0d, exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)
			for( i <- 0 until (1<<size)) {//
				for( j <- 0 until (1<<size)) {
                    val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(i).binaryEncode))
                    val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
                    val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(j).binaryEncode))
                    val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
                    c.io.operand1.value.poke(i.U(size.W))
                    c.io.operand2.value.poke(j.U(size.W))
                    c.clock.step()
                    val result_nrs : ro.upb.nrs.sl.FixedFloatingPoint_B = (value1_nrs * value2_nrs) match {
                        case that : ro.upb.nrs.sl.FixedFloatingPoint_B => that
                        case _ => ro.upb.nrs.sl.FixedFloatingPoint_NR(exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)
                    }
                    //println("i: " + i + " j: " + j)
                    c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
				}
			}
        }
    }
}


class TestDivisionSingleFixedFloatingPoint extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Division on FixedFloatingPoint"
    val exponentSize = 4
    val fractionSize = 3
    val size = exponentSize + fractionSize + 1

    it should "Division 2 values" in {
        test(new DivisionOperationModule(() => FixedFloatingPoint(exponentSize, fractionSize, RoundEven, true))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.FixedFloatingPoint(0.0d, exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)
            val a : Int = 128
            val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(a).binaryEncode))
            val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
            val b : Int = 128
            val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(b).binaryEncode))
            val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
            c.io.operand1.value.poke(a.U(size.W))
            c.io.operand2.value.poke(b.U(size.W))
			c.clock.step()
            val result_nrs : ro.upb.nrs.sl.FixedFloatingPoint_B = (value1_nrs / value2_nrs) match {
                case that : ro.upb.nrs.sl.FixedFloatingPoint_B => that
                case _ => ro.upb.nrs.sl.FixedFloatingPoint_NR(exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)
            }
            println("Value1 " + value1_nrs)
            println("Value2 " + value2_nrs)
            println("Result " + (value1_nrs / value2_nrs))
            println("Result " + result_nrs)
            c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
        }
    }
    
}


class TestDivisionExhaustiveFixedFloatingPoint extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Division on FixedFloatingPoint"
    val exponentSize = 4
    val fractionSize = 3
    val size = exponentSize + fractionSize + 1

    it should "exhausting verification /" in {
        test(new DivisionOperationModule(() => FixedFloatingPoint(exponentSize, fractionSize, RoundEven, false))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.FixedFloatingPoint(0.0d, exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)
			for( i <- 0 until (1<<size)) {//
				for( j <- 0 until (1<<size)) {
                    val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(i).binaryEncode))
                    val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
                    val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(j).binaryEncode))
                    val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
                    c.io.operand1.value.poke(i.U(size.W))
                    c.io.operand2.value.poke(j.U(size.W))
                    c.clock.step()
                    val result_nrs : ro.upb.nrs.sl.FixedFloatingPoint_B = (value1_nrs / value2_nrs) match {
                        case that : ro.upb.nrs.sl.FixedFloatingPoint_B => that
                        case _ => ro.upb.nrs.sl.FixedFloatingPoint_NR(exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)
                    }
                    //println("i: " + i + " j: " + j)
                    c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
				}
			}
        }
    }
}