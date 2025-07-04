package ro.upb.nrs.hgl

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.ChiselScalatestTester

import ro.upb.nrs.sl
import ro.upb.nrs.benchmark


class TestAdditionSingleMorrisBiasHEB extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Addition on MorrisBiasHEB"
    val gSize = 2
    val size = 8
    val rounding = RoundEven

    it should "add 2 values" in {
        test(new AdditionOperationModule(() => MorrisBiasHEB(gSize, size, rounding, true))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.MorrisBiasHEB(0.0d, gSize, size, ro.upb.nrs.sl.RoundEven)
            val a : Int = 144
            val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(a).binaryEncode))
            val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
            val b : Int = 254
            val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(b).binaryEncode))
            val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
            c.io.operand1.value.poke(a.U(size.W))
            c.io.operand2.value.poke(b.U(size.W))
			c.clock.step()
            val result_nrs : ro.upb.nrs.sl.MorrisBiasHEB_B = (value1_nrs + value2_nrs) match {
                case that : ro.upb.nrs.sl.MorrisBiasHEB_B => that
                case _ => ro.upb.nrs.sl.MorrisBiasHEB_NR(gSize, size, ro.upb.nrs.sl.RoundEven)
            }
            println("Value1 " + value1_nrs)
            println("Value2 " + value2_nrs)
            println("Result " + (value1_nrs + value2_nrs))
            println("Result " + result_nrs)
            c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
        }
    }
}


class TestAdditionExhaustiveMorrisBiasHEB extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Addition on MorrisBiasHEB"
    val gSize = 2
    val size = 8
    val rounding = RoundEven

    it should "exhausting verification +/-" in {
        test(new AdditionOperationModule(() => MorrisBiasHEB(gSize, size, rounding, false))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.MorrisBiasHEB(0.0d, gSize, size, ro.upb.nrs.sl.RoundEven)
			for( i <- 0 until (1<<size)) {//
				for( j <- 0 until (1<<size)) {
                    val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(i).binaryEncode))
                    val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
                    val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(j).binaryEncode))
                    val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
                    c.io.operand1.value.poke(i.U(size.W))
                    c.io.operand2.value.poke(j.U(size.W))
                    c.clock.step()
                    val result_nrs : ro.upb.nrs.sl.MorrisBiasHEB_B = (value1_nrs + value2_nrs) match {
                        case that : ro.upb.nrs.sl.MorrisBiasHEB_B => that
                        case _ => ro.upb.nrs.sl.MorrisBiasHEB_NR(gSize, size, ro.upb.nrs.sl.RoundEven)
                    }
                    println("i: " + i + " j: " + j)
                    c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
				}
			}
        }
    }
}

class TestMultiplicationSingleMorrisBiasHEB extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Multiplication on MorrisBiasHEB"
    val gSize = 2
    val size = 8
    val rounding = RoundEven

    it should "multiply 2 values" in {
        test(new MultiplicationOperationModule(() => MorrisBiasHEB(gSize, size, rounding, true))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.MorrisBiasHEB(0.0d, gSize, size, ro.upb.nrs.sl.RoundEven)
            val a : Int = 114
            val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(a).binaryEncode))
            val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
            val b : Int = 125
            val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(b).binaryEncode))
            val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
            c.io.operand1.value.poke(a.U(size.W))
            c.io.operand2.value.poke(b.U(size.W))
			c.clock.step()
            val result_nrs : ro.upb.nrs.sl.MorrisBiasHEB_B = (value1_nrs * value2_nrs) match {
                case that : ro.upb.nrs.sl.MorrisBiasHEB_B => that
                case _ => ro.upb.nrs.sl.MorrisBiasHEB_NR(gSize, size, ro.upb.nrs.sl.RoundEven)
            }
            println("Value1 " + value1_nrs)
            println("Value2 " + value2_nrs)
            println("Result " + (value1_nrs * value2_nrs))
            println("Result " + result_nrs)
            c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
        }
    }
}


class TestMultiplicationExhaustiveMorrisBiasHEB extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Multiplication on MorrisBiasHEB"
    val gSize = 2
    val size = 8
    val rounding = RoundEven

    it should "exhausting verification *" in {
        test(new MultiplicationOperationModule(() => MorrisBiasHEB(gSize, size, rounding, false))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.MorrisBiasHEB(0.0d, gSize, size, ro.upb.nrs.sl.RoundEven)
			for( i <- 0 until (1<<size)) {//
				for( j <- 0 until (1<<size)) {
                    val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(i).binaryEncode))
                    val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
                    val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(j).binaryEncode))
                    val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
                    c.io.operand1.value.poke(i.U(size.W))
                    c.io.operand2.value.poke(j.U(size.W))
                    c.clock.step()
                    val result_nrs : ro.upb.nrs.sl.MorrisBiasHEB_B = (value1_nrs * value2_nrs) match {
                        case that : ro.upb.nrs.sl.MorrisBiasHEB_B => that
                        case _ => ro.upb.nrs.sl.MorrisBiasHEB_NR(gSize, size, ro.upb.nrs.sl.RoundEven)
                    }
                    println("i: " + i + " j: " + j)
                    c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
				}
			}
        }
    }
}


class TestDivisionSingleMorrisBiasHEB extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Division on MorrisBiasHEB"
    val gSize = 2
    val size = 8
    val rounding = RoundEven
    it should "Division 2 values" in {
        test(new DivisionOperationModule(() => MorrisBiasHEB(gSize, size, rounding, true))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.MorrisBiasHEB(0.0d, gSize, size, ro.upb.nrs.sl.RoundEven)
            val a : Int = 128
            val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(a).binaryEncode))
            val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
            val b : Int = 128
            val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(b).binaryEncode))
            val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
            c.io.operand1.value.poke(a.U(size.W))
            c.io.operand2.value.poke(b.U(size.W))
			c.clock.step()
            val result_nrs : ro.upb.nrs.sl.MorrisBiasHEB_B = (value1_nrs / value2_nrs) match {
                case that : ro.upb.nrs.sl.MorrisBiasHEB_B => that
                case _ => ro.upb.nrs.sl.MorrisBiasHEB_NR(gSize, size, ro.upb.nrs.sl.RoundEven)
            }
            println("Value1 " + value1_nrs)
            println("Value2 " + value2_nrs)
            println("Result " + (value1_nrs / value2_nrs))
            println("Result " + result_nrs)
            c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
        }
    }
}


class TestDivisionExhaustiveMorrisBiasHEB extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Division on MorrisBiasHEB"
    val gSize = 2
    val size = 8
    val rounding = RoundEven

    it should "exhausting verification /" in {
        test(new DivisionOperationModule(() => MorrisBiasHEB(gSize, size, rounding, false))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.MorrisBiasHEB(0.0d, gSize, size, ro.upb.nrs.sl.RoundEven)
			for( i <- 0 until (1<<size)) {//
				for( j <- 0 until (1<<size)) {
                    val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(i).binaryEncode))
                    val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
                    val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(j).binaryEncode))
                    val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
                    c.io.operand1.value.poke(i.U(size.W))
                    c.io.operand2.value.poke(j.U(size.W))
                    c.clock.step()
                    val result_nrs : ro.upb.nrs.sl.MorrisBiasHEB_B = (value1_nrs / value2_nrs) match {
                        case that : ro.upb.nrs.sl.MorrisBiasHEB_B => that
                        case _ => ro.upb.nrs.sl.MorrisBiasHEB_NR(gSize, size, ro.upb.nrs.sl.RoundEven)
                    }
                    println("i: " + i + " j: " + j)
                    c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
				}
			}
        }
    }
}