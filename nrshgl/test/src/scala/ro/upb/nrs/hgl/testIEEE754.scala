package ro.upb.nrs.hgl

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.ChiselScalatestTester

import ro.upb.nrs.sl
import ro.upb.nrs.benchmark
import scala.runtime.RichInt


class TestAdditionSingleIEEE754 extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Addition on IEEE754"
    val exponent_size = 4
    val fraction_size = 3
    val rounding = RoundEven
    val size = exponent_size + fraction_size + 1

    it should "add 2 values" in {
        test(new AdditionOperationModule(() => IEEE754(exponent_size, fraction_size, rounding, true))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.IEEE754(0.0d, exponent_size, fraction_size, ro.upb.nrs.sl.RoundEven)
            val a : Int = 121
            val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(a).binaryEncode))
            val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
            val b : Int = 120
            val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(b).binaryEncode))
            val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
            c.io.operand1.value.poke(a.U(size.W))
            c.io.operand2.value.poke(b.U(size.W))
			c.clock.step()
            val result_nrs : ro.upb.nrs.sl.IEEE754_B = (value1_nrs + value2_nrs) match {
                case that : ro.upb.nrs.sl.IEEE754_B => that
                case _ => ro.upb.nrs.sl.IEEE754_NR(exponent_size, fraction_size, ro.upb.nrs.sl.RoundEven)
            }
            println("Value1 " + value1_nrs)
            println("Value2 " + value2_nrs)
            println("Result " + (value1_nrs + value2_nrs))
            println("Result " + result_nrs)
            c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
        }
    }
}


class TestAdditionExhaustiveIEEE754 extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Addition on IEEE754"
    val exponent_size = 4
    val fraction_size = 3
    val rounding = RoundEven
    val size = exponent_size + fraction_size + 1

    it should "exhausting verification +/-" in {
        test(new AdditionOperationModule(() => IEEE754(exponent_size, fraction_size, rounding, false))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.IEEE754(0.0d, exponent_size, fraction_size, ro.upb.nrs.sl.RoundEven)
			for( i <- 121 until (1<<size)) {//
				for( j <- 0 until (1<<size)) {
                    val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(i).binaryEncode))
                    val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
                    val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(j).binaryEncode))
                    val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
                    c.io.operand1.value.poke(i.U(size.W))
                    c.io.operand2.value.poke(j.U(size.W))
                    c.clock.step()
                    val result_nrs : ro.upb.nrs.sl.IEEE754_B = (value1_nrs + value2_nrs) match {
                        case that : ro.upb.nrs.sl.IEEE754_B => that
                        case _ => ro.upb.nrs.sl.IEEE754_NR(exponent_size, fraction_size, ro.upb.nrs.sl.RoundEven)
                    }
                    //println("i: " + i + " j: " + j)
                    c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
				}
			}
        }
    }
}


class TestMultiplicationSingleIEEE754 extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Multiplication on IEEE754"
    val exponent_size = 4
    val fraction_size = 3
    val rounding = RoundEven
    val size = exponent_size + fraction_size + 1

    it should "multiply 2 values" in {
        test(new MultiplicationOperationModule(() => IEEE754(exponent_size, fraction_size, rounding, true))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.IEEE754(0.0d, exponent_size, fraction_size, ro.upb.nrs.sl.RoundEven)
            val a : Int = 1
            val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(a).binaryEncode))
            val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
            val b : Int = 49
            val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(b).binaryEncode))
            val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
            c.io.operand1.value.poke(a.U(size.W))
            c.io.operand2.value.poke(b.U(size.W))
			c.clock.step()
            val result_nrs : ro.upb.nrs.sl.IEEE754_B = (value1_nrs * value2_nrs) match {
                case that : ro.upb.nrs.sl.IEEE754_B => that
                case _ => ro.upb.nrs.sl.IEEE754_NR(exponent_size, fraction_size, ro.upb.nrs.sl.RoundEven)
            }
            println("Value1 " + value1_nrs)
            println("Value2 " + value2_nrs)
            println("Result " + (value1_nrs * value2_nrs))
            println("Result " + result_nrs)
            c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
        }
    }
    
}


class TestMultiplicationExhaustiveIEEE754 extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Multiplication on IEEE754"
    val exponent_size = 4
    val fraction_size = 3
    val rounding = RoundEven
    val size = exponent_size + fraction_size + 1

    it should "exhausting verification *" in {
        test(new MultiplicationOperationModule(() => IEEE754(exponent_size, fraction_size, rounding, false))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.IEEE754(0.0d, exponent_size, fraction_size, ro.upb.nrs.sl.RoundEven)
			for( i <- 0 until (1<<size)) {//
				for( j <- 0 until (1<<size)) {
                    val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(i).binaryEncode))
                    val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
                    val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(j).binaryEncode))
                    val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
                    c.io.operand1.value.poke(i.U(size.W))
                    c.io.operand2.value.poke(j.U(size.W))
                    c.clock.step()
                    val result_nrs : ro.upb.nrs.sl.IEEE754_B = (value1_nrs * value2_nrs) match {
                        case that : ro.upb.nrs.sl.IEEE754_B => that
                        case _ => ro.upb.nrs.sl.IEEE754_NR(exponent_size, fraction_size, ro.upb.nrs.sl.RoundEven)
                    }
                    //println("i: " + i + " j: " + j)
                    c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
				}
			}
        }
    }
}


class TestDivisionSingleIEEE754 extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Division on IEEE754"
    val exponent_size = 4
    val fraction_size = 3
    val rounding = RoundEven
    val size = exponent_size + fraction_size + 1

    it should "Division 2 values" in {
        test(new DivisionOperationModule(() => IEEE754(exponent_size, fraction_size, rounding, true))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.IEEE754(0.0d, exponent_size, fraction_size, ro.upb.nrs.sl.RoundEven)
            val a : Int = 128
            val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(a).binaryEncode))
            val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
            val b : Int = 128
            val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(b).binaryEncode))
            val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
            c.io.operand1.value.poke(a.U(size.W))
            c.io.operand2.value.poke(b.U(size.W))
			c.clock.step()
            val result_nrs : ro.upb.nrs.sl.IEEE754_B = (value1_nrs / value2_nrs) match {
                case that : ro.upb.nrs.sl.IEEE754_B => that
                case _ => ro.upb.nrs.sl.IEEE754_NR(exponent_size, fraction_size, ro.upb.nrs.sl.RoundEven)
            }
            println("Value1 " + value1_nrs)
            println("Value2 " + value2_nrs)
            println("Result " + (value1_nrs / value2_nrs))
            println("Result " + result_nrs)
            c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
        }
    }
}


class TestDivisionExhaustiveIEEE754 extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "Division on IEEE754"
    val exponent_size = 4
    val fraction_size = 3
    val rounding = RoundEven
    val size = exponent_size + fraction_size + 1

    it should "exhausting verification /" in {
        test(new DivisionOperationModule(() => IEEE754(exponent_size, fraction_size, rounding, false))) { c =>
            val nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.IEEE754(0.0d, exponent_size, fraction_size, ro.upb.nrs.sl.RoundEven)
			for( i <- 0 until (1<<size)) {//
				for( j <- 0 until (1<<size)) {
                    val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(i).binaryEncode))
                    val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, nrs)
                    val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(j).binaryEncode))
                    val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, nrs)
                    c.io.operand1.value.poke(i.U(size.W))
                    c.io.operand2.value.poke(j.U(size.W))
                    c.clock.step()
                    val result_nrs : ro.upb.nrs.sl.IEEE754_B = (value1_nrs / value2_nrs) match {
                        case that : ro.upb.nrs.sl.IEEE754_B => that
                        case _ => ro.upb.nrs.sl.IEEE754_NR(exponent_size, fraction_size, ro.upb.nrs.sl.RoundEven)
                    }
                    //println("i: " + i + " j: " + j)
                    c.io.result.value.expect(("b" + result_nrs.toBinaryString).U(size.W))
				}
			}
        }
    }
}

class TestIntegerToFloatIEEE754 extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "TestIntegerToFloatIEEE754"

    val integerSize = 32
    val exponent_size = 8
    val fraction_size = 23
    val rounding = RoundEven
    val size = exponent_size + fraction_size + 1

  it should "pass" in {
    test(new ConvertIntegerToFP(integerSize, exponent_size, fraction_size, rounding, NRS_IEEE754)) { dut =>
        for (i <- 0 until 100) {
            val integer = BigInt(i.toString(), 10).U
            dut.io.integer.poke(integer)
            dut.io.sign.poke(false)
            dut.clock.step(1)
            val result = dut.io.binary.peek()

            println(s"Result: ${sl.IEEE754.apply(String.format("%32s", result.litValue.toString(2)).replace(' ', '0'),
                exponent_size, fraction_size, sl.RoundEven)}")
        }

        for (i <- -500 until -400) {
            val twosComplement = (BigInt(i) & ((BigInt(1) << integerSize) - 1))
            val integer = twosComplement.U(integerSize.W)

            dut.io.integer.poke(integer)
            dut.io.sign.poke(true)
            dut.clock.step(1)
            val result = dut.io.binary.peek()

            println(s"Result: ${sl.IEEE754.apply(String.format("%32s", result.litValue.toString(2)).replace(' ', '0'),
                exponent_size, fraction_size, sl.RoundEven)}")
        }
    }
  }
}

class TestFloatIEEE754ToInteger extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "TestFloatIEEE754ToInteger"

    val integerSize = 32
    val exponent_size = 8
    val fraction_size = 23
    val rounding = RoundEven
    val size = exponent_size + fraction_size + 1

  it should "pass" in {
    test(new ConvertFPToInteger(integerSize, exponent_size, fraction_size, rounding, NRS_IEEE754)) { dut =>
        val testValues = Array(
            // Whole numbers 
            0.0f, 1.0f, -1.0f, 2.0f, -2.0f, 28455.0f, -28455.0f, 
            // Very large numbers (near int32 limits)
            2147483520.0f, -2147483520.0f,  // Close to Int.MaxValue/MinValue
            // Fractional numbers
            3.14f, -3.14f, 0.5f, -0.5f, 0.25f, -0.25f, 0.75f, -0.75f,
            // Very small numbers that should round to 0
            0.1f, -0.1f, 0.01f, -0.01f,
            // Special cases
            Float.NaN
        )
        
        for (testValue <- testValues) {
            val bits = java.lang.Float.floatToIntBits(testValue)
            val bits_unsigned = bits & 0xFFFFFFFFL

            dut.io.binary.poke(bits_unsigned.U)
            dut.clock.step(1)
            
            val result = dut.io.integer.peek()
            
            println(s"Value: $testValue -> Integer result: ${result.litValue.intValue}")
        }
    }
  }
}

class MyTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MyModule"

    val exponent_size = 8
    val sig_size = 24
    val rounding = RoundEven
    val size = exponent_size + sig_size

  it should "pass" in {
    test(new MulAddRecFNPipe(exponent_size, sig_size, NRS_IEEE754)) { dut =>
        val a = 3.0f
        val b = 1.0f
        val c = 6.0f
        val op = 1

        val a_bits = java.lang.Float.floatToIntBits(a)
        val b_bits = java.lang.Float.floatToIntBits(b)
        val c_bits = java.lang.Float.floatToIntBits(c)
        
        dut.io.op.poke(op.U)
        dut.io.a.poke(a_bits.U)
        dut.io.b.poke(b_bits.U)
        dut.io.c.poke(c_bits.U)
        dut.clock.step(1)

        val result = dut.io.out.peek()
        println(s"Result: ${sl.IEEE754.apply(String.format("%32s", result.litValue.toString(2)).replace(' ', '0'), exponent_size, sig_size - 1, sl.RoundEven)}")

        val expected_result = op match {
            case 0 => a * b + c
            case 1 => a * b - c
            case 2 => -(a * b) + c
            case 3 => -(a * b) - c
            case _ => throw new IllegalArgumentException("Invalid operation")
        }
        println(s"Expected result: $expected_result")
    }
  }
}