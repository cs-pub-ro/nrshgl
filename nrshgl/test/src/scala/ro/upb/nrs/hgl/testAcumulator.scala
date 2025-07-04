package ro.upb.nrs.hgl

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chiseltest.ChiselScalatestTester
import chisel3.util._

import ro.upb.nrs.sl
import ro.upb.nrs.benchmark


class TestSingleAccumulator extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "test single operation on Accumulator"
    val exponentSize = 4
    val fractionSize = 3
    val size = exponentSize + fractionSize + 1
    val nrs : FloatingPointTrait = FixedFloatingPoint(exponentSize, fractionSize, RoundEven, false)
    val testNrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = ro.upb.nrs.sl.FixedFloatingPoint(0.0d, exponentSize, fractionSize, ro.upb.nrs.sl.RoundEven)
    val accumulatorSize = 40
    val accumulatorFractionSize = 20

    it should "add 2 values" in {
        test(new KAU(
                        nrs,
                        accumulatorSize,
                        accumulatorFractionSize,
                        true
            )) { c =>
            val operationType : Int = 0
            val a : Int = 64
            val b : Int = 64
            val binary1 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(a).binaryEncode))
            val value1_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary1, testNrs)
            val binary2 = ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodetoBinaryString(ro.upb.nrs.sl.auxiliaryFunctions.BinaryEncodeFixedWidth(size, ro.upb.nrs.sl.NaturalNumber(b).binaryEncode))
            val value2_nrs = ro.upb.nrs.benchmark.basicBenchmarkFunctions.fromBinaryString(binary2, testNrs)
            val acumualtor = new ro.upb.nrs.sl.Accumulator(
                ro.upb.nrs.sl.FixedPoint(
                    ro.upb.nrs.sl.IntegerNumber(0),
                    accumulatorSize,
                    accumulatorFractionSize,
                    ro.upb.nrs.sl.NoRounding
                ),
                accumulatorSize,
                accumulatorFractionSize
            )
            //clear
            c.io.inputAccumulator.value.poke(0.U(accumulatorSize.W))
            c.io.inputAccumulator.overflow.poke(false.B)
            c.io.inputAccumulator.underflow.poke(false.B)
            //fma 2*2
            c.io.operand1Value.poke(a.U(size.W))
            c.io.operand2Value.poke(b.U(size.W))
            c.io.operationType.poke(operationType.U(2.W))
			c.clock.step()
            val result : ro.upb.nrs.sl.NumberRepresentationSystem = operationType match {
                case 0 => (value1_nrs * value2_nrs)
                case 1 => -(value1_nrs * value2_nrs)
                case 2 => (value1_nrs - value1_nrs)
                case 3 => (value1_nrs + value2_nrs)
                case _ => value1_nrs
            }
            val result_nrs : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem = result match {
                case that : ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem => that
                case _ => throw new Error("not a fixed precision number")
            }
            val accumulatorResult  = (value1_nrs, value2_nrs) match {
                case a : (
                    ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem with ro.upb.nrs.sl.AccumulatorTrait,
                    ro.upb.nrs.sl.FixedPrecisionNumberRepresentationSystem with ro.upb.nrs.sl.AccumulatorTrait
                ) => acumualtor.fma(a._1, a._2)
                case _ => throw new Error("not a AccumulatorTrait")
            }
            println("Value1 " + value1_nrs)
            println("Value2 " + value2_nrs)
            println("Result " + result)
            println("Result " + result_nrs)
            println("Result accumulator " + accumulatorResult.value)
            c.io.resultValue.expect(("b" + result_nrs.toBinaryString).U(size.W))
            c.io.outputAccumulator.value.expect(("b" + accumulatorResult.value.toBinaryString).U(accumulatorSize.W))
        }
    }
}
