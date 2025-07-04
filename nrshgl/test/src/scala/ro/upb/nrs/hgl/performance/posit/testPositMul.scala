package ro.upb.nrs.hgl.performance

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._
import ro.upb.nrs.sl

class PositSingleMulTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Encode"

	it should "Encode should encode posit number" in {
		test(new PositMul(2, 8)) { c =>
			c.io.op1.poke(80.U(8.W))
			c.io.op2.poke(80.U(8.W))
			c.clock.step()
			c.io.res.expect(96.U(8.W))
		}
	}
}

class PositMulTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Encode"

	it should "Encode should encode posit number" in {
		test(new PositMul(2, 16)) { c =>
			//for( i <- 0 until 65536) {
			for( i <- 0 until 256) {
				for( j <- 0 until 256) {
					c.io.op1.poke(i.U(8.W))
					c.io.op2.poke(j.U(8.W))
					c.clock.step()
					c.io.res.expect(i.U(16.W))
				}
			}

		}
	}
}

class PositMul2Integers extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Mul"

	it should "Mul 2 integers" in {
		test(new PositMul(2, 32)) { c => 
			c.io.op1.poke("h40000000".U)
			c.io.op2.poke("h48000000".U)
			c.clock.step()
			c.io.res.expect	("h48000000".U)
		}
	}
}

class PositMul2Numbers extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Mul"

	it should "Mul 2 posit numbers" in {
		test(new PositMul(2, 32)) { c =>
			val a: BigDecimal = BigDecimal(-1.173742116799089)
			val b: BigDecimal = BigDecimal(-1.173742116799089)
			val aposit: ro.upb.nrs.sl.Posit_B = ro.upb.nrs.sl.Posit(a.toDouble, 2, 32, ro.upb.nrs.sl.RoundEven)
			val bposit: ro.upb.nrs.sl.Posit_B = ro.upb.nrs.sl.Posit(b.toDouble, 2, 32, ro.upb.nrs.sl.RoundEven)
			val aBinaryString: String = aposit.toBinaryString
			val bBinaryString: String = bposit.toBinaryString
			c.io.op1.poke(("b" + aBinaryString).U(32.W))
			c.io.op2.poke(("b" + bBinaryString).U(32.W))
			c.clock.step()
			c.io.res.expect(("b" + (aposit * bposit).toBinaryString).U(32.W))
			println("Big Decimal Result: " + (aposit * bposit).toBigDecimal)
			println("String Result: " + (aposit * bposit).toBinaryString)
			println("Result: " + (a * b))
		}
	}
}
