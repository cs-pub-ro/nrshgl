package ro.upb.nrs.hgl.performance

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._
import ro.upb.nrs.sl

class PositSingleAddTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Encode"

	it should "Encode should encode posit number" in {
		test(new PositAddSub(2, 8)) { c =>
			c.io.op1.poke(96.U(8.W))
			c.io.op2.poke(80.U(8.W))
			c.io.op_sel.poke(false.B)
			c.clock.step()
			c.io.res.expect(97.U(8.W))
		}
	}
}
class PositSingleSubTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Encode"

	it should "Encode should encode posit number" in {
		test(new PositAddSub(2, 8)) { c =>
			c.io.op1.poke(96.U(8.W))
			c.io.op2.poke(80.U(8.W))
			c.io.op_sel.poke(true.B)
			c.clock.step()
			c.io.res.expect(164.U(8.W))
		}
	}
}

class PositAddTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Encode"

	it should "Encode should encode posit number" in {
		test(new PositAddSub(2, 16)) { c =>
			//for( i <- 0 until 65536) {
			for( i <- 0 until 256) {
				for( j <- 0 until 256) {
					c.io.op1.poke(i.U(8.W))
					c.io.op2.poke(j.U(8.W))
					c.io.op_sel.poke(false.B)
					c.clock.step()
					c.io.res.expect(i.U(16.W))
				}
			}

		}
	}
}

class PositSubTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Encode"

	it should "Encode should encode posit number" in {
		test(new PositAddSub(2, 16)) { c =>
			//for( i <- 0 until 65536) {
			for( i <- 0 until 256) {
				for( j <- 0 until 256) {
					c.io.op1.poke(i.U(8.W))
					c.io.op2.poke(j.U(8.W))
					c.io.op_sel.poke(true.B)
					c.clock.step()
					c.io.res.expect(i.U(16.W))
				}
			}

		}
	}
}


class PositSingleAddDoubleTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Encode"

	it should "Encode should encode posit number" in {
		test(new PositAddSub(2, 8)) { c =>
			val a : Double = 1.0d
			val b : Double = 1.0d
			val res : Double = 2.0d
			val aposit : ro.upb.nrs.sl.Posit_B = ro.upb.nrs.sl.Posit(a, 2, 8, ro.upb.nrs.sl.RoundEven)
			val bposit : ro.upb.nrs.sl.Posit_B = ro.upb.nrs.sl.Posit(b, 2, 8, ro.upb.nrs.sl.RoundEven)
			val resposit : ro.upb.nrs.sl.Posit_B = ro.upb.nrs.sl.Posit(res, 2, 8, ro.upb.nrs.sl.RoundEven)
			val aBinaryString : String = aposit.toBinaryString
			val bBinaryString : String = bposit.toBinaryString
			val resBinaryString : String = resposit.toBinaryString
			c.io.op1.poke(("b" + aBinaryString).U(8.W))
			c.io.op2.poke(("b" + bBinaryString).U(8.W))
			c.io.op_sel.poke(false.B)
			c.clock.step()
			c.io.res.expect(("b" + resBinaryString).U(8.W))
			c.io.res.expect(("b" + (aposit + bposit).toBinaryString).U(8.W))
			val resBinary : String =  String.format("%" + 8 + "s", c.io.res.peek().litValue.toString(2)).replace(' ', '0')
			val resFrombinary :  ro.upb.nrs.sl.Posit_B = ro.upb.nrs.sl.Posit(resBinary, 2, 8, ro.upb.nrs.sl.RoundEven)
			println("resFrombinary:" + resFrombinary.toString)
			println("resposit:" + resposit.toString)
			println("resBinaryString:" + resBinaryString)
			println("output binary:" + resBinary)
			println("equal:" + (resposit == resFrombinary).toString)
			println("BigDecimal:" + resFrombinary.toBigDecimal)
		}
	}
}

class PositSingleAddBigDecimalTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "AddSub"

	it should "Add 2 posit number" in {
		test (new PositAddSub(2, 32)) { c => 
			val a = BigDecimal(3.26851631700992584228515625)
			val b = BigDecimal(0.21245285682380199432373046875)
			val aposit: ro.upb.nrs.sl.Posit_B = ro.upb.nrs.sl.Posit(a.toDouble, 2, 32, ro.upb.nrs.sl.RoundEven)
			val bposit: ro.upb.nrs.sl.Posit_B = ro.upb.nrs.sl.Posit(b.toDouble, 2, 32, ro.upb.nrs.sl.RoundEven)
			val aBinaryString: String = aposit.toBinaryString
			val bBinaryString: String = bposit.toBinaryString
			c.io.op1.poke(("b" + aBinaryString).U(32.W))
			c.io.op2.poke(("b" + bBinaryString).U(32.W))
			c.clock.step()
			c.io.res.expect(("b" + (aposit + bposit).toBinaryString).U(32.W))
			println("Big Decimal Result: " + (aposit + bposit).toBigDecimal)
			println("String Result: " + (aposit + bposit).toBinaryString)
			println("Result: " + (a + b))
			val resBinary: String = String.format("%" + 8 + "s", c.io.peek().litValue.toString(2)).replace(' ', '0')
			println("ResBinary: " + resBinary)
			val e = BigDecimal(1.3776705563068389892578125)
			val f = BigDecimal(3.48096917569637298583984375)
			val cposit: ro.upb.nrs.sl.Posit_B = ro.upb.nrs.sl.Posit(e.toDouble, 2, 32, ro.upb.nrs.sl.RoundEven)
			val dposit: ro.upb.nrs.sl.Posit_B = ro.upb.nrs.sl.Posit(f.toDouble, 2, 32, ro.upb.nrs.sl.RoundEven)
			val cBinaryString: String = cposit.toBinaryString
			val dBinaryString: String = dposit.toBinaryString
			c.io.op1.poke(("b" + cBinaryString).U(32.W))
			c.io.op2.poke(("b" + cBinaryString).U(32.W))
			c.clock.step()
			c.io.res.expect(("b" + (cposit + dposit).toBinaryString).U(32.W))
			println("Big Decimal Result: " + (cposit + dposit).toBigDecimal)
			println("String Result: " + (cposit + dposit).toBinaryString)
			println("Result: " + (e + f))
		}
	}
}