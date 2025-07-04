package ro.upb.nrs.hgl.performance
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._
import ro.upb.nrs.sl

class testFractionalMulDiv extends AnyFlatSpec with ChiselScalatestTester {
    	behavior of "MulDiv"

	it should "Mul 2 fractional number" in {
		test (new FractionalMulDiv(32, 32)) { c => 
			val a = BigDecimal(-0.9236903200656377)
			val b = BigDecimal(-1.2422619587121064)
			val aFractional = ro.upb.nrs.sl.FractionalNumber(a.toDouble, 32, 32, ro.upb.nrs.sl.RoundEven)
			val bFractional = ro.upb.nrs.sl.FractionalNumber(b.toDouble, 32, 32, ro.upb.nrs.sl.RoundEven)
			val aBinaryString: String = aFractional.toBinaryString
			val bBinaryString: String = bFractional.toBinaryString
			c.io.op1.poke(("b" + aBinaryString).U)
			c.io.op2.poke(("b" + bBinaryString).U)
            c.io.op_sel.poke(false.B)
			c.clock.step()
			c.io.res.expect(("b" + (aFractional * bFractional).toBinaryString).U)
			println("Big Decimal Result: " + (aFractional * bFractional).toBigDecimal)
			println("String Result: " + (aFractional * bFractional).toBinaryString)
			println("Result: " + (a * b))
			/*val resBinary: String = String.format("%" + 8 + "s", c.io.peek().litValue.toString(2)).replace(' ', '0')
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
			println("Result: " + (e + f))*/
		}
	}
}