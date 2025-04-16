package ro.upb.nrs.hgl.performance
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class DivTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "FloatPDiv"

	it should "compute the division of two floating point numbers" in {
		test(new FloatingPointDiv(8, 23)) { c =>
			c.io.op1.poke("h486201D5".U)
			c.io.op2.poke("h42DE71AA".U)
			c.clock.step()
			//c.io.sign.expect(false.B)
			//c.io.normalizedExponent.expect(130.U)
			//c.io.normalizedMantissa.expect(6.U)
			c.io.res.expect(25.U)
		}
	}
}


class DivTest2 extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "FloatPDiv"

	it should "compute the division of two floating point numbers" in {
		test(new FloatingPointDiv(8, 23)) { c => 
			c.io.op1.poke("hDB9A52D9".U)
			c.io.op2.poke("h4BA2AB6C".U)
			c.clock.step()
			c.io.res.expect(24.U)	
		}
	}
}