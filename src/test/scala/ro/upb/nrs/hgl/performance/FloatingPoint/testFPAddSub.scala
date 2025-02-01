package ro.upb.nrs.hgl.performance
import chisel3._
import chisel3.tester._
import org.scalatest.flatspec.AnyFlatSpec


class AddSubTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "FPAddSub"

	it should "compute ceva of two floating point numbers" in {
		test(new FloatingPointAddSub(8, 23)) { c =>
			c.io.op1.poke("hFF800000".U)
			c.io.op2.poke("h5BA64D71".U) 
            c.io.opSel.poke(false.B)
			c.clock.step()
			//c.io.sign.expect(false.B)
			//c.io.normalizedExponent.expect(130.U)
			//c.io.normalizedMantissa.expect(6.U)
			c.io.res.expect(25.U)
		}
	}
}
