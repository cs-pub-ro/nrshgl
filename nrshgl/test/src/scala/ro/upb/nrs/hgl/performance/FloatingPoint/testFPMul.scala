package ro.upb.nrs.hgl.performance
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec


class MulTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "FloatingPointMultiply"
	
	it should "Mul should compute product of two floating point numbers" in {
		test(new FloatingPointMultiply(8, 23)) { c =>
			c.io.op1.poke("hc0cdd763".U)
			c.io.op2.poke("hcaa6861d".U)
			c.clock.step()
			//c.io.sign.expect(false.B)
			c.io.res.expect(3.U)
		}
	}
}