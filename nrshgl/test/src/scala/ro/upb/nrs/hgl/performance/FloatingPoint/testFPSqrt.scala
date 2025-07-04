package ro.upb.nrs.hgl.performance
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

/*
class SqrtTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "FloatingPointSqrt"

	it should "Sqrt should compute the square root of a floating point number" in {
		test(new FloatingPointSqrt(8, 23)) { c =>
			val res1 = FPConverter.convert(2.3f)
			c.io.in.poke(res1.asUInt(32.W))
			c.clock.step()
			//c.io.sign.expect(false.B)
			//c.io.normalizedExponent.expect(130.U)
			//c.io.normalizedMantissa.expect(6.U)
		}
	}
}*/