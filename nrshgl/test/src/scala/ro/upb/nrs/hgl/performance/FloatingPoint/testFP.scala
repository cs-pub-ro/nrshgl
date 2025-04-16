package ro.upb.nrs.hgl.performance
import chisel3._
import chisel3.tester._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._
import java.lang._

object FPConverter {
	def convert(number: Float) : String = {
		val bits = Float.floatToRawIntBits(number)
		val str = Integer.toBinaryString(bits)

		return ("b" + "0" * (32 - str.length()) + str)
	}
}

class DecodeTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Decode"

	it should "Decode should decode floating point number" in {
		test(new Decode(8, 23)) { c =>
			val res = FPConverter.convert(0.0f)
			c.io.in.poke(res.asUInt(32.W))
			c.clock.step()
			c.io.sign.expect(false.B)
			//c.io.exponent.expect(5.U)
			//c.io.positiveZero.expect(false.B)
		}
	}
}