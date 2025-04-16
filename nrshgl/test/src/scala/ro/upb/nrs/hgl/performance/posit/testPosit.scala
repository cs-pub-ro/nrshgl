package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.tester._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._

class PositDecodeTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Decode"

	it should "Decode should decode posit number" in {
		test(new DecodePosit(2, 8)) { c =>
			c.io.binary.poke(1.U(32.W))
			c.clock.step()
		}
	}
}


class PositSingleDecodeNormaliseTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Encode"

	it should "Encode should encode posit number" in {
		test(new DecodeNormalisePosit(2, 16)) { c =>
			c.io.a.poke(16384.U(16.W))
			c.clock.step()
			c.io.b.expect(16384.U(16.W))
		}
	}
}

class PositDecodeNormaliseTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Encode"

	it should "Encode should encode posit number" in {
		test(new DecodeNormalisePosit(2, 16)) { c =>
			for( i <- 0 until 65536) {
			//for( i <- 0 until 256) {
				c.io.a.poke(i.U(16.W))
				c.clock.step()
				c.io.b.expect(i.U(16.W))
			}

		}
	}
}