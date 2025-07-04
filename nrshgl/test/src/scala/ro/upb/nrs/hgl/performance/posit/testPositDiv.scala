package ro.upb.nrs.hgl.performance

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._

class PositSingleDivTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Encode"

	it should "Encode should encode posit number" in {
		test(new PositDiv(2, 8)) { c =>
			c.io.op1.poke(80.U(8.W))
			c.io.op2.poke(72.U(8.W))
			c.clock.step()
			c.io.res.expect(72.U(8.W))
		}
	}
}

class PositDivTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Encode"

	it should "Encode should encode posit number" in {
		test(new PositDiv(2, 16)) { c =>
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
