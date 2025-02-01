package ro.upb.nrs.hgl.performance
import chisel3._
import chisel3.tester._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._

class FractionalDecodeTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Decode"

	it should "Decode should decode Fractional number" in {
		test(new DecodeFractional(8, 8)) { c =>
			c.io.binary.poke(1.U(16.W))
			c.clock.step()
		}
	}
}


class FractionalSingleDecodeNormaliseTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Encode"

	it should "Encode should encode Fractional number" in {
		test(new DecodeNormaliseEncodeFractional(7, 8)) { c =>
            val nar = 1 << 8
            val zero = 1
            val i : BigInt = 32769
			c.io.a.poke(i.U(16.W))
			c.clock.step()
            val sign : BigInt = i >> 15
            val j = if(sign==1) i - (1<<15) else i
            if(j % nar == 0)
                c.io.b.expect(nar.U(16.W))
            else if(j < nar)
                c.io.b.expect(zero.U(16.W))
            else {
                val numeretor : BigInt = (j >> 8)
                println("numerator:" + numeretor)
                val denominator : BigInt = j & ((1<<8)-1)
                println("denominator:" + denominator)
                val gcd : BigInt = numeretor.gcd(denominator)
                println("gcd:" + gcd)
                val binary : BigInt = ((numeretor / gcd) << 8) + (denominator /gcd) + (if(sign==1) (1<<15) else 0)
                c.io.b.expect(binary.U(16.W))
            }
		}
	}
}

class FractionalDecodeNormaliseTest extends AnyFlatSpec with ChiselScalatestTester {
	behavior of "Encode"

	it should "Encode should encode Fractional number" in {
		test(new DecodeNormaliseEncodeFractional(7, 8)) { c =>
            val nar = 1 << 8
            val zero = 1
			for( i <- 0 until 65536) {
			//for( i <- 0 until 256) {
				c.io.a.poke(i.U(16.W))
				c.clock.step()
                val sign : BigInt = i >> 15
                val j = if(sign==1) i - (1<<15) else i
                if(j % nar == 0)
				    c.io.b.expect(nar.U(16.W))
                else if(j < nar)
				    c.io.b.expect(zero.U(16.W))
                else {
                    val numeretor : BigInt = j >> 8
                    val denominator : BigInt = j & ((1<<8)-1)
                    val gcd : BigInt = numeretor.gcd(denominator)
                    val binary : BigInt = ((numeretor / gcd) << 8) + (denominator /gcd) + (if(sign==1) (1<<15) else 0)
				    c.io.b.expect(binary.U(16.W))
                }
			}

		}
	}
}