package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.tester._
import org.scalatest.flatspec.AnyFlatSpec

class gcdTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "nqrtTest"

    it should "compute natural sqrt" in {
        test(new GCD(16,5)) { c =>
            c.io.value1.poke(12.U)
            c.io.value2.poke(32.U)
            c.clock.step()
            c.io.gcd.expect(4.U)
        }
    }
}