package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.tester._
import org.scalatest.flatspec.AnyFlatSpec

class NSqrtSpec extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "NaturalSqrt"

    it should "compute natural sqrt" in {
        test(new NaturalSqrt(9,3)) { c =>
            c.io.op.poke(65.U)
            c.clock.step()
            c.io.q.expect(4.U)
            c.io.r.expect(1.U)
        }
    }
}