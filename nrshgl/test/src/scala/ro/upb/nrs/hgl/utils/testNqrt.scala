package ro.upb.nrs.hgl

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class nqrtTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "nqrtTest"

    it should "compute natural sqrt" in {
        test(new NQRT(9,3)) { c =>
            c.io.op.poke(65.U)
            c.clock.step()
            c.io.q.expect(4.U)
            c.io.r.expect(1.U)
        }
    }
}