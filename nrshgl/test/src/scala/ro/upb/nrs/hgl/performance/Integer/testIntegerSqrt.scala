package ro.upb.nrs.hgl.performance
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class ZSqrtSpec extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "NaturalSqrt"

    it should "compute natural sqrt" in {
        test(new ZSqrt(16)) { c =>
            c.io.op.poke(-26.S)
            c.io.loadingOp.poke(true.B)
            c.clock.step()
            c.io.loadingOp.poke(false.B)
            for (i <- 0 until 32) {
                c.clock.step()
                if (c.io.outputReady.peek().litToBoolean) {
                    c.io.notANumber.expect(false.B)
                    c.io.q.expect(15.U)
                }
            }
        }
    }
}
