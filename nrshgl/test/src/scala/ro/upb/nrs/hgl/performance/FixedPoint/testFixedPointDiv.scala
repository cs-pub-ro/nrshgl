package ro.upb.nrs.hgl.performance
import chisel3._
import chisel3.testers._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import fixedpoint._

class FixedPointQDivTest extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "FixedPointQOps"
    
    it should "1.75 / 2" in {
        test(new FixedPointQDiv(5, 2)) { c =>
            c.io.op1.poke(1.0.F(2.BP))
            c.io.op2.poke(2.0.F(2.BP))
            c.clock.step()
            c.io.res.expect(0.5.F(2.BP))
        }
    }
}
