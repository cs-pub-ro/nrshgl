package ro.upb.nrs.hgl.performance
import chisel3._
import chisel3.tester._
import org.scalatest.flatspec.AnyFlatSpec

class QAddSpec extends AnyFlatSpec with ChiselScalatestTester{
    behavior of "QAdd"
    
    it should "add 2 fractional numbers" in {
        test(new FractionalAddSub(2, 2)) { c =>
            c.io.op1.poke(5.U(5.W))
            c.io.op2.poke(5.U(5.W))
            c.io.op_sel.poke(false.B)
            c.clock.step()
            c.io.res.expect(5.U(5.W))
        }
    }
}

