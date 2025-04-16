package ro.upb.nrs.hgl.performance

import chisel3._

class GCD(size: Int, precision : Int) extends Module {
    val io = IO(new Bundle{
        val value1 = Input(UInt(size.W))
        val value2 = Input(UInt(size.W))
        val gcd = Output(UInt(size.W))
    })

    val a = Wire(Vec(precision + 1, UInt(size.W)))
    val b = Wire(Vec(precision + 1, UInt(size.W)))

    for (i <- 0 to precision ) {
        if(i == 0) {
            /*
            Initialization
            */
            a(i) := Mux(io.value1 >= io.value2, io.value1, io.value2)
            b(i) := Mux(io.value1 >= io.value2, io.value2, io.value1)
            
            //printf("i: %d, a(i) %d, b(i) %d\n", i.asUInt,  a(i), b(i))
        } else {
            /*
            a(i) = b(i - 1)
            b(i) = a(i - 1) % b(i - 1)
            */
            a(i) := Mux(b(i - 1) === 0.U, a(i - 1), b(i - 1))
            b(i) := Mux(b(i - 1) === 0.U, 0.U,  a(i - 1) % b(i - 1))
            
            //printf("i: %d, a(i) %d, b(i) %d\n", i.asUInt,  a(i), b(i))
         }
    }
    io.gcd := Mux(b(precision) === 0.U, a(precision), 1.U)
}
