package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.util._

/*
makes n order root of an operand of size bits
it returns q (quotient) - the results and r - the remainder
we consider op as divident
quotient = NQRT(divident) + remainder
order = n
ALGORITHM:
D = current Devident
Q = current result (quotient)
X = biggest posibile Digit so Y <= REMAINDER (1 or 0 in binary)
    so we use only 1 to verify because for X = 0 => Y = 0
B = the numerice base (2 for binary)
D = Devident (input)
Q = 0
REMAINDER = 0
while(D != 0)
Y = SUM of Base^iterator * PascalaTriangle(n, iterator) * Q^iterator * X^(n-iterator)
    with iterator from 0 to n-1
  = SUM of 2^iterator * PascalaTriangle(n, iterator) * Q^iterator * 1^(n-iterator)
    with iterator from 0 to n-1
  = SUM of (PascalaTriangle(n, iterator) * Q^iterator) << iterator
    with iterator from 0 to n-1
REMAINDER = REAMINDER << n + MSB n bits of D
D = D drop MSB n bits
//if we can extra y from remainder than we add a bit of 1 to outient
// otherwise we add a bit of zero
if(REMAINDER >= Y) { 
    REAMINDER = REMAINDER - Y
    Q = Q << 1 + 1
} else {
    Q = Q << 1
}
result = (Q, REMAINDER)
*/
class NaturalSqrt(size: Int, order : Int) extends Module {
    /*
    NQRT needs that the size to be multiple of n (because it taks n bits at the time)
    we calculate the right size and we fill de operand with zero on the MSB bits
    */
    val real_size : Int = size + size % order
    val toFill : Int = if(real_size == size) 0 else order - (size % order)
    val io = IO(new Bundle {
        val op = Input(UInt(size.W))
        val q = Output(UInt(size.W))
        val r = Output(UInt(size.W))
    })

    /*
    Variable use for the algorithm steps
    it takes size/order steps (every step consume order bits)
    First step is for initialise
    */
    // quotient at every step
    val quotients = Wire(Vec(real_size / order + 1, UInt(real_size.W)))
    // remainder at every step
    val remainders = Wire(Vec(real_size / order + 1, UInt(real_size.W)))
    // current remainder shifted and with the next n bits of divident
    val tmpr = Wire(Vec(real_size / order + 1, UInt(real_size.W)))
    //divident a every step
    val dividents = Wire(Vec(real_size / order + 1, (UInt(real_size.W))))
    // value to extract from remainder
    val ys = Wire(Vec(real_size / order + 1, (UInt(real_size.W))))
    // powers of current quotient
    val qpow = Wire(Vec(real_size / order + 1,Vec(order, (UInt(real_size.W)))))

    // pascal triangle function
    def pascal_triangle(row : Int, col : Int) : Int = {
        require(col <= row)
        (row, col) match {
        case (_, 0) => 1
        case (_, _) => if(col == row) 1 else pascal_triangle(row - 1, col - 1) + pascal_triangle(row - 1, col)
        }
    }

    for (i <- 0 until (real_size / order + 1) ) {
        if(i == 0) {
            /*
            Initialization
            */
            quotients(i) := 0.U
            remainders(i) := 0.U
            ys(i) := 0.U
            // Fill MSB with zeros of input if the real_size and size differs and reverse after
            // otherwise just reverse the bits from input
            if(real_size == size)
                dividents(i) := Reverse(io.op)
            else
                dividents(i) := Reverse(0.U(toFill.W) ## io.op)
            tmpr(i) := 0.U
            for (j <- 0 until order ) {
                qpow(i)(j) := 0.U
            }
            printf("i: %d, ys %d, tmpr %d, dividents %d, remainders %d, quotients %d\n", i.asUInt,  ys(i), tmpr(i), dividents(i), remainders(i), quotients(i) )
        } else {
            /*
            calculates q^iterator with iterator from 0 to order - 1
            */
            for (j <- 0 until order ) {
                if(j == 0) {
                    qpow(i)(j) := 1.U
                } else {
                    qpow(i)(j) := qpow(i)(j-1) * quotients(i-1)
                }
            }
            //Y =  SUM of ( (PascalaTriangle(order, iterator) * Q^iterator) << iterator )  with iterator from 0 to order - 1
            ys(i) := (0 to (order - 1)).toList map ( k => ( ( pascal_triangle(order, k).asUInt * qpow(i)(k) ) << k ) ) reduceLeft (_ + _)
            //REMAINDER = REAMINDER << n + MSB n bits of D
            tmpr(i) := (remainders(i - 1) << order) + ( Reverse(dividents(i - 1)(order - 1, 0)) )
            //D = D drop MSB n bits
            dividents(i) := dividents(i - 1) >> order
            /*
            if(REMAINDER >= Y) { 
                REAMINDER = REMAINDER - Y
            } else {
                REAMINDER = REMAINDER
            }
            */
            remainders(i) := Mux(tmpr(i) >= ys(i), tmpr(i) - ys(i), tmpr(i) )
            /*
            if(REMAINDER >= Y) { 
                Q = Q << 1 + 1
            } else {
                Q = Q << 1
            }
            */
            quotients(i) := Mux(tmpr(i) >= ys(i), (quotients(i - 1) << 1) + 1.U , (quotients(i - 1) << 1)  )
            printf("i: %d, ys %d, tmpr %d, dividents %d, remainders %d, quotients %d\n", i.asUInt,  ys(i), tmpr(i), dividents(i), remainders(i), quotients(i) )
         }
    }

    /*
    results will be the value of quotient the remainder at the last step
    */
    io.q := quotients((real_size / order))(size - 1, 0)
    io.r := remainders((real_size / order))(size - 1, 0)
}
