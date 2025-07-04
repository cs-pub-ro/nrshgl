package ro.upb.nrs.hgl

import chisel3._

class MatrixMultiply[T <: HNumberRepresentationSystem[T]](genIn: () => T, genOut: () => T, size: Int) extends Module {
    val io = IO(new Bundle {
      val matrix1 = Input(Vec(size * size, genIn().cloneType))
      val matrix2 = Input(Vec(size * size, genIn().cloneType))
      val matrixRes = Output(Vec(size * size, genIn().cloneType))
    })	

    for (i <- 0 until size) {
        for (j <-0 until size) {
			io.matrixRes(i * size + j) := (0 until size).toList map (k => io.matrix1(i * size + k) * io.matrix2(k * size + j)) reduceLeft(_ + _)
        }
    }

}
