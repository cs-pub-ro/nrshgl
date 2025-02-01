package ro.upb.nrs.hgl

import chisel3._

class AdditionOperationModule[T <: HNumberRepresentationSystem[T]](genType: () => T) extends Module {
    val io = IO(new Bundle {
      val operand1 = Input(genType().cloneType)
      val operand2 = Input(genType().cloneType)
      val result = Output(genType().cloneType)
    })	
    io.result := io.operand1 + io.operand2
}


class MultiplicationOperationModule[T <: HNumberRepresentationSystem[T]](genType: () => T) extends Module {
    val io = IO(new Bundle {
      val operand1 = Input(genType().cloneType)
      val operand2 = Input(genType().cloneType)
      val result = Output(genType().cloneType)
    })	
    io.result := io.operand1 * io.operand2
}

class DivisionOperationModule[T <: HNumberRepresentationSystem[T]](genType: () => T) extends Module {
    val io = IO(new Bundle {
      val operand1 = Input(genType().cloneType)
      val operand2 = Input(genType().cloneType)
      val result = Output(genType().cloneType)
    })	
    io.result := io.operand1 / io.operand2
}