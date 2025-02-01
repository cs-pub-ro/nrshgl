package ro.upb.nrs.hgl

import chisel3._
import chisel3.util._

class KAU (
    nrs : FloatingPointTrait,
    accumulatorSize : Int,
    accumulatorFractionSize : Int,
    softwareDebug : Boolean = false
) extends Module {
    val io = IO(new Bundle {
      val operand1Value = Input(UInt(nrs.getSize.W))
      val operand2Value = Input(UInt(nrs.getSize.W))
      val inputAccumulator = Input(new Accumulator(
            accumulatorSize,
            accumulatorFractionSize,
            softwareDebug
        ))
      val operationType = Input(UInt(2.W))
      val outputAccumulator = Output(new Accumulator(
            accumulatorSize,
            accumulatorFractionSize,
            softwareDebug
        ))
      val resultValue = Output(UInt(nrs.getSize.W))
    })

    val size : Int = nrs.getSize
    val internalExponentSize : Int = nrs.getInternalExponentSize
    val internalFractionSize : Int = nrs.getInternalFractionSize

    val operand1Decoder : DecoderFloatingPoint = Module(
        nrs.getDecoderFloatingPoint(
            internalExponentSize,
            internalFractionSize,
            size,
            softwareDebug
        )
    )

    operand1Decoder.io.binary := io.operand1Value

    val operand2Decoder : DecoderFloatingPoint = Module(
        nrs.getDecoderFloatingPoint(
            internalExponentSize,
            internalFractionSize,
            size,
            softwareDebug
        )
    )
    operand2Decoder.io.binary := io.operand2Value

    val operand1FloatingPoint = Wire(
                                    new FloatingPoint(
                                        internalExponentSize,
                                        internalFractionSize,
                                        softwareDebug
                                    )
                                )
    operand1FloatingPoint := operand1Decoder.io.result

    val operand2FloatingPoint = Wire(
                                    new FloatingPoint(
                                        internalExponentSize,
                                        internalFractionSize,
                                        softwareDebug
                                    )
                                )
    operand2FloatingPoint := operand2Decoder.io.result

    val resultFloatingPoint = Wire(
                                    new FloatingPoint(
                                        internalExponentSize,
                                        internalFractionSize,
                                        softwareDebug
                                    )
                                )
    
    val oneFloatingPoint = Wire(
                                    new FloatingPoint(
                                        internalExponentSize,
                                        internalFractionSize,
                                        softwareDebug
                                    )
                                )
    oneFloatingPoint.sign := false.B
    oneFloatingPoint.zero := false.B
    oneFloatingPoint.inf := false.B
    oneFloatingPoint.nan := false.B
    oneFloatingPoint.underflow := false.B
    oneFloatingPoint.overflow := false.B
    oneFloatingPoint.exponentSign := false.B
    oneFloatingPoint.exponentAbsoluteValue := 0.U
    oneFloatingPoint.mantissa := 1.U(1.W) ## 0.U(internalFractionSize.W)
    oneFloatingPoint.restBits := 0.U(3.W)
    
    when(io.operationType === 0.U) {
        io.outputAccumulator := io.inputAccumulator + operand1FloatingPoint.fm(operand2FloatingPoint, accumulatorSize, accumulatorFractionSize)
    } .elsewhen(io.operationType === 1.U) {
        io.outputAccumulator := io.inputAccumulator - operand1FloatingPoint.fm(operand2FloatingPoint, accumulatorSize, accumulatorFractionSize)
    } .elsewhen(io.operationType === 2.U) { //clear
        io.outputAccumulator.value := 0.U
        io.outputAccumulator.underflow := false.B
        io.outputAccumulator.overflow := false.B
    } .elsewhen(io.operationType === 3.U) { //faa <- fma(a, 1) fma(b, 1)
        io.outputAccumulator := io.inputAccumulator + 
                    operand1FloatingPoint.fm(oneFloatingPoint, accumulatorSize, accumulatorFractionSize) +
                    operand2FloatingPoint.fm(oneFloatingPoint, accumulatorSize, accumulatorFractionSize)
    } .otherwise {
        io.outputAccumulator.value := 0.U
        io.outputAccumulator.underflow := false.B
        io.outputAccumulator.overflow := false.B
    }

    resultFloatingPoint := io.outputAccumulator.toFloatingPoint(internalExponentSize, internalFractionSize)

    val resultEncoder : EncoderFloatingPoint = Module(
        nrs.getEncoderFloatingPoint(
            internalExponentSize,
            internalFractionSize,
            size,
            softwareDebug
        )
    )
    
    resultEncoder.io.floatingPoint := resultFloatingPoint
    io.resultValue := resultEncoder.io.binary

    if (softwareDebug) {
        //operand1
        printf("[KAU] operand1FloatingPoint.sign DEC: %d, operand1FloatingPoint.sign HEX %x\n",
        operand1FloatingPoint.sign, operand1FloatingPoint.sign)
        printf("[KAU] operand1FloatingPoint.nan DEC: %d, operand1FloatingPoint.nan HEX %x\n",
        operand1FloatingPoint.nan, operand1FloatingPoint.nan)
        printf("[KAU] operand1FloatingPoint.zero DEC: %d, operand1FloatingPoint.zero HEX %x\n",
        operand1FloatingPoint.zero, operand1FloatingPoint.zero)
        printf("[KAU] operand1FloatingPoint.inf DEC: %d, operand1FloatingPoint.inf HEX %x\n",
        operand1FloatingPoint.inf, operand1FloatingPoint.inf)
        printf("[KAU] operand1FloatingPoint.overflow DEC: %d, operand1FloatingPoint.overflow HEX %x\n",
        operand1FloatingPoint.overflow, operand1FloatingPoint.overflow)
        printf("[KAU] operand1FloatingPoint.underflow DEC: %d, operand1FloatingPoint.underflow HEX %x\n",
        operand1FloatingPoint.underflow, operand1FloatingPoint.underflow)
        printf("[KAU] operand1FloatingPoint.exponentSign DEC: %d, operand1FloatingPoint.exponentSign HEX %x\n",
        operand1FloatingPoint.exponentSign, operand1FloatingPoint.exponentSign)
        printf("[KAU] operand1FloatingPoint.exponentAbsoluteValue DEC: %d, operand1FloatingPoint.exponentAbsoluteValue HEX %x\n",
        operand1FloatingPoint.exponentAbsoluteValue, operand1FloatingPoint.exponentAbsoluteValue)
        printf("[KAU] operand1FloatingPoint.mantissa DEC: %d, operand1FloatingPoint.mantissa HEX %x\n",
        operand1FloatingPoint.mantissa, operand1FloatingPoint.mantissa)
        printf("[KAU] operand1FloatingPoint.restBits DEC: %d, operand1FloatingPoint.restBits HEX %x\n",
        operand1FloatingPoint.restBits, operand1FloatingPoint.restBits)
        //operand2
        printf("[KAU] operand2FloatingPoint.sign DEC: %d, operand2FloatingPoint.sign HEX %x\n",
        operand2FloatingPoint.sign, operand2FloatingPoint.sign)
        printf("[KAU] operand2FloatingPoint.nan DEC: %d, operand2FloatingPoint.nan HEX %x\n",
        operand2FloatingPoint.nan, operand2FloatingPoint.nan)
        printf("[KAU] operand2FloatingPoint.zero DEC: %d, operand2FloatingPoint.zero HEX %x\n",
        operand2FloatingPoint.zero, operand2FloatingPoint.zero)
        printf("[KAU] operand2FloatingPoint.inf DEC: %d, operand2FloatingPoint.inf HEX %x\n",
        operand2FloatingPoint.inf, operand2FloatingPoint.inf)
        printf("[KAU] operand2FloatingPoint.overflow DEC: %d, operand2FloatingPoint.overflow HEX %x\n",
        operand2FloatingPoint.overflow, operand2FloatingPoint.overflow)
        printf("[KAU] operand2FloatingPoint.underflow DEC: %d, operand2FloatingPoint.underflow HEX %x\n",
        operand2FloatingPoint.underflow, operand2FloatingPoint.underflow)
        printf("[KAU] operand2FloatingPoint.exponentSign DEC: %d, operand2FloatingPoint.exponentSign HEX %x\n",
        operand2FloatingPoint.exponentSign, operand2FloatingPoint.exponentSign)
        printf("[KAU] operand2FloatingPoint.exponentAbsoluteValue DEC: %d, operand2FloatingPoint.exponentAbsoluteValue HEX %x\n",
        operand2FloatingPoint.exponentAbsoluteValue, operand2FloatingPoint.exponentAbsoluteValue)
        printf("[KAU] operand2FloatingPoint.mantissa DEC: %d, operand2FloatingPoint.mantissa HEX %x\n",
        operand2FloatingPoint.mantissa, operand2FloatingPoint.mantissa)
        printf("[KAU] operand2FloatingPoint.restBits DEC: %d, operand2FloatingPoint.restBits HEX %x\n",
        operand2FloatingPoint.restBits, operand2FloatingPoint.restBits)
        //result
        printf("[KAU] resultFloatingPoint.sign DEC: %d, resultFloatingPoint.sign HEX %x\n",
        resultFloatingPoint.sign, resultFloatingPoint.sign)
        printf("[KAU] resultFloatingPoint.nan DEC: %d, resultFloatingPoint.nan HEX %x\n",
        resultFloatingPoint.nan, resultFloatingPoint.nan)
        printf("[KAU] resultFloatingPoint.zero DEC: %d, resultFloatingPoint.zero HEX %x\n",
        resultFloatingPoint.zero, resultFloatingPoint.zero)
        printf("[KAU] resultFloatingPoint.inf DEC: %d, resultFloatingPoint.inf HEX %x\n",
        resultFloatingPoint.inf, resultFloatingPoint.inf)
        printf("[KAU] resultFloatingPoint.overflow DEC: %d, resultFloatingPoint.overflow HEX %x\n",
        resultFloatingPoint.overflow, resultFloatingPoint.overflow)
        printf("[KAU] resultFloatingPoint.underflow DEC: %d, resultFloatingPoint.underflow HEX %x\n",
        resultFloatingPoint.underflow, resultFloatingPoint.underflow)
        printf("[KAU] resultFloatingPoint.exponentSign DEC: %d, resultFloatingPoint.exponentSign HEX %x\n",
        resultFloatingPoint.exponentSign, resultFloatingPoint.exponentSign)
        printf("[KAU] resultFloatingPoint.exponentAbsoluteValue DEC: %d, resultFloatingPoint.exponentAbsoluteValue HEX %x\n",
        resultFloatingPoint.exponentAbsoluteValue, resultFloatingPoint.exponentAbsoluteValue)
        printf("[KAU] resultFloatingPoint.mantissa DEC: %d, resultFloatingPoint.mantissa HEX %x\n",
        resultFloatingPoint.mantissa, resultFloatingPoint.mantissa)
        printf("[KAU] resultFloatingPoint.restBits DEC: %d, resultFloatingPoint.restBits HEX %x\n",
        resultFloatingPoint.restBits, resultFloatingPoint.restBits)
        //input accumulator
        printf("[KAU] io.inputAccumulator.value DEC: %d, io.inputAccumulator.value HEX %x\n",
        io.inputAccumulator.value, io.inputAccumulator.value)
        printf("[KAU] io.inputAccumulator.overflow DEC: %d, io.inputAccumulator.overflow HEX %x\n",
        io.inputAccumulator.overflow, io.inputAccumulator.overflow)
        printf("[KAU] io.inputAccumulator.underflow DEC: %d, io.inputAccumulator.underflow HEX %x\n",
        io.inputAccumulator.underflow, io.inputAccumulator.underflow)
        //output accumulator
        printf("[KAU] io.outputAccumulator.value DEC: %d, io.outputAccumulator.value HEX %x\n",
        io.outputAccumulator.value, io.outputAccumulator.value)
        printf("[KAU] io.outputAccumulator.overflow DEC: %d, io.outputAccumulator.overflow HEX %x\n",
        io.outputAccumulator.overflow, io.outputAccumulator.overflow)
        printf("[KAU] io.outputAccumulator.underflow DEC: %d, io.outputAccumulator.underflow HEX %x\n",
        io.outputAccumulator.underflow, io.outputAccumulator.underflow)
    }
}