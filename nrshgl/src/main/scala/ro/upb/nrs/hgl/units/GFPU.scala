package ro.upb.nrs.hgl

import chisel3._
import chisel3.util._

class GFPU (
    size : Int,
    nrss : List[FloatingPointTrait],
    softwareDebug : Boolean = false
) extends Module {
    val io = IO(new Bundle {
      val operand1Value = Input(UInt(size.W))
      val operand1Type = Input(UInt(log2Ceil(nrss.length).W))
      val operand2Value = Input(UInt(size.W))
      val operand2Type = Input(UInt(log2Ceil(nrss.length).W))
      val operationType = Input(UInt(2.W))
      val resultType = Input(UInt(log2Ceil(nrss.length).W))
      val resultValue = Output(UInt(size.W))
    })

    val internalExponentSize : Int = nrss.map(x => x.getInternalExponentSize).max
    val internalFractionSize : Int = nrss.map(x => x.getInternalFractionSize).max

    val operand1Decoders : List[DecoderFloatingPoint] = nrss.map(
        x => Module(x.getDecoderFloatingPoint(internalExponentSize, internalFractionSize, size, softwareDebug))
    )

    for(decoder <- operand1Decoders) {
        decoder.io.binary := io.operand1Value
    }

    val operand2Decoders : List[DecoderFloatingPoint] = nrss.map(
        x => Module(x.getDecoderFloatingPoint(internalExponentSize, internalFractionSize, size, softwareDebug))
    )

    for(decoder <- operand2Decoders) {
        decoder.io.binary := io.operand2Value
    }

    val operand1Cases = operand1Decoders.zipWithIndex.map(
        {case (decoder, index) => (io.operand1Type === index.U) -> decoder.io.result }
    )
    val operand1FloatingPoint = Wire(
                                    new FloatingPoint(
                                        internalExponentSize,
                                        internalFractionSize,
                                        softwareDebug
                                    )
                                )
    operand1FloatingPoint := MuxCase(operand1Decoders.head.io.result, operand1Cases)

    val operand2Cases = operand2Decoders.zipWithIndex.map(
        {case (decoder, index) => (io.operand2Type === index.U) -> decoder.io.result }
    )
    val operand2FloatingPoint = Wire(
                                    new FloatingPoint(
                                        internalExponentSize,
                                        internalFractionSize,
                                        softwareDebug
                                    )
                                )
    operand2FloatingPoint := MuxCase(operand2Decoders.head.io.result, operand2Cases)

    val resultFloatingPoint = Wire(
                                    new FloatingPoint(
                                        internalExponentSize,
                                        internalFractionSize,
                                        softwareDebug
                                    )
                                )
    
    when(io.operationType === 0.U) {
        resultFloatingPoint := operand1FloatingPoint + operand2FloatingPoint
    } .elsewhen(io.operationType === 1.U) {
        resultFloatingPoint := operand1FloatingPoint - operand2FloatingPoint
    } .elsewhen(io.operationType === 2.U) {
        resultFloatingPoint := operand1FloatingPoint * operand2FloatingPoint
    } .elsewhen(io.operationType === 3.U) {
        resultFloatingPoint := operand1FloatingPoint / operand2FloatingPoint
    } .otherwise {
        resultFloatingPoint := operand1FloatingPoint
    }

    val resultEncoders : List[EncoderFloatingPoint] = nrss.map(
        x => Module(x.getEncoderFloatingPoint(internalExponentSize, internalFractionSize, size, softwareDebug))
    )
    
    for(encoder <- resultEncoders) {
        encoder.io.floatingPoint := resultFloatingPoint
    }
    val resultCases = resultEncoders.zipWithIndex.map(
        {case (encoder, index) => (io.resultType === index.U) -> encoder.io.binary }
    )

    io.resultValue := MuxCase(resultEncoders.head.io.binary, resultCases)

    if (softwareDebug) {
        //operand1
        printf("[GFPU] operand1FloatingPoint.sign DEC: %d, operand1FloatingPoint.sign HEX %x\n",
        operand1FloatingPoint.sign, operand1FloatingPoint.sign)
        printf("[GFPU] operand1FloatingPoint.nan DEC: %d, operand1FloatingPoint.nan HEX %x\n",
        operand1FloatingPoint.nan, operand1FloatingPoint.nan)
        printf("[GFPU] operand1FloatingPoint.zero DEC: %d, operand1FloatingPoint.zero HEX %x\n",
        operand1FloatingPoint.zero, operand1FloatingPoint.zero)
        printf("[GFPU] operand1FloatingPoint.inf DEC: %d, operand1FloatingPoint.inf HEX %x\n",
        operand1FloatingPoint.inf, operand1FloatingPoint.inf)
        printf("[GFPU] operand1FloatingPoint.overflow DEC: %d, operand1FloatingPoint.overflow HEX %x\n",
        operand1FloatingPoint.overflow, operand1FloatingPoint.overflow)
        printf("[GFPU] operand1FloatingPoint.underflow DEC: %d, operand1FloatingPoint.underflow HEX %x\n",
        operand1FloatingPoint.underflow, operand1FloatingPoint.underflow)
        printf("[GFPU] operand1FloatingPoint.exponentSign DEC: %d, operand1FloatingPoint.exponentSign HEX %x\n",
        operand1FloatingPoint.exponentSign, operand1FloatingPoint.exponentSign)
        printf("[GFPU] operand1FloatingPoint.exponentAbsoluteValue DEC: %d, operand1FloatingPoint.exponentAbsoluteValue HEX %x\n",
        operand1FloatingPoint.exponentAbsoluteValue, operand1FloatingPoint.exponentAbsoluteValue)
        printf("[GFPU] operand1FloatingPoint.mantissa DEC: %d, operand1FloatingPoint.mantissa HEX %x\n",
        operand1FloatingPoint.mantissa, operand1FloatingPoint.mantissa)
        printf("[GFPU] operand1FloatingPoint.restBits DEC: %d, operand1FloatingPoint.restBits HEX %x\n",
        operand1FloatingPoint.restBits, operand1FloatingPoint.restBits)
        //operand2
        printf("[GFPU] operand2FloatingPoint.sign DEC: %d, operand2FloatingPoint.sign HEX %x\n",
        operand2FloatingPoint.sign, operand2FloatingPoint.sign)
        printf("[GFPU] operand2FloatingPoint.nan DEC: %d, operand2FloatingPoint.nan HEX %x\n",
        operand2FloatingPoint.nan, operand2FloatingPoint.nan)
        printf("[GFPU] operand2FloatingPoint.zero DEC: %d, operand2FloatingPoint.zero HEX %x\n",
        operand2FloatingPoint.zero, operand2FloatingPoint.zero)
        printf("[GFPU] operand2FloatingPoint.inf DEC: %d, operand2FloatingPoint.inf HEX %x\n",
        operand2FloatingPoint.inf, operand2FloatingPoint.inf)
        printf("[GFPU] operand2FloatingPoint.overflow DEC: %d, operand2FloatingPoint.overflow HEX %x\n",
        operand2FloatingPoint.overflow, operand2FloatingPoint.overflow)
        printf("[GFPU] operand2FloatingPoint.underflow DEC: %d, operand2FloatingPoint.underflow HEX %x\n",
        operand2FloatingPoint.underflow, operand2FloatingPoint.underflow)
        printf("[GFPU] operand2FloatingPoint.exponentSign DEC: %d, operand2FloatingPoint.exponentSign HEX %x\n",
        operand2FloatingPoint.exponentSign, operand2FloatingPoint.exponentSign)
        printf("[GFPU] operand2FloatingPoint.exponentAbsoluteValue DEC: %d, operand2FloatingPoint.exponentAbsoluteValue HEX %x\n",
        operand2FloatingPoint.exponentAbsoluteValue, operand2FloatingPoint.exponentAbsoluteValue)
        printf("[GFPU] operand2FloatingPoint.mantissa DEC: %d, operand2FloatingPoint.mantissa HEX %x\n",
        operand2FloatingPoint.mantissa, operand2FloatingPoint.mantissa)
        printf("[GFPU] operand2FloatingPoint.restBits DEC: %d, operand2FloatingPoint.restBits HEX %x\n",
        operand2FloatingPoint.restBits, operand2FloatingPoint.restBits)
        //result
        printf("[GFPU] resultFloatingPoint.sign DEC: %d, resultFloatingPoint.sign HEX %x\n",
        resultFloatingPoint.sign, resultFloatingPoint.sign)
        printf("[GFPU] resultFloatingPoint.nan DEC: %d, resultFloatingPoint.nan HEX %x\n",
        resultFloatingPoint.nan, resultFloatingPoint.nan)
        printf("[GFPU] resultFloatingPoint.zero DEC: %d, resultFloatingPoint.zero HEX %x\n",
        resultFloatingPoint.zero, resultFloatingPoint.zero)
        printf("[GFPU] resultFloatingPoint.inf DEC: %d, resultFloatingPoint.inf HEX %x\n",
        resultFloatingPoint.inf, resultFloatingPoint.inf)
        printf("[GFPU] resultFloatingPoint.overflow DEC: %d, resultFloatingPoint.overflow HEX %x\n",
        resultFloatingPoint.overflow, resultFloatingPoint.overflow)
        printf("[GFPU] resultFloatingPoint.underflow DEC: %d, resultFloatingPoint.underflow HEX %x\n",
        resultFloatingPoint.underflow, resultFloatingPoint.underflow)
        printf("[GFPU] resultFloatingPoint.exponentSign DEC: %d, resultFloatingPoint.exponentSign HEX %x\n",
        resultFloatingPoint.exponentSign, resultFloatingPoint.exponentSign)
        printf("[GFPU] resultFloatingPoint.exponentAbsoluteValue DEC: %d, resultFloatingPoint.exponentAbsoluteValue HEX %x\n",
        resultFloatingPoint.exponentAbsoluteValue, resultFloatingPoint.exponentAbsoluteValue)
        printf("[GFPU] resultFloatingPoint.mantissa DEC: %d, resultFloatingPoint.mantissa HEX %x\n",
        resultFloatingPoint.mantissa, resultFloatingPoint.mantissa)
        printf("[GFPU] resultFloatingPoint.restBits DEC: %d, resultFloatingPoint.restBits HEX %x\n",
        resultFloatingPoint.restBits, resultFloatingPoint.restBits)
    }
}