package ro.upb.nrs.hgl

import chisel3._
import chisel3.util._

class TFPU (
    nrs : FloatingPointTrait,
    softwareDebug : Boolean = false
) extends Module {
    val io = IO(new Bundle {
      val operand1Value = Input(UInt(nrs.getSize.W))
      val operand2Value = Input(UInt(nrs.getSize.W))
      val operationType = Input(UInt(2.W))
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
        printf("[TFPU] operand1FloatingPoint.sign DEC: %d, operand1FloatingPoint.sign HEX %x\n",
        operand1FloatingPoint.sign, operand1FloatingPoint.sign)
        printf("[TFPU] operand1FloatingPoint.nan DEC: %d, operand1FloatingPoint.nan HEX %x\n",
        operand1FloatingPoint.nan, operand1FloatingPoint.nan)
        printf("[TFPU] operand1FloatingPoint.zero DEC: %d, operand1FloatingPoint.zero HEX %x\n",
        operand1FloatingPoint.zero, operand1FloatingPoint.zero)
        printf("[TFPU] operand1FloatingPoint.inf DEC: %d, operand1FloatingPoint.inf HEX %x\n",
        operand1FloatingPoint.inf, operand1FloatingPoint.inf)
        printf("[TFPU] operand1FloatingPoint.overflow DEC: %d, operand1FloatingPoint.overflow HEX %x\n",
        operand1FloatingPoint.overflow, operand1FloatingPoint.overflow)
        printf("[TFPU] operand1FloatingPoint.underflow DEC: %d, operand1FloatingPoint.underflow HEX %x\n",
        operand1FloatingPoint.underflow, operand1FloatingPoint.underflow)
        printf("[TFPU] operand1FloatingPoint.exponentSign DEC: %d, operand1FloatingPoint.exponentSign HEX %x\n",
        operand1FloatingPoint.exponentSign, operand1FloatingPoint.exponentSign)
        printf("[TFPU] operand1FloatingPoint.exponentAbsoluteValue DEC: %d, operand1FloatingPoint.exponentAbsoluteValue HEX %x\n",
        operand1FloatingPoint.exponentAbsoluteValue, operand1FloatingPoint.exponentAbsoluteValue)
        printf("[TFPU] operand1FloatingPoint.mantissa DEC: %d, operand1FloatingPoint.mantissa HEX %x\n",
        operand1FloatingPoint.mantissa, operand1FloatingPoint.mantissa)
        printf("[TFPU] operand1FloatingPoint.restBits DEC: %d, operand1FloatingPoint.restBits HEX %x\n",
        operand1FloatingPoint.restBits, operand1FloatingPoint.restBits)
        //operand2
        printf("[TFPU] operand2FloatingPoint.sign DEC: %d, operand2FloatingPoint.sign HEX %x\n",
        operand2FloatingPoint.sign, operand2FloatingPoint.sign)
        printf("[TFPU] operand2FloatingPoint.nan DEC: %d, operand2FloatingPoint.nan HEX %x\n",
        operand2FloatingPoint.nan, operand2FloatingPoint.nan)
        printf("[TFPU] operand2FloatingPoint.zero DEC: %d, operand2FloatingPoint.zero HEX %x\n",
        operand2FloatingPoint.zero, operand2FloatingPoint.zero)
        printf("[TFPU] operand2FloatingPoint.inf DEC: %d, operand2FloatingPoint.inf HEX %x\n",
        operand2FloatingPoint.inf, operand2FloatingPoint.inf)
        printf("[TFPU] operand2FloatingPoint.overflow DEC: %d, operand2FloatingPoint.overflow HEX %x\n",
        operand2FloatingPoint.overflow, operand2FloatingPoint.overflow)
        printf("[TFPU] operand2FloatingPoint.underflow DEC: %d, operand2FloatingPoint.underflow HEX %x\n",
        operand2FloatingPoint.underflow, operand2FloatingPoint.underflow)
        printf("[TFPU] operand2FloatingPoint.exponentSign DEC: %d, operand2FloatingPoint.exponentSign HEX %x\n",
        operand2FloatingPoint.exponentSign, operand2FloatingPoint.exponentSign)
        printf("[TFPU] operand2FloatingPoint.exponentAbsoluteValue DEC: %d, operand2FloatingPoint.exponentAbsoluteValue HEX %x\n",
        operand2FloatingPoint.exponentAbsoluteValue, operand2FloatingPoint.exponentAbsoluteValue)
        printf("[TFPU] operand2FloatingPoint.mantissa DEC: %d, operand2FloatingPoint.mantissa HEX %x\n",
        operand2FloatingPoint.mantissa, operand2FloatingPoint.mantissa)
        printf("[TFPU] operand2FloatingPoint.restBits DEC: %d, operand2FloatingPoint.restBits HEX %x\n",
        operand2FloatingPoint.restBits, operand2FloatingPoint.restBits)
        //result
        printf("[TFPU] resultFloatingPoint.sign DEC: %d, resultFloatingPoint.sign HEX %x\n",
        resultFloatingPoint.sign, resultFloatingPoint.sign)
        printf("[TFPU] resultFloatingPoint.nan DEC: %d, resultFloatingPoint.nan HEX %x\n",
        resultFloatingPoint.nan, resultFloatingPoint.nan)
        printf("[TFPU] resultFloatingPoint.zero DEC: %d, resultFloatingPoint.zero HEX %x\n",
        resultFloatingPoint.zero, resultFloatingPoint.zero)
        printf("[TFPU] resultFloatingPoint.inf DEC: %d, resultFloatingPoint.inf HEX %x\n",
        resultFloatingPoint.inf, resultFloatingPoint.inf)
        printf("[TFPU] resultFloatingPoint.overflow DEC: %d, resultFloatingPoint.overflow HEX %x\n",
        resultFloatingPoint.overflow, resultFloatingPoint.overflow)
        printf("[TFPU] resultFloatingPoint.underflow DEC: %d, resultFloatingPoint.underflow HEX %x\n",
        resultFloatingPoint.underflow, resultFloatingPoint.underflow)
        printf("[TFPU] resultFloatingPoint.exponentSign DEC: %d, resultFloatingPoint.exponentSign HEX %x\n",
        resultFloatingPoint.exponentSign, resultFloatingPoint.exponentSign)
        printf("[TFPU] resultFloatingPoint.exponentAbsoluteValue DEC: %d, resultFloatingPoint.exponentAbsoluteValue HEX %x\n",
        resultFloatingPoint.exponentAbsoluteValue, resultFloatingPoint.exponentAbsoluteValue)
        printf("[TFPU] resultFloatingPoint.mantissa DEC: %d, resultFloatingPoint.mantissa HEX %x\n",
        resultFloatingPoint.mantissa, resultFloatingPoint.mantissa)
        printf("[TFPU] resultFloatingPoint.restBits DEC: %d, resultFloatingPoint.restBits HEX %x\n",
        resultFloatingPoint.restBits, resultFloatingPoint.restBits)
    }
}