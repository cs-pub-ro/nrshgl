package ro.upb.nrs.hgl

import chisel3._
import chisel3.util._

class Accumulator(size : Int, fractionSize : Int, softwareDebug : Boolean = false) extends Bundle {

    val value : UInt = Output(UInt(size.W))
    val overflow : Bool = Output(Bool())
    val underflow : Bool = Output(Bool())

    def +(that : Accumulator) : Accumulator = {
		val out = Wire(new Accumulator(size, fractionSize, softwareDebug))
        out.value := this.value + that.value
        out.overflow := this.overflow | that.overflow | (!this.value(size-1) & !this.value(size-1) & out.value(size-1))
        out.underflow := this.underflow | that.underflow | (this.value(size-1) & this.value(size-1) & !out.value(size-1))
        out
    }

    def -(that : Accumulator) : Accumulator = this + (-that)

    def unary_- : Accumulator = {
		val out = Wire(new Accumulator(size, fractionSize, softwareDebug))
        out.value := ~this.value + 1.U
        out.overflow := this.overflow
        out.underflow := this.underflow
        out
    }

    def toFloatingPoint(internalExponentSize : Int, internalFractionSize : Int) : FloatingPoint = {
        val floatingPoint = Wire(new FloatingPoint(
                internalExponentSize,
                internalFractionSize,
                this.softwareDebug
            )
        )

        //initialise
        floatingPoint.sign := this.value(size-1)
        floatingPoint.zero := !this.value.orR
        floatingPoint.inf := false.B
        floatingPoint.nan := false.B
        floatingPoint.underflow := false.B
        floatingPoint.overflow := false.B
        floatingPoint.restBits := 0.U(3.W)
        floatingPoint.exponentSign := false.B
        floatingPoint.exponentAbsoluteValue := 0.U
        floatingPoint.mantissa := 0.U

        //compute
        when(this.underflow) {
            floatingPoint.sign := true.B
            floatingPoint.overflow := true.B
        } .elsewhen(this.overflow) {
            floatingPoint.sign := false.B
            floatingPoint.overflow := true.B
        } .elsewhen(floatingPoint.zero) {
            floatingPoint.restBits := 0.U(3.W)
        } .otherwise {
            val decodeBits = Wire(UInt((this.size-1).W))
            decodeBits := Mux(floatingPoint.sign, ~this.value + 1.U, this.value)
            val shiftLeft = Wire(UInt(log2Ceil(this.size).W))
            shiftLeft := PriorityEncoder(Reverse(decodeBits))
            val msbIndex = Wire(UInt(log2Ceil(this.size).W))
            msbIndex := (this.size-2).U - shiftLeft
            val exponentSign = Wire(Bool())
            exponentSign := msbIndex < fractionSize.U
            val exponentAbsoluteValue = Wire(UInt(log2Ceil(this.size).W))
            exponentAbsoluteValue := Mux(
                exponentSign,
                fractionSize.U - msbIndex,
                msbIndex - fractionSize.U
            )
            floatingPoint.exponentSign := exponentSign
            floatingPoint.exponentAbsoluteValue := exponentAbsoluteValue
            if(internalExponentSize < log2Ceil(this.size)) {
                floatingPoint.overflow := exponentAbsoluteValue(log2Ceil(this.size)-1, internalExponentSize).orR & !exponentSign
                floatingPoint.underflow := exponentAbsoluteValue(log2Ceil(this.size)-1, internalExponentSize).orR & exponentSign
            } else {
                floatingPoint.overflow := false.B
                floatingPoint.underflow := false.B
            }
            val mantissaBits = Wire(UInt((this.size-1).W))
            mantissaBits := decodeBits << shiftLeft
            floatingPoint.mantissa := mantissaBits(this.size - 2, this.size - 2 - internalFractionSize)
            if((this.size - internalFractionSize) >= 6) {
                floatingPoint.restBits := mantissaBits(this.size - 3 - internalFractionSize, this.size - 5 - internalFractionSize) |
                                        mantissaBits(this.size - 6 - internalFractionSize, 0).orR
            } else {
                printf("[Accumulator][ERROR] size DEC: %d, internalFractionSize HEX %x\n", this.size.U, internalFractionSize.U)
            }

            if(softwareDebug) {
                printf("[Accumulator] mantissaBits DEC: %d, mantissaBits HEX %x\n", mantissaBits, mantissaBits)
                printf("[Accumulator] shiftLeft DEC: %d, shiftLeft HEX %x\n", shiftLeft, shiftLeft)
                printf("[Accumulator] msbIndex DEC: %d, msbIndex HEX %x\n", msbIndex, msbIndex)
            }
        }


        //result
        floatingPoint
    }

}