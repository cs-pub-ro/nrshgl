package ro.upb.nrs.hgl

import chisel3._
import ro.upb.nrs.hgl._

trait NRS {
  def getEncoder(expWidth : Int, sigWidth : Int) : EncoderFloatingPoint
  def getDecoder(expWidth : Int, sigWidth : Int) : DecoderFloatingPoint
}

object NRS_IEEE754 extends NRS {
  override def getEncoder(expWidth : Int, sigWidth : Int) : EncoderFloatingPoint = {
    new EncoderIEEE754(expWidth, sigWidth - 1, Some(RoundEven), expWidth, sigWidth - 1,
      expWidth + sigWidth)
  }

  override def getDecoder(expWidth : Int, sigWidth : Int) : DecoderFloatingPoint = {
    new DecoderIEEE754(expWidth, sigWidth - 1, expWidth, sigWidth - 1,
      expWidth + sigWidth)
  }
}

class MulAddRecFNPipe(expWidth: Int, sigWidth: Int, nrs: NRS) extends Module
{
    val io = IO(new Bundle {
        val op = Input(Bits(2.W))
        val a = Input(Bits((expWidth + sigWidth).W))
        val b = Input(Bits((expWidth + sigWidth).W))
        val c = Input(Bits((expWidth + sigWidth).W))
        val out = Output(Bits((expWidth + sigWidth).W))
    })

    //------------------------------------------------------------------------
    //------------------------------------------------------------------------

    val decoder_a = Module(nrs.getDecoder(expWidth, sigWidth))
    val decoder_b = Module(nrs.getDecoder(expWidth, sigWidth))
    val decoder_c = Module(nrs.getDecoder(expWidth, sigWidth))

    val floatingPoint_a = Wire(new FloatingPoint(expWidth, sigWidth - 1))
    val floatingPoint_b = Wire(new FloatingPoint(expWidth, sigWidth - 1))
    val floatingPoint_c = Wire(new FloatingPoint(expWidth, sigWidth - 1))

    decoder_a.io.binary := io.a
    decoder_b.io.binary := io.b
    decoder_c.io.binary := io.c

    floatingPoint_a := decoder_a.io.result
    floatingPoint_b := decoder_b.io.result
    floatingPoint_c := decoder_c.io.result

    val floatingPoint_result = Wire(new FloatingPoint(expWidth, sigWidth - 1))

    floatingPoint_result := 0.U.asTypeOf(floatingPoint_result)

    when (io.op === 0.U) {
        floatingPoint_result := floatingPoint_a * floatingPoint_b + floatingPoint_c
    } .elsewhen (io.op === 1.U) {
        floatingPoint_result := floatingPoint_a * floatingPoint_b - floatingPoint_c
    } .elsewhen (io.op === 2.U) {
        floatingPoint_result := -(floatingPoint_a * floatingPoint_b) + floatingPoint_c
    } .otherwise {
        floatingPoint_result := -(floatingPoint_a * floatingPoint_b) - floatingPoint_c
    }

    val encoder = Module(nrs.getEncoder(expWidth, sigWidth))
    encoder.io.floatingPoint := floatingPoint_result

    io.out            := encoder.io.binary
}