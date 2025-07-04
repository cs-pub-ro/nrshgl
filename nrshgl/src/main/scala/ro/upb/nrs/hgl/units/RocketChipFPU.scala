package ro.upb.nrs.hgl

import chisel3._
import chisel3.util._
import chisel3.{DontCare, WireInit, withClock, withReset}
import chisel3.experimental.SourceInfo
import chisel3.experimental.dataview._
import ro.upb.nrs.hgl._

trait NRS {
  def getInternalExponentSize(expWidth : Int, sigWidth : Int) : Int
  def getInternalFractionSize(expWidth : Int, sigWidth : Int) : Int

  def getEncoder(expWidth : Int, sigWidth : Int, internalExponentSize : Option[Int] = None, internalFractionSize : Option[Int] = None) : EncoderFloatingPoint
  def getDecoder(expWidth : Int, sigWidth : Int, internalExponentSize : Option[Int] = None, internalFractionSize : Option[Int] = None) : DecoderFloatingPoint

  def getZero(expWidth : Int, sigWidth : Int) : UInt
  def getOne(expWidth : Int, sigWidth : Int) : UInt

  def getSignIdx(expWidth : Int, sigWidth : Int) : Int = {
    expWidth + sigWidth - 1
  }
}

object NRS_IEEE754 extends NRS {
  override def getInternalExponentSize(expWidth : Int, sigWidth : Int) : Int = {
    IEEE754.internalExponentSize(expWidth, sigWidth - 1)
  }

  override def getInternalFractionSize(expWidth : Int, sigWidth : Int) : Int = {
    IEEE754.internalFractionSize(expWidth, sigWidth - 1)
  }

  override def getEncoder(expWidth : Int, sigWidth : Int, internalExponentSize : Option[Int] = None, internalFractionSize : Option[Int] = None) : EncoderFloatingPoint = {
    new EncoderIEEE754(expWidth, sigWidth - 1, None, internalExponentSize.getOrElse(getInternalExponentSize(expWidth, sigWidth)),
      internalFractionSize.getOrElse(getInternalFractionSize(expWidth, sigWidth)), expWidth + sigWidth)
  }

  override def getDecoder(expWidth : Int, sigWidth : Int, internalExponentSize : Option[Int] = None, internalFractionSize : Option[Int] = None) : DecoderFloatingPoint = {
    new DecoderIEEE754(expWidth, sigWidth - 1, internalExponentSize.getOrElse(getInternalExponentSize(expWidth, sigWidth)),
      internalFractionSize.getOrElse(getInternalFractionSize(expWidth, sigWidth)), expWidth + sigWidth)
  }

  override def getZero(expWidth : Int, sigWidth : Int) : UInt = {
    0.U((expWidth + sigWidth).W)
  }
  
  override def getOne(expWidth : Int, sigWidth : Int) : UInt = {
    0.U(2.W) ## Fill(expWidth - 1, 1.U(1.W)) ## 0.U((sigWidth - 1).W)
  }
}

object NRS_POSIT extends NRS {
  private def EXP = 2
  override def getInternalExponentSize(expWidth : Int, sigWidth : Int) : Int = {
    Posit.internalExponentSize(EXP, expWidth + sigWidth)
  }

  override def getInternalFractionSize(expWidth : Int, sigWidth : Int) : Int = {
    Posit.internalFractionSize(EXP, expWidth + sigWidth)
  }

  override def getEncoder(expWidth : Int, sigWidth : Int, internalExponentSize : Option[Int] = None, internalFractionSize : Option[Int] = None) : EncoderFloatingPoint = {
    new EncoderPosit(EXP, expWidth + sigWidth, None,
      internalExponentSize.getOrElse(getInternalExponentSize(expWidth, sigWidth)),
      internalFractionSize.getOrElse(getInternalFractionSize(expWidth, sigWidth)))
  }

  override def getDecoder(expWidth : Int, sigWidth : Int, internalExponentSize : Option[Int] = None, internalFractionSize : Option[Int] = None) : DecoderFloatingPoint = {
    new DecoderPosit(EXP, expWidth + sigWidth,
      internalExponentSize.getOrElse(getInternalExponentSize(expWidth, sigWidth)),
      internalFractionSize.getOrElse(getInternalFractionSize(expWidth, sigWidth)))
  }

  override def getZero(expWidth : Int, sigWidth : Int) : UInt = {
    0.U((expWidth + sigWidth).W)
  }
  
  override def getOne(expWidth : Int, sigWidth : Int) : UInt = {
    0.U(1.W) ## 1.U(1.W) ## 0.U((expWidth + sigWidth - 2).W)
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

class ConvertIntegerToFP(integerSize: Int, exponentSize: Int, fractionSize: Int, rounding: RoundingType, nrs: NRS, softwareDebug: Boolean = false) extends Module {
    val io = IO(new Bundle {
        val integer = Input(UInt(integerSize.W))
        val sign = Input(Bool())
        val binary = Output(Bits((exponentSize + fractionSize + 1).W))
    })

    val conv = Module(new IntegerToFloatingPoint(integerSize, exponentSize, fractionSize, softwareDebug))
    val fpResult = Wire(new FloatingPoint(exponentSize, fractionSize))
    conv.io.integer := io.integer
    conv.io.sign := io.sign
    fpResult := conv.io.result

    val encode = Module(nrs.getEncoder(exponentSize, fractionSize + 1))
    encode.io.floatingPoint := fpResult
    io.binary := encode.io.binary
}

class ConvertFPToInteger(integerSize: Int, exponentSize: Int, fractionSize: Int, rounding: RoundingType, nrs : NRS, softwareDebug: Boolean = false) extends Module {
    val io = IO(new Bundle {
        val binary = Input(Bits((exponentSize + fractionSize + 1).W))
        val signOut = Input(Bool())
        val integer = Output(UInt(integerSize.W))
    })

    val decode = Module(nrs.getDecoder(exponentSize, fractionSize + 1))
    decode.io.binary := io.binary

    val conv = Module(new FloatingPointToInteger(integerSize, exponentSize, fractionSize, softwareDebug))
    conv.io.floatingPoint := decode.io.result
    conv.io.roundingType := RoundingType.toUInt(rounding)
    conv.io.signOut := io.signOut
    io.integer := conv.io.integer
}

class IEEEToPositTest(exp: Int, sig: Int) extends Module {
    val io = IO(new Bundle {
        val ieeeDouble = Input(Bits((exp + sig).W))
        val out = Output(Bits((exp + sig).W))
    })

    val decoder = Module(NRS_IEEE754.getDecoder(exp, sig))
    decoder.io.binary := io.ieeeDouble
    
    val encoder = Module(NRS_POSIT.getEncoder(exp, sig, Some(NRS_IEEE754.getInternalExponentSize(exp, sig)), Some(NRS_IEEE754.getInternalFractionSize(exp, sig))))
    encoder.io.floatingPoint := decoder.io.result
    encoder.io.roundingType match {
        case Some(r) => r := 0.U
        case None => 
    }

    io.out := encoder.io.binary
}
