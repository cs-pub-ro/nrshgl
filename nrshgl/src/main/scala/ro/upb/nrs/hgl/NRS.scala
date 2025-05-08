package ro.upb.nrs.hgl

import chisel3._
import fixedpoint._

sealed trait RoundingType
case object RoundUp extends RoundingType
case object RoundDown extends RoundingType
case object RoundEven extends RoundingType
case object RoundZero extends RoundingType
case object RoundAwayZero extends RoundingType
case object NoRounding extends RoundingType

object RoundingType {
  def toUInt(mode: RoundingType): UInt = mode match {
    case RoundEven => 0.U(3.W)
    case RoundZero => 1.U(3.W)
    case RoundDown => 2.U(3.W)
    case RoundUp => 3.U(3.W)
    case RoundAwayZero => 4.U(3.W)
    case NoRounding => 5.U(3.W)
  }
}

trait HNumberRepresentationSystem[T] extends Bundle {
	def +(that: T) : T
	def -(that: T) : T
	def *(that: T) : T
	def /(that: T) : T
	def sqrt : T
}

