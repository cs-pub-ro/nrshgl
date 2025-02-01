package ro.upb.nrs.hgl

import chisel3._
import chisel3.experimental.FixedPoint

trait RoundingType {
  
}

case object RoundUp extends RoundingType
case object RoundDown extends RoundingType
case object RoundEven extends RoundingType
case object RoundZero extends RoundingType
case object RoundAwayZero extends RoundingType
case object NoRounding extends RoundingType

trait HNumberRepresentationSystem[T] extends Bundle {
	def +(that: T) : T
	def -(that: T) : T
	def *(that: T) : T
	def /(that: T) : T
	def sqrt : T
}

