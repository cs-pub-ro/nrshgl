package ro.upb.nrs.hgl.performance
import ro.upb.nrs.hgl.HNumberRepresentationSystem
import chisel3._
import fixedpoint._

case class Natural(lit: Option[Int] = None, size: Int) extends HNumberRepresentationSystem[Natural] {

	val value: UInt = lit match {
		case Some(x) => x.U(size.W)
		case _ => Output(UInt(size.W))
	}

	override def +(that: Natural): Natural = {
		val adder = Module(new NaturalAdd(size))
		adder.io.op1 := value
		adder.io.op2 := that.value
		val out = Wire(Natural(size))
		out.value := adder.io.res
		out
	}

	override def -(that: Natural): Natural = {
		val subtractor = Module(new NaturalSub(size))
		subtractor.io.op1 := value
		subtractor.io.op2 := that.value
		val out = Wire(Natural(size))
		out.value := subtractor.io.res
		out
	}

	override def *(that: Natural): Natural = {
		val multiplier = Module(new NaturalMultiply(size))
		multiplier.io.op1 := value
		multiplier.io.op2 := that.value
		val out = Wire(Natural(size))
		out.value := multiplier.io.res
		out
	}

	override def /(that: Natural): Natural = {
		val divider = Module(new NaturalDiv(size))
		divider.io.op1 := value
		divider.io.op2 := that.value
		val out = Wire(Natural(size))
		out.value := divider.io.res
		out
	}

	def %(that: Natural): Natural = {
		val modulo = Module(new NaturalMod(size))
		modulo.io.op1 := value
		modulo.io.op2 := that.value
		val out = Wire(Natural(size))
		out.value := modulo.io.res
		out
	}
	
	override def sqrt: Natural = ???

}

object Natural {
	
	def apply(size: Int): Natural = new Natural(None, size)
	def apply(size:Int, value: Double): Natural = {
		require(value > 0.0)
		new Natural(Some(value.toInt), size)
	}
	
}


case class Integer(lit : Option[BigInt] = None, size: Int) extends Bundle with HNumberRepresentationSystem[Integer] {

	val value: SInt = lit match {
		case Some(x) => x.S(size.W)
		case _ => Output(SInt(size.W))
	}
	override def +(that: Integer): Integer = {
		val adder = Module(new ZAdd(size))
		adder.io.op1 := value
		adder.io.op2 := that.value
		val out = Wire(Integer(size))
		out.value := adder.io.res
		out
	}

	override def -(that: Integer): Integer = {
		val subtractor =  Module(new ZSub(size))
		subtractor.io.op1 := value
		subtractor.io.op2 := that.value
		val out = Wire(Integer(size))
		out.value := subtractor.io.res
		out
	}

	override def *(that: Integer): Integer = {
		val multiplier = Module(new ZMultiply(size))
		multiplier.io.op1 := value
		multiplier.io.op2 := that.value
		val out = Wire(Integer(size))
		out.value := multiplier.io.res
		out
	}

	override def /(that: Integer): Integer = {
		val divider = Module(new ZDiv(size))
		divider.io.op1 := value
		divider.io.op2 := that.value
		val out = Wire(Integer(size))
		out.value := divider.io.res
		out
	}

	def %(that: Integer): Integer = {
		val modulo = Module(new ZModulo(size))
		modulo.io.op1 := value
		modulo.io.op2 := that.value
		val out = Wire(Integer(size))
		out.value := modulo.io.res
		out
	}

	override def sqrt: Integer = ???

}

object Integer {

	def apply(size: Int): Integer = new Integer(None, size)
	def apply(value: Double, size: Int): Integer = new Integer(Some(value.toInt), size)
}


case class Fractional(lit: Option[Int] = None, size_numerator: Int, size_denominator: Int) extends Bundle with HNumberRepresentationSystem[Fractional] {

	val value = lit match {
		case Some(x) => chiselTypeOf(x.U((size_numerator + size_denominator + 1).W))
		case _ => Output(UInt((size_numerator + size_denominator + 1).W))
	}

	override def +(that: Fractional): Fractional =
	{
		val adder = Module(new FractionalAddSub(size_numerator, size_denominator))
		adder.io.op1 := value
		adder.io.op2 := that.value
		adder.io.op_sel := false.B
		val out = Wire(Fractional(size_numerator, size_denominator))
		out.value := adder.io.res
		out
	}

	override def -(that: Fractional): Fractional = {
		val subtractor = Module(new FractionalAddSub(size_numerator, size_denominator))
		subtractor.io.op1 := value
		subtractor.io.op2 := that.value
		subtractor.io.op_sel := true.B
		val out = Wire(Fractional(size_numerator, size_denominator))
		out.value := subtractor.io.res
		out
	}

	override def *(that: Fractional): Fractional = {
		val multiplier = Module(new FractionalMulDiv(size_numerator, size_denominator))
		multiplier.io.op1 := value
		multiplier.io.op2 := that.value
		multiplier.io.op_sel := false.B
		val out = Wire(Fractional(size_numerator, size_denominator))
		out.value := multiplier.io.res
		out
	}

	override def /(that: Fractional): Fractional = {
		val divider = Module(new FractionalMulDiv(size_numerator, size_denominator))
		divider.io.op1 := value
		divider.io.op2 := that.value
		divider.io.op_sel := true.B
		val out = Wire(Fractional(size_numerator, size_denominator))
		out.value := divider.io.res
		out
	}
	
	override def sqrt: Fractional = ???
}

object Fractional {
	def apply(size_numerator: Int, size_denominator: Int): Fractional = new Fractional(None, size_numerator, size_denominator)
	def apply(x: Int, size_numerator: Int, size_denominator: Int): Fractional = new Fractional(Some(x), size_numerator, size_denominator)
}


case class FloatP(lit: Option[Int] = None, esize: Int, fsize: Int) extends Bundle with HNumberRepresentationSystem[FloatP] {

	val value = lit match {
		case Some(x) => chiselTypeOf(x.U((1 + esize + fsize).W))
		case _ => Output(UInt((1 + esize + fsize).W)) 
	}
	override def +(that: FloatP): FloatP = {
		val adder = Module(new FloatingPointAddSub(esize, fsize))
		adder.io.op1 := value
		adder.io.op2 := that.value
		adder.io.opSel := false.B
		val out = Wire(FloatP(esize, fsize))
		out.value := adder.io.res
		out
	}

	override def -(that: FloatP): FloatP = {
		val subtractor = Module(new FloatingPointAddSub(esize, fsize))
		subtractor.io.op1 := value
		subtractor.io.op2 := that.value
		subtractor.io.opSel := true.B
		val out = Wire(FloatP(esize, fsize))
		out.value := subtractor.io.res
		out
	}

	override def *(that: FloatP): FloatP = {
		val multiplier = Module(new FloatingPointMultiply(esize, fsize))
		multiplier.io.op1 := value
		multiplier.io.op2 := that.value
		val out = Wire(FloatP(esize, fsize))
		out.value := multiplier.io.res
		out
	}

	override def /(that: FloatP): FloatP = {
		val divider = Module(new FloatingPointDiv(esize, fsize))
		divider.io.op1 := value
		divider.io.op2 := that.value
		val out = Wire(FloatP(esize, fsize))
		out.value := divider.io.res
		out
	}
	
	override def sqrt: FloatP = ???
}

object FloatP {
	def apply(esize: Int, fsize: Int): FloatP = new FloatP(None, esize, fsize)
	def apply(value: Double, esize: Int, fsize: Int) = new FloatP(Some(value.toInt), esize, fsize) //possible wrong
}

case class FixedPointQ(lit: Option[Double] = None, size: Int, binaryPointPos: Int) extends Bundle with HNumberRepresentationSystem[FixedPointQ] {

	val value = lit match {
		case Some(x: Double) => x.F(binaryPointPos.BP)
		case _ => Output(FixedPoint(size.W, binaryPointPos.BP))  
	}
	override def +(that: FixedPointQ): FixedPointQ = {
		val adder = Module(new FixedPointQAdd(size, binaryPointPos))
		adder.io.op1 := value
		adder.io.op2 := that.value
		val out = Wire(FixedPointQ(size, binaryPointPos))
		out.value := adder.io.res
		out
	}

	override def -(that: FixedPointQ): FixedPointQ = {
		val subtractor = Module(new FixedPointQSub(size, binaryPointPos))
		subtractor.io.op1 := value
		subtractor.io.op2 := that.value
		val out = Wire(FixedPointQ(size, binaryPointPos))
		out.value := subtractor.io.res
		out
	}

	override def *(that: FixedPointQ): FixedPointQ = {
		val multiplier = Module(new FixedPointQMul(size, binaryPointPos))
		multiplier.io.op1 := value
		multiplier.io.op2 := that.value
		val out = Wire(FixedPointQ(size, binaryPointPos))
		out.value := multiplier.io.res
		out
	}

	override def /(that: FixedPointQ): FixedPointQ = {
		val divider = Module(new FixedPointQDiv(size, binaryPointPos))
		divider.io.op1 := value
		divider.io.op2 := that.value
		val out = Wire(FixedPointQ(size, binaryPointPos))
		out.value := divider.io.res
		out
	}
	
	override def sqrt: FixedPointQ = ???
}

object FixedPointQ {
	def apply(size: Int, fsize: Int): FixedPointQ = new FixedPointQ(None, size, fsize)
	def apply(value: Double, size: Int, fsize: Int): FixedPointQ = new FixedPointQ(Some(value), size, fsize)
}

case class Posit(lit: Option[BigInt] = None, exponent_size: Int = 2, size: Int) extends Bundle with HNumberRepresentationSystem[Posit] {

	val value = lit match {
		case Some(x) => chiselTypeOf(x.U(size.W))
		case _ => Output(UInt(size.W))
	}
	override def +(that: Posit): Posit = {
		val adder = Module(new PositAddSub(exponent_size, size))
		adder.io.op1 := value
		adder.io.op2 := that.value
		adder.io.op_sel := false.B
		val out = Wire(Posit(exponent_size, size))
		out.value := adder.io.res
		out
	}

	override def -(that: Posit): Posit = {
		val subtractor = Module(new PositAddSub(exponent_size, size))
		subtractor.io.op1 := value
		subtractor.io.op2 := that.value
		subtractor.io.op_sel := true.B
		val out = Wire(Posit(exponent_size, size))
		out.value := subtractor.io.res
		out
	}

	override def *(that: Posit): Posit = {
		val multiplier = Module(new PositMul(exponent_size, size))
		multiplier.io.op1 := value
		multiplier.io.op2 := that.value
		val out = Wire(Posit(exponent_size, size))
		out.value := multiplier.io.res
		out
	}

	override def /(that: Posit): Posit = {
		val divider = Module(new PositDiv(exponent_size, size))
		divider.io.op1 := value
		divider.io.op2 := that.value
		val out = Wire(Posit(exponent_size, size))
		out.value := divider.io.res
		out
	}

	override def sqrt: Posit = ???
}

object Posit {
	def apply(exponent_size: Int, size: Int): Posit = new Posit(None, exponent_size, size)
	def apply(value: Long, exponent_size: Int, size: Int) = new Posit(Some(value), exponent_size, size)
	def apply(value: Double, exponent_size: Int, size: Int) = new Posit(Some(value.toLong), exponent_size, size) //possible wrong
}