package ro.upb.nrs.hgl.performance

import chisel3._
import chisel3.util._


/*
We consider Internal Posit
mostly the same as Normal Posit byt with exponent mantisa and fraction size
double in size. They are the internal size so we can have exact rounding.
rest bits is if we had some other bits different than zero. Eg: remainder != 0
*/
class InternalPosit(exponent_size: Int, size: Int) extends Bundle {
	val restBits = Bool()
	val sign = Bool()
	val exponent = SInt((2 * (log2Ceil(size) + 1 + exponent_size) + 1).W)
	val mantissa = UInt((2 * size + 1).W)
	val fraction_size = UInt((log2Ceil(2 * size + 1)).W)
	val nar = Bool()
	val zero = Bool()
}

class InternalPositIO(exponent_size: Int, size: Int) extends Bundle {
	val posit = Input(new InternalPosit(exponent_size, size))
    val binary = Output(UInt(size.W))
}

/*
We get internal posit that has all the components on double the 
the size of normal posit
*/
class NormalisePosit(exponent_size: Int, size: Int) extends Module {
	val io = IO(new InternalPositIO(exponent_size, size))

    //printf("[NORMALISE] io.exponent: %d, io.mantissa %x\n", io.posit.exponent, io.posit.mantissa)
    //printf("[NORMALISE] io.fraction_size: %d, io.restBits %x\n", io.posit.fraction_size, io.posit.restBits)
	/*
	shift mantissa left so the fraction size will be 2*size-1
	for the nromalised_mantissa. The partial_mantissa will be shifeted
	left with the respect to the new fraction size
	new_matinsa = old_mantissa << (new_fraction_size - old_fraction_size)
	FRACTiON_SIZE = 2 * size - 1
	*/
	val normalised_fraction_size = Wire(UInt((log2Ceil(2 * size + 1)).W))
	normalised_fraction_size := (2 * size - 1).U

	val partial_mantissa = Wire(UInt((2 * size + 1).W))
	partial_mantissa := io.posit.mantissa

	//new_matinsa = old_mantissa << (new_fraction_size - old_fraction_size)
	when (io.posit.fraction_size === (2 * size - 1).U) {
		partial_mantissa := io.posit.mantissa
	} .otherwise {
		partial_mantissa := io.posit.mantissa << ( (2 * size - 1).U - io.posit.fraction_size )
	}

	/*
	the mantissa will be normalised so the hiiden bit will be 1 and we do not
	habe overflow or underflow. partial exponent will keep the actualised
	value of the exponent. It is not called normalised because after it must be
	verified if it is in range
	*/
	val partial_exponent = Wire(SInt((2 * (log2Ceil(size) + 1 + exponent_size) + 1).W))
    partial_exponent := io.posit.exponent
	val normalised_mantissa = Wire(UInt((2 * size + 1).W))
	normalised_mantissa := partial_mantissa

	// if we have overflow we have to keep in rest bits the last bit of mantisa
	val restBits_after_normalisation = Wire(Bool())
	restBits_after_normalisation := io.posit.restBits
	val shiftLeft_normalisation = Wire(UInt((log2Ceil(2 * size + 1)).W))

	shiftLeft_normalisation := 0.U
    when(partial_mantissa(2 * size) === 1.U) {
		/*
		mantisa in [2, 4) we incrrease the exponent with 1 and
		shift right mantisa with 1 so the new mantisa will be in [1, 2)
		*/
        partial_exponent := io.posit.exponent + 1.S
		normalised_mantissa := partial_mantissa >> 1
		restBits_after_normalisation := io.posit.restBits || (partial_mantissa(0) === 1.U) 
    } .elsewhen(io.posit.mantissa(2*size-1) === 1.U) {
		//mantisa is in right range
        partial_exponent := io.posit.exponent
    } .otherwise {
		/*
		mantisa is in [0,1)
		we find the first bit that is one and we shift it to the hidden bit place 
		*/
		shiftLeft_normalisation := PriorityEncoder(Reverse(partial_mantissa)) - 1.U
		normalised_mantissa := partial_mantissa << shiftLeft_normalisation
		partial_exponent := io.posit.exponent - shiftLeft_normalisation.zext
    }
	
    //printf("[NORMALISE] normalised_fraction_size: %d, partial_mantissa %x\n", normalised_fraction_size, partial_mantissa)
    //printf("[NORMALISE] shiftLeft_normalisation: %d, restBits_after_normalisation %d\n", shiftLeft_normalisation, restBits_after_normalisation)
    //printf("[NORMALISE] partial_exponent: %d, normalised_mantissa %x\n", partial_exponent, normalised_mantissa)

	
	val normalised_exponent = Wire(SInt((2 * (log2Ceil(size) + 1 + exponent_size) + 1).W))
    normalised_exponent := partial_exponent

	/*
	we setup the exponent in the correct range
	*/
	// we set a special case (exponent_outside_range) when the exponent is out of range
	// posit does not overflow or underflow to zero so it will have the minimum or the maximum value
	// io can go directly to binary value
	//> maximum exponent (size-2)<<es
	val exponent_outside_range = Wire(Bool())

	exponent_outside_range := false.B
	normalised_exponent := 0.S
	when( partial_exponent > ( ( size - 2 ) << exponent_size ).S ) {
		// > maximum exponent 
		normalised_exponent := ( ( size - 2 ) << exponent_size ).S //maximum possibele value 2^(size-2)
		exponent_outside_range := true.B
	} .elsewhen( partial_exponent < ( -1 * ( ( size - 2 ) << exponent_size ) ).S ) {
		// < minimum exponent 
		normalised_exponent := ( -1 * ( ( size - 2 ) << exponent_size ) ).S //minimum possibele value -2^(size-2)
		exponent_outside_range := true.B
	} .otherwise {
		normalised_exponent := partial_exponent
		exponent_outside_range := false.B
	}
    //printf("[NORMALISE] normalised_exponent: %d, exponent_outside_range %d\n", normalised_exponent, exponent_outside_range.asUInt)

	val binary_regime_size = Wire(UInt(log2Ceil(size).W))
    val binary_exponent_size = Wire(UInt(log2Ceil(size).W))
	
	// normalised exponent
    //printf("[NORMALISE] encode normalised_exponent: %d\n", normalised_exponent)
    val regime = Wire(SInt((log2Ceil(size) + 1).W))
	// regime is exponent >> exponet_size or exponent / 2^exponent_size
    regime := normalised_exponent >> exponent_size
    val possible_binary_exponent = Wire(UInt(exponent_size.W))
	// binary_exponent is exponent - regime << es
	// reamining bits binary_exponent >= always (because of the way the sifting is working on negative numbers)
    possible_binary_exponent := (normalised_exponent - (regime << exponent_size)).asUInt
    //printf("[NORMALISE] regime: %d, binary_exponent: %d\n", regime, possible_binary_exponent)
	//regime size is regime +2 for positive regime and -regime +1 for negative regime
	// positive regime regime+1 bits of 1 and one bit of zero
	// negative regime -regime bits of zero and one bit of one
	/*
	calculate the binary_regime_size and binary_exponent_size
	*/
    binary_regime_size := Mux(
								regime >= 0.S,
								regime.asUInt + 2.U,
								(-regime).asUInt + 1.U
							)
    binary_exponent_size := Mux(
								binary_regime_size >= (size - 1).U,
								0.U,
								Mux(
									binary_regime_size > (size - 1 - exponent_size).U,
									(size - 1).U - binary_regime_size,
									exponent_size.U
								)
							)
    //printf("[NORMALISE] binary_regime_size: %d, binary_exponent_size: %d\n", binary_regime_size, binary_exponent_size)
    /*
    calculate the number of fraction bits
    size - 1 (sign bit) - regime_size - max_exponent_size
	or zero if it is negative
    if the value is less than zero or zero than is zero
    */
	val binary_fraction_size = Wire(UInt(log2Ceil(size).W))
    binary_fraction_size := Mux(
									binary_regime_size >= (size - 1 - exponent_size).U,
                                    0.U,
                                    (size - 1 - exponent_size).U - binary_regime_size
								)
    //printf("[NORMALISE] binary_fraction_size: %d\n", binary_fraction_size)
	/*
	The regime has the next binary values
	positive regime regime+1 bits of 1 and one bit of zero
		111...1111 ^ 00...0011 -> 111...1100 << (binary_exponent_size + binary_fraction_size) and
		keep on size bits ->
		-> 11...1100_00_000 >> 1 -> 11...110_00_000 & 01111...11 = 0_11...10_00_000
		Eg: size = 8 regime = 2 exponent=2, matinsa=8
		regime_size = 3, binary_exponent_size = 2, fraction = 0, binary_fraction_size = 2
		11111111 ^ 11 -> 1111_1100 << (2+2) = 1111_1100 << 4 -> 1100_0000 >> 1 -> 0110_0000
	negative regime -regime bits of zero and one bit of one
		1 << (binary_exponent_size + binary_fraction_size)  -> 0010_0000
		Eg: size = 8 regime = -2 exponent=2, matinsa=8
		regime_size = 3, binary_exponent_size = 2, fraction = 0, binary_fraction_size = 2
		1 << (2 + 2) -> 1 << 4 -> 0001_0000
	*/
    val binary_regime = Wire(UInt(size.W))
    binary_regime := Mux(
							regime === (size-2).S, //maximum regime Eg: 0_111_1111
                            (Fill(size, 1.U(1.W))) & (~(1.U <<(size-1))),
                            Mux(
								regime >= 0.S,
                                //( ((Fill(size, 1.U(1.W)) ^ 3.U) << (binary_exponent_size + binary_fraction_size)) >> 1.U ) & (~(1.U <<(size-1))), // OLD_WAY
								( ((Fill(size, 1.U(1.W)) ^ 3.U) << (binary_exponent_size + binary_fraction_size))(size - 1, 0) >> 1 ),
                                ( (1.U << (binary_exponent_size + binary_fraction_size)) )
							)
						)
    
    /*
	binary exponent will be the MSB binary_exponent_size of the possible_binary_exponent
	if binary_exponent_size > 0
	possible_binary_exponent >> (exponent_size - binary_exponent_size) << binary_fraction_size (binary_fraction_size to make sure the bits are in the right side)
	Eg: possible_binary_exponent = 10  binary_exponent_size = 1 exponent_size = 2 binary_fraction_size = 0 
	10 >> (2-1) << 0 = 1
	Eg: possible_binary_exponent = 10  binary_exponent_size = 2 exponent_size = 2 binary_fraction_size = 1
	10 >> (2-2) << 1
	in fact binary_fraction_size is different than zero only when  binary_exponent_size == exponent_size
	so we can change the formula
	if exponent_size > binary_exponent_size
		possible_binary_exponent >> (exponent_size - binary_exponent_size)
	else
		possible_binary_exponent << binary_fraction_size
	*/
    val binary_exponent = Wire(UInt(size.W))
    binary_exponent := Mux(
							binary_exponent_size > 0.U,
							Mux(
								exponent_size.U > binary_exponent_size,
								possible_binary_exponent >> (exponent_size.U - binary_exponent_size),
								possible_binary_exponent << binary_fraction_size
							),
                            //(possible_binary_exponent >> (exponent_size.U - binary_exponent_size)) << binary_fraction_size,
                            0.U
						)
	/*
	binary_fraction will be the MSB bits of the mantisa with the hidden bit and the overflow bit (first 2 bits)
	*/
    val binary_fraction = Wire(UInt(size.W))
    binary_fraction := Mux(
							binary_fraction_size > 0.U,
                            //(normalised_mantissa(2*size, size) - (1.U << (size-1))) >> ((size - 1).U - binary_fraction_size), //TODO eliminate hidden bit
							( normalised_mantissa(2*size-2, size) ) >> ((size - 1).U - binary_fraction_size),
                            0.U
						)
    //printf("[NORMALISE] binary_regime: %d, binary_exponent: %d, binary_fraction: %d\n", binary_regime, binary_exponent, binary_fraction)

    val binary_value = Wire(UInt(size.W))
    binary_value := binary_regime | binary_exponent | binary_fraction


    //printf("[NORMALISE] binary_value: %d, possible_binary: %d\n", binary_value, possible_binary)
	/*
	Rounding l g r s bits
	*/
	val l = Wire(Bool())
	val g = Wire(Bool())
	val r = Wire(Bool())
	val s = Wire(Bool())

	// l is the LSB of the possible_binary
	l := possible_binary(0)
	// g and r are the next 2 bits that did not got the change
	// to be part of the binary value
	// They can be exponent bits or fraction bits
	when( (exponent_size.U - binary_exponent_size) >= 2.U) {
		//when g and r are exponent bits 
		if(exponent_size >= 2) {
			g := (possible_binary_exponent >> ((exponent_size - 1).U - binary_exponent_size))(0) === 1.U
			r := (possible_binary_exponent >> ((exponent_size - 2).U - binary_exponent_size))(0) === 1.U
			// calcualte s from the other exponent bits, mantisa bits and any other rest bits
			s := !((possible_binary_exponent & ((1.U<<(((exponent_size - 1).U - binary_exponent_size)))-1.U)) === 0.U) || normalised_mantissa(2 * size - 2, 0).orR || restBits_after_normalisation
		} else {
			//case imposible
			g := false.B
			r := false.B
			s := false.B
		}
	} .elsewhen( (exponent_size.U - binary_exponent_size) === 1.U) {
		// when g is exponent bits and rest are fraction
		if(exponent_size >= 1) {
			g := possible_binary_exponent(0)
		} else {
			//case imposible
			g := false.B
		}
		r := normalised_mantissa(2 * size - 2)
		s := normalised_mantissa(2 * size - 3, 0).orR || restBits_after_normalisation
	} .otherwise {
		//when g r and s are fraction bits
		g := !( ( normalised_mantissa & (1.U << ((2 * size - 2).U - binary_fraction_size)) ) === 0.U)
		r := !( ( normalised_mantissa & (1.U << ((2 * size - 3).U - binary_fraction_size)) ) === 0.U)
		s := !( ( normalised_mantissa & ((1.U << ((2 * size - 2).U - binary_fraction_size))-1.U) ) === 0.U) || restBits_after_normalisation
	}

	/*
	posit has only round to even but in future work other roundings can be tested
	if the criteria is met than we add one to possible binary representation
	*/
	val addOne = Wire(Bool())
	addOne := g && (l || r || s)
    //printf("[NORMALISE] addOne: %d, l: %d, g: %d, r: %d, s: %d\n", addOne, l, g, r, s)

	val possible_binary = Wire(Bits(size.W))
    /*
	if special number (nar or zero) they have special representations
	if normal number
		if negative we dow two's complement of the binary value
		else the possible binary remains the same
	*/
	when(addOne && !exponent_outside_range) {
		possible_binary := binary_value +& 1.U
	} .otherwise {
		possible_binary := binary_value
	}
    io.binary := Mux(
							io.posit.nar,
							(1.U(1.W) << (size-1)),
							Mux(
								(io.posit.zero | (io.posit.mantissa === 0.U)),
								Fill(size, 0.U(1.W)),
								Mux(
									io.posit.sign,
									(1.U << (size-1)) | (~(possible_binary-1.U)),
									possible_binary
								)
							)
						)

    //printf("[NORMALISE] io.binary: %d\n", io.binary)
}