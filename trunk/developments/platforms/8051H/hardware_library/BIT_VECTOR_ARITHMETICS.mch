MACHINE
	BIT_VECTOR_ARITHMETICS

SEES 
	BIT_DEFINITION,
	BIT_VECTOR_DEFINITION

CONSTANTS
	bv_to_nat
	,bv_par

PROPERTIES
	bv_to_nat : BIT_VECTOR --> NATURAL 
	&bv_to_nat = % (bv). (bv : BIT_VECTOR | SIGMA idx . (idx : dom(bv) | (2**idx) * bv(idx)))

 	&bv_par:BIT_VECTOR --> BIT
        &bv_par=%(bv).(bv:BIT_VECTOR| size(bv|>{1}) mod 2)
	

ASSERTIONS
	! ss. (ss : NATURAL1 => (bv_to_nat(bv_zero(ss)) = 0))
       &! ss. (ss : NATURAL1 => (bv_par(bv_zero(ss)) = 0))

END