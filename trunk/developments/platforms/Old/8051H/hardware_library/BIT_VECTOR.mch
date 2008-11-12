MACHINE
	BIT_VECTOR_DEFINITION

SEES 
	BIT

CONSTANTS
	BIT_VECTOR,
	bv_to_nat,
	bv_zero

PROPERTIES
	BIT_VECTOR = seq (BIT_CARRIER) &
	bv_to_nat : BIT_VECTOR --> NATURAL &
	bv_to_nat = % (bv). (bv : BIT_VECTOR | SIGMA idx . (idx : dom(bv) | (2**idx) * bv(idx))) &
	bv_zero : NATURAL1 --> BIT_VECTOR &
	bv_zero = %(sz) . (sz : NATURAL1 | (0..(sz-1)) * {0})
/*ASSERTIONS
	!(sz). (sz : NATURAL1 | (bv_to_nat (bv_zero(sz)) = 0))*/
END