MACHINE
    BIT_VECTOR_ARITHMETICS

SEES
    BIT_DEFINITION ,
    BIT_VECTOR_DEFINITION

CONCRETE_CONSTANTS
    bv_to_nat

PROPERTIES
    bv_to_nat : BIT_VECTOR --> NATURAL &
    bv_to_nat = % ( bv ) . ( bv : BIT_VECTOR | SIGMA idx . ( idx : dom ( bv ) | ( 2 ** idx ) * bv ( idx ) ) )

ASSERTIONS
    ! ss . ( ss : NATURAL1 => ( bv_to_nat ( bv_zero ( ss ) ) = 0 ) )

END
