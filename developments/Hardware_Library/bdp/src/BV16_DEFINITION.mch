/* BV16_DEFINITION
 * Author: atelierb
 * Creation date: Thu Feb 12 2009
 */
MACHINE
    BV16_DEFINITION
SEES
        BIT_DEFINITION ,
        BIT_VECTOR_DEFINITION ,
        BYTE_DEFINITION




CONCRETE_CONSTANTS
        BV16_INDX ,
        REAL_BV16_INDX ,
        BV16_ZERO ,
        BV16 ,
        bv16_byte ,
        byte_bv16 ,
        bv16_bit_get

PROPERTIES

    BV16_INDX = 1 .. ( 16     )
&        REAL_BV16_INDX = 0 .. ( 16     - 1 )
&        BV16 = { bt | bt : BIT_VECTOR & bv_size ( bt ) = 16  }
&        BV16_ZERO = BV16_INDX * { 0 }

&        bv16_byte : BV16 --> BYTE * BYTE
&         bv16_byte = % ( bv ) . ( bv : BV16 | ( { 0 |-> bv ( 8 ) , 1 |-> bv ( 9 ) , 2 |-> bv ( 10 ) , 3 |-> bv ( 11 ) , 4 |-> bv ( 12 ) , 5 |-> bv ( 13 ) , 6 |-> bv ( 14 ) , 7 |-> bv ( 15 ) }
                                                            , { 0 |-> bv ( 0 ) , 1 |-> bv ( 1 ) , 2 |-> bv ( 2 ) , 3 |-> bv ( 3 ) , 4 |-> bv ( 4 ) , 5 |-> bv ( 5 ) , 6 |-> bv ( 6 ) , 7 |-> bv ( 7 ) } ) )

&         byte_bv16 = bv16_byte ~

&        bv16_bit_get : ( BYTE * BV16_INDX ) --> BIT
        & byte_bit_get = % ( bt , ind ) . ( bt : BYTE & ind : REAL_BYTE_INDEX | bt ( ind + 1 ) )

ASSERTIONS

    BV16_ZERO : BV16
&        BV16 <: BIT_VECTOR
&        BV16_ZERO : BIT_VECTOR

END
