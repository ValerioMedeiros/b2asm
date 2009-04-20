MACHINE
    BYTE_DEFINITION

SEES
    BIT_DEFINITION ,
    BIT_VECTOR_DEFINITION ,
    POWER2






















CONCRETE_CONSTANTS

    BYTE ,
    BYTE_INDEX ,
    BYTE_ZERO ,
    PHYS_BYTE_INDEX ,


    is_zero ,
    parity_even ,
    and , ior , xor ,
    bitclear , bitset , bitget ,
    complement , swap , rotateleft , rotateright ,
    get_upper_digit ,
    get_lower_digit

PROPERTIES

    BYTE_INDEX = 1 .. ( 8       ) &
    PHYS_BYTE_INDEX = 0 .. ( 8      - 1 ) &
    BYTE = { bt | bt : BIT_VECTOR & bv_size ( bt ) = 8  }  &
    BYTE_ZERO : BYTE &
    BYTE_ZERO = BYTE_INDEX * { 0 } &


    is_zero : BYTE --> BIT &
    is_zero = % ( w1 ) . ( w1 : BYTE | bool_bit ( bool ( w1 ( 1 ) + w1 ( 2 ) + w1 ( 3 ) + w1 ( 4 ) + w1 ( 5 ) + w1 ( 6 ) + w1 ( 7 ) + w1 ( 8 ) = 0 ) ) ) &


    parity_even : BYTE --> BIT        &
    parity_even = % ( bv ) . ( bv : BYTE | 1 - ( bv ( 1 ) + bv ( 2 ) + bv ( 3 ) + bv ( 4 ) + bv ( 5 ) + bv ( 6 ) + bv ( 7 ) + bv ( 8 ) mod 2 ) ) &

    and : ( BYTE * BYTE ) --> BYTE &
    and = % ( bt1 , bt2 ) . ( bt1 : BYTE & bt2 : BYTE | bv_and ( bt1 , bt2 ) ) &

    ior : ( BYTE * BYTE ) --> BYTE &
    ior = % ( bt1 , bt2 ) . ( bt1 : BYTE & bt2 : BYTE | bv_or ( bt1 , bt2 ) ) &

    xor : ( BYTE * BYTE ) --> BYTE &
    xor = % ( bt1 , bt2 ) . ( bt1 : BYTE & bt2 : BYTE | bv_xor ( bt1 , bt2 ) ) &


    bitget : ( BYTE * PHYS_BYTE_INDEX ) --> BIT &
/*  bitget = %(bt1, ii).(bt1 : BYTE & ii : PHYS_BYTE_INDEX | bv_get(bt1, ii)) &  */
    bitget = % ( bt1 , ii ) . ( bt1 : BYTE & ii : PHYS_BYTE_INDEX | bt1 ( ii + 1 ) ) & /* mode more simple */

    bitset : ( BYTE * PHYS_BYTE_INDEX ) --> BYTE &
    bitset = % ( bt1 , ii ) . ( bt1 : BYTE & ii : PHYS_BYTE_INDEX | bv_set ( bt1 , ii ) ) &

    bitclear : ( BYTE * PHYS_BYTE_INDEX ) --> BYTE &
    ! ( ww , ii , bb ) . ( ww : BYTE & ii : PHYS_BYTE_INDEX & bb : BIT => bitclear ( ww , ii ) = bv_clear ( ww , ii ) ) &

    complement : BYTE --> BYTE &
    complement = % ( bt ) . ( bt : BYTE | bv_not ( bt ) ) &


    swap : BYTE --> BYTE &
    swap = % ( bt ) . ( bt : BYTE | { 1 |-> bt ( 5 ) , 2 |-> bt ( 6 ) , 3 |-> bt ( 7 ) , 4 |-> bt ( 8 ) , 5 |-> bt ( 1 ) , 6 |-> bt ( 2 ) , 7 |-> bt ( 3 ) , 8 |-> bt ( 4 ) } ) &


    rotateleft : BYTE --> BYTE  &
    rotateleft  = % ( bv ) . ( bv : BYTE | { 1 |-> bv ( 8 ) , 2 |-> bv ( 1 ) , 3 |-> bv ( 2 ) , 4 |-> bv ( 3 ) , 5 |-> bv ( 4 ) , 6 |-> bv ( 5 ) , 7 |-> bv ( 6 ) , 8 |-> bv ( 7 ) } ) &


    rotateright : BYTE --> BYTE  &
    rotateright = % ( bv ) . ( bv : BYTE | { 1 |-> bv ( 2 ) , 2 |-> bv ( 3 ) , 3 |-> bv ( 4 ) , 4 |-> bv ( 5 ) , 5 |-> bv ( 6 ) , 6 |-> bv ( 7 ) , 7 |-> bv ( 8 ) , 8 |-> bv ( 1 ) } ) &


    get_upper_digit = % ( by ) . ( by : BYTE | 8    * by ( 8 ) + 4     * by ( 7 ) + 2 * by ( 6 ) +  by ( 5 )   ) &
    get_lower_digit = % ( by ) . ( by : BYTE | 8    * by ( 4 ) + 4     * by ( 3 ) + 2 * by ( 2 ) +  by ( 1 )   )


ASSERTIONS

    size ( BYTE_ZERO ) = 8 &
    BYTE <: BIT_VECTOR &
    BYTE_ZERO : BIT_VECTOR &
    first ( BYTE_ZERO ) = 0 &

    ! ( bt ) . ( bt : BYTE => bv_size ( bt ) = size ( bt ) & bv_size ( bt ) = 8 & bt : seq1 ( BIT ) ) &

    8          : NATURAL &

    dom ( is_zero ) = BYTE &
    ran ( is_zero ) = BIT &

    dom ( parity_even ) = BYTE &
    ran ( parity_even ) = BIT &

    dom ( and ) = BYTE * BYTE &
    ran ( and ) = BYTE &

    dom ( ior ) = BYTE * BYTE &
    ran ( ior ) = BYTE &

    dom ( xor ) = BYTE * BYTE &
    ran ( xor ) = BYTE &

    dom ( bitget ) = ( BYTE * PHYS_BYTE_INDEX ) &
    ran ( bitget ) = BIT &

    dom ( bitset ) = ( BYTE * PHYS_BYTE_INDEX ) &
    ran ( bitset ) = BYTE &

    dom ( bitclear ) = ( BYTE * PHYS_BYTE_INDEX ) &
    ran ( bitclear ) = BYTE &

    dom ( complement ) = BYTE &
    ran ( complement ) = BYTE &

    dom ( swap ) = BYTE &
    ran ( swap ) = BYTE &

    dom ( rotateleft ) = BYTE &
    ran ( rotateleft ) = BYTE &

    dom ( rotateright ) = BYTE &
    ran ( rotateright ) = BYTE &

    dom ( get_upper_digit ) = BYTE &
    ran ( get_upper_digit ) = 0 .. ( 16 - 1 ) &

    dom ( get_lower_digit ) = BYTE &
    ran ( get_lower_digit ) = 0 .. ( 16 - 1 ) &


    [ 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 ] : BYTE &
    [ 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ] : BYTE &


    1      = 2 ** 0 &
    2      = 2 ** 1 &
    4      = 2 ** 2 &
    8      = 2 ** 3 &
    16     = 2 ** 4 &
    32     = 2 ** 5 &
    64     = 2 ** 6 &
    128    = 2 ** 7 &
    256    = 2 ** 8 &
    512    = 2 ** 9 &
    1024    = 2 ** 10 &
    2048    = 2 ** 11 &
    4096    = 2 ** 12 &
    8192    = 2 ** 13 &
    16384   = 2 ** 14 &
    32768   = 2 ** 15 &
    65536   = 2 ** 16




END
