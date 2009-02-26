MACHINE
        BYTE_DEFINITION

SEES
        BIT_DEFINITION ,
        BIT_VECTOR_DEFINITION




CONCRETE_CONSTANTS

        BYTE ,
        BYTE_INDEX ,
        BYTE_ZERO ,
        REAL_BYTE_INDEX ,
        byte_bit_get , /*Mudar o nome*/

   get_upper_digit ,
   get_lower_digit

PROPERTIES

        BYTE_INDEX = 1 .. ( 8       ) &
        REAL_BYTE_INDEX = 0 .. ( 8      - 1 ) &
        BYTE = { bt | bt : BIT_VECTOR & bv_size ( bt ) = 8  }  &
    BYTE_ZERO : BYTE &
        BYTE_ZERO = BYTE_INDEX * { 0 } &


        byte_bit_get : ( BYTE * REAL_BYTE_INDEX ) --> BIT &
        byte_bit_get = % ( bt , ind ) . ( bt : BYTE & ind : REAL_BYTE_INDEX | bt ( ind + 1 ) ) &

   get_upper_digit = % ( by ) . ( by : BYTE | 2 ** 3 * by ( 8 ) + 2 ** 2 * by ( 7 ) + 2 * by ( 6 ) + by ( 5 ) ) &
   get_lower_digit = % ( by ) . ( by : BYTE | 2 ** 3 * by ( 4 ) + 2 ** 2 * by ( 3 ) + 2 * by ( 2 ) + by ( 1 ) )





ASSERTIONS
    ! bt . ( bt : BYTE => size ( bt ) = 8 ) &
    size ( BYTE_ZERO ) = 8 &
    BYTE <: BIT_VECTOR &
    BYTE_ZERO : BIT_VECTOR &
    first ( BYTE_ZERO ) = 0 &

    8         : NATURAL

END
