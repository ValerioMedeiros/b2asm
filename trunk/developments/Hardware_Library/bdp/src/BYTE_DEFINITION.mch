MACHINE
        BYTE_DEFINITION

SEES
        BIT_DEFINITION ,
        BIT_VECTOR_DEFINITION

CONCRETE_CONSTANTS
        BYTE ,
        BYTE_WIDTH ,
        BYTE_INDEX ,
        BYTE_ZERO ,
        REAL_BYTE_INDEX ,
        byte_bit_get /*Mudar o nome*/

PROPERTIES
        BYTE_WIDTH = 8
        & BYTE_INDEX = 1 .. ( BYTE_WIDTH )
        & REAL_BYTE_INDEX = 0 .. ( BYTE_WIDTH - 1 )
        & BYTE = { bt | bt : BIT_VECTOR & bv_size ( bt ) = BYTE_WIDTH }
    & BYTE_ZERO : BYTE
        & BYTE_ZERO = BYTE_INDEX * { 0 }


        & byte_bit_get : ( BYTE * REAL_BYTE_INDEX ) --> BIT
        & byte_bit_get = % ( bt , ind ) . ( bt : BYTE & ind : REAL_BYTE_INDEX | bt ( ind + 1 ) )



ASSERTIONS
    ! bt . ( bt : BYTE => size ( bt ) = 8 )
    & size ( BYTE_ZERO ) = 8
    & BYTE <: BIT_VECTOR
    & BYTE_ZERO : BIT_VECTOR
    & first ( BYTE_ZERO ) = 0

END
