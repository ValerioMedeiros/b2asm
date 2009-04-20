/* USHORT_DEFINITION
 * Author: atelierb
 * Creation date: Thu Feb 12 2009
 */
MACHINE
    USHORT_DEFINITION
SEES
    BIT_DEFINITION ,
    BIT_VECTOR_DEFINITION ,
    BYTE_DEFINITION ,
    BV16_DEFINITION ,
    UCHAR_DEFINITION ,
    POWER2





















CONCRETE_CONSTANTS

    USHORT_MAX ,
    USHORT_MIN ,

    USHORT ,
    bv16_ushort ,
    ushort_bv16 ,

    uchar_ushort ,
    ushort_uchar


PROPERTIES

   USHORT_MAX =  ( 65536 ) - 1 &
   USHORT_MIN =  0 &

   USHORT = USHORT_MIN .. USHORT_MAX &

   bv16_ushort : BV16 --> USHORT &
   bv16_ushort = % ( v0 ) . ( v0 : BV16 | 32768 * bv16_bit_get ( v0 , 15 ) + 16384 * bv16_bit_get ( v0 , 14 ) + 8192 * bv16_bit_get ( v0 , 13 ) + 4096 * bv16_bit_get ( v0 , 12 ) +
       2048    * bv16_bit_get ( v0 , 11 ) + 1024   * bv16_bit_get ( v0 , 10 ) +  512    * bv16_bit_get ( v0 , 9 ) +  256    * bv16_bit_get ( v0 , 8 ) +
       128    * bv16_bit_get ( v0 , 7 ) +   64     * bv16_bit_get ( v0 , 6 ) +   32     * bv16_bit_get ( v0 , 5 ) +    16     * bv16_bit_get ( v0 , 4 ) +    8      *
       bv16_bit_get ( v0 , 3 ) +    4      * bv16_bit_get ( v0 , 2 ) +    2      * bv16_bit_get ( v0 , 1 ) + bv16_bit_get ( v0 , 0 ) ) &
   ushort_bv16 : USHORT  -->  BV16 &
   ushort_bv16 = bv16_ushort ~ &

   uchar_ushort : UCHAR * UCHAR --> USHORT &
   uchar_ushort = % ( w1 , w2 ) . ( w1 : UCHAR & w2 : UCHAR |
                                              bv16_ushort ( byte_bv16 ( uchar_byte ( w1 ) , uchar_byte ( w2 ) ) ) ) &

   ushort_uchar : USHORT --> UCHAR * UCHAR &
   ushort_uchar = uchar_ushort ~




ASSERTIONS

    USHORT_MAX : INTEGER &
    USHORT_MIN : INTEGER &

    dom ( bv16_ushort ) = BV16 &
    ran ( bv16_ushort ) = USHORT &

    dom ( ushort_bv16 ) = USHORT &
    ran ( ushort_bv16 ) = BV16 &

    dom ( uchar_ushort ) = ( UCHAR * UCHAR ) &
    ran ( uchar_ushort ) = USHORT &

    dom ( ushort_uchar ) = USHORT &
    ran ( ushort_uchar ) = ( UCHAR * UCHAR ) &

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
