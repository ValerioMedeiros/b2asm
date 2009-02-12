/* USHORT_DEFINITION
 * Author: atelierb
 * Creation date: Thu Feb 12 2009
 */
MACHINE
    USHORT_DEFINITION
SEES
    BV16_DEFINITION

DEFINITIONS
        USHORT_MAX ==  ( 2 ** 16 ) - 1 ;
        USHORT_MIN ==  0

CONCRETE_CONSTANTS

    USHORT ,
    bv16_ushort ,
    ushort_bv16




PROPERTIES


        USHORT = USHORT_MIN .. USHORT_MAX &

    bv16_ushort : BV16 --> USHORT &
        bv16_ushort = % ( v0 ) . ( v0 : BV16 | 32768 * v0 ( 15 ) + 16384 * v0 ( 14 ) + 8192 * v0 ( 13 ) + 4096 * v0 ( 12 ) + 2048 * v0 ( 11 ) +
                                                            1024 * v0 ( 10 ) +   512 * v0 ( 9 )  +  256 * v0 ( 8 ) +   128 * v0 ( 7 )  +   64 * v0 ( 6 )  +
                                                              32 * v0 ( 5 )  +    16 * v0 ( 4 )  +    8 * v0 ( 3 ) +     4 * v0 ( 2 )  +    2 * v0 ( 1 )  + v0 ( 0 ) ) &
        ushort_bv16 = bv16_ushort ~

END
