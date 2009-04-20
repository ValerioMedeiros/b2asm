/* SCHAR_DEFINITION
 * Author: atelierb
 * Creation date: Thu Feb 12 2009
 */
MACHINE
    SCHAR_DEFINITION

SEES
    BIT_DEFINITION ,
    BIT_VECTOR_DEFINITION ,
    BYTE_DEFINITION ,
    UCHAR_DEFINITION ,
    POWER2

DEFINITIONS

    pow2_0 == 1 ;
    pow2_1 == 2 ;
    pow2_2 == 4 ;
    pow2_3 == 8 ;
    pow2_4 == 16 ;
    pow2_5 == 32 ;
    pow2_6 == 64 ;
    pow2_7 == 128 ;
    pow2_8 == 256

CONCRETE_CONSTANTS

   SCHAR_MAX ,
   SCHAR_MIN ,

   SCHAR ,

   byte_schar ,
   schar_byte ,

   uchar_schar ,
   schar_uchar

PROPERTIES


   SCHAR_MAX = ( pow2_7 - 1 ) &
   SCHAR_MIN = ( - pow2_7 ) &

   SCHAR = SCHAR_MIN .. SCHAR_MAX &

   byte_schar : BYTE --> SCHAR &
   byte_schar = % ( v0 ) . ( v0 : BYTE | ( - pow2_7 ) * bitget ( v0 , 7 ) + pow2_6 * bitget ( v0 , 6 ) + pow2_5 * bitget ( v0 , 5 ) + pow2_4 * bitget ( v0 , 4 ) + pow2_3 * bitget ( v0 , 3 ) + pow2_2 * bitget ( v0 , 2 ) + 2 * bitget ( v0 , 1 ) + bitget ( v0 , 0 ) ) &
   schar_byte : SCHAR --> BYTE &
   schar_byte = byte_schar ~ &

   uchar_schar : UCHAR --> SCHAR &
   uchar_schar = % ( v1 ) . ( v1 : UCHAR & v1 <= SCHAR_MAX | v1 ) &
   uchar_schar = % ( v1 ) . ( v1 : UCHAR & not ( v1 <= SCHAR_MAX ) | v1 - UCHAR_MAX + 1 ) &

   schar_uchar : SCHAR --> UCHAR &
   schar_uchar = uchar_schar ~

ASSERTIONS

        SCHAR_MAX : INTEGER &
        SCHAR_MIN : INTEGER &

    dom ( byte_schar ) = BYTE &
    ran ( byte_schar ) = SCHAR &

    dom ( schar_byte ) = SCHAR &
    ran ( schar_byte ) = BYTE &

    dom ( uchar_schar ) = UCHAR &
    ran ( uchar_schar ) = SCHAR &
    dom ( schar_uchar ) = SCHAR &
    ran ( schar_uchar ) = UCHAR &

    pow2_0 = 2 ** 0 &
    pow2_1 = 2 ** 1 &
    pow2_2 = 2 ** 2 &
    pow2_3 = 2 ** 3 &
    pow2_4 = 2 ** 4 &
    pow2_5 = 2 ** 5 &
    pow2_6 = 2 ** 6 &
    pow2_7 = 2 ** 7 &
    pow2_8 = 2 ** 8


END
