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













CONCRETE_CONSTANTS

   SCHAR_MAX ,
   SCHAR_MIN ,

   SCHAR ,

   byte_schar ,
   schar_byte ,

   uchar_schar ,
   schar_uchar

PROPERTIES


   SCHAR_MAX = ( 128   - 1 ) &
   SCHAR_MIN = ( - 128  ) &

   SCHAR = SCHAR_MIN .. SCHAR_MAX &

   byte_schar : BYTE --> SCHAR &
   byte_schar = % ( v0 ) . ( v0 : BYTE | ( - 128  ) * bitget ( v0 , 7 ) + 64     * bitget ( v0 , 6 ) + 32     * bitget ( v0 , 5 ) + 16     * bitget ( v0 , 4 ) + 8      * bitget ( v0 , 3 ) + 4      * bitget ( v0 , 2 ) + 2 * bitget ( v0 , 1 ) + bitget ( v0 , 0 ) ) &
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

    1      = 2 ** 0 &
    2      = 2 ** 1 &
    4      = 2 ** 2 &
    8      = 2 ** 3 &
    16     = 2 ** 4 &
    32     = 2 ** 5 &
    64     = 2 ** 6 &
    128    = 2 ** 7 &
    256    = 2 ** 8


END
