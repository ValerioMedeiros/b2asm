/* UCHAR_DEFINITION
 * Author: atelierb
 * Creation date: Thu Feb 12 2009
 */
MACHINE
    UCHAR_DEFINITION

SEES


    BIT_DEFINITION ,
    BIT_VECTOR_DEFINITION ,
    BYTE_DEFINITION ,
    POWER2













CONCRETE_CONSTANTS

   UCHAR_MAX ,
   UCHAR_MIN ,


   UCHAR ,
   uchar_byte ,
   byte_uchar

PROPERTIES

   UCHAR_MAX = 256    - 1 &
   UCHAR_MIN = 0  &

   UCHAR = ( UCHAR_MIN .. UCHAR_MAX ) &

   byte_uchar : BYTE  -->  UCHAR &
   byte_uchar =  % ( v0 ) . ( v0 : BYTE |  128   * bitget ( v0 , 7 ) + 64   * bitget ( v0 , 6 ) + 32     * bitget ( v0 , 5 ) +
                                                               16     * bitget ( v0 , 4 ) + 8      * bitget ( v0 , 3 ) + 4      * bitget ( v0 , 2 ) + 2      * bitget ( v0 , 1 ) + 128    * bitget ( v0 , 0 ) ) &

   uchar_byte :  UCHAR  -->  BYTE &
   uchar_byte = byte_uchar ~

ASSERTIONS

    UCHAR_MAX : INTEGER &
    UCHAR_MIN : INTEGER &

    dom ( byte_uchar ) = BYTE &
    ran ( byte_uchar ) = UCHAR &

    dom ( uchar_byte ) = UCHAR &
    ran ( uchar_byte ) = BYTE &


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
