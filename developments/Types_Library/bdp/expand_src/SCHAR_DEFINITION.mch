/* SCHAR_DEFINITION
 * Author: atelierb
 * Creation date: Thu Feb 12 2009
 */
MACHINE
    SCHAR_DEFINITION

SEES
    BYTE_DEFINITION ,
    UCHAR_DEFINITION

DEFINITIONS
        SCHAR_MAX == ( 2 ** 7 - 1 ) ;
           SCHAR_MIN == ( - 2 ** 7 )

CONCRETE_CONSTANTS
   SCHAR ,

   byte_schar ,
   schar_byte ,

   uchar_schar ,
   schar_uchar

PROPERTIES

   SCHAR = SCHAR_MIN .. SCHAR_MAX &

   byte_schar : BYTE --> SCHAR &
   byte_schar = % ( v0 ) . ( v0 : BYTE | ( - 2 ** 7 ) * v0 ( 7 ) + 2 ** 6 * v0 ( 6 ) + 2 ** 5 * v0 ( 5 ) + 2 ** 4 * v0 ( 4 ) + 2 ** 3 * v0 ( 3 ) + 2 ** 2 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) ) &
   schar_byte : SCHAR --> BYTE &
   schar_byte = byte_schar ~ &

   uchar_schar : UCHAR --> SCHAR &
   uchar_schar = % ( v1 ) . ( v1 : UCHAR & v1 <= SCHAR_MAX | v1 ) &
   uchar_schar = % ( v1 ) . ( v1 : UCHAR & not ( v1 <= SCHAR_MAX ) | v1 - UCHAR_MAX + 1 ) &
   schar_uchar : SCHAR --> UCHAR &
   schar_uchar = uchar_schar ~

ASSERTIONS

        SCHAR_MAX : INTEGER ;
           SCHAR_MIN : INTEGER

END
