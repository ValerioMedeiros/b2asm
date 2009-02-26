/* SCHAR_DEFINITION
 * Author: atelierb
 * Creation date: Thu Feb 12 2009
 */
MACHINE
    SCHAR_DEFINITION

SEES
    BYTE_DEFINITION ,
    UCHAR_DEFINITION






CONCRETE_CONSTANTS
   SCHAR ,

   byte_schar ,
   schar_byte ,

   uchar_schar ,
   schar_uchar

PROPERTIES

   SCHAR = ( - 2 ** 7 ) .. ( 2 ** 7 - 1 ) &

   byte_schar : BYTE --> SCHAR &
   byte_schar = % ( v0 ) . ( v0 : BYTE | ( - 2 ** 7 ) * v0 ( 7 ) + 2 ** 6 * v0 ( 6 ) + 2 ** 5 * v0 ( 5 ) + 2 ** 4 * v0 ( 4 ) + 2 ** 3 * v0 ( 3 ) + 2 ** 2 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) ) &
   schar_byte : SCHAR --> BYTE &
   schar_byte = byte_schar ~ &

   uchar_schar : UCHAR --> SCHAR &
   uchar_schar = % ( v1 ) . ( v1 : UCHAR & v1 <= ( 2 ** 7 - 1 ) | v1 ) &
   uchar_schar = % ( v1 ) . ( v1 : UCHAR & not ( v1 <= ( 2 ** 7 - 1 ) ) | v1 - 2 ** 8 - 1 + 1 ) &
   schar_uchar : SCHAR --> UCHAR &
   schar_uchar = uchar_schar ~

ASSERTIONS

        ( 2 ** 7 - 1 ) : INTEGER ;
           ( - 2 ** 7 ) : INTEGER

END
