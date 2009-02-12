/* UCHAR
 * Author: atelierb
 * Creation date: Thu Feb 12 2009
 */
MACHINE
    UCHAR
CONSTANTS
   UCHAR ,
   UCHAR_MAX,
   UCHAR_MIN,
   uchar_to_byte,
   byte_to_uchar, 
   
   
PROPERTIES
   UCHAR = ( 0 .. (2**8-1) ) &
   UCHAR_MAX : INTEGER &
   UCHAR_MIN : INTEGER &
   UCHAR_MAX = 2**8 &
   UCHAR_MIN = 0 &
END
