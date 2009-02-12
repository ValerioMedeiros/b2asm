/* UCHAR_DEFINITION
 * Author: atelierb
 * Creation date: Thu Feb 12 2009
 */
MACHINE
    UCHAR_DEFINITION
    
SEES
	BYTE_DEFINITION

DEFINITIONS
   UCHAR_MAX == 2**8-1;
   UCHAR_MIN == 0 
   
CONSTANTS
    
   UCHAR ,
   uchar_byte,
   byte_uchar 
   
   
PROPERTIES
   UCHAR = ( UCHAR_MIN .. UCHAR_MAX) &

   byte_uchar : BYTE  -->  UCHAR &
   byte_uchar =  % ( v0 ) . ( v0 : BYTE |  2**7*v0 (7 ) + 2**6 * v0 ( 6 ) + 2**5 * v0 ( 5 ) + 
       							2**4 * v0 ( 4 ) + 2**3 * v0 ( 3 ) + 2**2 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) ) &
                            
   uchar_byte :  UCHAR  -->  BYTE &
   uchar_byte = byte_uchar ~ 

ASSERTIONS
    UCHAR_MAX : INTEGER &
   	UCHAR_MIN : INTEGER 
END
