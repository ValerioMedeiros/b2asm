/* UCHAR_DEFINITION
 * Author: atelierb
 * Creation date: Thu Feb 12 2009
 */
MACHINE
    UCHAR_DEFINITION
    
SEES
    
    
    BIT_DEFINITION,
    BIT_VECTOR_DEFINITION,
    BYTE_DEFINITION,
    POWER2
    
DEFINITIONS
    
    pow2_0 == 1;
    pow2_1 == 2;
    pow2_2 == 4;
    pow2_3 == 8;
    pow2_4 == 16;
    pow2_5 == 32;
    pow2_6 == 64;
    pow2_7 == 128;
    pow2_8 == 256
 
CONSTANTS
    
   UCHAR_MAX,
   UCHAR_MIN,      
    
    
   UCHAR ,
   uchar_byte,
   byte_uchar 
   
PROPERTIES
    
   UCHAR_MAX = pow2_8 - 1 &
   UCHAR_MIN = 0  &
     
   UCHAR = ( UCHAR_MIN .. UCHAR_MAX) &

   byte_uchar : BYTE  -->  UCHAR &
   byte_uchar =  % ( v0 ) . ( v0 : BYTE |  pow2_7*bitget( v0 , 7 ) + pow2_6 * bitget( v0 ,  6 ) + pow2_5 * bitget( v0 ,  5 ) + 
       							pow2_4 * bitget( v0 ,  4 ) + pow2_3 * bitget( v0 ,  3 ) + pow2_2 * bitget( v0 ,  2 ) + pow2_1 * bitget( v0 ,  1 ) + pow2_7 * bitget( v0 , 0 ) ) &
                            
   uchar_byte :  UCHAR  -->  BYTE &
   uchar_byte = byte_uchar ~ 

ASSERTIONS

    UCHAR_MAX : INTEGER &
    UCHAR_MIN : INTEGER &
    
    dom(byte_uchar) = BYTE &
    ran(byte_uchar) = UCHAR &

    dom(uchar_byte) = UCHAR &
    ran(uchar_byte) = BYTE &

        
    pow2_0 = 2**0 &
    pow2_1 = 2**1 &
    pow2_2 = 2**2 &
    pow2_3 = 2**3 &
    pow2_4 = 2**4 &
    pow2_5 = 2**5 &
    pow2_6 = 2**6 &
    pow2_7 = 2**7 &
    pow2_8 = 2**8
 
    
END

