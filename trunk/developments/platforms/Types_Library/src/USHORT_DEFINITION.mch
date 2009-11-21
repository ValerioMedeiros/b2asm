/* USHORT_DEFINITION
 * Author: atelierb
 * Creation date: Thu Feb 12 2009
 */
MACHINE
    USHORT_DEFINITION
SEES
    BIT_DEFINITION,
    BIT_VECTOR_DEFINITION,
    BYTE_DEFINITION,
    BV16_DEFINITION,
    UCHAR_DEFINITION,
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
    pow2_8 == 256;
    pow2_9 == 512;
    pow2_10 == 1024;    
    pow2_11 == 2048;  
    pow2_12 == 4096;   
    pow2_13 == 8192;
    pow2_14 == 16384;   
    pow2_15 == 32768; 
    pow2_16 == 65536        
    
CONSTANTS
    
    USHORT_MAX,
    USHORT_MIN,
    
    USHORT,
    bv16_ushort,
    ushort_bv16,
    
    uchar_ushort,
    ushort_uchar
   
    
PROPERTIES
    
   USHORT_MAX =  (pow2_16) -1 &
   USHORT_MIN =  0 &

   USHORT = USHORT_MIN..USHORT_MAX &

   bv16_ushort : BV16 --> USHORT &
   bv16_ushort = %(v0).(v0:BV16 | pow2_15 * bv16_bit_get( v0 , 15 ) + pow2_14 * bv16_bit_get( v0 , 14 ) + pow2_13 * bv16_bit_get( v0 , 13 ) + pow2_12 * bv16_bit_get( v0 , 12 ) +
       pow2_11 * bv16_bit_get( v0 , 11 ) + pow2_10 * bv16_bit_get( v0 , 10 ) +   pow2_9 * bv16_bit_get( v0 , 9 )  +  pow2_8 * bv16_bit_get( v0 , 8 ) +  
       pow2_7 * bv16_bit_get( v0 , 7 )  +   pow2_6 * bv16_bit_get( v0 , 6 )  +   pow2_5 * bv16_bit_get( v0 , 5 )  +    pow2_4 * bv16_bit_get( v0 , 4 )  +    pow2_3 * 
       bv16_bit_get( v0 , 3 ) +     pow2_2 * bv16_bit_get( v0 , 2 )  +    pow2_1 * bv16_bit_get( v0 , 1 )  + bv16_bit_get( v0 , 0 ) ) &
   ushort_bv16 : USHORT  -->  BV16 &
   ushort_bv16 = bv16_ushort~ &
    
   uchar_ushort: UCHAR * UCHAR --> USHORT &
   uchar_ushort = % ( w1 , w2 ) . ( w1 : UCHAR & w2 : UCHAR |
      					bv16_ushort( byte_bv16 ( uchar_byte ( w1 ) , uchar_byte ( w2 ) ) ) ) &
      	
   ushort_uchar : USHORT --> UCHAR * UCHAR &
   ushort_uchar = uchar_ushort ~ 
    
    
    
    
ASSERTIONS

    USHORT_MAX : INTEGER &
    USHORT_MIN : INTEGER &
    
    dom(bv16_ushort) = BV16 &
    ran(bv16_ushort) = USHORT &

    dom(ushort_bv16) = USHORT &
    ran(ushort_bv16) = BV16 &
    
    dom(uchar_ushort) = (UCHAR * UCHAR) &
    ran(uchar_ushort) = USHORT &
    
    dom(ushort_uchar) = USHORT &
    ran(ushort_uchar) = (UCHAR * UCHAR) &
    
    pow2_0 = 2**0 &
    pow2_1 = 2**1 &
    pow2_2 = 2**2 &
    pow2_3 = 2**3 &
    pow2_4 = 2**4 &
    pow2_5 = 2**5 &
    pow2_6 = 2**6 &
    pow2_7 = 2**7 &
    pow2_8 = 2**8 &
    pow2_9 = 2**9 &    
    pow2_10 = 2**10 &
    pow2_11 = 2**11 &
    pow2_12 = 2**12 &
    pow2_13 = 2**13 &
    pow2_14 = 2**14 &
    pow2_15 = 2**15 &
    pow2_16 = 2**16 
    
END
