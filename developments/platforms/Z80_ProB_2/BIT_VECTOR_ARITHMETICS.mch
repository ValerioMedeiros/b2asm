MACHINE
    BIT_VECTOR_ARITHMETICS
    
SEES 
    BIT_DEFINITION,
    BYTE_DEFINITION,
    UCHAR_DEFINITION,
    USHORT_DEFINITION,
    BIT_VECTOR_DEFINITION,
    POWER2
    
DEFINITIONS
    
    SET_PREF_SYMBOLIC == TRUE;
    
    
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
    bv_nat,
    bv_even,
    nat_bv
    
PROPERTIES
    bv_nat : BIT_VECTOR --> NATURAL &
    bv_nat = % (bv). (bv : BIT_VECTOR | ( SIGMA idx . (idx : dom(bv) | (2**idx) * bv(idx))))&
    
    nat_bv : ((1..16) * (USHORT)) +-> BIT_VECTOR &
    nat_bv = %(ii,nn). (ii : 2..16 & nn : 0..65535 | {ii|->(nn mod (2**(ii))/(2**(ii-1)))}\/ nat_bv(ii-1,nn) ) \/
             %(ii,nn). (ii : 1..1  & nn : 0..65535 | {1 |->(nn mod (2**(1 ))/(2**(1 -1)))}) &
    
    bv_even:BIT_VECTOR --> BIT &
    bv_even=%(bv).(bv:BIT_VECTOR| 1- (size(bv|>{1}) mod 2) )
    
    
ASSERTIONS

    ! val. (val : USHORT => (bv_nat(nat_bv(16,val)) = val)) 

    
END