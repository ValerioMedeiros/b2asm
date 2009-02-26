MACHINE
   TYPES
 /*	  
SEES
   BIT_DEFINITION ,
   BIT_VECTOR_DEFINITION,
   BYTE_DEFINITION,
   BV16_DEFINITION,

   UCHAR_DEFINITION,
   SCHAR_DEFINITION,
   SSHORT_DEFINITION,
   USHORT_DEFINITION	


   
CONCRETE_CONSTANTS
   update_refresh_reg,

   INST_SZ ,
   INSTRUCTION ,
   NB_INSTRUCTIONS ,
   INSTRUCTION_MAX ,
   instruction_next ,
   instruction_jump ,
   
   daa_function
   
PROPERTIES
   
  
   update_refresh_reg   : BYTE --> BYTE &
   update_refresh_reg =  % ( v0 ) .  ( v0 : BYTE | uchar_byte (2**7*v0 (7 ) + ((  2**6 * v0 ( 6 ) + 2**5 * v0 ( 5 ) + 2**4 * v0 ( 4 ) +
                2**3 * v0 ( 3 ) +   2**2 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) ) mod 64 ) ) ) &   

   NB_INSTRUCTIONS : NATURAL &
   INST_SZ : NATURAL &
   INST_SZ = 16 &
   NB_INSTRUCTIONS = 2 ** INST_SZ &
   INSTRUCTION_MAX = NB_INSTRUCTIONS - 1 &
   INSTRUCTION = USHORT  &
   instruction_next : USHORT --> USHORT &
   instruction_next =
   % (w1). (w1 : USHORT  |(w1+1) mod  65535  ) &
   
   instruction_jump = % ( p0 , e0 ) . ( p0 : INSTRUCTION & e0 : - 126 .. 129 | (p0 + e0) mod  65535 )  &
   
  
   daa_function : ( BIT * BIT * BIT * BYTE) --> ( BYTE * BIT * BIT ) &
   !(zn, c0 , h0 , value ) .(zn : BIT & c0 : BIT & h0 : BIT & value : BYTE =>
      (  (  zn = 0 & c0 = 0 & get_upper_digit(value) : 0..9 & h0 = 0 & get_lower_digit(value) : 0..9 =>
				daa_function(zn, c0 , h0  , value )=( value , 0 ,0  ) ) &
         (  zn = 0 & c0 = 0 & get_upper_digit(value) : 0..8 & h0 = 0 & get_lower_digit(value) : 10..15 =>
				daa_function(zn, c0 , h0  , value )= ( uchar_byte(simple_add8UCHAR( byte_uchar( value) , 6 )) , 0 ,  bool_bit(bool(   half(byte_uchar(value)) + half(6) >= 2**4)) ) ) &
         (  zn = 0 & c0 = 0 & get_upper_digit(value) : 0..9 & h0 = 1 & get_lower_digit(value) : 0..3 =>
				daa_function(zn, c0 , h0  , value )= ( uchar_byte(simple_add8UCHAR( byte_uchar( value) , 6 )) , 0 ,  bool_bit(bool(   half(byte_uchar(value)) + half(6) >= 2**4)) ) )&
         (  zn = 0 & c0 = 0 & get_upper_digit(value) : 10..15 & h0 = 0 & get_lower_digit(value) : 0..9 =>
				daa_function(zn, c0 , h0  , value )= ( uchar_byte(simple_add8UCHAR( byte_uchar( value) , 96 )) , 1,  bool_bit(bool(   half(byte_uchar(value)) + half(96) >= 2**4)) ) )&
         (  zn = 0 & c0 = 0 & get_upper_digit(value) : 9..15 & h0 = 0 & get_lower_digit(value) : 10..15 =>
				daa_function(zn, c0 , h0  , value )= ( uchar_byte(simple_add8UCHAR( byte_uchar( value) , 102 )) , 1,  bool_bit(bool(   half(byte_uchar(value)) + half(102) >= 2**4)) ) )&
         (  zn = 0 & c0 = 0 & get_upper_digit(value) : 10..15 & h0 = 1 & get_lower_digit(value) : 0..3 =>
				daa_function(zn, c0 , h0  , value )= ( uchar_byte(simple_add8UCHAR( byte_uchar( value) , 102 )) , 1,  bool_bit(bool(   half(byte_uchar(value)) + half(102) >= 2**4)) ) )&
         (  zn = 0 & c0 = 1 & get_upper_digit(value) : 0..2 & h0 = 0 & get_lower_digit(value) : 0..9 =>
				daa_function(zn, c0 , h0  , value )= ( uchar_byte(simple_add8UCHAR( byte_uchar( value) , 96 )) , 1,  bool_bit(bool(   half(byte_uchar(value)) + half(96) >= 2**4)) ) )&
         (  zn = 0 & c0 = 1 & get_upper_digit(value) : 0..2 & h0 = 0 & get_lower_digit(value) : 10..15 =>
				daa_function(zn, c0 , h0  , value )= ( uchar_byte(simple_add8UCHAR( byte_uchar( value) , 102)) , 1,  bool_bit(bool(   half(byte_uchar(value)) + half(102) >= 2**4)) ) )&
         (  zn = 0 & c0 = 1 & get_upper_digit(value) : 0..3 & h0 = 1 & get_lower_digit(value) : 0..3 =>
				daa_function(zn, c0 , h0  , value )= ( uchar_byte(simple_add8UCHAR( byte_uchar( value) , 102)) , 1,  bool_bit(bool(   half(byte_uchar(value)) + half(102) >= 2**4)) ) )&

         (  zn = 1 & c0 = 0 & get_upper_digit(value) : 0..9 & h0 = 0 & get_lower_digit(value) : 0..9 =>
				daa_function(zn, c0 , h0  , value )= (  value , 0 , 0 ) ) &
         (  zn = 1 & c0 = 0 & get_upper_digit(value) : 0..8 & h0 = 1 & get_lower_digit(value) : 6..15 =>
				daa_function(zn, c0 , h0  , value )= ( uchar_byte(simple_add8UCHAR( byte_uchar( value) , 250)) , 0,  bool_bit(bool(   half(byte_uchar(value)) + half(250) >= 2**4)) ) )&
         (  zn = 1 & c0 = 1 & get_upper_digit(value) : 7..15 & h0 = 0 & get_lower_digit(value) : 0..9 =>
				daa_function(zn, c0 , h0  , value )= ( uchar_byte(simple_add8UCHAR( byte_uchar( value) , 160)) , 1 ,  bool_bit(bool(   half(byte_uchar(value)) + half(160) >= 2**4)) ) )&
         (  zn = 1 & c0 = 1 & get_upper_digit(value) : 6..7 & h0 = 1 & get_lower_digit(value) : 6..15 =>
				daa_function(zn, c0 , h0  , value )= ( uchar_byte(simple_add8UCHAR( byte_uchar( value) , 154)) , 1,  bool_bit(bool(  half(byte_uchar(value)) + half(154) >= 2**4)) ) )  ) ) 


ASSERTIONS
   0 = schar_sshort ( 0 , 0 ) &
   
   
  */  
  
END

