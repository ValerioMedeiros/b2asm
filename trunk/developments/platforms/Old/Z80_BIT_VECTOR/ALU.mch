MACHINE
   ALU
   
SEES
   BIT_DEFINITION ,
   BIT_VECTOR_DEFINITION,
   BYTE_DEFINITION,
   BV16_DEFINITION,

   UCHAR_DEFINITION,
   SCHAR_DEFINITION,
   SSHORT_DEFINITION,
   USHORT_DEFINITION,
   POWER2 
DEFINITIONS
   
   SSHORT_MIN==-(2**15);
   SSHORT_MAX== (2**15-1);
   UCHAR_MAX == 2**8-1;
   UCHAR_MIN == 0 

CONCRETE_CONSTANTS
   
   is_zero,
   is_zeroUSHORT,
   is_negative,
   
   half,
   simple_add8UCHAR,
   add8UCHAR , substract8UCHAR,
   
   add16USHORT,
   add_carryUSHORT,
   add_halfcarryUSHORT,
   sub16USHORT,
   sub_carryUSHORT,
   sub_halfcarryUSHORT,
   
   inc_BYTE,
   dec_BYTE,
   inc_BV16,
   dec_BV16,
   
   parity_even_BYTE,
   
   and , ior , xor ,
   bitclear , bitset , bitget ,
   complement, swap , rotateleft , rotateright,
   
   update_refresh_reg,

   INST_SZ ,
   INSTRUCTION ,
   NB_INSTRUCTIONS ,
   INSTRUCTION_MAX ,
   instruction_next ,
   instruction_jump ,
   
   daa_function
   
PROPERTIES
   is_zero : BYTE --> BIT &
   is_zero = %(w1).(w1 : BYTE | bool_bit(bool( w1(0) + w1(1) + w1(2) + w1(3) + w1(4) + w1(5) + w1(6) + w1(7) = 0  ) ) ) &
   
   is_zeroUSHORT : USHORT --> BIT &
   is_zeroUSHORT = %(nat1).(nat1 : USHORT | bool_bit(bool(nat1 = 0  ) ) ) &
   
   is_negative : BYTE --> BIT &
   is_negative = %(w1).(w1 : BYTE |  w1(7)  )&
   
   
   half : UCHAR --> UCHAR &
   half = % (ww) . (ww : UCHAR | ww mod 2**4) &	
   
   
	simple_add8UCHAR  : (UCHAR * UCHAR) --> UCHAR &
   simple_add8UCHAR = % (w1,w2) . (w1 : UCHAR & w2 : UCHAR | (w1 + w2) mod 2**8) &	
   
   
   /* add (w1, w2) = (sum, is_negative, carry, half_carry, zero) */
   add8UCHAR : (BIT * UCHAR * UCHAR) --> (UCHAR * BIT * BIT * BIT * BIT) &
   add8UCHAR = % (carry,w1, w2). ( carry : BIT & w1 : UCHAR & w2 : UCHAR    |
      (( ( carry + w1 + w2 ) mod 256)   ,  
      bool_bit(bool( carry + uchar_schar(w1) + uchar_schar (w2) < 0))	,
      bool_bit(bool( carry + w1 + w2 > UCHAR_MAX)),
      bool_bit(bool( carry + half(w1) + half( w2) >= 2**4)),
      bool_bit(bool( ( (carry + w1 + w2 ) mod UCHAR_MAX-1) = 0))  ))  &
   
   
   /* substract8UCHAR (w1, w2) = (result, is_negative, carry, half_carry, zero) */
   substract8UCHAR : (BIT * UCHAR * UCHAR) --> (UCHAR * BIT * BIT * BIT * BIT) &
   substract8UCHAR = % (carry,w1, w2). ( carry : BIT & w1 : UCHAR & w2 : UCHAR   |
      ( ( ( carry + w1 - w2 ) mod 256)   ,  
      bool_bit(bool( carry + uchar_schar(w1) - uchar_schar (w2) < 0))	,
      bool_bit(bool( carry + w1 - w2 > UCHAR_MAX)),
      bool_bit(bool( carry + half(w1) - half( w2) >= 2**4)),
      bool_bit(bool( ( (carry + w1 - w2 ) mod UCHAR_MAX-1) = 0)) ) )  &
   
   
   add16USHORT : ( BIT * USHORT * USHORT ) --> (USHORT ) &
   add16USHORT = % (b1, w1, w2). (b1 : BIT & w1 : USHORT & w2 : USHORT  |(b1+w1+w2) mod  65536  ) &
   
   add_carryUSHORT : ( BIT * USHORT * USHORT ) --> BIT &
   add_carryUSHORT = % (b1,w1, w2). (b1 : BIT & w1 : USHORT & w2 : USHORT  |
      bool_bit(bool((b1+w1+w2) > 2**16 ) )  ) &
   
   add_halfcarryUSHORT : ( BIT * USHORT * USHORT ) --> BIT &
   add_halfcarryUSHORT = % (b1,w1, w2). (b1 : BIT & w1 : USHORT & w2 : USHORT  | 
      bool_bit(bool(( b1 + (w1) mod 2**12 + (w2) mod 2**12 ) >  2**12 ))  ) &
   
   sub16USHORT : ( BIT * USHORT * USHORT ) --> (USHORT ) &
   sub16USHORT = % (b1, w1, w2). (b1 : BIT & w1 : USHORT & w2 : USHORT  |(w1-w2-b1) mod  65536  ) &
   
   sub_carryUSHORT : ( BIT * USHORT * USHORT ) --> BIT &
   sub_carryUSHORT = % (b1,w1, w2). (b1 : BIT & w1 : USHORT & w2 : USHORT  |
      bool_bit(bool((w1-w2-b1) > 2**16 ) )  ) &
   
   sub_halfcarryUSHORT : ( BIT * USHORT * USHORT ) --> BIT &
   sub_halfcarryUSHORT = % (b1,w1, w2). (b1 : BIT & w1 : USHORT & w2 : USHORT  | 
      bool_bit(bool((  (w1) mod 2**12 - (w2) mod 2**12 ) -b1 >  2**12 ))  ) &

   
   inc_BYTE :  BYTE  --> BYTE &
   inc_BYTE = % (w1). (w1 : BYTE | uchar_byte((byte_uchar(w1) + 1)  mod 256 ) ) &
   
   dec_BYTE :  BYTE  --> BYTE &
   dec_BYTE = % (w1). (w1 : BYTE | uchar_byte((byte_uchar(w1)-1 ) mod 256 ) ) &
   
   
   inc_BV16 :  BV16  --> BV16 &
   inc_BV16 = % (w1). (w1 : BV16 | ushort_bv16 ((bv16_ushort(w1)+1 ) mod 65536)  ) &
   
   dec_BV16 :  BYTE  --> BYTE &
   dec_BV16 = % (w1). (w1 : BV16 | ushort_bv16 ((bv16_ushort(w1)-1)  mod 65536)  ) &
   
   
   parity_even_BYTE : BIT_VECTOR --> BIT	&
   parity_even_BYTE = % (bv). (bv : BIT_VECTOR | (1-( bv_get(bv,0)+ 
               bv_get(bv,1)+ bv_get(bv,2)+ bv_get(bv,3)+bv_get(bv,4)+bv_get(bv,5)+
               bv_get(bv,7))  mod 2)) &
   
   and : (BYTE * BYTE) --> BYTE &
   and = %(bt1, bt2).(bt1:BYTE & bt2:BYTE | bv_and(bt1,bt2)) &
   
   ior : (BYTE * BYTE) --> BYTE & 
   ior = %(bt1, bt2).(bt1:BYTE & bt2:BYTE | bv_or(bt1,bt2))   & 
   
   xor : (BYTE * BYTE) --> BYTE &
   xor = %(bt1, bt2).(bt1 : BYTE & bt2 : BYTE | bv_xor(bt1,bt2)) &
   
   
   bitget : (BYTE * BYTE_INDEX) --> BIT & 
   bitget = %(bt1, ii).(bt1 : BYTE & ii : BYTE_INDEX | bt1(ii)) & 
   
   bitset : (BYTE * BYTE_INDEX) --> BYTE & 
   ! (ww, ii).(ww : BYTE & ii : BYTE_INDEX => bitset(ww,ii) = bv_set(ww, ii)) & 
   
   bitclear : (BYTE * BYTE_INDEX) --> BYTE & 
   ! (ww, ii, bb).(ww : BYTE & ii : BYTE_INDEX & bb : BIT => bitclear(ww,ii) = bv_clear(ww, ii)) & 
   
   complement : BYTE --> BYTE &
   complement = %(bt).(bt : BYTE | bv_not(bt)) &
   
   
   swap : BYTE --> BYTE &
   swap=%(bt).(bt:BYTE |{0|->bt(4), 1|->bt(5), 2|->bt(6), 3|->bt(7), 4|->bt(0), 5|->bt(1), 6|->bt(2), 7|->bt(3)}) &
   
   
   rotateleft : BYTE --> BYTE  &
   rotateleft= %(bv).(bv:BYTE| {0|->bv(7), 1|->bv(0), 2|->bv(1), 3|->bv(2), 4|->bv(3),5|->bv(4),6|->bv(5),7|->bv(6)} ) &
   
   
   rotateright : BYTE --> BYTE  &
   rotateright= %(bv).(bv:BYTE|{0|->bv(1), 1|->bv(2), 2|->bv(3), 3|->bv(4),4|->bv(5), 5|->bv(6), 6|->bv(7), 7|->bv(0)})  &
   
   
   
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
   
   dom ( add8UCHAR ) = BIT*UCHAR * UCHAR &
   ran ( add8UCHAR ) : POW(UCHAR * BIT * BIT * BIT * BIT) &
   dom ( substract8UCHAR ) = BIT*UCHAR * UCHAR &
   ran ( substract8UCHAR ) : POW(UCHAR * BIT * BIT * BIT * BIT)  &
   
   dom(and) = BYTE * BYTE &
   ran(and) <: BYTE  &
   
   dom(ior) = BYTE * BYTE &
   ran(ior) <: BYTE  &
   
   dom(xor) = BYTE * BYTE &
   ran(xor) <: BYTE  &
   
   dom(complement) = BYTE &
   ran(complement) <: BYTE &
   
   dom(swap) = BYTE &
   ran(swap) <: BYTE &
   
   dom(rotateleft) = BYTE &
   ran(rotateleft) <: BYTE  &
   
   dom(rotateright) = BYTE &
   ran(rotateright) <: BYTE    &
   
   
   !(vec,in0).(vec : BYTE & in0 : 0..7 
      =>  bitget(vec,in0) = vec(in0) )    &
   
   !(x0,x1).(  x0 : UCHAR & x1 : UCHAR => simple_add8UCHAR(x0,x1) : UCHAR) &

   
    0 = schar_sshort ( 0 , 0 ) &
   
   /* The assertions related with Types */
  
   ! ( n0 ) . ( n0 : UCHAR => 0 <= n0 ) &  
   ! ( n0 ) . ( n0 : UCHAR => n0 <= 255 ) &
   
   instruction_next : USHORT -->  USHORT &
   
   
   !(xx).( xx : BYTE => byte_uchar(xx) : UCHAR) &   
   !(xx).( xx : UCHAR => uchar_byte(xx) : BYTE ) &
   
  !(xx).( xx : BYTE => update_refresh_reg(xx) : BYTE) &   
   
   !(xx).( xx : BYTE => byte_schar(xx) : SCHAR) &
   !(xx).( xx : SCHAR => schar_byte(xx) : BYTE ) &
   
   
   !(xx,yy).( xx : BYTE & yy : BYTE => byte_bv16(xx,yy) : BV16) &
   !(xx,yy).( xx : BYTE & yy : BYTE => #(zz).( zz : BV16 & byte_bv16(xx,yy) = zz) )&
   
   !(xx).( xx : BV16  => bv16_ushort(xx) : USHORT) &
   !(xx).( xx : USHORT => ushort_bv16(xx) : BV16 ) &
   
   !(xx).( xx : BYTE  =>  get_upper_digit (xx) : 0..15) &
      !(xx).( xx : BYTE  =>  get_lower_digit (xx) : 0..15) &
   
    !(zn, c0 , value , h0 ) .(zn : BIT & c0 : BIT & value : BYTE & h0 : BIT => 
          daa_function(zn, c0 , h0 , value ) : ( BYTE * BIT *BIT ) )   
   
   
END

