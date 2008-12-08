/* ALU
 * Author: valerio
 * Creation date: dom nov 23 2008
 */
MACHINE
   ALU
   
SEES
	TYPES 

DEFINITIONS
    power2_0 == 1;
    power2_1 == 2;
    power2_2 == 4;
    power2_3 == 8;
    power2_4 == 16;
    power2_5 == 32;
    power2_6 == 64;
    power2_7 == 128;
    power2_8 == 256;
    power2_9 == 512;
    power2_10 == 1024;
    power2_11 == 2048;
    power2_12 == 4096;
    power2_13 == 8192;
    power2_14 == 16384;
    power2_15 == 32768;
    power2_16 == 65536;


CONCRETE_CONSTANTS


   
   
   is_zero,
   is_zeroUSHORTINT,
   is_negative,

   halfSCHAR,
   add8UCHAR,
   add8SCHAR , substract8SCHAR,
   
   add16USHORTINT,
   add_carryUSHORTINT,
   add_halfcarryUSHORTINT,
   sub16USHORTINT,
   sub_carryUSHORTINT,
   sub_halfcarryUSHORTINT,
   
   inc_BYTE,
   dec_BYTE,
   inc_BV16,
   dec_BV16,
   
   parity_even_BYTE,
   
   and , ior , xor ,
   bitclear , bitset , bitget ,
   complement, swap , rotateleft , rotateright 

PROPERTIES
	is_zero : BYTE --> BIT &
	is_zero = %(w1).(w1 : BYTE | bool_to_bit(bool( w1(0) + w1(1) + w1(2) + w1(3) + w1(4) + w1(5) + w1(6) + w1(7) = 0  ) ) ) &
	
	is_zeroUSHORTINT : USHORTINT --> BIT &
	is_zeroUSHORTINT = %(nat1).(nat1 : USHORTINT | bool_to_bit(bool(nat1 = 0  ) ) ) &

	is_negative : BYTE --> BIT &
	is_negative = %(w1).(w1 : BYTE |  w1(7)  )&


	halfSCHAR : SCHAR --> UCHAR &
	halfSCHAR = % (ww) . (ww : SCHAR | (ww+128) mod power2_4) &	


	add8UCHAR : ( UCHAR * UCHAR ) --> (UCHAR ) &
	add8UCHAR = % (w1, w2). (w1 : UCHAR & w2 : UCHAR  |(w1+w2) mod  power2_8  ) &
	
	

      	/* I should review the functions add8SCHAR, substract8SCHAR */
	/* add (w1, w2) = (sum, is_negative, carry, half_carry, zero) */
	add8SCHAR : (BIT * SCHAR * SCHAR) --> (SCHAR * BIT * BIT * BIT * BIT) &
	add8SCHAR = % (carry,w1, w2). ( carry : BIT & w1 : SCHAR & w2 : SCHAR  & carry + w1 + w2 < -128  |
		  ( 256-(carry+w1+w2)   ,  
		  bool_to_bit(bool(carry+w1 + w2 < 0))	,
		  1,
		  bool_to_bit(bool(halfSCHAR(carry+w1) + halfSCHAR( w2) >= 16)),
		  0))
	 \/  % (carry,w1, w2). ( carry : BIT & w1 : SCHAR & w2 : SCHAR  & not(carry + w1 + w2 < -128) |
 		  ( ((carry + w1+w2) mod 128)  ,     /*pg 96 manual pdf - la parece esta errado o overflow superior da soma  */
		  bool_to_bit(bool(carry + w1 + w2 < 0))	,
		  bool_to_bit(bool(carry + w1 + w2 > 127 )),
		  bool_to_bit(bool(halfSCHAR(w1) + halfSCHAR(w2) >= power2_4)),
		  bool_to_bit(bool((carry + w1+w2) mod 128 = 0) ))) &

	/* substract8SCHAR (w1, w2) = (result, is_negative, carry, half_carry, zero) */
	substract8SCHAR : (BIT * SCHAR * SCHAR) --> (SCHAR * BIT * BIT * BIT * BIT) &
	substract8SCHAR = % (carry,w1, w2). ( carry : BIT & w1 : SCHAR & w2 : SCHAR  & carry + w1 - w2 < -128 |
 		  ( 256-( w1-w2 - carry)   ,  
		  bool_to_bit(bool(  w1 - w2 - carry< 0))	,
		  1,
		  bool_to_bit(bool(halfSCHAR(w1) - halfSCHAR(w2) >= power2_4)),
		  0))
	 \/ %(carry,w1, w2). ( carry : BIT & w1 : SCHAR & w2 : SCHAR  & not(carry + w1 - w2 < -128) |
 		( (( w1-w2- carry) mod 128)  ,     /*pg 96 manual pdf - la  parece estar errado o overflow superior da soma  */
		  bool_to_bit(bool( w1 - w2 - carry< 0))	,
		  bool_to_bit(bool( w1 - w2 - carry> 127 )),
		  bool_to_bit(bool(halfSCHAR(w1) - halfSCHAR(w2) >= 16)),
		  bool_to_bit(bool((( w1 - w2 - carry) mod 128) = 0) ))) &

	
    add16USHORTINT : ( BIT * USHORTINT * USHORTINT ) --> (USHORTINT ) &
    add16USHORTINT = % (b1, w1, w2). (b1 : BIT & w1 : USHORTINT & w2 : USHORTINT  |(b1+w1+w2) mod  65536  ) &

	add_carryUSHORTINT : ( BIT * USHORTINT * USHORTINT ) --> BIT &
	add_carryUSHORTINT = % (b1,w1, w2). (b1 : BIT & w1 : USHORTINT & w2 : USHORTINT  |
	 bool_to_bit(bool((b1+w1+w2) > 2**16 ) )  ) &

	add_halfcarryUSHORTINT : ( BIT * USHORTINT * USHORTINT ) --> BIT &
	add_halfcarryUSHORTINT = % (b1,w1, w2). (b1 : BIT & w1 : USHORTINT & w2 : USHORTINT  | 
	    bool_to_bit(bool(( b1 + (w1) mod 2**12 + (w2) mod 2**12 ) >  2**12 ))  ) &

    sub16USHORTINT : ( BIT * USHORTINT * USHORTINT ) --> (USHORTINT ) &
    sub16USHORTINT = % (b1, w1, w2). (b1 : BIT & w1 : USHORTINT & w2 : USHORTINT  |(w1-w2-b1) mod  65536  ) &

	sub_carryUSHORTINT : ( BIT * USHORTINT * USHORTINT ) --> BIT &
	sub_carryUSHORTINT = % (b1,w1, w2). (b1 : BIT & w1 : USHORTINT & w2 : USHORTINT  |
	 bool_to_bit(bool((w1-w2-b1) > 2**16 ) )  ) &

	sub_halfcarryUSHORTINT : ( BIT * USHORTINT * USHORTINT ) --> BIT &
	sub_halfcarryUSHORTINT = % (b1,w1, w2). (b1 : BIT & w1 : USHORTINT & w2 : USHORTINT  | 
	    bool_to_bit(bool((  (w1) mod 2**12 - (w2) mod 2**12 ) -b1 >  2**12 ))  ) &



    inc_BYTE :  BYTE  --> BYTE &
	inc_BYTE = % (w1). (w1 : BYTE | UCHAR_TO_BYTE((BYTE_TO_UCHAR(w1) + 1)  mod 256 ) ) &

    dec_BYTE :  BYTE  --> BYTE &
	dec_BYTE = % (w1). (w1 : BYTE | UCHAR_TO_BYTE((BYTE_TO_UCHAR(w1)-1 ) mod 256 ) ) &

   
    inc_BV16 :  BV16  --> BV16 &
	inc_BV16 = % (w1). (w1 : BV16 | USHORTINT_TO_BV16 ((BV16_TO_USHORTINT(w1)+1 ) mod 65536)  ) &

    dec_BV16 :  BYTE  --> BYTE &
	dec_BV16 = % (w1). (w1 : BV16 | USHORTINT_TO_BV16 ((BV16_TO_USHORTINT(w1)-1)  mod 65536)  ) &


	parity_even_BYTE : BIT_VECTOR --> BIT	&
	parity_even_BYTE = % (bv). (bv : BIT_VECTOR | (1-((SIGMA idx . (idx : dom(bv) | bv(idx) )) mod 2)) )&
	
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
	rotateright= %(bv).(bv:BYTE|{0|->bv(1), 1|->bv(2), 2|->bv(3), 3|->bv(4),4|->bv(5), 5|->bv(6), 6|->bv(7), 7|->bv(0)}) 

    

ASSERTIONS

   dom ( add8SCHAR ) = BIT*SCHAR * SCHAR &
   ran ( add8SCHAR ) : POW(SCHAR * BIT * BIT * BIT * BIT) &
   dom ( substract8SCHAR ) = BIT*SCHAR * SCHAR &
   ran ( substract8SCHAR ) : POW(SCHAR * BIT * BIT * BIT * BIT)  &
   
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
   =>  bitget(vec,in0) = vec(in0) ) 
	&

	!(xx,yy).(xx: INTEGER & yy: INTEGER => xx mod yy < yy & xx mod yy >= 0)

   
END

