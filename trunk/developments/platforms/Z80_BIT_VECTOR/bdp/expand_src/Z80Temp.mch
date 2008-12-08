/*************************************************************************
Copyright (c) 2008, Valerio Medeiros Jr, David Deharbe

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
    * Neither the name of the Universidade Federal do Rio Grande do Norte nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*************************************************************************/

MACHINE
    Z80Temp
INCLUDES
    MEMORY

SEES
TYPES ,
ALU ,
POWER2





SETS
 id_reg_8 = { a0 , f0 , f_0 , a_0 ,
                 b0 , c0 , b_0 , c_0 ,
                 d0 , e0 , d_0 , e_0 ,
                 h0 , l0 , h_0 , l_0 ,
                 i0 , r0 } ;

 id_reg_16 = { BC , DE , HL , SP , AF }


ABSTRACT_VARIABLES
   rgs8 ,
  /* mem,
   stack,*/

   pc , sp , ix , iy ,
   /*not implemented yet - i,r */
   i_o_ports






INVARIANT
   stack : BV16  --> BYTE &
   rgs8 : id_reg_8 --> BYTE &
   pc : INSTRUCTION & sp : BV16 & ix : BV16 & iy : BV16 &
   i_o_ports  : BYTE --> BYTE
   /*   --- Nao seria interessante colocar uma variavel para avaliar estoura da pilha e overflow?  
     									Ex: ( block_stack_overflow =>  )
  */


DEFINITIONS
 bv_BC == BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ;
 bv_HL == BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ;
 bv_DE == BYTE_TO_BV16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ;
 bv_AF == BYTE_TO_BV16 ( rgs8 ( a0 ) , rgs8 ( f0 ) ) ;

 bv_9BC0 == mem ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) ;
 bv_9DE0 == mem ( BYTE_TO_BV16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ) ;
 bv_9HL0 == mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ;
 bv_9AF0 == mem ( BYTE_TO_BV16 ( rgs8 ( a0 ) , rgs8 ( f0 ) ) ) ;

 bv_9SP0 == mem ( sp ) ;
 bv_9IX0 == mem ( ix ) ;
 bv_9IY0 == mem ( iy ) ;

sp_plus_one == USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 1 ) ) ;
sp_plus_two == USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 2 ) ) ;
sp_minus_one == USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 1 ) ) ;
sp_minus_two == USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 2 ) ) ;


 z_s  == bitget ( rgs8 ( f0 ) , 7 ) ;
 z_z  == bitget ( rgs8 ( f0 ) , 6 ) ;
 z_00 == bitget ( rgs8 ( f0 ) , 5 ) ;
 z_h  == bitget ( rgs8 ( f0 ) , 4 ) ;
 z_01 == bitget ( rgs8 ( f0 ) , 3 ) ;
 z_p  == bitget ( rgs8 ( f0 ) , 2 ) ;
 z_n  == bitget ( rgs8 ( f0 ) , 1 ) ;
 z_c  == bitget ( rgs8 ( f0 ) , 0 )

CONCRETE_CONSTANTS

get_bv_reg16 ,
REG16_TO_REG8 ,
REG8_TO_REG16 ,

update_flag_register_SZ_H_PvNC ,
get_new_flag_register_SZ_H_PvNC ,


 bv_IX_plus_d ,
 bv_IY_plus_d ,
 bv_9IX_plus_d0 ,
 bv_9IY_plus_d0




PROPERTIES


  get_bv_reg16 : ( BV16 * ( id_reg_8 --> BYTE ) * id_reg_16 ) --> ( BV16 )
  & ! ( sp_ , rgs8_ , r1 ) .
  ( sp_ :  BV16 & rgs8_ : ( id_reg_8 --> BYTE ) & r1 : id_reg_16
   => ( r1 = BC =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = BYTE_TO_BV16 ( rgs8_ ( b0 ) , rgs8_ ( c0 ) ) ) &
      ( r1 = DE =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = BYTE_TO_BV16 ( rgs8_ ( d0 ) , rgs8_ ( e0 ) ) ) &
      ( r1 = HL =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = BYTE_TO_BV16 ( rgs8_ ( h0 ) , rgs8_ ( l0 ) ) ) &
      ( r1 = SP =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = sp_ ) &
      ( r1 = AF =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = BYTE_TO_BV16 ( rgs8_ ( a0 ) , rgs8_ ( f0 ) ) )
   ) &


  REG16_TO_REG8 : id_reg_16 --> ( id_reg_8 * id_reg_8 )
  & ! ( r1 ) .
  ( r1 : id_reg_16
   => ( r1 = BC =>  REG16_TO_REG8 ( r1 ) = ( b0 , c0 ) ) &
      ( r1 = DE =>  REG16_TO_REG8 ( r1 ) = ( d0 , e0 ) ) &
      ( r1 = HL =>  REG16_TO_REG8 ( r1 ) = ( h0 , l0 ) ) &
      ( r1 = AF =>  REG16_TO_REG8 ( r1 ) = ( a0 , f0 ) )
   )
  &
  REG8_TO_REG16 = REG16_TO_REG8 ~ &

  update_flag_register_SZ_H_PvNC = % ( rgs8_ , s7 , z6 , h4 , pv2 , n_add_sub , c0 ) .
  ( rgs8_ : id_reg_8 --> BYTE & s7 : BIT & z6  : BIT &   h4 : BIT  & pv2 : BIT & n_add_sub : BIT & c0 : BIT |
      rgs8_ <+ { ( f0 |-> {
             /*S*/   ( 7 |-> s7 ) ,
                         /*Z*/         ( 6 |-> z6 ) ,
                                         /*(5 |-> rgs8_(f0)(5) ),*/
                         /*H*/         ( 4 |-> h4 ) ,
                                         /*(3 |-> rgs8_(f0)(3) ),*/
                         /*P/V*/ ( 2 |-> pv2 ) ,
                         /*N*/         ( 1 |-> n_add_sub ) ,
                         /*C*/         ( 0 |-> c0 ) } ) } )

        &

        get_new_flag_register_SZ_H_PvNC = % ( rgs8_ , s7 , z6 , h4 , pv2 , n_add_sub , c0 ) .
        ( rgs8_ : id_reg_8 --> BYTE & s7 : BIT & z6  : BIT &   h4 : BIT  & pv2 : BIT & n_add_sub : BIT & c0 : BIT |
        ( f0 |-> {
             /*S*/   ( 7 |-> s7 ) ,
                         /*Z*/         ( 6 |-> z6 ) ,
                                         /*(5 |-> rgs8_(f0)(5) ),*/
                         /*H*/         ( 4 |-> h4 ) ,
                                         /*(3 |-> rgs8_(f0)(3) ),*/
                         /*P/V*/ ( 2 |-> pv2 ) ,
                         /*N*/         ( 1 |-> n_add_sub ) ,
                         /*C*/         ( 0 |-> c0 ) } ) ) &

         bv_IX_plus_d         = % ( ix , desloc ) . ( ix : BV16 & desloc : SCHAR |
             USHORTINT_TO_BV16 ( BV16_TO_USHORTINT ( ix ) + desloc ) ) &

         bv_IY_plus_d = % ( iy , desloc ) . ( iy : BV16 & desloc : SCHAR |
             USHORTINT_TO_BV16 ( BV16_TO_USHORTINT ( iy ) + desloc ) ) &

        bv_9IX_plus_d0 = % ( mem , ix , desloc ) . ( mem : ( BV16 --> BYTE ) & ix : BV16 & desloc : SCHAR |
            mem ( USHORTINT_TO_BV16 ( ( BV16_TO_USHORTINT ( ix ) + desloc ) mod 65536 ) ) ) &

        bv_9IY_plus_d0 = % ( mem , iy , desloc ) . ( mem : ( BV16 --> BYTE ) & iy : BV16 & desloc : SCHAR |
            mem ( USHORTINT_TO_BV16 ( ( BV16_TO_USHORTINT ( iy ) + desloc ) mod 65536 ) ) )


ASSERTIONS
   dom ( stack ) = BV16 &
   ran ( stack ) <: BYTE &
   ran ( mem ) <: BYTE &
   dom ( mem ) = BV16 &
   ran ( rgs8 ) <: BYTE &
   dom ( rgs8 ) = id_reg_8 &
   INSTRUCTION_NEXT ( 0 ) = 1 &
   INSTRUCTION_NEXT ( 1 ) = 2 &
   INSTRUCTION_NEXT ( 2 ) = 3 &
   INSTRUCTION_NEXT ( 3 ) = 4 &
   INSTRUCTION_NEXT ( 4 ) = 5 &
   INSTRUCTION_NEXT ( 5 ) = 6 &
   INSTRUCTION_NEXT ( 6 ) = 7 &
   INSTRUCTION_NEXT ( 7 ) = 8 &
   INSTRUCTION_NEXT ( 8 ) = 9 &
   INSTRUCTION_NEXT ( 9 ) = 10 &
   INSTRUCTION_NEXT ( 10 ) = 11 &
   INSTRUCTION_NEXT ( 11 ) = 12 &
   INSTRUCTION_NEXT ( 12 ) = 13 &
   INSTRUCTION_NEXT ( 13 ) = 14 &

/*   !(vec,in0).(vec : BYTE & in0 : 0..7 
   =>  bitget(BYTE_TO_SCHAR(vec),in0) = vec(in0) ) &*/

   bv_9BC0 : BYTE  & mem ( BYTE_TO_BV16 ( SCHAR_TO_BYTE ( 0 ) , bv_9BC0 ) ) : BYTE &
   bv_9DE0 : BYTE  & mem ( BYTE_TO_BV16 ( SCHAR_TO_BYTE ( 0 ) , bv_9DE0 ) ) : BYTE &
   bv_9HL0 : BYTE  & mem ( BYTE_TO_BV16 ( SCHAR_TO_BYTE ( 0 ) , bv_9HL0 ) ) : BYTE &
   bv_9AF0 : BYTE  & mem ( BYTE_TO_BV16 ( SCHAR_TO_BYTE ( 0 ) , bv_9AF0 ) ) : BYTE &

   bv_9SP0 : BYTE  &
   bv_9IX0 : BYTE  &
   bv_9IY0 : BYTE &

   ! ( ii , des ) . ( ii : BV16 & des : SCHAR => bv_IX_plus_d ( ii , des ) : BV16 ) &
   ! ( ii , des ) . ( ii : BV16 & des : SCHAR => bv_IY_plus_d ( ii , des ) : BV16 ) &

   ! ( mmm , ii , des ) . ( mmm : BV16 >-> BYTE & ii : BV16 & des : SCHAR => bv_9IX_plus_d0 ( mmm , ii , des ) : BYTE ) &
   ! ( mmm , ii , des ) . ( mmm : BV16 >-> BYTE & ii : BV16 & des : SCHAR => bv_9IY_plus_d0 ( mmm , ii , des ) : BYTE )

INITIALISATION

   rgs8 :: id_reg_8 --> BYTE ||
   pc :: INSTRUCTION || sp :: BV16 || ix :: BV16 || iy :: BV16 ||
   i_o_ports  :: BYTE --> BYTE

/*OPERATIONS*/



/************************************************************************************** 
***  			Rotate and Shift Group
**************************************************************************************/
/*
 RLCA =
	BEGIN
		rgs8 := rgs8 <+ { a0 |-> rotateleft(rgs8(a0)) , 
		    			  f0 |->  }
		
	END
; 
  */


/************************************************************************************** 
***  			BIT Manipulation  Group
**************************************************************************************/
/*
 BIT_b_rr (bb,rr) =
      PRE bb : 0..(SCHAR_LENGTH-1)& rr : id_reg_8 THEN
	      rgs8 := update_flag_register_SZ_H_PvNC(rgs8, z_s, bit_not( bitget( rgs8(rr),bb) ), 1,z_p,0,z_c)||
          pc := INSTRUCTION_NEXT(pc)
     END
;

 BIT_b_9HL0 (bb) =  
	PRE bb : 0..(SCHAR_LENGTH-1) THEN
      rgs8 := update_flag_register_SZ_H_PvNC(rgs8, z_s, bit_not( bv_9HL0(bb) ), 1,z_p,0,z_c)||
	  pc := INSTRUCTION_NEXT(pc)
	END
;



 BIT_b_9IX_d0 (desloc, bb) =  
	PRE bb : 0..(SCHAR_LENGTH-1) & desloc : SCHAR THEN
      rgs8 := update_flag_register_SZ_H_PvNC(rgs8, z_s, bit_not( bv_9IX_plus_d0(mem,ix,desloc)(bb) ), 1,z_p,0,z_c)||
	  pc := INSTRUCTION_NEXT(pc)
	END
;


 BIT_b_9IY_d0 (desloc, bb) =  
	PRE bb : 0..(SCHAR_LENGTH-1) & desloc : SCHAR THEN
      rgs8 := update_flag_register_SZ_H_PvNC(rgs8, z_s, bit_not( bv_9IY_plus_d0(mem,iy,desloc)(bb) ), 1,z_p,0,z_c)||
	  pc := INSTRUCTION_NEXT(pc)
	END
;


 SET_b_r(bb,rr)=
 	PRE bb  : 0..(SCHAR_LENGTH-1) & rr : id_reg_8 THEN
 	    rgs8(rr):= bitset(rgs8(rr),bb) 	||
	    pc := INSTRUCTION_NEXT(pc)    
	END
;

 SET_b_9HL0(bb)=
 	PRE bb  : 0..(SCHAR_LENGTH-1) THEN
       updateAddressMem(bv_HL, bv_set(bv_9HL0,bb))||
	    pc := INSTRUCTION_NEXT(pc)    
	END
;
 SET_b_9IX_d0(bb,desloc)=
 	PRE bb  : 0..(SCHAR_LENGTH-1) & desloc : SCHAR THEN
       updateAddressMem(bv_HL, bv_set(bv_9IY_plus_d0(mem,ix,desloc),bb))||
	    pc := INSTRUCTION_NEXT(pc)    
	END
;
 SET_b_9IY_d0(bb,desloc)=
 	PRE bb  : 0..(SCHAR_LENGTH-1) & desloc : SCHAR THEN
       updateAddressMem(bv_HL, bv_set(bv_9IY_plus_d0(mem,iy,desloc),bb))||
	    pc := INSTRUCTION_NEXT(pc)    
	END
;

 RES_b_r(bb,rr)=
 	PRE bb  : 0..(SCHAR_LENGTH-1) & rr : id_reg_8 THEN
 	    rgs8(rr):= bv_clear(rgs8(rr),bb) 	 ||
	    pc := INSTRUCTION_NEXT(pc)       
	END
;

 RES_b_9HL0(bb)=
 	PRE bb  : 0..(SCHAR_LENGTH-1) THEN
       updateAddressMem(bv_HL, bv_clear(bv_9HL0,bb))||
	    pc := INSTRUCTION_NEXT(pc)    
	END
;
 RES_b_9IX_d0(bb,desloc)=
 	PRE bb  : 0..(SCHAR_LENGTH-1) & desloc : SCHAR THEN
       updateAddressMem(bv_HL, bv_clear(bv_9IY_plus_d0(mem,ix,desloc),bb))||
	    pc := INSTRUCTION_NEXT(pc)    
	END
;
 RES_b_9IY_d0(bb,desloc)=
 	PRE bb  : 0..(SCHAR_LENGTH-1) & desloc : SCHAR THEN
       updateAddressMem(bv_HL, bv_clear(bv_9IY_plus_d0(mem,iy,desloc),bb))||
	    pc := INSTRUCTION_NEXT(pc)    
	END
;*/
/************************************************************************************** 
***  			JUMP GROUP
**************************************************************************************/
/*
	JP_nn(nn)=
	PRE nn : USHORTINT THEN
	    pc:= nn
	END;

	JP_cc_nn(cc,nn)=
	PRE cc : BOOL & nn : USHORTINT THEN
		IF	cc = TRUE THEN    pc:= nn
		ELSE pc := INSTRUCTION_NEXT(pc) END  
	END;

	JR_e(ee)=
	PRE ee : SCHAR & ee-2 : SCHAR THEN
		 pc := (pc + ee) mod 2**16 
	END;

	JR_C_e(ee)=
	PRE ee : SCHAR & ee-2 : SCHAR THEN
		IF z_c = 0 THEN		 pc := INSTRUCTION_NEXT(pc) 
		ELSE pc := (pc + ee) mod 2**16 END
	END;


	JR_NC_e(ee)=
	PRE ee : SCHAR & ee-2 : SCHAR THEN
		IF z_c = 1 THEN		 pc := INSTRUCTION_NEXT(pc) 
		ELSE pc := (pc + ee) mod 2**16 END
	END;

	JR_Z_e(ee)=
	PRE ee : SCHAR & ee-2 : SCHAR THEN
		IF z_z = 0 THEN		 pc := INSTRUCTION_NEXT(pc) 
		ELSE pc := (pc + ee) mod 2**16 END
	END;

	JR_NZ_e(ee)=
	PRE ee : SCHAR & ee-2 : SCHAR THEN
		IF z_z = 1 THEN		 pc := INSTRUCTION_NEXT(pc) 
		ELSE pc := (pc + ee) mod 2**16 END
	END;

	JP_HL=
	BEGIN
		pc := BV16_TO_USHORTINT(bv_HL)
	END;

	JP_IX=
	BEGIN
		pc := BV16_TO_USHORTINT( ix)
	END;

	JP_IY=
	BEGIN
		pc :=  BV16_TO_USHORTINT( iy)
	END;

	DJNZ_e(ee)=
	PRE ee : SCHAR & ee-2 : SCHAR THEN
	    
		rgs8(b0):=	dec_BYTE(rgs8(b0))||
			
		IF is_zero(dec_BYTE(rgs8(b0))) = 1 THEN		 pc := INSTRUCTION_NEXT(pc) 
		ELSE pc := (pc + ee) mod 2**16 END

	END;
	*/
/************************************************************************************** 
***  			CALL and RETURN GROUP
**************************************************************************************/
 /*
 CALL_nn (nn) =
    PRE nn : USHORTINT THEN
        updateAddressStack( sp , USHORTINT_TO_BV16( INSTRUCTION_NEXT(pc) )) ||
        sp := inc_BV16(sp) ||
        pc :=  nn 
    END
 
;
 RETURN =
    PRE sp > 0 THEN
        pop(sp)  COMENT**stack := {sp - 1} <<| stack **COMENT_CLOSE ||
        pc := stack(sp - 1) ||
        sp := sp - 1
    END
 ;    
*/

END
