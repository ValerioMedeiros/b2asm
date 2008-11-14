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
   Z80
INCLUDES
    MEMORY
    
SEES
TYPES,
ALU,
POWER2





SETS
 id_reg_8 = {a0,f0,f_0,a_0,
	         b0,c0,b_0,c_0,
	         d0,e0,d_0,e_0,
	         h0,l0,h_0,l_0,
	         i0,r0};

 id_reg_16 = {BC,DE,HL,SP,AF}


ABSTRACT_VARIABLES
   rgs8,
  /* mem,
   stack,*/

   pc,sp,ix,iy,
   /*not implemented yet - i,r */ 
   i_o_ports






INVARIANT
   stack : USHORTINT  --> SCHAR & 
   rgs8 : id_reg_8 --> SCHAR &
   pc : USHORTINT & sp : USHORTINT & ix : USHORTINT & iy : USHORTINT &
  /* stack <: mem */

  /* I/O replace SCHAR for UCHAR*/
   i_o_ports  : UCHAR --> SCHAR


DEFINITIONS
 v_BC == SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0));
 v_DE == SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0));
 v_HL == SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0));
 v_AF == SCHAR_TO_SSHORTINT(rgs8(a0),rgs8(f0));

 v_9BC0 == mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))));
 v_9DE0 == mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))));
 v_9HL0 == mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))));
 v_9AF0 == mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(a0),rgs8(f0))));

 v_9SP0 == mem(SSHORTINT_TO_USHORTINT(sp));
 v_9IX0 == mem(SSHORTINT_TO_USHORTINT(ix));
 v_9IY0 == mem(SSHORTINT_TO_USHORTINT(iy));

 v_9IX_plus_d0 == mem(256-(ix+rgs8(d0)) );
 v_9IY_plus_d0 == mem(256-(iy+rgs8(d0)) );
 
 z_s  == bitgetSCHAR(rgs8(f0),7);
 z_z  == bitgetSCHAR(rgs8(f0),6);
 z_00 == bitgetSCHAR(rgs8(f0),5);
 z_h  == bitgetSCHAR(rgs8(f0),4);
 z_01 == bitgetSCHAR(rgs8(f0),3);
 z_p  == bitgetSCHAR(rgs8(f0),2);
 z_n  == bitgetSCHAR(rgs8(f0),1);
 z_c  == bitgetSCHAR(rgs8(f0),0)

CONSTANTS 
REG16_TO_REG8,
REG8_TO_REG16,

update_flag_register,
get_new_flag_register

  

PROPERTIES


  REG16_TO_REG8 : id_reg_16 --> ( id_reg_8 * id_reg_8)
  & ! (r1,r2,r3).
  (r1 :  id_reg_16  & r2 :  id_reg_8 & r3 :  id_reg_8
   => (r1 = BC =>   REG16_TO_REG8(r1)= ( b0, c0)    ) &
      (r1 = DE =>   REG16_TO_REG8(r1)= ( d0 , e0 )    ) &
      (r1 = HL =>   REG16_TO_REG8(r1)= ( h0 , l0 )    ) &
      (r1 = AF =>   REG16_TO_REG8(r1)= ( a0, f0)    ) 
   )
  & 
  REG8_TO_REG16 = REG16_TO_REG8~ &

    update_flag_register = %(rgs8_,c0,n_add_sub,pv2,  h4,  z6,s7).
  (rgs8_ : id_reg_8 --> SCHAR & c0 : BOOL & n_add_sub : BOOL& pv2 : BOOL &   h4 : BOOL &  z6  : BOOL & s7 : BOOL | 
      rgs8_ <+ { (f0 |-> BYTE_TO_SCHAR(SCHAR_TO_BYTE(rgs8_(f0)) <+ {   /* Essa conversão pode ser simplificado, usando + 2^7*v(7)... */
             /*S*/   (7 |-> bool_to_bit( s7) ),
			 /*Z*/	 (6 |-> bool_to_bit(z6 )),

			 /*H*/	 (4 |-> bool_to_bit(h4)),

			 /*P/V*/ (2 |-> bool_to_bit(pv2)),
			 /*N*/	 (1 |-> bool_to_bit(n_add_sub)),
			 /*C*/	 (0 |-> bool_to_bit(c0)) }))  } )

	&
	
	get_new_flag_register = %(rgs8_,c0,n_add_sub,pv2,  h4,  z6,s7).
	(rgs8_ : id_reg_8 --> SCHAR & c0 : BOOL& n_add_sub : BOOL& pv2 : BOOL &   h4 : BOOL &  z6  : BOOL & s7 : BOOL | 
        (f0 |-> BYTE_TO_SCHAR(SCHAR_TO_BYTE(rgs8_(f0)) <+ {			/* Essa conversão pode ser simplificado, usando + 2^7*v(7)... */
             /*S*/   (7 |-> bool_to_bit(s7 )),
			 /*Z*/	 (6 |-> bool_to_bit(z6 )),

			 /*H*/	 (4 |-> bool_to_bit(h4)),

			 /*P/V*/ (2 |-> bool_to_bit(pv2)),
			 /*N*/	 (1 |-> bool_to_bit(n_add_sub)),
			 /*C*/	 (0 |-> bool_to_bit(c0)) }))   )



ASSERTIONS
   dom(stack)= USHORTINT&
   ran(stack)<: SCHAR& 
   ran(mem) <: SCHAR &
   dom(mem) = USHORTINT &
   ran(rgs8) <: SCHAR &
   dom(rgs8) = id_reg_8&
   INSTRUCTION_NEXT(0) = 1 &
   INSTRUCTION_NEXT(1) = 2 &
   INSTRUCTION_NEXT(2) = 3 &
   INSTRUCTION_NEXT(3) = 4 &
   INSTRUCTION_NEXT(4) = 5 &
   INSTRUCTION_NEXT(5) = 6 &
   INSTRUCTION_NEXT(6) = 7 &
   INSTRUCTION_NEXT(7) = 8 &
   INSTRUCTION_NEXT(8) = 9 &
   INSTRUCTION_NEXT(9) = 10&
   INSTRUCTION_NEXT(10) = 11&
   INSTRUCTION_NEXT(11) = 12&
   INSTRUCTION_NEXT(12) = 13&
   INSTRUCTION_NEXT(13) = 14&

   !(vec,in0).(vec : BYTE & in0 : 0..7 
   =>  bitgetSCHAR(BYTE_TO_SCHAR(vec),in0) = vec(in0) ) &

   v_9BC0 : SCHAR  & mem(SSHORTINT_TO_USHORTINT( v_9BC0)) : SCHAR  &
   v_9DE0 : SCHAR  & mem(SSHORTINT_TO_USHORTINT( v_9DE0)) : SCHAR  &
   v_9HL0 : SCHAR  & mem(SSHORTINT_TO_USHORTINT( v_9HL0)) : SCHAR  &
   v_9AF0 : SCHAR  & mem(SSHORTINT_TO_USHORTINT( v_9AF0)) : SCHAR  &

   v_9SP0 : SCHAR  &
   v_9IX0 : SCHAR  &
   v_9IY0 : SCHAR 
   
 

INITIALISATION
   rgs8 :: id_reg_8 -->> SCHAR || 
/*   mem :: USHORTINT --> SCHAR || */
   pc :: USHORTINT || sp :: SSHORTINT || ix :: SSHORTINT || iy :: SSHORTINT ||
/*   stack :: USHORTINT  --> SCHAR ||*/
   i_o_ports  ::  UCHAR -->  SCHAR


OPERATIONS

 LD_r_r_(rr,rr_) =
     PRE rr : id_reg_8 & rr_ : id_reg_8  THEN
	rgs8(rr):= rgs8(rr_) ||
        pc := INSTRUCTION_NEXT(pc)
     END
;
  
 LD_r_n_(rr,n0) =
     PRE rr : id_reg_8 & n0 : SCHAR  THEN
	rgs8(rr):= n0 ||
        pc := INSTRUCTION_NEXT(pc)
     END
;
  
 LD_r_9HL0 (rr) =    
      PRE rr : id_reg_8 THEN
         ANY address WHERE address : SSHORTINT &
            address =  SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)) 
         THEN
	  rgs8(rr):= mem(address) ||
          pc := INSTRUCTION_NEXT(pc)
	 END
      END
;


 LD_r_9IX_d0 (rr , dd) =    
      PRE rr : id_reg_8 & dd : SCHAR  THEN
         ANY address,iszero,overflow 
         WHERE address : SSHORTINT & iszero : BOOL &  overflow : BOOL &
            address,iszero,overflow = add16SCHAR(ix , SCHAR_TO_SSHORTINT(0,dd))
         THEN
	  rgs8(rr):= mem(address) ||
          pc := INSTRUCTION_NEXT(pc)
	 END
      END
;
 LD_r_9IY_d0 (rr , dd) =    
      PRE rr : id_reg_8 & dd : SCHAR  THEN
         ANY address,iszero,overflow 
         WHERE address : SSHORTINT & iszero : BOOL & overflow : BOOL &
            address,iszero,overflow = add16SCHAR(iy , SCHAR_TO_SSHORTINT(0,dd))
         THEN
	  rgs8(rr):= mem(address) ||
          pc := INSTRUCTION_NEXT(pc)
	 END
      END
;
 LD_9HL0_r (rr) =    
      PRE rr : id_reg_8 THEN
         ANY address WHERE address : USHORTINT &
            address = SSHORTINT_TO_USHORTINT(v_9HL0)
           & address /: dom(stack) 
         THEN
	  updateAddressMem((address), rgs8(rr)) ||
          pc := INSTRUCTION_NEXT(pc)
	     END
      END
;


 LD_9IX_d0_r (dd,rr) =    
      PRE  dd : SCHAR & rr : id_reg_8 THEN
         ANY address,iszero,overflow 
         WHERE address : SSHORTINT & iszero : BOOL & overflow : BOOL &
             address,iszero,overflow = add16SCHAR(ix , SCHAR_TO_SSHORTINT(0,dd))
           & address /: dom(stack) 
         THEN
	  updateAddressMem((address), rgs8(rr)) ||
          pc := INSTRUCTION_NEXT(pc)
	 END
      END
;

 LD_9IY_d0_r (dd,rr) =    
      PRE  dd : SCHAR & rr : id_reg_8 THEN
         ANY address,iszero,overflow 
         WHERE address : SSHORTINT & iszero : BOOL & overflow : BOOL &
             address,iszero,overflow = add16SCHAR(iy , SCHAR_TO_SSHORTINT(0,dd))
         THEN
	  updateAddressMem((address), rgs8(rr)) ||
          pc := INSTRUCTION_NEXT(pc)
	 END
      END
;

 LD_9HL0_n (n0) =    
      PRE n0 : SCHAR THEN
         ANY address 
         WHERE address : USHORTINT &
            address =  v_9HL0 &
            address /: dom(stack)
         THEN
	  updateAddressMem((address), n0) ||
          pc := INSTRUCTION_NEXT(pc)
	 END
      END
;

 LD_9IX_d0_n (dd,n0) =    
      PRE  dd : SCHAR & n0 : SCHAR THEN
         ANY address,iszero,overflow 
         WHERE address : SSHORTINT & iszero : BOOL & overflow : BOOL &
             address,iszero,overflow = add16SCHAR(ix , SCHAR_TO_SSHORTINT(0,dd)) 
           & address /: dom(stack)  
         THEN
	  	  updateAddressMem((address), n0 )||
          pc := INSTRUCTION_NEXT(pc)
	 END
      END
;

 LD_9IY_d0_n (dd,n0) =    
      PRE  dd : SCHAR & n0 : SCHAR  THEN
         ANY address,iszero,overflow 
         WHERE address : SSHORTINT & iszero : BOOL & overflow : BOOL &
             address,iszero,overflow = add16SCHAR(iy , SCHAR_TO_SSHORTINT(0,dd))
         THEN
	  updateAddressMem((address),n0 ) ||
          pc := INSTRUCTION_NEXT(pc)
	 END
      END
;

 LD_A_9BC0  =    
      ANY address WHERE address : USHORTINT &
          address =  SSHORTINT_TO_USHORTINT(v_9BC0)
      THEN
	rgs8(a0):= mem(address) ||
        pc := INSTRUCTION_NEXT(pc)
      END
;

 LD_A_9DE0  =    
      ANY address WHERE address : USHORTINT &
          address =  SSHORTINT_TO_USHORTINT(v_9DE0)
      THEN
         rgs8(a0):= mem(address) ||
         pc := INSTRUCTION_NEXT(pc)
      END
;

 LD_A_9nn0(nn)  =    
      PRE nn :  USHORTINT
      THEN 
	   rgs8(a0):= mem(nn) ||
           pc := INSTRUCTION_NEXT(pc)
      END
;

 LD_9BC0_A  =  
      ANY address WHERE address : USHORTINT &
          address = SSHORTINT_TO_USHORTINT(v_9BC0)&
          address /: dom(stack) 
      THEN
          updateAddressMem((address) , rgs8(a0)) ||
          pc := INSTRUCTION_NEXT(pc)
      END
;  


 LD_9DE0_A  =  
      ANY address WHERE address : USHORTINT &
          address =  SSHORTINT_TO_USHORTINT(v_9DE0)
        & address /: dom(stack) 
      THEN
          updateAddressMem((address) , rgs8(a0)) ||
          pc := INSTRUCTION_NEXT(pc)
      END
;  

 LD_9nn0_A(nn)  =    
      PRE nn : USHORTINT
        & nn /: dom(stack) 
      THEN 
	   updateAddressMem((nn), rgs8(a0) )||
           pc := INSTRUCTION_NEXT(pc)
      END
;

/* Interrupcao
LD_A_I=

*/

/*
LD_A_R=

*/

/*
LD_I_A=

*/

/*
LD_A0=

*/


 LD_dd_nn(dd,nn)  =    
      PRE dd : id_reg_16 & nn : SSHORTINT  & dd /= AF 
      THEN 
        IF dd = SP THEN  sp := nn
        ELSE 
           ANY rh,rl,w1,w2 WHERE 
               rh : id_reg_8 &  rl : id_reg_8 &
               w1 : SCHAR & w2 : SCHAR &
               rh,rl =  REG16_TO_REG8(dd) &
               {rh|->w1,rl|->w2} : id_reg_8 --> SCHAR &
               SSHORTINT_TO_SCHAR(nn)= w1,w2
           THEN 
	    rgs8 := rgs8 <+ {rh|->w1,rl|->w2}   ||
            pc := INSTRUCTION_NEXT(pc) 
           END
        END 
     END
;

 LD_IX_nn (nn) =    
      PRE   nn : SSHORTINT 
      THEN
	  ix:= nn ||
          pc := INSTRUCTION_NEXT(pc)
      END
;
 LD_IY_nn (nn) =   
      PRE   nn : SSHORTINT
      THEN
	  iy:= nn ||
          pc := INSTRUCTION_NEXT(pc)
      END

;

 LD_HL_9nn0 (nn) =    
      PRE   nn : SSHORTINT  & nn +1 : SSHORTINT
      THEN
	  	  rgs8 := rgs8 <+ {h0 |-> mem(nn+1),l0|-> mem(nn)} ||
          pc := INSTRUCTION_NEXT(pc)
      END

;

 LD_dd_9nn0(dd,nn)  =    
      PRE dd : id_reg_16 & nn : USHORTINT  & dd /= AF 
      THEN 
        IF dd = SP THEN
        sp := SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(mem(nn+1),mem(nn)))
        ELSE 
           ANY rh,rl,w1,w2 WHERE 
               rh : id_reg_8 &  rl : id_reg_8 &
               w1 : SCHAR & w2 : SCHAR &
               rh,rl =  REG16_TO_REG8(dd) &
               w1 = mem(nn+1)&
               w2 = mem(nn)&
               {rh|->w1,rl|->w2} : id_reg_8 --> SCHAR 
           THEN 
	    rgs8 := rgs8 <+ {rh|->w1,rl|->w2}   ||
            pc := INSTRUCTION_NEXT(pc) 
           END
        END 
     END
;



 LD_IX_9nn0 (nn) =    
      PRE   nn : USHORTINT & nn +1 : USHORTINT
      THEN
	  ix := SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(mem(nn+1),mem(nn))) ||
          pc := INSTRUCTION_NEXT(pc)

      END
;

 LD_IY_9nn0 (nn) =    
      PRE   nn : SSHORTINT & nn +1 : SSHORTINT
      THEN
	  iy := SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(mem(nn+1),mem(nn))) ||
          pc := INSTRUCTION_NEXT(pc)
      END
;

 LD_9nn0_HL (nn) =    
      PRE   nn : USHORTINT & nn +1 : USHORTINT  &
            nn /: dom(stack) & nn+1 /: dom(stack) 
      THEN     
		  updateMem({nn+1|-> rgs8(h0),nn|-> rgs8(l0)})||/*mem:= mem <+ {nn+1|-> rgs8(h0),nn|-> rgs8(l0)}||*/
          pc := INSTRUCTION_NEXT(pc)
      END
;



 LD_9nn0_dd(nn,dd)  =    
      PRE dd : id_reg_16 &  dd /= AF & nn : USHORTINT  & nn+1 : USHORTINT  &
          nn /: dom(stack) &  nn+1 /: dom(stack)
      THEN 
        IF dd = SP 
        THEN 
           ANY vh,vl WHERE 
                vh : SCHAR & vl : SCHAR &   
		SSHORTINT_TO_SCHAR(sp) = vh,vl &
                {(nn+1)|->vh,(nn)|->vl} : SSHORTINT --> SCHAR 
           THEN
            		  updateMem({(nn+1)|->vh,(nn)|->vl})/*updateAddressMem(  {(nn+1)|->vh,(nn)|->vl}*/
           END
        ELSE 
           ANY rh,rl,w1,w2 WHERE 
               rh : id_reg_8 &  rl : id_reg_8 &
               w1 : SCHAR & w2 : SCHAR &
               rh,rl =  REG16_TO_REG8(dd) &
               {(nn+1)|->rgs8(rh),(nn)|->rgs8(rl)} : USHORTINT --> SCHAR 
           THEN 
			updateMem({(nn+1)|->rgs8(rh),(nn)|->rgs8(rl)} )/*updateAddressMem(  {(nn+1)|->rgs8(rh),(nn)|->rgs8(rl)} */  ||
            pc := INSTRUCTION_NEXT(pc) 
           END
        END 
     END
;


 LD_9nn0_IX (nn) =    
      PRE   nn : USHORTINT  & nn +1 : USHORTINT THEN
        ANY h_ix, l_ix  WHERE 
            h_ix : SCHAR & l_ix : SCHAR &
            h_ix,l_ix = SSHORTINT_TO_SCHAR(ix)
           & nn /: dom(stack) & nn+1 /: dom(stack)
        THEN
		  updateMem({nn+1|-> h_ix,nn|-> l_ix})||
          pc := INSTRUCTION_NEXT(pc)
        END
      END
;

 LD_9nn0_IY (nn) =    
      PRE   nn : USHORTINT  & nn +1 : USHORTINT THEN
        ANY h_iy, l_iy  WHERE 
            h_iy : SCHAR & l_iy : SCHAR &
            h_iy,l_iy = SSHORTINT_TO_SCHAR(ix)&
            nn /: dom(stack) & nn+1 /: dom(stack) 
        THEN
	  		updateMem({nn+1|-> h_iy,nn|-> l_iy})||
          pc := INSTRUCTION_NEXT(pc)
        END
      END
;

 LD_SP_HL  =    
      BEGIN
	  sp := SSHORTINT_TO_USHORTINT(v_HL)||
          pc := INSTRUCTION_NEXT(pc)
      END

;

 LD_SP_IX  =    
      BEGIN
	  sp := SSHORTINT_TO_USHORTINT(ix) ||
          pc := INSTRUCTION_NEXT(pc)
      END

;

 LD_SP_IY  =    
      BEGIN
	  sp := SSHORTINT_TO_USHORTINT(iy) ||
          pc := INSTRUCTION_NEXT(pc)
      END

;


 PUSH_qq(qq)  =
      PRE  qq : id_reg_16 & qq /= SP & (sp - 2 ) : USHORTINT
      THEN
        ANY
            qqh,qql
        WHERE qqh : id_reg_8 & qql : id_reg_8 &
              REG16_TO_REG8(qq)= qqh,qql &
              {sp-2 |-> rgs8(qql), sp-1 |-> rgs8(qqh)} : USHORTINT --> SCHAR
        THEN
             updateStack(  {sp-2 |-> rgs8(qql), sp-1 |-> rgs8(qqh)})||
             sp := sp - 2 ||
             pc := INSTRUCTION_NEXT(pc)
        END      
      END
;

 PUSH_IX  =
     PRE   (sp - 2 ) : USHORTINT
     THEN   
       ANY
           wh,wl
       WHERE wh : SCHAR & wl : SCHAR &
             SSHORTINT_TO_SCHAR(ix)= wh,wl &
             {sp-2 |-> wl, sp-1 |-> wh } : USHORTINT --> SCHAR
       THEN
             updateStack(  {sp-2 |-> wl, sp-1 |-> wh })||
             sp := sp - 2 ||
             pc := INSTRUCTION_NEXT(pc)
       END      
     END
;  

 PUSH_IY  =
   
       ANY
           wh,wl
       WHERE wh : SCHAR & wl : SCHAR &
             SSHORTINT_TO_SCHAR(iy)= wh,wl &
             {sp-2 |-> wl, sp-1 |-> wh } : USHORTINT --> SCHAR
       THEN
             updateStack(  {sp-2 |-> wl, sp-1 |-> wh })||
             sp := sp - 2
       END      
;

 POP_qq(qq)  =
      PRE  qq : id_reg_16 & qq /= SP &  (sp + 2 ) : USHORTINT
      THEN
        ANY
            qqh,qql
        WHERE qqh : id_reg_8 & qql : id_reg_8 &
              REG16_TO_REG8(qq)= qqh,qql &
              { qql |-> mem(sp+2) , qqh|->mem(sp+1)} : id_reg_8 --> SCHAR
        THEN
              rgs8 := rgs8 <+ {qql |-> mem(sp+2) , qqh |->mem(sp+1)}||
              sp := sp + 2 ||
              pc := INSTRUCTION_NEXT(pc)
        END      
      END
;

 POP_IX  =
      PRE   sp : USHORTINT & (sp + 2 ) : USHORTINT 
      THEN
       ANY
           nw8
       WHERE nw8 : SCHAR  &
             SCHAR_TO_SSHORTINT(mem(sp+1),mem(sp+2))= nw8 
       THEN
             ix := nw8 ||
             sp := sp + 2 ||
             pc := INSTRUCTION_NEXT(pc)
       END
     END      
;


 POP_IY  =
      PRE  (sp + 2 ) : USHORTINT
      THEN

       ANY
           nw8
       WHERE nw8 : SCHAR  &
             SCHAR_TO_SSHORTINT(mem(sp+1),mem(sp+2))= nw8
       THEN
             iy := nw8 ||
             sp := sp + 2 ||
             pc := INSTRUCTION_NEXT(pc)
       END      
      END
;

 EX_DE_HL = /* Duvida isso pode ser feito assim mesmo */
      
      PRE
      { d0 |-> rgs8(h0), e0 |-> rgs8(l0),  h0 |-> rgs8(d0), l0 |-> rgs8(e0)} :  id_reg_8 --> SCHAR
      THEN
       rgs8 := rgs8 <+ { d0 |-> rgs8(h0), e0 |-> rgs8(l0),  h0 |-> rgs8(d0), l0 |-> rgs8(e0)}||
       pc := INSTRUCTION_NEXT(pc)
      END
; 


 EX_AF_AF_ = 

      PRE
      { a0|-> rgs8(a_0), f0|-> rgs8(f_0),a_0 |-> rgs8(a0), f_0 |-> rgs8(f0)} :  id_reg_8 --> SCHAR
      THEN
       rgs8 := rgs8 <+ { a0|-> rgs8(a_0), f0|-> rgs8(f_0),a_0 |-> rgs8(a0), f_0 |-> rgs8(f0)}||
       pc := INSTRUCTION_NEXT(pc)
      END
;


 EXX =
      PRE
       { b0|-> rgs8(b_0), c0|-> rgs8(c_0), d0 |-> rgs8(d_0), e0 |-> rgs8(e_0), h0 |-> rgs8(h_0), l0 |-> rgs8(l_0),
                        b_0 |-> rgs8(b0), c_0 |-> rgs8(c0), d_0 |-> rgs8(d0), e_0 |-> rgs8(e0), h_0 |-> rgs8(h0), l_0 |-> rgs8(l0)} : id_reg_8 --> SCHAR
      THEN
       rgs8 := rgs8 <+ { b0|-> rgs8(b_0), c0|-> rgs8(c_0), d0 |-> rgs8(d_0), e0 |-> rgs8(e_0), h0 |-> rgs8(h_0), l0 |-> rgs8(l_0),
                        b_0 |-> rgs8(b0), c_0 |-> rgs8(c0), d_0 |-> rgs8(d0), e_0 |-> rgs8(e0), h_0 |-> rgs8(h0), l_0 |-> rgs8(l0)}||
       pc := INSTRUCTION_NEXT(pc)
      END
;
 
 EX_9SP0_HL =
      PRE {(sp+1) |-> rgs8(h0),sp |-> rgs8(l0)} : USHORTINT --> SCHAR &
          stack <+ { (sp+1) |-> rgs8(h0), sp |-> rgs8(l0)}  <: mem  
      THEN
       rgs8 := rgs8 <+ { h0 |-> stack(sp+1), l0|->stack(sp)}||
       updateStack(  { (sp+1) |-> rgs8(h0), sp |-> rgs8(l0)})||
       pc := INSTRUCTION_NEXT(pc)
      END
 ;

 EX_9SP0_IX =
      ANY wh,wl
      WHERE wh : SCHAR & wl : SCHAR &
            SSHORTINT_TO_SCHAR(ix)= wh,wl &
            {(sp+1) |-> wh, sp |-> wl } : USHORTINT --> SCHAR &
            stack <+ { (sp+1) |-> wh, sp |-> wl } <: mem  
      THEN
       ix := SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT( stack((sp+1)), stack(sp)))||
       updateStack(  { (sp+1) |-> wh, sp |-> wl })||
       pc := INSTRUCTION_NEXT(pc)
      END
 ;

 EX_9SP0_IY =
      ANY wh,wl
      WHERE wh : SCHAR & wl : SCHAR &
            SSHORTINT_TO_SCHAR(iy)= wh,wl &
            { (sp+1) |-> wh, sp |-> wl } : USHORTINT --> SCHAR &
            stack <+ { (sp+1) |-> wh, sp |-> wl } <: mem
      THEN
       iy := SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT( stack(sp+1), stack(sp)))||
       updateStack(  { (sp+1) |-> wh, sp |-> wl } )||
       pc := INSTRUCTION_NEXT(pc)
      END
 ;


LDI = 
   PRE   v_9HL0 + 1 : SSHORTINT & v_9DE0 + 1 : SSHORTINT &
         v_BC   - 1 : SSHORTINT 
   THEN 
    ANY  hvn,lvn, dvn,evn, bvn, cvn
    WHERE 
          hvn,lvn = SSHORTINT_TO_SCHAR(v_HL + 1)&
          dvn,evn = SSHORTINT_TO_SCHAR(v_DE + 1)&
          bvn,cvn = SSHORTINT_TO_SCHAR(v_BC - 1)

    THEN
	  updateAddressMem( SSHORTINT_TO_USHORTINT(v_9DE0) ,v_9HL0 ) ||
	  rgs8 :=  rgs8 <+ {(h0|-> hvn), (l0 |-> lvn), 
		           (d0|-> hvn), (e0 |-> lvn), 
		           (b0|-> hvn), (c0 |-> lvn) }||
	  pc := INSTRUCTION_NEXT(pc)

         /* Sem o efeito nos bits condicionais pg livro128,pdf 141 */

    END
   END


;

 LDIR = 
   PRE   v_9HL0 + 1 : SSHORTINT & v_9DE0 + 1 : SSHORTINT &
         v_BC   - 1 : SSHORTINT 
   THEN 
    ANY  hvn,lvn, dvn,evn, bvn, cvn
    WHERE 
          hvn,lvn = SSHORTINT_TO_SCHAR(v_HL + 1)&
          dvn,evn = SSHORTINT_TO_SCHAR(v_DE + 1)&
          bvn,cvn = SSHORTINT_TO_SCHAR(v_BC - 1)&
          {(h0|-> hvn), (l0 |-> lvn), 
           (d0|-> hvn), (e0 |-> lvn), 
           (b0|-> hvn), (c0 |-> lvn) } : id_reg_8 --> SCHAR
    THEN
	  updateAddressMem( SSHORTINT_TO_USHORTINT(v_9DE0) , mem(SSHORTINT_TO_USHORTINT(v_9HL0) ) ) ||
	  rgs8 :=  rgs8 <+ {(h0|-> hvn), (l0 |-> lvn), 
		           (d0|-> hvn), (e0 |-> lvn), 
		           (b0|-> hvn), (c0 |-> lvn) }||
	  IF  v_BC -1  = 0 THEN pc := INSTRUCTION_NEXT(pc)  END
	/* Sem o efeito nos bits condicionais pg livro128,pdf 141 */
     END      

   END

;


LDD = 
   PRE   v_9HL0 - 1 : SSHORTINT & v_9DE0 - 1 : SSHORTINT &
         v_BC   - 1 : SSHORTINT 
   THEN 
    ANY  hvn,lvn, dvn,evn, bvn, cvn
    WHERE 
          hvn,lvn = SSHORTINT_TO_SCHAR(v_HL - 1)&
          dvn,evn = SSHORTINT_TO_SCHAR(v_DE - 1)&
          bvn,cvn = SSHORTINT_TO_SCHAR(v_BC - 1)&
          {(h0|-> hvn), (l0 |-> lvn), 
           (d0|-> hvn), (e0 |-> lvn), 
           (b0|-> hvn), (c0 |-> lvn) } : id_reg_8 --> SCHAR
    THEN
	  updateAddressMem(( SSHORTINT_TO_USHORTINT(v_9DE0) ), mem(SSHORTINT_TO_USHORTINT(v_9HL0) ) )||
	  rgs8 :=  rgs8 <+ {(h0|-> hvn), (l0 |-> lvn), 
		           (d0|-> hvn), (e0 |-> lvn), 
		           (b0|-> hvn), (c0 |-> lvn) }||
	  IF  v_BC -1  = 0 THEN pc := INSTRUCTION_NEXT(pc)  END
	/* Sem o efeito nos bits condicionais pg livro128,pdf 141 */
     END      

   END

;


LDDR = 
   PRE   v_9HL0 - 1 : SSHORTINT & v_9DE0 - 1 : SSHORTINT &
         v_BC   - 1 : SSHORTINT 
   THEN 
    ANY  hvn,lvn, dvn,evn, bvn, cvn
    WHERE 
          hvn,lvn = SSHORTINT_TO_SCHAR(v_HL - 1)&
          dvn,evn = SSHORTINT_TO_SCHAR(v_DE - 1)&
          bvn,cvn = SSHORTINT_TO_SCHAR(v_BC - 1)&
          {(h0|-> hvn), (l0 |-> lvn), 
           (d0|-> hvn), (e0 |-> lvn), 
           (b0|-> hvn), (c0 |-> lvn) } : id_reg_8 --> SCHAR
    THEN
	  updateAddressMem(( SSHORTINT_TO_USHORTINT(v_9DE0) ),mem(SSHORTINT_TO_USHORTINT(v_9HL0) ) ) ||
	  rgs8 :=  rgs8 <+ {(h0|-> hvn), (l0 |-> lvn), 
		           (d0|-> hvn), (e0 |-> lvn), 
		           (b0|-> hvn), (c0 |-> lvn) }||
	  pc := INSTRUCTION_NEXT(pc)
	/* Sem o efeito nos bits condicionais pg livro128,pdf 141 */
     END      

   END

;


CPI = 
	ANY 
	    sum, is_negative, carry, digit_carry, zero
	WHERE
	    sum : SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero : BOOL	&
	    substract8SCHAR ( rgs8(a0),v_9HL0) =  sum, is_negative, carry, digit_carry, zero
	THEN
	    IF zero = TRUE THEN 
			rgs8 := { /*(BC)-1 e ... */   get_new_flag_register(rgs8, carry, TRUE, bool( v_BC - 1 /= 0),digit_carry,zero, is_negative )}
				    
		END 
		||
          pc := INSTRUCTION_NEXT(pc)
	END
	;
	
/*
;

CPIR = 

;

CPD = 

;

CPDR = 

;
       */
 
 
/*
ADD_A_r(rr)=   
  PRE rr : id_register_8  
  THEN  
     ANY w1,z,overflow
     WHERE  w1 : SCHAR & z : BOOL & overflow : BOOL &
        w1,z,overflow = add(rgs8(a0),rgs8(rr))
        add : (SCHAR * SCHAR) --> (SCHAR * BOOL * BOOL)
     THEN  
       rgs8:= rgs8 <+ { a0|-> rgs(a0) + rgs8(rr), f_0 |-> &   } 
     END  
  END  
;
 INC_r(rr) =
      PRE 
	rr : id_register_8
	THEN
	ANY address,nw8 WHERE address : SSHORTINT  &
             address =  SSHORTINT_TO_VALUE(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))
	 THEN 
          mem(address):= BYTE_TO_SCHAR(SCHAR_TO_BYTE(mem(address)+1)) ||
	  rgs8 := rgs8 <+ {(rr |->rgs8(rr)+1 ), (f0|-> FUNC_INC_r(rgs8(f0))) }	 ||
          pc := INSTRUCTION_NEXT(pc)
	END
      END
;


*/

/* Intruncoes fora da sequencia */

/* 	 add (w1, w2) = (sum, is_negative, carry, digit_carry, zero) 
	add8SCHAR : (SCHAR * SCHAR) --> (SCHAR * BOOL * BOOL * BOOL * BOOL)*/


 BIT_b_rr (bb,rr) =  
      PRE bb : 0..(SCHAR_LENGTH-1)& rr : id_reg_8 THEN
         ANY nw8,ib WHERE  ib : BIT & nw8 : BYTE &
             ib = SCHAR_TO_BYTE( rgs8(rr)  )(bb)&
              nw8 = bv_set( SCHAR_TO_BYTE(rgs8(f0)),6 )   /*RETIFICAR ISSO COM UMA NOVA FUNC nw8 = BYTE_SET_BIT( SCHAR_TO_BYTE(rgs8(f0)),6, BIT_FLIP(ib)  )  */
         THEN
	  rgs8(f_0) := BYTE_TO_SCHAR(nw8) ||
          pc := INSTRUCTION_NEXT(pc)
	 END
      END
;
 BIT_b_9HL0 (bb) =  
      PRE bb : 0..(SCHAR_LENGTH-1) THEN
         ANY address,nw8,ib WHERE address : SSHORTINT & ib : BIT & nw8 : BYTE &
             address =  SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))&
             ib = SCHAR_TO_BYTE((mem(address)))(bb)&
             nw8 = bv_set( SCHAR_TO_BYTE(rgs8(f_0)),6) /*RETIFICAR ISSO COM UMA NOVA FUNC nw8 = BYTE_SET_BIT( SCHAR_TO_BYTE(rgs8(f_0)),6, ib)*/
         THEN
	  rgs8(f_0) := BYTE_TO_SCHAR(nw8) ||
          pc := INSTRUCTION_NEXT(pc)
	 END
      END
;

 JR_NZ_e (ee) = 
      PRE ee : -126..129 & (ee + pc): USHORTINT THEN
         IF SCHAR_TO_BYTE(rgs8(f_0))(6) = 1 THEN   pc := INSTRUCTION_NEXT(pc)
         ELSE pc := INSTRUCTION_JUMP(pc,ee) 
         END
      END
;



 JR_Z_e (ee) = 
      PRE ee : -126..129 & (ee + pc): USHORTINT THEN
         IF SCHAR_TO_BYTE(rgs8(f_0))(6) = 0 THEN   pc := INSTRUCTION_NEXT(pc)
         ELSE pc := INSTRUCTION_JUMP(pc,ee) 
         END
      END
;


 SET_b_HL (bb)=  
      PRE bb : 0..(SCHAR_LENGTH-1) THEN
         ANY address,nw8 WHERE address : USHORTINT &  nw8 : BYTE &
             address =  v_HL &
             address /: dom(stack) &
             nw8 = bv_set( SCHAR_TO_BYTE(mem(address)),bb) 
         THEN    
	  	  updateAddressMem((address) , BYTE_TO_SCHAR(nw8) )||
          pc := INSTRUCTION_NEXT(pc)
	 END
      END
;

 RES_b_HL (bb)=  
      PRE bb : 0..(SCHAR_LENGTH-1) THEN
         ANY address,nw8 WHERE address : USHORTINT &  nw8 : BYTE &
             address =   v_HL &
             address /: dom(stack) &
             nw8 = bv_clear( SCHAR_TO_BYTE(mem(address)),bb)
         THEN    
	  		updateAddressMem(address,BYTE_TO_SCHAR(nw8)) ||
          pc := INSTRUCTION_NEXT(pc)
	 END
      END
;
 GOTO (k0) =
      PRE k0 : USHORTINT THEN
         pc := k0
      END
;
 CALL (k0) =
    PRE k0 : USHORTINT & sp + 1 : USHORTINT THEN
        updateAddressStack( stack(sp) , INSTRUCTION_NEXT(pc) ) ||
        sp := sp + 1 ||
        pc := k0
    END
 ;
 RETURN =
    PRE sp > 0 THEN
        pop(sp)  /*stack := {sp - 1} <<| stack */ ||
        pc := stack(sp - 1) ||
        sp := sp - 1
    END
 ;    


/* Talves eu possa eliminar o conceito de barramento e deixar
apenas o de porta, certificar quando fizer as instrucoes de interrup��o */
 IN_A_9n0_(nn,k_in)= /* Informando algo sobre o valor de entrada, pode passar uma constante definida em properties*/
    PRE nn : UCHAR & k_in : SCHAR   THEN
     ANY data_in WHERE data_in : SCHAR THEN
	  i_o_ports (nn) := data_in ||
          rgs8(a0) :=   k_in ||
          pc := INSTRUCTION_NEXT(pc)
     END
    END
;


 IN_A_9n0(nn)=  /* sem informar nada sobre o valor de entrada*/
    PRE nn : UCHAR    THEN
     ANY data_in WHERE data_in : SCHAR THEN
	  i_o_ports (nn) := data_in ||
          rgs8(a0) :=   i_o_ports (nn) ||
          pc := INSTRUCTION_NEXT(pc)
     END
    END
;
 /*IN_r_9C0 (rr)=
   PRE	 rr : id_reg_8   THEN
    ANY data_in, 
		sum, is_negative, carry, digit_carry, zero 
	WHERE data_in : SCHAR & sum: SCHAR & is_negative : BOOL & carry: BOOL & digit_carry: BOOL & zero : BOOL &
	    add8SCHAR(data_in,0) = sum, is_negative, carry, digit_carry, zero 
	THEN
	  i_o_ports (rgs8(c0)) := data_in ||

	  rgs8 := rgs8 <+ {( rr |-> i_o_ports (rgs8(c0))),    
	  get_new_flag_register(rgs8, z_c,FALSE,z_p,z_h, zero,is_negative) }    ||
	  pc := INSTRUCTION_NEXT(pc)
             COM o efeito nos bits condicionais pg livro272,pdf 290 


    END
   END
;*/

 INI =
   PRE  rgs8(b0)-1 : SCHAR & v_9HL0 + 1 : SSHORTINT THEN 
    ANY data_in, hvn,lvn
    WHERE data_in : SCHAR & 
          hvn,lvn = SSHORTINT_TO_SCHAR(v_9HL0 + 1)&
          {(h0|-> hvn), (l0 |-> lvn), (b0|->rgs8(b0)-1) } : id_reg_8 --> SCHAR
    THEN
	  i_o_ports (rgs8(c0)) := data_in ||
	  updateAddressMem((SSHORTINT_TO_USHORTINT(v_9HL0) ), i_o_ports (rgs8(c0)) )||
	  rgs8 :=  rgs8 <+ {(h0|-> hvn), (l0 |-> lvn), (b0|->rgs8(b0)-1) }||
	  pc := INSTRUCTION_NEXT(pc)

         /* Sem o efeito nos bits condicionais pg livro273,pdf 291 */

    END
   END
 
;

 INIR =
   PRE  rgs8(b0)-1 : SCHAR & v_9HL0 + 1 : SSHORTINT THEN 
    ANY data_in, hvn,lvn
    WHERE data_in : SCHAR & 
          hvn,lvn = SSHORTINT_TO_SCHAR(v_9HL0 + 1)&
          {(h0|-> hvn), (l0 |-> lvn), (b0|->rgs8(b0)-1) } : id_reg_8 --> SCHAR
    THEN
	  i_o_ports (rgs8(c0)) := data_in ||
	  updateAddressMem((SSHORTINT_TO_USHORTINT(v_9HL0) ), i_o_ports (rgs8(c0))) ||
	  rgs8 :=  rgs8 <+ {(h0|-> hvn), (l0 |-> lvn), (b0|->rgs8(b0)-1) }||
	  IF  rgs8(b0)-1 = 0 THEN pc := INSTRUCTION_NEXT(pc)  END
         /* Sem o efeito nos bits condicionais pg livro273,pdf 291 */

    END
   END
 
;
 
 IND =
   PRE  rgs8(b0)-1 : SCHAR & v_9HL0 - 1 : SSHORTINT THEN 
    ANY data_in, hvn,lvn
    WHERE data_in : SCHAR & 
          hvn,lvn = SSHORTINT_TO_SCHAR(v_9HL0 - 1)&
          {(h0|-> hvn), (l0 |-> lvn), (b0|->rgs8(b0)-1) } : id_reg_8 --> SCHAR
    THEN
	  i_o_ports (rgs8(c0)) := data_in ||
	  updateAddressMem((SSHORTINT_TO_USHORTINT(v_9HL0) ), i_o_ports (rgs8(c0))) ||
	  rgs8 :=  rgs8 <+ {(h0|-> hvn), (l0 |-> lvn), (b0|->rgs8(b0)-1) }||
	  pc := INSTRUCTION_NEXT(pc)


         /* Sem o efeito nos bits condicionais pg livro273,pdf 291 */

    END
   END
 
;
 
 INDR =
   PRE  rgs8(b0)-1 : SCHAR & v_9HL0 - 1 : SSHORTINT THEN 
    ANY data_in, hvn,lvn
    WHERE data_in : SCHAR & 
          hvn,lvn = SSHORTINT_TO_SCHAR(v_9HL0 - 1)&
          {(h0|-> hvn), (l0 |-> lvn), (b0|->rgs8(b0)-1) } : id_reg_8 --> SCHAR
    THEN
	  i_o_ports (rgs8(c0)) := data_in ||
	  updateAddressMem((SSHORTINT_TO_USHORTINT(v_9HL0) ), i_o_ports (rgs8(c0))) ||
	  rgs8 :=  rgs8 <+ {(h0|-> hvn), (l0 |-> lvn), (b0|->rgs8(b0)-1) }||
  	  IF  rgs8(b0)-1 = 0 THEN pc := INSTRUCTION_NEXT(pc)  END

         /* Sem o efeito nos bits condicionais pg livro273,pdf 291 */

    END
   END
 
;




 OUT_9n0_A(nn)=
   PRE nn : UCHAR    THEN 
          i_o_ports (nn) :=  rgs8(a0) ||
          pc := INSTRUCTION_NEXT(pc)
    END
;


 OUT_9C0_r(rr)=
   PRE rr : id_reg_8   THEN 
          i_o_ports (rgs8(rr)) :=  rgs8(c0) ||
          pc := INSTRUCTION_NEXT(pc)
    END;

 OUTI = 
   PRE  rgs8(b0)-1 : SCHAR & v_9HL0 + 1 : SSHORTINT THEN 
    ANY  hvn,lvn
    WHERE hvn,lvn = SSHORTINT_TO_SCHAR(v_9HL0 + 1)&
          {(h0|-> hvn), (l0 |-> lvn), (b0|->rgs8(b0)-1) } : id_reg_8 --> SCHAR
    THEN
	  i_o_ports(rgs8(c0)) := mem(SSHORTINT_TO_USHORTINT(v_9HL0) ) ||
	  rgs8 :=  rgs8 <+ {(h0|-> hvn), (l0 |-> lvn), (b0|->rgs8(b0)-1) }||
	  pc := INSTRUCTION_NEXT(pc)
         /* Sem o efeito nos bits condicionais pg livro273,pdf 291 */
    END
   END
 
;
 OUTIR = 
   PRE  rgs8(b0)-1 : SCHAR & v_9HL0 + 1 : SSHORTINT THEN 
    ANY  hvn,lvn
    WHERE hvn,lvn = SSHORTINT_TO_SCHAR(v_9HL0 + 1)&
          {(h0|-> hvn), (l0 |-> lvn), (b0|->rgs8(b0)-1) } : id_reg_8 --> SCHAR
    THEN
	  i_o_ports(rgs8(c0)) := mem(SSHORTINT_TO_USHORTINT(v_9HL0) ) ||
	  rgs8 :=  rgs8 <+ {(h0|-> hvn), (l0 |-> lvn), (b0|->rgs8(b0)-1) }||
          IF  rgs8(b0)-1 = 0 THEN pc := INSTRUCTION_NEXT(pc)  END
         /* Sem o efeito nos bits condicionais pg livro273,pdf 291 */
    END
   END
 

  



/*;
 INC_HL =
      ANY address,nw8 WHERE address : SSHORTINT  &
             address =  SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))

      THEN 
          mem(address):= BYTE_TO_SCHAR(SCHAR_TO_BYTE(mem(address))+1) ||
          pc := INSTRUCTION_NEXT(pc)
      END
*/





END