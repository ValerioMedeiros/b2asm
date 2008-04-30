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

SEES
TYPES ,
ALU


SETS
 id_register8 = { a , f , f_ , a_ ,
                 b , c , b_ , c_ ,
                 d , e , d_ , e_ ,
                 h , l , h_ , l_ ,
                 i , r }
CONCRETE_VARIABLES
   rgs8 ,
   mem ,
   pc , sp , ix , iy ,
   stack

INVARIANT
   stack : WORD16  --> WORD8 &
   rgs8 : id_register8 --> WORD8 &
   mem : WORD16 --> WORD8 &
   pc : WORD16 & sp : WORD16 & ix : WORD16 & iy : WORD16 /*&
   stack <:  mem */

ASSERTIONS
   dom ( stack ) = WORD16 &
   ran ( stack ) <: WORD8 &
   ran ( mem ) <: WORD8 &
   dom ( mem ) = WORD16 &
   ran ( rgs8 ) <: WORD8 &
   dom ( rgs8 ) = id_register8 &
   INSTRUCTION_NEXT ( 0 ) = 1 &
   INSTRUCTION_NEXT ( 1 ) = 2 &
   INSTRUCTION_NEXT ( 2 ) = 3 &
   INSTRUCTION_NEXT ( 3 ) = 4 &
   INSTRUCTION_NEXT ( 4 ) = 5 &
   INSTRUCTION_NEXT ( 5 ) = 6 &
   INSTRUCTION_NEXT ( 6 ) = 7 &
   INSTRUCTION_NEXT ( 7 ) = 8 &
   INSTRUCTION_NEXT ( 8 ) = 9 &
   INSTRUCTION_NEXT ( 9 ) = 10

INITIALISATION
   rgs8 :: id_register8 -->> WORD8 ||
   mem :: WORD16 --> WORD8 ||
   pc :: WORD16 || sp :: WORD16 || ix :: WORD16 || iy :: WORD16 ||
   stack :: WORD16  --> WORD8

OPERATIONS



 LD_r_r_ ( rr , rr_ ) =
     PRE rr : id_register8 & rr_ : id_register8  THEN
        rgs8 ( rr ) := rgs8 ( rr_ ) ||
        pc := INSTRUCTION_NEXT ( pc )
     END
;

 LD_r_n_ ( rr , n ) =
     PRE rr : id_register8 & n : WORD8  THEN
        rgs8 ( rr ) := n ||
        pc := INSTRUCTION_NEXT ( pc )
     END
;

 LD_r_HL ( rr ) =
      PRE rr : id_register8 THEN
         ANY address WHERE address : WORD16 &
            address =  WORD8_TO_WORD16 ( rgs8 ( h ) , rgs8 ( l ) )
         THEN
          rgs8 ( rr ) := mem ( address ) ||
          pc := INSTRUCTION_NEXT ( pc )
         END
      END
;

 LD_r_9ix_d0 ( rr , dd ) =
      PRE rr : id_register8 & dd : WORD8  THEN
         ANY address , iszero , overflow WHERE address : WORD16 & iszero : BOOL & overflow : BOOL &
            address , iszero , overflow = add16 ( ix , WORD8_TO_WORD16 ( 0 , dd ) )
         THEN
          rgs8 ( rr ) := mem ( address ) ||
          pc := INSTRUCTION_NEXT ( pc )
         END
      END
;

/* Intruncoes fora da sequencia */

 LD_HL_r ( rr ) =
      PRE rr : id_register8 THEN
         ANY address WHERE address : WORD16 &
            address =  WORD8_TO_WORD16 ( rgs8 ( h ) , rgs8 ( l ) )
         THEN
          mem ( address ) := rgs8 ( rr ) ||
          pc := INSTRUCTION_NEXT ( pc )
         END
      END
;
 BIT_b_HL ( bb ) = /* Vc conhece uma melhor  forma de modelar o tipo de b (questionamento)*/
      PRE bb : 0 .. ( WORD8_LENGTH - 1 ) THEN
         ANY address , nw8 , ib WHERE address : WORD16 & ib : BIT & nw8 : BV8 &
             address =  WORD8_TO_WORD16 ( rgs8 ( h ) , rgs8 ( l ) ) &
             ib = WORD8_TO_BV8 ( ( mem ( address ) ) ) ( bb ) &
             nw8 = BV8_SET_BIT ( WORD8_TO_BV8 ( rgs8 ( f_ ) ) , 6 , ib )
         THEN
          rgs8 ( f_ ) := BV8_TO_WORD8 ( nw8 ) ||
          pc := INSTRUCTION_NEXT ( pc )
         END
      END
;
 JP_z_e  =

      IF WORD8_TO_BV8 ( rgs8 ( f_ ) ) ( 6 ) = 0 THEN pc := INSTRUCTION_NEXT ( pc )
      ELSE pc := INSTRUCTION_NEXT ( INSTRUCTION_NEXT ( pc ) )
      END
;


 SET_b_HL ( bb ) =
      PRE bb : 0 .. ( WORD8_LENGTH - 1 ) THEN
         ANY address , nw8 WHERE address : WORD16 & nw8 : BV8 &
             address =  WORD8_TO_WORD16 ( rgs8 ( h ) , rgs8 ( l ) ) &
             nw8 = BV8_SET_BIT ( WORD8_TO_BV8 ( mem ( address ) ) , bb , 1 )
         THEN
          mem ( address ) := BV8_TO_WORD8 ( nw8 ) ||
          pc := INSTRUCTION_NEXT ( pc )
         END
      END
;

 RES_b_HL ( bb ) =
      PRE bb : 0 .. ( WORD8_LENGTH - 1 ) THEN
         ANY address , nw8 WHERE address : WORD16 & nw8 : BV8 &
             address =  WORD8_TO_WORD16 ( rgs8 ( h ) , rgs8 ( l ) ) &
             nw8 = BV8_SET_BIT ( WORD8_TO_BV8 ( mem ( address ) ) , bb , 0 )
         THEN
          mem ( address ) := BV8_TO_WORD8 ( nw8 ) ||
          pc := INSTRUCTION_NEXT ( pc )
         END
      END
;
 GOTO ( k ) =
      PRE k : INSTRUCTION THEN
         pc := k
      END
;
 CALL ( k ) =
    PRE k : INSTRUCTION THEN
        stack ( sp ) := INSTRUCTION_NEXT ( pc ) ||
        sp := sp + 1 ||
        pc := k
    END
 ;
 RETURN =
    PRE sp > 0 THEN
        stack := { sp - 1 } <<| stack ||
        pc := stack ( sp - 1 ) ||
        sp := sp - 1
    END


/*;
 INC_HL =
      ANY address,nw8 WHERE address : WORD16  &
             address =  WORD8_TO_WORD16(rgs8(h),rgs8(l))

      THEN 
          mem(address):= BV8_TO_WORD8(WORD8_TO_BV8(mem(address))+1) ||
          pc := INSTRUCTION_NEXT(pc)
      END
*/

/* Duvida a retirar sozinho, 
qual o registrador de flag utilizado "f" ou "f_" */



END
