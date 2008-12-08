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
   Z80LogAri
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
                     ( 7 |-> s7 ) ,
                                       ( 6 |-> z6 ) ,

                                       ( 4 |-> h4 ) ,

                                 ( 2 |-> pv2 ) ,
                                       ( 1 |-> n_add_sub ) ,
                                       ( 0 |-> c0 ) } ) } )

        &

        get_new_flag_register_SZ_H_PvNC = % ( rgs8_ , s7 , z6 , h4 , pv2 , n_add_sub , c0 ) .
        ( rgs8_ : id_reg_8 --> BYTE & s7 : BIT & z6  : BIT &   h4 : BIT  & pv2 : BIT & n_add_sub : BIT & c0 : BIT |
        ( f0 |-> {
                     ( 7 |-> s7 ) ,
                                       ( 6 |-> z6 ) ,

                                       ( 4 |-> h4 ) ,

                                 ( 2 |-> pv2 ) ,
                                       ( 1 |-> n_add_sub ) ,
                                       ( 0 |-> c0 ) } ) ) &

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

   mem ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) : BYTE & mem ( BYTE_TO_BV16 ( SCHAR_TO_BYTE ( 0 ) , mem ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) ) ) : BYTE &
   mem ( BYTE_TO_BV16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ) : BYTE & mem ( BYTE_TO_BV16 ( SCHAR_TO_BYTE ( 0 ) , mem ( BYTE_TO_BV16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ) ) ) : BYTE &
   mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) : BYTE & mem ( BYTE_TO_BV16 ( SCHAR_TO_BYTE ( 0 ) , mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) ) : BYTE &
   mem ( BYTE_TO_BV16 ( rgs8 ( a0 ) , rgs8 ( f0 ) ) ) : BYTE & mem ( BYTE_TO_BV16 ( SCHAR_TO_BYTE ( 0 ) , mem ( BYTE_TO_BV16 ( rgs8 ( a0 ) , rgs8 ( f0 ) ) ) ) ) : BYTE &

   mem ( sp ) : BYTE &
   mem ( ix ) : BYTE &
   mem ( iy ) : BYTE &

   ! ( ii , des ) . ( ii : BV16 & des : SCHAR => bv_IX_plus_d ( ii , des ) : BV16 ) &
   ! ( ii , des ) . ( ii : BV16 & des : SCHAR => bv_IY_plus_d ( ii , des ) : BV16 ) &

   ! ( mmm , ii , des ) . ( mmm : BV16 >-> BYTE & ii : BV16 & des : SCHAR => bv_9IX_plus_d0 ( mmm , ii , des ) : BYTE ) &
   ! ( mmm , ii , des ) . ( mmm : BV16 >-> BYTE & ii : BV16 & des : SCHAR => bv_9IY_plus_d0 ( mmm , ii , des ) : BYTE )

INITIALISATION

   rgs8 :: id_reg_8 --> BYTE ||
   pc :: INSTRUCTION || sp :: BV16 || ix :: BV16 || iy :: BV16 ||
   i_o_ports  :: BYTE --> BYTE


OPERATIONS


ADD_A_r ( rr ) =
  PRE rr : id_reg_8
  THEN
     ANY
         sum , negative , carry , half_carry , zero
     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = add8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( rgs8 ( rr ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , ( get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) ) }         ||  pc := INSTRUCTION_NEXT ( pc )
     END
  END

;
ADD_A_n ( n1 ) =
  PRE n1 : SCHAR
  THEN
     ANY
         sum , negative , carry , half_carry , zero

     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = add8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , n1 )
     THEN
       rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

ADD_A_9HL0 =
     ANY
         sum , negative , carry , half_carry , zero

     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = add8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

;

ADD_A_9IX_d0 ( desloc ) =
        PRE desloc : SCHAR THEN
                ANY
                 sum , negative , carry , half_carry , zero

             WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

                sum , negative , carry , half_carry , zero = add8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( bv_9IX_plus_d0 ( mem , ix , desloc ) ) )
                THEN
               rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
               pc := INSTRUCTION_NEXT ( pc )
                END
        END

;

ADD_A_9IY_d0 ( desloc ) =
        PRE desloc : SCHAR THEN
                ANY
                 sum , negative , carry , half_carry , zero

             WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

                sum , negative , carry , half_carry , zero = add8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( bv_9IY_plus_d0 ( mem , iy , desloc ) ) )
                THEN
               rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
               pc := INSTRUCTION_NEXT ( pc )
                END
        END
;


ADC_A_r ( rr ) =
  PRE rr : id_reg_8
  THEN
     ANY
         sum , negative , carry , half_carry , zero

         WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = add8SCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( rgs8 ( rr ) ) )
     THEN
               rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
                   pc := INSTRUCTION_NEXT ( pc )
         END
  END
 ;

ADC_A_n ( n1 ) =
  PRE n1 : SCHAR
  THEN
     ANY
         sum , negative , carry , half_carry , zero

     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = add8SCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , n1 )
     THEN
               rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
;

ADC_A_9HL0 =
     ANY
         sum , negative , carry , half_carry , zero
     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
        sum , negative , carry , half_carry , zero = add8SCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

;

ADC_A_9IX_d0 ( desloc ) =
        PRE desloc : SCHAR THEN
     ANY
         sum , negative , carry , half_carry , zero
     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
        sum , negative , carry , half_carry , zero = add8SCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( bv_9IX_plus_d0 ( mem , ix , desloc ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
           END
   END


;

ADC_A_9IY_d0 ( desloc ) =
        PRE desloc : SCHAR THEN
     ANY
         sum , negative , carry , half_carry , zero
     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
        sum , negative , carry , half_carry , zero = add8SCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( bv_9IY_plus_d0 ( mem , iy , desloc ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
            END
   END
;


SUB_A_r ( rr ) =
  PRE rr : id_reg_8
  THEN
     ANY
         sum , negative , carry , half_carry , zero
     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = substract8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( rgs8 ( rr ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

SUB_A_n ( n1 ) =
  PRE n1 : SCHAR
  THEN
     ANY
         sum , negative , carry , half_carry , zero

     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = substract8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , n1 )
     THEN
       rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
;

SUB_A_9HL0 =
     ANY
         sum , negative , carry , half_carry , zero
     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
        sum , negative , carry , half_carry , zero = substract8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

;

SUB_A_9IX_d0 ( desloc ) =
        PRE desloc : SCHAR THEN
     ANY
         sum , negative , carry , half_carry , zero
     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
        sum , negative , carry , half_carry , zero = substract8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( bv_9IX_plus_d0 ( mem , ix , desloc ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
        END

;

SUB_A_9IY_d0 ( desloc ) =
        PRE desloc : SCHAR THEN
     ANY
         sum , negative , carry , half_carry , zero
     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
        sum , negative , carry , half_carry , zero = substract8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( bv_9IY_plus_d0 ( mem , iy , desloc ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
        END
;


SBC_A_r ( rr ) =
  PRE rr : id_reg_8
  THEN
     ANY
         sum , negative , carry , half_carry , zero

     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = substract8SCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( rgs8 ( rr ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

SBC_A_n ( n1 ) =
  PRE n1 : SCHAR
  THEN
     ANY
         sum , negative , carry , half_carry , zero

     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = substract8SCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , n1 )
     THEN
       rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
;

SBC_A_9HL0 =
     ANY
         sum , negative , carry , half_carry , zero
     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
        sum , negative , carry , half_carry , zero = substract8SCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

;

SBC_A_9IX_d0 ( desloc ) =
        PRE desloc : SCHAR THEN
     ANY
         sum , negative , carry , half_carry , zero
     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
        sum , negative , carry , half_carry , zero = substract8SCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( ( bv_9IX_plus_d0 ( mem , ix , desloc ) ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
         END
   END

;

SBC_A_9IY_d0 ( desloc ) =
        PRE desloc : SCHAR THEN
     ANY
         sum , negative , carry , half_carry , zero
     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
        sum , negative , carry , half_carry , zero = substract8SCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( bv_9IX_plus_d0 ( mem , ix , desloc ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
         END
   END

;


/* AND OR XOR CP */

AND_A_r ( rr ) =
  PRE rr : id_reg_8
  THEN
     ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
     WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT
         & pv : BIT & add_sub : BIT & carry : BIT &
                 result = and ( rgs8 ( a0 ) , rgs8 ( rr ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0


     THEN
       rgs8 := rgs8 <+ { a0 |-> result ,
                          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

 AND_A_n ( n1 ) =
 PRE n1 : SCHAR
  THEN
     ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
     WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT
         & pv : BIT & add_sub : BIT & carry : BIT &
                 result = and ( rgs8 ( a0 ) , SCHAR_TO_BYTE ( n1 ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
     THEN
       rgs8 := rgs8 <+ { a0 |-> result ,
                          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
;

 AND_A_9HL0 =
     ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
     WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
                 result = and ( rgs8 ( a0 ) , mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
     THEN
       rgs8 := rgs8 <+ { a0 |-> result ,
                          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END


;

  AND_A_9IX_d0 ( desloc ) =
          PRE desloc : SCHAR THEN

     ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
     WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
                 result = and ( rgs8 ( a0 ) , bv_9IX_plus_d0 ( mem , ix , desloc ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
     THEN
       rgs8 := rgs8 <+ { a0 |-> result ,
                          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
         END
   END
;

  AND_A_9IY_d0 ( desloc ) =
        PRE desloc : SCHAR THEN

     ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
     WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
                 result = and ( rgs8 ( a0 ) , bv_9IY_plus_d0 ( mem , iy , desloc ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
     THEN
       rgs8 := rgs8 <+ { a0 |-> result ,
                          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
         END
   END
 ;


OR_A_r ( rr ) =
  PRE rr : id_reg_8
  THEN
     ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
     WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT
         & pv : BIT & add_sub : BIT & carry : BIT &
                 result = ior ( rgs8 ( a0 ) , rgs8 ( rr ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0

     THEN
       rgs8 := rgs8 <+ { a0 |-> result ,
                          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

 OR_A_n ( n1 ) =
 PRE n1 : SCHAR
  THEN
     ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
     WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT
         & pv : BIT & add_sub : BIT & carry : BIT &
                 result = ior ( rgs8 ( a0 ) , SCHAR_TO_BYTE ( n1 ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
     THEN
       rgs8 := rgs8 <+ { a0 |-> result ,
                          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

 OR_A_9HL0 =
         ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
     WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
                 result = ior ( rgs8 ( a0 ) , mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
     THEN
       rgs8 := rgs8 <+ { a0 |-> result ,
                          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

 ;

  OR_A_9IX_d0 ( desloc ) =
   PRE desloc : SCHAR THEN

     ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
     WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
                 result = ior ( rgs8 ( a0 ) , bv_9IX_plus_d0 ( mem , ix , desloc ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
     THEN
       rgs8 := rgs8 <+ { a0 |-> result ,
                          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
         END
   END
;

  OR_A_9IY_d0 ( desloc ) =
        PRE desloc : SCHAR THEN

     ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
     WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
                 result = ior ( rgs8 ( a0 ) , bv_9IY_plus_d0 ( mem , iy , desloc ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
     THEN
       rgs8 := rgs8 <+ { a0 |-> result ,
                          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
         END
   END
 ;

XOR_A_r ( rr ) =
  PRE rr : id_reg_8
  THEN
     ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
     WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT
         & pv : BIT & add_sub : BIT & carry : BIT &
                 result = xor ( rgs8 ( a0 ) , rgs8 ( rr ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0

     THEN
       rgs8 := rgs8 <+ { a0 |-> result ,
                          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

 XOR_A_n ( n1 ) =
 PRE n1 : SCHAR
  THEN
     ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
     WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT
         & pv : BIT & add_sub : BIT & carry : BIT &
                 result = xor ( rgs8 ( a0 ) , SCHAR_TO_BYTE ( n1 ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
     THEN
       rgs8 := rgs8 <+ { a0 |-> result ,
                          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

 XOR_A_9HL0 =
         ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
     WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
                 result = xor ( rgs8 ( a0 ) , mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
     THEN
       rgs8 := rgs8 <+ { a0 |-> result ,
                          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

 ;

 XOR_A_9IX_d0 ( desloc ) =
   PRE desloc : SCHAR THEN

     ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
     WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
                 result = xor ( rgs8 ( a0 ) , bv_9IX_plus_d0 ( mem , ix , desloc ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
     THEN
       rgs8 := rgs8 <+ { a0 |-> result ,
                          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
         END
   END
;

  XOR_A_9IY_d0 ( desloc ) =
        PRE desloc : SCHAR THEN

     ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
     WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
                 result = xor ( rgs8 ( a0 ) , bv_9IY_plus_d0 ( mem , iy , desloc ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
     THEN
       rgs8 := rgs8 <+ { a0 |-> result ,
                          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
         END
   END
 ;


  CP_A_r ( rr ) =
  PRE rr : id_reg_8
  THEN
     ANY
         sum , negative , carry , half_carry , zero
     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = substract8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( rgs8 ( rr ) ) )
     THEN
       rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

  CP_A_n ( n1 ) =
  PRE n1 : SCHAR
  THEN
     ANY
         sum , negative , carry , half_carry , zero

     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = substract8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , n1 )
     THEN
       rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
;

  CP_A_9HL0 =
     ANY
         sum , negative , carry , half_carry , zero
     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
        sum , negative , carry , half_carry , zero = substract8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) )
     THEN
       rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

;

  CP_A_9IX_d0 ( desloc ) =
        PRE desloc : SCHAR THEN
     ANY
         sum , negative , carry , half_carry , zero
     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
        sum , negative , carry , half_carry , zero = substract8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( bv_9IX_plus_d0 ( mem , ix , desloc ) ) )
     THEN
       rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
        END

;

  CP_A_9IY_d0 ( desloc ) =
        PRE desloc : SCHAR THEN
     ANY
         sum , negative , carry , half_carry , zero
     WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
        sum , negative , carry , half_carry , zero = substract8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( bv_9IY_plus_d0 ( mem , iy , desloc ) ) )
     THEN
       rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
        END

 /*   INC e DEC  */

;
 INC_r ( rr ) =
        PRE
                rr : id_reg_8
        THEN
         ANY
         sum , negative , carry , half_carry , zero

         WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = add8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( rr ) ) , 1 )
                THEN
                        rgs8 := rgs8 <+ { ( rr |-> SCHAR_TO_BYTE ( sum ) ) ,
                        get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
                        pc := INSTRUCTION_NEXT ( pc )
                END
     END
     ;

 INC_9HL0 =
         ANY
         sum , negative , carry , half_carry , zero

         WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = add8SCHAR ( 0 , BYTE_TO_SCHAR ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) , 1 )

         THEN
                        rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
                        updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , SCHAR_TO_BYTE ( sum ) ) ||
                        pc := INSTRUCTION_NEXT ( pc )
         END

        ;

 INC_9IX_d0 ( desloc ) =
  PRE desloc : SCHAR THEN
        ANY
         sum , negative , carry , half_carry , zero

         WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = add8SCHAR ( 0 , BYTE_TO_SCHAR ( bv_9IX_plus_d0 ( mem , ix , desloc ) ) , 1 )

        THEN
                rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
                updateAddressMem ( bv_IX_plus_d ( ix , desloc ) , SCHAR_TO_BYTE ( sum ) ) ||
                pc := INSTRUCTION_NEXT ( pc )
        END
  END
        ;

 INC_9IY_d0 ( desloc ) =
  PRE desloc : SCHAR THEN
        ANY
         sum , negative , carry , half_carry , zero

         WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = add8SCHAR ( 0 , BYTE_TO_SCHAR ( bv_9IY_plus_d0 ( mem , iy , desloc ) ) , 1 )

        THEN
                rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
                updateAddressMem ( bv_IY_plus_d ( iy , desloc ) , SCHAR_TO_BYTE ( sum ) ) ||
                pc := INSTRUCTION_NEXT ( pc )
        END
  END

/*   DEC   */
;

 DEC_r ( rr ) =
        PRE
                rr : id_reg_8
        THEN
         ANY
         sum , negative , carry , half_carry , zero

         WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = substract8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( rr ) ) , 1 )
                THEN
                        rgs8 := rgs8 <+ { ( rr |-> SCHAR_TO_BYTE ( sum ) ) ,
                        get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , bitget ( rgs8 ( f0 ) , 0 ) ) } ||
                        pc := INSTRUCTION_NEXT ( pc )
                END
     END
     ;

 DEC_9HL0 =
         ANY
         sum , negative , carry , half_carry , zero

         WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = substract8SCHAR ( 0 , BYTE_TO_SCHAR ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) , 1 )

         THEN
                        rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , bitget ( rgs8 ( f0 ) , 0 ) ) } ||
                        updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , SCHAR_TO_BYTE ( sum ) ) ||
                        pc := INSTRUCTION_NEXT ( pc )
         END

        ;

 DEC_9IX_d0 ( desloc ) =
  PRE desloc : SCHAR THEN
        ANY
         sum , negative , carry , half_carry , zero

         WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = substract8SCHAR ( 0 , BYTE_TO_SCHAR ( bv_9IX_plus_d0 ( mem , ix , desloc ) ) , 1 )

        THEN
                rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , bitget ( rgs8 ( f0 ) , 0 ) ) } ||
                updateAddressMem ( bv_IX_plus_d ( ix , desloc ) , SCHAR_TO_BYTE ( sum ) ) ||
                pc := INSTRUCTION_NEXT ( pc )
        END
  END
        ;

 DEC_9IY_d0 ( desloc ) =
  PRE desloc : SCHAR THEN
        ANY
         sum , negative , carry , half_carry , zero

         WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

        sum , negative , carry , half_carry , zero = substract8SCHAR ( 0 , BYTE_TO_SCHAR ( bv_9IY_plus_d0 ( mem , iy , desloc ) ) , 1 )

        THEN
                rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , bitget ( rgs8 ( f0 ) , 0 ) ) } ||
                updateAddressMem ( bv_IY_plus_d ( iy , desloc ) , SCHAR_TO_BYTE ( sum ) ) ||
                pc := INSTRUCTION_NEXT ( pc )
        END
  END ;



  ADD_HL_ss ( ss ) =
 PRE ss : id_reg_16 & not ( ss = AF ) THEN

          ANY result , bv_value , bvh , bvl ,
              negative , carry , half_carry , zero

    WHERE

        result : USHORTINT & bv_value : BYTE  &  bvh : BYTE &   bvl : BYTE &
        negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

                   get_bv_reg16 ( sp , rgs8 , ss ) = bv_value &

        result = add16USHORTINT ( 0 , BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , BV16_TO_USHORTINT ( bv_value ) ) &
                bvh , bvl = BV16_TO_BYTE ( USHORTINT_TO_BV16 ( result ) ) &
        negative = bitget ( rgs8 ( f0 ) , 7 ) &
        zero = bitget ( rgs8 ( f0 ) , 6 ) &
        half_carry = add_halfcarryUSHORTINT ( 0 , BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , BV16_TO_USHORTINT ( bv_value ) ) &
        carry =  add_carryUSHORTINT ( 0 , BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , BV16_TO_USHORTINT ( bv_value ) )

     THEN
                rgs8 := rgs8 <+ { h0 |-> bvh , l0 |-> bvl ,
                     get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , bitget ( rgs8 ( f0 ) , 2 ) , 0 , carry ) } ||

                pc := INSTRUCTION_NEXT ( pc )
     END
 END ;


 ADC_HL_ss ( ss ) =
  PRE ss : id_reg_16 & not ( ss = AF ) THEN

          ANY result , bv_value , bvh , bvl ,
              negative , carry , half_carry , zero

    WHERE
        result : USHORTINT & bv_value : BYTE  &  bvh : BYTE &   bvl : BYTE &
        negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

                   get_bv_reg16 ( sp , rgs8 , ss ) = bv_value &

        result = add16USHORTINT ( bitget ( rgs8 ( f0 ) , 0 ) , BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , BV16_TO_USHORTINT ( bv_value ) ) &
        negative = 1 &
        zero = is_zeroUSHORTINT ( result ) &
        half_carry = add_halfcarryUSHORTINT ( bitget ( rgs8 ( f0 ) , 0 ) , BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , BV16_TO_USHORTINT ( bv_value ) ) &
        carry =  add_carryUSHORTINT ( bitget ( rgs8 ( f0 ) , 0 ) , BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , BV16_TO_USHORTINT ( bv_value ) )

     THEN
                rgs8 := rgs8 <+ { h0 |-> bvh , l0 |-> bvl ,
                get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||

                pc := INSTRUCTION_NEXT ( pc )
     END
  END
;




 SBC_HL_ss ( ss ) =
  PRE ss : id_reg_16 & not ( ss = AF ) THEN

          ANY result , bv_value , bvh , bvl ,
              negative , carry , half_carry , zero

    WHERE
        result : USHORTINT & bv_value : BYTE  &  bvh : BYTE &   bvl : BYTE &
        negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

                   get_bv_reg16 ( sp , rgs8 , ss ) = bv_value &

        result = add16USHORTINT ( bitget ( rgs8 ( f0 ) , 0 ) , BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , BV16_TO_USHORTINT ( bv_value ) ) &
        negative = 1 &
        zero = is_zeroUSHORTINT ( result ) &
        half_carry = sub_halfcarryUSHORTINT ( bitget ( rgs8 ( f0 ) , 0 ) , BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , BV16_TO_USHORTINT ( bv_value ) ) &
        carry =  sub_carryUSHORTINT ( bitget ( rgs8 ( f0 ) , 0 ) , BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , BV16_TO_USHORTINT ( bv_value ) )

     THEN
                rgs8 := rgs8 <+ { h0 |-> bvh , l0 |-> bvl ,
                get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||

                pc := INSTRUCTION_NEXT ( pc )
     END
  END
;

   ADD_IX_ss ( ss ) =
 PRE ss : id_reg_16 & not ( ss = AF ) THEN

          ANY result , bv_value , bvh , bvl ,
              negative , carry , half_carry , zero

    WHERE

        result : USHORTINT & bv_value : BYTE  &  bvh : BYTE &   bvl : BYTE &
        negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

                   get_bv_reg16 ( sp , rgs8 , ss ) = bv_value &

        result = add16USHORTINT ( 0 , BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , BV16_TO_USHORTINT ( bv_value ) ) &
                bvh , bvl = BV16_TO_BYTE ( USHORTINT_TO_BV16 ( result ) ) &
        negative = bitget ( rgs8 ( f0 ) , 7 ) &
        zero = bitget ( rgs8 ( f0 ) , 6 ) &
        half_carry = add_halfcarryUSHORTINT ( 0 , BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , BV16_TO_USHORTINT ( bv_value ) ) &
        carry =  add_carryUSHORTINT ( 0 , BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , BV16_TO_USHORTINT ( bv_value ) )

     THEN
                rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , bitget ( rgs8 ( f0 ) , 2 ) , 0 , carry ) } ||
                ix :=  USHORTINT_TO_BV16 ( result ) ||
                pc := INSTRUCTION_NEXT ( pc )
     END
 END
;

   ADD_IY_ss ( ss ) =
 PRE ss : id_reg_16 & not ( ss = AF ) THEN

          ANY result , bv_value , bvh , bvl ,
              negative , carry , half_carry , zero

    WHERE

        result : USHORTINT & bv_value : BYTE  &  bvh : BYTE &   bvl : BYTE &
        negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

                   get_bv_reg16 ( sp , rgs8 , ss ) = bv_value &

        result = add16USHORTINT ( 0 , BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , BV16_TO_USHORTINT ( bv_value ) ) &
                bvh , bvl = BV16_TO_BYTE ( USHORTINT_TO_BV16 ( result ) ) &
        negative = bitget ( rgs8 ( f0 ) , 7 ) &
        zero = bitget ( rgs8 ( f0 ) , 6 ) &
        half_carry = add_halfcarryUSHORTINT ( 0 , BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , BV16_TO_USHORTINT ( bv_value ) ) &
        carry =  add_carryUSHORTINT ( 0 , BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , BV16_TO_USHORTINT ( bv_value ) )

     THEN
                rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , bitget ( rgs8 ( f0 ) , 2 ) , 0 , carry ) } ||
                iy :=  USHORTINT_TO_BV16 ( result ) ||
                pc := INSTRUCTION_NEXT ( pc )
     END
 END
;

  INC_ss ( ss ) =
 PRE ss : id_reg_16 & not ( ss = AF ) THEN

    IF ss = SP THEN sp := inc_BV16 ( sp )
    ELSE
        ANY
            rh , rl , vh , vl
        WHERE rh : id_reg_8 & rl : id_reg_8 & vh : BYTE &  vl : BYTE &
                    REG16_TO_REG8 ( ss ) = rh , rl &
                    BV16_TO_BYTE ( inc_BV16 ( BYTE_TO_BV16 ( rgs8 ( rh ) , rgs8 ( rl ) ) ) ) = vh , vl
        THEN
                rgs8 := rgs8 <+ { rh |-> vh , rl |-> vl }
                END
        END
        ||                pc := INSTRUCTION_NEXT ( pc )

  END
;

  INC_IX =
 BEGIN
     ix := inc_BV16 ( ix ) ||
         pc := INSTRUCTION_NEXT ( pc )
 END
 ;

 INC_IY =
 BEGIN
     iy := inc_BV16 ( iy ) ||
         pc := INSTRUCTION_NEXT ( pc )
 END
 ;

 DEC_ss ( ss ) =
 PRE ss : id_reg_16 & not ( ss = AF ) THEN

    IF ss = SP THEN sp := inc_BV16 ( sp )
    ELSE
        ANY
            rh , rl , vh , vl
        WHERE rh : id_reg_8 & rl : id_reg_8 & vh : BYTE &  vl : BYTE &
                    REG16_TO_REG8 ( ss ) = rh , rl &
                    BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( rh ) , rgs8 ( rl ) ) ) ) = vh , vl
        THEN
                rgs8 := rgs8 <+ { rh |-> vh , rl |-> vl }     ||                pc := INSTRUCTION_NEXT ( pc )
                END
        END

  END
;

  DEC_IX =
 BEGIN
     ix := dec_BV16 ( ix )     ||                pc := INSTRUCTION_NEXT ( pc )
 END
 ;

  DEC_IY =
 BEGIN
     ix := dec_BV16 ( ix )     ||                pc := INSTRUCTION_NEXT ( pc )
 END




END
