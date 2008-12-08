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

 LD_r_r_ ( rr , rr_ ) =
     PRE rr : id_reg_8 & rr_ : id_reg_8  THEN
                rgs8 ( rr ) := rgs8 ( rr_ ) ||
        pc := INSTRUCTION_NEXT ( pc )
     END
;

 LD_r_n_ ( rr , n0 ) =
     PRE rr : id_reg_8 & n0 : SCHAR  THEN
        rgs8 ( rr ) := SCHAR_TO_BYTE ( n0 ) ||
        pc := INSTRUCTION_NEXT ( pc )
     END
;

 LD_r_9HL0 ( rr ) =
    PRE rr : id_reg_8 THEN
         ANY address WHERE address : BV16 &
            address =  BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) )
         THEN
                        rgs8 ( rr ) := mem ( address ) ||
                        pc := INSTRUCTION_NEXT ( pc )
                END
        END
;


 LD_r_9IX_d0 ( rr , desloc ) =
      PRE rr : id_reg_8 & desloc : SCHAR
      THEN
          rgs8 ( rr ) := bv_9IX_plus_d0 ( mem , ix , desloc ) ||
          pc := INSTRUCTION_NEXT ( pc )
      END
;
 LD_r_9IY_d0 ( rr , desloc ) =
      PRE rr : id_reg_8 & desloc : SCHAR
      THEN
          rgs8 ( rr ) := bv_9IY_plus_d0 ( mem , iy , desloc ) ||
          pc := INSTRUCTION_NEXT ( pc )
      END
;
 LD_9HL0_r ( rr ) =
      PRE rr : id_reg_8 & BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) /: dom ( stack ) THEN
          updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , rgs8 ( rr ) ) ||
          pc := INSTRUCTION_NEXT ( pc )
      END
;


 LD_9IX_d0_r ( desloc , rr ) =
      PRE  desloc : SCHAR & rr : id_reg_8 & bv_IX_plus_d ( ix , desloc ) /: dom ( stack ) THEN
                    updateAddressMem ( bv_IX_plus_d ( ix , desloc ) , rgs8 ( rr ) ) ||
          pc := INSTRUCTION_NEXT ( pc )
      END
;

 LD_9IY_d0_r ( desloc , rr ) =
      PRE  desloc : SCHAR & rr : id_reg_8 & bv_IY_plus_d ( iy , desloc ) /: dom ( stack ) THEN
                    updateAddressMem ( bv_IY_plus_d ( iy , desloc ) , rgs8 ( rr ) ) ||
          pc := INSTRUCTION_NEXT ( pc )
      END
;

 LD_9HL0_n ( n0 ) =
      PRE n0 : SCHAR & BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) /: dom ( stack ) THEN
                    updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , SCHAR_TO_BYTE ( n0 ) ) ||
          pc := INSTRUCTION_NEXT ( pc )
      END
;

 LD_9IX_d0_n ( desloc , n0 ) =
      PRE  desloc : SCHAR & n0 : SCHAR &  bv_IX_plus_d ( ix , desloc ) /: dom ( stack ) THEN
                    updateAddressMem ( bv_IX_plus_d ( ix , desloc ) , SCHAR_TO_BYTE ( n0 ) ) ||
          pc := INSTRUCTION_NEXT ( pc )
      END
;

 LD_9IY_d0_n ( desloc , n0 ) =
      PRE  desloc : SCHAR & n0 : SCHAR & bv_IY_plus_d ( iy , desloc ) /: dom ( stack ) THEN
                          updateAddressMem ( bv_IY_plus_d ( iy , desloc ) , SCHAR_TO_BYTE ( n0 ) ) ||
          pc := INSTRUCTION_NEXT ( pc )
      END
;

 LD_A_9BC0  =
         BEGIN
                rgs8 ( a0 ) := mem ( mem ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) ) ||
        pc := INSTRUCTION_NEXT ( pc )
        END
;

 LD_A_9DE0  =
         BEGIN
         rgs8 ( a0 ) := mem ( BYTE_TO_BV16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ) ||
         pc := INSTRUCTION_NEXT ( pc )
      END
;

 LD_A_9nn0 ( nn ) =
      PRE nn :  USHORTINT
      THEN
                rgs8 ( a0 ) := mem ( USHORTINT_TO_BV16 ( nn ) ) ||
                pc := INSTRUCTION_NEXT ( pc )
      END
;

 LD_9BC0_A  =
      PRE           BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) /: dom ( stack )
      THEN
          updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) , rgs8 ( a0 ) ) ||
          pc := INSTRUCTION_NEXT ( pc )
      END
;


 LD_9DE0_A  =
      PRE mem ( BYTE_TO_BV16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ) /: dom ( stack )
      THEN
          updateAddressMem ( ( BYTE_TO_BV16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ) , rgs8 ( a0 ) ) ||
          pc := INSTRUCTION_NEXT ( pc )
      END
;

 LD_9nn0_A ( nn ) =
      PRE nn : USHORTINT
        & USHORTINT_TO_BV16 ( nn ) /: dom ( stack )
      THEN
           updateAddressMem ( USHORTINT_TO_BV16 ( nn ) , rgs8 ( a0 ) ) ||
           pc := INSTRUCTION_NEXT ( pc )
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


 LD_dd_nn ( dd , nn ) =
      PRE dd : id_reg_16 & nn : USHORTINT  & dd /= AF
      THEN
        IF dd = SP THEN  sp := USHORTINT_TO_BV16 ( nn )
        ELSE
           ANY rh , rl , w1 , w2 WHERE
               rh : id_reg_8 &  rl : id_reg_8 &
               w1 : BYTE & w2 : BYTE &
               rh , rl = REG16_TO_REG8 ( dd ) &
               BV16_TO_BYTE ( USHORTINT_TO_BV16 ( nn ) ) = w1 , w2
           THEN
                    rgs8 := rgs8 <+ { rh |-> w1 , rl |-> w2 } ||
            pc := INSTRUCTION_NEXT ( pc )
           END
        END
     END
;

 LD_IX_nn ( nn ) =
      PRE   nn : USHORTINT
      THEN
                ix := USHORTINT_TO_BV16 ( nn ) ||
                pc := INSTRUCTION_NEXT ( pc )
      END
;
 LD_IY_nn ( nn ) =
      PRE   nn : USHORTINT
      THEN
                iy := USHORTINT_TO_BV16 ( nn ) ||
                pc := INSTRUCTION_NEXT ( pc )
      END
;

 LD_HL_9nn0 ( nn ) =
      PRE   nn : USHORTINT
      THEN
                    rgs8 := rgs8 <+ { h0 |-> mem ( USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , nn , 1 ) ) ) , l0 |-> mem ( USHORTINT_TO_BV16 ( nn ) ) } ||
          pc := INSTRUCTION_NEXT ( pc )
      END
;

 LD_dd_9nn0 ( dd , nn ) =
      PRE dd : id_reg_16 & nn : USHORTINT  & dd /= AF
      THEN
        IF dd = SP THEN
        sp := BYTE_TO_BV16 ( mem ( USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , nn , 1 ) ) ) , mem ( USHORTINT_TO_BV16 ( nn ) ) )
        ELSE
           ANY rh , rl , w1 , w2 WHERE
               rh : id_reg_8 &  rl : id_reg_8 &
               w1 : BYTE & w2 : BYTE &
               rh , rl = REG16_TO_REG8 ( dd ) &
               w1 = mem ( USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , nn , 1 ) ) ) &
               w2 = mem ( USHORTINT_TO_BV16 ( nn ) )
           THEN
            rgs8 := rgs8 <+ { rh |-> w1 , rl |-> w2 } ||
            pc := INSTRUCTION_NEXT ( pc )
           END
        END
     END
;



 LD_IX_9nn0 ( nn ) =
      PRE   nn : USHORTINT
      THEN
                    ix := BYTE_TO_BV16 ( mem ( USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , nn , 1 ) ) ) , mem ( USHORTINT_TO_BV16 ( nn ) ) ) ||
          pc := INSTRUCTION_NEXT ( pc )

      END
;

 LD_IY_9nn0 ( nn ) =
      PRE   nn : USHORTINT
      THEN
          iy := BYTE_TO_BV16 ( mem ( USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , nn , 1 ) ) ) , mem ( USHORTINT_TO_BV16 ( nn ) ) ) ||
          pc := INSTRUCTION_NEXT ( pc )
      END
;

 LD_9nn0_HL ( nn ) =
      PRE   nn : USHORTINT &
            USHORTINT_TO_BV16 ( nn ) /: dom ( stack ) & USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , nn , 1 ) ) /: dom ( stack )
      THEN
                  updateMem ( { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , nn , 1 ) ) |-> rgs8 ( h0 ) , USHORTINT_TO_BV16 ( nn ) |-> rgs8 ( l0 ) } ) ||
          pc := INSTRUCTION_NEXT ( pc )
      END
;



 LD_9nn0_dd ( nn , dd ) =
      PRE dd : id_reg_16 &  dd /= AF & nn : USHORTINT  & /* USHORTINT_TO_BV16( add16USHORTINT(0,nn,1) ) : USHORTINT  &*/
          SCHAR_TO_BYTE ( nn ) /: dom ( stack )
      THEN
        IF dd = SP
        THEN
           ANY vh , vl WHERE
                vh : BYTE & vl : BYTE &
                                BV16_TO_BYTE ( sp ) = vh , vl &
                { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , nn , 1 ) ) |-> vh , USHORTINT_TO_BV16 ( nn ) |-> vl } : BV16 >-> BYTE
           THEN
                              updateMem ( { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , nn , 1 ) ) |-> vh , USHORTINT_TO_BV16 ( nn ) |-> vl } )
           END
        ELSE
           ANY rh , rl , w1 , w2 WHERE
               rh : id_reg_8 &  rl : id_reg_8 &
               w1 : SCHAR & w2 : BYTE &
               rh , rl = REG16_TO_REG8 ( dd ) &
               { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , nn , 1 ) ) |-> rgs8 ( rh ) , USHORTINT_TO_BV16 ( nn ) |-> rgs8 ( rl ) } : BV16 >-> BYTE
           THEN
               updateMem ( { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , nn , 1 ) ) |-> rgs8 ( rh ) , USHORTINT_TO_BV16 ( nn ) |-> rgs8 ( rl ) } ) ||
               pc := INSTRUCTION_NEXT ( pc )
           END
        END
     END
;


 LD_9nn0_IX ( nn ) =
      PRE   nn : USHORTINT                          THEN
        ANY h_ix , l_ix WHERE
            h_ix : BYTE & l_ix : BYTE &
            h_ix , l_ix = BV16_TO_BYTE ( ix )
           & USHORTINT_TO_BV16 ( nn ) /: dom ( stack ) & USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , nn , 1 ) ) /: dom ( stack )
        THEN
                  updateMem ( { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , nn , 1 ) ) |-> h_ix , USHORTINT_TO_BV16 ( nn ) |-> l_ix } ) ||
          pc := INSTRUCTION_NEXT ( pc )
        END
      END
;

 LD_9nn0_IY ( nn ) =
      PRE   nn : USHORTINT   THEN
        ANY h_iy , l_iy WHERE
            h_iy : BYTE & l_iy : BYTE &
            h_iy , l_iy = BV16_TO_BYTE ( iy ) &
            USHORTINT_TO_BV16 ( nn ) /: dom ( stack ) & USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , nn , 1 ) ) /: dom ( stack )
        THEN
                          updateMem ( { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , nn , 1 ) ) |-> h_iy , USHORTINT_TO_BV16 ( nn ) |-> l_iy } ) ||
          pc := INSTRUCTION_NEXT ( pc )
        END
      END
;

 LD_SP_HL  =
      BEGIN
                sp := BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ||
                pc := INSTRUCTION_NEXT ( pc )
      END

;

 LD_SP_IX  =
      BEGIN
                sp := ix ||
                pc := INSTRUCTION_NEXT ( pc )
      END

;

 LD_SP_IY  =
      BEGIN
                sp := iy ||
                pc := INSTRUCTION_NEXT ( pc )
      END

;


 PUSH_qq ( qq ) =
      PRE  qq : id_reg_16 & qq /= SP
      THEN
        ANY
            qqh , qql
        WHERE qqh : id_reg_8 & qql : id_reg_8 &
              REG16_TO_REG8 ( qq ) = qqh , qql &
              { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 2 ) ) |-> rgs8 ( qql ) ,
                                USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 1 ) ) |-> rgs8 ( qqh ) } : BV16 --> BYTE
        THEN
             updateStack ( { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 2 ) ) |-> rgs8 ( qql ) ,
                             USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 1 ) ) |-> rgs8 ( qqh ) } ) ||
             sp := USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 2 ) ) ||
             pc := INSTRUCTION_NEXT ( pc )
        END
      END
;

 PUSH_IX  =

       ANY
           wh , wl
       WHERE wh : BYTE & wl : BYTE &
             BV16_TO_BYTE ( ix ) = wh , wl &
             { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 2 ) ) |-> wl , USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 1 ) ) |-> wh } : BV16 --> BYTE
       THEN
             updateStack ( { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 2 ) ) |-> wl ,
                                                         USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 1 ) ) |-> wh } ) ||
             sp := USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 2 ) ) ||
             pc := INSTRUCTION_NEXT ( pc )
       END
/*     END*/
;

 PUSH_IY  =
       ANY
           wh , wl
       WHERE wh : BYTE & wl : BYTE &
             BV16_TO_BYTE ( iy ) = wh , wl &
             { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 2 ) ) |-> wl , USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 1 ) ) |-> wh } : BV16 --> BYTE
       THEN
             updateStack ( { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 2 ) ) |-> wl ,
                                                        USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 1 ) ) |-> wh } ) ||
             sp := USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , - 2 ) ) ||
             pc := INSTRUCTION_NEXT ( pc )
       END
;

 POP_qq ( qq ) =
      PRE  qq : id_reg_16 & qq /= SP
      THEN
        ANY
            qqh , qql
        WHERE qqh : id_reg_8 & qql : id_reg_8 &
              REG16_TO_REG8 ( qq ) = qqh , qql &
              { qql |-> mem ( USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 2 ) ) ) , qqh |-> mem ( USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 1 ) ) ) } : id_reg_8 --> BYTE
        THEN
              rgs8 := rgs8 <+ { qql |-> mem ( USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 2 ) ) ) , qqh |-> mem ( USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 1 ) ) ) } ||
              sp := USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 2 ) ) ||
              pc := INSTRUCTION_NEXT ( pc )
        END
      END
;

 POP_IX  =
      PRE   sp : BV16
      THEN
       ANY
           nw8
       WHERE nw8 : BYTE  &
             BYTE_TO_BV16 ( mem ( USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 1 ) ) ) , mem ( USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 2 ) ) ) ) = nw8
       THEN
             ix := nw8 ||
             sp := USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 2 ) ) ||
             pc := INSTRUCTION_NEXT ( pc )
       END
     END
;


 POP_IY  =
      PRE   sp : BV16
      THEN
       ANY
           nw8
       WHERE nw8 : BYTE  &
             BYTE_TO_BV16 ( mem ( USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 1 ) ) ) , mem ( USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 2 ) ) ) ) = nw8
       THEN
             iy := nw8 ||
             sp := USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 2 ) ) ||
             pc := INSTRUCTION_NEXT ( pc )
       END
     END
;

 EX_DE_HL =

      BEGIN
       rgs8 := rgs8 <+ { d0 |-> rgs8 ( h0 ) , e0 |-> rgs8 ( l0 ) , h0 |-> rgs8 ( d0 ) , l0 |-> rgs8 ( e0 ) } ||
       pc := INSTRUCTION_NEXT ( pc )
      END
;


 EX_AF_AF_ =

      BEGIN
       rgs8 := rgs8 <+ { a0 |-> rgs8 ( a_0 ) , f0 |-> rgs8 ( f_0 ) , a_0 |-> rgs8 ( a0 ) , f_0 |-> rgs8 ( f0 ) } ||
       pc := INSTRUCTION_NEXT ( pc )
      END
;


 EXX =
      BEGIN
       rgs8 := rgs8 <+ { b0 |-> rgs8 ( b_0 ) , c0 |-> rgs8 ( c_0 ) , d0 |-> rgs8 ( d_0 ) , e0 |-> rgs8 ( e_0 ) , h0 |-> rgs8 ( h_0 ) , l0 |-> rgs8 ( l_0 ) ,
                        b_0 |-> rgs8 ( b0 ) , c_0 |-> rgs8 ( c0 ) , d_0 |-> rgs8 ( d0 ) , e_0 |-> rgs8 ( e0 ) , h_0 |-> rgs8 ( h0 ) , l_0 |-> rgs8 ( l0 ) } ||
       pc := INSTRUCTION_NEXT ( pc )
      END
;

 EX_9SP0_HL =
      PRE
          stack <+ { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 1 ) ) |-> rgs8 ( h0 ) , sp |-> rgs8 ( l0 ) } <: mem
      THEN
       rgs8 := rgs8 <+ { h0 |-> stack ( USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 1 ) ) ) , l0 |-> stack ( sp ) } ||
       updateStack ( { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 1 ) ) |-> rgs8 ( h0 ) , sp |-> rgs8 ( l0 ) } ) ||
       pc := INSTRUCTION_NEXT ( pc )
      END
;

 EX_9SP0_IX =
      ANY wh , wl
      WHERE wh : BYTE & wl : BYTE &
            BV16_TO_BYTE ( ix ) = wh , wl &
            { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 1 ) ) |-> wh , sp |-> wl } : BV16 --> BYTE &
            stack <+ { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 1 ) ) |-> wh , sp |-> wl } <: mem
      THEN
       ix := BYTE_TO_BV16 ( stack ( USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 1 ) ) ) , stack ( sp ) ) ||
       updateStack ( { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 1 ) ) |-> wh , sp |-> wl } ) ||
       pc := INSTRUCTION_NEXT ( pc )
      END
 ;

 EX_9SP0_IY =
      ANY wh , wl
      WHERE wh : BYTE & wl : BYTE &
            BV16_TO_BYTE ( iy ) = wh , wl &
            { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 1 ) ) |-> wh , sp |-> wl } : BV16 --> BYTE &
            stack <+ { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 1 ) ) |-> wh , sp |-> wl } <: mem
      THEN
       iy := BYTE_TO_BV16 ( stack ( USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 1 ) ) ) , stack ( sp ) ) ||
       updateStack ( { USHORTINT_TO_BV16 ( add16USHORTINT ( 0 , BV16_TO_USHORTINT ( sp ) , 1 ) ) |-> wh , sp |-> wl } ) ||
       pc := INSTRUCTION_NEXT ( pc )
      END
 ;


LDI =
    ANY  hvn , lvn , dvn , evn , bvn , cvn
    WHERE
          hvn , lvn = BV16_TO_BYTE ( inc_BV16 ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
          dvn , evn = BV16_TO_BYTE ( inc_BV16 ( BYTE_TO_BV16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ) ) &
          bvn , cvn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) )

    THEN
          updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) , mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) ||
          rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) ,
                           ( d0 |-> dvn ) , ( e0 |-> evn ) ,
                           ( b0 |-> bvn ) , ( c0 |-> cvn ) ,
                           get_new_flag_register_SZ_H_PvNC ( rgs8 , bitget ( rgs8 ( f0 ) , 7 ) , bitget ( rgs8 ( f0 ) , 6 ) , 0 , bit_not ( is_zeroUSHORTINT ( BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) - 1 ) ) , 0 , bitget ( rgs8 ( f0 ) , 0 ) ) } ||
          pc := INSTRUCTION_NEXT ( pc )

    END

;

 LDIR =
    ANY  hvn , lvn , dvn , evn , bvn , cvn
    WHERE
          hvn , lvn = BV16_TO_BYTE ( inc_BV16 ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
          dvn , evn = BV16_TO_BYTE ( inc_BV16 ( BYTE_TO_BV16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ) ) &
          bvn , cvn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) )

    THEN
          updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) , mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) ||
          rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) ,
                           ( d0 |-> dvn ) , ( e0 |-> evn ) ,
                           ( b0 |-> bvn ) , ( c0 |-> cvn ) ,
                           get_new_flag_register_SZ_H_PvNC ( rgs8 , bitget ( rgs8 ( f0 ) , 7 ) , bitget ( rgs8 ( f0 ) , 6 ) , 0 , 0 , 0 , bitget ( rgs8 ( f0 ) , 0 ) ) } ||
           IF  is_zeroUSHORTINT ( BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) - 1 ) = 0 THEN pc := INSTRUCTION_NEXT ( pc ) END

    END


;

LDD =

        ANY  hvn , lvn , dvn , evn , bvn , cvn
    WHERE
          hvn , lvn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
          dvn , evn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ) ) &
          bvn , cvn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) )

    THEN
          updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) , mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) ||
          rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) ,
                           ( d0 |-> dvn ) , ( e0 |-> evn ) ,
                           ( b0 |-> bvn ) , ( c0 |-> cvn ) ,
                           get_new_flag_register_SZ_H_PvNC ( rgs8 , bitget ( rgs8 ( f0 ) , 7 ) , bitget ( rgs8 ( f0 ) , 6 ) , 0 , bit_not ( is_zeroUSHORTINT ( BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) - 1 ) ) , 0 , bitget ( rgs8 ( f0 ) , 0 ) ) } ||
           pc := INSTRUCTION_NEXT ( pc )

    END


;


LDDR =
        ANY  hvn , lvn , dvn , evn , bvn , cvn
    WHERE
          hvn , lvn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
          dvn , evn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ) ) &
          bvn , cvn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) )

    THEN
          updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) , mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) ||
          rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) ,
                           ( d0 |-> dvn ) , ( e0 |-> evn ) ,
                           ( b0 |-> bvn ) , ( c0 |-> cvn ) ,
                           get_new_flag_register_SZ_H_PvNC ( rgs8 , bitget ( rgs8 ( f0 ) , 7 ) , bitget ( rgs8 ( f0 ) , 6 ) , 0 , 0 , 0 , bitget ( rgs8 ( f0 ) , 0 ) ) } ||
           IF  is_zeroUSHORTINT ( BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) - 1 ) = 0 THEN pc := INSTRUCTION_NEXT ( pc ) END

    END

;


CPI =
        ANY
            sum , negative , carry , half_carry , zero ,
            hvn , lvn , bvn , cvn
        WHERE
            sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
            substract8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) ) = sum , negative , carry , half_carry , zero &
            hvn , lvn = BV16_TO_BYTE ( inc_BV16 ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
            bvn , cvn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) )

        THEN
            IF zero = 1 THEN
                        rgs8 := { h0 |-> hvn , l0 |-> lvn , b0 |-> bvn , c0 |-> cvn
                                     , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry
                                         , bit_not ( is_zeroUSHORTINT ( BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) - 1 ) ) , 1 , bitget ( rgs8 ( f0 ) , 0 ) ) }

                END
                ||
          pc := INSTRUCTION_NEXT ( pc )
        END
;

CPIR =
        ANY
            sum , negative , carry , half_carry , zero ,
            hvn , lvn , bvn , cvn
        WHERE
            sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
            substract8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) ) = sum , negative , carry , half_carry , zero &
            hvn , lvn = BV16_TO_BYTE ( inc_BV16 ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
            bvn , cvn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) )

        THEN
            IF zero = 1 THEN
                        rgs8 := { h0 |-> hvn , l0 |-> lvn , b0 |-> bvn , c0 |-> cvn
                                     , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry
                                         , bit_not ( is_zeroUSHORTINT ( BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) - 1 ) ) , 1 , bitget ( rgs8 ( f0 ) , 0 ) ) }

                END
                ||
         IF  is_zeroUSHORTINT ( BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) - 1 ) = 0 or ( zero = 1 ) THEN pc := INSTRUCTION_NEXT ( pc ) END
        END
;

CPD =
        ANY
            sum , negative , carry , half_carry , zero ,
            hvn , lvn , bvn , cvn
        WHERE
            sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
            substract8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) ) = sum , negative , carry , half_carry , zero &
            hvn , lvn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
            bvn , cvn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) )

        THEN
            IF zero = 1 THEN
                        rgs8 := { h0 |-> hvn , l0 |-> lvn , b0 |-> bvn , c0 |-> cvn
                                     , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry
                                         , bit_not ( is_zeroUSHORTINT ( BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) - 1 ) ) , 1 , bitget ( rgs8 ( f0 ) , 0 ) ) }

                END
                ||
          pc := INSTRUCTION_NEXT ( pc )
        END
;

CPDR =
        ANY
            sum , negative , carry , half_carry , zero ,
            hvn , lvn , bvn , cvn
        WHERE
            sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
            substract8SCHAR ( 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) , BYTE_TO_SCHAR ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) ) = sum , negative , carry , half_carry , zero &
            hvn , lvn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
            bvn , cvn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) )

        THEN
            IF zero = 1 THEN
                        rgs8 := { h0 |-> hvn , l0 |-> lvn , b0 |-> bvn , c0 |-> cvn
                                     , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry
                                         , bit_not ( is_zeroUSHORTINT ( BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) - 1 ) ) , 1 , bitget ( rgs8 ( f0 ) , 0 ) ) }

                END
                || IF  is_zeroUSHORTINT ( BV16_TO_USHORTINT ( BYTE_TO_BV16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) - 1 ) = 0 or ( zero = 1 ) THEN pc := INSTRUCTION_NEXT ( pc ) END
        END
;

/************************************************************************************** 
****** Arithmetic and Logic Intructions are in the module Z80LoAri - 8 and 16 bits
***************************************************************************************/



/************************************************************************************** 
****** General Purpose Arithmetic and CPU control 
***************************************************************************************/

 DAA = /* Not implemented yet */
 BEGIN
   rgs8 ( a0 ) :=  rgs8 ( a0 ) ||
   pc := INSTRUCTION_NEXT ( pc )
 END
 ;

 CPL =
 ANY  result
 WHERE result : BYTE  & result = complement ( rgs8 ( a0 ) )
 THEN
     rgs8 :=   rgs8 <+ {  ( a0 |-> result ) ,
                           get_new_flag_register_SZ_H_PvNC ( rgs8 , bitget ( rgs8 ( f0 ) , 7 ) , bitget ( rgs8 ( f0 ) , 6 ) , 1 , bitget ( rgs8 ( f0 ) , 2 ) , 1 , bitget ( rgs8 ( f0 ) , 0 ) ) } ||
     pc := INSTRUCTION_NEXT ( pc )

 END ;

 NEG =
 ANY
     sum , negative , carry , half_carry , zero
 WHERE  sum : SCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
     sum , negative , carry , half_carry , zero = substract8SCHAR ( 0 , 0 , BYTE_TO_SCHAR ( rgs8 ( a0 ) ) )
 THEN
    rgs8 := rgs8 <+ { a0 |-> SCHAR_TO_BYTE ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
    pc := INSTRUCTION_NEXT ( pc )
 END ;

 CCF =
 BEGIN
  rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , bitget ( rgs8 ( f0 ) , 7 ) , bitget ( rgs8 ( f0 ) , 6 ) , bitget ( rgs8 ( f0 ) , 4 ) , bitget ( rgs8 ( f0 ) , 2 ) , 0 , bit_not ( bitget ( rgs8 ( f0 ) , 0 ) ) ) } ||
  pc := INSTRUCTION_NEXT ( pc )
 END ;

 SCF =
 BEGIN
  rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , bitget ( rgs8 ( f0 ) , 7 ) , bitget ( rgs8 ( f0 ) , 6 ) , bitget ( rgs8 ( f0 ) , 4 ) , bitget ( rgs8 ( f0 ) , 2 ) , 0 , 1 ) } ||
  pc := INSTRUCTION_NEXT ( pc )
 END ;

 NOP =
 BEGIN
  pc := INSTRUCTION_NEXT ( pc )
 END ;

 HALT =
 BEGIN
  /* Not implemented yet */
  pc := INSTRUCTION_NEXT ( pc )
 END  ;

/*

DI=
BEGIN
END;


EI=
BEGIN
END;

IM0=
BEGIN
END;

IM1=
BEGIN
END;

IM2=
BEGIN
END;


*/


/*

Rotate and SHIFT GROUP


*/

/*

 IN_A_9n0_(nn,k_in)= COMENT** Informando algo sobre o valor de entrada, pode passar uma constante definida em properties**COMENT_CLOSE
    PRE nn : UCHAR & k_in : SCHAR   THEN
     ANY data_in WHERE data_in : SCHAR THEN
	  i_o_ports (nn) := data_in ||
          rgs8(a0) :=   k_in ||
          pc := INSTRUCTION_NEXT(pc)
     END
    END*/


/************************************************************************************** 
****** Input an Output Group
***************************************************************************************/

 IN_A_9n0 ( nn ) =
    PRE nn : UCHAR    THEN
     ANY data_in WHERE data_in : SCHAR THEN
                    i_o_ports ( UCHAR_TO_BYTE ( nn ) ) := SCHAR_TO_BYTE ( data_in ) ||
          rgs8 ( a0 ) := SCHAR_TO_BYTE ( data_in ) ||
          pc := INSTRUCTION_NEXT ( pc )
     END
    END
;
 IN_r_9C0 ( rr ) =
    PRE rr : id_reg_8    THEN
     ANY data_in ,
                  negative , zero , half_carry , pv , add_sub , carry
         WHERE data_in : BYTE &
             negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
                 negative = is_negative ( data_in ) &
         zero = is_zero ( data_in ) &
         half_carry = 0 &
         pv = parity_even_BYTE ( data_in )   &
         add_sub =  0 &
         carry = bitget ( rgs8 ( f0 ) , 0 )

         THEN
          i_o_ports ( rgs8 ( c0 ) ) := data_in ||
          rgs8 := rgs8 <+ { ( rr |-> data_in ) ,
          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
          pc := INSTRUCTION_NEXT ( pc )
     END
    END
;

 INI =
   ANY data_in , hvn , lvn ,
                negative , zero , half_carry , pv , add_sub , carry
   WHERE data_in : BYTE &
         negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
         hvn , lvn = BV16_TO_BYTE ( inc_BV16 ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &

                 negative = is_negative ( data_in ) &
         zero = is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) &
         half_carry = bitget ( rgs8 ( f0 ) , 4 ) &
         pv = parity_even_BYTE ( data_in )   &
         add_sub =  1 &
         carry = bitget ( rgs8 ( f0 ) , 0 )

   THEN
          i_o_ports ( rgs8 ( c0 ) ) := data_in ||
          updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , i_o_ports ( rgs8 ( c0 ) ) ) ||
          rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) , ( b0 |-> dec_BYTE ( rgs8 ( b0 ) ) ) ,
          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
          pc := INSTRUCTION_NEXT ( pc )

   END
;

INIR =

    ANY data_in , hvn , lvn ,
          negative , zero , half_carry , pv , add_sub , carry
    WHERE data_in : BYTE &
          negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
          hvn , lvn = BV16_TO_BYTE ( inc_BV16 ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &

                  negative = is_negative ( data_in ) &
          zero = 1  &
          half_carry = bitget ( rgs8 ( f0 ) , 4 ) &
          pv = parity_even_BYTE ( data_in )   &
          add_sub =  1 &
          carry = bitget ( rgs8 ( f0 ) , 0 )
    THEN
          i_o_ports ( rgs8 ( c0 ) ) := data_in ||
          updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , data_in ) ||
          rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) , ( b0 |-> dec_BYTE ( rgs8 ( b0 ) ) ) ,
          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
          IF  is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) = 1 THEN pc := INSTRUCTION_NEXT ( pc ) END


    END
;

IND =

    ANY data_in , hvn , lvn ,
          negative , zero , half_carry , pv , add_sub , carry
    WHERE data_in : BYTE &
          negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
          hvn , lvn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &

                  negative = is_negative ( data_in ) &
          zero = is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) &
          half_carry = bitget ( rgs8 ( f0 ) , 4 ) &
          pv = parity_even_BYTE ( data_in )   &
          add_sub =  1 &
          carry = bitget ( rgs8 ( f0 ) , 0 )
    THEN
          i_o_ports ( rgs8 ( c0 ) ) := data_in ||
          updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , data_in ) ||
          rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) , ( b0 |-> dec_BYTE ( rgs8 ( b0 ) ) ) ,
          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
          pc := INSTRUCTION_NEXT ( pc )


    END
;
INDR =

    ANY data_in , hvn , lvn ,
          negative , zero , half_carry , pv , add_sub , carry
    WHERE data_in : BYTE &
          negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
          hvn , lvn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &

                  negative = is_negative ( data_in ) &
          zero = is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) &
          half_carry = bitget ( rgs8 ( f0 ) , 4 ) &
          pv = parity_even_BYTE ( data_in )   &
          add_sub =  1 &
          carry = bitget ( rgs8 ( f0 ) , 0 )
    THEN
          i_o_ports ( rgs8 ( c0 ) ) := data_in ||
          updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , data_in ) ||
          rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) , ( b0 |-> dec_BYTE ( rgs8 ( b0 ) ) ) ,
          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
          IF  is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) = 1 THEN pc := INSTRUCTION_NEXT ( pc ) END


    END

;

 OUT_9n0_A ( nn ) =
   PRE nn : UCHAR    THEN
          i_o_ports ( UCHAR_TO_BYTE ( nn ) ) := rgs8 ( a0 ) ||
          pc := INSTRUCTION_NEXT ( pc )
    END

;

 OUT_9C0_r ( rr ) =
   PRE rr : id_reg_8   THEN
          i_o_ports ( rgs8 ( rr ) ) := rgs8 ( c0 ) ||
          pc := INSTRUCTION_NEXT ( pc )
    END
;

 OUTI =
   ANY  hvn , lvn ,
                negative , zero , half_carry , pv , add_sub , carry
   WHERE
         negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
         hvn , lvn = BV16_TO_BYTE ( inc_BV16 ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &

                 negative = is_negative ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
         zero = is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) &
         half_carry = bitget ( rgs8 ( f0 ) , 4 ) &
         pv = parity_even_BYTE ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
         add_sub =  1 &
         carry = bitget ( rgs8 ( f0 ) , 0 )

   THEN
          i_o_ports ( rgs8 ( c0 ) ) := mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ||
          updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , i_o_ports ( rgs8 ( c0 ) ) ) ||
          rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) , ( b0 |-> dec_BYTE ( rgs8 ( b0 ) ) ) ,
          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
          pc := INSTRUCTION_NEXT ( pc )

   END
;

 OUTIR =
   ANY  hvn , lvn ,
                negative , zero , half_carry , pv , add_sub , carry
   WHERE
         negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
         hvn , lvn = BV16_TO_BYTE ( inc_BV16 ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &

                 negative = is_negative ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
         zero = is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) &
         half_carry = bitget ( rgs8 ( f0 ) , 4 ) &
         pv = parity_even_BYTE ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
         add_sub =  1 &
         carry = bitget ( rgs8 ( f0 ) , 0 )

   THEN
          i_o_ports ( rgs8 ( c0 ) ) := mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ||
          updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , i_o_ports ( rgs8 ( c0 ) ) ) ||
          rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) , ( b0 |-> dec_BYTE ( rgs8 ( b0 ) ) ) ,
          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
          IF  is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) = 1 THEN pc := INSTRUCTION_NEXT ( pc ) END

   END
 ;

 OUTD =
   ANY  hvn , lvn ,
                negative , zero , half_carry , pv , add_sub , carry
   WHERE
         negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
         hvn , lvn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &

                 negative = is_negative ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
         zero = is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) &
         half_carry = bitget ( rgs8 ( f0 ) , 4 ) &
         pv = parity_even_BYTE ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
         add_sub =  1 &
         carry = bitget ( rgs8 ( f0 ) , 0 )

   THEN
          i_o_ports ( rgs8 ( c0 ) ) := mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ||
          updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , i_o_ports ( rgs8 ( c0 ) ) ) ||
          rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) , ( b0 |-> dec_BYTE ( rgs8 ( b0 ) ) ) ,
          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
          pc := INSTRUCTION_NEXT ( pc )

   END
;

 OUTDR =
   ANY  hvn , lvn ,
                negative , zero , half_carry , pv , add_sub , carry
   WHERE
         negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
         hvn , lvn = BV16_TO_BYTE ( dec_BV16 ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &

         negative = is_negative ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
         zero = is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) &
         half_carry = bitget ( rgs8 ( f0 ) , 4 ) &
         pv = parity_even_BYTE ( mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
         add_sub =  1 &
         carry = bitget ( rgs8 ( f0 ) , 0 )

   THEN
          i_o_ports ( rgs8 ( c0 ) ) := mem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ||
          updateAddressMem ( BYTE_TO_BV16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , i_o_ports ( rgs8 ( c0 ) ) ) ||
          rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) , ( b0 |-> dec_BYTE ( rgs8 ( b0 ) ) ) ,
          get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
          IF  is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) = 1 THEN pc := INSTRUCTION_NEXT ( pc ) END

   END



END
