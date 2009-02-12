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


   pc , sp , ix , iy ,
   i_ , r_ ,
   iff1 , iff2 ,
   im ,
   i_o_ports






INVARIANT

   rgs8 : id_reg_8 --> BYTE &
   pc : INSTRUCTION & sp : BV16 & ix : BV16 & iy : BV16 &
   i_ : BYTE & r_  : BYTE &
   iff1 : BIT &  iff2 : BIT &
   im : ( BIT * BIT ) &
   i_o_ports  : BYTE --> BYTE
































CONCRETE_CONSTANTS

   get_bv_reg16 ,
   REG16_TO_REG8 ,
   REG8_TO_REG16 ,

   update_flag_register_SZ_H_PvNC ,
   get_new_flag_register_SZ_H_PvNC ,

   bv_ireg_plus_d ,
   bv_9ireg_plus_d0 ,

   daa_function




PROPERTIES


   get_bv_reg16 : ( BV16 * ( id_reg_8 --> BYTE ) * id_reg_16 ) --> ( BV16 )
   & ! ( sp_ , rgs8_ , r1 ) .
   ( sp_ :  BV16 & rgs8_ : ( id_reg_8 --> BYTE ) & r1 : id_reg_16
      => ( r1 = BC =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = byte_to_bv16 ( rgs8_ ( b0 ) , rgs8_ ( c0 ) ) ) &
      ( r1 = DE =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = byte_to_bv16 ( rgs8_ ( d0 ) , rgs8_ ( e0 ) ) ) &
      ( r1 = HL =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = byte_to_bv16 ( rgs8_ ( h0 ) , rgs8_ ( l0 ) ) ) &
      ( r1 = SP =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = sp_ ) &
      ( r1 = AF =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = byte_to_bv16 ( rgs8_ ( a0 ) , rgs8_ ( f0 ) ) )
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

   bv_ireg_plus_d = % ( ix_iy , desloc ) . ( ix_iy : BV16 & desloc : SCHAR |
      ushortint_to_bv16 ( ( bv16_to_ushortint ( ix_iy ) + desloc ) mod 65536 ) ) &

   bv_9ireg_plus_d0 = % ( mem , ix_iy , desloc ) . ( mem : ( BV16 --> BYTE ) & ix_iy : BV16 & desloc : SCHAR |
      mem ( ushortint_to_bv16 ( ( bv16_to_ushortint ( ix_iy ) + desloc ) mod 65536 ) ) ) &

   daa_function : ( BIT * BIT * BYTE * BIT ) --> ( BYTE * BIT * BIT ) &
   ! ( zn , c0 , value , h0 ) . ( zn : BIT & c0 : BIT & value : BYTE & h0 : BIT =>
      (  (  zn = 0 & c0 = 0 & get_upper_digit ( value ) : 0 .. 9 & h0 = 0 & get_lower_digit ( value ) : 0 .. 9 =>
                                daa_function ( zn , c0 , value , h0 ) = ( value , 0 , 0 ) ) &
         (  zn = 0 & c0 = 0 & get_upper_digit ( value ) : 0 .. 8 & h0 = 0 & get_lower_digit ( value ) : 10 .. 15 =>
                                daa_function ( zn , c0 , value , h0 ) = ( uchar_to_byte ( simple_add8UCHAR ( byte_to_uchar ( value ) , 6 ) ) , 0 , bool_to_bit ( bool ( half ( byte_to_uchar ( value ) ) + half ( 6 ) >= 2 ** 4 ) ) ) ) &
         (  zn = 0 & c0 = 0 & get_upper_digit ( value ) : 0 .. 9 & h0 = 1 & get_lower_digit ( value ) : 0 .. 3 =>
                                daa_function ( zn , c0 , value , h0 ) = ( uchar_to_byte ( simple_add8UCHAR ( byte_to_uchar ( value ) , 6 ) ) , 0 , bool_to_bit ( bool ( half ( byte_to_uchar ( value ) ) + half ( 6 ) >= 2 ** 4 ) ) ) ) &
         (  zn = 0 & c0 = 0 & get_upper_digit ( value ) : 10 .. 15 & h0 = 0 & get_lower_digit ( value ) : 0 .. 9 =>
                                daa_function ( zn , c0 , value , h0 ) = ( uchar_to_byte ( simple_add8UCHAR ( byte_to_uchar ( value ) , 96 ) ) , 1 , bool_to_bit ( bool ( half ( byte_to_uchar ( value ) ) + half ( 96 ) >= 2 ** 4 ) ) ) ) &
         (  zn = 0 & c0 = 0 & get_upper_digit ( value ) : 9 .. 15 & h0 = 0 & get_lower_digit ( value ) : 10 .. 15 =>
                                daa_function ( zn , c0 , value , h0 ) = ( uchar_to_byte ( simple_add8UCHAR ( byte_to_uchar ( value ) , 102 ) ) , 1 , bool_to_bit ( bool ( half ( byte_to_uchar ( value ) ) + half ( 102 ) >= 2 ** 4 ) ) ) ) &
         (  zn = 0 & c0 = 0 & get_upper_digit ( value ) : 10 .. 15 & h0 = 1 & get_lower_digit ( value ) : 0 .. 3 =>
                                daa_function ( zn , c0 , value , h0 ) = ( uchar_to_byte ( simple_add8UCHAR ( byte_to_uchar ( value ) , 102 ) ) , 1 , bool_to_bit ( bool ( half ( byte_to_uchar ( value ) ) + half ( 102 ) >= 2 ** 4 ) ) ) ) &
         (  zn = 0 & c0 = 1 & get_upper_digit ( value ) : 0 .. 2 & h0 = 0 & get_lower_digit ( value ) : 0 .. 9 =>
                                daa_function ( zn , c0 , value , h0 ) = ( uchar_to_byte ( simple_add8UCHAR ( byte_to_uchar ( value ) , 96 ) ) , 1 , bool_to_bit ( bool ( half ( byte_to_uchar ( value ) ) + half ( 96 ) >= 2 ** 4 ) ) ) ) &
         (  zn = 0 & c0 = 1 & get_upper_digit ( value ) : 0 .. 2 & h0 = 0 & get_lower_digit ( value ) : 10 .. 15 =>
                                daa_function ( zn , c0 , value , h0 ) = ( uchar_to_byte ( simple_add8UCHAR ( byte_to_uchar ( value ) , 102 ) ) , 1 , bool_to_bit ( bool ( half ( byte_to_uchar ( value ) ) + half ( 102 ) >= 2 ** 4 ) ) ) ) &
         (  zn = 0 & c0 = 1 & get_upper_digit ( value ) : 0 .. 3 & h0 = 1 & get_lower_digit ( value ) : 0 .. 3 =>
                                daa_function ( zn , c0 , value , h0 ) = ( uchar_to_byte ( simple_add8UCHAR ( byte_to_uchar ( value ) , 102 ) ) , 1 , bool_to_bit ( bool ( half ( byte_to_uchar ( value ) ) + half ( 102 ) >= 2 ** 4 ) ) ) ) &

         (  zn = 1 & c0 = 0 & get_upper_digit ( value ) : 0 .. 9 & h0 = 0 & get_lower_digit ( value ) : 0 .. 9 =>
                                daa_function ( zn , c0 , value , h0 ) = ( value , 0 , 0 ) ) &
         (  zn = 1 & c0 = 0 & get_upper_digit ( value ) : 0 .. 8 & h0 = 1 & get_lower_digit ( value ) : 6 .. 15 =>
                                daa_function ( zn , c0 , value , h0 ) = ( uchar_to_byte ( simple_add8UCHAR ( byte_to_uchar ( value ) , 250 ) ) , 0 , bool_to_bit ( bool ( half ( byte_to_uchar ( value ) ) + half ( 250 ) >= 2 ** 4 ) ) ) ) &
         (  zn = 1 & c0 = 1 & get_upper_digit ( value ) : 7 .. 15 & h0 = 0 & get_lower_digit ( value ) : 0 .. 9 =>
                                daa_function ( zn , c0 , value , h0 ) = ( uchar_to_byte ( simple_add8UCHAR ( byte_to_uchar ( value ) , 160 ) ) , 1 , bool_to_bit ( bool ( half ( byte_to_uchar ( value ) ) + half ( 160 ) >= 2 ** 4 ) ) ) ) &
         (  zn = 1 & c0 = 1 & get_upper_digit ( value ) : 6 .. 7 & h0 = 1 & get_lower_digit ( value ) : 6 .. 15 =>
                                daa_function ( zn , c0 , value , h0 ) = ( uchar_to_byte ( simple_add8UCHAR ( byte_to_uchar ( value ) , 154 ) ) , 1 , bool_to_bit ( bool ( half ( byte_to_uchar ( value ) ) + half ( 154 ) >= 2 ** 4 ) ) ) ) ) )


ASSERTIONS
   dom ( stack ) = BV16 &
   ran ( stack ) <: BYTE &
   ran ( mem ) <: BYTE &
   dom ( mem ) = BV16 &
   ran ( rgs8 ) <: BYTE &
   dom ( rgs8 ) = id_reg_8 &
   instruction_next ( 0 ) = 1 &
   instruction_next ( 1 ) = 2 &
   instruction_next ( 2 ) = 3 &
   instruction_next ( 3 ) = 4 &
   instruction_next ( 4 ) = 5 &
   instruction_next ( 5 ) = 6 &
   instruction_next ( 6 ) = 7 &
   instruction_next ( 7 ) = 8 &
   instruction_next ( 8 ) = 9 &
   instruction_next ( 9 ) = 10 &
   instruction_next ( 10 ) = 11 &
   instruction_next ( 11 ) = 12 &
   instruction_next ( 12 ) = 13 &
   instruction_next ( 13 ) = 14 &

   mem ( byte_to_bv16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) : BYTE & mem ( byte_to_bv16 ( schar_to_byte ( 0 ) , mem ( byte_to_bv16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) ) ) : BYTE &
   mem ( byte_to_bv16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ) : BYTE & mem ( byte_to_bv16 ( schar_to_byte ( 0 ) , mem ( byte_to_bv16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ) ) ) : BYTE &
   mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) : BYTE & mem ( byte_to_bv16 ( schar_to_byte ( 0 ) , mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) ) : BYTE &
   mem ( byte_to_bv16 ( rgs8 ( a0 ) , rgs8 ( f0 ) ) ) : BYTE & mem ( byte_to_bv16 ( schar_to_byte ( 0 ) , mem ( byte_to_bv16 ( rgs8 ( a0 ) , rgs8 ( f0 ) ) ) ) ) : BYTE &

   mem ( sp ) : BYTE &
   mem ( ix ) : BYTE &
   mem ( iy ) : BYTE &

   ! ( ii , des ) . ( ii : BV16 & des : SCHAR => bv_ireg_plus_d ( ii , des ) : BV16 ) &

   ! ( mmm , ii , des ) . ( mmm : BV16 >-> BYTE & ii : BV16 & des : SCHAR => bv_9ireg_plus_d0 ( mmm , ii , des ) : BYTE ) &

   ! ( zn , c0 , value , h0 ) . ( zn : BIT & c0 : BIT & value : BYTE & h0 : BIT =>
          daa_function ( zn , c0 , value , h0 ) : ( BYTE * BIT * BIT ) )

INITIALISATION

   rgs8 :: id_reg_8 --> BYTE ||
   pc :: INSTRUCTION || sp :: BV16 || ix :: BV16 || iy :: BV16 ||
   i_ :: BYTE || r_  :: BYTE ||
   i_o_ports  :: BYTE --> BYTE ||
   iff1 :: BIT  ||  iff2 :: BIT ||
   im :: ( BIT * BIT )





OPERATIONS


   ADD_A_r ( rr ) =
   PRE rr : id_reg_8
   THEN
      ANY
         sum , negative , carry , half_carry , zero
      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = add8UCHAR ( 0 , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( rgs8 ( rr ) ) )
      THEN
         rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , ( get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) ) }         ||  pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END

   ;
   ADD_A_n ( n1 ) =
   PRE n1 : SCHAR
   THEN
      ANY
         sum , negative , carry , half_carry , zero

      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = add8UCHAR ( 0 , byte_to_uchar ( rgs8 ( a0 ) ) , schar_uchar ( n1 ) )
      THEN
         rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   ADD_A_9HL0 =
   ANY
      sum , negative , carry , half_carry , zero

   WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

      sum , negative , carry , half_carry , zero = add8UCHAR ( 0 , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) )
   THEN
      rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END

   ;

   ADD_A_9IX_d0 ( desloc ) =
   PRE desloc : SCHAR THEN
      ANY
         sum , negative , carry , half_carry , zero

      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = add8UCHAR ( 0 , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( bv_9ireg_plus_d0 ( mem , ix , desloc ) ) )
      THEN
         rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END

   ;

   ADD_A_9IY_d0 ( desloc ) =
   PRE desloc : SCHAR THEN
      ANY
         sum , negative , carry , half_carry , zero

      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = add8UCHAR ( 0 , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( bv_9ireg_plus_d0 ( mem , iy , desloc ) ) )
      THEN
         rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;


   ADC_A_r ( rr ) =
   PRE rr : id_reg_8
   THEN
      ANY
         sum , negative , carry , half_carry , zero

      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = add8UCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( rgs8 ( rr ) ) )
      THEN
         rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   ADC_A_n ( n1 ) =
   PRE n1 : SCHAR
   THEN
      ANY
         sum , negative , carry , half_carry , zero

      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = add8UCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , byte_to_uchar ( rgs8 ( a0 ) ) , schar_uchar ( n1 ) )
      THEN
         rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   ADC_A_9HL0 =
   ANY
      sum , negative , carry , half_carry , zero
   WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
      sum , negative , carry , half_carry , zero = add8UCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) )
   THEN
      rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END

   ;

   ADC_A_9IX_d0 ( desloc ) =
   PRE desloc : SCHAR THEN
      ANY
         sum , negative , carry , half_carry , zero
      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
         sum , negative , carry , half_carry , zero = add8UCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( bv_9ireg_plus_d0 ( mem , ix , desloc ) ) )
      THEN
         rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END


   ;

   ADC_A_9IY_d0 ( desloc ) =
   PRE desloc : SCHAR THEN
      ANY
         sum , negative , carry , half_carry , zero
      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
         sum , negative , carry , half_carry , zero = add8UCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( bv_9ireg_plus_d0 ( mem , iy , desloc ) ) )
      THEN
         rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;


   SUB_A_r ( rr ) =
   PRE rr : id_reg_8
   THEN
      ANY
         sum , negative , carry , half_carry , zero
      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = substract8UCHAR ( 0 , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( rgs8 ( rr ) ) )
      THEN
         rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   SUB_A_n ( n1 ) =
   PRE n1 : SCHAR
   THEN
      ANY
         sum , negative , carry , half_carry , zero

      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = substract8UCHAR ( 0 , byte_to_uchar ( rgs8 ( a0 ) ) , schar_uchar ( n1 ) )
      THEN
         rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   SUB_A_9HL0 =
   ANY
      sum , negative , carry , half_carry , zero
   WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
      sum , negative , carry , half_carry , zero = substract8UCHAR ( 0 , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) )
   THEN
      rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END

   ;

   SUB_A_9IX_d0 ( desloc ) =
   PRE desloc : SCHAR THEN
      ANY
         sum , negative , carry , half_carry , zero
      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
         sum , negative , carry , half_carry , zero = substract8UCHAR ( 0 , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( bv_9ireg_plus_d0 ( mem , ix , desloc ) ) )
      THEN
         rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END

   ;

   SUB_A_9IY_d0 ( desloc ) =
   PRE desloc : SCHAR THEN
      ANY
         sum , negative , carry , half_carry , zero
      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
         sum , negative , carry , half_carry , zero = substract8UCHAR ( 0 , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( bv_9ireg_plus_d0 ( mem , iy , desloc ) ) )
      THEN
         rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;


   SBC_A_r ( rr ) =
   PRE rr : id_reg_8
   THEN
      ANY
         sum , negative , carry , half_carry , zero

      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = substract8UCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( rgs8 ( rr ) ) )
      THEN
         rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   SBC_A_n ( n1 ) =
   PRE n1 : SCHAR
   THEN
      ANY
         sum , negative , carry , half_carry , zero

      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = substract8UCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , byte_to_uchar ( rgs8 ( a0 ) ) , n1 )
      THEN
         rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   SBC_A_9HL0 =
   ANY
      sum , negative , carry , half_carry , zero
   WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
      sum , negative , carry , half_carry , zero = substract8UCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) )
   THEN
      rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END

   ;

   SBC_A_9IX_d0 ( desloc ) =
   PRE desloc : SCHAR THEN
      ANY
         sum , negative , carry , half_carry , zero
      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
         sum , negative , carry , half_carry , zero = substract8UCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( ( bv_9ireg_plus_d0 ( mem , ix , desloc ) ) ) )
      THEN
         rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END

   ;

   SBC_A_9IY_d0 ( desloc ) =
   PRE desloc : SCHAR THEN
      ANY
         sum , negative , carry , half_carry , zero
      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
         sum , negative , carry , half_carry , zero = substract8UCHAR ( bitget ( rgs8 ( f0 ) , 0 ) , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( bv_9ireg_plus_d0 ( mem , ix , desloc ) ) )
      THEN
         rgs8 := rgs8 <+ { a0 |-> uchar_to_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
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
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
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
         result = and ( rgs8 ( a0 ) , schar_to_byte ( n1 ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
      THEN
         rgs8 := rgs8 <+ { a0 |-> result ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   AND_A_9HL0 =
   ANY
      result  , negative , zero , half_carry , pv , add_sub , carry
   WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
      result = and ( rgs8 ( a0 ) , mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
      negative = is_negative ( result ) &
      zero = is_zero ( result ) &
      half_carry = 0 &
      pv =   parity_even_BYTE ( result )   &
      add_sub =  0 &
      carry = 0
   THEN
      rgs8 := rgs8 <+ { a0 |-> result ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END


   ;

   AND_A_9IX_d0 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
      WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
         result = and ( rgs8 ( a0 ) , bv_9ireg_plus_d0 ( mem , ix , desloc ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
      THEN
         rgs8 := rgs8 <+ { a0 |-> result ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   AND_A_9IY_d0 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
      WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
         result = and ( rgs8 ( a0 ) , bv_9ireg_plus_d0 ( mem , iy , desloc ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
      THEN
         rgs8 := rgs8 <+ { a0 |-> result ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
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
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
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
         result = ior ( rgs8 ( a0 ) , schar_to_byte ( n1 ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
      THEN
         rgs8 := rgs8 <+ { a0 |-> result ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   OR_A_9HL0 =
   ANY
      result  , negative , zero , half_carry , pv , add_sub , carry
   WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
      result = ior ( rgs8 ( a0 ) , mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
      negative = is_negative ( result ) &
      zero = is_zero ( result ) &
      half_carry = 0 &
      pv =   parity_even_BYTE ( result )   &
      add_sub =  0 &
      carry = 0
   THEN
      rgs8 := rgs8 <+ { a0 |-> result ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END

   ;

   OR_A_9IX_d0 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
      WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
         result = ior ( rgs8 ( a0 ) , bv_9ireg_plus_d0 ( mem , ix , desloc ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
      THEN
         rgs8 := rgs8 <+ { a0 |-> result ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   OR_A_9IY_d0 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
      WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
         result = ior ( rgs8 ( a0 ) , bv_9ireg_plus_d0 ( mem , iy , desloc ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
      THEN
         rgs8 := rgs8 <+ { a0 |-> result ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
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
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
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
         result = xor ( rgs8 ( a0 ) , schar_to_byte ( n1 ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
      THEN
         rgs8 := rgs8 <+ { a0 |-> result ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   XOR_A_9HL0 =
   ANY
      result  , negative , zero , half_carry , pv , add_sub , carry
   WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
      result = xor ( rgs8 ( a0 ) , mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) &
      negative = is_negative ( result ) &
      zero = is_zero ( result ) &
      half_carry = 0 &
      pv =   parity_even_BYTE ( result )   &
      add_sub =  0 &
      carry = 0
   THEN
      rgs8 := rgs8 <+ { a0 |-> result ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END

   ;

   XOR_A_9IX_d0 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
      WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
         result = xor ( rgs8 ( a0 ) , bv_9ireg_plus_d0 ( mem , ix , desloc ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
      THEN
         rgs8 := rgs8 <+ { a0 |-> result ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   XOR_A_9IY_d0 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY
         result  , negative , zero , half_carry , pv , add_sub , carry
      WHERE  result : BYTE & negative : BIT & zero : BIT & half_carry : BIT &
         result = xor ( rgs8 ( a0 ) , bv_9ireg_plus_d0 ( mem , iy , desloc ) ) &
         negative = is_negative ( result ) &
         zero = is_zero ( result ) &
         half_carry = 0 &
         pv =   parity_even_BYTE ( result )   &
         add_sub =  0 &
         carry = 0
      THEN
         rgs8 := rgs8 <+ { a0 |-> result ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;


   CP_A_r ( rr ) =
   PRE rr : id_reg_8
   THEN
      ANY
         sum , negative , carry , half_carry , zero
      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = substract8UCHAR ( 0 , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( rgs8 ( rr ) ) )
      THEN
         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   CP_A_n ( n1 ) =
   PRE n1 : SCHAR
   THEN
      ANY
         sum , negative , carry , half_carry , zero

      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = substract8UCHAR ( 0 , byte_to_uchar ( rgs8 ( a0 ) ) , schar_uchar ( n1 ) )
      THEN
         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   CP_A_9HL0 =
   ANY
      sum , negative , carry , half_carry , zero
   WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
      sum , negative , carry , half_carry , zero = substract8UCHAR ( 0 , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) )
   THEN
      rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END

   ;

   CP_A_9IX_d0 ( desloc ) =
   PRE desloc : SCHAR THEN
      ANY
         sum , negative , carry , half_carry , zero
      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
         sum , negative , carry , half_carry , zero = substract8UCHAR ( 0 , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( bv_9ireg_plus_d0 ( mem , ix , desloc ) ) )
      THEN
         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END

   ;

   CP_A_9IY_d0 ( desloc ) =
   PRE desloc : SCHAR THEN
      ANY
         sum , negative , carry , half_carry , zero
      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
         sum , negative , carry , half_carry , zero = substract8UCHAR ( 0 , byte_to_uchar ( rgs8 ( a0 ) ) , byte_to_uchar ( bv_9ireg_plus_d0 ( mem , iy , desloc ) ) )
      THEN
         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
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

      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = add8UCHAR ( 0 , byte_to_uchar ( rgs8 ( rr ) ) , 1 )
      THEN
         rgs8 := rgs8 <+ { ( rr |-> uchar_to_byte ( sum ) ) ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   INC_9HL0 =
   ANY
      sum , negative , carry , half_carry , zero

   WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

      sum , negative , carry , half_carry , zero = add8UCHAR ( 0 , byte_to_uchar ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) , 1 )

   THEN
      rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , uchar_to_byte ( sum ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END

   ;

   INC_9IX_d0 ( desloc ) =
   PRE desloc : SCHAR THEN
      ANY
         sum , negative , carry , half_carry , zero

      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = add8UCHAR ( 0 , byte_to_uchar ( bv_9ireg_plus_d0 ( mem , ix , desloc ) ) , 1 )

      THEN
         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
         updateAddressMem ( bv_ireg_plus_d ( ix , desloc ) , uchar_to_byte ( sum ) ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   INC_9IY_d0 ( desloc ) =
   PRE desloc : SCHAR THEN
      ANY
         sum , negative , carry , half_carry , zero

      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = add8UCHAR ( 0 , byte_to_uchar ( bv_9ireg_plus_d0 ( mem , iy , desloc ) ) , 1 )

      THEN
         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||
         updateAddressMem ( bv_ireg_plus_d ( iy , desloc ) , uchar_to_byte ( sum ) ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
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

      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = substract8UCHAR ( 0 , byte_to_uchar ( rgs8 ( rr ) ) , 1 )
      THEN
         rgs8 := rgs8 <+ { ( rr |-> uchar_to_byte ( sum ) ) ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , bitget ( rgs8 ( f0 ) , 0 ) ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   DEC_9HL0 =
   ANY
      sum , negative , carry , half_carry , zero

   WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

      sum , negative , carry , half_carry , zero = substract8UCHAR ( 0 , byte_to_uchar ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) , 1 )

   THEN
      rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , bitget ( rgs8 ( f0 ) , 0 ) ) } ||
      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , uchar_to_byte ( sum ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END

   ;

   DEC_9IX_d0 ( desloc ) =
   PRE desloc : SCHAR THEN
      ANY
         sum , negative , carry , half_carry , zero

      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = substract8UCHAR ( 0 , byte_to_uchar ( bv_9ireg_plus_d0 ( mem , ix , desloc ) ) , 1 )

      THEN
         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , bitget ( rgs8 ( f0 ) , 0 ) ) } ||
         updateAddressMem ( bv_ireg_plus_d ( ix , desloc ) , uchar_to_byte ( sum ) ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   DEC_9IY_d0 ( desloc ) =
   PRE desloc : SCHAR THEN
      ANY
         sum , negative , carry , half_carry , zero

      WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &

         sum , negative , carry , half_carry , zero = substract8UCHAR ( 0 , byte_to_uchar ( bv_9ireg_plus_d0 ( mem , iy , desloc ) ) , 1 )

      THEN
         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , bitget ( rgs8 ( f0 ) , 0 ) ) } ||
         updateAddressMem ( bv_ireg_plus_d ( iy , desloc ) , uchar_to_byte ( sum ) ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
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
         result = add16USHORTINT ( 0 , bv16_to_ushortint ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bv16_to_ushortint ( bv_value ) ) &
         bvh , bvl = bv16_to_byte ( ushortint_to_bv16 ( result ) ) &
         negative = bitget ( rgs8 ( f0 ) , 7 ) &
         zero = bitget ( rgs8 ( f0 ) , 6 ) &
         half_carry = add_halfcarryUSHORTINT ( 0 , bv16_to_ushortint ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bv16_to_ushortint ( bv_value ) ) &
         carry =  add_carryUSHORTINT ( 0 , bv16_to_ushortint ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bv16_to_ushortint ( bv_value ) )

      THEN
         rgs8 := rgs8 <+ { h0 |-> bvh , l0 |-> bvl ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , bitget ( rgs8 ( f0 ) , 2 ) , 0 , carry ) } ||

         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
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

         result = add16USHORTINT ( bitget ( rgs8 ( f0 ) , 0 ) , bv16_to_ushortint ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bv16_to_ushortint ( bv_value ) ) &
         negative = 1 &
         zero = is_zeroUSHORTINT ( result ) &
         half_carry = add_halfcarryUSHORTINT ( bitget ( rgs8 ( f0 ) , 0 ) , bv16_to_ushortint ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bv16_to_ushortint ( bv_value ) ) &
         carry =  add_carryUSHORTINT ( bitget ( rgs8 ( f0 ) , 0 ) , bv16_to_ushortint ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bv16_to_ushortint ( bv_value ) )

      THEN
         rgs8 := rgs8 <+ { h0 |-> bvh , l0 |-> bvl ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||

         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
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

         result = add16USHORTINT ( bitget ( rgs8 ( f0 ) , 0 ) , bv16_to_ushortint ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bv16_to_ushortint ( bv_value ) ) &
         negative = 1 &
         zero = is_zeroUSHORTINT ( result ) &
         half_carry = sub_halfcarryUSHORTINT ( bitget ( rgs8 ( f0 ) , 0 ) , bv16_to_ushortint ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bv16_to_ushortint ( bv_value ) ) &
         carry =  sub_carryUSHORTINT ( bitget ( rgs8 ( f0 ) , 0 ) , bv16_to_ushortint ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bv16_to_ushortint ( bv_value ) )

      THEN
         rgs8 := rgs8 <+ { h0 |-> bvh , l0 |-> bvl ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 0 , carry ) } ||

         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
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

         result = add16USHORTINT ( 0 , bv16_to_ushortint ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bv16_to_ushortint ( bv_value ) ) &
         bvh , bvl = bv16_to_byte ( ushortint_to_bv16 ( result ) ) &
         negative = bitget ( rgs8 ( f0 ) , 7 ) &
         zero = bitget ( rgs8 ( f0 ) , 6 ) &
         half_carry = add_halfcarryUSHORTINT ( 0 , bv16_to_ushortint ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bv16_to_ushortint ( bv_value ) ) &
         carry =  add_carryUSHORTINT ( 0 , bv16_to_ushortint ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bv16_to_ushortint ( bv_value ) )

      THEN
         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , bitget ( rgs8 ( f0 ) , 2 ) , 0 , carry ) } ||
         ix :=  ushortint_to_bv16 ( result ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
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

         result = add16USHORTINT ( 0 , bv16_to_ushortint ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bv16_to_ushortint ( bv_value ) ) &
         bvh , bvl = bv16_to_byte ( ushortint_to_bv16 ( result ) ) &
         negative = bitget ( rgs8 ( f0 ) , 7 ) &
         zero = bitget ( rgs8 ( f0 ) , 6 ) &
         half_carry = add_halfcarryUSHORTINT ( 0 , bv16_to_ushortint ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bv16_to_ushortint ( bv_value ) ) &
         carry =  add_carryUSHORTINT ( 0 , bv16_to_ushortint ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bv16_to_ushortint ( bv_value ) )

      THEN
         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , bitget ( rgs8 ( f0 ) , 2 ) , 0 , carry ) } ||
         iy :=  ushortint_to_bv16 ( result ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
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
            bv16_to_byte ( inc_BV16 ( byte_to_bv16 ( rgs8 ( rh ) , rgs8 ( rl ) ) ) ) = vh , vl
         THEN
            rgs8 := rgs8 <+ { rh |-> vh , rl |-> vl }
         END
      END
      ||                pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )

   END
   ;

   INC_IX =
   BEGIN
      ix := inc_BV16 ( ix ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;

   INC_IY =
   BEGIN
      iy := inc_BV16 ( iy ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
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
            bv16_to_byte ( dec_BV16 ( byte_to_bv16 ( rgs8 ( rh ) , rgs8 ( rl ) ) ) ) = vh , vl
         THEN
            rgs8 := rgs8 <+ { rh |-> vh , rl |-> vl }     ||                pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
         END
      END

   END
   ;

   DEC_IX =
   BEGIN
      ix := dec_BV16 ( ix )     ||                pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;

   DEC_IY =
   BEGIN
      ix := dec_BV16 ( ix )     ||                pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END




END
