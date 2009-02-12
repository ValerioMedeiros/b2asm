/* Z80Man_bitControl
* Author: valerio
* Creation date: qua dez 3 2008
*/

MACHINE
   Z80Man_bitControl
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

   /************************************************************************************** 
   ***  			Rotate and Shift Group pg. 190
   **************************************************************************************/

   RLCA =
   BEGIN
      rgs8 := rgs8 <+ { a0 |-> rotateleft ( rgs8 ( a0 ) ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , bitget ( rgs8 ( f0 ) , 7 ) , bitget ( rgs8 ( f0 ) , 6 ) , 0 , bitget ( rgs8 ( f0 ) , 2 ) , 0 , bitget ( rgs8 ( a0 ) , 7 ) ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )

   END
   ;

   RLA =
   BEGIN
      rgs8 := rgs8 <+ { a0 |-> ( rotateleft ( rgs8 ( a0 ) ) <+ { ( 0 |-> bitget ( rgs8 ( f0 ) , 0 ) ) } ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , bitget ( rgs8 ( f0 ) , 7 ) , bitget ( rgs8 ( f0 ) , 6 ) , 0 , bitget ( rgs8 ( f0 ) , 2 ) , 0 , bitget ( rgs8 ( a0 ) , 7 ) ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )

   END
   ;

   RRCA =
   BEGIN
      rgs8 := rgs8 <+ { a0 |-> rotateright ( rgs8 ( a0 ) ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , bitget ( rgs8 ( f0 ) , 7 ) , bitget ( rgs8 ( f0 ) , 6 ) , 0 , bitget ( rgs8 ( f0 ) , 2 ) , 0 , bitget ( rgs8 ( a0 ) , 0 ) ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )

   END
   ;

   RRA =
   BEGIN
      rgs8 := rgs8 <+ { a0 |-> ( rotateright ( rgs8 ( a0 ) ) <+ { ( 0 |-> bitget ( rgs8 ( f0 ) , 0 ) ) } ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , bitget ( rgs8 ( f0 ) , 7 ) , bitget ( rgs8 ( f0 ) , 6 ) , 0 , bitget ( rgs8 ( f0 ) , 2 ) , 0 , bitget ( rgs8 ( a0 ) , 0 ) ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )

   END
   ;
   RLC_r ( rr ) =
   PRE rr : id_reg_8 THEN
      ANY res WHERE res : BYTE & res = rotateleft ( rgs8 ( rr ) ) THEN

         rgs8 := rgs8 <+ { rr |-> res ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) , 0 , bitget ( rgs8 ( rr ) , 7 ) ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   RLC_9HL0 =

   ANY res WHERE res : BYTE & res = rotateleft ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) THEN

      rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , is_negative ( res ) , is_zero ( res ) , 0 ,
            parity_even_BYTE ( res ) , 0 , bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 7 ) ) } ||
      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;

   RLC_9IX_d9 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY res WHERE res : BYTE & res = rotateleft ( bv_9ireg_plus_d0 ( mem , ix , desloc ) ) THEN

         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) ,
               0 , bitget ( bv_9ireg_plus_d0 ( mem , ix , desloc ) , 7 ) ) } ||
         updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   RLC_9IY_d9 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY res WHERE res : BYTE & res = rotateleft ( bv_9ireg_plus_d0 ( mem , iy , desloc ) ) THEN

         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) ,
               0 , bitget ( bv_9ireg_plus_d0 ( mem , iy , desloc ) , 7 ) ) } ||
         updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;



   RL_r ( rr ) =
   PRE rr : id_reg_8 THEN
      ANY res WHERE res : BYTE & res = ( rotateleft ( rgs8 ( rr ) ) <+ { ( 0 |-> bitget ( rgs8 ( f0 ) , 0 ) ) } ) THEN

         rgs8 := rgs8 <+ { rr |-> res ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) , 0 , bitget ( rgs8 ( rr ) , 7 ) ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   RL_9HL =

   ANY res WHERE res : BYTE & res = ( rotateleft ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) <+ { ( 0 |-> bitget ( rgs8 ( f0 ) , 0 ) ) } ) THEN

      rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
            is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) , 0 , bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 7 ) ) } ||

      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;

   RL_9IX_d9 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY res WHERE res : BYTE & res = ( rotateleft ( bv_9ireg_plus_d0 ( mem , ix , desloc ) ) <+ { ( 0 |-> bitget ( rgs8 ( f0 ) , 0 ) ) } ) THEN

         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) ,
               0 , bitget ( bv_9ireg_plus_d0 ( mem , ix , desloc ) , 7 ) ) } ||
         updateAddressMem ( bv_ireg_plus_d ( ix , desloc ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   RL_9IY_d9 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY res WHERE res : BYTE & res = ( rotateleft ( bv_9ireg_plus_d0 ( mem , iy , desloc ) ) <+ { ( 0 |-> bitget ( rgs8 ( f0 ) , 0 ) ) } )
      THEN

         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) ,
               0 , bitget ( bv_9ireg_plus_d0 ( mem , iy , desloc ) , 7 ) ) } ||
         updateAddressMem ( bv_ireg_plus_d ( iy , desloc ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   RRC_r ( rr ) =
   PRE rr : id_reg_8 THEN
      ANY res WHERE res : BYTE & res = rotateright ( rgs8 ( rr ) ) THEN

         rgs8 := rgs8 <+ { rr |-> res ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) , 0 , bitget ( rgs8 ( rr ) , 0 ) ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   RRC_9HL0 =

   ANY res WHERE res : BYTE & res = rotateright ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) THEN

      rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
            is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) , 0 , bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 0 ) ) } ||

      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;

   RRC_9IX_d9 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY res WHERE res : BYTE & res = rotateright ( bv_9ireg_plus_d0 ( mem , ix , desloc ) ) THEN

         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) ,
               0 , bitget ( bv_9ireg_plus_d0 ( mem , ix , desloc ) , 0 ) ) } ||
         updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   RRC_9IY_d9 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY res WHERE res : BYTE & res = rotateright ( bv_9ireg_plus_d0 ( mem , iy , desloc ) ) THEN

         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) ,
               0 , bitget ( bv_9ireg_plus_d0 ( mem , iy , desloc ) , 0 ) ) } ||
         updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   RR_r ( rr ) =
   PRE rr : id_reg_8 THEN
      ANY res WHERE res : BYTE & res = ( rotateright ( rgs8 ( rr ) ) <+ { ( 0 |-> bitget ( rgs8 ( f0 ) , 0 ) ) } ) THEN

         rgs8 := rgs8 <+ { rr |-> res ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) , 0 , bitget ( rgs8 ( rr ) , 0 ) ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   RR_9HL =

   ANY res WHERE res : BYTE & res = ( rotateright ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) <+ { ( 0 |-> bitget ( rgs8 ( f0 ) , 0 ) ) } ) THEN

      rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
            is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) , 0 , bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 0 ) ) } ||

      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;

   RR_9IX_d9 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY res WHERE res : BYTE & res = ( rotateright ( bv_9ireg_plus_d0 ( mem , ix , desloc ) ) <+ { ( 0 |-> bitget ( rgs8 ( f0 ) , 0 ) ) } ) THEN

         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) ,
               0 , bitget ( bv_9ireg_plus_d0 ( mem , ix , desloc ) , 0 ) ) } ||
         updateAddressMem ( bv_ireg_plus_d ( ix , desloc ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   RR_9IY_d9 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY res WHERE res : BYTE & res = ( rotateright ( bv_9ireg_plus_d0 ( mem , iy , desloc ) ) <+ { ( 0 |-> bitget ( rgs8 ( f0 ) , 0 ) ) } )
      THEN

         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) ,
               0 , bitget ( bv_9ireg_plus_d0 ( mem , iy , desloc ) , 0 ) ) } ||
         updateAddressMem ( bv_ireg_plus_d ( iy , desloc ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   SLA_r ( rr ) =
   PRE rr : id_reg_8 THEN
      ANY res WHERE res : BYTE & res = ( rotateright ( rgs8 ( rr ) ) <+ { ( 0 |-> bitget ( rgs8 ( f0 ) , 0 ) ) } ) THEN

         rgs8 := rgs8 <+ { rr |-> res ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) , 0 , bitget ( rgs8 ( rr ) , 7 ) ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   SLA_9HL =

   ANY res WHERE res : BYTE & res = ( rotateright ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) <+ { ( 0 |-> bitget ( rgs8 ( f0 ) , 0 ) ) } ) THEN

      rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
            is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) , 0 , bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 7 ) ) } ||

      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;

   SLA_9IX_d9 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY res WHERE res : BYTE & res = ( rotateright ( bv_9ireg_plus_d0 ( mem , ix , desloc ) ) <+ { ( 0 |-> bitget ( rgs8 ( f0 ) , 0 ) ) } ) THEN

         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) ,
               0 , bitget ( bv_9ireg_plus_d0 ( mem , ix , desloc ) , 7 ) ) } ||
         updateAddressMem ( bv_ireg_plus_d ( ix , desloc ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   SLA_9IY_d9 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY res WHERE res : BYTE & res = ( rotateright ( bv_9ireg_plus_d0 ( mem , iy , desloc ) ) <+ { ( 0 |-> bitget ( rgs8 ( f0 ) , 0 ) ) } )
      THEN

         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) ,
               0 , bitget ( bv_9ireg_plus_d0 ( mem , iy , desloc ) , 7 ) ) } ||
         updateAddressMem ( bv_ireg_plus_d ( iy , desloc ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   SRA_r ( rr ) =
   PRE rr : id_reg_8 THEN
      ANY res WHERE res : BYTE & res = ( rotateright ( rgs8 ( rr ) ) ) <+ { 7 |-> bitget ( rgs8 ( rr ) , 7 ) } THEN

         rgs8 := rgs8 <+ { rr |-> res ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) , 0 , bitget ( rgs8 ( rr ) , 0 ) ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   SRA_9HL0 =

   ANY res WHERE res : BYTE & res = ( rotateright ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) ) <+ { 7 |-> mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ( 7 ) } THEN

      rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
            is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) , 0 , bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 0 ) ) } ||

      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;

   SRA_9IX_d9 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY res WHERE res : BYTE &
         res = ( rotateright ( bv_9ireg_plus_d0 ( mem , ix , desloc ) ) ) <+
         { 7 |-> bitget ( bv_9ireg_plus_d0 ( mem , ix , desloc ) , 7 ) }
      THEN


         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) ,
               0 , bitget ( bv_9ireg_plus_d0 ( mem , ix , desloc ) , 0 ) ) } ||
         updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   SRA_9IY_d9 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY res WHERE res : BYTE &
         res = rotateright ( bv_9ireg_plus_d0 ( mem , iy , desloc ) ) <+
         { 7 |-> bv_9ireg_plus_d0 ( mem , iy , desloc ) ( 7 ) }
      THEN

         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               is_negative ( res ) , is_zero ( res ) , 0 , parity_even_BYTE ( res ) ,
               0 , bitget ( bv_9ireg_plus_d0 ( mem , iy , desloc ) , 0 ) ) } ||
         updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   SRL_r ( rr ) =
   PRE rr : id_reg_8 THEN
      ANY res WHERE res : BYTE & res = bitclear ( rotateright ( rgs8 ( rr ) ) , 7 ) THEN

         rgs8 := rgs8 <+ { rr |-> res ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               0 , is_zero ( res ) , 0 , parity_even_BYTE ( res ) , 0 , bitget ( rgs8 ( rr ) , 0 ) ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   SRL_9HL0 =

   ANY res WHERE res : BYTE & res = bitclear ( rotateright ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) , 7 ) THEN

      rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
            0 , is_zero ( res ) , 0 , parity_even_BYTE ( res ) , 0 , bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 0 ) ) } ||

      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;

   SRL_9IX_d9 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY res WHERE res : BYTE &
         res = bitclear ( rotateright ( bv_9ireg_plus_d0 ( mem , ix , desloc ) ) , 7 )
      THEN


         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               0 , is_zero ( res ) , 0 , parity_even_BYTE ( res ) ,
               0 , bitget ( bv_9ireg_plus_d0 ( mem , ix , desloc ) , 0 ) ) } ||
         updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   SRL_9IY_d9 ( desloc ) =
   PRE desloc : SCHAR THEN

      ANY res WHERE res : BYTE &
         res = bitclear ( rotateright ( bv_9ireg_plus_d0 ( mem , iy , desloc ) ) , 7 )
      THEN

         rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
               0 , is_zero ( res ) , 0 , parity_even_BYTE ( res ) ,
               0 , bitget ( bv_9ireg_plus_d0 ( mem , iy , desloc ) , 0 ) ) } ||
         updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END
   ;

   RLD =
   ANY res , acc WHERE res : BYTE & acc : BYTE &
      res =  { 7 |-> bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 3 ) , 6 |-> bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 2 ) , 5 |-> bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 1 ) ,
         4 |-> bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 0 ) ,
         3 |-> bitget ( rgs8 ( a0 ) , 3 ) , 2 |-> bitget ( rgs8 ( a0 ) , 2 ) , 1 |-> bitget ( rgs8 ( a0 ) , 1 ) ,
         0 |-> bitget ( rgs8 ( a0 ) , 0 ) } &
      acc = rgs8 ( a0 ) <+ { 3 |-> bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 7 ) , 2 |-> bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 6 ) ,
         1 |-> bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 5 ) , 0 |-> bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 4 ) }

   THEN

      rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 ,
            is_negative ( acc ) , is_zero ( acc ) , 0 , parity_even_BYTE ( acc ) ,
            0 , bitget ( rgs8 ( f0 ) , 0 ) ) } ||
      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res )
      || pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;
   RRD =
   ANY res , acc WHERE res : BYTE & acc : BYTE &
      res =  { 7 |-> bitget ( rgs8 ( a0 ) , 3 ) , 6 |-> bitget ( rgs8 ( a0 ) , 2 ) , 5 |-> bitget ( rgs8 ( a0 ) , 1 ) ,
         4 |-> bitget ( rgs8 ( a0 ) , 0 ) ,
         3 |-> bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 7 ) , 2 |-> bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 6 ) , 1 |-> bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 5 ) ,
         0 |-> bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 4 ) } &
      acc = rgs8 ( a0 ) <+ { 3 |-> bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 3 ) , 2 |-> bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 2 ) ,
         1 |-> bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 1 ) , 0 |-> bitget ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , 0 ) }

   THEN
      rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , is_negative ( acc ) , is_zero ( acc ) , 0 , parity_even_BYTE ( acc ) , 0 , bitget ( rgs8 ( f0 ) , 0 ) ) } ||
      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , res )
      || pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;

   /************************************************************************************** 
   ***  			BIT Manipulation  Group
   **************************************************************************************/

   BIT_b_rr ( bb , rr ) =
   PRE bb : 0 .. 7 & rr : id_reg_8 THEN
      rgs8 := update_flag_register_SZ_H_PvNC ( rgs8 , bitget ( rgs8 ( f0 ) , 7 ) , bit_not ( bitget ( rgs8 ( rr ) , bb ) ) , 1 , bitget ( rgs8 ( f0 ) , 2 ) , 0 , bitget ( rgs8 ( f0 ) , 0 ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;

   BIT_b_9HL0 ( bb ) =
   PRE bb : 0 .. 7 THEN
      rgs8 := update_flag_register_SZ_H_PvNC ( rgs8 , bitget ( rgs8 ( f0 ) , 7 ) , bit_not ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ( bb ) ) , 1 , bitget ( rgs8 ( f0 ) , 2 ) , 0 , bitget ( rgs8 ( f0 ) , 0 ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;



   BIT_b_9IX_d0 ( desloc , bb ) =
   PRE bb : 0 .. 7 & desloc : SCHAR THEN
      rgs8 := update_flag_register_SZ_H_PvNC ( rgs8 , bitget ( rgs8 ( f0 ) , 7 ) , bit_not ( bv_9ireg_plus_d0 ( mem , ix , desloc ) ( bb ) ) , 1 , bitget ( rgs8 ( f0 ) , 2 ) , 0 , bitget ( rgs8 ( f0 ) , 0 ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;


   BIT_b_9IY_d0 ( desloc , bb ) =
   PRE bb : 0 .. 7 & desloc : SCHAR THEN
      rgs8 := update_flag_register_SZ_H_PvNC ( rgs8 , bitget ( rgs8 ( f0 ) , 7 ) , bit_not ( bv_9ireg_plus_d0 ( mem , iy , desloc ) ( bb ) ) , 1 , bitget ( rgs8 ( f0 ) , 2 ) , 0 , bitget ( rgs8 ( f0 ) , 0 ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;


   SET_b_r ( bb , rr ) =
   PRE bb  : 0 .. 7 & rr : id_reg_8 THEN
      rgs8 ( rr ) := bitset ( rgs8 ( rr ) , bb ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;

   SET_b_9HL0 ( bb ) =
   PRE bb  : 0 .. 7 THEN
      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , bv_set ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bb ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;
   SET_b_9IX_d0 ( bb , desloc ) =
   PRE bb  : 0 .. 7 & desloc : SCHAR THEN
      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , bv_set ( bv_9ireg_plus_d0 ( mem , ix , desloc ) , bb ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;
   SET_b_9IY_d0 ( bb , desloc ) =
   PRE bb  : 0 .. 7 & desloc : SCHAR THEN
      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , bv_set ( bv_9ireg_plus_d0 ( mem , iy , desloc ) , bb ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;

   RES_b_r ( bb , rr ) =
   PRE bb  : 0 .. 7 & rr : id_reg_8 THEN
      rgs8 ( rr ) := bv_clear ( rgs8 ( rr ) , bb ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;

   RES_b_9HL0 ( bb ) =
   PRE bb  : 0 .. 7 THEN
      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , bv_clear ( mem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) , bb ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;
   RES_b_9IX_d0 ( bb , desloc ) =
   PRE bb  : 0 .. 7 & desloc : SCHAR THEN
      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , bv_clear ( bv_9ireg_plus_d0 ( mem , ix , desloc ) , bb ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;
   RES_b_9IY_d0 ( bb , desloc ) =
   PRE bb  : 0 .. 7 & desloc : SCHAR THEN
      updateAddressMem ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) , bv_clear ( bv_9ireg_plus_d0 ( mem , iy , desloc ) , bb ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END
   ;
   /************************************************************************************** 
   ***  			JUMP GROUP
   **************************************************************************************/

   JP_nn ( nn ) =
   PRE nn : USHORTINT THEN
      pc := nn || r_ := update_refresh_reg ( r_ )
   END ;

   JP_cc_nn ( cc , nn ) =
   PRE cc : BOOL & nn : USHORTINT THEN
      IF        cc = TRUE THEN    pc := nn
      ELSE pc := instruction_next ( pc ) END
      || r_ := update_refresh_reg ( r_ )
   END ;

   JR_e ( ee ) =
   PRE ee : SCHAR & ee - 2 : SCHAR THEN
      pc := ( pc + ee ) mod 2 ** 16 || r_ := update_refresh_reg ( r_ )
   END ;

   JR_C_e ( ee ) =
   PRE ee : SCHAR & ee - 2 : SCHAR THEN
      IF bitget ( rgs8 ( f0 ) , 0 ) = 0 THEN pc := instruction_next ( pc )
      ELSE pc := ( pc + ee ) mod 2 ** 16 END
      || r_ := update_refresh_reg ( r_ )
   END ;


   JR_NC_e ( ee ) =
   PRE ee : SCHAR & ee - 2 : SCHAR THEN
      IF bitget ( rgs8 ( f0 ) , 0 ) = 1 THEN pc := instruction_next ( pc )
      ELSE pc := ( pc + ee ) mod 2 ** 16 END
      || r_ := update_refresh_reg ( r_ )
   END ;

   JR_Z_e ( ee ) =
   PRE ee : SCHAR & ee - 2 : SCHAR THEN
      IF bitget ( rgs8 ( f0 ) , 6 ) = 0 THEN pc := instruction_next ( pc )
      ELSE pc := ( pc + ee ) mod 2 ** 16 END
      || r_ := update_refresh_reg ( r_ )
   END ;

   JR_NZ_e ( ee ) =
   PRE ee : SCHAR & ee - 2 : SCHAR THEN
      IF bitget ( rgs8 ( f0 ) , 6 ) = 1 THEN pc := instruction_next ( pc )
      ELSE pc := ( pc + ee ) mod 2 ** 16 END
      || r_ := update_refresh_reg ( r_ )
   END ;

   JP_HL =
   BEGIN
      pc := bv16_to_ushortint ( byte_to_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) || r_ := update_refresh_reg ( r_ )
   END ;

   JP_IX =
   BEGIN
      pc := bv16_to_ushortint ( ix ) || r_ := update_refresh_reg ( r_ )
   END ;

   JP_IY =
   BEGIN
      pc :=  bv16_to_ushortint ( iy ) || r_ := update_refresh_reg ( r_ )
   END ;

   DJNZ_e ( ee ) =
   PRE ee : SCHAR & ee - 2 : SCHAR THEN

      rgs8 ( b0 ) :=    dec_BYTE ( rgs8 ( b0 ) ) ||

      IF is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) = 1 THEN        pc := instruction_next ( pc )
      ELSE pc := ( pc + ee ) mod 2 ** 16 END || r_ := update_refresh_reg ( r_ )

   END ;

   /************************************************************************************** 
   ***  			CALL and RETURN GROUP
   **************************************************************************************/

   CALL_nn ( nn ) =
   PRE nn : USHORTINT THEN
      updateAddressStack ( sp , ushortint_to_bv16 ( instruction_next ( pc ) ) ) ||
      sp := inc_BV16 ( sp ) ||
      pc :=  nn
      || r_ := update_refresh_reg ( r_ )
   END

   /*;
   RETURN =
   PRE sp > 0 THEN
   pop(sp)  COMENT**stack := {sp - 1} <<| stack **COMENT_CLOSE ||
   pc := stack(sp - 1) ||
   sp := sp - 1
   || r_ := update_refresh_reg(r_)    
   END
   ;    
   */

END
