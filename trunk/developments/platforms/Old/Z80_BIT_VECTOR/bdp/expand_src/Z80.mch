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
    ALU ,
    BIT_DEFINITION ,
    BIT_VECTOR_DEFINITION ,
    BYTE_DEFINITION ,
    BV16_DEFINITION ,

    UCHAR_DEFINITION ,
    SCHAR_DEFINITION ,
    SSHORT_DEFINITION ,
    USHORT_DEFINITION ,
    POWER2
SETS
    id_reg_8 = { a0 , f0 , f_0 , a_0 ,
        b0 , c0 , b_0 , c_0 ,
        d0 , e0 , d_0 , e_0 ,
        h0 , l0 , h_0 , l_0 } ;
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
DEFINITIONS
    bv_BC == byte_bv16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ;
    bv_HL == byte_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ;
    bv_DE == byte_bv16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ;
    bv_AF == byte_bv16 ( rgs8 ( a0 ) , rgs8 ( f0 ) ) ;

    bv_9BC0 == mem ( byte_bv16 ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) ;
    bv_9DE0 == mem ( byte_bv16 ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ) ;
    bv_9HL0 == mem ( byte_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ;
    bv_9AF0 == mem ( byte_bv16 ( rgs8 ( a0 ) , rgs8 ( f0 ) ) ) ;

    bv_9SP0 == mem ( sp ) ;
    bv_9IX0 == mem ( ix ) ;
    bv_9IY0 == mem ( iy ) ;

    sp_plus_one == ushort_bv16 ( add16USHORT ( 0 , bv16_ushort ( sp ) , 1 ) ) ;
    sp_plus_two == ushort_bv16 ( add16USHORT ( 0 , bv16_ushort ( sp ) , 2 ) ) ;
    sp_minus_one == ushort_bv16 ( add16USHORT ( 0 , bv16_ushort ( sp ) , - 1 ) ) ;
    sp_minus_two == ushort_bv16 ( add16USHORT ( 0 , bv16_ushort ( sp ) , - 2 ) ) ;

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

    bv_ireg_plus_d ,
    bv_9ireg_plus_d0 ,

    cc_get
PROPERTIES


    get_bv_reg16 : ( BV16 * ( id_reg_8 --> BYTE ) * id_reg_16 ) --> ( BV16 )
    & ! ( sp_ , rgs8_ , r1 ) .
    ( sp_ :  BV16 & rgs8_ : ( id_reg_8 --> BYTE ) & r1 : id_reg_16
        => ( r1 = BC =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = byte_bv16 ( rgs8_ ( b0 ) , rgs8_ ( c0 ) ) ) &
        ( r1 = DE =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = byte_bv16 ( rgs8_ ( d0 ) , rgs8_ ( e0 ) ) ) &
        ( r1 = HL =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = byte_bv16 ( rgs8_ ( h0 ) , rgs8_ ( l0 ) ) ) &
        ( r1 = SP =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = sp_ ) &
        ( r1 = AF =>  get_bv_reg16 ( sp_ , rgs8_ , r1 ) = byte_bv16 ( rgs8_ ( a0 ) , rgs8_ ( f0 ) ) )
    ) &


    REG16_TO_REG8 : id_reg_16 --> ( id_reg_8 * id_reg_8 ) &
    REG16_TO_REG8 ( BC ) = ( b0 , c0 )  &
    REG16_TO_REG8 ( DE ) = ( d0 , e0 )  &
    REG16_TO_REG8 ( HL ) = ( h0 , l0 )  &
    REG16_TO_REG8 ( AF ) = ( a0 , f0 )

    &
    REG8_TO_REG16 = REG16_TO_REG8 ~ &

    update_flag_register_SZ_H_PvNC = % ( rgs8_ , s7 , z6 , h4 , pv2 , n_add_sub , c0 ) .
    ( rgs8_ : id_reg_8 --> BYTE & s7 : BIT & z6  : BIT &   h4 : BIT  & pv2 : BIT & n_add_sub : BIT & c0 : BIT |
        rgs8_ <+ { ( f0 |-> {
                    /*S*/         ( 7 |-> s7 ) ,
                    /*Z*/         ( 6 |-> z6 ) ,
                    /*(5 |-> rgs8_(f0)(5) ),*/
                    /*H*/         ( 4 |-> h4 ) ,
                    /*(3 |-> rgs8_(f0)(3) ),*/
                    /*P/V*/       ( 2 |-> pv2 ) ,
                    /*N*/         ( 1 |-> n_add_sub ) ,
                    /*C*/         ( 0 |-> c0 ) } ) } )

    &

    get_new_flag_register_SZ_H_PvNC = % ( rgs8_ , s7 , z6 , h4 , pv2 , n_add_sub , c0 ) .
    ( rgs8_ : id_reg_8 --> BYTE & s7 : BIT & z6  : BIT &   h4 : BIT  & pv2 : BIT & n_add_sub : BIT & c0 : BIT |
        ( f0 |-> {
                /*S*/         ( 7 |-> s7 ) ,
                /*Z*/         ( 6 |-> z6 ) ,
                /*(5 |-> rgs8_(f0)(5) ),*/
                /*H*/         ( 4 |-> h4 ) ,
                /*(3 |-> rgs8_(f0)(3) ),*/
                /*P/V*/       ( 2 |-> pv2 ) ,
                /*N*/         ( 1 |-> n_add_sub ) ,
                /*C*/         ( 0 |-> c0 ) } ) ) &

    bv_ireg_plus_d = % ( ix_iy , desloc ) . ( ix_iy : BV16 & desloc : SCHAR |
        ushort_bv16 ( ( bv16_ushort ( ix_iy ) + desloc ) mod 65536 ) ) &

    bv_9ireg_plus_d0 = % ( mem , ix_iy , desloc ) . ( mem : ( BV16 --> BYTE ) & ix_iy : BV16 & desloc : SCHAR |
        mem ( bv_ireg_plus_d ( ix_iy , desloc ) ) ) &

    cc_get : ( ( id_reg_8 --> BYTE ) * ( 0 .. 8 ) ) --> BIT &
    ! ( rgs8_ ) . ( rgs8_ : id_reg_8 --> BYTE =>    cc_get ( rgs8_ , 0 ) = 1 - bitget ( rgs8_ ( f0 ) , 6 ) &
        cc_get ( rgs8_ , 1 ) = bitget ( rgs8_ ( f0 ) , 6 ) &
        cc_get ( rgs8_ , 2 ) = 1 - bitget ( rgs8_ ( f0 ) , 0 ) &
        cc_get ( rgs8_ , 3 ) = bitget ( rgs8_ ( f0 ) , 0 )  &
        cc_get ( rgs8_ , 4 ) = 1 - bitget ( rgs8_ ( f0 ) , 2 ) &
        cc_get ( rgs8_ , 5 ) = bitget ( rgs8_ ( f0 ) , 2 )  &
        cc_get ( rgs8_ , 6 ) = 1 - bitget ( rgs8_ ( f0 ) , 7 ) &
        cc_get ( rgs8_ , 7 ) = bitget ( rgs8_ ( f0 ) , 7 ) )

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

    bv_9BC0 : BYTE  & mem ( byte_bv16 ( schar_byte ( 0 ) , bv_9BC0 ) ) : BYTE &
    bv_9DE0 : BYTE  & mem ( byte_bv16 ( schar_byte ( 0 ) , bv_9DE0 ) ) : BYTE &
    bv_9HL0 : BYTE  & mem ( byte_bv16 ( schar_byte ( 0 ) , bv_9HL0 ) ) : BYTE &
    bv_9AF0 : BYTE  & mem ( byte_bv16 ( schar_byte ( 0 ) , bv_9AF0 ) ) : BYTE &

    bv_9SP0 : BYTE  &
    bv_9IX0 : BYTE  &
    bv_9IY0 : BYTE &

    ! ( ii , des ) . ( ii : BV16 & des : SCHAR => bv_ireg_plus_d ( ii , des ) : BV16 ) &

    ! ( mmm , ii , des ) . ( mmm : BV16 >-> BYTE & ii : BV16 & des : SCHAR => bv_9ireg_plus_d0 ( mmm , ii , des ) : BYTE )

INITIALISATION

    rgs8 := { ( a0 |-> uchar_byte ( 255 ) ) , ( f0 |-> uchar_byte ( 255 ) ) , ( f_0 |-> uchar_byte ( 255 ) ) , ( a_0 |-> uchar_byte ( 255 ) ) ,
        ( b0 |-> uchar_byte ( 255 ) ) , ( c0 |-> uchar_byte ( 255 ) ) , ( b_0 |-> uchar_byte ( 255 ) ) , ( c_0 |-> uchar_byte ( 255 ) ) ,
        ( d0 |-> uchar_byte ( 255 ) ) , ( e0 |-> uchar_byte ( 255 ) ) , ( d_0 |-> uchar_byte ( 255 ) ) , ( e_0 |-> uchar_byte ( 255 ) ) ,
        ( h0 |-> uchar_byte ( 255 ) ) , ( l0 |-> uchar_byte ( 255 ) ) , ( h_0 |-> uchar_byte ( 255 ) ) , ( l_0 |-> uchar_byte ( 255 ) ) } ||

    pc := 0 || sp := ushort_bv16 ( 65535 ) || ix := ushort_bv16 ( 65535 ) || iy := ushort_bv16 ( 65535 ) ||
    i_ := uchar_byte ( 0 ) || r_ := uchar_byte ( 0 ) ||
    i_o_ports  :: ( BYTE --> { uchar_byte ( 0 ) } ) ||
    iff1 := 0 ||  iff2 := 0 ||
    im := ( 0 |-> 0 )

OPERATIONS

   LD_r_r_ ( rr , rr_ ) =
   PRE rr : id_reg_8 & rr_ : id_reg_8  THEN
      rgs8 ( rr ) := rgs8 ( rr_ ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   LD_r_n_ ( rr , n0 ) =
   PRE rr : id_reg_8 & n0 : SCHAR  THEN
      rgs8 ( rr ) := schar_byte ( n0 ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   LD_r_9HL0 ( rr ) =
   PRE rr : id_reg_8 THEN
      ANY address WHERE address : BV16 &
         address =  byte_bv16 ( rgs8 ( h0 ) , rgs8 ( l0 ) )
      THEN
         rgs8 ( rr ) := mem ( address ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END ;



   LD_r_9IX_d0 ( rr , desloc ) =
   PRE rr : id_reg_8 & desloc : SCHAR
   THEN
      rgs8 ( rr ) := bv_9ireg_plus_d0 ( mem , ix , desloc ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;

   LD_r_9IY_d0 ( rr , desloc ) =
   PRE rr : id_reg_8 & desloc : SCHAR
   THEN
      rgs8 ( rr ) := bv_9ireg_plus_d0 ( mem , iy , desloc ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;

   LD_9HL0_r ( rr ) =
   PRE rr : id_reg_8 & bv_HL /: dom ( stack ) THEN
      updateAddressMem ( bv_HL , rgs8 ( rr ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;



   LD_9IX_d0_r ( desloc , rr ) =
   PRE  desloc : SCHAR & rr : id_reg_8 & bv_ireg_plus_d ( ix , desloc ) /: dom ( stack ) THEN
      updateAddressMem ( bv_ireg_plus_d ( ix , desloc ) , rgs8 ( rr ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   LD_9IY_d0_r ( desloc , rr ) =
   PRE  desloc : SCHAR & rr : id_reg_8 & bv_ireg_plus_d ( iy , desloc ) /: dom ( stack ) THEN
      updateAddressMem ( bv_ireg_plus_d ( iy , desloc ) , rgs8 ( rr ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   LD_9HL0_n ( n0 ) =
   PRE n0 : SCHAR & bv_HL /: dom ( stack )     THEN
      updateAddressMem ( bv_HL , schar_byte ( n0 ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   LD_9IX_d0_n ( desloc , n0 ) =
   PRE  desloc : SCHAR & n0 : SCHAR &  bv_ireg_plus_d ( ix , desloc ) /: dom ( stack ) THEN
      updateAddressMem ( bv_ireg_plus_d ( ix , desloc ) , schar_byte ( n0 ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   LD_9IY_d0_n ( desloc , n0 ) =
   PRE  desloc : SCHAR & n0 : SCHAR & bv_ireg_plus_d ( iy , desloc ) /: dom ( stack ) THEN
      updateAddressMem ( bv_ireg_plus_d ( iy , desloc ) , schar_byte ( n0 ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   LD_A_9BC0  =
   BEGIN
      rgs8 ( a0 ) := mem ( bv_9BC0 ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   LD_A_9DE0  =
   BEGIN
      rgs8 ( a0 ) := bv_9DE0 ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   LD_A_9nn0 ( nn ) =
   PRE nn :  USHORT
   THEN
      rgs8 ( a0 ) := mem ( ushort_bv16 ( nn ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   LD_9BC0_A  =
   PRE           bv_BC /: dom ( stack )
   THEN
      updateAddressMem ( bv_BC , rgs8 ( a0 ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;



   LD_9DE0_A  =
   PRE bv_9DE0 /: dom ( stack )
   THEN
      updateAddressMem ( ( bv_DE ) , rgs8 ( a0 ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   LD_9nn0_A ( nn ) =
   PRE nn : USHORT
      & ushort_bv16 ( nn ) /: dom ( stack )
   THEN
      updateAddressMem ( ushort_bv16 ( nn ) , rgs8 ( a0 ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   /************************************************************************************** 
   ***  			Interruption Group 
   **************************************************************************************/
   LD_A_I =
   ANY  new_interrupt
   WHERE new_interrupt : BYTE
   THEN
      i_ := new_interrupt ||
      rgs8 := rgs8  <+ { a0 |-> new_interrupt , get_new_flag_register_SZ_H_PvNC ( rgs8 , is_negative ( new_interrupt ) , is_zero ( new_interrupt ) , 0 , iff2 , 0 , z_c ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;

   LD_A_R =
   BEGIN
      rgs8 := rgs8  <+ { a0 |-> r_ , get_new_flag_register_SZ_H_PvNC ( rgs8 , is_negative ( r_ ) , is_zero ( r_ ) , 0 , iff2 , 0 , z_c ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;

   LD_I_A =
   BEGIN
      i_ := rgs8 ( a0 ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;

   LD_R_A =
   BEGIN
      r_ := rgs8 ( a0 ) ||
      pc := instruction_next ( pc )
   END ;

   /************************************************************************************** 
   ***  			Load 16 bits Group 
   **************************************************************************************/

   LD_dd_nn ( dd , nn ) =
   PRE dd : id_reg_16 & nn : USHORT  & dd /= AF
   THEN
      IF dd = SP THEN  sp := ushort_bv16 ( nn )
      ELSE
         ANY rh , rl , w1 , w2 WHERE
            rh : id_reg_8 &  rl : id_reg_8 &
            w1 : BYTE & w2 : BYTE &
            rh , rl = REG16_TO_REG8 ( dd ) &
            bv16_byte ( ushort_bv16 ( nn ) ) = w1 , w2
         THEN
            rgs8 := rgs8 <+ { rh |-> w1 , rl |-> w2 } ||
            pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
         END
      END
   END ;


   LD_IX_nn ( nn ) =
   PRE   nn : USHORT
   THEN
      ix := ushort_bv16 ( nn ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;

   LD_IY_nn ( nn ) =
   PRE   nn : USHORT
   THEN
      iy := ushort_bv16 ( nn ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   LD_HL_9nn0 ( nn ) =
   PRE   nn : USHORT
   THEN
      rgs8 := rgs8 <+ { h0 |-> mem ( ushort_bv16 ( add16USHORT ( 0 , nn , 1 ) ) ) , l0 |-> mem ( ushort_bv16 ( nn ) ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   LD_dd_9nn0 ( dd , nn ) =
   PRE dd : id_reg_16 & nn : USHORT  & dd /= AF
   THEN
      r_ := update_refresh_reg ( r_ ) ||
      IF dd = SP THEN
         sp := byte_bv16 ( mem ( ushort_bv16 ( add16USHORT ( 0 , nn , 1 ) ) ) , mem ( ushort_bv16 ( nn ) ) )
      ELSE
         ANY rh , rl , w1 , w2 WHERE
            rh : id_reg_8 &  rl : id_reg_8 &
            w1 : BYTE & w2 : BYTE &
            rh , rl = REG16_TO_REG8 ( dd ) &
            w1 = mem ( ushort_bv16 ( add16USHORT ( 0 , nn , 1 ) ) ) &
            w2 = mem ( ushort_bv16 ( nn ) )
         THEN
            rgs8 := rgs8 <+ { rh |-> w1 , rl |-> w2 } ||
            pc := instruction_next ( pc )
         END
      END
   END ;




   LD_IX_9nn0 ( nn ) =
   PRE   nn : USHORT
   THEN
      ix := byte_bv16 ( mem ( ushort_bv16 ( add16USHORT ( 0 , nn , 1 ) ) ) , mem ( ushort_bv16 ( nn ) ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   LD_IY_9nn0 ( nn ) =
   PRE   nn : USHORT
   THEN
      iy := byte_bv16 ( mem ( ushort_bv16 ( add16USHORT ( 0 , nn , 1 ) ) ) , mem ( ushort_bv16 ( nn ) ) ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   LD_9nn0_HL ( nn ) =
   PRE   nn : USHORT &
      ushort_bv16 ( nn ) /: dom ( stack ) & ushort_bv16 ( add16USHORT ( 0 , nn , 1 ) ) /: dom ( stack )
   THEN
      updateMem ( { ushort_bv16 ( add16USHORT ( 0 , nn , 1 ) ) |-> rgs8 ( h0 ) , ushort_bv16 ( nn ) |-> rgs8 ( l0 ) } ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;




   LD_9nn0_dd ( nn , dd ) =
   PRE dd : id_reg_16 &  dd /= AF & nn : USHORT  &
      schar_byte ( nn ) /: dom ( stack ) &  inc_BYTE ( schar_byte ( nn ) ) /: dom ( stack )
   THEN
      IF dd = SP
      THEN
         ANY vh , vl WHERE
            vh : BYTE & vl : BYTE &
            bv16_byte ( sp ) = vh , vl &
            { ushort_bv16 ( add16USHORT ( 0 , nn , 1 ) ) |-> vh , ushort_bv16 ( nn ) |-> vl } : BV16 >-> BYTE
         THEN
            updateMem ( { ushort_bv16 ( add16USHORT ( 0 , nn , 1 ) ) |-> vh , ushort_bv16 ( nn ) |-> vl } )
            || r_ := update_refresh_reg ( r_ )
         END
      ELSE
         ANY rh , rl , w1 , w2 WHERE
            rh : id_reg_8 &  rl : id_reg_8 &
            w1 : SCHAR & w2 : BYTE &
            rh , rl = REG16_TO_REG8 ( dd ) &
            { ushort_bv16 ( add16USHORT ( 0 , nn , 1 ) ) |-> rgs8 ( rh ) , ushort_bv16 ( nn ) |-> rgs8 ( rl ) } : BV16 >-> BYTE
         THEN
            updateMem ( { ushort_bv16 ( add16USHORT ( 0 , nn , 1 ) ) |-> rgs8 ( rh ) , ushort_bv16 ( nn ) |-> rgs8 ( rl ) } ) ||
            pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
         END
      END
   END ;



   LD_9nn0_IX ( nn ) =
   PRE   nn : USHORT THEN
      ANY h_ix , l_ix WHERE
         h_ix : BYTE & l_ix : BYTE &
         h_ix , l_ix = bv16_byte ( ix )
         & ushort_bv16 ( nn ) /: dom ( stack ) & ushort_bv16 ( add16USHORT ( 0 , nn , 1 ) ) /: dom ( stack )
      THEN
         updateMem ( { ushort_bv16 ( add16USHORT ( 0 , nn , 1 ) ) |-> h_ix , ushort_bv16 ( nn ) |-> l_ix } ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END ;


   LD_9nn0_IY ( nn ) =
   PRE   nn : USHORT   THEN
      ANY h_iy , l_iy WHERE
         h_iy : BYTE & l_iy : BYTE &
         h_iy , l_iy = bv16_byte ( iy ) &
         ushort_bv16 ( nn ) /: dom ( stack ) & ushort_bv16 ( add16USHORT ( 0 , nn , 1 ) ) /: dom ( stack )
      THEN
         updateMem ( { ushort_bv16 ( add16USHORT ( 0 , nn , 1 ) ) |-> h_iy , ushort_bv16 ( nn ) |-> l_iy } ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END ;


   LD_SP_HL  =
   BEGIN
      sp := bv_HL ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;



   LD_SP_IX  =
   BEGIN
      sp := ix ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;



   LD_SP_IY  =
   BEGIN
      sp := iy ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;




   PUSH_qq ( qq ) =
   PRE  qq : id_reg_16 & qq /= SP
   THEN
      ANY
         qqh , qql
      WHERE qqh : id_reg_8 & qql : id_reg_8 &
         REG16_TO_REG8 ( qq ) = qqh , qql &
         { sp_minus_two |-> rgs8 ( qql ) ,
            sp_minus_one |-> rgs8 ( qqh ) } : BV16 --> BYTE
      THEN
         updateStack ( { sp_minus_two |-> rgs8 ( qql ) ,
               sp_minus_one |-> rgs8 ( qqh ) } ) ||
         sp := sp_minus_two ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END ;


   PUSH_IX  =
   ANY
      wh , wl
   WHERE wh : BYTE & wl : BYTE &
      bv16_byte ( ix ) = wh , wl &
      { sp_minus_two |-> wl , sp_minus_one |-> wh } : BV16 --> BYTE
   THEN
      updateStack ( { sp_minus_two |-> wl ,
            sp_minus_one |-> wh } ) ||
      sp := sp_minus_two ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   PUSH_IY  =
   ANY
      wh , wl
   WHERE wh : BYTE & wl : BYTE &
      bv16_byte ( iy ) = wh , wl &
      { sp_minus_two |-> wl , sp_minus_one |-> wh } : BV16 --> BYTE
   THEN
      updateStack ( { sp_minus_two |-> wl ,
            sp_minus_one |-> wh } ) ||
      sp := sp_minus_two ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   POP_qq ( qq ) =
   PRE  qq : id_reg_16 & qq /= SP
   THEN
      ANY
         qqh , qql
      WHERE qqh : id_reg_8 & qql : id_reg_8 &
         REG16_TO_REG8 ( qq ) = qqh , qql &
         { qql |-> mem ( sp_plus_two ) , qqh |-> mem ( sp_plus_one ) } : id_reg_8 --> BYTE
      THEN
         rgs8 := rgs8 <+ { qql |-> mem ( sp_plus_two ) , qqh |-> mem ( sp_plus_one ) } ||
         sp := sp_plus_two ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END ;


   POP_IX  =
   PRE   sp : BV16
   THEN
      ANY
         nw8
      WHERE nw8 : BYTE  &
         byte_bv16 ( mem ( sp_plus_one ) , mem ( sp_plus_two ) ) = nw8
      THEN
         ix := nw8 ||
         sp := sp_plus_two ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END ;



   POP_IY  =
   PRE   sp : BV16
   THEN
      ANY
         nw8
      WHERE nw8 : BYTE  &
         byte_bv16 ( mem ( sp_plus_one ) , mem ( sp_plus_two ) ) = nw8
      THEN
         iy := nw8 ||
         sp := sp_plus_two ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END ;


   EX_DE_HL =
   BEGIN
      rgs8 := rgs8 <+ { d0 |-> rgs8 ( h0 ) , e0 |-> rgs8 ( l0 ) , h0 |-> rgs8 ( d0 ) , l0 |-> rgs8 ( e0 ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;



   EX_AF_AF_ =
   BEGIN
      rgs8 := rgs8 <+ { a0 |-> rgs8 ( a_0 ) , f0 |-> rgs8 ( f_0 ) , a_0 |-> rgs8 ( a0 ) , f_0 |-> rgs8 ( f0 ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;



   EXX =
   BEGIN
      rgs8 := rgs8 <+ { b0 |-> rgs8 ( b_0 ) , c0 |-> rgs8 ( c_0 ) , d0 |-> rgs8 ( d_0 ) , e0 |-> rgs8 ( e_0 ) , h0 |-> rgs8 ( h_0 ) , l0 |-> rgs8 ( l_0 ) ,
         b_0 |-> rgs8 ( b0 ) , c_0 |-> rgs8 ( c0 ) , d_0 |-> rgs8 ( d0 ) , e_0 |-> rgs8 ( e0 ) , h_0 |-> rgs8 ( h0 ) , l_0 |-> rgs8 ( l0 ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   EX_9SP0_HL =
   PRE
      stack <+ { sp_plus_one |-> rgs8 ( h0 ) , sp |-> rgs8 ( l0 ) } <: mem
   THEN
      rgs8 := rgs8 <+ { h0 |-> stack ( sp_plus_one ) , l0 |-> stack ( sp ) } ||
      updateStack ( { sp_plus_one |-> rgs8 ( h0 ) , sp |-> rgs8 ( l0 ) } ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   EX_9SP0_IX =
   ANY wh , wl
   WHERE wh : BYTE & wl : BYTE &
      bv16_byte ( ix ) = wh , wl &
      { sp_plus_one |-> wh , sp |-> wl } : BV16 --> BYTE &
      stack <+ { sp_plus_one |-> wh , sp |-> wl } <: mem
   THEN
      ix := byte_bv16 ( stack ( sp_plus_one ) , stack ( sp ) ) ||
      updateStack ( { sp_plus_one |-> wh , sp |-> wl } ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   EX_9SP0_IY =
   ANY wh , wl
   WHERE wh : BYTE & wl : BYTE &
      bv16_byte ( iy ) = wh , wl &
      { sp_plus_one |-> wh , sp |-> wl } : BV16 --> BYTE &
      stack <+ { sp_plus_one |-> wh , sp |-> wl } <: mem
   THEN
      iy := byte_bv16 ( stack ( sp_plus_one ) , stack ( sp ) ) ||
      updateStack ( { sp_plus_one |-> wh , sp |-> wl } ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;



   LDI =
   ANY  hvn , lvn , dvn , evn , bvn , cvn
   WHERE
      hvn , lvn = bv16_byte ( inc_BV16 ( bv_HL ) ) &
      dvn , evn = bv16_byte ( inc_BV16 ( bv_DE ) ) &
      bvn , cvn = bv16_byte ( dec_BV16 ( bv_BC ) )
   THEN
      updateAddressMem ( bv_DE , bv_9HL0 ) ||
      rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) ,
         ( d0 |-> dvn ) , ( e0 |-> evn ) ,
         ( b0 |-> bvn ) , ( c0 |-> cvn ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , z_s , z_z , 0 , bit_not ( is_zeroUSHORT ( bv16_ushort ( bv_BC ) - 1 ) ) , 0 , z_c ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;



   LDIR =
   ANY  hvn , lvn , dvn , evn , bvn , cvn
   WHERE
      hvn , lvn = bv16_byte ( inc_BV16 ( bv_HL ) ) &
      dvn , evn = bv16_byte ( inc_BV16 ( bv_DE ) ) &
      bvn , cvn = bv16_byte ( dec_BV16 ( bv_BC ) )
   THEN
      updateAddressMem ( bv_DE , bv_9HL0 ) ||
      rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) ,
         ( d0 |-> dvn ) , ( e0 |-> evn ) ,
         ( b0 |-> bvn ) , ( c0 |-> cvn ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , z_s , z_z , 0 , 0 , 0 , z_c ) }
      || r_ := update_refresh_reg ( r_ ) ||
      IF  is_zeroUSHORT ( bv16_ushort ( bv_BC ) - 1 ) = 0 THEN pc := instruction_next ( pc )  END
   END ;


   LDD =
   ANY  hvn , lvn , dvn , evn , bvn , cvn
   WHERE
      hvn , lvn = bv16_byte ( dec_BV16 ( bv_HL ) ) &
      dvn , evn = bv16_byte ( dec_BV16 ( bv_DE ) ) &
      bvn , cvn = bv16_byte ( dec_BV16 ( bv_BC ) )
   THEN
      updateAddressMem ( bv_DE , bv_9HL0 ) ||
      rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) ,
         ( d0 |-> dvn ) , ( e0 |-> evn ) ,
         ( b0 |-> bvn ) , ( c0 |-> cvn ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , z_s , z_z , 0 , bit_not ( is_zeroUSHORT ( bv16_ushort ( bv_BC ) - 1 ) ) , 0 , z_c ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   LDDR =
   ANY  hvn , lvn , dvn , evn , bvn , cvn
   WHERE
      hvn , lvn = bv16_byte ( dec_BV16 ( bv_HL ) ) &
      dvn , evn = bv16_byte ( dec_BV16 ( bv_DE ) ) &
      bvn , cvn = bv16_byte ( dec_BV16 ( bv_BC ) )

   THEN
      updateAddressMem ( bv_DE , bv_9HL0 ) ||
      rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) ,
         ( d0 |-> dvn ) , ( e0 |-> evn ) ,
         ( b0 |-> bvn ) , ( c0 |-> cvn ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , z_s , z_z , 0 , 0 , 0 , z_c ) }
      || r_ := update_refresh_reg ( r_ ) ||
      IF  is_zeroUSHORT ( bv16_ushort ( bv_BC ) - 1 ) = 0 THEN pc := instruction_next ( pc )  END

   END ;


   CPI =
   ANY
      sum , negative , carry , half_carry , zero ,
      hvn , lvn , bvn , cvn
   WHERE
      sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
      substract8UCHAR ( 0 , byte_uchar ( rgs8 ( a0 ) ) , byte_uchar ( bv_9HL0 ) ) = sum , negative , carry , half_carry , zero &
      hvn , lvn = bv16_byte ( inc_BV16 ( bv_HL ) ) &
      bvn , cvn = bv16_byte ( dec_BV16 ( bv_BC ) )

   THEN
      IF zero = 1 THEN
         rgs8 := { h0 |-> hvn , l0 |-> lvn , b0 |-> bvn , c0 |-> cvn
            , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry
               , bit_not ( is_zeroUSHORT ( bv16_ushort ( bv_BC ) - 1 ) ) , 1 , z_c ) }

      END
      ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   CPIR =
   ANY
      sum , negative , carry , half_carry , zero ,
      hvn , lvn , bvn , cvn
   WHERE
      sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
      substract8UCHAR ( 0 , byte_uchar ( rgs8 ( a0 ) ) , byte_uchar ( bv_9HL0 ) ) = sum , negative , carry , half_carry , zero &
      hvn , lvn = bv16_byte ( inc_BV16 ( bv_HL ) ) &
      bvn , cvn = bv16_byte ( dec_BV16 ( bv_BC ) )

   THEN
      IF zero = 1 THEN
         rgs8 := { h0 |-> hvn , l0 |-> lvn , b0 |-> bvn , c0 |-> cvn
            , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry
               , bit_not ( is_zeroUSHORT ( bv16_ushort ( bv_BC ) - 1 ) ) , 1 , z_c ) }

      END
       || r_ := update_refresh_reg ( r_ ) ||
      IF  is_zeroUSHORT ( bv16_ushort ( bv_BC ) - 1 ) = 0 or ( zero = 1 ) THEN pc := instruction_next ( pc ) END
   END ;


   CPD =
   ANY
      sum , negative , carry , half_carry , zero ,
      hvn , lvn , bvn , cvn
   WHERE
      sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
      substract8UCHAR ( 0 , byte_uchar ( rgs8 ( a0 ) ) , byte_uchar ( bv_9HL0 ) ) = sum , negative , carry , half_carry , zero &
      hvn , lvn = bv16_byte ( dec_BV16 ( bv_HL ) ) &
      bvn , cvn = bv16_byte ( dec_BV16 ( bv_BC ) )

   THEN
      IF zero = 1 THEN
         rgs8 := { h0 |-> hvn , l0 |-> lvn , b0 |-> bvn , c0 |-> cvn
            , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry
               , bit_not ( is_zeroUSHORT ( bv16_ushort ( bv_BC ) - 1 ) ) , 1 , z_c ) }

      END
      ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   CPDR =
   ANY
      sum , negative , carry , half_carry , zero ,
      hvn , lvn , bvn , cvn
   WHERE
      sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
      substract8UCHAR ( 0 , byte_uchar ( rgs8 ( a0 ) ) , byte_uchar ( bv_9HL0 ) ) = sum , negative , carry , half_carry , zero &
      hvn , lvn = bv16_byte ( dec_BV16 ( bv_HL ) ) &
      bvn , cvn = bv16_byte ( dec_BV16 ( bv_BC ) )

   THEN
      IF zero = 1 THEN
         rgs8 := { h0 |-> hvn , l0 |-> lvn , b0 |-> bvn , c0 |-> cvn
            , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry
               , bit_not ( is_zeroUSHORT ( bv16_ushort ( bv_BC ) - 1 ) ) , 1 , z_c ) }
      END
      || IF  is_zeroUSHORT ( bv16_ushort ( bv_BC ) - 1 ) = 0 or ( zero = 1 ) THEN pc := instruction_next ( pc )  END ||
      r_ := update_refresh_reg ( r_ )
   END ;


   /************************************************************************************** 
   ****** Arithmetic and Logic Intructions are in the module Z80LoAri - 8 and 16 bits
   ***************************************************************************************/



   /************************************************************************************** 
   ****** General Purpose Arithmetic and CPU control 
   ***************************************************************************************/

   DAA =
   ANY
      result , s0 , z0 , h0 , pv0 , n0 , c0
   WHERE
      result : BYTE & s0 : BIT & z0 : BIT & h0 : BIT & pv0 : BIT & n0 : BIT & c0 : BIT &
      daa_function ( z_n , z_c , z_h , rgs8 ( a0 ) ) = ( result , c0 , h0 ) &
      s0 = bv_get ( result , 7 ) &
      z0 = is_zero ( result ) &
      pv0 = parity_even_BYTE ( result ) &
      n0 = z_n
   THEN
      rgs8 :=  rgs8 <+ { a0 |-> result , get_new_flag_register_SZ_H_PvNC ( rgs8 , s0 , z0 , h0 , pv0 , n0 , c0 ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   CPL =
   ANY  result
   WHERE result : BYTE  & result = complement ( rgs8 ( a0 ) )
   THEN
      rgs8 :=   rgs8 <+ {  ( a0 |-> result ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , z_s , z_z , 1 , z_p , 1 , z_c ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )

   END ;

   NEG =
   ANY
      sum , negative , carry , half_carry , zero
   WHERE  sum : UCHAR & negative : BIT & carry : BIT & half_carry : BIT & zero : BIT &
      sum , negative , carry , half_carry , zero = substract8UCHAR ( 0 , 0 , byte_uchar ( rgs8 ( a0 ) ) )
   THEN
      rgs8 := rgs8 <+ { a0 |-> uchar_byte ( sum ) , get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , carry , 1 , carry ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;

   CCF =
   BEGIN
      rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , z_s , z_z , z_h , z_p , 0 , bit_not ( z_c ) ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;

   SCF =
   BEGIN
      rgs8 := rgs8 <+ { get_new_flag_register_SZ_H_PvNC ( rgs8 , z_s , z_z , z_h , z_p , 0 , 1 ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;

   NOP =
   BEGIN
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;

   HALT =
   BEGIN
      r_ := update_refresh_reg ( r_ )
   END  ;

   DI =
   BEGIN
      iff1 := 0 || iff2 := 0  ||   pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;

   EI =
   BEGIN
      iff1 := 1 || iff2 := 1  ||   pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;

   IM0 =
   BEGIN
      im := ( 0 |-> 0 ) ||   pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;

   IM1 =
   BEGIN
      im := ( 0 |-> 1 ) ||  pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;

   IM2 =
   BEGIN
            im := ( 1 |-> 1 ) ||   pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;

   /************************************************************************************** 
   ******    Rotate and SHIFT GROUP in Z80Man_bitControl
   ***************************************************************************************/


   /************************************************************************************** 
   ****** Input an Output Group
   ***************************************************************************************/

   IN_A_9n0 ( nn ) =
   PRE nn : UCHAR    THEN
      ANY data_in WHERE data_in : SCHAR THEN
         i_o_ports ( uchar_byte ( nn ) ) := schar_byte ( data_in ) ||
         rgs8 ( a0 ) := schar_byte ( data_in ) ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END ;

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
         carry = z_c

      THEN
         i_o_ports ( rgs8 ( c0 ) ) := data_in ||
         rgs8 := rgs8 <+ { ( rr |-> data_in ) ,
            get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
         pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
      END
   END ;


   INI =
   ANY data_in , hvn , lvn ,
      negative , zero , half_carry , pv , add_sub , carry
   WHERE data_in : BYTE &
      negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
      hvn , lvn = bv16_byte ( inc_BV16 ( bv_HL ) ) &

      negative = is_negative ( data_in ) &
      zero = is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) &
      half_carry = z_h &
      pv = parity_even_BYTE ( data_in )   &
      add_sub =  1 &
      carry = z_c

   THEN
      i_o_ports ( rgs8 ( c0 ) ) := data_in ||
      updateAddressMem ( bv_HL , i_o_ports ( rgs8 ( c0 ) ) ) ||
      rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) , ( b0 |-> dec_BYTE ( rgs8 ( b0 ) ) ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )

   END ;


   INIR =

   ANY data_in , hvn , lvn ,
      negative , zero , half_carry , pv , add_sub , carry
   WHERE data_in : BYTE &
      negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
      hvn , lvn = bv16_byte ( inc_BV16 ( bv_HL ) ) &
      negative = is_negative ( data_in ) &
      zero = 1  &
      half_carry = z_h &
      pv = parity_even_BYTE ( data_in )   &
      add_sub =  1 &
      carry = z_c
   THEN
      i_o_ports ( rgs8 ( c0 ) ) := data_in ||
      updateAddressMem ( bv_HL , data_in ) ||
      rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) , ( b0 |-> dec_BYTE ( rgs8 ( b0 ) ) ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
      r_ := update_refresh_reg ( r_ ) ||
      IF  is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) = 1 THEN pc := instruction_next ( pc )  END
   END ;


   IND =
   ANY data_in , hvn , lvn ,
      negative , zero , half_carry , pv , add_sub , carry
   WHERE data_in : BYTE &
      negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
      hvn , lvn = bv16_byte ( dec_BV16 ( bv_HL ) ) &
      negative = is_negative ( data_in ) &
      zero = is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) &
      half_carry = z_h &
      pv = parity_even_BYTE ( data_in )   &
      add_sub =  1 &
      carry = z_c
   THEN
      i_o_ports ( rgs8 ( c0 ) ) := data_in ||
      updateAddressMem ( bv_HL , data_in ) ||
      rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) , ( b0 |-> dec_BYTE ( rgs8 ( b0 ) ) ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   INDR =
   ANY data_in , hvn , lvn ,
      negative , zero , half_carry , pv , add_sub , carry
   WHERE data_in : BYTE &
      negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
      hvn , lvn = bv16_byte ( dec_BV16 ( bv_HL ) ) &
      negative = is_negative ( data_in ) &
      zero = is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) &
      half_carry = z_h &
      pv = parity_even_BYTE ( data_in )   &
      add_sub =  1 &
      carry = z_c
   THEN
      i_o_ports ( rgs8 ( c0 ) ) := data_in ||
      updateAddressMem ( bv_HL , data_in ) ||
      rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) , ( b0 |-> dec_BYTE ( rgs8 ( b0 ) ) ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
      IF  is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) = 1 THEN pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ ) END
   END ;



   OUT_9n0_A ( nn ) =
   PRE nn : UCHAR    THEN
      i_o_ports ( uchar_byte ( nn ) ) := rgs8 ( a0 ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;



   OUT_9C0_r ( rr ) =
   PRE rr : id_reg_8   THEN
      i_o_ports ( rgs8 ( rr ) ) := rgs8 ( c0 ) ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )
   END ;


   OUTI =
   ANY  hvn , lvn ,
      negative , zero , half_carry , pv , add_sub , carry
   WHERE
      negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
      hvn , lvn = bv16_byte ( inc_BV16 ( bv_HL ) ) &

      negative = is_negative ( bv_9HL0 ) &
      zero = is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) &
      half_carry = z_h &
      pv = parity_even_BYTE ( bv_9HL0 ) &
      add_sub =  1 &
      carry = z_c

   THEN
      i_o_ports ( rgs8 ( c0 ) ) := bv_9HL0 ||
      updateAddressMem ( bv_HL , i_o_ports ( rgs8 ( c0 ) ) ) ||
      rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) , ( b0 |-> dec_BYTE ( rgs8 ( b0 ) ) ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )

   END ;


   OUTIR =
   ANY  hvn , lvn ,
      negative , zero , half_carry , pv , add_sub , carry
   WHERE
      negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
      hvn , lvn = bv16_byte ( inc_BV16 ( bv_HL ) ) &

      negative = is_negative ( bv_9HL0 ) &
      zero = is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) &
      half_carry = z_h &
      pv = parity_even_BYTE ( bv_9HL0 ) &
      add_sub =  1 &
      carry = z_c

   THEN
      i_o_ports ( rgs8 ( c0 ) ) := bv_9HL0 ||
      updateAddressMem ( bv_HL , i_o_ports ( rgs8 ( c0 ) ) ) ||
      rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) , ( b0 |-> dec_BYTE ( rgs8 ( b0 ) ) ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
      r_ := update_refresh_reg ( r_ ) ||
      IF  is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) = 1 THEN pc := instruction_next ( pc )  END

   END ;


   OUTD =
   ANY  hvn , lvn ,
      negative , zero , half_carry , pv , add_sub , carry
   WHERE
      negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
      hvn , lvn = bv16_byte ( dec_BV16 ( bv_HL ) ) &

      negative = is_negative ( bv_9HL0 ) &
      zero = is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) &
      half_carry = z_h &
      pv = parity_even_BYTE ( bv_9HL0 ) &
      add_sub =  1 &
      carry = z_c

   THEN
      i_o_ports ( rgs8 ( c0 ) ) := bv_9HL0 ||
      updateAddressMem ( bv_HL , i_o_ports ( rgs8 ( c0 ) ) ) ||
      rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) , ( b0 |-> dec_BYTE ( rgs8 ( b0 ) ) ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
      pc := instruction_next ( pc ) || r_ := update_refresh_reg ( r_ )

   END ;


   OUTDR =
   ANY  hvn , lvn ,
      negative , zero , half_carry , pv , add_sub , carry
   WHERE
      negative : BIT & carry : BIT & half_carry : BIT & zero : BIT     &
      hvn , lvn = bv16_byte ( dec_BV16 ( bv_HL ) ) &

      negative = is_negative ( bv_9HL0 ) &
      zero = is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) &
      half_carry = z_h &
      pv = parity_even_BYTE ( bv_9HL0 ) &
      add_sub =  1 &
      carry = z_c

   THEN
      i_o_ports ( rgs8 ( c0 ) ) := bv_9HL0 ||
      updateAddressMem ( bv_HL , i_o_ports ( rgs8 ( c0 ) ) ) ||
      rgs8 :=  rgs8 <+ { ( h0 |-> hvn ) , ( l0 |-> lvn ) , ( b0 |-> dec_BYTE ( rgs8 ( b0 ) ) ) ,
         get_new_flag_register_SZ_H_PvNC ( rgs8 , negative , zero , half_carry , pv , add_sub , carry ) } ||
      r_ := update_refresh_reg ( r_ ) ||
      IF  is_zero ( dec_BYTE ( rgs8 ( b0 ) ) ) = 1 THEN pc := instruction_next ( pc )  END

   END ;

   /************************************************************************************** 
   ****** Extern Interactions Group
   ***************************************************************************************/

         ext_NMI =
         ANY pc_low , pc_high WHERE pc_low : BYTE & pc_high : BYTE & bv16_byte ( ushort_bv16 ( pc ) ) = ( pc_low , pc_high )
             THEN
              updateStack ( { ( sp_minus_two |-> pc_low ) , ( sp_minus_one |-> pc_high ) } ) || sp := sp_minus_two ||
              pc := 102 || iff1 := 0 || iff2 := iff1 || r_ := update_refresh_reg ( r_ )
         END ;

    ext_INT ( byte_bus ) =
    PRE iff1 = 1  & byte_bus : 0 .. 255 THEN
        ANY pc_low , pc_high
        WHERE pc_low : BYTE & pc_high : BYTE &
              bv16_byte ( ushort_bv16 ( pc ) ) = ( pc_low , pc_high )
        THEN
            IF im = ( 0 |-> 0 ) THEN /* Mode 0 */
                IF  byte_bus = 199  /*RST00H*/ or byte_bus = 207 /*RST08H*/ or
                    byte_bus = 215  /*RST10H*/ or byte_bus = 223  /*RST18H*/ or
                    byte_bus = 231  /*RST20H*/ or byte_bus = 239  /*RST28H*/ or
                    byte_bus = 247  /*RST30H*/ or byte_bus = 255  /*RST38H*/
                THEN

                    pc := byte_bus - 199  ||
                            updateStack ( { stack ( sp_minus_one ) |-> pc_low , stack ( sp_minus_two ) |-> pc_high } ) ||
                            sp := sp_minus_two ||  r_ := update_refresh_reg ( r_ )
                ELSE
                    skip  /* Not implemeted yet the others instructions with codop of 8 bits */
                END
            ELSIF  im =  ( 0 |-> 1 ) THEN /* Mode 1 */
                pc :=  56 /* 56 decimal = 38H */                ||
                updateStack ( { stack ( sp_minus_one ) |-> pc_low , stack ( sp_minus_two ) |-> pc_high } ) ||
                sp := sp_minus_two ||  r_ := update_refresh_reg ( r_ )


            ELSIF    im = ( 1 |-> 1 ) THEN /* Mode 2 */
                pc := bv16_ushort ( byte_bv16 ( i_ , bv_clear ( rotateleft ( uchar_byte ( byte_bus ) ) , 0 ) ) ) ||
                updateStack ( { stack ( sp_minus_one ) |-> pc_low , stack ( sp_minus_two ) |-> pc_high } ) ||
                sp := sp_minus_two ||  r_ := update_refresh_reg ( r_ )
            END
        END
    END ;

    ext_RESET =
          BEGIN

                iff1 := 0 || iff2 := 0 || im := ( 0 |-> 0 ) || pc := 0 || i_ := uchar_byte ( 0 ) || r_ := uchar_byte ( 0 ) ||
                rgs8  := rgs8 <+ { ( a0 |-> uchar_byte ( 255 ) ) , ( f0 |-> uchar_byte ( 255 ) ) } ||
                sp := byte_bv16 ( uchar_byte ( 255 ) , uchar_byte ( 255 ) )

        END



END
