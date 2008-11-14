MACHINE
   ALU

SEES
   TYPES

CONCRETE_CONSTANTS
   add16SCHAR ,
   halfSCHAR ,
   add8SCHAR , substract8SCHAR ,
   andSCHAR , iorSCHAR , xorSCHAR ,
   bitclearSCHAR , bitsetSCHAR , bitgetSCHAR ,
   complementSCHAR , swapSCHAR , rotateleftSCHAR , rotaterightSCHAR ,


   is_zeroSCHAR ,
   is_negative


PROPERTIES

        is_zeroSCHAR : SCHAR --> BIT &
        is_zeroSCHAR = % ( w1 ) . ( w1 : SCHAR | bool_to_bit ( bool ( w1 = 0 ) ) ) &

        is_negative : SCHAR --> BIT &
        is_negative = % ( w1 ) . ( w1 : SCHAR | bool_to_bit ( bool ( w1 < 0 ) ) ) &

        parity_bit_from_BYTE : BIT_VECTOR --> BIT
        & parity_bit_from_BYTE = % ( bv ) . ( bv : BIT_VECTOR | ( SIGMA idx . ( idx : dom ( bv ) | bv ( idx ) ) ) mod 2 ) &


        halfSCHAR : SCHAR --> SCHAR &
        halfSCHAR = % ( ww ) . ( ww : SCHAR | ww mod 16 ) &


        /* add (w1, w2) = (sum, is_negative, carry, digit_carry, zero) */
        add8SCHAR : ( SCHAR * SCHAR ) --> ( SCHAR * BOOL * BOOL * BOOL * BOOL ) &
        add8SCHAR = % ( w1 , w2 ) . ( w1 : UCHAR & w2 : UCHAR & w1 + w2 < - 128 |
                  ( 256 - ( w1 + w2 ) ,
                  bool ( w1 + w2 < 0 )     ,
                  TRUE ,
                  bool ( halfSCHAR ( w1 ) + halfSCHAR ( w2 ) >= 16 ) ,
                  FALSE ) )
         \/  % ( w1 , w2 ) . ( w1 : UCHAR & w2 : UCHAR & not ( w1 + w2 < - 128 ) |
                   ( ( ( w1 + w2 ) mod 128 ) , /*pg 96 manual pdf - la nao parece esta errado o overflow superior da soma  */
                  bool ( w1 + w2 < 0 )     ,
                  bool ( w1 + w2 > 127 ) ,
                  bool ( halfSCHAR ( w1 ) + halfSCHAR ( w2 ) >= 16 ) ,
                  bool ( w1 = 0 ) ) ) &

        /* substract8SCHAR (w1, w2) = (sum, is_negative, carry, digit_carry, zero) */
        substract8SCHAR : ( SCHAR * SCHAR ) --> ( SCHAR * BOOL * BOOL * BOOL * BOOL ) &
        substract8SCHAR = % ( w1 , w2 ) . ( w1 : UCHAR & w2 : UCHAR & w1 - w2 < - 128 |
                 ( 256 - ( w1 - w2 ) ,
                  bool ( w1 - w2 < 0 )     ,
                  TRUE ,
                  bool ( halfSCHAR ( w1 ) - halfSCHAR ( w2 ) >= 16 ) ,
                  FALSE ) )
         \/  % ( w1 , w2 ) . ( w1 : UCHAR & w2 : UCHAR & not ( w1 - w2 < - 128 ) |
                 ( ( ( w1 - w2 ) mod 128 ) , /*pg 96 manual pdf - la nao parece estar errado o overflow superior da soma  */
                  bool ( w1 - w2 < 0 )     ,
                  bool ( w1 - w2 > 127 ) ,
                  bool ( halfSCHAR ( w1 ) - halfSCHAR ( w2 ) >= 16 ) ,
                  bool ( w1 = 0 ) ) ) &





        /* op1    op2        Res_sum      */
        /*add8SCHAR : ( SCHAR * SCHAR ) --> ( SCHAR )
	& ! ( w1 , w2 , sum ) .
     ( w1 : SCHAR & w2 : SCHAR & sum : SCHAR & sum = w1 + w2 =>
      ( ( sum : SCHAR  =>  add8SCHAR ( w1 , w2 ) = ( sum ) ) &
        ( sum < - 128  =>  add8SCHAR ( w1 , w2 ) = ( sum + 256 ) ) &
        ( sum > 127    =>  add8SCHAR ( w1 , w2 ) = ( sum - 256 ) )
       )
     )    &
*/

  add16SCHAR : ( SSHORTINT * SSHORTINT ) --> ( SSHORTINT * BOOL * BOOL ) &
   ! ( w1 , w2 , sum ) .
     ( w1 : SSHORTINT & w2 : SSHORTINT & sum : NATURAL & sum = w1 + w2 =>
      ( ( sum <= 65535 => add16SCHAR ( w1 , w2 ) = ( sum , bool ( sum = 0 ) , FALSE ) ) &
       ( 65536 <= sum => add16SCHAR ( w1 , w2 ) = ( sum - 65536 , bool ( sum = 65536 ) , TRUE ) ) ) )



/*
  & substract8SCHAR : SCHAR * SCHAR --> SCHAR * BOOL * BOOL
  & ! ( w1 , w2 , diff ) .
     ( w1 : SCHAR & w2 : SCHAR & diff : INTEGER & diff = w1 - w2 =>
      ( ( diff < 0 => substract8SCHAR ( w1 , w2 ) = ( diff + 256 , FALSE , TRUE ) ) &
       ( diff >= 0 => substract8SCHAR ( w1 , w2 ) = ( diff , bool ( diff = 0 ) , FALSE ) ) ) )
*/
  & andSCHAR : ( SCHAR * SCHAR ) --> ( SCHAR * BOOL )
  & ! ( w1 , w2 , w0 ) .
     ( w1 : SCHAR & w2 : SCHAR & w0 : SCHAR & w0 = BYTE_TO_SCHAR ( bv_and ( SCHAR_TO_BYTE ( w1 ) , SCHAR_TO_BYTE ( w2 ) ) ) =>
      andSCHAR ( w1 , w2 ) = ( w0 , bool ( w0 = 0 ) ) )

  & iorSCHAR : ( SCHAR * SCHAR ) --> ( SCHAR * BOOL )
  & ! ( w1 , w2 , w0 ) .
     ( w1 : SCHAR & w2 : SCHAR & w0 : SCHAR & w0 = BYTE_TO_SCHAR ( bv_or ( SCHAR_TO_BYTE ( w1 ) , SCHAR_TO_BYTE ( w2 ) ) ) =>
      iorSCHAR ( w1 , w2 ) = ( w0 , bool ( w0 = 0 ) ) )

  & xorSCHAR : ( SCHAR * SCHAR ) --> ( SCHAR * BOOL )
  & ! ( w1 , w2 , w0 ) .
     ( w1 : SCHAR & w2 : SCHAR & w0 : SCHAR & w0 = BYTE_TO_SCHAR ( bv_xor ( SCHAR_TO_BYTE ( w1 ) , SCHAR_TO_BYTE ( w2 ) ) ) =>
      xorSCHAR ( w1 , w2 ) = ( w0 , bool ( w0 = 0 ) ) )

  /*& bitgetSCHAR : SCHAR * SCHAR_POSITION --> BIT
  & bitgetSCHAR = %( w0 , i0 ) . ( w0 : SCHAR & i0 : SCHAR_POSITION | SCHAR_TO_BYTE ( w0 ) ( i0 ) ) */

  & bitgetSCHAR : SCHAR * SCHAR_POSITION --> BIT
  & bitgetSCHAR = % ( w0 , i0 ) . ( w0 : SCHAR & i0 : SCHAR_POSITION &  i0 = 7 | ( 1 - ( ( w0 + 2 ** 7 ) / 128 ) mod 2 ) ) \/
                                  % ( w0 , i0 ) . ( w0 : SCHAR & i0 : SCHAR_POSITION & i0 /= 7 |  ( ( w0 + 2 ** 7 ) / ( 2 ** ( i0 ) ) ) mod 2 )


  /*& bitsetSCHAR : SCHAR * SCHAR_POSITION --> SCHAR
  & ! ( w0 , i0 ) . ( w0 : SCHAR & i0 : SCHAR_POSITION => bitsetSCHAR ( w0 , i0 ) = BYTE_TO_SCHAR ( bv_set ( SCHAR_TO_BYTE ( w0 ) , i0 ) ) )*/

  & bitsetSCHAR : SCHAR * SCHAR_POSITION --> SCHAR
  &  bitsetSCHAR =
     % ( w0 , i0 ) . ( w0 : SCHAR & i0 : SCHAR_POSITION & bitgetSCHAR ( w0 , i0 ) = 1 | w0 ) \/
     % ( w0 , i0 ) . ( w0 : SCHAR & i0 : SCHAR_POSITION & bitgetSCHAR ( w0 , i0 ) /= 1 | w0 + 2 ** i0 )



/*  & bitclearSCHAR : SCHAR * SCHAR_POSITION --> SCHAR
  & ! ( w0 , i0 , b0 ) . ( w0 : SCHAR & i0 : SCHAR_POSITION & b0 : BIT => bitclearSCHAR ( w0 , i0 ) = BYTE_TO_SCHAR ( bv_clear ( SCHAR_TO_BYTE ( w0 ) , i0 ) ) )*/

  & bitclearSCHAR : SCHAR * SCHAR_POSITION --> SCHAR
  & bitclearSCHAR =
     % ( w0 , i0 ) . ( w0 : SCHAR & i0 : SCHAR_POSITION & bitgetSCHAR ( w0 , i0 ) = 0 | w0 )
  \/ % ( w0 , i0 ) . ( w0 : SCHAR & i0 : SCHAR_POSITION & bitgetSCHAR ( w0 , i0 ) /= 0 & i0 = 7 | w0 + 128 )
  \/ % ( w0 , i0 ) . ( w0 : SCHAR & i0 : SCHAR_POSITION & bitgetSCHAR ( w0 , i0 ) /= 0 & i0 /= 7 | w0 - 2 ** i0 )

 /* & complementSCHAR : SCHAR --> SCHAR
  & ! ( w0 ) . ( w0 : SCHAR => complementSCHAR ( w0 ) = BYTE_TO_SCHAR ( bv_not ( SCHAR_TO_BYTE ( w0 ) ) ) )*/


  & complementSCHAR : SCHAR --> SCHAR
  & complementSCHAR =   % ( w0 ) . ( w0 : SCHAR | 255 - ( w0 ) )


  & swapSCHAR : SCHAR --> SCHAR
  & ! ( w0 , v0 ) . ( w0 : SCHAR & v0 : BIT_VECTOR =>
     ( v0 = SCHAR_TO_BYTE ( w0 ) =>
      swapSCHAR ( w0 ) = BYTE_TO_SCHAR ( { 0 |-> v0 ( 4 ) , 1 |-> v0 ( 5 ) , 2 |-> v0 ( 6 ) , 3 |-> v0 ( 7 ) , 4 |-> v0 ( 0 ) , 5 |-> v0 ( 1 ) , 6 |-> v0 ( 2 ) , 7 |-> v0 ( 3 ) } ) ) )

  & rotateleftSCHAR : SCHAR --> SCHAR * BOOL
  & ! ( w0 , v0 ) . ( w0 : SCHAR & v0 : BIT_VECTOR =>
     ( v0 = SCHAR_TO_BYTE ( w0 ) =>
      rotateleftSCHAR ( w0 ) = BYTE_TO_SCHAR ( { 0 |-> v0 ( 7 ) , 1 |-> v0 ( 0 ) , 2 |-> v0 ( 1 ) , 3 |-> v0 ( 2 ) , 4 |-> v0 ( 3 ) , 5 |-> v0 ( 4 ) , 6 |-> v0 ( 5 ) , 7 |-> v0 ( 6 ) } ) , bool ( v0 ( 7 ) = 1 ) ) )

  & rotaterightSCHAR : SCHAR --> SCHAR * BOOL
  & ! ( w0 , v0 ) . ( w0 : SCHAR & v0 : BIT_VECTOR =>
     ( v0 = SCHAR_TO_BYTE ( w0 ) =>
      rotaterightSCHAR ( w0 ) = BYTE_TO_SCHAR ( { 0 |-> v0 ( 1 ) , 1 |-> v0 ( 2 ) , 2 |-> v0 ( 3 ) , 3 |-> v0 ( 4 ) , 4 |-> v0 ( 5 ) , 5 |-> v0 ( 6 ) , 6 |-> v0 ( 7 ) , 7 |-> v0 ( 0 ) } ) , bool ( v0 ( 0 ) = 1 ) ) )

ASSERTIONS
   dom ( add8SCHAR ) = SCHAR * SCHAR &
   ran ( add8SCHAR ) : POW ( SCHAR * BOOL * BOOL * BOOL * BOOL ) &
   dom ( substract8SCHAR ) = SCHAR * SCHAR &
   ran ( substract8SCHAR ) : POW ( SCHAR * BOOL * BOOL * BOOL * BOOL ) &
   dom ( andSCHAR ) = SCHAR * SCHAR &
   ran ( andSCHAR ) <: SCHAR * BOOL &
   dom ( iorSCHAR ) = SCHAR * SCHAR &
   ran ( iorSCHAR ) <: SCHAR * BOOL &
   dom ( xorSCHAR ) = SCHAR * SCHAR &
   ran ( xorSCHAR ) <: SCHAR * BOOL &
   dom ( bitclearSCHAR ) = SCHAR * SCHAR_POSITION &
   ran ( bitclearSCHAR ) <: SCHAR &
   dom ( bitsetSCHAR ) = SCHAR * SCHAR_POSITION &
   ran ( bitsetSCHAR ) <: SCHAR &
   dom ( bitgetSCHAR ) = SCHAR * SCHAR_POSITION &
   ran ( bitgetSCHAR ) <: BIT &
   dom ( complementSCHAR ) = SCHAR &
   ran ( complementSCHAR ) <: SCHAR &
   dom ( swapSCHAR ) = SCHAR &
   ran ( swapSCHAR ) <: SCHAR &
   ran ( rotateleftSCHAR ) <: SCHAR * BOOL &
   dom ( rotateleftSCHAR ) = SCHAR &
   dom ( rotaterightSCHAR ) = SCHAR &
   ran ( rotaterightSCHAR ) <: SCHAR * BOOL &
   ! ( w0 , idx ) . ( w0 : SCHAR & idx : SCHAR_POSITION =>
      bitgetSCHAR ( w0 , idx ) = SCHAR_TO_BYTE ( w0 ) ( idx ) ) &
   max ( SCHAR ) = 127
END
