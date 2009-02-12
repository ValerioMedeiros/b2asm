
MACHINE
   ALU

SEES
   TYPES

CONCRETE_CONSTANTS

   is_zero ,
   is_zeroUSHORTINT ,
   is_negative ,

   half ,
   simple_add8UCHAR ,
   add8UCHAR , substract8UCHAR ,

   add16USHORTINT ,
   add_carryUSHORTINT ,
   add_halfcarryUSHORTINT ,
   sub16USHORTINT ,
   sub_carryUSHORTINT ,
   sub_halfcarryUSHORTINT ,

   inc_BYTE ,
   dec_BYTE ,
   inc_BV16 ,
   dec_BV16 ,

   parity_even_BYTE ,

   and , ior , xor ,
   bitclear , bitset , bitget ,
   complement , swap , rotateleft , rotateright

PROPERTIES
   is_zero : BYTE --> BIT &
   is_zero = % ( w1 ) . ( w1 : BYTE | bool_to_bit ( bool ( w1 ( 0 ) + w1 ( 1 ) + w1 ( 2 ) + w1 ( 3 ) + w1 ( 4 ) + w1 ( 5 ) + w1 ( 6 ) + w1 ( 7 ) = 0 ) ) ) &

   is_zeroUSHORTINT : USHORTINT --> BIT &
   is_zeroUSHORTINT = % ( nat1 ) . ( nat1 : USHORTINT | bool_to_bit ( bool ( nat1 = 0 ) ) ) &

   is_negative : BYTE --> BIT &
   is_negative = % ( w1 ) . ( w1 : BYTE | w1 ( 7 ) ) &


   half : UCHAR --> UCHAR &
   half = % ( ww ) . ( ww : UCHAR | ww mod 2 ** 4 ) &


        simple_add8UCHAR  : ( UCHAR * UCHAR ) --> UCHAR &
   simple_add8UCHAR = % ( w1 , w2 ) . ( w1 : UCHAR & w2 : UCHAR | ( w1 + w2 ) mod 2 ** 8 ) &


   /* add (w1, w2) = (sum, is_negative, carry, half_carry, zero) */
   add8UCHAR : ( BIT * UCHAR * UCHAR ) --> ( UCHAR * BIT * BIT * BIT * BIT ) &
   add8UCHAR = % ( carry , w1 , w2 ) . ( carry : BIT & w1 : UCHAR & w2 : UCHAR |
      ( ( ( carry + w1 + w2 ) mod 256 ) ,
      bool_to_bit ( bool ( carry + uchar_schar ( w1 ) + uchar_schar ( w2 ) < 0 ) ) ,
      bool_to_bit ( bool ( carry + w1 + w2 > UCHAR_MAX ) ) ,
      bool_to_bit ( bool ( carry + half ( w1 ) + half ( w2 ) >= 2 ** 4 ) ) ,
      bool_to_bit ( bool ( ( ( carry + w1 + w2 ) mod UCHAR_MAX - 1 ) = 0 ) ) ) ) &


   /* substract8UCHAR (w1, w2) = (result, is_negative, carry, half_carry, zero) */
   substract8UCHAR : ( BIT * UCHAR * UCHAR ) --> ( UCHAR * BIT * BIT * BIT * BIT ) &
   substract8UCHAR = % ( carry , w1 , w2 ) . ( carry : BIT & w1 : UCHAR & w2 : UCHAR |
      ( ( ( carry + w1 - w2 ) mod 256 )  ,
      bool_to_bit ( bool ( carry + uchar_schar ( w1 ) - uchar_schar ( w2 ) < 0 ) ) ,
      bool_to_bit ( bool ( carry + w1 - w2 > UCHAR_MAX ) ) ,
      bool_to_bit ( bool ( carry + half ( w1 ) - half ( w2 ) >= 2 ** 4 ) ) ,
      bool_to_bit ( bool ( ( ( carry + w1 - w2 ) mod UCHAR_MAX - 1 ) = 0 ) ) ) ) &


   add16USHORTINT : ( BIT * USHORTINT * USHORTINT ) --> ( USHORTINT ) &
   add16USHORTINT = % ( b1 , w1 , w2 ) . ( b1 : BIT & w1 : USHORTINT & w2 : USHORTINT | ( b1 + w1 + w2 ) mod 65536 ) &

   add_carryUSHORTINT : ( BIT * USHORTINT * USHORTINT ) --> BIT &
   add_carryUSHORTINT = % ( b1 , w1 , w2 ) . ( b1 : BIT & w1 : USHORTINT & w2 : USHORTINT |
      bool_to_bit ( bool ( ( b1 + w1 + w2 ) > 2 ** 16 ) ) ) &

   add_halfcarryUSHORTINT : ( BIT * USHORTINT * USHORTINT ) --> BIT &
   add_halfcarryUSHORTINT = % ( b1 , w1 , w2 ) . ( b1 : BIT & w1 : USHORTINT & w2 : USHORTINT |
      bool_to_bit ( bool ( ( b1 + ( w1 ) mod 2 ** 12 + ( w2 ) mod 2 ** 12 ) > 2 ** 12 ) ) ) &

   sub16USHORTINT : ( BIT * USHORTINT * USHORTINT ) --> ( USHORTINT ) &
   sub16USHORTINT = % ( b1 , w1 , w2 ) . ( b1 : BIT & w1 : USHORTINT & w2 : USHORTINT | ( w1 - w2 - b1 ) mod 65536 ) &

   sub_carryUSHORTINT : ( BIT * USHORTINT * USHORTINT ) --> BIT &
   sub_carryUSHORTINT = % ( b1 , w1 , w2 ) . ( b1 : BIT & w1 : USHORTINT & w2 : USHORTINT |
      bool_to_bit ( bool ( ( w1 - w2 - b1 ) > 2 ** 16 ) ) ) &

   sub_halfcarryUSHORTINT : ( BIT * USHORTINT * USHORTINT ) --> BIT &
   sub_halfcarryUSHORTINT = % ( b1 , w1 , w2 ) . ( b1 : BIT & w1 : USHORTINT & w2 : USHORTINT |
      bool_to_bit ( bool ( ( ( w1 ) mod 2 ** 12 - ( w2 ) mod 2 ** 12 ) - b1 > 2 ** 12 ) ) ) &



   inc_BYTE :  BYTE  --> BYTE &
   inc_BYTE = % ( w1 ) . ( w1 : BYTE | uchar_to_byte ( ( byte_to_uchar ( w1 ) + 1 ) mod 256 ) ) &

   dec_BYTE :  BYTE  --> BYTE &
   dec_BYTE = % ( w1 ) . ( w1 : BYTE | uchar_to_byte ( ( byte_to_uchar ( w1 ) - 1 ) mod 256 ) ) &


   inc_BV16 :  BV16  --> BV16 &
   inc_BV16 = % ( w1 ) . ( w1 : BV16 | ushortint_to_bv16 ( ( bv16_to_ushortint ( w1 ) + 1 ) mod 65536 ) ) &

   dec_BV16 :  BYTE  --> BYTE &
   dec_BV16 = % ( w1 ) . ( w1 : BV16 | ushortint_to_bv16 ( ( bv16_to_ushortint ( w1 ) - 1 ) mod 65536 ) ) &


   parity_even_BYTE : BIT_VECTOR --> BIT        &
   parity_even_BYTE = % ( bv ) . ( bv : BIT_VECTOR | ( 1 - ( bv_get ( bv , 0 ) +
               bv_get ( bv , 1 ) + bv_get ( bv , 2 ) + bv_get ( bv , 3 ) + bv_get ( bv , 4 ) + bv_get ( bv , 5 ) +
               bv_get ( bv , 7 ) )                                         mod 2 ) ) &

   and : ( BYTE * BYTE ) --> BYTE &
   and = % ( bt1 , bt2 ) . ( bt1 : BYTE & bt2 : BYTE | bv_and ( bt1 , bt2 ) ) &

   ior : ( BYTE * BYTE ) --> BYTE &
   ior = % ( bt1 , bt2 ) . ( bt1 : BYTE & bt2 : BYTE | bv_or ( bt1 , bt2 ) ) &

   xor : ( BYTE * BYTE ) --> BYTE &
   xor = % ( bt1 , bt2 ) . ( bt1 : BYTE & bt2 : BYTE | bv_xor ( bt1 , bt2 ) ) &


   bitget : ( BYTE * BYTE_INDEX ) --> BIT &
   bitget = % ( bt1 , ii ) . ( bt1 : BYTE & ii : BYTE_INDEX | bt1 ( ii ) ) &

   bitset : ( BYTE * BYTE_INDEX ) --> BYTE &
   ! ( ww , ii ) . ( ww : BYTE & ii : BYTE_INDEX => bitset ( ww , ii ) = bv_set ( ww , ii ) ) &

   bitclear : ( BYTE * BYTE_INDEX ) --> BYTE &
   ! ( ww , ii , bb ) . ( ww : BYTE & ii : BYTE_INDEX & bb : BIT => bitclear ( ww , ii ) = bv_clear ( ww , ii ) ) &

   complement : BYTE --> BYTE &
   complement = % ( bt ) . ( bt : BYTE | bv_not ( bt ) ) &


   swap : BYTE --> BYTE &
   swap = % ( bt ) . ( bt : BYTE | { 0 |-> bt ( 4 ) , 1 |-> bt ( 5 ) , 2 |-> bt ( 6 ) , 3 |-> bt ( 7 ) , 4 |-> bt ( 0 ) , 5 |-> bt ( 1 ) , 6 |-> bt ( 2 ) , 7 |-> bt ( 3 ) } ) &

   rotateleft : BYTE --> BYTE  &
   rotateleft = % ( bv ) . ( bv : BYTE | { 0 |-> bv ( 7 ) , 1 |-> bv ( 0 ) , 2 |-> bv ( 1 ) , 3 |-> bv ( 2 ) , 4 |-> bv ( 3 ) , 5 |-> bv ( 4 ) , 6 |-> bv ( 5 ) , 7 |-> bv ( 6 ) } ) &


   rotateright : BYTE --> BYTE  &
   rotateright = % ( bv ) . ( bv : BYTE | { 0 |-> bv ( 1 ) , 1 |-> bv ( 2 ) , 2 |-> bv ( 3 ) , 3 |-> bv ( 4 ) , 4 |-> bv ( 5 ) , 5 |-> bv ( 6 ) , 6 |-> bv ( 7 ) , 7 |-> bv ( 0 ) } )



ASSERTIONS

   dom ( add8UCHAR ) = BIT * UCHAR * UCHAR &
   ran ( add8UCHAR ) : POW ( UCHAR * BIT * BIT * BIT * BIT ) &
   dom ( substract8UCHAR ) = BIT * UCHAR * UCHAR &
   ran ( substract8UCHAR ) : POW ( UCHAR * BIT * BIT * BIT * BIT ) &

   dom ( and ) = BYTE * BYTE &
   ran ( and ) <: BYTE &

   dom ( ior ) = BYTE * BYTE &
   ran ( ior ) <: BYTE &

   dom ( xor ) = BYTE * BYTE &
   ran ( xor ) <: BYTE &

   dom ( complement ) = BYTE &
   ran ( complement ) <: BYTE &

   dom ( swap ) = BYTE &
   ran ( swap ) <: BYTE &

   dom ( rotateleft ) = BYTE &
   ran ( rotateleft ) <: BYTE &

   dom ( rotateright ) = BYTE &
   ran ( rotateright ) <: BYTE &


   ! ( vec , in0 ) . ( vec : BYTE & in0 : 0 .. 7
      =>  bitget ( vec , in0 ) = vec ( in0 ) ) &

   ! ( x0 , x1 ) . ( x0 : UCHAR & x1 : UCHAR => simple_add8UCHAR ( x0 , x1 ) : UCHAR )


END
