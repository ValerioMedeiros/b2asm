MACHINE
   ALU

SEES
   TYPES

CONCRETE_CONSTANTS
   add16 ,
   add , substract ,
   and , ior , xor ,
   bitclear , bitset , bitget ,
   complement , swap , rotateleft , rotateright

PROPERTIES

    add16 : ( WORD16 * WORD16 ) --> ( WORD16 * BOOL * BOOL )
  & ! ( w1 , w2 , sum ) .
     ( w1 : WORD16 & w2 : WORD16 & sum : NATURAL & sum = w1 + w2 =>
      ( ( sum <= 65535 => add16 ( w1 , w2 ) = ( sum , bool ( sum = 0 ) , FALSE ) ) &
       ( 65536 <= sum => add16 ( w1 , w2 ) = ( sum - 65536 , bool ( sum = 65536 ) , TRUE ) ) ) )
  &


    add : ( WORD8 * WORD8 ) --> ( WORD8 * BOOL * BOOL )
  & ! ( w1 , w2 , sum ) .
     ( w1 : WORD8 & w2 : WORD8 & sum : NATURAL & sum = w1 + w2 =>
      ( ( sum <= 255 => add ( w1 , w2 ) = ( sum , bool ( sum = 0 ) , FALSE ) ) &
       ( 256 <= sum => add ( w1 , w2 ) = ( sum - 256 , bool ( sum = 256 ) , TRUE ) ) ) )

  & substract : WORD8 * WORD8 --> WORD8 * BOOL * BOOL
  & ! ( w1 , w2 , diff ) .
     ( w1 : WORD8 & w2 : WORD8 & diff : INTEGER & diff = w1 - w2 =>
      ( ( diff < 0 => substract ( w1 , w2 ) = ( diff + 256 , FALSE , TRUE ) ) &
       ( diff >= 0 => substract ( w1 , w2 ) = ( diff , bool ( diff = 0 ) , FALSE ) ) ) )

  & and : ( WORD8 * WORD8 ) --> ( WORD8 * BOOL )
  & ! ( w1 , w2 , w ) .
     ( w1 : WORD8 & w2 : WORD8 & w : WORD8 & w = BV8_TO_WORD8 ( BV8_AND ( WORD8_TO_BV8 ( w1 ) , WORD8_TO_BV8 ( w2 ) ) ) =>
      and ( w1 , w2 ) = ( w , bool ( w = 0 ) ) )

  & ior : ( WORD8 * WORD8 ) --> ( WORD8 * BOOL )
  & ! ( w1 , w2 , w ) .
     ( w1 : WORD8 & w2 : WORD8 & w : WORD8 & w = BV8_TO_WORD8 ( BV8_IOR ( WORD8_TO_BV8 ( w1 ) , WORD8_TO_BV8 ( w2 ) ) ) =>
      ior ( w1 , w2 ) = ( w , bool ( w = 0 ) ) )

  & xor : ( WORD8 * WORD8 ) --> ( WORD8 * BOOL )
  & ! ( w1 , w2 , w ) .
     ( w1 : WORD8 & w2 : WORD8 & w : WORD8 & w = BV8_TO_WORD8 ( BV8_XOR ( WORD8_TO_BV8 ( w1 ) , WORD8_TO_BV8 ( w2 ) ) ) =>
      xor ( w1 , w2 ) = ( w , bool ( w = 0 ) ) )

  & bitget : WORD8 * WORD8_POSITION --> BIT
  & ! ( w , i ) . ( w : WORD8 & i : WORD8_POSITION => bitget ( w , i ) = WORD8_TO_BV8 ( w ) ( i ) )

  & bitset : WORD8 * WORD8_POSITION --> WORD8
  & ! ( w , i ) . ( w : WORD8 & i : WORD8_POSITION => bitset ( w , i ) = BV8_TO_WORD8 ( BV8_SET_BIT ( WORD8_TO_BV8 ( w ) , i , 1 ) ) )

  & bitclear : WORD8 * WORD8_POSITION --> WORD8
  & ! ( w , i , b ) . ( w : WORD8 & i : WORD8_POSITION & b : BIT => bitclear ( w , i ) = BV8_TO_WORD8 ( BV8_SET_BIT ( WORD8_TO_BV8 ( w ) , i , 0 ) ) )

  & complement : WORD8 --> WORD8
  & ! ( w ) . ( w : WORD8 => complement ( w ) = BV8_TO_WORD8 ( BV8_COMPLEMENT ( WORD8_TO_BV8 ( w ) ) ) )

  & swap : WORD8 --> WORD8
  & ! ( w , v ) . ( w : WORD8 & v : BV8 =>
     ( v = WORD8_TO_BV8 ( w ) =>
      swap ( w ) = BV8_TO_WORD8 ( { 0 |-> v ( 4 ) , 1 |-> v ( 5 ) , 2 |-> v ( 6 ) , 3 |-> v ( 7 ) , 4 |-> v ( 0 ) , 5 |-> v ( 1 ) , 6 |-> v ( 2 ) , 7 |-> v ( 3 ) } ) ) )

  & rotateleft : WORD8 --> WORD8 * BOOL
  & ! ( w , v ) . ( w : WORD8 & v : BV8 =>
     ( v = WORD8_TO_BV8 ( w ) =>
      rotateleft ( w ) = BV8_TO_WORD8 ( { 0 |-> v ( 7 ) , 1 |-> v ( 0 ) , 2 |-> v ( 1 ) , 3 |-> v ( 2 ) , 4 |-> v ( 3 ) , 5 |-> v ( 4 ) , 6 |-> v ( 5 ) , 7 |-> v ( 6 ) } ) , bool ( v ( 7 ) = 1 ) ) )

  & rotateright : WORD8 --> WORD8 * BOOL
  & ! ( w , v ) . ( w : WORD8 & v : BV8 =>
     ( v = WORD8_TO_BV8 ( w ) =>
      rotateright ( w ) = BV8_TO_WORD8 ( { 0 |-> v ( 1 ) , 1 |-> v ( 2 ) , 2 |-> v ( 3 ) , 3 |-> v ( 4 ) , 4 |-> v ( 5 ) , 5 |-> v ( 6 ) , 6 |-> v ( 7 ) , 7 |-> v ( 0 ) } ) , bool ( v ( 0 ) = 1 ) ) )

ASSERTIONS
   dom ( add ) = WORD8 * WORD8 &
   ran ( add ) <: WORD8 * BOOL * BOOL &
   dom ( substract ) = WORD8 * WORD8 &
   ran ( substract ) <: WORD8 * BOOL * BOOL &
   dom ( and ) = WORD8 * WORD8 &
   ran ( and ) <: WORD8 * BOOL &
   dom ( ior ) = WORD8 * WORD8 &
   ran ( ior ) <: WORD8 * BOOL &
   dom ( xor ) = WORD8 * WORD8 &
   ran ( xor ) <: WORD8 * BOOL &
   dom ( bitclear ) = WORD8 * WORD8_POSITION &
   ran ( bitclear ) <: WORD8 &
   dom ( bitset ) = WORD8 * WORD8_POSITION &
   ran ( bitset ) <: WORD8 &
   dom ( bitget ) = WORD8 * WORD8_POSITION &
   ran ( bitget ) <: BIT &
   dom ( complement ) = WORD8 &
   ran ( complement ) <: WORD8 &
   dom ( swap ) = WORD8 &
   ran ( swap ) <: WORD8 &
   ran ( rotateleft ) <: WORD8 * BOOL &
   dom ( rotateleft ) = WORD8 &
   dom ( rotateright ) = WORD8 &
   ran ( rotateright ) <: WORD8 * BOOL

END
