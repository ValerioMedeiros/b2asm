MACHINE
    TYPE_BV16

SEES
    TYPE_BIT

CONCRETE_CONSTANTS
    BV16_INDEX
  , BV16
  , BV16_SET_BIT
  , BV16_COMPLEMENT
  , BV16_ALL_ZEROES
  , BV16_AND
  , BV16_IOR
  , BV16_XOR

PROPERTIES
    BV16_INDEX = ( 0 .. 15 )

  & BV16 = ( BV16_INDEX --> BIT )

  & BV16_SET_BIT : ( BV16 * BV16_INDEX * BIT ) --> BV16
  & ! ( v , i , j , b ) . ( v : BV16 & i : BV16_INDEX & j : BV16_INDEX & b : BIT =>
      ( i /= j => BV16_SET_BIT ( v , i , b ) ( j ) = v ( j ) ) )
  & ! ( v , i , b ) . ( v : BV16 & i : BV16_INDEX & b : BIT => BV16_SET_BIT ( v , i , b ) ( i ) = b )

  & BV16_COMPLEMENT : BV16 --> BV16
  & ! ( v , i ) . ( v : BV16 & i : BV16_INDEX => BV16_COMPLEMENT ( v ) ( i ) = BIT_FLIP ( v ( i ) ) )

  & BV16_ALL_ZEROES : BV16
  & ! ( i ) . ( i : BV16_INDEX => BV16_ALL_ZEROES ( i ) = 0 )

  & BV16_AND : BV16 * BV16 --> BV16
  & ! ( v1 , v2 , i ) . ( v1 : BV16 & v2 : BV16 & i : BV16_INDEX => BV16_AND ( v1 , v2 ) ( i ) = BIT_AND ( v1 ( i ) , v2 ( i ) ) )

  & BV16_IOR : BV16 * BV16 --> BV16
  & ! ( v1 , v2 , i ) . ( v1 : BV16 & v2 : BV16 & i : BV16_INDEX => BV16_IOR ( v1 , v2 ) ( i ) = BIT_IOR ( v1 ( i ) , v2 ( i ) ) )

  & BV16_XOR : BV16 * BV16 --> BV16
  & ! ( v1 , v2 , i ) . ( v1 : BV16 & v2 : BV16 & i : BV16_INDEX => BV16_XOR ( v1 , v2 ) ( i ) = BIT_XOR ( v1 ( i ) , v2 ( i ) ) )

END
