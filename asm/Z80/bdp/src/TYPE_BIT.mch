MACHINE
   TYPE_BIT

CONCRETE_CONSTANTS
    BIT
  , BIT_FLIP
  , BIT_AND
  , BIT_IOR
  , BIT_XOR

PROPERTIES
    BIT = 0 .. 1
  & BIT_FLIP : BIT --> BIT
  & ! ( b ) . ( b : BIT => BIT_FLIP ( b ) = 1 - b )
  & BIT_AND : BIT * BIT --> BIT
  & ! ( b1 , b2 ) . ( b1 : BIT & b2 : BIT => ( ( BIT_AND ( b1 , b2 ) = 1 ) <=> ( b1 = 1 ) & ( b2 = 1 ) ) )
  & BIT_IOR : BIT * BIT --> BIT
  & ! ( b1 , b2 ) . ( b1 : BIT & b2 : BIT => ( ( BIT_IOR ( b1 , b2 ) = 1 ) <=> ( b1 = 1 ) or ( b2 = 1 ) ) )
  & BIT_XOR : BIT * BIT --> BIT
  & ! ( b1 , b2 ) . ( b1 : BIT & b2 : BIT => ( ( BIT_XOR ( b1 , b2 ) = 1 ) <=> ( ( ( b1 = 1 ) & ( b2 = 0 ) ) or ( ( b1 = 0 ) & ( b2 = 1 ) ) ) ) )

ASSERTIONS
    BIT_FLIP ( 0 ) = 1 & BIT_FLIP ( 1 ) = 0
  & BIT_AND ( 0 , 0 ) = 0 & BIT_AND ( 0 , 1 ) = 0 & BIT_AND ( 1 , 0 ) = 0 & BIT_AND ( 1 , 1 ) = 1
  & BIT_IOR ( 0 , 0 ) = 0 & BIT_IOR ( 0 , 1 ) = 0 & BIT_IOR ( 1 , 0 ) = 0 & BIT_IOR ( 1 , 1 ) = 1
  & BIT_XOR ( 0 , 0 ) = 0 & BIT_XOR ( 0 , 1 ) = 1 & BIT_XOR ( 1 , 0 ) = 1 & BIT_XOR ( 1 , 1 ) = 0
END
