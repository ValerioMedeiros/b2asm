MACHINE
   TYPES

INCLUDES
   TYPE_BIT ,
   TYPE_BV8

CONCRETE_CONSTANTS
   WORD_LENGTH ,
   WORD ,
   WORD_POSITION ,
   NB_WORDS ,
   INST_SZ ,
   INSTRUCTION ,
   NB_INSTRUCTIONS ,
   INSTRUCTION_MAX ,
   INSTRUCTION_NEXT
  , BV_TO_WORD
  , WORD_TO_BV

PROPERTIES
   WORD_LENGTH : NATURAL &
   INST_SZ : NATURAL &
   NB_WORDS : NATURAL &
   NB_INSTRUCTIONS : NATURAL &
   WORD_LENGTH = 8 &
   NB_WORDS = 2 ** WORD_LENGTH &
   WORD = 0 .. ( NB_WORDS - 1 ) &
   WORD_POSITION = 0 .. ( WORD_LENGTH - 1 ) &
   NB_INSTRUCTIONS = 2 ** INST_SZ &
   INSTRUCTION_MAX = NB_INSTRUCTIONS - 1 &
   INSTRUCTION = 0 .. INSTRUCTION_MAX &
   INSTRUCTION_NEXT : INSTRUCTION --> INSTRUCTION &
   INSTRUCTION_NEXT = { p , q | p : INSTRUCTION & q : INSTRUCTION & 0 <= p & p < NB_INSTRUCTIONS - 1 & q = p + 1 } \/ { NB_INSTRUCTIONS - 1 |-> 0 }
  & BV_TO_WORD : BV8 --> WORD
  & WORD_TO_BV : WORD --> BV8
  & ! ( w , v ) . ( w : WORD & v : BV8 =>
                ( ( v = WORD_TO_BV ( w ) ) <=> ( w = 128 * v ( 7 ) + 64 * v ( 6 ) + 32 * v ( 5 ) + 16 * v ( 4 ) + 8 * v ( 3 ) + 4 * v ( 2 ) + 2 * v ( 1 ) + v ( 0 ) ) ) )
  & BV_TO_WORD = WORD_TO_BV ~

SETS
   REGISTER

ASSERTIONS
   NB_WORDS = 256 &
   ! ( n ) . ( n : WORD => 0 <= n ) &
   ! ( n ) . ( n : WORD => n <= 255 ) &
   WORD_POSITION = BV8_INDEX

END
