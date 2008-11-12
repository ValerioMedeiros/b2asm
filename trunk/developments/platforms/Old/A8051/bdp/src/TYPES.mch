MACHINE
   TYPES

INCLUDES
TYPE_BV8 , TYPE_BIT
CONCRETE_CONSTANTS

WORD
, WORD_LEN
, WORD_POSITION
, NB_WORD
, COMPLEMENT_2

, INSTRUCTION
, NB_INSTRUCTIONS
, INSTRUCTION_MAX
, INSTRUCTION_IJUMP
, INSTRUCTION_NEXT
/*,WORD_TO_INT*/
, BV_TO_WORD
, WORD_TO_BV
, WORD_TO_COMP2
, COMP2_TO_WORD
, OVERFLOW_ADD
, OVERFLOW_SUB

, MEM_ADDR
, RAM_ADDR
, SFR_ADDR
, MAX_ADDR
, BIT_ADDRESS

/*SETS 
ADDR*/

PROPERTIES


MAX_ADDR : NATURAL

& RAM_ADDR = 0 .. 127
& SFR_ADDR = 128 .. MAX_ADDR
& MAX_ADDR = 255
& MEM_ADDR = 0 .. MAX_ADDR

& COMPLEMENT_2 = - 128 .. 127

& NB_WORD : NATURAL
& WORD_LEN : NATURAL

& WORD_LEN = 8
& WORD_POSITION = 0 .. ( WORD_LEN - 1 )
& NB_WORD = 2 ** WORD_LEN
& WORD = 0 .. ( NB_WORD - 1 )
& NB_INSTRUCTIONS = 256 /* 2**INST_SZ */
& INSTRUCTION_MAX = NB_INSTRUCTIONS - 1
& INSTRUCTION = 0 .. INSTRUCTION_MAX

& INSTRUCTION_IJUMP : ( INSTRUCTION * INSTRUCTION ) --> INSTRUCTION
& ! ( i , j , sum ) . ( i : INSTRUCTION & j : INSTRUCTION & sum : NATURAL & sum = i + j
                 => ( ( sum >= 255 => INSTRUCTION_IJUMP ( i , j ) = 0 ) &
                    ( sum < 255 => INSTRUCTION_IJUMP ( i , j ) = sum + 1 ) ) )

& INSTRUCTION_NEXT : INSTRUCTION -->> INSTRUCTION
& ! ( i ) . ( i : INSTRUCTION => ( ( i = 255 => INSTRUCTION_NEXT ( i ) = 0 ) &
                         ( i < 255 => INSTRUCTION_NEXT ( i ) = i + 1 ) ) )

/*
WORD_TO_INT : WORD -->> INT & 
!(i,j).(i : WORD & j : NATURAL & i>0 & i <= 255 & j=i+0 & 0<j & j<=255  
 => WORD_TO_INT(i)=j )
*/

& BV_TO_WORD : BV8 --> WORD
  & WORD_TO_BV : WORD --> BV8
  & ! ( w , v ) . ( w : WORD & v : BV8 =>
                ( ( v = WORD_TO_BV ( w ) ) <=> ( w = 128 * v ( 7 ) + 64 * v ( 6 ) + 32 * v ( 5 ) + 16 * v ( 4 ) + 8 * v ( 3 ) + 4 * v ( 2 ) + 2 * v ( 1 ) + v ( 0 ) ) ) )
  & BV_TO_WORD = WORD_TO_BV ~

& BIT_ADDRESS = WORD * WORD_POSITION


& WORD_TO_COMP2 : WORD --> COMPLEMENT_2
& ! ( w ) . ( w : WORD => ( w <= 127 => WORD_TO_COMP2 ( w ) = w )
                    & ( w > 127 & w <= 255 => WORD_TO_COMP2 ( w ) = w - 255 ) )

& COMP2_TO_WORD = WORD_TO_COMP2 ~

& OVERFLOW_ADD : COMPLEMENT_2 * COMPLEMENT_2 --> BIT
& ! ( c1 , c2 , csum ) . ( c1 : COMPLEMENT_2 & c2 : COMPLEMENT_2 & csum : INTEGER &
                 csum = c1 + c2 =>
                        ( ( csum <= 127 & - 128 <= csum => OVERFLOW_ADD ( c1 , c2 ) = 0 )
                          & ( csum > 127 & - 128 > csum => OVERFLOW_ADD ( c1 , c2 ) = 1 )
                        )
                )

& OVERFLOW_SUB : COMPLEMENT_2 * COMPLEMENT_2 * BIT --> BIT
& ! ( c1 , c2 , cy , csum ) . ( c1 : COMPLEMENT_2 & c2 : COMPLEMENT_2 & csum : INTEGER & cy : BIT &
                 csum = c1 - c2 - cy =>
                        (
                          ( csum <= 127 & - 128 <= csum => OVERFLOW_SUB ( c1 , c2 , cy ) = 0 )
                          & ( csum > 127 & - 128 > csum => OVERFLOW_SUB ( c1 , c2 , cy ) = 1 )
                        )
                )



ASSERTIONS
NB_WORD = 256 &
! ( n ) . ( n : WORD => 0 <= n & n <= 255 ) &
INSTRUCTION_IJUMP ( 5 , 1 ) = 7 &
INSTRUCTION_IJUMP ( 0 , 2 ) = 3 &
INSTRUCTION_IJUMP ( 2 , 4 ) = 7 &
INSTRUCTION_IJUMP ( 3 , 2 ) = 6 &
INSTRUCTION_NEXT ( 0 ) = 1 &
INSTRUCTION_NEXT ( 1 ) = 2 &
INSTRUCTION_NEXT ( 2 ) = 3 &
INSTRUCTION_NEXT ( 3 ) = 4 &
INSTRUCTION_NEXT ( 4 ) = 5 &
INSTRUCTION_NEXT ( 5 ) = 6 &
INSTRUCTION_NEXT ( 6 ) = 7 &
INSTRUCTION_IJUMP ( 3 , 3 ) = 7 &
MEM_ADDR <: WORD &
RAM_ADDR <: WORD &
SFR_ADDR <: WORD &
MEM_ADDR = RAM_ADDR \/ SFR_ADDR




END
