/*************************************************************************
Copyright (c) 2007, David Deharbe

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
   INSTRUCTION_NEXT ,
   BV_TO_WORD ,
   WORD_TO_BV ,
   REGISTER


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
  & ! ( n ) . ( n : NATURAL & n > 0 => 2 ** n = 2 * ( 2 ** ( n - 1 ) ) )
  & 2 ** 0 = 1
  & REGISTER : POW ( INTEGER ) & REGISTER = 0 .. 127

ASSERTIONS
   2 ** 8 = 256 &
   NB_WORDS = 256 &
   ! ( n ) . ( n : WORD => 0 <= n ) &
   ! ( n ) . ( n : WORD => n <= 255 ) &
   WORD_POSITION = BV8_INDEX

END
