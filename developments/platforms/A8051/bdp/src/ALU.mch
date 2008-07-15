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
   ALU

SEES
   TYPES

CONCRETE_CONSTANTS
   add , substract ,
   and , ior , xor ,
   bitclear , bitset , bitget ,
   complement , swap , rotateleft , rotateright , par

PROPERTIES
                        /*r      p     c    ov */
  add : ( WORD * WORD ) --> ( WORD * BIT * BIT * BIT )
  & ! ( w1 , w2 , sum ) .
     ( w1 : WORD & w2 : WORD & sum : NATURAL  & sum = w1 + w2 =>

      (
       ( sum <= 255 => add ( w1 , w2 ) =
          ( sum , BV8_PAR ( WORD_TO_BV ( sum ) ) , 0 , OVERFLOW_ADD ( WORD_TO_COMP2 ( w1 ) , WORD_TO_COMP2 ( w2 ) ) ) )
       & ( 256 <= sum => add ( w1 , w2 ) =
          ( sum - 256 , BV8_PAR ( WORD_TO_BV ( sum - 256 ) ) , 1 , OVERFLOW_ADD ( WORD_TO_COMP2 ( w1 ) , WORD_TO_COMP2 ( w2 ) ) ) )
      )
     )
                                  /*r      p     c     ov */
  & substract : WORD * WORD * BIT --> WORD * BIT * BIT * BIT
  & ! ( w1 , w2 , cy , diff ) .
     ( w1 : WORD & w2 : WORD & diff : INTEGER & cy : BIT & diff = w1 - w2 - cy =>
      (
        diff < 0 => substract ( w1 , w2 , cy ) =
           ( diff + 256 , BV8_PAR ( WORD_TO_BV ( diff ) ) , 1 , OVERFLOW_SUB ( WORD_TO_COMP2 ( w1 ) , WORD_TO_COMP2 ( w2 ) , cy ) )
        & diff >= 0 => substract ( w1 , w2 , cy ) =
           ( diff , BV8_PAR ( WORD_TO_BV ( diff ) ) , 0 , OVERFLOW_SUB ( WORD_TO_COMP2 ( w1 ) , WORD_TO_COMP2 ( w2 ) , cy ) )
      )
     )

  & and : ( WORD * WORD ) --> ( WORD * BOOL )
  & ! ( w1 , w2 , w ) .
     ( w1 : WORD & w2 : WORD & w : WORD & w = BV_TO_WORD ( BV8_AND ( WORD_TO_BV ( w1 ) , WORD_TO_BV ( w2 ) ) ) =>
      and ( w1 , w2 ) = ( w , bool ( w = 0 ) ) )

  & ior : ( WORD * WORD ) --> ( WORD * BOOL )
  & ! ( w1 , w2 , w ) .
     ( w1 : WORD & w2 : WORD & w : WORD & w = BV_TO_WORD ( BV8_IOR ( WORD_TO_BV ( w1 ) , WORD_TO_BV ( w2 ) ) ) =>
      ior ( w1 , w2 ) = ( w , bool ( w = 0 ) ) )

  & xor : ( WORD * WORD ) --> ( WORD * BOOL )
  & ! ( w1 , w2 , w ) .
     ( w1 : WORD & w2 : WORD & w : WORD & w = BV_TO_WORD ( BV8_XOR ( WORD_TO_BV ( w1 ) , WORD_TO_BV ( w2 ) ) ) =>
      xor ( w1 , w2 ) = ( w , bool ( w = 0 ) ) )

  & par : ( WORD ) --> BIT
  & ! ( w1 ) . ( w1 : WORD => par ( w1 ) = BV8_PAR ( WORD_TO_BV ( w1 ) ) )

  & bitget : WORD * WORD_POSITION --> BIT
  & ! ( w , i ) . ( w : WORD & i : WORD_POSITION => bitget ( w , i ) = WORD_TO_BV ( w ) ( i ) )

  & bitset : WORD * WORD_POSITION --> WORD
  & ! ( w , i ) . ( w : WORD & i : WORD_POSITION => bitset ( w , i ) = BV_TO_WORD ( BV8_SET_BIT ( WORD_TO_BV ( w ) , i , 1 ) ) )

  & bitclear : WORD * WORD_POSITION --> WORD
  & ! ( w , i , b ) . ( w : WORD & i : WORD_POSITION & b : BIT => bitclear ( w , i ) = BV_TO_WORD ( BV8_SET_BIT ( WORD_TO_BV ( w ) , i , 0 ) ) )

  & complement : WORD --> WORD
  & ! ( w ) . ( w : WORD => complement ( w ) = BV_TO_WORD ( BV8_COMPLEMENT ( WORD_TO_BV ( w ) ) ) )

  & swap : WORD --> WORD
  & ! ( w , v ) . ( w : WORD & v : BV8 =>
     ( v = WORD_TO_BV ( w ) =>
      swap ( w ) = BV_TO_WORD ( { 0 |-> v ( 4 ) , 1 |-> v ( 5 ) , 2 |-> v ( 6 ) , 3 |-> v ( 7 ) , 4 |-> v ( 0 ) , 5 |-> v ( 1 ) , 6 |-> v ( 2 ) , 7 |-> v ( 3 ) } ) ) )

  & rotateleft : WORD --> WORD * BOOL
  & ! ( w , v ) . ( w : WORD & v : BV8 =>
     ( v = WORD_TO_BV ( w ) =>
      rotateleft ( w ) = BV_TO_WORD ( { 0 |-> v ( 7 ) , 1 |-> v ( 0 ) , 2 |-> v ( 1 ) , 3 |-> v ( 2 ) , 4 |-> v ( 3 ) , 5 |-> v ( 4 ) , 6 |-> v ( 5 ) , 7 |-> v ( 6 ) } ) , bool ( v ( 7 ) = 1 ) ) )

  & rotateright : WORD --> WORD * BOOL
  & ! ( w , v ) . ( w : WORD & v : BV8 =>
     ( v = WORD_TO_BV ( w ) =>
      rotateright ( w ) = BV_TO_WORD ( { 0 |-> v ( 1 ) , 1 |-> v ( 2 ) , 2 |-> v ( 3 ) , 3 |-> v ( 4 ) , 4 |-> v ( 5 ) , 5 |-> v ( 6 ) , 6 |-> v ( 7 ) , 7 |-> v ( 0 ) } ) , bool ( v ( 0 ) = 1 ) ) )

ASSERTIONS
   dom ( add ) = WORD * WORD &
   ran ( add ) <: WORD * BIT * BIT * BIT &
   dom ( substract ) = WORD * WORD * BIT &
   ran ( substract ) <: WORD * BIT * BIT * BIT &
   dom ( and ) = WORD * WORD &
   ran ( and ) <: WORD * BOOL &
   dom ( ior ) = WORD * WORD &
   ran ( ior ) <: WORD * BOOL &
   dom ( xor ) = WORD * WORD &
   ran ( xor ) <: WORD * BOOL &
   dom ( bitclear ) = WORD * WORD_POSITION &
   ran ( bitclear ) <: WORD &
   dom ( bitset ) = WORD * WORD_POSITION &
   ran ( bitset ) <: WORD &
   dom ( bitget ) = WORD * WORD_POSITION &
   ran ( bitget ) <: BIT &
   dom ( complement ) = WORD &
   ran ( complement ) <: WORD &
   dom ( swap ) = WORD &
   ran ( swap ) <: WORD &
   ran ( rotateleft ) <: WORD * BOOL &
   dom ( rotateleft ) = WORD &
   dom ( rotateright ) = WORD &
   ran ( rotateright ) <: WORD * BOOL

END
