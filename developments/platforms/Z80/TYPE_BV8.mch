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
    TYPE_BV8

SEES
 BIT_VECTOR_DEFINITION , BIT_DEFINITION

CONCRETE_CONSTANTS
    BYTE_WIDTH
  , BYTE_INDEX
  , BYTE
  , BV_TO_BYTE
  , BYTE_SET_BIT
  , BYTE_COMPLEMENT
  , BYTE_AND
  , BYTE_OR
  , BYTE_XOR
  , BYTE_PAR
  , BYTE_ZERO


PROPERTIES
    BYTE_WIDTH = 8
  & BYTE_INDEX = 0 .. BYTE_WIDTH - 1

  & BYTE <: BIT_VECTOR
  & BYTE = { bv | bv : BIT_VECTOR & size ( bv ) = BYTE_WIDTH }

  & BV_TO_BYTE : BIT_VECTOR --> BYTE
  & ! ( bv , byte , ind ) . ( bv : BIT_VECTOR & byte : BYTE & ind : BYTE_INDEX => BV_TO_BYTE ( bv ) ( ind ) = bv ( ind ) )

/*
  What is better ?	
  & BV_TO_BYTE = %(bv,byte).(bv : BIT_VECTOR & byte:BYTE |%(ind).(ind : BYTE_INDEX) | bv(ind) )
*/


  & BYTE_SET_BIT : BYTE * BYTE_INDEX * BIT --> BYTE
  & BYTE_SET_BIT = % ( byte , ind , bit ) . ( byte : BYTE & ind : BYTE_INDEX & bit : BIT & bit = 1 | BV_TO_BYTE ( bv_set ( byte , ind ) ) )
  & BYTE_SET_BIT = % ( byte , ind , bit ) . ( byte : BYTE & ind : BYTE_INDEX & bit : BIT & bit = 0 | BV_TO_BYTE ( bv_clear ( byte , ind ) ) )




  & BYTE_COMPLEMENT : BYTE --> BYTE
  & BYTE_COMPLEMENT = % ( byte ) . ( byte : BYTE | BV_TO_BYTE ( bv_not ( byte ) ) )

  & BYTE_AND : BYTE * BYTE --> BYTE
  & BYTE_AND = % ( byte1 , byte2 ) . ( byte1 : BYTE & byte2 : BYTE | BV_TO_BYTE ( bv_and ( byte1 , byte2 ) ) )

  & BYTE_OR : BYTE * BYTE --> BYTE
  & BYTE_OR = % ( byte1 , byte2 ) . ( byte1 : BYTE & byte2 : BYTE | BV_TO_BYTE ( bv_or ( byte1 , byte2 ) ) )

  & BYTE_XOR : BYTE * BYTE --> BYTE
  & BYTE_XOR = % ( byte1 , byte2 ) . ( byte1 : BYTE & byte2 : BYTE | BV_TO_BYTE ( bv_xor ( byte1 , byte2 ) ) )

  /* 
     Can be make better. Exemplo Make a EVEN SET and put Parity definition in BIT_VECTOR_DEFINITION.
     Ansk for theather how difini the even set, uses only add and sub 	
 */
  & BYTE_PAR : BYTE --> BIT
  & ! ( byte , none ) . ( byte : BYTE & none : NATURAL & none = size ( byte |> { 1 } ) => (
                 ( none mod 2 = 0 ) => BYTE_PAR ( byte ) = 0 & ( none mod 2 /= 0 ) => BYTE_PAR ( byte ) = 1 ) )
  & BYTE_ZERO : BYTE
  & BYTE_ZERO = BYTE_INDEX * { 0 }





ASSERTIONS

BYTE_INDEX <: NATURAL

END
