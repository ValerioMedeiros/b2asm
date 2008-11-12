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
    TYPE_BIT

CONSTANTS
    BV8_INDEX
  , BV8
  , BV8_SET_BIT
  , BV8_COMPLEMENT
  , BV8_ALL_ZEROES
  , BV8_AND
  , BV8_IOR
  , BV8_XOR
  , BV8_PAR

PROPERTIES
    BV8_INDEX = (0..7)

  & BV8 = (BV8_INDEX --> BIT)

  & BV8_SET_BIT : (BV8 * BV8_INDEX * BIT) --> BV8
  & ! (v, i, j, b) . (v : BV8 & i : BV8_INDEX & j : BV8_INDEX & b : BIT =>
      (i /= j => BV8_SET_BIT(v, i, b)(j) = v(j)))
  & ! (v, i, b) . (v : BV8 & i : BV8_INDEX & b : BIT => BV8_SET_BIT(v, i, b)(i) = b)

  & BV8_COMPLEMENT : BV8 --> BV8
  & ! (v, i).(v : BV8 & i : BV8_INDEX => BV8_COMPLEMENT(v)(i) = BIT_FLIP(v(i)))

  & BV8_ALL_ZEROES : BV8
  & ! (i) . (i : BV8_INDEX => BV8_ALL_ZEROES(i) = 0)

  & BV8_AND : BV8 * BV8 --> BV8
  & ! (v1, v2, i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_AND(v1, v2)(i) = BIT_AND(v1(i),v2(i)))

  & BV8_IOR : BV8 * BV8 --> BV8
  & ! (v1, v2, i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_IOR(v1, v2)(i) = BIT_IOR(v1(i),v2(i)))

  & BV8_XOR : BV8 * BV8 --> BV8
  & ! (v1, v2, i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_XOR(v1, v2)(i) = BIT_XOR(v1(i),v2(i)))
  
  &BV8_PAR : BV8 --> BIT
  & !(v1,t1,t2,t3,t4).(v1 : BV8 & t1 : BIT & t2 : BIT & t3 : BIT& t4 : BIT &
    t1=BIT_XOR(v1(7),v1(6)) & t2=BIT_XOR(v1(5),v1(4))& t3=BIT_XOR(v1(3),v1(2))&t4=BIT_XOR(v1(1),v1(0))         
    => BV8_PAR(v1) = BIT_XOR(BIT_XOR(t1,t2),BIT_XOR(t3,t4)))

  

END
