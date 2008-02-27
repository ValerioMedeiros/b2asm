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
