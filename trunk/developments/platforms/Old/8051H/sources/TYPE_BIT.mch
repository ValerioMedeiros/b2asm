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

SEES
   BIT_DEFINITION
	
CONSTANTS
   bit_add
  ,bit_sub
   
   


PROPERTIES
    
bit_add : BIT * BIT * BIT --> BIT * BIT
&bit_add = %(b1,b2,cin).(b1:BIT & b2:BIT & cin:BIT|bit_xor(bit_xor(b1,b2),cin)
						 ,bit_or(bit_and(b1,b2),bit_and(cin,bit_xor(b1,b2)))) 

&bit_sub : BIT * BIT * BIT --> BIT * BIT
&bit_sub = %(b1,b2,cin).(b1:BIT & b2:BIT & cin:BIT|bit_xor(bit_xor(b1,b2),cin)
					          ,bit_or(bit_and(bit_not(b1),b2),bit_and(cin,bit_or(bit_not(b1),b2))))







ASSERTIONS
 bit_add(0,0,0)=(0,0)
&bit_add(0,0,1)=(1,0)
&bit_add(0,1,0)=(1,0)
&bit_add(0,1,1)=(0,1)
&bit_add(1,0,0)=(1,0)
&bit_add(1,0,1)=(0,1)
&bit_add(1,1,0)=(0,1)
&bit_add(1,1,1)=(1,1)

&bit_sub(0,0,0)=(0,0)
&bit_sub(0,0,1)=(1,1)
&bit_sub(0,1,0)=(1,1)
&bit_sub(0,1,1)=(0,1)
&bit_sub(1,0,0)=(1,0)
&bit_sub(1,0,1)=(0,0)
&bit_sub(1,1,0)=(0,0)
&bit_sub(1,1,1)=(1,1)

END

