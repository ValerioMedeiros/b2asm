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

SEES
	BIT_DEFINITION,
	BIT_VECTOR_DEFINITION,
	BIT_VECTOR_ARITHMETICS,
	BYTE_DEFINITION,
	POWER2

CONSTANTS
	UCHAR_LENGTH,
	UCHAR,
	NB_UCHARS,
	INST_SZ,
	INSTRUCTION,
	NB_INSTRUCTIONS,
	INSTRUCTION_MAX,
	instruction_next,
	byte_to_uchar,
	uchar_to_byte,
	REGISTER,
	REGISTER0,
	REGISTER1

PROPERTIES
	UCHAR_LENGTH : NATURAL &
	NB_UCHARS : NATURAL &
	INST_SZ : NATURAL &
	NB_INSTRUCTIONS : NATURAL &
	UCHAR_LENGTH = 8 &
	NB_UCHARS = 2**UCHAR_LENGTH &
	UCHAR = 0..(NB_UCHARS-1) &
	NB_INSTRUCTIONS = 2**INST_SZ &
	INSTRUCTION_MAX = NB_INSTRUCTIONS - 1 &
	INSTRUCTION = 0..INSTRUCTION_MAX &
	instruction_next : INSTRUCTION --> INSTRUCTION &
	instruction_next = 
		{pp, qq | pp : INSTRUCTION & qq : INSTRUCTION & 0 <= pp & pp < NB_INSTRUCTIONS-1 & qq = pp+1} \/ 
		{ NB_INSTRUCTIONS-1 |-> 0 } &
	byte_to_uchar : BYTE --> UCHAR &
	byte_to_uchar = % (vv) . (vv : BYTE | bv_to_nat(vv)) &
	uchar_to_byte : UCHAR --> BYTE &
	uchar_to_byte = byte_to_uchar~ &
	REGISTER = 0..255 &
	REGISTER0 = 0..127 &
	REGISTER1 = 128..255

ASSERTIONS
	NB_UCHARS = 256;
	! (nn). (nn : UCHAR => 0 <= nn);
	! (nn). (nn : UCHAR => nn <= 255);
	REGISTER = UCHAR;
	ran (byte_to_uchar) = UCHAR

END