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
   programvariant
CONSTANTS
   pgvar, pgsize, loopaddr, loopsize

PROPERTIES
   pgsize : NATURAL & loopaddr : NATURAL & loopsize : NATURAL & pgvar : NATURAL * NATURAL * NATURAL +-> NATURAL &
   pgsize = 8 & loopsize = 5 & loopaddr = 2 &
   ! (pc, loopmax, loopvar).
	(pc : NATURAL & loopmax : NATURAL & loopvar : NATURAL =>
		(pc = 7 or pc = 8 => pgvar(pc, loopmax, loopvar) = pgsize - pc) &
                ((pc = 0 or pc = 1) => 
			pgvar(pc, loopmax, loopvar) = pgsize - pc + loopsize * loopmax) &
                ((pc = 2 or pc = 3 or pc = 4 or pc = 5 or pc = 6) => 
			pgvar(pc, loopmax, loopvar) = pgsize - loopaddr + loopsize * (loopmax -loopvar) - (pc - loopaddr)))
END
