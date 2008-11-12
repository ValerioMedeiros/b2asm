/*************************************************************************
Copyright (c) 2008, David Deharbe

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
   data_refinement
SEES
   definitions
CONCRETE_CONSTANTS
   color_refine , color_step
PROPERTIES
   color_refine : COLOR --> NATURAL &
   color_refine = { green |-> 0 , yellow |-> 1 , red |-> 2 } &
   color_step : 0 .. 2 --> 0 .. 2 &
   color_step = { 0 |-> 1 , 1 |-> 2 , 2 |-> 0 }
ASSERTIONS
   color_refine ( green ) = 0 &
   color_refine ( yellow ) = 1 &
   color_refine ( red ) = 2 &
   color_step ( 0 ) = 1 &
   color_step ( 1 ) = 2 &
   color_step ( 2 ) = 0
END
