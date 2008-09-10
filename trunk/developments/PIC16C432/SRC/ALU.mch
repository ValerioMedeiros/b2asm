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
	TYPES,
	BIT_DEFINITION,
	BIT_VECTOR_DEFINITION,
	BIT_VECTOR_ARITHMETICS,
	BYTE_DEFINITION

CONSTANTS
	half,
	add, substract,
	and, ior, xor,
	bitclear, bitset, bitget,
	complement, swap, rotateleft, rotateright

PROPERTIES
	half : UCHAR --> UCHAR &
	half = % (ww) . (ww : UCHAR | ww mod (2 ** 4)) &

	/* add (w1, w2) = (sum, carry, digit_carry, zero) */
	add : (UCHAR * UCHAR) --> (UCHAR * BOOL * BOOL * BOOL) &
	add = % (w1, w2). (w1 : UCHAR & w2 : UCHAR |
		((w1 + w2) mod (2 ** 8),
		 bool(w1 + w2 >= 2 ** 8),
		 bool(half(w1) + half(w2) >= 2 ** 4),
		 bool(w1 + w2 : {0, 256}))) &

	/* add (w1, w2) = (sum, inv(borrow), inv(digit_borrow), zero) */
	substract : (UCHAR * UCHAR) --> (UCHAR * BOOL * BOOL * BOOL) &
	substract = 
	% (w1, w2). (w1 : UCHAR & w2 : UCHAR & w1 >= w2 |
		(w1 - w2,
		 TRUE,
		 bool(half(w1) >= half(w2)),
		 bool(w1 = w2))) \/
	% (w1, w2). (w1 : UCHAR & w2 : UCHAR & w1 < w2 |
		(2 ** 8 + w1 - w2,
		 FALSE,
		 bool(half(w1) >= half(w2)),
		 FALSE)) &

	and : (BYTE * BYTE) --> (BYTE * BOOL) & 
	! (w1, w2, ww).
	  (w1 : BYTE & w2 : BYTE & ww : BYTE & ww = bv_and(w1, w2) =>
			and(w1, w2) = (ww, bool(bv_to_nat(ww) = 0))) & 
	ior : (BYTE * BYTE) --> (BYTE * BOOL) & 
	! (w1, w2, ww).
	  (w1 : BYTE & w2 : BYTE & ww : BYTE & ww = bv_or(w1, w2) =>
			ior(w1, w2) = (ww, bool(bv_to_nat(ww) = 0))) & 
	xor : (BYTE * BYTE) --> (BYTE * BOOL) & 
	! (w1, w2, ww).
	  (w1 : BYTE & w2 : BYTE & ww : BYTE =>
		(ww = bv_xor(w1, w2) =>
			xor(w1, w2) = (ww, bool(bv_to_nat(ww) = 0)))) & 
	bitget : (BYTE * BYTE_INDEX) --> BIT & 
	! (ww, ii).(ww : BYTE & ii : BYTE_INDEX => bitget(ww,ii) = ww(ii)) & 
	bitset : (BYTE * BYTE_INDEX) --> BYTE & 
	! (ww, ii).(ww : BYTE & ii : BYTE_INDEX => bitset(ww,ii) = bv_set(ww, ii)) & 
	bitclear : (BYTE * BYTE_INDEX) --> BYTE & 
	! (ww, ii, bb).(ww : BYTE & ii : BYTE_INDEX & bb : BIT => bitclear(ww,ii) = bv_clear(ww, ii)) & 
	complement : BYTE --> BYTE & 
	! (ww).(ww : BYTE => complement(ww) = bv_not(ww)) & 
	swap : BYTE --> BYTE & 
	! (ww).(ww : BYTE =>
	  (swap(ww) = {0|->ww(4), 1|->ww(5), 2|->ww(6), 3|->ww(7), 4|->ww(0), 5|->ww(1), 6|->ww(2), 7|->ww(3)})) & 
	rotateleft : BYTE --> BYTE * BOOL & 
	! (ww).(ww : BYTE =>
	  (rotateleft(ww) = ({0|->ww(7), 1|->ww(0), 2|->ww(1), 3|->ww(2), 4|->ww(3), 5|->ww(4), 6|->ww(5), 7|->ww(6)}, 
				bool(ww(7)=1)))) & 
	rotateright : BYTE --> BYTE * BOOL  & 
	! (ww).(ww : BYTE  =>
	  (rotateright(ww) = ({0|->ww(1), 1|->ww(2), 2|->ww(3), 3|->ww(4), 4|->ww(5), 5|->ww(6), 6|->ww(7), 7|->ww(0)}, 
				bool(ww(0)=1))))

ASSERTIONS
	dom(add) = UCHAR * UCHAR;
	ran(add) <: UCHAR * BOOL * BOOL * BOOL;
	dom(substract) = UCHAR * UCHAR;
	ran(substract) <: UCHAR * BOOL * BOOL * BOOL;
	dom(and) = BYTE * BYTE;
	ran(and) <: BYTE * BOOL;
	dom(ior) = BYTE * BYTE;
	ran(ior) <: BYTE * BOOL;
	dom(xor) = BYTE * BYTE;
	ran(xor) <: BYTE * BOOL;
	dom(bitclear) = BYTE * BYTE_INDEX;
	ran(bitclear) <: BYTE;
	dom(bitset) = BYTE * BYTE_INDEX;
	ran(bitset) <: BYTE;
	dom(bitget) = BYTE * BYTE_INDEX;
	ran(bitget) <: BIT;
	dom(complement) = BYTE;
	ran(complement) <: BYTE;
	dom(swap) = BYTE;
	ran(swap) <: BYTE;
	ran(rotateleft) <: BYTE * BOOL;
	dom(rotateleft) = BYTE;
	dom(rotateright) = BYTE;
	ran(rotateright) <: BYTE * BOOL;
	add(0,0) = (0, TRUE, FALSE, FALSE);
	add(255,1) = (0, TRUE, FALSE, TRUE);
	add(255,1) = (0, TRUE, TRUE, TRUE)
END
