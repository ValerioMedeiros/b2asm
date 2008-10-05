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
   TYPES,BIT_DEFINITION, BIT_VECTOR_DEFINITION, BYTE_DEFINITION,BIT_VECTOR_ARITHMETICS

CONSTANTS
   parity
/*
   ,bitget
   ,bitset
   ,bitclear
   ,bitcomplement
   ,bitput
*/
   ,half
   ,add
   ,sub
   ,and
   ,and_bit
   ,ior
   ,xor
   ,complement
   ,swap
   ,rotateleft
   ,rotateright

   
   
  
  

PROPERTIES
/*

   bitget : BYTE * BYTE_INDEX --> BIT
  &bitget = %(bv, ind).(bv : BYTE & ind : BYTE_INDEX |bv(ind))
   
  &bitset : BYTE * BYTE_INDEX --> BYTE
  &bitset = %(bv, ind).(bv : BYTE & ind : BYTE_INDEX | BYTE_SET_BIT(bv,ind,1))
  
  &bitclear : BYTE * BYTE_INDEX --> BYTE
  &bitclear = %(bv,ind).(bv:BYTE & ind:BYTE_INDEX |BYTE_SET_BIT(bv,ind,0))


  &bitcomplement : BYTE * BYTE_INDEX --> BYTE
  &bitcomplement=%(bv,ind).(bv : BYTE & ind : BYTE_INDEX | BYTE_SET_BIT(bv,ind,bit_not(bv(ind))))


  &bitput : BYTE * BYTE_INDEX * BIT --> BYTE
  &bitput = %(bv,ind,bit).(bv:BYTE & ind:BYTE_INDEX & bit:BIT | BYTE_SET_BIT(bv,ind,bit))

*/

  parity : BYTE --> BIT
  &parity = %(bv).(bv : BYTE | bv_par(bv))

  &half: UCHAR --> UCHAR
  &half = %(v1).(v1 : UCHAR |v1 mod 2**4)
		
		
  /*((ACC),Data,(bit))= (Sun,P,Ov,AC,Cy)*/
  &add : UCHAR * UCHAR * UCHAR --> UCHAR * BIT * BIT * BIT * BIT
  &add = %(v1,v2,ca).(v1:UCHAR & v2:UCHAR & ca:UCHAR| (v1+v2+ca) mod (2**8) 
					              ,parity(uchar_byte((v1+v2+ca)mod (2**8)))
						      ,bool_to_bit(bool((uchar_schar(v1)+uchar_schar(v2)+ca)/:SCHAR))
						      ,bool_to_bit(bool ( (half(v1)+half(v2)+ca) >=2**4))
						      ,bool_to_bit(bool((v1+v2+ca)/:UCHAR)))

   /*((ACC),Data,(bit))= (Diff,P,Ov,AC,Cy)*/
  &sub:UCHAR * UCHAR * UCHAR --> UCHAR * BIT * BIT * BIT * BIT
  &sub = %(v1,v2,ca).(v1:UCHAR & v2:UCHAR & ca:UCHAR| (bool_to_bit(bool(v1<(v2+ca)))*256) + v1-v2-ca
					              ,parity(uchar_byte((bool_to_bit(bool(v1<(v2+ca)))*256) + v1-v2-ca)) 
						      ,bool_to_bit(bool((uchar_schar(v1)-uchar_schar(v2)-ca)/:SCHAR))
						      ,bool_to_bit(bool(half(v1)<(half(v2)-ca)))
						      ,bool_to_bit(bool((v1-v2-ca)/:UCHAR)))
  
 
  & and : (BYTE * BYTE) --> BYTE
  & and = %(bt1, bt2).(bt1:BYTE & bt2:BYTE | bv_and(bt1,bt2))

  & and_bit : (BIT*BIT)-->BIT
  & and_bit = %(b1,b2).(b1:BIT & b2:BIT | bit_and(b1,b2))


  & ior : (BYTE * BYTE) --> BYTE
  & ior = %(bt1, bt2).(bt1:BYTE & bt2:BYTE | bv_or(bt1,bt2))

  & xor : (BYTE * BYTE) --> BYTE
  & xor = %(bt1, bt2).(bt1 : BYTE & bt2 : BYTE | bv_xor(bt1,bt2))

  & complement : BYTE --> BYTE
  & complement = %(bt).(bt : BYTE | bv_not(bt))

  & swap : BYTE --> BYTE
  & swap=%(bt).(bt:BYTE |{0|->bt(4), 1|->bt(5), 2|->bt(6), 3|->bt(7), 4|->bt(0), 5|->bt(1), 6|->bt(2), 7|->bt(3)})

  & rotateleft : BYTE --> BYTE 
  & rotateleft=%(bv).(bv:BYTE| {0|->bv(7), 1|->bv(0), 2|->bv(1), 3|->bv(2), 4|->bv(3),5|->bv(4),6|->bv(5),7|->bv(6)} )
				

  & rotateright : BYTE --> BYTE 
  & rotateright=%(bv).(bv:BYTE|{0|->bv(1), 1|->bv(2), 2|->bv(3), 3|->bv(4),4|->bv(5), 5|->bv(6), 6|->bv(7), 7|->bv(0)})

 

ASSERTIONS
   dom(parity)=BYTE&
   ran(parity) <:  BIT&
/*
   dom(bitget) = BYTE * BYTE_INDEX &
   ran(bitget) <: BIT &

   dom(bitset) = BYTE * BYTE_INDEX &
   ran(bitset) <: BYTE &

   dom(bitclear) = BYTE * BYTE_INDEX &
   ran(bitclear) <: BYTE &

   dom(bitcomplement)= BYTE * BYTE_INDEX &
   ran(bitcomplement)<: BYTE&	

   dom(bitput)= BYTE * BYTE_INDEX*BIT &
   ran(bitput)<: BYTE &   
*/

   dom(add) = UCHAR * UCHAR * UCHAR &
   ran(add) <: UCHAR * BIT * BIT * BIT * BIT &

   dom(sub) = UCHAR * UCHAR * UCHAR&
   ran(sub) <: UCHAR * BIT * BIT * BIT * BIT &

   dom(and) = BYTE * BYTE &
   ran(and) <: BYTE  &

   dom(ior) = BYTE * BYTE &
   ran(ior) <: BYTE  &

   dom(xor) = BYTE * BYTE &
   ran(xor) <: BYTE  &

   dom(complement) = BYTE &
   ran(complement) <: BYTE &

   dom(swap) = BYTE &
   ran(swap) <: BYTE &

   dom(rotateleft) = BYTE &
   ran(rotateleft) <: BYTE  &

   dom(rotateright) = BYTE &
   ran(rotateright) <: BYTE 

END
