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
   TYPES,BIT_DEFINITION, BIT_VECTOR_DEFINITION, BYTE_DEFINITION,BIT_VECTOR_ARITHMETICS,BIT_VECTOR16_DEFINITION

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
   ,div
   ,mult
   ,and
   ,and_bit
   ,ior
   ,xor
   ,complement
   ,swap
   ,rotateleft
   ,rotateright
   ,usint_byte
   ,bit_put

   
   
  
  

PROPERTIES


  parity : BYTE --> BIT
  & parity = %(bv).(bv : BYTE | bv_par(bv))

  & half: UCHAR --> UCHAR
  & half = %(v1).(v1 : UCHAR |v1 mod 2**4)

  & usint_byte : USHORT_INT --> BYTE * BYTE
  & usint_byte = %(usint).(usint:USHORT_INT |bv16_byte( usint_bv16(usint) ))
		
  /*
  	((ACC),Data,(bit))= (Sun,P,Ov,AC,Cy)
  	Add the uchars passed and calculate Parity, Overflow, Half-carry and Carry. 
  	The first uchar passed is the accumulator the second is the value to add to Accumulator and the 3ª is the carry of sun  	
  */
  & add : UCHAR * UCHAR * BIT --> UCHAR * BIT * BIT * BIT * BIT
  & add = %(v1,v2,ca).(v1:UCHAR & v2:UCHAR & ca:UCHAR| (v1+v2+ca) mod (UCHAR_MAX+1) /* Sun */
					              ,parity(uchar_byte((v1+v2+ca) mod (UCHAR_MAX+1))) /* Parity */
						      ,bool_to_bit(bool((uchar_schar(v1)+uchar_schar(v2)+ca)/:SCHAR)) /* Overflow */
						      ,bool_to_bit(bool ( (half(v1)+half(v2)+ca) >=2**4)) /* Half Carry */
						      ,bool_to_bit(bool((v1+v2+ca)/:UCHAR))) /* Carry */

   /*((ACC),Data,(bit))= (Diff,P,Ov,AC,Cy)*/
  & sub:UCHAR * UCHAR * UCHAR --> UCHAR * BIT * BIT * BIT * BIT
  & sub = %(v1,v2,ca).(v1:UCHAR & v2:UCHAR & ca:UCHAR| (bool_to_bit(bool(v1<(v2+ca)))*256) + v1-v2-ca/*Sun- Results less than zero are adds to (UCHAR_MAX+1) */
					              ,parity(uchar_byte((bool_to_bit(bool(v1<(v2+ca)))*256) + v1-v2-ca)) /*Parity*/
						      ,bool_to_bit(bool((uchar_schar(v1)-uchar_schar(v2)-ca)/:SCHAR))/*Overflow*/
						      ,bool_to_bit(bool(half(v1)<(half(v2)+ca)))/*Half-carry*/
						      ,bool_to_bit(bool((v1-v2-ca)/:UCHAR)))/*Carry*/
                          
	/*	  ((A),(B)	= (A),(B),OV,P*/                          
  & div :UCHAR *UCHAR --> UCHAR * UCHAR * BIT *BIT
  & div = %(aa,bb).(aa:UCHAR & bb:UCHAR | (aa / bb) ,
      									  (aa mod bb),
  										  bool_to_bit(bool(bb=0)),
  										  parity(uchar_byte(aa/bb))  )
	/*(A,B)=(A,B,OV)*/                                      
  & mult: UCHAR * UCHAR --> BYTE*BYTE*BIT
  & mult=%(aa,bb).(aa:UCHAR & bb:UCHAR |  usint_byte(aa*bb) ,
  										  bool_to_bit(bool(bb*aa>255))
  										  )
 
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

  & bit_put:BYTE *BYTE_INDEX*BIT --> BYTE
  & bit_put=%(byte,ind,bit).(byte:BYTE & ind:BYTE_INDEX & bit:BIT |bv_put(byte,ind,bit))
  

ASSERTIONS
   dom(parity)=BYTE&
   ran(parity) <:  BIT&

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
