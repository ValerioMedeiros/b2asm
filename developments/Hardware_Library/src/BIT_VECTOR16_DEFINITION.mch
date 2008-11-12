/* BIT_VECTOR16_DEFINITION
 * Author: Stephenson Galvão
 * Creation date: Thu Nov 6 2008
 */
MACHINE
    BIT_VECTOR16_DEFINITION
    
SEES 
	BIT_DEFINITION
	,BIT_VECTOR_DEFINITION
	,BYTE_DEFINITION
	
	
	
CONSTANTS 
	BV16_INDX
	,BV16_WIDTH
	,BV16_ZERO
	,BV16
	,bv16_byte
PROPERTIES 
    BV16_WIDTH: NATURAL
&   BV16_WIDTH = 16
&   BV16_INDX = 0..(BV16_WIDTH-1)
&	BV16 = { bt | bt: BV16_INDX--> BIT & size(bt)=16 } - {{}}
&	BV16_ZERO = BV16_INDX*{0}

&	bv16_byte: BV16 --> BYTE * BYTE
& 	bv16_byte = %(bv).(bv:BV16 | { 0|->bv(8),1|->bv(9),2|->bv(10),3|->bv(11),4|->bv(12),5|->bv(13),6|->bv(14),7|->bv(15)}
    							,{ 0|->bv(0),1|->bv(1),2|->bv(2),3|->bv(3),4|->bv(4),5|->bv(5),6|->bv(6),7|->bv(7)})    
ASSERTIONS
    BV16_ZERO: BV16
&	BV16<:BIT_VECTOR
&	BV16_ZERO : BIT_VECTOR

END
