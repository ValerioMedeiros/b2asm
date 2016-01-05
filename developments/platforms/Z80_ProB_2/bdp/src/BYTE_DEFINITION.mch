/*************************************************************************
Copyright (c) 2009,  David Deharbe, Valerio Medeiros Jr.

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
    BYTE_DEFINITION

SEES
    BIT_DEFINITION ,

    POWER2

























CONCRETE_CONSTANTS

    BYTE ,
    BYTE_INDEX ,
    PHYS_BYTE_INDEX ,


    is_zero ,
    parity_even ,
    bv8_and , bv8_or , bv8_xor ,
    bitclear , bitset , bv8get ,
    complement , swap , rotateleft , rotateright ,
    get_upper_digit ,
    get_lower_digit

PROPERTIES

    BYTE_INDEX = 1 .. ( 8      ) &
    PHYS_BYTE_INDEX = 0 .. ( 8      - 1 ) &


        BYTE =  ( BYTE_INDEX --> BIT )        & ! ( b1 ) . ( b1 : BYTE => size ( b1 ) = 8 & b1 : seq1 ( BIT ) ) &


    is_zero : BYTE --> BIT &
    is_zero = % ( w1 ) . ( w1 : BYTE | bool_bit ( bool ( w1 ( 1 ) = w1 ( 2 ) & w1 ( 2 ) = w1 ( 3 ) & w1 ( 3 ) = w1 ( 4 ) & w1 ( 4 ) = w1 ( 5 ) & w1 ( 5 ) = w1 ( 6 ) & w1 ( 6 ) = w1 ( 7 ) & w1 ( 7 ) = w1 ( 8 ) & w1 ( 8 ) = 0 ) ) ) &


          parity_even : BYTE --> BIT &
          parity_even = % ( bv ) . ( bv : BYTE | 1 - ( ( bv ( 1 ) + bv ( 2 ) + bv ( 3 ) + bv ( 4 ) + bv ( 5 ) + bv ( 6 ) + bv ( 7 ) + bv ( 8 ) ) mod 2 ) ) &

        /*THE LAST TEST TO PROVE AND ANIMATE*/
        /*  bv8_and:  POW(((INTEGER*INTEGER)*(INTEGER*INTEGER))*POW(INTEGER*INTEGER))*/ /*POW((BYTE * BYTE) *  BYTE) &*/

        /*bv8_and : (BYTE * BYTE) --> BYTE &*/
    bv8_and = % ( bt1 , bt2 ) . ( bt1 : BYTE & bt2 : BYTE | % ( idx ) . ( idx : 1 .. 8 | bit_and ( bt1 ( idx ) , bt2 ( idx ) ) ) ) &


          /*bv8_or : (BYTE * BYTE) --> BYTE &*/
    bv8_or = % ( bt1 , bt2 ) . ( bt1 : BYTE & bt2 : BYTE | % ( idx ) . ( idx : 1 .. 8 | bit_or ( bt1 ( idx ) , bt2 ( idx ) ) ) ) &

        /*bv8_xor : (BYTE * BYTE) --> BYTE &*/
    bv8_xor = % ( bt1 , bt2 ) . ( bt1 : BYTE & bt2 : BYTE | % ( idx ) . ( idx : 1 .. 8 | bit_xor ( bt1 ( idx ) , bt2 ( idx ) ) ) ) &

    /*and : (BYTE * BYTE) --> BYTE &
    and = %(bt1, bt2).(bt1:BYTE & bt2:BYTE | bv_and(bt1,bt2)) &
    
    ior : (BYTE * BYTE) --> BYTE & 
    ior = %(bt1, bt2).(bt1:BYTE & bt2:BYTE | bv_or(bt1,bt2))   & 
    
    xor : (BYTE * BYTE) --> BYTE &
    xor = %(bt1, bt2).(bt1 : BYTE & bt2 : BYTE | bv_xor(bt1,bt2)) &*/


    bv8get : ( BYTE * PHYS_BYTE_INDEX ) --> BIT &
    bv8get = % ( bt1 , ii ) . ( bt1 : BYTE & ii : PHYS_BYTE_INDEX | bt1 ( ii + 1 ) ) &

    bitset : ( BYTE * PHYS_BYTE_INDEX ) --> BYTE &
    bitset = % ( bt1 , ii ) . ( bt1 : BYTE & ii : PHYS_BYTE_INDEX | bt1 <+ { ( ii + 1 ) |-> 1 } ) &

    bitclear : ( BYTE * PHYS_BYTE_INDEX ) --> BYTE &
    bitclear = % ( bt1 , ii ) . ( bt1 : BYTE & ii : PHYS_BYTE_INDEX | bt1 <+ { ( ii + 1 ) |-> 0 } ) &


    complement : BYTE --> BYTE &
    complement = % ( bt ) . ( bt : BYTE | % ( idx ) . ( idx : 1 .. 8 | bit_not ( bt ( idx ) ) ) ) &


    swap : BYTE --> BYTE &
    swap = % ( bt ) . ( bt : BYTE | { 1 |-> bt ( 5 ) , 2 |-> bt ( 6 ) , 3 |-> bt ( 7 ) , 4 |-> bt ( 8 ) , 5 |-> bt ( 1 ) , 6 |-> bt ( 2 ) , 7 |-> bt ( 3 ) , 8 |-> bt ( 4 ) } ) &


    rotateleft : BYTE --> BYTE   &
    rotateleft  = % ( bv ) . ( bv : BYTE | { 1 |-> bv ( 8 ) , 2 |-> bv ( 1 ) , 3 |-> bv ( 2 ) , 4 |-> bv ( 3 ) , 5 |-> bv ( 4 ) , 6 |-> bv ( 5 ) , 7 |-> bv ( 6 ) , 8 |-> bv ( 7 ) } ) &


    rotateright : BYTE --> BYTE &
    rotateright = % ( bv ) . ( bv : BYTE | { 1 |-> bv ( 2 ) , 2 |-> bv ( 3 ) , 3 |-> bv ( 4 ) , 4 |-> bv ( 5 ) , 5 |-> bv ( 6 ) , 6 |-> bv ( 7 ) , 7 |-> bv ( 8 ) , 8 |-> bv ( 1 ) } ) &

    get_upper_digit : BYTE --> ( 0 .. 15 ) &
    get_upper_digit = % ( by ) . ( by : BYTE | 8    * by ( 8 ) + 4     * by ( 7 ) + 2 * by ( 6 ) +  by ( 5 )   ) &

    get_lower_digit : BYTE --> ( 0 .. 15 ) &
    get_lower_digit = % ( by ) . ( by : BYTE | 8    * by ( 4 ) + 4     * by ( 3 ) + 2 * by ( 2 ) +  by ( 1 )   )


ASSERTIONS

    /*card((1..8 --> {0,1}))=256 - Checked by ProB (Eval)  */
    card ( BYTE ) = 256   &

    /* properties moved to here */


          is_zero : BYTE --> BIT ;
                        parity_even : BYTE --> BIT ;

                        bv8_and : ( BYTE * BYTE ) --> BYTE ;
                        bv8_or : ( BYTE * BYTE ) --> BYTE ;
   bv8_xor : ( BYTE * BYTE ) --> BYTE ;

/*	bitget : (BYTE * PHYS_BYTE_INDEX) --> BIT ;
	
	  bitset : (BYTE * PHYS_BYTE_INDEX) --> BYTE ;
	  bitclear : (BYTE * PHYS_BYTE_INDEX) --> BYTE ;*/
          complement : BYTE --> BYTE ;
          swap : BYTE --> BYTE ;
          rotateleft : BYTE --> BYTE ;
             rotateright : BYTE --> BYTE ;




/*  BYTE <: BIT_VECTOR ;
    BYTE_ZERO : BIT_VECTOR ; */


    ! ( bt ) . ( bt : BYTE => size ( bt ) = 8 ) ;

    8          : NATURAL ;

    dom ( is_zero ) = BYTE ;
    ran ( is_zero ) <: BIT ;

    dom ( parity_even ) = BYTE ;
    ran ( parity_even ) <: BIT ;

    bv8_and : ( BYTE * BYTE ) --> BYTE ;
/*  dom(and) = BYTE * BYTE &
    ran(and) = BYTE  &
    
    dom(ior) = BYTE * BYTE &	
    ran(ior) = BYTE  &
    
    dom(xor) = BYTE * BYTE &
    ran(xor) = BYTE  &
    
    dom(bitget)=(BYTE * PHYS_BYTE_INDEX) ;
    ran(bitget)=BIT ;
    
    dom(bitset)=(BYTE * PHYS_BYTE_INDEX) ;
    ran(bitset)= BYTE ;
    
    dom(bitclear)=(BYTE * PHYS_BYTE_INDEX);
    ran(bitclear)= BYTE ;*/

    dom ( complement ) = BYTE ;
    ran ( complement ) <: BYTE ;

    dom ( swap ) = BYTE ;
    ran ( swap ) <: BYTE ;

    dom ( rotateleft ) = BYTE ;
    ran ( rotateleft ) <: BYTE ;

    dom ( rotateright ) = BYTE ;
    ran ( rotateright ) <: BYTE ;

    dom ( get_upper_digit ) = BYTE ;
    ran ( get_upper_digit ) <: 0 .. ( 16 - 1 ) ;

    dom ( get_lower_digit ) = BYTE ;
    ran ( get_lower_digit ) <: 0 .. ( 16 - 1 ) ;


    [ 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 ] : BYTE &
    [ 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ] : BYTE &


    1      = 2 ** 0 &
    2      = 2 ** 1 &
    4      = 2 ** 2 &
    8      = 2 ** 3 &
    16     = 2 ** 4 &
    32     = 2 ** 5 &
    64     = 2 ** 6 &
    128    = 2 ** 7 &
    256    = 2 ** 8 &
    512    = 2 ** 9 &
    1024    = 2 ** 10 &
    2048    = 2 ** 11 &
    4096    = 2 ** 12 &
    8192    = 2 ** 13 &
    16384   = 2 ** 14 &
    32768   = 2 ** 15 &
    65536   = 2 ** 16



END
