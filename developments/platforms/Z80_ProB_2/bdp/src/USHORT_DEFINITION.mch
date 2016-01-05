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
    USHORT_DEFINITION
SEES
    BIT_DEFINITION ,
    BYTE_DEFINITION ,
    BV16_DEFINITION ,
    UCHAR_DEFINITION ,
    POWER2
























CONCRETE_CONSTANTS

    USHORT_MAX ,
    USHORT_MIN ,

    USHORT ,
    bv16_ushort ,
    ushort_bv16 ,

    uchar_ushort


PROPERTIES

   USHORT_MAX =  ( 65536 ) - 1 &
   USHORT_MIN =  0 &

   USHORT = USHORT_MIN .. USHORT_MAX &

   /*bv16_ushort : BV16 >->> USHORT &  ProB simplification */
   bv16_ushort = % ( v0 ) . ( v0 : BV16 |
   32768  * v0 ( 16 ) + 16384 * v0 ( 15 ) + 8192 * v0 ( 14 ) + 4096 * v0 ( 13 ) +
   2048   * v0 ( 12 ) + 1024 * v0 ( 11 ) + 512 * v0 ( 10 ) + 256 * v0 ( 9 ) +
   128   * v0 ( 8 ) + 64 * v0 ( 7 ) + 32 * v0 ( 6 ) + 16 * v0 ( 5 ) +
   8      * v0 ( 4 ) + 4    * v0 ( 3 ) + 2  * v0 ( 2 ) + 1   * v0 ( 1 ) ) &

   /*ushort_bv16 : USHORT >->> BV16 &  ProB simplifcation */
   ushort_bv16 = % ( v0 ) . ( v0 : USHORT  |
            [   ( v0 mod 2    ) / 1   ,
                ( v0 mod 4    ) / 2   ,
                ( v0 mod 8    ) / 4   ,
                 ( v0 mod 16   ) / 8   ,
                 ( v0 mod 32   ) / 16  ,
                 ( v0 mod 64   ) / 32  ,
                 ( v0 mod 128  ) / 64  ,
                 ( v0 mod 256  ) / 128 ,

                 ( v0 mod 512  ) / 256 ,
                 ( v0 mod 1024  ) / 512 ,
                 ( v0 mod 2048  ) / 1024 ,
                 ( v0 mod 4096  ) / 2048 ,
                 ( v0 mod 8192  ) / 4096 ,
                 ( v0 mod 16384 ) / 8192 ,
                 ( v0 mod 32768 ) / 16384 ,
                 ( v0 mod 65536 ) / 32768         ] )


        &


   /*uchar_ushort: UCHAR * UCHAR --> USHORT & ProB simplification*/
   uchar_ushort = % ( w1 , w2 ) . ( w1 : UCHAR & w2 : UCHAR | bv16_ushort ( byte_bv16 ( uchar_byte ( w1 ) , uchar_byte ( w2 ) ) ) )



ASSERTIONS


    card ( bv16_ushort ) = 65536 &
    card ( ushort_bv16 ) = 65536 &


    USHORT_MAX : INTEGER &
    USHORT_MIN : INTEGER &



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
