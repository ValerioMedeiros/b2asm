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
    UCHAR_DEFINITION

SEES


    BIT_DEFINITION ,
    BYTE_DEFINITION ,
    POWER2















CONCRETE_CONSTANTS

   UCHAR_MAX ,
   UCHAR_MIN ,


   UCHAR ,
   uchar_byte ,
   byte_uchar

PROPERTIES

   UCHAR_MAX = 256    - 1 &
   UCHAR_MIN = 0  &

   UCHAR = ( UCHAR_MIN .. UCHAR_MAX ) &

   /*byte_uchar : BYTE  >->>  UCHAR &*/
   byte_uchar =  % ( v0 ) . ( v0 : BYTE |  128   * v0 ( 8 ) + 64 * v0 ( 7 ) + 32 * v0 ( 6 ) + 16 * v0 ( 5 ) +
   8      * v0 ( 4 ) + 4    * v0 ( 3 ) + 2  * v0 ( 2 ) + 1   * v0 ( 1 ) ) &

          /*uchar_byte :  UCHAR >->>  BYTE 	&	*/
        uchar_byte = % ( v0 ) . ( v0 : UCHAR  |
         [ ( v0 mod 2   ) / 1   ,
         ( v0 mod 4    ) / 2   ,
         ( v0 mod 8    ) / 4   ,
         ( v0 mod 16   ) / 8   ,
         ( v0 mod 32   ) / 16  ,
         ( v0 mod 64   ) / 32  ,
         ( v0 mod 128  ) / 64  ,
         ( v0 mod 256  ) / 128 ]  )



ASSERTIONS

    card ( byte_uchar ) = 256 &
    card ( uchar_byte ) = 256 &

                                                        /* ProB */

    UCHAR_MAX : INTEGER &
    UCHAR_MIN : INTEGER &

    1      = 2 ** 0 &
    2      = 2 ** 1 &
    4      = 2 ** 2 &
    8      = 2 ** 3 &
    16     = 2 ** 4 &
    32     = 2 ** 5 &
    64     = 2 ** 6 &
    128    = 2 ** 7 &
    256    = 2 ** 8


END
