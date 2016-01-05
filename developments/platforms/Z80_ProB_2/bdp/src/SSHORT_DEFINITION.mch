/*************************************************************************
Copyright (c) 2009,  David Deharbe , Valerio Medeiros Jr.

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
    SSHORT_DEFINITION

SEES
        BIT_DEFINITION ,
        BYTE_DEFINITION ,
        BV16_DEFINITION ,
        SCHAR_DEFINITION ,
        USHORT_DEFINITION ,
        POWER2























CONCRETE_CONSTANTS

   SSHORT_MIN ,
   SSHORT_MAX ,

   SSHORT ,

   bv16_sshort ,
   sshort_bv16 ,

   schar_sshort ,
 /*  sshort_schar,*/

   ushort_sshort ,
   sshort_ushort

PROPERTIES

   SSHORT_MIN = - ( 32768 ) &
   SSHORT_MAX = ( 32768 - 1 ) &

   SSHORT = SSHORT_MIN .. SSHORT_MAX &
   /*bv16_sshort : BV16 --> SSHORT &*/
   bv16_sshort = % ( v0 ) . ( v0 : BV16 | - 32768   * v0 ( 16 ) + 16384   * v0 ( 15 ) + 8192    * v0 ( 14 ) + 4096    *
                                                v0 ( 13 ) + 2048    * v0 ( 12 ) + 1024    * v0 ( 11 ) + 512    * v0 ( 10 ) + 256    * v0 ( 9 ) + 128    *
                                                v0 ( 8 ) + 64     * v0 ( 7 ) + 32     * v0 ( 6 ) + 16     * v0 ( 5 ) + 8      * v0 ( 4 ) + 4      * v0 ( 3 )
                                                + 2 * v0 ( 2 ) + v0 ( 1 ) )  &


  /* sshort_bv16 : SSHORT +-> BV16 &  ProB simplification */
   sshort_bv16 = % ( v0 ) . ( v0 : SSHORT &  v0 >= 0 |
        [ ( ( v0 ) mod 2 ) / 1   ,
             ( ( v0 ) mod 4  ) / 2   ,
             ( ( v0 ) mod 8  ) / 4   ,
         ( ( v0 ) mod 16 ) / 8   ,
         ( ( v0 ) mod 32 ) / 16  ,
         ( ( v0 ) mod 64 ) / 32  ,
         ( ( v0 ) mod 128 ) / 64 ,
         ( ( v0 ) mod 256 ) / 128 ,
         ( ( v0 ) mod 512 ) / 256 ,
         ( ( v0 ) mod 1024 ) / 512 ,
         ( ( v0 ) mod 2048 ) / 1024 ,
         ( ( v0 ) mod 4096 ) / 2048 ,
         ( ( v0 ) mod 8192 ) / 4096 ,
         ( ( v0 ) mod 16384 ) / 8192 ,
              ( ( v0 ) mod 32768 ) / 16384 ,
              ( ( v0 ) mod 65536 ) / 32768      ] ) \/
        % ( v0 ) . ( v0 : SSHORT & not ( v0 >= 0 ) |
        [ ( ( v0 + USHORT_MAX + 1 ) mod 2 ) / 1 ,
             ( ( v0 + USHORT_MAX + 1 ) mod 4 ) / 2  ,
             ( ( v0 + USHORT_MAX + 1 ) mod 8 ) / 4  ,
         ( ( v0 + USHORT_MAX + 1 ) mod 16 ) / 8 ,
         ( ( v0 + USHORT_MAX + 1 ) mod 32 ) / 16 ,
         ( ( v0 + USHORT_MAX + 1 ) mod 64 ) / 32 ,
         ( ( v0 + USHORT_MAX + 1 ) mod 128 ) / 64 ,
         ( ( v0 + USHORT_MAX + 1 ) mod 256 ) / 128 ,
         ( ( v0 + USHORT_MAX + 1 ) mod 512 ) / 256 ,
         ( ( v0 + USHORT_MAX + 1 ) mod 1024 ) / 512 ,
         ( ( v0 + USHORT_MAX + 1 ) mod 2048 ) / 1024 ,
         ( ( v0 + USHORT_MAX + 1 ) mod 4096 ) / 2048 ,
         ( ( v0 + USHORT_MAX + 1 ) mod 8192 ) / 4096 ,
         ( ( v0 + USHORT_MAX + 1 ) mod 16384 ) / 8192 ,
              ( ( v0 + USHORT_MAX + 1 ) mod 32768 ) / 16384 ,
              ( ( v0 + USHORT_MAX + 1 ) mod 65536 ) / 32768    ] )               &



    ushort_sshort = % ( v1 ) . ( v1 : USHORT & v1 <= SSHORT_MAX | v1 ) \/ % ( v1 ) . ( v1 : USHORT & not ( v1 <= SSHORT_MAX ) | v1 - USHORT_MAX - 1 ) &


    sshort_ushort = % ( v1 ) . ( v1 : SSHORT & v1 >= 0 | v1 ) \/
                  % ( v1 ) . ( v1 : SSHORT & not ( v1 >= 0 ) | v1 + USHORT_MAX + 1 )                   &


   schar_sshort = % ( w1 , w2 ) . ( w1 : SCHAR & w2 : SCHAR | ( bv16_sshort ( byte_bv16 ( schar_byte ( w1 ) , schar_byte ( w2 ) ) ) ) )



  /* sshort_schar  = % ( v0 ) . ( v0 : SSHORT &  v0>=0|
        byte_schar([((v0) mod pow2_1)/pow2_0,
	        ((v0) mod pow2_2)/pow2_1,
	        ((v0) mod pow2_3)/pow2_2,
                ((v0) mod pow2_4)/pow2_3,
                ((v0) mod pow2_5)/pow2_4,
                ((v0) mod pow2_6)/pow2_5,
                ((v0) mod pow2_7)/pow2_6, 
                ((v0) mod pow2_8)/pow2_7]) ,
         byte_schar([((v0) mod pow2_9)/pow2_8,
                ((v0) mod pow2_10)/pow2_9,
                ((v0) mod pow2_11)/pow2_10,
                ((v0) mod pow2_12)/pow2_11,
                ((v0) mod pow2_13)/pow2_12,
                ((v0) mod pow2_14)/pow2_13,
                ((v0) mod pow2_15)/pow2_14,
                ((v0) mod pow2_16)/pow2_15	])
 					)\/
  		 %( v0 ).( v0   card(ushort_sshort)=pow2_16 &
   card(sshort_ushort)=pow2_16 & : SSHORT & not(v0>=0) |
  		 (byte_schar([((v0+USHORT_MAX + 1) mod pow2_1)/pow2_0,
	     ((v0+USHORT_MAX + 1) mod pow2_2)/pow2_1,
	     ((v0+USHORT_MAX + 1) mod pow2_3)/pow2_2,
         ((v0+USHORT_MAX + 1) mod pow2_4)/pow2_3,
         ((v0+USHORT_MAX + 1) mod pow2_5)/pow2_4,
         ((v0+USHORT_MAX + 1) mod pow2_6)/pow2_5,
         ((v0+USHORT_MAX + 1) mod pow2_7)/pow2_6, 
         ((v0+USHORT_MAX + 1) mod pow2_8)/pow2_7])),
         (byte_schar([((v0+USHORT_MAX + 1) mod pow2_9)/pow2_8,
         ((v0+USHORT_MAX + 1) mod pow2_10)/pow2_9,
         ((v0+USHORT_   card(ushort_sshort)=pow2_16 &
   card(sshort_ushort)=pow2_16 &MAX + 1) mod pow2_11)/pow2_10,
         ((v0+USHORT_MAX + 1) mod pow2_12)/pow2_11,
         ((v0+USHORT_MAX + 1) mod pow2_13)/pow2_12,
         ((v0+USHORT_MAX + 1) mod pow2_14)/pow2_13,
 	     ((v0+USHORT_MAX + 1) mod pow2_15)/pow2_14,
 	     ((v0+USHORT_MAX + 1) mod pow2_16)/pow2_15	]))					
									)
*/



ASSERTIONS


    card ( bv16_sshort ) = 65536 &
    card ( sshort_bv16 ) = 65536 &


   SSHORT_MIN : SSHORT &
   SSHORT_MAX : SSHORT &





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
