/* 
SSHORT_DEFINITION

	\brief
 		This machine define the type short
 	
Author: Valério Jr., Stephenson Galvão, David Déharbe
Creation date: Thu Feb 12 2009
 */
MACHINE
    SSHORT_DEFINITION

SEES
        BIT_DEFINITION ,
        BIT_VECTOR_DEFINITION ,
        BYTE_DEFINITION ,
    BV16_DEFINITION ,
    SCHAR_DEFINITION ,
    POWER2

DEFINITIONS

    pow2_0 == 1 ;
    pow2_1 == 2 ;
    pow2_2 == 4 ;
    pow2_3 == 8 ;
    pow2_4 == 16 ;
    pow2_5 == 32 ;
    pow2_6 == 64 ;
    pow2_7 == 128 ;
    pow2_8 == 256 ;
    pow2_9 == 512 ;
    pow2_10 == 1024 ;
    pow2_11 == 2048 ;
    pow2_12 == 4096 ;
    pow2_13 == 8192 ;
    pow2_14 == 16384 ;
    pow2_15 == 32768 ;
    pow2_16 == 65536

CONCRETE_CONSTANTS

   SSHORT_MIN ,
   SSHORT_MAX ,

   SSHORT ,

   bv16_sshort ,
   sshort_bv16 ,

   schar_sshort ,
   sshort_schar

PROPERTIES

   SSHORT_MIN = - ( pow2_15 ) &
   SSHORT_MAX = ( pow2_15 - 1 ) &

   SSHORT = SSHORT_MIN .. SSHORT_MAX &
   bv16_sshort : BV16 --> SSHORT &
   bv16_sshort = % ( v0 ) . ( v0 : BV16 | - pow2_15 * v0 ( 16 ) + pow2_14 * v0 ( 15 ) + pow2_13 * v0 ( 14 ) + pow2_12 *
                                                v0 ( 13 ) + pow2_11 * v0 ( 12 ) + pow2_10 * v0 ( 11 ) + pow2_9 * v0 ( 10 ) + pow2_8 * v0 ( 9 ) + pow2_7 *
                                                v0 ( 8 ) + pow2_6 * v0 ( 7 ) + pow2_5 * v0 ( 6 ) + pow2_4 * v0 ( 5 ) + pow2_3 * v0 ( 4 ) + pow2_2 * v0 ( 3 ) + 2 * v0 ( 2 ) + v0 ( 1 ) )  &

   sshort_bv16 : SSHORT --> BV16 &
   sshort_bv16 = bv16_sshort ~ &

   schar_sshort : SCHAR * SCHAR --> SSHORT &
   schar_sshort = % ( w1 , w2 ) . ( w1 : SCHAR & w2 : SCHAR |
                                              bv16_sshort ( byte_bv16 ( schar_byte ( w1 ) , schar_byte ( w2 ) ) ) ) &

   sshort_schar : SSHORT --> SCHAR * SCHAR &
   sshort_schar = schar_sshort ~


ASSERTIONS

           SSHORT_MIN : SSHORT &
           SSHORT_MAX : SSHORT &


    dom ( bv16_sshort ) = BV16 &
    ran ( bv16_sshort ) = SSHORT &

    dom ( sshort_bv16 ) = SSHORT &
    ran ( sshort_bv16 ) = BV16 &

    dom ( schar_sshort ) = ( SCHAR * SCHAR ) &
    ran ( schar_sshort ) = SSHORT &

    dom ( sshort_schar ) = SSHORT &
    ran ( sshort_schar ) = ( SCHAR * SCHAR ) &



    pow2_0 = 2 ** 0 &
    pow2_1 = 2 ** 1 &
    pow2_2 = 2 ** 2 &
    pow2_3 = 2 ** 3 &
    pow2_4 = 2 ** 4 &
    pow2_5 = 2 ** 5 &
    pow2_6 = 2 ** 6 &
    pow2_7 = 2 ** 7 &
    pow2_8 = 2 ** 8 &
    pow2_9 = 2 ** 9 &
    pow2_10 = 2 ** 10 &
    pow2_11 = 2 ** 11 &
    pow2_12 = 2 ** 12 &
    pow2_13 = 2 ** 13 &
    pow2_14 = 2 ** 14 &
    pow2_15 = 2 ** 15 &
    pow2_16 = 2 ** 16

END
