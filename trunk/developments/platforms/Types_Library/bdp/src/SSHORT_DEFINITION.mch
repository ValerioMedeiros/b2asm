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





















CONCRETE_CONSTANTS

   SSHORT_MIN ,
   SSHORT_MAX ,

   SSHORT ,

   bv16_sshort ,
   sshort_bv16 ,

   schar_sshort ,
   sshort_schar

PROPERTIES

   SSHORT_MIN = - ( 32768 ) &
   SSHORT_MAX = ( 32768 - 1 ) &

   SSHORT = SSHORT_MIN .. SSHORT_MAX &
   bv16_sshort : BV16 --> SSHORT &
   bv16_sshort = % ( v0 ) . ( v0 : BV16 | - 32768   * v0 ( 16 ) + 16384   * v0 ( 15 ) + 8192    * v0 ( 14 ) + 4096    *
                                                v0 ( 13 ) + 2048    * v0 ( 12 ) + 1024    * v0 ( 11 ) + 512    * v0 ( 10 ) + 256    * v0 ( 9 ) + 128    *
                                                v0 ( 8 ) + 64     * v0 ( 7 ) + 32     * v0 ( 6 ) + 16     * v0 ( 5 ) + 8      * v0 ( 4 ) + 4      * v0 ( 3 ) + 2 * v0 ( 2 ) + v0 ( 1 ) )  &

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
