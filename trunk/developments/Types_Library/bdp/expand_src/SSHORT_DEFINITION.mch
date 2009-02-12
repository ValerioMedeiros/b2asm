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
    BV16_DEFINITION ,
    SCHAR_DEFINITION

DEFINITIONS

   SSHORT_MIN == ( - 2 ** 15 ) ;
   SSHORT_MAX == ( 2 ** 15 - 1 )


CONCRETE_CONSTANTS

   SSHORT ,

   bv16_sshort ,
   sshort_bv16 ,

   schar_sshort ,
   sshort_schar

PROPERTIES

    SSHORT = SSHORT_MIN .. SSHORT_MAX &
    bv16_sshort : BV16 --> SSHORT &
    bv16_sshort = % ( v0 ) . ( v0 : BV16 | - 2 ** 15 * v0 ( 15 ) + 2 ** 14 * v0 ( 14 ) + 2 ** 13 * v0 ( 13 ) + 2 ** 12 *
                                                v0 ( 12 ) + 2 ** 11 * v0 ( 11 ) + 2 ** 10 * v0 ( 10 ) + 2 ** 9 * v0 ( 9 ) + 2 ** 8 * v0 ( 8 ) + 2 ** 7 *
                                                v0 ( 7 ) + 2 ** 6 * v0 ( 6 ) + 2 ** 5 * v0 ( 5 ) + 2 ** 4 * v0 ( 4 ) + 2 ** 3 * v0 ( 3 ) + 2 ** 2 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) ) &

   sshort_bv16 : SSHORT --> BV16 &
   sshort_bv16 = bv16_sshort ~ &

   schar_sshort : SCHAR * SCHAR --> SSHORT &
   schar_sshort = % ( w1 , w2 ) . ( w1 : SCHAR & w2 : SCHAR |
                                              bv16_sshort ( byte_bv16 ( schar_byte ( w1 ) , schar_byte ( w2 ) ) ) ) &

   sshort_schar : SSHORT --> SCHAR * SCHAR &
   sshort_schar = schar_sshort ~

ASSERTIONS
    SSHORT_MIN : NATURAL ;
           SSHORT_MAX : NATURAL ;

           SSHORT_MIN : SSHORT ;
           SSHORT_MAX : SSHORT

END
