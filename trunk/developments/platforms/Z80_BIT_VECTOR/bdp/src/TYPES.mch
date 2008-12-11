MACHINE
   TYPES

INCLUDES
   BIT_DEFINITION ,
   BIT_VECTOR_DEFINITION ,
   BYTE_DEFINITION ,
   BIT_VECTOR16_DEFINITION


SEES
    POWER2

CONCRETE_CONSTANTS


   SCHAR ,
   SCHAR_LENGTH ,
   SCHAR_POSITION ,
   NB_SCHARS ,
   BYTE_TO_SCHAR ,
   SCHAR_TO_BYTE ,


   UCHAR ,
   UCHAR_TO_BYTE ,
   BYTE_TO_UCHAR ,


   SSHORTINT ,
   SSHORTINT_LENGTH ,
   SSHORTINT_POSITION ,
   NB_SSHORTINTS ,
   BV16_TO_SSHORTINT ,
   SSHORTINT_TO_BV16 ,


   INST_SZ ,
   INSTRUCTION ,
   NB_INSTRUCTIONS ,
   INSTRUCTION_MAX ,
   INSTRUCTION_NEXT ,
   INSTRUCTION_JUMP ,


   BYTE_TO_BV16 ,
   BV16_TO_BYTE ,
   SCHAR_TO_SSHORTINT ,
   SSHORTINT_TO_SCHAR ,


   USHORTINT ,
   BV16_TO_USHORTINT ,
   USHORTINT_TO_BV16 ,

   USHORTINT_TO_SSHORTINT ,
   SSHORTINT_TO_USHORTINT



PROPERTIES

   SCHAR_LENGTH : NATURAL &
   SCHAR_LENGTH = 8 &
   NB_SCHARS : NATURAL &
   NB_SCHARS = 2 ** SCHAR_LENGTH &
   SCHAR = ( - 2 ** 7 ) .. ( 2 ** 7 - 1 ) &
   SCHAR_POSITION = 0 .. ( SCHAR_LENGTH - 1 ) &
   UCHAR = ( 0 .. ( 2 ** 8 - 1 ) ) &


   SSHORTINT_LENGTH : NATURAL &
   SSHORTINT_LENGTH = 16 &
   NB_SSHORTINTS : NATURAL &
   NB_SSHORTINTS = 2 ** SSHORTINT_LENGTH &
   SSHORTINT = ( - 2 ** 15 ) .. ( 2 ** 15 - 1 ) &
   SSHORTINT_POSITION = 0 .. ( SSHORTINT_LENGTH - 1 ) &



   BYTE_TO_UCHAR : BYTE  -->  UCHAR &
   BYTE_TO_UCHAR =  % ( v0 ) .
   ( v0 : BYTE |  2 ** 7 * v0 ( 7 ) + 2 ** 6 * v0 ( 6 ) + 2 ** 5 * v0 ( 5 ) + 2 ** 4 * v0 ( 4 ) + 2 ** 3 * v0 ( 3 ) + 2 ** 2 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) ) &
   UCHAR_TO_BYTE :  UCHAR  -->  BYTE &
   UCHAR_TO_BYTE = BYTE_TO_UCHAR ~ &


   USHORTINT = ( 0 .. ( 2 ** 16 - 1 ) ) &


   NB_INSTRUCTIONS : NATURAL &
   INST_SZ : NATURAL &
   INST_SZ = 16 &
   NB_INSTRUCTIONS = 2 ** INST_SZ &
   INSTRUCTION_MAX = NB_INSTRUCTIONS - 1 &
   INSTRUCTION = USHORTINT  &
   INSTRUCTION_NEXT : USHORTINT --> USHORTINT &
   INSTRUCTION_NEXT =
                        % ( w1 ) . ( w1 : USHORTINT | ( w1 + 1 ) mod 65535 ) &

   INSTRUCTION_JUMP = % ( p0 , e0 ) . ( p0 : INSTRUCTION & e0 : - 126 .. 129 | ( p0 + e0 ) mod 65535 ) &

   BYTE_TO_SCHAR : BYTE --> SCHAR &
   BYTE_TO_SCHAR = % ( v0 ) . ( v0 : BYTE | ( - 2 ** 7 ) * v0 ( 7 ) + 2 ** 6 * v0 ( 6 ) + 2 ** 5 * v0 ( 5 ) + 2 ** 4 * v0 ( 4 ) + 2 ** 3 * v0 ( 3 ) + 2 ** 2 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) ) &
   SCHAR_TO_BYTE : SCHAR --> BYTE &
   SCHAR_TO_BYTE = BYTE_TO_SCHAR ~ &

   BV16_TO_SSHORTINT : BV16 --> SSHORTINT &
   BV16_TO_SSHORTINT = % ( v0 ) . ( v0 : BV16 | - 2 ** 15 * v0 ( 15 ) + 2 ** 14 * v0 ( 14 ) + 2 ** 13 * v0 ( 13 ) + 2 ** 12 * v0 ( 12 ) + 2 ** 11 * v0 ( 11 ) + 2 ** 10 * v0 ( 10 ) + 2 ** 9 * v0 ( 9 ) + 2 ** 8 * v0 ( 8 ) + 2 ** 7 * v0 ( 7 ) + 2 ** 6 * v0 ( 6 ) + 2 ** 5 * v0 ( 5 ) + 2 ** 4 * v0 ( 4 ) + 2 ** 3 * v0 ( 3 ) + 2 ** 2 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) )
   &
   SSHORTINT_TO_BV16 : SSHORTINT --> BV16 &
   SSHORTINT_TO_BV16 = BV16_TO_SSHORTINT ~ &

   BYTE_TO_BV16 : ( BYTE * BYTE ) --> BV16 &
   BYTE_TO_BV16 =
           % ( v1 , v2 ) . ( v1 : BV16 & v2 : BV16 |
           { 0 |-> v2 ( 0 ) , 1 |-> v2 ( 1 ) , 2 |-> v2 ( 2 ) , 3 |-> v2 ( 3 ) , 4 |-> v2 ( 4 ) , 5 |-> v2 ( 5 ) , 6 |-> v2 ( 6 ) , 7 |-> v2 ( 7 ) ,
             8 |-> v1 ( 0 ) , 9 |-> v1 ( 1 ) , 10 |-> v1 ( 2 ) , 11 |-> v1 ( 3 ) , 12 |-> v1 ( 4 ) , 13 |-> v1 ( 5 ) , 14 |-> v1 ( 6 ) , 15 |-> v1 ( 7 ) } ) &
   BV16_TO_BYTE : BV16 --> ( BYTE * BYTE ) &
   BV16_TO_BYTE = ( BYTE_TO_BV16 ) ~ &

   SCHAR_TO_SSHORTINT : SCHAR * SCHAR --> SSHORTINT &
   SCHAR_TO_SSHORTINT = % ( w1 , w2 ) . ( w1 : SCHAR & w2 : SCHAR |
   BV16_TO_SSHORTINT ( BYTE_TO_BV16 ( SCHAR_TO_BYTE ( w1 ) , SCHAR_TO_BYTE ( w2 ) ) ) ) &

   SSHORTINT_TO_SCHAR : SSHORTINT --> SCHAR * SCHAR &
   SSHORTINT_TO_SCHAR = SCHAR_TO_SSHORTINT ~ &


   BV16_TO_USHORTINT : BV16  -->  USHORTINT &
   BV16_TO_USHORTINT = % ( v0 ) .
   ( v0 : BV16 | 2 ** 15 * v0 ( 15 ) + 2 ** 14 * v0 ( 14 ) + 2 ** 13 * v0 ( 13 ) + 2 ** 12 * v0 ( 12 ) + 2 ** 11 * v0 ( 11 ) + 2 ** 10 * v0 ( 10 ) + 2 ** 9 * v0 ( 9 ) + 2 ** 8 * v0 ( 8 ) + 2 ** 7 * v0 ( 7 ) + 2 ** 6 * v0 ( 6 ) + 2 ** 5 * v0 ( 5 ) + 2 ** 4 * v0 ( 4 ) + 2 ** 3 * v0 ( 3 ) + 2 ** 2 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) ) &
   USHORTINT_TO_BV16 :  USHORTINT  -->  BV16 &
   USHORTINT_TO_BV16 = BV16_TO_USHORTINT ~ &




   SSHORTINT_TO_USHORTINT : SSHORTINT --> USHORTINT &
   SSHORTINT_TO_USHORTINT = % ( v0 ) . ( v0 : SSHORTINT | v0 - 32768 ) &
   USHORTINT_TO_SSHORTINT  : USHORTINT -->  SSHORTINT &
   USHORTINT_TO_SSHORTINT = SSHORTINT_TO_USHORTINT ~



ASSERTIONS
   0 = SCHAR_TO_SSHORTINT ( 0 , 0 ) &
   NB_SCHARS = 256 &

   /* Por algum motivo que desconheco consegui provar
    (   ! ( n0 ) . ( n0 : SCHAR => 0 <= n0 ) & ) , estranho... */

   ! ( n0 ) . ( n0 : UCHAR => 0 <= n0 ) &
   ! ( n0 ) . ( n0 : UCHAR => n0 <= 255 ) &

   INSTRUCTION_NEXT : USHORTINT -->  USHORTINT &


   ! ( xx ) . ( xx : BYTE => BYTE_TO_UCHAR ( xx ) : UCHAR ) &
   ! ( xx ) . ( xx : UCHAR => UCHAR_TO_BYTE ( xx ) : BYTE ) &


   ! ( xx ) . ( xx : BYTE => BYTE_TO_SCHAR ( xx ) : SCHAR ) &
   ! ( xx ) . ( xx : SCHAR => SCHAR_TO_BYTE ( xx ) : BYTE ) &


   ! ( xx , yy ) . ( xx : BYTE & yy : BYTE => BYTE_TO_BV16 ( xx , yy ) : BV16 ) &
   ! ( xx , yy ) . ( xx : BYTE & yy : BYTE => # ( zz ) . ( zz : BV16 & BYTE_TO_BV16 ( xx , yy ) = zz ) ) &

   ! ( xx ) . ( xx : BV16 => BV16_TO_USHORTINT ( xx ) : USHORTINT ) &
   ! ( xx ) . ( xx : USHORTINT => USHORTINT_TO_BV16 ( xx ) : BV16 )


END
