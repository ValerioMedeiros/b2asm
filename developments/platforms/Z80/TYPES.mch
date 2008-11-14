MACHINE
   TYPES

INCLUDES
   BIT_DEFINITION ,
   BIT_VECTOR_DEFINITION,
   BYTE_DEFINITION
 
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
   UCHAR_TO_BYTE,
   BYTE_TO_UCHAR, 

   
   BV16,
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
   
   parity_bit_from_BYTE,

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
   SCHAR = - 128 .. 127 &
   SCHAR_POSITION = 0 .. ( SCHAR_LENGTH - 1 ) &
   UCHAR = ( 0 .. 255 ) &


   SSHORTINT_LENGTH : NATURAL &
   SSHORTINT_LENGTH = 16 &
   NB_SSHORTINTS : NATURAL &
   NB_SSHORTINTS = 2 ** SSHORTINT_LENGTH &
   SSHORTINT = - 32768 .. 32767 &
   SSHORTINT_POSITION = 0 .. ( SSHORTINT_LENGTH - 1 ) &


   BV16 <: BIT_VECTOR &
   BV16 = { vv | vv : BIT_VECTOR & size(vv) = SSHORTINT_LENGTH } &  
   

   BYTE_TO_UCHAR : BYTE  -->  UCHAR &
   BYTE_TO_UCHAR =  % ( v0 ) . 
   ( v0 : BYTE | - 128 * v0 ( 7 ) + 64 * v0 ( 6 ) + 32 * v0 ( 5 ) + 16 * v0 ( 4 ) + 8 * v0 ( 3 ) + 4 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) ) &
   UCHAR_TO_BYTE :  UCHAR  -->  BYTE &
   UCHAR_TO_BYTE = BYTE_TO_UCHAR ~ &
   
   
   USHORTINT = ( 0 .. 65535 ) &


   NB_INSTRUCTIONS : NATURAL &
   INST_SZ : NATURAL &
   INST_SZ = 16 &
   NB_INSTRUCTIONS = 2 ** INST_SZ &
   INSTRUCTION_MAX = NB_INSTRUCTIONS - 1 &
   INSTRUCTION = USHORTINT  &
   INSTRUCTION_NEXT : USHORTINT --> USHORTINT &
   INSTRUCTION_NEXT =
    { p0 , q0 | p0 : INSTRUCTION & q0 : INSTRUCTION & 0 <= p0 & p0 < NB_INSTRUCTIONS - 1 & q0 = p0 + 1 } \/ { NB_INSTRUCTIONS - 1 |-> 0 } &
   INSTRUCTION_JUMP = % ( p0 , e0 ) . ( p0 : INSTRUCTION & e0 : - 126 .. 129 | p0 + e0 ) &

   BYTE_TO_SCHAR : BYTE --> SCHAR &
   BYTE_TO_SCHAR = % ( v0 ) . ( v0 : BYTE | - 128 * v0 ( 7 ) + 64 * v0 ( 6 ) + 32 * v0 ( 5 ) + 16 * v0 ( 4 ) + 8 * v0 ( 3 ) + 4 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) ) &
   SCHAR_TO_BYTE : SCHAR --> BYTE &
   SCHAR_TO_BYTE = BYTE_TO_SCHAR ~ &

   BV16_TO_SSHORTINT : BV16 --> SSHORTINT &
   BV16_TO_SSHORTINT =
        % ( v0 ) . ( v0 : BV16 | - 32768 * v0 ( 15 ) + 16384 * v0 ( 14 ) + 8192 * v0 ( 13 ) + 4096 * v0 ( 12 ) + 2048 * v0 ( 11 ) + 1024 * v0 ( 10 ) + 512 * v0 ( 9 ) + 256 * v0 ( 8 ) +
                128 * v0 ( 7 ) + 64 * v0 ( 6 ) + 32 * v0 ( 5 ) + 16 * v0 ( 4 ) + 8 * v0 ( 3 ) + 4 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) ) &
   SSHORTINT_TO_BV16 : SSHORTINT --> BV16 &
   SSHORTINT_TO_BV16 = BV16_TO_SSHORTINT ~ &

   BYTE_TO_BV16 : ( BYTE * BYTE ) --> BV16 &
   BYTE_TO_BV16 =
           % ( v1 , v2 ) . ( v1 : BV16 & v2 : BV16 |
           { 0 |-> v2 ( 0 ) , 1 |-> v2 ( 1 ) , 2 |-> v2 ( 2 ) , 3 |-> v2 ( 3 ) , 4 |-> v2 ( 4 ) , 5 |-> v2 ( 5 ) , 6 |-> v2 ( 6 ) , 7 |-> v2 ( 7 ) ,
             8 |-> v1 ( 0 ) , 9 |-> v1 ( 1 ) , 10 |-> v1 ( 2 ) , 11 |-> v1 ( 3 ) , 12 |-> v1 ( 4 ) , 13 |-> v1 ( 5 ) , 14 |-> v1 ( 6 ) , 15 |-> v1 ( 7 ) } ) &
   BV16_TO_BYTE : BV16 --> ( BYTE * BYTE ) &
   BV16_TO_BYTE = (BYTE_TO_BV16) ~ &
   
   SCHAR_TO_SSHORTINT : SCHAR * SCHAR --> SSHORTINT &
   SCHAR_TO_SSHORTINT = % ( w1 , w2 ) . ( w1 : SCHAR & w2 : SCHAR |
   BV16_TO_SSHORTINT ( BYTE_TO_BV16 ( SCHAR_TO_BYTE ( w1 ) , SCHAR_TO_BYTE ( w2 ) ) ) ) &

   SSHORTINT_TO_SCHAR : SSHORTINT --> SCHAR * SCHAR &
   SSHORTINT_TO_SCHAR = SCHAR_TO_SSHORTINT ~ &


   BV16_TO_USHORTINT : BV16  -->  USHORTINT &
   BV16_TO_USHORTINT = % ( v0 ) . ( v0 : BV16 |
                - 32768 * v0 ( 15 ) + 16384 * v0 ( 14 ) + 8192 * v0 ( 13 ) + 4096 * v0 ( 12 ) + 2048 * v0 ( 11 ) + 1024 * v0 ( 10 ) + 512 * v0 ( 9 ) + 256 * v0 ( 8 ) +
                128 * v0 ( 7 ) + 64 * v0 ( 6 ) + 32 * v0 ( 5 ) + 16 * v0 ( 4 ) + 8 * v0 ( 3 ) + 4 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) ) &
   USHORTINT_TO_BV16 :  USHORTINT  -->  BV16 &
   USHORTINT_TO_BV16 = BV16_TO_USHORTINT ~ &
   
   
   	parity_bit_from_BYTE : BIT_VECTOR --> BIT 
	&parity_bit_from_BYTE = % (bv). (bv : BIT_VECTOR | (SIGMA idx . (idx : dom(bv) | bv(idx) )) mod 2 )&


   SSHORTINT_TO_USHORTINT : SSHORTINT --> USHORTINT &
   SSHORTINT_TO_USHORTINT = % ( v0 ) . ( v0 : SSHORTINT | v0 - 32768 ) &
   USHORTINT_TO_SSHORTINT  : USHORTINT -->  SSHORTINT &
   USHORTINT_TO_SSHORTINT = SSHORTINT_TO_USHORTINT ~



ASSERTIONS
   NB_SCHARS = 256 &
   ! ( n0 ) . ( n0 : SCHAR => 0 <= n0 ) &
   ! ( n0 ) . ( n0 : SCHAR => n0 <= 255 ) &
   2 ** 16 = 65536 &
   INSTRUCTION_NEXT : USHORTINT -->  USHORTINT &
   0 = SCHAR_TO_SSHORTINT ( 0 , 0 ) 


END
