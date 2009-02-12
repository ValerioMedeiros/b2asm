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
   SCHAR_MAX ,
   SCHAR_MIN ,

   byte_to_schar ,
   schar_to_byte ,


   UCHAR ,
   UCHAR_MAX ,
   UCHAR_MIN ,
   uchar_to_byte ,
   byte_to_uchar ,

  update_refresh_reg ,

   uchar_schar ,
   schar_uchar ,

   SSHORTINT ,
   bv16_to_sshortint ,
   sshortint_to_bv16 ,


   INST_SZ ,
   INSTRUCTION ,
   NB_INSTRUCTIONS ,
   INSTRUCTION_MAX ,
   instruction_next ,
   instruction_jump ,


   byte_to_bv16 ,
   bv16_to_byte ,
   schar_to_sshortint ,
   sshortint_to_schar ,


   USHORTINT ,
   USHORTINT_MAX ,
   USHORTINT_MIN ,
   bv16_to_ushortint ,
   ushortint_to_bv16 ,

   ushortint_to_sshortint ,
   sshortint_to_ushortint ,

   get_upper_digit ,
   get_lower_digit

PROPERTIES


   SCHAR_MAX : INTEGER &
   SCHAR_MIN : INTEGER &
   SCHAR_MAX = ( 2 ** 7 - 1 ) &
   SCHAR_MIN = ( - 2 ** 7 ) &
   SCHAR = ( - 2 ** 7 ) .. ( 2 ** 7 - 1 ) &


   UCHAR = ( 0 .. ( 2 ** 8 - 1 ) ) &
   UCHAR_MAX : INTEGER &
   UCHAR_MIN : INTEGER &
   UCHAR_MAX = 2 ** 8 &
   UCHAR_MIN = 0 &

   uchar_schar : UCHAR --> SCHAR &
   uchar_schar = % ( v1 ) . ( v1 : UCHAR & v1 <= SCHAR_MAX | v1 ) &
   uchar_schar = % ( v1 ) . ( v1 : UCHAR & not ( v1 <= SCHAR_MAX ) | v1 - UCHAR_MAX + 1 ) &
   schar_uchar : SCHAR --> UCHAR &
   schar_uchar = uchar_schar ~ &

   SSHORTINT = ( - 2 ** 15 ) .. ( 2 ** 15 - 1 ) &

   byte_to_uchar : BYTE  -->  UCHAR &
   byte_to_uchar =  % ( v0 ) .
   ( v0 : BYTE |  2 ** 7 * v0 ( 7 ) + 2 ** 6 * v0 ( 6 ) + 2 ** 5 * v0 ( 5 ) + 2 ** 4 * v0 ( 4 ) + 2 ** 3 * v0 ( 3 ) + 2 ** 2 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) ) &
   uchar_to_byte :  UCHAR  -->  BYTE &
   uchar_to_byte = byte_to_uchar ~ &

   update_refresh_reg   : BYTE --> BYTE &
   update_refresh_reg =  % ( v0 ) .  ( v0 : BYTE | uchar_to_byte ( 2 ** 7 * v0 ( 7 ) + ( ( 2 ** 6 * v0 ( 6 ) + 2 ** 5 * v0 ( 5 ) + 2 ** 4 * v0 ( 4 ) +
                2 ** 3 * v0 ( 3 ) + 2 ** 2 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) ) mod 64 ) ) ) &

   USHORTINT = ( 0 .. ( 2 ** 16 - 1 ) ) &
   USHORTINT_MAX =    ( 2 ** 16 - 1 ) &
   USHORTINT_MIN =    0 &

   NB_INSTRUCTIONS : NATURAL &
   INST_SZ : NATURAL &
   INST_SZ = 16 &
   NB_INSTRUCTIONS = 2 ** INST_SZ &
   INSTRUCTION_MAX = NB_INSTRUCTIONS - 1 &
   INSTRUCTION = USHORTINT  &
   instruction_next : USHORTINT --> USHORTINT &
   instruction_next =
   % ( w1 ) . ( w1 : USHORTINT | ( w1 + 1 ) mod 65535 ) &

   instruction_jump = % ( p0 , e0 ) . ( p0 : INSTRUCTION & e0 : - 126 .. 129 | ( p0 + e0 ) mod 65535 ) &

   byte_to_schar : BYTE --> SCHAR &
   byte_to_schar = % ( v0 ) . ( v0 : BYTE | ( - 2 ** 7 ) * v0 ( 7 ) + 2 ** 6 * v0 ( 6 ) + 2 ** 5 * v0 ( 5 ) + 2 ** 4 * v0 ( 4 ) + 2 ** 3 * v0 ( 3 ) + 2 ** 2 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) ) &
   schar_to_byte : SCHAR --> BYTE &
   schar_to_byte = byte_to_schar ~ &

   bv16_to_sshortint : BV16 --> SSHORTINT &
   bv16_to_sshortint = % ( v0 ) . ( v0 : BV16 | - 2 ** 15 * v0 ( 15 ) + 2 ** 14 * v0 ( 14 ) + 2 ** 13 * v0 ( 13 ) + 2 ** 12 * v0 ( 12 ) + 2 ** 11 * v0 ( 11 ) + 2 ** 10 * v0 ( 10 ) + 2 ** 9 * v0 ( 9 ) + 2 ** 8 * v0 ( 8 ) + 2 ** 7 * v0 ( 7 ) + 2 ** 6 * v0 ( 6 ) + 2 ** 5 * v0 ( 5 ) + 2 ** 4 * v0 ( 4 ) + 2 ** 3 * v0 ( 3 ) + 2 ** 2 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) )
   &
   sshortint_to_bv16 : SSHORTINT --> BV16 &
   sshortint_to_bv16 = bv16_to_sshortint ~ &

   byte_to_bv16 : ( BYTE * BYTE ) --> BV16 &
   byte_to_bv16 =
   % ( v1 , v2 ) . ( v1 : BV16 & v2 : BV16 |
      { 0 |-> v2 ( 0 ) , 1 |-> v2 ( 1 ) , 2 |-> v2 ( 2 ) , 3 |-> v2 ( 3 ) , 4 |-> v2 ( 4 ) , 5 |-> v2 ( 5 ) , 6 |-> v2 ( 6 ) , 7 |-> v2 ( 7 ) ,
         8 |-> v1 ( 0 ) , 9 |-> v1 ( 1 ) , 10 |-> v1 ( 2 ) , 11 |-> v1 ( 3 ) , 12 |-> v1 ( 4 ) , 13 |-> v1 ( 5 ) , 14 |-> v1 ( 6 ) , 15 |-> v1 ( 7 ) } ) &
   bv16_to_byte : BV16 --> ( BYTE * BYTE ) &
   bv16_to_byte = ( byte_to_bv16 ) ~ &

   schar_to_sshortint : SCHAR * SCHAR --> SSHORTINT &
   schar_to_sshortint = % ( w1 , w2 ) . ( w1 : SCHAR & w2 : SCHAR |
      bv16_to_sshortint ( byte_to_bv16 ( schar_to_byte ( w1 ) , schar_to_byte ( w2 ) ) ) ) &

   sshortint_to_schar : SSHORTINT --> SCHAR * SCHAR &
   sshortint_to_schar = schar_to_sshortint ~ &


   bv16_to_ushortint : BV16  -->  USHORTINT &
   bv16_to_ushortint = % ( v0 ) .
   ( v0 : BV16 | 2 ** 15 * v0 ( 15 ) + 2 ** 14 * v0 ( 14 ) + 2 ** 13 * v0 ( 13 ) + 2 ** 12 * v0 ( 12 ) + 2 ** 11 * v0 ( 11 ) + 2 ** 10 * v0 ( 10 ) + 2 ** 9 * v0 ( 9 ) + 2 ** 8 * v0 ( 8 ) + 2 ** 7 * v0 ( 7 ) + 2 ** 6 * v0 ( 6 ) + 2 ** 5 * v0 ( 5 ) + 2 ** 4 * v0 ( 4 ) + 2 ** 3 * v0 ( 3 ) + 2 ** 2 * v0 ( 2 ) + 2 * v0 ( 1 ) + v0 ( 0 ) ) &
   ushortint_to_bv16 :  USHORTINT  -->  BV16 &
   ushortint_to_bv16 = bv16_to_ushortint ~ &




   sshortint_to_ushortint : SSHORTINT --> USHORTINT &
   sshortint_to_ushortint = % ( v0 ) . ( v0 : SSHORTINT | v0 - 32768 ) &
   ushortint_to_sshortint  : USHORTINT -->  SSHORTINT &
   ushortint_to_sshortint = sshortint_to_ushortint ~ &

   get_upper_digit = % ( by ) . ( by : BYTE | 2 ** 3 * by ( 7 ) + 2 ** 2 * by ( 6 ) + 2 * by ( 5 ) + by ( 4 ) ) &
   get_lower_digit = % ( by ) . ( by : BYTE | 2 ** 3 * by ( 3 ) + 2 ** 2 * by ( 2 ) + 2 * by ( 1 ) + by ( 0 ) )

ASSERTIONS
   0 = schar_to_sshortint ( 0 , 0 ) &


   /* Por algum motivo que desconheco consegui provar
   (   ! ( n0 ) . ( n0 : SCHAR => 0 <= n0 ) & ) , estranho... */

   ! ( n0 ) . ( n0 : UCHAR => 0 <= n0 ) &
   ! ( n0 ) . ( n0 : UCHAR => n0 <= 255 ) &

   instruction_next : USHORTINT -->  USHORTINT &


   ! ( xx ) . ( xx : BYTE => byte_to_uchar ( xx ) : UCHAR ) &
   ! ( xx ) . ( xx : UCHAR => uchar_to_byte ( xx ) : BYTE ) &

  ! ( xx ) . ( xx : BYTE => update_refresh_reg ( xx ) : BYTE ) &

   ! ( xx ) . ( xx : BYTE => byte_to_schar ( xx ) : SCHAR ) &
   ! ( xx ) . ( xx : SCHAR => schar_to_byte ( xx ) : BYTE ) &


   ! ( xx , yy ) . ( xx : BYTE & yy : BYTE => byte_to_bv16 ( xx , yy ) : BV16 ) &
   ! ( xx , yy ) . ( xx : BYTE & yy : BYTE => # ( zz ) . ( zz : BV16 & byte_to_bv16 ( xx , yy ) = zz ) ) &

   ! ( xx ) . ( xx : BV16 => bv16_to_ushortint ( xx ) : USHORTINT ) &
   ! ( xx ) . ( xx : USHORTINT => ushortint_to_bv16 ( xx ) : BV16 ) &

   ! ( xx ) . ( xx : BYTE => get_upper_digit ( xx ) : 0 .. 16 ) &
      ! ( xx ) . ( xx : BYTE => get_lower_digit ( xx ) : 0 .. 16 )


END
