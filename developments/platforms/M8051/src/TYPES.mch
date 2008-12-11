MACHINE
   TYPES

SEES
BYTE_DEFINITION,
BIT_DEFINITION,
BIT_VECTOR_ARITHMETICS,
BIT_VECTOR16_DEFINITION,
POWER2

CONSTANTS

UCHAR
,UCHAR_MAX

,SCHAR
,SCHAR_MAX
,SCHAR_MIN

,USHORT_INT
,USHORT_INT_MAX

,byte_schar
,schar_byte

,byte_uchar
,uchar_byte

,uchar_schar
,schar_uchar

,bv16_usint
,usint_bv16
PROPERTIES
UCHAR_MAX:NATURAL
&UCHAR_MAX=(2**BYTE_WIDTH)-1
&UCHAR = 0..UCHAR_MAX

&SCHAR_MAX : INTEGER
&SCHAR_MIN : INTEGER

&SCHAR_MAX = (2**(BYTE_WIDTH-1))-1
&SCHAR_MIN = -(2**(BYTE_WIDTH-1))
&SCHAR=SCHAR_MIN..SCHAR_MAX 

&USHORT_INT_MAX : INTEGER
&USHORT_INT_MAX = (2**(2*BYTE_WIDTH))-1
&USHORT_INT = 0..USHORT_INT_MAX

&byte_uchar : BYTE --> UCHAR
&byte_uchar = %(bv).(bv:BYTE | bv_to_nat(bv))
&uchar_byte=byte_uchar~

&byte_schar : BYTE --> SCHAR
&byte_schar = %(bv).(bv:BYTE |( -bv(7)*128+bv(6)*64+bv(5)*32+bv(4)*16+bv(3)*8+bv(2)*4+bv(1)*2+bv(0)*1))
&schar_byte=byte_schar~

&uchar_schar : UCHAR --> SCHAR
&uchar_schar=%(v1).(v1:UCHAR & v1<=SCHAR_MAX| v1) \/
			 %(v1).(v1:UCHAR & not(v1<=SCHAR_MAX) | v1-UCHAR_MAX )
&schar_uchar=uchar_schar~

&bv16_usint : BV16 --> USHORT_INT
&bv16_usint = %(v0).(v0:BV16 | 32768 * v0 ( 15 ) + 16384 * v0 ( 14 ) + 8192 * v0 ( 13 ) + 4096 * v0 ( 12 ) + 2048 * v0 ( 11 ) + 
    							1024 * v0 ( 10 ) +   512 * v0 ( 9 )  +  256 * v0 ( 8 ) +   128 * v0 ( 7 )  +   64 * v0 ( 6 )  + 
    							  32 * v0 ( 5 )  +    16 * v0 ( 4 )  +    8 * v0 ( 3 ) +     4 * v0 ( 2 )  +    2 * v0 ( 1 )  + v0 ( 0 ) )
&usint_bv16 = bv16_usint~
ASSERTIONS

dom(byte_schar)= BYTE
&ran(byte_schar)<:SCHAR

&dom(schar_byte)=SCHAR
&ran(schar_byte)<:BYTE

&dom(byte_uchar)=BYTE
&ran(byte_uchar)<:UCHAR

&dom(uchar_byte)=UCHAR
&ran(uchar_byte)<:BYTE

&dom(uchar_schar)=UCHAR
&ran(uchar_schar)<:SCHAR

&dom(schar_uchar)=SCHAR
&ran(schar_uchar)<:UCHAR





END