MACHINE
   TYPES

SEES
TYPE_BV8,BIT_DEFINITION

CONSTANTS

UCHAR
,UCHAR_MAX

,SCHAR
,SCHAR_MAX
,SCHAR_MIN

,byte_schar
,schar_byte

,byte_uchar
,uchar_byte

,uchar_schar
,schar_uchar
	
PROPERTIES
UCHAR_MAX:NATURAL
&UCHAR_MAX=(2**BYTE_WIDTH)-1
&UCHAR = 0..UCHAR_MAX

&SCHAR_MAX : INTEGER
&SCHAR_MIN : INTEGER

&SCHAR_MAX = -(2**BYTE_WIDTH)
&SCHAR_MIN = (2**BYTE_WIDTH)-1
&SCHAR=SCHAR_MIN..SCHAR_MAX 

&byte_uchar : BYTE --> UCHAR
&byte_uchar = %(bv).(bv:BYTE | bv(7)*128+bv(6)*64+bv(5)*32+bv(4)*16+bv(3)*8+bv(2)*4+bv(1)*2+bv(0)*1)
&uchar_byte=byte_uchar~

&byte_schar : BYTE --> SCHAR
&byte_schar = %(bv).(bv:BYTE |( -bv(7)*128+bv(6)*64+bv(5)*32+bv(4)*16+bv(3)*8+bv(2)*4+bv(1)*2+bv(0)*1))
&schar_byte=byte_schar~

&uchar_schar : UCHAR --> SCHAR
&uchar_schar=%(v1).(v1:UCHAR | v1-UCHAR_MAX* bool_to_bit(bool(v1 > (SCHAR_MAX))) )

&schar_uchar=uchar_schar~

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