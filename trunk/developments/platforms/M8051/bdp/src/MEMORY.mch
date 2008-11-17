/*
To Do

- Conjunto de bits endereçáveis para colocar como restrição nas operações que manipulam bits
- Passar o PC para MEMORY.mch ( acho que não pode)
- Ver se é melhor uma memoria que armazene UCHAR no lugar de BYTES
- Implementar a Program Memory

*/

/*
This machine represente and specific the memory of 8051, letting memory addresses, and divide then in Speficia Funcion Register (SFR)
and  General Purpose Registe (RAM) as like as it also specific the available address of program memory.
It also implement operations that are necessary for change state memory and ensure that the properties of memory will be hold.
*/
MACHINE
   MEMORY

SEES
BYTE_DEFINITION ,
BIT_DEFINITION ,
TYPES ,
BIT_VECTOR_DEFINITION ,
BIT_VECTOR_ARITHMETICS ,
POWER2

CONCRETE_CONSTANTS
 /*
 	Length of room in Kilobytes
 */
 ROM_LENGTH

 /*
 	Avaliables address for Program Memory. Memory read only
 */
, ROM_ADDR
, ROM_ADDR_MAX

/*
  Available address for Data Memory
*/
, MEM_ADDR

/*
 Part of data memory that have general function registers. This area of memory is accessed by direct and indirect addressing
*/
, RAM_ADDR
, RAM_MIN
, RAM_MAX

/*
Byte in RAM that are bit addressable
*/
, RAM_BIT_ADDRESSABLE

/*
Old implementation
,REGISTERS_ADDR
,register_to_nat
,register_addr
*/

/*
Area of data memory that have special function registers.This area of memory is accessed only by direct addressing
*/
, SFR_ADDR
, SFR_MIN
, SFR_MAX

/*
Byte in SFR that are bit addressable
*/
, SFR_BIT_ADDRESSABLE

/*
Set that implement the bit location
*/
, BIT_ADDRESS

/*
Bits addressable in data memory
*/
, BIT_ADDRESSABLE


/*
,INSTRUCTION
,INSTRUCTION_LEN
,INSTRUCTION_MAX
*/

, STACK_MAX
, PSWUPDATE
, BIT_GET

/* bit's address start with letter B */
, SBUF
, PCON
, SP

/*SCON*/
, SCON
, B98
, B99
, B9A
, B9B
, B9C
, B9D
, B9E
, B9F
, SCON_0
, SCON_1
, SCON_2
, SCON_3
, SCON_4
, SCON_5
, SCON_6
, SCON_7
, RI
, TI
, RB8
, TB8

, REN
, SM2
, SM1
, SM0


/*
Registers 
*/

, R0
, R1
, R2
, R3
, R4
, R5
, R6
, R7

/*Acumulator*/
, AA
, ACC
, BE0
, BE1
, BE2
, BE3
, BE4
, BE5
, BE6
, BE7

/*B*/
, BB
, BF0
, BF1
, BF2
, BF3
, BF4
, BF5
, BF6
, BF7

/*P0*/
, P0
, P0_0
, P0_1
, P0_2
, P0_3
, P0_4
, P0_5
, P0_6
, P0_7
, B80
, B81
, B82
, B83
, B84
, B85
, B86
, B87

/*P1*/
, P1
, P1_0
, P1_1
, P1_2
, P1_3
, P1_4
, P1_5
, P1_6
, P1_7
, B90
, B91
, B92
, B93
, B94
, B95
, B96
, B97

/*P2*/
, P2
, P2_0
, P2_1
, P2_2
, P2_3
, P2_4
, P2_5
, P2_6
, P2_7
, BA0
, BA1
, BA2
, BA3
, BA4
, BA5
, BA6
, BA7

/*P3*/
, P3
, P3_0
, P3_1
, P3_2
, P3_3
, P3_4
, P3_5
, P3_6
, P3_7
, BB0
, BB1
, BB2
, BB3
, BB4
, BB5
, BB6
, BB7

/*
Program Status Word (PSW) 
Most important register of microcontroler, it contain a several bits status that reflect the current state of CPU. 
*/
, PSW

/* Bit of PSW that indict the accumulator parity. Bit 0 of PSW*/
, PP
/* Bit of PSW that indict the overflow in arithmetic operations. Bit 2 of PSW*/
, OV
/* Bits of PSW that select the bank of register used. The 8051 have 4 bank of registers, with four registers both. Bits 3 and 4 of PSW*/
, RS0
, RS1
/* Bit of PSW of general purpose. Bit 5 of PSW*/
, F0
/* Bit of PSW that indict the half carry on addition operations. Bit 6 of PSW*/
, AC
/* Bit of PSW that indict the carry out of ALU operations. Bit 7 of PSW*/
, CY
/* Other name for CY*/
, CC

/*
Other names for PSW's bits 
*/
, BD0
, BD1
, BD2
, BD3
, BD4
, BD5
, BD6
, BD7

/*
Other names for PSW's bits 
*/
, PSW_0
, PSW_1
, PSW_2
, PSW_3
, PSW_4
, PSW_5
, PSW_6
, PSW_7


/*Funções Auxiliares*/
, ADDRCHANGE
, BITCHANGE
, PC_INCREMENT
, SP_INCREMENT
, SP_DECREMENT

/*
Old implementation
SETS
    REGISTERS ={R0,R1,R2,R3,R4,R5,R6,R7}
*/

PROPERTIES
ROM_LENGTH = 4

& ROM_ADDR_MAX = ROM_LENGTH * ( 2 ** 10 ) /* (2**10)= 1 KByte*/
& ROM_ADDR = 0 .. ROM_ADDR_MAX

& MEM_ADDR = UCHAR

& RAM_MIN : UCHAR
& RAM_MAX : UCHAR
& RAM_MIN = 0
& RAM_MAX = 127

/*Área de memória acessada direta ou indiretamente */
& RAM_ADDR = RAM_MIN .. RAM_MAX

/*
Bytes da RAM que s~ao possui bits endereç´aveis
*/
& RAM_BIT_ADDRESSABLE = 32 .. 47

/*Registers
*/
& R0 : RAM_ADDR
& R1 : RAM_ADDR
& R2 : RAM_ADDR
& R3 : RAM_ADDR
& R4 : RAM_ADDR
& R5 : RAM_ADDR
& R6 : RAM_ADDR
& R7 : RAM_ADDR

& R0 = 0
& R1 = 1
& R2 = 2
& R3 = 3
& R4 = 4
& R5 = 5
& R6 = 6
& R7 = 7

/*
Área de memória dos bancos de registradores, o 8051 possui 4 bancos com 8 registradores cada, R0 até R7.
Localizados nos endereços de memória de 0 a 1F.

&REGISTERS_ADDR = 0..32


&register_to_nat : REGISTERS --> 0..7
&register_to_nat = {R0|->0,R1|->1,R2|->2,R3|->3,R4|->4,R5|->5,R6|->6,R7|->7}
*/

/*
Função para calcular o endereço do registrador desejado, pois o banco de registradores que será utilizado depende
dos bits RS1 e RS0 do registrado PSW

&register_addr : REGISTERS*BYTE --> REGISTERS_ADDR
&register_addr = %(rg,psw).(rg:REGISTERS & psw:BYTE | bv_to_nat(bv_sub(psw,3,4))*register_to_nat(rg))
*/

& SFR_MIN : UCHAR
& SFR_MAX : UCHAR
& SFR_MIN = RAM_MAX + 1
& SFR_MAX = UCHAR_MAX

/*Área de memória acessada diretamente*/
& SFR_ADDR = SFR_MIN .. SFR_MAX

/*
Bytes in SFR that are bit addressable
*/
& SFR_BIT_ADDRESSABLE = { ba | ba : SFR_ADDR & ba >= 128 & ba <= 240 & ba mod 8 = 0 }

& BIT_ADDRESS = MEM_ADDR * BYTE_INDEX

/*

*/
& BIT_ADDRESSABLE = { addr , ind | addr : MEM_ADDR & ind : BYTE_INDEX & addr : ( SFR_BIT_ADDRESSABLE \/ RAM_BIT_ADDRESSABLE ) }


& SBUF : SFR_ADDR
& SP : SFR_ADDR
& PCON : SFR_ADDR

& SBUF = 153
& SP = 109
& PCON = 135




/*B*/
& BB : SFR_ADDR
& BB = 240
& BF0 : BIT_ADDRESS
& BF1 : BIT_ADDRESS
& BF2 : BIT_ADDRESS
& BF3 : BIT_ADDRESS
& BF4 : BIT_ADDRESS
& BF5 : BIT_ADDRESS
& BF6 : BIT_ADDRESS
& BF7 : BIT_ADDRESS

& BF0 = ( BB , 0 )
& BF1 = ( BB , 1 )
& BF2 = ( BB , 2 )
& BF3 = ( BB , 3 )
& BF4 = ( BB , 4 )
& BF5 = ( BB , 5 )
& BF6 = ( BB , 6 )
& BF7 = ( BB , 7 )

/*Acumulator*/
& AA : SFR_ADDR
& AA = 224
& ACC = AA
& BE0 : BIT_ADDRESS
& BE1 : BIT_ADDRESS
& BE2 : BIT_ADDRESS
& BE3 : BIT_ADDRESS
& BE4 : BIT_ADDRESS
& BE5 : BIT_ADDRESS
& BE6 : BIT_ADDRESS
& BE7 : BIT_ADDRESS

& BE0 = ( ACC , 0 )
& BE1 = ( ACC , 1 )
& BE2 = ( ACC , 2 )
& BE3 = ( ACC , 3 )
& BE4 = ( ACC , 4 )
& BE5 = ( ACC , 5 )
& BE6 = ( ACC , 6 )
& BE7 = ( ACC , 7 )


/*P0*/
& P0 : SFR_ADDR
& P0 = 128
& P0_0 : BIT_ADDRESS
& P0_1 : BIT_ADDRESS
& P0_2 : BIT_ADDRESS
& P0_3 : BIT_ADDRESS
& P0_4 : BIT_ADDRESS
& P0_5 : BIT_ADDRESS
& P0_6 : BIT_ADDRESS
& P0_7 : BIT_ADDRESS
& B80 : BIT_ADDRESS
& B81 : BIT_ADDRESS
& B82 : BIT_ADDRESS
& B83 : BIT_ADDRESS
& B84 : BIT_ADDRESS
& B85 : BIT_ADDRESS
& B86 : BIT_ADDRESS
& B87 : BIT_ADDRESS

& P0_0 = ( P0 , 0 )
& P0_1 = ( P0 , 1 )
& P0_2 = ( P0 , 2 )
& P0_3 = ( P0 , 3 )
& P0_4 = ( P0 , 4 )
& P0_5 = ( P0 , 5 )
& P0_6 = ( P0 , 6 )
& P0_7 = ( P0 , 7 )
& B80 = ( P0 , 0 )
& B81 = ( P0 , 1 )
& B82 = ( P0 , 2 )
& B83 = ( P0 , 3 )
& B84 = ( P0 , 4 )
& B85 = ( P0 , 5 )
& B86 = ( P0 , 6 )
& B87 = ( P0 , 7 )

/*P1*/
& P1 : SFR_ADDR
& P1 = 144
& P1_0 : BIT_ADDRESS
& P1_1 : BIT_ADDRESS
& P1_2 : BIT_ADDRESS
& P1_3 : BIT_ADDRESS
& P1_4 : BIT_ADDRESS
& P1_5 : BIT_ADDRESS
& P1_6 : BIT_ADDRESS
& P1_7 : BIT_ADDRESS
& B90 : BIT_ADDRESS
& B91 : BIT_ADDRESS
& B92 : BIT_ADDRESS
& B93 : BIT_ADDRESS
& B94 : BIT_ADDRESS
& B95 : BIT_ADDRESS
& B96 : BIT_ADDRESS
& B97 : BIT_ADDRESS

& P1_0 = ( P1 , 0 )
& P1_1 = ( P1 , 1 )
& P1_2 = ( P1 , 2 )
& P1_3 = ( P1 , 3 )
& P1_4 = ( P1 , 4 )
& P1_5 = ( P1 , 5 )
& P1_6 = ( P1 , 6 )
& P1_7 = ( P1 , 7 )
& B90 = ( P1 , 0 )
& B91 = ( P1 , 1 )
& B92 = ( P1 , 2 )
& B93 = ( P1 , 3 )
& B94 = ( P1 , 4 )
& B95 = ( P1 , 5 )
& B96 = ( P1 , 6 )
& B97 = ( P1 , 7 )

/*P2*/
& P2 : SFR_ADDR
& P2 = 160
& P2_0 : BIT_ADDRESS
& P2_1 : BIT_ADDRESS
& P2_2 : BIT_ADDRESS
& P2_3 : BIT_ADDRESS
& P2_4 : BIT_ADDRESS
& P2_5 : BIT_ADDRESS
& P2_6 : BIT_ADDRESS
& P2_7 : BIT_ADDRESS
& BA0 : BIT_ADDRESS
& BA1 : BIT_ADDRESS
& BA2 : BIT_ADDRESS
& BA3 : BIT_ADDRESS
& BA4 : BIT_ADDRESS
& BA5 : BIT_ADDRESS
& BA6 : BIT_ADDRESS
& BA7 : BIT_ADDRESS

& P2_0 = ( P2 , 0 )
& P2_1 = ( P2 , 1 )
& P2_2 = ( P2 , 2 )
& P2_3 = ( P2 , 3 )
& P2_4 = ( P2 , 4 )
& P2_5 = ( P2 , 5 )
& P2_6 = ( P2 , 6 )
& P2_7 = ( P2 , 7 )
& BA0 = ( P2 , 0 )
& BA1 = ( P2 , 1 )
& BA2 = ( P2 , 2 )
& BA3 = ( P2 , 3 )
& BA4 = ( P2 , 4 )
& BA5 = ( P2 , 5 )
& BA6 = ( P2 , 6 )
& BA7 = ( P2 , 7 )

/*P3*/
& P3 : SFR_ADDR
& P3 = 176
& P3_0 : BIT_ADDRESS
& P3_1 : BIT_ADDRESS
& P3_2 : BIT_ADDRESS
& P3_3 : BIT_ADDRESS
& P3_4 : BIT_ADDRESS
& P3_5 : BIT_ADDRESS
& P3_6 : BIT_ADDRESS
& P3_7 : BIT_ADDRESS
& BB0 : BIT_ADDRESS
& BB1 : BIT_ADDRESS
& BB2 : BIT_ADDRESS
& BB3 : BIT_ADDRESS
& BB4 : BIT_ADDRESS
& BB5 : BIT_ADDRESS
& BB6 : BIT_ADDRESS
& BB7 : BIT_ADDRESS

& P3_0 = ( P3 , 0 )
& P3_1 = ( P3 , 1 )
& P3_2 = ( P3 , 2 )
& P3_3 = ( P3 , 3 )
& P3_4 = ( P3 , 4 )
& P3_5 = ( P3 , 5 )
& P3_6 = ( P3 , 6 )
& P3_7 = ( P3 , 7 )
& BB0 = ( P3 , 0 )
& BB1 = ( P3 , 1 )
& BB2 = ( P3 , 2 )
& BB3 = ( P3 , 3 )
& BB4 = ( P3 , 4 )
& BB5 = ( P3 , 5 )
& BB6 = ( P3 , 6 )
& BB7 = ( P3 , 7 )

/*SCON- SERIAL PORT CONTROL REGISTER */
& SCON : SFR_ADDR
& SCON = 152
& B98 : BIT_ADDRESS
& B99 : BIT_ADDRESS
& B9A : BIT_ADDRESS
& B9B : BIT_ADDRESS
& B9C : BIT_ADDRESS
& B9D : BIT_ADDRESS
& B9E : BIT_ADDRESS
& B9F : BIT_ADDRESS
& B98 = ( SCON , 0 )
& B99 = ( SCON , 1 )
& B9A = ( SCON , 2 )
& B9B = ( SCON , 3 )
& B9C = ( SCON , 4 )
& B9D = ( SCON , 5 )
& B9E = ( SCON , 6 )
& B9F = ( SCON , 7 )


& SCON_0 : BIT_ADDRESS
& SCON_1 : BIT_ADDRESS
& SCON_2 : BIT_ADDRESS
& SCON_3 : BIT_ADDRESS
& SCON_4 : BIT_ADDRESS
& SCON_5 : BIT_ADDRESS
& SCON_6 : BIT_ADDRESS
& SCON_7 : BIT_ADDRESS
& SCON_0 = ( SCON , 0 )
& SCON_1 = ( SCON , 1 )
& SCON_2 = ( SCON , 2 )
& SCON_3 = ( SCON , 3 )
& SCON_4 = ( SCON , 4 )
& SCON_5 = ( SCON , 5 )
& SCON_6 = ( SCON , 6 )
& SCON_7 = ( SCON , 7 )

& RI : BIT_ADDRESS
& TI : BIT_ADDRESS
& RB8 : BIT_ADDRESS
& TB8 : BIT_ADDRESS
& REN : BIT_ADDRESS
& SM2 : BIT_ADDRESS
& SM1 : BIT_ADDRESS
& SM0 : BIT_ADDRESS
& RI = ( SCON , 0 )
& TI = ( SCON , 1 )
& RB8 = ( SCON , 2 )
& TB8 = ( SCON , 3 )
& REN = ( SCON , 4 )
& SM2 = ( SCON , 5 )
& SM1 = ( SCON , 6 )
& SM0 = ( SCON , 7 )



& PSW : SFR_ADDR
& PSW = 208

& PP : BIT_ADDRESS
& OV : BIT_ADDRESS
& RS0 : BIT_ADDRESS
& RS1 : BIT_ADDRESS
& F0 : BIT_ADDRESS
& AC : BIT_ADDRESS
& CY : BIT_ADDRESS
& CC : BIT_ADDRESS

& BD0 : BIT_ADDRESS
& BD1 : BIT_ADDRESS
& BD2 : BIT_ADDRESS
& BD3 : BIT_ADDRESS
& BD4 : BIT_ADDRESS
& BD5 : BIT_ADDRESS
& BD6 : BIT_ADDRESS
& BD7 : BIT_ADDRESS

& BD0 = ( PSW , 0 )
& BD1 = ( PSW , 1 )
& BD2 = ( PSW , 2 )
& BD3 = ( PSW , 3 )
& BD4 = ( PSW , 4 )
& BD5 = ( PSW , 5 )
& BD6 = ( PSW , 6 )
& BD7 = ( PSW , 7 )

& PSW_0 : BIT_ADDRESS
& PSW_1 : BIT_ADDRESS
& PSW_2 : BIT_ADDRESS
& PSW_3 : BIT_ADDRESS
& PSW_4 : BIT_ADDRESS
& PSW_5 : BIT_ADDRESS
& PSW_6 : BIT_ADDRESS
& PSW_7 : BIT_ADDRESS

& PSW_0 = ( PSW , 0 )
& PSW_1 = ( PSW , 1 )
& PSW_2 = ( PSW , 2 )
& PSW_3 = ( PSW , 3 )
& PSW_4 = ( PSW , 4 )
& PSW_5 = ( PSW , 5 )
& PSW_6 = ( PSW , 6 )
& PSW_7 = ( PSW , 7 )




& PP = ( PSW , 0 )
& OV = ( PSW , 2 )
& RS0 = ( PSW , 3 )
& RS1 = ( PSW , 4 )
& F0 = ( PSW , 5 )
& AC = ( PSW , 6 )
& CY = ( PSW , 7 )
& CC = ( PSW , 7 )



& STACK_MAX : RAM_ADDR
& STACK_MAX = RAM_MAX

/*Funções Auxiliares*/
/*
Old Implementation
&INSTRUCTION_LEN : NATURAL
&INSTRUCTION_LEN = 8 
 
&INSTRUCTION_MAX = 2**INSTRUCTION_LEN 
&INSTRUCTION = 0..(INSTRUCTION_MAX -1)

&PC_INCREMENT : (INSTRUCTION * INSTRUCTION) --> INSTRUCTION 
&PC_INCREMENT= %(ii,jj).(ii : INSTRUCTION & jj : INSTRUCTION | (ii+jj) mod (INSTRUCTION_MAX))
*/

/*
Function to increment the pc 
*/
& PC_INCREMENT : ( ROM_ADDR * ROM_ADDR ) --> ROM_ADDR
& PC_INCREMENT = % ( ii , jj ) . ( ii : ROM_ADDR & jj : ROM_ADDR | ( ii + jj ) mod ( ROM_ADDR_MAX + 1 ) )


& ADDRCHANGE : MEM_ADDR * BYTE * BYTE --> ( MEM_ADDR +-> BYTE )

& ADDRCHANGE = % ( addr , data , psw ) . ( addr : MEM_ADDR & data : BYTE & psw : BYTE & addr = ACC
                                                        | { addr |-> data , PSW |-> bv_put ( psw , 0 , bv_par ( data ) ) } )

& ADDRCHANGE = % ( addr , data , psw ) . ( addr : MEM_ADDR & data : BYTE & psw : BYTE & addr /= ACC & addr /= PSW | { addr |-> data } )

& BITCHANGE : MEM_ADDR * BYTE * BYTE_INDEX * BIT * BYTE --> ( MEM_ADDR +-> BYTE )

& BITCHANGE = % ( addr , data , ind , bit , psw ) . ( addr : MEM_ADDR & data : BYTE & ind : BYTE_INDEX & bit : BIT & psw : BYTE & addr = ACC
                                                                        | ADDRCHANGE ( addr , bv_put ( data , ind , bit ) , psw ) )

& BITCHANGE = % ( addr , data , ind , bit , psw ) . ( addr : MEM_ADDR & data : BYTE & ind : BYTE_INDEX & bit : BIT & psw : BYTE & addr /= ACC
                                                                                | { addr |-> bv_put ( data , ind , bit ) } )


& SP_INCREMENT : BYTE * ROM_ADDR --> RAM_ADDR
& SP_INCREMENT = % ( sp , inc ) . ( sp : BYTE & inc : RAM_ADDR | ( byte_uchar ( sp ) + inc ) mod STACK_MAX )


& SP_DECREMENT : BYTE * ROM_ADDR --> RAM_ADDR
& SP_DECREMENT = % ( sp , dec ) . ( sp : BYTE & dec : RAM_ADDR & byte_uchar ( sp ) - dec > 0 | ( byte_uchar ( sp ) - dec ) )
& SP_DECREMENT = % ( sp , dec ) . ( sp : BYTE & dec : RAM_ADDR & not ( byte_uchar ( sp ) - dec > 0 ) | STACK_MAX - ( byte_uchar ( sp ) - dec ) )

/*Avoild this definition*/
& PSWUPDATE : BYTE * BIT * BIT * BIT * BIT --> BYTE
& PSWUPDATE = % ( psw , Pa , Ov , Ac , Cy ) . ( psw : BYTE & Pa : BIT & Ov : BIT & Ac : BIT & Cy : BIT |
                        bv_put ( bv_put ( bv_put ( bv_put ( psw , 0 , Pa ) , 2 , Ov ) , 6 , Ac ) , 7 , Cy ) )

& BIT_GET : BIT_ADDRESS * ( MEM_ADDR +-> BYTE ) --> BIT
& ! ( bit , memory , addr , ind ) . ( bit : BIT_ADDRESS & memory : ( MEM_ADDR +-> BYTE ) & addr : MEM_ADDR & ind : BYTE_INDEX & addr , ind = bit
                                                                                => BIT_GET ( bit , memory ) = memory ( addr ) ( ind ) )
/*
####################Antention#####################
Put this for test
PSW : SFR_BIT_ADDRESSABLE
*/

ASSERTIONS

        ACC /: RAM_ADDR
&        RAM_ADDR <: MEM_ADDR
&        SFR_ADDR <: MEM_ADDR
&        RAM_ADDR \/ SFR_ADDR = MEM_ADDR

&         RAM_BIT_ADDRESSABLE <: RAM_ADDR
&        SFR_BIT_ADDRESSABLE <: SFR_ADDR

&         BIT_ADDRESSABLE <: BIT_ADDRESS

&        ROM_ADDR <: USHORT_INT

&        ! ( xx , yy ) . ( xx : ROM_ADDR & yy : ROM_ADDR =>
              ( xx + yy > ROM_ADDR_MAX => PC_INCREMENT ( xx , yy ) = ( ( xx + yy ) - ( ROM_ADDR_MAX + 1 ) ) )
            & ( not ( xx + yy > ROM_ADDR_MAX ) => PC_INCREMENT ( xx , yy ) = ( xx + yy ) ) )
/*
&	!(xx,yy).(xx:ROM_ADDR & yy:ROM_ADDR => ((xx+yy = ROM_ADDR_MAX) => PC_INCREMENT(xx,yy)= ROM_ADDR_MAX
										 & (xx+yy = (ROM_ADDR_MAX + 1)) => PC_INCREMENT(xx,yy)= 0))

& 	!(xx,yy).(xx:BYTE & yy:RAM_ADDR =>  (byte_uchar(xx) + yy < STACK_MAX => SP_INCREMENT(xx,yy)= byte_uchar(xx)+yy
    								  & not(byte_uchar(xx) + yy < STACK_MAX) => SP_INCREMENT(xx,yy)= byte_uchar(xx) + yy - STACK_MAX))


& 	!(xx,yy).(xx:BYTE & yy:RAM_ADDR => (not(byte_uchar(xx) - yy > 0) => SP_DECREMENT(xx,yy)=STACK_MAX + (byte_uchar(xx) - yy)
    								& byte_uchar(xx) - yy >0 => SP_DECREMENT(xx,yy)= byte_uchar(xx)-yy))
*/

/* 
Law necessary to prover stack operations
*/
& ! ( xx ) . ( xx : BYTE => not ( SP_INCREMENT ( xx , 1 ) = SP_INCREMENT ( xx , 2 ) ) )

ABSTRACT_VARIABLES

/*
State variable of data memory (SFR and RAM)
*/
mem


INVARIANT

mem : MEM_ADDR --> BYTE
& mem ( PSW ) ( 0 ) = bv_par ( mem ( ACC ) )


INITIALISATION
/*
Make the initialisation
*/
mem := MEM_ADDR * { BYTE_ZERO }

OPERATIONS

rr <-- bitGet ( bit ) =
PRE
  bit : BIT_ADDRESSABLE
THEN
  ANY addr , indx WHERE addr : MEM_ADDR & indx : BYTE_INDEX & addr , indx = bit
  THEN rr := mem ( addr ) ( indx )
  END
END
;

setMem ( addr , data ) =
PRE
   addr : MEM_ADDR & data : BYTE
THEN
   mem ( addr ) := data
END
;

/*
Why it allowed change the parity bit without change the Accumalator?
*/
bitSet ( bit ) =
PRE
  bit : BIT_ADDRESSABLE
THEN
  ANY addr , ind
  WHERE addr : MEM_ADDR & ind : BYTE_INDEX & addr , ind = bit
  THEN mem := mem <+ BITCHANGE ( addr , mem ( addr ) , ind , 1 , mem ( PSW ) )
  END
END
;

/*
Why it allowed change the parity bit without change the Accumalator?
*/
bitClear ( bit ) =
PRE
  bit : BIT_ADDRESSABLE
THEN
  ANY addr , ind
  WHERE addr : MEM_ADDR & ind : BYTE_INDEX & addr , ind = bit
  THEN mem := mem <+ BITCHANGE ( addr , mem ( addr ) , ind , 0 , mem ( PSW ) )
  END
END
;

addrSetDirect ( addr , data ) =
PRE addr : MEM_ADDR & data : BYTE
THEN
  mem := ADDRCHANGE ( addr , data , mem ( PSW ) )
END
;

addrSetIndirect ( Rn , data ) =
PRE
  Rn : RAM_ADDR & ( Rn = R1 or Rn = R2 ) & data : BYTE & byte_uchar ( mem ( Rn ) ) : RAM_ADDR
THEN
  ANY addr WHERE addr : RAM_ADDR & addr = byte_uchar ( mem ( Rn ) )
  THEN mem := mem <+ ADDRCHANGE ( addr , data , mem ( PSW ) )
  END
END

;

push_two ( data1 , data2 ) =
 PRE data1 : BYTE & data2 : BYTE & mem ( SP ) : BYTE
 THEN
     mem := mem <+ { SP_INCREMENT ( mem ( SP ) , 1 ) |-> data1 , SP_INCREMENT ( mem ( SP ) , 2 ) |-> data2 , SP |-> uchar_byte ( SP_INCREMENT ( mem ( SP ) , 2 ) ) }
 END
;

push ( data ) =
 PRE data : BYTE
 THEN
        mem ( SP_INCREMENT ( mem ( SP ) , 1 ) ) := data
 END
;


pop ( addr ) =
  PRE addr : MEM_ADDR
  THEN
      mem := mem <+ ( ADDRCHANGE ( addr , mem ( SP ) , mem ( PSW ) ) \/ { SP |-> uchar_byte ( SP_DECREMENT ( mem ( SP ) , 1 ) ) } )
  END
;


/*
Instruction that update the bits Pa,Ov,Ac,Cy  and the value of ACC after a arithmetic operation, 
holding the parity propertie 
*/
update ( acc , Pa , Ov , Ac , Cy ) =
 PRE acc : UCHAR & Pa : BIT & Ov : BIT & Ac : BIT & Cy : BIT
 THEN mem := mem <+ { ACC |-> uchar_byte ( acc ) , PSW |-> PSWUPDATE ( mem ( PSW ) , Pa , Ov , Ac , Cy ) }
 END


END

/*

CONSTANTS 
 bitjump
,changeacc
,incsp
,decsp

PROPERTIES 





bitjump : BIT * BIT * WORD * INSTRUCTION --> INSTRUCTION
&!(bit,test,jump,pc).(bit : BIT & test : BIT & pc : INSTRUCTION & jump : INSTRUCTION
			=> (   (bit=test) =>  bitjump(bit,test,jump,pc) = INSTRUCTION_IJUMP(pc,jump)
			    &  (bit/=test) => bitjump(bit,test,jump,pc) = INSTRUCTION_NEXT(pc)
				
	   		   )
)




&incsp:RAM_ADDR --> RAM_ADDR
&!(sp,ret).(sp:RAM_ADDR & ret:NATURAL & ret=sp+1 => ( (ret>=RAM_MAX) => incsp(sp)=RAM_MAX & (ret<RAM_MAX) => incsp(sp)=ret))

&decsp:RAM_ADDR --> RAM_ADDR
&!(sp,ret).(sp:RAM_ADDR & ret:NATURAL & ret = sp-1 => ( (ret<=0) => decsp(sp)=0& (ret>0) => decsp(sp)=ret))
*/
