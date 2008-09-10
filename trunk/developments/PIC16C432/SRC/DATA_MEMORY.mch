/*************************************************************************
Copyright (c) 2008 David Deharbe

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
    copyright notice, this list of conditions and the following
    disclaimer in the documentation and/or other materials provided
    with the distribution.

    * Neither the name of the Universidade Federal do Rio Grande do
    Norte nor the names of its contributors may be used to endorse
    or promote products derived from this software without specific
    prior written permission.

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
	DATA_MEMORY

SEES
	BIT_VECTOR_DEFINITION,
	BIT_VECTOR_ARITHMETICS,
	BYTE_DEFINITION,

	POWER2

CONSTANTS
	BANK_WIDTH, BANK, BANK_SIZE,
	LOCATION_WIDTH, LOCATION,
	ADDRESS_WIDTH, ADDRESS,
	FILE,

	/* locations of special function registers */
	INDF, TMR0, PCL, 
	STATUS, FSR, 
	PORTA, PORTB,
	PCLATH, INTCON, PIR1, CMCON,
	OPTION, TRISA, TRISB,
	PIE1, PCON, LININTF, VRCON,

	/* unimplemented memory locations */
	UNIMPLEMENTED_LOCATIONS,

	MAPPED_REGISTERS,
	register_addresses,

	IRP, RP1, RP0, TO, PD, ZERO, DC, CARRY

	address

PROPERTIES
	BANK_WIDTH = 2 &
	BANK <: BIT_VECTOR &
	BANK = { vv | vv : BIT_VECTOR & bv_size(vv) = BANK_WIDTH } &
	LOCATION_WIDTH = 7 &
	LOCATION <: BIT_VECTOR &
	LOCATION = { vv | vv : BIT_VECTOR & bv_size(vv) = LOCATION_WIDTH } &
	BANK_SIZE = 2**LOCATION_WIDTH &
	ADDRESS_WIDTH = BANK_WIDTH + LOCATION_WIDTH &
	ADDRESS = 0..((2**ADDRESS_WIDTH)-1)

	FILE = ADDRESS --> BYTE &

	/* locations of special function registers */
	INDF : ADDRESS & 	INDF = 0 & /* 00h */
	TMR0 : ADDRESS & 	TMR0 = 1 & /* 01h */
	PCL : ADDRESS & 	PCL = 2 & /* 02h */
	STATUS : ADDRESS & STATUS = 3 & /* 03h */
	FSR : ADDRESS & 	FSR = 4 & /* 04h */
	PORTA : ADDRESS & 	PORTA = 5 & /* 05h */
	PORTB : ADDRESS & 	PORTB = 6 & /* 06h */
	PCLATH : ADDRESS & PCLATH = 10 & /* 0Ah */
	INTCON : ADDRESS & INTCON = 11 & /* 0Bh */
	PIR1 : ADDRESS & 	PIR1 = 12 & /* 0Ch */
	CMCON : ADDRESS & 	CMCON = 31 & /* 1Fh */
	OPTION : ADDRESS & 	OPTION = 129 & /*  */
	TRISA : ADDRESS & 	TRISA = 133 & /* 85h */
	TRISB : ADDRESS & 	TRISB = 134 & /* 86h */
	PIE1 : ADDRESS & 	PIE1 = 140 & /* 8Ch */
	PCON : ADDRESS & 	PCON = 142 & /* 8Eh */
	LININTF : ADDRESS &LININTF = 144 & /* 90h */
	VRCON : ADDRESS & 	VRCON = 159 & /* 9FH */

	/* memory locations */
	UNIMPLEMENTED_LOCATIONS = 
		7..9 \/	13..30 \/ 135..137 \/ {141} \/ {143} \/ 145..158 \/ 192..239 &

	MAPPED_REGISTERS <: ADDRESS &
	MAPPED_REGISTERS = { INDF, PCL, STATUS, FSR, PCLATH, INTCON } \/ 112..127 &

	register_addresses : ADDRESS --> POW1 (ADDRESS) &
	!(ad). (ad : ADDRESS =>
		((ad /: MAPPED_REGISTERS => register_addresses(ad) = {ad}) &
		 (ad : MAPPED_REGISTERS => register_addresses(ad) = {ad, ad + BANK_SIZE}))) &

	IRP : BYTE_INDEX & IRP = 7 &
	RP1 : BYTE_INDEX & RP1 = 6 &
	RP0 : BYTE_INDEX & RP0 = 5 &
	TO : BYTE_INDEX & TO = 4 &
	PD : BYTE_INDEX & PD = 3 &
	ZERO : BYTE_INDEX & ZERO = 2 &
	DC : BYTE_INDEX & DC = 1 &
	CARRY : BYTE_INDEX & CARRY = 0 &

	address : ADDRESS * FILE --> ADDRESS &
	address =
		/* direct addressing */
		% (ad, fi) . (ad : ADDRESS & fi : FILE & ad /: register_addresses (INDF) |
			fi(STATUS)(RP1)*2**8 + fi(STATUS)(RP0)*2**7 + bv_to_nat(fi(FSR))) \/
		/* indirect addressing */
		% (ad, fi) . (ad : ADDRESS & fi : FILE & ad : register_addresses (INDF) |
			fi(STATUS)(IRP)*2**8 + bv_to_nat (fi(FSR)))

VARIABLES
	file

INVARIANT
	file : FILE &
	!(ad).(ad : UNIMPLEMENTED_LOCATIONS => file(ad) = BYTE_ZERO) &
	!(reg).(reg : MAPPED_REGISTERS => file(reg) = file(reg+BANK_SIZE)) &
	file(STATUS)(IRP) = 0 &
	file(STATUS)(RP1) = 0

ASSERTIONS
	BANK_SIZE = 128;
	register_addresses (INDF) = {0, 128};
	register_addresses (STATUS) = {3, 131};
	register_addresses (FSR) = {4, 132};
	register_addresses (PCLATH) = {10, 138};
	register_addresses (INTCON) = {11, 139};

	dom(file) = ADDRESS;
	ran(file) <: BYTE

INITIALISATION
	ANY random, init_status WHERE
		random : FILE &
		init_status : BYTE &
		init_status (RP1) = 0
	THEN
		file := random <+ { INDF |-> BYTE_ZERO,
					STATUS |-> init_status,
					INDF + 128 |-> BYTE_ZERO,
					STATUS + 128 |-> init_status }
				<+ (UNIMPLEMENTED_LOCATIONS * { BYTE_ZERO })
	END
END
