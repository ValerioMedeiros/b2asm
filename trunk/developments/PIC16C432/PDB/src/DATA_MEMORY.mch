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
    BIT_VECTOR_DEFINITION ,
    BIT_VECTOR_ARITHMETICS ,
    BYTE_DEFINITION ,

    POWER2

CONCRETE_CONSTANTS
    BANK_WIDTH , BANK , BANK_SIZE ,
    LOCATION_WIDTH , LOCATION ,
    ADDRESS_WIDTH , ADDRESS ,
    FILE ,
    make_address ,

    /* locations of special function registers */
    INDF_ADDR , TMR0_ADDR , PCL_ADDR ,
    STATUS_ADDR , FSR_ADDR ,
    PORTA_ADDR , PORTB_ADDR ,
    PCLATH_ADDR , INTCON_ADDR , PIR1_ADDR , CMCON_ADDR ,
    TRISA_ADDR , TRISB_ADDR ,
    PIE1_ADDR , PCON_ADDR , LININTF_ADDR , VRCON_ADDR ,

    /* unimplemented memory locations */
    UNIMPLEMENTED_LOCATIONS ,

    MAPPED_REGISTERS ,
    register_addresses ,

    IRP_POS , RP1_POS , RP0_POS ,

    actual_address

PROPERTIES
    BANK_WIDTH = 2 &
    BANK <: BIT_VECTOR &
    BANK = { vv | vv : BIT_VECTOR & bv_size ( vv ) = BANK_WIDTH } &
    LOCATION_WIDTH = 7 &
    LOCATION <: BIT_VECTOR &
    LOCATION = { vv | vv : BIT_VECTOR & bv_size ( vv ) = LOCATION_WIDTH } &
    BANK_SIZE = 2 ** LOCATION_WIDTH &
    ADDRESS_WIDTH = BANK_WIDTH + LOCATION_WIDTH &
    ADDRESS = 0 .. ( ( 2 ** ADDRESS_WIDTH ) - 1 ) &
    make_address : BANK * LOCATION --> ADDRESS &
    make_address = % ( vb , vl ) . ( vb : BANK & vl : LOCATION | bv_to_nat ( bv_catenate ( vb , vl ) ) ) &

    FILE = ADDRESS --> BYTE &

    /* locations of special function registers */
    INDF_ADDR : ADDRESS &     INDF_ADDR = 0 & /* 00h */
    TMR0_ADDR : ADDRESS &     TMR0_ADDR = 1 & /* 01h */
    PCL_ADDR : ADDRESS &     PCL_ADDR = 2 & /* 02h */
    STATUS_ADDR : ADDRESS & STATUS_ADDR = 3 & /* 03h */
    FSR_ADDR : ADDRESS &     FSR_ADDR = 4 & /* 04h */
    PORTA_ADDR : ADDRESS &     PORTA_ADDR = 5 & /* 05h */
    PORTB_ADDR : ADDRESS &     PORTB_ADDR = 6 & /* 06h */
    PCLATH_ADDR : ADDRESS & PCLATH_ADDR = 10 & /* 0Ah */
    INTCON_ADDR : ADDRESS & INTCON_ADDR = 11 & /* 0Bh */
    PIR1_ADDR : ADDRESS &     PIR1_ADDR = 12 & /* 0Ch */
    CMCON_ADDR : ADDRESS &     CMCON_ADDR = 127 & /* 1Fh */
    TRISA_ADDR : ADDRESS &     TRISA_ADDR = 133 & /* 85h */
    TRISB_ADDR : ADDRESS &     TRISB_ADDR = 134 & /* 86h */
    PIE1_ADDR : ADDRESS &     PIE1_ADDR = 140 & /* 8Ch */
    PCON_ADDR : ADDRESS &     PCON_ADDR = 142 & /* 8Eh */
    LININTF_ADDR : ADDRESS & LININTF_ADDR = 144 & /* 90h */
    VRCON_ADDR : ADDRESS &     VRCON_ADDR = 159 & /* 9FH */

    /* memory locations */
    UNIMPLEMENTED_LOCATIONS =
        7 .. 9 \/  13 .. 30 \/ 135 .. 137 \/ { 141 } \/ { 143 } \/ 145 .. 158 \/ 192 .. 240 &

    MAPPED_REGISTERS <: ADDRESS &
    MAPPED_REGISTERS = { INDF_ADDR , STATUS_ADDR , FSR_ADDR , PCLATH_ADDR , INTCON_ADDR } &

    register_addresses : ADDRESS --> POW1 ( ADDRESS ) &
    ! ( ad ) . ( ad : ADDRESS =>
        ( ( ad /: MAPPED_REGISTERS => register_addresses ( ad ) = { ad } ) &
         ( ad : MAPPED_REGISTERS => register_addresses ( ad ) = { ad , ad + BANK_SIZE } ) ) ) &

    IRP_POS : BYTE_INDEX & IRP_POS = 7 &
    RP1_POS : BYTE_INDEX & RP1_POS = 6 &
    RP0_POS : BYTE_INDEX & RP0_POS = 5 &

    actual_address : ADDRESS * FILE --> ADDRESS &
    actual_address =
        % ( ad , fi ) . ( ad : ADDRESS & fi : FILE & ad : register_addresses ( INDF_ADDR ) |
            ( fi ( STATUS_ADDR ) ( RP0_POS ) * 2 ** 7 ) + ( fi ( STATUS_ADDR ) ( RP1_POS ) * 2 ** 8 ) + bv_to_nat ( fi ( FSR_ADDR ) ) ) \/
        % ( ad , fi ) . ( ad : ADDRESS & fi : FILE & ad /: register_addresses ( INDF_ADDR ) |
            ( fi ( STATUS_ADDR ) ( IRP_POS ) * 2 ** 8 ) + bv_to_nat ( fi ( FSR_ADDR ) ) )

ABSTRACT_VARIABLES
    file

INVARIANT
    file : FILE &
    ! ( ad ) . ( ad : UNIMPLEMENTED_LOCATIONS => file ( ad ) = BYTE_ZERO ) &
    ! ( reg ) . ( reg : MAPPED_REGISTERS => file ( reg ) = file ( reg + BANK_SIZE ) ) &
    file ( STATUS_ADDR ) ( IRP_POS ) = 0 &
    file ( STATUS_ADDR ) ( RP1_POS ) = 0

ASSERTIONS
    BANK_SIZE = 128 ;
    register_addresses ( INDF_ADDR ) = { 0 , 128 } ;
    register_addresses ( STATUS_ADDR ) = { 3 , 131 } ;
    register_addresses ( FSR_ADDR ) = { 4 , 132 } ;
    register_addresses ( PCLATH_ADDR ) = { 10 , 138 } ;
    register_addresses ( INTCON_ADDR ) = { 11 , 139 } ;

    dom ( file ) = ADDRESS ;
    ran ( file ) <: BYTE

INITIALISATION
    ANY random , init_status WHERE
        random : FILE &
        init_status : BYTE &
        init_status ( RP1_POS ) = 0
    THEN
        file := random <+ { INDF_ADDR |-> BYTE_ZERO ,
                    STATUS_ADDR |-> init_status ,
                    INDF_ADDR + 128 |-> BYTE_ZERO ,
                    STATUS_ADDR + 128 |-> init_status }
                <+ ( UNIMPLEMENTED_LOCATIONS * { BYTE_ZERO } )
    END
END
