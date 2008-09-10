/*************************************************************************
Copyright (c) 2007, 2008 David Deharbe

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
    PIC

SEES
    BIT_DEFINITION ,
    BYTE_DEFINITION ,
    TYPES

USES
    ALU

INCLUDES
    DATA_MEMORY

ABSTRACT_VARIABLES
    W_REGISTER ,
    pc ,
    stack ,
    sp

INVARIANT
    W_REGISTER : BYTE &
    pc : INSTRUCTION &
    sp : NATURAL &
    stack : NATURAL +-> INSTRUCTION &
    dom ( stack ) = 0 .. ( sp - 1 )

CONCRETE_CONSTANTS
    /* identify bits in the STATUS register */
    TO_POS , PD_POS ,
    ZERO_POS , DCARRY_POS , CARRY_POS ,

    /* control logic */
    get_address ,
    get_offset

PROPERTIES

    /* identify bits in the STATUS register */
    TO_POS : BYTE_INDEX &    TO_POS = 4 &
    PD_POS : BYTE_INDEX &    PD_POS = 3 &
    ZERO_POS : BYTE_INDEX &    ZERO_POS = 2 &
    DCARRY_POS : BYTE_INDEX &    DCARRY_POS = 1 &
    CARRY_POS : BYTE_INDEX &    CARRY_POS = 0

INITIALISATION
    W_REGISTER :: BYTE ||
    pc :: INSTRUCTION ||
    stack := {} ||
    sp := 0

OPERATIONS
    CALL ( kk ) =
        PRE kk : INSTRUCTION THEN
            stack ( sp ) := instruction_next ( pc ) ||
            sp := sp + 1 ||
            pc := kk
        END
    ;
    RETURN =
        PRE sp > 0 THEN
            stack := { sp - 1 } <<| stack ||
            pc := stack ( sp - 1 ) ||
            sp := sp - 1
        END
    ;
    RETLW ( kk ) =
        PRE kk : UCHAR & sp > 0 THEN
            pc := stack ( sp - 1 ) ||
            W_REGISTER := uchar_to_byte ( kk )
        END
    ;
    ADDLW ( kk ) =
        PRE kk : UCHAR THEN
            ANY result , carry , digit_carry , zero WHERE
                result : UCHAR & digit_carry : BOOL & carry : BOOL & zero : BOOL &
                result , digit_carry , carry , zero = add ( kk , byte_to_uchar ( W_REGISTER ) )
            THEN
                W_REGISTER := uchar_to_byte ( result ) ||
                mem := update_status ( zero , digit_carry , carry )
            END ||
            pc := instruction_next ( pc )
        END
    ;
    ADDWF ( ff , dd ) =
        PRE ff : REGISTER0 & dd : BIT THEN
            ANY dest , result , carry , digit_carry , zero WHERE
                dest : REGISTER &
                result : UCHAR & digit_carry : BOOL & carry : BOOL & zero : BOOL &
                dest = get_address ( ff , RP0_VAL , byte_to_uchar ( FSR_VAL ) ) &
                result , digit_carry , carry , zero = add ( byte_to_uchar ( mem ( dest ) ) , byte_to_uchar ( W_REGISTER ) )
            THEN
                ANY mem2 , bv WHERE
                    bv : BYTE & bv = uchar_to_byte ( result ) &
                    mem2 : REGISTER --> BYTE &
                    ( dd /= 0 & dest /: UNIMPLEMENTED_LOCATIONS => mem2 = mem <+ { dest |-> bv } ) &
                    ( dd = 0 or dest : UNIMPLEMENTED_LOCATIONS => mem2 = mem )
                THEN
                    IF dd = 0 THEN
                        W_REGISTER := bv
                    END ||
                    IF ff /= STATUS_ADDR0 THEN
                        mem := update_status ( zero , digit_carry , carry )
                    END
                END
            END ||
            pc := instruction_next ( pc )
        END
/*
	;
	SUBLW (kk) =
		PRE kk : UCHAR THEN
			ANY result, borrow, zero WHERE 
				result : UCHAR & borrow : BOOL & zero : BOOL &
				result, borrow, zero = substract(kk, W_REGISTER)
			THEN
				W_REGISTER, cc, zz := result, borrow, zero
			END ||
			pc := instruction_next(pc)
		END
	;
	SUBWF (ff, dd) =
		PRE ff : REGISTER & dd : BIT THEN
			ANY result, borrow, zero WHERE 
				result : UCHAR & borrow : BOOL & zero : BOOL &
				result, borrow, zero = substract(byte_to_uchar(mem(ff)), W_REGISTER)
			THEN
				IF dd = 0 THEN
					W_REGISTER := result 
				ELSE
					mem(ff) :=  uchar_to_byte(result)
				END ||
				cc := borrow ||
				zz := zero
			END ||
			pc := instruction_next(pc)
		END
	;
	ANDLW (kk) =
		PRE kk : UCHAR THEN
			ANY result, zero WHERE 
				result : UCHAR & zero : BOOL &
				result, zero = and(kk, W_REGISTER)
			THEN
				W_REGISTER, zz := result, zero
			END ||
			pc := instruction_next(pc)
		END
	;
	ANDLWF (ff, dd) =
		PRE ff : REGISTER & dd : BIT THEN
			ANY result, zero WHERE 
				result : UCHAR & zero : BOOL &
				result, zero = and(byte_to_uchar(mem(ff)), W_REGISTER)
			THEN
				IF dd = 0 THEN
					W_REGISTER := result
				ELSE
					mem(ff) := uchar_to_byte(result)
				END
			END ||
			pc := instruction_next(pc)
		END
	;
	IORLW (kk) =
		PRE kk : UCHAR THEN
			ANY result, zero WHERE 
				result : UCHAR & zero : BOOL &
				result, zero = ior(kk, W_REGISTER)
			THEN
				W_REGISTER, zz := result, zero
			END ||
			pc := instruction_next(pc)
		END
	;
	IORLWF (ff, dd) =
		PRE ff : REGISTER & dd : BIT THEN
			ANY result, zero WHERE 
				result : UCHAR & zero : BOOL &
				result, zero = ior(byte_to_uchar(mem(ff)), W_REGISTER)
			THEN
				IF dd = 0 THEN
					W_REGISTER := result
				ELSE
					mem(ff) := uchar_to_byte(result)
				END
			END ||
			pc := instruction_next(pc)
		END
	;
	XORLW (kk) =
		PRE kk : UCHAR THEN
			ANY result, zero WHERE 
				result : UCHAR & zero : BOOL &
				result, zero = xor(kk, W_REGISTER)
			THEN
				W_REGISTER, zz := result, zero
			END ||
			pc := instruction_next(pc)
		END
	;
	XORLWF (ff, dd) =
		PRE ff : REGISTER & dd : BIT THEN
			ANY result, zero WHERE 
				result : UCHAR & zero : BOOL &
				result, zero = xor(byte_to_uchar(mem(ff)), W_REGISTER)
			THEN
				IF dd = 0 THEN
					W_REGISTER := result
				ELSE
					mem(ff) := uchar_to_byte(result)
				END
			END ||
			pc := instruction_next(pc)
		END
	;
	BCF (ff, bb) =
		PRE ff : REGISTER & bb : BYTE_INDEX THEN
			mem(ff) := uchar_to_byte(bitclear(byte_to_uchar(mem(ff)), bb)) ||
			pc := instruction_next(pc)
		END
	;
	BSF (ff, bb) =
		PRE ff : REGISTER & bb : BYTE_INDEX THEN
			mem(ff) := bitset(mem(ff), bb) ||
			pc := instruction_next(pc)
		END
	;
	BTFSC (ff, bb) =
		PRE ff : REGISTER & bb : BYTE_INDEX THEN
			IF bitget(mem(ff), bb) = 0 THEN
			  pc := instruction_next(instruction_next(pc))
			ELSE
			  pc := instruction_next(pc)
			END
		END
	;
	BTFSS (ff, bb) =
		PRE ff : REGISTER & bb : BYTE_INDEX THEN
			IF bitget(mem(ff), bb) = 1 THEN
			  pc := instruction_next(instruction_next(pc))
			ELSE
			  pc := instruction_next(pc)
			END
		END
	;
	CLRF (ff) =
		PRE ff : REGISTER THEN
			mem(ff) := 0 ||
			zz := TRUE ||
			pc := instruction_next(pc)
		END
	;
	CLRW =
		BEGIN
			W_REGISTER := 0 ||
			zz := TRUE ||
			pc := instruction_next(pc)
		END
	;
	COMF (ff, dd) =
		PRE ff : REGISTER & dd : BIT THEN
			ANY result WHERE result = complement(mem(ff))
			THEN
				IF dd = 0 THEN
					W_REGISTER := result
				ELSE
					mem(ff) := result
				END ||
				zz := bool(result = 0) 
			END ||
			pc := instruction_next(pc)
		END
	;
	DECF (ff, dd) =
		PRE ff : REGISTER & dd : BIT THEN
			ANY result, borrow, zero WHERE 
				result : UCHAR & borrow : BOOL & zero : BOOL &
				result, borrow, zero = substract(mem(ff), 1)
			THEN
				IF dd = 0 THEN
					W_REGISTER := result 
				ELSE
					mem(ff) :=  result
				END ||
				zz := zero
			END ||
			pc := instruction_next(pc)
		END
	;
	DECFSZ (ff, dd) =
		PRE ff : REGISTER & dd : BIT THEN
			ANY result, borrow, zero WHERE 
				result : UCHAR & borrow : BOOL & zero : BOOL &
				result, borrow, zero = substract(mem(ff), 1)
			THEN
				IF dd = 0 THEN
					W_REGISTER := result 
				ELSE
					mem(ff) :=  result
				END ||
				zz := zero ||
				IF result = 0 THEN
				  pc := instruction_next(instruction_next(pc))
				ELSE
				  pc := instruction_next(pc)
				END
			END
		END
	;
	GOTO (kk) =
		PRE kk : INSTRUCTION THEN
			pc := kk
		END
	;
	INCF (ff, dd) =
		PRE ff : REGISTER & dd : BIT THEN
			ANY result, carry, zero WHERE 
				result : UCHAR & carry : BOOL & zero : BOOL &
				result, carry, zero = add(mem(ff), 1)
			THEN
				IF dd = 0 THEN
					W_REGISTER := result
				ELSE
					mem(ff) := result
				END ||
				zz := bool(result = 0)
			END ||
			pc := instruction_next(pc)
		END
	;
  INCFSZ (ff, dd) =
		PRE ff : REGISTER & dd : BIT THEN
			ANY result, carry, zero WHERE
				result : UCHAR & carry : BOOL & zero : BOOL &
				result, carry, zero = add(mem(ff), 1)
			THEN
				IF dd = 0 THEN
					W_REGISTER := result
				ELSE
					mem(ff) := result
				END ||
				IF result = 0 THEN
					pc := instruction_next(instruction_next(pc))
				ELSE
					pc := instruction_next(pc)
				END
			END
		END
	;
	MOVLW (kk) =
		PRE kk : UCHAR THEN
			W_REGISTER := kk ||
			pc := instruction_next(pc)
		END
	;
	MOVF (ff, dd) =
		PRE ff : REGISTER & dd : BIT THEN
			IF dd = 0 THEN
				W_REGISTER := mem(ff)
			ELSE
				skip
			END ||
			zz := bool(mem(ff) = 0) ||
			pc := instruction_next(pc)
		END
	;
	MOVWF (ff) =
		PRE ff : REGISTER THEN
			mem (ff) := W_REGISTER ||
			pc := instruction_next(pc)
		END
	;
	NOP = pc := instruction_next(pc)
	;
	ROLF (ff, dd) =
		PRE ff : REGISTER & dd : BIT THEN
			ANY result, carry WHERE 
				result : UCHAR & carry : BOOL & 
				result, carry = rotateleft(mem(ff))
			THEN
				IF dd = 0 THEN
					W_REGISTER := result
				ELSE
					mem(ff) := result
				END ||
				cc := carry
			END ||
			pc := instruction_next(pc)
		END
	;
	RORF (ff, dd) =
		PRE ff : REGISTER & dd : BIT THEN
			ANY result, carry WHERE 
				result : UCHAR & carry : BOOL & 
				result, carry = rotateright(mem(ff))
			THEN
				IF dd = 0 THEN
			 		W_REGISTER := result
				ELSE
					mem(ff) := result
				END ||
				cc := carry
			END ||
			pc := instruction_next(pc)
		END
	;
	SWAP (ff, dd) =
		PRE ff : REGISTER & dd : BIT THEN
			ANY result WHERE result = swap(mem(ff))
			THEN
				IF dd = 0 THEN
			 		W_REGISTER := result
				ELSE
					mem(ff) := result
				END
			END ||
			pc := instruction_next(pc)
		END

*/
END
