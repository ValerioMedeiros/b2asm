﻿Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(MEMORY))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(MEMORY))==(Machine(MEMORY));
  Level(Machine(MEMORY))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(MEMORY)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(MEMORY))==(ALU,BIT_DEFINITION,BYTE_DEFINITION,BV16_DEFINITION,UCHAR_DEFINITION,SCHAR_DEFINITION,SSHORT_DEFINITION,USHORT_DEFINITION,POWER2)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(MEMORY))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(MEMORY))==(?);
  List_Includes(Machine(MEMORY))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(MEMORY))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(MEMORY))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(MEMORY))==(?);
  Context_List_Variables(Machine(MEMORY))==(?);
  Abstract_List_Variables(Machine(MEMORY))==(?);
  Local_List_Variables(Machine(MEMORY))==(mem);
  List_Variables(Machine(MEMORY))==(mem);
  External_List_Variables(Machine(MEMORY))==(mem)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(MEMORY))==(?);
  Abstract_List_VisibleVariables(Machine(MEMORY))==(?);
  External_List_VisibleVariables(Machine(MEMORY))==(?);
  Expanded_List_VisibleVariables(Machine(MEMORY))==(?);
  List_VisibleVariables(Machine(MEMORY))==(?);
  Internal_List_VisibleVariables(Machine(MEMORY))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(MEMORY))==(btrue);
  Gluing_List_Invariant(Machine(MEMORY))==(btrue);
  Expanded_List_Invariant(Machine(MEMORY))==(btrue);
  Abstract_List_Invariant(Machine(MEMORY))==(btrue);
  Context_List_Invariant(Machine(MEMORY))==(btrue);
  List_Invariant(Machine(MEMORY))==(mem: BV16 --> BYTE)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(MEMORY))==(btrue);
  Abstract_List_Assertions(Machine(MEMORY))==(btrue);
  Context_List_Assertions(Machine(MEMORY))==(is_zero16USHORT: USHORT --> BIT & is_negative: BYTE --> BIT & half8UCHAR: UCHAR --> UCHAR & inc: BYTE --> BYTE & dec: BYTE --> BYTE & simple_add8UCHAR: UCHAR*UCHAR --> UCHAR & add16USHORT: BIT*USHORT*USHORT --> USHORT & substract8UCHAR: BIT*UCHAR*UCHAR --> UCHAR*BIT*BIT*BIT*BIT & add_carryUSHORT: BIT*USHORT*USHORT --> BIT & sub16USHORT: BIT*USHORT*USHORT --> USHORT & sub_carryUSHORT: BIT*USHORT*USHORT --> BIT & inc_BV16: BV16 --> BV16 & dec_BV16: BV16 --> BV16 & update_refresh_reg: BYTE --> BYTE & instruction_next: USHORT --> USHORT & instruction_jump: USHORT*(-126..129) --> USHORT & dom(is_zero16USHORT) = USHORT & ran(is_zero16USHORT) = BIT & dom(is_negative) = BYTE & ran(is_negative) = BIT & dom(half8UCHAR) = UCHAR & ran(half8UCHAR) <: UCHAR & dom(inc) = BYTE & ran(inc) = BYTE & dom(dec) = BYTE & ran(dec) = BYTE & dom(simple_add8UCHAR) = UCHAR*UCHAR & ran(simple_add8UCHAR) = UCHAR & dom(add8UCHAR) = BIT*UCHAR*UCHAR & ran(add8UCHAR): POW(UCHAR*BIT*BIT*BIT*BIT) & dom(substract8UCHAR) = BIT*UCHAR*UCHAR & ran(substract8UCHAR): POW(UCHAR*BIT*BIT*BIT*BIT) & dom(add16USHORT) = BIT*USHORT*USHORT & ran(add16USHORT) = USHORT & dom(add_carryUSHORT) = BIT*USHORT*USHORT & ran(add_carryUSHORT) = BIT & dom(sub16USHORT) = BIT*USHORT*USHORT & ran(sub16USHORT) = USHORT & dom(sub_carryUSHORT) = BIT*USHORT*USHORT & ran(sub_carryUSHORT) = BIT & dom(inc_BV16) = BV16 & ran(inc_BV16) = BV16 & dom(dec_BV16) = BV16 & ran(dec_BV16) = BV16 & dom(update_refresh_reg) = BYTE & ran(update_refresh_reg) = BYTE & dom(instruction_next) = USHORT & ran(instruction_next) = USHORT & dom(instruction_jump) = USHORT*(-126..129) & ran(instruction_jump) = USHORT & 0 = schar_sshort(0,0) & 1 = 2**0 & 2 = 2**1 & 4 = 2**2 & 8 = 2**3 & 16 = 2**4 & 32 = 2**5 & 64 = 2**6 & 128 = 2**7 & 256 = 2**8 & 512 = 2**9 & 1024 = 2**10 & 2048 = 2**11 & 4096 = 2**12 & 8192 = 2**13 & 16384 = 2**14 & 32768 = 2**15 & 65536 = 2**16 & !bb.(bb: BIT => bit_not(bb) = 1-bb) & dom(bit_and) = BIT*BIT & ran(bit_not) = BIT & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & dom(bit_and) = BIT*BIT & ran(bit_and) = BIT & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & dom(bit_or) = BIT*BIT & ran(bit_or) = BIT & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & dom(bit_xor) = BIT*BIT & ran(bit_xor) = BIT & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & dom(bool_bit) = BOOL & ran(bit_xor) = BIT & bool_bit(TRUE) = 1 & bool_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (card(BYTE) = 256 & is_zero: BYTE --> BIT;parity_even: BYTE --> BIT;bv8_and: BYTE*BYTE --> BYTE;bv8_or: BYTE*BYTE --> BYTE;bv8_xor: BYTE*BYTE --> BYTE;complement: BYTE --> BYTE;swap: BYTE --> BYTE;rotateleft: BYTE --> BYTE;rotateright: BYTE --> BYTE;!bt.(bt: BYTE => size(bt) = 8);8: NATURAL;dom(is_zero) = BYTE;ran(is_zero) <: BIT;dom(parity_even) = BYTE;ran(parity_even) <: BIT;bv8_and: BYTE*BYTE --> BYTE;dom(complement) = BYTE;ran(complement) <: BYTE;dom(swap) = BYTE;ran(swap) <: BYTE;dom(rotateleft) = BYTE;ran(rotateleft) <: BYTE;dom(rotateright) = BYTE;ran(rotateright) <: BYTE;dom(get_upper_digit) = BYTE;ran(get_upper_digit) <: 0..16-1;dom(get_lower_digit) = BYTE;ran(get_lower_digit) <: 0..16-1;[1,1,1,1,1,1,1,1]: BYTE & [0,0,0,0,0,0,0,0]: BYTE & 1 = 2**0 & 2 = 2**1 & 4 = 2**2 & 8 = 2**3 & 16 = 2**4 & 32 = 2**5 & 64 = 2**6 & 128 = 2**7 & 256 = 2**8 & 512 = 2**9 & 1024 = 2**10 & 2048 = 2**11 & 4096 = 2**12 & 8192 = 2**13 & 16384 = 2**14 & 32768 = 2**15 & 65536 = 2**16) & card(BV16) = 65536 & complement16: BV16 --> BV16 & byte_bv16 = bv16_byte~ & [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]: BV16 & [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]: BV16 & card(byte_uchar) = 256 & card(uchar_byte) = 256 & UCHAR_MAX: INTEGER & UCHAR_MIN: INTEGER & 1 = 2**0 & 2 = 2**1 & 4 = 2**2 & 8 = 2**3 & 16 = 2**4 & 32 = 2**5 & 64 = 2**6 & 128 = 2**7 & 256 = 2**8 & card(byte_schar) = 256 & card(schar_byte) = 256 & SCHAR_MAX: INTEGER & SCHAR_MIN: INTEGER & 1 = 2**0 & 2 = 2**1 & 4 = 2**2 & 8 = 2**3 & 16 = 2**4 & 32 = 2**5 & 64 = 2**6 & 128 = 2**7 & 256 = 2**8 & card(bv16_sshort) = 65536 & card(sshort_bv16) = 65536 & SSHORT_MIN: SSHORT & SSHORT_MAX: SSHORT & 1 = 2**0 & 2 = 2**1 & 4 = 2**2 & 8 = 2**3 & 16 = 2**4 & 32 = 2**5 & 64 = 2**6 & 128 = 2**7 & 256 = 2**8 & 512 = 2**9 & 1024 = 2**10 & 2048 = 2**11 & 4096 = 2**12 & 8192 = 2**13 & 16384 = 2**14 & 32768 = 2**15 & 65536 = 2**16 & card(bv16_ushort) = 65536 & card(ushort_bv16) = 65536 & USHORT_MAX: INTEGER & USHORT_MIN: INTEGER & 1 = 2**0 & 2 = 2**1 & 4 = 2**2 & 8 = 2**3 & 16 = 2**4 & 32 = 2**5 & 64 = 2**6 & 128 = 2**7 & 256 = 2**8 & 512 = 2**9 & 1024 = 2**10 & 2048 = 2**11 & 4096 = 2**12 & 8192 = 2**13 & 16384 = 2**14 & 32768 = 2**15 & 65536 = 2**16 & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;(-2)**7 = -128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536));
  List_Assertions(Machine(MEMORY))==(ran(mem) <: BYTE & dom(mem) = BV16 & !(address,value).(address: BV16 & value: BYTE => mem<+{address|->value}: BV16 --> BYTE))
END
&
THEORY ListCoverageX IS
  List_Coverage(Machine(MEMORY))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Machine(MEMORY))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(MEMORY))==(mem:=%xx.(xx: BV16 | [0,0,0,0,0,0,0,0]));
  Context_List_Initialisation(Machine(MEMORY))==(skip);
  List_Initialisation(Machine(MEMORY))==(mem:=%xx.(xx: BV16 | [0,0,0,0,0,0,0,0]))
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(MEMORY))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(MEMORY),Machine(ALU))==(?);
  List_Instanciated_Parameters(Machine(MEMORY),Machine(BIT_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(MEMORY),Machine(BYTE_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(MEMORY),Machine(BV16_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(MEMORY),Machine(UCHAR_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(MEMORY),Machine(SCHAR_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(MEMORY),Machine(SSHORT_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(MEMORY),Machine(USHORT_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(MEMORY),Machine(POWER2))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(MEMORY))==(btrue);
  List_Constraints(Machine(MEMORY))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(MEMORY))==(updateMem,updateAddressMem,updateStack,updateAddressStack);
  List_Operations(Machine(MEMORY))==(updateMem,updateAddressMem,updateStack,updateAddressStack)
END
&
THEORY ListInputX IS
  List_Input(Machine(MEMORY),updateMem)==(NewValues);
  List_Input(Machine(MEMORY),updateAddressMem)==(address,value);
  List_Input(Machine(MEMORY),updateStack)==(NewValues);
  List_Input(Machine(MEMORY),updateAddressStack)==(address,value)
END
&
THEORY ListOutputX IS
  List_Output(Machine(MEMORY),updateMem)==(?);
  List_Output(Machine(MEMORY),updateAddressMem)==(?);
  List_Output(Machine(MEMORY),updateStack)==(?);
  List_Output(Machine(MEMORY),updateAddressStack)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(MEMORY),updateMem)==(updateMem(NewValues));
  List_Header(Machine(MEMORY),updateAddressMem)==(updateAddressMem(address,value));
  List_Header(Machine(MEMORY),updateStack)==(updateStack(NewValues));
  List_Header(Machine(MEMORY),updateAddressStack)==(updateAddressStack(address,value))
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(MEMORY),updateMem)==(NewValues: BV16 +-> BYTE);
  List_Precondition(Machine(MEMORY),updateAddressMem)==(address: BV16 & value: BYTE);
  List_Precondition(Machine(MEMORY),updateStack)==(NewValues: BV16 +-> BYTE);
  List_Precondition(Machine(MEMORY),updateAddressStack)==(address: BV16 & value: BYTE)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(MEMORY),updateAddressStack)==(address: BV16 & value: BYTE | mem:=mem<+{address|->value});
  Expanded_List_Substitution(Machine(MEMORY),updateStack)==(NewValues: BV16 +-> BYTE | mem:=mem<+NewValues);
  Expanded_List_Substitution(Machine(MEMORY),updateAddressMem)==(address: BV16 & value: BYTE | mem:=mem<+{address|->value});
  Expanded_List_Substitution(Machine(MEMORY),updateMem)==(NewValues: BV16 +-> BYTE | mem:=mem<+NewValues);
  List_Substitution(Machine(MEMORY),updateMem)==(mem:=mem<+NewValues);
  List_Substitution(Machine(MEMORY),updateAddressMem)==(mem:=mem<+{address|->value});
  List_Substitution(Machine(MEMORY),updateStack)==(mem:=mem<+NewValues);
  List_Substitution(Machine(MEMORY),updateAddressStack)==(mem:=mem<+{address|->value})
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(MEMORY))==(PROGRAM_R_ADR,DATA_R_ADR,STACK_R_ADR);
  Inherited_List_Constants(Machine(MEMORY))==(?);
  List_Constants(Machine(MEMORY))==(PROGRAM_R_ADR,DATA_R_ADR,STACK_R_ADR)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(MEMORY))==(?);
  Context_List_Defered(Machine(MEMORY))==(?);
  Context_List_Sets(Machine(MEMORY))==(?);
  List_Valuable_Sets(Machine(MEMORY))==(?);
  Inherited_List_Enumerated(Machine(MEMORY))==(?);
  Inherited_List_Defered(Machine(MEMORY))==(?);
  Inherited_List_Sets(Machine(MEMORY))==(?);
  List_Enumerated(Machine(MEMORY))==(?);
  List_Defered(Machine(MEMORY))==(?);
  List_Sets(Machine(MEMORY))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(MEMORY))==(?);
  Expanded_List_HiddenConstants(Machine(MEMORY))==(?);
  List_HiddenConstants(Machine(MEMORY))==(?);
  External_List_HiddenConstants(Machine(MEMORY))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(MEMORY))==(btrue);
  Context_List_Properties(Machine(MEMORY))==(is_zero16USHORT = %nat1.(nat1: USHORT | bool_bit(bool(nat1 = 0))) & is_negative = %w1.(w1: BYTE | w1(7)) & half8UCHAR = %ww.(ww: UCHAR | ww mod 16) & inc = %w1.(w1: BYTE | uchar_byte((byte_uchar(w1)+1) mod 256)) & dec = %w1.(w1: BYTE | uchar_byte((byte_uchar(w1)+255) mod 256)) & simple_add8UCHAR = %(w1,w2).(w1: UCHAR & w2: UCHAR | (w1+w2) mod 256) & add8UCHAR = %(carry,w1,w2).(carry: BIT & w1: UCHAR & w2: UCHAR | (carry+w1+w2) mod 256,bool_bit(bool(carry+uchar_schar(w1)+uchar_schar(w2)<0)),bool_bit(bool(carry+w1+w2>UCHAR_MAX)),bool_bit(bool(carry+half8UCHAR(w1)+half8UCHAR(w2)>=16)),bool_bit(bool((carry+w1+w2) mod 256 = 0))) & substract8UCHAR = %(carry,w1,w2).(carry: BIT & w1: UCHAR & w2: UCHAR | (carry+w1+(256-w2)) mod 256,bool_bit(bool(carry+uchar_schar(w1)-uchar_schar(w2)<0)),bool_bit(bool(carry+w1-w2>UCHAR_MAX)),bool_bit(bool(carry+half8UCHAR(w1)-half8UCHAR(w2)>=16)),bool_bit(bool((carry+w1+(256-w2)) mod 256 = 0))) & add16USHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | (b1+w1+w2) mod 65536) & add_carryUSHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | bool_bit(bool(b1+w1+w2>=65536))) & add_halfcarryUSHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | bool_bit(bool(b1+w1 mod 4096+w2 mod 4096>4096))) & sub16USHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | (b1+w1+(65536-w2)) mod 65536) & sub_carryUSHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | bool_bit(bool(w1+w2-b1>65536))) & sub_halfcarryUSHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | bool_bit(bool(w1 mod 4096+w2 mod 4096-b1>4096))) & inc_BV16 = %w1.(w1: BV16 | ushort_bv16((bv16_ushort(w1)+1) mod 65536)) & dec_BV16 = %w1.(w1: BV16 | ushort_bv16((bv16_ushort(w1)+65536-1) mod 65536)) & update_refresh_reg = %v0.(v0: BYTE | uchar_byte(128*v0(8)+(64*v0(7)+32*v0(6)+16*v0(5)+8*v0(4)+4*v0(3)+2*v0(2)+v0(1)) mod 128)) & instruction_next = %w1.(w1: USHORT | (w1+1) mod 65536) & instruction_jump = %(p0,e0).(p0: USHORT & e0: -126..129 | (p0+e0+65536) mod 65536) & BIT = {0,1} & bit_not: BIT >->> BIT & bit_not = {0|->1}\/{1|->0} & bit_and: BIT*BIT --> BIT & bit_and = {(0,0)|->0}\/{(0,1)|->0}\/{(1,0)|->0}\/{(1,1)|->1} & bit_or: BIT*BIT --> BIT & bit_or = {(0,0)|->0}\/{(0,1)|->1}\/{(1,0)|->1}\/{(1,1)|->1} & bit_xor: BIT*BIT --> BIT & bit_xor = {(0,0)|->0}\/{(0,1)|->1}\/{(1,0)|->1}\/{(1,1)|->0} & bool_bit = {TRUE|->1,FALSE|->0} & BYTE_INDEX = 1..8 & PHYS_BYTE_INDEX = 0..8-1 & BYTE = BYTE_INDEX --> BIT & !b1.(b1: BYTE => size(b1) = 8 & b1: seq1(BIT)) & is_zero: BYTE --> BIT & is_zero = %w1.(w1: BYTE | bool_bit(bool(w1(1) = w1(2) & w1(2) = w1(3) & w1(3) = w1(4) & w1(4) = w1(5) & w1(5) = w1(6) & w1(6) = w1(7) & w1(7) = w1(8) & w1(8) = 0))) & parity_even: BYTE --> BIT & parity_even = %bv.(bv: BYTE | 1-(bv(1)+bv(2)+bv(3)+bv(4)+bv(5)+bv(6)+bv(7)+bv(8)) mod 2) & bv8_and = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | %idx.(idx: 1..8 | bit_and(bt1(idx),bt2(idx)))) & bv8_or = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | %idx.(idx: 1..8 | bit_or(bt1(idx),bt2(idx)))) & bv8_xor = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | %idx.(idx: 1..8 | bit_xor(bt1(idx),bt2(idx)))) & bv8get: BYTE*PHYS_BYTE_INDEX --> BIT & bv8get = %(bt1,ii).(bt1: BYTE & ii: PHYS_BYTE_INDEX | bt1(ii+1)) & bitset: BYTE*PHYS_BYTE_INDEX --> BYTE & bitset = %(bt1,ii).(bt1: BYTE & ii: PHYS_BYTE_INDEX | bt1<+{ii+1|->1}) & bitclear: BYTE*PHYS_BYTE_INDEX --> BYTE & bitclear = %(bt1,ii).(bt1: BYTE & ii: PHYS_BYTE_INDEX | bt1<+{ii+1|->0}) & complement: BYTE --> BYTE & complement = %bt.(bt: BYTE | %idx.(idx: 1..8 | bit_not(bt(idx)))) & swap: BYTE --> BYTE & swap = %bt.(bt: BYTE | {1|->bt(5),2|->bt(6),3|->bt(7),4|->bt(8),5|->bt(1),6|->bt(2),7|->bt(3),8|->bt(4)}) & rotateleft: BYTE --> BYTE & rotateleft = %bv.(bv: BYTE | {1|->bv(8),2|->bv(1),3|->bv(2),4|->bv(3),5|->bv(4),6|->bv(5),7|->bv(6),8|->bv(7)}) & rotateright: BYTE --> BYTE & rotateright = %bv.(bv: BYTE | {1|->bv(2),2|->bv(3),3|->bv(4),4|->bv(5),5|->bv(6),6|->bv(7),7|->bv(8),8|->bv(1)}) & get_upper_digit: BYTE --> 0..15 & get_upper_digit = %by.(by: BYTE | 8*by(8)+4*by(7)+2*by(6)+by(5)) & get_lower_digit: BYTE --> 0..15 & get_lower_digit = %by.(by: BYTE | 8*by(4)+4*by(3)+2*by(2)+by(1)) & BV16_INDX = 1..16 & PHYS_BV16_INDEX = 0..16-1 & BV16 = BV16_INDX --> BIT & !b1.(b1: BV16 => size(b1) = 16 & b1: seq1(BIT)) & complement16 = %bt.(bt: BV16 | %idx.(idx: 1..16 | bit_not(bt(idx)))) & bv16_byte = %bv.(bv: BV16 | {8|->bv(16),7|->bv(15),6|->bv(14),5|->bv(13),4|->bv(12),3|->bv(11),2|->bv(10),1|->bv(9)},{8|->bv(8),7|->bv(7),6|->bv(6),5|->bv(5),4|->bv(4),3|->bv(3),2|->bv(2),1|->bv(1)}) & byte_bv16 = %(bv1,bv2).(bv1: BYTE & bv2: BYTE | {16|->bv1(8),15|->bv1(7),14|->bv1(6),13|->bv1(5),12|->bv1(4),11|->bv1(3),10|->bv1(2),9|->bv1(1),8|->bv2(8),7|->bv2(7),6|->bv2(6),5|->bv2(5),4|->bv2(4),3|->bv2(3),2|->bv2(2),1|->bv2(1)}) & UCHAR_MAX = 256-1 & UCHAR_MIN = 0 & UCHAR = UCHAR_MIN..UCHAR_MAX & byte_uchar = %v0.(v0: BYTE | 128*v0(8)+64*v0(7)+32*v0(6)+16*v0(5)+8*v0(4)+4*v0(3)+2*v0(2)+1*v0(1)) & uchar_byte = %v0.(v0: UCHAR | [v0 mod 2/1,v0 mod 4/2,v0 mod 8/4,v0 mod 16/8,v0 mod 32/16,v0 mod 64/32,v0 mod 128/64,v0 mod 256/128]) & SCHAR_MAX = 128-1 & SCHAR_MIN = -128 & SCHAR = SCHAR_MIN..SCHAR_MAX & byte_schar = %v0.(v0: BYTE | (-128)*v0(8)+64*v0(7)+32*v0(6)+16*v0(5)+8*v0(4)+4*v0(3)+2*v0(2)+v0(1)) & schar_byte = %v0.(v0: SCHAR & v0>=0 | [v0 mod 2/1,v0 mod 4/2,v0 mod 8/4,v0 mod 16/8,v0 mod 32/16,v0 mod 64/32,v0 mod 128/64,v0 mod 256/128])\/%v0.(v0: SCHAR & not(v0>=0) | [(v0+256) mod 2/1,(v0+256) mod 4/2,(v0+256) mod 8/4,(v0+256) mod 16/8,(v0+256) mod 32/16,(v0+256) mod 64/32,(v0+256) mod 128/64,(v0+256) mod 256/128]) & uchar_schar = %v1.(v1: UCHAR & v1<=SCHAR_MAX | v1)\/%v1.(v1: UCHAR & not(v1<=SCHAR_MAX) | v1-256) & schar_uchar = %v1.(v1: 0..127 | v1)\/%v1.(v1: -128.. -1 | v1+256) & SSHORT_MIN = -32768 & SSHORT_MAX = 32768-1 & SSHORT = SSHORT_MIN..SSHORT_MAX & bv16_sshort = %v0.(v0: BV16 | (-32768)*v0(16)+16384*v0(15)+8192*v0(14)+4096*v0(13)+2048*v0(12)+1024*v0(11)+512*v0(10)+256*v0(9)+128*v0(8)+64*v0(7)+32*v0(6)+16*v0(5)+8*v0(4)+4*v0(3)+2*v0(2)+v0(1)) & sshort_bv16 = %v0.(v0: SSHORT & v0>=0 | [v0 mod 2/1,v0 mod 4/2,v0 mod 8/4,v0 mod 16/8,v0 mod 32/16,v0 mod 64/32,v0 mod 128/64,v0 mod 256/128,v0 mod 512/256,v0 mod 1024/512,v0 mod 2048/1024,v0 mod 4096/2048,v0 mod 8192/4096,v0 mod 16384/8192,v0 mod 32768/16384,v0 mod 65536/32768])\/%v0.(v0: SSHORT & not(v0>=0) | [(v0+USHORT_MAX+1) mod 2/1,(v0+USHORT_MAX+1) mod 4/2,(v0+USHORT_MAX+1) mod 8/4,(v0+USHORT_MAX+1) mod 16/8,(v0+USHORT_MAX+1) mod 32/16,(v0+USHORT_MAX+1) mod 64/32,(v0+USHORT_MAX+1) mod 128/64,(v0+USHORT_MAX+1) mod 256/128,(v0+USHORT_MAX+1) mod 512/256,(v0+USHORT_MAX+1) mod 1024/512,(v0+USHORT_MAX+1) mod 2048/1024,(v0+USHORT_MAX+1) mod 4096/2048,(v0+USHORT_MAX+1) mod 8192/4096,(v0+USHORT_MAX+1) mod 16384/8192,(v0+USHORT_MAX+1) mod 32768/16384,(v0+USHORT_MAX+1) mod 65536/32768]) & ushort_sshort = %v1.(v1: USHORT & v1<=SSHORT_MAX | v1)\/%v1.(v1: USHORT & not(v1<=SSHORT_MAX) | v1-USHORT_MAX-1) & sshort_ushort = %v1.(v1: SSHORT & v1>=0 | v1)\/%v1.(v1: SSHORT & not(v1>=0) | v1+USHORT_MAX+1) & schar_sshort = %(w1,w2).(w1: SCHAR & w2: SCHAR | bv16_sshort(byte_bv16(schar_byte(w1),schar_byte(w2)))) & USHORT_MAX = 65536-1 & USHORT_MIN = 0 & USHORT = USHORT_MIN..USHORT_MAX & bv16_ushort = %v0.(v0: BV16 | 32768*v0(16)+16384*v0(15)+8192*v0(14)+4096*v0(13)+2048*v0(12)+1024*v0(11)+512*v0(10)+256*v0(9)+128*v0(8)+64*v0(7)+32*v0(6)+16*v0(5)+8*v0(4)+4*v0(3)+2*v0(2)+1*v0(1)) & ushort_bv16 = %v0.(v0: USHORT | [v0 mod 2/1,v0 mod 4/2,v0 mod 8/4,v0 mod 16/8,v0 mod 32/16,v0 mod 64/32,v0 mod 128/64,v0 mod 256/128,v0 mod 512/256,v0 mod 1024/512,v0 mod 2048/1024,v0 mod 4096/2048,v0 mod 8192/4096,v0 mod 16384/8192,v0 mod 32768/16384,v0 mod 65536/32768]) & uchar_ushort = %(w1,w2).(w1: UCHAR & w2: UCHAR | bv16_ushort(byte_bv16(uchar_byte(w1),uchar_byte(w2)))));
  Inherited_List_Properties(Machine(MEMORY))==(btrue);
  List_Properties(Machine(MEMORY))==(PROGRAM_R_ADR = USHORT & DATA_R_ADR = USHORT & STACK_R_ADR = USHORT)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(POWER2))==(?);
  Seen_Context_List_Assertions(Machine(MEMORY))==(!(nn,pp).(nn: NATURAL1 & pp: NAT => (pp = 0 => nn**pp = 1) & (pp = 1 => nn**pp = nn) & (pp>1 => nn**pp = nn*nn**(pp-1))));
  Seen_Context_List_Enumerated(Machine(MEMORY))==(?);
  Seen_Context_List_Invariant(Machine(MEMORY))==(btrue);
  Seen_Context_List_Assertions(Machine(MEMORY))==(byte_uchar: BYTE >->> UCHAR & uchar_byte: UCHAR >->> BYTE & byte_uchar = uchar_byte~ & schar_uchar: SCHAR >->> UCHAR & uchar_schar: UCHAR >->> SCHAR & schar_uchar = uchar_schar~ & byte_schar: BYTE >->> SCHAR & schar_byte: SCHAR >->> BYTE & byte_schar = schar_byte~ & bv16_byte: BV16 >->> BYTE*BYTE & byte_bv16: BYTE*BYTE >->> BV16 & bv16_byte = byte_bv16~ & bv16_ushort: BV16 >->> USHORT & ushort_bv16: USHORT >->> BV16 & bv16_ushort = ushort_bv16~ & uchar_ushort: UCHAR*UCHAR >->> USHORT & bv16_sshort: BV16 >->> SSHORT & sshort_bv16: SSHORT >->> BV16 & bv16_sshort = sshort_bv16~ & schar_sshort: SCHAR*SCHAR >->> SSHORT & sshort_ushort: SSHORT >->> USHORT & ushort_sshort: USHORT >->> SSHORT & sshort_ushort = ushort_sshort~ & !bb.(bb: BIT => bit_not(bb) = 1-bb) & dom(bit_and) = BIT*BIT & ran(bit_not) = BIT & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & dom(bit_and) = BIT*BIT & ran(bit_and) = BIT & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & dom(bit_or) = BIT*BIT & ran(bit_or) = BIT & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & dom(bit_xor) = BIT*BIT & ran(bit_xor) = BIT & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & dom(bool_bit) = BOOL & ran(bit_xor) = BIT & bool_bit(TRUE) = 1 & bool_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (card(BYTE) = 256 & is_zero: BYTE --> BIT;parity_even: BYTE --> BIT;bv8_and: BYTE*BYTE --> BYTE;bv8_or: BYTE*BYTE --> BYTE;bv8_xor: BYTE*BYTE --> BYTE;complement: BYTE --> BYTE;swap: BYTE --> BYTE;rotateleft: BYTE --> BYTE;rotateright: BYTE --> BYTE;!bt.(bt: BYTE => size(bt) = 8);8: NATURAL;dom(is_zero) = BYTE;ran(is_zero) <: BIT;dom(parity_even) = BYTE;ran(parity_even) <: BIT;bv8_and: BYTE*BYTE --> BYTE;dom(complement) = BYTE;ran(complement) <: BYTE;dom(swap) = BYTE;ran(swap) <: BYTE;dom(rotateleft) = BYTE;ran(rotateleft) <: BYTE;dom(rotateright) = BYTE;ran(rotateright) <: BYTE;dom(get_upper_digit) = BYTE;ran(get_upper_digit) <: 0..16-1;dom(get_lower_digit) = BYTE;ran(get_lower_digit) <: 0..16-1;[1,1,1,1,1,1,1,1]: BYTE & [0,0,0,0,0,0,0,0]: BYTE & 1 = 2**0 & 2 = 2**1 & 4 = 2**2 & 8 = 2**3 & 16 = 2**4 & 32 = 2**5 & 64 = 2**6 & 128 = 2**7 & 256 = 2**8 & 512 = 2**9 & 1024 = 2**10 & 2048 = 2**11 & 4096 = 2**12 & 8192 = 2**13 & 16384 = 2**14 & 32768 = 2**15 & 65536 = 2**16) & card(BV16) = 65536 & complement16: BV16 --> BV16 & byte_bv16 = bv16_byte~ & [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]: BV16 & [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]: BV16 & card(byte_uchar) = 256 & card(uchar_byte) = 256 & UCHAR_MAX: INTEGER & UCHAR_MIN: INTEGER & 1 = 2**0 & 2 = 2**1 & 4 = 2**2 & 8 = 2**3 & 16 = 2**4 & 32 = 2**5 & 64 = 2**6 & 128 = 2**7 & 256 = 2**8 & card(byte_schar) = 256 & card(schar_byte) = 256 & SCHAR_MAX: INTEGER & SCHAR_MIN: INTEGER & 1 = 2**0 & 2 = 2**1 & 4 = 2**2 & 8 = 2**3 & 16 = 2**4 & 32 = 2**5 & 64 = 2**6 & 128 = 2**7 & 256 = 2**8 & card(bv16_sshort) = 65536 & card(sshort_bv16) = 65536 & SSHORT_MIN: SSHORT & SSHORT_MAX: SSHORT & 1 = 2**0 & 2 = 2**1 & 4 = 2**2 & 8 = 2**3 & 16 = 2**4 & 32 = 2**5 & 64 = 2**6 & 128 = 2**7 & 256 = 2**8 & 512 = 2**9 & 1024 = 2**10 & 2048 = 2**11 & 4096 = 2**12 & 8192 = 2**13 & 16384 = 2**14 & 32768 = 2**15 & 65536 = 2**16 & card(bv16_ushort) = 65536 & card(ushort_bv16) = 65536 & USHORT_MAX: INTEGER & USHORT_MIN: INTEGER & 1 = 2**0 & 2 = 2**1 & 4 = 2**2 & 8 = 2**3 & 16 = 2**4 & 32 = 2**5 & 64 = 2**6 & 128 = 2**7 & 256 = 2**8 & 512 = 2**9 & 1024 = 2**10 & 2048 = 2**11 & 4096 = 2**12 & 8192 = 2**13 & 16384 = 2**14 & 32768 = 2**15 & 65536 = 2**16 & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;(-2)**7 = -128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536));
  Seen_Context_List_Properties(Machine(MEMORY))==(BIT = {0,1} & bit_not: BIT >->> BIT & bit_not = {0|->1}\/{1|->0} & bit_and: BIT*BIT --> BIT & bit_and = {(0,0)|->0}\/{(0,1)|->0}\/{(1,0)|->0}\/{(1,1)|->1} & bit_or: BIT*BIT --> BIT & bit_or = {(0,0)|->0}\/{(0,1)|->1}\/{(1,0)|->1}\/{(1,1)|->1} & bit_xor: BIT*BIT --> BIT & bit_xor = {(0,0)|->0}\/{(0,1)|->1}\/{(1,0)|->1}\/{(1,1)|->0} & bool_bit = {TRUE|->1,FALSE|->0} & BYTE_INDEX = 1..8 & PHYS_BYTE_INDEX = 0..8-1 & BYTE = BYTE_INDEX --> BIT & !b1.(b1: BYTE => size(b1) = 8 & b1: seq1(BIT)) & is_zero: BYTE --> BIT & is_zero = %w1.(w1: BYTE | bool_bit(bool(w1(1) = w1(2) & w1(2) = w1(3) & w1(3) = w1(4) & w1(4) = w1(5) & w1(5) = w1(6) & w1(6) = w1(7) & w1(7) = w1(8) & w1(8) = 0))) & parity_even: BYTE --> BIT & parity_even = %bv.(bv: BYTE | 1-(bv(1)+bv(2)+bv(3)+bv(4)+bv(5)+bv(6)+bv(7)+bv(8)) mod 2) & bv8_and = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | %idx.(idx: 1..8 | bit_and(bt1(idx),bt2(idx)))) & bv8_or = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | %idx.(idx: 1..8 | bit_or(bt1(idx),bt2(idx)))) & bv8_xor = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | %idx.(idx: 1..8 | bit_xor(bt1(idx),bt2(idx)))) & bv8get: BYTE*PHYS_BYTE_INDEX --> BIT & bv8get = %(bt1,ii).(bt1: BYTE & ii: PHYS_BYTE_INDEX | bt1(ii+1)) & bitset: BYTE*PHYS_BYTE_INDEX --> BYTE & bitset = %(bt1,ii).(bt1: BYTE & ii: PHYS_BYTE_INDEX | bt1<+{ii+1|->1}) & bitclear: BYTE*PHYS_BYTE_INDEX --> BYTE & bitclear = %(bt1,ii).(bt1: BYTE & ii: PHYS_BYTE_INDEX | bt1<+{ii+1|->0}) & complement: BYTE --> BYTE & complement = %bt.(bt: BYTE | %idx.(idx: 1..8 | bit_not(bt(idx)))) & swap: BYTE --> BYTE & swap = %bt.(bt: BYTE | {1|->bt(5),2|->bt(6),3|->bt(7),4|->bt(8),5|->bt(1),6|->bt(2),7|->bt(3),8|->bt(4)}) & rotateleft: BYTE --> BYTE & rotateleft = %bv.(bv: BYTE | {1|->bv(8),2|->bv(1),3|->bv(2),4|->bv(3),5|->bv(4),6|->bv(5),7|->bv(6),8|->bv(7)}) & rotateright: BYTE --> BYTE & rotateright = %bv.(bv: BYTE | {1|->bv(2),2|->bv(3),3|->bv(4),4|->bv(5),5|->bv(6),6|->bv(7),7|->bv(8),8|->bv(1)}) & get_upper_digit: BYTE --> 0..15 & get_upper_digit = %by.(by: BYTE | 8*by(8)+4*by(7)+2*by(6)+by(5)) & get_lower_digit: BYTE --> 0..15 & get_lower_digit = %by.(by: BYTE | 8*by(4)+4*by(3)+2*by(2)+by(1)) & BV16_INDX = 1..16 & PHYS_BV16_INDEX = 0..16-1 & BV16 = BV16_INDX --> BIT & !b1.(b1: BV16 => size(b1) = 16 & b1: seq1(BIT)) & complement16 = %bt.(bt: BV16 | %idx.(idx: 1..16 | bit_not(bt(idx)))) & bv16_byte = %bv.(bv: BV16 | {8|->bv(16),7|->bv(15),6|->bv(14),5|->bv(13),4|->bv(12),3|->bv(11),2|->bv(10),1|->bv(9)},{8|->bv(8),7|->bv(7),6|->bv(6),5|->bv(5),4|->bv(4),3|->bv(3),2|->bv(2),1|->bv(1)}) & byte_bv16 = %(bv1,bv2).(bv1: BYTE & bv2: BYTE | {16|->bv1(8),15|->bv1(7),14|->bv1(6),13|->bv1(5),12|->bv1(4),11|->bv1(3),10|->bv1(2),9|->bv1(1),8|->bv2(8),7|->bv2(7),6|->bv2(6),5|->bv2(5),4|->bv2(4),3|->bv2(3),2|->bv2(2),1|->bv2(1)}) & UCHAR_MAX = 256-1 & UCHAR_MIN = 0 & UCHAR = UCHAR_MIN..UCHAR_MAX & byte_uchar = %v0.(v0: BYTE | 128*v0(8)+64*v0(7)+32*v0(6)+16*v0(5)+8*v0(4)+4*v0(3)+2*v0(2)+1*v0(1)) & uchar_byte = %v0.(v0: UCHAR | [v0 mod 2/1,v0 mod 4/2,v0 mod 8/4,v0 mod 16/8,v0 mod 32/16,v0 mod 64/32,v0 mod 128/64,v0 mod 256/128]) & SCHAR_MAX = 128-1 & SCHAR_MIN = -128 & SCHAR = SCHAR_MIN..SCHAR_MAX & byte_schar = %v0.(v0: BYTE | (-128)*v0(8)+64*v0(7)+32*v0(6)+16*v0(5)+8*v0(4)+4*v0(3)+2*v0(2)+v0(1)) & schar_byte = %v0.(v0: SCHAR & v0>=0 | [v0 mod 2/1,v0 mod 4/2,v0 mod 8/4,v0 mod 16/8,v0 mod 32/16,v0 mod 64/32,v0 mod 128/64,v0 mod 256/128])\/%v0.(v0: SCHAR & not(v0>=0) | [(v0+256) mod 2/1,(v0+256) mod 4/2,(v0+256) mod 8/4,(v0+256) mod 16/8,(v0+256) mod 32/16,(v0+256) mod 64/32,(v0+256) mod 128/64,(v0+256) mod 256/128]) & uchar_schar = %v1.(v1: UCHAR & v1<=SCHAR_MAX | v1)\/%v1.(v1: UCHAR & not(v1<=SCHAR_MAX) | v1-256) & schar_uchar = %v1.(v1: 0..127 | v1)\/%v1.(v1: -128.. -1 | v1+256) & SSHORT_MIN = -32768 & SSHORT_MAX = 32768-1 & SSHORT = SSHORT_MIN..SSHORT_MAX & bv16_sshort = %v0.(v0: BV16 | (-32768)*v0(16)+16384*v0(15)+8192*v0(14)+4096*v0(13)+2048*v0(12)+1024*v0(11)+512*v0(10)+256*v0(9)+128*v0(8)+64*v0(7)+32*v0(6)+16*v0(5)+8*v0(4)+4*v0(3)+2*v0(2)+v0(1)) & sshort_bv16 = %v0.(v0: SSHORT & v0>=0 | [v0 mod 2/1,v0 mod 4/2,v0 mod 8/4,v0 mod 16/8,v0 mod 32/16,v0 mod 64/32,v0 mod 128/64,v0 mod 256/128,v0 mod 512/256,v0 mod 1024/512,v0 mod 2048/1024,v0 mod 4096/2048,v0 mod 8192/4096,v0 mod 16384/8192,v0 mod 32768/16384,v0 mod 65536/32768])\/%v0.(v0: SSHORT & not(v0>=0) | [(v0+USHORT_MAX+1) mod 2/1,(v0+USHORT_MAX+1) mod 4/2,(v0+USHORT_MAX+1) mod 8/4,(v0+USHORT_MAX+1) mod 16/8,(v0+USHORT_MAX+1) mod 32/16,(v0+USHORT_MAX+1) mod 64/32,(v0+USHORT_MAX+1) mod 128/64,(v0+USHORT_MAX+1) mod 256/128,(v0+USHORT_MAX+1) mod 512/256,(v0+USHORT_MAX+1) mod 1024/512,(v0+USHORT_MAX+1) mod 2048/1024,(v0+USHORT_MAX+1) mod 4096/2048,(v0+USHORT_MAX+1) mod 8192/4096,(v0+USHORT_MAX+1) mod 16384/8192,(v0+USHORT_MAX+1) mod 32768/16384,(v0+USHORT_MAX+1) mod 65536/32768]) & ushort_sshort = %v1.(v1: USHORT & v1<=SSHORT_MAX | v1)\/%v1.(v1: USHORT & not(v1<=SSHORT_MAX) | v1-USHORT_MAX-1) & sshort_ushort = %v1.(v1: SSHORT & v1>=0 | v1)\/%v1.(v1: SSHORT & not(v1>=0) | v1+USHORT_MAX+1) & schar_sshort = %(w1,w2).(w1: SCHAR & w2: SCHAR | bv16_sshort(byte_bv16(schar_byte(w1),schar_byte(w2)))) & USHORT_MAX = 65536-1 & USHORT_MIN = 0 & USHORT = USHORT_MIN..USHORT_MAX & bv16_ushort = %v0.(v0: BV16 | 32768*v0(16)+16384*v0(15)+8192*v0(14)+4096*v0(13)+2048*v0(12)+1024*v0(11)+512*v0(10)+256*v0(9)+128*v0(8)+64*v0(7)+32*v0(6)+16*v0(5)+8*v0(4)+4*v0(3)+2*v0(2)+1*v0(1)) & ushort_bv16 = %v0.(v0: USHORT | [v0 mod 2/1,v0 mod 4/2,v0 mod 8/4,v0 mod 16/8,v0 mod 32/16,v0 mod 64/32,v0 mod 128/64,v0 mod 256/128,v0 mod 512/256,v0 mod 1024/512,v0 mod 2048/1024,v0 mod 4096/2048,v0 mod 8192/4096,v0 mod 16384/8192,v0 mod 32768/16384,v0 mod 65536/32768]) & uchar_ushort = %(w1,w2).(w1: UCHAR & w2: UCHAR | bv16_ushort(byte_bv16(uchar_byte(w1),uchar_byte(w2)))));
  Seen_List_Constraints(Machine(MEMORY))==(btrue);
  Seen_List_Operations(Machine(MEMORY),Machine(POWER2))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(POWER2))==(btrue);
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(USHORT_DEFINITION))==(?);
  Seen_List_Operations(Machine(MEMORY),Machine(USHORT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(USHORT_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(SSHORT_DEFINITION))==(?);
  Seen_List_Operations(Machine(MEMORY),Machine(SSHORT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(SSHORT_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(SCHAR_DEFINITION))==(?);
  Seen_List_Operations(Machine(MEMORY),Machine(SCHAR_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(SCHAR_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(UCHAR_DEFINITION))==(?);
  Seen_List_Operations(Machine(MEMORY),Machine(UCHAR_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(UCHAR_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(BV16_DEFINITION))==(?);
  Seen_List_Operations(Machine(MEMORY),Machine(BV16_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(BV16_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(BYTE_DEFINITION))==(?);
  Seen_List_Operations(Machine(MEMORY),Machine(BYTE_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(BYTE_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(BIT_DEFINITION))==(?);
  Seen_List_Operations(Machine(MEMORY),Machine(BIT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(BIT_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(ALU))==(?);
  Seen_List_Operations(Machine(MEMORY),Machine(ALU))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(ALU))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(MEMORY),updateMem)==(?);
  List_ANY_Var(Machine(MEMORY),updateAddressMem)==(?);
  List_ANY_Var(Machine(MEMORY),updateStack)==(?);
  List_ANY_Var(Machine(MEMORY),updateAddressStack)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(MEMORY)) == (PROGRAM_R_ADR,DATA_R_ADR,STACK_R_ADR | ? | mem | ? | updateMem,updateAddressMem,updateStack,updateAddressStack | ? | seen(Machine(ALU)),seen(Machine(BIT_DEFINITION)),seen(Machine(BYTE_DEFINITION)),seen(Machine(BV16_DEFINITION)),seen(Machine(UCHAR_DEFINITION)),seen(Machine(SCHAR_DEFINITION)),seen(Machine(SSHORT_DEFINITION)),seen(Machine(USHORT_DEFINITION)),seen(Machine(POWER2)) | ? | MEMORY);
  List_Of_HiddenCst_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(MEMORY)) == (PROGRAM_R_ADR,DATA_R_ADR,STACK_R_ADR);
  List_Of_VisibleVar_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(MEMORY)) == (?: ?);
  List_Of_Ids(Machine(POWER2)) == (? | ? | ? | ? | ? | ? | seen(Machine(POWER)) | ? | POWER2);
  List_Of_HiddenCst_Ids(Machine(POWER2)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(POWER2)) == (?);
  List_Of_VisibleVar_Ids(Machine(POWER2)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(POWER2)) == (?: ?);
  List_Of_Ids(Machine(POWER)) == (? | ? | ? | ? | ? | ? | ? | ? | POWER);
  List_Of_HiddenCst_Ids(Machine(POWER)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(POWER)) == (?);
  List_Of_VisibleVar_Ids(Machine(POWER)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(POWER)) == (?: ?);
  List_Of_Ids(Machine(USHORT_DEFINITION)) == (USHORT_MAX,USHORT_MIN,USHORT,bv16_ushort,ushort_bv16,uchar_ushort | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BYTE_DEFINITION)),seen(Machine(BV16_DEFINITION)),seen(Machine(UCHAR_DEFINITION)),seen(Machine(POWER2)) | ? | USHORT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(USHORT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(USHORT_DEFINITION)) == (USHORT_MAX,USHORT_MIN,USHORT,bv16_ushort,ushort_bv16,uchar_ushort);
  List_Of_VisibleVar_Ids(Machine(USHORT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(USHORT_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(UCHAR_DEFINITION)) == (UCHAR_MAX,UCHAR_MIN,UCHAR,uchar_byte,byte_uchar | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BYTE_DEFINITION)),seen(Machine(POWER2)) | ? | UCHAR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(UCHAR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(UCHAR_DEFINITION)) == (UCHAR_MAX,UCHAR_MIN,UCHAR,uchar_byte,byte_uchar);
  List_Of_VisibleVar_Ids(Machine(UCHAR_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(UCHAR_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_INDEX,PHYS_BYTE_INDEX,is_zero,parity_even,bv8_and,bv8_or,bv8_xor,bitclear,bitset,bv8get,complement,swap,rotateleft,rotateright,get_upper_digit,get_lower_digit | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(POWER2)) | ? | BYTE_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_INDEX,PHYS_BYTE_INDEX,is_zero,parity_even,bv8_and,bv8_or,bv8_xor,bitclear,bitset,bv8get,complement,swap,rotateleft,rotateright,get_upper_digit,get_lower_digit);
  List_Of_VisibleVar_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BYTE_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_bit | ? | ? | ? | ? | ? | ? | ? | BIT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_bit);
  List_Of_VisibleVar_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BV16_DEFINITION)) == (BV16_INDX,PHYS_BV16_INDEX,BV16,bv16_byte,byte_bv16,complement16 | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BYTE_DEFINITION)) | ? | BV16_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BV16_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BV16_DEFINITION)) == (BV16_INDX,PHYS_BV16_INDEX,BV16,bv16_byte,byte_bv16,complement16);
  List_Of_VisibleVar_Ids(Machine(BV16_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BV16_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(SSHORT_DEFINITION)) == (SSHORT_MIN,SSHORT_MAX,SSHORT,bv16_sshort,sshort_bv16,schar_sshort,ushort_sshort,sshort_ushort | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BYTE_DEFINITION)),seen(Machine(BV16_DEFINITION)),seen(Machine(SCHAR_DEFINITION)),seen(Machine(USHORT_DEFINITION)),seen(Machine(POWER2)) | ? | SSHORT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(SSHORT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(SSHORT_DEFINITION)) == (SSHORT_MIN,SSHORT_MAX,SSHORT,bv16_sshort,sshort_bv16,schar_sshort,ushort_sshort,sshort_ushort);
  List_Of_VisibleVar_Ids(Machine(SSHORT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(SSHORT_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(SCHAR_DEFINITION)) == (SCHAR_MAX,SCHAR_MIN,SCHAR,byte_schar,schar_byte,uchar_schar,schar_uchar | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BYTE_DEFINITION)),seen(Machine(UCHAR_DEFINITION)),seen(Machine(POWER2)) | ? | SCHAR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(SCHAR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(SCHAR_DEFINITION)) == (SCHAR_MAX,SCHAR_MIN,SCHAR,byte_schar,schar_byte,uchar_schar,schar_uchar);
  List_Of_VisibleVar_Ids(Machine(SCHAR_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(SCHAR_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(ALU)) == (is_zero16USHORT,is_negative,half8UCHAR,inc,dec,simple_add8UCHAR,add8UCHAR,substract8UCHAR,add16USHORT,add_carryUSHORT,add_halfcarryUSHORT,sub16USHORT,sub_carryUSHORT,sub_halfcarryUSHORT,inc_BV16,dec_BV16,update_refresh_reg,instruction_next,instruction_jump | ? | ? | ? | ? | ? | seen(Machine(TYPES)),seen(Machine(BIT_DEFINITION)),seen(Machine(BYTE_DEFINITION)),seen(Machine(BV16_DEFINITION)),seen(Machine(UCHAR_DEFINITION)),seen(Machine(SCHAR_DEFINITION)),seen(Machine(SSHORT_DEFINITION)),seen(Machine(USHORT_DEFINITION)),seen(Machine(POWER2)) | ? | ALU);
  List_Of_HiddenCst_Ids(Machine(ALU)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ALU)) == (is_zero16USHORT,is_negative,half8UCHAR,inc,dec,simple_add8UCHAR,add8UCHAR,substract8UCHAR,add16USHORT,add_carryUSHORT,add_halfcarryUSHORT,sub16USHORT,sub_carryUSHORT,sub_halfcarryUSHORT,inc_BV16,dec_BV16,update_refresh_reg,instruction_next,instruction_jump);
  List_Of_VisibleVar_Ids(Machine(ALU)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ALU)) == (?: ?);
  List_Of_Ids(Machine(TYPES)) == (? | ? | ? | ? | ? | ? | seen(Machine(UCHAR_DEFINITION)),seen(Machine(SCHAR_DEFINITION)),seen(Machine(USHORT_DEFINITION)),seen(Machine(SSHORT_DEFINITION)),seen(Machine(BYTE_DEFINITION)),seen(Machine(BV16_DEFINITION)),seen(Machine(BIT_DEFINITION)) | ? | TYPES);
  List_Of_HiddenCst_Ids(Machine(TYPES)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (?);
  List_Of_VisibleVar_Ids(Machine(TYPES)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPES)) == (?: ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(MEMORY)) == (Type(PROGRAM_R_ADR) == Cst(SetOf(btype(INTEGER,"[PROGRAM_R_ADR","]PROGRAM_R_ADR")));Type(DATA_R_ADR) == Cst(SetOf(btype(INTEGER,"[DATA_R_ADR","]DATA_R_ADR")));Type(STACK_R_ADR) == Cst(SetOf(btype(INTEGER,"[STACK_R_ADR","]STACK_R_ADR"))))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(MEMORY)) == (Type(mem) == Mvl(SetOf(SetOf(btype(INTEGER,"[BV16_INDX","]BV16_INDX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BYTE_INDEX","]BYTE_INDEX")*btype(INTEGER,"[BIT","]BIT")))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(MEMORY)) == (Type(updateAddressStack) == Cst(No_type,SetOf(btype(INTEGER,"[BV16_INDX","]BV16_INDX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BYTE_INDEX","]BYTE_INDEX")*btype(INTEGER,"[BIT","]BIT")));Type(updateStack) == Cst(No_type,SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(updateAddressMem) == Cst(No_type,SetOf(btype(INTEGER,"[BV16_INDX","]BV16_INDX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BYTE_INDEX","]BYTE_INDEX")*btype(INTEGER,"[BIT","]BIT")));Type(updateMem) == Cst(No_type,SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO;
  project_type == SOFTWARE_TYPE;
  event_b_deadlockfreeness == KO;
  variant_clause_mandatory == KO;
  event_b_coverage == KO;
  event_b_exclusivity == KO;
  genFeasibilityPO == KO
END
)
