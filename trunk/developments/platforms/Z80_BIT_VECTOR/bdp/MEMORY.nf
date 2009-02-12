Normalised(
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
  List_Sees(Machine(MEMORY))==(TYPES,ALU)
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
  Local_List_Variables(Machine(MEMORY))==(stack,mem);
  List_Variables(Machine(MEMORY))==(stack,mem);
  External_List_Variables(Machine(MEMORY))==(stack,mem)
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
  List_Invariant(Machine(MEMORY))==(mem: BV16 --> BYTE & stack: BV16 --> BYTE & stack <<: mem)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(MEMORY))==(btrue);
  Abstract_List_Assertions(Machine(MEMORY))==(btrue);
  Context_List_Assertions(Machine(MEMORY))==(0 = schar_to_sshortint(0,0) & !n0.(n0: UCHAR => 0<=n0) & !n0.(n0: UCHAR => n0<=255) & instruction_next: USHORTINT --> USHORTINT & !xx.(xx: BYTE => byte_to_uchar(xx): UCHAR) & !xx.(xx: UCHAR => uchar_to_byte(xx): BYTE) & !xx.(xx: BYTE => update_refresh_reg(xx): BYTE) & !xx.(xx: BYTE => byte_to_schar(xx): SCHAR) & !xx.(xx: SCHAR => schar_to_byte(xx): BYTE) & !(xx,yy).(xx: BYTE & yy: BYTE => byte_to_bv16(xx,yy): BV16) & !(xx,yy).(xx: BYTE & yy: BYTE => #zz.(zz: BV16 & byte_to_bv16(xx,yy) = zz)) & !xx.(xx: BV16 => bv16_to_ushortint(xx): USHORTINT) & !xx.(xx: USHORTINT => ushortint_to_bv16(xx): BV16) & !xx.(xx: BYTE => get_upper_digit(xx): 0..16) & !xx.(xx: BYTE => get_lower_digit(xx): 0..16) & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_to_bit(TRUE) = 1 & bool_to_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_not(bv_not(bv)),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: 0..bv_size(bv)-1 & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,v2),indx) = bv_get(bv_and(v2,v1),indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,bv_and(v2,v3)),indx) = bv_get(bv_and(bv_and(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv_zero(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_one(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,v2),indx) = bv_get(bv_or(v2,v1),indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,bv_or(v2,v3)),indx) = bv_get(bv_or(bv_or(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_one(bv_size(bv))),indx) = bv_get(bv_one(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_xor(v1,v2),indx) = bv_get(bv_xor(v2,v1),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_xor(bv,bv),indx) = bv_get(bv_zero(bv_size(bv)),indx))) & !bt.(bt: BYTE => size(bt) = 8) & size(BYTE_ZERO) = 8 & BYTE <: BIT_VECTOR & BYTE_ZERO: BIT_VECTOR & first(BYTE_ZERO) = 0 & BV16_ZERO: BV16 & BV16 <: BIT_VECTOR & BV16_ZERO: BIT_VECTOR & dom(add8UCHAR) = BIT*UCHAR*UCHAR & ran(add8UCHAR): POW(UCHAR*BIT*BIT*BIT*BIT) & dom(substract8UCHAR) = BIT*UCHAR*UCHAR & ran(substract8UCHAR): POW(UCHAR*BIT*BIT*BIT*BIT) & dom(and) = BYTE*BYTE & ran(and) <: BYTE & dom(ior) = BYTE*BYTE & ran(ior) <: BYTE & dom(xor) = BYTE*BYTE & ran(xor) <: BYTE & dom(complement) = BYTE & ran(complement) <: BYTE & dom(swap) = BYTE & ran(swap) <: BYTE & dom(rotateleft) = BYTE & ran(rotateleft) <: BYTE & dom(rotateright) = BYTE & ran(rotateright) <: BYTE & !(vec,in0).(vec: BYTE & in0: 0..7 => bitget(vec,in0) = vec(in0)) & !(x0,x1).(x0: UCHAR & x1: UCHAR => simple_add8UCHAR(x0,x1): UCHAR));
  List_Assertions(Machine(MEMORY))==(dom(stack) = BV16 & ran(stack) <: BYTE & ran(mem) <: BYTE & dom(mem) = BV16 & !(address,value).(address: BV16 & value: BYTE => mem<+{address|->value}: BV16 --> BYTE) & !(address,value).(address: BV16 & value: BYTE => stack<+{address|->value}: BV16 --> BYTE))
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(MEMORY))==(@(mem$0).(mem$0: BV16 --> BYTE ==> mem:=mem$0) || @(stack$0).(stack$0: BV16 --> BYTE ==> stack:=stack$0));
  Context_List_Initialisation(Machine(MEMORY))==(skip);
  List_Initialisation(Machine(MEMORY))==(mem:: BV16 --> BYTE || stack:: BV16 --> BYTE)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(MEMORY))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(MEMORY),Machine(TYPES))==(?);
  List_Instanciated_Parameters(Machine(MEMORY),Machine(ALU))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(MEMORY))==(btrue);
  List_Constraints(Machine(MEMORY))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(MEMORY))==(updateMem,updateAddressMem,updateStack,updateAddressStack,pop);
  List_Operations(Machine(MEMORY))==(updateMem,updateAddressMem,updateStack,updateAddressStack,pop)
END
&
THEORY ListInputX IS
  List_Input(Machine(MEMORY),updateMem)==(NewValues);
  List_Input(Machine(MEMORY),updateAddressMem)==(address,value);
  List_Input(Machine(MEMORY),updateStack)==(NewValues);
  List_Input(Machine(MEMORY),updateAddressStack)==(address,value);
  List_Input(Machine(MEMORY),pop)==(sp)
END
&
THEORY ListOutputX IS
  List_Output(Machine(MEMORY),updateMem)==(?);
  List_Output(Machine(MEMORY),updateAddressMem)==(?);
  List_Output(Machine(MEMORY),updateStack)==(?);
  List_Output(Machine(MEMORY),updateAddressStack)==(?);
  List_Output(Machine(MEMORY),pop)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(MEMORY),updateMem)==(updateMem(NewValues));
  List_Header(Machine(MEMORY),updateAddressMem)==(updateAddressMem(address,value));
  List_Header(Machine(MEMORY),updateStack)==(updateStack(NewValues));
  List_Header(Machine(MEMORY),updateAddressStack)==(updateAddressStack(address,value));
  List_Header(Machine(MEMORY),pop)==(pop(sp))
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(MEMORY),updateMem)==(NewValues: BV16 --> BYTE & dom(NewValues)/\dom(stack) = {});
  List_Precondition(Machine(MEMORY),updateAddressMem)==(address: BV16 & value: BYTE & dom({address|->value})/\dom(stack) = {} & not(address|->value: stack));
  List_Precondition(Machine(MEMORY),updateStack)==(NewValues: BV16 --> BYTE);
  List_Precondition(Machine(MEMORY),updateAddressStack)==(address: BV16 & value: BYTE);
  List_Precondition(Machine(MEMORY),pop)==(sp: BYTE)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(MEMORY),pop)==(sp: BYTE | stack:={dec_BV16(sp)}<<|stack);
  Expanded_List_Substitution(Machine(MEMORY),updateAddressStack)==(address: BV16 & value: BYTE | stack,mem:=stack<+{address|->value},mem<+{address|->value});
  Expanded_List_Substitution(Machine(MEMORY),updateStack)==(NewValues: BV16 --> BYTE | stack,mem:=stack<+NewValues,mem<+NewValues);
  Expanded_List_Substitution(Machine(MEMORY),updateAddressMem)==(address: BV16 & value: BYTE & dom({address|->value})/\dom(stack) = {} & not(address|->value: stack) | mem:=mem<+{address|->value});
  Expanded_List_Substitution(Machine(MEMORY),updateMem)==(NewValues: BV16 --> BYTE & dom(NewValues)/\dom(stack) = {} | mem:=mem<+NewValues);
  List_Substitution(Machine(MEMORY),updateMem)==(mem:=mem<+NewValues);
  List_Substitution(Machine(MEMORY),updateAddressMem)==(mem:=mem<+{address|->value});
  List_Substitution(Machine(MEMORY),updateStack)==(stack:=stack<+NewValues || mem:=mem<+NewValues);
  List_Substitution(Machine(MEMORY),updateAddressStack)==(stack(address):=value || mem:=mem<+{address|->value});
  List_Substitution(Machine(MEMORY),pop)==(stack:={dec_BV16(sp)}<<|stack)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(MEMORY))==(RESERVED_ADDRESS);
  Inherited_List_Constants(Machine(MEMORY))==(?);
  List_Constants(Machine(MEMORY))==(RESERVED_ADDRESS)
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
  Context_List_Properties(Machine(MEMORY))==(SCHAR_MAX: INTEGER & SCHAR_MIN: INTEGER & SCHAR_MAX = 2**7-1 & SCHAR_MIN = (-2)**7 & SCHAR = (-2)**7..2**7-1 & UCHAR = 0..2**8-1 & UCHAR_MAX: INTEGER & UCHAR_MIN: INTEGER & UCHAR_MAX = 2**8 & UCHAR_MIN = 0 & uchar_schar: UCHAR --> SCHAR & uchar_schar = %v1.(v1: UCHAR & v1<=SCHAR_MAX | v1) & uchar_schar = %v1.(v1: UCHAR & not(v1<=SCHAR_MAX) | v1-UCHAR_MAX+1) & schar_uchar: SCHAR --> UCHAR & schar_uchar = uchar_schar~ & SSHORTINT = (-2)**15..2**15-1 & byte_to_uchar: BYTE --> UCHAR & byte_to_uchar = %v0.(v0: BYTE | 2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & uchar_to_byte: UCHAR --> BYTE & uchar_to_byte = byte_to_uchar~ & update_refresh_reg: BYTE --> BYTE & update_refresh_reg = %v0.(v0: BYTE | uchar_to_byte(2**7*v0(7)+(2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) mod 64)) & USHORTINT = 0..2**16-1 & USHORTINT_MAX = 2**16-1 & USHORTINT_MIN = 0 & NB_INSTRUCTIONS: NATURAL & INST_SZ: NATURAL & INST_SZ = 16 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = USHORTINT & instruction_next: USHORTINT --> USHORTINT & instruction_next = %w1.(w1: USHORTINT | (w1+1) mod 65535) & instruction_jump = %(p0,e0).(p0: INSTRUCTION & e0: -126..129 | (p0+e0) mod 65535) & byte_to_schar: BYTE --> SCHAR & byte_to_schar = %v0.(v0: BYTE | (-2)**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & schar_to_byte: SCHAR --> BYTE & schar_to_byte = byte_to_schar~ & bv16_to_sshortint: BV16 --> SSHORTINT & bv16_to_sshortint = %v0.(v0: BV16 | (-2)**15*v0(15)+2**14*v0(14)+2**13*v0(13)+2**12*v0(12)+2**11*v0(11)+2**10*v0(10)+2**9*v0(9)+2**8*v0(8)+2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & sshortint_to_bv16: SSHORTINT --> BV16 & sshortint_to_bv16 = bv16_to_sshortint~ & byte_to_bv16: BYTE*BYTE --> BV16 & byte_to_bv16 = %(v1,v2).(v1: BV16 & v2: BV16 | {0|->v2(0),1|->v2(1),2|->v2(2),3|->v2(3),4|->v2(4),5|->v2(5),6|->v2(6),7|->v2(7),8|->v1(0),9|->v1(1),10|->v1(2),11|->v1(3),12|->v1(4),13|->v1(5),14|->v1(6),15|->v1(7)}) & bv16_to_byte: BV16 --> BYTE*BYTE & bv16_to_byte = byte_to_bv16~ & schar_to_sshortint: SCHAR*SCHAR --> SSHORTINT & schar_to_sshortint = %(w1,w2).(w1: SCHAR & w2: SCHAR | bv16_to_sshortint(byte_to_bv16(schar_to_byte(w1),schar_to_byte(w2)))) & sshortint_to_schar: SSHORTINT --> SCHAR*SCHAR & sshortint_to_schar = schar_to_sshortint~ & bv16_to_ushortint: BV16 --> USHORTINT & bv16_to_ushortint = %v0.(v0: BV16 | 2**15*v0(15)+2**14*v0(14)+2**13*v0(13)+2**12*v0(12)+2**11*v0(11)+2**10*v0(10)+2**9*v0(9)+2**8*v0(8)+2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & ushortint_to_bv16: USHORTINT --> BV16 & ushortint_to_bv16 = bv16_to_ushortint~ & sshortint_to_ushortint: SSHORTINT --> USHORTINT & sshortint_to_ushortint = %v0.(v0: SSHORTINT | v0-32768) & ushortint_to_sshortint: USHORTINT --> SSHORTINT & ushortint_to_sshortint = sshortint_to_ushortint~ & get_upper_digit = %by.(by: BYTE | 2**3*by(7)+2**2*by(6)+2*by(5)+by(4)) & get_lower_digit = %by.(by: BYTE | 2**3*by(3)+2**2*by(2)+2*by(1)+by(0)) & BIT = 0..1 & bit_not: BIT --> BIT & bit_not = %bb.(bb: BIT | 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = seq1(BIT) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_index: BIT_VECTOR --> POW(NATURAL) & bv_index = %v1.(v1: BIT_VECTOR | 0..bv_size(v1)-1) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: low..bv_size(bv)-1 | %i0.(i0: 1..high-low+1 | bv(i0+low))) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (1..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (1..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 1..bv_size(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_xor(v1(idx),v2(idx)))) & bv_get: BIT_VECTOR*NATURAL --> BIT & bv_get = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1(idx+1)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 & bit: BIT | v1<+{idx+1|->bit}) & BYTE_WIDTH = 8 & BYTE_INDEX = 1..BYTE_WIDTH & REAL_BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE = {bt | bt: BIT_VECTOR & bv_size(bt) = BYTE_WIDTH} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & byte_bit_get: BYTE*REAL_BYTE_INDEX --> BIT & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)) & BV16_INDX = 1..16 & REAL_BV16_INDX = 0..16-1 & BV16 = {bt | bt: BIT_VECTOR & bv_size(bt) = 16} & BV16_ZERO = BV16_INDX*{0} & bv16_byte: BV16 --> BYTE*BYTE & bv16_byte = %bv.(bv: BV16 | {0|->bv(8),1|->bv(9),2|->bv(10),3|->bv(11),4|->bv(12),5|->bv(13),6|->bv(14),7|->bv(15)},{0|->bv(0),1|->bv(1),2|->bv(2),3|->bv(3),4|->bv(4),5|->bv(5),6|->bv(6),7|->bv(7)}) & byte_bv16 = bv16_byte~ & bv16_bit_get: BYTE*BV16_INDX --> BIT & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)) & is_zero: BYTE --> BIT & is_zero = %w1.(w1: BYTE | bool_to_bit(bool(w1(0)+w1(1)+w1(2)+w1(3)+w1(4)+w1(5)+w1(6)+w1(7) = 0))) & is_zeroUSHORTINT: USHORTINT --> BIT & is_zeroUSHORTINT = %nat1.(nat1: USHORTINT | bool_to_bit(bool(nat1 = 0))) & is_negative: BYTE --> BIT & is_negative = %w1.(w1: BYTE | w1(7)) & half: UCHAR --> UCHAR & half = %ww.(ww: UCHAR | ww mod 2**4) & simple_add8UCHAR: UCHAR*UCHAR --> UCHAR & simple_add8UCHAR = %(w1,w2).(w1: UCHAR & w2: UCHAR | (w1+w2) mod 2**8) & add8UCHAR: BIT*UCHAR*UCHAR --> UCHAR*BIT*BIT*BIT*BIT & add8UCHAR = %(carry,w1,w2).(carry: BIT & w1: UCHAR & w2: UCHAR | (carry+w1+w2) mod 256,bool_to_bit(bool(carry+uchar_schar(w1)+uchar_schar(w2)<0)),bool_to_bit(bool(carry+w1+w2>UCHAR_MAX)),bool_to_bit(bool(carry+half(w1)+half(w2)>=2**4)),bool_to_bit(bool((carry+w1+w2) mod UCHAR_MAX-1 = 0))) & substract8UCHAR: BIT*UCHAR*UCHAR --> UCHAR*BIT*BIT*BIT*BIT & substract8UCHAR = %(carry,w1,w2).(carry: BIT & w1: UCHAR & w2: UCHAR | (carry+w1-w2) mod 256,bool_to_bit(bool(carry+uchar_schar(w1)-uchar_schar(w2)<0)),bool_to_bit(bool(carry+w1-w2>UCHAR_MAX)),bool_to_bit(bool(carry+half(w1)-half(w2)>=2**4)),bool_to_bit(bool((carry+w1-w2) mod UCHAR_MAX-1 = 0))) & add16USHORTINT: BIT*USHORTINT*USHORTINT --> USHORTINT & add16USHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | (b1+w1+w2) mod 65536) & add_carryUSHORTINT: BIT*USHORTINT*USHORTINT --> BIT & add_carryUSHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | bool_to_bit(bool(b1+w1+w2>2**16))) & add_halfcarryUSHORTINT: BIT*USHORTINT*USHORTINT --> BIT & add_halfcarryUSHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | bool_to_bit(bool(b1+w1 mod 2**12+w2 mod 2**12>2**12))) & sub16USHORTINT: BIT*USHORTINT*USHORTINT --> USHORTINT & sub16USHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | (w1-w2-b1) mod 65536) & sub_carryUSHORTINT: BIT*USHORTINT*USHORTINT --> BIT & sub_carryUSHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | bool_to_bit(bool(w1-w2-b1>2**16))) & sub_halfcarryUSHORTINT: BIT*USHORTINT*USHORTINT --> BIT & sub_halfcarryUSHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | bool_to_bit(bool(w1 mod 2**12-w2 mod 2**12-b1>2**12))) & inc_BYTE: BYTE --> BYTE & inc_BYTE = %w1.(w1: BYTE | uchar_to_byte((byte_to_uchar(w1)+1) mod 256)) & dec_BYTE: BYTE --> BYTE & dec_BYTE = %w1.(w1: BYTE | uchar_to_byte((byte_to_uchar(w1)-1) mod 256)) & inc_BV16: BV16 --> BV16 & inc_BV16 = %w1.(w1: BV16 | ushortint_to_bv16((bv16_to_ushortint(w1)+1) mod 65536)) & dec_BV16: BYTE --> BYTE & dec_BV16 = %w1.(w1: BV16 | ushortint_to_bv16((bv16_to_ushortint(w1)-1) mod 65536)) & parity_even_BYTE: BIT_VECTOR --> BIT & parity_even_BYTE = %bv.(bv: BIT_VECTOR | 1-(bv_get(bv,0)+bv_get(bv,1)+bv_get(bv,2)+bv_get(bv,3)+bv_get(bv,4)+bv_get(bv,5)+bv_get(bv,7)) mod 2) & and: BYTE*BYTE --> BYTE & and = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_and(bt1,bt2)) & ior: BYTE*BYTE --> BYTE & ior = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_or(bt1,bt2)) & xor: BYTE*BYTE --> BYTE & xor = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_xor(bt1,bt2)) & bitget: BYTE*BYTE_INDEX --> BIT & bitget = %(bt1,ii).(bt1: BYTE & ii: BYTE_INDEX | bt1(ii)) & bitset: BYTE*BYTE_INDEX --> BYTE & !(ww,ii).(ww: BYTE & ii: BYTE_INDEX => bitset(ww,ii) = bv_set(ww,ii)) & bitclear: BYTE*BYTE_INDEX --> BYTE & !(ww,ii,bb).(ww: BYTE & ii: BYTE_INDEX & bb: BIT => bitclear(ww,ii) = bv_clear(ww,ii)) & complement: BYTE --> BYTE & complement = %bt.(bt: BYTE | bv_not(bt)) & swap: BYTE --> BYTE & swap = %bt.(bt: BYTE | {0|->bt(4),1|->bt(5),2|->bt(6),3|->bt(7),4|->bt(0),5|->bt(1),6|->bt(2),7|->bt(3)}) & rotateleft: BYTE --> BYTE & rotateleft = %bv.(bv: BYTE | {0|->bv(7),1|->bv(0),2|->bv(1),3|->bv(2),4|->bv(3),5|->bv(4),6|->bv(5),7|->bv(6)}) & rotateright: BYTE --> BYTE & rotateright = %bv.(bv: BYTE | {0|->bv(1),1|->bv(2),2|->bv(3),3|->bv(4),4|->bv(5),5|->bv(6),6|->bv(7),7|->bv(0)}));
  Inherited_List_Properties(Machine(MEMORY))==(btrue);
  List_Properties(Machine(MEMORY))==(RESERVED_ADDRESS = 0..16384)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(ALU))==(?);
  Seen_Context_List_Enumerated(Machine(MEMORY))==(?);
  Seen_Context_List_Invariant(Machine(MEMORY))==(btrue);
  Seen_Context_List_Assertions(Machine(MEMORY))==((2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536) & BV16_ZERO: BIT_VECTOR & BV16 <: BIT_VECTOR & BV16_ZERO: BV16 & first(BYTE_ZERO) = 0 & BYTE_ZERO: BIT_VECTOR & BYTE <: BIT_VECTOR & size(BYTE_ZERO) = 8 & !bt.(bt: BYTE => size(bt) = 8) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_not(bv_not(bv)),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: 0..bv_size(bv)-1 & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,v2),indx) = bv_get(bv_and(v2,v1),indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,bv_and(v2,v3)),indx) = bv_get(bv_and(bv_and(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv_zero(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_one(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,v2),indx) = bv_get(bv_or(v2,v1),indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,bv_or(v2,v3)),indx) = bv_get(bv_or(bv_or(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_one(bv_size(bv))),indx) = bv_get(bv_one(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_xor(v1,v2),indx) = bv_get(bv_xor(v2,v1),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_xor(bv,bv),indx) = bv_get(bv_zero(bv_size(bv)),indx))) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT => bb = 0 or bb = 1) & bool_to_bit(FALSE) = 0 & bool_to_bit(TRUE) = 1 & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & bit_xor(1,1) = 0 & bit_xor(1,0) = 1 & bit_xor(0,1) = 1 & bit_xor(0,0) = 0 & !b1.(b1: BIT => bit_or(0,b1) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & bit_or(1,1) = 1 & bit_or(1,0) = 1 & bit_or(0,1) = 1 & bit_or(0,0) = 0 & !b1.(b1: BIT => bit_and(b1,0) = 0) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & bit_and(1,1) = 1 & bit_and(1,0) = 0 & bit_and(0,1) = 0 & bit_and(0,0) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_not(1) = 0 & bit_not(0) = 1 & !bb.(bb: BIT => bit_not(bb) = 1-bb) & !xx.(xx: BYTE => get_lower_digit(xx): 0..16) & !xx.(xx: BYTE => get_upper_digit(xx): 0..16) & !xx.(xx: USHORTINT => ushortint_to_bv16(xx): BV16) & !xx.(xx: BV16 => bv16_to_ushortint(xx): USHORTINT) & !(xx,yy).(xx: BYTE & yy: BYTE => #zz.(zz: BV16 & byte_to_bv16(xx,yy) = zz)) & !(xx,yy).(xx: BYTE & yy: BYTE => byte_to_bv16(xx,yy): BV16) & !xx.(xx: SCHAR => schar_to_byte(xx): BYTE) & !xx.(xx: BYTE => byte_to_schar(xx): SCHAR) & !xx.(xx: BYTE => update_refresh_reg(xx): BYTE) & !xx.(xx: UCHAR => uchar_to_byte(xx): BYTE) & !xx.(xx: BYTE => byte_to_uchar(xx): UCHAR) & instruction_next: USHORTINT --> USHORTINT & !n0.(n0: UCHAR => n0<=255) & !n0.(n0: UCHAR => 0<=n0) & 0 = schar_to_sshortint(0,0));
  Seen_Context_List_Properties(Machine(MEMORY))==(SCHAR_MAX: INTEGER & SCHAR_MIN: INTEGER & SCHAR_MAX = 2**7-1 & SCHAR_MIN = (-2)**7 & SCHAR = (-2)**7..2**7-1 & UCHAR = 0..2**8-1 & UCHAR_MAX: INTEGER & UCHAR_MIN: INTEGER & UCHAR_MAX = 2**8 & UCHAR_MIN = 0 & uchar_schar: UCHAR --> SCHAR & uchar_schar = %v1.(v1: UCHAR & v1<=SCHAR_MAX | v1) & uchar_schar = %v1.(v1: UCHAR & not(v1<=SCHAR_MAX) | v1-UCHAR_MAX+1) & schar_uchar: SCHAR --> UCHAR & schar_uchar = uchar_schar~ & SSHORTINT = (-2)**15..2**15-1 & byte_to_uchar: BYTE --> UCHAR & byte_to_uchar = %v0.(v0: BYTE | 2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & uchar_to_byte: UCHAR --> BYTE & uchar_to_byte = byte_to_uchar~ & update_refresh_reg: BYTE --> BYTE & update_refresh_reg = %v0.(v0: BYTE | uchar_to_byte(2**7*v0(7)+(2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) mod 64)) & USHORTINT = 0..2**16-1 & USHORTINT_MAX = 2**16-1 & USHORTINT_MIN = 0 & NB_INSTRUCTIONS: NATURAL & INST_SZ: NATURAL & INST_SZ = 16 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = USHORTINT & instruction_next: USHORTINT --> USHORTINT & instruction_next = %w1.(w1: USHORTINT | (w1+1) mod 65535) & instruction_jump = %(p0,e0).(p0: INSTRUCTION & e0: -126..129 | (p0+e0) mod 65535) & byte_to_schar: BYTE --> SCHAR & byte_to_schar = %v0.(v0: BYTE | (-2)**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & schar_to_byte: SCHAR --> BYTE & schar_to_byte = byte_to_schar~ & bv16_to_sshortint: BV16 --> SSHORTINT & bv16_to_sshortint = %v0.(v0: BV16 | (-2)**15*v0(15)+2**14*v0(14)+2**13*v0(13)+2**12*v0(12)+2**11*v0(11)+2**10*v0(10)+2**9*v0(9)+2**8*v0(8)+2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & sshortint_to_bv16: SSHORTINT --> BV16 & sshortint_to_bv16 = bv16_to_sshortint~ & byte_to_bv16: BYTE*BYTE --> BV16 & byte_to_bv16 = %(v1,v2).(v1: BV16 & v2: BV16 | {0|->v2(0),1|->v2(1),2|->v2(2),3|->v2(3),4|->v2(4),5|->v2(5),6|->v2(6),7|->v2(7),8|->v1(0),9|->v1(1),10|->v1(2),11|->v1(3),12|->v1(4),13|->v1(5),14|->v1(6),15|->v1(7)}) & bv16_to_byte: BV16 --> BYTE*BYTE & bv16_to_byte = byte_to_bv16~ & schar_to_sshortint: SCHAR*SCHAR --> SSHORTINT & schar_to_sshortint = %(w1,w2).(w1: SCHAR & w2: SCHAR | bv16_to_sshortint(byte_to_bv16(schar_to_byte(w1),schar_to_byte(w2)))) & sshortint_to_schar: SSHORTINT --> SCHAR*SCHAR & sshortint_to_schar = schar_to_sshortint~ & bv16_to_ushortint: BV16 --> USHORTINT & bv16_to_ushortint = %v0.(v0: BV16 | 2**15*v0(15)+2**14*v0(14)+2**13*v0(13)+2**12*v0(12)+2**11*v0(11)+2**10*v0(10)+2**9*v0(9)+2**8*v0(8)+2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & ushortint_to_bv16: USHORTINT --> BV16 & ushortint_to_bv16 = bv16_to_ushortint~ & sshortint_to_ushortint: SSHORTINT --> USHORTINT & sshortint_to_ushortint = %v0.(v0: SSHORTINT | v0-32768) & ushortint_to_sshortint: USHORTINT --> SSHORTINT & ushortint_to_sshortint = sshortint_to_ushortint~ & get_upper_digit = %by.(by: BYTE | 2**3*by(7)+2**2*by(6)+2*by(5)+by(4)) & get_lower_digit = %by.(by: BYTE | 2**3*by(3)+2**2*by(2)+2*by(1)+by(0)) & BIT = 0..1 & bit_not: BIT --> BIT & bit_not = %bb.(bb: BIT | 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = seq1(BIT) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_index: BIT_VECTOR --> POW(NATURAL) & bv_index = %v1.(v1: BIT_VECTOR | 0..bv_size(v1)-1) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: low..bv_size(bv)-1 | %i0.(i0: 1..high-low+1 | bv(i0+low))) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (1..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (1..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 1..bv_size(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_xor(v1(idx),v2(idx)))) & bv_get: BIT_VECTOR*NATURAL --> BIT & bv_get = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1(idx+1)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 & bit: BIT | v1<+{idx+1|->bit}) & BYTE_WIDTH = 8 & BYTE_INDEX = 1..BYTE_WIDTH & REAL_BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE = {bt | bt: BIT_VECTOR & bv_size(bt) = BYTE_WIDTH} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & byte_bit_get: BYTE*REAL_BYTE_INDEX --> BIT & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)) & BV16_INDX = 1..16 & REAL_BV16_INDX = 0..16-1 & BV16 = {bt | bt: BIT_VECTOR & bv_size(bt) = 16} & BV16_ZERO = BV16_INDX*{0} & bv16_byte: BV16 --> BYTE*BYTE & bv16_byte = %bv.(bv: BV16 | {0|->bv(8),1|->bv(9),2|->bv(10),3|->bv(11),4|->bv(12),5|->bv(13),6|->bv(14),7|->bv(15)},{0|->bv(0),1|->bv(1),2|->bv(2),3|->bv(3),4|->bv(4),5|->bv(5),6|->bv(6),7|->bv(7)}) & byte_bv16 = bv16_byte~ & bv16_bit_get: BYTE*BV16_INDX --> BIT & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)));
  Seen_List_Constraints(Machine(MEMORY))==(btrue);
  Seen_List_Operations(Machine(MEMORY),Machine(ALU))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(ALU))==(btrue);
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(TYPES))==(?);
  Seen_List_Operations(Machine(MEMORY),Machine(TYPES))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(TYPES))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(MEMORY),updateMem)==(?);
  List_ANY_Var(Machine(MEMORY),updateAddressMem)==(?);
  List_ANY_Var(Machine(MEMORY),updateStack)==(?);
  List_ANY_Var(Machine(MEMORY),updateAddressStack)==(?);
  List_ANY_Var(Machine(MEMORY),pop)==(?);
  List_ANY_Var(Machine(MEMORY),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(MEMORY)) == (RESERVED_ADDRESS | ? | stack,mem | ? | updateMem,updateAddressMem,updateStack,updateAddressStack,pop | ? | seen(Machine(TYPES)),seen(Machine(ALU)) | ? | MEMORY);
  List_Of_HiddenCst_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(MEMORY)) == (RESERVED_ADDRESS);
  List_Of_VisibleVar_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(MEMORY)) == (?: ?);
  List_Of_Ids(Machine(ALU)) == (is_zero,is_zeroUSHORTINT,is_negative,half,simple_add8UCHAR,add8UCHAR,substract8UCHAR,add16USHORTINT,add_carryUSHORTINT,add_halfcarryUSHORTINT,sub16USHORTINT,sub_carryUSHORTINT,sub_halfcarryUSHORTINT,inc_BYTE,dec_BYTE,inc_BV16,dec_BV16,parity_even_BYTE,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright | ? | ? | ? | ? | ? | seen(Machine(TYPES)) | ? | ALU);
  List_Of_HiddenCst_Ids(Machine(ALU)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ALU)) == (is_zero,is_zeroUSHORTINT,is_negative,half,simple_add8UCHAR,add8UCHAR,substract8UCHAR,add16USHORTINT,add_carryUSHORTINT,add_halfcarryUSHORTINT,sub16USHORTINT,sub_carryUSHORTINT,sub_halfcarryUSHORTINT,inc_BYTE,dec_BYTE,inc_BV16,dec_BV16,parity_even_BYTE,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright);
  List_Of_VisibleVar_Ids(Machine(ALU)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ALU)) == (?: ?);
  List_Of_Ids(Machine(TYPES)) == (SCHAR,SCHAR_MAX,SCHAR_MIN,byte_to_schar,schar_to_byte,UCHAR,UCHAR_MAX,UCHAR_MIN,uchar_to_byte,byte_to_uchar,update_refresh_reg,uchar_schar,schar_uchar,SSHORTINT,bv16_to_sshortint,sshortint_to_bv16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,instruction_jump,byte_to_bv16,bv16_to_byte,schar_to_sshortint,sshortint_to_schar,USHORTINT,USHORTINT_MAX,USHORTINT_MIN,bv16_to_ushortint,ushortint_to_bv16,ushortint_to_sshortint,sshortint_to_ushortint,get_upper_digit,get_lower_digit | BV16_INDX,REAL_BV16_INDX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get,BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index,BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | seen(Machine(POWER2)),included(Machine(BIT_DEFINITION)),included(Machine(BIT_VECTOR_DEFINITION)),included(Machine(BYTE_DEFINITION)),included(Machine(BIT_VECTOR16_DEFINITION)) | ? | TYPES);
  List_Of_HiddenCst_Ids(Machine(TYPES)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (SCHAR,SCHAR_MAX,SCHAR_MIN,byte_to_schar,schar_to_byte,UCHAR,UCHAR_MAX,UCHAR_MIN,uchar_to_byte,byte_to_uchar,update_refresh_reg,uchar_schar,schar_uchar,SSHORTINT,bv16_to_sshortint,sshortint_to_bv16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,instruction_jump,byte_to_bv16,bv16_to_byte,schar_to_sshortint,sshortint_to_schar,USHORTINT,USHORTINT_MAX,USHORTINT_MIN,bv16_to_ushortint,ushortint_to_bv16,ushortint_to_sshortint,sshortint_to_ushortint,get_upper_digit,get_lower_digit,BV16_INDX,REAL_BV16_INDX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get,BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index,BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit);
  List_Of_VisibleVar_Ids(Machine(TYPES)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPES)) == (seen(Machine(BIT_DEFINITION)): (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(BIT_VECTOR_DEFINITION)): (BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(BYTE_DEFINITION)): (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(BIT_VECTOR16_DEFINITION)) == (BV16_INDX,REAL_BV16_INDX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BYTE_DEFINITION)) | ? | BIT_VECTOR16_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR16_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR16_DEFINITION)) == (BV16_INDX,REAL_BV16_INDX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR16_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR16_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)) | ? | BYTE_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get);
  List_Of_VisibleVar_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BYTE_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)) | ? | BIT_VECTOR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ? | BIT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit);
  List_Of_VisibleVar_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(POWER2)) == (? | ? | ? | ? | ? | ? | seen(Machine(POWER)) | ? | POWER2);
  List_Of_HiddenCst_Ids(Machine(POWER2)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(POWER2)) == (?);
  List_Of_VisibleVar_Ids(Machine(POWER2)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(POWER2)) == (?: ?);
  List_Of_Ids(Machine(POWER)) == (? | ? | ? | ? | ? | ? | ? | ? | POWER);
  List_Of_HiddenCst_Ids(Machine(POWER)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(POWER)) == (?);
  List_Of_VisibleVar_Ids(Machine(POWER)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(POWER)) == (?: ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(MEMORY)) == (Type(RESERVED_ADDRESS) == Cst(SetOf(btype(INTEGER,"[RESERVED_ADDRESS","]RESERVED_ADDRESS"))))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(MEMORY)) == (Type(stack) == Mvl(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(mem) == Mvl(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(MEMORY)) == (Type(pop) == Cst(No_type,SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(updateAddressStack) == Cst(No_type,SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(updateStack) == Cst(No_type,SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(updateAddressMem) == Cst(No_type,SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(updateMem) == Cst(No_type,SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO;
  event_b_project == KO;
  event_b_deadlockfreeness == KO;
  variant_clause_mandatory == OK
END
)
