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
  List_Invariant(Machine(MEMORY))==(stack: USHORTINT --> SCHAR & mem: USHORTINT --> SCHAR)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(MEMORY))==(btrue);
  Abstract_List_Assertions(Machine(MEMORY))==(btrue);
  Context_List_Assertions(Machine(MEMORY))==(NB_SCHARS = 256 & !n0.(n0: SCHAR => 0<=n0) & !n0.(n0: SCHAR => n0<=255) & 2**16 = 65536 & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & 0 = SCHAR_TO_SSHORTINT(0,0) & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_to_bit(TRUE) = 1 & bool_to_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_not(bv_not(bv))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_and(v1,v2)(indx) = bv_and(v2,v1)(indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_and(v1,bv_and(v2,v3))(indx) = bv_and(bv_and(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_zero(bv_size(bv)))(indx) = bv_zero(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_one(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_or(v1,v2)(indx) = bv_or(v2,v1)(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_or(v1,bv_or(v2,v3))(indx) = bv_or(bv_or(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_one(bv_size(bv)))(indx) = bv_one(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_zero(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_xor(v1,v2)(indx) = bv_xor(v2,v1)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_xor(bv,bv)(indx) = bv_zero(bv_size(bv))(indx))) & dom(add8SCHAR) = SCHAR*SCHAR & ran(add8SCHAR): POW(SCHAR*BOOL*BOOL*BOOL*BOOL) & dom(substract8SCHAR) = SCHAR*SCHAR & ran(substract8SCHAR): POW(SCHAR*BOOL*BOOL*BOOL*BOOL) & dom(andSCHAR) = SCHAR*SCHAR & ran(andSCHAR) <: SCHAR*BOOL & dom(iorSCHAR) = SCHAR*SCHAR & ran(iorSCHAR) <: SCHAR*BOOL & dom(xorSCHAR) = SCHAR*SCHAR & ran(xorSCHAR) <: SCHAR*BOOL & dom(bitclearSCHAR) = SCHAR*SCHAR_POSITION & ran(bitclearSCHAR) <: SCHAR & dom(bitsetSCHAR) = SCHAR*SCHAR_POSITION & ran(bitsetSCHAR) <: SCHAR & dom(bitgetSCHAR) = SCHAR*SCHAR_POSITION & ran(bitgetSCHAR) <: BIT & dom(complementSCHAR) = SCHAR & ran(complementSCHAR) <: SCHAR & dom(swapSCHAR) = SCHAR & ran(swapSCHAR) <: SCHAR & ran(rotateleftSCHAR) <: SCHAR*BOOL & dom(rotateleftSCHAR) = SCHAR & dom(rotaterightSCHAR) = SCHAR & ran(rotaterightSCHAR) <: SCHAR*BOOL & !(w0,idx).(w0: SCHAR & idx: SCHAR_POSITION => bitgetSCHAR(w0,idx) = SCHAR_TO_BYTE(w0)(idx)) & max(SCHAR) = 127);
  List_Assertions(Machine(MEMORY))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(MEMORY))==(@(mem$0).(mem$0: USHORTINT --> SCHAR ==> mem:=mem$0) || @(stack$0).(stack$0: USHORTINT --> SCHAR ==> stack:=stack$0));
  Context_List_Initialisation(Machine(MEMORY))==(skip);
  List_Initialisation(Machine(MEMORY))==(mem:: USHORTINT --> SCHAR || stack:: USHORTINT --> SCHAR)
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
  List_Precondition(Machine(MEMORY),updateMem)==(NewValues: USHORTINT --> SCHAR);
  List_Precondition(Machine(MEMORY),updateAddressMem)==(address: USHORTINT & value: SCHAR);
  List_Precondition(Machine(MEMORY),updateStack)==(NewValues: USHORTINT --> SCHAR);
  List_Precondition(Machine(MEMORY),updateAddressStack)==(address: USHORTINT & value: SCHAR);
  List_Precondition(Machine(MEMORY),pop)==(sp: USHORTINT)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(MEMORY),pop)==(sp: USHORTINT | stack:={sp-1}<<|stack);
  Expanded_List_Substitution(Machine(MEMORY),updateAddressStack)==(address: USHORTINT & value: SCHAR | stack:=stack<+{address|->value});
  Expanded_List_Substitution(Machine(MEMORY),updateStack)==(NewValues: USHORTINT --> SCHAR | stack:=stack<+NewValues);
  Expanded_List_Substitution(Machine(MEMORY),updateAddressMem)==(address: USHORTINT & value: SCHAR | mem:=mem<+{address|->value});
  Expanded_List_Substitution(Machine(MEMORY),updateMem)==(NewValues: USHORTINT --> SCHAR | mem:=mem<+NewValues);
  List_Substitution(Machine(MEMORY),updateMem)==(mem:=mem<+NewValues);
  List_Substitution(Machine(MEMORY),updateAddressMem)==(mem(address):=value);
  List_Substitution(Machine(MEMORY),updateStack)==(stack:=stack<+NewValues);
  List_Substitution(Machine(MEMORY),updateAddressStack)==(stack(address):=value);
  List_Substitution(Machine(MEMORY),pop)==(stack:={sp-1}<<|stack)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(MEMORY))==(?);
  Inherited_List_Constants(Machine(MEMORY))==(?);
  List_Constants(Machine(MEMORY))==(?)
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
  Context_List_Properties(Machine(MEMORY))==(SCHAR_LENGTH: NATURAL & SCHAR_LENGTH = 8 & NB_SCHARS: NATURAL & NB_SCHARS = 2**SCHAR_LENGTH & SCHAR = -128..127 & SCHAR_POSITION = 0..SCHAR_LENGTH-1 & UCHAR = 0..255 & SSHORTINT_LENGTH: NATURAL & SSHORTINT_LENGTH = 16 & NB_SSHORTINTS: NATURAL & NB_SSHORTINTS = 2**SSHORTINT_LENGTH & SSHORTINT = -32768..32767 & SSHORTINT_POSITION = 0..SSHORTINT_LENGTH-1 & BV16 <: BIT_VECTOR & BV16 = {vv | vv: BIT_VECTOR & size(vv) = SSHORTINT_LENGTH} & BYTE_TO_UCHAR: BYTE --> UCHAR & BYTE_TO_UCHAR = %v0.(v0: BYTE | (-128)*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & UCHAR_TO_BYTE: UCHAR --> BYTE & UCHAR_TO_BYTE = BYTE_TO_UCHAR~ & USHORTINT = 0..65535 & NB_INSTRUCTIONS: NATURAL & INST_SZ: NATURAL & INST_SZ = 16 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = USHORTINT & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & INSTRUCTION_NEXT = {p0,q0 | p0: INSTRUCTION & q0: INSTRUCTION & 0<=p0 & p0<NB_INSTRUCTIONS-1 & q0 = p0+1}\/{NB_INSTRUCTIONS-1|->0} & INSTRUCTION_JUMP = %(p0,e0).(p0: INSTRUCTION & e0: -126..129 | p0+e0) & BYTE_TO_SCHAR: BYTE --> SCHAR & BYTE_TO_SCHAR = %v0.(v0: BYTE | (-128)*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SCHAR_TO_BYTE: SCHAR --> BYTE & SCHAR_TO_BYTE = BYTE_TO_SCHAR~ & BV16_TO_SSHORTINT: BV16 --> SSHORTINT & BV16_TO_SSHORTINT = %v0.(v0: BV16 | (-32768)*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SSHORTINT_TO_BV16: SSHORTINT --> BV16 & SSHORTINT_TO_BV16 = BV16_TO_SSHORTINT~ & BYTE_TO_BV16: BYTE*BYTE --> BV16 & BYTE_TO_BV16 = %(v1,v2).(v1: BV16 & v2: BV16 | {0|->v2(0),1|->v2(1),2|->v2(2),3|->v2(3),4|->v2(4),5|->v2(5),6|->v2(6),7|->v2(7),8|->v1(0),9|->v1(1),10|->v1(2),11|->v1(3),12|->v1(4),13|->v1(5),14|->v1(6),15|->v1(7)}) & BV16_TO_BYTE: BV16 --> BYTE*BYTE & BV16_TO_BYTE = BYTE_TO_BV16~ & SCHAR_TO_SSHORTINT: SCHAR*SCHAR --> SSHORTINT & SCHAR_TO_SSHORTINT = %(w1,w2).(w1: SCHAR & w2: SCHAR | BV16_TO_SSHORTINT(BYTE_TO_BV16(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2)))) & SSHORTINT_TO_SCHAR: SSHORTINT --> SCHAR*SCHAR & SSHORTINT_TO_SCHAR = SCHAR_TO_SSHORTINT~ & BV16_TO_USHORTINT: BV16 --> USHORTINT & BV16_TO_USHORTINT = %v0.(v0: BV16 | (-32768)*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & USHORTINT_TO_BV16: USHORTINT --> BV16 & USHORTINT_TO_BV16 = BV16_TO_USHORTINT~ & parity_bit_from_BYTE: BIT_VECTOR --> BIT & parity_bit_from_BYTE = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | bv(idx)) mod 2) & SSHORTINT_TO_USHORTINT: SSHORTINT --> USHORTINT & SSHORTINT_TO_USHORTINT = %v0.(v0: SSHORTINT | v0-32768) & USHORTINT_TO_SSHORTINT: USHORTINT --> SSHORTINT & USHORTINT_TO_SSHORTINT = SSHORTINT_TO_USHORTINT~ & BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = {bt | bt: NATURAL +-> BIT}-{{}} & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & BV_INDX: BIT_VECTOR --> POW(NATURAL) & BV_INDX = %bv.(bv: BIT_VECTOR | 0..bv_size(bv)-1) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high | low..high<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (0..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (0..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: BV_INDX(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1(idx)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: BV_INDX(v1) & bit: BIT | v1<+{idx|->bit}) & BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE <: BIT_VECTOR & BYTE = {vv | vv: BIT_VECTOR & bv_size(vv) = BYTE_WIDTH} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & is_zeroSCHAR: SCHAR --> BIT & is_zeroSCHAR = %w1.(w1: SCHAR | bool_to_bit(bool(w1 = 0))) & is_negative: SCHAR --> BIT & is_negative = %w1.(w1: SCHAR | bool_to_bit(bool(w1<0))) & parity_bit_from_BYTE: BIT_VECTOR --> BIT & parity_bit_from_BYTE = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | bv(idx)) mod 2) & halfSCHAR: SCHAR --> SCHAR & halfSCHAR = %ww.(ww: SCHAR | ww mod 16) & add8SCHAR: SCHAR*SCHAR --> SCHAR*BOOL*BOOL*BOOL*BOOL & add8SCHAR = %(w1,w2).(w1: UCHAR & w2: UCHAR & w1+w2< -128 | 256-(w1+w2),bool(w1+w2<0),TRUE,bool(halfSCHAR(w1)+halfSCHAR(w2)>=16),FALSE)\/%(w1,w2).(w1: UCHAR & w2: UCHAR & not(w1+w2< -128) | (w1+w2) mod 128,bool(w1+w2<0),bool(w1+w2>127),bool(halfSCHAR(w1)+halfSCHAR(w2)>=16),bool(w1 = 0)) & substract8SCHAR: SCHAR*SCHAR --> SCHAR*BOOL*BOOL*BOOL*BOOL & substract8SCHAR = %(w1,w2).(w1: UCHAR & w2: UCHAR & w1-w2< -128 | 256-(w1-w2),bool(w1-w2<0),TRUE,bool(halfSCHAR(w1)-halfSCHAR(w2)>=16),FALSE)\/%(w1,w2).(w1: UCHAR & w2: UCHAR & not(w1-w2< -128) | (w1-w2) mod 128,bool(w1-w2<0),bool(w1-w2>127),bool(halfSCHAR(w1)-halfSCHAR(w2)>=16),bool(w1 = 0)) & add16SCHAR: SSHORTINT*SSHORTINT --> SSHORTINT*BOOL*BOOL & !(w1,w2,sum).(w1: SSHORTINT & w2: SSHORTINT & sum: NATURAL & sum = w1+w2 => (sum<=65535 => add16SCHAR(w1,w2) = sum,bool(sum = 0),FALSE) & (65536<=sum => add16SCHAR(w1,w2) = sum-65536,bool(sum = 65536),TRUE)) & andSCHAR: SCHAR*SCHAR --> SCHAR*BOOL & !(w1,w2,w0).(w1: SCHAR & w2: SCHAR & w0: SCHAR & w0 = BYTE_TO_SCHAR(bv_and(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2))) => andSCHAR(w1,w2) = w0,bool(w0 = 0)) & iorSCHAR: SCHAR*SCHAR --> SCHAR*BOOL & !(w1,w2,w0).(w1: SCHAR & w2: SCHAR & w0: SCHAR & w0 = BYTE_TO_SCHAR(bv_or(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2))) => iorSCHAR(w1,w2) = w0,bool(w0 = 0)) & xorSCHAR: SCHAR*SCHAR --> SCHAR*BOOL & !(w1,w2,w0).(w1: SCHAR & w2: SCHAR & w0: SCHAR & w0 = BYTE_TO_SCHAR(bv_xor(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2))) => xorSCHAR(w1,w2) = w0,bool(w0 = 0)) & bitgetSCHAR: SCHAR*SCHAR_POSITION --> BIT & bitgetSCHAR = %(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & i0 = 7 | 1-(w0+2**7)/128 mod 2)\/%(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & i0/=7 | (w0+2**7)/2**i0 mod 2) & bitsetSCHAR: SCHAR*SCHAR_POSITION --> SCHAR & bitsetSCHAR = %(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & bitgetSCHAR(w0,i0) = 1 | w0)\/%(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & bitgetSCHAR(w0,i0)/=1 | w0+2**i0) & bitclearSCHAR: SCHAR*SCHAR_POSITION --> SCHAR & bitclearSCHAR = %(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & bitgetSCHAR(w0,i0) = 0 | w0)\/%(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & bitgetSCHAR(w0,i0)/=0 & i0 = 7 | w0+128)\/%(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & bitgetSCHAR(w0,i0)/=0 & i0/=7 | w0-2**i0) & complementSCHAR: SCHAR --> SCHAR & complementSCHAR = %w0.(w0: SCHAR | 255-w0) & swapSCHAR: SCHAR --> SCHAR & !(w0,v0).(w0: SCHAR & v0: BIT_VECTOR => (v0 = SCHAR_TO_BYTE(w0) => swapSCHAR(w0) = BYTE_TO_SCHAR({0|->v0(4),1|->v0(5),2|->v0(6),3|->v0(7),4|->v0(0),5|->v0(1),6|->v0(2),7|->v0(3)}))) & rotateleftSCHAR: SCHAR --> SCHAR*BOOL & !(w0,v0).(w0: SCHAR & v0: BIT_VECTOR => (v0 = SCHAR_TO_BYTE(w0) => rotateleftSCHAR(w0) = BYTE_TO_SCHAR({0|->v0(7),1|->v0(0),2|->v0(1),3|->v0(2),4|->v0(3),5|->v0(4),6|->v0(5),7|->v0(6)}),bool(v0(7) = 1))) & rotaterightSCHAR: SCHAR --> SCHAR*BOOL & !(w0,v0).(w0: SCHAR & v0: BIT_VECTOR => (v0 = SCHAR_TO_BYTE(w0) => rotaterightSCHAR(w0) = BYTE_TO_SCHAR({0|->v0(1),1|->v0(2),2|->v0(3),3|->v0(4),4|->v0(5),5|->v0(6),6|->v0(7),7|->v0(0)}),bool(v0(0) = 1))));
  Inherited_List_Properties(Machine(MEMORY))==(btrue);
  List_Properties(Machine(MEMORY))==(btrue)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(ALU))==(?);
  Seen_Context_List_Enumerated(Machine(MEMORY))==(?);
  Seen_Context_List_Invariant(Machine(MEMORY))==(btrue);
  Seen_Context_List_Assertions(Machine(MEMORY))==((2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_not(bv_not(bv))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_and(v1,v2)(indx) = bv_and(v2,v1)(indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_and(v1,bv_and(v2,v3))(indx) = bv_and(bv_and(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_zero(bv_size(bv)))(indx) = bv_zero(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_one(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_or(v1,v2)(indx) = bv_or(v2,v1)(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_or(v1,bv_or(v2,v3))(indx) = bv_or(bv_or(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_one(bv_size(bv)))(indx) = bv_one(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_zero(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_xor(v1,v2)(indx) = bv_xor(v2,v1)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_xor(bv,bv)(indx) = bv_zero(bv_size(bv))(indx))) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT => bb = 0 or bb = 1) & bool_to_bit(FALSE) = 0 & bool_to_bit(TRUE) = 1 & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & bit_xor(1,1) = 0 & bit_xor(1,0) = 1 & bit_xor(0,1) = 1 & bit_xor(0,0) = 0 & !b1.(b1: BIT => bit_or(0,b1) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & bit_or(1,1) = 1 & bit_or(1,0) = 1 & bit_or(0,1) = 1 & bit_or(0,0) = 0 & !b1.(b1: BIT => bit_and(b1,0) = 0) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & bit_and(1,1) = 1 & bit_and(1,0) = 0 & bit_and(0,1) = 0 & bit_and(0,0) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_not(1) = 0 & bit_not(0) = 1 & 0 = SCHAR_TO_SSHORTINT(0,0) & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & 2**16 = 65536 & !n0.(n0: SCHAR => n0<=255) & !n0.(n0: SCHAR => 0<=n0) & NB_SCHARS = 256);
  Seen_Context_List_Properties(Machine(MEMORY))==(SCHAR_LENGTH: NATURAL & SCHAR_LENGTH = 8 & NB_SCHARS: NATURAL & NB_SCHARS = 2**SCHAR_LENGTH & SCHAR = -128..127 & SCHAR_POSITION = 0..SCHAR_LENGTH-1 & UCHAR = 0..255 & SSHORTINT_LENGTH: NATURAL & SSHORTINT_LENGTH = 16 & NB_SSHORTINTS: NATURAL & NB_SSHORTINTS = 2**SSHORTINT_LENGTH & SSHORTINT = -32768..32767 & SSHORTINT_POSITION = 0..SSHORTINT_LENGTH-1 & BV16 <: BIT_VECTOR & BV16 = {vv | vv: BIT_VECTOR & size(vv) = SSHORTINT_LENGTH} & BYTE_TO_UCHAR: BYTE --> UCHAR & BYTE_TO_UCHAR = %v0.(v0: BYTE | (-128)*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & UCHAR_TO_BYTE: UCHAR --> BYTE & UCHAR_TO_BYTE = BYTE_TO_UCHAR~ & USHORTINT = 0..65535 & NB_INSTRUCTIONS: NATURAL & INST_SZ: NATURAL & INST_SZ = 16 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = USHORTINT & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & INSTRUCTION_NEXT = {p0,q0 | p0: INSTRUCTION & q0: INSTRUCTION & 0<=p0 & p0<NB_INSTRUCTIONS-1 & q0 = p0+1}\/{NB_INSTRUCTIONS-1|->0} & INSTRUCTION_JUMP = %(p0,e0).(p0: INSTRUCTION & e0: -126..129 | p0+e0) & BYTE_TO_SCHAR: BYTE --> SCHAR & BYTE_TO_SCHAR = %v0.(v0: BYTE | (-128)*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SCHAR_TO_BYTE: SCHAR --> BYTE & SCHAR_TO_BYTE = BYTE_TO_SCHAR~ & BV16_TO_SSHORTINT: BV16 --> SSHORTINT & BV16_TO_SSHORTINT = %v0.(v0: BV16 | (-32768)*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SSHORTINT_TO_BV16: SSHORTINT --> BV16 & SSHORTINT_TO_BV16 = BV16_TO_SSHORTINT~ & BYTE_TO_BV16: BYTE*BYTE --> BV16 & BYTE_TO_BV16 = %(v1,v2).(v1: BV16 & v2: BV16 | {0|->v2(0),1|->v2(1),2|->v2(2),3|->v2(3),4|->v2(4),5|->v2(5),6|->v2(6),7|->v2(7),8|->v1(0),9|->v1(1),10|->v1(2),11|->v1(3),12|->v1(4),13|->v1(5),14|->v1(6),15|->v1(7)}) & BV16_TO_BYTE: BV16 --> BYTE*BYTE & BV16_TO_BYTE = BYTE_TO_BV16~ & SCHAR_TO_SSHORTINT: SCHAR*SCHAR --> SSHORTINT & SCHAR_TO_SSHORTINT = %(w1,w2).(w1: SCHAR & w2: SCHAR | BV16_TO_SSHORTINT(BYTE_TO_BV16(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2)))) & SSHORTINT_TO_SCHAR: SSHORTINT --> SCHAR*SCHAR & SSHORTINT_TO_SCHAR = SCHAR_TO_SSHORTINT~ & BV16_TO_USHORTINT: BV16 --> USHORTINT & BV16_TO_USHORTINT = %v0.(v0: BV16 | (-32768)*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & USHORTINT_TO_BV16: USHORTINT --> BV16 & USHORTINT_TO_BV16 = BV16_TO_USHORTINT~ & parity_bit_from_BYTE: BIT_VECTOR --> BIT & parity_bit_from_BYTE = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | bv(idx)) mod 2) & SSHORTINT_TO_USHORTINT: SSHORTINT --> USHORTINT & SSHORTINT_TO_USHORTINT = %v0.(v0: SSHORTINT | v0-32768) & USHORTINT_TO_SSHORTINT: USHORTINT --> SSHORTINT & USHORTINT_TO_SSHORTINT = SSHORTINT_TO_USHORTINT~ & BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = {bt | bt: NATURAL +-> BIT}-{{}} & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & BV_INDX: BIT_VECTOR --> POW(NATURAL) & BV_INDX = %bv.(bv: BIT_VECTOR | 0..bv_size(bv)-1) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high | low..high<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (0..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (0..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: BV_INDX(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1(idx)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: BV_INDX(v1) & bit: BIT | v1<+{idx|->bit}) & BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE <: BIT_VECTOR & BYTE = {vv | vv: BIT_VECTOR & bv_size(vv) = BYTE_WIDTH} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0});
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
  List_Of_Ids(Machine(MEMORY)) == (? | ? | stack,mem | ? | updateMem,updateAddressMem,updateStack,updateAddressStack,pop | ? | seen(Machine(TYPES)),seen(Machine(ALU)) | ? | MEMORY);
  List_Of_HiddenCst_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(MEMORY)) == (?);
  List_Of_VisibleVar_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(MEMORY)) == (?: ?);
  List_Of_Ids(Machine(ALU)) == (add16SCHAR,halfSCHAR,add8SCHAR,substract8SCHAR,andSCHAR,iorSCHAR,xorSCHAR,bitclearSCHAR,bitsetSCHAR,bitgetSCHAR,complementSCHAR,swapSCHAR,rotateleftSCHAR,rotaterightSCHAR,is_zeroSCHAR,is_negative | ? | ? | ? | ? | ? | seen(Machine(TYPES)) | ? | ALU);
  List_Of_HiddenCst_Ids(Machine(ALU)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ALU)) == (add16SCHAR,halfSCHAR,add8SCHAR,substract8SCHAR,andSCHAR,iorSCHAR,xorSCHAR,bitclearSCHAR,bitsetSCHAR,bitgetSCHAR,complementSCHAR,swapSCHAR,rotateleftSCHAR,rotaterightSCHAR,is_zeroSCHAR,is_negative);
  List_Of_VisibleVar_Ids(Machine(ALU)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ALU)) == (?: ?);
  List_Of_Ids(Machine(TYPES)) == (SCHAR,SCHAR_LENGTH,SCHAR_POSITION,NB_SCHARS,BYTE_TO_SCHAR,SCHAR_TO_BYTE,UCHAR,UCHAR_TO_BYTE,BYTE_TO_UCHAR,BV16,SSHORTINT,SSHORTINT_LENGTH,SSHORTINT_POSITION,NB_SSHORTINTS,BV16_TO_SSHORTINT,SSHORTINT_TO_BV16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,INSTRUCTION_JUMP,BYTE_TO_BV16,BV16_TO_BYTE,SCHAR_TO_SSHORTINT,SSHORTINT_TO_SCHAR,parity_bit_from_BYTE,USHORTINT,BV16_TO_USHORTINT,USHORTINT_TO_BV16,USHORTINT_TO_SSHORTINT,SSHORTINT_TO_USHORTINT | BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put,BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | seen(Machine(POWER2)),included(Machine(BIT_DEFINITION)),included(Machine(BIT_VECTOR_DEFINITION)),included(Machine(BYTE_DEFINITION)) | ? | TYPES);
  List_Of_HiddenCst_Ids(Machine(TYPES)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (SCHAR,SCHAR_LENGTH,SCHAR_POSITION,NB_SCHARS,BYTE_TO_SCHAR,SCHAR_TO_BYTE,UCHAR,UCHAR_TO_BYTE,BYTE_TO_UCHAR,BV16,SSHORTINT,SSHORTINT_LENGTH,SSHORTINT_POSITION,NB_SSHORTINTS,BV16_TO_SSHORTINT,SSHORTINT_TO_BV16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,INSTRUCTION_JUMP,BYTE_TO_BV16,BV16_TO_BYTE,SCHAR_TO_SSHORTINT,SSHORTINT_TO_SCHAR,parity_bit_from_BYTE,USHORTINT,BV16_TO_USHORTINT,USHORTINT_TO_BV16,USHORTINT_TO_SSHORTINT,SSHORTINT_TO_USHORTINT,BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put,BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit);
  List_Of_VisibleVar_Ids(Machine(TYPES)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPES)) == (seen(Machine(BIT_DEFINITION)): (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(BIT_VECTOR_DEFINITION)): (BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)) | ? | BYTE_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO);
  List_Of_VisibleVar_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BYTE_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)) | ? | BIT_VECTOR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put);
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
THEORY VariablesEnvX IS
  Variables(Machine(MEMORY)) == (Type(stack) == Mvl(SetOf(btype(INTEGER,"[USHORTINT","]USHORTINT")*btype(INTEGER,"[SCHAR","]SCHAR")));Type(mem) == Mvl(SetOf(btype(INTEGER,"[USHORTINT","]USHORTINT")*btype(INTEGER,"[SCHAR","]SCHAR"))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(MEMORY)) == (Type(pop) == Cst(No_type,btype(INTEGER,?,?));Type(updateAddressStack) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(updateStack) == Cst(No_type,SetOf(btype(INTEGER,"[USHORTINT","]USHORTINT")*btype(INTEGER,"[SCHAR","]SCHAR")));Type(updateAddressMem) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(updateMem) == Cst(No_type,SetOf(btype(INTEGER,"[USHORTINT","]USHORTINT")*btype(INTEGER,"[SCHAR","]SCHAR"))))
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
