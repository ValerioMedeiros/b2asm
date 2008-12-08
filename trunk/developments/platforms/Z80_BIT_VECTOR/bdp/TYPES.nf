Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(TYPES))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(TYPES))==(Machine(TYPES));
  Level(Machine(TYPES))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(TYPES)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(TYPES))==(POWER2)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(TYPES))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(TYPES))==(BYTE_DEFINITION,BIT_VECTOR_DEFINITION,BIT_DEFINITION);
  List_Includes(Machine(TYPES))==(BIT_DEFINITION,BIT_VECTOR_DEFINITION,BYTE_DEFINITION)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(TYPES))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(TYPES))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(TYPES))==(?);
  Context_List_Variables(Machine(TYPES))==(?);
  Abstract_List_Variables(Machine(TYPES))==(?);
  Local_List_Variables(Machine(TYPES))==(?);
  List_Variables(Machine(TYPES))==(?);
  External_List_Variables(Machine(TYPES))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(TYPES))==(?);
  Abstract_List_VisibleVariables(Machine(TYPES))==(?);
  External_List_VisibleVariables(Machine(TYPES))==(?);
  Expanded_List_VisibleVariables(Machine(TYPES))==(?);
  List_VisibleVariables(Machine(TYPES))==(?);
  Internal_List_VisibleVariables(Machine(TYPES))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(TYPES))==(btrue);
  Gluing_List_Invariant(Machine(TYPES))==(btrue);
  Abstract_List_Invariant(Machine(TYPES))==(btrue);
  Expanded_List_Invariant(Machine(TYPES))==(btrue);
  Context_List_Invariant(Machine(TYPES))==(btrue);
  List_Invariant(Machine(TYPES))==(btrue)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Machine(TYPES))==(btrue);
  Expanded_List_Assertions(Machine(TYPES))==(bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_to_bit(TRUE) = 1 & bool_to_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_not(bv_not(bv))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_and(v1,v2)(indx) = bv_and(v2,v1)(indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_and(v1,bv_and(v2,v3))(indx) = bv_and(bv_and(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_zero(bv_size(bv)))(indx) = bv_zero(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_one(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_or(v1,v2)(indx) = bv_or(v2,v1)(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_or(v1,bv_or(v2,v3))(indx) = bv_or(bv_or(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_one(bv_size(bv)))(indx) = bv_one(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_zero(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_xor(v1,v2)(indx) = bv_xor(v2,v1)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_xor(bv,bv)(indx) = bv_zero(bv_size(bv))(indx))) & !bt.(bt: BYTE => size(bt) = 8) & size(BYTE_ZERO) = 8 & BYTE <: BIT_VECTOR & BYTE_ZERO: BIT_VECTOR & first(BYTE_ZERO) = 0);
  Context_List_Assertions(Machine(TYPES))==(2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536);
  List_Assertions(Machine(TYPES))==(NB_SCHARS = 256 & !n0.(n0: SCHAR => 0<=n0) & !n0.(n0: SCHAR => n0<=255) & 2**16 = 65536 & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & 0 = SCHAR_TO_SSHORTINT(0,0) & !xx.(xx: BYTE => BYTE_TO_UCHAR(xx): UCHAR) & !xx.(xx: UCHAR => UCHAR_TO_BYTE(xx): BYTE) & !xx.(xx: BYTE => BYTE_TO_SCHAR(xx): SCHAR) & !xx.(xx: SCHAR => SCHAR_TO_BYTE(xx): BYTE) & !(xx,yy).(xx: BYTE & yy: BYTE => BYTE_TO_BV16(xx,yy): BV16) & !(xx,yy).(xx: BYTE & yy: BYTE => #zz.(zz: BV16 & BYTE_TO_BV16(xx,yy) = zz)) & !xx.(xx: BV16 => BV16_TO_USHORTINT(xx): USHORTINT) & !xx.(xx: USHORTINT => USHORTINT_TO_BV16(xx): BV16))
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(TYPES))==(skip);
  Context_List_Initialisation(Machine(TYPES))==(skip);
  List_Initialisation(Machine(TYPES))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(TYPES))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(TYPES),Machine(BIT_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(TYPES),Machine(BIT_VECTOR_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(TYPES),Machine(BYTE_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(TYPES),Machine(POWER2))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Machine(TYPES),Machine(BYTE_DEFINITION))==(btrue);
  List_Context_Constraints(Machine(TYPES))==(btrue);
  List_Constraints(Machine(TYPES))==(btrue);
  List_Constraints(Machine(TYPES),Machine(BIT_VECTOR_DEFINITION))==(btrue);
  List_Constraints(Machine(TYPES),Machine(BIT_DEFINITION))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(TYPES))==(?);
  List_Operations(Machine(TYPES))==(?)
END
&
THEORY ListInputX END
&
THEORY ListOutputX END
&
THEORY ListHeaderX END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX END
&
THEORY ListSubstitutionX END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(TYPES))==(BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit,BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,SCHAR,SCHAR_LENGTH,SCHAR_POSITION,NB_SCHARS,BYTE_TO_SCHAR,SCHAR_TO_BYTE,UCHAR,UCHAR_TO_BYTE,BYTE_TO_UCHAR,BV16,SSHORTINT,SSHORTINT_LENGTH,SSHORTINT_POSITION,NB_SSHORTINTS,BV16_TO_SSHORTINT,SSHORTINT_TO_BV16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,INSTRUCTION_JUMP,BYTE_TO_BV16,BV16_TO_BYTE,SCHAR_TO_SSHORTINT,SSHORTINT_TO_SCHAR,parity_bit_from_BYTE,USHORTINT,BV16_TO_USHORTINT,USHORTINT_TO_BV16,USHORTINT_TO_SSHORTINT,SSHORTINT_TO_USHORTINT);
  Inherited_List_Constants(Machine(TYPES))==(BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit,BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO);
  List_Constants(Machine(TYPES))==(SCHAR,SCHAR_LENGTH,SCHAR_POSITION,NB_SCHARS,BYTE_TO_SCHAR,SCHAR_TO_BYTE,UCHAR,UCHAR_TO_BYTE,BYTE_TO_UCHAR,BV16,SSHORTINT,SSHORTINT_LENGTH,SSHORTINT_POSITION,NB_SSHORTINTS,BV16_TO_SSHORTINT,SSHORTINT_TO_BV16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,INSTRUCTION_JUMP,BYTE_TO_BV16,BV16_TO_BYTE,SCHAR_TO_SSHORTINT,SSHORTINT_TO_SCHAR,parity_bit_from_BYTE,USHORTINT,BV16_TO_USHORTINT,USHORTINT_TO_BV16,USHORTINT_TO_SSHORTINT,SSHORTINT_TO_USHORTINT)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(TYPES))==(?);
  Context_List_Defered(Machine(TYPES))==(?);
  Context_List_Sets(Machine(TYPES))==(?);
  List_Valuable_Sets(Machine(TYPES))==(?);
  Inherited_List_Enumerated(Machine(TYPES))==(?);
  Inherited_List_Defered(Machine(TYPES))==(?);
  Inherited_List_Sets(Machine(TYPES))==(?);
  List_Enumerated(Machine(TYPES))==(?);
  List_Defered(Machine(TYPES))==(?);
  List_Sets(Machine(TYPES))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(TYPES))==(?);
  Expanded_List_HiddenConstants(Machine(TYPES))==(?);
  List_HiddenConstants(Machine(TYPES))==(?);
  External_List_HiddenConstants(Machine(TYPES))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(TYPES))==(btrue);
  Context_List_Properties(Machine(TYPES))==(btrue);
  Inherited_List_Properties(Machine(TYPES))==(BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = {bt | bt: NATURAL +-> BIT}-{{}} & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & BV_INDX: BIT_VECTOR --> POW(NATURAL) & BV_INDX = %bv.(bv: BIT_VECTOR | 0..bv_size(bv)-1) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high | low..high<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (0..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (0..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: BV_INDX(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_xor(v1(idx),v2(idx)))) & bv_get: BIT_VECTOR*NATURAL --> BIT & bv_get = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1(idx)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: BV_INDX(v1) & bit: BIT | v1<+{idx|->bit}) & BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE = {bt | bt: BYTE_INDEX --> BIT & size(bt) = BYTE_WIDTH}-{{}} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0});
  List_Properties(Machine(TYPES))==(SCHAR_LENGTH: NATURAL & SCHAR_LENGTH = 8 & NB_SCHARS: NATURAL & NB_SCHARS = 2**SCHAR_LENGTH & SCHAR = -128..127 & SCHAR_POSITION = 0..SCHAR_LENGTH-1 & UCHAR = 0..255 & SSHORTINT_LENGTH: NATURAL & SSHORTINT_LENGTH = 16 & NB_SSHORTINTS: NATURAL & NB_SSHORTINTS = 2**SSHORTINT_LENGTH & SSHORTINT = -32768..32767 & SSHORTINT_POSITION = 0..SSHORTINT_LENGTH-1 & BV16 <: BIT_VECTOR & BV16 = {vv | vv: BIT_VECTOR & size(vv) = SSHORTINT_LENGTH} & BYTE_TO_UCHAR: BYTE --> UCHAR & BYTE_TO_UCHAR = %v0.(v0: BYTE | 128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & UCHAR_TO_BYTE: UCHAR --> BYTE & UCHAR_TO_BYTE = BYTE_TO_UCHAR~ & USHORTINT = 0..65535 & NB_INSTRUCTIONS: NATURAL & INST_SZ: NATURAL & INST_SZ = 16 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = USHORTINT & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & INSTRUCTION_NEXT = %w1.(w1: USHORTINT | (w1+1) mod 65535) & INSTRUCTION_JUMP = %(p0,e0).(p0: INSTRUCTION & e0: -126..129 | (p0+e0) mod 65535) & BYTE_TO_SCHAR: BYTE --> SCHAR & BYTE_TO_SCHAR = %v0.(v0: BYTE | (-128)*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SCHAR_TO_BYTE: SCHAR --> BYTE & SCHAR_TO_BYTE = BYTE_TO_SCHAR~ & BV16_TO_SSHORTINT: BV16 --> SSHORTINT & BV16_TO_SSHORTINT = %v0.(v0: BV16 | (-32768)*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SSHORTINT_TO_BV16: SSHORTINT --> BV16 & SSHORTINT_TO_BV16 = BV16_TO_SSHORTINT~ & BYTE_TO_BV16: BYTE*BYTE --> BV16 & BYTE_TO_BV16 = %(v1,v2).(v1: BV16 & v2: BV16 | {0|->v2(0),1|->v2(1),2|->v2(2),3|->v2(3),4|->v2(4),5|->v2(5),6|->v2(6),7|->v2(7),8|->v1(0),9|->v1(1),10|->v1(2),11|->v1(3),12|->v1(4),13|->v1(5),14|->v1(6),15|->v1(7)}) & BV16_TO_BYTE: BV16 --> BYTE*BYTE & BV16_TO_BYTE = BYTE_TO_BV16~ & SCHAR_TO_SSHORTINT: SCHAR*SCHAR --> SSHORTINT & SCHAR_TO_SSHORTINT = %(w1,w2).(w1: SCHAR & w2: SCHAR | BV16_TO_SSHORTINT(BYTE_TO_BV16(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2)))) & SSHORTINT_TO_SCHAR: SSHORTINT --> SCHAR*SCHAR & SSHORTINT_TO_SCHAR = SCHAR_TO_SSHORTINT~ & BV16_TO_USHORTINT: BV16 --> USHORTINT & BV16_TO_USHORTINT = %v0.(v0: BV16 | 32768*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & USHORTINT_TO_BV16: USHORTINT --> BV16 & USHORTINT_TO_BV16 = BV16_TO_USHORTINT~ & parity_bit_from_BYTE: BIT_VECTOR --> BIT & parity_bit_from_BYTE = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | bv(idx)) mod 2) & SSHORTINT_TO_USHORTINT: SSHORTINT --> USHORTINT & SSHORTINT_TO_USHORTINT = %v0.(v0: SSHORTINT | v0-32768) & USHORTINT_TO_SSHORTINT: USHORTINT --> SSHORTINT & USHORTINT_TO_SSHORTINT = SSHORTINT_TO_USHORTINT~)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(TYPES),Machine(POWER2))==(?);
  Seen_Context_List_Enumerated(Machine(TYPES))==(?);
  Seen_Context_List_Invariant(Machine(TYPES))==(btrue);
  Seen_Context_List_Assertions(Machine(TYPES))==(!(nn,pp).(nn: NATURAL1 & pp: NAT => (pp = 0 => nn**pp = 1) & (pp = 1 => nn**pp = nn) & (pp>1 => nn**pp = nn*nn**(pp-1))));
  Seen_Context_List_Properties(Machine(TYPES))==(btrue);
  Seen_List_Constraints(Machine(TYPES))==(btrue);
  Seen_List_Operations(Machine(TYPES),Machine(POWER2))==(?);
  Seen_Expanded_List_Invariant(Machine(TYPES),Machine(POWER2))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(TYPES),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(TYPES)) == (SCHAR,SCHAR_LENGTH,SCHAR_POSITION,NB_SCHARS,BYTE_TO_SCHAR,SCHAR_TO_BYTE,UCHAR,UCHAR_TO_BYTE,BYTE_TO_UCHAR,BV16,SSHORTINT,SSHORTINT_LENGTH,SSHORTINT_POSITION,NB_SSHORTINTS,BV16_TO_SSHORTINT,SSHORTINT_TO_BV16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,INSTRUCTION_JUMP,BYTE_TO_BV16,BV16_TO_BYTE,SCHAR_TO_SSHORTINT,SSHORTINT_TO_SCHAR,parity_bit_from_BYTE,USHORTINT,BV16_TO_USHORTINT,USHORTINT_TO_BV16,USHORTINT_TO_SSHORTINT,SSHORTINT_TO_USHORTINT | BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | seen(Machine(POWER2)),included(Machine(BIT_DEFINITION)),included(Machine(BIT_VECTOR_DEFINITION)),included(Machine(BYTE_DEFINITION)) | ? | TYPES);
  List_Of_HiddenCst_Ids(Machine(TYPES)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (SCHAR,SCHAR_LENGTH,SCHAR_POSITION,NB_SCHARS,BYTE_TO_SCHAR,SCHAR_TO_BYTE,UCHAR,UCHAR_TO_BYTE,BYTE_TO_UCHAR,BV16,SSHORTINT,SSHORTINT_LENGTH,SSHORTINT_POSITION,NB_SSHORTINTS,BV16_TO_SSHORTINT,SSHORTINT_TO_BV16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,INSTRUCTION_JUMP,BYTE_TO_BV16,BV16_TO_BYTE,SCHAR_TO_SSHORTINT,SSHORTINT_TO_SCHAR,parity_bit_from_BYTE,USHORTINT,BV16_TO_USHORTINT,USHORTINT_TO_BV16,USHORTINT_TO_SSHORTINT,SSHORTINT_TO_USHORTINT,BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit);
  List_Of_VisibleVar_Ids(Machine(TYPES)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPES)) == (seen(Machine(BIT_DEFINITION)): (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(BIT_VECTOR_DEFINITION)): (BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)) | ? | BYTE_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO);
  List_Of_VisibleVar_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BYTE_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)) | ? | BIT_VECTOR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put);
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
  Constants(Machine(TYPES)) == (Type(bool_to_bit) == Cst(SetOf(btype(BOOL,0,1)*btype(INTEGER,"[BIT","]BIT")));Type(bit_xor) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(bit_or) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(bit_and) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(bit_not) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")));Type(bv_put) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*btype(INTEGER,"[BIT","]BIT")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_clear) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_set) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_get) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*btype(INTEGER,"[BIT","]BIT")));Type(bv_xor) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_or) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_and) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_not) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_size) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)));Type(bv_one) == Cst(SetOf(btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_zero) == Cst(SetOf(btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_sub) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_catenate) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(BIT_VECTOR) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(BV_INDX) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?))));Type(BYTE_ZERO) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(BYTE_INDEX) == Cst(SetOf(btype(INTEGER,"[BYTE_INDEX","]BYTE_INDEX")));Type(BYTE_WIDTH) == Cst(btype(INTEGER,?,?));Type(BYTE) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(SCHAR) == Cst(SetOf(btype(INTEGER,"[SCHAR","]SCHAR")));Type(SCHAR_LENGTH) == Cst(btype(INTEGER,?,?));Type(SCHAR_POSITION) == Cst(SetOf(btype(INTEGER,"[SCHAR_POSITION","]SCHAR_POSITION")));Type(NB_SCHARS) == Cst(btype(INTEGER,?,?));Type(BYTE_TO_SCHAR) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[SCHAR","]SCHAR")));Type(SCHAR_TO_BYTE) == Cst(SetOf(btype(INTEGER,"[SCHAR","]SCHAR")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(UCHAR) == Cst(SetOf(btype(INTEGER,"[UCHAR","]UCHAR")));Type(UCHAR_TO_BYTE) == Cst(SetOf(btype(INTEGER,"[UCHAR","]UCHAR")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(BYTE_TO_UCHAR) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[UCHAR","]UCHAR")));Type(BV16) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(SSHORTINT) == Cst(SetOf(btype(INTEGER,"[SSHORTINT","]SSHORTINT")));Type(SSHORTINT_LENGTH) == Cst(btype(INTEGER,?,?));Type(SSHORTINT_POSITION) == Cst(SetOf(btype(INTEGER,"[SSHORTINT_POSITION","]SSHORTINT_POSITION")));Type(NB_SSHORTINTS) == Cst(btype(INTEGER,?,?));Type(BV16_TO_SSHORTINT) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[SSHORTINT","]SSHORTINT")));Type(SSHORTINT_TO_BV16) == Cst(SetOf(btype(INTEGER,"[SSHORTINT","]SSHORTINT")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(INST_SZ) == Cst(btype(INTEGER,?,?));Type(INSTRUCTION) == Cst(SetOf(btype(INTEGER,"[INSTRUCTION","]INSTRUCTION")));Type(NB_INSTRUCTIONS) == Cst(btype(INTEGER,?,?));Type(INSTRUCTION_MAX) == Cst(btype(INTEGER,?,?));Type(INSTRUCTION_NEXT) == Cst(SetOf(btype(INTEGER,"[USHORTINT","]USHORTINT")*btype(INTEGER,"[USHORTINT","]USHORTINT")));Type(INSTRUCTION_JUMP) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(BYTE_TO_BV16) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(BV16_TO_BYTE) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))));Type(SCHAR_TO_SSHORTINT) == Cst(SetOf(btype(INTEGER,"[SCHAR","]SCHAR")*btype(INTEGER,"[SCHAR","]SCHAR")*btype(INTEGER,"[SSHORTINT","]SSHORTINT")));Type(SSHORTINT_TO_SCHAR) == Cst(SetOf(btype(INTEGER,"[SSHORTINT","]SSHORTINT")*(btype(INTEGER,"[SCHAR","]SCHAR")*btype(INTEGER,"[SCHAR","]SCHAR"))));Type(parity_bit_from_BYTE) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BIT","]BIT")));Type(USHORTINT) == Cst(SetOf(btype(INTEGER,"[USHORTINT","]USHORTINT")));Type(BV16_TO_USHORTINT) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[USHORTINT","]USHORTINT")));Type(USHORTINT_TO_BV16) == Cst(SetOf(btype(INTEGER,"[USHORTINT","]USHORTINT")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(USHORTINT_TO_SSHORTINT) == Cst(SetOf(btype(INTEGER,"[USHORTINT","]USHORTINT")*btype(INTEGER,"[SSHORTINT","]SSHORTINT")));Type(SSHORTINT_TO_USHORTINT) == Cst(SetOf(btype(INTEGER,"[SSHORTINT","]SSHORTINT")*btype(INTEGER,"[USHORTINT","]USHORTINT"))))
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
