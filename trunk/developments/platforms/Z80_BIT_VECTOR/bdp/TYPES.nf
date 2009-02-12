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
  Inherited_List_Includes(Machine(TYPES))==(BIT_VECTOR16_DEFINITION,BYTE_DEFINITION,BIT_VECTOR_DEFINITION,BIT_DEFINITION);
  List_Includes(Machine(TYPES))==(BIT_DEFINITION,BIT_VECTOR_DEFINITION,BYTE_DEFINITION,BIT_VECTOR16_DEFINITION)
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
  Expanded_List_Assertions(Machine(TYPES))==(!bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_to_bit(TRUE) = 1 & bool_to_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_not(bv_not(bv)),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: 0..bv_size(bv)-1 & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,v2),indx) = bv_get(bv_and(v2,v1),indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,bv_and(v2,v3)),indx) = bv_get(bv_and(bv_and(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv_zero(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_one(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,v2),indx) = bv_get(bv_or(v2,v1),indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,bv_or(v2,v3)),indx) = bv_get(bv_or(bv_or(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_one(bv_size(bv))),indx) = bv_get(bv_one(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_xor(v1,v2),indx) = bv_get(bv_xor(v2,v1),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_xor(bv,bv),indx) = bv_get(bv_zero(bv_size(bv)),indx))) & !bt.(bt: BYTE => size(bt) = 8) & size(BYTE_ZERO) = 8 & BYTE <: BIT_VECTOR & BYTE_ZERO: BIT_VECTOR & first(BYTE_ZERO) = 0 & BV16_ZERO: BV16 & BV16 <: BIT_VECTOR & BV16_ZERO: BIT_VECTOR);
  Context_List_Assertions(Machine(TYPES))==(2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536);
  List_Assertions(Machine(TYPES))==(0 = schar_to_sshortint(0,0) & !n0.(n0: UCHAR => 0<=n0) & !n0.(n0: UCHAR => n0<=255) & instruction_next: USHORTINT --> USHORTINT & !xx.(xx: BYTE => byte_to_uchar(xx): UCHAR) & !xx.(xx: UCHAR => uchar_to_byte(xx): BYTE) & !xx.(xx: BYTE => update_refresh_reg(xx): BYTE) & !xx.(xx: BYTE => byte_to_schar(xx): SCHAR) & !xx.(xx: SCHAR => schar_to_byte(xx): BYTE) & !(xx,yy).(xx: BYTE & yy: BYTE => byte_to_bv16(xx,yy): BV16) & !(xx,yy).(xx: BYTE & yy: BYTE => #zz.(zz: BV16 & byte_to_bv16(xx,yy) = zz)) & !xx.(xx: BV16 => bv16_to_ushortint(xx): USHORTINT) & !xx.(xx: USHORTINT => ushortint_to_bv16(xx): BV16) & !xx.(xx: BYTE => get_upper_digit(xx): 0..16) & !xx.(xx: BYTE => get_lower_digit(xx): 0..16))
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
  List_Instanciated_Parameters(Machine(TYPES),Machine(BIT_VECTOR16_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(TYPES),Machine(POWER2))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Machine(TYPES),Machine(BIT_VECTOR16_DEFINITION))==(btrue);
  List_Context_Constraints(Machine(TYPES))==(btrue);
  List_Constraints(Machine(TYPES))==(btrue);
  List_Constraints(Machine(TYPES),Machine(BYTE_DEFINITION))==(btrue);
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
  List_Valuable_Constants(Machine(TYPES))==(BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index,BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get,BV16_INDX,REAL_BV16_INDX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get,SCHAR,SCHAR_MAX,SCHAR_MIN,byte_to_schar,schar_to_byte,UCHAR,UCHAR_MAX,UCHAR_MIN,uchar_to_byte,byte_to_uchar,update_refresh_reg,uchar_schar,schar_uchar,SSHORTINT,bv16_to_sshortint,sshortint_to_bv16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,instruction_jump,byte_to_bv16,bv16_to_byte,schar_to_sshortint,sshortint_to_schar,USHORTINT,USHORTINT_MAX,USHORTINT_MIN,bv16_to_ushortint,ushortint_to_bv16,ushortint_to_sshortint,sshortint_to_ushortint,get_upper_digit,get_lower_digit);
  Inherited_List_Constants(Machine(TYPES))==(BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index,BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get,BV16_INDX,REAL_BV16_INDX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get);
  List_Constants(Machine(TYPES))==(SCHAR,SCHAR_MAX,SCHAR_MIN,byte_to_schar,schar_to_byte,UCHAR,UCHAR_MAX,UCHAR_MIN,uchar_to_byte,byte_to_uchar,update_refresh_reg,uchar_schar,schar_uchar,SSHORTINT,bv16_to_sshortint,sshortint_to_bv16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,instruction_jump,byte_to_bv16,bv16_to_byte,schar_to_sshortint,sshortint_to_schar,USHORTINT,USHORTINT_MAX,USHORTINT_MIN,bv16_to_ushortint,ushortint_to_bv16,ushortint_to_sshortint,sshortint_to_ushortint,get_upper_digit,get_lower_digit)
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
  Inherited_List_Properties(Machine(TYPES))==(BIT = 0..1 & bit_not: BIT --> BIT & bit_not = %bb.(bb: BIT | 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = seq1(BIT) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_index: BIT_VECTOR --> POW(NATURAL) & bv_index = %v1.(v1: BIT_VECTOR | 0..bv_size(v1)-1) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: low..bv_size(bv)-1 | %i0.(i0: 1..high-low+1 | bv(i0+low))) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (1..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (1..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 1..bv_size(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_xor(v1(idx),v2(idx)))) & bv_get: BIT_VECTOR*NATURAL --> BIT & bv_get = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1(idx+1)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 & bit: BIT | v1<+{idx+1|->bit}) & BYTE_WIDTH = 8 & BYTE_INDEX = 1..BYTE_WIDTH & REAL_BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE = {bt | bt: BIT_VECTOR & bv_size(bt) = BYTE_WIDTH} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & byte_bit_get: BYTE*REAL_BYTE_INDEX --> BIT & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)) & BV16_INDX = 1..16 & REAL_BV16_INDX = 0..16-1 & BV16 = {bt | bt: BIT_VECTOR & bv_size(bt) = 16} & BV16_ZERO = BV16_INDX*{0} & bv16_byte: BV16 --> BYTE*BYTE & bv16_byte = %bv.(bv: BV16 | {0|->bv(8),1|->bv(9),2|->bv(10),3|->bv(11),4|->bv(12),5|->bv(13),6|->bv(14),7|->bv(15)},{0|->bv(0),1|->bv(1),2|->bv(2),3|->bv(3),4|->bv(4),5|->bv(5),6|->bv(6),7|->bv(7)}) & byte_bv16 = bv16_byte~ & bv16_bit_get: BYTE*BV16_INDX --> BIT & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)));
  List_Properties(Machine(TYPES))==(SCHAR_MAX: INTEGER & SCHAR_MIN: INTEGER & SCHAR_MAX = 2**7-1 & SCHAR_MIN = (-2)**7 & SCHAR = (-2)**7..2**7-1 & UCHAR = 0..2**8-1 & UCHAR_MAX: INTEGER & UCHAR_MIN: INTEGER & UCHAR_MAX = 2**8 & UCHAR_MIN = 0 & uchar_schar: UCHAR --> SCHAR & uchar_schar = %v1.(v1: UCHAR & v1<=SCHAR_MAX | v1) & uchar_schar = %v1.(v1: UCHAR & not(v1<=SCHAR_MAX) | v1-UCHAR_MAX+1) & schar_uchar: SCHAR --> UCHAR & schar_uchar = uchar_schar~ & SSHORTINT = (-2)**15..2**15-1 & byte_to_uchar: BYTE --> UCHAR & byte_to_uchar = %v0.(v0: BYTE | 2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & uchar_to_byte: UCHAR --> BYTE & uchar_to_byte = byte_to_uchar~ & update_refresh_reg: BYTE --> BYTE & update_refresh_reg = %v0.(v0: BYTE | uchar_to_byte(2**7*v0(7)+(2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) mod 64)) & USHORTINT = 0..2**16-1 & USHORTINT_MAX = 2**16-1 & USHORTINT_MIN = 0 & NB_INSTRUCTIONS: NATURAL & INST_SZ: NATURAL & INST_SZ = 16 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = USHORTINT & instruction_next: USHORTINT --> USHORTINT & instruction_next = %w1.(w1: USHORTINT | (w1+1) mod 65535) & instruction_jump = %(p0,e0).(p0: INSTRUCTION & e0: -126..129 | (p0+e0) mod 65535) & byte_to_schar: BYTE --> SCHAR & byte_to_schar = %v0.(v0: BYTE | (-2)**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & schar_to_byte: SCHAR --> BYTE & schar_to_byte = byte_to_schar~ & bv16_to_sshortint: BV16 --> SSHORTINT & bv16_to_sshortint = %v0.(v0: BV16 | (-2)**15*v0(15)+2**14*v0(14)+2**13*v0(13)+2**12*v0(12)+2**11*v0(11)+2**10*v0(10)+2**9*v0(9)+2**8*v0(8)+2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & sshortint_to_bv16: SSHORTINT --> BV16 & sshortint_to_bv16 = bv16_to_sshortint~ & byte_to_bv16: BYTE*BYTE --> BV16 & byte_to_bv16 = %(v1,v2).(v1: BV16 & v2: BV16 | {0|->v2(0),1|->v2(1),2|->v2(2),3|->v2(3),4|->v2(4),5|->v2(5),6|->v2(6),7|->v2(7),8|->v1(0),9|->v1(1),10|->v1(2),11|->v1(3),12|->v1(4),13|->v1(5),14|->v1(6),15|->v1(7)}) & bv16_to_byte: BV16 --> BYTE*BYTE & bv16_to_byte = byte_to_bv16~ & schar_to_sshortint: SCHAR*SCHAR --> SSHORTINT & schar_to_sshortint = %(w1,w2).(w1: SCHAR & w2: SCHAR | bv16_to_sshortint(byte_to_bv16(schar_to_byte(w1),schar_to_byte(w2)))) & sshortint_to_schar: SSHORTINT --> SCHAR*SCHAR & sshortint_to_schar = schar_to_sshortint~ & bv16_to_ushortint: BV16 --> USHORTINT & bv16_to_ushortint = %v0.(v0: BV16 | 2**15*v0(15)+2**14*v0(14)+2**13*v0(13)+2**12*v0(12)+2**11*v0(11)+2**10*v0(10)+2**9*v0(9)+2**8*v0(8)+2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & ushortint_to_bv16: USHORTINT --> BV16 & ushortint_to_bv16 = bv16_to_ushortint~ & sshortint_to_ushortint: SSHORTINT --> USHORTINT & sshortint_to_ushortint = %v0.(v0: SSHORTINT | v0-32768) & ushortint_to_sshortint: USHORTINT --> SSHORTINT & ushortint_to_sshortint = sshortint_to_ushortint~ & get_upper_digit = %by.(by: BYTE | 2**3*by(7)+2**2*by(6)+2*by(5)+by(4)) & get_lower_digit = %by.(by: BYTE | 2**3*by(3)+2**2*by(2)+2*by(1)+by(0)))
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
  Constants(Machine(TYPES)) == (Type(bool_to_bit) == Cst(SetOf(btype(BOOL,0,1)*btype(INTEGER,"[BIT","]BIT")));Type(bit_xor) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(bit_or) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(bit_and) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(bit_not) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")));Type(bv_index) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?))));Type(bv_put) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*btype(INTEGER,"[BIT","]BIT")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_clear) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_set) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_get) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*btype(INTEGER,"[BIT","]BIT")));Type(bv_xor) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_or) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_and) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_not) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_size) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)));Type(bv_one) == Cst(SetOf(btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_zero) == Cst(SetOf(btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_sub) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_catenate) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(BIT_VECTOR) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(byte_bit_get) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[REAL_BYTE_INDEX","]REAL_BYTE_INDEX")*btype(INTEGER,"[BIT","]BIT")));Type(REAL_BYTE_INDEX) == Cst(SetOf(btype(INTEGER,"[REAL_BYTE_INDEX","]REAL_BYTE_INDEX")));Type(BYTE_ZERO) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(BYTE_INDEX) == Cst(SetOf(btype(INTEGER,"[BYTE_INDEX","]BYTE_INDEX")));Type(BYTE_WIDTH) == Cst(btype(INTEGER,?,?));Type(BYTE) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv16_bit_get) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BV16_INDX","]BV16_INDX")*btype(INTEGER,"[BIT","]BIT")));Type(byte_bv16) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv16_byte) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))));Type(BV16) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(BV16_ZERO) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(REAL_BV16_INDX) == Cst(SetOf(btype(INTEGER,"[REAL_BV16_INDX","]REAL_BV16_INDX")));Type(BV16_INDX) == Cst(SetOf(btype(INTEGER,"[BV16_INDX","]BV16_INDX")));Type(SCHAR) == Cst(SetOf(btype(INTEGER,"[SCHAR","]SCHAR")));Type(SCHAR_MAX) == Cst(btype(INTEGER,?,?));Type(SCHAR_MIN) == Cst(btype(INTEGER,?,?));Type(byte_to_schar) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[SCHAR","]SCHAR")));Type(schar_to_byte) == Cst(SetOf(btype(INTEGER,"[SCHAR","]SCHAR")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(UCHAR) == Cst(SetOf(btype(INTEGER,"[UCHAR","]UCHAR")));Type(UCHAR_MAX) == Cst(btype(INTEGER,?,?));Type(UCHAR_MIN) == Cst(btype(INTEGER,?,?));Type(uchar_to_byte) == Cst(SetOf(btype(INTEGER,"[UCHAR","]UCHAR")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(byte_to_uchar) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[UCHAR","]UCHAR")));Type(update_refresh_reg) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(uchar_schar) == Cst(SetOf(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[SCHAR","]SCHAR")));Type(schar_uchar) == Cst(SetOf(btype(INTEGER,"[SCHAR","]SCHAR")*btype(INTEGER,"[UCHAR","]UCHAR")));Type(SSHORTINT) == Cst(SetOf(btype(INTEGER,"[SSHORTINT","]SSHORTINT")));Type(bv16_to_sshortint) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[SSHORTINT","]SSHORTINT")));Type(sshortint_to_bv16) == Cst(SetOf(btype(INTEGER,"[SSHORTINT","]SSHORTINT")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(INST_SZ) == Cst(btype(INTEGER,?,?));Type(INSTRUCTION) == Cst(SetOf(btype(INTEGER,"[INSTRUCTION","]INSTRUCTION")));Type(NB_INSTRUCTIONS) == Cst(btype(INTEGER,?,?));Type(INSTRUCTION_MAX) == Cst(btype(INTEGER,?,?));Type(instruction_next) == Cst(SetOf(btype(INTEGER,"[USHORTINT","]USHORTINT")*btype(INTEGER,"[USHORTINT","]USHORTINT")));Type(instruction_jump) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(byte_to_bv16) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv16_to_byte) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))));Type(schar_to_sshortint) == Cst(SetOf(btype(INTEGER,"[SCHAR","]SCHAR")*btype(INTEGER,"[SCHAR","]SCHAR")*btype(INTEGER,"[SSHORTINT","]SSHORTINT")));Type(sshortint_to_schar) == Cst(SetOf(btype(INTEGER,"[SSHORTINT","]SSHORTINT")*(btype(INTEGER,"[SCHAR","]SCHAR")*btype(INTEGER,"[SCHAR","]SCHAR"))));Type(USHORTINT) == Cst(SetOf(btype(INTEGER,"[USHORTINT","]USHORTINT")));Type(USHORTINT_MAX) == Cst(btype(INTEGER,?,?));Type(USHORTINT_MIN) == Cst(btype(INTEGER,?,?));Type(bv16_to_ushortint) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[USHORTINT","]USHORTINT")));Type(ushortint_to_bv16) == Cst(SetOf(btype(INTEGER,"[USHORTINT","]USHORTINT")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(ushortint_to_sshortint) == Cst(SetOf(btype(INTEGER,"[USHORTINT","]USHORTINT")*btype(INTEGER,"[SSHORTINT","]SSHORTINT")));Type(sshortint_to_ushortint) == Cst(SetOf(btype(INTEGER,"[SSHORTINT","]SSHORTINT")*btype(INTEGER,"[USHORTINT","]USHORTINT")));Type(get_upper_digit) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)));Type(get_lower_digit) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?))))
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
