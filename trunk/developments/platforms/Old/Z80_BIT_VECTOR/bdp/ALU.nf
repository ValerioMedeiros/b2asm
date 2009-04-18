Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(ALU))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(ALU))==(Machine(ALU));
  Level(Machine(ALU))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(ALU)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(ALU))==(BIT_DEFINITION,BIT_VECTOR_DEFINITION,BYTE_DEFINITION,BV16_DEFINITION,UCHAR_DEFINITION,SCHAR_DEFINITION,SSHORT_DEFINITION,USHORT_DEFINITION,POWER2)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(ALU))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(ALU))==(?);
  List_Includes(Machine(ALU))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(ALU))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(ALU))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(ALU))==(?);
  Context_List_Variables(Machine(ALU))==(?);
  Abstract_List_Variables(Machine(ALU))==(?);
  Local_List_Variables(Machine(ALU))==(?);
  List_Variables(Machine(ALU))==(?);
  External_List_Variables(Machine(ALU))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(ALU))==(?);
  Abstract_List_VisibleVariables(Machine(ALU))==(?);
  External_List_VisibleVariables(Machine(ALU))==(?);
  Expanded_List_VisibleVariables(Machine(ALU))==(?);
  List_VisibleVariables(Machine(ALU))==(?);
  Internal_List_VisibleVariables(Machine(ALU))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(ALU))==(btrue);
  Gluing_List_Invariant(Machine(ALU))==(btrue);
  Expanded_List_Invariant(Machine(ALU))==(btrue);
  Abstract_List_Invariant(Machine(ALU))==(btrue);
  Context_List_Invariant(Machine(ALU))==(btrue);
  List_Invariant(Machine(ALU))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(ALU))==(btrue);
  Abstract_List_Assertions(Machine(ALU))==(btrue);
  Context_List_Assertions(Machine(ALU))==(!bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_bit(TRUE) = 1 & bool_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_not(bv_not(bv)),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: 0..bv_size(bv)-1 & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,v2),indx) = bv_get(bv_and(v2,v1),indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,bv_and(v2,v3)),indx) = bv_get(bv_and(bv_and(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv_zero(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_one(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,v2),indx) = bv_get(bv_or(v2,v1),indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,bv_or(v2,v3)),indx) = bv_get(bv_or(bv_or(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_one(bv_size(bv))),indx) = bv_get(bv_one(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_xor(v1,v2),indx) = bv_get(bv_xor(v2,v1),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_xor(bv,bv),indx) = bv_get(bv_zero(bv_size(bv)),indx))) & !bt.(bt: BYTE => size(bt) = 8) & size(BYTE_ZERO) = 8 & BYTE <: BIT_VECTOR & BYTE_ZERO: BIT_VECTOR & first(BYTE_ZERO) = 0 & 8: NATURAL & BV16_ZERO: BV16 & BV16 <: BIT_VECTOR & BV16_ZERO: BIT_VECTOR & 2**8-1: INTEGER & 0: INTEGER & (2**7-1: INTEGER;(-2)**7: INTEGER) & (-(2**15): SSHORT;2**15-1: SSHORT) & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536));
  List_Assertions(Machine(ALU))==(dom(add8UCHAR) = BIT*UCHAR*UCHAR & ran(add8UCHAR): POW(UCHAR*BIT*BIT*BIT*BIT) & dom(substract8UCHAR) = BIT*UCHAR*UCHAR & ran(substract8UCHAR): POW(UCHAR*BIT*BIT*BIT*BIT) & dom(and) = BYTE*BYTE & ran(and) <: BYTE & dom(ior) = BYTE*BYTE & ran(ior) <: BYTE & dom(xor) = BYTE*BYTE & ran(xor) <: BYTE & dom(complement) = BYTE & ran(complement) <: BYTE & dom(swap) = BYTE & ran(swap) <: BYTE & dom(rotateleft) = BYTE & ran(rotateleft) <: BYTE & dom(rotateright) = BYTE & ran(rotateright) <: BYTE & !(vec,in0).(vec: BYTE & in0: 0..7 => bitget(vec,in0) = vec(in0)) & !(x0,x1).(x0: UCHAR & x1: UCHAR => simple_add8UCHAR(x0,x1): UCHAR) & 0 = schar_sshort(0,0) & !n0.(n0: UCHAR => 0<=n0) & !n0.(n0: UCHAR => n0<=255) & instruction_next: USHORT --> USHORT & !xx.(xx: BYTE => byte_uchar(xx): UCHAR) & !xx.(xx: UCHAR => uchar_byte(xx): BYTE) & !xx.(xx: BYTE => update_refresh_reg(xx): BYTE) & !xx.(xx: BYTE => byte_schar(xx): SCHAR) & !xx.(xx: SCHAR => schar_byte(xx): BYTE) & !(xx,yy).(xx: BYTE & yy: BYTE => byte_bv16(xx,yy): BV16) & !(xx,yy).(xx: BYTE & yy: BYTE => #zz.(zz: BV16 & byte_bv16(xx,yy) = zz)) & !xx.(xx: BV16 => bv16_ushort(xx): USHORT) & !xx.(xx: USHORT => ushort_bv16(xx): BV16) & !xx.(xx: BYTE => get_upper_digit(xx): 0..16) & !xx.(xx: BYTE => get_lower_digit(xx): 0..16) & !(zn,c0,value,h0).(zn: BIT & c0: BIT & value: BYTE & h0: BIT => daa_function(zn,c0,h0,value): BYTE*BIT*BIT))
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(ALU))==(skip);
  Context_List_Initialisation(Machine(ALU))==(skip);
  List_Initialisation(Machine(ALU))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(ALU))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(ALU),Machine(BIT_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(ALU),Machine(BIT_VECTOR_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(ALU),Machine(BYTE_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(ALU),Machine(BV16_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(ALU),Machine(UCHAR_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(ALU),Machine(SCHAR_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(ALU),Machine(SSHORT_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(ALU),Machine(USHORT_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(ALU),Machine(POWER2))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(ALU))==(btrue);
  List_Constraints(Machine(ALU))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(ALU))==(?);
  List_Operations(Machine(ALU))==(?)
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
  List_Valuable_Constants(Machine(ALU))==(is_zero,is_zeroUSHORT,is_negative,half,simple_add8UCHAR,add8UCHAR,substract8UCHAR,add16USHORT,add_carryUSHORT,add_halfcarryUSHORT,sub16USHORT,sub_carryUSHORT,sub_halfcarryUSHORT,inc_BYTE,dec_BYTE,inc_BV16,dec_BV16,parity_even_BYTE,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright,update_refresh_reg,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,instruction_jump,daa_function);
  Inherited_List_Constants(Machine(ALU))==(?);
  List_Constants(Machine(ALU))==(is_zero,is_zeroUSHORT,is_negative,half,simple_add8UCHAR,add8UCHAR,substract8UCHAR,add16USHORT,add_carryUSHORT,add_halfcarryUSHORT,sub16USHORT,sub_carryUSHORT,sub_halfcarryUSHORT,inc_BYTE,dec_BYTE,inc_BV16,dec_BV16,parity_even_BYTE,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright,update_refresh_reg,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,instruction_jump,daa_function)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(ALU))==(?);
  Context_List_Defered(Machine(ALU))==(?);
  Context_List_Sets(Machine(ALU))==(?);
  List_Valuable_Sets(Machine(ALU))==(?);
  Inherited_List_Enumerated(Machine(ALU))==(?);
  Inherited_List_Defered(Machine(ALU))==(?);
  Inherited_List_Sets(Machine(ALU))==(?);
  List_Enumerated(Machine(ALU))==(?);
  List_Defered(Machine(ALU))==(?);
  List_Sets(Machine(ALU))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(ALU))==(?);
  Expanded_List_HiddenConstants(Machine(ALU))==(?);
  List_HiddenConstants(Machine(ALU))==(?);
  External_List_HiddenConstants(Machine(ALU))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(ALU))==(btrue);
  Context_List_Properties(Machine(ALU))==(BIT = 0..1 & bit_not: BIT --> BIT & bit_not = %bb.(bb: BIT | 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_bit: BOOL --> BIT & bool_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = seq1(BIT) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_index: BIT_VECTOR --> POW(NATURAL) & bv_index = %v1.(v1: BIT_VECTOR | 0..bv_size(v1)-1) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: low..bv_size(bv)-1 | %i0.(i0: 1..high-low+1 | bv(i0+low))) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (1..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (1..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 1..bv_size(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_xor(v1(idx),v2(idx)))) & bv_get: BIT_VECTOR*NATURAL --> BIT & bv_get = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1(idx+1)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 & bit: BIT | v1<+{idx+1|->bit}) & BYTE_INDEX = 1..8 & REAL_BYTE_INDEX = 0..8-1 & BYTE = {bt | bt: BIT_VECTOR & bv_size(bt) = 8} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & byte_bit_get: BYTE*REAL_BYTE_INDEX --> BIT & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)) & get_upper_digit = %by.(by: BYTE | 2**3*by(8)+2**2*by(7)+2*by(6)+by(5)) & get_lower_digit = %by.(by: BYTE | 2**3*by(4)+2**2*by(3)+2*by(2)+by(1)) & BV16_INDX = 1..16 & REAL_BV16_INDX = 0..16-1 & BV16 = {bt | bt: BIT_VECTOR & bv_size(bt) = 16} & BV16_ZERO = BV16_INDX*{0} & bv16_byte: BV16 --> BYTE*BYTE & bv16_byte = %bv.(bv: BV16 | {0|->bv(8),1|->bv(9),2|->bv(10),3|->bv(11),4|->bv(12),5|->bv(13),6|->bv(14),7|->bv(15)},{0|->bv(0),1|->bv(1),2|->bv(2),3|->bv(3),4|->bv(4),5|->bv(5),6|->bv(6),7|->bv(7)}) & byte_bv16 = bv16_byte~ & bv16_bit_get: BYTE*BV16_INDX --> BIT & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)) & UCHAR = 0..2**8-1 & byte_uchar: BYTE --> UCHAR & byte_uchar = %v0.(v0: BYTE | 2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & uchar_byte: UCHAR --> BYTE & uchar_byte = byte_uchar~ & SCHAR = (-2)**7..2**7-1 & byte_schar: BYTE --> SCHAR & byte_schar = %v0.(v0: BYTE | (-2)**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & schar_byte: SCHAR --> BYTE & schar_byte = byte_schar~ & uchar_schar: UCHAR --> SCHAR & uchar_schar = %v1.(v1: UCHAR & v1<=2**7-1 | v1) & uchar_schar = %v1.(v1: UCHAR & not(v1<=2**7-1) | v1-2**8-1+1) & schar_uchar: SCHAR --> UCHAR & schar_uchar = uchar_schar~ & SSHORT = -(2**15)..2**15-1 & bv16_sshort: BV16 --> SSHORT & bv16_sshort = %v0.(v0: BV16 | (-2)**15*v0(15)+2**14*v0(14)+2**13*v0(13)+2**12*v0(12)+2**11*v0(11)+2**10*v0(10)+2**9*v0(9)+2**8*v0(8)+2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & sshort_bv16: SSHORT --> BV16 & sshort_bv16 = bv16_sshort~ & schar_sshort: SCHAR*SCHAR --> SSHORT & schar_sshort = %(w1,w2).(w1: SCHAR & w2: SCHAR | bv16_sshort(byte_bv16(schar_byte(w1),schar_byte(w2)))) & sshort_schar: SSHORT --> SCHAR*SCHAR & sshort_schar = schar_sshort~ & USHORT = 0..2**16-1 & bv16_ushort: BV16 --> USHORT & bv16_ushort = %v0.(v0: BV16 | 32768*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & ushort_bv16 = bv16_ushort~);
  Inherited_List_Properties(Machine(ALU))==(btrue);
  List_Properties(Machine(ALU))==(is_zero: BYTE --> BIT & is_zero = %w1.(w1: BYTE | bool_bit(bool(w1(0)+w1(1)+w1(2)+w1(3)+w1(4)+w1(5)+w1(6)+w1(7) = 0))) & is_zeroUSHORT: USHORT --> BIT & is_zeroUSHORT = %nat1.(nat1: USHORT | bool_bit(bool(nat1 = 0))) & is_negative: BYTE --> BIT & is_negative = %w1.(w1: BYTE | w1(7)) & half: UCHAR --> UCHAR & half = %ww.(ww: UCHAR | ww mod 2**4) & simple_add8UCHAR: UCHAR*UCHAR --> UCHAR & simple_add8UCHAR = %(w1,w2).(w1: UCHAR & w2: UCHAR | (w1+w2) mod 2**8) & add8UCHAR: BIT*UCHAR*UCHAR --> UCHAR*BIT*BIT*BIT*BIT & add8UCHAR = %(carry,w1,w2).(carry: BIT & w1: UCHAR & w2: UCHAR | (carry+w1+w2) mod 256,bool_bit(bool(carry+uchar_schar(w1)+uchar_schar(w2)<0)),bool_bit(bool(carry+w1+w2>2**8-1)),bool_bit(bool(carry+half(w1)+half(w2)>=2**4)),bool_bit(bool((carry+w1+w2) mod 2**8-1-1 = 0))) & substract8UCHAR: BIT*UCHAR*UCHAR --> UCHAR*BIT*BIT*BIT*BIT & substract8UCHAR = %(carry,w1,w2).(carry: BIT & w1: UCHAR & w2: UCHAR | (carry+w1-w2) mod 256,bool_bit(bool(carry+uchar_schar(w1)-uchar_schar(w2)<0)),bool_bit(bool(carry+w1-w2>2**8-1)),bool_bit(bool(carry+half(w1)-half(w2)>=2**4)),bool_bit(bool((carry+w1-w2) mod 2**8-1-1 = 0))) & add16USHORT: BIT*USHORT*USHORT --> USHORT & add16USHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | (b1+w1+w2) mod 65536) & add_carryUSHORT: BIT*USHORT*USHORT --> BIT & add_carryUSHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | bool_bit(bool(b1+w1+w2>2**16))) & add_halfcarryUSHORT: BIT*USHORT*USHORT --> BIT & add_halfcarryUSHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | bool_bit(bool(b1+w1 mod 2**12+w2 mod 2**12>2**12))) & sub16USHORT: BIT*USHORT*USHORT --> USHORT & sub16USHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | (w1-w2-b1) mod 65536) & sub_carryUSHORT: BIT*USHORT*USHORT --> BIT & sub_carryUSHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | bool_bit(bool(w1-w2-b1>2**16))) & sub_halfcarryUSHORT: BIT*USHORT*USHORT --> BIT & sub_halfcarryUSHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | bool_bit(bool(w1 mod 2**12-w2 mod 2**12-b1>2**12))) & inc_BYTE: BYTE --> BYTE & inc_BYTE = %w1.(w1: BYTE | uchar_byte((byte_uchar(w1)+1) mod 256)) & dec_BYTE: BYTE --> BYTE & dec_BYTE = %w1.(w1: BYTE | uchar_byte((byte_uchar(w1)-1) mod 256)) & inc_BV16: BV16 --> BV16 & inc_BV16 = %w1.(w1: BV16 | ushort_bv16((bv16_ushort(w1)+1) mod 65536)) & dec_BV16: BYTE --> BYTE & dec_BV16 = %w1.(w1: BV16 | ushort_bv16((bv16_ushort(w1)-1) mod 65536)) & parity_even_BYTE: BIT_VECTOR --> BIT & parity_even_BYTE = %bv.(bv: BIT_VECTOR | 1-(bv_get(bv,0)+bv_get(bv,1)+bv_get(bv,2)+bv_get(bv,3)+bv_get(bv,4)+bv_get(bv,5)+bv_get(bv,7)) mod 2) & and: BYTE*BYTE --> BYTE & and = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_and(bt1,bt2)) & ior: BYTE*BYTE --> BYTE & ior = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_or(bt1,bt2)) & xor: BYTE*BYTE --> BYTE & xor = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_xor(bt1,bt2)) & bitget: BYTE*BYTE_INDEX --> BIT & bitget = %(bt1,ii).(bt1: BYTE & ii: BYTE_INDEX | bt1(ii)) & bitset: BYTE*BYTE_INDEX --> BYTE & !(ww,ii).(ww: BYTE & ii: BYTE_INDEX => bitset(ww,ii) = bv_set(ww,ii)) & bitclear: BYTE*BYTE_INDEX --> BYTE & !(ww,ii,bb).(ww: BYTE & ii: BYTE_INDEX & bb: BIT => bitclear(ww,ii) = bv_clear(ww,ii)) & complement: BYTE --> BYTE & complement = %bt.(bt: BYTE | bv_not(bt)) & swap: BYTE --> BYTE & swap = %bt.(bt: BYTE | {0|->bt(4),1|->bt(5),2|->bt(6),3|->bt(7),4|->bt(0),5|->bt(1),6|->bt(2),7|->bt(3)}) & rotateleft: BYTE --> BYTE & rotateleft = %bv.(bv: BYTE | {0|->bv(7),1|->bv(0),2|->bv(1),3|->bv(2),4|->bv(3),5|->bv(4),6|->bv(5),7|->bv(6)}) & rotateright: BYTE --> BYTE & rotateright = %bv.(bv: BYTE | {0|->bv(1),1|->bv(2),2|->bv(3),3|->bv(4),4|->bv(5),5|->bv(6),6|->bv(7),7|->bv(0)}) & update_refresh_reg: BYTE --> BYTE & update_refresh_reg = %v0.(v0: BYTE | uchar_byte(2**7*v0(7)+(2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) mod 64)) & NB_INSTRUCTIONS: NATURAL & INST_SZ: NATURAL & INST_SZ = 16 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = USHORT & instruction_next: USHORT --> USHORT & instruction_next = %w1.(w1: USHORT | (w1+1) mod 65535) & instruction_jump = %(p0,e0).(p0: INSTRUCTION & e0: -126..129 | (p0+e0) mod 65535) & daa_function: BIT*BIT*BIT*BYTE --> BYTE*BIT*BIT & !(zn,c0,h0,value).(zn: BIT & c0: BIT & h0: BIT & value: BYTE => (zn = 0 & c0 = 0 & get_upper_digit(value): 0..9 & h0 = 0 & get_lower_digit(value): 0..9 => daa_function(zn,c0,h0,value) = value,0,0) & (zn = 0 & c0 = 0 & get_upper_digit(value): 0..8 & h0 = 0 & get_lower_digit(value): 10..15 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),6)),0,bool_bit(bool(half(byte_uchar(value))+half(6)>=2**4))) & (zn = 0 & c0 = 0 & get_upper_digit(value): 0..9 & h0 = 1 & get_lower_digit(value): 0..3 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),6)),0,bool_bit(bool(half(byte_uchar(value))+half(6)>=2**4))) & (zn = 0 & c0 = 0 & get_upper_digit(value): 10..15 & h0 = 0 & get_lower_digit(value): 0..9 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),96)),1,bool_bit(bool(half(byte_uchar(value))+half(96)>=2**4))) & (zn = 0 & c0 = 0 & get_upper_digit(value): 9..15 & h0 = 0 & get_lower_digit(value): 10..15 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),102)),1,bool_bit(bool(half(byte_uchar(value))+half(102)>=2**4))) & (zn = 0 & c0 = 0 & get_upper_digit(value): 10..15 & h0 = 1 & get_lower_digit(value): 0..3 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),102)),1,bool_bit(bool(half(byte_uchar(value))+half(102)>=2**4))) & (zn = 0 & c0 = 1 & get_upper_digit(value): 0..2 & h0 = 0 & get_lower_digit(value): 0..9 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),96)),1,bool_bit(bool(half(byte_uchar(value))+half(96)>=2**4))) & (zn = 0 & c0 = 1 & get_upper_digit(value): 0..2 & h0 = 0 & get_lower_digit(value): 10..15 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),102)),1,bool_bit(bool(half(byte_uchar(value))+half(102)>=2**4))) & (zn = 0 & c0 = 1 & get_upper_digit(value): 0..3 & h0 = 1 & get_lower_digit(value): 0..3 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),102)),1,bool_bit(bool(half(byte_uchar(value))+half(102)>=2**4))) & (zn = 1 & c0 = 0 & get_upper_digit(value): 0..9 & h0 = 0 & get_lower_digit(value): 0..9 => daa_function(zn,c0,h0,value) = value,0,0) & (zn = 1 & c0 = 0 & get_upper_digit(value): 0..8 & h0 = 1 & get_lower_digit(value): 6..15 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),250)),0,bool_bit(bool(half(byte_uchar(value))+half(250)>=2**4))) & (zn = 1 & c0 = 1 & get_upper_digit(value): 7..15 & h0 = 0 & get_lower_digit(value): 0..9 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),160)),1,bool_bit(bool(half(byte_uchar(value))+half(160)>=2**4))) & (zn = 1 & c0 = 1 & get_upper_digit(value): 6..7 & h0 = 1 & get_lower_digit(value): 6..15 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),154)),1,bool_bit(bool(half(byte_uchar(value))+half(154)>=2**4)))))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(ALU),Machine(POWER2))==(?);
  Seen_Context_List_Enumerated(Machine(ALU))==(?);
  Seen_Context_List_Invariant(Machine(ALU))==(btrue);
  Seen_Context_List_Assertions(Machine(ALU))==(!bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_bit(TRUE) = 1 & bool_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_not(bv_not(bv)),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: 0..bv_size(bv)-1 & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,v2),indx) = bv_get(bv_and(v2,v1),indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,bv_and(v2,v3)),indx) = bv_get(bv_and(bv_and(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv_zero(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_one(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,v2),indx) = bv_get(bv_or(v2,v1),indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,bv_or(v2,v3)),indx) = bv_get(bv_or(bv_or(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_one(bv_size(bv))),indx) = bv_get(bv_one(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_xor(v1,v2),indx) = bv_get(bv_xor(v2,v1),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_xor(bv,bv),indx) = bv_get(bv_zero(bv_size(bv)),indx))) & 8: NATURAL & first(BYTE_ZERO) = 0 & BYTE_ZERO: BIT_VECTOR & BYTE <: BIT_VECTOR & size(BYTE_ZERO) = 8 & !bt.(bt: BYTE => size(bt) = 8) & 0: INTEGER & 2**8-1: INTEGER & (2**7-1: INTEGER;(-2)**7: INTEGER) & BV16_ZERO: BIT_VECTOR & BV16 <: BIT_VECTOR & BV16_ZERO: BV16 & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536) & !(nn,pp).(nn: NATURAL1 & pp: NAT => (pp = 0 => nn**pp = 1) & (pp = 1 => nn**pp = nn) & (pp>1 => nn**pp = nn*nn**(pp-1))));
  Seen_Context_List_Properties(Machine(ALU))==(BIT = 0..1 & bit_not: BIT --> BIT & bit_not = %bb.(bb: BIT | 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_bit: BOOL --> BIT & bool_bit = {TRUE|->1,FALSE|->0} & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 & bit: BIT | v1<+{idx+1|->bit}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->0}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->1}) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_get = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1(idx+1)) & bv_get: BIT_VECTOR*NATURAL --> BIT & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_xor(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_or(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_and(v1(idx),v2(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 1..bv_size(v1) | bit_not(v1(idx)))) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (1..sz)*{1}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (1..sz)*{0}) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: low..bv_size(bv)-1 | %i0.(i0: 1..high-low+1 | bv(i0+low))) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_index = %v1.(v1: BIT_VECTOR | 0..bv_size(v1)-1) & bv_index: BIT_VECTOR --> POW(NATURAL) & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_size: BIT_VECTOR --> NATURAL1 & BIT_VECTOR = seq1(BIT) & get_lower_digit = %by.(by: BYTE | 2**3*by(4)+2**2*by(3)+2*by(2)+by(1)) & get_upper_digit = %by.(by: BYTE | 2**3*by(8)+2**2*by(7)+2*by(6)+by(5)) & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)) & byte_bit_get: BYTE*REAL_BYTE_INDEX --> BIT & BYTE_ZERO = BYTE_INDEX*{0} & BYTE_ZERO: BYTE & BYTE = {bt | bt: BIT_VECTOR & bv_size(bt) = 8} & REAL_BYTE_INDEX = 0..8-1 & BYTE_INDEX = 1..8 & uchar_byte = byte_uchar~ & uchar_byte: UCHAR --> BYTE & byte_uchar = %v0.(v0: BYTE | 2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & byte_uchar: BYTE --> UCHAR & UCHAR = 0..2**8-1 & schar_uchar = uchar_schar~ & schar_uchar: SCHAR --> UCHAR & uchar_schar = %v1.(v1: UCHAR & not(v1<=2**7-1) | v1-2**8-1+1) & uchar_schar = %v1.(v1: UCHAR & v1<=2**7-1 | v1) & uchar_schar: UCHAR --> SCHAR & schar_byte = byte_schar~ & schar_byte: SCHAR --> BYTE & byte_schar = %v0.(v0: BYTE | (-2)**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & byte_schar: BYTE --> SCHAR & SCHAR = (-2)**7..2**7-1 & bv16_bit_get: BYTE*BV16_INDX --> BIT & byte_bv16 = bv16_byte~ & bv16_byte = %bv.(bv: BV16 | {0|->bv(8),1|->bv(9),2|->bv(10),3|->bv(11),4|->bv(12),5|->bv(13),6|->bv(14),7|->bv(15)},{0|->bv(0),1|->bv(1),2|->bv(2),3|->bv(3),4|->bv(4),5|->bv(5),6|->bv(6),7|->bv(7)}) & bv16_byte: BV16 --> BYTE*BYTE & BV16_ZERO = BV16_INDX*{0} & BV16 = {bt | bt: BIT_VECTOR & bv_size(bt) = 16} & REAL_BV16_INDX = 0..16-1 & BV16_INDX = 1..16);
  Seen_List_Constraints(Machine(ALU))==(btrue);
  Seen_List_Operations(Machine(ALU),Machine(POWER2))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(POWER2))==(btrue);
  Seen_Internal_List_Operations(Machine(ALU),Machine(USHORT_DEFINITION))==(?);
  Seen_List_Operations(Machine(ALU),Machine(USHORT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(USHORT_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(ALU),Machine(SSHORT_DEFINITION))==(?);
  Seen_List_Operations(Machine(ALU),Machine(SSHORT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(SSHORT_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(ALU),Machine(SCHAR_DEFINITION))==(?);
  Seen_List_Operations(Machine(ALU),Machine(SCHAR_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(SCHAR_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(ALU),Machine(UCHAR_DEFINITION))==(?);
  Seen_List_Operations(Machine(ALU),Machine(UCHAR_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(UCHAR_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(ALU),Machine(BV16_DEFINITION))==(?);
  Seen_List_Operations(Machine(ALU),Machine(BV16_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(BV16_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(ALU),Machine(BYTE_DEFINITION))==(?);
  Seen_List_Operations(Machine(ALU),Machine(BYTE_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(BYTE_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(ALU),Machine(BIT_VECTOR_DEFINITION))==(?);
  Seen_List_Operations(Machine(ALU),Machine(BIT_VECTOR_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(ALU),Machine(BIT_DEFINITION))==(?);
  Seen_List_Operations(Machine(ALU),Machine(BIT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(BIT_DEFINITION))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(ALU),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(ALU)) == (is_zero,is_zeroUSHORT,is_negative,half,simple_add8UCHAR,add8UCHAR,substract8UCHAR,add16USHORT,add_carryUSHORT,add_halfcarryUSHORT,sub16USHORT,sub_carryUSHORT,sub_halfcarryUSHORT,inc_BYTE,dec_BYTE,inc_BV16,dec_BV16,parity_even_BYTE,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright,update_refresh_reg,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,instruction_jump,daa_function | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BYTE_DEFINITION)),seen(Machine(BV16_DEFINITION)),seen(Machine(UCHAR_DEFINITION)),seen(Machine(SCHAR_DEFINITION)),seen(Machine(SSHORT_DEFINITION)),seen(Machine(USHORT_DEFINITION)),seen(Machine(POWER2)) | ? | ALU);
  List_Of_HiddenCst_Ids(Machine(ALU)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ALU)) == (is_zero,is_zeroUSHORT,is_negative,half,simple_add8UCHAR,add8UCHAR,substract8UCHAR,add16USHORT,add_carryUSHORT,add_halfcarryUSHORT,sub16USHORT,sub_carryUSHORT,sub_halfcarryUSHORT,inc_BYTE,dec_BYTE,inc_BV16,dec_BV16,parity_even_BYTE,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright,update_refresh_reg,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,instruction_jump,daa_function);
  List_Of_VisibleVar_Ids(Machine(ALU)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ALU)) == (?: ?);
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
  List_Of_Ids(Machine(USHORT_DEFINITION)) == (USHORT,bv16_ushort,ushort_bv16 | ? | ? | ? | ? | ? | seen(Machine(BV16_DEFINITION)) | ? | USHORT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(USHORT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(USHORT_DEFINITION)) == (USHORT,bv16_ushort,ushort_bv16);
  List_Of_VisibleVar_Ids(Machine(USHORT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(USHORT_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BV16_DEFINITION)) == (BV16_INDX,REAL_BV16_INDX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BYTE_DEFINITION)) | ? | BV16_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BV16_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BV16_DEFINITION)) == (BV16_INDX,REAL_BV16_INDX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get);
  List_Of_VisibleVar_Ids(Machine(BV16_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BV16_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get,get_upper_digit,get_lower_digit | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)) | ? | BYTE_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get,get_upper_digit,get_lower_digit);
  List_Of_VisibleVar_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BYTE_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)) | ? | BIT_VECTOR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_bit | ? | ? | ? | ? | ? | ? | ? | BIT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_bit);
  List_Of_VisibleVar_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(SSHORT_DEFINITION)) == (SSHORT,bv16_sshort,sshort_bv16,schar_sshort,sshort_schar | ? | ? | ? | ? | ? | seen(Machine(POWER2)),seen(Machine(BV16_DEFINITION)),seen(Machine(SCHAR_DEFINITION)) | ? | SSHORT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(SSHORT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(SSHORT_DEFINITION)) == (SSHORT,bv16_sshort,sshort_bv16,schar_sshort,sshort_schar);
  List_Of_VisibleVar_Ids(Machine(SSHORT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(SSHORT_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(SCHAR_DEFINITION)) == (SCHAR,byte_schar,schar_byte,uchar_schar,schar_uchar | ? | ? | ? | ? | ? | seen(Machine(BYTE_DEFINITION)),seen(Machine(UCHAR_DEFINITION)) | ? | SCHAR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(SCHAR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(SCHAR_DEFINITION)) == (SCHAR,byte_schar,schar_byte,uchar_schar,schar_uchar);
  List_Of_VisibleVar_Ids(Machine(SCHAR_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(SCHAR_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(UCHAR_DEFINITION)) == (UCHAR,uchar_byte,byte_uchar | ? | ? | ? | ? | ? | seen(Machine(BYTE_DEFINITION)) | ? | UCHAR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(UCHAR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(UCHAR_DEFINITION)) == (UCHAR,uchar_byte,byte_uchar);
  List_Of_VisibleVar_Ids(Machine(UCHAR_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(UCHAR_DEFINITION)) == (?: ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(ALU)) == (Type(is_zero) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BIT","]BIT")));Type(is_zeroUSHORT) == Cst(SetOf(btype(INTEGER,"[USHORT","]USHORT")*btype(INTEGER,"[BIT","]BIT")));Type(is_negative) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BIT","]BIT")));Type(half) == Cst(SetOf(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[UCHAR","]UCHAR")));Type(simple_add8UCHAR) == Cst(SetOf(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[UCHAR","]UCHAR")));Type(add8UCHAR) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[UCHAR","]UCHAR")*(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT"))));Type(substract8UCHAR) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[UCHAR","]UCHAR")*(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT"))));Type(add16USHORT) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[USHORT","]USHORT")*btype(INTEGER,"[USHORT","]USHORT")*btype(INTEGER,"[USHORT","]USHORT")));Type(add_carryUSHORT) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[USHORT","]USHORT")*btype(INTEGER,"[USHORT","]USHORT")*btype(INTEGER,"[BIT","]BIT")));Type(add_halfcarryUSHORT) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[USHORT","]USHORT")*btype(INTEGER,"[USHORT","]USHORT")*btype(INTEGER,"[BIT","]BIT")));Type(sub16USHORT) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[USHORT","]USHORT")*btype(INTEGER,"[USHORT","]USHORT")*btype(INTEGER,"[USHORT","]USHORT")));Type(sub_carryUSHORT) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[USHORT","]USHORT")*btype(INTEGER,"[USHORT","]USHORT")*btype(INTEGER,"[BIT","]BIT")));Type(sub_halfcarryUSHORT) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[USHORT","]USHORT")*btype(INTEGER,"[USHORT","]USHORT")*btype(INTEGER,"[BIT","]BIT")));Type(inc_BYTE) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(dec_BYTE) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(inc_BV16) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(dec_BV16) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(parity_even_BYTE) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BIT","]BIT")));Type(and) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(ior) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(xor) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bitclear) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BYTE_INDEX","]BYTE_INDEX")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bitset) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BYTE_INDEX","]BYTE_INDEX")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bitget) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BYTE_INDEX","]BYTE_INDEX")*btype(INTEGER,"[BIT","]BIT")));Type(complement) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(swap) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(rotateleft) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(rotateright) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(update_refresh_reg) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(INST_SZ) == Cst(btype(INTEGER,?,?));Type(INSTRUCTION) == Cst(SetOf(btype(INTEGER,"[INSTRUCTION","]INSTRUCTION")));Type(NB_INSTRUCTIONS) == Cst(btype(INTEGER,?,?));Type(INSTRUCTION_MAX) == Cst(btype(INTEGER,?,?));Type(instruction_next) == Cst(SetOf(btype(INTEGER,"[USHORT","]USHORT")*btype(INTEGER,"[USHORT","]USHORT")));Type(instruction_jump) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(daa_function) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")))))
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
