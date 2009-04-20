Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(USHORT_DEFINITION))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(USHORT_DEFINITION))==(Machine(USHORT_DEFINITION));
  Level(Machine(USHORT_DEFINITION))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(USHORT_DEFINITION)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(USHORT_DEFINITION))==(BIT_DEFINITION,BIT_VECTOR_DEFINITION,BYTE_DEFINITION,BV16_DEFINITION,UCHAR_DEFINITION,POWER2)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(USHORT_DEFINITION))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(USHORT_DEFINITION))==(?);
  List_Includes(Machine(USHORT_DEFINITION))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(USHORT_DEFINITION))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(USHORT_DEFINITION))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(USHORT_DEFINITION))==(?);
  Context_List_Variables(Machine(USHORT_DEFINITION))==(?);
  Abstract_List_Variables(Machine(USHORT_DEFINITION))==(?);
  Local_List_Variables(Machine(USHORT_DEFINITION))==(?);
  List_Variables(Machine(USHORT_DEFINITION))==(?);
  External_List_Variables(Machine(USHORT_DEFINITION))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(USHORT_DEFINITION))==(?);
  Abstract_List_VisibleVariables(Machine(USHORT_DEFINITION))==(?);
  External_List_VisibleVariables(Machine(USHORT_DEFINITION))==(?);
  Expanded_List_VisibleVariables(Machine(USHORT_DEFINITION))==(?);
  List_VisibleVariables(Machine(USHORT_DEFINITION))==(?);
  Internal_List_VisibleVariables(Machine(USHORT_DEFINITION))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(USHORT_DEFINITION))==(btrue);
  Gluing_List_Invariant(Machine(USHORT_DEFINITION))==(btrue);
  Expanded_List_Invariant(Machine(USHORT_DEFINITION))==(btrue);
  Abstract_List_Invariant(Machine(USHORT_DEFINITION))==(btrue);
  Context_List_Invariant(Machine(USHORT_DEFINITION))==(btrue);
  List_Invariant(Machine(USHORT_DEFINITION))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(USHORT_DEFINITION))==(btrue);
  Abstract_List_Assertions(Machine(USHORT_DEFINITION))==(btrue);
  Context_List_Assertions(Machine(USHORT_DEFINITION))==(!bb.(bb: BIT => bit_not(bb) = 1-bb) & dom(bit_and) = BIT*BIT & ran(bit_not) = BIT & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & dom(bit_and) = BIT*BIT & ran(bit_and) = BIT & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & dom(bit_or) = BIT*BIT & ran(bit_or) = BIT & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & dom(bit_xor) = BIT*BIT & ran(bit_xor) = BIT & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & dom(bool_bit) = BOOL & ran(bit_xor) = BIT & bool_bit(TRUE) = 1 & bool_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_not(bv_not(bv)),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: 0..bv_size(bv)-1 & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,v2),indx) = bv_get(bv_and(v2,v1),indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,bv_and(v2,v3)),indx) = bv_get(bv_and(bv_and(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv_zero(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_one(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,v2),indx) = bv_get(bv_or(v2,v1),indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,bv_or(v2,v3)),indx) = bv_get(bv_or(bv_or(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_one(bv_size(bv))),indx) = bv_get(bv_one(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_xor(v1,v2),indx) = bv_get(bv_xor(v2,v1),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_xor(bv,bv),indx) = bv_get(bv_zero(bv_size(bv)),indx))) & size(BYTE_ZERO) = 8 & BYTE <: BIT_VECTOR & BYTE_ZERO: BIT_VECTOR & first(BYTE_ZERO) = 0 & !bt.(bt: BYTE => bv_size(bt) = size(bt) & bv_size(bt) = 8 & bt: seq1(BIT)) & 8: NATURAL & dom(is_zero) = BYTE & ran(is_zero) = BIT & dom(parity_even) = BYTE & ran(parity_even) = BIT & dom(and) = BYTE*BYTE & ran(and) = BYTE & dom(ior) = BYTE*BYTE & ran(ior) = BYTE & dom(xor) = BYTE*BYTE & ran(xor) = BYTE & dom(bitget) = BYTE*PHYS_BYTE_INDEX & ran(bitget) = BIT & dom(bitset) = BYTE*PHYS_BYTE_INDEX & ran(bitset) = BYTE & dom(bitclear) = BYTE*PHYS_BYTE_INDEX & ran(bitclear) = BYTE & dom(complement) = BYTE & ran(complement) = BYTE & dom(swap) = BYTE & ran(swap) = BYTE & dom(rotateleft) = BYTE & ran(rotateleft) = BYTE & dom(rotateright) = BYTE & ran(rotateright) = BYTE & dom(get_upper_digit) = BYTE & ran(get_upper_digit) = 0..16-1 & dom(get_lower_digit) = BYTE & ran(get_lower_digit) = 0..16-1 & [1,1,1,1,1,1,1,1]: BYTE & [0,0,0,0,0,0,0,0]: BYTE & 1 = 2**0 & 2 = 2**1 & 4 = 2**2 & 8 = 2**3 & 16 = 2**4 & 32 = 2**5 & 64 = 2**6 & 128 = 2**7 & 256 = 2**8 & 512 = 2**9 & 1024 = 2**10 & 2048 = 2**11 & 4096 = 2**12 & 8192 = 2**13 & 16384 = 2**14 & 32768 = 2**15 & 65536 = 2**16 & BV16_ZERO: BV16 & BV16 <: BIT_VECTOR & !bt.(bt: BV16 => bv_size(bt) = size(bt) & bv_size(bt) = 16 & bt: seq1(BIT)) & dom(bv16_byte) = BV16 & ran(bv16_byte) = BYTE*BYTE & dom(byte_bv16) = BYTE*BYTE & ran(byte_bv16) = BV16 & dom(bv16_bit_get) = BV16*PHYS_BV16_INDEX & ran(bv16_bit_get) = BIT & [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]: BV16 & [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]: BV16 & UCHAR_MAX: INTEGER & UCHAR_MIN: INTEGER & dom(byte_uchar) = BYTE & ran(byte_uchar) = UCHAR & dom(uchar_byte) = UCHAR & ran(uchar_byte) = BYTE & 1 = 2**0 & 2 = 2**1 & 4 = 2**2 & 8 = 2**3 & 16 = 2**4 & 32 = 2**5 & 64 = 2**6 & 128 = 2**7 & 256 = 2**8 & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;(-2)**7 = -128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536));
  List_Assertions(Machine(USHORT_DEFINITION))==(USHORT_MAX: INTEGER & USHORT_MIN: INTEGER & dom(bv16_ushort) = BV16 & ran(bv16_ushort) = USHORT & dom(ushort_bv16) = USHORT & ran(ushort_bv16) = BV16 & dom(uchar_ushort) = UCHAR*UCHAR & ran(uchar_ushort) = USHORT & dom(ushort_uchar) = USHORT & ran(ushort_uchar) = UCHAR*UCHAR & 1 = 2**0 & 2 = 2**1 & 4 = 2**2 & 8 = 2**3 & 16 = 2**4 & 32 = 2**5 & 64 = 2**6 & 128 = 2**7 & 256 = 2**8 & 512 = 2**9 & 1024 = 2**10 & 2048 = 2**11 & 4096 = 2**12 & 8192 = 2**13 & 16384 = 2**14 & 32768 = 2**15 & 65536 = 2**16)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(USHORT_DEFINITION))==(skip);
  Context_List_Initialisation(Machine(USHORT_DEFINITION))==(skip);
  List_Initialisation(Machine(USHORT_DEFINITION))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(USHORT_DEFINITION))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(USHORT_DEFINITION),Machine(BIT_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(USHORT_DEFINITION),Machine(BIT_VECTOR_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(USHORT_DEFINITION),Machine(BYTE_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(USHORT_DEFINITION),Machine(BV16_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(USHORT_DEFINITION),Machine(UCHAR_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(USHORT_DEFINITION),Machine(POWER2))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(USHORT_DEFINITION))==(btrue);
  List_Constraints(Machine(USHORT_DEFINITION))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(USHORT_DEFINITION))==(?);
  List_Operations(Machine(USHORT_DEFINITION))==(?)
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
  List_Valuable_Constants(Machine(USHORT_DEFINITION))==(USHORT_MAX,USHORT_MIN,USHORT,bv16_ushort,ushort_bv16,uchar_ushort,ushort_uchar);
  Inherited_List_Constants(Machine(USHORT_DEFINITION))==(?);
  List_Constants(Machine(USHORT_DEFINITION))==(USHORT_MAX,USHORT_MIN,USHORT,bv16_ushort,ushort_bv16,uchar_ushort,ushort_uchar)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(USHORT_DEFINITION))==(?);
  Context_List_Defered(Machine(USHORT_DEFINITION))==(?);
  Context_List_Sets(Machine(USHORT_DEFINITION))==(?);
  List_Valuable_Sets(Machine(USHORT_DEFINITION))==(?);
  Inherited_List_Enumerated(Machine(USHORT_DEFINITION))==(?);
  Inherited_List_Defered(Machine(USHORT_DEFINITION))==(?);
  Inherited_List_Sets(Machine(USHORT_DEFINITION))==(?);
  List_Enumerated(Machine(USHORT_DEFINITION))==(?);
  List_Defered(Machine(USHORT_DEFINITION))==(?);
  List_Sets(Machine(USHORT_DEFINITION))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(USHORT_DEFINITION))==(?);
  Expanded_List_HiddenConstants(Machine(USHORT_DEFINITION))==(?);
  List_HiddenConstants(Machine(USHORT_DEFINITION))==(?);
  External_List_HiddenConstants(Machine(USHORT_DEFINITION))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(USHORT_DEFINITION))==(btrue);
  Context_List_Properties(Machine(USHORT_DEFINITION))==(BIT = 0..1 & bit_not: BIT --> BIT & bit_not = %bb.(bb: BIT | 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_bit: BOOL --> BIT & bool_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = seq1(BIT) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & phys_bv_index: BIT_VECTOR --> POW(NATURAL) & phys_bv_index = %v1.(v1: BIT_VECTOR | 0..bv_size(v1)-1) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: low..bv_size(bv)-1 | %i0.(i0: 1..high-low+1 | bv(i0+low))) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (1..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (1..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 1..bv_size(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_xor(v1(idx),v2(idx)))) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->0}) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->1}) & bv_get: BIT_VECTOR*NATURAL --> BIT & bv_get = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1(idx+1)) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 & bit: BIT | v1<+{idx+1|->bit}) & BYTE_INDEX = 1..8 & PHYS_BYTE_INDEX = 0..8-1 & BYTE = {bt | bt: BIT_VECTOR & bv_size(bt) = 8} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & is_zero: BYTE --> BIT & is_zero = %w1.(w1: BYTE | bool_bit(bool(w1(1)+w1(2)+w1(3)+w1(4)+w1(5)+w1(6)+w1(7)+w1(8) = 0))) & parity_even: BYTE --> BIT & parity_even = %bv.(bv: BYTE | 1-(bv(1)+bv(2)+bv(3)+bv(4)+bv(5)+bv(6)+bv(7)+bv(8) mod 2)) & and: BYTE*BYTE --> BYTE & and = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_and(bt1,bt2)) & ior: BYTE*BYTE --> BYTE & ior = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_or(bt1,bt2)) & xor: BYTE*BYTE --> BYTE & xor = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_xor(bt1,bt2)) & bitget: BYTE*PHYS_BYTE_INDEX --> BIT & bitget = %(bt1,ii).(bt1: BYTE & ii: PHYS_BYTE_INDEX | bt1(ii+1)) & bitset: BYTE*PHYS_BYTE_INDEX --> BYTE & bitset = %(bt1,ii).(bt1: BYTE & ii: PHYS_BYTE_INDEX | bv_set(bt1,ii)) & bitclear: BYTE*PHYS_BYTE_INDEX --> BYTE & !(ww,ii,bb).(ww: BYTE & ii: PHYS_BYTE_INDEX & bb: BIT => bitclear(ww,ii) = bv_clear(ww,ii)) & complement: BYTE --> BYTE & complement = %bt.(bt: BYTE | bv_not(bt)) & swap: BYTE --> BYTE & swap = %bt.(bt: BYTE | {1|->bt(5),2|->bt(6),3|->bt(7),4|->bt(8),5|->bt(1),6|->bt(2),7|->bt(3),8|->bt(4)}) & rotateleft: BYTE --> BYTE & rotateleft = %bv.(bv: BYTE | {1|->bv(8),2|->bv(1),3|->bv(2),4|->bv(3),5|->bv(4),6|->bv(5),7|->bv(6),8|->bv(7)}) & rotateright: BYTE --> BYTE & rotateright = %bv.(bv: BYTE | {1|->bv(2),2|->bv(3),3|->bv(4),4|->bv(5),5|->bv(6),6|->bv(7),7|->bv(8),8|->bv(1)}) & get_upper_digit = %by.(by: BYTE | 8*by(8)+4*by(7)+2*by(6)+by(5)) & get_lower_digit = %by.(by: BYTE | 8*by(4)+4*by(3)+2*by(2)+by(1)) & BV16_INDX = 1..16 & PHYS_BV16_INDEX = 0..16-1 & BV16 = {bt | bt: BIT_VECTOR & bv_size(bt) = 16} & BV16_ZERO = BV16_INDX*{0} & bv16_byte: BV16 --> BYTE*BYTE & bv16_byte = %bv.(bv: BV16 | {1|->bv(9),2|->bv(10),3|->bv(11),4|->bv(12),5|->bv(13),6|->bv(14),7|->bv(15),8|->bv(16)},{1|->bv(1),2|->bv(2),3|->bv(3),4|->bv(4),5|->bv(5),6|->bv(6),7|->bv(7),8|->bv(8)}) & byte_bv16: BYTE*BYTE --> BV16 & byte_bv16 = bv16_byte~ & bv16_bit_get: BV16*PHYS_BV16_INDEX --> BIT & bv16_bit_get = %(bt,ind).(bt: BV16 & ind: PHYS_BV16_INDEX | bt(ind+1)) & UCHAR_MAX = 256-1 & UCHAR_MIN = 0 & UCHAR = UCHAR_MIN..UCHAR_MAX & byte_uchar: BYTE --> UCHAR & byte_uchar = %v0.(v0: BYTE | 128*bitget(v0,7)+64*bitget(v0,6)+32*bitget(v0,5)+16*bitget(v0,4)+8*bitget(v0,3)+4*bitget(v0,2)+2*bitget(v0,1)+128*bitget(v0,0)) & uchar_byte: UCHAR --> BYTE & uchar_byte = byte_uchar~);
  Inherited_List_Properties(Machine(USHORT_DEFINITION))==(btrue);
  List_Properties(Machine(USHORT_DEFINITION))==(USHORT_MAX = 65536-1 & USHORT_MIN = 0 & USHORT = USHORT_MIN..USHORT_MAX & bv16_ushort: BV16 --> USHORT & bv16_ushort = %v0.(v0: BV16 | 32768*bv16_bit_get(v0,15)+16384*bv16_bit_get(v0,14)+8192*bv16_bit_get(v0,13)+4096*bv16_bit_get(v0,12)+2048*bv16_bit_get(v0,11)+1024*bv16_bit_get(v0,10)+512*bv16_bit_get(v0,9)+256*bv16_bit_get(v0,8)+128*bv16_bit_get(v0,7)+64*bv16_bit_get(v0,6)+32*bv16_bit_get(v0,5)+16*bv16_bit_get(v0,4)+8*bv16_bit_get(v0,3)+4*bv16_bit_get(v0,2)+2*bv16_bit_get(v0,1)+bv16_bit_get(v0,0)) & ushort_bv16: USHORT --> BV16 & ushort_bv16 = bv16_ushort~ & uchar_ushort: UCHAR*UCHAR --> USHORT & uchar_ushort = %(w1,w2).(w1: UCHAR & w2: UCHAR | bv16_ushort(byte_bv16(uchar_byte(w1),uchar_byte(w2)))) & ushort_uchar: USHORT --> UCHAR*UCHAR & ushort_uchar = uchar_ushort~)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(USHORT_DEFINITION),Machine(POWER2))==(?);
  Seen_Context_List_Enumerated(Machine(USHORT_DEFINITION))==(?);
  Seen_Context_List_Invariant(Machine(USHORT_DEFINITION))==(btrue);
  Seen_Context_List_Assertions(Machine(USHORT_DEFINITION))==(!bb.(bb: BIT => bit_not(bb) = 1-bb) & dom(bit_and) = BIT*BIT & ran(bit_not) = BIT & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & dom(bit_and) = BIT*BIT & ran(bit_and) = BIT & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & dom(bit_or) = BIT*BIT & ran(bit_or) = BIT & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & dom(bit_xor) = BIT*BIT & ran(bit_xor) = BIT & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & dom(bool_bit) = BOOL & ran(bit_xor) = BIT & bool_bit(TRUE) = 1 & bool_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;(-2)**7 = -128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_not(bv_not(bv)),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: 0..bv_size(bv)-1 & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,v2),indx) = bv_get(bv_and(v2,v1),indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,bv_and(v2,v3)),indx) = bv_get(bv_and(bv_and(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv_zero(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_one(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,v2),indx) = bv_get(bv_or(v2,v1),indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,bv_or(v2,v3)),indx) = bv_get(bv_or(bv_or(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_one(bv_size(bv))),indx) = bv_get(bv_one(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_xor(v1,v2),indx) = bv_get(bv_xor(v2,v1),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_xor(bv,bv),indx) = bv_get(bv_zero(bv_size(bv)),indx))) & 65536 = 2**16 & 32768 = 2**15 & 16384 = 2**14 & 8192 = 2**13 & 4096 = 2**12 & 2048 = 2**11 & 1024 = 2**10 & 512 = 2**9 & 256 = 2**8 & 128 = 2**7 & 64 = 2**6 & 32 = 2**5 & 16 = 2**4 & 8 = 2**3 & 4 = 2**2 & 2 = 2**1 & 1 = 2**0 & [0,0,0,0,0,0,0,0]: BYTE & [1,1,1,1,1,1,1,1]: BYTE & ran(get_lower_digit) = 0..16-1 & dom(get_lower_digit) = BYTE & ran(get_upper_digit) = 0..16-1 & dom(get_upper_digit) = BYTE & ran(rotateright) = BYTE & dom(rotateright) = BYTE & ran(rotateleft) = BYTE & dom(rotateleft) = BYTE & ran(swap) = BYTE & dom(swap) = BYTE & ran(complement) = BYTE & dom(complement) = BYTE & ran(bitclear) = BYTE & dom(bitclear) = BYTE*PHYS_BYTE_INDEX & ran(bitset) = BYTE & dom(bitset) = BYTE*PHYS_BYTE_INDEX & ran(bitget) = BIT & dom(bitget) = BYTE*PHYS_BYTE_INDEX & ran(xor) = BYTE & dom(xor) = BYTE*BYTE & ran(ior) = BYTE & dom(ior) = BYTE*BYTE & ran(and) = BYTE & dom(and) = BYTE*BYTE & ran(parity_even) = BIT & dom(parity_even) = BYTE & ran(is_zero) = BIT & dom(is_zero) = BYTE & 8: NATURAL & !bt.(bt: BYTE => bv_size(bt) = size(bt) & bv_size(bt) = 8 & bt: seq1(BIT)) & first(BYTE_ZERO) = 0 & BYTE_ZERO: BIT_VECTOR & BYTE <: BIT_VECTOR & size(BYTE_ZERO) = 8 & !(nn,pp).(nn: NATURAL1 & pp: NAT => (pp = 0 => nn**pp = 1) & (pp = 1 => nn**pp = nn) & (pp>1 => nn**pp = nn*nn**(pp-1))));
  Seen_Context_List_Properties(Machine(USHORT_DEFINITION))==(BIT = 0..1 & bit_not: BIT --> BIT & bit_not = %bb.(bb: BIT | 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_bit: BOOL --> BIT & bool_bit = {TRUE|->1,FALSE|->0} & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 & bit: BIT | v1<+{idx+1|->bit}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_get = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1(idx+1)) & bv_get: BIT_VECTOR*NATURAL --> BIT & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->1}) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->0}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_xor(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_or(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_and(v1(idx),v2(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 1..bv_size(v1) | bit_not(v1(idx)))) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (1..sz)*{1}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (1..sz)*{0}) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: low..bv_size(bv)-1 | %i0.(i0: 1..high-low+1 | bv(i0+low))) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & phys_bv_index = %v1.(v1: BIT_VECTOR | 0..bv_size(v1)-1) & phys_bv_index: BIT_VECTOR --> POW(NATURAL) & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_size: BIT_VECTOR --> NATURAL1 & BIT_VECTOR = seq1(BIT) & get_lower_digit = %by.(by: BYTE | 8*by(4)+4*by(3)+2*by(2)+by(1)) & get_upper_digit = %by.(by: BYTE | 8*by(8)+4*by(7)+2*by(6)+by(5)) & rotateright = %bv.(bv: BYTE | {1|->bv(2),2|->bv(3),3|->bv(4),4|->bv(5),5|->bv(6),6|->bv(7),7|->bv(8),8|->bv(1)}) & rotateright: BYTE --> BYTE & rotateleft = %bv.(bv: BYTE | {1|->bv(8),2|->bv(1),3|->bv(2),4|->bv(3),5|->bv(4),6|->bv(5),7|->bv(6),8|->bv(7)}) & rotateleft: BYTE --> BYTE & swap = %bt.(bt: BYTE | {1|->bt(5),2|->bt(6),3|->bt(7),4|->bt(8),5|->bt(1),6|->bt(2),7|->bt(3),8|->bt(4)}) & swap: BYTE --> BYTE & complement = %bt.(bt: BYTE | bv_not(bt)) & complement: BYTE --> BYTE & !(ww,ii,bb).(ww: BYTE & ii: PHYS_BYTE_INDEX & bb: BIT => bitclear(ww,ii) = bv_clear(ww,ii)) & bitclear: BYTE*PHYS_BYTE_INDEX --> BYTE & bitset = %(bt1,ii).(bt1: BYTE & ii: PHYS_BYTE_INDEX | bv_set(bt1,ii)) & bitset: BYTE*PHYS_BYTE_INDEX --> BYTE & bitget = %(bt1,ii).(bt1: BYTE & ii: PHYS_BYTE_INDEX | bt1(ii+1)) & bitget: BYTE*PHYS_BYTE_INDEX --> BIT & xor = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_xor(bt1,bt2)) & xor: BYTE*BYTE --> BYTE & ior = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_or(bt1,bt2)) & ior: BYTE*BYTE --> BYTE & and = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_and(bt1,bt2)) & and: BYTE*BYTE --> BYTE & parity_even = %bv.(bv: BYTE | 1-(bv(1)+bv(2)+bv(3)+bv(4)+bv(5)+bv(6)+bv(7)+bv(8) mod 2)) & parity_even: BYTE --> BIT & is_zero = %w1.(w1: BYTE | bool_bit(bool(w1(1)+w1(2)+w1(3)+w1(4)+w1(5)+w1(6)+w1(7)+w1(8) = 0))) & is_zero: BYTE --> BIT & BYTE_ZERO = BYTE_INDEX*{0} & BYTE_ZERO: BYTE & BYTE = {bt | bt: BIT_VECTOR & bv_size(bt) = 8} & PHYS_BYTE_INDEX = 0..8-1 & BYTE_INDEX = 1..8);
  Seen_List_Constraints(Machine(USHORT_DEFINITION))==(btrue);
  Seen_List_Operations(Machine(USHORT_DEFINITION),Machine(POWER2))==(?);
  Seen_Expanded_List_Invariant(Machine(USHORT_DEFINITION),Machine(POWER2))==(btrue);
  Seen_Internal_List_Operations(Machine(USHORT_DEFINITION),Machine(UCHAR_DEFINITION))==(?);
  Seen_List_Operations(Machine(USHORT_DEFINITION),Machine(UCHAR_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(USHORT_DEFINITION),Machine(UCHAR_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(USHORT_DEFINITION),Machine(BV16_DEFINITION))==(?);
  Seen_List_Operations(Machine(USHORT_DEFINITION),Machine(BV16_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(USHORT_DEFINITION),Machine(BV16_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(USHORT_DEFINITION),Machine(BYTE_DEFINITION))==(?);
  Seen_List_Operations(Machine(USHORT_DEFINITION),Machine(BYTE_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(USHORT_DEFINITION),Machine(BYTE_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(USHORT_DEFINITION),Machine(BIT_VECTOR_DEFINITION))==(?);
  Seen_List_Operations(Machine(USHORT_DEFINITION),Machine(BIT_VECTOR_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(USHORT_DEFINITION),Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(USHORT_DEFINITION),Machine(BIT_DEFINITION))==(?);
  Seen_List_Operations(Machine(USHORT_DEFINITION),Machine(BIT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(USHORT_DEFINITION),Machine(BIT_DEFINITION))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(USHORT_DEFINITION),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(USHORT_DEFINITION)) == (USHORT_MAX,USHORT_MIN,USHORT,bv16_ushort,ushort_bv16,uchar_ushort,ushort_uchar | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BYTE_DEFINITION)),seen(Machine(BV16_DEFINITION)),seen(Machine(UCHAR_DEFINITION)),seen(Machine(POWER2)) | ? | USHORT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(USHORT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(USHORT_DEFINITION)) == (USHORT_MAX,USHORT_MIN,USHORT,bv16_ushort,ushort_bv16,uchar_ushort,ushort_uchar);
  List_Of_VisibleVar_Ids(Machine(USHORT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(USHORT_DEFINITION)) == (?: ?);
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
  List_Of_Ids(Machine(UCHAR_DEFINITION)) == (UCHAR_MAX,UCHAR_MIN,UCHAR,uchar_byte,byte_uchar | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BYTE_DEFINITION)),seen(Machine(POWER2)) | ? | UCHAR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(UCHAR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(UCHAR_DEFINITION)) == (UCHAR_MAX,UCHAR_MIN,UCHAR,uchar_byte,byte_uchar);
  List_Of_VisibleVar_Ids(Machine(UCHAR_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(UCHAR_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_INDEX,BYTE_ZERO,PHYS_BYTE_INDEX,is_zero,parity_even,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright,get_upper_digit,get_lower_digit | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(POWER2)) | ? | BYTE_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_INDEX,BYTE_ZERO,PHYS_BYTE_INDEX,is_zero,parity_even,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright,get_upper_digit,get_lower_digit);
  List_Of_VisibleVar_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BYTE_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,phys_bv_index | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)) | ? | BIT_VECTOR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,phys_bv_index);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_bit | ? | ? | ? | ? | ? | ? | ? | BIT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_bit);
  List_Of_VisibleVar_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BV16_DEFINITION)) == (BV16_INDX,PHYS_BV16_INDEX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BYTE_DEFINITION)) | ? | BV16_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BV16_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BV16_DEFINITION)) == (BV16_INDX,PHYS_BV16_INDEX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get);
  List_Of_VisibleVar_Ids(Machine(BV16_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BV16_DEFINITION)) == (?: ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(USHORT_DEFINITION)) == (Type(USHORT_MAX) == Cst(btype(INTEGER,?,?));Type(USHORT_MIN) == Cst(btype(INTEGER,?,?));Type(USHORT) == Cst(SetOf(btype(INTEGER,"[USHORT","]USHORT")));Type(bv16_ushort) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[USHORT","]USHORT")));Type(ushort_bv16) == Cst(SetOf(btype(INTEGER,"[USHORT","]USHORT")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(uchar_ushort) == Cst(SetOf(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[USHORT","]USHORT")));Type(ushort_uchar) == Cst(SetOf(btype(INTEGER,"[USHORT","]USHORT")*(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[UCHAR","]UCHAR")))))
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
  variant_clause_mandatory == KO
END
)
