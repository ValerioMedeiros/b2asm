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
  List_Sees(Machine(ALU))==(TYPES,BIT_DEFINITION,BIT_VECTOR_DEFINITION,BYTE_DEFINITION,BIT_VECTOR_ARITHMETICS,BIT_VECTOR16_DEFINITION)
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
  Context_List_Assertions(Machine(ALU))==(dom(byte_schar) = BYTE & ran(byte_schar) <: SCHAR & dom(schar_byte) = SCHAR & ran(schar_byte) <: BYTE & dom(byte_uchar) = BYTE & ran(byte_uchar) <: UCHAR & dom(uchar_byte) = UCHAR & ran(uchar_byte) <: BYTE & dom(uchar_schar) = UCHAR & ran(uchar_schar) <: SCHAR & dom(schar_uchar) = SCHAR & ran(schar_uchar) <: UCHAR & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_to_bit(TRUE) = 1 & bool_to_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_not(bv_not(bv))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_and(v1,v2)(indx) = bv_and(v2,v1)(indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_and(v1,bv_and(v2,v3))(indx) = bv_and(bv_and(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_zero(bv_size(bv)))(indx) = bv_zero(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_one(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_or(v1,v2)(indx) = bv_or(v2,v1)(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_or(v1,bv_or(v2,v3))(indx) = bv_or(bv_or(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_one(bv_size(bv)))(indx) = bv_one(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_zero(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_xor(v1,v2)(indx) = bv_xor(v2,v1)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_xor(bv,bv)(indx) = bv_zero(bv_size(bv))(indx))) & !bt.(bt: BYTE => size(bt) = 8) & size(BYTE_ZERO) = 8 & BYTE <: BIT_VECTOR & BYTE_ZERO: BIT_VECTOR & first(BYTE_ZERO) = 0 & !ss.(ss: NATURAL1 => bv_to_nat(bv_zero(ss)) = 0) & !ss.(ss: NATURAL1 => bv_par(bv_zero(ss)) = 0) & BV16_ZERO: BV16 & BV16 <: BIT_VECTOR & BV16_ZERO: BIT_VECTOR);
  List_Assertions(Machine(ALU))==(dom(parity) = BYTE & ran(parity) <: BIT & dom(add) = UCHAR*UCHAR*UCHAR & ran(add) <: UCHAR*BIT*BIT*BIT*BIT & dom(sub) = UCHAR*UCHAR*UCHAR & ran(sub) <: UCHAR*BIT*BIT*BIT*BIT & dom(and) = BYTE*BYTE & ran(and) <: BYTE & dom(ior) = BYTE*BYTE & ran(ior) <: BYTE & dom(xor) = BYTE*BYTE & ran(xor) <: BYTE & dom(complement) = BYTE & ran(complement) <: BYTE & dom(swap) = BYTE & ran(swap) <: BYTE & dom(rotateleft) = BYTE & ran(rotateleft) <: BYTE & dom(rotateright) = BYTE & ran(rotateright) <: BYTE)
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
  List_Instanciated_Parameters(Machine(ALU),Machine(TYPES))==(?);
  List_Instanciated_Parameters(Machine(ALU),Machine(BIT_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(ALU),Machine(BIT_VECTOR_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(ALU),Machine(BYTE_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(ALU),Machine(BIT_VECTOR_ARITHMETICS))==(?);
  List_Instanciated_Parameters(Machine(ALU),Machine(BIT_VECTOR16_DEFINITION))==(?)
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
  List_Valuable_Constants(Machine(ALU))==(parity,half,add,sub,and,and_bit,ior,xor,complement,swap,rotateleft,rotateright,usint_byte);
  Inherited_List_Constants(Machine(ALU))==(?);
  List_Constants(Machine(ALU))==(parity,half,add,sub,and,and_bit,ior,xor,complement,swap,rotateleft,rotateright,usint_byte)
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
  Context_List_Properties(Machine(ALU))==(UCHAR_MAX: NATURAL & UCHAR_MAX = 2**BYTE_WIDTH-1 & UCHAR = 0..UCHAR_MAX & SCHAR_MAX: INTEGER & SCHAR_MIN: INTEGER & SCHAR_MAX = 2**(BYTE_WIDTH-1)-1 & SCHAR_MIN = -(2**(BYTE_WIDTH-1)) & SCHAR = SCHAR_MIN..SCHAR_MAX & USHORT_INT_MAX: INTEGER & USHORT_INT_MAX = 2**(2*BYTE_WIDTH)-1 & USHORT_INT = 0..USHORT_INT_MAX & byte_uchar: BYTE --> UCHAR & byte_uchar = %bv.(bv: BYTE | bv_to_nat(bv)) & uchar_byte = byte_uchar~ & byte_schar: BYTE --> SCHAR & byte_schar = %bv.(bv: BYTE | (-bv(7))*128+bv(6)*64+bv(5)*32+bv(4)*16+bv(3)*8+bv(2)*4+bv(1)*2+bv(0)*1) & schar_byte = byte_schar~ & uchar_schar: UCHAR --> SCHAR & uchar_schar = %v1.(v1: UCHAR & v1<=SCHAR_MAX | v1) & uchar_schar = %v1.(v1: UCHAR & not(v1<=SCHAR_MAX) | v1-UCHAR_MAX) & schar_uchar = uchar_schar~ & bv16_usint: BV16 --> USHORT_INT & bv16_usint = %v0.(v0: BV16 | 32768*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & usint_bv16 = bv16_usint~ & BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = {bt | bt: NATURAL +-> BIT}-{{}} & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & BV_INDX: BIT_VECTOR --> POW(NATURAL) & BV_INDX = %bv.(bv: BIT_VECTOR | 0..bv_size(bv)-1) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high | low..high<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (0..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (0..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: BV_INDX(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1(idx)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: BV_INDX(v1) & bit: BIT | v1<+{idx|->bit}) & BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE = {bt | bt: BYTE_INDEX --> BIT & size(bt) = BYTE_WIDTH}-{{}} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & bv_to_nat: BIT_VECTOR --> NATURAL & bv_to_nat = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | 2**idx*bv(idx))) & bv_par: BIT_VECTOR --> BIT & bv_par = %bv.(bv: BIT_VECTOR | size(bv|>{1}) mod 2) & BV16_WIDTH: NATURAL & BV16_WIDTH = 16 & BV16_INDX = 0..BV16_WIDTH-1 & BV16 = {bt | bt: BV16_INDX --> BIT & size(bt) = 16}-{{}} & BV16_ZERO = BV16_INDX*{0} & bv16_byte: BV16 --> BYTE*BYTE & bv16_byte = %bv.(bv: BV16 | {0|->bv(8),1|->bv(9),2|->bv(10),3|->bv(11),4|->bv(12),5|->bv(13),6|->bv(14),7|->bv(15)},{0|->bv(0),1|->bv(1),2|->bv(2),3|->bv(3),4|->bv(4),5|->bv(5),6|->bv(6),7|->bv(7)}));
  Inherited_List_Properties(Machine(ALU))==(btrue);
  List_Properties(Machine(ALU))==(parity: BYTE --> BIT & parity = %bv.(bv: BYTE | bv_par(bv)) & half: UCHAR --> UCHAR & half = %v1.(v1: UCHAR | v1 mod 2**4) & add: UCHAR*UCHAR*BIT --> UCHAR*BIT*BIT*BIT*BIT & add = %(v1,v2,ca).(v1: UCHAR & v2: UCHAR & ca: UCHAR | (v1+v2+ca) mod (UCHAR_MAX+1),parity(uchar_byte((v1+v2+ca) mod (UCHAR_MAX+1))),bool_to_bit(bool(uchar_schar(v1)+uchar_schar(v2)+ca/:SCHAR)),bool_to_bit(bool(half(v1)+half(v2)+ca>=2**4)),bool_to_bit(bool(v1+v2+ca/:UCHAR))) & sub: UCHAR*UCHAR*UCHAR --> UCHAR*BIT*BIT*BIT*BIT & sub = %(v1,v2,ca).(v1: UCHAR & v2: UCHAR & ca: UCHAR | bool_to_bit(bool(v1<v2+ca))*256+v1-v2-ca,parity(uchar_byte(bool_to_bit(bool(v1<v2+ca))*256+v1-v2-ca)),bool_to_bit(bool(uchar_schar(v1)-uchar_schar(v2)-ca/:SCHAR)),bool_to_bit(bool(half(v1)<half(v2)-ca)),bool_to_bit(bool(v1-v2-ca/:UCHAR))) & and: BYTE*BYTE --> BYTE & and = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_and(bt1,bt2)) & and_bit: BIT*BIT --> BIT & and_bit = %(b1,b2).(b1: BIT & b2: BIT | bit_and(b1,b2)) & ior: BYTE*BYTE --> BYTE & ior = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_or(bt1,bt2)) & xor: BYTE*BYTE --> BYTE & xor = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_xor(bt1,bt2)) & complement: BYTE --> BYTE & complement = %bt.(bt: BYTE | bv_not(bt)) & swap: BYTE --> BYTE & swap = %bt.(bt: BYTE | {0|->bt(4),1|->bt(5),2|->bt(6),3|->bt(7),4|->bt(0),5|->bt(1),6|->bt(2),7|->bt(3)}) & rotateleft: BYTE --> BYTE & rotateleft = %bv.(bv: BYTE | {0|->bv(7),1|->bv(0),2|->bv(1),3|->bv(2),4|->bv(3),5|->bv(4),6|->bv(5),7|->bv(6)}) & rotateright: BYTE --> BYTE & rotateright = %bv.(bv: BYTE | {0|->bv(1),1|->bv(2),2|->bv(3),3|->bv(4),4|->bv(5),5|->bv(6),6|->bv(7),7|->bv(0)}) & usint_byte: USHORT_INT --> BYTE*BYTE & usint_byte = %usint.(usint: USHORT_INT | bv16_byte(usint_bv16(usint))))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(ALU),Machine(BIT_VECTOR16_DEFINITION))==(?);
  Seen_Context_List_Enumerated(Machine(ALU))==(?);
  Seen_Context_List_Invariant(Machine(ALU))==(btrue);
  Seen_Context_List_Assertions(Machine(ALU))==(!bt.(bt: BYTE => size(bt) = 8) & size(BYTE_ZERO) = 8 & BYTE <: BIT_VECTOR & BYTE_ZERO: BIT_VECTOR & first(BYTE_ZERO) = 0 & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_to_bit(TRUE) = 1 & bool_to_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & !ss.(ss: NATURAL1 => bv_to_nat(bv_zero(ss)) = 0) & !ss.(ss: NATURAL1 => bv_par(bv_zero(ss)) = 0) & BV16_ZERO: BV16 & BV16 <: BIT_VECTOR & BV16_ZERO: BIT_VECTOR & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_not(bv_not(bv))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_and(v1,v2)(indx) = bv_and(v2,v1)(indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_and(v1,bv_and(v2,v3))(indx) = bv_and(bv_and(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_zero(bv_size(bv)))(indx) = bv_zero(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_one(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_or(v1,v2)(indx) = bv_or(v2,v1)(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_or(v1,bv_or(v2,v3))(indx) = bv_or(bv_or(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_one(bv_size(bv)))(indx) = bv_one(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_zero(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_xor(v1,v2)(indx) = bv_xor(v2,v1)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_xor(bv,bv)(indx) = bv_zero(bv_size(bv))(indx))));
  Seen_Context_List_Properties(Machine(ALU))==(BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE = {bt | bt: BYTE_INDEX --> BIT & size(bt) = BYTE_WIDTH}-{{}} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & bv_to_nat: BIT_VECTOR --> NATURAL & bv_to_nat = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | 2**idx*bv(idx))) & bv_par: BIT_VECTOR --> BIT & bv_par = %bv.(bv: BIT_VECTOR | size(bv|>{1}) mod 2) & BV16_WIDTH: NATURAL & BV16_WIDTH = 16 & BV16_INDX = 0..BV16_WIDTH-1 & BV16 = {bt | bt: BV16_INDX --> BIT & size(bt) = 16}-{{}} & BV16_ZERO = BV16_INDX*{0} & bv16_byte: BV16 --> BYTE*BYTE & bv16_byte = %bv.(bv: BV16 | {0|->bv(8),1|->bv(9),2|->bv(10),3|->bv(11),4|->bv(12),5|->bv(13),6|->bv(14),7|->bv(15)},{0|->bv(0),1|->bv(1),2|->bv(2),3|->bv(3),4|->bv(4),5|->bv(5),6|->bv(6),7|->bv(7)}) & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: BV_INDX(v1) & bit: BIT | v1<+{idx|->bit}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->0}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->1}) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1(idx)) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_xor(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_or(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_and(v1(idx),v2(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: BV_INDX(v1) | bit_not(v1(idx)))) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (0..sz)*{1}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (0..sz)*{0}) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high | low..high<|bv) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & BV_INDX = %bv.(bv: BIT_VECTOR | 0..bv_size(bv)-1) & BV_INDX: BIT_VECTOR --> POW(NATURAL) & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_size: BIT_VECTOR --> NATURAL1 & BIT_VECTOR = {bt | bt: NATURAL +-> BIT}-{{}});
  Seen_List_Constraints(Machine(ALU))==(btrue);
  Seen_List_Operations(Machine(ALU),Machine(BIT_VECTOR16_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(BIT_VECTOR16_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(ALU),Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Seen_List_Operations(Machine(ALU),Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(BIT_VECTOR_ARITHMETICS))==(btrue);
  Seen_Internal_List_Operations(Machine(ALU),Machine(BYTE_DEFINITION))==(?);
  Seen_List_Operations(Machine(ALU),Machine(BYTE_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(BYTE_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(ALU),Machine(BIT_VECTOR_DEFINITION))==(?);
  Seen_List_Operations(Machine(ALU),Machine(BIT_VECTOR_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(ALU),Machine(BIT_DEFINITION))==(?);
  Seen_List_Operations(Machine(ALU),Machine(BIT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(BIT_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(ALU),Machine(TYPES))==(?);
  Seen_List_Operations(Machine(ALU),Machine(TYPES))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(TYPES))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(ALU),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(ALU)) == (parity,half,add,sub,and,and_bit,ior,xor,complement,swap,rotateleft,rotateright,usint_byte | ? | ? | ? | ? | ? | seen(Machine(TYPES)),seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BYTE_DEFINITION)),seen(Machine(BIT_VECTOR_ARITHMETICS)),seen(Machine(BIT_VECTOR16_DEFINITION)) | ? | ALU);
  List_Of_HiddenCst_Ids(Machine(ALU)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ALU)) == (parity,half,add,sub,and,and_bit,ior,xor,complement,swap,rotateleft,rotateright,usint_byte);
  List_Of_VisibleVar_Ids(Machine(ALU)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ALU)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR16_DEFINITION)) == (BV16_INDX,BV16_WIDTH,BV16_ZERO,BV16,bv16_byte | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BYTE_DEFINITION)) | ? | BIT_VECTOR16_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR16_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR16_DEFINITION)) == (BV16_INDX,BV16_WIDTH,BV16_ZERO,BV16,bv16_byte);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR16_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR16_DEFINITION)) == (?: ?);
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
  List_Of_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (bv_to_nat,bv_par | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(POWER2)) | ? | BIT_VECTOR_ARITHMETICS);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (bv_to_nat,bv_par);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR_ARITHMETICS)) == (?: ?);
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
  List_Of_Ids(Machine(TYPES)) == (UCHAR,UCHAR_MAX,SCHAR,SCHAR_MAX,SCHAR_MIN,USHORT_INT,USHORT_INT_MAX,byte_schar,schar_byte,byte_uchar,uchar_byte,uchar_schar,schar_uchar,bv16_usint,usint_bv16 | ? | ? | ? | ? | ? | seen(Machine(BYTE_DEFINITION)),seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_ARITHMETICS)),seen(Machine(BIT_VECTOR16_DEFINITION)),seen(Machine(POWER2)) | ? | TYPES);
  List_Of_HiddenCst_Ids(Machine(TYPES)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (UCHAR,UCHAR_MAX,SCHAR,SCHAR_MAX,SCHAR_MIN,USHORT_INT,USHORT_INT_MAX,byte_schar,schar_byte,byte_uchar,uchar_byte,uchar_schar,schar_uchar,bv16_usint,usint_bv16);
  List_Of_VisibleVar_Ids(Machine(TYPES)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPES)) == (?: ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(ALU)) == (Type(parity) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BIT","]BIT")));Type(half) == Cst(SetOf(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[UCHAR","]UCHAR")));Type(add) == Cst(SetOf(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[BIT","]BIT")*(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT"))));Type(sub) == Cst(SetOf(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[UCHAR","]UCHAR")*(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT"))));Type(and) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(and_bit) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(ior) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(xor) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(complement) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(swap) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(rotateleft) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(rotateright) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(usint_byte) == Cst(SetOf(btype(INTEGER,"[USHORT_INT","]USHORT_INT")*(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))))))
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
