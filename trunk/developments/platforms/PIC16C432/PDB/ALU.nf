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
  List_Sees(Machine(ALU))==(TYPES,BIT_DEFINITION,BIT_VECTOR_DEFINITION,BIT_VECTOR_ARITHMETICS,BYTE_DEFINITION)
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
  Context_List_Assertions(Machine(ALU))==((NB_UCHARS = 256;!nn.(nn: UCHAR => 0<=nn);!nn.(nn: UCHAR => nn<=255);REGISTER = UCHAR;ran(byte_to_uchar) = UCHAR) & (bit_not(0) = 1;bit_not(1) = 0;!bb.(bb: BIT => bit_not(bit_not(bb)) = bb);bit_and(0,0) = 0;bit_and(0,1) = 0;bit_and(1,0) = 0;bit_and(1,1) = 1;!(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3));!b1.(b1: BIT => bit_and(b1,1) = b1);!b1.(b1: BIT => bit_and(b1,0) = 0);bit_or(0,0) = 0;bit_or(0,1) = 0;bit_or(1,0) = 0;bit_or(1,1) = 1;!(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3));!b1.(b1: BIT => bit_or(b1,1) = 1);!b1.(b1: BIT => bit_or(b1,0) = b1);bit_xor(0,0) = 0;bit_xor(0,1) = 1;bit_xor(1,0) = 1;bit_xor(1,1) = 0;!(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3));!bb.(bb: BIT => bit_xor(bb,bb) = 0);bool_to_bit(TRUE) = 1;bool_to_bit(FALSE) = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!bv.(bv: BIT_VECTOR => bv_not(bv_not(bv)) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: NATURAL & high: NATURAL & low<=high => bv_size(bv_sub(bv,low,high)) = 1+high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_and(v1,v2) = bv_and(v2,v1));!(v1,v2,v3).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) => bv_and(v1,bv_and(v2,v3)) = bv_and(bv_and(v1,v2),v3));!bv.(bv: BIT_VECTOR => bv_and(bv,bv_zero(bv_size(bv))) = bv_zero(bv_size(bv)));!bv.(bv: BIT_VECTOR => bv_and(bv,bv_one(bv_size(bv))) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_or(v1,v2) = bv_or(v2,v1));!(v1,v2,v3).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) => bv_or(v1,bv_or(v2,v3)) = bv_or(bv_or(v1,v2),v3));!bv.(bv: BIT_VECTOR => bv_or(bv,bv_one(bv_size(bv))) = bv_one(bv_size(bv)));!bv.(bv: BIT_VECTOR => bv_or(bv,bv_zero(bv_size(bv))) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_xor(v1,v2) = bv_xor(v2,v1));!bv.(bv: BIT_VECTOR => bv_xor(bv,bv) = bv_zero(bv_size(bv)))) & !ss.(ss: NATURAL1 => bv_to_nat(bv_zero(ss)) = 0));
  List_Assertions(Machine(ALU))==(dom(add) = UCHAR*UCHAR;ran(add) <: UCHAR*BOOL*BOOL*BOOL;dom(substract) = UCHAR*UCHAR;ran(substract) <: UCHAR*BOOL*BOOL*BOOL;dom(and) = BYTE*BYTE;ran(and) <: BYTE*BOOL;dom(ior) = BYTE*BYTE;ran(ior) <: BYTE*BOOL;dom(xor) = BYTE*BYTE;ran(xor) <: BYTE*BOOL;dom(bitclear) = BYTE*BYTE_INDEX;ran(bitclear) <: BYTE;dom(bitset) = BYTE*BYTE_INDEX;ran(bitset) <: BYTE;dom(bitget) = BYTE*BYTE_INDEX;ran(bitget) <: BIT;dom(complement) = BYTE;ran(complement) <: BYTE;dom(swap) = BYTE;ran(swap) <: BYTE;ran(rotateleft) <: BYTE*BOOL;dom(rotateleft) = BYTE;dom(rotateright) = BYTE;ran(rotateright) <: BYTE*BOOL;add(0,0) = 0,TRUE,FALSE,FALSE;add(255,1) = 0,TRUE,FALSE,TRUE;add(255,1) = 0,TRUE,TRUE,TRUE)
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
  List_Instanciated_Parameters(Machine(ALU),Machine(BIT_VECTOR_ARITHMETICS))==(?);
  List_Instanciated_Parameters(Machine(ALU),Machine(BYTE_DEFINITION))==(?)
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
THEORY ListPreconditionX END
&
THEORY ListSubstitutionX END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(ALU))==(half,add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright);
  Inherited_List_Constants(Machine(ALU))==(?);
  List_Constants(Machine(ALU))==(half,add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright)
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
  Context_List_Properties(Machine(ALU))==(UCHAR_LENGTH: NATURAL & NB_UCHARS: NATURAL & INST_SZ: NATURAL & NB_INSTRUCTIONS: NATURAL & UCHAR_LENGTH = 8 & NB_UCHARS = 2**UCHAR_LENGTH & UCHAR = 0..NB_UCHARS-1 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = 0..INSTRUCTION_MAX & instruction_next: INSTRUCTION --> INSTRUCTION & instruction_next = {pp,qq | pp: INSTRUCTION & qq: INSTRUCTION & 0<=pp & pp<NB_INSTRUCTIONS-1 & qq = pp+1}\/{NB_INSTRUCTIONS-1|->0} & byte_to_uchar: BYTE --> UCHAR & byte_to_uchar = %vv.(vv: BYTE | bv_to_nat(vv)) & uchar_to_byte: UCHAR --> BYTE & uchar_to_byte = byte_to_uchar~ & REGISTER = 0..255 & REGISTER0 = 0..127 & REGISTER1 = 128..255 & BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = seq1(BIT) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v2^v1) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: NATURAL & high: NATURAL & low<=high | 0..high-low<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (1..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (1..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 1..size(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 1..size(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 1..size(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 1..size(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1(idx+1)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1<+{idx+1|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1<+{idx+1|->0}) & bv_to_nat: BIT_VECTOR --> NATURAL & bv_to_nat = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | 2**idx*bv(idx))) & BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE <: BIT_VECTOR & BYTE = {vv | vv: BIT_VECTOR & bv_size(vv) = BYTE_WIDTH} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0});
  Inherited_List_Properties(Machine(ALU))==(btrue);
  List_Properties(Machine(ALU))==(half: UCHAR --> UCHAR & half = %ww.(ww: UCHAR | ww mod 2**4) & add: UCHAR*UCHAR --> UCHAR*BOOL*BOOL*BOOL & add = %(w1,w2).(w1: UCHAR & w2: UCHAR | (w1+w2) mod 2**8,bool(w1+w2>=2**8),bool(half(w1)+half(w2)>=2**4),bool(w1+w2: {0,256})) & substract: UCHAR*UCHAR --> UCHAR*BOOL*BOOL*BOOL & substract = %(w1,w2).(w1: UCHAR & w2: UCHAR & w1>=w2 | w1-w2,TRUE,bool(half(w1)>=half(w2)),bool(w1 = w2))\/%(w1,w2).(w1: UCHAR & w2: UCHAR & w1<w2 | 2**8+w1-w2,FALSE,bool(half(w1)>=half(w2)),FALSE) & and: BYTE*BYTE --> BYTE*BOOL & !(w1,w2,ww).(w1: BYTE & w2: BYTE & ww: BYTE & ww = bv_and(w1,w2) => and(w1,w2) = ww,bool(bv_to_nat(ww) = 0)) & ior: BYTE*BYTE --> BYTE*BOOL & !(w1,w2,ww).(w1: BYTE & w2: BYTE & ww: BYTE & ww = bv_or(w1,w2) => ior(w1,w2) = ww,bool(bv_to_nat(ww) = 0)) & xor: BYTE*BYTE --> BYTE*BOOL & !(w1,w2,ww).(w1: BYTE & w2: BYTE & ww: BYTE => (ww = bv_xor(w1,w2) => xor(w1,w2) = ww,bool(bv_to_nat(ww) = 0))) & bitget: BYTE*BYTE_INDEX --> BIT & !(ww,ii).(ww: BYTE & ii: BYTE_INDEX => bitget(ww,ii) = ww(ii)) & bitset: BYTE*BYTE_INDEX --> BYTE & !(ww,ii).(ww: BYTE & ii: BYTE_INDEX => bitset(ww,ii) = bv_set(ww,ii)) & bitclear: BYTE*BYTE_INDEX --> BYTE & !(ww,ii,bb).(ww: BYTE & ii: BYTE_INDEX & bb: BIT => bitclear(ww,ii) = bv_clear(ww,ii)) & complement: BYTE --> BYTE & !ww.(ww: BYTE => complement(ww) = bv_not(ww)) & swap: BYTE --> BYTE & !ww.(ww: BYTE => swap(ww) = {0|->ww(4),1|->ww(5),2|->ww(6),3|->ww(7),4|->ww(0),5|->ww(1),6|->ww(2),7|->ww(3)}) & rotateleft: BYTE --> BYTE*BOOL & !ww.(ww: BYTE => rotateleft(ww) = {0|->ww(7),1|->ww(0),2|->ww(1),3|->ww(2),4|->ww(3),5|->ww(4),6|->ww(5),7|->ww(6)},bool(ww(7) = 1)) & rotateright: BYTE --> BYTE*BOOL & !ww.(ww: BYTE => rotateright(ww) = {0|->ww(1),1|->ww(2),2|->ww(3),3|->ww(4),4|->ww(5),5|->ww(6),6|->ww(7),7|->ww(0)},bool(ww(0) = 1)))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(ALU),Machine(BYTE_DEFINITION))==(?);
  Seen_Context_List_Enumerated(Machine(ALU))==(?);
  Seen_Context_List_Invariant(Machine(ALU))==(btrue);
  Seen_Context_List_Assertions(Machine(ALU))==((bit_not(0) = 1;bit_not(1) = 0;!bb.(bb: BIT => bit_not(bit_not(bb)) = bb);bit_and(0,0) = 0;bit_and(0,1) = 0;bit_and(1,0) = 0;bit_and(1,1) = 1;!(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3));!b1.(b1: BIT => bit_and(b1,1) = b1);!b1.(b1: BIT => bit_and(b1,0) = 0);bit_or(0,0) = 0;bit_or(0,1) = 0;bit_or(1,0) = 0;bit_or(1,1) = 1;!(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3));!b1.(b1: BIT => bit_or(b1,1) = 1);!b1.(b1: BIT => bit_or(b1,0) = b1);bit_xor(0,0) = 0;bit_xor(0,1) = 1;bit_xor(1,0) = 1;bit_xor(1,1) = 0;!(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3));!bb.(bb: BIT => bit_xor(bb,bb) = 0);bool_to_bit(TRUE) = 1;bool_to_bit(FALSE) = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!bv.(bv: BIT_VECTOR => bv_not(bv_not(bv)) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: NATURAL & high: NATURAL & low<=high => bv_size(bv_sub(bv,low,high)) = 1+high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_and(v1,v2) = bv_and(v2,v1));!(v1,v2,v3).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) => bv_and(v1,bv_and(v2,v3)) = bv_and(bv_and(v1,v2),v3));!bv.(bv: BIT_VECTOR => bv_and(bv,bv_zero(bv_size(bv))) = bv_zero(bv_size(bv)));!bv.(bv: BIT_VECTOR => bv_and(bv,bv_one(bv_size(bv))) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_or(v1,v2) = bv_or(v2,v1));!(v1,v2,v3).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) => bv_or(v1,bv_or(v2,v3)) = bv_or(bv_or(v1,v2),v3));!bv.(bv: BIT_VECTOR => bv_or(bv,bv_one(bv_size(bv))) = bv_one(bv_size(bv)));!bv.(bv: BIT_VECTOR => bv_or(bv,bv_zero(bv_size(bv))) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_xor(v1,v2) = bv_xor(v2,v1));!bv.(bv: BIT_VECTOR => bv_xor(bv,bv) = bv_zero(bv_size(bv)))) & !ss.(ss: NATURAL1 => bv_to_nat(bv_zero(ss)) = 0) & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536));
  Seen_Context_List_Properties(Machine(ALU))==(BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = seq1(BIT) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v2^v1) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: NATURAL & high: NATURAL & low<=high | 0..high-low<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (1..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (1..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 1..size(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 1..size(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 1..size(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 1..size(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1(idx+1)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1<+{idx+1|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1<+{idx+1|->0}) & bv_to_nat: BIT_VECTOR --> NATURAL & bv_to_nat = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | 2**idx*bv(idx))) & BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE <: BIT_VECTOR & BYTE = {vv | vv: BIT_VECTOR & bv_size(vv) = BYTE_WIDTH} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0});
  Seen_List_Constraints(Machine(ALU))==(btrue);
  Seen_List_Operations(Machine(ALU),Machine(BYTE_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(BYTE_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(ALU),Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Seen_List_Operations(Machine(ALU),Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(BIT_VECTOR_ARITHMETICS))==(btrue);
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
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(ALU)) == (half,add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright | ? | ? | ? | ? | ? | seen(Machine(TYPES)),seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BIT_VECTOR_ARITHMETICS)),seen(Machine(BYTE_DEFINITION)) | ? | ALU);
  List_Of_HiddenCst_Ids(Machine(ALU)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ALU)) == (half,add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright);
  List_Of_VisibleVar_Ids(Machine(ALU)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ALU)) == (?: ?);
  List_Of_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)) | ? | BYTE_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO);
  List_Of_VisibleVar_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BYTE_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_size,bv_catenate,bv_sub,bv_zero,bv_one,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)) | ? | BIT_VECTOR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_size,bv_catenate,bv_sub,bv_zero,bv_one,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ? | BIT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit);
  List_Of_VisibleVar_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (bv_to_nat | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)) | ? | BIT_VECTOR_ARITHMETICS);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (bv_to_nat);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR_ARITHMETICS)) == (?: ?);
  List_Of_Ids(Machine(TYPES)) == (UCHAR_LENGTH,UCHAR,NB_UCHARS,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,byte_to_uchar,uchar_to_byte,REGISTER,REGISTER0,REGISTER1 | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BIT_VECTOR_ARITHMETICS)),seen(Machine(BYTE_DEFINITION)),seen(Machine(POWER2)) | ? | TYPES);
  List_Of_HiddenCst_Ids(Machine(TYPES)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (UCHAR_LENGTH,UCHAR,NB_UCHARS,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,byte_to_uchar,uchar_to_byte,REGISTER,REGISTER0,REGISTER1);
  List_Of_VisibleVar_Ids(Machine(TYPES)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPES)) == (?: ?);
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
  Constants(Machine(ALU)) == (Type(half) == Cst(SetOf(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[UCHAR","]UCHAR")));Type(add) == Cst(SetOf(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[UCHAR","]UCHAR")*(btype(INTEGER,"[UCHAR","]UCHAR")*btype(BOOL,0,1)*btype(BOOL,0,1)*btype(BOOL,0,1))));Type(substract) == Cst(SetOf(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[UCHAR","]UCHAR")*(btype(INTEGER,"[UCHAR","]UCHAR")*btype(BOOL,0,1)*btype(BOOL,0,1)*btype(BOOL,0,1))));Type(and) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(BOOL,0,1))));Type(ior) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(BOOL,0,1))));Type(xor) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(BOOL,0,1))));Type(bitclear) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BYTE_INDEX","]BYTE_INDEX")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bitset) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BYTE_INDEX","]BYTE_INDEX")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bitget) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BYTE_INDEX","]BYTE_INDEX")*btype(INTEGER,"[BIT","]BIT")));Type(complement) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(swap) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(rotateleft) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(BOOL,0,1))));Type(rotateright) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(BOOL,0,1)))))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO
END
)
