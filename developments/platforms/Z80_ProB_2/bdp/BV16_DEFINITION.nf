﻿Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(BV16_DEFINITION))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(BV16_DEFINITION))==(Machine(BV16_DEFINITION));
  Level(Machine(BV16_DEFINITION))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(BV16_DEFINITION)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(BV16_DEFINITION))==(BIT_DEFINITION,BYTE_DEFINITION)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(BV16_DEFINITION))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(BV16_DEFINITION))==(?);
  List_Includes(Machine(BV16_DEFINITION))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(BV16_DEFINITION))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(BV16_DEFINITION))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(BV16_DEFINITION))==(?);
  Context_List_Variables(Machine(BV16_DEFINITION))==(?);
  Abstract_List_Variables(Machine(BV16_DEFINITION))==(?);
  Local_List_Variables(Machine(BV16_DEFINITION))==(?);
  List_Variables(Machine(BV16_DEFINITION))==(?);
  External_List_Variables(Machine(BV16_DEFINITION))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(BV16_DEFINITION))==(?);
  Abstract_List_VisibleVariables(Machine(BV16_DEFINITION))==(?);
  External_List_VisibleVariables(Machine(BV16_DEFINITION))==(?);
  Expanded_List_VisibleVariables(Machine(BV16_DEFINITION))==(?);
  List_VisibleVariables(Machine(BV16_DEFINITION))==(?);
  Internal_List_VisibleVariables(Machine(BV16_DEFINITION))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(BV16_DEFINITION))==(btrue);
  Gluing_List_Invariant(Machine(BV16_DEFINITION))==(btrue);
  Expanded_List_Invariant(Machine(BV16_DEFINITION))==(btrue);
  Abstract_List_Invariant(Machine(BV16_DEFINITION))==(btrue);
  Context_List_Invariant(Machine(BV16_DEFINITION))==(btrue);
  List_Invariant(Machine(BV16_DEFINITION))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(BV16_DEFINITION))==(btrue);
  Abstract_List_Assertions(Machine(BV16_DEFINITION))==(btrue);
  Context_List_Assertions(Machine(BV16_DEFINITION))==(!bb.(bb: BIT => bit_not(bb) = 1-bb) & dom(bit_and) = BIT*BIT & ran(bit_not) = BIT & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & dom(bit_and) = BIT*BIT & ran(bit_and) = BIT & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & dom(bit_or) = BIT*BIT & ran(bit_or) = BIT & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & dom(bit_xor) = BIT*BIT & ran(bit_xor) = BIT & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & dom(bool_bit) = BOOL & ran(bit_xor) = BIT & bool_bit(TRUE) = 1 & bool_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (card(BYTE) = 256 & is_zero: BYTE --> BIT;parity_even: BYTE --> BIT;bv8_and: BYTE*BYTE --> BYTE;bv8_or: BYTE*BYTE --> BYTE;bv8_xor: BYTE*BYTE --> BYTE;complement: BYTE --> BYTE;swap: BYTE --> BYTE;rotateleft: BYTE --> BYTE;rotateright: BYTE --> BYTE;!bt.(bt: BYTE => size(bt) = 8);8: NATURAL;dom(is_zero) = BYTE;ran(is_zero) <: BIT;dom(parity_even) = BYTE;ran(parity_even) <: BIT;bv8_and: BYTE*BYTE --> BYTE;dom(complement) = BYTE;ran(complement) <: BYTE;dom(swap) = BYTE;ran(swap) <: BYTE;dom(rotateleft) = BYTE;ran(rotateleft) <: BYTE;dom(rotateright) = BYTE;ran(rotateright) <: BYTE;dom(get_upper_digit) = BYTE;ran(get_upper_digit) <: 0..16-1;dom(get_lower_digit) = BYTE;ran(get_lower_digit) <: 0..16-1;[1,1,1,1,1,1,1,1]: BYTE & [0,0,0,0,0,0,0,0]: BYTE & 1 = 2**0 & 2 = 2**1 & 4 = 2**2 & 8 = 2**3 & 16 = 2**4 & 32 = 2**5 & 64 = 2**6 & 128 = 2**7 & 256 = 2**8 & 512 = 2**9 & 1024 = 2**10 & 2048 = 2**11 & 4096 = 2**12 & 8192 = 2**13 & 16384 = 2**14 & 32768 = 2**15 & 65536 = 2**16));
  List_Assertions(Machine(BV16_DEFINITION))==(card(BV16) = 65536 & complement16: BV16 --> BV16 & byte_bv16 = bv16_byte~ & [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]: BV16 & [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]: BV16)
END
&
THEORY ListCoverageX IS
  List_Coverage(Machine(BV16_DEFINITION))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Machine(BV16_DEFINITION))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(BV16_DEFINITION))==(skip);
  Context_List_Initialisation(Machine(BV16_DEFINITION))==(skip);
  List_Initialisation(Machine(BV16_DEFINITION))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(BV16_DEFINITION))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(BV16_DEFINITION),Machine(BIT_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(BV16_DEFINITION),Machine(BYTE_DEFINITION))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(BV16_DEFINITION))==(btrue);
  List_Constraints(Machine(BV16_DEFINITION))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(BV16_DEFINITION))==(?);
  List_Operations(Machine(BV16_DEFINITION))==(?)
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
  List_Valuable_Constants(Machine(BV16_DEFINITION))==(BV16_INDX,PHYS_BV16_INDEX,BV16,bv16_byte,byte_bv16,complement16);
  Inherited_List_Constants(Machine(BV16_DEFINITION))==(?);
  List_Constants(Machine(BV16_DEFINITION))==(BV16_INDX,PHYS_BV16_INDEX,BV16,bv16_byte,byte_bv16,complement16)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(BV16_DEFINITION))==(?);
  Context_List_Defered(Machine(BV16_DEFINITION))==(?);
  Context_List_Sets(Machine(BV16_DEFINITION))==(?);
  List_Valuable_Sets(Machine(BV16_DEFINITION))==(?);
  Inherited_List_Enumerated(Machine(BV16_DEFINITION))==(?);
  Inherited_List_Defered(Machine(BV16_DEFINITION))==(?);
  Inherited_List_Sets(Machine(BV16_DEFINITION))==(?);
  List_Enumerated(Machine(BV16_DEFINITION))==(?);
  List_Defered(Machine(BV16_DEFINITION))==(?);
  List_Sets(Machine(BV16_DEFINITION))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(BV16_DEFINITION))==(?);
  Expanded_List_HiddenConstants(Machine(BV16_DEFINITION))==(?);
  List_HiddenConstants(Machine(BV16_DEFINITION))==(?);
  External_List_HiddenConstants(Machine(BV16_DEFINITION))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(BV16_DEFINITION))==(btrue);
  Context_List_Properties(Machine(BV16_DEFINITION))==(BIT = {0,1} & bit_not: BIT >->> BIT & bit_not = {0|->1}\/{1|->0} & bit_and: BIT*BIT --> BIT & bit_and = {(0,0)|->0}\/{(0,1)|->0}\/{(1,0)|->0}\/{(1,1)|->1} & bit_or: BIT*BIT --> BIT & bit_or = {(0,0)|->0}\/{(0,1)|->1}\/{(1,0)|->1}\/{(1,1)|->1} & bit_xor: BIT*BIT --> BIT & bit_xor = {(0,0)|->0}\/{(0,1)|->1}\/{(1,0)|->1}\/{(1,1)|->0} & bool_bit = {TRUE|->1,FALSE|->0} & BYTE_INDEX = 1..8 & PHYS_BYTE_INDEX = 0..8-1 & BYTE = BYTE_INDEX --> BIT & !b1.(b1: BYTE => size(b1) = 8 & b1: seq1(BIT)) & is_zero: BYTE --> BIT & is_zero = %w1.(w1: BYTE | bool_bit(bool(w1(1) = w1(2) & w1(2) = w1(3) & w1(3) = w1(4) & w1(4) = w1(5) & w1(5) = w1(6) & w1(6) = w1(7) & w1(7) = w1(8) & w1(8) = 0))) & parity_even: BYTE --> BIT & parity_even = %bv.(bv: BYTE | 1-(bv(1)+bv(2)+bv(3)+bv(4)+bv(5)+bv(6)+bv(7)+bv(8)) mod 2) & bv8_and = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | %idx.(idx: 1..8 | bit_and(bt1(idx),bt2(idx)))) & bv8_or = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | %idx.(idx: 1..8 | bit_or(bt1(idx),bt2(idx)))) & bv8_xor = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | %idx.(idx: 1..8 | bit_xor(bt1(idx),bt2(idx)))) & bv8get: BYTE*PHYS_BYTE_INDEX --> BIT & bv8get = %(bt1,ii).(bt1: BYTE & ii: PHYS_BYTE_INDEX | bt1(ii+1)) & bitset: BYTE*PHYS_BYTE_INDEX --> BYTE & bitset = %(bt1,ii).(bt1: BYTE & ii: PHYS_BYTE_INDEX | bt1<+{ii+1|->1}) & bitclear: BYTE*PHYS_BYTE_INDEX --> BYTE & bitclear = %(bt1,ii).(bt1: BYTE & ii: PHYS_BYTE_INDEX | bt1<+{ii+1|->0}) & complement: BYTE --> BYTE & complement = %bt.(bt: BYTE | %idx.(idx: 1..8 | bit_not(bt(idx)))) & swap: BYTE --> BYTE & swap = %bt.(bt: BYTE | {1|->bt(5),2|->bt(6),3|->bt(7),4|->bt(8),5|->bt(1),6|->bt(2),7|->bt(3),8|->bt(4)}) & rotateleft: BYTE --> BYTE & rotateleft = %bv.(bv: BYTE | {1|->bv(8),2|->bv(1),3|->bv(2),4|->bv(3),5|->bv(4),6|->bv(5),7|->bv(6),8|->bv(7)}) & rotateright: BYTE --> BYTE & rotateright = %bv.(bv: BYTE | {1|->bv(2),2|->bv(3),3|->bv(4),4|->bv(5),5|->bv(6),6|->bv(7),7|->bv(8),8|->bv(1)}) & get_upper_digit: BYTE --> 0..15 & get_upper_digit = %by.(by: BYTE | 8*by(8)+4*by(7)+2*by(6)+by(5)) & get_lower_digit: BYTE --> 0..15 & get_lower_digit = %by.(by: BYTE | 8*by(4)+4*by(3)+2*by(2)+by(1)));
  Inherited_List_Properties(Machine(BV16_DEFINITION))==(btrue);
  List_Properties(Machine(BV16_DEFINITION))==(BV16_INDX = 1..16 & PHYS_BV16_INDEX = 0..16-1 & BV16 = BV16_INDX --> BIT & !b1.(b1: BV16 => size(b1) = 16 & b1: seq1(BIT)) & complement16 = %bt.(bt: BV16 | %idx.(idx: 1..16 | bit_not(bt(idx)))) & bv16_byte = %bv.(bv: BV16 | {8|->bv(16),7|->bv(15),6|->bv(14),5|->bv(13),4|->bv(12),3|->bv(11),2|->bv(10),1|->bv(9)},{8|->bv(8),7|->bv(7),6|->bv(6),5|->bv(5),4|->bv(4),3|->bv(3),2|->bv(2),1|->bv(1)}) & byte_bv16 = %(bv1,bv2).(bv1: BYTE & bv2: BYTE | {16|->bv1(8),15|->bv1(7),14|->bv1(6),13|->bv1(5),12|->bv1(4),11|->bv1(3),10|->bv1(2),9|->bv1(1),8|->bv2(8),7|->bv2(7),6|->bv2(6),5|->bv2(5),4|->bv2(4),3|->bv2(3),2|->bv2(2),1|->bv2(1)}))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(BV16_DEFINITION),Machine(BYTE_DEFINITION))==(?);
  Seen_Context_List_Enumerated(Machine(BV16_DEFINITION))==(?);
  Seen_Context_List_Invariant(Machine(BV16_DEFINITION))==(btrue);
  Seen_Context_List_Assertions(Machine(BV16_DEFINITION))==(!bb.(bb: BIT => bit_not(bb) = 1-bb) & dom(bit_and) = BIT*BIT & ran(bit_not) = BIT & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & dom(bit_and) = BIT*BIT & ran(bit_and) = BIT & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & dom(bit_or) = BIT*BIT & ran(bit_or) = BIT & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & dom(bit_xor) = BIT*BIT & ran(bit_xor) = BIT & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & dom(bool_bit) = BOOL & ran(bit_xor) = BIT & bool_bit(TRUE) = 1 & bool_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;(-2)**7 = -128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536));
  Seen_Context_List_Properties(Machine(BV16_DEFINITION))==(BIT = {0,1} & bit_not: BIT >->> BIT & bit_not = {0|->1}\/{1|->0} & bit_and: BIT*BIT --> BIT & bit_and = {(0,0)|->0}\/{(0,1)|->0}\/{(1,0)|->0}\/{(1,1)|->1} & bit_or: BIT*BIT --> BIT & bit_or = {(0,0)|->0}\/{(0,1)|->1}\/{(1,0)|->1}\/{(1,1)|->1} & bit_xor: BIT*BIT --> BIT & bit_xor = {(0,0)|->0}\/{(0,1)|->1}\/{(1,0)|->1}\/{(1,1)|->0} & bool_bit = {TRUE|->1,FALSE|->0});
  Seen_List_Constraints(Machine(BV16_DEFINITION))==(btrue);
  Seen_List_Operations(Machine(BV16_DEFINITION),Machine(BYTE_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(BV16_DEFINITION),Machine(BYTE_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(BV16_DEFINITION),Machine(BIT_DEFINITION))==(?);
  Seen_List_Operations(Machine(BV16_DEFINITION),Machine(BIT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(BV16_DEFINITION),Machine(BIT_DEFINITION))==(btrue)
END
&
THEORY ListANYVarX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(BV16_DEFINITION)) == (BV16_INDX,PHYS_BV16_INDEX,BV16,bv16_byte,byte_bv16,complement16 | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BYTE_DEFINITION)) | ? | BV16_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BV16_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BV16_DEFINITION)) == (BV16_INDX,PHYS_BV16_INDEX,BV16,bv16_byte,byte_bv16,complement16);
  List_Of_VisibleVar_Ids(Machine(BV16_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BV16_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_INDEX,PHYS_BYTE_INDEX,is_zero,parity_even,bv8_and,bv8_or,bv8_xor,bitclear,bitset,bv8get,complement,swap,rotateleft,rotateright,get_upper_digit,get_lower_digit | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(POWER2)) | ? | BYTE_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_INDEX,PHYS_BYTE_INDEX,is_zero,parity_even,bv8_and,bv8_or,bv8_xor,bitclear,bitset,bv8get,complement,swap,rotateleft,rotateright,get_upper_digit,get_lower_digit);
  List_Of_VisibleVar_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BYTE_DEFINITION)) == (?: ?);
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
  List_Of_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_bit | ? | ? | ? | ? | ? | ? | ? | BIT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_bit);
  List_Of_VisibleVar_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_DEFINITION)) == (?: ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(BV16_DEFINITION)) == (Type(BV16_INDX) == Cst(SetOf(btype(INTEGER,"[BV16_INDX","]BV16_INDX")));Type(PHYS_BV16_INDEX) == Cst(SetOf(btype(INTEGER,"[PHYS_BV16_INDEX","]PHYS_BV16_INDEX")));Type(BV16) == Cst(SetOf(SetOf(btype(INTEGER,"[BV16_INDX","]BV16_INDX")*btype(INTEGER,"[BIT","]BIT"))));Type(bv16_byte) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))));Type(byte_bv16) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(complement16) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))))
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
