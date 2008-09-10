Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(BIT_VECTOR_ARITHMETICS))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(BIT_VECTOR_ARITHMETICS))==(Machine(BIT_VECTOR_ARITHMETICS));
  Level(Machine(BIT_VECTOR_ARITHMETICS))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(BIT_VECTOR_ARITHMETICS)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(BIT_VECTOR_ARITHMETICS))==(BIT_DEFINITION,BIT_VECTOR_DEFINITION)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(BIT_VECTOR_ARITHMETICS))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  List_Includes(Machine(BIT_VECTOR_ARITHMETICS))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(BIT_VECTOR_ARITHMETICS))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(BIT_VECTOR_ARITHMETICS))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Context_List_Variables(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Abstract_List_Variables(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Local_List_Variables(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  List_Variables(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  External_List_Variables(Machine(BIT_VECTOR_ARITHMETICS))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Abstract_List_VisibleVariables(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  External_List_VisibleVariables(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Expanded_List_VisibleVariables(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  List_VisibleVariables(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Internal_List_VisibleVariables(Machine(BIT_VECTOR_ARITHMETICS))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(BIT_VECTOR_ARITHMETICS))==(btrue);
  Gluing_List_Invariant(Machine(BIT_VECTOR_ARITHMETICS))==(btrue);
  Expanded_List_Invariant(Machine(BIT_VECTOR_ARITHMETICS))==(btrue);
  Abstract_List_Invariant(Machine(BIT_VECTOR_ARITHMETICS))==(btrue);
  Context_List_Invariant(Machine(BIT_VECTOR_ARITHMETICS))==(btrue);
  List_Invariant(Machine(BIT_VECTOR_ARITHMETICS))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(BIT_VECTOR_ARITHMETICS))==(btrue);
  Abstract_List_Assertions(Machine(BIT_VECTOR_ARITHMETICS))==(btrue);
  Context_List_Assertions(Machine(BIT_VECTOR_ARITHMETICS))==((bit_not(0) = 1;bit_not(1) = 0;!bb.(bb: BIT => bit_not(bit_not(bb)) = bb);bit_and(0,0) = 0;bit_and(0,1) = 0;bit_and(1,0) = 0;bit_and(1,1) = 1;!(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3));!b1.(b1: BIT => bit_and(b1,1) = b1);!b1.(b1: BIT => bit_and(b1,0) = 0);bit_or(0,0) = 0;bit_or(0,1) = 0;bit_or(1,0) = 0;bit_or(1,1) = 1;!(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3));!b1.(b1: BIT => bit_or(b1,1) = 1);!b1.(b1: BIT => bit_or(b1,0) = b1);bit_xor(0,0) = 0;bit_xor(0,1) = 1;bit_xor(1,0) = 1;bit_xor(1,1) = 0;!(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3));!bb.(bb: BIT => bit_xor(bb,bb) = 0);bool_to_bit(TRUE) = 1;bool_to_bit(FALSE) = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!bv.(bv: BIT_VECTOR => bv_not(bv_not(bv)) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: NATURAL & high: NATURAL & low<=high => bv_size(bv_sub(bv,low,high)) = 1+high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_and(v1,v2) = bv_and(v2,v1));!(v1,v2,v3).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) => bv_and(v1,bv_and(v2,v3)) = bv_and(bv_and(v1,v2),v3));!bv.(bv: BIT_VECTOR => bv_and(bv,bv_zero(bv_size(bv))) = bv_zero(bv_size(bv)));!bv.(bv: BIT_VECTOR => bv_and(bv,bv_one(bv_size(bv))) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_or(v1,v2) = bv_or(v2,v1));!(v1,v2,v3).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) => bv_or(v1,bv_or(v2,v3)) = bv_or(bv_or(v1,v2),v3));!bv.(bv: BIT_VECTOR => bv_or(bv,bv_one(bv_size(bv))) = bv_one(bv_size(bv)));!bv.(bv: BIT_VECTOR => bv_or(bv,bv_zero(bv_size(bv))) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_xor(v1,v2) = bv_xor(v2,v1));!bv.(bv: BIT_VECTOR => bv_xor(bv,bv) = bv_zero(bv_size(bv)))));
  List_Assertions(Machine(BIT_VECTOR_ARITHMETICS))==(!ss.(ss: NATURAL1 => bv_to_nat(bv_zero(ss)) = 0))
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(BIT_VECTOR_ARITHMETICS))==(skip);
  Context_List_Initialisation(Machine(BIT_VECTOR_ARITHMETICS))==(skip);
  List_Initialisation(Machine(BIT_VECTOR_ARITHMETICS))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(BIT_VECTOR_ARITHMETICS))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(BIT_VECTOR_ARITHMETICS),Machine(BIT_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(BIT_VECTOR_ARITHMETICS),Machine(BIT_VECTOR_DEFINITION))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(BIT_VECTOR_ARITHMETICS))==(btrue);
  List_Constraints(Machine(BIT_VECTOR_ARITHMETICS))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  List_Operations(Machine(BIT_VECTOR_ARITHMETICS))==(?)
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
  List_Valuable_Constants(Machine(BIT_VECTOR_ARITHMETICS))==(bv_to_nat);
  Inherited_List_Constants(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  List_Constants(Machine(BIT_VECTOR_ARITHMETICS))==(bv_to_nat)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Context_List_Defered(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Context_List_Sets(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  List_Valuable_Sets(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Inherited_List_Enumerated(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Inherited_List_Defered(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Inherited_List_Sets(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  List_Enumerated(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  List_Defered(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  List_Sets(Machine(BIT_VECTOR_ARITHMETICS))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Expanded_List_HiddenConstants(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  List_HiddenConstants(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  External_List_HiddenConstants(Machine(BIT_VECTOR_ARITHMETICS))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(BIT_VECTOR_ARITHMETICS))==(btrue);
  Context_List_Properties(Machine(BIT_VECTOR_ARITHMETICS))==(BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = seq1(BIT) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v2^v1) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: NATURAL & high: NATURAL & low<=high | 0..high-low<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (1..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (1..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 1..size(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 1..size(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 1..size(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 1..size(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1(idx+1)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1<+{idx+1|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1<+{idx+1|->0}));
  Inherited_List_Properties(Machine(BIT_VECTOR_ARITHMETICS))==(btrue);
  List_Properties(Machine(BIT_VECTOR_ARITHMETICS))==(bv_to_nat: BIT_VECTOR --> NATURAL & bv_to_nat = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | 2**idx*bv(idx))))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(BIT_VECTOR_ARITHMETICS),Machine(BIT_VECTOR_DEFINITION))==(?);
  Seen_Context_List_Enumerated(Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Seen_Context_List_Invariant(Machine(BIT_VECTOR_ARITHMETICS))==(btrue);
  Seen_Context_List_Assertions(Machine(BIT_VECTOR_ARITHMETICS))==(bit_not(0) = 1;bit_not(1) = 0;!bb.(bb: BIT => bit_not(bit_not(bb)) = bb);bit_and(0,0) = 0;bit_and(0,1) = 0;bit_and(1,0) = 0;bit_and(1,1) = 1;!(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3));!b1.(b1: BIT => bit_and(b1,1) = b1);!b1.(b1: BIT => bit_and(b1,0) = 0);bit_or(0,0) = 0;bit_or(0,1) = 0;bit_or(1,0) = 0;bit_or(1,1) = 1;!(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3));!b1.(b1: BIT => bit_or(b1,1) = 1);!b1.(b1: BIT => bit_or(b1,0) = b1);bit_xor(0,0) = 0;bit_xor(0,1) = 1;bit_xor(1,0) = 1;bit_xor(1,1) = 0;!(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3));!bb.(bb: BIT => bit_xor(bb,bb) = 0);bool_to_bit(TRUE) = 1;bool_to_bit(FALSE) = 0);
  Seen_Context_List_Properties(Machine(BIT_VECTOR_ARITHMETICS))==(BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0});
  Seen_List_Constraints(Machine(BIT_VECTOR_ARITHMETICS))==(btrue);
  Seen_List_Operations(Machine(BIT_VECTOR_ARITHMETICS),Machine(BIT_VECTOR_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(BIT_VECTOR_ARITHMETICS),Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(BIT_VECTOR_ARITHMETICS),Machine(BIT_DEFINITION))==(?);
  Seen_List_Operations(Machine(BIT_VECTOR_ARITHMETICS),Machine(BIT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(BIT_VECTOR_ARITHMETICS),Machine(BIT_DEFINITION))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (bv_to_nat | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)) | ? | BIT_VECTOR_ARITHMETICS);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (bv_to_nat);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR_ARITHMETICS)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_size,bv_catenate,bv_sub,bv_zero,bv_one,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)) | ? | BIT_VECTOR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_size,bv_catenate,bv_sub,bv_zero,bv_one,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ? | BIT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit);
  List_Of_VisibleVar_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_DEFINITION)) == (?: ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(BIT_VECTOR_ARITHMETICS)) == (Type(bv_to_nat) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?))))
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
