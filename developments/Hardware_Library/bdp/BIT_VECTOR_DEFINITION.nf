Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(BIT_VECTOR_DEFINITION))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(BIT_VECTOR_DEFINITION))==(Machine(BIT_VECTOR_DEFINITION));
  Level(Machine(BIT_VECTOR_DEFINITION))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(BIT_VECTOR_DEFINITION)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(BIT_VECTOR_DEFINITION))==(BIT_DEFINITION)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(BIT_VECTOR_DEFINITION))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(BIT_VECTOR_DEFINITION))==(?);
  List_Includes(Machine(BIT_VECTOR_DEFINITION))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(BIT_VECTOR_DEFINITION))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(BIT_VECTOR_DEFINITION))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(BIT_VECTOR_DEFINITION))==(?);
  Context_List_Variables(Machine(BIT_VECTOR_DEFINITION))==(?);
  Abstract_List_Variables(Machine(BIT_VECTOR_DEFINITION))==(?);
  Local_List_Variables(Machine(BIT_VECTOR_DEFINITION))==(?);
  List_Variables(Machine(BIT_VECTOR_DEFINITION))==(?);
  External_List_Variables(Machine(BIT_VECTOR_DEFINITION))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(BIT_VECTOR_DEFINITION))==(?);
  Abstract_List_VisibleVariables(Machine(BIT_VECTOR_DEFINITION))==(?);
  External_List_VisibleVariables(Machine(BIT_VECTOR_DEFINITION))==(?);
  Expanded_List_VisibleVariables(Machine(BIT_VECTOR_DEFINITION))==(?);
  List_VisibleVariables(Machine(BIT_VECTOR_DEFINITION))==(?);
  Internal_List_VisibleVariables(Machine(BIT_VECTOR_DEFINITION))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Gluing_List_Invariant(Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Expanded_List_Invariant(Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Abstract_List_Invariant(Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Context_List_Invariant(Machine(BIT_VECTOR_DEFINITION))==(btrue);
  List_Invariant(Machine(BIT_VECTOR_DEFINITION))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Abstract_List_Assertions(Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Context_List_Assertions(Machine(BIT_VECTOR_DEFINITION))==(bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_to_bit(TRUE) = 1 & bool_to_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0));
  List_Assertions(Machine(BIT_VECTOR_DEFINITION))==(!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_not(bv_not(bv))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_and(v1,v2)(indx) = bv_and(v2,v1)(indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_and(v1,bv_and(v2,v3))(indx) = bv_and(bv_and(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_zero(bv_size(bv)))(indx) = bv_zero(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_one(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_or(v1,v2)(indx) = bv_or(v2,v1)(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_or(v1,bv_or(v2,v3))(indx) = bv_or(bv_or(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_one(bv_size(bv)))(indx) = bv_one(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_zero(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_xor(v1,v2)(indx) = bv_xor(v2,v1)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_xor(bv,bv)(indx) = bv_zero(bv_size(bv))(indx)))
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(BIT_VECTOR_DEFINITION))==(skip);
  Context_List_Initialisation(Machine(BIT_VECTOR_DEFINITION))==(skip);
  List_Initialisation(Machine(BIT_VECTOR_DEFINITION))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(BIT_VECTOR_DEFINITION))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(BIT_VECTOR_DEFINITION),Machine(BIT_DEFINITION))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(BIT_VECTOR_DEFINITION))==(btrue);
  List_Constraints(Machine(BIT_VECTOR_DEFINITION))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(BIT_VECTOR_DEFINITION))==(?);
  List_Operations(Machine(BIT_VECTOR_DEFINITION))==(?)
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
  List_Valuable_Constants(Machine(BIT_VECTOR_DEFINITION))==(BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put);
  Inherited_List_Constants(Machine(BIT_VECTOR_DEFINITION))==(?);
  List_Constants(Machine(BIT_VECTOR_DEFINITION))==(BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(BIT_VECTOR_DEFINITION))==(?);
  Context_List_Defered(Machine(BIT_VECTOR_DEFINITION))==(?);
  Context_List_Sets(Machine(BIT_VECTOR_DEFINITION))==(?);
  List_Valuable_Sets(Machine(BIT_VECTOR_DEFINITION))==(?);
  Inherited_List_Enumerated(Machine(BIT_VECTOR_DEFINITION))==(?);
  Inherited_List_Defered(Machine(BIT_VECTOR_DEFINITION))==(?);
  Inherited_List_Sets(Machine(BIT_VECTOR_DEFINITION))==(?);
  List_Enumerated(Machine(BIT_VECTOR_DEFINITION))==(?);
  List_Defered(Machine(BIT_VECTOR_DEFINITION))==(?);
  List_Sets(Machine(BIT_VECTOR_DEFINITION))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(BIT_VECTOR_DEFINITION))==(?);
  Expanded_List_HiddenConstants(Machine(BIT_VECTOR_DEFINITION))==(?);
  List_HiddenConstants(Machine(BIT_VECTOR_DEFINITION))==(?);
  External_List_HiddenConstants(Machine(BIT_VECTOR_DEFINITION))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Context_List_Properties(Machine(BIT_VECTOR_DEFINITION))==(BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0});
  Inherited_List_Properties(Machine(BIT_VECTOR_DEFINITION))==(btrue);
  List_Properties(Machine(BIT_VECTOR_DEFINITION))==(BIT_VECTOR = {bt | bt: NATURAL +-> BIT}-{{}} & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & BV_INDX: BIT_VECTOR --> POW(NATURAL) & BV_INDX = %bv.(bv: BIT_VECTOR | 0..bv_size(bv)-1) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high | low..high<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (0..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (0..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: BV_INDX(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1(idx)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: BV_INDX(v1) & bit: BIT | v1<+{idx|->bit}))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(BIT_VECTOR_DEFINITION),Machine(BIT_DEFINITION))==(?);
  Seen_Context_List_Enumerated(Machine(BIT_VECTOR_DEFINITION))==(?);
  Seen_Context_List_Invariant(Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Seen_Context_List_Assertions(Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Seen_Context_List_Properties(Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Seen_List_Constraints(Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Seen_List_Operations(Machine(BIT_VECTOR_DEFINITION),Machine(BIT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(BIT_VECTOR_DEFINITION),Machine(BIT_DEFINITION))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(BIT_VECTOR_DEFINITION),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)) | ? | BIT_VECTOR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put);
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
  Constants(Machine(BIT_VECTOR_DEFINITION)) == (Type(BV_INDX) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?))));Type(BIT_VECTOR) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_catenate) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_sub) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_zero) == Cst(SetOf(btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_one) == Cst(SetOf(btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_size) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)));Type(bv_not) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_and) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_or) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_xor) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_at) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*btype(INTEGER,"[BIT","]BIT")));Type(bv_set) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_clear) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_put) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*btype(INTEGER,"[BIT","]BIT")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))))
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
