Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(TYPE_BV16))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(TYPE_BV16))==(Machine(TYPE_BV16));
  Level(Machine(TYPE_BV16))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(TYPE_BV16)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(TYPE_BV16))==(TYPE_BIT)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(TYPE_BV16))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(TYPE_BV16))==(?);
  List_Includes(Machine(TYPE_BV16))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(TYPE_BV16))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(TYPE_BV16))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(TYPE_BV16))==(?);
  Context_List_Variables(Machine(TYPE_BV16))==(?);
  Abstract_List_Variables(Machine(TYPE_BV16))==(?);
  Local_List_Variables(Machine(TYPE_BV16))==(?);
  List_Variables(Machine(TYPE_BV16))==(?);
  External_List_Variables(Machine(TYPE_BV16))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(TYPE_BV16))==(?);
  Abstract_List_VisibleVariables(Machine(TYPE_BV16))==(?);
  External_List_VisibleVariables(Machine(TYPE_BV16))==(?);
  Expanded_List_VisibleVariables(Machine(TYPE_BV16))==(?);
  List_VisibleVariables(Machine(TYPE_BV16))==(?);
  Internal_List_VisibleVariables(Machine(TYPE_BV16))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(TYPE_BV16))==(btrue);
  Gluing_List_Invariant(Machine(TYPE_BV16))==(btrue);
  Expanded_List_Invariant(Machine(TYPE_BV16))==(btrue);
  Abstract_List_Invariant(Machine(TYPE_BV16))==(btrue);
  Context_List_Invariant(Machine(TYPE_BV16))==(btrue);
  List_Invariant(Machine(TYPE_BV16))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(TYPE_BV16))==(btrue);
  Abstract_List_Assertions(Machine(TYPE_BV16))==(btrue);
  Context_List_Assertions(Machine(TYPE_BV16))==(BIT_FLIP(0) = 1 & BIT_FLIP(1) = 0 & BIT_AND(0,0) = 0 & BIT_AND(0,1) = 0 & BIT_AND(1,0) = 0 & BIT_AND(1,1) = 1 & BIT_IOR(0,0) = 0 & BIT_IOR(0,1) = 0 & BIT_IOR(1,0) = 0 & BIT_IOR(1,1) = 1 & BIT_XOR(0,0) = 0 & BIT_XOR(0,1) = 1 & BIT_XOR(1,0) = 1 & BIT_XOR(1,1) = 0);
  List_Assertions(Machine(TYPE_BV16))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(TYPE_BV16))==(skip);
  Context_List_Initialisation(Machine(TYPE_BV16))==(skip);
  List_Initialisation(Machine(TYPE_BV16))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(TYPE_BV16))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(TYPE_BV16),Machine(TYPE_BIT))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(TYPE_BV16))==(btrue);
  List_Constraints(Machine(TYPE_BV16))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(TYPE_BV16))==(?);
  List_Operations(Machine(TYPE_BV16))==(?)
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
  List_Valuable_Constants(Machine(TYPE_BV16))==(BV16_INDEX,BV16,BV16_SET_BIT,BV16_COMPLEMENT,BV16_ALL_ZEROES,BV16_AND,BV16_IOR,BV16_XOR);
  Inherited_List_Constants(Machine(TYPE_BV16))==(?);
  List_Constants(Machine(TYPE_BV16))==(BV16_INDEX,BV16,BV16_SET_BIT,BV16_COMPLEMENT,BV16_ALL_ZEROES,BV16_AND,BV16_IOR,BV16_XOR)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(TYPE_BV16))==(?);
  Context_List_Defered(Machine(TYPE_BV16))==(?);
  Context_List_Sets(Machine(TYPE_BV16))==(?);
  List_Valuable_Sets(Machine(TYPE_BV16))==(?);
  Inherited_List_Enumerated(Machine(TYPE_BV16))==(?);
  Inherited_List_Defered(Machine(TYPE_BV16))==(?);
  Inherited_List_Sets(Machine(TYPE_BV16))==(?);
  List_Enumerated(Machine(TYPE_BV16))==(?);
  List_Defered(Machine(TYPE_BV16))==(?);
  List_Sets(Machine(TYPE_BV16))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(TYPE_BV16))==(?);
  Expanded_List_HiddenConstants(Machine(TYPE_BV16))==(?);
  List_HiddenConstants(Machine(TYPE_BV16))==(?);
  External_List_HiddenConstants(Machine(TYPE_BV16))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(TYPE_BV16))==(btrue);
  Context_List_Properties(Machine(TYPE_BV16))==(BIT = 0..1 & BIT_FLIP : BIT --> BIT & !b.(b : BIT => BIT_FLIP(b) = 1-b) & BIT_AND : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_AND(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & BIT_IOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_IOR(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & BIT_XOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_XOR(b1,b2) = 1 <=> (b1 = 1 & b2 = 0 or (b1 = 0 & b2 = 1))));
  Inherited_List_Properties(Machine(TYPE_BV16))==(btrue);
  List_Properties(Machine(TYPE_BV16))==(BV16_INDEX = 0..15 & BV16 = BV16_INDEX --> BIT & BV16_SET_BIT : BV16*BV16_INDEX*BIT --> BV16 & !(v,i,j,b).(v : BV16 & i : BV16_INDEX & j : BV16_INDEX & b : BIT => (i/=j => BV16_SET_BIT(v,i,b)(j) = v(j))) & !(v,i,b).(v : BV16 & i : BV16_INDEX & b : BIT => BV16_SET_BIT(v,i,b)(i) = b) & BV16_COMPLEMENT : BV16 --> BV16 & !(v,i).(v : BV16 & i : BV16_INDEX => BV16_COMPLEMENT(v)(i) = BIT_FLIP(v(i))) & BV16_ALL_ZEROES : BV16 & !i.(i : BV16_INDEX => BV16_ALL_ZEROES(i) = 0) & BV16_AND : BV16*BV16 --> BV16 & !(v1,v2,i).(v1 : BV16 & v2 : BV16 & i : BV16_INDEX => BV16_AND(v1,v2)(i) = BIT_AND(v1(i),v2(i))) & BV16_IOR : BV16*BV16 --> BV16 & !(v1,v2,i).(v1 : BV16 & v2 : BV16 & i : BV16_INDEX => BV16_IOR(v1,v2)(i) = BIT_IOR(v1(i),v2(i))) & BV16_XOR : BV16*BV16 --> BV16 & !(v1,v2,i).(v1 : BV16 & v2 : BV16 & i : BV16_INDEX => BV16_XOR(v1,v2)(i) = BIT_XOR(v1(i),v2(i))))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(TYPE_BV16),Machine(TYPE_BIT))==(?);
  Seen_Context_List_Enumerated(Machine(TYPE_BV16))==(?);
  Seen_Context_List_Invariant(Machine(TYPE_BV16))==(btrue);
  Seen_Context_List_Assertions(Machine(TYPE_BV16))==(btrue);
  Seen_Context_List_Properties(Machine(TYPE_BV16))==(btrue);
  Seen_List_Constraints(Machine(TYPE_BV16))==(btrue);
  Seen_List_Operations(Machine(TYPE_BV16),Machine(TYPE_BIT))==(?);
  Seen_Expanded_List_Invariant(Machine(TYPE_BV16),Machine(TYPE_BIT))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(TYPE_BV16)) == (BV16_INDEX,BV16,BV16_SET_BIT,BV16_COMPLEMENT,BV16_ALL_ZEROES,BV16_AND,BV16_IOR,BV16_XOR | ? | ? | ? | ? | ? | seen(Machine(TYPE_BIT)) | ? | TYPE_BV16);
  List_Of_HiddenCst_Ids(Machine(TYPE_BV16)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BV16)) == (BV16_INDEX,BV16,BV16_SET_BIT,BV16_COMPLEMENT,BV16_ALL_ZEROES,BV16_AND,BV16_IOR,BV16_XOR);
  List_Of_VisibleVar_Ids(Machine(TYPE_BV16)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BV16)) == (? : ?);
  List_Of_Ids(Machine(TYPE_BIT)) == (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | ? | ? | ? | TYPE_BIT);
  List_Of_HiddenCst_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BIT)) == (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR);
  List_Of_VisibleVar_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BIT)) == (? : ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(TYPE_BV16)) == (Type(BV16_INDEX) == Cst(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")));Type(BV16) == Cst(SetOf(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV16_SET_BIT) == Cst(SetOf(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT")*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV16_COMPLEMENT) == Cst(SetOf(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV16_ALL_ZEROES) == Cst(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT")));Type(BV16_AND) == Cst(SetOf(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV16_IOR) == Cst(SetOf(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV16_XOR) == Cst(SetOf(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT")))))
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
