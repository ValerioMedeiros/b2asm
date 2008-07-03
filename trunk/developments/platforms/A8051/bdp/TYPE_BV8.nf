Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(TYPE_BV8))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(TYPE_BV8))==(Machine(TYPE_BV8));
  Level(Machine(TYPE_BV8))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(TYPE_BV8)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(TYPE_BV8))==(TYPE_BIT)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(TYPE_BV8))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(TYPE_BV8))==(?);
  List_Includes(Machine(TYPE_BV8))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(TYPE_BV8))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(TYPE_BV8))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(TYPE_BV8))==(?);
  Context_List_Variables(Machine(TYPE_BV8))==(?);
  Abstract_List_Variables(Machine(TYPE_BV8))==(?);
  Local_List_Variables(Machine(TYPE_BV8))==(?);
  List_Variables(Machine(TYPE_BV8))==(?);
  External_List_Variables(Machine(TYPE_BV8))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(TYPE_BV8))==(?);
  Abstract_List_VisibleVariables(Machine(TYPE_BV8))==(?);
  External_List_VisibleVariables(Machine(TYPE_BV8))==(?);
  Expanded_List_VisibleVariables(Machine(TYPE_BV8))==(?);
  List_VisibleVariables(Machine(TYPE_BV8))==(?);
  Internal_List_VisibleVariables(Machine(TYPE_BV8))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(TYPE_BV8))==(btrue);
  Gluing_List_Invariant(Machine(TYPE_BV8))==(btrue);
  Expanded_List_Invariant(Machine(TYPE_BV8))==(btrue);
  Abstract_List_Invariant(Machine(TYPE_BV8))==(btrue);
  Context_List_Invariant(Machine(TYPE_BV8))==(btrue);
  List_Invariant(Machine(TYPE_BV8))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(TYPE_BV8))==(btrue);
  Abstract_List_Assertions(Machine(TYPE_BV8))==(btrue);
  Context_List_Assertions(Machine(TYPE_BV8))==(BIT_FLIP(0) = 1 & BIT_FLIP(1) = 0 & BIT_AND(0,0) = 0 & BIT_AND(0,1) = 0 & BIT_AND(1,0) = 0 & BIT_AND(1,1) = 1 & BIT_IOR(0,0) = 0 & BIT_IOR(0,1) = 0 & BIT_IOR(1,0) = 0 & BIT_IOR(1,1) = 1 & BIT_XOR(0,0) = 0 & BIT_XOR(0,1) = 1 & BIT_XOR(1,0) = 1 & BIT_XOR(1,1) = 0);
  List_Assertions(Machine(TYPE_BV8))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(TYPE_BV8))==(skip);
  Context_List_Initialisation(Machine(TYPE_BV8))==(skip);
  List_Initialisation(Machine(TYPE_BV8))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(TYPE_BV8))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(TYPE_BV8),Machine(TYPE_BIT))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(TYPE_BV8))==(btrue);
  List_Constraints(Machine(TYPE_BV8))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(TYPE_BV8))==(?);
  List_Operations(Machine(TYPE_BV8))==(?)
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
  List_Valuable_Constants(Machine(TYPE_BV8))==(BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BV8_PAR);
  Inherited_List_Constants(Machine(TYPE_BV8))==(?);
  List_Constants(Machine(TYPE_BV8))==(BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BV8_PAR)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(TYPE_BV8))==(?);
  Context_List_Defered(Machine(TYPE_BV8))==(?);
  Context_List_Sets(Machine(TYPE_BV8))==(?);
  List_Valuable_Sets(Machine(TYPE_BV8))==(?);
  Inherited_List_Enumerated(Machine(TYPE_BV8))==(?);
  Inherited_List_Defered(Machine(TYPE_BV8))==(?);
  Inherited_List_Sets(Machine(TYPE_BV8))==(?);
  List_Enumerated(Machine(TYPE_BV8))==(?);
  List_Defered(Machine(TYPE_BV8))==(?);
  List_Sets(Machine(TYPE_BV8))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(TYPE_BV8))==(?);
  Expanded_List_HiddenConstants(Machine(TYPE_BV8))==(?);
  List_HiddenConstants(Machine(TYPE_BV8))==(?);
  External_List_HiddenConstants(Machine(TYPE_BV8))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(TYPE_BV8))==(btrue);
  Context_List_Properties(Machine(TYPE_BV8))==(BIT = 0..1 & BIT_FLIP : BIT --> BIT & !b.(b : BIT => BIT_FLIP(b) = 1-b) & BIT_AND : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_AND(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & BIT_IOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_IOR(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & BIT_XOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_XOR(b1,b2) = 1 <=> (b1 = 1 & b2 = 0 or (b1 = 0 & b2 = 1))));
  Inherited_List_Properties(Machine(TYPE_BV8))==(btrue);
  List_Properties(Machine(TYPE_BV8))==(BV8_INDEX = 0..7 & BV8 = BV8_INDEX --> BIT & BV8_SET_BIT : BV8*BV8_INDEX*BIT --> BV8 & !(v,i,j,b).(v : BV8 & i : BV8_INDEX & j : BV8_INDEX & b : BIT => (i/=j => BV8_SET_BIT(v,i,b)(j) = v(j))) & !(v,i,b).(v : BV8 & i : BV8_INDEX & b : BIT => BV8_SET_BIT(v,i,b)(i) = b) & BV8_COMPLEMENT : BV8 --> BV8 & !(v,i).(v : BV8 & i : BV8_INDEX => BV8_COMPLEMENT(v)(i) = BIT_FLIP(v(i))) & BV8_ALL_ZEROES : BV8 & !i.(i : BV8_INDEX => BV8_ALL_ZEROES(i) = 0) & BV8_AND : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_AND(v1,v2)(i) = BIT_AND(v1(i),v2(i))) & BV8_IOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_IOR(v1,v2)(i) = BIT_IOR(v1(i),v2(i))) & BV8_XOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_XOR(v1,v2)(i) = BIT_XOR(v1(i),v2(i))) & BV8_PAR : BV8 --> BIT & !(v1,t1,t2,t3,t4).(v1 : BV8 & t1 : BIT & t2 : BIT & t3 : BIT & t4 : BIT & t1 = BIT_XOR(v1(7),v1(6)) & t2 = BIT_XOR(v1(5),v1(4)) & t3 = BIT_XOR(v1(3),v1(2)) & t4 = BIT_XOR(v1(1),v1(0)) => BV8_PAR(v1) = BIT_XOR(BIT_XOR(t1,t2),BIT_XOR(t3,t4))))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(TYPE_BV8),Machine(TYPE_BIT))==(?);
  Seen_Context_List_Enumerated(Machine(TYPE_BV8))==(?);
  Seen_Context_List_Invariant(Machine(TYPE_BV8))==(btrue);
  Seen_Context_List_Assertions(Machine(TYPE_BV8))==(btrue);
  Seen_Context_List_Properties(Machine(TYPE_BV8))==(btrue);
  Seen_List_Constraints(Machine(TYPE_BV8))==(btrue);
  Seen_List_Operations(Machine(TYPE_BV8),Machine(TYPE_BIT))==(?);
  Seen_Expanded_List_Invariant(Machine(TYPE_BV8),Machine(TYPE_BIT))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(TYPE_BV8)) == (BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BV8_PAR | ? | ? | ? | ? | ? | seen(Machine(TYPE_BIT)) | ? | TYPE_BV8);
  List_Of_HiddenCst_Ids(Machine(TYPE_BV8)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BV8)) == (BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BV8_PAR);
  List_Of_VisibleVar_Ids(Machine(TYPE_BV8)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BV8)) == (? : ?);
  List_Of_Ids(Machine(TYPE_BIT)) == (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | ? | ? | ? | TYPE_BIT);
  List_Of_HiddenCst_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BIT)) == (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR);
  List_Of_VisibleVar_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BIT)) == (? : ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(TYPE_BV8)) == (Type(BV8_INDEX) == Cst(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")));Type(BV8) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_SET_BIT) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT")*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_COMPLEMENT) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_ALL_ZEROES) == Cst(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT")));Type(BV8_AND) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_IOR) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_XOR) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_PAR) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*btype(INTEGER,"[BIT","]BIT"))))
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
