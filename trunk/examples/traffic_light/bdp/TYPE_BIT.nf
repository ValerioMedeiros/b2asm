Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(TYPE_BIT))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(TYPE_BIT))==(Machine(TYPE_BIT));
  Level(Machine(TYPE_BIT))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(TYPE_BIT)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(TYPE_BIT))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(TYPE_BIT))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(TYPE_BIT))==(?);
  List_Includes(Machine(TYPE_BIT))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(TYPE_BIT))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(TYPE_BIT))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(TYPE_BIT))==(?);
  Context_List_Variables(Machine(TYPE_BIT))==(?);
  Abstract_List_Variables(Machine(TYPE_BIT))==(?);
  Local_List_Variables(Machine(TYPE_BIT))==(?);
  List_Variables(Machine(TYPE_BIT))==(?);
  External_List_Variables(Machine(TYPE_BIT))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(TYPE_BIT))==(?);
  Abstract_List_VisibleVariables(Machine(TYPE_BIT))==(?);
  External_List_VisibleVariables(Machine(TYPE_BIT))==(?);
  Expanded_List_VisibleVariables(Machine(TYPE_BIT))==(?);
  List_VisibleVariables(Machine(TYPE_BIT))==(?);
  Internal_List_VisibleVariables(Machine(TYPE_BIT))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(TYPE_BIT))==(btrue);
  Gluing_List_Invariant(Machine(TYPE_BIT))==(btrue);
  Expanded_List_Invariant(Machine(TYPE_BIT))==(btrue);
  Abstract_List_Invariant(Machine(TYPE_BIT))==(btrue);
  Context_List_Invariant(Machine(TYPE_BIT))==(btrue);
  List_Invariant(Machine(TYPE_BIT))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(TYPE_BIT))==(btrue);
  Abstract_List_Assertions(Machine(TYPE_BIT))==(btrue);
  Context_List_Assertions(Machine(TYPE_BIT))==(btrue);
  List_Assertions(Machine(TYPE_BIT))==(BIT_FLIP(0) = 1 & BIT_FLIP(1) = 0 & BIT_AND(0,0) = 0 & BIT_AND(0,1) = 0 & BIT_AND(1,0) = 0 & BIT_AND(1,1) = 1 & BIT_IOR(0,0) = 0 & BIT_IOR(0,1) = 0 & BIT_IOR(1,0) = 0 & BIT_IOR(1,1) = 1 & BIT_XOR(0,0) = 0 & BIT_XOR(0,1) = 1 & BIT_XOR(1,0) = 1 & BIT_XOR(1,1) = 0)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(TYPE_BIT))==(skip);
  Context_List_Initialisation(Machine(TYPE_BIT))==(skip);
  List_Initialisation(Machine(TYPE_BIT))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(TYPE_BIT))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(TYPE_BIT))==(btrue);
  List_Constraints(Machine(TYPE_BIT))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(TYPE_BIT))==(?);
  List_Operations(Machine(TYPE_BIT))==(?)
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
  List_Valuable_Constants(Machine(TYPE_BIT))==(BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR);
  Inherited_List_Constants(Machine(TYPE_BIT))==(?);
  List_Constants(Machine(TYPE_BIT))==(BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(TYPE_BIT))==(?);
  Context_List_Defered(Machine(TYPE_BIT))==(?);
  Context_List_Sets(Machine(TYPE_BIT))==(?);
  List_Valuable_Sets(Machine(TYPE_BIT))==(?);
  Inherited_List_Enumerated(Machine(TYPE_BIT))==(?);
  Inherited_List_Defered(Machine(TYPE_BIT))==(?);
  Inherited_List_Sets(Machine(TYPE_BIT))==(?);
  List_Enumerated(Machine(TYPE_BIT))==(?);
  List_Defered(Machine(TYPE_BIT))==(?);
  List_Sets(Machine(TYPE_BIT))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(TYPE_BIT))==(?);
  Expanded_List_HiddenConstants(Machine(TYPE_BIT))==(?);
  List_HiddenConstants(Machine(TYPE_BIT))==(?);
  External_List_HiddenConstants(Machine(TYPE_BIT))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(TYPE_BIT))==(btrue);
  Context_List_Properties(Machine(TYPE_BIT))==(btrue);
  Inherited_List_Properties(Machine(TYPE_BIT))==(btrue);
  List_Properties(Machine(TYPE_BIT))==(BIT = 0..1 & BIT_FLIP : BIT --> BIT & !b.(b : BIT => BIT_FLIP(b) = 1-b) & BIT_AND : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_AND(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & BIT_IOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_IOR(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & BIT_XOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_XOR(b1,b2) = 1 <=> (b1 = 1 & b2 = 0 or (b1 = 0 & b2 = 1))))
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(TYPE_BIT)) == (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | ? | ? | ? | TYPE_BIT);
  List_Of_HiddenCst_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BIT)) == (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR);
  List_Of_VisibleVar_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BIT)) == (? : ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(TYPE_BIT)) == (Type(BIT) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")));Type(BIT_FLIP) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT_AND) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT_IOR) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT_XOR) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT"))))
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
