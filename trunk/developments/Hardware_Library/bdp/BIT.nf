Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(BIT))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(BIT))==(Machine(BIT));
  Level(Machine(BIT))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(BIT)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(BIT))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(BIT))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(BIT))==(?);
  List_Includes(Machine(BIT))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(BIT))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(BIT))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(BIT))==(?);
  Context_List_Variables(Machine(BIT))==(?);
  Abstract_List_Variables(Machine(BIT))==(?);
  Local_List_Variables(Machine(BIT))==(?);
  List_Variables(Machine(BIT))==(?);
  External_List_Variables(Machine(BIT))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(BIT))==(?);
  Abstract_List_VisibleVariables(Machine(BIT))==(?);
  External_List_VisibleVariables(Machine(BIT))==(?);
  Expanded_List_VisibleVariables(Machine(BIT))==(?);
  List_VisibleVariables(Machine(BIT))==(?);
  Internal_List_VisibleVariables(Machine(BIT))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(BIT))==(btrue);
  Gluing_List_Invariant(Machine(BIT))==(btrue);
  Expanded_List_Invariant(Machine(BIT))==(btrue);
  Abstract_List_Invariant(Machine(BIT))==(btrue);
  Context_List_Invariant(Machine(BIT))==(btrue);
  List_Invariant(Machine(BIT))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(BIT))==(btrue);
  Abstract_List_Assertions(Machine(BIT))==(btrue);
  Context_List_Assertions(Machine(BIT))==(btrue);
  List_Assertions(Machine(BIT))==(BIT_NOT(0) = 1;BIT_NOT(1) = 0;BIT_AND(0,0) = 0;BIT_AND(0,1) = 0;BIT_AND(1,0) = 0;BIT_AND(1,1) = 1;BIT_IOR(0,0) = 0;BIT_IOR(0,1) = 0;BIT_IOR(1,0) = 0;BIT_IOR(1,1) = 1;BIT_XOR(0,0) = 0;BIT_XOR(0,1) = 1;BIT_XOR(1,0) = 1;BIT_XOR(1,1) = 0;BOOL_TO_BIT(TRUE) = 1;BOOL_TO_BIT(FALSE) = 0)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(BIT))==(skip);
  Context_List_Initialisation(Machine(BIT))==(skip);
  List_Initialisation(Machine(BIT))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(BIT))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(BIT))==(btrue);
  List_Constraints(Machine(BIT))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(BIT))==(?);
  List_Operations(Machine(BIT))==(?)
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
  List_Valuable_Constants(Machine(BIT))==(BIT_CARRIER,BIT_NOT,BIT_AND,BIT_IOR,BIT_XOR,BOOL_TO_BIT);
  Inherited_List_Constants(Machine(BIT))==(?);
  List_Constants(Machine(BIT))==(BIT_CARRIER,BIT_NOT,BIT_AND,BIT_IOR,BIT_XOR,BOOL_TO_BIT)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(BIT))==(?);
  Context_List_Defered(Machine(BIT))==(?);
  Context_List_Sets(Machine(BIT))==(?);
  List_Valuable_Sets(Machine(BIT))==(?);
  Inherited_List_Enumerated(Machine(BIT))==(?);
  Inherited_List_Defered(Machine(BIT))==(?);
  Inherited_List_Sets(Machine(BIT))==(?);
  List_Enumerated(Machine(BIT))==(?);
  List_Defered(Machine(BIT))==(?);
  List_Sets(Machine(BIT))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(BIT))==(?);
  Expanded_List_HiddenConstants(Machine(BIT))==(?);
  List_HiddenConstants(Machine(BIT))==(?);
  External_List_HiddenConstants(Machine(BIT))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(BIT))==(btrue);
  Context_List_Properties(Machine(BIT))==(btrue);
  Inherited_List_Properties(Machine(BIT))==(btrue);
  List_Properties(Machine(BIT))==(BIT_CARRIER = 0..1 & BIT_NOT: BIT_CARRIER --> BIT_CARRIER & !bb.(bb: BIT_CARRIER => BIT_NOT(bb) = 1-bb) & BIT_AND: BIT_CARRIER*BIT_CARRIER --> BIT_CARRIER & !(b1,b2).(b1: BIT_CARRIER & b2: BIT_CARRIER => BIT_AND(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & BIT_IOR: BIT_CARRIER*BIT_CARRIER --> BIT_CARRIER & !(b1,b2).(b1: BIT_CARRIER & b2: BIT_CARRIER => BIT_IOR(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & BIT_XOR: BIT_CARRIER*BIT_CARRIER --> BIT_CARRIER & !(b1,b2).(b1: BIT_CARRIER & b2: BIT_CARRIER => BIT_XOR(b1,b2) = 1 <=> b1/=b2) & BOOL_TO_BIT: BOOL --> BIT_CARRIER & BOOL_TO_BIT = {TRUE|->1,FALSE|->0})
END
&
THEORY ListSeenInfoX END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(BIT),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(BIT)) == (BIT_CARRIER,BIT_NOT,BIT_AND,BIT_IOR,BIT_XOR,BOOL_TO_BIT | ? | ? | ? | ? | ? | ? | ? | BIT);
  List_Of_HiddenCst_Ids(Machine(BIT)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT)) == (BIT_CARRIER,BIT_NOT,BIT_AND,BIT_IOR,BIT_XOR,BOOL_TO_BIT);
  List_Of_VisibleVar_Ids(Machine(BIT)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT)) == (?: ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(BIT)) == (Type(BIT_CARRIER) == Cst(SetOf(btype(INTEGER,"[BIT_CARRIER","]BIT_CARRIER")));Type(BIT_NOT) == Cst(SetOf(btype(INTEGER,"[BIT_CARRIER","]BIT_CARRIER")*btype(INTEGER,"[BIT_CARRIER","]BIT_CARRIER")));Type(BIT_AND) == Cst(SetOf(btype(INTEGER,"[BIT_CARRIER","]BIT_CARRIER")*btype(INTEGER,"[BIT_CARRIER","]BIT_CARRIER")*btype(INTEGER,"[BIT_CARRIER","]BIT_CARRIER")));Type(BIT_IOR) == Cst(SetOf(btype(INTEGER,"[BIT_CARRIER","]BIT_CARRIER")*btype(INTEGER,"[BIT_CARRIER","]BIT_CARRIER")*btype(INTEGER,"[BIT_CARRIER","]BIT_CARRIER")));Type(BIT_XOR) == Cst(SetOf(btype(INTEGER,"[BIT_CARRIER","]BIT_CARRIER")*btype(INTEGER,"[BIT_CARRIER","]BIT_CARRIER")*btype(INTEGER,"[BIT_CARRIER","]BIT_CARRIER")));Type(BOOL_TO_BIT) == Cst(SetOf(btype(BOOL,0,1)*btype(INTEGER,"[BIT_CARRIER","]BIT_CARRIER"))))
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
