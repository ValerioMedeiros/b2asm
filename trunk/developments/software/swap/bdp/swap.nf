Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(swap))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(swap))==(Machine(swap));
  Level(Machine(swap))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(swap)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(swap))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(swap))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(swap))==(?);
  List_Includes(Machine(swap))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(swap))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(swap))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(swap))==(?);
  Context_List_Variables(Machine(swap))==(?);
  Abstract_List_Variables(Machine(swap))==(?);
  Local_List_Variables(Machine(swap))==(b,a);
  List_Variables(Machine(swap))==(b,a);
  External_List_Variables(Machine(swap))==(b,a)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(swap))==(?);
  Abstract_List_VisibleVariables(Machine(swap))==(?);
  External_List_VisibleVariables(Machine(swap))==(?);
  Expanded_List_VisibleVariables(Machine(swap))==(?);
  List_VisibleVariables(Machine(swap))==(?);
  Internal_List_VisibleVariables(Machine(swap))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(swap))==(btrue);
  Gluing_List_Invariant(Machine(swap))==(btrue);
  Expanded_List_Invariant(Machine(swap))==(btrue);
  Abstract_List_Invariant(Machine(swap))==(btrue);
  Context_List_Invariant(Machine(swap))==(btrue);
  List_Invariant(Machine(swap))==(a : INTEGER & b : INTEGER & a = b)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(swap))==(btrue);
  Abstract_List_Assertions(Machine(swap))==(btrue);
  Context_List_Assertions(Machine(swap))==(btrue);
  List_Assertions(Machine(swap))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(swap))==(a,b:=0,0);
  Context_List_Initialisation(Machine(swap))==(skip);
  List_Initialisation(Machine(swap))==(a,b:=0,0)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(swap))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(swap))==(btrue);
  List_Constraints(Machine(swap))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(swap))==(run);
  List_Operations(Machine(swap))==(run)
END
&
THEORY ListInputX IS
  List_Input(Machine(swap),run)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(swap),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(swap),run)==(run)
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(swap),run)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(swap),run)==(btrue | a,b:=b,a);
  List_Substitution(Machine(swap),run)==(a:=b || b:=a)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(swap))==(?);
  Inherited_List_Constants(Machine(swap))==(?);
  List_Constants(Machine(swap))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(swap))==(?);
  Context_List_Defered(Machine(swap))==(?);
  Context_List_Sets(Machine(swap))==(?);
  List_Valuable_Sets(Machine(swap))==(?);
  Inherited_List_Enumerated(Machine(swap))==(?);
  Inherited_List_Defered(Machine(swap))==(?);
  Inherited_List_Sets(Machine(swap))==(?);
  List_Enumerated(Machine(swap))==(?);
  List_Defered(Machine(swap))==(?);
  List_Sets(Machine(swap))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(swap))==(?);
  Expanded_List_HiddenConstants(Machine(swap))==(?);
  List_HiddenConstants(Machine(swap))==(?);
  External_List_HiddenConstants(Machine(swap))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(swap))==(btrue);
  Context_List_Properties(Machine(swap))==(btrue);
  Inherited_List_Properties(Machine(swap))==(btrue);
  List_Properties(Machine(swap))==(btrue)
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(swap)) == (? | ? | b,a | ? | run | ? | ? | ? | swap);
  List_Of_HiddenCst_Ids(Machine(swap)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(swap)) == (?);
  List_Of_VisibleVar_Ids(Machine(swap)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(swap)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(swap)) == (Type(b) == Mvl(btype(INTEGER,?,?));Type(a) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(swap)) == (Type(run) == Cst(No_type,No_type))
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
