Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(addition))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(addition))==(Machine(addition));
  Level(Machine(addition))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(addition)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(addition))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(addition))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(addition))==(?);
  List_Includes(Machine(addition))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(addition))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(addition))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(addition))==(?);
  Context_List_Variables(Machine(addition))==(?);
  Abstract_List_Variables(Machine(addition))==(?);
  Local_List_Variables(Machine(addition))==(s,b,a);
  List_Variables(Machine(addition))==(s,b,a);
  External_List_Variables(Machine(addition))==(s,b,a)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(addition))==(?);
  Abstract_List_VisibleVariables(Machine(addition))==(?);
  External_List_VisibleVariables(Machine(addition))==(?);
  Expanded_List_VisibleVariables(Machine(addition))==(?);
  List_VisibleVariables(Machine(addition))==(?);
  Internal_List_VisibleVariables(Machine(addition))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(addition))==(btrue);
  Gluing_List_Invariant(Machine(addition))==(btrue);
  Expanded_List_Invariant(Machine(addition))==(btrue);
  Abstract_List_Invariant(Machine(addition))==(btrue);
  Context_List_Invariant(Machine(addition))==(btrue);
  List_Invariant(Machine(addition))==(a : NATURAL & b : NATURAL & s : NATURAL & a+b<=MAXINT & s = a+b)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(addition))==(btrue);
  Abstract_List_Assertions(Machine(addition))==(btrue);
  Context_List_Assertions(Machine(addition))==(btrue);
  List_Assertions(Machine(addition))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(addition))==(a,b,s:=0,0,0);
  Context_List_Initialisation(Machine(addition))==(skip);
  List_Initialisation(Machine(addition))==(a,b,s:=0,0,0)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(addition))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(addition))==(btrue);
  List_Constraints(Machine(addition))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(addition))==(run);
  List_Operations(Machine(addition))==(run)
END
&
THEORY ListInputX IS
  List_Input(Machine(addition),run)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(addition),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(addition),run)==(run)
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(addition),run)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(addition),run)==(btrue | s:=a+b);
  List_Substitution(Machine(addition),run)==(s:=a+b)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(addition))==(?);
  Inherited_List_Constants(Machine(addition))==(?);
  List_Constants(Machine(addition))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(addition))==(?);
  Context_List_Defered(Machine(addition))==(?);
  Context_List_Sets(Machine(addition))==(?);
  List_Valuable_Sets(Machine(addition))==(?);
  Inherited_List_Enumerated(Machine(addition))==(?);
  Inherited_List_Defered(Machine(addition))==(?);
  Inherited_List_Sets(Machine(addition))==(?);
  List_Enumerated(Machine(addition))==(?);
  List_Defered(Machine(addition))==(?);
  List_Sets(Machine(addition))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(addition))==(?);
  Expanded_List_HiddenConstants(Machine(addition))==(?);
  List_HiddenConstants(Machine(addition))==(?);
  External_List_HiddenConstants(Machine(addition))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(addition))==(btrue);
  Context_List_Properties(Machine(addition))==(btrue);
  Inherited_List_Properties(Machine(addition))==(btrue);
  List_Properties(Machine(addition))==(btrue)
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(addition)) == (? | ? | s,b,a | ? | run | ? | ? | ? | addition);
  List_Of_HiddenCst_Ids(Machine(addition)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(addition)) == (?);
  List_Of_VisibleVar_Ids(Machine(addition)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(addition)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(addition)) == (Type(s) == Mvl(btype(INTEGER,?,?));Type(b) == Mvl(btype(INTEGER,?,?));Type(a) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(addition)) == (Type(run) == Cst(No_type,No_type))
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
