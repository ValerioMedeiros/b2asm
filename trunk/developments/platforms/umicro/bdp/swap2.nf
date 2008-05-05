Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(swap2))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(swap2))==(Machine(swap2));
  Level(Machine(swap2))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(swap2)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(swap2))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(swap2))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(swap2))==(?);
  List_Includes(Machine(swap2))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(swap2))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(swap2))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(swap2))==(?);
  Context_List_Variables(Machine(swap2))==(?);
  Abstract_List_Variables(Machine(swap2))==(?);
  Local_List_Variables(Machine(swap2))==(b,a);
  List_Variables(Machine(swap2))==(b,a);
  External_List_Variables(Machine(swap2))==(b,a)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(swap2))==(?);
  Abstract_List_VisibleVariables(Machine(swap2))==(?);
  External_List_VisibleVariables(Machine(swap2))==(?);
  Expanded_List_VisibleVariables(Machine(swap2))==(?);
  List_VisibleVariables(Machine(swap2))==(?);
  Internal_List_VisibleVariables(Machine(swap2))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(swap2))==(btrue);
  Gluing_List_Invariant(Machine(swap2))==(btrue);
  Expanded_List_Invariant(Machine(swap2))==(btrue);
  Abstract_List_Invariant(Machine(swap2))==(btrue);
  Context_List_Invariant(Machine(swap2))==(btrue);
  List_Invariant(Machine(swap2))==(a : INT & b : INT)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(swap2))==(btrue);
  Abstract_List_Assertions(Machine(swap2))==(btrue);
  Context_List_Assertions(Machine(swap2))==(btrue);
  List_Assertions(Machine(swap2))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(swap2))==(a,b:=0,0);
  Context_List_Initialisation(Machine(swap2))==(skip);
  List_Initialisation(Machine(swap2))==(a,b:=0,0)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(swap2))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(swap2))==(btrue);
  List_Constraints(Machine(swap2))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(swap2))==(swap);
  List_Operations(Machine(swap2))==(swap)
END
&
THEORY ListInputX IS
  List_Input(Machine(swap2),swap)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(swap2),swap)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(swap2),swap)==(swap)
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(swap2),swap)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(swap2),swap)==(btrue | a,b:=b,a);
  List_Substitution(Machine(swap2),swap)==(a,b:=b,a)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(swap2))==(?);
  Inherited_List_Constants(Machine(swap2))==(?);
  List_Constants(Machine(swap2))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(swap2))==(?);
  Context_List_Defered(Machine(swap2))==(?);
  Context_List_Sets(Machine(swap2))==(?);
  List_Valuable_Sets(Machine(swap2))==(?);
  Inherited_List_Enumerated(Machine(swap2))==(?);
  Inherited_List_Defered(Machine(swap2))==(?);
  Inherited_List_Sets(Machine(swap2))==(?);
  List_Enumerated(Machine(swap2))==(?);
  List_Defered(Machine(swap2))==(?);
  List_Sets(Machine(swap2))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(swap2))==(?);
  Expanded_List_HiddenConstants(Machine(swap2))==(?);
  List_HiddenConstants(Machine(swap2))==(?);
  External_List_HiddenConstants(Machine(swap2))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(swap2))==(btrue);
  Context_List_Properties(Machine(swap2))==(btrue);
  Inherited_List_Properties(Machine(swap2))==(btrue);
  List_Properties(Machine(swap2))==(btrue)
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(swap2)) == (? | ? | b,a | ? | swap | ? | ? | ? | swap2);
  List_Of_HiddenCst_Ids(Machine(swap2)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(swap2)) == (?);
  List_Of_VisibleVar_Ids(Machine(swap2)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(swap2)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(swap2)) == (Type(b) == Mvl(btype(INTEGER,?,?));Type(a) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(swap2)) == (Type(swap) == Cst(No_type,No_type))
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
