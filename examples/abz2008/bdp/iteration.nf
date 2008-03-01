Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(iteration))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(iteration))==(Machine(iteration));
  Level(Machine(iteration))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(iteration)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(iteration))==(types)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(iteration))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(iteration))==(?);
  List_Includes(Machine(iteration))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(iteration))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(iteration))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(iteration))==(?);
  Context_List_Variables(Machine(iteration))==(?);
  Abstract_List_Variables(Machine(iteration))==(?);
  Local_List_Variables(Machine(iteration))==(s,b,a);
  List_Variables(Machine(iteration))==(s,b,a);
  External_List_Variables(Machine(iteration))==(s,b,a)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(iteration))==(?);
  Abstract_List_VisibleVariables(Machine(iteration))==(?);
  External_List_VisibleVariables(Machine(iteration))==(?);
  Expanded_List_VisibleVariables(Machine(iteration))==(?);
  List_VisibleVariables(Machine(iteration))==(?);
  Internal_List_VisibleVariables(Machine(iteration))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(iteration))==(btrue);
  Gluing_List_Invariant(Machine(iteration))==(btrue);
  Expanded_List_Invariant(Machine(iteration))==(btrue);
  Abstract_List_Invariant(Machine(iteration))==(btrue);
  Context_List_Invariant(Machine(iteration))==(btrue);
  List_Invariant(Machine(iteration))==(a : uint32 & b : uint32 & s : uint32 & a+b : uint32 & s = a+b)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(iteration))==(btrue);
  Abstract_List_Assertions(Machine(iteration))==(btrue);
  Context_List_Assertions(Machine(iteration))==(btrue);
  List_Assertions(Machine(iteration))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(iteration))==(a,b,s:=0,0,0);
  Context_List_Initialisation(Machine(iteration))==(skip);
  List_Initialisation(Machine(iteration))==(a,b,s:=0,0,0)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(iteration))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(iteration),Machine(types))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(iteration))==(btrue);
  List_Constraints(Machine(iteration))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(iteration))==(run);
  List_Operations(Machine(iteration))==(run)
END
&
THEORY ListInputX IS
  List_Input(Machine(iteration),run)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(iteration),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(iteration),run)==(run)
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(iteration),run)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(iteration),run)==(btrue | s:=a+b);
  List_Substitution(Machine(iteration),run)==(s:=a+b)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(iteration))==(?);
  Inherited_List_Constants(Machine(iteration))==(?);
  List_Constants(Machine(iteration))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(iteration))==(?);
  Context_List_Defered(Machine(iteration))==(?);
  Context_List_Sets(Machine(iteration))==(?);
  List_Valuable_Sets(Machine(iteration))==(?);
  Inherited_List_Enumerated(Machine(iteration))==(?);
  Inherited_List_Defered(Machine(iteration))==(?);
  Inherited_List_Sets(Machine(iteration))==(?);
  List_Enumerated(Machine(iteration))==(?);
  List_Defered(Machine(iteration))==(?);
  List_Sets(Machine(iteration))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(iteration))==(?);
  Expanded_List_HiddenConstants(Machine(iteration))==(?);
  List_HiddenConstants(Machine(iteration))==(?);
  External_List_HiddenConstants(Machine(iteration))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(iteration))==(btrue);
  Context_List_Properties(Machine(iteration))==(uint32 : POW(INTEGER) & uint32 = 0..MAXINT & uint16 : POW(INTEGER) & uint16 = 0..65535);
  Inherited_List_Properties(Machine(iteration))==(btrue);
  List_Properties(Machine(iteration))==(btrue)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(iteration),Machine(types))==(?);
  Seen_Context_List_Enumerated(Machine(iteration))==(?);
  Seen_Context_List_Invariant(Machine(iteration))==(btrue);
  Seen_Context_List_Assertions(Machine(iteration))==(btrue);
  Seen_Context_List_Properties(Machine(iteration))==(btrue);
  Seen_List_Constraints(Machine(iteration))==(btrue);
  Seen_List_Operations(Machine(iteration),Machine(types))==(?);
  Seen_Expanded_List_Invariant(Machine(iteration),Machine(types))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(iteration)) == (? | ? | s,b,a | ? | run | ? | seen(Machine(types)) | ? | iteration);
  List_Of_HiddenCst_Ids(Machine(iteration)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(iteration)) == (?);
  List_Of_VisibleVar_Ids(Machine(iteration)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(iteration)) == (? : ?);
  List_Of_Ids(Machine(types)) == (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | types);
  List_Of_HiddenCst_Ids(Machine(types)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(types)) == (uint32,uint16);
  List_Of_VisibleVar_Ids(Machine(types)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(types)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(iteration)) == (Type(s) == Mvl(btype(INTEGER,?,?));Type(b) == Mvl(btype(INTEGER,?,?));Type(a) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(iteration)) == (Type(run) == Cst(No_type,No_type))
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
