Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(sequence))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(sequence))==(Machine(sequence));
  Level(Machine(sequence))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(sequence)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(sequence))==(types)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(sequence))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(sequence))==(?);
  List_Includes(Machine(sequence))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(sequence))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(sequence))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(sequence))==(?);
  Context_List_Variables(Machine(sequence))==(?);
  Abstract_List_Variables(Machine(sequence))==(?);
  Local_List_Variables(Machine(sequence))==(b,a);
  List_Variables(Machine(sequence))==(b,a);
  External_List_Variables(Machine(sequence))==(b,a)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(sequence))==(?);
  Abstract_List_VisibleVariables(Machine(sequence))==(?);
  External_List_VisibleVariables(Machine(sequence))==(?);
  Expanded_List_VisibleVariables(Machine(sequence))==(?);
  List_VisibleVariables(Machine(sequence))==(?);
  Internal_List_VisibleVariables(Machine(sequence))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(sequence))==(btrue);
  Gluing_List_Invariant(Machine(sequence))==(btrue);
  Expanded_List_Invariant(Machine(sequence))==(btrue);
  Abstract_List_Invariant(Machine(sequence))==(btrue);
  Context_List_Invariant(Machine(sequence))==(btrue);
  List_Invariant(Machine(sequence))==(a : uint32 & b : uint32)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(sequence))==(btrue);
  Abstract_List_Assertions(Machine(sequence))==(btrue);
  Context_List_Assertions(Machine(sequence))==(btrue);
  List_Assertions(Machine(sequence))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(sequence))==(@(a$0).(a$0 : uint32 ==> a:=a$0) || @(b$0).(b$0 : uint32 ==> b:=b$0));
  Context_List_Initialisation(Machine(sequence))==(skip);
  List_Initialisation(Machine(sequence))==(a:: uint32 || b:: uint32)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(sequence))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(sequence),Machine(types))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(sequence))==(btrue);
  List_Constraints(Machine(sequence))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(sequence))==(run);
  List_Operations(Machine(sequence))==(run)
END
&
THEORY ListInputX IS
  List_Input(Machine(sequence),run)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(sequence),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(sequence),run)==(run)
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(sequence),run)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(sequence),run)==(btrue | a,b:=b,a);
  List_Substitution(Machine(sequence),run)==(a:=b || b:=a)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(sequence))==(?);
  Inherited_List_Constants(Machine(sequence))==(?);
  List_Constants(Machine(sequence))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(sequence))==(?);
  Context_List_Defered(Machine(sequence))==(?);
  Context_List_Sets(Machine(sequence))==(?);
  List_Valuable_Sets(Machine(sequence))==(?);
  Inherited_List_Enumerated(Machine(sequence))==(?);
  Inherited_List_Defered(Machine(sequence))==(?);
  Inherited_List_Sets(Machine(sequence))==(?);
  List_Enumerated(Machine(sequence))==(?);
  List_Defered(Machine(sequence))==(?);
  List_Sets(Machine(sequence))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(sequence))==(?);
  Expanded_List_HiddenConstants(Machine(sequence))==(?);
  List_HiddenConstants(Machine(sequence))==(?);
  External_List_HiddenConstants(Machine(sequence))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(sequence))==(btrue);
  Context_List_Properties(Machine(sequence))==(uint32 : POW(INTEGER) & uint32 = 0..MAXINT & uint16 : POW(INTEGER) & uint16 = 0..65535);
  Inherited_List_Properties(Machine(sequence))==(btrue);
  List_Properties(Machine(sequence))==(btrue)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(sequence),Machine(types))==(?);
  Seen_Context_List_Enumerated(Machine(sequence))==(?);
  Seen_Context_List_Invariant(Machine(sequence))==(btrue);
  Seen_Context_List_Assertions(Machine(sequence))==(btrue);
  Seen_Context_List_Properties(Machine(sequence))==(btrue);
  Seen_List_Constraints(Machine(sequence))==(btrue);
  Seen_List_Operations(Machine(sequence),Machine(types))==(?);
  Seen_Expanded_List_Invariant(Machine(sequence),Machine(types))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(sequence)) == (? | ? | b,a | ? | run | ? | seen(Machine(types)) | ? | sequence);
  List_Of_HiddenCst_Ids(Machine(sequence)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(sequence)) == (?);
  List_Of_VisibleVar_Ids(Machine(sequence)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(sequence)) == (? : ?);
  List_Of_Ids(Machine(types)) == (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | types);
  List_Of_HiddenCst_Ids(Machine(types)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(types)) == (uint32,uint16);
  List_Of_VisibleVar_Ids(Machine(types)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(types)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(sequence)) == (Type(b) == Mvl(btype(INTEGER,?,?));Type(a) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(sequence)) == (Type(run) == Cst(No_type,No_type))
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
