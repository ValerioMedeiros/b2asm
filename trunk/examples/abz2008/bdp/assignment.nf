Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(assignment))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(assignment))==(Machine(assignment));
  Level(Machine(assignment))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(assignment)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(assignment))==(types)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(assignment))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(assignment))==(?);
  List_Includes(Machine(assignment))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(assignment))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(assignment))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(assignment))==(?);
  Context_List_Variables(Machine(assignment))==(?);
  Abstract_List_Variables(Machine(assignment))==(?);
  Local_List_Variables(Machine(assignment))==(v);
  List_Variables(Machine(assignment))==(v);
  External_List_Variables(Machine(assignment))==(v)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(assignment))==(?);
  Abstract_List_VisibleVariables(Machine(assignment))==(?);
  External_List_VisibleVariables(Machine(assignment))==(?);
  Expanded_List_VisibleVariables(Machine(assignment))==(?);
  List_VisibleVariables(Machine(assignment))==(?);
  Internal_List_VisibleVariables(Machine(assignment))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(assignment))==(btrue);
  Gluing_List_Invariant(Machine(assignment))==(btrue);
  Expanded_List_Invariant(Machine(assignment))==(btrue);
  Abstract_List_Invariant(Machine(assignment))==(btrue);
  Context_List_Invariant(Machine(assignment))==(btrue);
  List_Invariant(Machine(assignment))==(v : uint32)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(assignment))==(btrue);
  Abstract_List_Assertions(Machine(assignment))==(btrue);
  Context_List_Assertions(Machine(assignment))==(btrue);
  List_Assertions(Machine(assignment))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(assignment))==(@(v$0).(v$0 : uint32 ==> v:=v$0));
  Context_List_Initialisation(Machine(assignment))==(skip);
  List_Initialisation(Machine(assignment))==(v:: uint32)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(assignment))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(assignment),Machine(types))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(assignment))==(btrue);
  List_Constraints(Machine(assignment))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(assignment))==(run);
  List_Operations(Machine(assignment))==(run)
END
&
THEORY ListInputX IS
  List_Input(Machine(assignment),run)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(assignment),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(assignment),run)==(run)
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(assignment),run)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(assignment),run)==(btrue | v:=0);
  List_Substitution(Machine(assignment),run)==(v:=0)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(assignment))==(?);
  Inherited_List_Constants(Machine(assignment))==(?);
  List_Constants(Machine(assignment))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(assignment))==(?);
  Context_List_Defered(Machine(assignment))==(?);
  Context_List_Sets(Machine(assignment))==(?);
  List_Valuable_Sets(Machine(assignment))==(?);
  Inherited_List_Enumerated(Machine(assignment))==(?);
  Inherited_List_Defered(Machine(assignment))==(?);
  Inherited_List_Sets(Machine(assignment))==(?);
  List_Enumerated(Machine(assignment))==(?);
  List_Defered(Machine(assignment))==(?);
  List_Sets(Machine(assignment))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(assignment))==(?);
  Expanded_List_HiddenConstants(Machine(assignment))==(?);
  List_HiddenConstants(Machine(assignment))==(?);
  External_List_HiddenConstants(Machine(assignment))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(assignment))==(btrue);
  Context_List_Properties(Machine(assignment))==(uint32 : POW(INTEGER) & uint32 = 0..MAXINT & uint16 : POW(INTEGER) & uint16 = 0..65535);
  Inherited_List_Properties(Machine(assignment))==(btrue);
  List_Properties(Machine(assignment))==(btrue)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(assignment),Machine(types))==(?);
  Seen_Context_List_Enumerated(Machine(assignment))==(?);
  Seen_Context_List_Invariant(Machine(assignment))==(btrue);
  Seen_Context_List_Assertions(Machine(assignment))==(btrue);
  Seen_Context_List_Properties(Machine(assignment))==(btrue);
  Seen_List_Constraints(Machine(assignment))==(btrue);
  Seen_List_Operations(Machine(assignment),Machine(types))==(?);
  Seen_Expanded_List_Invariant(Machine(assignment),Machine(types))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(assignment)) == (? | ? | v | ? | run | ? | seen(Machine(types)) | ? | assignment);
  List_Of_HiddenCst_Ids(Machine(assignment)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(assignment)) == (?);
  List_Of_VisibleVar_Ids(Machine(assignment)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(assignment)) == (? : ?);
  List_Of_Ids(Machine(types)) == (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | types);
  List_Of_HiddenCst_Ids(Machine(types)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(types)) == (uint32,uint16);
  List_Of_VisibleVar_Ids(Machine(types)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(types)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(assignment)) == (Type(v) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(assignment)) == (Type(run) == Cst(No_type,No_type))
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
