Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(nat))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(nat))==(Machine(nat));
  Level(Machine(nat))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(nat)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(nat))==(types)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(nat))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(nat))==(?);
  List_Includes(Machine(nat))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(nat))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(nat))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(nat))==(?);
  Context_List_Variables(Machine(nat))==(?);
  Abstract_List_Variables(Machine(nat))==(?);
  Local_List_Variables(Machine(nat))==(value);
  List_Variables(Machine(nat))==(value);
  External_List_Variables(Machine(nat))==(value)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(nat))==(?);
  Abstract_List_VisibleVariables(Machine(nat))==(?);
  External_List_VisibleVariables(Machine(nat))==(?);
  Expanded_List_VisibleVariables(Machine(nat))==(?);
  List_VisibleVariables(Machine(nat))==(?);
  Internal_List_VisibleVariables(Machine(nat))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(nat))==(btrue);
  Gluing_List_Invariant(Machine(nat))==(btrue);
  Expanded_List_Invariant(Machine(nat))==(btrue);
  Abstract_List_Invariant(Machine(nat))==(btrue);
  Context_List_Invariant(Machine(nat))==(btrue);
  List_Invariant(Machine(nat))==(value : uint32)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(nat))==(btrue);
  Abstract_List_Assertions(Machine(nat))==(btrue);
  Context_List_Assertions(Machine(nat))==(btrue);
  List_Assertions(Machine(nat))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(nat))==(@(value$0).(value$0 : uint32 ==> value:=value$0));
  Context_List_Initialisation(Machine(nat))==(skip);
  List_Initialisation(Machine(nat))==(value:: uint32)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(nat))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(nat),Machine(types))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(nat))==(btrue);
  List_Constraints(Machine(nat))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(nat))==(get,set);
  List_Operations(Machine(nat))==(get,set)
END
&
THEORY ListInputX IS
  List_Input(Machine(nat),get)==(?);
  List_Input(Machine(nat),set)==(v)
END
&
THEORY ListOutputX IS
  List_Output(Machine(nat),get)==(v);
  List_Output(Machine(nat),set)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(nat),get)==(v <-- get);
  List_Header(Machine(nat),set)==(set(v))
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(nat),get)==(btrue);
  List_Precondition(Machine(nat),set)==(v : uint32)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(nat),set)==(v : uint32 | value:=v);
  Expanded_List_Substitution(Machine(nat),get)==(btrue | v:=value);
  List_Substitution(Machine(nat),get)==(v:=value);
  List_Substitution(Machine(nat),set)==(value:=v)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(nat))==(?);
  Inherited_List_Constants(Machine(nat))==(?);
  List_Constants(Machine(nat))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(nat))==(?);
  Context_List_Defered(Machine(nat))==(?);
  Context_List_Sets(Machine(nat))==(?);
  List_Valuable_Sets(Machine(nat))==(?);
  Inherited_List_Enumerated(Machine(nat))==(?);
  Inherited_List_Defered(Machine(nat))==(?);
  Inherited_List_Sets(Machine(nat))==(?);
  List_Enumerated(Machine(nat))==(?);
  List_Defered(Machine(nat))==(?);
  List_Sets(Machine(nat))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(nat))==(?);
  Expanded_List_HiddenConstants(Machine(nat))==(?);
  List_HiddenConstants(Machine(nat))==(?);
  External_List_HiddenConstants(Machine(nat))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(nat))==(btrue);
  Context_List_Properties(Machine(nat))==(uint32 : POW(INTEGER) & uint32 = 0..MAXINT);
  Inherited_List_Properties(Machine(nat))==(btrue);
  List_Properties(Machine(nat))==(btrue)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(nat),Machine(types))==(?);
  Seen_Context_List_Enumerated(Machine(nat))==(?);
  Seen_Context_List_Invariant(Machine(nat))==(btrue);
  Seen_Context_List_Assertions(Machine(nat))==(btrue);
  Seen_Context_List_Properties(Machine(nat))==(btrue);
  Seen_List_Constraints(Machine(nat))==(btrue);
  Seen_List_Operations(Machine(nat),Machine(types))==(?);
  Seen_Expanded_List_Invariant(Machine(nat),Machine(types))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(nat)) == (? | ? | value | ? | get,set | ? | seen(Machine(types)) | ? | nat);
  List_Of_HiddenCst_Ids(Machine(nat)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(nat)) == (?);
  List_Of_VisibleVar_Ids(Machine(nat)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(nat)) == (? : ?);
  List_Of_Ids(Machine(types)) == (uint32 | ? | ? | ? | ? | ? | ? | ? | types);
  List_Of_HiddenCst_Ids(Machine(types)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(types)) == (uint32);
  List_Of_VisibleVar_Ids(Machine(types)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(types)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(nat)) == (Type(value) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(nat)) == (Type(set) == Cst(No_type,btype(INTEGER,?,?));Type(get) == Cst(btype(INTEGER,?,?),No_type));
  Observers(Machine(nat)) == (Type(get) == Cst(btype(INTEGER,?,?),No_type))
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
