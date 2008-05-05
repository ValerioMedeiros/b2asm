Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(int))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(int))==(Machine(int));
  Level(Machine(int))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(int)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(int))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(int))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(int))==(?);
  List_Includes(Machine(int))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(int))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(int))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(int))==(?);
  Context_List_Variables(Machine(int))==(?);
  Abstract_List_Variables(Machine(int))==(?);
  Local_List_Variables(Machine(int))==(value);
  List_Variables(Machine(int))==(value);
  External_List_Variables(Machine(int))==(value)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(int))==(?);
  Abstract_List_VisibleVariables(Machine(int))==(?);
  External_List_VisibleVariables(Machine(int))==(?);
  Expanded_List_VisibleVariables(Machine(int))==(?);
  List_VisibleVariables(Machine(int))==(?);
  Internal_List_VisibleVariables(Machine(int))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(int))==(btrue);
  Gluing_List_Invariant(Machine(int))==(btrue);
  Expanded_List_Invariant(Machine(int))==(btrue);
  Abstract_List_Invariant(Machine(int))==(btrue);
  Context_List_Invariant(Machine(int))==(btrue);
  List_Invariant(Machine(int))==(value : INTEGER)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(int))==(btrue);
  Abstract_List_Assertions(Machine(int))==(btrue);
  Context_List_Assertions(Machine(int))==(btrue);
  List_Assertions(Machine(int))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(int))==(@(value$0).(value$0 : INTEGER ==> value:=value$0));
  Context_List_Initialisation(Machine(int))==(skip);
  List_Initialisation(Machine(int))==(value:: INTEGER)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(int))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(int))==(btrue);
  List_Constraints(Machine(int))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(int))==(get,set);
  List_Operations(Machine(int))==(get,set)
END
&
THEORY ListInputX IS
  List_Input(Machine(int),get)==(?);
  List_Input(Machine(int),set)==(v)
END
&
THEORY ListOutputX IS
  List_Output(Machine(int),get)==(v);
  List_Output(Machine(int),set)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(int),get)==(v <-- get);
  List_Header(Machine(int),set)==(set(v))
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(int),get)==(btrue);
  List_Precondition(Machine(int),set)==(v : INTEGER)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(int),set)==(v : INTEGER | value:=v);
  Expanded_List_Substitution(Machine(int),get)==(btrue | v:=value);
  List_Substitution(Machine(int),get)==(v:=value);
  List_Substitution(Machine(int),set)==(value:=v)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(int))==(?);
  Inherited_List_Constants(Machine(int))==(?);
  List_Constants(Machine(int))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(int))==(?);
  Context_List_Defered(Machine(int))==(?);
  Context_List_Sets(Machine(int))==(?);
  List_Valuable_Sets(Machine(int))==(?);
  Inherited_List_Enumerated(Machine(int))==(?);
  Inherited_List_Defered(Machine(int))==(?);
  Inherited_List_Sets(Machine(int))==(?);
  List_Enumerated(Machine(int))==(?);
  List_Defered(Machine(int))==(?);
  List_Sets(Machine(int))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(int))==(?);
  Expanded_List_HiddenConstants(Machine(int))==(?);
  List_HiddenConstants(Machine(int))==(?);
  External_List_HiddenConstants(Machine(int))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(int))==(btrue);
  Context_List_Properties(Machine(int))==(btrue);
  Inherited_List_Properties(Machine(int))==(btrue);
  List_Properties(Machine(int))==(btrue)
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(int)) == (? | ? | value | ? | get,set | ? | ? | ? | int);
  List_Of_HiddenCst_Ids(Machine(int)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(int)) == (?);
  List_Of_VisibleVar_Ids(Machine(int)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(int)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(int)) == (Type(value) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(int)) == (Type(set) == Cst(No_type,btype(INTEGER,?,?));Type(get) == Cst(btype(INTEGER,?,?),No_type));
  Observers(Machine(int)) == (Type(get) == Cst(btype(INTEGER,?,?),No_type))
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
