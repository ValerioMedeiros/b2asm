Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(VarNatural))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(VarNatural))==(Machine(VarNatural));
  Level(Machine(VarNatural))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(VarNatural)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(VarNatural))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(VarNatural))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(VarNatural))==(?);
  List_Includes(Machine(VarNatural))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(VarNatural))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(VarNatural))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(VarNatural))==(?);
  Context_List_Variables(Machine(VarNatural))==(?);
  Abstract_List_Variables(Machine(VarNatural))==(?);
  Local_List_Variables(Machine(VarNatural))==(?);
  List_Variables(Machine(VarNatural))==(?);
  External_List_Variables(Machine(VarNatural))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(VarNatural))==(?);
  Abstract_List_VisibleVariables(Machine(VarNatural))==(?);
  External_List_VisibleVariables(Machine(VarNatural))==(?);
  Expanded_List_VisibleVariables(Machine(VarNatural))==(?);
  List_VisibleVariables(Machine(VarNatural))==(value);
  Internal_List_VisibleVariables(Machine(VarNatural))==(value)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(VarNatural))==(btrue);
  Gluing_List_Invariant(Machine(VarNatural))==(btrue);
  Expanded_List_Invariant(Machine(VarNatural))==(btrue);
  Abstract_List_Invariant(Machine(VarNatural))==(btrue);
  Context_List_Invariant(Machine(VarNatural))==(btrue);
  List_Invariant(Machine(VarNatural))==(value : NATURAL)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(VarNatural))==(btrue);
  Abstract_List_Assertions(Machine(VarNatural))==(btrue);
  Context_List_Assertions(Machine(VarNatural))==(btrue);
  List_Assertions(Machine(VarNatural))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(VarNatural))==(value:=0);
  Context_List_Initialisation(Machine(VarNatural))==(skip);
  List_Initialisation(Machine(VarNatural))==(value:=0)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(VarNatural))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(VarNatural))==(btrue);
  List_Constraints(Machine(VarNatural))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(VarNatural))==(set,get);
  List_Operations(Machine(VarNatural))==(set,get)
END
&
THEORY ListInputX IS
  List_Input(Machine(VarNatural),set)==(new_value);
  List_Input(Machine(VarNatural),get)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(VarNatural),set)==(?);
  List_Output(Machine(VarNatural),get)==(res)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(VarNatural),set)==(set(new_value));
  List_Header(Machine(VarNatural),get)==(res <-- get)
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(VarNatural),set)==(new_value : NATURAL);
  List_Precondition(Machine(VarNatural),get)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(VarNatural),get)==(btrue | res:=value);
  Expanded_List_Substitution(Machine(VarNatural),set)==(new_value : NATURAL | value:=new_value);
  List_Substitution(Machine(VarNatural),set)==(value:=new_value);
  List_Substitution(Machine(VarNatural),get)==(res:=value)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(VarNatural))==(?);
  Inherited_List_Constants(Machine(VarNatural))==(?);
  List_Constants(Machine(VarNatural))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(VarNatural))==(?);
  Context_List_Defered(Machine(VarNatural))==(?);
  Context_List_Sets(Machine(VarNatural))==(?);
  List_Valuable_Sets(Machine(VarNatural))==(?);
  Inherited_List_Enumerated(Machine(VarNatural))==(?);
  Inherited_List_Defered(Machine(VarNatural))==(?);
  Inherited_List_Sets(Machine(VarNatural))==(?);
  List_Enumerated(Machine(VarNatural))==(?);
  List_Defered(Machine(VarNatural))==(?);
  List_Sets(Machine(VarNatural))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(VarNatural))==(?);
  Expanded_List_HiddenConstants(Machine(VarNatural))==(?);
  List_HiddenConstants(Machine(VarNatural))==(?);
  External_List_HiddenConstants(Machine(VarNatural))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(VarNatural))==(btrue);
  Context_List_Properties(Machine(VarNatural))==(btrue);
  Inherited_List_Properties(Machine(VarNatural))==(btrue);
  List_Properties(Machine(VarNatural))==(btrue)
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(VarNatural)) == (? | ? | ? | ? | set,get | ? | ? | ? | VarNatural);
  List_Of_HiddenCst_Ids(Machine(VarNatural)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(VarNatural)) == (?);
  List_Of_VisibleVar_Ids(Machine(VarNatural)) == (value | ?);
  List_Of_Ids_SeenBNU(Machine(VarNatural)) == (? : ?)
END
&
THEORY VisibleVariablesEnvX IS
  VisibleVariables(Machine(VarNatural)) == (Type(value) == Mvv(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(VarNatural)) == (Type(get) == Cst(btype(INTEGER,?,?),No_type);Type(set) == Cst(No_type,btype(INTEGER,?,?)));
  Observers(Machine(VarNatural)) == (Type(get) == Cst(btype(INTEGER,?,?),No_type))
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
