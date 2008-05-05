Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Nat))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Nat))==(Machine(Nat));
  Level(Machine(Nat))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Nat)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Nat))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Nat))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Nat))==(?);
  List_Includes(Machine(Nat))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Nat))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Nat))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Nat))==(?);
  Context_List_Variables(Machine(Nat))==(?);
  Abstract_List_Variables(Machine(Nat))==(?);
  Local_List_Variables(Machine(Nat))==(?);
  List_Variables(Machine(Nat))==(?);
  External_List_Variables(Machine(Nat))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Nat))==(?);
  Abstract_List_VisibleVariables(Machine(Nat))==(?);
  External_List_VisibleVariables(Machine(Nat))==(?);
  Expanded_List_VisibleVariables(Machine(Nat))==(?);
  List_VisibleVariables(Machine(Nat))==(value);
  Internal_List_VisibleVariables(Machine(Nat))==(value)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Nat))==(btrue);
  Gluing_List_Invariant(Machine(Nat))==(btrue);
  Expanded_List_Invariant(Machine(Nat))==(btrue);
  Abstract_List_Invariant(Machine(Nat))==(btrue);
  Context_List_Invariant(Machine(Nat))==(btrue);
  List_Invariant(Machine(Nat))==(value : NATURAL & value<256)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(Nat))==(btrue);
  Abstract_List_Assertions(Machine(Nat))==(btrue);
  Context_List_Assertions(Machine(Nat))==(btrue);
  List_Assertions(Machine(Nat))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Nat))==(value:=0);
  Context_List_Initialisation(Machine(Nat))==(skip);
  List_Initialisation(Machine(Nat))==(value:=0)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Nat))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(Nat))==(btrue);
  List_Constraints(Machine(Nat))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Nat))==(set,get);
  List_Operations(Machine(Nat))==(set,get)
END
&
THEORY ListInputX IS
  List_Input(Machine(Nat),set)==(new_value);
  List_Input(Machine(Nat),get)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Nat),set)==(?);
  List_Output(Machine(Nat),get)==(res)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Nat),set)==(set(new_value));
  List_Header(Machine(Nat),get)==(res <-- get)
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Nat),set)==(new_value : NATURAL & new_value<256);
  List_Precondition(Machine(Nat),get)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Nat),get)==(btrue | res:=value);
  Expanded_List_Substitution(Machine(Nat),set)==(new_value : NATURAL & new_value<256 | value:=new_value);
  List_Substitution(Machine(Nat),set)==(value:=new_value);
  List_Substitution(Machine(Nat),get)==(res:=value)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Nat))==(?);
  Inherited_List_Constants(Machine(Nat))==(?);
  List_Constants(Machine(Nat))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(Nat))==(?);
  Context_List_Defered(Machine(Nat))==(?);
  Context_List_Sets(Machine(Nat))==(?);
  List_Valuable_Sets(Machine(Nat))==(?);
  Inherited_List_Enumerated(Machine(Nat))==(?);
  Inherited_List_Defered(Machine(Nat))==(?);
  Inherited_List_Sets(Machine(Nat))==(?);
  List_Enumerated(Machine(Nat))==(?);
  List_Defered(Machine(Nat))==(?);
  List_Sets(Machine(Nat))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Nat))==(?);
  Expanded_List_HiddenConstants(Machine(Nat))==(?);
  List_HiddenConstants(Machine(Nat))==(?);
  External_List_HiddenConstants(Machine(Nat))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Nat))==(btrue);
  Context_List_Properties(Machine(Nat))==(btrue);
  Inherited_List_Properties(Machine(Nat))==(btrue);
  List_Properties(Machine(Nat))==(btrue)
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Nat)) == (? | ? | ? | ? | set,get | ? | ? | ? | Nat);
  List_Of_HiddenCst_Ids(Machine(Nat)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Nat)) == (?);
  List_Of_VisibleVar_Ids(Machine(Nat)) == (value | ?);
  List_Of_Ids_SeenBNU(Machine(Nat)) == (? : ?)
END
&
THEORY VisibleVariablesEnvX IS
  VisibleVariables(Machine(Nat)) == (Type(value) == Mvv(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Nat)) == (Type(get) == Cst(btype(INTEGER,?,?),No_type);Type(set) == Cst(No_type,btype(INTEGER,?,?)));
  Observers(Machine(Nat)) == (Type(get) == Cst(btype(INTEGER,?,?),No_type))
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
