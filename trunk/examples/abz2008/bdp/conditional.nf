Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(conditional))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(conditional))==(Machine(conditional));
  Level(Machine(conditional))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(conditional)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(conditional))==(types)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(conditional))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(conditional))==(?);
  List_Includes(Machine(conditional))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(conditional))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(conditional))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(conditional))==(?);
  Context_List_Variables(Machine(conditional))==(?);
  Abstract_List_Variables(Machine(conditional))==(?);
  Local_List_Variables(Machine(conditional))==(st);
  List_Variables(Machine(conditional))==(st);
  External_List_Variables(Machine(conditional))==(st)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(conditional))==(?);
  Abstract_List_VisibleVariables(Machine(conditional))==(?);
  External_List_VisibleVariables(Machine(conditional))==(?);
  Expanded_List_VisibleVariables(Machine(conditional))==(?);
  List_VisibleVariables(Machine(conditional))==(?);
  Internal_List_VisibleVariables(Machine(conditional))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(conditional))==(btrue);
  Gluing_List_Invariant(Machine(conditional))==(btrue);
  Expanded_List_Invariant(Machine(conditional))==(btrue);
  Abstract_List_Invariant(Machine(conditional))==(btrue);
  Context_List_Invariant(Machine(conditional))==(btrue);
  List_Invariant(Machine(conditional))==(st : POW(uint32))
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(conditional))==(btrue);
  Abstract_List_Assertions(Machine(conditional))==(btrue);
  Context_List_Assertions(Machine(conditional))==(btrue);
  List_Assertions(Machine(conditional))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(conditional))==(st:={});
  Context_List_Initialisation(Machine(conditional))==(skip);
  List_Initialisation(Machine(conditional))==(st:={})
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(conditional))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(conditional),Machine(types))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(conditional))==(btrue);
  List_Constraints(Machine(conditional))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(conditional))==(add,largest);
  List_Operations(Machine(conditional))==(add,largest)
END
&
THEORY ListInputX IS
  List_Input(Machine(conditional),add)==(v);
  List_Input(Machine(conditional),largest)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(conditional),add)==(?);
  List_Output(Machine(conditional),largest)==(res)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(conditional),add)==(add(v));
  List_Header(Machine(conditional),largest)==(res <-- largest)
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(conditional),add)==(v : uint32 & v/:st);
  List_Precondition(Machine(conditional),largest)==(st/={})
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(conditional),largest)==(st/={} | res:=max(st));
  Expanded_List_Substitution(Machine(conditional),add)==(v : uint32 & v/:st | st:=st\/{v});
  List_Substitution(Machine(conditional),add)==(st:=st\/{v});
  List_Substitution(Machine(conditional),largest)==(res:=max(st))
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(conditional))==(?);
  Inherited_List_Constants(Machine(conditional))==(?);
  List_Constants(Machine(conditional))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(conditional))==(?);
  Context_List_Defered(Machine(conditional))==(?);
  Context_List_Sets(Machine(conditional))==(?);
  List_Valuable_Sets(Machine(conditional))==(?);
  Inherited_List_Enumerated(Machine(conditional))==(?);
  Inherited_List_Defered(Machine(conditional))==(?);
  Inherited_List_Sets(Machine(conditional))==(?);
  List_Enumerated(Machine(conditional))==(?);
  List_Defered(Machine(conditional))==(?);
  List_Sets(Machine(conditional))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(conditional))==(?);
  Expanded_List_HiddenConstants(Machine(conditional))==(?);
  List_HiddenConstants(Machine(conditional))==(?);
  External_List_HiddenConstants(Machine(conditional))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(conditional))==(btrue);
  Context_List_Properties(Machine(conditional))==(uint32 : POW(INTEGER) & uint32 = 0..MAXINT & uint16 : POW(INTEGER) & uint16 = 0..65535);
  Inherited_List_Properties(Machine(conditional))==(btrue);
  List_Properties(Machine(conditional))==(btrue)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(conditional),Machine(types))==(?);
  Seen_Context_List_Enumerated(Machine(conditional))==(?);
  Seen_Context_List_Invariant(Machine(conditional))==(btrue);
  Seen_Context_List_Assertions(Machine(conditional))==(btrue);
  Seen_Context_List_Properties(Machine(conditional))==(btrue);
  Seen_List_Constraints(Machine(conditional))==(btrue);
  Seen_List_Operations(Machine(conditional),Machine(types))==(?);
  Seen_Expanded_List_Invariant(Machine(conditional),Machine(types))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(conditional)) == (? | ? | st | ? | add,largest | ? | seen(Machine(types)) | ? | conditional);
  List_Of_HiddenCst_Ids(Machine(conditional)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(conditional)) == (?);
  List_Of_VisibleVar_Ids(Machine(conditional)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(conditional)) == (? : ?);
  List_Of_Ids(Machine(types)) == (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | types);
  List_Of_HiddenCst_Ids(Machine(types)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(types)) == (uint32,uint16);
  List_Of_VisibleVar_Ids(Machine(types)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(types)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(conditional)) == (Type(st) == Mvl(SetOf(btype(INTEGER,?,?))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(conditional)) == (Type(largest) == Cst(btype(INTEGER,?,?),No_type);Type(add) == Cst(No_type,btype(INTEGER,?,?)));
  Observers(Machine(conditional)) == (Type(largest) == Cst(btype(INTEGER,?,?),No_type))
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
