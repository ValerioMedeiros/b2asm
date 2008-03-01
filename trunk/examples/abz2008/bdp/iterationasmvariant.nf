Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(iterationasmvariant))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(iterationasmvariant))==(Machine(iterationasmvariant));
  Level(Machine(iterationasmvariant))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(iterationasmvariant)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(iterationasmvariant))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(iterationasmvariant))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(iterationasmvariant))==(?);
  List_Includes(Machine(iterationasmvariant))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(iterationasmvariant))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(iterationasmvariant))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(iterationasmvariant))==(?);
  Context_List_Variables(Machine(iterationasmvariant))==(?);
  Abstract_List_Variables(Machine(iterationasmvariant))==(?);
  Local_List_Variables(Machine(iterationasmvariant))==(?);
  List_Variables(Machine(iterationasmvariant))==(?);
  External_List_Variables(Machine(iterationasmvariant))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(iterationasmvariant))==(?);
  Abstract_List_VisibleVariables(Machine(iterationasmvariant))==(?);
  External_List_VisibleVariables(Machine(iterationasmvariant))==(?);
  Expanded_List_VisibleVariables(Machine(iterationasmvariant))==(?);
  List_VisibleVariables(Machine(iterationasmvariant))==(?);
  Internal_List_VisibleVariables(Machine(iterationasmvariant))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(iterationasmvariant))==(btrue);
  Gluing_List_Invariant(Machine(iterationasmvariant))==(btrue);
  Expanded_List_Invariant(Machine(iterationasmvariant))==(btrue);
  Abstract_List_Invariant(Machine(iterationasmvariant))==(btrue);
  Context_List_Invariant(Machine(iterationasmvariant))==(btrue);
  List_Invariant(Machine(iterationasmvariant))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(iterationasmvariant))==(btrue);
  Abstract_List_Assertions(Machine(iterationasmvariant))==(btrue);
  Context_List_Assertions(Machine(iterationasmvariant))==(btrue);
  List_Assertions(Machine(iterationasmvariant))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(iterationasmvariant))==(skip);
  Context_List_Initialisation(Machine(iterationasmvariant))==(skip);
  List_Initialisation(Machine(iterationasmvariant))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(iterationasmvariant))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(iterationasmvariant))==(btrue);
  List_Constraints(Machine(iterationasmvariant))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(iterationasmvariant))==(?);
  List_Operations(Machine(iterationasmvariant))==(?)
END
&
THEORY ListInputX END
&
THEORY ListOutputX END
&
THEORY ListHeaderX END
&
THEORY ListPreconditionX END
&
THEORY ListSubstitutionX END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(iterationasmvariant))==(pgvar,pgsize,loopaddr,loopsize);
  Inherited_List_Constants(Machine(iterationasmvariant))==(?);
  List_Constants(Machine(iterationasmvariant))==(pgvar,pgsize,loopaddr,loopsize)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(iterationasmvariant))==(?);
  Context_List_Defered(Machine(iterationasmvariant))==(?);
  Context_List_Sets(Machine(iterationasmvariant))==(?);
  List_Valuable_Sets(Machine(iterationasmvariant))==(?);
  Inherited_List_Enumerated(Machine(iterationasmvariant))==(?);
  Inherited_List_Defered(Machine(iterationasmvariant))==(?);
  Inherited_List_Sets(Machine(iterationasmvariant))==(?);
  List_Enumerated(Machine(iterationasmvariant))==(?);
  List_Defered(Machine(iterationasmvariant))==(?);
  List_Sets(Machine(iterationasmvariant))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(iterationasmvariant))==(?);
  Expanded_List_HiddenConstants(Machine(iterationasmvariant))==(?);
  List_HiddenConstants(Machine(iterationasmvariant))==(?);
  External_List_HiddenConstants(Machine(iterationasmvariant))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(iterationasmvariant))==(btrue);
  Context_List_Properties(Machine(iterationasmvariant))==(btrue);
  Inherited_List_Properties(Machine(iterationasmvariant))==(btrue);
  List_Properties(Machine(iterationasmvariant))==(pgsize : NATURAL & loopaddr : NATURAL & loopsize : NATURAL & pgvar : NATURAL*NATURAL*NATURAL +-> NATURAL & pgsize = 7 & loopsize = 5 & loopaddr = 2 & !(pc,loopmax,loopvar).(pc : NATURAL & loopmax : NATURAL & loopvar : NATURAL => (pc = 7 => pgvar(pc,loopmax,loopvar) = pgsize-pc) & (pc = 0 or pc = 1 => pgvar(pc,loopmax,loopvar) = pgsize-pc+loopsize*loopmax) & (pc = 2 or pc = 3 or pc = 4 or pc = 5 or pc = 6 => pgvar(pc,loopmax,loopvar) = pgsize-loopaddr+loopsize*(loopmax-loopvar)-(pc-loopaddr))))
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(iterationasmvariant)) == (pgvar,pgsize,loopaddr,loopsize | ? | ? | ? | ? | ? | ? | ? | iterationasmvariant);
  List_Of_HiddenCst_Ids(Machine(iterationasmvariant)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(iterationasmvariant)) == (pgvar,pgsize,loopaddr,loopsize);
  List_Of_VisibleVar_Ids(Machine(iterationasmvariant)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(iterationasmvariant)) == (? : ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(iterationasmvariant)) == (Type(pgvar) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(pgsize) == Cst(btype(INTEGER,?,?));Type(loopaddr) == Cst(btype(INTEGER,?,?));Type(loopsize) == Cst(btype(INTEGER,?,?)))
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
