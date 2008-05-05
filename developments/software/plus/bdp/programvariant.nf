Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(programvariant))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(programvariant))==(Machine(programvariant));
  Level(Machine(programvariant))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(programvariant)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(programvariant))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(programvariant))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(programvariant))==(?);
  List_Includes(Machine(programvariant))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(programvariant))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(programvariant))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(programvariant))==(?);
  Context_List_Variables(Machine(programvariant))==(?);
  Abstract_List_Variables(Machine(programvariant))==(?);
  Local_List_Variables(Machine(programvariant))==(?);
  List_Variables(Machine(programvariant))==(?);
  External_List_Variables(Machine(programvariant))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(programvariant))==(?);
  Abstract_List_VisibleVariables(Machine(programvariant))==(?);
  External_List_VisibleVariables(Machine(programvariant))==(?);
  Expanded_List_VisibleVariables(Machine(programvariant))==(?);
  List_VisibleVariables(Machine(programvariant))==(?);
  Internal_List_VisibleVariables(Machine(programvariant))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(programvariant))==(btrue);
  Gluing_List_Invariant(Machine(programvariant))==(btrue);
  Expanded_List_Invariant(Machine(programvariant))==(btrue);
  Abstract_List_Invariant(Machine(programvariant))==(btrue);
  Context_List_Invariant(Machine(programvariant))==(btrue);
  List_Invariant(Machine(programvariant))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(programvariant))==(btrue);
  Abstract_List_Assertions(Machine(programvariant))==(btrue);
  Context_List_Assertions(Machine(programvariant))==(btrue);
  List_Assertions(Machine(programvariant))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(programvariant))==(skip);
  Context_List_Initialisation(Machine(programvariant))==(skip);
  List_Initialisation(Machine(programvariant))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(programvariant))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(programvariant))==(btrue);
  List_Constraints(Machine(programvariant))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(programvariant))==(?);
  List_Operations(Machine(programvariant))==(?)
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
  List_Valuable_Constants(Machine(programvariant))==(pgvar,pgsize,loopaddr,loopsize);
  Inherited_List_Constants(Machine(programvariant))==(?);
  List_Constants(Machine(programvariant))==(pgvar,pgsize,loopaddr,loopsize)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(programvariant))==(?);
  Context_List_Defered(Machine(programvariant))==(?);
  Context_List_Sets(Machine(programvariant))==(?);
  List_Valuable_Sets(Machine(programvariant))==(?);
  Inherited_List_Enumerated(Machine(programvariant))==(?);
  Inherited_List_Defered(Machine(programvariant))==(?);
  Inherited_List_Sets(Machine(programvariant))==(?);
  List_Enumerated(Machine(programvariant))==(?);
  List_Defered(Machine(programvariant))==(?);
  List_Sets(Machine(programvariant))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(programvariant))==(?);
  Expanded_List_HiddenConstants(Machine(programvariant))==(?);
  List_HiddenConstants(Machine(programvariant))==(?);
  External_List_HiddenConstants(Machine(programvariant))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(programvariant))==(btrue);
  Context_List_Properties(Machine(programvariant))==(btrue);
  Inherited_List_Properties(Machine(programvariant))==(btrue);
  List_Properties(Machine(programvariant))==(pgsize : NATURAL & loopaddr : NATURAL & loopsize : NATURAL & pgvar : NATURAL*NATURAL*NATURAL +-> NATURAL & pgsize = 8 & loopsize = 5 & loopaddr = 2 & !(pc,loopmax,loopvar).(pc : NATURAL & loopmax : NATURAL & loopvar : NATURAL => (pc = 7 or pc = 8 => pgvar(pc,loopmax,loopvar) = pgsize-pc) & (pc = 0 or pc = 1 => pgvar(pc,loopmax,loopvar) = pgsize-pc+loopsize*loopmax) & (pc = 2 or pc = 3 or pc = 4 or pc = 5 or pc = 6 => pgvar(pc,loopmax,loopvar) = pgsize-loopaddr+loopsize*(loopmax-loopvar)-(pc-loopaddr))))
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(programvariant)) == (pgvar,pgsize,loopaddr,loopsize | ? | ? | ? | ? | ? | ? | ? | programvariant);
  List_Of_HiddenCst_Ids(Machine(programvariant)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(programvariant)) == (pgvar,pgsize,loopaddr,loopsize);
  List_Of_VisibleVar_Ids(Machine(programvariant)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(programvariant)) == (? : ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(programvariant)) == (Type(pgvar) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(pgsize) == Cst(btype(INTEGER,?,?));Type(loopaddr) == Cst(btype(INTEGER,?,?));Type(loopsize) == Cst(btype(INTEGER,?,?)))
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
