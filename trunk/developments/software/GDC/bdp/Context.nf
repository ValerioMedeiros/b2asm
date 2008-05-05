Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Context))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Context))==(Machine(Context));
  Level(Machine(Context))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Context)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Context))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Context))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Context))==(?);
  List_Includes(Machine(Context))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Context))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Context))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Context))==(?);
  Context_List_Variables(Machine(Context))==(?);
  Abstract_List_Variables(Machine(Context))==(?);
  Local_List_Variables(Machine(Context))==(?);
  List_Variables(Machine(Context))==(?);
  External_List_Variables(Machine(Context))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Context))==(?);
  Abstract_List_VisibleVariables(Machine(Context))==(?);
  External_List_VisibleVariables(Machine(Context))==(?);
  Expanded_List_VisibleVariables(Machine(Context))==(?);
  List_VisibleVariables(Machine(Context))==(?);
  Internal_List_VisibleVariables(Machine(Context))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Context))==(btrue);
  Gluing_List_Invariant(Machine(Context))==(btrue);
  Expanded_List_Invariant(Machine(Context))==(btrue);
  Abstract_List_Invariant(Machine(Context))==(btrue);
  Context_List_Invariant(Machine(Context))==(btrue);
  List_Invariant(Machine(Context))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(Context))==(btrue);
  Abstract_List_Assertions(Machine(Context))==(btrue);
  Context_List_Assertions(Machine(Context))==(btrue);
  List_Assertions(Machine(Context))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Context))==(skip);
  Context_List_Initialisation(Machine(Context))==(skip);
  List_Initialisation(Machine(Context))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Context))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(Context))==(btrue);
  List_Constraints(Machine(Context))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Context))==(?);
  List_Operations(Machine(Context))==(?)
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
  List_Valuable_Constants(Machine(Context))==(?);
  Inherited_List_Constants(Machine(Context))==(?);
  List_Constants(Machine(Context))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(Context),STATE)==({LEADER,NONE,OPPOSITE});
  Context_List_Enumerated(Machine(Context))==(?);
  Context_List_Defered(Machine(Context))==(?);
  Context_List_Sets(Machine(Context))==(?);
  List_Valuable_Sets(Machine(Context))==(?);
  Inherited_List_Enumerated(Machine(Context))==(?);
  Inherited_List_Defered(Machine(Context))==(?);
  Inherited_List_Sets(Machine(Context))==(?);
  List_Enumerated(Machine(Context))==(STATE,CONDUCTION,DOORS,COMMAND);
  List_Defered(Machine(Context))==(?);
  List_Sets(Machine(Context))==(STATE,CONDUCTION,DOORS,COMMAND);
  Set_Definition(Machine(Context),CONDUCTION)==({MANUAL,CONTROLLED,CONDUCTION_OFF});
  Set_Definition(Machine(Context),DOORS)==({OPENED,CLOSED});
  Set_Definition(Machine(Context),COMMAND)==({NO_COMMAND,OPEN_DOORS,CLOSE_DOORS})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Context))==(?);
  Expanded_List_HiddenConstants(Machine(Context))==(?);
  List_HiddenConstants(Machine(Context))==(?);
  External_List_HiddenConstants(Machine(Context))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Context))==(btrue);
  Context_List_Properties(Machine(Context))==(btrue);
  Inherited_List_Properties(Machine(Context))==(btrue);
  List_Properties(Machine(Context))==(not(STATE = {}) & not(CONDUCTION = {}) & not(DOORS = {}) & not(COMMAND = {}))
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Context)) == (STATE,CONDUCTION,DOORS,COMMAND,LEADER,NONE,OPPOSITE,MANUAL,CONTROLLED,CONDUCTION_OFF,OPENED,CLOSED,NO_COMMAND,OPEN_DOORS,CLOSE_DOORS | ? | ? | ? | ? | ? | ? | ? | Context);
  List_Of_HiddenCst_Ids(Machine(Context)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Context)) == (?);
  List_Of_VisibleVar_Ids(Machine(Context)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Context)) == (? : ?)
END
&
THEORY SetsEnvX IS
  Sets(Machine(Context)) == (Type(STATE) == Cst(SetOf(etype(STATE,0,2)));Type(CONDUCTION) == Cst(SetOf(etype(CONDUCTION,0,2)));Type(DOORS) == Cst(SetOf(etype(DOORS,0,1)));Type(COMMAND) == Cst(SetOf(etype(COMMAND,0,2))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(Context)) == (Type(LEADER) == Cst(etype(STATE,0,2));Type(NONE) == Cst(etype(STATE,0,2));Type(OPPOSITE) == Cst(etype(STATE,0,2));Type(MANUAL) == Cst(etype(CONDUCTION,0,2));Type(CONTROLLED) == Cst(etype(CONDUCTION,0,2));Type(CONDUCTION_OFF) == Cst(etype(CONDUCTION,0,2));Type(OPENED) == Cst(etype(DOORS,0,1));Type(CLOSED) == Cst(etype(DOORS,0,1));Type(NO_COMMAND) == Cst(etype(COMMAND,0,2));Type(OPEN_DOORS) == Cst(etype(COMMAND,0,2));Type(CLOSE_DOORS) == Cst(etype(COMMAND,0,2)))
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
