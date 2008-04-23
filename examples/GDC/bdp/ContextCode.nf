Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(ContextCode))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(ContextCode))==(Machine(ContextCode));
  Level(Machine(ContextCode))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(ContextCode)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(ContextCode))==(Context)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(ContextCode))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(ContextCode))==(?);
  List_Includes(Machine(ContextCode))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(ContextCode))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(ContextCode))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(ContextCode))==(?);
  Context_List_Variables(Machine(ContextCode))==(?);
  Abstract_List_Variables(Machine(ContextCode))==(?);
  Local_List_Variables(Machine(ContextCode))==(?);
  List_Variables(Machine(ContextCode))==(?);
  External_List_Variables(Machine(ContextCode))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(ContextCode))==(?);
  Abstract_List_VisibleVariables(Machine(ContextCode))==(?);
  External_List_VisibleVariables(Machine(ContextCode))==(?);
  Expanded_List_VisibleVariables(Machine(ContextCode))==(?);
  List_VisibleVariables(Machine(ContextCode))==(?);
  Internal_List_VisibleVariables(Machine(ContextCode))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(ContextCode))==(btrue);
  Gluing_List_Invariant(Machine(ContextCode))==(btrue);
  Expanded_List_Invariant(Machine(ContextCode))==(btrue);
  Abstract_List_Invariant(Machine(ContextCode))==(btrue);
  Context_List_Invariant(Machine(ContextCode))==(btrue);
  List_Invariant(Machine(ContextCode))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(ContextCode))==(btrue);
  Abstract_List_Assertions(Machine(ContextCode))==(btrue);
  Context_List_Assertions(Machine(ContextCode))==(btrue);
  List_Assertions(Machine(ContextCode))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(ContextCode))==(skip);
  Context_List_Initialisation(Machine(ContextCode))==(skip);
  List_Initialisation(Machine(ContextCode))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(ContextCode))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(ContextCode),Machine(Context))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(ContextCode))==(btrue);
  List_Constraints(Machine(ContextCode))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(ContextCode))==(?);
  List_Operations(Machine(ContextCode))==(?)
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
  List_Valuable_Constants(Machine(ContextCode))==(STATE_CODE,CONDUCTION_CODE,DOORS_CODE,COMMAND_CODE,BOOL_CODE,select_command_code);
  Inherited_List_Constants(Machine(ContextCode))==(?);
  List_Constants(Machine(ContextCode))==(STATE_CODE,CONDUCTION_CODE,DOORS_CODE,COMMAND_CODE,BOOL_CODE,select_command_code)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(ContextCode),COMMAND)==({NO_COMMAND,OPEN_DOORS,CLOSE_DOORS});
  Context_List_Enumerated(Machine(ContextCode))==(STATE,CONDUCTION,DOORS,COMMAND);
  Context_List_Defered(Machine(ContextCode))==(?);
  Context_List_Sets(Machine(ContextCode))==(STATE,CONDUCTION,DOORS,COMMAND);
  List_Valuable_Sets(Machine(ContextCode))==(?);
  Inherited_List_Enumerated(Machine(ContextCode))==(?);
  Inherited_List_Defered(Machine(ContextCode))==(?);
  Inherited_List_Sets(Machine(ContextCode))==(?);
  List_Enumerated(Machine(ContextCode))==(?);
  List_Defered(Machine(ContextCode))==(?);
  List_Sets(Machine(ContextCode))==(?);
  Set_Definition(Machine(ContextCode),DOORS)==({OPENED,CLOSED});
  Set_Definition(Machine(ContextCode),CONDUCTION)==({MANUAL,CONTROLLED,CONDUCTION_OFF});
  Set_Definition(Machine(ContextCode),STATE)==({LEADER,NONE,OPPOSITE})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(ContextCode))==(?);
  Expanded_List_HiddenConstants(Machine(ContextCode))==(?);
  List_HiddenConstants(Machine(ContextCode))==(?);
  External_List_HiddenConstants(Machine(ContextCode))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(ContextCode))==(btrue);
  Context_List_Properties(Machine(ContextCode))==(not(STATE = {}) & not(CONDUCTION = {}) & not(DOORS = {}) & not(COMMAND = {}));
  Inherited_List_Properties(Machine(ContextCode))==(btrue);
  List_Properties(Machine(ContextCode))==(STATE_CODE : STATE --> NATURAL & STATE_CODE = {NONE|->0,LEADER|->1,OPPOSITE|->2} & CONDUCTION_CODE : CONDUCTION --> NATURAL & CONDUCTION_CODE = {CONDUCTION_OFF|->0,MANUAL|->1,CONTROLLED|->2} & DOORS_CODE : DOORS --> NATURAL & DOORS_CODE = {OPENED|->0,CLOSED|->1} & COMMAND_CODE : COMMAND --> NATURAL & COMMAND_CODE = {NO_COMMAND|->0,OPEN_DOORS|->1,CLOSE_DOORS|->2} & BOOL_CODE : BOOL --> NATURAL & BOOL_CODE = {FALSE|->0,TRUE|->1} & select_command_code : NATURAL*NATURAL*NATURAL*NATURAL --> NATURAL & !(slow,close,open,mem).(slow : NATURAL & close : NATURAL & open : NATURAL & mem : NATURAL => (slow = BOOL_CODE(FALSE) or close = BOOL_CODE(TRUE) => select_command_code(slow,close,open,mem) = COMMAND_CODE(CLOSE_DOORS)) & (slow = BOOL_CODE(TRUE) & close = BOOL_CODE(FALSE) & open = BOOL_CODE(TRUE) => select_command_code(slow,close,open,mem) = COMMAND_CODE(OPEN_DOORS)) & (slow = BOOL_CODE(TRUE) & close = BOOL_CODE(FALSE) & open = BOOL_CODE(FALSE) => select_command_code(slow,close,open,mem) = mem)))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(ContextCode),Machine(Context))==(?);
  Seen_Context_List_Enumerated(Machine(ContextCode))==(?);
  Seen_Context_List_Invariant(Machine(ContextCode))==(btrue);
  Seen_Context_List_Assertions(Machine(ContextCode))==(btrue);
  Seen_Context_List_Properties(Machine(ContextCode))==(btrue);
  Seen_List_Constraints(Machine(ContextCode))==(btrue);
  Seen_List_Operations(Machine(ContextCode),Machine(Context))==(?);
  Seen_Expanded_List_Invariant(Machine(ContextCode),Machine(Context))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(ContextCode)) == (STATE_CODE,CONDUCTION_CODE,DOORS_CODE,COMMAND_CODE,BOOL_CODE,select_command_code | ? | ? | ? | ? | ? | seen(Machine(Context)) | ? | ContextCode);
  List_Of_HiddenCst_Ids(Machine(ContextCode)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ContextCode)) == (STATE_CODE,CONDUCTION_CODE,DOORS_CODE,COMMAND_CODE,BOOL_CODE,select_command_code);
  List_Of_VisibleVar_Ids(Machine(ContextCode)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ContextCode)) == (? : ?);
  List_Of_Ids(Machine(Context)) == (STATE,CONDUCTION,DOORS,COMMAND,LEADER,NONE,OPPOSITE,MANUAL,CONTROLLED,CONDUCTION_OFF,OPENED,CLOSED,NO_COMMAND,OPEN_DOORS,CLOSE_DOORS | ? | ? | ? | ? | ? | ? | ? | Context);
  List_Of_HiddenCst_Ids(Machine(Context)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Context)) == (?);
  List_Of_VisibleVar_Ids(Machine(Context)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Context)) == (? : ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(ContextCode)) == (Type(STATE_CODE) == Cst(SetOf(etype(STATE,0,2)*btype(INTEGER,?,?)));Type(CONDUCTION_CODE) == Cst(SetOf(etype(CONDUCTION,0,2)*btype(INTEGER,?,?)));Type(DOORS_CODE) == Cst(SetOf(etype(DOORS,0,1)*btype(INTEGER,?,?)));Type(COMMAND_CODE) == Cst(SetOf(etype(COMMAND,0,2)*btype(INTEGER,?,?)));Type(BOOL_CODE) == Cst(SetOf(btype(BOOL,0,1)*btype(INTEGER,?,?)));Type(select_command_code) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?))))
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
