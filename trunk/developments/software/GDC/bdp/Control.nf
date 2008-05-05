Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Control))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Control))==(Machine(Control));
  Level(Machine(Control))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Control)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Control))==(Context,BoolXor)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Control))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Control))==(?);
  List_Includes(Machine(Control))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Control))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Control))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Control))==(?);
  Context_List_Variables(Machine(Control))==(?);
  Abstract_List_Variables(Machine(Control))==(?);
  Local_List_Variables(Machine(Control))==(?);
  List_Variables(Machine(Control))==(?);
  External_List_Variables(Machine(Control))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Control))==(?);
  Abstract_List_VisibleVariables(Machine(Control))==(?);
  External_List_VisibleVariables(Machine(Control))==(?);
  Expanded_List_VisibleVariables(Machine(Control))==(?);
  List_VisibleVariables(Machine(Control))==(?);
  Internal_List_VisibleVariables(Machine(Control))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Control))==(btrue);
  Gluing_List_Invariant(Machine(Control))==(btrue);
  Expanded_List_Invariant(Machine(Control))==(btrue);
  Abstract_List_Invariant(Machine(Control))==(btrue);
  Context_List_Invariant(Machine(Control))==(btrue);
  List_Invariant(Machine(Control))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(Control))==(btrue);
  Abstract_List_Assertions(Machine(Control))==(btrue);
  Context_List_Assertions(Machine(Control))==(xor(TRUE,FALSE) = TRUE;xor(FALSE,TRUE) = TRUE;xor(TRUE,TRUE) = FALSE;xor(FALSE,FALSE) = FALSE);
  List_Assertions(Machine(Control))==(conduction_p(TRUE,TRUE,FALSE) = TRUE;conduction_p(TRUE,FALSE,TRUE) = TRUE;conduction_p(TRUE,TRUE,TRUE) = FALSE;conduction_p(TRUE,FALSE,FALSE) = FALSE;!(mm,cc).(mm : BOOL & cc : BOOL => conduction_p(FALSE,mm,cc) = FALSE);!(cc,oo,mm).(cc : BOOL & oo : BOOL & mm : COMMAND => select_command(FALSE,cc,oo,mm) = CLOSE_DOORS);!(ss,oo,mm).(ss : BOOL & oo : BOOL & mm : COMMAND => select_command(ss,TRUE,oo,mm) = CLOSE_DOORS);!mm.(mm : COMMAND => select_command(TRUE,FALSE,TRUE,mm) = OPEN_DOORS);!mm.(mm : COMMAND => select_command(TRUE,FALSE,FALSE,mm) = mm))
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Control))==(skip);
  Context_List_Initialisation(Machine(Control))==(skip);
  List_Initialisation(Machine(Control))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Control))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(Control),Machine(Context))==(?);
  List_Instanciated_Parameters(Machine(Control),Machine(BoolXor))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(Control))==(btrue);
  List_Constraints(Machine(Control))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Control))==(?);
  List_Operations(Machine(Control))==(?)
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
  List_Valuable_Constants(Machine(Control))==(conduction_p,select_command);
  Inherited_List_Constants(Machine(Control))==(?);
  List_Constants(Machine(Control))==(conduction_p,select_command)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(Control),COMMAND)==({NO_COMMAND,OPEN_DOORS,CLOSE_DOORS});
  Context_List_Enumerated(Machine(Control))==(STATE,CONDUCTION,DOORS,COMMAND);
  Context_List_Defered(Machine(Control))==(?);
  Context_List_Sets(Machine(Control))==(STATE,CONDUCTION,DOORS,COMMAND);
  List_Valuable_Sets(Machine(Control))==(?);
  Inherited_List_Enumerated(Machine(Control))==(?);
  Inherited_List_Defered(Machine(Control))==(?);
  Inherited_List_Sets(Machine(Control))==(?);
  List_Enumerated(Machine(Control))==(?);
  List_Defered(Machine(Control))==(?);
  List_Sets(Machine(Control))==(?);
  Set_Definition(Machine(Control),DOORS)==({OPENED,CLOSED});
  Set_Definition(Machine(Control),CONDUCTION)==({MANUAL,CONTROLLED,CONDUCTION_OFF});
  Set_Definition(Machine(Control),STATE)==({LEADER,NONE,OPPOSITE})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Control))==(?);
  Expanded_List_HiddenConstants(Machine(Control))==(?);
  List_HiddenConstants(Machine(Control))==(?);
  External_List_HiddenConstants(Machine(Control))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Control))==(btrue);
  Context_List_Properties(Machine(Control))==(not(STATE = {}) & not(CONDUCTION = {}) & not(DOORS = {}) & not(COMMAND = {}) & xor : BOOL*BOOL --> BOOL & !(a,b).(a : BOOL & b : BOOL => xor(a,b) = bool(a/=b)));
  Inherited_List_Properties(Machine(Control))==(btrue);
  List_Properties(Machine(Control))==(conduction_p : BOOL*BOOL*BOOL --> BOOL & conduction_p = %(leader,manual,controlled).(leader : BOOL & manual : BOOL & controlled : BOOL | bool(leader = TRUE & xor(manual,controlled) = TRUE)) & !(l,m,c).(l : BOOL & m : BOOL & c : BOOL => conduction_p(l,m,c) = bool(l = TRUE & xor(m,c) = TRUE)) & select_command : BOOL*BOOL*BOOL*COMMAND --> COMMAND & !(slow,close,open,mem).(slow : BOOL & close : BOOL & open : BOOL & mem : COMMAND => (slow = FALSE or close = TRUE => select_command(slow,close,open,mem) = CLOSE_DOORS) & (slow = TRUE & close = FALSE & open = TRUE => select_command(slow,close,open,mem) = OPEN_DOORS) & (slow = TRUE & close = FALSE & open = FALSE => select_command(slow,close,open,mem) = mem)))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(Control),Machine(BoolXor))==(?);
  Seen_Context_List_Enumerated(Machine(Control))==(?);
  Seen_Context_List_Invariant(Machine(Control))==(btrue);
  Seen_Context_List_Assertions(Machine(Control))==(btrue);
  Seen_Context_List_Properties(Machine(Control))==(btrue);
  Seen_List_Constraints(Machine(Control))==(btrue);
  Seen_List_Operations(Machine(Control),Machine(BoolXor))==(?);
  Seen_Expanded_List_Invariant(Machine(Control),Machine(BoolXor))==(btrue);
  Seen_Internal_List_Operations(Machine(Control),Machine(Context))==(?);
  Seen_List_Operations(Machine(Control),Machine(Context))==(?);
  Seen_Expanded_List_Invariant(Machine(Control),Machine(Context))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Control)) == (conduction_p,select_command | ? | ? | ? | ? | ? | seen(Machine(Context)),seen(Machine(BoolXor)) | ? | Control);
  List_Of_HiddenCst_Ids(Machine(Control)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Control)) == (conduction_p,select_command);
  List_Of_VisibleVar_Ids(Machine(Control)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Control)) == (? : ?);
  List_Of_Ids(Machine(BoolXor)) == (xor | ? | ? | ? | ? | ? | ? | ? | BoolXor);
  List_Of_HiddenCst_Ids(Machine(BoolXor)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BoolXor)) == (xor);
  List_Of_VisibleVar_Ids(Machine(BoolXor)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BoolXor)) == (? : ?);
  List_Of_Ids(Machine(Context)) == (STATE,CONDUCTION,DOORS,COMMAND,LEADER,NONE,OPPOSITE,MANUAL,CONTROLLED,CONDUCTION_OFF,OPENED,CLOSED,NO_COMMAND,OPEN_DOORS,CLOSE_DOORS | ? | ? | ? | ? | ? | ? | ? | Context);
  List_Of_HiddenCst_Ids(Machine(Context)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Context)) == (?);
  List_Of_VisibleVar_Ids(Machine(Context)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Context)) == (? : ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(Control)) == (Type(conduction_p) == Cst(SetOf(btype(BOOL,0,1)*btype(BOOL,0,1)*btype(BOOL,0,1)*btype(BOOL,0,1)));Type(select_command) == Cst(SetOf(btype(BOOL,0,1)*btype(BOOL,0,1)*btype(BOOL,0,1)*etype(COMMAND,0,2)*etype(COMMAND,0,2))))
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
