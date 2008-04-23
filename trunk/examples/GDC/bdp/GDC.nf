Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(GDC))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(GDC))==(Machine(GDC));
  Level(Machine(GDC))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(GDC)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(GDC))==(Context,Control,BoolXor)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(GDC))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(GDC))==(?);
  List_Includes(Machine(GDC))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(GDC))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(GDC))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(GDC))==(?);
  Context_List_Variables(Machine(GDC))==(?);
  Abstract_List_Variables(Machine(GDC))==(?);
  Local_List_Variables(Machine(GDC))==(Rightvisual_open,Righttl_command,Righttl_sensor_open,Rightclose_doors,Rightopen_doors,Rightcommand_mem,Leftvisual_open,Lefttl_command,Lefttl_sensor_open,Leftclose_doors,Leftopen_doors,Leftcommand_mem,controlled,manual,conduction,opposite,leader,cabin,traction,slow_speed);
  List_Variables(Machine(GDC))==(Rightvisual_open,Righttl_command,Righttl_sensor_open,Rightclose_doors,Rightopen_doors,Rightcommand_mem,Leftvisual_open,Lefttl_command,Lefttl_sensor_open,Leftclose_doors,Leftopen_doors,Leftcommand_mem,controlled,manual,conduction,opposite,leader,cabin,traction,slow_speed);
  External_List_Variables(Machine(GDC))==(Rightvisual_open,Righttl_command,Righttl_sensor_open,Rightclose_doors,Rightopen_doors,Rightcommand_mem,Leftvisual_open,Lefttl_command,Lefttl_sensor_open,Leftclose_doors,Leftopen_doors,Leftcommand_mem,controlled,manual,conduction,opposite,leader,cabin,traction,slow_speed)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(GDC))==(?);
  Abstract_List_VisibleVariables(Machine(GDC))==(?);
  External_List_VisibleVariables(Machine(GDC))==(?);
  Expanded_List_VisibleVariables(Machine(GDC))==(?);
  List_VisibleVariables(Machine(GDC))==(?);
  Internal_List_VisibleVariables(Machine(GDC))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(GDC))==(btrue);
  Gluing_List_Invariant(Machine(GDC))==(btrue);
  Expanded_List_Invariant(Machine(GDC))==(btrue);
  Abstract_List_Invariant(Machine(GDC))==(btrue);
  Context_List_Invariant(Machine(GDC))==(btrue);
  List_Invariant(Machine(GDC))==(slow_speed : BOOL & traction : BOOL & cabin : STATE & leader : BOOL & opposite : BOOL & not(leader = TRUE & opposite = TRUE) & leader = TRUE <=> (cabin = LEADER) & opposite = TRUE <=> (cabin = OPPOSITE) & (leader = FALSE & opposite = FALSE) <=> (cabin = NONE) & conduction : CONDUCTION & manual : BOOL & controlled : BOOL & conduction = MANUAL <=> (manual = TRUE & controlled = FALSE) & conduction = CONTROLLED <=> (manual = FALSE & controlled = TRUE) & conduction = CONDUCTION_OFF <=> (manual = FALSE & controlled = FALSE or (manual = TRUE & controlled = TRUE)) & Leftcommand_mem : COMMAND & Lefttl_command : COMMAND & Leftopen_doors : BOOL & Leftclose_doors : BOOL & Lefttl_sensor_open : BOOL & Leftvisual_open : BOOL & Leftvisual_open = Lefttl_sensor_open & Rightcommand_mem : COMMAND & Righttl_command : COMMAND & Rightopen_doors : BOOL & Rightclose_doors : BOOL & Righttl_sensor_open : BOOL & Rightvisual_open : BOOL & Rightvisual_open = Righttl_sensor_open & (cabin = LEADER & (conduction = MANUAL or conduction = CONTROLLED) => (slow_speed = FALSE => Lefttl_command = CLOSE_DOORS & Righttl_command = CLOSE_DOORS) & (Lefttl_sensor_open = TRUE or Righttl_sensor_open = TRUE => traction = FALSE)))
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(GDC))==(btrue);
  Abstract_List_Assertions(Machine(GDC))==(btrue);
  Context_List_Assertions(Machine(GDC))==((conduction_p(TRUE,TRUE,FALSE) = TRUE;conduction_p(TRUE,FALSE,TRUE) = TRUE;conduction_p(TRUE,TRUE,TRUE) = FALSE;conduction_p(TRUE,FALSE,FALSE) = FALSE;!(mm,cc).(mm : BOOL & cc : BOOL => conduction_p(FALSE,mm,cc) = FALSE);!(cc,oo,mm).(cc : BOOL & oo : BOOL & mm : COMMAND => select_command(FALSE,cc,oo,mm) = CLOSE_DOORS);!(ss,oo,mm).(ss : BOOL & oo : BOOL & mm : COMMAND => select_command(ss,TRUE,oo,mm) = CLOSE_DOORS);!mm.(mm : COMMAND => select_command(TRUE,FALSE,TRUE,mm) = OPEN_DOORS);!mm.(mm : COMMAND => select_command(TRUE,FALSE,FALSE,mm) = mm)) & (xor(TRUE,FALSE) = TRUE;xor(FALSE,TRUE) = TRUE;xor(TRUE,TRUE) = FALSE;xor(FALSE,FALSE) = FALSE));
  List_Assertions(Machine(GDC))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(GDC))==(slow_speed,traction,cabin,leader,opposite,conduction,manual,controlled,Leftcommand_mem,Lefttl_command,Leftopen_doors,Leftclose_doors,Lefttl_sensor_open,Leftvisual_open,Rightcommand_mem,Righttl_command,Rightopen_doors,Rightclose_doors,Righttl_sensor_open,Rightvisual_open:=TRUE,TRUE,NONE,FALSE,FALSE,MANUAL,TRUE,FALSE,CLOSE_DOORS,CLOSE_DOORS,FALSE,TRUE,FALSE,FALSE,CLOSE_DOORS,CLOSE_DOORS,FALSE,TRUE,FALSE,FALSE);
  Context_List_Initialisation(Machine(GDC))==(skip);
  List_Initialisation(Machine(GDC))==(slow_speed:=TRUE || traction:=TRUE || cabin:=NONE || leader:=FALSE || opposite:=FALSE || conduction:=MANUAL || manual:=TRUE || controlled:=FALSE || Leftcommand_mem:=CLOSE_DOORS || Lefttl_command:=CLOSE_DOORS || Leftopen_doors:=FALSE || Leftclose_doors:=TRUE || Lefttl_sensor_open:=FALSE || Leftvisual_open:=FALSE || Rightcommand_mem:=CLOSE_DOORS || Righttl_command:=CLOSE_DOORS || Rightopen_doors:=FALSE || Rightclose_doors:=TRUE || Righttl_sensor_open:=FALSE || Rightvisual_open:=FALSE)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(GDC))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(GDC),Machine(Context))==(?);
  List_Instanciated_Parameters(Machine(GDC),Machine(Control))==(?);
  List_Instanciated_Parameters(Machine(GDC),Machine(BoolXor))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(GDC))==(btrue);
  List_Constraints(Machine(GDC))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(GDC))==(update_controller);
  List_Operations(Machine(GDC))==(update_controller)
END
&
THEORY ListInputX IS
  List_Input(Machine(GDC),update_controller)==(i_open_left_doors,i_close_left_doors,i_tl_sensor_left_open,i_tl_left_command,i_open_right_doors,i_close_right_doors,i_tl_sensor_right_open,i_tl_right_command,i_leader,i_opposite,i_manual,i_controlled,i_slow_speed)
END
&
THEORY ListOutputX IS
  List_Output(Machine(GDC),update_controller)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(GDC),update_controller)==(update_controller(i_open_left_doors,i_close_left_doors,i_tl_sensor_left_open,i_tl_left_command,i_open_right_doors,i_close_right_doors,i_tl_sensor_right_open,i_tl_right_command,i_leader,i_opposite,i_manual,i_controlled,i_slow_speed))
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(GDC),update_controller)==(i_open_left_doors : BOOL & i_close_left_doors : BOOL & i_tl_sensor_left_open : BOOL & i_open_right_doors : BOOL & i_close_right_doors : BOOL & i_tl_sensor_right_open : BOOL & i_leader : BOOL & i_opposite : BOOL & i_manual : BOOL & i_controlled : BOOL & i_slow_speed : BOOL & i_tl_left_command : COMMAND & i_tl_right_command : COMMAND & not(i_leader = TRUE & i_opposite = TRUE))
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(GDC),update_controller)==(i_open_left_doors : BOOL & i_close_left_doors : BOOL & i_tl_sensor_left_open : BOOL & i_open_right_doors : BOOL & i_close_right_doors : BOOL & i_tl_sensor_right_open : BOOL & i_leader : BOOL & i_opposite : BOOL & i_manual : BOOL & i_controlled : BOOL & i_slow_speed : BOOL & i_tl_left_command : COMMAND & i_tl_right_command : COMMAND & not(i_leader = TRUE & i_opposite = TRUE) | leader,opposite:=i_leader,i_opposite || (i_leader = TRUE ==> cabin:=LEADER [] not(i_leader = TRUE) ==> (i_opposite = TRUE ==> cabin:=OPPOSITE [] not(i_opposite = TRUE) ==> cabin:=NONE)) || slow_speed:=i_slow_speed || manual:=i_manual || controlled:=i_controlled || (i_manual = TRUE ==> (i_controlled = TRUE ==> conduction:=CONDUCTION_OFF [] not(i_controlled = TRUE) ==> conduction:=MANUAL) [] not(i_manual = TRUE) ==> (i_controlled = TRUE ==> conduction:=CONTROLLED [] not(i_controlled = TRUE) ==> conduction:=CONDUCTION_OFF)) || @cond.(cond : BOOL & cond = conduction_p(i_leader,i_manual,i_controlled) ==> (Leftopen_doors,Leftclose_doors,Lefttl_sensor_open,Leftvisual_open,Rightopen_doors,Rightclose_doors,Righttl_sensor_open,Rightvisual_open:=i_open_left_doors,i_close_left_doors,i_tl_sensor_left_open,i_tl_sensor_left_open,i_open_right_doors,i_close_right_doors,i_tl_sensor_right_open,i_tl_sensor_right_open || (cond = TRUE ==> (@cl.(cl : COMMAND & cl = select_command(i_slow_speed,i_close_left_doors,i_open_left_doors,Leftcommand_mem) ==> (cond = TRUE ==> Lefttl_command:=cl [] not(cond = TRUE) ==> Lefttl_command:=i_tl_left_command || (cond = TRUE ==> Leftcommand_mem:=cl [] not(cond = TRUE) ==> Leftcommand_mem:=i_tl_left_command))) || @cr.(cr : COMMAND & cr = select_command(i_slow_speed,i_close_right_doors,i_open_right_doors,Rightcommand_mem) ==> (cond = TRUE ==> Righttl_command:=cr [] not(cond = TRUE) ==> Righttl_command:=i_tl_right_command || (cond = TRUE ==> Rightcommand_mem:=cr [] not(cond = TRUE) ==> Rightcommand_mem:=i_tl_right_command)))) [] not(cond = TRUE) ==> skip))) || (i_tl_sensor_left_open = TRUE or i_tl_sensor_right_open = TRUE ==> traction:=FALSE [] not(i_tl_sensor_left_open = TRUE or i_tl_sensor_right_open = TRUE) ==> traction:=TRUE));
  List_Substitution(Machine(GDC),update_controller)==(leader:=i_leader || opposite:=i_opposite || IF i_leader = TRUE THEN cabin:=LEADER ELSIF i_opposite = TRUE THEN cabin:=OPPOSITE ELSE cabin:=NONE END || slow_speed:=i_slow_speed || manual:=i_manual || controlled:=i_controlled || IF i_manual = TRUE THEN IF i_controlled = TRUE THEN conduction:=CONDUCTION_OFF ELSE conduction:=MANUAL END ELSE IF i_controlled = TRUE THEN conduction:=CONTROLLED ELSE conduction:=CONDUCTION_OFF END END || ANY cond WHERE cond : BOOL & cond = conduction_p(i_leader,i_manual,i_controlled) THEN Leftopen_doors:=i_open_left_doors || Leftclose_doors:=i_close_left_doors || Lefttl_sensor_open:=i_tl_sensor_left_open || Leftvisual_open:=i_tl_sensor_left_open || Rightopen_doors:=i_open_right_doors || Rightclose_doors:=i_close_right_doors || Righttl_sensor_open:=i_tl_sensor_right_open || Rightvisual_open:=i_tl_sensor_right_open || IF cond = TRUE THEN ANY cl WHERE cl : COMMAND & cl = select_command(i_slow_speed,i_close_left_doors,i_open_left_doors,Leftcommand_mem) THEN IF cond = TRUE THEN Lefttl_command:=cl ELSE Lefttl_command:=i_tl_left_command END || IF cond = TRUE THEN Leftcommand_mem:=cl ELSE Leftcommand_mem:=i_tl_left_command END END || ANY cr WHERE cr : COMMAND & cr = select_command(i_slow_speed,i_close_right_doors,i_open_right_doors,Rightcommand_mem) THEN IF cond = TRUE THEN Righttl_command:=cr ELSE Righttl_command:=i_tl_right_command END || IF cond = TRUE THEN Rightcommand_mem:=cr ELSE Rightcommand_mem:=i_tl_right_command END END END END || IF i_tl_sensor_left_open = TRUE or i_tl_sensor_right_open = TRUE THEN traction:=FALSE ELSE traction:=TRUE END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(GDC))==(?);
  Inherited_List_Constants(Machine(GDC))==(?);
  List_Constants(Machine(GDC))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(GDC),COMMAND)==({NO_COMMAND,OPEN_DOORS,CLOSE_DOORS});
  Context_List_Enumerated(Machine(GDC))==(STATE,CONDUCTION,DOORS,COMMAND);
  Context_List_Defered(Machine(GDC))==(?);
  Context_List_Sets(Machine(GDC))==(STATE,CONDUCTION,DOORS,COMMAND);
  List_Valuable_Sets(Machine(GDC))==(?);
  Inherited_List_Enumerated(Machine(GDC))==(?);
  Inherited_List_Defered(Machine(GDC))==(?);
  Inherited_List_Sets(Machine(GDC))==(?);
  List_Enumerated(Machine(GDC))==(?);
  List_Defered(Machine(GDC))==(?);
  List_Sets(Machine(GDC))==(?);
  Set_Definition(Machine(GDC),DOORS)==({OPENED,CLOSED});
  Set_Definition(Machine(GDC),CONDUCTION)==({MANUAL,CONTROLLED,CONDUCTION_OFF});
  Set_Definition(Machine(GDC),STATE)==({LEADER,NONE,OPPOSITE})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(GDC))==(?);
  Expanded_List_HiddenConstants(Machine(GDC))==(?);
  List_HiddenConstants(Machine(GDC))==(?);
  External_List_HiddenConstants(Machine(GDC))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(GDC))==(btrue);
  Context_List_Properties(Machine(GDC))==(not(STATE = {}) & not(CONDUCTION = {}) & not(DOORS = {}) & not(COMMAND = {}) & conduction_p : BOOL*BOOL*BOOL --> BOOL & conduction_p = %(leader,manual,controlled).(leader : BOOL & manual : BOOL & controlled : BOOL | bool(leader = TRUE & xor(manual,controlled) = TRUE)) & !(l,m,c).(l : BOOL & m : BOOL & c : BOOL => conduction_p(l,m,c) = bool(l = TRUE & xor(m,c) = TRUE)) & select_command : BOOL*BOOL*BOOL*COMMAND --> COMMAND & !(slow,close,open,mem).(slow : BOOL & close : BOOL & open : BOOL & mem : COMMAND => (slow = FALSE or close = TRUE => select_command(slow,close,open,mem) = CLOSE_DOORS) & (slow = TRUE & close = FALSE & open = TRUE => select_command(slow,close,open,mem) = OPEN_DOORS) & (slow = TRUE & close = FALSE & open = FALSE => select_command(slow,close,open,mem) = mem)) & xor : BOOL*BOOL --> BOOL & !(a,b).(a : BOOL & b : BOOL => xor(a,b) = bool(a/=b)));
  Inherited_List_Properties(Machine(GDC))==(btrue);
  List_Properties(Machine(GDC))==(btrue)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(GDC),Machine(BoolXor))==(?);
  Seen_Context_List_Enumerated(Machine(GDC))==(STATE,CONDUCTION,DOORS,COMMAND);
  Seen_Context_List_Invariant(Machine(GDC))==(btrue);
  Seen_Context_List_Assertions(Machine(GDC))==(xor(TRUE,FALSE) = TRUE;xor(FALSE,TRUE) = TRUE;xor(TRUE,TRUE) = FALSE;xor(FALSE,FALSE) = FALSE);
  Seen_Context_List_Properties(Machine(GDC))==(not(STATE = {}) & not(CONDUCTION = {}) & not(DOORS = {}) & not(COMMAND = {}) & xor : BOOL*BOOL --> BOOL & !(a,b).(a : BOOL & b : BOOL => xor(a,b) = bool(a/=b)));
  Seen_List_Constraints(Machine(GDC))==(btrue);
  Seen_List_Operations(Machine(GDC),Machine(BoolXor))==(?);
  Seen_Expanded_List_Invariant(Machine(GDC),Machine(BoolXor))==(btrue);
  Seen_Internal_List_Operations(Machine(GDC),Machine(Control))==(?);
  Seen_List_Operations(Machine(GDC),Machine(Control))==(?);
  Seen_Expanded_List_Invariant(Machine(GDC),Machine(Control))==(btrue);
  Set_Definition(Machine(GDC),COMMAND)==({NO_COMMAND,OPEN_DOORS,CLOSE_DOORS});
  Set_Definition(Machine(GDC),DOORS)==({OPENED,CLOSED});
  Set_Definition(Machine(GDC),CONDUCTION)==({MANUAL,CONTROLLED,CONDUCTION_OFF});
  Set_Definition(Machine(GDC),STATE)==({LEADER,NONE,OPPOSITE});
  Seen_Internal_List_Operations(Machine(GDC),Machine(Context))==(?);
  Seen_List_Operations(Machine(GDC),Machine(Context))==(?);
  Seen_Expanded_List_Invariant(Machine(GDC),Machine(Context))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(GDC)) == (? | ? | Rightvisual_open,Righttl_command,Righttl_sensor_open,Rightclose_doors,Rightopen_doors,Rightcommand_mem,Leftvisual_open,Lefttl_command,Lefttl_sensor_open,Leftclose_doors,Leftopen_doors,Leftcommand_mem,controlled,manual,conduction,opposite,leader,cabin,traction,slow_speed | ? | update_controller | ? | seen(Machine(Context)),seen(Machine(Control)),seen(Machine(BoolXor)) | ? | GDC);
  List_Of_HiddenCst_Ids(Machine(GDC)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(GDC)) == (?);
  List_Of_VisibleVar_Ids(Machine(GDC)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(GDC)) == (? : ?);
  List_Of_Ids(Machine(BoolXor)) == (xor | ? | ? | ? | ? | ? | ? | ? | BoolXor);
  List_Of_HiddenCst_Ids(Machine(BoolXor)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BoolXor)) == (xor);
  List_Of_VisibleVar_Ids(Machine(BoolXor)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BoolXor)) == (? : ?);
  List_Of_Ids(Machine(Control)) == (conduction_p,select_command | ? | ? | ? | ? | ? | seen(Machine(Context)),seen(Machine(BoolXor)) | ? | Control);
  List_Of_HiddenCst_Ids(Machine(Control)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Control)) == (conduction_p,select_command);
  List_Of_VisibleVar_Ids(Machine(Control)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Control)) == (? : ?);
  List_Of_Ids(Machine(Context)) == (STATE,CONDUCTION,DOORS,COMMAND,LEADER,NONE,OPPOSITE,MANUAL,CONTROLLED,CONDUCTION_OFF,OPENED,CLOSED,NO_COMMAND,OPEN_DOORS,CLOSE_DOORS | ? | ? | ? | ? | ? | ? | ? | Context);
  List_Of_HiddenCst_Ids(Machine(Context)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Context)) == (?);
  List_Of_VisibleVar_Ids(Machine(Context)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Context)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(GDC)) == (Type(Rightvisual_open) == Mvl(btype(BOOL,?,?));Type(Righttl_command) == Mvl(etype(COMMAND,?,?));Type(Righttl_sensor_open) == Mvl(btype(BOOL,?,?));Type(Rightclose_doors) == Mvl(btype(BOOL,?,?));Type(Rightopen_doors) == Mvl(btype(BOOL,?,?));Type(Rightcommand_mem) == Mvl(etype(COMMAND,?,?));Type(Leftvisual_open) == Mvl(btype(BOOL,?,?));Type(Lefttl_command) == Mvl(etype(COMMAND,?,?));Type(Lefttl_sensor_open) == Mvl(btype(BOOL,?,?));Type(Leftclose_doors) == Mvl(btype(BOOL,?,?));Type(Leftopen_doors) == Mvl(btype(BOOL,?,?));Type(Leftcommand_mem) == Mvl(etype(COMMAND,?,?));Type(controlled) == Mvl(btype(BOOL,?,?));Type(manual) == Mvl(btype(BOOL,?,?));Type(conduction) == Mvl(etype(CONDUCTION,?,?));Type(opposite) == Mvl(btype(BOOL,?,?));Type(leader) == Mvl(btype(BOOL,?,?));Type(cabin) == Mvl(etype(STATE,?,?));Type(traction) == Mvl(btype(BOOL,?,?));Type(slow_speed) == Mvl(btype(BOOL,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(GDC)) == (Type(update_controller) == Cst(No_type,btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*etype(COMMAND,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*etype(COMMAND,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)))
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
