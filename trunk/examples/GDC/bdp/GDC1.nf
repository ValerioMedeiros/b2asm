Normalised(
THEORY MagicNumberX IS
  MagicNumber(Refinement(GDC1))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Refinement(GDC1))==(Machine(GDC));
  Level(Refinement(GDC1))==(1);
  Upper_Level(Refinement(GDC1))==(Machine(GDC))
END
&
THEORY LoadedStructureX IS
  Refinement(GDC1)
END
&
THEORY ListSeesX IS
  List_Sees(Refinement(GDC1))==(Context,Control,BoolXor)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Refinement(GDC1))==(?);
  List_Includes(Refinement(GDC1))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Refinement(GDC1))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Refinement(GDC1))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Refinement(GDC1))==(?);
  Context_List_Variables(Refinement(GDC1))==(?);
  Abstract_List_Variables(Refinement(GDC1))==(Rightvisual_open,Righttl_command,Righttl_sensor_open,Rightclose_doors,Rightopen_doors,Rightcommand_mem,Leftvisual_open,Lefttl_command,Lefttl_sensor_open,Leftclose_doors,Leftopen_doors,Leftcommand_mem,controlled,manual,conduction,opposite,leader,cabin,traction,slow_speed);
  Local_List_Variables(Refinement(GDC1))==(Rightvisual_open,Righttl_command,Righttl_sensor_open,Rightclose_doors,Rightopen_doors,Rightcommand_mem,Leftvisual_open,Lefttl_command,Lefttl_sensor_open,Leftclose_doors,Leftopen_doors,Leftcommand_mem,controlled,manual,conduction,opposite,leader,cabin,traction,slow_speed);
  List_Variables(Refinement(GDC1))==(Rightvisual_open,Righttl_command,Righttl_sensor_open,Rightclose_doors,Rightopen_doors,Rightcommand_mem,Leftvisual_open,Lefttl_command,Lefttl_sensor_open,Leftclose_doors,Leftopen_doors,Leftcommand_mem,controlled,manual,conduction,opposite,leader,cabin,traction,slow_speed);
  External_List_Variables(Refinement(GDC1))==(Rightvisual_open,Righttl_command,Righttl_sensor_open,Rightclose_doors,Rightopen_doors,Rightcommand_mem,Leftvisual_open,Lefttl_command,Lefttl_sensor_open,Leftclose_doors,Leftopen_doors,Leftcommand_mem,controlled,manual,conduction,opposite,leader,cabin,traction,slow_speed)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Refinement(GDC1))==(?);
  Abstract_List_VisibleVariables(Refinement(GDC1))==(?);
  External_List_VisibleVariables(Refinement(GDC1))==(?);
  Expanded_List_VisibleVariables(Refinement(GDC1))==(?);
  List_VisibleVariables(Refinement(GDC1))==(?);
  Internal_List_VisibleVariables(Refinement(GDC1))==(?)
END
&
THEORY ListOfNewVariablesX IS
  List_Of_New_Variables(Refinement(GDC1))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Refinement(GDC1))==(btrue);
  Expanded_List_Invariant(Refinement(GDC1))==(btrue);
  Abstract_List_Invariant(Refinement(GDC1))==(slow_speed : BOOL & traction : BOOL & cabin : STATE & leader : BOOL & opposite : BOOL & not(leader = TRUE & opposite = TRUE) & leader = TRUE <=> (cabin = LEADER) & opposite = TRUE <=> (cabin = OPPOSITE) & (leader = FALSE & opposite = FALSE) <=> (cabin = NONE) & conduction : CONDUCTION & manual : BOOL & controlled : BOOL & conduction = MANUAL <=> (manual = TRUE & controlled = FALSE) & conduction = CONTROLLED <=> (manual = FALSE & controlled = TRUE) & conduction = CONDUCTION_OFF <=> (manual = FALSE & controlled = FALSE or (manual = TRUE & controlled = TRUE)) & Leftcommand_mem : COMMAND & Lefttl_command : COMMAND & Leftopen_doors : BOOL & Leftclose_doors : BOOL & Lefttl_sensor_open : BOOL & Leftvisual_open : BOOL & Leftvisual_open = Lefttl_sensor_open & Rightcommand_mem : COMMAND & Righttl_command : COMMAND & Rightopen_doors : BOOL & Rightclose_doors : BOOL & Righttl_sensor_open : BOOL & Rightvisual_open : BOOL & Rightvisual_open = Righttl_sensor_open & (cabin = LEADER & (conduction = MANUAL or conduction = CONTROLLED) => (slow_speed = FALSE => Lefttl_command = CLOSE_DOORS & Righttl_command = CLOSE_DOORS) & (Lefttl_sensor_open = TRUE or Righttl_sensor_open = TRUE => traction = FALSE)));
  Context_List_Invariant(Refinement(GDC1))==(btrue);
  List_Invariant(Refinement(GDC1))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Refinement(GDC1))==(btrue);
  Abstract_List_Assertions(Refinement(GDC1))==(btrue);
  Context_List_Assertions(Refinement(GDC1))==((conduction_p(TRUE,TRUE,FALSE) = TRUE;conduction_p(TRUE,FALSE,TRUE) = TRUE;conduction_p(TRUE,TRUE,TRUE) = FALSE;conduction_p(TRUE,FALSE,FALSE) = FALSE;!(mm,cc).(mm : BOOL & cc : BOOL => conduction_p(FALSE,mm,cc) = FALSE);!(cc,oo,mm).(cc : BOOL & oo : BOOL & mm : COMMAND => select_command(FALSE,cc,oo,mm) = CLOSE_DOORS);!(ss,oo,mm).(ss : BOOL & oo : BOOL & mm : COMMAND => select_command(ss,TRUE,oo,mm) = CLOSE_DOORS);!mm.(mm : COMMAND => select_command(TRUE,FALSE,TRUE,mm) = OPEN_DOORS);!mm.(mm : COMMAND => select_command(TRUE,FALSE,FALSE,mm) = mm)) & (xor(TRUE,FALSE) = TRUE;xor(FALSE,TRUE) = TRUE;xor(TRUE,TRUE) = FALSE;xor(FALSE,FALSE) = FALSE));
  List_Assertions(Refinement(GDC1))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Refinement(GDC1))==(slow_speed:=TRUE;traction:=TRUE;cabin:=NONE;leader:=FALSE;opposite:=FALSE;conduction:=MANUAL;manual:=TRUE;controlled:=FALSE;Leftcommand_mem:=CLOSE_DOORS;Lefttl_command:=CLOSE_DOORS;Leftopen_doors:=FALSE;Leftclose_doors:=TRUE;Lefttl_sensor_open:=FALSE;Leftvisual_open:=FALSE;Rightcommand_mem:=CLOSE_DOORS;Righttl_command:=CLOSE_DOORS;Rightopen_doors:=FALSE;Rightclose_doors:=TRUE;Righttl_sensor_open:=FALSE;Rightvisual_open:=FALSE);
  Context_List_Initialisation(Refinement(GDC1))==(skip);
  List_Initialisation(Refinement(GDC1))==(slow_speed:=TRUE;traction:=TRUE;cabin:=NONE;leader:=FALSE;opposite:=FALSE;conduction:=MANUAL;manual:=TRUE;controlled:=FALSE;Leftcommand_mem:=CLOSE_DOORS;Lefttl_command:=CLOSE_DOORS;Leftopen_doors:=FALSE;Leftclose_doors:=TRUE;Lefttl_sensor_open:=FALSE;Leftvisual_open:=FALSE;Rightcommand_mem:=CLOSE_DOORS;Righttl_command:=CLOSE_DOORS;Rightopen_doors:=FALSE;Rightclose_doors:=TRUE;Righttl_sensor_open:=FALSE;Rightvisual_open:=FALSE)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Refinement(GDC1))==(update_controller);
  List_Operations(Refinement(GDC1))==(update_controller)
END
&
THEORY ListInputX IS
  List_Input(Refinement(GDC1),update_controller)==(i_open_left_doors,i_close_left_doors,i_tl_sensor_left_open,i_tl_left_command,i_open_right_doors,i_close_right_doors,i_tl_sensor_right_open,i_tl_right_command,i_leader,i_opposite,i_manual,i_controlled,i_slow_speed)
END
&
THEORY ListOutputX IS
  List_Output(Refinement(GDC1),update_controller)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Refinement(GDC1),update_controller)==(update_controller(i_open_left_doors,i_close_left_doors,i_tl_sensor_left_open,i_tl_left_command,i_open_right_doors,i_close_right_doors,i_tl_sensor_right_open,i_tl_right_command,i_leader,i_opposite,i_manual,i_controlled,i_slow_speed))
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Refinement(GDC1),update_controller)==(i_open_left_doors : BOOL & i_close_left_doors : BOOL & i_tl_sensor_left_open : BOOL & i_open_right_doors : BOOL & i_close_right_doors : BOOL & i_tl_sensor_right_open : BOOL & i_leader : BOOL & i_opposite : BOOL & i_manual : BOOL & i_controlled : BOOL & i_slow_speed : BOOL & i_tl_left_command : COMMAND & i_tl_right_command : COMMAND & not(i_leader = TRUE & i_opposite = TRUE));
  List_Precondition(Refinement(GDC1),update_controller)==(i_open_left_doors : BOOL & i_close_left_doors : BOOL & i_tl_sensor_left_open : BOOL & i_open_right_doors : BOOL & i_close_right_doors : BOOL & i_tl_sensor_right_open : BOOL & i_leader : BOOL & i_opposite : BOOL & i_manual : BOOL & i_controlled : BOOL & i_slow_speed : BOOL & i_tl_left_command : COMMAND & i_tl_right_command : COMMAND & not(i_leader = TRUE & i_opposite = TRUE) & i_open_left_doors : BOOL & i_close_left_doors : BOOL & i_tl_sensor_left_open : BOOL & i_open_right_doors : BOOL & i_close_right_doors : BOOL & i_tl_sensor_right_open : BOOL & i_leader : BOOL & i_opposite : BOOL & i_manual : BOOL & i_controlled : BOOL & i_slow_speed : BOOL & i_tl_left_command : COMMAND & i_tl_right_command : COMMAND & not(i_leader = TRUE & i_opposite = TRUE))
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Refinement(GDC1),update_controller)==(i_open_left_doors : BOOL & i_close_left_doors : BOOL & i_tl_sensor_left_open : BOOL & i_open_right_doors : BOOL & i_close_right_doors : BOOL & i_tl_sensor_right_open : BOOL & i_leader : BOOL & i_opposite : BOOL & i_manual : BOOL & i_controlled : BOOL & i_slow_speed : BOOL & i_tl_left_command : COMMAND & i_tl_right_command : COMMAND & not(i_leader = TRUE & i_opposite = TRUE) & i_open_left_doors : BOOL & i_close_left_doors : BOOL & i_tl_sensor_left_open : BOOL & i_open_right_doors : BOOL & i_close_right_doors : BOOL & i_tl_sensor_right_open : BOOL & i_leader : BOOL & i_opposite : BOOL & i_manual : BOOL & i_controlled : BOOL & i_slow_speed : BOOL & i_tl_left_command : COMMAND & i_tl_right_command : COMMAND & not(i_leader = TRUE & i_opposite = TRUE) | leader:=i_leader;opposite:=i_opposite;(i_leader = TRUE ==> cabin:=LEADER [] not(i_leader = TRUE) ==> (i_opposite = TRUE ==> cabin:=OPPOSITE [] not(i_opposite = TRUE) ==> cabin:=NONE));slow_speed:=i_slow_speed;manual:=i_manual;controlled:=i_controlled;(i_manual = TRUE ==> (i_controlled = TRUE ==> conduction:=CONDUCTION_OFF [] not(i_controlled = TRUE) ==> conduction:=MANUAL) [] not(i_manual = TRUE) ==> (i_controlled = TRUE ==> conduction:=CONTROLLED [] not(i_controlled = TRUE) ==> conduction:=CONDUCTION_OFF));Leftopen_doors:=i_open_left_doors;Leftclose_doors:=i_close_left_doors;Lefttl_sensor_open:=i_tl_sensor_left_open;Leftvisual_open:=i_tl_sensor_left_open;Rightopen_doors:=i_open_right_doors;Rightclose_doors:=i_close_right_doors;Righttl_sensor_open:=i_tl_sensor_right_open;Rightvisual_open:=i_tl_sensor_right_open;@cond.(cond:=conduction_p(i_leader,i_manual,i_controlled);(cond = TRUE ==> (@cl.(cl : COMMAND & cl = select_command(i_slow_speed,i_close_left_doors,i_open_left_doors,Leftcommand_mem) ==> (cond = TRUE ==> Lefttl_command:=cl [] not(cond = TRUE) ==> Lefttl_command:=i_tl_left_command;(cond = TRUE ==> Leftcommand_mem:=cl [] not(cond = TRUE) ==> Leftcommand_mem:=i_tl_left_command)));@cr.(cr : COMMAND & cr = select_command(i_slow_speed,i_close_right_doors,i_open_right_doors,Rightcommand_mem) ==> (cond = TRUE ==> Righttl_command:=cr [] not(cond = TRUE) ==> Righttl_command:=i_tl_right_command;(cond = TRUE ==> Rightcommand_mem:=cr [] not(cond = TRUE) ==> Rightcommand_mem:=i_tl_right_command)))) [] not(cond = TRUE) ==> skip));(i_tl_sensor_left_open = TRUE or i_tl_sensor_right_open = TRUE ==> traction:=FALSE [] not(i_tl_sensor_left_open = TRUE or i_tl_sensor_right_open = TRUE) ==> traction:=TRUE));
  List_Substitution(Refinement(GDC1),update_controller)==(leader:=i_leader;opposite:=i_opposite;IF i_leader = TRUE THEN cabin:=LEADER ELSIF i_opposite = TRUE THEN cabin:=OPPOSITE ELSE cabin:=NONE END;slow_speed:=i_slow_speed;manual:=i_manual;controlled:=i_controlled;IF i_manual = TRUE THEN IF i_controlled = TRUE THEN conduction:=CONDUCTION_OFF ELSE conduction:=MANUAL END ELSE IF i_controlled = TRUE THEN conduction:=CONTROLLED ELSE conduction:=CONDUCTION_OFF END END;Leftopen_doors:=i_open_left_doors;Leftclose_doors:=i_close_left_doors;Lefttl_sensor_open:=i_tl_sensor_left_open;Leftvisual_open:=i_tl_sensor_left_open;Rightopen_doors:=i_open_right_doors;Rightclose_doors:=i_close_right_doors;Righttl_sensor_open:=i_tl_sensor_right_open;Rightvisual_open:=i_tl_sensor_right_open;VAR cond IN cond:=conduction_p(i_leader,i_manual,i_controlled);IF cond = TRUE THEN ANY cl WHERE cl : COMMAND & cl = select_command(i_slow_speed,i_close_left_doors,i_open_left_doors,Leftcommand_mem) THEN IF cond = TRUE THEN Lefttl_command:=cl ELSE Lefttl_command:=i_tl_left_command END;IF cond = TRUE THEN Leftcommand_mem:=cl ELSE Leftcommand_mem:=i_tl_left_command END END;ANY cr WHERE cr : COMMAND & cr = select_command(i_slow_speed,i_close_right_doors,i_open_right_doors,Rightcommand_mem) THEN IF cond = TRUE THEN Righttl_command:=cr ELSE Righttl_command:=i_tl_right_command END;IF cond = TRUE THEN Rightcommand_mem:=cr ELSE Rightcommand_mem:=i_tl_right_command END END END END;IF i_tl_sensor_left_open = TRUE or i_tl_sensor_right_open = TRUE THEN traction:=FALSE ELSE traction:=TRUE END)
END
&
THEORY ListParametersX IS
  List_Parameters(Refinement(GDC1))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Refinement(GDC1),Machine(Context))==(?);
  List_Instanciated_Parameters(Refinement(GDC1),Machine(Control))==(?);
  List_Instanciated_Parameters(Refinement(GDC1),Machine(BoolXor))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Refinement(GDC1))==(btrue);
  List_Context_Constraints(Refinement(GDC1))==(btrue)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Refinement(GDC1))==(?);
  Inherited_List_Constants(Refinement(GDC1))==(?);
  List_Constants(Refinement(GDC1))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Refinement(GDC1),COMMAND)==({NO_COMMAND,OPEN_DOORS,CLOSE_DOORS});
  Context_List_Enumerated(Refinement(GDC1))==(STATE,CONDUCTION,DOORS,COMMAND);
  Context_List_Defered(Refinement(GDC1))==(?);
  Context_List_Sets(Refinement(GDC1))==(STATE,CONDUCTION,DOORS,COMMAND);
  List_Valuable_Sets(Refinement(GDC1))==(?);
  Inherited_List_Enumerated(Refinement(GDC1))==(?);
  Inherited_List_Defered(Refinement(GDC1))==(?);
  Inherited_List_Sets(Refinement(GDC1))==(?);
  List_Enumerated(Refinement(GDC1))==(?);
  List_Defered(Refinement(GDC1))==(?);
  List_Sets(Refinement(GDC1))==(?);
  Set_Definition(Refinement(GDC1),DOORS)==({OPENED,CLOSED});
  Set_Definition(Refinement(GDC1),CONDUCTION)==({MANUAL,CONTROLLED,CONDUCTION_OFF});
  Set_Definition(Refinement(GDC1),STATE)==({LEADER,NONE,OPPOSITE})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Refinement(GDC1))==(?);
  Expanded_List_HiddenConstants(Refinement(GDC1))==(?);
  List_HiddenConstants(Refinement(GDC1))==(?);
  External_List_HiddenConstants(Refinement(GDC1))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Refinement(GDC1))==(btrue);
  Context_List_Properties(Refinement(GDC1))==(not(STATE = {}) & not(CONDUCTION = {}) & not(DOORS = {}) & not(COMMAND = {}) & conduction_p : BOOL*BOOL*BOOL --> BOOL & conduction_p = %(leader,manual,controlled).(leader : BOOL & manual : BOOL & controlled : BOOL | bool(leader = TRUE & xor(manual,controlled) = TRUE)) & !(l,m,c).(l : BOOL & m : BOOL & c : BOOL => conduction_p(l,m,c) = bool(l = TRUE & xor(m,c) = TRUE)) & select_command : BOOL*BOOL*BOOL*COMMAND --> COMMAND & !(slow,close,open,mem).(slow : BOOL & close : BOOL & open : BOOL & mem : COMMAND => (slow = FALSE or close = TRUE => select_command(slow,close,open,mem) = CLOSE_DOORS) & (slow = TRUE & close = FALSE & open = TRUE => select_command(slow,close,open,mem) = OPEN_DOORS) & (slow = TRUE & close = FALSE & open = FALSE => select_command(slow,close,open,mem) = mem)) & xor : BOOL*BOOL --> BOOL & !(a,b).(a : BOOL & b : BOOL => xor(a,b) = bool(a/=b)));
  Inherited_List_Properties(Refinement(GDC1))==(btrue);
  List_Properties(Refinement(GDC1))==(btrue)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Refinement(GDC1),Machine(BoolXor))==(?);
  Seen_Context_List_Enumerated(Refinement(GDC1))==(STATE,CONDUCTION,DOORS,COMMAND);
  Seen_Context_List_Invariant(Refinement(GDC1))==(btrue);
  Seen_Context_List_Assertions(Refinement(GDC1))==(xor(TRUE,FALSE) = TRUE;xor(FALSE,TRUE) = TRUE;xor(TRUE,TRUE) = FALSE;xor(FALSE,FALSE) = FALSE);
  Seen_Context_List_Properties(Refinement(GDC1))==(not(STATE = {}) & not(CONDUCTION = {}) & not(DOORS = {}) & not(COMMAND = {}) & xor : BOOL*BOOL --> BOOL & !(a,b).(a : BOOL & b : BOOL => xor(a,b) = bool(a/=b)));
  Seen_List_Constraints(Refinement(GDC1))==(btrue);
  Seen_List_Operations(Refinement(GDC1),Machine(BoolXor))==(?);
  Seen_Expanded_List_Invariant(Refinement(GDC1),Machine(BoolXor))==(btrue);
  Seen_Internal_List_Operations(Refinement(GDC1),Machine(Control))==(?);
  Seen_List_Operations(Refinement(GDC1),Machine(Control))==(?);
  Seen_Expanded_List_Invariant(Refinement(GDC1),Machine(Control))==(btrue);
  Set_Definition(Refinement(GDC1),COMMAND)==({NO_COMMAND,OPEN_DOORS,CLOSE_DOORS});
  Set_Definition(Refinement(GDC1),DOORS)==({OPENED,CLOSED});
  Set_Definition(Refinement(GDC1),CONDUCTION)==({MANUAL,CONTROLLED,CONDUCTION_OFF});
  Set_Definition(Refinement(GDC1),STATE)==({LEADER,NONE,OPPOSITE});
  Seen_Internal_List_Operations(Refinement(GDC1),Machine(Context))==(?);
  Seen_List_Operations(Refinement(GDC1),Machine(Context))==(?);
  Seen_Expanded_List_Invariant(Refinement(GDC1),Machine(Context))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Refinement(GDC1)) == (? | ? | Rightvisual_open,Righttl_command,Righttl_sensor_open,Rightclose_doors,Rightopen_doors,Rightcommand_mem,Leftvisual_open,Lefttl_command,Lefttl_sensor_open,Leftclose_doors,Leftopen_doors,Leftcommand_mem,controlled,manual,conduction,opposite,leader,cabin,traction,slow_speed | ? | update_controller | ? | seen(Machine(Context)),seen(Machine(Control)),seen(Machine(BoolXor)) | ? | GDC1);
  List_Of_HiddenCst_Ids(Refinement(GDC1)) == (? | ?);
  List_Of_VisibleCst_Ids(Refinement(GDC1)) == (?);
  List_Of_VisibleVar_Ids(Refinement(GDC1)) == (? | ?);
  List_Of_Ids_SeenBNU(Refinement(GDC1)) == (? : ?);
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
  Variables(Refinement(GDC1)) == (Type(Rightvisual_open) == Mvl(btype(BOOL,?,?));Type(Righttl_command) == Mvl(etype(COMMAND,?,?));Type(Righttl_sensor_open) == Mvl(btype(BOOL,?,?));Type(Rightclose_doors) == Mvl(btype(BOOL,?,?));Type(Rightopen_doors) == Mvl(btype(BOOL,?,?));Type(Rightcommand_mem) == Mvl(etype(COMMAND,?,?));Type(Leftvisual_open) == Mvl(btype(BOOL,?,?));Type(Lefttl_command) == Mvl(etype(COMMAND,?,?));Type(Lefttl_sensor_open) == Mvl(btype(BOOL,?,?));Type(Leftclose_doors) == Mvl(btype(BOOL,?,?));Type(Leftopen_doors) == Mvl(btype(BOOL,?,?));Type(Leftcommand_mem) == Mvl(etype(COMMAND,?,?));Type(controlled) == Mvl(btype(BOOL,?,?));Type(manual) == Mvl(btype(BOOL,?,?));Type(conduction) == Mvl(etype(CONDUCTION,?,?));Type(opposite) == Mvl(btype(BOOL,?,?));Type(leader) == Mvl(btype(BOOL,?,?));Type(cabin) == Mvl(etype(STATE,?,?));Type(traction) == Mvl(btype(BOOL,?,?));Type(slow_speed) == Mvl(btype(BOOL,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Refinement(GDC1)) == (Type(update_controller) == Cst(No_type,btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*etype(COMMAND,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*etype(COMMAND,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)))
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
