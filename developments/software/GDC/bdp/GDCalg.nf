Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(GDCalg))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(GDCalg))==(Machine(GDC));
  Level(Implementation(GDCalg))==(3);
  Upper_Level(Implementation(GDCalg))==(Refinement(GDC2))
END
&
THEORY LoadedStructureX IS
  Implementation(GDCalg)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(GDCalg))==(Context,ContextCode,Control,BoolXor)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(GDCalg))==(vslow_speed.Nat,vtraction.Nat,vcabin.Nat,vopposite.Nat,vconduction.Nat,vmanual.Nat,vcontrolled.Nat,vLeftcommand_mem.Nat,vLeftopen_doors.Nat,vLeftclose_doors.Nat,vLefttl_sensor_open.Nat,vLefttl_command.Nat,vLeftvisual_open.Nat,vRightcommand_mem.Nat,vRightopen_doors.Nat,vRightclose_doors.Nat,vRighttl_sensor_open.Nat,vRighttl_command.Nat,vRightvisual_open.Nat);
  Inherited_List_Includes(Implementation(GDCalg))==(vRightvisual_open.Nat,vRighttl_command.Nat,vRighttl_sensor_open.Nat,vRightclose_doors.Nat,vRightopen_doors.Nat,vRightcommand_mem.Nat,vLeftvisual_open.Nat,vLefttl_command.Nat,vLefttl_sensor_open.Nat,vLeftclose_doors.Nat,vLeftopen_doors.Nat,vLeftcommand_mem.Nat,vcontrolled.Nat,vmanual.Nat,vconduction.Nat,vopposite.Nat,vcabin.Nat,vtraction.Nat,vslow_speed.Nat)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(GDCalg))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(GDCalg))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(GDCalg))==(?);
  Context_List_Variables(Implementation(GDCalg))==(?);
  Abstract_List_Variables(Implementation(GDCalg))==(Rightvisual_open,Righttl_command,Righttl_sensor_open,Rightclose_doors,Rightopen_doors,Rightcommand_mem,Leftvisual_open,Lefttl_command,Lefttl_sensor_open,Leftclose_doors,Leftopen_doors,Leftcommand_mem,controlled,manual,conduction,opposite,leader,cabin,traction,slow_speed,Rightvisual_open,Righttl_command,Righttl_sensor_open,Rightclose_doors,Rightopen_doors,Rightcommand_mem,Leftvisual_open,Lefttl_command,Lefttl_sensor_open,Leftclose_doors,Leftopen_doors,Leftcommand_mem,controlled,manual,conduction,opposite,leader,cabin,traction,slow_speed,Rightvisual_open,Righttl_command,Righttl_sensor_open,Rightclose_doors,Rightopen_doors,Rightcommand_mem,Leftvisual_open,Lefttl_command,Lefttl_sensor_open,Leftclose_doors,Leftopen_doors,Leftcommand_mem,controlled,manual,conduction,opposite,leader,cabin,traction,slow_speed);
  Local_List_Variables(Implementation(GDCalg))==(?);
  List_Variables(Implementation(GDCalg))==(?);
  External_List_Variables(Implementation(GDCalg))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(GDCalg))==(?);
  Abstract_List_VisibleVariables(Implementation(GDCalg))==(?);
  External_List_VisibleVariables(Implementation(GDCalg))==(vslow_speed.value,vtraction.value,vcabin.value,vopposite.value,vconduction.value,vmanual.value,vcontrolled.value,vLeftcommand_mem.value,vLeftopen_doors.value,vLeftclose_doors.value,vLefttl_sensor_open.value,vLefttl_command.value,vLeftvisual_open.value,vRightcommand_mem.value,vRightopen_doors.value,vRightclose_doors.value,vRighttl_sensor_open.value,vRighttl_command.value,vRightvisual_open.value);
  Expanded_List_VisibleVariables(Implementation(GDCalg))==(vslow_speedvalue,vtractionvalue,vcabinvalue,voppositevalue,vconductionvalue,vmanualvalue,vcontrolledvalue,vLeftcommand_memvalue,vLeftopen_doorsvalue,vLeftclose_doorsvalue,vLefttl_sensor_openvalue,vLefttl_commandvalue,vLeftvisual_openvalue,vRightcommand_memvalue,vRightopen_doorsvalue,vRightclose_doorsvalue,vRighttl_sensor_openvalue,vRighttl_commandvalue,vRightvisual_openvalue);
  List_VisibleVariables(Implementation(GDCalg))==(vleader);
  Internal_List_VisibleVariables(Implementation(GDCalg))==(vleader)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(GDCalg))==(btrue);
  Abstract_List_Invariant(Implementation(GDCalg))==(slow_speed : BOOL & traction : BOOL & cabin : STATE & leader : BOOL & opposite : BOOL & not(leader = TRUE & opposite = TRUE) & leader = TRUE <=> (cabin = LEADER) & opposite = TRUE <=> (cabin = OPPOSITE) & (leader = FALSE & opposite = FALSE) <=> (cabin = NONE) & conduction : CONDUCTION & manual : BOOL & controlled : BOOL & conduction = MANUAL <=> (manual = TRUE & controlled = FALSE) & conduction = CONTROLLED <=> (manual = FALSE & controlled = TRUE) & conduction = CONDUCTION_OFF <=> (manual = FALSE & controlled = FALSE or (manual = TRUE & controlled = TRUE)) & Leftcommand_mem : COMMAND & Lefttl_command : COMMAND & Leftopen_doors : BOOL & Leftclose_doors : BOOL & Lefttl_sensor_open : BOOL & Leftvisual_open : BOOL & Leftvisual_open = Lefttl_sensor_open & Rightcommand_mem : COMMAND & Righttl_command : COMMAND & Rightopen_doors : BOOL & Rightclose_doors : BOOL & Righttl_sensor_open : BOOL & Rightvisual_open : BOOL & Rightvisual_open = Righttl_sensor_open & (cabin = LEADER & (conduction = MANUAL or conduction = CONTROLLED) => (slow_speed = FALSE => Lefttl_command = CLOSE_DOORS & Righttl_command = CLOSE_DOORS) & (Lefttl_sensor_open = TRUE or Righttl_sensor_open = TRUE => traction = FALSE)));
  Expanded_List_Invariant(Implementation(GDCalg))==(vslow_speedvalue : NATURAL & vslow_speedvalue<256 & vtractionvalue : NATURAL & vtractionvalue<256 & vcabinvalue : NATURAL & vcabinvalue<256 & voppositevalue : NATURAL & voppositevalue<256 & vconductionvalue : NATURAL & vconductionvalue<256 & vmanualvalue : NATURAL & vmanualvalue<256 & vcontrolledvalue : NATURAL & vcontrolledvalue<256 & vLeftcommand_memvalue : NATURAL & vLeftcommand_memvalue<256 & vLeftopen_doorsvalue : NATURAL & vLeftopen_doorsvalue<256 & vLeftclose_doorsvalue : NATURAL & vLeftclose_doorsvalue<256 & vLefttl_sensor_openvalue : NATURAL & vLefttl_sensor_openvalue<256 & vLefttl_commandvalue : NATURAL & vLefttl_commandvalue<256 & vLeftvisual_openvalue : NATURAL & vLeftvisual_openvalue<256 & vRightcommand_memvalue : NATURAL & vRightcommand_memvalue<256 & vRightopen_doorsvalue : NATURAL & vRightopen_doorsvalue<256 & vRightclose_doorsvalue : NATURAL & vRightclose_doorsvalue<256 & vRighttl_sensor_openvalue : NATURAL & vRighttl_sensor_openvalue<256 & vRighttl_commandvalue : NATURAL & vRighttl_commandvalue<256 & vRightvisual_openvalue : NATURAL & vRightvisual_openvalue<256);
  Context_List_Invariant(Implementation(GDCalg))==(btrue);
  List_Invariant(Implementation(GDCalg))==(vleader : NATURAL & vleader = BOOL_CODE(leader) & vslow_speedvalue = BOOL_CODE(slow_speed) & vtractionvalue = BOOL_CODE(traction) & vcabinvalue = STATE_CODE(cabin) & vconductionvalue = CONDUCTION_CODE(conduction) & vmanualvalue = BOOL_CODE(manual) & vcontrolledvalue = BOOL_CODE(controlled) & vLeftcommand_memvalue = COMMAND_CODE(Leftcommand_mem) & vLefttl_commandvalue = COMMAND_CODE(Lefttl_command) & vLeftopen_doorsvalue = BOOL_CODE(Leftopen_doors) & vLeftclose_doorsvalue = BOOL_CODE(Leftclose_doors) & vLeftvisual_openvalue = BOOL_CODE(Leftvisual_open) & vRightcommand_memvalue = COMMAND_CODE(Rightcommand_mem) & vRighttl_commandvalue = COMMAND_CODE(Righttl_command) & vRightopen_doorsvalue = BOOL_CODE(Rightopen_doors) & vRightclose_doorsvalue = BOOL_CODE(Rightclose_doors) & vRightvisual_openvalue = BOOL_CODE(Rightvisual_open))
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(GDCalg))==(btrue);
  Expanded_List_Assertions(Implementation(GDCalg))==(btrue);
  Context_List_Assertions(Implementation(GDCalg))==((conduction_p(TRUE,TRUE,FALSE) = TRUE;conduction_p(TRUE,FALSE,TRUE) = TRUE;conduction_p(TRUE,TRUE,TRUE) = FALSE;conduction_p(TRUE,FALSE,FALSE) = FALSE;!(mm,cc).(mm : BOOL & cc : BOOL => conduction_p(FALSE,mm,cc) = FALSE);!(cc,oo,mm).(cc : BOOL & oo : BOOL & mm : COMMAND => select_command(FALSE,cc,oo,mm) = CLOSE_DOORS);!(ss,oo,mm).(ss : BOOL & oo : BOOL & mm : COMMAND => select_command(ss,TRUE,oo,mm) = CLOSE_DOORS);!mm.(mm : COMMAND => select_command(TRUE,FALSE,TRUE,mm) = OPEN_DOORS);!mm.(mm : COMMAND => select_command(TRUE,FALSE,FALSE,mm) = mm)) & (xor(TRUE,FALSE) = TRUE;xor(FALSE,TRUE) = TRUE;xor(TRUE,TRUE) = FALSE;xor(FALSE,FALSE) = FALSE));
  List_Assertions(Implementation(GDCalg))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(GDCalg))==(vslow_speedvalue:=0;vtractionvalue:=0;vcabinvalue:=0;voppositevalue:=0;vconductionvalue:=0;vmanualvalue:=0;vcontrolledvalue:=0;vLeftcommand_memvalue:=0;vLeftopen_doorsvalue:=0;vLeftclose_doorsvalue:=0;vLefttl_sensor_openvalue:=0;vLefttl_commandvalue:=0;vLeftvisual_openvalue:=0;vRightcommand_memvalue:=0;vRightopen_doorsvalue:=0;vRightclose_doorsvalue:=0;vRighttl_sensor_openvalue:=0;vRighttl_commandvalue:=0;vRightvisual_openvalue:=0;(TRUE : dom(BOOL_CODE) & BOOL_CODE(TRUE) : NATURAL & BOOL_CODE(TRUE)<256 | vslow_speedvalue:=BOOL_CODE(TRUE));(TRUE : dom(BOOL_CODE) & BOOL_CODE(TRUE) : NATURAL & BOOL_CODE(TRUE)<256 | vtractionvalue:=BOOL_CODE(TRUE));(NONE : dom(STATE_CODE) & STATE_CODE(NONE) : NATURAL & STATE_CODE(NONE)<256 | vcabinvalue:=STATE_CODE(NONE));(BOOL_CODE(FALSE) : INT & FALSE : dom(BOOL_CODE) | vleader:=BOOL_CODE(FALSE));(FALSE : dom(BOOL_CODE) & BOOL_CODE(FALSE) : NATURAL & BOOL_CODE(FALSE)<256 | voppositevalue:=BOOL_CODE(FALSE));(MANUAL : dom(CONDUCTION_CODE) & CONDUCTION_CODE(MANUAL) : NATURAL & CONDUCTION_CODE(MANUAL)<256 | vconductionvalue:=CONDUCTION_CODE(MANUAL));(TRUE : dom(BOOL_CODE) & BOOL_CODE(TRUE) : NATURAL & BOOL_CODE(TRUE)<256 | vmanualvalue:=BOOL_CODE(TRUE));(FALSE : dom(BOOL_CODE) & BOOL_CODE(FALSE) : NATURAL & BOOL_CODE(FALSE)<256 | vcontrolledvalue:=BOOL_CODE(FALSE));(CLOSE_DOORS : dom(COMMAND_CODE) & COMMAND_CODE(CLOSE_DOORS) : NATURAL & COMMAND_CODE(CLOSE_DOORS)<256 | vLeftcommand_memvalue:=COMMAND_CODE(CLOSE_DOORS));(CLOSE_DOORS : dom(COMMAND_CODE) & COMMAND_CODE(CLOSE_DOORS) : NATURAL & COMMAND_CODE(CLOSE_DOORS)<256 | vLefttl_commandvalue:=COMMAND_CODE(CLOSE_DOORS));(FALSE : dom(BOOL_CODE) & BOOL_CODE(FALSE) : NATURAL & BOOL_CODE(FALSE)<256 | vLeftopen_doorsvalue:=BOOL_CODE(FALSE));(TRUE : dom(BOOL_CODE) & BOOL_CODE(TRUE) : NATURAL & BOOL_CODE(TRUE)<256 | vLeftclose_doorsvalue:=BOOL_CODE(TRUE));(FALSE : dom(BOOL_CODE) & BOOL_CODE(FALSE) : NATURAL & BOOL_CODE(FALSE)<256 | vLefttl_sensor_openvalue:=BOOL_CODE(FALSE));(FALSE : dom(BOOL_CODE) & BOOL_CODE(FALSE) : NATURAL & BOOL_CODE(FALSE)<256 | vLeftvisual_openvalue:=BOOL_CODE(FALSE));(CLOSE_DOORS : dom(COMMAND_CODE) & COMMAND_CODE(CLOSE_DOORS) : NATURAL & COMMAND_CODE(CLOSE_DOORS)<256 | vRightcommand_memvalue:=COMMAND_CODE(CLOSE_DOORS));(CLOSE_DOORS : dom(COMMAND_CODE) & COMMAND_CODE(CLOSE_DOORS) : NATURAL & COMMAND_CODE(CLOSE_DOORS)<256 | vRighttl_commandvalue:=COMMAND_CODE(CLOSE_DOORS));(FALSE : dom(BOOL_CODE) & BOOL_CODE(FALSE) : NATURAL & BOOL_CODE(FALSE)<256 | vRightopen_doorsvalue:=BOOL_CODE(FALSE));(TRUE : dom(BOOL_CODE) & BOOL_CODE(TRUE) : NATURAL & BOOL_CODE(TRUE)<256 | vRightclose_doorsvalue:=BOOL_CODE(TRUE));(FALSE : dom(BOOL_CODE) & BOOL_CODE(FALSE) : NATURAL & BOOL_CODE(FALSE)<256 | vRighttl_sensor_openvalue:=BOOL_CODE(FALSE));(FALSE : dom(BOOL_CODE) & BOOL_CODE(FALSE) : NATURAL & BOOL_CODE(FALSE)<256 | vRightvisual_openvalue:=BOOL_CODE(FALSE)));
  Context_List_Initialisation(Implementation(GDCalg))==(skip);
  List_Initialisation(Implementation(GDCalg))==((vslow_speed.set)(BOOL_CODE(TRUE));(vtraction.set)(BOOL_CODE(TRUE));(vcabin.set)(STATE_CODE(NONE));vleader:=BOOL_CODE(FALSE);(vopposite.set)(BOOL_CODE(FALSE));(vconduction.set)(CONDUCTION_CODE(MANUAL));(vmanual.set)(BOOL_CODE(TRUE));(vcontrolled.set)(BOOL_CODE(FALSE));(vLeftcommand_mem.set)(COMMAND_CODE(CLOSE_DOORS));(vLefttl_command.set)(COMMAND_CODE(CLOSE_DOORS));(vLeftopen_doors.set)(BOOL_CODE(FALSE));(vLeftclose_doors.set)(BOOL_CODE(TRUE));(vLefttl_sensor_open.set)(BOOL_CODE(FALSE));(vLeftvisual_open.set)(BOOL_CODE(FALSE));(vRightcommand_mem.set)(COMMAND_CODE(CLOSE_DOORS));(vRighttl_command.set)(COMMAND_CODE(CLOSE_DOORS));(vRightopen_doors.set)(BOOL_CODE(FALSE));(vRightclose_doors.set)(BOOL_CODE(TRUE));(vRighttl_sensor_open.set)(BOOL_CODE(FALSE));(vRightvisual_open.set)(BOOL_CODE(FALSE)))
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(GDCalg))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vslow_speed.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vtraction.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vcabin.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vopposite.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vconduction.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vmanual.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vcontrolled.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vLeftcommand_mem.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vLeftopen_doors.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vLeftclose_doors.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vLefttl_sensor_open.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vLefttl_command.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vLeftvisual_open.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vRightcommand_mem.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vRightopen_doors.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vRightclose_doors.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vRighttl_sensor_open.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vRighttl_command.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(vRightvisual_open.Nat))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(Context))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(ContextCode))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(Control))==(?);
  List_Instanciated_Parameters(Implementation(GDCalg),Machine(BoolXor))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(GDCalg),Machine(vRightvisual_open.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg))==(btrue);
  List_Context_Constraints(Implementation(GDCalg))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vRighttl_command.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vRighttl_sensor_open.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vRightclose_doors.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vRightopen_doors.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vRightcommand_mem.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vLeftvisual_open.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vLefttl_command.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vLefttl_sensor_open.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vLeftclose_doors.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vLeftopen_doors.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vLeftcommand_mem.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vcontrolled.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vmanual.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vconduction.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vopposite.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vcabin.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vtraction.Nat))==(btrue);
  List_Constraints(Implementation(GDCalg),Machine(vslow_speed.Nat))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(GDCalg))==(update_controller);
  List_Operations(Implementation(GDCalg))==(update_controller)
END
&
THEORY ListInputX IS
  List_Input(Implementation(GDCalg),update_controller)==(i_open_left_doors,i_close_left_doors,i_tl_sensor_left_open,i_tl_left_command,i_open_right_doors,i_close_right_doors,i_tl_sensor_right_open,i_tl_right_command,i_leader,i_opposite,i_manual,i_controlled,i_slow_speed)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(GDCalg),update_controller)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(GDCalg),update_controller)==(update_controller(i_open_left_doors,i_close_left_doors,i_tl_sensor_left_open,i_tl_left_command,i_open_right_doors,i_close_right_doors,i_tl_sensor_right_open,i_tl_right_command,i_leader,i_opposite,i_manual,i_controlled,i_slow_speed))
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(GDCalg),update_controller)==(btrue);
  List_Precondition(Implementation(GDCalg),update_controller)==(i_open_left_doors : BOOL & i_close_left_doors : BOOL & i_tl_sensor_left_open : BOOL & i_open_right_doors : BOOL & i_close_right_doors : BOOL & i_tl_sensor_right_open : BOOL & i_leader : BOOL & i_opposite : BOOL & i_manual : BOOL & i_controlled : BOOL & i_slow_speed : BOOL & i_tl_left_command : COMMAND & i_tl_right_command : COMMAND & i_open_left_doors : BOOL & i_close_left_doors : BOOL & i_tl_sensor_left_open : BOOL & i_open_right_doors : BOOL & i_close_right_doors : BOOL & i_tl_sensor_right_open : BOOL & i_leader : BOOL & i_opposite : BOOL & i_manual : BOOL & i_controlled : BOOL & i_slow_speed : BOOL & i_tl_left_command : COMMAND & i_tl_right_command : COMMAND & not(i_leader = TRUE & i_opposite = TRUE) & i_open_left_doors : BOOL & i_close_left_doors : BOOL & i_tl_sensor_left_open : BOOL & i_open_right_doors : BOOL & i_close_right_doors : BOOL & i_tl_sensor_right_open : BOOL & i_leader : BOOL & i_opposite : BOOL & i_manual : BOOL & i_controlled : BOOL & i_slow_speed : BOOL & i_tl_left_command : COMMAND & i_tl_right_command : COMMAND & not(i_leader = TRUE & i_opposite = TRUE))
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(GDCalg),update_controller)==(i_open_left_doors : BOOL & i_close_left_doors : BOOL & i_tl_sensor_left_open : BOOL & i_open_right_doors : BOOL & i_close_right_doors : BOOL & i_tl_sensor_right_open : BOOL & i_leader : BOOL & i_opposite : BOOL & i_manual : BOOL & i_controlled : BOOL & i_slow_speed : BOOL & i_tl_left_command : COMMAND & i_tl_right_command : COMMAND & i_open_left_doors : BOOL & i_close_left_doors : BOOL & i_tl_sensor_left_open : BOOL & i_open_right_doors : BOOL & i_close_right_doors : BOOL & i_tl_sensor_right_open : BOOL & i_leader : BOOL & i_opposite : BOOL & i_manual : BOOL & i_controlled : BOOL & i_slow_speed : BOOL & i_tl_left_command : COMMAND & i_tl_right_command : COMMAND & not(i_leader = TRUE & i_opposite = TRUE) & i_open_left_doors : BOOL & i_close_left_doors : BOOL & i_tl_sensor_left_open : BOOL & i_open_right_doors : BOOL & i_close_right_doors : BOOL & i_tl_sensor_right_open : BOOL & i_leader : BOOL & i_opposite : BOOL & i_manual : BOOL & i_controlled : BOOL & i_slow_speed : BOOL & i_tl_left_command : COMMAND & i_tl_right_command : COMMAND & not(i_leader = TRUE & i_opposite = TRUE) | (BOOL_CODE(i_leader) : INT & i_leader : dom(BOOL_CODE) | vleader:=BOOL_CODE(i_leader));(i_opposite : dom(BOOL_CODE) & BOOL_CODE(i_opposite) : NATURAL & BOOL_CODE(i_opposite)<256 | voppositevalue:=BOOL_CODE(i_opposite));(i_slow_speed : dom(BOOL_CODE) & BOOL_CODE(i_slow_speed) : NATURAL & BOOL_CODE(i_slow_speed)<256 | vslow_speedvalue:=BOOL_CODE(i_slow_speed));(i_manual : dom(BOOL_CODE) & BOOL_CODE(i_manual) : NATURAL & BOOL_CODE(i_manual)<256 | vmanualvalue:=BOOL_CODE(i_manual));(i_controlled : dom(BOOL_CODE) & BOOL_CODE(i_controlled) : NATURAL & BOOL_CODE(i_controlled)<256 | vcontrolledvalue:=BOOL_CODE(i_controlled));(i_open_left_doors : dom(BOOL_CODE) & BOOL_CODE(i_open_left_doors) : NATURAL & BOOL_CODE(i_open_left_doors)<256 | vLeftopen_doorsvalue:=BOOL_CODE(i_open_left_doors));(i_close_left_doors : dom(BOOL_CODE) & BOOL_CODE(i_close_left_doors) : NATURAL & BOOL_CODE(i_close_left_doors)<256 | vLeftclose_doorsvalue:=BOOL_CODE(i_close_left_doors));(i_tl_sensor_left_open : dom(BOOL_CODE) & BOOL_CODE(i_tl_sensor_left_open) : NATURAL & BOOL_CODE(i_tl_sensor_left_open)<256 | vLefttl_sensor_openvalue:=BOOL_CODE(i_tl_sensor_left_open));(i_tl_sensor_left_open : dom(BOOL_CODE) & BOOL_CODE(i_tl_sensor_left_open) : NATURAL & BOOL_CODE(i_tl_sensor_left_open)<256 | vLeftvisual_openvalue:=BOOL_CODE(i_tl_sensor_left_open));(i_open_right_doors : dom(BOOL_CODE) & BOOL_CODE(i_open_right_doors) : NATURAL & BOOL_CODE(i_open_right_doors)<256 | vRightopen_doorsvalue:=BOOL_CODE(i_open_right_doors));(i_close_right_doors : dom(BOOL_CODE) & BOOL_CODE(i_close_right_doors) : NATURAL & BOOL_CODE(i_close_right_doors)<256 | vRightclose_doorsvalue:=BOOL_CODE(i_close_right_doors));(i_tl_sensor_right_open : dom(BOOL_CODE) & BOOL_CODE(i_tl_sensor_right_open) : NATURAL & BOOL_CODE(i_tl_sensor_right_open)<256 | vRighttl_sensor_openvalue:=BOOL_CODE(i_tl_sensor_right_open));(i_tl_sensor_right_open : dom(BOOL_CODE) & BOOL_CODE(i_tl_sensor_right_open) : NATURAL & BOOL_CODE(i_tl_sensor_right_open)<256 | vRightvisual_openvalue:=BOOL_CODE(i_tl_sensor_right_open));(vleader = BOOL_CODE(TRUE) ==> (LEADER : dom(STATE_CODE) & STATE_CODE(LEADER) : NATURAL & STATE_CODE(LEADER)<256 | vcabinvalue:=STATE_CODE(LEADER)) [] not(vleader = BOOL_CODE(TRUE)) ==> (voppositevalue = BOOL_CODE(TRUE) ==> (OPPOSITE : dom(STATE_CODE) & STATE_CODE(OPPOSITE) : NATURAL & STATE_CODE(OPPOSITE)<256 | vcabinvalue:=STATE_CODE(OPPOSITE)) [] not(voppositevalue = BOOL_CODE(TRUE)) ==> (NONE : dom(STATE_CODE) & STATE_CODE(NONE) : NATURAL & STATE_CODE(NONE)<256 | vcabinvalue:=STATE_CODE(NONE))));(vmanualvalue = vcontrolledvalue ==> (CONDUCTION_OFF : dom(CONDUCTION_CODE) & CONDUCTION_CODE(CONDUCTION_OFF) : NATURAL & CONDUCTION_CODE(CONDUCTION_OFF)<256 | vconductionvalue:=CONDUCTION_CODE(CONDUCTION_OFF)) [] not(vmanualvalue = vcontrolledvalue) ==> (vmanualvalue = BOOL_CODE(TRUE) ==> (MANUAL : dom(CONDUCTION_CODE) & CONDUCTION_CODE(MANUAL) : NATURAL & CONDUCTION_CODE(MANUAL)<256 | vconductionvalue:=CONDUCTION_CODE(MANUAL)) [] not(vmanualvalue = BOOL_CODE(TRUE)) ==> (CONTROLLED : dom(CONDUCTION_CODE) & CONDUCTION_CODE(CONTROLLED) : NATURAL & CONDUCTION_CODE(CONTROLLED)<256 | vconductionvalue:=CONDUCTION_CODE(CONTROLLED))));@cond.((i_leader,i_manual,i_controlled : dom(conduction_p) | cond:=conduction_p(i_leader,i_manual,i_controlled));(cond = TRUE ==> @(cl,cr).((vslow_speedvalue,vLeftclose_doorsvalue,vLeftopen_doorsvalue,vLeftcommand_memvalue : dom(select_command_code) | cl:=select_command_code(vslow_speedvalue,vLeftclose_doorsvalue,vLeftopen_doorsvalue,vLeftcommand_memvalue));(vslow_speedvalue,vRightclose_doorsvalue,vRightopen_doorsvalue,vRightcommand_memvalue : dom(select_command_code) | cr:=select_command_code(vslow_speedvalue,vRightclose_doorsvalue,vRightopen_doorsvalue,vRightcommand_memvalue));(cond = TRUE ==> ((cl : NATURAL & cl<256 | vLefttl_commandvalue:=cl);(cl : NATURAL & cl<256 | vLeftcommand_memvalue:=cl);(cr : NATURAL & cr<256 | vRighttl_commandvalue:=cr);(cr : NATURAL & cr<256 | vRightcommand_memvalue:=cr)) [] not(cond = TRUE) ==> ((vLefttl_commandvalue : NATURAL & vLefttl_commandvalue<256 | vLefttl_commandvalue:=vLefttl_commandvalue);(vLefttl_commandvalue : NATURAL & vLefttl_commandvalue<256 | vLeftcommand_memvalue:=vLefttl_commandvalue);(vRighttl_commandvalue : NATURAL & vRighttl_commandvalue<256 | vRighttl_commandvalue:=vRighttl_commandvalue);(vRighttl_commandvalue : NATURAL & vRighttl_commandvalue<256 | vRightcommand_memvalue:=vRighttl_commandvalue)))) [] not(cond = TRUE) ==> skip));(vLefttl_sensor_openvalue = BOOL_CODE(TRUE) or vRighttl_sensor_openvalue = BOOL_CODE(TRUE) ==> (FALSE : dom(BOOL_CODE) & BOOL_CODE(FALSE) : NATURAL & BOOL_CODE(FALSE)<256 | vtractionvalue:=BOOL_CODE(FALSE)) [] not(vLefttl_sensor_openvalue = BOOL_CODE(TRUE) or vRighttl_sensor_openvalue = BOOL_CODE(TRUE)) ==> (TRUE : dom(BOOL_CODE) & BOOL_CODE(TRUE) : NATURAL & BOOL_CODE(TRUE)<256 | vtractionvalue:=BOOL_CODE(TRUE))));
  List_Substitution(Implementation(GDCalg),update_controller)==(vleader:=BOOL_CODE(i_leader);(vopposite.set)(BOOL_CODE(i_opposite));(vslow_speed.set)(BOOL_CODE(i_slow_speed));(vmanual.set)(BOOL_CODE(i_manual));(vcontrolled.set)(BOOL_CODE(i_controlled));(vLeftopen_doors.set)(BOOL_CODE(i_open_left_doors));(vLeftclose_doors.set)(BOOL_CODE(i_close_left_doors));(vLefttl_sensor_open.set)(BOOL_CODE(i_tl_sensor_left_open));(vLeftvisual_open.set)(BOOL_CODE(i_tl_sensor_left_open));(vRightopen_doors.set)(BOOL_CODE(i_open_right_doors));(vRightclose_doors.set)(BOOL_CODE(i_close_right_doors));(vRighttl_sensor_open.set)(BOOL_CODE(i_tl_sensor_right_open));(vRightvisual_open.set)(BOOL_CODE(i_tl_sensor_right_open));IF vleader = BOOL_CODE(TRUE) THEN (vcabin.set)(STATE_CODE(LEADER)) ELSIF vopposite.value = BOOL_CODE(TRUE) THEN (vcabin.set)(STATE_CODE(OPPOSITE)) ELSE (vcabin.set)(STATE_CODE(NONE)) END;IF vmanual.value = vcontrolled.value THEN (vconduction.set)(CONDUCTION_CODE(CONDUCTION_OFF)) ELSIF vmanual.value = BOOL_CODE(TRUE) THEN (vconduction.set)(CONDUCTION_CODE(MANUAL)) ELSE (vconduction.set)(CONDUCTION_CODE(CONTROLLED)) END;VAR cond IN cond:=conduction_p(i_leader,i_manual,i_controlled);IF cond = TRUE THEN VAR cl,cr IN cl:=select_command_code(vslow_speed.value,vLeftclose_doors.value,vLeftopen_doors.value,vLeftcommand_mem.value);cr:=select_command_code(vslow_speed.value,vRightclose_doors.value,vRightopen_doors.value,vRightcommand_mem.value);IF cond = TRUE THEN (vLefttl_command.set)(cl);(vLeftcommand_mem.set)(cl);(vRighttl_command.set)(cr);(vRightcommand_mem.set)(cr) ELSE (vLefttl_command.set)(vLefttl_command.value);(vLeftcommand_mem.set)(vLefttl_command.value);(vRighttl_command.set)(vRighttl_command.value);(vRightcommand_mem.set)(vRighttl_command.value) END END END END;IF vLefttl_sensor_open.value = BOOL_CODE(TRUE) or vRighttl_sensor_open.value = BOOL_CODE(TRUE) THEN (vtraction.set)(BOOL_CODE(FALSE)) ELSE (vtraction.set)(BOOL_CODE(TRUE)) END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(GDCalg))==(?);
  Inherited_List_Constants(Implementation(GDCalg))==(?);
  List_Constants(Implementation(GDCalg))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Implementation(GDCalg),COMMAND)==({NO_COMMAND,OPEN_DOORS,CLOSE_DOORS});
  Context_List_Enumerated(Implementation(GDCalg))==(STATE,CONDUCTION,DOORS,COMMAND);
  Context_List_Defered(Implementation(GDCalg))==(?);
  Context_List_Sets(Implementation(GDCalg))==(STATE,CONDUCTION,DOORS,COMMAND);
  List_Own_Enumerated(Implementation(GDCalg))==(?);
  List_Valuable_Sets(Implementation(GDCalg))==(?);
  Inherited_List_Enumerated(Implementation(GDCalg))==(?);
  Inherited_List_Defered(Implementation(GDCalg))==(?);
  Inherited_List_Sets(Implementation(GDCalg))==(?);
  List_Enumerated(Implementation(GDCalg))==(?);
  List_Defered(Implementation(GDCalg))==(?);
  List_Sets(Implementation(GDCalg))==(?);
  Set_Definition(Implementation(GDCalg),DOORS)==({OPENED,CLOSED});
  Set_Definition(Implementation(GDCalg),CONDUCTION)==({MANUAL,CONTROLLED,CONDUCTION_OFF});
  Set_Definition(Implementation(GDCalg),STATE)==({LEADER,NONE,OPPOSITE})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(GDCalg))==(?);
  Expanded_List_HiddenConstants(Implementation(GDCalg))==(?);
  List_HiddenConstants(Implementation(GDCalg))==(?);
  External_List_HiddenConstants(Implementation(GDCalg))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(GDCalg))==(btrue);
  Context_List_Properties(Implementation(GDCalg))==(not(STATE = {}) & not(CONDUCTION = {}) & not(DOORS = {}) & not(COMMAND = {}) & STATE_CODE : STATE --> NATURAL & STATE_CODE = {NONE|->0,LEADER|->1,OPPOSITE|->2} & CONDUCTION_CODE : CONDUCTION --> NATURAL & CONDUCTION_CODE = {CONDUCTION_OFF|->0,MANUAL|->1,CONTROLLED|->2} & DOORS_CODE : DOORS --> NATURAL & DOORS_CODE = {OPENED|->0,CLOSED|->1} & COMMAND_CODE : COMMAND --> NATURAL & COMMAND_CODE = {NO_COMMAND|->0,OPEN_DOORS|->1,CLOSE_DOORS|->2} & BOOL_CODE : BOOL --> NATURAL & BOOL_CODE = {FALSE|->0,TRUE|->1} & select_command_code : NATURAL*NATURAL*NATURAL*NATURAL --> NATURAL & !(slow,close,open,mem).(slow : NATURAL & close : NATURAL & open : NATURAL & mem : NATURAL => (slow = BOOL_CODE(FALSE) or close = BOOL_CODE(TRUE) => select_command_code(slow,close,open,mem) = COMMAND_CODE(CLOSE_DOORS)) & (slow = BOOL_CODE(TRUE) & close = BOOL_CODE(FALSE) & open = BOOL_CODE(TRUE) => select_command_code(slow,close,open,mem) = COMMAND_CODE(OPEN_DOORS)) & (slow = BOOL_CODE(TRUE) & close = BOOL_CODE(FALSE) & open = BOOL_CODE(FALSE) => select_command_code(slow,close,open,mem) = mem)) & conduction_p : BOOL*BOOL*BOOL --> BOOL & conduction_p = %(leader,manual,controlled).(leader : BOOL & manual : BOOL & controlled : BOOL | bool(leader = TRUE & xor(manual,controlled) = TRUE)) & !(l,m,c).(l : BOOL & m : BOOL & c : BOOL => conduction_p(l,m,c) = bool(l = TRUE & xor(m,c) = TRUE)) & select_command : BOOL*BOOL*BOOL*COMMAND --> COMMAND & !(slow,close,open,mem).(slow : BOOL & close : BOOL & open : BOOL & mem : COMMAND => (slow = FALSE or close = TRUE => select_command(slow,close,open,mem) = CLOSE_DOORS) & (slow = TRUE & close = FALSE & open = TRUE => select_command(slow,close,open,mem) = OPEN_DOORS) & (slow = TRUE & close = FALSE & open = FALSE => select_command(slow,close,open,mem) = mem)) & xor : BOOL*BOOL --> BOOL & !(a,b).(a : BOOL & b : BOOL => xor(a,b) = bool(a/=b)));
  Inherited_List_Properties(Implementation(GDCalg))==(btrue);
  List_Properties(Implementation(GDCalg))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(GDCalg))==(aa : aa);
  List_Values(Implementation(GDCalg))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(GDCalg),Machine(BoolXor))==(?);
  Seen_Context_List_Enumerated(Implementation(GDCalg))==(STATE,CONDUCTION,DOORS,COMMAND);
  Seen_Context_List_Invariant(Implementation(GDCalg))==(btrue);
  Seen_Context_List_Assertions(Implementation(GDCalg))==(xor(TRUE,FALSE) = TRUE;xor(FALSE,TRUE) = TRUE;xor(TRUE,TRUE) = FALSE;xor(FALSE,FALSE) = FALSE);
  Seen_Context_List_Properties(Implementation(GDCalg))==(not(STATE = {}) & not(CONDUCTION = {}) & not(DOORS = {}) & not(COMMAND = {}) & !(a,b).(a : BOOL & b : BOOL => xor(a,b) = bool(a/=b)) & xor : BOOL*BOOL --> BOOL);
  Seen_List_Constraints(Implementation(GDCalg))==(btrue);
  Seen_List_Operations(Implementation(GDCalg),Machine(BoolXor))==(?);
  Seen_Expanded_List_Invariant(Implementation(GDCalg),Machine(BoolXor))==(btrue);
  Seen_Internal_List_Operations(Implementation(GDCalg),Machine(Control))==(?);
  Seen_List_Operations(Implementation(GDCalg),Machine(Control))==(?);
  Seen_Expanded_List_Invariant(Implementation(GDCalg),Machine(Control))==(btrue);
  Seen_Internal_List_Operations(Implementation(GDCalg),Machine(ContextCode))==(?);
  Seen_List_Operations(Implementation(GDCalg),Machine(ContextCode))==(?);
  Seen_Expanded_List_Invariant(Implementation(GDCalg),Machine(ContextCode))==(btrue);
  Set_Definition(Implementation(GDCalg),COMMAND)==({NO_COMMAND,OPEN_DOORS,CLOSE_DOORS});
  Set_Definition(Implementation(GDCalg),DOORS)==({OPENED,CLOSED});
  Set_Definition(Implementation(GDCalg),CONDUCTION)==({MANUAL,CONTROLLED,CONDUCTION_OFF});
  Set_Definition(Implementation(GDCalg),STATE)==({LEADER,NONE,OPPOSITE});
  Seen_Internal_List_Operations(Implementation(GDCalg),Machine(Context))==(?);
  Seen_List_Operations(Implementation(GDCalg),Machine(Context))==(?);
  Seen_Expanded_List_Invariant(Implementation(GDCalg),Machine(Context))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(GDCalg),Machine(Nat))==(set,get)
END
&
THEORY InheritedEnvX IS
  VisibleVariables(Implementation(GDCalg))==(Type(vleader) == Mvv(btype(INTEGER,?,?)));
  Operations(Implementation(GDCalg))==(Type(update_controller) == Cst(No_type,btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*etype(COMMAND,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*etype(COMMAND,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)))
END
&
THEORY ListVisibleStaticX IS
  List_Constants(Implementation(GDCalg),Machine(BoolXor))==(xor);
  List_Constants_Env(Implementation(GDCalg),Machine(BoolXor))==(Type(xor) == Cst(SetOf(btype(BOOL,0,1)*btype(BOOL,0,1)*btype(BOOL,0,1))));
  List_Constants(Implementation(GDCalg),Machine(Control))==(conduction_p,select_command);
  List_Constants_Env(Implementation(GDCalg),Machine(Control))==(Type(conduction_p) == Cst(SetOf(btype(BOOL,0,1)*btype(BOOL,0,1)*btype(BOOL,0,1)*btype(BOOL,0,1)));Type(select_command) == Cst(SetOf(btype(BOOL,0,1)*btype(BOOL,0,1)*btype(BOOL,0,1)*etype(COMMAND,0,2)*etype(COMMAND,0,2))));
  List_Constants(Implementation(GDCalg),Machine(ContextCode))==(STATE_CODE,CONDUCTION_CODE,DOORS_CODE,COMMAND_CODE,BOOL_CODE,select_command_code);
  List_Constants_Env(Implementation(GDCalg),Machine(ContextCode))==(Type(STATE_CODE) == Cst(SetOf(etype(STATE,0,2)*btype(INTEGER,?,?)));Type(CONDUCTION_CODE) == Cst(SetOf(etype(CONDUCTION,0,2)*btype(INTEGER,?,?)));Type(DOORS_CODE) == Cst(SetOf(etype(DOORS,0,1)*btype(INTEGER,?,?)));Type(COMMAND_CODE) == Cst(SetOf(etype(COMMAND,0,2)*btype(INTEGER,?,?)));Type(BOOL_CODE) == Cst(SetOf(btype(BOOL,0,1)*btype(INTEGER,?,?)));Type(select_command_code) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_Constants_Env(Implementation(GDCalg),Machine(Context))==(Type(LEADER) == Cst(etype(STATE,0,2));Type(NONE) == Cst(etype(STATE,0,2));Type(OPPOSITE) == Cst(etype(STATE,0,2));Type(MANUAL) == Cst(etype(CONDUCTION,0,2));Type(CONTROLLED) == Cst(etype(CONDUCTION,0,2));Type(CONDUCTION_OFF) == Cst(etype(CONDUCTION,0,2));Type(OPENED) == Cst(etype(DOORS,0,1));Type(CLOSED) == Cst(etype(DOORS,0,1));Type(NO_COMMAND) == Cst(etype(COMMAND,0,2));Type(OPEN_DOORS) == Cst(etype(COMMAND,0,2));Type(CLOSE_DOORS) == Cst(etype(COMMAND,0,2)));
  Enumerate_Definition(Implementation(GDCalg),Machine(Context),COMMAND)==({NO_COMMAND,OPEN_DOORS,CLOSE_DOORS});
  Enumerate_Definition(Implementation(GDCalg),Machine(Context),DOORS)==({OPENED,CLOSED});
  Enumerate_Definition(Implementation(GDCalg),Machine(Context),CONDUCTION)==({MANUAL,CONTROLLED,CONDUCTION_OFF});
  Enumerate_Definition(Implementation(GDCalg),Machine(Context),STATE)==({LEADER,NONE,OPPOSITE})
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(GDCalg)) == (? | ? | ? | ? | update_controller | ? | seen(Machine(Context)),seen(Machine(ContextCode)),seen(Machine(Control)),seen(Machine(BoolXor)),imported(Machine(vslow_speed.Nat)),imported(Machine(vtraction.Nat)),imported(Machine(vcabin.Nat)),imported(Machine(vopposite.Nat)),imported(Machine(vconduction.Nat)),imported(Machine(vmanual.Nat)),imported(Machine(vcontrolled.Nat)),imported(Machine(vLeftcommand_mem.Nat)),imported(Machine(vLeftopen_doors.Nat)),imported(Machine(vLeftclose_doors.Nat)),imported(Machine(vLefttl_sensor_open.Nat)),imported(Machine(vLefttl_command.Nat)),imported(Machine(vLeftvisual_open.Nat)),imported(Machine(vRightcommand_mem.Nat)),imported(Machine(vRightopen_doors.Nat)),imported(Machine(vRightclose_doors.Nat)),imported(Machine(vRighttl_sensor_open.Nat)),imported(Machine(vRighttl_command.Nat)),imported(Machine(vRightvisual_open.Nat)) | ? | GDCalg);
  List_Of_HiddenCst_Ids(Implementation(GDCalg)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(GDCalg)) == (?);
  List_Of_VisibleVar_Ids(Implementation(GDCalg)) == (vleader | vRightvisual_openvalue,vRighttl_commandvalue,vRighttl_sensor_openvalue,vRightclose_doorsvalue,vRightopen_doorsvalue,vRightcommand_memvalue,vLeftvisual_openvalue,vLefttl_commandvalue,vLefttl_sensor_openvalue,vLeftclose_doorsvalue,vLeftopen_doorsvalue,vLeftcommand_memvalue,vcontrolledvalue,vmanualvalue,vconductionvalue,voppositevalue,vcabinvalue,vtractionvalue,vslow_speedvalue);
  List_Of_Ids_SeenBNU(Implementation(GDCalg)) == (? : ?);
  List_Of_Ids(Machine(Nat)) == (? | ? | ? | ? | set,get | ? | ? | ? | Nat);
  List_Of_HiddenCst_Ids(Machine(Nat)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Nat)) == (?);
  List_Of_VisibleVar_Ids(Machine(Nat)) == (value | ?);
  List_Of_Ids_SeenBNU(Machine(Nat)) == (? : ?);
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
  List_Of_Ids_SeenBNU(Machine(Context)) == (? : ?);
  List_Of_Ids(Machine(ContextCode)) == (STATE_CODE,CONDUCTION_CODE,DOORS_CODE,COMMAND_CODE,BOOL_CODE,select_command_code | ? | ? | ? | ? | ? | seen(Machine(Context)) | ? | ContextCode);
  List_Of_HiddenCst_Ids(Machine(ContextCode)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ContextCode)) == (STATE_CODE,CONDUCTION_CODE,DOORS_CODE,COMMAND_CODE,BOOL_CODE,select_command_code);
  List_Of_VisibleVar_Ids(Machine(ContextCode)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ContextCode)) == (? : ?)
END
&
THEORY VisibleVariablesEnvX IS
  VisibleVariables(Implementation(GDCalg)) == (Type(vleader) == Mvv(btype(INTEGER,?,?)))
END
&
THEORY VariablesLocEnvX IS
  Variables_Loc(Implementation(GDCalg),update_controller, 2) == (Type(cl) == Lvl(btype(INTEGER,?,?));Type(cr) == Lvl(btype(INTEGER,?,?)));
  Variables_Loc(Implementation(GDCalg),update_controller, 1) == (Type(cond) == Lvl(btype(BOOL,?,?)))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO
END
&
THEORY ListLocalOperationsX IS
  List_Local_Operations(Implementation(GDCalg))==(?)
END
&
THEORY ListLocalInputX END
&
THEORY ListLocalOutputX END
&
THEORY ListLocalHeaderX END
&
THEORY ListLocalPreconditionX END
&
THEORY ListLocalSubstitutionX END
&
THEORY TypingPredicateX IS
  TypingPredicate(Implementation(GDCalg))==(vleader : INTEGER)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(GDCalg),Machine(vslow_speed.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vslow_speed.Nat))==(vslow_speed.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vtraction.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vtraction.Nat))==(vtraction.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vcabin.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vcabin.Nat))==(vcabin.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vopposite.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vopposite.Nat))==(vopposite.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vconduction.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vconduction.Nat))==(vconduction.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vmanual.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vmanual.Nat))==(vmanual.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vcontrolled.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vcontrolled.Nat))==(vcontrolled.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vLeftcommand_mem.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vLeftcommand_mem.Nat))==(vLeftcommand_mem.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vLeftopen_doors.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vLeftopen_doors.Nat))==(vLeftopen_doors.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vLeftclose_doors.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vLeftclose_doors.Nat))==(vLeftclose_doors.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vLefttl_sensor_open.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vLefttl_sensor_open.Nat))==(vLefttl_sensor_open.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vLefttl_command.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vLefttl_command.Nat))==(vLefttl_command.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vLeftvisual_open.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vLeftvisual_open.Nat))==(vLeftvisual_open.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vRightcommand_mem.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vRightcommand_mem.Nat))==(vRightcommand_mem.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vRightopen_doors.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vRightopen_doors.Nat))==(vRightopen_doors.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vRightclose_doors.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vRightclose_doors.Nat))==(vRightclose_doors.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vRighttl_sensor_open.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vRighttl_sensor_open.Nat))==(vRighttl_sensor_open.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vRighttl_command.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vRighttl_command.Nat))==(vRighttl_command.value);
  ImportedVariablesList(Implementation(GDCalg),Machine(vRightvisual_open.Nat))==(?);
  ImportedVisVariablesList(Implementation(GDCalg),Machine(vRightvisual_open.Nat))==(vRightvisual_open.value)
END
&
THEORY ListLocalOpInvariantX END
)
