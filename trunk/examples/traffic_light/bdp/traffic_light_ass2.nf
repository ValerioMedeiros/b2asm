Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(traffic_light_ass2))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(traffic_light_ass2))==(Machine(traffic_light));
  Level(Implementation(traffic_light_ass2))==(2);
  Upper_Level(Implementation(traffic_light_ass2))==(Refinement(traffic_light_data_refinement))
END
&
THEORY LoadedStructureX IS
  Implementation(traffic_light_ass2)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(traffic_light_ass2))==(definitions,data_refinement)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(traffic_light_ass2))==(uc.Microcontroller);
  Inherited_List_Includes(Implementation(traffic_light_ass2))==(uc.Microcontroller)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(traffic_light_ass2))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(traffic_light_ass2))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(traffic_light_ass2))==(?);
  Context_List_Variables(Implementation(traffic_light_ass2))==(?);
  Abstract_List_Variables(Implementation(traffic_light_ass2))==(counter,color);
  Local_List_Variables(Implementation(traffic_light_ass2))==(?);
  List_Variables(Implementation(traffic_light_ass2))==(?);
  External_List_Variables(Implementation(traffic_light_ass2))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(traffic_light_ass2))==(?);
  Abstract_List_VisibleVariables(Implementation(traffic_light_ass2))==(?);
  External_List_VisibleVariables(Implementation(traffic_light_ass2))==(uc.end,uc.pc,uc.w,uc.stack,uc.memory_data);
  Expanded_List_VisibleVariables(Implementation(traffic_light_ass2))==(ucend,ucpc,ucw,ucstack,ucmemory_data);
  List_VisibleVariables(Implementation(traffic_light_ass2))==(?);
  Internal_List_VisibleVariables(Implementation(traffic_light_ass2))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(traffic_light_ass2))==(btrue);
  Abstract_List_Invariant(Implementation(traffic_light_ass2))==(counter : NATURAL & counter : 0..2 & counter = color_refine(color) & color : COLOR);
  Expanded_List_Invariant(Implementation(traffic_light_ass2))==(ucmemory_data : NATURAL --> NATURAL & ucstack : 1..8 +-> NATURAL & card(ucstack)<=8 & ucw : NATURAL & ucpc : NATURAL & ucend : NATURAL & ucpc<=ucend);
  Context_List_Invariant(Implementation(traffic_light_ass2))==(btrue);
  List_Invariant(Implementation(traffic_light_ass2))==(ucmemory_data(0) : 0..2 & ucmemory_data(0) = counter)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(traffic_light_ass2))==(btrue);
  Expanded_List_Assertions(Implementation(traffic_light_ass2))==(btrue);
  Context_List_Assertions(Implementation(traffic_light_ass2))==(!c.(c : COLOR => c = green or c = yellow or c = red) & color_refine(green) = 0 & color_refine(yellow) = 1 & color_refine(red) = 2);
  List_Assertions(Implementation(traffic_light_ass2))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(traffic_light_ass2))==(@(memory_data$0).(memory_data$0 : NATURAL --> NATURAL ==> ucmemory_data:=memory_data$0) || ucstack:={1|->0,2|->0,3|->0,4|->0,5|->0,6|->0,7|->0,8|->0} || ucw:=0 || ucpc:=0 || ucend:=0;(0 : NATURAL & 0 : NATURAL & dom(ucmemory_data<+{0|->0}) = NATURAL | ucmemory_data:=ucmemory_data<+{0|->0}));
  Context_List_Initialisation(Implementation(traffic_light_ass2))==(skip);
  List_Initialisation(Implementation(traffic_light_ass2))==((uc.init_data)(0,0))
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(traffic_light_ass2))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(traffic_light_ass2),Machine(uc.Microcontroller))==(?);
  List_Instanciated_Parameters(Implementation(traffic_light_ass2),Machine(definitions))==(?);
  List_Instanciated_Parameters(Implementation(traffic_light_ass2),Machine(data_refinement))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(traffic_light_ass2),Machine(uc.Microcontroller))==(btrue);
  List_Constraints(Implementation(traffic_light_ass2))==(btrue);
  List_Context_Constraints(Implementation(traffic_light_ass2))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(traffic_light_ass2))==(advance);
  List_Operations(Implementation(traffic_light_ass2))==(advance)
END
&
THEORY ListInputX IS
  List_Input(Implementation(traffic_light_ass2),advance)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(traffic_light_ass2),advance)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(traffic_light_ass2),advance)==(advance)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(traffic_light_ass2),advance)==(btrue);
  List_Precondition(Implementation(traffic_light_ass2),advance)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(traffic_light_ass2),advance)==(btrue | (0 : NATURAL & 10 : NATURAL & 0<=10 | ucpc,ucend:=0,10);WHILE ucpc<ucend DO not(ucpc = 9) & not(ucpc = 8) & not(ucpc = 7) & not(ucpc = 6) & not(ucpc = 5) & not(ucpc = 4) & not(ucpc = 3) & not(ucpc = 2) & not(ucpc = 1) & ucpc = 0 ==> (0 : NATURAL & 0 : NATURAL & ucpc+2<ucend | ucmemory_data(0) = 0 ==> ucpc:=ucpc+1 [] not(ucmemory_data(0) = 0) ==> ucpc:=ucpc+2) [] not(ucpc = 0) & not(ucpc = 9) & not(ucpc = 8) & not(ucpc = 7) & not(ucpc = 6) & not(ucpc = 5) & not(ucpc = 4) & not(ucpc = 3) & not(ucpc = 2) & ucpc = 1 ==> (5 : NATURAL & 5<=ucend | ucpc:=5) [] not(ucpc = 0) & not(ucpc = 1) & not(ucpc = 9) & not(ucpc = 8) & not(ucpc = 7) & not(ucpc = 6) & not(ucpc = 5) & not(ucpc = 4) & not(ucpc = 3) & ucpc = 2 ==> (0 : NATURAL & 1 : NATURAL & ucpc+2<ucend | ucmemory_data(0) = 1 ==> ucpc:=ucpc+1 [] not(ucmemory_data(0) = 1) ==> ucpc:=ucpc+2) [] not(ucpc = 0) & not(ucpc = 1) & not(ucpc = 2) & not(ucpc = 9) & not(ucpc = 8) & not(ucpc = 7) & not(ucpc = 6) & not(ucpc = 5) & not(ucpc = 4) & ucpc = 3 ==> (7 : NATURAL & 7<=ucend | ucpc:=7) [] not(ucpc = 0) & not(ucpc = 1) & not(ucpc = 2) & not(ucpc = 3) & not(ucpc = 9) & not(ucpc = 8) & not(ucpc = 7) & not(ucpc = 6) & not(ucpc = 5) & ucpc = 4 ==> (9 : NATURAL & 9<=ucend | ucpc:=9) [] not(ucpc = 0) & not(ucpc = 1) & not(ucpc = 2) & not(ucpc = 3) & not(ucpc = 4) & not(ucpc = 9) & not(ucpc = 8) & not(ucpc = 7) & not(ucpc = 6) & ucpc = 5 ==> (0 : NATURAL & 1 : NATURAL & ucpc+1<=ucend | ucmemory_data,ucpc:=ucmemory_data<+{0|->1},ucpc+1) [] not(ucpc = 0) & not(ucpc = 1) & not(ucpc = 2) & not(ucpc = 3) & not(ucpc = 4) & not(ucpc = 5) & not(ucpc = 9) & not(ucpc = 8) & not(ucpc = 7) & ucpc = 6 ==> (10 : NATURAL & 10<=ucend | ucpc:=10) [] not(ucpc = 0) & not(ucpc = 1) & not(ucpc = 2) & not(ucpc = 3) & not(ucpc = 4) & not(ucpc = 5) & not(ucpc = 6) & not(ucpc = 9) & not(ucpc = 8) & ucpc = 7 ==> (0 : NATURAL & 2 : NATURAL & ucpc+1<=ucend | ucmemory_data,ucpc:=ucmemory_data<+{0|->2},ucpc+1) [] not(ucpc = 0) & not(ucpc = 1) & not(ucpc = 2) & not(ucpc = 3) & not(ucpc = 4) & not(ucpc = 5) & not(ucpc = 6) & not(ucpc = 7) & not(ucpc = 9) & ucpc = 8 ==> (10 : NATURAL & 10<=ucend | ucpc:=10) [] not(ucpc = 0) & not(ucpc = 1) & not(ucpc = 2) & not(ucpc = 3) & not(ucpc = 4) & not(ucpc = 5) & not(ucpc = 6) & not(ucpc = 7) & not(ucpc = 8) & ucpc = 9 ==> (0 : NATURAL & 0 : NATURAL & ucpc+1<=ucend | ucmemory_data,ucpc:=ucmemory_data<+{0|->0},ucpc+1) [] not(ucpc = 0) & not(ucpc = 1) & not(ucpc = 2) & not(ucpc = 3) & not(ucpc = 4) & not(ucpc = 5) & not(ucpc = 6) & not(ucpc = 7) & not(ucpc = 8) & not(ucpc = 9) ==> skip INVARIANT 0<=ucpc & ucpc<=10 & ucmemory_data(0) : 0..2 & ucmemory_data(0) = counter & (ucpc = 0 => ucmemory_data(0) : 0..2) & (ucpc = 1 => ucmemory_data(0) = 0) & (ucpc = 2 => ucmemory_data(0)/=0) & (ucpc = 3 => ucmemory_data(0) = 1) & (ucpc = 4 => ucmemory_data(0) = 2) & (ucpc = 5 => ucmemory_data(0) = 0) & (ucpc = 6 => ucmemory_data(0) = 1) & (ucpc = 7 => ucmemory_data(0) = 1) & (ucpc = 8 => ucmemory_data(0) = 2) & (ucpc = 9 => ucmemory_data(0) = 2) VARIANT ucend-ucpc END);
  List_Substitution(Implementation(traffic_light_ass2),advance)==((uc.init)(0,10);WHILE uc.pc<uc.end DO BEGIN CASE uc.pc OF EITHER 0 THEN (uc.isequal)(0,0) OR 1 THEN (uc.goto)(5) OR 2 THEN (uc.isequal)(0,1) OR 3 THEN (uc.goto)(7) OR 4 THEN (uc.goto)(9) OR 5 THEN (uc.set_data)(0,1) OR 6 THEN (uc.goto)(10) OR 7 THEN (uc.set_data)(0,2) OR 8 THEN (uc.goto)(10) OR 9 THEN (uc.set_data)(0,0) END END END INVARIANT 0<=uc.pc & uc.pc<=10 & (uc.memory_data)(0) : 0..2 & (uc.memory_data)(0) = counter & (uc.pc = 0 => (uc.memory_data)(0) : 0..2) & (uc.pc = 1 => (uc.memory_data)(0) = 0) & (uc.pc = 2 => (uc.memory_data)(0)/=0) & (uc.pc = 3 => (uc.memory_data)(0) = 1) & (uc.pc = 4 => (uc.memory_data)(0) = 2) & (uc.pc = 5 => (uc.memory_data)(0) = 0) & (uc.pc = 6 => (uc.memory_data)(0) = 1) & (uc.pc = 7 => (uc.memory_data)(0) = 1) & (uc.pc = 8 => (uc.memory_data)(0) = 2) & (uc.pc = 9 => (uc.memory_data)(0) = 2) VARIANT uc.end-uc.pc END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(traffic_light_ass2))==(?);
  Inherited_List_Constants(Implementation(traffic_light_ass2))==(?);
  List_Constants(Implementation(traffic_light_ass2))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Implementation(traffic_light_ass2),COLOR)==({green,yellow,red});
  Context_List_Enumerated(Implementation(traffic_light_ass2))==(COLOR);
  Context_List_Defered(Implementation(traffic_light_ass2))==(?);
  Context_List_Sets(Implementation(traffic_light_ass2))==(COLOR);
  List_Own_Enumerated(Implementation(traffic_light_ass2))==(?);
  List_Valuable_Sets(Implementation(traffic_light_ass2))==(?);
  Inherited_List_Enumerated(Implementation(traffic_light_ass2))==(?);
  Inherited_List_Defered(Implementation(traffic_light_ass2))==(?);
  Inherited_List_Sets(Implementation(traffic_light_ass2))==(?);
  List_Enumerated(Implementation(traffic_light_ass2))==(?);
  List_Defered(Implementation(traffic_light_ass2))==(?);
  List_Sets(Implementation(traffic_light_ass2))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(traffic_light_ass2))==(?);
  Expanded_List_HiddenConstants(Implementation(traffic_light_ass2))==(?);
  List_HiddenConstants(Implementation(traffic_light_ass2))==(?);
  External_List_HiddenConstants(Implementation(traffic_light_ass2))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(traffic_light_ass2))==(btrue);
  Context_List_Properties(Implementation(traffic_light_ass2))==(not(COLOR = {}) & color_refine : COLOR --> NATURAL & color_refine = {green|->0,yellow|->1,red|->2});
  Inherited_List_Properties(Implementation(traffic_light_ass2))==(btrue);
  List_Properties(Implementation(traffic_light_ass2))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(traffic_light_ass2))==(aa : aa);
  List_Values(Implementation(traffic_light_ass2))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(traffic_light_ass2),Machine(data_refinement))==(?);
  Seen_Context_List_Enumerated(Implementation(traffic_light_ass2))==(COLOR);
  Seen_Context_List_Invariant(Implementation(traffic_light_ass2))==(btrue);
  Seen_Context_List_Assertions(Implementation(traffic_light_ass2))==(!c.(c : COLOR => c = green or c = yellow or c = red));
  Seen_Context_List_Properties(Implementation(traffic_light_ass2))==(not(COLOR = {}));
  Seen_List_Constraints(Implementation(traffic_light_ass2))==(btrue);
  Seen_List_Operations(Implementation(traffic_light_ass2),Machine(data_refinement))==(?);
  Seen_Expanded_List_Invariant(Implementation(traffic_light_ass2),Machine(data_refinement))==(btrue);
  Set_Definition(Implementation(traffic_light_ass2),COLOR)==({green,yellow,red});
  Seen_Internal_List_Operations(Implementation(traffic_light_ass2),Machine(definitions))==(?);
  Seen_List_Operations(Implementation(traffic_light_ass2),Machine(definitions))==(?);
  Seen_Expanded_List_Invariant(Implementation(traffic_light_ass2),Machine(definitions))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(traffic_light_ass2),Machine(Microcontroller))==(init_data,init,push,pop_1,pop_2,get_data,get_pc,set_end,get_end,set_w,get_w,nop,goto,iszero,isequal,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(traffic_light_ass2))==(Type(advance) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX IS
  List_Constants(Implementation(traffic_light_ass2),Machine(data_refinement))==(color_refine);
  List_Constants_Env(Implementation(traffic_light_ass2),Machine(data_refinement))==(Type(color_refine) == Cst(SetOf(etype(COLOR,0,2)*btype(INTEGER,?,?))));
  List_Constants_Env(Implementation(traffic_light_ass2),Machine(definitions))==(Type(green) == Cst(etype(COLOR,0,2));Type(yellow) == Cst(etype(COLOR,0,2));Type(red) == Cst(etype(COLOR,0,2)));
  Enumerate_Definition(Implementation(traffic_light_ass2),Machine(definitions),COLOR)==({green,yellow,red})
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(traffic_light_ass2)) == (? | ? | ? | ? | advance | ? | seen(Machine(definitions)),seen(Machine(data_refinement)),imported(Machine(uc.Microcontroller)) | ? | traffic_light_ass2);
  List_Of_HiddenCst_Ids(Implementation(traffic_light_ass2)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(traffic_light_ass2)) == (?);
  List_Of_VisibleVar_Ids(Implementation(traffic_light_ass2)) == (? | ucmemory_data,ucstack,ucw,ucpc,ucend);
  List_Of_Ids_SeenBNU(Implementation(traffic_light_ass2)) == (? : ?);
  List_Of_Ids(Machine(Microcontroller)) == (? | ? | ? | ? | init_data,init,push,pop_1,pop_2,get_data,get_pc,set_end,get_end,set_w,get_w,nop,goto,iszero,isequal,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div | ? | ? | ? | Microcontroller);
  List_Of_HiddenCst_Ids(Machine(Microcontroller)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Microcontroller)) == (?);
  List_Of_VisibleVar_Ids(Machine(Microcontroller)) == (end,pc,w,stack,memory_data | ?);
  List_Of_Ids_SeenBNU(Machine(Microcontroller)) == (? : ?);
  List_Of_Ids(Machine(data_refinement)) == (color_refine | ? | ? | ? | ? | ? | seen(Machine(definitions)) | ? | data_refinement);
  List_Of_HiddenCst_Ids(Machine(data_refinement)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(data_refinement)) == (color_refine);
  List_Of_VisibleVar_Ids(Machine(data_refinement)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(data_refinement)) == (? : ?);
  List_Of_Ids(Machine(definitions)) == (COLOR,green,yellow,red | ? | ? | ? | ? | ? | ? | ? | definitions);
  List_Of_HiddenCst_Ids(Machine(definitions)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(definitions)) == (?);
  List_Of_VisibleVar_Ids(Machine(definitions)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(definitions)) == (? : ?)
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
  List_Local_Operations(Implementation(traffic_light_ass2))==(?)
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
  TypingPredicate(Implementation(traffic_light_ass2))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(traffic_light_ass2),Machine(uc.Microcontroller))==(?);
  ImportedVisVariablesList(Implementation(traffic_light_ass2),Machine(uc.Microcontroller))==(uc.memory_data,uc.stack,uc.w,uc.pc,uc.end)
END
&
THEORY ListLocalOpInvariantX END
)
