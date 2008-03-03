Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(traffic_light_ass))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(traffic_light_ass))==(Machine(traffic_light));
  Level(Implementation(traffic_light_ass))==(2);
  Upper_Level(Implementation(traffic_light_ass))==(Refinement(traffic_light_data_refinement))
END
&
THEORY LoadedStructureX IS
  Implementation(traffic_light_ass)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(traffic_light_ass))==(definitions,data_refinement)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(traffic_light_ass))==(uc.Microcontroller);
  Inherited_List_Includes(Implementation(traffic_light_ass))==(uc.Microcontroller)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(traffic_light_ass))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(traffic_light_ass))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(traffic_light_ass))==(?);
  Context_List_Variables(Implementation(traffic_light_ass))==(?);
  Abstract_List_Variables(Implementation(traffic_light_ass))==(counter,color);
  Local_List_Variables(Implementation(traffic_light_ass))==(?);
  List_Variables(Implementation(traffic_light_ass))==(?);
  External_List_Variables(Implementation(traffic_light_ass))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(traffic_light_ass))==(?);
  Abstract_List_VisibleVariables(Implementation(traffic_light_ass))==(?);
  External_List_VisibleVariables(Implementation(traffic_light_ass))==(uc.end,uc.pc,uc.memory_data);
  Expanded_List_VisibleVariables(Implementation(traffic_light_ass))==(ucend,ucpc,ucmemory_data);
  List_VisibleVariables(Implementation(traffic_light_ass))==(?);
  Internal_List_VisibleVariables(Implementation(traffic_light_ass))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(traffic_light_ass))==(btrue);
  Abstract_List_Invariant(Implementation(traffic_light_ass))==(counter : NATURAL & counter : 0..2 & counter = color_refine(color) & color : COLOR);
  Expanded_List_Invariant(Implementation(traffic_light_ass))==(ucmemory_data : NATURAL --> NATURAL & ucpc : NATURAL & ucend : NATURAL & ucpc<=ucend);
  Context_List_Invariant(Implementation(traffic_light_ass))==(btrue);
  List_Invariant(Implementation(traffic_light_ass))==(ucmemory_data(0) : 0..2 & ucmemory_data(0) = counter)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(traffic_light_ass))==(btrue);
  Expanded_List_Assertions(Implementation(traffic_light_ass))==(btrue);
  Context_List_Assertions(Implementation(traffic_light_ass))==(!c.(c : COLOR => c = green or c = yellow or c = red) & color_refine(green) = 0 & color_refine(yellow) = 1 & color_refine(red) = 2);
  List_Assertions(Implementation(traffic_light_ass))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(traffic_light_ass))==(@(memory_data$0).(memory_data$0 : NATURAL --> NATURAL ==> ucmemory_data:=memory_data$0) || ucpc:=0 || ucend:=0;(0 : NATURAL & 0 : NATURAL & dom(ucmemory_data<+{0|->0}) = NATURAL | ucmemory_data:=ucmemory_data<+{0|->0}));
  Context_List_Initialisation(Implementation(traffic_light_ass))==(skip);
  List_Initialisation(Implementation(traffic_light_ass))==((uc.init_data)(0,0))
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(traffic_light_ass))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(traffic_light_ass),Machine(uc.Microcontroller))==(?);
  List_Instanciated_Parameters(Implementation(traffic_light_ass),Machine(definitions))==(?);
  List_Instanciated_Parameters(Implementation(traffic_light_ass),Machine(data_refinement))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(traffic_light_ass),Machine(uc.Microcontroller))==(btrue);
  List_Constraints(Implementation(traffic_light_ass))==(btrue);
  List_Context_Constraints(Implementation(traffic_light_ass))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(traffic_light_ass))==(advance);
  List_Operations(Implementation(traffic_light_ass))==(advance)
END
&
THEORY ListInputX IS
  List_Input(Implementation(traffic_light_ass),advance)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(traffic_light_ass),advance)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(traffic_light_ass),advance)==(advance)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(traffic_light_ass),advance)==(btrue);
  List_Precondition(Implementation(traffic_light_ass),advance)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(traffic_light_ass),advance)==(btrue | (0 : NATURAL & 11 : NATURAL & 0<=11 | ucpc,ucend:=0,11);(ucpc = 0 ==> (0 : NATURAL & 0 : NATURAL & (ucmemory_data(0) = 0 => ucpc+1<=ucend) & (ucmemory_data(0)/=0 => ucpc+2<=ucend) | ucmemory_data(0) = 0 ==> ucpc:=ucpc+1 [] not(ucmemory_data(0) = 0) ==> ucpc:=ucpc+2) [] not(ucpc = 0) ==> skip);(ucpc = 1 ==> (5 : NATURAL & 5<=ucend | ucpc:=5) [] not(ucpc = 1) ==> skip);(ucpc = 2 ==> (0 : NATURAL & 1 : NATURAL & (ucmemory_data(0) = 1 => ucpc+1<=ucend) & (ucmemory_data(0)/=1 => ucpc+2<=ucend) | ucmemory_data(0) = 1 ==> ucpc:=ucpc+1 [] not(ucmemory_data(0) = 1) ==> ucpc:=ucpc+2) [] not(ucpc = 2) ==> skip);(ucpc = 3 ==> (7 : NATURAL & 7<=ucend | ucpc:=7) [] not(ucpc = 3) ==> skip);(ucpc = 4 ==> (9 : NATURAL & 9<=ucend | ucpc:=9) [] not(ucpc = 4) ==> skip);(ucpc = 5 ==> (0 : NATURAL & 1 : NATURAL & ucpc+1<=ucend | ucmemory_data,ucpc:=ucmemory_data<+{0|->1},ucpc+1) [] not(ucpc = 5) ==> skip);(ucpc = 6 ==> (10 : NATURAL & 10<=ucend | ucpc:=10) [] not(ucpc = 6) ==> skip);(ucpc = 7 ==> (0 : NATURAL & 2 : NATURAL & ucpc+1<=ucend | ucmemory_data,ucpc:=ucmemory_data<+{0|->2},ucpc+1) [] not(ucpc = 7) ==> skip);(ucpc = 8 ==> (10 : NATURAL & 10<=ucend | ucpc:=10) [] not(ucpc = 8) ==> skip);(ucpc = 9 ==> (0 : NATURAL & 0 : NATURAL & ucpc+1<=ucend | ucmemory_data,ucpc:=ucmemory_data<+{0|->0},ucpc+1) [] not(ucpc = 9) ==> skip);(ucpc/=10 ==> (-1 : INT & 0 : INT & 1 : INT & -1 : NATURAL & -1<=ucend | ucpc:= -1) [] not(ucpc/=10) ==> skip));
  List_Substitution(Implementation(traffic_light_ass),advance)==((uc.init)(0,11);IF uc.pc = 0 THEN (uc.isequal)(0,0) END;IF uc.pc = 1 THEN (uc.goto)(5) END;IF uc.pc = 2 THEN (uc.isequal)(0,1) END;IF uc.pc = 3 THEN (uc.goto)(7) END;IF uc.pc = 4 THEN (uc.goto)(9) END;IF uc.pc = 5 THEN (uc.set_data)(0,1) END;IF uc.pc = 6 THEN (uc.goto)(10) END;IF uc.pc = 7 THEN (uc.set_data)(0,2) END;IF uc.pc = 8 THEN (uc.goto)(10) END;IF uc.pc = 9 THEN (uc.set_data)(0,0) END;IF uc.pc/=10 THEN (uc.goto)(-1) END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(traffic_light_ass))==(?);
  Inherited_List_Constants(Implementation(traffic_light_ass))==(?);
  List_Constants(Implementation(traffic_light_ass))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Implementation(traffic_light_ass),COLOR)==({green,yellow,red});
  Context_List_Enumerated(Implementation(traffic_light_ass))==(COLOR);
  Context_List_Defered(Implementation(traffic_light_ass))==(?);
  Context_List_Sets(Implementation(traffic_light_ass))==(COLOR);
  List_Own_Enumerated(Implementation(traffic_light_ass))==(?);
  List_Valuable_Sets(Implementation(traffic_light_ass))==(?);
  Inherited_List_Enumerated(Implementation(traffic_light_ass))==(?);
  Inherited_List_Defered(Implementation(traffic_light_ass))==(?);
  Inherited_List_Sets(Implementation(traffic_light_ass))==(?);
  List_Enumerated(Implementation(traffic_light_ass))==(?);
  List_Defered(Implementation(traffic_light_ass))==(?);
  List_Sets(Implementation(traffic_light_ass))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(traffic_light_ass))==(?);
  Expanded_List_HiddenConstants(Implementation(traffic_light_ass))==(?);
  List_HiddenConstants(Implementation(traffic_light_ass))==(?);
  External_List_HiddenConstants(Implementation(traffic_light_ass))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(traffic_light_ass))==(btrue);
  Context_List_Properties(Implementation(traffic_light_ass))==(not(COLOR = {}) & color_refine : COLOR --> NATURAL & color_refine = {green|->0,yellow|->1,red|->2});
  Inherited_List_Properties(Implementation(traffic_light_ass))==(btrue);
  List_Properties(Implementation(traffic_light_ass))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(traffic_light_ass))==(aa : aa);
  List_Values(Implementation(traffic_light_ass))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(traffic_light_ass),Machine(data_refinement))==(?);
  Seen_Context_List_Enumerated(Implementation(traffic_light_ass))==(COLOR);
  Seen_Context_List_Invariant(Implementation(traffic_light_ass))==(btrue);
  Seen_Context_List_Assertions(Implementation(traffic_light_ass))==(!c.(c : COLOR => c = green or c = yellow or c = red));
  Seen_Context_List_Properties(Implementation(traffic_light_ass))==(not(COLOR = {}));
  Seen_List_Constraints(Implementation(traffic_light_ass))==(btrue);
  Seen_List_Operations(Implementation(traffic_light_ass),Machine(data_refinement))==(?);
  Seen_Expanded_List_Invariant(Implementation(traffic_light_ass),Machine(data_refinement))==(btrue);
  Set_Definition(Implementation(traffic_light_ass),COLOR)==({green,yellow,red});
  Seen_Internal_List_Operations(Implementation(traffic_light_ass),Machine(definitions))==(?);
  Seen_List_Operations(Implementation(traffic_light_ass),Machine(definitions))==(?);
  Seen_Expanded_List_Invariant(Implementation(traffic_light_ass),Machine(definitions))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(traffic_light_ass),Machine(Microcontroller))==(init_data,init,goto,isequal,copy,set_data,inc)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(traffic_light_ass))==(Type(advance) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX IS
  List_Constants(Implementation(traffic_light_ass),Machine(data_refinement))==(color_refine);
  List_Constants_Env(Implementation(traffic_light_ass),Machine(data_refinement))==(Type(color_refine) == Cst(SetOf(etype(COLOR,0,2)*btype(INTEGER,?,?))));
  List_Constants_Env(Implementation(traffic_light_ass),Machine(definitions))==(Type(green) == Cst(etype(COLOR,0,2));Type(yellow) == Cst(etype(COLOR,0,2));Type(red) == Cst(etype(COLOR,0,2)));
  Enumerate_Definition(Implementation(traffic_light_ass),Machine(definitions),COLOR)==({green,yellow,red})
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(traffic_light_ass)) == (? | ? | ? | ? | advance | ? | seen(Machine(definitions)),seen(Machine(data_refinement)),imported(Machine(uc.Microcontroller)) | ? | traffic_light_ass);
  List_Of_HiddenCst_Ids(Implementation(traffic_light_ass)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(traffic_light_ass)) == (?);
  List_Of_VisibleVar_Ids(Implementation(traffic_light_ass)) == (? | ucmemory_data,ucpc,ucend);
  List_Of_Ids_SeenBNU(Implementation(traffic_light_ass)) == (? : ?);
  List_Of_Ids(Machine(Microcontroller)) == (? | ? | ? | ? | init_data,init,goto,isequal,copy,set_data,inc | ? | ? | ? | Microcontroller);
  List_Of_HiddenCst_Ids(Machine(Microcontroller)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Microcontroller)) == (?);
  List_Of_VisibleVar_Ids(Machine(Microcontroller)) == (end,pc,memory_data | ?);
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
  List_Local_Operations(Implementation(traffic_light_ass))==(?)
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
  TypingPredicate(Implementation(traffic_light_ass))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(traffic_light_ass),Machine(uc.Microcontroller))==(?);
  ImportedVisVariablesList(Implementation(traffic_light_ass),Machine(uc.Microcontroller))==(uc.memory_data,uc.pc,uc.end)
END
&
THEORY ListLocalOpInvariantX END
)
