Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(traffic_light_asm))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(traffic_light_asm))==(Machine(traffic_light));
  Level(Implementation(traffic_light_asm))==(1);
  Upper_Level(Implementation(traffic_light_asm))==(Machine(traffic_light))
END
&
THEORY LoadedStructureX IS
  Implementation(traffic_light_asm)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(traffic_light_asm))==(definitions)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(traffic_light_asm))==(micro.Microcontroller);
  Inherited_List_Includes(Implementation(traffic_light_asm))==(micro.Microcontroller)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(traffic_light_asm))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(traffic_light_asm))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(traffic_light_asm))==(?);
  Context_List_Variables(Implementation(traffic_light_asm))==(?);
  Abstract_List_Variables(Implementation(traffic_light_asm))==(state);
  Local_List_Variables(Implementation(traffic_light_asm))==(?);
  List_Variables(Implementation(traffic_light_asm))==(microend,micropc,microw,microstack,micromemory_data);
  External_List_Variables(Implementation(traffic_light_asm))==(micro.end,micro.pc,micro.w,micro.stack,micro.memory_data)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(traffic_light_asm))==(?);
  Abstract_List_VisibleVariables(Implementation(traffic_light_asm))==(?);
  External_List_VisibleVariables(Implementation(traffic_light_asm))==(?);
  Expanded_List_VisibleVariables(Implementation(traffic_light_asm))==(?);
  List_VisibleVariables(Implementation(traffic_light_asm))==(?);
  Internal_List_VisibleVariables(Implementation(traffic_light_asm))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(traffic_light_asm))==(btrue);
  Abstract_List_Invariant(Implementation(traffic_light_asm))==(state : 1..3);
  Expanded_List_Invariant(Implementation(traffic_light_asm))==(micromemory_data : NATURAL --> NATURAL & microstack : 1..8 +-> NATURAL & card(microstack)<=8 & microw : NATURAL & micropc : NATURAL & microend : NATURAL & micropc<=microend);
  Context_List_Invariant(Implementation(traffic_light_asm))==(btrue);
  List_Invariant(Implementation(traffic_light_asm))==(micromemory_data(0) = state)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(traffic_light_asm))==(btrue);
  Expanded_List_Assertions(Implementation(traffic_light_asm))==(btrue);
  Context_List_Assertions(Implementation(traffic_light_asm))==(btrue);
  List_Assertions(Implementation(traffic_light_asm))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(traffic_light_asm))==(@(memory_data$0).(memory_data$0 : NATURAL --> NATURAL ==> micromemory_data:=memory_data$0) || microstack:={1|->0,2|->0,3|->0,4|->0,5|->0,6|->0,7|->0,8|->0} || microw:=0 || micropc:=0 || microend:=0;(0 : NATURAL & 3 : NATURAL & dom(micromemory_data<+{0|->3}) = NATURAL | micromemory_data:=micromemory_data<+{0|->3}));
  Context_List_Initialisation(Implementation(traffic_light_asm))==(skip);
  List_Initialisation(Implementation(traffic_light_asm))==((micro.init_data)(0,3))
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(traffic_light_asm))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(traffic_light_asm),Machine(micro.Microcontroller))==(?);
  List_Instanciated_Parameters(Implementation(traffic_light_asm),Machine(definitions))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(traffic_light_asm),Machine(micro.Microcontroller))==(btrue);
  List_Constraints(Implementation(traffic_light_asm))==(btrue);
  List_Context_Constraints(Implementation(traffic_light_asm))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(traffic_light_asm))==(advance);
  List_Operations(Implementation(traffic_light_asm))==(advance)
END
&
THEORY ListInputX IS
  List_Input(Implementation(traffic_light_asm),advance)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(traffic_light_asm),advance)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(traffic_light_asm),advance)==(advance)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(traffic_light_asm),advance)==(btrue);
  List_Precondition(Implementation(traffic_light_asm),advance)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(traffic_light_asm),advance)==(btrue | @local_pc.((0 : NATURAL & 5 : NATURAL & 0<=5 | micropc,microend:=0,5);(0 : NATURAL & 1 : NATURAL & micropc+2<microend | micromemory_data(0) = 1 ==> micropc:=micropc+1 [] not(micromemory_data(0) = 1) ==> micropc:=micropc+2);(btrue | local_pc:=micropc);(local_pc = 1 ==> ((4 : NATURAL & 4<=microend | micropc:=4);(btrue | local_pc:=micropc)) [] not(local_pc = 1) ==> skip);(local_pc = 2 ==> ((0 : NATURAL & micromemory_data(0)-1 : NATURAL & micropc+1 : NATURAL & micropc+1<=microend & dom(micromemory_data<+{0|->micromemory_data(0)-1}) = NATURAL | micromemory_data,micropc:=micromemory_data<+{0|->micromemory_data(0)-1},micropc+1);(btrue | local_pc:=micropc)) [] not(local_pc = 2) ==> skip);(local_pc = 3 ==> ((5 : NATURAL & 5<=microend | micropc:=5);(btrue | local_pc:=micropc)) [] not(local_pc = 3) ==> skip);(local_pc = 4 ==> ((0 : NATURAL & 3 : NATURAL & micropc+1<=microend | micromemory_data,micropc:=micromemory_data<+{0|->3},micropc+1);(btrue | local_pc:=micropc)) [] not(local_pc = 4) ==> skip);(local_pc/=5 ==> (-1 : INT & 0 : INT & 1 : INT & -1 : NATURAL & -1<=microend | micropc:= -1) [] not(local_pc/=5) ==> skip)));
  List_Substitution(Implementation(traffic_light_asm),advance)==(VAR local_pc IN (micro.init)(0,5);(micro.isequal)(0,1);local_pc <-- micro.get_pc;IF local_pc = 1 THEN (micro.goto)(4);local_pc <-- micro.get_pc END;IF local_pc = 2 THEN (micro.dec)(0);local_pc <-- micro.get_pc END;IF local_pc = 3 THEN (micro.goto)(5);local_pc <-- micro.get_pc END;IF local_pc = 4 THEN (micro.set_data)(0,3);local_pc <-- micro.get_pc END;IF local_pc/=5 THEN (micro.goto)(-1) END END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(traffic_light_asm))==(?);
  Inherited_List_Constants(Implementation(traffic_light_asm))==(?);
  List_Constants(Implementation(traffic_light_asm))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Implementation(traffic_light_asm))==(?);
  Context_List_Defered(Implementation(traffic_light_asm))==(?);
  Context_List_Sets(Implementation(traffic_light_asm))==(?);
  List_Own_Enumerated(Implementation(traffic_light_asm))==(?);
  List_Valuable_Sets(Implementation(traffic_light_asm))==(?);
  Inherited_List_Enumerated(Implementation(traffic_light_asm))==(?);
  Inherited_List_Defered(Implementation(traffic_light_asm))==(?);
  Inherited_List_Sets(Implementation(traffic_light_asm))==(?);
  List_Enumerated(Implementation(traffic_light_asm))==(?);
  List_Defered(Implementation(traffic_light_asm))==(?);
  List_Sets(Implementation(traffic_light_asm))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(traffic_light_asm))==(?);
  Expanded_List_HiddenConstants(Implementation(traffic_light_asm))==(?);
  List_HiddenConstants(Implementation(traffic_light_asm))==(?);
  External_List_HiddenConstants(Implementation(traffic_light_asm))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(traffic_light_asm))==(btrue);
  Context_List_Properties(Implementation(traffic_light_asm))==(btrue);
  Inherited_List_Properties(Implementation(traffic_light_asm))==(btrue);
  List_Properties(Implementation(traffic_light_asm))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(traffic_light_asm))==(aa : aa);
  List_Values(Implementation(traffic_light_asm))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(traffic_light_asm),Machine(definitions))==(?);
  Seen_Context_List_Enumerated(Implementation(traffic_light_asm))==(?);
  Seen_Context_List_Invariant(Implementation(traffic_light_asm))==(btrue);
  Seen_Context_List_Assertions(Implementation(traffic_light_asm))==(btrue);
  Seen_Context_List_Properties(Implementation(traffic_light_asm))==(btrue);
  Seen_List_Constraints(Implementation(traffic_light_asm))==(btrue);
  Seen_List_Operations(Implementation(traffic_light_asm),Machine(definitions))==(?);
  Seen_Expanded_List_Invariant(Implementation(traffic_light_asm),Machine(definitions))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(traffic_light_asm),Machine(Microcontroller))==(init_data,init,push,pop_1,pop_2,get_data,get_pc,set_end,get_end,set_w,get_w,nop,goto,iszero,isequal,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(traffic_light_asm))==(Type(advance) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(traffic_light_asm)) == (? | ? | ? | micromemory_data,microstack,microw,micropc,microend | advance | ? | seen(Machine(definitions)),imported(Machine(micro.Microcontroller)) | ? | traffic_light_asm);
  List_Of_HiddenCst_Ids(Implementation(traffic_light_asm)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(traffic_light_asm)) == (?);
  List_Of_VisibleVar_Ids(Implementation(traffic_light_asm)) == (? | ?);
  List_Of_Ids_SeenBNU(Implementation(traffic_light_asm)) == (? : ?);
  List_Of_Ids(Machine(Microcontroller)) == (? | ? | end,pc,w,stack,memory_data | ? | init_data,init,push,pop_1,pop_2,get_data,get_pc,set_end,get_end,set_w,get_w,nop,goto,iszero,isequal,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div | ? | ? | ? | Microcontroller);
  List_Of_HiddenCst_Ids(Machine(Microcontroller)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Microcontroller)) == (?);
  List_Of_VisibleVar_Ids(Machine(Microcontroller)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Microcontroller)) == (? : ?);
  List_Of_Ids(Machine(definitions)) == (? | ? | ? | ? | ? | ? | ? | ? | definitions);
  List_Of_HiddenCst_Ids(Machine(definitions)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(definitions)) == (?);
  List_Of_VisibleVar_Ids(Machine(definitions)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(definitions)) == (? : ?)
END
&
THEORY VariablesLocEnvX IS
  Variables_Loc(Implementation(traffic_light_asm),advance, 1) == (Type(local_pc) == Lvl(btype(INTEGER,?,?)))
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
  List_Local_Operations(Implementation(traffic_light_asm))==(?)
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
  TypingPredicate(Implementation(traffic_light_asm))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(traffic_light_asm),Machine(micro.Microcontroller))==(micro.memory_data,micro.stack,micro.w,micro.pc,micro.end);
  ImportedVisVariablesList(Implementation(traffic_light_asm),Machine(micro.Microcontroller))==(?)
END
&
THEORY ListLocalOpInvariantX END
)
