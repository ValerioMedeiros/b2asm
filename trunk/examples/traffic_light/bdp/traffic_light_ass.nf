Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(traffic_light_ass))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(traffic_light_ass))==(Machine(traffic_light));
  Level(Implementation(traffic_light_ass))==(1);
  Upper_Level(Implementation(traffic_light_ass))==(Machine(traffic_light))
END
&
THEORY LoadedStructureX IS
  Implementation(traffic_light_ass)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(traffic_light_ass))==(?)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(traffic_light_ass))==(micro.Microcontroller);
  Inherited_List_Includes(Implementation(traffic_light_ass))==(micro.Microcontroller)
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
  Abstract_List_Variables(Implementation(traffic_light_ass))==(color);
  Local_List_Variables(Implementation(traffic_light_ass))==(?);
  List_Variables(Implementation(traffic_light_ass))==(microend,micropc,microw,microstack,micromemory_data);
  External_List_Variables(Implementation(traffic_light_ass))==(micro.end,micro.pc,micro.w,micro.stack,micro.memory_data)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(traffic_light_ass))==(?);
  Abstract_List_VisibleVariables(Implementation(traffic_light_ass))==(?);
  External_List_VisibleVariables(Implementation(traffic_light_ass))==(?);
  Expanded_List_VisibleVariables(Implementation(traffic_light_ass))==(?);
  List_VisibleVariables(Implementation(traffic_light_ass))==(?);
  Internal_List_VisibleVariables(Implementation(traffic_light_ass))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(traffic_light_ass))==(btrue);
  Abstract_List_Invariant(Implementation(traffic_light_ass))==(color : COLOR);
  Expanded_List_Invariant(Implementation(traffic_light_ass))==(micromemory_data : NATURAL --> NATURAL & microstack : 1..8 +-> NATURAL & card(microstack)<=8 & microw : NATURAL & micropc : NATURAL & microend : NATURAL & micropc<=microend);
  Context_List_Invariant(Implementation(traffic_light_ass))==(btrue);
  List_Invariant(Implementation(traffic_light_ass))==(micromemory_data(0) = 0 <=> (color = GREEN) & micromemory_data(0) = 1 <=> (color = YELLOW) & micromemory_data(0) = 2 <=> (color = RED) & micromemory_data(0) : 0..2)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(traffic_light_ass))==(btrue);
  Expanded_List_Assertions(Implementation(traffic_light_ass))==(btrue);
  Context_List_Assertions(Implementation(traffic_light_ass))==(btrue);
  List_Assertions(Implementation(traffic_light_ass))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(traffic_light_ass))==(@(memory_data$0).(memory_data$0 : NATURAL --> NATURAL ==> micromemory_data:=memory_data$0) || microstack:={1|->0,2|->0,3|->0,4|->0,5|->0,6|->0,7|->0,8|->0} || microw:=0 || micropc:=0 || microend:=0;(0 : NATURAL & 0 : NATURAL & dom(micromemory_data<+{0|->0}) = NATURAL | micromemory_data:=micromemory_data<+{0|->0}));
  Context_List_Initialisation(Implementation(traffic_light_ass))==(skip);
  List_Initialisation(Implementation(traffic_light_ass))==((micro.init_data)(0,0))
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(traffic_light_ass))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(traffic_light_ass),Machine(micro.Microcontroller))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(traffic_light_ass),Machine(micro.Microcontroller))==(btrue);
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
  Expanded_List_Substitution(Implementation(traffic_light_ass),advance)==(btrue | @local_pc.((0 : NATURAL & 4 : NATURAL & 0<=4 | micropc,microend:=0,4);(0 : NATURAL & micromemory_data(0)+1 : NATURAL & micropc+1 : NATURAL & micropc+1<=microend & dom(micromemory_data<+{0|->micromemory_data(0)+1}) = NATURAL | micromemory_data,micropc:=micromemory_data<+{0|->micromemory_data(0)+1},micropc+1);(0 : NATURAL & 3 : NATURAL & micropc+2<microend | micromemory_data(0) = 3 ==> micropc:=micropc+1 [] not(micromemory_data(0) = 3) ==> micropc:=micropc+2);(btrue | local_pc:=micropc);(local_pc = 2 ==> (0 : NATURAL & 0 : NATURAL & micropc+1<=microend | micromemory_data,micropc:=micromemory_data<+{0|->0},micropc+1) [] not(local_pc = 2) ==> skip)));
  List_Substitution(Implementation(traffic_light_ass),advance)==(VAR local_pc IN (micro.init)(0,4);(micro.inc)(0);(micro.isequal)(0,3);local_pc <-- micro.get_pc;IF local_pc = 2 THEN (micro.set_data)(0,0) END END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(traffic_light_ass))==(?);
  Inherited_List_Constants(Implementation(traffic_light_ass))==(?);
  List_Constants(Implementation(traffic_light_ass))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Implementation(traffic_light_ass),COLOR)==({GREEN,YELLOW,RED});
  Context_List_Enumerated(Implementation(traffic_light_ass))==(?);
  Context_List_Defered(Implementation(traffic_light_ass))==(?);
  Context_List_Sets(Implementation(traffic_light_ass))==(?);
  List_Own_Enumerated(Implementation(traffic_light_ass))==(COLOR);
  List_Valuable_Sets(Implementation(traffic_light_ass))==(?);
  Inherited_List_Enumerated(Implementation(traffic_light_ass))==(COLOR);
  Inherited_List_Defered(Implementation(traffic_light_ass))==(?);
  Inherited_List_Sets(Implementation(traffic_light_ass))==(COLOR);
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
  Abstract_List_Properties(Implementation(traffic_light_ass))==(not(COLOR = {}));
  Context_List_Properties(Implementation(traffic_light_ass))==(btrue);
  Inherited_List_Properties(Implementation(traffic_light_ass))==(btrue);
  List_Properties(Implementation(traffic_light_ass))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(traffic_light_ass))==(aa : aa);
  List_Values(Implementation(traffic_light_ass))==(?)
END
&
THEORY ListSeenInfoX END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(traffic_light_ass),Machine(Microcontroller))==(init_data,init,push,pop_1,pop_2,get_data,get_pc,set_end,get_end,set_w,get_w,nop,goto,iszero,isequal,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(traffic_light_ass))==(Type(advance) == Cst(No_type,No_type));
  Constants(Implementation(traffic_light_ass))==(Type(GREEN) == Cst(etype(COLOR,0,2));Type(YELLOW) == Cst(etype(COLOR,0,2));Type(RED) == Cst(etype(COLOR,0,2)))
END
&
THEORY ListVisibleStaticX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(traffic_light_ass)) == (? | ? | ? | micromemory_data,microstack,microw,micropc,microend | advance | ? | imported(Machine(micro.Microcontroller)) | ? | traffic_light_ass);
  List_Of_HiddenCst_Ids(Implementation(traffic_light_ass)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(traffic_light_ass)) == (?);
  List_Of_VisibleVar_Ids(Implementation(traffic_light_ass)) == (? | ?);
  List_Of_Ids_SeenBNU(Implementation(traffic_light_ass)) == (? : ?);
  List_Of_Ids(Machine(Microcontroller)) == (? | ? | end,pc,w,stack,memory_data | ? | init_data,init,push,pop_1,pop_2,get_data,get_pc,set_end,get_end,set_w,get_w,nop,goto,iszero,isequal,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div | ? | ? | ? | Microcontroller);
  List_Of_HiddenCst_Ids(Machine(Microcontroller)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Microcontroller)) == (?);
  List_Of_VisibleVar_Ids(Machine(Microcontroller)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Microcontroller)) == (? : ?)
END
&
THEORY SetsEnvX IS
  Sets(Implementation(traffic_light_ass)) == (Type(COLOR) == Cst(SetOf(etype(COLOR,0,2))))
END
&
THEORY ConstantsEnvX IS
  Constants(Implementation(traffic_light_ass)) == (Type(RED) == Cst(etype(COLOR,0,2));Type(YELLOW) == Cst(etype(COLOR,0,2));Type(GREEN) == Cst(etype(COLOR,0,2)))
END
&
THEORY VariablesLocEnvX IS
  Variables_Loc(Implementation(traffic_light_ass),advance, 1) == (Type(local_pc) == Lvl(btype(INTEGER,?,?)))
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
  ImportedVariablesList(Implementation(traffic_light_ass),Machine(micro.Microcontroller))==(micro.memory_data,micro.stack,micro.w,micro.pc,micro.end);
  ImportedVisVariablesList(Implementation(traffic_light_ass),Machine(micro.Microcontroller))==(?)
END
&
THEORY ListLocalOpInvariantX END
)
