Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(traffic_light_ass2))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(traffic_light_ass2))==(Machine(traffic_light));
  Level(Implementation(traffic_light_ass2))==(1);
  Upper_Level(Implementation(traffic_light_ass2))==(Machine(traffic_light))
END
&
THEORY LoadedStructureX IS
  Implementation(traffic_light_ass2)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(traffic_light_ass2))==(?)
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
  Abstract_List_Variables(Implementation(traffic_light_ass2))==(state);
  Local_List_Variables(Implementation(traffic_light_ass2))==(?);
  List_Variables(Implementation(traffic_light_ass2))==(ucend,ucpc,ucw,ucstack,ucmemory_data);
  External_List_Variables(Implementation(traffic_light_ass2))==(uc.end,uc.pc,uc.w,uc.stack,uc.memory_data)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(traffic_light_ass2))==(?);
  Abstract_List_VisibleVariables(Implementation(traffic_light_ass2))==(?);
  External_List_VisibleVariables(Implementation(traffic_light_ass2))==(?);
  Expanded_List_VisibleVariables(Implementation(traffic_light_ass2))==(?);
  List_VisibleVariables(Implementation(traffic_light_ass2))==(?);
  Internal_List_VisibleVariables(Implementation(traffic_light_ass2))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(traffic_light_ass2))==(btrue);
  Abstract_List_Invariant(Implementation(traffic_light_ass2))==(state : 0..2);
  Expanded_List_Invariant(Implementation(traffic_light_ass2))==(ucmemory_data : NATURAL --> NATURAL & ucstack : 1..8 +-> NATURAL & card(ucstack)<=8 & ucw : NATURAL & ucpc : NATURAL & ucend : NATURAL & ucpc<=ucend);
  Context_List_Invariant(Implementation(traffic_light_ass2))==(btrue);
  List_Invariant(Implementation(traffic_light_ass2))==(ucmemory_data(0) = state)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(traffic_light_ass2))==(btrue);
  Expanded_List_Assertions(Implementation(traffic_light_ass2))==(btrue);
  Context_List_Assertions(Implementation(traffic_light_ass2))==(btrue);
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
  List_Instanciated_Parameters(Implementation(traffic_light_ass2),Machine(uc.Microcontroller))==(?)
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
  Expanded_List_Substitution(Implementation(traffic_light_ass2),advance)==(btrue | @(pc,end).(pc:=0;end:=4;(pc : NATURAL & end : NATURAL & pc<=end | ucpc,ucend:=pc,end);WHILE pc<end DO not(pc = 3) & not(pc = 2) & not(pc = 1) & pc = 0 ==> (0 : NATURAL & ucmemory_data(0)+1 : NATURAL & ucpc+1 : NATURAL & ucpc+1<=ucend & dom(ucmemory_data<+{0|->ucmemory_data(0)+1}) = NATURAL | ucmemory_data,ucpc:=ucmemory_data<+{0|->ucmemory_data(0)+1},ucpc+1) [] not(pc = 0) & not(pc = 3) & not(pc = 2) & pc = 1 ==> (0 : NATURAL & 3 : NATURAL & ucpc+2<ucend | ucmemory_data(0) = 3 ==> ucpc:=ucpc+1 [] not(ucmemory_data(0) = 3) ==> ucpc:=ucpc+2) [] not(pc = 0) & not(pc = 1) & not(pc = 3) & pc = 2 ==> (0 : NATURAL & 0 : NATURAL & ucpc+1<=ucend | ucmemory_data,ucpc:=ucmemory_data<+{0|->0},ucpc+1) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & pc = 3 ==> (ucpc<ucend | ucpc:=ucpc+1) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & not(pc = 3) ==> skip;(btrue | pc:=ucpc) INVARIANT pc>=0 & pc<=4 & pc = ucpc & end = ucend & ucmemory_data : NATURAL --> NATURAL & (pc = 0 => ucmemory_data(0) = state) & (pc = 1 => ucmemory_data(0) = state+1) & (pc = 2 => ucmemory_data(0) = state+1 & ucmemory_data(0) = 3) & (pc = 3 => ucmemory_data(0) = (state+1) mod 3) & (pc = 4 => ucmemory_data(0) = (state+1) mod 3) VARIANT end-pc END));
  List_Substitution(Implementation(traffic_light_ass2),advance)==(VAR pc,end IN pc:=0;end:=4;(uc.init)(pc,end);WHILE pc<end DO BEGIN CASE pc OF EITHER 0 THEN (uc.inc)(0) OR 1 THEN (uc.isequal)(0,3) OR 2 THEN (uc.set_data)(0,0) OR 3 THEN uc.nop END END;pc <-- uc.get_pc END INVARIANT pc>=0 & pc<=4 & pc = uc.pc & end = uc.end & uc.memory_data : NATURAL --> NATURAL & (pc = 0 => (uc.memory_data)(0) = state) & (pc = 1 => (uc.memory_data)(0) = state+1) & (pc = 2 => (uc.memory_data)(0) = state+1 & (uc.memory_data)(0) = 3) & (pc = 3 => (uc.memory_data)(0) = (state+1) mod 3) & (pc = 4 => (uc.memory_data)(0) = (state+1) mod 3) VARIANT end-pc END END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(traffic_light_ass2))==(?);
  Inherited_List_Constants(Implementation(traffic_light_ass2))==(?);
  List_Constants(Implementation(traffic_light_ass2))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Implementation(traffic_light_ass2))==(?);
  Context_List_Defered(Implementation(traffic_light_ass2))==(?);
  Context_List_Sets(Implementation(traffic_light_ass2))==(?);
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
  Context_List_Properties(Implementation(traffic_light_ass2))==(btrue);
  Inherited_List_Properties(Implementation(traffic_light_ass2))==(btrue);
  List_Properties(Implementation(traffic_light_ass2))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(traffic_light_ass2))==(aa : aa);
  List_Values(Implementation(traffic_light_ass2))==(?)
END
&
THEORY ListSeenInfoX END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(traffic_light_ass2),Machine(Microcontroller))==(init_data,init,push,pop_1,pop_2,get_data,get_pc,set_end,get_end,set_w,get_w,nop,goto,iszero,isequal,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(traffic_light_ass2))==(Type(advance) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(traffic_light_ass2)) == (? | ? | ? | ucmemory_data,ucstack,ucw,ucpc,ucend | advance | ? | imported(Machine(uc.Microcontroller)) | ? | traffic_light_ass2);
  List_Of_HiddenCst_Ids(Implementation(traffic_light_ass2)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(traffic_light_ass2)) == (?);
  List_Of_VisibleVar_Ids(Implementation(traffic_light_ass2)) == (? | ?);
  List_Of_Ids_SeenBNU(Implementation(traffic_light_ass2)) == (? : ?);
  List_Of_Ids(Machine(Microcontroller)) == (? | ? | end,pc,w,stack,memory_data | ? | init_data,init,push,pop_1,pop_2,get_data,get_pc,set_end,get_end,set_w,get_w,nop,goto,iszero,isequal,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div | ? | ? | ? | Microcontroller);
  List_Of_HiddenCst_Ids(Machine(Microcontroller)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Microcontroller)) == (?);
  List_Of_VisibleVar_Ids(Machine(Microcontroller)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Microcontroller)) == (? : ?)
END
&
THEORY VariablesLocEnvX IS
  Variables_Loc(Implementation(traffic_light_ass2),advance, 1) == (Type(pc) == Lvl(btype(INTEGER,?,?));Type(end) == Lvl(btype(INTEGER,?,?)))
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
  ImportedVariablesList(Implementation(traffic_light_ass2),Machine(uc.Microcontroller))==(uc.memory_data,uc.stack,uc.w,uc.pc,uc.end);
  ImportedVisVariablesList(Implementation(traffic_light_ass2),Machine(uc.Microcontroller))==(?)
END
&
THEORY ListLocalOpInvariantX END
)
