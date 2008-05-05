Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(assembly))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(assembly))==(Machine(max2));
  Level(Implementation(assembly))==(1);
  Upper_Level(Implementation(assembly))==(Machine(max2))
END
&
THEORY LoadedStructureX IS
  Implementation(assembly)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(assembly))==(?)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(assembly))==(uc.microcontroller);
  Inherited_List_Includes(Implementation(assembly))==(uc.microcontroller)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(assembly))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(assembly))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(assembly))==(?);
  Context_List_Variables(Implementation(assembly))==(?);
  Abstract_List_Variables(Implementation(assembly))==(m,b,a);
  Local_List_Variables(Implementation(assembly))==(?);
  List_Variables(Implementation(assembly))==(ucend,ucpc,ucw,ucmemory_data);
  External_List_Variables(Implementation(assembly))==(uc.end,uc.pc,uc.w,uc.memory_data)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(assembly))==(?);
  Abstract_List_VisibleVariables(Implementation(assembly))==(?);
  External_List_VisibleVariables(Implementation(assembly))==(?);
  Expanded_List_VisibleVariables(Implementation(assembly))==(?);
  List_VisibleVariables(Implementation(assembly))==(?);
  Internal_List_VisibleVariables(Implementation(assembly))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(assembly))==(btrue);
  Abstract_List_Invariant(Implementation(assembly))==(a : INT & b : INT & m : INT & (m = a or m = b) & m>=a & m>=b);
  Expanded_List_Invariant(Implementation(assembly))==(ucmemory_data : NATURAL --> NATURAL & ucw : NATURAL & ucpc : NATURAL & ucend : NATURAL & ucpc<=ucend);
  Context_List_Invariant(Implementation(assembly))==(btrue);
  List_Invariant(Implementation(assembly))==(a = ucmemory_data(0) & b = ucmemory_data(1) & m = ucmemory_data(2))
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(assembly))==(btrue);
  Expanded_List_Assertions(Implementation(assembly))==(btrue);
  Context_List_Assertions(Implementation(assembly))==(btrue);
  List_Assertions(Implementation(assembly))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(assembly))==(@(memory_data$0).(memory_data$0 : NATURAL --> NATURAL ==> ucmemory_data:=memory_data$0) || ucw:=0 || ucpc:=0 || ucend:=0;(0 : NATURAL & 0 : NATURAL & dom(ucmemory_data<+{0|->0}) = NATURAL | ucmemory_data:=ucmemory_data<+{0|->0});(1 : NATURAL & 0 : NATURAL & dom(ucmemory_data<+{1|->0}) = NATURAL | ucmemory_data:=ucmemory_data<+{1|->0});(2 : NATURAL & 0 : NATURAL & dom(ucmemory_data<+{2|->0}) = NATURAL | ucmemory_data:=ucmemory_data<+{2|->0});(6 : NATURAL & ucpc<=6 | ucend:=6));
  Context_List_Initialisation(Implementation(assembly))==(skip);
  List_Initialisation(Implementation(assembly))==(BEGIN (uc.init_data)(0,0);(uc.init_data)(1,0);(uc.init_data)(2,0);(uc.set_end)(6) END)
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(assembly))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(assembly),Machine(uc.microcontroller))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(assembly),Machine(uc.microcontroller))==(btrue);
  List_Constraints(Implementation(assembly))==(btrue);
  List_Context_Constraints(Implementation(assembly))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(assembly))==(run);
  List_Operations(Implementation(assembly))==(run)
END
&
THEORY ListInputX IS
  List_Input(Implementation(assembly),run)==(v)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(assembly),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(assembly),run)==(run(v))
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(assembly),run)==(btrue);
  List_Precondition(Implementation(assembly),run)==(v : INT & v>=0)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(assembly),run)==(v : INT & v>=0 | @(v0,v1).((0 : NATURAL & 6 : NATURAL & 0<=6 | ucpc,ucend:=0,6);(4 : NATURAL & v : NATURAL & ucpc+1 : NATURAL & ucpc+1<=ucend | ucmemory_data,ucpc:=ucmemory_data<+{4|->v},ucpc+1);(0 : NATURAL & 1 : NATURAL & ucpc+1<=ucend & ucpc+1 : NATURAL | ucmemory_data,ucpc:=ucmemory_data<+{1|->ucmemory_data(0)},ucpc+1);(4 : NATURAL & 0 : NATURAL & ucpc+1<=ucend & ucpc+1 : NATURAL | ucmemory_data,ucpc:=ucmemory_data<+{0|->ucmemory_data(4)},ucpc+1);(0 : NATURAL & 1 : NATURAL & (ucmemory_data(0)>ucmemory_data(1) => ucpc+1 : NATURAL & ucpc+1<=ucend) & (ucmemory_data(0)<=ucmemory_data(1) => ucpc+2 : NATURAL & ucpc+2<=ucend) | ucmemory_data(0)>ucmemory_data(1) ==> ucpc:=ucpc+1 [] not(ucmemory_data(0)>ucmemory_data(1)) ==> ucpc:=ucpc+2);(0 : NATURAL | v0:=ucmemory_data(0));(1 : NATURAL | v1:=ucmemory_data(1));(v0>v1 ==> (0 : NATURAL & 2 : NATURAL & ucpc+1<=ucend & ucpc+1 : NATURAL | ucmemory_data,ucpc:=ucmemory_data<+{2|->ucmemory_data(0)},ucpc+1) [] not(v0>v1) ==> (1 : NATURAL & 2 : NATURAL & ucpc+1<=ucend & ucpc+1 : NATURAL | ucmemory_data,ucpc:=ucmemory_data<+{2|->ucmemory_data(1)},ucpc+1))));
  List_Substitution(Implementation(assembly),run)==(VAR v0,v1 IN BEGIN (uc.init)(0,6);(uc.set_data)(4,v);(uc.move)(0,1);(uc.move)(4,0);(uc.testgt)(0,1);v0 <-- (uc.get_data)(0);v1 <-- (uc.get_data)(1);IF v0>v1 THEN (uc.move)(0,2) ELSE (uc.move)(1,2) END END END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(assembly))==(?);
  Inherited_List_Constants(Implementation(assembly))==(?);
  List_Constants(Implementation(assembly))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Implementation(assembly))==(?);
  Context_List_Defered(Implementation(assembly))==(?);
  Context_List_Sets(Implementation(assembly))==(?);
  List_Own_Enumerated(Implementation(assembly))==(?);
  List_Valuable_Sets(Implementation(assembly))==(?);
  Inherited_List_Enumerated(Implementation(assembly))==(?);
  Inherited_List_Defered(Implementation(assembly))==(?);
  Inherited_List_Sets(Implementation(assembly))==(?);
  List_Enumerated(Implementation(assembly))==(?);
  List_Defered(Implementation(assembly))==(?);
  List_Sets(Implementation(assembly))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(assembly))==(?);
  Expanded_List_HiddenConstants(Implementation(assembly))==(?);
  List_HiddenConstants(Implementation(assembly))==(?);
  External_List_HiddenConstants(Implementation(assembly))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(assembly))==(btrue);
  Context_List_Properties(Implementation(assembly))==(btrue);
  Inherited_List_Properties(Implementation(assembly))==(btrue);
  List_Properties(Implementation(assembly))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(assembly))==(aa : aa);
  List_Values(Implementation(assembly))==(?)
END
&
THEORY ListSeenInfoX END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(assembly),Machine(microcontroller))==(init_data,init,get_data,get_pc,set_end,get_end,nop,set_w,get_w,goto,iszero,isnotzero_simple,testgt,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(assembly))==(Type(run) == Cst(No_type,btype(INTEGER,?,?)))
END
&
THEORY ListVisibleStaticX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(assembly)) == (? | ? | ? | ucmemory_data,ucw,ucpc,ucend | run | ? | imported(Machine(uc.microcontroller)) | ? | assembly);
  List_Of_HiddenCst_Ids(Implementation(assembly)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(assembly)) == (?);
  List_Of_VisibleVar_Ids(Implementation(assembly)) == (? | ?);
  List_Of_Ids_SeenBNU(Implementation(assembly)) == (? : ?);
  List_Of_Ids(Machine(microcontroller)) == (? | ? | end,pc,w,memory_data | ? | init_data,init,get_data,get_pc,set_end,get_end,nop,set_w,get_w,goto,iszero,isnotzero_simple,testgt,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div | ? | ? | ? | microcontroller);
  List_Of_HiddenCst_Ids(Machine(microcontroller)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(microcontroller)) == (?);
  List_Of_VisibleVar_Ids(Machine(microcontroller)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(microcontroller)) == (? : ?)
END
&
THEORY VariablesLocEnvX IS
  Variables_Loc(Implementation(assembly),run, 1) == (Type(v0) == Lvl(btype(INTEGER,?,?));Type(v1) == Lvl(btype(INTEGER,?,?)))
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
  List_Local_Operations(Implementation(assembly))==(?)
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
  TypingPredicate(Implementation(assembly))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(assembly),Machine(uc.microcontroller))==(uc.memory_data,uc.w,uc.pc,uc.end);
  ImportedVisVariablesList(Implementation(assembly),Machine(uc.microcontroller))==(?)
END
&
THEORY ListLocalOpInvariantX END
)
