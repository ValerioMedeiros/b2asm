Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(assembly2))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(assembly2))==(Machine(max2));
  Level(Implementation(assembly2))==(1);
  Upper_Level(Implementation(assembly2))==(Machine(max2))
END
&
THEORY LoadedStructureX IS
  Implementation(assembly2)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(assembly2))==(?)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(assembly2))==(uc.microcontroller);
  Inherited_List_Includes(Implementation(assembly2))==(uc.microcontroller)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(assembly2))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(assembly2))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(assembly2))==(?);
  Context_List_Variables(Implementation(assembly2))==(?);
  Abstract_List_Variables(Implementation(assembly2))==(m,b,a);
  Local_List_Variables(Implementation(assembly2))==(?);
  List_Variables(Implementation(assembly2))==(ucend,ucpc,ucw,ucmemory_data);
  External_List_Variables(Implementation(assembly2))==(uc.end,uc.pc,uc.w,uc.memory_data)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(assembly2))==(?);
  Abstract_List_VisibleVariables(Implementation(assembly2))==(?);
  External_List_VisibleVariables(Implementation(assembly2))==(?);
  Expanded_List_VisibleVariables(Implementation(assembly2))==(?);
  List_VisibleVariables(Implementation(assembly2))==(?);
  Internal_List_VisibleVariables(Implementation(assembly2))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(assembly2))==(btrue);
  Abstract_List_Invariant(Implementation(assembly2))==(a : INT & b : INT & m : INT & (m = a or m = b) & m>=a & m>=b);
  Expanded_List_Invariant(Implementation(assembly2))==(ucmemory_data : NATURAL --> NATURAL & ucw : NATURAL & ucpc : NATURAL & ucend : NATURAL & ucpc<=ucend);
  Context_List_Invariant(Implementation(assembly2))==(btrue);
  List_Invariant(Implementation(assembly2))==(a = ucmemory_data(0) & b = ucmemory_data(1) & m = ucmemory_data(2))
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(assembly2))==(btrue);
  Expanded_List_Assertions(Implementation(assembly2))==(btrue);
  Context_List_Assertions(Implementation(assembly2))==(btrue);
  List_Assertions(Implementation(assembly2))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(assembly2))==(@(memory_data$0).(memory_data$0 : NATURAL --> NATURAL ==> ucmemory_data:=memory_data$0) || ucw:=0 || ucpc:=0 || ucend:=0;(0 : NATURAL & 0 : NATURAL & dom(ucmemory_data<+{0|->0}) = NATURAL | ucmemory_data:=ucmemory_data<+{0|->0});(1 : NATURAL & 0 : NATURAL & dom(ucmemory_data<+{1|->0}) = NATURAL | ucmemory_data:=ucmemory_data<+{1|->0});(2 : NATURAL & 0 : NATURAL & dom(ucmemory_data<+{2|->0}) = NATURAL | ucmemory_data:=ucmemory_data<+{2|->0});(0 : NATURAL & ucpc<=0 | ucend:=0));
  Context_List_Initialisation(Implementation(assembly2))==(skip);
  List_Initialisation(Implementation(assembly2))==(BEGIN (uc.init_data)(0,0);(uc.init_data)(1,0);(uc.init_data)(2,0);(uc.set_end)(0) END)
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(assembly2))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(assembly2),Machine(uc.microcontroller))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(assembly2),Machine(uc.microcontroller))==(btrue);
  List_Constraints(Implementation(assembly2))==(btrue);
  List_Context_Constraints(Implementation(assembly2))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(assembly2))==(run);
  List_Operations(Implementation(assembly2))==(run)
END
&
THEORY ListInputX IS
  List_Input(Implementation(assembly2),run)==(v)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(assembly2),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(assembly2),run)==(run(v))
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(assembly2),run)==(btrue);
  List_Precondition(Implementation(assembly2),run)==(v : INT & v>=0)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(assembly2),run)==(v : INT & v>=0 | @(pc,end).(pc:=0;end:=10;(pc : NATURAL & end : NATURAL & pc<=end | ucpc,ucend:=pc,end);WHILE pc<end DO not(pc = 9) & not(pc = 8) & not(pc = 7) & not(pc = 6) & not(pc = 5) & not(pc = 4) & not(pc = 3) & not(pc = 2) & not(pc = 1) & pc = 0 ==> (3 : NATURAL & v : NATURAL & ucpc+1 : NATURAL & ucpc+1<=ucend | ucmemory_data,ucpc:=ucmemory_data<+{3|->v},ucpc+1) [] not(pc = 0) & not(pc = 9) & not(pc = 8) & not(pc = 7) & not(pc = 6) & not(pc = 5) & not(pc = 4) & not(pc = 3) & not(pc = 2) & pc = 1 ==> (0 : NATURAL & 1 : NATURAL & ucpc+1<=ucend & ucpc+1 : NATURAL | ucmemory_data,ucpc:=ucmemory_data<+{1|->ucmemory_data(0)},ucpc+1) [] not(pc = 0) & not(pc = 1) & not(pc = 9) & not(pc = 8) & not(pc = 7) & not(pc = 6) & not(pc = 5) & not(pc = 4) & not(pc = 3) & pc = 2 ==> (3 : NATURAL & 0 : NATURAL & ucpc+1<=ucend & ucpc+1 : NATURAL | ucmemory_data,ucpc:=ucmemory_data<+{0|->ucmemory_data(3)},ucpc+1) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & not(pc = 9) & not(pc = 8) & not(pc = 7) & not(pc = 6) & not(pc = 5) & not(pc = 4) & pc = 3 ==> (0 : NATURAL & 1 : NATURAL & (ucmemory_data(0)>ucmemory_data(1) => ucpc+1 : NATURAL & ucpc+1<=ucend) & (ucmemory_data(0)<=ucmemory_data(1) => ucpc+2 : NATURAL & ucpc+2<=ucend) | ucmemory_data(0)>ucmemory_data(1) ==> ucpc:=ucpc+1 [] not(ucmemory_data(0)>ucmemory_data(1)) ==> ucpc:=ucpc+2) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & not(pc = 3) & not(pc = 9) & not(pc = 8) & not(pc = 7) & not(pc = 6) & not(pc = 5) & pc = 4 ==> (6 : NATURAL & 6<=ucend | ucpc:=6) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & not(pc = 3) & not(pc = 4) & not(pc = 9) & not(pc = 8) & not(pc = 7) & not(pc = 6) & pc = 5 ==> (8 : NATURAL & 8<=ucend | ucpc:=8) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & not(pc = 3) & not(pc = 4) & not(pc = 5) & not(pc = 9) & not(pc = 8) & not(pc = 7) & pc = 6 ==> (0 : NATURAL & 2 : NATURAL & ucpc+1<=ucend & ucpc+1 : NATURAL | ucmemory_data,ucpc:=ucmemory_data<+{2|->ucmemory_data(0)},ucpc+1) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & not(pc = 3) & not(pc = 4) & not(pc = 5) & not(pc = 6) & not(pc = 9) & not(pc = 8) & pc = 7 ==> (9 : NATURAL & 9<=ucend | ucpc:=9) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & not(pc = 3) & not(pc = 4) & not(pc = 5) & not(pc = 6) & not(pc = 7) & not(pc = 9) & pc = 8 ==> (1 : NATURAL & 2 : NATURAL & ucpc+1<=ucend & ucpc+1 : NATURAL | ucmemory_data,ucpc:=ucmemory_data<+{2|->ucmemory_data(1)},ucpc+1) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & not(pc = 3) & not(pc = 4) & not(pc = 5) & not(pc = 6) & not(pc = 7) & not(pc = 8) & pc = 9 ==> (ucpc<ucend | ucpc:=ucpc+1) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & not(pc = 3) & not(pc = 4) & not(pc = 5) & not(pc = 6) & not(pc = 7) & not(pc = 8) & not(pc = 9) ==> skip;(btrue | pc:=ucpc) INVARIANT pc>=0 & pc<=end & pc = ucpc & end = ucend & (pc = 0 => ucmemory_data(0) = a & ucmemory_data(1) = b & ucmemory_data(2) = m) & (pc = 1 => ucmemory_data(0) = a & ucmemory_data(1) = b & ucmemory_data(2) = m & ucmemory_data(3) = v) & (pc = 2 => ucmemory_data(0) = a & ucmemory_data(1) = a & ucmemory_data(2) = m & ucmemory_data(3) = v) & (pc = 3 => ucmemory_data(0) = v & ucmemory_data(1) = a & ucmemory_data(2) = m & ucmemory_data(3) = v) & (pc = 4 => ucmemory_data(0) = v & ucmemory_data(1) = a & ucmemory_data(2) = m & ucmemory_data(3) = v & v>a) & (pc = 5 => ucmemory_data(0) = v & ucmemory_data(1) = a & ucmemory_data(2) = m & ucmemory_data(3) = v & v<=a) & (pc = 6 => ucmemory_data(0) = v & ucmemory_data(1) = a & ucmemory_data(2) = m & ucmemory_data(3) = v & v>a) & (pc = 7 => ucmemory_data(0) = v & ucmemory_data(1) = a & ucmemory_data(2) = v & ucmemory_data(3) = v & v>a) & (pc = 8 => ucmemory_data(0) = v & ucmemory_data(1) = a & ucmemory_data(2) = m & ucmemory_data(3) = v & v<=a) & (pc = 9 => ucmemory_data(0) = v & ucmemory_data(1) = a & ucmemory_data(2) = max({a,v}) & ucmemory_data(3) = v) & (pc = 10 => ucmemory_data(0) = v & ucmemory_data(1) = a & ucmemory_data(2) = max({a,v}) & ucmemory_data(3) = v) VARIANT end-pc END));
  List_Substitution(Implementation(assembly2),run)==(VAR pc,end IN pc:=0;end:=10;(uc.init)(pc,end);WHILE pc<end DO BEGIN CASE pc OF EITHER 0 THEN (uc.set_data)(3,v) OR 1 THEN (uc.move)(0,1) OR 2 THEN (uc.move)(3,0) OR 3 THEN (uc.testgt)(0,1) OR 4 THEN (uc.goto)(6) OR 5 THEN (uc.goto)(8) OR 6 THEN (uc.move)(0,2) OR 7 THEN (uc.goto)(9) OR 8 THEN (uc.move)(1,2) OR 9 THEN uc.nop END END;pc <-- uc.get_pc END INVARIANT pc>=0 & pc<=end & pc = uc.pc & end = uc.end & (pc = 0 => (uc.memory_data)(0) = a & (uc.memory_data)(1) = b & (uc.memory_data)(2) = m) & (pc = 1 => (uc.memory_data)(0) = a & (uc.memory_data)(1) = b & (uc.memory_data)(2) = m & (uc.memory_data)(3) = v) & (pc = 2 => (uc.memory_data)(0) = a & (uc.memory_data)(1) = a & (uc.memory_data)(2) = m & (uc.memory_data)(3) = v) & (pc = 3 => (uc.memory_data)(0) = v & (uc.memory_data)(1) = a & (uc.memory_data)(2) = m & (uc.memory_data)(3) = v) & (pc = 4 => (uc.memory_data)(0) = v & (uc.memory_data)(1) = a & (uc.memory_data)(2) = m & (uc.memory_data)(3) = v & v>a) & (pc = 5 => (uc.memory_data)(0) = v & (uc.memory_data)(1) = a & (uc.memory_data)(2) = m & (uc.memory_data)(3) = v & v<=a) & (pc = 6 => (uc.memory_data)(0) = v & (uc.memory_data)(1) = a & (uc.memory_data)(2) = m & (uc.memory_data)(3) = v & v>a) & (pc = 7 => (uc.memory_data)(0) = v & (uc.memory_data)(1) = a & (uc.memory_data)(2) = v & (uc.memory_data)(3) = v & v>a) & (pc = 8 => (uc.memory_data)(0) = v & (uc.memory_data)(1) = a & (uc.memory_data)(2) = m & (uc.memory_data)(3) = v & v<=a) & (pc = 9 => (uc.memory_data)(0) = v & (uc.memory_data)(1) = a & (uc.memory_data)(2) = max({a,v}) & (uc.memory_data)(3) = v) & (pc = 10 => (uc.memory_data)(0) = v & (uc.memory_data)(1) = a & (uc.memory_data)(2) = max({a,v}) & (uc.memory_data)(3) = v) VARIANT end-pc END END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(assembly2))==(?);
  Inherited_List_Constants(Implementation(assembly2))==(?);
  List_Constants(Implementation(assembly2))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Implementation(assembly2))==(?);
  Context_List_Defered(Implementation(assembly2))==(?);
  Context_List_Sets(Implementation(assembly2))==(?);
  List_Own_Enumerated(Implementation(assembly2))==(?);
  List_Valuable_Sets(Implementation(assembly2))==(?);
  Inherited_List_Enumerated(Implementation(assembly2))==(?);
  Inherited_List_Defered(Implementation(assembly2))==(?);
  Inherited_List_Sets(Implementation(assembly2))==(?);
  List_Enumerated(Implementation(assembly2))==(?);
  List_Defered(Implementation(assembly2))==(?);
  List_Sets(Implementation(assembly2))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(assembly2))==(?);
  Expanded_List_HiddenConstants(Implementation(assembly2))==(?);
  List_HiddenConstants(Implementation(assembly2))==(?);
  External_List_HiddenConstants(Implementation(assembly2))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(assembly2))==(btrue);
  Context_List_Properties(Implementation(assembly2))==(btrue);
  Inherited_List_Properties(Implementation(assembly2))==(btrue);
  List_Properties(Implementation(assembly2))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(assembly2))==(aa : aa);
  List_Values(Implementation(assembly2))==(?)
END
&
THEORY ListSeenInfoX END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(assembly2),Machine(microcontroller))==(init_data,init,get_data,get_pc,set_end,get_end,nop,set_w,get_w,goto,iszero,isnotzero_simple,testgt,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(assembly2))==(Type(run) == Cst(No_type,btype(INTEGER,?,?)))
END
&
THEORY ListVisibleStaticX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(assembly2)) == (? | ? | ? | ucmemory_data,ucw,ucpc,ucend | run | ? | imported(Machine(uc.microcontroller)) | ? | assembly2);
  List_Of_HiddenCst_Ids(Implementation(assembly2)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(assembly2)) == (?);
  List_Of_VisibleVar_Ids(Implementation(assembly2)) == (? | ?);
  List_Of_Ids_SeenBNU(Implementation(assembly2)) == (? : ?);
  List_Of_Ids(Machine(microcontroller)) == (? | ? | end,pc,w,memory_data | ? | init_data,init,get_data,get_pc,set_end,get_end,nop,set_w,get_w,goto,iszero,isnotzero_simple,testgt,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div | ? | ? | ? | microcontroller);
  List_Of_HiddenCst_Ids(Machine(microcontroller)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(microcontroller)) == (?);
  List_Of_VisibleVar_Ids(Machine(microcontroller)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(microcontroller)) == (? : ?)
END
&
THEORY VariablesLocEnvX IS
  Variables_Loc(Implementation(assembly2),run, 1) == (Type(pc) == Lvl(btype(INTEGER,?,?));Type(end) == Lvl(btype(INTEGER,?,?)))
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
  List_Local_Operations(Implementation(assembly2))==(?)
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
  TypingPredicate(Implementation(assembly2))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(assembly2),Machine(uc.microcontroller))==(uc.memory_data,uc.w,uc.pc,uc.end);
  ImportedVisVariablesList(Implementation(assembly2),Machine(uc.microcontroller))==(?)
END
&
THEORY ListLocalOpInvariantX END
)
