Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(assembly2))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(assembly2))==(Machine(addition));
  Level(Implementation(assembly2))==(1);
  Upper_Level(Implementation(assembly2))==(Machine(addition))
END
&
THEORY LoadedStructureX IS
  Implementation(assembly2)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(assembly2))==(programvariant)
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
  Abstract_List_Variables(Implementation(assembly2))==(s,b,a);
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
  Abstract_List_Invariant(Implementation(assembly2))==(a : NATURAL & b : NATURAL & s : NATURAL & a+b<=MAXINT & s = a+b);
  Expanded_List_Invariant(Implementation(assembly2))==(ucmemory_data : NATURAL --> NATURAL & ucw : NATURAL & ucpc : NATURAL & ucend : NATURAL & ucpc<=ucend);
  Context_List_Invariant(Implementation(assembly2))==(btrue);
  List_Invariant(Implementation(assembly2))==(a = ucmemory_data(0) & b = ucmemory_data(1) & s = ucmemory_data(2))
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
  List_Instanciated_Parameters(Implementation(assembly2),Machine(uc.microcontroller))==(?);
  List_Instanciated_Parameters(Implementation(assembly2),Machine(programvariant))==(?)
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
  List_Input(Implementation(assembly2),run)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(assembly2),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(assembly2),run)==(run)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(assembly2),run)==(btrue);
  List_Precondition(Implementation(assembly2),run)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(assembly2),run)==(btrue | @(pc,end,loopvar).(pc:=0;end:=8;(pc : NATURAL & end : NATURAL & pc<=end | ucpc,ucend:=pc,end);loopvar:=0;WHILE pc<end DO not(pc = 7) & not(pc = 6) & not(pc = 5) & not(pc = 4) & not(pc = 3) & not(pc = 2) & not(pc = 1) & pc = 0 ==> (0 : NATURAL & 2 : NATURAL & ucpc+1<=ucend & ucpc+1 : NATURAL | ucmemory_data,ucpc:=ucmemory_data<+{2|->ucmemory_data(0)},ucpc+1) [] not(pc = 0) & not(pc = 7) & not(pc = 6) & not(pc = 5) & not(pc = 4) & not(pc = 3) & not(pc = 2) & pc = 1 ==> (3 : NATURAL & 0 : NATURAL & ucpc+1 : NATURAL & ucpc+1<=ucend | ucmemory_data,ucpc:=ucmemory_data<+{3|->0},ucpc+1) [] not(pc = 0) & not(pc = 1) & not(pc = 7) & not(pc = 6) & not(pc = 5) & not(pc = 4) & not(pc = 3) & pc = 2 ==> (1 : NATURAL & 3 : NATURAL & (ucmemory_data(1) = ucmemory_data(3) => ucpc+1 : NATURAL & ucpc+1<=ucend) & (ucmemory_data(1)/=ucmemory_data(3) => ucpc+2 : NATURAL & ucpc+2<=ucend) | ucmemory_data(1) = ucmemory_data(3) ==> ucpc:=ucpc+1 [] not(ucmemory_data(1) = ucmemory_data(3)) ==> ucpc:=ucpc+2) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & not(pc = 7) & not(pc = 6) & not(pc = 5) & not(pc = 4) & pc = 3 ==> (7 : NATURAL & 7<=ucend | ucpc:=7) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & not(pc = 3) & not(pc = 7) & not(pc = 6) & not(pc = 5) & pc = 4 ==> (2 : NATURAL & ucmemory_data(2)+1 : NATURAL & ucpc+1 : NATURAL & ucpc+1<=ucend & dom(ucmemory_data<+{2|->ucmemory_data(2)+1}) = NATURAL | ucmemory_data,ucpc:=ucmemory_data<+{2|->ucmemory_data(2)+1},ucpc+1) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & not(pc = 3) & not(pc = 4) & not(pc = 7) & not(pc = 6) & pc = 5 ==> (3 : NATURAL & ucmemory_data(3)+1 : NATURAL & ucpc+1 : NATURAL & ucpc+1<=ucend & dom(ucmemory_data<+{3|->ucmemory_data(3)+1}) = NATURAL | ucmemory_data,ucpc:=ucmemory_data<+{3|->ucmemory_data(3)+1},ucpc+1) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & not(pc = 3) & not(pc = 4) & not(pc = 5) & not(pc = 7) & pc = 6 ==> ((2 : NATURAL & 2<=ucend | ucpc:=2);(loopvar+1 : INT & loopvar : INT & 1 : INT | loopvar:=loopvar+1)) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & not(pc = 3) & not(pc = 4) & not(pc = 5) & not(pc = 6) & pc = 7 ==> (ucpc<ucend | ucpc:=ucpc+1) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & not(pc = 3) & not(pc = 4) & not(pc = 5) & not(pc = 6) & not(pc = 7) ==> skip;(btrue | pc:=ucpc) INVARIANT pc>=0 & pc<=end & pc = ucpc & end = ucend & end = 8 & loopvar : NATURAL & ucmemory_data : NATURAL --> NATURAL & ucmemory_data(0) = a & ucmemory_data(1) = b & (pc = 0 => loopvar = 0 & ucmemory_data(2) = s) & (pc = 1 => loopvar = 0 & ucmemory_data(2) = a) & (pc = 2 => ucmemory_data(2) = a+ucmemory_data(3) & ucmemory_data(3) = loopvar & loopvar<=b) & (pc = 3 => ucmemory_data(2) = a+ucmemory_data(3) & ucmemory_data(3) = loopvar & loopvar = b) & (pc = 4 => ucmemory_data(2) = a+ucmemory_data(3) & ucmemory_data(3) = loopvar & loopvar<b) & (pc = 5 => ucmemory_data(2) = a+ucmemory_data(3)+1 & ucmemory_data(3) = loopvar & loopvar<b) & (pc = 6 => ucmemory_data(2) = a+ucmemory_data(3) & ucmemory_data(3) = loopvar+1 & loopvar<b) & (pc = 7 => ucmemory_data(2) = a+ucmemory_data(3) & ucmemory_data(3) = loopvar & loopvar = b) & (pc = 8 => ucmemory_data(2) = a+ucmemory_data(3) & ucmemory_data(3) = loopvar & loopvar = b) VARIANT pgvar(pc,ucmemory_data(1),loopvar) END));
  List_Substitution(Implementation(assembly2),run)==(VAR pc,end,loopvar IN pc:=0;end:=8;(uc.init)(pc,end);loopvar:=0;WHILE pc<end DO BEGIN CASE pc OF EITHER 0 THEN (uc.move)(0,2) OR 1 THEN (uc.set_data)(3,0) OR 2 THEN (uc.testeq)(1,3) OR 3 THEN (uc.goto)(7) OR 4 THEN (uc.inc)(2) OR 5 THEN (uc.inc)(3) OR 6 THEN BEGIN (uc.goto)(2);loopvar:=loopvar+1 END OR 7 THEN uc.nop END END;pc <-- uc.get_pc END INVARIANT pc>=0 & pc<=end & pc = uc.pc & end = uc.end & end = 8 & loopvar : NATURAL & uc.memory_data : NATURAL --> NATURAL & (uc.memory_data)(0) = a & (uc.memory_data)(1) = b & (pc = 0 => loopvar = 0 & (uc.memory_data)(2) = s) & (pc = 1 => loopvar = 0 & (uc.memory_data)(2) = a) & (pc = 2 => (uc.memory_data)(2) = a+(uc.memory_data)(3) & (uc.memory_data)(3) = loopvar & loopvar<=b) & (pc = 3 => (uc.memory_data)(2) = a+(uc.memory_data)(3) & (uc.memory_data)(3) = loopvar & loopvar = b) & (pc = 4 => (uc.memory_data)(2) = a+(uc.memory_data)(3) & (uc.memory_data)(3) = loopvar & loopvar<b) & (pc = 5 => (uc.memory_data)(2) = a+(uc.memory_data)(3)+1 & (uc.memory_data)(3) = loopvar & loopvar<b) & (pc = 6 => (uc.memory_data)(2) = a+(uc.memory_data)(3) & (uc.memory_data)(3) = loopvar+1 & loopvar<b) & (pc = 7 => (uc.memory_data)(2) = a+(uc.memory_data)(3) & (uc.memory_data)(3) = loopvar & loopvar = b) & (pc = 8 => (uc.memory_data)(2) = a+(uc.memory_data)(3) & (uc.memory_data)(3) = loopvar & loopvar = b) VARIANT pgvar(pc,(uc.memory_data)(1),loopvar) END END)
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
  Context_List_Properties(Implementation(assembly2))==(pgsize : NATURAL & loopaddr : NATURAL & loopsize : NATURAL & pgvar : NATURAL*NATURAL*NATURAL +-> NATURAL & pgsize = 8 & loopsize = 5 & loopaddr = 2 & !(pc,loopmax,loopvar).(pc : NATURAL & loopmax : NATURAL & loopvar : NATURAL => (pc = 7 or pc = 8 => pgvar(pc,loopmax,loopvar) = pgsize-pc) & (pc = 0 or pc = 1 => pgvar(pc,loopmax,loopvar) = pgsize-pc+loopsize*loopmax) & (pc = 2 or pc = 3 or pc = 4 or pc = 5 or pc = 6 => pgvar(pc,loopmax,loopvar) = pgsize-loopaddr+loopsize*(loopmax-loopvar)-(pc-loopaddr))));
  Inherited_List_Properties(Implementation(assembly2))==(btrue);
  List_Properties(Implementation(assembly2))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(assembly2))==(aa : aa);
  List_Values(Implementation(assembly2))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(assembly2),Machine(programvariant))==(?);
  Seen_Context_List_Enumerated(Implementation(assembly2))==(?);
  Seen_Context_List_Invariant(Implementation(assembly2))==(btrue);
  Seen_Context_List_Assertions(Implementation(assembly2))==(btrue);
  Seen_Context_List_Properties(Implementation(assembly2))==(btrue);
  Seen_List_Constraints(Implementation(assembly2))==(btrue);
  Seen_List_Operations(Implementation(assembly2),Machine(programvariant))==(?);
  Seen_Expanded_List_Invariant(Implementation(assembly2),Machine(programvariant))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(assembly2),Machine(microcontroller))==(init_data,init,get_data,get_pc,set_end,get_end,nop,set_w,get_w,goto,iszero,isnotzero_simple,testeq,testgt,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(assembly2))==(Type(run) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX IS
  List_Constants(Implementation(assembly2),Machine(programvariant))==(pgvar,pgsize,loopaddr,loopsize);
  List_Constants_Env(Implementation(assembly2),Machine(programvariant))==(Type(pgvar) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(pgsize) == Cst(btype(INTEGER,?,?));Type(loopaddr) == Cst(btype(INTEGER,?,?));Type(loopsize) == Cst(btype(INTEGER,?,?)))
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(assembly2)) == (? | ? | ? | ucmemory_data,ucw,ucpc,ucend | run | ? | seen(Machine(programvariant)),imported(Machine(uc.microcontroller)) | ? | assembly2);
  List_Of_HiddenCst_Ids(Implementation(assembly2)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(assembly2)) == (?);
  List_Of_VisibleVar_Ids(Implementation(assembly2)) == (? | ?);
  List_Of_Ids_SeenBNU(Implementation(assembly2)) == (? : ?);
  List_Of_Ids(Machine(microcontroller)) == (? | ? | end,pc,w,memory_data | ? | init_data,init,get_data,get_pc,set_end,get_end,nop,set_w,get_w,goto,iszero,isnotzero_simple,testeq,testgt,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div | ? | ? | ? | microcontroller);
  List_Of_HiddenCst_Ids(Machine(microcontroller)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(microcontroller)) == (?);
  List_Of_VisibleVar_Ids(Machine(microcontroller)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(microcontroller)) == (? : ?);
  List_Of_Ids(Machine(programvariant)) == (pgvar,pgsize,loopaddr,loopsize | ? | ? | ? | ? | ? | ? | ? | programvariant);
  List_Of_HiddenCst_Ids(Machine(programvariant)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(programvariant)) == (pgvar,pgsize,loopaddr,loopsize);
  List_Of_VisibleVar_Ids(Machine(programvariant)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(programvariant)) == (? : ?)
END
&
THEORY VariablesLocEnvX IS
  Variables_Loc(Implementation(assembly2),run, 1) == (Type(pc) == Lvl(btype(INTEGER,?,?));Type(end) == Lvl(btype(INTEGER,?,?));Type(loopvar) == Lvl(btype(INTEGER,?,?)))
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
