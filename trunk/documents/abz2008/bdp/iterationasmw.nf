Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(iterationasmw))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(iterationasmw))==(Machine(iteration));
  Level(Implementation(iterationasmw))==(1);
  Upper_Level(Implementation(iterationasmw))==(Machine(iteration))
END
&
THEORY LoadedStructureX IS
  Implementation(iterationasmw)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(iterationasmw))==(types,iterationasmvariant)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(iterationasmw))==(uc.ram);
  Inherited_List_Includes(Implementation(iterationasmw))==(uc.ram)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(iterationasmw))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(iterationasmw))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(iterationasmw))==(?);
  Context_List_Variables(Implementation(iterationasmw))==(?);
  Abstract_List_Variables(Implementation(iterationasmw))==(s,b,a);
  Local_List_Variables(Implementation(iterationasmw))==(?);
  List_Variables(Implementation(iterationasmw))==(?);
  External_List_Variables(Implementation(iterationasmw))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(iterationasmw))==(?);
  Abstract_List_VisibleVariables(Implementation(iterationasmw))==(?);
  External_List_VisibleVariables(Implementation(iterationasmw))==(uc.end,uc.pc,uc.mem);
  Expanded_List_VisibleVariables(Implementation(iterationasmw))==(ucend,ucpc,ucmem);
  List_VisibleVariables(Implementation(iterationasmw))==(?);
  Internal_List_VisibleVariables(Implementation(iterationasmw))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(iterationasmw))==(btrue);
  Abstract_List_Invariant(Implementation(iterationasmw))==(a : uint32 & b : uint32 & s : uint32 & a+b : uint32 & s = a+b);
  Expanded_List_Invariant(Implementation(iterationasmw))==(ucmem : NATURAL --> uint32 & ucpc : NATURAL & ucend : NATURAL & ucpc<=ucend);
  Context_List_Invariant(Implementation(iterationasmw))==(btrue);
  List_Invariant(Implementation(iterationasmw))==(a = ucmem(0) & b = ucmem(1) & s = ucmem(2))
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(iterationasmw))==(btrue);
  Expanded_List_Assertions(Implementation(iterationasmw))==(btrue);
  Context_List_Assertions(Implementation(iterationasmw))==(btrue);
  List_Assertions(Implementation(iterationasmw))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(iterationasmw))==(@(mem$0).(mem$0 : NATURAL --> uint32 ==> ucmem:=mem$0) || ucpc:=0 || @(end$0).(end$0 : NATURAL ==> ucend:=end$0);(3 : NATURAL | ucpc,ucend:=0,3);(0 : NATURAL & 0 : uint32 & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{0|->0},ucpc+1);(1 : NATURAL & 0 : uint32 & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{1|->0},ucpc+1);(2 : NATURAL & 0 : uint32 & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{2|->0},ucpc+1));
  Context_List_Initialisation(Implementation(iterationasmw))==(skip);
  List_Initialisation(Implementation(iterationasmw))==(BEGIN (uc.init)(3);(uc.set)(0,0);(uc.set)(1,0);(uc.set)(2,0) END)
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(iterationasmw))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(iterationasmw),Machine(uc.ram))==(?);
  List_Instanciated_Parameters(Implementation(iterationasmw),Machine(types))==(?);
  List_Instanciated_Parameters(Implementation(iterationasmw),Machine(iterationasmvariant))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(iterationasmw),Machine(uc.ram))==(btrue);
  List_Constraints(Implementation(iterationasmw))==(btrue);
  List_Context_Constraints(Implementation(iterationasmw))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(iterationasmw))==(run);
  List_Operations(Implementation(iterationasmw))==(run)
END
&
THEORY ListInputX IS
  List_Input(Implementation(iterationasmw),run)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(iterationasmw),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(iterationasmw),run)==(run)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(iterationasmw),run)==(btrue);
  List_Precondition(Implementation(iterationasmw),run)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(iterationasmw),run)==(btrue | @loopvar.((7 : NATURAL | ucpc,ucend:=0,7);loopvar:=0;WHILE ucpc<ucend DO not(ucpc = 6) & not(ucpc = 5) & not(ucpc = 4) & not(ucpc = 3) & not(ucpc = 2) & not(ucpc = 1) & ucpc = 0 ==> (0 : NATURAL & 2 : NATURAL & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{2|->ucmem(0)},ucpc+1) [] not(ucpc = 0) & not(ucpc = 6) & not(ucpc = 5) & not(ucpc = 4) & not(ucpc = 3) & not(ucpc = 2) & ucpc = 1 ==> (3 : NATURAL & 0 : uint32 & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{3|->0},ucpc+1) [] not(ucpc = 0) & not(ucpc = 1) & not(ucpc = 6) & not(ucpc = 5) & not(ucpc = 4) & not(ucpc = 3) & ucpc = 2 ==> (1 : NATURAL & 3 : NATURAL & (ucmem(1) = ucmem(3) => ucpc+1<=ucend) & (ucmem(1)/=ucmem(3) => ucpc+2<=ucend) | ucmem(1) = ucmem(3) ==> ucpc:=ucpc+1 [] not(ucmem(1) = ucmem(3)) ==> ucpc:=ucpc+2) [] not(ucpc = 0) & not(ucpc = 1) & not(ucpc = 2) & not(ucpc = 6) & not(ucpc = 5) & not(ucpc = 4) & ucpc = 3 ==> (7 : NATURAL & 7<=ucend | ucpc:=7) [] not(ucpc = 0) & not(ucpc = 1) & not(ucpc = 2) & not(ucpc = 3) & not(ucpc = 6) & not(ucpc = 5) & ucpc = 4 ==> (2 : NATURAL & ucpc+1<=ucend & ucmem(2)<MAXINT | ucmem,ucpc:=ucmem<+{2|->ucmem(2)+1},ucpc+1) [] not(ucpc = 0) & not(ucpc = 1) & not(ucpc = 2) & not(ucpc = 3) & not(ucpc = 4) & not(ucpc = 6) & ucpc = 5 ==> (3 : NATURAL & ucpc+1<=ucend & ucmem(3)<MAXINT | ucmem,ucpc:=ucmem<+{3|->ucmem(3)+1},ucpc+1) [] not(ucpc = 0) & not(ucpc = 1) & not(ucpc = 2) & not(ucpc = 3) & not(ucpc = 4) & not(ucpc = 5) & ucpc = 6 ==> ((2 : NATURAL & 2<=ucend | ucpc:=2);(loopvar+1 : INT & loopvar : INT & 1 : INT | loopvar:=loopvar+1)) [] not(ucpc = 0) & not(ucpc = 1) & not(ucpc = 2) & not(ucpc = 3) & not(ucpc = 4) & not(ucpc = 5) & not(ucpc = 6) ==> skip INVARIANT 0<=ucpc & ucpc<=ucend & loopvar : NATURAL & ucmem : NATURAL --> NATURAL & ucmem(0) = a & ucmem(1) = b & (ucpc = 0 => loopvar = 0 & ucmem(2) = s) & (ucpc = 1 => loopvar = 0 & ucmem(2) = a) & (ucpc = 2 => ucmem(2) = a+ucmem(3) & ucmem(3) = loopvar & loopvar<=b) & (ucpc = 3 => ucmem(2) = a+ucmem(3) & ucmem(3) = loopvar & loopvar = b) & (ucpc = 4 => ucmem(2) = a+ucmem(3) & ucmem(3) = loopvar & loopvar<b) & (ucpc = 5 => ucmem(2) = a+ucmem(3)+1 & ucmem(3) = loopvar & loopvar<b) & (ucpc = 6 => ucmem(2) = a+ucmem(3) & ucmem(3) = loopvar+1 & loopvar<b) & (ucpc = 7 => ucmem(2) = a+ucmem(3) & ucmem(3) = loopvar & loopvar = b) VARIANT pgvar(ucpc,ucmem(1),loopvar) END));
  List_Substitution(Implementation(iterationasmw),run)==(VAR loopvar IN (uc.init)(7);loopvar:=0;WHILE uc.pc<uc.end DO BEGIN CASE uc.pc OF EITHER 0 THEN (uc.copy)(0,2) OR 1 THEN (uc.set)(3,0) OR 2 THEN (uc.testeq)(1,3) OR 3 THEN (uc.goto)(7) OR 4 THEN (uc.inc)(2) OR 5 THEN (uc.inc)(3) OR 6 THEN BEGIN (uc.goto)(2);loopvar:=loopvar+1 END END END END INVARIANT 0<=uc.pc & uc.pc<=uc.end & loopvar : NATURAL & uc.mem : NATURAL --> NATURAL & (uc.mem)(0) = a & (uc.mem)(1) = b & (uc.pc = 0 => loopvar = 0 & (uc.mem)(2) = s) & (uc.pc = 1 => loopvar = 0 & (uc.mem)(2) = a) & (uc.pc = 2 => (uc.mem)(2) = a+(uc.mem)(3) & (uc.mem)(3) = loopvar & loopvar<=b) & (uc.pc = 3 => (uc.mem)(2) = a+(uc.mem)(3) & (uc.mem)(3) = loopvar & loopvar = b) & (uc.pc = 4 => (uc.mem)(2) = a+(uc.mem)(3) & (uc.mem)(3) = loopvar & loopvar<b) & (uc.pc = 5 => (uc.mem)(2) = a+(uc.mem)(3)+1 & (uc.mem)(3) = loopvar & loopvar<b) & (uc.pc = 6 => (uc.mem)(2) = a+(uc.mem)(3) & (uc.mem)(3) = loopvar+1 & loopvar<b) & (uc.pc = 7 => (uc.mem)(2) = a+(uc.mem)(3) & (uc.mem)(3) = loopvar & loopvar = b) VARIANT pgvar(uc.pc,(uc.mem)(1),loopvar) END END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(iterationasmw))==(?);
  Inherited_List_Constants(Implementation(iterationasmw))==(?);
  List_Constants(Implementation(iterationasmw))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Implementation(iterationasmw))==(?);
  Context_List_Defered(Implementation(iterationasmw))==(?);
  Context_List_Sets(Implementation(iterationasmw))==(?);
  List_Own_Enumerated(Implementation(iterationasmw))==(?);
  List_Valuable_Sets(Implementation(iterationasmw))==(?);
  Inherited_List_Enumerated(Implementation(iterationasmw))==(?);
  Inherited_List_Defered(Implementation(iterationasmw))==(?);
  Inherited_List_Sets(Implementation(iterationasmw))==(?);
  List_Enumerated(Implementation(iterationasmw))==(?);
  List_Defered(Implementation(iterationasmw))==(?);
  List_Sets(Implementation(iterationasmw))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(iterationasmw))==(?);
  Expanded_List_HiddenConstants(Implementation(iterationasmw))==(?);
  List_HiddenConstants(Implementation(iterationasmw))==(?);
  External_List_HiddenConstants(Implementation(iterationasmw))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(iterationasmw))==(btrue);
  Context_List_Properties(Implementation(iterationasmw))==(uint32 : POW(INTEGER) & uint32 = 0..MAXINT & uint16 : POW(INTEGER) & uint16 = 0..65535 & pgsize : NATURAL & loopaddr : NATURAL & loopsize : NATURAL & pgvar : NATURAL*NATURAL*NATURAL +-> NATURAL & pgsize = 7 & loopsize = 5 & loopaddr = 2 & !(pc,loopmax,loopvar).(pc : NATURAL & loopmax : NATURAL & loopvar : NATURAL => (pc = 7 => pgvar(pc,loopmax,loopvar) = pgsize-pc) & (pc = 0 or pc = 1 => pgvar(pc,loopmax,loopvar) = pgsize-pc+loopsize*loopmax) & (pc = 2 or pc = 3 or pc = 4 or pc = 5 or pc = 6 => pgvar(pc,loopmax,loopvar) = pgsize-loopaddr+loopsize*(loopmax-loopvar)-(pc-loopaddr))));
  Inherited_List_Properties(Implementation(iterationasmw))==(btrue);
  List_Properties(Implementation(iterationasmw))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(iterationasmw))==(aa : aa);
  List_Values(Implementation(iterationasmw))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(iterationasmw),Machine(iterationasmvariant))==(?);
  Seen_Context_List_Enumerated(Implementation(iterationasmw))==(?);
  Seen_Context_List_Invariant(Implementation(iterationasmw))==(btrue);
  Seen_Context_List_Assertions(Implementation(iterationasmw))==(btrue);
  Seen_Context_List_Properties(Implementation(iterationasmw))==(btrue);
  Seen_List_Constraints(Implementation(iterationasmw))==(btrue);
  Seen_List_Operations(Implementation(iterationasmw),Machine(iterationasmvariant))==(?);
  Seen_Expanded_List_Invariant(Implementation(iterationasmw),Machine(iterationasmvariant))==(btrue);
  Seen_Internal_List_Operations(Implementation(iterationasmw),Machine(types))==(?);
  Seen_List_Operations(Implementation(iterationasmw),Machine(types))==(?);
  Seen_Expanded_List_Invariant(Implementation(iterationasmw),Machine(types))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(iterationasmw),Machine(ram))==(init,nop,set,inc,copy,testgt,testeq,goto,get_data,get_pc)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(iterationasmw))==(Type(run) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX IS
  List_Constants(Implementation(iterationasmw),Machine(iterationasmvariant))==(pgvar,pgsize,loopaddr,loopsize);
  List_Constants_Env(Implementation(iterationasmw),Machine(iterationasmvariant))==(Type(pgvar) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(pgsize) == Cst(btype(INTEGER,?,?));Type(loopaddr) == Cst(btype(INTEGER,?,?));Type(loopsize) == Cst(btype(INTEGER,?,?)));
  List_Constants(Implementation(iterationasmw),Machine(types))==(uint32,uint16);
  List_Constants_Env(Implementation(iterationasmw),Machine(types))==(Type(uint32) == Cst(SetOf(btype(INTEGER,"[uint32","]uint32")));Type(uint16) == Cst(SetOf(btype(INTEGER,"[uint16","]uint16"))))
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(iterationasmw)) == (? | ? | ? | ? | run | ? | seen(Machine(types)),seen(Machine(iterationasmvariant)),imported(Machine(uc.ram)) | ? | iterationasmw);
  List_Of_HiddenCst_Ids(Implementation(iterationasmw)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(iterationasmw)) == (?);
  List_Of_VisibleVar_Ids(Implementation(iterationasmw)) == (? | ucmem,ucpc,ucend);
  List_Of_Ids_SeenBNU(Implementation(iterationasmw)) == (seen(Machine(types)) : (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(ram)) == (? | ? | ? | ? | init,nop,set,inc,copy,testgt,testeq,goto,get_data,get_pc | ? | seen(Machine(types)) | ? | ram);
  List_Of_HiddenCst_Ids(Machine(ram)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ram)) == (?);
  List_Of_VisibleVar_Ids(Machine(ram)) == (end,pc,mem | ?);
  List_Of_Ids_SeenBNU(Machine(ram)) == (? : ?);
  List_Of_Ids(Machine(types)) == (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | types);
  List_Of_HiddenCst_Ids(Machine(types)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(types)) == (uint32,uint16);
  List_Of_VisibleVar_Ids(Machine(types)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(types)) == (? : ?);
  List_Of_Ids(Machine(iterationasmvariant)) == (pgvar,pgsize,loopaddr,loopsize | ? | ? | ? | ? | ? | ? | ? | iterationasmvariant);
  List_Of_HiddenCst_Ids(Machine(iterationasmvariant)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(iterationasmvariant)) == (pgvar,pgsize,loopaddr,loopsize);
  List_Of_VisibleVar_Ids(Machine(iterationasmvariant)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(iterationasmvariant)) == (? : ?)
END
&
THEORY VariablesLocEnvX IS
  Variables_Loc(Implementation(iterationasmw),run, 1) == (Type(loopvar) == Lvl(btype(INTEGER,?,?)))
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
  List_Local_Operations(Implementation(iterationasmw))==(?)
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
  TypingPredicate(Implementation(iterationasmw))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(iterationasmw),Machine(uc.ram))==(?);
  ImportedVisVariablesList(Implementation(iterationasmw),Machine(uc.ram))==(uc.mem,uc.pc,uc.end)
END
&
THEORY ListLocalOpInvariantX END
)
