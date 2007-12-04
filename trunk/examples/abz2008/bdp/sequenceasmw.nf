Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(sequenceasmw))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(sequenceasmw))==(Machine(sequence));
  Level(Implementation(sequenceasmw))==(1);
  Upper_Level(Implementation(sequenceasmw))==(Machine(sequence))
END
&
THEORY LoadedStructureX IS
  Implementation(sequenceasmw)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(sequenceasmw))==(types)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(sequenceasmw))==(uc.ram);
  Inherited_List_Includes(Implementation(sequenceasmw))==(uc.ram)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(sequenceasmw))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(sequenceasmw))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(sequenceasmw))==(?);
  Context_List_Variables(Implementation(sequenceasmw))==(?);
  Abstract_List_Variables(Implementation(sequenceasmw))==(b,a);
  Local_List_Variables(Implementation(sequenceasmw))==(?);
  List_Variables(Implementation(sequenceasmw))==(ucend,ucpc,ucmem);
  External_List_Variables(Implementation(sequenceasmw))==(uc.end,uc.pc,uc.mem)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(sequenceasmw))==(?);
  Abstract_List_VisibleVariables(Implementation(sequenceasmw))==(?);
  External_List_VisibleVariables(Implementation(sequenceasmw))==(?);
  Expanded_List_VisibleVariables(Implementation(sequenceasmw))==(?);
  List_VisibleVariables(Implementation(sequenceasmw))==(?);
  Internal_List_VisibleVariables(Implementation(sequenceasmw))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(sequenceasmw))==(btrue);
  Abstract_List_Invariant(Implementation(sequenceasmw))==(a : uint32 & b : uint32);
  Expanded_List_Invariant(Implementation(sequenceasmw))==(ucmem : NATURAL --> uint32 & ucpc : NATURAL & ucend : NATURAL & ucpc<=ucend);
  Context_List_Invariant(Implementation(sequenceasmw))==(btrue);
  List_Invariant(Implementation(sequenceasmw))==(a = ucmem(0) & b = ucmem(1))
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(sequenceasmw))==(btrue);
  Expanded_List_Assertions(Implementation(sequenceasmw))==(btrue);
  Context_List_Assertions(Implementation(sequenceasmw))==(btrue);
  List_Assertions(Implementation(sequenceasmw))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(sequenceasmw))==(@(mem$0).(mem$0 : NATURAL --> uint32 ==> ucmem:=mem$0) || ucpc:=0 || @(end$0).(end$0 : NATURAL ==> ucend:=end$0));
  Context_List_Initialisation(Implementation(sequenceasmw))==(skip);
  List_Initialisation(Implementation(sequenceasmw))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(sequenceasmw))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(sequenceasmw),Machine(uc.ram))==(?);
  List_Instanciated_Parameters(Implementation(sequenceasmw),Machine(types))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(sequenceasmw),Machine(uc.ram))==(btrue);
  List_Constraints(Implementation(sequenceasmw))==(btrue);
  List_Context_Constraints(Implementation(sequenceasmw))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(sequenceasmw))==(run);
  List_Operations(Implementation(sequenceasmw))==(run)
END
&
THEORY ListInputX IS
  List_Input(Implementation(sequenceasmw),run)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(sequenceasmw),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(sequenceasmw),run)==(run)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(sequenceasmw),run)==(btrue);
  List_Precondition(Implementation(sequenceasmw),run)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(sequenceasmw),run)==(btrue | @(pc,end).(pc:=0;end:=4;(end : NATURAL | ucpc,ucend:=0,end);WHILE pc<end DO not(pc = 3) & not(pc = 2) & not(pc = 1) & pc = 0 ==> (0 : NATURAL & 2 : NATURAL & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{2|->ucmem(0)},ucpc+1) [] not(pc = 0) & not(pc = 3) & not(pc = 2) & pc = 1 ==> (1 : NATURAL & 3 : NATURAL & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{3|->ucmem(1)},ucpc+1) [] not(pc = 0) & not(pc = 1) & not(pc = 3) & pc = 2 ==> (3 : NATURAL & 0 : NATURAL & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{0|->ucmem(3)},ucpc+1) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & pc = 3 ==> (2 : NATURAL & 1 : NATURAL & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{1|->ucmem(2)},ucpc+1) [] not(pc = 0) & not(pc = 1) & not(pc = 2) & not(pc = 3) ==> skip;(btrue | pc:=ucpc) INVARIANT pc>=0 & pc<=end & pc = ucpc & end = ucend & (pc = 0 => ucmem(0) = a & ucmem(1) = b) & (pc = 1 => ucmem(0) = a & ucmem(1) = b & ucmem(2) = a) & (pc = 2 => ucmem(0) = a & ucmem(1) = b & ucmem(2) = a & ucmem(3) = b) & (pc = 3 => ucmem(0) = b & ucmem(1) = b & ucmem(2) = a) & (pc = 4 => ucmem(0) = b & ucmem(1) = a) VARIANT end-pc END));
  List_Substitution(Implementation(sequenceasmw),run)==(VAR pc,end IN pc:=0;end:=4;(uc.init)(end);WHILE pc<end DO BEGIN CASE pc OF EITHER 0 THEN (uc.move)(0,2) OR 1 THEN (uc.move)(1,3) OR 2 THEN (uc.move)(3,0) OR 3 THEN (uc.move)(2,1) END END;pc <-- uc.get_pc END INVARIANT pc>=0 & pc<=end & pc = uc.pc & end = uc.end & (pc = 0 => (uc.mem)(0) = a & (uc.mem)(1) = b) & (pc = 1 => (uc.mem)(0) = a & (uc.mem)(1) = b & (uc.mem)(2) = a) & (pc = 2 => (uc.mem)(0) = a & (uc.mem)(1) = b & (uc.mem)(2) = a & (uc.mem)(3) = b) & (pc = 3 => (uc.mem)(0) = b & (uc.mem)(1) = b & (uc.mem)(2) = a) & (pc = 4 => (uc.mem)(0) = b & (uc.mem)(1) = a) VARIANT end-pc END END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(sequenceasmw))==(?);
  Inherited_List_Constants(Implementation(sequenceasmw))==(?);
  List_Constants(Implementation(sequenceasmw))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Implementation(sequenceasmw))==(?);
  Context_List_Defered(Implementation(sequenceasmw))==(?);
  Context_List_Sets(Implementation(sequenceasmw))==(?);
  List_Own_Enumerated(Implementation(sequenceasmw))==(?);
  List_Valuable_Sets(Implementation(sequenceasmw))==(?);
  Inherited_List_Enumerated(Implementation(sequenceasmw))==(?);
  Inherited_List_Defered(Implementation(sequenceasmw))==(?);
  Inherited_List_Sets(Implementation(sequenceasmw))==(?);
  List_Enumerated(Implementation(sequenceasmw))==(?);
  List_Defered(Implementation(sequenceasmw))==(?);
  List_Sets(Implementation(sequenceasmw))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(sequenceasmw))==(?);
  Expanded_List_HiddenConstants(Implementation(sequenceasmw))==(?);
  List_HiddenConstants(Implementation(sequenceasmw))==(?);
  External_List_HiddenConstants(Implementation(sequenceasmw))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(sequenceasmw))==(btrue);
  Context_List_Properties(Implementation(sequenceasmw))==(uint32 : POW(INTEGER) & uint32 = 0..MAXINT);
  Inherited_List_Properties(Implementation(sequenceasmw))==(btrue);
  List_Properties(Implementation(sequenceasmw))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(sequenceasmw))==(aa : aa);
  List_Values(Implementation(sequenceasmw))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(sequenceasmw),Machine(types))==(?);
  Seen_Context_List_Enumerated(Implementation(sequenceasmw))==(?);
  Seen_Context_List_Invariant(Implementation(sequenceasmw))==(btrue);
  Seen_Context_List_Assertions(Implementation(sequenceasmw))==(btrue);
  Seen_Context_List_Properties(Implementation(sequenceasmw))==(btrue);
  Seen_List_Constraints(Implementation(sequenceasmw))==(btrue);
  Seen_List_Operations(Implementation(sequenceasmw),Machine(types))==(?);
  Seen_Expanded_List_Invariant(Implementation(sequenceasmw),Machine(types))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(sequenceasmw),Machine(ram))==(init,nop,set,inc,move,testgt,testeq,goto,get_data,get_pc)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(sequenceasmw))==(Type(run) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX IS
  List_Constants(Implementation(sequenceasmw),Machine(types))==(uint32);
  List_Constants_Env(Implementation(sequenceasmw),Machine(types))==(Type(uint32) == Cst(SetOf(btype(INTEGER,"[uint32","]uint32"))))
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(sequenceasmw)) == (? | ? | ? | ucmem,ucpc,ucend | run | ? | seen(Machine(types)),imported(Machine(uc.ram)) | ? | sequenceasmw);
  List_Of_HiddenCst_Ids(Implementation(sequenceasmw)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(sequenceasmw)) == (?);
  List_Of_VisibleVar_Ids(Implementation(sequenceasmw)) == (? | ?);
  List_Of_Ids_SeenBNU(Implementation(sequenceasmw)) == (seen(Machine(types)) : (uint32 | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(ram)) == (? | ? | end,pc,mem | ? | init,nop,set,inc,move,testgt,testeq,goto,get_data,get_pc | ? | seen(Machine(types)) | ? | ram);
  List_Of_HiddenCst_Ids(Machine(ram)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ram)) == (?);
  List_Of_VisibleVar_Ids(Machine(ram)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ram)) == (? : ?);
  List_Of_Ids(Machine(types)) == (uint32 | ? | ? | ? | ? | ? | ? | ? | types);
  List_Of_HiddenCst_Ids(Machine(types)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(types)) == (uint32);
  List_Of_VisibleVar_Ids(Machine(types)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(types)) == (? : ?)
END
&
THEORY VariablesLocEnvX IS
  Variables_Loc(Implementation(sequenceasmw),run, 1) == (Type(pc) == Lvl(btype(INTEGER,?,?));Type(end) == Lvl(btype(INTEGER,?,?)))
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
  List_Local_Operations(Implementation(sequenceasmw))==(?)
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
  TypingPredicate(Implementation(sequenceasmw))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(sequenceasmw),Machine(uc.ram))==(uc.mem,uc.pc,uc.end);
  ImportedVisVariablesList(Implementation(sequenceasmw),Machine(uc.ram))==(?)
END
&
THEORY ListLocalOpInvariantX END
)
