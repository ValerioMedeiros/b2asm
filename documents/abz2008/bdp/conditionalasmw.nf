Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(conditionalasmw))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(conditionalasmw))==(Machine(conditional));
  Level(Implementation(conditionalasmw))==(1);
  Upper_Level(Implementation(conditionalasmw))==(Machine(conditional))
END
&
THEORY LoadedStructureX IS
  Implementation(conditionalasmw)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(conditionalasmw))==(types)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(conditionalasmw))==(uc.ram);
  Inherited_List_Includes(Implementation(conditionalasmw))==(uc.ram)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(conditionalasmw))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(conditionalasmw))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(conditionalasmw))==(?);
  Context_List_Variables(Implementation(conditionalasmw))==(?);
  Abstract_List_Variables(Implementation(conditionalasmw))==(st);
  Local_List_Variables(Implementation(conditionalasmw))==(?);
  List_Variables(Implementation(conditionalasmw))==(?);
  External_List_Variables(Implementation(conditionalasmw))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(conditionalasmw))==(?);
  Abstract_List_VisibleVariables(Implementation(conditionalasmw))==(?);
  External_List_VisibleVariables(Implementation(conditionalasmw))==(uc.end,uc.pc,uc.mem);
  Expanded_List_VisibleVariables(Implementation(conditionalasmw))==(ucend,ucpc,ucmem);
  List_VisibleVariables(Implementation(conditionalasmw))==(?);
  Internal_List_VisibleVariables(Implementation(conditionalasmw))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(conditionalasmw))==(btrue);
  Abstract_List_Invariant(Implementation(conditionalasmw))==(st : POW(uint32));
  Expanded_List_Invariant(Implementation(conditionalasmw))==(ucmem : NATURAL --> uint32 & ucpc : NATURAL & ucend : NATURAL & ucpc<=ucend);
  Context_List_Invariant(Implementation(conditionalasmw))==(btrue);
  List_Invariant(Implementation(conditionalasmw))==(max(st) = ucmem(0))
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(conditionalasmw))==(btrue);
  Expanded_List_Assertions(Implementation(conditionalasmw))==(btrue);
  Context_List_Assertions(Implementation(conditionalasmw))==(btrue);
  List_Assertions(Implementation(conditionalasmw))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(conditionalasmw))==(@(mem$0).(mem$0 : NATURAL --> uint32 ==> ucmem:=mem$0) || ucpc:=0 || @(end$0).(end$0 : NATURAL ==> ucend:=end$0);(1 : NATURAL | ucpc,ucend:=0,1);(0 : NATURAL & 0 : uint32 & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{0|->0},ucpc+1));
  Context_List_Initialisation(Implementation(conditionalasmw))==(skip);
  List_Initialisation(Implementation(conditionalasmw))==(BEGIN (uc.init)(1);(uc.set)(0,0) END)
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(conditionalasmw))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(conditionalasmw),Machine(uc.ram))==(?);
  List_Instanciated_Parameters(Implementation(conditionalasmw),Machine(types))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(conditionalasmw),Machine(uc.ram))==(btrue);
  List_Constraints(Implementation(conditionalasmw))==(btrue);
  List_Context_Constraints(Implementation(conditionalasmw))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(conditionalasmw))==(add,largest);
  List_Operations(Implementation(conditionalasmw))==(add,largest)
END
&
THEORY ListInputX IS
  List_Input(Implementation(conditionalasmw),add)==(v);
  List_Input(Implementation(conditionalasmw),largest)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(conditionalasmw),add)==(?);
  List_Output(Implementation(conditionalasmw),largest)==(res)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(conditionalasmw),add)==(add(v));
  List_Header(Implementation(conditionalasmw),largest)==(res <-- largest)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(conditionalasmw),add)==(btrue);
  List_Precondition(Implementation(conditionalasmw),add)==(v : uint32 & v/:st);
  Own_Precondition(Implementation(conditionalasmw),largest)==(btrue);
  List_Precondition(Implementation(conditionalasmw),largest)==(st/={})
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(conditionalasmw),largest)==(st/={} & ucmem(0) : INT & 0 : dom(ucmem) | res:=ucmem(0));
  Expanded_List_Substitution(Implementation(conditionalasmw),add)==(v : uint32 & v/:st | (3 : NATURAL | ucpc,ucend:=0,3);WHILE ucpc<ucend DO not(ucpc = 2) & not(ucpc = 1) & ucpc = 0 ==> (1 : NATURAL & v : uint32 & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{1|->v},ucpc+1) [] not(ucpc = 0) & not(ucpc = 2) & ucpc = 1 ==> (1 : NATURAL & 0 : NATURAL & (ucmem(1)>ucmem(0) => ucpc+1<=ucend) & (ucmem(1)<=ucmem(0) => ucpc+2<=ucend) | ucmem(1)>ucmem(0) ==> ucpc:=ucpc+1 [] not(ucmem(1)>ucmem(0)) ==> ucpc:=ucpc+2) [] not(ucpc = 0) & not(ucpc = 1) & ucpc = 2 ==> (1 : NATURAL & 0 : NATURAL & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{0|->ucmem(1)},ucpc+1) [] not(ucpc = 0) & not(ucpc = 1) & not(ucpc = 2) ==> skip INVARIANT 0<=ucpc & ucpc<=ucend & (ucpc = 0 => ucmem(0) = max(st)) & (ucpc = 1 => ucmem(0) = max(st) & ucmem(1) = v) & (ucpc = 2 => ucmem(0) = max(st) & ucmem(1) = v & max(st)<v) & (ucpc = 3 => ucmem(0) = max({v,max(st)})) VARIANT ucend-ucpc END);
  List_Substitution(Implementation(conditionalasmw),add)==((uc.init)(3);WHILE uc.pc<uc.end DO BEGIN CASE uc.pc OF EITHER 0 THEN (uc.set)(1,v) OR 1 THEN (uc.testgt)(1,0) OR 2 THEN (uc.copy)(1,0) END END END INVARIANT 0<=uc.pc & uc.pc<=uc.end & (uc.pc = 0 => (uc.mem)(0) = max(st)) & (uc.pc = 1 => (uc.mem)(0) = max(st) & (uc.mem)(1) = v) & (uc.pc = 2 => (uc.mem)(0) = max(st) & (uc.mem)(1) = v & max(st)<v) & (uc.pc = 3 => (uc.mem)(0) = max({v,max(st)})) VARIANT uc.end-uc.pc END);
  List_Substitution(Implementation(conditionalasmw),largest)==(res:=(uc.mem)(0))
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(conditionalasmw))==(?);
  Inherited_List_Constants(Implementation(conditionalasmw))==(?);
  List_Constants(Implementation(conditionalasmw))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Implementation(conditionalasmw))==(?);
  Context_List_Defered(Implementation(conditionalasmw))==(?);
  Context_List_Sets(Implementation(conditionalasmw))==(?);
  List_Own_Enumerated(Implementation(conditionalasmw))==(?);
  List_Valuable_Sets(Implementation(conditionalasmw))==(?);
  Inherited_List_Enumerated(Implementation(conditionalasmw))==(?);
  Inherited_List_Defered(Implementation(conditionalasmw))==(?);
  Inherited_List_Sets(Implementation(conditionalasmw))==(?);
  List_Enumerated(Implementation(conditionalasmw))==(?);
  List_Defered(Implementation(conditionalasmw))==(?);
  List_Sets(Implementation(conditionalasmw))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(conditionalasmw))==(?);
  Expanded_List_HiddenConstants(Implementation(conditionalasmw))==(?);
  List_HiddenConstants(Implementation(conditionalasmw))==(?);
  External_List_HiddenConstants(Implementation(conditionalasmw))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(conditionalasmw))==(btrue);
  Context_List_Properties(Implementation(conditionalasmw))==(uint32 : POW(INTEGER) & uint32 = 0..MAXINT & uint16 : POW(INTEGER) & uint16 = 0..65535);
  Inherited_List_Properties(Implementation(conditionalasmw))==(btrue);
  List_Properties(Implementation(conditionalasmw))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(conditionalasmw))==(aa : aa);
  List_Values(Implementation(conditionalasmw))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(conditionalasmw),Machine(types))==(?);
  Seen_Context_List_Enumerated(Implementation(conditionalasmw))==(?);
  Seen_Context_List_Invariant(Implementation(conditionalasmw))==(btrue);
  Seen_Context_List_Assertions(Implementation(conditionalasmw))==(btrue);
  Seen_Context_List_Properties(Implementation(conditionalasmw))==(btrue);
  Seen_List_Constraints(Implementation(conditionalasmw))==(btrue);
  Seen_List_Operations(Implementation(conditionalasmw),Machine(types))==(?);
  Seen_Expanded_List_Invariant(Implementation(conditionalasmw),Machine(types))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(conditionalasmw),Machine(ram))==(init,nop,set,inc,copy,testgt,testeq,goto,get_data,get_pc)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(conditionalasmw))==(Type(largest) == Cst(btype(INTEGER,?,?),No_type);Type(add) == Cst(No_type,btype(INTEGER,?,?)))
END
&
THEORY ListVisibleStaticX IS
  List_Constants(Implementation(conditionalasmw),Machine(types))==(uint32,uint16);
  List_Constants_Env(Implementation(conditionalasmw),Machine(types))==(Type(uint32) == Cst(SetOf(btype(INTEGER,"[uint32","]uint32")));Type(uint16) == Cst(SetOf(btype(INTEGER,"[uint16","]uint16"))))
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(conditionalasmw)) == (? | ? | ? | ? | add,largest | ? | seen(Machine(types)),imported(Machine(uc.ram)) | ? | conditionalasmw);
  List_Of_HiddenCst_Ids(Implementation(conditionalasmw)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(conditionalasmw)) == (?);
  List_Of_VisibleVar_Ids(Implementation(conditionalasmw)) == (? | ucmem,ucpc,ucend);
  List_Of_Ids_SeenBNU(Implementation(conditionalasmw)) == (seen(Machine(types)) : (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(ram)) == (? | ? | ? | ? | init,nop,set,inc,copy,testgt,testeq,goto,get_data,get_pc | ? | seen(Machine(types)) | ? | ram);
  List_Of_HiddenCst_Ids(Machine(ram)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ram)) == (?);
  List_Of_VisibleVar_Ids(Machine(ram)) == (end,pc,mem | ?);
  List_Of_Ids_SeenBNU(Machine(ram)) == (? : ?);
  List_Of_Ids(Machine(types)) == (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | types);
  List_Of_HiddenCst_Ids(Machine(types)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(types)) == (uint32,uint16);
  List_Of_VisibleVar_Ids(Machine(types)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(types)) == (? : ?)
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
  List_Local_Operations(Implementation(conditionalasmw))==(?)
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
  TypingPredicate(Implementation(conditionalasmw))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(conditionalasmw),Machine(uc.ram))==(?);
  ImportedVisVariablesList(Implementation(conditionalasmw),Machine(uc.ram))==(uc.mem,uc.pc,uc.end)
END
&
THEORY ListLocalOpInvariantX END
)
