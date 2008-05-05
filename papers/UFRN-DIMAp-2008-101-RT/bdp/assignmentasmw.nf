Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(assignmentasmw))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(assignmentasmw))==(Machine(assignment));
  Level(Implementation(assignmentasmw))==(1);
  Upper_Level(Implementation(assignmentasmw))==(Machine(assignment))
END
&
THEORY LoadedStructureX IS
  Implementation(assignmentasmw)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(assignmentasmw))==(types)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(assignmentasmw))==(uc.ram);
  Inherited_List_Includes(Implementation(assignmentasmw))==(uc.ram)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(assignmentasmw))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(assignmentasmw))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(assignmentasmw))==(?);
  Context_List_Variables(Implementation(assignmentasmw))==(?);
  Abstract_List_Variables(Implementation(assignmentasmw))==(v);
  Local_List_Variables(Implementation(assignmentasmw))==(?);
  List_Variables(Implementation(assignmentasmw))==(?);
  External_List_Variables(Implementation(assignmentasmw))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(assignmentasmw))==(?);
  Abstract_List_VisibleVariables(Implementation(assignmentasmw))==(?);
  External_List_VisibleVariables(Implementation(assignmentasmw))==(uc.end,uc.pc,uc.mem);
  Expanded_List_VisibleVariables(Implementation(assignmentasmw))==(ucend,ucpc,ucmem);
  List_VisibleVariables(Implementation(assignmentasmw))==(?);
  Internal_List_VisibleVariables(Implementation(assignmentasmw))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(assignmentasmw))==(btrue);
  Abstract_List_Invariant(Implementation(assignmentasmw))==(v : uint32);
  Expanded_List_Invariant(Implementation(assignmentasmw))==(ucmem : NATURAL --> uint32 & ucpc : NATURAL & ucend : NATURAL & ucpc<=ucend);
  Context_List_Invariant(Implementation(assignmentasmw))==(btrue);
  List_Invariant(Implementation(assignmentasmw))==(v = ucmem(0))
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(assignmentasmw))==(btrue);
  Expanded_List_Assertions(Implementation(assignmentasmw))==(btrue);
  Context_List_Assertions(Implementation(assignmentasmw))==(btrue);
  List_Assertions(Implementation(assignmentasmw))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(assignmentasmw))==(@(mem$0).(mem$0 : NATURAL --> uint32 ==> ucmem:=mem$0) || ucpc:=0 || @(end$0).(end$0 : NATURAL ==> ucend:=end$0);(0 : NATURAL | ucpc,ucend:=0,0));
  Context_List_Initialisation(Implementation(assignmentasmw))==(skip);
  List_Initialisation(Implementation(assignmentasmw))==((uc.init)(0))
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(assignmentasmw))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(assignmentasmw),Machine(uc.ram))==(?);
  List_Instanciated_Parameters(Implementation(assignmentasmw),Machine(types))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(assignmentasmw),Machine(uc.ram))==(btrue);
  List_Constraints(Implementation(assignmentasmw))==(btrue);
  List_Context_Constraints(Implementation(assignmentasmw))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(assignmentasmw))==(run);
  List_Operations(Implementation(assignmentasmw))==(run)
END
&
THEORY ListInputX IS
  List_Input(Implementation(assignmentasmw),run)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(assignmentasmw),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(assignmentasmw),run)==(run)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(assignmentasmw),run)==(btrue);
  List_Precondition(Implementation(assignmentasmw),run)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(assignmentasmw),run)==(btrue | (1 : NATURAL | ucpc,ucend:=0,1);WHILE ucpc<ucend DO ucpc = 0 ==> (0 : NATURAL & 0 : uint32 & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{0|->0},ucpc+1) [] not(ucpc = 0) ==> skip INVARIANT 0<=ucpc & ucpc<=ucend & (ucpc = 1 => ucmem(0) = 0) VARIANT ucend-ucpc END);
  List_Substitution(Implementation(assignmentasmw),run)==((uc.init)(1);WHILE uc.pc<uc.end DO BEGIN CASE uc.pc OF EITHER 0 THEN (uc.set)(0,0) END END END INVARIANT 0<=uc.pc & uc.pc<=uc.end & (uc.pc = 1 => (uc.mem)(0) = 0) VARIANT uc.end-uc.pc END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(assignmentasmw))==(?);
  Inherited_List_Constants(Implementation(assignmentasmw))==(?);
  List_Constants(Implementation(assignmentasmw))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Implementation(assignmentasmw))==(?);
  Context_List_Defered(Implementation(assignmentasmw))==(?);
  Context_List_Sets(Implementation(assignmentasmw))==(?);
  List_Own_Enumerated(Implementation(assignmentasmw))==(?);
  List_Valuable_Sets(Implementation(assignmentasmw))==(?);
  Inherited_List_Enumerated(Implementation(assignmentasmw))==(?);
  Inherited_List_Defered(Implementation(assignmentasmw))==(?);
  Inherited_List_Sets(Implementation(assignmentasmw))==(?);
  List_Enumerated(Implementation(assignmentasmw))==(?);
  List_Defered(Implementation(assignmentasmw))==(?);
  List_Sets(Implementation(assignmentasmw))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(assignmentasmw))==(?);
  Expanded_List_HiddenConstants(Implementation(assignmentasmw))==(?);
  List_HiddenConstants(Implementation(assignmentasmw))==(?);
  External_List_HiddenConstants(Implementation(assignmentasmw))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(assignmentasmw))==(btrue);
  Context_List_Properties(Implementation(assignmentasmw))==(uint32 : POW(INTEGER) & uint32 = 0..MAXINT & uint16 : POW(INTEGER) & uint16 = 0..65535);
  Inherited_List_Properties(Implementation(assignmentasmw))==(btrue);
  List_Properties(Implementation(assignmentasmw))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(assignmentasmw))==(aa : aa);
  List_Values(Implementation(assignmentasmw))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(assignmentasmw),Machine(types))==(?);
  Seen_Context_List_Enumerated(Implementation(assignmentasmw))==(?);
  Seen_Context_List_Invariant(Implementation(assignmentasmw))==(btrue);
  Seen_Context_List_Assertions(Implementation(assignmentasmw))==(btrue);
  Seen_Context_List_Properties(Implementation(assignmentasmw))==(btrue);
  Seen_List_Constraints(Implementation(assignmentasmw))==(btrue);
  Seen_List_Operations(Implementation(assignmentasmw),Machine(types))==(?);
  Seen_Expanded_List_Invariant(Implementation(assignmentasmw),Machine(types))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(assignmentasmw),Machine(ram))==(init,nop,set,inc,copy,testgt,testeq,goto,get_data,get_pc)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(assignmentasmw))==(Type(run) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX IS
  List_Constants(Implementation(assignmentasmw),Machine(types))==(uint32,uint16);
  List_Constants_Env(Implementation(assignmentasmw),Machine(types))==(Type(uint32) == Cst(SetOf(btype(INTEGER,"[uint32","]uint32")));Type(uint16) == Cst(SetOf(btype(INTEGER,"[uint16","]uint16"))))
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(assignmentasmw)) == (? | ? | ? | ? | run | ? | seen(Machine(types)),imported(Machine(uc.ram)) | ? | assignmentasmw);
  List_Of_HiddenCst_Ids(Implementation(assignmentasmw)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(assignmentasmw)) == (?);
  List_Of_VisibleVar_Ids(Implementation(assignmentasmw)) == (? | ucmem,ucpc,ucend);
  List_Of_Ids_SeenBNU(Implementation(assignmentasmw)) == (seen(Machine(types)) : (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | ?));
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
  List_Local_Operations(Implementation(assignmentasmw))==(?)
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
  TypingPredicate(Implementation(assignmentasmw))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(assignmentasmw),Machine(uc.ram))==(?);
  ImportedVisVariablesList(Implementation(assignmentasmw),Machine(uc.ram))==(uc.mem,uc.pc,uc.end)
END
&
THEORY ListLocalOpInvariantX END
)
