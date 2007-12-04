Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(assignmentasm))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(assignmentasm))==(Machine(assignment));
  Level(Implementation(assignmentasm))==(1);
  Upper_Level(Implementation(assignmentasm))==(Machine(assignment))
END
&
THEORY LoadedStructureX IS
  Implementation(assignmentasm)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(assignmentasm))==(types)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(assignmentasm))==(uc.ram);
  Inherited_List_Includes(Implementation(assignmentasm))==(uc.ram)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(assignmentasm))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(assignmentasm))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(assignmentasm))==(?);
  Context_List_Variables(Implementation(assignmentasm))==(?);
  Abstract_List_Variables(Implementation(assignmentasm))==(v);
  Local_List_Variables(Implementation(assignmentasm))==(?);
  List_Variables(Implementation(assignmentasm))==(ucend,ucpc,ucmem);
  External_List_Variables(Implementation(assignmentasm))==(uc.end,uc.pc,uc.mem)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(assignmentasm))==(?);
  Abstract_List_VisibleVariables(Implementation(assignmentasm))==(?);
  External_List_VisibleVariables(Implementation(assignmentasm))==(?);
  Expanded_List_VisibleVariables(Implementation(assignmentasm))==(?);
  List_VisibleVariables(Implementation(assignmentasm))==(?);
  Internal_List_VisibleVariables(Implementation(assignmentasm))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(assignmentasm))==(btrue);
  Abstract_List_Invariant(Implementation(assignmentasm))==(v : uint32);
  Expanded_List_Invariant(Implementation(assignmentasm))==(ucmem : NATURAL --> uint32 & ucpc : NATURAL & ucend : NATURAL & ucpc<=ucend);
  Context_List_Invariant(Implementation(assignmentasm))==(btrue);
  List_Invariant(Implementation(assignmentasm))==(v = ucmem(0))
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(assignmentasm))==(btrue);
  Expanded_List_Assertions(Implementation(assignmentasm))==(btrue);
  Context_List_Assertions(Implementation(assignmentasm))==(btrue);
  List_Assertions(Implementation(assignmentasm))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(assignmentasm))==(@(mem$0).(mem$0 : NATURAL --> uint32 ==> ucmem:=mem$0) || ucpc:=0 || @(end$0).(end$0 : NATURAL ==> ucend:=end$0);(0 : NATURAL | ucpc,ucend:=0,0));
  Context_List_Initialisation(Implementation(assignmentasm))==(skip);
  List_Initialisation(Implementation(assignmentasm))==((uc.init)(0))
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(assignmentasm))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(assignmentasm),Machine(uc.ram))==(?);
  List_Instanciated_Parameters(Implementation(assignmentasm),Machine(types))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(assignmentasm),Machine(uc.ram))==(btrue);
  List_Constraints(Implementation(assignmentasm))==(btrue);
  List_Context_Constraints(Implementation(assignmentasm))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(assignmentasm))==(run);
  List_Operations(Implementation(assignmentasm))==(run)
END
&
THEORY ListInputX IS
  List_Input(Implementation(assignmentasm),run)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(assignmentasm),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(assignmentasm),run)==(run)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(assignmentasm),run)==(btrue);
  List_Precondition(Implementation(assignmentasm),run)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(assignmentasm),run)==(btrue | (1 : NATURAL | ucpc,ucend:=0,1);(0 : NATURAL & 0 : uint32 & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{0|->0},ucpc+1));
  List_Substitution(Implementation(assignmentasm),run)==((uc.init)(1);(uc.set)(0,0))
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(assignmentasm))==(?);
  Inherited_List_Constants(Implementation(assignmentasm))==(?);
  List_Constants(Implementation(assignmentasm))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Implementation(assignmentasm))==(?);
  Context_List_Defered(Implementation(assignmentasm))==(?);
  Context_List_Sets(Implementation(assignmentasm))==(?);
  List_Own_Enumerated(Implementation(assignmentasm))==(?);
  List_Valuable_Sets(Implementation(assignmentasm))==(?);
  Inherited_List_Enumerated(Implementation(assignmentasm))==(?);
  Inherited_List_Defered(Implementation(assignmentasm))==(?);
  Inherited_List_Sets(Implementation(assignmentasm))==(?);
  List_Enumerated(Implementation(assignmentasm))==(?);
  List_Defered(Implementation(assignmentasm))==(?);
  List_Sets(Implementation(assignmentasm))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(assignmentasm))==(?);
  Expanded_List_HiddenConstants(Implementation(assignmentasm))==(?);
  List_HiddenConstants(Implementation(assignmentasm))==(?);
  External_List_HiddenConstants(Implementation(assignmentasm))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(assignmentasm))==(btrue);
  Context_List_Properties(Implementation(assignmentasm))==(uint32 : POW(INTEGER) & uint32 = 0..MAXINT & uint16 : POW(INTEGER) & uint16 = 0..65535);
  Inherited_List_Properties(Implementation(assignmentasm))==(btrue);
  List_Properties(Implementation(assignmentasm))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(assignmentasm))==(aa : aa);
  List_Values(Implementation(assignmentasm))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(assignmentasm),Machine(types))==(?);
  Seen_Context_List_Enumerated(Implementation(assignmentasm))==(?);
  Seen_Context_List_Invariant(Implementation(assignmentasm))==(btrue);
  Seen_Context_List_Assertions(Implementation(assignmentasm))==(btrue);
  Seen_Context_List_Properties(Implementation(assignmentasm))==(btrue);
  Seen_List_Constraints(Implementation(assignmentasm))==(btrue);
  Seen_List_Operations(Implementation(assignmentasm),Machine(types))==(?);
  Seen_Expanded_List_Invariant(Implementation(assignmentasm),Machine(types))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(assignmentasm),Machine(ram))==(init,nop,set,inc,move,testgt,testeq,goto,get_data,get_pc)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(assignmentasm))==(Type(run) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX IS
  List_Constants(Implementation(assignmentasm),Machine(types))==(uint32,uint16);
  List_Constants_Env(Implementation(assignmentasm),Machine(types))==(Type(uint32) == Cst(SetOf(btype(INTEGER,"[uint32","]uint32")));Type(uint16) == Cst(SetOf(btype(INTEGER,"[uint16","]uint16"))))
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(assignmentasm)) == (? | ? | ? | ucmem,ucpc,ucend | run | ? | seen(Machine(types)),imported(Machine(uc.ram)) | ? | assignmentasm);
  List_Of_HiddenCst_Ids(Implementation(assignmentasm)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(assignmentasm)) == (?);
  List_Of_VisibleVar_Ids(Implementation(assignmentasm)) == (? | ?);
  List_Of_Ids_SeenBNU(Implementation(assignmentasm)) == (seen(Machine(types)) : (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(ram)) == (? | ? | end,pc,mem | ? | init,nop,set,inc,move,testgt,testeq,goto,get_data,get_pc | ? | seen(Machine(types)) | ? | ram);
  List_Of_HiddenCst_Ids(Machine(ram)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ram)) == (?);
  List_Of_VisibleVar_Ids(Machine(ram)) == (? | ?);
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
  List_Local_Operations(Implementation(assignmentasm))==(?)
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
  TypingPredicate(Implementation(assignmentasm))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(assignmentasm),Machine(uc.ram))==(uc.mem,uc.pc,uc.end);
  ImportedVisVariablesList(Implementation(assignmentasm),Machine(uc.ram))==(?)
END
&
THEORY ListLocalOpInvariantX END
)
