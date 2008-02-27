Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(sequenceasm))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(sequenceasm))==(Machine(sequence));
  Level(Implementation(sequenceasm))==(1);
  Upper_Level(Implementation(sequenceasm))==(Machine(sequence))
END
&
THEORY LoadedStructureX IS
  Implementation(sequenceasm)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(sequenceasm))==(types)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(sequenceasm))==(uc.ram);
  Inherited_List_Includes(Implementation(sequenceasm))==(uc.ram)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(sequenceasm))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(sequenceasm))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(sequenceasm))==(?);
  Context_List_Variables(Implementation(sequenceasm))==(?);
  Abstract_List_Variables(Implementation(sequenceasm))==(b,a);
  Local_List_Variables(Implementation(sequenceasm))==(?);
  List_Variables(Implementation(sequenceasm))==(?);
  External_List_Variables(Implementation(sequenceasm))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(sequenceasm))==(?);
  Abstract_List_VisibleVariables(Implementation(sequenceasm))==(?);
  External_List_VisibleVariables(Implementation(sequenceasm))==(uc.end,uc.pc,uc.mem);
  Expanded_List_VisibleVariables(Implementation(sequenceasm))==(ucend,ucpc,ucmem);
  List_VisibleVariables(Implementation(sequenceasm))==(?);
  Internal_List_VisibleVariables(Implementation(sequenceasm))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(sequenceasm))==(btrue);
  Abstract_List_Invariant(Implementation(sequenceasm))==(a : uint32 & b : uint32);
  Expanded_List_Invariant(Implementation(sequenceasm))==(ucmem : NATURAL --> uint32 & ucpc : NATURAL & ucend : NATURAL & ucpc<=ucend);
  Context_List_Invariant(Implementation(sequenceasm))==(btrue);
  List_Invariant(Implementation(sequenceasm))==(a = ucmem(0) & b = ucmem(1))
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(sequenceasm))==(btrue);
  Expanded_List_Assertions(Implementation(sequenceasm))==(btrue);
  Context_List_Assertions(Implementation(sequenceasm))==(btrue);
  List_Assertions(Implementation(sequenceasm))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(sequenceasm))==(@(mem$0).(mem$0 : NATURAL --> uint32 ==> ucmem:=mem$0) || ucpc:=0 || @(end$0).(end$0 : NATURAL ==> ucend:=end$0));
  Context_List_Initialisation(Implementation(sequenceasm))==(skip);
  List_Initialisation(Implementation(sequenceasm))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(sequenceasm))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(sequenceasm),Machine(uc.ram))==(?);
  List_Instanciated_Parameters(Implementation(sequenceasm),Machine(types))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(sequenceasm),Machine(uc.ram))==(btrue);
  List_Constraints(Implementation(sequenceasm))==(btrue);
  List_Context_Constraints(Implementation(sequenceasm))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(sequenceasm))==(run);
  List_Operations(Implementation(sequenceasm))==(run)
END
&
THEORY ListInputX IS
  List_Input(Implementation(sequenceasm),run)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(sequenceasm),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(sequenceasm),run)==(run)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(sequenceasm),run)==(btrue);
  List_Precondition(Implementation(sequenceasm),run)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(sequenceasm),run)==(btrue | (3 : NATURAL | ucpc,ucend:=0,3);(1 : NATURAL & 2 : NATURAL & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{2|->ucmem(1)},ucpc+1);(0 : NATURAL & 1 : NATURAL & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{1|->ucmem(0)},ucpc+1);(2 : NATURAL & 0 : NATURAL & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{0|->ucmem(2)},ucpc+1));
  List_Substitution(Implementation(sequenceasm),run)==((uc.init)(3);(uc.move)(1,2);(uc.move)(0,1);(uc.move)(2,0))
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(sequenceasm))==(?);
  Inherited_List_Constants(Implementation(sequenceasm))==(?);
  List_Constants(Implementation(sequenceasm))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Implementation(sequenceasm))==(?);
  Context_List_Defered(Implementation(sequenceasm))==(?);
  Context_List_Sets(Implementation(sequenceasm))==(?);
  List_Own_Enumerated(Implementation(sequenceasm))==(?);
  List_Valuable_Sets(Implementation(sequenceasm))==(?);
  Inherited_List_Enumerated(Implementation(sequenceasm))==(?);
  Inherited_List_Defered(Implementation(sequenceasm))==(?);
  Inherited_List_Sets(Implementation(sequenceasm))==(?);
  List_Enumerated(Implementation(sequenceasm))==(?);
  List_Defered(Implementation(sequenceasm))==(?);
  List_Sets(Implementation(sequenceasm))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(sequenceasm))==(?);
  Expanded_List_HiddenConstants(Implementation(sequenceasm))==(?);
  List_HiddenConstants(Implementation(sequenceasm))==(?);
  External_List_HiddenConstants(Implementation(sequenceasm))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(sequenceasm))==(btrue);
  Context_List_Properties(Implementation(sequenceasm))==(uint32 : POW(INTEGER) & uint32 = 0..MAXINT & uint16 : POW(INTEGER) & uint16 = 0..65535);
  Inherited_List_Properties(Implementation(sequenceasm))==(btrue);
  List_Properties(Implementation(sequenceasm))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(sequenceasm))==(aa : aa);
  List_Values(Implementation(sequenceasm))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(sequenceasm),Machine(types))==(?);
  Seen_Context_List_Enumerated(Implementation(sequenceasm))==(?);
  Seen_Context_List_Invariant(Implementation(sequenceasm))==(btrue);
  Seen_Context_List_Assertions(Implementation(sequenceasm))==(btrue);
  Seen_Context_List_Properties(Implementation(sequenceasm))==(btrue);
  Seen_List_Constraints(Implementation(sequenceasm))==(btrue);
  Seen_List_Operations(Implementation(sequenceasm),Machine(types))==(?);
  Seen_Expanded_List_Invariant(Implementation(sequenceasm),Machine(types))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(sequenceasm),Machine(ram))==(init,nop,set,inc,move,testgt,testeq,goto,get_data,get_pc)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(sequenceasm))==(Type(run) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX IS
  List_Constants(Implementation(sequenceasm),Machine(types))==(uint32,uint16);
  List_Constants_Env(Implementation(sequenceasm),Machine(types))==(Type(uint32) == Cst(SetOf(btype(INTEGER,"[uint32","]uint32")));Type(uint16) == Cst(SetOf(btype(INTEGER,"[uint16","]uint16"))))
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(sequenceasm)) == (? | ? | ? | ? | run | ? | seen(Machine(types)),imported(Machine(uc.ram)) | ? | sequenceasm);
  List_Of_HiddenCst_Ids(Implementation(sequenceasm)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(sequenceasm)) == (?);
  List_Of_VisibleVar_Ids(Implementation(sequenceasm)) == (? | ucmem,ucpc,ucend);
  List_Of_Ids_SeenBNU(Implementation(sequenceasm)) == (seen(Machine(types)) : (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(ram)) == (? | ? | ? | ? | init,nop,set,inc,move,testgt,testeq,goto,get_data,get_pc | ? | seen(Machine(types)) | ? | ram);
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
  List_Local_Operations(Implementation(sequenceasm))==(?)
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
  TypingPredicate(Implementation(sequenceasm))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(sequenceasm),Machine(uc.ram))==(?);
  ImportedVisVariablesList(Implementation(sequenceasm),Machine(uc.ram))==(uc.mem,uc.pc,uc.end)
END
&
THEORY ListLocalOpInvariantX END
)
