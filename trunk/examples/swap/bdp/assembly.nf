Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(assembly))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(assembly))==(Machine(swap));
  Level(Implementation(assembly))==(1);
  Upper_Level(Implementation(assembly))==(Machine(swap))
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
  Abstract_List_Variables(Implementation(assembly))==(b,a);
  Local_List_Variables(Implementation(assembly))==(?);
  List_Variables(Implementation(assembly))==(ucend,ucpc,ucmem);
  External_List_Variables(Implementation(assembly))==(uc.end,uc.pc,uc.mem)
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
  Abstract_List_Invariant(Implementation(assembly))==(a : INTEGER & b : INTEGER & a = b);
  Expanded_List_Invariant(Implementation(assembly))==(ucmem : NATURAL --> NATURAL & ucpc : NATURAL & ucend : NATURAL & ucpc<=ucend);
  Context_List_Invariant(Implementation(assembly))==(btrue);
  List_Invariant(Implementation(assembly))==(a = ucmem(0) & b = ucmem(1))
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
  Expanded_List_Initialisation(Implementation(assembly))==(@(mem$0).(mem$0 : NATURAL --> NATURAL ==> ucmem:=mem$0) || ucpc:=0 || ucend:=0;(0 : NATURAL & 0 : NATURAL | ucmem:=ucmem<+{0|->0});(1 : NATURAL & 0 : NATURAL | ucmem:=ucmem<+{1|->0}));
  Context_List_Initialisation(Implementation(assembly))==(skip);
  List_Initialisation(Implementation(assembly))==(BEGIN (uc.init_data)(0,0);(uc.init_data)(1,0) END)
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
  List_Input(Implementation(assembly),run)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(assembly),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(assembly),run)==(run)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(assembly),run)==(btrue);
  List_Precondition(Implementation(assembly),run)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(assembly),run)==(btrue | (0 : NATURAL & 4 : NATURAL & 0<=4 | ucpc,ucend:=0,4);(0 : NATURAL & 2 : NATURAL & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{2|->ucmem(0)},ucpc+1);(1 : NATURAL & 3 : NATURAL & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{3|->ucmem(1)},ucpc+1);(3 : NATURAL & 0 : NATURAL & ucpc+1<=ucend | ucmem,ucpc:=ucmem<+{0|->ucmem(3)},ucpc+1));
  List_Substitution(Implementation(assembly),run)==((uc.init)(0,4);(uc.move)(0,2);(uc.move)(1,3);(uc.move)(3,0))
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
  List_Included_Operations(Implementation(assembly),Machine(microcontroller))==(init_data,init,get_data,set_end,nop,goto,testgt,testeq,move,set_data,inc)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(assembly))==(Type(run) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(assembly)) == (? | ? | ? | ucmem,ucpc,ucend | run | ? | imported(Machine(uc.microcontroller)) | ? | assembly);
  List_Of_HiddenCst_Ids(Implementation(assembly)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(assembly)) == (?);
  List_Of_VisibleVar_Ids(Implementation(assembly)) == (? | ?);
  List_Of_Ids_SeenBNU(Implementation(assembly)) == (? : ?);
  List_Of_Ids(Machine(microcontroller)) == (? | ? | end,pc,mem | ? | init_data,init,get_data,set_end,nop,goto,testgt,testeq,move,set_data,inc | ? | ? | ? | microcontroller);
  List_Of_HiddenCst_Ids(Machine(microcontroller)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(microcontroller)) == (?);
  List_Of_VisibleVar_Ids(Machine(microcontroller)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(microcontroller)) == (? : ?)
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
  ImportedVariablesList(Implementation(assembly),Machine(uc.microcontroller))==(uc.mem,uc.pc,uc.end);
  ImportedVisVariablesList(Implementation(assembly),Machine(uc.microcontroller))==(?)
END
&
THEORY ListLocalOpInvariantX END
)
