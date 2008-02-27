Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(sequencealg))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(sequencealg))==(Machine(sequence));
  Level(Implementation(sequencealg))==(1);
  Upper_Level(Implementation(sequencealg))==(Machine(sequence))
END
&
THEORY LoadedStructureX IS
  Implementation(sequencealg)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(sequencealg))==(types)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(sequencealg))==(ai.nat,bi.nat);
  Inherited_List_Includes(Implementation(sequencealg))==(bi.nat,ai.nat)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(sequencealg))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(sequencealg))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(sequencealg))==(?);
  Context_List_Variables(Implementation(sequencealg))==(?);
  Abstract_List_Variables(Implementation(sequencealg))==(b,a);
  Local_List_Variables(Implementation(sequencealg))==(?);
  List_Variables(Implementation(sequencealg))==(?);
  External_List_Variables(Implementation(sequencealg))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(sequencealg))==(?);
  Abstract_List_VisibleVariables(Implementation(sequencealg))==(?);
  External_List_VisibleVariables(Implementation(sequencealg))==(ai.value,bi.value);
  Expanded_List_VisibleVariables(Implementation(sequencealg))==(aivalue,bivalue);
  List_VisibleVariables(Implementation(sequencealg))==(?);
  Internal_List_VisibleVariables(Implementation(sequencealg))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(sequencealg))==(btrue);
  Abstract_List_Invariant(Implementation(sequencealg))==(a : uint32 & b : uint32);
  Expanded_List_Invariant(Implementation(sequencealg))==(aivalue : uint32 & bivalue : uint32);
  Context_List_Invariant(Implementation(sequencealg))==(btrue);
  List_Invariant(Implementation(sequencealg))==(a = aivalue & b = bivalue)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(sequencealg))==(btrue);
  Expanded_List_Assertions(Implementation(sequencealg))==(btrue);
  Context_List_Assertions(Implementation(sequencealg))==(btrue);
  List_Assertions(Implementation(sequencealg))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(sequencealg))==(@(value$0).(value$0 : uint32 ==> aivalue:=value$0);@(value$0).(value$0 : uint32 ==> bivalue:=value$0));
  Context_List_Initialisation(Implementation(sequencealg))==(skip);
  List_Initialisation(Implementation(sequencealg))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(sequencealg))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(sequencealg),Machine(ai.nat))==(?);
  List_Instanciated_Parameters(Implementation(sequencealg),Machine(bi.nat))==(?);
  List_Instanciated_Parameters(Implementation(sequencealg),Machine(types))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(sequencealg),Machine(bi.nat))==(btrue);
  List_Constraints(Implementation(sequencealg))==(btrue);
  List_Context_Constraints(Implementation(sequencealg))==(btrue);
  List_Constraints(Implementation(sequencealg),Machine(ai.nat))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(sequencealg))==(run);
  List_Operations(Implementation(sequencealg))==(run)
END
&
THEORY ListInputX IS
  List_Input(Implementation(sequencealg),run)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(sequencealg),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(sequencealg),run)==(run)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(sequencealg),run)==(btrue);
  List_Precondition(Implementation(sequencealg),run)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(sequencealg),run)==(btrue | @tmp.(tmp:=bivalue;(aivalue : uint32 | bivalue:=aivalue);(tmp : uint32 | aivalue:=tmp)));
  List_Substitution(Implementation(sequencealg),run)==(VAR tmp IN tmp:=bi.value;(bi.set)(ai.value);(ai.set)(tmp) END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(sequencealg))==(?);
  Inherited_List_Constants(Implementation(sequencealg))==(?);
  List_Constants(Implementation(sequencealg))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Implementation(sequencealg))==(?);
  Context_List_Defered(Implementation(sequencealg))==(?);
  Context_List_Sets(Implementation(sequencealg))==(?);
  List_Own_Enumerated(Implementation(sequencealg))==(?);
  List_Valuable_Sets(Implementation(sequencealg))==(?);
  Inherited_List_Enumerated(Implementation(sequencealg))==(?);
  Inherited_List_Defered(Implementation(sequencealg))==(?);
  Inherited_List_Sets(Implementation(sequencealg))==(?);
  List_Enumerated(Implementation(sequencealg))==(?);
  List_Defered(Implementation(sequencealg))==(?);
  List_Sets(Implementation(sequencealg))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(sequencealg))==(?);
  Expanded_List_HiddenConstants(Implementation(sequencealg))==(?);
  List_HiddenConstants(Implementation(sequencealg))==(?);
  External_List_HiddenConstants(Implementation(sequencealg))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(sequencealg))==(btrue);
  Context_List_Properties(Implementation(sequencealg))==(uint32 : POW(INTEGER) & uint32 = 0..MAXINT & uint16 : POW(INTEGER) & uint16 = 0..65535);
  Inherited_List_Properties(Implementation(sequencealg))==(btrue);
  List_Properties(Implementation(sequencealg))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(sequencealg))==(aa : aa);
  List_Values(Implementation(sequencealg))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(sequencealg),Machine(types))==(?);
  Seen_Context_List_Enumerated(Implementation(sequencealg))==(?);
  Seen_Context_List_Invariant(Implementation(sequencealg))==(btrue);
  Seen_Context_List_Assertions(Implementation(sequencealg))==(btrue);
  Seen_Context_List_Properties(Implementation(sequencealg))==(btrue);
  Seen_List_Constraints(Implementation(sequencealg))==(btrue);
  Seen_List_Operations(Implementation(sequencealg),Machine(types))==(?);
  Seen_Expanded_List_Invariant(Implementation(sequencealg),Machine(types))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(sequencealg),Machine(nat))==(get,set)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(sequencealg))==(Type(run) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX IS
  List_Constants(Implementation(sequencealg),Machine(types))==(uint32,uint16);
  List_Constants_Env(Implementation(sequencealg),Machine(types))==(Type(uint32) == Cst(SetOf(btype(INTEGER,"[uint32","]uint32")));Type(uint16) == Cst(SetOf(btype(INTEGER,"[uint16","]uint16"))))
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(sequencealg)) == (? | ? | ? | ? | run | ? | seen(Machine(types)),imported(Machine(ai.nat)),imported(Machine(bi.nat)) | ? | sequencealg);
  List_Of_HiddenCst_Ids(Implementation(sequencealg)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(sequencealg)) == (?);
  List_Of_VisibleVar_Ids(Implementation(sequencealg)) == (? | bivalue,aivalue);
  List_Of_Ids_SeenBNU(Implementation(sequencealg)) == (seen(Machine(types)) : (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(nat)) == (? | ? | ? | ? | get,set | ? | seen(Machine(types)) | ? | nat);
  List_Of_HiddenCst_Ids(Machine(nat)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(nat)) == (?);
  List_Of_VisibleVar_Ids(Machine(nat)) == (value | ?);
  List_Of_Ids_SeenBNU(Machine(nat)) == (? : ?);
  List_Of_Ids(Machine(types)) == (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | types);
  List_Of_HiddenCst_Ids(Machine(types)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(types)) == (uint32,uint16);
  List_Of_VisibleVar_Ids(Machine(types)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(types)) == (? : ?)
END
&
THEORY VariablesLocEnvX IS
  Variables_Loc(Implementation(sequencealg),run, 1) == (Type(tmp) == Lvl(btype(INTEGER,?,?)))
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
  List_Local_Operations(Implementation(sequencealg))==(?)
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
  TypingPredicate(Implementation(sequencealg))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(sequencealg),Machine(ai.nat))==(?);
  ImportedVisVariablesList(Implementation(sequencealg),Machine(ai.nat))==(ai.value);
  ImportedVariablesList(Implementation(sequencealg),Machine(bi.nat))==(?);
  ImportedVisVariablesList(Implementation(sequencealg),Machine(bi.nat))==(bi.value)
END
&
THEORY ListLocalOpInvariantX END
)
