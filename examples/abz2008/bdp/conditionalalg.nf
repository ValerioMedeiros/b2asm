Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(conditionalalg))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(conditionalalg))==(Machine(conditional));
  Level(Implementation(conditionalalg))==(1);
  Upper_Level(Implementation(conditionalalg))==(Machine(conditional))
END
&
THEORY LoadedStructureX IS
  Implementation(conditionalalg)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(conditionalalg))==(types)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(conditionalalg))==(mi.nat);
  Inherited_List_Includes(Implementation(conditionalalg))==(mi.nat)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(conditionalalg))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(conditionalalg))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(conditionalalg))==(?);
  Context_List_Variables(Implementation(conditionalalg))==(?);
  Abstract_List_Variables(Implementation(conditionalalg))==(st);
  Local_List_Variables(Implementation(conditionalalg))==(?);
  List_Variables(Implementation(conditionalalg))==(?);
  External_List_Variables(Implementation(conditionalalg))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(conditionalalg))==(?);
  Abstract_List_VisibleVariables(Implementation(conditionalalg))==(?);
  External_List_VisibleVariables(Implementation(conditionalalg))==(mi.value);
  Expanded_List_VisibleVariables(Implementation(conditionalalg))==(mivalue);
  List_VisibleVariables(Implementation(conditionalalg))==(?);
  Internal_List_VisibleVariables(Implementation(conditionalalg))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(conditionalalg))==(btrue);
  Abstract_List_Invariant(Implementation(conditionalalg))==(st : POW(uint32));
  Expanded_List_Invariant(Implementation(conditionalalg))==(mivalue : uint32);
  Context_List_Invariant(Implementation(conditionalalg))==(btrue);
  List_Invariant(Implementation(conditionalalg))==(max(st) = mivalue)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(conditionalalg))==(btrue);
  Expanded_List_Assertions(Implementation(conditionalalg))==(btrue);
  Context_List_Assertions(Implementation(conditionalalg))==(btrue);
  List_Assertions(Implementation(conditionalalg))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(conditionalalg))==(@(value$0).(value$0 : uint32 ==> mivalue:=value$0);(0 : uint32 | mivalue:=0));
  Context_List_Initialisation(Implementation(conditionalalg))==(skip);
  List_Initialisation(Implementation(conditionalalg))==(BEGIN (mi.set)(0) END)
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(conditionalalg))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(conditionalalg),Machine(mi.nat))==(?);
  List_Instanciated_Parameters(Implementation(conditionalalg),Machine(types))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(conditionalalg),Machine(mi.nat))==(btrue);
  List_Constraints(Implementation(conditionalalg))==(btrue);
  List_Context_Constraints(Implementation(conditionalalg))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(conditionalalg))==(add,largest);
  List_Operations(Implementation(conditionalalg))==(add,largest)
END
&
THEORY ListInputX IS
  List_Input(Implementation(conditionalalg),add)==(v);
  List_Input(Implementation(conditionalalg),largest)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(conditionalalg),add)==(?);
  List_Output(Implementation(conditionalalg),largest)==(res)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(conditionalalg),add)==(add(v));
  List_Header(Implementation(conditionalalg),largest)==(res <-- largest)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(conditionalalg),add)==(btrue);
  List_Precondition(Implementation(conditionalalg),add)==(v : uint32 & v/:st);
  Own_Precondition(Implementation(conditionalalg),largest)==(btrue);
  List_Precondition(Implementation(conditionalalg),largest)==(st/={})
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(conditionalalg),largest)==(st/={} & mivalue : INT | res:=mivalue);
  Expanded_List_Substitution(Implementation(conditionalalg),add)==(v : uint32 & v/:st | mivalue<v ==> (v : uint32 | mivalue:=v) [] not(mivalue<v) ==> skip);
  List_Substitution(Implementation(conditionalalg),add)==(IF mi.value<v THEN (mi.set)(v) END);
  List_Substitution(Implementation(conditionalalg),largest)==(res:=mi.value)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(conditionalalg))==(?);
  Inherited_List_Constants(Implementation(conditionalalg))==(?);
  List_Constants(Implementation(conditionalalg))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Implementation(conditionalalg))==(?);
  Context_List_Defered(Implementation(conditionalalg))==(?);
  Context_List_Sets(Implementation(conditionalalg))==(?);
  List_Own_Enumerated(Implementation(conditionalalg))==(?);
  List_Valuable_Sets(Implementation(conditionalalg))==(?);
  Inherited_List_Enumerated(Implementation(conditionalalg))==(?);
  Inherited_List_Defered(Implementation(conditionalalg))==(?);
  Inherited_List_Sets(Implementation(conditionalalg))==(?);
  List_Enumerated(Implementation(conditionalalg))==(?);
  List_Defered(Implementation(conditionalalg))==(?);
  List_Sets(Implementation(conditionalalg))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(conditionalalg))==(?);
  Expanded_List_HiddenConstants(Implementation(conditionalalg))==(?);
  List_HiddenConstants(Implementation(conditionalalg))==(?);
  External_List_HiddenConstants(Implementation(conditionalalg))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(conditionalalg))==(btrue);
  Context_List_Properties(Implementation(conditionalalg))==(uint32 : POW(INTEGER) & uint32 = 0..MAXINT & uint16 : POW(INTEGER) & uint16 = 0..65535);
  Inherited_List_Properties(Implementation(conditionalalg))==(btrue);
  List_Properties(Implementation(conditionalalg))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(conditionalalg))==(aa : aa);
  List_Values(Implementation(conditionalalg))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(conditionalalg),Machine(types))==(?);
  Seen_Context_List_Enumerated(Implementation(conditionalalg))==(?);
  Seen_Context_List_Invariant(Implementation(conditionalalg))==(btrue);
  Seen_Context_List_Assertions(Implementation(conditionalalg))==(btrue);
  Seen_Context_List_Properties(Implementation(conditionalalg))==(btrue);
  Seen_List_Constraints(Implementation(conditionalalg))==(btrue);
  Seen_List_Operations(Implementation(conditionalalg),Machine(types))==(?);
  Seen_Expanded_List_Invariant(Implementation(conditionalalg),Machine(types))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(conditionalalg),Machine(nat))==(get,set)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(conditionalalg))==(Type(largest) == Cst(btype(INTEGER,?,?),No_type);Type(add) == Cst(No_type,btype(INTEGER,?,?)))
END
&
THEORY ListVisibleStaticX IS
  List_Constants(Implementation(conditionalalg),Machine(types))==(uint32,uint16);
  List_Constants_Env(Implementation(conditionalalg),Machine(types))==(Type(uint32) == Cst(SetOf(btype(INTEGER,"[uint32","]uint32")));Type(uint16) == Cst(SetOf(btype(INTEGER,"[uint16","]uint16"))))
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(conditionalalg)) == (? | ? | ? | ? | add,largest | ? | seen(Machine(types)),imported(Machine(mi.nat)) | ? | conditionalalg);
  List_Of_HiddenCst_Ids(Implementation(conditionalalg)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(conditionalalg)) == (?);
  List_Of_VisibleVar_Ids(Implementation(conditionalalg)) == (? | mivalue);
  List_Of_Ids_SeenBNU(Implementation(conditionalalg)) == (seen(Machine(types)) : (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | ?));
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
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO
END
&
THEORY ListLocalOperationsX IS
  List_Local_Operations(Implementation(conditionalalg))==(?)
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
  TypingPredicate(Implementation(conditionalalg))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(conditionalalg),Machine(mi.nat))==(?);
  ImportedVisVariablesList(Implementation(conditionalalg),Machine(mi.nat))==(mi.value)
END
&
THEORY ListLocalOpInvariantX END
)
