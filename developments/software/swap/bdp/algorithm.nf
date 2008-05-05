Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(algorithm))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(algorithm))==(Machine(swap));
  Level(Implementation(algorithm))==(1);
  Upper_Level(Implementation(algorithm))==(Machine(swap))
END
&
THEORY LoadedStructureX IS
  Implementation(algorithm)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(algorithm))==(?)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(algorithm))==(ai.int,bi.int);
  Inherited_List_Includes(Implementation(algorithm))==(bi.int,ai.int)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(algorithm))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(algorithm))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(algorithm))==(?);
  Context_List_Variables(Implementation(algorithm))==(?);
  Abstract_List_Variables(Implementation(algorithm))==(b,a);
  Local_List_Variables(Implementation(algorithm))==(?);
  List_Variables(Implementation(algorithm))==(aivalue,bivalue);
  External_List_Variables(Implementation(algorithm))==(ai.value,bi.value)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(algorithm))==(?);
  Abstract_List_VisibleVariables(Implementation(algorithm))==(?);
  External_List_VisibleVariables(Implementation(algorithm))==(?);
  Expanded_List_VisibleVariables(Implementation(algorithm))==(?);
  List_VisibleVariables(Implementation(algorithm))==(?);
  Internal_List_VisibleVariables(Implementation(algorithm))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(algorithm))==(btrue);
  Abstract_List_Invariant(Implementation(algorithm))==(a : INTEGER & b : INTEGER & a = b);
  Expanded_List_Invariant(Implementation(algorithm))==(aivalue : INTEGER & bivalue : INTEGER);
  Context_List_Invariant(Implementation(algorithm))==(btrue);
  List_Invariant(Implementation(algorithm))==(btrue)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(algorithm))==(btrue);
  Expanded_List_Assertions(Implementation(algorithm))==(btrue);
  Context_List_Assertions(Implementation(algorithm))==(btrue);
  List_Assertions(Implementation(algorithm))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(algorithm))==(@(value$0).(value$0 : INTEGER ==> aivalue:=value$0);@(value$0).(value$0 : INTEGER ==> bivalue:=value$0));
  Context_List_Initialisation(Implementation(algorithm))==(skip);
  List_Initialisation(Implementation(algorithm))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(algorithm))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(algorithm),Machine(ai.int))==(?);
  List_Instanciated_Parameters(Implementation(algorithm),Machine(bi.int))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(algorithm),Machine(bi.int))==(btrue);
  List_Constraints(Implementation(algorithm))==(btrue);
  List_Context_Constraints(Implementation(algorithm))==(btrue);
  List_Constraints(Implementation(algorithm),Machine(ai.int))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(algorithm))==(run);
  List_Operations(Implementation(algorithm))==(run)
END
&
THEORY ListInputX IS
  List_Input(Implementation(algorithm),run)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(algorithm),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(algorithm),run)==(run)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(algorithm),run)==(btrue);
  List_Precondition(Implementation(algorithm),run)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(algorithm),run)==(btrue | @(va,vb).((btrue | va:=aivalue);(btrue | vb:=bivalue);(vb : INTEGER | aivalue:=vb);(va : INTEGER | bivalue:=va)));
  List_Substitution(Implementation(algorithm),run)==(VAR va,vb IN va <-- ai.get;vb <-- bi.get;(ai.set)(vb);(bi.set)(va) END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(algorithm))==(?);
  Inherited_List_Constants(Implementation(algorithm))==(?);
  List_Constants(Implementation(algorithm))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Implementation(algorithm))==(?);
  Context_List_Defered(Implementation(algorithm))==(?);
  Context_List_Sets(Implementation(algorithm))==(?);
  List_Own_Enumerated(Implementation(algorithm))==(?);
  List_Valuable_Sets(Implementation(algorithm))==(?);
  Inherited_List_Enumerated(Implementation(algorithm))==(?);
  Inherited_List_Defered(Implementation(algorithm))==(?);
  Inherited_List_Sets(Implementation(algorithm))==(?);
  List_Enumerated(Implementation(algorithm))==(?);
  List_Defered(Implementation(algorithm))==(?);
  List_Sets(Implementation(algorithm))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(algorithm))==(?);
  Expanded_List_HiddenConstants(Implementation(algorithm))==(?);
  List_HiddenConstants(Implementation(algorithm))==(?);
  External_List_HiddenConstants(Implementation(algorithm))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(algorithm))==(btrue);
  Context_List_Properties(Implementation(algorithm))==(btrue);
  Inherited_List_Properties(Implementation(algorithm))==(btrue);
  List_Properties(Implementation(algorithm))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(algorithm))==(aa : aa);
  List_Values(Implementation(algorithm))==(?)
END
&
THEORY ListSeenInfoX END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(algorithm),Machine(int))==(get,set)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(algorithm))==(Type(run) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(algorithm)) == (? | ? | ? | bivalue,aivalue | run | ? | imported(Machine(ai.int)),imported(Machine(bi.int)) | ? | algorithm);
  List_Of_HiddenCst_Ids(Implementation(algorithm)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(algorithm)) == (?);
  List_Of_VisibleVar_Ids(Implementation(algorithm)) == (? | ?);
  List_Of_Ids_SeenBNU(Implementation(algorithm)) == (? : ?);
  List_Of_Ids(Machine(int)) == (? | ? | value | ? | get,set | ? | ? | ? | int);
  List_Of_HiddenCst_Ids(Machine(int)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(int)) == (?);
  List_Of_VisibleVar_Ids(Machine(int)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(int)) == (? : ?)
END
&
THEORY VariablesLocEnvX IS
  Variables_Loc(Implementation(algorithm),run, 1) == (Type(va) == Lvl(btype(INTEGER,?,?));Type(vb) == Lvl(btype(INTEGER,?,?)))
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
  List_Local_Operations(Implementation(algorithm))==(?)
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
  TypingPredicate(Implementation(algorithm))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(algorithm),Machine(ai.int))==(ai.value);
  ImportedVisVariablesList(Implementation(algorithm),Machine(ai.int))==(?);
  ImportedVariablesList(Implementation(algorithm),Machine(bi.int))==(bi.value);
  ImportedVisVariablesList(Implementation(algorithm),Machine(bi.int))==(?)
END
&
THEORY ListLocalOpInvariantX END
)
