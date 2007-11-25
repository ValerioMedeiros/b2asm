Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(algorithm))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(algorithm))==(Machine(addition));
  Level(Implementation(algorithm))==(1);
  Upper_Level(Implementation(algorithm))==(Machine(addition))
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
  List_Includes(Implementation(algorithm))==(ar.int,br.int,sr.int);
  Inherited_List_Includes(Implementation(algorithm))==(sr.int,br.int,ar.int)
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
  Abstract_List_Variables(Implementation(algorithm))==(s,b,a);
  Local_List_Variables(Implementation(algorithm))==(?);
  List_Variables(Implementation(algorithm))==(arvalue,brvalue,srvalue);
  External_List_Variables(Implementation(algorithm))==(ar.value,br.value,sr.value)
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
  Abstract_List_Invariant(Implementation(algorithm))==(a : NATURAL & b : NATURAL & s : NATURAL & a+b<=MAXINT & s = a+b);
  Expanded_List_Invariant(Implementation(algorithm))==(arvalue : INTEGER & brvalue : INTEGER & srvalue : INTEGER);
  Context_List_Invariant(Implementation(algorithm))==(btrue);
  List_Invariant(Implementation(algorithm))==(arvalue = a & brvalue = b & srvalue = s)
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
  Expanded_List_Initialisation(Implementation(algorithm))==(@(value$0).(value$0 : INTEGER ==> arvalue:=value$0);@(value$0).(value$0 : INTEGER ==> brvalue:=value$0);@(value$0).(value$0 : INTEGER ==> srvalue:=value$0);(0 : INTEGER | arvalue:=0);(0 : INTEGER | brvalue:=0);(0 : INTEGER | srvalue:=0));
  Context_List_Initialisation(Implementation(algorithm))==(skip);
  List_Initialisation(Implementation(algorithm))==(BEGIN (ar.set)(0);(br.set)(0);(sr.set)(0) END)
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(algorithm))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(algorithm),Machine(ar.int))==(?);
  List_Instanciated_Parameters(Implementation(algorithm),Machine(br.int))==(?);
  List_Instanciated_Parameters(Implementation(algorithm),Machine(sr.int))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(algorithm),Machine(sr.int))==(btrue);
  List_Constraints(Implementation(algorithm))==(btrue);
  List_Context_Constraints(Implementation(algorithm))==(btrue);
  List_Constraints(Implementation(algorithm),Machine(br.int))==(btrue);
  List_Constraints(Implementation(algorithm),Machine(ar.int))==(btrue)
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
  Expanded_List_Substitution(Implementation(algorithm),run)==(btrue | @(sv,av,bv,v).((btrue | av:=arvalue);(btrue | bv:=brvalue);sv:=av;v:=0;WHILE v/=bv DO (sv+1 : INT & sv : INT & 1 : INT | sv:=sv+1);(v+1 : INT & v : INT & 1 : INT | v:=v+1) INVARIANT v : INTEGER & 0<=v & v<=bv & sv = av+v VARIANT bv-v END;(sv : INTEGER | srvalue:=sv)));
  List_Substitution(Implementation(algorithm),run)==(VAR sv,av,bv,v IN BEGIN av <-- ar.get;bv <-- br.get;sv:=av;v:=0;WHILE v/=bv DO BEGIN sv:=sv+1;v:=v+1 END INVARIANT v : INTEGER & 0<=v & v<=bv & sv = av+v VARIANT bv-v END;(sr.set)(sv) END END)
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
  List_Of_Ids(Implementation(algorithm)) == (? | ? | ? | srvalue,brvalue,arvalue | run | ? | imported(Machine(ar.int)),imported(Machine(br.int)),imported(Machine(sr.int)) | ? | algorithm);
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
  Variables_Loc(Implementation(algorithm),run, 1) == (Type(sv) == Lvl(btype(INTEGER,?,?));Type(av) == Lvl(btype(INTEGER,?,?));Type(bv) == Lvl(btype(INTEGER,?,?));Type(v) == Lvl(btype(INTEGER,?,?)))
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
  ImportedVariablesList(Implementation(algorithm),Machine(ar.int))==(ar.value);
  ImportedVisVariablesList(Implementation(algorithm),Machine(ar.int))==(?);
  ImportedVariablesList(Implementation(algorithm),Machine(br.int))==(br.value);
  ImportedVisVariablesList(Implementation(algorithm),Machine(br.int))==(?);
  ImportedVariablesList(Implementation(algorithm),Machine(sr.int))==(sr.value);
  ImportedVisVariablesList(Implementation(algorithm),Machine(sr.int))==(?)
END
&
THEORY ListLocalOpInvariantX END
)
