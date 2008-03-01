Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(iterationalg))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(iterationalg))==(Machine(iteration));
  Level(Implementation(iterationalg))==(1);
  Upper_Level(Implementation(iterationalg))==(Machine(iteration))
END
&
THEORY LoadedStructureX IS
  Implementation(iterationalg)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(iterationalg))==(types)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(iterationalg))==(ai.nat,bi.nat,si.nat);
  Inherited_List_Includes(Implementation(iterationalg))==(si.nat,bi.nat,ai.nat)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(iterationalg))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(iterationalg))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(iterationalg))==(?);
  Context_List_Variables(Implementation(iterationalg))==(?);
  Abstract_List_Variables(Implementation(iterationalg))==(s,b,a);
  Local_List_Variables(Implementation(iterationalg))==(?);
  List_Variables(Implementation(iterationalg))==(?);
  External_List_Variables(Implementation(iterationalg))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(iterationalg))==(?);
  Abstract_List_VisibleVariables(Implementation(iterationalg))==(?);
  External_List_VisibleVariables(Implementation(iterationalg))==(ai.value,bi.value,si.value);
  Expanded_List_VisibleVariables(Implementation(iterationalg))==(aivalue,bivalue,sivalue);
  List_VisibleVariables(Implementation(iterationalg))==(?);
  Internal_List_VisibleVariables(Implementation(iterationalg))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(iterationalg))==(btrue);
  Abstract_List_Invariant(Implementation(iterationalg))==(a : uint32 & b : uint32 & s : uint32 & a+b : uint32 & s = a+b);
  Expanded_List_Invariant(Implementation(iterationalg))==(aivalue : uint32 & bivalue : uint32 & sivalue : uint32);
  Context_List_Invariant(Implementation(iterationalg))==(btrue);
  List_Invariant(Implementation(iterationalg))==(aivalue = a & bivalue = b & sivalue = s)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(iterationalg))==(btrue);
  Expanded_List_Assertions(Implementation(iterationalg))==(btrue);
  Context_List_Assertions(Implementation(iterationalg))==(btrue);
  List_Assertions(Implementation(iterationalg))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(iterationalg))==(@(value$0).(value$0 : uint32 ==> aivalue:=value$0);@(value$0).(value$0 : uint32 ==> bivalue:=value$0);@(value$0).(value$0 : uint32 ==> sivalue:=value$0);(0 : uint32 | aivalue:=0);(0 : uint32 | bivalue:=0);(0 : uint32 | sivalue:=0));
  Context_List_Initialisation(Implementation(iterationalg))==(skip);
  List_Initialisation(Implementation(iterationalg))==(BEGIN (ai.set)(0);(bi.set)(0);(si.set)(0) END)
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(iterationalg))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(iterationalg),Machine(ai.nat))==(?);
  List_Instanciated_Parameters(Implementation(iterationalg),Machine(bi.nat))==(?);
  List_Instanciated_Parameters(Implementation(iterationalg),Machine(si.nat))==(?);
  List_Instanciated_Parameters(Implementation(iterationalg),Machine(types))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(iterationalg),Machine(si.nat))==(btrue);
  List_Constraints(Implementation(iterationalg))==(btrue);
  List_Context_Constraints(Implementation(iterationalg))==(btrue);
  List_Constraints(Implementation(iterationalg),Machine(bi.nat))==(btrue);
  List_Constraints(Implementation(iterationalg),Machine(ai.nat))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(iterationalg))==(run);
  List_Operations(Implementation(iterationalg))==(run)
END
&
THEORY ListInputX IS
  List_Input(Implementation(iterationalg),run)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(iterationalg),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(iterationalg),run)==(run)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(iterationalg),run)==(btrue);
  List_Precondition(Implementation(iterationalg),run)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(iterationalg),run)==(btrue | @(sv,v).(sv:=aivalue;v:=0;WHILE v/=bivalue DO (sv+1 : INT & sv : INT & 1 : INT | sv:=sv+1);(v+1 : INT & v : INT & 1 : INT | v:=v+1) INVARIANT v : uint32 & v<=bivalue & sv = aivalue+v VARIANT bivalue-v END;(sv : uint32 | sivalue:=sv)));
  List_Substitution(Implementation(iterationalg),run)==(VAR sv,v IN sv:=ai.value;v:=0;WHILE v/=bi.value DO BEGIN sv:=sv+1;v:=v+1 END INVARIANT v : uint32 & v<=bi.value & sv = ai.value+v VARIANT bi.value-v END;(si.set)(sv) END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(iterationalg))==(?);
  Inherited_List_Constants(Implementation(iterationalg))==(?);
  List_Constants(Implementation(iterationalg))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Implementation(iterationalg))==(?);
  Context_List_Defered(Implementation(iterationalg))==(?);
  Context_List_Sets(Implementation(iterationalg))==(?);
  List_Own_Enumerated(Implementation(iterationalg))==(?);
  List_Valuable_Sets(Implementation(iterationalg))==(?);
  Inherited_List_Enumerated(Implementation(iterationalg))==(?);
  Inherited_List_Defered(Implementation(iterationalg))==(?);
  Inherited_List_Sets(Implementation(iterationalg))==(?);
  List_Enumerated(Implementation(iterationalg))==(?);
  List_Defered(Implementation(iterationalg))==(?);
  List_Sets(Implementation(iterationalg))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(iterationalg))==(?);
  Expanded_List_HiddenConstants(Implementation(iterationalg))==(?);
  List_HiddenConstants(Implementation(iterationalg))==(?);
  External_List_HiddenConstants(Implementation(iterationalg))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(iterationalg))==(btrue);
  Context_List_Properties(Implementation(iterationalg))==(uint32 : POW(INTEGER) & uint32 = 0..MAXINT & uint16 : POW(INTEGER) & uint16 = 0..65535);
  Inherited_List_Properties(Implementation(iterationalg))==(btrue);
  List_Properties(Implementation(iterationalg))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(iterationalg))==(aa : aa);
  List_Values(Implementation(iterationalg))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(iterationalg),Machine(types))==(?);
  Seen_Context_List_Enumerated(Implementation(iterationalg))==(?);
  Seen_Context_List_Invariant(Implementation(iterationalg))==(btrue);
  Seen_Context_List_Assertions(Implementation(iterationalg))==(btrue);
  Seen_Context_List_Properties(Implementation(iterationalg))==(btrue);
  Seen_List_Constraints(Implementation(iterationalg))==(btrue);
  Seen_List_Operations(Implementation(iterationalg),Machine(types))==(?);
  Seen_Expanded_List_Invariant(Implementation(iterationalg),Machine(types))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(iterationalg),Machine(nat))==(get,set)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(iterationalg))==(Type(run) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX IS
  List_Constants(Implementation(iterationalg),Machine(types))==(uint32,uint16);
  List_Constants_Env(Implementation(iterationalg),Machine(types))==(Type(uint32) == Cst(SetOf(btype(INTEGER,"[uint32","]uint32")));Type(uint16) == Cst(SetOf(btype(INTEGER,"[uint16","]uint16"))))
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(iterationalg)) == (? | ? | ? | ? | run | ? | seen(Machine(types)),imported(Machine(ai.nat)),imported(Machine(bi.nat)),imported(Machine(si.nat)) | ? | iterationalg);
  List_Of_HiddenCst_Ids(Implementation(iterationalg)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(iterationalg)) == (?);
  List_Of_VisibleVar_Ids(Implementation(iterationalg)) == (? | sivalue,bivalue,aivalue);
  List_Of_Ids_SeenBNU(Implementation(iterationalg)) == (seen(Machine(types)) : (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | ?));
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
  Variables_Loc(Implementation(iterationalg),run, 1) == (Type(sv) == Lvl(btype(INTEGER,?,?));Type(v) == Lvl(btype(INTEGER,?,?)))
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
  List_Local_Operations(Implementation(iterationalg))==(?)
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
  TypingPredicate(Implementation(iterationalg))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(iterationalg),Machine(ai.nat))==(?);
  ImportedVisVariablesList(Implementation(iterationalg),Machine(ai.nat))==(ai.value);
  ImportedVariablesList(Implementation(iterationalg),Machine(bi.nat))==(?);
  ImportedVisVariablesList(Implementation(iterationalg),Machine(bi.nat))==(bi.value);
  ImportedVariablesList(Implementation(iterationalg),Machine(si.nat))==(?);
  ImportedVisVariablesList(Implementation(iterationalg),Machine(si.nat))==(si.value)
END
&
THEORY ListLocalOpInvariantX END
)
