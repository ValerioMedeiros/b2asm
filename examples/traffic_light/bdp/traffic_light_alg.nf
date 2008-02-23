Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(traffic_light_alg))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(traffic_light_alg))==(Machine(traffic_light));
  Level(Implementation(traffic_light_alg))==(1);
  Upper_Level(Implementation(traffic_light_alg))==(Machine(traffic_light))
END
&
THEORY LoadedStructureX IS
  Implementation(traffic_light_alg)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(traffic_light_alg))==(?)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(traffic_light_alg))==(state.VarNatural);
  Inherited_List_Includes(Implementation(traffic_light_alg))==(state.VarNatural)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(traffic_light_alg))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(traffic_light_alg))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(traffic_light_alg))==(?);
  Context_List_Variables(Implementation(traffic_light_alg))==(?);
  Abstract_List_Variables(Implementation(traffic_light_alg))==(state);
  Local_List_Variables(Implementation(traffic_light_alg))==(?);
  List_Variables(Implementation(traffic_light_alg))==(statevalue);
  External_List_Variables(Implementation(traffic_light_alg))==(state.value)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(traffic_light_alg))==(?);
  Abstract_List_VisibleVariables(Implementation(traffic_light_alg))==(?);
  External_List_VisibleVariables(Implementation(traffic_light_alg))==(?);
  Expanded_List_VisibleVariables(Implementation(traffic_light_alg))==(?);
  List_VisibleVariables(Implementation(traffic_light_alg))==(?);
  Internal_List_VisibleVariables(Implementation(traffic_light_alg))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(traffic_light_alg))==(btrue);
  Abstract_List_Invariant(Implementation(traffic_light_alg))==(state : 0..2);
  Expanded_List_Invariant(Implementation(traffic_light_alg))==(statevalue : NATURAL);
  Context_List_Invariant(Implementation(traffic_light_alg))==(btrue);
  List_Invariant(Implementation(traffic_light_alg))==(statevalue = state)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(traffic_light_alg))==(btrue);
  Expanded_List_Assertions(Implementation(traffic_light_alg))==(btrue);
  Context_List_Assertions(Implementation(traffic_light_alg))==(btrue);
  List_Assertions(Implementation(traffic_light_alg))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(traffic_light_alg))==(statevalue:=0;(0 : NATURAL | statevalue:=0));
  Context_List_Initialisation(Implementation(traffic_light_alg))==(skip);
  List_Initialisation(Implementation(traffic_light_alg))==((state.set)(0))
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(traffic_light_alg))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(traffic_light_alg),Machine(state.VarNatural))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(traffic_light_alg),Machine(state.VarNatural))==(btrue);
  List_Constraints(Implementation(traffic_light_alg))==(btrue);
  List_Context_Constraints(Implementation(traffic_light_alg))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(traffic_light_alg))==(advance);
  List_Operations(Implementation(traffic_light_alg))==(advance)
END
&
THEORY ListInputX IS
  List_Input(Implementation(traffic_light_alg),advance)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(traffic_light_alg),advance)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(traffic_light_alg),advance)==(advance)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(traffic_light_alg),advance)==(btrue);
  List_Precondition(Implementation(traffic_light_alg),advance)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(traffic_light_alg),advance)==(btrue | @tmp.((btrue | tmp:=statevalue);(tmp+1 : INT & tmp : INT & 1 : INT & (tmp+1) mod 3 : NAT & tmp+1 : NAT & 3 : NAT1 & (tmp+1) mod 3 : NATURAL | statevalue:=(tmp+1) mod 3)));
  List_Substitution(Implementation(traffic_light_alg),advance)==(VAR tmp IN tmp <-- state.get;(state.set)((tmp+1) mod 3) END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(traffic_light_alg))==(?);
  Inherited_List_Constants(Implementation(traffic_light_alg))==(?);
  List_Constants(Implementation(traffic_light_alg))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Implementation(traffic_light_alg))==(?);
  Context_List_Defered(Implementation(traffic_light_alg))==(?);
  Context_List_Sets(Implementation(traffic_light_alg))==(?);
  List_Own_Enumerated(Implementation(traffic_light_alg))==(?);
  List_Valuable_Sets(Implementation(traffic_light_alg))==(?);
  Inherited_List_Enumerated(Implementation(traffic_light_alg))==(?);
  Inherited_List_Defered(Implementation(traffic_light_alg))==(?);
  Inherited_List_Sets(Implementation(traffic_light_alg))==(?);
  List_Enumerated(Implementation(traffic_light_alg))==(?);
  List_Defered(Implementation(traffic_light_alg))==(?);
  List_Sets(Implementation(traffic_light_alg))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(traffic_light_alg))==(?);
  Expanded_List_HiddenConstants(Implementation(traffic_light_alg))==(?);
  List_HiddenConstants(Implementation(traffic_light_alg))==(?);
  External_List_HiddenConstants(Implementation(traffic_light_alg))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(traffic_light_alg))==(btrue);
  Context_List_Properties(Implementation(traffic_light_alg))==(btrue);
  Inherited_List_Properties(Implementation(traffic_light_alg))==(btrue);
  List_Properties(Implementation(traffic_light_alg))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(traffic_light_alg))==(aa : aa);
  List_Values(Implementation(traffic_light_alg))==(?)
END
&
THEORY ListSeenInfoX END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(traffic_light_alg),Machine(VarNatural))==(set,get)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(traffic_light_alg))==(Type(advance) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(traffic_light_alg)) == (? | ? | ? | statevalue | advance | ? | imported(Machine(state.VarNatural)) | ? | traffic_light_alg);
  List_Of_HiddenCst_Ids(Implementation(traffic_light_alg)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(traffic_light_alg)) == (?);
  List_Of_VisibleVar_Ids(Implementation(traffic_light_alg)) == (? | ?);
  List_Of_Ids_SeenBNU(Implementation(traffic_light_alg)) == (? : ?);
  List_Of_Ids(Machine(VarNatural)) == (? | ? | value | ? | set,get | ? | ? | ? | VarNatural);
  List_Of_HiddenCst_Ids(Machine(VarNatural)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(VarNatural)) == (?);
  List_Of_VisibleVar_Ids(Machine(VarNatural)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(VarNatural)) == (? : ?)
END
&
THEORY VariablesLocEnvX IS
  Variables_Loc(Implementation(traffic_light_alg),advance, 1) == (Type(tmp) == Lvl(btype(INTEGER,?,?)))
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
  List_Local_Operations(Implementation(traffic_light_alg))==(?)
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
  TypingPredicate(Implementation(traffic_light_alg))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(traffic_light_alg),Machine(state.VarNatural))==(state.value);
  ImportedVisVariablesList(Implementation(traffic_light_alg),Machine(state.VarNatural))==(?)
END
&
THEORY ListLocalOpInvariantX END
)
