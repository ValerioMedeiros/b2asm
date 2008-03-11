Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(traffic_light_alg))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(traffic_light_alg))==(Machine(traffic_light));
  Level(Implementation(traffic_light_alg))==(2);
  Upper_Level(Implementation(traffic_light_alg))==(Refinement(traffic_light_data_refinement))
END
&
THEORY LoadedStructureX IS
  Implementation(traffic_light_alg)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(traffic_light_alg))==(definitions,data_refinement)
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
  Abstract_List_Variables(Implementation(traffic_light_alg))==(counter,color);
  Local_List_Variables(Implementation(traffic_light_alg))==(?);
  List_Variables(Implementation(traffic_light_alg))==(?);
  External_List_Variables(Implementation(traffic_light_alg))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(traffic_light_alg))==(?);
  Abstract_List_VisibleVariables(Implementation(traffic_light_alg))==(?);
  External_List_VisibleVariables(Implementation(traffic_light_alg))==(state.value);
  Expanded_List_VisibleVariables(Implementation(traffic_light_alg))==(statevalue);
  List_VisibleVariables(Implementation(traffic_light_alg))==(?);
  Internal_List_VisibleVariables(Implementation(traffic_light_alg))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(traffic_light_alg))==(btrue);
  Abstract_List_Invariant(Implementation(traffic_light_alg))==(counter : NATURAL & counter : 0..2 & counter = color_refine(color) & color : COLOR);
  Expanded_List_Invariant(Implementation(traffic_light_alg))==(statevalue : NATURAL);
  Context_List_Invariant(Implementation(traffic_light_alg))==(btrue);
  List_Invariant(Implementation(traffic_light_alg))==(statevalue : 0..2 & statevalue = counter)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(traffic_light_alg))==(btrue);
  Expanded_List_Assertions(Implementation(traffic_light_alg))==(btrue);
  Context_List_Assertions(Implementation(traffic_light_alg))==(!c.(c : COLOR => c = green or c = yellow or c = red) & color_refine(green) = 0 & color_refine(yellow) = 1 & color_refine(red) = 2 & color_step(0) = 1 & color_step(1) = 2 & color_step(2) = 0);
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
  List_Instanciated_Parameters(Implementation(traffic_light_alg),Machine(state.VarNatural))==(?);
  List_Instanciated_Parameters(Implementation(traffic_light_alg),Machine(definitions))==(?);
  List_Instanciated_Parameters(Implementation(traffic_light_alg),Machine(data_refinement))==(?)
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
  Expanded_List_Substitution(Implementation(traffic_light_alg),advance)==(btrue | statevalue = 0 ==> (1 : NATURAL | statevalue:=1) [] not(statevalue = 0) ==> (statevalue = 1 ==> (2 : NATURAL | statevalue:=2) [] not(statevalue = 1) ==> (0 : NATURAL | statevalue:=0)));
  List_Substitution(Implementation(traffic_light_alg),advance)==(IF state.value = 0 THEN (state.set)(1) ELSIF state.value = 1 THEN (state.set)(2) ELSE (state.set)(0) END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(traffic_light_alg))==(?);
  Inherited_List_Constants(Implementation(traffic_light_alg))==(?);
  List_Constants(Implementation(traffic_light_alg))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Implementation(traffic_light_alg),COLOR)==({green,yellow,red});
  Context_List_Enumerated(Implementation(traffic_light_alg))==(COLOR);
  Context_List_Defered(Implementation(traffic_light_alg))==(?);
  Context_List_Sets(Implementation(traffic_light_alg))==(COLOR);
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
  Context_List_Properties(Implementation(traffic_light_alg))==(not(COLOR = {}) & color_refine : COLOR --> NATURAL & color_refine = {green|->0,yellow|->1,red|->2} & color_step : 0..2 --> 0..2 & color_step = {0|->1,1|->2,2|->0});
  Inherited_List_Properties(Implementation(traffic_light_alg))==(btrue);
  List_Properties(Implementation(traffic_light_alg))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(traffic_light_alg))==(aa : aa);
  List_Values(Implementation(traffic_light_alg))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(traffic_light_alg),Machine(data_refinement))==(?);
  Seen_Context_List_Enumerated(Implementation(traffic_light_alg))==(COLOR);
  Seen_Context_List_Invariant(Implementation(traffic_light_alg))==(btrue);
  Seen_Context_List_Assertions(Implementation(traffic_light_alg))==(!c.(c : COLOR => c = green or c = yellow or c = red));
  Seen_Context_List_Properties(Implementation(traffic_light_alg))==(not(COLOR = {}));
  Seen_List_Constraints(Implementation(traffic_light_alg))==(btrue);
  Seen_List_Operations(Implementation(traffic_light_alg),Machine(data_refinement))==(?);
  Seen_Expanded_List_Invariant(Implementation(traffic_light_alg),Machine(data_refinement))==(btrue);
  Set_Definition(Implementation(traffic_light_alg),COLOR)==({green,yellow,red});
  Seen_Internal_List_Operations(Implementation(traffic_light_alg),Machine(definitions))==(?);
  Seen_List_Operations(Implementation(traffic_light_alg),Machine(definitions))==(?);
  Seen_Expanded_List_Invariant(Implementation(traffic_light_alg),Machine(definitions))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(traffic_light_alg),Machine(VarNatural))==(set,get)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(traffic_light_alg))==(Type(advance) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX IS
  List_Constants(Implementation(traffic_light_alg),Machine(data_refinement))==(color_refine,color_step);
  List_Constants_Env(Implementation(traffic_light_alg),Machine(data_refinement))==(Type(color_refine) == Cst(SetOf(etype(COLOR,0,2)*btype(INTEGER,?,?)));Type(color_step) == Cst(SetOf(btype(INTEGER,0,2)*btype(INTEGER,0,2))));
  List_Constants_Env(Implementation(traffic_light_alg),Machine(definitions))==(Type(green) == Cst(etype(COLOR,0,2));Type(yellow) == Cst(etype(COLOR,0,2));Type(red) == Cst(etype(COLOR,0,2)));
  Enumerate_Definition(Implementation(traffic_light_alg),Machine(definitions),COLOR)==({green,yellow,red})
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(traffic_light_alg)) == (? | ? | ? | ? | advance | ? | seen(Machine(definitions)),seen(Machine(data_refinement)),imported(Machine(state.VarNatural)) | ? | traffic_light_alg);
  List_Of_HiddenCst_Ids(Implementation(traffic_light_alg)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(traffic_light_alg)) == (?);
  List_Of_VisibleVar_Ids(Implementation(traffic_light_alg)) == (? | statevalue);
  List_Of_Ids_SeenBNU(Implementation(traffic_light_alg)) == (? : ?);
  List_Of_Ids(Machine(VarNatural)) == (? | ? | ? | ? | set,get | ? | ? | ? | VarNatural);
  List_Of_HiddenCst_Ids(Machine(VarNatural)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(VarNatural)) == (?);
  List_Of_VisibleVar_Ids(Machine(VarNatural)) == (value | ?);
  List_Of_Ids_SeenBNU(Machine(VarNatural)) == (? : ?);
  List_Of_Ids(Machine(data_refinement)) == (color_refine,color_step | ? | ? | ? | ? | ? | seen(Machine(definitions)) | ? | data_refinement);
  List_Of_HiddenCst_Ids(Machine(data_refinement)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(data_refinement)) == (color_refine,color_step);
  List_Of_VisibleVar_Ids(Machine(data_refinement)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(data_refinement)) == (? : ?);
  List_Of_Ids(Machine(definitions)) == (COLOR,green,yellow,red | ? | ? | ? | ? | ? | ? | ? | definitions);
  List_Of_HiddenCst_Ids(Machine(definitions)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(definitions)) == (?);
  List_Of_VisibleVar_Ids(Machine(definitions)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(definitions)) == (? : ?)
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
  ImportedVariablesList(Implementation(traffic_light_alg),Machine(state.VarNatural))==(?);
  ImportedVisVariablesList(Implementation(traffic_light_alg),Machine(state.VarNatural))==(state.value)
END
&
THEORY ListLocalOpInvariantX END
)
