Normalised(
THEORY MagicNumberX IS
  MagicNumber(Refinement(traffic_light_data_refinement))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Refinement(traffic_light_data_refinement))==(Machine(traffic_light));
  Level(Refinement(traffic_light_data_refinement))==(1);
  Upper_Level(Refinement(traffic_light_data_refinement))==(Machine(traffic_light))
END
&
THEORY LoadedStructureX IS
  Refinement(traffic_light_data_refinement)
END
&
THEORY ListSeesX IS
  List_Sees(Refinement(traffic_light_data_refinement))==(definitions,data_refinement)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Refinement(traffic_light_data_refinement))==(?);
  List_Includes(Refinement(traffic_light_data_refinement))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Refinement(traffic_light_data_refinement))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Refinement(traffic_light_data_refinement))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Refinement(traffic_light_data_refinement))==(?);
  Context_List_Variables(Refinement(traffic_light_data_refinement))==(?);
  Abstract_List_Variables(Refinement(traffic_light_data_refinement))==(color);
  Local_List_Variables(Refinement(traffic_light_data_refinement))==(counter);
  List_Variables(Refinement(traffic_light_data_refinement))==(counter);
  External_List_Variables(Refinement(traffic_light_data_refinement))==(counter)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Refinement(traffic_light_data_refinement))==(?);
  Abstract_List_VisibleVariables(Refinement(traffic_light_data_refinement))==(?);
  External_List_VisibleVariables(Refinement(traffic_light_data_refinement))==(?);
  Expanded_List_VisibleVariables(Refinement(traffic_light_data_refinement))==(?);
  List_VisibleVariables(Refinement(traffic_light_data_refinement))==(?);
  Internal_List_VisibleVariables(Refinement(traffic_light_data_refinement))==(?)
END
&
THEORY ListOfNewVariablesX IS
  List_Of_New_Variables(Refinement(traffic_light_data_refinement))==(counter)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Refinement(traffic_light_data_refinement))==(btrue);
  Expanded_List_Invariant(Refinement(traffic_light_data_refinement))==(btrue);
  Abstract_List_Invariant(Refinement(traffic_light_data_refinement))==(color : COLOR);
  Context_List_Invariant(Refinement(traffic_light_data_refinement))==(btrue);
  List_Invariant(Refinement(traffic_light_data_refinement))==(counter : NATURAL & counter : 0..2 & counter = color_refine(color))
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Refinement(traffic_light_data_refinement))==(btrue);
  Abstract_List_Assertions(Refinement(traffic_light_data_refinement))==(btrue);
  Context_List_Assertions(Refinement(traffic_light_data_refinement))==(!c.(c : COLOR => c = green or c = yellow or c = red) & color_refine(green) = 0 & color_refine(yellow) = 1 & color_refine(red) = 2 & color_step(0) = 1 & color_step(1) = 2 & color_step(2) = 0);
  List_Assertions(Refinement(traffic_light_data_refinement))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Refinement(traffic_light_data_refinement))==(counter:=0);
  Context_List_Initialisation(Refinement(traffic_light_data_refinement))==(skip);
  List_Initialisation(Refinement(traffic_light_data_refinement))==(counter:=0)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Refinement(traffic_light_data_refinement))==(advance);
  List_Operations(Refinement(traffic_light_data_refinement))==(advance)
END
&
THEORY ListInputX IS
  List_Input(Refinement(traffic_light_data_refinement),advance)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Refinement(traffic_light_data_refinement),advance)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Refinement(traffic_light_data_refinement),advance)==(advance)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Refinement(traffic_light_data_refinement),advance)==(btrue);
  List_Precondition(Refinement(traffic_light_data_refinement),advance)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Refinement(traffic_light_data_refinement),advance)==(btrue | counter:=color_step(counter));
  List_Substitution(Refinement(traffic_light_data_refinement),advance)==(counter:=color_step(counter))
END
&
THEORY ListParametersX IS
  List_Parameters(Refinement(traffic_light_data_refinement))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Refinement(traffic_light_data_refinement),Machine(definitions))==(?);
  List_Instanciated_Parameters(Refinement(traffic_light_data_refinement),Machine(data_refinement))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Refinement(traffic_light_data_refinement))==(btrue);
  List_Context_Constraints(Refinement(traffic_light_data_refinement))==(btrue)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Refinement(traffic_light_data_refinement))==(?);
  Inherited_List_Constants(Refinement(traffic_light_data_refinement))==(?);
  List_Constants(Refinement(traffic_light_data_refinement))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Refinement(traffic_light_data_refinement),COLOR)==({green,yellow,red});
  Context_List_Enumerated(Refinement(traffic_light_data_refinement))==(COLOR);
  Context_List_Defered(Refinement(traffic_light_data_refinement))==(?);
  Context_List_Sets(Refinement(traffic_light_data_refinement))==(COLOR);
  List_Valuable_Sets(Refinement(traffic_light_data_refinement))==(?);
  Inherited_List_Enumerated(Refinement(traffic_light_data_refinement))==(?);
  Inherited_List_Defered(Refinement(traffic_light_data_refinement))==(?);
  Inherited_List_Sets(Refinement(traffic_light_data_refinement))==(?);
  List_Enumerated(Refinement(traffic_light_data_refinement))==(?);
  List_Defered(Refinement(traffic_light_data_refinement))==(?);
  List_Sets(Refinement(traffic_light_data_refinement))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Refinement(traffic_light_data_refinement))==(?);
  Expanded_List_HiddenConstants(Refinement(traffic_light_data_refinement))==(?);
  List_HiddenConstants(Refinement(traffic_light_data_refinement))==(?);
  External_List_HiddenConstants(Refinement(traffic_light_data_refinement))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Refinement(traffic_light_data_refinement))==(btrue);
  Context_List_Properties(Refinement(traffic_light_data_refinement))==(not(COLOR = {}) & color_refine : COLOR --> NATURAL & color_refine = {green|->0,yellow|->1,red|->2} & color_step : 0..2 --> 0..2 & color_step = {0|->1,1|->2,2|->0});
  Inherited_List_Properties(Refinement(traffic_light_data_refinement))==(btrue);
  List_Properties(Refinement(traffic_light_data_refinement))==(btrue)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Refinement(traffic_light_data_refinement),Machine(data_refinement))==(?);
  Seen_Context_List_Enumerated(Refinement(traffic_light_data_refinement))==(COLOR);
  Seen_Context_List_Invariant(Refinement(traffic_light_data_refinement))==(btrue);
  Seen_Context_List_Assertions(Refinement(traffic_light_data_refinement))==(!c.(c : COLOR => c = green or c = yellow or c = red));
  Seen_Context_List_Properties(Refinement(traffic_light_data_refinement))==(not(COLOR = {}));
  Seen_List_Constraints(Refinement(traffic_light_data_refinement))==(btrue);
  Seen_List_Operations(Refinement(traffic_light_data_refinement),Machine(data_refinement))==(?);
  Seen_Expanded_List_Invariant(Refinement(traffic_light_data_refinement),Machine(data_refinement))==(btrue);
  Set_Definition(Refinement(traffic_light_data_refinement),COLOR)==({green,yellow,red});
  Seen_Internal_List_Operations(Refinement(traffic_light_data_refinement),Machine(definitions))==(?);
  Seen_List_Operations(Refinement(traffic_light_data_refinement),Machine(definitions))==(?);
  Seen_Expanded_List_Invariant(Refinement(traffic_light_data_refinement),Machine(definitions))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Refinement(traffic_light_data_refinement)) == (? | ? | counter | ? | advance | ? | seen(Machine(definitions)),seen(Machine(data_refinement)) | ? | traffic_light_data_refinement);
  List_Of_HiddenCst_Ids(Refinement(traffic_light_data_refinement)) == (? | ?);
  List_Of_VisibleCst_Ids(Refinement(traffic_light_data_refinement)) == (?);
  List_Of_VisibleVar_Ids(Refinement(traffic_light_data_refinement)) == (? | ?);
  List_Of_Ids_SeenBNU(Refinement(traffic_light_data_refinement)) == (? : ?);
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
THEORY VariablesEnvX IS
  Variables(Refinement(traffic_light_data_refinement)) == (Type(counter) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Refinement(traffic_light_data_refinement)) == (Type(advance) == Cst(No_type,No_type))
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO
END
)
