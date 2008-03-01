Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(data_refinement))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(data_refinement))==(Machine(data_refinement));
  Level(Machine(data_refinement))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(data_refinement)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(data_refinement))==(definitions)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(data_refinement))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(data_refinement))==(?);
  List_Includes(Machine(data_refinement))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(data_refinement))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(data_refinement))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(data_refinement))==(?);
  Context_List_Variables(Machine(data_refinement))==(?);
  Abstract_List_Variables(Machine(data_refinement))==(?);
  Local_List_Variables(Machine(data_refinement))==(?);
  List_Variables(Machine(data_refinement))==(?);
  External_List_Variables(Machine(data_refinement))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(data_refinement))==(?);
  Abstract_List_VisibleVariables(Machine(data_refinement))==(?);
  External_List_VisibleVariables(Machine(data_refinement))==(?);
  Expanded_List_VisibleVariables(Machine(data_refinement))==(?);
  List_VisibleVariables(Machine(data_refinement))==(?);
  Internal_List_VisibleVariables(Machine(data_refinement))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(data_refinement))==(btrue);
  Gluing_List_Invariant(Machine(data_refinement))==(btrue);
  Expanded_List_Invariant(Machine(data_refinement))==(btrue);
  Abstract_List_Invariant(Machine(data_refinement))==(btrue);
  Context_List_Invariant(Machine(data_refinement))==(btrue);
  List_Invariant(Machine(data_refinement))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(data_refinement))==(btrue);
  Abstract_List_Assertions(Machine(data_refinement))==(btrue);
  Context_List_Assertions(Machine(data_refinement))==(!c.(c : COLOR => c = green or c = yellow or c = red));
  List_Assertions(Machine(data_refinement))==(color_refine(green) = 0 & color_refine(yellow) = 1 & color_refine(red) = 2)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(data_refinement))==(skip);
  Context_List_Initialisation(Machine(data_refinement))==(skip);
  List_Initialisation(Machine(data_refinement))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(data_refinement))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(data_refinement),Machine(definitions))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(data_refinement))==(btrue);
  List_Constraints(Machine(data_refinement))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(data_refinement))==(?);
  List_Operations(Machine(data_refinement))==(?)
END
&
THEORY ListInputX END
&
THEORY ListOutputX END
&
THEORY ListHeaderX END
&
THEORY ListPreconditionX END
&
THEORY ListSubstitutionX END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(data_refinement))==(color_refine);
  Inherited_List_Constants(Machine(data_refinement))==(?);
  List_Constants(Machine(data_refinement))==(color_refine)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(data_refinement),COLOR)==({green,yellow,red});
  Context_List_Enumerated(Machine(data_refinement))==(COLOR);
  Context_List_Defered(Machine(data_refinement))==(?);
  Context_List_Sets(Machine(data_refinement))==(COLOR);
  List_Valuable_Sets(Machine(data_refinement))==(?);
  Inherited_List_Enumerated(Machine(data_refinement))==(?);
  Inherited_List_Defered(Machine(data_refinement))==(?);
  Inherited_List_Sets(Machine(data_refinement))==(?);
  List_Enumerated(Machine(data_refinement))==(?);
  List_Defered(Machine(data_refinement))==(?);
  List_Sets(Machine(data_refinement))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(data_refinement))==(?);
  Expanded_List_HiddenConstants(Machine(data_refinement))==(?);
  List_HiddenConstants(Machine(data_refinement))==(?);
  External_List_HiddenConstants(Machine(data_refinement))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(data_refinement))==(btrue);
  Context_List_Properties(Machine(data_refinement))==(not(COLOR = {}));
  Inherited_List_Properties(Machine(data_refinement))==(btrue);
  List_Properties(Machine(data_refinement))==(color_refine : COLOR --> NATURAL & color_refine = {green|->0,yellow|->1,red|->2})
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(data_refinement),Machine(definitions))==(?);
  Seen_Context_List_Enumerated(Machine(data_refinement))==(?);
  Seen_Context_List_Invariant(Machine(data_refinement))==(btrue);
  Seen_Context_List_Assertions(Machine(data_refinement))==(btrue);
  Seen_Context_List_Properties(Machine(data_refinement))==(btrue);
  Seen_List_Constraints(Machine(data_refinement))==(btrue);
  Seen_List_Operations(Machine(data_refinement),Machine(definitions))==(?);
  Seen_Expanded_List_Invariant(Machine(data_refinement),Machine(definitions))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(data_refinement)) == (color_refine | ? | ? | ? | ? | ? | seen(Machine(definitions)) | ? | data_refinement);
  List_Of_HiddenCst_Ids(Machine(data_refinement)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(data_refinement)) == (color_refine);
  List_Of_VisibleVar_Ids(Machine(data_refinement)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(data_refinement)) == (? : ?);
  List_Of_Ids(Machine(definitions)) == (COLOR,green,yellow,red | ? | ? | ? | ? | ? | ? | ? | definitions);
  List_Of_HiddenCst_Ids(Machine(definitions)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(definitions)) == (?);
  List_Of_VisibleVar_Ids(Machine(definitions)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(definitions)) == (? : ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(data_refinement)) == (Type(color_refine) == Cst(SetOf(etype(COLOR,0,2)*btype(INTEGER,?,?))))
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
