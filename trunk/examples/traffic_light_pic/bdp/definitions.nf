Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(definitions))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(definitions))==(Machine(definitions));
  Level(Machine(definitions))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(definitions)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(definitions))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(definitions))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(definitions))==(?);
  List_Includes(Machine(definitions))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(definitions))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(definitions))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(definitions))==(?);
  Context_List_Variables(Machine(definitions))==(?);
  Abstract_List_Variables(Machine(definitions))==(?);
  Local_List_Variables(Machine(definitions))==(?);
  List_Variables(Machine(definitions))==(?);
  External_List_Variables(Machine(definitions))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(definitions))==(?);
  Abstract_List_VisibleVariables(Machine(definitions))==(?);
  External_List_VisibleVariables(Machine(definitions))==(?);
  Expanded_List_VisibleVariables(Machine(definitions))==(?);
  List_VisibleVariables(Machine(definitions))==(?);
  Internal_List_VisibleVariables(Machine(definitions))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(definitions))==(btrue);
  Gluing_List_Invariant(Machine(definitions))==(btrue);
  Expanded_List_Invariant(Machine(definitions))==(btrue);
  Abstract_List_Invariant(Machine(definitions))==(btrue);
  Context_List_Invariant(Machine(definitions))==(btrue);
  List_Invariant(Machine(definitions))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(definitions))==(btrue);
  Abstract_List_Assertions(Machine(definitions))==(btrue);
  Context_List_Assertions(Machine(definitions))==(btrue);
  List_Assertions(Machine(definitions))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(definitions))==(skip);
  Context_List_Initialisation(Machine(definitions))==(skip);
  List_Initialisation(Machine(definitions))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(definitions))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(definitions))==(btrue);
  List_Constraints(Machine(definitions))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(definitions))==(?);
  List_Operations(Machine(definitions))==(?)
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
  List_Valuable_Constants(Machine(definitions))==(?);
  Inherited_List_Constants(Machine(definitions))==(?);
  List_Constants(Machine(definitions))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(definitions))==(?);
  Context_List_Defered(Machine(definitions))==(?);
  Context_List_Sets(Machine(definitions))==(?);
  List_Valuable_Sets(Machine(definitions))==(?);
  Inherited_List_Enumerated(Machine(definitions))==(?);
  Inherited_List_Defered(Machine(definitions))==(?);
  Inherited_List_Sets(Machine(definitions))==(?);
  List_Enumerated(Machine(definitions))==(?);
  List_Defered(Machine(definitions))==(?);
  List_Sets(Machine(definitions))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(definitions))==(?);
  Expanded_List_HiddenConstants(Machine(definitions))==(?);
  List_HiddenConstants(Machine(definitions))==(?);
  External_List_HiddenConstants(Machine(definitions))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(definitions))==(btrue);
  Context_List_Properties(Machine(definitions))==(btrue);
  Inherited_List_Properties(Machine(definitions))==(btrue);
  List_Properties(Machine(definitions))==(btrue)
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(definitions)) == (? | ? | ? | ? | ? | ? | ? | ? | definitions);
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
)
