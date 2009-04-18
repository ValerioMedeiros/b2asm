Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Z80Temp))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Z80Temp))==(Machine(Z80Temp));
  Level(Machine(Z80Temp))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Z80Temp)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Z80Temp))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Z80Temp))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Z80Temp))==(?);
  List_Includes(Machine(Z80Temp))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Z80Temp))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Z80Temp))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Z80Temp))==(?);
  Context_List_Variables(Machine(Z80Temp))==(?);
  Abstract_List_Variables(Machine(Z80Temp))==(?);
  Local_List_Variables(Machine(Z80Temp))==(?);
  List_Variables(Machine(Z80Temp))==(?);
  External_List_Variables(Machine(Z80Temp))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Z80Temp))==(?);
  Abstract_List_VisibleVariables(Machine(Z80Temp))==(?);
  External_List_VisibleVariables(Machine(Z80Temp))==(?);
  Expanded_List_VisibleVariables(Machine(Z80Temp))==(?);
  List_VisibleVariables(Machine(Z80Temp))==(?);
  Internal_List_VisibleVariables(Machine(Z80Temp))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Z80Temp))==(btrue);
  Gluing_List_Invariant(Machine(Z80Temp))==(btrue);
  Expanded_List_Invariant(Machine(Z80Temp))==(btrue);
  Abstract_List_Invariant(Machine(Z80Temp))==(btrue);
  Context_List_Invariant(Machine(Z80Temp))==(btrue);
  List_Invariant(Machine(Z80Temp))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(Z80Temp))==(btrue);
  Abstract_List_Assertions(Machine(Z80Temp))==(btrue);
  Context_List_Assertions(Machine(Z80Temp))==(btrue);
  List_Assertions(Machine(Z80Temp))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Z80Temp))==(skip);
  Context_List_Initialisation(Machine(Z80Temp))==(skip);
  List_Initialisation(Machine(Z80Temp))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Z80Temp))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(Z80Temp))==(btrue);
  List_Constraints(Machine(Z80Temp))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Z80Temp))==(?);
  List_Operations(Machine(Z80Temp))==(?)
END
&
THEORY ListInputX END
&
THEORY ListOutputX END
&
THEORY ListHeaderX END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX END
&
THEORY ListSubstitutionX END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Z80Temp))==(?);
  Inherited_List_Constants(Machine(Z80Temp))==(?);
  List_Constants(Machine(Z80Temp))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(Z80Temp))==(?);
  Context_List_Defered(Machine(Z80Temp))==(?);
  Context_List_Sets(Machine(Z80Temp))==(?);
  List_Valuable_Sets(Machine(Z80Temp))==(?);
  Inherited_List_Enumerated(Machine(Z80Temp))==(?);
  Inherited_List_Defered(Machine(Z80Temp))==(?);
  Inherited_List_Sets(Machine(Z80Temp))==(?);
  List_Enumerated(Machine(Z80Temp))==(?);
  List_Defered(Machine(Z80Temp))==(?);
  List_Sets(Machine(Z80Temp))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Z80Temp))==(?);
  Expanded_List_HiddenConstants(Machine(Z80Temp))==(?);
  List_HiddenConstants(Machine(Z80Temp))==(?);
  External_List_HiddenConstants(Machine(Z80Temp))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Z80Temp))==(btrue);
  Context_List_Properties(Machine(Z80Temp))==(btrue);
  Inherited_List_Properties(Machine(Z80Temp))==(btrue);
  List_Properties(Machine(Z80Temp))==(btrue)
END
&
THEORY ListSeenInfoX END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(Z80Temp),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Z80Temp)) == (? | ? | ? | ? | ? | ? | ? | ? | Z80Temp);
  List_Of_HiddenCst_Ids(Machine(Z80Temp)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Z80Temp)) == (?);
  List_Of_VisibleVar_Ids(Machine(Z80Temp)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Z80Temp)) == (?: ?)
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO;
  event_b_project == KO;
  event_b_deadlockfreeness == KO;
  variant_clause_mandatory == OK
END
)
