Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(POWER))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(POWER))==(Machine(POWER));
  Level(Machine(POWER))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(POWER)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(POWER))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(POWER))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(POWER))==(?);
  List_Includes(Machine(POWER))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(POWER))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(POWER))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(POWER))==(?);
  Context_List_Variables(Machine(POWER))==(?);
  Abstract_List_Variables(Machine(POWER))==(?);
  Local_List_Variables(Machine(POWER))==(?);
  List_Variables(Machine(POWER))==(?);
  External_List_Variables(Machine(POWER))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(POWER))==(?);
  Abstract_List_VisibleVariables(Machine(POWER))==(?);
  External_List_VisibleVariables(Machine(POWER))==(?);
  Expanded_List_VisibleVariables(Machine(POWER))==(?);
  List_VisibleVariables(Machine(POWER))==(?);
  Internal_List_VisibleVariables(Machine(POWER))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(POWER))==(btrue);
  Gluing_List_Invariant(Machine(POWER))==(btrue);
  Expanded_List_Invariant(Machine(POWER))==(btrue);
  Abstract_List_Invariant(Machine(POWER))==(btrue);
  Context_List_Invariant(Machine(POWER))==(btrue);
  List_Invariant(Machine(POWER))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(POWER))==(btrue);
  Abstract_List_Assertions(Machine(POWER))==(btrue);
  Context_List_Assertions(Machine(POWER))==(btrue);
  List_Assertions(Machine(POWER))==(!(nn,pp).(nn: NATURAL1 & pp: NAT => (pp = 0 => nn**pp = 1) & (pp = 1 => nn**pp = nn) & (pp>1 => nn**pp = nn*nn**(pp-1))))
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(POWER))==(skip);
  Context_List_Initialisation(Machine(POWER))==(skip);
  List_Initialisation(Machine(POWER))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(POWER))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(POWER))==(btrue);
  List_Constraints(Machine(POWER))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(POWER))==(?);
  List_Operations(Machine(POWER))==(?)
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
  List_Valuable_Constants(Machine(POWER))==(?);
  Inherited_List_Constants(Machine(POWER))==(?);
  List_Constants(Machine(POWER))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(POWER))==(?);
  Context_List_Defered(Machine(POWER))==(?);
  Context_List_Sets(Machine(POWER))==(?);
  List_Valuable_Sets(Machine(POWER))==(?);
  Inherited_List_Enumerated(Machine(POWER))==(?);
  Inherited_List_Defered(Machine(POWER))==(?);
  Inherited_List_Sets(Machine(POWER))==(?);
  List_Enumerated(Machine(POWER))==(?);
  List_Defered(Machine(POWER))==(?);
  List_Sets(Machine(POWER))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(POWER))==(?);
  Expanded_List_HiddenConstants(Machine(POWER))==(?);
  List_HiddenConstants(Machine(POWER))==(?);
  External_List_HiddenConstants(Machine(POWER))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(POWER))==(btrue);
  Context_List_Properties(Machine(POWER))==(btrue);
  Inherited_List_Properties(Machine(POWER))==(btrue);
  List_Properties(Machine(POWER))==(btrue)
END
&
THEORY ListSeenInfoX END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(POWER),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(POWER)) == (? | ? | ? | ? | ? | ? | ? | ? | POWER);
  List_Of_HiddenCst_Ids(Machine(POWER)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(POWER)) == (?);
  List_Of_VisibleVar_Ids(Machine(POWER)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(POWER)) == (?: ?)
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
