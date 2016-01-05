Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(POWER2))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(POWER2))==(Machine(POWER2));
  Level(Machine(POWER2))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(POWER2)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(POWER2))==(POWER)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(POWER2))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(POWER2))==(?);
  List_Includes(Machine(POWER2))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(POWER2))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(POWER2))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(POWER2))==(?);
  Context_List_Variables(Machine(POWER2))==(?);
  Abstract_List_Variables(Machine(POWER2))==(?);
  Local_List_Variables(Machine(POWER2))==(?);
  List_Variables(Machine(POWER2))==(?);
  External_List_Variables(Machine(POWER2))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(POWER2))==(?);
  Abstract_List_VisibleVariables(Machine(POWER2))==(?);
  External_List_VisibleVariables(Machine(POWER2))==(?);
  Expanded_List_VisibleVariables(Machine(POWER2))==(?);
  List_VisibleVariables(Machine(POWER2))==(?);
  Internal_List_VisibleVariables(Machine(POWER2))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(POWER2))==(btrue);
  Gluing_List_Invariant(Machine(POWER2))==(btrue);
  Expanded_List_Invariant(Machine(POWER2))==(btrue);
  Abstract_List_Invariant(Machine(POWER2))==(btrue);
  Context_List_Invariant(Machine(POWER2))==(btrue);
  List_Invariant(Machine(POWER2))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(POWER2))==(btrue);
  Abstract_List_Assertions(Machine(POWER2))==(btrue);
  Context_List_Assertions(Machine(POWER2))==(!(nn,pp).(nn: NATURAL1 & pp: NAT => (pp = 0 => nn**pp = 1) & (pp = 1 => nn**pp = nn) & (pp>1 => nn**pp = nn*nn**(pp-1))));
  List_Assertions(Machine(POWER2))==(2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;(-2)**7 = -128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536)
END
&
THEORY ListCoverageX IS
  List_Coverage(Machine(POWER2))==(btrue)
END
&
THEORY ListExclusivityX IS
  List_Exclusivity(Machine(POWER2))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(POWER2))==(skip);
  Context_List_Initialisation(Machine(POWER2))==(skip);
  List_Initialisation(Machine(POWER2))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(POWER2))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(POWER2),Machine(POWER))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(POWER2))==(btrue);
  List_Constraints(Machine(POWER2))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(POWER2))==(?);
  List_Operations(Machine(POWER2))==(?)
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
  List_Valuable_Constants(Machine(POWER2))==(?);
  Inherited_List_Constants(Machine(POWER2))==(?);
  List_Constants(Machine(POWER2))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(POWER2))==(?);
  Context_List_Defered(Machine(POWER2))==(?);
  Context_List_Sets(Machine(POWER2))==(?);
  List_Valuable_Sets(Machine(POWER2))==(?);
  Inherited_List_Enumerated(Machine(POWER2))==(?);
  Inherited_List_Defered(Machine(POWER2))==(?);
  Inherited_List_Sets(Machine(POWER2))==(?);
  List_Enumerated(Machine(POWER2))==(?);
  List_Defered(Machine(POWER2))==(?);
  List_Sets(Machine(POWER2))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(POWER2))==(?);
  Expanded_List_HiddenConstants(Machine(POWER2))==(?);
  List_HiddenConstants(Machine(POWER2))==(?);
  External_List_HiddenConstants(Machine(POWER2))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(POWER2))==(btrue);
  Context_List_Properties(Machine(POWER2))==(btrue);
  Inherited_List_Properties(Machine(POWER2))==(btrue);
  List_Properties(Machine(POWER2))==(btrue)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(POWER2),Machine(POWER))==(?);
  Seen_Context_List_Enumerated(Machine(POWER2))==(?);
  Seen_Context_List_Invariant(Machine(POWER2))==(btrue);
  Seen_Context_List_Assertions(Machine(POWER2))==(btrue);
  Seen_Context_List_Properties(Machine(POWER2))==(btrue);
  Seen_List_Constraints(Machine(POWER2))==(btrue);
  Seen_List_Operations(Machine(POWER2),Machine(POWER))==(?);
  Seen_Expanded_List_Invariant(Machine(POWER2),Machine(POWER))==(btrue)
END
&
THEORY ListANYVarX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(POWER2)) == (? | ? | ? | ? | ? | ? | seen(Machine(POWER)) | ? | POWER2);
  List_Of_HiddenCst_Ids(Machine(POWER2)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(POWER2)) == (?);
  List_Of_VisibleVar_Ids(Machine(POWER2)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(POWER2)) == (?: ?);
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
  project_type == SOFTWARE_TYPE;
  event_b_deadlockfreeness == KO;
  variant_clause_mandatory == KO;
  event_b_coverage == KO;
  event_b_exclusivity == KO;
  genFeasibilityPO == KO
END
)
