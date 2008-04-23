Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(BoolXor))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(BoolXor))==(Machine(BoolXor));
  Level(Machine(BoolXor))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(BoolXor)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(BoolXor))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(BoolXor))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(BoolXor))==(?);
  List_Includes(Machine(BoolXor))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(BoolXor))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(BoolXor))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(BoolXor))==(?);
  Context_List_Variables(Machine(BoolXor))==(?);
  Abstract_List_Variables(Machine(BoolXor))==(?);
  Local_List_Variables(Machine(BoolXor))==(?);
  List_Variables(Machine(BoolXor))==(?);
  External_List_Variables(Machine(BoolXor))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(BoolXor))==(?);
  Abstract_List_VisibleVariables(Machine(BoolXor))==(?);
  External_List_VisibleVariables(Machine(BoolXor))==(?);
  Expanded_List_VisibleVariables(Machine(BoolXor))==(?);
  List_VisibleVariables(Machine(BoolXor))==(?);
  Internal_List_VisibleVariables(Machine(BoolXor))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(BoolXor))==(btrue);
  Gluing_List_Invariant(Machine(BoolXor))==(btrue);
  Expanded_List_Invariant(Machine(BoolXor))==(btrue);
  Abstract_List_Invariant(Machine(BoolXor))==(btrue);
  Context_List_Invariant(Machine(BoolXor))==(btrue);
  List_Invariant(Machine(BoolXor))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(BoolXor))==(btrue);
  Abstract_List_Assertions(Machine(BoolXor))==(btrue);
  Context_List_Assertions(Machine(BoolXor))==(btrue);
  List_Assertions(Machine(BoolXor))==(xor(TRUE,FALSE) = TRUE;xor(FALSE,TRUE) = TRUE;xor(TRUE,TRUE) = FALSE;xor(FALSE,FALSE) = FALSE)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(BoolXor))==(skip);
  Context_List_Initialisation(Machine(BoolXor))==(skip);
  List_Initialisation(Machine(BoolXor))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(BoolXor))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(BoolXor))==(btrue);
  List_Constraints(Machine(BoolXor))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(BoolXor))==(?);
  List_Operations(Machine(BoolXor))==(?)
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
  List_Valuable_Constants(Machine(BoolXor))==(xor);
  Inherited_List_Constants(Machine(BoolXor))==(?);
  List_Constants(Machine(BoolXor))==(xor)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(BoolXor))==(?);
  Context_List_Defered(Machine(BoolXor))==(?);
  Context_List_Sets(Machine(BoolXor))==(?);
  List_Valuable_Sets(Machine(BoolXor))==(?);
  Inherited_List_Enumerated(Machine(BoolXor))==(?);
  Inherited_List_Defered(Machine(BoolXor))==(?);
  Inherited_List_Sets(Machine(BoolXor))==(?);
  List_Enumerated(Machine(BoolXor))==(?);
  List_Defered(Machine(BoolXor))==(?);
  List_Sets(Machine(BoolXor))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(BoolXor))==(?);
  Expanded_List_HiddenConstants(Machine(BoolXor))==(?);
  List_HiddenConstants(Machine(BoolXor))==(?);
  External_List_HiddenConstants(Machine(BoolXor))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(BoolXor))==(btrue);
  Context_List_Properties(Machine(BoolXor))==(btrue);
  Inherited_List_Properties(Machine(BoolXor))==(btrue);
  List_Properties(Machine(BoolXor))==(xor : BOOL*BOOL --> BOOL & !(a,b).(a : BOOL & b : BOOL => xor(a,b) = bool(a/=b)))
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(BoolXor)) == (xor | ? | ? | ? | ? | ? | ? | ? | BoolXor);
  List_Of_HiddenCst_Ids(Machine(BoolXor)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BoolXor)) == (xor);
  List_Of_VisibleVar_Ids(Machine(BoolXor)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BoolXor)) == (? : ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(BoolXor)) == (Type(xor) == Cst(SetOf(btype(BOOL,0,1)*btype(BOOL,0,1)*btype(BOOL,0,1))))
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
