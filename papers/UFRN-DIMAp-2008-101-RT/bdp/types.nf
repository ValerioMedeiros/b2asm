Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(types))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(types))==(Machine(types));
  Level(Machine(types))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(types)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(types))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(types))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(types))==(?);
  List_Includes(Machine(types))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(types))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(types))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(types))==(?);
  Context_List_Variables(Machine(types))==(?);
  Abstract_List_Variables(Machine(types))==(?);
  Local_List_Variables(Machine(types))==(?);
  List_Variables(Machine(types))==(?);
  External_List_Variables(Machine(types))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(types))==(?);
  Abstract_List_VisibleVariables(Machine(types))==(?);
  External_List_VisibleVariables(Machine(types))==(?);
  Expanded_List_VisibleVariables(Machine(types))==(?);
  List_VisibleVariables(Machine(types))==(?);
  Internal_List_VisibleVariables(Machine(types))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(types))==(btrue);
  Gluing_List_Invariant(Machine(types))==(btrue);
  Expanded_List_Invariant(Machine(types))==(btrue);
  Abstract_List_Invariant(Machine(types))==(btrue);
  Context_List_Invariant(Machine(types))==(btrue);
  List_Invariant(Machine(types))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(types))==(btrue);
  Abstract_List_Assertions(Machine(types))==(btrue);
  Context_List_Assertions(Machine(types))==(btrue);
  List_Assertions(Machine(types))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(types))==(skip);
  Context_List_Initialisation(Machine(types))==(skip);
  List_Initialisation(Machine(types))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(types))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(types))==(btrue);
  List_Constraints(Machine(types))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(types))==(?);
  List_Operations(Machine(types))==(?)
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
  List_Valuable_Constants(Machine(types))==(uint32,uint16);
  Inherited_List_Constants(Machine(types))==(?);
  List_Constants(Machine(types))==(uint32,uint16)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(types))==(?);
  Context_List_Defered(Machine(types))==(?);
  Context_List_Sets(Machine(types))==(?);
  List_Valuable_Sets(Machine(types))==(?);
  Inherited_List_Enumerated(Machine(types))==(?);
  Inherited_List_Defered(Machine(types))==(?);
  Inherited_List_Sets(Machine(types))==(?);
  List_Enumerated(Machine(types))==(?);
  List_Defered(Machine(types))==(?);
  List_Sets(Machine(types))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(types))==(?);
  Expanded_List_HiddenConstants(Machine(types))==(?);
  List_HiddenConstants(Machine(types))==(?);
  External_List_HiddenConstants(Machine(types))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(types))==(btrue);
  Context_List_Properties(Machine(types))==(btrue);
  Inherited_List_Properties(Machine(types))==(btrue);
  List_Properties(Machine(types))==(uint32 : POW(INTEGER) & uint32 = 0..MAXINT & uint16 : POW(INTEGER) & uint16 = 0..65535)
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(types)) == (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | types);
  List_Of_HiddenCst_Ids(Machine(types)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(types)) == (uint32,uint16);
  List_Of_VisibleVar_Ids(Machine(types)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(types)) == (? : ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(types)) == (Type(uint32) == Cst(SetOf(btype(INTEGER,"[uint32","]uint32")));Type(uint16) == Cst(SetOf(btype(INTEGER,"[uint16","]uint16"))))
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
