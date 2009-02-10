Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(BIT_DEFINITION))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(BIT_DEFINITION))==(Machine(BIT_DEFINITION));
  Level(Machine(BIT_DEFINITION))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(BIT_DEFINITION)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(BIT_DEFINITION))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(BIT_DEFINITION))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(BIT_DEFINITION))==(?);
  List_Includes(Machine(BIT_DEFINITION))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(BIT_DEFINITION))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(BIT_DEFINITION))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(BIT_DEFINITION))==(?);
  Context_List_Variables(Machine(BIT_DEFINITION))==(?);
  Abstract_List_Variables(Machine(BIT_DEFINITION))==(?);
  Local_List_Variables(Machine(BIT_DEFINITION))==(?);
  List_Variables(Machine(BIT_DEFINITION))==(?);
  External_List_Variables(Machine(BIT_DEFINITION))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(BIT_DEFINITION))==(?);
  Abstract_List_VisibleVariables(Machine(BIT_DEFINITION))==(?);
  External_List_VisibleVariables(Machine(BIT_DEFINITION))==(?);
  Expanded_List_VisibleVariables(Machine(BIT_DEFINITION))==(?);
  List_VisibleVariables(Machine(BIT_DEFINITION))==(?);
  Internal_List_VisibleVariables(Machine(BIT_DEFINITION))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(BIT_DEFINITION))==(btrue);
  Gluing_List_Invariant(Machine(BIT_DEFINITION))==(btrue);
  Expanded_List_Invariant(Machine(BIT_DEFINITION))==(btrue);
  Abstract_List_Invariant(Machine(BIT_DEFINITION))==(btrue);
  Context_List_Invariant(Machine(BIT_DEFINITION))==(btrue);
  List_Invariant(Machine(BIT_DEFINITION))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(BIT_DEFINITION))==(btrue);
  Abstract_List_Assertions(Machine(BIT_DEFINITION))==(btrue);
  Context_List_Assertions(Machine(BIT_DEFINITION))==(btrue);
  List_Assertions(Machine(BIT_DEFINITION))==(!bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_to_bit(TRUE) = 1 & bool_to_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0))
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(BIT_DEFINITION))==(skip);
  Context_List_Initialisation(Machine(BIT_DEFINITION))==(skip);
  List_Initialisation(Machine(BIT_DEFINITION))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(BIT_DEFINITION))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(BIT_DEFINITION))==(btrue);
  List_Constraints(Machine(BIT_DEFINITION))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(BIT_DEFINITION))==(?);
  List_Operations(Machine(BIT_DEFINITION))==(?)
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
  List_Valuable_Constants(Machine(BIT_DEFINITION))==(BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit);
  Inherited_List_Constants(Machine(BIT_DEFINITION))==(?);
  List_Constants(Machine(BIT_DEFINITION))==(BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(BIT_DEFINITION))==(?);
  Context_List_Defered(Machine(BIT_DEFINITION))==(?);
  Context_List_Sets(Machine(BIT_DEFINITION))==(?);
  List_Valuable_Sets(Machine(BIT_DEFINITION))==(?);
  Inherited_List_Enumerated(Machine(BIT_DEFINITION))==(?);
  Inherited_List_Defered(Machine(BIT_DEFINITION))==(?);
  Inherited_List_Sets(Machine(BIT_DEFINITION))==(?);
  List_Enumerated(Machine(BIT_DEFINITION))==(?);
  List_Defered(Machine(BIT_DEFINITION))==(?);
  List_Sets(Machine(BIT_DEFINITION))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(BIT_DEFINITION))==(?);
  Expanded_List_HiddenConstants(Machine(BIT_DEFINITION))==(?);
  List_HiddenConstants(Machine(BIT_DEFINITION))==(?);
  External_List_HiddenConstants(Machine(BIT_DEFINITION))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(BIT_DEFINITION))==(btrue);
  Context_List_Properties(Machine(BIT_DEFINITION))==(btrue);
  Inherited_List_Properties(Machine(BIT_DEFINITION))==(btrue);
  List_Properties(Machine(BIT_DEFINITION))==(BIT = 0..1 & bit_not: BIT --> BIT & bit_not = %bb.(bb: BIT | 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0})
END
&
THEORY ListSeenInfoX END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(BIT_DEFINITION),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ? | BIT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit);
  List_Of_VisibleVar_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_DEFINITION)) == (?: ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(BIT_DEFINITION)) == (Type(BIT) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")));Type(bit_not) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(bit_and) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(bit_or) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(bit_xor) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(bool_to_bit) == Cst(SetOf(btype(BOOL,0,1)*btype(INTEGER,"[BIT","]BIT"))))
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
