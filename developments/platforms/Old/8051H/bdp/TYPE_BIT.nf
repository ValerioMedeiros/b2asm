Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(TYPE_BIT))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(TYPE_BIT))==(Machine(TYPE_BIT));
  Level(Machine(TYPE_BIT))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(TYPE_BIT)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(TYPE_BIT))==(BIT_DEFINITION)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(TYPE_BIT))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(TYPE_BIT))==(?);
  List_Includes(Machine(TYPE_BIT))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(TYPE_BIT))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(TYPE_BIT))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(TYPE_BIT))==(?);
  Context_List_Variables(Machine(TYPE_BIT))==(?);
  Abstract_List_Variables(Machine(TYPE_BIT))==(?);
  Local_List_Variables(Machine(TYPE_BIT))==(?);
  List_Variables(Machine(TYPE_BIT))==(?);
  External_List_Variables(Machine(TYPE_BIT))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(TYPE_BIT))==(?);
  Abstract_List_VisibleVariables(Machine(TYPE_BIT))==(?);
  External_List_VisibleVariables(Machine(TYPE_BIT))==(?);
  Expanded_List_VisibleVariables(Machine(TYPE_BIT))==(?);
  List_VisibleVariables(Machine(TYPE_BIT))==(?);
  Internal_List_VisibleVariables(Machine(TYPE_BIT))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(TYPE_BIT))==(btrue);
  Gluing_List_Invariant(Machine(TYPE_BIT))==(btrue);
  Expanded_List_Invariant(Machine(TYPE_BIT))==(btrue);
  Abstract_List_Invariant(Machine(TYPE_BIT))==(btrue);
  Context_List_Invariant(Machine(TYPE_BIT))==(btrue);
  List_Invariant(Machine(TYPE_BIT))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(TYPE_BIT))==(btrue);
  Abstract_List_Assertions(Machine(TYPE_BIT))==(btrue);
  Context_List_Assertions(Machine(TYPE_BIT))==(bit_not(0) = 1;bit_not(1) = 0;!bb.(bb: BIT => bit_not(bit_not(bb)) = bb);bit_and(0,0) = 0;bit_and(0,1) = 0;bit_and(1,0) = 0;bit_and(1,1) = 1;!(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3));!b1.(b1: BIT => bit_and(b1,1) = b1);!b1.(b1: BIT => bit_and(b1,0) = 0);bit_or(0,0) = 0;bit_or(0,1) = 0;bit_or(1,0) = 0;bit_or(1,1) = 1;!(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3));!b1.(b1: BIT => bit_or(b1,1) = 1);!b1.(b1: BIT => bit_or(b1,0) = b1);bit_xor(0,0) = 0;bit_xor(0,1) = 1;bit_xor(1,0) = 1;bit_xor(1,1) = 0;!(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3));!bb.(bb: BIT => bit_xor(bb,bb) = 0);bool_to_bit(TRUE) = 1;bool_to_bit(FALSE) = 0);
  List_Assertions(Machine(TYPE_BIT))==(bit_add(0,0,0) = 0,0 & bit_add(0,0,1) = 1,0 & bit_add(0,1,0) = 1,0 & bit_add(0,1,1) = 0,1 & bit_add(1,0,0) = 1,0 & bit_add(1,0,1) = 0,1 & bit_add(1,1,0) = 0,1 & bit_add(1,1,1) = 1,1 & bit_sub(0,0,0) = 0,0 & bit_sub(0,0,1) = 1,1 & bit_sub(0,1,0) = 1,1 & bit_sub(0,1,1) = 0,1 & bit_sub(1,0,0) = 1,0 & bit_sub(1,0,1) = 0,0 & bit_sub(1,1,0) = 0,0 & bit_sub(1,1,1) = 1,1)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(TYPE_BIT))==(skip);
  Context_List_Initialisation(Machine(TYPE_BIT))==(skip);
  List_Initialisation(Machine(TYPE_BIT))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(TYPE_BIT))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(TYPE_BIT),Machine(BIT_DEFINITION))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(TYPE_BIT))==(btrue);
  List_Constraints(Machine(TYPE_BIT))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(TYPE_BIT))==(?);
  List_Operations(Machine(TYPE_BIT))==(?)
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
  List_Valuable_Constants(Machine(TYPE_BIT))==(bit_add,bit_sub);
  Inherited_List_Constants(Machine(TYPE_BIT))==(?);
  List_Constants(Machine(TYPE_BIT))==(bit_add,bit_sub)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(TYPE_BIT))==(?);
  Context_List_Defered(Machine(TYPE_BIT))==(?);
  Context_List_Sets(Machine(TYPE_BIT))==(?);
  List_Valuable_Sets(Machine(TYPE_BIT))==(?);
  Inherited_List_Enumerated(Machine(TYPE_BIT))==(?);
  Inherited_List_Defered(Machine(TYPE_BIT))==(?);
  Inherited_List_Sets(Machine(TYPE_BIT))==(?);
  List_Enumerated(Machine(TYPE_BIT))==(?);
  List_Defered(Machine(TYPE_BIT))==(?);
  List_Sets(Machine(TYPE_BIT))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(TYPE_BIT))==(?);
  Expanded_List_HiddenConstants(Machine(TYPE_BIT))==(?);
  List_HiddenConstants(Machine(TYPE_BIT))==(?);
  External_List_HiddenConstants(Machine(TYPE_BIT))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(TYPE_BIT))==(btrue);
  Context_List_Properties(Machine(TYPE_BIT))==(BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0});
  Inherited_List_Properties(Machine(TYPE_BIT))==(btrue);
  List_Properties(Machine(TYPE_BIT))==(bit_add: BIT*BIT*BIT --> BIT*BIT & bit_add = %(b1,b2,cin).(b1: BIT & b2: BIT & cin: BIT | bit_xor(bit_xor(b1,b2),cin),bit_or(bit_and(b1,b2),bit_and(cin,bit_xor(b1,b2)))) & bit_sub: BIT*BIT*BIT --> BIT*BIT & bit_sub = %(b1,b2,cin).(b1: BIT & b2: BIT & cin: BIT | bit_xor(bit_xor(b1,b2),cin),bit_or(bit_and(bit_not(b1),b2),bit_and(cin,bit_or(bit_not(b1),b2)))))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(TYPE_BIT),Machine(BIT_DEFINITION))==(?);
  Seen_Context_List_Enumerated(Machine(TYPE_BIT))==(?);
  Seen_Context_List_Invariant(Machine(TYPE_BIT))==(btrue);
  Seen_Context_List_Assertions(Machine(TYPE_BIT))==(btrue);
  Seen_Context_List_Properties(Machine(TYPE_BIT))==(btrue);
  Seen_List_Constraints(Machine(TYPE_BIT))==(btrue);
  Seen_List_Operations(Machine(TYPE_BIT),Machine(BIT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(TYPE_BIT),Machine(BIT_DEFINITION))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(TYPE_BIT)) == (bit_add,bit_sub | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)) | ? | TYPE_BIT);
  List_Of_HiddenCst_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BIT)) == (bit_add,bit_sub);
  List_Of_VisibleVar_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BIT)) == (?: ?);
  List_Of_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ? | BIT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit);
  List_Of_VisibleVar_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_DEFINITION)) == (?: ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(TYPE_BIT)) == (Type(bit_add) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT"))));Type(bit_sub) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")))))
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
