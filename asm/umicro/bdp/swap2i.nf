Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(swap2i))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(swap2i))==(Machine(swap2));
  Level(Implementation(swap2i))==(2);
  Upper_Level(Implementation(swap2i))==(Refinement(swap2r))
END
&
THEORY LoadedStructureX IS
  Implementation(swap2i)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(swap2i))==(?)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(swap2i))==(up.move3);
  Inherited_List_Includes(Implementation(swap2i))==(up.move3)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(swap2i))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(swap2i))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(swap2i))==(?);
  Context_List_Variables(Implementation(swap2i))==(?);
  Abstract_List_Variables(Implementation(swap2i))==(br,ar,b,a);
  Local_List_Variables(Implementation(swap2i))==(?);
  List_Variables(Implementation(swap2i))==(?);
  External_List_Variables(Implementation(swap2i))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(swap2i))==(?);
  Abstract_List_VisibleVariables(Implementation(swap2i))==(?);
  External_List_VisibleVariables(Implementation(swap2i))==(up.reg);
  Expanded_List_VisibleVariables(Implementation(swap2i))==(upreg);
  List_VisibleVariables(Implementation(swap2i))==(?);
  Internal_List_VisibleVariables(Implementation(swap2i))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(swap2i))==(btrue);
  Abstract_List_Invariant(Implementation(swap2i))==(ar : INT & br : INT & ar = a & br = b & a : INT & b : INT);
  Expanded_List_Invariant(Implementation(swap2i))==(upreg : 1..3 --> NAT);
  Context_List_Invariant(Implementation(swap2i))==(btrue);
  List_Invariant(Implementation(swap2i))==(ar = upreg(1) & br = upreg(2))
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(swap2i))==(btrue);
  Expanded_List_Assertions(Implementation(swap2i))==(btrue);
  Context_List_Assertions(Implementation(swap2i))==(btrue);
  List_Assertions(Implementation(swap2i))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(swap2i))==(@(reg$0).(reg$0 : 1..3 --> NAT ==> upreg:=reg$0);(1 : 1..3 | upreg:=upreg<+{1|->0});(2 : 1..3 | upreg:=upreg<+{2|->0}));
  Context_List_Initialisation(Implementation(swap2i))==(skip);
  List_Initialisation(Implementation(swap2i))==(BEGIN (up.clr)(1);(up.clr)(2) END)
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(swap2i))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(swap2i),Machine(up.move3))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(swap2i),Machine(up.move3))==(btrue);
  List_Constraints(Implementation(swap2i))==(btrue);
  List_Context_Constraints(Implementation(swap2i))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(swap2i))==(swap);
  List_Operations(Implementation(swap2i))==(swap)
END
&
THEORY ListInputX IS
  List_Input(Implementation(swap2i),swap)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(swap2i),swap)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(swap2i),swap)==(swap)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(swap2i),swap)==(btrue);
  List_Precondition(Implementation(swap2i),swap)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(swap2i),swap)==(btrue | (1 : 1..3 & 3 : 1..3 & 1/=3 | upreg:=upreg<+{3|->upreg(1)});(2 : 1..3 & 1 : 1..3 & 2/=1 | upreg:=upreg<+{1|->upreg(2)});(3 : 1..3 & 2 : 1..3 & 3/=2 | upreg:=upreg<+{2|->upreg(3)}));
  List_Substitution(Implementation(swap2i),swap)==((up.move)(1,3);(up.move)(2,1);(up.move)(3,2))
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(swap2i))==(?);
  Inherited_List_Constants(Implementation(swap2i))==(?);
  List_Constants(Implementation(swap2i))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Implementation(swap2i))==(?);
  Context_List_Defered(Implementation(swap2i))==(?);
  Context_List_Sets(Implementation(swap2i))==(?);
  List_Own_Enumerated(Implementation(swap2i))==(?);
  List_Valuable_Sets(Implementation(swap2i))==(?);
  Inherited_List_Enumerated(Implementation(swap2i))==(?);
  Inherited_List_Defered(Implementation(swap2i))==(?);
  Inherited_List_Sets(Implementation(swap2i))==(?);
  List_Enumerated(Implementation(swap2i))==(?);
  List_Defered(Implementation(swap2i))==(?);
  List_Sets(Implementation(swap2i))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(swap2i))==(?);
  Expanded_List_HiddenConstants(Implementation(swap2i))==(?);
  List_HiddenConstants(Implementation(swap2i))==(?);
  External_List_HiddenConstants(Implementation(swap2i))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(swap2i))==(btrue);
  Context_List_Properties(Implementation(swap2i))==(btrue);
  Inherited_List_Properties(Implementation(swap2i))==(btrue);
  List_Properties(Implementation(swap2i))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(swap2i))==(aa : aa);
  List_Values(Implementation(swap2i))==(?)
END
&
THEORY ListSeenInfoX END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(swap2i),Machine(move3))==(clr,move)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(swap2i))==(Type(swap) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(swap2i)) == (? | ? | ? | ? | swap | ? | imported(Machine(up.move3)) | ? | swap2i);
  List_Of_HiddenCst_Ids(Implementation(swap2i)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(swap2i)) == (?);
  List_Of_VisibleVar_Ids(Implementation(swap2i)) == (? | upreg);
  List_Of_Ids_SeenBNU(Implementation(swap2i)) == (? : ?);
  List_Of_Ids(Machine(move3)) == (? | ? | ? | ? | clr,move | ? | ? | ? | move3);
  List_Of_HiddenCst_Ids(Machine(move3)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(move3)) == (?);
  List_Of_VisibleVar_Ids(Machine(move3)) == (reg | ?);
  List_Of_Ids_SeenBNU(Machine(move3)) == (? : ?)
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO
END
&
THEORY ListLocalOperationsX IS
  List_Local_Operations(Implementation(swap2i))==(?)
END
&
THEORY ListLocalInputX END
&
THEORY ListLocalOutputX END
&
THEORY ListLocalHeaderX END
&
THEORY ListLocalPreconditionX END
&
THEORY ListLocalSubstitutionX END
&
THEORY TypingPredicateX IS
  TypingPredicate(Implementation(swap2i))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(swap2i),Machine(up.move3))==(?);
  ImportedVisVariablesList(Implementation(swap2i),Machine(up.move3))==(up.reg)
END
&
THEORY ListLocalOpInvariantX END
)
