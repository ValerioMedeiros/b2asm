Normalised(
THEORY MagicNumberX IS
  MagicNumber(Refinement(swap2r))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Refinement(swap2r))==(Machine(swap2));
  Level(Refinement(swap2r))==(1);
  Upper_Level(Refinement(swap2r))==(Machine(swap2))
END
&
THEORY LoadedStructureX IS
  Refinement(swap2r)
END
&
THEORY ListSeesX IS
  List_Sees(Refinement(swap2r))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Refinement(swap2r))==(?);
  List_Includes(Refinement(swap2r))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Refinement(swap2r))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Refinement(swap2r))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Refinement(swap2r))==(?);
  Context_List_Variables(Refinement(swap2r))==(?);
  Abstract_List_Variables(Refinement(swap2r))==(b,a);
  Local_List_Variables(Refinement(swap2r))==(br,ar);
  List_Variables(Refinement(swap2r))==(br,ar);
  External_List_Variables(Refinement(swap2r))==(br,ar)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Refinement(swap2r))==(?);
  Abstract_List_VisibleVariables(Refinement(swap2r))==(?);
  External_List_VisibleVariables(Refinement(swap2r))==(?);
  Expanded_List_VisibleVariables(Refinement(swap2r))==(?);
  List_VisibleVariables(Refinement(swap2r))==(?);
  Internal_List_VisibleVariables(Refinement(swap2r))==(?)
END
&
THEORY ListOfNewVariablesX IS
  List_Of_New_Variables(Refinement(swap2r))==(br,ar)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Refinement(swap2r))==(btrue);
  Expanded_List_Invariant(Refinement(swap2r))==(btrue);
  Abstract_List_Invariant(Refinement(swap2r))==(a : INT & b : INT);
  Context_List_Invariant(Refinement(swap2r))==(btrue);
  List_Invariant(Refinement(swap2r))==(ar : INT & br : INT & ar = a & br = b)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Refinement(swap2r))==(btrue);
  Abstract_List_Assertions(Refinement(swap2r))==(btrue);
  Context_List_Assertions(Refinement(swap2r))==(btrue);
  List_Assertions(Refinement(swap2r))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Refinement(swap2r))==(ar:=0;br:=0);
  Context_List_Initialisation(Refinement(swap2r))==(skip);
  List_Initialisation(Refinement(swap2r))==(BEGIN ar:=0;br:=0 END)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Refinement(swap2r))==(swap);
  List_Operations(Refinement(swap2r))==(swap)
END
&
THEORY ListInputX IS
  List_Input(Refinement(swap2r),swap)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Refinement(swap2r),swap)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Refinement(swap2r),swap)==(swap)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Refinement(swap2r),swap)==(btrue);
  List_Precondition(Refinement(swap2r),swap)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Refinement(swap2r),swap)==(btrue | @tmp.(tmp:=ar;ar:=br;br:=tmp));
  List_Substitution(Refinement(swap2r),swap)==(VAR tmp IN tmp:=ar;ar:=br;br:=tmp END)
END
&
THEORY ListParametersX IS
  List_Parameters(Refinement(swap2r))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Constraints(Refinement(swap2r))==(btrue);
  List_Context_Constraints(Refinement(swap2r))==(btrue)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Refinement(swap2r))==(?);
  Inherited_List_Constants(Refinement(swap2r))==(?);
  List_Constants(Refinement(swap2r))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Refinement(swap2r))==(?);
  Context_List_Defered(Refinement(swap2r))==(?);
  Context_List_Sets(Refinement(swap2r))==(?);
  List_Valuable_Sets(Refinement(swap2r))==(?);
  Inherited_List_Enumerated(Refinement(swap2r))==(?);
  Inherited_List_Defered(Refinement(swap2r))==(?);
  Inherited_List_Sets(Refinement(swap2r))==(?);
  List_Enumerated(Refinement(swap2r))==(?);
  List_Defered(Refinement(swap2r))==(?);
  List_Sets(Refinement(swap2r))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Refinement(swap2r))==(?);
  Expanded_List_HiddenConstants(Refinement(swap2r))==(?);
  List_HiddenConstants(Refinement(swap2r))==(?);
  External_List_HiddenConstants(Refinement(swap2r))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Refinement(swap2r))==(btrue);
  Context_List_Properties(Refinement(swap2r))==(btrue);
  Inherited_List_Properties(Refinement(swap2r))==(btrue);
  List_Properties(Refinement(swap2r))==(btrue)
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Refinement(swap2r)) == (? | ? | br,ar | ? | swap | ? | ? | ? | swap2r);
  List_Of_HiddenCst_Ids(Refinement(swap2r)) == (? | ?);
  List_Of_VisibleCst_Ids(Refinement(swap2r)) == (?);
  List_Of_VisibleVar_Ids(Refinement(swap2r)) == (? | ?);
  List_Of_Ids_SeenBNU(Refinement(swap2r)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Refinement(swap2r)) == (Type(br) == Mvl(btype(INTEGER,?,?));Type(ar) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Refinement(swap2r)) == (Type(swap) == Cst(No_type,No_type))
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
