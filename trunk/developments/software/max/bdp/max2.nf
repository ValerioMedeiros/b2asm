Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(max2))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(max2))==(Machine(max2));
  Level(Machine(max2))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(max2)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(max2))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(max2))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(max2))==(?);
  List_Includes(Machine(max2))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(max2))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(max2))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(max2))==(?);
  Context_List_Variables(Machine(max2))==(?);
  Abstract_List_Variables(Machine(max2))==(?);
  Local_List_Variables(Machine(max2))==(m,b,a);
  List_Variables(Machine(max2))==(m,b,a);
  External_List_Variables(Machine(max2))==(m,b,a)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(max2))==(?);
  Abstract_List_VisibleVariables(Machine(max2))==(?);
  External_List_VisibleVariables(Machine(max2))==(?);
  Expanded_List_VisibleVariables(Machine(max2))==(?);
  List_VisibleVariables(Machine(max2))==(?);
  Internal_List_VisibleVariables(Machine(max2))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(max2))==(btrue);
  Gluing_List_Invariant(Machine(max2))==(btrue);
  Expanded_List_Invariant(Machine(max2))==(btrue);
  Abstract_List_Invariant(Machine(max2))==(btrue);
  Context_List_Invariant(Machine(max2))==(btrue);
  List_Invariant(Machine(max2))==(a : INT & b : INT & m : INT & (m = a or m = b) & m>=a & m>=b)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(max2))==(btrue);
  Abstract_List_Assertions(Machine(max2))==(btrue);
  Context_List_Assertions(Machine(max2))==(btrue);
  List_Assertions(Machine(max2))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(max2))==(a,b,m:=0,0,0);
  Context_List_Initialisation(Machine(max2))==(skip);
  List_Initialisation(Machine(max2))==(a,b,m:=0,0,0)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(max2))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(max2))==(btrue);
  List_Constraints(Machine(max2))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(max2))==(run);
  List_Operations(Machine(max2))==(run)
END
&
THEORY ListInputX IS
  List_Input(Machine(max2),run)==(v)
END
&
THEORY ListOutputX IS
  List_Output(Machine(max2),run)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(max2),run)==(run(v))
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(max2),run)==(v : INT & v>=0)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(max2),run)==(v : INT & v>=0 | a,b,m:=v,a,max({a,v}));
  List_Substitution(Machine(max2),run)==(a:=v || b:=a || m:=max({a,v}))
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(max2))==(?);
  Inherited_List_Constants(Machine(max2))==(?);
  List_Constants(Machine(max2))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(max2))==(?);
  Context_List_Defered(Machine(max2))==(?);
  Context_List_Sets(Machine(max2))==(?);
  List_Valuable_Sets(Machine(max2))==(?);
  Inherited_List_Enumerated(Machine(max2))==(?);
  Inherited_List_Defered(Machine(max2))==(?);
  Inherited_List_Sets(Machine(max2))==(?);
  List_Enumerated(Machine(max2))==(?);
  List_Defered(Machine(max2))==(?);
  List_Sets(Machine(max2))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(max2))==(?);
  Expanded_List_HiddenConstants(Machine(max2))==(?);
  List_HiddenConstants(Machine(max2))==(?);
  External_List_HiddenConstants(Machine(max2))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(max2))==(btrue);
  Context_List_Properties(Machine(max2))==(btrue);
  Inherited_List_Properties(Machine(max2))==(btrue);
  List_Properties(Machine(max2))==(btrue)
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(max2)) == (? | ? | m,b,a | ? | run | ? | ? | ? | max2);
  List_Of_HiddenCst_Ids(Machine(max2)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(max2)) == (?);
  List_Of_VisibleVar_Ids(Machine(max2)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(max2)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(max2)) == (Type(m) == Mvl(btype(INTEGER,?,?));Type(b) == Mvl(btype(INTEGER,?,?));Type(a) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(max2)) == (Type(run) == Cst(No_type,btype(INTEGER,?,?)))
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
