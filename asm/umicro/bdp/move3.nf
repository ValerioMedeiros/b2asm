Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(move3))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(move3))==(Machine(move3));
  Level(Machine(move3))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(move3)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(move3))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(move3))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(move3))==(?);
  List_Includes(Machine(move3))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(move3))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(move3))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(move3))==(?);
  Context_List_Variables(Machine(move3))==(?);
  Abstract_List_Variables(Machine(move3))==(?);
  Local_List_Variables(Machine(move3))==(?);
  List_Variables(Machine(move3))==(?);
  External_List_Variables(Machine(move3))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(move3))==(?);
  Abstract_List_VisibleVariables(Machine(move3))==(?);
  External_List_VisibleVariables(Machine(move3))==(?);
  Expanded_List_VisibleVariables(Machine(move3))==(?);
  List_VisibleVariables(Machine(move3))==(reg);
  Internal_List_VisibleVariables(Machine(move3))==(reg)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(move3))==(btrue);
  Gluing_List_Invariant(Machine(move3))==(btrue);
  Expanded_List_Invariant(Machine(move3))==(btrue);
  Abstract_List_Invariant(Machine(move3))==(btrue);
  Context_List_Invariant(Machine(move3))==(btrue);
  List_Invariant(Machine(move3))==(reg : 1..3 --> NAT)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(move3))==(btrue);
  Abstract_List_Assertions(Machine(move3))==(btrue);
  Context_List_Assertions(Machine(move3))==(btrue);
  List_Assertions(Machine(move3))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(move3))==(@(reg$0).(reg$0 : 1..3 --> NAT ==> reg:=reg$0));
  Context_List_Initialisation(Machine(move3))==(skip);
  List_Initialisation(Machine(move3))==(reg:: 1..3 --> NAT)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(move3))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(move3))==(btrue);
  List_Constraints(Machine(move3))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(move3))==(clr,move);
  List_Operations(Machine(move3))==(clr,move)
END
&
THEORY ListInputX IS
  List_Input(Machine(move3),clr)==(i);
  List_Input(Machine(move3),move)==(i,j)
END
&
THEORY ListOutputX IS
  List_Output(Machine(move3),clr)==(?);
  List_Output(Machine(move3),move)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(move3),clr)==(clr(i));
  List_Header(Machine(move3),move)==(move(i,j))
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(move3),clr)==(i : 1..3);
  List_Precondition(Machine(move3),move)==(i : 1..3 & j : 1..3 & i/=j)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(move3),move)==(i : 1..3 & j : 1..3 & i/=j | reg:=reg<+{j|->reg(i)});
  Expanded_List_Substitution(Machine(move3),clr)==(i : 1..3 | reg:=reg<+{i|->0});
  List_Substitution(Machine(move3),clr)==(reg(i):=0);
  List_Substitution(Machine(move3),move)==(reg(j):=reg(i))
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(move3))==(?);
  Inherited_List_Constants(Machine(move3))==(?);
  List_Constants(Machine(move3))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(move3))==(?);
  Context_List_Defered(Machine(move3))==(?);
  Context_List_Sets(Machine(move3))==(?);
  List_Valuable_Sets(Machine(move3))==(?);
  Inherited_List_Enumerated(Machine(move3))==(?);
  Inherited_List_Defered(Machine(move3))==(?);
  Inherited_List_Sets(Machine(move3))==(?);
  List_Enumerated(Machine(move3))==(?);
  List_Defered(Machine(move3))==(?);
  List_Sets(Machine(move3))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(move3))==(?);
  Expanded_List_HiddenConstants(Machine(move3))==(?);
  List_HiddenConstants(Machine(move3))==(?);
  External_List_HiddenConstants(Machine(move3))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(move3))==(btrue);
  Context_List_Properties(Machine(move3))==(btrue);
  Inherited_List_Properties(Machine(move3))==(btrue);
  List_Properties(Machine(move3))==(btrue)
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(move3)) == (? | ? | ? | ? | clr,move | ? | ? | ? | move3);
  List_Of_HiddenCst_Ids(Machine(move3)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(move3)) == (?);
  List_Of_VisibleVar_Ids(Machine(move3)) == (reg | ?);
  List_Of_Ids_SeenBNU(Machine(move3)) == (? : ?)
END
&
THEORY VisibleVariablesEnvX IS
  VisibleVariables(Machine(move3)) == (Type(reg) == Mvv(SetOf(btype(INTEGER,1,3)*btype(INTEGER,0,MAXINT))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(move3)) == (Type(move) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(clr) == Cst(No_type,btype(INTEGER,?,?)))
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
