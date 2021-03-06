Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(traffic_light))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(traffic_light))==(Machine(traffic_light));
  Level(Machine(traffic_light))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(traffic_light)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(traffic_light))==(definitions)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(traffic_light))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(traffic_light))==(?);
  List_Includes(Machine(traffic_light))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(traffic_light))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(traffic_light))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(traffic_light))==(?);
  Context_List_Variables(Machine(traffic_light))==(?);
  Abstract_List_Variables(Machine(traffic_light))==(?);
  Local_List_Variables(Machine(traffic_light))==(color);
  List_Variables(Machine(traffic_light))==(color);
  External_List_Variables(Machine(traffic_light))==(color)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(traffic_light))==(?);
  Abstract_List_VisibleVariables(Machine(traffic_light))==(?);
  External_List_VisibleVariables(Machine(traffic_light))==(?);
  Expanded_List_VisibleVariables(Machine(traffic_light))==(?);
  List_VisibleVariables(Machine(traffic_light))==(?);
  Internal_List_VisibleVariables(Machine(traffic_light))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(traffic_light))==(btrue);
  Gluing_List_Invariant(Machine(traffic_light))==(btrue);
  Expanded_List_Invariant(Machine(traffic_light))==(btrue);
  Abstract_List_Invariant(Machine(traffic_light))==(btrue);
  Context_List_Invariant(Machine(traffic_light))==(btrue);
  List_Invariant(Machine(traffic_light))==(color : COLOR)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(traffic_light))==(btrue);
  Abstract_List_Assertions(Machine(traffic_light))==(btrue);
  Context_List_Assertions(Machine(traffic_light))==(!c.(c : COLOR => c = green or c = yellow or c = red));
  List_Assertions(Machine(traffic_light))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(traffic_light))==(@(color$0).(color$0 : COLOR ==> color:=color$0));
  Context_List_Initialisation(Machine(traffic_light))==(skip);
  List_Initialisation(Machine(traffic_light))==(color:: COLOR)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(traffic_light))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(traffic_light),Machine(definitions))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(traffic_light))==(btrue);
  List_Constraints(Machine(traffic_light))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(traffic_light))==(advance);
  List_Operations(Machine(traffic_light))==(advance)
END
&
THEORY ListInputX IS
  List_Input(Machine(traffic_light),advance)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(traffic_light),advance)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(traffic_light),advance)==(advance)
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(traffic_light),advance)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(traffic_light),advance)==(btrue | not(color = red) & not(color = yellow) & color = green ==> color:=yellow [] not(color = green) & not(color = red) & color = yellow ==> color:=red [] not(color = green) & not(color = yellow) & color = red ==> color:=green [] not(color = green) & not(color = yellow) & not(color = red) ==> skip);
  List_Substitution(Machine(traffic_light),advance)==(CASE color OF EITHER green THEN color:=yellow OR yellow THEN color:=red OR red THEN color:=green END END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(traffic_light))==(?);
  Inherited_List_Constants(Machine(traffic_light))==(?);
  List_Constants(Machine(traffic_light))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(traffic_light),COLOR)==({green,yellow,red});
  Context_List_Enumerated(Machine(traffic_light))==(COLOR);
  Context_List_Defered(Machine(traffic_light))==(?);
  Context_List_Sets(Machine(traffic_light))==(COLOR);
  List_Valuable_Sets(Machine(traffic_light))==(?);
  Inherited_List_Enumerated(Machine(traffic_light))==(?);
  Inherited_List_Defered(Machine(traffic_light))==(?);
  Inherited_List_Sets(Machine(traffic_light))==(?);
  List_Enumerated(Machine(traffic_light))==(?);
  List_Defered(Machine(traffic_light))==(?);
  List_Sets(Machine(traffic_light))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(traffic_light))==(?);
  Expanded_List_HiddenConstants(Machine(traffic_light))==(?);
  List_HiddenConstants(Machine(traffic_light))==(?);
  External_List_HiddenConstants(Machine(traffic_light))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(traffic_light))==(btrue);
  Context_List_Properties(Machine(traffic_light))==(not(COLOR = {}));
  Inherited_List_Properties(Machine(traffic_light))==(btrue);
  List_Properties(Machine(traffic_light))==(btrue)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(traffic_light),Machine(definitions))==(?);
  Seen_Context_List_Enumerated(Machine(traffic_light))==(?);
  Seen_Context_List_Invariant(Machine(traffic_light))==(btrue);
  Seen_Context_List_Assertions(Machine(traffic_light))==(btrue);
  Seen_Context_List_Properties(Machine(traffic_light))==(btrue);
  Seen_List_Constraints(Machine(traffic_light))==(btrue);
  Seen_List_Operations(Machine(traffic_light),Machine(definitions))==(?);
  Seen_Expanded_List_Invariant(Machine(traffic_light),Machine(definitions))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(traffic_light)) == (? | ? | color | ? | advance | ? | seen(Machine(definitions)) | ? | traffic_light);
  List_Of_HiddenCst_Ids(Machine(traffic_light)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(traffic_light)) == (?);
  List_Of_VisibleVar_Ids(Machine(traffic_light)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(traffic_light)) == (? : ?);
  List_Of_Ids(Machine(definitions)) == (COLOR,green,yellow,red | ? | ? | ? | ? | ? | ? | ? | definitions);
  List_Of_HiddenCst_Ids(Machine(definitions)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(definitions)) == (?);
  List_Of_VisibleVar_Ids(Machine(definitions)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(definitions)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(traffic_light)) == (Type(color) == Mvl(etype(COLOR,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(traffic_light)) == (Type(advance) == Cst(No_type,No_type))
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
