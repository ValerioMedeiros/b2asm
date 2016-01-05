Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Test))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Test))==(Machine(Test));
  Level(Machine(Test))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Test)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Test))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Test))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Test))==(?);
  List_Includes(Machine(Test))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Test))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Test))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Test))==(?);
  Context_List_Variables(Machine(Test))==(?);
  Abstract_List_Variables(Machine(Test))==(?);
  Local_List_Variables(Machine(Test))==(block_code,result,rest,divisor,number);
  List_Variables(Machine(Test))==(block_code,result,rest,divisor,number);
  External_List_Variables(Machine(Test))==(block_code,result,rest,divisor,number)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Test))==(?);
  Abstract_List_VisibleVariables(Machine(Test))==(?);
  External_List_VisibleVariables(Machine(Test))==(?);
  Expanded_List_VisibleVariables(Machine(Test))==(?);
  List_VisibleVariables(Machine(Test))==(?);
  Internal_List_VisibleVariables(Machine(Test))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Test))==(btrue);
  Gluing_List_Invariant(Machine(Test))==(btrue);
  Expanded_List_Invariant(Machine(Test))==(btrue);
  Abstract_List_Invariant(Machine(Test))==(btrue);
  Context_List_Invariant(Machine(Test))==(btrue);
  List_Invariant(Machine(Test))==(number: INT & divisor: INT & rest: INT & result: INT & block_code: 1..4 & (block_code = 1 => 1 = 1) & (block_code = 2 => result = 0 & rest>=0 & rest<=number) & (block_code = 3 => result<=number & result>=0 & (rest>=0 => rest = number-result*divisor) & (rest<0 => result = number/divisor)) & (block_code = 4 => result = number/divisor))
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(Test))==(btrue);
  Abstract_List_Assertions(Machine(Test))==(btrue);
  Context_List_Assertions(Machine(Test))==(btrue);
  List_Assertions(Machine(Test))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Test))==(number,divisor,rest,block_code,result:=0,0,0,1,0);
  Context_List_Initialisation(Machine(Test))==(skip);
  List_Initialisation(Machine(Test))==(number:=0 || divisor:=0 || rest:=0 || block_code:=1 || result:=0)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Test))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(Test))==(btrue);
  List_Constraints(Machine(Test))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Test))==(b1_code,b2_code,b3_code);
  List_Operations(Machine(Test))==(b1_code,b2_code,b3_code)
END
&
THEORY ListInputX IS
  List_Input(Machine(Test),b1_code)==(p_number,p_divisor);
  List_Input(Machine(Test),b2_code)==(?);
  List_Input(Machine(Test),b3_code)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Test),b1_code)==(?);
  List_Output(Machine(Test),b2_code)==(?);
  List_Output(Machine(Test),b3_code)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Test),b1_code)==(b1_code(p_number,p_divisor));
  List_Header(Machine(Test),b2_code)==(b2_code);
  List_Header(Machine(Test),b3_code)==(b3_code)
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Test),b1_code)==(p_number: 0..255 & p_divisor: INT & block_code = 1);
  List_Precondition(Machine(Test),b2_code)==(number: 0..255 & divisor: 0..255 & block_code = 2);
  List_Precondition(Machine(Test),b3_code)==(number: INT & divisor: INT & block_code = 3)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Test),b3_code)==(number: INT & divisor: INT & block_code = 3 | rest>0 ==> result,block_code:=result+1,2 [] not(rest>0) ==> block_code:=4);
  Expanded_List_Substitution(Machine(Test),b2_code)==(number: 0..255 & divisor: 0..255 & block_code = 2 | rest,block_code:=number-result*divisor,3);
  Expanded_List_Substitution(Machine(Test),b1_code)==(p_number: 0..255 & p_divisor: INT & block_code = 1 | number,divisor,result,rest,block_code:=p_number,p_divisor,0,p_number,2);
  List_Substitution(Machine(Test),b1_code)==(number:=p_number || divisor:=p_divisor || result:=0 || rest:=p_number || block_code:=2);
  List_Substitution(Machine(Test),b2_code)==(rest:=number-result*divisor || block_code:=3);
  List_Substitution(Machine(Test),b3_code)==(IF rest>0 THEN result:=result+1 || block_code:=2 ELSE block_code:=4 END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Test))==(?);
  Inherited_List_Constants(Machine(Test))==(?);
  List_Constants(Machine(Test))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(Test))==(?);
  Context_List_Defered(Machine(Test))==(?);
  Context_List_Sets(Machine(Test))==(?);
  List_Valuable_Sets(Machine(Test))==(?);
  Inherited_List_Enumerated(Machine(Test))==(?);
  Inherited_List_Defered(Machine(Test))==(?);
  Inherited_List_Sets(Machine(Test))==(?);
  List_Enumerated(Machine(Test))==(?);
  List_Defered(Machine(Test))==(?);
  List_Sets(Machine(Test))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Test))==(?);
  Expanded_List_HiddenConstants(Machine(Test))==(?);
  List_HiddenConstants(Machine(Test))==(?);
  External_List_HiddenConstants(Machine(Test))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Test))==(btrue);
  Context_List_Properties(Machine(Test))==(btrue);
  Inherited_List_Properties(Machine(Test))==(btrue);
  List_Properties(Machine(Test))==(btrue)
END
&
THEORY ListSeenInfoX END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(Test),b1_code)==(?);
  List_ANY_Var(Machine(Test),b2_code)==(?);
  List_ANY_Var(Machine(Test),b3_code)==(?);
  List_ANY_Var(Machine(Test),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Test)) == (? | ? | block_code,result,rest,divisor,number | ? | b1_code,b2_code,b3_code | ? | ? | ? | Test);
  List_Of_HiddenCst_Ids(Machine(Test)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Test)) == (?);
  List_Of_VisibleVar_Ids(Machine(Test)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Test)) == (?: ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Test)) == (Type(block_code) == Mvl(btype(INTEGER,?,?));Type(result) == Mvl(btype(INTEGER,?,?));Type(rest) == Mvl(btype(INTEGER,?,?));Type(divisor) == Mvl(btype(INTEGER,?,?));Type(number) == Mvl(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Test)) == (Type(b3_code) == Cst(No_type,No_type);Type(b2_code) == Cst(No_type,No_type);Type(b1_code) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)))
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
  variant_clause_mandatory == KO
END
)
