Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Microcontroller))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Microcontroller))==(Machine(Microcontroller));
  Level(Machine(Microcontroller))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Microcontroller)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Microcontroller))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Microcontroller))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Microcontroller))==(?);
  List_Includes(Machine(Microcontroller))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Microcontroller))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Microcontroller))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Microcontroller))==(?);
  Context_List_Variables(Machine(Microcontroller))==(?);
  Abstract_List_Variables(Machine(Microcontroller))==(?);
  Local_List_Variables(Machine(Microcontroller))==(?);
  List_Variables(Machine(Microcontroller))==(?);
  External_List_Variables(Machine(Microcontroller))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Microcontroller))==(?);
  Abstract_List_VisibleVariables(Machine(Microcontroller))==(?);
  External_List_VisibleVariables(Machine(Microcontroller))==(?);
  Expanded_List_VisibleVariables(Machine(Microcontroller))==(?);
  List_VisibleVariables(Machine(Microcontroller))==(end,pc,memory_data);
  Internal_List_VisibleVariables(Machine(Microcontroller))==(end,pc,memory_data)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Microcontroller))==(btrue);
  Gluing_List_Invariant(Machine(Microcontroller))==(btrue);
  Expanded_List_Invariant(Machine(Microcontroller))==(btrue);
  Abstract_List_Invariant(Machine(Microcontroller))==(btrue);
  Context_List_Invariant(Machine(Microcontroller))==(btrue);
  List_Invariant(Machine(Microcontroller))==(memory_data : NATURAL --> NATURAL & pc : NATURAL & end : NATURAL & pc<=end)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(Microcontroller))==(btrue);
  Abstract_List_Assertions(Machine(Microcontroller))==(btrue);
  Context_List_Assertions(Machine(Microcontroller))==(btrue);
  List_Assertions(Machine(Microcontroller))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Microcontroller))==(@(memory_data$0).(memory_data$0 : NATURAL --> NATURAL ==> memory_data:=memory_data$0) || pc:=0 || end:=0);
  Context_List_Initialisation(Machine(Microcontroller))==(skip);
  List_Initialisation(Machine(Microcontroller))==(memory_data:: NATURAL --> NATURAL || pc:=0 || end:=0)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Microcontroller))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(Microcontroller))==(btrue);
  List_Constraints(Machine(Microcontroller))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Microcontroller))==(init_data,init,goto,isequal,copy,set_data,inc);
  List_Operations(Machine(Microcontroller))==(init_data,init,goto,isequal,copy,set_data,inc)
END
&
THEORY ListInputX IS
  List_Input(Machine(Microcontroller),init_data)==(address,value);
  List_Input(Machine(Microcontroller),init)==(pc_,end_);
  List_Input(Machine(Microcontroller),goto)==(value);
  List_Input(Machine(Microcontroller),isequal)==(address_1,value);
  List_Input(Machine(Microcontroller),copy)==(address_s,address_t);
  List_Input(Machine(Microcontroller),set_data)==(address,value);
  List_Input(Machine(Microcontroller),inc)==(address)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Microcontroller),init_data)==(?);
  List_Output(Machine(Microcontroller),init)==(?);
  List_Output(Machine(Microcontroller),goto)==(?);
  List_Output(Machine(Microcontroller),isequal)==(?);
  List_Output(Machine(Microcontroller),copy)==(?);
  List_Output(Machine(Microcontroller),set_data)==(?);
  List_Output(Machine(Microcontroller),inc)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Microcontroller),init_data)==(init_data(address,value));
  List_Header(Machine(Microcontroller),init)==(init(pc_,end_));
  List_Header(Machine(Microcontroller),goto)==(goto(value));
  List_Header(Machine(Microcontroller),isequal)==(isequal(address_1,value));
  List_Header(Machine(Microcontroller),copy)==(copy(address_s,address_t));
  List_Header(Machine(Microcontroller),set_data)==(set_data(address,value));
  List_Header(Machine(Microcontroller),inc)==(inc(address))
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Microcontroller),init_data)==(address : NATURAL & value : NATURAL & dom(memory_data<+{address|->value}) = NATURAL);
  List_Precondition(Machine(Microcontroller),init)==(pc_ : NATURAL & end_ : NATURAL & pc_<=end_);
  List_Precondition(Machine(Microcontroller),goto)==(value : NATURAL & value<=end);
  List_Precondition(Machine(Microcontroller),isequal)==(address_1 : NATURAL & value : NATURAL & (memory_data(address_1) = value => pc+1<=end) & (memory_data(address_1)/=value => pc+2<=end));
  List_Precondition(Machine(Microcontroller),copy)==(address_s : NATURAL & address_t : NATURAL & pc+1<=end & pc+1 : NATURAL);
  List_Precondition(Machine(Microcontroller),set_data)==(address : NATURAL & value : NATURAL & pc+1<=end);
  List_Precondition(Machine(Microcontroller),inc)==(address : NATURAL & memory_data(address)+1 : NATURAL & pc+1 : NATURAL & pc+1<=end & dom(memory_data<+{address|->memory_data(address)+1}) = NATURAL)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Microcontroller),inc)==(address : NATURAL & memory_data(address)+1 : NATURAL & pc+1 : NATURAL & pc+1<=end & dom(memory_data<+{address|->memory_data(address)+1}) = NATURAL | memory_data,pc:=memory_data<+{address|->memory_data(address)+1},pc+1);
  Expanded_List_Substitution(Machine(Microcontroller),set_data)==(address : NATURAL & value : NATURAL & pc+1<=end | memory_data,pc:=memory_data<+{address|->value},pc+1);
  Expanded_List_Substitution(Machine(Microcontroller),copy)==(address_s : NATURAL & address_t : NATURAL & pc+1<=end & pc+1 : NATURAL | memory_data,pc:=memory_data<+{address_t|->memory_data(address_s)},pc+1);
  Expanded_List_Substitution(Machine(Microcontroller),isequal)==(address_1 : NATURAL & value : NATURAL & (memory_data(address_1) = value => pc+1<=end) & (memory_data(address_1)/=value => pc+2<=end) | memory_data(address_1) = value ==> pc:=pc+1 [] not(memory_data(address_1) = value) ==> pc:=pc+2);
  Expanded_List_Substitution(Machine(Microcontroller),goto)==(value : NATURAL & value<=end | pc:=value);
  Expanded_List_Substitution(Machine(Microcontroller),init)==(pc_ : NATURAL & end_ : NATURAL & pc_<=end_ | pc,end:=pc_,end_);
  Expanded_List_Substitution(Machine(Microcontroller),init_data)==(address : NATURAL & value : NATURAL & dom(memory_data<+{address|->value}) = NATURAL | memory_data:=memory_data<+{address|->value});
  List_Substitution(Machine(Microcontroller),init_data)==(memory_data(address):=value);
  List_Substitution(Machine(Microcontroller),init)==(pc:=pc_ || end:=end_);
  List_Substitution(Machine(Microcontroller),goto)==(pc:=value);
  List_Substitution(Machine(Microcontroller),isequal)==(IF memory_data(address_1) = value THEN pc:=pc+1 ELSE pc:=pc+2 END);
  List_Substitution(Machine(Microcontroller),copy)==(memory_data(address_t):=memory_data(address_s) || pc:=pc+1);
  List_Substitution(Machine(Microcontroller),set_data)==(memory_data(address):=value || pc:=pc+1);
  List_Substitution(Machine(Microcontroller),inc)==(memory_data(address):=memory_data(address)+1 || pc:=pc+1)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Microcontroller))==(?);
  Inherited_List_Constants(Machine(Microcontroller))==(?);
  List_Constants(Machine(Microcontroller))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(Microcontroller))==(?);
  Context_List_Defered(Machine(Microcontroller))==(?);
  Context_List_Sets(Machine(Microcontroller))==(?);
  List_Valuable_Sets(Machine(Microcontroller))==(?);
  Inherited_List_Enumerated(Machine(Microcontroller))==(?);
  Inherited_List_Defered(Machine(Microcontroller))==(?);
  Inherited_List_Sets(Machine(Microcontroller))==(?);
  List_Enumerated(Machine(Microcontroller))==(?);
  List_Defered(Machine(Microcontroller))==(?);
  List_Sets(Machine(Microcontroller))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Microcontroller))==(?);
  Expanded_List_HiddenConstants(Machine(Microcontroller))==(?);
  List_HiddenConstants(Machine(Microcontroller))==(?);
  External_List_HiddenConstants(Machine(Microcontroller))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Microcontroller))==(btrue);
  Context_List_Properties(Machine(Microcontroller))==(btrue);
  Inherited_List_Properties(Machine(Microcontroller))==(btrue);
  List_Properties(Machine(Microcontroller))==(btrue)
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Microcontroller)) == (? | ? | ? | ? | init_data,init,goto,isequal,copy,set_data,inc | ? | ? | ? | Microcontroller);
  List_Of_HiddenCst_Ids(Machine(Microcontroller)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Microcontroller)) == (?);
  List_Of_VisibleVar_Ids(Machine(Microcontroller)) == (end,pc,memory_data | ?);
  List_Of_Ids_SeenBNU(Machine(Microcontroller)) == (? : ?)
END
&
THEORY VisibleVariablesEnvX IS
  VisibleVariables(Machine(Microcontroller)) == (Type(end) == Mvv(btype(INTEGER,?,?));Type(pc) == Mvv(btype(INTEGER,?,?));Type(memory_data) == Mvv(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Microcontroller)) == (Type(inc) == Cst(No_type,btype(INTEGER,?,?));Type(set_data) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(copy) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(isequal) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(goto) == Cst(No_type,btype(INTEGER,?,?));Type(init) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(init_data) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)))
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
