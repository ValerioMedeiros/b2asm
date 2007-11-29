Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(microcontroller))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(microcontroller))==(Machine(microcontroller));
  Level(Machine(microcontroller))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(microcontroller)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(microcontroller))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(microcontroller))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(microcontroller))==(?);
  List_Includes(Machine(microcontroller))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(microcontroller))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(microcontroller))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(microcontroller))==(?);
  Context_List_Variables(Machine(microcontroller))==(?);
  Abstract_List_Variables(Machine(microcontroller))==(?);
  Local_List_Variables(Machine(microcontroller))==(end,pc,mem);
  List_Variables(Machine(microcontroller))==(end,pc,mem);
  External_List_Variables(Machine(microcontroller))==(end,pc,mem)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(microcontroller))==(?);
  Abstract_List_VisibleVariables(Machine(microcontroller))==(?);
  External_List_VisibleVariables(Machine(microcontroller))==(?);
  Expanded_List_VisibleVariables(Machine(microcontroller))==(?);
  List_VisibleVariables(Machine(microcontroller))==(?);
  Internal_List_VisibleVariables(Machine(microcontroller))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(microcontroller))==(btrue);
  Gluing_List_Invariant(Machine(microcontroller))==(btrue);
  Expanded_List_Invariant(Machine(microcontroller))==(btrue);
  Abstract_List_Invariant(Machine(microcontroller))==(btrue);
  Context_List_Invariant(Machine(microcontroller))==(btrue);
  List_Invariant(Machine(microcontroller))==(mem : NATURAL --> NATURAL & pc : NATURAL & end : NATURAL & pc<=end)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(microcontroller))==(btrue);
  Abstract_List_Assertions(Machine(microcontroller))==(btrue);
  Context_List_Assertions(Machine(microcontroller))==(btrue);
  List_Assertions(Machine(microcontroller))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(microcontroller))==(@(mem$0).(mem$0 : NATURAL --> NATURAL ==> mem:=mem$0) || pc:=0 || end:=0);
  Context_List_Initialisation(Machine(microcontroller))==(skip);
  List_Initialisation(Machine(microcontroller))==(mem:: NATURAL --> NATURAL || pc:=0 || end:=0)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(microcontroller))==(?)
END
&
THEORY ListInstanciatedParametersX END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(microcontroller))==(btrue);
  List_Constraints(Machine(microcontroller))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(microcontroller))==(init_data,init,get_data,set_end,nop,goto,testgt,testeq,move,set_data,inc);
  List_Operations(Machine(microcontroller))==(init_data,init,get_data,set_end,nop,goto,testgt,testeq,move,set_data,inc)
END
&
THEORY ListInputX IS
  List_Input(Machine(microcontroller),init_data)==(address,value);
  List_Input(Machine(microcontroller),init)==(pc_,end_);
  List_Input(Machine(microcontroller),get_data)==(address);
  List_Input(Machine(microcontroller),set_end)==(value);
  List_Input(Machine(microcontroller),nop)==(?);
  List_Input(Machine(microcontroller),goto)==(value);
  List_Input(Machine(microcontroller),testgt)==(address1,address2);
  List_Input(Machine(microcontroller),testeq)==(address1,address2);
  List_Input(Machine(microcontroller),move)==(address_s,address_t);
  List_Input(Machine(microcontroller),set_data)==(address,value);
  List_Input(Machine(microcontroller),inc)==(address)
END
&
THEORY ListOutputX IS
  List_Output(Machine(microcontroller),init_data)==(?);
  List_Output(Machine(microcontroller),init)==(?);
  List_Output(Machine(microcontroller),get_data)==(res);
  List_Output(Machine(microcontroller),set_end)==(?);
  List_Output(Machine(microcontroller),nop)==(?);
  List_Output(Machine(microcontroller),goto)==(?);
  List_Output(Machine(microcontroller),testgt)==(?);
  List_Output(Machine(microcontroller),testeq)==(?);
  List_Output(Machine(microcontroller),move)==(?);
  List_Output(Machine(microcontroller),set_data)==(?);
  List_Output(Machine(microcontroller),inc)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(microcontroller),init_data)==(init_data(address,value));
  List_Header(Machine(microcontroller),init)==(init(pc_,end_));
  List_Header(Machine(microcontroller),get_data)==(res <-- get_data(address));
  List_Header(Machine(microcontroller),set_end)==(set_end(value));
  List_Header(Machine(microcontroller),nop)==(nop);
  List_Header(Machine(microcontroller),goto)==(goto(value));
  List_Header(Machine(microcontroller),testgt)==(testgt(address1,address2));
  List_Header(Machine(microcontroller),testeq)==(testeq(address1,address2));
  List_Header(Machine(microcontroller),move)==(move(address_s,address_t));
  List_Header(Machine(microcontroller),set_data)==(set_data(address,value));
  List_Header(Machine(microcontroller),inc)==(inc(address))
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(microcontroller),init_data)==(address : NATURAL & value : NATURAL);
  List_Precondition(Machine(microcontroller),init)==(pc_ : NATURAL & end_ : NATURAL & pc_<=end_);
  List_Precondition(Machine(microcontroller),get_data)==(address : NATURAL);
  List_Precondition(Machine(microcontroller),set_end)==(value : NATURAL & pc<=value);
  List_Precondition(Machine(microcontroller),nop)==(pc<end);
  List_Precondition(Machine(microcontroller),goto)==(value : NATURAL & value<=end);
  List_Precondition(Machine(microcontroller),testgt)==(address1 : NATURAL & address2 : NATURAL & (mem(address1)>mem(address2) => pc+1<=end) & (mem(address1)<=mem(address2) => pc+2<=end));
  List_Precondition(Machine(microcontroller),testeq)==(address1 : NATURAL & address2 : NATURAL & (mem(address1) = mem(address2) => pc+1<=end) & (mem(address1)/=mem(address2) => pc+2<=end));
  List_Precondition(Machine(microcontroller),move)==(address_s : NATURAL & address_t : NATURAL & pc+1<=end);
  List_Precondition(Machine(microcontroller),set_data)==(address : NATURAL & value : NATURAL & pc+1<=end);
  List_Precondition(Machine(microcontroller),inc)==(address : NATURAL & pc+1<=end)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(microcontroller),inc)==(address : NATURAL & pc+1<=end | mem,pc:=mem<+{address|->mem(address)+1},pc+1);
  Expanded_List_Substitution(Machine(microcontroller),set_data)==(address : NATURAL & value : NATURAL & pc+1<=end | mem,pc:=mem<+{address|->value},pc+1);
  Expanded_List_Substitution(Machine(microcontroller),move)==(address_s : NATURAL & address_t : NATURAL & pc+1<=end | mem,pc:=mem<+{address_t|->mem(address_s)},pc+1);
  Expanded_List_Substitution(Machine(microcontroller),testeq)==(address1 : NATURAL & address2 : NATURAL & (mem(address1) = mem(address2) => pc+1<=end) & (mem(address1)/=mem(address2) => pc+2<=end) | mem(address1) = mem(address2) ==> pc:=pc+1 [] not(mem(address1) = mem(address2)) ==> pc:=pc+2);
  Expanded_List_Substitution(Machine(microcontroller),testgt)==(address1 : NATURAL & address2 : NATURAL & (mem(address1)>mem(address2) => pc+1<=end) & (mem(address1)<=mem(address2) => pc+2<=end) | mem(address1)>mem(address2) ==> pc:=pc+1 [] not(mem(address1)>mem(address2)) ==> pc:=pc+2);
  Expanded_List_Substitution(Machine(microcontroller),goto)==(value : NATURAL & value<=end | pc:=value);
  Expanded_List_Substitution(Machine(microcontroller),nop)==(pc<end | pc:=pc+1);
  Expanded_List_Substitution(Machine(microcontroller),set_end)==(value : NATURAL & pc<=value | end:=value);
  Expanded_List_Substitution(Machine(microcontroller),get_data)==(address : NATURAL | res:=mem(address));
  Expanded_List_Substitution(Machine(microcontroller),init)==(pc_ : NATURAL & end_ : NATURAL & pc_<=end_ | pc,end:=pc_,end_);
  Expanded_List_Substitution(Machine(microcontroller),init_data)==(address : NATURAL & value : NATURAL | mem:=mem<+{address|->value});
  List_Substitution(Machine(microcontroller),init_data)==(mem(address):=value);
  List_Substitution(Machine(microcontroller),init)==(pc:=pc_ || end:=end_);
  List_Substitution(Machine(microcontroller),get_data)==(res:=mem(address));
  List_Substitution(Machine(microcontroller),set_end)==(end:=value);
  List_Substitution(Machine(microcontroller),nop)==(pc:=pc+1);
  List_Substitution(Machine(microcontroller),goto)==(pc:=value);
  List_Substitution(Machine(microcontroller),testgt)==(IF mem(address1)>mem(address2) THEN pc:=pc+1 ELSE pc:=pc+2 END);
  List_Substitution(Machine(microcontroller),testeq)==(IF mem(address1) = mem(address2) THEN pc:=pc+1 ELSE pc:=pc+2 END);
  List_Substitution(Machine(microcontroller),move)==(mem(address_t):=mem(address_s) || pc:=pc+1);
  List_Substitution(Machine(microcontroller),set_data)==(mem(address):=value || pc:=pc+1);
  List_Substitution(Machine(microcontroller),inc)==(mem(address):=mem(address)+1 || pc:=pc+1)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(microcontroller))==(?);
  Inherited_List_Constants(Machine(microcontroller))==(?);
  List_Constants(Machine(microcontroller))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(microcontroller))==(?);
  Context_List_Defered(Machine(microcontroller))==(?);
  Context_List_Sets(Machine(microcontroller))==(?);
  List_Valuable_Sets(Machine(microcontroller))==(?);
  Inherited_List_Enumerated(Machine(microcontroller))==(?);
  Inherited_List_Defered(Machine(microcontroller))==(?);
  Inherited_List_Sets(Machine(microcontroller))==(?);
  List_Enumerated(Machine(microcontroller))==(?);
  List_Defered(Machine(microcontroller))==(?);
  List_Sets(Machine(microcontroller))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(microcontroller))==(?);
  Expanded_List_HiddenConstants(Machine(microcontroller))==(?);
  List_HiddenConstants(Machine(microcontroller))==(?);
  External_List_HiddenConstants(Machine(microcontroller))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(microcontroller))==(btrue);
  Context_List_Properties(Machine(microcontroller))==(btrue);
  Inherited_List_Properties(Machine(microcontroller))==(btrue);
  List_Properties(Machine(microcontroller))==(btrue)
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(microcontroller)) == (? | ? | end,pc,mem | ? | init_data,init,get_data,set_end,nop,goto,testgt,testeq,move,set_data,inc | ? | ? | ? | microcontroller);
  List_Of_HiddenCst_Ids(Machine(microcontroller)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(microcontroller)) == (?);
  List_Of_VisibleVar_Ids(Machine(microcontroller)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(microcontroller)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(microcontroller)) == (Type(end) == Mvl(btype(INTEGER,?,?));Type(pc) == Mvl(btype(INTEGER,?,?));Type(mem) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(microcontroller)) == (Type(inc) == Cst(No_type,btype(INTEGER,?,?));Type(set_data) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(move) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(testeq) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(testgt) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(goto) == Cst(No_type,btype(INTEGER,?,?));Type(nop) == Cst(No_type,No_type);Type(set_end) == Cst(No_type,btype(INTEGER,?,?));Type(get_data) == Cst(btype(INTEGER,?,?),btype(INTEGER,?,?));Type(init) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(init_data) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  Observers(Machine(microcontroller)) == (Type(get_data) == Cst(btype(INTEGER,?,?),btype(INTEGER,?,?)))
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
