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
  Local_List_Variables(Machine(microcontroller))==(end,pc,w,memory_data);
  List_Variables(Machine(microcontroller))==(end,pc,w,memory_data);
  External_List_Variables(Machine(microcontroller))==(end,pc,w,memory_data)
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
  List_Invariant(Machine(microcontroller))==(memory_data : NATURAL --> NATURAL & w : NATURAL & pc : NATURAL & end : NATURAL & pc<=end)
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
  Expanded_List_Initialisation(Machine(microcontroller))==(@(memory_data$0).(memory_data$0 : NATURAL --> NATURAL ==> memory_data:=memory_data$0) || w:=0 || pc:=0 || end:=0);
  Context_List_Initialisation(Machine(microcontroller))==(skip);
  List_Initialisation(Machine(microcontroller))==(memory_data:: NATURAL --> NATURAL || w:=0 || pc:=0 || end:=0)
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
  Internal_List_Operations(Machine(microcontroller))==(init_data,init,get_data,get_pc,set_end,get_end,nop,set_w,get_w,goto,iszero,isnotzero_simple,testeq,testgt,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div);
  List_Operations(Machine(microcontroller))==(init_data,init,get_data,get_pc,set_end,get_end,nop,set_w,get_w,goto,iszero,isnotzero_simple,testeq,testgt,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div)
END
&
THEORY ListInputX IS
  List_Input(Machine(microcontroller),init_data)==(address,value);
  List_Input(Machine(microcontroller),init)==(pc_,end_);
  List_Input(Machine(microcontroller),get_data)==(address);
  List_Input(Machine(microcontroller),get_pc)==(?);
  List_Input(Machine(microcontroller),set_end)==(value);
  List_Input(Machine(microcontroller),get_end)==(?);
  List_Input(Machine(microcontroller),nop)==(?);
  List_Input(Machine(microcontroller),set_w)==(value);
  List_Input(Machine(microcontroller),get_w)==(?);
  List_Input(Machine(microcontroller),goto)==(value);
  List_Input(Machine(microcontroller),iszero)==(address);
  List_Input(Machine(microcontroller),isnotzero_simple)==(address);
  List_Input(Machine(microcontroller),testeq)==(address1,address2);
  List_Input(Machine(microcontroller),testgt)==(address1,address2);
  List_Input(Machine(microcontroller),move)==(address_s,address_t);
  List_Input(Machine(microcontroller),move_w_m)==(address);
  List_Input(Machine(microcontroller),move_m_w)==(address);
  List_Input(Machine(microcontroller),reset)==(address);
  List_Input(Machine(microcontroller),reset_w)==(?);
  List_Input(Machine(microcontroller),set_data)==(address,value);
  List_Input(Machine(microcontroller),inc)==(address);
  List_Input(Machine(microcontroller),dec)==(address);
  List_Input(Machine(microcontroller),add)==(address1,address2);
  List_Input(Machine(microcontroller),sub)==(address1,address2);
  List_Input(Machine(microcontroller),mul)==(address1,address2);
  List_Input(Machine(microcontroller),div)==(address1,address2)
END
&
THEORY ListOutputX IS
  List_Output(Machine(microcontroller),init_data)==(?);
  List_Output(Machine(microcontroller),init)==(?);
  List_Output(Machine(microcontroller),get_data)==(res);
  List_Output(Machine(microcontroller),get_pc)==(res);
  List_Output(Machine(microcontroller),set_end)==(?);
  List_Output(Machine(microcontroller),get_end)==(res);
  List_Output(Machine(microcontroller),nop)==(?);
  List_Output(Machine(microcontroller),set_w)==(?);
  List_Output(Machine(microcontroller),get_w)==(res);
  List_Output(Machine(microcontroller),goto)==(?);
  List_Output(Machine(microcontroller),iszero)==(?);
  List_Output(Machine(microcontroller),isnotzero_simple)==(?);
  List_Output(Machine(microcontroller),testeq)==(?);
  List_Output(Machine(microcontroller),testgt)==(?);
  List_Output(Machine(microcontroller),move)==(?);
  List_Output(Machine(microcontroller),move_w_m)==(?);
  List_Output(Machine(microcontroller),move_m_w)==(?);
  List_Output(Machine(microcontroller),reset)==(?);
  List_Output(Machine(microcontroller),reset_w)==(?);
  List_Output(Machine(microcontroller),set_data)==(?);
  List_Output(Machine(microcontroller),inc)==(?);
  List_Output(Machine(microcontroller),dec)==(?);
  List_Output(Machine(microcontroller),add)==(?);
  List_Output(Machine(microcontroller),sub)==(?);
  List_Output(Machine(microcontroller),mul)==(?);
  List_Output(Machine(microcontroller),div)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(microcontroller),init_data)==(init_data(address,value));
  List_Header(Machine(microcontroller),init)==(init(pc_,end_));
  List_Header(Machine(microcontroller),get_data)==(res <-- get_data(address));
  List_Header(Machine(microcontroller),get_pc)==(res <-- get_pc);
  List_Header(Machine(microcontroller),set_end)==(set_end(value));
  List_Header(Machine(microcontroller),get_end)==(res <-- get_end);
  List_Header(Machine(microcontroller),nop)==(nop);
  List_Header(Machine(microcontroller),set_w)==(set_w(value));
  List_Header(Machine(microcontroller),get_w)==(res <-- get_w);
  List_Header(Machine(microcontroller),goto)==(goto(value));
  List_Header(Machine(microcontroller),iszero)==(iszero(address));
  List_Header(Machine(microcontroller),isnotzero_simple)==(isnotzero_simple(address));
  List_Header(Machine(microcontroller),testeq)==(testeq(address1,address2));
  List_Header(Machine(microcontroller),testgt)==(testgt(address1,address2));
  List_Header(Machine(microcontroller),move)==(move(address_s,address_t));
  List_Header(Machine(microcontroller),move_w_m)==(move_w_m(address));
  List_Header(Machine(microcontroller),move_m_w)==(move_m_w(address));
  List_Header(Machine(microcontroller),reset)==(reset(address));
  List_Header(Machine(microcontroller),reset_w)==(reset_w);
  List_Header(Machine(microcontroller),set_data)==(set_data(address,value));
  List_Header(Machine(microcontroller),inc)==(inc(address));
  List_Header(Machine(microcontroller),dec)==(dec(address));
  List_Header(Machine(microcontroller),add)==(add(address1,address2));
  List_Header(Machine(microcontroller),sub)==(sub(address1,address2));
  List_Header(Machine(microcontroller),mul)==(mul(address1,address2));
  List_Header(Machine(microcontroller),div)==(div(address1,address2))
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(microcontroller),init_data)==(address : NATURAL & value : NATURAL & dom(memory_data<+{address|->value}) = NATURAL);
  List_Precondition(Machine(microcontroller),init)==(pc_ : NATURAL & end_ : NATURAL & pc_<=end_);
  List_Precondition(Machine(microcontroller),get_data)==(address : NATURAL);
  List_Precondition(Machine(microcontroller),get_pc)==(btrue);
  List_Precondition(Machine(microcontroller),set_end)==(value : NATURAL & pc<=value);
  List_Precondition(Machine(microcontroller),get_end)==(btrue);
  List_Precondition(Machine(microcontroller),nop)==(pc<end);
  List_Precondition(Machine(microcontroller),set_w)==(value : NATURAL);
  List_Precondition(Machine(microcontroller),get_w)==(btrue);
  List_Precondition(Machine(microcontroller),goto)==(value : NATURAL & value<=end);
  List_Precondition(Machine(microcontroller),iszero)==(address : NATURAL & (memory_data(address) = 0 => pc+1 : NATURAL & pc+1<=end) & (memory_data(address)/=0 => pc+2 : NATURAL & pc+2<=end));
  List_Precondition(Machine(microcontroller),isnotzero_simple)==(address : NATURAL & (memory_data(address) = 0 => pc+3 : NATURAL & pc+3<=end) & (memory_data(address)/=0 => pc+1 : NATURAL & pc+1<=end));
  List_Precondition(Machine(microcontroller),testeq)==(address1 : NATURAL & address2 : NATURAL & (memory_data(address1) = memory_data(address2) => pc+1 : NATURAL & pc+1<=end) & (memory_data(address1)/=memory_data(address2) => pc+2 : NATURAL & pc+2<=end));
  List_Precondition(Machine(microcontroller),testgt)==(address1 : NATURAL & address2 : NATURAL & (memory_data(address1)>memory_data(address2) => pc+1 : NATURAL & pc+1<=end) & (memory_data(address1)<=memory_data(address2) => pc+2 : NATURAL & pc+2<=end));
  List_Precondition(Machine(microcontroller),move)==(address_s : NATURAL & address_t : NATURAL & pc+1<=end & pc+1 : NATURAL);
  List_Precondition(Machine(microcontroller),move_w_m)==(address : NATURAL & pc+1<=end & pc+1 : NATURAL);
  List_Precondition(Machine(microcontroller),move_m_w)==(address : NATURAL & pc+1<=end & pc+1 : NATURAL);
  List_Precondition(Machine(microcontroller),reset)==(address : NATURAL & memory_data(address) : NATURAL & pc+1<=end & pc+1 : NATURAL);
  List_Precondition(Machine(microcontroller),reset_w)==(btrue);
  List_Precondition(Machine(microcontroller),set_data)==(address : NATURAL & value : NATURAL & pc+1 : NATURAL & pc+1<=end);
  List_Precondition(Machine(microcontroller),inc)==(address : NATURAL & memory_data(address)+1 : NATURAL & pc+1 : NATURAL & pc+1<=end & dom(memory_data<+{address|->memory_data(address)+1}) = NATURAL);
  List_Precondition(Machine(microcontroller),dec)==(address : NATURAL & memory_data(address)-1 : NATURAL & pc+1 : NATURAL & pc+1<=end & dom(memory_data<+{address|->memory_data(address)-1}) = NATURAL);
  List_Precondition(Machine(microcontroller),add)==(address1 : NATURAL & address2 : NATURAL & memory_data(address1)+memory_data(address2) : NATURAL);
  List_Precondition(Machine(microcontroller),sub)==(address1 : NATURAL & address2 : NATURAL & memory_data(address1)-memory_data(address2) : NATURAL);
  List_Precondition(Machine(microcontroller),mul)==(address1 : NATURAL & address2 : NATURAL & memory_data(address1)*memory_data(address2) : NATURAL);
  List_Precondition(Machine(microcontroller),div)==(address1 : NATURAL & address2 : NATURAL & memory_data(address1)/memory_data(address2) : NATURAL)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(microcontroller),div)==(address1 : NATURAL & address2 : NATURAL & memory_data(address1)/memory_data(address2) : NATURAL | w:=memory_data(address1)/memory_data(address2));
  Expanded_List_Substitution(Machine(microcontroller),mul)==(address1 : NATURAL & address2 : NATURAL & memory_data(address1)*memory_data(address2) : NATURAL | w:=memory_data(address1)*memory_data(address2));
  Expanded_List_Substitution(Machine(microcontroller),sub)==(address1 : NATURAL & address2 : NATURAL & memory_data(address1)-memory_data(address2) : NATURAL | w:=memory_data(address1)-memory_data(address2));
  Expanded_List_Substitution(Machine(microcontroller),add)==(address1 : NATURAL & address2 : NATURAL & memory_data(address1)+memory_data(address2) : NATURAL | w:=memory_data(address1)+memory_data(address2));
  Expanded_List_Substitution(Machine(microcontroller),dec)==(address : NATURAL & memory_data(address)-1 : NATURAL & pc+1 : NATURAL & pc+1<=end & dom(memory_data<+{address|->memory_data(address)-1}) = NATURAL | memory_data,pc:=memory_data<+{address|->memory_data(address)-1},pc+1);
  Expanded_List_Substitution(Machine(microcontroller),inc)==(address : NATURAL & memory_data(address)+1 : NATURAL & pc+1 : NATURAL & pc+1<=end & dom(memory_data<+{address|->memory_data(address)+1}) = NATURAL | memory_data,pc:=memory_data<+{address|->memory_data(address)+1},pc+1);
  Expanded_List_Substitution(Machine(microcontroller),set_data)==(address : NATURAL & value : NATURAL & pc+1 : NATURAL & pc+1<=end | memory_data,pc:=memory_data<+{address|->value},pc+1);
  Expanded_List_Substitution(Machine(microcontroller),reset_w)==(btrue | w:=0);
  Expanded_List_Substitution(Machine(microcontroller),reset)==(address : NATURAL & memory_data(address) : NATURAL & pc+1<=end & pc+1 : NATURAL | memory_data,pc:=memory_data<+{address|->0},pc+1);
  Expanded_List_Substitution(Machine(microcontroller),move_m_w)==(address : NATURAL & pc+1<=end & pc+1 : NATURAL | w,pc:=memory_data(address),pc+1);
  Expanded_List_Substitution(Machine(microcontroller),move_w_m)==(address : NATURAL & pc+1<=end & pc+1 : NATURAL | memory_data,pc:=memory_data<+{address|->w},pc+1);
  Expanded_List_Substitution(Machine(microcontroller),move)==(address_s : NATURAL & address_t : NATURAL & pc+1<=end & pc+1 : NATURAL | memory_data,pc:=memory_data<+{address_t|->memory_data(address_s)},pc+1);
  Expanded_List_Substitution(Machine(microcontroller),testgt)==(address1 : NATURAL & address2 : NATURAL & (memory_data(address1)>memory_data(address2) => pc+1 : NATURAL & pc+1<=end) & (memory_data(address1)<=memory_data(address2) => pc+2 : NATURAL & pc+2<=end) | memory_data(address1)>memory_data(address2) ==> pc:=pc+1 [] not(memory_data(address1)>memory_data(address2)) ==> pc:=pc+2);
  Expanded_List_Substitution(Machine(microcontroller),testeq)==(address1 : NATURAL & address2 : NATURAL & (memory_data(address1) = memory_data(address2) => pc+1 : NATURAL & pc+1<=end) & (memory_data(address1)/=memory_data(address2) => pc+2 : NATURAL & pc+2<=end) | memory_data(address1) = memory_data(address2) ==> pc:=pc+1 [] not(memory_data(address1) = memory_data(address2)) ==> pc:=pc+2);
  Expanded_List_Substitution(Machine(microcontroller),isnotzero_simple)==(address : NATURAL & (memory_data(address) = 0 => pc+3 : NATURAL & pc+3<=end) & (memory_data(address)/=0 => pc+1 : NATURAL & pc+1<=end) | memory_data(address) = 0 ==> pc:=pc+3 [] not(memory_data(address) = 0) ==> pc:=pc+1);
  Expanded_List_Substitution(Machine(microcontroller),iszero)==(address : NATURAL & (memory_data(address) = 0 => pc+1 : NATURAL & pc+1<=end) & (memory_data(address)/=0 => pc+2 : NATURAL & pc+2<=end) | memory_data(address) = 0 ==> pc:=pc+1 [] not(memory_data(address) = 0) ==> pc:=pc+2);
  Expanded_List_Substitution(Machine(microcontroller),goto)==(value : NATURAL & value<=end | pc:=value);
  Expanded_List_Substitution(Machine(microcontroller),get_w)==(btrue | res:=w);
  Expanded_List_Substitution(Machine(microcontroller),set_w)==(value : NATURAL | w:=value);
  Expanded_List_Substitution(Machine(microcontroller),nop)==(pc<end | pc:=pc+1);
  Expanded_List_Substitution(Machine(microcontroller),get_end)==(btrue | res:=end);
  Expanded_List_Substitution(Machine(microcontroller),set_end)==(value : NATURAL & pc<=value | end:=value);
  Expanded_List_Substitution(Machine(microcontroller),get_pc)==(btrue | res:=pc);
  Expanded_List_Substitution(Machine(microcontroller),get_data)==(address : NATURAL | res:=memory_data(address));
  Expanded_List_Substitution(Machine(microcontroller),init)==(pc_ : NATURAL & end_ : NATURAL & pc_<=end_ | pc,end:=pc_,end_);
  Expanded_List_Substitution(Machine(microcontroller),init_data)==(address : NATURAL & value : NATURAL & dom(memory_data<+{address|->value}) = NATURAL | memory_data:=memory_data<+{address|->value});
  List_Substitution(Machine(microcontroller),init_data)==(memory_data(address):=value);
  List_Substitution(Machine(microcontroller),init)==(pc:=pc_ || end:=end_);
  List_Substitution(Machine(microcontroller),get_data)==(res:=memory_data(address));
  List_Substitution(Machine(microcontroller),get_pc)==(res:=pc);
  List_Substitution(Machine(microcontroller),set_end)==(end:=value);
  List_Substitution(Machine(microcontroller),get_end)==(res:=end);
  List_Substitution(Machine(microcontroller),nop)==(pc:=pc+1);
  List_Substitution(Machine(microcontroller),set_w)==(w:=value);
  List_Substitution(Machine(microcontroller),get_w)==(res:=w);
  List_Substitution(Machine(microcontroller),goto)==(pc:=value);
  List_Substitution(Machine(microcontroller),iszero)==(IF memory_data(address) = 0 THEN pc:=pc+1 ELSE pc:=pc+2 END);
  List_Substitution(Machine(microcontroller),isnotzero_simple)==(IF memory_data(address) = 0 THEN pc:=pc+3 ELSE pc:=pc+1 END);
  List_Substitution(Machine(microcontroller),testeq)==(IF memory_data(address1) = memory_data(address2) THEN pc:=pc+1 ELSE pc:=pc+2 END);
  List_Substitution(Machine(microcontroller),testgt)==(IF memory_data(address1)>memory_data(address2) THEN pc:=pc+1 ELSE pc:=pc+2 END);
  List_Substitution(Machine(microcontroller),move)==(memory_data(address_t):=memory_data(address_s) || pc:=pc+1);
  List_Substitution(Machine(microcontroller),move_w_m)==(memory_data(address):=w || pc:=pc+1);
  List_Substitution(Machine(microcontroller),move_m_w)==(w:=memory_data(address) || pc:=pc+1);
  List_Substitution(Machine(microcontroller),reset)==(memory_data(address):=0 || pc:=pc+1);
  List_Substitution(Machine(microcontroller),reset_w)==(w:=0);
  List_Substitution(Machine(microcontroller),set_data)==(memory_data(address):=value || pc:=pc+1);
  List_Substitution(Machine(microcontroller),inc)==(memory_data(address):=memory_data(address)+1 || pc:=pc+1);
  List_Substitution(Machine(microcontroller),dec)==(memory_data(address):=memory_data(address)-1 || pc:=pc+1);
  List_Substitution(Machine(microcontroller),add)==(w:=memory_data(address1)+memory_data(address2));
  List_Substitution(Machine(microcontroller),sub)==(w:=memory_data(address1)-memory_data(address2));
  List_Substitution(Machine(microcontroller),mul)==(w:=memory_data(address1)*memory_data(address2));
  List_Substitution(Machine(microcontroller),div)==(w:=memory_data(address1)/memory_data(address2))
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
  List_Of_Ids(Machine(microcontroller)) == (? | ? | end,pc,w,memory_data | ? | init_data,init,get_data,get_pc,set_end,get_end,nop,set_w,get_w,goto,iszero,isnotzero_simple,testeq,testgt,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div | ? | ? | ? | microcontroller);
  List_Of_HiddenCst_Ids(Machine(microcontroller)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(microcontroller)) == (?);
  List_Of_VisibleVar_Ids(Machine(microcontroller)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(microcontroller)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(microcontroller)) == (Type(end) == Mvl(btype(INTEGER,?,?));Type(pc) == Mvl(btype(INTEGER,?,?));Type(w) == Mvl(btype(INTEGER,?,?));Type(memory_data) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(microcontroller)) == (Type(div) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(mul) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(sub) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(add) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(dec) == Cst(No_type,btype(INTEGER,?,?));Type(inc) == Cst(No_type,btype(INTEGER,?,?));Type(set_data) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(reset_w) == Cst(No_type,No_type);Type(reset) == Cst(No_type,btype(INTEGER,?,?));Type(move_m_w) == Cst(No_type,btype(INTEGER,?,?));Type(move_w_m) == Cst(No_type,btype(INTEGER,?,?));Type(move) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(testgt) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(testeq) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(isnotzero_simple) == Cst(No_type,btype(INTEGER,?,?));Type(iszero) == Cst(No_type,btype(INTEGER,?,?));Type(goto) == Cst(No_type,btype(INTEGER,?,?));Type(get_w) == Cst(btype(INTEGER,?,?),No_type);Type(set_w) == Cst(No_type,btype(INTEGER,?,?));Type(nop) == Cst(No_type,No_type);Type(get_end) == Cst(btype(INTEGER,?,?),No_type);Type(set_end) == Cst(No_type,btype(INTEGER,?,?));Type(get_pc) == Cst(btype(INTEGER,?,?),No_type);Type(get_data) == Cst(btype(INTEGER,?,?),btype(INTEGER,?,?));Type(init) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(init_data) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  Observers(Machine(microcontroller)) == (Type(get_w) == Cst(btype(INTEGER,?,?),No_type);Type(get_end) == Cst(btype(INTEGER,?,?),No_type);Type(get_pc) == Cst(btype(INTEGER,?,?),No_type);Type(get_data) == Cst(btype(INTEGER,?,?),btype(INTEGER,?,?)))
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
