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
  Local_List_Variables(Machine(Microcontroller))==(end,pc,w,stack,memory_data);
  List_Variables(Machine(Microcontroller))==(end,pc,w,stack,memory_data);
  External_List_Variables(Machine(Microcontroller))==(end,pc,w,stack,memory_data)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Microcontroller))==(?);
  Abstract_List_VisibleVariables(Machine(Microcontroller))==(?);
  External_List_VisibleVariables(Machine(Microcontroller))==(?);
  Expanded_List_VisibleVariables(Machine(Microcontroller))==(?);
  List_VisibleVariables(Machine(Microcontroller))==(?);
  Internal_List_VisibleVariables(Machine(Microcontroller))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Microcontroller))==(btrue);
  Gluing_List_Invariant(Machine(Microcontroller))==(btrue);
  Expanded_List_Invariant(Machine(Microcontroller))==(btrue);
  Abstract_List_Invariant(Machine(Microcontroller))==(btrue);
  Context_List_Invariant(Machine(Microcontroller))==(btrue);
  List_Invariant(Machine(Microcontroller))==(memory_data : NATURAL --> NATURAL & stack : 1..8 +-> NATURAL & card(stack)<=8 & w : NATURAL & pc : NATURAL & end : NATURAL & pc<=end)
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
  Expanded_List_Initialisation(Machine(Microcontroller))==(@(memory_data$0).(memory_data$0 : NATURAL --> NATURAL ==> memory_data:=memory_data$0) || stack:={1|->0,2|->0,3|->0,4|->0,5|->0,6|->0,7|->0,8|->0} || w:=0 || pc:=0 || end:=0);
  Context_List_Initialisation(Machine(Microcontroller))==(skip);
  List_Initialisation(Machine(Microcontroller))==(memory_data:: NATURAL --> NATURAL || stack:={1|->0,2|->0,3|->0,4|->0,5|->0,6|->0,7|->0,8|->0} || w:=0 || pc:=0 || end:=0)
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
  Internal_List_Operations(Machine(Microcontroller))==(init_data,init,push,pop_1,pop_2,get_data,get_pc,set_end,get_end,set_w,get_w,nop,goto,iszero,isequal,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div);
  List_Operations(Machine(Microcontroller))==(init_data,init,push,pop_1,pop_2,get_data,get_pc,set_end,get_end,set_w,get_w,nop,goto,iszero,isequal,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div)
END
&
THEORY ListInputX IS
  List_Input(Machine(Microcontroller),init_data)==(address,value);
  List_Input(Machine(Microcontroller),init)==(pc_,end_);
  List_Input(Machine(Microcontroller),push)==(?);
  List_Input(Machine(Microcontroller),pop_1)==(?);
  List_Input(Machine(Microcontroller),pop_2)==(?);
  List_Input(Machine(Microcontroller),get_data)==(address);
  List_Input(Machine(Microcontroller),get_pc)==(?);
  List_Input(Machine(Microcontroller),set_end)==(value);
  List_Input(Machine(Microcontroller),get_end)==(?);
  List_Input(Machine(Microcontroller),set_w)==(value);
  List_Input(Machine(Microcontroller),get_w)==(?);
  List_Input(Machine(Microcontroller),nop)==(?);
  List_Input(Machine(Microcontroller),goto)==(value);
  List_Input(Machine(Microcontroller),iszero)==(address);
  List_Input(Machine(Microcontroller),isequal)==(address_1,value);
  List_Input(Machine(Microcontroller),move)==(address_s,address_t);
  List_Input(Machine(Microcontroller),move_w_m)==(address);
  List_Input(Machine(Microcontroller),move_m_w)==(address);
  List_Input(Machine(Microcontroller),reset)==(address);
  List_Input(Machine(Microcontroller),reset_w)==(?);
  List_Input(Machine(Microcontroller),set_data)==(address,value);
  List_Input(Machine(Microcontroller),inc)==(address);
  List_Input(Machine(Microcontroller),dec)==(address);
  List_Input(Machine(Microcontroller),add)==(address1,address2);
  List_Input(Machine(Microcontroller),sub)==(address1,address2);
  List_Input(Machine(Microcontroller),mul)==(address1,address2);
  List_Input(Machine(Microcontroller),div)==(address1,address2)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Microcontroller),init_data)==(?);
  List_Output(Machine(Microcontroller),init)==(?);
  List_Output(Machine(Microcontroller),push)==(?);
  List_Output(Machine(Microcontroller),pop_1)==(?);
  List_Output(Machine(Microcontroller),pop_2)==(?);
  List_Output(Machine(Microcontroller),get_data)==(res);
  List_Output(Machine(Microcontroller),get_pc)==(res);
  List_Output(Machine(Microcontroller),set_end)==(?);
  List_Output(Machine(Microcontroller),get_end)==(res);
  List_Output(Machine(Microcontroller),set_w)==(?);
  List_Output(Machine(Microcontroller),get_w)==(res);
  List_Output(Machine(Microcontroller),nop)==(?);
  List_Output(Machine(Microcontroller),goto)==(?);
  List_Output(Machine(Microcontroller),iszero)==(?);
  List_Output(Machine(Microcontroller),isequal)==(?);
  List_Output(Machine(Microcontroller),move)==(?);
  List_Output(Machine(Microcontroller),move_w_m)==(?);
  List_Output(Machine(Microcontroller),move_m_w)==(?);
  List_Output(Machine(Microcontroller),reset)==(?);
  List_Output(Machine(Microcontroller),reset_w)==(?);
  List_Output(Machine(Microcontroller),set_data)==(?);
  List_Output(Machine(Microcontroller),inc)==(?);
  List_Output(Machine(Microcontroller),dec)==(?);
  List_Output(Machine(Microcontroller),add)==(?);
  List_Output(Machine(Microcontroller),sub)==(?);
  List_Output(Machine(Microcontroller),mul)==(?);
  List_Output(Machine(Microcontroller),div)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Microcontroller),init_data)==(init_data(address,value));
  List_Header(Machine(Microcontroller),init)==(init(pc_,end_));
  List_Header(Machine(Microcontroller),push)==(push);
  List_Header(Machine(Microcontroller),pop_1)==(pop_1);
  List_Header(Machine(Microcontroller),pop_2)==(pop_2);
  List_Header(Machine(Microcontroller),get_data)==(res <-- get_data(address));
  List_Header(Machine(Microcontroller),get_pc)==(res <-- get_pc);
  List_Header(Machine(Microcontroller),set_end)==(set_end(value));
  List_Header(Machine(Microcontroller),get_end)==(res <-- get_end);
  List_Header(Machine(Microcontroller),set_w)==(set_w(value));
  List_Header(Machine(Microcontroller),get_w)==(res <-- get_w);
  List_Header(Machine(Microcontroller),nop)==(nop);
  List_Header(Machine(Microcontroller),goto)==(goto(value));
  List_Header(Machine(Microcontroller),iszero)==(iszero(address));
  List_Header(Machine(Microcontroller),isequal)==(isequal(address_1,value));
  List_Header(Machine(Microcontroller),move)==(move(address_s,address_t));
  List_Header(Machine(Microcontroller),move_w_m)==(move_w_m(address));
  List_Header(Machine(Microcontroller),move_m_w)==(move_m_w(address));
  List_Header(Machine(Microcontroller),reset)==(reset(address));
  List_Header(Machine(Microcontroller),reset_w)==(reset_w);
  List_Header(Machine(Microcontroller),set_data)==(set_data(address,value));
  List_Header(Machine(Microcontroller),inc)==(inc(address));
  List_Header(Machine(Microcontroller),dec)==(dec(address));
  List_Header(Machine(Microcontroller),add)==(add(address1,address2));
  List_Header(Machine(Microcontroller),sub)==(sub(address1,address2));
  List_Header(Machine(Microcontroller),mul)==(mul(address1,address2));
  List_Header(Machine(Microcontroller),div)==(div(address1,address2))
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Microcontroller),init_data)==(address : NATURAL & value : NATURAL & dom(memory_data<+{address|->value}) = NATURAL);
  List_Precondition(Machine(Microcontroller),init)==(pc_ : NATURAL & end_ : NATURAL & pc_<=end_);
  List_Precondition(Machine(Microcontroller),push)==(card(stack)<8 & stack<+{card(stack)+1|->pc} : 0..7 +-> NATURAL & pc+1<=end & card(dom(stack))<8);
  List_Precondition(Machine(Microcontroller),pop_1)==(stack(card(stack))<end);
  List_Precondition(Machine(Microcontroller),pop_2)==(card({card(stack)}<<|stack)<=8 & card(stack)>0);
  List_Precondition(Machine(Microcontroller),get_data)==(address : NATURAL);
  List_Precondition(Machine(Microcontroller),get_pc)==(btrue);
  List_Precondition(Machine(Microcontroller),set_end)==(value : NATURAL & pc<=value);
  List_Precondition(Machine(Microcontroller),get_end)==(btrue);
  List_Precondition(Machine(Microcontroller),set_w)==(value : NATURAL);
  List_Precondition(Machine(Microcontroller),get_w)==(btrue);
  List_Precondition(Machine(Microcontroller),nop)==(pc<end);
  List_Precondition(Machine(Microcontroller),goto)==(value : NATURAL & value<=end);
  List_Precondition(Machine(Microcontroller),iszero)==(address : NATURAL & (memory_data(address) = 0 => pc+1 : NATURAL & pc+1<=end) & (memory_data(address)/=0 => pc+2 : NATURAL & pc+2<=end));
  List_Precondition(Machine(Microcontroller),isequal)==(address_1 : NATURAL & value : NATURAL & pc+2<end);
  List_Precondition(Machine(Microcontroller),move)==(address_s : NATURAL & address_t : NATURAL & pc+1<=end & pc+1 : NATURAL);
  List_Precondition(Machine(Microcontroller),move_w_m)==(address : NATURAL & pc+1<=end & pc+1 : NATURAL);
  List_Precondition(Machine(Microcontroller),move_m_w)==(address : NATURAL & pc+1<=end & pc+1 : NATURAL);
  List_Precondition(Machine(Microcontroller),reset)==(address : NATURAL & memory_data(address) : NATURAL & pc+1<=end & pc+1 : NATURAL);
  List_Precondition(Machine(Microcontroller),reset_w)==(btrue);
  List_Precondition(Machine(Microcontroller),set_data)==(address : NATURAL & value : NATURAL & pc+1<=end);
  List_Precondition(Machine(Microcontroller),inc)==(address : NATURAL & memory_data(address)+1 : NATURAL & pc+1 : NATURAL & pc+1<=end & dom(memory_data<+{address|->memory_data(address)+1}) = NATURAL);
  List_Precondition(Machine(Microcontroller),dec)==(address : NATURAL & memory_data(address)-1 : NATURAL & pc+1 : NATURAL & pc+1<=end & dom(memory_data<+{address|->memory_data(address)-1}) = NATURAL);
  List_Precondition(Machine(Microcontroller),add)==(address1 : NATURAL & address2 : NATURAL & memory_data(address1)+memory_data(address2) : NATURAL);
  List_Precondition(Machine(Microcontroller),sub)==(address1 : NATURAL & address2 : NATURAL & memory_data(address1)-memory_data(address2) : NATURAL);
  List_Precondition(Machine(Microcontroller),mul)==(address1 : NATURAL & address2 : NATURAL & memory_data(address1)*memory_data(address2) : NATURAL);
  List_Precondition(Machine(Microcontroller),div)==(address1 : NATURAL & address2 : NATURAL & memory_data(address1)/memory_data(address2) : NATURAL)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Microcontroller),div)==(address1 : NATURAL & address2 : NATURAL & memory_data(address1)/memory_data(address2) : NATURAL | w:=memory_data(address1)/memory_data(address2));
  Expanded_List_Substitution(Machine(Microcontroller),mul)==(address1 : NATURAL & address2 : NATURAL & memory_data(address1)*memory_data(address2) : NATURAL | w:=memory_data(address1)*memory_data(address2));
  Expanded_List_Substitution(Machine(Microcontroller),sub)==(address1 : NATURAL & address2 : NATURAL & memory_data(address1)-memory_data(address2) : NATURAL | w:=memory_data(address1)-memory_data(address2));
  Expanded_List_Substitution(Machine(Microcontroller),add)==(address1 : NATURAL & address2 : NATURAL & memory_data(address1)+memory_data(address2) : NATURAL | w:=memory_data(address1)+memory_data(address2));
  Expanded_List_Substitution(Machine(Microcontroller),dec)==(address : NATURAL & memory_data(address)-1 : NATURAL & pc+1 : NATURAL & pc+1<=end & dom(memory_data<+{address|->memory_data(address)-1}) = NATURAL | memory_data,pc:=memory_data<+{address|->memory_data(address)-1},pc+1);
  Expanded_List_Substitution(Machine(Microcontroller),inc)==(address : NATURAL & memory_data(address)+1 : NATURAL & pc+1 : NATURAL & pc+1<=end & dom(memory_data<+{address|->memory_data(address)+1}) = NATURAL | memory_data,pc:=memory_data<+{address|->memory_data(address)+1},pc+1);
  Expanded_List_Substitution(Machine(Microcontroller),set_data)==(address : NATURAL & value : NATURAL & pc+1<=end | memory_data,pc:=memory_data<+{address|->value},pc+1);
  Expanded_List_Substitution(Machine(Microcontroller),reset_w)==(btrue | w:=0);
  Expanded_List_Substitution(Machine(Microcontroller),reset)==(address : NATURAL & memory_data(address) : NATURAL & pc+1<=end & pc+1 : NATURAL | memory_data,pc:=memory_data<+{address|->0},pc+1);
  Expanded_List_Substitution(Machine(Microcontroller),move_m_w)==(address : NATURAL & pc+1<=end & pc+1 : NATURAL | w,pc:=memory_data(address),pc+1);
  Expanded_List_Substitution(Machine(Microcontroller),move_w_m)==(address : NATURAL & pc+1<=end & pc+1 : NATURAL | memory_data,pc:=memory_data<+{address|->w},pc+1);
  Expanded_List_Substitution(Machine(Microcontroller),move)==(address_s : NATURAL & address_t : NATURAL & pc+1<=end & pc+1 : NATURAL | memory_data,pc:=memory_data<+{address_t|->memory_data(address_s)},pc+1);
  Expanded_List_Substitution(Machine(Microcontroller),isequal)==(address_1 : NATURAL & value : NATURAL & pc+2<end | memory_data(address_1) = value ==> pc:=pc+1 [] not(memory_data(address_1) = value) ==> pc:=pc+2);
  Expanded_List_Substitution(Machine(Microcontroller),iszero)==(address : NATURAL & (memory_data(address) = 0 => pc+1 : NATURAL & pc+1<=end) & (memory_data(address)/=0 => pc+2 : NATURAL & pc+2<=end) | memory_data(address) = 0 ==> pc:=pc+1 [] not(memory_data(address) = 0) ==> pc:=pc+2);
  Expanded_List_Substitution(Machine(Microcontroller),goto)==(value : NATURAL & value<=end | pc:=value);
  Expanded_List_Substitution(Machine(Microcontroller),nop)==(pc<end | pc:=pc+1);
  Expanded_List_Substitution(Machine(Microcontroller),get_w)==(btrue | res:=w);
  Expanded_List_Substitution(Machine(Microcontroller),set_w)==(value : NATURAL | w:=value);
  Expanded_List_Substitution(Machine(Microcontroller),get_end)==(btrue | res:=end);
  Expanded_List_Substitution(Machine(Microcontroller),set_end)==(value : NATURAL & pc<=value | end:=value);
  Expanded_List_Substitution(Machine(Microcontroller),get_pc)==(btrue | res:=pc);
  Expanded_List_Substitution(Machine(Microcontroller),get_data)==(address : NATURAL | res:=memory_data(address));
  Expanded_List_Substitution(Machine(Microcontroller),pop_2)==(card({card(stack)}<<|stack)<=8 & card(stack)>0 | stack:={card(stack)}<<|stack);
  Expanded_List_Substitution(Machine(Microcontroller),pop_1)==(stack(card(stack))<end | pc:=stack(card(stack)));
  Expanded_List_Substitution(Machine(Microcontroller),push)==(card(stack)<8 & stack<+{card(stack)+1|->pc} : 0..7 +-> NATURAL & pc+1<=end & card(dom(stack))<8 | stack,pc:=stack<+{card(stack)+1|->pc},pc+1);
  Expanded_List_Substitution(Machine(Microcontroller),init)==(pc_ : NATURAL & end_ : NATURAL & pc_<=end_ | pc,end:=pc_,end_);
  Expanded_List_Substitution(Machine(Microcontroller),init_data)==(address : NATURAL & value : NATURAL & dom(memory_data<+{address|->value}) = NATURAL | memory_data:=memory_data<+{address|->value});
  List_Substitution(Machine(Microcontroller),init_data)==(memory_data(address):=value);
  List_Substitution(Machine(Microcontroller),init)==(pc:=pc_ || end:=end_);
  List_Substitution(Machine(Microcontroller),push)==(stack(card(stack)+1):=pc || pc:=pc+1);
  List_Substitution(Machine(Microcontroller),pop_1)==(pc:=stack(card(stack)));
  List_Substitution(Machine(Microcontroller),pop_2)==(stack:={card(stack)}<<|stack);
  List_Substitution(Machine(Microcontroller),get_data)==(res:=memory_data(address));
  List_Substitution(Machine(Microcontroller),get_pc)==(res:=pc);
  List_Substitution(Machine(Microcontroller),set_end)==(end:=value);
  List_Substitution(Machine(Microcontroller),get_end)==(res:=end);
  List_Substitution(Machine(Microcontroller),set_w)==(w:=value);
  List_Substitution(Machine(Microcontroller),get_w)==(res:=w);
  List_Substitution(Machine(Microcontroller),nop)==(pc:=pc+1);
  List_Substitution(Machine(Microcontroller),goto)==(pc:=value);
  List_Substitution(Machine(Microcontroller),iszero)==(IF memory_data(address) = 0 THEN pc:=pc+1 ELSE pc:=pc+2 END);
  List_Substitution(Machine(Microcontroller),isequal)==(IF memory_data(address_1) = value THEN pc:=pc+1 ELSE pc:=pc+2 END);
  List_Substitution(Machine(Microcontroller),move)==(memory_data(address_t):=memory_data(address_s) || pc:=pc+1);
  List_Substitution(Machine(Microcontroller),move_w_m)==(memory_data(address):=w || pc:=pc+1);
  List_Substitution(Machine(Microcontroller),move_m_w)==(w:=memory_data(address) || pc:=pc+1);
  List_Substitution(Machine(Microcontroller),reset)==(memory_data(address):=0 || pc:=pc+1);
  List_Substitution(Machine(Microcontroller),reset_w)==(w:=0);
  List_Substitution(Machine(Microcontroller),set_data)==(memory_data(address):=value || pc:=pc+1);
  List_Substitution(Machine(Microcontroller),inc)==(memory_data(address):=memory_data(address)+1 || pc:=pc+1);
  List_Substitution(Machine(Microcontroller),dec)==(memory_data(address):=memory_data(address)-1 || pc:=pc+1);
  List_Substitution(Machine(Microcontroller),add)==(w:=memory_data(address1)+memory_data(address2));
  List_Substitution(Machine(Microcontroller),sub)==(w:=memory_data(address1)-memory_data(address2));
  List_Substitution(Machine(Microcontroller),mul)==(w:=memory_data(address1)*memory_data(address2));
  List_Substitution(Machine(Microcontroller),div)==(w:=memory_data(address1)/memory_data(address2))
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
  List_Of_Ids(Machine(Microcontroller)) == (? | ? | end,pc,w,stack,memory_data | ? | init_data,init,push,pop_1,pop_2,get_data,get_pc,set_end,get_end,set_w,get_w,nop,goto,iszero,isequal,move,move_w_m,move_m_w,reset,reset_w,set_data,inc,dec,add,sub,mul,div | ? | ? | ? | Microcontroller);
  List_Of_HiddenCst_Ids(Machine(Microcontroller)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Microcontroller)) == (?);
  List_Of_VisibleVar_Ids(Machine(Microcontroller)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Microcontroller)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Microcontroller)) == (Type(end) == Mvl(btype(INTEGER,?,?));Type(pc) == Mvl(btype(INTEGER,?,?));Type(w) == Mvl(btype(INTEGER,?,?));Type(stack) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(memory_data) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Microcontroller)) == (Type(div) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(mul) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(sub) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(add) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(dec) == Cst(No_type,btype(INTEGER,?,?));Type(inc) == Cst(No_type,btype(INTEGER,?,?));Type(set_data) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(reset_w) == Cst(No_type,No_type);Type(reset) == Cst(No_type,btype(INTEGER,?,?));Type(move_m_w) == Cst(No_type,btype(INTEGER,?,?));Type(move_w_m) == Cst(No_type,btype(INTEGER,?,?));Type(move) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(isequal) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(iszero) == Cst(No_type,btype(INTEGER,?,?));Type(goto) == Cst(No_type,btype(INTEGER,?,?));Type(nop) == Cst(No_type,No_type);Type(get_w) == Cst(btype(INTEGER,?,?),No_type);Type(set_w) == Cst(No_type,btype(INTEGER,?,?));Type(get_end) == Cst(btype(INTEGER,?,?),No_type);Type(set_end) == Cst(No_type,btype(INTEGER,?,?));Type(get_pc) == Cst(btype(INTEGER,?,?),No_type);Type(get_data) == Cst(btype(INTEGER,?,?),btype(INTEGER,?,?));Type(pop_2) == Cst(No_type,No_type);Type(pop_1) == Cst(No_type,No_type);Type(push) == Cst(No_type,No_type);Type(init) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(init_data) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  Observers(Machine(Microcontroller)) == (Type(get_w) == Cst(btype(INTEGER,?,?),No_type);Type(get_end) == Cst(btype(INTEGER,?,?),No_type);Type(get_pc) == Cst(btype(INTEGER,?,?),No_type);Type(get_data) == Cst(btype(INTEGER,?,?),btype(INTEGER,?,?)))
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
