Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(ram))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(ram))==(Machine(ram));
  Level(Machine(ram))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(ram)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(ram))==(types)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(ram))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(ram))==(?);
  List_Includes(Machine(ram))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(ram))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(ram))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(ram))==(?);
  Context_List_Variables(Machine(ram))==(?);
  Abstract_List_Variables(Machine(ram))==(?);
  Local_List_Variables(Machine(ram))==(end,pc,mem);
  List_Variables(Machine(ram))==(end,pc,mem);
  External_List_Variables(Machine(ram))==(end,pc,mem)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(ram))==(?);
  Abstract_List_VisibleVariables(Machine(ram))==(?);
  External_List_VisibleVariables(Machine(ram))==(?);
  Expanded_List_VisibleVariables(Machine(ram))==(?);
  List_VisibleVariables(Machine(ram))==(?);
  Internal_List_VisibleVariables(Machine(ram))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(ram))==(btrue);
  Gluing_List_Invariant(Machine(ram))==(btrue);
  Expanded_List_Invariant(Machine(ram))==(btrue);
  Abstract_List_Invariant(Machine(ram))==(btrue);
  Context_List_Invariant(Machine(ram))==(btrue);
  List_Invariant(Machine(ram))==(mem : NATURAL --> uint32 & pc : NATURAL & end : NATURAL & pc<=end)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(ram))==(btrue);
  Abstract_List_Assertions(Machine(ram))==(btrue);
  Context_List_Assertions(Machine(ram))==(btrue);
  List_Assertions(Machine(ram))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(ram))==(@(mem$0).(mem$0 : NATURAL --> uint32 ==> mem:=mem$0) || pc:=0 || @(end$0).(end$0 : NATURAL ==> end:=end$0));
  Context_List_Initialisation(Machine(ram))==(skip);
  List_Initialisation(Machine(ram))==(mem:: NATURAL --> uint32 || pc:=0 || end:: NATURAL)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(ram))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(ram),Machine(types))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(ram))==(btrue);
  List_Constraints(Machine(ram))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(ram))==(init,nop,set,inc,move,testgt,testeq,goto,get_data,get_pc);
  List_Operations(Machine(ram))==(init,nop,set,inc,move,testgt,testeq,goto,get_data,get_pc)
END
&
THEORY ListInputX IS
  List_Input(Machine(ram),init)==(sz);
  List_Input(Machine(ram),nop)==(?);
  List_Input(Machine(ram),set)==(addr,val);
  List_Input(Machine(ram),inc)==(addr);
  List_Input(Machine(ram),move)==(src,dest);
  List_Input(Machine(ram),testgt)==(addr1,addr2);
  List_Input(Machine(ram),testeq)==(addr1,addr2);
  List_Input(Machine(ram),goto)==(dest);
  List_Input(Machine(ram),get_data)==(addr);
  List_Input(Machine(ram),get_pc)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(ram),init)==(?);
  List_Output(Machine(ram),nop)==(?);
  List_Output(Machine(ram),set)==(?);
  List_Output(Machine(ram),inc)==(?);
  List_Output(Machine(ram),move)==(?);
  List_Output(Machine(ram),testgt)==(?);
  List_Output(Machine(ram),testeq)==(?);
  List_Output(Machine(ram),goto)==(?);
  List_Output(Machine(ram),get_data)==(res);
  List_Output(Machine(ram),get_pc)==(res)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(ram),init)==(init(sz));
  List_Header(Machine(ram),nop)==(nop);
  List_Header(Machine(ram),set)==(set(addr,val));
  List_Header(Machine(ram),inc)==(inc(addr));
  List_Header(Machine(ram),move)==(move(src,dest));
  List_Header(Machine(ram),testgt)==(testgt(addr1,addr2));
  List_Header(Machine(ram),testeq)==(testeq(addr1,addr2));
  List_Header(Machine(ram),goto)==(goto(dest));
  List_Header(Machine(ram),get_data)==(res <-- get_data(addr));
  List_Header(Machine(ram),get_pc)==(res <-- get_pc)
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(ram),init)==(sz : NATURAL);
  List_Precondition(Machine(ram),nop)==(pc+1<=end);
  List_Precondition(Machine(ram),set)==(addr : NATURAL & val : uint32 & pc+1<=end);
  List_Precondition(Machine(ram),inc)==(addr : NATURAL & pc+1<=end & mem(addr)<MAXINT);
  List_Precondition(Machine(ram),move)==(src : NATURAL & dest : NATURAL & pc+1<=end);
  List_Precondition(Machine(ram),testgt)==(addr1 : NATURAL & addr2 : NATURAL & (mem(addr1)>mem(addr2) => pc+1<=end) & (mem(addr1)<=mem(addr2) => pc+2<=end));
  List_Precondition(Machine(ram),testeq)==(addr1 : NATURAL & addr2 : NATURAL & (mem(addr1) = mem(addr2) => pc+1<=end) & (mem(addr1)/=mem(addr2) => pc+2<=end));
  List_Precondition(Machine(ram),goto)==(dest : NATURAL & dest<=end);
  List_Precondition(Machine(ram),get_data)==(addr : NATURAL);
  List_Precondition(Machine(ram),get_pc)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(ram),get_pc)==(btrue | res:=pc);
  Expanded_List_Substitution(Machine(ram),get_data)==(addr : NATURAL | res:=mem(addr));
  Expanded_List_Substitution(Machine(ram),goto)==(dest : NATURAL & dest<=end | pc:=dest);
  Expanded_List_Substitution(Machine(ram),testeq)==(addr1 : NATURAL & addr2 : NATURAL & (mem(addr1) = mem(addr2) => pc+1<=end) & (mem(addr1)/=mem(addr2) => pc+2<=end) | mem(addr1) = mem(addr2) ==> pc:=pc+1 [] not(mem(addr1) = mem(addr2)) ==> pc:=pc+2);
  Expanded_List_Substitution(Machine(ram),testgt)==(addr1 : NATURAL & addr2 : NATURAL & (mem(addr1)>mem(addr2) => pc+1<=end) & (mem(addr1)<=mem(addr2) => pc+2<=end) | mem(addr1)>mem(addr2) ==> pc:=pc+1 [] not(mem(addr1)>mem(addr2)) ==> pc:=pc+2);
  Expanded_List_Substitution(Machine(ram),move)==(src : NATURAL & dest : NATURAL & pc+1<=end | mem,pc:=mem<+{dest|->mem(src)},pc+1);
  Expanded_List_Substitution(Machine(ram),inc)==(addr : NATURAL & pc+1<=end & mem(addr)<MAXINT | mem:=mem<+{addr|->mem(addr)+1});
  Expanded_List_Substitution(Machine(ram),set)==(addr : NATURAL & val : uint32 & pc+1<=end | mem,pc:=mem<+{addr|->val},pc+1);
  Expanded_List_Substitution(Machine(ram),nop)==(pc+1<=end | pc:=pc+1);
  Expanded_List_Substitution(Machine(ram),init)==(sz : NATURAL | pc,end:=0,sz);
  List_Substitution(Machine(ram),init)==(pc:=0 || end:=sz);
  List_Substitution(Machine(ram),nop)==(pc:=pc+1);
  List_Substitution(Machine(ram),set)==(mem(addr):=val || pc:=pc+1);
  List_Substitution(Machine(ram),inc)==(mem(addr):=mem(addr)+1);
  List_Substitution(Machine(ram),move)==(mem(dest):=mem(src) || pc:=pc+1);
  List_Substitution(Machine(ram),testgt)==(IF mem(addr1)>mem(addr2) THEN pc:=pc+1 ELSE pc:=pc+2 END);
  List_Substitution(Machine(ram),testeq)==(IF mem(addr1) = mem(addr2) THEN pc:=pc+1 ELSE pc:=pc+2 END);
  List_Substitution(Machine(ram),goto)==(pc:=dest);
  List_Substitution(Machine(ram),get_data)==(res:=mem(addr));
  List_Substitution(Machine(ram),get_pc)==(res:=pc)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(ram))==(?);
  Inherited_List_Constants(Machine(ram))==(?);
  List_Constants(Machine(ram))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(ram))==(?);
  Context_List_Defered(Machine(ram))==(?);
  Context_List_Sets(Machine(ram))==(?);
  List_Valuable_Sets(Machine(ram))==(?);
  Inherited_List_Enumerated(Machine(ram))==(?);
  Inherited_List_Defered(Machine(ram))==(?);
  Inherited_List_Sets(Machine(ram))==(?);
  List_Enumerated(Machine(ram))==(?);
  List_Defered(Machine(ram))==(?);
  List_Sets(Machine(ram))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(ram))==(?);
  Expanded_List_HiddenConstants(Machine(ram))==(?);
  List_HiddenConstants(Machine(ram))==(?);
  External_List_HiddenConstants(Machine(ram))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(ram))==(btrue);
  Context_List_Properties(Machine(ram))==(uint32 : POW(INTEGER) & uint32 = 0..MAXINT & uint16 : POW(INTEGER) & uint16 = 0..65535);
  Inherited_List_Properties(Machine(ram))==(btrue);
  List_Properties(Machine(ram))==(btrue)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(ram),Machine(types))==(?);
  Seen_Context_List_Enumerated(Machine(ram))==(?);
  Seen_Context_List_Invariant(Machine(ram))==(btrue);
  Seen_Context_List_Assertions(Machine(ram))==(btrue);
  Seen_Context_List_Properties(Machine(ram))==(btrue);
  Seen_List_Constraints(Machine(ram))==(btrue);
  Seen_List_Operations(Machine(ram),Machine(types))==(?);
  Seen_Expanded_List_Invariant(Machine(ram),Machine(types))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(ram)) == (? | ? | end,pc,mem | ? | init,nop,set,inc,move,testgt,testeq,goto,get_data,get_pc | ? | seen(Machine(types)) | ? | ram);
  List_Of_HiddenCst_Ids(Machine(ram)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ram)) == (?);
  List_Of_VisibleVar_Ids(Machine(ram)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ram)) == (? : ?);
  List_Of_Ids(Machine(types)) == (uint32,uint16 | ? | ? | ? | ? | ? | ? | ? | types);
  List_Of_HiddenCst_Ids(Machine(types)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(types)) == (uint32,uint16);
  List_Of_VisibleVar_Ids(Machine(types)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(types)) == (? : ?)
END
&
THEORY VariablesEnvX IS
  Variables(Machine(ram)) == (Type(end) == Mvl(btype(INTEGER,?,?));Type(pc) == Mvl(btype(INTEGER,?,?));Type(mem) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,"[uint32","]uint32"))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(ram)) == (Type(get_pc) == Cst(btype(INTEGER,?,?),No_type);Type(get_data) == Cst(btype(INTEGER,?,?),btype(INTEGER,?,?));Type(goto) == Cst(No_type,btype(INTEGER,?,?));Type(testeq) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(testgt) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(move) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(inc) == Cst(No_type,btype(INTEGER,?,?));Type(set) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(nop) == Cst(No_type,No_type);Type(init) == Cst(No_type,btype(INTEGER,?,?)));
  Observers(Machine(ram)) == (Type(get_pc) == Cst(btype(INTEGER,?,?),No_type);Type(get_data) == Cst(btype(INTEGER,?,?),btype(INTEGER,?,?)))
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
