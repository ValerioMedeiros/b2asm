Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(A8051))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(A8051))==(Machine(A8051));
  Level(Machine(A8051))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(A8051)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(A8051))==(TYPES,ALU)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(A8051))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(A8051))==(?);
  List_Includes(Machine(A8051))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(A8051))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(A8051))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(A8051))==(?);
  Context_List_Variables(Machine(A8051))==(?);
  Abstract_List_Variables(Machine(A8051))==(?);
  Local_List_Variables(Machine(A8051))==(?);
  List_Variables(Machine(A8051))==(?);
  External_List_Variables(Machine(A8051))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(A8051))==(?);
  Abstract_List_VisibleVariables(Machine(A8051))==(?);
  External_List_VisibleVariables(Machine(A8051))==(?);
  Expanded_List_VisibleVariables(Machine(A8051))==(?);
  List_VisibleVariables(Machine(A8051))==(sp,pc,mem,stack);
  Internal_List_VisibleVariables(Machine(A8051))==(sp,pc,mem,stack)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(A8051))==(btrue);
  Gluing_List_Invariant(Machine(A8051))==(btrue);
  Expanded_List_Invariant(Machine(A8051))==(btrue);
  Abstract_List_Invariant(Machine(A8051))==(btrue);
  Context_List_Invariant(Machine(A8051))==(btrue);
  List_Invariant(Machine(A8051))==(mem : MEM_ADDR -->> WORD & pc : INSTRUCTION & sp : NATURAL & stack : NATURAL +->> INSTRUCTION & dom(stack) = 0..sp-1 & bitget(P) = par(mem(a)))
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(A8051))==(btrue);
  Abstract_List_Assertions(Machine(A8051))==(btrue);
  Context_List_Assertions(Machine(A8051))==(NB_WORD = 256 & !n.(n : WORD => 0<=n & n<=255) & INSTRUCTION_IJUMP(5,1) = 7 & INSTRUCTION_IJUMP(0,2) = 3 & INSTRUCTION_IJUMP(2,4) = 7 & INSTRUCTION_IJUMP(3,2) = 6 & INSTRUCTION_NEXT(0) = 1 & INSTRUCTION_NEXT(1) = 2 & INSTRUCTION_NEXT(2) = 3 & INSTRUCTION_NEXT(3) = 4 & INSTRUCTION_NEXT(4) = 5 & INSTRUCTION_NEXT(5) = 6 & INSTRUCTION_NEXT(6) = 7 & INSTRUCTION_IJUMP(3,3) = 7 & MEM_ADDR <: WORD & RAM_ADDR <: WORD & SFR_ADDR <: WORD & MEM_ADDR = RAM_ADDR\/SFR_ADDR & BIT_FLIP(0) = 1 & BIT_FLIP(1) = 0 & BIT_AND(0,0) = 0 & BIT_AND(0,1) = 0 & BIT_AND(1,0) = 0 & BIT_AND(1,1) = 1 & BIT_IOR(0,0) = 0 & BIT_IOR(0,1) = 0 & BIT_IOR(1,0) = 0 & BIT_IOR(1,1) = 1 & BIT_XOR(0,0) = 0 & BIT_XOR(0,1) = 1 & BIT_XOR(1,0) = 1 & BIT_XOR(1,1) = 0 & dom(add) = WORD*WORD & ran(add) <: WORD*BOOL*BOOL & dom(substract) = WORD*WORD & ran(substract) <: WORD*BOOL*BOOL & dom(and) = WORD*WORD & ran(and) <: WORD*BOOL & dom(ior) = WORD*WORD & ran(ior) <: WORD*BOOL & dom(xor) = WORD*WORD & ran(xor) <: WORD*BOOL & dom(bitclear) = WORD*WORD_POSITION & ran(bitclear) <: WORD & dom(bitset) = WORD*WORD_POSITION & ran(bitset) <: WORD & dom(bitget) = WORD*WORD_POSITION & ran(bitget) <: BIT & dom(complement) = WORD & ran(complement) <: WORD & dom(swap) = WORD & ran(swap) <: WORD & ran(rotateleft) <: WORD*BOOL & dom(rotateleft) = WORD & dom(rotateright) = WORD & ran(rotateright) <: WORD*BOOL);
  List_Assertions(Machine(A8051))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(A8051))==(sp,stack:=0,{} || @(mem$0).(mem$0 : MEM_ADDR -->> WORD ==> mem:=mem$0) || pc:=0);
  Context_List_Initialisation(Machine(A8051))==(skip);
  List_Initialisation(Machine(A8051))==(sp:=0 || stack:={} || mem:: MEM_ADDR -->> WORD || pc:=0)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(A8051))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(A8051),Machine(TYPES))==(?);
  List_Instanciated_Parameters(Machine(A8051),Machine(ALU))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(A8051))==(btrue);
  List_Constraints(Machine(A8051))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(A8051))==(CJNE,CJNEI,MOV,MOVI,ANL,ACALL,SJMP,JC,JNB,CLR,AJMP,ADDS,ADDD,SUBBS,SUBBD,INC,DEC,INCS);
  List_Operations(Machine(A8051))==(CJNE,CJNEI,MOV,MOVI,ANL,ACALL,SJMP,JC,JNB,CLR,AJMP,ADDS,ADDD,SUBBS,SUBBD,INC,DEC,INCS)
END
&
THEORY ListInputX IS
  List_Input(Machine(A8051),CJNE)==(addr,data,jump);
  List_Input(Machine(A8051),CJNEI)==(Rn,data,jump);
  List_Input(Machine(A8051),MOV)==(addr,data);
  List_Input(Machine(A8051),MOVI)==(Rn,data);
  List_Input(Machine(A8051),ANL)==(addr);
  List_Input(Machine(A8051),ACALL)==(inst);
  List_Input(Machine(A8051),SJMP)==(jump);
  List_Input(Machine(A8051),JC)==(jump);
  List_Input(Machine(A8051),JNB)==(bit,jump);
  List_Input(Machine(A8051),CLR)==(b);
  List_Input(Machine(A8051),AJMP)==(jump);
  List_Input(Machine(A8051),ADDS)==(acc,src);
  List_Input(Machine(A8051),ADDD)==(acc,data);
  List_Input(Machine(A8051),SUBBS)==(acc,src);
  List_Input(Machine(A8051),SUBBD)==(acc,data);
  List_Input(Machine(A8051),INC)==(?);
  List_Input(Machine(A8051),DEC)==(?);
  List_Input(Machine(A8051),INCS)==(src)
END
&
THEORY ListOutputX IS
  List_Output(Machine(A8051),CJNE)==(?);
  List_Output(Machine(A8051),CJNEI)==(?);
  List_Output(Machine(A8051),MOV)==(?);
  List_Output(Machine(A8051),MOVI)==(?);
  List_Output(Machine(A8051),ANL)==(?);
  List_Output(Machine(A8051),ACALL)==(?);
  List_Output(Machine(A8051),SJMP)==(?);
  List_Output(Machine(A8051),JC)==(?);
  List_Output(Machine(A8051),JNB)==(?);
  List_Output(Machine(A8051),CLR)==(?);
  List_Output(Machine(A8051),AJMP)==(?);
  List_Output(Machine(A8051),ADDS)==(?);
  List_Output(Machine(A8051),ADDD)==(?);
  List_Output(Machine(A8051),SUBBS)==(?);
  List_Output(Machine(A8051),SUBBD)==(?);
  List_Output(Machine(A8051),INC)==(?);
  List_Output(Machine(A8051),DEC)==(?);
  List_Output(Machine(A8051),INCS)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(A8051),CJNE)==(CJNE(addr,data,jump));
  List_Header(Machine(A8051),CJNEI)==(CJNEI(Rn,data,jump));
  List_Header(Machine(A8051),MOV)==(MOV(addr,data));
  List_Header(Machine(A8051),MOVI)==(MOVI(Rn,data));
  List_Header(Machine(A8051),ANL)==(ANL(addr));
  List_Header(Machine(A8051),ACALL)==(ACALL(inst));
  List_Header(Machine(A8051),SJMP)==(SJMP(jump));
  List_Header(Machine(A8051),JC)==(JC(jump));
  List_Header(Machine(A8051),JNB)==(JNB(bit,jump));
  List_Header(Machine(A8051),CLR)==(CLR(b));
  List_Header(Machine(A8051),AJMP)==(AJMP(jump));
  List_Header(Machine(A8051),ADDS)==(ADDS(acc,src));
  List_Header(Machine(A8051),ADDD)==(ADDD(acc,data));
  List_Header(Machine(A8051),SUBBS)==(SUBBS(acc,src));
  List_Header(Machine(A8051),SUBBD)==(SUBBD(acc,data));
  List_Header(Machine(A8051),INC)==(INC);
  List_Header(Machine(A8051),DEC)==(DEC);
  List_Header(Machine(A8051),INCS)==(INCS(src))
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(A8051),CJNE)==(addr : MEM_ADDR & data : WORD & jump : INSTRUCTION);
  List_Precondition(Machine(A8051),CJNEI)==(Rn : RAM_ADDR & (Rn = R1 or Rn = R2) & data : WORD & jump : INSTRUCTION & mem(Rn) : RAM_ADDR);
  List_Precondition(Machine(A8051),MOV)==(addr : MEM_ADDR & data : WORD);
  List_Precondition(Machine(A8051),MOVI)==(Rn : RAM_ADDR & (Rn = R1 or Rn = R2) & data : WORD & mem(Rn) : RAM_ADDR);
  List_Precondition(Machine(A8051),ANL)==(addr : RAM_ADDR);
  List_Precondition(Machine(A8051),ACALL)==(inst : INSTRUCTION);
  List_Precondition(Machine(A8051),SJMP)==(jump : INSTRUCTION);
  List_Precondition(Machine(A8051),JC)==(jump : INSTRUCTION);
  List_Precondition(Machine(A8051),JNB)==(bit : BIT & jump : INSTRUCTION);
  List_Precondition(Machine(A8051),CLR)==(b : BIT_ADDRESS);
  List_Precondition(Machine(A8051),AJMP)==(jump : INSTRUCTION);
  List_Precondition(Machine(A8051),ADDS)==(acc : SFR_ADDR & acc = a & src : MEM_ADDR);
  List_Precondition(Machine(A8051),ADDD)==(acc : SFR_ADDR & acc = a & data : WORD);
  List_Precondition(Machine(A8051),SUBBS)==(acc : SFR_ADDR & acc = a & src : MEM_ADDR);
  List_Precondition(Machine(A8051),SUBBD)==(acc : SFR_ADDR & acc = a & data : WORD);
  List_Precondition(Machine(A8051),INC)==(btrue);
  List_Precondition(Machine(A8051),DEC)==(btrue);
  List_Precondition(Machine(A8051),INCS)==(src : MEM_ADDR)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(A8051),INCS)==(src : MEM_ADDR | @(result,carry,zero).(result : WORD & result,carry,zero = add(mem(src),1) & carry : BOOL & zero : BOOL ==> mem:=mem<+{src|->result}) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(A8051),DEC)==(btrue | @(result,carry,zero).(result : WORD & result,carry,zero = substract(a,1) & carry : BOOL & zero : BOOL ==> mem:=mem<+{a|->result}) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(A8051),INC)==(btrue | @(result,carry,zero).(result : WORD & result,carry,zero = add(mem(a),1) & carry : BOOL & zero : BOOL ==> mem:=mem<+{a|->result}) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(A8051),SUBBD)==(acc : SFR_ADDR & acc = a & data : WORD | @(result,carry,zero).(result : WORD & result,carry,zero = substract(mem(a),data) & carry : BOOL & zero : BOOL ==> mem:=mem<+{a|->result}) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(A8051),SUBBS)==(acc : SFR_ADDR & acc = a & src : MEM_ADDR | @(result,carry,zero).(result : WORD & result,carry,zero = substract(mem(a),mem(src)) & carry : BOOL & zero : BOOL ==> mem:=mem<+{a|->result}) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(A8051),ADDD)==(acc : SFR_ADDR & acc = a & data : WORD | @(result,carry,zero).(result : WORD & result,carry,zero = add(mem(a),data) & carry : BOOL & zero : BOOL ==> mem:=mem<+{a|->result}) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(A8051),ADDS)==(acc : SFR_ADDR & acc = a & src : MEM_ADDR | @(result,carry,zero).(result : WORD & result,carry,zero = add(mem(a),mem(src)) & carry : BOOL & zero : BOOL ==> mem:=mem<+{a|->result}) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(A8051),AJMP)==(jump : INSTRUCTION | pc:=jump);
  Expanded_List_Substitution(Machine(A8051),CLR)==(b : BIT_ADDRESS | @(w,p).(w : WORD & p : WORD_POSITION & w,p = b ==> mem:=mem<+{w|->bitset(mem(w),p)}) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(A8051),JNB)==(bit : BIT & jump : INSTRUCTION | bit = 0 ==> pc:=INSTRUCTION_IJUMP(pc,jump) [] not(bit = 0) ==> pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(A8051),JC)==(jump : INSTRUCTION | bitget(mem(psw),7) = 1 ==> pc:=INSTRUCTION_IJUMP(pc,jump) [] not(bitget(mem(psw),7) = 1) ==> pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(A8051),SJMP)==(jump : INSTRUCTION | pc:=INSTRUCTION_IJUMP(pc,jump));
  Expanded_List_Substitution(Machine(A8051),ACALL)==(inst : INSTRUCTION | sp,stack,pc:=sp+1,stack<+{sp|->INSTRUCTION_NEXT(pc)},inst);
  Expanded_List_Substitution(Machine(A8051),ANL)==(addr : RAM_ADDR | @(result,b).(result : WORD & b : BOOL & result,b = and(a,addr) ==> mem:=mem<+{a|->result}) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(A8051),MOVI)==(Rn : RAM_ADDR & (Rn = R1 or Rn = R2) & data : WORD & mem(Rn) : RAM_ADDR | @addr.(addr : RAM_ADDR & addr = mem(Rn) ==> mem,pc:=mem<+{addr|->data},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(A8051),MOV)==(addr : MEM_ADDR & data : WORD | mem,pc:=mem<+{addr|->data},INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(A8051),CJNEI)==(Rn : RAM_ADDR & (Rn = R1 or Rn = R2) & data : WORD & jump : INSTRUCTION & mem(Rn) : RAM_ADDR | @addr.(addr : RAM_ADDR & addr = mem(Rn) ==> (not(mem(addr) = data) ==> (mem(addr)<data ==> mem:=mem<+{psw|->bitset(mem(psw),7)} [] not(mem(addr)<data) ==> mem:=mem<+{psw|->bitclear(mem(psw),7)} || pc:=INSTRUCTION_IJUMP(pc,jump)) [] not(not(mem(addr) = data)) ==> pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(A8051),CJNE)==(addr : MEM_ADDR & data : WORD & jump : INSTRUCTION | not(mem(addr) = data) ==> (mem(addr)<data ==> mem:=mem<+{psw|->bitset(mem(psw),7)} [] not(mem(addr)<data) ==> mem:=mem<+{psw|->bitclear(mem(psw),7)} || pc:=INSTRUCTION_IJUMP(pc,jump)) [] not(not(mem(addr) = data)) ==> pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(A8051),CJNE)==(IF not(mem(addr) = data) THEN BEGIN IF mem(addr)<data THEN mem(psw):=bitset(mem(psw),7) ELSE mem(psw):=bitclear(mem(psw),7) END END || pc:=INSTRUCTION_IJUMP(pc,jump) ELSE pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(A8051),CJNEI)==(ANY addr WHERE addr : RAM_ADDR & addr = mem(Rn) THEN IF not(mem(addr) = data) THEN BEGIN IF mem(addr)<data THEN mem(psw):=bitset(mem(psw),7) ELSE mem(psw):=bitclear(mem(psw),7) END END || pc:=INSTRUCTION_IJUMP(pc,jump) ELSE pc:=INSTRUCTION_NEXT(pc) END END);
  List_Substitution(Machine(A8051),MOV)==(mem(addr):=data || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(A8051),MOVI)==(ANY addr WHERE addr : RAM_ADDR & addr = mem(Rn) THEN mem(addr):=data || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(A8051),ANL)==(ANY result,b WHERE result : WORD & b : BOOL & result,b = and(a,addr) THEN mem(a):=result END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(A8051),ACALL)==(sp:=sp+1 || stack(sp):=INSTRUCTION_NEXT(pc) || pc:=inst);
  List_Substitution(Machine(A8051),SJMP)==(pc:=INSTRUCTION_IJUMP(pc,jump));
  List_Substitution(Machine(A8051),JC)==(IF bitget(mem(psw),7) = 1 THEN pc:=INSTRUCTION_IJUMP(pc,jump) ELSE pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(A8051),JNB)==(IF bit = 0 THEN pc:=INSTRUCTION_IJUMP(pc,jump) ELSE pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(A8051),CLR)==(ANY w,p WHERE w : WORD & p : WORD_POSITION & w,p = b THEN mem(w):=bitset(mem(w),p) END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(A8051),AJMP)==(pc:=jump);
  List_Substitution(Machine(A8051),ADDS)==(ANY result,carry,zero WHERE result : WORD & result,carry,zero = add(mem(a),mem(src)) & carry : BOOL & zero : BOOL THEN mem(a):=result END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(A8051),ADDD)==(ANY result,carry,zero WHERE result : WORD & result,carry,zero = add(mem(a),data) & carry : BOOL & zero : BOOL THEN mem(a):=result END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(A8051),SUBBS)==(ANY result,carry,zero WHERE result : WORD & result,carry,zero = substract(mem(a),mem(src)) & carry : BOOL & zero : BOOL THEN mem(a):=result END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(A8051),SUBBD)==(ANY result,carry,zero WHERE result : WORD & result,carry,zero = substract(mem(a),data) & carry : BOOL & zero : BOOL THEN mem(a):=result END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(A8051),INC)==(ANY result,carry,zero WHERE result : WORD & result,carry,zero = add(mem(a),1) & carry : BOOL & zero : BOOL THEN mem(a):=result END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(A8051),DEC)==(ANY result,carry,zero WHERE result : WORD & result,carry,zero = substract(a,1) & carry : BOOL & zero : BOOL THEN mem(a):=result END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(A8051),INCS)==(ANY result,carry,zero WHERE result : WORD & result,carry,zero = add(mem(src),1) & carry : BOOL & zero : BOOL THEN mem(src):=result END || pc:=INSTRUCTION_NEXT(pc))
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(A8051))==(sbuf,R0,R1,R2,R3,R4,R5,R6,R7,a,P0,P1,P2,P3,scon,psw,P);
  Inherited_List_Constants(Machine(A8051))==(?);
  List_Constants(Machine(A8051))==(sbuf,R0,R1,R2,R3,R4,R5,R6,R7,a,P0,P1,P2,P3,scon,psw,P)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(A8051))==(?);
  Context_List_Defered(Machine(A8051))==(?);
  Context_List_Sets(Machine(A8051))==(?);
  List_Valuable_Sets(Machine(A8051))==(?);
  Inherited_List_Enumerated(Machine(A8051))==(?);
  Inherited_List_Defered(Machine(A8051))==(?);
  Inherited_List_Sets(Machine(A8051))==(?);
  List_Enumerated(Machine(A8051))==(?);
  List_Defered(Machine(A8051))==(?);
  List_Sets(Machine(A8051))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(A8051))==(?);
  Expanded_List_HiddenConstants(Machine(A8051))==(?);
  List_HiddenConstants(Machine(A8051))==(?);
  External_List_HiddenConstants(Machine(A8051))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(A8051))==(btrue);
  Context_List_Properties(Machine(A8051))==(MAX_ADDR : NATURAL & RAM_ADDR = 0..127 & SFR_ADDR = 128..MAX_ADDR & MAX_ADDR = 255 & MEM_ADDR = 0..MAX_ADDR & NB_WORD : NATURAL & WORD_LEN : NATURAL & WORD_LEN = 8 & WORD_POSITION = 0..WORD_LEN-1 & NB_WORD = 2**WORD_LEN & WORD = 0..NB_WORD-1 & NB_INSTRUCTIONS = 256 & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = 0..INSTRUCTION_MAX & INSTRUCTION_IJUMP : INSTRUCTION*INSTRUCTION --> INSTRUCTION & !(i,j,sum).(i : INSTRUCTION & j : INSTRUCTION & sum : NATURAL & sum = i+j => (sum>=255 => INSTRUCTION_IJUMP(i,j) = 0) & (sum<255 => INSTRUCTION_IJUMP(i,j) = sum+1)) & INSTRUCTION_NEXT : INSTRUCTION -->> INSTRUCTION & !i.(i : INSTRUCTION => (i = 255 => INSTRUCTION_NEXT(i) = 0) & (i<255 => INSTRUCTION_NEXT(i) = i+1)) & WORD_TO_INT : WORD -->> INT & !(i,j).(i : WORD & j : NATURAL & i>0 & i<=255 & j = i+0 & 0<j & j<=255 => WORD_TO_INT(i) = j) & BV_TO_WORD : BV8 --> WORD & WORD_TO_BV : WORD --> BV8 & !(w,v).(w : WORD & v : BV8 => v = WORD_TO_BV(w) <=> (w = 128*v(7)+64*v(6)+32*v(5)+16*v(4)+8*v(3)+4*v(2)+2*v(1)+v(0))) & BV_TO_WORD = WORD_TO_BV~ & BIT_ADDRESS = WORD*WORD_POSITION & BV8_INDEX = 0..7 & BV8 = BV8_INDEX --> BIT & BV8_SET_BIT : BV8*BV8_INDEX*BIT --> BV8 & !(v,i,j,b).(v : BV8 & i : BV8_INDEX & j : BV8_INDEX & b : BIT => (i/=j => BV8_SET_BIT(v,i,b)(j) = v(j))) & !(v,i,b).(v : BV8 & i : BV8_INDEX & b : BIT => BV8_SET_BIT(v,i,b)(i) = b) & BV8_COMPLEMENT : BV8 --> BV8 & !(v,i).(v : BV8 & i : BV8_INDEX => BV8_COMPLEMENT(v)(i) = BIT_FLIP(v(i))) & BV8_ALL_ZEROES : BV8 & !i.(i : BV8_INDEX => BV8_ALL_ZEROES(i) = 0) & BV8_AND : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_AND(v1,v2)(i) = BIT_AND(v1(i),v2(i))) & BV8_IOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_IOR(v1,v2)(i) = BIT_IOR(v1(i),v2(i))) & BV8_XOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_XOR(v1,v2)(i) = BIT_XOR(v1(i),v2(i))) & BV8_PAR : BV8 --> BIT & !(v1,t1,t2,t3,t4).(v1 : BV8 & t1 : BIT & t2 : BIT & t3 : BIT & t4 : BIT & t1 = BIT_XOR(v1(7),v1(6)) & t2 = BIT_XOR(v1(5),v1(4)) & t3 = BIT_XOR(v1(3),v1(2)) & t4 = BIT_XOR(v1(1),v1(0)) => BV8_PAR(v1) = BIT_XOR(BIT_XOR(t1,t2),BIT_XOR(t3,t4))) & BIT = 0..1 & BIT_FLIP : BIT --> BIT & !b.(b : BIT => BIT_FLIP(b) = 1-b) & BIT_AND : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_AND(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & BIT_IOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_IOR(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & BIT_XOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_XOR(b1,b2) = 1 <=> (b1 = 1 & b2 = 0 or (b1 = 0 & b2 = 1))) & add : WORD*WORD --> WORD*BOOL*BOOL & !(w1,w2,sum).(w1 : WORD & w2 : WORD & sum : NATURAL & sum = w1+w2 => (sum<=255 => add(w1,w2) = sum,bool(sum = 0),FALSE) & (256<=sum => add(w1,w2) = sum-256,bool(sum = 256),TRUE)) & substract : WORD*WORD --> WORD*BOOL*BOOL & !(w1,w2,diff).(w1 : WORD & w2 : WORD & diff : INTEGER & diff = w1-w2 => (diff<0 => substract(w1,w2) = diff+256,FALSE,TRUE) & (diff>=0 => substract(w1,w2) = diff,bool(diff = 0),FALSE)) & and : WORD*WORD --> WORD*BOOL & !(w1,w2,w).(w1 : WORD & w2 : WORD & w : WORD & w = BV_TO_WORD(BV8_AND(WORD_TO_BV(w1),WORD_TO_BV(w2))) => and(w1,w2) = w,bool(w = 0)) & ior : WORD*WORD --> WORD*BOOL & !(w1,w2,w).(w1 : WORD & w2 : WORD & w : WORD & w = BV_TO_WORD(BV8_IOR(WORD_TO_BV(w1),WORD_TO_BV(w2))) => ior(w1,w2) = w,bool(w = 0)) & xor : WORD*WORD --> WORD*BOOL & !(w1,w2,w).(w1 : WORD & w2 : WORD & w : WORD & w = BV_TO_WORD(BV8_XOR(WORD_TO_BV(w1),WORD_TO_BV(w2))) => xor(w1,w2) = w,bool(w = 0)) & par : WORD --> BIT & !w1.(w1 : WORD => par(w1) = BV8_PAR(WORD_TO_BV(w1))) & bitget : WORD*WORD_POSITION --> BIT & !(w,i).(w : WORD & i : WORD_POSITION => bitget(w,i) = WORD_TO_BV(w)(i)) & bitset : WORD*WORD_POSITION --> WORD & !(w,i).(w : WORD & i : WORD_POSITION => bitset(w,i) = BV_TO_WORD(BV8_SET_BIT(WORD_TO_BV(w),i,1))) & bitclear : WORD*WORD_POSITION --> WORD & !(w,i,b).(w : WORD & i : WORD_POSITION & b : BIT => bitclear(w,i) = BV_TO_WORD(BV8_SET_BIT(WORD_TO_BV(w),i,0))) & complement : WORD --> WORD & !w.(w : WORD => complement(w) = BV_TO_WORD(BV8_COMPLEMENT(WORD_TO_BV(w)))) & swap : WORD --> WORD & !(w,v).(w : WORD & v : BV8 => (v = WORD_TO_BV(w) => swap(w) = BV_TO_WORD({0|->v(4),1|->v(5),2|->v(6),3|->v(7),4|->v(0),5|->v(1),6|->v(2),7|->v(3)}))) & rotateleft : WORD --> WORD*BOOL & !(w,v).(w : WORD & v : BV8 => (v = WORD_TO_BV(w) => rotateleft(w) = BV_TO_WORD({0|->v(7),1|->v(0),2|->v(1),3|->v(2),4|->v(3),5|->v(4),6|->v(5),7|->v(6)}),bool(v(7) = 1))) & rotateright : WORD --> WORD*BOOL & !(w,v).(w : WORD & v : BV8 => (v = WORD_TO_BV(w) => rotateright(w) = BV_TO_WORD({0|->v(1),1|->v(2),2|->v(3),3|->v(4),4|->v(5),5|->v(6),6|->v(7),7|->v(0)}),bool(v(0) = 1))));
  Inherited_List_Properties(Machine(A8051))==(btrue);
  List_Properties(Machine(A8051))==(sbuf = 153 & R0 = 0 & R1 = 1 & R2 = 2 & R3 = 3 & R4 = 4 & R5 = 5 & R6 = 6 & R7 = 7 & a = 240 & P0 = 128 & P1 = 144 & P2 = 160 & P3 = 176 & scon = 152 & psw = 208 & P : BIT_ADDRESS & P = psw,0)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(A8051),Machine(ALU))==(?);
  Seen_Context_List_Enumerated(Machine(A8051))==(?);
  Seen_Context_List_Invariant(Machine(A8051))==(btrue);
  Seen_Context_List_Assertions(Machine(A8051))==(NB_WORD = 256 & !n.(n : WORD => 0<=n & n<=255) & INSTRUCTION_IJUMP(5,1) = 7 & INSTRUCTION_IJUMP(0,2) = 3 & INSTRUCTION_IJUMP(2,4) = 7 & INSTRUCTION_IJUMP(3,2) = 6 & INSTRUCTION_NEXT(0) = 1 & INSTRUCTION_NEXT(1) = 2 & INSTRUCTION_NEXT(2) = 3 & INSTRUCTION_NEXT(3) = 4 & INSTRUCTION_NEXT(4) = 5 & INSTRUCTION_NEXT(5) = 6 & INSTRUCTION_NEXT(6) = 7 & INSTRUCTION_IJUMP(3,3) = 7 & MEM_ADDR <: WORD & RAM_ADDR <: WORD & SFR_ADDR <: WORD & MEM_ADDR = RAM_ADDR\/SFR_ADDR & BIT_FLIP(0) = 1 & BIT_FLIP(1) = 0 & BIT_AND(0,0) = 0 & BIT_AND(0,1) = 0 & BIT_AND(1,0) = 0 & BIT_AND(1,1) = 1 & BIT_IOR(0,0) = 0 & BIT_IOR(0,1) = 0 & BIT_IOR(1,0) = 0 & BIT_IOR(1,1) = 1 & BIT_XOR(0,0) = 0 & BIT_XOR(0,1) = 1 & BIT_XOR(1,0) = 1 & BIT_XOR(1,1) = 0);
  Seen_Context_List_Properties(Machine(A8051))==(MAX_ADDR : NATURAL & RAM_ADDR = 0..127 & SFR_ADDR = 128..MAX_ADDR & MAX_ADDR = 255 & MEM_ADDR = 0..MAX_ADDR & NB_WORD : NATURAL & WORD_LEN : NATURAL & WORD_LEN = 8 & WORD_POSITION = 0..WORD_LEN-1 & NB_WORD = 2**WORD_LEN & WORD = 0..NB_WORD-1 & NB_INSTRUCTIONS = 256 & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = 0..INSTRUCTION_MAX & INSTRUCTION_IJUMP : INSTRUCTION*INSTRUCTION --> INSTRUCTION & !(i,j,sum).(i : INSTRUCTION & j : INSTRUCTION & sum : NATURAL & sum = i+j => (sum>=255 => INSTRUCTION_IJUMP(i,j) = 0) & (sum<255 => INSTRUCTION_IJUMP(i,j) = sum+1)) & INSTRUCTION_NEXT : INSTRUCTION -->> INSTRUCTION & !i.(i : INSTRUCTION => (i = 255 => INSTRUCTION_NEXT(i) = 0) & (i<255 => INSTRUCTION_NEXT(i) = i+1)) & WORD_TO_INT : WORD -->> INT & !(i,j).(i : WORD & j : NATURAL & i>0 & i<=255 & j = i+0 & 0<j & j<=255 => WORD_TO_INT(i) = j) & BV_TO_WORD : BV8 --> WORD & WORD_TO_BV : WORD --> BV8 & !(w,v).(w : WORD & v : BV8 => v = WORD_TO_BV(w) <=> (w = 128*v(7)+64*v(6)+32*v(5)+16*v(4)+8*v(3)+4*v(2)+2*v(1)+v(0))) & BV_TO_WORD = WORD_TO_BV~ & BIT_ADDRESS = WORD*WORD_POSITION & BV8_INDEX = 0..7 & BV8 = BV8_INDEX --> BIT & BV8_SET_BIT : BV8*BV8_INDEX*BIT --> BV8 & !(v,i,j,b).(v : BV8 & i : BV8_INDEX & j : BV8_INDEX & b : BIT => (i/=j => BV8_SET_BIT(v,i,b)(j) = v(j))) & !(v,i,b).(v : BV8 & i : BV8_INDEX & b : BIT => BV8_SET_BIT(v,i,b)(i) = b) & BV8_COMPLEMENT : BV8 --> BV8 & !(v,i).(v : BV8 & i : BV8_INDEX => BV8_COMPLEMENT(v)(i) = BIT_FLIP(v(i))) & BV8_ALL_ZEROES : BV8 & !i.(i : BV8_INDEX => BV8_ALL_ZEROES(i) = 0) & BV8_AND : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_AND(v1,v2)(i) = BIT_AND(v1(i),v2(i))) & BV8_IOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_IOR(v1,v2)(i) = BIT_IOR(v1(i),v2(i))) & BV8_XOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_XOR(v1,v2)(i) = BIT_XOR(v1(i),v2(i))) & BV8_PAR : BV8 --> BIT & !(v1,t1,t2,t3,t4).(v1 : BV8 & t1 : BIT & t2 : BIT & t3 : BIT & t4 : BIT & t1 = BIT_XOR(v1(7),v1(6)) & t2 = BIT_XOR(v1(5),v1(4)) & t3 = BIT_XOR(v1(3),v1(2)) & t4 = BIT_XOR(v1(1),v1(0)) => BV8_PAR(v1) = BIT_XOR(BIT_XOR(t1,t2),BIT_XOR(t3,t4))) & BIT = 0..1 & BIT_FLIP : BIT --> BIT & !b.(b : BIT => BIT_FLIP(b) = 1-b) & BIT_AND : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_AND(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & BIT_IOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_IOR(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & BIT_XOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_XOR(b1,b2) = 1 <=> (b1 = 1 & b2 = 0 or (b1 = 0 & b2 = 1))));
  Seen_List_Constraints(Machine(A8051))==(btrue);
  Seen_List_Operations(Machine(A8051),Machine(ALU))==(?);
  Seen_Expanded_List_Invariant(Machine(A8051),Machine(ALU))==(btrue);
  Seen_Internal_List_Operations(Machine(A8051),Machine(TYPES))==(?);
  Seen_List_Operations(Machine(A8051),Machine(TYPES))==(?);
  Seen_Expanded_List_Invariant(Machine(A8051),Machine(TYPES))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(A8051)) == (sbuf,R0,R1,R2,R3,R4,R5,R6,R7,a,P0,P1,P2,P3,scon,psw,P | ? | ? | ? | CJNE,CJNEI,MOV,MOVI,ANL,ACALL,SJMP,JC,JNB,CLR,AJMP,ADDS,ADDD,SUBBS,SUBBD,INC,DEC,INCS | ? | seen(Machine(TYPES)),seen(Machine(ALU)) | ? | A8051);
  List_Of_HiddenCst_Ids(Machine(A8051)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(A8051)) == (sbuf,R0,R1,R2,R3,R4,R5,R6,R7,a,P0,P1,P2,P3,scon,psw,P);
  List_Of_VisibleVar_Ids(Machine(A8051)) == (sp,pc,mem,stack | ?);
  List_Of_Ids_SeenBNU(Machine(A8051)) == (? : ?);
  List_Of_Ids(Machine(ALU)) == (add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright,par | ? | ? | ? | ? | ? | seen(Machine(TYPES)) | ? | ALU);
  List_Of_HiddenCst_Ids(Machine(ALU)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ALU)) == (add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright,par);
  List_Of_VisibleVar_Ids(Machine(ALU)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ALU)) == (? : ?);
  List_Of_Ids(Machine(TYPES)) == (WORD,WORD_LEN,WORD_POSITION,NB_WORD,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_IJUMP,INSTRUCTION_NEXT,WORD_TO_INT,BV_TO_WORD,WORD_TO_BV,MEM_ADDR,RAM_ADDR,SFR_ADDR,MAX_ADDR,BIT_ADDRESS | BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR,BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BV8_PAR | ? | ? | ? | ? | included(Machine(TYPE_BV8)),included(Machine(TYPE_BIT)) | ? | TYPES);
  List_Of_HiddenCst_Ids(Machine(TYPES)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (WORD,WORD_LEN,WORD_POSITION,NB_WORD,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_IJUMP,INSTRUCTION_NEXT,WORD_TO_INT,BV_TO_WORD,WORD_TO_BV,MEM_ADDR,RAM_ADDR,SFR_ADDR,MAX_ADDR,BIT_ADDRESS,BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR,BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BV8_PAR);
  List_Of_VisibleVar_Ids(Machine(TYPES)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPES)) == (seen(Machine(TYPE_BIT)) : (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(TYPE_BIT)) == (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | ? | ? | ? | TYPE_BIT);
  List_Of_HiddenCst_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BIT)) == (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR);
  List_Of_VisibleVar_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BIT)) == (? : ?);
  List_Of_Ids(Machine(TYPE_BV8)) == (BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BV8_PAR | ? | ? | ? | ? | ? | seen(Machine(TYPE_BIT)) | ? | TYPE_BV8);
  List_Of_HiddenCst_Ids(Machine(TYPE_BV8)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BV8)) == (BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BV8_PAR);
  List_Of_VisibleVar_Ids(Machine(TYPE_BV8)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BV8)) == (? : ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(A8051)) == (Type(sbuf) == Cst(btype(INTEGER,?,?));Type(R0) == Cst(btype(INTEGER,?,?));Type(R1) == Cst(btype(INTEGER,?,?));Type(R2) == Cst(btype(INTEGER,?,?));Type(R3) == Cst(btype(INTEGER,?,?));Type(R4) == Cst(btype(INTEGER,?,?));Type(R5) == Cst(btype(INTEGER,?,?));Type(R6) == Cst(btype(INTEGER,?,?));Type(R7) == Cst(btype(INTEGER,?,?));Type(a) == Cst(btype(INTEGER,?,?));Type(P0) == Cst(btype(INTEGER,?,?));Type(P1) == Cst(btype(INTEGER,?,?));Type(P2) == Cst(btype(INTEGER,?,?));Type(P3) == Cst(btype(INTEGER,?,?));Type(scon) == Cst(btype(INTEGER,?,?));Type(psw) == Cst(btype(INTEGER,?,?));Type(P) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?)))
END
&
THEORY VisibleVariablesEnvX IS
  VisibleVariables(Machine(A8051)) == (Type(sp) == Mvv(btype(INTEGER,?,?));Type(pc) == Mvv(btype(INTEGER,?,?));Type(mem) == Mvv(SetOf(btype(INTEGER,"[MEM_ADDR","]MEM_ADDR")*btype(INTEGER,"[WORD","]WORD")));Type(stack) == Mvv(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(A8051)) == (Type(INCS) == Cst(No_type,btype(INTEGER,?,?));Type(DEC) == Cst(No_type,No_type);Type(INC) == Cst(No_type,No_type);Type(SUBBD) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SUBBS) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ADDD) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ADDS) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(AJMP) == Cst(No_type,btype(INTEGER,?,?));Type(CLR) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(JNB) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(JC) == Cst(No_type,btype(INTEGER,?,?));Type(SJMP) == Cst(No_type,btype(INTEGER,?,?));Type(ACALL) == Cst(No_type,btype(INTEGER,?,?));Type(ANL) == Cst(No_type,btype(INTEGER,?,?));Type(MOVI) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(MOV) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(CJNEI) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(CJNE) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)))
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
