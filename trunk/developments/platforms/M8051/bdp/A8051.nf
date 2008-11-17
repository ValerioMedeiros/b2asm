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
  List_Sees(Machine(A8051))==(ALU,TYPES,BYTE_DEFINITION,BIT_DEFINITION)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(A8051))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(A8051))==(MEMORY);
  List_Includes(Machine(A8051))==(MEMORY)
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
  List_Variables(Machine(A8051))==(mem);
  External_List_Variables(Machine(A8051))==(mem)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(A8051))==(?);
  Abstract_List_VisibleVariables(Machine(A8051))==(?);
  External_List_VisibleVariables(Machine(A8051))==(?);
  Expanded_List_VisibleVariables(Machine(A8051))==(?);
  List_VisibleVariables(Machine(A8051))==(pc);
  Internal_List_VisibleVariables(Machine(A8051))==(pc)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(A8051))==(btrue);
  Gluing_List_Invariant(Machine(A8051))==(btrue);
  Abstract_List_Invariant(Machine(A8051))==(btrue);
  Expanded_List_Invariant(Machine(A8051))==(mem: MEM_ADDR --> BYTE & mem(PSW)(0) = bv_par(mem(ACC)));
  Context_List_Invariant(Machine(A8051))==(btrue);
  List_Invariant(Machine(A8051))==(pc: ROM_ADDR)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Machine(A8051))==(btrue);
  Expanded_List_Assertions(Machine(A8051))==(ACC/:RAM_ADDR & RAM_ADDR <: MEM_ADDR & SFR_ADDR <: MEM_ADDR & RAM_ADDR\/SFR_ADDR = MEM_ADDR & RAM_BIT_ADDRESSABLE <: RAM_ADDR & SFR_BIT_ADDRESSABLE <: SFR_ADDR & BIT_ADDRESSABLE <: BIT_ADDRESS & ROM_ADDR <: USHORT_INT & !(xx,yy).(xx: ROM_ADDR & yy: ROM_ADDR => (xx+yy>ROM_ADDR_MAX => PC_INCREMENT(xx,yy) = xx+yy-(ROM_ADDR_MAX+1)) & (not(xx+yy>ROM_ADDR_MAX) => PC_INCREMENT(xx,yy) = xx+yy)) & !xx.(xx: BYTE => not(SP_INCREMENT(xx,1) = SP_INCREMENT(xx,2))));
  Context_List_Assertions(Machine(A8051))==(dom(parity) = BYTE & ran(parity) <: BIT & dom(add) = UCHAR*UCHAR*UCHAR & ran(add) <: UCHAR*BIT*BIT*BIT*BIT & dom(sub) = UCHAR*UCHAR*UCHAR & ran(sub) <: UCHAR*BIT*BIT*BIT*BIT & dom(and) = BYTE*BYTE & ran(and) <: BYTE & dom(ior) = BYTE*BYTE & ran(ior) <: BYTE & dom(xor) = BYTE*BYTE & ran(xor) <: BYTE & dom(complement) = BYTE & ran(complement) <: BYTE & dom(swap) = BYTE & ran(swap) <: BYTE & dom(rotateleft) = BYTE & ran(rotateleft) <: BYTE & dom(rotateright) = BYTE & ran(rotateright) <: BYTE & dom(byte_schar) = BYTE & ran(byte_schar) <: SCHAR & dom(schar_byte) = SCHAR & ran(schar_byte) <: BYTE & dom(byte_uchar) = BYTE & ran(byte_uchar) <: UCHAR & dom(uchar_byte) = UCHAR & ran(uchar_byte) <: BYTE & dom(uchar_schar) = UCHAR & ran(uchar_schar) <: SCHAR & dom(schar_uchar) = SCHAR & ran(schar_uchar) <: UCHAR & !bt.(bt: BYTE => size(bt) = 8) & size(BYTE_ZERO) = 8 & BYTE <: BIT_VECTOR & BYTE_ZERO: BIT_VECTOR & first(BYTE_ZERO) = 0 & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_to_bit(TRUE) = 1 & bool_to_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0));
  List_Assertions(Machine(A8051))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(A8051))==(mem:=MEM_ADDR*{BYTE_ZERO};pc:=0);
  Context_List_Initialisation(Machine(A8051))==(skip);
  List_Initialisation(Machine(A8051))==(pc:=0)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(A8051))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(A8051),Machine(MEMORY))==(?);
  List_Instanciated_Parameters(Machine(A8051),Machine(ALU))==(?);
  List_Instanciated_Parameters(Machine(A8051),Machine(TYPES))==(?);
  List_Instanciated_Parameters(Machine(A8051),Machine(BYTE_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(A8051),Machine(BIT_DEFINITION))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Machine(A8051),Machine(MEMORY))==(btrue);
  List_Context_Constraints(Machine(A8051))==(btrue);
  List_Constraints(Machine(A8051))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(A8051))==(INIT,ADD,ADDI,ADDD,ADDC,ADDCI,ADDCD,ANL,ANLI,ANLD,ANLDA,ANLDD,ANLB,CJNE,CJNED,CJNEI,CLR,CLRA,CPL,CPLA,DEC,DECI,DJNZ,INC,INCI,JB,JBC,JC,JNB,JNC,JNZ,JZ,LCALL,LJMP,MOV,MOVD,MOVDI,MOVI,MOVID,MOVB,NOP,ORL,ORLD,ORLI,ORLB,POP,PUSH,RL,RR,SETB,SJMP);
  List_Operations(Machine(A8051))==(INIT,ADD,ADDI,ADDD,ADDC,ADDCI,ADDCD,ANL,ANLI,ANLD,ANLDA,ANLDD,ANLB,CJNE,CJNED,CJNEI,CLR,CLRA,CPL,CPLA,DEC,DECI,DJNZ,INC,INCI,JB,JBC,JC,JNB,JNC,JNZ,JZ,LCALL,LJMP,MOV,MOVD,MOVDI,MOVI,MOVID,MOVB,NOP,ORL,ORLD,ORLI,ORLB,POP,PUSH,RL,RR,SETB,SJMP)
END
&
THEORY ListInputX IS
  List_Input(Machine(A8051),INIT)==(?);
  List_Input(Machine(A8051),ADD)==(ac,src);
  List_Input(Machine(A8051),ADDI)==(ac,Rn);
  List_Input(Machine(A8051),ADDD)==(ac,data);
  List_Input(Machine(A8051),ADDC)==(ac,src);
  List_Input(Machine(A8051),ADDCI)==(ac,Rn);
  List_Input(Machine(A8051),ADDCD)==(ac,data);
  List_Input(Machine(A8051),ANL)==(acc,direct);
  List_Input(Machine(A8051),ANLI)==(acc,Rn);
  List_Input(Machine(A8051),ANLD)==(acc,data);
  List_Input(Machine(A8051),ANLDA)==(dest,acc);
  List_Input(Machine(A8051),ANLDD)==(dest,data);
  List_Input(Machine(A8051),ANLB)==(cy,bit);
  List_Input(Machine(A8051),CJNE)==(acc,direct,jump);
  List_Input(Machine(A8051),CJNED)==(dest,data,jump);
  List_Input(Machine(A8051),CJNEI)==(Rn,data,jump);
  List_Input(Machine(A8051),CLR)==(bt);
  List_Input(Machine(A8051),CLRA)==(addr);
  List_Input(Machine(A8051),CPL)==(bit);
  List_Input(Machine(A8051),CPLA)==(acc);
  List_Input(Machine(A8051),DEC)==(src);
  List_Input(Machine(A8051),DECI)==(Rn);
  List_Input(Machine(A8051),DJNZ)==(direct,jump);
  List_Input(Machine(A8051),INC)==(src);
  List_Input(Machine(A8051),INCI)==(Rn);
  List_Input(Machine(A8051),JB)==(bit,jump);
  List_Input(Machine(A8051),JBC)==(bit,jump);
  List_Input(Machine(A8051),JC)==(jump);
  List_Input(Machine(A8051),JNB)==(bit,jump);
  List_Input(Machine(A8051),JNC)==(bit,jump);
  List_Input(Machine(A8051),JNZ)==(jump);
  List_Input(Machine(A8051),JZ)==(jump);
  List_Input(Machine(A8051),LCALL)==(rom_addr);
  List_Input(Machine(A8051),LJMP)==(rom_addr);
  List_Input(Machine(A8051),MOV)==(direct_d,direct_s);
  List_Input(Machine(A8051),MOVD)==(direct,data);
  List_Input(Machine(A8051),MOVDI)==(direct,Rn);
  List_Input(Machine(A8051),MOVI)==(Rn,direct);
  List_Input(Machine(A8051),MOVID)==(Rn,data);
  List_Input(Machine(A8051),MOVB)==(dest,src);
  List_Input(Machine(A8051),NOP)==(?);
  List_Input(Machine(A8051),ORL)==(dest,src);
  List_Input(Machine(A8051),ORLD)==(dest,data);
  List_Input(Machine(A8051),ORLI)==(dest,Rn);
  List_Input(Machine(A8051),ORLB)==(dest,src);
  List_Input(Machine(A8051),POP)==(direct);
  List_Input(Machine(A8051),PUSH)==(direct);
  List_Input(Machine(A8051),RL)==(acc);
  List_Input(Machine(A8051),RR)==(acc);
  List_Input(Machine(A8051),SETB)==(bit);
  List_Input(Machine(A8051),SJMP)==(jump)
END
&
THEORY ListOutputX IS
  List_Output(Machine(A8051),INIT)==(?);
  List_Output(Machine(A8051),ADD)==(?);
  List_Output(Machine(A8051),ADDI)==(?);
  List_Output(Machine(A8051),ADDD)==(?);
  List_Output(Machine(A8051),ADDC)==(?);
  List_Output(Machine(A8051),ADDCI)==(?);
  List_Output(Machine(A8051),ADDCD)==(?);
  List_Output(Machine(A8051),ANL)==(?);
  List_Output(Machine(A8051),ANLI)==(?);
  List_Output(Machine(A8051),ANLD)==(?);
  List_Output(Machine(A8051),ANLDA)==(?);
  List_Output(Machine(A8051),ANLDD)==(?);
  List_Output(Machine(A8051),ANLB)==(?);
  List_Output(Machine(A8051),CJNE)==(?);
  List_Output(Machine(A8051),CJNED)==(?);
  List_Output(Machine(A8051),CJNEI)==(?);
  List_Output(Machine(A8051),CLR)==(?);
  List_Output(Machine(A8051),CLRA)==(?);
  List_Output(Machine(A8051),CPL)==(?);
  List_Output(Machine(A8051),CPLA)==(?);
  List_Output(Machine(A8051),DEC)==(?);
  List_Output(Machine(A8051),DECI)==(?);
  List_Output(Machine(A8051),DJNZ)==(?);
  List_Output(Machine(A8051),INC)==(?);
  List_Output(Machine(A8051),INCI)==(?);
  List_Output(Machine(A8051),JB)==(?);
  List_Output(Machine(A8051),JBC)==(?);
  List_Output(Machine(A8051),JC)==(?);
  List_Output(Machine(A8051),JNB)==(?);
  List_Output(Machine(A8051),JNC)==(?);
  List_Output(Machine(A8051),JNZ)==(?);
  List_Output(Machine(A8051),JZ)==(?);
  List_Output(Machine(A8051),LCALL)==(?);
  List_Output(Machine(A8051),LJMP)==(?);
  List_Output(Machine(A8051),MOV)==(?);
  List_Output(Machine(A8051),MOVD)==(?);
  List_Output(Machine(A8051),MOVDI)==(?);
  List_Output(Machine(A8051),MOVI)==(?);
  List_Output(Machine(A8051),MOVID)==(?);
  List_Output(Machine(A8051),MOVB)==(?);
  List_Output(Machine(A8051),NOP)==(?);
  List_Output(Machine(A8051),ORL)==(?);
  List_Output(Machine(A8051),ORLD)==(?);
  List_Output(Machine(A8051),ORLI)==(?);
  List_Output(Machine(A8051),ORLB)==(?);
  List_Output(Machine(A8051),POP)==(?);
  List_Output(Machine(A8051),PUSH)==(?);
  List_Output(Machine(A8051),RL)==(?);
  List_Output(Machine(A8051),RR)==(?);
  List_Output(Machine(A8051),SETB)==(?);
  List_Output(Machine(A8051),SJMP)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(A8051),INIT)==(INIT);
  List_Header(Machine(A8051),ADD)==(ADD(ac,src));
  List_Header(Machine(A8051),ADDI)==(ADDI(ac,Rn));
  List_Header(Machine(A8051),ADDD)==(ADDD(ac,data));
  List_Header(Machine(A8051),ADDC)==(ADDC(ac,src));
  List_Header(Machine(A8051),ADDCI)==(ADDCI(ac,Rn));
  List_Header(Machine(A8051),ADDCD)==(ADDCD(ac,data));
  List_Header(Machine(A8051),ANL)==(ANL(acc,direct));
  List_Header(Machine(A8051),ANLI)==(ANLI(acc,Rn));
  List_Header(Machine(A8051),ANLD)==(ANLD(acc,data));
  List_Header(Machine(A8051),ANLDA)==(ANLDA(dest,acc));
  List_Header(Machine(A8051),ANLDD)==(ANLDD(dest,data));
  List_Header(Machine(A8051),ANLB)==(ANLB(cy,bit));
  List_Header(Machine(A8051),CJNE)==(CJNE(acc,direct,jump));
  List_Header(Machine(A8051),CJNED)==(CJNED(dest,data,jump));
  List_Header(Machine(A8051),CJNEI)==(CJNEI(Rn,data,jump));
  List_Header(Machine(A8051),CLR)==(CLR(bt));
  List_Header(Machine(A8051),CLRA)==(CLRA(addr));
  List_Header(Machine(A8051),CPL)==(CPL(bit));
  List_Header(Machine(A8051),CPLA)==(CPLA(acc));
  List_Header(Machine(A8051),DEC)==(DEC(src));
  List_Header(Machine(A8051),DECI)==(DECI(Rn));
  List_Header(Machine(A8051),DJNZ)==(DJNZ(direct,jump));
  List_Header(Machine(A8051),INC)==(INC(src));
  List_Header(Machine(A8051),INCI)==(INCI(Rn));
  List_Header(Machine(A8051),JB)==(JB(bit,jump));
  List_Header(Machine(A8051),JBC)==(JBC(bit,jump));
  List_Header(Machine(A8051),JC)==(JC(jump));
  List_Header(Machine(A8051),JNB)==(JNB(bit,jump));
  List_Header(Machine(A8051),JNC)==(JNC(bit,jump));
  List_Header(Machine(A8051),JNZ)==(JNZ(jump));
  List_Header(Machine(A8051),JZ)==(JZ(jump));
  List_Header(Machine(A8051),LCALL)==(LCALL(rom_addr));
  List_Header(Machine(A8051),LJMP)==(LJMP(rom_addr));
  List_Header(Machine(A8051),MOV)==(MOV(direct_d,direct_s));
  List_Header(Machine(A8051),MOVD)==(MOVD(direct,data));
  List_Header(Machine(A8051),MOVDI)==(MOVDI(direct,Rn));
  List_Header(Machine(A8051),MOVI)==(MOVI(Rn,direct));
  List_Header(Machine(A8051),MOVID)==(MOVID(Rn,data));
  List_Header(Machine(A8051),MOVB)==(MOVB(dest,src));
  List_Header(Machine(A8051),NOP)==(NOP);
  List_Header(Machine(A8051),ORL)==(ORL(dest,src));
  List_Header(Machine(A8051),ORLD)==(ORLD(dest,data));
  List_Header(Machine(A8051),ORLI)==(ORLI(dest,Rn));
  List_Header(Machine(A8051),ORLB)==(ORLB(dest,src));
  List_Header(Machine(A8051),POP)==(POP(direct));
  List_Header(Machine(A8051),PUSH)==(PUSH(direct));
  List_Header(Machine(A8051),RL)==(RL(acc));
  List_Header(Machine(A8051),RR)==(RR(acc));
  List_Header(Machine(A8051),SETB)==(SETB(bit));
  List_Header(Machine(A8051),SJMP)==(SJMP(jump))
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(A8051),INIT)==(btrue);
  List_Precondition(Machine(A8051),ADD)==(ac = ACC & src: MEM_ADDR);
  List_Precondition(Machine(A8051),ADDI)==(Rn: SFR_ADDR & ac = ACC & (Rn = R0 or Rn = R1) & byte_uchar(mem(Rn)): RAM_ADDR);
  List_Precondition(Machine(A8051),ADDD)==(ac: SFR_ADDR & ac = ACC & data: UCHAR);
  List_Precondition(Machine(A8051),ADDC)==(ac = ACC & src: MEM_ADDR);
  List_Precondition(Machine(A8051),ADDCI)==(Rn: SFR_ADDR & ac = ACC & (Rn = R0 or Rn = R1) & byte_uchar(mem(Rn)): RAM_ADDR);
  List_Precondition(Machine(A8051),ADDCD)==(ac: SFR_ADDR & ac = ACC & data: UCHAR);
  List_Precondition(Machine(A8051),ANL)==(acc = ACC & direct: MEM_ADDR);
  List_Precondition(Machine(A8051),ANLI)==(acc = ACC & Rn: RAM_ADDR & (Rn = R1 or Rn = R2) & byte_uchar(mem(Rn)): RAM_ADDR);
  List_Precondition(Machine(A8051),ANLD)==(acc = ACC & data: UCHAR);
  List_Precondition(Machine(A8051),ANLDA)==(dest: MEM_ADDR & acc = ACC);
  List_Precondition(Machine(A8051),ANLDD)==(dest: MEM_ADDR & data: UCHAR);
  List_Precondition(Machine(A8051),ANLB)==(cy = CY & bit: BIT_ADDRESS);
  List_Precondition(Machine(A8051),CJNE)==(acc = ACC & direct: MEM_ADDR & jump: ROM_ADDR);
  List_Precondition(Machine(A8051),CJNED)==(dest: R0..R7\/{ACC} & data: UCHAR & jump: ROM_ADDR);
  List_Precondition(Machine(A8051),CJNEI)==(Rn: RAM_ADDR & (Rn = R1 or Rn = R2) & data: UCHAR & jump: ROM_ADDR & byte_uchar(mem(Rn)): RAM_ADDR);
  List_Precondition(Machine(A8051),CLR)==(bt: BIT_ADDRESS);
  List_Precondition(Machine(A8051),CLRA)==(addr = ACC);
  List_Precondition(Machine(A8051),CPL)==(bit: BIT_ADDRESS);
  List_Precondition(Machine(A8051),CPLA)==(acc = ACC);
  List_Precondition(Machine(A8051),DEC)==(src: MEM_ADDR);
  List_Precondition(Machine(A8051),DECI)==(Rn: SFR_ADDR & (Rn = R0 or Rn = R1) & byte_uchar(mem(Rn)): RAM_ADDR);
  List_Precondition(Machine(A8051),DJNZ)==(direct: RAM_ADDR & jump: ROM_ADDR);
  List_Precondition(Machine(A8051),INC)==(src: MEM_ADDR);
  List_Precondition(Machine(A8051),INCI)==(Rn: SFR_ADDR & (Rn = R0 or Rn = R1) & byte_uchar(mem(Rn)): RAM_ADDR);
  List_Precondition(Machine(A8051),JB)==(bit: BIT_ADDRESS & jump: ROM_ADDR);
  List_Precondition(Machine(A8051),JBC)==(bit: BIT_ADDRESS & jump: ROM_ADDR);
  List_Precondition(Machine(A8051),JC)==(jump: ROM_ADDR);
  List_Precondition(Machine(A8051),JNB)==(bit: BIT_ADDRESS & jump: ROM_ADDR);
  List_Precondition(Machine(A8051),JNC)==(bit: BIT_ADDRESS & jump: ROM_ADDR);
  List_Precondition(Machine(A8051),JNZ)==(jump: ROM_ADDR);
  List_Precondition(Machine(A8051),JZ)==(jump: ROM_ADDR);
  List_Precondition(Machine(A8051),LCALL)==(rom_addr: ROM_ADDR);
  List_Precondition(Machine(A8051),LJMP)==(rom_addr: ROM_ADDR);
  List_Precondition(Machine(A8051),MOV)==(direct_d: RAM_ADDR & direct_s: RAM_ADDR);
  List_Precondition(Machine(A8051),MOVD)==(direct: MEM_ADDR & data: UCHAR);
  List_Precondition(Machine(A8051),MOVDI)==(direct: MEM_ADDR & Rn: RAM_ADDR & (Rn = R1 or Rn = R2) & byte_uchar(mem(Rn)): RAM_ADDR);
  List_Precondition(Machine(A8051),MOVI)==(Rn: RAM_ADDR & (Rn = R1 or Rn = R2) & byte_uchar(mem(Rn)): RAM_ADDR & direct: MEM_ADDR);
  List_Precondition(Machine(A8051),MOVID)==(Rn: RAM_ADDR & (Rn = R1 or Rn = R2) & byte_uchar(mem(Rn)): RAM_ADDR & data: UCHAR);
  List_Precondition(Machine(A8051),MOVB)==(dest: BIT_ADDRESS & src: BIT_ADDRESS & (dest = CY or src = CY));
  List_Precondition(Machine(A8051),NOP)==(btrue);
  List_Precondition(Machine(A8051),ORL)==(dest: MEM_ADDR & src: MEM_ADDR & (dest = ACC or src = ACC));
  List_Precondition(Machine(A8051),ORLD)==(dest: MEM_ADDR & data: UCHAR);
  List_Precondition(Machine(A8051),ORLI)==(dest: MEM_ADDR & Rn: RAM_ADDR & (Rn = R0 or Rn = R1) & byte_uchar(mem(Rn)): RAM_ADDR);
  List_Precondition(Machine(A8051),ORLB)==(dest: BIT_ADDRESS & src: BIT_ADDRESS & (src = CY or dest = CY));
  List_Precondition(Machine(A8051),POP)==(direct: MEM_ADDR);
  List_Precondition(Machine(A8051),PUSH)==(direct: MEM_ADDR);
  List_Precondition(Machine(A8051),RL)==(acc = ACC);
  List_Precondition(Machine(A8051),RR)==(acc = ACC);
  List_Precondition(Machine(A8051),SETB)==(bit: BIT_ADDRESS);
  List_Precondition(Machine(A8051),SJMP)==(jump: ROM_ADDR)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(A8051),SJMP)==(jump: ROM_ADDR | pc:=PC_INCREMENT(pc,jump+1));
  Expanded_List_Substitution(Machine(A8051),SETB)==(bit: BIT_ADDRESS & bit: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = bit ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,1,mem(PSW))) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),RR)==(acc = ACC & ACC: MEM_ADDR & rotateright(mem(ACC)): BYTE | mem:=ADDRCHANGE(ACC,rotateright(mem(ACC)),mem(PSW)) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),RL)==(acc = ACC & ACC: MEM_ADDR & rotateleft(mem(ACC)): BYTE | mem:=ADDRCHANGE(ACC,rotateleft(mem(ACC)),mem(PSW)) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),PUSH)==(direct: MEM_ADDR & mem(direct): BYTE | mem:=mem<+{SP_INCREMENT(mem(SP),1)|->mem(direct)} || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),POP)==(direct: MEM_ADDR | mem:=mem<+(ADDRCHANGE(direct,mem(SP),mem(PSW))\/{SP|->uchar_byte(SP_DECREMENT(mem(SP),1))}));
  Expanded_List_Substitution(Machine(A8051),ORLB)==(dest: BIT_ADDRESS & src: BIT_ADDRESS & (src = CY or dest = CY) | BIT_GET(src,mem) = 1 ==> (src: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = src ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,1,mem(PSW)))) [] not(BIT_GET(src,mem) = 1) ==> skip || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),ORLI)==(dest: MEM_ADDR & Rn: RAM_ADDR & (Rn = R0 or Rn = R1) & byte_uchar(mem(Rn)): RAM_ADDR | @any(src).(src: RAM_ADDR & src = byte_uchar(mem(Rn)) ==> (dest: MEM_ADDR & ior(mem(dest),mem(src)): BYTE | mem:=ADDRCHANGE(dest,ior(mem(dest),mem(src)),mem(PSW)) || pc:=PC_INCREMENT(pc,1))));
  Expanded_List_Substitution(Machine(A8051),ORLD)==(dest: MEM_ADDR & data: UCHAR & dest: MEM_ADDR & ior(mem(dest),uchar_byte(data)): BYTE | mem:=ADDRCHANGE(dest,ior(mem(dest),uchar_byte(data)),mem(PSW)) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),ORL)==(dest: MEM_ADDR & src: MEM_ADDR & (dest = ACC or src = ACC) & dest: MEM_ADDR & ior(mem(dest),mem(src)): BYTE | mem:=ADDRCHANGE(dest,ior(mem(dest),mem(src)),mem(PSW)) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),NOP)==(btrue | pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),MOVB)==(dest: BIT_ADDRESS & src: BIT_ADDRESS & (dest = CY or src = CY) | BIT_GET(src,mem) = 1 ==> (src: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = src ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,1,mem(PSW)))) [] not(BIT_GET(src,mem) = 1) ==> (src: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = src ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,0,mem(PSW)))) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),MOVID)==(Rn: RAM_ADDR & (Rn = R1 or Rn = R2) & byte_uchar(mem(Rn)): RAM_ADDR & data: UCHAR & Rn: RAM_ADDR & (Rn = R1 or Rn = R2) & uchar_byte(data): BYTE & byte_uchar(mem(Rn)): RAM_ADDR | @any(addr).(addr: RAM_ADDR & addr = byte_uchar(mem(Rn)) ==> mem:=mem<+ADDRCHANGE(addr,uchar_byte(data),mem(PSW))) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),MOVI)==(Rn: RAM_ADDR & (Rn = R1 or Rn = R2) & byte_uchar(mem(Rn)): RAM_ADDR & direct: MEM_ADDR & Rn: RAM_ADDR & (Rn = R1 or Rn = R2) & mem(direct): BYTE & byte_uchar(mem(Rn)): RAM_ADDR | @any(addr).(addr: RAM_ADDR & addr = byte_uchar(mem(Rn)) ==> mem:=mem<+ADDRCHANGE(addr,mem(direct),mem(PSW))) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),MOVDI)==(direct: MEM_ADDR & Rn: RAM_ADDR & (Rn = R1 or Rn = R2) & byte_uchar(mem(Rn)): RAM_ADDR | @any(src).(src: RAM_ADDR & src = byte_uchar(mem(Rn)) ==> (direct: MEM_ADDR & mem(src): BYTE | mem:=ADDRCHANGE(direct,mem(src),mem(PSW)) || pc:=PC_INCREMENT(pc,1))));
  Expanded_List_Substitution(Machine(A8051),MOVD)==(direct: MEM_ADDR & data: UCHAR & direct: MEM_ADDR & uchar_byte(data): BYTE | mem:=ADDRCHANGE(direct,uchar_byte(data),mem(PSW)) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),MOV)==(direct_d: RAM_ADDR & direct_s: RAM_ADDR & direct_d: MEM_ADDR & mem(direct_s): BYTE | mem:=ADDRCHANGE(direct_d,mem(direct_s),mem(PSW)) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),LJMP)==(rom_addr: ROM_ADDR | pc:=rom_addr);
  Expanded_List_Substitution(Machine(A8051),LCALL)==(rom_addr: ROM_ADDR | @any(upper_pc,lower_pc).(upper_pc,lower_pc: BYTE*BYTE & upper_pc,lower_pc = usint_byte(PC_INCREMENT(pc,1)) ==> (lower_pc: BYTE & upper_pc: BYTE & mem(SP): BYTE | mem:=mem<+{SP_INCREMENT(mem(SP),1)|->lower_pc,SP_INCREMENT(mem(SP),2)|->upper_pc,SP|->uchar_byte(SP_INCREMENT(mem(SP),2))} || pc:=rom_addr)));
  Expanded_List_Substitution(Machine(A8051),JZ)==(jump: ROM_ADDR | byte_uchar(mem(ACC)) = 1 ==> pc:=PC_INCREMENT(pc,jump+1) [] not(byte_uchar(mem(ACC)) = 1) ==> pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),JNZ)==(jump: ROM_ADDR | byte_uchar(mem(ACC))/=0 ==> pc:=PC_INCREMENT(pc,jump+1) [] not(byte_uchar(mem(ACC))/=0) ==> pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),JNC)==(bit: BIT_ADDRESS & jump: ROM_ADDR | BIT_GET(bit,mem) = 0 ==> (bit: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = bit ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,0,mem(PSW))) || pc:=PC_INCREMENT(pc,jump+1)) [] not(BIT_GET(bit,mem) = 0) ==> pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),JNB)==(bit: BIT_ADDRESS & jump: ROM_ADDR | BIT_GET(bit,mem) = 0 ==> pc:=PC_INCREMENT(pc,jump+1) [] not(BIT_GET(bit,mem) = 0) ==> pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),JC)==(jump: ROM_ADDR | BIT_GET(CY,mem) = 1 ==> pc:=PC_INCREMENT(pc,1+jump) [] not(BIT_GET(CY,mem) = 1) ==> pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),JBC)==(bit: BIT_ADDRESS & jump: ROM_ADDR | BIT_GET(bit,mem) = 1 ==> (bit: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = bit ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,0,mem(PSW))) || pc:=PC_INCREMENT(pc,jump+1)) [] not(BIT_GET(bit,mem) = 1) ==> pc:=PC_INCREMENT(pc,jump+1));
  Expanded_List_Substitution(Machine(A8051),JB)==(bit: BIT_ADDRESS & jump: ROM_ADDR | BIT_GET(bit,mem) = 1 ==> pc:=PC_INCREMENT(pc,jump+1) [] not(BIT_GET(bit,mem) = 1) ==> pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),INCI)==(Rn: SFR_ADDR & (Rn = R0 or Rn = R1) & byte_uchar(mem(Rn)): RAM_ADDR | @any(acc,Pa,Cy,Ac,Ov,src).(acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & src: RAM_ADDR & src = byte_uchar(mem(Rn)) & acc,Pa,Ov,Ac,Cy = add(byte_uchar(mem(src)),1,0) ==> (src: MEM_ADDR & uchar_byte(acc): BYTE | mem:=ADDRCHANGE(src,uchar_byte(acc),mem(PSW)))) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),INC)==(src: MEM_ADDR | @any(acc,Pa,Cy,Ac,Ov).(acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & acc,Pa,Ov,Ac,Cy = add(byte_uchar(mem(src)),1,0) ==> (src: MEM_ADDR & uchar_byte(acc): BYTE | mem:=ADDRCHANGE(src,uchar_byte(acc),mem(PSW)))) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),DJNZ)==(direct: RAM_ADDR & jump: ROM_ADDR | @any(result,Pa,Cy,Ac,Ov).(result: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & result,Pa,Ov,Ac,Cy = sub(byte_uchar(mem(direct)),1,0) ==> (direct: MEM_ADDR & uchar_byte(result): BYTE | mem:=ADDRCHANGE(direct,uchar_byte(result),mem(PSW)) || (not(result = 0) ==> pc:=PC_INCREMENT(pc,jump+1) [] not(not(result = 0)) ==> pc:=PC_INCREMENT(pc,1)))));
  Expanded_List_Substitution(Machine(A8051),DECI)==(Rn: SFR_ADDR & (Rn = R0 or Rn = R1) & byte_uchar(mem(Rn)): RAM_ADDR | @any(acc,Pa,Cy,Ac,Ov,src).(acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & src: RAM_ADDR & src = byte_uchar(mem(Rn)) & acc,Pa,Ov,Ac,Cy = sub(byte_uchar(mem(src)),1,0) ==> (src: MEM_ADDR & uchar_byte(acc): BYTE | mem:=ADDRCHANGE(src,uchar_byte(acc),mem(PSW)))) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),DEC)==(src: MEM_ADDR | @any(acc,Pa,Cy,Ac,Ov).(acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & acc,Pa,Ov,Ac,Cy = sub(byte_uchar(mem(src)),1,0) ==> (src: MEM_ADDR & uchar_byte(acc): BYTE | mem:=ADDRCHANGE(src,uchar_byte(acc),mem(PSW)))) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),CPLA)==(acc = ACC & ACC: MEM_ADDR & complement(mem(ACC)): BYTE | mem:=ADDRCHANGE(ACC,complement(mem(ACC)),mem(PSW)) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),CPL)==(bit: BIT_ADDRESS | BIT_GET(bit,mem) = 0 ==> (bit: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = bit ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,1,mem(PSW)))) [] not(BIT_GET(bit,mem) = 0) ==> (bit: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = bit ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,0,mem(PSW)))) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),CLRA)==(addr = ACC & ACC: MEM_ADDR & uchar_byte(0): BYTE | mem:=ADDRCHANGE(ACC,uchar_byte(0),mem(PSW)) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),CLR)==(bt: BIT_ADDRESS & bt: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = bt ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,0,mem(PSW))) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),CJNEI)==(Rn: RAM_ADDR & (Rn = R1 or Rn = R2) & data: UCHAR & jump: ROM_ADDR & byte_uchar(mem(Rn)): RAM_ADDR | @any(addr$0).(addr$0: RAM_ADDR & addr$0 = byte_uchar(mem(Rn)) ==> (not(byte_uchar(mem(addr$0)) = data) ==> (byte_uchar(mem(addr$0))<data ==> (CY: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = CY ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,1,mem(PSW)))) [] not(byte_uchar(mem(addr$0))<data) ==> (CY: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = CY ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,0,mem(PSW)))) || pc:=PC_INCREMENT(pc,jump+1)) [] not(not(byte_uchar(mem(addr$0)) = data)) ==> pc:=PC_INCREMENT(pc,1))));
  Expanded_List_Substitution(Machine(A8051),CJNED)==(dest: R0..R7\/{ACC} & data: UCHAR & jump: ROM_ADDR | not(byte_uchar(mem(dest)) = data) ==> (byte_uchar(mem(dest))<data ==> (CY: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = CY ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,1,mem(PSW)))) [] not(byte_uchar(mem(dest))<data) ==> (CY: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = CY ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,0,mem(PSW)))) || pc:=PC_INCREMENT(pc,jump+1)) [] not(not(byte_uchar(mem(dest)) = data)) ==> pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),CJNE)==(acc = ACC & direct: MEM_ADDR & jump: ROM_ADDR | not(byte_uchar(mem(ACC)) = byte_uchar(mem(direct))) ==> (byte_uchar(mem(ACC))<byte_uchar(mem(direct)) ==> (CY: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = CY ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,1,mem(PSW)))) [] not(byte_uchar(mem(ACC))<byte_uchar(mem(direct))) ==> (CY: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = CY ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,0,mem(PSW)))) || pc:=PC_INCREMENT(pc,jump+1)) [] not(not(byte_uchar(mem(ACC)) = byte_uchar(mem(direct)))) ==> pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),ANLB)==(cy = CY & bit: BIT_ADDRESS | BIT_GET(bit,mem) = 0 ==> (CY: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = CY ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,0,mem(PSW)))) [] not(BIT_GET(bit,mem) = 0) ==> skip || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),ANLDD)==(dest: MEM_ADDR & data: UCHAR & dest: MEM_ADDR & and(mem(dest),uchar_byte(data)): BYTE | mem:=ADDRCHANGE(dest,and(mem(dest),uchar_byte(data)),mem(PSW)) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),ANLDA)==(dest: MEM_ADDR & acc = ACC & dest: MEM_ADDR & and(mem(dest),mem(ACC)): BYTE | mem:=ADDRCHANGE(dest,and(mem(dest),mem(ACC)),mem(PSW)) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),ANLD)==(acc = ACC & data: UCHAR & ACC: MEM_ADDR & and(mem(ACC),uchar_byte(data)): BYTE | mem:=ADDRCHANGE(ACC,and(mem(ACC),uchar_byte(data)),mem(PSW)) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),ANLI)==(acc = ACC & Rn: RAM_ADDR & (Rn = R1 or Rn = R2) & byte_uchar(mem(Rn)): RAM_ADDR | @any(addr).(addr: RAM_ADDR & addr = byte_uchar(mem(Rn)) ==> (ACC: MEM_ADDR & and(mem(ACC),mem(addr)): BYTE | mem:=ADDRCHANGE(ACC,and(mem(ACC),mem(addr)),mem(PSW)) || pc:=PC_INCREMENT(pc,1))));
  Expanded_List_Substitution(Machine(A8051),ANL)==(acc = ACC & direct: MEM_ADDR & ACC: MEM_ADDR & and(mem(ACC),mem(direct)): BYTE | mem:=ADDRCHANGE(ACC,and(mem(ACC),mem(direct)),mem(PSW)) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),ADDCD)==(ac: SFR_ADDR & ac = ACC & data: UCHAR | @any(acc,Pa,Cy,Ac,Ov).(acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & acc,Pa,Ov,Ac,Cy = add(byte_uchar(mem(ACC)),data,BIT_GET(CY,mem)) ==> (acc: UCHAR & Pa: BIT & Ov: BIT & Ac: BIT & Cy: BIT | mem:=mem<+{ACC|->uchar_byte(acc),PSW|->PSWUPDATE(mem(PSW),Pa,Ov,Ac,Cy)})) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),ADDCI)==(Rn: SFR_ADDR & ac = ACC & (Rn = R0 or Rn = R1) & byte_uchar(mem(Rn)): RAM_ADDR | @any(acc,Pa,Cy,Ac,Ov,src).(acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & src: RAM_ADDR & src = byte_uchar(mem(Rn)) & acc,Pa,Ov,Ac,Cy = add(byte_uchar(mem(ACC)),byte_uchar(mem(src)),BIT_GET(CY,mem)) ==> (acc: UCHAR & Pa: BIT & Ov: BIT & Ac: BIT & Cy: BIT | mem:=mem<+{ACC|->uchar_byte(acc),PSW|->PSWUPDATE(mem(PSW),Pa,Ov,Ac,Cy)})) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),ADDC)==(ac = ACC & src: MEM_ADDR | @any(acc,Pa,Cy,Ac,Ov).(acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & acc,Pa,Ov,Ac,Cy = add(byte_uchar(mem(ACC)),byte_uchar(mem(src)),BIT_GET(CY,mem)) ==> (acc: UCHAR & Pa: BIT & Ov: BIT & Ac: BIT & Cy: BIT | mem:=mem<+{ACC|->uchar_byte(acc),PSW|->PSWUPDATE(mem(PSW),Pa,Ov,Ac,Cy)} || pc:=PC_INCREMENT(pc,1))));
  Expanded_List_Substitution(Machine(A8051),ADDD)==(ac: SFR_ADDR & ac = ACC & data: UCHAR | @any(acc,Pa,Cy,Ac,Ov).(acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & acc,Pa,Ov,Ac,Cy = add(byte_uchar(mem(ACC)),data,0) ==> (acc: UCHAR & Pa: BIT & Ov: BIT & Ac: BIT & Cy: BIT | mem:=mem<+{ACC|->uchar_byte(acc),PSW|->PSWUPDATE(mem(PSW),Pa,Ov,Ac,Cy)})) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),ADDI)==(Rn: SFR_ADDR & ac = ACC & (Rn = R0 or Rn = R1) & byte_uchar(mem(Rn)): RAM_ADDR | @any(acc,Pa,Cy,Ac,Ov,src).(acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & src: RAM_ADDR & src = byte_uchar(mem(Rn)) & acc,Pa,Ov,Ac,Cy = add(byte_uchar(mem(ACC)),byte_uchar(mem(src)),0) ==> (acc: UCHAR & Pa: BIT & Ov: BIT & Ac: BIT & Cy: BIT | mem:=mem<+{ACC|->uchar_byte(acc),PSW|->PSWUPDATE(mem(PSW),Pa,Ov,Ac,Cy)})) || pc:=PC_INCREMENT(pc,1));
  Expanded_List_Substitution(Machine(A8051),ADD)==(ac = ACC & src: MEM_ADDR | @any(acc,Pa,Cy,Ac,Ov).(acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & acc,Pa,Ov,Ac,Cy = add(byte_uchar(mem(ACC)),byte_uchar(mem(src)),0) ==> (acc: UCHAR & Pa: BIT & Ov: BIT & Ac: BIT & Cy: BIT | mem:=mem<+{ACC|->uchar_byte(acc),PSW|->PSWUPDATE(mem(PSW),Pa,Ov,Ac,Cy)} || pc:=PC_INCREMENT(pc,1))));
  Expanded_List_Substitution(Machine(A8051),INIT)==(btrue | pc:=0);
  List_Substitution(Machine(A8051),INIT)==(pc:=0);
  List_Substitution(Machine(A8051),ADD)==(ANY acc,Pa,Cy,Ac,Ov WHERE acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & acc,Pa,Ov,Ac,Cy = add(byte_uchar(mem(ACC)),byte_uchar(mem(src)),0) THEN update(acc,Pa,Ov,Ac,Cy) || pc:=PC_INCREMENT(pc,1) END);
  List_Substitution(Machine(A8051),ADDI)==(ANY acc,Pa,Cy,Ac,Ov,src WHERE acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & src: RAM_ADDR & src = byte_uchar(mem(Rn)) & acc,Pa,Ov,Ac,Cy = add(byte_uchar(mem(ACC)),byte_uchar(mem(src)),0) THEN update(acc,Pa,Ov,Ac,Cy) END || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),ADDD)==(ANY acc,Pa,Cy,Ac,Ov WHERE acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & acc,Pa,Ov,Ac,Cy = add(byte_uchar(mem(ACC)),data,0) THEN update(acc,Pa,Ov,Ac,Cy) END || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),ADDC)==(ANY acc,Pa,Cy,Ac,Ov WHERE acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & acc,Pa,Ov,Ac,Cy = add(byte_uchar(mem(ACC)),byte_uchar(mem(src)),BIT_GET(CY,mem)) THEN update(acc,Pa,Ov,Ac,Cy) || pc:=PC_INCREMENT(pc,1) END);
  List_Substitution(Machine(A8051),ADDCI)==(ANY acc,Pa,Cy,Ac,Ov,src WHERE acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & src: RAM_ADDR & src = byte_uchar(mem(Rn)) & acc,Pa,Ov,Ac,Cy = add(byte_uchar(mem(ACC)),byte_uchar(mem(src)),BIT_GET(CY,mem)) THEN update(acc,Pa,Ov,Ac,Cy) END || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),ADDCD)==(ANY acc,Pa,Cy,Ac,Ov WHERE acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & acc,Pa,Ov,Ac,Cy = add(byte_uchar(mem(ACC)),data,BIT_GET(CY,mem)) THEN update(acc,Pa,Ov,Ac,Cy) END || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),ANL)==(addrSetDirect(ACC,and(mem(ACC),mem(direct))) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),ANLI)==(ANY addr WHERE addr: RAM_ADDR & addr = byte_uchar(mem(Rn)) THEN addrSetDirect(ACC,and(mem(ACC),mem(addr))) || pc:=PC_INCREMENT(pc,1) END);
  List_Substitution(Machine(A8051),ANLD)==(addrSetDirect(ACC,and(mem(ACC),uchar_byte(data))) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),ANLDA)==(addrSetDirect(dest,and(mem(dest),mem(ACC))) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),ANLDD)==(addrSetDirect(dest,and(mem(dest),uchar_byte(data))) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),ANLB)==(IF BIT_GET(bit,mem) = 0 THEN bitClear(CY) END || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),CJNE)==(IF not(byte_uchar(mem(ACC)) = byte_uchar(mem(direct))) THEN IF byte_uchar(mem(ACC))<byte_uchar(mem(direct)) THEN bitSet(CY) ELSE bitClear(CY) END || pc:=PC_INCREMENT(pc,jump+1) ELSE pc:=PC_INCREMENT(pc,1) END);
  List_Substitution(Machine(A8051),CJNED)==(IF not(byte_uchar(mem(dest)) = data) THEN IF byte_uchar(mem(dest))<data THEN bitSet(CY) ELSE bitClear(CY) END || pc:=PC_INCREMENT(pc,jump+1) ELSE pc:=PC_INCREMENT(pc,1) END);
  List_Substitution(Machine(A8051),CJNEI)==(ANY addr WHERE addr: RAM_ADDR & addr = byte_uchar(mem(Rn)) THEN IF not(byte_uchar(mem(addr)) = data) THEN IF byte_uchar(mem(addr))<data THEN bitSet(CY) ELSE bitClear(CY) END || pc:=PC_INCREMENT(pc,jump+1) ELSE pc:=PC_INCREMENT(pc,1) END END);
  List_Substitution(Machine(A8051),CLR)==(bitClear(bt) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),CLRA)==(addrSetDirect(ACC,uchar_byte(0)) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),CPL)==(IF BIT_GET(bit,mem) = 0 THEN bitSet(bit) ELSE bitClear(bit) END || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),CPLA)==(addrSetDirect(ACC,complement(mem(ACC))) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),DEC)==(ANY acc,Pa,Cy,Ac,Ov WHERE acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & acc,Pa,Ov,Ac,Cy = sub(byte_uchar(mem(src)),1,0) THEN addrSetDirect(src,uchar_byte(acc)) END || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),DECI)==(ANY acc,Pa,Cy,Ac,Ov,src WHERE acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & src: RAM_ADDR & src = byte_uchar(mem(Rn)) & acc,Pa,Ov,Ac,Cy = sub(byte_uchar(mem(src)),1,0) THEN addrSetDirect(src,uchar_byte(acc)) END || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),DJNZ)==(ANY result,Pa,Cy,Ac,Ov WHERE result: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & result,Pa,Ov,Ac,Cy = sub(byte_uchar(mem(direct)),1,0) THEN addrSetDirect(direct,uchar_byte(result)) || IF not(result = 0) THEN pc:=PC_INCREMENT(pc,jump+1) ELSE pc:=PC_INCREMENT(pc,1) END END);
  List_Substitution(Machine(A8051),INC)==(ANY acc,Pa,Cy,Ac,Ov WHERE acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & acc,Pa,Ov,Ac,Cy = add(byte_uchar(mem(src)),1,0) THEN addrSetDirect(src,uchar_byte(acc)) END || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),INCI)==(ANY acc,Pa,Cy,Ac,Ov,src WHERE acc: UCHAR & Pa: BIT & Cy: BIT & Ac: BIT & Ov: BIT & src: RAM_ADDR & src = byte_uchar(mem(Rn)) & acc,Pa,Ov,Ac,Cy = add(byte_uchar(mem(src)),1,0) THEN addrSetDirect(src,uchar_byte(acc)) END || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),JB)==(IF BIT_GET(bit,mem) = 1 THEN pc:=PC_INCREMENT(pc,jump+1) ELSE pc:=PC_INCREMENT(pc,1) END);
  List_Substitution(Machine(A8051),JBC)==(IF BIT_GET(bit,mem) = 1 THEN bitClear(bit) || pc:=PC_INCREMENT(pc,jump+1) ELSE pc:=PC_INCREMENT(pc,jump+1) END);
  List_Substitution(Machine(A8051),JC)==(IF BIT_GET(CY,mem) = 1 THEN pc:=PC_INCREMENT(pc,1+jump) ELSE pc:=PC_INCREMENT(pc,1) END);
  List_Substitution(Machine(A8051),JNB)==(IF BIT_GET(bit,mem) = 0 THEN pc:=PC_INCREMENT(pc,jump+1) ELSE pc:=PC_INCREMENT(pc,1) END);
  List_Substitution(Machine(A8051),JNC)==(IF BIT_GET(bit,mem) = 0 THEN bitClear(bit) || pc:=PC_INCREMENT(pc,jump+1) ELSE pc:=PC_INCREMENT(pc,1) END);
  List_Substitution(Machine(A8051),JNZ)==(IF byte_uchar(mem(ACC))/=0 THEN pc:=PC_INCREMENT(pc,jump+1) ELSE pc:=PC_INCREMENT(pc,1) END);
  List_Substitution(Machine(A8051),JZ)==(IF byte_uchar(mem(ACC)) = 1 THEN pc:=PC_INCREMENT(pc,jump+1) ELSE pc:=PC_INCREMENT(pc,1) END);
  List_Substitution(Machine(A8051),LCALL)==(ANY upper_pc,lower_pc WHERE upper_pc,lower_pc: BYTE*BYTE & upper_pc,lower_pc = usint_byte(PC_INCREMENT(pc,1)) THEN push_two(lower_pc,upper_pc) || pc:=rom_addr END);
  List_Substitution(Machine(A8051),LJMP)==(pc:=rom_addr);
  List_Substitution(Machine(A8051),MOV)==(addrSetDirect(direct_d,mem(direct_s)) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),MOVD)==(addrSetDirect(direct,uchar_byte(data)) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),MOVDI)==(ANY src WHERE src: RAM_ADDR & src = byte_uchar(mem(Rn)) THEN addrSetDirect(direct,mem(src)) || pc:=PC_INCREMENT(pc,1) END);
  List_Substitution(Machine(A8051),MOVI)==(addrSetIndirect(Rn,mem(direct)) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),MOVID)==(addrSetIndirect(Rn,uchar_byte(data)) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),MOVB)==(IF BIT_GET(src,mem) = 1 THEN bitSet(src) ELSE bitClear(src) END || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),NOP)==(pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),ORL)==(addrSetDirect(dest,ior(mem(dest),mem(src))) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),ORLD)==(addrSetDirect(dest,ior(mem(dest),uchar_byte(data))) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),ORLI)==(ANY src WHERE src: RAM_ADDR & src = byte_uchar(mem(Rn)) THEN addrSetDirect(dest,ior(mem(dest),mem(src))) || pc:=PC_INCREMENT(pc,1) END);
  List_Substitution(Machine(A8051),ORLB)==(IF BIT_GET(src,mem) = 1 THEN bitSet(src) END || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),POP)==(pop(direct));
  List_Substitution(Machine(A8051),PUSH)==(push(mem(direct)) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),RL)==(addrSetDirect(ACC,rotateleft(mem(ACC))) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),RR)==(addrSetDirect(ACC,rotateright(mem(ACC))) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),SETB)==(bitSet(bit) || pc:=PC_INCREMENT(pc,1));
  List_Substitution(Machine(A8051),SJMP)==(pc:=PC_INCREMENT(pc,jump+1))
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(A8051))==(ROM_LENGTH,ROM_ADDR,ROM_ADDR_MAX,MEM_ADDR,RAM_ADDR,RAM_MIN,RAM_MAX,RAM_BIT_ADDRESSABLE,SFR_ADDR,SFR_MIN,SFR_MAX,SFR_BIT_ADDRESSABLE,BIT_ADDRESS,BIT_ADDRESSABLE,STACK_MAX,PSWUPDATE,BIT_GET,SBUF,PCON,SP,SCON,B98,B99,B9A,B9B,B9C,B9D,B9E,B9F,SCON_0,SCON_1,SCON_2,SCON_3,SCON_4,SCON_5,SCON_6,SCON_7,RI,TI,RB8,TB8,REN,SM2,SM1,SM0,R0,R1,R2,R3,R4,R5,R6,R7,AA,ACC,BE0,BE1,BE2,BE3,BE4,BE5,BE6,BE7,BB,BF0,BF1,BF2,BF3,BF4,BF5,BF6,BF7,P0,P0_0,P0_1,P0_2,P0_3,P0_4,P0_5,P0_6,P0_7,B80,B81,B82,B83,B84,B85,B86,B87,P1,P1_0,P1_1,P1_2,P1_3,P1_4,P1_5,P1_6,P1_7,B90,B91,B92,B93,B94,B95,B96,B97,P2,P2_0,P2_1,P2_2,P2_3,P2_4,P2_5,P2_6,P2_7,BA0,BA1,BA2,BA3,BA4,BA5,BA6,BA7,P3,P3_0,P3_1,P3_2,P3_3,P3_4,P3_5,P3_6,P3_7,BB0,BB1,BB2,BB3,BB4,BB5,BB6,BB7,PSW,PP,OV,RS0,RS1,F0,AC,CY,CC,BD0,BD1,BD2,BD3,BD4,BD5,BD6,BD7,PSW_0,PSW_1,PSW_2,PSW_3,PSW_4,PSW_5,PSW_6,PSW_7,ADDRCHANGE,BITCHANGE,PC_INCREMENT,SP_INCREMENT,SP_DECREMENT);
  Inherited_List_Constants(Machine(A8051))==(ROM_LENGTH,ROM_ADDR,ROM_ADDR_MAX,MEM_ADDR,RAM_ADDR,RAM_MIN,RAM_MAX,RAM_BIT_ADDRESSABLE,SFR_ADDR,SFR_MIN,SFR_MAX,SFR_BIT_ADDRESSABLE,BIT_ADDRESS,BIT_ADDRESSABLE,STACK_MAX,PSWUPDATE,BIT_GET,SBUF,PCON,SP,SCON,B98,B99,B9A,B9B,B9C,B9D,B9E,B9F,SCON_0,SCON_1,SCON_2,SCON_3,SCON_4,SCON_5,SCON_6,SCON_7,RI,TI,RB8,TB8,REN,SM2,SM1,SM0,R0,R1,R2,R3,R4,R5,R6,R7,AA,ACC,BE0,BE1,BE2,BE3,BE4,BE5,BE6,BE7,BB,BF0,BF1,BF2,BF3,BF4,BF5,BF6,BF7,P0,P0_0,P0_1,P0_2,P0_3,P0_4,P0_5,P0_6,P0_7,B80,B81,B82,B83,B84,B85,B86,B87,P1,P1_0,P1_1,P1_2,P1_3,P1_4,P1_5,P1_6,P1_7,B90,B91,B92,B93,B94,B95,B96,B97,P2,P2_0,P2_1,P2_2,P2_3,P2_4,P2_5,P2_6,P2_7,BA0,BA1,BA2,BA3,BA4,BA5,BA6,BA7,P3,P3_0,P3_1,P3_2,P3_3,P3_4,P3_5,P3_6,P3_7,BB0,BB1,BB2,BB3,BB4,BB5,BB6,BB7,PSW,PP,OV,RS0,RS1,F0,AC,CY,CC,BD0,BD1,BD2,BD3,BD4,BD5,BD6,BD7,PSW_0,PSW_1,PSW_2,PSW_3,PSW_4,PSW_5,PSW_6,PSW_7,ADDRCHANGE,BITCHANGE,PC_INCREMENT,SP_INCREMENT,SP_DECREMENT);
  List_Constants(Machine(A8051))==(?)
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
  Context_List_Properties(Machine(A8051))==(parity: BYTE --> BIT & parity = %bv.(bv: BYTE | bv_par(bv)) & half: UCHAR --> UCHAR & half = %v1.(v1: UCHAR | v1 mod 2**4) & add: UCHAR*UCHAR*BIT --> UCHAR*BIT*BIT*BIT*BIT & add = %(v1,v2,ca).(v1: UCHAR & v2: UCHAR & ca: UCHAR | (v1+v2+ca) mod (UCHAR_MAX+1),parity(uchar_byte((v1+v2+ca) mod (UCHAR_MAX+1))),bool_to_bit(bool(uchar_schar(v1)+uchar_schar(v2)+ca/:SCHAR)),bool_to_bit(bool(half(v1)+half(v2)+ca>=2**4)),bool_to_bit(bool(v1+v2+ca/:UCHAR))) & sub: UCHAR*UCHAR*UCHAR --> UCHAR*BIT*BIT*BIT*BIT & sub = %(v1,v2,ca).(v1: UCHAR & v2: UCHAR & ca: UCHAR | bool_to_bit(bool(v1<v2+ca))*256+v1-v2-ca,parity(uchar_byte(bool_to_bit(bool(v1<v2+ca))*256+v1-v2-ca)),bool_to_bit(bool(uchar_schar(v1)-uchar_schar(v2)-ca/:SCHAR)),bool_to_bit(bool(half(v1)<half(v2)+ca)),bool_to_bit(bool(v1-v2-ca/:UCHAR))) & and: BYTE*BYTE --> BYTE & and = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_and(bt1,bt2)) & and_bit: BIT*BIT --> BIT & and_bit = %(b1,b2).(b1: BIT & b2: BIT | bit_and(b1,b2)) & ior: BYTE*BYTE --> BYTE & ior = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_or(bt1,bt2)) & xor: BYTE*BYTE --> BYTE & xor = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_xor(bt1,bt2)) & complement: BYTE --> BYTE & complement = %bt.(bt: BYTE | bv_not(bt)) & swap: BYTE --> BYTE & swap = %bt.(bt: BYTE | {0|->bt(4),1|->bt(5),2|->bt(6),3|->bt(7),4|->bt(0),5|->bt(1),6|->bt(2),7|->bt(3)}) & rotateleft: BYTE --> BYTE & rotateleft = %bv.(bv: BYTE | {0|->bv(7),1|->bv(0),2|->bv(1),3|->bv(2),4|->bv(3),5|->bv(4),6|->bv(5),7|->bv(6)}) & rotateright: BYTE --> BYTE & rotateright = %bv.(bv: BYTE | {0|->bv(1),1|->bv(2),2|->bv(3),3|->bv(4),4|->bv(5),5|->bv(6),6|->bv(7),7|->bv(0)}) & usint_byte: USHORT_INT --> BYTE*BYTE & usint_byte = %usint.(usint: USHORT_INT | bv16_byte(usint_bv16(usint))) & UCHAR_MAX: NATURAL & UCHAR_MAX = 2**BYTE_WIDTH-1 & UCHAR = 0..UCHAR_MAX & SCHAR_MAX: INTEGER & SCHAR_MIN: INTEGER & SCHAR_MAX = 2**(BYTE_WIDTH-1)-1 & SCHAR_MIN = -(2**(BYTE_WIDTH-1)) & SCHAR = SCHAR_MIN..SCHAR_MAX & USHORT_INT_MAX: INTEGER & USHORT_INT_MAX = 2**(2*BYTE_WIDTH)-1 & USHORT_INT = 0..USHORT_INT_MAX & byte_uchar: BYTE --> UCHAR & byte_uchar = %bv.(bv: BYTE | bv_to_nat(bv)) & uchar_byte = byte_uchar~ & byte_schar: BYTE --> SCHAR & byte_schar = %bv.(bv: BYTE | (-bv(7))*128+bv(6)*64+bv(5)*32+bv(4)*16+bv(3)*8+bv(2)*4+bv(1)*2+bv(0)*1) & schar_byte = byte_schar~ & uchar_schar: UCHAR --> SCHAR & uchar_schar = %v1.(v1: UCHAR & v1<=SCHAR_MAX | v1) & uchar_schar = %v1.(v1: UCHAR & not(v1<=SCHAR_MAX) | v1-UCHAR_MAX) & schar_uchar = uchar_schar~ & bv16_usint: BV16 --> USHORT_INT & bv16_usint = %v0.(v0: BV16 | 32768*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & usint_bv16 = bv16_usint~ & BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE = {bt | bt: BYTE_INDEX --> BIT & size(bt) = BYTE_WIDTH}-{{}} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0});
  Inherited_List_Properties(Machine(A8051))==(ROM_LENGTH = 4 & ROM_ADDR_MAX = ROM_LENGTH*2**10 & ROM_ADDR = 0..ROM_ADDR_MAX & MEM_ADDR = UCHAR & RAM_MIN: UCHAR & RAM_MAX: UCHAR & RAM_MIN = 0 & RAM_MAX = 127 & RAM_ADDR = RAM_MIN..RAM_MAX & RAM_BIT_ADDRESSABLE = 32..47 & R0: RAM_ADDR & R1: RAM_ADDR & R2: RAM_ADDR & R3: RAM_ADDR & R4: RAM_ADDR & R5: RAM_ADDR & R6: RAM_ADDR & R7: RAM_ADDR & R0 = 0 & R1 = 1 & R2 = 2 & R3 = 3 & R4 = 4 & R5 = 5 & R6 = 6 & R7 = 7 & SFR_MIN: UCHAR & SFR_MAX: UCHAR & SFR_MIN = RAM_MAX+1 & SFR_MAX = UCHAR_MAX & SFR_ADDR = SFR_MIN..SFR_MAX & SFR_BIT_ADDRESSABLE = {ba | ba: SFR_ADDR & ba>=128 & ba<=240 & ba mod 8 = 0} & BIT_ADDRESS = MEM_ADDR*BYTE_INDEX & BIT_ADDRESSABLE = {addr,ind | addr: MEM_ADDR & ind: BYTE_INDEX & addr: SFR_BIT_ADDRESSABLE\/RAM_BIT_ADDRESSABLE} & SBUF: SFR_ADDR & SP: SFR_ADDR & PCON: SFR_ADDR & SBUF = 153 & SP = 109 & PCON = 135 & BB: SFR_ADDR & BB = 240 & BF0: BIT_ADDRESS & BF1: BIT_ADDRESS & BF2: BIT_ADDRESS & BF3: BIT_ADDRESS & BF4: BIT_ADDRESS & BF5: BIT_ADDRESS & BF6: BIT_ADDRESS & BF7: BIT_ADDRESS & BF0 = BB,0 & BF1 = BB,1 & BF2 = BB,2 & BF3 = BB,3 & BF4 = BB,4 & BF5 = BB,5 & BF6 = BB,6 & BF7 = BB,7 & AA: SFR_ADDR & AA = 224 & ACC = AA & BE0: BIT_ADDRESS & BE1: BIT_ADDRESS & BE2: BIT_ADDRESS & BE3: BIT_ADDRESS & BE4: BIT_ADDRESS & BE5: BIT_ADDRESS & BE6: BIT_ADDRESS & BE7: BIT_ADDRESS & BE0 = ACC,0 & BE1 = ACC,1 & BE2 = ACC,2 & BE3 = ACC,3 & BE4 = ACC,4 & BE5 = ACC,5 & BE6 = ACC,6 & BE7 = ACC,7 & P0: SFR_ADDR & P0 = 128 & P0_0: BIT_ADDRESS & P0_1: BIT_ADDRESS & P0_2: BIT_ADDRESS & P0_3: BIT_ADDRESS & P0_4: BIT_ADDRESS & P0_5: BIT_ADDRESS & P0_6: BIT_ADDRESS & P0_7: BIT_ADDRESS & B80: BIT_ADDRESS & B81: BIT_ADDRESS & B82: BIT_ADDRESS & B83: BIT_ADDRESS & B84: BIT_ADDRESS & B85: BIT_ADDRESS & B86: BIT_ADDRESS & B87: BIT_ADDRESS & P0_0 = P0,0 & P0_1 = P0,1 & P0_2 = P0,2 & P0_3 = P0,3 & P0_4 = P0,4 & P0_5 = P0,5 & P0_6 = P0,6 & P0_7 = P0,7 & B80 = P0,0 & B81 = P0,1 & B82 = P0,2 & B83 = P0,3 & B84 = P0,4 & B85 = P0,5 & B86 = P0,6 & B87 = P0,7 & P1: SFR_ADDR & P1 = 144 & P1_0: BIT_ADDRESS & P1_1: BIT_ADDRESS & P1_2: BIT_ADDRESS & P1_3: BIT_ADDRESS & P1_4: BIT_ADDRESS & P1_5: BIT_ADDRESS & P1_6: BIT_ADDRESS & P1_7: BIT_ADDRESS & B90: BIT_ADDRESS & B91: BIT_ADDRESS & B92: BIT_ADDRESS & B93: BIT_ADDRESS & B94: BIT_ADDRESS & B95: BIT_ADDRESS & B96: BIT_ADDRESS & B97: BIT_ADDRESS & P1_0 = P1,0 & P1_1 = P1,1 & P1_2 = P1,2 & P1_3 = P1,3 & P1_4 = P1,4 & P1_5 = P1,5 & P1_6 = P1,6 & P1_7 = P1,7 & B90 = P1,0 & B91 = P1,1 & B92 = P1,2 & B93 = P1,3 & B94 = P1,4 & B95 = P1,5 & B96 = P1,6 & B97 = P1,7 & P2: SFR_ADDR & P2 = 160 & P2_0: BIT_ADDRESS & P2_1: BIT_ADDRESS & P2_2: BIT_ADDRESS & P2_3: BIT_ADDRESS & P2_4: BIT_ADDRESS & P2_5: BIT_ADDRESS & P2_6: BIT_ADDRESS & P2_7: BIT_ADDRESS & BA0: BIT_ADDRESS & BA1: BIT_ADDRESS & BA2: BIT_ADDRESS & BA3: BIT_ADDRESS & BA4: BIT_ADDRESS & BA5: BIT_ADDRESS & BA6: BIT_ADDRESS & BA7: BIT_ADDRESS & P2_0 = P2,0 & P2_1 = P2,1 & P2_2 = P2,2 & P2_3 = P2,3 & P2_4 = P2,4 & P2_5 = P2,5 & P2_6 = P2,6 & P2_7 = P2,7 & BA0 = P2,0 & BA1 = P2,1 & BA2 = P2,2 & BA3 = P2,3 & BA4 = P2,4 & BA5 = P2,5 & BA6 = P2,6 & BA7 = P2,7 & P3: SFR_ADDR & P3 = 176 & P3_0: BIT_ADDRESS & P3_1: BIT_ADDRESS & P3_2: BIT_ADDRESS & P3_3: BIT_ADDRESS & P3_4: BIT_ADDRESS & P3_5: BIT_ADDRESS & P3_6: BIT_ADDRESS & P3_7: BIT_ADDRESS & BB0: BIT_ADDRESS & BB1: BIT_ADDRESS & BB2: BIT_ADDRESS & BB3: BIT_ADDRESS & BB4: BIT_ADDRESS & BB5: BIT_ADDRESS & BB6: BIT_ADDRESS & BB7: BIT_ADDRESS & P3_0 = P3,0 & P3_1 = P3,1 & P3_2 = P3,2 & P3_3 = P3,3 & P3_4 = P3,4 & P3_5 = P3,5 & P3_6 = P3,6 & P3_7 = P3,7 & BB0 = P3,0 & BB1 = P3,1 & BB2 = P3,2 & BB3 = P3,3 & BB4 = P3,4 & BB5 = P3,5 & BB6 = P3,6 & BB7 = P3,7 & SCON: SFR_ADDR & SCON = 152 & B98: BIT_ADDRESS & B99: BIT_ADDRESS & B9A: BIT_ADDRESS & B9B: BIT_ADDRESS & B9C: BIT_ADDRESS & B9D: BIT_ADDRESS & B9E: BIT_ADDRESS & B9F: BIT_ADDRESS & B98 = SCON,0 & B99 = SCON,1 & B9A = SCON,2 & B9B = SCON,3 & B9C = SCON,4 & B9D = SCON,5 & B9E = SCON,6 & B9F = SCON,7 & SCON_0: BIT_ADDRESS & SCON_1: BIT_ADDRESS & SCON_2: BIT_ADDRESS & SCON_3: BIT_ADDRESS & SCON_4: BIT_ADDRESS & SCON_5: BIT_ADDRESS & SCON_6: BIT_ADDRESS & SCON_7: BIT_ADDRESS & SCON_0 = SCON,0 & SCON_1 = SCON,1 & SCON_2 = SCON,2 & SCON_3 = SCON,3 & SCON_4 = SCON,4 & SCON_5 = SCON,5 & SCON_6 = SCON,6 & SCON_7 = SCON,7 & RI: BIT_ADDRESS & TI: BIT_ADDRESS & RB8: BIT_ADDRESS & TB8: BIT_ADDRESS & REN: BIT_ADDRESS & SM2: BIT_ADDRESS & SM1: BIT_ADDRESS & SM0: BIT_ADDRESS & RI = SCON,0 & TI = SCON,1 & RB8 = SCON,2 & TB8 = SCON,3 & REN = SCON,4 & SM2 = SCON,5 & SM1 = SCON,6 & SM0 = SCON,7 & PSW: SFR_ADDR & PSW = 208 & PP: BIT_ADDRESS & OV: BIT_ADDRESS & RS0: BIT_ADDRESS & RS1: BIT_ADDRESS & F0: BIT_ADDRESS & AC: BIT_ADDRESS & CY: BIT_ADDRESS & CC: BIT_ADDRESS & BD0: BIT_ADDRESS & BD1: BIT_ADDRESS & BD2: BIT_ADDRESS & BD3: BIT_ADDRESS & BD4: BIT_ADDRESS & BD5: BIT_ADDRESS & BD6: BIT_ADDRESS & BD7: BIT_ADDRESS & BD0 = PSW,0 & BD1 = PSW,1 & BD2 = PSW,2 & BD3 = PSW,3 & BD4 = PSW,4 & BD5 = PSW,5 & BD6 = PSW,6 & BD7 = PSW,7 & PSW_0: BIT_ADDRESS & PSW_1: BIT_ADDRESS & PSW_2: BIT_ADDRESS & PSW_3: BIT_ADDRESS & PSW_4: BIT_ADDRESS & PSW_5: BIT_ADDRESS & PSW_6: BIT_ADDRESS & PSW_7: BIT_ADDRESS & PSW_0 = PSW,0 & PSW_1 = PSW,1 & PSW_2 = PSW,2 & PSW_3 = PSW,3 & PSW_4 = PSW,4 & PSW_5 = PSW,5 & PSW_6 = PSW,6 & PSW_7 = PSW,7 & PP = PSW,0 & OV = PSW,2 & RS0 = PSW,3 & RS1 = PSW,4 & F0 = PSW,5 & AC = PSW,6 & CY = PSW,7 & CC = PSW,7 & STACK_MAX: RAM_ADDR & STACK_MAX = RAM_MAX & PC_INCREMENT: ROM_ADDR*ROM_ADDR --> ROM_ADDR & PC_INCREMENT = %(ii,jj).(ii: ROM_ADDR & jj: ROM_ADDR | (ii+jj) mod (ROM_ADDR_MAX+1)) & ADDRCHANGE: MEM_ADDR*BYTE*BYTE --> (MEM_ADDR +-> BYTE) & ADDRCHANGE = %(addr,data,psw).(addr: MEM_ADDR & data: BYTE & psw: BYTE & addr = ACC | {addr|->data,PSW|->bv_put(psw,0,bv_par(data))}) & ADDRCHANGE = %(addr,data,psw).(addr: MEM_ADDR & data: BYTE & psw: BYTE & addr/=ACC & addr/=PSW | {addr|->data}) & BITCHANGE: MEM_ADDR*BYTE*BYTE_INDEX*BIT*BYTE --> (MEM_ADDR +-> BYTE) & BITCHANGE = %(addr,data,ind,bit,psw).(addr: MEM_ADDR & data: BYTE & ind: BYTE_INDEX & bit: BIT & psw: BYTE & addr = ACC | ADDRCHANGE(addr,bv_put(data,ind,bit),psw)) & BITCHANGE = %(addr,data,ind,bit,psw).(addr: MEM_ADDR & data: BYTE & ind: BYTE_INDEX & bit: BIT & psw: BYTE & addr/=ACC | {addr|->bv_put(data,ind,bit)}) & SP_INCREMENT: BYTE*ROM_ADDR --> RAM_ADDR & SP_INCREMENT = %(sp,inc).(sp: BYTE & inc: RAM_ADDR | (byte_uchar(sp)+inc) mod STACK_MAX) & SP_DECREMENT: BYTE*ROM_ADDR --> RAM_ADDR & SP_DECREMENT = %(sp,dec).(sp: BYTE & dec: RAM_ADDR & byte_uchar(sp)-dec>0 | byte_uchar(sp)-dec) & SP_DECREMENT = %(sp,dec).(sp: BYTE & dec: RAM_ADDR & not(byte_uchar(sp)-dec>0) | STACK_MAX-(byte_uchar(sp)-dec)) & PSWUPDATE: BYTE*BIT*BIT*BIT*BIT --> BYTE & PSWUPDATE = %(psw,Pa,Ov,Ac,Cy).(psw: BYTE & Pa: BIT & Ov: BIT & Ac: BIT & Cy: BIT | bv_put(bv_put(bv_put(bv_put(psw,0,Pa),2,Ov),6,Ac),7,Cy)) & BIT_GET: BIT_ADDRESS*(MEM_ADDR +-> BYTE) --> BIT & !(bit,memory,addr,ind).(bit: BIT_ADDRESS & memory: MEM_ADDR +-> BYTE & addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = bit => BIT_GET(bit,memory) = memory(addr)(ind)));
  List_Properties(Machine(A8051))==(btrue)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(A8051),Machine(BIT_DEFINITION))==(?);
  Seen_Context_List_Enumerated(Machine(A8051))==(?);
  Seen_Context_List_Invariant(Machine(A8051))==(btrue);
  Seen_Context_List_Assertions(Machine(A8051))==(dom(byte_schar) = BYTE & ran(byte_schar) <: SCHAR & dom(schar_byte) = SCHAR & ran(schar_byte) <: BYTE & dom(byte_uchar) = BYTE & ran(byte_uchar) <: UCHAR & dom(uchar_byte) = UCHAR & ran(uchar_byte) <: BYTE & dom(uchar_schar) = UCHAR & ran(uchar_schar) <: SCHAR & dom(schar_uchar) = SCHAR & ran(schar_uchar) <: UCHAR & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_to_bit(TRUE) = 1 & bool_to_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_not(bv_not(bv))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_and(v1,v2)(indx) = bv_and(v2,v1)(indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_and(v1,bv_and(v2,v3))(indx) = bv_and(bv_and(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_zero(bv_size(bv)))(indx) = bv_zero(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_one(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_or(v1,v2)(indx) = bv_or(v2,v1)(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_or(v1,bv_or(v2,v3))(indx) = bv_or(bv_or(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_one(bv_size(bv)))(indx) = bv_one(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_zero(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_xor(v1,v2)(indx) = bv_xor(v2,v1)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_xor(bv,bv)(indx) = bv_zero(bv_size(bv))(indx))) & !bt.(bt: BYTE => size(bt) = 8) & size(BYTE_ZERO) = 8 & BYTE <: BIT_VECTOR & BYTE_ZERO: BIT_VECTOR & first(BYTE_ZERO) = 0 & !ss.(ss: NATURAL1 => bv_to_nat(bv_zero(ss)) = 0) & !ss.(ss: NATURAL1 => bv_par(bv_zero(ss)) = 0) & BV16_ZERO: BV16 & BV16 <: BIT_VECTOR & BV16_ZERO: BIT_VECTOR & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536));
  Seen_Context_List_Properties(Machine(A8051))==(UCHAR_MAX: NATURAL & UCHAR_MAX = 2**BYTE_WIDTH-1 & UCHAR = 0..UCHAR_MAX & SCHAR_MAX: INTEGER & SCHAR_MIN: INTEGER & SCHAR_MAX = 2**(BYTE_WIDTH-1)-1 & SCHAR_MIN = -(2**(BYTE_WIDTH-1)) & SCHAR = SCHAR_MIN..SCHAR_MAX & USHORT_INT_MAX: INTEGER & USHORT_INT_MAX = 2**(2*BYTE_WIDTH)-1 & USHORT_INT = 0..USHORT_INT_MAX & byte_uchar: BYTE --> UCHAR & byte_uchar = %bv.(bv: BYTE | bv_to_nat(bv)) & uchar_byte = byte_uchar~ & byte_schar: BYTE --> SCHAR & byte_schar = %bv.(bv: BYTE | (-bv(7))*128+bv(6)*64+bv(5)*32+bv(4)*16+bv(3)*8+bv(2)*4+bv(1)*2+bv(0)*1) & schar_byte = byte_schar~ & uchar_schar: UCHAR --> SCHAR & uchar_schar = %v1.(v1: UCHAR & v1<=SCHAR_MAX | v1) & uchar_schar = %v1.(v1: UCHAR & not(v1<=SCHAR_MAX) | v1-UCHAR_MAX) & schar_uchar = uchar_schar~ & bv16_usint: BV16 --> USHORT_INT & bv16_usint = %v0.(v0: BV16 | 32768*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & usint_bv16 = bv16_usint~ & BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = {bt | bt: NATURAL +-> BIT}-{{}} & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & BV_INDX: BIT_VECTOR --> POW(NATURAL) & BV_INDX = %bv.(bv: BIT_VECTOR | 0..bv_size(bv)-1) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high | low..high<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (0..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (0..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: BV_INDX(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1(idx)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: BV_INDX(v1) & bit: BIT | v1<+{idx|->bit}) & BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE = {bt | bt: BYTE_INDEX --> BIT & size(bt) = BYTE_WIDTH}-{{}} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & bv_to_nat: BIT_VECTOR --> NATURAL & bv_to_nat = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | 2**idx*bv(idx))) & bv_par: BIT_VECTOR --> BIT & bv_par = %bv.(bv: BIT_VECTOR | size(bv|>{1}) mod 2) & BV16_WIDTH: NATURAL & BV16_WIDTH = 16 & BV16_INDX = 0..BV16_WIDTH-1 & BV16 = {bt | bt: BV16_INDX --> BIT & size(bt) = 16}-{{}} & BV16_ZERO = BV16_INDX*{0} & bv16_byte: BV16 --> BYTE*BYTE & bv16_byte = %bv.(bv: BV16 | {0|->bv(8),1|->bv(9),2|->bv(10),3|->bv(11),4|->bv(12),5|->bv(13),6|->bv(14),7|->bv(15)},{0|->bv(0),1|->bv(1),2|->bv(2),3|->bv(3),4|->bv(4),5|->bv(5),6|->bv(6),7|->bv(7)}));
  Seen_List_Constraints(Machine(A8051))==(btrue);
  Seen_List_Operations(Machine(A8051),Machine(BIT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(A8051),Machine(BIT_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(A8051),Machine(BYTE_DEFINITION))==(?);
  Seen_List_Operations(Machine(A8051),Machine(BYTE_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(A8051),Machine(BYTE_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(A8051),Machine(TYPES))==(?);
  Seen_List_Operations(Machine(A8051),Machine(TYPES))==(?);
  Seen_Expanded_List_Invariant(Machine(A8051),Machine(TYPES))==(btrue);
  Seen_Internal_List_Operations(Machine(A8051),Machine(ALU))==(?);
  Seen_List_Operations(Machine(A8051),Machine(ALU))==(?);
  Seen_Expanded_List_Invariant(Machine(A8051),Machine(ALU))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(A8051),INIT)==(?);
  List_ANY_Var(Machine(A8051),ADD)==((Var(acc) == btype(INTEGER,?,?)),(Var(Pa) == btype(INTEGER,?,?)),(Var(Cy) == btype(INTEGER,?,?)),(Var(Ac) == btype(INTEGER,?,?)),(Var(Ov) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(A8051),ADDI)==((Var(acc) == btype(INTEGER,?,?)),(Var(Pa) == btype(INTEGER,?,?)),(Var(Cy) == btype(INTEGER,?,?)),(Var(Ac) == btype(INTEGER,?,?)),(Var(Ov) == btype(INTEGER,?,?)),(Var(src) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(A8051),ADDD)==((Var(acc) == btype(INTEGER,?,?)),(Var(Pa) == btype(INTEGER,?,?)),(Var(Cy) == btype(INTEGER,?,?)),(Var(Ac) == btype(INTEGER,?,?)),(Var(Ov) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(A8051),ADDC)==((Var(acc) == btype(INTEGER,?,?)),(Var(Pa) == btype(INTEGER,?,?)),(Var(Cy) == btype(INTEGER,?,?)),(Var(Ac) == btype(INTEGER,?,?)),(Var(Ov) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(A8051),ADDCI)==((Var(acc) == btype(INTEGER,?,?)),(Var(Pa) == btype(INTEGER,?,?)),(Var(Cy) == btype(INTEGER,?,?)),(Var(Ac) == btype(INTEGER,?,?)),(Var(Ov) == btype(INTEGER,?,?)),(Var(src) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(A8051),ADDCD)==((Var(acc) == btype(INTEGER,?,?)),(Var(Pa) == btype(INTEGER,?,?)),(Var(Cy) == btype(INTEGER,?,?)),(Var(Ac) == btype(INTEGER,?,?)),(Var(Ov) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(A8051),ANL)==(?);
  List_ANY_Var(Machine(A8051),ANLI)==(Var(addr) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(A8051),ANLD)==(?);
  List_ANY_Var(Machine(A8051),ANLDA)==(?);
  List_ANY_Var(Machine(A8051),ANLDD)==(?);
  List_ANY_Var(Machine(A8051),ANLB)==(?);
  List_ANY_Var(Machine(A8051),CJNE)==(?);
  List_ANY_Var(Machine(A8051),CJNED)==(?);
  List_ANY_Var(Machine(A8051),CJNEI)==(Var(addr) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(A8051),CLR)==(?);
  List_ANY_Var(Machine(A8051),CLRA)==(?);
  List_ANY_Var(Machine(A8051),CPL)==(?);
  List_ANY_Var(Machine(A8051),CPLA)==(?);
  List_ANY_Var(Machine(A8051),DEC)==((Var(acc) == btype(INTEGER,?,?)),(Var(Pa) == btype(INTEGER,?,?)),(Var(Cy) == btype(INTEGER,?,?)),(Var(Ac) == btype(INTEGER,?,?)),(Var(Ov) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(A8051),DECI)==((Var(acc) == btype(INTEGER,?,?)),(Var(Pa) == btype(INTEGER,?,?)),(Var(Cy) == btype(INTEGER,?,?)),(Var(Ac) == btype(INTEGER,?,?)),(Var(Ov) == btype(INTEGER,?,?)),(Var(src) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(A8051),DJNZ)==((Var(result) == btype(INTEGER,?,?)),(Var(Pa) == btype(INTEGER,?,?)),(Var(Cy) == btype(INTEGER,?,?)),(Var(Ac) == btype(INTEGER,?,?)),(Var(Ov) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(A8051),INC)==((Var(acc) == btype(INTEGER,?,?)),(Var(Pa) == btype(INTEGER,?,?)),(Var(Cy) == btype(INTEGER,?,?)),(Var(Ac) == btype(INTEGER,?,?)),(Var(Ov) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(A8051),INCI)==((Var(acc) == btype(INTEGER,?,?)),(Var(Pa) == btype(INTEGER,?,?)),(Var(Cy) == btype(INTEGER,?,?)),(Var(Ac) == btype(INTEGER,?,?)),(Var(Ov) == btype(INTEGER,?,?)),(Var(src) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(A8051),JB)==(?);
  List_ANY_Var(Machine(A8051),JBC)==(?);
  List_ANY_Var(Machine(A8051),JC)==(?);
  List_ANY_Var(Machine(A8051),JNB)==(?);
  List_ANY_Var(Machine(A8051),JNC)==(?);
  List_ANY_Var(Machine(A8051),JNZ)==(?);
  List_ANY_Var(Machine(A8051),JZ)==(?);
  List_ANY_Var(Machine(A8051),LCALL)==((Var(upper_pc) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lower_pc) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(A8051),LJMP)==(?);
  List_ANY_Var(Machine(A8051),MOV)==(?);
  List_ANY_Var(Machine(A8051),MOVD)==(?);
  List_ANY_Var(Machine(A8051),MOVDI)==(Var(src) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(A8051),MOVI)==(?);
  List_ANY_Var(Machine(A8051),MOVID)==(?);
  List_ANY_Var(Machine(A8051),MOVB)==(?);
  List_ANY_Var(Machine(A8051),NOP)==(?);
  List_ANY_Var(Machine(A8051),ORL)==(?);
  List_ANY_Var(Machine(A8051),ORLD)==(?);
  List_ANY_Var(Machine(A8051),ORLI)==(Var(src) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(A8051),ORLB)==(?);
  List_ANY_Var(Machine(A8051),POP)==(?);
  List_ANY_Var(Machine(A8051),PUSH)==(?);
  List_ANY_Var(Machine(A8051),RL)==(?);
  List_ANY_Var(Machine(A8051),RR)==(?);
  List_ANY_Var(Machine(A8051),SETB)==(?);
  List_ANY_Var(Machine(A8051),SJMP)==(?);
  List_ANY_Var(Machine(A8051),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(A8051)) == (? | ROM_LENGTH,ROM_ADDR,ROM_ADDR_MAX,MEM_ADDR,RAM_ADDR,RAM_MIN,RAM_MAX,RAM_BIT_ADDRESSABLE,SFR_ADDR,SFR_MIN,SFR_MAX,SFR_BIT_ADDRESSABLE,BIT_ADDRESS,BIT_ADDRESSABLE,STACK_MAX,PSWUPDATE,BIT_GET,SBUF,PCON,SP,SCON,B98,B99,B9A,B9B,B9C,B9D,B9E,B9F,SCON_0,SCON_1,SCON_2,SCON_3,SCON_4,SCON_5,SCON_6,SCON_7,RI,TI,RB8,TB8,REN,SM2,SM1,SM0,R0,R1,R2,R3,R4,R5,R6,R7,AA,ACC,BE0,BE1,BE2,BE3,BE4,BE5,BE6,BE7,BB,BF0,BF1,BF2,BF3,BF4,BF5,BF6,BF7,P0,P0_0,P0_1,P0_2,P0_3,P0_4,P0_5,P0_6,P0_7,B80,B81,B82,B83,B84,B85,B86,B87,P1,P1_0,P1_1,P1_2,P1_3,P1_4,P1_5,P1_6,P1_7,B90,B91,B92,B93,B94,B95,B96,B97,P2,P2_0,P2_1,P2_2,P2_3,P2_4,P2_5,P2_6,P2_7,BA0,BA1,BA2,BA3,BA4,BA5,BA6,BA7,P3,P3_0,P3_1,P3_2,P3_3,P3_4,P3_5,P3_6,P3_7,BB0,BB1,BB2,BB3,BB4,BB5,BB6,BB7,PSW,PP,OV,RS0,RS1,F0,AC,CY,CC,BD0,BD1,BD2,BD3,BD4,BD5,BD6,BD7,PSW_0,PSW_1,PSW_2,PSW_3,PSW_4,PSW_5,PSW_6,PSW_7,ADDRCHANGE,BITCHANGE,PC_INCREMENT,SP_INCREMENT,SP_DECREMENT | ? | mem | INIT,ADD,ADDI,ADDD,ADDC,ADDCI,ADDCD,ANL,ANLI,ANLD,ANLDA,ANLDD,ANLB,CJNE,CJNED,CJNEI,CLR,CLRA,CPL,CPLA,DEC,DECI,DJNZ,INC,INCI,JB,JBC,JC,JNB,JNC,JNZ,JZ,LCALL,LJMP,MOV,MOVD,MOVDI,MOVI,MOVID,MOVB,NOP,ORL,ORLD,ORLI,ORLB,POP,PUSH,RL,RR,SETB,SJMP | ? | seen(Machine(ALU)),seen(Machine(TYPES)),seen(Machine(BYTE_DEFINITION)),seen(Machine(BIT_DEFINITION)),included(Machine(MEMORY)) | ? | A8051);
  List_Of_HiddenCst_Ids(Machine(A8051)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(A8051)) == (ROM_LENGTH,ROM_ADDR,ROM_ADDR_MAX,MEM_ADDR,RAM_ADDR,RAM_MIN,RAM_MAX,RAM_BIT_ADDRESSABLE,SFR_ADDR,SFR_MIN,SFR_MAX,SFR_BIT_ADDRESSABLE,BIT_ADDRESS,BIT_ADDRESSABLE,STACK_MAX,PSWUPDATE,BIT_GET,SBUF,PCON,SP,SCON,B98,B99,B9A,B9B,B9C,B9D,B9E,B9F,SCON_0,SCON_1,SCON_2,SCON_3,SCON_4,SCON_5,SCON_6,SCON_7,RI,TI,RB8,TB8,REN,SM2,SM1,SM0,R0,R1,R2,R3,R4,R5,R6,R7,AA,ACC,BE0,BE1,BE2,BE3,BE4,BE5,BE6,BE7,BB,BF0,BF1,BF2,BF3,BF4,BF5,BF6,BF7,P0,P0_0,P0_1,P0_2,P0_3,P0_4,P0_5,P0_6,P0_7,B80,B81,B82,B83,B84,B85,B86,B87,P1,P1_0,P1_1,P1_2,P1_3,P1_4,P1_5,P1_6,P1_7,B90,B91,B92,B93,B94,B95,B96,B97,P2,P2_0,P2_1,P2_2,P2_3,P2_4,P2_5,P2_6,P2_7,BA0,BA1,BA2,BA3,BA4,BA5,BA6,BA7,P3,P3_0,P3_1,P3_2,P3_3,P3_4,P3_5,P3_6,P3_7,BB0,BB1,BB2,BB3,BB4,BB5,BB6,BB7,PSW,PP,OV,RS0,RS1,F0,AC,CY,CC,BD0,BD1,BD2,BD3,BD4,BD5,BD6,BD7,PSW_0,PSW_1,PSW_2,PSW_3,PSW_4,PSW_5,PSW_6,PSW_7,ADDRCHANGE,BITCHANGE,PC_INCREMENT,SP_INCREMENT,SP_DECREMENT);
  List_Of_VisibleVar_Ids(Machine(A8051)) == (pc | ?);
  List_Of_Ids_SeenBNU(Machine(A8051)) == (seen(Machine(BYTE_DEFINITION)): (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(BIT_DEFINITION)): (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(TYPES)): (UCHAR,UCHAR_MAX,SCHAR,SCHAR_MAX,SCHAR_MIN,USHORT_INT,USHORT_INT_MAX,byte_schar,schar_byte,byte_uchar,uchar_byte,uchar_schar,schar_uchar,bv16_usint,usint_bv16 | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(BIT_VECTOR_DEFINITION)): (BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(BIT_VECTOR_ARITHMETICS)): (bv_to_nat,bv_par | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(POWER2)): (? | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(MEMORY)) == (ROM_LENGTH,ROM_ADDR,ROM_ADDR_MAX,MEM_ADDR,RAM_ADDR,RAM_MIN,RAM_MAX,RAM_BIT_ADDRESSABLE,SFR_ADDR,SFR_MIN,SFR_MAX,SFR_BIT_ADDRESSABLE,BIT_ADDRESS,BIT_ADDRESSABLE,STACK_MAX,PSWUPDATE,BIT_GET,SBUF,PCON,SP,SCON,B98,B99,B9A,B9B,B9C,B9D,B9E,B9F,SCON_0,SCON_1,SCON_2,SCON_3,SCON_4,SCON_5,SCON_6,SCON_7,RI,TI,RB8,TB8,REN,SM2,SM1,SM0,R0,R1,R2,R3,R4,R5,R6,R7,AA,ACC,BE0,BE1,BE2,BE3,BE4,BE5,BE6,BE7,BB,BF0,BF1,BF2,BF3,BF4,BF5,BF6,BF7,P0,P0_0,P0_1,P0_2,P0_3,P0_4,P0_5,P0_6,P0_7,B80,B81,B82,B83,B84,B85,B86,B87,P1,P1_0,P1_1,P1_2,P1_3,P1_4,P1_5,P1_6,P1_7,B90,B91,B92,B93,B94,B95,B96,B97,P2,P2_0,P2_1,P2_2,P2_3,P2_4,P2_5,P2_6,P2_7,BA0,BA1,BA2,BA3,BA4,BA5,BA6,BA7,P3,P3_0,P3_1,P3_2,P3_3,P3_4,P3_5,P3_6,P3_7,BB0,BB1,BB2,BB3,BB4,BB5,BB6,BB7,PSW,PP,OV,RS0,RS1,F0,AC,CY,CC,BD0,BD1,BD2,BD3,BD4,BD5,BD6,BD7,PSW_0,PSW_1,PSW_2,PSW_3,PSW_4,PSW_5,PSW_6,PSW_7,ADDRCHANGE,BITCHANGE,PC_INCREMENT,SP_INCREMENT,SP_DECREMENT | ? | mem | ? | bitGet,setMem,bitSet,bitClear,addrSetDirect,addrSetIndirect,push_two,push,pop,update | ? | seen(Machine(BYTE_DEFINITION)),seen(Machine(BIT_DEFINITION)),seen(Machine(TYPES)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BIT_VECTOR_ARITHMETICS)),seen(Machine(POWER2)) | ? | MEMORY);
  List_Of_HiddenCst_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(MEMORY)) == (ROM_LENGTH,ROM_ADDR,ROM_ADDR_MAX,MEM_ADDR,RAM_ADDR,RAM_MIN,RAM_MAX,RAM_BIT_ADDRESSABLE,SFR_ADDR,SFR_MIN,SFR_MAX,SFR_BIT_ADDRESSABLE,BIT_ADDRESS,BIT_ADDRESSABLE,STACK_MAX,PSWUPDATE,BIT_GET,SBUF,PCON,SP,SCON,B98,B99,B9A,B9B,B9C,B9D,B9E,B9F,SCON_0,SCON_1,SCON_2,SCON_3,SCON_4,SCON_5,SCON_6,SCON_7,RI,TI,RB8,TB8,REN,SM2,SM1,SM0,R0,R1,R2,R3,R4,R5,R6,R7,AA,ACC,BE0,BE1,BE2,BE3,BE4,BE5,BE6,BE7,BB,BF0,BF1,BF2,BF3,BF4,BF5,BF6,BF7,P0,P0_0,P0_1,P0_2,P0_3,P0_4,P0_5,P0_6,P0_7,B80,B81,B82,B83,B84,B85,B86,B87,P1,P1_0,P1_1,P1_2,P1_3,P1_4,P1_5,P1_6,P1_7,B90,B91,B92,B93,B94,B95,B96,B97,P2,P2_0,P2_1,P2_2,P2_3,P2_4,P2_5,P2_6,P2_7,BA0,BA1,BA2,BA3,BA4,BA5,BA6,BA7,P3,P3_0,P3_1,P3_2,P3_3,P3_4,P3_5,P3_6,P3_7,BB0,BB1,BB2,BB3,BB4,BB5,BB6,BB7,PSW,PP,OV,RS0,RS1,F0,AC,CY,CC,BD0,BD1,BD2,BD3,BD4,BD5,BD6,BD7,PSW_0,PSW_1,PSW_2,PSW_3,PSW_4,PSW_5,PSW_6,PSW_7,ADDRCHANGE,BITCHANGE,PC_INCREMENT,SP_INCREMENT,SP_DECREMENT);
  List_Of_VisibleVar_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(MEMORY)) == (?: ?);
  List_Of_Ids(Machine(POWER2)) == (? | ? | ? | ? | ? | ? | seen(Machine(POWER)) | ? | POWER2);
  List_Of_HiddenCst_Ids(Machine(POWER2)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(POWER2)) == (?);
  List_Of_VisibleVar_Ids(Machine(POWER2)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(POWER2)) == (?: ?);
  List_Of_Ids(Machine(POWER)) == (? | ? | ? | ? | ? | ? | ? | ? | POWER);
  List_Of_HiddenCst_Ids(Machine(POWER)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(POWER)) == (?);
  List_Of_VisibleVar_Ids(Machine(POWER)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(POWER)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (bv_to_nat,bv_par | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(POWER2)) | ? | BIT_VECTOR_ARITHMETICS);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (bv_to_nat,bv_par);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR_ARITHMETICS)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)) | ? | BIT_VECTOR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ? | BIT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit);
  List_Of_VisibleVar_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(TYPES)) == (UCHAR,UCHAR_MAX,SCHAR,SCHAR_MAX,SCHAR_MIN,USHORT_INT,USHORT_INT_MAX,byte_schar,schar_byte,byte_uchar,uchar_byte,uchar_schar,schar_uchar,bv16_usint,usint_bv16 | ? | ? | ? | ? | ? | seen(Machine(BYTE_DEFINITION)),seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_ARITHMETICS)),seen(Machine(BIT_VECTOR16_DEFINITION)),seen(Machine(POWER2)) | ? | TYPES);
  List_Of_HiddenCst_Ids(Machine(TYPES)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (UCHAR,UCHAR_MAX,SCHAR,SCHAR_MAX,SCHAR_MIN,USHORT_INT,USHORT_INT_MAX,byte_schar,schar_byte,byte_uchar,uchar_byte,uchar_schar,schar_uchar,bv16_usint,usint_bv16);
  List_Of_VisibleVar_Ids(Machine(TYPES)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPES)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR16_DEFINITION)) == (BV16_INDX,BV16_WIDTH,BV16_ZERO,BV16,bv16_byte | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BYTE_DEFINITION)) | ? | BIT_VECTOR16_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR16_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR16_DEFINITION)) == (BV16_INDX,BV16_WIDTH,BV16_ZERO,BV16,bv16_byte);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR16_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR16_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)) | ? | BYTE_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO);
  List_Of_VisibleVar_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BYTE_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(ALU)) == (parity,half,add,sub,and,and_bit,ior,xor,complement,swap,rotateleft,rotateright,usint_byte | ? | ? | ? | ? | ? | seen(Machine(TYPES)),seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BYTE_DEFINITION)),seen(Machine(BIT_VECTOR_ARITHMETICS)),seen(Machine(BIT_VECTOR16_DEFINITION)) | ? | ALU);
  List_Of_HiddenCst_Ids(Machine(ALU)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ALU)) == (parity,half,add,sub,and,and_bit,ior,xor,complement,swap,rotateleft,rotateright,usint_byte);
  List_Of_VisibleVar_Ids(Machine(ALU)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ALU)) == (?: ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(A8051)) == (Type(SP_DECREMENT) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[ROM_ADDR","]ROM_ADDR")*btype(INTEGER,"[RAM_ADDR","]RAM_ADDR")));Type(SP_INCREMENT) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[ROM_ADDR","]ROM_ADDR")*btype(INTEGER,"[RAM_ADDR","]RAM_ADDR")));Type(PC_INCREMENT) == Cst(SetOf(btype(INTEGER,"[ROM_ADDR","]ROM_ADDR")*btype(INTEGER,"[ROM_ADDR","]ROM_ADDR")*btype(INTEGER,"[ROM_ADDR","]ROM_ADDR")));Type(BITCHANGE) == Cst(SetOf(btype(INTEGER,"[MEM_ADDR","]MEM_ADDR")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BYTE_INDEX","]BYTE_INDEX")*btype(INTEGER,"[BIT","]BIT")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))));Type(ADDRCHANGE) == Cst(SetOf(btype(INTEGER,"[MEM_ADDR","]MEM_ADDR")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))));Type(PSW_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(CC) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(CY) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(AC) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(F0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RS1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RS0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(OV) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PP) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW) == Cst(btype(INTEGER,?,?));Type(BB7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3) == Cst(btype(INTEGER,?,?));Type(BA7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2) == Cst(btype(INTEGER,?,?));Type(B97) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B96) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B95) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B94) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B93) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B92) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B91) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B90) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1) == Cst(btype(INTEGER,?,?));Type(B87) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B86) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B85) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B84) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B83) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B82) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B81) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B80) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0) == Cst(btype(INTEGER,?,?));Type(BF7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB) == Cst(btype(INTEGER,?,?));Type(BE7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ACC) == Cst(btype(INTEGER,?,?));Type(AA) == Cst(btype(INTEGER,?,?));Type(R7) == Cst(btype(INTEGER,?,?));Type(R6) == Cst(btype(INTEGER,?,?));Type(R5) == Cst(btype(INTEGER,?,?));Type(R4) == Cst(btype(INTEGER,?,?));Type(R3) == Cst(btype(INTEGER,?,?));Type(R2) == Cst(btype(INTEGER,?,?));Type(R1) == Cst(btype(INTEGER,?,?));Type(R0) == Cst(btype(INTEGER,?,?));Type(SM0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SM1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SM2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(REN) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(TB8) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RB8) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(TI) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RI) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9F) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9E) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9D) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9C) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9B) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9A) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B99) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B98) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON) == Cst(btype(INTEGER,?,?));Type(SP) == Cst(btype(INTEGER,?,?));Type(PCON) == Cst(btype(INTEGER,?,?));Type(SBUF) == Cst(btype(INTEGER,?,?));Type(BIT_GET) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*btype(INTEGER,"[BIT","]BIT")));Type(PSWUPDATE) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(STACK_MAX) == Cst(btype(INTEGER,?,?));Type(BIT_ADDRESSABLE) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(BIT_ADDRESS) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(SFR_BIT_ADDRESSABLE) == Cst(SetOf(btype(INTEGER,"[SFR_BIT_ADDRESSABLE","]SFR_BIT_ADDRESSABLE")));Type(SFR_MAX) == Cst(btype(INTEGER,?,?));Type(SFR_MIN) == Cst(btype(INTEGER,?,?));Type(SFR_ADDR) == Cst(SetOf(btype(INTEGER,"[SFR_ADDR","]SFR_ADDR")));Type(RAM_BIT_ADDRESSABLE) == Cst(SetOf(btype(INTEGER,"[RAM_BIT_ADDRESSABLE","]RAM_BIT_ADDRESSABLE")));Type(RAM_MAX) == Cst(btype(INTEGER,?,?));Type(RAM_MIN) == Cst(btype(INTEGER,?,?));Type(RAM_ADDR) == Cst(SetOf(btype(INTEGER,"[RAM_ADDR","]RAM_ADDR")));Type(MEM_ADDR) == Cst(SetOf(btype(INTEGER,"[MEM_ADDR","]MEM_ADDR")));Type(ROM_ADDR_MAX) == Cst(btype(INTEGER,?,?));Type(ROM_ADDR) == Cst(SetOf(btype(INTEGER,"[ROM_ADDR","]ROM_ADDR")));Type(ROM_LENGTH) == Cst(btype(INTEGER,?,?)))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(A8051)) == (Type(mem) == Mvl(SetOf(btype(INTEGER,"[MEM_ADDR","]MEM_ADDR")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))))
END
&
THEORY VisibleVariablesEnvX IS
  VisibleVariables(Machine(A8051)) == (Type(pc) == Mvv(btype(INTEGER,?,?)))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(A8051)) == (Type(SJMP) == Cst(No_type,btype(INTEGER,?,?));Type(SETB) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RR) == Cst(No_type,btype(INTEGER,?,?));Type(RL) == Cst(No_type,btype(INTEGER,?,?));Type(PUSH) == Cst(No_type,btype(INTEGER,?,?));Type(POP) == Cst(No_type,btype(INTEGER,?,?));Type(ORLB) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(ORLI) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ORLD) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ORL) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(NOP) == Cst(No_type,No_type);Type(MOVB) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(MOVID) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(MOVI) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(MOVDI) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(MOVD) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(MOV) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(LJMP) == Cst(No_type,btype(INTEGER,?,?));Type(LCALL) == Cst(No_type,btype(INTEGER,?,?));Type(JZ) == Cst(No_type,btype(INTEGER,?,?));Type(JNZ) == Cst(No_type,btype(INTEGER,?,?));Type(JNC) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(JNB) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(JC) == Cst(No_type,btype(INTEGER,?,?));Type(JBC) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(JB) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(INCI) == Cst(No_type,btype(INTEGER,?,?));Type(INC) == Cst(No_type,btype(INTEGER,?,?));Type(DJNZ) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(DECI) == Cst(No_type,btype(INTEGER,?,?));Type(DEC) == Cst(No_type,btype(INTEGER,?,?));Type(CPLA) == Cst(No_type,btype(INTEGER,?,?));Type(CPL) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(CLRA) == Cst(No_type,btype(INTEGER,?,?));Type(CLR) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(CJNEI) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(CJNED) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(CJNE) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ANLB) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)*(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(ANLDD) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ANLDA) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ANLD) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ANLI) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ANL) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ADDCD) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ADDCI) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ADDC) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ADDD) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ADDI) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ADD) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(INIT) == Cst(No_type,No_type))
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
  variant_clause_mandatory == OK
END
)
