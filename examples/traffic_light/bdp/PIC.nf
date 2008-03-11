Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(PIC))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(PIC))==(Machine(PIC));
  Level(Machine(PIC))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(PIC)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(PIC))==(TYPES)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(PIC))==(ALU)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(PIC))==(?);
  List_Includes(Machine(PIC))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(PIC))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(PIC))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(PIC))==(?);
  Context_List_Variables(Machine(PIC))==(?);
  Abstract_List_Variables(Machine(PIC))==(?);
  Local_List_Variables(Machine(PIC))==(?);
  List_Variables(Machine(PIC))==(?);
  External_List_Variables(Machine(PIC))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(PIC))==(?);
  Abstract_List_VisibleVariables(Machine(PIC))==(?);
  External_List_VisibleVariables(Machine(PIC))==(?);
  Expanded_List_VisibleVariables(Machine(PIC))==(?);
  List_VisibleVariables(Machine(PIC))==(sp,stack,pc,c,z,w,mem);
  Internal_List_VisibleVariables(Machine(PIC))==(sp,stack,pc,c,z,w,mem)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(PIC))==(btrue);
  Gluing_List_Invariant(Machine(PIC))==(btrue);
  Expanded_List_Invariant(Machine(PIC))==(btrue);
  Abstract_List_Invariant(Machine(PIC))==(btrue);
  Context_List_Invariant(Machine(PIC))==(btrue);
  List_Invariant(Machine(PIC))==(mem : REGISTER --> WORD & w : WORD & z : BOOL & c : BOOL & pc : INSTRUCTION & sp : NATURAL & stack : NATURAL +-> INSTRUCTION & dom(stack) = 0..sp-1)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(PIC))==(btrue);
  Abstract_List_Assertions(Machine(PIC))==(btrue);
  Context_List_Assertions(Machine(PIC))==(NB_WORDS = 256 & !n.(n : WORD => 0<=n) & !n.(n : WORD => n<=255) & WORD_POSITION = BV8_INDEX & BIT_FLIP(0) = 1 & BIT_FLIP(1) = 0 & BIT_AND(0,0) = 0 & BIT_AND(0,1) = 0 & BIT_AND(1,0) = 0 & BIT_AND(1,1) = 1 & BIT_IOR(0,0) = 0 & BIT_IOR(0,1) = 0 & BIT_IOR(1,0) = 0 & BIT_IOR(1,1) = 1 & BIT_XOR(0,0) = 0 & BIT_XOR(0,1) = 1 & BIT_XOR(1,0) = 1 & BIT_XOR(1,1) = 0 & dom(add) = WORD*WORD & ran(add) <: WORD*BOOL*BOOL & dom(substract) = WORD*WORD & ran(substract) <: WORD*BOOL*BOOL & dom(and) = WORD*WORD & ran(and) <: WORD*BOOL & dom(ior) = WORD*WORD & ran(ior) <: WORD*BOOL & dom(xor) = WORD*WORD & ran(xor) <: WORD*BOOL & dom(bitclear) = WORD*WORD_POSITION & ran(bitclear) <: WORD & dom(bitset) = WORD*WORD_POSITION & ran(bitset) <: WORD & dom(bitget) = WORD*WORD_POSITION & ran(bitget) <: BIT & dom(complement) = WORD & ran(complement) <: WORD & dom(swap) = WORD & ran(swap) <: WORD & ran(rotateleft) <: WORD*BOOL & dom(rotateleft) = WORD & dom(rotateright) = WORD & ran(rotateright) <: WORD*BOOL);
  List_Assertions(Machine(PIC))==(ran(mem) <: WORD & dom(mem) = REGISTER)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(PIC))==(@(w$0).(w$0 : WORD ==> w:=w$0) || @(mem$0).(mem$0 : REGISTER --> WORD ==> mem:=mem$0) || @(z$0).(z$0 : BOOL ==> z:=z$0) || @(c$0).(c$0 : BOOL ==> c:=c$0) || @(pc$0).(pc$0 : INSTRUCTION ==> pc:=pc$0) || stack:={} || sp:=0);
  Context_List_Initialisation(Machine(PIC))==(skip);
  List_Initialisation(Machine(PIC))==(w:: WORD || mem:: REGISTER --> WORD || z:: BOOL || c:: BOOL || pc:: INSTRUCTION || stack:={} || sp:=0)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(PIC))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(PIC),Machine(ALU))==(?);
  List_Instanciated_Parameters(Machine(PIC),Machine(TYPES))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(PIC))==(btrue);
  List_Constraints(Machine(PIC))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(PIC))==(CALL,RETURN,RETLW,ADDLW,ADDWF,SUBLW,SUBWF,ANDLW,ANDLWF,IORLW,IORLWF,XORLW,XORLWF,BCF,BSF,BTFSC,BTFSS,CLRF,CLRW,COMF,DECF,DECFSZ,GOTO,INCF,INCFSZ,MOVLW,MOVF,MOVWF,NOP,ROLF,RORF,SWAP);
  List_Operations(Machine(PIC))==(CALL,RETURN,RETLW,ADDLW,ADDWF,SUBLW,SUBWF,ANDLW,ANDLWF,IORLW,IORLWF,XORLW,XORLWF,BCF,BSF,BTFSC,BTFSS,CLRF,CLRW,COMF,DECF,DECFSZ,GOTO,INCF,INCFSZ,MOVLW,MOVF,MOVWF,NOP,ROLF,RORF,SWAP)
END
&
THEORY ListInputX IS
  List_Input(Machine(PIC),CALL)==(k);
  List_Input(Machine(PIC),RETURN)==(?);
  List_Input(Machine(PIC),RETLW)==(k);
  List_Input(Machine(PIC),ADDLW)==(k);
  List_Input(Machine(PIC),ADDWF)==(f,d);
  List_Input(Machine(PIC),SUBLW)==(k);
  List_Input(Machine(PIC),SUBWF)==(f,d);
  List_Input(Machine(PIC),ANDLW)==(k);
  List_Input(Machine(PIC),ANDLWF)==(f,d);
  List_Input(Machine(PIC),IORLW)==(k);
  List_Input(Machine(PIC),IORLWF)==(f,d);
  List_Input(Machine(PIC),XORLW)==(k);
  List_Input(Machine(PIC),XORLWF)==(f,d);
  List_Input(Machine(PIC),BCF)==(f,b);
  List_Input(Machine(PIC),BSF)==(f,b);
  List_Input(Machine(PIC),BTFSC)==(f,b);
  List_Input(Machine(PIC),BTFSS)==(f,b);
  List_Input(Machine(PIC),CLRF)==(f);
  List_Input(Machine(PIC),CLRW)==(?);
  List_Input(Machine(PIC),COMF)==(f,d);
  List_Input(Machine(PIC),DECF)==(f,d);
  List_Input(Machine(PIC),DECFSZ)==(f,d);
  List_Input(Machine(PIC),GOTO)==(k);
  List_Input(Machine(PIC),INCF)==(f,d);
  List_Input(Machine(PIC),INCFSZ)==(f,d);
  List_Input(Machine(PIC),MOVLW)==(k);
  List_Input(Machine(PIC),MOVF)==(f,d);
  List_Input(Machine(PIC),MOVWF)==(f);
  List_Input(Machine(PIC),NOP)==(?);
  List_Input(Machine(PIC),ROLF)==(f,d);
  List_Input(Machine(PIC),RORF)==(f,d);
  List_Input(Machine(PIC),SWAP)==(f,d)
END
&
THEORY ListOutputX IS
  List_Output(Machine(PIC),CALL)==(?);
  List_Output(Machine(PIC),RETURN)==(?);
  List_Output(Machine(PIC),RETLW)==(?);
  List_Output(Machine(PIC),ADDLW)==(?);
  List_Output(Machine(PIC),ADDWF)==(?);
  List_Output(Machine(PIC),SUBLW)==(?);
  List_Output(Machine(PIC),SUBWF)==(?);
  List_Output(Machine(PIC),ANDLW)==(?);
  List_Output(Machine(PIC),ANDLWF)==(?);
  List_Output(Machine(PIC),IORLW)==(?);
  List_Output(Machine(PIC),IORLWF)==(?);
  List_Output(Machine(PIC),XORLW)==(?);
  List_Output(Machine(PIC),XORLWF)==(?);
  List_Output(Machine(PIC),BCF)==(?);
  List_Output(Machine(PIC),BSF)==(?);
  List_Output(Machine(PIC),BTFSC)==(?);
  List_Output(Machine(PIC),BTFSS)==(?);
  List_Output(Machine(PIC),CLRF)==(?);
  List_Output(Machine(PIC),CLRW)==(?);
  List_Output(Machine(PIC),COMF)==(?);
  List_Output(Machine(PIC),DECF)==(?);
  List_Output(Machine(PIC),DECFSZ)==(?);
  List_Output(Machine(PIC),GOTO)==(?);
  List_Output(Machine(PIC),INCF)==(?);
  List_Output(Machine(PIC),INCFSZ)==(?);
  List_Output(Machine(PIC),MOVLW)==(?);
  List_Output(Machine(PIC),MOVF)==(?);
  List_Output(Machine(PIC),MOVWF)==(?);
  List_Output(Machine(PIC),NOP)==(?);
  List_Output(Machine(PIC),ROLF)==(?);
  List_Output(Machine(PIC),RORF)==(?);
  List_Output(Machine(PIC),SWAP)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(PIC),CALL)==(CALL(k));
  List_Header(Machine(PIC),RETURN)==(RETURN);
  List_Header(Machine(PIC),RETLW)==(RETLW(k));
  List_Header(Machine(PIC),ADDLW)==(ADDLW(k));
  List_Header(Machine(PIC),ADDWF)==(ADDWF(f,d));
  List_Header(Machine(PIC),SUBLW)==(SUBLW(k));
  List_Header(Machine(PIC),SUBWF)==(SUBWF(f,d));
  List_Header(Machine(PIC),ANDLW)==(ANDLW(k));
  List_Header(Machine(PIC),ANDLWF)==(ANDLWF(f,d));
  List_Header(Machine(PIC),IORLW)==(IORLW(k));
  List_Header(Machine(PIC),IORLWF)==(IORLWF(f,d));
  List_Header(Machine(PIC),XORLW)==(XORLW(k));
  List_Header(Machine(PIC),XORLWF)==(XORLWF(f,d));
  List_Header(Machine(PIC),BCF)==(BCF(f,b));
  List_Header(Machine(PIC),BSF)==(BSF(f,b));
  List_Header(Machine(PIC),BTFSC)==(BTFSC(f,b));
  List_Header(Machine(PIC),BTFSS)==(BTFSS(f,b));
  List_Header(Machine(PIC),CLRF)==(CLRF(f));
  List_Header(Machine(PIC),CLRW)==(CLRW);
  List_Header(Machine(PIC),COMF)==(COMF(f,d));
  List_Header(Machine(PIC),DECF)==(DECF(f,d));
  List_Header(Machine(PIC),DECFSZ)==(DECFSZ(f,d));
  List_Header(Machine(PIC),GOTO)==(GOTO(k));
  List_Header(Machine(PIC),INCF)==(INCF(f,d));
  List_Header(Machine(PIC),INCFSZ)==(INCFSZ(f,d));
  List_Header(Machine(PIC),MOVLW)==(MOVLW(k));
  List_Header(Machine(PIC),MOVF)==(MOVF(f,d));
  List_Header(Machine(PIC),MOVWF)==(MOVWF(f));
  List_Header(Machine(PIC),NOP)==(NOP);
  List_Header(Machine(PIC),ROLF)==(ROLF(f,d));
  List_Header(Machine(PIC),RORF)==(RORF(f,d));
  List_Header(Machine(PIC),SWAP)==(SWAP(f,d))
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(PIC),CALL)==(k : INSTRUCTION);
  List_Precondition(Machine(PIC),RETURN)==(sp>0);
  List_Precondition(Machine(PIC),RETLW)==(k : WORD & sp>0);
  List_Precondition(Machine(PIC),ADDLW)==(k : WORD);
  List_Precondition(Machine(PIC),ADDWF)==(f : REGISTER & d : BIT);
  List_Precondition(Machine(PIC),SUBLW)==(k : WORD);
  List_Precondition(Machine(PIC),SUBWF)==(f : REGISTER & d : BIT);
  List_Precondition(Machine(PIC),ANDLW)==(k : WORD);
  List_Precondition(Machine(PIC),ANDLWF)==(f : REGISTER & d : BIT);
  List_Precondition(Machine(PIC),IORLW)==(k : WORD);
  List_Precondition(Machine(PIC),IORLWF)==(f : REGISTER & d : BIT);
  List_Precondition(Machine(PIC),XORLW)==(k : WORD);
  List_Precondition(Machine(PIC),XORLWF)==(f : REGISTER & d : BIT);
  List_Precondition(Machine(PIC),BCF)==(f : REGISTER & b : WORD_POSITION);
  List_Precondition(Machine(PIC),BSF)==(f : REGISTER & b : WORD_POSITION);
  List_Precondition(Machine(PIC),BTFSC)==(f : REGISTER & b : WORD_POSITION);
  List_Precondition(Machine(PIC),BTFSS)==(f : REGISTER & b : WORD_POSITION);
  List_Precondition(Machine(PIC),CLRF)==(f : REGISTER);
  List_Precondition(Machine(PIC),CLRW)==(btrue);
  List_Precondition(Machine(PIC),COMF)==(f : REGISTER & d : BIT);
  List_Precondition(Machine(PIC),DECF)==(f : REGISTER & d : BIT);
  List_Precondition(Machine(PIC),DECFSZ)==(f : REGISTER & d : BIT);
  List_Precondition(Machine(PIC),GOTO)==(k : INSTRUCTION);
  List_Precondition(Machine(PIC),INCF)==(f : REGISTER & d : BIT);
  List_Precondition(Machine(PIC),INCFSZ)==(f : REGISTER & d : BIT);
  List_Precondition(Machine(PIC),MOVLW)==(k : WORD);
  List_Precondition(Machine(PIC),MOVF)==(f : REGISTER & d : BIT);
  List_Precondition(Machine(PIC),MOVWF)==(f : REGISTER);
  List_Precondition(Machine(PIC),NOP)==(btrue);
  List_Precondition(Machine(PIC),ROLF)==(f : REGISTER & d : BIT);
  List_Precondition(Machine(PIC),RORF)==(f : REGISTER & d : BIT);
  List_Precondition(Machine(PIC),SWAP)==(f : REGISTER & d : BIT)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(PIC),SWAP)==(f : REGISTER & d : BIT | @result.(result = swap(mem(f)) ==> (d = 0 ==> w:=result [] not(d = 0) ==> mem:=mem<+{f|->result})) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),RORF)==(f : REGISTER & d : BIT | @(result,carry).(result : WORD & carry : BOOL & result,carry = rotateright(mem(f)) ==> (d = 0 ==> w:=result [] not(d = 0) ==> mem:=mem<+{f|->result} || c:=carry)) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),ROLF)==(f : REGISTER & d : BIT | @(result,carry).(result : WORD & carry : BOOL & result,carry = rotateleft(mem(f)) ==> (d = 0 ==> w:=result [] not(d = 0) ==> mem:=mem<+{f|->result} || c:=carry)) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),NOP)==(btrue | pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),MOVWF)==(f : REGISTER | mem,pc:=mem<+{f|->w},INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),MOVF)==(f : REGISTER & d : BIT | d = 0 ==> w:=mem(f) [] not(d = 0) ==> skip || z:=bool(mem(f) = 0) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),MOVLW)==(k : WORD | w,pc:=k,INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),INCFSZ)==(f : REGISTER & d : BIT | @(result,carry,zero).(result : WORD & carry : BOOL & zero : BOOL & result,carry,zero = add(mem(f),1) ==> (d = 0 ==> w:=result [] not(d = 0) ==> mem:=mem<+{f|->result} || (result = 0 ==> pc:=INSTRUCTION_NEXT(INSTRUCTION_NEXT(pc)) [] not(result = 0) ==> pc:=INSTRUCTION_NEXT(pc)))));
  Expanded_List_Substitution(Machine(PIC),INCF)==(f : REGISTER & d : BIT | @(result,carry,zero).(result : WORD & carry : BOOL & zero : BOOL & result,carry,zero = add(mem(f),1) ==> (d = 0 ==> w:=result [] not(d = 0) ==> mem:=mem<+{f|->result} || z:=bool(result = 0))) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),GOTO)==(k : INSTRUCTION | pc:=k);
  Expanded_List_Substitution(Machine(PIC),DECFSZ)==(f : REGISTER & d : BIT | @(result,borrow,zero).(result : WORD & borrow : BOOL & zero : BOOL & result,borrow,zero = substract(mem(f),1) ==> (d = 0 ==> w:=result [] not(d = 0) ==> mem:=mem<+{f|->result} || z:=zero || (result = 0 ==> pc:=INSTRUCTION_NEXT(INSTRUCTION_NEXT(pc)) [] not(result = 0) ==> pc:=INSTRUCTION_NEXT(pc)))));
  Expanded_List_Substitution(Machine(PIC),DECF)==(f : REGISTER & d : BIT | @(result,borrow,zero).(result : WORD & borrow : BOOL & zero : BOOL & result,borrow,zero = substract(mem(f),1) ==> (d = 0 ==> w:=result [] not(d = 0) ==> mem:=mem<+{f|->result} || z:=zero)) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),COMF)==(f : REGISTER & d : BIT | @result.(result = complement(mem(f)) ==> (d = 0 ==> w:=result [] not(d = 0) ==> mem:=mem<+{f|->result} || z:=bool(result = 0))) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),CLRW)==(btrue | w,z,pc:=0,TRUE,INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),CLRF)==(f : REGISTER | mem,z,pc:=mem<+{f|->0},TRUE,INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),BTFSS)==(f : REGISTER & b : WORD_POSITION | bitget(mem(f),b) = 1 ==> pc:=INSTRUCTION_NEXT(INSTRUCTION_NEXT(pc)) [] not(bitget(mem(f),b) = 1) ==> pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),BTFSC)==(f : REGISTER & b : WORD_POSITION | bitget(mem(f),b) = 0 ==> pc:=INSTRUCTION_NEXT(INSTRUCTION_NEXT(pc)) [] not(bitget(mem(f),b) = 0) ==> pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),BSF)==(f : REGISTER & b : WORD_POSITION | mem,pc:=mem<+{f|->bitset(mem(f),b)},INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),BCF)==(f : REGISTER & b : WORD_POSITION | mem,pc:=mem<+{f|->bitclear(mem(f),b)},INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),XORLWF)==(f : REGISTER & d : BIT | @(result,zero).(result : WORD & zero : BOOL & result,zero = xor(mem(f),w) ==> (d = 0 ==> w:=result [] not(d = 0) ==> mem:=mem<+{f|->result})) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),XORLW)==(k : WORD | @(result,zero).(result : WORD & zero : BOOL & result,zero = xor(k,w) ==> w,z:=result,zero) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),IORLWF)==(f : REGISTER & d : BIT | @(result,zero).(result : WORD & zero : BOOL & result,zero = ior(mem(f),w) ==> (d = 0 ==> w:=result [] not(d = 0) ==> mem:=mem<+{f|->result})) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),IORLW)==(k : WORD | @(result,zero).(result : WORD & zero : BOOL & result,zero = ior(k,w) ==> w,z:=result,zero) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),ANDLWF)==(f : REGISTER & d : BIT | @(result,zero).(result : WORD & zero : BOOL & result,zero = and(mem(f),w) ==> (d = 0 ==> w:=result [] not(d = 0) ==> mem:=mem<+{f|->result})) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),ANDLW)==(k : WORD | @(result,zero).(result : WORD & zero : BOOL & result,zero = and(k,w) ==> w,z:=result,zero) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),SUBWF)==(f : REGISTER & d : BIT | @(result,borrow,zero).(result : WORD & borrow : BOOL & zero : BOOL & result,borrow,zero = substract(mem(f),w) ==> (d = 0 ==> w:=result [] not(d = 0) ==> mem:=mem<+{f|->result} || c:=borrow || z:=zero)) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),SUBLW)==(k : WORD | @(result,borrow,zero).(result : WORD & borrow : BOOL & zero : BOOL & result,borrow,zero = substract(k,w) ==> w,c,z:=result,borrow,zero) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),ADDWF)==(f : REGISTER & d : BIT | @(result,carry,zero).(result : WORD & carry : BOOL & zero : BOOL & result,carry,zero = add(mem(f),w) ==> (d = 0 ==> w:=result [] not(d = 0) ==> mem:=mem<+{f|->result} || c:=carry || z:=zero)) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),ADDLW)==(k : WORD | @(result,carry,zero).(result : WORD & carry : BOOL & zero : BOOL & result,carry,zero = add(k,w) ==> w,c,z:=result,carry,zero) || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(PIC),RETLW)==(k : WORD & sp>0 | pc,w:=stack(sp-1),k);
  Expanded_List_Substitution(Machine(PIC),RETURN)==(sp>0 | stack,pc,sp:={sp-1}<<|stack,stack(sp-1),sp-1);
  Expanded_List_Substitution(Machine(PIC),CALL)==(k : INSTRUCTION | stack,sp,pc:=stack<+{sp|->INSTRUCTION_NEXT(pc)},sp+1,k);
  List_Substitution(Machine(PIC),CALL)==(stack(sp):=INSTRUCTION_NEXT(pc) || sp:=sp+1 || pc:=k);
  List_Substitution(Machine(PIC),RETURN)==(stack:={sp-1}<<|stack || pc:=stack(sp-1) || sp:=sp-1);
  List_Substitution(Machine(PIC),RETLW)==(pc:=stack(sp-1) || w:=k);
  List_Substitution(Machine(PIC),ADDLW)==(ANY result,carry,zero WHERE result : WORD & carry : BOOL & zero : BOOL & result,carry,zero = add(k,w) THEN w,c,z:=result,carry,zero END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),ADDWF)==(ANY result,carry,zero WHERE result : WORD & carry : BOOL & zero : BOOL & result,carry,zero = add(mem(f),w) THEN IF d = 0 THEN w:=result ELSE mem(f):=result END || c:=carry || z:=zero END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),SUBLW)==(ANY result,borrow,zero WHERE result : WORD & borrow : BOOL & zero : BOOL & result,borrow,zero = substract(k,w) THEN w,c,z:=result,borrow,zero END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),SUBWF)==(ANY result,borrow,zero WHERE result : WORD & borrow : BOOL & zero : BOOL & result,borrow,zero = substract(mem(f),w) THEN IF d = 0 THEN w:=result ELSE mem(f):=result END || c:=borrow || z:=zero END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),ANDLW)==(ANY result,zero WHERE result : WORD & zero : BOOL & result,zero = and(k,w) THEN w,z:=result,zero END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),ANDLWF)==(ANY result,zero WHERE result : WORD & zero : BOOL & result,zero = and(mem(f),w) THEN IF d = 0 THEN w:=result ELSE mem(f):=result END END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),IORLW)==(ANY result,zero WHERE result : WORD & zero : BOOL & result,zero = ior(k,w) THEN w,z:=result,zero END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),IORLWF)==(ANY result,zero WHERE result : WORD & zero : BOOL & result,zero = ior(mem(f),w) THEN IF d = 0 THEN w:=result ELSE mem(f):=result END END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),XORLW)==(ANY result,zero WHERE result : WORD & zero : BOOL & result,zero = xor(k,w) THEN w,z:=result,zero END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),XORLWF)==(ANY result,zero WHERE result : WORD & zero : BOOL & result,zero = xor(mem(f),w) THEN IF d = 0 THEN w:=result ELSE mem(f):=result END END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),BCF)==(mem(f):=bitclear(mem(f),b) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),BSF)==(mem(f):=bitset(mem(f),b) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),BTFSC)==(IF bitget(mem(f),b) = 0 THEN pc:=INSTRUCTION_NEXT(INSTRUCTION_NEXT(pc)) ELSE pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(PIC),BTFSS)==(IF bitget(mem(f),b) = 1 THEN pc:=INSTRUCTION_NEXT(INSTRUCTION_NEXT(pc)) ELSE pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(PIC),CLRF)==(mem(f):=0 || z:=TRUE || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),CLRW)==(w:=0 || z:=TRUE || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),COMF)==(ANY result WHERE result = complement(mem(f)) THEN IF d = 0 THEN w:=result ELSE mem(f):=result END || z:=bool(result = 0) END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),DECF)==(ANY result,borrow,zero WHERE result : WORD & borrow : BOOL & zero : BOOL & result,borrow,zero = substract(mem(f),1) THEN IF d = 0 THEN w:=result ELSE mem(f):=result END || z:=zero END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),DECFSZ)==(ANY result,borrow,zero WHERE result : WORD & borrow : BOOL & zero : BOOL & result,borrow,zero = substract(mem(f),1) THEN IF d = 0 THEN w:=result ELSE mem(f):=result END || z:=zero || IF result = 0 THEN pc:=INSTRUCTION_NEXT(INSTRUCTION_NEXT(pc)) ELSE pc:=INSTRUCTION_NEXT(pc) END END);
  List_Substitution(Machine(PIC),GOTO)==(pc:=k);
  List_Substitution(Machine(PIC),INCF)==(ANY result,carry,zero WHERE result : WORD & carry : BOOL & zero : BOOL & result,carry,zero = add(mem(f),1) THEN IF d = 0 THEN w:=result ELSE mem(f):=result END || z:=bool(result = 0) END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),INCFSZ)==(ANY result,carry,zero WHERE result : WORD & carry : BOOL & zero : BOOL & result,carry,zero = add(mem(f),1) THEN IF d = 0 THEN w:=result ELSE mem(f):=result END || IF result = 0 THEN pc:=INSTRUCTION_NEXT(INSTRUCTION_NEXT(pc)) ELSE pc:=INSTRUCTION_NEXT(pc) END END);
  List_Substitution(Machine(PIC),MOVLW)==(w:=k || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),MOVF)==(IF d = 0 THEN w:=mem(f) ELSE skip END || z:=bool(mem(f) = 0) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),MOVWF)==(mem(f):=w || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),NOP)==(pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),ROLF)==(ANY result,carry WHERE result : WORD & carry : BOOL & result,carry = rotateleft(mem(f)) THEN IF d = 0 THEN w:=result ELSE mem(f):=result END || c:=carry END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),RORF)==(ANY result,carry WHERE result : WORD & carry : BOOL & result,carry = rotateright(mem(f)) THEN IF d = 0 THEN w:=result ELSE mem(f):=result END || c:=carry END || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(PIC),SWAP)==(ANY result WHERE result = swap(mem(f)) THEN IF d = 0 THEN w:=result ELSE mem(f):=result END END || pc:=INSTRUCTION_NEXT(pc))
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(PIC))==(?);
  Inherited_List_Constants(Machine(PIC))==(?);
  List_Constants(Machine(PIC))==(?)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(PIC))==(?);
  Context_List_Defered(Machine(PIC))==(REGISTER);
  Context_List_Sets(Machine(PIC))==(REGISTER);
  List_Valuable_Sets(Machine(PIC))==(?);
  Inherited_List_Enumerated(Machine(PIC))==(?);
  Inherited_List_Defered(Machine(PIC))==(?);
  Inherited_List_Sets(Machine(PIC))==(?);
  List_Enumerated(Machine(PIC))==(?);
  List_Defered(Machine(PIC))==(?);
  List_Sets(Machine(PIC))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(PIC))==(?);
  Expanded_List_HiddenConstants(Machine(PIC))==(?);
  List_HiddenConstants(Machine(PIC))==(?);
  External_List_HiddenConstants(Machine(PIC))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(PIC))==(btrue);
  Context_List_Properties(Machine(PIC))==(WORD_LENGTH : NATURAL & INST_SZ : NATURAL & NB_WORDS : NATURAL & NB_INSTRUCTIONS : NATURAL & WORD_LENGTH = 8 & NB_WORDS = 2**WORD_LENGTH & WORD = 0..NB_WORDS-1 & WORD_POSITION = 0..WORD_LENGTH-1 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = 0..INSTRUCTION_MAX & INSTRUCTION_NEXT : INSTRUCTION --> INSTRUCTION & INSTRUCTION_NEXT = {p,q | p : INSTRUCTION & q : INSTRUCTION & 0<=p & p<NB_INSTRUCTIONS-1 & q = p+1}\/{NB_INSTRUCTIONS-1|->0} & BV_TO_WORD : BV8 --> WORD & WORD_TO_BV : WORD --> BV8 & !(w,v).(w : WORD & v : BV8 => v = WORD_TO_BV(w) <=> (w = 128*v(7)+64*v(6)+32*v(5)+16*v(4)+8*v(3)+4*v(2)+2*v(1)+v(0))) & BV_TO_WORD = WORD_TO_BV~ & not(REGISTER = {}) & BIT = 0..1 & BIT_FLIP : BIT --> BIT & !b.(b : BIT => BIT_FLIP(b) = 1-b) & BIT_AND : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_AND(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & BIT_IOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_IOR(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & BIT_XOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_XOR(b1,b2) = 1 <=> (b1 = 1 & b2 = 0 or (b1 = 0 & b2 = 1))) & BV8_INDEX = 0..7 & BV8 = BV8_INDEX --> BIT & BV8_SET_BIT : BV8*BV8_INDEX*BIT --> BV8 & !(v,i,j,b).(v : BV8 & i : BV8_INDEX & j : BV8_INDEX & b : BIT => (i/=j => BV8_SET_BIT(v,i,b)(j) = v(j))) & !(v,i,b).(v : BV8 & i : BV8_INDEX & b : BIT => BV8_SET_BIT(v,i,b)(i) = b) & BV8_COMPLEMENT : BV8 --> BV8 & !(v,i).(v : BV8 & i : BV8_INDEX => BV8_COMPLEMENT(v)(i) = BIT_FLIP(v(i))) & BV8_ALL_ZEROES : BV8 & !i.(i : BV8_INDEX => BV8_ALL_ZEROES(i) = 0) & BV8_AND : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_AND(v1,v2)(i) = BIT_AND(v1(i),v2(i))) & BV8_IOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_IOR(v1,v2)(i) = BIT_IOR(v1(i),v2(i))) & BV8_XOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_XOR(v1,v2)(i) = BIT_XOR(v1(i),v2(i))) & add : WORD*WORD --> WORD*BOOL*BOOL & !(w1,w2,sum).(w1 : WORD & w2 : WORD & sum : NATURAL & sum = w1+w2 => (sum<=255 => add(w1,w2) = sum,bool(sum = 0),FALSE) & (256<=sum => add(w1,w2) = sum-256,bool(sum = 256),TRUE)) & substract : WORD*WORD --> WORD*BOOL*BOOL & !(w1,w2,diff).(w1 : WORD & w2 : WORD & diff : INTEGER & diff = w1-w2 => (diff<0 => substract(w1,w2) = diff+256,FALSE,TRUE) & (diff>=0 => substract(w1,w2) = diff,bool(diff = 0),FALSE)) & and : WORD*WORD --> WORD*BOOL & !(w1,w2,w).(w1 : WORD & w2 : WORD & w : WORD & w = BV_TO_WORD(BV8_AND(WORD_TO_BV(w1),WORD_TO_BV(w2))) => and(w1,w2) = w,bool(w = 0)) & ior : WORD*WORD --> WORD*BOOL & !(w1,w2,w).(w1 : WORD & w2 : WORD & w : WORD & w = BV_TO_WORD(BV8_IOR(WORD_TO_BV(w1),WORD_TO_BV(w2))) => ior(w1,w2) = w,bool(w = 0)) & xor : WORD*WORD --> WORD*BOOL & !(w1,w2,w).(w1 : WORD & w2 : WORD & w : WORD & w = BV_TO_WORD(BV8_XOR(WORD_TO_BV(w1),WORD_TO_BV(w2))) => xor(w1,w2) = w,bool(w = 0)) & bitget : WORD*WORD_POSITION --> BIT & !(w,i).(w : WORD & i : WORD_POSITION => bitget(w,i) = WORD_TO_BV(w)(i)) & bitset : WORD*WORD_POSITION --> WORD & !(w,i).(w : WORD & i : WORD_POSITION => bitset(w,i) = BV_TO_WORD(BV8_SET_BIT(WORD_TO_BV(w),i,1))) & bitclear : WORD*WORD_POSITION --> WORD & !(w,i,b).(w : WORD & i : WORD_POSITION & b : BIT => bitclear(w,i) = BV_TO_WORD(BV8_SET_BIT(WORD_TO_BV(w),i,0))) & complement : WORD --> WORD & !w.(w : WORD => complement(w) = BV_TO_WORD(BV8_COMPLEMENT(WORD_TO_BV(w)))) & swap : WORD --> WORD & !(w,v).(w : WORD & v : BV8 => (v = WORD_TO_BV(w) => swap(w) = BV_TO_WORD({0|->v(4),1|->v(5),2|->v(6),3|->v(7),4|->v(0),5|->v(1),6|->v(2),7|->v(3)}))) & rotateleft : WORD --> WORD*BOOL & !(w,v).(w : WORD & v : BV8 => (v = WORD_TO_BV(w) => rotateleft(w) = BV_TO_WORD({0|->v(7),1|->v(0),2|->v(1),3|->v(2),4|->v(3),5|->v(4),6|->v(5),7|->v(6)}),bool(v(7) = 1))) & rotateright : WORD --> WORD*BOOL & !(w,v).(w : WORD & v : BV8 => (v = WORD_TO_BV(w) => rotateright(w) = BV_TO_WORD({0|->v(1),1|->v(2),2|->v(3),3|->v(4),4|->v(5),5|->v(6),6|->v(7),7|->v(0)}),bool(v(0) = 1))));
  Inherited_List_Properties(Machine(PIC))==(btrue);
  List_Properties(Machine(PIC))==(btrue)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(PIC),Machine(TYPES))==(?);
  Seen_Context_List_Enumerated(Machine(PIC))==(?);
  Seen_Context_List_Invariant(Machine(PIC))==(btrue);
  Seen_Context_List_Assertions(Machine(PIC))==(btrue);
  Seen_Context_List_Properties(Machine(PIC))==(btrue);
  Seen_List_Constraints(Machine(PIC))==(btrue);
  Seen_List_Operations(Machine(PIC),Machine(TYPES))==(?);
  Seen_Expanded_List_Invariant(Machine(PIC),Machine(TYPES))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(PIC)) == (? | ? | ? | ? | CALL,RETURN,RETLW,ADDLW,ADDWF,SUBLW,SUBWF,ANDLW,ANDLWF,IORLW,IORLWF,XORLW,XORLWF,BCF,BSF,BTFSC,BTFSS,CLRF,CLRW,COMF,DECF,DECFSZ,GOTO,INCF,INCFSZ,MOVLW,MOVF,MOVWF,NOP,ROLF,RORF,SWAP | ? | seen(Machine(TYPES)),used(Machine(ALU)) | ? | PIC);
  List_Of_HiddenCst_Ids(Machine(PIC)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(PIC)) == (?);
  List_Of_VisibleVar_Ids(Machine(PIC)) == (sp,stack,pc,c,z,w,mem | ?);
  List_Of_Ids_SeenBNU(Machine(PIC)) == (? : ?);
  List_Of_Ids(Machine(ALU)) == (add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright | ? | ? | ? | ? | ? | seen(Machine(TYPES)) | ? | ALU);
  List_Of_HiddenCst_Ids(Machine(ALU)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ALU)) == (add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright);
  List_Of_VisibleVar_Ids(Machine(ALU)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ALU)) == (? : ?);
  List_Of_Ids(Machine(TYPES)) == (WORD_LENGTH,WORD,WORD_POSITION,NB_WORDS,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,BV_TO_WORD,WORD_TO_BV,REGISTER | BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | included(Machine(TYPE_BIT)),included(Machine(TYPE_BV8)) | ? | TYPES);
  List_Of_HiddenCst_Ids(Machine(TYPES)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (WORD_LENGTH,WORD,WORD_POSITION,NB_WORDS,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,BV_TO_WORD,WORD_TO_BV,BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR);
  List_Of_VisibleVar_Ids(Machine(TYPES)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPES)) == (seen(Machine(TYPE_BIT)) : (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(TYPE_BV8)) == (BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR | ? | ? | ? | ? | ? | seen(Machine(TYPE_BIT)) | ? | TYPE_BV8);
  List_Of_HiddenCst_Ids(Machine(TYPE_BV8)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BV8)) == (BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR);
  List_Of_VisibleVar_Ids(Machine(TYPE_BV8)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BV8)) == (? : ?);
  List_Of_Ids(Machine(TYPE_BIT)) == (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | ? | ? | ? | TYPE_BIT);
  List_Of_HiddenCst_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BIT)) == (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR);
  List_Of_VisibleVar_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BIT)) == (? : ?)
END
&
THEORY VisibleVariablesEnvX IS
  VisibleVariables(Machine(PIC)) == (Type(sp) == Mvv(btype(INTEGER,?,?));Type(stack) == Mvv(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(pc) == Mvv(btype(INTEGER,?,?));Type(c) == Mvv(btype(BOOL,?,?));Type(z) == Mvv(btype(BOOL,?,?));Type(w) == Mvv(btype(INTEGER,?,?));Type(mem) == Mvv(SetOf(atype(REGISTER,"[REGISTER","]REGISTER")*btype(INTEGER,"[WORD","]WORD"))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(PIC)) == (Type(SWAP) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(RORF) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(ROLF) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(NOP) == Cst(No_type,No_type);Type(MOVWF) == Cst(No_type,atype(REGISTER,?,?));Type(MOVF) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(MOVLW) == Cst(No_type,btype(INTEGER,?,?));Type(INCFSZ) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(INCF) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(GOTO) == Cst(No_type,btype(INTEGER,?,?));Type(DECFSZ) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(DECF) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(COMF) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(CLRW) == Cst(No_type,No_type);Type(CLRF) == Cst(No_type,atype(REGISTER,?,?));Type(BTFSS) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(BTFSC) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(BSF) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(BCF) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(XORLWF) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(XORLW) == Cst(No_type,btype(INTEGER,?,?));Type(IORLWF) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(IORLW) == Cst(No_type,btype(INTEGER,?,?));Type(ANDLWF) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(ANDLW) == Cst(No_type,btype(INTEGER,?,?));Type(SUBWF) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(SUBLW) == Cst(No_type,btype(INTEGER,?,?));Type(ADDWF) == Cst(No_type,atype(REGISTER,?,?)*btype(INTEGER,?,?));Type(ADDLW) == Cst(No_type,btype(INTEGER,?,?));Type(RETLW) == Cst(No_type,btype(INTEGER,?,?));Type(RETURN) == Cst(No_type,No_type);Type(CALL) == Cst(No_type,btype(INTEGER,?,?)))
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
