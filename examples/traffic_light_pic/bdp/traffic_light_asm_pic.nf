Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(traffic_light_asm_pic))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(traffic_light_asm_pic))==(Machine(traffic_light));
  Level(Implementation(traffic_light_asm_pic))==(2);
  Upper_Level(Implementation(traffic_light_asm_pic))==(Refinement(traffic_light_data_refinement))
END
&
THEORY LoadedStructureX IS
  Implementation(traffic_light_asm_pic)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(traffic_light_asm_pic))==(definitions,data_refinement,TYPES)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(traffic_light_asm_pic))==(m.PIC);
  Inherited_List_Includes(Implementation(traffic_light_asm_pic))==(m.PIC)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(traffic_light_asm_pic))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(traffic_light_asm_pic))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(traffic_light_asm_pic))==(?);
  Context_List_Variables(Implementation(traffic_light_asm_pic))==(?);
  Abstract_List_Variables(Implementation(traffic_light_asm_pic))==(counter,color);
  Local_List_Variables(Implementation(traffic_light_asm_pic))==(?);
  List_Variables(Implementation(traffic_light_asm_pic))==(?);
  External_List_Variables(Implementation(traffic_light_asm_pic))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(traffic_light_asm_pic))==(?);
  Abstract_List_VisibleVariables(Implementation(traffic_light_asm_pic))==(?);
  External_List_VisibleVariables(Implementation(traffic_light_asm_pic))==(m.sp,m.stack,m.pc,m.c,m.z,m.w,m.mem);
  Expanded_List_VisibleVariables(Implementation(traffic_light_asm_pic))==(msp,mstack,mpc,mc,mz,mw,mmem);
  List_VisibleVariables(Implementation(traffic_light_asm_pic))==(?);
  Internal_List_VisibleVariables(Implementation(traffic_light_asm_pic))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(traffic_light_asm_pic))==(btrue);
  Abstract_List_Invariant(Implementation(traffic_light_asm_pic))==(counter : NATURAL & counter : 0..2 & counter = color_refine(color) & color : COLOR);
  Expanded_List_Invariant(Implementation(traffic_light_asm_pic))==(mmem : REGISTER --> WORD & mw : WORD & mz : BOOL & mc : BOOL & mpc : INSTRUCTION & msp : NATURAL & mstack : NATURAL +-> INSTRUCTION & dom(mstack) = 0..msp-1);
  Context_List_Invariant(Implementation(traffic_light_asm_pic))==(btrue);
  List_Invariant(Implementation(traffic_light_asm_pic))==(mmem(0) : 0..2 & mmem(0) = counter)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(traffic_light_asm_pic))==(btrue);
  Expanded_List_Assertions(Implementation(traffic_light_asm_pic))==(ran(mmem) <: WORD & dom(mmem) = REGISTER);
  Context_List_Assertions(Implementation(traffic_light_asm_pic))==(!c.(c : COLOR => c = green or c = yellow or c = red) & color_refine(green) = 0 & color_refine(yellow) = 1 & color_refine(red) = 2 & color_step(0) = 1 & color_step(1) = 2 & color_step(2) = 0 & 2**8 = 256 & NB_WORDS = 256 & !n.(n : WORD => 0<=n) & !n.(n : WORD => n<=255) & WORD_POSITION = BV8_INDEX & BIT_FLIP(0) = 1 & BIT_FLIP(1) = 0 & BIT_AND(0,0) = 0 & BIT_AND(0,1) = 0 & BIT_AND(1,0) = 0 & BIT_AND(1,1) = 1 & BIT_IOR(0,0) = 0 & BIT_IOR(0,1) = 0 & BIT_IOR(1,0) = 0 & BIT_IOR(1,1) = 1 & BIT_XOR(0,0) = 0 & BIT_XOR(0,1) = 1 & BIT_XOR(1,0) = 1 & BIT_XOR(1,1) = 0);
  List_Assertions(Implementation(traffic_light_asm_pic))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(traffic_light_asm_pic))==(@(w$0).(w$0 : WORD ==> mw:=w$0) || @(mem$0).(mem$0 : REGISTER --> WORD ==> mmem:=mem$0) || @(z$0).(z$0 : BOOL ==> mz:=z$0) || @(c$0).(c$0 : BOOL ==> mc:=c$0) || @(pc$0).(pc$0 : INSTRUCTION ==> mpc:=pc$0) || mstack:={} || msp:=0;(0 : WORD | mw,mpc:=0,INSTRUCTION_NEXT(mpc));(0 : REGISTER | mmem,mpc:=mmem<+{0|->mw},INSTRUCTION_NEXT(mpc)));
  Context_List_Initialisation(Implementation(traffic_light_asm_pic))==(skip);
  List_Initialisation(Implementation(traffic_light_asm_pic))==(BEGIN (m.MOVLW)(0);(m.MOVWF)(0) END)
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(traffic_light_asm_pic))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(traffic_light_asm_pic),Machine(m.PIC))==(?);
  List_Instanciated_Parameters(Implementation(traffic_light_asm_pic),Machine(definitions))==(?);
  List_Instanciated_Parameters(Implementation(traffic_light_asm_pic),Machine(data_refinement))==(?);
  List_Instanciated_Parameters(Implementation(traffic_light_asm_pic),Machine(TYPES))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(traffic_light_asm_pic),Machine(m.PIC))==(btrue);
  List_Constraints(Implementation(traffic_light_asm_pic))==(btrue);
  List_Context_Constraints(Implementation(traffic_light_asm_pic))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(traffic_light_asm_pic))==(advance);
  List_Operations(Implementation(traffic_light_asm_pic))==(advance)
END
&
THEORY ListInputX IS
  List_Input(Implementation(traffic_light_asm_pic),advance)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(traffic_light_asm_pic),advance)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(traffic_light_asm_pic),advance)==(advance)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(traffic_light_asm_pic),advance)==(btrue);
  List_Precondition(Implementation(traffic_light_asm_pic),advance)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(traffic_light_asm_pic),advance)==(btrue | WHILE mpc<10 DO not(mpc = 9) & not(mpc = 8) & not(mpc = 7) & not(mpc = 6) & not(mpc = 5) & not(mpc = 4) & not(mpc = 3) & not(mpc = 2) & not(mpc = 1) & mpc = 0 ==> (0 : REGISTER & 1 : WORD_POSITION | bitget(mmem(0),1) = 0 ==> mpc:=INSTRUCTION_NEXT(INSTRUCTION_NEXT(mpc)) [] not(bitget(mmem(0),1) = 0) ==> mpc:=INSTRUCTION_NEXT(mpc)) [] not(mpc = 0) & not(mpc = 9) & not(mpc = 8) & not(mpc = 7) & not(mpc = 6) & not(mpc = 5) & not(mpc = 4) & not(mpc = 3) & not(mpc = 2) & mpc = 1 ==> (8 : INSTRUCTION | mpc:=8) [] not(mpc = 0) & not(mpc = 1) & not(mpc = 9) & not(mpc = 8) & not(mpc = 7) & not(mpc = 6) & not(mpc = 5) & not(mpc = 4) & not(mpc = 3) & mpc = 2 ==> (0 : REGISTER & 0 : WORD_POSITION | bitget(mmem(0),0) = 0 ==> mpc:=INSTRUCTION_NEXT(INSTRUCTION_NEXT(mpc)) [] not(bitget(mmem(0),0) = 0) ==> mpc:=INSTRUCTION_NEXT(mpc)) [] not(mpc = 0) & not(mpc = 1) & not(mpc = 2) & not(mpc = 9) & not(mpc = 8) & not(mpc = 7) & not(mpc = 6) & not(mpc = 5) & not(mpc = 4) & mpc = 3 ==> (6 : INSTRUCTION | mpc:=6) [] not(mpc = 0) & not(mpc = 1) & not(mpc = 2) & not(mpc = 3) & not(mpc = 9) & not(mpc = 8) & not(mpc = 7) & not(mpc = 6) & not(mpc = 5) & mpc = 4 ==> (1 : WORD | mw,mpc:=1,INSTRUCTION_NEXT(mpc)) [] not(mpc = 0) & not(mpc = 1) & not(mpc = 2) & not(mpc = 3) & not(mpc = 4) & not(mpc = 9) & not(mpc = 8) & not(mpc = 7) & not(mpc = 6) & mpc = 5 ==> (9 : INSTRUCTION | mpc:=9) [] not(mpc = 0) & not(mpc = 1) & not(mpc = 2) & not(mpc = 3) & not(mpc = 4) & not(mpc = 5) & not(mpc = 9) & not(mpc = 8) & not(mpc = 7) & mpc = 6 ==> (2 : WORD | mw,mpc:=2,INSTRUCTION_NEXT(mpc)) [] not(mpc = 0) & not(mpc = 1) & not(mpc = 2) & not(mpc = 3) & not(mpc = 4) & not(mpc = 5) & not(mpc = 6) & not(mpc = 9) & not(mpc = 8) & mpc = 7 ==> (9 : INSTRUCTION | mpc:=9) [] not(mpc = 0) & not(mpc = 1) & not(mpc = 2) & not(mpc = 3) & not(mpc = 4) & not(mpc = 5) & not(mpc = 6) & not(mpc = 7) & not(mpc = 9) & mpc = 8 ==> (0 : WORD | mw,mpc:=0,INSTRUCTION_NEXT(mpc)) [] not(mpc = 0) & not(mpc = 1) & not(mpc = 2) & not(mpc = 3) & not(mpc = 4) & not(mpc = 5) & not(mpc = 6) & not(mpc = 7) & not(mpc = 8) & mpc = 9 ==> (0 : REGISTER | mmem,mpc:=mmem<+{0|->mw},INSTRUCTION_NEXT(mpc)) [] not(mpc = 0) & not(mpc = 1) & not(mpc = 2) & not(mpc = 3) & not(mpc = 4) & not(mpc = 5) & not(mpc = 6) & not(mpc = 7) & not(mpc = 8) & not(mpc = 9) ==> skip INVARIANT 0<=mpc & mpc<=10 & mmem(0) : 0..2 & (mpc = 0 => mmem(0) : 0..2 & mmem(0) = counter) & (mpc = 1 => mmem(0) = 2 & counter = 2) & (mpc = 2 => mmem(0)/=2 & mmem(0) = counter) & (mpc = 3 => mmem(0) = 1 & counter = 1) & (mpc = 4 => mmem(0) = 0 & mmem(0) = counter) & (mpc = 5 => mmem(0) = 0 & mmem(0) = counter & mw = 1) & (mpc = 6 => mmem(0) = 1 & mmem(0) = counter) & (mpc = 7 => mmem(0) = 1 & mmem(0) = counter & mw = 2) & (mpc = 8 => mmem(0) = 2 & mmem(0) = counter) & (mpc = 9 => mmem(0) : 0..2 & mmem(0) = counter & mw = color_step(counter)) & (mpc = 10 => mmem(0) : 0..2 & mmem(0) = color_step(counter)) VARIANT 10-mpc END);
  List_Substitution(Implementation(traffic_light_asm_pic),advance)==(WHILE m.pc<10 DO BEGIN CASE m.pc OF EITHER 0 THEN (m.BTFSC)(0,1) OR 1 THEN (m.GOTO)(8) OR 2 THEN (m.BTFSC)(0,0) OR 3 THEN (m.GOTO)(6) OR 4 THEN (m.MOVLW)(1) OR 5 THEN (m.GOTO)(9) OR 6 THEN (m.MOVLW)(2) OR 7 THEN (m.GOTO)(9) OR 8 THEN (m.MOVLW)(0) OR 9 THEN (m.MOVWF)(0) END END END INVARIANT 0<=m.pc & m.pc<=10 & (m.mem)(0) : 0..2 & (m.pc = 0 => (m.mem)(0) : 0..2 & (m.mem)(0) = counter) & (m.pc = 1 => (m.mem)(0) = 2 & counter = 2) & (m.pc = 2 => (m.mem)(0)/=2 & (m.mem)(0) = counter) & (m.pc = 3 => (m.mem)(0) = 1 & counter = 1) & (m.pc = 4 => (m.mem)(0) = 0 & (m.mem)(0) = counter) & (m.pc = 5 => (m.mem)(0) = 0 & (m.mem)(0) = counter & m.w = 1) & (m.pc = 6 => (m.mem)(0) = 1 & (m.mem)(0) = counter) & (m.pc = 7 => (m.mem)(0) = 1 & (m.mem)(0) = counter & m.w = 2) & (m.pc = 8 => (m.mem)(0) = 2 & (m.mem)(0) = counter) & (m.pc = 9 => (m.mem)(0) : 0..2 & (m.mem)(0) = counter & m.w = color_step(counter)) & (m.pc = 10 => (m.mem)(0) : 0..2 & (m.mem)(0) = color_step(counter)) VARIANT 10-m.pc END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(traffic_light_asm_pic))==(?);
  Inherited_List_Constants(Implementation(traffic_light_asm_pic))==(?);
  List_Constants(Implementation(traffic_light_asm_pic))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Implementation(traffic_light_asm_pic),COLOR)==({green,yellow,red});
  Context_List_Enumerated(Implementation(traffic_light_asm_pic))==(COLOR);
  Context_List_Defered(Implementation(traffic_light_asm_pic))==(?);
  Context_List_Sets(Implementation(traffic_light_asm_pic))==(COLOR);
  List_Own_Enumerated(Implementation(traffic_light_asm_pic))==(?);
  List_Valuable_Sets(Implementation(traffic_light_asm_pic))==(?);
  Inherited_List_Enumerated(Implementation(traffic_light_asm_pic))==(?);
  Inherited_List_Defered(Implementation(traffic_light_asm_pic))==(?);
  Inherited_List_Sets(Implementation(traffic_light_asm_pic))==(?);
  List_Enumerated(Implementation(traffic_light_asm_pic))==(?);
  List_Defered(Implementation(traffic_light_asm_pic))==(?);
  List_Sets(Implementation(traffic_light_asm_pic))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(traffic_light_asm_pic))==(?);
  Expanded_List_HiddenConstants(Implementation(traffic_light_asm_pic))==(?);
  List_HiddenConstants(Implementation(traffic_light_asm_pic))==(?);
  External_List_HiddenConstants(Implementation(traffic_light_asm_pic))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(traffic_light_asm_pic))==(btrue);
  Context_List_Properties(Implementation(traffic_light_asm_pic))==(not(COLOR = {}) & color_refine : COLOR --> NATURAL & color_refine = {green|->0,yellow|->1,red|->2} & color_step : 0..2 --> 0..2 & color_step = {0|->1,1|->2,2|->0} & WORD_LENGTH : NATURAL & INST_SZ : NATURAL & NB_WORDS : NATURAL & NB_INSTRUCTIONS : NATURAL & WORD_LENGTH = 8 & NB_WORDS = 2**WORD_LENGTH & WORD = 0..NB_WORDS-1 & WORD_POSITION = 0..WORD_LENGTH-1 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = 0..INSTRUCTION_MAX & INSTRUCTION_NEXT : INSTRUCTION --> INSTRUCTION & INSTRUCTION_NEXT = {p,q | p : INSTRUCTION & q : INSTRUCTION & 0<=p & p<NB_INSTRUCTIONS-1 & q = p+1}\/{NB_INSTRUCTIONS-1|->0} & BV_TO_WORD : BV8 --> WORD & WORD_TO_BV : WORD --> BV8 & !(w,v).(w : WORD & v : BV8 => v = WORD_TO_BV(w) <=> (w = 128*v(7)+64*v(6)+32*v(5)+16*v(4)+8*v(3)+4*v(2)+2*v(1)+v(0))) & BV_TO_WORD = WORD_TO_BV~ & !n.(n : NATURAL & n>0 => 2**n = 2*2**(n-1)) & 2**0 = 1 & REGISTER : POW(INTEGER) & REGISTER = 0..127 & BIT = 0..1 & BIT_FLIP : BIT --> BIT & !b.(b : BIT => BIT_FLIP(b) = 1-b) & BIT_AND : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_AND(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & BIT_IOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_IOR(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & BIT_XOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_XOR(b1,b2) = 1 <=> (b1 = 1 & b2 = 0 or (b1 = 0 & b2 = 1))) & BV8_INDEX = 0..7 & BV8 = BV8_INDEX --> BIT & BV8_SET_BIT : BV8*BV8_INDEX*BIT --> BV8 & !(v,i,j,b).(v : BV8 & i : BV8_INDEX & j : BV8_INDEX & b : BIT => (i/=j => BV8_SET_BIT(v,i,b)(j) = v(j))) & !(v,i,b).(v : BV8 & i : BV8_INDEX & b : BIT => BV8_SET_BIT(v,i,b)(i) = b) & BV8_COMPLEMENT : BV8 --> BV8 & !(v,i).(v : BV8 & i : BV8_INDEX => BV8_COMPLEMENT(v)(i) = BIT_FLIP(v(i))) & BV8_ALL_ZEROES : BV8 & !i.(i : BV8_INDEX => BV8_ALL_ZEROES(i) = 0) & BV8_AND : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_AND(v1,v2)(i) = BIT_AND(v1(i),v2(i))) & BV8_IOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_IOR(v1,v2)(i) = BIT_IOR(v1(i),v2(i))) & BV8_XOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_XOR(v1,v2)(i) = BIT_XOR(v1(i),v2(i))));
  Inherited_List_Properties(Implementation(traffic_light_asm_pic))==(btrue);
  List_Properties(Implementation(traffic_light_asm_pic))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(traffic_light_asm_pic))==(aa : aa);
  List_Values(Implementation(traffic_light_asm_pic))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(traffic_light_asm_pic),Machine(TYPES))==(?);
  Seen_Context_List_Enumerated(Implementation(traffic_light_asm_pic))==(COLOR);
  Seen_Context_List_Invariant(Implementation(traffic_light_asm_pic))==(btrue);
  Seen_Context_List_Assertions(Implementation(traffic_light_asm_pic))==(!c.(c : COLOR => c = green or c = yellow or c = red));
  Seen_Context_List_Properties(Implementation(traffic_light_asm_pic))==(not(COLOR = {}));
  Seen_List_Constraints(Implementation(traffic_light_asm_pic))==(btrue);
  Seen_List_Operations(Implementation(traffic_light_asm_pic),Machine(TYPES))==(?);
  Seen_Expanded_List_Invariant(Implementation(traffic_light_asm_pic),Machine(TYPES))==(btrue);
  Seen_Internal_List_Operations(Implementation(traffic_light_asm_pic),Machine(data_refinement))==(?);
  Seen_List_Operations(Implementation(traffic_light_asm_pic),Machine(data_refinement))==(?);
  Seen_Expanded_List_Invariant(Implementation(traffic_light_asm_pic),Machine(data_refinement))==(btrue);
  Set_Definition(Implementation(traffic_light_asm_pic),COLOR)==({green,yellow,red});
  Seen_Internal_List_Operations(Implementation(traffic_light_asm_pic),Machine(definitions))==(?);
  Seen_List_Operations(Implementation(traffic_light_asm_pic),Machine(definitions))==(?);
  Seen_Expanded_List_Invariant(Implementation(traffic_light_asm_pic),Machine(definitions))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(traffic_light_asm_pic),Machine(PIC))==(CALL,RETURN,RETLW,ADDLW,ADDWF,SUBLW,SUBWF,ANDLW,ANDLWF,IORLW,IORLWF,XORLW,XORLWF,BCF,BSF,BTFSC,BTFSS,CLRF,CLRW,COMF,DECF,DECFSZ,GOTO,INCF,INCFSZ,MOVLW,MOVF,MOVWF,NOP,ROLF,RORF,SWAP)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(traffic_light_asm_pic))==(Type(advance) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX IS
  List_Constants(Implementation(traffic_light_asm_pic),Machine(TYPES))==(BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR,BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,WORD_LENGTH,WORD,WORD_POSITION,NB_WORDS,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,BV_TO_WORD,WORD_TO_BV,REGISTER);
  List_Constants_Env(Implementation(traffic_light_asm_pic),Machine(TYPES))==(Type(BIT_XOR) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT_IOR) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT_AND) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT_FLIP) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")));Type(BV8_XOR) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_IOR) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_AND) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_ALL_ZEROES) == Cst(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT")));Type(BV8_COMPLEMENT) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_SET_BIT) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT")*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_INDEX) == Cst(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")));Type(WORD_LENGTH) == Cst(btype(INTEGER,?,?));Type(WORD) == Cst(SetOf(btype(INTEGER,"[WORD","]WORD")));Type(WORD_POSITION) == Cst(SetOf(btype(INTEGER,"[WORD_POSITION","]WORD_POSITION")));Type(NB_WORDS) == Cst(btype(INTEGER,?,?));Type(INST_SZ) == Cst(btype(INTEGER,?,?));Type(INSTRUCTION) == Cst(SetOf(btype(INTEGER,"[INSTRUCTION","]INSTRUCTION")));Type(NB_INSTRUCTIONS) == Cst(btype(INTEGER,?,?));Type(INSTRUCTION_MAX) == Cst(btype(INTEGER,?,?));Type(INSTRUCTION_NEXT) == Cst(SetOf(btype(INTEGER,"[INSTRUCTION","]INSTRUCTION")*btype(INTEGER,"[INSTRUCTION","]INSTRUCTION")));Type(BV_TO_WORD) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*btype(INTEGER,"[WORD","]WORD")));Type(WORD_TO_BV) == Cst(SetOf(btype(INTEGER,"[WORD","]WORD")*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(REGISTER) == Cst(SetOf(btype(INTEGER,"[REGISTER","]REGISTER"))));
  List_Constants(Implementation(traffic_light_asm_pic),Machine(data_refinement))==(color_refine,color_step);
  List_Constants_Env(Implementation(traffic_light_asm_pic),Machine(data_refinement))==(Type(color_refine) == Cst(SetOf(etype(COLOR,0,2)*btype(INTEGER,?,?)));Type(color_step) == Cst(SetOf(btype(INTEGER,0,2)*btype(INTEGER,0,2))));
  List_Constants_Env(Implementation(traffic_light_asm_pic),Machine(definitions))==(Type(green) == Cst(etype(COLOR,0,2));Type(yellow) == Cst(etype(COLOR,0,2));Type(red) == Cst(etype(COLOR,0,2)));
  Enumerate_Definition(Implementation(traffic_light_asm_pic),Machine(definitions),COLOR)==({green,yellow,red})
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(traffic_light_asm_pic)) == (? | ? | ? | ? | advance | ? | seen(Machine(definitions)),seen(Machine(data_refinement)),seen(Machine(TYPES)),imported(Machine(m.PIC)) | ? | traffic_light_asm_pic);
  List_Of_HiddenCst_Ids(Implementation(traffic_light_asm_pic)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(traffic_light_asm_pic)) == (?);
  List_Of_VisibleVar_Ids(Implementation(traffic_light_asm_pic)) == (? | mmem,mw,mz,mc,mpc,mstack,msp);
  List_Of_Ids_SeenBNU(Implementation(traffic_light_asm_pic)) == (seen(Machine(TYPES)) : (WORD_LENGTH,WORD,WORD_POSITION,NB_WORDS,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,BV_TO_WORD,WORD_TO_BV,REGISTER | BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(ALU)) : (add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(PIC)) == (? | ? | ? | ? | CALL,RETURN,RETLW,ADDLW,ADDWF,SUBLW,SUBWF,ANDLW,ANDLWF,IORLW,IORLWF,XORLW,XORLWF,BCF,BSF,BTFSC,BTFSS,CLRF,CLRW,COMF,DECF,DECFSZ,GOTO,INCF,INCFSZ,MOVLW,MOVF,MOVWF,NOP,ROLF,RORF,SWAP | ? | seen(Machine(TYPES)),seen(Machine(ALU)) | ? | PIC);
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
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (WORD_LENGTH,WORD,WORD_POSITION,NB_WORDS,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,BV_TO_WORD,WORD_TO_BV,REGISTER,BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR);
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
  List_Of_Ids_SeenBNU(Machine(TYPE_BIT)) == (? : ?);
  List_Of_Ids(Machine(data_refinement)) == (color_refine,color_step | ? | ? | ? | ? | ? | seen(Machine(definitions)) | ? | data_refinement);
  List_Of_HiddenCst_Ids(Machine(data_refinement)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(data_refinement)) == (color_refine,color_step);
  List_Of_VisibleVar_Ids(Machine(data_refinement)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(data_refinement)) == (? : ?);
  List_Of_Ids(Machine(definitions)) == (COLOR,green,yellow,red | ? | ? | ? | ? | ? | ? | ? | definitions);
  List_Of_HiddenCst_Ids(Machine(definitions)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(definitions)) == (?);
  List_Of_VisibleVar_Ids(Machine(definitions)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(definitions)) == (? : ?)
END
&
THEORY TCIntRdX IS
  predB0 == OK;
  extended_sees == KO;
  B0check_tab == KO;
  local_op == OK;
  abstract_constants_visible_in_values == KO
END
&
THEORY ListLocalOperationsX IS
  List_Local_Operations(Implementation(traffic_light_asm_pic))==(?)
END
&
THEORY ListLocalInputX END
&
THEORY ListLocalOutputX END
&
THEORY ListLocalHeaderX END
&
THEORY ListLocalPreconditionX END
&
THEORY ListLocalSubstitutionX END
&
THEORY TypingPredicateX IS
  TypingPredicate(Implementation(traffic_light_asm_pic))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(traffic_light_asm_pic),Machine(m.PIC))==(?);
  ImportedVisVariablesList(Implementation(traffic_light_asm_pic),Machine(m.PIC))==(m.mem,m.w,m.z,m.c,m.pc,m.stack,m.sp)
END
&
THEORY ListLocalOpInvariantX END
)
