Normalised(
THEORY MagicNumberX IS
  MagicNumber(Implementation(traffic_light_asm_z80))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Implementation(traffic_light_asm_z80))==(Machine(traffic_light));
  Level(Implementation(traffic_light_asm_z80))==(2);
  Upper_Level(Implementation(traffic_light_asm_z80))==(Refinement(traffic_light_data_refinement))
END
&
THEORY LoadedStructureX IS
  Implementation(traffic_light_asm_z80)
END
&
THEORY ListSeesX IS
  List_Sees(Implementation(traffic_light_asm_z80))==(definitions,TYPES,ALU,data_refinement)
END
&
THEORY ListIncludesX IS
  List_Includes(Implementation(traffic_light_asm_z80))==(mp.Z80);
  Inherited_List_Includes(Implementation(traffic_light_asm_z80))==(mp.Z80)
END
&
THEORY ListPromotesX IS
  List_Promotes(Implementation(traffic_light_asm_z80))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Implementation(traffic_light_asm_z80))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Implementation(traffic_light_asm_z80))==(?);
  Context_List_Variables(Implementation(traffic_light_asm_z80))==(?);
  Abstract_List_Variables(Implementation(traffic_light_asm_z80))==(counter,color);
  Local_List_Variables(Implementation(traffic_light_asm_z80))==(?);
  List_Variables(Implementation(traffic_light_asm_z80))==(?);
  External_List_Variables(Implementation(traffic_light_asm_z80))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Implementation(traffic_light_asm_z80))==(?);
  Abstract_List_VisibleVariables(Implementation(traffic_light_asm_z80))==(?);
  External_List_VisibleVariables(Implementation(traffic_light_asm_z80))==(mp.stack,mp.iy,mp.ix,mp.sp,mp.pc,mp.mem,mp.rgs8);
  Expanded_List_VisibleVariables(Implementation(traffic_light_asm_z80))==(mpstack,mpiy,mpix,mpsp,mppc,mpmem,mprgs8);
  List_VisibleVariables(Implementation(traffic_light_asm_z80))==(?);
  Internal_List_VisibleVariables(Implementation(traffic_light_asm_z80))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Implementation(traffic_light_asm_z80))==(btrue);
  Abstract_List_Invariant(Implementation(traffic_light_asm_z80))==(counter : NATURAL & counter : 0..2 & counter = color_refine(color) & color : COLOR);
  Expanded_List_Invariant(Implementation(traffic_light_asm_z80))==(mpstack : WORD16 --> WORD8 & mprgs8 : id_register8 --> WORD8 & mpmem : WORD16 --> WORD8 & mppc : WORD16 & mpsp : WORD16 & mpix : WORD16 & mpiy : WORD16);
  Context_List_Invariant(Implementation(traffic_light_asm_z80))==(btrue);
  List_Invariant(Implementation(traffic_light_asm_z80))==(mpmem(0) : 0..2 & mpmem(0) = counter)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Implementation(traffic_light_asm_z80))==(btrue);
  Expanded_List_Assertions(Implementation(traffic_light_asm_z80))==(dom(mpstack) = WORD16 & ran(mpstack) <: WORD8 & ran(mpmem) <: WORD8 & dom(mpmem) = WORD16 & ran(mprgs8) <: WORD8 & dom(mprgs8) = id_register8 & INSTRUCTION_NEXT(0) = 1 & INSTRUCTION_NEXT(1) = 2 & INSTRUCTION_NEXT(2) = 3 & INSTRUCTION_NEXT(3) = 4 & INSTRUCTION_NEXT(4) = 5 & INSTRUCTION_NEXT(5) = 6 & INSTRUCTION_NEXT(6) = 7 & INSTRUCTION_NEXT(7) = 8 & INSTRUCTION_NEXT(8) = 9 & INSTRUCTION_NEXT(9) = 10);
  Context_List_Assertions(Implementation(traffic_light_asm_z80))==(!c.(c : COLOR => c = green or c = yellow or c = red) & NB_WORD8S = 256 & !n.(n : WORD8 => 0<=n) & !n.(n : WORD8 => n<=255) & WORD8_POSITION = BV8_INDEX & BIT_FLIP(0) = 1 & BIT_FLIP(1) = 0 & BIT_AND(0,0) = 0 & BIT_AND(0,1) = 0 & BIT_AND(1,0) = 0 & BIT_AND(1,1) = 1 & BIT_IOR(0,0) = 0 & BIT_IOR(0,1) = 0 & BIT_IOR(1,0) = 0 & BIT_IOR(1,1) = 1 & BIT_XOR(0,0) = 0 & BIT_XOR(0,1) = 1 & BIT_XOR(1,0) = 1 & BIT_XOR(1,1) = 0 & dom(add) = WORD8*WORD8 & ran(add) <: WORD8*BOOL*BOOL & dom(substract) = WORD8*WORD8 & ran(substract) <: WORD8*BOOL*BOOL & dom(and) = WORD8*WORD8 & ran(and) <: WORD8*BOOL & dom(ior) = WORD8*WORD8 & ran(ior) <: WORD8*BOOL & dom(xor) = WORD8*WORD8 & ran(xor) <: WORD8*BOOL & dom(bitclear) = WORD8*WORD8_POSITION & ran(bitclear) <: WORD8 & dom(bitset) = WORD8*WORD8_POSITION & ran(bitset) <: WORD8 & dom(bitget) = WORD8*WORD8_POSITION & ran(bitget) <: BIT & dom(complement) = WORD8 & ran(complement) <: WORD8 & dom(swap) = WORD8 & ran(swap) <: WORD8 & ran(rotateleft) <: WORD8*BOOL & dom(rotateleft) = WORD8 & dom(rotateright) = WORD8 & ran(rotateright) <: WORD8*BOOL & color_refine(green) = 0 & color_refine(yellow) = 1 & color_refine(red) = 2 & color_step(0) = 1 & color_step(1) = 2 & color_step(2) = 0);
  List_Assertions(Implementation(traffic_light_asm_z80))==(INSTRUCTION_NEXT(0) = 1 & INSTRUCTION_NEXT(1) = 2 & INSTRUCTION_NEXT(2) = 3 & INSTRUCTION_NEXT(3) = 4 & INSTRUCTION_NEXT(4) = 5 & INSTRUCTION_NEXT(5) = 6 & INSTRUCTION_NEXT(6) = 7 & INSTRUCTION_NEXT(7) = 8 & INSTRUCTION_NEXT(8) = 9 & INSTRUCTION_NEXT(9) = 10 & INSTRUCTION_NEXT(10) = 11 & INSTRUCTION_NEXT(11) = 12 & INSTRUCTION_NEXT(12) = 13 & INSTRUCTION_NEXT(13) = 14)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Implementation(traffic_light_asm_z80))==(@(rgs8$0).(rgs8$0 : id_register8 -->> WORD8 ==> mprgs8:=rgs8$0) || @(mem$0).(mem$0 : WORD16 --> WORD8 ==> mpmem:=mem$0) || @(pc$0).(pc$0 : WORD16 ==> mppc:=pc$0) || @(sp$0).(sp$0 : WORD16 ==> mpsp:=sp$0) || @(ix$0).(ix$0 : WORD16 ==> mpix:=ix$0) || @(iy$0).(iy$0 : WORD16 ==> mpiy:=iy$0) || @(stack$0).(stack$0 : WORD16 --> WORD8 ==> mpstack:=stack$0);(h : id_register8 & 0 : WORD8 | mprgs8,mppc:=mprgs8<+{h|->0},INSTRUCTION_NEXT(mppc));(l : id_register8 & 0 : WORD8 | mprgs8,mppc:=mprgs8<+{l|->0},INSTRUCTION_NEXT(mppc));(h : id_register8 | @address.(address : WORD16 & address = WORD8_TO_WORD16(mprgs8(h),mprgs8(l)) ==> mpmem,mppc:=mpmem<+{address|->mprgs8(h)},INSTRUCTION_NEXT(mppc))));
  Context_List_Initialisation(Implementation(traffic_light_asm_z80))==(skip);
  List_Initialisation(Implementation(traffic_light_asm_z80))==((mp.LD_r_n_)(h,0);(mp.LD_r_n_)(l,0);(mp.LD_HL_r)(h))
END
&
THEORY ListParametersX IS
  List_Parameters(Implementation(traffic_light_asm_z80))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Implementation(traffic_light_asm_z80),Machine(mp.Z80))==(?);
  List_Instanciated_Parameters(Implementation(traffic_light_asm_z80),Machine(definitions))==(?);
  List_Instanciated_Parameters(Implementation(traffic_light_asm_z80),Machine(TYPES))==(?);
  List_Instanciated_Parameters(Implementation(traffic_light_asm_z80),Machine(ALU))==(?);
  List_Instanciated_Parameters(Implementation(traffic_light_asm_z80),Machine(data_refinement))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Implementation(traffic_light_asm_z80),Machine(mp.Z80))==(btrue);
  List_Constraints(Implementation(traffic_light_asm_z80))==(btrue);
  List_Context_Constraints(Implementation(traffic_light_asm_z80))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Implementation(traffic_light_asm_z80))==(advance);
  List_Operations(Implementation(traffic_light_asm_z80))==(advance)
END
&
THEORY ListInputX IS
  List_Input(Implementation(traffic_light_asm_z80),advance)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Implementation(traffic_light_asm_z80),advance)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Implementation(traffic_light_asm_z80),advance)==(advance)
END
&
THEORY ListPreconditionX IS
  Own_Precondition(Implementation(traffic_light_asm_z80),advance)==(btrue);
  List_Precondition(Implementation(traffic_light_asm_z80),advance)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Implementation(traffic_light_asm_z80),advance)==(btrue | (0 : INSTRUCTION | mpstack,mpsp,mppc:=mpstack<+{mpsp|->INSTRUCTION_NEXT(mppc)},mpsp+1,0);WHILE mppc<14 DO not(mppc = 14) & not(mppc = 13) & not(mppc = 12) & not(mppc = 11) & not(mppc = 10) & not(mppc = 9) & not(mppc = 8) & not(mppc = 7) & not(mppc = 6) & not(mppc = 5) & not(mppc = 4) & not(mppc = 3) & not(mppc = 2) & not(mppc = 1) & mppc = 0 ==> (h : id_register8 & 0 : WORD8 | mprgs8,mppc:=mprgs8<+{h|->0},INSTRUCTION_NEXT(mppc)) [] not(mppc = 0) & not(mppc = 14) & not(mppc = 13) & not(mppc = 12) & not(mppc = 11) & not(mppc = 10) & not(mppc = 9) & not(mppc = 8) & not(mppc = 7) & not(mppc = 6) & not(mppc = 5) & not(mppc = 4) & not(mppc = 3) & not(mppc = 2) & mppc = 1 ==> (l : id_register8 & 0 : WORD8 | mprgs8,mppc:=mprgs8<+{l|->0},INSTRUCTION_NEXT(mppc)) [] not(mppc = 0) & not(mppc = 1) & not(mppc = 14) & not(mppc = 13) & not(mppc = 12) & not(mppc = 11) & not(mppc = 10) & not(mppc = 9) & not(mppc = 8) & not(mppc = 7) & not(mppc = 6) & not(mppc = 5) & not(mppc = 4) & not(mppc = 3) & mppc = 2 ==> (1 : 0..WORD8_LENGTH-1 | @(address,nw8,ib).(address : WORD16 & ib : BIT & nw8 : BV8 & address = WORD8_TO_WORD16(mprgs8(h),mprgs8(l)) & ib = WORD8_TO_BV8(mpmem(address))(1) & nw8 = BV8_SET_BIT(WORD8_TO_BV8(mprgs8(f_)),6,ib) ==> mprgs8,mppc:=mprgs8<+{f_|->BV8_TO_WORD8(nw8)},INSTRUCTION_NEXT(mppc))) [] not(mppc = 0) & not(mppc = 1) & not(mppc = 2) & not(mppc = 14) & not(mppc = 13) & not(mppc = 12) & not(mppc = 11) & not(mppc = 10) & not(mppc = 9) & not(mppc = 8) & not(mppc = 7) & not(mppc = 6) & not(mppc = 5) & not(mppc = 4) & mppc = 3 ==> (btrue | WORD8_TO_BV8(mprgs8(f_))(6) = 0 ==> mppc:=INSTRUCTION_NEXT(mppc) [] not(WORD8_TO_BV8(mprgs8(f_))(6) = 0) ==> mppc:=INSTRUCTION_NEXT(INSTRUCTION_NEXT(mppc))) [] not(mppc = 0) & not(mppc = 1) & not(mppc = 2) & not(mppc = 3) & not(mppc = 14) & not(mppc = 13) & not(mppc = 12) & not(mppc = 11) & not(mppc = 10) & not(mppc = 9) & not(mppc = 8) & not(mppc = 7) & not(mppc = 6) & not(mppc = 5) & mppc = 4 ==> (7 : INSTRUCTION | mppc:=7) [] not(mppc = 0) & not(mppc = 1) & not(mppc = 2) & not(mppc = 3) & not(mppc = 4) & not(mppc = 14) & not(mppc = 13) & not(mppc = 12) & not(mppc = 11) & not(mppc = 10) & not(mppc = 9) & not(mppc = 8) & not(mppc = 7) & not(mppc = 6) & mppc = 5 ==> (a : id_register8 & 0 : WORD8 | mprgs8,mppc:=mprgs8<+{a|->0},INSTRUCTION_NEXT(mppc)) [] not(mppc = 0) & not(mppc = 1) & not(mppc = 2) & not(mppc = 3) & not(mppc = 4) & not(mppc = 5) & not(mppc = 14) & not(mppc = 13) & not(mppc = 12) & not(mppc = 11) & not(mppc = 10) & not(mppc = 9) & not(mppc = 8) & not(mppc = 7) & mppc = 6 ==> (14 : INSTRUCTION | mppc:=14) [] not(mppc = 0) & not(mppc = 1) & not(mppc = 2) & not(mppc = 3) & not(mppc = 4) & not(mppc = 5) & not(mppc = 6) & not(mppc = 14) & not(mppc = 13) & not(mppc = 12) & not(mppc = 11) & not(mppc = 10) & not(mppc = 9) & not(mppc = 8) & mppc = 7 ==> (0 : 0..WORD8_LENGTH-1 | @(address,nw8,ib).(address : WORD16 & ib : BIT & nw8 : BV8 & address = WORD8_TO_WORD16(mprgs8(h),mprgs8(l)) & ib = WORD8_TO_BV8(mpmem(address))(0) & nw8 = BV8_SET_BIT(WORD8_TO_BV8(mprgs8(f_)),6,ib) ==> mprgs8,mppc:=mprgs8<+{f_|->BV8_TO_WORD8(nw8)},INSTRUCTION_NEXT(mppc))) [] not(mppc = 0) & not(mppc = 1) & not(mppc = 2) & not(mppc = 3) & not(mppc = 4) & not(mppc = 5) & not(mppc = 6) & not(mppc = 7) & not(mppc = 14) & not(mppc = 13) & not(mppc = 12) & not(mppc = 11) & not(mppc = 10) & not(mppc = 9) & mppc = 8 ==> (btrue | WORD8_TO_BV8(mprgs8(f_))(6) = 0 ==> mppc:=INSTRUCTION_NEXT(mppc) [] not(WORD8_TO_BV8(mprgs8(f_))(6) = 0) ==> mppc:=INSTRUCTION_NEXT(INSTRUCTION_NEXT(mppc))) [] not(mppc = 0) & not(mppc = 1) & not(mppc = 2) & not(mppc = 3) & not(mppc = 4) & not(mppc = 5) & not(mppc = 6) & not(mppc = 7) & not(mppc = 8) & not(mppc = 14) & not(mppc = 13) & not(mppc = 12) & not(mppc = 11) & not(mppc = 10) & mppc = 9 ==> (11 : INSTRUCTION | mppc:=11) [] not(mppc = 0) & not(mppc = 1) & not(mppc = 2) & not(mppc = 3) & not(mppc = 4) & not(mppc = 5) & not(mppc = 6) & not(mppc = 7) & not(mppc = 8) & not(mppc = 9) & not(mppc = 14) & not(mppc = 13) & not(mppc = 12) & not(mppc = 11) & mppc = 10 ==> (13 : INSTRUCTION | mppc:=13) [] not(mppc = 0) & not(mppc = 1) & not(mppc = 2) & not(mppc = 3) & not(mppc = 4) & not(mppc = 5) & not(mppc = 6) & not(mppc = 7) & not(mppc = 8) & not(mppc = 9) & not(mppc = 10) & not(mppc = 14) & not(mppc = 13) & not(mppc = 12) & mppc = 11 ==> (a : id_register8 & 1 : WORD8 | mprgs8,mppc:=mprgs8<+{a|->1},INSTRUCTION_NEXT(mppc)) [] not(mppc = 0) & not(mppc = 1) & not(mppc = 2) & not(mppc = 3) & not(mppc = 4) & not(mppc = 5) & not(mppc = 6) & not(mppc = 7) & not(mppc = 8) & not(mppc = 9) & not(mppc = 10) & not(mppc = 11) & not(mppc = 14) & not(mppc = 13) & mppc = 12 ==> (14 : INSTRUCTION | mppc:=14) [] not(mppc = 0) & not(mppc = 1) & not(mppc = 2) & not(mppc = 3) & not(mppc = 4) & not(mppc = 5) & not(mppc = 6) & not(mppc = 7) & not(mppc = 8) & not(mppc = 9) & not(mppc = 10) & not(mppc = 11) & not(mppc = 12) & not(mppc = 14) & mppc = 13 ==> (a : id_register8 & 2 : WORD8 | mprgs8,mppc:=mprgs8<+{a|->2},INSTRUCTION_NEXT(mppc)) [] not(mppc = 0) & not(mppc = 1) & not(mppc = 2) & not(mppc = 3) & not(mppc = 4) & not(mppc = 5) & not(mppc = 6) & not(mppc = 7) & not(mppc = 8) & not(mppc = 9) & not(mppc = 10) & not(mppc = 11) & not(mppc = 12) & not(mppc = 13) & mppc = 14 ==> (a : id_register8 | @address.(address : WORD16 & address = WORD8_TO_WORD16(mprgs8(h),mprgs8(l)) ==> mpmem,mppc:=mpmem<+{address|->mprgs8(a)},INSTRUCTION_NEXT(mppc))) [] not(mppc = 0) & not(mppc = 1) & not(mppc = 2) & not(mppc = 3) & not(mppc = 4) & not(mppc = 5) & not(mppc = 6) & not(mppc = 7) & not(mppc = 8) & not(mppc = 9) & not(mppc = 10) & not(mppc = 11) & not(mppc = 12) & not(mppc = 13) & not(mppc = 14) ==> skip INVARIANT 0<=mppc & mppc<=14 & mpsp>0 & mpmem(0) : 0..2 & (mppc = 0 => mpmem(0) : 0..2 & mpmem(0) = counter & mprgs8(h) = 0) & (mppc = 1 => mprgs8(l) = 0) & (mppc = 2 => bitget(mpmem(0),1) = bitget(mprgs8(f_),1)) & (mppc = 10 => mpmem(0) : 0..2 & mpmem(0) = color_step(counter)) VARIANT 14-mppc END;(mpsp>0 | mpstack,mppc,mpsp:={mpsp-1}<<|mpstack,mpstack(mpsp-1),mpsp-1));
  List_Substitution(Implementation(traffic_light_asm_z80),advance)==((mp.CALL)(0);WHILE mp.pc<14 DO BEGIN CASE mp.pc OF EITHER 0 THEN (mp.LD_r_n_)(h,0) OR 1 THEN (mp.LD_r_n_)(l,0) OR 2 THEN (mp.BIT_b_HL)(1) OR 3 THEN mp.JP_z_e OR 4 THEN (mp.GOTO)(7) OR 5 THEN (mp.LD_r_n_)(a,0) OR 6 THEN (mp.GOTO)(14) OR 7 THEN (mp.BIT_b_HL)(0) OR 8 THEN mp.JP_z_e OR 9 THEN (mp.GOTO)(11) OR 10 THEN (mp.GOTO)(13) OR 11 THEN (mp.LD_r_n_)(a,1) OR 12 THEN (mp.GOTO)(14) OR 13 THEN (mp.LD_r_n_)(a,2) OR 14 THEN (mp.LD_HL_r)(a) END END END INVARIANT 0<=mp.pc & mp.pc<=14 & mp.sp>0 & (mp.mem)(0) : 0..2 & (mp.pc = 0 => (mp.mem)(0) : 0..2 & (mp.mem)(0) = counter & (mp.rgs8)(h) = 0) & (mp.pc = 1 => (mp.rgs8)(l) = 0) & (mp.pc = 2 => bitget((mp.mem)(0),1) = bitget((mp.rgs8)(f_),1)) & (mp.pc = 10 => (mp.mem)(0) : 0..2 & (mp.mem)(0) = color_step(counter)) VARIANT 14-mp.pc END;mp.RETURN)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Implementation(traffic_light_asm_z80))==(?);
  Inherited_List_Constants(Implementation(traffic_light_asm_z80))==(?);
  List_Constants(Implementation(traffic_light_asm_z80))==(?)
END
&
THEORY ListSetsX IS
  Set_Definition(Implementation(traffic_light_asm_z80),id_register8)==({a,f,f_,a_,b,c,b_,c_,d,e,d_,e_,h,l,h_,l_,i,r});
  Context_List_Enumerated(Implementation(traffic_light_asm_z80))==(COLOR);
  Context_List_Defered(Implementation(traffic_light_asm_z80))==(?);
  Context_List_Sets(Implementation(traffic_light_asm_z80))==(COLOR);
  List_Own_Enumerated(Implementation(traffic_light_asm_z80))==(?);
  List_Valuable_Sets(Implementation(traffic_light_asm_z80))==(?);
  Inherited_List_Enumerated(Implementation(traffic_light_asm_z80))==(id_register8);
  Inherited_List_Defered(Implementation(traffic_light_asm_z80))==(?);
  Inherited_List_Sets(Implementation(traffic_light_asm_z80))==(id_register8);
  List_Enumerated(Implementation(traffic_light_asm_z80))==(?);
  List_Defered(Implementation(traffic_light_asm_z80))==(?);
  List_Sets(Implementation(traffic_light_asm_z80))==(?);
  Set_Definition(Implementation(traffic_light_asm_z80),COLOR)==({green,yellow,red})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Implementation(traffic_light_asm_z80))==(?);
  Expanded_List_HiddenConstants(Implementation(traffic_light_asm_z80))==(?);
  List_HiddenConstants(Implementation(traffic_light_asm_z80))==(?);
  External_List_HiddenConstants(Implementation(traffic_light_asm_z80))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Implementation(traffic_light_asm_z80))==(btrue);
  Context_List_Properties(Implementation(traffic_light_asm_z80))==(not(COLOR = {}) & WORD16_LENGTH : NATURAL & WORD16_LENGTH = 16 & NB_WORD16S : NATURAL & NB_WORD16S = 2**WORD16_LENGTH & WORD16 = 0..NB_WORD16S-1 & WORD16_POSITION = 0..WORD16_LENGTH-1 & WORD8_LENGTH : NATURAL & WORD8_LENGTH = 8 & NB_WORD8S : NATURAL & NB_WORD8S = 2**WORD8_LENGTH & WORD8 = 0..NB_WORD8S-1 & WORD8_POSITION = 0..WORD8_LENGTH-1 & NB_INSTRUCTIONS : NATURAL & INST_SZ : NATURAL & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = 0..INSTRUCTION_MAX & INSTRUCTION_NEXT : INSTRUCTION --> INSTRUCTION & INSTRUCTION_NEXT = {p,q | p : INSTRUCTION & q : INSTRUCTION & 0<=p & p<NB_INSTRUCTIONS-1 & q = p+1}\/{NB_INSTRUCTIONS-1|->0} & BV8_TO_WORD8 : BV8 --> WORD8 & BV8_TO_WORD8 = %v.(v : BV8 | 128*v(7)+64*v(6)+32*v(5)+16*v(4)+8*v(3)+4*v(2)+2*v(1)+v(0)) & WORD8_TO_BV8 : WORD8 --> BV8 & WORD8_TO_BV8 = BV8_TO_WORD8~ & BV16_TO_WORD16 : BV16 --> WORD16 & BV16_TO_WORD16 = %v.(v : BV16 | 32768*v(15)+16384*v(14)+8192*v(13)+4096*v(12)+2048*v(11)+1024*v(10)+512*v(9)+256*v(8)+128*v(7)+64*v(6)+32*v(5)+16*v(4)+8*v(3)+4*v(2)+2*v(1)+v(0)) & WORD16_TO_BV16 : WORD16 --> BV16 & WORD16_TO_BV16 = BV16_TO_WORD16~ & BV8_TO_BV16 : BV8*BV8 --> BV16 & BV8_TO_BV16 = %(v1,v2).(v1 : BV8 & v2 : BV8 | {0|->v1(0),1|->v1(1),2|->v1(2),3|->v1(3),4|->v1(4),5|->v1(5),6|->v1(6),7|->v1(7),8|->v2(0),9|->v2(1),10|->v2(2),11|->v2(3),12|->v2(4),13|->v2(5),14|->v2(6),15|->v2(7)}) & WORD8_TO_WORD16 : WORD8*WORD8 --> WORD16 & WORD8_TO_WORD16 = %(w1,w2).(w1 : WORD8 & w2 : WORD8 | BV16_TO_WORD16(BV8_TO_BV16(WORD8_TO_BV8(w1),WORD8_TO_BV8(w2)))) & WORD16_TO_WORD8 : WORD16 --> WORD8*WORD8 & WORD16_TO_WORD8 = WORD8_TO_WORD16~ & BIT = 0..1 & BIT_FLIP : BIT --> BIT & !b.(b : BIT => BIT_FLIP(b) = 1-b) & BIT_AND : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_AND(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & BIT_IOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_IOR(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & BIT_XOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_XOR(b1,b2) = 1 <=> (b1 = 1 & b2 = 0 or (b1 = 0 & b2 = 1))) & BV8_INDEX = 0..7 & BV8 = BV8_INDEX --> BIT & BV8_SET_BIT : BV8*BV8_INDEX*BIT --> BV8 & !(v,i,j,b).(v : BV8 & i : BV8_INDEX & j : BV8_INDEX & b : BIT => (i/=j => BV8_SET_BIT(v,i,b)(j) = v(j))) & !(v,i,b).(v : BV8 & i : BV8_INDEX & b : BIT => BV8_SET_BIT(v,i,b)(i) = b) & BV8_COMPLEMENT : BV8 --> BV8 & !(v,i).(v : BV8 & i : BV8_INDEX => BV8_COMPLEMENT(v)(i) = BIT_FLIP(v(i))) & BV8_ALL_ZEROES : BV8 & !i.(i : BV8_INDEX => BV8_ALL_ZEROES(i) = 0) & BV8_AND : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_AND(v1,v2)(i) = BIT_AND(v1(i),v2(i))) & BV8_IOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_IOR(v1,v2)(i) = BIT_IOR(v1(i),v2(i))) & BV8_XOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_XOR(v1,v2)(i) = BIT_XOR(v1(i),v2(i))) & BV16_INDEX = 0..15 & BV16 = BV16_INDEX --> BIT & BV16_SET_BIT : BV16*BV16_INDEX*BIT --> BV16 & !(v,i,j,b).(v : BV16 & i : BV16_INDEX & j : BV16_INDEX & b : BIT => (i/=j => BV16_SET_BIT(v,i,b)(j) = v(j))) & !(v,i,b).(v : BV16 & i : BV16_INDEX & b : BIT => BV16_SET_BIT(v,i,b)(i) = b) & BV16_COMPLEMENT : BV16 --> BV16 & !(v,i).(v : BV16 & i : BV16_INDEX => BV16_COMPLEMENT(v)(i) = BIT_FLIP(v(i))) & BV16_ALL_ZEROES : BV16 & !i.(i : BV16_INDEX => BV16_ALL_ZEROES(i) = 0) & BV16_AND : BV16*BV16 --> BV16 & !(v1,v2,i).(v1 : BV16 & v2 : BV16 & i : BV16_INDEX => BV16_AND(v1,v2)(i) = BIT_AND(v1(i),v2(i))) & BV16_IOR : BV16*BV16 --> BV16 & !(v1,v2,i).(v1 : BV16 & v2 : BV16 & i : BV16_INDEX => BV16_IOR(v1,v2)(i) = BIT_IOR(v1(i),v2(i))) & BV16_XOR : BV16*BV16 --> BV16 & !(v1,v2,i).(v1 : BV16 & v2 : BV16 & i : BV16_INDEX => BV16_XOR(v1,v2)(i) = BIT_XOR(v1(i),v2(i))) & add16 : WORD16*WORD16 --> WORD16*BOOL*BOOL & !(w1,w2,sum).(w1 : WORD16 & w2 : WORD16 & sum : NATURAL & sum = w1+w2 => (sum<=65535 => add16(w1,w2) = sum,bool(sum = 0),FALSE) & (65536<=sum => add16(w1,w2) = sum-65536,bool(sum = 65536),TRUE)) & add : WORD8*WORD8 --> WORD8*BOOL*BOOL & !(w1,w2,sum).(w1 : WORD8 & w2 : WORD8 & sum : NATURAL & sum = w1+w2 => (sum<=255 => add(w1,w2) = sum,bool(sum = 0),FALSE) & (256<=sum => add(w1,w2) = sum-256,bool(sum = 256),TRUE)) & substract : WORD8*WORD8 --> WORD8*BOOL*BOOL & !(w1,w2,diff).(w1 : WORD8 & w2 : WORD8 & diff : INTEGER & diff = w1-w2 => (diff<0 => substract(w1,w2) = diff+256,FALSE,TRUE) & (diff>=0 => substract(w1,w2) = diff,bool(diff = 0),FALSE)) & and : WORD8*WORD8 --> WORD8*BOOL & !(w1,w2,w).(w1 : WORD8 & w2 : WORD8 & w : WORD8 & w = BV8_TO_WORD8(BV8_AND(WORD8_TO_BV8(w1),WORD8_TO_BV8(w2))) => and(w1,w2) = w,bool(w = 0)) & ior : WORD8*WORD8 --> WORD8*BOOL & !(w1,w2,w).(w1 : WORD8 & w2 : WORD8 & w : WORD8 & w = BV8_TO_WORD8(BV8_IOR(WORD8_TO_BV8(w1),WORD8_TO_BV8(w2))) => ior(w1,w2) = w,bool(w = 0)) & xor : WORD8*WORD8 --> WORD8*BOOL & !(w1,w2,w).(w1 : WORD8 & w2 : WORD8 & w : WORD8 & w = BV8_TO_WORD8(BV8_XOR(WORD8_TO_BV8(w1),WORD8_TO_BV8(w2))) => xor(w1,w2) = w,bool(w = 0)) & bitget : WORD8*WORD8_POSITION --> BIT & !(w,i).(w : WORD8 & i : WORD8_POSITION => bitget(w,i) = WORD8_TO_BV8(w)(i)) & bitset : WORD8*WORD8_POSITION --> WORD8 & !(w,i).(w : WORD8 & i : WORD8_POSITION => bitset(w,i) = BV8_TO_WORD8(BV8_SET_BIT(WORD8_TO_BV8(w),i,1))) & bitclear : WORD8*WORD8_POSITION --> WORD8 & !(w,i,b).(w : WORD8 & i : WORD8_POSITION & b : BIT => bitclear(w,i) = BV8_TO_WORD8(BV8_SET_BIT(WORD8_TO_BV8(w),i,0))) & complement : WORD8 --> WORD8 & !w.(w : WORD8 => complement(w) = BV8_TO_WORD8(BV8_COMPLEMENT(WORD8_TO_BV8(w)))) & swap : WORD8 --> WORD8 & !(w,v).(w : WORD8 & v : BV8 => (v = WORD8_TO_BV8(w) => swap(w) = BV8_TO_WORD8({0|->v(4),1|->v(5),2|->v(6),3|->v(7),4|->v(0),5|->v(1),6|->v(2),7|->v(3)}))) & rotateleft : WORD8 --> WORD8*BOOL & !(w,v).(w : WORD8 & v : BV8 => (v = WORD8_TO_BV8(w) => rotateleft(w) = BV8_TO_WORD8({0|->v(7),1|->v(0),2|->v(1),3|->v(2),4|->v(3),5|->v(4),6|->v(5),7|->v(6)}),bool(v(7) = 1))) & rotateright : WORD8 --> WORD8*BOOL & !(w,v).(w : WORD8 & v : BV8 => (v = WORD8_TO_BV8(w) => rotateright(w) = BV8_TO_WORD8({0|->v(1),1|->v(2),2|->v(3),3|->v(4),4|->v(5),5|->v(6),6|->v(7),7|->v(0)}),bool(v(0) = 1))) & color_refine : COLOR --> NATURAL & color_refine = {green|->0,yellow|->1,red|->2} & color_step : 0..2 --> 0..2 & color_step = {0|->1,1|->2,2|->0});
  Inherited_List_Properties(Implementation(traffic_light_asm_z80))==(not(id_register8 = {}));
  List_Properties(Implementation(traffic_light_asm_z80))==(btrue)
END
&
THEORY ListValuesX IS
  Values_Subs(Implementation(traffic_light_asm_z80))==(aa : aa);
  List_Values(Implementation(traffic_light_asm_z80))==(?)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Implementation(traffic_light_asm_z80),Machine(data_refinement))==(?);
  Seen_Context_List_Enumerated(Implementation(traffic_light_asm_z80))==(COLOR);
  Seen_Context_List_Invariant(Implementation(traffic_light_asm_z80))==(btrue);
  Seen_Context_List_Assertions(Implementation(traffic_light_asm_z80))==(NB_WORD8S = 256 & !n.(n : WORD8 => 0<=n) & !n.(n : WORD8 => n<=255) & WORD8_POSITION = BV8_INDEX & BIT_FLIP(0) = 1 & BIT_FLIP(1) = 0 & BIT_AND(0,0) = 0 & BIT_AND(0,1) = 0 & BIT_AND(1,0) = 0 & BIT_AND(1,1) = 1 & BIT_IOR(0,0) = 0 & BIT_IOR(0,1) = 0 & BIT_IOR(1,0) = 0 & BIT_IOR(1,1) = 1 & BIT_XOR(0,0) = 0 & BIT_XOR(0,1) = 1 & BIT_XOR(1,0) = 1 & BIT_XOR(1,1) = 0 & !c.(c : COLOR => c = green or c = yellow or c = red));
  Seen_Context_List_Properties(Implementation(traffic_light_asm_z80))==(WORD16_LENGTH : NATURAL & WORD16_LENGTH = 16 & NB_WORD16S : NATURAL & NB_WORD16S = 2**WORD16_LENGTH & WORD16 = 0..NB_WORD16S-1 & WORD16_POSITION = 0..WORD16_LENGTH-1 & WORD8_LENGTH : NATURAL & WORD8_LENGTH = 8 & NB_WORD8S : NATURAL & NB_WORD8S = 2**WORD8_LENGTH & WORD8 = 0..NB_WORD8S-1 & WORD8_POSITION = 0..WORD8_LENGTH-1 & NB_INSTRUCTIONS : NATURAL & INST_SZ : NATURAL & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = 0..INSTRUCTION_MAX & INSTRUCTION_NEXT : INSTRUCTION --> INSTRUCTION & INSTRUCTION_NEXT = {p,q | p : INSTRUCTION & q : INSTRUCTION & 0<=p & p<NB_INSTRUCTIONS-1 & q = p+1}\/{NB_INSTRUCTIONS-1|->0} & BV8_TO_WORD8 : BV8 --> WORD8 & BV8_TO_WORD8 = %v.(v : BV8 | 128*v(7)+64*v(6)+32*v(5)+16*v(4)+8*v(3)+4*v(2)+2*v(1)+v(0)) & WORD8_TO_BV8 : WORD8 --> BV8 & WORD8_TO_BV8 = BV8_TO_WORD8~ & BV16_TO_WORD16 : BV16 --> WORD16 & BV16_TO_WORD16 = %v.(v : BV16 | 32768*v(15)+16384*v(14)+8192*v(13)+4096*v(12)+2048*v(11)+1024*v(10)+512*v(9)+256*v(8)+128*v(7)+64*v(6)+32*v(5)+16*v(4)+8*v(3)+4*v(2)+2*v(1)+v(0)) & WORD16_TO_BV16 : WORD16 --> BV16 & WORD16_TO_BV16 = BV16_TO_WORD16~ & BV8_TO_BV16 : BV8*BV8 --> BV16 & BV8_TO_BV16 = %(v1,v2).(v1 : BV8 & v2 : BV8 | {0|->v1(0),1|->v1(1),2|->v1(2),3|->v1(3),4|->v1(4),5|->v1(5),6|->v1(6),7|->v1(7),8|->v2(0),9|->v2(1),10|->v2(2),11|->v2(3),12|->v2(4),13|->v2(5),14|->v2(6),15|->v2(7)}) & WORD8_TO_WORD16 : WORD8*WORD8 --> WORD16 & WORD8_TO_WORD16 = %(w1,w2).(w1 : WORD8 & w2 : WORD8 | BV16_TO_WORD16(BV8_TO_BV16(WORD8_TO_BV8(w1),WORD8_TO_BV8(w2)))) & WORD16_TO_WORD8 : WORD16 --> WORD8*WORD8 & WORD16_TO_WORD8 = WORD8_TO_WORD16~ & BIT = 0..1 & BIT_FLIP : BIT --> BIT & !b.(b : BIT => BIT_FLIP(b) = 1-b) & BIT_AND : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_AND(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & BIT_IOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_IOR(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & BIT_XOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_XOR(b1,b2) = 1 <=> (b1 = 1 & b2 = 0 or (b1 = 0 & b2 = 1))) & BV8_INDEX = 0..7 & BV8 = BV8_INDEX --> BIT & BV8_SET_BIT : BV8*BV8_INDEX*BIT --> BV8 & !(v,i,j,b).(v : BV8 & i : BV8_INDEX & j : BV8_INDEX & b : BIT => (i/=j => BV8_SET_BIT(v,i,b)(j) = v(j))) & !(v,i,b).(v : BV8 & i : BV8_INDEX & b : BIT => BV8_SET_BIT(v,i,b)(i) = b) & BV8_COMPLEMENT : BV8 --> BV8 & !(v,i).(v : BV8 & i : BV8_INDEX => BV8_COMPLEMENT(v)(i) = BIT_FLIP(v(i))) & BV8_ALL_ZEROES : BV8 & !i.(i : BV8_INDEX => BV8_ALL_ZEROES(i) = 0) & BV8_AND : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_AND(v1,v2)(i) = BIT_AND(v1(i),v2(i))) & BV8_IOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_IOR(v1,v2)(i) = BIT_IOR(v1(i),v2(i))) & BV8_XOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_XOR(v1,v2)(i) = BIT_XOR(v1(i),v2(i))) & BV16_INDEX = 0..15 & BV16 = BV16_INDEX --> BIT & BV16_SET_BIT : BV16*BV16_INDEX*BIT --> BV16 & !(v,i,j,b).(v : BV16 & i : BV16_INDEX & j : BV16_INDEX & b : BIT => (i/=j => BV16_SET_BIT(v,i,b)(j) = v(j))) & !(v,i,b).(v : BV16 & i : BV16_INDEX & b : BIT => BV16_SET_BIT(v,i,b)(i) = b) & BV16_COMPLEMENT : BV16 --> BV16 & !(v,i).(v : BV16 & i : BV16_INDEX => BV16_COMPLEMENT(v)(i) = BIT_FLIP(v(i))) & BV16_ALL_ZEROES : BV16 & !i.(i : BV16_INDEX => BV16_ALL_ZEROES(i) = 0) & BV16_AND : BV16*BV16 --> BV16 & !(v1,v2,i).(v1 : BV16 & v2 : BV16 & i : BV16_INDEX => BV16_AND(v1,v2)(i) = BIT_AND(v1(i),v2(i))) & BV16_IOR : BV16*BV16 --> BV16 & !(v1,v2,i).(v1 : BV16 & v2 : BV16 & i : BV16_INDEX => BV16_IOR(v1,v2)(i) = BIT_IOR(v1(i),v2(i))) & BV16_XOR : BV16*BV16 --> BV16 & !(v1,v2,i).(v1 : BV16 & v2 : BV16 & i : BV16_INDEX => BV16_XOR(v1,v2)(i) = BIT_XOR(v1(i),v2(i))) & not(COLOR = {}));
  Seen_List_Constraints(Implementation(traffic_light_asm_z80))==(btrue);
  Seen_List_Operations(Implementation(traffic_light_asm_z80),Machine(data_refinement))==(?);
  Seen_Expanded_List_Invariant(Implementation(traffic_light_asm_z80),Machine(data_refinement))==(btrue);
  Set_Definition(Implementation(traffic_light_asm_z80),COLOR)==({green,yellow,red});
  Seen_Internal_List_Operations(Implementation(traffic_light_asm_z80),Machine(ALU))==(?);
  Seen_List_Operations(Implementation(traffic_light_asm_z80),Machine(ALU))==(?);
  Seen_Expanded_List_Invariant(Implementation(traffic_light_asm_z80),Machine(ALU))==(btrue);
  Seen_Internal_List_Operations(Implementation(traffic_light_asm_z80),Machine(TYPES))==(?);
  Seen_List_Operations(Implementation(traffic_light_asm_z80),Machine(TYPES))==(?);
  Seen_Expanded_List_Invariant(Implementation(traffic_light_asm_z80),Machine(TYPES))==(btrue);
  Seen_Internal_List_Operations(Implementation(traffic_light_asm_z80),Machine(definitions))==(?);
  Seen_List_Operations(Implementation(traffic_light_asm_z80),Machine(definitions))==(?);
  Seen_Expanded_List_Invariant(Implementation(traffic_light_asm_z80),Machine(definitions))==(btrue)
END
&
THEORY ListIncludedOperationsX IS
  List_Included_Operations(Implementation(traffic_light_asm_z80),Machine(Z80))==(LD_r_r_,LD_r_n_,LD_r_HL,LD_r_9ix_d0,LD_HL_r,BIT_b_HL,JP_z_e,SET_b_HL,RES_b_HL,GOTO,CALL,RETURN)
END
&
THEORY InheritedEnvX IS
  Operations(Implementation(traffic_light_asm_z80))==(Type(advance) == Cst(No_type,No_type))
END
&
THEORY ListVisibleStaticX IS
  List_Constants_Env(Implementation(traffic_light_asm_z80),Machine(Z80))==(Type(a) == Cst(etype(id_register8,0,17));Type(f) == Cst(etype(id_register8,0,17));Type(f_) == Cst(etype(id_register8,0,17));Type(a_) == Cst(etype(id_register8,0,17));Type(b) == Cst(etype(id_register8,0,17));Type(c) == Cst(etype(id_register8,0,17));Type(b_) == Cst(etype(id_register8,0,17));Type(c_) == Cst(etype(id_register8,0,17));Type(d) == Cst(etype(id_register8,0,17));Type(e) == Cst(etype(id_register8,0,17));Type(d_) == Cst(etype(id_register8,0,17));Type(e_) == Cst(etype(id_register8,0,17));Type(h) == Cst(etype(id_register8,0,17));Type(l) == Cst(etype(id_register8,0,17));Type(h_) == Cst(etype(id_register8,0,17));Type(l_) == Cst(etype(id_register8,0,17));Type(i) == Cst(etype(id_register8,0,17));Type(r) == Cst(etype(id_register8,0,17)));
  Enumerate_Definition(Implementation(traffic_light_asm_z80),Machine(Z80),id_register8)==({a,f,f_,a_,b,c,b_,c_,d,e,d_,e_,h,l,h_,l_,i,r});
  List_Constants(Implementation(traffic_light_asm_z80),Machine(data_refinement))==(color_refine,color_step);
  List_Constants_Env(Implementation(traffic_light_asm_z80),Machine(data_refinement))==(Type(color_refine) == Cst(SetOf(etype(COLOR,0,2)*btype(INTEGER,?,?)));Type(color_step) == Cst(SetOf(btype(INTEGER,0,2)*btype(INTEGER,0,2))));
  List_Constants(Implementation(traffic_light_asm_z80),Machine(ALU))==(add16,add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright);
  List_Constants_Env(Implementation(traffic_light_asm_z80),Machine(ALU))==(Type(add16) == Cst(SetOf(btype(INTEGER,"[WORD16","]WORD16")*btype(INTEGER,"[WORD16","]WORD16")*(btype(INTEGER,"[WORD16","]WORD16")*btype(BOOL,0,1)*btype(BOOL,0,1))));Type(add) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8","]WORD8")*(btype(INTEGER,"[WORD8","]WORD8")*btype(BOOL,0,1)*btype(BOOL,0,1))));Type(substract) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8","]WORD8")*(btype(INTEGER,"[WORD8","]WORD8")*btype(BOOL,0,1)*btype(BOOL,0,1))));Type(and) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8","]WORD8")*(btype(INTEGER,"[WORD8","]WORD8")*btype(BOOL,0,1))));Type(ior) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8","]WORD8")*(btype(INTEGER,"[WORD8","]WORD8")*btype(BOOL,0,1))));Type(xor) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8","]WORD8")*(btype(INTEGER,"[WORD8","]WORD8")*btype(BOOL,0,1))));Type(bitclear) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8_POSITION","]WORD8_POSITION")*btype(INTEGER,"[WORD8","]WORD8")));Type(bitset) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8_POSITION","]WORD8_POSITION")*btype(INTEGER,"[WORD8","]WORD8")));Type(bitget) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8_POSITION","]WORD8_POSITION")*btype(INTEGER,"[BIT","]BIT")));Type(complement) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8","]WORD8")));Type(swap) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8","]WORD8")));Type(rotateleft) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*(btype(INTEGER,"[WORD8","]WORD8")*btype(BOOL,0,1))));Type(rotateright) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*(btype(INTEGER,"[WORD8","]WORD8")*btype(BOOL,0,1)))));
  List_Constants(Implementation(traffic_light_asm_z80),Machine(TYPES))==(BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR,BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BV16_INDEX,BV16,BV16_SET_BIT,BV16_COMPLEMENT,BV16_ALL_ZEROES,BV16_AND,BV16_IOR,BV16_XOR,WORD8,WORD8_LENGTH,WORD8_POSITION,NB_WORD8S,BV8_TO_WORD8,WORD8_TO_BV8,WORD16,WORD16_LENGTH,WORD16_POSITION,NB_WORD16S,BV16_TO_WORD16,WORD16_TO_BV16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,BV8_TO_BV16,WORD8_TO_WORD16,WORD16_TO_WORD8);
  List_Constants_Env(Implementation(traffic_light_asm_z80),Machine(TYPES))==(Type(BIT_XOR) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT_IOR) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT_AND) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT_FLIP) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")));Type(BV8_XOR) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_IOR) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_AND) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_ALL_ZEROES) == Cst(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT")));Type(BV8_COMPLEMENT) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_SET_BIT) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT")*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_INDEX) == Cst(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")));Type(BV16_XOR) == Cst(SetOf(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV16_IOR) == Cst(SetOf(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV16_AND) == Cst(SetOf(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV16_ALL_ZEROES) == Cst(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT")));Type(BV16_COMPLEMENT) == Cst(SetOf(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV16_SET_BIT) == Cst(SetOf(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT")*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV16) == Cst(SetOf(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV16_INDEX) == Cst(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")));Type(WORD8) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")));Type(WORD8_LENGTH) == Cst(btype(INTEGER,?,?));Type(WORD8_POSITION) == Cst(SetOf(btype(INTEGER,"[WORD8_POSITION","]WORD8_POSITION")));Type(NB_WORD8S) == Cst(btype(INTEGER,?,?));Type(BV8_TO_WORD8) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*btype(INTEGER,"[WORD8","]WORD8")));Type(WORD8_TO_BV8) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(WORD16) == Cst(SetOf(btype(INTEGER,"[WORD16","]WORD16")));Type(WORD16_LENGTH) == Cst(btype(INTEGER,?,?));Type(WORD16_POSITION) == Cst(SetOf(btype(INTEGER,"[WORD16_POSITION","]WORD16_POSITION")));Type(NB_WORD16S) == Cst(btype(INTEGER,?,?));Type(BV16_TO_WORD16) == Cst(SetOf(SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))*btype(INTEGER,"[WORD16","]WORD16")));Type(WORD16_TO_BV16) == Cst(SetOf(btype(INTEGER,"[WORD16","]WORD16")*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(INST_SZ) == Cst(btype(INTEGER,?,?));Type(INSTRUCTION) == Cst(SetOf(btype(INTEGER,"[INSTRUCTION","]INSTRUCTION")));Type(NB_INSTRUCTIONS) == Cst(btype(INTEGER,?,?));Type(INSTRUCTION_MAX) == Cst(btype(INTEGER,?,?));Type(INSTRUCTION_NEXT) == Cst(SetOf(btype(INTEGER,"[INSTRUCTION","]INSTRUCTION")*btype(INTEGER,"[INSTRUCTION","]INSTRUCTION")));Type(BV8_TO_BV16) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV16_INDEX","]BV16_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(WORD8_TO_WORD16) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD16","]WORD16")));Type(WORD16_TO_WORD8) == Cst(SetOf(btype(INTEGER,"[WORD16","]WORD16")*(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8","]WORD8")))));
  List_Constants_Env(Implementation(traffic_light_asm_z80),Machine(definitions))==(Type(green) == Cst(etype(COLOR,0,2));Type(yellow) == Cst(etype(COLOR,0,2));Type(red) == Cst(etype(COLOR,0,2)));
  Enumerate_Definition(Implementation(traffic_light_asm_z80),Machine(definitions),COLOR)==({green,yellow,red})
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Implementation(traffic_light_asm_z80)) == (? | id_register8,a,f,f_,a_,b,c,b_,c_,d,e,d_,e_,h,l,h_,l_,i,r | ? | ? | advance | ? | seen(Machine(definitions)),seen(Machine(TYPES)),seen(Machine(ALU)),seen(Machine(data_refinement)),imported(Machine(mp.Z80)) | ? | traffic_light_asm_z80);
  List_Of_HiddenCst_Ids(Implementation(traffic_light_asm_z80)) == (? | ?);
  List_Of_VisibleCst_Ids(Implementation(traffic_light_asm_z80)) == (?);
  List_Of_VisibleVar_Ids(Implementation(traffic_light_asm_z80)) == (? | mprgs8,mpmem,mppc,mpsp,mpix,mpiy,mpstack);
  List_Of_Ids_SeenBNU(Implementation(traffic_light_asm_z80)) == (seen(Machine(TYPES)) : (WORD8,WORD8_LENGTH,WORD8_POSITION,NB_WORD8S,BV8_TO_WORD8,WORD8_TO_BV8,WORD16,WORD16_LENGTH,WORD16_POSITION,NB_WORD16S,BV16_TO_WORD16,WORD16_TO_BV16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,BV8_TO_BV16,WORD8_TO_WORD16,WORD16_TO_WORD8 | BV16_INDEX,BV16,BV16_SET_BIT,BV16_COMPLEMENT,BV16_ALL_ZEROES,BV16_AND,BV16_IOR,BV16_XOR,BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(ALU)) : (add16,add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(Z80)) == (id_register8,a,f,f_,a_,b,c,b_,c_,d,e,d_,e_,h,l,h_,l_,i,r | ? | ? | ? | LD_r_r_,LD_r_n_,LD_r_HL,LD_r_9ix_d0,LD_HL_r,BIT_b_HL,JP_z_e,SET_b_HL,RES_b_HL,GOTO,CALL,RETURN | ? | seen(Machine(TYPES)),seen(Machine(ALU)) | ? | Z80);
  List_Of_HiddenCst_Ids(Machine(Z80)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Z80)) == (?);
  List_Of_VisibleVar_Ids(Machine(Z80)) == (stack,iy,ix,sp,pc,mem,rgs8 | ?);
  List_Of_Ids_SeenBNU(Machine(Z80)) == (? : ?);
  List_Of_Ids(Machine(ALU)) == (add16,add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright | ? | ? | ? | ? | ? | seen(Machine(TYPES)) | ? | ALU);
  List_Of_HiddenCst_Ids(Machine(ALU)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ALU)) == (add16,add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright);
  List_Of_VisibleVar_Ids(Machine(ALU)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ALU)) == (? : ?);
  List_Of_Ids(Machine(TYPES)) == (WORD8,WORD8_LENGTH,WORD8_POSITION,NB_WORD8S,BV8_TO_WORD8,WORD8_TO_BV8,WORD16,WORD16_LENGTH,WORD16_POSITION,NB_WORD16S,BV16_TO_WORD16,WORD16_TO_BV16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,BV8_TO_BV16,WORD8_TO_WORD16,WORD16_TO_WORD8 | BV16_INDEX,BV16,BV16_SET_BIT,BV16_COMPLEMENT,BV16_ALL_ZEROES,BV16_AND,BV16_IOR,BV16_XOR,BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | included(Machine(TYPE_BIT)),included(Machine(TYPE_BV8)),included(Machine(TYPE_BV16)) | ? | TYPES);
  List_Of_HiddenCst_Ids(Machine(TYPES)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (WORD8,WORD8_LENGTH,WORD8_POSITION,NB_WORD8S,BV8_TO_WORD8,WORD8_TO_BV8,WORD16,WORD16_LENGTH,WORD16_POSITION,NB_WORD16S,BV16_TO_WORD16,WORD16_TO_BV16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,BV8_TO_BV16,WORD8_TO_WORD16,WORD16_TO_WORD8,BV16_INDEX,BV16,BV16_SET_BIT,BV16_COMPLEMENT,BV16_ALL_ZEROES,BV16_AND,BV16_IOR,BV16_XOR,BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR);
  List_Of_VisibleVar_Ids(Machine(TYPES)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPES)) == (seen(Machine(TYPE_BIT)) : (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(TYPE_BV16)) == (BV16_INDEX,BV16,BV16_SET_BIT,BV16_COMPLEMENT,BV16_ALL_ZEROES,BV16_AND,BV16_IOR,BV16_XOR | ? | ? | ? | ? | ? | seen(Machine(TYPE_BIT)) | ? | TYPE_BV16);
  List_Of_HiddenCst_Ids(Machine(TYPE_BV16)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BV16)) == (BV16_INDEX,BV16,BV16_SET_BIT,BV16_COMPLEMENT,BV16_ALL_ZEROES,BV16_AND,BV16_IOR,BV16_XOR);
  List_Of_VisibleVar_Ids(Machine(TYPE_BV16)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BV16)) == (? : ?);
  List_Of_Ids(Machine(TYPE_BIT)) == (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | ? | ? | ? | TYPE_BIT);
  List_Of_HiddenCst_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BIT)) == (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR);
  List_Of_VisibleVar_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BIT)) == (? : ?);
  List_Of_Ids(Machine(TYPE_BV8)) == (BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR | ? | ? | ? | ? | ? | seen(Machine(TYPE_BIT)) | ? | TYPE_BV8);
  List_Of_HiddenCst_Ids(Machine(TYPE_BV8)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BV8)) == (BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR);
  List_Of_VisibleVar_Ids(Machine(TYPE_BV8)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BV8)) == (? : ?);
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
THEORY SetsEnvX IS
  Sets(Implementation(traffic_light_asm_z80)) == (Type(id_register8) == Cst(SetOf(etype(id_register8,0,17))))
END
&
THEORY ConstantsEnvX IS
  Constants(Implementation(traffic_light_asm_z80)) == (Type(r) == Cst(etype(id_register8,0,17));Type(i) == Cst(etype(id_register8,0,17));Type(l_) == Cst(etype(id_register8,0,17));Type(h_) == Cst(etype(id_register8,0,17));Type(l) == Cst(etype(id_register8,0,17));Type(h) == Cst(etype(id_register8,0,17));Type(e_) == Cst(etype(id_register8,0,17));Type(d_) == Cst(etype(id_register8,0,17));Type(e) == Cst(etype(id_register8,0,17));Type(d) == Cst(etype(id_register8,0,17));Type(c_) == Cst(etype(id_register8,0,17));Type(b_) == Cst(etype(id_register8,0,17));Type(c) == Cst(etype(id_register8,0,17));Type(b) == Cst(etype(id_register8,0,17));Type(a_) == Cst(etype(id_register8,0,17));Type(f_) == Cst(etype(id_register8,0,17));Type(f) == Cst(etype(id_register8,0,17));Type(a) == Cst(etype(id_register8,0,17)))
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
  List_Local_Operations(Implementation(traffic_light_asm_z80))==(?)
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
  TypingPredicate(Implementation(traffic_light_asm_z80))==(btrue)
END
&
THEORY ImportedVariablesListX IS
  ImportedVariablesList(Implementation(traffic_light_asm_z80),Machine(mp.Z80))==(?);
  ImportedVisVariablesList(Implementation(traffic_light_asm_z80),Machine(mp.Z80))==(mp.rgs8,mp.mem,mp.pc,mp.sp,mp.ix,mp.iy,mp.stack)
END
&
THEORY ListLocalOpInvariantX END
)
