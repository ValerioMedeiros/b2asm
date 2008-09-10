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
  List_Sees(Machine(PIC))==(BIT_DEFINITION,BYTE_DEFINITION,TYPES)
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
  Local_List_Variables(Machine(PIC))==(sp,stack,pc,W_REGISTER,mem);
  List_Variables(Machine(PIC))==(sp,stack,pc,W_REGISTER,mem);
  External_List_Variables(Machine(PIC))==(sp,stack,pc,W_REGISTER,mem)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(PIC))==(?);
  Abstract_List_VisibleVariables(Machine(PIC))==(?);
  External_List_VisibleVariables(Machine(PIC))==(?);
  Expanded_List_VisibleVariables(Machine(PIC))==(?);
  List_VisibleVariables(Machine(PIC))==(?);
  Internal_List_VisibleVariables(Machine(PIC))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(PIC))==(btrue);
  Gluing_List_Invariant(Machine(PIC))==(btrue);
  Expanded_List_Invariant(Machine(PIC))==(btrue);
  Abstract_List_Invariant(Machine(PIC))==(btrue);
  Context_List_Invariant(Machine(PIC))==(btrue);
  List_Invariant(Machine(PIC))==(mem: REGISTER --> BYTE & W_REGISTER: BYTE & pc: INSTRUCTION & sp: NATURAL & stack: NATURAL +-> INSTRUCTION & dom(stack) = 0..sp-1)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(PIC))==(btrue);
  Abstract_List_Assertions(Machine(PIC))==(btrue);
  Context_List_Assertions(Machine(PIC))==((bit_not(0) = 1;bit_not(1) = 0;!bb.(bb: BIT => bit_not(bit_not(bb)) = bb);bit_and(0,0) = 0;bit_and(0,1) = 0;bit_and(1,0) = 0;bit_and(1,1) = 1;!(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3));!b1.(b1: BIT => bit_and(b1,1) = b1);!b1.(b1: BIT => bit_and(b1,0) = 0);bit_or(0,0) = 0;bit_or(0,1) = 0;bit_or(1,0) = 0;bit_or(1,1) = 1;!(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3));!b1.(b1: BIT => bit_or(b1,1) = 1);!b1.(b1: BIT => bit_or(b1,0) = b1);bit_xor(0,0) = 0;bit_xor(0,1) = 1;bit_xor(1,0) = 1;bit_xor(1,1) = 0;!(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3));!bb.(bb: BIT => bit_xor(bb,bb) = 0);bool_to_bit(TRUE) = 1;bool_to_bit(FALSE) = 0) & (NB_UCHARS = 256;!nn.(nn: UCHAR => 0<=nn);!nn.(nn: UCHAR => nn<=255);REGISTER = UCHAR) & (dom(add) = UCHAR*UCHAR;ran(add) <: UCHAR*BOOL*BOOL*BOOL;dom(substract) = UCHAR*UCHAR;ran(substract) <: UCHAR*BOOL*BOOL;dom(and) = BYTE*BYTE;ran(and) <: BYTE*BOOL;dom(ior) = BYTE*BYTE;ran(ior) <: BYTE*BOOL;dom(xor) = BYTE*BYTE;ran(xor) <: BYTE*BOOL;dom(bitclear) = BYTE*BYTE_INDEX;ran(bitclear) <: BYTE;dom(bitset) = BYTE*BYTE_INDEX;ran(bitset) <: BYTE;dom(bitget) = BYTE*BYTE_INDEX;ran(bitget) <: BIT;dom(complement) = BYTE;ran(complement) <: BYTE;dom(swap) = BYTE;ran(swap) <: BYTE;ran(rotateleft) <: BYTE*BOOL;dom(rotateleft) = BYTE;dom(rotateright) = BYTE;ran(rotateright) <: BYTE*BOOL));
  List_Assertions(Machine(PIC))==(ran(mem) <: BYTE;dom(mem) = REGISTER)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(PIC))==(@(W_REGISTER$0).(W_REGISTER$0: BYTE ==> W_REGISTER:=W_REGISTER$0) || @(random,init_status).(random: REGISTER --> BYTE & init_status: BYTE & init_status(RP1_POS) = 0 ==> mem:=random<+{INDF_ADDR0|->BYTE_ZERO,STATUS_ADDR0|->init_status,INDF_ADDR0|->BYTE_ZERO,STATUS_ADDR1|->init_status}<+UNIMPLEMENTED_LOCATIONS*{BYTE_ZERO}) || @(pc$0).(pc$0: INSTRUCTION ==> pc:=pc$0) || stack:={} || sp:=0);
  Context_List_Initialisation(Machine(PIC))==(skip);
  List_Initialisation(Machine(PIC))==(W_REGISTER:: BYTE || ANY random,init_status WHERE random: REGISTER --> BYTE & init_status: BYTE & init_status(RP1_POS) = 0 THEN mem:=random<+{INDF_ADDR0|->BYTE_ZERO,STATUS_ADDR0|->init_status,INDF_ADDR0|->BYTE_ZERO,STATUS_ADDR1|->init_status}<+UNIMPLEMENTED_LOCATIONS*{BYTE_ZERO} END || pc:: INSTRUCTION || stack:={} || sp:=0)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(PIC))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(PIC),Machine(ALU))==(?);
  List_Instanciated_Parameters(Machine(PIC),Machine(BIT_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(PIC),Machine(BYTE_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(PIC),Machine(TYPES))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(PIC))==(btrue);
  List_Constraints(Machine(PIC))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(PIC))==(CALL,RETURN,RETLW,ADDLW,ADDWF);
  List_Operations(Machine(PIC))==(CALL,RETURN,RETLW,ADDLW,ADDWF)
END
&
THEORY ListInputX IS
  List_Input(Machine(PIC),CALL)==(kk);
  List_Input(Machine(PIC),RETURN)==(?);
  List_Input(Machine(PIC),RETLW)==(kk);
  List_Input(Machine(PIC),ADDLW)==(kk);
  List_Input(Machine(PIC),ADDWF)==(ff,dd)
END
&
THEORY ListOutputX IS
  List_Output(Machine(PIC),CALL)==(?);
  List_Output(Machine(PIC),RETURN)==(?);
  List_Output(Machine(PIC),RETLW)==(?);
  List_Output(Machine(PIC),ADDLW)==(?);
  List_Output(Machine(PIC),ADDWF)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(PIC),CALL)==(CALL(kk));
  List_Header(Machine(PIC),RETURN)==(RETURN);
  List_Header(Machine(PIC),RETLW)==(RETLW(kk));
  List_Header(Machine(PIC),ADDLW)==(ADDLW(kk));
  List_Header(Machine(PIC),ADDWF)==(ADDWF(ff,dd))
END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(PIC),CALL)==(kk: INSTRUCTION);
  List_Precondition(Machine(PIC),RETURN)==(sp>0);
  List_Precondition(Machine(PIC),RETLW)==(kk: UCHAR & sp>0);
  List_Precondition(Machine(PIC),ADDLW)==(kk: UCHAR);
  List_Precondition(Machine(PIC),ADDWF)==(ff: REGISTER0 & dd: BIT)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(PIC),ADDWF)==(ff: REGISTER0 & dd: BIT | @(dest,result,carry,digit_carry,zero).(dest: REGISTER & result: UCHAR & digit_carry: BOOL & carry: BOOL & zero: BOOL & dest = get_address(ff,mem(STATUS_ADDR0)(RP0_POS),byte_to_uchar(mem(FSR_ADDR0))) & result,digit_carry,carry,zero = add(byte_to_uchar(mem(dest)),byte_to_uchar(W_REGISTER)) ==> @(mem2,bv).(bv: BYTE & bv = uchar_to_byte(result) & mem2: REGISTER --> BYTE & (dd/=0 & dest/:UNIMPLEMENTED_LOCATIONS => mem2 = mem<+{dest|->bv}) & (dd = 0 or dest: UNIMPLEMENTED_LOCATIONS => mem2 = mem) ==> (dd = 0 ==> W_REGISTER:=bv [] not(dd = 0) ==> skip || (ff/=STATUS_ADDR0 ==> mem:=mem<+{STATUS_ADDR0|->(mem(STATUS_ADDR0)<+{ZERO_POS|->bool_to_bit(zero),DCARRY_POS|->bool_to_bit(digit_carry),CARRY_POS|->bool_to_bit(carry)}),STATUS_ADDR1|->(mem(STATUS_ADDR0)<+{ZERO_POS|->bool_to_bit(zero),DCARRY_POS|->bool_to_bit(digit_carry),CARRY_POS|->bool_to_bit(carry)})} [] not(ff/=STATUS_ADDR0) ==> skip)))) || pc:=instruction_next(pc));
  Expanded_List_Substitution(Machine(PIC),ADDLW)==(kk: UCHAR | @(result,carry,digit_carry,zero).(result: UCHAR & digit_carry: BOOL & carry: BOOL & zero: BOOL & result,digit_carry,carry,zero = add(kk,byte_to_uchar(W_REGISTER)) ==> W_REGISTER,mem:=uchar_to_byte(result),mem<+{STATUS_ADDR0|->(mem(STATUS_ADDR0)<+{ZERO_POS|->bool_to_bit(zero),DCARRY_POS|->bool_to_bit(digit_carry),CARRY_POS|->bool_to_bit(carry)}),STATUS_ADDR1|->(mem(STATUS_ADDR0)<+{ZERO_POS|->bool_to_bit(zero),DCARRY_POS|->bool_to_bit(digit_carry),CARRY_POS|->bool_to_bit(carry)})}) || pc:=instruction_next(pc));
  Expanded_List_Substitution(Machine(PIC),RETLW)==(kk: UCHAR & sp>0 | pc,W_REGISTER:=stack(sp-1),uchar_to_byte(kk));
  Expanded_List_Substitution(Machine(PIC),RETURN)==(sp>0 | stack,pc,sp:={sp-1}<<|stack,stack(sp-1),sp-1);
  Expanded_List_Substitution(Machine(PIC),CALL)==(kk: INSTRUCTION | stack,sp,pc:=stack<+{sp|->instruction_next(pc)},sp+1,kk);
  List_Substitution(Machine(PIC),CALL)==(stack(sp):=instruction_next(pc) || sp:=sp+1 || pc:=kk);
  List_Substitution(Machine(PIC),RETURN)==(stack:={sp-1}<<|stack || pc:=stack(sp-1) || sp:=sp-1);
  List_Substitution(Machine(PIC),RETLW)==(pc:=stack(sp-1) || W_REGISTER:=uchar_to_byte(kk));
  List_Substitution(Machine(PIC),ADDLW)==(ANY result,carry,digit_carry,zero WHERE result: UCHAR & digit_carry: BOOL & carry: BOOL & zero: BOOL & result,digit_carry,carry,zero = add(kk,byte_to_uchar(W_REGISTER)) THEN W_REGISTER:=uchar_to_byte(result) || mem:=mem<+{STATUS_ADDR0|->(mem(STATUS_ADDR0)<+{ZERO_POS|->bool_to_bit(zero),DCARRY_POS|->bool_to_bit(digit_carry),CARRY_POS|->bool_to_bit(carry)}),STATUS_ADDR1|->(mem(STATUS_ADDR0)<+{ZERO_POS|->bool_to_bit(zero),DCARRY_POS|->bool_to_bit(digit_carry),CARRY_POS|->bool_to_bit(carry)})} END || pc:=instruction_next(pc));
  List_Substitution(Machine(PIC),ADDWF)==(ANY dest,result,carry,digit_carry,zero WHERE dest: REGISTER & result: UCHAR & digit_carry: BOOL & carry: BOOL & zero: BOOL & dest = get_address(ff,mem(STATUS_ADDR0)(RP0_POS),byte_to_uchar(mem(FSR_ADDR0))) & result,digit_carry,carry,zero = add(byte_to_uchar(mem(dest)),byte_to_uchar(W_REGISTER)) THEN ANY mem2,bv WHERE bv: BYTE & bv = uchar_to_byte(result) & mem2: REGISTER --> BYTE & (dd/=0 & dest/:UNIMPLEMENTED_LOCATIONS => mem2 = mem<+{dest|->bv}) & (dd = 0 or dest: UNIMPLEMENTED_LOCATIONS => mem2 = mem) THEN IF dd = 0 THEN W_REGISTER:=bv END || IF ff/=STATUS_ADDR0 THEN mem:=mem<+{STATUS_ADDR0|->(mem(STATUS_ADDR0)<+{ZERO_POS|->bool_to_bit(zero),DCARRY_POS|->bool_to_bit(digit_carry),CARRY_POS|->bool_to_bit(carry)}),STATUS_ADDR1|->(mem(STATUS_ADDR0)<+{ZERO_POS|->bool_to_bit(zero),DCARRY_POS|->bool_to_bit(digit_carry),CARRY_POS|->bool_to_bit(carry)})} END END END || pc:=instruction_next(pc))
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(PIC))==(IRP_POS,RP1_POS,RP0_POS,TO_POS,PD_POS,ZERO_POS,DCARRY_POS,CARRY_POS,INDF_ADDR0,TMR0_ADDR,PCL_ADDR,STATUS_ADDR0,FSR_ADDR0,PORTA_ADDR,PORTB_ADDR,PCLATH_ADDR0,INTCON_ADDR0,PIR1_ADDR,CMCON_ADDR,INDF_ADDR1,STATUS_ADDR1,FSR_ADDR1,TRISA_ADDR,TRISB_ADDR,PCLATH_ADDR1,INTCON_ADDR1,PIE1_ADDR,PCON_ADDR,LININTF_ADDR,VRCON_ADDR,UNIMPLEMENTED_LOCATIONS,get_address,get_offset);
  Inherited_List_Constants(Machine(PIC))==(?);
  List_Constants(Machine(PIC))==(IRP_POS,RP1_POS,RP0_POS,TO_POS,PD_POS,ZERO_POS,DCARRY_POS,CARRY_POS,INDF_ADDR0,TMR0_ADDR,PCL_ADDR,STATUS_ADDR0,FSR_ADDR0,PORTA_ADDR,PORTB_ADDR,PCLATH_ADDR0,INTCON_ADDR0,PIR1_ADDR,CMCON_ADDR,INDF_ADDR1,STATUS_ADDR1,FSR_ADDR1,TRISA_ADDR,TRISB_ADDR,PCLATH_ADDR1,INTCON_ADDR1,PIE1_ADDR,PCON_ADDR,LININTF_ADDR,VRCON_ADDR,UNIMPLEMENTED_LOCATIONS,get_address,get_offset)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(PIC))==(?);
  Context_List_Defered(Machine(PIC))==(?);
  Context_List_Sets(Machine(PIC))==(?);
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
  Context_List_Properties(Machine(PIC))==(BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE <: BIT_VECTOR & BYTE = {vv | vv: BIT_VECTOR & bv_size(vv) = BYTE_WIDTH} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & UCHAR_LENGTH: NATURAL & NB_UCHARS: NATURAL & INST_SZ: NATURAL & NB_INSTRUCTIONS: NATURAL & UCHAR_LENGTH = 8 & NB_UCHARS = 2**UCHAR_LENGTH & UCHAR = 0..NB_UCHARS-1 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = 0..INSTRUCTION_MAX & instruction_next: INSTRUCTION --> INSTRUCTION & instruction_next = {pp,qq | pp: INSTRUCTION & qq: INSTRUCTION & 0<=pp & pp<NB_INSTRUCTIONS-1 & qq = pp+1}\/{NB_INSTRUCTIONS-1|->0} & byte_to_uchar: BYTE --> UCHAR & !vv.(vv: BYTE => byte_to_uchar(vv) = bv_to_nat(vv)) & uchar_to_byte: UCHAR --> BYTE & uchar_to_byte = byte_to_uchar~ & REGISTER = 0..255 & REGISTER0 = 0..127 & REGISTER1 = 128..255 & add: UCHAR*UCHAR --> UCHAR*BOOL*BOOL*BOOL & !(w1,w2,sum).(w1: UCHAR & w2: UCHAR & sum: NATURAL & sum = w1+w2 => (sum<=255 => add(w1,w2) = sum,bool(sum = 0),bool(w1/16+w2/16>15),FALSE) & (256<=sum => add(w1,w2) = sum-256,bool(sum = 256),bool(w1/16+w2/16>15),TRUE)) & substract: UCHAR*UCHAR --> UCHAR*BOOL*BOOL & !(w1,w2,diff).(w1: UCHAR & w2: UCHAR & diff: INTEGER & diff = w1-w2 => (diff<0 => substract(w1,w2) = diff+256,FALSE,TRUE) & (diff>=0 => substract(w1,w2) = diff,bool(diff = 0),FALSE)) & and: BYTE*BYTE --> BYTE*BOOL & !(w1,w2,ww).(w1: BYTE & w2: BYTE & ww: BYTE & ww = bv_and(w1,w2) => and(w1,w2) = ww,bool(bv_to_nat(ww) = 0)) & ior: BYTE*BYTE --> BYTE*BOOL & !(w1,w2,ww).(w1: BYTE & w2: BYTE & ww: BYTE & ww = bv_or(w1,w2) => ior(w1,w2) = ww,bool(bv_to_nat(ww) = 0)) & xor: BYTE*BYTE --> BYTE*BOOL & !(w1,w2,ww).(w1: BYTE & w2: BYTE & ww: BYTE => (ww = bv_xor(w1,w2) => xor(w1,w2) = ww,bool(bv_to_nat(ww) = 0))) & bitget: BYTE*BYTE_INDEX --> BIT & !(ww,ii).(ww: BYTE & ii: BYTE_INDEX => bitget(ww,ii) = ww(ii)) & bitset: BYTE*BYTE_INDEX --> BYTE & !(ww,ii).(ww: BYTE & ii: BYTE_INDEX => bitset(ww,ii) = bv_set(ww,ii)) & bitclear: BYTE*BYTE_INDEX --> BYTE & !(ww,ii,bb).(ww: BYTE & ii: BYTE_INDEX & bb: BIT => bitclear(ww,ii) = bv_clear(ww,ii)) & complement: BYTE --> BYTE & !ww.(ww: BYTE => complement(ww) = bv_not(ww)) & swap: BYTE --> BYTE & !ww.(ww: BYTE => swap(ww) = {0|->ww(4),1|->ww(5),2|->ww(6),3|->ww(7),4|->ww(0),5|->ww(1),6|->ww(2),7|->ww(3)}) & rotateleft: BYTE --> BYTE*BOOL & !ww.(ww: BYTE => rotateleft(ww) = {0|->ww(7),1|->ww(0),2|->ww(1),3|->ww(2),4|->ww(3),5|->ww(4),6|->ww(5),7|->ww(6)},bool(ww(7) = 1)) & rotateright: BYTE --> BYTE*BOOL & !ww.(ww: BYTE => rotateright(ww) = {0|->ww(1),1|->ww(2),2|->ww(3),3|->ww(4),4|->ww(5),5|->ww(6),6|->ww(7),7|->ww(0)},bool(ww(0) = 1)));
  Inherited_List_Properties(Machine(PIC))==(btrue);
  List_Properties(Machine(PIC))==(INDF_ADDR0: REGISTER & INDF_ADDR0 = 0 & TMR0_ADDR: REGISTER & TMR0_ADDR = 1 & PCL_ADDR: REGISTER & PCL_ADDR = 2 & STATUS_ADDR0: REGISTER & STATUS_ADDR0 = 3 & FSR_ADDR0: REGISTER & FSR_ADDR0 = 4 & PORTA_ADDR: REGISTER & PORTA_ADDR = 5 & PORTB_ADDR: REGISTER & PORTB_ADDR = 6 & PCLATH_ADDR0: REGISTER & PCLATH_ADDR0 = 10 & INTCON_ADDR0: REGISTER & INTCON_ADDR0 = 11 & PIR1_ADDR: REGISTER & PIR1_ADDR = 12 & CMCON_ADDR: REGISTER & CMCON_ADDR = 127 & INDF_ADDR1: REGISTER & INDF_ADDR1 = 128 & STATUS_ADDR1: REGISTER & STATUS_ADDR1 = 131 & FSR_ADDR1: REGISTER & FSR_ADDR1 = 132 & TRISA_ADDR: REGISTER & TRISA_ADDR = 133 & TRISB_ADDR: REGISTER & TRISB_ADDR = 134 & PCLATH_ADDR1: REGISTER & PCLATH_ADDR1 = 138 & INTCON_ADDR1: REGISTER & INTCON_ADDR1 = 139 & PIE1_ADDR: REGISTER & PIE1_ADDR = 140 & PCON_ADDR: REGISTER & PCON_ADDR = 142 & LININTF_ADDR: REGISTER & LININTF_ADDR = 144 & VRCON_ADDR: REGISTER & VRCON_ADDR = 159 & UNIMPLEMENTED_LOCATIONS = 7..9\/13..30\/135..137\/{141}\/{143}\/145..158\/192..240 & IRP_POS: BYTE_INDEX & IRP_POS = 7 & RP1_POS: BYTE_INDEX & RP1_POS = 6 & RP0_POS: BYTE_INDEX & RP0_POS = 5 & TO_POS: BYTE_INDEX & TO_POS = 4 & PD_POS: BYTE_INDEX & PD_POS = 3 & ZERO_POS: BYTE_INDEX & ZERO_POS = 2 & DCARRY_POS: BYTE_INDEX & DCARRY_POS = 1 & CARRY_POS: BYTE_INDEX & CARRY_POS = 0 & get_offset: REGISTER*REGISTER --> REGISTER & !(ff,ind).(ff: REGISTER & ind: REGISTER => (ff = INDF_ADDR0 => get_offset(ind,ff) = ind) & (ff/=INDF_ADDR0 => get_offset(ind,ff) = ff)) & get_address: REGISTER*BIT*REGISTER --> REGISTER & !(ff,bb,ind).(ff: REGISTER & bb: BIT & ind: REGISTER => get_address(ff,bb,ind) = 128*bb+get_offset(ff,ind)))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(PIC),Machine(TYPES))==(?);
  Seen_Context_List_Enumerated(Machine(PIC))==(?);
  Seen_Context_List_Invariant(Machine(PIC))==(btrue);
  Seen_Context_List_Assertions(Machine(PIC))==((bit_not(0) = 1;bit_not(1) = 0;!bb.(bb: BIT => bit_not(bit_not(bb)) = bb);bit_and(0,0) = 0;bit_and(0,1) = 0;bit_and(1,0) = 0;bit_and(1,1) = 1;!(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3));!b1.(b1: BIT => bit_and(b1,1) = b1);!b1.(b1: BIT => bit_and(b1,0) = 0);bit_or(0,0) = 0;bit_or(0,1) = 0;bit_or(1,0) = 0;bit_or(1,1) = 1;!(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3));!b1.(b1: BIT => bit_or(b1,1) = 1);!b1.(b1: BIT => bit_or(b1,0) = b1);bit_xor(0,0) = 0;bit_xor(0,1) = 1;bit_xor(1,0) = 1;bit_xor(1,1) = 0;!(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3));!bb.(bb: BIT => bit_xor(bb,bb) = 0);bool_to_bit(TRUE) = 1;bool_to_bit(FALSE) = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!bv.(bv: BIT_VECTOR => bv_not(bv_not(bv)) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_and(v1,v2) = bv_and(v2,v1));!(v1,v2,v3).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) => bv_and(v1,bv_and(v2,v3)) = bv_and(bv_and(v1,v2),v3));!bv.(bv: BIT_VECTOR => bv_and(bv,bv_zero(bv_size(bv))) = bv_zero(bv_size(bv)));!bv.(bv: BIT_VECTOR => bv_and(bv,bv_one(bv_size(bv))) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_or(v1,v2) = bv_or(v2,v1));!(v1,v2,v3).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) => bv_or(v1,bv_or(v2,v3)) = bv_or(bv_or(v1,v2),v3));!bv.(bv: BIT_VECTOR => bv_or(bv,bv_one(bv_size(bv))) = bv_one(bv_size(bv)));!bv.(bv: BIT_VECTOR => bv_or(bv,bv_zero(bv_size(bv))) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_xor(v1,v2) = bv_xor(v2,v1));!bv.(bv: BIT_VECTOR => bv_xor(bv,bv) = bv_zero(bv_size(bv)))) & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536) & !ss.(ss: NATURAL1 => bv_to_nat(bv_zero(ss)) = 0));
  Seen_Context_List_Properties(Machine(PIC))==(BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = seq1(BIT) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (0..sz-1)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (0..sz-1)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 0..size(v1)-1 | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 0..size(v1)-1 | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 0..size(v1)-1 | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 0..size(v1)-1 | bit_xor(v1(idx),v2(idx)))) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1<+{idx|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1<+{idx|->0}) & BYTE_ZERO = BYTE_INDEX*{0} & BYTE_ZERO: BYTE & BYTE = {vv | vv: BIT_VECTOR & bv_size(vv) = BYTE_WIDTH} & BYTE <: BIT_VECTOR & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE_WIDTH = 8 & bv_to_nat = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | 2**idx*bv(idx))) & bv_to_nat: BIT_VECTOR --> NATURAL);
  Seen_List_Constraints(Machine(PIC))==(btrue);
  Seen_List_Operations(Machine(PIC),Machine(TYPES))==(?);
  Seen_Expanded_List_Invariant(Machine(PIC),Machine(TYPES))==(btrue);
  Seen_Internal_List_Operations(Machine(PIC),Machine(BYTE_DEFINITION))==(?);
  Seen_List_Operations(Machine(PIC),Machine(BYTE_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(PIC),Machine(BYTE_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(PIC),Machine(BIT_DEFINITION))==(?);
  Seen_List_Operations(Machine(PIC),Machine(BIT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(PIC),Machine(BIT_DEFINITION))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(PIC)) == (IRP_POS,RP1_POS,RP0_POS,TO_POS,PD_POS,ZERO_POS,DCARRY_POS,CARRY_POS,INDF_ADDR0,TMR0_ADDR,PCL_ADDR,STATUS_ADDR0,FSR_ADDR0,PORTA_ADDR,PORTB_ADDR,PCLATH_ADDR0,INTCON_ADDR0,PIR1_ADDR,CMCON_ADDR,INDF_ADDR1,STATUS_ADDR1,FSR_ADDR1,TRISA_ADDR,TRISB_ADDR,PCLATH_ADDR1,INTCON_ADDR1,PIE1_ADDR,PCON_ADDR,LININTF_ADDR,VRCON_ADDR,UNIMPLEMENTED_LOCATIONS,get_address,get_offset | ? | sp,stack,pc,W_REGISTER,mem | ? | CALL,RETURN,RETLW,ADDLW,ADDWF | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BYTE_DEFINITION)),seen(Machine(TYPES)),used(Machine(ALU)) | ? | PIC);
  List_Of_HiddenCst_Ids(Machine(PIC)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(PIC)) == (IRP_POS,RP1_POS,RP0_POS,TO_POS,PD_POS,ZERO_POS,DCARRY_POS,CARRY_POS,INDF_ADDR0,TMR0_ADDR,PCL_ADDR,STATUS_ADDR0,FSR_ADDR0,PORTA_ADDR,PORTB_ADDR,PCLATH_ADDR0,INTCON_ADDR0,PIR1_ADDR,CMCON_ADDR,INDF_ADDR1,STATUS_ADDR1,FSR_ADDR1,TRISA_ADDR,TRISB_ADDR,PCLATH_ADDR1,INTCON_ADDR1,PIE1_ADDR,PCON_ADDR,LININTF_ADDR,VRCON_ADDR,UNIMPLEMENTED_LOCATIONS,get_address,get_offset);
  List_Of_VisibleVar_Ids(Machine(PIC)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(PIC)) == (?: ?);
  List_Of_Ids(Machine(ALU)) == (add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright | ? | ? | ? | ? | ? | seen(Machine(TYPES)),seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BIT_VECTOR_ARITHMETICS)),seen(Machine(BYTE_DEFINITION)) | ? | ALU);
  List_Of_HiddenCst_Ids(Machine(ALU)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ALU)) == (add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright);
  List_Of_VisibleVar_Ids(Machine(ALU)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ALU)) == (?: ?);
  List_Of_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)) | ? | BYTE_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO);
  List_Of_VisibleVar_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BYTE_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_size,bv_zero,bv_one,bv_not,bv_and,bv_or,bv_xor,bv_set,bv_clear | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)) | ? | BIT_VECTOR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_size,bv_zero,bv_one,bv_not,bv_and,bv_or,bv_xor,bv_set,bv_clear);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ? | BIT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit);
  List_Of_VisibleVar_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (bv_to_nat | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)) | ? | BIT_VECTOR_ARITHMETICS);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (bv_to_nat);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR_ARITHMETICS)) == (?: ?);
  List_Of_Ids(Machine(TYPES)) == (UCHAR_LENGTH,UCHAR,NB_UCHARS,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,byte_to_uchar,uchar_to_byte,REGISTER,REGISTER0,REGISTER1 | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BIT_VECTOR_ARITHMETICS)),seen(Machine(BYTE_DEFINITION)),seen(Machine(POWER2)) | ? | TYPES);
  List_Of_HiddenCst_Ids(Machine(TYPES)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (UCHAR_LENGTH,UCHAR,NB_UCHARS,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,byte_to_uchar,uchar_to_byte,REGISTER,REGISTER0,REGISTER1);
  List_Of_VisibleVar_Ids(Machine(TYPES)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPES)) == (?: ?);
  List_Of_Ids(Machine(POWER2)) == (? | ? | ? | ? | ? | ? | seen(Machine(POWER)) | ? | POWER2);
  List_Of_HiddenCst_Ids(Machine(POWER2)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(POWER2)) == (?);
  List_Of_VisibleVar_Ids(Machine(POWER2)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(POWER2)) == (?: ?);
  List_Of_Ids(Machine(POWER)) == (? | ? | ? | ? | ? | ? | ? | ? | POWER);
  List_Of_HiddenCst_Ids(Machine(POWER)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(POWER)) == (?);
  List_Of_VisibleVar_Ids(Machine(POWER)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(POWER)) == (?: ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(PIC)) == (Type(IRP_POS) == Cst(btype(INTEGER,?,?));Type(RP1_POS) == Cst(btype(INTEGER,?,?));Type(RP0_POS) == Cst(btype(INTEGER,?,?));Type(TO_POS) == Cst(btype(INTEGER,?,?));Type(PD_POS) == Cst(btype(INTEGER,?,?));Type(ZERO_POS) == Cst(btype(INTEGER,?,?));Type(DCARRY_POS) == Cst(btype(INTEGER,?,?));Type(CARRY_POS) == Cst(btype(INTEGER,?,?));Type(INDF_ADDR0) == Cst(btype(INTEGER,?,?));Type(TMR0_ADDR) == Cst(btype(INTEGER,?,?));Type(PCL_ADDR) == Cst(btype(INTEGER,?,?));Type(STATUS_ADDR0) == Cst(btype(INTEGER,?,?));Type(FSR_ADDR0) == Cst(btype(INTEGER,?,?));Type(PORTA_ADDR) == Cst(btype(INTEGER,?,?));Type(PORTB_ADDR) == Cst(btype(INTEGER,?,?));Type(PCLATH_ADDR0) == Cst(btype(INTEGER,?,?));Type(INTCON_ADDR0) == Cst(btype(INTEGER,?,?));Type(PIR1_ADDR) == Cst(btype(INTEGER,?,?));Type(CMCON_ADDR) == Cst(btype(INTEGER,?,?));Type(INDF_ADDR1) == Cst(btype(INTEGER,?,?));Type(STATUS_ADDR1) == Cst(btype(INTEGER,?,?));Type(FSR_ADDR1) == Cst(btype(INTEGER,?,?));Type(TRISA_ADDR) == Cst(btype(INTEGER,?,?));Type(TRISB_ADDR) == Cst(btype(INTEGER,?,?));Type(PCLATH_ADDR1) == Cst(btype(INTEGER,?,?));Type(INTCON_ADDR1) == Cst(btype(INTEGER,?,?));Type(PIE1_ADDR) == Cst(btype(INTEGER,?,?));Type(PCON_ADDR) == Cst(btype(INTEGER,?,?));Type(LININTF_ADDR) == Cst(btype(INTEGER,?,?));Type(VRCON_ADDR) == Cst(btype(INTEGER,?,?));Type(UNIMPLEMENTED_LOCATIONS) == Cst(SetOf(btype(INTEGER,?,?)));Type(get_address) == Cst(SetOf(btype(INTEGER,"[REGISTER","]REGISTER")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[REGISTER","]REGISTER")*btype(INTEGER,"[REGISTER","]REGISTER")));Type(get_offset) == Cst(SetOf(btype(INTEGER,"[REGISTER","]REGISTER")*btype(INTEGER,"[REGISTER","]REGISTER")*btype(INTEGER,"[REGISTER","]REGISTER"))))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(PIC)) == (Type(sp) == Mvl(btype(INTEGER,?,?));Type(stack) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(pc) == Mvl(btype(INTEGER,?,?));Type(W_REGISTER) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(mem) == Mvl(SetOf(btype(INTEGER,"[REGISTER","]REGISTER")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(PIC)) == (Type(ADDWF) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ADDLW) == Cst(No_type,btype(INTEGER,?,?));Type(RETLW) == Cst(No_type,btype(INTEGER,?,?));Type(RETURN) == Cst(No_type,No_type);Type(CALL) == Cst(No_type,btype(INTEGER,?,?)))
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
