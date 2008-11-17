Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(MEMORY))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(MEMORY))==(Machine(MEMORY));
  Level(Machine(MEMORY))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(MEMORY)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(MEMORY))==(BYTE_DEFINITION,BIT_DEFINITION,TYPES,BIT_VECTOR_DEFINITION,BIT_VECTOR_ARITHMETICS,POWER2)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(MEMORY))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(MEMORY))==(?);
  List_Includes(Machine(MEMORY))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(MEMORY))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(MEMORY))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(MEMORY))==(?);
  Context_List_Variables(Machine(MEMORY))==(?);
  Abstract_List_Variables(Machine(MEMORY))==(?);
  Local_List_Variables(Machine(MEMORY))==(mem);
  List_Variables(Machine(MEMORY))==(mem);
  External_List_Variables(Machine(MEMORY))==(mem)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(MEMORY))==(?);
  Abstract_List_VisibleVariables(Machine(MEMORY))==(?);
  External_List_VisibleVariables(Machine(MEMORY))==(?);
  Expanded_List_VisibleVariables(Machine(MEMORY))==(?);
  List_VisibleVariables(Machine(MEMORY))==(?);
  Internal_List_VisibleVariables(Machine(MEMORY))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(MEMORY))==(btrue);
  Gluing_List_Invariant(Machine(MEMORY))==(btrue);
  Expanded_List_Invariant(Machine(MEMORY))==(btrue);
  Abstract_List_Invariant(Machine(MEMORY))==(btrue);
  Context_List_Invariant(Machine(MEMORY))==(btrue);
  List_Invariant(Machine(MEMORY))==(mem: MEM_ADDR --> BYTE & mem(PSW)(0) = bv_par(mem(ACC)))
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(MEMORY))==(btrue);
  Abstract_List_Assertions(Machine(MEMORY))==(btrue);
  Context_List_Assertions(Machine(MEMORY))==(!bt.(bt: BYTE => size(bt) = 8) & size(BYTE_ZERO) = 8 & BYTE <: BIT_VECTOR & BYTE_ZERO: BIT_VECTOR & first(BYTE_ZERO) = 0 & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_to_bit(TRUE) = 1 & bool_to_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & dom(byte_schar) = BYTE & ran(byte_schar) <: SCHAR & dom(schar_byte) = SCHAR & ran(schar_byte) <: BYTE & dom(byte_uchar) = BYTE & ran(byte_uchar) <: UCHAR & dom(uchar_byte) = UCHAR & ran(uchar_byte) <: BYTE & dom(uchar_schar) = UCHAR & ran(uchar_schar) <: SCHAR & dom(schar_uchar) = SCHAR & ran(schar_uchar) <: UCHAR & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_not(bv_not(bv))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_and(v1,v2)(indx) = bv_and(v2,v1)(indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_and(v1,bv_and(v2,v3))(indx) = bv_and(bv_and(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_zero(bv_size(bv)))(indx) = bv_zero(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_one(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_or(v1,v2)(indx) = bv_or(v2,v1)(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_or(v1,bv_or(v2,v3))(indx) = bv_or(bv_or(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_one(bv_size(bv)))(indx) = bv_one(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_zero(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_xor(v1,v2)(indx) = bv_xor(v2,v1)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_xor(bv,bv)(indx) = bv_zero(bv_size(bv))(indx))) & !ss.(ss: NATURAL1 => bv_to_nat(bv_zero(ss)) = 0) & !ss.(ss: NATURAL1 => bv_par(bv_zero(ss)) = 0) & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536));
  List_Assertions(Machine(MEMORY))==(ACC/:RAM_ADDR & RAM_ADDR <: MEM_ADDR & SFR_ADDR <: MEM_ADDR & RAM_ADDR\/SFR_ADDR = MEM_ADDR & RAM_BIT_ADDRESSABLE <: RAM_ADDR & SFR_BIT_ADDRESSABLE <: SFR_ADDR & BIT_ADDRESSABLE <: BIT_ADDRESS & ROM_ADDR <: USHORT_INT & !(xx,yy).(xx: ROM_ADDR & yy: ROM_ADDR => (xx+yy>ROM_ADDR_MAX => PC_INCREMENT(xx,yy) = xx+yy-(ROM_ADDR_MAX+1)) & (not(xx+yy>ROM_ADDR_MAX) => PC_INCREMENT(xx,yy) = xx+yy)) & !xx.(xx: BYTE => not(SP_INCREMENT(xx,1) = SP_INCREMENT(xx,2))))
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(MEMORY))==(mem:=MEM_ADDR*{BYTE_ZERO});
  Context_List_Initialisation(Machine(MEMORY))==(skip);
  List_Initialisation(Machine(MEMORY))==(mem:=MEM_ADDR*{BYTE_ZERO})
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(MEMORY))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(MEMORY),Machine(BYTE_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(MEMORY),Machine(BIT_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(MEMORY),Machine(TYPES))==(?);
  List_Instanciated_Parameters(Machine(MEMORY),Machine(BIT_VECTOR_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(MEMORY),Machine(BIT_VECTOR_ARITHMETICS))==(?);
  List_Instanciated_Parameters(Machine(MEMORY),Machine(POWER2))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(MEMORY))==(btrue);
  List_Constraints(Machine(MEMORY))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(MEMORY))==(bitGet,setMem,bitSet,bitClear,addrSetDirect,addrSetIndirect,push_two,push,pop,update);
  List_Operations(Machine(MEMORY))==(bitGet,setMem,bitSet,bitClear,addrSetDirect,addrSetIndirect,push_two,push,pop,update)
END
&
THEORY ListInputX IS
  List_Input(Machine(MEMORY),bitGet)==(bit);
  List_Input(Machine(MEMORY),setMem)==(addr,data);
  List_Input(Machine(MEMORY),bitSet)==(bit);
  List_Input(Machine(MEMORY),bitClear)==(bit);
  List_Input(Machine(MEMORY),addrSetDirect)==(addr,data);
  List_Input(Machine(MEMORY),addrSetIndirect)==(Rn,data);
  List_Input(Machine(MEMORY),push_two)==(data1,data2);
  List_Input(Machine(MEMORY),push)==(data);
  List_Input(Machine(MEMORY),pop)==(addr);
  List_Input(Machine(MEMORY),update)==(acc,Pa,Ov,Ac,Cy)
END
&
THEORY ListOutputX IS
  List_Output(Machine(MEMORY),bitGet)==(rr);
  List_Output(Machine(MEMORY),setMem)==(?);
  List_Output(Machine(MEMORY),bitSet)==(?);
  List_Output(Machine(MEMORY),bitClear)==(?);
  List_Output(Machine(MEMORY),addrSetDirect)==(?);
  List_Output(Machine(MEMORY),addrSetIndirect)==(?);
  List_Output(Machine(MEMORY),push_two)==(?);
  List_Output(Machine(MEMORY),push)==(?);
  List_Output(Machine(MEMORY),pop)==(?);
  List_Output(Machine(MEMORY),update)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(MEMORY),bitGet)==(rr <-- bitGet(bit));
  List_Header(Machine(MEMORY),setMem)==(setMem(addr,data));
  List_Header(Machine(MEMORY),bitSet)==(bitSet(bit));
  List_Header(Machine(MEMORY),bitClear)==(bitClear(bit));
  List_Header(Machine(MEMORY),addrSetDirect)==(addrSetDirect(addr,data));
  List_Header(Machine(MEMORY),addrSetIndirect)==(addrSetIndirect(Rn,data));
  List_Header(Machine(MEMORY),push_two)==(push_two(data1,data2));
  List_Header(Machine(MEMORY),push)==(push(data));
  List_Header(Machine(MEMORY),pop)==(pop(addr));
  List_Header(Machine(MEMORY),update)==(update(acc,Pa,Ov,Ac,Cy))
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(MEMORY),bitGet)==(bit: BIT_ADDRESSABLE);
  List_Precondition(Machine(MEMORY),setMem)==(addr: MEM_ADDR & data: BYTE);
  List_Precondition(Machine(MEMORY),bitSet)==(bit: BIT_ADDRESSABLE);
  List_Precondition(Machine(MEMORY),bitClear)==(bit: BIT_ADDRESSABLE);
  List_Precondition(Machine(MEMORY),addrSetDirect)==(addr: MEM_ADDR & data: BYTE);
  List_Precondition(Machine(MEMORY),addrSetIndirect)==(Rn: RAM_ADDR & (Rn = R1 or Rn = R2) & data: BYTE & byte_uchar(mem(Rn)): RAM_ADDR);
  List_Precondition(Machine(MEMORY),push_two)==(data1: BYTE & data2: BYTE & mem(SP): BYTE);
  List_Precondition(Machine(MEMORY),push)==(data: BYTE);
  List_Precondition(Machine(MEMORY),pop)==(addr: MEM_ADDR);
  List_Precondition(Machine(MEMORY),update)==(acc: UCHAR & Pa: BIT & Ov: BIT & Ac: BIT & Cy: BIT)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(MEMORY),update)==(acc: UCHAR & Pa: BIT & Ov: BIT & Ac: BIT & Cy: BIT | mem:=mem<+{ACC|->uchar_byte(acc),PSW|->PSWUPDATE(mem(PSW),Pa,Ov,Ac,Cy)});
  Expanded_List_Substitution(Machine(MEMORY),pop)==(addr: MEM_ADDR | mem:=mem<+(ADDRCHANGE(addr,mem(SP),mem(PSW))\/{SP|->uchar_byte(SP_DECREMENT(mem(SP),1))}));
  Expanded_List_Substitution(Machine(MEMORY),push)==(data: BYTE | mem:=mem<+{SP_INCREMENT(mem(SP),1)|->data});
  Expanded_List_Substitution(Machine(MEMORY),push_two)==(data1: BYTE & data2: BYTE & mem(SP): BYTE | mem:=mem<+{SP_INCREMENT(mem(SP),1)|->data1,SP_INCREMENT(mem(SP),2)|->data2,SP|->uchar_byte(SP_INCREMENT(mem(SP),2))});
  Expanded_List_Substitution(Machine(MEMORY),addrSetIndirect)==(Rn: RAM_ADDR & (Rn = R1 or Rn = R2) & data: BYTE & byte_uchar(mem(Rn)): RAM_ADDR | @any(addr).(addr: RAM_ADDR & addr = byte_uchar(mem(Rn)) ==> mem:=mem<+ADDRCHANGE(addr,data,mem(PSW))));
  Expanded_List_Substitution(Machine(MEMORY),addrSetDirect)==(addr: MEM_ADDR & data: BYTE | mem:=ADDRCHANGE(addr,data,mem(PSW)));
  Expanded_List_Substitution(Machine(MEMORY),bitClear)==(bit: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = bit ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,0,mem(PSW))));
  Expanded_List_Substitution(Machine(MEMORY),bitSet)==(bit: BIT_ADDRESSABLE | @any(addr,ind).(addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = bit ==> mem:=mem<+BITCHANGE(addr,mem(addr),ind,1,mem(PSW))));
  Expanded_List_Substitution(Machine(MEMORY),setMem)==(addr: MEM_ADDR & data: BYTE | mem:=mem<+{addr|->data});
  Expanded_List_Substitution(Machine(MEMORY),bitGet)==(bit: BIT_ADDRESSABLE | @any(addr,indx).(addr: MEM_ADDR & indx: BYTE_INDEX & addr,indx = bit ==> rr:=mem(addr)(indx)));
  List_Substitution(Machine(MEMORY),bitGet)==(ANY addr,indx WHERE addr: MEM_ADDR & indx: BYTE_INDEX & addr,indx = bit THEN rr:=mem(addr)(indx) END);
  List_Substitution(Machine(MEMORY),setMem)==(mem(addr):=data);
  List_Substitution(Machine(MEMORY),bitSet)==(ANY addr,ind WHERE addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = bit THEN mem:=mem<+BITCHANGE(addr,mem(addr),ind,1,mem(PSW)) END);
  List_Substitution(Machine(MEMORY),bitClear)==(ANY addr,ind WHERE addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = bit THEN mem:=mem<+BITCHANGE(addr,mem(addr),ind,0,mem(PSW)) END);
  List_Substitution(Machine(MEMORY),addrSetDirect)==(mem:=ADDRCHANGE(addr,data,mem(PSW)));
  List_Substitution(Machine(MEMORY),addrSetIndirect)==(ANY addr WHERE addr: RAM_ADDR & addr = byte_uchar(mem(Rn)) THEN mem:=mem<+ADDRCHANGE(addr,data,mem(PSW)) END);
  List_Substitution(Machine(MEMORY),push_two)==(mem:=mem<+{SP_INCREMENT(mem(SP),1)|->data1,SP_INCREMENT(mem(SP),2)|->data2,SP|->uchar_byte(SP_INCREMENT(mem(SP),2))});
  List_Substitution(Machine(MEMORY),push)==(mem(SP_INCREMENT(mem(SP),1)):=data);
  List_Substitution(Machine(MEMORY),pop)==(mem:=mem<+(ADDRCHANGE(addr,mem(SP),mem(PSW))\/{SP|->uchar_byte(SP_DECREMENT(mem(SP),1))}));
  List_Substitution(Machine(MEMORY),update)==(mem:=mem<+{ACC|->uchar_byte(acc),PSW|->PSWUPDATE(mem(PSW),Pa,Ov,Ac,Cy)})
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(MEMORY))==(ROM_LENGTH,ROM_ADDR,ROM_ADDR_MAX,MEM_ADDR,RAM_ADDR,RAM_MIN,RAM_MAX,RAM_BIT_ADDRESSABLE,SFR_ADDR,SFR_MIN,SFR_MAX,SFR_BIT_ADDRESSABLE,BIT_ADDRESS,BIT_ADDRESSABLE,STACK_MAX,PSWUPDATE,BIT_GET,SBUF,PCON,SP,SCON,B98,B99,B9A,B9B,B9C,B9D,B9E,B9F,SCON_0,SCON_1,SCON_2,SCON_3,SCON_4,SCON_5,SCON_6,SCON_7,RI,TI,RB8,TB8,REN,SM2,SM1,SM0,R0,R1,R2,R3,R4,R5,R6,R7,AA,ACC,BE0,BE1,BE2,BE3,BE4,BE5,BE6,BE7,BB,BF0,BF1,BF2,BF3,BF4,BF5,BF6,BF7,P0,P0_0,P0_1,P0_2,P0_3,P0_4,P0_5,P0_6,P0_7,B80,B81,B82,B83,B84,B85,B86,B87,P1,P1_0,P1_1,P1_2,P1_3,P1_4,P1_5,P1_6,P1_7,B90,B91,B92,B93,B94,B95,B96,B97,P2,P2_0,P2_1,P2_2,P2_3,P2_4,P2_5,P2_6,P2_7,BA0,BA1,BA2,BA3,BA4,BA5,BA6,BA7,P3,P3_0,P3_1,P3_2,P3_3,P3_4,P3_5,P3_6,P3_7,BB0,BB1,BB2,BB3,BB4,BB5,BB6,BB7,PSW,PP,OV,RS0,RS1,F0,AC,CY,CC,BD0,BD1,BD2,BD3,BD4,BD5,BD6,BD7,PSW_0,PSW_1,PSW_2,PSW_3,PSW_4,PSW_5,PSW_6,PSW_7,ADDRCHANGE,BITCHANGE,PC_INCREMENT,SP_INCREMENT,SP_DECREMENT);
  Inherited_List_Constants(Machine(MEMORY))==(?);
  List_Constants(Machine(MEMORY))==(ROM_LENGTH,ROM_ADDR,ROM_ADDR_MAX,MEM_ADDR,RAM_ADDR,RAM_MIN,RAM_MAX,RAM_BIT_ADDRESSABLE,SFR_ADDR,SFR_MIN,SFR_MAX,SFR_BIT_ADDRESSABLE,BIT_ADDRESS,BIT_ADDRESSABLE,STACK_MAX,PSWUPDATE,BIT_GET,SBUF,PCON,SP,SCON,B98,B99,B9A,B9B,B9C,B9D,B9E,B9F,SCON_0,SCON_1,SCON_2,SCON_3,SCON_4,SCON_5,SCON_6,SCON_7,RI,TI,RB8,TB8,REN,SM2,SM1,SM0,R0,R1,R2,R3,R4,R5,R6,R7,AA,ACC,BE0,BE1,BE2,BE3,BE4,BE5,BE6,BE7,BB,BF0,BF1,BF2,BF3,BF4,BF5,BF6,BF7,P0,P0_0,P0_1,P0_2,P0_3,P0_4,P0_5,P0_6,P0_7,B80,B81,B82,B83,B84,B85,B86,B87,P1,P1_0,P1_1,P1_2,P1_3,P1_4,P1_5,P1_6,P1_7,B90,B91,B92,B93,B94,B95,B96,B97,P2,P2_0,P2_1,P2_2,P2_3,P2_4,P2_5,P2_6,P2_7,BA0,BA1,BA2,BA3,BA4,BA5,BA6,BA7,P3,P3_0,P3_1,P3_2,P3_3,P3_4,P3_5,P3_6,P3_7,BB0,BB1,BB2,BB3,BB4,BB5,BB6,BB7,PSW,PP,OV,RS0,RS1,F0,AC,CY,CC,BD0,BD1,BD2,BD3,BD4,BD5,BD6,BD7,PSW_0,PSW_1,PSW_2,PSW_3,PSW_4,PSW_5,PSW_6,PSW_7,ADDRCHANGE,BITCHANGE,PC_INCREMENT,SP_INCREMENT,SP_DECREMENT)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(MEMORY))==(?);
  Context_List_Defered(Machine(MEMORY))==(?);
  Context_List_Sets(Machine(MEMORY))==(?);
  List_Valuable_Sets(Machine(MEMORY))==(?);
  Inherited_List_Enumerated(Machine(MEMORY))==(?);
  Inherited_List_Defered(Machine(MEMORY))==(?);
  Inherited_List_Sets(Machine(MEMORY))==(?);
  List_Enumerated(Machine(MEMORY))==(?);
  List_Defered(Machine(MEMORY))==(?);
  List_Sets(Machine(MEMORY))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(MEMORY))==(?);
  Expanded_List_HiddenConstants(Machine(MEMORY))==(?);
  List_HiddenConstants(Machine(MEMORY))==(?);
  External_List_HiddenConstants(Machine(MEMORY))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(MEMORY))==(btrue);
  Context_List_Properties(Machine(MEMORY))==(BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE = {bt | bt: BYTE_INDEX --> BIT & size(bt) = BYTE_WIDTH}-{{}} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & UCHAR_MAX: NATURAL & UCHAR_MAX = 2**BYTE_WIDTH-1 & UCHAR = 0..UCHAR_MAX & SCHAR_MAX: INTEGER & SCHAR_MIN: INTEGER & SCHAR_MAX = 2**(BYTE_WIDTH-1)-1 & SCHAR_MIN = -(2**(BYTE_WIDTH-1)) & SCHAR = SCHAR_MIN..SCHAR_MAX & USHORT_INT_MAX: INTEGER & USHORT_INT_MAX = 2**(2*BYTE_WIDTH)-1 & USHORT_INT = 0..USHORT_INT_MAX & byte_uchar: BYTE --> UCHAR & byte_uchar = %bv.(bv: BYTE | bv_to_nat(bv)) & uchar_byte = byte_uchar~ & byte_schar: BYTE --> SCHAR & byte_schar = %bv.(bv: BYTE | (-bv(7))*128+bv(6)*64+bv(5)*32+bv(4)*16+bv(3)*8+bv(2)*4+bv(1)*2+bv(0)*1) & schar_byte = byte_schar~ & uchar_schar: UCHAR --> SCHAR & uchar_schar = %v1.(v1: UCHAR & v1<=SCHAR_MAX | v1) & uchar_schar = %v1.(v1: UCHAR & not(v1<=SCHAR_MAX) | v1-UCHAR_MAX) & schar_uchar = uchar_schar~ & bv16_usint: BV16 --> USHORT_INT & bv16_usint = %v0.(v0: BV16 | 32768*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & usint_bv16 = bv16_usint~ & BIT_VECTOR = {bt | bt: NATURAL +-> BIT}-{{}} & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & BV_INDX: BIT_VECTOR --> POW(NATURAL) & BV_INDX = %bv.(bv: BIT_VECTOR | 0..bv_size(bv)-1) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high | low..high<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (0..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (0..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: BV_INDX(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1(idx)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: BV_INDX(v1) & bit: BIT | v1<+{idx|->bit}) & bv_to_nat: BIT_VECTOR --> NATURAL & bv_to_nat = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | 2**idx*bv(idx))) & bv_par: BIT_VECTOR --> BIT & bv_par = %bv.(bv: BIT_VECTOR | size(bv|>{1}) mod 2));
  Inherited_List_Properties(Machine(MEMORY))==(btrue);
  List_Properties(Machine(MEMORY))==(ROM_LENGTH = 4 & ROM_ADDR_MAX = ROM_LENGTH*2**10 & ROM_ADDR = 0..ROM_ADDR_MAX & MEM_ADDR = UCHAR & RAM_MIN: UCHAR & RAM_MAX: UCHAR & RAM_MIN = 0 & RAM_MAX = 127 & RAM_ADDR = RAM_MIN..RAM_MAX & RAM_BIT_ADDRESSABLE = 32..47 & R0: RAM_ADDR & R1: RAM_ADDR & R2: RAM_ADDR & R3: RAM_ADDR & R4: RAM_ADDR & R5: RAM_ADDR & R6: RAM_ADDR & R7: RAM_ADDR & R0 = 0 & R1 = 1 & R2 = 2 & R3 = 3 & R4 = 4 & R5 = 5 & R6 = 6 & R7 = 7 & SFR_MIN: UCHAR & SFR_MAX: UCHAR & SFR_MIN = RAM_MAX+1 & SFR_MAX = UCHAR_MAX & SFR_ADDR = SFR_MIN..SFR_MAX & SFR_BIT_ADDRESSABLE = {ba | ba: SFR_ADDR & ba>=128 & ba<=240 & ba mod 8 = 0} & BIT_ADDRESS = MEM_ADDR*BYTE_INDEX & BIT_ADDRESSABLE = {addr,ind | addr: MEM_ADDR & ind: BYTE_INDEX & addr: SFR_BIT_ADDRESSABLE\/RAM_BIT_ADDRESSABLE} & SBUF: SFR_ADDR & SP: SFR_ADDR & PCON: SFR_ADDR & SBUF = 153 & SP = 109 & PCON = 135 & BB: SFR_ADDR & BB = 240 & BF0: BIT_ADDRESS & BF1: BIT_ADDRESS & BF2: BIT_ADDRESS & BF3: BIT_ADDRESS & BF4: BIT_ADDRESS & BF5: BIT_ADDRESS & BF6: BIT_ADDRESS & BF7: BIT_ADDRESS & BF0 = BB,0 & BF1 = BB,1 & BF2 = BB,2 & BF3 = BB,3 & BF4 = BB,4 & BF5 = BB,5 & BF6 = BB,6 & BF7 = BB,7 & AA: SFR_ADDR & AA = 224 & ACC = AA & BE0: BIT_ADDRESS & BE1: BIT_ADDRESS & BE2: BIT_ADDRESS & BE3: BIT_ADDRESS & BE4: BIT_ADDRESS & BE5: BIT_ADDRESS & BE6: BIT_ADDRESS & BE7: BIT_ADDRESS & BE0 = ACC,0 & BE1 = ACC,1 & BE2 = ACC,2 & BE3 = ACC,3 & BE4 = ACC,4 & BE5 = ACC,5 & BE6 = ACC,6 & BE7 = ACC,7 & P0: SFR_ADDR & P0 = 128 & P0_0: BIT_ADDRESS & P0_1: BIT_ADDRESS & P0_2: BIT_ADDRESS & P0_3: BIT_ADDRESS & P0_4: BIT_ADDRESS & P0_5: BIT_ADDRESS & P0_6: BIT_ADDRESS & P0_7: BIT_ADDRESS & B80: BIT_ADDRESS & B81: BIT_ADDRESS & B82: BIT_ADDRESS & B83: BIT_ADDRESS & B84: BIT_ADDRESS & B85: BIT_ADDRESS & B86: BIT_ADDRESS & B87: BIT_ADDRESS & P0_0 = P0,0 & P0_1 = P0,1 & P0_2 = P0,2 & P0_3 = P0,3 & P0_4 = P0,4 & P0_5 = P0,5 & P0_6 = P0,6 & P0_7 = P0,7 & B80 = P0,0 & B81 = P0,1 & B82 = P0,2 & B83 = P0,3 & B84 = P0,4 & B85 = P0,5 & B86 = P0,6 & B87 = P0,7 & P1: SFR_ADDR & P1 = 144 & P1_0: BIT_ADDRESS & P1_1: BIT_ADDRESS & P1_2: BIT_ADDRESS & P1_3: BIT_ADDRESS & P1_4: BIT_ADDRESS & P1_5: BIT_ADDRESS & P1_6: BIT_ADDRESS & P1_7: BIT_ADDRESS & B90: BIT_ADDRESS & B91: BIT_ADDRESS & B92: BIT_ADDRESS & B93: BIT_ADDRESS & B94: BIT_ADDRESS & B95: BIT_ADDRESS & B96: BIT_ADDRESS & B97: BIT_ADDRESS & P1_0 = P1,0 & P1_1 = P1,1 & P1_2 = P1,2 & P1_3 = P1,3 & P1_4 = P1,4 & P1_5 = P1,5 & P1_6 = P1,6 & P1_7 = P1,7 & B90 = P1,0 & B91 = P1,1 & B92 = P1,2 & B93 = P1,3 & B94 = P1,4 & B95 = P1,5 & B96 = P1,6 & B97 = P1,7 & P2: SFR_ADDR & P2 = 160 & P2_0: BIT_ADDRESS & P2_1: BIT_ADDRESS & P2_2: BIT_ADDRESS & P2_3: BIT_ADDRESS & P2_4: BIT_ADDRESS & P2_5: BIT_ADDRESS & P2_6: BIT_ADDRESS & P2_7: BIT_ADDRESS & BA0: BIT_ADDRESS & BA1: BIT_ADDRESS & BA2: BIT_ADDRESS & BA3: BIT_ADDRESS & BA4: BIT_ADDRESS & BA5: BIT_ADDRESS & BA6: BIT_ADDRESS & BA7: BIT_ADDRESS & P2_0 = P2,0 & P2_1 = P2,1 & P2_2 = P2,2 & P2_3 = P2,3 & P2_4 = P2,4 & P2_5 = P2,5 & P2_6 = P2,6 & P2_7 = P2,7 & BA0 = P2,0 & BA1 = P2,1 & BA2 = P2,2 & BA3 = P2,3 & BA4 = P2,4 & BA5 = P2,5 & BA6 = P2,6 & BA7 = P2,7 & P3: SFR_ADDR & P3 = 176 & P3_0: BIT_ADDRESS & P3_1: BIT_ADDRESS & P3_2: BIT_ADDRESS & P3_3: BIT_ADDRESS & P3_4: BIT_ADDRESS & P3_5: BIT_ADDRESS & P3_6: BIT_ADDRESS & P3_7: BIT_ADDRESS & BB0: BIT_ADDRESS & BB1: BIT_ADDRESS & BB2: BIT_ADDRESS & BB3: BIT_ADDRESS & BB4: BIT_ADDRESS & BB5: BIT_ADDRESS & BB6: BIT_ADDRESS & BB7: BIT_ADDRESS & P3_0 = P3,0 & P3_1 = P3,1 & P3_2 = P3,2 & P3_3 = P3,3 & P3_4 = P3,4 & P3_5 = P3,5 & P3_6 = P3,6 & P3_7 = P3,7 & BB0 = P3,0 & BB1 = P3,1 & BB2 = P3,2 & BB3 = P3,3 & BB4 = P3,4 & BB5 = P3,5 & BB6 = P3,6 & BB7 = P3,7 & SCON: SFR_ADDR & SCON = 152 & B98: BIT_ADDRESS & B99: BIT_ADDRESS & B9A: BIT_ADDRESS & B9B: BIT_ADDRESS & B9C: BIT_ADDRESS & B9D: BIT_ADDRESS & B9E: BIT_ADDRESS & B9F: BIT_ADDRESS & B98 = SCON,0 & B99 = SCON,1 & B9A = SCON,2 & B9B = SCON,3 & B9C = SCON,4 & B9D = SCON,5 & B9E = SCON,6 & B9F = SCON,7 & SCON_0: BIT_ADDRESS & SCON_1: BIT_ADDRESS & SCON_2: BIT_ADDRESS & SCON_3: BIT_ADDRESS & SCON_4: BIT_ADDRESS & SCON_5: BIT_ADDRESS & SCON_6: BIT_ADDRESS & SCON_7: BIT_ADDRESS & SCON_0 = SCON,0 & SCON_1 = SCON,1 & SCON_2 = SCON,2 & SCON_3 = SCON,3 & SCON_4 = SCON,4 & SCON_5 = SCON,5 & SCON_6 = SCON,6 & SCON_7 = SCON,7 & RI: BIT_ADDRESS & TI: BIT_ADDRESS & RB8: BIT_ADDRESS & TB8: BIT_ADDRESS & REN: BIT_ADDRESS & SM2: BIT_ADDRESS & SM1: BIT_ADDRESS & SM0: BIT_ADDRESS & RI = SCON,0 & TI = SCON,1 & RB8 = SCON,2 & TB8 = SCON,3 & REN = SCON,4 & SM2 = SCON,5 & SM1 = SCON,6 & SM0 = SCON,7 & PSW: SFR_ADDR & PSW = 208 & PP: BIT_ADDRESS & OV: BIT_ADDRESS & RS0: BIT_ADDRESS & RS1: BIT_ADDRESS & F0: BIT_ADDRESS & AC: BIT_ADDRESS & CY: BIT_ADDRESS & CC: BIT_ADDRESS & BD0: BIT_ADDRESS & BD1: BIT_ADDRESS & BD2: BIT_ADDRESS & BD3: BIT_ADDRESS & BD4: BIT_ADDRESS & BD5: BIT_ADDRESS & BD6: BIT_ADDRESS & BD7: BIT_ADDRESS & BD0 = PSW,0 & BD1 = PSW,1 & BD2 = PSW,2 & BD3 = PSW,3 & BD4 = PSW,4 & BD5 = PSW,5 & BD6 = PSW,6 & BD7 = PSW,7 & PSW_0: BIT_ADDRESS & PSW_1: BIT_ADDRESS & PSW_2: BIT_ADDRESS & PSW_3: BIT_ADDRESS & PSW_4: BIT_ADDRESS & PSW_5: BIT_ADDRESS & PSW_6: BIT_ADDRESS & PSW_7: BIT_ADDRESS & PSW_0 = PSW,0 & PSW_1 = PSW,1 & PSW_2 = PSW,2 & PSW_3 = PSW,3 & PSW_4 = PSW,4 & PSW_5 = PSW,5 & PSW_6 = PSW,6 & PSW_7 = PSW,7 & PP = PSW,0 & OV = PSW,2 & RS0 = PSW,3 & RS1 = PSW,4 & F0 = PSW,5 & AC = PSW,6 & CY = PSW,7 & CC = PSW,7 & STACK_MAX: RAM_ADDR & STACK_MAX = RAM_MAX & PC_INCREMENT: ROM_ADDR*ROM_ADDR --> ROM_ADDR & PC_INCREMENT = %(ii,jj).(ii: ROM_ADDR & jj: ROM_ADDR | (ii+jj) mod (ROM_ADDR_MAX+1)) & ADDRCHANGE: MEM_ADDR*BYTE*BYTE --> (MEM_ADDR +-> BYTE) & ADDRCHANGE = %(addr,data,psw).(addr: MEM_ADDR & data: BYTE & psw: BYTE & addr = ACC | {addr|->data,PSW|->bv_put(psw,0,bv_par(data))}) & ADDRCHANGE = %(addr,data,psw).(addr: MEM_ADDR & data: BYTE & psw: BYTE & addr/=ACC & addr/=PSW | {addr|->data}) & BITCHANGE: MEM_ADDR*BYTE*BYTE_INDEX*BIT*BYTE --> (MEM_ADDR +-> BYTE) & BITCHANGE = %(addr,data,ind,bit,psw).(addr: MEM_ADDR & data: BYTE & ind: BYTE_INDEX & bit: BIT & psw: BYTE & addr = ACC | ADDRCHANGE(addr,bv_put(data,ind,bit),psw)) & BITCHANGE = %(addr,data,ind,bit,psw).(addr: MEM_ADDR & data: BYTE & ind: BYTE_INDEX & bit: BIT & psw: BYTE & addr/=ACC | {addr|->bv_put(data,ind,bit)}) & SP_INCREMENT: BYTE*ROM_ADDR --> RAM_ADDR & SP_INCREMENT = %(sp,inc).(sp: BYTE & inc: RAM_ADDR | (byte_uchar(sp)+inc) mod STACK_MAX) & SP_DECREMENT: BYTE*ROM_ADDR --> RAM_ADDR & SP_DECREMENT = %(sp,dec).(sp: BYTE & dec: RAM_ADDR & byte_uchar(sp)-dec>0 | byte_uchar(sp)-dec) & SP_DECREMENT = %(sp,dec).(sp: BYTE & dec: RAM_ADDR & not(byte_uchar(sp)-dec>0) | STACK_MAX-(byte_uchar(sp)-dec)) & PSWUPDATE: BYTE*BIT*BIT*BIT*BIT --> BYTE & PSWUPDATE = %(psw,Pa,Ov,Ac,Cy).(psw: BYTE & Pa: BIT & Ov: BIT & Ac: BIT & Cy: BIT | bv_put(bv_put(bv_put(bv_put(psw,0,Pa),2,Ov),6,Ac),7,Cy)) & BIT_GET: BIT_ADDRESS*(MEM_ADDR +-> BYTE) --> BIT & !(bit,memory,addr,ind).(bit: BIT_ADDRESS & memory: MEM_ADDR +-> BYTE & addr: MEM_ADDR & ind: BYTE_INDEX & addr,ind = bit => BIT_GET(bit,memory) = memory(addr)(ind)))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(POWER2))==(?);
  Seen_Context_List_Enumerated(Machine(MEMORY))==(?);
  Seen_Context_List_Invariant(Machine(MEMORY))==(btrue);
  Seen_Context_List_Assertions(Machine(MEMORY))==(bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_to_bit(TRUE) = 1 & bool_to_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_not(bv_not(bv))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_and(v1,v2)(indx) = bv_and(v2,v1)(indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_and(v1,bv_and(v2,v3))(indx) = bv_and(bv_and(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_zero(bv_size(bv)))(indx) = bv_zero(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_one(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_or(v1,v2)(indx) = bv_or(v2,v1)(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_or(v1,bv_or(v2,v3))(indx) = bv_or(bv_or(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_one(bv_size(bv)))(indx) = bv_one(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_zero(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_xor(v1,v2)(indx) = bv_xor(v2,v1)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_xor(bv,bv)(indx) = bv_zero(bv_size(bv))(indx))) & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536) & BV16_ZERO: BIT_VECTOR & BV16 <: BIT_VECTOR & BV16_ZERO: BV16 & !ss.(ss: NATURAL1 => bv_par(bv_zero(ss)) = 0) & !ss.(ss: NATURAL1 => bv_to_nat(bv_zero(ss)) = 0) & first(BYTE_ZERO) = 0 & BYTE_ZERO: BIT_VECTOR & BYTE <: BIT_VECTOR & size(BYTE_ZERO) = 8 & !bt.(bt: BYTE => size(bt) = 8) & !(nn,pp).(nn: NATURAL1 & pp: NAT => (pp = 0 => nn**pp = 1) & (pp = 1 => nn**pp = nn) & (pp>1 => nn**pp = nn*nn**(pp-1))));
  Seen_Context_List_Properties(Machine(MEMORY))==(BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = {bt | bt: NATURAL +-> BIT}-{{}} & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & BV_INDX: BIT_VECTOR --> POW(NATURAL) & BV_INDX = %bv.(bv: BIT_VECTOR | 0..bv_size(bv)-1) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high | low..high<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (0..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (0..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: BV_INDX(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1(idx)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: BV_INDX(v1) & bit: BIT | v1<+{idx|->bit}) & bv16_byte = %bv.(bv: BV16 | {0|->bv(8),1|->bv(9),2|->bv(10),3|->bv(11),4|->bv(12),5|->bv(13),6|->bv(14),7|->bv(15)},{0|->bv(0),1|->bv(1),2|->bv(2),3|->bv(3),4|->bv(4),5|->bv(5),6|->bv(6),7|->bv(7)}) & bv16_byte: BV16 --> BYTE*BYTE & BV16_ZERO = BV16_INDX*{0} & BV16 = {bt | bt: BV16_INDX --> BIT & size(bt) = 16}-{{}} & BV16_INDX = 0..BV16_WIDTH-1 & BV16_WIDTH = 16 & BV16_WIDTH: NATURAL & bv_par = %bv.(bv: BIT_VECTOR | size(bv|>{1}) mod 2) & bv_par: BIT_VECTOR --> BIT & bv_to_nat = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | 2**idx*bv(idx))) & bv_to_nat: BIT_VECTOR --> NATURAL & BYTE_ZERO = BYTE_INDEX*{0} & BYTE_ZERO: BYTE & BYTE = {bt | bt: BYTE_INDEX --> BIT & size(bt) = BYTE_WIDTH}-{{}} & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE_WIDTH = 8);
  Seen_List_Constraints(Machine(MEMORY))==(btrue);
  Seen_List_Operations(Machine(MEMORY),Machine(POWER2))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(POWER2))==(btrue);
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Seen_List_Operations(Machine(MEMORY),Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(BIT_VECTOR_ARITHMETICS))==(btrue);
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(BIT_VECTOR_DEFINITION))==(?);
  Seen_List_Operations(Machine(MEMORY),Machine(BIT_VECTOR_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(TYPES))==(?);
  Seen_List_Operations(Machine(MEMORY),Machine(TYPES))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(TYPES))==(btrue);
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(BIT_DEFINITION))==(?);
  Seen_List_Operations(Machine(MEMORY),Machine(BIT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(BIT_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(BYTE_DEFINITION))==(?);
  Seen_List_Operations(Machine(MEMORY),Machine(BYTE_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(BYTE_DEFINITION))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(MEMORY),bitGet)==((Var(addr) == btype(INTEGER,?,?)),(Var(indx) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(MEMORY),setMem)==(?);
  List_ANY_Var(Machine(MEMORY),bitSet)==((Var(addr) == btype(INTEGER,?,?)),(Var(ind) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(MEMORY),bitClear)==((Var(addr) == btype(INTEGER,?,?)),(Var(ind) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(MEMORY),addrSetDirect)==(?);
  List_ANY_Var(Machine(MEMORY),addrSetIndirect)==(Var(addr) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(MEMORY),push_two)==(?);
  List_ANY_Var(Machine(MEMORY),push)==(?);
  List_ANY_Var(Machine(MEMORY),pop)==(?);
  List_ANY_Var(Machine(MEMORY),update)==(?);
  List_ANY_Var(Machine(MEMORY),?)==(?)
END
&
THEORY ListOfIdsX IS
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
  List_Of_Ids_SeenBNU(Machine(BYTE_DEFINITION)) == (?: ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(MEMORY)) == (Type(ROM_LENGTH) == Cst(btype(INTEGER,?,?));Type(ROM_ADDR) == Cst(SetOf(btype(INTEGER,"[ROM_ADDR","]ROM_ADDR")));Type(ROM_ADDR_MAX) == Cst(btype(INTEGER,?,?));Type(MEM_ADDR) == Cst(SetOf(btype(INTEGER,"[MEM_ADDR","]MEM_ADDR")));Type(RAM_ADDR) == Cst(SetOf(btype(INTEGER,"[RAM_ADDR","]RAM_ADDR")));Type(RAM_MIN) == Cst(btype(INTEGER,?,?));Type(RAM_MAX) == Cst(btype(INTEGER,?,?));Type(RAM_BIT_ADDRESSABLE) == Cst(SetOf(btype(INTEGER,"[RAM_BIT_ADDRESSABLE","]RAM_BIT_ADDRESSABLE")));Type(SFR_ADDR) == Cst(SetOf(btype(INTEGER,"[SFR_ADDR","]SFR_ADDR")));Type(SFR_MIN) == Cst(btype(INTEGER,?,?));Type(SFR_MAX) == Cst(btype(INTEGER,?,?));Type(SFR_BIT_ADDRESSABLE) == Cst(SetOf(btype(INTEGER,"[SFR_BIT_ADDRESSABLE","]SFR_BIT_ADDRESSABLE")));Type(BIT_ADDRESS) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(BIT_ADDRESSABLE) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(STACK_MAX) == Cst(btype(INTEGER,?,?));Type(PSWUPDATE) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(BIT_GET) == Cst(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*btype(INTEGER,"[BIT","]BIT")));Type(SBUF) == Cst(btype(INTEGER,?,?));Type(PCON) == Cst(btype(INTEGER,?,?));Type(SP) == Cst(btype(INTEGER,?,?));Type(SCON) == Cst(btype(INTEGER,?,?));Type(B98) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B99) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9A) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9B) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9C) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9D) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9E) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9F) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RI) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(TI) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RB8) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(TB8) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(REN) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SM2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SM1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SM0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(R0) == Cst(btype(INTEGER,?,?));Type(R1) == Cst(btype(INTEGER,?,?));Type(R2) == Cst(btype(INTEGER,?,?));Type(R3) == Cst(btype(INTEGER,?,?));Type(R4) == Cst(btype(INTEGER,?,?));Type(R5) == Cst(btype(INTEGER,?,?));Type(R6) == Cst(btype(INTEGER,?,?));Type(R7) == Cst(btype(INTEGER,?,?));Type(AA) == Cst(btype(INTEGER,?,?));Type(ACC) == Cst(btype(INTEGER,?,?));Type(BE0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB) == Cst(btype(INTEGER,?,?));Type(BF0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0) == Cst(btype(INTEGER,?,?));Type(P0_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B80) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B81) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B82) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B83) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B84) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B85) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B86) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B87) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1) == Cst(btype(INTEGER,?,?));Type(P1_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B90) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B91) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B92) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B93) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B94) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B95) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B96) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B97) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2) == Cst(btype(INTEGER,?,?));Type(P2_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3) == Cst(btype(INTEGER,?,?));Type(P3_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW) == Cst(btype(INTEGER,?,?));Type(PP) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(OV) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RS0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RS1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(F0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(AC) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(CY) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(CC) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(ADDRCHANGE) == Cst(SetOf(btype(INTEGER,"[MEM_ADDR","]MEM_ADDR")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))));Type(BITCHANGE) == Cst(SetOf(btype(INTEGER,"[MEM_ADDR","]MEM_ADDR")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BYTE_INDEX","]BYTE_INDEX")*btype(INTEGER,"[BIT","]BIT")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))));Type(PC_INCREMENT) == Cst(SetOf(btype(INTEGER,"[ROM_ADDR","]ROM_ADDR")*btype(INTEGER,"[ROM_ADDR","]ROM_ADDR")*btype(INTEGER,"[ROM_ADDR","]ROM_ADDR")));Type(SP_INCREMENT) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[ROM_ADDR","]ROM_ADDR")*btype(INTEGER,"[RAM_ADDR","]RAM_ADDR")));Type(SP_DECREMENT) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[ROM_ADDR","]ROM_ADDR")*btype(INTEGER,"[RAM_ADDR","]RAM_ADDR"))))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(MEMORY)) == (Type(mem) == Mvl(SetOf(btype(INTEGER,"[MEM_ADDR","]MEM_ADDR")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(MEMORY)) == (Type(update) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(pop) == Cst(No_type,btype(INTEGER,?,?));Type(push) == Cst(No_type,SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(push_two) == Cst(No_type,SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(addrSetIndirect) == Cst(No_type,btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(addrSetDirect) == Cst(No_type,btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(bitClear) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(bitSet) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(setMem) == Cst(No_type,btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(bitGet) == Cst(btype(INTEGER,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  Observers(Machine(MEMORY)) == (Type(bitGet) == Cst(btype(INTEGER,?,?),btype(INTEGER,?,?)*btype(INTEGER,?,?)))
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
