Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Z80Man_bitControl))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Z80Man_bitControl))==(Machine(Z80Man_bitControl));
  Level(Machine(Z80Man_bitControl))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Z80Man_bitControl)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Z80Man_bitControl))==(ALU,BIT_DEFINITION,BIT_VECTOR_DEFINITION,BYTE_DEFINITION,BV16_DEFINITION,UCHAR_DEFINITION,SCHAR_DEFINITION,SSHORT_DEFINITION,USHORT_DEFINITION,POWER2)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Z80Man_bitControl))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Z80Man_bitControl))==(MEMORY);
  List_Includes(Machine(Z80Man_bitControl))==(MEMORY)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Z80Man_bitControl))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Z80Man_bitControl))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Z80Man_bitControl))==(?);
  Context_List_Variables(Machine(Z80Man_bitControl))==(?);
  Abstract_List_Variables(Machine(Z80Man_bitControl))==(?);
  Local_List_Variables(Machine(Z80Man_bitControl))==(i_o_ports,im,iff2,iff1,r_,i_,iy,ix,sp,pc,rgs8);
  List_Variables(Machine(Z80Man_bitControl))==(i_o_ports,im,iff2,iff1,r_,i_,iy,ix,sp,pc,rgs8,stack,mem);
  External_List_Variables(Machine(Z80Man_bitControl))==(i_o_ports,im,iff2,iff1,r_,i_,iy,ix,sp,pc,rgs8,stack,mem)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Z80Man_bitControl))==(?);
  Abstract_List_VisibleVariables(Machine(Z80Man_bitControl))==(?);
  External_List_VisibleVariables(Machine(Z80Man_bitControl))==(?);
  Expanded_List_VisibleVariables(Machine(Z80Man_bitControl))==(?);
  List_VisibleVariables(Machine(Z80Man_bitControl))==(?);
  Internal_List_VisibleVariables(Machine(Z80Man_bitControl))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Z80Man_bitControl))==(btrue);
  Gluing_List_Invariant(Machine(Z80Man_bitControl))==(btrue);
  Abstract_List_Invariant(Machine(Z80Man_bitControl))==(btrue);
  Expanded_List_Invariant(Machine(Z80Man_bitControl))==(mem: BV16 --> BYTE & stack: BV16 --> BYTE & stack <<: mem);
  Context_List_Invariant(Machine(Z80Man_bitControl))==(btrue);
  List_Invariant(Machine(Z80Man_bitControl))==(rgs8: id_reg_8 --> BYTE & pc: INSTRUCTION & sp: BV16 & ix: BV16 & iy: BV16 & i_: BYTE & r_: BYTE & iff1: BIT & iff2: BIT & im: BIT*BIT & i_o_ports: BYTE --> BYTE)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Machine(Z80Man_bitControl))==(btrue);
  Expanded_List_Assertions(Machine(Z80Man_bitControl))==(dom(stack) = BV16 & ran(stack) <: BYTE & ran(mem) <: BYTE & dom(mem) = BV16 & !(address,value).(address: BV16 & value: BYTE => mem<+{address|->value}: BV16 --> BYTE) & !(address,value).(address: BV16 & value: BYTE => stack<+{address|->value}: BV16 --> BYTE));
  Context_List_Assertions(Machine(Z80Man_bitControl))==(dom(add8UCHAR) = BIT*UCHAR*UCHAR & ran(add8UCHAR): POW(UCHAR*BIT*BIT*BIT*BIT) & dom(substract8UCHAR) = BIT*UCHAR*UCHAR & ran(substract8UCHAR): POW(UCHAR*BIT*BIT*BIT*BIT) & dom(and) = BYTE*BYTE & ran(and) <: BYTE & dom(ior) = BYTE*BYTE & ran(ior) <: BYTE & dom(xor) = BYTE*BYTE & ran(xor) <: BYTE & dom(complement) = BYTE & ran(complement) <: BYTE & dom(swap) = BYTE & ran(swap) <: BYTE & dom(rotateleft) = BYTE & ran(rotateleft) <: BYTE & dom(rotateright) = BYTE & ran(rotateright) <: BYTE & !(vec,in0).(vec: BYTE & in0: 0..7 => bitget(vec,in0) = vec(in0)) & !(x0,x1).(x0: UCHAR & x1: UCHAR => simple_add8UCHAR(x0,x1): UCHAR) & 0 = schar_sshort(0,0) & !n0.(n0: UCHAR => 0<=n0) & !n0.(n0: UCHAR => n0<=255) & instruction_next: USHORT --> USHORT & !xx.(xx: BYTE => byte_uchar(xx): UCHAR) & !xx.(xx: UCHAR => uchar_byte(xx): BYTE) & !xx.(xx: BYTE => update_refresh_reg(xx): BYTE) & !xx.(xx: BYTE => byte_schar(xx): SCHAR) & !xx.(xx: SCHAR => schar_byte(xx): BYTE) & !(xx,yy).(xx: BYTE & yy: BYTE => byte_bv16(xx,yy): BV16) & !(xx,yy).(xx: BYTE & yy: BYTE => #zz.(zz: BV16 & byte_bv16(xx,yy) = zz)) & !xx.(xx: BV16 => bv16_ushort(xx): USHORT) & !xx.(xx: USHORT => ushort_bv16(xx): BV16) & !xx.(xx: BYTE => get_upper_digit(xx): 0..16) & !xx.(xx: BYTE => get_lower_digit(xx): 0..16) & !(zn,c0,value,h0).(zn: BIT & c0: BIT & value: BYTE & h0: BIT => daa_function(zn,c0,h0,value): BYTE*BIT*BIT) & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_bit(TRUE) = 1 & bool_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_not(bv_not(bv)),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: 0..bv_size(bv)-1 & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,v2),indx) = bv_get(bv_and(v2,v1),indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,bv_and(v2,v3)),indx) = bv_get(bv_and(bv_and(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv_zero(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_one(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,v2),indx) = bv_get(bv_or(v2,v1),indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,bv_or(v2,v3)),indx) = bv_get(bv_or(bv_or(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_one(bv_size(bv))),indx) = bv_get(bv_one(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_xor(v1,v2),indx) = bv_get(bv_xor(v2,v1),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_xor(bv,bv),indx) = bv_get(bv_zero(bv_size(bv)),indx))) & !bt.(bt: BYTE => size(bt) = 8) & size(BYTE_ZERO) = 8 & BYTE <: BIT_VECTOR & BYTE_ZERO: BIT_VECTOR & first(BYTE_ZERO) = 0 & 8: NATURAL & BV16_ZERO: BV16 & BV16 <: BIT_VECTOR & BV16_ZERO: BIT_VECTOR & 2**8-1: INTEGER & 0: INTEGER & (2**7-1: INTEGER;(-2)**7: INTEGER) & (-(2**15): SSHORT;2**15-1: SSHORT) & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536));
  List_Assertions(Machine(Z80Man_bitControl))==(dom(stack) = BV16 & ran(stack) <: BYTE & ran(mem) <: BYTE & dom(mem) = BV16 & ran(rgs8) <: BYTE & dom(rgs8) = id_reg_8 & instruction_next(0) = 1 & instruction_next(1) = 2 & instruction_next(2) = 3 & instruction_next(3) = 4 & instruction_next(4) = 5 & instruction_next(5) = 6 & instruction_next(6) = 7 & instruction_next(7) = 8 & instruction_next(8) = 9 & instruction_next(9) = 10 & instruction_next(10) = 11 & instruction_next(11) = 12 & instruction_next(12) = 13 & instruction_next(13) = 14 & mem(byte_bv16(rgs8(b0),rgs8(c0))): BYTE & mem(byte_bv16(schar_byte(0),mem(byte_bv16(rgs8(b0),rgs8(c0))))): BYTE & mem(byte_bv16(rgs8(d0),rgs8(e0))): BYTE & mem(byte_bv16(schar_byte(0),mem(byte_bv16(rgs8(d0),rgs8(e0))))): BYTE & mem(byte_bv16(rgs8(h0),rgs8(l0))): BYTE & mem(byte_bv16(schar_byte(0),mem(byte_bv16(rgs8(h0),rgs8(l0))))): BYTE & mem(byte_bv16(rgs8(a0),rgs8(f0))): BYTE & mem(byte_bv16(schar_byte(0),mem(byte_bv16(rgs8(a0),rgs8(f0))))): BYTE & mem(sp): BYTE & mem(ix): BYTE & mem(iy): BYTE & !(ii,des).(ii: BV16 & des: SCHAR => bv_ireg_plus_d(ii,des): BV16) & !(mmm,ii,des).(mmm: BV16 >-> BYTE & ii: BV16 & des: SCHAR => bv_9ireg_plus_d0(mmm,ii,des): BYTE))
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Z80Man_bitControl))==(@(mem$0).(mem$0: BV16 --> BYTE ==> mem:=mem$0) || @(stack$0).(stack$0: BV16 --> BYTE ==> stack:=stack$0);(rgs8,pc,sp,ix,iy,i_,r_:={a0|->uchar_byte(255),f0|->uchar_byte(255),f_0|->uchar_byte(255),a_0|->uchar_byte(255),b0|->uchar_byte(255),c0|->uchar_byte(255),b_0|->uchar_byte(255),c_0|->uchar_byte(255),d0|->uchar_byte(255),e0|->uchar_byte(255),d_0|->uchar_byte(255),e_0|->uchar_byte(255),h0|->uchar_byte(255),l0|->uchar_byte(255),h_0|->uchar_byte(255),l_0|->uchar_byte(255)},0,ushort_bv16(65535),ushort_bv16(65535),ushort_bv16(65535),uchar_byte(0),uchar_byte(0) || @(i_o_ports$0).(i_o_ports$0: BYTE --> {uchar_byte(0)} ==> i_o_ports:=i_o_ports$0) || iff1:=0 || iff2:=0 || im:=0|->0));
  Context_List_Initialisation(Machine(Z80Man_bitControl))==(skip);
  List_Initialisation(Machine(Z80Man_bitControl))==(rgs8:={a0|->uchar_byte(255),f0|->uchar_byte(255),f_0|->uchar_byte(255),a_0|->uchar_byte(255),b0|->uchar_byte(255),c0|->uchar_byte(255),b_0|->uchar_byte(255),c_0|->uchar_byte(255),d0|->uchar_byte(255),e0|->uchar_byte(255),d_0|->uchar_byte(255),e_0|->uchar_byte(255),h0|->uchar_byte(255),l0|->uchar_byte(255),h_0|->uchar_byte(255),l_0|->uchar_byte(255)} || pc:=0 || sp:=ushort_bv16(65535) || ix:=ushort_bv16(65535) || iy:=ushort_bv16(65535) || i_:=uchar_byte(0) || r_:=uchar_byte(0) || i_o_ports:: BYTE --> {uchar_byte(0)} || iff1:=0 || iff2:=0 || im:=0|->0)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Z80Man_bitControl))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(Z80Man_bitControl),Machine(MEMORY))==(?);
  List_Instanciated_Parameters(Machine(Z80Man_bitControl),Machine(ALU))==(?);
  List_Instanciated_Parameters(Machine(Z80Man_bitControl),Machine(BIT_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(Z80Man_bitControl),Machine(BIT_VECTOR_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(Z80Man_bitControl),Machine(BYTE_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(Z80Man_bitControl),Machine(BV16_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(Z80Man_bitControl),Machine(UCHAR_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(Z80Man_bitControl),Machine(SCHAR_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(Z80Man_bitControl),Machine(SSHORT_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(Z80Man_bitControl),Machine(USHORT_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(Z80Man_bitControl),Machine(POWER2))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Machine(Z80Man_bitControl),Machine(MEMORY))==(btrue);
  List_Context_Constraints(Machine(Z80Man_bitControl))==(btrue);
  List_Constraints(Machine(Z80Man_bitControl))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Z80Man_bitControl))==(RLCA,RLA,RRCA,RRA,RLC_r,RLC_9HL0,RLC_9IX_d9,RLC_9IY_d9,RL_r,RL_9HL,RL_9IX_d9,RL_9IY_d9,RRC_r,RRC_9HL0,RRC_9IX_d9,RRC_9IY_d9,RR_r,RR_9HL,RR_9IX_d9,RR_9IY_d9,SLA_r,SLA_9HL,SLA_9IX_d9,SLA_9IY_d9,SRA_r,SRA_9HL0,SRA_9IX_d9,SRA_9IY_d9,SRL_r,SRL_9HL0,SRL_9IX_d9,SRL_9IY_d9,RLD,RRD,BIT_b_rr,BIT_b_9HL0,BIT_b_9IX_d0,BIT_b_9IY_d0,SET_b_r,SET_b_9HL0,SET_b_9IX_d0,SET_b_9IY_d0,RES_b_r,RES_b_9HL0,RES_b_9IX_d0,RES_b_9IY_d0,JP_nn,JP_cc_nn,JR_e,JR_C_e,JR_NC_e,JR_Z_e,JR_NZ_e,JP_HL,JP_IX,JP_IY,DJNZ_e,CALL_nn,CALL_cc_nn,RET,RET_cc,RETI,RETN,RST_p);
  List_Operations(Machine(Z80Man_bitControl))==(RLCA,RLA,RRCA,RRA,RLC_r,RLC_9HL0,RLC_9IX_d9,RLC_9IY_d9,RL_r,RL_9HL,RL_9IX_d9,RL_9IY_d9,RRC_r,RRC_9HL0,RRC_9IX_d9,RRC_9IY_d9,RR_r,RR_9HL,RR_9IX_d9,RR_9IY_d9,SLA_r,SLA_9HL,SLA_9IX_d9,SLA_9IY_d9,SRA_r,SRA_9HL0,SRA_9IX_d9,SRA_9IY_d9,SRL_r,SRL_9HL0,SRL_9IX_d9,SRL_9IY_d9,RLD,RRD,BIT_b_rr,BIT_b_9HL0,BIT_b_9IX_d0,BIT_b_9IY_d0,SET_b_r,SET_b_9HL0,SET_b_9IX_d0,SET_b_9IY_d0,RES_b_r,RES_b_9HL0,RES_b_9IX_d0,RES_b_9IY_d0,JP_nn,JP_cc_nn,JR_e,JR_C_e,JR_NC_e,JR_Z_e,JR_NZ_e,JP_HL,JP_IX,JP_IY,DJNZ_e,CALL_nn,CALL_cc_nn,RET,RET_cc,RETI,RETN,RST_p)
END
&
THEORY ListInputX IS
  List_Input(Machine(Z80Man_bitControl),RLCA)==(?);
  List_Input(Machine(Z80Man_bitControl),RLA)==(?);
  List_Input(Machine(Z80Man_bitControl),RRCA)==(?);
  List_Input(Machine(Z80Man_bitControl),RRA)==(?);
  List_Input(Machine(Z80Man_bitControl),RLC_r)==(rr);
  List_Input(Machine(Z80Man_bitControl),RLC_9HL0)==(?);
  List_Input(Machine(Z80Man_bitControl),RLC_9IX_d9)==(desloc);
  List_Input(Machine(Z80Man_bitControl),RLC_9IY_d9)==(desloc);
  List_Input(Machine(Z80Man_bitControl),RL_r)==(rr);
  List_Input(Machine(Z80Man_bitControl),RL_9HL)==(?);
  List_Input(Machine(Z80Man_bitControl),RL_9IX_d9)==(desloc);
  List_Input(Machine(Z80Man_bitControl),RL_9IY_d9)==(desloc);
  List_Input(Machine(Z80Man_bitControl),RRC_r)==(rr);
  List_Input(Machine(Z80Man_bitControl),RRC_9HL0)==(?);
  List_Input(Machine(Z80Man_bitControl),RRC_9IX_d9)==(desloc);
  List_Input(Machine(Z80Man_bitControl),RRC_9IY_d9)==(desloc);
  List_Input(Machine(Z80Man_bitControl),RR_r)==(rr);
  List_Input(Machine(Z80Man_bitControl),RR_9HL)==(?);
  List_Input(Machine(Z80Man_bitControl),RR_9IX_d9)==(desloc);
  List_Input(Machine(Z80Man_bitControl),RR_9IY_d9)==(desloc);
  List_Input(Machine(Z80Man_bitControl),SLA_r)==(rr);
  List_Input(Machine(Z80Man_bitControl),SLA_9HL)==(?);
  List_Input(Machine(Z80Man_bitControl),SLA_9IX_d9)==(desloc);
  List_Input(Machine(Z80Man_bitControl),SLA_9IY_d9)==(desloc);
  List_Input(Machine(Z80Man_bitControl),SRA_r)==(rr);
  List_Input(Machine(Z80Man_bitControl),SRA_9HL0)==(?);
  List_Input(Machine(Z80Man_bitControl),SRA_9IX_d9)==(desloc);
  List_Input(Machine(Z80Man_bitControl),SRA_9IY_d9)==(desloc);
  List_Input(Machine(Z80Man_bitControl),SRL_r)==(rr);
  List_Input(Machine(Z80Man_bitControl),SRL_9HL0)==(?);
  List_Input(Machine(Z80Man_bitControl),SRL_9IX_d9)==(desloc);
  List_Input(Machine(Z80Man_bitControl),SRL_9IY_d9)==(desloc);
  List_Input(Machine(Z80Man_bitControl),RLD)==(?);
  List_Input(Machine(Z80Man_bitControl),RRD)==(?);
  List_Input(Machine(Z80Man_bitControl),BIT_b_rr)==(bb,rr);
  List_Input(Machine(Z80Man_bitControl),BIT_b_9HL0)==(bb);
  List_Input(Machine(Z80Man_bitControl),BIT_b_9IX_d0)==(desloc,bb);
  List_Input(Machine(Z80Man_bitControl),BIT_b_9IY_d0)==(desloc,bb);
  List_Input(Machine(Z80Man_bitControl),SET_b_r)==(bb,rr);
  List_Input(Machine(Z80Man_bitControl),SET_b_9HL0)==(bb);
  List_Input(Machine(Z80Man_bitControl),SET_b_9IX_d0)==(bb,desloc);
  List_Input(Machine(Z80Man_bitControl),SET_b_9IY_d0)==(bb,desloc);
  List_Input(Machine(Z80Man_bitControl),RES_b_r)==(bb,rr);
  List_Input(Machine(Z80Man_bitControl),RES_b_9HL0)==(bb);
  List_Input(Machine(Z80Man_bitControl),RES_b_9IX_d0)==(bb,desloc);
  List_Input(Machine(Z80Man_bitControl),RES_b_9IY_d0)==(bb,desloc);
  List_Input(Machine(Z80Man_bitControl),JP_nn)==(nn);
  List_Input(Machine(Z80Man_bitControl),JP_cc_nn)==(cc,nn);
  List_Input(Machine(Z80Man_bitControl),JR_e)==(ee);
  List_Input(Machine(Z80Man_bitControl),JR_C_e)==(ee);
  List_Input(Machine(Z80Man_bitControl),JR_NC_e)==(ee);
  List_Input(Machine(Z80Man_bitControl),JR_Z_e)==(ee);
  List_Input(Machine(Z80Man_bitControl),JR_NZ_e)==(ee);
  List_Input(Machine(Z80Man_bitControl),JP_HL)==(?);
  List_Input(Machine(Z80Man_bitControl),JP_IX)==(?);
  List_Input(Machine(Z80Man_bitControl),JP_IY)==(?);
  List_Input(Machine(Z80Man_bitControl),DJNZ_e)==(ee);
  List_Input(Machine(Z80Man_bitControl),CALL_nn)==(nn);
  List_Input(Machine(Z80Man_bitControl),CALL_cc_nn)==(cc,nn);
  List_Input(Machine(Z80Man_bitControl),RET)==(?);
  List_Input(Machine(Z80Man_bitControl),RET_cc)==(cc);
  List_Input(Machine(Z80Man_bitControl),RETI)==(?);
  List_Input(Machine(Z80Man_bitControl),RETN)==(?);
  List_Input(Machine(Z80Man_bitControl),RST_p)==(pp)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Z80Man_bitControl),RLCA)==(?);
  List_Output(Machine(Z80Man_bitControl),RLA)==(?);
  List_Output(Machine(Z80Man_bitControl),RRCA)==(?);
  List_Output(Machine(Z80Man_bitControl),RRA)==(?);
  List_Output(Machine(Z80Man_bitControl),RLC_r)==(?);
  List_Output(Machine(Z80Man_bitControl),RLC_9HL0)==(?);
  List_Output(Machine(Z80Man_bitControl),RLC_9IX_d9)==(?);
  List_Output(Machine(Z80Man_bitControl),RLC_9IY_d9)==(?);
  List_Output(Machine(Z80Man_bitControl),RL_r)==(?);
  List_Output(Machine(Z80Man_bitControl),RL_9HL)==(?);
  List_Output(Machine(Z80Man_bitControl),RL_9IX_d9)==(?);
  List_Output(Machine(Z80Man_bitControl),RL_9IY_d9)==(?);
  List_Output(Machine(Z80Man_bitControl),RRC_r)==(?);
  List_Output(Machine(Z80Man_bitControl),RRC_9HL0)==(?);
  List_Output(Machine(Z80Man_bitControl),RRC_9IX_d9)==(?);
  List_Output(Machine(Z80Man_bitControl),RRC_9IY_d9)==(?);
  List_Output(Machine(Z80Man_bitControl),RR_r)==(?);
  List_Output(Machine(Z80Man_bitControl),RR_9HL)==(?);
  List_Output(Machine(Z80Man_bitControl),RR_9IX_d9)==(?);
  List_Output(Machine(Z80Man_bitControl),RR_9IY_d9)==(?);
  List_Output(Machine(Z80Man_bitControl),SLA_r)==(?);
  List_Output(Machine(Z80Man_bitControl),SLA_9HL)==(?);
  List_Output(Machine(Z80Man_bitControl),SLA_9IX_d9)==(?);
  List_Output(Machine(Z80Man_bitControl),SLA_9IY_d9)==(?);
  List_Output(Machine(Z80Man_bitControl),SRA_r)==(?);
  List_Output(Machine(Z80Man_bitControl),SRA_9HL0)==(?);
  List_Output(Machine(Z80Man_bitControl),SRA_9IX_d9)==(?);
  List_Output(Machine(Z80Man_bitControl),SRA_9IY_d9)==(?);
  List_Output(Machine(Z80Man_bitControl),SRL_r)==(?);
  List_Output(Machine(Z80Man_bitControl),SRL_9HL0)==(?);
  List_Output(Machine(Z80Man_bitControl),SRL_9IX_d9)==(?);
  List_Output(Machine(Z80Man_bitControl),SRL_9IY_d9)==(?);
  List_Output(Machine(Z80Man_bitControl),RLD)==(?);
  List_Output(Machine(Z80Man_bitControl),RRD)==(?);
  List_Output(Machine(Z80Man_bitControl),BIT_b_rr)==(?);
  List_Output(Machine(Z80Man_bitControl),BIT_b_9HL0)==(?);
  List_Output(Machine(Z80Man_bitControl),BIT_b_9IX_d0)==(?);
  List_Output(Machine(Z80Man_bitControl),BIT_b_9IY_d0)==(?);
  List_Output(Machine(Z80Man_bitControl),SET_b_r)==(?);
  List_Output(Machine(Z80Man_bitControl),SET_b_9HL0)==(?);
  List_Output(Machine(Z80Man_bitControl),SET_b_9IX_d0)==(?);
  List_Output(Machine(Z80Man_bitControl),SET_b_9IY_d0)==(?);
  List_Output(Machine(Z80Man_bitControl),RES_b_r)==(?);
  List_Output(Machine(Z80Man_bitControl),RES_b_9HL0)==(?);
  List_Output(Machine(Z80Man_bitControl),RES_b_9IX_d0)==(?);
  List_Output(Machine(Z80Man_bitControl),RES_b_9IY_d0)==(?);
  List_Output(Machine(Z80Man_bitControl),JP_nn)==(?);
  List_Output(Machine(Z80Man_bitControl),JP_cc_nn)==(?);
  List_Output(Machine(Z80Man_bitControl),JR_e)==(?);
  List_Output(Machine(Z80Man_bitControl),JR_C_e)==(?);
  List_Output(Machine(Z80Man_bitControl),JR_NC_e)==(?);
  List_Output(Machine(Z80Man_bitControl),JR_Z_e)==(?);
  List_Output(Machine(Z80Man_bitControl),JR_NZ_e)==(?);
  List_Output(Machine(Z80Man_bitControl),JP_HL)==(?);
  List_Output(Machine(Z80Man_bitControl),JP_IX)==(?);
  List_Output(Machine(Z80Man_bitControl),JP_IY)==(?);
  List_Output(Machine(Z80Man_bitControl),DJNZ_e)==(?);
  List_Output(Machine(Z80Man_bitControl),CALL_nn)==(?);
  List_Output(Machine(Z80Man_bitControl),CALL_cc_nn)==(?);
  List_Output(Machine(Z80Man_bitControl),RET)==(?);
  List_Output(Machine(Z80Man_bitControl),RET_cc)==(?);
  List_Output(Machine(Z80Man_bitControl),RETI)==(?);
  List_Output(Machine(Z80Man_bitControl),RETN)==(?);
  List_Output(Machine(Z80Man_bitControl),RST_p)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Z80Man_bitControl),RLCA)==(RLCA);
  List_Header(Machine(Z80Man_bitControl),RLA)==(RLA);
  List_Header(Machine(Z80Man_bitControl),RRCA)==(RRCA);
  List_Header(Machine(Z80Man_bitControl),RRA)==(RRA);
  List_Header(Machine(Z80Man_bitControl),RLC_r)==(RLC_r(rr));
  List_Header(Machine(Z80Man_bitControl),RLC_9HL0)==(RLC_9HL0);
  List_Header(Machine(Z80Man_bitControl),RLC_9IX_d9)==(RLC_9IX_d9(desloc));
  List_Header(Machine(Z80Man_bitControl),RLC_9IY_d9)==(RLC_9IY_d9(desloc));
  List_Header(Machine(Z80Man_bitControl),RL_r)==(RL_r(rr));
  List_Header(Machine(Z80Man_bitControl),RL_9HL)==(RL_9HL);
  List_Header(Machine(Z80Man_bitControl),RL_9IX_d9)==(RL_9IX_d9(desloc));
  List_Header(Machine(Z80Man_bitControl),RL_9IY_d9)==(RL_9IY_d9(desloc));
  List_Header(Machine(Z80Man_bitControl),RRC_r)==(RRC_r(rr));
  List_Header(Machine(Z80Man_bitControl),RRC_9HL0)==(RRC_9HL0);
  List_Header(Machine(Z80Man_bitControl),RRC_9IX_d9)==(RRC_9IX_d9(desloc));
  List_Header(Machine(Z80Man_bitControl),RRC_9IY_d9)==(RRC_9IY_d9(desloc));
  List_Header(Machine(Z80Man_bitControl),RR_r)==(RR_r(rr));
  List_Header(Machine(Z80Man_bitControl),RR_9HL)==(RR_9HL);
  List_Header(Machine(Z80Man_bitControl),RR_9IX_d9)==(RR_9IX_d9(desloc));
  List_Header(Machine(Z80Man_bitControl),RR_9IY_d9)==(RR_9IY_d9(desloc));
  List_Header(Machine(Z80Man_bitControl),SLA_r)==(SLA_r(rr));
  List_Header(Machine(Z80Man_bitControl),SLA_9HL)==(SLA_9HL);
  List_Header(Machine(Z80Man_bitControl),SLA_9IX_d9)==(SLA_9IX_d9(desloc));
  List_Header(Machine(Z80Man_bitControl),SLA_9IY_d9)==(SLA_9IY_d9(desloc));
  List_Header(Machine(Z80Man_bitControl),SRA_r)==(SRA_r(rr));
  List_Header(Machine(Z80Man_bitControl),SRA_9HL0)==(SRA_9HL0);
  List_Header(Machine(Z80Man_bitControl),SRA_9IX_d9)==(SRA_9IX_d9(desloc));
  List_Header(Machine(Z80Man_bitControl),SRA_9IY_d9)==(SRA_9IY_d9(desloc));
  List_Header(Machine(Z80Man_bitControl),SRL_r)==(SRL_r(rr));
  List_Header(Machine(Z80Man_bitControl),SRL_9HL0)==(SRL_9HL0);
  List_Header(Machine(Z80Man_bitControl),SRL_9IX_d9)==(SRL_9IX_d9(desloc));
  List_Header(Machine(Z80Man_bitControl),SRL_9IY_d9)==(SRL_9IY_d9(desloc));
  List_Header(Machine(Z80Man_bitControl),RLD)==(RLD);
  List_Header(Machine(Z80Man_bitControl),RRD)==(RRD);
  List_Header(Machine(Z80Man_bitControl),BIT_b_rr)==(BIT_b_rr(bb,rr));
  List_Header(Machine(Z80Man_bitControl),BIT_b_9HL0)==(BIT_b_9HL0(bb));
  List_Header(Machine(Z80Man_bitControl),BIT_b_9IX_d0)==(BIT_b_9IX_d0(desloc,bb));
  List_Header(Machine(Z80Man_bitControl),BIT_b_9IY_d0)==(BIT_b_9IY_d0(desloc,bb));
  List_Header(Machine(Z80Man_bitControl),SET_b_r)==(SET_b_r(bb,rr));
  List_Header(Machine(Z80Man_bitControl),SET_b_9HL0)==(SET_b_9HL0(bb));
  List_Header(Machine(Z80Man_bitControl),SET_b_9IX_d0)==(SET_b_9IX_d0(bb,desloc));
  List_Header(Machine(Z80Man_bitControl),SET_b_9IY_d0)==(SET_b_9IY_d0(bb,desloc));
  List_Header(Machine(Z80Man_bitControl),RES_b_r)==(RES_b_r(bb,rr));
  List_Header(Machine(Z80Man_bitControl),RES_b_9HL0)==(RES_b_9HL0(bb));
  List_Header(Machine(Z80Man_bitControl),RES_b_9IX_d0)==(RES_b_9IX_d0(bb,desloc));
  List_Header(Machine(Z80Man_bitControl),RES_b_9IY_d0)==(RES_b_9IY_d0(bb,desloc));
  List_Header(Machine(Z80Man_bitControl),JP_nn)==(JP_nn(nn));
  List_Header(Machine(Z80Man_bitControl),JP_cc_nn)==(JP_cc_nn(cc,nn));
  List_Header(Machine(Z80Man_bitControl),JR_e)==(JR_e(ee));
  List_Header(Machine(Z80Man_bitControl),JR_C_e)==(JR_C_e(ee));
  List_Header(Machine(Z80Man_bitControl),JR_NC_e)==(JR_NC_e(ee));
  List_Header(Machine(Z80Man_bitControl),JR_Z_e)==(JR_Z_e(ee));
  List_Header(Machine(Z80Man_bitControl),JR_NZ_e)==(JR_NZ_e(ee));
  List_Header(Machine(Z80Man_bitControl),JP_HL)==(JP_HL);
  List_Header(Machine(Z80Man_bitControl),JP_IX)==(JP_IX);
  List_Header(Machine(Z80Man_bitControl),JP_IY)==(JP_IY);
  List_Header(Machine(Z80Man_bitControl),DJNZ_e)==(DJNZ_e(ee));
  List_Header(Machine(Z80Man_bitControl),CALL_nn)==(CALL_nn(nn));
  List_Header(Machine(Z80Man_bitControl),CALL_cc_nn)==(CALL_cc_nn(cc,nn));
  List_Header(Machine(Z80Man_bitControl),RET)==(RET);
  List_Header(Machine(Z80Man_bitControl),RET_cc)==(RET_cc(cc));
  List_Header(Machine(Z80Man_bitControl),RETI)==(RETI);
  List_Header(Machine(Z80Man_bitControl),RETN)==(RETN);
  List_Header(Machine(Z80Man_bitControl),RST_p)==(RST_p(pp))
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Z80Man_bitControl),RLCA)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),RLA)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),RRCA)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),RRA)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),RLC_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80Man_bitControl),RLC_9HL0)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),RLC_9IX_d9)==(desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),RLC_9IY_d9)==(desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),RL_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80Man_bitControl),RL_9HL)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),RL_9IX_d9)==(desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),RL_9IY_d9)==(desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),RRC_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80Man_bitControl),RRC_9HL0)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),RRC_9IX_d9)==(desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),RRC_9IY_d9)==(desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),RR_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80Man_bitControl),RR_9HL)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),RR_9IX_d9)==(desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),RR_9IY_d9)==(desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),SLA_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80Man_bitControl),SLA_9HL)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),SLA_9IX_d9)==(desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),SLA_9IY_d9)==(desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),SRA_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80Man_bitControl),SRA_9HL0)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),SRA_9IX_d9)==(desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),SRA_9IY_d9)==(desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),SRL_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80Man_bitControl),SRL_9HL0)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),SRL_9IX_d9)==(desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),SRL_9IY_d9)==(desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),RLD)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),RRD)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),BIT_b_rr)==(bb: 0..7 & rr: id_reg_8);
  List_Precondition(Machine(Z80Man_bitControl),BIT_b_9HL0)==(bb: 0..7);
  List_Precondition(Machine(Z80Man_bitControl),BIT_b_9IX_d0)==(bb: 0..7 & desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),BIT_b_9IY_d0)==(bb: 0..7 & desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),SET_b_r)==(bb: 0..7 & rr: id_reg_8);
  List_Precondition(Machine(Z80Man_bitControl),SET_b_9HL0)==(bb: 0..7);
  List_Precondition(Machine(Z80Man_bitControl),SET_b_9IX_d0)==(bb: 0..7 & desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),SET_b_9IY_d0)==(bb: 0..7 & desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),RES_b_r)==(bb: 0..7 & rr: id_reg_8);
  List_Precondition(Machine(Z80Man_bitControl),RES_b_9HL0)==(bb: 0..7);
  List_Precondition(Machine(Z80Man_bitControl),RES_b_9IX_d0)==(bb: 0..7 & desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),RES_b_9IY_d0)==(bb: 0..7 & desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),JP_nn)==(nn: USHORT);
  List_Precondition(Machine(Z80Man_bitControl),JP_cc_nn)==(cc: BOOL & nn: USHORT);
  List_Precondition(Machine(Z80Man_bitControl),JR_e)==(ee: SCHAR & ee-2: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),JR_C_e)==(ee: SCHAR & ee-2: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),JR_NC_e)==(ee: SCHAR & ee-2: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),JR_Z_e)==(ee: SCHAR & ee-2: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),JR_NZ_e)==(ee: SCHAR & ee-2: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),JP_HL)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),JP_IX)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),JP_IY)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),DJNZ_e)==(ee: SCHAR & ee-2: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),CALL_nn)==(nn: USHORT);
  List_Precondition(Machine(Z80Man_bitControl),CALL_cc_nn)==(cc: 0..8 & nn: USHORT);
  List_Precondition(Machine(Z80Man_bitControl),RET)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),RET_cc)==(cc: 0..7);
  List_Precondition(Machine(Z80Man_bitControl),RETI)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),RETN)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),RST_p)==(pp: 0..7)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RST_p)==(pp: 0..7 | @(pc_l,pc_h).(pc_l: BYTE & pc_h: BYTE & bv16_byte(ushort_bv16(pc)) = pc_l,pc_h ==> ({stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -1)))|->pc_h,stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)))|->pc_l}: BV16 --> BYTE | stack,mem:=stack<+{stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -1)))|->pc_h,stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)))|->pc_l},mem<+{stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -1)))|->pc_h,stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)))|->pc_l} || sp:=ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)) || pc:=pp*8 || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RETN)==(btrue | pc,sp,r_,iff1:=bv16_ushort(byte_bv16(mem(ushort_bv16(add16USHORT(0,bv16_ushort(sp),1))),mem(sp))),ushort_bv16(add16USHORT(0,bv16_ushort(sp),2)),update_refresh_reg(r_),iff2);
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RETI)==(btrue | pc,sp,r_:=bv16_ushort(byte_bv16(mem(ushort_bv16(add16USHORT(0,bv16_ushort(sp),1))),mem(sp))),ushort_bv16(add16USHORT(0,bv16_ushort(sp),2)),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RET_cc)==(cc: 0..7 | cc_get(rgs8,cc) = 1 ==> pc,sp:=bv16_ushort(byte_bv16(mem(ushort_bv16(add16USHORT(0,bv16_ushort(sp),1))),mem(sp))),ushort_bv16(add16USHORT(0,bv16_ushort(sp),2)) [] not(cc_get(rgs8,cc) = 1) ==> pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RET)==(btrue | pc,sp,r_:=bv16_ushort(byte_bv16(mem(ushort_bv16(add16USHORT(0,bv16_ushort(sp),1))),mem(sp))),ushort_bv16(add16USHORT(0,bv16_ushort(sp),2)),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),CALL_cc_nn)==(cc: 0..8 & nn: USHORT | @(high,low).(high: BYTE & low: BYTE & high,low = bv16_byte(ushort_bv16(pc)) ==> (cc_get(rgs8,cc) = 1 ==> ({stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -1)))|->high,stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)))|->low}: BV16 --> BYTE | stack,mem:=stack<+{stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -1)))|->high,stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)))|->low},mem<+{stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -1)))|->high,stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)))|->low} || sp:=ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)) || pc:=nn) [] not(cc_get(rgs8,cc) = 1) ==> pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),CALL_nn)==(nn: USHORT | @(high,low).(high: BYTE & low: BYTE & high,low = bv16_byte(ushort_bv16(pc)) ==> ({stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -1)))|->high,stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)))|->low}: BV16 --> BYTE | stack,mem:=stack<+{stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -1)))|->high,stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)))|->low},mem<+{stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -1)))|->high,stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)))|->low} || sp:=ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)) || pc:=nn)));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),DJNZ_e)==(ee: SCHAR & ee-2: SCHAR | rgs8:=rgs8<+{b0|->dec_BYTE(rgs8(b0))} || (is_zero(dec_BYTE(rgs8(b0))) = 1 ==> pc:=instruction_next(pc) [] not(is_zero(dec_BYTE(rgs8(b0))) = 1) ==> pc:=(pc+ee) mod 2**16) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JP_IY)==(btrue | pc,r_:=bv16_ushort(iy),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JP_IX)==(btrue | pc,r_:=bv16_ushort(ix),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JP_HL)==(btrue | pc,r_:=bv16_ushort(byte_bv16(rgs8(h0),rgs8(l0))),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JR_NZ_e)==(ee: SCHAR & ee-2: SCHAR | bitget(rgs8(f0),6) = 1 ==> pc:=instruction_next(pc) [] not(bitget(rgs8(f0),6) = 1) ==> pc:=(pc+ee) mod 2**16 || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JR_Z_e)==(ee: SCHAR & ee-2: SCHAR | bitget(rgs8(f0),6) = 0 ==> pc:=instruction_next(pc) [] not(bitget(rgs8(f0),6) = 0) ==> pc:=(pc+ee) mod 2**16 || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JR_NC_e)==(ee: SCHAR & ee-2: SCHAR | bitget(rgs8(f0),0) = 1 ==> pc:=instruction_next(pc) [] not(bitget(rgs8(f0),0) = 1) ==> pc:=(pc+ee) mod 2**16 || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JR_C_e)==(ee: SCHAR & ee-2: SCHAR | bitget(rgs8(f0),0) = 0 ==> pc:=instruction_next(pc) [] not(bitget(rgs8(f0),0) = 0) ==> pc:=(pc+ee) mod 2**16 || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JR_e)==(ee: SCHAR & ee-2: SCHAR | pc,r_:=(pc+ee) mod 2**16,update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JP_cc_nn)==(cc: BOOL & nn: USHORT | cc = TRUE ==> pc:=nn [] not(cc = TRUE) ==> pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JP_nn)==(nn: USHORT | pc,r_:=nn,update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RES_b_9IY_d0)==(bb: 0..7 & desloc: SCHAR & byte_bv16(rgs8(h0),rgs8(l0)): BV16 & bv_clear(bv_9ireg_plus_d0(mem,iy,desloc),bb): BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->bv_clear(bv_9ireg_plus_d0(mem,iy,desloc),bb)})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->bv_clear(bv_9ireg_plus_d0(mem,iy,desloc),bb): stack) | mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->bv_clear(bv_9ireg_plus_d0(mem,iy,desloc),bb)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RES_b_9IX_d0)==(bb: 0..7 & desloc: SCHAR & byte_bv16(rgs8(h0),rgs8(l0)): BV16 & bv_clear(bv_9ireg_plus_d0(mem,ix,desloc),bb): BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->bv_clear(bv_9ireg_plus_d0(mem,ix,desloc),bb)})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->bv_clear(bv_9ireg_plus_d0(mem,ix,desloc),bb): stack) | mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->bv_clear(bv_9ireg_plus_d0(mem,ix,desloc),bb)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RES_b_9HL0)==(bb: 0..7 & byte_bv16(rgs8(h0),rgs8(l0)): BV16 & bv_clear(mem(byte_bv16(rgs8(h0),rgs8(l0))),bb): BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->bv_clear(mem(byte_bv16(rgs8(h0),rgs8(l0))),bb)})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->bv_clear(mem(byte_bv16(rgs8(h0),rgs8(l0))),bb): stack) | mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->bv_clear(mem(byte_bv16(rgs8(h0),rgs8(l0))),bb)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RES_b_r)==(bb: 0..7 & rr: id_reg_8 | rgs8,pc,r_:=rgs8<+{rr|->bv_clear(rgs8(rr),bb)},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SET_b_9IY_d0)==(bb: 0..7 & desloc: SCHAR & byte_bv16(rgs8(h0),rgs8(l0)): BV16 & bv_set(bv_9ireg_plus_d0(mem,iy,desloc),bb): BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->bv_set(bv_9ireg_plus_d0(mem,iy,desloc),bb)})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->bv_set(bv_9ireg_plus_d0(mem,iy,desloc),bb): stack) | mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->bv_set(bv_9ireg_plus_d0(mem,iy,desloc),bb)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SET_b_9IX_d0)==(bb: 0..7 & desloc: SCHAR & byte_bv16(rgs8(h0),rgs8(l0)): BV16 & bv_set(bv_9ireg_plus_d0(mem,ix,desloc),bb): BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->bv_set(bv_9ireg_plus_d0(mem,ix,desloc),bb)})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->bv_set(bv_9ireg_plus_d0(mem,ix,desloc),bb): stack) | mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->bv_set(bv_9ireg_plus_d0(mem,ix,desloc),bb)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SET_b_9HL0)==(bb: 0..7 & byte_bv16(rgs8(h0),rgs8(l0)): BV16 & bv_set(mem(byte_bv16(rgs8(h0),rgs8(l0))),bb): BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->bv_set(mem(byte_bv16(rgs8(h0),rgs8(l0))),bb)})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->bv_set(mem(byte_bv16(rgs8(h0),rgs8(l0))),bb): stack) | mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->bv_set(mem(byte_bv16(rgs8(h0),rgs8(l0))),bb)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SET_b_r)==(bb: 0..7 & rr: id_reg_8 | rgs8,pc,r_:=rgs8<+{rr|->bitset(rgs8(rr),bb)},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),BIT_b_9IY_d0)==(bb: 0..7 & desloc: SCHAR | rgs8,pc,r_:=update_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bit_not(bv_9ireg_plus_d0(mem,iy,desloc)(bb)),1,bitget(rgs8(f0),2),0,bitget(rgs8(f0),0)),instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),BIT_b_9IX_d0)==(bb: 0..7 & desloc: SCHAR | rgs8,pc,r_:=update_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bit_not(bv_9ireg_plus_d0(mem,ix,desloc)(bb)),1,bitget(rgs8(f0),2),0,bitget(rgs8(f0),0)),instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),BIT_b_9HL0)==(bb: 0..7 | rgs8,pc,r_:=update_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bit_not(mem(byte_bv16(rgs8(h0),rgs8(l0)))(bb)),1,bitget(rgs8(f0),2),0,bitget(rgs8(f0),0)),instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),BIT_b_rr)==(bb: 0..7 & rr: id_reg_8 | rgs8,pc,r_:=update_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bit_not(bitget(rgs8(rr),bb)),1,bitget(rgs8(f0),2),0,bitget(rgs8(f0),0)),instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RRD)==(btrue | @(res,acc).(res: BYTE & acc: BYTE & res = {7|->bitget(rgs8(a0),3),6|->bitget(rgs8(a0),2),5|->bitget(rgs8(a0),1),4|->bitget(rgs8(a0),0),3|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),7),2|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),6),1|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),5),0|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),4)} & acc = rgs8(a0)<+{3|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),3),2|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),2),1|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),1),0|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),0)} ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(acc),is_zero(acc),0,parity_even_BYTE(acc),0,bitget(rgs8(f0),0))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RLD)==(btrue | @(res,acc).(res: BYTE & acc: BYTE & res = {7|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),3),6|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),2),5|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),1),4|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),0),3|->bitget(rgs8(a0),3),2|->bitget(rgs8(a0),2),1|->bitget(rgs8(a0),1),0|->bitget(rgs8(a0),0)} & acc = rgs8(a0)<+{3|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),7),2|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),6),1|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),5),0|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),4)} ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(acc),is_zero(acc),0,parity_even_BYTE(acc),0,bitget(rgs8(f0),0))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SRL_9IY_d9)==(desloc: SCHAR | @res.(res: BYTE & res = bitclear(rotateright(bv_9ireg_plus_d0(mem,iy,desloc)),7) ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,0,is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),0))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SRL_9IX_d9)==(desloc: SCHAR | @res.(res: BYTE & res = bitclear(rotateright(bv_9ireg_plus_d0(mem,ix,desloc)),7) ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,0,is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),0))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SRL_9HL0)==(btrue | @res.(res: BYTE & res = bitclear(rotateright(mem(byte_bv16(rgs8(h0),rgs8(l0)))),7) ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,0,is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),0))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SRL_r)==(rr: id_reg_8 | @res.(res: BYTE & res = bitclear(rotateright(rgs8(rr)),7) ==> rgs8,pc,r_:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,0,is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),0))},instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SRA_9IY_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateright(bv_9ireg_plus_d0(mem,iy,desloc))<+{7|->bv_9ireg_plus_d0(mem,iy,desloc)(7)} ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),0))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SRA_9IX_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateright(bv_9ireg_plus_d0(mem,ix,desloc))<+{7|->bitget(bv_9ireg_plus_d0(mem,ix,desloc),7)} ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),0))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SRA_9HL0)==(btrue | @res.(res: BYTE & res = rotateright(mem(byte_bv16(rgs8(h0),rgs8(l0))))<+{7|->mem(byte_bv16(rgs8(h0),rgs8(l0)))(7)} ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),0))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SRA_r)==(rr: id_reg_8 | @res.(res: BYTE & res = rotateright(rgs8(rr))<+{7|->bitget(rgs8(rr),7)} ==> rgs8,pc,r_:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),0))},instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SLA_9IY_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateright(bv_9ireg_plus_d0(mem,iy,desloc))<+{0|->bitget(rgs8(f0),0)} ==> (bv_ireg_plus_d(iy,desloc): BV16 & res: BYTE & dom({bv_ireg_plus_d(iy,desloc)|->res})/\dom(stack) = {} & not(bv_ireg_plus_d(iy,desloc)|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),7))} || mem:=mem<+{bv_ireg_plus_d(iy,desloc)|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SLA_9IX_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateright(bv_9ireg_plus_d0(mem,ix,desloc))<+{0|->bitget(rgs8(f0),0)} ==> (bv_ireg_plus_d(ix,desloc): BV16 & res: BYTE & dom({bv_ireg_plus_d(ix,desloc)|->res})/\dom(stack) = {} & not(bv_ireg_plus_d(ix,desloc)|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),7))} || mem:=mem<+{bv_ireg_plus_d(ix,desloc)|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SLA_9HL)==(btrue | @res.(res: BYTE & res = rotateright(mem(byte_bv16(rgs8(h0),rgs8(l0))))<+{0|->bitget(rgs8(f0),0)} ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),7))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SLA_r)==(rr: id_reg_8 | @res.(res: BYTE & res = rotateright(rgs8(rr))<+{0|->bitget(rgs8(f0),0)} ==> rgs8,pc,r_:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),7))},instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RR_9IY_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateright(bv_9ireg_plus_d0(mem,iy,desloc))<+{0|->bitget(rgs8(f0),0)} ==> (bv_ireg_plus_d(iy,desloc): BV16 & res: BYTE & dom({bv_ireg_plus_d(iy,desloc)|->res})/\dom(stack) = {} & not(bv_ireg_plus_d(iy,desloc)|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),0))} || mem:=mem<+{bv_ireg_plus_d(iy,desloc)|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RR_9IX_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateright(bv_9ireg_plus_d0(mem,ix,desloc))<+{0|->bitget(rgs8(f0),0)} ==> (bv_ireg_plus_d(ix,desloc): BV16 & res: BYTE & dom({bv_ireg_plus_d(ix,desloc)|->res})/\dom(stack) = {} & not(bv_ireg_plus_d(ix,desloc)|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),0))} || mem:=mem<+{bv_ireg_plus_d(ix,desloc)|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RR_9HL)==(btrue | @res.(res: BYTE & res = rotateright(mem(byte_bv16(rgs8(h0),rgs8(l0))))<+{0|->bitget(rgs8(f0),0)} ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),0))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RR_r)==(rr: id_reg_8 | @res.(res: BYTE & res = rotateright(rgs8(rr))<+{0|->bitget(rgs8(f0),0)} ==> rgs8,pc,r_:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),0))},instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RRC_9IY_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateright(bv_9ireg_plus_d0(mem,iy,desloc)) ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),0))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RRC_9IX_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateright(bv_9ireg_plus_d0(mem,ix,desloc)) ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),0))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RRC_9HL0)==(btrue | @res.(res: BYTE & res = rotateright(mem(byte_bv16(rgs8(h0),rgs8(l0)))) ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),0))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RRC_r)==(rr: id_reg_8 | @res.(res: BYTE & res = rotateright(rgs8(rr)) ==> rgs8,pc,r_:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),0))},instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RL_9IY_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateleft(bv_9ireg_plus_d0(mem,iy,desloc))<+{0|->bitget(rgs8(f0),0)} ==> (bv_ireg_plus_d(iy,desloc): BV16 & res: BYTE & dom({bv_ireg_plus_d(iy,desloc)|->res})/\dom(stack) = {} & not(bv_ireg_plus_d(iy,desloc)|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),7))} || mem:=mem<+{bv_ireg_plus_d(iy,desloc)|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RL_9IX_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateleft(bv_9ireg_plus_d0(mem,ix,desloc))<+{0|->bitget(rgs8(f0),0)} ==> (bv_ireg_plus_d(ix,desloc): BV16 & res: BYTE & dom({bv_ireg_plus_d(ix,desloc)|->res})/\dom(stack) = {} & not(bv_ireg_plus_d(ix,desloc)|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),7))} || mem:=mem<+{bv_ireg_plus_d(ix,desloc)|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RL_9HL)==(btrue | @res.(res: BYTE & res = rotateleft(mem(byte_bv16(rgs8(h0),rgs8(l0))))<+{0|->bitget(rgs8(f0),0)} ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),7))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RL_r)==(rr: id_reg_8 | @res.(res: BYTE & res = rotateleft(rgs8(rr))<+{0|->bitget(rgs8(f0),0)} ==> rgs8,pc,r_:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),7))},instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RLC_9IY_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateleft(bv_9ireg_plus_d0(mem,iy,desloc)) ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),7))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RLC_9IX_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateleft(bv_9ireg_plus_d0(mem,ix,desloc)) ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),7))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RLC_9HL0)==(btrue | @res.(res: BYTE & res = rotateleft(mem(byte_bv16(rgs8(h0),rgs8(l0)))) ==> (byte_bv16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & dom({byte_bv16(rgs8(h0),rgs8(l0))|->res})/\dom(stack) = {} & not(byte_bv16(rgs8(h0),rgs8(l0))|->res: stack) | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),7))} || mem:=mem<+{byte_bv16(rgs8(h0),rgs8(l0))|->res} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RLC_r)==(rr: id_reg_8 | @res.(res: BYTE & res = rotateleft(rgs8(rr)) ==> rgs8,pc,r_:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),7))},instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RRA)==(btrue | rgs8,pc,r_:=rgs8<+{a0|->(rotateright(rgs8(a0))<+{0|->bitget(rgs8(f0),0)}),get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bitget(rgs8(f0),2),0,bitget(rgs8(a0),0))},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RRCA)==(btrue | rgs8,pc,r_:=rgs8<+{a0|->rotateright(rgs8(a0)),get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bitget(rgs8(f0),2),0,bitget(rgs8(a0),0))},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RLA)==(btrue | rgs8,pc,r_:=rgs8<+{a0|->(rotateleft(rgs8(a0))<+{0|->bitget(rgs8(f0),0)}),get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bitget(rgs8(f0),2),0,bitget(rgs8(a0),7))},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RLCA)==(btrue | rgs8,pc,r_:=rgs8<+{a0|->rotateleft(rgs8(a0)),get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bitget(rgs8(f0),2),0,bitget(rgs8(a0),7))},instruction_next(pc),update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),RLCA)==(rgs8:=rgs8<+{a0|->rotateleft(rgs8(a0)),get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bitget(rgs8(f0),2),0,bitget(rgs8(a0),7))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),RLA)==(rgs8:=rgs8<+{a0|->(rotateleft(rgs8(a0))<+{0|->bitget(rgs8(f0),0)}),get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bitget(rgs8(f0),2),0,bitget(rgs8(a0),7))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),RRCA)==(rgs8:=rgs8<+{a0|->rotateright(rgs8(a0)),get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bitget(rgs8(f0),2),0,bitget(rgs8(a0),0))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),RRA)==(rgs8:=rgs8<+{a0|->(rotateright(rgs8(a0))<+{0|->bitget(rgs8(f0),0)}),get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bitget(rgs8(f0),2),0,bitget(rgs8(a0),0))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),RLC_r)==(ANY res WHERE res: BYTE & res = rotateleft(rgs8(rr)) THEN rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),7))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RLC_9HL0)==(ANY res WHERE res: BYTE & res = rotateleft(mem(byte_bv16(rgs8(h0),rgs8(l0)))) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),7))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RLC_9IX_d9)==(ANY res WHERE res: BYTE & res = rotateleft(bv_9ireg_plus_d0(mem,ix,desloc)) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),7))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RLC_9IY_d9)==(ANY res WHERE res: BYTE & res = rotateleft(bv_9ireg_plus_d0(mem,iy,desloc)) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),7))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RL_r)==(ANY res WHERE res: BYTE & res = rotateleft(rgs8(rr))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),7))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RL_9HL)==(ANY res WHERE res: BYTE & res = rotateleft(mem(byte_bv16(rgs8(h0),rgs8(l0))))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),7))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RL_9IX_d9)==(ANY res WHERE res: BYTE & res = rotateleft(bv_9ireg_plus_d0(mem,ix,desloc))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),7))} || updateAddressMem(bv_ireg_plus_d(ix,desloc),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RL_9IY_d9)==(ANY res WHERE res: BYTE & res = rotateleft(bv_9ireg_plus_d0(mem,iy,desloc))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),7))} || updateAddressMem(bv_ireg_plus_d(iy,desloc),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RRC_r)==(ANY res WHERE res: BYTE & res = rotateright(rgs8(rr)) THEN rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),0))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RRC_9HL0)==(ANY res WHERE res: BYTE & res = rotateright(mem(byte_bv16(rgs8(h0),rgs8(l0)))) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),0))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RRC_9IX_d9)==(ANY res WHERE res: BYTE & res = rotateright(bv_9ireg_plus_d0(mem,ix,desloc)) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),0))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RRC_9IY_d9)==(ANY res WHERE res: BYTE & res = rotateright(bv_9ireg_plus_d0(mem,iy,desloc)) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),0))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RR_r)==(ANY res WHERE res: BYTE & res = rotateright(rgs8(rr))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),0))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RR_9HL)==(ANY res WHERE res: BYTE & res = rotateright(mem(byte_bv16(rgs8(h0),rgs8(l0))))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),0))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RR_9IX_d9)==(ANY res WHERE res: BYTE & res = rotateright(bv_9ireg_plus_d0(mem,ix,desloc))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),0))} || updateAddressMem(bv_ireg_plus_d(ix,desloc),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RR_9IY_d9)==(ANY res WHERE res: BYTE & res = rotateright(bv_9ireg_plus_d0(mem,iy,desloc))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),0))} || updateAddressMem(bv_ireg_plus_d(iy,desloc),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),SLA_r)==(ANY res WHERE res: BYTE & res = rotateright(rgs8(rr))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),7))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),SLA_9HL)==(ANY res WHERE res: BYTE & res = rotateright(mem(byte_bv16(rgs8(h0),rgs8(l0))))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),7))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),SLA_9IX_d9)==(ANY res WHERE res: BYTE & res = rotateright(bv_9ireg_plus_d0(mem,ix,desloc))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),7))} || updateAddressMem(bv_ireg_plus_d(ix,desloc),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),SLA_9IY_d9)==(ANY res WHERE res: BYTE & res = rotateright(bv_9ireg_plus_d0(mem,iy,desloc))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),7))} || updateAddressMem(bv_ireg_plus_d(iy,desloc),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),SRA_r)==(ANY res WHERE res: BYTE & res = rotateright(rgs8(rr))<+{7|->bitget(rgs8(rr),7)} THEN rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),0))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),SRA_9HL0)==(ANY res WHERE res: BYTE & res = rotateright(mem(byte_bv16(rgs8(h0),rgs8(l0))))<+{7|->mem(byte_bv16(rgs8(h0),rgs8(l0)))(7)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),0))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),SRA_9IX_d9)==(ANY res WHERE res: BYTE & res = rotateright(bv_9ireg_plus_d0(mem,ix,desloc))<+{7|->bitget(bv_9ireg_plus_d0(mem,ix,desloc),7)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),0))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),SRA_9IY_d9)==(ANY res WHERE res: BYTE & res = rotateright(bv_9ireg_plus_d0(mem,iy,desloc))<+{7|->bv_9ireg_plus_d0(mem,iy,desloc)(7)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),0))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),SRL_r)==(ANY res WHERE res: BYTE & res = bitclear(rotateright(rgs8(rr)),7) THEN rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,0,is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),0))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),SRL_9HL0)==(ANY res WHERE res: BYTE & res = bitclear(rotateright(mem(byte_bv16(rgs8(h0),rgs8(l0)))),7) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,0,is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),0))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),SRL_9IX_d9)==(ANY res WHERE res: BYTE & res = bitclear(rotateright(bv_9ireg_plus_d0(mem,ix,desloc)),7) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,0,is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,ix,desloc),0))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),SRL_9IY_d9)==(ANY res WHERE res: BYTE & res = bitclear(rotateright(bv_9ireg_plus_d0(mem,iy,desloc)),7) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,0,is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9ireg_plus_d0(mem,iy,desloc),0))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RLD)==(ANY res,acc WHERE res: BYTE & acc: BYTE & res = {7|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),3),6|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),2),5|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),1),4|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),0),3|->bitget(rgs8(a0),3),2|->bitget(rgs8(a0),2),1|->bitget(rgs8(a0),1),0|->bitget(rgs8(a0),0)} & acc = rgs8(a0)<+{3|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),7),2|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),6),1|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),5),0|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),4)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(acc),is_zero(acc),0,parity_even_BYTE(acc),0,bitget(rgs8(f0),0))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RRD)==(ANY res,acc WHERE res: BYTE & acc: BYTE & res = {7|->bitget(rgs8(a0),3),6|->bitget(rgs8(a0),2),5|->bitget(rgs8(a0),1),4|->bitget(rgs8(a0),0),3|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),7),2|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),6),1|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),5),0|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),4)} & acc = rgs8(a0)<+{3|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),3),2|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),2),1|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),1),0|->bitget(mem(byte_bv16(rgs8(h0),rgs8(l0))),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(acc),is_zero(acc),0,parity_even_BYTE(acc),0,bitget(rgs8(f0),0))} || updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),res) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),BIT_b_rr)==(rgs8:=update_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bit_not(bitget(rgs8(rr),bb)),1,bitget(rgs8(f0),2),0,bitget(rgs8(f0),0)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),BIT_b_9HL0)==(rgs8:=update_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bit_not(mem(byte_bv16(rgs8(h0),rgs8(l0)))(bb)),1,bitget(rgs8(f0),2),0,bitget(rgs8(f0),0)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),BIT_b_9IX_d0)==(rgs8:=update_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bit_not(bv_9ireg_plus_d0(mem,ix,desloc)(bb)),1,bitget(rgs8(f0),2),0,bitget(rgs8(f0),0)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),BIT_b_9IY_d0)==(rgs8:=update_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bit_not(bv_9ireg_plus_d0(mem,iy,desloc)(bb)),1,bitget(rgs8(f0),2),0,bitget(rgs8(f0),0)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),SET_b_r)==(rgs8(rr):=bitset(rgs8(rr),bb) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),SET_b_9HL0)==(updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),bv_set(mem(byte_bv16(rgs8(h0),rgs8(l0))),bb)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),SET_b_9IX_d0)==(updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),bv_set(bv_9ireg_plus_d0(mem,ix,desloc),bb)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),SET_b_9IY_d0)==(updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),bv_set(bv_9ireg_plus_d0(mem,iy,desloc),bb)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),RES_b_r)==(rgs8(rr):=bv_clear(rgs8(rr),bb) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),RES_b_9HL0)==(updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),bv_clear(mem(byte_bv16(rgs8(h0),rgs8(l0))),bb)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),RES_b_9IX_d0)==(updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),bv_clear(bv_9ireg_plus_d0(mem,ix,desloc),bb)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),RES_b_9IY_d0)==(updateAddressMem(byte_bv16(rgs8(h0),rgs8(l0)),bv_clear(bv_9ireg_plus_d0(mem,iy,desloc),bb)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),JP_nn)==(pc:=nn || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),JP_cc_nn)==(IF cc = TRUE THEN pc:=nn ELSE pc:=instruction_next(pc) END || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),JR_e)==(pc:=(pc+ee) mod 2**16 || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),JR_C_e)==(IF bitget(rgs8(f0),0) = 0 THEN pc:=instruction_next(pc) ELSE pc:=(pc+ee) mod 2**16 END || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),JR_NC_e)==(IF bitget(rgs8(f0),0) = 1 THEN pc:=instruction_next(pc) ELSE pc:=(pc+ee) mod 2**16 END || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),JR_Z_e)==(IF bitget(rgs8(f0),6) = 0 THEN pc:=instruction_next(pc) ELSE pc:=(pc+ee) mod 2**16 END || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),JR_NZ_e)==(IF bitget(rgs8(f0),6) = 1 THEN pc:=instruction_next(pc) ELSE pc:=(pc+ee) mod 2**16 END || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),JP_HL)==(pc:=bv16_ushort(byte_bv16(rgs8(h0),rgs8(l0))) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),JP_IX)==(pc:=bv16_ushort(ix) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),JP_IY)==(pc:=bv16_ushort(iy) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),DJNZ_e)==(rgs8(b0):=dec_BYTE(rgs8(b0)) || IF is_zero(dec_BYTE(rgs8(b0))) = 1 THEN pc:=instruction_next(pc) ELSE pc:=(pc+ee) mod 2**16 END || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),CALL_nn)==(ANY high,low WHERE high: BYTE & low: BYTE & high,low = bv16_byte(ushort_bv16(pc)) THEN updateStack({stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -1)))|->high,stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)))|->low}) || sp:=ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)) || pc:=nn END);
  List_Substitution(Machine(Z80Man_bitControl),CALL_cc_nn)==(ANY high,low WHERE high: BYTE & low: BYTE & high,low = bv16_byte(ushort_bv16(pc)) THEN IF cc_get(rgs8,cc) = 1 THEN updateStack({stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -1)))|->high,stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)))|->low}) || sp:=ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)) || pc:=nn ELSE pc:=instruction_next(pc) END || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80Man_bitControl),RET)==(pc:=bv16_ushort(byte_bv16(mem(ushort_bv16(add16USHORT(0,bv16_ushort(sp),1))),mem(sp))) || sp:=ushort_bv16(add16USHORT(0,bv16_ushort(sp),2)) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),RET_cc)==(IF cc_get(rgs8,cc) = 1 THEN pc:=bv16_ushort(byte_bv16(mem(ushort_bv16(add16USHORT(0,bv16_ushort(sp),1))),mem(sp))) || sp:=ushort_bv16(add16USHORT(0,bv16_ushort(sp),2)) ELSE pc:=instruction_next(pc) END || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),RETI)==(pc:=bv16_ushort(byte_bv16(mem(ushort_bv16(add16USHORT(0,bv16_ushort(sp),1))),mem(sp))) || sp:=ushort_bv16(add16USHORT(0,bv16_ushort(sp),2)) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80Man_bitControl),RETN)==(pc:=bv16_ushort(byte_bv16(mem(ushort_bv16(add16USHORT(0,bv16_ushort(sp),1))),mem(sp))) || sp:=ushort_bv16(add16USHORT(0,bv16_ushort(sp),2)) || r_:=update_refresh_reg(r_) || iff1:=iff2);
  List_Substitution(Machine(Z80Man_bitControl),RST_p)==(ANY pc_l,pc_h WHERE pc_l: BYTE & pc_h: BYTE & bv16_byte(ushort_bv16(pc)) = pc_l,pc_h THEN updateStack({stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -1)))|->pc_h,stack(ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)))|->pc_l}) || sp:=ushort_bv16(add16USHORT(0,bv16_ushort(sp), -2)) || pc:=pp*8 || r_:=update_refresh_reg(r_) END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Z80Man_bitControl))==(RESERVED_ADDRESS,get_bv_reg16,REG16_TO_REG8,REG8_TO_REG16,update_flag_register_SZ_H_PvNC,get_new_flag_register_SZ_H_PvNC,bv_ireg_plus_d,bv_9ireg_plus_d0,cc_get);
  Inherited_List_Constants(Machine(Z80Man_bitControl))==(RESERVED_ADDRESS);
  List_Constants(Machine(Z80Man_bitControl))==(get_bv_reg16,REG16_TO_REG8,REG8_TO_REG16,update_flag_register_SZ_H_PvNC,get_new_flag_register_SZ_H_PvNC,bv_ireg_plus_d,bv_9ireg_plus_d0,cc_get)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(Z80Man_bitControl),id_reg_8)==({a0,f0,f_0,a_0,b0,c0,b_0,c_0,d0,e0,d_0,e_0,h0,l0,h_0,l_0});
  Context_List_Enumerated(Machine(Z80Man_bitControl))==(?);
  Context_List_Defered(Machine(Z80Man_bitControl))==(?);
  Context_List_Sets(Machine(Z80Man_bitControl))==(?);
  List_Valuable_Sets(Machine(Z80Man_bitControl))==(?);
  Inherited_List_Enumerated(Machine(Z80Man_bitControl))==(?);
  Inherited_List_Defered(Machine(Z80Man_bitControl))==(?);
  Inherited_List_Sets(Machine(Z80Man_bitControl))==(?);
  List_Enumerated(Machine(Z80Man_bitControl))==(id_reg_8,id_reg_16);
  List_Defered(Machine(Z80Man_bitControl))==(?);
  List_Sets(Machine(Z80Man_bitControl))==(id_reg_8,id_reg_16);
  Set_Definition(Machine(Z80Man_bitControl),id_reg_16)==({BC,DE,HL,SP,AF})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Z80Man_bitControl))==(?);
  Expanded_List_HiddenConstants(Machine(Z80Man_bitControl))==(?);
  List_HiddenConstants(Machine(Z80Man_bitControl))==(?);
  External_List_HiddenConstants(Machine(Z80Man_bitControl))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Z80Man_bitControl))==(btrue);
  Context_List_Properties(Machine(Z80Man_bitControl))==(is_zero: BYTE --> BIT & is_zero = %w1.(w1: BYTE | bool_bit(bool(w1(0)+w1(1)+w1(2)+w1(3)+w1(4)+w1(5)+w1(6)+w1(7) = 0))) & is_zeroUSHORT: USHORT --> BIT & is_zeroUSHORT = %nat1.(nat1: USHORT | bool_bit(bool(nat1 = 0))) & is_negative: BYTE --> BIT & is_negative = %w1.(w1: BYTE | w1(7)) & half: UCHAR --> UCHAR & half = %ww.(ww: UCHAR | ww mod 2**4) & simple_add8UCHAR: UCHAR*UCHAR --> UCHAR & simple_add8UCHAR = %(w1,w2).(w1: UCHAR & w2: UCHAR | (w1+w2) mod 2**8) & add8UCHAR: BIT*UCHAR*UCHAR --> UCHAR*BIT*BIT*BIT*BIT & add8UCHAR = %(carry,w1,w2).(carry: BIT & w1: UCHAR & w2: UCHAR | (carry+w1+w2) mod 256,bool_bit(bool(carry+uchar_schar(w1)+uchar_schar(w2)<0)),bool_bit(bool(carry+w1+w2>2**8-1)),bool_bit(bool(carry+half(w1)+half(w2)>=2**4)),bool_bit(bool((carry+w1+w2) mod 2**8-1-1 = 0))) & substract8UCHAR: BIT*UCHAR*UCHAR --> UCHAR*BIT*BIT*BIT*BIT & substract8UCHAR = %(carry,w1,w2).(carry: BIT & w1: UCHAR & w2: UCHAR | (carry+w1-w2) mod 256,bool_bit(bool(carry+uchar_schar(w1)-uchar_schar(w2)<0)),bool_bit(bool(carry+w1-w2>2**8-1)),bool_bit(bool(carry+half(w1)-half(w2)>=2**4)),bool_bit(bool((carry+w1-w2) mod 2**8-1-1 = 0))) & add16USHORT: BIT*USHORT*USHORT --> USHORT & add16USHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | (b1+w1+w2) mod 65536) & add_carryUSHORT: BIT*USHORT*USHORT --> BIT & add_carryUSHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | bool_bit(bool(b1+w1+w2>2**16))) & add_halfcarryUSHORT: BIT*USHORT*USHORT --> BIT & add_halfcarryUSHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | bool_bit(bool(b1+w1 mod 2**12+w2 mod 2**12>2**12))) & sub16USHORT: BIT*USHORT*USHORT --> USHORT & sub16USHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | (w1-w2-b1) mod 65536) & sub_carryUSHORT: BIT*USHORT*USHORT --> BIT & sub_carryUSHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | bool_bit(bool(w1-w2-b1>2**16))) & sub_halfcarryUSHORT: BIT*USHORT*USHORT --> BIT & sub_halfcarryUSHORT = %(b1,w1,w2).(b1: BIT & w1: USHORT & w2: USHORT | bool_bit(bool(w1 mod 2**12-w2 mod 2**12-b1>2**12))) & inc_BYTE: BYTE --> BYTE & inc_BYTE = %w1.(w1: BYTE | uchar_byte((byte_uchar(w1)+1) mod 256)) & dec_BYTE: BYTE --> BYTE & dec_BYTE = %w1.(w1: BYTE | uchar_byte((byte_uchar(w1)-1) mod 256)) & inc_BV16: BV16 --> BV16 & inc_BV16 = %w1.(w1: BV16 | ushort_bv16((bv16_ushort(w1)+1) mod 65536)) & dec_BV16: BYTE --> BYTE & dec_BV16 = %w1.(w1: BV16 | ushort_bv16((bv16_ushort(w1)-1) mod 65536)) & parity_even_BYTE: BIT_VECTOR --> BIT & parity_even_BYTE = %bv.(bv: BIT_VECTOR | 1-(bv_get(bv,0)+bv_get(bv,1)+bv_get(bv,2)+bv_get(bv,3)+bv_get(bv,4)+bv_get(bv,5)+bv_get(bv,7)) mod 2) & and: BYTE*BYTE --> BYTE & and = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_and(bt1,bt2)) & ior: BYTE*BYTE --> BYTE & ior = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_or(bt1,bt2)) & xor: BYTE*BYTE --> BYTE & xor = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_xor(bt1,bt2)) & bitget: BYTE*BYTE_INDEX --> BIT & bitget = %(bt1,ii).(bt1: BYTE & ii: BYTE_INDEX | bt1(ii)) & bitset: BYTE*BYTE_INDEX --> BYTE & !(ww,ii).(ww: BYTE & ii: BYTE_INDEX => bitset(ww,ii) = bv_set(ww,ii)) & bitclear: BYTE*BYTE_INDEX --> BYTE & !(ww,ii,bb).(ww: BYTE & ii: BYTE_INDEX & bb: BIT => bitclear(ww,ii) = bv_clear(ww,ii)) & complement: BYTE --> BYTE & complement = %bt.(bt: BYTE | bv_not(bt)) & swap: BYTE --> BYTE & swap = %bt.(bt: BYTE | {0|->bt(4),1|->bt(5),2|->bt(6),3|->bt(7),4|->bt(0),5|->bt(1),6|->bt(2),7|->bt(3)}) & rotateleft: BYTE --> BYTE & rotateleft = %bv.(bv: BYTE | {0|->bv(7),1|->bv(0),2|->bv(1),3|->bv(2),4|->bv(3),5|->bv(4),6|->bv(5),7|->bv(6)}) & rotateright: BYTE --> BYTE & rotateright = %bv.(bv: BYTE | {0|->bv(1),1|->bv(2),2|->bv(3),3|->bv(4),4|->bv(5),5|->bv(6),6|->bv(7),7|->bv(0)}) & update_refresh_reg: BYTE --> BYTE & update_refresh_reg = %v0.(v0: BYTE | uchar_byte(2**7*v0(7)+(2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) mod 64)) & NB_INSTRUCTIONS: NATURAL & INST_SZ: NATURAL & INST_SZ = 16 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = USHORT & instruction_next: USHORT --> USHORT & instruction_next = %w1.(w1: USHORT | (w1+1) mod 65535) & instruction_jump = %(p0,e0).(p0: INSTRUCTION & e0: -126..129 | (p0+e0) mod 65535) & daa_function: BIT*BIT*BIT*BYTE --> BYTE*BIT*BIT & !(zn,c0,h0,value).(zn: BIT & c0: BIT & h0: BIT & value: BYTE => (zn = 0 & c0 = 0 & get_upper_digit(value): 0..9 & h0 = 0 & get_lower_digit(value): 0..9 => daa_function(zn,c0,h0,value) = value,0,0) & (zn = 0 & c0 = 0 & get_upper_digit(value): 0..8 & h0 = 0 & get_lower_digit(value): 10..15 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),6)),0,bool_bit(bool(half(byte_uchar(value))+half(6)>=2**4))) & (zn = 0 & c0 = 0 & get_upper_digit(value): 0..9 & h0 = 1 & get_lower_digit(value): 0..3 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),6)),0,bool_bit(bool(half(byte_uchar(value))+half(6)>=2**4))) & (zn = 0 & c0 = 0 & get_upper_digit(value): 10..15 & h0 = 0 & get_lower_digit(value): 0..9 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),96)),1,bool_bit(bool(half(byte_uchar(value))+half(96)>=2**4))) & (zn = 0 & c0 = 0 & get_upper_digit(value): 9..15 & h0 = 0 & get_lower_digit(value): 10..15 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),102)),1,bool_bit(bool(half(byte_uchar(value))+half(102)>=2**4))) & (zn = 0 & c0 = 0 & get_upper_digit(value): 10..15 & h0 = 1 & get_lower_digit(value): 0..3 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),102)),1,bool_bit(bool(half(byte_uchar(value))+half(102)>=2**4))) & (zn = 0 & c0 = 1 & get_upper_digit(value): 0..2 & h0 = 0 & get_lower_digit(value): 0..9 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),96)),1,bool_bit(bool(half(byte_uchar(value))+half(96)>=2**4))) & (zn = 0 & c0 = 1 & get_upper_digit(value): 0..2 & h0 = 0 & get_lower_digit(value): 10..15 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),102)),1,bool_bit(bool(half(byte_uchar(value))+half(102)>=2**4))) & (zn = 0 & c0 = 1 & get_upper_digit(value): 0..3 & h0 = 1 & get_lower_digit(value): 0..3 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),102)),1,bool_bit(bool(half(byte_uchar(value))+half(102)>=2**4))) & (zn = 1 & c0 = 0 & get_upper_digit(value): 0..9 & h0 = 0 & get_lower_digit(value): 0..9 => daa_function(zn,c0,h0,value) = value,0,0) & (zn = 1 & c0 = 0 & get_upper_digit(value): 0..8 & h0 = 1 & get_lower_digit(value): 6..15 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),250)),0,bool_bit(bool(half(byte_uchar(value))+half(250)>=2**4))) & (zn = 1 & c0 = 1 & get_upper_digit(value): 7..15 & h0 = 0 & get_lower_digit(value): 0..9 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),160)),1,bool_bit(bool(half(byte_uchar(value))+half(160)>=2**4))) & (zn = 1 & c0 = 1 & get_upper_digit(value): 6..7 & h0 = 1 & get_lower_digit(value): 6..15 => daa_function(zn,c0,h0,value) = uchar_byte(simple_add8UCHAR(byte_uchar(value),154)),1,bool_bit(bool(half(byte_uchar(value))+half(154)>=2**4)))) & BIT = 0..1 & bit_not: BIT --> BIT & bit_not = %bb.(bb: BIT | 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_bit: BOOL --> BIT & bool_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = seq1(BIT) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_index: BIT_VECTOR --> POW(NATURAL) & bv_index = %v1.(v1: BIT_VECTOR | 0..bv_size(v1)-1) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: low..bv_size(bv)-1 | %i0.(i0: 1..high-low+1 | bv(i0+low))) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (1..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (1..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 1..bv_size(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_xor(v1(idx),v2(idx)))) & bv_get: BIT_VECTOR*NATURAL --> BIT & bv_get = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1(idx+1)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 & bit: BIT | v1<+{idx+1|->bit}) & BYTE_INDEX = 1..8 & REAL_BYTE_INDEX = 0..8-1 & BYTE = {bt | bt: BIT_VECTOR & bv_size(bt) = 8} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & byte_bit_get: BYTE*REAL_BYTE_INDEX --> BIT & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)) & get_upper_digit = %by.(by: BYTE | 2**3*by(8)+2**2*by(7)+2*by(6)+by(5)) & get_lower_digit = %by.(by: BYTE | 2**3*by(4)+2**2*by(3)+2*by(2)+by(1)) & BV16_INDX = 1..16 & REAL_BV16_INDX = 0..16-1 & BV16 = {bt | bt: BIT_VECTOR & bv_size(bt) = 16} & BV16_ZERO = BV16_INDX*{0} & bv16_byte: BV16 --> BYTE*BYTE & bv16_byte = %bv.(bv: BV16 | {0|->bv(8),1|->bv(9),2|->bv(10),3|->bv(11),4|->bv(12),5|->bv(13),6|->bv(14),7|->bv(15)},{0|->bv(0),1|->bv(1),2|->bv(2),3|->bv(3),4|->bv(4),5|->bv(5),6|->bv(6),7|->bv(7)}) & byte_bv16 = bv16_byte~ & bv16_bit_get: BYTE*BV16_INDX --> BIT & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)) & UCHAR = 0..2**8-1 & byte_uchar: BYTE --> UCHAR & byte_uchar = %v0.(v0: BYTE | 2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & uchar_byte: UCHAR --> BYTE & uchar_byte = byte_uchar~ & SCHAR = (-2)**7..2**7-1 & byte_schar: BYTE --> SCHAR & byte_schar = %v0.(v0: BYTE | (-2)**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & schar_byte: SCHAR --> BYTE & schar_byte = byte_schar~ & uchar_schar: UCHAR --> SCHAR & uchar_schar = %v1.(v1: UCHAR & v1<=2**7-1 | v1) & uchar_schar = %v1.(v1: UCHAR & not(v1<=2**7-1) | v1-2**8-1+1) & schar_uchar: SCHAR --> UCHAR & schar_uchar = uchar_schar~ & SSHORT = -(2**15)..2**15-1 & bv16_sshort: BV16 --> SSHORT & bv16_sshort = %v0.(v0: BV16 | (-2)**15*v0(15)+2**14*v0(14)+2**13*v0(13)+2**12*v0(12)+2**11*v0(11)+2**10*v0(10)+2**9*v0(9)+2**8*v0(8)+2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & sshort_bv16: SSHORT --> BV16 & sshort_bv16 = bv16_sshort~ & schar_sshort: SCHAR*SCHAR --> SSHORT & schar_sshort = %(w1,w2).(w1: SCHAR & w2: SCHAR | bv16_sshort(byte_bv16(schar_byte(w1),schar_byte(w2)))) & sshort_schar: SSHORT --> SCHAR*SCHAR & sshort_schar = schar_sshort~ & USHORT = 0..2**16-1 & bv16_ushort: BV16 --> USHORT & bv16_ushort = %v0.(v0: BV16 | 32768*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & ushort_bv16 = bv16_ushort~);
  Inherited_List_Properties(Machine(Z80Man_bitControl))==(RESERVED_ADDRESS = 0..16384);
  List_Properties(Machine(Z80Man_bitControl))==(get_bv_reg16: BV16*(id_reg_8 --> BYTE)*id_reg_16 --> BV16 & !(sp_,rgs8_,r1).(sp_: BV16 & rgs8_: id_reg_8 --> BYTE & r1: id_reg_16 => (r1 = BC => get_bv_reg16(sp_,rgs8_,r1) = byte_bv16(rgs8_(b0),rgs8_(c0))) & (r1 = DE => get_bv_reg16(sp_,rgs8_,r1) = byte_bv16(rgs8_(d0),rgs8_(e0))) & (r1 = HL => get_bv_reg16(sp_,rgs8_,r1) = byte_bv16(rgs8_(h0),rgs8_(l0))) & (r1 = SP => get_bv_reg16(sp_,rgs8_,r1) = sp_) & (r1 = AF => get_bv_reg16(sp_,rgs8_,r1) = byte_bv16(rgs8_(a0),rgs8_(f0)))) & REG16_TO_REG8: id_reg_16 --> id_reg_8*id_reg_8 & REG16_TO_REG8(BC) = b0,c0 & REG16_TO_REG8(DE) = d0,e0 & REG16_TO_REG8(HL) = h0,l0 & REG16_TO_REG8(AF) = a0,f0 & REG8_TO_REG16 = REG16_TO_REG8~ & update_flag_register_SZ_H_PvNC = %(rgs8_,s7,z6,h4,pv2,n_add_sub,c0).(rgs8_: id_reg_8 --> BYTE & s7: BIT & z6: BIT & h4: BIT & pv2: BIT & n_add_sub: BIT & c0: BIT | rgs8_<+{f0|->{7|->s7,6|->z6,4|->h4,2|->pv2,1|->n_add_sub,0|->c0}}) & get_new_flag_register_SZ_H_PvNC = %(rgs8_,s7,z6,h4,pv2,n_add_sub,c0).(rgs8_: id_reg_8 --> BYTE & s7: BIT & z6: BIT & h4: BIT & pv2: BIT & n_add_sub: BIT & c0: BIT | f0|->{7|->s7,6|->z6,4|->h4,2|->pv2,1|->n_add_sub,0|->c0}) & bv_ireg_plus_d = %(ix_iy,desloc).(ix_iy: BV16 & desloc: SCHAR | ushort_bv16((bv16_ushort(ix_iy)+desloc) mod 65536)) & bv_9ireg_plus_d0 = %(mem,ix_iy,desloc).(mem: BV16 --> BYTE & ix_iy: BV16 & desloc: SCHAR | mem(bv_ireg_plus_d(ix_iy,desloc))) & cc_get: (id_reg_8 --> BYTE)*(0..8) --> BIT & !rgs8_.(rgs8_: id_reg_8 --> BYTE => cc_get(rgs8_,0) = 1-bitget(rgs8_(f0),6) & cc_get(rgs8_,1) = bitget(rgs8_(f0),6) & cc_get(rgs8_,2) = 1-bitget(rgs8_(f0),0) & cc_get(rgs8_,3) = bitget(rgs8_(f0),0) & cc_get(rgs8_,4) = 1-bitget(rgs8_(f0),2) & cc_get(rgs8_,5) = bitget(rgs8_(f0),2) & cc_get(rgs8_,6) = 1-bitget(rgs8_(f0),7) & cc_get(rgs8_,7) = bitget(rgs8_(f0),7)) & id_reg_8: FIN(INTEGER) & not(id_reg_8 = {}) & id_reg_16: FIN(INTEGER) & not(id_reg_16 = {}))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(Z80Man_bitControl),Machine(POWER2))==(?);
  Seen_Context_List_Enumerated(Machine(Z80Man_bitControl))==(?);
  Seen_Context_List_Invariant(Machine(Z80Man_bitControl))==(btrue);
  Seen_Context_List_Assertions(Machine(Z80Man_bitControl))==(!bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_bit(TRUE) = 1 & bool_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_not(bv_not(bv)),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: 0..bv_size(bv)-1 & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,v2),indx) = bv_get(bv_and(v2,v1),indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,bv_and(v2,v3)),indx) = bv_get(bv_and(bv_and(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv_zero(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_one(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,v2),indx) = bv_get(bv_or(v2,v1),indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,bv_or(v2,v3)),indx) = bv_get(bv_or(bv_or(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_one(bv_size(bv))),indx) = bv_get(bv_one(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_xor(v1,v2),indx) = bv_get(bv_xor(v2,v1),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_xor(bv,bv),indx) = bv_get(bv_zero(bv_size(bv)),indx))) & !bt.(bt: BYTE => size(bt) = 8) & size(BYTE_ZERO) = 8 & BYTE <: BIT_VECTOR & BYTE_ZERO: BIT_VECTOR & first(BYTE_ZERO) = 0 & 8: NATURAL & BV16_ZERO: BV16 & BV16 <: BIT_VECTOR & BV16_ZERO: BIT_VECTOR & 2**8-1: INTEGER & 0: INTEGER & (2**7-1: INTEGER;(-2)**7: INTEGER) & (-(2**15): SSHORT;2**15-1: SSHORT) & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536) & !(nn,pp).(nn: NATURAL1 & pp: NAT => (pp = 0 => nn**pp = 1) & (pp = 1 => nn**pp = nn) & (pp>1 => nn**pp = nn*nn**(pp-1))));
  Seen_Context_List_Properties(Machine(Z80Man_bitControl))==(BIT = 0..1 & bit_not: BIT --> BIT & bit_not = %bb.(bb: BIT | 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_bit: BOOL --> BIT & bool_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = seq1(BIT) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_index: BIT_VECTOR --> POW(NATURAL) & bv_index = %v1.(v1: BIT_VECTOR | 0..bv_size(v1)-1) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: low..bv_size(bv)-1 | %i0.(i0: 1..high-low+1 | bv(i0+low))) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (1..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (1..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 1..bv_size(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_xor(v1(idx),v2(idx)))) & bv_get: BIT_VECTOR*NATURAL --> BIT & bv_get = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1(idx+1)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 & bit: BIT | v1<+{idx+1|->bit}) & BYTE_INDEX = 1..8 & REAL_BYTE_INDEX = 0..8-1 & BYTE = {bt | bt: BIT_VECTOR & bv_size(bt) = 8} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & byte_bit_get: BYTE*REAL_BYTE_INDEX --> BIT & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)) & get_upper_digit = %by.(by: BYTE | 2**3*by(8)+2**2*by(7)+2*by(6)+by(5)) & get_lower_digit = %by.(by: BYTE | 2**3*by(4)+2**2*by(3)+2*by(2)+by(1)) & BV16_INDX = 1..16 & REAL_BV16_INDX = 0..16-1 & BV16 = {bt | bt: BIT_VECTOR & bv_size(bt) = 16} & BV16_ZERO = BV16_INDX*{0} & bv16_byte: BV16 --> BYTE*BYTE & bv16_byte = %bv.(bv: BV16 | {0|->bv(8),1|->bv(9),2|->bv(10),3|->bv(11),4|->bv(12),5|->bv(13),6|->bv(14),7|->bv(15)},{0|->bv(0),1|->bv(1),2|->bv(2),3|->bv(3),4|->bv(4),5|->bv(5),6|->bv(6),7|->bv(7)}) & byte_bv16 = bv16_byte~ & bv16_bit_get: BYTE*BV16_INDX --> BIT & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)) & UCHAR = 0..2**8-1 & byte_uchar: BYTE --> UCHAR & byte_uchar = %v0.(v0: BYTE | 2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & uchar_byte: UCHAR --> BYTE & uchar_byte = byte_uchar~ & SCHAR = (-2)**7..2**7-1 & byte_schar: BYTE --> SCHAR & byte_schar = %v0.(v0: BYTE | (-2)**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & schar_byte: SCHAR --> BYTE & schar_byte = byte_schar~ & uchar_schar: UCHAR --> SCHAR & uchar_schar = %v1.(v1: UCHAR & v1<=2**7-1 | v1) & uchar_schar = %v1.(v1: UCHAR & not(v1<=2**7-1) | v1-2**8-1+1) & schar_uchar: SCHAR --> UCHAR & schar_uchar = uchar_schar~ & SSHORT = -(2**15)..2**15-1 & bv16_sshort: BV16 --> SSHORT & bv16_sshort = %v0.(v0: BV16 | (-2)**15*v0(15)+2**14*v0(14)+2**13*v0(13)+2**12*v0(12)+2**11*v0(11)+2**10*v0(10)+2**9*v0(9)+2**8*v0(8)+2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & sshort_bv16: SSHORT --> BV16 & sshort_bv16 = bv16_sshort~ & schar_sshort: SCHAR*SCHAR --> SSHORT & schar_sshort = %(w1,w2).(w1: SCHAR & w2: SCHAR | bv16_sshort(byte_bv16(schar_byte(w1),schar_byte(w2)))) & sshort_schar: SSHORT --> SCHAR*SCHAR & sshort_schar = schar_sshort~ & USHORT = 0..2**16-1 & bv16_ushort: BV16 --> USHORT & bv16_ushort = %v0.(v0: BV16 | 32768*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & ushort_bv16 = bv16_ushort~);
  Seen_List_Constraints(Machine(Z80Man_bitControl))==(btrue);
  Seen_List_Operations(Machine(Z80Man_bitControl),Machine(POWER2))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80Man_bitControl),Machine(POWER2))==(btrue);
  Seen_Internal_List_Operations(Machine(Z80Man_bitControl),Machine(USHORT_DEFINITION))==(?);
  Seen_List_Operations(Machine(Z80Man_bitControl),Machine(USHORT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80Man_bitControl),Machine(USHORT_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(Z80Man_bitControl),Machine(SSHORT_DEFINITION))==(?);
  Seen_List_Operations(Machine(Z80Man_bitControl),Machine(SSHORT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80Man_bitControl),Machine(SSHORT_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(Z80Man_bitControl),Machine(SCHAR_DEFINITION))==(?);
  Seen_List_Operations(Machine(Z80Man_bitControl),Machine(SCHAR_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80Man_bitControl),Machine(SCHAR_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(Z80Man_bitControl),Machine(UCHAR_DEFINITION))==(?);
  Seen_List_Operations(Machine(Z80Man_bitControl),Machine(UCHAR_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80Man_bitControl),Machine(UCHAR_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(Z80Man_bitControl),Machine(BV16_DEFINITION))==(?);
  Seen_List_Operations(Machine(Z80Man_bitControl),Machine(BV16_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80Man_bitControl),Machine(BV16_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(Z80Man_bitControl),Machine(BYTE_DEFINITION))==(?);
  Seen_List_Operations(Machine(Z80Man_bitControl),Machine(BYTE_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80Man_bitControl),Machine(BYTE_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(Z80Man_bitControl),Machine(BIT_VECTOR_DEFINITION))==(?);
  Seen_List_Operations(Machine(Z80Man_bitControl),Machine(BIT_VECTOR_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80Man_bitControl),Machine(BIT_VECTOR_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(Z80Man_bitControl),Machine(BIT_DEFINITION))==(?);
  Seen_List_Operations(Machine(Z80Man_bitControl),Machine(BIT_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80Man_bitControl),Machine(BIT_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(Z80Man_bitControl),Machine(ALU))==(?);
  Seen_List_Operations(Machine(Z80Man_bitControl),Machine(ALU))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80Man_bitControl),Machine(ALU))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(Z80Man_bitControl),RLCA)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),RLA)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),RRCA)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),RRA)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),RLC_r)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),RLC_9HL0)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),RLC_9IX_d9)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),RLC_9IY_d9)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),RL_r)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),RL_9HL)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),RL_9IX_d9)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),RL_9IY_d9)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),RRC_r)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),RRC_9HL0)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),RRC_9IX_d9)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),RRC_9IY_d9)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),RR_r)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),RR_9HL)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),RR_9IX_d9)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),RR_9IY_d9)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),SLA_r)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),SLA_9HL)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),SLA_9IX_d9)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),SLA_9IY_d9)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),SRA_r)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),SRA_9HL0)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),SRA_9IX_d9)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),SRA_9IY_d9)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),SRL_r)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),SRL_9HL0)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),SRL_9IX_d9)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),SRL_9IY_d9)==(Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80Man_bitControl),RLD)==((Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(acc) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80Man_bitControl),RRD)==((Var(res) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(acc) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80Man_bitControl),BIT_b_rr)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),BIT_b_9HL0)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),BIT_b_9IX_d0)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),BIT_b_9IY_d0)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),SET_b_r)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),SET_b_9HL0)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),SET_b_9IX_d0)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),SET_b_9IY_d0)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),RES_b_r)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),RES_b_9HL0)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),RES_b_9IX_d0)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),RES_b_9IY_d0)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),JP_nn)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),JP_cc_nn)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),JR_e)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),JR_C_e)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),JR_NC_e)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),JR_Z_e)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),JR_NZ_e)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),JP_HL)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),JP_IX)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),JP_IY)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),DJNZ_e)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),CALL_nn)==((Var(high) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(low) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80Man_bitControl),CALL_cc_nn)==((Var(high) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(low) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80Man_bitControl),RET)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),RET_cc)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),RETI)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),RETN)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),RST_p)==((Var(pc_l) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(pc_h) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80Man_bitControl),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Z80Man_bitControl)) == (get_bv_reg16,REG16_TO_REG8,REG8_TO_REG16,update_flag_register_SZ_H_PvNC,get_new_flag_register_SZ_H_PvNC,bv_ireg_plus_d,bv_9ireg_plus_d0,cc_get,id_reg_8,id_reg_16,a0,f0,f_0,a_0,b0,c0,b_0,c_0,d0,e0,d_0,e_0,h0,l0,h_0,l_0,BC,DE,HL,SP,AF | RESERVED_ADDRESS | i_o_ports,im,iff2,iff1,r_,i_,iy,ix,sp,pc,rgs8 | stack,mem | RLCA,RLA,RRCA,RRA,RLC_r,RLC_9HL0,RLC_9IX_d9,RLC_9IY_d9,RL_r,RL_9HL,RL_9IX_d9,RL_9IY_d9,RRC_r,RRC_9HL0,RRC_9IX_d9,RRC_9IY_d9,RR_r,RR_9HL,RR_9IX_d9,RR_9IY_d9,SLA_r,SLA_9HL,SLA_9IX_d9,SLA_9IY_d9,SRA_r,SRA_9HL0,SRA_9IX_d9,SRA_9IY_d9,SRL_r,SRL_9HL0,SRL_9IX_d9,SRL_9IY_d9,RLD,RRD,BIT_b_rr,BIT_b_9HL0,BIT_b_9IX_d0,BIT_b_9IY_d0,SET_b_r,SET_b_9HL0,SET_b_9IX_d0,SET_b_9IY_d0,RES_b_r,RES_b_9HL0,RES_b_9IX_d0,RES_b_9IY_d0,JP_nn,JP_cc_nn,JR_e,JR_C_e,JR_NC_e,JR_Z_e,JR_NZ_e,JP_HL,JP_IX,JP_IY,DJNZ_e,CALL_nn,CALL_cc_nn,RET,RET_cc,RETI,RETN,RST_p | ? | seen(Machine(ALU)),seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BYTE_DEFINITION)),seen(Machine(BV16_DEFINITION)),seen(Machine(UCHAR_DEFINITION)),seen(Machine(SCHAR_DEFINITION)),seen(Machine(SSHORT_DEFINITION)),seen(Machine(USHORT_DEFINITION)),seen(Machine(POWER2)),included(Machine(MEMORY)) | ? | Z80Man_bitControl);
  List_Of_HiddenCst_Ids(Machine(Z80Man_bitControl)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Z80Man_bitControl)) == (get_bv_reg16,REG16_TO_REG8,REG8_TO_REG16,update_flag_register_SZ_H_PvNC,get_new_flag_register_SZ_H_PvNC,bv_ireg_plus_d,bv_9ireg_plus_d0,cc_get,RESERVED_ADDRESS);
  List_Of_VisibleVar_Ids(Machine(Z80Man_bitControl)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Z80Man_bitControl)) == (seen(Machine(ALU)): (is_zero,is_zeroUSHORT,is_negative,half,simple_add8UCHAR,add8UCHAR,substract8UCHAR,add16USHORT,add_carryUSHORT,add_halfcarryUSHORT,sub16USHORT,sub_carryUSHORT,sub_halfcarryUSHORT,inc_BYTE,dec_BYTE,inc_BV16,dec_BV16,parity_even_BYTE,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright,update_refresh_reg,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,instruction_jump,daa_function | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(BIT_DEFINITION)): (BIT,bit_not,bit_and,bit_or,bit_xor,bool_bit | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(BIT_VECTOR_DEFINITION)): (BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(BYTE_DEFINITION)): (BYTE,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get,get_upper_digit,get_lower_digit | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(BV16_DEFINITION)): (BV16_INDX,REAL_BV16_INDX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(UCHAR_DEFINITION)): (UCHAR,uchar_byte,byte_uchar | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(SCHAR_DEFINITION)): (SCHAR,byte_schar,schar_byte,uchar_schar,schar_uchar | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(SSHORT_DEFINITION)): (SSHORT,bv16_sshort,sshort_bv16,schar_sshort,sshort_schar | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(USHORT_DEFINITION)): (USHORT,bv16_ushort,ushort_bv16 | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(POWER2)): (? | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(MEMORY)) == (RESERVED_ADDRESS | ? | stack,mem | ? | updateMem,updateAddressMem,updateStack,updateAddressStack,pop | ? | seen(Machine(ALU)),seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BYTE_DEFINITION)),seen(Machine(BV16_DEFINITION)),seen(Machine(UCHAR_DEFINITION)),seen(Machine(SCHAR_DEFINITION)),seen(Machine(SSHORT_DEFINITION)),seen(Machine(USHORT_DEFINITION)),seen(Machine(POWER2)) | ? | MEMORY);
  List_Of_HiddenCst_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(MEMORY)) == (RESERVED_ADDRESS);
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
  List_Of_Ids(Machine(USHORT_DEFINITION)) == (USHORT,bv16_ushort,ushort_bv16 | ? | ? | ? | ? | ? | seen(Machine(BV16_DEFINITION)) | ? | USHORT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(USHORT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(USHORT_DEFINITION)) == (USHORT,bv16_ushort,ushort_bv16);
  List_Of_VisibleVar_Ids(Machine(USHORT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(USHORT_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BV16_DEFINITION)) == (BV16_INDX,REAL_BV16_INDX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BYTE_DEFINITION)) | ? | BV16_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BV16_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BV16_DEFINITION)) == (BV16_INDX,REAL_BV16_INDX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get);
  List_Of_VisibleVar_Ids(Machine(BV16_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BV16_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get,get_upper_digit,get_lower_digit | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)) | ? | BYTE_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get,get_upper_digit,get_lower_digit);
  List_Of_VisibleVar_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BYTE_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)) | ? | BIT_VECTOR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_bit | ? | ? | ? | ? | ? | ? | ? | BIT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_bit);
  List_Of_VisibleVar_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(SSHORT_DEFINITION)) == (SSHORT,bv16_sshort,sshort_bv16,schar_sshort,sshort_schar | ? | ? | ? | ? | ? | seen(Machine(POWER2)),seen(Machine(BV16_DEFINITION)),seen(Machine(SCHAR_DEFINITION)) | ? | SSHORT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(SSHORT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(SSHORT_DEFINITION)) == (SSHORT,bv16_sshort,sshort_bv16,schar_sshort,sshort_schar);
  List_Of_VisibleVar_Ids(Machine(SSHORT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(SSHORT_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(SCHAR_DEFINITION)) == (SCHAR,byte_schar,schar_byte,uchar_schar,schar_uchar | ? | ? | ? | ? | ? | seen(Machine(BYTE_DEFINITION)),seen(Machine(UCHAR_DEFINITION)) | ? | SCHAR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(SCHAR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(SCHAR_DEFINITION)) == (SCHAR,byte_schar,schar_byte,uchar_schar,schar_uchar);
  List_Of_VisibleVar_Ids(Machine(SCHAR_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(SCHAR_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(UCHAR_DEFINITION)) == (UCHAR,uchar_byte,byte_uchar | ? | ? | ? | ? | ? | seen(Machine(BYTE_DEFINITION)) | ? | UCHAR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(UCHAR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(UCHAR_DEFINITION)) == (UCHAR,uchar_byte,byte_uchar);
  List_Of_VisibleVar_Ids(Machine(UCHAR_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(UCHAR_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(ALU)) == (is_zero,is_zeroUSHORT,is_negative,half,simple_add8UCHAR,add8UCHAR,substract8UCHAR,add16USHORT,add_carryUSHORT,add_halfcarryUSHORT,sub16USHORT,sub_carryUSHORT,sub_halfcarryUSHORT,inc_BYTE,dec_BYTE,inc_BV16,dec_BV16,parity_even_BYTE,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright,update_refresh_reg,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,instruction_jump,daa_function | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BYTE_DEFINITION)),seen(Machine(BV16_DEFINITION)),seen(Machine(UCHAR_DEFINITION)),seen(Machine(SCHAR_DEFINITION)),seen(Machine(SSHORT_DEFINITION)),seen(Machine(USHORT_DEFINITION)),seen(Machine(POWER2)) | ? | ALU);
  List_Of_HiddenCst_Ids(Machine(ALU)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ALU)) == (is_zero,is_zeroUSHORT,is_negative,half,simple_add8UCHAR,add8UCHAR,substract8UCHAR,add16USHORT,add_carryUSHORT,add_halfcarryUSHORT,sub16USHORT,sub_carryUSHORT,sub_halfcarryUSHORT,inc_BYTE,dec_BYTE,inc_BV16,dec_BV16,parity_even_BYTE,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright,update_refresh_reg,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,instruction_jump,daa_function);
  List_Of_VisibleVar_Ids(Machine(ALU)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ALU)) == (?: ?)
END
&
THEORY SetsEnvX IS
  Sets(Machine(Z80Man_bitControl)) == (Type(id_reg_8) == Cst(SetOf(etype(id_reg_8,0,15)));Type(id_reg_16) == Cst(SetOf(etype(id_reg_16,0,4))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(Z80Man_bitControl)) == (Type(RESERVED_ADDRESS) == Cst(SetOf(btype(INTEGER,"[RESERVED_ADDRESS","]RESERVED_ADDRESS")));Type(a0) == Cst(etype(id_reg_8,0,15));Type(f0) == Cst(etype(id_reg_8,0,15));Type(f_0) == Cst(etype(id_reg_8,0,15));Type(a_0) == Cst(etype(id_reg_8,0,15));Type(b0) == Cst(etype(id_reg_8,0,15));Type(c0) == Cst(etype(id_reg_8,0,15));Type(b_0) == Cst(etype(id_reg_8,0,15));Type(c_0) == Cst(etype(id_reg_8,0,15));Type(d0) == Cst(etype(id_reg_8,0,15));Type(e0) == Cst(etype(id_reg_8,0,15));Type(d_0) == Cst(etype(id_reg_8,0,15));Type(e_0) == Cst(etype(id_reg_8,0,15));Type(h0) == Cst(etype(id_reg_8,0,15));Type(l0) == Cst(etype(id_reg_8,0,15));Type(h_0) == Cst(etype(id_reg_8,0,15));Type(l_0) == Cst(etype(id_reg_8,0,15));Type(BC) == Cst(etype(id_reg_16,0,4));Type(DE) == Cst(etype(id_reg_16,0,4));Type(HL) == Cst(etype(id_reg_16,0,4));Type(SP) == Cst(etype(id_reg_16,0,4));Type(AF) == Cst(etype(id_reg_16,0,4));Type(get_bv_reg16) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(etype(id_reg_8,0,15)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*etype(id_reg_16,0,4)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(REG16_TO_REG8) == Cst(SetOf(etype(id_reg_16,0,4)*(etype(id_reg_8,0,15)*etype(id_reg_8,0,15))));Type(REG8_TO_REG16) == Cst(SetOf(etype(id_reg_8,?,?)*etype(id_reg_8,?,?)*etype(id_reg_16,?,?)));Type(update_flag_register_SZ_H_PvNC) == Cst(SetOf(SetOf(etype(id_reg_8,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*SetOf(etype(id_reg_8,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))));Type(get_new_flag_register_SZ_H_PvNC) == Cst(SetOf(SetOf(etype(id_reg_8,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*(etype(id_reg_8,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))));Type(bv_ireg_plus_d) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_9ireg_plus_d0) == Cst(SetOf(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(cc_get) == Cst(SetOf(SetOf(etype(id_reg_8,0,15)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*btype(INTEGER,0,8)*btype(INTEGER,"[BIT","]BIT"))))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Z80Man_bitControl)) == (Type(mem) == Mvl(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(stack) == Mvl(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(i_o_ports) == Mvl(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(im) == Mvl(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(iff2) == Mvl(btype(INTEGER,?,?));Type(iff1) == Mvl(btype(INTEGER,?,?));Type(r_) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(i_) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(iy) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(ix) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(sp) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(pc) == Mvl(btype(INTEGER,?,?));Type(rgs8) == Mvl(SetOf(etype(id_reg_8,0,15)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Z80Man_bitControl)) == (Type(RST_p) == Cst(No_type,btype(INTEGER,?,?));Type(RETN) == Cst(No_type,No_type);Type(RETI) == Cst(No_type,No_type);Type(RET_cc) == Cst(No_type,btype(INTEGER,?,?));Type(RET) == Cst(No_type,No_type);Type(CALL_cc_nn) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(CALL_nn) == Cst(No_type,btype(INTEGER,?,?));Type(DJNZ_e) == Cst(No_type,btype(INTEGER,?,?));Type(JP_IY) == Cst(No_type,No_type);Type(JP_IX) == Cst(No_type,No_type);Type(JP_HL) == Cst(No_type,No_type);Type(JR_NZ_e) == Cst(No_type,btype(INTEGER,?,?));Type(JR_Z_e) == Cst(No_type,btype(INTEGER,?,?));Type(JR_NC_e) == Cst(No_type,btype(INTEGER,?,?));Type(JR_C_e) == Cst(No_type,btype(INTEGER,?,?));Type(JR_e) == Cst(No_type,btype(INTEGER,?,?));Type(JP_cc_nn) == Cst(No_type,btype(BOOL,?,?)*btype(INTEGER,?,?));Type(JP_nn) == Cst(No_type,btype(INTEGER,?,?));Type(RES_b_9IY_d0) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RES_b_9IX_d0) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RES_b_9HL0) == Cst(No_type,btype(INTEGER,?,?));Type(RES_b_r) == Cst(No_type,btype(INTEGER,?,?)*etype(id_reg_8,?,?));Type(SET_b_9IY_d0) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SET_b_9IX_d0) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SET_b_9HL0) == Cst(No_type,btype(INTEGER,?,?));Type(SET_b_r) == Cst(No_type,btype(INTEGER,?,?)*etype(id_reg_8,?,?));Type(BIT_b_9IY_d0) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BIT_b_9IX_d0) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BIT_b_9HL0) == Cst(No_type,btype(INTEGER,?,?));Type(BIT_b_rr) == Cst(No_type,btype(INTEGER,?,?)*etype(id_reg_8,?,?));Type(RRD) == Cst(No_type,No_type);Type(RLD) == Cst(No_type,No_type);Type(SRL_9IY_d9) == Cst(No_type,btype(INTEGER,?,?));Type(SRL_9IX_d9) == Cst(No_type,btype(INTEGER,?,?));Type(SRL_9HL0) == Cst(No_type,No_type);Type(SRL_r) == Cst(No_type,etype(id_reg_8,?,?));Type(SRA_9IY_d9) == Cst(No_type,btype(INTEGER,?,?));Type(SRA_9IX_d9) == Cst(No_type,btype(INTEGER,?,?));Type(SRA_9HL0) == Cst(No_type,No_type);Type(SRA_r) == Cst(No_type,etype(id_reg_8,?,?));Type(SLA_9IY_d9) == Cst(No_type,btype(INTEGER,?,?));Type(SLA_9IX_d9) == Cst(No_type,btype(INTEGER,?,?));Type(SLA_9HL) == Cst(No_type,No_type);Type(SLA_r) == Cst(No_type,etype(id_reg_8,?,?));Type(RR_9IY_d9) == Cst(No_type,btype(INTEGER,?,?));Type(RR_9IX_d9) == Cst(No_type,btype(INTEGER,?,?));Type(RR_9HL) == Cst(No_type,No_type);Type(RR_r) == Cst(No_type,etype(id_reg_8,?,?));Type(RRC_9IY_d9) == Cst(No_type,btype(INTEGER,?,?));Type(RRC_9IX_d9) == Cst(No_type,btype(INTEGER,?,?));Type(RRC_9HL0) == Cst(No_type,No_type);Type(RRC_r) == Cst(No_type,etype(id_reg_8,?,?));Type(RL_9IY_d9) == Cst(No_type,btype(INTEGER,?,?));Type(RL_9IX_d9) == Cst(No_type,btype(INTEGER,?,?));Type(RL_9HL) == Cst(No_type,No_type);Type(RL_r) == Cst(No_type,etype(id_reg_8,?,?));Type(RLC_9IY_d9) == Cst(No_type,btype(INTEGER,?,?));Type(RLC_9IX_d9) == Cst(No_type,btype(INTEGER,?,?));Type(RLC_9HL0) == Cst(No_type,No_type);Type(RLC_r) == Cst(No_type,etype(id_reg_8,?,?));Type(RRA) == Cst(No_type,No_type);Type(RRCA) == Cst(No_type,No_type);Type(RLA) == Cst(No_type,No_type);Type(RLCA) == Cst(No_type,No_type))
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
