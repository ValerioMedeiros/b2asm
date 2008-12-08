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
  List_Sees(Machine(Z80Man_bitControl))==(TYPES,ALU,POWER2)
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
  Local_List_Variables(Machine(Z80Man_bitControl))==(i_o_ports,iy,ix,sp,pc,rgs8);
  List_Variables(Machine(Z80Man_bitControl))==(i_o_ports,iy,ix,sp,pc,rgs8,stack,mem);
  External_List_Variables(Machine(Z80Man_bitControl))==(i_o_ports,iy,ix,sp,pc,rgs8,stack,mem)
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
  Expanded_List_Invariant(Machine(Z80Man_bitControl))==(stack: BV16 >-> BYTE & mem: BV16 >-> BYTE);
  Context_List_Invariant(Machine(Z80Man_bitControl))==(btrue);
  List_Invariant(Machine(Z80Man_bitControl))==(stack: BV16 --> BYTE & rgs8: id_reg_8 --> BYTE & pc: INSTRUCTION & sp: BV16 & ix: BV16 & iy: BV16 & i_o_ports: BYTE --> BYTE)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Machine(Z80Man_bitControl))==(btrue);
  Expanded_List_Assertions(Machine(Z80Man_bitControl))==(dom(stack) = BV16 & ran(stack) <: BYTE & ran(mem) <: BYTE & dom(mem) = BV16);
  Context_List_Assertions(Machine(Z80Man_bitControl))==(NB_SCHARS = 256 & !n0.(n0: SCHAR => 0<=n0) & !n0.(n0: SCHAR => n0<=255) & 2**16 = 65536 & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & 0 = SCHAR_TO_SSHORTINT(0,0) & !xx.(xx: BYTE => BYTE_TO_UCHAR(xx): UCHAR) & !xx.(xx: UCHAR => UCHAR_TO_BYTE(xx): BYTE) & !xx.(xx: BYTE => BYTE_TO_SCHAR(xx): SCHAR) & !xx.(xx: SCHAR => SCHAR_TO_BYTE(xx): BYTE) & !(xx,yy).(xx: BYTE & yy: BYTE => BYTE_TO_BV16(xx,yy): BV16) & !(xx,yy).(xx: BYTE & yy: BYTE => #zz.(zz: BV16 & BYTE_TO_BV16(xx,yy) = zz)) & !xx.(xx: BV16 => BV16_TO_USHORTINT(xx): USHORTINT) & !xx.(xx: USHORTINT => USHORTINT_TO_BV16(xx): BV16) & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_to_bit(TRUE) = 1 & bool_to_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_not(bv_not(bv))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_and(v1,v2)(indx) = bv_and(v2,v1)(indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_and(v1,bv_and(v2,v3))(indx) = bv_and(bv_and(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_zero(bv_size(bv)))(indx) = bv_zero(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_one(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_or(v1,v2)(indx) = bv_or(v2,v1)(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_or(v1,bv_or(v2,v3))(indx) = bv_or(bv_or(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_one(bv_size(bv)))(indx) = bv_one(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_zero(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_xor(v1,v2)(indx) = bv_xor(v2,v1)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_xor(bv,bv)(indx) = bv_zero(bv_size(bv))(indx))) & !bt.(bt: BYTE => size(bt) = 8) & size(BYTE_ZERO) = 8 & BYTE <: BIT_VECTOR & BYTE_ZERO: BIT_VECTOR & first(BYTE_ZERO) = 0 & dom(add8SCHAR) = BIT*SCHAR*SCHAR & ran(add8SCHAR): POW(SCHAR*BIT*BIT*BIT*BIT) & dom(substract8SCHAR) = BIT*SCHAR*SCHAR & ran(substract8SCHAR): POW(SCHAR*BIT*BIT*BIT*BIT) & dom(and) = BYTE*BYTE & ran(and) <: BYTE & dom(ior) = BYTE*BYTE & ran(ior) <: BYTE & dom(xor) = BYTE*BYTE & ran(xor) <: BYTE & dom(complement) = BYTE & ran(complement) <: BYTE & dom(swap) = BYTE & ran(swap) <: BYTE & dom(rotateleft) = BYTE & ran(rotateleft) <: BYTE & dom(rotateright) = BYTE & ran(rotateright) <: BYTE & !(vec,in0).(vec: BYTE & in0: 0..7 => bitget(vec,in0) = vec(in0)) & !(xx,yy).(xx: INTEGER & yy: INTEGER => xx mod yy<yy & xx mod yy>=0) & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536));
  List_Assertions(Machine(Z80Man_bitControl))==(dom(stack) = BV16 & ran(stack) <: BYTE & ran(mem) <: BYTE & dom(mem) = BV16 & ran(rgs8) <: BYTE & dom(rgs8) = id_reg_8 & INSTRUCTION_NEXT(0) = 1 & INSTRUCTION_NEXT(1) = 2 & INSTRUCTION_NEXT(2) = 3 & INSTRUCTION_NEXT(3) = 4 & INSTRUCTION_NEXT(4) = 5 & INSTRUCTION_NEXT(5) = 6 & INSTRUCTION_NEXT(6) = 7 & INSTRUCTION_NEXT(7) = 8 & INSTRUCTION_NEXT(8) = 9 & INSTRUCTION_NEXT(9) = 10 & INSTRUCTION_NEXT(10) = 11 & INSTRUCTION_NEXT(11) = 12 & INSTRUCTION_NEXT(12) = 13 & INSTRUCTION_NEXT(13) = 14 & mem(BYTE_TO_BV16(rgs8(b0),rgs8(c0))): BYTE & mem(BYTE_TO_BV16(SCHAR_TO_BYTE(0),mem(BYTE_TO_BV16(rgs8(b0),rgs8(c0))))): BYTE & mem(BYTE_TO_BV16(rgs8(d0),rgs8(e0))): BYTE & mem(BYTE_TO_BV16(SCHAR_TO_BYTE(0),mem(BYTE_TO_BV16(rgs8(d0),rgs8(e0))))): BYTE & mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))): BYTE & mem(BYTE_TO_BV16(SCHAR_TO_BYTE(0),mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))))): BYTE & mem(BYTE_TO_BV16(rgs8(a0),rgs8(f0))): BYTE & mem(BYTE_TO_BV16(SCHAR_TO_BYTE(0),mem(BYTE_TO_BV16(rgs8(a0),rgs8(f0))))): BYTE & mem(sp): BYTE & mem(ix): BYTE & mem(iy): BYTE & !(ii,des).(ii: BV16 & des: SCHAR => bv_IX_plus_d(ii,des): BV16) & !(ii,des).(ii: BV16 & des: SCHAR => bv_IY_plus_d(ii,des): BV16) & !(mmm,ii,des).(mmm: BV16 >-> BYTE & ii: BV16 & des: SCHAR => bv_9IX_plus_d0(mmm,ii,des): BYTE) & !(mmm,ii,des).(mmm: BV16 >-> BYTE & ii: BV16 & des: SCHAR => bv_9IY_plus_d0(mmm,ii,des): BYTE))
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Z80Man_bitControl))==(@(mem$0).(mem$0: BV16 >-> BYTE ==> mem:=mem$0) || @(stack$0).(stack$0: BV16 >-> BYTE ==> stack:=stack$0);(@(rgs8$0).(rgs8$0: id_reg_8 --> BYTE ==> rgs8:=rgs8$0) || @(pc$0).(pc$0: INSTRUCTION ==> pc:=pc$0) || @(sp$0).(sp$0: BV16 ==> sp:=sp$0) || @(ix$0).(ix$0: BV16 ==> ix:=ix$0) || @(iy$0).(iy$0: BV16 ==> iy:=iy$0) || @(i_o_ports$0).(i_o_ports$0: BYTE --> BYTE ==> i_o_ports:=i_o_ports$0)));
  Context_List_Initialisation(Machine(Z80Man_bitControl))==(skip);
  List_Initialisation(Machine(Z80Man_bitControl))==(rgs8:: id_reg_8 --> BYTE || pc:: INSTRUCTION || sp:: BV16 || ix:: BV16 || iy:: BV16 || i_o_ports:: BYTE --> BYTE)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Z80Man_bitControl))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(Z80Man_bitControl),Machine(MEMORY))==(?);
  List_Instanciated_Parameters(Machine(Z80Man_bitControl),Machine(TYPES))==(?);
  List_Instanciated_Parameters(Machine(Z80Man_bitControl),Machine(ALU))==(?);
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
  Internal_List_Operations(Machine(Z80Man_bitControl))==(RLCA,RLA,RRCA,RRA,RLC_r,RLC_9HL0,RLC_9IX_d9,RLC_9IY_d9,RL_r,RL_9HL,RL_9IX_d9,RL_9IY_d9,RRC_r,RRC_9HL0,RRC_9IX_d9,RRC_9IY_d9,RR_r,RR_9HL,RR_9IX_d9,RR_9IY_d9,SLA_r,SLA_9HL,SLA_9IX_d9,SLA_9IY_d9,SRA_r,SRA_9HL0,SRA_9IX_d9,SRA_9IY_d9,SRL_r,SRL_9HL0,SRL_9IX_d9,SRL_9IY_d9,RLD,RRD,BIT_b_rr,BIT_b_9HL0,BIT_b_9IX_d0,BIT_b_9IY_d0,SET_b_r,SET_b_9HL0,SET_b_9IX_d0,SET_b_9IY_d0,RES_b_r,RES_b_9HL0,RES_b_9IX_d0,RES_b_9IY_d0,JP_nn,JP_cc_nn,JR_e,JR_C_e,JR_NC_e,JR_Z_e,JR_NZ_e,JP_HL,JP_IX,JP_IY,DJNZ_e,CALL_nn);
  List_Operations(Machine(Z80Man_bitControl))==(RLCA,RLA,RRCA,RRA,RLC_r,RLC_9HL0,RLC_9IX_d9,RLC_9IY_d9,RL_r,RL_9HL,RL_9IX_d9,RL_9IY_d9,RRC_r,RRC_9HL0,RRC_9IX_d9,RRC_9IY_d9,RR_r,RR_9HL,RR_9IX_d9,RR_9IY_d9,SLA_r,SLA_9HL,SLA_9IX_d9,SLA_9IY_d9,SRA_r,SRA_9HL0,SRA_9IX_d9,SRA_9IY_d9,SRL_r,SRL_9HL0,SRL_9IX_d9,SRL_9IY_d9,RLD,RRD,BIT_b_rr,BIT_b_9HL0,BIT_b_9IX_d0,BIT_b_9IY_d0,SET_b_r,SET_b_9HL0,SET_b_9IX_d0,SET_b_9IY_d0,RES_b_r,RES_b_9HL0,RES_b_9IX_d0,RES_b_9IY_d0,JP_nn,JP_cc_nn,JR_e,JR_C_e,JR_NC_e,JR_Z_e,JR_NZ_e,JP_HL,JP_IX,JP_IY,DJNZ_e,CALL_nn)
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
  List_Input(Machine(Z80Man_bitControl),CALL_nn)==(nn)
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
  List_Output(Machine(Z80Man_bitControl),CALL_nn)==(?)
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
  List_Header(Machine(Z80Man_bitControl),CALL_nn)==(CALL_nn(nn))
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
  List_Precondition(Machine(Z80Man_bitControl),BIT_b_rr)==(bb: 0..SCHAR_LENGTH-1 & rr: id_reg_8);
  List_Precondition(Machine(Z80Man_bitControl),BIT_b_9HL0)==(bb: 0..SCHAR_LENGTH-1);
  List_Precondition(Machine(Z80Man_bitControl),BIT_b_9IX_d0)==(bb: 0..SCHAR_LENGTH-1 & desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),BIT_b_9IY_d0)==(bb: 0..SCHAR_LENGTH-1 & desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),SET_b_r)==(bb: 0..SCHAR_LENGTH-1 & rr: id_reg_8);
  List_Precondition(Machine(Z80Man_bitControl),SET_b_9HL0)==(bb: 0..SCHAR_LENGTH-1);
  List_Precondition(Machine(Z80Man_bitControl),SET_b_9IX_d0)==(bb: 0..SCHAR_LENGTH-1 & desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),SET_b_9IY_d0)==(bb: 0..SCHAR_LENGTH-1 & desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),RES_b_r)==(bb: 0..SCHAR_LENGTH-1 & rr: id_reg_8);
  List_Precondition(Machine(Z80Man_bitControl),RES_b_9HL0)==(bb: 0..SCHAR_LENGTH-1);
  List_Precondition(Machine(Z80Man_bitControl),RES_b_9IX_d0)==(bb: 0..SCHAR_LENGTH-1 & desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),RES_b_9IY_d0)==(bb: 0..SCHAR_LENGTH-1 & desloc: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),JP_nn)==(nn: USHORTINT);
  List_Precondition(Machine(Z80Man_bitControl),JP_cc_nn)==(cc: BOOL & nn: USHORTINT);
  List_Precondition(Machine(Z80Man_bitControl),JR_e)==(ee: SCHAR & ee-2: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),JR_C_e)==(ee: SCHAR & ee-2: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),JR_NC_e)==(ee: SCHAR & ee-2: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),JR_Z_e)==(ee: SCHAR & ee-2: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),JR_NZ_e)==(ee: SCHAR & ee-2: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),JP_HL)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),JP_IX)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),JP_IY)==(btrue);
  List_Precondition(Machine(Z80Man_bitControl),DJNZ_e)==(ee: SCHAR & ee-2: SCHAR);
  List_Precondition(Machine(Z80Man_bitControl),CALL_nn)==(nn: USHORTINT)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Z80Man_bitControl),CALL_nn)==(nn: USHORTINT & sp: BV16 & USHORTINT_TO_BV16(INSTRUCTION_NEXT(pc)): BYTE & stack<+{sp|->USHORTINT_TO_BV16(INSTRUCTION_NEXT(pc))}: BV16 >-> BYTE | stack:=stack<+{sp|->USHORTINT_TO_BV16(INSTRUCTION_NEXT(pc))} || sp:=inc_BV16(sp) || pc:=nn);
  Expanded_List_Substitution(Machine(Z80Man_bitControl),DJNZ_e)==(ee: SCHAR & ee-2: SCHAR | rgs8:=rgs8<+{b0|->dec_BYTE(rgs8(b0))} || (is_zero(dec_BYTE(rgs8(b0))) = 1 ==> pc:=INSTRUCTION_NEXT(pc) [] not(is_zero(dec_BYTE(rgs8(b0))) = 1) ==> pc:=(pc+ee) mod 2**16));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JP_IY)==(btrue | pc:=BV16_TO_USHORTINT(iy));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JP_IX)==(btrue | pc:=BV16_TO_USHORTINT(ix));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JP_HL)==(btrue | pc:=BV16_TO_USHORTINT(BYTE_TO_BV16(rgs8(h0),rgs8(l0))));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JR_NZ_e)==(ee: SCHAR & ee-2: SCHAR | bitget(rgs8(f0),6) = 1 ==> pc:=INSTRUCTION_NEXT(pc) [] not(bitget(rgs8(f0),6) = 1) ==> pc:=(pc+ee) mod 2**16);
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JR_Z_e)==(ee: SCHAR & ee-2: SCHAR | bitget(rgs8(f0),6) = 0 ==> pc:=INSTRUCTION_NEXT(pc) [] not(bitget(rgs8(f0),6) = 0) ==> pc:=(pc+ee) mod 2**16);
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JR_NC_e)==(ee: SCHAR & ee-2: SCHAR | bitget(rgs8(f0),0) = 1 ==> pc:=INSTRUCTION_NEXT(pc) [] not(bitget(rgs8(f0),0) = 1) ==> pc:=(pc+ee) mod 2**16);
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JR_C_e)==(ee: SCHAR & ee-2: SCHAR | bitget(rgs8(f0),0) = 0 ==> pc:=INSTRUCTION_NEXT(pc) [] not(bitget(rgs8(f0),0) = 0) ==> pc:=(pc+ee) mod 2**16);
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JR_e)==(ee: SCHAR & ee-2: SCHAR | pc:=(pc+ee) mod 2**16);
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JP_cc_nn)==(cc: BOOL & nn: USHORTINT | cc = TRUE ==> pc:=nn [] not(cc = TRUE) ==> pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),JP_nn)==(nn: USHORTINT | pc:=nn);
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RES_b_9IY_d0)==(bb: 0..SCHAR_LENGTH-1 & desloc: SCHAR & BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & bv_clear(bv_9IY_plus_d0(mem,iy,desloc),bb): BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->bv_clear(bv_9IY_plus_d0(mem,iy,desloc),bb)}: BV16 >-> BYTE | mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->bv_clear(bv_9IY_plus_d0(mem,iy,desloc),bb)} || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RES_b_9IX_d0)==(bb: 0..SCHAR_LENGTH-1 & desloc: SCHAR & BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & bv_clear(bv_9IY_plus_d0(mem,ix,desloc),bb): BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->bv_clear(bv_9IY_plus_d0(mem,ix,desloc),bb)}: BV16 >-> BYTE | mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->bv_clear(bv_9IY_plus_d0(mem,ix,desloc),bb)} || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RES_b_9HL0)==(bb: 0..SCHAR_LENGTH-1 & BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & bv_clear(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),bb): BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->bv_clear(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),bb)}: BV16 >-> BYTE | mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->bv_clear(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),bb)} || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RES_b_r)==(bb: 0..SCHAR_LENGTH-1 & rr: id_reg_8 | rgs8,pc:=rgs8<+{rr|->bv_clear(rgs8(rr),bb)},INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SET_b_9IY_d0)==(bb: 0..SCHAR_LENGTH-1 & desloc: SCHAR & BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & bv_set(bv_9IY_plus_d0(mem,iy,desloc),bb): BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->bv_set(bv_9IY_plus_d0(mem,iy,desloc),bb)}: BV16 >-> BYTE | mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->bv_set(bv_9IY_plus_d0(mem,iy,desloc),bb)} || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SET_b_9IX_d0)==(bb: 0..SCHAR_LENGTH-1 & desloc: SCHAR & BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & bv_set(bv_9IY_plus_d0(mem,ix,desloc),bb): BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->bv_set(bv_9IY_plus_d0(mem,ix,desloc),bb)}: BV16 >-> BYTE | mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->bv_set(bv_9IY_plus_d0(mem,ix,desloc),bb)} || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SET_b_9HL0)==(bb: 0..SCHAR_LENGTH-1 & BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & bv_set(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),bb): BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->bv_set(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),bb)}: BV16 >-> BYTE | mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->bv_set(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),bb)} || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SET_b_r)==(bb: 0..SCHAR_LENGTH-1 & rr: id_reg_8 | rgs8,pc:=rgs8<+{rr|->bitset(rgs8(rr),bb)},INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),BIT_b_9IY_d0)==(bb: 0..SCHAR_LENGTH-1 & desloc: SCHAR | rgs8,pc:=update_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bit_not(bv_9IY_plus_d0(mem,iy,desloc)(bb)),1,bitget(rgs8(f0),2),0,bitget(rgs8(f0),0)),INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),BIT_b_9IX_d0)==(bb: 0..SCHAR_LENGTH-1 & desloc: SCHAR | rgs8,pc:=update_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bit_not(bv_9IX_plus_d0(mem,ix,desloc)(bb)),1,bitget(rgs8(f0),2),0,bitget(rgs8(f0),0)),INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),BIT_b_9HL0)==(bb: 0..SCHAR_LENGTH-1 | rgs8,pc:=update_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bit_not(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)))(bb)),1,bitget(rgs8(f0),2),0,bitget(rgs8(f0),0)),INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),BIT_b_rr)==(bb: 0..SCHAR_LENGTH-1 & rr: id_reg_8 | rgs8,pc:=update_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bit_not(bitget(rgs8(rr),bb)),1,bitget(rgs8(f0),2),0,bitget(rgs8(f0),0)),INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RRD)==(btrue | @(res,acc).(res: BYTE & acc: BYTE & res = {7|->bitget(rgs8(a0),3),6|->bitget(rgs8(a0),2),5|->bitget(rgs8(a0),1),4|->bitget(rgs8(a0),0),3|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),7),2|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),6),1|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),5),0|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),4)} & acc = rgs8(a0)<+{3|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),3),2|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),2),1|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),1),0|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),0)} ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(acc),is_zero(acc),0,parity_even_BYTE(acc),0,bitget(rgs8(f0),0))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RLD)==(btrue | @(res,acc).(res: BYTE & acc: BYTE & res = {7|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),3),6|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),2),5|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),1),4|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),0),3|->bitget(rgs8(a0),3),2|->bitget(rgs8(a0),2),1|->bitget(rgs8(a0),1),0|->bitget(rgs8(a0),0)} & acc = rgs8(a0)<+{3|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),7),2|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),6),1|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),5),0|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),4)} ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(acc),is_zero(acc),0,parity_even_BYTE(acc),0,bitget(rgs8(f0),0))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SRL_9IY_d9)==(desloc: SCHAR | @res.(res: BYTE & res = bitclear(rotateright(bv_9IY_plus_d0(mem,iy,desloc)),7) ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,0,is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IY_plus_d0(mem,iy,desloc),0))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SRL_9IX_d9)==(desloc: SCHAR | @res.(res: BYTE & res = bitclear(rotateright(bv_9IX_plus_d0(mem,ix,desloc)),7) ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,0,is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IX_plus_d0(mem,ix,desloc),0))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SRL_9HL0)==(btrue | @res.(res: BYTE & res = bitclear(rotateright(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)))),7) ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,0,is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),0))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SRL_r)==(rr: id_reg_8 | @res.(res: BYTE & res = bitclear(rotateright(rgs8(rr)),7) ==> rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,0,is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),0))}));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SRA_9IY_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateright(bv_9IY_plus_d0(mem,iy,desloc))<+{7|->bv_9IY_plus_d0(mem,iy,desloc)(7)} ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IY_plus_d0(mem,iy,desloc),0))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SRA_9IX_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateright(bv_9IX_plus_d0(mem,ix,desloc))<+{7|->bitget(bv_9IX_plus_d0(mem,ix,desloc),7)} ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IX_plus_d0(mem,ix,desloc),0))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SRA_9HL0)==(btrue | @res.(res: BYTE & res = rotateright(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))))<+{7|->mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)))(7)} ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),0))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SRA_r)==(rr: id_reg_8 | @res.(res: BYTE & res = rotateright(rgs8(rr))<+{7|->bitget(rgs8(rr),7)} ==> rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),0))}));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SLA_9IY_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateright(bv_9IY_plus_d0(mem,iy,desloc))<+{0|->bitget(rgs8(f0),0)} ==> (bv_IY_plus_d(iy,desloc): BV16 & res: BYTE & mem<+{bv_IY_plus_d(iy,desloc)|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IY_plus_d0(mem,iy,desloc),7))} || mem:=mem<+{bv_IY_plus_d(iy,desloc)|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SLA_9IX_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateright(bv_9IX_plus_d0(mem,ix,desloc))<+{0|->bitget(rgs8(f0),0)} ==> (bv_IX_plus_d(ix,desloc): BV16 & res: BYTE & mem<+{bv_IX_plus_d(ix,desloc)|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IX_plus_d0(mem,ix,desloc),7))} || mem:=mem<+{bv_IX_plus_d(ix,desloc)|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SLA_9HL)==(btrue | @res.(res: BYTE & res = rotateright(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))))<+{0|->bitget(rgs8(f0),0)} ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),7))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),SLA_r)==(rr: id_reg_8 | @res.(res: BYTE & res = rotateright(rgs8(rr))<+{0|->bitget(rgs8(f0),0)} ==> rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),7))}));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RR_9IY_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateright(bv_9IY_plus_d0(mem,iy,desloc))<+{0|->bitget(rgs8(f0),0)} ==> (bv_IY_plus_d(iy,desloc): BV16 & res: BYTE & mem<+{bv_IY_plus_d(iy,desloc)|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IY_plus_d0(mem,iy,desloc),0))} || mem:=mem<+{bv_IY_plus_d(iy,desloc)|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RR_9IX_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateright(bv_9IX_plus_d0(mem,ix,desloc))<+{0|->bitget(rgs8(f0),0)} ==> (bv_IX_plus_d(ix,desloc): BV16 & res: BYTE & mem<+{bv_IX_plus_d(ix,desloc)|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IX_plus_d0(mem,ix,desloc),0))} || mem:=mem<+{bv_IX_plus_d(ix,desloc)|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RR_9HL)==(btrue | @res.(res: BYTE & res = rotateright(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))))<+{0|->bitget(rgs8(f0),0)} ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),0))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RR_r)==(rr: id_reg_8 | @res.(res: BYTE & res = rotateright(rgs8(rr))<+{0|->bitget(rgs8(f0),0)} ==> rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),0))}));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RRC_9IY_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateright(bv_9IY_plus_d0(mem,iy,desloc)) ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IY_plus_d0(mem,iy,desloc),0))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RRC_9IX_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateright(bv_9IX_plus_d0(mem,ix,desloc)) ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IX_plus_d0(mem,ix,desloc),0))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RRC_9HL0)==(btrue | @res.(res: BYTE & res = rotateright(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)))) ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),0))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RRC_r)==(rr: id_reg_8 | @res.(res: BYTE & res = rotateright(rgs8(rr)) ==> rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),0))}));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RL_9IY_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateleft(bv_9IY_plus_d0(mem,iy,desloc))<+{0|->bitget(rgs8(f0),0)} ==> (bv_IY_plus_d(iy,desloc): BV16 & res: BYTE & mem<+{bv_IY_plus_d(iy,desloc)|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IY_plus_d0(mem,iy,desloc),7))} || mem:=mem<+{bv_IY_plus_d(iy,desloc)|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RL_9IX_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateleft(bv_9IX_plus_d0(mem,ix,desloc))<+{0|->bitget(rgs8(f0),0)} ==> (bv_IX_plus_d(ix,desloc): BV16 & res: BYTE & mem<+{bv_IX_plus_d(ix,desloc)|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IX_plus_d0(mem,ix,desloc),7))} || mem:=mem<+{bv_IX_plus_d(ix,desloc)|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RL_9HL)==(btrue | @res.(res: BYTE & res = rotateleft(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))))<+{0|->bitget(rgs8(f0),0)} ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),7))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RL_r)==(rr: id_reg_8 | @res.(res: BYTE & res = rotateleft(rgs8(rr))<+{0|->bitget(rgs8(f0),0)} ==> rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),7))}));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RLC_9IY_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateleft(bv_9IY_plus_d0(mem,iy,desloc)) ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IY_plus_d0(mem,iy,desloc),7))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RLC_9IX_d9)==(desloc: SCHAR | @res.(res: BYTE & res = rotateleft(bv_9IX_plus_d0(mem,ix,desloc)) ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IX_plus_d0(mem,ix,desloc),7))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RLC_9HL0)==(btrue | @res.(res: BYTE & res = rotateleft(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)))) ==> (BYTE_TO_BV16(rgs8(h0),rgs8(l0)): BV16 & res: BYTE & mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res}: BV16 >-> BYTE | rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),7))} || mem:=mem<+{BYTE_TO_BV16(rgs8(h0),rgs8(l0))|->res})));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RLC_r)==(rr: id_reg_8 | @res.(res: BYTE & res = rotateleft(rgs8(rr)) ==> rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),7))}));
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RRA)==(btrue | rgs8:=rgs8<+{a0|->(rotateright(rgs8(a0))<+{0|->bitget(rgs8(f0),0)}),get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bitget(rgs8(f0),2),0,bitget(rgs8(a0),0))});
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RRCA)==(btrue | rgs8:=rgs8<+{a0|->rotateright(rgs8(a0)),get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bitget(rgs8(f0),2),0,bitget(rgs8(a0),0))});
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RLA)==(btrue | rgs8:=rgs8<+{a0|->(rotateleft(rgs8(a0))<+{0|->bitget(rgs8(f0),0)}),get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bitget(rgs8(f0),2),0,bitget(rgs8(a0),7))});
  Expanded_List_Substitution(Machine(Z80Man_bitControl),RLCA)==(btrue | rgs8:=rgs8<+{a0|->rotateleft(rgs8(a0)),get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bitget(rgs8(f0),2),0,bitget(rgs8(a0),7))});
  List_Substitution(Machine(Z80Man_bitControl),RLCA)==(rgs8:=rgs8<+{a0|->rotateleft(rgs8(a0)),get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bitget(rgs8(f0),2),0,bitget(rgs8(a0),7))});
  List_Substitution(Machine(Z80Man_bitControl),RLA)==(rgs8:=rgs8<+{a0|->(rotateleft(rgs8(a0))<+{0|->bitget(rgs8(f0),0)}),get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bitget(rgs8(f0),2),0,bitget(rgs8(a0),7))});
  List_Substitution(Machine(Z80Man_bitControl),RRCA)==(rgs8:=rgs8<+{a0|->rotateright(rgs8(a0)),get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bitget(rgs8(f0),2),0,bitget(rgs8(a0),0))});
  List_Substitution(Machine(Z80Man_bitControl),RRA)==(rgs8:=rgs8<+{a0|->(rotateright(rgs8(a0))<+{0|->bitget(rgs8(f0),0)}),get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bitget(rgs8(f0),2),0,bitget(rgs8(a0),0))});
  List_Substitution(Machine(Z80Man_bitControl),RLC_r)==(ANY res WHERE res: BYTE & res = rotateleft(rgs8(rr)) THEN rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),7))} END);
  List_Substitution(Machine(Z80Man_bitControl),RLC_9HL0)==(ANY res WHERE res: BYTE & res = rotateleft(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)))) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),7))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),RLC_9IX_d9)==(ANY res WHERE res: BYTE & res = rotateleft(bv_9IX_plus_d0(mem,ix,desloc)) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IX_plus_d0(mem,ix,desloc),7))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),RLC_9IY_d9)==(ANY res WHERE res: BYTE & res = rotateleft(bv_9IY_plus_d0(mem,iy,desloc)) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IY_plus_d0(mem,iy,desloc),7))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),RL_r)==(ANY res WHERE res: BYTE & res = rotateleft(rgs8(rr))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),7))} END);
  List_Substitution(Machine(Z80Man_bitControl),RL_9HL)==(ANY res WHERE res: BYTE & res = rotateleft(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),7))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),RL_9IX_d9)==(ANY res WHERE res: BYTE & res = rotateleft(bv_9IX_plus_d0(mem,ix,desloc))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IX_plus_d0(mem,ix,desloc),7))} || updateAddressMem(bv_IX_plus_d(ix,desloc),res) END);
  List_Substitution(Machine(Z80Man_bitControl),RL_9IY_d9)==(ANY res WHERE res: BYTE & res = rotateleft(bv_9IY_plus_d0(mem,iy,desloc))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IY_plus_d0(mem,iy,desloc),7))} || updateAddressMem(bv_IY_plus_d(iy,desloc),res) END);
  List_Substitution(Machine(Z80Man_bitControl),RRC_r)==(ANY res WHERE res: BYTE & res = rotateright(rgs8(rr)) THEN rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),0))} END);
  List_Substitution(Machine(Z80Man_bitControl),RRC_9HL0)==(ANY res WHERE res: BYTE & res = rotateright(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)))) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),0))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),RRC_9IX_d9)==(ANY res WHERE res: BYTE & res = rotateright(bv_9IX_plus_d0(mem,ix,desloc)) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IX_plus_d0(mem,ix,desloc),0))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),RRC_9IY_d9)==(ANY res WHERE res: BYTE & res = rotateright(bv_9IY_plus_d0(mem,iy,desloc)) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IY_plus_d0(mem,iy,desloc),0))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),RR_r)==(ANY res WHERE res: BYTE & res = rotateright(rgs8(rr))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),0))} END);
  List_Substitution(Machine(Z80Man_bitControl),RR_9HL)==(ANY res WHERE res: BYTE & res = rotateright(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),0))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),RR_9IX_d9)==(ANY res WHERE res: BYTE & res = rotateright(bv_9IX_plus_d0(mem,ix,desloc))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IX_plus_d0(mem,ix,desloc),0))} || updateAddressMem(bv_IX_plus_d(ix,desloc),res) END);
  List_Substitution(Machine(Z80Man_bitControl),RR_9IY_d9)==(ANY res WHERE res: BYTE & res = rotateright(bv_9IY_plus_d0(mem,iy,desloc))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IY_plus_d0(mem,iy,desloc),0))} || updateAddressMem(bv_IY_plus_d(iy,desloc),res) END);
  List_Substitution(Machine(Z80Man_bitControl),SLA_r)==(ANY res WHERE res: BYTE & res = rotateright(rgs8(rr))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),7))} END);
  List_Substitution(Machine(Z80Man_bitControl),SLA_9HL)==(ANY res WHERE res: BYTE & res = rotateright(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),7))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),SLA_9IX_d9)==(ANY res WHERE res: BYTE & res = rotateright(bv_9IX_plus_d0(mem,ix,desloc))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IX_plus_d0(mem,ix,desloc),7))} || updateAddressMem(bv_IX_plus_d(ix,desloc),res) END);
  List_Substitution(Machine(Z80Man_bitControl),SLA_9IY_d9)==(ANY res WHERE res: BYTE & res = rotateright(bv_9IY_plus_d0(mem,iy,desloc))<+{0|->bitget(rgs8(f0),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IY_plus_d0(mem,iy,desloc),7))} || updateAddressMem(bv_IY_plus_d(iy,desloc),res) END);
  List_Substitution(Machine(Z80Man_bitControl),SRA_r)==(ANY res WHERE res: BYTE & res = rotateright(rgs8(rr))<+{7|->bitget(rgs8(rr),7)} THEN rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),0))} END);
  List_Substitution(Machine(Z80Man_bitControl),SRA_9HL0)==(ANY res WHERE res: BYTE & res = rotateright(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))))<+{7|->mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)))(7)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),0))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),SRA_9IX_d9)==(ANY res WHERE res: BYTE & res = rotateright(bv_9IX_plus_d0(mem,ix,desloc))<+{7|->bitget(bv_9IX_plus_d0(mem,ix,desloc),7)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IX_plus_d0(mem,ix,desloc),0))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),SRA_9IY_d9)==(ANY res WHERE res: BYTE & res = rotateright(bv_9IY_plus_d0(mem,iy,desloc))<+{7|->bv_9IY_plus_d0(mem,iy,desloc)(7)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(res),is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IY_plus_d0(mem,iy,desloc),0))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),SRL_r)==(ANY res WHERE res: BYTE & res = bitclear(rotateright(rgs8(rr)),7) THEN rgs8:=rgs8<+{rr|->res,get_new_flag_register_SZ_H_PvNC(rgs8,0,is_zero(res),0,parity_even_BYTE(res),0,bitget(rgs8(rr),0))} END);
  List_Substitution(Machine(Z80Man_bitControl),SRL_9HL0)==(ANY res WHERE res: BYTE & res = bitclear(rotateright(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)))),7) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,0,is_zero(res),0,parity_even_BYTE(res),0,bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),0))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),SRL_9IX_d9)==(ANY res WHERE res: BYTE & res = bitclear(rotateright(bv_9IX_plus_d0(mem,ix,desloc)),7) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,0,is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IX_plus_d0(mem,ix,desloc),0))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),SRL_9IY_d9)==(ANY res WHERE res: BYTE & res = bitclear(rotateright(bv_9IY_plus_d0(mem,iy,desloc)),7) THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,0,is_zero(res),0,parity_even_BYTE(res),0,bitget(bv_9IY_plus_d0(mem,iy,desloc),0))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),RLD)==(ANY res,acc WHERE res: BYTE & acc: BYTE & res = {7|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),3),6|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),2),5|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),1),4|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),0),3|->bitget(rgs8(a0),3),2|->bitget(rgs8(a0),2),1|->bitget(rgs8(a0),1),0|->bitget(rgs8(a0),0)} & acc = rgs8(a0)<+{3|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),7),2|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),6),1|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),5),0|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),4)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(acc),is_zero(acc),0,parity_even_BYTE(acc),0,bitget(rgs8(f0),0))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),RRD)==(ANY res,acc WHERE res: BYTE & acc: BYTE & res = {7|->bitget(rgs8(a0),3),6|->bitget(rgs8(a0),2),5|->bitget(rgs8(a0),1),4|->bitget(rgs8(a0),0),3|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),7),2|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),6),1|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),5),0|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),4)} & acc = rgs8(a0)<+{3|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),3),2|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),2),1|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),1),0|->bitget(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),0)} THEN rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(acc),is_zero(acc),0,parity_even_BYTE(acc),0,bitget(rgs8(f0),0))} || updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),res) END);
  List_Substitution(Machine(Z80Man_bitControl),BIT_b_rr)==(rgs8:=update_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bit_not(bitget(rgs8(rr),bb)),1,bitget(rgs8(f0),2),0,bitget(rgs8(f0),0)) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80Man_bitControl),BIT_b_9HL0)==(rgs8:=update_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bit_not(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)))(bb)),1,bitget(rgs8(f0),2),0,bitget(rgs8(f0),0)) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80Man_bitControl),BIT_b_9IX_d0)==(rgs8:=update_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bit_not(bv_9IX_plus_d0(mem,ix,desloc)(bb)),1,bitget(rgs8(f0),2),0,bitget(rgs8(f0),0)) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80Man_bitControl),BIT_b_9IY_d0)==(rgs8:=update_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bit_not(bv_9IY_plus_d0(mem,iy,desloc)(bb)),1,bitget(rgs8(f0),2),0,bitget(rgs8(f0),0)) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80Man_bitControl),SET_b_r)==(rgs8(rr):=bitset(rgs8(rr),bb) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80Man_bitControl),SET_b_9HL0)==(updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),bv_set(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),bb)) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80Man_bitControl),SET_b_9IX_d0)==(updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),bv_set(bv_9IY_plus_d0(mem,ix,desloc),bb)) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80Man_bitControl),SET_b_9IY_d0)==(updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),bv_set(bv_9IY_plus_d0(mem,iy,desloc),bb)) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80Man_bitControl),RES_b_r)==(rgs8(rr):=bv_clear(rgs8(rr),bb) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80Man_bitControl),RES_b_9HL0)==(updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),bv_clear(mem(BYTE_TO_BV16(rgs8(h0),rgs8(l0))),bb)) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80Man_bitControl),RES_b_9IX_d0)==(updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),bv_clear(bv_9IY_plus_d0(mem,ix,desloc),bb)) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80Man_bitControl),RES_b_9IY_d0)==(updateAddressMem(BYTE_TO_BV16(rgs8(h0),rgs8(l0)),bv_clear(bv_9IY_plus_d0(mem,iy,desloc),bb)) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80Man_bitControl),JP_nn)==(pc:=nn);
  List_Substitution(Machine(Z80Man_bitControl),JP_cc_nn)==(IF cc = TRUE THEN pc:=nn ELSE pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80Man_bitControl),JR_e)==(pc:=(pc+ee) mod 2**16);
  List_Substitution(Machine(Z80Man_bitControl),JR_C_e)==(IF bitget(rgs8(f0),0) = 0 THEN pc:=INSTRUCTION_NEXT(pc) ELSE pc:=(pc+ee) mod 2**16 END);
  List_Substitution(Machine(Z80Man_bitControl),JR_NC_e)==(IF bitget(rgs8(f0),0) = 1 THEN pc:=INSTRUCTION_NEXT(pc) ELSE pc:=(pc+ee) mod 2**16 END);
  List_Substitution(Machine(Z80Man_bitControl),JR_Z_e)==(IF bitget(rgs8(f0),6) = 0 THEN pc:=INSTRUCTION_NEXT(pc) ELSE pc:=(pc+ee) mod 2**16 END);
  List_Substitution(Machine(Z80Man_bitControl),JR_NZ_e)==(IF bitget(rgs8(f0),6) = 1 THEN pc:=INSTRUCTION_NEXT(pc) ELSE pc:=(pc+ee) mod 2**16 END);
  List_Substitution(Machine(Z80Man_bitControl),JP_HL)==(pc:=BV16_TO_USHORTINT(BYTE_TO_BV16(rgs8(h0),rgs8(l0))));
  List_Substitution(Machine(Z80Man_bitControl),JP_IX)==(pc:=BV16_TO_USHORTINT(ix));
  List_Substitution(Machine(Z80Man_bitControl),JP_IY)==(pc:=BV16_TO_USHORTINT(iy));
  List_Substitution(Machine(Z80Man_bitControl),DJNZ_e)==(rgs8(b0):=dec_BYTE(rgs8(b0)) || IF is_zero(dec_BYTE(rgs8(b0))) = 1 THEN pc:=INSTRUCTION_NEXT(pc) ELSE pc:=(pc+ee) mod 2**16 END);
  List_Substitution(Machine(Z80Man_bitControl),CALL_nn)==(updateAddressStack(sp,USHORTINT_TO_BV16(INSTRUCTION_NEXT(pc))) || sp:=inc_BV16(sp) || pc:=nn)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Z80Man_bitControl))==(get_bv_reg16,REG16_TO_REG8,REG8_TO_REG16,update_flag_register_SZ_H_PvNC,get_new_flag_register_SZ_H_PvNC,bv_IX_plus_d,bv_IY_plus_d,bv_9IX_plus_d0,bv_9IY_plus_d0);
  Inherited_List_Constants(Machine(Z80Man_bitControl))==(?);
  List_Constants(Machine(Z80Man_bitControl))==(get_bv_reg16,REG16_TO_REG8,REG8_TO_REG16,update_flag_register_SZ_H_PvNC,get_new_flag_register_SZ_H_PvNC,bv_IX_plus_d,bv_IY_plus_d,bv_9IX_plus_d0,bv_9IY_plus_d0)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(Z80Man_bitControl),id_reg_8)==({a0,f0,f_0,a_0,b0,c0,b_0,c_0,d0,e0,d_0,e_0,h0,l0,h_0,l_0,i0,r0});
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
  Context_List_Properties(Machine(Z80Man_bitControl))==(SCHAR_LENGTH: NATURAL & SCHAR_LENGTH = 8 & NB_SCHARS: NATURAL & NB_SCHARS = 2**SCHAR_LENGTH & SCHAR = -128..127 & SCHAR_POSITION = 0..SCHAR_LENGTH-1 & UCHAR = 0..255 & SSHORTINT_LENGTH: NATURAL & SSHORTINT_LENGTH = 16 & NB_SSHORTINTS: NATURAL & NB_SSHORTINTS = 2**SSHORTINT_LENGTH & SSHORTINT = -32768..32767 & SSHORTINT_POSITION = 0..SSHORTINT_LENGTH-1 & BV16 <: BIT_VECTOR & BV16 = {vv | vv: BIT_VECTOR & size(vv) = SSHORTINT_LENGTH} & BYTE_TO_UCHAR: BYTE --> UCHAR & BYTE_TO_UCHAR = %v0.(v0: BYTE | 128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & UCHAR_TO_BYTE: UCHAR --> BYTE & UCHAR_TO_BYTE = BYTE_TO_UCHAR~ & USHORTINT = 0..65535 & NB_INSTRUCTIONS: NATURAL & INST_SZ: NATURAL & INST_SZ = 16 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = USHORTINT & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & INSTRUCTION_NEXT = %w1.(w1: USHORTINT | (w1+1) mod 65535) & INSTRUCTION_JUMP = %(p0,e0).(p0: INSTRUCTION & e0: -126..129 | (p0+e0) mod 65535) & BYTE_TO_SCHAR: BYTE --> SCHAR & BYTE_TO_SCHAR = %v0.(v0: BYTE | (-128)*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SCHAR_TO_BYTE: SCHAR --> BYTE & SCHAR_TO_BYTE = BYTE_TO_SCHAR~ & BV16_TO_SSHORTINT: BV16 --> SSHORTINT & BV16_TO_SSHORTINT = %v0.(v0: BV16 | (-32768)*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SSHORTINT_TO_BV16: SSHORTINT --> BV16 & SSHORTINT_TO_BV16 = BV16_TO_SSHORTINT~ & BYTE_TO_BV16: BYTE*BYTE --> BV16 & BYTE_TO_BV16 = %(v1,v2).(v1: BV16 & v2: BV16 | {0|->v2(0),1|->v2(1),2|->v2(2),3|->v2(3),4|->v2(4),5|->v2(5),6|->v2(6),7|->v2(7),8|->v1(0),9|->v1(1),10|->v1(2),11|->v1(3),12|->v1(4),13|->v1(5),14|->v1(6),15|->v1(7)}) & BV16_TO_BYTE: BV16 --> BYTE*BYTE & BV16_TO_BYTE = BYTE_TO_BV16~ & SCHAR_TO_SSHORTINT: SCHAR*SCHAR --> SSHORTINT & SCHAR_TO_SSHORTINT = %(w1,w2).(w1: SCHAR & w2: SCHAR | BV16_TO_SSHORTINT(BYTE_TO_BV16(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2)))) & SSHORTINT_TO_SCHAR: SSHORTINT --> SCHAR*SCHAR & SSHORTINT_TO_SCHAR = SCHAR_TO_SSHORTINT~ & BV16_TO_USHORTINT: BV16 --> USHORTINT & BV16_TO_USHORTINT = %v0.(v0: BV16 | 32768*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & USHORTINT_TO_BV16: USHORTINT --> BV16 & USHORTINT_TO_BV16 = BV16_TO_USHORTINT~ & parity_bit_from_BYTE: BIT_VECTOR --> BIT & parity_bit_from_BYTE = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | bv(idx)) mod 2) & SSHORTINT_TO_USHORTINT: SSHORTINT --> USHORTINT & SSHORTINT_TO_USHORTINT = %v0.(v0: SSHORTINT | v0-32768) & USHORTINT_TO_SSHORTINT: USHORTINT --> SSHORTINT & USHORTINT_TO_SSHORTINT = SSHORTINT_TO_USHORTINT~ & BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = {bt | bt: NATURAL +-> BIT}-{{}} & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & BV_INDX: BIT_VECTOR --> POW(NATURAL) & BV_INDX = %bv.(bv: BIT_VECTOR | 0..bv_size(bv)-1) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high | low..high<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (0..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (0..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: BV_INDX(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1(idx)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: BV_INDX(v1) & bit: BIT | v1<+{idx|->bit}) & BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE = {bt | bt: BYTE_INDEX --> BIT & size(bt) = BYTE_WIDTH}-{{}} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & is_zero: BYTE --> BIT & is_zero = %w1.(w1: BYTE | bool_to_bit(bool(w1(0)+w1(1)+w1(2)+w1(3)+w1(4)+w1(5)+w1(6)+w1(7) = 0))) & is_zeroUSHORTINT: USHORTINT --> BIT & is_zeroUSHORTINT = %nat1.(nat1: USHORTINT | bool_to_bit(bool(nat1 = 0))) & is_negative: BYTE --> BIT & is_negative = %w1.(w1: BYTE | w1(7)) & halfSCHAR: SCHAR --> UCHAR & halfSCHAR = %ww.(ww: SCHAR | (ww+128) mod 16) & add8UCHAR: UCHAR*UCHAR --> UCHAR & add8UCHAR = %(w1,w2).(w1: UCHAR & w2: UCHAR | (w1+w2) mod 256) & add8SCHAR: BIT*SCHAR*SCHAR --> SCHAR*BIT*BIT*BIT*BIT & add8SCHAR = %(carry,w1,w2).(carry: BIT & w1: SCHAR & w2: SCHAR & carry+w1+w2< -128 | 256-(carry+w1+w2),bool_to_bit(bool(carry+w1+w2<0)),1,bool_to_bit(bool(halfSCHAR(carry+w1)+halfSCHAR(w2)>=16)),0)\/%(carry,w1,w2).(carry: BIT & w1: SCHAR & w2: SCHAR & not(carry+w1+w2< -128) | (carry+w1+w2) mod 128,bool_to_bit(bool(carry+w1+w2<0)),bool_to_bit(bool(carry+w1+w2>127)),bool_to_bit(bool(halfSCHAR(w1)+halfSCHAR(w2)>=16)),bool_to_bit(bool((carry+w1+w2) mod 128 = 0))) & substract8SCHAR: BIT*SCHAR*SCHAR --> SCHAR*BIT*BIT*BIT*BIT & substract8SCHAR = %(carry,w1,w2).(carry: BIT & w1: SCHAR & w2: SCHAR & carry+w1-w2< -128 | 256-(w1-w2-carry),bool_to_bit(bool(w1-w2-carry<0)),1,bool_to_bit(bool(halfSCHAR(w1)-halfSCHAR(w2)>=16)),0)\/%(carry,w1,w2).(carry: BIT & w1: SCHAR & w2: SCHAR & not(carry+w1-w2< -128) | (w1-w2-carry) mod 128,bool_to_bit(bool(w1-w2-carry<0)),bool_to_bit(bool(w1-w2-carry>127)),bool_to_bit(bool(halfSCHAR(w1)-halfSCHAR(w2)>=16)),bool_to_bit(bool((w1-w2-carry) mod 128 = 0))) & add16USHORTINT: BIT*USHORTINT*USHORTINT --> USHORTINT & add16USHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | (b1+w1+w2) mod 65536) & add_carryUSHORTINT: BIT*USHORTINT*USHORTINT --> BIT & add_carryUSHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | bool_to_bit(bool(b1+w1+w2>2**16))) & add_halfcarryUSHORTINT: BIT*USHORTINT*USHORTINT --> BIT & add_halfcarryUSHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | bool_to_bit(bool(b1+w1 mod 2**12+w2 mod 2**12>2**12))) & sub16USHORTINT: BIT*USHORTINT*USHORTINT --> USHORTINT & sub16USHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | (w1-w2-b1) mod 65536) & sub_carryUSHORTINT: BIT*USHORTINT*USHORTINT --> BIT & sub_carryUSHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | bool_to_bit(bool(w1-w2-b1>2**16))) & sub_halfcarryUSHORTINT: BIT*USHORTINT*USHORTINT --> BIT & sub_halfcarryUSHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | bool_to_bit(bool(w1 mod 2**12-w2 mod 2**12-b1>2**12))) & inc_BYTE: BYTE --> BYTE & inc_BYTE = %w1.(w1: BYTE | UCHAR_TO_BYTE((BYTE_TO_UCHAR(w1)+1) mod 256)) & dec_BYTE: BYTE --> BYTE & dec_BYTE = %w1.(w1: BYTE | UCHAR_TO_BYTE((BYTE_TO_UCHAR(w1)-1) mod 256)) & inc_BV16: BV16 --> BV16 & inc_BV16 = %w1.(w1: BV16 | USHORTINT_TO_BV16((BV16_TO_USHORTINT(w1)+1) mod 65536)) & dec_BV16: BYTE --> BYTE & dec_BV16 = %w1.(w1: BV16 | USHORTINT_TO_BV16((BV16_TO_USHORTINT(w1)-1) mod 65536)) & parity_even_BYTE: BIT_VECTOR --> BIT & parity_even_BYTE = %bv.(bv: BIT_VECTOR | 1-SIGMA(idx).(idx: dom(bv) | bv(idx)) mod 2) & and: BYTE*BYTE --> BYTE & and = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_and(bt1,bt2)) & ior: BYTE*BYTE --> BYTE & ior = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_or(bt1,bt2)) & xor: BYTE*BYTE --> BYTE & xor = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_xor(bt1,bt2)) & bitget: BYTE*BYTE_INDEX --> BIT & bitget = %(bt1,ii).(bt1: BYTE & ii: BYTE_INDEX | bt1(ii)) & bitset: BYTE*BYTE_INDEX --> BYTE & !(ww,ii).(ww: BYTE & ii: BYTE_INDEX => bitset(ww,ii) = bv_set(ww,ii)) & bitclear: BYTE*BYTE_INDEX --> BYTE & !(ww,ii,bb).(ww: BYTE & ii: BYTE_INDEX & bb: BIT => bitclear(ww,ii) = bv_clear(ww,ii)) & complement: BYTE --> BYTE & complement = %bt.(bt: BYTE | bv_not(bt)) & swap: BYTE --> BYTE & swap = %bt.(bt: BYTE | {0|->bt(4),1|->bt(5),2|->bt(6),3|->bt(7),4|->bt(0),5|->bt(1),6|->bt(2),7|->bt(3)}) & rotateleft: BYTE --> BYTE & rotateleft = %bv.(bv: BYTE | {0|->bv(7),1|->bv(0),2|->bv(1),3|->bv(2),4|->bv(3),5|->bv(4),6|->bv(5),7|->bv(6)}) & rotateright: BYTE --> BYTE & rotateright = %bv.(bv: BYTE | {0|->bv(1),1|->bv(2),2|->bv(3),3|->bv(4),4|->bv(5),5|->bv(6),6|->bv(7),7|->bv(0)}));
  Inherited_List_Properties(Machine(Z80Man_bitControl))==(btrue);
  List_Properties(Machine(Z80Man_bitControl))==(get_bv_reg16: BV16*(id_reg_8 --> BYTE)*id_reg_16 --> BV16 & !(sp_,rgs8_,r1).(sp_: BV16 & rgs8_: id_reg_8 --> BYTE & r1: id_reg_16 => (r1 = BC => get_bv_reg16(sp_,rgs8_,r1) = BYTE_TO_BV16(rgs8_(b0),rgs8_(c0))) & (r1 = DE => get_bv_reg16(sp_,rgs8_,r1) = BYTE_TO_BV16(rgs8_(d0),rgs8_(e0))) & (r1 = HL => get_bv_reg16(sp_,rgs8_,r1) = BYTE_TO_BV16(rgs8_(h0),rgs8_(l0))) & (r1 = SP => get_bv_reg16(sp_,rgs8_,r1) = sp_) & (r1 = AF => get_bv_reg16(sp_,rgs8_,r1) = BYTE_TO_BV16(rgs8_(a0),rgs8_(f0)))) & REG16_TO_REG8: id_reg_16 --> id_reg_8*id_reg_8 & !r1.(r1: id_reg_16 => (r1 = BC => REG16_TO_REG8(r1) = b0,c0) & (r1 = DE => REG16_TO_REG8(r1) = d0,e0) & (r1 = HL => REG16_TO_REG8(r1) = h0,l0) & (r1 = AF => REG16_TO_REG8(r1) = a0,f0)) & REG8_TO_REG16 = REG16_TO_REG8~ & update_flag_register_SZ_H_PvNC = %(rgs8_,s7,z6,h4,pv2,n_add_sub,c0).(rgs8_: id_reg_8 --> BYTE & s7: BIT & z6: BIT & h4: BIT & pv2: BIT & n_add_sub: BIT & c0: BIT | rgs8_<+{f0|->{7|->s7,6|->z6,4|->h4,2|->pv2,1|->n_add_sub,0|->c0}}) & get_new_flag_register_SZ_H_PvNC = %(rgs8_,s7,z6,h4,pv2,n_add_sub,c0).(rgs8_: id_reg_8 --> BYTE & s7: BIT & z6: BIT & h4: BIT & pv2: BIT & n_add_sub: BIT & c0: BIT | f0|->{7|->s7,6|->z6,4|->h4,2|->pv2,1|->n_add_sub,0|->c0}) & bv_IX_plus_d = %(ix,desloc).(ix: BV16 & desloc: SCHAR | USHORTINT_TO_BV16(BV16_TO_USHORTINT(ix)+desloc)) & bv_IY_plus_d = %(iy,desloc).(iy: BV16 & desloc: SCHAR | USHORTINT_TO_BV16(BV16_TO_USHORTINT(iy)+desloc)) & bv_9IX_plus_d0 = %(mem,ix,desloc).(mem: BV16 --> BYTE & ix: BV16 & desloc: SCHAR | mem(USHORTINT_TO_BV16((BV16_TO_USHORTINT(ix)+desloc) mod 65536))) & bv_9IY_plus_d0 = %(mem,iy,desloc).(mem: BV16 --> BYTE & iy: BV16 & desloc: SCHAR | mem(USHORTINT_TO_BV16((BV16_TO_USHORTINT(iy)+desloc) mod 65536))) & id_reg_8: FIN(INTEGER) & not(id_reg_8 = {}) & id_reg_16: FIN(INTEGER) & not(id_reg_16 = {}))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(Z80Man_bitControl),Machine(POWER2))==(?);
  Seen_Context_List_Enumerated(Machine(Z80Man_bitControl))==(?);
  Seen_Context_List_Invariant(Machine(Z80Man_bitControl))==(btrue);
  Seen_Context_List_Assertions(Machine(Z80Man_bitControl))==((2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536) & first(BYTE_ZERO) = 0 & BYTE_ZERO: BIT_VECTOR & BYTE <: BIT_VECTOR & size(BYTE_ZERO) = 8 & !bt.(bt: BYTE => size(bt) = 8) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_not(bv_not(bv))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_and(v1,v2)(indx) = bv_and(v2,v1)(indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_and(v1,bv_and(v2,v3))(indx) = bv_and(bv_and(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_zero(bv_size(bv)))(indx) = bv_zero(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_one(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_or(v1,v2)(indx) = bv_or(v2,v1)(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_or(v1,bv_or(v2,v3))(indx) = bv_or(bv_or(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_one(bv_size(bv)))(indx) = bv_one(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_zero(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_xor(v1,v2)(indx) = bv_xor(v2,v1)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_xor(bv,bv)(indx) = bv_zero(bv_size(bv))(indx))) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT => bb = 0 or bb = 1) & bool_to_bit(FALSE) = 0 & bool_to_bit(TRUE) = 1 & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & bit_xor(1,1) = 0 & bit_xor(1,0) = 1 & bit_xor(0,1) = 1 & bit_xor(0,0) = 0 & !b1.(b1: BIT => bit_or(0,b1) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & bit_or(1,1) = 1 & bit_or(1,0) = 1 & bit_or(0,1) = 1 & bit_or(0,0) = 0 & !b1.(b1: BIT => bit_and(b1,0) = 0) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & bit_and(1,1) = 1 & bit_and(1,0) = 0 & bit_and(0,1) = 0 & bit_and(0,0) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_not(1) = 0 & bit_not(0) = 1 & !xx.(xx: USHORTINT => USHORTINT_TO_BV16(xx): BV16) & !xx.(xx: BV16 => BV16_TO_USHORTINT(xx): USHORTINT) & !(xx,yy).(xx: BYTE & yy: BYTE => #zz.(zz: BV16 & BYTE_TO_BV16(xx,yy) = zz)) & !(xx,yy).(xx: BYTE & yy: BYTE => BYTE_TO_BV16(xx,yy): BV16) & !xx.(xx: SCHAR => SCHAR_TO_BYTE(xx): BYTE) & !xx.(xx: BYTE => BYTE_TO_SCHAR(xx): SCHAR) & !xx.(xx: UCHAR => UCHAR_TO_BYTE(xx): BYTE) & !xx.(xx: BYTE => BYTE_TO_UCHAR(xx): UCHAR) & 0 = SCHAR_TO_SSHORTINT(0,0) & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & 2**16 = 65536 & !n0.(n0: SCHAR => n0<=255) & !n0.(n0: SCHAR => 0<=n0) & NB_SCHARS = 256 & !(nn,pp).(nn: NATURAL1 & pp: NAT => (pp = 0 => nn**pp = 1) & (pp = 1 => nn**pp = nn) & (pp>1 => nn**pp = nn*nn**(pp-1))));
  Seen_Context_List_Properties(Machine(Z80Man_bitControl))==(SCHAR_LENGTH: NATURAL & SCHAR_LENGTH = 8 & NB_SCHARS: NATURAL & NB_SCHARS = 2**SCHAR_LENGTH & SCHAR = -128..127 & SCHAR_POSITION = 0..SCHAR_LENGTH-1 & UCHAR = 0..255 & SSHORTINT_LENGTH: NATURAL & SSHORTINT_LENGTH = 16 & NB_SSHORTINTS: NATURAL & NB_SSHORTINTS = 2**SSHORTINT_LENGTH & SSHORTINT = -32768..32767 & SSHORTINT_POSITION = 0..SSHORTINT_LENGTH-1 & BV16 <: BIT_VECTOR & BV16 = {vv | vv: BIT_VECTOR & size(vv) = SSHORTINT_LENGTH} & BYTE_TO_UCHAR: BYTE --> UCHAR & BYTE_TO_UCHAR = %v0.(v0: BYTE | 128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & UCHAR_TO_BYTE: UCHAR --> BYTE & UCHAR_TO_BYTE = BYTE_TO_UCHAR~ & USHORTINT = 0..65535 & NB_INSTRUCTIONS: NATURAL & INST_SZ: NATURAL & INST_SZ = 16 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = USHORTINT & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & INSTRUCTION_NEXT = %w1.(w1: USHORTINT | (w1+1) mod 65535) & INSTRUCTION_JUMP = %(p0,e0).(p0: INSTRUCTION & e0: -126..129 | (p0+e0) mod 65535) & BYTE_TO_SCHAR: BYTE --> SCHAR & BYTE_TO_SCHAR = %v0.(v0: BYTE | (-128)*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SCHAR_TO_BYTE: SCHAR --> BYTE & SCHAR_TO_BYTE = BYTE_TO_SCHAR~ & BV16_TO_SSHORTINT: BV16 --> SSHORTINT & BV16_TO_SSHORTINT = %v0.(v0: BV16 | (-32768)*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SSHORTINT_TO_BV16: SSHORTINT --> BV16 & SSHORTINT_TO_BV16 = BV16_TO_SSHORTINT~ & BYTE_TO_BV16: BYTE*BYTE --> BV16 & BYTE_TO_BV16 = %(v1,v2).(v1: BV16 & v2: BV16 | {0|->v2(0),1|->v2(1),2|->v2(2),3|->v2(3),4|->v2(4),5|->v2(5),6|->v2(6),7|->v2(7),8|->v1(0),9|->v1(1),10|->v1(2),11|->v1(3),12|->v1(4),13|->v1(5),14|->v1(6),15|->v1(7)}) & BV16_TO_BYTE: BV16 --> BYTE*BYTE & BV16_TO_BYTE = BYTE_TO_BV16~ & SCHAR_TO_SSHORTINT: SCHAR*SCHAR --> SSHORTINT & SCHAR_TO_SSHORTINT = %(w1,w2).(w1: SCHAR & w2: SCHAR | BV16_TO_SSHORTINT(BYTE_TO_BV16(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2)))) & SSHORTINT_TO_SCHAR: SSHORTINT --> SCHAR*SCHAR & SSHORTINT_TO_SCHAR = SCHAR_TO_SSHORTINT~ & BV16_TO_USHORTINT: BV16 --> USHORTINT & BV16_TO_USHORTINT = %v0.(v0: BV16 | 32768*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & USHORTINT_TO_BV16: USHORTINT --> BV16 & USHORTINT_TO_BV16 = BV16_TO_USHORTINT~ & parity_bit_from_BYTE: BIT_VECTOR --> BIT & parity_bit_from_BYTE = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | bv(idx)) mod 2) & SSHORTINT_TO_USHORTINT: SSHORTINT --> USHORTINT & SSHORTINT_TO_USHORTINT = %v0.(v0: SSHORTINT | v0-32768) & USHORTINT_TO_SSHORTINT: USHORTINT --> SSHORTINT & USHORTINT_TO_SSHORTINT = SSHORTINT_TO_USHORTINT~ & BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = {bt | bt: NATURAL +-> BIT}-{{}} & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & BV_INDX: BIT_VECTOR --> POW(NATURAL) & BV_INDX = %bv.(bv: BIT_VECTOR | 0..bv_size(bv)-1) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high | low..high<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (0..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (0..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: BV_INDX(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1(idx)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: BV_INDX(v1) & bit: BIT | v1<+{idx|->bit}) & BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE = {bt | bt: BYTE_INDEX --> BIT & size(bt) = BYTE_WIDTH}-{{}} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0});
  Seen_List_Constraints(Machine(Z80Man_bitControl))==(btrue);
  Seen_List_Operations(Machine(Z80Man_bitControl),Machine(POWER2))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80Man_bitControl),Machine(POWER2))==(btrue);
  Seen_Internal_List_Operations(Machine(Z80Man_bitControl),Machine(ALU))==(?);
  Seen_List_Operations(Machine(Z80Man_bitControl),Machine(ALU))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80Man_bitControl),Machine(ALU))==(btrue);
  Seen_Internal_List_Operations(Machine(Z80Man_bitControl),Machine(TYPES))==(?);
  Seen_List_Operations(Machine(Z80Man_bitControl),Machine(TYPES))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80Man_bitControl),Machine(TYPES))==(btrue)
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
  List_ANY_Var(Machine(Z80Man_bitControl),CALL_nn)==(?);
  List_ANY_Var(Machine(Z80Man_bitControl),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Z80Man_bitControl)) == (get_bv_reg16,REG16_TO_REG8,REG8_TO_REG16,update_flag_register_SZ_H_PvNC,get_new_flag_register_SZ_H_PvNC,bv_IX_plus_d,bv_IY_plus_d,bv_9IX_plus_d0,bv_9IY_plus_d0,id_reg_8,id_reg_16,a0,f0,f_0,a_0,b0,c0,b_0,c_0,d0,e0,d_0,e_0,h0,l0,h_0,l_0,i0,r0,BC,DE,HL,SP,AF | ? | i_o_ports,iy,ix,sp,pc,rgs8 | stack,mem | RLCA,RLA,RRCA,RRA,RLC_r,RLC_9HL0,RLC_9IX_d9,RLC_9IY_d9,RL_r,RL_9HL,RL_9IX_d9,RL_9IY_d9,RRC_r,RRC_9HL0,RRC_9IX_d9,RRC_9IY_d9,RR_r,RR_9HL,RR_9IX_d9,RR_9IY_d9,SLA_r,SLA_9HL,SLA_9IX_d9,SLA_9IY_d9,SRA_r,SRA_9HL0,SRA_9IX_d9,SRA_9IY_d9,SRL_r,SRL_9HL0,SRL_9IX_d9,SRL_9IY_d9,RLD,RRD,BIT_b_rr,BIT_b_9HL0,BIT_b_9IX_d0,BIT_b_9IY_d0,SET_b_r,SET_b_9HL0,SET_b_9IX_d0,SET_b_9IY_d0,RES_b_r,RES_b_9HL0,RES_b_9IX_d0,RES_b_9IY_d0,JP_nn,JP_cc_nn,JR_e,JR_C_e,JR_NC_e,JR_Z_e,JR_NZ_e,JP_HL,JP_IX,JP_IY,DJNZ_e,CALL_nn | ? | seen(Machine(TYPES)),seen(Machine(ALU)),seen(Machine(POWER2)),included(Machine(MEMORY)) | ? | Z80Man_bitControl);
  List_Of_HiddenCst_Ids(Machine(Z80Man_bitControl)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Z80Man_bitControl)) == (get_bv_reg16,REG16_TO_REG8,REG8_TO_REG16,update_flag_register_SZ_H_PvNC,get_new_flag_register_SZ_H_PvNC,bv_IX_plus_d,bv_IY_plus_d,bv_9IX_plus_d0,bv_9IY_plus_d0);
  List_Of_VisibleVar_Ids(Machine(Z80Man_bitControl)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Z80Man_bitControl)) == (seen(Machine(TYPES)): (SCHAR,SCHAR_LENGTH,SCHAR_POSITION,NB_SCHARS,BYTE_TO_SCHAR,SCHAR_TO_BYTE,UCHAR,UCHAR_TO_BYTE,BYTE_TO_UCHAR,BV16,SSHORTINT,SSHORTINT_LENGTH,SSHORTINT_POSITION,NB_SSHORTINTS,BV16_TO_SSHORTINT,SSHORTINT_TO_BV16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,INSTRUCTION_JUMP,BYTE_TO_BV16,BV16_TO_BYTE,SCHAR_TO_SSHORTINT,SSHORTINT_TO_SCHAR,parity_bit_from_BYTE,USHORTINT,BV16_TO_USHORTINT,USHORTINT_TO_BV16,USHORTINT_TO_SSHORTINT,SSHORTINT_TO_USHORTINT | BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put,BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(ALU)): (is_zero,is_zeroUSHORTINT,is_negative,halfSCHAR,add8UCHAR,add8SCHAR,substract8SCHAR,add16USHORTINT,add_carryUSHORTINT,add_halfcarryUSHORTINT,sub16USHORTINT,sub_carryUSHORTINT,sub_halfcarryUSHORTINT,inc_BYTE,dec_BYTE,inc_BV16,dec_BV16,parity_even_BYTE,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(MEMORY)) == (? | ? | stack,mem | ? | updateMem,updateAddressMem,updateStack,updateAddressStack,pop | ? | seen(Machine(TYPES)),seen(Machine(ALU)) | ? | MEMORY);
  List_Of_HiddenCst_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(MEMORY)) == (?);
  List_Of_VisibleVar_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(MEMORY)) == (?: ?);
  List_Of_Ids(Machine(ALU)) == (is_zero,is_zeroUSHORTINT,is_negative,halfSCHAR,add8UCHAR,add8SCHAR,substract8SCHAR,add16USHORTINT,add_carryUSHORTINT,add_halfcarryUSHORTINT,sub16USHORTINT,sub_carryUSHORTINT,sub_halfcarryUSHORTINT,inc_BYTE,dec_BYTE,inc_BV16,dec_BV16,parity_even_BYTE,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright | ? | ? | ? | ? | ? | seen(Machine(TYPES)) | ? | ALU);
  List_Of_HiddenCst_Ids(Machine(ALU)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ALU)) == (is_zero,is_zeroUSHORTINT,is_negative,halfSCHAR,add8UCHAR,add8SCHAR,substract8SCHAR,add16USHORTINT,add_carryUSHORTINT,add_halfcarryUSHORTINT,sub16USHORTINT,sub_carryUSHORTINT,sub_halfcarryUSHORTINT,inc_BYTE,dec_BYTE,inc_BV16,dec_BV16,parity_even_BYTE,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright);
  List_Of_VisibleVar_Ids(Machine(ALU)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ALU)) == (?: ?);
  List_Of_Ids(Machine(TYPES)) == (SCHAR,SCHAR_LENGTH,SCHAR_POSITION,NB_SCHARS,BYTE_TO_SCHAR,SCHAR_TO_BYTE,UCHAR,UCHAR_TO_BYTE,BYTE_TO_UCHAR,BV16,SSHORTINT,SSHORTINT_LENGTH,SSHORTINT_POSITION,NB_SSHORTINTS,BV16_TO_SSHORTINT,SSHORTINT_TO_BV16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,INSTRUCTION_JUMP,BYTE_TO_BV16,BV16_TO_BYTE,SCHAR_TO_SSHORTINT,SSHORTINT_TO_SCHAR,parity_bit_from_BYTE,USHORTINT,BV16_TO_USHORTINT,USHORTINT_TO_BV16,USHORTINT_TO_SSHORTINT,SSHORTINT_TO_USHORTINT | BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put,BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | seen(Machine(POWER2)),included(Machine(BIT_DEFINITION)),included(Machine(BIT_VECTOR_DEFINITION)),included(Machine(BYTE_DEFINITION)) | ? | TYPES);
  List_Of_HiddenCst_Ids(Machine(TYPES)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (SCHAR,SCHAR_LENGTH,SCHAR_POSITION,NB_SCHARS,BYTE_TO_SCHAR,SCHAR_TO_BYTE,UCHAR,UCHAR_TO_BYTE,BYTE_TO_UCHAR,BV16,SSHORTINT,SSHORTINT_LENGTH,SSHORTINT_POSITION,NB_SSHORTINTS,BV16_TO_SSHORTINT,SSHORTINT_TO_BV16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,INSTRUCTION_JUMP,BYTE_TO_BV16,BV16_TO_BYTE,SCHAR_TO_SSHORTINT,SSHORTINT_TO_SCHAR,parity_bit_from_BYTE,USHORTINT,BV16_TO_USHORTINT,USHORTINT_TO_BV16,USHORTINT_TO_SSHORTINT,SSHORTINT_TO_USHORTINT,BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put,BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit);
  List_Of_VisibleVar_Ids(Machine(TYPES)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPES)) == (seen(Machine(BIT_DEFINITION)): (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(BIT_VECTOR_DEFINITION)): (BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)) | ? | BYTE_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO);
  List_Of_VisibleVar_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BYTE_DEFINITION)) == (?: ?);
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
THEORY SetsEnvX IS
  Sets(Machine(Z80Man_bitControl)) == (Type(id_reg_8) == Cst(SetOf(etype(id_reg_8,0,17)));Type(id_reg_16) == Cst(SetOf(etype(id_reg_16,0,4))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(Z80Man_bitControl)) == (Type(a0) == Cst(etype(id_reg_8,0,17));Type(f0) == Cst(etype(id_reg_8,0,17));Type(f_0) == Cst(etype(id_reg_8,0,17));Type(a_0) == Cst(etype(id_reg_8,0,17));Type(b0) == Cst(etype(id_reg_8,0,17));Type(c0) == Cst(etype(id_reg_8,0,17));Type(b_0) == Cst(etype(id_reg_8,0,17));Type(c_0) == Cst(etype(id_reg_8,0,17));Type(d0) == Cst(etype(id_reg_8,0,17));Type(e0) == Cst(etype(id_reg_8,0,17));Type(d_0) == Cst(etype(id_reg_8,0,17));Type(e_0) == Cst(etype(id_reg_8,0,17));Type(h0) == Cst(etype(id_reg_8,0,17));Type(l0) == Cst(etype(id_reg_8,0,17));Type(h_0) == Cst(etype(id_reg_8,0,17));Type(l_0) == Cst(etype(id_reg_8,0,17));Type(i0) == Cst(etype(id_reg_8,0,17));Type(r0) == Cst(etype(id_reg_8,0,17));Type(BC) == Cst(etype(id_reg_16,0,4));Type(DE) == Cst(etype(id_reg_16,0,4));Type(HL) == Cst(etype(id_reg_16,0,4));Type(SP) == Cst(etype(id_reg_16,0,4));Type(AF) == Cst(etype(id_reg_16,0,4));Type(get_bv_reg16) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(etype(id_reg_8,0,17)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*etype(id_reg_16,0,4)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(REG16_TO_REG8) == Cst(SetOf(etype(id_reg_16,0,4)*(etype(id_reg_8,0,17)*etype(id_reg_8,0,17))));Type(REG8_TO_REG16) == Cst(SetOf(etype(id_reg_8,?,?)*etype(id_reg_8,?,?)*etype(id_reg_16,?,?)));Type(update_flag_register_SZ_H_PvNC) == Cst(SetOf(SetOf(etype(id_reg_8,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*SetOf(etype(id_reg_8,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))));Type(get_new_flag_register_SZ_H_PvNC) == Cst(SetOf(SetOf(etype(id_reg_8,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*(etype(id_reg_8,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))));Type(bv_IX_plus_d) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_IY_plus_d) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_9IX_plus_d0) == Cst(SetOf(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_9IY_plus_d0) == Cst(SetOf(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Z80Man_bitControl)) == (Type(mem) == Mvl(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(stack) == Mvl(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(i_o_ports) == Mvl(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(iy) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(ix) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(sp) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(pc) == Mvl(btype(INTEGER,?,?));Type(rgs8) == Mvl(SetOf(etype(id_reg_8,0,17)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Z80Man_bitControl)) == (Type(CALL_nn) == Cst(No_type,btype(INTEGER,?,?));Type(DJNZ_e) == Cst(No_type,btype(INTEGER,?,?));Type(JP_IY) == Cst(No_type,No_type);Type(JP_IX) == Cst(No_type,No_type);Type(JP_HL) == Cst(No_type,No_type);Type(JR_NZ_e) == Cst(No_type,btype(INTEGER,?,?));Type(JR_Z_e) == Cst(No_type,btype(INTEGER,?,?));Type(JR_NC_e) == Cst(No_type,btype(INTEGER,?,?));Type(JR_C_e) == Cst(No_type,btype(INTEGER,?,?));Type(JR_e) == Cst(No_type,btype(INTEGER,?,?));Type(JP_cc_nn) == Cst(No_type,btype(BOOL,?,?)*btype(INTEGER,?,?));Type(JP_nn) == Cst(No_type,btype(INTEGER,?,?));Type(RES_b_9IY_d0) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RES_b_9IX_d0) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RES_b_9HL0) == Cst(No_type,btype(INTEGER,?,?));Type(RES_b_r) == Cst(No_type,btype(INTEGER,?,?)*etype(id_reg_8,?,?));Type(SET_b_9IY_d0) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SET_b_9IX_d0) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SET_b_9HL0) == Cst(No_type,btype(INTEGER,?,?));Type(SET_b_r) == Cst(No_type,btype(INTEGER,?,?)*etype(id_reg_8,?,?));Type(BIT_b_9IY_d0) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BIT_b_9IX_d0) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BIT_b_9HL0) == Cst(No_type,btype(INTEGER,?,?));Type(BIT_b_rr) == Cst(No_type,btype(INTEGER,?,?)*etype(id_reg_8,?,?));Type(RRD) == Cst(No_type,No_type);Type(RLD) == Cst(No_type,No_type);Type(SRL_9IY_d9) == Cst(No_type,btype(INTEGER,?,?));Type(SRL_9IX_d9) == Cst(No_type,btype(INTEGER,?,?));Type(SRL_9HL0) == Cst(No_type,No_type);Type(SRL_r) == Cst(No_type,etype(id_reg_8,?,?));Type(SRA_9IY_d9) == Cst(No_type,btype(INTEGER,?,?));Type(SRA_9IX_d9) == Cst(No_type,btype(INTEGER,?,?));Type(SRA_9HL0) == Cst(No_type,No_type);Type(SRA_r) == Cst(No_type,etype(id_reg_8,?,?));Type(SLA_9IY_d9) == Cst(No_type,btype(INTEGER,?,?));Type(SLA_9IX_d9) == Cst(No_type,btype(INTEGER,?,?));Type(SLA_9HL) == Cst(No_type,No_type);Type(SLA_r) == Cst(No_type,etype(id_reg_8,?,?));Type(RR_9IY_d9) == Cst(No_type,btype(INTEGER,?,?));Type(RR_9IX_d9) == Cst(No_type,btype(INTEGER,?,?));Type(RR_9HL) == Cst(No_type,No_type);Type(RR_r) == Cst(No_type,etype(id_reg_8,?,?));Type(RRC_9IY_d9) == Cst(No_type,btype(INTEGER,?,?));Type(RRC_9IX_d9) == Cst(No_type,btype(INTEGER,?,?));Type(RRC_9HL0) == Cst(No_type,No_type);Type(RRC_r) == Cst(No_type,etype(id_reg_8,?,?));Type(RL_9IY_d9) == Cst(No_type,btype(INTEGER,?,?));Type(RL_9IX_d9) == Cst(No_type,btype(INTEGER,?,?));Type(RL_9HL) == Cst(No_type,No_type);Type(RL_r) == Cst(No_type,etype(id_reg_8,?,?));Type(RLC_9IY_d9) == Cst(No_type,btype(INTEGER,?,?));Type(RLC_9IX_d9) == Cst(No_type,btype(INTEGER,?,?));Type(RLC_9HL0) == Cst(No_type,No_type);Type(RLC_r) == Cst(No_type,etype(id_reg_8,?,?));Type(RRA) == Cst(No_type,No_type);Type(RRCA) == Cst(No_type,No_type);Type(RLA) == Cst(No_type,No_type);Type(RLCA) == Cst(No_type,No_type))
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
