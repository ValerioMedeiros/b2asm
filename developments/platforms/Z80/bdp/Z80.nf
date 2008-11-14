Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Z80))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Z80))==(Machine(Z80));
  Level(Machine(Z80))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Z80)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Z80))==(TYPES,ALU,POWER2)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Z80))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Z80))==(MEMORY);
  List_Includes(Machine(Z80))==(MEMORY)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Z80))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Z80))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Z80))==(?);
  Context_List_Variables(Machine(Z80))==(?);
  Abstract_List_Variables(Machine(Z80))==(?);
  Local_List_Variables(Machine(Z80))==(i_o_ports,iy,ix,sp,pc,rgs8);
  List_Variables(Machine(Z80))==(i_o_ports,iy,ix,sp,pc,rgs8,stack,mem);
  External_List_Variables(Machine(Z80))==(i_o_ports,iy,ix,sp,pc,rgs8,stack,mem)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Z80))==(?);
  Abstract_List_VisibleVariables(Machine(Z80))==(?);
  External_List_VisibleVariables(Machine(Z80))==(?);
  Expanded_List_VisibleVariables(Machine(Z80))==(?);
  List_VisibleVariables(Machine(Z80))==(?);
  Internal_List_VisibleVariables(Machine(Z80))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Z80))==(btrue);
  Gluing_List_Invariant(Machine(Z80))==(btrue);
  Abstract_List_Invariant(Machine(Z80))==(btrue);
  Expanded_List_Invariant(Machine(Z80))==(stack: USHORTINT --> SCHAR & mem: USHORTINT --> SCHAR);
  Context_List_Invariant(Machine(Z80))==(btrue);
  List_Invariant(Machine(Z80))==(stack: USHORTINT --> SCHAR & rgs8: id_reg_8 --> SCHAR & pc: USHORTINT & sp: USHORTINT & ix: USHORTINT & iy: USHORTINT & i_o_ports: UCHAR --> SCHAR)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Machine(Z80))==(btrue);
  Expanded_List_Assertions(Machine(Z80))==(btrue);
  Context_List_Assertions(Machine(Z80))==(NB_SCHARS = 256 & !n0.(n0: SCHAR => 0<=n0) & !n0.(n0: SCHAR => n0<=255) & 2**16 = 65536 & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & 0 = SCHAR_TO_SSHORTINT(0,0) & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_to_bit(TRUE) = 1 & bool_to_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_not(bv_not(bv))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_and(v1,v2)(indx) = bv_and(v2,v1)(indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_and(v1,bv_and(v2,v3))(indx) = bv_and(bv_and(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_zero(bv_size(bv)))(indx) = bv_zero(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_one(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_or(v1,v2)(indx) = bv_or(v2,v1)(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_or(v1,bv_or(v2,v3))(indx) = bv_or(bv_or(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_one(bv_size(bv)))(indx) = bv_one(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_zero(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_xor(v1,v2)(indx) = bv_xor(v2,v1)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_xor(bv,bv)(indx) = bv_zero(bv_size(bv))(indx))) & dom(add8SCHAR) = SCHAR*SCHAR & ran(add8SCHAR): POW(SCHAR*BOOL*BOOL*BOOL*BOOL) & dom(substract8SCHAR) = SCHAR*SCHAR & ran(substract8SCHAR): POW(SCHAR*BOOL*BOOL*BOOL*BOOL) & dom(andSCHAR) = SCHAR*SCHAR & ran(andSCHAR) <: SCHAR*BOOL & dom(iorSCHAR) = SCHAR*SCHAR & ran(iorSCHAR) <: SCHAR*BOOL & dom(xorSCHAR) = SCHAR*SCHAR & ran(xorSCHAR) <: SCHAR*BOOL & dom(bitclearSCHAR) = SCHAR*SCHAR_POSITION & ran(bitclearSCHAR) <: SCHAR & dom(bitsetSCHAR) = SCHAR*SCHAR_POSITION & ran(bitsetSCHAR) <: SCHAR & dom(bitgetSCHAR) = SCHAR*SCHAR_POSITION & ran(bitgetSCHAR) <: BIT & dom(complementSCHAR) = SCHAR & ran(complementSCHAR) <: SCHAR & dom(swapSCHAR) = SCHAR & ran(swapSCHAR) <: SCHAR & ran(rotateleftSCHAR) <: SCHAR*BOOL & dom(rotateleftSCHAR) = SCHAR & dom(rotaterightSCHAR) = SCHAR & ran(rotaterightSCHAR) <: SCHAR*BOOL & !(w0,idx).(w0: SCHAR & idx: SCHAR_POSITION => bitgetSCHAR(w0,idx) = SCHAR_TO_BYTE(w0)(idx)) & max(SCHAR) = 127 & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536));
  List_Assertions(Machine(Z80))==(dom(stack) = USHORTINT & ran(stack) <: SCHAR & ran(mem) <: SCHAR & dom(mem) = USHORTINT & ran(rgs8) <: SCHAR & dom(rgs8) = id_reg_8 & INSTRUCTION_NEXT(0) = 1 & INSTRUCTION_NEXT(1) = 2 & INSTRUCTION_NEXT(2) = 3 & INSTRUCTION_NEXT(3) = 4 & INSTRUCTION_NEXT(4) = 5 & INSTRUCTION_NEXT(5) = 6 & INSTRUCTION_NEXT(6) = 7 & INSTRUCTION_NEXT(7) = 8 & INSTRUCTION_NEXT(8) = 9 & INSTRUCTION_NEXT(9) = 10 & INSTRUCTION_NEXT(10) = 11 & INSTRUCTION_NEXT(11) = 12 & INSTRUCTION_NEXT(12) = 13 & INSTRUCTION_NEXT(13) = 14 & !(vec,in0).(vec: BYTE & in0: 0..7 => bitgetSCHAR(BYTE_TO_SCHAR(vec),in0) = vec(in0)) & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0)))): SCHAR & mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0)))))): SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0)))): SCHAR & mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0)))))): SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))): SCHAR & mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))))): SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(a0),rgs8(f0)))): SCHAR & mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(a0),rgs8(f0)))))): SCHAR & mem(SSHORTINT_TO_USHORTINT(sp)): SCHAR & mem(SSHORTINT_TO_USHORTINT(ix)): SCHAR & mem(SSHORTINT_TO_USHORTINT(iy)): SCHAR)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Z80))==(@(mem$0).(mem$0: USHORTINT --> SCHAR ==> mem:=mem$0) || @(stack$0).(stack$0: USHORTINT --> SCHAR ==> stack:=stack$0);(@(rgs8$0).(rgs8$0: id_reg_8 -->> SCHAR ==> rgs8:=rgs8$0) || @(pc$0).(pc$0: USHORTINT ==> pc:=pc$0) || @(sp$0).(sp$0: SSHORTINT ==> sp:=sp$0) || @(ix$0).(ix$0: SSHORTINT ==> ix:=ix$0) || @(iy$0).(iy$0: SSHORTINT ==> iy:=iy$0) || @(i_o_ports$0).(i_o_ports$0: UCHAR --> SCHAR ==> i_o_ports:=i_o_ports$0)));
  Context_List_Initialisation(Machine(Z80))==(skip);
  List_Initialisation(Machine(Z80))==(rgs8:: id_reg_8 -->> SCHAR || pc:: USHORTINT || sp:: SSHORTINT || ix:: SSHORTINT || iy:: SSHORTINT || i_o_ports:: UCHAR --> SCHAR)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Z80))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(Z80),Machine(MEMORY))==(?);
  List_Instanciated_Parameters(Machine(Z80),Machine(TYPES))==(?);
  List_Instanciated_Parameters(Machine(Z80),Machine(ALU))==(?);
  List_Instanciated_Parameters(Machine(Z80),Machine(POWER2))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Machine(Z80),Machine(MEMORY))==(btrue);
  List_Context_Constraints(Machine(Z80))==(btrue);
  List_Constraints(Machine(Z80))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Z80))==(LD_r_r_,LD_r_n_,LD_r_9HL0,LD_r_9IX_d0,LD_r_9IY_d0,LD_9HL0_r,LD_9IX_d0_r,LD_9IY_d0_r,LD_9HL0_n,LD_9IX_d0_n,LD_9IY_d0_n,LD_A_9BC0,LD_A_9DE0,LD_A_9nn0,LD_9BC0_A,LD_9DE0_A,LD_9nn0_A,LD_dd_nn,LD_IX_nn,LD_IY_nn,LD_HL_9nn0,LD_dd_9nn0,LD_IX_9nn0,LD_IY_9nn0,LD_9nn0_HL,LD_9nn0_dd,LD_9nn0_IX,LD_9nn0_IY,LD_SP_HL,LD_SP_IX,LD_SP_IY,PUSH_qq,PUSH_IX,PUSH_IY,POP_qq,POP_IX,POP_IY,EX_DE_HL,EX_AF_AF_,EXX,EX_9SP0_HL,EX_9SP0_IX,EX_9SP0_IY,LDI,LDIR,LDD,LDDR,CPI,BIT_b_rr,BIT_b_9HL0,JR_NZ_e,JR_Z_e,SET_b_HL,RES_b_HL,GOTO,CALL,RETURN,IN_A_9n0_,IN_A_9n0,INI,INIR,IND,INDR,OUT_9n0_A,OUT_9C0_r,OUTI,OUTIR);
  List_Operations(Machine(Z80))==(LD_r_r_,LD_r_n_,LD_r_9HL0,LD_r_9IX_d0,LD_r_9IY_d0,LD_9HL0_r,LD_9IX_d0_r,LD_9IY_d0_r,LD_9HL0_n,LD_9IX_d0_n,LD_9IY_d0_n,LD_A_9BC0,LD_A_9DE0,LD_A_9nn0,LD_9BC0_A,LD_9DE0_A,LD_9nn0_A,LD_dd_nn,LD_IX_nn,LD_IY_nn,LD_HL_9nn0,LD_dd_9nn0,LD_IX_9nn0,LD_IY_9nn0,LD_9nn0_HL,LD_9nn0_dd,LD_9nn0_IX,LD_9nn0_IY,LD_SP_HL,LD_SP_IX,LD_SP_IY,PUSH_qq,PUSH_IX,PUSH_IY,POP_qq,POP_IX,POP_IY,EX_DE_HL,EX_AF_AF_,EXX,EX_9SP0_HL,EX_9SP0_IX,EX_9SP0_IY,LDI,LDIR,LDD,LDDR,CPI,BIT_b_rr,BIT_b_9HL0,JR_NZ_e,JR_Z_e,SET_b_HL,RES_b_HL,GOTO,CALL,RETURN,IN_A_9n0_,IN_A_9n0,INI,INIR,IND,INDR,OUT_9n0_A,OUT_9C0_r,OUTI,OUTIR)
END
&
THEORY ListInputX IS
  List_Input(Machine(Z80),LD_r_r_)==(rr,rr_);
  List_Input(Machine(Z80),LD_r_n_)==(rr,n0);
  List_Input(Machine(Z80),LD_r_9HL0)==(rr);
  List_Input(Machine(Z80),LD_r_9IX_d0)==(rr,dd);
  List_Input(Machine(Z80),LD_r_9IY_d0)==(rr,dd);
  List_Input(Machine(Z80),LD_9HL0_r)==(rr);
  List_Input(Machine(Z80),LD_9IX_d0_r)==(dd,rr);
  List_Input(Machine(Z80),LD_9IY_d0_r)==(dd,rr);
  List_Input(Machine(Z80),LD_9HL0_n)==(n0);
  List_Input(Machine(Z80),LD_9IX_d0_n)==(dd,n0);
  List_Input(Machine(Z80),LD_9IY_d0_n)==(dd,n0);
  List_Input(Machine(Z80),LD_A_9BC0)==(?);
  List_Input(Machine(Z80),LD_A_9DE0)==(?);
  List_Input(Machine(Z80),LD_A_9nn0)==(nn);
  List_Input(Machine(Z80),LD_9BC0_A)==(?);
  List_Input(Machine(Z80),LD_9DE0_A)==(?);
  List_Input(Machine(Z80),LD_9nn0_A)==(nn);
  List_Input(Machine(Z80),LD_dd_nn)==(dd,nn);
  List_Input(Machine(Z80),LD_IX_nn)==(nn);
  List_Input(Machine(Z80),LD_IY_nn)==(nn);
  List_Input(Machine(Z80),LD_HL_9nn0)==(nn);
  List_Input(Machine(Z80),LD_dd_9nn0)==(dd,nn);
  List_Input(Machine(Z80),LD_IX_9nn0)==(nn);
  List_Input(Machine(Z80),LD_IY_9nn0)==(nn);
  List_Input(Machine(Z80),LD_9nn0_HL)==(nn);
  List_Input(Machine(Z80),LD_9nn0_dd)==(nn,dd);
  List_Input(Machine(Z80),LD_9nn0_IX)==(nn);
  List_Input(Machine(Z80),LD_9nn0_IY)==(nn);
  List_Input(Machine(Z80),LD_SP_HL)==(?);
  List_Input(Machine(Z80),LD_SP_IX)==(?);
  List_Input(Machine(Z80),LD_SP_IY)==(?);
  List_Input(Machine(Z80),PUSH_qq)==(qq);
  List_Input(Machine(Z80),PUSH_IX)==(?);
  List_Input(Machine(Z80),PUSH_IY)==(?);
  List_Input(Machine(Z80),POP_qq)==(qq);
  List_Input(Machine(Z80),POP_IX)==(?);
  List_Input(Machine(Z80),POP_IY)==(?);
  List_Input(Machine(Z80),EX_DE_HL)==(?);
  List_Input(Machine(Z80),EX_AF_AF_)==(?);
  List_Input(Machine(Z80),EXX)==(?);
  List_Input(Machine(Z80),EX_9SP0_HL)==(?);
  List_Input(Machine(Z80),EX_9SP0_IX)==(?);
  List_Input(Machine(Z80),EX_9SP0_IY)==(?);
  List_Input(Machine(Z80),LDI)==(?);
  List_Input(Machine(Z80),LDIR)==(?);
  List_Input(Machine(Z80),LDD)==(?);
  List_Input(Machine(Z80),LDDR)==(?);
  List_Input(Machine(Z80),CPI)==(?);
  List_Input(Machine(Z80),BIT_b_rr)==(bb,rr);
  List_Input(Machine(Z80),BIT_b_9HL0)==(bb);
  List_Input(Machine(Z80),JR_NZ_e)==(ee);
  List_Input(Machine(Z80),JR_Z_e)==(ee);
  List_Input(Machine(Z80),SET_b_HL)==(bb);
  List_Input(Machine(Z80),RES_b_HL)==(bb);
  List_Input(Machine(Z80),GOTO)==(k0);
  List_Input(Machine(Z80),CALL)==(k0);
  List_Input(Machine(Z80),RETURN)==(?);
  List_Input(Machine(Z80),IN_A_9n0_)==(nn,k_in);
  List_Input(Machine(Z80),IN_A_9n0)==(nn);
  List_Input(Machine(Z80),INI)==(?);
  List_Input(Machine(Z80),INIR)==(?);
  List_Input(Machine(Z80),IND)==(?);
  List_Input(Machine(Z80),INDR)==(?);
  List_Input(Machine(Z80),OUT_9n0_A)==(nn);
  List_Input(Machine(Z80),OUT_9C0_r)==(rr);
  List_Input(Machine(Z80),OUTI)==(?);
  List_Input(Machine(Z80),OUTIR)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Z80),LD_r_r_)==(?);
  List_Output(Machine(Z80),LD_r_n_)==(?);
  List_Output(Machine(Z80),LD_r_9HL0)==(?);
  List_Output(Machine(Z80),LD_r_9IX_d0)==(?);
  List_Output(Machine(Z80),LD_r_9IY_d0)==(?);
  List_Output(Machine(Z80),LD_9HL0_r)==(?);
  List_Output(Machine(Z80),LD_9IX_d0_r)==(?);
  List_Output(Machine(Z80),LD_9IY_d0_r)==(?);
  List_Output(Machine(Z80),LD_9HL0_n)==(?);
  List_Output(Machine(Z80),LD_9IX_d0_n)==(?);
  List_Output(Machine(Z80),LD_9IY_d0_n)==(?);
  List_Output(Machine(Z80),LD_A_9BC0)==(?);
  List_Output(Machine(Z80),LD_A_9DE0)==(?);
  List_Output(Machine(Z80),LD_A_9nn0)==(?);
  List_Output(Machine(Z80),LD_9BC0_A)==(?);
  List_Output(Machine(Z80),LD_9DE0_A)==(?);
  List_Output(Machine(Z80),LD_9nn0_A)==(?);
  List_Output(Machine(Z80),LD_dd_nn)==(?);
  List_Output(Machine(Z80),LD_IX_nn)==(?);
  List_Output(Machine(Z80),LD_IY_nn)==(?);
  List_Output(Machine(Z80),LD_HL_9nn0)==(?);
  List_Output(Machine(Z80),LD_dd_9nn0)==(?);
  List_Output(Machine(Z80),LD_IX_9nn0)==(?);
  List_Output(Machine(Z80),LD_IY_9nn0)==(?);
  List_Output(Machine(Z80),LD_9nn0_HL)==(?);
  List_Output(Machine(Z80),LD_9nn0_dd)==(?);
  List_Output(Machine(Z80),LD_9nn0_IX)==(?);
  List_Output(Machine(Z80),LD_9nn0_IY)==(?);
  List_Output(Machine(Z80),LD_SP_HL)==(?);
  List_Output(Machine(Z80),LD_SP_IX)==(?);
  List_Output(Machine(Z80),LD_SP_IY)==(?);
  List_Output(Machine(Z80),PUSH_qq)==(?);
  List_Output(Machine(Z80),PUSH_IX)==(?);
  List_Output(Machine(Z80),PUSH_IY)==(?);
  List_Output(Machine(Z80),POP_qq)==(?);
  List_Output(Machine(Z80),POP_IX)==(?);
  List_Output(Machine(Z80),POP_IY)==(?);
  List_Output(Machine(Z80),EX_DE_HL)==(?);
  List_Output(Machine(Z80),EX_AF_AF_)==(?);
  List_Output(Machine(Z80),EXX)==(?);
  List_Output(Machine(Z80),EX_9SP0_HL)==(?);
  List_Output(Machine(Z80),EX_9SP0_IX)==(?);
  List_Output(Machine(Z80),EX_9SP0_IY)==(?);
  List_Output(Machine(Z80),LDI)==(?);
  List_Output(Machine(Z80),LDIR)==(?);
  List_Output(Machine(Z80),LDD)==(?);
  List_Output(Machine(Z80),LDDR)==(?);
  List_Output(Machine(Z80),CPI)==(?);
  List_Output(Machine(Z80),BIT_b_rr)==(?);
  List_Output(Machine(Z80),BIT_b_9HL0)==(?);
  List_Output(Machine(Z80),JR_NZ_e)==(?);
  List_Output(Machine(Z80),JR_Z_e)==(?);
  List_Output(Machine(Z80),SET_b_HL)==(?);
  List_Output(Machine(Z80),RES_b_HL)==(?);
  List_Output(Machine(Z80),GOTO)==(?);
  List_Output(Machine(Z80),CALL)==(?);
  List_Output(Machine(Z80),RETURN)==(?);
  List_Output(Machine(Z80),IN_A_9n0_)==(?);
  List_Output(Machine(Z80),IN_A_9n0)==(?);
  List_Output(Machine(Z80),INI)==(?);
  List_Output(Machine(Z80),INIR)==(?);
  List_Output(Machine(Z80),IND)==(?);
  List_Output(Machine(Z80),INDR)==(?);
  List_Output(Machine(Z80),OUT_9n0_A)==(?);
  List_Output(Machine(Z80),OUT_9C0_r)==(?);
  List_Output(Machine(Z80),OUTI)==(?);
  List_Output(Machine(Z80),OUTIR)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Z80),LD_r_r_)==(LD_r_r_(rr,rr_));
  List_Header(Machine(Z80),LD_r_n_)==(LD_r_n_(rr,n0));
  List_Header(Machine(Z80),LD_r_9HL0)==(LD_r_9HL0(rr));
  List_Header(Machine(Z80),LD_r_9IX_d0)==(LD_r_9IX_d0(rr,dd));
  List_Header(Machine(Z80),LD_r_9IY_d0)==(LD_r_9IY_d0(rr,dd));
  List_Header(Machine(Z80),LD_9HL0_r)==(LD_9HL0_r(rr));
  List_Header(Machine(Z80),LD_9IX_d0_r)==(LD_9IX_d0_r(dd,rr));
  List_Header(Machine(Z80),LD_9IY_d0_r)==(LD_9IY_d0_r(dd,rr));
  List_Header(Machine(Z80),LD_9HL0_n)==(LD_9HL0_n(n0));
  List_Header(Machine(Z80),LD_9IX_d0_n)==(LD_9IX_d0_n(dd,n0));
  List_Header(Machine(Z80),LD_9IY_d0_n)==(LD_9IY_d0_n(dd,n0));
  List_Header(Machine(Z80),LD_A_9BC0)==(LD_A_9BC0);
  List_Header(Machine(Z80),LD_A_9DE0)==(LD_A_9DE0);
  List_Header(Machine(Z80),LD_A_9nn0)==(LD_A_9nn0(nn));
  List_Header(Machine(Z80),LD_9BC0_A)==(LD_9BC0_A);
  List_Header(Machine(Z80),LD_9DE0_A)==(LD_9DE0_A);
  List_Header(Machine(Z80),LD_9nn0_A)==(LD_9nn0_A(nn));
  List_Header(Machine(Z80),LD_dd_nn)==(LD_dd_nn(dd,nn));
  List_Header(Machine(Z80),LD_IX_nn)==(LD_IX_nn(nn));
  List_Header(Machine(Z80),LD_IY_nn)==(LD_IY_nn(nn));
  List_Header(Machine(Z80),LD_HL_9nn0)==(LD_HL_9nn0(nn));
  List_Header(Machine(Z80),LD_dd_9nn0)==(LD_dd_9nn0(dd,nn));
  List_Header(Machine(Z80),LD_IX_9nn0)==(LD_IX_9nn0(nn));
  List_Header(Machine(Z80),LD_IY_9nn0)==(LD_IY_9nn0(nn));
  List_Header(Machine(Z80),LD_9nn0_HL)==(LD_9nn0_HL(nn));
  List_Header(Machine(Z80),LD_9nn0_dd)==(LD_9nn0_dd(nn,dd));
  List_Header(Machine(Z80),LD_9nn0_IX)==(LD_9nn0_IX(nn));
  List_Header(Machine(Z80),LD_9nn0_IY)==(LD_9nn0_IY(nn));
  List_Header(Machine(Z80),LD_SP_HL)==(LD_SP_HL);
  List_Header(Machine(Z80),LD_SP_IX)==(LD_SP_IX);
  List_Header(Machine(Z80),LD_SP_IY)==(LD_SP_IY);
  List_Header(Machine(Z80),PUSH_qq)==(PUSH_qq(qq));
  List_Header(Machine(Z80),PUSH_IX)==(PUSH_IX);
  List_Header(Machine(Z80),PUSH_IY)==(PUSH_IY);
  List_Header(Machine(Z80),POP_qq)==(POP_qq(qq));
  List_Header(Machine(Z80),POP_IX)==(POP_IX);
  List_Header(Machine(Z80),POP_IY)==(POP_IY);
  List_Header(Machine(Z80),EX_DE_HL)==(EX_DE_HL);
  List_Header(Machine(Z80),EX_AF_AF_)==(EX_AF_AF_);
  List_Header(Machine(Z80),EXX)==(EXX);
  List_Header(Machine(Z80),EX_9SP0_HL)==(EX_9SP0_HL);
  List_Header(Machine(Z80),EX_9SP0_IX)==(EX_9SP0_IX);
  List_Header(Machine(Z80),EX_9SP0_IY)==(EX_9SP0_IY);
  List_Header(Machine(Z80),LDI)==(LDI);
  List_Header(Machine(Z80),LDIR)==(LDIR);
  List_Header(Machine(Z80),LDD)==(LDD);
  List_Header(Machine(Z80),LDDR)==(LDDR);
  List_Header(Machine(Z80),CPI)==(CPI);
  List_Header(Machine(Z80),BIT_b_rr)==(BIT_b_rr(bb,rr));
  List_Header(Machine(Z80),BIT_b_9HL0)==(BIT_b_9HL0(bb));
  List_Header(Machine(Z80),JR_NZ_e)==(JR_NZ_e(ee));
  List_Header(Machine(Z80),JR_Z_e)==(JR_Z_e(ee));
  List_Header(Machine(Z80),SET_b_HL)==(SET_b_HL(bb));
  List_Header(Machine(Z80),RES_b_HL)==(RES_b_HL(bb));
  List_Header(Machine(Z80),GOTO)==(GOTO(k0));
  List_Header(Machine(Z80),CALL)==(CALL(k0));
  List_Header(Machine(Z80),RETURN)==(RETURN);
  List_Header(Machine(Z80),IN_A_9n0_)==(IN_A_9n0_(nn,k_in));
  List_Header(Machine(Z80),IN_A_9n0)==(IN_A_9n0(nn));
  List_Header(Machine(Z80),INI)==(INI);
  List_Header(Machine(Z80),INIR)==(INIR);
  List_Header(Machine(Z80),IND)==(IND);
  List_Header(Machine(Z80),INDR)==(INDR);
  List_Header(Machine(Z80),OUT_9n0_A)==(OUT_9n0_A(nn));
  List_Header(Machine(Z80),OUT_9C0_r)==(OUT_9C0_r(rr));
  List_Header(Machine(Z80),OUTI)==(OUTI);
  List_Header(Machine(Z80),OUTIR)==(OUTIR)
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Z80),LD_r_r_)==(rr: id_reg_8 & rr_: id_reg_8);
  List_Precondition(Machine(Z80),LD_r_n_)==(rr: id_reg_8 & n0: SCHAR);
  List_Precondition(Machine(Z80),LD_r_9HL0)==(rr: id_reg_8);
  List_Precondition(Machine(Z80),LD_r_9IX_d0)==(rr: id_reg_8 & dd: SCHAR);
  List_Precondition(Machine(Z80),LD_r_9IY_d0)==(rr: id_reg_8 & dd: SCHAR);
  List_Precondition(Machine(Z80),LD_9HL0_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80),LD_9IX_d0_r)==(dd: SCHAR & rr: id_reg_8);
  List_Precondition(Machine(Z80),LD_9IY_d0_r)==(dd: SCHAR & rr: id_reg_8);
  List_Precondition(Machine(Z80),LD_9HL0_n)==(n0: SCHAR);
  List_Precondition(Machine(Z80),LD_9IX_d0_n)==(dd: SCHAR & n0: SCHAR);
  List_Precondition(Machine(Z80),LD_9IY_d0_n)==(dd: SCHAR & n0: SCHAR);
  List_Precondition(Machine(Z80),LD_A_9BC0)==(btrue);
  List_Precondition(Machine(Z80),LD_A_9DE0)==(btrue);
  List_Precondition(Machine(Z80),LD_A_9nn0)==(nn: USHORTINT);
  List_Precondition(Machine(Z80),LD_9BC0_A)==(btrue);
  List_Precondition(Machine(Z80),LD_9DE0_A)==(btrue);
  List_Precondition(Machine(Z80),LD_9nn0_A)==(nn: USHORTINT & nn/:dom(stack));
  List_Precondition(Machine(Z80),LD_dd_nn)==(dd: id_reg_16 & nn: SSHORTINT & dd/=AF);
  List_Precondition(Machine(Z80),LD_IX_nn)==(nn: SSHORTINT);
  List_Precondition(Machine(Z80),LD_IY_nn)==(nn: SSHORTINT);
  List_Precondition(Machine(Z80),LD_HL_9nn0)==(nn: SSHORTINT & nn+1: SSHORTINT);
  List_Precondition(Machine(Z80),LD_dd_9nn0)==(dd: id_reg_16 & nn: USHORTINT & dd/=AF);
  List_Precondition(Machine(Z80),LD_IX_9nn0)==(nn: USHORTINT & nn+1: USHORTINT);
  List_Precondition(Machine(Z80),LD_IY_9nn0)==(nn: SSHORTINT & nn+1: SSHORTINT);
  List_Precondition(Machine(Z80),LD_9nn0_HL)==(nn: USHORTINT & nn+1: USHORTINT & nn/:dom(stack) & nn+1/:dom(stack));
  List_Precondition(Machine(Z80),LD_9nn0_dd)==(dd: id_reg_16 & dd/=AF & nn: USHORTINT & nn+1: USHORTINT & nn/:dom(stack) & nn+1/:dom(stack));
  List_Precondition(Machine(Z80),LD_9nn0_IX)==(nn: USHORTINT & nn+1: USHORTINT);
  List_Precondition(Machine(Z80),LD_9nn0_IY)==(nn: USHORTINT & nn+1: USHORTINT);
  List_Precondition(Machine(Z80),LD_SP_HL)==(btrue);
  List_Precondition(Machine(Z80),LD_SP_IX)==(btrue);
  List_Precondition(Machine(Z80),LD_SP_IY)==(btrue);
  List_Precondition(Machine(Z80),PUSH_qq)==(qq: id_reg_16 & qq/=SP & sp-2: USHORTINT);
  List_Precondition(Machine(Z80),PUSH_IX)==(sp-2: USHORTINT);
  List_Precondition(Machine(Z80),PUSH_IY)==(btrue);
  List_Precondition(Machine(Z80),POP_qq)==(qq: id_reg_16 & qq/=SP & sp+2: USHORTINT);
  List_Precondition(Machine(Z80),POP_IX)==(sp: USHORTINT & sp+2: USHORTINT);
  List_Precondition(Machine(Z80),POP_IY)==(sp+2: USHORTINT);
  List_Precondition(Machine(Z80),EX_DE_HL)==({d0|->rgs8(h0),e0|->rgs8(l0),h0|->rgs8(d0),l0|->rgs8(e0)}: id_reg_8 --> SCHAR);
  List_Precondition(Machine(Z80),EX_AF_AF_)==({a0|->rgs8(a_0),f0|->rgs8(f_0),a_0|->rgs8(a0),f_0|->rgs8(f0)}: id_reg_8 --> SCHAR);
  List_Precondition(Machine(Z80),EXX)==({b0|->rgs8(b_0),c0|->rgs8(c_0),d0|->rgs8(d_0),e0|->rgs8(e_0),h0|->rgs8(h_0),l0|->rgs8(l_0),b_0|->rgs8(b0),c_0|->rgs8(c0),d_0|->rgs8(d0),e_0|->rgs8(e0),h_0|->rgs8(h0),l_0|->rgs8(l0)}: id_reg_8 --> SCHAR);
  List_Precondition(Machine(Z80),EX_9SP0_HL)==({sp+1|->rgs8(h0),sp|->rgs8(l0)}: USHORTINT --> SCHAR & stack<+{sp+1|->rgs8(h0),sp|->rgs8(l0)} <: mem);
  List_Precondition(Machine(Z80),EX_9SP0_IX)==(btrue);
  List_Precondition(Machine(Z80),EX_9SP0_IY)==(btrue);
  List_Precondition(Machine(Z80),LDI)==(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1: SSHORTINT & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))+1: SSHORTINT & SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1: SSHORTINT);
  List_Precondition(Machine(Z80),LDIR)==(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1: SSHORTINT & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))+1: SSHORTINT & SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1: SSHORTINT);
  List_Precondition(Machine(Z80),LDD)==(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))-1: SSHORTINT & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))-1: SSHORTINT & SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1: SSHORTINT);
  List_Precondition(Machine(Z80),LDDR)==(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))-1: SSHORTINT & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))-1: SSHORTINT & SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1: SSHORTINT);
  List_Precondition(Machine(Z80),CPI)==(btrue);
  List_Precondition(Machine(Z80),BIT_b_rr)==(bb: 0..SCHAR_LENGTH-1 & rr: id_reg_8);
  List_Precondition(Machine(Z80),BIT_b_9HL0)==(bb: 0..SCHAR_LENGTH-1);
  List_Precondition(Machine(Z80),JR_NZ_e)==(ee: -126..129 & ee+pc: USHORTINT);
  List_Precondition(Machine(Z80),JR_Z_e)==(ee: -126..129 & ee+pc: USHORTINT);
  List_Precondition(Machine(Z80),SET_b_HL)==(bb: 0..SCHAR_LENGTH-1);
  List_Precondition(Machine(Z80),RES_b_HL)==(bb: 0..SCHAR_LENGTH-1);
  List_Precondition(Machine(Z80),GOTO)==(k0: USHORTINT);
  List_Precondition(Machine(Z80),CALL)==(k0: USHORTINT & sp+1: USHORTINT);
  List_Precondition(Machine(Z80),RETURN)==(sp>0);
  List_Precondition(Machine(Z80),IN_A_9n0_)==(nn: UCHAR & k_in: SCHAR);
  List_Precondition(Machine(Z80),IN_A_9n0)==(nn: UCHAR);
  List_Precondition(Machine(Z80),INI)==(rgs8(b0)-1: SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1: SSHORTINT);
  List_Precondition(Machine(Z80),INIR)==(rgs8(b0)-1: SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1: SSHORTINT);
  List_Precondition(Machine(Z80),IND)==(rgs8(b0)-1: SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))-1: SSHORTINT);
  List_Precondition(Machine(Z80),INDR)==(rgs8(b0)-1: SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))-1: SSHORTINT);
  List_Precondition(Machine(Z80),OUT_9n0_A)==(nn: UCHAR);
  List_Precondition(Machine(Z80),OUT_9C0_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80),OUTI)==(rgs8(b0)-1: SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1: SSHORTINT);
  List_Precondition(Machine(Z80),OUTIR)==(rgs8(b0)-1: SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1: SSHORTINT)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Z80),OUTIR)==(rgs8(b0)-1: SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1: SSHORTINT | @any(hvn,lvn).(hvn,lvn = SSHORTINT_TO_SCHAR(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1) & {h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1}: id_reg_8 --> SCHAR ==> (i_o_ports,rgs8:=i_o_ports<+{rgs8(c0)|->mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))))},rgs8<+{h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1} || (rgs8(b0)-1 = 0 ==> pc:=INSTRUCTION_NEXT(pc) [] not(rgs8(b0)-1 = 0) ==> skip))));
  Expanded_List_Substitution(Machine(Z80),OUTI)==(rgs8(b0)-1: SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1: SSHORTINT | @any(hvn,lvn).(hvn,lvn = SSHORTINT_TO_SCHAR(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1) & {h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1}: id_reg_8 --> SCHAR ==> i_o_ports,rgs8,pc:=i_o_ports<+{rgs8(c0)|->mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))))},rgs8<+{h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80),OUT_9C0_r)==(rr: id_reg_8 | i_o_ports,pc:=i_o_ports<+{rgs8(rr)|->rgs8(c0)},INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),OUT_9n0_A)==(nn: UCHAR | i_o_ports,pc:=i_o_ports<+{nn|->rgs8(a0)},INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),INDR)==(rgs8(b0)-1: SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))-1: SSHORTINT | @any(data_in,hvn,lvn).(data_in: SCHAR & hvn,lvn = SSHORTINT_TO_SCHAR(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))-1) & {h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1}: id_reg_8 --> SCHAR ==> (SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))): USHORTINT & i_o_ports(rgs8(c0)): SCHAR | i_o_ports:=i_o_ports<+{rgs8(c0)|->data_in} || mem:=mem<+{SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))))|->i_o_ports(rgs8(c0))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1} || (rgs8(b0)-1 = 0 ==> pc:=INSTRUCTION_NEXT(pc) [] not(rgs8(b0)-1 = 0) ==> skip))));
  Expanded_List_Substitution(Machine(Z80),IND)==(rgs8(b0)-1: SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))-1: SSHORTINT | @any(data_in,hvn,lvn).(data_in: SCHAR & hvn,lvn = SSHORTINT_TO_SCHAR(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))-1) & {h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1}: id_reg_8 --> SCHAR ==> (SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))): USHORTINT & i_o_ports(rgs8(c0)): SCHAR | i_o_ports:=i_o_ports<+{rgs8(c0)|->data_in} || mem:=mem<+{SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))))|->i_o_ports(rgs8(c0))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),INIR)==(rgs8(b0)-1: SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1: SSHORTINT | @any(data_in,hvn,lvn).(data_in: SCHAR & hvn,lvn = SSHORTINT_TO_SCHAR(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1) & {h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1}: id_reg_8 --> SCHAR ==> (SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))): USHORTINT & i_o_ports(rgs8(c0)): SCHAR | i_o_ports:=i_o_ports<+{rgs8(c0)|->data_in} || mem:=mem<+{SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))))|->i_o_ports(rgs8(c0))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1} || (rgs8(b0)-1 = 0 ==> pc:=INSTRUCTION_NEXT(pc) [] not(rgs8(b0)-1 = 0) ==> skip))));
  Expanded_List_Substitution(Machine(Z80),INI)==(rgs8(b0)-1: SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1: SSHORTINT | @any(data_in,hvn,lvn).(data_in: SCHAR & hvn,lvn = SSHORTINT_TO_SCHAR(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1) & {h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1}: id_reg_8 --> SCHAR ==> (SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))): USHORTINT & i_o_ports(rgs8(c0)): SCHAR | i_o_ports:=i_o_ports<+{rgs8(c0)|->data_in} || mem:=mem<+{SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))))|->i_o_ports(rgs8(c0))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),IN_A_9n0)==(nn: UCHAR | @any(data_in).(data_in: SCHAR ==> i_o_ports,rgs8,pc:=i_o_ports<+{nn|->data_in},rgs8<+{a0|->i_o_ports(nn)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80),IN_A_9n0_)==(nn: UCHAR & k_in: SCHAR | @any(data_in).(data_in: SCHAR ==> i_o_ports,rgs8,pc:=i_o_ports<+{nn|->data_in},rgs8<+{a0|->k_in},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80),RETURN)==(sp>0 & sp: USHORTINT | stack:={sp-1}<<|stack || pc:=stack(sp-1) || sp:=sp-1);
  Expanded_List_Substitution(Machine(Z80),CALL)==(k0: USHORTINT & sp+1: USHORTINT & stack(sp): USHORTINT & INSTRUCTION_NEXT(pc): SCHAR | stack:=stack<+{stack(sp)|->INSTRUCTION_NEXT(pc)} || sp:=sp+1 || pc:=k0);
  Expanded_List_Substitution(Machine(Z80),GOTO)==(k0: USHORTINT | pc:=k0);
  Expanded_List_Substitution(Machine(Z80),RES_b_HL)==(bb: 0..SCHAR_LENGTH-1 | @any(address,nw8).(address: USHORTINT & nw8: BYTE & address = SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)) & address/:dom(stack) & nw8 = bv_clear(SCHAR_TO_BYTE(mem(address)),bb) ==> (address: USHORTINT & BYTE_TO_SCHAR(nw8): SCHAR | mem:=mem<+{address|->BYTE_TO_SCHAR(nw8)} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),SET_b_HL)==(bb: 0..SCHAR_LENGTH-1 | @any(address,nw8).(address: USHORTINT & nw8: BYTE & address = SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)) & address/:dom(stack) & nw8 = bv_set(SCHAR_TO_BYTE(mem(address)),bb) ==> (address: USHORTINT & BYTE_TO_SCHAR(nw8): SCHAR | mem:=mem<+{address|->BYTE_TO_SCHAR(nw8)} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),JR_Z_e)==(ee: -126..129 & ee+pc: USHORTINT | SCHAR_TO_BYTE(rgs8(f_0))(6) = 0 ==> pc:=INSTRUCTION_NEXT(pc) [] not(SCHAR_TO_BYTE(rgs8(f_0))(6) = 0) ==> pc:=INSTRUCTION_JUMP(pc,ee));
  Expanded_List_Substitution(Machine(Z80),JR_NZ_e)==(ee: -126..129 & ee+pc: USHORTINT | SCHAR_TO_BYTE(rgs8(f_0))(6) = 1 ==> pc:=INSTRUCTION_NEXT(pc) [] not(SCHAR_TO_BYTE(rgs8(f_0))(6) = 1) ==> pc:=INSTRUCTION_JUMP(pc,ee));
  Expanded_List_Substitution(Machine(Z80),BIT_b_9HL0)==(bb: 0..SCHAR_LENGTH-1 | @any(address,nw8,ib).(address: SSHORTINT & ib: BIT & nw8: BYTE & address = SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)) & ib = SCHAR_TO_BYTE(mem(address))(bb) & nw8 = bv_set(SCHAR_TO_BYTE(rgs8(f_0)),6) ==> rgs8,pc:=rgs8<+{f_0|->BYTE_TO_SCHAR(nw8)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80),BIT_b_rr)==(bb: 0..SCHAR_LENGTH-1 & rr: id_reg_8 | @any(nw8,ib).(ib: BIT & nw8: BYTE & ib = SCHAR_TO_BYTE(rgs8(rr))(bb) & nw8 = bv_set(SCHAR_TO_BYTE(rgs8(f0)),6) ==> rgs8,pc:=rgs8<+{f_0|->BYTE_TO_SCHAR(nw8)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80),CPI)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & substract8SCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) = sum,is_negative,carry,digit_carry,zero ==> (zero = TRUE ==> rgs8:={get_new_flag_register(rgs8,carry,TRUE,bool(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1/=0),digit_carry,zero,is_negative)} [] not(zero = TRUE) ==> skip || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),LDDR)==(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))-1: SSHORTINT & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))-1: SSHORTINT & SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1: SSHORTINT | @any(hvn,lvn,dvn,evn,bvn,cvn).(hvn,lvn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))-1) & dvn,evn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))-1) & bvn,cvn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1) & {h0|->hvn,l0|->lvn,d0|->hvn,e0|->lvn,b0|->hvn,c0|->lvn}: id_reg_8 --> SCHAR ==> (SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))): USHORTINT & mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))))): SCHAR | mem:=mem<+{SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0)))))|->mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,d0|->hvn,e0|->lvn,b0|->hvn,c0|->lvn} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),LDD)==(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))-1: SSHORTINT & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))-1: SSHORTINT & SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1: SSHORTINT | @any(hvn,lvn,dvn,evn,bvn,cvn).(hvn,lvn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))-1) & dvn,evn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))-1) & bvn,cvn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1) & {h0|->hvn,l0|->lvn,d0|->hvn,e0|->lvn,b0|->hvn,c0|->lvn}: id_reg_8 --> SCHAR ==> (SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))): USHORTINT & mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))))): SCHAR | mem:=mem<+{SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0)))))|->mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,d0|->hvn,e0|->lvn,b0|->hvn,c0|->lvn} || (SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1 = 0 ==> pc:=INSTRUCTION_NEXT(pc) [] not(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1 = 0) ==> skip))));
  Expanded_List_Substitution(Machine(Z80),LDIR)==(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1: SSHORTINT & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))+1: SSHORTINT & SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1: SSHORTINT | @any(hvn,lvn,dvn,evn,bvn,cvn).(hvn,lvn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))+1) & dvn,evn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))+1) & bvn,cvn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1) & {h0|->hvn,l0|->lvn,d0|->hvn,e0|->lvn,b0|->hvn,c0|->lvn}: id_reg_8 --> SCHAR ==> (SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))): USHORTINT & mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))))): SCHAR | mem:=mem<+{SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0)))))|->mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,d0|->hvn,e0|->lvn,b0|->hvn,c0|->lvn} || (SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1 = 0 ==> pc:=INSTRUCTION_NEXT(pc) [] not(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1 = 0) ==> skip))));
  Expanded_List_Substitution(Machine(Z80),LDI)==(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1: SSHORTINT & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))+1: SSHORTINT & SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1: SSHORTINT | @any(hvn,lvn,dvn,evn,bvn,cvn).(hvn,lvn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))+1) & dvn,evn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))+1) & bvn,cvn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1) ==> (SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))): USHORTINT & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))): SCHAR | mem:=mem<+{SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0)))))|->mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,d0|->hvn,e0|->lvn,b0|->hvn,c0|->lvn} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),EX_9SP0_IY)==(btrue | @any(wh,wl).(wh: SCHAR & wl: SCHAR & SSHORTINT_TO_SCHAR(iy) = wh,wl & {sp+1|->wh,sp|->wl}: USHORTINT --> SCHAR & stack<+{sp+1|->wh,sp|->wl} <: mem ==> ({sp+1|->wh,sp|->wl}: USHORTINT --> SCHAR | iy:=SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(stack(sp+1),stack(sp))) || stack:=stack<+{sp+1|->wh,sp|->wl} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),EX_9SP0_IX)==(btrue | @any(wh,wl).(wh: SCHAR & wl: SCHAR & SSHORTINT_TO_SCHAR(ix) = wh,wl & {sp+1|->wh,sp|->wl}: USHORTINT --> SCHAR & stack<+{sp+1|->wh,sp|->wl} <: mem ==> ({sp+1|->wh,sp|->wl}: USHORTINT --> SCHAR | ix:=SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(stack(sp+1),stack(sp))) || stack:=stack<+{sp+1|->wh,sp|->wl} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),EX_9SP0_HL)==({sp+1|->rgs8(h0),sp|->rgs8(l0)}: USHORTINT --> SCHAR & stack<+{sp+1|->rgs8(h0),sp|->rgs8(l0)} <: mem & {sp+1|->rgs8(h0),sp|->rgs8(l0)}: USHORTINT --> SCHAR | rgs8:=rgs8<+{h0|->stack(sp+1),l0|->stack(sp)} || stack:=stack<+{sp+1|->rgs8(h0),sp|->rgs8(l0)} || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),EXX)==({b0|->rgs8(b_0),c0|->rgs8(c_0),d0|->rgs8(d_0),e0|->rgs8(e_0),h0|->rgs8(h_0),l0|->rgs8(l_0),b_0|->rgs8(b0),c_0|->rgs8(c0),d_0|->rgs8(d0),e_0|->rgs8(e0),h_0|->rgs8(h0),l_0|->rgs8(l0)}: id_reg_8 --> SCHAR | rgs8,pc:=rgs8<+{b0|->rgs8(b_0),c0|->rgs8(c_0),d0|->rgs8(d_0),e0|->rgs8(e_0),h0|->rgs8(h_0),l0|->rgs8(l_0),b_0|->rgs8(b0),c_0|->rgs8(c0),d_0|->rgs8(d0),e_0|->rgs8(e0),h_0|->rgs8(h0),l_0|->rgs8(l0)},INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),EX_AF_AF_)==({a0|->rgs8(a_0),f0|->rgs8(f_0),a_0|->rgs8(a0),f_0|->rgs8(f0)}: id_reg_8 --> SCHAR | rgs8,pc:=rgs8<+{a0|->rgs8(a_0),f0|->rgs8(f_0),a_0|->rgs8(a0),f_0|->rgs8(f0)},INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),EX_DE_HL)==({d0|->rgs8(h0),e0|->rgs8(l0),h0|->rgs8(d0),l0|->rgs8(e0)}: id_reg_8 --> SCHAR | rgs8,pc:=rgs8<+{d0|->rgs8(h0),e0|->rgs8(l0),h0|->rgs8(d0),l0|->rgs8(e0)},INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),POP_IY)==(sp+2: USHORTINT | @any(nw8).(nw8: SCHAR & SCHAR_TO_SSHORTINT(mem(sp+1),mem(sp+2)) = nw8 ==> iy,sp,pc:=nw8,sp+2,INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80),POP_IX)==(sp: USHORTINT & sp+2: USHORTINT | @any(nw8).(nw8: SCHAR & SCHAR_TO_SSHORTINT(mem(sp+1),mem(sp+2)) = nw8 ==> ix,sp,pc:=nw8,sp+2,INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80),POP_qq)==(qq: id_reg_16 & qq/=SP & sp+2: USHORTINT | @any(qqh,qql).(qqh: id_reg_8 & qql: id_reg_8 & REG16_TO_REG8(qq) = qqh,qql & {qql|->mem(sp+2),qqh|->mem(sp+1)}: id_reg_8 --> SCHAR ==> rgs8,sp,pc:=rgs8<+{qql|->mem(sp+2),qqh|->mem(sp+1)},sp+2,INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80),PUSH_IY)==(btrue | @any(wh,wl).(wh: SCHAR & wl: SCHAR & SSHORTINT_TO_SCHAR(iy) = wh,wl & {sp-2|->wl,sp-1|->wh}: USHORTINT --> SCHAR ==> ({sp-2|->wl,sp-1|->wh}: USHORTINT --> SCHAR | stack:=stack<+{sp-2|->wl,sp-1|->wh} || sp:=sp-2)));
  Expanded_List_Substitution(Machine(Z80),PUSH_IX)==(sp-2: USHORTINT | @any(wh,wl).(wh: SCHAR & wl: SCHAR & SSHORTINT_TO_SCHAR(ix) = wh,wl & {sp-2|->wl,sp-1|->wh}: USHORTINT --> SCHAR ==> ({sp-2|->wl,sp-1|->wh}: USHORTINT --> SCHAR | stack:=stack<+{sp-2|->wl,sp-1|->wh} || sp:=sp-2 || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),PUSH_qq)==(qq: id_reg_16 & qq/=SP & sp-2: USHORTINT | @any(qqh,qql).(qqh: id_reg_8 & qql: id_reg_8 & REG16_TO_REG8(qq) = qqh,qql & {sp-2|->rgs8(qql),sp-1|->rgs8(qqh)}: USHORTINT --> SCHAR ==> ({sp-2|->rgs8(qql),sp-1|->rgs8(qqh)}: USHORTINT --> SCHAR | stack:=stack<+{sp-2|->rgs8(qql),sp-1|->rgs8(qqh)} || sp:=sp-2 || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),LD_SP_IY)==(btrue | sp,pc:=SSHORTINT_TO_USHORTINT(iy),INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),LD_SP_IX)==(btrue | sp,pc:=SSHORTINT_TO_USHORTINT(ix),INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),LD_SP_HL)==(btrue | sp,pc:=SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))),INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),LD_9nn0_IY)==(nn: USHORTINT & nn+1: USHORTINT | @any(h_iy,l_iy).(h_iy: SCHAR & l_iy: SCHAR & h_iy,l_iy = SSHORTINT_TO_SCHAR(ix) & nn/:dom(stack) & nn+1/:dom(stack) ==> ({nn+1|->h_iy,nn|->l_iy}: USHORTINT --> SCHAR | mem:=mem<+{nn+1|->h_iy,nn|->l_iy} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),LD_9nn0_IX)==(nn: USHORTINT & nn+1: USHORTINT | @any(h_ix,l_ix).(h_ix: SCHAR & l_ix: SCHAR & h_ix,l_ix = SSHORTINT_TO_SCHAR(ix) & nn/:dom(stack) & nn+1/:dom(stack) ==> ({nn+1|->h_ix,nn|->l_ix}: USHORTINT --> SCHAR | mem:=mem<+{nn+1|->h_ix,nn|->l_ix} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),LD_9nn0_dd)==(dd: id_reg_16 & dd/=AF & nn: USHORTINT & nn+1: USHORTINT & nn/:dom(stack) & nn+1/:dom(stack) | dd = SP ==> @any(vh,vl).(vh: SCHAR & vl: SCHAR & SSHORTINT_TO_SCHAR(sp) = vh,vl & {nn+1|->vh,nn|->vl}: SSHORTINT --> SCHAR ==> ({nn+1|->vh,nn|->vl}: USHORTINT --> SCHAR | mem:=mem<+{nn+1|->vh,nn|->vl})) [] not(dd = SP) ==> @any(rh,rl,w1,w2).(rh: id_reg_8 & rl: id_reg_8 & w1: SCHAR & w2: SCHAR & rh,rl = REG16_TO_REG8(dd) & {nn+1|->rgs8(rh),nn|->rgs8(rl)}: USHORTINT --> SCHAR ==> ({nn+1|->rgs8(rh),nn|->rgs8(rl)}: USHORTINT --> SCHAR | mem:=mem<+{nn+1|->rgs8(rh),nn|->rgs8(rl)} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),LD_9nn0_HL)==(nn: USHORTINT & nn+1: USHORTINT & nn/:dom(stack) & nn+1/:dom(stack) & {nn+1|->rgs8(h0),nn|->rgs8(l0)}: USHORTINT --> SCHAR | mem:=mem<+{nn+1|->rgs8(h0),nn|->rgs8(l0)} || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),LD_IY_9nn0)==(nn: SSHORTINT & nn+1: SSHORTINT | iy,pc:=SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(mem(nn+1),mem(nn))),INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),LD_IX_9nn0)==(nn: USHORTINT & nn+1: USHORTINT | ix,pc:=SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(mem(nn+1),mem(nn))),INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),LD_dd_9nn0)==(dd: id_reg_16 & nn: USHORTINT & dd/=AF | dd = SP ==> sp:=SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(mem(nn+1),mem(nn))) [] not(dd = SP) ==> @any(rh,rl,w1,w2).(rh: id_reg_8 & rl: id_reg_8 & w1: SCHAR & w2: SCHAR & rh,rl = REG16_TO_REG8(dd) & w1 = mem(nn+1) & w2 = mem(nn) & {rh|->w1,rl|->w2}: id_reg_8 --> SCHAR ==> rgs8,pc:=rgs8<+{rh|->w1,rl|->w2},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80),LD_HL_9nn0)==(nn: SSHORTINT & nn+1: SSHORTINT | rgs8,pc:=rgs8<+{h0|->mem(nn+1),l0|->mem(nn)},INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),LD_IY_nn)==(nn: SSHORTINT | iy,pc:=nn,INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),LD_IX_nn)==(nn: SSHORTINT | ix,pc:=nn,INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),LD_dd_nn)==(dd: id_reg_16 & nn: SSHORTINT & dd/=AF | dd = SP ==> sp:=nn [] not(dd = SP) ==> @any(rh,rl,w1,w2).(rh: id_reg_8 & rl: id_reg_8 & w1: SCHAR & w2: SCHAR & rh,rl = REG16_TO_REG8(dd) & {rh|->w1,rl|->w2}: id_reg_8 --> SCHAR & SSHORTINT_TO_SCHAR(nn) = w1,w2 ==> rgs8,pc:=rgs8<+{rh|->w1,rl|->w2},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80),LD_9nn0_A)==(nn: USHORTINT & nn/:dom(stack) & nn: USHORTINT & rgs8(a0): SCHAR | mem:=mem<+{nn|->rgs8(a0)} || pc:=INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),LD_9DE0_A)==(btrue | @any(address).(address: USHORTINT & address = SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))) & address/:dom(stack) ==> (address: USHORTINT & rgs8(a0): SCHAR | mem:=mem<+{address|->rgs8(a0)} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),LD_9BC0_A)==(btrue | @any(address).(address: USHORTINT & address = SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))))) & address/:dom(stack) ==> (address: USHORTINT & rgs8(a0): SCHAR | mem:=mem<+{address|->rgs8(a0)} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),LD_A_9nn0)==(nn: USHORTINT | rgs8,pc:=rgs8<+{a0|->mem(nn)},INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),LD_A_9DE0)==(btrue | @any(address).(address: USHORTINT & address = SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))) ==> rgs8,pc:=rgs8<+{a0|->mem(address)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80),LD_A_9BC0)==(btrue | @any(address).(address: USHORTINT & address = SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))))) ==> rgs8,pc:=rgs8<+{a0|->mem(address)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80),LD_9IY_d0_n)==(dd: SCHAR & n0: SCHAR | @any(address,iszero,overflow).(address: SSHORTINT & iszero: BOOL & overflow: BOOL & address,iszero,overflow = add16SCHAR(iy,SCHAR_TO_SSHORTINT(0,dd)) ==> (address: USHORTINT & n0: SCHAR | mem:=mem<+{address|->n0} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),LD_9IX_d0_n)==(dd: SCHAR & n0: SCHAR | @any(address,iszero,overflow).(address: SSHORTINT & iszero: BOOL & overflow: BOOL & address,iszero,overflow = add16SCHAR(ix,SCHAR_TO_SSHORTINT(0,dd)) & address/:dom(stack) ==> (address: USHORTINT & n0: SCHAR | mem:=mem<+{address|->n0} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),LD_9HL0_n)==(n0: SCHAR | @any(address).(address: USHORTINT & address = mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))) & address/:dom(stack) ==> (address: USHORTINT & n0: SCHAR | mem:=mem<+{address|->n0} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),LD_9IY_d0_r)==(dd: SCHAR & rr: id_reg_8 | @any(address,iszero,overflow).(address: SSHORTINT & iszero: BOOL & overflow: BOOL & address,iszero,overflow = add16SCHAR(iy,SCHAR_TO_SSHORTINT(0,dd)) ==> (address: USHORTINT & rgs8(rr): SCHAR | mem:=mem<+{address|->rgs8(rr)} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),LD_9IX_d0_r)==(dd: SCHAR & rr: id_reg_8 | @any(address,iszero,overflow).(address: SSHORTINT & iszero: BOOL & overflow: BOOL & address,iszero,overflow = add16SCHAR(ix,SCHAR_TO_SSHORTINT(0,dd)) & address/:dom(stack) ==> (address: USHORTINT & rgs8(rr): SCHAR | mem:=mem<+{address|->rgs8(rr)} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),LD_9HL0_r)==(rr: id_reg_8 | @any(address).(address: USHORTINT & address = SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) & address/:dom(stack) ==> (address: USHORTINT & rgs8(rr): SCHAR | mem:=mem<+{address|->rgs8(rr)} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80),LD_r_9IY_d0)==(rr: id_reg_8 & dd: SCHAR | @any(address,iszero,overflow).(address: SSHORTINT & iszero: BOOL & overflow: BOOL & address,iszero,overflow = add16SCHAR(iy,SCHAR_TO_SSHORTINT(0,dd)) ==> rgs8,pc:=rgs8<+{rr|->mem(address)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80),LD_r_9IX_d0)==(rr: id_reg_8 & dd: SCHAR | @any(address,iszero,overflow).(address: SSHORTINT & iszero: BOOL & overflow: BOOL & address,iszero,overflow = add16SCHAR(ix,SCHAR_TO_SSHORTINT(0,dd)) ==> rgs8,pc:=rgs8<+{rr|->mem(address)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80),LD_r_9HL0)==(rr: id_reg_8 | @any(address).(address: SSHORTINT & address = SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)) ==> rgs8,pc:=rgs8<+{rr|->mem(address)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80),LD_r_n_)==(rr: id_reg_8 & n0: SCHAR | rgs8,pc:=rgs8<+{rr|->n0},INSTRUCTION_NEXT(pc));
  Expanded_List_Substitution(Machine(Z80),LD_r_r_)==(rr: id_reg_8 & rr_: id_reg_8 | rgs8,pc:=rgs8<+{rr|->rgs8(rr_)},INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),LD_r_r_)==(rgs8(rr):=rgs8(rr_) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),LD_r_n_)==(rgs8(rr):=n0 || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),LD_r_9HL0)==(ANY address WHERE address: SSHORTINT & address = SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)) THEN rgs8(rr):=mem(address) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LD_r_9IX_d0)==(ANY address,iszero,overflow WHERE address: SSHORTINT & iszero: BOOL & overflow: BOOL & address,iszero,overflow = add16SCHAR(ix,SCHAR_TO_SSHORTINT(0,dd)) THEN rgs8(rr):=mem(address) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LD_r_9IY_d0)==(ANY address,iszero,overflow WHERE address: SSHORTINT & iszero: BOOL & overflow: BOOL & address,iszero,overflow = add16SCHAR(iy,SCHAR_TO_SSHORTINT(0,dd)) THEN rgs8(rr):=mem(address) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LD_9HL0_r)==(ANY address WHERE address: USHORTINT & address = SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) & address/:dom(stack) THEN updateAddressMem(address,rgs8(rr)) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LD_9IX_d0_r)==(ANY address,iszero,overflow WHERE address: SSHORTINT & iszero: BOOL & overflow: BOOL & address,iszero,overflow = add16SCHAR(ix,SCHAR_TO_SSHORTINT(0,dd)) & address/:dom(stack) THEN updateAddressMem(address,rgs8(rr)) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LD_9IY_d0_r)==(ANY address,iszero,overflow WHERE address: SSHORTINT & iszero: BOOL & overflow: BOOL & address,iszero,overflow = add16SCHAR(iy,SCHAR_TO_SSHORTINT(0,dd)) THEN updateAddressMem(address,rgs8(rr)) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LD_9HL0_n)==(ANY address WHERE address: USHORTINT & address = mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))) & address/:dom(stack) THEN updateAddressMem(address,n0) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LD_9IX_d0_n)==(ANY address,iszero,overflow WHERE address: SSHORTINT & iszero: BOOL & overflow: BOOL & address,iszero,overflow = add16SCHAR(ix,SCHAR_TO_SSHORTINT(0,dd)) & address/:dom(stack) THEN updateAddressMem(address,n0) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LD_9IY_d0_n)==(ANY address,iszero,overflow WHERE address: SSHORTINT & iszero: BOOL & overflow: BOOL & address,iszero,overflow = add16SCHAR(iy,SCHAR_TO_SSHORTINT(0,dd)) THEN updateAddressMem(address,n0) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LD_A_9BC0)==(ANY address WHERE address: USHORTINT & address = SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))))) THEN rgs8(a0):=mem(address) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LD_A_9DE0)==(ANY address WHERE address: USHORTINT & address = SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))) THEN rgs8(a0):=mem(address) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LD_A_9nn0)==(rgs8(a0):=mem(nn) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),LD_9BC0_A)==(ANY address WHERE address: USHORTINT & address = SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))))) & address/:dom(stack) THEN updateAddressMem(address,rgs8(a0)) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LD_9DE0_A)==(ANY address WHERE address: USHORTINT & address = SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))) & address/:dom(stack) THEN updateAddressMem(address,rgs8(a0)) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LD_9nn0_A)==(updateAddressMem(nn,rgs8(a0)) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),LD_dd_nn)==(IF dd = SP THEN sp:=nn ELSE ANY rh,rl,w1,w2 WHERE rh: id_reg_8 & rl: id_reg_8 & w1: SCHAR & w2: SCHAR & rh,rl = REG16_TO_REG8(dd) & {rh|->w1,rl|->w2}: id_reg_8 --> SCHAR & SSHORTINT_TO_SCHAR(nn) = w1,w2 THEN rgs8:=rgs8<+{rh|->w1,rl|->w2} || pc:=INSTRUCTION_NEXT(pc) END END);
  List_Substitution(Machine(Z80),LD_IX_nn)==(ix:=nn || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),LD_IY_nn)==(iy:=nn || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),LD_HL_9nn0)==(rgs8:=rgs8<+{h0|->mem(nn+1),l0|->mem(nn)} || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),LD_dd_9nn0)==(IF dd = SP THEN sp:=SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(mem(nn+1),mem(nn))) ELSE ANY rh,rl,w1,w2 WHERE rh: id_reg_8 & rl: id_reg_8 & w1: SCHAR & w2: SCHAR & rh,rl = REG16_TO_REG8(dd) & w1 = mem(nn+1) & w2 = mem(nn) & {rh|->w1,rl|->w2}: id_reg_8 --> SCHAR THEN rgs8:=rgs8<+{rh|->w1,rl|->w2} || pc:=INSTRUCTION_NEXT(pc) END END);
  List_Substitution(Machine(Z80),LD_IX_9nn0)==(ix:=SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(mem(nn+1),mem(nn))) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),LD_IY_9nn0)==(iy:=SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(mem(nn+1),mem(nn))) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),LD_9nn0_HL)==(updateMem({nn+1|->rgs8(h0),nn|->rgs8(l0)}) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),LD_9nn0_dd)==(IF dd = SP THEN ANY vh,vl WHERE vh: SCHAR & vl: SCHAR & SSHORTINT_TO_SCHAR(sp) = vh,vl & {nn+1|->vh,nn|->vl}: SSHORTINT --> SCHAR THEN updateMem({nn+1|->vh,nn|->vl}) END ELSE ANY rh,rl,w1,w2 WHERE rh: id_reg_8 & rl: id_reg_8 & w1: SCHAR & w2: SCHAR & rh,rl = REG16_TO_REG8(dd) & {nn+1|->rgs8(rh),nn|->rgs8(rl)}: USHORTINT --> SCHAR THEN updateMem({nn+1|->rgs8(rh),nn|->rgs8(rl)}) || pc:=INSTRUCTION_NEXT(pc) END END);
  List_Substitution(Machine(Z80),LD_9nn0_IX)==(ANY h_ix,l_ix WHERE h_ix: SCHAR & l_ix: SCHAR & h_ix,l_ix = SSHORTINT_TO_SCHAR(ix) & nn/:dom(stack) & nn+1/:dom(stack) THEN updateMem({nn+1|->h_ix,nn|->l_ix}) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LD_9nn0_IY)==(ANY h_iy,l_iy WHERE h_iy: SCHAR & l_iy: SCHAR & h_iy,l_iy = SSHORTINT_TO_SCHAR(ix) & nn/:dom(stack) & nn+1/:dom(stack) THEN updateMem({nn+1|->h_iy,nn|->l_iy}) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LD_SP_HL)==(sp:=SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),LD_SP_IX)==(sp:=SSHORTINT_TO_USHORTINT(ix) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),LD_SP_IY)==(sp:=SSHORTINT_TO_USHORTINT(iy) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),PUSH_qq)==(ANY qqh,qql WHERE qqh: id_reg_8 & qql: id_reg_8 & REG16_TO_REG8(qq) = qqh,qql & {sp-2|->rgs8(qql),sp-1|->rgs8(qqh)}: USHORTINT --> SCHAR THEN updateStack({sp-2|->rgs8(qql),sp-1|->rgs8(qqh)}) || sp:=sp-2 || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),PUSH_IX)==(ANY wh,wl WHERE wh: SCHAR & wl: SCHAR & SSHORTINT_TO_SCHAR(ix) = wh,wl & {sp-2|->wl,sp-1|->wh}: USHORTINT --> SCHAR THEN updateStack({sp-2|->wl,sp-1|->wh}) || sp:=sp-2 || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),PUSH_IY)==(ANY wh,wl WHERE wh: SCHAR & wl: SCHAR & SSHORTINT_TO_SCHAR(iy) = wh,wl & {sp-2|->wl,sp-1|->wh}: USHORTINT --> SCHAR THEN updateStack({sp-2|->wl,sp-1|->wh}) || sp:=sp-2 END);
  List_Substitution(Machine(Z80),POP_qq)==(ANY qqh,qql WHERE qqh: id_reg_8 & qql: id_reg_8 & REG16_TO_REG8(qq) = qqh,qql & {qql|->mem(sp+2),qqh|->mem(sp+1)}: id_reg_8 --> SCHAR THEN rgs8:=rgs8<+{qql|->mem(sp+2),qqh|->mem(sp+1)} || sp:=sp+2 || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),POP_IX)==(ANY nw8 WHERE nw8: SCHAR & SCHAR_TO_SSHORTINT(mem(sp+1),mem(sp+2)) = nw8 THEN ix:=nw8 || sp:=sp+2 || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),POP_IY)==(ANY nw8 WHERE nw8: SCHAR & SCHAR_TO_SSHORTINT(mem(sp+1),mem(sp+2)) = nw8 THEN iy:=nw8 || sp:=sp+2 || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),EX_DE_HL)==(rgs8:=rgs8<+{d0|->rgs8(h0),e0|->rgs8(l0),h0|->rgs8(d0),l0|->rgs8(e0)} || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),EX_AF_AF_)==(rgs8:=rgs8<+{a0|->rgs8(a_0),f0|->rgs8(f_0),a_0|->rgs8(a0),f_0|->rgs8(f0)} || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),EXX)==(rgs8:=rgs8<+{b0|->rgs8(b_0),c0|->rgs8(c_0),d0|->rgs8(d_0),e0|->rgs8(e_0),h0|->rgs8(h_0),l0|->rgs8(l_0),b_0|->rgs8(b0),c_0|->rgs8(c0),d_0|->rgs8(d0),e_0|->rgs8(e0),h_0|->rgs8(h0),l_0|->rgs8(l0)} || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),EX_9SP0_HL)==(rgs8:=rgs8<+{h0|->stack(sp+1),l0|->stack(sp)} || updateStack({sp+1|->rgs8(h0),sp|->rgs8(l0)}) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),EX_9SP0_IX)==(ANY wh,wl WHERE wh: SCHAR & wl: SCHAR & SSHORTINT_TO_SCHAR(ix) = wh,wl & {sp+1|->wh,sp|->wl}: USHORTINT --> SCHAR & stack<+{sp+1|->wh,sp|->wl} <: mem THEN ix:=SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(stack(sp+1),stack(sp))) || updateStack({sp+1|->wh,sp|->wl}) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),EX_9SP0_IY)==(ANY wh,wl WHERE wh: SCHAR & wl: SCHAR & SSHORTINT_TO_SCHAR(iy) = wh,wl & {sp+1|->wh,sp|->wl}: USHORTINT --> SCHAR & stack<+{sp+1|->wh,sp|->wl} <: mem THEN iy:=SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(stack(sp+1),stack(sp))) || updateStack({sp+1|->wh,sp|->wl}) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LDI)==(ANY hvn,lvn,dvn,evn,bvn,cvn WHERE hvn,lvn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))+1) & dvn,evn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))+1) & bvn,cvn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1) THEN updateAddressMem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,d0|->hvn,e0|->lvn,b0|->hvn,c0|->lvn} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),LDIR)==(ANY hvn,lvn,dvn,evn,bvn,cvn WHERE hvn,lvn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))+1) & dvn,evn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))+1) & bvn,cvn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1) & {h0|->hvn,l0|->lvn,d0|->hvn,e0|->lvn,b0|->hvn,c0|->lvn}: id_reg_8 --> SCHAR THEN updateAddressMem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))),mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,d0|->hvn,e0|->lvn,b0|->hvn,c0|->lvn} || IF SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1 = 0 THEN pc:=INSTRUCTION_NEXT(pc) END END);
  List_Substitution(Machine(Z80),LDD)==(ANY hvn,lvn,dvn,evn,bvn,cvn WHERE hvn,lvn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))-1) & dvn,evn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))-1) & bvn,cvn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1) & {h0|->hvn,l0|->lvn,d0|->hvn,e0|->lvn,b0|->hvn,c0|->lvn}: id_reg_8 --> SCHAR THEN updateAddressMem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))),mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,d0|->hvn,e0|->lvn,b0|->hvn,c0|->lvn} || IF SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1 = 0 THEN pc:=INSTRUCTION_NEXT(pc) END END);
  List_Substitution(Machine(Z80),LDDR)==(ANY hvn,lvn,dvn,evn,bvn,cvn WHERE hvn,lvn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))-1) & dvn,evn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))-1) & bvn,cvn = SSHORTINT_TO_SCHAR(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1) & {h0|->hvn,l0|->lvn,d0|->hvn,e0|->lvn,b0|->hvn,c0|->lvn}: id_reg_8 --> SCHAR THEN updateAddressMem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))))),mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,d0|->hvn,e0|->lvn,b0|->hvn,c0|->lvn} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),CPI)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & substract8SCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) = sum,is_negative,carry,digit_carry,zero THEN IF zero = TRUE THEN rgs8:={get_new_flag_register(rgs8,carry,TRUE,bool(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))-1/=0),digit_carry,zero,is_negative)} END || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),BIT_b_rr)==(ANY nw8,ib WHERE ib: BIT & nw8: BYTE & ib = SCHAR_TO_BYTE(rgs8(rr))(bb) & nw8 = bv_set(SCHAR_TO_BYTE(rgs8(f0)),6) THEN rgs8(f_0):=BYTE_TO_SCHAR(nw8) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),BIT_b_9HL0)==(ANY address,nw8,ib WHERE address: SSHORTINT & ib: BIT & nw8: BYTE & address = SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)) & ib = SCHAR_TO_BYTE(mem(address))(bb) & nw8 = bv_set(SCHAR_TO_BYTE(rgs8(f_0)),6) THEN rgs8(f_0):=BYTE_TO_SCHAR(nw8) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),JR_NZ_e)==(IF SCHAR_TO_BYTE(rgs8(f_0))(6) = 1 THEN pc:=INSTRUCTION_NEXT(pc) ELSE pc:=INSTRUCTION_JUMP(pc,ee) END);
  List_Substitution(Machine(Z80),JR_Z_e)==(IF SCHAR_TO_BYTE(rgs8(f_0))(6) = 0 THEN pc:=INSTRUCTION_NEXT(pc) ELSE pc:=INSTRUCTION_JUMP(pc,ee) END);
  List_Substitution(Machine(Z80),SET_b_HL)==(ANY address,nw8 WHERE address: USHORTINT & nw8: BYTE & address = SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)) & address/:dom(stack) & nw8 = bv_set(SCHAR_TO_BYTE(mem(address)),bb) THEN updateAddressMem(address,BYTE_TO_SCHAR(nw8)) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),RES_b_HL)==(ANY address,nw8 WHERE address: USHORTINT & nw8: BYTE & address = SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)) & address/:dom(stack) & nw8 = bv_clear(SCHAR_TO_BYTE(mem(address)),bb) THEN updateAddressMem(address,BYTE_TO_SCHAR(nw8)) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),GOTO)==(pc:=k0);
  List_Substitution(Machine(Z80),CALL)==(updateAddressStack(stack(sp),INSTRUCTION_NEXT(pc)) || sp:=sp+1 || pc:=k0);
  List_Substitution(Machine(Z80),RETURN)==(pop(sp) || pc:=stack(sp-1) || sp:=sp-1);
  List_Substitution(Machine(Z80),IN_A_9n0_)==(ANY data_in WHERE data_in: SCHAR THEN i_o_ports(nn):=data_in || rgs8(a0):=k_in || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),IN_A_9n0)==(ANY data_in WHERE data_in: SCHAR THEN i_o_ports(nn):=data_in || rgs8(a0):=i_o_ports(nn) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),INI)==(ANY data_in,hvn,lvn WHERE data_in: SCHAR & hvn,lvn = SSHORTINT_TO_SCHAR(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1) & {h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1}: id_reg_8 --> SCHAR THEN i_o_ports(rgs8(c0)):=data_in || updateAddressMem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))),i_o_ports(rgs8(c0))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),INIR)==(ANY data_in,hvn,lvn WHERE data_in: SCHAR & hvn,lvn = SSHORTINT_TO_SCHAR(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1) & {h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1}: id_reg_8 --> SCHAR THEN i_o_ports(rgs8(c0)):=data_in || updateAddressMem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))),i_o_ports(rgs8(c0))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1} || IF rgs8(b0)-1 = 0 THEN pc:=INSTRUCTION_NEXT(pc) END END);
  List_Substitution(Machine(Z80),IND)==(ANY data_in,hvn,lvn WHERE data_in: SCHAR & hvn,lvn = SSHORTINT_TO_SCHAR(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))-1) & {h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1}: id_reg_8 --> SCHAR THEN i_o_ports(rgs8(c0)):=data_in || updateAddressMem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))),i_o_ports(rgs8(c0))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),INDR)==(ANY data_in,hvn,lvn WHERE data_in: SCHAR & hvn,lvn = SSHORTINT_TO_SCHAR(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))-1) & {h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1}: id_reg_8 --> SCHAR THEN i_o_ports(rgs8(c0)):=data_in || updateAddressMem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))),i_o_ports(rgs8(c0))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1} || IF rgs8(b0)-1 = 0 THEN pc:=INSTRUCTION_NEXT(pc) END END);
  List_Substitution(Machine(Z80),OUT_9n0_A)==(i_o_ports(nn):=rgs8(a0) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),OUT_9C0_r)==(i_o_ports(rgs8(rr)):=rgs8(c0) || pc:=INSTRUCTION_NEXT(pc));
  List_Substitution(Machine(Z80),OUTI)==(ANY hvn,lvn WHERE hvn,lvn = SSHORTINT_TO_SCHAR(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1) & {h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1}: id_reg_8 --> SCHAR THEN i_o_ports(rgs8(c0)):=mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80),OUTIR)==(ANY hvn,lvn WHERE hvn,lvn = SSHORTINT_TO_SCHAR(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+1) & {h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1}: id_reg_8 --> SCHAR THEN i_o_ports(rgs8(c0)):=mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->rgs8(b0)-1} || IF rgs8(b0)-1 = 0 THEN pc:=INSTRUCTION_NEXT(pc) END END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Z80))==(REG16_TO_REG8,REG8_TO_REG16,update_flag_register,get_new_flag_register);
  Inherited_List_Constants(Machine(Z80))==(?);
  List_Constants(Machine(Z80))==(REG16_TO_REG8,REG8_TO_REG16,update_flag_register,get_new_flag_register)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(Z80),id_reg_8)==({a0,f0,f_0,a_0,b0,c0,b_0,c_0,d0,e0,d_0,e_0,h0,l0,h_0,l_0,i0,r0});
  Context_List_Enumerated(Machine(Z80))==(?);
  Context_List_Defered(Machine(Z80))==(?);
  Context_List_Sets(Machine(Z80))==(?);
  List_Valuable_Sets(Machine(Z80))==(?);
  Inherited_List_Enumerated(Machine(Z80))==(?);
  Inherited_List_Defered(Machine(Z80))==(?);
  Inherited_List_Sets(Machine(Z80))==(?);
  List_Enumerated(Machine(Z80))==(id_reg_8,id_reg_16);
  List_Defered(Machine(Z80))==(?);
  List_Sets(Machine(Z80))==(id_reg_8,id_reg_16);
  Set_Definition(Machine(Z80),id_reg_16)==({BC,DE,HL,SP,AF})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Z80))==(?);
  Expanded_List_HiddenConstants(Machine(Z80))==(?);
  List_HiddenConstants(Machine(Z80))==(?);
  External_List_HiddenConstants(Machine(Z80))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Z80))==(btrue);
  Context_List_Properties(Machine(Z80))==(SCHAR_LENGTH: NATURAL & SCHAR_LENGTH = 8 & NB_SCHARS: NATURAL & NB_SCHARS = 2**SCHAR_LENGTH & SCHAR = -128..127 & SCHAR_POSITION = 0..SCHAR_LENGTH-1 & UCHAR = 0..255 & SSHORTINT_LENGTH: NATURAL & SSHORTINT_LENGTH = 16 & NB_SSHORTINTS: NATURAL & NB_SSHORTINTS = 2**SSHORTINT_LENGTH & SSHORTINT = -32768..32767 & SSHORTINT_POSITION = 0..SSHORTINT_LENGTH-1 & BV16 <: BIT_VECTOR & BV16 = {vv | vv: BIT_VECTOR & size(vv) = SSHORTINT_LENGTH} & BYTE_TO_UCHAR: BYTE --> UCHAR & BYTE_TO_UCHAR = %v0.(v0: BYTE | (-128)*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & UCHAR_TO_BYTE: UCHAR --> BYTE & UCHAR_TO_BYTE = BYTE_TO_UCHAR~ & USHORTINT = 0..65535 & NB_INSTRUCTIONS: NATURAL & INST_SZ: NATURAL & INST_SZ = 16 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = USHORTINT & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & INSTRUCTION_NEXT = {p0,q0 | p0: INSTRUCTION & q0: INSTRUCTION & 0<=p0 & p0<NB_INSTRUCTIONS-1 & q0 = p0+1}\/{NB_INSTRUCTIONS-1|->0} & INSTRUCTION_JUMP = %(p0,e0).(p0: INSTRUCTION & e0: -126..129 | p0+e0) & BYTE_TO_SCHAR: BYTE --> SCHAR & BYTE_TO_SCHAR = %v0.(v0: BYTE | (-128)*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SCHAR_TO_BYTE: SCHAR --> BYTE & SCHAR_TO_BYTE = BYTE_TO_SCHAR~ & BV16_TO_SSHORTINT: BV16 --> SSHORTINT & BV16_TO_SSHORTINT = %v0.(v0: BV16 | (-32768)*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SSHORTINT_TO_BV16: SSHORTINT --> BV16 & SSHORTINT_TO_BV16 = BV16_TO_SSHORTINT~ & BYTE_TO_BV16: BYTE*BYTE --> BV16 & BYTE_TO_BV16 = %(v1,v2).(v1: BV16 & v2: BV16 | {0|->v2(0),1|->v2(1),2|->v2(2),3|->v2(3),4|->v2(4),5|->v2(5),6|->v2(6),7|->v2(7),8|->v1(0),9|->v1(1),10|->v1(2),11|->v1(3),12|->v1(4),13|->v1(5),14|->v1(6),15|->v1(7)}) & BV16_TO_BYTE: BV16 --> BYTE*BYTE & BV16_TO_BYTE = BYTE_TO_BV16~ & SCHAR_TO_SSHORTINT: SCHAR*SCHAR --> SSHORTINT & SCHAR_TO_SSHORTINT = %(w1,w2).(w1: SCHAR & w2: SCHAR | BV16_TO_SSHORTINT(BYTE_TO_BV16(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2)))) & SSHORTINT_TO_SCHAR: SSHORTINT --> SCHAR*SCHAR & SSHORTINT_TO_SCHAR = SCHAR_TO_SSHORTINT~ & BV16_TO_USHORTINT: BV16 --> USHORTINT & BV16_TO_USHORTINT = %v0.(v0: BV16 | (-32768)*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & USHORTINT_TO_BV16: USHORTINT --> BV16 & USHORTINT_TO_BV16 = BV16_TO_USHORTINT~ & parity_bit_from_BYTE: BIT_VECTOR --> BIT & parity_bit_from_BYTE = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | bv(idx)) mod 2) & SSHORTINT_TO_USHORTINT: SSHORTINT --> USHORTINT & SSHORTINT_TO_USHORTINT = %v0.(v0: SSHORTINT | v0-32768) & USHORTINT_TO_SSHORTINT: USHORTINT --> SSHORTINT & USHORTINT_TO_SSHORTINT = SSHORTINT_TO_USHORTINT~ & BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = {bt | bt: NATURAL +-> BIT}-{{}} & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & BV_INDX: BIT_VECTOR --> POW(NATURAL) & BV_INDX = %bv.(bv: BIT_VECTOR | 0..bv_size(bv)-1) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high | low..high<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (0..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (0..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: BV_INDX(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1(idx)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: BV_INDX(v1) & bit: BIT | v1<+{idx|->bit}) & BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE <: BIT_VECTOR & BYTE = {vv | vv: BIT_VECTOR & bv_size(vv) = BYTE_WIDTH} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & is_zeroSCHAR: SCHAR --> BIT & is_zeroSCHAR = %w1.(w1: SCHAR | bool_to_bit(bool(w1 = 0))) & is_negative: SCHAR --> BIT & is_negative = %w1.(w1: SCHAR | bool_to_bit(bool(w1<0))) & parity_bit_from_BYTE: BIT_VECTOR --> BIT & parity_bit_from_BYTE = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | bv(idx)) mod 2) & halfSCHAR: SCHAR --> SCHAR & halfSCHAR = %ww.(ww: SCHAR | ww mod 16) & add8SCHAR: SCHAR*SCHAR --> SCHAR*BOOL*BOOL*BOOL*BOOL & add8SCHAR = %(w1,w2).(w1: UCHAR & w2: UCHAR & w1+w2< -128 | 256-(w1+w2),bool(w1+w2<0),TRUE,bool(halfSCHAR(w1)+halfSCHAR(w2)>=16),FALSE)\/%(w1,w2).(w1: UCHAR & w2: UCHAR & not(w1+w2< -128) | (w1+w2) mod 128,bool(w1+w2<0),bool(w1+w2>127),bool(halfSCHAR(w1)+halfSCHAR(w2)>=16),bool(w1 = 0)) & substract8SCHAR: SCHAR*SCHAR --> SCHAR*BOOL*BOOL*BOOL*BOOL & substract8SCHAR = %(w1,w2).(w1: UCHAR & w2: UCHAR & w1-w2< -128 | 256-(w1-w2),bool(w1-w2<0),TRUE,bool(halfSCHAR(w1)-halfSCHAR(w2)>=16),FALSE)\/%(w1,w2).(w1: UCHAR & w2: UCHAR & not(w1-w2< -128) | (w1-w2) mod 128,bool(w1-w2<0),bool(w1-w2>127),bool(halfSCHAR(w1)-halfSCHAR(w2)>=16),bool(w1 = 0)) & add16SCHAR: SSHORTINT*SSHORTINT --> SSHORTINT*BOOL*BOOL & !(w1,w2,sum).(w1: SSHORTINT & w2: SSHORTINT & sum: NATURAL & sum = w1+w2 => (sum<=65535 => add16SCHAR(w1,w2) = sum,bool(sum = 0),FALSE) & (65536<=sum => add16SCHAR(w1,w2) = sum-65536,bool(sum = 65536),TRUE)) & andSCHAR: SCHAR*SCHAR --> SCHAR*BOOL & !(w1,w2,w0).(w1: SCHAR & w2: SCHAR & w0: SCHAR & w0 = BYTE_TO_SCHAR(bv_and(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2))) => andSCHAR(w1,w2) = w0,bool(w0 = 0)) & iorSCHAR: SCHAR*SCHAR --> SCHAR*BOOL & !(w1,w2,w0).(w1: SCHAR & w2: SCHAR & w0: SCHAR & w0 = BYTE_TO_SCHAR(bv_or(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2))) => iorSCHAR(w1,w2) = w0,bool(w0 = 0)) & xorSCHAR: SCHAR*SCHAR --> SCHAR*BOOL & !(w1,w2,w0).(w1: SCHAR & w2: SCHAR & w0: SCHAR & w0 = BYTE_TO_SCHAR(bv_xor(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2))) => xorSCHAR(w1,w2) = w0,bool(w0 = 0)) & bitgetSCHAR: SCHAR*SCHAR_POSITION --> BIT & bitgetSCHAR = %(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & i0 = 7 | 1-(w0+2**7)/128 mod 2)\/%(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & i0/=7 | (w0+2**7)/2**i0 mod 2) & bitsetSCHAR: SCHAR*SCHAR_POSITION --> SCHAR & bitsetSCHAR = %(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & bitgetSCHAR(w0,i0) = 1 | w0)\/%(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & bitgetSCHAR(w0,i0)/=1 | w0+2**i0) & bitclearSCHAR: SCHAR*SCHAR_POSITION --> SCHAR & bitclearSCHAR = %(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & bitgetSCHAR(w0,i0) = 0 | w0)\/%(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & bitgetSCHAR(w0,i0)/=0 & i0 = 7 | w0+128)\/%(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & bitgetSCHAR(w0,i0)/=0 & i0/=7 | w0-2**i0) & complementSCHAR: SCHAR --> SCHAR & complementSCHAR = %w0.(w0: SCHAR | 255-w0) & swapSCHAR: SCHAR --> SCHAR & !(w0,v0).(w0: SCHAR & v0: BIT_VECTOR => (v0 = SCHAR_TO_BYTE(w0) => swapSCHAR(w0) = BYTE_TO_SCHAR({0|->v0(4),1|->v0(5),2|->v0(6),3|->v0(7),4|->v0(0),5|->v0(1),6|->v0(2),7|->v0(3)}))) & rotateleftSCHAR: SCHAR --> SCHAR*BOOL & !(w0,v0).(w0: SCHAR & v0: BIT_VECTOR => (v0 = SCHAR_TO_BYTE(w0) => rotateleftSCHAR(w0) = BYTE_TO_SCHAR({0|->v0(7),1|->v0(0),2|->v0(1),3|->v0(2),4|->v0(3),5|->v0(4),6|->v0(5),7|->v0(6)}),bool(v0(7) = 1))) & rotaterightSCHAR: SCHAR --> SCHAR*BOOL & !(w0,v0).(w0: SCHAR & v0: BIT_VECTOR => (v0 = SCHAR_TO_BYTE(w0) => rotaterightSCHAR(w0) = BYTE_TO_SCHAR({0|->v0(1),1|->v0(2),2|->v0(3),3|->v0(4),4|->v0(5),5|->v0(6),6|->v0(7),7|->v0(0)}),bool(v0(0) = 1))));
  Inherited_List_Properties(Machine(Z80))==(btrue);
  List_Properties(Machine(Z80))==(REG16_TO_REG8: id_reg_16 --> id_reg_8*id_reg_8 & !(r1,r2,r3).(r1: id_reg_16 & r2: id_reg_8 & r3: id_reg_8 => (r1 = BC => REG16_TO_REG8(r1) = b0,c0) & (r1 = DE => REG16_TO_REG8(r1) = d0,e0) & (r1 = HL => REG16_TO_REG8(r1) = h0,l0) & (r1 = AF => REG16_TO_REG8(r1) = a0,f0)) & REG8_TO_REG16 = REG16_TO_REG8~ & update_flag_register = %(rgs8_,c0,n_add_sub,pv2,h4,z6,s7).(rgs8_: id_reg_8 --> SCHAR & c0: BOOL & n_add_sub: BOOL & pv2: BOOL & h4: BOOL & z6: BOOL & s7: BOOL | rgs8_<+{f0|->BYTE_TO_SCHAR(SCHAR_TO_BYTE(rgs8_(f0))<+{7|->bool_to_bit(s7),6|->bool_to_bit(z6),4|->bool_to_bit(h4),2|->bool_to_bit(pv2),1|->bool_to_bit(n_add_sub),0|->bool_to_bit(c0)})}) & get_new_flag_register = %(rgs8_,c0,n_add_sub,pv2,h4,z6,s7).(rgs8_: id_reg_8 --> SCHAR & c0: BOOL & n_add_sub: BOOL & pv2: BOOL & h4: BOOL & z6: BOOL & s7: BOOL | f0|->BYTE_TO_SCHAR(SCHAR_TO_BYTE(rgs8_(f0))<+{7|->bool_to_bit(s7),6|->bool_to_bit(z6),4|->bool_to_bit(h4),2|->bool_to_bit(pv2),1|->bool_to_bit(n_add_sub),0|->bool_to_bit(c0)})) & id_reg_8: FIN(INTEGER) & not(id_reg_8 = {}) & id_reg_16: FIN(INTEGER) & not(id_reg_16 = {}))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(Z80),Machine(POWER2))==(?);
  Seen_Context_List_Enumerated(Machine(Z80))==(?);
  Seen_Context_List_Invariant(Machine(Z80))==(btrue);
  Seen_Context_List_Assertions(Machine(Z80))==((2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_not(bv_not(bv))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_and(v1,v2)(indx) = bv_and(v2,v1)(indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_and(v1,bv_and(v2,v3))(indx) = bv_and(bv_and(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_zero(bv_size(bv)))(indx) = bv_zero(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_one(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_or(v1,v2)(indx) = bv_or(v2,v1)(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_or(v1,bv_or(v2,v3))(indx) = bv_or(bv_or(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_one(bv_size(bv)))(indx) = bv_one(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_zero(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_xor(v1,v2)(indx) = bv_xor(v2,v1)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_xor(bv,bv)(indx) = bv_zero(bv_size(bv))(indx))) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT => bb = 0 or bb = 1) & bool_to_bit(FALSE) = 0 & bool_to_bit(TRUE) = 1 & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & bit_xor(1,1) = 0 & bit_xor(1,0) = 1 & bit_xor(0,1) = 1 & bit_xor(0,0) = 0 & !b1.(b1: BIT => bit_or(0,b1) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & bit_or(1,1) = 1 & bit_or(1,0) = 1 & bit_or(0,1) = 1 & bit_or(0,0) = 0 & !b1.(b1: BIT => bit_and(b1,0) = 0) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & bit_and(1,1) = 1 & bit_and(1,0) = 0 & bit_and(0,1) = 0 & bit_and(0,0) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_not(1) = 0 & bit_not(0) = 1 & 0 = SCHAR_TO_SSHORTINT(0,0) & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & 2**16 = 65536 & !n0.(n0: SCHAR => n0<=255) & !n0.(n0: SCHAR => 0<=n0) & NB_SCHARS = 256 & !(nn,pp).(nn: INT & pp: NAT => (pp = 0 => nn**pp = 1) & (pp = 1 => nn**pp = nn) & (pp>1 => nn**pp = nn*nn**(pp-1))));
  Seen_Context_List_Properties(Machine(Z80))==(SCHAR_LENGTH: NATURAL & SCHAR_LENGTH = 8 & NB_SCHARS: NATURAL & NB_SCHARS = 2**SCHAR_LENGTH & SCHAR = -128..127 & SCHAR_POSITION = 0..SCHAR_LENGTH-1 & UCHAR = 0..255 & SSHORTINT_LENGTH: NATURAL & SSHORTINT_LENGTH = 16 & NB_SSHORTINTS: NATURAL & NB_SSHORTINTS = 2**SSHORTINT_LENGTH & SSHORTINT = -32768..32767 & SSHORTINT_POSITION = 0..SSHORTINT_LENGTH-1 & BV16 <: BIT_VECTOR & BV16 = {vv | vv: BIT_VECTOR & size(vv) = SSHORTINT_LENGTH} & BYTE_TO_UCHAR: BYTE --> UCHAR & BYTE_TO_UCHAR = %v0.(v0: BYTE | (-128)*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & UCHAR_TO_BYTE: UCHAR --> BYTE & UCHAR_TO_BYTE = BYTE_TO_UCHAR~ & USHORTINT = 0..65535 & NB_INSTRUCTIONS: NATURAL & INST_SZ: NATURAL & INST_SZ = 16 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = USHORTINT & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & INSTRUCTION_NEXT = {p0,q0 | p0: INSTRUCTION & q0: INSTRUCTION & 0<=p0 & p0<NB_INSTRUCTIONS-1 & q0 = p0+1}\/{NB_INSTRUCTIONS-1|->0} & INSTRUCTION_JUMP = %(p0,e0).(p0: INSTRUCTION & e0: -126..129 | p0+e0) & BYTE_TO_SCHAR: BYTE --> SCHAR & BYTE_TO_SCHAR = %v0.(v0: BYTE | (-128)*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SCHAR_TO_BYTE: SCHAR --> BYTE & SCHAR_TO_BYTE = BYTE_TO_SCHAR~ & BV16_TO_SSHORTINT: BV16 --> SSHORTINT & BV16_TO_SSHORTINT = %v0.(v0: BV16 | (-32768)*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SSHORTINT_TO_BV16: SSHORTINT --> BV16 & SSHORTINT_TO_BV16 = BV16_TO_SSHORTINT~ & BYTE_TO_BV16: BYTE*BYTE --> BV16 & BYTE_TO_BV16 = %(v1,v2).(v1: BV16 & v2: BV16 | {0|->v2(0),1|->v2(1),2|->v2(2),3|->v2(3),4|->v2(4),5|->v2(5),6|->v2(6),7|->v2(7),8|->v1(0),9|->v1(1),10|->v1(2),11|->v1(3),12|->v1(4),13|->v1(5),14|->v1(6),15|->v1(7)}) & BV16_TO_BYTE: BV16 --> BYTE*BYTE & BV16_TO_BYTE = BYTE_TO_BV16~ & SCHAR_TO_SSHORTINT: SCHAR*SCHAR --> SSHORTINT & SCHAR_TO_SSHORTINT = %(w1,w2).(w1: SCHAR & w2: SCHAR | BV16_TO_SSHORTINT(BYTE_TO_BV16(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2)))) & SSHORTINT_TO_SCHAR: SSHORTINT --> SCHAR*SCHAR & SSHORTINT_TO_SCHAR = SCHAR_TO_SSHORTINT~ & BV16_TO_USHORTINT: BV16 --> USHORTINT & BV16_TO_USHORTINT = %v0.(v0: BV16 | (-32768)*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & USHORTINT_TO_BV16: USHORTINT --> BV16 & USHORTINT_TO_BV16 = BV16_TO_USHORTINT~ & parity_bit_from_BYTE: BIT_VECTOR --> BIT & parity_bit_from_BYTE = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | bv(idx)) mod 2) & SSHORTINT_TO_USHORTINT: SSHORTINT --> USHORTINT & SSHORTINT_TO_USHORTINT = %v0.(v0: SSHORTINT | v0-32768) & USHORTINT_TO_SSHORTINT: USHORTINT --> SSHORTINT & USHORTINT_TO_SSHORTINT = SSHORTINT_TO_USHORTINT~ & BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = {bt | bt: NATURAL +-> BIT}-{{}} & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & BV_INDX: BIT_VECTOR --> POW(NATURAL) & BV_INDX = %bv.(bv: BIT_VECTOR | 0..bv_size(bv)-1) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high | low..high<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (0..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (0..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: BV_INDX(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1(idx)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: BV_INDX(v1) & bit: BIT | v1<+{idx|->bit}) & BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE <: BIT_VECTOR & BYTE = {vv | vv: BIT_VECTOR & bv_size(vv) = BYTE_WIDTH} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0});
  Seen_List_Constraints(Machine(Z80))==(btrue);
  Seen_List_Operations(Machine(Z80),Machine(POWER2))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80),Machine(POWER2))==(btrue);
  Seen_Internal_List_Operations(Machine(Z80),Machine(ALU))==(?);
  Seen_List_Operations(Machine(Z80),Machine(ALU))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80),Machine(ALU))==(btrue);
  Seen_Internal_List_Operations(Machine(Z80),Machine(TYPES))==(?);
  Seen_List_Operations(Machine(Z80),Machine(TYPES))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80),Machine(TYPES))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(Z80),LD_r_r_)==(?);
  List_ANY_Var(Machine(Z80),LD_r_n_)==(?);
  List_ANY_Var(Machine(Z80),LD_r_9HL0)==(Var(address) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(Z80),LD_r_9IX_d0)==((Var(address) == btype(INTEGER,?,?)),(Var(iszero) == btype(BOOL,?,?)),(Var(overflow) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80),LD_r_9IY_d0)==((Var(address) == btype(INTEGER,?,?)),(Var(iszero) == btype(BOOL,?,?)),(Var(overflow) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80),LD_9HL0_r)==(Var(address) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(Z80),LD_9IX_d0_r)==((Var(address) == btype(INTEGER,?,?)),(Var(iszero) == btype(BOOL,?,?)),(Var(overflow) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80),LD_9IY_d0_r)==((Var(address) == btype(INTEGER,?,?)),(Var(iszero) == btype(BOOL,?,?)),(Var(overflow) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80),LD_9HL0_n)==(Var(address) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(Z80),LD_9IX_d0_n)==((Var(address) == btype(INTEGER,?,?)),(Var(iszero) == btype(BOOL,?,?)),(Var(overflow) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80),LD_9IY_d0_n)==((Var(address) == btype(INTEGER,?,?)),(Var(iszero) == btype(BOOL,?,?)),(Var(overflow) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80),LD_A_9BC0)==(Var(address) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(Z80),LD_A_9DE0)==(Var(address) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(Z80),LD_A_9nn0)==(?);
  List_ANY_Var(Machine(Z80),LD_9BC0_A)==(Var(address) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(Z80),LD_9DE0_A)==(Var(address) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(Z80),LD_9nn0_A)==(?);
  List_ANY_Var(Machine(Z80),LD_dd_nn)==((Var(rh) == etype(id_reg_8,?,?)),(Var(rl) == etype(id_reg_8,?,?)),(Var(w1) == btype(INTEGER,?,?)),(Var(w2) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),LD_IX_nn)==(?);
  List_ANY_Var(Machine(Z80),LD_IY_nn)==(?);
  List_ANY_Var(Machine(Z80),LD_HL_9nn0)==(?);
  List_ANY_Var(Machine(Z80),LD_dd_9nn0)==((Var(rh) == etype(id_reg_8,?,?)),(Var(rl) == etype(id_reg_8,?,?)),(Var(w1) == btype(INTEGER,?,?)),(Var(w2) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),LD_IX_9nn0)==(?);
  List_ANY_Var(Machine(Z80),LD_IY_9nn0)==(?);
  List_ANY_Var(Machine(Z80),LD_9nn0_HL)==(?);
  List_ANY_Var(Machine(Z80),LD_9nn0_dd)==((Var(vh) == btype(INTEGER,?,?)),(Var(vl) == btype(INTEGER,?,?)),(Var(rh) == etype(id_reg_8,?,?)),(Var(rl) == etype(id_reg_8,?,?)),(Var(w1) == btype(INTEGER,?,?)),(Var(w2) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),LD_9nn0_IX)==((Var(h_ix) == btype(INTEGER,?,?)),(Var(l_ix) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),LD_9nn0_IY)==((Var(h_iy) == btype(INTEGER,?,?)),(Var(l_iy) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),LD_SP_HL)==(?);
  List_ANY_Var(Machine(Z80),LD_SP_IX)==(?);
  List_ANY_Var(Machine(Z80),LD_SP_IY)==(?);
  List_ANY_Var(Machine(Z80),PUSH_qq)==((Var(qqh) == etype(id_reg_8,?,?)),(Var(qql) == etype(id_reg_8,?,?)));
  List_ANY_Var(Machine(Z80),PUSH_IX)==((Var(wh) == btype(INTEGER,?,?)),(Var(wl) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),PUSH_IY)==((Var(wh) == btype(INTEGER,?,?)),(Var(wl) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),POP_qq)==((Var(qqh) == etype(id_reg_8,?,?)),(Var(qql) == etype(id_reg_8,?,?)));
  List_ANY_Var(Machine(Z80),POP_IX)==(Var(nw8) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(Z80),POP_IY)==(Var(nw8) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(Z80),EX_DE_HL)==(?);
  List_ANY_Var(Machine(Z80),EX_AF_AF_)==(?);
  List_ANY_Var(Machine(Z80),EXX)==(?);
  List_ANY_Var(Machine(Z80),EX_9SP0_HL)==(?);
  List_ANY_Var(Machine(Z80),EX_9SP0_IX)==((Var(wh) == btype(INTEGER,?,?)),(Var(wl) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),EX_9SP0_IY)==((Var(wh) == btype(INTEGER,?,?)),(Var(wl) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),LDI)==((Var(hvn) == btype(INTEGER,?,?)),(Var(lvn) == btype(INTEGER,?,?)),(Var(dvn) == btype(INTEGER,?,?)),(Var(evn) == btype(INTEGER,?,?)),(Var(bvn) == btype(INTEGER,?,?)),(Var(cvn) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),LDIR)==((Var(hvn) == btype(INTEGER,?,?)),(Var(lvn) == btype(INTEGER,?,?)),(Var(dvn) == btype(INTEGER,?,?)),(Var(evn) == btype(INTEGER,?,?)),(Var(bvn) == btype(INTEGER,?,?)),(Var(cvn) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),LDD)==((Var(hvn) == btype(INTEGER,?,?)),(Var(lvn) == btype(INTEGER,?,?)),(Var(dvn) == btype(INTEGER,?,?)),(Var(evn) == btype(INTEGER,?,?)),(Var(bvn) == btype(INTEGER,?,?)),(Var(cvn) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),LDDR)==((Var(hvn) == btype(INTEGER,?,?)),(Var(lvn) == btype(INTEGER,?,?)),(Var(dvn) == btype(INTEGER,?,?)),(Var(evn) == btype(INTEGER,?,?)),(Var(bvn) == btype(INTEGER,?,?)),(Var(cvn) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),CPI)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80),BIT_b_rr)==((Var(nw8) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(ib) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),BIT_b_9HL0)==((Var(address) == btype(INTEGER,?,?)),(Var(nw8) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(ib) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),JR_NZ_e)==(?);
  List_ANY_Var(Machine(Z80),JR_Z_e)==(?);
  List_ANY_Var(Machine(Z80),SET_b_HL)==((Var(address) == btype(INTEGER,?,?)),(Var(nw8) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),RES_b_HL)==((Var(address) == btype(INTEGER,?,?)),(Var(nw8) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),GOTO)==(?);
  List_ANY_Var(Machine(Z80),CALL)==(?);
  List_ANY_Var(Machine(Z80),RETURN)==(?);
  List_ANY_Var(Machine(Z80),IN_A_9n0_)==(Var(data_in) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(Z80),IN_A_9n0)==(Var(data_in) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(Z80),INI)==((Var(data_in) == btype(INTEGER,?,?)),(Var(hvn) == btype(INTEGER,?,?)),(Var(lvn) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),INIR)==((Var(data_in) == btype(INTEGER,?,?)),(Var(hvn) == btype(INTEGER,?,?)),(Var(lvn) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),IND)==((Var(data_in) == btype(INTEGER,?,?)),(Var(hvn) == btype(INTEGER,?,?)),(Var(lvn) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),INDR)==((Var(data_in) == btype(INTEGER,?,?)),(Var(hvn) == btype(INTEGER,?,?)),(Var(lvn) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),OUT_9n0_A)==(?);
  List_ANY_Var(Machine(Z80),OUT_9C0_r)==(?);
  List_ANY_Var(Machine(Z80),OUTI)==((Var(hvn) == btype(INTEGER,?,?)),(Var(lvn) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),OUTIR)==((Var(hvn) == btype(INTEGER,?,?)),(Var(lvn) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Z80)) == (REG16_TO_REG8,REG8_TO_REG16,update_flag_register,get_new_flag_register,id_reg_8,id_reg_16,a0,f0,f_0,a_0,b0,c0,b_0,c_0,d0,e0,d_0,e_0,h0,l0,h_0,l_0,i0,r0,BC,DE,HL,SP,AF | ? | i_o_ports,iy,ix,sp,pc,rgs8 | stack,mem | LD_r_r_,LD_r_n_,LD_r_9HL0,LD_r_9IX_d0,LD_r_9IY_d0,LD_9HL0_r,LD_9IX_d0_r,LD_9IY_d0_r,LD_9HL0_n,LD_9IX_d0_n,LD_9IY_d0_n,LD_A_9BC0,LD_A_9DE0,LD_A_9nn0,LD_9BC0_A,LD_9DE0_A,LD_9nn0_A,LD_dd_nn,LD_IX_nn,LD_IY_nn,LD_HL_9nn0,LD_dd_9nn0,LD_IX_9nn0,LD_IY_9nn0,LD_9nn0_HL,LD_9nn0_dd,LD_9nn0_IX,LD_9nn0_IY,LD_SP_HL,LD_SP_IX,LD_SP_IY,PUSH_qq,PUSH_IX,PUSH_IY,POP_qq,POP_IX,POP_IY,EX_DE_HL,EX_AF_AF_,EXX,EX_9SP0_HL,EX_9SP0_IX,EX_9SP0_IY,LDI,LDIR,LDD,LDDR,CPI,BIT_b_rr,BIT_b_9HL0,JR_NZ_e,JR_Z_e,SET_b_HL,RES_b_HL,GOTO,CALL,RETURN,IN_A_9n0_,IN_A_9n0,INI,INIR,IND,INDR,OUT_9n0_A,OUT_9C0_r,OUTI,OUTIR | ? | seen(Machine(TYPES)),seen(Machine(ALU)),seen(Machine(POWER2)),included(Machine(MEMORY)) | ? | Z80);
  List_Of_HiddenCst_Ids(Machine(Z80)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Z80)) == (REG16_TO_REG8,REG8_TO_REG16,update_flag_register,get_new_flag_register);
  List_Of_VisibleVar_Ids(Machine(Z80)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Z80)) == (seen(Machine(TYPES)): (SCHAR,SCHAR_LENGTH,SCHAR_POSITION,NB_SCHARS,BYTE_TO_SCHAR,SCHAR_TO_BYTE,UCHAR,UCHAR_TO_BYTE,BYTE_TO_UCHAR,BV16,SSHORTINT,SSHORTINT_LENGTH,SSHORTINT_POSITION,NB_SSHORTINTS,BV16_TO_SSHORTINT,SSHORTINT_TO_BV16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,INSTRUCTION_JUMP,BYTE_TO_BV16,BV16_TO_BYTE,SCHAR_TO_SSHORTINT,SSHORTINT_TO_SCHAR,parity_bit_from_BYTE,USHORTINT,BV16_TO_USHORTINT,USHORTINT_TO_BV16,USHORTINT_TO_SSHORTINT,SSHORTINT_TO_USHORTINT | BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put,BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(ALU)): (add16SCHAR,halfSCHAR,add8SCHAR,substract8SCHAR,andSCHAR,iorSCHAR,xorSCHAR,bitclearSCHAR,bitsetSCHAR,bitgetSCHAR,complementSCHAR,swapSCHAR,rotateleftSCHAR,rotaterightSCHAR,is_zeroSCHAR,is_negative | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(MEMORY)) == (? | ? | stack,mem | ? | updateMem,updateAddressMem,updateStack,updateAddressStack,pop | ? | seen(Machine(TYPES)),seen(Machine(ALU)) | ? | MEMORY);
  List_Of_HiddenCst_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(MEMORY)) == (?);
  List_Of_VisibleVar_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(MEMORY)) == (?: ?);
  List_Of_Ids(Machine(ALU)) == (add16SCHAR,halfSCHAR,add8SCHAR,substract8SCHAR,andSCHAR,iorSCHAR,xorSCHAR,bitclearSCHAR,bitsetSCHAR,bitgetSCHAR,complementSCHAR,swapSCHAR,rotateleftSCHAR,rotaterightSCHAR,is_zeroSCHAR,is_negative | ? | ? | ? | ? | ? | seen(Machine(TYPES)) | ? | ALU);
  List_Of_HiddenCst_Ids(Machine(ALU)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ALU)) == (add16SCHAR,halfSCHAR,add8SCHAR,substract8SCHAR,andSCHAR,iorSCHAR,xorSCHAR,bitclearSCHAR,bitsetSCHAR,bitgetSCHAR,complementSCHAR,swapSCHAR,rotateleftSCHAR,rotaterightSCHAR,is_zeroSCHAR,is_negative);
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
  Sets(Machine(Z80)) == (Type(id_reg_8) == Cst(SetOf(etype(id_reg_8,0,17)));Type(id_reg_16) == Cst(SetOf(etype(id_reg_16,0,4))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(Z80)) == (Type(a0) == Cst(etype(id_reg_8,0,17));Type(f0) == Cst(etype(id_reg_8,0,17));Type(f_0) == Cst(etype(id_reg_8,0,17));Type(a_0) == Cst(etype(id_reg_8,0,17));Type(b0) == Cst(etype(id_reg_8,0,17));Type(c0) == Cst(etype(id_reg_8,0,17));Type(b_0) == Cst(etype(id_reg_8,0,17));Type(c_0) == Cst(etype(id_reg_8,0,17));Type(d0) == Cst(etype(id_reg_8,0,17));Type(e0) == Cst(etype(id_reg_8,0,17));Type(d_0) == Cst(etype(id_reg_8,0,17));Type(e_0) == Cst(etype(id_reg_8,0,17));Type(h0) == Cst(etype(id_reg_8,0,17));Type(l0) == Cst(etype(id_reg_8,0,17));Type(h_0) == Cst(etype(id_reg_8,0,17));Type(l_0) == Cst(etype(id_reg_8,0,17));Type(i0) == Cst(etype(id_reg_8,0,17));Type(r0) == Cst(etype(id_reg_8,0,17));Type(BC) == Cst(etype(id_reg_16,0,4));Type(DE) == Cst(etype(id_reg_16,0,4));Type(HL) == Cst(etype(id_reg_16,0,4));Type(SP) == Cst(etype(id_reg_16,0,4));Type(AF) == Cst(etype(id_reg_16,0,4));Type(REG16_TO_REG8) == Cst(SetOf(etype(id_reg_16,0,4)*(etype(id_reg_8,0,17)*etype(id_reg_8,0,17))));Type(REG8_TO_REG16) == Cst(SetOf(etype(id_reg_8,?,?)*etype(id_reg_8,?,?)*etype(id_reg_16,?,?)));Type(update_flag_register) == Cst(SetOf(SetOf(etype(id_reg_8,?,?)*btype(INTEGER,?,?))*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*SetOf(etype(id_reg_8,?,?)*btype(INTEGER,?,?))));Type(get_new_flag_register) == Cst(SetOf(SetOf(etype(id_reg_8,?,?)*btype(INTEGER,?,?))*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*(etype(id_reg_8,?,?)*btype(INTEGER,?,?)))))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Z80)) == (Type(mem) == Mvl(SetOf(btype(INTEGER,"[USHORTINT","]USHORTINT")*btype(INTEGER,"[SCHAR","]SCHAR")));Type(stack) == Mvl(SetOf(btype(INTEGER,"[USHORTINT","]USHORTINT")*btype(INTEGER,"[SCHAR","]SCHAR")));Type(i_o_ports) == Mvl(SetOf(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[SCHAR","]SCHAR")));Type(iy) == Mvl(btype(INTEGER,?,?));Type(ix) == Mvl(btype(INTEGER,?,?));Type(sp) == Mvl(btype(INTEGER,?,?));Type(pc) == Mvl(btype(INTEGER,?,?));Type(rgs8) == Mvl(SetOf(etype(id_reg_8,0,17)*btype(INTEGER,"[SCHAR","]SCHAR"))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Z80)) == (Type(OUTIR) == Cst(No_type,No_type);Type(OUTI) == Cst(No_type,No_type);Type(OUT_9C0_r) == Cst(No_type,etype(id_reg_8,?,?));Type(OUT_9n0_A) == Cst(No_type,btype(INTEGER,?,?));Type(INDR) == Cst(No_type,No_type);Type(IND) == Cst(No_type,No_type);Type(INIR) == Cst(No_type,No_type);Type(INI) == Cst(No_type,No_type);Type(IN_A_9n0) == Cst(No_type,btype(INTEGER,?,?));Type(IN_A_9n0_) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RETURN) == Cst(No_type,No_type);Type(CALL) == Cst(No_type,btype(INTEGER,?,?));Type(GOTO) == Cst(No_type,btype(INTEGER,?,?));Type(RES_b_HL) == Cst(No_type,btype(INTEGER,?,?));Type(SET_b_HL) == Cst(No_type,btype(INTEGER,?,?));Type(JR_Z_e) == Cst(No_type,btype(INTEGER,?,?));Type(JR_NZ_e) == Cst(No_type,btype(INTEGER,?,?));Type(BIT_b_9HL0) == Cst(No_type,btype(INTEGER,?,?));Type(BIT_b_rr) == Cst(No_type,btype(INTEGER,?,?)*etype(id_reg_8,?,?));Type(CPI) == Cst(No_type,No_type);Type(LDDR) == Cst(No_type,No_type);Type(LDD) == Cst(No_type,No_type);Type(LDIR) == Cst(No_type,No_type);Type(LDI) == Cst(No_type,No_type);Type(EX_9SP0_IY) == Cst(No_type,No_type);Type(EX_9SP0_IX) == Cst(No_type,No_type);Type(EX_9SP0_HL) == Cst(No_type,No_type);Type(EXX) == Cst(No_type,No_type);Type(EX_AF_AF_) == Cst(No_type,No_type);Type(EX_DE_HL) == Cst(No_type,No_type);Type(POP_IY) == Cst(No_type,No_type);Type(POP_IX) == Cst(No_type,No_type);Type(POP_qq) == Cst(No_type,etype(id_reg_16,?,?));Type(PUSH_IY) == Cst(No_type,No_type);Type(PUSH_IX) == Cst(No_type,No_type);Type(PUSH_qq) == Cst(No_type,etype(id_reg_16,?,?));Type(LD_SP_IY) == Cst(No_type,No_type);Type(LD_SP_IX) == Cst(No_type,No_type);Type(LD_SP_HL) == Cst(No_type,No_type);Type(LD_9nn0_IY) == Cst(No_type,btype(INTEGER,?,?));Type(LD_9nn0_IX) == Cst(No_type,btype(INTEGER,?,?));Type(LD_9nn0_dd) == Cst(No_type,btype(INTEGER,?,?)*etype(id_reg_16,?,?));Type(LD_9nn0_HL) == Cst(No_type,btype(INTEGER,?,?));Type(LD_IY_9nn0) == Cst(No_type,btype(INTEGER,?,?));Type(LD_IX_9nn0) == Cst(No_type,btype(INTEGER,?,?));Type(LD_dd_9nn0) == Cst(No_type,etype(id_reg_16,?,?)*btype(INTEGER,?,?));Type(LD_HL_9nn0) == Cst(No_type,btype(INTEGER,?,?));Type(LD_IY_nn) == Cst(No_type,btype(INTEGER,?,?));Type(LD_IX_nn) == Cst(No_type,btype(INTEGER,?,?));Type(LD_dd_nn) == Cst(No_type,etype(id_reg_16,?,?)*btype(INTEGER,?,?));Type(LD_9nn0_A) == Cst(No_type,btype(INTEGER,?,?));Type(LD_9DE0_A) == Cst(No_type,No_type);Type(LD_9BC0_A) == Cst(No_type,No_type);Type(LD_A_9nn0) == Cst(No_type,btype(INTEGER,?,?));Type(LD_A_9DE0) == Cst(No_type,No_type);Type(LD_A_9BC0) == Cst(No_type,No_type);Type(LD_9IY_d0_n) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(LD_9IX_d0_n) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(LD_9HL0_n) == Cst(No_type,btype(INTEGER,?,?));Type(LD_9IY_d0_r) == Cst(No_type,btype(INTEGER,?,?)*etype(id_reg_8,?,?));Type(LD_9IX_d0_r) == Cst(No_type,btype(INTEGER,?,?)*etype(id_reg_8,?,?));Type(LD_9HL0_r) == Cst(No_type,etype(id_reg_8,?,?));Type(LD_r_9IY_d0) == Cst(No_type,etype(id_reg_8,?,?)*btype(INTEGER,?,?));Type(LD_r_9IX_d0) == Cst(No_type,etype(id_reg_8,?,?)*btype(INTEGER,?,?));Type(LD_r_9HL0) == Cst(No_type,etype(id_reg_8,?,?));Type(LD_r_n_) == Cst(No_type,etype(id_reg_8,?,?)*btype(INTEGER,?,?));Type(LD_r_r_) == Cst(No_type,etype(id_reg_8,?,?)*etype(id_reg_8,?,?)))
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
