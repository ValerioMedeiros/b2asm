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
  Local_List_Variables(Machine(Z80))==(i_o_ports,im,iff2,iff1,r_,i_,iy,ix,sp,pc,rgs8);
  List_Variables(Machine(Z80))==(i_o_ports,im,iff2,iff1,r_,i_,iy,ix,sp,pc,rgs8,stack,mem);
  External_List_Variables(Machine(Z80))==(i_o_ports,im,iff2,iff1,r_,i_,iy,ix,sp,pc,rgs8,stack,mem)
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
  Expanded_List_Invariant(Machine(Z80))==(mem: BV16 --> BYTE & stack: BV16 --> BYTE & stack <<: mem);
  Context_List_Invariant(Machine(Z80))==(btrue);
  List_Invariant(Machine(Z80))==(rgs8: id_reg_8 --> BYTE & pc: INSTRUCTION & sp: BV16 & ix: BV16 & iy: BV16 & i_: BYTE & r_: BYTE & iff1: BIT & iff2: BIT & im: BIT*BIT & i_o_ports: BYTE --> BYTE)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Machine(Z80))==(btrue);
  Expanded_List_Assertions(Machine(Z80))==(dom(stack) = BV16 & ran(stack) <: BYTE & ran(mem) <: BYTE & dom(mem) = BV16 & !(address,value).(address: BV16 & value: BYTE => mem<+{address|->value}: BV16 --> BYTE) & !(address,value).(address: BV16 & value: BYTE => stack<+{address|->value}: BV16 --> BYTE));
  Context_List_Assertions(Machine(Z80))==(0 = schar_to_sshortint(0,0) & !n0.(n0: UCHAR => 0<=n0) & !n0.(n0: UCHAR => n0<=255) & instruction_next: USHORTINT --> USHORTINT & !xx.(xx: BYTE => byte_to_uchar(xx): UCHAR) & !xx.(xx: UCHAR => uchar_to_byte(xx): BYTE) & !xx.(xx: BYTE => update_refresh_reg(xx): BYTE) & !xx.(xx: BYTE => byte_to_schar(xx): SCHAR) & !xx.(xx: SCHAR => schar_to_byte(xx): BYTE) & !(xx,yy).(xx: BYTE & yy: BYTE => byte_to_bv16(xx,yy): BV16) & !(xx,yy).(xx: BYTE & yy: BYTE => #zz.(zz: BV16 & byte_to_bv16(xx,yy) = zz)) & !xx.(xx: BV16 => bv16_to_ushortint(xx): USHORTINT) & !xx.(xx: USHORTINT => ushortint_to_bv16(xx): BV16) & !xx.(xx: BYTE => get_upper_digit(xx): 0..16) & !xx.(xx: BYTE => get_lower_digit(xx): 0..16) & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_to_bit(TRUE) = 1 & bool_to_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_not(bv_not(bv)),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: 0..bv_size(bv)-1 & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,v2),indx) = bv_get(bv_and(v2,v1),indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,bv_and(v2,v3)),indx) = bv_get(bv_and(bv_and(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv_zero(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_one(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,v2),indx) = bv_get(bv_or(v2,v1),indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,bv_or(v2,v3)),indx) = bv_get(bv_or(bv_or(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_one(bv_size(bv))),indx) = bv_get(bv_one(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_xor(v1,v2),indx) = bv_get(bv_xor(v2,v1),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_xor(bv,bv),indx) = bv_get(bv_zero(bv_size(bv)),indx))) & !bt.(bt: BYTE => size(bt) = 8) & size(BYTE_ZERO) = 8 & BYTE <: BIT_VECTOR & BYTE_ZERO: BIT_VECTOR & first(BYTE_ZERO) = 0 & BV16_ZERO: BV16 & BV16 <: BIT_VECTOR & BV16_ZERO: BIT_VECTOR & dom(add8UCHAR) = BIT*UCHAR*UCHAR & ran(add8UCHAR): POW(UCHAR*BIT*BIT*BIT*BIT) & dom(substract8UCHAR) = BIT*UCHAR*UCHAR & ran(substract8UCHAR): POW(UCHAR*BIT*BIT*BIT*BIT) & dom(and) = BYTE*BYTE & ran(and) <: BYTE & dom(ior) = BYTE*BYTE & ran(ior) <: BYTE & dom(xor) = BYTE*BYTE & ran(xor) <: BYTE & dom(complement) = BYTE & ran(complement) <: BYTE & dom(swap) = BYTE & ran(swap) <: BYTE & dom(rotateleft) = BYTE & ran(rotateleft) <: BYTE & dom(rotateright) = BYTE & ran(rotateright) <: BYTE & !(vec,in0).(vec: BYTE & in0: 0..7 => bitget(vec,in0) = vec(in0)) & !(x0,x1).(x0: UCHAR & x1: UCHAR => simple_add8UCHAR(x0,x1): UCHAR) & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536));
  List_Assertions(Machine(Z80))==(dom(stack) = BV16 & ran(stack) <: BYTE & ran(mem) <: BYTE & dom(mem) = BV16 & ran(rgs8) <: BYTE & dom(rgs8) = id_reg_8 & instruction_next(0) = 1 & instruction_next(1) = 2 & instruction_next(2) = 3 & instruction_next(3) = 4 & instruction_next(4) = 5 & instruction_next(5) = 6 & instruction_next(6) = 7 & instruction_next(7) = 8 & instruction_next(8) = 9 & instruction_next(9) = 10 & instruction_next(10) = 11 & instruction_next(11) = 12 & instruction_next(12) = 13 & instruction_next(13) = 14 & mem(byte_to_bv16(rgs8(b0),rgs8(c0))): BYTE & mem(byte_to_bv16(schar_to_byte(0),mem(byte_to_bv16(rgs8(b0),rgs8(c0))))): BYTE & mem(byte_to_bv16(rgs8(d0),rgs8(e0))): BYTE & mem(byte_to_bv16(schar_to_byte(0),mem(byte_to_bv16(rgs8(d0),rgs8(e0))))): BYTE & mem(byte_to_bv16(rgs8(h0),rgs8(l0))): BYTE & mem(byte_to_bv16(schar_to_byte(0),mem(byte_to_bv16(rgs8(h0),rgs8(l0))))): BYTE & mem(byte_to_bv16(rgs8(a0),rgs8(f0))): BYTE & mem(byte_to_bv16(schar_to_byte(0),mem(byte_to_bv16(rgs8(a0),rgs8(f0))))): BYTE & mem(sp): BYTE & mem(ix): BYTE & mem(iy): BYTE & !(ii,des).(ii: BV16 & des: SCHAR => bv_ireg_plus_d(ii,des): BV16) & !(mmm,ii,des).(mmm: BV16 >-> BYTE & ii: BV16 & des: SCHAR => bv_9ireg_plus_d0(mmm,ii,des): BYTE) & !(zn,c0,value,h0).(zn: BIT & c0: BIT & value: BYTE & h0: BIT => daa_function(zn,c0,value,h0): BYTE*BIT*BIT))
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Z80))==(@(mem$0).(mem$0: BV16 --> BYTE ==> mem:=mem$0) || @(stack$0).(stack$0: BV16 --> BYTE ==> stack:=stack$0);(@(rgs8$0).(rgs8$0: id_reg_8 --> BYTE ==> rgs8:=rgs8$0) || @(pc$0).(pc$0: INSTRUCTION ==> pc:=pc$0) || @(sp$0).(sp$0: BV16 ==> sp:=sp$0) || @(ix$0).(ix$0: BV16 ==> ix:=ix$0) || @(iy$0).(iy$0: BV16 ==> iy:=iy$0) || @(i_$0).(i_$0: BYTE ==> i_:=i_$0) || @(r_$0).(r_$0: BYTE ==> r_:=r_$0) || @(i_o_ports$0).(i_o_ports$0: BYTE --> BYTE ==> i_o_ports:=i_o_ports$0) || @(iff1$0).(iff1$0: BIT ==> iff1:=iff1$0) || @(iff2$0).(iff2$0: BIT ==> iff2:=iff2$0) || @(im$0).(im$0: BIT*BIT ==> im:=im$0)));
  Context_List_Initialisation(Machine(Z80))==(skip);
  List_Initialisation(Machine(Z80))==(rgs8:: id_reg_8 --> BYTE || pc:: INSTRUCTION || sp:: BV16 || ix:: BV16 || iy:: BV16 || i_:: BYTE || r_:: BYTE || i_o_ports:: BYTE --> BYTE || iff1:: BIT || iff2:: BIT || im:: BIT*BIT)
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
  Internal_List_Operations(Machine(Z80))==(LD_r_r_,LD_r_n_,LD_r_9HL0,LD_r_9IX_d0,LD_r_9IY_d0,LD_9HL0_r,LD_9IX_d0_r,LD_9IY_d0_r,LD_9HL0_n,LD_9IX_d0_n,LD_9IY_d0_n,LD_A_9BC0,LD_A_9DE0,LD_A_9nn0,LD_9BC0_A,LD_9DE0_A,LD_9nn0_A,LD_A_I,LD_A_R,LD_I_A,LD_R_A,LD_dd_nn,LD_IX_nn,LD_IY_nn,LD_HL_9nn0,LD_dd_9nn0,LD_IX_9nn0,LD_IY_9nn0,LD_9nn0_HL,LD_9nn0_dd,LD_9nn0_IX,LD_9nn0_IY,LD_SP_HL,LD_SP_IX,LD_SP_IY,PUSH_qq,PUSH_IX,PUSH_IY,POP_qq,POP_IX,POP_IY,EX_DE_HL,EX_AF_AF_,EXX,EX_9SP0_HL,EX_9SP0_IX,EX_9SP0_IY,LDI,LDIR,LDD,LDDR,CPI,CPIR,CPD,CPDR,DAA,CPL,NEG,CCF,SCF,NOP,HALT,DI,EI,IM0,IM1,IM2,IN_A_9n0,IN_r_9C0,INI,INIR,IND,INDR,OUT_9n0_A,OUT_9C0_r,OUTI,OUTIR,OUTD,OUTDR);
  List_Operations(Machine(Z80))==(LD_r_r_,LD_r_n_,LD_r_9HL0,LD_r_9IX_d0,LD_r_9IY_d0,LD_9HL0_r,LD_9IX_d0_r,LD_9IY_d0_r,LD_9HL0_n,LD_9IX_d0_n,LD_9IY_d0_n,LD_A_9BC0,LD_A_9DE0,LD_A_9nn0,LD_9BC0_A,LD_9DE0_A,LD_9nn0_A,LD_A_I,LD_A_R,LD_I_A,LD_R_A,LD_dd_nn,LD_IX_nn,LD_IY_nn,LD_HL_9nn0,LD_dd_9nn0,LD_IX_9nn0,LD_IY_9nn0,LD_9nn0_HL,LD_9nn0_dd,LD_9nn0_IX,LD_9nn0_IY,LD_SP_HL,LD_SP_IX,LD_SP_IY,PUSH_qq,PUSH_IX,PUSH_IY,POP_qq,POP_IX,POP_IY,EX_DE_HL,EX_AF_AF_,EXX,EX_9SP0_HL,EX_9SP0_IX,EX_9SP0_IY,LDI,LDIR,LDD,LDDR,CPI,CPIR,CPD,CPDR,DAA,CPL,NEG,CCF,SCF,NOP,HALT,DI,EI,IM0,IM1,IM2,IN_A_9n0,IN_r_9C0,INI,INIR,IND,INDR,OUT_9n0_A,OUT_9C0_r,OUTI,OUTIR,OUTD,OUTDR)
END
&
THEORY ListInputX IS
  List_Input(Machine(Z80),LD_r_r_)==(rr,rr_);
  List_Input(Machine(Z80),LD_r_n_)==(rr,n0);
  List_Input(Machine(Z80),LD_r_9HL0)==(rr);
  List_Input(Machine(Z80),LD_r_9IX_d0)==(rr,desloc);
  List_Input(Machine(Z80),LD_r_9IY_d0)==(rr,desloc);
  List_Input(Machine(Z80),LD_9HL0_r)==(rr);
  List_Input(Machine(Z80),LD_9IX_d0_r)==(desloc,rr);
  List_Input(Machine(Z80),LD_9IY_d0_r)==(desloc,rr);
  List_Input(Machine(Z80),LD_9HL0_n)==(n0);
  List_Input(Machine(Z80),LD_9IX_d0_n)==(desloc,n0);
  List_Input(Machine(Z80),LD_9IY_d0_n)==(desloc,n0);
  List_Input(Machine(Z80),LD_A_9BC0)==(?);
  List_Input(Machine(Z80),LD_A_9DE0)==(?);
  List_Input(Machine(Z80),LD_A_9nn0)==(nn);
  List_Input(Machine(Z80),LD_9BC0_A)==(?);
  List_Input(Machine(Z80),LD_9DE0_A)==(?);
  List_Input(Machine(Z80),LD_9nn0_A)==(nn);
  List_Input(Machine(Z80),LD_A_I)==(?);
  List_Input(Machine(Z80),LD_A_R)==(?);
  List_Input(Machine(Z80),LD_I_A)==(?);
  List_Input(Machine(Z80),LD_R_A)==(?);
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
  List_Input(Machine(Z80),CPIR)==(?);
  List_Input(Machine(Z80),CPD)==(?);
  List_Input(Machine(Z80),CPDR)==(?);
  List_Input(Machine(Z80),DAA)==(?);
  List_Input(Machine(Z80),CPL)==(?);
  List_Input(Machine(Z80),NEG)==(?);
  List_Input(Machine(Z80),CCF)==(?);
  List_Input(Machine(Z80),SCF)==(?);
  List_Input(Machine(Z80),NOP)==(?);
  List_Input(Machine(Z80),HALT)==(?);
  List_Input(Machine(Z80),DI)==(?);
  List_Input(Machine(Z80),EI)==(?);
  List_Input(Machine(Z80),IM0)==(?);
  List_Input(Machine(Z80),IM1)==(?);
  List_Input(Machine(Z80),IM2)==(?);
  List_Input(Machine(Z80),IN_A_9n0)==(nn);
  List_Input(Machine(Z80),IN_r_9C0)==(rr);
  List_Input(Machine(Z80),INI)==(?);
  List_Input(Machine(Z80),INIR)==(?);
  List_Input(Machine(Z80),IND)==(?);
  List_Input(Machine(Z80),INDR)==(?);
  List_Input(Machine(Z80),OUT_9n0_A)==(nn);
  List_Input(Machine(Z80),OUT_9C0_r)==(rr);
  List_Input(Machine(Z80),OUTI)==(?);
  List_Input(Machine(Z80),OUTIR)==(?);
  List_Input(Machine(Z80),OUTD)==(?);
  List_Input(Machine(Z80),OUTDR)==(?)
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
  List_Output(Machine(Z80),LD_A_I)==(?);
  List_Output(Machine(Z80),LD_A_R)==(?);
  List_Output(Machine(Z80),LD_I_A)==(?);
  List_Output(Machine(Z80),LD_R_A)==(?);
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
  List_Output(Machine(Z80),CPIR)==(?);
  List_Output(Machine(Z80),CPD)==(?);
  List_Output(Machine(Z80),CPDR)==(?);
  List_Output(Machine(Z80),DAA)==(?);
  List_Output(Machine(Z80),CPL)==(?);
  List_Output(Machine(Z80),NEG)==(?);
  List_Output(Machine(Z80),CCF)==(?);
  List_Output(Machine(Z80),SCF)==(?);
  List_Output(Machine(Z80),NOP)==(?);
  List_Output(Machine(Z80),HALT)==(?);
  List_Output(Machine(Z80),DI)==(?);
  List_Output(Machine(Z80),EI)==(?);
  List_Output(Machine(Z80),IM0)==(?);
  List_Output(Machine(Z80),IM1)==(?);
  List_Output(Machine(Z80),IM2)==(?);
  List_Output(Machine(Z80),IN_A_9n0)==(?);
  List_Output(Machine(Z80),IN_r_9C0)==(?);
  List_Output(Machine(Z80),INI)==(?);
  List_Output(Machine(Z80),INIR)==(?);
  List_Output(Machine(Z80),IND)==(?);
  List_Output(Machine(Z80),INDR)==(?);
  List_Output(Machine(Z80),OUT_9n0_A)==(?);
  List_Output(Machine(Z80),OUT_9C0_r)==(?);
  List_Output(Machine(Z80),OUTI)==(?);
  List_Output(Machine(Z80),OUTIR)==(?);
  List_Output(Machine(Z80),OUTD)==(?);
  List_Output(Machine(Z80),OUTDR)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Z80),LD_r_r_)==(LD_r_r_(rr,rr_));
  List_Header(Machine(Z80),LD_r_n_)==(LD_r_n_(rr,n0));
  List_Header(Machine(Z80),LD_r_9HL0)==(LD_r_9HL0(rr));
  List_Header(Machine(Z80),LD_r_9IX_d0)==(LD_r_9IX_d0(rr,desloc));
  List_Header(Machine(Z80),LD_r_9IY_d0)==(LD_r_9IY_d0(rr,desloc));
  List_Header(Machine(Z80),LD_9HL0_r)==(LD_9HL0_r(rr));
  List_Header(Machine(Z80),LD_9IX_d0_r)==(LD_9IX_d0_r(desloc,rr));
  List_Header(Machine(Z80),LD_9IY_d0_r)==(LD_9IY_d0_r(desloc,rr));
  List_Header(Machine(Z80),LD_9HL0_n)==(LD_9HL0_n(n0));
  List_Header(Machine(Z80),LD_9IX_d0_n)==(LD_9IX_d0_n(desloc,n0));
  List_Header(Machine(Z80),LD_9IY_d0_n)==(LD_9IY_d0_n(desloc,n0));
  List_Header(Machine(Z80),LD_A_9BC0)==(LD_A_9BC0);
  List_Header(Machine(Z80),LD_A_9DE0)==(LD_A_9DE0);
  List_Header(Machine(Z80),LD_A_9nn0)==(LD_A_9nn0(nn));
  List_Header(Machine(Z80),LD_9BC0_A)==(LD_9BC0_A);
  List_Header(Machine(Z80),LD_9DE0_A)==(LD_9DE0_A);
  List_Header(Machine(Z80),LD_9nn0_A)==(LD_9nn0_A(nn));
  List_Header(Machine(Z80),LD_A_I)==(LD_A_I);
  List_Header(Machine(Z80),LD_A_R)==(LD_A_R);
  List_Header(Machine(Z80),LD_I_A)==(LD_I_A);
  List_Header(Machine(Z80),LD_R_A)==(LD_R_A);
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
  List_Header(Machine(Z80),CPIR)==(CPIR);
  List_Header(Machine(Z80),CPD)==(CPD);
  List_Header(Machine(Z80),CPDR)==(CPDR);
  List_Header(Machine(Z80),DAA)==(DAA);
  List_Header(Machine(Z80),CPL)==(CPL);
  List_Header(Machine(Z80),NEG)==(NEG);
  List_Header(Machine(Z80),CCF)==(CCF);
  List_Header(Machine(Z80),SCF)==(SCF);
  List_Header(Machine(Z80),NOP)==(NOP);
  List_Header(Machine(Z80),HALT)==(HALT);
  List_Header(Machine(Z80),DI)==(DI);
  List_Header(Machine(Z80),EI)==(EI);
  List_Header(Machine(Z80),IM0)==(IM0);
  List_Header(Machine(Z80),IM1)==(IM1);
  List_Header(Machine(Z80),IM2)==(IM2);
  List_Header(Machine(Z80),IN_A_9n0)==(IN_A_9n0(nn));
  List_Header(Machine(Z80),IN_r_9C0)==(IN_r_9C0(rr));
  List_Header(Machine(Z80),INI)==(INI);
  List_Header(Machine(Z80),INIR)==(INIR);
  List_Header(Machine(Z80),IND)==(IND);
  List_Header(Machine(Z80),INDR)==(INDR);
  List_Header(Machine(Z80),OUT_9n0_A)==(OUT_9n0_A(nn));
  List_Header(Machine(Z80),OUT_9C0_r)==(OUT_9C0_r(rr));
  List_Header(Machine(Z80),OUTI)==(OUTI);
  List_Header(Machine(Z80),OUTIR)==(OUTIR);
  List_Header(Machine(Z80),OUTD)==(OUTD);
  List_Header(Machine(Z80),OUTDR)==(OUTDR)
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Z80),LD_r_r_)==(rr: id_reg_8 & rr_: id_reg_8);
  List_Precondition(Machine(Z80),LD_r_n_)==(rr: id_reg_8 & n0: SCHAR);
  List_Precondition(Machine(Z80),LD_r_9HL0)==(rr: id_reg_8);
  List_Precondition(Machine(Z80),LD_r_9IX_d0)==(rr: id_reg_8 & desloc: SCHAR);
  List_Precondition(Machine(Z80),LD_r_9IY_d0)==(rr: id_reg_8 & desloc: SCHAR);
  List_Precondition(Machine(Z80),LD_9HL0_r)==(rr: id_reg_8 & byte_to_bv16(rgs8(h0),rgs8(l0))/:dom(stack));
  List_Precondition(Machine(Z80),LD_9IX_d0_r)==(desloc: SCHAR & rr: id_reg_8 & bv_ireg_plus_d(ix,desloc)/:dom(stack));
  List_Precondition(Machine(Z80),LD_9IY_d0_r)==(desloc: SCHAR & rr: id_reg_8 & bv_ireg_plus_d(iy,desloc)/:dom(stack));
  List_Precondition(Machine(Z80),LD_9HL0_n)==(n0: SCHAR & byte_to_bv16(rgs8(h0),rgs8(l0))/:dom(stack));
  List_Precondition(Machine(Z80),LD_9IX_d0_n)==(desloc: SCHAR & n0: SCHAR & bv_ireg_plus_d(ix,desloc)/:dom(stack));
  List_Precondition(Machine(Z80),LD_9IY_d0_n)==(desloc: SCHAR & n0: SCHAR & bv_ireg_plus_d(iy,desloc)/:dom(stack));
  List_Precondition(Machine(Z80),LD_A_9BC0)==(btrue);
  List_Precondition(Machine(Z80),LD_A_9DE0)==(btrue);
  List_Precondition(Machine(Z80),LD_A_9nn0)==(nn: USHORTINT);
  List_Precondition(Machine(Z80),LD_9BC0_A)==(byte_to_bv16(rgs8(b0),rgs8(c0))/:dom(stack));
  List_Precondition(Machine(Z80),LD_9DE0_A)==(mem(byte_to_bv16(rgs8(d0),rgs8(e0)))/:dom(stack));
  List_Precondition(Machine(Z80),LD_9nn0_A)==(nn: USHORTINT & ushortint_to_bv16(nn)/:dom(stack));
  List_Precondition(Machine(Z80),LD_A_I)==(btrue);
  List_Precondition(Machine(Z80),LD_A_R)==(btrue);
  List_Precondition(Machine(Z80),LD_I_A)==(btrue);
  List_Precondition(Machine(Z80),LD_R_A)==(btrue);
  List_Precondition(Machine(Z80),LD_dd_nn)==(dd: id_reg_16 & nn: USHORTINT & dd/=AF);
  List_Precondition(Machine(Z80),LD_IX_nn)==(nn: USHORTINT);
  List_Precondition(Machine(Z80),LD_IY_nn)==(nn: USHORTINT);
  List_Precondition(Machine(Z80),LD_HL_9nn0)==(nn: USHORTINT);
  List_Precondition(Machine(Z80),LD_dd_9nn0)==(dd: id_reg_16 & nn: USHORTINT & dd/=AF);
  List_Precondition(Machine(Z80),LD_IX_9nn0)==(nn: USHORTINT);
  List_Precondition(Machine(Z80),LD_IY_9nn0)==(nn: USHORTINT);
  List_Precondition(Machine(Z80),LD_9nn0_HL)==(nn: USHORTINT & ushortint_to_bv16(nn)/:dom(stack) & ushortint_to_bv16(add16USHORTINT(0,nn,1))/:dom(stack));
  List_Precondition(Machine(Z80),LD_9nn0_dd)==(dd: id_reg_16 & dd/=AF & nn: USHORTINT & schar_to_byte(nn)/:dom(stack) & inc_BYTE(schar_to_byte(nn))/:dom(stack));
  List_Precondition(Machine(Z80),LD_9nn0_IX)==(nn: USHORTINT);
  List_Precondition(Machine(Z80),LD_9nn0_IY)==(nn: USHORTINT);
  List_Precondition(Machine(Z80),LD_SP_HL)==(btrue);
  List_Precondition(Machine(Z80),LD_SP_IX)==(btrue);
  List_Precondition(Machine(Z80),LD_SP_IY)==(btrue);
  List_Precondition(Machine(Z80),PUSH_qq)==(qq: id_reg_16 & qq/=SP);
  List_Precondition(Machine(Z80),PUSH_IX)==(btrue);
  List_Precondition(Machine(Z80),PUSH_IY)==(btrue);
  List_Precondition(Machine(Z80),POP_qq)==(qq: id_reg_16 & qq/=SP);
  List_Precondition(Machine(Z80),POP_IX)==(sp: BV16);
  List_Precondition(Machine(Z80),POP_IY)==(sp: BV16);
  List_Precondition(Machine(Z80),EX_DE_HL)==(btrue);
  List_Precondition(Machine(Z80),EX_AF_AF_)==(btrue);
  List_Precondition(Machine(Z80),EXX)==(btrue);
  List_Precondition(Machine(Z80),EX_9SP0_HL)==(stack<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->rgs8(h0),sp|->rgs8(l0)} <: mem);
  List_Precondition(Machine(Z80),EX_9SP0_IX)==(btrue);
  List_Precondition(Machine(Z80),EX_9SP0_IY)==(btrue);
  List_Precondition(Machine(Z80),LDI)==(btrue);
  List_Precondition(Machine(Z80),LDIR)==(btrue);
  List_Precondition(Machine(Z80),LDD)==(btrue);
  List_Precondition(Machine(Z80),LDDR)==(btrue);
  List_Precondition(Machine(Z80),CPI)==(btrue);
  List_Precondition(Machine(Z80),CPIR)==(btrue);
  List_Precondition(Machine(Z80),CPD)==(btrue);
  List_Precondition(Machine(Z80),CPDR)==(btrue);
  List_Precondition(Machine(Z80),DAA)==(btrue);
  List_Precondition(Machine(Z80),CPL)==(btrue);
  List_Precondition(Machine(Z80),NEG)==(btrue);
  List_Precondition(Machine(Z80),CCF)==(btrue);
  List_Precondition(Machine(Z80),SCF)==(btrue);
  List_Precondition(Machine(Z80),NOP)==(btrue);
  List_Precondition(Machine(Z80),HALT)==(btrue);
  List_Precondition(Machine(Z80),DI)==(btrue);
  List_Precondition(Machine(Z80),EI)==(btrue);
  List_Precondition(Machine(Z80),IM0)==(btrue);
  List_Precondition(Machine(Z80),IM1)==(btrue);
  List_Precondition(Machine(Z80),IM2)==(btrue);
  List_Precondition(Machine(Z80),IN_A_9n0)==(nn: UCHAR);
  List_Precondition(Machine(Z80),IN_r_9C0)==(rr: id_reg_8);
  List_Precondition(Machine(Z80),INI)==(btrue);
  List_Precondition(Machine(Z80),INIR)==(btrue);
  List_Precondition(Machine(Z80),IND)==(btrue);
  List_Precondition(Machine(Z80),INDR)==(btrue);
  List_Precondition(Machine(Z80),OUT_9n0_A)==(nn: UCHAR);
  List_Precondition(Machine(Z80),OUT_9C0_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80),OUTI)==(btrue);
  List_Precondition(Machine(Z80),OUTIR)==(btrue);
  List_Precondition(Machine(Z80),OUTD)==(btrue);
  List_Precondition(Machine(Z80),OUTDR)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Z80),OUTDR)==(btrue | @(hvn,lvn,negative,zero,half_carry,pv,add_sub,carry).(negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & hvn,lvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & negative = is_negative(mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) & zero = is_zero(dec_BYTE(rgs8(b0))) & half_carry = bitget(rgs8(f0),4) & pv = parity_even_BYTE(mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) & add_sub = 1 & carry = bitget(rgs8(f0),0) ==> (byte_to_bv16(rgs8(h0),rgs8(l0)): BV16 & i_o_ports(rgs8(c0)): BYTE & dom({byte_to_bv16(rgs8(h0),rgs8(l0))|->i_o_ports(rgs8(c0))})/\dom(stack) = {} & not(byte_to_bv16(rgs8(h0),rgs8(l0))|->i_o_ports(rgs8(c0)): stack) | i_o_ports:=i_o_ports<+{rgs8(c0)|->mem(byte_to_bv16(rgs8(h0),rgs8(l0)))} || mem:=mem<+{byte_to_bv16(rgs8(h0),rgs8(l0))|->i_o_ports(rgs8(c0))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->dec_BYTE(rgs8(b0)),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || r_:=update_refresh_reg(r_) || (is_zero(dec_BYTE(rgs8(b0))) = 1 ==> pc:=instruction_next(pc) [] not(is_zero(dec_BYTE(rgs8(b0))) = 1) ==> skip))));
  Expanded_List_Substitution(Machine(Z80),OUTD)==(btrue | @(hvn,lvn,negative,zero,half_carry,pv,add_sub,carry).(negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & hvn,lvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & negative = is_negative(mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) & zero = is_zero(dec_BYTE(rgs8(b0))) & half_carry = bitget(rgs8(f0),4) & pv = parity_even_BYTE(mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) & add_sub = 1 & carry = bitget(rgs8(f0),0) ==> (byte_to_bv16(rgs8(h0),rgs8(l0)): BV16 & i_o_ports(rgs8(c0)): BYTE & dom({byte_to_bv16(rgs8(h0),rgs8(l0))|->i_o_ports(rgs8(c0))})/\dom(stack) = {} & not(byte_to_bv16(rgs8(h0),rgs8(l0))|->i_o_ports(rgs8(c0)): stack) | i_o_ports:=i_o_ports<+{rgs8(c0)|->mem(byte_to_bv16(rgs8(h0),rgs8(l0)))} || mem:=mem<+{byte_to_bv16(rgs8(h0),rgs8(l0))|->i_o_ports(rgs8(c0))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->dec_BYTE(rgs8(b0)),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),OUTIR)==(btrue | @(hvn,lvn,negative,zero,half_carry,pv,add_sub,carry).(negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & hvn,lvn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & negative = is_negative(mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) & zero = is_zero(dec_BYTE(rgs8(b0))) & half_carry = bitget(rgs8(f0),4) & pv = parity_even_BYTE(mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) & add_sub = 1 & carry = bitget(rgs8(f0),0) ==> (byte_to_bv16(rgs8(h0),rgs8(l0)): BV16 & i_o_ports(rgs8(c0)): BYTE & dom({byte_to_bv16(rgs8(h0),rgs8(l0))|->i_o_ports(rgs8(c0))})/\dom(stack) = {} & not(byte_to_bv16(rgs8(h0),rgs8(l0))|->i_o_ports(rgs8(c0)): stack) | i_o_ports:=i_o_ports<+{rgs8(c0)|->mem(byte_to_bv16(rgs8(h0),rgs8(l0)))} || mem:=mem<+{byte_to_bv16(rgs8(h0),rgs8(l0))|->i_o_ports(rgs8(c0))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->dec_BYTE(rgs8(b0)),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || r_:=update_refresh_reg(r_) || (is_zero(dec_BYTE(rgs8(b0))) = 1 ==> pc:=instruction_next(pc) [] not(is_zero(dec_BYTE(rgs8(b0))) = 1) ==> skip))));
  Expanded_List_Substitution(Machine(Z80),OUTI)==(btrue | @(hvn,lvn,negative,zero,half_carry,pv,add_sub,carry).(negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & hvn,lvn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & negative = is_negative(mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) & zero = is_zero(dec_BYTE(rgs8(b0))) & half_carry = bitget(rgs8(f0),4) & pv = parity_even_BYTE(mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) & add_sub = 1 & carry = bitget(rgs8(f0),0) ==> (byte_to_bv16(rgs8(h0),rgs8(l0)): BV16 & i_o_ports(rgs8(c0)): BYTE & dom({byte_to_bv16(rgs8(h0),rgs8(l0))|->i_o_ports(rgs8(c0))})/\dom(stack) = {} & not(byte_to_bv16(rgs8(h0),rgs8(l0))|->i_o_ports(rgs8(c0)): stack) | i_o_ports:=i_o_ports<+{rgs8(c0)|->mem(byte_to_bv16(rgs8(h0),rgs8(l0)))} || mem:=mem<+{byte_to_bv16(rgs8(h0),rgs8(l0))|->i_o_ports(rgs8(c0))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->dec_BYTE(rgs8(b0)),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),OUT_9C0_r)==(rr: id_reg_8 | i_o_ports,pc,r_:=i_o_ports<+{rgs8(rr)|->rgs8(c0)},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),OUT_9n0_A)==(nn: UCHAR | i_o_ports,pc,r_:=i_o_ports<+{uchar_to_byte(nn)|->rgs8(a0)},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),INDR)==(btrue | @(data_in,hvn,lvn,negative,zero,half_carry,pv,add_sub,carry).(data_in: BYTE & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & hvn,lvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & negative = is_negative(data_in) & zero = is_zero(dec_BYTE(rgs8(b0))) & half_carry = bitget(rgs8(f0),4) & pv = parity_even_BYTE(data_in) & add_sub = 1 & carry = bitget(rgs8(f0),0) ==> (byte_to_bv16(rgs8(h0),rgs8(l0)): BV16 & data_in: BYTE & dom({byte_to_bv16(rgs8(h0),rgs8(l0))|->data_in})/\dom(stack) = {} & not(byte_to_bv16(rgs8(h0),rgs8(l0))|->data_in: stack) | i_o_ports:=i_o_ports<+{rgs8(c0)|->data_in} || mem:=mem<+{byte_to_bv16(rgs8(h0),rgs8(l0))|->data_in} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->dec_BYTE(rgs8(b0)),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || (is_zero(dec_BYTE(rgs8(b0))) = 1 ==> pc,r_:=instruction_next(pc),update_refresh_reg(r_) [] not(is_zero(dec_BYTE(rgs8(b0))) = 1) ==> skip))));
  Expanded_List_Substitution(Machine(Z80),IND)==(btrue | @(data_in,hvn,lvn,negative,zero,half_carry,pv,add_sub,carry).(data_in: BYTE & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & hvn,lvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & negative = is_negative(data_in) & zero = is_zero(dec_BYTE(rgs8(b0))) & half_carry = bitget(rgs8(f0),4) & pv = parity_even_BYTE(data_in) & add_sub = 1 & carry = bitget(rgs8(f0),0) ==> (byte_to_bv16(rgs8(h0),rgs8(l0)): BV16 & data_in: BYTE & dom({byte_to_bv16(rgs8(h0),rgs8(l0))|->data_in})/\dom(stack) = {} & not(byte_to_bv16(rgs8(h0),rgs8(l0))|->data_in: stack) | i_o_ports:=i_o_ports<+{rgs8(c0)|->data_in} || mem:=mem<+{byte_to_bv16(rgs8(h0),rgs8(l0))|->data_in} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->dec_BYTE(rgs8(b0)),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),INIR)==(btrue | @(data_in,hvn,lvn,negative,zero,half_carry,pv,add_sub,carry).(data_in: BYTE & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & hvn,lvn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & negative = is_negative(data_in) & zero = 1 & half_carry = bitget(rgs8(f0),4) & pv = parity_even_BYTE(data_in) & add_sub = 1 & carry = bitget(rgs8(f0),0) ==> (byte_to_bv16(rgs8(h0),rgs8(l0)): BV16 & data_in: BYTE & dom({byte_to_bv16(rgs8(h0),rgs8(l0))|->data_in})/\dom(stack) = {} & not(byte_to_bv16(rgs8(h0),rgs8(l0))|->data_in: stack) | i_o_ports:=i_o_ports<+{rgs8(c0)|->data_in} || mem:=mem<+{byte_to_bv16(rgs8(h0),rgs8(l0))|->data_in} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->dec_BYTE(rgs8(b0)),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || r_:=update_refresh_reg(r_) || (is_zero(dec_BYTE(rgs8(b0))) = 1 ==> pc:=instruction_next(pc) [] not(is_zero(dec_BYTE(rgs8(b0))) = 1) ==> skip))));
  Expanded_List_Substitution(Machine(Z80),INI)==(btrue | @(data_in,hvn,lvn,negative,zero,half_carry,pv,add_sub,carry).(data_in: BYTE & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & hvn,lvn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & negative = is_negative(data_in) & zero = is_zero(dec_BYTE(rgs8(b0))) & half_carry = bitget(rgs8(f0),4) & pv = parity_even_BYTE(data_in) & add_sub = 1 & carry = bitget(rgs8(f0),0) ==> (byte_to_bv16(rgs8(h0),rgs8(l0)): BV16 & i_o_ports(rgs8(c0)): BYTE & dom({byte_to_bv16(rgs8(h0),rgs8(l0))|->i_o_ports(rgs8(c0))})/\dom(stack) = {} & not(byte_to_bv16(rgs8(h0),rgs8(l0))|->i_o_ports(rgs8(c0)): stack) | i_o_ports:=i_o_ports<+{rgs8(c0)|->data_in} || mem:=mem<+{byte_to_bv16(rgs8(h0),rgs8(l0))|->i_o_ports(rgs8(c0))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->dec_BYTE(rgs8(b0)),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),IN_r_9C0)==(rr: id_reg_8 | @(data_in,negative,zero,half_carry,pv,add_sub,carry).(data_in: BYTE & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & negative = is_negative(data_in) & zero = is_zero(data_in) & half_carry = 0 & pv = parity_even_BYTE(data_in) & add_sub = 0 & carry = bitget(rgs8(f0),0) ==> i_o_ports,rgs8,pc,r_:=i_o_ports<+{rgs8(c0)|->data_in},rgs8<+{rr|->data_in,get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)},instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80),IN_A_9n0)==(nn: UCHAR | @data_in.(data_in: SCHAR ==> i_o_ports,rgs8,pc,r_:=i_o_ports<+{uchar_to_byte(nn)|->schar_to_byte(data_in)},rgs8<+{a0|->schar_to_byte(data_in)},instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80),IM2)==(btrue | im,pc,r_:=1|->1,instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),IM1)==(btrue | im,pc,r_:=1|->0,instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),IM0)==(btrue | im,pc,r_:=0|->0,instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),EI)==(btrue | iff1,iff2,pc,r_:=1,1,instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),DI)==(btrue | iff1,iff2,pc,r_:=0,0,instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),HALT)==(btrue | r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),NOP)==(btrue | pc,r_:=instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),SCF)==(btrue | rgs8,pc,r_:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),bitget(rgs8(f0),4),bitget(rgs8(f0),2),0,1)},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),CCF)==(btrue | rgs8,pc,r_:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),bitget(rgs8(f0),4),bitget(rgs8(f0),2),0,bit_not(bitget(rgs8(f0),0)))},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),NEG)==(btrue | @(sum,negative,carry,half_carry,zero).(sum: UCHAR & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & sum,negative,carry,half_carry,zero = substract8UCHAR(0,0,byte_to_uchar(rgs8(a0))) ==> rgs8,pc,r_:=rgs8<+{a0|->uchar_to_byte(sum),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,carry,1,carry)},instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80),CPL)==(btrue | @result.(result: BYTE & result = complement(rgs8(a0)) ==> rgs8,pc,r_:=rgs8<+{a0|->result,get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),1,bitget(rgs8(f0),2),1,bitget(rgs8(f0),0))},instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80),DAA)==(btrue | @(result,s0,z0,h0,pv0,n0,c0).(result: BYTE & s0: BIT & z0: BIT & h0: BIT & pv0: BIT & n0: BIT & c0: BIT & daa_function(bitget(rgs8(f0),1),bitget(rgs8(f0),0),rgs8(a0),bitget(rgs8(f0),4)) = result,c0,h0 & s0 = bv_get(result,7) & z0 = is_zero(result) & pv0 = parity_even_BYTE(result) & n0 = bitget(rgs8(f0),1) ==> rgs8,pc,r_:=rgs8<+{a0|->result,get_new_flag_register_SZ_H_PvNC(rgs8,s0,z0,h0,pv0,n0,c0)},instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80),CPDR)==(btrue | @(sum,negative,carry,half_carry,zero,hvn,lvn,bvn,cvn).(sum: UCHAR & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & substract8UCHAR(0,byte_to_uchar(rgs8(a0)),byte_to_uchar(mem(byte_to_bv16(rgs8(h0),rgs8(l0))))) = sum,negative,carry,half_carry,zero & hvn,lvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & bvn,cvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(b0),rgs8(c0)))) ==> (zero = 1 ==> rgs8:={h0|->hvn,l0|->lvn,b0|->bvn,c0|->cvn,get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,bit_not(is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1)),1,bitget(rgs8(f0),0))} [] not(zero = 1) ==> skip || (is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1) = 0 or zero = 1 ==> pc:=instruction_next(pc) [] not(is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1) = 0 or zero = 1) ==> skip) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),CPD)==(btrue | @(sum,negative,carry,half_carry,zero,hvn,lvn,bvn,cvn).(sum: UCHAR & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & substract8UCHAR(0,byte_to_uchar(rgs8(a0)),byte_to_uchar(mem(byte_to_bv16(rgs8(h0),rgs8(l0))))) = sum,negative,carry,half_carry,zero & hvn,lvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & bvn,cvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(b0),rgs8(c0)))) ==> (zero = 1 ==> rgs8:={h0|->hvn,l0|->lvn,b0|->bvn,c0|->cvn,get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,bit_not(is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1)),1,bitget(rgs8(f0),0))} [] not(zero = 1) ==> skip || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),CPIR)==(btrue | @(sum,negative,carry,half_carry,zero,hvn,lvn,bvn,cvn).(sum: UCHAR & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & substract8UCHAR(0,byte_to_uchar(rgs8(a0)),byte_to_uchar(mem(byte_to_bv16(rgs8(h0),rgs8(l0))))) = sum,negative,carry,half_carry,zero & hvn,lvn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & bvn,cvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(b0),rgs8(c0)))) ==> (zero = 1 ==> rgs8:={h0|->hvn,l0|->lvn,b0|->bvn,c0|->cvn,get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,bit_not(is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1)),1,bitget(rgs8(f0),0))} [] not(zero = 1) ==> skip || r_:=update_refresh_reg(r_) || (is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1) = 0 or zero = 1 ==> pc:=instruction_next(pc) [] not(is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1) = 0 or zero = 1) ==> skip))));
  Expanded_List_Substitution(Machine(Z80),CPI)==(btrue | @(sum,negative,carry,half_carry,zero,hvn,lvn,bvn,cvn).(sum: UCHAR & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & substract8UCHAR(0,byte_to_uchar(rgs8(a0)),byte_to_uchar(mem(byte_to_bv16(rgs8(h0),rgs8(l0))))) = sum,negative,carry,half_carry,zero & hvn,lvn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & bvn,cvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(b0),rgs8(c0)))) ==> (zero = 1 ==> rgs8:={h0|->hvn,l0|->lvn,b0|->bvn,c0|->cvn,get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,bit_not(is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1)),1,bitget(rgs8(f0),0))} [] not(zero = 1) ==> skip || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),LDDR)==(btrue | @(hvn,lvn,dvn,evn,bvn,cvn).(hvn,lvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & dvn,evn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(d0),rgs8(e0)))) & bvn,cvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(b0),rgs8(c0)))) ==> (byte_to_bv16(rgs8(d0),rgs8(e0)): BV16 & mem(byte_to_bv16(rgs8(h0),rgs8(l0))): BYTE & dom({byte_to_bv16(rgs8(d0),rgs8(e0))|->mem(byte_to_bv16(rgs8(h0),rgs8(l0)))})/\dom(stack) = {} & not(byte_to_bv16(rgs8(d0),rgs8(e0))|->mem(byte_to_bv16(rgs8(h0),rgs8(l0))): stack) | mem:=mem<+{byte_to_bv16(rgs8(d0),rgs8(e0))|->mem(byte_to_bv16(rgs8(h0),rgs8(l0)))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,d0|->dvn,e0|->evn,b0|->bvn,c0|->cvn,get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,0,0,bitget(rgs8(f0),0))} || r_:=update_refresh_reg(r_) || (is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1) = 0 ==> pc:=instruction_next(pc) [] not(is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1) = 0) ==> skip))));
  Expanded_List_Substitution(Machine(Z80),LDD)==(btrue | @(hvn,lvn,dvn,evn,bvn,cvn).(hvn,lvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & dvn,evn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(d0),rgs8(e0)))) & bvn,cvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(b0),rgs8(c0)))) ==> (byte_to_bv16(rgs8(d0),rgs8(e0)): BV16 & mem(byte_to_bv16(rgs8(h0),rgs8(l0))): BYTE & dom({byte_to_bv16(rgs8(d0),rgs8(e0))|->mem(byte_to_bv16(rgs8(h0),rgs8(l0)))})/\dom(stack) = {} & not(byte_to_bv16(rgs8(d0),rgs8(e0))|->mem(byte_to_bv16(rgs8(h0),rgs8(l0))): stack) | mem:=mem<+{byte_to_bv16(rgs8(d0),rgs8(e0))|->mem(byte_to_bv16(rgs8(h0),rgs8(l0)))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,d0|->dvn,e0|->evn,b0|->bvn,c0|->cvn,get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bit_not(is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1)),0,bitget(rgs8(f0),0))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),LDIR)==(btrue | @(hvn,lvn,dvn,evn,bvn,cvn).(hvn,lvn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & dvn,evn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(d0),rgs8(e0)))) & bvn,cvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(b0),rgs8(c0)))) ==> (byte_to_bv16(rgs8(d0),rgs8(e0)): BV16 & mem(byte_to_bv16(rgs8(h0),rgs8(l0))): BYTE & dom({byte_to_bv16(rgs8(d0),rgs8(e0))|->mem(byte_to_bv16(rgs8(h0),rgs8(l0)))})/\dom(stack) = {} & not(byte_to_bv16(rgs8(d0),rgs8(e0))|->mem(byte_to_bv16(rgs8(h0),rgs8(l0))): stack) | mem:=mem<+{byte_to_bv16(rgs8(d0),rgs8(e0))|->mem(byte_to_bv16(rgs8(h0),rgs8(l0)))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,d0|->dvn,e0|->evn,b0|->bvn,c0|->cvn,get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,0,0,bitget(rgs8(f0),0))} || r_:=update_refresh_reg(r_) || (is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1) = 0 ==> pc:=instruction_next(pc) [] not(is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1) = 0) ==> skip))));
  Expanded_List_Substitution(Machine(Z80),LDI)==(btrue | @(hvn,lvn,dvn,evn,bvn,cvn).(hvn,lvn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & dvn,evn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(d0),rgs8(e0)))) & bvn,cvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(b0),rgs8(c0)))) ==> (byte_to_bv16(rgs8(d0),rgs8(e0)): BV16 & mem(byte_to_bv16(rgs8(h0),rgs8(l0))): BYTE & dom({byte_to_bv16(rgs8(d0),rgs8(e0))|->mem(byte_to_bv16(rgs8(h0),rgs8(l0)))})/\dom(stack) = {} & not(byte_to_bv16(rgs8(d0),rgs8(e0))|->mem(byte_to_bv16(rgs8(h0),rgs8(l0))): stack) | mem:=mem<+{byte_to_bv16(rgs8(d0),rgs8(e0))|->mem(byte_to_bv16(rgs8(h0),rgs8(l0)))} || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,d0|->dvn,e0|->evn,b0|->bvn,c0|->cvn,get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bit_not(is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1)),0,bitget(rgs8(f0),0))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),EX_9SP0_IY)==(btrue | @(wh,wl).(wh: BYTE & wl: BYTE & bv16_to_byte(iy) = wh,wl & {ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->wh,sp|->wl}: BV16 --> BYTE & stack<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->wh,sp|->wl} <: mem ==> ({ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->wh,sp|->wl}: BV16 --> BYTE | iy:=byte_to_bv16(stack(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))),stack(sp)) || stack,mem:=stack<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->wh,sp|->wl},mem<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->wh,sp|->wl} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),EX_9SP0_IX)==(btrue | @(wh,wl).(wh: BYTE & wl: BYTE & bv16_to_byte(ix) = wh,wl & {ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->wh,sp|->wl}: BV16 --> BYTE & stack<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->wh,sp|->wl} <: mem ==> ({ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->wh,sp|->wl}: BV16 --> BYTE | ix:=byte_to_bv16(stack(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))),stack(sp)) || stack,mem:=stack<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->wh,sp|->wl},mem<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->wh,sp|->wl} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),EX_9SP0_HL)==(stack<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->rgs8(h0),sp|->rgs8(l0)} <: mem & {ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->rgs8(h0),sp|->rgs8(l0)}: BV16 --> BYTE | rgs8:=rgs8<+{h0|->stack(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))),l0|->stack(sp)} || stack,mem:=stack<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->rgs8(h0),sp|->rgs8(l0)},mem<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->rgs8(h0),sp|->rgs8(l0)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),EXX)==(btrue | rgs8,pc,r_:=rgs8<+{b0|->rgs8(b_0),c0|->rgs8(c_0),d0|->rgs8(d_0),e0|->rgs8(e_0),h0|->rgs8(h_0),l0|->rgs8(l_0),b_0|->rgs8(b0),c_0|->rgs8(c0),d_0|->rgs8(d0),e_0|->rgs8(e0),h_0|->rgs8(h0),l_0|->rgs8(l0)},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),EX_AF_AF_)==(btrue | rgs8,pc,r_:=rgs8<+{a0|->rgs8(a_0),f0|->rgs8(f_0),a_0|->rgs8(a0),f_0|->rgs8(f0)},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),EX_DE_HL)==(btrue | rgs8,pc,r_:=rgs8<+{d0|->rgs8(h0),e0|->rgs8(l0),h0|->rgs8(d0),l0|->rgs8(e0)},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),POP_IY)==(sp: BV16 | @nw8.(nw8: BYTE & byte_to_bv16(mem(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))),mem(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),2)))) = nw8 ==> iy,sp,pc,r_:=nw8,ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),2)),instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80),POP_IX)==(sp: BV16 | @nw8.(nw8: BYTE & byte_to_bv16(mem(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))),mem(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),2)))) = nw8 ==> ix,sp,pc,r_:=nw8,ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),2)),instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80),POP_qq)==(qq: id_reg_16 & qq/=SP | @(qqh,qql).(qqh: id_reg_8 & qql: id_reg_8 & REG16_TO_REG8(qq) = qqh,qql & {qql|->mem(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),2))),qqh|->mem(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1)))}: id_reg_8 --> BYTE ==> rgs8,sp,pc,r_:=rgs8<+{qql|->mem(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),2))),qqh|->mem(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1)))},ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),2)),instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80),PUSH_IY)==(btrue | @(wh,wl).(wh: BYTE & wl: BYTE & bv16_to_byte(iy) = wh,wl & {ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->wl,ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->wh}: BV16 --> BYTE ==> ({ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->wl,ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->wh}: BV16 --> BYTE | stack,mem:=stack<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->wl,ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->wh},mem<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->wl,ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->wh} || sp:=ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),PUSH_IX)==(btrue | @(wh,wl).(wh: BYTE & wl: BYTE & bv16_to_byte(ix) = wh,wl & {ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->wl,ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->wh}: BV16 --> BYTE ==> ({ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->wl,ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->wh}: BV16 --> BYTE | stack,mem:=stack<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->wl,ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->wh},mem<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->wl,ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->wh} || sp:=ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),PUSH_qq)==(qq: id_reg_16 & qq/=SP | @(qqh,qql).(qqh: id_reg_8 & qql: id_reg_8 & REG16_TO_REG8(qq) = qqh,qql & {ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->rgs8(qql),ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->rgs8(qqh)}: BV16 --> BYTE ==> ({ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->rgs8(qql),ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->rgs8(qqh)}: BV16 --> BYTE | stack,mem:=stack<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->rgs8(qql),ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->rgs8(qqh)},mem<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->rgs8(qql),ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->rgs8(qqh)} || sp:=ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),LD_SP_IY)==(btrue | sp,pc,r_:=iy,instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_SP_IX)==(btrue | sp,pc,r_:=ix,instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_SP_HL)==(btrue | sp,pc,r_:=byte_to_bv16(rgs8(h0),rgs8(l0)),instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_9nn0_IY)==(nn: USHORTINT | @(h_iy,l_iy).(h_iy: BYTE & l_iy: BYTE & h_iy,l_iy = bv16_to_byte(iy) & ushortint_to_bv16(nn)/:dom(stack) & ushortint_to_bv16(add16USHORTINT(0,nn,1))/:dom(stack) ==> ({ushortint_to_bv16(add16USHORTINT(0,nn,1))|->h_iy,ushortint_to_bv16(nn)|->l_iy}: BV16 --> BYTE & dom({ushortint_to_bv16(add16USHORTINT(0,nn,1))|->h_iy,ushortint_to_bv16(nn)|->l_iy})/\dom(stack) = {} | mem:=mem<+{ushortint_to_bv16(add16USHORTINT(0,nn,1))|->h_iy,ushortint_to_bv16(nn)|->l_iy} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),LD_9nn0_IX)==(nn: USHORTINT | @(h_ix,l_ix).(h_ix: BYTE & l_ix: BYTE & h_ix,l_ix = bv16_to_byte(ix) & ushortint_to_bv16(nn)/:dom(stack) & ushortint_to_bv16(add16USHORTINT(0,nn,1))/:dom(stack) ==> ({ushortint_to_bv16(add16USHORTINT(0,nn,1))|->h_ix,ushortint_to_bv16(nn)|->l_ix}: BV16 --> BYTE & dom({ushortint_to_bv16(add16USHORTINT(0,nn,1))|->h_ix,ushortint_to_bv16(nn)|->l_ix})/\dom(stack) = {} | mem:=mem<+{ushortint_to_bv16(add16USHORTINT(0,nn,1))|->h_ix,ushortint_to_bv16(nn)|->l_ix} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),LD_9nn0_dd)==(dd: id_reg_16 & dd/=AF & nn: USHORTINT & schar_to_byte(nn)/:dom(stack) & inc_BYTE(schar_to_byte(nn))/:dom(stack) | dd = SP ==> @(vh,vl).(vh: BYTE & vl: BYTE & bv16_to_byte(sp) = vh,vl & {ushortint_to_bv16(add16USHORTINT(0,nn,1))|->vh,ushortint_to_bv16(nn)|->vl}: BV16 >-> BYTE ==> ({ushortint_to_bv16(add16USHORTINT(0,nn,1))|->vh,ushortint_to_bv16(nn)|->vl}: BV16 --> BYTE & dom({ushortint_to_bv16(add16USHORTINT(0,nn,1))|->vh,ushortint_to_bv16(nn)|->vl})/\dom(stack) = {} | mem:=mem<+{ushortint_to_bv16(add16USHORTINT(0,nn,1))|->vh,ushortint_to_bv16(nn)|->vl} || r_:=update_refresh_reg(r_))) [] not(dd = SP) ==> @(rh,rl,w1,w2).(rh: id_reg_8 & rl: id_reg_8 & w1: SCHAR & w2: BYTE & rh,rl = REG16_TO_REG8(dd) & {ushortint_to_bv16(add16USHORTINT(0,nn,1))|->rgs8(rh),ushortint_to_bv16(nn)|->rgs8(rl)}: BV16 >-> BYTE ==> ({ushortint_to_bv16(add16USHORTINT(0,nn,1))|->rgs8(rh),ushortint_to_bv16(nn)|->rgs8(rl)}: BV16 --> BYTE & dom({ushortint_to_bv16(add16USHORTINT(0,nn,1))|->rgs8(rh),ushortint_to_bv16(nn)|->rgs8(rl)})/\dom(stack) = {} | mem:=mem<+{ushortint_to_bv16(add16USHORTINT(0,nn,1))|->rgs8(rh),ushortint_to_bv16(nn)|->rgs8(rl)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_))));
  Expanded_List_Substitution(Machine(Z80),LD_9nn0_HL)==(nn: USHORTINT & ushortint_to_bv16(nn)/:dom(stack) & ushortint_to_bv16(add16USHORTINT(0,nn,1))/:dom(stack) & {ushortint_to_bv16(add16USHORTINT(0,nn,1))|->rgs8(h0),ushortint_to_bv16(nn)|->rgs8(l0)}: BV16 --> BYTE & dom({ushortint_to_bv16(add16USHORTINT(0,nn,1))|->rgs8(h0),ushortint_to_bv16(nn)|->rgs8(l0)})/\dom(stack) = {} | mem:=mem<+{ushortint_to_bv16(add16USHORTINT(0,nn,1))|->rgs8(h0),ushortint_to_bv16(nn)|->rgs8(l0)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_IY_9nn0)==(nn: USHORTINT | iy,pc,r_:=byte_to_bv16(mem(ushortint_to_bv16(add16USHORTINT(0,nn,1))),mem(ushortint_to_bv16(nn))),instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_IX_9nn0)==(nn: USHORTINT | ix,pc,r_:=byte_to_bv16(mem(ushortint_to_bv16(add16USHORTINT(0,nn,1))),mem(ushortint_to_bv16(nn))),instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_dd_9nn0)==(dd: id_reg_16 & nn: USHORTINT & dd/=AF | r_:=update_refresh_reg(r_) || (dd = SP ==> sp:=byte_to_bv16(mem(ushortint_to_bv16(add16USHORTINT(0,nn,1))),mem(ushortint_to_bv16(nn))) [] not(dd = SP) ==> @(rh,rl,w1,w2).(rh: id_reg_8 & rl: id_reg_8 & w1: BYTE & w2: BYTE & rh,rl = REG16_TO_REG8(dd) & w1 = mem(ushortint_to_bv16(add16USHORTINT(0,nn,1))) & w2 = mem(ushortint_to_bv16(nn)) ==> rgs8,pc:=rgs8<+{rh|->w1,rl|->w2},instruction_next(pc))));
  Expanded_List_Substitution(Machine(Z80),LD_HL_9nn0)==(nn: USHORTINT | rgs8,pc,r_:=rgs8<+{h0|->mem(ushortint_to_bv16(add16USHORTINT(0,nn,1))),l0|->mem(ushortint_to_bv16(nn))},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_IY_nn)==(nn: USHORTINT | iy,pc,r_:=ushortint_to_bv16(nn),instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_IX_nn)==(nn: USHORTINT | ix,pc,r_:=ushortint_to_bv16(nn),instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_dd_nn)==(dd: id_reg_16 & nn: USHORTINT & dd/=AF | dd = SP ==> sp:=ushortint_to_bv16(nn) [] not(dd = SP) ==> @(rh,rl,w1,w2).(rh: id_reg_8 & rl: id_reg_8 & w1: BYTE & w2: BYTE & rh,rl = REG16_TO_REG8(dd) & bv16_to_byte(ushortint_to_bv16(nn)) = w1,w2 ==> rgs8,pc,r_:=rgs8<+{rh|->w1,rl|->w2},instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80),LD_R_A)==(btrue | r_,pc:=rgs8(a0),instruction_next(pc));
  Expanded_List_Substitution(Machine(Z80),LD_I_A)==(btrue | i_,pc,r_:=rgs8(a0),instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_A_R)==(btrue | rgs8,pc,r_:=rgs8<+{a0|->r_,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(r_),is_zero(r_),0,iff2,0,bitget(rgs8(f0),0))},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_A_I)==(btrue | @new_interrupt.(new_interrupt: BYTE ==> i_,rgs8,pc,r_:=new_interrupt,rgs8<+{a0|->new_interrupt,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(new_interrupt),is_zero(new_interrupt),0,iff2,0,bitget(rgs8(f0),0))},instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80),LD_9nn0_A)==(nn: USHORTINT & ushortint_to_bv16(nn)/:dom(stack) & ushortint_to_bv16(nn): BV16 & rgs8(a0): BYTE & dom({ushortint_to_bv16(nn)|->rgs8(a0)})/\dom(stack) = {} & not(ushortint_to_bv16(nn)|->rgs8(a0): stack) | mem:=mem<+{ushortint_to_bv16(nn)|->rgs8(a0)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_9DE0_A)==(mem(byte_to_bv16(rgs8(d0),rgs8(e0)))/:dom(stack) & byte_to_bv16(rgs8(d0),rgs8(e0)): BV16 & rgs8(a0): BYTE & dom({byte_to_bv16(rgs8(d0),rgs8(e0))|->rgs8(a0)})/\dom(stack) = {} & not(byte_to_bv16(rgs8(d0),rgs8(e0))|->rgs8(a0): stack) | mem:=mem<+{byte_to_bv16(rgs8(d0),rgs8(e0))|->rgs8(a0)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_9BC0_A)==(byte_to_bv16(rgs8(b0),rgs8(c0))/:dom(stack) & byte_to_bv16(rgs8(b0),rgs8(c0)): BV16 & rgs8(a0): BYTE & dom({byte_to_bv16(rgs8(b0),rgs8(c0))|->rgs8(a0)})/\dom(stack) = {} & not(byte_to_bv16(rgs8(b0),rgs8(c0))|->rgs8(a0): stack) | mem:=mem<+{byte_to_bv16(rgs8(b0),rgs8(c0))|->rgs8(a0)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_A_9nn0)==(nn: USHORTINT | rgs8,pc,r_:=rgs8<+{a0|->mem(ushortint_to_bv16(nn))},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_A_9DE0)==(btrue | rgs8,pc,r_:=rgs8<+{a0|->mem(byte_to_bv16(rgs8(d0),rgs8(e0)))},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_A_9BC0)==(btrue | rgs8,pc,r_:=rgs8<+{a0|->mem(mem(byte_to_bv16(rgs8(b0),rgs8(c0))))},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_9IY_d0_n)==(desloc: SCHAR & n0: SCHAR & bv_ireg_plus_d(iy,desloc)/:dom(stack) & bv_ireg_plus_d(iy,desloc): BV16 & schar_to_byte(n0): BYTE & dom({bv_ireg_plus_d(iy,desloc)|->schar_to_byte(n0)})/\dom(stack) = {} & not(bv_ireg_plus_d(iy,desloc)|->schar_to_byte(n0): stack) | mem:=mem<+{bv_ireg_plus_d(iy,desloc)|->schar_to_byte(n0)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_9IX_d0_n)==(desloc: SCHAR & n0: SCHAR & bv_ireg_plus_d(ix,desloc)/:dom(stack) & bv_ireg_plus_d(ix,desloc): BV16 & schar_to_byte(n0): BYTE & dom({bv_ireg_plus_d(ix,desloc)|->schar_to_byte(n0)})/\dom(stack) = {} & not(bv_ireg_plus_d(ix,desloc)|->schar_to_byte(n0): stack) | mem:=mem<+{bv_ireg_plus_d(ix,desloc)|->schar_to_byte(n0)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_9HL0_n)==(n0: SCHAR & byte_to_bv16(rgs8(h0),rgs8(l0))/:dom(stack) & byte_to_bv16(rgs8(h0),rgs8(l0)): BV16 & schar_to_byte(n0): BYTE & dom({byte_to_bv16(rgs8(h0),rgs8(l0))|->schar_to_byte(n0)})/\dom(stack) = {} & not(byte_to_bv16(rgs8(h0),rgs8(l0))|->schar_to_byte(n0): stack) | mem:=mem<+{byte_to_bv16(rgs8(h0),rgs8(l0))|->schar_to_byte(n0)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_9IY_d0_r)==(desloc: SCHAR & rr: id_reg_8 & bv_ireg_plus_d(iy,desloc)/:dom(stack) & bv_ireg_plus_d(iy,desloc): BV16 & rgs8(rr): BYTE & dom({bv_ireg_plus_d(iy,desloc)|->rgs8(rr)})/\dom(stack) = {} & not(bv_ireg_plus_d(iy,desloc)|->rgs8(rr): stack) | mem:=mem<+{bv_ireg_plus_d(iy,desloc)|->rgs8(rr)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_9IX_d0_r)==(desloc: SCHAR & rr: id_reg_8 & bv_ireg_plus_d(ix,desloc)/:dom(stack) & bv_ireg_plus_d(ix,desloc): BV16 & rgs8(rr): BYTE & dom({bv_ireg_plus_d(ix,desloc)|->rgs8(rr)})/\dom(stack) = {} & not(bv_ireg_plus_d(ix,desloc)|->rgs8(rr): stack) | mem:=mem<+{bv_ireg_plus_d(ix,desloc)|->rgs8(rr)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_9HL0_r)==(rr: id_reg_8 & byte_to_bv16(rgs8(h0),rgs8(l0))/:dom(stack) & byte_to_bv16(rgs8(h0),rgs8(l0)): BV16 & rgs8(rr): BYTE & dom({byte_to_bv16(rgs8(h0),rgs8(l0))|->rgs8(rr)})/\dom(stack) = {} & not(byte_to_bv16(rgs8(h0),rgs8(l0))|->rgs8(rr): stack) | mem:=mem<+{byte_to_bv16(rgs8(h0),rgs8(l0))|->rgs8(rr)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_r_9IY_d0)==(rr: id_reg_8 & desloc: SCHAR | rgs8,pc,r_:=rgs8<+{rr|->bv_9ireg_plus_d0(mem,iy,desloc)},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_r_9IX_d0)==(rr: id_reg_8 & desloc: SCHAR | rgs8,pc,r_:=rgs8<+{rr|->bv_9ireg_plus_d0(mem,ix,desloc)},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_r_9HL0)==(rr: id_reg_8 | @address.(address: BV16 & address = byte_to_bv16(rgs8(h0),rgs8(l0)) ==> rgs8,pc,r_:=rgs8<+{rr|->mem(address)},instruction_next(pc),update_refresh_reg(r_)));
  Expanded_List_Substitution(Machine(Z80),LD_r_n_)==(rr: id_reg_8 & n0: SCHAR | rgs8,pc,r_:=rgs8<+{rr|->schar_to_byte(n0)},instruction_next(pc),update_refresh_reg(r_));
  Expanded_List_Substitution(Machine(Z80),LD_r_r_)==(rr: id_reg_8 & rr_: id_reg_8 | rgs8,pc,r_:=rgs8<+{rr|->rgs8(rr_)},instruction_next(pc),update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_r_r_)==(rgs8(rr):=rgs8(rr_) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_r_n_)==(rgs8(rr):=schar_to_byte(n0) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_r_9HL0)==(ANY address WHERE address: BV16 & address = byte_to_bv16(rgs8(h0),rgs8(l0)) THEN rgs8(rr):=mem(address) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),LD_r_9IX_d0)==(rgs8(rr):=bv_9ireg_plus_d0(mem,ix,desloc) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_r_9IY_d0)==(rgs8(rr):=bv_9ireg_plus_d0(mem,iy,desloc) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_9HL0_r)==(updateAddressMem(byte_to_bv16(rgs8(h0),rgs8(l0)),rgs8(rr)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_9IX_d0_r)==(updateAddressMem(bv_ireg_plus_d(ix,desloc),rgs8(rr)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_9IY_d0_r)==(updateAddressMem(bv_ireg_plus_d(iy,desloc),rgs8(rr)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_9HL0_n)==(updateAddressMem(byte_to_bv16(rgs8(h0),rgs8(l0)),schar_to_byte(n0)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_9IX_d0_n)==(updateAddressMem(bv_ireg_plus_d(ix,desloc),schar_to_byte(n0)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_9IY_d0_n)==(updateAddressMem(bv_ireg_plus_d(iy,desloc),schar_to_byte(n0)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_A_9BC0)==(rgs8(a0):=mem(mem(byte_to_bv16(rgs8(b0),rgs8(c0)))) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_A_9DE0)==(rgs8(a0):=mem(byte_to_bv16(rgs8(d0),rgs8(e0))) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_A_9nn0)==(rgs8(a0):=mem(ushortint_to_bv16(nn)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_9BC0_A)==(updateAddressMem(byte_to_bv16(rgs8(b0),rgs8(c0)),rgs8(a0)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_9DE0_A)==(updateAddressMem(byte_to_bv16(rgs8(d0),rgs8(e0)),rgs8(a0)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_9nn0_A)==(updateAddressMem(ushortint_to_bv16(nn),rgs8(a0)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_A_I)==(ANY new_interrupt WHERE new_interrupt: BYTE THEN i_:=new_interrupt || rgs8:=rgs8<+{a0|->new_interrupt,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(new_interrupt),is_zero(new_interrupt),0,iff2,0,bitget(rgs8(f0),0))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),LD_A_R)==(rgs8:=rgs8<+{a0|->r_,get_new_flag_register_SZ_H_PvNC(rgs8,is_negative(r_),is_zero(r_),0,iff2,0,bitget(rgs8(f0),0))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_I_A)==(i_:=rgs8(a0) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_R_A)==(r_:=rgs8(a0) || pc:=instruction_next(pc));
  List_Substitution(Machine(Z80),LD_dd_nn)==(IF dd = SP THEN sp:=ushortint_to_bv16(nn) ELSE ANY rh,rl,w1,w2 WHERE rh: id_reg_8 & rl: id_reg_8 & w1: BYTE & w2: BYTE & rh,rl = REG16_TO_REG8(dd) & bv16_to_byte(ushortint_to_bv16(nn)) = w1,w2 THEN rgs8:=rgs8<+{rh|->w1,rl|->w2} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END END);
  List_Substitution(Machine(Z80),LD_IX_nn)==(ix:=ushortint_to_bv16(nn) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_IY_nn)==(iy:=ushortint_to_bv16(nn) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_HL_9nn0)==(rgs8:=rgs8<+{h0|->mem(ushortint_to_bv16(add16USHORTINT(0,nn,1))),l0|->mem(ushortint_to_bv16(nn))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_dd_9nn0)==(r_:=update_refresh_reg(r_) || IF dd = SP THEN sp:=byte_to_bv16(mem(ushortint_to_bv16(add16USHORTINT(0,nn,1))),mem(ushortint_to_bv16(nn))) ELSE ANY rh,rl,w1,w2 WHERE rh: id_reg_8 & rl: id_reg_8 & w1: BYTE & w2: BYTE & rh,rl = REG16_TO_REG8(dd) & w1 = mem(ushortint_to_bv16(add16USHORTINT(0,nn,1))) & w2 = mem(ushortint_to_bv16(nn)) THEN rgs8:=rgs8<+{rh|->w1,rl|->w2} || pc:=instruction_next(pc) END END);
  List_Substitution(Machine(Z80),LD_IX_9nn0)==(ix:=byte_to_bv16(mem(ushortint_to_bv16(add16USHORTINT(0,nn,1))),mem(ushortint_to_bv16(nn))) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_IY_9nn0)==(iy:=byte_to_bv16(mem(ushortint_to_bv16(add16USHORTINT(0,nn,1))),mem(ushortint_to_bv16(nn))) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_9nn0_HL)==(updateMem({ushortint_to_bv16(add16USHORTINT(0,nn,1))|->rgs8(h0),ushortint_to_bv16(nn)|->rgs8(l0)}) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_9nn0_dd)==(IF dd = SP THEN ANY vh,vl WHERE vh: BYTE & vl: BYTE & bv16_to_byte(sp) = vh,vl & {ushortint_to_bv16(add16USHORTINT(0,nn,1))|->vh,ushortint_to_bv16(nn)|->vl}: BV16 >-> BYTE THEN updateMem({ushortint_to_bv16(add16USHORTINT(0,nn,1))|->vh,ushortint_to_bv16(nn)|->vl}) || r_:=update_refresh_reg(r_) END ELSE ANY rh,rl,w1,w2 WHERE rh: id_reg_8 & rl: id_reg_8 & w1: SCHAR & w2: BYTE & rh,rl = REG16_TO_REG8(dd) & {ushortint_to_bv16(add16USHORTINT(0,nn,1))|->rgs8(rh),ushortint_to_bv16(nn)|->rgs8(rl)}: BV16 >-> BYTE THEN updateMem({ushortint_to_bv16(add16USHORTINT(0,nn,1))|->rgs8(rh),ushortint_to_bv16(nn)|->rgs8(rl)}) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END END);
  List_Substitution(Machine(Z80),LD_9nn0_IX)==(ANY h_ix,l_ix WHERE h_ix: BYTE & l_ix: BYTE & h_ix,l_ix = bv16_to_byte(ix) & ushortint_to_bv16(nn)/:dom(stack) & ushortint_to_bv16(add16USHORTINT(0,nn,1))/:dom(stack) THEN updateMem({ushortint_to_bv16(add16USHORTINT(0,nn,1))|->h_ix,ushortint_to_bv16(nn)|->l_ix}) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),LD_9nn0_IY)==(ANY h_iy,l_iy WHERE h_iy: BYTE & l_iy: BYTE & h_iy,l_iy = bv16_to_byte(iy) & ushortint_to_bv16(nn)/:dom(stack) & ushortint_to_bv16(add16USHORTINT(0,nn,1))/:dom(stack) THEN updateMem({ushortint_to_bv16(add16USHORTINT(0,nn,1))|->h_iy,ushortint_to_bv16(nn)|->l_iy}) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),LD_SP_HL)==(sp:=byte_to_bv16(rgs8(h0),rgs8(l0)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_SP_IX)==(sp:=ix || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),LD_SP_IY)==(sp:=iy || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),PUSH_qq)==(ANY qqh,qql WHERE qqh: id_reg_8 & qql: id_reg_8 & REG16_TO_REG8(qq) = qqh,qql & {ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->rgs8(qql),ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->rgs8(qqh)}: BV16 --> BYTE THEN updateStack({ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->rgs8(qql),ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->rgs8(qqh)}) || sp:=ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),PUSH_IX)==(ANY wh,wl WHERE wh: BYTE & wl: BYTE & bv16_to_byte(ix) = wh,wl & {ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->wl,ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->wh}: BV16 --> BYTE THEN updateStack({ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->wl,ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->wh}) || sp:=ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),PUSH_IY)==(ANY wh,wl WHERE wh: BYTE & wl: BYTE & bv16_to_byte(iy) = wh,wl & {ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->wl,ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->wh}: BV16 --> BYTE THEN updateStack({ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2))|->wl,ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -1))|->wh}) || sp:=ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp), -2)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),POP_qq)==(ANY qqh,qql WHERE qqh: id_reg_8 & qql: id_reg_8 & REG16_TO_REG8(qq) = qqh,qql & {qql|->mem(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),2))),qqh|->mem(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1)))}: id_reg_8 --> BYTE THEN rgs8:=rgs8<+{qql|->mem(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),2))),qqh|->mem(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1)))} || sp:=ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),2)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),POP_IX)==(ANY nw8 WHERE nw8: BYTE & byte_to_bv16(mem(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))),mem(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),2)))) = nw8 THEN ix:=nw8 || sp:=ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),2)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),POP_IY)==(ANY nw8 WHERE nw8: BYTE & byte_to_bv16(mem(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))),mem(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),2)))) = nw8 THEN iy:=nw8 || sp:=ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),2)) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),EX_DE_HL)==(rgs8:=rgs8<+{d0|->rgs8(h0),e0|->rgs8(l0),h0|->rgs8(d0),l0|->rgs8(e0)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),EX_AF_AF_)==(rgs8:=rgs8<+{a0|->rgs8(a_0),f0|->rgs8(f_0),a_0|->rgs8(a0),f_0|->rgs8(f0)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),EXX)==(rgs8:=rgs8<+{b0|->rgs8(b_0),c0|->rgs8(c_0),d0|->rgs8(d_0),e0|->rgs8(e_0),h0|->rgs8(h_0),l0|->rgs8(l_0),b_0|->rgs8(b0),c_0|->rgs8(c0),d_0|->rgs8(d0),e_0|->rgs8(e0),h_0|->rgs8(h0),l_0|->rgs8(l0)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),EX_9SP0_HL)==(rgs8:=rgs8<+{h0|->stack(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))),l0|->stack(sp)} || updateStack({ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->rgs8(h0),sp|->rgs8(l0)}) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),EX_9SP0_IX)==(ANY wh,wl WHERE wh: BYTE & wl: BYTE & bv16_to_byte(ix) = wh,wl & {ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->wh,sp|->wl}: BV16 --> BYTE & stack<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->wh,sp|->wl} <: mem THEN ix:=byte_to_bv16(stack(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))),stack(sp)) || updateStack({ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->wh,sp|->wl}) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),EX_9SP0_IY)==(ANY wh,wl WHERE wh: BYTE & wl: BYTE & bv16_to_byte(iy) = wh,wl & {ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->wh,sp|->wl}: BV16 --> BYTE & stack<+{ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->wh,sp|->wl} <: mem THEN iy:=byte_to_bv16(stack(ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))),stack(sp)) || updateStack({ushortint_to_bv16(add16USHORTINT(0,bv16_to_ushortint(sp),1))|->wh,sp|->wl}) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),LDI)==(ANY hvn,lvn,dvn,evn,bvn,cvn WHERE hvn,lvn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & dvn,evn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(d0),rgs8(e0)))) & bvn,cvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(b0),rgs8(c0)))) THEN updateAddressMem(byte_to_bv16(rgs8(d0),rgs8(e0)),mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,d0|->dvn,e0|->evn,b0|->bvn,c0|->cvn,get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bit_not(is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1)),0,bitget(rgs8(f0),0))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),LDIR)==(ANY hvn,lvn,dvn,evn,bvn,cvn WHERE hvn,lvn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & dvn,evn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(d0),rgs8(e0)))) & bvn,cvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(b0),rgs8(c0)))) THEN updateAddressMem(byte_to_bv16(rgs8(d0),rgs8(e0)),mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,d0|->dvn,e0|->evn,b0|->bvn,c0|->cvn,get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,0,0,bitget(rgs8(f0),0))} || r_:=update_refresh_reg(r_) || IF is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1) = 0 THEN pc:=instruction_next(pc) END END);
  List_Substitution(Machine(Z80),LDD)==(ANY hvn,lvn,dvn,evn,bvn,cvn WHERE hvn,lvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & dvn,evn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(d0),rgs8(e0)))) & bvn,cvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(b0),rgs8(c0)))) THEN updateAddressMem(byte_to_bv16(rgs8(d0),rgs8(e0)),mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,d0|->dvn,e0|->evn,b0|->bvn,c0|->cvn,get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,bit_not(is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1)),0,bitget(rgs8(f0),0))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),LDDR)==(ANY hvn,lvn,dvn,evn,bvn,cvn WHERE hvn,lvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & dvn,evn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(d0),rgs8(e0)))) & bvn,cvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(b0),rgs8(c0)))) THEN updateAddressMem(byte_to_bv16(rgs8(d0),rgs8(e0)),mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,d0|->dvn,e0|->evn,b0|->bvn,c0|->cvn,get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),0,0,0,bitget(rgs8(f0),0))} || r_:=update_refresh_reg(r_) || IF is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1) = 0 THEN pc:=instruction_next(pc) END END);
  List_Substitution(Machine(Z80),CPI)==(ANY sum,negative,carry,half_carry,zero,hvn,lvn,bvn,cvn WHERE sum: UCHAR & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & substract8UCHAR(0,byte_to_uchar(rgs8(a0)),byte_to_uchar(mem(byte_to_bv16(rgs8(h0),rgs8(l0))))) = sum,negative,carry,half_carry,zero & hvn,lvn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & bvn,cvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(b0),rgs8(c0)))) THEN IF zero = 1 THEN rgs8:={h0|->hvn,l0|->lvn,b0|->bvn,c0|->cvn,get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,bit_not(is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1)),1,bitget(rgs8(f0),0))} END || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),CPIR)==(ANY sum,negative,carry,half_carry,zero,hvn,lvn,bvn,cvn WHERE sum: UCHAR & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & substract8UCHAR(0,byte_to_uchar(rgs8(a0)),byte_to_uchar(mem(byte_to_bv16(rgs8(h0),rgs8(l0))))) = sum,negative,carry,half_carry,zero & hvn,lvn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & bvn,cvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(b0),rgs8(c0)))) THEN IF zero = 1 THEN rgs8:={h0|->hvn,l0|->lvn,b0|->bvn,c0|->cvn,get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,bit_not(is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1)),1,bitget(rgs8(f0),0))} END || r_:=update_refresh_reg(r_) || IF is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1) = 0 or zero = 1 THEN pc:=instruction_next(pc) END END);
  List_Substitution(Machine(Z80),CPD)==(ANY sum,negative,carry,half_carry,zero,hvn,lvn,bvn,cvn WHERE sum: UCHAR & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & substract8UCHAR(0,byte_to_uchar(rgs8(a0)),byte_to_uchar(mem(byte_to_bv16(rgs8(h0),rgs8(l0))))) = sum,negative,carry,half_carry,zero & hvn,lvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & bvn,cvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(b0),rgs8(c0)))) THEN IF zero = 1 THEN rgs8:={h0|->hvn,l0|->lvn,b0|->bvn,c0|->cvn,get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,bit_not(is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1)),1,bitget(rgs8(f0),0))} END || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),CPDR)==(ANY sum,negative,carry,half_carry,zero,hvn,lvn,bvn,cvn WHERE sum: UCHAR & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & substract8UCHAR(0,byte_to_uchar(rgs8(a0)),byte_to_uchar(mem(byte_to_bv16(rgs8(h0),rgs8(l0))))) = sum,negative,carry,half_carry,zero & hvn,lvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & bvn,cvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(b0),rgs8(c0)))) THEN IF zero = 1 THEN rgs8:={h0|->hvn,l0|->lvn,b0|->bvn,c0|->cvn,get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,bit_not(is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1)),1,bitget(rgs8(f0),0))} END || IF is_zeroUSHORTINT(bv16_to_ushortint(byte_to_bv16(rgs8(b0),rgs8(c0)))-1) = 0 or zero = 1 THEN pc:=instruction_next(pc) END || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),DAA)==(ANY result,s0,z0,h0,pv0,n0,c0 WHERE result: BYTE & s0: BIT & z0: BIT & h0: BIT & pv0: BIT & n0: BIT & c0: BIT & daa_function(bitget(rgs8(f0),1),bitget(rgs8(f0),0),rgs8(a0),bitget(rgs8(f0),4)) = result,c0,h0 & s0 = bv_get(result,7) & z0 = is_zero(result) & pv0 = parity_even_BYTE(result) & n0 = bitget(rgs8(f0),1) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register_SZ_H_PvNC(rgs8,s0,z0,h0,pv0,n0,c0)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),CPL)==(ANY result WHERE result: BYTE & result = complement(rgs8(a0)) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),1,bitget(rgs8(f0),2),1,bitget(rgs8(f0),0))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),NEG)==(ANY sum,negative,carry,half_carry,zero WHERE sum: UCHAR & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & sum,negative,carry,half_carry,zero = substract8UCHAR(0,0,byte_to_uchar(rgs8(a0))) THEN rgs8:=rgs8<+{a0|->uchar_to_byte(sum),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,carry,1,carry)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),CCF)==(rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),bitget(rgs8(f0),4),bitget(rgs8(f0),2),0,bit_not(bitget(rgs8(f0),0)))} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),SCF)==(rgs8:=rgs8<+{get_new_flag_register_SZ_H_PvNC(rgs8,bitget(rgs8(f0),7),bitget(rgs8(f0),6),bitget(rgs8(f0),4),bitget(rgs8(f0),2),0,1)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),NOP)==(pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),HALT)==(r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),DI)==(iff1:=0 || iff2:=0 || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),EI)==(iff1:=1 || iff2:=1 || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),IM0)==(im:=0|->0 || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),IM1)==(im:=1|->0 || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),IM2)==(im:=1|->1 || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),IN_A_9n0)==(ANY data_in WHERE data_in: SCHAR THEN i_o_ports(uchar_to_byte(nn)):=schar_to_byte(data_in) || rgs8(a0):=schar_to_byte(data_in) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),IN_r_9C0)==(ANY data_in,negative,zero,half_carry,pv,add_sub,carry WHERE data_in: BYTE & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & negative = is_negative(data_in) & zero = is_zero(data_in) & half_carry = 0 & pv = parity_even_BYTE(data_in) & add_sub = 0 & carry = bitget(rgs8(f0),0) THEN i_o_ports(rgs8(c0)):=data_in || rgs8:=rgs8<+{rr|->data_in,get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),INI)==(ANY data_in,hvn,lvn,negative,zero,half_carry,pv,add_sub,carry WHERE data_in: BYTE & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & hvn,lvn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & negative = is_negative(data_in) & zero = is_zero(dec_BYTE(rgs8(b0))) & half_carry = bitget(rgs8(f0),4) & pv = parity_even_BYTE(data_in) & add_sub = 1 & carry = bitget(rgs8(f0),0) THEN i_o_ports(rgs8(c0)):=data_in || updateAddressMem(byte_to_bv16(rgs8(h0),rgs8(l0)),i_o_ports(rgs8(c0))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->dec_BYTE(rgs8(b0)),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),INIR)==(ANY data_in,hvn,lvn,negative,zero,half_carry,pv,add_sub,carry WHERE data_in: BYTE & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & hvn,lvn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & negative = is_negative(data_in) & zero = 1 & half_carry = bitget(rgs8(f0),4) & pv = parity_even_BYTE(data_in) & add_sub = 1 & carry = bitget(rgs8(f0),0) THEN i_o_ports(rgs8(c0)):=data_in || updateAddressMem(byte_to_bv16(rgs8(h0),rgs8(l0)),data_in) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->dec_BYTE(rgs8(b0)),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || r_:=update_refresh_reg(r_) || IF is_zero(dec_BYTE(rgs8(b0))) = 1 THEN pc:=instruction_next(pc) END END);
  List_Substitution(Machine(Z80),IND)==(ANY data_in,hvn,lvn,negative,zero,half_carry,pv,add_sub,carry WHERE data_in: BYTE & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & hvn,lvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & negative = is_negative(data_in) & zero = is_zero(dec_BYTE(rgs8(b0))) & half_carry = bitget(rgs8(f0),4) & pv = parity_even_BYTE(data_in) & add_sub = 1 & carry = bitget(rgs8(f0),0) THEN i_o_ports(rgs8(c0)):=data_in || updateAddressMem(byte_to_bv16(rgs8(h0),rgs8(l0)),data_in) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->dec_BYTE(rgs8(b0)),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),INDR)==(ANY data_in,hvn,lvn,negative,zero,half_carry,pv,add_sub,carry WHERE data_in: BYTE & negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & hvn,lvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & negative = is_negative(data_in) & zero = is_zero(dec_BYTE(rgs8(b0))) & half_carry = bitget(rgs8(f0),4) & pv = parity_even_BYTE(data_in) & add_sub = 1 & carry = bitget(rgs8(f0),0) THEN i_o_ports(rgs8(c0)):=data_in || updateAddressMem(byte_to_bv16(rgs8(h0),rgs8(l0)),data_in) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->dec_BYTE(rgs8(b0)),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || IF is_zero(dec_BYTE(rgs8(b0))) = 1 THEN pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END END);
  List_Substitution(Machine(Z80),OUT_9n0_A)==(i_o_ports(uchar_to_byte(nn)):=rgs8(a0) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),OUT_9C0_r)==(i_o_ports(rgs8(rr)):=rgs8(c0) || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_));
  List_Substitution(Machine(Z80),OUTI)==(ANY hvn,lvn,negative,zero,half_carry,pv,add_sub,carry WHERE negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & hvn,lvn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & negative = is_negative(mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) & zero = is_zero(dec_BYTE(rgs8(b0))) & half_carry = bitget(rgs8(f0),4) & pv = parity_even_BYTE(mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) & add_sub = 1 & carry = bitget(rgs8(f0),0) THEN i_o_ports(rgs8(c0)):=mem(byte_to_bv16(rgs8(h0),rgs8(l0))) || updateAddressMem(byte_to_bv16(rgs8(h0),rgs8(l0)),i_o_ports(rgs8(c0))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->dec_BYTE(rgs8(b0)),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),OUTIR)==(ANY hvn,lvn,negative,zero,half_carry,pv,add_sub,carry WHERE negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & hvn,lvn = bv16_to_byte(inc_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & negative = is_negative(mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) & zero = is_zero(dec_BYTE(rgs8(b0))) & half_carry = bitget(rgs8(f0),4) & pv = parity_even_BYTE(mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) & add_sub = 1 & carry = bitget(rgs8(f0),0) THEN i_o_ports(rgs8(c0)):=mem(byte_to_bv16(rgs8(h0),rgs8(l0))) || updateAddressMem(byte_to_bv16(rgs8(h0),rgs8(l0)),i_o_ports(rgs8(c0))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->dec_BYTE(rgs8(b0)),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || r_:=update_refresh_reg(r_) || IF is_zero(dec_BYTE(rgs8(b0))) = 1 THEN pc:=instruction_next(pc) END END);
  List_Substitution(Machine(Z80),OUTD)==(ANY hvn,lvn,negative,zero,half_carry,pv,add_sub,carry WHERE negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & hvn,lvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & negative = is_negative(mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) & zero = is_zero(dec_BYTE(rgs8(b0))) & half_carry = bitget(rgs8(f0),4) & pv = parity_even_BYTE(mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) & add_sub = 1 & carry = bitget(rgs8(f0),0) THEN i_o_ports(rgs8(c0)):=mem(byte_to_bv16(rgs8(h0),rgs8(l0))) || updateAddressMem(byte_to_bv16(rgs8(h0),rgs8(l0)),i_o_ports(rgs8(c0))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->dec_BYTE(rgs8(b0)),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || pc:=instruction_next(pc) || r_:=update_refresh_reg(r_) END);
  List_Substitution(Machine(Z80),OUTDR)==(ANY hvn,lvn,negative,zero,half_carry,pv,add_sub,carry WHERE negative: BIT & carry: BIT & half_carry: BIT & zero: BIT & hvn,lvn = bv16_to_byte(dec_BV16(byte_to_bv16(rgs8(h0),rgs8(l0)))) & negative = is_negative(mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) & zero = is_zero(dec_BYTE(rgs8(b0))) & half_carry = bitget(rgs8(f0),4) & pv = parity_even_BYTE(mem(byte_to_bv16(rgs8(h0),rgs8(l0)))) & add_sub = 1 & carry = bitget(rgs8(f0),0) THEN i_o_ports(rgs8(c0)):=mem(byte_to_bv16(rgs8(h0),rgs8(l0))) || updateAddressMem(byte_to_bv16(rgs8(h0),rgs8(l0)),i_o_ports(rgs8(c0))) || rgs8:=rgs8<+{h0|->hvn,l0|->lvn,b0|->dec_BYTE(rgs8(b0)),get_new_flag_register_SZ_H_PvNC(rgs8,negative,zero,half_carry,pv,add_sub,carry)} || r_:=update_refresh_reg(r_) || IF is_zero(dec_BYTE(rgs8(b0))) = 1 THEN pc:=instruction_next(pc) END END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Z80))==(RESERVED_ADDRESS,get_bv_reg16,REG16_TO_REG8,REG8_TO_REG16,update_flag_register_SZ_H_PvNC,get_new_flag_register_SZ_H_PvNC,bv_ireg_plus_d,bv_9ireg_plus_d0,daa_function);
  Inherited_List_Constants(Machine(Z80))==(RESERVED_ADDRESS);
  List_Constants(Machine(Z80))==(get_bv_reg16,REG16_TO_REG8,REG8_TO_REG16,update_flag_register_SZ_H_PvNC,get_new_flag_register_SZ_H_PvNC,bv_ireg_plus_d,bv_9ireg_plus_d0,daa_function)
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
  Context_List_Properties(Machine(Z80))==(SCHAR_MAX: INTEGER & SCHAR_MIN: INTEGER & SCHAR_MAX = 2**7-1 & SCHAR_MIN = (-2)**7 & SCHAR = (-2)**7..2**7-1 & UCHAR = 0..2**8-1 & UCHAR_MAX: INTEGER & UCHAR_MIN: INTEGER & UCHAR_MAX = 2**8 & UCHAR_MIN = 0 & uchar_schar: UCHAR --> SCHAR & uchar_schar = %v1.(v1: UCHAR & v1<=SCHAR_MAX | v1) & uchar_schar = %v1.(v1: UCHAR & not(v1<=SCHAR_MAX) | v1-UCHAR_MAX+1) & schar_uchar: SCHAR --> UCHAR & schar_uchar = uchar_schar~ & SSHORTINT = (-2)**15..2**15-1 & byte_to_uchar: BYTE --> UCHAR & byte_to_uchar = %v0.(v0: BYTE | 2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & uchar_to_byte: UCHAR --> BYTE & uchar_to_byte = byte_to_uchar~ & update_refresh_reg: BYTE --> BYTE & update_refresh_reg = %v0.(v0: BYTE | uchar_to_byte(2**7*v0(7)+(2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) mod 64)) & USHORTINT = 0..2**16-1 & USHORTINT_MAX = 2**16-1 & USHORTINT_MIN = 0 & NB_INSTRUCTIONS: NATURAL & INST_SZ: NATURAL & INST_SZ = 16 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = USHORTINT & instruction_next: USHORTINT --> USHORTINT & instruction_next = %w1.(w1: USHORTINT | (w1+1) mod 65535) & instruction_jump = %(p0,e0).(p0: INSTRUCTION & e0: -126..129 | (p0+e0) mod 65535) & byte_to_schar: BYTE --> SCHAR & byte_to_schar = %v0.(v0: BYTE | (-2)**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & schar_to_byte: SCHAR --> BYTE & schar_to_byte = byte_to_schar~ & bv16_to_sshortint: BV16 --> SSHORTINT & bv16_to_sshortint = %v0.(v0: BV16 | (-2)**15*v0(15)+2**14*v0(14)+2**13*v0(13)+2**12*v0(12)+2**11*v0(11)+2**10*v0(10)+2**9*v0(9)+2**8*v0(8)+2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & sshortint_to_bv16: SSHORTINT --> BV16 & sshortint_to_bv16 = bv16_to_sshortint~ & byte_to_bv16: BYTE*BYTE --> BV16 & byte_to_bv16 = %(v1,v2).(v1: BV16 & v2: BV16 | {0|->v2(0),1|->v2(1),2|->v2(2),3|->v2(3),4|->v2(4),5|->v2(5),6|->v2(6),7|->v2(7),8|->v1(0),9|->v1(1),10|->v1(2),11|->v1(3),12|->v1(4),13|->v1(5),14|->v1(6),15|->v1(7)}) & bv16_to_byte: BV16 --> BYTE*BYTE & bv16_to_byte = byte_to_bv16~ & schar_to_sshortint: SCHAR*SCHAR --> SSHORTINT & schar_to_sshortint = %(w1,w2).(w1: SCHAR & w2: SCHAR | bv16_to_sshortint(byte_to_bv16(schar_to_byte(w1),schar_to_byte(w2)))) & sshortint_to_schar: SSHORTINT --> SCHAR*SCHAR & sshortint_to_schar = schar_to_sshortint~ & bv16_to_ushortint: BV16 --> USHORTINT & bv16_to_ushortint = %v0.(v0: BV16 | 2**15*v0(15)+2**14*v0(14)+2**13*v0(13)+2**12*v0(12)+2**11*v0(11)+2**10*v0(10)+2**9*v0(9)+2**8*v0(8)+2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & ushortint_to_bv16: USHORTINT --> BV16 & ushortint_to_bv16 = bv16_to_ushortint~ & sshortint_to_ushortint: SSHORTINT --> USHORTINT & sshortint_to_ushortint = %v0.(v0: SSHORTINT | v0-32768) & ushortint_to_sshortint: USHORTINT --> SSHORTINT & ushortint_to_sshortint = sshortint_to_ushortint~ & get_upper_digit = %by.(by: BYTE | 2**3*by(7)+2**2*by(6)+2*by(5)+by(4)) & get_lower_digit = %by.(by: BYTE | 2**3*by(3)+2**2*by(2)+2*by(1)+by(0)) & BIT = 0..1 & bit_not: BIT --> BIT & bit_not = %bb.(bb: BIT | 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = seq1(BIT) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_index: BIT_VECTOR --> POW(NATURAL) & bv_index = %v1.(v1: BIT_VECTOR | 0..bv_size(v1)-1) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: low..bv_size(bv)-1 | %i0.(i0: 1..high-low+1 | bv(i0+low))) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (1..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (1..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 1..bv_size(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_xor(v1(idx),v2(idx)))) & bv_get: BIT_VECTOR*NATURAL --> BIT & bv_get = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1(idx+1)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 & bit: BIT | v1<+{idx+1|->bit}) & BYTE_WIDTH = 8 & BYTE_INDEX = 1..BYTE_WIDTH & REAL_BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE = {bt | bt: BIT_VECTOR & bv_size(bt) = BYTE_WIDTH} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & byte_bit_get: BYTE*REAL_BYTE_INDEX --> BIT & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)) & BV16_INDX = 1..16 & REAL_BV16_INDX = 0..16-1 & BV16 = {bt | bt: BIT_VECTOR & bv_size(bt) = 16} & BV16_ZERO = BV16_INDX*{0} & bv16_byte: BV16 --> BYTE*BYTE & bv16_byte = %bv.(bv: BV16 | {0|->bv(8),1|->bv(9),2|->bv(10),3|->bv(11),4|->bv(12),5|->bv(13),6|->bv(14),7|->bv(15)},{0|->bv(0),1|->bv(1),2|->bv(2),3|->bv(3),4|->bv(4),5|->bv(5),6|->bv(6),7|->bv(7)}) & byte_bv16 = bv16_byte~ & bv16_bit_get: BYTE*BV16_INDX --> BIT & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)) & is_zero: BYTE --> BIT & is_zero = %w1.(w1: BYTE | bool_to_bit(bool(w1(0)+w1(1)+w1(2)+w1(3)+w1(4)+w1(5)+w1(6)+w1(7) = 0))) & is_zeroUSHORTINT: USHORTINT --> BIT & is_zeroUSHORTINT = %nat1.(nat1: USHORTINT | bool_to_bit(bool(nat1 = 0))) & is_negative: BYTE --> BIT & is_negative = %w1.(w1: BYTE | w1(7)) & half: UCHAR --> UCHAR & half = %ww.(ww: UCHAR | ww mod 2**4) & simple_add8UCHAR: UCHAR*UCHAR --> UCHAR & simple_add8UCHAR = %(w1,w2).(w1: UCHAR & w2: UCHAR | (w1+w2) mod 2**8) & add8UCHAR: BIT*UCHAR*UCHAR --> UCHAR*BIT*BIT*BIT*BIT & add8UCHAR = %(carry,w1,w2).(carry: BIT & w1: UCHAR & w2: UCHAR | (carry+w1+w2) mod 256,bool_to_bit(bool(carry+uchar_schar(w1)+uchar_schar(w2)<0)),bool_to_bit(bool(carry+w1+w2>UCHAR_MAX)),bool_to_bit(bool(carry+half(w1)+half(w2)>=2**4)),bool_to_bit(bool((carry+w1+w2) mod UCHAR_MAX-1 = 0))) & substract8UCHAR: BIT*UCHAR*UCHAR --> UCHAR*BIT*BIT*BIT*BIT & substract8UCHAR = %(carry,w1,w2).(carry: BIT & w1: UCHAR & w2: UCHAR | (carry+w1-w2) mod 256,bool_to_bit(bool(carry+uchar_schar(w1)-uchar_schar(w2)<0)),bool_to_bit(bool(carry+w1-w2>UCHAR_MAX)),bool_to_bit(bool(carry+half(w1)-half(w2)>=2**4)),bool_to_bit(bool((carry+w1-w2) mod UCHAR_MAX-1 = 0))) & add16USHORTINT: BIT*USHORTINT*USHORTINT --> USHORTINT & add16USHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | (b1+w1+w2) mod 65536) & add_carryUSHORTINT: BIT*USHORTINT*USHORTINT --> BIT & add_carryUSHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | bool_to_bit(bool(b1+w1+w2>2**16))) & add_halfcarryUSHORTINT: BIT*USHORTINT*USHORTINT --> BIT & add_halfcarryUSHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | bool_to_bit(bool(b1+w1 mod 2**12+w2 mod 2**12>2**12))) & sub16USHORTINT: BIT*USHORTINT*USHORTINT --> USHORTINT & sub16USHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | (w1-w2-b1) mod 65536) & sub_carryUSHORTINT: BIT*USHORTINT*USHORTINT --> BIT & sub_carryUSHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | bool_to_bit(bool(w1-w2-b1>2**16))) & sub_halfcarryUSHORTINT: BIT*USHORTINT*USHORTINT --> BIT & sub_halfcarryUSHORTINT = %(b1,w1,w2).(b1: BIT & w1: USHORTINT & w2: USHORTINT | bool_to_bit(bool(w1 mod 2**12-w2 mod 2**12-b1>2**12))) & inc_BYTE: BYTE --> BYTE & inc_BYTE = %w1.(w1: BYTE | uchar_to_byte((byte_to_uchar(w1)+1) mod 256)) & dec_BYTE: BYTE --> BYTE & dec_BYTE = %w1.(w1: BYTE | uchar_to_byte((byte_to_uchar(w1)-1) mod 256)) & inc_BV16: BV16 --> BV16 & inc_BV16 = %w1.(w1: BV16 | ushortint_to_bv16((bv16_to_ushortint(w1)+1) mod 65536)) & dec_BV16: BYTE --> BYTE & dec_BV16 = %w1.(w1: BV16 | ushortint_to_bv16((bv16_to_ushortint(w1)-1) mod 65536)) & parity_even_BYTE: BIT_VECTOR --> BIT & parity_even_BYTE = %bv.(bv: BIT_VECTOR | 1-(bv_get(bv,0)+bv_get(bv,1)+bv_get(bv,2)+bv_get(bv,3)+bv_get(bv,4)+bv_get(bv,5)+bv_get(bv,7)) mod 2) & and: BYTE*BYTE --> BYTE & and = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_and(bt1,bt2)) & ior: BYTE*BYTE --> BYTE & ior = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_or(bt1,bt2)) & xor: BYTE*BYTE --> BYTE & xor = %(bt1,bt2).(bt1: BYTE & bt2: BYTE | bv_xor(bt1,bt2)) & bitget: BYTE*BYTE_INDEX --> BIT & bitget = %(bt1,ii).(bt1: BYTE & ii: BYTE_INDEX | bt1(ii)) & bitset: BYTE*BYTE_INDEX --> BYTE & !(ww,ii).(ww: BYTE & ii: BYTE_INDEX => bitset(ww,ii) = bv_set(ww,ii)) & bitclear: BYTE*BYTE_INDEX --> BYTE & !(ww,ii,bb).(ww: BYTE & ii: BYTE_INDEX & bb: BIT => bitclear(ww,ii) = bv_clear(ww,ii)) & complement: BYTE --> BYTE & complement = %bt.(bt: BYTE | bv_not(bt)) & swap: BYTE --> BYTE & swap = %bt.(bt: BYTE | {0|->bt(4),1|->bt(5),2|->bt(6),3|->bt(7),4|->bt(0),5|->bt(1),6|->bt(2),7|->bt(3)}) & rotateleft: BYTE --> BYTE & rotateleft = %bv.(bv: BYTE | {0|->bv(7),1|->bv(0),2|->bv(1),3|->bv(2),4|->bv(3),5|->bv(4),6|->bv(5),7|->bv(6)}) & rotateright: BYTE --> BYTE & rotateright = %bv.(bv: BYTE | {0|->bv(1),1|->bv(2),2|->bv(3),3|->bv(4),4|->bv(5),5|->bv(6),6|->bv(7),7|->bv(0)}));
  Inherited_List_Properties(Machine(Z80))==(RESERVED_ADDRESS = 0..16384);
  List_Properties(Machine(Z80))==(get_bv_reg16: BV16*(id_reg_8 --> BYTE)*id_reg_16 --> BV16 & !(sp_,rgs8_,r1).(sp_: BV16 & rgs8_: id_reg_8 --> BYTE & r1: id_reg_16 => (r1 = BC => get_bv_reg16(sp_,rgs8_,r1) = byte_to_bv16(rgs8_(b0),rgs8_(c0))) & (r1 = DE => get_bv_reg16(sp_,rgs8_,r1) = byte_to_bv16(rgs8_(d0),rgs8_(e0))) & (r1 = HL => get_bv_reg16(sp_,rgs8_,r1) = byte_to_bv16(rgs8_(h0),rgs8_(l0))) & (r1 = SP => get_bv_reg16(sp_,rgs8_,r1) = sp_) & (r1 = AF => get_bv_reg16(sp_,rgs8_,r1) = byte_to_bv16(rgs8_(a0),rgs8_(f0)))) & REG16_TO_REG8: id_reg_16 --> id_reg_8*id_reg_8 & REG16_TO_REG8(BC) = b0,c0 & REG16_TO_REG8(DE) = d0,e0 & REG16_TO_REG8(HL) = h0,l0 & REG16_TO_REG8(AF) = a0,f0 & REG8_TO_REG16 = REG16_TO_REG8~ & update_flag_register_SZ_H_PvNC = %(rgs8_,s7,z6,h4,pv2,n_add_sub,c0).(rgs8_: id_reg_8 --> BYTE & s7: BIT & z6: BIT & h4: BIT & pv2: BIT & n_add_sub: BIT & c0: BIT | rgs8_<+{f0|->{7|->s7,6|->z6,4|->h4,2|->pv2,1|->n_add_sub,0|->c0}}) & get_new_flag_register_SZ_H_PvNC = %(rgs8_,s7,z6,h4,pv2,n_add_sub,c0).(rgs8_: id_reg_8 --> BYTE & s7: BIT & z6: BIT & h4: BIT & pv2: BIT & n_add_sub: BIT & c0: BIT | f0|->{7|->s7,6|->z6,4|->h4,2|->pv2,1|->n_add_sub,0|->c0}) & bv_ireg_plus_d = %(ix_iy,desloc).(ix_iy: BV16 & desloc: SCHAR | ushortint_to_bv16((bv16_to_ushortint(ix_iy)+desloc) mod 65536)) & bv_9ireg_plus_d0 = %(mem,ix_iy,desloc).(mem: BV16 --> BYTE & ix_iy: BV16 & desloc: SCHAR | mem(bv_ireg_plus_d(ix_iy,desloc))) & daa_function: BIT*BIT*BYTE*BIT --> BYTE*BIT*BIT & !(zn,c0,value,h0).(zn: BIT & c0: BIT & value: BYTE & h0: BIT => (zn = 0 & c0 = 0 & get_upper_digit(value): 0..9 & h0 = 0 & get_lower_digit(value): 0..9 => daa_function(zn,c0,value,h0) = value,0,0) & (zn = 0 & c0 = 0 & get_upper_digit(value): 0..8 & h0 = 0 & get_lower_digit(value): 10..15 => daa_function(zn,c0,value,h0) = uchar_to_byte(simple_add8UCHAR(byte_to_uchar(value),6)),0,bool_to_bit(bool(half(byte_to_uchar(value))+half(6)>=2**4))) & (zn = 0 & c0 = 0 & get_upper_digit(value): 0..9 & h0 = 1 & get_lower_digit(value): 0..3 => daa_function(zn,c0,value,h0) = uchar_to_byte(simple_add8UCHAR(byte_to_uchar(value),6)),0,bool_to_bit(bool(half(byte_to_uchar(value))+half(6)>=2**4))) & (zn = 0 & c0 = 0 & get_upper_digit(value): 10..15 & h0 = 0 & get_lower_digit(value): 0..9 => daa_function(zn,c0,value,h0) = uchar_to_byte(simple_add8UCHAR(byte_to_uchar(value),96)),1,bool_to_bit(bool(half(byte_to_uchar(value))+half(96)>=2**4))) & (zn = 0 & c0 = 0 & get_upper_digit(value): 9..15 & h0 = 0 & get_lower_digit(value): 10..15 => daa_function(zn,c0,value,h0) = uchar_to_byte(simple_add8UCHAR(byte_to_uchar(value),102)),1,bool_to_bit(bool(half(byte_to_uchar(value))+half(102)>=2**4))) & (zn = 0 & c0 = 0 & get_upper_digit(value): 10..15 & h0 = 1 & get_lower_digit(value): 0..3 => daa_function(zn,c0,value,h0) = uchar_to_byte(simple_add8UCHAR(byte_to_uchar(value),102)),1,bool_to_bit(bool(half(byte_to_uchar(value))+half(102)>=2**4))) & (zn = 0 & c0 = 1 & get_upper_digit(value): 0..2 & h0 = 0 & get_lower_digit(value): 0..9 => daa_function(zn,c0,value,h0) = uchar_to_byte(simple_add8UCHAR(byte_to_uchar(value),96)),1,bool_to_bit(bool(half(byte_to_uchar(value))+half(96)>=2**4))) & (zn = 0 & c0 = 1 & get_upper_digit(value): 0..2 & h0 = 0 & get_lower_digit(value): 10..15 => daa_function(zn,c0,value,h0) = uchar_to_byte(simple_add8UCHAR(byte_to_uchar(value),102)),1,bool_to_bit(bool(half(byte_to_uchar(value))+half(102)>=2**4))) & (zn = 0 & c0 = 1 & get_upper_digit(value): 0..3 & h0 = 1 & get_lower_digit(value): 0..3 => daa_function(zn,c0,value,h0) = uchar_to_byte(simple_add8UCHAR(byte_to_uchar(value),102)),1,bool_to_bit(bool(half(byte_to_uchar(value))+half(102)>=2**4))) & (zn = 1 & c0 = 0 & get_upper_digit(value): 0..9 & h0 = 0 & get_lower_digit(value): 0..9 => daa_function(zn,c0,value,h0) = value,0,0) & (zn = 1 & c0 = 0 & get_upper_digit(value): 0..8 & h0 = 1 & get_lower_digit(value): 6..15 => daa_function(zn,c0,value,h0) = uchar_to_byte(simple_add8UCHAR(byte_to_uchar(value),250)),0,bool_to_bit(bool(half(byte_to_uchar(value))+half(250)>=2**4))) & (zn = 1 & c0 = 1 & get_upper_digit(value): 7..15 & h0 = 0 & get_lower_digit(value): 0..9 => daa_function(zn,c0,value,h0) = uchar_to_byte(simple_add8UCHAR(byte_to_uchar(value),160)),1,bool_to_bit(bool(half(byte_to_uchar(value))+half(160)>=2**4))) & (zn = 1 & c0 = 1 & get_upper_digit(value): 6..7 & h0 = 1 & get_lower_digit(value): 6..15 => daa_function(zn,c0,value,h0) = uchar_to_byte(simple_add8UCHAR(byte_to_uchar(value),154)),1,bool_to_bit(bool(half(byte_to_uchar(value))+half(154)>=2**4)))) & id_reg_8: FIN(INTEGER) & not(id_reg_8 = {}) & id_reg_16: FIN(INTEGER) & not(id_reg_16 = {}))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(Z80),Machine(POWER2))==(?);
  Seen_Context_List_Enumerated(Machine(Z80))==(?);
  Seen_Context_List_Invariant(Machine(Z80))==(btrue);
  Seen_Context_List_Assertions(Machine(Z80))==((2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536) & BV16_ZERO: BIT_VECTOR & BV16 <: BIT_VECTOR & BV16_ZERO: BV16 & first(BYTE_ZERO) = 0 & BYTE_ZERO: BIT_VECTOR & BYTE <: BIT_VECTOR & size(BYTE_ZERO) = 8 & !bt.(bt: BYTE => size(bt) = 8) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_not(bv_not(bv)),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: 0..bv_size(bv)-1 & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,v2),indx) = bv_get(bv_and(v2,v1),indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_and(v1,bv_and(v2,v3)),indx) = bv_get(bv_and(bv_and(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv_zero(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_and(bv,bv_one(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,v2),indx) = bv_get(bv_or(v2,v1),indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: 0..bv_size(v1)-1 => bv_get(bv_or(v1,bv_or(v2,v3)),indx) = bv_get(bv_or(bv_or(v1,v2),v3),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_one(bv_size(bv))),indx) = bv_get(bv_one(bv_size(bv)),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_or(bv,bv_zero(bv_size(bv))),indx) = bv_get(bv,indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: 0..bv_size(v1)-1 => bv_get(bv_xor(v1,v2),indx) = bv_get(bv_xor(v2,v1),indx));!(bv,indx).(bv: BIT_VECTOR & indx: 0..bv_size(bv)-1 => bv_get(bv_xor(bv,bv),indx) = bv_get(bv_zero(bv_size(bv)),indx))) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT => bb = 0 or bb = 1) & bool_to_bit(FALSE) = 0 & bool_to_bit(TRUE) = 1 & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & bit_xor(1,1) = 0 & bit_xor(1,0) = 1 & bit_xor(0,1) = 1 & bit_xor(0,0) = 0 & !b1.(b1: BIT => bit_or(0,b1) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & bit_or(1,1) = 1 & bit_or(1,0) = 1 & bit_or(0,1) = 1 & bit_or(0,0) = 0 & !b1.(b1: BIT => bit_and(b1,0) = 0) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & bit_and(1,1) = 1 & bit_and(1,0) = 0 & bit_and(0,1) = 0 & bit_and(0,0) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_not(1) = 0 & bit_not(0) = 1 & !bb.(bb: BIT => bit_not(bb) = 1-bb) & !xx.(xx: BYTE => get_lower_digit(xx): 0..16) & !xx.(xx: BYTE => get_upper_digit(xx): 0..16) & !xx.(xx: USHORTINT => ushortint_to_bv16(xx): BV16) & !xx.(xx: BV16 => bv16_to_ushortint(xx): USHORTINT) & !(xx,yy).(xx: BYTE & yy: BYTE => #zz.(zz: BV16 & byte_to_bv16(xx,yy) = zz)) & !(xx,yy).(xx: BYTE & yy: BYTE => byte_to_bv16(xx,yy): BV16) & !xx.(xx: SCHAR => schar_to_byte(xx): BYTE) & !xx.(xx: BYTE => byte_to_schar(xx): SCHAR) & !xx.(xx: BYTE => update_refresh_reg(xx): BYTE) & !xx.(xx: UCHAR => uchar_to_byte(xx): BYTE) & !xx.(xx: BYTE => byte_to_uchar(xx): UCHAR) & instruction_next: USHORTINT --> USHORTINT & !n0.(n0: UCHAR => n0<=255) & !n0.(n0: UCHAR => 0<=n0) & 0 = schar_to_sshortint(0,0) & !(nn,pp).(nn: NATURAL1 & pp: NAT => (pp = 0 => nn**pp = 1) & (pp = 1 => nn**pp = nn) & (pp>1 => nn**pp = nn*nn**(pp-1))));
  Seen_Context_List_Properties(Machine(Z80))==(SCHAR_MAX: INTEGER & SCHAR_MIN: INTEGER & SCHAR_MAX = 2**7-1 & SCHAR_MIN = (-2)**7 & SCHAR = (-2)**7..2**7-1 & UCHAR = 0..2**8-1 & UCHAR_MAX: INTEGER & UCHAR_MIN: INTEGER & UCHAR_MAX = 2**8 & UCHAR_MIN = 0 & uchar_schar: UCHAR --> SCHAR & uchar_schar = %v1.(v1: UCHAR & v1<=SCHAR_MAX | v1) & uchar_schar = %v1.(v1: UCHAR & not(v1<=SCHAR_MAX) | v1-UCHAR_MAX+1) & schar_uchar: SCHAR --> UCHAR & schar_uchar = uchar_schar~ & SSHORTINT = (-2)**15..2**15-1 & byte_to_uchar: BYTE --> UCHAR & byte_to_uchar = %v0.(v0: BYTE | 2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & uchar_to_byte: UCHAR --> BYTE & uchar_to_byte = byte_to_uchar~ & update_refresh_reg: BYTE --> BYTE & update_refresh_reg = %v0.(v0: BYTE | uchar_to_byte(2**7*v0(7)+(2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) mod 64)) & USHORTINT = 0..2**16-1 & USHORTINT_MAX = 2**16-1 & USHORTINT_MIN = 0 & NB_INSTRUCTIONS: NATURAL & INST_SZ: NATURAL & INST_SZ = 16 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = USHORTINT & instruction_next: USHORTINT --> USHORTINT & instruction_next = %w1.(w1: USHORTINT | (w1+1) mod 65535) & instruction_jump = %(p0,e0).(p0: INSTRUCTION & e0: -126..129 | (p0+e0) mod 65535) & byte_to_schar: BYTE --> SCHAR & byte_to_schar = %v0.(v0: BYTE | (-2)**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & schar_to_byte: SCHAR --> BYTE & schar_to_byte = byte_to_schar~ & bv16_to_sshortint: BV16 --> SSHORTINT & bv16_to_sshortint = %v0.(v0: BV16 | (-2)**15*v0(15)+2**14*v0(14)+2**13*v0(13)+2**12*v0(12)+2**11*v0(11)+2**10*v0(10)+2**9*v0(9)+2**8*v0(8)+2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & sshortint_to_bv16: SSHORTINT --> BV16 & sshortint_to_bv16 = bv16_to_sshortint~ & byte_to_bv16: BYTE*BYTE --> BV16 & byte_to_bv16 = %(v1,v2).(v1: BV16 & v2: BV16 | {0|->v2(0),1|->v2(1),2|->v2(2),3|->v2(3),4|->v2(4),5|->v2(5),6|->v2(6),7|->v2(7),8|->v1(0),9|->v1(1),10|->v1(2),11|->v1(3),12|->v1(4),13|->v1(5),14|->v1(6),15|->v1(7)}) & bv16_to_byte: BV16 --> BYTE*BYTE & bv16_to_byte = byte_to_bv16~ & schar_to_sshortint: SCHAR*SCHAR --> SSHORTINT & schar_to_sshortint = %(w1,w2).(w1: SCHAR & w2: SCHAR | bv16_to_sshortint(byte_to_bv16(schar_to_byte(w1),schar_to_byte(w2)))) & sshortint_to_schar: SSHORTINT --> SCHAR*SCHAR & sshortint_to_schar = schar_to_sshortint~ & bv16_to_ushortint: BV16 --> USHORTINT & bv16_to_ushortint = %v0.(v0: BV16 | 2**15*v0(15)+2**14*v0(14)+2**13*v0(13)+2**12*v0(12)+2**11*v0(11)+2**10*v0(10)+2**9*v0(9)+2**8*v0(8)+2**7*v0(7)+2**6*v0(6)+2**5*v0(5)+2**4*v0(4)+2**3*v0(3)+2**2*v0(2)+2*v0(1)+v0(0)) & ushortint_to_bv16: USHORTINT --> BV16 & ushortint_to_bv16 = bv16_to_ushortint~ & sshortint_to_ushortint: SSHORTINT --> USHORTINT & sshortint_to_ushortint = %v0.(v0: SSHORTINT | v0-32768) & ushortint_to_sshortint: USHORTINT --> SSHORTINT & ushortint_to_sshortint = sshortint_to_ushortint~ & get_upper_digit = %by.(by: BYTE | 2**3*by(7)+2**2*by(6)+2*by(5)+by(4)) & get_lower_digit = %by.(by: BYTE | 2**3*by(3)+2**2*by(2)+2*by(1)+by(0)) & BIT = 0..1 & bit_not: BIT --> BIT & bit_not = %bb.(bb: BIT | 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = seq1(BIT) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_index: BIT_VECTOR --> POW(NATURAL) & bv_index = %v1.(v1: BIT_VECTOR | 0..bv_size(v1)-1) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: 0..bv_size(bv)-1 & high: low..bv_size(bv)-1 | %i0.(i0: 1..high-low+1 | bv(i0+low))) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (1..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (1..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 1..bv_size(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: 1..bv_size(v1) | bit_xor(v1(idx),v2(idx)))) & bv_get: BIT_VECTOR*NATURAL --> BIT & bv_get = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1(idx+1)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 | v1<+{idx+1|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: 0..bv_size(v1)-1 & bit: BIT | v1<+{idx+1|->bit}) & BYTE_WIDTH = 8 & BYTE_INDEX = 1..BYTE_WIDTH & REAL_BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE = {bt | bt: BIT_VECTOR & bv_size(bt) = BYTE_WIDTH} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & byte_bit_get: BYTE*REAL_BYTE_INDEX --> BIT & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)) & BV16_INDX = 1..16 & REAL_BV16_INDX = 0..16-1 & BV16 = {bt | bt: BIT_VECTOR & bv_size(bt) = 16} & BV16_ZERO = BV16_INDX*{0} & bv16_byte: BV16 --> BYTE*BYTE & bv16_byte = %bv.(bv: BV16 | {0|->bv(8),1|->bv(9),2|->bv(10),3|->bv(11),4|->bv(12),5|->bv(13),6|->bv(14),7|->bv(15)},{0|->bv(0),1|->bv(1),2|->bv(2),3|->bv(3),4|->bv(4),5|->bv(5),6|->bv(6),7|->bv(7)}) & byte_bv16 = bv16_byte~ & bv16_bit_get: BYTE*BV16_INDX --> BIT & byte_bit_get = %(bt,ind).(bt: BYTE & ind: REAL_BYTE_INDEX | bt(ind+1)));
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
  List_ANY_Var(Machine(Z80),LD_r_9HL0)==(Var(address) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),LD_r_9IX_d0)==(?);
  List_ANY_Var(Machine(Z80),LD_r_9IY_d0)==(?);
  List_ANY_Var(Machine(Z80),LD_9HL0_r)==(?);
  List_ANY_Var(Machine(Z80),LD_9IX_d0_r)==(?);
  List_ANY_Var(Machine(Z80),LD_9IY_d0_r)==(?);
  List_ANY_Var(Machine(Z80),LD_9HL0_n)==(?);
  List_ANY_Var(Machine(Z80),LD_9IX_d0_n)==(?);
  List_ANY_Var(Machine(Z80),LD_9IY_d0_n)==(?);
  List_ANY_Var(Machine(Z80),LD_A_9BC0)==(?);
  List_ANY_Var(Machine(Z80),LD_A_9DE0)==(?);
  List_ANY_Var(Machine(Z80),LD_A_9nn0)==(?);
  List_ANY_Var(Machine(Z80),LD_9BC0_A)==(?);
  List_ANY_Var(Machine(Z80),LD_9DE0_A)==(?);
  List_ANY_Var(Machine(Z80),LD_9nn0_A)==(?);
  List_ANY_Var(Machine(Z80),LD_A_I)==(Var(new_interrupt) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),LD_A_R)==(?);
  List_ANY_Var(Machine(Z80),LD_I_A)==(?);
  List_ANY_Var(Machine(Z80),LD_R_A)==(?);
  List_ANY_Var(Machine(Z80),LD_dd_nn)==((Var(rh) == etype(id_reg_8,?,?)),(Var(rl) == etype(id_reg_8,?,?)),(Var(w1) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(w2) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),LD_IX_nn)==(?);
  List_ANY_Var(Machine(Z80),LD_IY_nn)==(?);
  List_ANY_Var(Machine(Z80),LD_HL_9nn0)==(?);
  List_ANY_Var(Machine(Z80),LD_dd_9nn0)==((Var(rh) == etype(id_reg_8,?,?)),(Var(rl) == etype(id_reg_8,?,?)),(Var(w1) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(w2) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),LD_IX_9nn0)==(?);
  List_ANY_Var(Machine(Z80),LD_IY_9nn0)==(?);
  List_ANY_Var(Machine(Z80),LD_9nn0_HL)==(?);
  List_ANY_Var(Machine(Z80),LD_9nn0_dd)==((Var(vh) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(vl) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(rh) == etype(id_reg_8,?,?)),(Var(rl) == etype(id_reg_8,?,?)),(Var(w1) == btype(INTEGER,?,?)),(Var(w2) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),LD_9nn0_IX)==((Var(h_ix) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(l_ix) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),LD_9nn0_IY)==((Var(h_iy) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(l_iy) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),LD_SP_HL)==(?);
  List_ANY_Var(Machine(Z80),LD_SP_IX)==(?);
  List_ANY_Var(Machine(Z80),LD_SP_IY)==(?);
  List_ANY_Var(Machine(Z80),PUSH_qq)==((Var(qqh) == etype(id_reg_8,?,?)),(Var(qql) == etype(id_reg_8,?,?)));
  List_ANY_Var(Machine(Z80),PUSH_IX)==((Var(wh) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(wl) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),PUSH_IY)==((Var(wh) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(wl) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),POP_qq)==((Var(qqh) == etype(id_reg_8,?,?)),(Var(qql) == etype(id_reg_8,?,?)));
  List_ANY_Var(Machine(Z80),POP_IX)==(Var(nw8) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),POP_IY)==(Var(nw8) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),EX_DE_HL)==(?);
  List_ANY_Var(Machine(Z80),EX_AF_AF_)==(?);
  List_ANY_Var(Machine(Z80),EXX)==(?);
  List_ANY_Var(Machine(Z80),EX_9SP0_HL)==(?);
  List_ANY_Var(Machine(Z80),EX_9SP0_IX)==((Var(wh) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(wl) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),EX_9SP0_IY)==((Var(wh) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(wl) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),LDI)==((Var(hvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(dvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(evn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(bvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(cvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),LDIR)==((Var(hvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(dvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(evn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(bvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(cvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),LDD)==((Var(hvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(dvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(evn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(bvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(cvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),LDDR)==((Var(hvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(dvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(evn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(bvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(cvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),CPI)==((Var(sum) == btype(INTEGER,?,?)),(Var(negative) == btype(INTEGER,?,?)),(Var(carry) == btype(INTEGER,?,?)),(Var(half_carry) == btype(INTEGER,?,?)),(Var(zero) == btype(INTEGER,?,?)),(Var(hvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(bvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(cvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),CPIR)==((Var(sum) == btype(INTEGER,?,?)),(Var(negative) == btype(INTEGER,?,?)),(Var(carry) == btype(INTEGER,?,?)),(Var(half_carry) == btype(INTEGER,?,?)),(Var(zero) == btype(INTEGER,?,?)),(Var(hvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(bvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(cvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),CPD)==((Var(sum) == btype(INTEGER,?,?)),(Var(negative) == btype(INTEGER,?,?)),(Var(carry) == btype(INTEGER,?,?)),(Var(half_carry) == btype(INTEGER,?,?)),(Var(zero) == btype(INTEGER,?,?)),(Var(hvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(bvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(cvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),CPDR)==((Var(sum) == btype(INTEGER,?,?)),(Var(negative) == btype(INTEGER,?,?)),(Var(carry) == btype(INTEGER,?,?)),(Var(half_carry) == btype(INTEGER,?,?)),(Var(zero) == btype(INTEGER,?,?)),(Var(hvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(bvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(cvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));
  List_ANY_Var(Machine(Z80),DAA)==((Var(result) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(s0) == btype(INTEGER,?,?)),(Var(z0) == btype(INTEGER,?,?)),(Var(h0) == btype(INTEGER,?,?)),(Var(pv0) == btype(INTEGER,?,?)),(Var(n0) == btype(INTEGER,?,?)),(Var(c0) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),CPL)==(Var(result) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),NEG)==((Var(sum) == btype(INTEGER,?,?)),(Var(negative) == btype(INTEGER,?,?)),(Var(carry) == btype(INTEGER,?,?)),(Var(half_carry) == btype(INTEGER,?,?)),(Var(zero) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),CCF)==(?);
  List_ANY_Var(Machine(Z80),SCF)==(?);
  List_ANY_Var(Machine(Z80),NOP)==(?);
  List_ANY_Var(Machine(Z80),HALT)==(?);
  List_ANY_Var(Machine(Z80),DI)==(?);
  List_ANY_Var(Machine(Z80),EI)==(?);
  List_ANY_Var(Machine(Z80),IM0)==(?);
  List_ANY_Var(Machine(Z80),IM1)==(?);
  List_ANY_Var(Machine(Z80),IM2)==(?);
  List_ANY_Var(Machine(Z80),IN_A_9n0)==(Var(data_in) == btype(INTEGER,?,?));
  List_ANY_Var(Machine(Z80),IN_r_9C0)==((Var(data_in) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(negative) == btype(INTEGER,?,?)),(Var(zero) == btype(INTEGER,?,?)),(Var(half_carry) == btype(INTEGER,?,?)),(Var(pv) == btype(INTEGER,?,?)),(Var(add_sub) == btype(INTEGER,?,?)),(Var(carry) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),INI)==((Var(data_in) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(hvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(negative) == btype(INTEGER,?,?)),(Var(zero) == btype(INTEGER,?,?)),(Var(half_carry) == btype(INTEGER,?,?)),(Var(pv) == btype(INTEGER,?,?)),(Var(add_sub) == btype(INTEGER,?,?)),(Var(carry) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),INIR)==((Var(data_in) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(hvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(negative) == btype(INTEGER,?,?)),(Var(zero) == btype(INTEGER,?,?)),(Var(half_carry) == btype(INTEGER,?,?)),(Var(pv) == btype(INTEGER,?,?)),(Var(add_sub) == btype(INTEGER,?,?)),(Var(carry) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),IND)==((Var(data_in) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(hvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(negative) == btype(INTEGER,?,?)),(Var(zero) == btype(INTEGER,?,?)),(Var(half_carry) == btype(INTEGER,?,?)),(Var(pv) == btype(INTEGER,?,?)),(Var(add_sub) == btype(INTEGER,?,?)),(Var(carry) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),INDR)==((Var(data_in) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(hvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(negative) == btype(INTEGER,?,?)),(Var(zero) == btype(INTEGER,?,?)),(Var(half_carry) == btype(INTEGER,?,?)),(Var(pv) == btype(INTEGER,?,?)),(Var(add_sub) == btype(INTEGER,?,?)),(Var(carry) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),OUT_9n0_A)==(?);
  List_ANY_Var(Machine(Z80),OUT_9C0_r)==(?);
  List_ANY_Var(Machine(Z80),OUTI)==((Var(hvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(negative) == btype(INTEGER,?,?)),(Var(zero) == btype(INTEGER,?,?)),(Var(half_carry) == btype(INTEGER,?,?)),(Var(pv) == btype(INTEGER,?,?)),(Var(add_sub) == btype(INTEGER,?,?)),(Var(carry) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),OUTIR)==((Var(hvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(negative) == btype(INTEGER,?,?)),(Var(zero) == btype(INTEGER,?,?)),(Var(half_carry) == btype(INTEGER,?,?)),(Var(pv) == btype(INTEGER,?,?)),(Var(add_sub) == btype(INTEGER,?,?)),(Var(carry) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),OUTD)==((Var(hvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(negative) == btype(INTEGER,?,?)),(Var(zero) == btype(INTEGER,?,?)),(Var(half_carry) == btype(INTEGER,?,?)),(Var(pv) == btype(INTEGER,?,?)),(Var(add_sub) == btype(INTEGER,?,?)),(Var(carry) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),OUTDR)==((Var(hvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(lvn) == SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))),(Var(negative) == btype(INTEGER,?,?)),(Var(zero) == btype(INTEGER,?,?)),(Var(half_carry) == btype(INTEGER,?,?)),(Var(pv) == btype(INTEGER,?,?)),(Var(add_sub) == btype(INTEGER,?,?)),(Var(carry) == btype(INTEGER,?,?)));
  List_ANY_Var(Machine(Z80),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Z80)) == (get_bv_reg16,REG16_TO_REG8,REG8_TO_REG16,update_flag_register_SZ_H_PvNC,get_new_flag_register_SZ_H_PvNC,bv_ireg_plus_d,bv_9ireg_plus_d0,daa_function,id_reg_8,id_reg_16,a0,f0,f_0,a_0,b0,c0,b_0,c_0,d0,e0,d_0,e_0,h0,l0,h_0,l_0,i0,r0,BC,DE,HL,SP,AF | RESERVED_ADDRESS | i_o_ports,im,iff2,iff1,r_,i_,iy,ix,sp,pc,rgs8 | stack,mem | LD_r_r_,LD_r_n_,LD_r_9HL0,LD_r_9IX_d0,LD_r_9IY_d0,LD_9HL0_r,LD_9IX_d0_r,LD_9IY_d0_r,LD_9HL0_n,LD_9IX_d0_n,LD_9IY_d0_n,LD_A_9BC0,LD_A_9DE0,LD_A_9nn0,LD_9BC0_A,LD_9DE0_A,LD_9nn0_A,LD_A_I,LD_A_R,LD_I_A,LD_R_A,LD_dd_nn,LD_IX_nn,LD_IY_nn,LD_HL_9nn0,LD_dd_9nn0,LD_IX_9nn0,LD_IY_9nn0,LD_9nn0_HL,LD_9nn0_dd,LD_9nn0_IX,LD_9nn0_IY,LD_SP_HL,LD_SP_IX,LD_SP_IY,PUSH_qq,PUSH_IX,PUSH_IY,POP_qq,POP_IX,POP_IY,EX_DE_HL,EX_AF_AF_,EXX,EX_9SP0_HL,EX_9SP0_IX,EX_9SP0_IY,LDI,LDIR,LDD,LDDR,CPI,CPIR,CPD,CPDR,DAA,CPL,NEG,CCF,SCF,NOP,HALT,DI,EI,IM0,IM1,IM2,IN_A_9n0,IN_r_9C0,INI,INIR,IND,INDR,OUT_9n0_A,OUT_9C0_r,OUTI,OUTIR,OUTD,OUTDR | ? | seen(Machine(TYPES)),seen(Machine(ALU)),seen(Machine(POWER2)),included(Machine(MEMORY)) | ? | Z80);
  List_Of_HiddenCst_Ids(Machine(Z80)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Z80)) == (get_bv_reg16,REG16_TO_REG8,REG8_TO_REG16,update_flag_register_SZ_H_PvNC,get_new_flag_register_SZ_H_PvNC,bv_ireg_plus_d,bv_9ireg_plus_d0,daa_function,RESERVED_ADDRESS);
  List_Of_VisibleVar_Ids(Machine(Z80)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Z80)) == (seen(Machine(TYPES)): (SCHAR,SCHAR_MAX,SCHAR_MIN,byte_to_schar,schar_to_byte,UCHAR,UCHAR_MAX,UCHAR_MIN,uchar_to_byte,byte_to_uchar,update_refresh_reg,uchar_schar,schar_uchar,SSHORTINT,bv16_to_sshortint,sshortint_to_bv16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,instruction_jump,byte_to_bv16,bv16_to_byte,schar_to_sshortint,sshortint_to_schar,USHORTINT,USHORTINT_MAX,USHORTINT_MIN,bv16_to_ushortint,ushortint_to_bv16,ushortint_to_sshortint,sshortint_to_ushortint,get_upper_digit,get_lower_digit | BV16_INDX,REAL_BV16_INDX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get,BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index,BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(ALU)): (is_zero,is_zeroUSHORTINT,is_negative,half,simple_add8UCHAR,add8UCHAR,substract8UCHAR,add16USHORTINT,add_carryUSHORTINT,add_halfcarryUSHORTINT,sub16USHORTINT,sub_carryUSHORTINT,sub_halfcarryUSHORTINT,inc_BYTE,dec_BYTE,inc_BV16,dec_BV16,parity_even_BYTE,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(MEMORY)) == (RESERVED_ADDRESS | ? | stack,mem | ? | updateMem,updateAddressMem,updateStack,updateAddressStack,pop | ? | seen(Machine(TYPES)),seen(Machine(ALU)) | ? | MEMORY);
  List_Of_HiddenCst_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(MEMORY)) == (RESERVED_ADDRESS);
  List_Of_VisibleVar_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(MEMORY)) == (?: ?);
  List_Of_Ids(Machine(ALU)) == (is_zero,is_zeroUSHORTINT,is_negative,half,simple_add8UCHAR,add8UCHAR,substract8UCHAR,add16USHORTINT,add_carryUSHORTINT,add_halfcarryUSHORTINT,sub16USHORTINT,sub_carryUSHORTINT,sub_halfcarryUSHORTINT,inc_BYTE,dec_BYTE,inc_BV16,dec_BV16,parity_even_BYTE,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright | ? | ? | ? | ? | ? | seen(Machine(TYPES)) | ? | ALU);
  List_Of_HiddenCst_Ids(Machine(ALU)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ALU)) == (is_zero,is_zeroUSHORTINT,is_negative,half,simple_add8UCHAR,add8UCHAR,substract8UCHAR,add16USHORTINT,add_carryUSHORTINT,add_halfcarryUSHORTINT,sub16USHORTINT,sub_carryUSHORTINT,sub_halfcarryUSHORTINT,inc_BYTE,dec_BYTE,inc_BV16,dec_BV16,parity_even_BYTE,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright);
  List_Of_VisibleVar_Ids(Machine(ALU)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ALU)) == (?: ?);
  List_Of_Ids(Machine(TYPES)) == (SCHAR,SCHAR_MAX,SCHAR_MIN,byte_to_schar,schar_to_byte,UCHAR,UCHAR_MAX,UCHAR_MIN,uchar_to_byte,byte_to_uchar,update_refresh_reg,uchar_schar,schar_uchar,SSHORTINT,bv16_to_sshortint,sshortint_to_bv16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,instruction_jump,byte_to_bv16,bv16_to_byte,schar_to_sshortint,sshortint_to_schar,USHORTINT,USHORTINT_MAX,USHORTINT_MIN,bv16_to_ushortint,ushortint_to_bv16,ushortint_to_sshortint,sshortint_to_ushortint,get_upper_digit,get_lower_digit | BV16_INDX,REAL_BV16_INDX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get,BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index,BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | seen(Machine(POWER2)),included(Machine(BIT_DEFINITION)),included(Machine(BIT_VECTOR_DEFINITION)),included(Machine(BYTE_DEFINITION)),included(Machine(BIT_VECTOR16_DEFINITION)) | ? | TYPES);
  List_Of_HiddenCst_Ids(Machine(TYPES)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (SCHAR,SCHAR_MAX,SCHAR_MIN,byte_to_schar,schar_to_byte,UCHAR,UCHAR_MAX,UCHAR_MIN,uchar_to_byte,byte_to_uchar,update_refresh_reg,uchar_schar,schar_uchar,SSHORTINT,bv16_to_sshortint,sshortint_to_bv16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,instruction_next,instruction_jump,byte_to_bv16,bv16_to_byte,schar_to_sshortint,sshortint_to_schar,USHORTINT,USHORTINT_MAX,USHORTINT_MIN,bv16_to_ushortint,ushortint_to_bv16,ushortint_to_sshortint,sshortint_to_ushortint,get_upper_digit,get_lower_digit,BV16_INDX,REAL_BV16_INDX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get,BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index,BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit);
  List_Of_VisibleVar_Ids(Machine(TYPES)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPES)) == (seen(Machine(BIT_DEFINITION)): (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(BIT_VECTOR_DEFINITION)): (BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index | ? | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(BYTE_DEFINITION)): (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(BIT_VECTOR16_DEFINITION)) == (BV16_INDX,REAL_BV16_INDX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BYTE_DEFINITION)) | ? | BIT_VECTOR16_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR16_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR16_DEFINITION)) == (BV16_INDX,REAL_BV16_INDX,BV16_ZERO,BV16,bv16_byte,byte_bv16,bv16_bit_get);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR16_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR16_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)) | ? | BYTE_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,REAL_BYTE_INDEX,byte_bit_get);
  List_Of_VisibleVar_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BYTE_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)) | ? | BIT_VECTOR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_get,bv_set,bv_clear,bv_put,bv_index);
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
  Constants(Machine(Z80)) == (Type(RESERVED_ADDRESS) == Cst(SetOf(btype(INTEGER,"[RESERVED_ADDRESS","]RESERVED_ADDRESS")));Type(a0) == Cst(etype(id_reg_8,0,17));Type(f0) == Cst(etype(id_reg_8,0,17));Type(f_0) == Cst(etype(id_reg_8,0,17));Type(a_0) == Cst(etype(id_reg_8,0,17));Type(b0) == Cst(etype(id_reg_8,0,17));Type(c0) == Cst(etype(id_reg_8,0,17));Type(b_0) == Cst(etype(id_reg_8,0,17));Type(c_0) == Cst(etype(id_reg_8,0,17));Type(d0) == Cst(etype(id_reg_8,0,17));Type(e0) == Cst(etype(id_reg_8,0,17));Type(d_0) == Cst(etype(id_reg_8,0,17));Type(e_0) == Cst(etype(id_reg_8,0,17));Type(h0) == Cst(etype(id_reg_8,0,17));Type(l0) == Cst(etype(id_reg_8,0,17));Type(h_0) == Cst(etype(id_reg_8,0,17));Type(l_0) == Cst(etype(id_reg_8,0,17));Type(i0) == Cst(etype(id_reg_8,0,17));Type(r0) == Cst(etype(id_reg_8,0,17));Type(BC) == Cst(etype(id_reg_16,0,4));Type(DE) == Cst(etype(id_reg_16,0,4));Type(HL) == Cst(etype(id_reg_16,0,4));Type(SP) == Cst(etype(id_reg_16,0,4));Type(AF) == Cst(etype(id_reg_16,0,4));Type(get_bv_reg16) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(etype(id_reg_8,0,17)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*etype(id_reg_16,0,4)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(REG16_TO_REG8) == Cst(SetOf(etype(id_reg_16,0,4)*(etype(id_reg_8,0,17)*etype(id_reg_8,0,17))));Type(REG8_TO_REG16) == Cst(SetOf(etype(id_reg_8,?,?)*etype(id_reg_8,?,?)*etype(id_reg_16,?,?)));Type(update_flag_register_SZ_H_PvNC) == Cst(SetOf(SetOf(etype(id_reg_8,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*SetOf(etype(id_reg_8,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))));Type(get_new_flag_register_SZ_H_PvNC) == Cst(SetOf(SetOf(etype(id_reg_8,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*btype(INTEGER,?,?)*(etype(id_reg_8,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))));Type(bv_ireg_plus_d) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(bv_9ireg_plus_d0) == Cst(SetOf(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,?,?)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(daa_function) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BIT","]BIT")*(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")))))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Z80)) == (Type(mem) == Mvl(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(stack) == Mvl(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(i_o_ports) == Mvl(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(im) == Mvl(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(iff2) == Mvl(btype(INTEGER,?,?));Type(iff1) == Mvl(btype(INTEGER,?,?));Type(r_) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(i_) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(iy) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(ix) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(sp) == Mvl(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)));Type(pc) == Mvl(btype(INTEGER,?,?));Type(rgs8) == Mvl(SetOf(etype(id_reg_8,0,17)*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Z80)) == (Type(OUTDR) == Cst(No_type,No_type);Type(OUTD) == Cst(No_type,No_type);Type(OUTIR) == Cst(No_type,No_type);Type(OUTI) == Cst(No_type,No_type);Type(OUT_9C0_r) == Cst(No_type,etype(id_reg_8,?,?));Type(OUT_9n0_A) == Cst(No_type,btype(INTEGER,?,?));Type(INDR) == Cst(No_type,No_type);Type(IND) == Cst(No_type,No_type);Type(INIR) == Cst(No_type,No_type);Type(INI) == Cst(No_type,No_type);Type(IN_r_9C0) == Cst(No_type,etype(id_reg_8,?,?));Type(IN_A_9n0) == Cst(No_type,btype(INTEGER,?,?));Type(IM2) == Cst(No_type,No_type);Type(IM1) == Cst(No_type,No_type);Type(IM0) == Cst(No_type,No_type);Type(EI) == Cst(No_type,No_type);Type(DI) == Cst(No_type,No_type);Type(HALT) == Cst(No_type,No_type);Type(NOP) == Cst(No_type,No_type);Type(SCF) == Cst(No_type,No_type);Type(CCF) == Cst(No_type,No_type);Type(NEG) == Cst(No_type,No_type);Type(CPL) == Cst(No_type,No_type);Type(DAA) == Cst(No_type,No_type);Type(CPDR) == Cst(No_type,No_type);Type(CPD) == Cst(No_type,No_type);Type(CPIR) == Cst(No_type,No_type);Type(CPI) == Cst(No_type,No_type);Type(LDDR) == Cst(No_type,No_type);Type(LDD) == Cst(No_type,No_type);Type(LDIR) == Cst(No_type,No_type);Type(LDI) == Cst(No_type,No_type);Type(EX_9SP0_IY) == Cst(No_type,No_type);Type(EX_9SP0_IX) == Cst(No_type,No_type);Type(EX_9SP0_HL) == Cst(No_type,No_type);Type(EXX) == Cst(No_type,No_type);Type(EX_AF_AF_) == Cst(No_type,No_type);Type(EX_DE_HL) == Cst(No_type,No_type);Type(POP_IY) == Cst(No_type,No_type);Type(POP_IX) == Cst(No_type,No_type);Type(POP_qq) == Cst(No_type,etype(id_reg_16,?,?));Type(PUSH_IY) == Cst(No_type,No_type);Type(PUSH_IX) == Cst(No_type,No_type);Type(PUSH_qq) == Cst(No_type,etype(id_reg_16,?,?));Type(LD_SP_IY) == Cst(No_type,No_type);Type(LD_SP_IX) == Cst(No_type,No_type);Type(LD_SP_HL) == Cst(No_type,No_type);Type(LD_9nn0_IY) == Cst(No_type,btype(INTEGER,?,?));Type(LD_9nn0_IX) == Cst(No_type,btype(INTEGER,?,?));Type(LD_9nn0_dd) == Cst(No_type,btype(INTEGER,?,?)*etype(id_reg_16,?,?));Type(LD_9nn0_HL) == Cst(No_type,btype(INTEGER,?,?));Type(LD_IY_9nn0) == Cst(No_type,btype(INTEGER,?,?));Type(LD_IX_9nn0) == Cst(No_type,btype(INTEGER,?,?));Type(LD_dd_9nn0) == Cst(No_type,etype(id_reg_16,?,?)*btype(INTEGER,?,?));Type(LD_HL_9nn0) == Cst(No_type,btype(INTEGER,?,?));Type(LD_IY_nn) == Cst(No_type,btype(INTEGER,?,?));Type(LD_IX_nn) == Cst(No_type,btype(INTEGER,?,?));Type(LD_dd_nn) == Cst(No_type,etype(id_reg_16,?,?)*btype(INTEGER,?,?));Type(LD_R_A) == Cst(No_type,No_type);Type(LD_I_A) == Cst(No_type,No_type);Type(LD_A_R) == Cst(No_type,No_type);Type(LD_A_I) == Cst(No_type,No_type);Type(LD_9nn0_A) == Cst(No_type,btype(INTEGER,?,?));Type(LD_9DE0_A) == Cst(No_type,No_type);Type(LD_9BC0_A) == Cst(No_type,No_type);Type(LD_A_9nn0) == Cst(No_type,btype(INTEGER,?,?));Type(LD_A_9DE0) == Cst(No_type,No_type);Type(LD_A_9BC0) == Cst(No_type,No_type);Type(LD_9IY_d0_n) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(LD_9IX_d0_n) == Cst(No_type,btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(LD_9HL0_n) == Cst(No_type,btype(INTEGER,?,?));Type(LD_9IY_d0_r) == Cst(No_type,btype(INTEGER,?,?)*etype(id_reg_8,?,?));Type(LD_9IX_d0_r) == Cst(No_type,btype(INTEGER,?,?)*etype(id_reg_8,?,?));Type(LD_9HL0_r) == Cst(No_type,etype(id_reg_8,?,?));Type(LD_r_9IY_d0) == Cst(No_type,etype(id_reg_8,?,?)*btype(INTEGER,?,?));Type(LD_r_9IX_d0) == Cst(No_type,etype(id_reg_8,?,?)*btype(INTEGER,?,?));Type(LD_r_9HL0) == Cst(No_type,etype(id_reg_8,?,?));Type(LD_r_n_) == Cst(No_type,etype(id_reg_8,?,?)*btype(INTEGER,?,?));Type(LD_r_r_) == Cst(No_type,etype(id_reg_8,?,?)*etype(id_reg_8,?,?)))
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
