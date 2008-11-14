Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(Z80LogAri))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(Z80LogAri))==(Machine(Z80LogAri));
  Level(Machine(Z80LogAri))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(Z80LogAri)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(Z80LogAri))==(TYPES,ALU,POWER2)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(Z80LogAri))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(Z80LogAri))==(MEMORY);
  List_Includes(Machine(Z80LogAri))==(MEMORY)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(Z80LogAri))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(Z80LogAri))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(Z80LogAri))==(?);
  Context_List_Variables(Machine(Z80LogAri))==(?);
  Abstract_List_Variables(Machine(Z80LogAri))==(?);
  Local_List_Variables(Machine(Z80LogAri))==(i_o_ports,iy,ix,sp,pc,rgs8);
  List_Variables(Machine(Z80LogAri))==(i_o_ports,iy,ix,sp,pc,rgs8,stack,mem);
  External_List_Variables(Machine(Z80LogAri))==(i_o_ports,iy,ix,sp,pc,rgs8,stack,mem)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(Z80LogAri))==(?);
  Abstract_List_VisibleVariables(Machine(Z80LogAri))==(?);
  External_List_VisibleVariables(Machine(Z80LogAri))==(?);
  Expanded_List_VisibleVariables(Machine(Z80LogAri))==(?);
  List_VisibleVariables(Machine(Z80LogAri))==(?);
  Internal_List_VisibleVariables(Machine(Z80LogAri))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(Z80LogAri))==(btrue);
  Gluing_List_Invariant(Machine(Z80LogAri))==(btrue);
  Abstract_List_Invariant(Machine(Z80LogAri))==(btrue);
  Expanded_List_Invariant(Machine(Z80LogAri))==(stack: USHORTINT --> SCHAR & mem: USHORTINT --> SCHAR);
  Context_List_Invariant(Machine(Z80LogAri))==(btrue);
  List_Invariant(Machine(Z80LogAri))==(stack: USHORTINT --> SCHAR & rgs8: id_reg_8 --> SCHAR & pc: USHORTINT & sp: USHORTINT & ix: USHORTINT & iy: USHORTINT & i_o_ports: UCHAR --> SCHAR)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Machine(Z80LogAri))==(btrue);
  Expanded_List_Assertions(Machine(Z80LogAri))==(btrue);
  Context_List_Assertions(Machine(Z80LogAri))==(NB_SCHARS = 256 & !n0.(n0: SCHAR => 0<=n0) & !n0.(n0: SCHAR => n0<=255) & 2**16 = 65536 & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & 0 = SCHAR_TO_SSHORTINT(0,0) & bit_not(0) = 1 & bit_not(1) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_and(0,0) = 0 & bit_and(0,1) = 0 & bit_and(1,0) = 0 & bit_and(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !b1.(b1: BIT => bit_and(b1,0) = 0) & bit_or(0,0) = 0 & bit_or(0,1) = 1 & bit_or(1,0) = 1 & bit_or(1,1) = 1 & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(0,b1) = b1) & bit_xor(0,0) = 0 & bit_xor(0,1) = 1 & bit_xor(1,0) = 1 & bit_xor(1,1) = 0 & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & bool_to_bit(TRUE) = 1 & bool_to_bit(FALSE) = 0 & !bb.(bb: BIT => bb = 0 or bb = 1) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_not(bv_not(bv))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_and(v1,v2)(indx) = bv_and(v2,v1)(indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_and(v1,bv_and(v2,v3))(indx) = bv_and(bv_and(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_zero(bv_size(bv)))(indx) = bv_zero(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_one(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_or(v1,v2)(indx) = bv_or(v2,v1)(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_or(v1,bv_or(v2,v3))(indx) = bv_or(bv_or(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_one(bv_size(bv)))(indx) = bv_one(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_zero(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_xor(v1,v2)(indx) = bv_xor(v2,v1)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_xor(bv,bv)(indx) = bv_zero(bv_size(bv))(indx))) & dom(add8SCHAR) = SCHAR*SCHAR & ran(add8SCHAR): POW(SCHAR*BOOL*BOOL*BOOL*BOOL) & dom(substract8SCHAR) = SCHAR*SCHAR & ran(substract8SCHAR): POW(SCHAR*BOOL*BOOL*BOOL*BOOL) & dom(andSCHAR) = SCHAR*SCHAR & ran(andSCHAR) <: SCHAR*BOOL & dom(iorSCHAR) = SCHAR*SCHAR & ran(iorSCHAR) <: SCHAR*BOOL & dom(xorSCHAR) = SCHAR*SCHAR & ran(xorSCHAR) <: SCHAR*BOOL & dom(bitclearSCHAR) = SCHAR*SCHAR_POSITION & ran(bitclearSCHAR) <: SCHAR & dom(bitsetSCHAR) = SCHAR*SCHAR_POSITION & ran(bitsetSCHAR) <: SCHAR & dom(bitgetSCHAR) = SCHAR*SCHAR_POSITION & ran(bitgetSCHAR) <: BIT & dom(complementSCHAR) = SCHAR & ran(complementSCHAR) <: SCHAR & dom(swapSCHAR) = SCHAR & ran(swapSCHAR) <: SCHAR & ran(rotateleftSCHAR) <: SCHAR*BOOL & dom(rotateleftSCHAR) = SCHAR & dom(rotaterightSCHAR) = SCHAR & ran(rotaterightSCHAR) <: SCHAR*BOOL & !(w0,idx).(w0: SCHAR & idx: SCHAR_POSITION => bitgetSCHAR(w0,idx) = SCHAR_TO_BYTE(w0)(idx)) & max(SCHAR) = 127 & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536));
  List_Assertions(Machine(Z80LogAri))==(dom(stack) = USHORTINT & ran(stack) <: SCHAR & ran(mem) <: SCHAR & dom(mem) = USHORTINT & ran(rgs8) <: SCHAR & dom(rgs8) = id_reg_8 & INSTRUCTION_NEXT(0) = 1 & INSTRUCTION_NEXT(1) = 2 & INSTRUCTION_NEXT(2) = 3 & INSTRUCTION_NEXT(3) = 4 & INSTRUCTION_NEXT(4) = 5 & INSTRUCTION_NEXT(5) = 6 & INSTRUCTION_NEXT(6) = 7 & INSTRUCTION_NEXT(7) = 8 & INSTRUCTION_NEXT(8) = 9 & INSTRUCTION_NEXT(9) = 10 & INSTRUCTION_NEXT(10) = 11 & INSTRUCTION_NEXT(11) = 12 & INSTRUCTION_NEXT(12) = 13 & INSTRUCTION_NEXT(13) = 14 & !(vec,in0).(vec: BYTE & in0: 0..7 => bitgetSCHAR(BYTE_TO_SCHAR(vec),in0) = vec(in0)) & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0)))): SCHAR & mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0)))))): SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0)))): SCHAR & mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0)))))): SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))): SCHAR & mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))))): SCHAR & mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(a0),rgs8(f0)))): SCHAR & mem(SSHORTINT_TO_USHORTINT(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(a0),rgs8(f0)))))): SCHAR & mem(SSHORTINT_TO_USHORTINT(sp)): SCHAR & mem(SSHORTINT_TO_USHORTINT(ix)): SCHAR & mem(SSHORTINT_TO_USHORTINT(iy)): SCHAR)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(Z80LogAri))==(@(mem$0).(mem$0: USHORTINT --> SCHAR ==> mem:=mem$0) || @(stack$0).(stack$0: USHORTINT --> SCHAR ==> stack:=stack$0);(@(rgs8$0).(rgs8$0: id_reg_8 -->> SCHAR ==> rgs8:=rgs8$0) || @(pc$0).(pc$0: USHORTINT ==> pc:=pc$0) || @(sp$0).(sp$0: SSHORTINT ==> sp:=sp$0) || @(ix$0).(ix$0: SSHORTINT ==> ix:=ix$0) || @(iy$0).(iy$0: SSHORTINT ==> iy:=iy$0) || @(i_o_ports$0).(i_o_ports$0: UCHAR --> SCHAR ==> i_o_ports:=i_o_ports$0)));
  Context_List_Initialisation(Machine(Z80LogAri))==(skip);
  List_Initialisation(Machine(Z80LogAri))==(rgs8:: id_reg_8 -->> SCHAR || pc:: USHORTINT || sp:: SSHORTINT || ix:: SSHORTINT || iy:: SSHORTINT || i_o_ports:: UCHAR --> SCHAR)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(Z80LogAri))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(Z80LogAri),Machine(MEMORY))==(?);
  List_Instanciated_Parameters(Machine(Z80LogAri),Machine(TYPES))==(?);
  List_Instanciated_Parameters(Machine(Z80LogAri),Machine(ALU))==(?);
  List_Instanciated_Parameters(Machine(Z80LogAri),Machine(POWER2))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Machine(Z80LogAri),Machine(MEMORY))==(btrue);
  List_Context_Constraints(Machine(Z80LogAri))==(btrue);
  List_Constraints(Machine(Z80LogAri))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(Z80LogAri))==(ADD_A_r,ADD_A_n,ADD_A_9HL0,ADD_A_9IX_d0,ADD_A_9IY_d0,ADC_A_r,ADC_A_n,ADC_A_9HL0,ADC_A_9IX_d0,ADC_A_9IY_d0,SUB_A_r,SUB_A_n,SUB_A_9HL0,SUB_A_9IX_d0,SUB_A_9IY_d0,SBC_A_r,SBC_A_n,SBC_A_9HL0,SBC_A_9IX_d0,SBC_A_9IY_d0,AND_A_r,AND_A_n,AND_A_9HL0,AND_A_9IX_d0,AND_A_9IY_d0,OR_A_r,OR_A_n,OR_A_9HL0,OR_A_9IX_d0,OR_A_9IY_d0,XOR_A_r,XOR_A_n,XOR_A_9HL0,XOR_A_9IX_d0,XOR_A_9IY_d0,CP_A_r,CP_A_n,CP_A_9HL0,CP_A_9IX_d0,CP_A_9IY_d0,INC_r,INC_9HL0,INC_9IX_d0,INC_9IY_d0,DEC_r,DEC_9HL0,DEC_9IX_d0,DEC_9IY_d0);
  List_Operations(Machine(Z80LogAri))==(ADD_A_r,ADD_A_n,ADD_A_9HL0,ADD_A_9IX_d0,ADD_A_9IY_d0,ADC_A_r,ADC_A_n,ADC_A_9HL0,ADC_A_9IX_d0,ADC_A_9IY_d0,SUB_A_r,SUB_A_n,SUB_A_9HL0,SUB_A_9IX_d0,SUB_A_9IY_d0,SBC_A_r,SBC_A_n,SBC_A_9HL0,SBC_A_9IX_d0,SBC_A_9IY_d0,AND_A_r,AND_A_n,AND_A_9HL0,AND_A_9IX_d0,AND_A_9IY_d0,OR_A_r,OR_A_n,OR_A_9HL0,OR_A_9IX_d0,OR_A_9IY_d0,XOR_A_r,XOR_A_n,XOR_A_9HL0,XOR_A_9IX_d0,XOR_A_9IY_d0,CP_A_r,CP_A_n,CP_A_9HL0,CP_A_9IX_d0,CP_A_9IY_d0,INC_r,INC_9HL0,INC_9IX_d0,INC_9IY_d0,DEC_r,DEC_9HL0,DEC_9IX_d0,DEC_9IY_d0)
END
&
THEORY ListInputX IS
  List_Input(Machine(Z80LogAri),ADD_A_r)==(rr);
  List_Input(Machine(Z80LogAri),ADD_A_n)==(nn);
  List_Input(Machine(Z80LogAri),ADD_A_9HL0)==(?);
  List_Input(Machine(Z80LogAri),ADD_A_9IX_d0)==(?);
  List_Input(Machine(Z80LogAri),ADD_A_9IY_d0)==(?);
  List_Input(Machine(Z80LogAri),ADC_A_r)==(rr);
  List_Input(Machine(Z80LogAri),ADC_A_n)==(nn);
  List_Input(Machine(Z80LogAri),ADC_A_9HL0)==(?);
  List_Input(Machine(Z80LogAri),ADC_A_9IX_d0)==(?);
  List_Input(Machine(Z80LogAri),ADC_A_9IY_d0)==(?);
  List_Input(Machine(Z80LogAri),SUB_A_r)==(rr);
  List_Input(Machine(Z80LogAri),SUB_A_n)==(nn);
  List_Input(Machine(Z80LogAri),SUB_A_9HL0)==(?);
  List_Input(Machine(Z80LogAri),SUB_A_9IX_d0)==(?);
  List_Input(Machine(Z80LogAri),SUB_A_9IY_d0)==(?);
  List_Input(Machine(Z80LogAri),SBC_A_r)==(rr);
  List_Input(Machine(Z80LogAri),SBC_A_n)==(nn);
  List_Input(Machine(Z80LogAri),SBC_A_9HL0)==(?);
  List_Input(Machine(Z80LogAri),SBC_A_9IX_d0)==(?);
  List_Input(Machine(Z80LogAri),SBC_A_9IY_d0)==(?);
  List_Input(Machine(Z80LogAri),AND_A_r)==(rr);
  List_Input(Machine(Z80LogAri),AND_A_n)==(nn);
  List_Input(Machine(Z80LogAri),AND_A_9HL0)==(?);
  List_Input(Machine(Z80LogAri),AND_A_9IX_d0)==(?);
  List_Input(Machine(Z80LogAri),AND_A_9IY_d0)==(?);
  List_Input(Machine(Z80LogAri),OR_A_r)==(rr);
  List_Input(Machine(Z80LogAri),OR_A_n)==(nn);
  List_Input(Machine(Z80LogAri),OR_A_9HL0)==(?);
  List_Input(Machine(Z80LogAri),OR_A_9IX_d0)==(?);
  List_Input(Machine(Z80LogAri),OR_A_9IY_d0)==(?);
  List_Input(Machine(Z80LogAri),XOR_A_r)==(rr);
  List_Input(Machine(Z80LogAri),XOR_A_n)==(nn);
  List_Input(Machine(Z80LogAri),XOR_A_9HL0)==(?);
  List_Input(Machine(Z80LogAri),XOR_A_9IX_d0)==(?);
  List_Input(Machine(Z80LogAri),XOR_A_9IY_d0)==(?);
  List_Input(Machine(Z80LogAri),CP_A_r)==(rr);
  List_Input(Machine(Z80LogAri),CP_A_n)==(nn);
  List_Input(Machine(Z80LogAri),CP_A_9HL0)==(?);
  List_Input(Machine(Z80LogAri),CP_A_9IX_d0)==(?);
  List_Input(Machine(Z80LogAri),CP_A_9IY_d0)==(?);
  List_Input(Machine(Z80LogAri),INC_r)==(rr);
  List_Input(Machine(Z80LogAri),INC_9HL0)==(?);
  List_Input(Machine(Z80LogAri),INC_9IX_d0)==(?);
  List_Input(Machine(Z80LogAri),INC_9IY_d0)==(?);
  List_Input(Machine(Z80LogAri),DEC_r)==(rr);
  List_Input(Machine(Z80LogAri),DEC_9HL0)==(?);
  List_Input(Machine(Z80LogAri),DEC_9IX_d0)==(?);
  List_Input(Machine(Z80LogAri),DEC_9IY_d0)==(?)
END
&
THEORY ListOutputX IS
  List_Output(Machine(Z80LogAri),ADD_A_r)==(?);
  List_Output(Machine(Z80LogAri),ADD_A_n)==(?);
  List_Output(Machine(Z80LogAri),ADD_A_9HL0)==(?);
  List_Output(Machine(Z80LogAri),ADD_A_9IX_d0)==(?);
  List_Output(Machine(Z80LogAri),ADD_A_9IY_d0)==(?);
  List_Output(Machine(Z80LogAri),ADC_A_r)==(?);
  List_Output(Machine(Z80LogAri),ADC_A_n)==(?);
  List_Output(Machine(Z80LogAri),ADC_A_9HL0)==(?);
  List_Output(Machine(Z80LogAri),ADC_A_9IX_d0)==(?);
  List_Output(Machine(Z80LogAri),ADC_A_9IY_d0)==(?);
  List_Output(Machine(Z80LogAri),SUB_A_r)==(?);
  List_Output(Machine(Z80LogAri),SUB_A_n)==(?);
  List_Output(Machine(Z80LogAri),SUB_A_9HL0)==(?);
  List_Output(Machine(Z80LogAri),SUB_A_9IX_d0)==(?);
  List_Output(Machine(Z80LogAri),SUB_A_9IY_d0)==(?);
  List_Output(Machine(Z80LogAri),SBC_A_r)==(?);
  List_Output(Machine(Z80LogAri),SBC_A_n)==(?);
  List_Output(Machine(Z80LogAri),SBC_A_9HL0)==(?);
  List_Output(Machine(Z80LogAri),SBC_A_9IX_d0)==(?);
  List_Output(Machine(Z80LogAri),SBC_A_9IY_d0)==(?);
  List_Output(Machine(Z80LogAri),AND_A_r)==(?);
  List_Output(Machine(Z80LogAri),AND_A_n)==(?);
  List_Output(Machine(Z80LogAri),AND_A_9HL0)==(?);
  List_Output(Machine(Z80LogAri),AND_A_9IX_d0)==(?);
  List_Output(Machine(Z80LogAri),AND_A_9IY_d0)==(?);
  List_Output(Machine(Z80LogAri),OR_A_r)==(?);
  List_Output(Machine(Z80LogAri),OR_A_n)==(?);
  List_Output(Machine(Z80LogAri),OR_A_9HL0)==(?);
  List_Output(Machine(Z80LogAri),OR_A_9IX_d0)==(?);
  List_Output(Machine(Z80LogAri),OR_A_9IY_d0)==(?);
  List_Output(Machine(Z80LogAri),XOR_A_r)==(?);
  List_Output(Machine(Z80LogAri),XOR_A_n)==(?);
  List_Output(Machine(Z80LogAri),XOR_A_9HL0)==(?);
  List_Output(Machine(Z80LogAri),XOR_A_9IX_d0)==(?);
  List_Output(Machine(Z80LogAri),XOR_A_9IY_d0)==(?);
  List_Output(Machine(Z80LogAri),CP_A_r)==(?);
  List_Output(Machine(Z80LogAri),CP_A_n)==(?);
  List_Output(Machine(Z80LogAri),CP_A_9HL0)==(?);
  List_Output(Machine(Z80LogAri),CP_A_9IX_d0)==(?);
  List_Output(Machine(Z80LogAri),CP_A_9IY_d0)==(?);
  List_Output(Machine(Z80LogAri),INC_r)==(?);
  List_Output(Machine(Z80LogAri),INC_9HL0)==(?);
  List_Output(Machine(Z80LogAri),INC_9IX_d0)==(?);
  List_Output(Machine(Z80LogAri),INC_9IY_d0)==(?);
  List_Output(Machine(Z80LogAri),DEC_r)==(?);
  List_Output(Machine(Z80LogAri),DEC_9HL0)==(?);
  List_Output(Machine(Z80LogAri),DEC_9IX_d0)==(?);
  List_Output(Machine(Z80LogAri),DEC_9IY_d0)==(?)
END
&
THEORY ListHeaderX IS
  List_Header(Machine(Z80LogAri),ADD_A_r)==(ADD_A_r(rr));
  List_Header(Machine(Z80LogAri),ADD_A_n)==(ADD_A_n(nn));
  List_Header(Machine(Z80LogAri),ADD_A_9HL0)==(ADD_A_9HL0);
  List_Header(Machine(Z80LogAri),ADD_A_9IX_d0)==(ADD_A_9IX_d0);
  List_Header(Machine(Z80LogAri),ADD_A_9IY_d0)==(ADD_A_9IY_d0);
  List_Header(Machine(Z80LogAri),ADC_A_r)==(ADC_A_r(rr));
  List_Header(Machine(Z80LogAri),ADC_A_n)==(ADC_A_n(nn));
  List_Header(Machine(Z80LogAri),ADC_A_9HL0)==(ADC_A_9HL0);
  List_Header(Machine(Z80LogAri),ADC_A_9IX_d0)==(ADC_A_9IX_d0);
  List_Header(Machine(Z80LogAri),ADC_A_9IY_d0)==(ADC_A_9IY_d0);
  List_Header(Machine(Z80LogAri),SUB_A_r)==(SUB_A_r(rr));
  List_Header(Machine(Z80LogAri),SUB_A_n)==(SUB_A_n(nn));
  List_Header(Machine(Z80LogAri),SUB_A_9HL0)==(SUB_A_9HL0);
  List_Header(Machine(Z80LogAri),SUB_A_9IX_d0)==(SUB_A_9IX_d0);
  List_Header(Machine(Z80LogAri),SUB_A_9IY_d0)==(SUB_A_9IY_d0);
  List_Header(Machine(Z80LogAri),SBC_A_r)==(SBC_A_r(rr));
  List_Header(Machine(Z80LogAri),SBC_A_n)==(SBC_A_n(nn));
  List_Header(Machine(Z80LogAri),SBC_A_9HL0)==(SBC_A_9HL0);
  List_Header(Machine(Z80LogAri),SBC_A_9IX_d0)==(SBC_A_9IX_d0);
  List_Header(Machine(Z80LogAri),SBC_A_9IY_d0)==(SBC_A_9IY_d0);
  List_Header(Machine(Z80LogAri),AND_A_r)==(AND_A_r(rr));
  List_Header(Machine(Z80LogAri),AND_A_n)==(AND_A_n(nn));
  List_Header(Machine(Z80LogAri),AND_A_9HL0)==(AND_A_9HL0);
  List_Header(Machine(Z80LogAri),AND_A_9IX_d0)==(AND_A_9IX_d0);
  List_Header(Machine(Z80LogAri),AND_A_9IY_d0)==(AND_A_9IY_d0);
  List_Header(Machine(Z80LogAri),OR_A_r)==(OR_A_r(rr));
  List_Header(Machine(Z80LogAri),OR_A_n)==(OR_A_n(nn));
  List_Header(Machine(Z80LogAri),OR_A_9HL0)==(OR_A_9HL0);
  List_Header(Machine(Z80LogAri),OR_A_9IX_d0)==(OR_A_9IX_d0);
  List_Header(Machine(Z80LogAri),OR_A_9IY_d0)==(OR_A_9IY_d0);
  List_Header(Machine(Z80LogAri),XOR_A_r)==(XOR_A_r(rr));
  List_Header(Machine(Z80LogAri),XOR_A_n)==(XOR_A_n(nn));
  List_Header(Machine(Z80LogAri),XOR_A_9HL0)==(XOR_A_9HL0);
  List_Header(Machine(Z80LogAri),XOR_A_9IX_d0)==(XOR_A_9IX_d0);
  List_Header(Machine(Z80LogAri),XOR_A_9IY_d0)==(XOR_A_9IY_d0);
  List_Header(Machine(Z80LogAri),CP_A_r)==(CP_A_r(rr));
  List_Header(Machine(Z80LogAri),CP_A_n)==(CP_A_n(nn));
  List_Header(Machine(Z80LogAri),CP_A_9HL0)==(CP_A_9HL0);
  List_Header(Machine(Z80LogAri),CP_A_9IX_d0)==(CP_A_9IX_d0);
  List_Header(Machine(Z80LogAri),CP_A_9IY_d0)==(CP_A_9IY_d0);
  List_Header(Machine(Z80LogAri),INC_r)==(INC_r(rr));
  List_Header(Machine(Z80LogAri),INC_9HL0)==(INC_9HL0);
  List_Header(Machine(Z80LogAri),INC_9IX_d0)==(INC_9IX_d0);
  List_Header(Machine(Z80LogAri),INC_9IY_d0)==(INC_9IY_d0);
  List_Header(Machine(Z80LogAri),DEC_r)==(DEC_r(rr));
  List_Header(Machine(Z80LogAri),DEC_9HL0)==(DEC_9HL0);
  List_Header(Machine(Z80LogAri),DEC_9IX_d0)==(DEC_9IX_d0);
  List_Header(Machine(Z80LogAri),DEC_9IY_d0)==(DEC_9IY_d0)
END
&
THEORY ListOperationGuardX END
&
THEORY ListPreconditionX IS
  List_Precondition(Machine(Z80LogAri),ADD_A_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80LogAri),ADD_A_n)==(nn: SCHAR);
  List_Precondition(Machine(Z80LogAri),ADD_A_9HL0)==(btrue);
  List_Precondition(Machine(Z80LogAri),ADD_A_9IX_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),ADD_A_9IY_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),ADC_A_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80LogAri),ADC_A_n)==(nn: SCHAR);
  List_Precondition(Machine(Z80LogAri),ADC_A_9HL0)==(btrue);
  List_Precondition(Machine(Z80LogAri),ADC_A_9IX_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),ADC_A_9IY_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),SUB_A_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80LogAri),SUB_A_n)==(nn: SCHAR);
  List_Precondition(Machine(Z80LogAri),SUB_A_9HL0)==(btrue);
  List_Precondition(Machine(Z80LogAri),SUB_A_9IX_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),SUB_A_9IY_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),SBC_A_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80LogAri),SBC_A_n)==(nn: SCHAR);
  List_Precondition(Machine(Z80LogAri),SBC_A_9HL0)==(btrue);
  List_Precondition(Machine(Z80LogAri),SBC_A_9IX_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),SBC_A_9IY_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),AND_A_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80LogAri),AND_A_n)==(nn: SCHAR);
  List_Precondition(Machine(Z80LogAri),AND_A_9HL0)==(btrue);
  List_Precondition(Machine(Z80LogAri),AND_A_9IX_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),AND_A_9IY_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),OR_A_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80LogAri),OR_A_n)==(nn: SCHAR);
  List_Precondition(Machine(Z80LogAri),OR_A_9HL0)==(btrue);
  List_Precondition(Machine(Z80LogAri),OR_A_9IX_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),OR_A_9IY_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),XOR_A_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80LogAri),XOR_A_n)==(nn: SCHAR);
  List_Precondition(Machine(Z80LogAri),XOR_A_9HL0)==(btrue);
  List_Precondition(Machine(Z80LogAri),XOR_A_9IX_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),XOR_A_9IY_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),CP_A_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80LogAri),CP_A_n)==(nn: SCHAR);
  List_Precondition(Machine(Z80LogAri),CP_A_9HL0)==(btrue);
  List_Precondition(Machine(Z80LogAri),CP_A_9IX_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),CP_A_9IY_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),INC_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80LogAri),INC_9HL0)==(btrue);
  List_Precondition(Machine(Z80LogAri),INC_9IX_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),INC_9IY_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),DEC_r)==(rr: id_reg_8);
  List_Precondition(Machine(Z80LogAri),DEC_9HL0)==(btrue);
  List_Precondition(Machine(Z80LogAri),DEC_9IX_d0)==(btrue);
  List_Precondition(Machine(Z80LogAri),DEC_9IY_d0)==(btrue)
END
&
THEORY ListSubstitutionX IS
  Expanded_List_Substitution(Machine(Z80LogAri),DEC_9IY_d0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & add8SCHAR(mem(256-(iy+rgs8(d0))),1) = sum,is_negative,carry,digit_carry,zero ==> (mem(256-(iy+rgs8(d0))): USHORTINT & sum: SCHAR | rgs8:=rgs8<+{get_new_flag_register(rgs8,carry,TRUE,bool(mem(256-(iy+rgs8(d0))) = -128),digit_carry,zero,is_negative)} || mem:=mem<+{mem(256-(iy+rgs8(d0)))|->sum} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80LogAri),DEC_9IX_d0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & add8SCHAR(mem(256-(ix+rgs8(d0))),1) = sum,is_negative,carry,digit_carry,zero ==> (mem(256-(ix+rgs8(d0))): USHORTINT & sum: SCHAR | rgs8:=rgs8<+{get_new_flag_register(rgs8,carry,TRUE,bool(mem(256-(ix+rgs8(d0))) = -128),digit_carry,zero,is_negative)} || mem:=mem<+{mem(256-(ix+rgs8(d0)))|->sum} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80LogAri),DEC_9HL0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & add8SCHAR(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))),1) = sum,is_negative,carry,digit_carry,zero ==> (mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))): USHORTINT & sum: SCHAR | rgs8:=rgs8<+{get_new_flag_register(rgs8,carry,TRUE,bool(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))) = -128),digit_carry,zero,is_negative)} || mem:=mem<+{mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))|->sum} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80LogAri),DEC_r)==(rr: id_reg_8 | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & substract8SCHAR(rgs8(rr),1) = sum,is_negative,carry,digit_carry,zero ==> rgs8,pc:=rgs8<+{rr|->sum,get_new_flag_register(rgs8,carry,TRUE,bool(rgs8(rr) = -128),digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),INC_9IY_d0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & add8SCHAR(mem(256-(iy+rgs8(d0))),1) = sum,is_negative,carry,digit_carry,zero ==> (mem(256-(iy+rgs8(d0))): USHORTINT & sum: SCHAR | rgs8:=rgs8<+{get_new_flag_register(rgs8,carry,FALSE,bool(mem(256-(iy+rgs8(d0))) = 127),digit_carry,zero,is_negative)} || mem:=mem<+{mem(256-(iy+rgs8(d0)))|->sum} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80LogAri),INC_9IX_d0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & add8SCHAR(mem(256-(ix+rgs8(d0))),1) = sum,is_negative,carry,digit_carry,zero ==> (mem(256-(ix+rgs8(d0))): USHORTINT & sum: SCHAR | rgs8:=rgs8<+{get_new_flag_register(rgs8,carry,FALSE,bool(mem(256-(ix+rgs8(d0))) = 127),digit_carry,zero,is_negative)} || mem:=mem<+{mem(256-(ix+rgs8(d0)))|->sum} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80LogAri),INC_9HL0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & add8SCHAR(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))),1) = sum,is_negative,carry,digit_carry,zero ==> (mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))): USHORTINT & sum: SCHAR | rgs8:=rgs8<+{get_new_flag_register(rgs8,carry,FALSE,bool(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))) = 127),digit_carry,zero,is_negative)} || mem:=mem<+{mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))|->sum} || pc:=INSTRUCTION_NEXT(pc))));
  Expanded_List_Substitution(Machine(Z80LogAri),INC_r)==(rr: id_reg_8 | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & add8SCHAR(rgs8(rr),1) = sum,is_negative,carry,digit_carry,zero ==> rgs8,pc:=rgs8<+{rr|->sum,get_new_flag_register(rgs8,carry,FALSE,bool(rgs8(rr) = 127),digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),CP_A_9IY_d0)==(btrue | @any(result,carry,n_add_sub,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & n_add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,is_negative,carry,halfcarry,zero = substract8SCHAR(rgs8(a0),mem(256-(iy+rgs8(d0)))) & n_add_sub = TRUE & pv = carry ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,n_add_sub,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),CP_A_9IX_d0)==(btrue | @any(result,carry,n_add_sub,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & n_add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,is_negative,carry,halfcarry,zero = substract8SCHAR(rgs8(a0),mem(256-(ix+rgs8(d0)))) & n_add_sub = TRUE & pv = carry ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,n_add_sub,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),CP_A_9HL0)==(btrue | @any(result,carry,n_add_sub,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & n_add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,is_negative,carry,halfcarry,zero = substract8SCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) & n_add_sub = TRUE & pv = carry ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,n_add_sub,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),CP_A_n)==(nn: SCHAR | @any(result,carry,n_add_sub,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & n_add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,is_negative,carry,halfcarry,zero = substract8SCHAR(rgs8(a0),nn) & n_add_sub = TRUE & pv = carry ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,n_add_sub,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),CP_A_r)==(rr: id_reg_8 | @any(result,carry,n_add_sub,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & n_add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,is_negative,carry,halfcarry,zero = substract8SCHAR(rgs8(a0),rgs8(rr)) & n_add_sub = TRUE & pv = carry ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,n_add_sub,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),XOR_A_9IY_d0)==(btrue | @any(result,carry,nn,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = xorSCHAR(rgs8(a0),mem(256-(iy+rgs8(d0)))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),XOR_A_9IX_d0)==(btrue | @any(result,carry,nn,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = xorSCHAR(rgs8(a0),mem(256-(ix+rgs8(d0)))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),XOR_A_9HL0)==(btrue | @any(result,carry,nn,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = xorSCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),XOR_A_n)==(nn: SCHAR | @any(result,carry,add_sub,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = xorSCHAR(rgs8(a0),nn) & carry = FALSE & add_sub = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,add_sub,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),XOR_A_r)==(rr: id_reg_8 | @any(result,carry,add_sub,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = xorSCHAR(rgs8(a0),rgs8(rr)) & carry = FALSE & add_sub = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,add_sub,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),OR_A_9IY_d0)==(btrue | @any(result,carry,nn,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = iorSCHAR(rgs8(a0),mem(256-(iy+rgs8(d0)))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),OR_A_9IX_d0)==(btrue | @any(result,carry,nn,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = iorSCHAR(rgs8(a0),mem(256-(ix+rgs8(d0)))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),OR_A_9HL0)==(btrue | @any(result,carry,nn,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = iorSCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),OR_A_n)==(nn: SCHAR | @any(result,carry,add_sub,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = iorSCHAR(rgs8(a0),nn) & carry = FALSE & add_sub = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,add_sub,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),OR_A_r)==(rr: id_reg_8 | @any(result,carry,add_sub,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = iorSCHAR(rgs8(a0),rgs8(rr)) & carry = FALSE & add_sub = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,add_sub,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),AND_A_9IY_d0)==(btrue | @any(result,carry,nn,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = andSCHAR(rgs8(a0),mem(256-(iy+rgs8(d0)))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),AND_A_9IX_d0)==(btrue | @any(result,carry,nn,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = andSCHAR(rgs8(a0),mem(256-(ix+rgs8(d0)))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),AND_A_9HL0)==(btrue | @any(result,carry,nn,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = andSCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),AND_A_n)==(nn: SCHAR | @any(result,carry,add_sub,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = andSCHAR(rgs8(a0),nn) & carry = FALSE & add_sub = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,add_sub,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),AND_A_r)==(rr: id_reg_8 | @any(result,carry,add_sub,pv,halfcarry,zero,is_negative).(result: SCHAR & carry: BOOL & add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = andSCHAR(rgs8(a0),rgs8(rr)) & carry = FALSE & add_sub = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) ==> rgs8,pc:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,add_sub,pv,halfcarry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),SBC_A_9IY_d0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(256-(iy+rgs8(d0)))-bitgetSCHAR(rgs8(f0),0)) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),SBC_A_9IX_d0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(256-(ix+rgs8(d0)))-bitgetSCHAR(rgs8(f0),0)) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),SBC_A_9HL0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))-bitgetSCHAR(rgs8(f0),0)) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),SBC_A_n)==(nn: SCHAR | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),nn-bitgetSCHAR(rgs8(f0),0)) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),SBC_A_r)==(rr: id_reg_8 | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = substract8SCHAR(rgs8(a0),rgs8(rr)-bitgetSCHAR(rgs8(f0),0)) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),SUB_A_9IY_d0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = substract8SCHAR(rgs8(a0),mem(256-(iy+rgs8(d0)))) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),SUB_A_9IX_d0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = substract8SCHAR(rgs8(a0),mem(256-(ix+rgs8(d0)))) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),SUB_A_9HL0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = substract8SCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),SUB_A_n)==(nn: SCHAR | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = substract8SCHAR(rgs8(a0),nn) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),SUB_A_r)==(rr: id_reg_8 | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = substract8SCHAR(rgs8(a0),rgs8(rr)) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),ADC_A_9IY_d0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(256-(iy+rgs8(d0)))+bitgetSCHAR(rgs8(f0),0)) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),ADC_A_9IX_d0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(256-(ix+rgs8(d0)))+bitgetSCHAR(rgs8(f0),0)) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),ADC_A_9HL0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+bitgetSCHAR(rgs8(f0),0)) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),ADC_A_n)==(nn: SCHAR | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),nn+bitgetSCHAR(rgs8(f0),0)) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),ADC_A_r)==(rr: id_reg_8 | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),rgs8(rr)+bitgetSCHAR(rgs8(f0),0)) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),ADD_A_9IY_d0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(256-(iy+rgs8(d0)))) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),ADD_A_9IX_d0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(256-(ix+rgs8(d0)))) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),ADD_A_9HL0)==(btrue | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),ADD_A_n)==(nn: SCHAR | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),nn) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  Expanded_List_Substitution(Machine(Z80LogAri),ADD_A_r)==(rr: id_reg_8 | @any(sum,is_negative,carry,digit_carry,zero).(sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),rgs8(rr)) ==> rgs8,pc:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)},INSTRUCTION_NEXT(pc)));
  List_Substitution(Machine(Z80LogAri),ADD_A_r)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),rgs8(rr)) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),ADD_A_n)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),nn) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),ADD_A_9HL0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),ADD_A_9IX_d0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(256-(ix+rgs8(d0)))) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),ADD_A_9IY_d0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(256-(iy+rgs8(d0)))) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),ADC_A_r)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),rgs8(rr)+bitgetSCHAR(rgs8(f0),0)) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),ADC_A_n)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),nn+bitgetSCHAR(rgs8(f0),0)) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),ADC_A_9HL0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))+bitgetSCHAR(rgs8(f0),0)) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),ADC_A_9IX_d0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(256-(ix+rgs8(d0)))+bitgetSCHAR(rgs8(f0),0)) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),ADC_A_9IY_d0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(256-(iy+rgs8(d0)))+bitgetSCHAR(rgs8(f0),0)) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,FALSE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),SUB_A_r)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = substract8SCHAR(rgs8(a0),rgs8(rr)) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),SUB_A_n)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = substract8SCHAR(rgs8(a0),nn) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),SUB_A_9HL0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = substract8SCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),SUB_A_9IX_d0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = substract8SCHAR(rgs8(a0),mem(256-(ix+rgs8(d0)))) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),SUB_A_9IY_d0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = substract8SCHAR(rgs8(a0),mem(256-(iy+rgs8(d0)))) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),SBC_A_r)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = substract8SCHAR(rgs8(a0),rgs8(rr)-bitgetSCHAR(rgs8(f0),0)) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),SBC_A_n)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),nn-bitgetSCHAR(rgs8(f0),0)) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),SBC_A_9HL0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))-bitgetSCHAR(rgs8(f0),0)) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),SBC_A_9IX_d0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(256-(ix+rgs8(d0)))-bitgetSCHAR(rgs8(f0),0)) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),SBC_A_9IY_d0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & sum,is_negative,carry,digit_carry,zero = add8SCHAR(rgs8(a0),mem(256-(iy+rgs8(d0)))-bitgetSCHAR(rgs8(f0),0)) THEN rgs8:=rgs8<+{a0|->sum,get_new_flag_register(rgs8,carry,TRUE,carry,digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),AND_A_r)==(ANY result,carry,add_sub,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = andSCHAR(rgs8(a0),rgs8(rr)) & carry = FALSE & add_sub = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,add_sub,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),AND_A_n)==(ANY result,carry,add_sub,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = andSCHAR(rgs8(a0),nn) & carry = FALSE & add_sub = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,add_sub,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),AND_A_9HL0)==(ANY result,carry,nn,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = andSCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),AND_A_9IX_d0)==(ANY result,carry,nn,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = andSCHAR(rgs8(a0),mem(256-(ix+rgs8(d0)))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),AND_A_9IY_d0)==(ANY result,carry,nn,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = andSCHAR(rgs8(a0),mem(256-(iy+rgs8(d0)))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),OR_A_r)==(ANY result,carry,add_sub,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = iorSCHAR(rgs8(a0),rgs8(rr)) & carry = FALSE & add_sub = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,add_sub,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),OR_A_n)==(ANY result,carry,add_sub,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = iorSCHAR(rgs8(a0),nn) & carry = FALSE & add_sub = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,add_sub,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),OR_A_9HL0)==(ANY result,carry,nn,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = iorSCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),OR_A_9IX_d0)==(ANY result,carry,nn,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = iorSCHAR(rgs8(a0),mem(256-(ix+rgs8(d0)))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),OR_A_9IY_d0)==(ANY result,carry,nn,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = iorSCHAR(rgs8(a0),mem(256-(iy+rgs8(d0)))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),XOR_A_r)==(ANY result,carry,add_sub,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = xorSCHAR(rgs8(a0),rgs8(rr)) & carry = FALSE & add_sub = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,add_sub,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),XOR_A_n)==(ANY result,carry,add_sub,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = xorSCHAR(rgs8(a0),nn) & carry = FALSE & add_sub = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,add_sub,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),XOR_A_9HL0)==(ANY result,carry,nn,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = xorSCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),XOR_A_9IX_d0)==(ANY result,carry,nn,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = xorSCHAR(rgs8(a0),mem(256-(ix+rgs8(d0)))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),XOR_A_9IY_d0)==(ANY result,carry,nn,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & nn: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,zero = xorSCHAR(rgs8(a0),mem(256-(iy+rgs8(d0)))) & carry = FALSE & nn = FALSE & pv = FALSE & halfcarry = FALSE & is_negative = bool(result<0) THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,nn,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),CP_A_r)==(ANY result,carry,n_add_sub,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & n_add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,is_negative,carry,halfcarry,zero = substract8SCHAR(rgs8(a0),rgs8(rr)) & n_add_sub = TRUE & pv = carry THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,n_add_sub,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),CP_A_n)==(ANY result,carry,n_add_sub,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & n_add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,is_negative,carry,halfcarry,zero = substract8SCHAR(rgs8(a0),nn) & n_add_sub = TRUE & pv = carry THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,n_add_sub,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),CP_A_9HL0)==(ANY result,carry,n_add_sub,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & n_add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,is_negative,carry,halfcarry,zero = substract8SCHAR(rgs8(a0),mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))))) & n_add_sub = TRUE & pv = carry THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,n_add_sub,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),CP_A_9IX_d0)==(ANY result,carry,n_add_sub,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & n_add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,is_negative,carry,halfcarry,zero = substract8SCHAR(rgs8(a0),mem(256-(ix+rgs8(d0)))) & n_add_sub = TRUE & pv = carry THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,n_add_sub,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),CP_A_9IY_d0)==(ANY result,carry,n_add_sub,pv,halfcarry,zero,is_negative WHERE result: SCHAR & carry: BOOL & n_add_sub: BOOL & pv: BOOL & halfcarry: BOOL & zero: BOOL & is_negative: BOOL & result,is_negative,carry,halfcarry,zero = substract8SCHAR(rgs8(a0),mem(256-(iy+rgs8(d0)))) & n_add_sub = TRUE & pv = carry THEN rgs8:=rgs8<+{a0|->result,get_new_flag_register(rgs8,carry,n_add_sub,pv,halfcarry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),INC_r)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & add8SCHAR(rgs8(rr),1) = sum,is_negative,carry,digit_carry,zero THEN rgs8:=rgs8<+{rr|->sum,get_new_flag_register(rgs8,carry,FALSE,bool(rgs8(rr) = 127),digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),INC_9HL0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & add8SCHAR(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))),1) = sum,is_negative,carry,digit_carry,zero THEN rgs8:=rgs8<+{get_new_flag_register(rgs8,carry,FALSE,bool(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))) = 127),digit_carry,zero,is_negative)} || updateAddressMem(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))),sum) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),INC_9IX_d0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & add8SCHAR(mem(256-(ix+rgs8(d0))),1) = sum,is_negative,carry,digit_carry,zero THEN rgs8:=rgs8<+{get_new_flag_register(rgs8,carry,FALSE,bool(mem(256-(ix+rgs8(d0))) = 127),digit_carry,zero,is_negative)} || updateAddressMem(mem(256-(ix+rgs8(d0))),sum) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),INC_9IY_d0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & add8SCHAR(mem(256-(iy+rgs8(d0))),1) = sum,is_negative,carry,digit_carry,zero THEN rgs8:=rgs8<+{get_new_flag_register(rgs8,carry,FALSE,bool(mem(256-(iy+rgs8(d0))) = 127),digit_carry,zero,is_negative)} || updateAddressMem(mem(256-(iy+rgs8(d0))),sum) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),DEC_r)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & substract8SCHAR(rgs8(rr),1) = sum,is_negative,carry,digit_carry,zero THEN rgs8:=rgs8<+{rr|->sum,get_new_flag_register(rgs8,carry,TRUE,bool(rgs8(rr) = -128),digit_carry,zero,is_negative)} || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),DEC_9HL0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & add8SCHAR(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))),1) = sum,is_negative,carry,digit_carry,zero THEN rgs8:=rgs8<+{get_new_flag_register(rgs8,carry,TRUE,bool(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))) = -128),digit_carry,zero,is_negative)} || updateAddressMem(mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0)))),sum) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),DEC_9IX_d0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & add8SCHAR(mem(256-(ix+rgs8(d0))),1) = sum,is_negative,carry,digit_carry,zero THEN rgs8:=rgs8<+{get_new_flag_register(rgs8,carry,TRUE,bool(mem(256-(ix+rgs8(d0))) = -128),digit_carry,zero,is_negative)} || updateAddressMem(mem(256-(ix+rgs8(d0))),sum) || pc:=INSTRUCTION_NEXT(pc) END);
  List_Substitution(Machine(Z80LogAri),DEC_9IY_d0)==(ANY sum,is_negative,carry,digit_carry,zero WHERE sum: SCHAR & is_negative: BOOL & carry: BOOL & digit_carry: BOOL & zero: BOOL & add8SCHAR(mem(256-(iy+rgs8(d0))),1) = sum,is_negative,carry,digit_carry,zero THEN rgs8:=rgs8<+{get_new_flag_register(rgs8,carry,TRUE,bool(mem(256-(iy+rgs8(d0))) = -128),digit_carry,zero,is_negative)} || updateAddressMem(mem(256-(iy+rgs8(d0))),sum) || pc:=INSTRUCTION_NEXT(pc) END)
END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(Z80LogAri))==(REG16_TO_REG8,REG8_TO_REG16,update_flag_register,get_new_flag_register);
  Inherited_List_Constants(Machine(Z80LogAri))==(?);
  List_Constants(Machine(Z80LogAri))==(REG16_TO_REG8,REG8_TO_REG16,update_flag_register,get_new_flag_register)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(Z80LogAri),id_reg_8)==({a0,f0,f_0,a_0,b0,c0,b_0,c_0,d0,e0,d_0,e_0,h0,l0,h_0,l_0,i0,r0});
  Context_List_Enumerated(Machine(Z80LogAri))==(?);
  Context_List_Defered(Machine(Z80LogAri))==(?);
  Context_List_Sets(Machine(Z80LogAri))==(?);
  List_Valuable_Sets(Machine(Z80LogAri))==(?);
  Inherited_List_Enumerated(Machine(Z80LogAri))==(?);
  Inherited_List_Defered(Machine(Z80LogAri))==(?);
  Inherited_List_Sets(Machine(Z80LogAri))==(?);
  List_Enumerated(Machine(Z80LogAri))==(id_reg_8,id_reg_16);
  List_Defered(Machine(Z80LogAri))==(?);
  List_Sets(Machine(Z80LogAri))==(id_reg_8,id_reg_16);
  Set_Definition(Machine(Z80LogAri),id_reg_16)==({BC,DE,HL,SP,AF})
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(Z80LogAri))==(?);
  Expanded_List_HiddenConstants(Machine(Z80LogAri))==(?);
  List_HiddenConstants(Machine(Z80LogAri))==(?);
  External_List_HiddenConstants(Machine(Z80LogAri))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(Z80LogAri))==(btrue);
  Context_List_Properties(Machine(Z80LogAri))==(SCHAR_LENGTH: NATURAL & SCHAR_LENGTH = 8 & NB_SCHARS: NATURAL & NB_SCHARS = 2**SCHAR_LENGTH & SCHAR = -128..127 & SCHAR_POSITION = 0..SCHAR_LENGTH-1 & UCHAR = 0..255 & SSHORTINT_LENGTH: NATURAL & SSHORTINT_LENGTH = 16 & NB_SSHORTINTS: NATURAL & NB_SSHORTINTS = 2**SSHORTINT_LENGTH & SSHORTINT = -32768..32767 & SSHORTINT_POSITION = 0..SSHORTINT_LENGTH-1 & BV16 <: BIT_VECTOR & BV16 = {vv | vv: BIT_VECTOR & size(vv) = SSHORTINT_LENGTH} & BYTE_TO_UCHAR: BYTE --> UCHAR & BYTE_TO_UCHAR = %v0.(v0: BYTE | (-128)*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & UCHAR_TO_BYTE: UCHAR --> BYTE & UCHAR_TO_BYTE = BYTE_TO_UCHAR~ & USHORTINT = 0..65535 & NB_INSTRUCTIONS: NATURAL & INST_SZ: NATURAL & INST_SZ = 16 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = USHORTINT & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & INSTRUCTION_NEXT = {p0,q0 | p0: INSTRUCTION & q0: INSTRUCTION & 0<=p0 & p0<NB_INSTRUCTIONS-1 & q0 = p0+1}\/{NB_INSTRUCTIONS-1|->0} & INSTRUCTION_JUMP = %(p0,e0).(p0: INSTRUCTION & e0: -126..129 | p0+e0) & BYTE_TO_SCHAR: BYTE --> SCHAR & BYTE_TO_SCHAR = %v0.(v0: BYTE | (-128)*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SCHAR_TO_BYTE: SCHAR --> BYTE & SCHAR_TO_BYTE = BYTE_TO_SCHAR~ & BV16_TO_SSHORTINT: BV16 --> SSHORTINT & BV16_TO_SSHORTINT = %v0.(v0: BV16 | (-32768)*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SSHORTINT_TO_BV16: SSHORTINT --> BV16 & SSHORTINT_TO_BV16 = BV16_TO_SSHORTINT~ & BYTE_TO_BV16: BYTE*BYTE --> BV16 & BYTE_TO_BV16 = %(v1,v2).(v1: BV16 & v2: BV16 | {0|->v2(0),1|->v2(1),2|->v2(2),3|->v2(3),4|->v2(4),5|->v2(5),6|->v2(6),7|->v2(7),8|->v1(0),9|->v1(1),10|->v1(2),11|->v1(3),12|->v1(4),13|->v1(5),14|->v1(6),15|->v1(7)}) & BV16_TO_BYTE: BV16 --> BYTE*BYTE & BV16_TO_BYTE = BYTE_TO_BV16~ & SCHAR_TO_SSHORTINT: SCHAR*SCHAR --> SSHORTINT & SCHAR_TO_SSHORTINT = %(w1,w2).(w1: SCHAR & w2: SCHAR | BV16_TO_SSHORTINT(BYTE_TO_BV16(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2)))) & SSHORTINT_TO_SCHAR: SSHORTINT --> SCHAR*SCHAR & SSHORTINT_TO_SCHAR = SCHAR_TO_SSHORTINT~ & BV16_TO_USHORTINT: BV16 --> USHORTINT & BV16_TO_USHORTINT = %v0.(v0: BV16 | (-32768)*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & USHORTINT_TO_BV16: USHORTINT --> BV16 & USHORTINT_TO_BV16 = BV16_TO_USHORTINT~ & parity_bit_from_BYTE: BIT_VECTOR --> BIT & parity_bit_from_BYTE = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | bv(idx)) mod 2) & SSHORTINT_TO_USHORTINT: SSHORTINT --> USHORTINT & SSHORTINT_TO_USHORTINT = %v0.(v0: SSHORTINT | v0-32768) & USHORTINT_TO_SSHORTINT: USHORTINT --> SSHORTINT & USHORTINT_TO_SSHORTINT = SSHORTINT_TO_USHORTINT~ & BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = {bt | bt: NATURAL +-> BIT}-{{}} & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & BV_INDX: BIT_VECTOR --> POW(NATURAL) & BV_INDX = %bv.(bv: BIT_VECTOR | 0..bv_size(bv)-1) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high | low..high<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (0..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (0..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: BV_INDX(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1(idx)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: BV_INDX(v1) & bit: BIT | v1<+{idx|->bit}) & BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE <: BIT_VECTOR & BYTE = {vv | vv: BIT_VECTOR & bv_size(vv) = BYTE_WIDTH} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0} & is_zeroSCHAR: SCHAR --> BIT & is_zeroSCHAR = %w1.(w1: SCHAR | bool_to_bit(bool(w1 = 0))) & is_negative: SCHAR --> BIT & is_negative = %w1.(w1: SCHAR | bool_to_bit(bool(w1<0))) & parity_bit_from_BYTE: BIT_VECTOR --> BIT & parity_bit_from_BYTE = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | bv(idx)) mod 2) & halfSCHAR: SCHAR --> SCHAR & halfSCHAR = %ww.(ww: SCHAR | ww mod 16) & add8SCHAR: SCHAR*SCHAR --> SCHAR*BOOL*BOOL*BOOL*BOOL & add8SCHAR = %(w1,w2).(w1: UCHAR & w2: UCHAR & w1+w2< -128 | 256-(w1+w2),bool(w1+w2<0),TRUE,bool(halfSCHAR(w1)+halfSCHAR(w2)>=16),FALSE)\/%(w1,w2).(w1: UCHAR & w2: UCHAR & not(w1+w2< -128) | (w1+w2) mod 128,bool(w1+w2<0),bool(w1+w2>127),bool(halfSCHAR(w1)+halfSCHAR(w2)>=16),bool(w1 = 0)) & substract8SCHAR: SCHAR*SCHAR --> SCHAR*BOOL*BOOL*BOOL*BOOL & substract8SCHAR = %(w1,w2).(w1: UCHAR & w2: UCHAR & w1-w2< -128 | 256-(w1-w2),bool(w1-w2<0),TRUE,bool(halfSCHAR(w1)-halfSCHAR(w2)>=16),FALSE)\/%(w1,w2).(w1: UCHAR & w2: UCHAR & not(w1-w2< -128) | (w1-w2) mod 128,bool(w1-w2<0),bool(w1-w2>127),bool(halfSCHAR(w1)-halfSCHAR(w2)>=16),bool(w1 = 0)) & add16SCHAR: SSHORTINT*SSHORTINT --> SSHORTINT*BOOL*BOOL & !(w1,w2,sum).(w1: SSHORTINT & w2: SSHORTINT & sum: NATURAL & sum = w1+w2 => (sum<=65535 => add16SCHAR(w1,w2) = sum,bool(sum = 0),FALSE) & (65536<=sum => add16SCHAR(w1,w2) = sum-65536,bool(sum = 65536),TRUE)) & andSCHAR: SCHAR*SCHAR --> SCHAR*BOOL & !(w1,w2,w0).(w1: SCHAR & w2: SCHAR & w0: SCHAR & w0 = BYTE_TO_SCHAR(bv_and(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2))) => andSCHAR(w1,w2) = w0,bool(w0 = 0)) & iorSCHAR: SCHAR*SCHAR --> SCHAR*BOOL & !(w1,w2,w0).(w1: SCHAR & w2: SCHAR & w0: SCHAR & w0 = BYTE_TO_SCHAR(bv_or(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2))) => iorSCHAR(w1,w2) = w0,bool(w0 = 0)) & xorSCHAR: SCHAR*SCHAR --> SCHAR*BOOL & !(w1,w2,w0).(w1: SCHAR & w2: SCHAR & w0: SCHAR & w0 = BYTE_TO_SCHAR(bv_xor(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2))) => xorSCHAR(w1,w2) = w0,bool(w0 = 0)) & bitgetSCHAR: SCHAR*SCHAR_POSITION --> BIT & bitgetSCHAR = %(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & i0 = 7 | 1-(w0+2**7)/128 mod 2)\/%(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & i0/=7 | (w0+2**7)/2**i0 mod 2) & bitsetSCHAR: SCHAR*SCHAR_POSITION --> SCHAR & bitsetSCHAR = %(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & bitgetSCHAR(w0,i0) = 1 | w0)\/%(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & bitgetSCHAR(w0,i0)/=1 | w0+2**i0) & bitclearSCHAR: SCHAR*SCHAR_POSITION --> SCHAR & bitclearSCHAR = %(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & bitgetSCHAR(w0,i0) = 0 | w0)\/%(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & bitgetSCHAR(w0,i0)/=0 & i0 = 7 | w0+128)\/%(w0,i0).(w0: SCHAR & i0: SCHAR_POSITION & bitgetSCHAR(w0,i0)/=0 & i0/=7 | w0-2**i0) & complementSCHAR: SCHAR --> SCHAR & complementSCHAR = %w0.(w0: SCHAR | 255-w0) & swapSCHAR: SCHAR --> SCHAR & !(w0,v0).(w0: SCHAR & v0: BIT_VECTOR => (v0 = SCHAR_TO_BYTE(w0) => swapSCHAR(w0) = BYTE_TO_SCHAR({0|->v0(4),1|->v0(5),2|->v0(6),3|->v0(7),4|->v0(0),5|->v0(1),6|->v0(2),7|->v0(3)}))) & rotateleftSCHAR: SCHAR --> SCHAR*BOOL & !(w0,v0).(w0: SCHAR & v0: BIT_VECTOR => (v0 = SCHAR_TO_BYTE(w0) => rotateleftSCHAR(w0) = BYTE_TO_SCHAR({0|->v0(7),1|->v0(0),2|->v0(1),3|->v0(2),4|->v0(3),5|->v0(4),6|->v0(5),7|->v0(6)}),bool(v0(7) = 1))) & rotaterightSCHAR: SCHAR --> SCHAR*BOOL & !(w0,v0).(w0: SCHAR & v0: BIT_VECTOR => (v0 = SCHAR_TO_BYTE(w0) => rotaterightSCHAR(w0) = BYTE_TO_SCHAR({0|->v0(1),1|->v0(2),2|->v0(3),3|->v0(4),4|->v0(5),5|->v0(6),6|->v0(7),7|->v0(0)}),bool(v0(0) = 1))));
  Inherited_List_Properties(Machine(Z80LogAri))==(btrue);
  List_Properties(Machine(Z80LogAri))==(REG16_TO_REG8: id_reg_16 --> id_reg_8*id_reg_8 & !(r1,r2,r3).(r1: id_reg_16 & r2: id_reg_8 & r3: id_reg_8 => (r1 = BC => REG16_TO_REG8(r1) = b0,c0) & (r1 = DE => REG16_TO_REG8(r1) = d0,e0) & (r1 = HL => REG16_TO_REG8(r1) = h0,l0) & (r1 = AF => REG16_TO_REG8(r1) = a0,f0)) & REG8_TO_REG16 = REG16_TO_REG8~ & update_flag_register = %(rgs8_,c0,n_add_sub,pv2,h4,z6,s7).(rgs8_: id_reg_8 --> SCHAR & c0: BOOL & n_add_sub: BOOL & pv2: BOOL & h4: BOOL & z6: BOOL & s7: BOOL | rgs8_<+{f0|->BYTE_TO_SCHAR(SCHAR_TO_BYTE(rgs8_(f0))<+{7|->bool_to_bit(s7),6|->bool_to_bit(z6),4|->bool_to_bit(h4),2|->bool_to_bit(pv2),1|->bool_to_bit(n_add_sub),0|->bool_to_bit(c0)})}) & get_new_flag_register = %(rgs8_,c0,n_add_sub,pv2,h4,z6,s7).(rgs8_: id_reg_8 --> SCHAR & c0: BOOL & n_add_sub: BOOL & pv2: BOOL & h4: BOOL & z6: BOOL & s7: BOOL | f0|->BYTE_TO_SCHAR(SCHAR_TO_BYTE(rgs8_(f0))<+{7|->bool_to_bit(s7),6|->bool_to_bit(z6),4|->bool_to_bit(h4),2|->bool_to_bit(pv2),1|->bool_to_bit(n_add_sub),0|->bool_to_bit(c0)})) & id_reg_8: FIN(INTEGER) & not(id_reg_8 = {}) & id_reg_16: FIN(INTEGER) & not(id_reg_16 = {}))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(Z80LogAri),Machine(POWER2))==(?);
  Seen_Context_List_Enumerated(Machine(Z80LogAri))==(?);
  Seen_Context_List_Invariant(Machine(Z80LogAri))==(btrue);
  Seen_Context_List_Assertions(Machine(Z80LogAri))==((2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_not(bv_not(bv))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high => bv_size(bv_sub(bv,low,high)) = high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_and(v1,v2)(indx) = bv_and(v2,v1)(indx));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_and(v1,bv_and(v2,v3))(indx) = bv_and(bv_and(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_zero(bv_size(bv)))(indx) = bv_zero(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_and(bv,bv_one(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_or(v1,v2)(indx) = bv_or(v2,v1)(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2,v3,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) & indx: BV_INDX(v1) => bv_or(v1,bv_or(v2,v3))(indx) = bv_or(bv_or(v1,v2),v3)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_one(bv_size(bv)))(indx) = bv_one(bv_size(bv))(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_or(bv,bv_zero(bv_size(bv)))(indx) = bv(indx));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2,indx).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) & indx: BV_INDX(v1) => bv_xor(v1,v2)(indx) = bv_xor(v2,v1)(indx));!(bv,indx).(bv: BIT_VECTOR & indx: BV_INDX(bv) => bv_xor(bv,bv)(indx) = bv_zero(bv_size(bv))(indx))) & !bb.(bb: BIT & not(bb = 1) => bb = 0) & !bb.(bb: BIT & not(bb = 0) => bb = 1) & !bb.(bb: BIT => bb = 0 or bb = 1) & bool_to_bit(FALSE) = 0 & bool_to_bit(TRUE) = 1 & !bb.(bb: BIT => bit_xor(bb,bb) = 0) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(1,b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_xor(b1,b2) = 1 => bit_xor(b1,bit_xor(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 0 => bit_xor(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_xor(b1,b2) = 1 => bit_xor(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1)) & bit_xor(1,1) = 0 & bit_xor(1,0) = 1 & bit_xor(0,1) = 1 & bit_xor(0,0) = 0 & !b1.(b1: BIT => bit_or(0,b1) = b1) & !b1.(b1: BIT => bit_or(1,b1) = 1) & !b1.(b1: BIT => bit_or(b1,0) = b1) & !b1.(b1: BIT => bit_or(b1,1) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = bit_or(1,b3)) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT & bit_or(b1,b2) = 1 => bit_or(b1,bit_or(b2,b3)) = 1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 0 => b1 = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,0) = 1 => b1 = 1) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 0 => bit_or(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_or(b1,b2) = 1 => bit_or(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1)) & bit_or(1,1) = 1 & bit_or(1,0) = 1 & bit_or(0,1) = 1 & bit_or(0,0) = 0 & !b1.(b1: BIT => bit_and(b1,0) = 0) & !b1.(b1: BIT => bit_and(b1,1) = b1) & !(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3)) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 0 => bit_and(b2,b1) = 0) & !(b1,b2).(b1: BIT & b2: BIT & bit_and(b1,b2) = 1 => bit_and(b2,b1) = 1) & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1)) & bit_and(1,1) = 1 & bit_and(1,0) = 0 & bit_and(0,1) = 0 & bit_and(0,0) = 0 & !bb.(bb: BIT => bit_not(bit_not(bb)) = bb) & bit_not(1) = 0 & bit_not(0) = 1 & 0 = SCHAR_TO_SSHORTINT(0,0) & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & 2**16 = 65536 & !n0.(n0: SCHAR => n0<=255) & !n0.(n0: SCHAR => 0<=n0) & NB_SCHARS = 256 & !(nn,pp).(nn: INT & pp: NAT => (pp = 0 => nn**pp = 1) & (pp = 1 => nn**pp = nn) & (pp>1 => nn**pp = nn*nn**(pp-1))));
  Seen_Context_List_Properties(Machine(Z80LogAri))==(SCHAR_LENGTH: NATURAL & SCHAR_LENGTH = 8 & NB_SCHARS: NATURAL & NB_SCHARS = 2**SCHAR_LENGTH & SCHAR = -128..127 & SCHAR_POSITION = 0..SCHAR_LENGTH-1 & UCHAR = 0..255 & SSHORTINT_LENGTH: NATURAL & SSHORTINT_LENGTH = 16 & NB_SSHORTINTS: NATURAL & NB_SSHORTINTS = 2**SSHORTINT_LENGTH & SSHORTINT = -32768..32767 & SSHORTINT_POSITION = 0..SSHORTINT_LENGTH-1 & BV16 <: BIT_VECTOR & BV16 = {vv | vv: BIT_VECTOR & size(vv) = SSHORTINT_LENGTH} & BYTE_TO_UCHAR: BYTE --> UCHAR & BYTE_TO_UCHAR = %v0.(v0: BYTE | (-128)*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & UCHAR_TO_BYTE: UCHAR --> BYTE & UCHAR_TO_BYTE = BYTE_TO_UCHAR~ & USHORTINT = 0..65535 & NB_INSTRUCTIONS: NATURAL & INST_SZ: NATURAL & INST_SZ = 16 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = USHORTINT & INSTRUCTION_NEXT: USHORTINT --> USHORTINT & INSTRUCTION_NEXT = {p0,q0 | p0: INSTRUCTION & q0: INSTRUCTION & 0<=p0 & p0<NB_INSTRUCTIONS-1 & q0 = p0+1}\/{NB_INSTRUCTIONS-1|->0} & INSTRUCTION_JUMP = %(p0,e0).(p0: INSTRUCTION & e0: -126..129 | p0+e0) & BYTE_TO_SCHAR: BYTE --> SCHAR & BYTE_TO_SCHAR = %v0.(v0: BYTE | (-128)*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SCHAR_TO_BYTE: SCHAR --> BYTE & SCHAR_TO_BYTE = BYTE_TO_SCHAR~ & BV16_TO_SSHORTINT: BV16 --> SSHORTINT & BV16_TO_SSHORTINT = %v0.(v0: BV16 | (-32768)*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & SSHORTINT_TO_BV16: SSHORTINT --> BV16 & SSHORTINT_TO_BV16 = BV16_TO_SSHORTINT~ & BYTE_TO_BV16: BYTE*BYTE --> BV16 & BYTE_TO_BV16 = %(v1,v2).(v1: BV16 & v2: BV16 | {0|->v2(0),1|->v2(1),2|->v2(2),3|->v2(3),4|->v2(4),5|->v2(5),6|->v2(6),7|->v2(7),8|->v1(0),9|->v1(1),10|->v1(2),11|->v1(3),12|->v1(4),13|->v1(5),14|->v1(6),15|->v1(7)}) & BV16_TO_BYTE: BV16 --> BYTE*BYTE & BV16_TO_BYTE = BYTE_TO_BV16~ & SCHAR_TO_SSHORTINT: SCHAR*SCHAR --> SSHORTINT & SCHAR_TO_SSHORTINT = %(w1,w2).(w1: SCHAR & w2: SCHAR | BV16_TO_SSHORTINT(BYTE_TO_BV16(SCHAR_TO_BYTE(w1),SCHAR_TO_BYTE(w2)))) & SSHORTINT_TO_SCHAR: SSHORTINT --> SCHAR*SCHAR & SSHORTINT_TO_SCHAR = SCHAR_TO_SSHORTINT~ & BV16_TO_USHORTINT: BV16 --> USHORTINT & BV16_TO_USHORTINT = %v0.(v0: BV16 | (-32768)*v0(15)+16384*v0(14)+8192*v0(13)+4096*v0(12)+2048*v0(11)+1024*v0(10)+512*v0(9)+256*v0(8)+128*v0(7)+64*v0(6)+32*v0(5)+16*v0(4)+8*v0(3)+4*v0(2)+2*v0(1)+v0(0)) & USHORTINT_TO_BV16: USHORTINT --> BV16 & USHORTINT_TO_BV16 = BV16_TO_USHORTINT~ & parity_bit_from_BYTE: BIT_VECTOR --> BIT & parity_bit_from_BYTE = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | bv(idx)) mod 2) & SSHORTINT_TO_USHORTINT: SSHORTINT --> USHORTINT & SSHORTINT_TO_USHORTINT = %v0.(v0: SSHORTINT | v0-32768) & USHORTINT_TO_SSHORTINT: USHORTINT --> SSHORTINT & USHORTINT_TO_SSHORTINT = SSHORTINT_TO_USHORTINT~ & BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & BIT_VECTOR = {bt | bt: NATURAL +-> BIT}-{{}} & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & BV_INDX: BIT_VECTOR --> POW(NATURAL) & BV_INDX = %bv.(bv: BIT_VECTOR | 0..bv_size(bv)-1) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v1^v2) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: BV_INDX(bv) & high: BV_INDX(bv) & low<=high | low..high<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (0..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (0..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: BV_INDX(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) | %idx.(idx: BV_INDX(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1(idx)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: BV_INDX(v1) | v1<+{idx|->0}) & bv_put: BIT_VECTOR*NATURAL*BIT --> BIT_VECTOR & bv_put = %(v1,idx,bit).(v1: BIT_VECTOR & idx: BV_INDX(v1) & bit: BIT | v1<+{idx|->bit}) & BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE <: BIT_VECTOR & BYTE = {vv | vv: BIT_VECTOR & bv_size(vv) = BYTE_WIDTH} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0});
  Seen_List_Constraints(Machine(Z80LogAri))==(btrue);
  Seen_List_Operations(Machine(Z80LogAri),Machine(POWER2))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80LogAri),Machine(POWER2))==(btrue);
  Seen_Internal_List_Operations(Machine(Z80LogAri),Machine(ALU))==(?);
  Seen_List_Operations(Machine(Z80LogAri),Machine(ALU))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80LogAri),Machine(ALU))==(btrue);
  Seen_Internal_List_Operations(Machine(Z80LogAri),Machine(TYPES))==(?);
  Seen_List_Operations(Machine(Z80LogAri),Machine(TYPES))==(?);
  Seen_Expanded_List_Invariant(Machine(Z80LogAri),Machine(TYPES))==(btrue)
END
&
THEORY ListANYVarX IS
  List_ANY_Var(Machine(Z80LogAri),ADD_A_r)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),ADD_A_n)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),ADD_A_9HL0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),ADD_A_9IX_d0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),ADD_A_9IY_d0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),ADC_A_r)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),ADC_A_n)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),ADC_A_9HL0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),ADC_A_9IX_d0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),ADC_A_9IY_d0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),SUB_A_r)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),SUB_A_n)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),SUB_A_9HL0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),SUB_A_9IX_d0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),SUB_A_9IY_d0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),SBC_A_r)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),SBC_A_n)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),SBC_A_9HL0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),SBC_A_9IX_d0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),SBC_A_9IY_d0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),AND_A_r)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(add_sub) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),AND_A_n)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(add_sub) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),AND_A_9HL0)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(nn) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),AND_A_9IX_d0)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(nn) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),AND_A_9IY_d0)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(nn) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),OR_A_r)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(add_sub) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),OR_A_n)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(add_sub) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),OR_A_9HL0)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(nn) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),OR_A_9IX_d0)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(nn) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),OR_A_9IY_d0)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(nn) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),XOR_A_r)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(add_sub) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),XOR_A_n)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(add_sub) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),XOR_A_9HL0)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(nn) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),XOR_A_9IX_d0)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(nn) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),XOR_A_9IY_d0)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(nn) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),CP_A_r)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(n_add_sub) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),CP_A_n)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(n_add_sub) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),CP_A_9HL0)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(n_add_sub) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),CP_A_9IX_d0)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(n_add_sub) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),CP_A_9IY_d0)==((Var(result) == btype(INTEGER,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(n_add_sub) == btype(BOOL,?,?)),(Var(pv) == btype(BOOL,?,?)),(Var(halfcarry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)),(Var(is_negative) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),INC_r)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),INC_9HL0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),INC_9IX_d0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),INC_9IY_d0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),DEC_r)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),DEC_9HL0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),DEC_9IX_d0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),DEC_9IY_d0)==((Var(sum) == btype(INTEGER,?,?)),(Var(is_negative) == btype(BOOL,?,?)),(Var(carry) == btype(BOOL,?,?)),(Var(digit_carry) == btype(BOOL,?,?)),(Var(zero) == btype(BOOL,?,?)));
  List_ANY_Var(Machine(Z80LogAri),?)==(?)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(Z80LogAri)) == (REG16_TO_REG8,REG8_TO_REG16,update_flag_register,get_new_flag_register,id_reg_8,id_reg_16,a0,f0,f_0,a_0,b0,c0,b_0,c_0,d0,e0,d_0,e_0,h0,l0,h_0,l_0,i0,r0,BC,DE,HL,SP,AF | ? | i_o_ports,iy,ix,sp,pc,rgs8 | stack,mem | ADD_A_r,ADD_A_n,ADD_A_9HL0,ADD_A_9IX_d0,ADD_A_9IY_d0,ADC_A_r,ADC_A_n,ADC_A_9HL0,ADC_A_9IX_d0,ADC_A_9IY_d0,SUB_A_r,SUB_A_n,SUB_A_9HL0,SUB_A_9IX_d0,SUB_A_9IY_d0,SBC_A_r,SBC_A_n,SBC_A_9HL0,SBC_A_9IX_d0,SBC_A_9IY_d0,AND_A_r,AND_A_n,AND_A_9HL0,AND_A_9IX_d0,AND_A_9IY_d0,OR_A_r,OR_A_n,OR_A_9HL0,OR_A_9IX_d0,OR_A_9IY_d0,XOR_A_r,XOR_A_n,XOR_A_9HL0,XOR_A_9IX_d0,XOR_A_9IY_d0,CP_A_r,CP_A_n,CP_A_9HL0,CP_A_9IX_d0,CP_A_9IY_d0,INC_r,INC_9HL0,INC_9IX_d0,INC_9IY_d0,DEC_r,DEC_9HL0,DEC_9IX_d0,DEC_9IY_d0 | ? | seen(Machine(TYPES)),seen(Machine(ALU)),seen(Machine(POWER2)),included(Machine(MEMORY)) | ? | Z80LogAri);
  List_Of_HiddenCst_Ids(Machine(Z80LogAri)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(Z80LogAri)) == (REG16_TO_REG8,REG8_TO_REG16,update_flag_register,get_new_flag_register);
  List_Of_VisibleVar_Ids(Machine(Z80LogAri)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(Z80LogAri)) == (seen(Machine(TYPES)): (SCHAR,SCHAR_LENGTH,SCHAR_POSITION,NB_SCHARS,BYTE_TO_SCHAR,SCHAR_TO_BYTE,UCHAR,UCHAR_TO_BYTE,BYTE_TO_UCHAR,BV16,SSHORTINT,SSHORTINT_LENGTH,SSHORTINT_POSITION,NB_SSHORTINTS,BV16_TO_SSHORTINT,SSHORTINT_TO_BV16,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,INSTRUCTION_JUMP,BYTE_TO_BV16,BV16_TO_BYTE,SCHAR_TO_SSHORTINT,SSHORTINT_TO_SCHAR,parity_bit_from_BYTE,USHORTINT,BV16_TO_USHORTINT,USHORTINT_TO_BV16,USHORTINT_TO_SSHORTINT,SSHORTINT_TO_USHORTINT | BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO,BV_INDX,BIT_VECTOR,bv_catenate,bv_sub,bv_zero,bv_one,bv_size,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear,bv_put,BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ?) | seen(Machine(ALU)): (add16SCHAR,halfSCHAR,add8SCHAR,substract8SCHAR,andSCHAR,iorSCHAR,xorSCHAR,bitclearSCHAR,bitsetSCHAR,bitgetSCHAR,complementSCHAR,swapSCHAR,rotateleftSCHAR,rotaterightSCHAR,is_zeroSCHAR,is_negative | ? | ? | ? | ? | ? | ? | ? | ?));
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
  Sets(Machine(Z80LogAri)) == (Type(id_reg_8) == Cst(SetOf(etype(id_reg_8,0,17)));Type(id_reg_16) == Cst(SetOf(etype(id_reg_16,0,4))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(Z80LogAri)) == (Type(a0) == Cst(etype(id_reg_8,0,17));Type(f0) == Cst(etype(id_reg_8,0,17));Type(f_0) == Cst(etype(id_reg_8,0,17));Type(a_0) == Cst(etype(id_reg_8,0,17));Type(b0) == Cst(etype(id_reg_8,0,17));Type(c0) == Cst(etype(id_reg_8,0,17));Type(b_0) == Cst(etype(id_reg_8,0,17));Type(c_0) == Cst(etype(id_reg_8,0,17));Type(d0) == Cst(etype(id_reg_8,0,17));Type(e0) == Cst(etype(id_reg_8,0,17));Type(d_0) == Cst(etype(id_reg_8,0,17));Type(e_0) == Cst(etype(id_reg_8,0,17));Type(h0) == Cst(etype(id_reg_8,0,17));Type(l0) == Cst(etype(id_reg_8,0,17));Type(h_0) == Cst(etype(id_reg_8,0,17));Type(l_0) == Cst(etype(id_reg_8,0,17));Type(i0) == Cst(etype(id_reg_8,0,17));Type(r0) == Cst(etype(id_reg_8,0,17));Type(BC) == Cst(etype(id_reg_16,0,4));Type(DE) == Cst(etype(id_reg_16,0,4));Type(HL) == Cst(etype(id_reg_16,0,4));Type(SP) == Cst(etype(id_reg_16,0,4));Type(AF) == Cst(etype(id_reg_16,0,4));Type(REG16_TO_REG8) == Cst(SetOf(etype(id_reg_16,0,4)*(etype(id_reg_8,0,17)*etype(id_reg_8,0,17))));Type(REG8_TO_REG16) == Cst(SetOf(etype(id_reg_8,?,?)*etype(id_reg_8,?,?)*etype(id_reg_16,?,?)));Type(update_flag_register) == Cst(SetOf(SetOf(etype(id_reg_8,?,?)*btype(INTEGER,?,?))*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*SetOf(etype(id_reg_8,?,?)*btype(INTEGER,?,?))));Type(get_new_flag_register) == Cst(SetOf(SetOf(etype(id_reg_8,?,?)*btype(INTEGER,?,?))*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*btype(BOOL,?,?)*(etype(id_reg_8,?,?)*btype(INTEGER,?,?)))))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(Z80LogAri)) == (Type(mem) == Mvl(SetOf(btype(INTEGER,"[USHORTINT","]USHORTINT")*btype(INTEGER,"[SCHAR","]SCHAR")));Type(stack) == Mvl(SetOf(btype(INTEGER,"[USHORTINT","]USHORTINT")*btype(INTEGER,"[SCHAR","]SCHAR")));Type(i_o_ports) == Mvl(SetOf(btype(INTEGER,"[UCHAR","]UCHAR")*btype(INTEGER,"[SCHAR","]SCHAR")));Type(iy) == Mvl(btype(INTEGER,?,?));Type(ix) == Mvl(btype(INTEGER,?,?));Type(sp) == Mvl(btype(INTEGER,?,?));Type(pc) == Mvl(btype(INTEGER,?,?));Type(rgs8) == Mvl(SetOf(etype(id_reg_8,0,17)*btype(INTEGER,"[SCHAR","]SCHAR"))))
END
&
THEORY OperationsEnvX IS
  Operations(Machine(Z80LogAri)) == (Type(DEC_9IY_d0) == Cst(No_type,No_type);Type(DEC_9IX_d0) == Cst(No_type,No_type);Type(DEC_9HL0) == Cst(No_type,No_type);Type(DEC_r) == Cst(No_type,etype(id_reg_8,?,?));Type(INC_9IY_d0) == Cst(No_type,No_type);Type(INC_9IX_d0) == Cst(No_type,No_type);Type(INC_9HL0) == Cst(No_type,No_type);Type(INC_r) == Cst(No_type,etype(id_reg_8,?,?));Type(CP_A_9IY_d0) == Cst(No_type,No_type);Type(CP_A_9IX_d0) == Cst(No_type,No_type);Type(CP_A_9HL0) == Cst(No_type,No_type);Type(CP_A_n) == Cst(No_type,btype(INTEGER,?,?));Type(CP_A_r) == Cst(No_type,etype(id_reg_8,?,?));Type(XOR_A_9IY_d0) == Cst(No_type,No_type);Type(XOR_A_9IX_d0) == Cst(No_type,No_type);Type(XOR_A_9HL0) == Cst(No_type,No_type);Type(XOR_A_n) == Cst(No_type,btype(INTEGER,?,?));Type(XOR_A_r) == Cst(No_type,etype(id_reg_8,?,?));Type(OR_A_9IY_d0) == Cst(No_type,No_type);Type(OR_A_9IX_d0) == Cst(No_type,No_type);Type(OR_A_9HL0) == Cst(No_type,No_type);Type(OR_A_n) == Cst(No_type,btype(INTEGER,?,?));Type(OR_A_r) == Cst(No_type,etype(id_reg_8,?,?));Type(AND_A_9IY_d0) == Cst(No_type,No_type);Type(AND_A_9IX_d0) == Cst(No_type,No_type);Type(AND_A_9HL0) == Cst(No_type,No_type);Type(AND_A_n) == Cst(No_type,btype(INTEGER,?,?));Type(AND_A_r) == Cst(No_type,etype(id_reg_8,?,?));Type(SBC_A_9IY_d0) == Cst(No_type,No_type);Type(SBC_A_9IX_d0) == Cst(No_type,No_type);Type(SBC_A_9HL0) == Cst(No_type,No_type);Type(SBC_A_n) == Cst(No_type,btype(INTEGER,?,?));Type(SBC_A_r) == Cst(No_type,etype(id_reg_8,?,?));Type(SUB_A_9IY_d0) == Cst(No_type,No_type);Type(SUB_A_9IX_d0) == Cst(No_type,No_type);Type(SUB_A_9HL0) == Cst(No_type,No_type);Type(SUB_A_n) == Cst(No_type,btype(INTEGER,?,?));Type(SUB_A_r) == Cst(No_type,etype(id_reg_8,?,?));Type(ADC_A_9IY_d0) == Cst(No_type,No_type);Type(ADC_A_9IX_d0) == Cst(No_type,No_type);Type(ADC_A_9HL0) == Cst(No_type,No_type);Type(ADC_A_n) == Cst(No_type,btype(INTEGER,?,?));Type(ADC_A_r) == Cst(No_type,etype(id_reg_8,?,?));Type(ADD_A_9IY_d0) == Cst(No_type,No_type);Type(ADD_A_9IX_d0) == Cst(No_type,No_type);Type(ADD_A_9HL0) == Cst(No_type,No_type);Type(ADD_A_n) == Cst(No_type,btype(INTEGER,?,?));Type(ADD_A_r) == Cst(No_type,etype(id_reg_8,?,?)))
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
