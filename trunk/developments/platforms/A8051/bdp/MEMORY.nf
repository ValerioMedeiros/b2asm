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
  List_Sees(Machine(MEMORY))==(TYPES)
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
  Local_List_Variables(Machine(MEMORY))==(?);
  List_Variables(Machine(MEMORY))==(?);
  External_List_Variables(Machine(MEMORY))==(?)
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
  List_Invariant(Machine(MEMORY))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(MEMORY))==(btrue);
  Abstract_List_Assertions(Machine(MEMORY))==(btrue);
  Context_List_Assertions(Machine(MEMORY))==(NB_WORD = 256 & !n.(n : WORD => 0<=n & n<=255) & INSTRUCTION_IJUMP(5,1) = 7 & INSTRUCTION_IJUMP(0,2) = 3 & INSTRUCTION_IJUMP(2,4) = 7 & INSTRUCTION_IJUMP(3,2) = 6 & INSTRUCTION_NEXT(0) = 1 & INSTRUCTION_NEXT(1) = 2 & INSTRUCTION_NEXT(2) = 3 & INSTRUCTION_NEXT(3) = 4 & INSTRUCTION_NEXT(4) = 5 & INSTRUCTION_NEXT(5) = 6 & INSTRUCTION_NEXT(6) = 7 & INSTRUCTION_IJUMP(3,3) = 7 & MEM_ADDR <: WORD & RAM_ADDR <: WORD & SFR_ADDR <: WORD & MEM_ADDR = RAM_ADDR\/SFR_ADDR & BIT_FLIP(0) = 1 & BIT_FLIP(1) = 0 & BIT_AND(0,0) = 0 & BIT_AND(0,1) = 0 & BIT_AND(1,0) = 0 & BIT_AND(1,1) = 1 & BIT_IOR(0,0) = 0 & BIT_IOR(0,1) = 0 & BIT_IOR(1,0) = 0 & BIT_IOR(1,1) = 1 & BIT_XOR(0,0) = 0 & BIT_XOR(0,1) = 1 & BIT_XOR(1,0) = 1 & BIT_XOR(1,1) = 0);
  List_Assertions(Machine(MEMORY))==(btrue)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(MEMORY))==(skip);
  Context_List_Initialisation(Machine(MEMORY))==(skip);
  List_Initialisation(Machine(MEMORY))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(MEMORY))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(MEMORY),Machine(TYPES))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(MEMORY))==(btrue);
  List_Constraints(Machine(MEMORY))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(MEMORY))==(?);
  List_Operations(Machine(MEMORY))==(?)
END
&
THEORY ListInputX END
&
THEORY ListOutputX END
&
THEORY ListHeaderX END
&
THEORY ListPreconditionX END
&
THEORY ListSubstitutionX END
&
THEORY ListConstantsX IS
  List_Valuable_Constants(Machine(MEMORY))==(sbuf,pcon,SCON,B98,B99,B9A,B9B,B9C,B9D,B9E,B9F,SCON_0,SCON_1,SCON_2,SCON_3,SCON_4,SCON_5,SCON_6,SCON_7,RI,TI,RB8,TB8,REN,SM2,SM1,SM0,R0,R1,R2,R3,R4,R5,R6,R7,A,ACC,BE0,BE1,BE2,BE3,BE4,BE5,BE6,BE7,B,BF0,BF1,BF2,BF3,BF4,BF5,BF6,BF7,P0,P0_0,P0_1,P0_2,P0_3,P0_4,P0_5,P0_6,P0_7,B80,B81,B82,B83,B84,B85,B86,B87,P1,P1_0,P1_1,P1_2,P1_3,P1_4,P1_5,P1_6,P1_7,B90,B91,B92,B93,B94,B95,B96,B97,P2,P2_0,P2_1,P2_2,P2_3,P2_4,P2_5,P2_6,P2_7,BA0,BA1,BA2,BA3,BA4,BA5,BA6,BA7,P3,P3_0,P3_1,P3_2,P3_3,P3_4,P3_5,P3_6,P3_7,BB0,BB1,BB2,BB3,BB4,BB5,BB6,BB7,psw,BD0,BD1,BD2,BD3,BD4,BD5,BD6,BD7,PSW_0,PSW_1,PSW_2,PSW_3,PSW_4,PSW_5,PSW_6,PSW_7,P,OV,RS0,RS1,F0,AC,CY,C);
  Inherited_List_Constants(Machine(MEMORY))==(?);
  List_Constants(Machine(MEMORY))==(sbuf,pcon,SCON,B98,B99,B9A,B9B,B9C,B9D,B9E,B9F,SCON_0,SCON_1,SCON_2,SCON_3,SCON_4,SCON_5,SCON_6,SCON_7,RI,TI,RB8,TB8,REN,SM2,SM1,SM0,R0,R1,R2,R3,R4,R5,R6,R7,A,ACC,BE0,BE1,BE2,BE3,BE4,BE5,BE6,BE7,B,BF0,BF1,BF2,BF3,BF4,BF5,BF6,BF7,P0,P0_0,P0_1,P0_2,P0_3,P0_4,P0_5,P0_6,P0_7,B80,B81,B82,B83,B84,B85,B86,B87,P1,P1_0,P1_1,P1_2,P1_3,P1_4,P1_5,P1_6,P1_7,B90,B91,B92,B93,B94,B95,B96,B97,P2,P2_0,P2_1,P2_2,P2_3,P2_4,P2_5,P2_6,P2_7,BA0,BA1,BA2,BA3,BA4,BA5,BA6,BA7,P3,P3_0,P3_1,P3_2,P3_3,P3_4,P3_5,P3_6,P3_7,BB0,BB1,BB2,BB3,BB4,BB5,BB6,BB7,psw,BD0,BD1,BD2,BD3,BD4,BD5,BD6,BD7,PSW_0,PSW_1,PSW_2,PSW_3,PSW_4,PSW_5,PSW_6,PSW_7,P,OV,RS0,RS1,F0,AC,CY,C)
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
  Context_List_Properties(Machine(MEMORY))==(MAX_ADDR : NATURAL & RAM_ADDR = 0..127 & SFR_ADDR = 128..MAX_ADDR & MAX_ADDR = 255 & MEM_ADDR = 0..MAX_ADDR & COMPLEMENT_2 = -128..127 & NB_WORD : NATURAL & WORD_LEN : NATURAL & WORD_LEN = 8 & WORD_POSITION = 0..WORD_LEN-1 & NB_WORD = 2**WORD_LEN & WORD = 0..NB_WORD-1 & NB_INSTRUCTIONS = 256 & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = 0..INSTRUCTION_MAX & INSTRUCTION_IJUMP : INSTRUCTION*INSTRUCTION --> INSTRUCTION & !(i,j,sum).(i : INSTRUCTION & j : INSTRUCTION & sum : NATURAL & sum = i+j => (sum>=255 => INSTRUCTION_IJUMP(i,j) = 0) & (sum<255 => INSTRUCTION_IJUMP(i,j) = sum+1)) & INSTRUCTION_NEXT : INSTRUCTION -->> INSTRUCTION & !i.(i : INSTRUCTION => (i = 255 => INSTRUCTION_NEXT(i) = 0) & (i<255 => INSTRUCTION_NEXT(i) = i+1)) & BV_TO_WORD : BV8 --> WORD & WORD_TO_BV : WORD --> BV8 & !(w,v).(w : WORD & v : BV8 => v = WORD_TO_BV(w) <=> (w = 128*v(7)+64*v(6)+32*v(5)+16*v(4)+8*v(3)+4*v(2)+2*v(1)+v(0))) & BV_TO_WORD = WORD_TO_BV~ & BIT_ADDRESS = WORD*WORD_POSITION & WORD_TO_COMP2 : WORD --> COMPLEMENT_2 & !w.(w : WORD => (w<=127 => WORD_TO_COMP2(w) = w) & (w>127 & w<=255 => WORD_TO_COMP2(w) = w-255)) & COMP2_TO_WORD = WORD_TO_COMP2~ & OVERFLOW_ADD : COMPLEMENT_2*COMPLEMENT_2 --> BIT & !(c1,c2,csum).(c1 : COMPLEMENT_2 & c2 : COMPLEMENT_2 & csum : INTEGER & csum = c1+c2 => (csum<=127 & -128<=csum => OVERFLOW_ADD(c1,c2) = 0) & (csum>127 & -128>csum => OVERFLOW_ADD(c1,c2) = 1)) & OVERFLOW_SUB : COMPLEMENT_2*COMPLEMENT_2*BIT --> BIT & !(c1,c2,cy,csum).(c1 : COMPLEMENT_2 & c2 : COMPLEMENT_2 & csum : INTEGER & cy : BIT & csum = c1-c2-cy => (csum<=127 & -128<=csum => OVERFLOW_SUB(c1,c2,cy) = 0) & (csum>127 & -128>csum => OVERFLOW_SUB(c1,c2,cy) = 1)) & BV8_INDEX = 0..7 & BV8 = BV8_INDEX --> BIT & BV8_SET_BIT : BV8*BV8_INDEX*BIT --> BV8 & !(v,i,j,b).(v : BV8 & i : BV8_INDEX & j : BV8_INDEX & b : BIT => (i/=j => BV8_SET_BIT(v,i,b)(j) = v(j))) & !(v,i,b).(v : BV8 & i : BV8_INDEX & b : BIT => BV8_SET_BIT(v,i,b)(i) = b) & BV8_COMPLEMENT : BV8 --> BV8 & !(v,i).(v : BV8 & i : BV8_INDEX => BV8_COMPLEMENT(v)(i) = BIT_FLIP(v(i))) & BV8_ALL_ZEROES : BV8 & !i.(i : BV8_INDEX => BV8_ALL_ZEROES(i) = 0) & BV8_AND : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_AND(v1,v2)(i) = BIT_AND(v1(i),v2(i))) & BV8_IOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_IOR(v1,v2)(i) = BIT_IOR(v1(i),v2(i))) & BV8_XOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_XOR(v1,v2)(i) = BIT_XOR(v1(i),v2(i))) & BV8_PAR : BV8 --> BIT & !(v1,t1,t2,t3,t4).(v1 : BV8 & t1 : BIT & t2 : BIT & t3 : BIT & t4 : BIT & t1 = BIT_XOR(v1(7),v1(6)) & t2 = BIT_XOR(v1(5),v1(4)) & t3 = BIT_XOR(v1(3),v1(2)) & t4 = BIT_XOR(v1(1),v1(0)) => BV8_PAR(v1) = BIT_XOR(BIT_XOR(t1,t2),BIT_XOR(t3,t4))) & BIT = 0..1 & BIT_FLIP : BIT --> BIT & !b.(b : BIT => BIT_FLIP(b) = 1-b) & BIT_AND : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_AND(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & BIT_IOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_IOR(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & BIT_XOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_XOR(b1,b2) = 1 <=> (b1 = 1 & b2 = 0 or (b1 = 0 & b2 = 1))));
  Inherited_List_Properties(Machine(MEMORY))==(btrue);
  List_Properties(Machine(MEMORY))==(sbuf = 153 & pcon = 135 & R0 = 0 & R1 = 1 & R2 = 2 & R3 = 3 & R4 = 4 & R5 = 5 & R6 = 6 & R7 = 7 & B = 240 & BF0 : BIT_ADDRESS & BF1 : BIT_ADDRESS & BF2 : BIT_ADDRESS & BF3 : BIT_ADDRESS & BF4 : BIT_ADDRESS & BF5 : BIT_ADDRESS & BF6 : BIT_ADDRESS & BF7 : BIT_ADDRESS & BF0 = B,0 & BF1 = B,1 & BF2 = B,2 & BF3 = B,3 & BF4 = B,4 & BF5 = B,5 & BF6 = B,6 & BF7 = B,7 & A = 224 & ACC = 224 & BE0 : BIT_ADDRESS & BE1 : BIT_ADDRESS & BE2 : BIT_ADDRESS & BE3 : BIT_ADDRESS & BE4 : BIT_ADDRESS & BE5 : BIT_ADDRESS & BE6 : BIT_ADDRESS & BE7 : BIT_ADDRESS & BE0 = ACC,0 & BE1 = ACC,1 & BE2 = ACC,2 & BE3 = ACC,3 & BE4 = ACC,4 & BE5 = ACC,5 & BE6 = ACC,6 & BE7 = ACC,7 & P0 = 128 & P0_0 : BIT_ADDRESS & P0_1 : BIT_ADDRESS & P0_2 : BIT_ADDRESS & P0_3 : BIT_ADDRESS & P0_4 : BIT_ADDRESS & P0_5 : BIT_ADDRESS & P0_6 : BIT_ADDRESS & P0_7 : BIT_ADDRESS & B80 : BIT_ADDRESS & B81 : BIT_ADDRESS & B82 : BIT_ADDRESS & B83 : BIT_ADDRESS & B84 : BIT_ADDRESS & B85 : BIT_ADDRESS & B86 : BIT_ADDRESS & B87 : BIT_ADDRESS & P0_0 = P0,0 & P0_1 = P0,1 & P0_2 = P0,2 & P0_3 = P0,3 & P0_4 = P0,4 & P0_5 = P0,5 & P0_6 = P0,6 & P0_7 = P0,7 & B80 = P0,0 & B81 = P0,1 & B82 = P0,2 & B83 = P0,3 & B84 = P0,4 & B85 = P0,5 & B86 = P0,6 & B87 = P0,7 & P1 = 144 & P1_0 : BIT_ADDRESS & P1_1 : BIT_ADDRESS & P1_2 : BIT_ADDRESS & P1_3 : BIT_ADDRESS & P1_4 : BIT_ADDRESS & P1_5 : BIT_ADDRESS & P1_6 : BIT_ADDRESS & P1_7 : BIT_ADDRESS & B90 : BIT_ADDRESS & B91 : BIT_ADDRESS & B92 : BIT_ADDRESS & B93 : BIT_ADDRESS & B94 : BIT_ADDRESS & B95 : BIT_ADDRESS & B96 : BIT_ADDRESS & B97 : BIT_ADDRESS & P1_0 = P1,0 & P1_1 = P1,1 & P1_2 = P1,2 & P1_3 = P1,3 & P1_4 = P1,4 & P1_5 = P1,5 & P1_6 = P1,6 & P1_7 = P1,7 & B90 = P1,0 & B91 = P1,1 & B92 = P1,2 & B93 = P1,3 & B94 = P1,4 & B95 = P1,5 & B96 = P1,6 & B97 = P1,7 & P2 = 160 & P2_0 : BIT_ADDRESS & P2_1 : BIT_ADDRESS & P2_2 : BIT_ADDRESS & P2_3 : BIT_ADDRESS & P2_4 : BIT_ADDRESS & P2_5 : BIT_ADDRESS & P2_6 : BIT_ADDRESS & P2_7 : BIT_ADDRESS & BA0 : BIT_ADDRESS & BA1 : BIT_ADDRESS & BA2 : BIT_ADDRESS & BA3 : BIT_ADDRESS & BA4 : BIT_ADDRESS & BA5 : BIT_ADDRESS & BA6 : BIT_ADDRESS & BA7 : BIT_ADDRESS & P2_0 = P2,0 & P2_1 = P2,1 & P2_2 = P2,2 & P2_3 = P2,3 & P2_4 = P2,4 & P2_5 = P2,5 & P2_6 = P2,6 & P2_7 = P2,7 & BA0 = P2,0 & BA1 = P2,1 & BA2 = P2,2 & BA3 = P2,3 & BA4 = P2,4 & BA5 = P2,5 & BA6 = P2,6 & BA7 = P2,7 & P3 = 176 & P3_0 : BIT_ADDRESS & P3_1 : BIT_ADDRESS & P3_2 : BIT_ADDRESS & P3_3 : BIT_ADDRESS & P3_4 : BIT_ADDRESS & P3_5 : BIT_ADDRESS & P3_6 : BIT_ADDRESS & P3_7 : BIT_ADDRESS & BB0 : BIT_ADDRESS & BB1 : BIT_ADDRESS & BB2 : BIT_ADDRESS & BB3 : BIT_ADDRESS & BB4 : BIT_ADDRESS & BB5 : BIT_ADDRESS & BB6 : BIT_ADDRESS & BB7 : BIT_ADDRESS & P3_0 = P3,0 & P3_1 = P3,1 & P3_2 = P3,2 & P3_3 = P3,3 & P3_4 = P3,4 & P3_5 = P3,5 & P3_6 = P3,6 & P3_7 = P3,7 & BB0 = P3,0 & BB1 = P3,1 & BB2 = P3,2 & BB3 = P3,3 & BB4 = P3,4 & BB5 = P3,5 & BB6 = P3,6 & BB7 = P3,7 & SCON = 152 & B98 : BIT_ADDRESS & B99 : BIT_ADDRESS & B9A : BIT_ADDRESS & B9B : BIT_ADDRESS & B9C : BIT_ADDRESS & B9D : BIT_ADDRESS & B9E : BIT_ADDRESS & B9F : BIT_ADDRESS & B98 = SCON,0 & B99 = SCON,1 & B9A = SCON,2 & B9B = SCON,3 & B9C = SCON,4 & B9D = SCON,5 & B9E = SCON,6 & B9F = SCON,7 & SCON_0 : BIT_ADDRESS & SCON_1 : BIT_ADDRESS & SCON_2 : BIT_ADDRESS & SCON_3 : BIT_ADDRESS & SCON_4 : BIT_ADDRESS & SCON_5 : BIT_ADDRESS & SCON_6 : BIT_ADDRESS & SCON_7 : BIT_ADDRESS & SCON_0 = SCON,0 & SCON_1 = SCON,1 & SCON_2 = SCON,2 & SCON_3 = SCON,3 & SCON_4 = SCON,4 & SCON_5 = SCON,5 & SCON_6 = SCON,6 & SCON_7 = SCON,7 & RI : BIT_ADDRESS & TI : BIT_ADDRESS & RB8 : BIT_ADDRESS & TB8 : BIT_ADDRESS & REN : BIT_ADDRESS & SM2 : BIT_ADDRESS & SM1 : BIT_ADDRESS & SM0 : BIT_ADDRESS & RI = SCON,0 & TI = SCON,1 & RB8 = SCON,2 & TB8 = SCON,3 & REN = SCON,4 & SM2 = SCON,5 & SM1 = SCON,6 & SM0 = SCON,7 & psw = 208 & BD0 : BIT_ADDRESS & BD1 : BIT_ADDRESS & BD2 : BIT_ADDRESS & BD3 : BIT_ADDRESS & BD4 : BIT_ADDRESS & BD5 : BIT_ADDRESS & BD6 : BIT_ADDRESS & BD7 : BIT_ADDRESS & BD0 = psw,0 & BD1 = psw,1 & BD2 = psw,2 & BD3 = psw,3 & BD4 = psw,4 & BD5 = psw,5 & BD6 = psw,6 & BD7 = psw,7 & PSW_0 : BIT_ADDRESS & PSW_1 : BIT_ADDRESS & PSW_2 : BIT_ADDRESS & PSW_3 : BIT_ADDRESS & PSW_4 : BIT_ADDRESS & PSW_5 : BIT_ADDRESS & PSW_6 : BIT_ADDRESS & PSW_7 : BIT_ADDRESS & PSW_0 = psw,0 & PSW_1 = psw,1 & PSW_2 = psw,2 & PSW_3 = psw,3 & PSW_4 = psw,4 & PSW_5 = psw,5 & PSW_6 = psw,6 & PSW_7 = psw,7 & P : BIT_ADDRESS & OV : BIT_ADDRESS & RS0 : BIT_ADDRESS & RS1 : BIT_ADDRESS & F0 : BIT_ADDRESS & AC : BIT_ADDRESS & CY : BIT_ADDRESS & P = psw,0 & OV = psw,2 & RS0 = psw,3 & RS1 = psw,4 & F0 = psw,5 & AC = psw,6 & CY = psw,7 & C = psw,7)
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(MEMORY),Machine(TYPES))==(?);
  Seen_Context_List_Enumerated(Machine(MEMORY))==(?);
  Seen_Context_List_Invariant(Machine(MEMORY))==(btrue);
  Seen_Context_List_Assertions(Machine(MEMORY))==(btrue);
  Seen_Context_List_Properties(Machine(MEMORY))==(btrue);
  Seen_List_Constraints(Machine(MEMORY))==(btrue);
  Seen_List_Operations(Machine(MEMORY),Machine(TYPES))==(?);
  Seen_Expanded_List_Invariant(Machine(MEMORY),Machine(TYPES))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(MEMORY)) == (sbuf,pcon,SCON,B98,B99,B9A,B9B,B9C,B9D,B9E,B9F,SCON_0,SCON_1,SCON_2,SCON_3,SCON_4,SCON_5,SCON_6,SCON_7,RI,TI,RB8,TB8,REN,SM2,SM1,SM0,R0,R1,R2,R3,R4,R5,R6,R7,A,ACC,BE0,BE1,BE2,BE3,BE4,BE5,BE6,BE7,B,BF0,BF1,BF2,BF3,BF4,BF5,BF6,BF7,P0,P0_0,P0_1,P0_2,P0_3,P0_4,P0_5,P0_6,P0_7,B80,B81,B82,B83,B84,B85,B86,B87,P1,P1_0,P1_1,P1_2,P1_3,P1_4,P1_5,P1_6,P1_7,B90,B91,B92,B93,B94,B95,B96,B97,P2,P2_0,P2_1,P2_2,P2_3,P2_4,P2_5,P2_6,P2_7,BA0,BA1,BA2,BA3,BA4,BA5,BA6,BA7,P3,P3_0,P3_1,P3_2,P3_3,P3_4,P3_5,P3_6,P3_7,BB0,BB1,BB2,BB3,BB4,BB5,BB6,BB7,psw,BD0,BD1,BD2,BD3,BD4,BD5,BD6,BD7,PSW_0,PSW_1,PSW_2,PSW_3,PSW_4,PSW_5,PSW_6,PSW_7,P,OV,RS0,RS1,F0,AC,CY,C | ? | ? | ? | ? | ? | seen(Machine(TYPES)) | ? | MEMORY);
  List_Of_HiddenCst_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(MEMORY)) == (sbuf,pcon,SCON,B98,B99,B9A,B9B,B9C,B9D,B9E,B9F,SCON_0,SCON_1,SCON_2,SCON_3,SCON_4,SCON_5,SCON_6,SCON_7,RI,TI,RB8,TB8,REN,SM2,SM1,SM0,R0,R1,R2,R3,R4,R5,R6,R7,A,ACC,BE0,BE1,BE2,BE3,BE4,BE5,BE6,BE7,B,BF0,BF1,BF2,BF3,BF4,BF5,BF6,BF7,P0,P0_0,P0_1,P0_2,P0_3,P0_4,P0_5,P0_6,P0_7,B80,B81,B82,B83,B84,B85,B86,B87,P1,P1_0,P1_1,P1_2,P1_3,P1_4,P1_5,P1_6,P1_7,B90,B91,B92,B93,B94,B95,B96,B97,P2,P2_0,P2_1,P2_2,P2_3,P2_4,P2_5,P2_6,P2_7,BA0,BA1,BA2,BA3,BA4,BA5,BA6,BA7,P3,P3_0,P3_1,P3_2,P3_3,P3_4,P3_5,P3_6,P3_7,BB0,BB1,BB2,BB3,BB4,BB5,BB6,BB7,psw,BD0,BD1,BD2,BD3,BD4,BD5,BD6,BD7,PSW_0,PSW_1,PSW_2,PSW_3,PSW_4,PSW_5,PSW_6,PSW_7,P,OV,RS0,RS1,F0,AC,CY,C);
  List_Of_VisibleVar_Ids(Machine(MEMORY)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(MEMORY)) == (? : ?);
  List_Of_Ids(Machine(TYPES)) == (WORD,WORD_LEN,WORD_POSITION,NB_WORD,COMPLEMENT_2,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_IJUMP,INSTRUCTION_NEXT,BV_TO_WORD,WORD_TO_BV,WORD_TO_COMP2,COMP2_TO_WORD,OVERFLOW_ADD,OVERFLOW_SUB,MEM_ADDR,RAM_ADDR,SFR_ADDR,MAX_ADDR,BIT_ADDRESS | BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR,BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BV8_PAR | ? | ? | ? | ? | included(Machine(TYPE_BV8)),included(Machine(TYPE_BIT)) | ? | TYPES);
  List_Of_HiddenCst_Ids(Machine(TYPES)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (WORD,WORD_LEN,WORD_POSITION,NB_WORD,COMPLEMENT_2,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_IJUMP,INSTRUCTION_NEXT,BV_TO_WORD,WORD_TO_BV,WORD_TO_COMP2,COMP2_TO_WORD,OVERFLOW_ADD,OVERFLOW_SUB,MEM_ADDR,RAM_ADDR,SFR_ADDR,MAX_ADDR,BIT_ADDRESS,BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR,BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BV8_PAR);
  List_Of_VisibleVar_Ids(Machine(TYPES)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPES)) == (seen(Machine(TYPE_BIT)) : (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(TYPE_BIT)) == (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | ? | ? | ? | TYPE_BIT);
  List_Of_HiddenCst_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BIT)) == (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR);
  List_Of_VisibleVar_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BIT)) == (? : ?);
  List_Of_Ids(Machine(TYPE_BV8)) == (BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BV8_PAR | ? | ? | ? | ? | ? | seen(Machine(TYPE_BIT)) | ? | TYPE_BV8);
  List_Of_HiddenCst_Ids(Machine(TYPE_BV8)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BV8)) == (BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BV8_PAR);
  List_Of_VisibleVar_Ids(Machine(TYPE_BV8)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BV8)) == (? : ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(MEMORY)) == (Type(sbuf) == Cst(btype(INTEGER,?,?));Type(pcon) == Cst(btype(INTEGER,?,?));Type(SCON) == Cst(btype(INTEGER,?,?));Type(B98) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B99) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9A) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9B) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9C) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9D) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9E) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B9F) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SCON_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RI) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(TI) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RB8) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(TB8) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(REN) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SM2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SM1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(SM0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(R0) == Cst(btype(INTEGER,?,?));Type(R1) == Cst(btype(INTEGER,?,?));Type(R2) == Cst(btype(INTEGER,?,?));Type(R3) == Cst(btype(INTEGER,?,?));Type(R4) == Cst(btype(INTEGER,?,?));Type(R5) == Cst(btype(INTEGER,?,?));Type(R6) == Cst(btype(INTEGER,?,?));Type(R7) == Cst(btype(INTEGER,?,?));Type(A) == Cst(btype(INTEGER,?,?));Type(ACC) == Cst(btype(INTEGER,?,?));Type(BE0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BE7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B) == Cst(btype(INTEGER,?,?));Type(BF0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BF7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0) == Cst(btype(INTEGER,?,?));Type(P0_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P0_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B80) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B81) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B82) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B83) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B84) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B85) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B86) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B87) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1) == Cst(btype(INTEGER,?,?));Type(P1_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P1_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B90) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B91) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B92) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B93) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B94) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B95) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B96) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(B97) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2) == Cst(btype(INTEGER,?,?));Type(P2_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P2_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BA7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3) == Cst(btype(INTEGER,?,?));Type(P3_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P3_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BB7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(psw) == Cst(btype(INTEGER,?,?));Type(BD0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(BD7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_2) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_3) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_4) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_5) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_6) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(PSW_7) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(P) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(OV) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RS0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(RS1) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(F0) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(AC) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(CY) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?));Type(C) == Cst(btype(INTEGER,?,?)*btype(INTEGER,?,?)))
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
