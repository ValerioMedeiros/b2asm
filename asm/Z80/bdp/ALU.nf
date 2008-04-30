Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(ALU))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(ALU))==(Machine(ALU));
  Level(Machine(ALU))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(ALU)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(ALU))==(TYPES)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(ALU))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(ALU))==(?);
  List_Includes(Machine(ALU))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(ALU))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(ALU))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(ALU))==(?);
  Context_List_Variables(Machine(ALU))==(?);
  Abstract_List_Variables(Machine(ALU))==(?);
  Local_List_Variables(Machine(ALU))==(?);
  List_Variables(Machine(ALU))==(?);
  External_List_Variables(Machine(ALU))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(ALU))==(?);
  Abstract_List_VisibleVariables(Machine(ALU))==(?);
  External_List_VisibleVariables(Machine(ALU))==(?);
  Expanded_List_VisibleVariables(Machine(ALU))==(?);
  List_VisibleVariables(Machine(ALU))==(?);
  Internal_List_VisibleVariables(Machine(ALU))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(ALU))==(btrue);
  Gluing_List_Invariant(Machine(ALU))==(btrue);
  Expanded_List_Invariant(Machine(ALU))==(btrue);
  Abstract_List_Invariant(Machine(ALU))==(btrue);
  Context_List_Invariant(Machine(ALU))==(btrue);
  List_Invariant(Machine(ALU))==(btrue)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(ALU))==(btrue);
  Abstract_List_Assertions(Machine(ALU))==(btrue);
  Context_List_Assertions(Machine(ALU))==(NB_WORD8S = 256 & !n.(n : WORD8 => 0<=n) & !n.(n : WORD8 => n<=255) & WORD8_POSITION = BV8_INDEX & BIT_FLIP(0) = 1 & BIT_FLIP(1) = 0 & BIT_AND(0,0) = 0 & BIT_AND(0,1) = 0 & BIT_AND(1,0) = 0 & BIT_AND(1,1) = 1 & BIT_IOR(0,0) = 0 & BIT_IOR(0,1) = 0 & BIT_IOR(1,0) = 0 & BIT_IOR(1,1) = 1 & BIT_XOR(0,0) = 0 & BIT_XOR(0,1) = 1 & BIT_XOR(1,0) = 1 & BIT_XOR(1,1) = 0);
  List_Assertions(Machine(ALU))==(dom(add) = WORD8*WORD8 & ran(add) <: WORD8*BOOL*BOOL & dom(substract) = WORD8*WORD8 & ran(substract) <: WORD8*BOOL*BOOL & dom(and) = WORD8*WORD8 & ran(and) <: WORD8*BOOL & dom(ior) = WORD8*WORD8 & ran(ior) <: WORD8*BOOL & dom(xor) = WORD8*WORD8 & ran(xor) <: WORD8*BOOL & dom(bitclear) = WORD8*WORD8_POSITION & ran(bitclear) <: WORD8 & dom(bitset) = WORD8*WORD8_POSITION & ran(bitset) <: WORD8 & dom(bitget) = WORD8*WORD8_POSITION & ran(bitget) <: BIT & dom(complement) = WORD8 & ran(complement) <: WORD8 & dom(swap) = WORD8 & ran(swap) <: WORD8 & ran(rotateleft) <: WORD8*BOOL & dom(rotateleft) = WORD8 & dom(rotateright) = WORD8 & ran(rotateright) <: WORD8*BOOL)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(ALU))==(skip);
  Context_List_Initialisation(Machine(ALU))==(skip);
  List_Initialisation(Machine(ALU))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(ALU))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(ALU),Machine(TYPES))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(ALU))==(btrue);
  List_Constraints(Machine(ALU))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(ALU))==(?);
  List_Operations(Machine(ALU))==(?)
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
  List_Valuable_Constants(Machine(ALU))==(add16,add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright);
  Inherited_List_Constants(Machine(ALU))==(?);
  List_Constants(Machine(ALU))==(add16,add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(ALU))==(?);
  Context_List_Defered(Machine(ALU))==(?);
  Context_List_Sets(Machine(ALU))==(?);
  List_Valuable_Sets(Machine(ALU))==(?);
  Inherited_List_Enumerated(Machine(ALU))==(?);
  Inherited_List_Defered(Machine(ALU))==(?);
  Inherited_List_Sets(Machine(ALU))==(?);
  List_Enumerated(Machine(ALU))==(?);
  List_Defered(Machine(ALU))==(?);
  List_Sets(Machine(ALU))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(ALU))==(?);
  Expanded_List_HiddenConstants(Machine(ALU))==(?);
  List_HiddenConstants(Machine(ALU))==(?);
  External_List_HiddenConstants(Machine(ALU))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(ALU))==(btrue);
  Context_List_Properties(Machine(ALU))==(WORD16_LENGTH : NATURAL & WORD16_LENGTH = 16 & NB_WORD16S : NATURAL & NB_WORD16S = 2**WORD16_LENGTH & WORD16 = 0..NB_WORD16S-1 & WORD16_POSITION = 0..WORD16_LENGTH-1 & WORD8_LENGTH : NATURAL & WORD8_LENGTH = 8 & NB_WORD8S : NATURAL & NB_WORD8S = 2**WORD8_LENGTH & WORD8 = 0..NB_WORD8S-1 & WORD8_POSITION = 0..WORD8_LENGTH-1 & NB_INSTRUCTIONS : NATURAL & INST_SZ : NATURAL & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = 0..INSTRUCTION_MAX & INSTRUCTION_NEXT : INSTRUCTION --> INSTRUCTION & INSTRUCTION_NEXT = {p,q | p : INSTRUCTION & q : INSTRUCTION & 0<=p & p<NB_INSTRUCTIONS-1 & q = p+1}\/{NB_INSTRUCTIONS-1|->0} & BV8_TO_WORD8 : BV8 --> WORD8 & BV8_TO_WORD8 = %v.(v : BV8 | 128*v(7)+64*v(6)+32*v(5)+16*v(4)+8*v(3)+4*v(2)+2*v(1)+v(0)) & WORD8_TO_BV8 : WORD8 --> BV8 & WORD8_TO_BV8 = BV8_TO_WORD8~ & BV16_TO_WORD16 : BV16 --> WORD16 & BV16_TO_WORD16 = %v.(v : BV16 | 32768*v(15)+16384*v(14)+8192*v(13)+4096*v(12)+2048*v(11)+1024*v(10)+512*v(9)+256*v(8)+128*v(7)+64*v(6)+32*v(5)+16*v(4)+8*v(3)+4*v(2)+2*v(1)+v(0)) & WORD16_TO_BV16 : WORD16 --> BV16 & WORD16_TO_BV16 = BV16_TO_WORD16~ & BV8_TO_BV16 : BV8*BV8 --> BV16 & BV8_TO_BV16 = %(v1,v2).(v1 : BV8 & v2 : BV8 | {0|->v1(0),1|->v1(1),2|->v1(2),3|->v1(3),4|->v1(4),5|->v1(5),6|->v1(6),7|->v1(7),8|->v2(0),9|->v2(1),10|->v2(2),11|->v2(3),12|->v2(4),13|->v2(5),14|->v2(6),15|->v2(7)}) & WORD8_TO_WORD16 : WORD8*WORD8 --> WORD16 & WORD8_TO_WORD16 = %(w1,w2).(w1 : WORD8 & w2 : WORD8 | BV16_TO_WORD16(BV8_TO_BV16(WORD8_TO_BV8(w1),WORD8_TO_BV8(w2)))) & WORD16_TO_WORD8 : WORD16 --> WORD8*WORD8 & WORD16_TO_WORD8 = WORD8_TO_WORD16~ & BIT = 0..1 & BIT_FLIP : BIT --> BIT & !b.(b : BIT => BIT_FLIP(b) = 1-b) & BIT_AND : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_AND(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & BIT_IOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_IOR(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & BIT_XOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_XOR(b1,b2) = 1 <=> (b1 = 1 & b2 = 0 or (b1 = 0 & b2 = 1))) & BV8_INDEX = 0..7 & BV8 = BV8_INDEX --> BIT & BV8_SET_BIT : BV8*BV8_INDEX*BIT --> BV8 & !(v,i,j,b).(v : BV8 & i : BV8_INDEX & j : BV8_INDEX & b : BIT => (i/=j => BV8_SET_BIT(v,i,b)(j) = v(j))) & !(v,i,b).(v : BV8 & i : BV8_INDEX & b : BIT => BV8_SET_BIT(v,i,b)(i) = b) & BV8_COMPLEMENT : BV8 --> BV8 & !(v,i).(v : BV8 & i : BV8_INDEX => BV8_COMPLEMENT(v)(i) = BIT_FLIP(v(i))) & BV8_ALL_ZEROES : BV8 & !i.(i : BV8_INDEX => BV8_ALL_ZEROES(i) = 0) & BV8_AND : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_AND(v1,v2)(i) = BIT_AND(v1(i),v2(i))) & BV8_IOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_IOR(v1,v2)(i) = BIT_IOR(v1(i),v2(i))) & BV8_XOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_XOR(v1,v2)(i) = BIT_XOR(v1(i),v2(i))) & BV16_INDEX = 0..15 & BV16 = BV16_INDEX --> BIT & BV16_SET_BIT : BV16*BV16_INDEX*BIT --> BV16 & !(v,i,j,b).(v : BV16 & i : BV16_INDEX & j : BV16_INDEX & b : BIT => (i/=j => BV16_SET_BIT(v,i,b)(j) = v(j))) & !(v,i,b).(v : BV16 & i : BV16_INDEX & b : BIT => BV16_SET_BIT(v,i,b)(i) = b) & BV16_COMPLEMENT : BV16 --> BV16 & !(v,i).(v : BV16 & i : BV16_INDEX => BV16_COMPLEMENT(v)(i) = BIT_FLIP(v(i))) & BV16_ALL_ZEROES : BV16 & !i.(i : BV16_INDEX => BV16_ALL_ZEROES(i) = 0) & BV16_AND : BV16*BV16 --> BV16 & !(v1,v2,i).(v1 : BV16 & v2 : BV16 & i : BV16_INDEX => BV16_AND(v1,v2)(i) = BIT_AND(v1(i),v2(i))) & BV16_IOR : BV16*BV16 --> BV16 & !(v1,v2,i).(v1 : BV16 & v2 : BV16 & i : BV16_INDEX => BV16_IOR(v1,v2)(i) = BIT_IOR(v1(i),v2(i))) & BV16_XOR : BV16*BV16 --> BV16 & !(v1,v2,i).(v1 : BV16 & v2 : BV16 & i : BV16_INDEX => BV16_XOR(v1,v2)(i) = BIT_XOR(v1(i),v2(i))));
  Inherited_List_Properties(Machine(ALU))==(btrue);
  List_Properties(Machine(ALU))==(add16 : WORD16*WORD16 --> WORD16*BOOL*BOOL & !(w1,w2,sum).(w1 : WORD16 & w2 : WORD16 & sum : NATURAL & sum = w1+w2 => (sum<=65535 => add16(w1,w2) = sum,bool(sum = 0),FALSE) & (65536<=sum => add16(w1,w2) = sum-65536,bool(sum = 65536),TRUE)) & add : WORD8*WORD8 --> WORD8*BOOL*BOOL & !(w1,w2,sum).(w1 : WORD8 & w2 : WORD8 & sum : NATURAL & sum = w1+w2 => (sum<=255 => add(w1,w2) = sum,bool(sum = 0),FALSE) & (256<=sum => add(w1,w2) = sum-256,bool(sum = 256),TRUE)) & substract : WORD8*WORD8 --> WORD8*BOOL*BOOL & !(w1,w2,diff).(w1 : WORD8 & w2 : WORD8 & diff : INTEGER & diff = w1-w2 => (diff<0 => substract(w1,w2) = diff+256,FALSE,TRUE) & (diff>=0 => substract(w1,w2) = diff,bool(diff = 0),FALSE)) & and : WORD8*WORD8 --> WORD8*BOOL & !(w1,w2,w).(w1 : WORD8 & w2 : WORD8 & w : WORD8 & w = BV8_TO_WORD8(BV8_AND(WORD8_TO_BV8(w1),WORD8_TO_BV8(w2))) => and(w1,w2) = w,bool(w = 0)) & ior : WORD8*WORD8 --> WORD8*BOOL & !(w1,w2,w).(w1 : WORD8 & w2 : WORD8 & w : WORD8 & w = BV8_TO_WORD8(BV8_IOR(WORD8_TO_BV8(w1),WORD8_TO_BV8(w2))) => ior(w1,w2) = w,bool(w = 0)) & xor : WORD8*WORD8 --> WORD8*BOOL & !(w1,w2,w).(w1 : WORD8 & w2 : WORD8 & w : WORD8 & w = BV8_TO_WORD8(BV8_XOR(WORD8_TO_BV8(w1),WORD8_TO_BV8(w2))) => xor(w1,w2) = w,bool(w = 0)) & bitget : WORD8*WORD8_POSITION --> BIT & !(w,i).(w : WORD8 & i : WORD8_POSITION => bitget(w,i) = WORD8_TO_BV8(w)(i)) & bitset : WORD8*WORD8_POSITION --> WORD8 & !(w,i).(w : WORD8 & i : WORD8_POSITION => bitset(w,i) = BV8_TO_WORD8(BV8_SET_BIT(WORD8_TO_BV8(w),i,1))) & bitclear : WORD8*WORD8_POSITION --> WORD8 & !(w,i,b).(w : WORD8 & i : WORD8_POSITION & b : BIT => bitclear(w,i) = BV8_TO_WORD8(BV8_SET_BIT(WORD8_TO_BV8(w),i,0))) & complement : WORD8 --> WORD8 & !w.(w : WORD8 => complement(w) = BV8_TO_WORD8(BV8_COMPLEMENT(WORD8_TO_BV8(w)))) & swap : WORD8 --> WORD8 & !(w,v).(w : WORD8 & v : BV8 => (v = WORD8_TO_BV8(w) => swap(w) = BV8_TO_WORD8({0|->v(4),1|->v(5),2|->v(6),3|->v(7),4|->v(0),5|->v(1),6|->v(2),7|->v(3)}))) & rotateleft : WORD8 --> WORD8*BOOL & !(w,v).(w : WORD8 & v : BV8 => (v = WORD8_TO_BV8(w) => rotateleft(w) = BV8_TO_WORD8({0|->v(7),1|->v(0),2|->v(1),3|->v(2),4|->v(3),5|->v(4),6|->v(5),7|->v(6)}),bool(v(7) = 1))) & rotateright : WORD8 --> WORD8*BOOL & !(w,v).(w : WORD8 & v : BV8 => (v = WORD8_TO_BV8(w) => rotateright(w) = BV8_TO_WORD8({0|->v(1),1|->v(2),2|->v(3),3|->v(4),4|->v(5),5|->v(6),6|->v(7),7|->v(0)}),bool(v(0) = 1))))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(ALU),Machine(TYPES))==(?);
  Seen_Context_List_Enumerated(Machine(ALU))==(?);
  Seen_Context_List_Invariant(Machine(ALU))==(btrue);
  Seen_Context_List_Assertions(Machine(ALU))==(btrue);
  Seen_Context_List_Properties(Machine(ALU))==(btrue);
  Seen_List_Constraints(Machine(ALU))==(btrue);
  Seen_List_Operations(Machine(ALU),Machine(TYPES))==(?);
  Seen_Expanded_List_Invariant(Machine(ALU),Machine(TYPES))==(btrue)
END
&
THEORY ListOfIdsX IS
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
  List_Of_Ids_SeenBNU(Machine(TYPE_BV8)) == (? : ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(ALU)) == (Type(add16) == Cst(SetOf(btype(INTEGER,"[WORD16","]WORD16")*btype(INTEGER,"[WORD16","]WORD16")*(btype(INTEGER,"[WORD16","]WORD16")*btype(BOOL,0,1)*btype(BOOL,0,1))));Type(add) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8","]WORD8")*(btype(INTEGER,"[WORD8","]WORD8")*btype(BOOL,0,1)*btype(BOOL,0,1))));Type(substract) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8","]WORD8")*(btype(INTEGER,"[WORD8","]WORD8")*btype(BOOL,0,1)*btype(BOOL,0,1))));Type(and) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8","]WORD8")*(btype(INTEGER,"[WORD8","]WORD8")*btype(BOOL,0,1))));Type(ior) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8","]WORD8")*(btype(INTEGER,"[WORD8","]WORD8")*btype(BOOL,0,1))));Type(xor) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8","]WORD8")*(btype(INTEGER,"[WORD8","]WORD8")*btype(BOOL,0,1))));Type(bitclear) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8_POSITION","]WORD8_POSITION")*btype(INTEGER,"[WORD8","]WORD8")));Type(bitset) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8_POSITION","]WORD8_POSITION")*btype(INTEGER,"[WORD8","]WORD8")));Type(bitget) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8_POSITION","]WORD8_POSITION")*btype(INTEGER,"[BIT","]BIT")));Type(complement) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8","]WORD8")));Type(swap) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*btype(INTEGER,"[WORD8","]WORD8")));Type(rotateleft) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*(btype(INTEGER,"[WORD8","]WORD8")*btype(BOOL,0,1))));Type(rotateright) == Cst(SetOf(btype(INTEGER,"[WORD8","]WORD8")*(btype(INTEGER,"[WORD8","]WORD8")*btype(BOOL,0,1)))))
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
