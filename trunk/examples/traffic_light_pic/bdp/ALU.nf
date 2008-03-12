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
  Context_List_Assertions(Machine(ALU))==(2**8 = 256 & NB_WORDS = 256 & !n.(n : WORD => 0<=n) & !n.(n : WORD => n<=255) & WORD_POSITION = BV8_INDEX & BIT_FLIP(0) = 1 & BIT_FLIP(1) = 0 & BIT_AND(0,0) = 0 & BIT_AND(0,1) = 0 & BIT_AND(1,0) = 0 & BIT_AND(1,1) = 1 & BIT_IOR(0,0) = 0 & BIT_IOR(0,1) = 0 & BIT_IOR(1,0) = 0 & BIT_IOR(1,1) = 1 & BIT_XOR(0,0) = 0 & BIT_XOR(0,1) = 1 & BIT_XOR(1,0) = 1 & BIT_XOR(1,1) = 0);
  List_Assertions(Machine(ALU))==(dom(add) = WORD*WORD & ran(add) <: WORD*BOOL*BOOL & dom(substract) = WORD*WORD & ran(substract) <: WORD*BOOL*BOOL & dom(and) = WORD*WORD & ran(and) <: WORD*BOOL & dom(ior) = WORD*WORD & ran(ior) <: WORD*BOOL & dom(xor) = WORD*WORD & ran(xor) <: WORD*BOOL & dom(bitclear) = WORD*WORD_POSITION & ran(bitclear) <: WORD & dom(bitset) = WORD*WORD_POSITION & ran(bitset) <: WORD & dom(bitget) = WORD*WORD_POSITION & ran(bitget) <: BIT & dom(complement) = WORD & ran(complement) <: WORD & dom(swap) = WORD & ran(swap) <: WORD & ran(rotateleft) <: WORD*BOOL & dom(rotateleft) = WORD & dom(rotateright) = WORD & ran(rotateright) <: WORD*BOOL)
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
  List_Valuable_Constants(Machine(ALU))==(add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright);
  Inherited_List_Constants(Machine(ALU))==(?);
  List_Constants(Machine(ALU))==(add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright)
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
  Context_List_Properties(Machine(ALU))==(WORD_LENGTH : NATURAL & INST_SZ : NATURAL & NB_WORDS : NATURAL & NB_INSTRUCTIONS : NATURAL & WORD_LENGTH = 8 & NB_WORDS = 2**WORD_LENGTH & WORD = 0..NB_WORDS-1 & WORD_POSITION = 0..WORD_LENGTH-1 & INST_SZ = 8 & NB_INSTRUCTIONS = 256 & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = 0..INSTRUCTION_MAX & INSTRUCTION_NEXT : INSTRUCTION --> INSTRUCTION & !i.(i : INSTRUCTION => (i = 255 => INSTRUCTION_NEXT(i) = 0) & (i<255 => INSTRUCTION_NEXT(i) = i+1)) & BV_TO_WORD : BV8 --> WORD & WORD_TO_BV : WORD --> BV8 & !(w,v).(w : WORD & v : BV8 => v = WORD_TO_BV(w) <=> (w = 128*v(7)+64*v(6)+32*v(5)+16*v(4)+8*v(3)+4*v(2)+2*v(1)+v(0))) & BV_TO_WORD = WORD_TO_BV~ & !n.(n : NATURAL & n>0 => 2**n = 2*2**(n-1)) & REGISTER : POW(INTEGER) & REGISTER = 0..127 & BIT = 0..1 & BIT_FLIP : BIT --> BIT & !b.(b : BIT => BIT_FLIP(b) = 1-b) & BIT_AND : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_AND(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & BIT_IOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_IOR(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & BIT_XOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_XOR(b1,b2) = 1 <=> (b1 = 1 & b2 = 0 or (b1 = 0 & b2 = 1))) & BV8_INDEX = 0..7 & BV8 = BV8_INDEX --> BIT & BV8_SET_BIT : BV8*BV8_INDEX*BIT --> BV8 & !(v,i,j,b).(v : BV8 & i : BV8_INDEX & j : BV8_INDEX & b : BIT => (i/=j => BV8_SET_BIT(v,i,b)(j) = v(j))) & !(v,i,b).(v : BV8 & i : BV8_INDEX & b : BIT => BV8_SET_BIT(v,i,b)(i) = b) & BV8_COMPLEMENT : BV8 --> BV8 & !(v,i).(v : BV8 & i : BV8_INDEX => BV8_COMPLEMENT(v)(i) = BIT_FLIP(v(i))) & BV8_ALL_ZEROES : BV8 & !i.(i : BV8_INDEX => BV8_ALL_ZEROES(i) = 0) & BV8_AND : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_AND(v1,v2)(i) = BIT_AND(v1(i),v2(i))) & BV8_IOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_IOR(v1,v2)(i) = BIT_IOR(v1(i),v2(i))) & BV8_XOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_XOR(v1,v2)(i) = BIT_XOR(v1(i),v2(i))));
  Inherited_List_Properties(Machine(ALU))==(btrue);
  List_Properties(Machine(ALU))==(add : WORD*WORD --> WORD*BOOL*BOOL & !(w1,w2,sum).(w1 : WORD & w2 : WORD & sum : NATURAL & sum = w1+w2 => (sum<=255 => add(w1,w2) = sum,bool(sum = 0),FALSE) & (256<=sum => add(w1,w2) = sum-256,bool(sum = 256),TRUE)) & substract : WORD*WORD --> WORD*BOOL*BOOL & !(w1,w2,diff).(w1 : WORD & w2 : WORD & diff : INTEGER & diff = w1-w2 => (diff<0 => substract(w1,w2) = diff+256,FALSE,TRUE) & (diff>=0 => substract(w1,w2) = diff,bool(diff = 0),FALSE)) & and : WORD*WORD --> WORD*BOOL & !(w1,w2,w).(w1 : WORD & w2 : WORD & w : WORD & w = BV_TO_WORD(BV8_AND(WORD_TO_BV(w1),WORD_TO_BV(w2))) => and(w1,w2) = w,bool(w = 0)) & ior : WORD*WORD --> WORD*BOOL & !(w1,w2,w).(w1 : WORD & w2 : WORD & w : WORD & w = BV_TO_WORD(BV8_IOR(WORD_TO_BV(w1),WORD_TO_BV(w2))) => ior(w1,w2) = w,bool(w = 0)) & xor : WORD*WORD --> WORD*BOOL & !(w1,w2,w).(w1 : WORD & w2 : WORD & w : WORD & w = BV_TO_WORD(BV8_XOR(WORD_TO_BV(w1),WORD_TO_BV(w2))) => xor(w1,w2) = w,bool(w = 0)) & bitget : WORD*WORD_POSITION --> BIT & !(w,i).(w : WORD & i : WORD_POSITION => bitget(w,i) = WORD_TO_BV(w)(i)) & bitset : WORD*WORD_POSITION --> WORD & !(w,i).(w : WORD & i : WORD_POSITION => bitset(w,i) = BV_TO_WORD(BV8_SET_BIT(WORD_TO_BV(w),i,1))) & bitclear : WORD*WORD_POSITION --> WORD & !(w,i,b).(w : WORD & i : WORD_POSITION & b : BIT => bitclear(w,i) = BV_TO_WORD(BV8_SET_BIT(WORD_TO_BV(w),i,0))) & complement : WORD --> WORD & !w.(w : WORD => complement(w) = BV_TO_WORD(BV8_COMPLEMENT(WORD_TO_BV(w)))) & swap : WORD --> WORD & !(w,v).(w : WORD & v : BV8 => (v = WORD_TO_BV(w) => swap(w) = BV_TO_WORD({0|->v(4),1|->v(5),2|->v(6),3|->v(7),4|->v(0),5|->v(1),6|->v(2),7|->v(3)}))) & rotateleft : WORD --> WORD*BOOL & !(w,v).(w : WORD & v : BV8 => (v = WORD_TO_BV(w) => rotateleft(w) = BV_TO_WORD({0|->v(7),1|->v(0),2|->v(1),3|->v(2),4|->v(3),5|->v(4),6|->v(5),7|->v(6)}),bool(v(7) = 1))) & rotateright : WORD --> WORD*BOOL & !(w,v).(w : WORD & v : BV8 => (v = WORD_TO_BV(w) => rotateright(w) = BV_TO_WORD({0|->v(1),1|->v(2),2|->v(3),3|->v(4),4|->v(5),5|->v(6),6|->v(7),7|->v(0)}),bool(v(0) = 1))))
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
  List_Of_Ids(Machine(ALU)) == (add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright | ? | ? | ? | ? | ? | seen(Machine(TYPES)) | ? | ALU);
  List_Of_HiddenCst_Ids(Machine(ALU)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(ALU)) == (add,substract,and,ior,xor,bitclear,bitset,bitget,complement,swap,rotateleft,rotateright);
  List_Of_VisibleVar_Ids(Machine(ALU)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(ALU)) == (? : ?);
  List_Of_Ids(Machine(TYPES)) == (WORD_LENGTH,WORD,WORD_POSITION,NB_WORDS,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,BV_TO_WORD,WORD_TO_BV,REGISTER | BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | included(Machine(TYPE_BIT)),included(Machine(TYPE_BV8)) | ? | TYPES);
  List_Of_HiddenCst_Ids(Machine(TYPES)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (WORD_LENGTH,WORD,WORD_POSITION,NB_WORDS,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,BV_TO_WORD,WORD_TO_BV,REGISTER,BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR);
  List_Of_VisibleVar_Ids(Machine(TYPES)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPES)) == (seen(Machine(TYPE_BIT)) : (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | ? | ? | ? | ?));
  List_Of_Ids(Machine(TYPE_BV8)) == (BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR | ? | ? | ? | ? | ? | seen(Machine(TYPE_BIT)) | ? | TYPE_BV8);
  List_Of_HiddenCst_Ids(Machine(TYPE_BV8)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BV8)) == (BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR);
  List_Of_VisibleVar_Ids(Machine(TYPE_BV8)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BV8)) == (? : ?);
  List_Of_Ids(Machine(TYPE_BIT)) == (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | ? | ? | ? | TYPE_BIT);
  List_Of_HiddenCst_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPE_BIT)) == (BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR);
  List_Of_VisibleVar_Ids(Machine(TYPE_BIT)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(TYPE_BIT)) == (? : ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(ALU)) == (Type(add) == Cst(SetOf(btype(INTEGER,"[WORD","]WORD")*btype(INTEGER,"[WORD","]WORD")*(btype(INTEGER,"[WORD","]WORD")*btype(BOOL,0,1)*btype(BOOL,0,1))));Type(substract) == Cst(SetOf(btype(INTEGER,"[WORD","]WORD")*btype(INTEGER,"[WORD","]WORD")*(btype(INTEGER,"[WORD","]WORD")*btype(BOOL,0,1)*btype(BOOL,0,1))));Type(and) == Cst(SetOf(btype(INTEGER,"[WORD","]WORD")*btype(INTEGER,"[WORD","]WORD")*(btype(INTEGER,"[WORD","]WORD")*btype(BOOL,0,1))));Type(ior) == Cst(SetOf(btype(INTEGER,"[WORD","]WORD")*btype(INTEGER,"[WORD","]WORD")*(btype(INTEGER,"[WORD","]WORD")*btype(BOOL,0,1))));Type(xor) == Cst(SetOf(btype(INTEGER,"[WORD","]WORD")*btype(INTEGER,"[WORD","]WORD")*(btype(INTEGER,"[WORD","]WORD")*btype(BOOL,0,1))));Type(bitclear) == Cst(SetOf(btype(INTEGER,"[WORD","]WORD")*btype(INTEGER,"[WORD_POSITION","]WORD_POSITION")*btype(INTEGER,"[WORD","]WORD")));Type(bitset) == Cst(SetOf(btype(INTEGER,"[WORD","]WORD")*btype(INTEGER,"[WORD_POSITION","]WORD_POSITION")*btype(INTEGER,"[WORD","]WORD")));Type(bitget) == Cst(SetOf(btype(INTEGER,"[WORD","]WORD")*btype(INTEGER,"[WORD_POSITION","]WORD_POSITION")*btype(INTEGER,"[BIT","]BIT")));Type(complement) == Cst(SetOf(btype(INTEGER,"[WORD","]WORD")*btype(INTEGER,"[WORD","]WORD")));Type(swap) == Cst(SetOf(btype(INTEGER,"[WORD","]WORD")*btype(INTEGER,"[WORD","]WORD")));Type(rotateleft) == Cst(SetOf(btype(INTEGER,"[WORD","]WORD")*(btype(INTEGER,"[WORD","]WORD")*btype(BOOL,0,1))));Type(rotateright) == Cst(SetOf(btype(INTEGER,"[WORD","]WORD")*(btype(INTEGER,"[WORD","]WORD")*btype(BOOL,0,1)))))
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
