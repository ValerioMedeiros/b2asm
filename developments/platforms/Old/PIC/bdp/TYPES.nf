Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(TYPES))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(TYPES))==(Machine(TYPES));
  Level(Machine(TYPES))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(TYPES)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(TYPES))==(?)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(TYPES))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(TYPES))==(TYPE_BV8,TYPE_BIT);
  List_Includes(Machine(TYPES))==(TYPE_BIT,TYPE_BV8)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(TYPES))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(TYPES))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(TYPES))==(?);
  Context_List_Variables(Machine(TYPES))==(?);
  Abstract_List_Variables(Machine(TYPES))==(?);
  Local_List_Variables(Machine(TYPES))==(?);
  List_Variables(Machine(TYPES))==(?);
  External_List_Variables(Machine(TYPES))==(?)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(TYPES))==(?);
  Abstract_List_VisibleVariables(Machine(TYPES))==(?);
  External_List_VisibleVariables(Machine(TYPES))==(?);
  Expanded_List_VisibleVariables(Machine(TYPES))==(?);
  List_VisibleVariables(Machine(TYPES))==(?);
  Internal_List_VisibleVariables(Machine(TYPES))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(TYPES))==(btrue);
  Gluing_List_Invariant(Machine(TYPES))==(btrue);
  Abstract_List_Invariant(Machine(TYPES))==(btrue);
  Expanded_List_Invariant(Machine(TYPES))==(btrue);
  Context_List_Invariant(Machine(TYPES))==(btrue);
  List_Invariant(Machine(TYPES))==(btrue)
END
&
THEORY ListAssertionsX IS
  Abstract_List_Assertions(Machine(TYPES))==(btrue);
  Expanded_List_Assertions(Machine(TYPES))==(BIT_FLIP(0) = 1 & BIT_FLIP(1) = 0 & BIT_AND(0,0) = 0 & BIT_AND(0,1) = 0 & BIT_AND(1,0) = 0 & BIT_AND(1,1) = 1 & BIT_IOR(0,0) = 0 & BIT_IOR(0,1) = 0 & BIT_IOR(1,0) = 0 & BIT_IOR(1,1) = 1 & BIT_XOR(0,0) = 0 & BIT_XOR(0,1) = 1 & BIT_XOR(1,0) = 1 & BIT_XOR(1,1) = 0);
  Context_List_Assertions(Machine(TYPES))==(btrue);
  List_Assertions(Machine(TYPES))==(NB_WORDS = 256 & !n.(n : WORD => 0<=n) & !n.(n : WORD => n<=255) & WORD_POSITION = BV8_INDEX)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(TYPES))==(skip);
  Context_List_Initialisation(Machine(TYPES))==(skip);
  List_Initialisation(Machine(TYPES))==(skip)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(TYPES))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(TYPES),Machine(TYPE_BIT))==(?);
  List_Instanciated_Parameters(Machine(TYPES),Machine(TYPE_BV8))==(?)
END
&
THEORY ListConstraintsX IS
  List_Constraints(Machine(TYPES),Machine(TYPE_BV8))==(btrue);
  List_Context_Constraints(Machine(TYPES))==(btrue);
  List_Constraints(Machine(TYPES))==(btrue);
  List_Constraints(Machine(TYPES),Machine(TYPE_BIT))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(TYPES))==(?);
  List_Operations(Machine(TYPES))==(?)
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
  List_Valuable_Constants(Machine(TYPES))==(BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR,BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,WORD_LENGTH,WORD,WORD_POSITION,NB_WORDS,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,BV_TO_WORD,WORD_TO_BV);
  Inherited_List_Constants(Machine(TYPES))==(BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR,BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR);
  List_Constants(Machine(TYPES))==(WORD_LENGTH,WORD,WORD_POSITION,NB_WORDS,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,BV_TO_WORD,WORD_TO_BV)
END
&
THEORY ListSetsX IS
  Set_Definition(Machine(TYPES),REGISTER)==(?);
  Context_List_Enumerated(Machine(TYPES))==(?);
  Context_List_Defered(Machine(TYPES))==(?);
  Context_List_Sets(Machine(TYPES))==(?);
  List_Valuable_Sets(Machine(TYPES))==(REGISTER);
  Inherited_List_Enumerated(Machine(TYPES))==(?);
  Inherited_List_Defered(Machine(TYPES))==(?);
  Inherited_List_Sets(Machine(TYPES))==(?);
  List_Enumerated(Machine(TYPES))==(?);
  List_Defered(Machine(TYPES))==(REGISTER);
  List_Sets(Machine(TYPES))==(REGISTER)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(TYPES))==(?);
  Expanded_List_HiddenConstants(Machine(TYPES))==(?);
  List_HiddenConstants(Machine(TYPES))==(?);
  External_List_HiddenConstants(Machine(TYPES))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(TYPES))==(btrue);
  Context_List_Properties(Machine(TYPES))==(btrue);
  Inherited_List_Properties(Machine(TYPES))==(BIT = 0..1 & BIT_FLIP : BIT --> BIT & !b.(b : BIT => BIT_FLIP(b) = 1-b) & BIT_AND : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_AND(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & BIT_IOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_IOR(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & BIT_XOR : BIT*BIT --> BIT & !(b1,b2).(b1 : BIT & b2 : BIT => BIT_XOR(b1,b2) = 1 <=> (b1 = 1 & b2 = 0 or (b1 = 0 & b2 = 1))) & BV8_INDEX = 0..7 & BV8 = BV8_INDEX --> BIT & BV8_SET_BIT : BV8*BV8_INDEX*BIT --> BV8 & !(v,i,j,b).(v : BV8 & i : BV8_INDEX & j : BV8_INDEX & b : BIT => (i/=j => BV8_SET_BIT(v,i,b)(j) = v(j))) & !(v,i,b).(v : BV8 & i : BV8_INDEX & b : BIT => BV8_SET_BIT(v,i,b)(i) = b) & BV8_COMPLEMENT : BV8 --> BV8 & !(v,i).(v : BV8 & i : BV8_INDEX => BV8_COMPLEMENT(v)(i) = BIT_FLIP(v(i))) & BV8_ALL_ZEROES : BV8 & !i.(i : BV8_INDEX => BV8_ALL_ZEROES(i) = 0) & BV8_AND : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_AND(v1,v2)(i) = BIT_AND(v1(i),v2(i))) & BV8_IOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_IOR(v1,v2)(i) = BIT_IOR(v1(i),v2(i))) & BV8_XOR : BV8*BV8 --> BV8 & !(v1,v2,i).(v1 : BV8 & v2 : BV8 & i : BV8_INDEX => BV8_XOR(v1,v2)(i) = BIT_XOR(v1(i),v2(i))));
  List_Properties(Machine(TYPES))==(WORD_LENGTH : NATURAL & INST_SZ : NATURAL & NB_WORDS : NATURAL & NB_INSTRUCTIONS : NATURAL & WORD_LENGTH = 8 & NB_WORDS = 2**WORD_LENGTH & WORD = 0..NB_WORDS-1 & WORD_POSITION = 0..WORD_LENGTH-1 & NB_INSTRUCTIONS = 2**INST_SZ & INSTRUCTION_MAX = NB_INSTRUCTIONS-1 & INSTRUCTION = 0..INSTRUCTION_MAX & INSTRUCTION_NEXT : INSTRUCTION --> INSTRUCTION & INSTRUCTION_NEXT = {p,q | p : INSTRUCTION & q : INSTRUCTION & 0<=p & p<NB_INSTRUCTIONS-1 & q = p+1}\/{NB_INSTRUCTIONS-1|->0} & BV_TO_WORD : BV8 --> WORD & WORD_TO_BV : WORD --> BV8 & !(w,v).(w : WORD & v : BV8 => v = WORD_TO_BV(w) <=> (w = 128*v(7)+64*v(6)+32*v(5)+16*v(4)+8*v(3)+4*v(2)+2*v(1)+v(0))) & BV_TO_WORD = WORD_TO_BV~ & not(REGISTER = {}))
END
&
THEORY ListSeenInfoX END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(TYPES)) == (WORD_LENGTH,WORD,WORD_POSITION,NB_WORDS,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,BV_TO_WORD,WORD_TO_BV,REGISTER | BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR | ? | ? | ? | ? | included(Machine(TYPE_BIT)),included(Machine(TYPE_BV8)) | ? | TYPES);
  List_Of_HiddenCst_Ids(Machine(TYPES)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(TYPES)) == (WORD_LENGTH,WORD,WORD_POSITION,NB_WORDS,INST_SZ,INSTRUCTION,NB_INSTRUCTIONS,INSTRUCTION_MAX,INSTRUCTION_NEXT,BV_TO_WORD,WORD_TO_BV,BV8_INDEX,BV8,BV8_SET_BIT,BV8_COMPLEMENT,BV8_ALL_ZEROES,BV8_AND,BV8_IOR,BV8_XOR,BIT,BIT_FLIP,BIT_AND,BIT_IOR,BIT_XOR);
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
THEORY SetsEnvX IS
  Sets(Machine(TYPES)) == (Type(REGISTER) == Cst(SetOf(atype(REGISTER,"[REGISTER","]REGISTER"))))
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(TYPES)) == (Type(BIT_XOR) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT_IOR) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT_AND) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT_FLIP) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")*btype(INTEGER,"[BIT","]BIT")));Type(BIT) == Cst(SetOf(btype(INTEGER,"[BIT","]BIT")));Type(BV8_XOR) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_IOR) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_AND) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_ALL_ZEROES) == Cst(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT")));Type(BV8_COMPLEMENT) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_SET_BIT) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT")*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))));Type(BV8_INDEX) == Cst(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")));Type(WORD_LENGTH) == Cst(btype(INTEGER,?,?));Type(WORD) == Cst(SetOf(btype(INTEGER,"[WORD","]WORD")));Type(WORD_POSITION) == Cst(SetOf(btype(INTEGER,"[WORD_POSITION","]WORD_POSITION")));Type(NB_WORDS) == Cst(btype(INTEGER,?,?));Type(INST_SZ) == Cst(btype(INTEGER,?,?));Type(INSTRUCTION) == Cst(SetOf(btype(INTEGER,"[INSTRUCTION","]INSTRUCTION")));Type(NB_INSTRUCTIONS) == Cst(btype(INTEGER,?,?));Type(INSTRUCTION_MAX) == Cst(btype(INTEGER,?,?));Type(INSTRUCTION_NEXT) == Cst(SetOf(btype(INTEGER,"[INSTRUCTION","]INSTRUCTION")*btype(INTEGER,"[INSTRUCTION","]INSTRUCTION")));Type(BV_TO_WORD) == Cst(SetOf(SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT"))*btype(INTEGER,"[WORD","]WORD")));Type(WORD_TO_BV) == Cst(SetOf(btype(INTEGER,"[WORD","]WORD")*SetOf(btype(INTEGER,"[BV8_INDEX","]BV8_INDEX")*btype(INTEGER,"[BIT","]BIT")))))
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
