Normalised(
THEORY MagicNumberX IS
  MagicNumber(Machine(DATA_MEMORY))==(3.5)
END
&
THEORY UpperLevelX IS
  First_Level(Machine(DATA_MEMORY))==(Machine(DATA_MEMORY));
  Level(Machine(DATA_MEMORY))==(0)
END
&
THEORY LoadedStructureX IS
  Machine(DATA_MEMORY)
END
&
THEORY ListSeesX IS
  List_Sees(Machine(DATA_MEMORY))==(BIT_VECTOR_DEFINITION,BIT_VECTOR_ARITHMETICS,BYTE_DEFINITION,POWER2)
END
&
THEORY ListUsesX IS
  List_Uses(Machine(DATA_MEMORY))==(?)
END
&
THEORY ListIncludesX IS
  Inherited_List_Includes(Machine(DATA_MEMORY))==(?);
  List_Includes(Machine(DATA_MEMORY))==(?)
END
&
THEORY ListPromotesX IS
  List_Promotes(Machine(DATA_MEMORY))==(?)
END
&
THEORY ListExtendsX IS
  List_Extends(Machine(DATA_MEMORY))==(?)
END
&
THEORY ListVariablesX IS
  External_Context_List_Variables(Machine(DATA_MEMORY))==(?);
  Context_List_Variables(Machine(DATA_MEMORY))==(?);
  Abstract_List_Variables(Machine(DATA_MEMORY))==(?);
  Local_List_Variables(Machine(DATA_MEMORY))==(file);
  List_Variables(Machine(DATA_MEMORY))==(file);
  External_List_Variables(Machine(DATA_MEMORY))==(file)
END
&
THEORY ListVisibleVariablesX IS
  Inherited_List_VisibleVariables(Machine(DATA_MEMORY))==(?);
  Abstract_List_VisibleVariables(Machine(DATA_MEMORY))==(?);
  External_List_VisibleVariables(Machine(DATA_MEMORY))==(?);
  Expanded_List_VisibleVariables(Machine(DATA_MEMORY))==(?);
  List_VisibleVariables(Machine(DATA_MEMORY))==(?);
  Internal_List_VisibleVariables(Machine(DATA_MEMORY))==(?)
END
&
THEORY ListInvariantX IS
  Gluing_Seen_List_Invariant(Machine(DATA_MEMORY))==(btrue);
  Gluing_List_Invariant(Machine(DATA_MEMORY))==(btrue);
  Expanded_List_Invariant(Machine(DATA_MEMORY))==(btrue);
  Abstract_List_Invariant(Machine(DATA_MEMORY))==(btrue);
  Context_List_Invariant(Machine(DATA_MEMORY))==(btrue);
  List_Invariant(Machine(DATA_MEMORY))==(file: FILE & !ad.(ad: UNIMPLEMENTED_LOCATIONS => file(ad) = BYTE_ZERO) & !reg.(reg: MAPPED_REGISTERS => file(reg) = file(reg+BANK_SIZE)) & file(STATUS_ADDR)(IRP_POS) = 0 & file(STATUS_ADDR)(RP1_POS) = 0)
END
&
THEORY ListAssertionsX IS
  Expanded_List_Assertions(Machine(DATA_MEMORY))==(btrue);
  Abstract_List_Assertions(Machine(DATA_MEMORY))==(btrue);
  Context_List_Assertions(Machine(DATA_MEMORY))==((!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!bv.(bv: BIT_VECTOR => bv_not(bv_not(bv)) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: NATURAL & high: NATURAL & low<=high => bv_size(bv_sub(bv,low,high)) = 1+high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_and(v1,v2) = bv_and(v2,v1));!(v1,v2,v3).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) => bv_and(v1,bv_and(v2,v3)) = bv_and(bv_and(v1,v2),v3));!bv.(bv: BIT_VECTOR => bv_and(bv,bv_zero(bv_size(bv))) = bv_zero(bv_size(bv)));!bv.(bv: BIT_VECTOR => bv_and(bv,bv_one(bv_size(bv))) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_or(v1,v2) = bv_or(v2,v1));!(v1,v2,v3).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) => bv_or(v1,bv_or(v2,v3)) = bv_or(bv_or(v1,v2),v3));!bv.(bv: BIT_VECTOR => bv_or(bv,bv_one(bv_size(bv))) = bv_one(bv_size(bv)));!bv.(bv: BIT_VECTOR => bv_or(bv,bv_zero(bv_size(bv))) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_xor(v1,v2) = bv_xor(v2,v1));!bv.(bv: BIT_VECTOR => bv_xor(bv,bv) = bv_zero(bv_size(bv)))) & !ss.(ss: NATURAL1 => bv_to_nat(bv_zero(ss)) = 0) & (2**0 = 1;2**1 = 2;2**2 = 4;2**3 = 8;2**4 = 16;2**5 = 32;2**6 = 64;2**7 = 128;2**8 = 256;2**9 = 512;2**10 = 1024;2**11 = 2048;2**12 = 4096;2**13 = 8192;2**14 = 16384;2**15 = 32768;2**16 = 65536));
  List_Assertions(Machine(DATA_MEMORY))==(BANK_SIZE = 128;register_addresses(INDF_ADDR) = {0,128};register_addresses(STATUS_ADDR) = {3,131};register_addresses(FSR_ADDR) = {4,132};register_addresses(PCLATH_ADDR) = {10,138};register_addresses(INTCON_ADDR) = {11,139};dom(file) = ADDRESS;ran(file) <: BYTE)
END
&
THEORY ListInitialisationX IS
  Expanded_List_Initialisation(Machine(DATA_MEMORY))==(@(random,init_status).(random: FILE & init_status: BYTE & init_status(RP1_POS) = 0 ==> file:=random<+{INDF_ADDR|->BYTE_ZERO,STATUS_ADDR|->init_status,INDF_ADDR+128|->BYTE_ZERO,STATUS_ADDR+128|->init_status}<+UNIMPLEMENTED_LOCATIONS*{BYTE_ZERO}));
  Context_List_Initialisation(Machine(DATA_MEMORY))==(skip);
  List_Initialisation(Machine(DATA_MEMORY))==(ANY random,init_status WHERE random: FILE & init_status: BYTE & init_status(RP1_POS) = 0 THEN file:=random<+{INDF_ADDR|->BYTE_ZERO,STATUS_ADDR|->init_status,INDF_ADDR+128|->BYTE_ZERO,STATUS_ADDR+128|->init_status}<+UNIMPLEMENTED_LOCATIONS*{BYTE_ZERO} END)
END
&
THEORY ListParametersX IS
  List_Parameters(Machine(DATA_MEMORY))==(?)
END
&
THEORY ListInstanciatedParametersX IS
  List_Instanciated_Parameters(Machine(DATA_MEMORY),Machine(BIT_VECTOR_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(DATA_MEMORY),Machine(BIT_VECTOR_ARITHMETICS))==(?);
  List_Instanciated_Parameters(Machine(DATA_MEMORY),Machine(BYTE_DEFINITION))==(?);
  List_Instanciated_Parameters(Machine(DATA_MEMORY),Machine(POWER2))==(?)
END
&
THEORY ListConstraintsX IS
  List_Context_Constraints(Machine(DATA_MEMORY))==(btrue);
  List_Constraints(Machine(DATA_MEMORY))==(btrue)
END
&
THEORY ListOperationsX IS
  Internal_List_Operations(Machine(DATA_MEMORY))==(?);
  List_Operations(Machine(DATA_MEMORY))==(?)
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
  List_Valuable_Constants(Machine(DATA_MEMORY))==(BANK_WIDTH,BANK,BANK_SIZE,LOCATION_WIDTH,LOCATION,ADDRESS_WIDTH,ADDRESS,FILE,make_address,INDF_ADDR,TMR0_ADDR,PCL_ADDR,STATUS_ADDR,FSR_ADDR,PORTA_ADDR,PORTB_ADDR,PCLATH_ADDR,INTCON_ADDR,PIR1_ADDR,CMCON_ADDR,TRISA_ADDR,TRISB_ADDR,PIE1_ADDR,PCON_ADDR,LININTF_ADDR,VRCON_ADDR,UNIMPLEMENTED_LOCATIONS,MAPPED_REGISTERS,register_addresses,IRP_POS,RP1_POS,RP0_POS,actual_address);
  Inherited_List_Constants(Machine(DATA_MEMORY))==(?);
  List_Constants(Machine(DATA_MEMORY))==(BANK_WIDTH,BANK,BANK_SIZE,LOCATION_WIDTH,LOCATION,ADDRESS_WIDTH,ADDRESS,FILE,make_address,INDF_ADDR,TMR0_ADDR,PCL_ADDR,STATUS_ADDR,FSR_ADDR,PORTA_ADDR,PORTB_ADDR,PCLATH_ADDR,INTCON_ADDR,PIR1_ADDR,CMCON_ADDR,TRISA_ADDR,TRISB_ADDR,PIE1_ADDR,PCON_ADDR,LININTF_ADDR,VRCON_ADDR,UNIMPLEMENTED_LOCATIONS,MAPPED_REGISTERS,register_addresses,IRP_POS,RP1_POS,RP0_POS,actual_address)
END
&
THEORY ListSetsX IS
  Context_List_Enumerated(Machine(DATA_MEMORY))==(?);
  Context_List_Defered(Machine(DATA_MEMORY))==(?);
  Context_List_Sets(Machine(DATA_MEMORY))==(?);
  List_Valuable_Sets(Machine(DATA_MEMORY))==(?);
  Inherited_List_Enumerated(Machine(DATA_MEMORY))==(?);
  Inherited_List_Defered(Machine(DATA_MEMORY))==(?);
  Inherited_List_Sets(Machine(DATA_MEMORY))==(?);
  List_Enumerated(Machine(DATA_MEMORY))==(?);
  List_Defered(Machine(DATA_MEMORY))==(?);
  List_Sets(Machine(DATA_MEMORY))==(?)
END
&
THEORY ListHiddenConstantsX IS
  Abstract_List_HiddenConstants(Machine(DATA_MEMORY))==(?);
  Expanded_List_HiddenConstants(Machine(DATA_MEMORY))==(?);
  List_HiddenConstants(Machine(DATA_MEMORY))==(?);
  External_List_HiddenConstants(Machine(DATA_MEMORY))==(?)
END
&
THEORY ListPropertiesX IS
  Abstract_List_Properties(Machine(DATA_MEMORY))==(btrue);
  Context_List_Properties(Machine(DATA_MEMORY))==(BIT_VECTOR = seq1(BIT) & bv_size: BIT_VECTOR --> NATURAL1 & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v2^v1) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: NATURAL & high: NATURAL & low<=high | 0..high-low<|bv) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (1..sz)*{0}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (1..sz)*{1}) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 1..size(v1) | bit_not(v1(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 1..size(v1) | bit_and(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 1..size(v1) | bit_or(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 1..size(v1) | bit_xor(v1(idx),v2(idx)))) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1(idx+1)) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1<+{idx+1|->1}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1<+{idx+1|->0}) & bv_to_nat: BIT_VECTOR --> NATURAL & bv_to_nat = %bv.(bv: BIT_VECTOR | SIGMA(idx).(idx: dom(bv) | 2**idx*bv(idx))) & BYTE_WIDTH = 8 & BYTE_INDEX = 0..BYTE_WIDTH-1 & BYTE <: BIT_VECTOR & BYTE = {vv | vv: BIT_VECTOR & bv_size(vv) = BYTE_WIDTH} & BYTE_ZERO: BYTE & BYTE_ZERO = BYTE_INDEX*{0});
  Inherited_List_Properties(Machine(DATA_MEMORY))==(btrue);
  List_Properties(Machine(DATA_MEMORY))==(BANK_WIDTH = 2 & BANK <: BIT_VECTOR & BANK = {vv | vv: BIT_VECTOR & bv_size(vv) = BANK_WIDTH} & LOCATION_WIDTH = 7 & LOCATION <: BIT_VECTOR & LOCATION = {vv | vv: BIT_VECTOR & bv_size(vv) = LOCATION_WIDTH} & BANK_SIZE = 2**LOCATION_WIDTH & ADDRESS_WIDTH = BANK_WIDTH+LOCATION_WIDTH & ADDRESS = 0..2**ADDRESS_WIDTH-1 & make_address: BANK*LOCATION --> ADDRESS & make_address = %(vb,vl).(vb: BANK & vl: LOCATION | bv_to_nat(bv_catenate(vb,vl))) & FILE = ADDRESS --> BYTE & INDF_ADDR: ADDRESS & INDF_ADDR = 0 & TMR0_ADDR: ADDRESS & TMR0_ADDR = 1 & PCL_ADDR: ADDRESS & PCL_ADDR = 2 & STATUS_ADDR: ADDRESS & STATUS_ADDR = 3 & FSR_ADDR: ADDRESS & FSR_ADDR = 4 & PORTA_ADDR: ADDRESS & PORTA_ADDR = 5 & PORTB_ADDR: ADDRESS & PORTB_ADDR = 6 & PCLATH_ADDR: ADDRESS & PCLATH_ADDR = 10 & INTCON_ADDR: ADDRESS & INTCON_ADDR = 11 & PIR1_ADDR: ADDRESS & PIR1_ADDR = 12 & CMCON_ADDR: ADDRESS & CMCON_ADDR = 127 & TRISA_ADDR: ADDRESS & TRISA_ADDR = 133 & TRISB_ADDR: ADDRESS & TRISB_ADDR = 134 & PIE1_ADDR: ADDRESS & PIE1_ADDR = 140 & PCON_ADDR: ADDRESS & PCON_ADDR = 142 & LININTF_ADDR: ADDRESS & LININTF_ADDR = 144 & VRCON_ADDR: ADDRESS & VRCON_ADDR = 159 & UNIMPLEMENTED_LOCATIONS = 7..9\/13..30\/135..137\/{141}\/{143}\/145..158\/192..240 & MAPPED_REGISTERS <: ADDRESS & MAPPED_REGISTERS = {INDF_ADDR,STATUS_ADDR,FSR_ADDR,PCLATH_ADDR,INTCON_ADDR} & register_addresses: ADDRESS --> POW1(ADDRESS) & !ad.(ad: ADDRESS => (ad/:MAPPED_REGISTERS => register_addresses(ad) = {ad}) & (ad: MAPPED_REGISTERS => register_addresses(ad) = {ad,ad+BANK_SIZE})) & IRP_POS: BYTE_INDEX & IRP_POS = 7 & RP1_POS: BYTE_INDEX & RP1_POS = 6 & RP0_POS: BYTE_INDEX & RP0_POS = 5 & actual_address: ADDRESS*FILE --> ADDRESS & actual_address = %(ad,fi).(ad: ADDRESS & fi: FILE & ad: register_addresses(INDF_ADDR) | fi(STATUS_ADDR)(RP0_POS)*2**7+fi(STATUS_ADDR)(RP1_POS)*2**8+bv_to_nat(fi(FSR_ADDR)))\/%(ad,fi).(ad: ADDRESS & fi: FILE & ad/:register_addresses(INDF_ADDR) | fi(STATUS_ADDR)(IRP_POS)*2**8+bv_to_nat(fi(FSR_ADDR))))
END
&
THEORY ListSeenInfoX IS
  Seen_Internal_List_Operations(Machine(DATA_MEMORY),Machine(POWER2))==(?);
  Seen_Context_List_Enumerated(Machine(DATA_MEMORY))==(?);
  Seen_Context_List_Invariant(Machine(DATA_MEMORY))==(btrue);
  Seen_Context_List_Assertions(Machine(DATA_MEMORY))==((bit_not(0) = 1;bit_not(1) = 0;!bb.(bb: BIT => bit_not(bit_not(bb)) = bb);bit_and(0,0) = 0;bit_and(0,1) = 0;bit_and(1,0) = 0;bit_and(1,1) = 1;!(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = bit_and(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_and(b1,bit_and(b2,b3)) = bit_and(bit_and(b1,b2),b3));!b1.(b1: BIT => bit_and(b1,1) = b1);!b1.(b1: BIT => bit_and(b1,0) = 0);bit_or(0,0) = 0;bit_or(0,1) = 0;bit_or(1,0) = 0;bit_or(1,1) = 1;!(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = bit_or(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_or(b1,bit_or(b2,b3)) = bit_or(bit_or(b1,b2),b3));!b1.(b1: BIT => bit_or(b1,1) = 1);!b1.(b1: BIT => bit_or(b1,0) = b1);bit_xor(0,0) = 0;bit_xor(0,1) = 1;bit_xor(1,0) = 1;bit_xor(1,1) = 0;!(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = bit_xor(b2,b1));!(b1,b2,b3).(b1: BIT & b2: BIT & b3: BIT => bit_xor(b1,bit_xor(b2,b3)) = bit_xor(bit_xor(b1,b2),b3));!bb.(bb: BIT => bit_xor(bb,bb) = 0);bool_to_bit(TRUE) = 1;bool_to_bit(FALSE) = 0) & (!bv.(bv: BIT_VECTOR => bv_size(bv_not(bv)) = bv_size(bv));!bv.(bv: BIT_VECTOR => bv_not(bv_not(bv)) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR => bv_size(bv_catenate(v1,v2)) = bv_size(v1)+bv_size(v2));!(bv,low,high).(bv: BIT_VECTOR & low: NATURAL & high: NATURAL & low<=high => bv_size(bv_sub(bv,low,high)) = 1+high-low);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_and(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_and(v1,v2) = bv_and(v2,v1));!(v1,v2,v3).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) => bv_and(v1,bv_and(v2,v3)) = bv_and(bv_and(v1,v2),v3));!bv.(bv: BIT_VECTOR => bv_and(bv,bv_zero(bv_size(bv))) = bv_zero(bv_size(bv)));!bv.(bv: BIT_VECTOR => bv_and(bv,bv_one(bv_size(bv))) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_or(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_or(v1,v2) = bv_or(v2,v1));!(v1,v2,v3).(v1: BIT_VECTOR & v2: BIT_VECTOR & v3: BIT_VECTOR & bv_size(v1) = bv_size(v2) & bv_size(v1) = bv_size(v3) => bv_or(v1,bv_or(v2,v3)) = bv_or(bv_or(v1,v2),v3));!bv.(bv: BIT_VECTOR => bv_or(bv,bv_one(bv_size(bv))) = bv_one(bv_size(bv)));!bv.(bv: BIT_VECTOR => bv_or(bv,bv_zero(bv_size(bv))) = bv);!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v1));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_size(bv_xor(v1,v2)) = bv_size(v2));!(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & bv_size(v1) = bv_size(v2) => bv_xor(v1,v2) = bv_xor(v2,v1));!bv.(bv: BIT_VECTOR => bv_xor(bv,bv) = bv_zero(bv_size(bv)))) & !(nn,pp).(nn: INT & pp: NAT => (pp = 0 => nn**pp = 1) & (pp = 1 => nn**pp = nn) & (pp>1 => nn**pp = nn*nn**(pp-1))));
  Seen_Context_List_Properties(Machine(DATA_MEMORY))==(BIT = 0..1 & bit_not: BIT --> BIT & !bb.(bb: BIT => bit_not(bb) = 1-bb) & bit_and: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_and(b1,b2) = 1 <=> (b1 = 1) & b2 = 1) & bit_or: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_or(b1,b2) = 1 <=> (b1 = 1) or b2 = 1) & bit_xor: BIT*BIT --> BIT & !(b1,b2).(b1: BIT & b2: BIT => bit_xor(b1,b2) = 1 <=> b1/=b2) & bool_to_bit: BOOL --> BIT & bool_to_bit = {TRUE|->1,FALSE|->0} & bv_clear = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1<+{idx+1|->0}) & bv_clear: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_set = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1<+{idx+1|->1}) & bv_set: BIT_VECTOR*NATURAL --> BIT_VECTOR & bv_at = %(v1,idx).(v1: BIT_VECTOR & idx: NATURAL & idx<size(v1) | v1(idx+1)) & bv_at: BIT_VECTOR*NATURAL --> BIT & bv_xor = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 1..size(v1) | bit_xor(v1(idx),v2(idx)))) & bv_xor: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_or = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 1..size(v1) | bit_or(v1(idx),v2(idx)))) & bv_or: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_and = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR & size(v1) = size(v2) | %idx.(idx: 1..size(v1) | bit_and(v1(idx),v2(idx)))) & bv_and: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_not = %v1.(v1: BIT_VECTOR | %idx.(idx: 1..size(v1) | bit_not(v1(idx)))) & bv_not: BIT_VECTOR --> BIT_VECTOR & bv_one = %sz.(sz: NATURAL1 | (1..sz)*{1}) & bv_one: NATURAL1 --> BIT_VECTOR & bv_zero = %sz.(sz: NATURAL1 | (1..sz)*{0}) & bv_zero: NATURAL1 --> BIT_VECTOR & bv_sub = %(bv,low,high).(bv: BIT_VECTOR & low: NATURAL & high: NATURAL & low<=high | 0..high-low<|bv) & bv_sub: BIT_VECTOR*NATURAL*NATURAL --> BIT_VECTOR & bv_catenate = %(v1,v2).(v1: BIT_VECTOR & v2: BIT_VECTOR | v2^v1) & bv_catenate: BIT_VECTOR*BIT_VECTOR --> BIT_VECTOR & bv_size = %bv.(bv: BIT_VECTOR | size(bv)) & bv_size: BIT_VECTOR --> NATURAL1 & BIT_VECTOR = seq1(BIT));
  Seen_List_Constraints(Machine(DATA_MEMORY))==(btrue);
  Seen_List_Operations(Machine(DATA_MEMORY),Machine(POWER2))==(?);
  Seen_Expanded_List_Invariant(Machine(DATA_MEMORY),Machine(POWER2))==(btrue);
  Seen_Internal_List_Operations(Machine(DATA_MEMORY),Machine(BYTE_DEFINITION))==(?);
  Seen_List_Operations(Machine(DATA_MEMORY),Machine(BYTE_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(DATA_MEMORY),Machine(BYTE_DEFINITION))==(btrue);
  Seen_Internal_List_Operations(Machine(DATA_MEMORY),Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Seen_List_Operations(Machine(DATA_MEMORY),Machine(BIT_VECTOR_ARITHMETICS))==(?);
  Seen_Expanded_List_Invariant(Machine(DATA_MEMORY),Machine(BIT_VECTOR_ARITHMETICS))==(btrue);
  Seen_Internal_List_Operations(Machine(DATA_MEMORY),Machine(BIT_VECTOR_DEFINITION))==(?);
  Seen_List_Operations(Machine(DATA_MEMORY),Machine(BIT_VECTOR_DEFINITION))==(?);
  Seen_Expanded_List_Invariant(Machine(DATA_MEMORY),Machine(BIT_VECTOR_DEFINITION))==(btrue)
END
&
THEORY ListOfIdsX IS
  List_Of_Ids(Machine(DATA_MEMORY)) == (BANK_WIDTH,BANK,BANK_SIZE,LOCATION_WIDTH,LOCATION,ADDRESS_WIDTH,ADDRESS,FILE,make_address,INDF_ADDR,TMR0_ADDR,PCL_ADDR,STATUS_ADDR,FSR_ADDR,PORTA_ADDR,PORTB_ADDR,PCLATH_ADDR,INTCON_ADDR,PIR1_ADDR,CMCON_ADDR,TRISA_ADDR,TRISB_ADDR,PIE1_ADDR,PCON_ADDR,LININTF_ADDR,VRCON_ADDR,UNIMPLEMENTED_LOCATIONS,MAPPED_REGISTERS,register_addresses,IRP_POS,RP1_POS,RP0_POS,actual_address | ? | file | ? | ? | ? | seen(Machine(BIT_VECTOR_DEFINITION)),seen(Machine(BIT_VECTOR_ARITHMETICS)),seen(Machine(BYTE_DEFINITION)),seen(Machine(POWER2)) | ? | DATA_MEMORY);
  List_Of_HiddenCst_Ids(Machine(DATA_MEMORY)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(DATA_MEMORY)) == (BANK_WIDTH,BANK,BANK_SIZE,LOCATION_WIDTH,LOCATION,ADDRESS_WIDTH,ADDRESS,FILE,make_address,INDF_ADDR,TMR0_ADDR,PCL_ADDR,STATUS_ADDR,FSR_ADDR,PORTA_ADDR,PORTB_ADDR,PCLATH_ADDR,INTCON_ADDR,PIR1_ADDR,CMCON_ADDR,TRISA_ADDR,TRISB_ADDR,PIE1_ADDR,PCON_ADDR,LININTF_ADDR,VRCON_ADDR,UNIMPLEMENTED_LOCATIONS,MAPPED_REGISTERS,register_addresses,IRP_POS,RP1_POS,RP0_POS,actual_address);
  List_Of_VisibleVar_Ids(Machine(DATA_MEMORY)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(DATA_MEMORY)) == (?: ?);
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
  List_Of_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)) | ? | BYTE_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BYTE_DEFINITION)) == (BYTE,BYTE_WIDTH,BYTE_INDEX,BYTE_ZERO);
  List_Of_VisibleVar_Ids(Machine(BYTE_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BYTE_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_size,bv_catenate,bv_sub,bv_zero,bv_one,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)) | ? | BIT_VECTOR_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_DEFINITION)) == (BIT_VECTOR,bv_size,bv_catenate,bv_sub,bv_zero,bv_one,bv_not,bv_and,bv_or,bv_xor,bv_at,bv_set,bv_clear);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit | ? | ? | ? | ? | ? | ? | ? | BIT_DEFINITION);
  List_Of_HiddenCst_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_DEFINITION)) == (BIT,bit_not,bit_and,bit_or,bit_xor,bool_to_bit);
  List_Of_VisibleVar_Ids(Machine(BIT_DEFINITION)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_DEFINITION)) == (?: ?);
  List_Of_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (bv_to_nat | ? | ? | ? | ? | ? | seen(Machine(BIT_DEFINITION)),seen(Machine(BIT_VECTOR_DEFINITION)) | ? | BIT_VECTOR_ARITHMETICS);
  List_Of_HiddenCst_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (? | ?);
  List_Of_VisibleCst_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (bv_to_nat);
  List_Of_VisibleVar_Ids(Machine(BIT_VECTOR_ARITHMETICS)) == (? | ?);
  List_Of_Ids_SeenBNU(Machine(BIT_VECTOR_ARITHMETICS)) == (?: ?)
END
&
THEORY ConstantsEnvX IS
  Constants(Machine(DATA_MEMORY)) == (Type(BANK_WIDTH) == Cst(btype(INTEGER,?,?));Type(BANK) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(BANK_SIZE) == Cst(btype(INTEGER,?,?));Type(LOCATION_WIDTH) == Cst(btype(INTEGER,?,?));Type(LOCATION) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))));Type(ADDRESS_WIDTH) == Cst(btype(INTEGER,?,?));Type(ADDRESS) == Cst(SetOf(btype(INTEGER,"[ADDRESS","]ADDRESS")));Type(FILE) == Cst(SetOf(SetOf(btype(INTEGER,"[ADDRESS","]ADDRESS")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))));Type(make_address) == Cst(SetOf(SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?))*btype(INTEGER,"[ADDRESS","]ADDRESS")));Type(INDF_ADDR) == Cst(btype(INTEGER,?,?));Type(TMR0_ADDR) == Cst(btype(INTEGER,?,?));Type(PCL_ADDR) == Cst(btype(INTEGER,?,?));Type(STATUS_ADDR) == Cst(btype(INTEGER,?,?));Type(FSR_ADDR) == Cst(btype(INTEGER,?,?));Type(PORTA_ADDR) == Cst(btype(INTEGER,?,?));Type(PORTB_ADDR) == Cst(btype(INTEGER,?,?));Type(PCLATH_ADDR) == Cst(btype(INTEGER,?,?));Type(INTCON_ADDR) == Cst(btype(INTEGER,?,?));Type(PIR1_ADDR) == Cst(btype(INTEGER,?,?));Type(CMCON_ADDR) == Cst(btype(INTEGER,?,?));Type(TRISA_ADDR) == Cst(btype(INTEGER,?,?));Type(TRISB_ADDR) == Cst(btype(INTEGER,?,?));Type(PIE1_ADDR) == Cst(btype(INTEGER,?,?));Type(PCON_ADDR) == Cst(btype(INTEGER,?,?));Type(LININTF_ADDR) == Cst(btype(INTEGER,?,?));Type(VRCON_ADDR) == Cst(btype(INTEGER,?,?));Type(UNIMPLEMENTED_LOCATIONS) == Cst(SetOf(btype(INTEGER,?,?)));Type(MAPPED_REGISTERS) == Cst(SetOf(btype(INTEGER,"[MAPPED_REGISTERS","]MAPPED_REGISTERS")));Type(register_addresses) == Cst(SetOf(btype(INTEGER,"[ADDRESS","]ADDRESS")*SetOf(btype(INTEGER,"[ADDRESS","]ADDRESS"))));Type(IRP_POS) == Cst(btype(INTEGER,?,?));Type(RP1_POS) == Cst(btype(INTEGER,?,?));Type(RP0_POS) == Cst(btype(INTEGER,?,?));Type(actual_address) == Cst(SetOf(btype(INTEGER,"[ADDRESS","]ADDRESS")*SetOf(btype(INTEGER,"[ADDRESS","]ADDRESS")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))*btype(INTEGER,"[ADDRESS","]ADDRESS"))))
END
&
THEORY VariablesEnvX IS
  Variables(Machine(DATA_MEMORY)) == (Type(file) == Mvl(SetOf(btype(INTEGER,"[ADDRESS","]ADDRESS")*SetOf(btype(INTEGER,?,?)*btype(INTEGER,?,?)))))
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
