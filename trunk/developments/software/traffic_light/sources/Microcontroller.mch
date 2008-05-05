MACHINE
  Microcontroller

CONCRETE_VARIABLES 
  memory_data,pc,end

INVARIANT
  memory_data  : (NATURAL --> NATURAL) &
  pc : NATURAL &
  end : NATURAL &
  pc <= end

INITIALISATION
  memory_data :: (NATURAL --> NATURAL) ||
  pc := 0 ||
  end := 0

OPERATIONS
  
  /* Operacoes de encapsulamento */

  init_data(address,value)= 
  PRE
   address : NATURAL & value : NATURAL & dom(memory_data<+{address|->value}) = NATURAL 
  THEN
    memory_data(address):=value
  END;
  init(pc_,end_)=
  PRE
  pc_ : NATURAL & end_ : NATURAL & pc_ <= end_
  THEN
  pc := pc_ || end := end_
  END;

  goto(value) = 
  PRE
  value : NATURAL & value <= end
  THEN
  pc := value
  END;

  isequal(address_1,value)=
  PRE
    address_1 : NATURAL & value : NATURAL & 
    (memory_data(address_1) = value => pc + 1 <= end) &
    (memory_data(address_1) /= value => pc + 2 <= end)
  THEN
   IF  memory_data(address_1) =  value 
    THEN  pc := pc +1
    ELSE  pc := pc +2
    END
  END;


  copy(address_s,address_t) =
   PRE
    address_s : NATURAL & address_t : NATURAL & (pc + 1) <= end & (pc+1) : NATURAL  
  THEN
    memory_data(address_t):= memory_data(address_s)|| pc:=pc+1
  END;

  set_data(address,value)=
  PRE
    address : NATURAL & value : NATURAL & pc +1 <= end
  THEN
    memory_data(address):=value || pc := pc +1 
  END;


  inc(address)=
  PRE
    address : NATURAL & (memory_data(address)+1 ) : NATURAL & (pc + 1) : NATURAL & pc + 1 <= end &
     dom(memory_data<+{address|->memory_data(address)+1}) = NATURAL 
  THEN
    memory_data(address):= memory_data(address) + 1 ||
    pc := pc +1
  END

END
