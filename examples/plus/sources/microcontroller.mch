MACHINE
  microcontroller

VARIABLES
  memory_data,w, pc,end

INVARIANT
  memory_data  : (NATURAL --> NATURAL) &
  w : NATURAL &
  pc : NATURAL &
  end : NATURAL &
  pc <= end

INITIALISATION
  memory_data :: (NATURAL --> NATURAL) ||
  w  := 0 ||
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

  res <-- get_data(address) =
  PRE
    address : NATURAL 
  THEN  res:= memory_data(address) 
  END;
  
  res <--  get_pc =
  BEGIN
    res:= pc
  END;
  
  set_end(value)=
  PRE
  value : NATURAL & pc <= value 
  THEN
  end:= value
  END;

  res <-- get_end =
  BEGIN
    res := end
  END;
  
  nop = 
  PRE
    pc < end
  THEN
    pc := pc + 1
  END;
  
  set_w(value)=
  PRE
  value : NATURAL
  THEN 
  w := value
  END;

  res <-- get_w =
  BEGIN
  res := w
  END;
  

  goto(value) = 
  PRE
  value : NATURAL & value <= end
  THEN
  pc := value
  END;

  iszero(address) =
  PRE
    address : NATURAL & 
    (memory_data(address) = 0 => ((pc + 1) : NATURAL & (pc + 1)<= end ))&
    (memory_data(address) /= 0 => ((pc + 2) : NATURAL & (pc + 2)<= end ))
  THEN
    IF  memory_data(address) = 0 
    THEN  pc := pc +1
    ELSE  pc := pc +2
    END
  END;

  isnotzero_simple(address) =
  PRE
    address : NATURAL & (memory_data(address) = 0 => ((pc + 3) : NATURAL & (pc + 3)<= end ))&
    (memory_data(address) /= 0 => ((pc + 1) : NATURAL & (pc + 1)<= end ))
  THEN
    IF  memory_data(address) = 0 
    THEN  pc := pc +3
    ELSE  pc := pc +1
    END
  END;

  testeq(address1, address2) =
  PRE
    address1 : NATURAL & address2 : NATURAL &
    (memory_data(address1) = memory_data(address2) => ((pc + 1) : NATURAL & (pc + 1) <= end)) &
    (memory_data(address1) /= memory_data(address2) => ((pc + 2) : NATURAL & (pc + 2) <= end))
  THEN
    IF  memory_data(address1) = memory_data(address2)
    THEN  pc := pc +1
    ELSE  pc := pc +2
    END
  END;

  testgt(address1, address2) =
  PRE
    address1 : NATURAL & address2 : NATURAL &
    (memory_data(address1) > memory_data(address2) => ((pc + 1) : NATURAL & (pc + 1) <= end)) &
    (memory_data(address1) <= memory_data(address2) => ((pc + 2) : NATURAL & (pc + 2) <= end))
  THEN
    IF  memory_data(address1) > memory_data(address2)
    THEN  pc := pc +1
    ELSE  pc := pc +2
    END
  END;

  move(address_s,address_t) =
   PRE
    address_s : NATURAL & address_t : NATURAL & (pc + 1) <= end & (pc+1) : NATURAL  
  THEN
    memory_data(address_t):= memory_data(address_s)|| pc:=pc+1
  END;

  move_w_m(address) =
   PRE
    address : NATURAL & (pc + 1) <= end  & (pc+1) : NATURAL 
  THEN
    memory_data(address):= w || pc:=pc+1
  END;

  move_m_w(address) =
   PRE
    address : NATURAL & (pc + 1) <= end & (pc+1) : NATURAL
  THEN
    w := memory_data(address) || pc:=pc+1
  END;
  
  reset(address) =
  PRE
    address : NATURAL & memory_data(address) : NATURAL  &
    (pc + 1) <= end & (pc+1) : NATURAL
  THEN
    memory_data(address):=0 || pc:= pc + 1 
  END;

  reset_w =
  BEGIN
    w := 0
  END;

  set_data(address,value)=
  PRE
    address : NATURAL & value : NATURAL & pc + 1 : NATURAL & pc +1 <= end
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
  END;


  dec(address)=
  PRE
    address : NATURAL & (memory_data(address)-1 ) : NATURAL & (pc + 1) : NATURAL & pc + 1 <= end &
     dom(memory_data<+{address|->memory_data(address)-1}) = NATURAL 
  THEN
    memory_data(address):= memory_data(address) - 1 ||
    pc := pc +1
  END;
    
  add(address1,address2)=
  PRE
    address1 : NATURAL & address2 : NATURAL &
    memory_data(address1)+ memory_data(address2) : NATURAL
  THEN
    w:= memory_data(address1)+memory_data(address2)
  
  END;

  sub(address1,address2)=
  PRE
    address1 : NATURAL & address2 : NATURAL &  
    memory_data(address1)-memory_data(address2) : NATURAL
  THEN
    w:= memory_data(address1)-memory_data(address2)
  END;

  mul(address1,address2)=
  PRE
    address1 : NATURAL & address2 : NATURAL &
    memory_data(address1)*memory_data(address2) : NATURAL
  THEN
    w:= memory_data(address1)*memory_data(address2)
  
  END;

  div(address1,address2)=
  PRE
    address1 : NATURAL & address2 : NATURAL &
    memory_data(address1)/memory_data(address2) : NATURAL
  THEN
    w:= memory_data(address1)/memory_data(address2)
  
  END
  

END
