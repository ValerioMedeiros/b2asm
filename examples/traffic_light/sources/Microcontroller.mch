/*************************************************************************
Copyright (c) 2008 David Deharbe, Valerio Medeiros Jr

All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
    * Neither the name of the Universidade Federal do Rio Grande do Norte nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*************************************************************************/
MACHINE
  Microcontroller

VARIABLES
  memory_data,stack,w,pc,end

INVARIANT
  memory_data  : (NATURAL --> NATURAL) &
/*  stack  : ( 1..8 --> NATURAL ) & */
  stack  : ( 1..8 +-> NATURAL ) & 
  card(stack)<=8 &
  w : NATURAL &
  pc : NATURAL &
  end : NATURAL &
  pc <= end

INITIALISATION
  memory_data :: (NATURAL --> NATURAL) ||
/*  stack := {}||*/
  stack := {(1|->0),(2|->0),(3|->0),(4|->0),(5|->0),(6|->0),(7|->0),(8|->0)}||/**/
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

 /* Operacoes de manipulacao da pilha call_function */  
  push = 
  PRE
  card(stack) < 8 & stack<+{card(stack)+1|->pc} : 0..7 +-> NATURAL  /*stack <<| { card(stack) + 1  |-> pc } : 0..7 |-> NATURAL */
  & (pc + 1) <= end &  card(dom(stack)) < 8 
  THEN
  stack(card(stack)+1):= pc ||  pc := pc +1
  END
  ;

  pop_1 = 
  PRE
    stack(card(stack))<end 
  THEN
    pc := stack(card(stack))
  END
  ;
  pop_2 = 
  PRE
    card({card(stack)}<<|stack)<=8 &
    /*card( {max(dom(stack)) }<<|stack)<= 8   &*/ card(stack)>0 
  THEN
    stack :=   {card(stack)} <<| stack
  END
  ;

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
    address : NATURAL & (memory_data(address) = 0 => ((pc + 1) : NATURAL & (pc + 1)<= end ))&
    (memory_data(address) /= 0 => ((pc + 2) : NATURAL & (pc + 2)<= end ))
  THEN
    IF  memory_data(address) = 0 
    THEN  pc := pc +1
    ELSE  pc := pc +2
    END
  END;

 
  
  isequal(address_1,value)=
  PRE
    address_1 : NATURAL & value : NATURAL &   pc + 2 < end
  THEN
   IF  memory_data(address_1) =  value 
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
