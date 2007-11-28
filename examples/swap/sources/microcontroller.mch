/*************************************************************************
Copyright (c) 2007, David Deharbe.

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:
    * Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following disclaimer
in the documentation and/or other materials provided with the
distribution.
    * Neither the name of the Universidade Federal do Rio Grande do
Norte nor the names of its contributors may be used to endorse or
promote products derived from this software without specific prior
written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*************************************************************************/

MACHINE
  microcontroller

VARIABLES
  mem, pc, end

INVARIANT
  mem  : (NATURAL --> NATURAL) &
  pc : NATURAL &
  end : NATURAL &
  pc <= end

INITIALISATION
  mem :: (NATURAL --> NATURAL) ||
  pc := 0 ||
  end := 0

OPERATIONS
  
  /* Operacoes de encapsulamento */

	/* USED */
  init_data(address,value)= 
  PRE
   address : NATURAL & value : NATURAL & dom(mem<+{address|->value}) = NATURAL 
  THEN
    mem(address):=value
  END;
  
	/* USED */
  init(pc_,end_)=
  PRE
  pc_ : NATURAL & end_ : NATURAL & pc_ <= end_
  THEN
  pc := pc_ || end := end_
  END;

	/* USED */
  res <-- get_data(address) =
  PRE
    address : NATURAL 
  THEN  res:= mem(address) 
  END;
  
	/* USED */
  set_end(value)=
  PRE
  value : NATURAL & pc <= value 
  THEN
  end:= value
  END;

	/* USED */
  nop = 
  PRE
    pc < end
  THEN
    pc := pc + 1
  END;
  
	/* USED */
  goto(value) = 
  PRE
  value : NATURAL & value <= end
  THEN
  pc := value
  END;

	/* USED */
  testgt(address1, address2) =
  PRE
    address1 : NATURAL & address2 : NATURAL &
    (mem(address1) > mem(address2) => ((pc + 1) : NATURAL & (pc + 1) <= end)) &
    (mem(address1) <= mem(address2) => ((pc + 2) : NATURAL & (pc + 2) <= end))
  THEN
    IF  mem(address1) > mem(address2)
    THEN  pc := pc +1
    ELSE  pc := pc +2
    END
  END;

	/* USED */
  testeq(address1, address2) =
  PRE
    address1 : NATURAL & address2 : NATURAL &
    (mem(address1) = mem(address2) => ((pc + 1) : NATURAL & (pc + 1) <= end)) &
    (mem(address1) /= mem(address2) => ((pc + 2) : NATURAL & (pc + 2) <= end))
  THEN
    IF  mem(address1) = mem(address2)
    THEN  pc := pc +1
    ELSE  pc := pc +2
    END
  END;

	/* USED */
  move(address_s,address_t) =
   PRE
    address_s : NATURAL & address_t : NATURAL & (pc + 1) <= end & (pc+1) : NATURAL  
  THEN
    mem(address_t):= mem(address_s)|| pc:=pc+1
  END;

	/* USED */
  set_data(address,value)=
  PRE
    address : NATURAL & value : NATURAL & pc + 1 : NATURAL & pc +1 <= end
  THEN
    mem(address):=value || pc := pc +1 
  END;


	/* USED */
  inc(address)=
  PRE
    address : NATURAL & (mem(address)+1 ) : NATURAL & (pc + 1) : NATURAL & pc + 1 <= end &
     dom(mem<+{address|->mem(address)+1}) = NATURAL 
  THEN
    mem(address):= mem(address) + 1 ||
    pc := pc +1
  END

END
