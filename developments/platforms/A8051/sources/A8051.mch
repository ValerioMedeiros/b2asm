MODEL
   A8051


SEES
TYPES,ALU,MEMORY


CONCRETE_VARIABLES
stack,
mem,
pc,
sp



INVARIANT
mem : MEM_ADDR -->> WORD &
pc : INSTRUCTION & 
sp : NATURAL &
stack : NATURAL +->> INSTRUCTION &
dom(stack)=0..(sp-1)&
bitget(P)=par(mem(A))

INITIALISATION
sp :=0||
stack := {}|| mem :: MEM_ADDR-->> WORD ||pc:=0

OPERATIONS 

CJNE(addr,data,jump)=

	PRE 
	  addr : MEM_ADDR  & 
	  data : WORD & 
	  jump : INSTRUCTION

	THEN 
	  IF (not(mem(addr)=data))THEN
            BEGIN
	      IF(mem(addr)< data)THEN
	    	mem(psw):=bitset(mem(psw),7)
	      ELSE 
	    	mem(psw):=bitclear(mem(psw),7)
	      END 
	    END || pc := INSTRUCTION_IJUMP(pc,jump)			
	  ELSE	
	    pc:= INSTRUCTION_NEXT(pc)
	  END 
	END
;

CJNEI(Rn,data,jump)=
	PRE 
	  Rn : RAM_ADDR &	
	  (Rn=R1 or  Rn=R2)  &
	  data : WORD &
	  jump : INSTRUCTION &
	  mem(Rn) : RAM_ADDR

	THEN 
	   ANY addr WHERE addr : RAM_ADDR & addr = mem(Rn)
	     THEN
	  	IF (not(mem(addr)=data))THEN
                  BEGIN
	            IF(mem(addr)< data)THEN
	    	      mem(psw):=bitset(mem(psw),7)
	            ELSE 
	    	      mem(psw):=bitclear(mem(psw),7)
	            END 
	          END || pc := INSTRUCTION_IJUMP(pc,jump)			
	       ELSE	
	         pc:= INSTRUCTION_NEXT(pc)
	       END 
	     END
       END
;

/*-->-->-->-->-->-Data Transfer-->-->-->-->-->-->-->-->--> */

MOV(addr,data)=
	PRE 
	  addr : MEM_ADDR &
	  data : WORD 	
	THEN
	  mem(addr) := data ||
          pc:= INSTRUCTION_NEXT(pc)
	END
;


MOVI(Rn,data)=
	PRE 
	   Rn : 	RAM_ADDR &
	   (Rn=R1 or Rn=R2) & data : WORD &
		mem(Rn): RAM_ADDR
	THEN
	   ANY addr 

	      WHERE	
	        addr : RAM_ADDR & addr = mem(Rn)
	      THEN
	        mem(addr):=data||
		 pc:=INSTRUCTION_NEXT(pc)
	      END
	END
;


/*-->-->-->-->-->-->END-->-->-->-->-->-->-->-->-->-->-->-->-->--> */



ANL(addr)=
	PRE 
	  addr : RAM_ADDR
	THEN
          ANY result,b
           WHERE 
		result : WORD & b : BOOL &
                result,b= and(A,addr)  
           THEN
		
		mem(A):=result

	   END || pc:=INSTRUCTION_NEXT(pc)  
	END
;


ACALL(inst)=
	PRE inst : INSTRUCTION 

	THEN
		sp:=sp+1||
		stack(sp):= INSTRUCTION_NEXT(pc)||
		pc:=inst
	END
;


SJMP(jump)=
	PRE 
	  jump : INSTRUCTION
	THEN
	  pc:=INSTRUCTION_IJUMP(pc,jump)
	END
;

/*-->-->-->-->-->Boolean variable manipulation-->-->-->-->-*/
JC(jump)=	
	PRE
	  jump : INSTRUCTION
	THEN
	  IF bitget(mem(psw),7)=1 THEN pc := INSTRUCTION_IJUMP(pc,jump)
	  ELSE pc:=INSTRUCTION_NEXT(pc) END 
 	END
;
JNB(bit,jump)=
	PRE 
	  bit : BIT &
	  jump : INSTRUCTION
	THEN 
	  IF bit = 0 THEN  pc := INSTRUCTION_IJUMP(pc,jump)
	  ELSE pc:= INSTRUCTION_NEXT(pc) END	
	  
	END
;

CLR(b)=

	PRE
	  b : BIT_ADDRESS
	THEN
	  ANY w,p WHERE	
	    w : WORD & p : WORD_POSITION &  w,p= b
	  THEN
	   mem(w) := bitset(mem(w), p)
	  END||pc:= INSTRUCTION_NEXT(pc)	
	END
;

SETB(b)=
	PRE
	  b : BIT_ADDRESS
	THEN

	  ANY w,p WHERE	
	    w : WORD & p : WORD_POSITION &  w,p= b
	  THEN
	   mem(w) := bitclear(mem(w), p)
	  END
	  ||pc:= INSTRUCTION_NEXT(pc)	

	END
;

/*-->-->-->-->END-->-->-->-->-->-->-->-->--->-->-->-->-->-->-->-->-->-->-->-->-->-->-->-->*/

AJMP(jump)=
	PRE
	  jump : INSTRUCTION	
	THEN
		pc:=jump
	END
;

/* Falta o setar o bit de paridade */	
ADD(ac,src)=
	PRE
	  ac : SFR_ADDR& 
	 (ac=A or ac=ACC)&	
	  src : MEM_ADDR

	THEN
          ANY 
            result,par,c,ov
	  WHERE 
	    result : WORD &  par : BIT & c : BIT & ov : BIT &
	    result,par,c,ov = add(mem(A),mem(src))
	    
	  THEN 
   	    mem(A):=result
          END ||
          pc:= INSTRUCTION_NEXT(pc)
	END 
;

ADDD(ac,data)=
	PRE
	   ac : SFR_ADDR & 
	  (ac=A or ac=ACC)&	
	  data : WORD
	THEN
          ANY 
            result,par,c,ov
	  WHERE 
	    result : WORD &  par : BIT & c : BIT & ov : BIT &
	    result,par,c,ov = add(mem(A),data)
	    
	  THEN  
   	    mem(A):=result
          END ||
          pc:= INSTRUCTION_NEXT(pc)
	END 

;
SUBBS(ac,src)=
	PRE
	  ac : SFR_ADDR&
	  (ac = A or ac=ACC)  &	
	  src : MEM_ADDR
	THEN
          ANY 
            result,carry,zero 
	  WHERE 
	    result : WORD & result,carry,zero = substract(mem(A),mem(src)) &
	    carry : BOOL & zero : BOOL
	  THEN 
   	    mem(A):=result
	    	
          END ||
          pc:= INSTRUCTION_NEXT(pc)
	END 
;
SUBBD(acc,data)=
	PRE
	   acc : SFR_ADDR & 
	  (acc = A or acc=ACC)&	
	  data : WORD
	THEN
          ANY 
            result,carry,zero 
	  WHERE 
	    result : WORD & result,carry,zero = substract(mem(A),data) &
	    carry : BOOL & zero : BOOL
	  THEN 
   	    mem(A):=result
	    	
          END ||
          pc:= INSTRUCTION_NEXT(pc)
	END 
;
INC =
	BEGIN
	 ANY 
            result,par,c,ov
	  WHERE 
	    result : WORD &  par : BIT & c : BIT & ov : BIT &
	    result,par,c,ov = add(mem(A),1)
	    
	  THEN  
   	    mem(A):=result
 	   
          END ||
          pc:= INSTRUCTION_NEXT(pc)   
	END
;

DEC =
	BEGIN

	  ANY 
            result,carry,zero 
	  WHERE 
	    result : WORD & result,carry,zero = substract(A,1) &
	    carry : BOOL & zero : BOOL
	  THEN 
   	   mem(A):=result
          END ||
          pc:= INSTRUCTION_NEXT(pc)   
	END
;

INCS(src) =
	PRE
	  src : MEM_ADDR
	THEN
	  ANY 
            result,par,c,ov
	  WHERE 
	    result : WORD &  par : BIT & c : BIT & ov : BIT &
	    result,par,c,ov = add(mem(src),1)
	    
	  THEN  
   	    mem(src):=result
          END ||
          pc:= INSTRUCTION_NEXT(pc)
	END

END