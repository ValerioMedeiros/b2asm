MACHINE
   A8051


SEES
ALU,TYPES,TYPE_BV8,BIT_DEFINITION

INCLUDES
MEMORY

CONCRETE_VARIABLES
pc


INVARIANT
pc : INSTRUCTION 





INITIALISATION
pc:=0


OPERATIONS 




INIT = 
  BEGIN pc :=0 END
;




/*CJNE A,#data,rel --- Direct Adrres */
/*CJNE Rn,#data,rel --- Direct Adrres */
CJNE(addr,data,jump)=

	PRE 
	  addr : MEM_ADDR  & 
	  data : UCHAR & 
	  jump : INSTRUCTION

	THEN 
	  IF (not(mem(addr)=uchar_byte(data)))THEN
            BEGIN
	      IF(byte_uchar(mem(addr))<data)THEN
	    	bitSet(CY)
	      ELSE 
	    	bitClear(CY)
	      END 
	    END ||pc:= INSTRUCTION_INCREMENT(pc,jump+1)
	  ELSE	
	   pc:= INSTRUCTION_INCREMENT(pc,1)
	  END 
	END
;

/*CJNE @Ri,<data>,rel ----- Indirect */
CJNEI(Rn,data,jump)=
	PRE 
	   Rn : RAM_ADDR &	
	  (Rn=R1 or  Rn=R2)&
	  data : UCHAR&
	  jump : INSTRUCTION &
	  byte_uchar(mem(Rn)) : RAM_ADDR

	THEN 
	   ANY addr WHERE addr : RAM_ADDR & addr = byte_uchar(mem(Rn))
	     THEN
	  	IF (not(mem(addr)=uchar_byte(data)))THEN
                  BEGIN
	            IF(byte_uchar(mem(addr))< data)THEN
	    	      bitSet(CY)
	            ELSE 
	    	      bitClear(CY)
	            END 
	          END || pc := INSTRUCTION_INCREMENT(pc,jump+1)			
	       ELSE	
	         pc:=INSTRUCTION_INCREMENT(pc,1)			
	       END 
	     END
       END


;




/*CJNE A,direct,rel ---- jump Direct */
CJNEM(dest,src,jump)=

	PRE 
	  dest : MEM_ADDR  &
	  dest = ACC & 
	  src : MEM_ADDR & 
	  jump : INSTRUCTION

	THEN 
	  IF (not(mem(dest)=mem(src)))THEN
            BEGIN
	      IF(byte_uchar(mem(dest))< byte_uchar(mem(src)))THEN
	    	bitSet(CY)
	      ELSE 
	    	bitClear(CY)
	      END 
	    END || pc := INSTRUCTION_INCREMENT(pc,jump+1)			
	  ELSE	
	    pc:= INSTRUCTION_INCREMENT(pc,1)
	  END 
	END
;

SJMP(jump)=
	PRE 
	  jump : INSTRUCTION
	THEN
	  pc:=INSTRUCTION_INCREMENT(pc,jump+1)			
	END
;

AJMP(jump)=
	PRE
	  jump : INSTRUCTION	
	THEN
	  pc:=jump
	END
;

MOV(dest,data)=
	PRE 
	  dest : MEM_ADDR & data:UCHAR	
	THEN
	  addrSetDirect(dest,uchar_byte(data)) || pc:= INSTRUCTION_INCREMENT(pc,1)
	END


;

MOVI(Rn,data)=
	PRE 
	    Rn : RAM_ADDR &(Rn=R1 or Rn=R2) & data : UCHAR & byte_uchar(mem(Rn)): RAM_ADDR
	THEN
	   addrSetIndirect(Rn,uchar_byte(data))||pc:= INSTRUCTION_INCREMENT(pc,1)
	END

;
MOVM(dest,src)=
	PRE
	  src : MEM_ADDR & dest : MEM_ADDR 
	THEN
	  addrSetDirect(dest,mem(src))|| pc := INSTRUCTION_INCREMENT(pc,1)
	END

;

MOVMI(dest,Rn)=
	PRE
	   dest : MEM_ADDR & Rn : RAM_ADDR & (Rn=R1 or Rn =R0) & byte_uchar(mem(Rn)) : RAM_ADDR
	THEN
	  ANY src
	  WHERE src : RAM_ADDR & src = byte_uchar(mem(Rn))
	  THEN
	    addrSetDirect(dest,mem(src))||pc := INSTRUCTION_INCREMENT(pc,1)
	  END
	END

;

MOVIM(Rn,src)=
	PRE
	  Rn : RAM_ADDR & src : MEM_ADDR 
	  & (Rn = R1 or Rn = R2)
	  & byte_uchar(mem(Rn)) : RAM_ADDR
	THEN
	   addrSetIndirect(Rn,mem(src))|| pc := INSTRUCTION_INCREMENT(pc,1)
	  
	END
;
/*
MOVB(dest,src)=
	PRE
	  dest : BIT_ADDRESS & src : BIT_ADDRESS & (dest=CY or src=CY)
	THEN
	    IF bitGet(src)=1 THEN bitSet(src)ELSE bitClear(src)END ||pc := INSTRUCTION_INCREMENT(pc,1)
	END
;
*/

ANL(addr,data)=
	PRE 
	  addr : MEM_ADDR &
	  data : UCHAR
	THEN
	  addrSetDirect(addr,and(mem(addr),uchar_byte(data)))|| pc:=INSTRUCTION_INCREMENT(pc,1)  
	END 

;

ANLI(Rn,data)=
	PRE 
	  Rn : RAM_ADDR
	  & (Rn=R1 or Rn=R2)
	  & byte_uchar(mem(Rn)):RAM_ADDR
	  & data:UCHAR

	THEN 
	  ANY addr WHERE addr : RAM_ADDR & addr= byte_uchar(mem(Rn)) THEN
	     addrSetIndirect(Rn,and(mem(addr),uchar_byte(data)))||pc:=INSTRUCTION_INCREMENT(pc,1)  
	  END 
       END
;
ANLM(dest,src)=
   	PRE
	  dest : MEM_ADDR
	& src : MEM_ADDR
	THEN 
	  addrSetDirect(dest,and(mem(dest),mem(src)))
	END
;

ANLMI(dest,Rn)=
   	PRE
	  dest : MEM_ADDR
	& Rn : RAM_ADDR
	& (Rn = R1 or Rn=R2)
	& byte_uchar(mem(Rn)) : RAM_ADDR
	THEN 
	  ANY src WHERE src : RAM_ADDR & src = byte_uchar(mem(Rn)) 
	  THEN
	      addrSetDirect(dest,and(mem(dest),mem(src)))      
	  END
	END
/*
???????
ANLB(ca,bit)=
	PRE 
	  bit : BIT_ADDRESS
	  & ca  : BIT_ADDRESS
	  & (ca = CC)
	THEN
	  bitSet(CY,and_bit(bitGet(CY),bitGet(bit)))
	END
;


ANLBN(ca,bit)=
	PRE 
	  bit : BIT_ADDRESS
	  & ca  : BIT_ADDRESS
	  &(ca = CY)
	THEN
	  bitSet(CY,and_bit(bitGet(CY),bitGet(bit)))
	END
*/

/*

NÃO ATUALIZADAS










ACALL(inst)=
	PRE 
	     inst : INSTRUCTION 
	   & mem(SP) : RAM_ADDR

	THEN
	   ANY
	     data_sp
	   WHERE
	     data_sp : RAM_ADDR & data_sp = incsp(mem(SP))
	   THEN 
		
	      mem:= mem <+ {data_sp|->INSTRUCTION_NEXT(pc),SP |-> data_sp}||
	      pc := inst 
	   END
	END
;

RET =
        PRE 
	  mem(SP): RAM_ADDR &
	  mem(mem(SP)) : INSTRUCTION
	THEN 
	   ANY 
	     addr
	   WHERE
	     addr : RAM_ADDR &  addr = mem(SP)
	   THEN
	     pc := mem(addr)||
	     mem(SP):=decsp(mem(SP))
	   END
	END
;

POP(direct) = 
      PRE
	direct : MEM_ADDR &
	mem(SP): RAM_ADDR
	
      THEN

	ANY
	 addr,data
	WHERE
	 addr : RAM_ADDR & addr = mem(SP)
	 & data : WORD & data = mem(addr)
	THEN
	 IF direct = ACC
	 THEN mem := mem <+ (changeacc(data,mem(PSW))\/ {SP |-> decsp(addr)})
	 ELSE mem := mem <+ {direct |-> mem(addr), SP|-> decsp(mem(SP))}
	 END
	END
	
      END
;

PUSH(direct)=
     PRE
	direct : MEM_ADDR &
	mem(SP): RAM_ADDR
     THEN	
	ANY
	  addr,data
	WHERE
	  addr : RAM_ADDR & addr = mem(SP) & data : WORD & data =mem(direct)
	THEN
	  mem:={addr|-> data,SP|->incsp(mem(SP))}
	END
     END	
;



JB(bit,jump)=
	PRE
	 bit : BIT_ADDRESS & jump : INSTRUCTION 
	THEN 
	 ANY
	  addr,ps
	 WHERE
	  addr : MEM_ADDR & ps : WORD_INDEX & addr,ps = bit
	 THEN
	  pc:= bitjump(bitget(mem(addr),ps),1,pc,jump)   
	 END
	END
;

JBC(bit,jump)=
	PRE
	  bit : BIT_ADDRESS & jump :  INSTRUCTION
	THEN
	  ANY
	    addr,ps,bt,wd
	  WHERE
	    addr : MEM_ADDR & ps : WORD_INDEX & bt : BIT & wd : WORD & addr,ps = bit 
	    &  bt= bitget(mem(addr),ps)
	    &  wd= bitclear(mem(addr),ps)
	
	  THEN 

	    IF addr = ACC
		
	    THEN
		pc := bitjump(bt,1,pc,jump)||
		mem := mem <+ changeacc(wd,mem(PSW))
	    ELSE
		pc := bitjump(bt,1,pc,jump)||
		mem(addr):=wd
	    END 
	    
	  END
	END
;

JC(jump)=	
	PRE
	  jump : INSTRUCTION
	THEN
	  pc:= bitjump(bitget(mem(PSW),7),1,jump,pc)
 	END
;

JNB(bit,jump)=
	PRE 
	  bit : BIT_ADDRESS &
	  jump : INSTRUCTION
	THEN 
	  	
	  ANY
	    addr,ps
	  WHERE
	    addr : MEM_ADDR & ps : WORD_INDEX & addr,ps=bit	    
	  THEN
	    pc:= bitjump(bitget(mem(addr),ps),0,jump,pc) 
	  END
	  
	END
;

JNC(bit,jump)=
	PRE
	     bit : BIT_ADDRESS
	  &  jump : INSTRUCTION
	THEN
	  ANY
	    addr,ps,bt
	  WHERE 
	    addr : MEM_ADDR & ps : WORD_INDEX & bt : BIT & addr,ps=bit &
	    bt = bitget(mem(addr),ps)
	  THEN
	    pc:= bitjump(bt,0,jump,pc)
	  END 
	END
;

JNZ(jump)=
	PRE
	  jump : INSTRUCTION
	THEN
	 IF mem(ACC)/=0 
	 THEN pc:= INSTRUCTION_IJUMP(pc,jump)
	 ELSE pc:=INSTRUCTION_NEXT(pc)
	 END
	END
;
JZ(jump)=
	PRE
	  jump : INSTRUCTION
	THEN
	 IF mem(ACC)= 0 
	 THEN pc:= INSTRUCTION_IJUMP(pc,jump)
	 ELSE pc:=INSTRUCTION_NEXT(pc)
	 END
	END
;

CLR(bt)=

	PRE
	  bt : BIT_ADDRESS
	THEN
	  ANY addr,ps,data WHERE	
	    addr : MEM_ADDR & data : WORD & ps : WORD_INDEX &  addr,ps= bt
	    & data = bitset(mem(addr),ps)
	  THEN
	   IF addr=ACC 
	   THEN mem:= mem <+ changeacc(data,mem(PSW))
	   ELSE mem(addr) := data
	   END
	   
	  END||pc:= INSTRUCTION_NEXT(pc)	
	END
;

CLRA(addr)=

	PRE
	  addr : SFR_ADDR
	  & addr = ACC
	THEN
	  mem:= mem <+ changeacc(0,mem(PSW))
	 ||pc:= INSTRUCTION_NEXT(pc)	
	END
;
CPL(ca) = 
	PRE
	 ca : BIT_ADDRESS 
	THEN 
	 ANY
	  addr,ps,result
	 WHERE
	   addr : MEM_ADDR & result : WORD & ps : WORD_INDEX & addr,ps = ca
	   & result = bitcomplement(mem(addr),ps)
	 THEN
	   IF addr = ACC 
	   THEN
	      mem := mem <+ changeacc(result,mem(PSW))
	     
	   ELSE
	      mem(addr):=result
	   END
	 END||pc:= INSTRUCTION_NEXT(pc)	
        END
;

CPLA(addr)=
	PRE 
	  addr : SFR_ADDR
	& addr = ACC
	THEN
	  ANY
	    result
	  WHERE
	    result : WORD & result = complement(mem(ACC))
	  THEN
	    mem:= mem <+ changeacc(result,mem(PSW))
	  END||pc:= INSTRUCTION_NEXT(pc)	
	END
;

SETB(bit)=
	PRE
	  bit : BIT_ADDRESS
	THEN

	 ANY addr,ps,data WHERE	
	    addr : MEM_ADDR & ps : WORD_INDEX &  addr,ps= bit
	    & data = bitclear(mem(addr),ps)
	  THEN
	   IF addr=ACC 
	   THEN mem:= mem <+ changeacc(data,mem(PSW))
	   ELSE mem(addr) := data
	   END
	   
	  END||pc:= INSTRUCTION_NEXT(pc)		

	END
;


ADD(ac,src)=
	PRE
	  ac : SFR_ADDR& 
	 (ac=AA or ac=ACC)&	
	  src : MEM_ADDR

	THEN
          ANY 
            result,psw_new
	  WHERE 
	    result : WORD & psw_new : WORD &
	    result,psw_new = ula_add(mem(ACC),mem(src),0,mem(PSW))
	    
	  THEN 
   	    mem := mem <+ {ACC|->result,PSW |->psw_new }
          END ||
          pc:= INSTRUCTION_NEXT(pc)
	END 
;

ADDI(ac,reg)=
	PRE
	  ac : SFR_ADDR & reg : SFR_ADDR
	 &(ac=AA or ac=ACC)
	 & (reg = R0 or reg = R1)
	 & mem(reg) : RAM_ADDR

	THEN
          ANY 
            addr,result,psw_new
	  WHERE 
	    result : WORD &  psw_new : WORD 
            &addr=mem(reg)
	    &result,psw_new = ula_add(mem(ACC),mem(addr),0,mem(PSW))
	  THEN 
   	    mem:= mem <+ {ACC |-> result, PSW |-> psw_new}
          END ||
          pc:= INSTRUCTION_NEXT(pc)
	END 
;


ADDD(ac,data)=
	PRE
	   ac : SFR_ADDR & 
	  (ac=AA or ac=ACC)&	
	  data : WORD
	THEN
          ANY 
            result,psw_new
	  WHERE 
	    result : WORD & psw_new : WORD & 
	    result,psw_new = ula_add(mem(ACC),data,0,mem(PSW))
	    
	  THEN  
   	    mem:= mem <+ {PSW|->psw_new,ACC|->result}
          END ||
          pc:= INSTRUCTION_NEXT(pc)
	END 

;


SUBB(ac,src)=
	PRE
	  ac : SFR_ADDR&
	 (ac = AA or ac=ACC)  &	
	  src : MEM_ADDR
	THEN
          ANY 
            result,psw_new 
	  WHERE 
	    result : WORD & psw_new : WORD
	    & result,psw_new = ula_sub(mem(ACC),mem(src),bitget(mem(PSW),7),mem(PSW))
	   
	  THEN 
   	    mem := mem <+ {ACC|->result,PSW |-> psw_new}
	    	
          END ||
          pc:= INSTRUCTION_NEXT(pc)
	END 
;

SUBBI(ac,reg)=
	PRE
	  ac : SFR_ADDR
	&(ac = AA or ac=ACC)  
	&reg : SFR_ADDR
	&(reg = R0 or reg = R1)
	& mem(reg): RAM_ADDR
        
	THEN
          ANY 
            result,psw_new,src
	  WHERE 
	   
	    src : WORD & src=mem(reg)	
	    &result : WORD & psw_new : WORD
	    & result,psw_new =ula_sub(mem(ACC),mem(src),bitget(mem(PSW),7),mem(PSW))
	   
	  THEN 
   	    mem := mem <+ {ACC|->result, PSW|->psw_new}
	    	
          END ||
          pc:= INSTRUCTION_NEXT(pc)
	END 
;

SUBBD(acc,data)=
	PRE
	   acc : SFR_ADDR & 
	  (acc = AA or acc=ACC)&	
	  data : WORD
	THEN
          ANY 
            result,psw_new
	  WHERE 
	    
	    result : WORD & psw_new : WORD
	    & result,psw_new =ula_sub(mem(AA),data,bitget(mem(PSW),7),mem(PSW))
	  THEN 
	 
   	    mem:=mem <+ {ACC|->result, PSW|->psw_new}
	    	
          END ||
          pc:= INSTRUCTION_NEXT(pc)
	END 
;


INC(src) =
	PRE
	  src : MEM_ADDR
	THEN
	  ANY 
            result,psw_new
	  WHERE 
	    result : WORD &  psw_new : WORD &
	    result,psw_new = ula_add(mem(src),1,0,0)    
	  THEN  
   	    IF (src=ACC) 
	    THEN mem := mem <+ changeacc(result,mem(PSW))
	    ELSE mem(src):=result
	    END

          END ||

          pc:= INSTRUCTION_NEXT(pc)
	END
;

INCI(Rn)=
	PRE
	Rn : SFR_ADDR
	& (Rn = R0 or Rn = R1) 
	& mem(Rn) : RAM_ADDR
	THEN
	 ANY 
	  result,psw_new,src
	 WHERE
	  result : WORD & psw_new : WORD & src : RAM_ADDR & src= mem(Rn)	
	  & result, psw_new =ula_add(mem(src),1,0,0)
	 THEN
	  IF (src=ACC) 
	  THEN mem := mem <+ changeacc(result,mem(PSW))
	  ELSE mem(src):=result
	  END 
	 END||
	 pc:= INSTRUCTION_NEXT(pc)   
	END
;

DEC(src)=
	PRE
	  src : MEM_ADDR
	THEN
	   ANY result,psw_new
	   WHERE
		result : WORD & psw_new : WORD 
		& result,psw_new = ula_sub(mem(src),1,0,0)
	   THEN
	     IF (src=ACC) 
	     THEN mem := mem <+ changeacc(result,mem(PSW))
	     ELSE mem(src):=result
	     END 
	   END||
	   pc := INSTRUCTION_NEXT(pc)
	END
;
DECI(Rn)=
	PRE 
	  Rn : SFR_ADDR 
	  &(Rn= R0 or Rn = R1)
 	  & mem(Rn): RAM_ADDR
	
        THEN
	  ANY
	   result,psw_new,src
	  WHERE 
	   result : WORD & psw_new : WORD & src : RAM_ADDR
	   & src= mem(Rn) 
	   & result,psw_new = ula_sub(mem(src),1,0,0)
	  THEN
	    mem(src):=result
	  END||
	   pc := INSTRUCTION_NEXT(pc)
	  
	END
*/
END