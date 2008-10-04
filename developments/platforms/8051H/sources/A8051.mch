MACHINE
   A8051


SEES
ALU,TYPES,TYPE_BV8,BIT_DEFINITION

INCLUDES
MEMORY

PROMOTES
pop,push

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
	  Rn : RAM_ADDR & src : MEM_ADDR & (Rn = R1 or Rn = R2)
	  & byte_uchar(mem(Rn)) : RAM_ADDR
	THEN
	   addrSetIndirect(Rn,mem(src))|| pc := INSTRUCTION_INCREMENT(pc,1)
	  
	END
;

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
	  Rn : RAM_ADDR & (Rn=R1 or Rn=R2)
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
	  dest : MEM_ADDR & src : MEM_ADDR
	THEN 
	  addrSetDirect(dest,and(mem(dest),mem(src)))||pc:=INSTRUCTION_INCREMENT(pc,1)  
	END
;

ANLMI(dest,Rn)=
   	PRE
	  dest : MEM_ADDR & Rn : RAM_ADDR & (Rn = R1 or Rn=R2) & byte_uchar(mem(Rn)) : RAM_ADDR
	THEN 
	  ANY src WHERE src : RAM_ADDR & src = byte_uchar(mem(Rn)) 
	  THEN
	      addrSetDirect(dest,and(mem(dest),mem(src)))      
	  END||pc:=INSTRUCTION_INCREMENT(pc,1)  
	END

;


ACALL(inst)=
	PRE 
	     inst : INSTRUCTION 

	THEN
	   push(uchar_byte(INSTRUCTION_INCREMENT(pc,1)))|| pc:=inst
	END
;


JNZ(jump)=
	PRE
	  jump : INSTRUCTION
	THEN
	 IF byte_uchar(mem(ACC))/=0 
	  THEN pc:= INSTRUCTION_INCREMENT(pc,jump+1)
	  ELSE pc:=INSTRUCTION_INCREMENT(pc,1)
	 END
	END

;
JZ(jump)=
	PRE
	  jump : INSTRUCTION
	THEN
	 IF byte_uchar(mem(ACC))= 0 
	 THEN pc:= INSTRUCTION_INCREMENT(pc,jump+1)
	 ELSE pc:=INSTRUCTION_INCREMENT(pc,1)
	 END
	END
;


CLR(bt)=

	PRE
	  bt : BIT_ADDRESS
	THEN
	  bitClear(bt)
	END


;
CLRA(addr)=

	PRE
	  addr : SFR_ADDR & addr = ACC
	THEN
	  addrSetDirect(addr,uchar_byte(0))||pc:= INSTRUCTION_INCREMENT(pc,1)	
	END

;

CPLA(addr)=
   PRE addr : SFR_ADDR & addr = ACC
   THEN addrSetDirect(addr,complement(mem(ACC))) ||pc:= INSTRUCTION_INCREMENT(pc,1)
   END

;

SETB(bit)=
   PRE bit : BIT_ADDRESS  
   THEN  bitSet(bit)||pc:= INSTRUCTION_INCREMENT(pc,1)
   END

;
ADD(ac,src)=
	PRE
	  ac : SFR_ADDR &(ac=AA or ac=ACC) & src : MEM_ADDR
	
	THEN
          ANY acc,Pa,Cy,Ac,Ov
	  WHERE acc: UCHAR & Pa:BIT & Cy:BIT & Ac:BIT & Ov:BIT &
	   acc,Pa,Ov,Ac,Cy =add( byte_uchar(mem(ACC)), byte_uchar(mem(src)) , 0)
	  THEN 
            update(acc,Pa,Ov,Ac,Cy)
	     || pc:=INSTRUCTION_INCREMENT(pc,1)
	  END
	END 
;

ADDI(ac,Rn)=
	PRE
	  ac : SFR_ADDR & Rn : SFR_ADDR  &(ac=AA or ac=ACC)
	 & (Rn = R0 or Rn = R1) & byte_uchar(mem(Rn)) : RAM_ADDR

	THEN
          ANY acc,Pa,Cy,Ac,Ov,src
	  WHERE acc: UCHAR & Pa:BIT & Cy:BIT & Ac:BIT & Ov:BIT & src :RAM_ADDR & src = byte_uchar(mem(Rn))
	   & acc,Pa,Ov,Ac,Cy =add( byte_uchar(mem(ACC)), byte_uchar(mem(src)) , 0)
	  THEN 
   	    update(acc,Pa,Ov,Ac,Cy)
          END || pc:=INSTRUCTION_INCREMENT(pc,1)
          
	END 
;

ADDD(ac,data)=
	PRE
	   ac : SFR_ADDR &  (ac=AA or ac=ACC)&	 data : UCHAR
	THEN
          ANY 
            acc,Pa,Cy,Ac,Ov
	  WHERE 
	    acc: UCHAR & Pa:BIT & Cy:BIT & Ac:BIT & Ov:BIT 
	   &acc,Pa,Ov,Ac,Cy =add( byte_uchar(mem(ACC)), data , 0)
	  THEN  
   	    update(acc,Pa,Ov,Ac,Cy)
          END ||pc:=INSTRUCTION_INCREMENT(pc,1)
	END 

;

SUBB(ac,src)=
	PRE
	  ac : SFR_ADDR & (ac = AA or ac=ACC) & src : MEM_ADDR
	THEN
          ANY 
            acc,Pa,Cy,Ac,Ov
	  WHERE 
	    acc: UCHAR & Pa:BIT & Cy:BIT & Ac:BIT & Ov:BIT 	
	   &acc,Pa,Ov,Ac,Cy =sub( byte_uchar(mem(ACC)), byte_uchar(mem(src)),mem(PSW)(7))
	   
	  THEN 
   	    update(acc,Pa,Ov,Ac,Cy)
          END ||
          pc:= INSTRUCTION_INCREMENT(pc,1)
	END 
;

SUBBI(ac,Rn)=
	PRE
	  ac : SFR_ADDR	&(ac = AA or ac=ACC) & Rn : SFR_ADDR
	&(Rn = R0 or Rn = R1)	& byte_uchar(mem(Rn)): RAM_ADDR
        
	THEN
          ANY 
            acc,Pa,Cy,Ac,Ov,src
	  WHERE 
	   acc: UCHAR & Pa:BIT & Cy:BIT & Ac:BIT & Ov:BIT & src : RAM_ADDR &  src = byte_uchar(mem(Rn))
	   &acc,Pa,Ov,Ac,Cy =sub( byte_uchar(mem(ACC)), byte_uchar(mem(src)),mem(PSW)(7))
	  THEN 
   	    update(acc,Pa,Ov,Ac,Cy)
          END||pc:=INSTRUCTION_INCREMENT(pc,1)
	END 
;
SUBBD(dest,data)=
	PRE
	   dest : SFR_ADDR & (dest = AA or dest=ACC) & data : UCHAR
	THEN
          ANY 
            acc,Pa,Cy,Ac,Ov
	  WHERE 
	    acc: UCHAR & Pa:BIT & Cy:BIT & Ac:BIT & Ov:BIT 
	    &acc,Pa,Ov,Ac,Cy =sub( byte_uchar(mem(ACC)),data,mem(PSW)(7))
	  THEN 
	    update(acc,Pa,Ov,Ac,Cy)
	    	
          END ||pc:=INSTRUCTION_INCREMENT(pc,1)
	END 
;
INC(src) =
	PRE
	  src : MEM_ADDR
	THEN
	  ANY 
            acc,Pa,Cy,Ac,Ov
	  WHERE 
	    acc: UCHAR & Pa:BIT & Cy:BIT & Ac:BIT & Ov:BIT 
	    &acc,Pa,Ov,Ac,Cy =add( byte_uchar(mem(src)),1,0)
	  THEN  
   	    addrSetDirect(src,uchar_byte(acc))
          END ||pc:= INSTRUCTION_INCREMENT(pc,1)
	END
;
INCI(Rn)=
	PRE
	Rn : SFR_ADDR & (Rn = R0 or Rn = R1) & byte_uchar(mem(Rn)) : RAM_ADDR
	THEN
	 ANY 
	  acc,Pa,Cy,Ac,Ov,src
	 WHERE
	  acc: UCHAR & Pa:BIT & Cy:BIT & Ac:BIT & Ov:BIT & src:RAM_ADDR & src = byte_uchar(mem(Rn))
	  & acc,Pa,Ov,Ac,Cy =add(byte_uchar(mem(src)),1,0)
	 THEN
   	    addrSetDirect(src,uchar_byte(acc))	  
	 END||pc:= INSTRUCTION_INCREMENT(pc,1)

	END


;
DEC(src)=
	PRE
	  src : MEM_ADDR
	THEN
	   ANY acc,Pa,Cy,Ac,Ov
	   WHERE
	    acc: UCHAR & Pa:BIT & Cy:BIT & Ac:BIT & Ov:BIT 
	    & acc,Pa,Ov,Ac,Cy =sub(byte_uchar(mem(src)),1,0)	
	   THEN
             addrSetDirect(src,uchar_byte(acc))	  
	   END|| pc := INSTRUCTION_INCREMENT(pc,1)
	END
;
DECI(Rn)=
	PRE 
	  Rn : SFR_ADDR &(Rn= R0 or Rn = R1) & byte_uchar(mem(Rn)): RAM_ADDR
	
        THEN
	  ANY
	   acc,Pa,Cy,Ac,Ov,src
	  WHERE 
	   acc: UCHAR & Pa:BIT & Cy:BIT & Ac:BIT & Ov:BIT & src:RAM_ADDR & src=byte_uchar(mem(Rn))
	   & acc,Pa,Ov,Ac,Cy =sub(byte_uchar(mem(src)),1,0)	
	  THEN
	   addrSetDirect(src,uchar_byte(acc))	  
	  END||pc := INSTRUCTION_INCREMENT(pc,1)
	END

END

/*Dúvidas

CPL(ca) = 
	PRE
	 ca : BIT_ADDRESS 
	THEN 
	 IF bitGet(ca)=0 THEN bitSet(ca)ELSE bitClear(ca)
	 END||pc:= INSTRUCTION_INCREMENT(pc,1)	
        END
;

MOVB(dest,src)=
	PRE
	  dest : BIT_ADDRESS & src : BIT_ADDRESS & (dest=CY or src=CY)
	THEN
	    IF bitGet(src)=1 THEN bitSet(src)ELSE bitClear(src)END ||pc := INSTRUCTION_INCREMENT(pc,1)
	END
;

JBC(bit,jump)=
   PRE
    bit : BIT_ADDRESS & jump :  INSTRUCTION
   THEN
    IF bitGet(bit)=1 
     THEN bitClear(bit)||pc:=INSTRUCTION_INCREMENT(pc,jump+1)
     ELSE pc:=INSTRUCTION_INCREMENT(pc,jump+1)
    END
   END
;

JB(bit,jump)=
   PRE
    bit : BIT_ADDRESS & jump : INSTRUCTION 
   THEN 
     IF bitGet(bit)=1 THEN pc:=INSTRUCTION_INCREMENT(pc,jump+1) 
      ELSE pc:=INSTRUCTION_INCREMENT(pc,1)
     END  
   END
;

RET =
   PRE 
    mem(SP) : INSTRUCTION
   THEN 
    pc:=pop  
   END
;

ANLB(ca,bit)=
   PRE 
    bit : BIT_ADDRESS & ca  : BIT_ADDRESS & (ca = CC)
   THEN
    bitSet(CY,and_bit(bitGet(CY),bitGet(bit)))
   END
;


ANLBN(ca,bit)=
   PRE 
    bit : BIT_ADDRESS & ca  : BIT_ADDRESS &(ca = CY)
   THEN
    bitSet(CY,and_bit(bitGet(CY),bitGet(bit)))
   END

JC(jump)=	
   PRE
    jump : INSTRUCTION
   THEN
    pc:= bitjump(bitget(mem(PSW),7),1,jump,pc)
   END
;

JNB(bit,jump)=
	PRE 
	  bit : BIT_ADDRESS & jump : INSTRUCTION
	THEN 
	  IF bitGet(bit)=0 
	   THEN pc:= INSTRUCTION_INCREMENT(pc,jump+1) 
	   ELSE pc:= INSTRUCTION_INCREMENT(pc,1) 
	  END
	END
;


JNC(bit,jump)=
	PRE
	  bit : BIT_ADDRESS & jump : INSTRUCTION
	THEN
	  IF bitGet(bit)=0
	   THEN bitClear(bit)||pc:=INSTRUCTION_INCREMENT(pc,jump+1)
	   ELSE pc:=INSTRUCTION_INCREMENT(pc,1)
	  END
	END
;


*/