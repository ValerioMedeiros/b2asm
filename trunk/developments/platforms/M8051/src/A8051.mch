/**
TO DO:
- Pass the rest off instruction for the new impletation (Doing)
**/

MACHINE
   A8051


SEES
ALU,TYPES,BYTE_DEFINITION,BIT_DEFINITION

INCLUDES
MEMORY


CONCRETE_VARIABLES
pc

INVARIANT
pc : ROM_ADDR

INITIALISATION
pc:=0

OPERATIONS 
    
/*
This operation here aren't in 8051, but this is necessary in utilizing of 8051 Machine for other aplication
*/
INIT = 
  BEGIN pc :=0 END
;

/************************************************************************************************************************
Unconditionally call a subroutine located at the indicated address.This increment pc, take the next instrution address,
and put than in stack, low bits first and upper bits after, just the pc have length of 16 bits. For this is necessary 
increment the SP twice
************************************************************************************************************************/
/*
Represents the follow 8051's operations :
	ACALL addr - Set pc to address of the ROM passed
*/
ACALL(paddr)=
	PRE 
	    paddr:ROM_ADDR 
	THEN
	    ANY 
	    	upper_pc,lower_pc
		WHERE
		    upper_pc,lower_pc:BYTE*BYTE & upper_pc,lower_pc=usint_byte(PC_INCREMENT(pc,1))
		THEN
	    	push_two(lower_pc,upper_pc)
	    	|| pc:=paddr
	    END
	END
;

/************************************************************************************************************************
Set of operation ADD that adds the byte variables indicated (constant, indirect address, direct address) and the Accumulator, 
leaving the result in the Accumulator . This operation affected the flag carry(CY), half-carry(AC) and overflow(OV).
*************************************************************************************************************************/

/*
Represents the follows 8051's instructions :
ADD A,Rn - Add the contents of Rn Register to the Accumulator
ADD A,direct - Add the contents of direct addr to the Accumulator
*/
ADD(ac,src)=
	PRE
	  ac=ACC & src:MEM_ADDR	
	THEN
          ANY acc,Pa,Cy,Ac,Ov
	  WHERE acc: UCHAR & Pa:BIT & Cy:BIT & Ac:BIT & Ov:BIT &
	   acc,Pa,Ov,Ac,Cy =add( byte_uchar(mem(ACC)), byte_uchar(mem(src)) , 0)
	  THEN 
            update(acc,Pa,Ov,Ac,Cy)
	     || pc:=PC_INCREMENT(pc,1)
	  END
	END 
;

/*
Represents the follow 8051's instruction :
ADD A,@Rn - Add the contents of address indicate for register Rn to the Accumulator
*/
ADDI(ac,Rn)=
	PRE
	 Rn : SFR_ADDR  & ac=ACC & (Rn = R0 or Rn = R1) & byte_uchar(mem(Rn)):RAM_ADDR
	THEN
      ANY acc,Pa,Cy,Ac,Ov,src
	  WHERE 
	      acc: UCHAR & Pa:BIT & Cy:BIT & Ac:BIT & Ov:BIT & src :RAM_ADDR & src = byte_uchar(mem(Rn))
	    & acc,Pa,Ov,Ac,Cy =add( byte_uchar(mem(ACC)), byte_uchar(mem(src)) , 0)
	  THEN 
   	    update(acc,Pa,Ov,Ac,Cy)
      END || pc:=PC_INCREMENT(pc,1)
          
	END 
;

/*
Represents the follows 8051's instructions :
ADD A,#data - Add the value passed to the Accumulator
OBS: The value of constant need be passed as UCHAR
*/
ADDD(ac,data)=
	PRE
	   ac:SFR_ADDR & ac=ACC & data:UCHAR
	THEN
      ANY 
          acc,Pa,Cy,Ac,Ov
	  WHERE 
	      acc: UCHAR & Pa:BIT & Cy:BIT & Ac:BIT & Ov:BIT 
	      & acc,Pa,Ov,Ac,Cy =add( byte_uchar(mem(ACC)), data , 0)
	  THEN  
	      update(acc,Pa,Ov,Ac,Cy)
      END || pc:=PC_INCREMENT(pc,1)
	END 
;

/************************************************************************************************************************
Set of operation ADDC that adds simultaneously the byte variables indicated (constant, indirect address, direct address), 
the Accumulator and the carry leaving the result in the Accumulator .
This operation affected the flag carry(CY), half-carry(AC) and overflow(OV).
*************************************************************************************************************************/

/*
Represents the follows 8051's instructions :
ADDC A,Rn - Adds the value of register passed, the Accumulator and the carry
ADDC A,direct - Adds the value of address passed, the Accumulator and the carry
*/
ADDC(ac,src)=
	PRE
	  ac=ACC & src:MEM_ADDR	
	THEN
          ANY acc,Pa,Cy,Ac,Ov
	  WHERE acc: UCHAR & Pa:BIT & Cy:BIT & Ac:BIT & Ov:BIT &
	   acc,Pa,Ov,Ac,Cy =add(byte_uchar(mem(ACC)), byte_uchar(mem(src)),BIT_GET(CY,mem))
	  THEN 
            update(acc,Pa,Ov,Ac,Cy)
	     || pc:=PC_INCREMENT(pc,1)
	  END
	END 
;

/*
Represents the follow 8051's instruction :
ADDC A,@Rn - Adds the value of address indicate by register passed, the Accumulator and the carry
*/
ADDCI(ac,Rn)=
	PRE
	 Rn : SFR_ADDR  & ac=ACC & (Rn = R0 or Rn = R1) & byte_uchar(mem(Rn)):RAM_ADDR
	THEN
      ANY acc,Pa,Cy,Ac,Ov,src
	  WHERE 
	      acc: UCHAR & Pa:BIT & Cy:BIT & Ac:BIT & Ov:BIT & src :RAM_ADDR & src = byte_uchar(mem(Rn))
	    & acc,Pa,Ov,Ac,Cy =add(byte_uchar(mem(ACC)),byte_uchar(mem(src)),BIT_GET(CY,mem))
	  THEN 
   	    update(acc,Pa,Ov,Ac,Cy)
      END || pc:=PC_INCREMENT(pc,1)
          
	END 
;
/*
Represents the follow 8051's instruction :
ADDC A,@Rn - Adds the value passed, the Accumulator and the carry.
*/
ADDCD(ac,data)=
	PRE
	   ac:SFR_ADDR & ac=ACC & data:UCHAR
	THEN
      ANY 
          acc,Pa,Cy,Ac,Ov
	  WHERE 
	      acc: UCHAR & Pa:BIT & Cy:BIT & Ac:BIT & Ov:BIT 
	      & acc,Pa,Ov,Ac,Cy =add( byte_uchar(mem(ACC)),data,BIT_GET(CY,mem))
	  THEN  
	      update(acc,Pa,Ov,Ac,Cy)
      END || pc:=PC_INCREMENT(pc,1)
	END 
;

/*
AJMP no implement
*/


/************************************************************************************************************************
Set of operation ANL that performs the bitwise logical-AND between the variables indicated (constant, indirect address, 
direct address), and story the result in the destination. No flag (CY,AC and OV) are affected.
*************************************************************************************************************************/

/*
Represents the follow 8051's instruction :
ANL A,Rn - bitwise-AND between the Accumulator and the register passed.
ANL A,direct - bitwise-AND between the Accumulator and the content of passed address
*/
ANL(acc,direct)=
	PRE 
	  acc = ACC &
	  direct : MEM_ADDR
	THEN
	  addrSetDirect(ACC,and(mem(ACC),mem(direct)))|| pc:=PC_INCREMENT(pc,1)  
	END 

;

/*
Represents the follow 8051's instruction :
ANL A,@Rn - bitwise-AND between the Accumulator and the value of address indicate by register passed.
*/
ANLI(acc,Rn)=
	PRE 
	    acc=ACC
	  & Rn : RAM_ADDR & (Rn=R1 or Rn=R2)
	  & byte_uchar(mem(Rn)):RAM_ADDR
	  
	THEN 
	  ANY addr WHERE addr : RAM_ADDR & addr= byte_uchar(mem(Rn)) THEN
	     addrSetDirect(ACC, and(mem(ACC),mem(addr))) || pc:=PC_INCREMENT(pc,1)  
	  END 
    END
;

/*
Represents the follow 8051's instruction :
ANL A,@Rn - bitwise-AND between the Accumulator and the value passed.
*/
ANLD(acc,data)=
   	PRE
	  acc=ACC & data:UCHAR
	THEN 
	  addrSetDirect(ACC,and(mem(ACC),uchar_byte(data)))||pc:=PC_INCREMENT(pc,1)  
	END
;

/*
Represents the follow 8051's instruction :
ANL direct,A - bitwise-AND between the value of address passed as destination and the Accumulator.
*/
ANLDA(dest,acc)=
   	PRE
	  dest:MEM_ADDR & acc= ACC
	THEN 
	  addrSetDirect(dest,and(mem(dest),mem(ACC)))      
	  ||pc:=PC_INCREMENT(pc,1)  
	END
;

/*
Represents the follow 8051's instruction :
ANL direct,A - bitwise-AND between the value of address passed as destination and the Accumulator.
*/
ANLDD(dest,data)=
   	PRE
	  dest:MEM_ADDR & data:UCHAR
	THEN 
	  addrSetDirect(dest,and(mem(dest),uchar_byte(data)))      
	  ||pc:=PC_INCREMENT(pc,1)  
	END


END





/*CJNE A,#data,rel --- Direct Adrres */
/*CJNE Rn,#data,rel --- Direct Adrres */
/*
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
*/
/*CJNE @Ri,<data>,rel ----- Indirect */

/*
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


*/

/*CJNE A,direct,rel ---- jump Direct */
/*
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
;

CPL(ca) = 
	PRE
	 ca : BIT_ADDRESS 
	THEN 
	 
	  IF BIT_GET(ca,mem)=0 THEN bitSet(ca)ELSE bitClear(ca)
	  END||pc:= INSTRUCTION_INCREMENT(pc,1)	
         
        END


;

MOVB(dest,src)=
	PRE
	  dest : BIT_ADDRESS & src : BIT_ADDRESS & (dest=CY or src=CY)
	THEN
	    IF BIT_GET(src,mem)=1 THEN bitSet(src)ELSE bitClear(src)END ||pc := INSTRUCTION_INCREMENT(pc,1)
	END
;

JBC(bit,jump)=
   PRE
    bit : BIT_ADDRESS & jump :  INSTRUCTION
   THEN
    IF BIT_GET(bit,mem)=1 
     THEN bitClear(bit)||pc:=INSTRUCTION_INCREMENT(pc,jump+1)
     ELSE pc:=INSTRUCTION_INCREMENT(pc,jump+1)
    END
   END
;


JB(bit,jump)=
   PRE
    bit : BIT_ADDRESS & jump : INSTRUCTION 
   THEN 
     IF BIT_GET(bit,mem)=1 THEN pc:=INSTRUCTION_INCREMENT(pc,jump+1) 
      ELSE pc:=INSTRUCTION_INCREMENT(pc,1)
     END  
   END
;




JNB(bit,jump)=
	PRE 
	  bit : BIT_ADDRESS & jump : INSTRUCTION
	THEN 
	  IF BIT_GET(bit,mem)=0 
	   THEN pc:= INSTRUCTION_INCREMENT(pc,jump+1) 
	   ELSE pc:= INSTRUCTION_INCREMENT(pc,1) 
	  END
	END
;


JNC(bit,jump)=
	PRE
	  bit : BIT_ADDRESS & jump : INSTRUCTION
	THEN
	  IF BIT_GET(bit,mem)=0
	   THEN bitClear(bit)||pc:=INSTRUCTION_INCREMENT(pc,jump+1)
	   ELSE pc:=INSTRUCTION_INCREMENT(pc,1)
	  END
	END
;
SWAP(addr)=
	PRE
  	  addr : MEM_ADDR
	THEN
  	  addrSetDirect(addr,swap(mem(addr)))
	END

;

JC(jump)=	
   PRE
    jump : INSTRUCTION
   THEN
    pc:= bitjump(bitget(mem(PSW),7),1,jump,pc)
   END

END
*/
/*D�vidas



ANLBN(ca,bit)=
   PRE 
    bit : BIT_ADDRESS & ca  : BIT_ADDRESS &(ca = CY)
   THEN
    bitSet(CY,and_bit(bitGet(CY),bitGet(bit)))
   END


ANLB(ca,bit)=
   PRE 
    bit : BIT_ADDRESS & ca  : BIT_ADDRESS & (ca = CC)
   THEN
    bitSet(CY,and_bit(BIT_GET(CY,mem),BIT_GET(bit,mem)))
   END


RET =
   PRE 
    mem(SP) : INSTRUCTION
   THEN 
    pc:=pop  
   END
;

*/