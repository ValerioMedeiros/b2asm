/**
TO DO:
- Pass the rest off instruction for the new impletation (Doing)
- Set of DIV instructions
**/

/**
This machine implement the set of 8051's instructions. However, some features as wide of instruction, implementation of data memory and
reading in port not was necessary for this specification.
**/
MACHINE
   A8051


SEES
ALU,TYPES,BYTE_DEFINITION,BIT_DEFINITION

INCLUDES
MEMORY


CONCRETE_VARIABLES
/*Program Counter Register*/
pc

INVARIANT
pc : ROM_ADDR

INITIALISATION
pc:=0

OPERATIONS 
    
/*
This operation aren't in 8051, but this is necessary in utilizing of 8051 Machine for other aplication
*/
INIT = 
  BEGIN pc :=0 END
;

/************************************************************************************************************************
Unconditionally call a subroutine located at the indicated address.This increment pc, take the next instrution address,
and put than in stack, low bits first and upper bits after, just the pc have length of 16 bits. For this is necessary 
increment the SP twice.The destination address is obtained by successively concatenating the five high-order.....
see in manual
************************************************************************************************************************/
/*
Represents the follow 8051's operations :
	ACALL addr - Set pc to address of the ROM passed

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
*/

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
	    WHERE 
	        acc: UCHAR & 
	        Pa:BIT & 
	        Cy:BIT & 
	        Ac:BIT & 
	        Ov:BIT &
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
	    ac=ACC & 
	    Rn : SFR_ADDR  & 
	    (Rn = R0 or Rn = R1) & 
	    byte_uchar(mem(Rn)):RAM_ADDR
	THEN
      ANY acc,Pa,Cy,Ac,Ov,src
	  WHERE 
	      acc: UCHAR & 
	      Pa:BIT & 
	      Cy:BIT & 
	      Ac:BIT & 
	      Ov:BIT & 
	      src :RAM_ADDR & 
	      src = byte_uchar(mem(Rn)) & 
	      acc,Pa,Ov,Ac,Cy =add( byte_uchar(mem(ACC)), byte_uchar(mem(src)) , 0)
	  THEN 
	      update(acc,Pa,Ov,Ac,Cy)
      END || 
      pc:=PC_INCREMENT(pc,1)          
	END 
;

/*
Represents the follows 8051's instructions :
ADD A,#data - Add the value passed to the Accumulator
OBS: The value of constant must be passed as UCHAR
*/
ADDD(ac,data)=
	PRE
	   ac:SFR_ADDR & 
	   ac=ACC & 
	   data:UCHAR
	THEN
	    ANY 
	        acc,Pa,Cy,Ac,Ov
	    WHERE 
	        acc: UCHAR & 
	        Pa:BIT & 
	        Cy:BIT & 
	        Ac:BIT & 
	        Ov:BIT & 
	        acc,Pa,Ov,Ac,Cy =add( byte_uchar(mem(ACC)), data , 0)
	  THEN  
	      update(acc,Pa,Ov,Ac,Cy)
      END || 
      pc:=PC_INCREMENT(pc,1)
	END 
;

/************************************************************************************************************************
Set of operation ADDC that adds simultaneously the byte variables indicated (constant, indirect address, direct address), 
the Accumulator and the carry leaving the result in the Accumulator. This operation affected the flag carry(CY), 
half-carry(AC) and overflow(OV).
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
	  	WHERE
	  	     acc: UCHAR & 
	  	     Pa:BIT & 
	  	     Cy:BIT & 
	  	     Ac:BIT & 
	  	     Ov:BIT &
	  	     acc,Pa,Ov,Ac,Cy =add(byte_uchar(mem(ACC)), byte_uchar(mem(src)),BIT_GET(CY,mem))
	  	THEN 
            update(acc,Pa,Ov,Ac,Cy) || 
            pc:=PC_INCREMENT(pc,1)
	  	END
	END 
;

/*
Represents the follow 8051's instruction :
ADDC A,@Rn - Adds the value of address indicate by register passed, the Accumulator and the carry
*/
ADDCI(ac,Rn)=
	PRE
	    Rn : SFR_ADDR  & 
	    	(Rn = R0 or Rn = R1) & 
	    ac=ACC & 
	    byte_uchar(mem(Rn)):RAM_ADDR
	THEN
	    ANY acc,Pa,Cy,Ac,Ov,src
	  	WHERE 
	  	    acc: UCHAR & 
	  	    Pa:BIT & 
	  	    Cy:BIT & 
	  	    Ac:BIT & 
	  	    Ov:BIT & 
	  	    src :RAM_ADDR & 
	  	    src = byte_uchar(mem(Rn)) & 
	  	    acc,Pa,Ov,Ac,Cy =add(byte_uchar(mem(ACC)),byte_uchar(mem(src)),BIT_GET(CY,mem))
	  	THEN 
   	    	update(acc,Pa,Ov,Ac,Cy)
      	END || 
      	pc:=PC_INCREMENT(pc,1)  
	END 
;

/*
Represents the follow 8051's instruction :
ADDC A,data - Adds the value passed, the Accumulator and the carry.
*/
ADDCD(ac,data)=
	PRE
	   ac:SFR_ADDR & 
	   ac=ACC & 
	   data:UCHAR
	THEN
	    ANY 
	        acc,Pa,Cy,Ac,Ov
	  	WHERE 
	  	    acc: UCHAR & 
	      	Pa:BIT & 
	      	Cy:BIT & 
	      	Ac:BIT & 
	      	Ov:BIT & 
	      	acc,Pa,Ov,Ac,Cy =add( byte_uchar(mem(ACC)),data,BIT_GET(CY,mem))
	  THEN  
	      	update(acc,Pa,Ov,Ac,Cy)
      END || pc:=PC_INCREMENT(pc,1)
	END 
;

/****************************************************************
???AJMP no implement - I don't know how implement then???
*****************************************************************/


/************************************************************************************************************************
Set of operation ANL that performs the bitwise logical-AND between the variables indicated (constant, indirect address, 
direct address), and story the result in the destination. No flag (CY,AC and OV) are affected.
*************************************************************************************************************************/

/*
Represents the follows 8051's instructions :
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
	    ANY addr 
	    WHERE 
	        addr : RAM_ADDR & 
	        addr= byte_uchar(mem(Rn)) 
	    THEN
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
	  	addrSetDirect(ACC,and(mem(ACC),uchar_byte(data)))||
	  	pc:=PC_INCREMENT(pc,1)  
	END
;

/*
Represents the follow 8051's instruction :
ANL direct,A - bitwise-AND between the value of address passed as destination and the Accumulator.

Obs: ANL Rn,A is not available in 8051, but here it is permitted. This problem will be corrects in next version
*/
ANLDA(dest,acc)=
   	PRE
   	    dest:MEM_ADDR & acc= ACC
	THEN 
	    addrSetDirect(dest,and(mem(dest),mem(ACC)))||
	    pc:=PC_INCREMENT(pc,1)  
	END
;

/*
Represents the follow 8051's instruction :
ANL direct,A - bitwise-AND between the value of address passed as destination and the Accumulator.

Obs: ANL Rn,#data is not available in 8051, but here it is permitted. This problem will be corrects in next version
*/
ANLDD(dest,data)=
   	PRE
   	    dest:MEM_ADDR & data:UCHAR
	THEN 
	    addrSetDirect(dest,and(mem(dest),uchar_byte(data)))||
	    pc:=PC_INCREMENT(pc,1)  
	END
;

/*
Represents the follow 8051's instruction :
ANL C bit - Logic and between CY and bit passed, leaving the result in CY

Obs: The feature of complement of the bit passed is not available here. Exemplo: ANL C,/bit
*/
ANLB (cy,bit)=
	PRE
	    cy = CY & 
	    bit: BIT_ADDRESS
	THEN
	    ANY 
	        addr,
	        ind 
	    WHERE
	        addr:MEM_ADDR &
	        ind:BYTE_INDEX
	    THEN
	        IF BIT_GET(bit,mem)=0 
	        THEN 
	            bitClear(addr,ind) 
	        END
	    END||
	    pc:= PC_INCREMENT(pc,1)	        
	END
	
;

/************************************************************************************************************************
The set of CJNE compare the magnitude of the first two operations and branches if their are not equals. The branch 
destination is formend for adds of the rel parameter and the location of next instruction. This instruction set the 
carry CY if the unsigned value of the first parameter is less than second and clear otherwise.
************************************************************************************************************************/

/*
Represents the follow 8051's instruction :
CJNE A,direct,rel - Compare the value of Accumulator and the value of address passed

Obs: CJNE A,Rn,rel is not available in 8051, but here it is available. This problem will be corrects in next version
*/
CJNE(acc,direct,jump)=

	PRE 
	  acc = ACC & 
	  direct:MEM_ADDR & 
	  jump : ROM_ADDR
	THEN 
	  IF not(byte_uchar(mem(ACC))=byte_uchar(mem(direct)))
	      THEN
	          IF byte_uchar(mem(ACC))< byte_uchar(mem(direct))
	              THEN
	              	bitSet(PSW,7) /* CY*/
	              ELSE 
	                bitClear(PSW,7) /* CY*/
	          END || 
	          pc:= PC_INCREMENT(pc,jump+1)
	  	  ELSE	
	   		pc:= PC_INCREMENT(pc,1) 		
	  END 
	END
;

/*

Represents the follow 8051's instruction :
CJNE A,#data,rel - Compare the value of Accumulator and the value passed
CJNE Rn,#data,rel - Compare the value of Register passed and the constant passed
*/
CJNED(dest,data,jump)=
	PRE 
	  dest: R0..R7\/{ACC} & 
	  data : UCHAR & 
	  jump : ROM_ADDR
	THEN 
	  IF not(byte_uchar(mem(dest))=data)
	      THEN
	          IF(byte_uchar(mem(dest))<data)
	              THEN
	              	bitSet(PSW,7)/*CY*/
	              ELSE 
	                bitClear(PSW,7)/*CY*/
	          END||pc:= PC_INCREMENT(pc,jump+1)
	  	  ELSE	
	   		pc:= PC_INCREMENT(pc,1) 		
	  END 
	END
;

/*
Represents the follow 8051's instruction :
CJNE @Rn,#data,rel - Compare the value of address indicate by the register passed and the constant passed
*/
CJNEI(Rn,data,jump)=
	PRE 
	   Rn : RAM_ADDR & 
	   (Rn=R1 or  Rn=R2) & 
	   data:UCHAR & 
	   jump : ROM_ADDR & 
	   byte_uchar(mem(Rn)) : RAM_ADDR
	THEN 
	   ANY addr 
	   WHERE 
	       addr:RAM_ADDR & 
	       addr = byte_uchar(mem(Rn))	       
	   THEN
	       IF not(byte_uchar(mem(addr))=data)
	           THEN
	               IF(byte_uchar(mem(addr))< data)
	               THEN 
	                   bitSet(PSW,7) /*CY*/	                   
	               ELSE 
	                   bitClear(PSW,7)/*CY*/
	               END || 
	               pc := PC_INCREMENT(pc,jump+1)			
	       	   ELSE	
	         	   pc:=PC_INCREMENT(pc,1)			
	       END 
	   END
    END
;


/************************************************************************************************************************
The set of CLR clear the Accumulator or the bit passed. No flags are affected
************************************************************************************************************************/

/*
Represents the follow 8051's instruction :
CLR bt - Clear the bit passed (if i clear the parity bit ? - The 8051 allowed clear the parity bit, but its generate a inconsistence)
CLR C - Clear the CY
*/
CLR(bt)=
	PRE
	  bt:BIT_ADDRESS
	THEN
	    ANY
	        addr,ind
	    WHERE
	        addr:MEM_ADDR &
	        ind:BYTE_INDEX &
	        addr,ind = bt
	    THEN
	        bitClear(addr,ind)
	    END||
	    pc:= PC_INCREMENT(pc,1)
	    
	END
;

/*
Represents the follow 8051's instruction :
CLR ACC - Clear the Accumulator
*/
CLRA(addr)=
	PRE
	    addr=ACC
	THEN
	    addrSetDirect(ACC,uchar_byte(0))||
	 	pc:= PC_INCREMENT(pc,1)	
	END
;
/************************************************************************************************************************
The set of CPL make the complement of Accumulator or a bit addressable passed and put the result in both. 
No flags are affect
************************************************************************************************************************/

/*
Represents the follows 8051's instructions :
CPL bit - Complement the bit passed
CPL C - Complement the Carry CY
*/
CPL(bit) = 
	PRE
	    bit:BIT_ADDRESS 
	THEN 
	    ANY 
	        addr,ind
	    WHERE
	        addr:MEM_ADDR &
	        ind:BYTE_INDEX &
	        addr,ind=bit 
	    THEN	        
	    	IF BIT_GET(bit,mem)=0 
	    	THEN 
	        	bitSet(addr,ind) 
	    	ELSE 
	        	bitClear(addr,ind) 
	    	END
	    END||pc:= PC_INCREMENT(pc,1)	    
	    	
    END
;

/*
Represents the follow 8051's instruction:
CPL A - Complement the accumulator
*/
CPLA(acc) = 
	PRE
	    acc= ACC
	THEN 
	    addrSetDirect(ACC,complement(mem(ACC))) || 
	    pc:= PC_INCREMENT(pc,1)
    END
;    

/************************************************************************************************************************
The DA instruction is not implement yet
************************************************************************************************************************/
/*
DA
*/


/************************************************************************************************************************
The set of DEC decrement by 1 the bytes indicates. No flags are affected 
************************************************************************************************************************/
/*
Represents the follows 8051's instructions:
DEC A - Decrement the value of accumulator
DEC Rn - Decrement the value of register passed
DEC direct - Decrement the value of the address passed
*/
DEC(src)=
	PRE
	  src : MEM_ADDR
	THEN
	   ANY acc,Pa,Cy,Ac,Ov
	   WHERE
	       acc: UCHAR & Pa:BIT & 
	       Cy:BIT & 
	       Ac:BIT & 
	       Ov:BIT 
	       & acc,Pa,Ov,Ac,Cy =sub(byte_uchar(mem(src)),1,0)	
	   THEN
	       addrSetDirect(src,uchar_byte(acc))	  
	   END|| pc := PC_INCREMENT(pc,1)
	END
;

/*
Represents the follow 8051's instruction:
DEC @Rn - Decrement the value of the address indicate for register passed
*/
DECI(Rn)=
	PRE 
	    Rn : SFR_ADDR &(Rn= R0 or Rn = R1) & byte_uchar(mem(Rn)): RAM_ADDR	
    THEN
        ANY	acc,Pa,Cy,Ac,Ov,src
	  	WHERE 
	   		acc: UCHAR & 
	   		Pa:BIT & 
	   		Cy:BIT & 
	   		Ac:BIT & 
	   		Ov:BIT & 
	   		src:RAM_ADDR & 
	   		src=byte_uchar(mem(Rn)) &
	   		acc,Pa,Ov,Ac,Cy =sub(byte_uchar(mem(src)),1,0)	
	  	THEN
	   		addrSetDirect(src,uchar_byte(acc))	  
	  	END||
	  	pc := PC_INCREMENT(pc,1)
	END
;

/************************************************************************************************************************
The DIV instruction perform the integer division of accumulator and B register. The accumulator recives the quotient
and the B register recive the remainder. If the B registe have the value zero, the overflow bit is setting, otherwise
than are clean. The bit overflow is clean in every case.
************************************************************************************************************************/

/*
Represents the follow 8051's instruction:
DIV AB - The div between Accumulator and B register. leaving the result in A and the remainder in B.
*/
DIV_AB =
BEGIN
    ANY 
        result,rest,ov,pp 
    WHERE 
        result:UCHAR & 
        rest:UCHAR & 
        ov:BIT & 
        result,rest,ov,pp=div(byte_uchar(mem(AA)),byte_uchar(mem(BB))) 
    THEN
       update_div(uchar_byte(result),uchar_byte(rest),ov,pp)
    END    	 
END
;



/************************************************************************************************************************
The DJNZ decrement by 1 the value indicates and branche to relative adrress, formed for adds between next 
instruction address and jump passed, if the result is not zero, otherwise set the pc to next instruction.
No flags are affected
************************************************************************************************************************/
/*
Represents the follows 8051's instructions:
DJNZ Rn, rel - Decrement the value of the indicate Register of working
DJNZ direct, rel - Decrement the value of the address indicate and branches if the result is not zero

Obs: The instruction DJNZ A,rel is not permitted in 8051 and here
*/
DJNZ(direct,jump)=
	PRE
	    direct:RAM_ADDR & 
	    jump:ROM_ADDR & 
	    not(direct=ACC)
	THEN
	    ANY	
	        result,Pa,Cy,Ac,Ov
	  	WHERE 
	   		result: UCHAR & 
	   		Pa:BIT & 
	   		Cy:BIT & 
	   		Ac:BIT & 
	   		Ov:BIT & 
	   		result,Pa,Ov,Ac,Cy =sub(byte_uchar(mem(direct)),1,0)	
	  	THEN
	  	    addrSetDirect(direct,uchar_byte(result)) ||
	  	    IF not(result=0) 
	  	    THEN 
	  	        pc := PC_INCREMENT(pc,jump+1) 
	  	    ELSE 
	  	        pc:=PC_INCREMENT(pc,1) 
	  	    END
	  	END
	END
;

/************************************************************************************************************************
The set of INC instructions increment the indicate variable by 1. No flags are affected
************************************************************************************************************************/
/*
Represents the follows 8051's instructions:
INC A - Increment the value of accumulator
INC Rn - Increment the value of register passed
INC direct - Increment the value of the address passed
*/
INC(src) =
	PRE
	    src : MEM_ADDR
	THEN
	    ANY 
	        acc,Pa,Cy,Ac,Ov
        WHERE
            acc: UCHAR & 
            Pa:BIT & 
            Cy:BIT & 
            Ac:BIT & 
            Ov:BIT &
            acc,Pa,Ov,Ac,Cy =add( byte_uchar(mem(src)),1,0)
	  	THEN  
   	    	addrSetDirect(src,uchar_byte(acc))
        END ||
        pc:= PC_INCREMENT(pc,1)
	END
;

/*
Represents the follow 8051's instruction:
INC @Ri - Increment the value of address indicate for register passed
*/
INCI(Rn)=
	PRE
	    Rn : SFR_ADDR & 
	    	(Rn = R0 or Rn = R1) & 
	    byte_uchar(mem(Rn)) : RAM_ADDR
	THEN
	    ANY 
	        acc,Pa,Cy,Ac,Ov,src
	 	WHERE
	 	    acc: UCHAR & 
	 	    Pa:BIT & 
	 	    Cy:BIT & 
	 	    Ac:BIT & 
	 	    Ov:BIT & 
	 	    src:RAM_ADDR & 
	 	    src = byte_uchar(mem(Rn)) & 
	 	    acc,Pa,Ov,Ac,Cy =add(byte_uchar(mem(src)),1,0)
	 	THEN
   	    	addrSetDirect(src,uchar_byte(acc))	  
	 	END||
	 	pc:= PC_INCREMENT(pc,1)
	END
;

/************************************************************************************************************************
The JB instruction jum to the relative addres, formed for adds of addr passed and the addres of next instuction, if the
bit passed is one, otherwise it go to next instruction. No flags are affected and the bit tested is not modified
************************************************************************************************************************/
/*
Represents the follow 8051's instruction:
JB bit,rel - make a relative jump if then bit is one

*/
JB(bit,jump)=
   PRE
       bit : BIT_ADDRESS & 
       jump : ROM_ADDR 
   THEN 
       IF BIT_GET(bit,mem)=1 
       THEN 
           pc:=PC_INCREMENT(pc,jump+1)            
       ELSE 
           pc:=PC_INCREMENT(pc,1) 
       END         
   END
;

/************************************************************************************************************************
The JBC instruction jum to the relative address, formed for adds of addr passed and the addres of next instuction, if the
bit passed is one and clear then, otherwise it go to next instruction with midified the bit passed. No flags are affected. 
************************************************************************************************************************/
/*
Represents the follow 8051's instruction:
JBC bit,rel - make a relative jump if then bit passed is one and clear then
*/
JBC(bit,jump)=
   PRE
       bit : BIT_ADDRESS & 
       jump : ROM_ADDR
   THEN
       IF BIT_GET(bit,mem)=1
       THEN 
           ANY
               addr,ind
           WHERE
               addr:MEM_ADDR &
               ind:BYTE_INDEX & 
               addr,ind = bit
           THEN
               bitClear(addr,ind)||
               pc:=PC_INCREMENT(pc,jump+1)
           END
       ELSE 
           pc:=PC_INCREMENT(pc,jump+1)           
       END           
   END
;

/************************************************************************************************************************
The JC instruction jum to the relative addres, formed for adds of addr passed and the addres of next instuction, if the
carry (CY) is one, otherwise it go to next instruction. No flags are affected.
************************************************************************************************************************/
/*
Represents the follow 8051's instruction:
JC rel - make a relative jump if then bit if the carry is one
*/
JC(jump)=	
   PRE
       jump : ROM_ADDR
   THEN
       IF BIT_GET(CY,mem)=1
       THEN 
           pc:= PC_INCREMENT(pc,1+jump)
       ELSE 
           pc:= PC_INCREMENT(pc,1)
       END
   END
;

/************************************************************************************************************************
The JNB instruction jum to the relative addres, formed for adds of addr passed and the addres of next instuction, if the
bit passed is not one, otherwise it go to next instruction. No flags are affected and the bit tested is not modified
************************************************************************************************************************/
/*
Represents the follow 8051's instruction:
JNB bit,rel - make a relative jump if then bit is not one
*/
JNB(bit,jump)=
	PRE 
	    bit : BIT_ADDRESS & jump : ROM_ADDR
	THEN 
	    IF BIT_GET(bit,mem) = 0 
	    THEN 
	        pc:= PC_INCREMENT(pc,jump+1) 
	    ELSE 
	        pc:= PC_INCREMENT(pc,1)	        
	    END
	END
;

/************************************************************************************************************************
The JC instruction jum to the relative addres, formed for adds of addr passed and the addres of next instuction, if the
carry (CY) is not one, otherwise it go to next instruction. No flags are affected.
************************************************************************************************************************/
/*
Represents the follow 8051's instruction:
JNC rel - make a relative jump if then bit if the carry is not one
*/
JNC(bit,jump)=
	PRE
	    bit : BIT_ADDRESS & jump :ROM_ADDR
	THEN
	    IF BIT_GET(bit,mem)=0
	    THEN 
	        ANY addr,ind
	        WHERE
	            addr:MEM_ADDR &
	            ind:BYTE_INDEX&
	            addr,ind=bit
	        THEN
	            bitClear(addr,ind)||
	        	pc:=PC_INCREMENT(pc,jump+1)
	        END
	    ELSE 
	        pc:=PC_INCREMENT(pc,1)    	        
	  	END
	END

;
/************************************************************************************************************************
The JNZ instruction jum to the relative addres, formed for adds of addr passed and the addres of next instuction, if any 
bit of the accumulator is one, otherwise it go to next instruction. No flags are affected.
************************************************************************************************************************/
/*
Represents the follow 8051's instruction:
JNZ rel - make a relative jump if then bit if the carry is one
*/
JNZ(jump)=
	PRE
	    jump : ROM_ADDR
	THEN
	    IF byte_uchar(mem(ACC))/=0
	    THEN 
	        pc:= PC_INCREMENT(pc,jump+1)
	    ELSE 
	        pc:=PC_INCREMENT(pc,1) 
	    END
	END
;

/************************************************************************************************************************
The JZ instruction jum to the relative addres, formed for adds of addr passed and the addres of next instuction, if all 
bits of the accumulator is one, otherwise it go to next instruction. No flags are affected.
************************************************************************************************************************/
/*
Represents the follow 8051's instruction:
JNZ rel - make a relative jump if then bit if the carry is one
*/
JZ(jump)=
	PRE
	    jump : ROM_ADDR
	THEN
	    IF byte_uchar(mem(ACC))= 1 
	    THEN 
	        pc:= PC_INCREMENT(pc,jump+1)
	    ELSE 
	        pc:=PC_INCREMENT(pc,1)	        
	 	END
	END
;




/************************************************************************************************************************
The LCALL instruction loaded the PC with the subroutine locate at the indicated address. It put the address of next
instruction in the stack. For this, it increment the PC by 1 and the SP by 2, putting the first low bytes of PC first
************************************************************************************************************************/
/*
Represents the follow 8051's instruction:
LCALL addr16 - calls a subroutine in the passed address
*/
LCALL(rom_addr)=
PRE
    rom_addr : ROM_ADDR
THEN
    ANY upper_pc,lower_pc
	WHERE 
	    upper_pc: BYTE &
	    lower_pc:BYTE & 
	    upper_pc,lower_pc=usint_byte(PC_INCREMENT(pc,1))
	THEN
	    push_two(lower_pc,upper_pc)
	    || pc:=rom_addr
	END
END
;

/************************************************************************************************************************
The LJMP causes an unconditional branch to the indicate address. No flags are affected in this instruction
Obs: I think that this instruction can be used for replace the INIT instruction, because when I calls LJMP(0) 
have the same effects that calls INIT
************************************************************************************************************************/
/*
Represents the follow 8051's instruction:
LJMP addr16 - branch to the indicate address
*/

LJMP(rom_addr)=
PRE
    rom_addr : ROM_ADDR
THEN
    pc:= rom_addr
END
;

/************************************************************************************************************************
The set of MOV the by far the most flexible operation set, this permit fiveteen combination mode. This copie the value
indicate for second operation in the first operation.
************************************************************************************************************************/

/*
Represent the follows 8051's instructions
MOV A, Rn - Copie the value of registe Rn to the accumulator
MOV A, direct - Copie the value of passed addres ( direct) to the accumulator
MOV Rn, A - Copie the value of the accumulator to register Rn
MOV Rn,direct - Copie the value of the direct addres passed to the register Rn
MOV direct, A - Copie the value of accumulator to the direct addres passed
MOV direct, Rn - Copie the value of the register Rn to the direct address passed
MOV direct, direct - Copie the value of the second direct addres to the first direct address

Obs : 
-	The instruction mov A,A isn't available in 8051, but the function mov A,ACC , mov ACC,ACC and mov ACC,A are 
	available. In this specification both mov A,A and other are available
- 	The instruction mov Rn,Rn isn't available in 8051, but here it is available
*/
MOV(direct_d,direct_s)=
	PRE 
	    direct_d:RAM_ADDR & direct_s:RAM_ADDR 
	THEN
	    addrSetDirect(direct_d,mem(direct_s)) || pc:= PC_INCREMENT(pc,1)
	END
;

/*
Represent the follows 8051's instructions:
MOV A,data - Copie the constant passed to the accumulator
MOV direct,data - Copie the constant passed to the direct address indicate
MOV Rn,data - Copie the constant passed to the register Rn
*/
MOVD(direct,data)=
	PRE 
	    direct:MEM_ADDR & data:UCHAR
	THEN
	    addrSetDirect(direct,uchar_byte(data)) || pc:= PC_INCREMENT(pc,1)
	END
;

/*
Represent the follows 8051's instructions:
MOV A,@Ri- Copie the value of the addres indicate for register to the accumulator
MOV direct,@Ri - Copie the value of the addres indicate for register to the direct address

Obs:
	-The instruction mov Rn,@Ri isn't available in 8051, but here it is available
*/
MOVDI(direct,Rn)=
	PRE 
	    direct:MEM_ADDR & Rn:RAM_ADDR & (Rn=R1 or Rn=R2) & byte_uchar(mem(Rn)):RAM_ADDR
	THEN
	    ANY src 
	    WHERE 
	        src:RAM_ADDR & 
	        src =byte_uchar(mem(Rn))	        
	    THEN
	        addrSetDirect(direct,mem(src)) || 
	        pc:= PC_INCREMENT(pc,1)
	    END
	END
;

/*
Represent the follows 8051's instructions:
MOV @Ri, A - Copie the value of the accumulator to the address indicate for the register passed
MOV @Ri, direct - Copie the value of the direct addres to the address indicate for the register passed

Obs:
	-The instruction mov @Ri,Rn isn't available in 8051, but here it is available
*/
MOVI(Rn,direct)=
	PRE 
	    Rn:RAM_ADDR &
	    	(Rn=R1 or Rn=R2) & 
	    byte_uchar(mem(Rn)):RAM_ADDR & 
	    direct:MEM_ADDR
	THEN	    
	    addrSetIndirect(Rn,mem(direct))||
	    pc:= PC_INCREMENT(pc,1)
	END
;

/*
Represent the follow 8051's instruction:
MOV @Ri, data - Copie the value of the constant to the address indicate for the register passed
*/
MOVID(Rn,data)=
	PRE 
	    Rn:RAM_ADDR &
	    	(Rn=R1 or Rn=R2) & 
	    byte_uchar(mem(Rn)):RAM_ADDR & 
	    data:UCHAR
	THEN	    
	    addrSetIndirect(Rn,uchar_byte(data))||
	    pc:= PC_INCREMENT(pc,1)
	END
;

/************************************************************************************************************************
The set of MOV with bit, copie the bit indicate for the second operation into the location indicate for the first
operation. One of bits passed must be the Carry. No flags are affected in this instruction
************************************************************************************************************************/
/*
Represent the follows 8051's instructions:
MOV bit, C - Copie the value of the carry to the bit passed
MOV C, bit - Copie the value of the bit passed to the carry 

Obs: 
	- The fucntion MOV C,C is available both in 8051 and here
*/
MOVB(dest,src)=
	PRE
	  dest : BIT_ADDRESS & 
	  src : BIT_ADDRESS & 
	  (dest=CY or src=CY)
	THEN
	    ANY addr,ind
	    WHERE
	        addr:MEM_ADDR &
	        ind:BYTE_INDEX &
	        addr,ind =src
	    THEN
	        IF BIT_GET(src,mem)=1 
	    	THEN 
	    	    bitSet(addr,ind)
	    	ELSE 
	    	    bitClear(addr,ind)
	    	END
	    END||
	    pc := PC_INCREMENT(pc,1)	    
	END
;

/************************************************************************************************************************
The MUL instruction perform the multiplication between the accumulator and the B register, The high-order bits of result
is leave in the B register and the low-order bits is leave in the accumulator. the overflow is set if the result is
greater then 255, otherwise then is clean. The carry bit is clean in every case
************************************************************************************************************************/
/*
Represent the follows 8051's instructions:
MUL AB - mutiplication between the accumulator and the B register
*/
MUL_AB=
BEGIN
    ANY aa,bb,ov,pp 
    WHERE 
        aa:BYTE & 
        bb:BYTE & 
        ov:BIT & 
        pp:BIT & 
        bb,aa,ov=mult(byte_uchar(mem(AA)),byte_uchar(mem(BB))) & pp=parity(aa)
    THEN
        update_div(aa,bb,ov,pp)
    END
END
;




/************************************************************************************************************************
This instruction do nothink more tha jump to next instruction
************************************************************************************************************************/
/*
Represent the follows 8051's instructions:
NOP
*/
NOP=
	BEGIN
	    pc:=PC_INCREMENT(pc,1)
	END
;
/************************************************************************************************************************
The set of ORL instruction performace a bitwise logical-OR operation between the indicates bytes and leave the result
into the byte indicate for the first operation. No flags are affected in this instruction
************************************************************************************************************************/
/*
Represent the follows 8051's instructions:
ORL A,Rn - bitwise logical-OR between the accumulator and the register passed
ORL A,direct - logical-OR between the accumulator and the address passed
ORL direct,A - logical-OR between the direct address and the accumulator

Obs: 
	-ORL A,A and and ORL ACC,ACC are fucntion no available in 8051, but both ORL A,ACC and ORL ACC,A. In this
	 specification all four function are available. The some problem occur in ORL Rn,A instruction the are no
	 available in 8051, but here is.
	
*/
ORL(dest,src)=
PRE
	dest:MEM_ADDR  &  
	src:MEM_ADDR  &  
	(dest=ACC or src=ACC)
THEN
    addrSetDirect(dest,ior(mem(dest),mem(src)))||pc:=PC_INCREMENT(pc,1)
END
;

/*
Represent the follows 8051's instructions:
ORL A,#data - bitwise logical-OR between the accumulator and the data passed
ORL direct,#data - logical-OR between the direct address and the data passed

Obs: - The fucntion ORL Rn,#data isn't available in 8051, but here is.
*/
ORLD(dest,data)=
PRE
	dest:MEM_ADDR  &  data:UCHAR
THEN
    addrSetDirect(dest,ior(mem(dest),uchar_byte(data)))||pc:=PC_INCREMENT(pc,1)
END
;

/*
Represent the follow 8051's instruction:
ORL A,@Ri - bitwise logical-OR between the accumulator and the byte indicate for the register passed
*/
ORLI(dest,Rn)=
PRE
	dest:MEM_ADDR  &  
	Rn:RAM_ADDR & 
		(Rn = R0 or Rn= R1) & 
	byte_uchar(mem(Rn)):RAM_ADDR
THEN
    ANY src 
    WHERE 
        src:RAM_ADDR & 
        src = byte_uchar(mem(Rn))
    THEN
        addrSetDirect(dest,ior(mem(dest),mem(src)))||pc:=PC_INCREMENT(pc,1)
    END
END
;

/*
Represent the follows 8051's instructions:
ORL C,bit - logical or between the carry and the bit passed, leave the result in the carry
ORL bit,C - logial or between the carry and the bit passed, leave the result in the bit address passed as first operation
*/
ORLB(dest,src)=
PRE
	dest:BIT_ADDRESS & 
	src:BIT_ADDRESS & 
		(src=CY or dest = CY)
THEN
    IF BIT_GET(src,mem)=1 
    THEN 
        ANY addr,ind
        WHERE
            addr:MEM_ADDR &
            ind:BYTE_INDEX &
            addr,ind=src
        THEN
            bitSet(addr,ind)
        END
    END ||
    pc := PC_INCREMENT(pc,1)    
END
;
/************************************************************************************************************************
The POP instruction copie the byte in the address indicate for SP (stack point) to the address passed and decrements 
the SP by 1.
************************************************************************************************************************/
/*
Represent the follows 8051's instructions:
POP direct - copie the value in the top of stack for the passed address
*/
POP(direct)=
PRE
    direct: MEM_ADDR
THEN
    pop(direct)
END
;
/************************************************************************************************************************
The RET function pops the high and low -order bytes of the PC successively from the stack, decrementing the stack by two
************************************************************************************************************************/
/*
Represent the follow 8051's instruction:
RET - 
*/
RET=
BEGIN
    ANY sp1,sp2 
    WHERE 
        sp1:RAM_ADDR & 
        sp2:RAM_ADDR & 
        sp1=byte_uchar(mem(SP)) & 
        sp2=SP_DECREMENT(mem(SP),1)        
    THEN 
        pc:=bv16_usint(GET_RET(sp1,sp2,mem))||
        decrement_sp(1)
    END    
END
;

/************************************************************************************************************************
The PUSH instruction incement the stack point (SP), and copie the content of the passed addres to the address indicate
for the SP register. No flags are affected in this operation
************************************************************************************************************************/
/*
Represent the follow 8051's instruction:
PUSH direct - Put the contents of the address into the stack
*/
PUSH(direct)=
PRE
    direct : MEM_ADDR
THEN
    push(mem(direct))||pc:=PC_INCREMENT(pc,1)
END
;

/************************************************************************************************************************
The RL represent the rotate left of the accumulator bits, bit 7 is rotated into the bit 0, bit 6 into the 1, and so on
************************************************************************************************************************/
/*
Represent the follow 8051's instruction:
RL A - rotate left the accumulator bytes
*/
RL(acc)=
PRE
    acc=ACC
THEN
    addrSetDirect(ACC,rotateleft(mem(ACC)))||
    pc:=PC_INCREMENT(pc,1)
END
;


/************************************************************************************************************************
The RR represent the rotate right of the accumulator bits, bit 0 is rotated into the bit 7, bit 1 into the 6, and so on
************************************************************************************************************************/
/*
Represent the follow 8051's instruction:
RR A - rotate right the accumulator bytes
*/
RR(acc)=
PRE
    acc=ACC
THEN
    addrSetDirect(ACC,rotateright(mem(ACC)))||
    pc:=PC_INCREMENT(pc,1)
END
;

/************************************************************************************************************************
The SETB sets the bit indicate to one. No flags are affect in this operation
************************************************************************************************************************/
/*
Represent the follow 8051's instruction:
SETB C - set the passed bit
SETB bit - set the carry
*/

SETB(bit)=
   PRE bit : BIT_ADDRESS  
   THEN  
       ANY addr,ind
       WHERE
           addr:MEM_ADDR &
           ind:BYTE_INDEX &
           addr,ind=bit
       THEN
           bitSet(addr,ind)
       END||
       pc:= PC_INCREMENT(pc,1)
   END
;


/************************************************************************************************************************
The SJMP braches unconditionally to the addrress computed by the adds of increments pc by 1 and the jump passed
************************************************************************************************************************/
/*
Represent the follow 8051's instruction:
SJMP rel - jump the numbers of address passed
*/
SJMP(jump)=
	PRE 
	    jump : ROM_ADDR
	THEN
	    pc:=PC_INCREMENT(pc,jump+1)			
	END
	
	
;

/************************************************************************************************************************
The set of SUBB instructions substracts the indicates bytes and the accumulator together with accumulator leaving the
result into the accumulator. The flags carry, aux-carry e overflow are affected.
************************************************************************************************************************/
/*
Represent the follows 8051's instructions:
SUBB A,direct - substract ac and direct address together with carry
SUBB A,Rn - substract ac and register Rn together with carry

Obs: - The instructions SUBB A,A, SUBB ACC,ACC, SUBB ACC,A aren't available in 8051, but here are .
*/
SUBB(ac,src)=
	PRE
	  ac=ACC & src:MEM_ADDR
	THEN
	    ANY 
	        acc,Pa,Cy,Ac,Ov
	  	WHERE 
	      	acc: UCHAR & 
	      	Pa:BIT & 
	      	Cy:BIT & 
	      	Ac:BIT & 
	      	Ov:BIT & 
	      	acc,Pa,Ov,Ac,Cy =sub(byte_uchar(mem(ACC)), byte_uchar(mem(src)),BIT_GET(CY,mem))	   
	  	THEN 
	      	update(acc,Pa,Ov,Ac,Cy)
   	  	END || pc:= PC_INCREMENT(pc,1)
	END 
;

/*
Represent the follow 8051's instruction:
SUBB A,@Ri - substract ac and adrress indicate for register passed together with carry
*/
SUBBI(ac,Rn)=
	PRE
	    ac=ACC & 
	    Rn:MEM_ADDR & 
	    (Rn = R0 or Rn = R1) &
	     byte_uchar(mem(Rn)): RAM_ADDR
        
	THEN
	    ANY 
            acc,Pa,Cy,Ac,Ov,src
        WHERE 
	   		acc: UCHAR & 
	   		Pa:BIT & 
	   		Cy:BIT &
	   		Ac:BIT & 
	   		Ov:BIT & 
	   		src : RAM_ADDR &  
	   		src = byte_uchar(mem(Rn)) & 
	   		acc,Pa,Ov,Ac,Cy =sub(byte_uchar(mem(ACC)), byte_uchar(mem(src)),BIT_GET(CY,mem))
	  	THEN 
   	    	update(acc,Pa,Ov,Ac,Cy)
		END||
   	    	pc:=PC_INCREMENT(pc,1)
	END 
;
/*
Represent the follow 8051's instruction:
SUBB A,#data - substract ac and data passed together with carry
*/
SUBBD(dest,data)=
	PRE
	  	dest=ACC & data : UCHAR
	THEN
	    ANY 
            acc,Pa,Cy,Ac,Ov
	  	WHERE 
	    	acc: UCHAR & 
	    	Pa:BIT & 
	    	Cy:BIT & 
	    	Ac:BIT & 
	    	Ov:BIT & 
	    	acc,Pa,Ov,Ac,Cy =sub( byte_uchar(mem(ACC)),data,BIT_GET(CY,mem))
	  	THEN 
	    	update(acc,Pa,Ov,Ac,Cy)
      	END ||
      	pc:=PC_INCREMENT(pc,1)
	END 

;

/************************************************************************************************************************
The SWAP instructions interchanges the low and high - order nibbles of the accumulator. No flags are affected
************************************************************************************************************************/
/*
Represent the follow 8051's instruction:
SWAP A - interchanges the low and high order nibbles of the accumulator
*/
SWAP(acc)=
	PRE
		acc=ACC
	THEN
  	  	addrSetDirect(ACC,swap(mem(ACC)))
	END
;

/************************************************************************************************************************
The set of XCH instructions load the accumulator with the content of the indicate byte variable and at the some time
copie the contents of the accumulator to the variable byte.
************************************************************************************************************************/
/*
Represent the follows 8051's instructions:
XCH A,direct - external change between the accumulator and the direct address
XCH A,Rn - external change between the accumulator and the register Rn

Obs: 
 	-The instructions XCH A,A, XCH ACC,A and XCH ACC,A aren't available in the 8051, but here are.
*/
XCH(acc,direct)=
PRE
    acc=ACC & direct:MEM_ADDR
THEN
    exChange(ACC,direct)
END
;

/*
Represent the follow 8051's instruction:
XCH A,@Ri- external change between the accumulator and the address indicate of the register
*/
XCHI(acc,Rn)=
PRE
    acc=ACC & Rn:RAM_ADDR & (Rn =0 or Rn=1) & byte_uchar(mem(Rn)):RAM_ADDR
THEN
    ANY src WHERE src:RAM_ADDR & src=byte_uchar(mem(Rn)) 
   	THEN
    	exChange(ACC,src)
    END
END
;

/************************************************************************************************************************
The set of XRL instructions performace the bitwise logical-XOR between the bytes variables indicates. No flags are 
affected in this instruction
************************************************************************************************************************/
/*
Represent the follow 8051's instruction:
XRL A,Rn- bitwise logical-XOR between the accumulator and the register Rn
XRL A,direct- bitwise logical-XOR between the accumulator and the contents of the address passed
XRL direct,A- bitwise logical-XOR between the address passed and the accumulator

Obs: the instruction XRL Rn,direct ,XRL Rn,A and XRL Rn,Rn are not available in 8051, but here it are available.
In future version this problem will be correct
*/
XRL(dest,src)=
PRE
    dest:MEM_ADDR & src:MEM_ADDR & (dest=ACC or src=ACC)
THEN
	addrSetIndirect(dest,xor(mem(dest),mem(src)))
END
;

/*
Represent the follow 8051's instruction:
XRL A,@Ri- the bitwise logical-XOR between the accumulator and the contents of the address indicate by register
*/
XRLI(acc,Rn)=
PRE
    acc = ACC & Rn:RAM_ADDR & (Rn=R0 or Rn=R1) & byte_uchar(mem(Rn)):RAM_ADDR
THEN
    ANY src WHERE src:RAM_ADDR & src= byte_uchar(mem(Rn))
    THEN
		addrSetDirect(ACC,xor(mem(ACC),mem(src)))
	END
END
;


/*
Represent the follow 8051's instruction:
XRL A,#data- the bitwise logical-XOR between the accumulator and the constant passed
XRL direct,#data- the bitwise logical-XOR between the address passed and the constant passed

Obs: In 8051 no exist the instruction XRL Rn,#data, but in the specification it is implemented. In next versions this 
problem will be corrects
*/
XRLD(dest,data)=
PRE
    dest:MEM_ADDR & data:UCHAR
THEN
	addrSetIndirect(dest,xor(mem(dest),uchar_byte(data)))
END

END






/*



;

*/
/*D�vidas


RET =
   PRE 
    mem(SP) : INSTRUCTION
   THEN 
    pc:=pop  
   END
;

*/