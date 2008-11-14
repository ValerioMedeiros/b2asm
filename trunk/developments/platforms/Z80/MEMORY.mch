/* MEMORY
 * Author: valerio
 * Creation date: sex out 31 2008
 */
MACHINE
   MEMORY

SEES
TYPES,ALU


ABSTRACT_VARIABLES

  
   mem,
   stack /*, i,r */ 



/*DEFINITIONS
 v_BC == SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0));
 v_DE == SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0));
 v_HL == SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0));
 v_AF == SCHAR_TO_SSHORTINT(rgs8(a0),rgs8(f0));

 v_9BC0 == mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(b0),rgs8(c0))));
 v_9DE0 == mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(d0),rgs8(e0))));
 v_9HL0 == mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(h0),rgs8(l0))));
 v_9AF0 == mem(SSHORTINT_TO_USHORTINT(SCHAR_TO_SSHORTINT(rgs8(a0),rgs8(f0))));
*/




INVARIANT
   stack : USHORTINT  --> SCHAR & 
   mem : USHORTINT --> SCHAR 



/*

ASSERTIONS

  dom(stack)= USHORTINT&
   ran(stack)<: SCHAR& 
   ran(mem) <: SCHAR &
   dom(mem) = USHORTINT &
   ran(rgs8) <: SCHAR &
   dom(rgs8) = id_reg_8&
   INSTRUCTION_NEXT(0) = 1 &
   INSTRUCTION_NEXT(1) = 2 &
   INSTRUCTION_NEXT(2) = 3 &
   INSTRUCTION_NEXT(3) = 4 &
   INSTRUCTION_NEXT(4) = 5 &
   INSTRUCTION_NEXT(5) = 6 &
   INSTRUCTION_NEXT(6) = 7 &
   INSTRUCTION_NEXT(7) = 8 &
   INSTRUCTION_NEXT(8) = 9 &
   INSTRUCTION_NEXT(9) = 10&
   INSTRUCTION_NEXT(10) = 11&
   INSTRUCTION_NEXT(11) = 12&
   INSTRUCTION_NEXT(12) = 13&
   INSTRUCTION_NEXT(13) = 14&

   !(vec,in0).(vec : BYTE & in0 : 0..7 
   =>  bitget(BYTE_TO_SCHAR(vec),in0) = vec(in0) ) &

   v_9BC0 : SCHAR  & mem(SSHORTINT_TO_USHORTINT( v_9BC0)) : USHORTINT  &
   v_9DE0 : SCHAR  & mem(SSHORTINT_TO_USHORTINT( v_9DE0)) : USHORTINT &
   v_9HL0 : SCHAR  & mem(SSHORTINT_TO_USHORTINT( v_9HL0)) : USHORTINT &
   v_9AF0 : SCHAR  & mem(SSHORTINT_TO_USHORTINT( v_9AF0)) : USHORTINT  */


INITIALISATION
/*   rgs8 :: id_reg_8 -->> SCHAR || */
   mem :: USHORTINT --> SCHAR ||
   
   stack :: USHORTINT  --> SCHAR 
   
  OPERATIONS
      
  updateMem(NewValues) =
  PRE NewValues : USHORTINT --> SCHAR 
    THEN
      mem:= mem <+ NewValues
  END;
  
  updateAddressMem(address, value) =
  PRE address : USHORTINT & value : SCHAR 
    THEN
      mem(address):=value
  END;
  
  updateStack(NewValues) =
    PRE NewValues : USHORTINT --> SCHAR 
      THEN
      stack:= stack <+ NewValues
	END;
	
    updateAddressStack(address, value) =
  PRE address : USHORTINT & value : SCHAR 
    THEN
      stack(address):=value
    END;

   pop(sp) =
	PRE sp : USHORTINT
	THEN
	     stack := {sp - 1} <<| stack
	END
	

END