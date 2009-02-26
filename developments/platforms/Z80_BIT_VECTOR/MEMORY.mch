/* MEMORY
* Author: valerio
* Creation date: sex out 31 2008
*/
MACHINE
   MEMORY
   
SEES
   ALU,
   BIT_DEFINITION ,
   BIT_VECTOR_DEFINITION,
   BYTE_DEFINITION,
   BV16_DEFINITION,

   UCHAR_DEFINITION,
   SCHAR_DEFINITION,
   SSHORT_DEFINITION,
   USHORT_DEFINITION,
   POWER2 
CONSTANTS
   RESERVED_ADDRESS
PROPERTIES
   RESERVED_ADDRESS = 0..16384   

ABSTRACT_VARIABLES
   
   mem,
   stack 
   
INVARIANT

   mem : BV16 --> BYTE  &
   stack : BV16 --> BYTE &    
   stack <<: mem
   
   
ASSERTIONS
   
   dom(stack)= BV16&
   ran(stack)<: BYTE& 
   ran(mem) <: BYTE &
   dom(mem) = BV16 &
   !(address,value).( address : BV16 & value : BYTE =>    mem <+ { address |-> value } : BV16 --> BYTE ) &
   !(address,value).( address : BV16 & value : BYTE =>    stack <+ { address |-> value } : BV16 --> BYTE ) 

   
INITIALISATION
   mem :: BV16 --> BYTE ||
   stack :: BV16  --> BYTE 
   
   
OPERATIONS
   
   updateMem(NewValues) =
   PRE NewValues : BV16 --> BYTE  &  dom(NewValues) /\ dom(stack) = {} 
   THEN
      mem:= mem <+ NewValues 
   END;
  
   updateAddressMem(address, value) =
   PRE   address : BV16 & value : BYTE & 
				 dom({ address |-> value}) /\ dom(stack) = {}  & not(address|->value: stack)
   THEN
      mem:= mem <+ { address |-> value }
   END;
   
   updateStack(NewValues) =
   PRE NewValues : BV16 --> BYTE  
   THEN
      stack:= stack <+ NewValues ||       mem:= mem <+ NewValues 
   END;
   
   updateAddressStack(address, value) =
   PRE address : BV16 & value : BYTE   
   THEN
      stack(address):=value ||       mem:= mem <+ { address |-> value }
   END;
   
   pop(sp) =
   PRE sp : BV16
   THEN
      stack := { dec_BV16( sp) } <<| stack
   END
   
   
END