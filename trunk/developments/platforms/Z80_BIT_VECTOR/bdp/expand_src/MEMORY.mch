/* MEMORY
 * Author: valerio
 * Creation date: sex out 31 2008
 */
MACHINE
   MEMORY

SEES
TYPES , ALU

ABSTRACT_VARIABLES

   mem ,
   stack

INVARIANT
   stack : BV16 >-> BYTE &
   mem : BV16 >-> BYTE



ASSERTIONS

   dom ( stack ) = BV16 &
   ran ( stack ) <: BYTE &
   ran ( mem ) <: BYTE &
   dom ( mem ) = BV16


INITIALISATION
/*   rgs8 :: id_reg_8 -->> SCHAR || */
   mem :: BV16 >-> BYTE ||
   stack :: BV16  >-> BYTE

  OPERATIONS

  updateMem ( NewValues ) =
  PRE NewValues : BV16 >-> BYTE
    THEN
      mem := mem <+ NewValues
  END ;

  updateAddressMem ( address , value ) =
        PRE   address : BV16 & value : BYTE & mem <+ { address |-> value } : BV16 >-> BYTE
        THEN
                mem := mem <+ { address |-> value }
        END ;

  updateStack ( NewValues ) =
    PRE NewValues : BV16 >-> BYTE
        THEN
      stack := stack <+ NewValues
        END ;

    updateAddressStack ( address , value ) =
        PRE address : BV16 & value : BYTE & stack <+ { address |-> value } : BV16 >-> BYTE
    THEN
                stack ( address ) := value
        END ;

   pop ( sp ) =
        PRE sp : BYTE
        THEN
             stack := { SSHORTINT_TO_BV16 ( BV16_TO_SSHORTINT ( sp ) - 1 ) } <<| stack
        END


END
