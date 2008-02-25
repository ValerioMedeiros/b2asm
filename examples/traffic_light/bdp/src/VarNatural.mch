MACHINE
   VarNatural
ABSTRACT_VARIABLES
   value
INVARIANT
   value : NATURAL
INITIALISATION
   value := 0

OPERATIONS

set ( new_value ) =
   PRE
   new_value : NATURAL
   THEN
   value := new_value
   END ;

res <-- get =
        res := value
END
