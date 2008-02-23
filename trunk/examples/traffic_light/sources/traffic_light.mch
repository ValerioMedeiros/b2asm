MODEL
   traffic_light
VARIABLES
   state
INVARIANT
   state :  0..2  /*{red,green,yellow} */

INITIALISATION
   state := 0

OPERATIONS

advance = 
BEGIN
  state := (state + 1 ) mod 3
END


END