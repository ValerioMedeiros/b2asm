MACHINE
   swap
VARIABLES
   a, b
INVARIANT
   a : INTEGER & b : INTEGER & a = b
INITIALISATION
   a, b := 0, 0
OPERATIONS
   run = 
   BEGIN
      a := b || 
      b := a
   END
END