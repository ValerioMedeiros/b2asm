MACHINE
   plus
VARIABLES
   a, b, s
INVARIANT
   a : NATURAL & b : NATURAL & s : NATURAL &
   s = a + b
INITIALISATION
   a, b, s := 0, 0, 0
OPERATIONS
   run =
     BEGIN
        s := a + b
     END
END