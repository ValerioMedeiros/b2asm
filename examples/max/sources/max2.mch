MACHINE
  max2
VARIABLES
  a, b, m
INVARIANT
  a : INT & b : INT & m : INT & 
  (m = a or m = b) & m >= a & m >= b
INITIALISATION
  a, b, m := 0, 0, 0
OPERATIONS
  run (v) =
    PRE
      v : INT & v >= 0
    THEN
      a := v ||
      b := a ||
      m := max ({a, v})
    END
END