MACHINE
  move3
CONCRETE_VARIABLES
  reg
INVARIANT
  reg : (1..3)--> NAT
INITIALISATION
  reg :: (1..3)--> NAT
OPERATIONS
  clr (i) =
    PRE i : (1..3) THEN
      reg(i) := 0
    END
  ;
  move (i, j) =
    PRE i : (1..3) & j : (1..3) & i /= j THEN
      reg(j) := reg(i)
    END
END