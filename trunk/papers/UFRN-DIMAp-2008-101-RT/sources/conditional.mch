MACHINE
  conditional
SEES types
VARIABLES
  st
INVARIANT
  st : POW(uint32)
INITIALISATION
  st := {}
OPERATIONS
  add (v) =
   PRE v : uint32 & v /: st THEN st := st \/ {v} END;
  res <-- largest =
    PRE st /= {} THEN res := max(st) END
END