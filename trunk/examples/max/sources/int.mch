MACHINE
  int
VARIABLES
  value
INVARIANT
  value : INTEGER
INITIALISATION
  value :: INTEGER
OPERATIONS
  v <-- get = 
  BEGIN 
    v := value
  END;
  set (v) =
  PRE v : INTEGER
  THEN
    value := v
  END
END