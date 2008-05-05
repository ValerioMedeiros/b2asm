MACHINE sequence
SEES types
VARIABLES a, b
INVARIANT a : uint32 & b : uint32
INITIALISATION
   a :: uint32 || b :: uint32
OPERATIONS
   run = 
   BEGIN
      a := b || 
      b := a
   END
END