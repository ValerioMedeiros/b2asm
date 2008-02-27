MACHINE
  ram
SEES
  types
CONCRETE_VARIABLES
  mem, pc, end
INVARIANT
  mem : (NATURAL --> uint32) & pc : NATURAL & end : NATURAL & pc <= end
INITIALISATION
  mem :: (NATURAL --> uint32) || pc := 0 || end :: NATURAL
OPERATIONS
  init (sz) =
    PRE sz : NATURAL THEN pc := 0 || end := sz END;
  nop =
    PRE pc + 1 <= end THEN pc := pc + 1 END;
  set (addr, val) =
    PRE addr : NATURAL & val : uint32 & pc + 1 <= end THEN
      mem (addr) := val || pc := pc + 1
    END;
  inc (addr) =
    PRE addr : NATURAL & pc + 1 <= end & mem(addr) < MAXINT THEN 
      mem (addr) := mem (addr) + 1 || pc := pc + 1
    END;
  move (src, dest) =
    PRE src : NATURAL & dest : NATURAL & pc + 1 <= end THEN 
      mem (dest) := mem (src) || pc := pc + 1
    END;
  testgt (addr1, addr2) =
    PRE addr1 : NATURAL & addr2 : NATURAL &
      (mem(addr1) > mem(addr2) => pc + 1 <= end) &
      (mem(addr1) <= mem(addr2) => pc + 2 <= end)
    THEN
      IF mem(addr1) > mem(addr2) THEN pc := pc + 1 ELSE pc := pc + 2 END
    END;
  testeq (addr1, addr2) =
    PRE addr1 : NATURAL & addr2 : NATURAL &
      (mem(addr1) = mem(addr2) => pc + 1 <= end) &
      (mem(addr1) /= mem(addr2) => pc + 2 <= end)
    THEN
      IF mem(addr1) = mem(addr2) THEN pc := pc + 1 ELSE pc := pc + 2 END
    END;
  goto (dest) =
    PRE dest : NATURAL & dest <= end THEN 
      pc := dest
    END;
  res <-- get_data (addr) =
    PRE addr : NATURAL THEN res := mem(addr) END;
  res <-- get_pc = res := pc
END