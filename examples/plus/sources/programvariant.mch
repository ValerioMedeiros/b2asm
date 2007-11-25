MACHINE
   programvariant
CONSTANTS
   pgvar, pgsize, loopaddr, loopsize

PROPERTIES
   pgsize : NATURAL & loopaddr : NATURAL & loopsize : NATURAL & pgvar : NATURAL * NATURAL * NATURAL +-> NATURAL &
   pgsize = 8 & loopsize = 5 & loopaddr = 2 &
   ! (pc, loopmax, loopvar).
	(pc : NATURAL & loopmax : NATURAL & loopvar : NATURAL =>
		(pc = 7 or pc = 8 => pgvar(pc, loopmax, loopvar) = pgsize - pc) &
                ((pc = 0 or pc = 1) => 
			pgvar(pc, loopmax, loopvar) = pgsize - pc + loopsize * loopmax) &
                ((pc = 2 or pc = 3 or pc = 4 or pc = 5 or pc = 6) => 
			pgvar(pc, loopmax, loopvar) = pgsize - loopaddr + loopsize * (loopmax -loopvar) - (pc - loopaddr)))
END