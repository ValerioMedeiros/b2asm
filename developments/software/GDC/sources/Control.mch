MACHINE
	Control

SEES
	Context,
	BoolXor

CONSTANTS
	conduction_p,
	select_command

PROPERTIES
/* conduction_p defines whether the current cabin system is in normal
conduction conditions, and may thus accept commands */
	conduction_p : (BOOL * BOOL * BOOL) --> BOOL &
	conduction_p = %(leader,manual,controlled).
	(leader : BOOL & manual : BOOL & controlled : BOOL |
		bool(leader = TRUE & xor(manual, controlled) = TRUE)) &
	!(l,m,c). (l : BOOL & m : BOOL & c : BOOL =>
		conduction_p(l, m, c) = bool(l = TRUE & xor(m, c) = TRUE)) &
/* select_command defines the command that must be issued by the CGP
controller as a function of the train speed, close and open input
variables and the last memorized command */
  	select_command : (BOOL * BOOL * BOOL * COMMAND) --> COMMAND &
  	! (slow, close, open, mem) .
      		(slow : BOOL & close : BOOL & open : BOOL & mem : COMMAND =>
       			(((slow = FALSE or close = TRUE) => 
			       select_command(slow, close, open, mem) = CLOSE_DOORS) &
        		 ((slow = TRUE & close = FALSE & open = TRUE) => 
            			select_command(slow, close, open, mem) = OPEN_DOORS) &
			 ((slow = TRUE & close = FALSE & open = FALSE) => 
				select_command(slow, close, open, mem) = mem)))

ASSERTIONS
	conduction_p(TRUE, TRUE, FALSE) = TRUE;
	conduction_p(TRUE, FALSE, TRUE) = TRUE;
	conduction_p(TRUE, TRUE, TRUE) = FALSE;
	conduction_p(TRUE, FALSE, FALSE) = FALSE;
	! (mm, cc) . (mm : BOOL & cc : BOOL => conduction_p(FALSE, mm, cc) = FALSE);
	! (cc, oo, mm) . (cc : BOOL & oo : BOOL & mm : COMMAND => select_command(FALSE, cc, oo, mm) = CLOSE_DOORS);
  	! (ss, oo, mm) . (ss : BOOL & oo : BOOL & mm : COMMAND => select_command(ss, TRUE, oo, mm) = CLOSE_DOORS);
  	! (mm) . (mm : COMMAND => select_command(TRUE, FALSE, TRUE, mm) = OPEN_DOORS);
  	! (mm) . (mm : COMMAND => select_command(TRUE, FALSE, FALSE, mm) = mm)

END