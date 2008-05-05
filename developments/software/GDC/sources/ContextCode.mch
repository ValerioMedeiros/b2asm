MACHINE
	ContextCode

SEES
	Context

CONSTANTS
	STATE_CODE,
	CONDUCTION_CODE,
	DOORS_CODE,
	COMMAND_CODE,
	BOOL_CODE,
	select_command_code

PROPERTIES
	STATE_CODE : STATE --> NATURAL &
	STATE_CODE = {NONE |-> 0, LEADER |-> 1, OPPOSITE |-> 2} &
	CONDUCTION_CODE : CONDUCTION --> NATURAL &
	CONDUCTION_CODE = {CONDUCTION_OFF |-> 0, MANUAL |-> 1, CONTROLLED |-> 2} &
	DOORS_CODE : DOORS --> NATURAL &
	DOORS_CODE = {OPENED |-> 0, CLOSED |-> 1 } &
	COMMAND_CODE : COMMAND --> NATURAL &
	COMMAND_CODE = {NO_COMMAND |-> 0, OPEN_DOORS |-> 1, CLOSE_DOORS |-> 2} &
	BOOL_CODE : BOOL --> NATURAL &
	BOOL_CODE = {FALSE |-> 0, TRUE |-> 1} &
  	select_command_code : (NATURAL * NATURAL * NATURAL * NATURAL) --> NATURAL &
  	! (slow, close, open, mem) .
      		(slow : NATURAL & close : NATURAL & open : NATURAL & mem : NATURAL =>
       			(((slow = BOOL_CODE(FALSE) or close = BOOL_CODE(TRUE)) => 
			       select_command_code(slow, close, open, mem) = COMMAND_CODE(CLOSE_DOORS)) &
        		 ((slow = BOOL_CODE(TRUE) & close = BOOL_CODE(FALSE) & open = BOOL_CODE(TRUE)) => 
            			select_command_code(slow, close, open, mem) = COMMAND_CODE(OPEN_DOORS)) &
			 ((slow = BOOL_CODE(TRUE) & close = BOOL_CODE(FALSE) & open = BOOL_CODE(FALSE)) => 
				select_command_code(slow, close, open, mem) = mem)))
END

