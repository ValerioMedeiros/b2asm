MACHINE
	BoolXor

CONSTANTS
	xor

PROPERTIES
	xor : (BOOL * BOOL) --> BOOL &
	! (a, b).(a : BOOL & b : BOOL => xor(a, b) = bool(a /= b))

ASSERTIONS
	xor(TRUE, FALSE) = TRUE;
	xor(FALSE, TRUE) = TRUE;
	xor(TRUE, TRUE) = FALSE;
	xor(FALSE, FALSE) = FALSE

END