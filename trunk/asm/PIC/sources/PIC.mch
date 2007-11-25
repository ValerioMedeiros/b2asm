MACHINE
   PIC

SEES
   TYPES

USES
   ALU

VARIABLES
   mem,
   w,
   z, 
   c,
   pc,
   stack,
   sp

INVARIANT
   mem : REGISTER --> WORD &
   w : WORD &
   z : BOOL &
   c : BOOL &
   pc : INSTRUCTION &
   sp : NATURAL &
   stack : NATURAL +-> INSTRUCTION &
   dom(stack) = 0..(sp-1)

ASSERTIONS
   ran(mem) <: WORD &
   dom(mem) = REGISTER

INITIALISATION
   w :: WORD ||
   mem :: REGISTER --> WORD ||
   z :: BOOL ||
   c :: BOOL ||
   pc :: INSTRUCTION ||
   stack := {} ||
   sp := 0

OPERATIONS

   CALL (k) =
      PRE k : INSTRUCTION THEN
         stack(sp) := INSTRUCTION_NEXT(pc) ||
         sp := sp + 1 ||
         pc := k
      END
   ;
   RETURN =
      PRE sp > 0 THEN
         stack := {sp - 1} <<| stack ||
         pc := stack(sp - 1) ||
         sp := sp - 1
      END
   ;
   RETLW (k) =
      PRE k : WORD & sp > 0 THEN
         pc := stack(sp - 1) ||
         w := k
      END
   ;
   ADDLW (k) =
      PRE k : WORD THEN
         ANY result, carry, zero WHERE 
            result : WORD & carry : BOOL & zero : BOOL &
            result, carry, zero = add(k, w)
         THEN
            w, c, z := result, carry, zero
         END ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   ADDWF (f, d) =
      PRE f : REGISTER & d : BIT THEN
         ANY result, carry, zero WHERE 
            result, carry, zero = add(mem(f), w)
         THEN
            IF d = 0 THEN
               w := result 
            ELSE
               mem(f) :=  result
            END ||
            c := carry ||
	    z := zero
         END ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   SUBLW (k) =
      PRE k : WORD THEN
         ANY result, borrow, zero WHERE 
            result : WORD & borrow : BOOL & zero : BOOL &
            result, borrow, zero = substract(k, w)
         THEN
            w, c, z := result, borrow, zero
         END ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   SUBWF (f, d) =
      PRE f : REGISTER & d : BIT THEN
         ANY result, borrow, zero WHERE 
            result, borrow, zero = substract(mem(f), w)
         THEN
            IF d = 0 THEN
               w := result 
            ELSE
               mem(f) :=  result
            END ||
            c := borrow ||
	    z := zero
         END ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   ANDLW (k) =
      PRE k : WORD THEN
         ANY result, zero WHERE 
            result : WORD & zero : BOOL &
            result, zero = and(k, w)
         THEN
            w, z := result, zero
         END ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   ANDLWF (f, d) =
      PRE f : REGISTER & d : BIT THEN
         ANY result, zero WHERE 
            result : WORD & zero : BOOL &
            result, zero = and(mem(f), w)
         THEN
            IF d = 0 THEN
               w := result
            ELSE
               mem(f) := result
            END
         END ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   IORLW (k) =
      PRE k : WORD THEN
         ANY result, zero WHERE 
            result : WORD & zero : BOOL &
            result, zero = ior(k, w)
         THEN
            w, z := result, zero
         END ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   IORLWF (f, d) =
      PRE f : REGISTER & d : BIT THEN
         ANY result, zero WHERE 
            result : WORD & zero : BOOL &
            result, zero = ior(mem(f), w)
         THEN
            IF d = 0 THEN
               w := result
            ELSE
               mem(f) := result
            END
         END ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   XORLW (k) =
      PRE k : WORD THEN
         ANY result, zero WHERE 
            result : WORD & zero : BOOL &
            result, zero = xor(k, w)
         THEN
            w, z := result, zero
         END ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   XORLWF (f, d) =
      PRE f : REGISTER & d : BIT THEN
         ANY result, zero WHERE 
            result : WORD & zero : BOOL &
            result, zero = xor(mem(f), w)
         THEN
            IF d = 0 THEN
               w := result
            ELSE
               mem(f) := result
            END
         END ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   BCF (f, b) =
      PRE f : REGISTER & b : WORD_POSITION THEN
         mem(f) := bitclear(mem(f), b) ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   BSF (f, b) =
      PRE f : REGISTER & b : WORD_POSITION THEN
         mem(f) := bitset(mem(f), b) ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   BTFSC (f, b) =
      PRE f : REGISTER & b : WORD_POSITION THEN
         IF bitget(mem(f), b) = 0 THEN
           pc := INSTRUCTION_NEXT(INSTRUCTION_NEXT(pc))
         ELSE
           pc := INSTRUCTION_NEXT(pc)
         END
      END
   ;
   BTFSS (f, b) =
      PRE f : REGISTER & b : WORD_POSITION THEN
         IF bitget(mem(f), b) = 1 THEN
           pc := INSTRUCTION_NEXT(INSTRUCTION_NEXT(pc))
         ELSE
           pc := INSTRUCTION_NEXT(pc)
         END
      END
   ;
   CLRF (f) =
      PRE f : REGISTER THEN
         mem(f) := 0 ||
         z := TRUE ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   CLRW =
      BEGIN
         w := 0 ||
         z := TRUE ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   COMF (f, d) =
      PRE f : REGISTER & d : BIT THEN
         ANY result WHERE result = complement(mem(f))
         THEN
            IF d = 0 THEN
               w := result
            ELSE
               mem(f) := result
            END ||
            z := bool(result = 0) 
         END ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   DECF (f, d) =
      PRE f : REGISTER & d : BIT THEN
         ANY result, borrow, zero WHERE 
            result, borrow, zero = substract(mem(f), 1)
         THEN
            IF d = 0 THEN
               w := result 
            ELSE
               mem(f) :=  result
            END ||
	    z := zero
         END ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   DECFSZ (f, d) =
      PRE f : REGISTER & d : BIT THEN
         ANY result, borrow, zero WHERE 
            result, borrow, zero = substract(mem(f), 1)
         THEN
            IF d = 0 THEN
               w := result 
            ELSE
               mem(f) :=  result
            END ||
	    z := zero ||
            IF result = 0 THEN
              pc := INSTRUCTION_NEXT(INSTRUCTION_NEXT(pc))
            ELSE
              pc := INSTRUCTION_NEXT(pc)
            END
         END
      END
   ;
   GOTO (k) =
      PRE k : INSTRUCTION THEN
         pc := k
      END
   ;
   INCF (f, d) =
      PRE f : REGISTER & d : BIT THEN
         ANY result, carry, zero WHERE 
	    result : WORD & carry : BOOL & zero : BOOL &
            result, carry, zero = add(mem(f), 1)
         THEN
            IF d = 0 THEN
               w := result
            ELSE
               mem(f) := result
            END ||
            z := bool(result = 0)
         END ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
  INCFSZ (f, d) =
      PRE f : REGISTER & d : BIT THEN
         ANY result, carry, zero WHERE
	    result : WORD & carry : BOOL & zero : BOOL &
            result, carry, zero = add(mem(f), 1)
         THEN
            IF d = 0 THEN
               w := result
            ELSE
               mem(f) := result
            END ||
            IF result = 0 THEN
               pc := INSTRUCTION_NEXT(INSTRUCTION_NEXT(pc))
            ELSE
               pc := INSTRUCTION_NEXT(pc)
            END
         END
      END
   ;
   MOVLW (k) =
      PRE k : WORD THEN
         w := k ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   MOVF (f, d) =
      PRE f : REGISTER & d : BIT THEN
         IF d = 0 THEN
	    w := mem(f)
         ELSE
            skip
         END ||
         z := bool(mem(f) = 0) ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   MOVWF (f) =
      PRE f : REGISTER THEN
         mem (f) := w ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   NOP = 
     pc := INSTRUCTION_NEXT(pc)
   ;
   ROLF (f, d) =
      PRE f : REGISTER & d : BIT THEN
         ANY result, carry WHERE result, carry = rotateleft(mem(f))
         THEN
            IF d = 0 THEN
	       w := result
            ELSE
               mem(f) := result
            END ||
            c := carry
         END ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   RORF (f, d) =
      PRE f : REGISTER & d : BIT THEN
         ANY result, carry WHERE result, carry = rotateright(mem(f))
         THEN
            IF d = 0 THEN
	       w := result
            ELSE
               mem(f) := result
            END ||
            c := carry
         END ||
         pc := INSTRUCTION_NEXT(pc)
      END
   ;
   SWAP (f, d) =
      PRE f : REGISTER & d : BIT THEN
         ANY result WHERE result = swap(mem(f))
         THEN
            IF d = 0 THEN
	       w := result
            ELSE
               mem(f) := result
            END
         END ||
         pc := INSTRUCTION_NEXT(pc)
      END


END