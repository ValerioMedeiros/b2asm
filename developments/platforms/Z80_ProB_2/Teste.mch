MACHINE Teste
DEFINITIONS
aaa == 1;
aab == %( s7 ) .( s7 : 0..1 | bb ) \/ %( s7 ) .( s7 : 2..3 | aa )

SETS
 ID={aa,bb}
CONSTANTS iv
PROPERTIES
 iv:ID
VARIABLES xx
INVARIANT
    aab : (0..3 --> ID) &
    xx:ID
INITIALISATION xx:=iv
OPERATIONS
  Set(yy) = PRE yy:ID THEN xx:= yy END;
  Seta(yy) = PRE yy:ID THEN xx:= (aab)(1) END
END

