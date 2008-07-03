MACHINE
   A8051


SEES
TYPES , ALU

CONCRETE_CONSTANTS
sbuf ,
R0 ,
R1 ,
R2 ,
R3 ,
R4 ,
R5 ,
R6 ,
R7 ,
a ,
P0 ,
P1 ,
P2 ,
P3 ,
scon ,
psw ,
P






PROPERTIES
sbuf = 153 &
R0 = 0 &
R1 = 1 &
R2 = 2 &
R3 = 3 &
R4 = 4 &
R5 = 5 &
R6 = 6 &
R7 = 7 &
a = 240 &
P0 = 128 &
P1 = 144 &
P2 = 160 &
P3 = 176 &
scon = 152 &
psw = 208 &
P : BIT_ADDRESS &
P = ( psw , 0 )





CONCRETE_VARIABLES
stack ,
mem ,
pc ,
sp



INVARIANT
mem : MEM_ADDR -->> WORD &
pc : INSTRUCTION &
sp : NATURAL &
stack : NATURAL +->> INSTRUCTION &
dom ( stack ) = 0 .. ( sp - 1 ) &
bitget ( P ) = par ( mem ( a ) )








INITIALISATION
sp := 0 ||
stack := {} || mem :: MEM_ADDR -->> WORD || pc := 0





OPERATIONS

CJNE ( addr , data , jump ) =

        PRE
          addr : MEM_ADDR  &
          data : WORD &
          jump : INSTRUCTION

        THEN
          IF ( not ( mem ( addr ) = data ) ) THEN
            BEGIN
              IF ( mem ( addr ) < data ) THEN
                    mem ( psw ) := bitset ( mem ( psw ) , 7 )
              ELSE
                    mem ( psw ) := bitclear ( mem ( psw ) , 7 )
              END
            END || pc := INSTRUCTION_IJUMP ( pc , jump )
          ELSE
            pc := INSTRUCTION_NEXT ( pc )
          END
        END
;


CJNEI ( Rn , data , jump ) =
        PRE
          Rn : RAM_ADDR &
          ( Rn = R1 or Rn = R2 ) &
          data : WORD &
          jump : INSTRUCTION &
          mem ( Rn ) : RAM_ADDR

        THEN
           ANY addr WHERE addr : RAM_ADDR & addr = mem ( Rn )
             THEN
                  IF ( not ( mem ( addr ) = data ) ) THEN
                  BEGIN
                    IF ( mem ( addr ) < data ) THEN
                          mem ( psw ) := bitset ( mem ( psw ) , 7 )
                    ELSE
                          mem ( psw ) := bitclear ( mem ( psw ) , 7 )
                    END
                  END || pc := INSTRUCTION_IJUMP ( pc , jump )
               ELSE
                 pc := INSTRUCTION_NEXT ( pc )
               END
             END
       END
;

/*-->-->-->-->-->-Data Transfer-->-->-->-->-->-->-->-->--> */

MOV ( addr , data ) =
        PRE
          addr : MEM_ADDR &
          data : WORD
        THEN
          mem ( addr ) := data ||
          pc := INSTRUCTION_NEXT ( pc )
        END
;


MOVI ( Rn , data ) =
        PRE
           Rn :         RAM_ADDR &
           ( Rn = R1 or Rn = R2 ) & data : WORD &
                mem ( Rn ) : RAM_ADDR
        THEN
           ANY addr

              WHERE
                addr : RAM_ADDR & addr = mem ( Rn )
              THEN
                mem ( addr ) := data ||
                 pc := INSTRUCTION_NEXT ( pc )
              END
        END


;

/*-->-->-->-->-->-->END-->-->-->-->-->-->-->-->-->-->-->-->-->--> */


/*Falta colocar a paridade do acumulador*/
ANL ( addr ) =
        PRE
          addr : RAM_ADDR
        THEN
          ANY result , b
           WHERE
                result : WORD & b : BOOL &
                result , b = and ( a , addr )
           THEN

                mem ( a ) := result

           END || pc := INSTRUCTION_NEXT ( pc )
        END
;


ACALL ( inst ) =
        PRE inst : INSTRUCTION

        THEN
                sp := sp + 1 ||
                stack ( sp ) := INSTRUCTION_NEXT ( pc ) ||
                pc := inst
        END
;


SJMP ( jump ) =
        PRE
          jump : INSTRUCTION
        THEN
          pc := INSTRUCTION_IJUMP ( pc , jump )
        END
;

/*-->-->-->-->-->Boolean variable manipulation-->-->-->-->-*/
JC ( jump ) =
        PRE
          jump : INSTRUCTION
        THEN
          IF bitget ( mem ( psw ) , 7 ) = 1 THEN pc := INSTRUCTION_IJUMP ( pc , jump )
          ELSE pc := INSTRUCTION_NEXT ( pc ) END
         END
;
JNB ( bit , jump ) =
        PRE
          bit : BIT &
          jump : INSTRUCTION
        THEN
          IF bit = 0 THEN  pc := INSTRUCTION_IJUMP ( pc , jump )
          ELSE pc := INSTRUCTION_NEXT ( pc ) END

        END
;

CLR ( b ) =

        PRE
          b : BIT_ADDRESS
        THEN
          ANY w , p WHERE
            w : WORD & p : WORD_POSITION &  w , p = b
          THEN
           mem ( w ) := bitset ( mem ( w ) , p )
          END || pc := INSTRUCTION_NEXT ( pc )
        END
;


/*-->-->-->-->END-->-->-->-->-->-->-->-->--->-->-->-->-->-->-->-->-->-->-->-->-->-->-->-->*/

AJMP ( jump ) =
        PRE
          jump : INSTRUCTION
        THEN
                pc := jump
        END
;

/* Falta o setar o bit de paridade */
ADDS ( acc , src ) =
        PRE
          acc : SFR_ADDR &
          acc = a &
          src : MEM_ADDR
        THEN
          ANY
            result , carry , zero
          WHERE
            result : WORD & result , carry , zero = add ( mem ( a ) , mem ( src ) ) &
            carry : BOOL & zero : BOOL
          THEN
               mem ( a ) := result
          END ||
          pc := INSTRUCTION_NEXT ( pc )
        END
;
ADDD ( acc , data ) =
        PRE
          acc : SFR_ADDR &
          acc = a &
          data : WORD
        THEN
          ANY
            result , carry , zero
          WHERE
            result : WORD & result , carry , zero = add ( mem ( a ) , data ) &
            carry : BOOL & zero : BOOL
          THEN
               mem ( a ) := result
          END ||
          pc := INSTRUCTION_NEXT ( pc )
        END

;
SUBBS ( acc , src ) =
        PRE
          acc : SFR_ADDR &
          acc = a &
          src : MEM_ADDR
        THEN
          ANY
            result , carry , zero
          WHERE
            result : WORD & result , carry , zero = substract ( mem ( a ) , mem ( src ) ) &
            carry : BOOL & zero : BOOL
          THEN
               mem ( a ) := result

          END ||
          pc := INSTRUCTION_NEXT ( pc )
        END
;
SUBBD ( acc , data ) =
        PRE
          acc : SFR_ADDR &
          acc = a &
          data : WORD
        THEN
          ANY
            result , carry , zero
          WHERE
            result : WORD & result , carry , zero = substract ( mem ( a ) , data ) &
            carry : BOOL & zero : BOOL
          THEN
               mem ( a ) := result

          END ||
          pc := INSTRUCTION_NEXT ( pc )
        END
;
INC =
        BEGIN
          ANY
            result , carry , zero
          WHERE
            result : WORD & result , carry , zero = add ( mem ( a ) , 1 ) &
            carry : BOOL & zero : BOOL
          THEN
               mem ( a ) := result
          END ||
          pc := INSTRUCTION_NEXT ( pc )
        END
;

DEC =
        BEGIN
          ANY
            result , carry , zero
          WHERE
            result : WORD & result , carry , zero = substract ( a , 1 ) &
            carry : BOOL & zero : BOOL
          THEN
              mem ( a ) := result
          END ||
          pc := INSTRUCTION_NEXT ( pc )
        END
;
INCS ( src ) =

        PRE
          src : MEM_ADDR
        THEN
          ANY
            result , carry , zero
          WHERE
            result : WORD & result , carry , zero = add ( mem ( src ) , 1 ) &
            carry : BOOL & zero : BOOL
          THEN
               mem ( src ) := result
          END ||
          pc := INSTRUCTION_NEXT ( pc )
        END

END
