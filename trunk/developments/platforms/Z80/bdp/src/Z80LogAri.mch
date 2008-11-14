
MACHINE
   Z80LogAri
INCLUDES
    MEMORY

SEES
TYPES ,
ALU ,
POWER2





SETS
 id_reg_8 = { a0 , f0 , f_0 , a_0 ,
                 b0 , c0 , b_0 , c_0 ,
                 d0 , e0 , d_0 , e_0 ,
                 h0 , l0 , h_0 , l_0 ,
                 i0 , r0 } ;

 id_reg_16 = { BC , DE , HL , SP , AF }


ABSTRACT_VARIABLES
   rgs8 ,
  /* mem,
   stack,*/

   pc , sp , ix , iy ,
   /*not implemented yet - i,r */
   i_o_ports






INVARIANT
   stack : USHORTINT  --> SCHAR &
   rgs8 : id_reg_8 --> SCHAR &
   pc : USHORTINT & sp : USHORTINT & ix : USHORTINT & iy : USHORTINT &
  /* stack <: mem */

  /* I/O replace SCHAR for UCHAR*/
   i_o_ports  : UCHAR --> SCHAR





























CONCRETE_CONSTANTS
REG16_TO_REG8 ,
REG8_TO_REG16 ,

update_flag_register ,
get_new_flag_register



PROPERTIES


  REG16_TO_REG8 : id_reg_16 --> ( id_reg_8 * id_reg_8 )
  & ! ( r1 , r2 , r3 ) .
  ( r1 : id_reg_16  & r2 :  id_reg_8 & r3 :  id_reg_8
   => ( r1 = BC =>  REG16_TO_REG8 ( r1 ) = ( b0 , c0 ) ) &
      ( r1 = DE =>  REG16_TO_REG8 ( r1 ) = ( d0 , e0 ) ) &
      ( r1 = HL =>  REG16_TO_REG8 ( r1 ) = ( h0 , l0 ) ) &
      ( r1 = AF =>  REG16_TO_REG8 ( r1 ) = ( a0 , f0 ) )
   )
  &
  REG8_TO_REG16 = REG16_TO_REG8 ~ &

  update_flag_register = % ( rgs8_ , c0 , n_add_sub , pv2 , h4 , z6 , s7 ) .
  ( rgs8_ : id_reg_8 --> SCHAR & c0 : BOOL & n_add_sub : BOOL & pv2 : BOOL & h4 : BOOL &  z6  : BOOL & s7 : BOOL |
      rgs8_ <+ { ( f0 |-> BYTE_TO_SCHAR ( SCHAR_TO_BYTE ( rgs8_ ( f0 ) ) <+ {
                     ( 7 |-> bool_to_bit ( s7 ) ) ,
                                       ( 6 |-> bool_to_bit ( z6 ) ) ,

                                       ( 4 |-> bool_to_bit ( h4 ) ) ,

                                 ( 2 |-> bool_to_bit ( pv2 ) ) ,
                                       ( 1 |-> bool_to_bit ( n_add_sub ) ) ,
                                       ( 0 |-> bool_to_bit ( c0 ) ) } ) ) } )

        &

        get_new_flag_register = % ( rgs8_ , c0 , n_add_sub , pv2 , h4 , z6 , s7 ) .
        ( rgs8_ : id_reg_8 --> SCHAR & c0 : BOOL & n_add_sub : BOOL & pv2 : BOOL & h4 : BOOL & z6  : BOOL & s7 : BOOL |
        ( f0 |-> BYTE_TO_SCHAR ( SCHAR_TO_BYTE ( rgs8_ ( f0 ) ) <+ {
                     ( 7 |-> bool_to_bit ( s7 ) ) ,
                                       ( 6 |-> bool_to_bit ( z6 ) ) ,

                                       ( 4 |-> bool_to_bit ( h4 ) ) ,

                                 ( 2 |-> bool_to_bit ( pv2 ) ) ,
                                       ( 1 |-> bool_to_bit ( n_add_sub ) ) ,
                                       ( 0 |-> bool_to_bit ( c0 ) ) } ) ) )



ASSERTIONS
   dom ( stack ) = USHORTINT &
   ran ( stack ) <: SCHAR &
   ran ( mem ) <: SCHAR &
   dom ( mem ) = USHORTINT &
   ran ( rgs8 ) <: SCHAR &
   dom ( rgs8 ) = id_reg_8 &
   INSTRUCTION_NEXT ( 0 ) = 1 &
   INSTRUCTION_NEXT ( 1 ) = 2 &
   INSTRUCTION_NEXT ( 2 ) = 3 &
   INSTRUCTION_NEXT ( 3 ) = 4 &
   INSTRUCTION_NEXT ( 4 ) = 5 &
   INSTRUCTION_NEXT ( 5 ) = 6 &
   INSTRUCTION_NEXT ( 6 ) = 7 &
   INSTRUCTION_NEXT ( 7 ) = 8 &
   INSTRUCTION_NEXT ( 8 ) = 9 &
   INSTRUCTION_NEXT ( 9 ) = 10 &
   INSTRUCTION_NEXT ( 10 ) = 11 &
   INSTRUCTION_NEXT ( 11 ) = 12 &
   INSTRUCTION_NEXT ( 12 ) = 13 &
   INSTRUCTION_NEXT ( 13 ) = 14 &

   ! ( vec , in0 ) . ( vec : BYTE & in0 : 0 .. 7
   =>  bitgetSCHAR ( BYTE_TO_SCHAR ( vec ) , in0 ) = vec ( in0 ) ) &

   mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) ) : SCHAR & mem ( SSHORTINT_TO_USHORTINT ( mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( b0 ) , rgs8 ( c0 ) ) ) ) ) ) : SCHAR &
   mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ) ) : SCHAR & mem ( SSHORTINT_TO_USHORTINT ( mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( d0 ) , rgs8 ( e0 ) ) ) ) ) ) : SCHAR &
   mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) : SCHAR & mem ( SSHORTINT_TO_USHORTINT ( mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) ) ) : SCHAR &
   mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( a0 ) , rgs8 ( f0 ) ) ) ) : SCHAR & mem ( SSHORTINT_TO_USHORTINT ( mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( a0 ) , rgs8 ( f0 ) ) ) ) ) ) : SCHAR &

   mem ( SSHORTINT_TO_USHORTINT ( sp ) ) : SCHAR &
   mem ( SSHORTINT_TO_USHORTINT ( ix ) ) : SCHAR &
   mem ( SSHORTINT_TO_USHORTINT ( iy ) ) : SCHAR



INITIALISATION
   rgs8 :: id_reg_8 -->> SCHAR ||
/*   mem :: USHORTINT --> SCHAR || */
   pc :: USHORTINT || sp :: SSHORTINT || ix :: SSHORTINT || iy :: SSHORTINT ||
/*   stack :: USHORTINT  --> SCHAR ||*/
   i_o_ports  ::  UCHAR -->  SCHAR


OPERATIONS


/*substract8SCHAR (w1, w2) = (sum, is_negative, carry, digit_carry, zero)
  get_new_flag_register = %(rgs8_,c0,n1,pv2,  h4,  z6,s7). 
  
  Duvida: Modoluzarizo os registradores ?
  Pois a consulta, atribuiçao e atualizaçao tornam-se mais simples 


*/



ADD_A_r ( rr ) =
  PRE rr : id_reg_8
  THEN
     ANY
         sum , is_negative , carry , digit_carry , zero

     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &

        sum , is_negative , carry , digit_carry , zero = add8SCHAR ( rgs8 ( a0 ) , rgs8 ( rr ) )
     THEN                                                                                                                  /* verificar se correto os carry, carry , aqui e nos demais abaixo.*/
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , FALSE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

ADD_A_n ( nn ) =
  PRE nn : SCHAR
  THEN
     ANY
         sum , is_negative , carry , digit_carry , zero

     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &

        sum , is_negative , carry , digit_carry , zero = add8SCHAR ( rgs8 ( a0 ) , nn )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , FALSE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
;

ADD_A_9HL0 =
     ANY
         sum , is_negative , carry , digit_carry , zero
     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
        sum , is_negative , carry , digit_carry , zero = add8SCHAR ( rgs8 ( a0 ) , mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , FALSE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

;

ADD_A_9IX_d0 =
     ANY
         sum , is_negative , carry , digit_carry , zero
     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
        sum , is_negative , carry , digit_carry , zero = add8SCHAR ( rgs8 ( a0 ) , mem ( 256 - ( ix + rgs8 ( d0 ) ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , FALSE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END


;

ADD_A_9IY_d0 =
     ANY
         sum , is_negative , carry , digit_carry , zero
     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
        sum , is_negative , carry , digit_carry , zero = add8SCHAR ( rgs8 ( a0 ) , mem ( 256 - ( iy + rgs8 ( d0 ) ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , FALSE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
;


ADC_A_r ( rr ) =
  PRE rr : id_reg_8
  THEN
     ANY
         sum , is_negative , carry , digit_carry , zero

     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &

        sum , is_negative , carry , digit_carry , zero = add8SCHAR ( rgs8 ( a0 ) , rgs8 ( rr ) + bitgetSCHAR ( rgs8 ( f0 ) , 0 ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , FALSE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

ADC_A_n ( nn ) =
  PRE nn : SCHAR
  THEN
     ANY
         sum , is_negative , carry , digit_carry , zero

     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &

        sum , is_negative , carry , digit_carry , zero = add8SCHAR ( rgs8 ( a0 ) , nn + bitgetSCHAR ( rgs8 ( f0 ) , 0 ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , FALSE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
;

ADC_A_9HL0 =
     ANY
         sum , is_negative , carry , digit_carry , zero
     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
        sum , is_negative , carry , digit_carry , zero = add8SCHAR ( rgs8 ( a0 ) , mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) + bitgetSCHAR ( rgs8 ( f0 ) , 0 ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , FALSE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

;

ADC_A_9IX_d0 =
     ANY
         sum , is_negative , carry , digit_carry , zero
     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
        sum , is_negative , carry , digit_carry , zero = add8SCHAR ( rgs8 ( a0 ) , mem ( 256 - ( ix + rgs8 ( d0 ) ) ) + bitgetSCHAR ( rgs8 ( f0 ) , 0 ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , FALSE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END


;

ADC_A_9IY_d0 =
     ANY
         sum , is_negative , carry , digit_carry , zero
     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
        sum , is_negative , carry , digit_carry , zero = add8SCHAR ( rgs8 ( a0 ) , mem ( 256 - ( iy + rgs8 ( d0 ) ) ) + bitgetSCHAR ( rgs8 ( f0 ) , 0 ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , FALSE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
;




SUB_A_r ( rr ) =
  PRE rr : id_reg_8
  THEN
     ANY
         sum , is_negative , carry , digit_carry , zero
     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &

        sum , is_negative , carry , digit_carry , zero = substract8SCHAR ( rgs8 ( a0 ) , rgs8 ( rr ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , TRUE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

SUB_A_n ( nn ) =
  PRE nn : SCHAR
  THEN
     ANY
         sum , is_negative , carry , digit_carry , zero

     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &

        sum , is_negative , carry , digit_carry , zero = substract8SCHAR ( rgs8 ( a0 ) , nn )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , TRUE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
;

SUB_A_9HL0 =
     ANY
         sum , is_negative , carry , digit_carry , zero
     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
        sum , is_negative , carry , digit_carry , zero = substract8SCHAR ( rgs8 ( a0 ) , mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , TRUE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

;

SUB_A_9IX_d0 =
     ANY
         sum , is_negative , carry , digit_carry , zero
     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
        sum , is_negative , carry , digit_carry , zero = substract8SCHAR ( rgs8 ( a0 ) , mem ( 256 - ( ix + rgs8 ( d0 ) ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , TRUE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END


;

SUB_A_9IY_d0 =
     ANY
         sum , is_negative , carry , digit_carry , zero
     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
        sum , is_negative , carry , digit_carry , zero = substract8SCHAR ( rgs8 ( a0 ) , mem ( 256 - ( iy + rgs8 ( d0 ) ) ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , TRUE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
;


SBC_A_r ( rr ) =
  PRE rr : id_reg_8
  THEN
     ANY
         sum , is_negative , carry , digit_carry , zero

     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &

        sum , is_negative , carry , digit_carry , zero = substract8SCHAR ( rgs8 ( a0 ) , rgs8 ( rr ) - bitgetSCHAR ( rgs8 ( f0 ) , 0 ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , TRUE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

SBC_A_n ( nn ) =
  PRE nn : SCHAR
  THEN
     ANY
         sum , is_negative , carry , digit_carry , zero

     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &

        sum , is_negative , carry , digit_carry , zero = add8SCHAR ( rgs8 ( a0 ) , nn - bitgetSCHAR ( rgs8 ( f0 ) , 0 ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , TRUE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
;

SBC_A_9HL0 =
     ANY
         sum , is_negative , carry , digit_carry , zero
     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
        sum , is_negative , carry , digit_carry , zero = add8SCHAR ( rgs8 ( a0 ) , mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) - bitgetSCHAR ( rgs8 ( f0 ) , 0 ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , TRUE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

;

SBC_A_9IX_d0 =
     ANY
         sum , is_negative , carry , digit_carry , zero
     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
        sum , is_negative , carry , digit_carry , zero = add8SCHAR ( rgs8 ( a0 ) , mem ( 256 - ( ix + rgs8 ( d0 ) ) ) - bitgetSCHAR ( rgs8 ( f0 ) , 0 ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , TRUE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

;

SBC_A_9IY_d0 =
     ANY
         sum , is_negative , carry , digit_carry , zero
     WHERE  sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
        sum , is_negative , carry , digit_carry , zero = add8SCHAR ( rgs8 ( a0 ) , mem ( 256 - ( iy + rgs8 ( d0 ) ) ) - bitgetSCHAR ( rgs8 ( f0 ) , 0 ) )
     THEN
       rgs8 := rgs8 <+ { a0 |-> sum , get_new_flag_register ( rgs8 , carry , TRUE , carry , digit_carry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

;

/* AND OR XOR CP */

AND_A_r ( rr ) =
  PRE rr : id_reg_8
  THEN
     ANY
         result , carry , add_sub , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & add_sub : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , zero = andSCHAR ( rgs8 ( a0 ) , rgs8 ( rr ) ) &
         carry = FALSE &
         add_sub =  FALSE      &
         pv =   FALSE      &
         halfcarry = FALSE &
         /*zero = bool(result=0)  &*/
         is_negative = bool ( result < 0 )
     THEN                                                                                             /* %(rgs8_,c0,n1,pv2,  h4,  z6,s7)*/
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , add_sub , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

 AND_A_n ( nn ) =
  PRE nn : SCHAR
  THEN
     ANY
         result , carry , add_sub , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & add_sub : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , zero = andSCHAR ( rgs8 ( a0 ) , nn ) &
         carry = FALSE &
         add_sub =  FALSE      &
         pv =   FALSE      &
         halfcarry = FALSE &
         /*zero = bool(result=0)  &*/
         is_negative = bool ( result < 0 )
     THEN                                                                                             /* %(rgs8_,c0,n1,pv2,  h4,  z6,s7)*/
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , add_sub , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

 AND_A_9HL0 =
     ANY
         result , carry , nn , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & nn : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , zero = andSCHAR ( rgs8 ( a0 ) , mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) ) &
         carry = FALSE &
         nn =  FALSE      &
         pv =   FALSE      &
         halfcarry = FALSE &
         /*zero = bool(result=0)  &*/
         is_negative = bool ( result < 0 )
     THEN                                                                                             /* %(rgs8_,c0,n1,pv2,  h4,  z6,s7)*/
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , nn , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

 ;

  AND_A_9IX_d0 =
     ANY
         result , carry , nn , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & nn : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , zero = andSCHAR ( rgs8 ( a0 ) , mem ( 256 - ( ix + rgs8 ( d0 ) ) ) ) &
         carry = FALSE &
         nn =  FALSE      &
         pv =   FALSE      &
         halfcarry = FALSE &
         /*zero = bool(result=0)  &*/
         is_negative = bool ( result < 0 )
     THEN                                                                                             /* %(rgs8_,c0,n1,pv2,  h4,  z6,s7)*/
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , nn , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
;

  AND_A_9IY_d0 =
     ANY
         result , carry , nn , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & nn : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , zero = andSCHAR ( rgs8 ( a0 ) , mem ( 256 - ( iy + rgs8 ( d0 ) ) ) ) &
         carry = FALSE &
         nn =  FALSE      &
         pv =   FALSE      &
         halfcarry = FALSE &
         /*zero = bool(result=0)  &*/
         is_negative = bool ( result < 0 )
     THEN                                                                                             /* %(rgs8_,c0,n1,pv2,  h4,  z6,s7)*/
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , nn , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
 ;


OR_A_r ( rr ) =
  PRE rr : id_reg_8
  THEN
     ANY
         result , carry , add_sub , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & add_sub : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , zero = iorSCHAR ( rgs8 ( a0 ) , rgs8 ( rr ) ) &
         carry = FALSE &
         add_sub =  FALSE      &
         pv =   FALSE      &
         halfcarry = FALSE &
         /*zero = bool(result=0)  &*/
         is_negative = bool ( result < 0 )
     THEN                                                                                             /* %(rgs8_,c0,n1,pv2,  h4,  z6,s7)*/
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , add_sub , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

 OR_A_n ( nn ) =
  PRE nn : SCHAR
  THEN
     ANY
         result , carry , add_sub , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & add_sub : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , zero = iorSCHAR ( rgs8 ( a0 ) , nn ) &
         carry = FALSE &
         add_sub =  FALSE      &
         pv =   FALSE      &
         halfcarry = FALSE &
         /*zero = bool(result=0)  &*/
         is_negative = bool ( result < 0 )
     THEN                                                                                             /* %(rgs8_,c0,n1,pv2,  h4,  z6,s7)*/
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , add_sub , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

 OR_A_9HL0 =
     ANY
         result , carry , nn , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & nn : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , zero = iorSCHAR ( rgs8 ( a0 ) , mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) ) &
         carry = FALSE &
         nn =  FALSE      &
         pv =   FALSE      &
         halfcarry = FALSE &
         /*zero = bool(result=0)  &*/
         is_negative = bool ( result < 0 )
     THEN                                                                                             /* %(rgs8_,c0,n1,pv2,  h4,  z6,s7)*/
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , nn , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

 ;

  OR_A_9IX_d0 =
     ANY
         result , carry , nn , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & nn : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , zero = iorSCHAR ( rgs8 ( a0 ) , mem ( 256 - ( ix + rgs8 ( d0 ) ) ) ) &
         carry = FALSE &
         nn =  FALSE      &
         pv =   FALSE      &
         halfcarry = FALSE &
         /*zero = bool(result=0)  &*/
         is_negative = bool ( result < 0 )
     THEN                                                                                             /* %(rgs8_,c0,n1,pv2,  h4,  z6,s7)*/
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , nn , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
;

  OR_A_9IY_d0 =
     ANY
         result , carry , nn , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & nn : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , zero = iorSCHAR ( rgs8 ( a0 ) , mem ( 256 - ( iy + rgs8 ( d0 ) ) ) ) &
         carry = FALSE &
         nn =  FALSE      &
         pv =   FALSE      &
         halfcarry = FALSE &
         /*zero = bool(result=0)  &*/
         is_negative = bool ( result < 0 )
     THEN                                                                                             /* %(rgs8_,c0,n1,pv2,  h4,  z6,s7)*/
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , nn , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
 ;

XOR_A_r ( rr ) =
  PRE rr : id_reg_8
  THEN
     ANY
         result , carry , add_sub , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & add_sub : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , zero = xorSCHAR ( rgs8 ( a0 ) , rgs8 ( rr ) ) &
         carry = FALSE &
         add_sub =  FALSE      &
         pv =   FALSE      &
         halfcarry = FALSE &
         /*zero = bool(result=0)  &*/
         is_negative = bool ( result < 0 )
     THEN                                                                                             /* %(rgs8_,c0,n1,pv2,  h4,  z6,s7)*/
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , add_sub , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

 XOR_A_n ( nn ) =
  PRE nn : SCHAR
  THEN
     ANY
         result , carry , add_sub , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & add_sub : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , zero = xorSCHAR ( rgs8 ( a0 ) , nn ) &
         carry = FALSE &
         add_sub =  FALSE      &
         pv =   FALSE      &
         halfcarry = FALSE &
         /*zero = bool(result=0)  &*/
         is_negative = bool ( result < 0 )
     THEN                                                                                             /* %(rgs8_,c0,n1,pv2,  h4,  z6,s7)*/
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , add_sub , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

 XOR_A_9HL0 =
     ANY
         result , carry , nn , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & nn : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , zero = xorSCHAR ( rgs8 ( a0 ) , mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) ) &
         carry = FALSE &
         nn =  FALSE      &
         pv =   FALSE      &
         halfcarry = FALSE &
         /*zero = bool(result=0)  &*/
         is_negative = bool ( result < 0 )
     THEN                                                                                             /* %(rgs8_,c0,n1,pv2,  h4,  z6,s7)*/
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , nn , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

 ;

  XOR_A_9IX_d0 =
     ANY
         result , carry , nn , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & nn : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , zero = xorSCHAR ( rgs8 ( a0 ) , mem ( 256 - ( ix + rgs8 ( d0 ) ) ) ) &
         carry = FALSE &
         nn =  FALSE      &
         pv =   FALSE      &
         halfcarry = FALSE &
         /*zero = bool(result=0)  &*/
         is_negative = bool ( result < 0 )
     THEN                                                                                             /* %(rgs8_,c0,n1,pv2,  h4,  z6,s7)*/
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , nn , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
;

  XOR_A_9IY_d0 =
     ANY
         result , carry , nn , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & nn : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , zero = xorSCHAR ( rgs8 ( a0 ) , mem ( 256 - ( iy + rgs8 ( d0 ) ) ) ) &
         carry = FALSE &
         nn =  FALSE      &
         pv =   FALSE      &
         halfcarry = FALSE &
         /*zero = bool(result=0)  &*/
         is_negative = bool ( result < 0 )
     THEN                                                                                             /* %(rgs8_,c0,n1,pv2,  h4,  z6,s7)*/
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , nn , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
 ;

  CP_A_r ( rr ) =
  PRE rr : id_reg_8
  THEN
     ANY
         result , carry , n_add_sub , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & n_add_sub : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , is_negative , carry , halfcarry , zero = substract8SCHAR ( rgs8 ( a0 ) , rgs8 ( rr ) ) &
         n_add_sub =  TRUE      &        pv =   carry

     THEN
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , n_add_sub , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;

  CP_A_n ( nn ) =
  PRE nn : SCHAR
  THEN
     ANY
         result , carry , n_add_sub , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & n_add_sub : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , is_negative , carry , halfcarry , zero = substract8SCHAR ( rgs8 ( a0 ) , nn ) &
         n_add_sub =  TRUE      &        pv =   carry

     THEN
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , n_add_sub , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END
  END
 ;


  CP_A_9HL0 =
     ANY
         result , carry , n_add_sub , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & n_add_sub : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , is_negative , carry , halfcarry , zero = substract8SCHAR ( rgs8 ( a0 ) , mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) ) &
         n_add_sub =  TRUE      &        pv =   carry

     THEN
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , n_add_sub , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

 ;

  CP_A_9IX_d0 =
     ANY
         result , carry , n_add_sub , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & n_add_sub : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , is_negative , carry , halfcarry , zero = substract8SCHAR ( rgs8 ( a0 ) , mem ( 256 - ( ix + rgs8 ( d0 ) ) ) ) &
         n_add_sub =  TRUE      &        pv =   carry

     THEN
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , n_add_sub , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

 ;

  CP_A_9IY_d0 =
     ANY
         result , carry , n_add_sub , pv , halfcarry , zero , is_negative
     WHERE  result : SCHAR & carry : BOOL & n_add_sub : BOOL & pv  : BOOL & halfcarry : BOOL & zero : BOOL & is_negative : BOOL &
                 result , is_negative , carry , halfcarry , zero = substract8SCHAR ( rgs8 ( a0 ) , mem ( 256 - ( iy + rgs8 ( d0 ) ) ) ) &
         n_add_sub =  TRUE      &        pv =   carry

     THEN
       rgs8 := rgs8 <+ { a0 |-> result , get_new_flag_register ( rgs8 , carry , n_add_sub , pv , halfcarry , zero , is_negative ) } ||
       pc := INSTRUCTION_NEXT ( pc )
     END

 ;
 /*INC e DEC  */


 INC_r ( rr ) =
        PRE
                rr : id_reg_8
        THEN
                ANY
                        sum , is_negative , carry , digit_carry , zero
                WHERE
                        sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
                        add8SCHAR ( rgs8 ( rr ) , 1 ) = sum , is_negative , carry , digit_carry , zero
                THEN
                        rgs8 := rgs8 <+ { ( rr |-> sum ) , get_new_flag_register ( rgs8 , carry , FALSE , bool ( rgs8 ( rr ) = 127 ) , digit_carry , zero , is_negative ) } ||
                        pc := INSTRUCTION_NEXT ( pc )
                END
     END
     ;

 INC_9HL0 =
        ANY
                sum , is_negative , carry , digit_carry , zero
        WHERE
                sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
                add8SCHAR ( mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) , 1 ) = sum , is_negative , carry , digit_carry , zero
        THEN
                rgs8 := rgs8 <+ { get_new_flag_register ( rgs8 , carry , FALSE , bool ( mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) = 127 ) , digit_carry , zero , is_negative ) } ||
                updateAddressMem ( mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) , sum ) ||
                pc := INSTRUCTION_NEXT ( pc )
        END

        ;

 INC_9IX_d0 =
        ANY
                sum , is_negative , carry , digit_carry , zero
        WHERE
                sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
                add8SCHAR ( mem ( 256 - ( ix + rgs8 ( d0 ) ) ) , 1 ) = sum , is_negative , carry , digit_carry , zero
        THEN
                rgs8 := rgs8 <+ { get_new_flag_register ( rgs8 , carry , FALSE , bool ( mem ( 256 - ( ix + rgs8 ( d0 ) ) ) = 127 ) , digit_carry , zero , is_negative ) } ||
                updateAddressMem ( mem ( 256 - ( ix + rgs8 ( d0 ) ) ) , sum ) ||
                pc := INSTRUCTION_NEXT ( pc )
        END

        ;

 INC_9IY_d0 =
        ANY
                sum , is_negative , carry , digit_carry , zero
        WHERE
                sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
                add8SCHAR ( mem ( 256 - ( iy + rgs8 ( d0 ) ) ) , 1 ) = sum , is_negative , carry , digit_carry , zero
        THEN
                rgs8 := rgs8 <+ { get_new_flag_register ( rgs8 , carry , FALSE , bool ( mem ( 256 - ( iy + rgs8 ( d0 ) ) ) = 127 ) , digit_carry , zero , is_negative ) } ||
                updateAddressMem ( mem ( 256 - ( iy + rgs8 ( d0 ) ) ) , sum ) ||
                pc := INSTRUCTION_NEXT ( pc )
        END

;


 DEC_r ( rr ) =
        PRE
                rr : id_reg_8
        THEN
                ANY
                        sum , is_negative , carry , digit_carry , zero
                WHERE
                        sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
                        substract8SCHAR ( rgs8 ( rr ) , 1 ) = sum , is_negative , carry , digit_carry , zero
                THEN
                        rgs8 := rgs8 <+ { ( rr |-> sum ) , get_new_flag_register ( rgs8 , carry , TRUE , bool ( rgs8 ( rr ) = - 128 ) , digit_carry , zero , is_negative ) } ||
                        pc := INSTRUCTION_NEXT ( pc )
                END
     END
     ;

 DEC_9HL0 =
        ANY
                sum , is_negative , carry , digit_carry , zero
        WHERE
                sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
                add8SCHAR ( mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) , 1 ) = sum , is_negative , carry , digit_carry , zero
        THEN
                rgs8 := rgs8 <+ { get_new_flag_register ( rgs8 , carry , TRUE , bool ( mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) = - 128 ) , digit_carry , zero , is_negative ) } ||
                updateAddressMem ( mem ( SSHORTINT_TO_USHORTINT ( SCHAR_TO_SSHORTINT ( rgs8 ( h0 ) , rgs8 ( l0 ) ) ) ) , sum ) ||
                pc := INSTRUCTION_NEXT ( pc )
        END

        ;

 DEC_9IX_d0 =
        ANY
                sum , is_negative , carry , digit_carry , zero
        WHERE
                sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
                add8SCHAR ( mem ( 256 - ( ix + rgs8 ( d0 ) ) ) , 1 ) = sum , is_negative , carry , digit_carry , zero
        THEN
                rgs8 := rgs8 <+ { get_new_flag_register ( rgs8 , carry , TRUE , bool ( mem ( 256 - ( ix + rgs8 ( d0 ) ) ) = - 128 ) , digit_carry , zero , is_negative ) } ||
                updateAddressMem ( mem ( 256 - ( ix + rgs8 ( d0 ) ) ) , sum ) ||
                pc := INSTRUCTION_NEXT ( pc )
        END

        ;

 DEC_9IY_d0 =
        ANY
                sum , is_negative , carry , digit_carry , zero
        WHERE
                sum : SCHAR & is_negative : BOOL & carry : BOOL & digit_carry : BOOL & zero : BOOL &
                add8SCHAR ( mem ( 256 - ( iy + rgs8 ( d0 ) ) ) , 1 ) = sum , is_negative , carry , digit_carry , zero
        THEN
                rgs8 := rgs8 <+ { get_new_flag_register ( rgs8 , carry , TRUE , bool ( mem ( 256 - ( iy + rgs8 ( d0 ) ) ) = - 128 ) , digit_carry , zero , is_negative ) } ||
                updateAddressMem ( mem ( 256 - ( iy + rgs8 ( d0 ) ) ) , sum ) ||
                pc := INSTRUCTION_NEXT ( pc )
        END

END
