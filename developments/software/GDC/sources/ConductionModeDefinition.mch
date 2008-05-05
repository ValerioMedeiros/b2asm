MACHINE
   ConductionModeDefinition

SEES
    Context

VARIABLES
/* ************************ */
/* internal state variables */
/* ************************ */
   conduction, 

/* ************************ */
/* input variables representing the monitored 
   environment variables */
/* ************************ */
   manual,     
   controlled

INVARIANT
/* ************************ */
/* typing */
/* ************************ */
    conduction : CONDUCTION 
  & manual : BOOL
  & controlled : BOOL

/* ************************ */
/* consistency between the internal state variables 
   and the input variables */
/* ************************ */
  & ((conduction = MANUAL) <=> 
     ((manual = TRUE) & (controlled = FALSE)))
  & ((conduction = CONTROLLED) <=> 
     ((manual = FALSE) & (controlled = TRUE)))
  & ((conduction = CONDUCTION_OFF) <=> 
     (  ((manual = FALSE) & (controlled = FALSE))
      or ((manual = TRUE) & (controlled = TRUE))))

INITIALISATION
    conduction := MANUAL ||
    manual := TRUE ||
    controlled := FALSE

OPERATIONS

/* ************************ */
/* update_conduction has two boolean arguments corresponding to the
monitored variables selecting manual or controlled
conduction modes; updates the corresponding input variables
and sets the internal state variable accordingly. */
/* ************************ */

update_conduction(i_manual, i_controlled) =
  PRE
      i_manual : BOOL
    & i_controlled : BOOL
  THEN
    manual := i_manual ||
    controlled := i_controlled ||
    IF (i_manual = TRUE) THEN
      IF (i_controlled = TRUE) THEN
        conduction := CONDUCTION_OFF
      ELSE
        conduction := MANUAL
      END
    ELSE
      IF (i_controlled = TRUE) THEN
        conduction := CONTROLLED
      ELSE
        conduction := CONDUCTION_OFF
      END
    END
  END

END