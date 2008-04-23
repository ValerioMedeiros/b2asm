MACHINE
   LeaderDefinition

SEES
   Context

VARIABLES
/* ************************ */
/* internal state variables */
/* ************************ */
   cabin,

/* ************************ */
/* input variables representing the monitored 
   environment variables */
/* ************************ */
   leader,
   opposite

INVARIANT
/* ************************ */
/* typing */
/* ************************ */
   cabin : STATE &
   leader : BOOL &
   opposite : BOOL &

/* ************************ */
/* assumption on the relation between input variables */
/* ************************ */

   ( not (leader = TRUE & opposite = TRUE)) &

/* ************************ */
/* consistency between the internal state variables 
   and the input variables */
/* ************************ */
   ( (leader = TRUE) <=> (cabin = LEADER) ) &
   ( (opposite = TRUE) <=> (cabin = OPPOSITE) ) &
   ( ((leader = FALSE) & (opposite = FALSE)) <=> (cabin = NONE) ) 

ASSERTIONS
   ((leader = TRUE) => ((cabin = LEADER) & (opposite = FALSE))) ;
   ((opposite = TRUE) => ((cabin = OPPOSITE) & (leader = FALSE)))

INITIALISATION
    cabin := NONE ||
    leader := FALSE ||
    opposite := FALSE

OPERATIONS

/* ************************ */
/* update_leader has two boolean arguments corresponding to the
monitored variables defining if the current cabin is supposed to be
leader or not; updates the corresponding input variables
and sets the internal state variable accordingly. */
/* ************************ */
update_leader (i_leader, i_opposite) =
  PRE
    i_leader : BOOL &
    i_opposite : BOOL &

/* according to the informal specification there is a guarantee that
there will never be inconsistency in these entries, so, there is no
need to deal with this inconsistency in the SOFT system */
    not (i_leader = TRUE & i_opposite = TRUE)
  THEN
    leader := i_leader ||
    opposite := i_opposite ||
    IF i_leader = TRUE THEN
      cabin := LEADER
    ELSIF i_opposite = TRUE THEN
      cabin := OPPOSITE
    ELSE
      cabin := NONE
    END
  END

END