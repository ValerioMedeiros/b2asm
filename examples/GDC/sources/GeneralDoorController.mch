MACHINE
    GeneralDoorController

SEES
    Context
  , Control

INCLUDES

    LeaderDefinition
  , ConductionModeDefinition
  , Left.SideDoorController
  , Right.SideDoorController

VARIABLES
/* ************************ */
/* input variable representing a monitored 
   environment variable */
/* ************************ */
    slow_speed

/* ************************ */
/* output variable */
/* ************************ */
  , traction

INVARIANT

/* ************************ */
/* typing */
/* ************************ */
    slow_speed : BOOL
  & traction : BOOL

/* ************************ */
/* consistency between the internal state variables 
   and the input and output variables */
/* ************************ */
  & (((cabin = LEADER) & (conduction = MANUAL or conduction = CONTROLLED)) =>
     (  (slow_speed = FALSE => 
         (Left.tl_command = CLOSE_DOORS & 
          Right.tl_command = CLOSE_DOORS))
      & ((Left.tl_sensor_open = TRUE or 
          Right.tl_sensor_open = TRUE) => traction = FALSE)))

INITIALISATION
    slow_speed := TRUE ||
    traction := TRUE

OPERATIONS

/* ************************ */
/* update_controller has thirteen boolean arguments corresponding to
all monitored variables; updates (directly or indirectly) the
corresponding input variables, checks whether the current cabin is to
issue commands, defines which command has to be issued, if any, and
sets the internal state variables and outputs accordingly. */
/* ************************ */
update_controller (i_open_left_doors, i_close_left_doors
    , i_tl_sensor_left_open, i_tl_left_command
    , i_open_right_doors, i_close_right_doors
    , i_tl_sensor_right_open, i_tl_right_command
    , i_leader, i_opposite
    , i_manual, i_controlled
    , i_slow_speed) =
  PRE
      i_open_left_doors : BOOL
    & i_close_left_doors : BOOL
    & i_tl_sensor_left_open : BOOL
    & i_open_right_doors : BOOL
    & i_close_right_doors : BOOL
    & i_tl_sensor_right_open : BOOL
    & i_leader : BOOL
    & i_opposite : BOOL
    & i_manual : BOOL
    & i_controlled : BOOL
    & i_slow_speed : BOOL
    & i_tl_left_command : COMMAND
    & i_tl_right_command : COMMAND
    & not (i_leader = TRUE & i_opposite = TRUE)
  THEN
    update_leader (i_leader, i_opposite) ||
    update_conduction (i_manual, i_controlled) ||
    slow_speed := i_slow_speed ||
    ANY cond WHERE
        cond : BOOL
      & cond = conduction_p(i_leader, i_manual, i_controlled)
    THEN
      Left.update_controller(cond
        , i_open_left_doors, i_close_left_doors
        , i_tl_sensor_left_open, i_tl_left_command
        , i_slow_speed) ||
      Right.update_controller(cond
        , i_open_right_doors, i_close_right_doors
        , i_tl_sensor_right_open, i_tl_right_command
        , i_slow_speed)
    END ||
    IF i_tl_sensor_left_open = TRUE or i_tl_sensor_right_open = TRUE THEN
      traction := FALSE
    ELSE
      traction := TRUE
    END
  END
END

