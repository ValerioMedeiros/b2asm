MACHINE
    SideDoorController

SEES
    Context
  , Control

VARIABLES

/* ************************ */
/* internal state variables */
/* ************************ */
    command_mem  /* memorizes commands */

/* ************************ */
/* input variables representing the monitored 
   environment inputs */
/* ************************ */
  , open_doors
  , close_doors
  , tl_sensor_open

/* ************************ */
/* output variables */
/* ************************ */
  , tl_command
  , visual_open

INVARIANT
/* ************************ */
/* typing */
/* ************************ */
    command_mem : COMMAND
  & tl_command : COMMAND
  & open_doors : BOOL
  & close_doors : BOOL
  & tl_sensor_open : BOOL
  & visual_open : BOOL
  
/* ************************ */
/* functional requirements */
/* ************************ */
  & visual_open = tl_sensor_open

INITIALISATION
  command_mem := CLOSE_DOORS ||
  tl_command := CLOSE_DOORS || 
  open_doors := FALSE ||
  close_doors := TRUE ||
  tl_sensor_open := FALSE ||
  visual_open := FALSE

OPERATIONS

/* ************************ */
/* update_controller has five boolean arguments corresponding to the
monitored variables and a boolean parameter defining if the current
cabin is supposed to produce commands or not; updates the
corresponding input variables, defines which command has to be issued,
if any, and sets the internal state variables accordingly. */
/* ************************ */
update_controller (cond
    , i_open_doors, i_close_doors
    , i_tl_sensor_open, i_tl_command
    , i_slow_speed) =
  PRE
      cond : BOOL
    & i_open_doors : BOOL
    & i_close_doors : BOOL
    & i_tl_sensor_open : BOOL
    & i_slow_speed : BOOL
    & i_tl_command : COMMAND
  THEN
    open_doors := i_open_doors ||
    close_doors := i_close_doors ||
    tl_sensor_open := i_tl_sensor_open ||
    IF cond = TRUE THEN
      ANY cc WHERE
          cc : COMMAND
        & cc = select_command(i_slow_speed, i_close_doors, i_open_doors, command_mem)
      THEN
        IF cond = TRUE THEN
          tl_command := cc
        ELSE
          tl_command := i_tl_command
        END ||
        IF cond = TRUE THEN
          command_mem := cc
        ELSE
          command_mem := i_tl_command
        END
      END
    END ||
    visual_open := i_tl_sensor_open
  END
END