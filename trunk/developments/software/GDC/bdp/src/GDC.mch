MACHINE
        GDC
SEES
        Context ,
        Control ,
        BoolXor

ABSTRACT_VARIABLES
        slow_speed ,
        traction ,
        cabin ,
        leader , opposite ,
        conduction ,
        manual , controlled ,
        Leftcommand_mem ,
        Leftopen_doors , Leftclose_doors ,
        Lefttl_sensor_open ,
        Lefttl_command ,
        Leftvisual_open ,
        Rightcommand_mem ,
        Rightopen_doors , Rightclose_doors ,
        Righttl_sensor_open ,
        Righttl_command ,
        Rightvisual_open

INVARIANT
        slow_speed : BOOL & traction : BOOL &

        cabin : STATE & leader : BOOL & opposite : BOOL &
        not ( leader = TRUE & opposite = TRUE ) &
        ( ( leader = TRUE ) <=> ( cabin = LEADER ) ) &
        ( ( opposite = TRUE ) <=> ( cabin = OPPOSITE ) ) &
        ( ( ( leader = FALSE ) & ( opposite = FALSE ) ) <=> ( cabin = NONE ) ) &

        conduction : CONDUCTION & manual : BOOL & controlled : BOOL &
        ( ( conduction = MANUAL ) <=> ( ( manual = TRUE ) & ( controlled = FALSE ) ) ) &
        ( ( conduction = CONTROLLED ) <=> ( ( manual = FALSE ) & ( controlled = TRUE ) ) ) &
        ( ( conduction = CONDUCTION_OFF ) <=>
                        ( ( ( manual = FALSE ) & ( controlled = FALSE ) ) or
                         ( ( manual = TRUE ) & ( controlled = TRUE ) ) ) ) &
        Leftcommand_mem : COMMAND & Lefttl_command : COMMAND &
        Leftopen_doors : BOOL & Leftclose_doors : BOOL & Lefttl_sensor_open : BOOL & Leftvisual_open : BOOL &
        Leftvisual_open = Lefttl_sensor_open &
        Rightcommand_mem : COMMAND & Righttl_command : COMMAND &
        Rightopen_doors : BOOL & Rightclose_doors : BOOL & Righttl_sensor_open : BOOL & Rightvisual_open : BOOL &
        Rightvisual_open = Righttl_sensor_open &

        ( ( ( cabin = LEADER ) & ( conduction = MANUAL or conduction = CONTROLLED ) ) =>
                     ( ( slow_speed = FALSE =>
                        ( Lefttl_command = CLOSE_DOORS & Righttl_command = CLOSE_DOORS ) ) &
                 ( ( Lefttl_sensor_open = TRUE or Righttl_sensor_open = TRUE ) =>
                        traction = FALSE ) ) )

INITIALISATION
        slow_speed := TRUE || traction := TRUE || cabin := NONE ||
        leader := FALSE || opposite := FALSE ||
        conduction := MANUAL ||
        manual := TRUE || controlled := FALSE ||
        Leftcommand_mem := CLOSE_DOORS ||
        Lefttl_command := CLOSE_DOORS ||
        Leftopen_doors := FALSE || Leftclose_doors := TRUE ||
        Lefttl_sensor_open := FALSE ||
        Leftvisual_open := FALSE ||
        Rightcommand_mem := CLOSE_DOORS ||
        Righttl_command := CLOSE_DOORS ||
        Rightopen_doors := FALSE || Rightclose_doors := TRUE ||
        Righttl_sensor_open := FALSE ||
        Rightvisual_open := FALSE

OPERATIONS

update_controller ( i_open_left_doors , i_close_left_doors , i_tl_sensor_left_open ,
        i_tl_left_command , i_open_right_doors , i_close_right_doors ,
        i_tl_sensor_right_open , i_tl_right_command ,
        i_leader , i_opposite ,
        i_manual , i_controlled ,
        i_slow_speed ) =
        PRE
                i_open_left_doors : BOOL & i_close_left_doors : BOOL &
                i_tl_sensor_left_open : BOOL &
                i_open_right_doors : BOOL & i_close_right_doors : BOOL &
                i_tl_sensor_right_open : BOOL &
                i_leader : BOOL & i_opposite : BOOL &
                i_manual : BOOL & i_controlled : BOOL & i_slow_speed : BOOL &
                i_tl_left_command : COMMAND & i_tl_right_command : COMMAND &
                    not ( i_leader = TRUE & i_opposite = TRUE )
        THEN
        leader := i_leader || opposite := i_opposite ||
        IF i_leader = TRUE THEN
                cabin := LEADER
                ELSIF i_opposite = TRUE THEN
                cabin := OPPOSITE
        ELSE
                cabin := NONE
        END ||
        slow_speed := i_slow_speed ||
        manual := i_manual ||
        controlled := i_controlled ||
        IF ( i_manual = TRUE ) THEN
                IF ( i_controlled = TRUE ) THEN
                        conduction := CONDUCTION_OFF
                ELSE
                        conduction := MANUAL
                END
        ELSE
                IF ( i_controlled = TRUE ) THEN
                        conduction := CONTROLLED
                ELSE
                        conduction := CONDUCTION_OFF
                END
        END ||
        ANY cond WHERE
                cond : BOOL &
                cond = conduction_p ( i_leader , i_manual , i_controlled )
        THEN
                Leftopen_doors := i_open_left_doors ||
                Leftclose_doors := i_close_left_doors ||
                Lefttl_sensor_open := i_tl_sensor_left_open ||
                Leftvisual_open := i_tl_sensor_left_open ||
                Rightopen_doors := i_open_right_doors ||
                Rightclose_doors := i_close_right_doors ||
                Righttl_sensor_open := i_tl_sensor_right_open ||
                Rightvisual_open := i_tl_sensor_right_open ||
                IF cond = TRUE THEN
                        ANY cl WHERE
                                cl : COMMAND &
                                cl = select_command ( i_slow_speed , i_close_left_doors , i_open_left_doors , Leftcommand_mem )
                        THEN
                                IF cond = TRUE THEN
                                        Lefttl_command := cl
                                ELSE
                                        Lefttl_command := i_tl_left_command
                                END ||
                                IF cond = TRUE THEN
                                        Leftcommand_mem := cl
                                ELSE
                                        Leftcommand_mem := i_tl_left_command
                                END
                        END ||
                        ANY cr WHERE
                                cr : COMMAND &
                                cr = select_command ( i_slow_speed , i_close_right_doors , i_open_right_doors , Rightcommand_mem )
                        THEN
                                IF cond = TRUE THEN
                                        Righttl_command := cr
                                ELSE
                                        Righttl_command := i_tl_right_command
                                END ||
                                IF cond = TRUE THEN
                                        Rightcommand_mem := cr
                                ELSE
                                        Rightcommand_mem := i_tl_right_command
                                END
                        END
                END
        END ||
        IF i_tl_sensor_left_open = TRUE or i_tl_sensor_right_open = TRUE THEN
                traction := FALSE
        ELSE
                traction := TRUE
        END
        END
END
