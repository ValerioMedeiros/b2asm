MACHINE
   data_refinement
SEES
   definitions
CONSTANTS
   color_refine, color_step
PROPERTIES
   color_refine : COLOR --> NATURAL &
   color_refine = { green |-> 0, yellow |-> 1, red |-> 2} &
   color_step : 0..2 --> 0..2 &
   color_step = { 0 |-> 1, 1 |-> 2, 2 |-> 0 }
ASSERTIONS
   color_refine (green) = 0 &
   color_refine (yellow) = 1 &
   color_refine (red) = 2 &
   color_step (0) = 1 &
   color_step (1) = 2 &
   color_step (2) = 0
END