# assembly program that implements the logic of the traffic light controller
# target:	PIC16C432
0:	BTFSC 0, 1 # if bit 1 of mem(0) is clear, skip next instruction
1:	GOTO 8     # jump to code handling 2
2:	BTFSC 0, 0 # if bit 0 of mem(0) is clear, skip next instruction
3:	GOTO 6     # jump to code handling 1
4:	MOVLW 1    # mem(0) should be 0, set working register to 1
5:	GOTO 9     # jump to code copying working register to mem(0)
6:	MOVLW 2    # mem(0) should be 1, set working register to 2
7:	GOTO 9     # jump to code copying working register to mem(0)
8:	MOVLW 0    # mem(0) should be 2, set working register to 0
9:	MOVWF 0    # copies working register to mem(0)
