		INCL "definitions.asm"

		ORG  0000H
START:  LXI  H,STACK
		SPHL
		JMP LOOP

		;INCL "vdp.asm"
		;INCL "utils.asm"
		;INCL "hexdump.asm"
		INCL "labels.asm"
		
LOOP:
		MVI C, 32
		CALL DELAY
		MVI A, 0FAH
		CALL HEXDUMP_A
		JMP LOOP
		
;       ORG  1366H
;		ORG  1F00H
		ORG	 4400H
TXTEND: DS   0                          ;TEXT SAVE AREA ENDS
VARBGN: DS   55                         ;VARIABLE @(0)
BUFFER: DS   64                         ;INPUT BUFFER
BUFEND: DS   1
CFLBA3	DS	 1
CFLBA2	DS	 1
CFLBA1	DS	 1
CFLBA0	DS	 1                          ;BUFFER ENDS
BLKDAT: DS   512                        ;BUFFER FOR SECTOR TRANSFER
BLKENDL DS   1                          ;BUFFER ENDS
SYSTICK DS   2                          ;Systick timer
RTCTICK DS   2							;RTC tick timer/uptime
KBDDATA DS   1                          ;Keyboard last received code
KBDKRFL DS	 1							;Keyboard key release flag
KBDSFFL DS	 1							;Keyboard Shift flag
KBDOLD	DS	 1							;Keyboard old data
KBDNEW	DS	 1							;Keyboard new data
CURSOR  DS   2                          ;VDP cursor x position
STKLMT: DS   1                          ;TOP LIMIT FOR STACK
;       ORG  1400H
        ORG  7FFFH
STACK:  DS   0                          ;STACK STARTS HERE
;

CR      EQU  0DH
LF      EQU  0AH

		END
