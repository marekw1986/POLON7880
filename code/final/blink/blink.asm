;IRQ0 - KBD
;IRQ1 - UART_TX
;IRQ2 - UART_RX
;IRQ3 - RTC
;IRQ4 - TIMER
;IRQ5 - UNUSED
;IRQ6 - UNUSED
;IRQ7 - UNUSED

		INCL "../common/definitions.asm"
		
FULLSYS EQU 0

        ORG  0000H
        JMP  SET_PC
SET_PC:
		MVI  A, 80H
        OUT  PORT_8212
START:  LXI  H,STACK
		SPHL
		JMP INIT
		
		INCL "../common/utils.asm"

INIT:
	IF FULLSYS
        ;Initialize 8253
		MVI  A, 30H                     ;TIMER0 - systick
		OUT  CONTR_W_8253               ;Timer 0, write LSB then MSB, mode 0, binary 
		MVI  A, 60H                     ;LSB, interrupt every 20ms
		OUT  COUNT_REG_0_8253
		MVI  A, 0EAH                    ;MSB, interrupt every 20ms (0xF0 for 30 ms)
		OUT  COUNT_REG_0_8253	
		MVI  A, 0B6H                    ;TIMER2 - baudrate generator for 8251
		OUT CONTR_W_8253                ;Timer 2, write LSB then MSB, mode 3, binary
		MVI  A, 13H                     ;LSB
		OUT  COUNT_REG_2_8253
		XRA A                     ;MSB
		OUT  COUNT_REG_2_8253          
        ;Initialize 8251
        MVI	 A, 4EH
        OUT	 UART_8251_CTRL
        MVI	 A, 27H
        OUT	 UART_8251_CTRL
        ;Initialize 8259
        MVI  A, 0FFH					;ICW1 - LSB of IR0_VECT = 0xE0, level triggered, 4 byte intervals, one 8259, ICW4 needed
        OUT  PIC_8259_LOW				;ICW1 is written to the low port of 8259
        MVI  A, 0FFH					;ICW2, MSB of IR0_VECT
        OUT	 PIC_8259_HIGH				;ICW2 is written to the high port of 8259
        MVI  A, 02H						;ICW4 - NOT special full nested mode, not buffored, master, automatic EOI, 8080 processor
        OUT  PIC_8259_HIGH				;ICW4 is written to the high port of 8259        
        MVI  A, 0EFH					;OCW1 active TIMER; RTC and KBD and UART interrupts disabled
        OUT  PIC_8259_HIGH				;OCW1 is written to the high port of 8259
        MVI  A, 80H						;OCW2 - Rotation of priorities, no explicit EOI
        OUT  PIC_8259_LOW				;OCW2 is written to the low port of 8259
;        MVI  A, 4BH				    ;OCW3 - ESMM SMM RESET SPECIAL MASK, NO POLL COMMAND, RR_RIS_READ_IS_REG
;        OUT  PIC_8259_LOW				;OCW3 is written to the low port of 8259
        ;Initialize M6442B RTC
;        MVI  A, 04H                     ;30 AJD = 0, IRQ FLAG = 1 (required), BUSY = 0(?), HOLD = 0
;        OUT  RTC_CTRLD_REG
;        MVI  A, 06H                     ;Innterrupt mode, STD.P enabled, 1 s.
;        OUT  RTC_CTRLE_REG
;        MVI  A, 04H                     ;TEST = 0, 24h mode, STOP = 0, RESET = 0
;        OUT  RTC_CTRLF_REG
		EI
	ENDIF
		
LOOP:
		MVI A, 40H
		OUT PORT_8212
		MVI C, 255
		CALL DELAY
		MVI A, 80H
		OUT PORT_8212
		MVI C, 255
		CALL DELAY
		JMP LOOP

        
;       ORG  1366H
;		ORG  1F00H
		ORG	 8000H
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
        
        ORG  0FFDFH
STACK:  DS   0                          ;STACK STARTS HERE
;

CR      EQU  0DH
LF      EQU  0AH

		END

