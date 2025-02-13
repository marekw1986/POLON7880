;IRQ0 - KBD
;IRQ1 - UART_TX
;IRQ2 - UART_RX
;IRQ3 - RTC
;IRQ4 - TIMER
;IRQ5 - UNUSED
;IRQ6 - UNUSED
;IRQ7 - UNUSED

		INCL "../common/definitions.asm"

        ORG  0C000H
        JMP  SET_PC
SET_PC:
		MVI  A, 04H
        OUT  PORT_74237
START:  LXI  H,STACK
		SPHL
		JMP INIT
		
		INCL "../common/utils.asm"
		INCL "../common/keyboard.asm"

INIT:
        ;Set SYSTICK, RTCTICK and KBDDATA to 0x00
        LXI  H, 0000H
        SHLD SYSTICK
        LXI  H, 0000H
        SHLD RTCTICK
        MVI A, 00H
        STA  KBDDATA
        ;Initialize 8253
		MVI  A, 30H                     ;TIMER0 - systick
		OUT  CONTR_W_8253               ;Timer 0, write LSB then MSB, mode 0, binary 
		MVI  A, 00H                     ;LSB, interrupt every 20ms
		OUT  COUNT_REG_0_8253
		MVI  A, 0A0H                    ;MSB, interrupt every 20ms (0xF0 for 30 ms)
		OUT  COUNT_REG_0_8253	
		MVI  A, 0B6H                    ;TIMER2 - baudrate generator for 8251
		OUT CONTR_W_8253                ;Timer 2, write LSB then MSB, mode 3, binary
		MVI  A, 0DH                     ;LSB
		OUT  COUNT_REG_2_8253
		MVI  A, 00H                     ;MSB
		OUT  COUNT_REG_2_8253          
        ;Initialize 8251
        MVI  A, 00H
        OUT  UART_8251_CTRL
        MVI  A, 00H
        OUT  UART_8251_CTRL
        MVI  A, 00H
        OUT  UART_8251_CTRL
        MVI  A, 40H						;Initiate UART reset
        OUT  UART_8251_CTRL
        MVI	 A, 4EH						;Mode: 8 data, 1 stop, x16
        OUT	 UART_8251_CTRL
        MVI	 A, 37H
        OUT	 UART_8251_CTRL
        ;Initialize 8259
        MVI  A, 0FFH					;ICW1 - LSB of IR0_VECT = 0xE0, level triggered, 4 byte intervals, one 8259, ICW4 needed
        OUT  PIC_8259_LOW				;ICW1 is written to the low port of 8259
        MVI  A, 0FFH					;ICW2, MSB of IR0_VECT
        OUT	 PIC_8259_HIGH				;ICW2 is written to the high port of 8259
        MVI  A, 02H						;ICW4 - NOT special full nested mode, not buffored, master, automatic EOI, 8080 processor
        OUT  PIC_8259_HIGH				;ICW4 is written to the high port of 8259        
        MVI  A, 0EEH					;OCW1 active TIMER and KBD; RTC and UART interrupts disabled
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
TRYKBINIT:
        CALL KBDINIT                        ;Call init routine
        MOV A, B							;Move result of operation to A
        CPI 00H								;Check if OK
        JNZ TRYKBINIT						;Retry if not ok. TODO add limit of retries
		EI
		
LOOP:
		PUSH B
		PUSH D
		PUSH H
		CALL KBD2ASCII
		POP H
		POP D
		POP B
		CPI 00H
		JZ LOOP
		CALL OUT_CHAR
		JMP LOOP
		
		INCL "../common/ps2_scancodes.asm"

        
;Interrupt routines
UART_RX_ISR:
		PUSH PSW						;Save condition bits and accumulator
        PUSH H
        PUSH D
        POP D
        POP H        
		POP PSW							;Restore machine status
        EI                              ;Re-enable interrupts
		RET								;Return to interrupted program

UART_TX_ISR:
		PUSH PSW						;Save condition bits and accumulator
        PUSH H
        PUSH D
        POP D
        POP H        
		POP PSW							;Restore machine status
        EI                              ;Re-enable interrupts
		RET								;Return to interrupted program

KBD_ISR:
		PUSH PSW						;Save condition bits and accumulator
        PUSH H
        PUSH D
        ;IN KBD_STATUS                  ;NO NEED TO TEST, INTERRUPT MODE!
        ;ANI 01H                         ;Check if output buffer full
        ;JZ KBD_ISR_RET                  ;Output buffer empty, end ISR
        IN KBD_DATA                     ;Get keyboard data
        STA KBDDATA                     ;Save received code
KBD_ISR_RET:        
        POP D
        POP H        
		POP PSW							;Restore machine status
        EI                              ;Re-enable interrupts
		RET								;Return to interrupted program

TIMER_ISR:
		PUSH PSW						;Save condition bits and accumulator
        PUSH H
        PUSH D
        LHLD SYSTICK                    ;Load SYSTICK variable to HL
        INX H                           ;Increment HL
        SHLD SYSTICK                    ;Save HL in SYSTICK variable
 	 	MVI  A, 00H                     ;Reload. LSB, interrupt every 20ms
  		OUT  COUNT_REG_0_8253
  		MVI  A, 0A0H                    ;Reload. MSB, interrupt every 20ms (0xF0 for 30 ms)
  		OUT  COUNT_REG_0_8253                
        POP D
        POP H        
		POP PSW							;Restore machine status
        EI                              ;Re-enable interrupts
		RET								;Return to interrupted program
		
RTC_ISR:
		PUSH PSW						;Save condition bits and accumulator
        PUSH H
        PUSH D
        MVI A, 00H                      ;Clear the RTC interrupt flag to change state of the line
        OUT RTC_CTRLD_REG
        LHLD RTCTICK                    ;Load RTCTICK variable to HL
        INX H                           ;Increment HL
        SHLD RTCTICK                    ;Save HL in RTCTICK variable        
        POP D
        POP H        
		POP PSW							;Restore machine status
        EI                              ;Re-enable interrupts
		RET								;Return to interrupted program

;Interrupt vectors
IR0_VECT:
		ORG  0FFE0H
		JMP KBD_ISR
        NOP
        ;EI
        ;RET
        ;NOP
        ;NOP        
IR1_VECT:
		;JMP UART_TX_ISR
        ;NOP
        EI
        RET
        NOP
        NOP
IR2_VECT:
		;JMP UART_RX_ISR
        ;NOP
        EI
        RET
        NOP
        NOP
IR3_VECT:
		JMP RTC_ISR
        NOP
IR4_VECT:
		JMP TIMER_ISR
        NOP
IR5_VECT:
        EI	
        RET
        NOP
        NOP
IR6_VECT:
        EI	
        RET
        NOP
        NOP
IR7_VECT
        EI	
        RET
        NOP
        NOP
        
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
        
        ORG  7FFFH
STACK:  DS   0                          ;STACK STARTS HERE
;

CR      EQU  0DH
LF      EQU  0AH

		END
