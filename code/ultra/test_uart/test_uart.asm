;IRQ0 - KBD
;IRQ1 - UART_TX
;IRQ2 - UART_RX
;IRQ3 - RTC
;IRQ4 - TIMER
;IRQ5 - UNUSED
;IRQ6 - UNUSED
;IRQ7 - UNUSED

		INCL "../common/definitions.asm"

        ORG  0000H
        JMP  SET_PC
SET_PC:
        MVI  A, 80H
        OUT  PORT_74273
START:  LXI  H,STACK
		SPHL
		JMP INIT
		
		INCL "../common/utils.asm"
		INCL "../common/hexdump.asm"

INIT:
        ; Reset receiver
        MVI A, 20H
        OUT SCC2681_CRA
        ; Reset transmitter
        MVI A, 30H
        OUT SCC2681_CRA
        ; Reset MR pointer
        MVI A, 10H
        OUT SCC2681_CRA

        ; MR1A: 8-bit, no parity
        MVI A, 13H
        OUT SCC2681_MR1A
        ; MR2A: 1 stop, no CTS
        MVI A, 07H
        OUT SCC2681_MR1A

        ; ACR: BRG set 2
        MVI A, 80H
        OUT SCC2681_ACR

        ; CSRA: 0xBB (9600) for RX and TX
        MVI A, 0BBH
        OUT SCC2681_CSRA

        ; Enable TX and RX
        MVI A, 05H
        OUT SCC2681_CRA

        ;Initialize 8259
        MVI  A, 0FFH					;ICW1 - LSB of IR0_VECT = 0xE0, level triggered, 4 byte intervals, one 8259, ICW4 needed
        OUT  PIC_8259_LOW				;ICW1 is written to the low port of 8259
        MVI  A, 0FFH					;ICW2, MSB of IR0_VECT
        OUT	 PIC_8259_HIGH				;ICW2 is written to the high port of 8259
        MVI  A, 02H						;ICW4 - NOT special full nested mode, not buffored, master, automatic EOI, 8080 processor
        OUT  PIC_8259_HIGH				;ICW4 is written to the high port of 8259        
        MVI  A, 0FFH					;OCW1 All interrupts disabled
        OUT  PIC_8259_HIGH				;OCW1 is written to the high port of 8259
        MVI  A, 80H						;OCW2 - Rotation of priorities, no explicit EOI
        OUT  PIC_8259_LOW				;OCW2 is written to the low port of 8259
;        MVI  A, 4BH				    ;OCW3 - ESMM SMM RESET SPECIAL MASK, NO POLL COMMAND, RR_RIS_READ_IS_REG
;        OUT  PIC_8259_LOW				;OCW3 is written to the low port of 8259

		DI
		
LOOP:
		MVI A, 55H
        CALL OUT_CHAR
        
        MVI C, 255
		CALL DELAY
		
		;MVI A, 0FAH
		;CALL HEXDUMP_A
		
		JMP LOOP

        
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
        POP D
        POP H        
		POP PSW							;Restore machine status
        EI                              ;Re-enable interrupts
		RET								;Return to interrupted program						;Return to interrupted program

TIMER_ISR:
		PUSH PSW						;Save condition bits and accumulator
        PUSH H
        PUSH D
        POP D
        POP H        
		POP PSW							;Restore machine status
        EI                              ;Re-enable interrupts
		RET		
		
RTC_ISR:
		PUSH PSW						;Save condition bits and accumulator
        PUSH H
        PUSH D
        XRA A                      ;Clear the RTC interrupt flag to change state of the line
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
		ORG  0FFE0H
IR0_VECT:
		;JMP UART_TX_ISR
        ;NOP
        EI
        RET
        NOP
        NOP
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
		;JMP UART_TX_ISR
        ;NOP
        EI
        RET
        NOP
        NOP
IR4_VECT:
		;JMP UART_TX_ISR
        ;NOP
        EI
        RET
        NOP
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
        
        ORG  0FFD0H
STACK:  DS   0                          ;STACK STARTS HERE
;

CR      EQU  0DH
LF      EQU  0AH

		END
