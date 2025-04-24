;*************************************************************
; 
;                 TINY BASIC FOR INTEL 8080
;                       VERSION 2.0
;                     BY LI-CHEN WANG
;                  MODIFIED AND TRANSLATED
;                    TO INTEL MNEMONICS
;                     BY ROGER RAUSKOLB
;                      10 OCTOBER,1976
;                        @COPYLEFT
;                   ALL WRONGS RESERVED
; 
;*************************************************************
; 
; *** ZERO PAGE SUBROUTINES ***
; 
; THE 8080 INSTRUCTION SET LETS YOU HAVE 8 ROUTINES IN LOW
; MEMORY THAT MAY BE CALLED BY RST N, N BEING 0 THROUGH 7.
; THIS IS A ONE BYTE INSTRUCTION AND HAS THE SAME POWER AS
; THE THREE BYTE INSTRUCTION CALL LLHH.  TINY BASIC WILL
; USE RST 0 AS START AND RST 1 THROUGH RST 7 FOR
; THE SEVEN MOST FREQUENTLY USED SUBROUTINES.
; TWO OTHER SUBROUTINES (CRLF AND TSTNUM) ARE ALSO IN THIS
; SECTION.  THEY CAN BE REACHED ONLY BY 3-BYTE CALLS.
;

IR_VECTORS_RAM EQU 0FFE0H
STACK          EQU IR_VECTORS_RAM-1

		INCL "../common/definitions.asm"

        ORG  0000H
START:  LXI  H,STACK                   ;*** COLD START ***
		SPHL
        MVI  A,0FFH
        JMP  INIT
;

		INCL "../common/cf.asm"
		INCL "keyboard.asm"
		INCL "../common/utils.asm"
		INCL "../common/hexdump.asm"

        ;Set SYSTICK, RTCTICK and KBDDATA to 0x00
INIT:   LXI  H, 0000H
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
        MVI  A, 0EFH					;OCW1 active TIMER; RTC, KBD and UART interrupts disabled
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
        		
        LXI B, 32                       ;BYTES TO TRANSFER
        LXI D, IR_VECTORS_ROM           ;SOURCE
        LXI H, IR_VECTORS_RAM           ;DESTINATION
        CALL MEMCOPY

        ; Wait before initializing CF card
		MVI C, 255
		CALL DELAY
        MVI C, 255
		CALL DELAY
		MVI C, 255
		CALL DELAY
		MVI C, 255
		CALL DELAY
        
		CALL IPUTS
		DB 'CF CARD: '
		DB 00H
		CALL CFINIT
		CPI 00H								; Check if CF_WAIT during initialization timeouted
		JZ GET_CFINFO
		CALL IPUTS
		DB 'missing'
		DB 00H
		CALL NEWLINE
		JMP $
GET_CFINFO:
        CALL CFINFO
        CALL IPUTS
        DB 'Received MBR: '
        DB 00H
        CALL CFGETMBR
        ; HEXDUMP MBR - START
        ;LXI D, LOAD_BASE
        ;MVI B, 128
        ;CALL HEXDUMP
        ;LXI D, LOAD_BASE+128
        ;MVI B, 128
        ;CALL HEXDUMP
        ;LXI D, LOAD_BASE+256
        ;MVI B, 128
        ;CALL HEXDUMP
        ;LXI D, LOAD_BASE+384
        ;MVI B, 128
        ;CALL HEXDUMP
        ;CALL NEWLINE
        ; HEXDUMP MBR - END
        ; Check if MBR is proper
        LXI D, LOAD_BASE+510
        LDAX D
        CPI 55H
        JNZ LOG_FAULTY_MBR
        INX D
        LDAX D
        CPI 0AAH
        JNZ LOG_FAULTY_MBR
        JMP LOG_PARTITION_TABLE
LOG_FAULTY_MBR:
		CALL IPUTS
		DB 'ERROR: faulty MBR'
		DB 00H
		CALL NEWLINE
        JMP $
LOG_PARTITION_TABLE:
		CALL IPUTS
		DB 'Partition table'
		DB 00H
        CALL NEWLINE
        CALL PRN_PARTITION_TABLE
        CALL NEWLINE
        ; Check if partition 1 is present
        LXI D, LOAD_BASE+446+8		; Address of first partition
        CALL ISZERO32BIT
        JNZ CHECK_PARTITION1_SIZE
        CALL IPUTS
		DB 'ERROR: partition 1 missing'
		DB 00H
        CALL NEWLINE
        JMP $
CHECK_PARTITION1_SIZE:
		; Check if partition 1 is larger than 16kB (32 sectors)
		LXI D, LOAD_BASE+446+12		; First partition size
		LDAX D
		CPI 32						; Check least significant byte
		JZ BOOT_CPM ;PRINT_BOOT_OPTIONS		; It is equal. Good enough.
		JNC BOOT_CPM ;PRINT_BOOT_OPTIONS		; It is bigger
		INX D
		LDAX D
		CPI 00H
		JNZ BOOT_CPM ;PRINT_BOOT_OPTIONS
		INX D
		LDAX D
		CPI 00H
		JNZ BOOT_CPM ;PRINT_BOOT_OPTIONS
		INX D
		LDAX D
		CPI 00H
		JNZ BOOT_CPM ;PRINT_BOOT_OPTIONS
		CALL IPUTS
		DB 'ERROR: partition 1 < 16kB'
		DB 00H
		CALL NEWLINE
		JMP $
        
BOOT_CPM:
		DI
        CALL LOAD_PARTITION1
        CPI 00H
        JZ JUMP_TO_CPM
        CALL IPUTS
        DB 'CP/M load error. Reset.'
        DB 00H
        CALL ENDLESS_LOOP
JUMP_TO_CPM:
        CALL NEWLINE
        CALL IPUTS
        DB 'Load successfull.'
        DB 00H
        CALL NEWLINE
        JMP BIOS_ADDR
        
MSG1:   DB   'TINY '
        DB   'BASIC'
        DB   CR
CFERRM: DB   'CF ERROR: '
        DB   CR
STARTADDRSTR:
		DB	 'Addr: '
		DB	 CR
SIZESTR:
		DB	 'Size: '
		DB	 CR

		INCL "fonts1.asm"
		INCL "ps2_scancodes.asm"
        
;Interrupt vectors defined in rom
IR_VECTORS_ROM:
IR0_VECT_ROM:
		JMP KBD_ISR
        NOP
        ;EI
        ;RET
        ;NOP
        ;NOP        
IR1_VECT_ROM:
		;JMP UART_TX_ISR
        ;NOP
        EI
        RET
        NOP
        NOP
IR2_VECT_ROM:
		;JMP UART_RX_ISR
        ;NOP
        EI
        RET
        NOP
        NOP
IR3_VECT_ROM:
		JMP RTC_ISR
        NOP
IR4_VECT_ROM:
		JMP TIMER_ISR
        NOP
IR5_VECT_ROM:
        EI	
        RET
        NOP
        NOP
IR6_VECT_ROM:
        EI	
        RET
        NOP
        NOP
IR7_VECT_ROM:
        EI	
        RET
        NOP
        NOP

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

;       ORG  1366H
;       ORG  1F00H
		ORG	 0FBDFH
SYSTEM_VARIABLES:
BLKDAT: DS   512                        ;BUFFER FOR SECTOR TRANSFER
BLKENDL DS   0                          ;BUFFER ENDS
CFLBA3	DS	 1
CFLBA2	DS	 1
CFLBA1	DS	 1
CFLBA0	DS	 1                          
SYSTICK DS   2                          ;Systick timer
RTCTICK DS   2							;RTC tick timer/uptime
KBDDATA DS   1                          ;Keyboard last received code
KBDKRFL DS	 1							;Keyboard key release flag
KBDSFFL DS	 1							;Keyboard Shift flag
KBDOLD	DS	 1							;Keyboard old data
KBDNEW	DS	 1							;Keyboard new data
STKLMT: DS   1                          ;TOP LIMIT FOR STACK

CR      EQU  0DH
LF      EQU  0AH

        END
