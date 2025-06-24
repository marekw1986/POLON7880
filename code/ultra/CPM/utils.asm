; Various utils

OUT_CHAR:
		PUSH PSW
OUT_CHAR_WAIT:    
		IN   UART_8251_CTRL             ;COME HERE TO DO OUTPUT
        ANI  TxRDY_MASK                 ;STATUS BIT
        JZ   OUT_CHAR_WAIT              ;NOT READY, WAIT
        POP  PSW                        ;READY, GET OLD A BACK
        OUT  UART_8251_DATA             ;AND SEND IT OUT
		RET
    
MEMCOPY:
        MOV A, B                        ;Copy register B to register A
        ORA C                           ;Bitwise OR of register A and register C into register A
        RZ                              ;Return if the zero-flag is set high.
MC_LOOP:
        LDAX D                          ;Load A from the address pointed by DE
        MOV M, A                        ;Store A into the address pointed by HL
        INX D                           ;Increment DE
        INX H                           ;Increment HL
        DCX B                           ;Decrement BC   (does not affect Flags)
        MOV A, B                        ;Copy B to A    (so as to compare BC with zero)
        ORA C                           ;A = A | C      (set zero)
        JNZ MC_LOOP                     ;Jump to 'loop:' if the zero-flag is not set.   
        RET                             ;Return

NEWLINE:
		MVI A, 0DH
		CALL OUT_CHAR
		;MVI A, 0AH					; Comment out when LF implementation
		;CALL OUT_CHAR				; in VDP will be ready
		RET
		
PRN_ZERO_EX:
		MVI A, 48					;48 is 0 in ASCII
		CALL OUT_CHAR
		MVI A, 120					;120 is x in ASCII
		CALL OUT_CHAR
		RET
		
IPUTS:
		XTHL
		CALL PUTS_LOOP
		INX H
		XTHL
		RET
		
PUTS:
		PUSH H
		PUSH D
		CALL PUTS_LOOP
		POP D
		POP H
		RET
PUTS_LOOP:
		MOV D, H
		MOV E, L
		LDAX D
		CPI 00H
		RZ					; If a is zero, return
		CALL OUT_CHAR
		INX H
		JMP PUTS_LOOP

; Checks if 32 variable pointed by DL is zero		
ISZERO32BIT:
		LDAX D
		CPI 00H
		RNZ
		INX D
		LDAX D
		CPI 00H
		RNZ
		INX D
		LDAX D
		CPI 00H
		RNZ
		INX D
		LDAX D
		CPI 00H
		RET
		
; CRC-16/ARC for 8080/Z80
; On entry HL = old CRC, A = byte
; On exit HL = new CRC, A,B undefined
CRC16_ARC_F:
        XRA     L
        MOV     L,A
        RRC
        RRC
        JPO     BLUR
        ANA     A
BLUR:   JPE     BLUR1
        STC
BLUR1:  RAR
        ANI     0E0H
        RAL
        MOV     B,A
        RAL
        XRA     B
        XRA     H
        MOV     B,A
        XRA     H
        RAR
        MOV     A,L
        RAR
        MOV     L,A
        ANA     A
        RAR
        XRA     L
        MOV     L,B
        MOV     H,A
        RET
        
; Checks if 32bit values pointed by HL and DE are equal        
IS32BIT_EQUAL:
    MVI B, 4        		; Set counter for 4 bytes
    MVI A, 1        		; Assume values are equal (flag set to 1)
IS32BIT_EQUAL_LOOP:
    LDAX D          		; Load byte from second value (ADDR2)
    CMP M           		; Compare with byte at first value (ADDR1)
    JNZ IS32BIT_NOT_EQUAL   ; If not equal, jump to NOT_EQUAL
    INX H           		; Move to next byte in ADDR1
    INX D           		; Move to next byte in ADDR2
    DCR B           		; Decrement byte counter
    JNZ IS32BIT_EQUAL_LOOP  ; Repeat until all bytes are checked
    RET             		; Return with A = 1 (equal)
IS32BIT_NOT_EQUAL:
    MVI A, 0        		; Set A = 0 (not equal)
    RET             		; Return

;THIS IS JUSY ENDLESS LOOP. Go here if something is wrong.		
ENDLESS_LOOP:
		NOP
		JMP ENDLESS_LOOP
        
