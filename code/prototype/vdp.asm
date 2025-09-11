VDPINIT:
        XOR A
        STA CURSOR                      ;Set CURSOR to 0
        STA CURSOR+1                    
        XOR A                      ;REG0 (TEXT MODE, NO EXTERNAL VIDEO)
        OUT VDP_MODE
        MVI A, 80H                      ;SELECT REG0
        OUT VDP_MODE

        MVI A, 0D0H                     ;REG1 (16K, ENABLE DISP, DISABLE INT)
        OUT VDP_MODE
        MVI A, 81H                      ;SELECT REG1
        OUT VDP_MODE

        MVI A, 02H                      ;REG2 (ADDRESS OF NAME TABLE IN VRAM = 0x0800)
        OUT VDP_MODE
        MVI A, 82H                      ;SELECT REG2
        OUT VDP_MODE
 
        XOR A                      ;REG4 (ADDRESS OF PATTERN TABLE IN VRAM = 0x0000)
        OUT VDP_MODE
        MVI A, 84H                      ;SELECT REG4
        OUT VDP_MODE
        
        ;CLEAR VRAM
		LXI D, 0000H					;ZERO VRAM STARTING FROM 0x0000
		LXI H, 4000H					;ZERO 16kB
		CALL VDPZEROVRAM        
        
        ; INITIALIZE VRAM
        LXI B, CHARS
        LXI D, 0000H + (32*8)
        LXI H, CHARS_END-CHARS
        CALL VDPWVRAM
        
;        MVI A, 20H                      ;REG5 (ADDRESS OF SPRITE ATTRIBUTE TABLE IN VRAM = 0x1000)
;        OUT VDP_MODE
;        MVI A, 85H                      ;SELECT REG5
;        OUT VDP_MODE
;        MVI A, 00H                      ;REG6 (ADDRESS OF SPRITE PATTERN TABLE IN VRAM = 0x0000)
;        OUT VDP_MODE
;        MVI A, 86H                      ;SELECT REG6
;        OUT VDP_MODE

        MVI A, 0F1H                     ;REG7 (WHITE TEXT ON BLACK BACKGROUND)
        OUT VDP_MODE
        MVI A, 87H                      ;SELECT REG7
        OUT VDP_MODE 
        RET


;CLEAR SCREEN
VDPCLS:
		LXI D, 0800H					;ZERO VRAM STARTING FROM 0x0800
		LXI H, 03C0H					;ZERO 16kB
		CALL VDPZEROVRAM
		RET
		

;VRAM ADDRES IN DE, DATA LENGTH IN HL        
VDPZEROVRAM:
        MOV A, E
        OUT VDP_MODE
        NOP
        NOP
        MOV A, D
        ORI 40H
        OUT VDP_MODE
        NOP
        NOP
VDPZEROVRAML:
        XOR A
        OUT VDP_DATA
        NOP
        NOP
        DCX H
        MOV A, H
        ORA L          
        JNZ VDPZEROVRAML
        RET
        
        
VDPSCROLLUP:
		;Mowe 12 lines
		;Read them first
        LXI B, VDPBUF
        LXI D, 0828H
        LXI H, 01E0H
		CALL VDPRVRAM
		;Move lines from buffer to the beginning of the screen
        LXI B, VDPBUF
        LXI D, 0800H
        LXI H, 01E0H
        CALL VDPWVRAM		
        ;Move remaining 11 lines
        ;Read them first
        LXI B, VDPBUF
        LXI D, 0A08H
        LXI H, 01B8H
		CALL VDPRVRAM
		;Write those lines to the middle of the screen
        LXI B, VDPBUF
        LXI D, 09E0H
        LXI H, 01B8H
        CALL VDPWVRAM			
		;Clear last line
		LXI D, 0B98H
		LXI H, 0028H 
		CALL VDPZEROVRAM
		;Calculate new cursor position
		MVI A, 98H
		STA CURSOR
		MVI A, 03H
		STA CURSOR+1
		RET

;RAM ADDRESS IN BC, VRAM ADDRES IN DE, DATA LENGTH IN HL        
VDPWVRAM:
        MOV A, E
        OUT VDP_MODE
        NOP
        NOP
        MOV A, D
        ORI 40H
        OUT VDP_MODE
        NOP
        NOP
VDPWVRAML:
        LDAX B
        OUT VDP_DATA
        NOP
        NOP
        INX B
        DCX H
        MOV A, H
        ORA L          
        JNZ VDPWVRAML
        RET
        
;RAM ADDRES IN BC, VRAM ADDRES IN DE, DATA LENGTH IN HL
VDPRVRAM:
        MOV A, E
        OUT VDP_MODE
        NOP
        NOP
        MOV A, D
        OUT VDP_MODE
        NOP
        NOP
VDPRVRAML:
		IN VDP_DATA
		NOP
		NOP
        STAX B
        INX B
        DCX H
        MOV A, H
        ORA L        
        JNZ VDPRVRAML
        RET
        

;PUTS CHRACTER FROM  ON SCREEN, HANDLES CURSOR VALUE
VDPPUTC:
		CPI 08H							;Check if it is BACKSPACE
		JNZ VDPPUTC_CHLF				;It is not. Check for next special character
		LDA CURSOR
		OR A
		JNZ VDPPUTC_BSDEC				;It is not zero. We can decrement.
		LDA CURSOR+1
		OR A
		JNZ VDPPUTC_BSDEC				;It is not zero. We can decrement.
		RET								;It is zero. Just return
VDPPUTC_BSDEC		
		CALL VDPCLCURSOR
		LHLD CURSOR
		DCX H
		SHLD CURSOR
		JMP VDPPUTC_RET
VDPPUTC_CHLF
		CPI 0AH
		JNZ VDPPUTC_CHCR
		RET								;Ignore it (TEMP SOLUTION)
		;LDA CURSOR						;Load CURSOR to A
		;ADI 28H						;Add 40 (0x28) to A
		;STA CURSOR						;Save result in CURSOR
		;LDA CURSOR+1					;Load CURSOR+1 to A
		;ACI 00H						;Add 0 to A with carry
		;STA CURSOR+1					;Save result in CURSOR+1
		;JMP VDPPUTC_CHECKCURSOR
VDPPUTC_CHCR:
		CPI 0DH
		JNZ VDPPUTC_SEND
		CALL VDPCLCURSOR				;Clear cursor first
		CALL NEXTLINE
		CALL VDP_CHECKCURSOR
		RET
VDPPUTC_SEND:
		PUSH PSW
		CALL VDP_CHECKCURSOR
		POP PSW
		MOV B, A						;Store A in B
		;Add CURSOR to the address of NAME TABLE in VRAM
		LDA CURSOR						;Load CURSOR (LSB) tp A
		OUT VDP_MODE					;Send result to VDP
		NOP
		NOP
		LDA CURSOR+1					;Load CURSOR+1 (MSB) to A
		ADI 08H							;Add 0x08 to A
		ORI 40H
		OUT VDP_MODE					;Address is set
		NOP
		NOP
		MOV A, B						;Restore character from B
		OUT VDP_DATA					;Now simpluy send the character
		;So... Now we need to increment CURSORLIST
		LHLD CURSOR
		INX H
		SHLD CURSOR	
VDPPUTC_RET:
		;Before we return, we need to put cursor in its new place
		LDA CURSOR						;Load CURSOR (LSB) tp A
		OUT VDP_MODE					;Send result to VDP
		NOP
		NOP
		LDA CURSOR+1					;Load CURSOR+1 (MSB) to A
		ADI 08H							;Add 0x08 to A
		ORI 40H
		OUT VDP_MODE					;Address is set
		NOP
		NOP
		MVI A, 05FH						;Cursor is '_' character
		OUT VDP_DATA					;Now simpluy send the character		
		RET
		
VDP_CHECKCURSOR:
		;NOW WE NEED TO CHECK IF VDP_CURSOR IS HIGHTER THAN 960 (0x3C0)
		LDA CURSOR+1
		CPI 03H
		RC								;CURSOR+1 < 0x03 - it is in range. We can return
		JZ VDP_CHECKCURSORLSB			;CURSOR+1 = 0x03 - we need to check LSB to be sure
		CALL VDPSCROLLUP				;Otherwise CURSOR+1 > 0x03
VDP_CHECKCURSORLSB:		
		;CURSOR is equal to 0x03. We need to test lower byte! CHECK THIS!!!!!!!!!!!!!!!!
		LDA CURSOR
		CPI 0C0H
		RC								;CURSOR < 0xC0 - it is in range. We can return. Otherwise exceeded.	
VDP_EXCEEDED:
		CALL VDPSCROLLUP
		RET
		
		
VDPCLCURSOR:
		LDA CURSOR						;Load CURSOR (LSB) tp A
		OUT VDP_MODE					;Send result to VDP
		NOP
		NOP
		LDA CURSOR+1					;Load CURSOR+1 (MSB) to A
		ADI 08H							;Add 0x08 to A
		ORI 40H
		OUT VDP_MODE					;Address is set
		NOP
		NOP
		MVI A, 20H						;Cursor is ' ' character
		OUT VDP_DATA					;Now simpluy send the character
		RET
		
		
NEXTLINE:
		; Check if we are in the last line
		LDA CURSOR+1
		CPI 03H
		JC NLCALCULATE					;MSB < 0x03. We are definately in range. Just calculate new cursor position.
		LDA CURSOR
		CPI 97H
		JC NLCALCULATE					;LSB < 97. We are definately in range. Just calculate new cursor position.
		JZ NLCALCULATE					;Also if it is equal. TODO - check if this is valid
		; Otherwise we are in the las line. Scrollup!
		CALL VDPSCROLLUP
		RET
NLCALCULATE:
		LDA CURSOR+1					;Load CURSOR MSB
		MOV B, A						;Move it to B
		LDA CURSOR						;Load CURSOR LSB, it stays in A
		MVI C, 28H						;Denominator (40) in C
		INR B							;Increase B register
		LXI H,0000H						;Store 0000Hinto HL pair
NLDIV40L:
		SUB C   						;Subtract C from acc
		JC NLDIV40SKIP    				;Jump to SKIPwhen CY = 1
NLDIV40INCR:
		INX H   						;Increase quotient part
		JMP NLDIV40L   					;Jump to LOOP
NLDIV40SKIP:
		DCR B   						;Decrease B		
		JZ NLDIV40STORE    				;Jump to STOREwhen Z = 1
		JMP NLDIV40INCR    				;Jump to INCR
NLDIV40STORE:
		MOV A, L						;Store the lower order quotient. It is all we need - it is < 24
		INR A							;Increment - next line. It is never zero.
		MOV B, A						;Store it in B
		SUB A							;Zero A
		STA CURSOR						;Write zero to CURSOR
		STA CURSOR+1					;Write zero to CURSOR+1
NLMUL40L:
		LDA CURSOR
		ADI 28H
		STA CURSOR
		LDA CURSOR+1
		ACI 00H
		STA CURSOR+1
		DCR B
		JNZ NLMUL40L
		RET
