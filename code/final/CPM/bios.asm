		include "labels.asm"
;STACK 	EQU 7FFFH
CCP		EQU	9C00H   ; was 2200H
BDOS	EQU CCP+806H
BIOS	EQU CCP+1600H
CDISK	EQU	0004H
IOBYTE	EQU 0003H

; WARNING: The following assumes that CPM_BASE%128 = 0
WB_NSECTS	EQU		(BIOS_BOOT-CCP)/128					; number of sectors to load
WB_TRK		EQU		0									; first track now at beginning of partition now
WB_SEC		EQU		(CCP/128)&03H						; first sector number

DEBUG	EQU 0

;**************************************************************
;*
;*        B I O S   J U M P   T A B L E
;*
;**************************************************************
		
		org 0B200H   ; was 3800H
	IF $ != CCP+1600H
		error "BIOS begins at wrong address!"
	ENDIF	
BIOS_BOOT:   JMP      BIOS_BOOT_PROC
BIOS_WBOOT:  JMP      BIOS_WBOOT_PROC
BIOS_CONST:  JMP      BIOS_CONST_PROC
BIOS_CONIN:  JMP      BIOS_CONIN_PROC
BIOS_CONOUT: JMP      BIOS_CONOUT_PROC
BIOS_LIST:   JMP      BIOS_LIST_PROC
BIOS_PUNCH:  JMP      BIOS_PUNCH_PROC
BIOS_READER: JMP      BIOS_READER_PROC
BIOS_HOME:   JMP      BIOS_HOME_PROC
BIOS_SELDSK: JMP      BIOS_SELDSK_PROC
BIOS_SETTRK: JMP      BIOS_SETTRK_PROC
BIOS_SETSEC: JMP      BIOS_SETSEC_PROC
BIOS_SETDMA: JMP      BIOS_SETDMA_PROC
BIOS_READ:   JMP      BIOS_READ_PROC
BIOS_WRITE:  JMP      BIOS_WRITE_PROC
BIOS_PRSTAT: JMP      BIOS_PRSTAT_PROC
BIOS_SECTRN: JMP      BIOS_SECTRN_PROC	

BIOS_BOOT_PROC:
		DI
		LXI  H, BIOS_STACK
		SPHL
        
        XRA A
        LXI H, 0000H
        LXI B, 9C00H
ZERO_LOOP:
        MVI A, 00H
        MOV M, A
        INX H
        DCX B
        MOV A, B
        ORA C
        JNZ ZERO_LOOP
	IF DEBUG > 0
	    PUSH PSW
        PUSH B
		PUSH D
		PUSH H
		CALL IPUTS
		DB 'Cold boot procedure entered'
		DB CR
		DB 00H
		POP H
		POP D
		POP B
		POP PSW
	ENDIF
		CALL IPUTS
		DB 'Loading CP/M 2.2'
		DB CR
		DB 00H
		CALL IPUTS
		DB 'CRC: '
		DB 00H
		LXI D, CCP
		LXI B, BIOS_STACK_END-CCP
		LXI H, 0000H
CPM_CRC_LOOP:
		LDAX D
		PUSH B
		CALL CRC16_ARC_F
		POP B
		INX D
		DCX B
		MOV A, B
		ORA C
		JNZ CPM_CRC_LOOP
		CALL PRN_ZERO_EX
		MOV A, H
		CALL HEXDUMP_A
		MOV A, L
		CALL HEXDUMP_A
		CALL NEWLINE
		XRA A
		STA IOBYTE
		STA CDISK
;BIOS_TRYKBINIT:								;Reinitialize keyboard
;        CALL BIOS_KBDINIT                        ;Call init routine
;        MOV A, B							;Move result of operation to A
;        CPI 00H								;Check if OK
;        JNZ BIOS_TRYKBINIT						;Retry if not ok. TODO add limit of retries
		JMP GOCPM
	
BIOS_WBOOT_PROC:
		DI
		; We can't just blindly set SP=bios_stack here because disk_read can overwrite it!
		; But we CAN set to use other areas that we KNOW are not currently in use!
		LXI  H, BIOS_WBOOT_STACK		; 
		SPHL
	IF DEBUG > 0
	    PUSH PSW
        PUSH B
		PUSH D
		PUSH H
		CALL IPUTS
		DB 'WBOOT procedure entered'
		DB CR
		DB 00H
		POP H
		POP D
		POP B
		POP PSW
	ENDIF
	IF 0
		CALL IPUTS
		DB 'Wboot not implemented!'
		DB CR
        DB 00H
        JMP $
    ENDIF
		MVI C, 0
		CALL BIOS_SELDSK
		LXI B, WB_TRK
		CALL BIOS_SETTRK
		LXI B, WB_SEC
		CALL BIOS_SETSEC
		LXI B, CCP
		CALL BIOS_SETDMA
		LXI B, WB_NSECTS
BIOS_WBOOT_LOOP:
		PUSH B	; Save remaining sector count
		CALL BIOS_READ
		ORA A
		JZ BIOS_WBOOT_SEC_OK
		CALL IPUTS
		DB 'ERROR: WBOOT FAILED. HALTING.'
		DB CR
		DB '*** PRESS RESET TO REBOOT ***'
		DB CR
		DB 00H
		JMP $
BIOS_WBOOT_SEC_OK:
		; Advance the DMA pointer by 128 bytes
		LHLD DISK_DMA	; HL = last used DMA address
		MOV A, L
		ADI 80H
		MOV L, A
		MOV A, H
		ACI 00H
		MOV H, A
		MOV B, H
		MOV C, L	; BC = HL
		CALL BIOS_SETDMA
		LDA DISK_SECTOR		; A = last used sector number (low byte only for 0..3)
		INR A
		ANI 03H				; if A+1 = 4 then A=0
		JNZ BIOS_WBOOT_SEC	; if A+1 !=4 then do not advance the track number
		; Advance to the next track
		PUSH H
		LHLD DISK_TRACK
		MOV B, H
		MOV C, L
		POP H
		INX B
		CALL BIOS_SETTRK
		MVI A, 00H 			; sett A=0 for first sector on new track
BIOS_WBOOT_SEC:
		MVI B, 00H
		MOV C, A
		CALL BIOS_SETSEC
		POP B				; BC = remaining sector counter value
		DCX B				; BC--
		MOV A, B
		ORA C
		JNZ BIOS_WBOOT_LOOP
		; Fall through into GOCPM
GOCPM:
		MVI A, 0C3H ;C3 is a jump instruction
		STA	0		;for JMP to wboot
		LXI H, BIOS_WBOOT	;WBOOT entry point
		SHLD 1		;Set address field for jmp at 0
		
		STA 5		;For JMP to bdos
		LXI H, BDOS	;BDOS entry point
		SHLD 6		;Address field of jump at 5 to bd
		
		; This is here because it is in example CBIOS (AG p. 52)
		LXI H, 0080H	;default DMA address is 80H (TODO! Check!)
		SHLD DISK_DMA
		
		; EI			;Enable interrupts
		LDA CDISK	;Load current disk number
		MOV C, A	;Send to the CCP
		JMP CCP		;Go to the CP/M for further processing
	
BIOS_CONST_PROC:
        IN   UART_8251_CTRL
        NOP                             ;STATUS BIT FLIPPED?
        ANI  RxRDY_MASK                 ;MASK STATUS BIT
		RET
	
BIOS_CONIN_PROC:
        IN   UART_8251_CTRL
        NOP
        ANI  RxRDY_MASK
        JZ BIOS_CONIN_PROC
        IN   UART_8251_DATA
		RET
	
BIOS_CONOUT_PROC:
		PUSH H				; Save content  of HL on original stack, then switch to bios stack
		LXI H, 0000H
		DAD SP	; HL = HL + SP
		SHLD ORIGINAL_SP
		LXI H, BIOS_STACK
		SPHL
		PUSH PSW
		MOV A, C			; Save A on BIOS stack
		CALL OUT_CHAR
		POP PSW			; Restore A from new stack
		LHLD ORIGINAL_SP; Restore original stack
		SPHL
		POP H			; Restore original content of HL	
		RET
		
BIOS_LIST_PROC:
		RET
		  
BIOS_PUNCH_PROC:
		RET
		 
BIOS_READER_PROC:
		MVI A, 1AH
		RET
		
BIOS_HOME_PROC:
	IF DEBUG > 0
		PUSH H				; Save content  of HL on original stack, then switch to bios stack
		LXI H, 0000H
		DAD SP	; HL = HL + SP
		SHLD ORIGINAL_SP
		LXI H, BIOS_STACK
		SPHL
        PUSH PSW
        PUSH B
		PUSH D
		PUSH H
		CALL IPUTS
		DB 'HOME procedure entered'
		DB CR
		DB 00H
		POP H
		POP D
		POP B
		POP PSW
		LHLD ORIGINAL_SP; Restore original stack
		SPHL
		POP H			; Restore original content of HL
	ENDIF
		LXI B, 0000H
		;FALL INTO BIOS_SETTRK_PROC!!!
		
BIOS_SETTRK_PROC:
		PUSH H
		MOV L, C
		MOV H, B
		SHLD DISK_TRACK
		POP H
	IF DEBUG > 1
		PUSH H				; Save content  of HL on original stack, then switch to bios stack
		LXI H, 0000H
		DAD SP	; HL = HL + SP
		SHLD ORIGINAL_SP
		LXI H, BIOS_STACK
		SPHL	
        PUSH PSW
        PUSH B
		PUSH D
		PUSH H
		CALL IPUTS
		DB 'SETTRK procedure entered: '
		DB 00H
		CALL PRINT_DISK_DEBUG
		POP H
		POP D
		POP B
		POP PSW
		LHLD ORIGINAL_SP; Restore original stack
		SPHL
		POP H			; Restore original content of HL
	ENDIF		
		RET
		  
BIOS_SELDSK_PROC:
		PUSH PSW
		MOV A, C
		STA DISK_DISK
		POP PSW
		;LXI H, 0
		LXI H, DISKA_DPH
	IF DEBUG > 1
		PUSH H				; Save content  of HL on original stack, then switch to bios stack
		LXI H, 0000H
		DAD SP	; HL = HL + SP
		SHLD ORIGINAL_SP
		LXI H, BIOS_STACK
		SPHL	
        PUSH PSW
        PUSH B
		PUSH D
		PUSH H
		CALL IPUTS
		DB 'SELDSK procedure entered: '
		DB 00H
		CALL PRINT_DISK_DEBUG
		POP H
		POP D
		POP B
		POP PSW
		LHLD ORIGINAL_SP; Restore original stack
		SPHL
		POP H			; Restore original content of HL
	ENDIF		
		RET
		
BIOS_SETSEC_PROC:
		PUSH H
		MOV L, C
		MOV H, B
		SHLD DISK_SECTOR
		POP H	
	IF DEBUG > 1
		PUSH H				; Save content  of HL on original stack, then switch to bios stack
		LXI H, 0000H
		DAD SP	; HL = HL + SP
		SHLD ORIGINAL_SP
		LXI H, BIOS_STACK
		SPHL
        PUSH PSW
        PUSH B
		PUSH D
		PUSH H
		CALL IPUTS
		DB 'SETSEC procedure entered: '
		DB 00H
		CALL PRINT_DISK_DEBUG
		POP H
		POP D
		POP B
		POP PSW
		LHLD ORIGINAL_SP; Restore original stack
		SPHL
		POP H			; Restore original content of HL
	ENDIF		
		RET
		
BIOS_SETDMA_PROC:
		PUSH H
		MOV L, C
		MOV H, B
		SHLD DISK_DMA
		POP H
	IF DEBUG > 1		
		PUSH H				; Save content  of HL on original stack
		LXI H, 0000H		; then switch to bios stack
		DAD SP	; HL = HL + SP
		SHLD ORIGINAL_SP
		LXI H, BIOS_STACK
		SPHL				; Bios stack set. 		
        PUSH PSW
        PUSH B
		PUSH D
		PUSH H
		CALL IPUTS
		DB 'SETDMA procedure entered: '
		DB 00H
		CALL PRINT_DISK_DEBUG
		POP H
		POP D
		POP B
		POP PSW
		LHLD ORIGINAL_SP; Restore original stack
		SPHL
		POP H			; Restore original content of HL
	ENDIF
		RET
		
BIOS_READ_PROC:
		PUSH H				; Save content  of HL on original stack, then switch to bios stack
		LXI H, 0000H
		DAD SP	; HL = HL + SP
		SHLD ORIGINAL_SP
		LXI H, BIOS_STACK
		SPHL				; Bios stack set. 	
	IF DEBUG > 0
        PUSH PSW
        PUSH B
		PUSH D
		PUSH H
		CALL IPUTS
		DB 'READ procedure entered: '
		DB 00H
		CALL PRINT_DISK_DEBUG
		POP H
		POP D
		POP B
		POP PSW
	ENDIF
		PUSH B				; Now save remaining registers
		PUSH D
		LDA DISK_TRACK
		ADI 00H				;This is temp, TODO: remove hardwired addres of partition
		STA CFLBA0
		LDA DISK_TRACK+1
		ACI 08H
		STA CFLBA1
		MVI A, 0
		STA CFLBA2
		STA CFLBA3
		LXI D, BLKDAT
		CALL CFRSECT
	IF DEBUG > 1
        PUSH PSW
        PUSH B
		PUSH D
		PUSH H
		CALL PRINT_CFLBA_DEBUG
		POP H
		POP D
		POP B
		POP PSW
	ENDIF
		; If no error there should be 0 in A
		CPI 00H
		JZ BIOS_READ_PROC_GET_SECT		; No error, just read sector
		POP D							; Otherwise report error and return - first restore registers
		POP B
		LHLD ORIGINAL_SP				; Restore original stack
		SPHL
		POP H							; Restore original content of HL
		MVI A, 1						; Report error					
		RET								; Return
BIOS_READ_PROC_GET_SECT
		LDA DISK_SECTOR
		
		MOV E, A
		MVI D, 0
		
		ADD E
		MOV E, A
		MOV A, D
		ADC D
		MOV D, A
		
		MOV A, E
		ADD E
		MOV E, A
		MOV A, D
		ADC D
		MOV D, A
		
		MOV A, E
		ADD E
		MOV E, A
		MOV A, D
		ADC D
		MOV D, A
		
		MOV A, E
		ADD E
		MOV E, A
		MOV A, D
		ADC D
		MOV D, A
		
		MOV A, E
		ADD E
		MOV E, A
		MOV A, D
		ADC D
		MOV D, A
		
		MOV A, E
		ADD E
		MOV E, A
		MOV A, D
		ADC D
		MOV D, A
		
		MOV A, E
		ADD E
		MOV E, A
		MOV A, D
		ADC D
		MOV D, A

		; Now DE contains the 16-bit result of multiplying the original value by 128
		; D holds the high byte and E holds the low byte of the result
		; Calculate the address of the CP/M sector in the BLKDAT
		LXI H, BLKDAT
		MOV A, E
		ADD L
		MOV E, A
		MOV A, D
		ADC H
		MOV D, A
	IF DEBUG > 1
		PUSH D	; Store DE (source address) in stack
		CALL IPUTS
		DB 'Calculated source address in CF bufffer = 0x'
		DB 00H
		POP D	; Retrieve DE (source address)
		MOV A, D
		CALL HEXDUMP_A
		MOV A, E
		CALL HEXDUMP_A
		MVI A, CR
		CALL OUT_CHAR
	IF 0
		PUSH D
		CALL IPUTS
		DB 'Buffer before copy: '
		DB 00H
		POP D
		PUSH D
		MVI B, 80H
		CALL HEXDUMP
		MVI A, CR
		CALL OUT_CHAR
		POP D
	ENDIF
	ENDIF
		; Source addres in DE
		LHLD DISK_DMA	; Load target address to HL
		LXI B, 0080H	; How many bytes?
		CALL MEMCOPY
	IF 0 ;DEBUG > 0
		PUSH D
		CALL IPUTS
		DB 'Buffer after copy: '
		DB 00H		
		POP D
		LHLD DISK_DMA
		MOV D, H
		MOV E, L
		MVI B, 80H
		CALL HEXDUMP
		MVI A, CR
		CALL OUT_CHAR
	ENDIF
		POP D
		POP B
		LHLD ORIGINAL_SP; Restore original stack
		SPHL
		POP H			; Restore original content of HL
		MVI A, 0
		RET
		  
BIOS_WRITE_PROC:
		PUSH H				; Save content  of HL on original stack, then switch to bios stack
		LXI H, 0000H
		DAD SP	; HL = HL + SP
		SHLD ORIGINAL_SP
		LXI H, BIOS_STACK
		SPHL				; Bios stack set. 
		
	IF DEBUG > 0
        PUSH PSW
        PUSH B
		PUSH D
		PUSH H
		CALL IPUTS
		DB 'WRITE procedure entered'
		DB CR
		DB 00H
		POP H
		POP D
		POP B
		POP PSW
	ENDIF
	
		LHLD ORIGINAL_SP; Restore original stack
		SPHL
		POP H			; Restore original content of HL
		MVI A, 1	; <--- Stub error in every write
		RET
		 
BIOS_PRSTAT_PROC:
		MVI A, 0 ;Printer is never ready
		RET
		
BIOS_SECTRN_PROC:
		; 1:1 translation (no skew factor)
		MOV H, B
		MOV L, C
	IF DEBUG > 1
		PUSH H				; Save content  of HL on original stack, then switch to bios stack
		LXI H, 0000H
		DAD SP	; HL = HL + SP
		SHLD ORIGINAL_SP
		LXI H, BIOS_STACK
		SPHL				; Bios stack set. 
        PUSH PSW
        PUSH B
		PUSH D
		PUSH H
		CALL IPUTS
		DB 'SECTRN procedure entered: '
		DB 00H
		CALL PRINT_DISK_DEBUG
		POP H
		POP D
		POP B
		POP PSW
		LHLD ORIGINAL_SP; Restore original stack
		SPHL
		POP H			; Restore original content of HL
	ENDIF		
		RET
		
;KBDINIT - initializes 8042/8242 PS/2 keyboard controller
;Affects: A, B, C, Z
;Returns result code in  B:
;0x00 - OK
;0x01 - Controller self test failed
;0x02 - CLK stuck low
;0x03 - CLK stuck high
;0x04 - KBD DATA stuck low
;0x05 - KBD DATA stuck high
;0x06 - Interface didn't pass the test
;0x07 - Keyboard reset failure/no keyboard present        
BIOS_KBDINIT:
        ;1. Disable devices
        CALL KBDWAITINBUF           ;Send 0xAD command to the PS/2 controller
        MVI A, 0ADH
        OUT KBD_CMD
        ;2. Flush The Output Buffer
        IN KBD_STATUS
        ANI 01H                     ;Check if there is data to flush
        JZ BIOS_KBDCRTLSET              	;No? Next step then
        IN KBD_DATA                 ;Yes? Get the data byte        
BIOS_KBDCRTLSET:
        ;3. Set the Controller Configuration Byte (temp)
        CALL KBDWAITINBUF			;Send 0x60 command to the PS/2 controller
        MVI A, 60H
        OUT KBD_CMD
        CALL KBDWAITINBUF			;Send actual configuration byte
        MVI A, 08H					;Interrupts disabled, system flag set, first port clock enabled
		OUT KBD_DATA				;second port clock disabled, first port translation disabled
        ;4. Controller self test
		CALL KBDWAITINBUF
        MVI A, 0AAH                 ;Send 0xAA command to the PS/2 controller
        OUT KBD_CMD
		CALL KBDWAITOUTBUF          ;Wait for response
        IN KBD_DATA                 ;Get byte
        CPI 55H                     ;Is it 0x55?
        MVI B, 01H					;Return result code if not
        RNZ                          ;No? Return then
        ;5. Interface test
		CALL KBDWAITINBUF
        MVI A, 0ABH                 ;Send 0xAB command
        OUT KBD_CMD
		CALL KBDWAITOUTBUF          ;Wait for response
        IN KBD_DATA                 ;Get byte
        CPI 01H                     ;Check if it is CLK stuck low error
        MVI B, 02H                  ;Return result code if it is
        RZ                         
        CPI 02H                     ;Check if it is CLK stuck high error
        MVI B, 03H                  ;Return result code if it is
        RZ                         
        CPI 03H                     ;Check if it is KBD DATA stuck low error
        MVI B, 04H                  ;Return result code if it is
        RZ
        CPI 04H                     ;Check if it is KBD DATA stuck high error
        MVI B, 05H                  ;Return result code if it is
        RZ                         
        CPI 00H                     ;Is it 0x00? Did it pass the test?
        MVI B, 06H					;Return result code if not
        RNZ                          ;No? Return then
        ;6. Enable Devices
        CALL KBDWAITINBUF
        MVI A, 0AEH                 ;Send 0xAE command
        OUT KBD_CMD
        ;7. Reset Device
        CALL KBDWAITINBUF           ;Wait untill ready to send
        MVI A, 0FFH                 ;Send 0xFF to device
        OUT KBD_DATA                ;Send it to device, not the controller
        MVI C, 130                  ;Setup DELAY routine
        CALL DELAY                  ;This is required to avoid freeze
        CALL KBDWAITOUTBUF          ;Wait for response
        IN KBD_DATA                 ;Get byte
        CPI 0FAH                    ;Is it 0xFA? 0xFC means failure. No response means no device present.
        MVI B, 07H					;Return result code if not
        RNZ                          ;No? Return then
        ;8. Set the Controller Configuration Byte (final)
        CALL KBDWAITINBUF			;Send 0x60 command to the PS/2 controller
        MVI A, 60H
        OUT KBD_CMD
        CALL KBDWAITINBUF			;Send actual configuration byte
        MVI A, 08H					;Interrupts disabled, system flag set, first port clock enabled
		OUT KBD_DATA				;second port clock disabled, first port translation disabled
        ;9. Zero out buffer        
        MVI A, 00H                  
        STA KBDDATA					;Zero KBDDATA
        STA KBDKRFL					;Zero key release flag
        STA KBDSFFL					;Zero shift flag
        STA KBDOLD					;Zero old data
        STA KBDNEW					;Zero new data
        MVI B, 00H					;Return result code
        RET
		
		
	IF DEBUG > 0
PRINT_DISK_DEBUG
		CALL IPUTS
		DB 'disk=0x'
		DB 00H
		LDA DISK_DISK
		CALL HEXDUMP_A
		CALL PRINT_COLON
		CALL IPUTS
		DB 'track=0x'
		DB 00H
		LDA DISK_TRACK+1
		CALL HEXDUMP_A
		LDA DISK_TRACK
		CALL HEXDUMP_A
		CALL PRINT_COLON
		CALL IPUTS
		DB 'sector=0x'
		DB 00H
		LDA DISK_SECTOR+1
		CALL HEXDUMP_A
		LDA DISK_SECTOR
		CALL HEXDUMP_A
		CALL PRINT_COLON
		CALL IPUTS
		DB 'dma=0x'
		DB 00H
		LDA DISK_DMA+1
		CALL HEXDUMP_A
		LDA DISK_DMA
		CALL HEXDUMP_A
		CALL PRINT_COLON
		CALL PRINT_STACK_POINTER
		CALL IPUTS
		DB CR
		DB 00H
		RET
		
PRINT_STACK_POINTER
		CALL IPUTS
		DB 'SP=0x'
		DB 00H
		LXI H, 0000H
		DAD SP
		MOV A, H
		CALL HEXDUMP_A
		MOV A, L
		CALL HEXDUMP_A
		RET
		
PRINT_CFLBA_DEBUG
		CALL IPUTS
		DB 'LBA=0x'
		DB 00H
		LDA CFLBA3
		CALL HEXDUMP_A
		LDA CFLBA2
		CALL HEXDUMP_A
		LDA CFLBA1
		CALL HEXDUMP_A
		LDA CFLBA0
		CALL HEXDUMP_A
		CALL IPUTS
		DB CR
		DB 00H
		RET
		
PRINT_COLON:
		CALL IPUTS
		DB ', '
		DB 00H
		RET
	ENDIF
	
LAST_CHAR		DB	00H		; Last ASCII character from keyboard	
DISK_DISK:		DB	00H		; Should it be here?
DISK_TRACK:		DW	0000H	; Should it be here?
DISK_SECTOR: 	DW 	0000H	; Should it be here?
DISK_DMA:		DW 	0000H	; Should it be here?
ORIGINAL_SP:	DW	0000H

; Plan:
; - Put 4 128-byte CP/M sectors into each 512-byte CF card block
; - Treat each CF card block as a CP/M track
; 
; This filesystem has:
;	128 bytes/sector (CP/M requirement)
;	4 sectors/track (BIOS designer's choice)
;	65536 total sectors (CP/M limit)
; 	65536*128 = 8388608 gross bytes (max CP/M limit)
;	65536/4 = 16384 tracks
;	2048 allocation block size BLS (BIOS designer's choice)
;	8388608/2048 = 4096 totalal allocation blocks
;	512 directory entries (BIOS designer's choice)
;	512*32 = 16384 total bytes in the directory
;	ceiling(16384/2048) = 8 allocation blocks for the directory

;	BLS		BSH		BLM		------EXM------
;							DSM<256		DSM>255
;	1024	3		7		0			x
;	2048	4		15		1			0		<--------- This is what we are using
;	4096	5		31		3			1
;	8192	6		63		7			3
;	16384	7		127		15			7
;
; ** NOTE: 	This filesystem design is inefficient because it is unlikely
;			that ALL of the allocation blocks will ultimately get used! 

DISKA_DPH:
				DW	0000H		; XLT
				DW	0000H		; SCRPAD
				DW	0000H
				DW	0000H
				DW	DIRBUF		; DIRBUF
				DW	DISKA_DPB	; DPB
				DW	0000H		; CSV
				DW	DISKA_ALV	; ALV

DISKA_DPB:
				DW	4			; SPT (four 128 bytes per 512 byte track)
				DB	4			; BSH (for BLS 2048)
				DB	15			; BLM (for BLS 2048)
				DB	00H			; EXM
				DW	4063		; DSM (max allocation number)
				DW	511			; DRM
				DB	0FFH		; AL0
				DB	00H			; AL1
				DW	0000H		; CKS
				DW	0011H		; OFF
				
DISKA_ALV:
				DS	(4065/8)+1
DISKA_ALV_END

DIRBUF
				DS 	128
BIOS_WBOOT_STACK

BIOS_STACK_END
				DS 128
BIOS_STACK

	IF $ < BIOS_BOOT
		error "BIOS rolled over memory!"
	ENDIF
	
	IF $ >= SYSTEM_VARIABLES
		error "Bios overwritten system variables!"
	ENDIF
		END
		
		
;        ORG  7FFFH
;STACK:  DS   0                          ;STACK STARTS HERE
