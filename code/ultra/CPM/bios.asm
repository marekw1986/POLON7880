        include "variables.asm"

CCP		EQU	0DC00H   ; was 2200H
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
		
		org 0F200H   ; was 3800H
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
        
        ; Turn on ROM shadowing
		MVI  A, 88H
        OUT  PORT_74273
        NOP
        NOP
        
		LXI  H, BIOS_STACK
		SPHL
        
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
        
        XRA A
        LXI H, 0000H
        LXI B, CCP
ZERO_LOOP:
        XOR A
        MOV M, A
        INX H
        DCX B
        MOV A, B
        ORA C
        JNZ ZERO_LOOP
        CALL CFGETMBR
        OR A                     ; Check if MBR loaded properly
        JZ LD_PART_TABLE
        CALL IPUTS
        DB 'MBR load err. Reset required.'
        DB 00H
        CALL ENDLESS_LOOP
LD_PART_TABLE:
        CALL CFLDPARTADDR
CFVAR_INIT:
		XOR A
		STA	CFLBA3
		STA	CFLBA2
		STA	CFLBA1
		STA	CFLBA0
		STA PCFLBA3
		STA PCFLBA2
		STA PCFLBA1
		STA PCFLBA0
		STA CFVAL
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
		DB 'Running CP/M 2.2'
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
		XOR A 			; sett A=0 for first sector on new track
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
        IN   SCC2681_SRA
        NOP                             ;STATUS BIT FLIPPED?
        ANI  RxRDY_MASK                 ;MASK STATUS BIT
		RET
	
BIOS_CONIN_PROC:
        IN   SCC2681_SRA
        ANI  RxRDY_MASK              ; Wait until RxRDY (bit 0) is set
        JZ   BIOS_CONIN_PROC
        IN   SCC2681_RHRA           ; Read from Rx holding register
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
		MOV A, C
        CPI 04H     ; Only four partitions supported
        JNC BIOS_SELDSK_PROC_WRNDSK
        STA DISK_DISK
		LXI H, DISKA_DPH	
		RET
BIOS_SELDSK_PROC_WRNDSK:
        LXI H, 0
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
        CALL CALC_CFLBA_FROM_PART_ADR
        OR A                            ; If 0 in A, no valid LBA calculated
        JZ BIOS_READ_PROC_RET_ERR          ; In that case return and report error
		CALL CFRSECT_WITH_CACHE
	IF DEBUG > 0
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
		OR A
		JZ BIOS_READ_PROC_GET_SECT		; No error, just read sector. Otherwise report error and return.
        JMP BIOS_READ_PROC_RET_ERR		; Return
BIOS_READ_PROC_GET_SECT:
        CALL BIOS_CALC_SECT_IN_BUFFER
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
BIOS_READ_PROC_RET_ERR
        MVI A, 1
        JMP BIOS_READ_PROC_RET
BIOS_READ_PROC_RET_OK    
        MVI A, 0
BIOS_READ_PROC_RET
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
		PUSH B				; Now save remaining registers
		PUSH D
        ; Check content of C - deblocking code
        MOV A, C
        CPI 2               ; Is it first sector of new track?
        JZ BIOS_WRITE_NEW_TRACK
		; First read sector to have complete data in buffer
		CALL CFRSECT_WITH_CACHE
		OR A
		JNZ BIOS_WRITE_RET_ERR			; If we ae unable to read sector, it ends here. We would risk FS crash otherwise.
		CALL BIOS_CALC_SECT_IN_BUFFER
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
        JMP BIOS_WRITE_PERFORM
        ; No need to calculate sector location in BLKDAT.
        ; Thanks to deblocking code = 2 we know it is first secor of new track
        ; Just fill remaining bytes of buffer with 0xE5 and copy secotr to the
        ; beginning of BLKDAT. Then write.
BIOS_WRITE_NEW_TRACK
        LXI H, BLKDAT+128
        LXI B, 384
BIOS_WRITE_E5_FILL_LOOP:
        MVI A, 0E5H
        MOV M, A
        INX H
        DCX B
        MOV A, B
        ORA C
        JNZ BIOS_WRITE_E5_FILL_LOOP
        LXI D, BLKDAT
BIOS_WRITE_PERFORM:
		; Addres of sector in BLKDAT is now in DE
		LHLD DISK_DMA	; Load source address to HL
		; Replace HL and DE. HL will now contain address od sector in BLKDAT and DE will store source from DISK_DMA
		XCHG
		LXI B, 0080H	; How many bytes to copy?
		CALL MEMCOPY
		; Buffer is updated with new sector data. Perform write.
        CALL CALC_CFLBA_FROM_PART_ADR
        OR A         ; If A=0, no valid LBA calculated
        JZ BIOS_WRITE_RET_ERR ; Return and report error
		LXI D, BLKDAT
		CALL CFWSECT
		OR A			; Check result
		JNZ BIOS_WRITE_RET_ERR
		JMP BIOS_WRITE_RET_OK				
BIOS_WRITE_RET_ERR:
        XOR A
        STA CFVAL
		MVI A, 1
		JMP BIOS_WRITE_RET
BIOS_WRITE_RET_OK:
        MVI A, 01H
        STA CFVAL
        CALL CFUPDPLBA
		MVI A, 0
BIOS_WRITE_RET:
		POP D
		POP B	
		LHLD ORIGINAL_SP; Restore original stack
		SPHL
		POP H			; Restore original content of HL
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
;BIOS_KBDINIT:
;        ;1. Disable devices
;        CALL KBDWAITINBUF           ;Send 0xAD command to the PS/2 controller
;        MVI A, 0ADH
;        OUT KBD_CMD
;        ;2. Flush The Output Buffer
;        IN KBD_STATUS
;        ANI 01H                     ;Check if there is data to flush
;        JZ BIOS_KBDCRTLSET              	;No? Next step then
;        IN KBD_DATA                 ;Yes? Get the data byte        
;BIOS_KBDCRTLSET:
;        ;3. Set the Controller Configuration Byte (temp)
;        CALL KBDWAITINBUF			;Send 0x60 command to the PS/2 controller
;        MVI A, 60H
;        OUT KBD_CMD
;        CALL KBDWAITINBUF			;Send actual configuration byte
;        MVI A, 08H					;Interrupts disabled, system flag set, first port clock enabled
;		OUT KBD_DATA				;second port clock disabled, first port translation disabled
;        ;4. Controller self test
;		CALL KBDWAITINBUF
;        MVI A, 0AAH                 ;Send 0xAA command to the PS/2 controller
;        OUT KBD_CMD
;		CALL KBDWAITOUTBUF          ;Wait for response
;        IN KBD_DATA                 ;Get byte
;        CPI 55H                     ;Is it 0x55?
;        MVI B, 01H					;Return result code if not
;        RNZ                          ;No? Return then
;        ;5. Interface test
;		CALL KBDWAITINBUF
;        MVI A, 0ABH                 ;Send 0xAB command
;       OUT KBD_CMD
;		CALL KBDWAITOUTBUF          ;Wait for response
;        IN KBD_DATA                 ;Get byte
;        CPI 01H                     ;Check if it is CLK stuck low error
;        MVI B, 02H                  ;Return result code if it is
;        RZ                         
;        CPI 02H                     ;Check if it is CLK stuck high error
;        MVI B, 03H                  ;Return result code if it is
;        RZ                         
;        CPI 03H                     ;Check if it is KBD DATA stuck low error
;        MVI B, 04H                  ;Return result code if it is
;        RZ
;        CPI 04H                     ;Check if it is KBD DATA stuck high error
;        MVI B, 05H                  ;Return result code if it is
;        RZ                         
;        CPI 00H                     ;Is it 0x00? Did it pass the test?
;        MVI B, 06H					;Return result code if not
;        RNZ                          ;No? Return then
;        ;6. Enable Devices
;        CALL KBDWAITINBUF
;        MVI A, 0AEH                 ;Send 0xAE command
;        OUT KBD_CMD
;        ;7. Reset Device
;        CALL KBDWAITINBUF           ;Wait untill ready to send
;        MVI A, 0FFH                 ;Send 0xFF to device
;        OUT KBD_DATA                ;Send it to device, not the controller
;        MVI C, 130                  ;Setup DELAY routine
;        CALL DELAY                  ;This is required to avoid freeze
;        CALL KBDWAITOUTBUF          ;Wait for response
;        IN KBD_DATA                 ;Get byte
;        CPI 0FAH                    ;Is it 0xFA? 0xFC means failure. No response means no device present.
;        MVI B, 07H					;Return result code if not
;        RNZ                          ;No? Return then
;        ;8. Set the Controller Configuration Byte (final)
;        CALL KBDWAITINBUF			;Send 0x60 command to the PS/2 controller
;        MVI A, 60H
;        OUT KBD_CMD
;        CALL KBDWAITINBUF			;Send actual configuration byte
;        MVI A, 08H					;Interrupts disabled, system flag set, first port clock enabled
;		OUT KBD_DATA				;second port clock disabled, first port translation disabled
;        ;9. Zero out buffer        
;        MVI A, 00H                  
;        STA KBDDATA					;Zero KBDDATA
;        STA KBDKRFL					;Zero key release flag
;        STA KBDSFFL					;Zero shift flag
;        STA KBDOLD					;Zero old data
;        STA KBDNEW					;Zero new data
;        MVI B, 00H					;Return result code
;        RET

BIOS_CALC_SECT_IN_BUFFER:
        LDA DISK_SECTOR  ; Load sector number
        MOV E, A         ; Store in E (low byte)
        MVI D, 0         ; Clear D (high byte)
        MVI B, 7         ; Loop counter (7 shifts)
CALC_SECTOR_SHIFT_LOOP:
        MOV A, E  
        ADD A   ; Shift E left (×2)
        MOV E, A  
        MOV A, D  
        ADC A   ; Shift D left with carry
        MOV D, A  
        DCR B   ; Decrement counter
        JNZ CALC_SECTOR_SHIFT_LOOP  ; Repeat until done		
		RET
        
CALC_CFLBA_FROM_PART_ADR:
        LXI H, PARTADDR
        LDA DISK_DISK
CALC_CFLBA_LOOP_START
        OR A
        JZ CALC_CFLBA_LOOP_END
        DCR A
        INX H
        INX H
        INX H
        INX H
        JMP CALC_CFLBA_LOOP_START
; Check if partition address is != 0
        MOV D, H
        MOV E, L
        CALL ISZERO32BIT
        JZ CALC_CFLBA_RET_ERR
CALC_CFLBA_LOOP_END:       
        MOV B, M
        INX H
        MOV C, M
        INX H
        MOV D, M
        INX H
        MOV E, M
        LHLD DISK_TRACK
        ; ADD lower 16 bits (HL + BC)
        MOV   A, L
        ADD   B            ; A = L + B
        MOV   B, A         ; Store result in C
        MOV   A, H
        ADC   C            ; A = H + C + Carry
        MOV   C, A         ; Store result in B
        ; ADD upper 16 bits (DE + Carry)
        MOV   A, D
        MVI   D, 00H
        ADC   D             ; D = D + Carry
        MOV   D, A
        MOV   A, E
        MVI   E, 00H
        ADC   E             ; E = E + Carry
        MOV   E, A
        ; Store the result back at LBA        
        MOV A, B
        STA CFLBA0
        MOV A, C
        STA CFLBA1
        MOV A, D
        STA CFLBA2
        MOV A, E
        STA CFLBA3
        MVI A, 01H
        RET
CALC_CFLBA_RET_ERR
        XOR A
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

        include "cf.asm"
        include "utils.asm"
        include "../common/definitions.asm"
        include "../common/hexdump.asm"
	
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
