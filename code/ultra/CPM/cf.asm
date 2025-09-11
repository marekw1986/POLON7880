CFWAIT:
        IN CFREG7
        ANI 80H                         ;MASK OUT BUSY FLAG
        JNZ CFWAIT
        RET

CFCHERR:	
        IN CFREG7
        ANI	01H		                    ;MASK OUT ERROR BIT
        JZ	CFNERR
        IN	CFREG1
		RET
CFNERR:
		XRA A
        RET    
            
CFREAD:
        CALL CFWAIT
        IN CFREG7
        ANI	08H	                    ;FILTER OUT DRQ
        JZ CFREADE
        IN CFREG0		            ;READ DATA BYTE
        STAX D
        INX D
        JMP	CFREAD
CFREADE:
        RET
        
CFWRITE:
        CALL CFWAIT
        IN CFREG7
        ANI 08H                     ;FILTER OUT DRQ
        JZ CFWRITEE
        LDAX D
        OUT CFREG0
        INX D
        JMP CFWRITE
CFWRITEE:
        RET
        
CFSLBA:
        LDA CFLBA0		                ;LBA 0
        OUT CFREG3
        LDA CFLBA1		                ;LBA 1
        OUT CFREG4
        LDA CFLBA2		                ;LBA 2
        OUT CFREG5	
        LDA CFLBA3		                ;LBA 3
        ANI 0FH	                        ;FILTER OUT LBA BITS
        ORI 0E0H	                    ;MODE LBA, MASTER DEV
        OUT CFREG6
        RET

; Reads single sector
; Source in CFLBAx variables
; Destination address in DE        
CFRSECT:
		CALL CFSLBA						;SET LBA
		MVI A, 01H
		OUT	CFREG2						;READ ONE SECTOR
		CALL CFWAIT
		MVI A, 20H						;READ SECTOR COMMAND
		OUT	CFREG7
		CALL CFREAD
		CALL CFCHERR
		RET
        
; Reads single sector
; Source in CFLBAx variables
; Destination address is BLKDAT     
CFRSECT_WITH_CACHE:
		LDA CFVAL						; Check if we have valid data in buffer
		ORA A
		JZ	CFRSECT_WITH_CACHE_PERFORM  ; If not, read
		LXI H, CFLBA3					; Check if old and new LBA values are equal
		LXI D, PCFLBA3
		CALL IS32BIT_EQUAL
		ORA A							; If not, new LBA. Read imediately
		JZ CFRSECT_WITH_CACHE_PERFORM
		; We already have valid data in buffer. No need to read it again
		XRA A						; Store 0 in A to signalize no err
		RET
CFRSECT_WITH_CACHE_PERFORM:
		CALL CFSLBA						;SET LBA
		MVI A, 01H
		OUT	CFREG2						;READ ONE SECTOR
		CALL CFWAIT
		MVI A, 20H						;READ SECTOR COMMAND
		OUT	CFREG7
		LXI	D, BLKDAT
		CALL CFREAD
		CALL CFCHERR
		ORA A							; If A=0, no error, good read
		JNZ CFRSECT_WITH_CACHE_BAD
		PUSH PSW
		MVI A, 01H
		STA CFVAL
		; copy CFLBAx toPCFLBAx
        CALL CFUPDPLBA
		POP PSW 
		RET
CFRSECT_WITH_CACHE_BAD:
        PUSH PSW
        XRA A
        STA CFVAL
        POP PSW
		RET
        
CFWSECT:
        CALL CFSLBA                     ;SET LBA
        MVI A, 01H
        OUT CFREG2                      ;WRITE ONE SECTOR
        CALL CFWAIT
        MVI A, 30H                      ;WRITE SECTOR COMMAND
        OUT CFREG7
        CALL CFWRITE
        CALL CFCHERR
        RET
        
CFGETMBR:
		XRA A
		OUT CFREG3						;LBA 0
		OUT CFREG4						;LBA 1
		OUT CFREG5						;LBA 2
		;ANI 0FH	                        ;FILTER OUT LBA BITS
		;ORI 0E0H	                    ;MODE LBA, MASTER DEV
		MVI A, 0E0H
        OUT CFREG6
        MVI A, 01H
        OUT	CFREG2						;READ ONE SECTOR
		CALL CFWAIT
		MVI A, 20H						;READ SECTOR COMMAND
		OUT	CFREG7
		LXI	D, BLKDAT
		CALL CFREAD
		CALL CFCHERR
		RET

; This assumes that MBR is already in BLKDAT
; CALL CFGETMBR FIRST!
CFLDPARTADDR:
        LDA BLKDAT+446+8
        STA PARTADDR
        LDA BLKDAT+446+8+1
        STA PARTADDR+1
        LDA BLKDAT+446+8+2
        STA PARTADDR+2
        LDA BLKDAT+446+8+3
        STA PARTADDR+3
        
        LDA BLKDAT+462+8
        STA PARTADDR+4
        LDA BLKDAT+462+8+1
        STA PARTADDR+5
        LDA BLKDAT+462+8+2
        STA PARTADDR+6
        LDA BLKDAT+462+8+3
        STA PARTADDR+7
        
        LDA BLKDAT+478+8
        STA PARTADDR+8
        LDA BLKDAT+478+8+1
        STA PARTADDR+9
        LDA BLKDAT+478+8+2
        STA PARTADDR+10
        LDA BLKDAT+478+8+3
        STA PARTADDR+11

        LDA BLKDAT+494+8
        STA PARTADDR+12
        LDA BLKDAT+494+8+1
        STA PARTADDR+13
        LDA BLKDAT+494+8+2
        STA PARTADDR+14
        LDA BLKDAT+494+8+3
        STA PARTADDR+15
        RET

CFUPDPLBA:
        LDA CFLBA3
        STA PCFLBA3
        LDA CFLBA2
        STA PCFLBA2
        LDA CFLBA1
        STA PCFLBA1
        LDA CFLBA0
        STA PCFLBA0
        RET
