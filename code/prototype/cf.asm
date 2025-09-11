CFINIT:
		XRA A
		STA	CFLBA3
		STA	CFLBA2
		STA	CFLBA1
		STA	CFLBA0
		STA PCFLBA3
		STA PCFLBA2
		STA PCFLBA1
		STA PCFLBA0
		STA CFVAL
        MVI A, 04H
        OUT CFREG7
        CALL CFWAIT_TMOUT
        MVI A, 0E0H		                ;LBA3=0, MASTER, MODE=LBA
        OUT	CFREG6
        MVI A, 01H		                ;8-BIT TRANSFERS
        OUT CFREG1
        MVI A, 0EFH		                ;SET FEATURE COMMAND
        OUT CFREG7
        CALL CFWAIT_TMOUT
        ORA A							;Check if wait loop timeouted
        JNZ CFINIT_RET					;If so there is no point in checking error code
        CALL CFCHERR
CFINIT_RET
        RET

CFWAIT:
        IN CFREG7
        ANI 80H                         ;MASK OUT BUSY FLAG
        JNZ CFWAIT
        RET
        
CFWAIT_TMOUT:
		MVI C, 64
CFWAIT_TMOUT_LOOP_EXT:
		MVI B, 255
CFWAIT_TMOUT_LOOP_INT:
        IN CFREG7
        ANI 80H                  		;MASK OUT BUSY FLAG
        JZ CFWAIT_TMOUT_OK
        DCR B
        JNZ CFWAIT_TMOUT_LOOP_INT
        DCR C
        JZ CFWAIT_TMOUT_NOK
        JMP CFWAIT_TMOUT_LOOP_EXT
CFWAIT_TMOUT_OK:
        XRA A						;OK result
        RET
CFWAIT_TMOUT_NOK:
		MVI A, 01H						;CF card timeout
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
		LXI	D, LOAD_BASE
		CALL CFREAD
		CALL CFCHERR
		RET        
  
CFINFO:	
        CALL CFWAIT
        MVI	A, 0ECH	                    ;DRIVE ID COMMAND
        OUT	CFREG7
        LXI	D, LOAD_BASE
        CALL CFREAD
        LXI D, LOAD_BASE+54
        MVI B, 20
        CALL SWPSTR
        LXI D, LOAD_BASE+54
        MVI B, 40
        CALL PRNSTR
        CALL NEWLINE
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
		
CFR32SECTORS:
		CALL CFSLBA
		MVI A, 20H						;Read 32 sectors
		OUT CFREG2
		CALL CFWAIT
		MVI A, 20H
		OUT CFREG7
		LXI D, LOAD_BASE
		CALL CFREAD
		CALL CFCHERR
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
        

PRN_PARTITION_TABLE:
        ;Print partition info
        ;Print partition 1 addres first
        MVI A, 1
        CALL PRN_IND_DIGIT
        LXI D, STARTADDRSTR
        MVI B, 6
        CALL PRNSTR
        CALL PRN_ZERO_EX
        LXI D, LOAD_BASE+446+8+3
		CALL HEXDUMP32BITVAL_PLUS_SPACE
        ;Print size then
        LXI D, SIZESTR
        MVI B, 6
        CALL PRNSTR
        CALL PRN_ZERO_EX
        LXI D, LOAD_BASE+446+12+3
        CALL HEXDUMP32BITVAL
        CALL NEWLINE
        ;Print partition 2 addres first    
        MVI A, 2;
        CALL PRN_IND_DIGIT
        LXI D, STARTADDRSTR
        MVI B, 6
        CALL PRNSTR
        CALL PRN_ZERO_EX
        LXI D, LOAD_BASE+462+8+3
		CALL HEXDUMP32BITVAL_PLUS_SPACE
		;Print size then
        LXI D, SIZESTR
        MVI B, 6
        CALL PRNSTR
        CALL PRN_ZERO_EX
		LXI D, LOAD_BASE+462+12+3
		CALL HEXDUMP32BITVAL
		CALL NEWLINE
        ;Print partition 3 addres first
        MVI A, 3;
        CALL PRN_IND_DIGIT
        LXI D, STARTADDRSTR
        MVI B, 6
        CALL PRNSTR
        CALL PRN_ZERO_EX
        LXI D, LOAD_BASE+478+8+3
        CALL HEXDUMP32BITVAL_PLUS_SPACE
		;Print size then
        LXI D, SIZESTR
        MVI B, 6
        CALL PRNSTR
        CALL PRN_ZERO_EX
		LXI D, LOAD_BASE+478+12+3
		CALL HEXDUMP32BITVAL
		CALL NEWLINE
        ;Print partition 4 addres first
        MVI A, 4;
        CALL PRN_IND_DIGIT
        LXI D, STARTADDRSTR
        MVI B, 6
        CALL PRNSTR
        CALL PRN_ZERO_EX
        LXI D, LOAD_BASE+494+8+3
        CALL HEXDUMP32BITVAL_PLUS_SPACE
		;Print size then
        LXI D, SIZESTR
        MVI B, 6
        CALL PRNSTR
        CALL PRN_ZERO_EX
		LXI D, LOAD_BASE+494+12+3
		CALL HEXDUMP32BITVAL
		CALL NEWLINE
		RET

LOAD_PARTITION1:
		LXI D, LOAD_BASE+446+8
		LDAX D
		OUT CFREG3						;LBA 0
		INX D
		LDAX D
		OUT CFREG4						;LBA 1
		INX D
		LDAX D
		OUT CFREG5						;LBA 2
		INX D
		LDAX D
		ANI 0FH							;FILTER OUT LBA BITS
		ORI 0E0H						;MODE LBA, MASTER DEV
		OUT CFREG6						;LBA 3
		; MVI A, 01H					;READ ONE SECTOR
		MVI A, 17						;READ 17 SECTORS (9kB-512 bytes to preserve stack)
		OUT	CFREG2						
		CALL CFWAIT
		MVI A, 20H						;READ SECTOR COMMAND
		OUT	CFREG7
		LXI	D, LOAD_BASE
		CALL CFREAD
		CALL CFCHERR
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
