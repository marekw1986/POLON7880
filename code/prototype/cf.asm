CFINIT:
		MVI A, 00H
		STA	CFLBA3
		MVI A, 00H
		STA	CFLBA2
		MVI A, 00H
		STA	CFLBA1
		MVI A, 00H
		STA	CFLBA0
        MVI A, 04H
        OUT CFREG7
        CALL CFWAIT
        MVI A, 0E0H		                ;LBA3=0, MASTER, MODE=LBA
        OUT	CFREG6
        MVI A, 01H		                ;8-BIT TRANSFERS
        OUT CFREG1
        MVI A, 0EFH		                ;SET FEATURE COMMAND
        OUT CFREG7
        CALL CFWAIT
        CALL CFCHERR
        RET

CFWAIT:
        IN CFREG7
        ANI 80H                         ;MASK OUT BUSY FLAG
        JNZ CFWAIT
        RET	

CFCHERR:	
        IN CFREG7
        ANI	01H		                    ;MASK OUT ERROR BIT
        JZ	CFNERR
        LXI D, CFERRM
        CALL PRTSTG
        IN	CFREG1
        MOV L, A
        MVI H, 0H
        MVI C, 4H
        CALL PRTNUM
CFNERR:	
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
		MVI A, 00H
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
		LXI D, CFMSG1
		MVI B, 9
		CALL PRNSTR
        LXI D, LOAD_BASE+54
        MVI B, 20
        CALL SWPSTR
        LXI D, LOAD_BASE+54
        MVI B, 40
        CALL PRNSTR
        CALL CRLF
        RET        
        
CFRSECT:
		CALL CFSLBA						;SET LBA
		MVI A, 01H
		OUT	CFREG2						;READ ONE SECTOR
		CALL CFWAIT
		MVI A, 20H						;READ SECTOR COMMAND
		OUT	CFREG7
		LXI	D, LOAD_BASE
		CALL CFREAD
		CALL CFCHERR
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
        LXI D, LOAD_BASE
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
        CALL CRLF
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
		CALL CRLF
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
		CALL CRLF
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
		CALL CRLF
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
		MVI A, 01H
		OUT	CFREG2						;READ ONE SECTOR
		CALL CFWAIT
		MVI A, 20H						;READ SECTOR COMMAND
		OUT	CFREG7
		LXI	D, LOAD_BASE
		CALL CFREAD
		CALL CFCHERR
		RET
