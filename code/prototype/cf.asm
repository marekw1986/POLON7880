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
        ORI 0F0H	                    ;MODE LBA, MASTER DEV
        OUT CFREG6
        RET
        
CFGETMBR:
		MVI A, 00H
		OUT CFREG3						;LBA 0
		OUT CFREG4						;LBA 1
		OUT CFREG5						;LBA 2
		ANI 0FH	                        ;FILTER OUT LBA BITS
		ORI 0F0H	                    ;MODE LBA, MASTER DEV
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
  
CFINFO:	
        CALL CFWAIT
        MVI	A, 0ECH	                    ;DRIVE ID COMMAND
        OUT	CFREG7
        LXI	D, BLKDAT
        CALL CFREAD
        ;CALL CRLF
;PRINT CFMSG
		LXI D, CFMSG1
		MVI B, 9
		CALL PRNSTR
		;CALL CRLF
;PRINT SERIAL
        ;LXI D, CFSER
        ;CALL PRTSTG
        ;LXI D, BLKDAT+20
        ;MVI B, 10
        ;CALL SWPSTR
        ;LXI D, BLKDAT+20
        ;MVI B, 20
        ;CALL PRNSTR
        ;CALL CRLF
;PRINT FIRMWARE REV
        ;LXI D, CFFW
        ;CALL PRTSTG
        ;LXI D, BLKDAT+46
        ;MVI B, 4
        ;CALL SWPSTR
        ;LXI D, BLKDAT+46
        ;MVI B, 8
        ;CALL PRNSTR
        ;CALL CRLF
;PRINT MODEL NUMBER
        ;LXI D, CFMOD
        ;CALL PRTSTG
        LXI D, BLKDAT+54
        MVI B, 20
        CALL SWPSTR
        LXI D, BLKDAT+54
        MVI B, 40
        CALL PRNSTR
        CALL CRLF
;PRINT LBA SIZE
        ;LXI D, CFLBAS
        ;CALL PRTSTG
        ;LDX	#BLKDAT+123
        ;JSR	OUT2HS
			;MVI C, 0
			;LHLD BLKDAT+123
			;CALL PRTNUM
			;CALL CRLF
        ;DEX
        ;DEX
        ;JSR	OUT2HS
			;MVI C, 0
			;LHLD BLKDAT+125
			;CALL PRTNUM
			;CALL CRLF
        ;DEX
        ;DEX
        ;JSR	OUT2HS
			;MVI C, 0
			;LHLD BLKDAT+127
			;CALL PRTNUM
			;CALL CRLF
        ;DEX
        ;DEX
        ;JSR	OUT2HS
			;MVI C, 0
			;LHLD BLKDAT+129
			;CALL PRTNUM
        CALL CRLF
        RET        
        
CFRSECT:
		CALL CFSLBA						;SET LBA
		MVI A, 01H
		OUT	CFREG2						;READ ONE SECTOR
		CALL CFWAIT
		MVI A, 20H						;READ SECTOR COMMAND
		OUT	CFREG7
		LXI	D, BLKDAT
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
        LXI D, BLKDAT
        CALL CFWRITE
        CALL CFCHERR
        RET
