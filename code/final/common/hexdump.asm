;Dumps number of bytes (passed in B) in hex
;Beginning in DE
HEXDUMP:
	MOV A, B
	CPI 00H
	RZ
	LDAX D				;Get byte
	CALL HEXDUMP_A		;Print current byte as hex value
	MVI A, 32			;Print space
	CALL OUT_CHAR
	INX D
	DCR B
	JMP HEXDUMP

;Print the value in A in hex
HEXDUMP_A:
	PUSH PSW
	RRC
	RRC
	RRC
	RRC
	ANI 00FH
	CALL HEXDUMP_NIB
	POP PSW
	PUSH PSW
	ANI 00FH
	CALL HEXDUMP_NIB
	POP PSW
	RET	
HEXDUMP_NIB:
	ADI 48	;48 is 0 in ascii
	CPI 57+1	;57 is 9 in ascii
	JC HEXDUMP_NUM
	ADI 65-57-1	;'A'-'9'-1
HEXDUMP_NUM:
	CALL OUT_CHAR
	RET

HEXDUMP32BITVAL:
	    LDAX D
        CALL HEXDUMP_A
        DCX D
        LDAX D
        CALL HEXDUMP_A
        DCX D
        LDAX D
        CALL HEXDUMP_A
        DCX D
        LDAX D
        CALL HEXDUMP_A
        RET

HEXDUMP32BITVAL_PLUS_SPACE:
		CALL HEXDUMP32BITVAL
		MVI A, 32
		CALL OUT_CHAR
		RET
