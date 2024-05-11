;Dumps number of bytes (passed in B) in hex
;Beginning in DE
HEXDUMP:
	MOV A, B
	CPI 00H
	RZ
	LDAX D				;Get byte
	CALL HEXDUMP_A		;Print current byte as hex value
	MVI A, 32			;Print space
	CALL OUTC
	INX D
	DCR B
	JNZ HEXDUMP

;Print the value in A in hex
;Affects C
HEXDUMP_A:
	PUSH PSW
	RRC
	RRC
	RRC
	RRC
	MVI C, 00FH
	ANA C
	CALL HEXDUMP_NIB
	POP PSW
	PUSH PSW
	MVI C, 00FH
	ANA C
	CALL HEXDUMP_NIB
	POP PSW
	RET	
HEXDUMP_NIB:
	MVI C, 48	;48 is 0 in ascii
	ADD C
	CPI 57+1	;57 is 9 in ascii
	JNC HEXDUMP_NUM
	MVI C, 65-57-1	;'A'-'9'-1
	ADD C
HEXDUMP_NUM:
	CALL OUTC
