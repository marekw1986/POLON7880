;Print the value in A in hex
;Affects B
HEXDUMP_A:
	PUSH PSW
	RRC
	RRC
	RRC
	RRC
	MVI B, 00FH
	ANA B
	CALL HEXDUMP_NIB
	POP PSW
	PUSH PSW
	MVI B, 00FH
	ANA B
	CALL HEXDUMP_NIB
	POP PSW
	RET	
HEXDUMP_NIB:
	MVI B, 48	;48 - 0 in ascii
	ADD B
	CPI 57+1	;57 - 9 in ascii
	;jp m,.hexdump_num
	MVI B, 65-57-1	;'A'-'9'-1
	ADD B
HEXDUMP_NUM:
	;send
