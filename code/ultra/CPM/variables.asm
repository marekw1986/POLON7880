		ORG	 0FBDFH
SYSTEM_VARIABLES:
PARTADDR    DS   16                         ;PARTITION ADDR TABLE
BLKDAT:     DS   512                        ;BUFFER FOR SECTOR TRANSFER
BLKENDL     DS   1                          ;BUFFER ENDS
CFVAL       DS   1
CFLBA3	    DS	 1
CFLBA2	    DS	 1
CFLBA1	    DS	 1
CFLBA0	    DS	 1
PCFLBA3	    DS	 1
PCFLBA2	    DS	 1
PCFLBA1	    DS	 1
PCFLBA0	    DS	 1                            						;Keyboard new data
STKLMT:     DS   1                          ;TOP LIMIT FOR STACK
        ORG  0FFDFH
STACK       DS   1

CR          EQU  0DH
LF          EQU  0AH
