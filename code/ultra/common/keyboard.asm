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
KBDINIT:
        ;1. Disable devices
        CALL KBDWAITINBUF           ;Send 0xAD command to the PS/2 controller
        MVI A, 0ADH
        OUT KBD_CMD
        ;2. Flush The Output Buffer
        IN KBD_STATUS
        ANI 01H                     ;Check if there is data to flush
        JZ KBDCRTLSET              	;No? Next step then
        IN KBD_DATA                 ;Yes? Get the data byte        
KBDCRTLSET:
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
        MVI A, 09H					;Interrupts enabled, system flag set, first port clock enabled
		OUT KBD_DATA				;second port clock disabled, first port translation disabled
        ;9. Zero out buffer        
        XRA A                  
        STA KBDDATA					;Zero KBDDATA
        STA KBDKRFL					;Zero key release flag
        STA KBDSFFL					;Zero shift flag
        STA KBDOLD					;Zero old data
        STA KBDNEW					;Zero new data
        MVI B, 00H					;Return result code
        RET
        
KBDWAITINBUF:
		;TODO: Timeout
		IN KBD_STATUS
		ANI 02H
		JNZ KBDWAITINBUF
		RET
		
KBDWAITOUTBUF:
		;TODO: Timeout
		IN KBD_STATUS
		ANI 01H
		JZ KBDWAITOUTBUF
		RET
		
KBD2ASCII:
		LDA KBDDATA					;Load latest received PS/2 scancode
		CPI 00H						;Is it 0? (this is needed - LDA doesn't affect flags)
		RZ							;Return if code = 0;
		CPI 0F0H					;Is it 0xF0 (key release)?
		JNZ KBD2A_CHKSFT			;If not, go to the next stage
		MVI A, 01H					;Set key release flag
		STA KBDKRFL
		JMP KBD2A_CLRDATA_RETURN	;Zero out KBBDDATA and return
KBD2A_CHKSFT:
		CPI 12H						;Check if it is (left) shift code
		JZ KBD2A_CHKKRSETSF			;If not, go to the next stage
		CPI 59H						;Check if it is (right) shift code
		JZ KBD2A_CHKKRSETSF			;If not, go to the next stage
KBD2A_SVNEWDATA:
		MOV B, A					;Save current code in B
		LDA KBDNEW
		STA KBDOLD					;Old data = new data
		MOV A, B
		STA KBDNEW					;New data = received code
		LDA KBDKRFL
		CPI 01H						;Check if key release flag is set
        JNZ KBD2A_CHKSHFFLSET		;If not, go to the next stage
        LDA KBDOLD					;Load old data to acumulator
        MOV B, A
        LDA KBDNEW
        CMP B						;Compare acumulator with new data
        JZ KBD2A_CLRKRFL			;If yes, clear release flag and return
        NOP							;If not, handle error here.
        NOP							;These are just a placeholders
KBD2A_CLRKRFL:
		XRA A
		STA KBDKRFL
		JMP KBD2A_CLRDATA_RETURN
KBD2A_CHKSHFFLSET:
		MVI L, 01H					;Just assume we are looking LC table
		LDA KBDNEW					;Put newest key scancode in A
		MOV B, A					;Then move it to B
		LDA KBDSFFL					;Check shift flag
		CPI 00H                     ;This is needed - LDA doesn't affect zero flag
		JZ KBD2A_LOOKUP				;Just search in LC table
		MVI L, 02H					;We are looking in UC table if shift flag is set
KBD2A_LOOKUP		
		CALL KBDSCANTABLE			;Call scantable searching subroutine
		CPI 00H						;Check if it returned zero (this is needed)
		JZ KBD2A_CLRDATA_RETURN		;If yes, clear data and return
		MOV B, A					;Else clear KBDDATA and return
		XRA A					;Passing ASCII character in A
		STA KBDDATA
		MOV A, B
		RET
KBD2A_CHKKRSETSF:        
		LDA KBDKRFL
		CPI 01H						;Check if key release flag is set
		JZ KBD2A_CLRFLDATA_RETURN	;If yes clear flags (and data?) and return
		MVI A, 01H					;If not, set shift flag
		STA KBDSFFL
		JMP KBD2A_CLRDATA_RETURN    ;Clear KBDDATA and return        
KBD2A_CLRFLDATA_RETURN:
		XRA A
		STA KBDSFFL
		STA KBDKRFL
KBD2A_CLRDATA_RETURN:
        XRA A
		STA KBDDATA		
		RET
		
; Current scancode must be loaded to B
; Shift in L
; Uses A, C, DE, HL		
KBDSCANTABLE:
		LXI D, PS2_SCANCODES  			;Table address
		MVI H, 00H						; Make sure that H is zero
KBDSCANTABLE_LOOP:
		LDAX D				        	;Load next scancode from table to A
		CMP B							;Compare A with current receivedscancode (stored in B)
		JZ KBDSCANTABLE_FOUND
		INX D                       	;Increment index pointer three times
		INX D                       	;To go to the next scancode
		INX D
		MOV A, D						;Move high address stored in DE to A
		MVI C, HIGH(PS2_SCANCODES_END)	;High byte of address of scandoce table end in C
		CMP C							;Compare A with C
		JNZ KBDSCANTABLE_REL
		MOV A, E						;Move low address stored in DE to A
		MVI C, LOW(PS2_SCANCODES_END)	;Low byte of address of scandoce table end in C
		CMP C							;Compare A with C
KBDSCANTABLE_REL:
		JC KBDSCANTABLE_LOOP
		XRA A                     	;End of the loop, return zero
		RET
KBDSCANTABLE_FOUND:
		DAD D							;Add DE to HL
		MOV D, H						;Move result back to DE
		MOV E, L
		LDAX D							;Load ASCII code to A				
		RET								;Then return
