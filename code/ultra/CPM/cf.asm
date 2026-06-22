; ---------------------------------------------------------------------------
; cf_8080.asm - CompactFlash driver for 8080 CP/M BIOS
;
; Changes vs original:
;
; NEW VARIABLE:
;   DEFERREDWR DS 1  - dirty-buffer flag. Set to 1 when BLKDAT has been
;   modified by a deferred (C=0) write but not yet physically written to
;   the CF card. Must also be initialised to 0 in CFVAR_INIT in bios.asm.
;
; CFRSECT_WITH_CACHE_PERFORM:
;   Added deferred-write flush before evicting BLKDAT for a new LBA.
;   Without this, a dirty buffer would be silently overwritten by the
;   incoming read, losing the deferred write permanently.
;
; NEW - CFSWAPLBA:
;   Atomically swaps CFLBAx <-> PCFLBAx. Required by the flush-before-
;   evict logic: we must write using the OLD (dirty block's) LBA while
;   CFLBAx currently holds the NEW (wanted) LBA.
;
; NEW - CFFLUSHDEFFERED:
;   Explicitly flushes a pending deferred write. Called by BIOS_WBOOT_PROC
;   before reloading CCP (so a dirty block is never lost on warm boot) and
;   by BIOS_WRITE_NEW_TRACK before overwriting BLKDAT with a new block.
;   Returns A=0 on success or if nothing needed flushing.
;   Returns A<>0 on write failure; DEFERREDWR is left set on failure so
;   the dirty block is not silently treated as safely committed.
; ---------------------------------------------------------------------------

; ---------------------------------------------------------------------------
; Variables - these live in the data area alongside BLKDAT, CFLBAx etc.
; Add DEFERREDWR to the existing variable block in your data section.
; ---------------------------------------------------------------------------
; PARTADDR    DS   16
; BLKDAT      DS   512
; BLKENDL     DS   1
; CFVAL       DS   1
; CFLBA3      DS   1
; CFLBA2      DS   1
; CFLBA1      DS   1
; CFLBA0      DS   1
; PCFLBA3     DS   1
; PCFLBA2     DS   1
; PCFLBA1     DS   1
; PCFLBA0     DS   1
DEFERREDWR  DS   1          ; NEW: 1 = BLKDAT is dirty, physical write pending

; ---------------------------------------------------------------------------
; CFWAIT - wait until CF card is not busy
; ---------------------------------------------------------------------------
CFWAIT:
        IN CFREG7
        ANI 80H                         ;MASK OUT BUSY FLAG
        JNZ CFWAIT
        RET

; ---------------------------------------------------------------------------
; CFCHERR - check CF error register
; Returns A=0 if no error, A=error code otherwise
; ---------------------------------------------------------------------------
CFCHERR:
        IN CFREG7
        ANI 01H                         ;MASK OUT ERROR BIT
        JZ CFNERR
        IN CFREG1
        RET
CFNERR:
        XRA A
        RET

; ---------------------------------------------------------------------------
; CFREAD - read bytes from CF data register into memory at DE
; Reads until DRQ deasserts
; ---------------------------------------------------------------------------
CFREAD:
        CALL CFWAIT
        IN CFREG7
        ANI 08H                         ;FILTER OUT DRQ
        JZ CFREADE
        IN CFREG0                       ;READ DATA BYTE
        STAX D
        INX D
        JMP CFREAD
CFREADE:
        RET

; ---------------------------------------------------------------------------
; CFWRITE - write bytes from memory at DE to CF data register
; Writes until DRQ deasserts
; ---------------------------------------------------------------------------
CFWRITE:
        CALL CFWAIT
        IN CFREG7
        ANI 08H                         ;FILTER OUT DRQ
        JZ CFWRITEE
        LDAX D
        OUT CFREG0
        INX D
        JMP CFWRITE
CFWRITEE:
        RET

; ---------------------------------------------------------------------------
; CFSLBA - load CFLBAx variables into CF LBA registers
; ---------------------------------------------------------------------------
CFSLBA:
        LDA CFLBA0                      ;LBA 0
        OUT CFREG3
        LDA CFLBA1                      ;LBA 1
        OUT CFREG4
        LDA CFLBA2                      ;LBA 2
        OUT CFREG5
        LDA CFLBA3                      ;LBA 3
        ANI 0FH                         ;FILTER OUT LBA BITS
        ORI 0E0H                        ;MODE LBA, MASTER DEV
        OUT CFREG6
        RET

; ---------------------------------------------------------------------------
; CFRSECT - read single sector
; CFLBAx must be set before calling. Destination address in DE.
; ---------------------------------------------------------------------------
CFRSECT:
        CALL CFSLBA                     ;SET LBA
        MVI A, 01H
        OUT CFREG2                      ;READ ONE SECTOR
        CALL CFWAIT
        MVI A, 20H                      ;READ SECTOR COMMAND
        OUT CFREG7
        CALL CFREAD
        CALL CFCHERR
        RET

; ---------------------------------------------------------------------------
; CFRSECT_WITH_CACHE - read single sector into BLKDAT, with caching.
; CFLBAx must be set to the wanted LBA before calling.
; Returns A=0 on success (including cache hit), A<>0 on error.
;
; Cache hit (CFVAL=1 and CFLBAx==PCFLBAx): returns immediately, no card I/O.
; Cache miss: flushes dirty buffer if DEFERREDWR=1 (using old PCFLBAx via
;   CFSWAPLBA), then reads the new sector from the card.
; ---------------------------------------------------------------------------
CFRSECT_WITH_CACHE:
        LDA CFVAL                       ; is there valid data in buffer?
        ORA A
        JZ CFRSECT_WITH_CACHE_PERFORM   ; no: must read
        LXI H, CFLBA3                   ; yes: check if LBA matches
        LXI D, PCFLBA3
        CALL IS32BIT_EQUAL
        ORA A
        JZ CFRSECT_WITH_CACHE_PERFORM   ; LBA changed: must read (and flush first)
        ; Cache hit - buffer already holds the right block
        XRA A                           ; A=0: no error
        RET

CFRSECT_WITH_CACHE_PERFORM:
        ; Cache miss. Before overwriting BLKDAT with the new block, check
        ; whether it currently holds a dirty deferred write for the OLD LBA.
        ; If so, flush it to the card first using the OLD LBA (PCFLBAx).
        LDA DEFERREDWR
        ORA A
        JZ CFRSECT_WITH_CACHE_PERFORM1  ; nothing pending, go straight to read
        ; Flush dirty buffer using OLD LBA.
        ; CFSWAPLBA puts the old LBA (currently in PCFLBA) into CFLBA so
        ; CFWSECT sends it to the right place, and saves the new wanted LBA
        ; (currently in CFLBA) into PCFLBA so we can restore it afterward.
        CALL CFSWAPLBA                  ; CFLBA = dirty block's LBA, PCFLBA = new LBA
        LXI D, BLKDAT
        CALL CFWSECT                    ; write dirty buffer to card
        ORA A
        JNZ CFRSECT_WITH_CACHE_BAD      ; write failed: propagate error
        XRA A
        STA DEFERREDWR                  ; confirmed written: buffer is now clean
        CALL CFSWAPLBA                  ; restore: CFLBA = new wanted LBA
CFRSECT_WITH_CACHE_PERFORM1:
        ; Read new block from card into BLKDAT
        CALL CFSLBA                     ;SET LBA (new wanted LBA now in CFLBA)
        MVI A, 01H
        OUT CFREG2                      ;READ ONE SECTOR
        CALL CFWAIT
        MVI A, 20H                      ;READ SECTOR COMMAND
        OUT CFREG7
        LXI D, BLKDAT
        CALL CFREAD
        CALL CFCHERR
        ORA A                           ; A=0: good read
        JNZ CFRSECT_WITH_CACHE_BAD
        PUSH PSW
        MVI A, 01H
        STA CFVAL                       ; buffer is now valid
        CALL CFUPDPLBA                  ; PCFLBA = CFLBA (record what's in buffer)
        POP PSW
        RET
CFRSECT_WITH_CACHE_BAD:
        PUSH PSW
        XRA A
        STA CFVAL                       ; invalidate buffer on any error
        POP PSW
        RET

; ---------------------------------------------------------------------------
; CFWSECT - write single sector from BLKDAT to card
; CFLBAx must be set before calling.
; Returns A=0 on success, A<>0 on error.
; ---------------------------------------------------------------------------
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

; ---------------------------------------------------------------------------
; CFGETMBR - read MBR (LBA 0) into BLKDAT
; Returns A=0 on success, A<>0 on error.
; ---------------------------------------------------------------------------
CFGETMBR:
        XRA A
        OUT CFREG3                      ;LBA 0
        OUT CFREG4                      ;LBA 1
        OUT CFREG5                      ;LBA 2
        MVI A, 0E0H
        OUT CFREG6                      ;MODE LBA, MASTER DEV
        MVI A, 01H
        OUT CFREG2                      ;READ ONE SECTOR
        CALL CFWAIT
        MVI A, 20H                      ;READ SECTOR COMMAND
        OUT CFREG7
        LXI D, BLKDAT
        CALL CFREAD
        CALL CFCHERR
        RET

; ---------------------------------------------------------------------------
; CFLDPARTADDR - parse partition table from MBR already in BLKDAT
; Call CFGETMBR first. Loads LBA start addresses for 4 partitions
; into PARTADDR table (16 bytes, 4 bytes per partition, little-endian).
; ---------------------------------------------------------------------------
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

; ---------------------------------------------------------------------------
; CFUPDPLBA - copy CFLBAx to PCFLBAx
; Records which block is currently resident in BLKDAT.
; Clobbers A only.
; ---------------------------------------------------------------------------
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

; ---------------------------------------------------------------------------
; CFSWAPLBA - swap CFLBAx <-> PCFLBAx
; Used by CFRSECT_WITH_CACHE_PERFORM to temporarily install the old (dirty)
; LBA into CFLBAx for CFWSECT, then swap back to the new wanted LBA.
; Clobbers A and B only.
; ---------------------------------------------------------------------------
CFSWAPLBA:
        LDA CFLBA0
        MOV B, A
        LDA PCFLBA0
        STA CFLBA0
        MOV A, B
        STA PCFLBA0

        LDA CFLBA1
        MOV B, A
        LDA PCFLBA1
        STA CFLBA1
        MOV A, B
        STA PCFLBA1

        LDA CFLBA2
        MOV B, A
        LDA PCFLBA2
        STA CFLBA2
        MOV A, B
        STA PCFLBA2

        LDA CFLBA3
        MOV B, A
        LDA PCFLBA3
        STA CFLBA3
        MOV A, B
        STA PCFLBA3
        RET

; ---------------------------------------------------------------------------
; CFFLUSHDEFFERED - flush pending deferred write if one exists
; Returns A=0 if nothing to flush, or if flush succeeded.
; Returns A<>0 on write failure. DEFERREDWR is left set on failure so
; the dirty block is not silently treated as committed - a later retry
; via cache eviction or warm boot can still recover it.
; ---------------------------------------------------------------------------
CFFLUSHDEFFERED:
        LDA CFVAL
        ORA A
        RZ                              ; no valid buffer, nothing to flush (A=0)
        LDA DEFERREDWR
        ORA A
        RZ                              ; no pending write (A=0)
        LXI D, BLKDAT
        CALL CFWSECT                    ; write buffer using current CFLBAx
        ORA A
        RNZ                             ; failed: propagate error, leave DEFERREDWR=1
        XRA A
        STA DEFERREDWR                  ; success: buffer is now clean
        RET
