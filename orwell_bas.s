; Microsoft BASIC for 6502 (OSI VERSION) tweaked to run on the Orwell computer.
; Simon Jansen
;
; October 2013
; VERSION 1.0 REV 0.0
; Added in my own routines for keyboard scanning and reading as well
; as video output.
; ROM start moved to $8000.
; Space from $0200 - $0310 set aside for ORWELL variables.
; RAM start set to $030F.
; Width set to 38.
; Added CLS BASIC command.
; Added INKEY$ BASIC command.
; Changed LOAD to be silent. 
; Added ECHOLOAD command to echo loading to the screen.
; Added a MOVE (cursor) command. The row and column values are poked into memory
; at locations; row = 783, column = 784 
;
; April 2014
; VERSION 1.0 REV 1.1
; Updated video hardware to Grant Searles monitor/keyboard. 
; New character outputting routines added.
; Column width set to 80 by default. 
; Added new variables to support graphics commands.	
; Changed MOVE command to use new variables.
;
; VERSION 1.0 REV 1.2
; Added in PLOT, LINE and RECT commands.
; Changed token parsing to work across 256 byte boundary.
; Added SETCUR function.
; Added in CIRC command.
; Modified LIST command to handle token name table greater than 256 bytes.
; Added FONT command.
;
; VERSION 1.0 REV 1.3
; Changed INKEY$ to GET. Will return empty string on no key press.
; Removed MEMORY and TERMINAL width prompts.
;
; VERSION 1.0 REV 1.4
; Changes to save/load mode.
;
; VERSION 1.0 REV 1.5
; Added in support for IO board.
;
; VERSION 1.0 REV 1.6
; Added in support for binary loading.
; Added in hooks for interrupt vector.
;
; VERSION 1.0 REV 1.7
; Changes to the serial receive code.
;
; VERSION 1.0 REV 1.8
; Addred flow control around parsing lines.
; Reverted to 80 column default width.
;
; ==================================================================================
;
; Microsoft BASIC for 6502 (OSI VERSION)
;
; ==================================================================================
; MODIFIED FROM THE ORIGINAL FILES AT http://www.pagetable.com/?p=46
; MERGED INTO ONE FILE AND MACROS AND CONDITIONAL STATEMENTS REMOVED
; BY G. SEARLE 2013
;
; I/O and dummy load/saves added to the end of this code
;
; This then assembles to the OSI version with the following
; minor changes to the original BASIC code:
; 1. Control-C call changed
; 2. Load/save calls changed
; 3. RAM start set to $0200 instead of $0300
; 4. ROM start set to $C000
; 5. Second letter of error messages back to ASCII value (instead of $80+val)
; ==================================================================================
;
; Extract of original header comments follows:
;
; (first revision of this distribution, 20 Oct 2008, Michael Steil www.pagetable.com)
;
;

; Name                 Release   MS Version    ROM   9digit INPUTBUFFER   extensions
;---------------------------------------------------------------------------------------------------
; OSI BASIC             1977     1.0 REV 3.2    Y      N ZP            -
;
; Credits:
; * main work by Michael Steil
; * function names and all uppercase comments taken from Bob Sander-Cederlof's excellent AppleSoft II disassembly:
;   http://www.txbobsc.com/scsc/scdocumentor/
; * Applesoft lite by Tom Greene http://cowgod.org/replica1/applesoft/ helped a lot, too.
; * Thanks to Joe Zbicak for help with Intellision Keyboard BASIC
; * This work is dedicated to the memory of my dear hacking pal Michael "acidity" Kollmann.

.debuginfo +

.setcpu "6502"
.macpack longbranch

; =============================================================================
; Zero page.
; =============================================================================

ZP_START1 = $00
ZP_START2 = $0D
ZP_START3 = $5B
ZP_START4 = $65

; =============================================================================
; Extra ZP variables.
; =============================================================================

USR := $000A

; =============================================================================
; Constants.
; =============================================================================

STACK_TOP           := $FC
SPACE_FOR_GOSUB     := $33
NULL_MAX            := $0A
WIDTH               := 80 ; SJ. 72 Original default value. 80 for Orwell machine.
WIDTH2              := 56
GRAPHICSXMAX		:= 160 ; SJ added. Maximum graphics X co-ord (160 pixels).
GRAPHICSYMAX		:= 100 ; SJ added. Maximum graphics Y co-ord (100 pixels).

; =============================================================================
; Memory layout.
; =============================================================================

ORWELL_VARIABLES    := $0200        ; Added to support Orwell keyboard routines.
RAMSTART2           := $0320        ; Start of program RAM.
BYTES_FP            := 4
BYTES_PER_ELEMENT   := BYTES_FP
BYTES_PER_VARIABLE  := BYTES_FP+2
MANTISSA_BYTES      := BYTES_FP-1
BYTES_PER_FRAME     := 2*BYTES_FP+8
FOR_STACK1          := 2*BYTES_FP+5
FOR_STACK2          := BYTES_FP+4
MAX_EXPON           = 10
STACK               := $0100

INPUTBUFFERX = INPUTBUFFER & $FF00

; =============================================================================
; Carriage return/line feed.
; =============================================================================

CR        = $0D
LF        = $0A
CRLF_1    := CR
CRLF_2    := LF

; =============================================================================
; Zero page variables.
; =============================================================================

.feature org_per_seg
.zeropage
.org $0000
.org ZP_START1  ; $00

GORESTART:
    .res 3
GOSTROUT:
    .res 3
GOAYINT:
    .res 2
GOGIVEAYF:
    .res 2

.org ZP_START2  ; $OD
Z15:
    .res 1
POSX:           ; Number of characters since last CR.
    .res 1
Z17:            ; Screen width for auto CR/LF.
    .res 1
Z18:
    .res 1
LINNUM:         ; Current line number.
TXPSV:
    .res 2
INPUTBUFFER:    ; Line input buffer.

.org ZP_START3  ; $5B
CHARAC:
    .res 1      ; Alternate string terminator.
ENDCHR:
    .res 1      ; String terminator.
EOLPNTR:
    .res 1      ; String terminator.
DIMFLG:
    .res 1      
VALTYP:
    .res 1
DATAFLG:
    .res 1      ; Used in parse.
SUBFLG:
    .res 1
INPUTFLG:
    .res 1      ; $40 for GET. $98 for READ.
CPRMASK:
    .res 1      ; Receives CPRTYP in FRMEVL.
Z14:
    .res 1      ; Error flag.

.org ZP_START4  ; $65
load_address:   ; SJ added April 2015. 
    .res 2
TEMPPT:
    .res 1
LASTPT:
    .res 2
TEMPST:
    .res 9
INDEX:
    .res 2
DEST:
    .res 2
RESULT:
    .res BYTES_FP
RESULT_LAST = RESULT + BYTES_FP-1
TXTTAB:
    .res 2    ; Pointer to start of BASIC program space.
VARTAB:
    .res 2    ; Pointer to start of BASIC variable storage.
ARYTAB:
    .res 2    ; Pointer to start of BASIC array space.
STREND:
    .res 2    ; Pointer to end of array space/start of free memory.
FRETOP:
    .res 2    ; Pointer to end of string space/top of free memory.
FRESPC:
    .res 2    ; Pointer to top of BASIC memory.
MEMSIZ:
    .res 2
CURLIN:
    .res 2    ; Current line number.
OLDLIN:
    .res 2    ; Old line number.
OLDTEXT:
    .res 2
Z8C:
    .res 2
DATPTR:
    .res 2    ; Data pointer.
INPTR:
    .res 2
VARNAM:
    .res 2
VARPNT:
    .res 2
FORPNT:
    .res 2
LASTOP:
    .res 2
CPRTYP:
    .res 1
FNCNAM:
TEMP3:
    .res 2
DSCPTR:
    .res 2
DSCLEN:
    .res 2
JMPADRS := DSCLEN + 1
Z52:
    .res 1
ARGEXTENSION:
TEMP1:
    .res 1
HIGHDS:
    .res 2
HIGHTR:
    .res 2
INDX:
TMPEXP:
TEMP2:
    .res 1
EXPON:
    .res 1
LOWTR:
LOWTRX:
    .res 1
EXPSGN:
    .res 1
FAC:
    .res BYTES_FP
FAC_LAST = FAC + BYTES_FP-1
FACSIGN:
    .res 1
SERLEN:
    .res 1
SHIFTSIGNEXT:
    .res 1
ARG:
    .res BYTES_FP
ARG_LAST = ARG + BYTES_FP-1
ARGSIGN:
    .res 1
STRNG1:
    .res 2
SGNCPR = STRNG1
FACEXTENSION = STRNG1+1
STRNG2:
    .res 2
    
; SJ Added April 2014.
; Variables used in graphics routines.
DX:	
 	.res 1      
IX:	
 	.res 1      
DY:	
 	.res 1      
IY:
 	.res 1      
X1:    
    .res 1      
Y1:
    .res 1      
X2:   
    .res 1     
Y2:
	.res 1
FF:	      
	.res 1
Rad:
	.res 1
	    
; =============================================================================
; Space allocated for code which gets copied from ROM into RAM here
; during startup.
; =============================================================================

CHRGET:
TXTPTR = <(GENERIC_TXTPTR-GENERIC_CHRGET + CHRGET)
CHRGOT = <(GENERIC_CHRGOT-GENERIC_CHRGET + CHRGET)
CHRGOT2 = <(GENERIC_CHRGOT2-GENERIC_CHRGET + CHRGET)
RNDSEED = <(GENERIC_RNDSEED-GENERIC_CHRGET + CHRGET)

; =============================================================================
; Orwell specific constants.
; =============================================================================

IO          := $6000        ; Top of I/O space. 8 bit buffer.
via1        := $5000        ; VIA 1 I/O. Used for keyboard and video.
via1_b      := via1 + $0    ; VIA port b.
via1_a      := via1 + $1    ; VIA port a.
via1_ddrb   := via1 + $2    ; VIA port b direction 1 = output.
via1_ddra   := via1 + $3    ; VIA port a direction 1 = output.
via1_t1cl   := via1 + $4    ; VIA timer 1 low latch.
via1_t1ch   := via1 + $5    ; VIA timer 1 high latch.
via1_acr    := via1 + $B    ; VIA auxiliary control register.
via1_pcr    := via1 + $C    ; VIA peripheral control register.
via1_ifr    := via1 + $D    ; VIA interrupt flag register.
via1_ier    := via1 + $E    ; VIA interrupt enable register.

acia1       := $4800         ; ACIA 1 I/O.
acia1_d     := acia1 + $0    ; ACIA data register.
acia1_s     := acia1 + $1    ; ACIA reset/status register.
acia1_cm    := acia1 + $2    ; ACIA command register.
acia1_ct    := acia1 + $3    ; ACIA control register.

via2        := $4400        ; VIA 2 I/O. Used for analog to digital conversion.
via2_b      := via2 + $0    ; VIA port b.
via2_a      := via2 + $1    ; VIA port a.
via2_ddrb   := via2 + $2    ; VIA port b direction 1 = output.
via2_ddra   := via2 + $3    ; VIA port a direction 1 = output.
via2_t1cl   := via2 + $4    ; VIA timer 1 low latch.
via2_t1ch   := via2 + $5    ; VIA timer 1 high latch.
via2_acr    := via2 + $B    ; VIA auxiliary control register.
via2_pcr    := via2 + $C    ; VIA peripheral control register.
via2_ifr    := via2 + $D    ; VIA interrupt flag register.
via2_ier    := via2 + $E    ; VIA interrupt enable register.

via3        := $4200        ; VIA 1 I/O. Used for general purpose IO.
via3_b      := via3 + $0    ; VIA port b.
via3_a      := via3 + $1    ; VIA port a.
via3_ddrb   := via3 + $2    ; VIA port b direction 1 = output.
via3_ddra   := via3 + $3    ; VIA port a direction 1 = output.
via3_t1cl   := via3 + $4    ; VIA timer 1 low latch.
via3_t1ch   := via3 + $5    ; VIA timer 1 high latch.
via3_acr    := via3 + $B    ; VIA auxiliary control register.
via3_pcr    := via3 + $C    ; VIA peripheral control register.
via3_ifr    := via3 + $D    ; VIA interrupt flag register.
via3_ier    := via3 + $E    ; VIA interrupt enable register.

; =============================================================================
; Orwell specific variables.
; =============================================================================

.segment "ORWELL"
;.org ORWELL_VARIABLES

key_counter:    ; $200 Key debounce counter.
    .res 1
key_buffer:     ; $201 Keyboard port buffer.
    .res 1
key_status:     ; $202 Keyboard status: bit0 = function, bit1 = control, bit2 = shift, bit3 = alphalock,
    .res 1      ; bit6 = key pressed, bit7 = new data.
key_row:        ; $203 Keyboard row.
    .res 1
key_col:        ; $204 Keyboard column.
    .res 1
key_pressed:    ; $205 Key pressed (as found in the lookup table).
    .res 1
key_last_key:   ; $206 Last value of key pressed
    .res 1
key_data:       ; $207 Debounced key value available for users.
    .res 1
dump_number:
    .res 1      ; $208 (520) Number of bytes to dump to the screen.
acia1_rx_buffer:
    .res 256    ; $209 Reserve 256 bytes of RAM for the buffer ring itself.
acia1_rd_ptr:
    .res 1      ; $309 Buffer read pointer.
acia1_wr_ptr:
    .res 1      ; $30A Buffer write pointer.
acia1_rx_byte:
    .res 1      ; $30B Single byte receive buffer.
acia1_s_copy:
    .res 1      ; $30C ACIA status register copy.    
ls_mode:      
    .res 1      ; $30D (781) Load/Save flag. 0 = normal. 
    			; 1 = load. 2 = binary. 4 = save.
				; Top bit set if in flow control mode.
beeper:      
    .res 1      ; $30E Beeper timer 1 low value.  
c_val:    
    .res 1      ; $30F (783) C value.
x_val:    
    .res 1      ; $310 (784) X value.
y_val:
    .res 1      ; $311 (785) Y value.
a_val:   
    .res 1      ; $312 (786) A value.
b_val:
    .res 1      ; $313 (787) B value.   
char_to_send:    
    .res 1      ; $314 (788) Character to send.
char_sending:  
	.res 1      ; $315 (789) Character being sent flag.
load_count:
	.res 2 		; $316 - $317 (790 - 791) Count of loaded bytes for binary load.
int_vector1:	
	.res 2		; $318 - $319 (792 - 793) Address of first interrupt vector.
int_vector2:	
	.res 2		; $31A - $31B (793 - 794) Address of second interrupt vector.
int_a:
	.res 1		; $31C A register storage.	
int_x:
	.res 1		; $31D X register storage.	
int_y:
	.res 1		; $31E Y register storage.
int_p:
	.res 1		; $31F P register storage.

; =============================================================================
; Main code.
; =============================================================================

.segment "CODE"
;.org $8000

TOKEN_ADDRESS_TABLE:
        .word END-1
        .word FOR-1
        .word NEXT-1
        .word DATA-1
        .WORD INPUT-1        
        .word DIM-1
        .word READ-1
        .word LET-1
TOKEN_GOTO=$80+(*-TOKEN_ADDRESS_TABLE)/2
        .word GOTO-1
        .word RUN-1
        .word IF-1
        .word RESTORE-1
TOKEN_GOSUB=$80+(*-TOKEN_ADDRESS_TABLE)/2
        .word GOSUB-1
        .word POP-1
TOKEN_REM=$80+(*-TOKEN_ADDRESS_TABLE)/2
        .word REM-1
        .word STOP-1
        .word ON-1
        .word NULL-1
        .word WAIT-1
        .word SAVE-1
        .WORD LOAD-1        
        .word NORMAL-1 ; SJ Added Apr 2014.
        .WORD CLS-1 ; SJ Added Nov 2013.          
        .word MOVE-1 ; SJ Added Apr 2014. 
        .WORD PLOT-1 ; SJ Added Apr 2014. 
        .WORD LINE-1 ; SJ Added Apr 2014. 
        .WORD RECT-1 ; SJ Added Apr 2014. 
        .WORD CIRC-1 ; SJ Added Apr 2014. 
        .WORD SETCUR-1 ; SJ Added Apr 2014.
        .WORD FONT-1 ; SJ Added Apr 2014.        
        .word GET-1 ; SJ Added Apr 2014.
        .word BINARY-1 ; SJ Added Apr 2015.
        .word DEF-1
        .word POKE-1
TOKEN_PRINT=$80+(*-TOKEN_ADDRESS_TABLE)/2
        .word PRINT-1
        .word CONT-1
        .word LIST-1
        .word CLEAR-1
        .word NEW-1
TOKEN_TAB=$00+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_TO=$01+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_FN=$02+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_SPC=$03+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_THEN=$04+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_NOT=$05+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_STEP=$06+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_PLUS=$07+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_MINUS=$08+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_GREATER=$0E+$80+(*-TOKEN_ADDRESS_TABLE)/2
TOKEN_EQUAL=$0F+$80+(*-TOKEN_ADDRESS_TABLE)/2
NUM_TOKENS=(*-TOKEN_ADDRESS_TABLE)/2

UNFNC:
TOKEN_SGN=$11+$80+(*-TOKEN_ADDRESS_TABLE)/2
        .WORD SGN         
        .word INT
        .word ABS
        .word USR
        .word FRE
        .word POS
        .word SQR
        .word RND
        .word LOG
        .word EXP
        .word COS
        .word SIN
        .word TAN
        .word ATN
        .word PEEK        
        .word LEN
        .word STR
        .word VAL
        .word ASC
        .word CHRSTR       
        
TOKEN_LEFTSTR=$11+$80+(*-TOKEN_ADDRESS_TABLE)/2
        .word LEFTSTR
        .word RIGHTSTR
        .word MIDSTR

MATHTBL:
        .byte   $79
        .word   FADDT-1
        .byte   $79
        .word   FSUBT-1
        .byte   $7B
        .word   FMULTT-1
        .byte   $7B
        .word   FDIVT-1
        .byte   $7F
        .word   FPWRT-1
        .byte   $50
        .word   TAND-1
        .byte   $46
        .word   OR-1
        .byte   $7D
        .word   NEGOP-1
        .byte   $5A
        .word   EQUOP-1
        .byte   $64
        .word   RELOPS-1

TOKEN_NAME_TABLE:
        .byte "EN", $80+'D'
        .byte "FO", $80+'R'
        .byte "NEX", $80+'T'
        .byte "DAT", $80+'A'
        .byte "INPU", $80+'T'
        .byte "DI", $80+'M'
        .byte "REA", $80+'D'
        .byte "LE", $80+'T'
        .byte "GOT", $80+'O'
        .byte "RU", $80+'N'
        .byte "I", $80+'F'
        .byte "RESTOR", $80+'E'
        .byte "GOSU", $80+'B'
        .byte "RETUR", $80+'N'
        .byte "RE", $80+'M'
        .byte "STO", $80+'P'
        .byte "O", $80+'N'
        .byte "NUL", $80+'L'
        .byte "WAI", $80+'T'
        .byte "SAV", $80+'E'
        .BYTE "LOA", $80+'D' 
        .byte "NORMA", $80+'L' ; SJ Added Apr 2014. 
        .BYTE "CL", $80+'S' ; SJ Added Nov 2013. 
        .BYTE "MOV", $80+'E' ; SJ Added Apr 2014.     
        .BYTE "PLO", $80+'T' ; SJ Added Apr 2014. 
        .BYTE "LIN", $80+'E' ; SJ Added Apr 2014.
        .BYTE "REC", $80+'T' ; SJ Added Apr 2014.    
        .BYTE "CIR", $80+'C' ; SJ Added Apr 2014.         
        .BYTE "SETCU", $80+'R' ; SJ Added Apr 2014.
        .BYTE "FON", $80+'T' ; SJ Added Apr 2014. 
        .BYTE "GE", $80+'T' ; SJ Added Apr 2014. 
        .BYTE "BINAR", $80+'Y' ; SJ Added Apr 2015. 
        .byte "DE", $80+'F'
        .byte "POK", $80+'E'
        .byte "PRIN", $80+'T'
        .byte "CON", $80+'T'
        .byte "LIS", $80+'T'
        .byte "CLEA", $80+'R'
        .byte "NE", $80+'W'
        .byte "TAB", $80+'('
        .byte "T", $80+'O'
        .byte "F", $80+'N'
        .byte "SPC", $80+'('
        .byte "THE", $80+'N'
        .byte "NO", $80+'T'
        .byte "STE", $80+'P'
        .byte "", $80+'+'
        .byte "", $80+'-'
        .byte "", $80+'*'
        .byte "", $80+'/'
        .byte "", $80+'^'
        .byte "AN", $80+'D'
        .byte "O", $80+'R'
        .byte "", $80+'>'
        .byte "", $80+'='
        .byte "", $80+'<'
        .BYTE "SG", $80+'N'        
        .byte "IN", $80+'T'
        .byte "AB", $80+'S'
        .byte "US", $80+'R'
        .byte "FR", $80+'E'
        .byte "PO", $80+'S'
        .byte "SQ", $80+'R'
        .byte "RN", $80+'D'
        .byte "LO", $80+'G'
        .byte "EX", $80+'P'
        .byte "CO", $80+'S'
        .byte "SI", $80+'N'
        .byte "TA", $80+'N'
        .byte "AT", $80+'N'
        .byte "PEE", $80+'K'       
        .byte "LE", $80+'N'
        .byte "STR", $80+'$'
        .byte "VA", $80+'L'
        .byte "AS", $80+'C'
        .byte "CHR", $80+'$'
        .byte "LEFT", $80+'$'
        .byte "RIGHT", $80+'$'
        .byte "MID", $80+'$'
        .byte   0

ERROR_MESSAGES:
ERR_NOFOR := <(*-ERROR_MESSAGES)
        .byte "NF"
ERR_SYNTAX := <(*-ERROR_MESSAGES)
        .byte "SN"
ERR_NOGOSUB := <(*-ERROR_MESSAGES)
        .byte "RG"
ERR_NODATA := <(*-ERROR_MESSAGES)
        .byte "OD"
ERR_ILLQTY := <(*-ERROR_MESSAGES)
        .byte "FC"
ERR_OVERFLOW := <(*-ERROR_MESSAGES)
        .byte "OV"
ERR_MEMFULL := <(*-ERROR_MESSAGES)
        .byte "OM"
ERR_UNDEFSTAT := <(*-ERROR_MESSAGES)
        .byte "US"
ERR_BADSUBS := <(*-ERROR_MESSAGES)
        .byte "BS"
ERR_REDIMD := <(*-ERROR_MESSAGES)
        .byte "DD"
ERR_ZERODIV := <(*-ERROR_MESSAGES)
        .byte "/0"
ERR_ILLDIR := <(*-ERROR_MESSAGES)
        .byte "ID"
ERR_BADTYPE := <(*-ERROR_MESSAGES)
        .byte "TM"
ERR_STRLONG := <(*-ERROR_MESSAGES)
        .byte "LS"
ERR_FRMCPX := <(*-ERROR_MESSAGES)
        .byte "ST"
ERR_CANTCONT := <(*-ERROR_MESSAGES)
        .byte "CN"
ERR_UNDEFFN := <(*-ERROR_MESSAGES)
        .byte "UF"

; =============================================================================
; Global messages: "ERROR", "IN", "OK", "BREAK".
; =============================================================================

QT_ERROR:
        .byte   " ERROR"
        .byte   0
QT_IN:
        .byte   " IN "
        .byte   $00
QT_OK:
        .byte   CR,LF,"OK",CR,LF
        .byte    0
QT_BREAK:
        .byte CR,LF,"BREAK"
        .byte   0

; =============================================================================
; Generic stack and memory management code
; this code is identical across all versions of
; BASIC.

; CALLED BY "NEXT" AND "FOR" TO SCAN THROUGH
; THE STACK FOR A FRAME WITH THE SAME VARIABLE.
;
; (FORPNT) = ADDRESS OF VARIABLE IF "FOR" OR "NEXT"
;     = $XXFF IF CALLED FROM "RETURN"
;     <<< BUG: SHOULD BE $FFXX >>>
;
;    RETURNS .NE. IF VARIABLE NOT FOUND,
;    (X) = STACK PNTR AFTER SKIPPING ALL FRAMES
;
;    .EQ. IF FOUND
;    (X) = STACK PNTR OF FRAME FOUND
; =============================================================================

GTFORPNT:
        tsx
        inx
        inx
        inx
        inx
L2279:
        lda     STACK+1,x
        cmp     #$81
        bne     L22A1
        lda     FORPNT+1
        bne     L228E
        lda     STACK+2,x
        sta     FORPNT
        lda     STACK+3,x
        sta     FORPNT+1
L228E:
        cmp     STACK+3,x
        bne     L229A
        lda     FORPNT
        cmp     STACK+2,x
        beq     L22A1
L229A:
        txa
        clc
        adc     #BYTES_PER_FRAME
        tax
        bne     L2279
L22A1:
        rts
; =============================================================================
; MOVE BLOCK OF MEMORY UP
;
; ON ENTRY:
;    (Y,A) = (HIGHDS) = DESTINATION END+1
;    (LOWTR) = LOWEST ADDRESS OF SOURCE
;    (HIGHTR) = HIGHEST SOURCE ADDRESS+1
; =============================================================================

BLTU:
        jsr     REASON
        sta     STREND
        sty     STREND+1
BLTU2:
        sec
        lda     HIGHTR
        sbc     LOWTR
        sta     INDEX
        tay
        lda     HIGHTR+1
        sbc     LOWTR+1
        tax
        inx
        tya
        beq     L22DD
        lda     HIGHTR
        sec
        sbc     INDEX
        sta     HIGHTR
        bcs     L22C6
        dec     HIGHTR+1
        sec
L22C6:
        lda     HIGHDS
        sbc     INDEX
        sta     HIGHDS
        bcs     L22D6
        dec     HIGHDS+1
        bcc     L22D6
L22D2:
        lda     (HIGHTR),y
        sta     (HIGHDS),y
L22D6:
        dey
        bne     L22D2
        lda     (HIGHTR),y
        sta     (HIGHDS),y
L22DD:
        dec     HIGHTR+1
        dec     HIGHDS+1
        dex
        bne     L22D6
        rts

; =============================================================================
; CHECK IF ENOUGH ROOM LEFT ON STACK
; FOR "FOR", "GOSUB", OR EXPRESSION EVALUATION
; =============================================================================

CHKMEM:
        asl     a
        adc     #SPACE_FOR_GOSUB
        bcs     MEMERR
        sta     INDEX
        tsx
        cpx     INDEX
        bcc     MEMERR
        rts
; =============================================================================
; CHECK IF ENOUGH ROOM BETWEEN ARRAYS AND STRINGS
; (Y,A) = ADDR ARRAYS NEED TO GROW TO
; =============================================================================

REASON:
        cpy     FRETOP+1
        bcc     L231E
        bne     L22FC
        cmp     FRETOP
        bcc     L231E
L22FC:
        pha
        ldx     #FAC-TEMP1-1
        tya
L2300:
        pha
        lda     TEMP1,x
        dex
        bpl     L2300
        jsr     GARBAG
        ldx     #TEMP1-FAC+1
L230B:
        pla
        sta     FAC,x
        inx
        bmi     L230B
        pla
        tay
        pla
        cpy     FRETOP+1
        bcc     L231E
        bne     MEMERR
        cmp     FRETOP
        bcs     MEMERR
L231E:
        rts
MEMERR:
        ldx     #ERR_MEMFULL

; =============================================================================
; HANDLE AN ERROR
;
; (X)=OFFSET IN ERROR MESSAGE TABLE
; (ERRFLG) > 128 IF "ON ERR" TURNED ON
; (CURLIN+1) = $FF IF IN DIRECT MODE
; =============================================================================
ERROR:
        lsr     Z14
        jsr     CRDO
        jsr     OUTQUES
        lda     ERROR_MESSAGES,x
        jsr     OUTDO
        lda     ERROR_MESSAGES+1,x
        jsr     OUTDO
        jsr     STKINI
        lda     #<QT_ERROR
        ldy     #>QT_ERROR

; =============================================================================
; PRINT STRING AT (Y,A)
; PRINT CURRENT LINE # UNLESS IN DIRECT MODE
; FALL INTO WARM RESTART
; =============================================================================

PRINT_ERROR_LINNUM:
        jsr     STROUT
        ldy     CURLIN+1
        iny
        beq     RESTART
        jsr     INPRT

; =============================================================================
; WARM RESTART ENTRY
; =============================================================================

RESTART:

        LSR     Z14
        LDA     #<QT_OK
        LDY     #>QT_OK
        JSR     GOSTROUT
        
                                ; Start the ACDs free running.
        LDA     #$00            ; Set CA2 to input. 
        STA     via2_pcr 
            
L2351:                  ; The code loops here. Lines are read and acted
                        ; on then we end up back here again.                        
        
        LDA     #%00000010       ; If we are in binary mode bit 2 is set.
        AND     ls_mode            
    	BEQ     RESTART_INLIN    ; Get binary input from the serial port.       
                
        JSR     MONISCNTC       ; Check if control C is pressed.
        BCS     RESTART_CTRL_C  ; Yes, so we go out of load mode.                                      
        
		JSR     ACIA_Buffer_Get ; Else get the next character.
        BCC     L2351           ; If there isn't one just keep looping.    
		tax						; Store it in X.
		
								; Store the byte in memory.
		lda		load_count		; Get the count.
		tay						; Store the count in Y.	
		txa						; Get back the value.	
		sta		(load_address), y				
						
								; Increment the counter by one.
        inc 	load_count  	; Increment the LSB.
        lda     load_count
        sta     IO
        bne     L2351           ; If the result was not zero we're done.
        inc 	load_count + 1  ; Increment the MSB if LSB wrapped round.
		inc		load_address + 1; Increment the high byte of the address we're storing data into. 
        jmp     L2351
        
RESTART_CTRL_C:       
        JSR     NORMAL    		; Normal mode.
        jmp     L2351
RESTART_INLIN:        
        JSR     INLIN
        STX     TXTPTR
        STY     TXTPTR+1
        JSR     CHRGET

; bug in pre-1.1: CHRGET sets Z on '\0'
; and ':' - a line starting with ':' in
; direct mode gets ignored

        TAX
        BEQ     L2351
        PHA
        LDA     #%00000001      ; If we are in load mode bit 1 is set.
    	AND     ls_mode            
    	beq     RESTART_normal  ; Normal mode.
        jsr     Flow_Off ; While processing a line stop receiving to avoid
                         ; overflowing the buffer.
 RESTART_normal:       
        PLA
        ldx     #$FF
        STX     CURLIN+1
        bcc     NUMBERED_LINE
        jsr     PARSE_INPUT_LINE
        
        PHA
        LDA     #%00000001      ; If we are in load mode bit 1 is set.
    	AND     ls_mode 
        beq     RESTART_done  ; Normal mode.        
        jsr     Flow_On  ; Start receiving again.
        
RESTART_done:
        PLA
        jmp     NEWSTT2

; =============================================================================
; HANDLE NUMBERED LINE
; =============================================================================

NUMBERED_LINE:

        JSR     LINGET              ; Get the line number.
        JSR     PARSE_INPUT_LINE    ; Parse the line.
        STY     EOLPNTR
        JSR     FNDLIN              ; Check if the line exists already?
        BCC     PUT_NEW_LINE        ; It doesn't so branch and add it.

        LDY     #$01                ; It exists already so modify it.
        LDA     (LOWTR),y
        STA     INDEX+1
        LDA     VARTAB
        STA     INDEX
        LDA     LOWTR+1
        STA     DEST+1
        LDA     LOWTR
        DEY
        SBC     (LOWTR),y
        CLC
        ADC     VARTAB
        STA     VARTAB
        STA     DEST
        LDA     VARTAB+1
        ADC     #$FF
        STA     VARTAB+1
        SBC     LOWTR+1
        TAX
        SEC
        LDA     LOWTR
        SBC     VARTAB
        TAY
        BCS     L23A5
        INX
        DEC     DEST+1
L23A5:
        CLC
        ADC     INDEX
        BCC     L23AD
        DEC     INDEX+1
        CLC
L23AD:
        LDA     (INDEX),y
        STA     (DEST),y
        INY
        BNE     L23AD
        INC     INDEX+1
        INC     DEST+1
        DEX
        BNE     L23AD

; =============================================================================

PUT_NEW_LINE:

        LDA     INPUTBUFFER
        BEQ     FIX_LINKS
        LDA     MEMSIZ
        LDY     MEMSIZ+1
        STA     FRETOP
        STY     FRETOP+1
        LDA     VARTAB
        STA     HIGHTR
        ADC     EOLPNTR
        STA     HIGHDS
        LDY     VARTAB+1
        STY     HIGHTR+1
        BCC     L23D6
        INY
L23D6:
        STY     HIGHDS+1
        JSR     BLTU

        LDA     STREND
        LDY     STREND+1
        STA     VARTAB
        STY     VARTAB+1
        LDY     EOLPNTR
        DEY

; =============================================================================
; COPY LINE INTO PROGRAM
; =============================================================================

L23E6:
        LDA     INPUTBUFFER-4,y
        STA     (LOWTR),y
        DEY
        BPL     L23E6

; =============================================================================
; CLEAR ALL VARIABLES
; RE-ESTABLISH ALL FORWARD LINKS
; =============================================================================

FIX_LINKS:
        JSR     SETPTRS
        LDA     TXTTAB
        LDY     TXTTAB+1
        STA     INDEX
        STY     INDEX+1
        CLC
L23FA:
        LDY     #$01
        LDA     (INDEX),y
        jeq     L2351

        LDY     #$04
L2405:
        INY
        LDA     (INDEX),y
        BNE     L2405
        INY
        TYA
        ADC     INDEX
        TAX
        LDY     #$00
        STA     (INDEX),y
        LDA     INDEX+1
        ADC     #$00
        INY
        STA     (INDEX),y
        STX     INDEX
        STA     INDEX+1
        BCC     L23FA    ; always

; =============================================================================

L2420:
        JSR     OUTDO
        DEX
        BPL     INLIN2
L2423:
        JSR     OUTDO
        jsr     CRDO

; =============================================================================
; READ A LINE, AND STRIP OFF SIGN BITS
; =============================================================================

INLIN:
        ldx     #$00  
INLIN2:        
        JSR     GETLN
        cmp     #$07    ; BEL character.
        beq     L2443
        CMP     #$08    ; Backspace (back arrow) character.
        BEQ     L2420
        CMP     #$0D    ; CR character.
        beq     L2453
        cmp     #$20    ; Space character.
        bcc     INLIN2
        cmp     #$7D    ; } character.
        bcs     INLIN2
        cmp     #$40    ; @ character.
        beq     L2423
        cmp     #$5F    ; _ character.
        BEQ     L2420

L2443:           
        CPX     #$47    ; Check the line length.
        BCS     L244C

        STA     INPUTBUFFER,x   ; Store in our input buffer.
        INX
cont:        
        .BYTE   $2C ; Skip the next instruction.

L244C:
        lda     #$07     ; BEL character. We go DING! if the line is full.
        jsr     OUTDO
        bne     INLIN2
L2453:  
        JMP     L29B9
        
; =============================================================================
; Get a character.
; =============================================================================

GETLN: 			
        LDA     #%00000001      ; If we are in load mode bit 1 is set.
    	AND     ls_mode            
    	BNE     GETLN_serial    ; Get input from the serial port.        
        
        JSR     MONRDKEY        ; Else we get input from the keyboard.            
        JMP     GETLN_key       ; Then process normally.        
        
GETLN_CTRL_C:
        JSR     NORMAL    		; Normal mode.        
        JMP     GETLN_key       ; Then process normally.       
        
GETLN_serial:    		                       
        JSR     MONISCNTC       ; Check if control C is pressed.
        BCS     GETLN_CTRL_C    ; Yes, so we go out of load mode.                                      
        JSR     ACIA_Buffer_Get ; Else get the next character.                               
        CMP     #3              ; Check if control C was sent.    
        BEQ     GETLN_CTRL_C    ; Yes, so we go out of load/save mode.    
                         
GETLN_key:                      ; We got a character.
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        and     #$7F
RDKEY:
        cmp     #$0F
        bne     L2465
        pha
        lda     Z14
        eor     #$FF
        sta     Z14
        PLA
L2465:
        rts
; =============================================================================
; TOKENIZE THE INPUT LINE
; =============================================================================

PARSE_INPUT_LINE:

        LDX     TXTPTR          ; Index into unparsed line.        
        ldy     #$04			; Index into output line.
        sty     DATAFLG			; Set up the dataflag.
L246C:

        LDA     INPUTBUFFERX,x	; Get next input character.
        cmp     #$20            ; Ignore spaces.
        beq     L24AC
        sta     ENDCHR
        cmp     #$22            ; Start of quote marks.
        beq     L24D0
        bit     DATAFLG         ; In a DATA statement?
        bvs     L24AC
        cmp     #$3F            ; Print '?' character.
        bne     L2484
        lda     #TOKEN_PRINT
        bne     L24AC
L2484:
        cmp     #$30            ; Is it a digit?
        bcc     L248C
        cmp     #$3C            ; Or punctuation?
        bcc     L24AC

; =============================================================================
; SEARCH TOKEN NAME TABLE FOR MATCH STARTING
; WITH CURRENT CHAR FROM INPUT LINE
; SJ April 2014: Tweaked with code from Applesoft to use FAC to index
; token name table rather to get past 256 byte limit.
; =============================================================================

L248C:
        sty     STRNG2          	; Save index into output line.      
        
        LDA   	#<TOKEN_NAME_TABLE  	; Get ADL of table.        
        STA   	FAC              		; and put it in FAC.
        LDA   	#>TOKEN_NAME_TABLE  	; Get ADH of table        		
        			            		; and put it in FAC+1.        
        SEC
        SBC		#$01					; But take back one to account for the inc
        STA   	FAC+1					; that happens later.
        								
        ldy     #$00				; Y used to index table.
        sty     EOLPNTR				; Current token.	
        dey							; Dec so we start at 0 after INY below.
        stx     TXTPTR				; Save position in input line.
        DEX                
L2496:
        INY                     	; Y is the index into the token table.
        BNE		L2497
        inc		FAC+1				; Next page in table.					
L2497:
        inx                     	; X is the index into the line.
L2498:
        lda     INPUTBUFFERX,x  	; Get next character.
        cmp     #$20            	; Ignore spaces.
        beq     L2497

        SEC							; Compare.
        sbc     (FAC),y
        beq     L2496
        cmp     #$80
        bne     L24D7
        ora     EOLPNTR         	; Token matched.

; =============================================================================
; STORE CHARACTER OR TOKEN IN OUTPUT LINE
; =============================================================================

L24AA:
        ldy     STRNG2          ; Get index to output line.
L24AC:
        inx
        iny
        sta     INPUTBUFFER-5,y ; Store character or token.
        lda     INPUTBUFFER-5,y ; Look for end of line or statement.
        beq     L24EA
        sec
        sbc     #$3A            ; Look for :
        beq     L24BF
        cmp     #$49
        bne     L24C1
L24BF:
        sta     DATAFLG
L24C1:
        sec
        sbc     #TOKEN_REM-':'
        bne     L246C
        sta     ENDCHR

; =============================================================================
; HANDLE LITERAL (BETWEEN QUOTES) OR REMARK,
; BY COPYING CHARS UP TO ENDCHR.
; =============================================================================

L24C8:
        lda     INPUTBUFFERX,x
        beq     L24AC
        cmp     ENDCHR
        beq     L24AC
L24D0:
        iny
        sta     INPUTBUFFER-5,y
        inx
        bne     L24C8

; =============================================================================
; ADVANCE POINTER TO NEXT TOKEN NAME
; =============================================================================

L24D7:
        ldx     TXTPTR
        inc     EOLPNTR
L24DB:
        LDA     (FAC),y
        INY
        BNE		L24DB_a
        INC		FAC+1
 L24DB_a:
 		ASL
 		BCC		L24DB
 		LDA     (FAC),y       
        bne     L2498
        lda     INPUTBUFFERX,x
        bpl     L24AA
; ---END OF LINE------------------
L24EA:
        STA     INPUTBUFFER-3,y
        lda     #<INPUTBUFFER-1
        sta     TXTPTR
        rts

; =============================================================================
; SEARCH FOR LINE
;
; (LINNUM) = LINE # TO FIND
; IF NOT FOUND:  CARRY = 0
;    LOWTR POINTS AT NEXT LINE
; IF FOUND:      CARRY = 1
;    LOWTR POINTS AT LINE
; =============================================================================

FNDLIN:
        lda     TXTTAB
        LDX     TXTTAB+1

FL1:
        ldy     #$01
        sta     LOWTR
        stx     LOWTR+1
        lda     (LOWTR),y   ; End of program. Line not found.
        beq     L251F
        iny                 ; Increment.
        iny                 ; Increment.
        lda     LINNUM+1    ; Compare line number lower byte.
        cmp     (LOWTR),y
        bcc     L2520       ; Not found.
        beq     L250D       ; Matches.
        dey
        bne     L2516
L250D:
        lda     LINNUM      ; Check upper byte.
        dey
        cmp     (LOWTR),y
        bcc     L2520       ; Branch is value less than.
        beq     L2520
L2516:
        dey
        lda     (LOWTR),y
        tax
        dey
        lda     (LOWTR),y
        bcs     FL1
L251F:
        clc                 ; Line not found.
L2520:
        rts

; =============================================================================
; "NEW" STATEMENT
; =============================================================================

NEW:
        bne     L2520
SCRTCH:

        lda     #$00
        tay

        STA     (TXTTAB),y
        iny
        sta     (TXTTAB),y
        lda     TXTTAB
        adc     #$02
        sta     VARTAB
        lda     TXTTAB+1
        adc     #$00
        sta     VARTAB+1

; =============================================================================
SETPTRS:
        jsr     STXTPT

; =============================================================================
; "CLEAR" STATEMENT
; =============================================================================

CLEARC:
        lda     MEMSIZ
        ldy     MEMSIZ+1
        sta     FRETOP
        sty     FRETOP+1
        lda     VARTAB
        ldy     VARTAB+1
        sta     ARYTAB
        sty     ARYTAB+1
        sta     STREND
        sty     STREND+1
        jsr     RESTORE

; =============================================================================

STKINI:
        ldx     #TEMPST
        stx     TEMPPT
        pla
        sta     STACK+STACK_TOP+1
        pla
        sta     STACK+STACK_TOP+2
        ldx     #STACK_TOP
        txs
        lda     #$00
        sta     OLDTEXT+1
        sta     SUBFLG
L256A:
        rts

; =============================================================================
; SET TXTPTR TO BEGINNING OF PROGRAM
; =============================================================================

STXTPT:
        clc
        lda     TXTTAB
        adc     #$FF
        sta     TXTPTR
        lda     TXTTAB+1
        adc     #$FF
        sta     TXTPTR+1
        rts

; =============================================================================
; "LIST" STATEMENT
; =============================================================================

LIST:
        BCC     L2581
        beq     L2581
        cmp     #TOKEN_MINUS
        bne     L256A
L2581:
        JSR     LINGET
        jsr     FNDLIN
        jsr     CHRGOT
        beq     L2598
        cmp     #TOKEN_MINUS
        bne     L2520
        jsr     CHRGET
        jsr     LINGET
        bne     L2520
L2598:
        pla
        pla
        lda     LINNUM
        ora     LINNUM+1
        bne     L25A6
        lda     #$FF
        sta     LINNUM
        sta     LINNUM+1
L25A6:
        ldy     #$01
        sty     DATAFLG
        lda     (LOWTRX),y
        beq     L25E5
        jsr     ISCNTC
        jsr     CRDO
        iny
        lda     (LOWTRX),y
        tax
        iny
        lda     (LOWTRX),y
        cmp     LINNUM+1
        bne     L25C1
        cpx     LINNUM
        beq     L25C3
L25C1:
        bcs     L25E5
; ---LIST ONE LINE----------------
L25C3:
        sty     FORPNT
        jsr     LINPRT
        lda     #$20
L25CA:
        ldy     FORPNT
        and     #$7F
L25CE:
        jsr     OUTDO
        cmp     #$22
        bne     LA519
        lda     DATAFLG
        eor     #$FF
        sta     DATAFLG
LA519:
        iny
        lda     (LOWTRX),y
        bne     L25E8
        tay
        lda     (LOWTRX),y
        tax
        iny
        lda     (LOWTRX),y
        stx     LOWTRX
        sta     LOWTRX+1
        bne     L25A6
L25E5:
        jmp     RESTART
L25E8:
        bpl     L25CE
        cmp     #$FF
        beq     L25CE
        bit     DATAFLG
        bmi     L25CE
        sec
        sbc     #$7F
        tax
        sty     FORPNT
        
        ; SJ April 2014. Modified this to handle 
        ; table greater than 256 bytes.
        				
        LDA   	#<TOKEN_NAME_TABLE  	; Get ADL of table.        
        STA   	FAC              		; and put it in FAC.
        LDA   	#>TOKEN_NAME_TABLE  	; Get ADH of table        		
        			            		; and put it in FAC+1.        
        SEC
        SBC		#$01					; But take back one to account for the inc
        STA   	FAC+1					; that happens later.
        LDY     #$FF					; Y starts 1 less than 0.
        
L25F2:
        dex
        beq     L25FD
L25F5:
        iny								; Increment Y.
        ;lda     TOKEN_NAME_TABLE,y
        BNE		L25F5_a					; Check if we rolled over.
        INC		FAC+1					; Next page in table.
L25F5_a:
		LDA 	(FAC), Y        		; Get the value.
        bpl     L25F5
        bmi     L25F2
L25FD:
        iny
        ; lda     TOKEN_NAME_TABLE,y
        BNE		L25FD_a					; Check if we rolled over.
        INC		FAC+1					; Next page in table.
L25FD_a:
		LDA 	(FAC), Y        		; Get the value.
        bmi     L25CA
        jsr     OUTDO
        bne     L25FD    ; always

; =============================================================================
; "FOR" STATEMENT
;
; FOR PUSHES 18 BYTES ON THE STACK:
; 2 -- TXTPTR
; 2 -- LINE NUMBER
; 5 -- INITIAL (CURRENT)  FOR VARIABLE VALUE
; 1 -- STEP SIGN
; 5 -- STEP VALUE
; 2 -- ADDRESS OF FOR VARIABLE IN VARTAB
; 1 -- FOR TOKEN ($81)
; =============================================================================

FOR:
        lda     #$80
        sta     SUBFLG
        jsr     LET
        jsr     GTFORPNT
        bne     L2619
        txa
        adc     #FOR_STACK1
        tax
        txs
L2619:
        pla
        pla
        lda     #FOR_STACK2
        jsr     CHKMEM
        jsr     DATAN
        clc
        tya
        adc     TXTPTR
        pha
        lda     TXTPTR+1
        adc     #$00
        pha
        lda     CURLIN+1
        pha
        lda     CURLIN
        pha
        lda     #TOKEN_TO
        jsr     SYNCHR
        jsr     CHKNUM
        jsr     FRMNUM
        lda     FACSIGN
        ora     #$7F
        and     FAC+1
        sta     FAC+1
        lda     #<STEP
        ldy     #>STEP
        sta     INDEX
        sty     INDEX+1
        jmp     FRM_STACK3

; =============================================================================
; "STEP" PHRASE OF "FOR" STATEMENT
; =============================================================================

STEP:
        lda     #<CON_ONE
        ldy     #>CON_ONE
        jsr     LOAD_FAC_FROM_YA
        jsr     CHRGOT
        cmp     #TOKEN_STEP
        bne     L2665
        jsr     CHRGET
        jsr     FRMNUM
L2665:
        jsr     SIGN
        jsr     FRM_STACK2
        lda     FORPNT+1
        pha
        lda     FORPNT
        pha
        lda     #$81
        pha

; =============================================================================
; PERFORM NEXT STATEMENT
; =============================================================================

NEWSTT:
        jsr     ISCNTC
        lda     TXTPTR
        ldy     TXTPTR+1
        beq     L2683
        sta     OLDTEXT
        sty     OLDTEXT+1
        ldy     #$00
L2683:
        lda     (TXTPTR),y
        beq     LA5DC           ; old: 1 cycle more on generic case
        cmp     #$3A
        beq     NEWSTT2
SYNERR1:
        jmp     SYNERR
LA5DC:
        ldy     #$02
        lda     (TXTPTR),y
        clc
        beq     L2701
        iny
        lda     (TXTPTR),y
        sta     CURLIN
        iny
        lda     (TXTPTR),y
        sta     CURLIN+1
        tya
        adc     TXTPTR
        sta     TXTPTR
        bcc     NEWSTT2
        inc     TXTPTR+1
NEWSTT2:
        jsr     CHRGET
        jsr     EXECUTE_STATEMENT
        jmp     NEWSTT

; =============================================================================
; EXECUTE A STATEMENT
;
; (A) IS FIRST CHAR OF STATEMENT
; CARRY IS SET
; =============================================================================

EXECUTE_STATEMENT:
        beq     RET1
        sec
EXECUTE_STATEMENT1:
        sbc     #$80
        jcc     LET                     ; old: 1 cycle more on instr.

        cmp     #NUM_TOKENS
        bcs     SYNERR1
        asl     a
        tay
        lda     TOKEN_ADDRESS_TABLE+1,y
        pha
        lda     TOKEN_ADDRESS_TABLE,y
        pha
        jmp     CHRGET

; =============================================================================
; "RESTORE" STATEMENT
; =============================================================================

RESTORE:
        sec
        lda     TXTTAB
        sbc     #$01
        ldy     TXTTAB+1
        bcs     SETDA
        dey
SETDA:
        sta     DATPTR
        sty     DATPTR+1
        rts

; =============================================================================
; SEE IF CONTROL-C TYPED
; =============================================================================

ISCNTC:
    JSR    MONISCNTC
    BCS    STOP
    RTS

; =============================================================================
; "STOP" STATEMENT
; =============================================================================

STOP:
        bcs     END2

; =============================================================================
; "END" STATEMENT
; =============================================================================

END:
        clc
END2:
        bne     RET1
        lda     TXTPTR
        ldy     TXTPTR+1
        beq     END4
        sta     OLDTEXT
        sty     OLDTEXT+1
CONTROL_C_TYPED:
        lda     CURLIN
        ldy     CURLIN+1
        sta     OLDLIN
        sty     OLDLIN+1
END4:
        pla
        pla
L2701:
        lda     #<QT_BREAK
        ldy     #>QT_BREAK
        ldx     #$00
        stx     Z14
        bcc     L270E
        jmp     PRINT_ERROR_LINNUM
L270E:
        jmp     RESTART

; =============================================================================
; "CONT" COMMAND
; =============================================================================

CONT:
        bne     RET1
        ldx     #ERR_CANTCONT
        ldy     OLDTEXT+1
        bne     L271C
        jmp     ERROR
L271C:
        lda     OLDTEXT
        sta     TXTPTR
        sty     TXTPTR+1
        lda     OLDLIN
        ldy     OLDLIN+1
        sta     CURLIN
        sty     CURLIN+1
RET1:
        rts

; =============================================================================
; "NULL" COMMAND
; =============================================================================

NULL:
        jsr     GETBYT
        bne     RET1
        inx
        cpx     #NULL_MAX
        bcs     L2739
        dex
        stx     Z15
        rts
L2739:
        jmp     IQERR
CLEAR:
        bne     RET1
        jmp     CLEARC

; =============================================================================
; "RUN" COMMAND
; =============================================================================

RUN:
        bne     L27CF
        jmp     SETPTRS
L27CF:
        jsr     CLEARC
        jmp     L27E9

; =============================================================================
; "GOSUB" STATEMENT
;
; LEAVES 7 BYTES ON STACK:
; 2 -- RETURN ADDRESS (NEWSTT)
; 2 -- TXTPTR
; 2 -- LINE #
; 1 -- GOSUB TOKEN
; =============================================================================

GOSUB:
        lda     #$03
        jsr     CHKMEM
        lda     TXTPTR+1
        pha
        lda     TXTPTR
        pha
        lda     CURLIN+1
        pha
        lda     CURLIN
        pha
        lda     #TOKEN_GOSUB
        pha
L27E9:
        jsr     CHRGOT
        jsr     GOTO
        jmp     NEWSTT

; =============================================================================
; "GOTO" STATEMENT
; ALSO USED BY "RUN" AND "GOSUB"
; =============================================================================

GOTO:
        jsr     LINGET
        jsr     REMN
        lda     CURLIN+1
        cmp     LINNUM+1
        bcs     L2809
        tya
        sec
        adc     TXTPTR
        ldx     TXTPTR+1
        bcc     L280D
        inx
        bcs     L280D
L2809:
        lda     TXTTAB
        ldx     TXTTAB+1
L280D:
        jsr     FL1
        bcc     UNDERR
        lda     LOWTRX
        sbc     #$01
        sta     TXTPTR
        lda     LOWTRX+1
        sbc     #$00
        sta     TXTPTR+1
L281E:
        rts

; =============================================================================
; "POP" AND "RETURN" STATEMENTS
; =============================================================================

POP:
        bne     L281E
        lda     #$FF
        sta     FORPNT
        jsr     GTFORPNT
        txs
        cmp     #TOKEN_GOSUB
        beq     RETURN
        ldx     #ERR_NOGOSUB
        .byte   $2C
UNDERR:
        ldx     #ERR_UNDEFSTAT
        jmp     ERROR

; =============================================================================

SYNERR2:
        jmp     SYNERR

; =============================================================================

RETURN:
        pla
        pla
        sta     CURLIN
        pla
        sta     CURLIN+1
        pla
        sta     TXTPTR
        pla
        sta     TXTPTR+1

; =============================================================================
; "DATA" STATEMENT
; EXECUTED BY SKIPPING TO NEXT COLON OR EOL
; =============================================================================

DATA:
        jsr     DATAN

; =============================================================================
; ADD (Y) TO TXTPTR
; =============================================================================

ADDON:
        tya
        clc
        adc     TXTPTR
        sta     TXTPTR
        bcc     L2852
        inc     TXTPTR+1
L2852:
        rts

; =============================================================================
; SCAN AHEAD TO NEXT ":" OR EOL
; =============================================================================

DATAN:
        ldx     #$3A
        .byte   $2C
REMN:
        ldx     #$00
        stx     CHARAC
        ldy     #$00
        sty     ENDCHR
L285E:
        lda     ENDCHR
        ldx     CHARAC
        sta     CHARAC
        stx     ENDCHR
L2866:
        lda     (TXTPTR),y
        beq     L2852
        cmp     ENDCHR
        beq     L2852
        iny
        cmp     #$22
        beq     L285E           ; old: swap & cont is faster
        bne     L2866

; =============================================================================
; "IF" STATEMENT
; =============================================================================

IF:
        jsr     FRMEVL
        jsr     CHRGOT
        cmp     #TOKEN_GOTO
        beq     L2884
        lda     #TOKEN_THEN
        jsr     SYNCHR
L2884:
        lda     FAC
        bne     L288D

; =============================================================================
; "REM" STATEMENT, OR FALSE "IF" STATEMENT
; =============================================================================

REM:
        jsr     REMN
        beq     ADDON
L288D:
        jsr     CHRGOT
        bcs     L2895
        jmp     GOTO
L2895:
        jmp     EXECUTE_STATEMENT

; =============================================================================
; "ON" STATEMENT
;
; ON <EXP> GOTO <LIST>
; ON <EXP> GOSUB <LIST>
; =============================================================================

ON:
        jsr     GETBYT
        pha
        cmp     #TOKEN_GOSUB
        beq     L28A4
L28A0:
        cmp     #TOKEN_GOTO
        bne     SYNERR2
L28A4:
        dec     FAC_LAST
        bne     L28AC
        pla
        jmp     EXECUTE_STATEMENT1
L28AC:
        jsr     CHRGET
        jsr     LINGET
        cmp     #$2C
        beq     L28A4
        pla
L28B7:
        rts

; =============================================================================
; CONVERT LINE NUMBER
; =============================================================================

LINGET:

        ldx     #$00
        stx     LINNUM
        STX     LINNUM+1

L28BE:

    bcs     L28B7
        sbc     #$2F
        STA     CHARAC
        lda     LINNUM+1
        sta     INDEX
        cmp     #$19
        bcs     L28A0
        
; <<<<<DANGEROUS CODE>>>>>
; NOTE THAT IF (A) = $AB ON THE LINE ABOVE,
; ON.1 WILL COMPARE = AND CAUSE A CATASTROPHIC
; JUMP TO $22D9 (FOR GOTO), OR OTHER LOCATIONS
; FOR OTHER CALLS TO LINGET.
;
; YOU CAN SEE THIS IS YOU FIRST PUT "BRK" IN $22D9,
; THEN TYPE "GO TO 437761".
;
; ANY VALUE FROM 437760 THROUGH 440319 WILL CAUSE
; THE PROBLEM.  ($AB00 - $ABFF)
; <<<<<DANGEROUS CODE>>>>>

        LDA     LINNUM
        asl     a
        rol     INDEX
        asl     a
        rol     INDEX
        adc     LINNUM
        sta     LINNUM
        lda     INDEX
        adc     LINNUM+1
        sta     LINNUM+1
        asl     LINNUM
        rol     LINNUM+1
        lda     LINNUM
        adc     CHARAC
        sta     LINNUM
        bcc     L28EC
        INC     LINNUM+1


L28EC:
        jsr     CHRGET
        jmp     L28BE

; =============================================================================
; "LET" STATEMENT
;
; LET <VAR> = <EXP>
; <VAR> = <EXP>
; =============================================================================

LET:
        jsr     PTRGET
        sta     FORPNT
        sty     FORPNT+1
        lda     #TOKEN_EQUAL
        jsr     SYNCHR
        lda     VALTYP
        pha
        jsr     FRMEVL
        pla
        rol     a
        jsr     CHKVAL
        bne     LETSTRING

; =============================================================================
; REAL VARIABLE = EXPRESSION
; =============================================================================

        jmp     SETFOR

LETSTRING:

; =============================================================================
; INSTALL STRING, DESCRIPTOR ADDRESS IS AT FAC+3,4
; =============================================================================

        ldy     #$02
        lda     (FAC_LAST-1),y
        cmp     FRETOP+1
        bcc     L2946
        bne     L2938
        dey
        lda     (FAC_LAST-1),y
        cmp     FRETOP
        bcc     L2946
L2938:
        ldy     FAC_LAST
        cpy     VARTAB+1
        bcc     L2946
        bne     L294D
        lda     FAC_LAST-1
        cmp     VARTAB
        bcs     L294D
L2946:
        lda     FAC_LAST-1
        ldy     FAC_LAST
        jmp     L2963
L294D:
        ldy     #$00
        lda     (FAC_LAST-1),y
        jsr     STRINI
        lda     DSCPTR
        ldy     DSCPTR+1
        sta     STRNG1
        sty     STRNG1+1
        jsr     MOVINS
        lda     #FAC
        ldy     #$00
L2963:
        sta     DSCPTR
        sty     DSCPTR+1
        jsr     FRETMS
        ldy     #$00
        lda     (DSCPTR),y
        sta     (FORPNT),y
        iny
        lda     (DSCPTR),y
        sta     (FORPNT),y
        iny
        lda     (DSCPTR),y
        sta     (FORPNT),y
        rts
PRSTRING:
        jsr     STRPRT
L297E:
        jsr     CHRGOT

; =============================================================================
; "PRINT" STATEMENT
; =============================================================================

PRINT:
        beq     CRDO
PRINT2:
        beq     L29DD
        cmp     #TOKEN_TAB
        beq     L29F5
        cmp     #TOKEN_SPC
        beq     L29F5
        cmp     #','
        beq     L29DE
        cmp     #$3B
        beq     L2A0D
        jsr     FRMEVL
        bit     VALTYP
        bmi     PRSTRING
        jsr     FOUT
        jsr     STRLIT
        ldy     #$00
        lda     (FAC_LAST-1),y
        clc
        adc     POSX
        cmp     Z17
        bcc     L29B1
        jsr     CRDO
L29B1:
        jsr     STRPRT
        jsr     OUTSP
        bne     L297E ; branch always
L29B9:
        ldy     #$00
        STY     INPUTBUFFER,x
        LDX     #LINNUM+1

CRDO:
        lda     #CRLF_1
        sta     POSX
        jsr     OUTDO
        lda     #CRLF_2
        jsr     OUTDO
        
PRINTNULLS:
        txa
        pha
        ldx     Z15
        beq     L29D9
        lda     #$00
L29D3:
        jsr     OUTDO
        dex
        bne     L29D3
L29D9:
        stx     POSX
        pla
        tax
L29DD:
        rts
L29DE:
        lda     POSX
        cmp     Z18
        bcc     L29EA
        jsr     CRDO
        jmp     L2A0D
L29EA:
        sec
L29EB:
        sbc     #$0E
        bcs     L29EB
        eor     #$FF
        adc     #$01
        bne     L2A08
L29F5:
        pha
        jsr     GTBYTC
        cmp     #')'
        bne     SYNERR4
        pla
        cmp     #TOKEN_TAB
        bne     L2A0A
        txa
        sbc     POSX
        bcc     L2A0D
        beq     L2A0D
L2A08:
        tax
L2A0A:
        jsr     OUTSP
        dex
        bne     L2A0A
L2A0D:
        jsr     CHRGET
        jmp     PRINT2

; =============================================================================
; PRINT STRING AT (Y,A)
; =============================================================================

STROUT:
        jsr     STRLIT

; =============================================================================
; PRINT STRING AT (FACMO,FACLO)
; =============================================================================

STRPRT:
        jsr     FREFAC
        tax
        ldy     #$00
        inx
L2A22:
        dex
        beq     L29DD
        lda     (INDEX),y
        jsr     OUTDO
        iny
        cmp     #$0D
        bne     L2A22
        jsr     PRINTNULLS
        jmp     L2A22
; =============================================================================

OUTSP:
        lda     #$20
        .byte   $2C
OUTQUES:
        lda     #$3F

; =============================================================================
; PRINT CHAR FROM (A)
; =============================================================================

OUTDO:
        bit     Z14
        bmi     L2A56
; Commodore forgot to remove this in CBM1
        pha
        cmp     #$07    ; Bell.
        beq     OUTDO_bell
OUTDO_1:       
        cmp     #$20
        BCC     L2A4E
        LDA     POSX
        cmp     Z17    ; Screen width.
        BNE     L2A4C        
        jsr     CRDO
L2A4C:
        inc     POSX
L2A4E:
; Commodore forgot to remove this in CBM1
        PLA
        jsr     MONCOUT
        nop
        nop
        nop
        nop
L2A56:
        and     #$FF
        rts
OUTDO_bell:
        jsr     Beep1
        jmp     OUTDO_1


; =============================================================================
; ???
; INPUT CONVERSION ERROR:  ILLEGAL CHARACTER
; IN NUMERIC FIELD.  MUST DISTINGUISH
; BETWEEN INPUT, READ, AND GET
; =============================================================================

INPUTERR:
        lda     INPUTFLG
        beq     RESPERR     ; INPUT
                            ; without this, it treats GET errors
                            ; like READ errors
                            
		BMI     L2A63		; READ
		LDY     #$FF		; GET
		BNE     L2A67


L2A63:                            
        lda     Z8C
        LDY     Z8C+1
L2A67:        
        sta     CURLIN
        sty     CURLIN+1
SYNERR4:
        jmp     SYNERR
RESPERR:
        lda     #<ERRREENTRY
        ldy     #>ERRREENTRY
        jsr     STROUT
        lda     OLDTEXT
        ldy     OLDTEXT+1
        sta     TXTPTR
        sty     TXTPTR+1
        rts

; =============================================================================
; "GET" STATEMENT
; SJ April 2014. Added back in GET functionality.
; =============================================================================
GET:
	jsr     ERRDIR	
	LDX     #<(INPUTBUFFER+1)
	LDY     #>(INPUTBUFFER+1)

	STY     INPUTBUFFER+1
	LDA     #$40
        
	JSR     PROCESS_INPUT_LIST
	RTS

; =============================================================================
; "INPUT#" STATEMENT
; "INPUT" STATEMENT
; =============================================================================

INPUT:
        lsr     Z14
        cmp     #$22
        bne     L2A9E
        jsr     STRTXT
        lda     #$3B
        jsr     SYNCHR
        jsr     STRPRT
L2A9E:
        jsr     ERRDIR
        lda     #$2C
        sta     INPUTBUFFER-1
        jsr     NXIN
        lda     INPUTBUFFER
        bne     L2ABE
        clc
        jmp     CONTROL_C_TYPED
NXIN:
        jsr     OUTQUES    ; '?'
        jsr     OUTSP
        jmp     INLIN

; =============================================================================
; "GETC" STATEMENT
; "READ" STATEMENT
; =============================================================================

READ:
        ldx     DATPTR
        ldy     DATPTR+1
; AppleSoft II, too
        .byte   $A9    ; LDA #$98
L2ABE:
        tya

; =============================================================================
; PROCESS INPUT LIST
;
; (Y,X) IS ADDRESS OF INPUT DATA STRING
; (A) = VALUE FOR INPUTFLG:  $00 FOR INPUT
;                 $40 FOR GET
;                $98 FOR READ
; =============================================================================
PROCESS_INPUT_LIST:
        sta     INPUTFLG
        stx     INPTR
        sty     INPTR+1
PROCESS_INPUT_ITEM:
        jsr     PTRGET
        sta     FORPNT
        sty     FORPNT+1
        lda     TXTPTR
        ldy     TXTPTR+1
        sta     TXPSV
        sty     TXPSV+1
        ldx     INPTR
        ldy     INPTR+1
        stx     TXTPTR
        sty     TXTPTR+1
        jsr     CHRGOT
        bne     INSTART
        BIT     INPUTFLG
        BVC		L2AF0			; SJ April 2014. Adding GET back in.
        
        JSR     Matrix_Scan     ; Scan the key matrix.
    	LDA     key_pressed     ; Will return the ASCII value of the key or zero.               
        STA 	INPUTBUFFER		; Store it.      
            		
; BUG: The beq/bne L2AF8 below is supposed
; to be always taken. For this to happen,
; the last load must be a 0 for beq
; and != 0 for bne. The original Microsoft
; code had ldx/ldy/bne here, which was only
; correct for a non-ZP INPUTBUFFER. Commodore
; fixed it in CBMBASIC V1 by swapping the
; ldx and the ldy. It was broken on KIM,
; but okay on APPLE and CBM2, because
; these used a non-ZP INPUTBUFFER.
; Microsoft fixed this somewhere after KIM
; and before MICROTAN, by using beq instead
; of bne in the ZP case.

        LDY     #>(INPUTBUFFER-1)
        
        LDX		#<(INPUTBUFFER-1)
        jmp     L2AF8	; Always. SJ April changed branch to jump.
        
L2AF0:                
        BMI     FINDATA
        jsr     OUTQUES
        JSR     NXIN
L2AF8:       
        stx     TXTPTR
        sty     TXTPTR+1
; =============================================================================

INSTART:
        jsr     CHRGET
        bit     VALTYP
        bpl     L2B34
        
        BIT     INPUTFLG	; SJ April 2014. Adding GET back in.  
        bvc     L2B10
      
      	INX
     	STX     TXTPTR
     	LDA     #$00
     	STA     CHARAC
     	beq     L2B1C
        
L2B10:        
        STA     CHARAC        
        cmp     #$22
        beq     L2B1D
        lda     #$3A
        sta     CHARAC
        LDA     #$2C
L2B1C:        
        clc
L2B1D:
        sta     ENDCHR
        lda     TXTPTR
        ldy     TXTPTR+1
        adc     #$00
        bcc     L2B28
        iny
L2B28:
        jsr     STRLT2
        jsr     POINT
        jsr     LETSTRING
        jmp     INPUT_MORE
        
; =============================================================================
L2B34:
        jsr     FIN
        jsr     SETFOR
; =============================================================================

INPUT_MORE:
        jsr     CHRGOT
        beq     L2B48
        cmp     #$2C
        beq     L2B48
        jmp     INPUTERR
L2B48:
        lda     TXTPTR
        ldy     TXTPTR+1
        sta     INPTR
        sty     INPTR+1
        lda     TXPSV
        ldy     TXPSV+1
        sta     TXTPTR
        sty     TXTPTR+1
        jsr     CHRGOT
        beq     INPDONE
        jsr     CHKCOM
        jmp     PROCESS_INPUT_ITEM
        
; =============================================================================

FINDATA:
        jsr     DATAN
        iny
        tax
        bne     L2B7C
        ldx     #ERR_NODATA
        iny
        lda     (TXTPTR),y
        beq     GERR
        iny
        lda     (TXTPTR),y
        sta     Z8C
        iny
        lda     (TXTPTR),y
        iny
        sta     Z8C+1
L2B7C:
        lda     (TXTPTR),y
        tax
        jsr     ADDON
        cpx     #$83
        bne     FINDATA
        jmp     INSTART
; ---NO MORE INPUT REQUESTED------
INPDONE:
        lda     INPTR
        ldy     INPTR+1
        ldx     INPUTFLG
        bpl     L2B94       ; INPUT or GET
        jmp     SETDA
L2B94:
        ldy     #$00
        lda     (INPTR),y
        beq     L2BA1
        lda     #<ERREXTRA
        ldy     #>ERREXTRA
        jmp     STROUT
L2BA1:
        rts
; =============================================================================

ERREXTRA:
        .byte   "?EXTRA IGNORED"


        .byte   $0D,$0A,$00
ERRREENTRY:
        .byte   "?REDO FROM START"


        .byte   $0D,$0A,$00

; =============================================================================
; "NEXT" STATEMENT
; =============================================================================

NEXT:
        bne     NEXT1
        ldy     #$00
        beq     NEXT2
NEXT1:
        jsr     PTRGET
NEXT2:
        sta     FORPNT
        sty     FORPNT+1
        jsr     GTFORPNT
        beq     NEXT3
        ldx     #$00
GERR:
        beq     JERROR
NEXT3:
        txs
        inx
        inx
        inx
        inx
        txa
        inx
        inx
        inx
        inx
        inx
        stx     DEST
        ldy     #>STACK
        jsr     LOAD_FAC_FROM_YA
        tsx
        lda     STACK+BYTES_FP+4,x
        sta     FACSIGN
        lda     FORPNT
        ldy     FORPNT+1
        jsr     FADD
        jsr     SETFOR
        ldy     #>STACK
        jsr     FCOMP2
        tsx
        sec
        sbc     STACK+BYTES_FP+4,x
        beq     L2C22
        lda     STACK+2*BYTES_FP+5,x
        sta     CURLIN
        lda     STACK+2*BYTES_FP+6,x
        sta     CURLIN+1
        lda     STACK+2*BYTES_FP+8,x
        sta     TXTPTR
        lda     STACK+2*BYTES_FP+7,x
        sta     TXTPTR+1
L2C1F:
        jmp     NEWSTT
L2C22:
        txa
        adc     #2*BYTES_FP+7
        tax
        txs
        jsr     CHRGOT
        cmp     #$2C
        bne     L2C1F
        jsr     CHRGET
        jsr     NEXT1

; =============================================================================
; EVALUATE EXPRESSION, MAKE SURE IT IS NUMERIC
; =============================================================================

FRMNUM:
        jsr     FRMEVL

; =============================================================================
; MAKE SURE (FAC) IS NUMERIC
; =============================================================================
CHKNUM:
        clc
        .byte   $24

; =============================================================================
; MAKE SURE (FAC) IS STRING
; =============================================================================
CHKSTR:
        sec

; =============================================================================
; MAKE SURE (FAC) IS CORRECT TYPE
; IF C=0, TYPE MUST BE NUMERIC
; IF C=1, TYPE MUST BE STRING
; =============================================================================

CHKVAL:
        bit     VALTYP
        bmi     L2C41
        bcs     L2C43
L2C40:
        rts
L2C41:
        bcs     L2C40
L2C43:
        ldx     #ERR_BADTYPE
JERROR:
        jmp     ERROR

; =============================================================================
; EVALUATE THE EXPRESSION AT TXTPTR, LEAVING THE
; RESULT IN FAC.  WORKS FOR BOTH STRING AND NUMERIC
; EXPRESSIONS.
; =============================================================================

FRMEVL:
        ldx     TXTPTR
        bne     L2C4E
        dec     TXTPTR+1
L2C4E:
        dec     TXTPTR
        ldx     #$00
        .byte   $24
FRMEVL1:
        pha
        txa
        pha
        lda     #$01
        jsr     CHKMEM
        jsr     FRM_ELEMENT
        lda     #$00
        sta     CPRTYP
FRMEVL2:
        jsr     CHRGOT
L2C65:
        sec
        sbc     #TOKEN_GREATER
        bcc     L2C81
        cmp     #$03
        bcs     L2C81
        cmp     #$01
        rol     a
        eor     #$01
        eor     CPRTYP
        cmp     CPRTYP
        bcc     SNTXERR
        sta     CPRTYP
        jsr     CHRGET
        jmp     L2C65
L2C81:
        ldx     CPRTYP
        bne     FRM_RELATIONAL
        bcs     L2D02
        adc     #$07
        bcc     L2D02
        adc     VALTYP
        bne     L2C92
        jmp     CAT
L2C92:
        adc     #$FF
        sta     INDEX
        asl     a
        adc     INDEX
        tay
FRM_PRECEDENCE_TEST:
        pla
        cmp     MATHTBL,y
        bcs     FRM_PERFORM1
        jsr     CHKNUM
L2CA3:
        pha
L2CA4:
        jsr     FRM_RECURSE
        pla
        ldy     LASTOP
        bpl     PREFNC
        tax
        beq     GOEX
        bne     FRM_PERFORM2

; =============================================================================
; FOUND ONE OR MORE RELATIONAL OPERATORS <,=,>
; =============================================================================

FRM_RELATIONAL:
        lsr     VALTYP
        txa
        rol     a
        ldx     TXTPTR
        bne     L2CBB
        dec     TXTPTR+1
L2CBB:
        dec     TXTPTR
        ldy     #$1B
        sta     CPRTYP
        bne     FRM_PRECEDENCE_TEST
PREFNC:
        cmp     MATHTBL,y
        bcs     FRM_PERFORM2
        bcc     L2CA3

; =============================================================================
; STACK THIS OPERATION AND CALL FRMEVL FOR
; ANOTHER ONE
; =============================================================================

FRM_RECURSE:
        lda     MATHTBL+2,y
        pha
        lda     MATHTBL+1,y
        pha
        jsr     FRM_STACK1
        lda     CPRTYP
        jmp     FRMEVL1
SNTXERR:
        jmp     SYNERR

; =============================================================================
; STACK (FAC)
; THREE ENTRY POINTS:
;     1, FROM FRMEVL
;    2, FROM "STEP"
;    3, FROM "FOR"
; =============================================================================

FRM_STACK1:
        lda     FACSIGN
        ldx     MATHTBL,y

; =============================================================================
; ENTER HERE FROM "STEP", TO PUSH STEP SIGN AND VALUE
; =============================================================================

FRM_STACK2:
        tay
        pla
        sta     INDEX
        inc     INDEX ; bug: assumes not on page boundary
; bug exists on AppleSoft II
        pla
        sta     INDEX+1
        tya
        pha

; =============================================================================
; ENTER HERE FROM "FOR", WITH (INDEX) = STEP,
; TO PUSH INITIAL VALUE OF "FOR" VARIABLE
; =============================================================================

FRM_STACK3:
        jsr     ROUND_FAC
        lda     FAC+3
        pha
        lda     FAC+2
        pha
        lda     FAC+1
        pha
        lda     FAC
        pha
        jmp     (INDEX)
L2D02:
        ldy     #$FF
        pla
GOEX:
        beq     EXIT

; =============================================================================
; PERFORM STACKED OPERATION
;
; (A) = PRECEDENCE BYTE
; STACK:  1 -- CPRMASK
;    5 -- (ARG)
;    2 -- ADDR OF PERFORMER
; =============================================================================

FRM_PERFORM1:
        cmp     #$64
        beq     L2D0E
        jsr     CHKNUM
L2D0E:
        sty     LASTOP
FRM_PERFORM2:
        pla
        lsr     a
        sta     CPRMASK
        pla
        sta     ARG
        pla
        sta     ARG+1
        pla
        sta     ARG+2
        pla
        sta     ARG+3
        pla
        sta     ARGSIGN
        eor     FACSIGN
        sta     SGNCPR
EXIT:
        lda     FAC
        rts

; =============================================================================
; GET ELEMENT IN EXPRESSION
;
; GET VALUE OF VARIABLE OR NUMBER AT TXTPNT, OR POINT
; TO STRING DESCRIPTOR IF A STRING, AND PUT IN FAC.
; =============================================================================

FRM_ELEMENT:
        lda     #$00
        sta     VALTYP
L2D31:
        jsr     CHRGET
        bcs     L2D39
L2D36:
        jmp     FIN
L2D39:
        jsr     ISLETC
        bcs     FRM_VARIABLE
CON_PI:
        cmp     #$2E
        beq     L2D36
        cmp     #TOKEN_MINUS
        beq     MIN
        cmp     #TOKEN_PLUS
        beq     L2D31
        cmp     #$22
        bne     NOT_

; =============================================================================
; STRING CONSTANT ELEMENT
;
; SET Y,A = (TXTPTR)+CARRY
; =============================================================================

STRTXT:
        lda     TXTPTR
        ldy     TXTPTR+1
        adc     #$00
        bcc     L2D57
        iny
L2D57:
        jsr     STRLIT
        jmp     POINT

; =============================================================================
; "NOT" FUNCTION
; IF FAC=0, RETURN FAC=1
; IF FAC<>0, RETURN FAC=0
; =============================================================================

NOT_:
        cmp     #TOKEN_NOT
        bne     L2D74
        ldy     #$18
        bne     EQUL

; =============================================================================
; COMPARISON FOR EQUALITY (= OPERATOR)
; ALSO USED TO EVALUATE "NOT" FUNCTION
; =============================================================================

EQUOP:
        jsr     AYINT
        lda     FAC_LAST
        eor     #$FF
        tay
        lda     FAC_LAST-1
        eor     #$FF
        jmp     GIVAYF
L2D74:
        cmp     #TOKEN_FN
        bne     L2D7B
        jmp     L31F3
L2D7B:
        cmp     #TOKEN_SGN
        bcc     PARCHK
        jmp     UNARY

; =============================================================================
; EVALUATE "(EXPRESSION)"
; =============================================================================

PARCHK:
        jsr     CHKOPN
        jsr     FRMEVL
CHKCLS:
        lda     #$29
        .byte   $2C
CHKOPN:
        lda     #$28
        .byte   $2C
CHKCOM:
        lda     #$2C

; =============================================================================
; UNLESS CHAR AT TXTPTR = (A), SYNTAX ERROR
; =============================================================================

SYNCHR:    ; XXX all CBM code calls SYNCHR instead of CHKCOM
        ldy     #$00
        cmp     (TXTPTR),y
        bne     SYNERR
        jmp     CHRGET

; =============================================================================

SYNERR:
        ldx     #ERR_SYNTAX
        jmp     ERROR

; =============================================================================

MIN:
        ldy     #$15
EQUL:
        pla
        pla
        jmp     L2CA4

; =============================================================================

FRM_VARIABLE:
        jsr     PTRGET

FRM_VARIABLE_CALL    = *-1
        sta     FAC_LAST-1
        sty     FAC_LAST
        ldx     VALTYP
        beq     L2DB1
; bugfix?
; fixed on AppleSoft II, not on any CBM
        rts
L2DB1:
        jmp     LOAD_FAC_FROM_YA

; =============================================================================

UNARY:
        asl     a
        pha
        tax
        jsr     CHRGET
        cpx     #<(TOKEN_LEFTSTR*2-1)
        bcc     L2DEF
        jsr     CHKOPN
        jsr     FRMEVL
        jsr     CHKCOM
        jsr     CHKSTR
        pla
        tax
        lda     FAC_LAST
        pha
        lda     FAC_LAST-1
        pha
        txa
        pha
        jsr     GETBYT
        pla
        tay
        txa
        pha
        jmp     L2DF4
L2DEF:
        jsr     PARCHK
        pla
        tay
L2DF4:
        lda     UNFNC+($80-TOKEN_SGN)*2,y
        sta     JMPADRS+1
        lda     UNFNC+($80-TOKEN_SGN)*2+1,y
        sta     JMPADRS+2
        jsr     JMPADRS
        jmp     CHKNUM

; =============================================================================

OR:
        ldy     #$FF
        .byte   $2C

; =============================================================================

TAND:
        ldy     #$00
        sty     EOLPNTR
        jsr     AYINT
        lda     FAC_LAST-1
        eor     EOLPNTR
        sta     CHARAC
        lda     FAC_LAST
        eor     EOLPNTR
        sta     ENDCHR
        jsr     COPY_ARG_TO_FAC
        jsr     AYINT
        lda     FAC_LAST
        eor     EOLPNTR
        and     ENDCHR
        eor     EOLPNTR
        tay
        lda     FAC_LAST-1
        eor     EOLPNTR
        and     CHARAC
        eor     EOLPNTR
        jmp     GIVAYF

; =============================================================================
; PERFORM RELATIONAL OPERATIONS
; =============================================================================

RELOPS:
        jsr     CHKVAL
        bcs     STRCMP
        lda     ARGSIGN
        ora     #$7F
        and     ARG+1
        sta     ARG+1
        lda     #<ARG
        ldy     #$00
        jsr     FCOMP
        tax
        jmp     NUMCMP

; =============================================================================
; STRING COMPARISON
; =============================================================================

STRCMP:
        lda     #$00
        sta     VALTYP
        dec     CPRTYP
        jsr     FREFAC
        sta     FAC
        stx     FAC+1
        sty     FAC+2
        lda     ARG_LAST-1
        ldy     ARG_LAST
        jsr     FRETMP
        stx     ARG_LAST-1
        sty     ARG_LAST
        tax
        sec
        sbc     FAC
        beq     L2E74
        lda     #$01
        bcc     L2E74
        ldx     FAC
        lda     #$FF
L2E74:
        sta     FACSIGN
        ldy     #$FF
        inx
STRCMP1:
        iny
        dex
        bne     L2E84
        ldx     FACSIGN
NUMCMP:
        bmi     CMPDONE
        clc
        bcc     CMPDONE
L2E84:
        lda     (ARG_LAST-1),y
        cmp     (FAC+1),y
        beq     STRCMP1
        ldx     #$FF
        bcs     CMPDONE
        ldx     #$01
CMPDONE:
        inx
        txa
        rol     a
        and     CPRMASK
        beq     L2E99
        lda     #$FF
L2E99:
        jmp     FLOAT

; =============================================================================
; "DIM" STATEMENT
; =============================================================================

NXDIM:
        jsr     CHKCOM
DIM:
        tax
        jsr     PTRGET2
        jsr     CHRGOT
        bne     NXDIM
        rts

; =============================================================================
; PTRGET -- GENERAL VARIABLE SCAN
;
; SCANS VARIABLE NAME AT TXTPTR, AND SEARCHES THE
; VARTAB AND ARYTAB FOR THE NAME.
; IF NOT FOUND, CREATE VARIABLE OF APPROPRIATE TYPE.
; RETURN WITH ADDRESS IN VARPNT AND Y,A
;
; ACTUAL ACTIVITY CONTROLLED SOMEWHAT BY TWO FLAGS:
;    DIMFLG -- NONZERO IF CALLED FROM "DIM"
;        ELSE = 0
;
;    SUBFLG -- = $00
;        = $40 IF CALLED FROM "GETARYPT"
; =============================================================================

PTRGET:
        ldx     #$00
        jsr     CHRGOT
PTRGET2:
        stx     DIMFLG
PTRGET3:
        sta     VARNAM
        jsr     CHRGOT
        jsr     ISLETC
        bcs     NAMOK
        jmp     SYNERR
NAMOK:
        ldx     #$00
        stx     VALTYP
        jsr     CHRGET
        bcc     L2ECD
        jsr     ISLETC
        bcc     L2ED8
L2ECD:
        tax
L2ECE:
        jsr     CHRGET
        bcc     L2ECE
        jsr     ISLETC
        bcs     L2ECE
L2ED8:
        cmp     #$24
        bne     L2EF9
        lda     #$FF
        sta     VALTYP
        txa
        ora     #$80
        tax
        jsr     CHRGET
L2EF9:
        stx     VARNAM+1
        sec
        ora     SUBFLG
        sbc     #$28
        bne     L2F05
        jmp     ARRAY
L2F05:
        lda     #$00
        sta     SUBFLG
        lda     VARTAB
        ldx     VARTAB+1
        ldy     #$00
L2F0F:
        stx     LOWTR+1
L2F11:
        sta     LOWTR
        cpx     ARYTAB+1
        bne     L2F1B
        cmp     ARYTAB
        beq     NAMENOTFOUND
L2F1B:
        lda     VARNAM
        cmp     (LOWTR),y
        bne     L2F29
        lda     VARNAM+1
        iny
        cmp     (LOWTR),y
        beq     SET_VARPNT_AND_YA
        dey
L2F29:
        clc
        lda     LOWTR
        adc     #BYTES_PER_VARIABLE
        bcc     L2F11
        inx
        bne     L2F0F

; =============================================================================
; CHECK IF (A) IS ASCII LETTER A-Z
;
; RETURN CARRY = 1 IF A-Z
;    = 0 IF NOT
; =============================================================================

ISLETC:
        cmp     #$41
        bcc     L2F3C
        sbc     #$5B
        sec
        sbc     #$A5
L2F3C:
        rts

; =============================================================================
; VARIABLE NOT FOUND, SO MAKE ONE
; =============================================================================

NAMENOTFOUND:
        pla
        pha
        cmp     #<FRM_VARIABLE_CALL
        bne     MAKENEWVARIABLE
        lda     #<C_ZERO
        ldy     #>C_ZERO
        rts

; =============================================================================

C_ZERO:
        .byte   $00,$00

; =============================================================================
; MAKE A NEW SIMPLE VARIABLE
;
; MOVE ARRAYS UP 7 BYTES TO MAKE ROOM FOR NEW VARIABLE
; ENTER 7-BYTE VARIABLE DATA IN THE HOLE
; =============================================================================

MAKENEWVARIABLE:
        lda     ARYTAB
        ldy     ARYTAB+1
        sta     LOWTR
        sty     LOWTR+1
        lda     STREND
        ldy     STREND+1
        sta     HIGHTR
        sty     HIGHTR+1
        clc
        adc     #BYTES_PER_VARIABLE
        bcc     L2F68
        iny
L2F68:
        sta     HIGHDS
        sty     HIGHDS+1
        jsr     BLTU
        lda     HIGHDS
        ldy     HIGHDS+1
        iny
        sta     ARYTAB
        sty     ARYTAB+1
        ldy     #$00
        lda     VARNAM
        sta     (LOWTR),y
        iny
        lda     VARNAM+1
        sta     (LOWTR),y
        lda     #$00
        iny
        sta     (LOWTR),y
        iny
        sta     (LOWTR),y
        iny
        sta     (LOWTR),y
        iny
        sta     (LOWTR),y

; =============================================================================
; PUT ADDRESS OF VALUE OF VARIABLE IN VARPNT AND Y,A
; =============================================================================

SET_VARPNT_AND_YA:
        lda     LOWTR
        clc
        adc     #$02
        ldy     LOWTR+1
        bcc     L2F9E
        iny
L2F9E:
        sta     VARPNT
        sty     VARPNT+1
        rts

; =============================================================================
; COMPUTE ADDRESS OF FIRST VALUE IN ARRAY
; ARYPNT = (LOWTR) + #DIMS*2 + 5
; =============================================================================

GETARY:
        lda     EOLPNTR
        asl     a
        adc     #$05
        adc     LOWTR
        ldy     LOWTR+1
        bcc     L2FAF
        iny
L2FAF:
        sta     HIGHDS
        sty     HIGHDS+1
        rts

; =============================================================================
NEG32768:
        .byte   $90,$80,$00,$00

; =============================================================================
; EVALUATE NUMERIC FORMULA AT TXTPTR
; CONVERTING RESULT TO INTEGER 0 <= X <= 32767
; IN FAC+3,4
; =============================================================================

MAKINT:
        jsr     CHRGET
        jsr     FRMNUM

; =============================================================================
; CONVERT FAC TO INTEGER
; MUST BE POSITIVE AND LESS THAN 32768
; =============================================================================

MKINT:
        lda     FACSIGN
        bmi     MI1

; =============================================================================
; CONVERT FAC TO INTEGER
; MUST BE -32767 <= FAC <= 32767
; =============================================================================

AYINT:
        lda     FAC
        cmp     #$90
        bcc     MI2
        lda     #<NEG32768
        ldy     #>NEG32768
        jsr     FCOMP
MI1:
        bne     IQERR
MI2:
        jmp     QINT

; =============================================================================
; LOCATE ARRAY ELEMENT OR CREATE AN ARRAY
; =============================================================================

ARRAY:
        lda     DIMFLG
        pha
        lda     VALTYP
        pha
        ldy     #$00
L2FDE:
        tya
        pha
        lda     VARNAM+1
        pha
        lda     VARNAM
        pha
        jsr     MAKINT
        pla
        sta     VARNAM
        pla
        sta     VARNAM+1
        pla
        tay
        tsx
        lda     STACK+2,x
        pha
        lda     STACK+1,x
        pha
        lda     FAC_LAST-1
        sta     STACK+2,x
        lda     FAC_LAST
        sta     STACK+1,x
        iny
        jsr     CHRGOT
        cmp     #$2C
        beq     L2FDE
        sty     EOLPNTR
        jsr     CHKCLS
        pla
        sta     VALTYP
        pla
        sta     DIMFLG

; =============================================================================
; SEARCH ARRAY TABLE FOR THIS ARRAY NAME
; =============================================================================

        ldx     ARYTAB
        lda     ARYTAB+1
L301F:
        stx     LOWTR
        sta     LOWTR+1
        cmp     STREND+1
        bne     L302B
        cpx     STREND
        beq     MAKE_NEW_ARRAY
L302B:
        ldy     #$00
        lda     (LOWTR),y
        iny
        cmp     VARNAM
        bne     L303A
        lda     VARNAM+1
        cmp     (LOWTR),y
        beq     USE_OLD_ARRAY
L303A:
        iny
        lda     (LOWTR),y
        clc
        adc     LOWTR
        tax
        iny
        lda     (LOWTR),y
        adc     LOWTR+1
        bcc     L301F

; =============================================================================
; ERROR:  BAD SUBSCRIPTS
; =============================================================================

SUBERR:
        ldx     #ERR_BADSUBS
        .byte   $2C

; =============================================================================
; ERROR:  ILLEGAL QUANTITY
; =============================================================================

IQERR:
        ldx     #ERR_ILLQTY
JER:
        jmp     ERROR

; =============================================================================
; FOUND THE ARRAY
; =============================================================================

USE_OLD_ARRAY:
        ldx     #ERR_REDIMD
        lda     DIMFLG
        bne     JER
        jsr     GETARY
        lda     EOLPNTR
        ldy     #$04
        cmp     (LOWTR),y
        bne     SUBERR
        jmp     FIND_ARRAY_ELEMENT

; =============================================================================
; CREATE A NEW ARRAY, UNLESS CALLED FROM GETARYPT
; =============================================================================

MAKE_NEW_ARRAY:
        jsr     GETARY
        jsr     REASON
        lda     #$00
        tay
        sta     STRNG2+1
        ldx     #BYTES_PER_ELEMENT
        stx     STRNG2
        lda     VARNAM
        sta     (LOWTR),y
        iny
        lda     VARNAM+1
        sta     (LOWTR),y
        lda     EOLPNTR
        iny
        iny
        iny
        sta     (LOWTR),y
L308A:
        ldx     #$0B
        lda     #$00
        bit     DIMFLG
        bvc     L309A
        pla
        clc
        adc     #$01
        tax
        pla
        adc     #$00
L309A:
        iny
        sta     (LOWTR),y
        iny
        txa
        sta     (LOWTR),y
        jsr     MULTIPLY_SUBSCRIPT
        stx     STRNG2
        sta     STRNG2+1
        ldy     INDEX
        dec     EOLPNTR
        bne     L308A
        adc     HIGHDS+1
        bcs     GME
        sta     HIGHDS+1
        tay
        txa
        adc     HIGHDS
        bcc     L30BD
        iny
        beq     GME
L30BD:
        jsr     REASON
        sta     STREND
        sty     STREND+1
        lda     #$00
        inc     STRNG2+1
        ldy     STRNG2
        beq     L30D1
L30CC:
        dey
        sta     (HIGHDS),y
        bne     L30CC
L30D1:
        dec     HIGHDS+1
        dec     STRNG2+1
        bne     L30CC
        inc     HIGHDS+1
        sec
        lda     STREND
        sbc     LOWTR
        ldy     #$02
        sta     (LOWTR),y
        lda     STREND+1
        iny
        sbc     LOWTR+1
        sta     (LOWTR),y
        lda     DIMFLG
        bne     RTS9
        iny

; =============================================================================
; FIND SPECIFIED ARRAY ELEMENT
;
; (LOWTR),Y POINTS AT # OF DIMS IN ARRAY DESCRIPTOR
; THE SUBSCRIPTS ARE ALL ON THE STACK AS INTEGERS
; =============================================================================

FIND_ARRAY_ELEMENT:
        lda     (LOWTR),y
        sta     EOLPNTR
        lda     #$00
        sta     STRNG2
L30F6:
        sta     STRNG2+1
        iny
        pla
        tax
        sta     FAC_LAST-1
        pla
        sta     FAC_LAST
        cmp     (LOWTR),y
        bcc     FAE2
        bne     GSE
        iny
        txa
        cmp     (LOWTR),y
        bcc     FAE3

; =============================================================================

GSE:
        jmp     SUBERR
GME:
        jmp     MEMERR

; =============================================================================

FAE2:
        iny
FAE3:
        lda     STRNG2+1
        ora     STRNG2
        clc
        beq     L3124
        jsr     MULTIPLY_SUBSCRIPT
        txa
        adc     FAC_LAST-1
        tax
        tya
        ldy     INDEX
L3124:
        adc     FAC_LAST
        stx     STRNG2
        dec     EOLPNTR
        bne     L30F6
        asl     STRNG2
        rol     a
        bcs     GSE
        asl     STRNG2
        rol     a
        bcs     GSE
        tay
        lda     STRNG2
        adc     HIGHDS
        sta     VARPNT
        tya
        adc     HIGHDS+1
        sta     VARPNT+1
        tay
        lda     VARPNT
RTS9:
        rts

; =============================================================================
; MULTIPLY (STRNG2) BY ((LOWTR),Y)
; LEAVING PRODUCT IN A,X.  (HI-BYTE ALSO IN Y.)
; USED ONLY BY ARRAY SUBSCRIPT ROUTINES
; =============================================================================

MULTIPLY_SUBSCRIPT:
        sty     INDEX
        lda     (LOWTR),y
        sta     RESULT_LAST-2
        dey
        lda     (LOWTR),y
        sta     RESULT_LAST-1
        lda     #$10
        sta     INDX
        ldx     #$00
        ldy     #$00
L3163:
        txa
        asl     a
        tax
        tya
        rol     a
        tay
        bcs     GME
        asl     STRNG2
        rol     STRNG2+1
        bcc     L317C
        clc
        txa
        adc     RESULT_LAST-2
        tax
        tya
        adc     RESULT_LAST-1
        tay
        bcs     GME
L317C:
        dec     INDX
        bne     L3163
        rts

; =============================================================================
; "FRE" FUNCTION
;
; COLLECTS GARBAGE AND RETURNS # BYTES OF MEMORY LEFT
; =============================================================================

FRE:
        lda     VALTYP
        beq     L3188
        jsr     FREFAC
L3188:
        jsr     GARBAG
        sec
        lda     FRETOP
        sbc     STREND
        tay
        lda     FRETOP+1
        sbc     STREND+1
; FALL INTO GIVAYF TO FLOAT THE VALUE
; NOTE THAT VALUES OVER 32767 WILL RETURN AS NEGATIVE

; =============================================================================
; FLOAT THE SIGNED INTEGER IN A,Y
; =============================================================================

GIVAYF:
        ldx     #$00
        stx     VALTYP
        sta     FAC+1
        sty     FAC+2
        ldx     #$90
        jmp     FLOAT1
POS:
        ldy     POSX

; =============================================================================
; FLOAT (Y) INTO FAC, GIVING VALUE 0-255
; =============================================================================

SNGFLT:
        lda     #$00
        beq     GIVAYF

; =============================================================================
; CHECK FOR DIRECT OR RUNNING MODE
; GIVING ERROR IF DIRECT MODE
; =============================================================================

ERRDIR:
        ldx     CURLIN+1
        inx
        bne     RTS9
        ldx     #ERR_ILLDIR
L31AF:
        jmp     ERROR
DEF:
        jsr     FNC
        jsr     ERRDIR
        jsr     CHKOPN
        lda     #$80
        sta     SUBFLG
        jsr     PTRGET
        jsr     CHKNUM
        jsr     CHKCLS
        lda     #TOKEN_EQUAL
        jsr     SYNCHR
        lda     VARPNT+1
        pha
        lda     VARPNT
        pha
        lda     TXTPTR+1
        pha
        lda     TXTPTR
        pha
        jsr     DATA
        jmp     L3250
FNC:
        lda     #TOKEN_FN
        jsr     SYNCHR
        ora     #$80
        sta     SUBFLG
        jsr     PTRGET3
        sta     FNCNAM
        sty     FNCNAM+1
        jmp     CHKNUM
L31F3:
        jsr     FNC
        lda     FNCNAM+1
        pha
        lda     FNCNAM
        pha
        jsr     PARCHK
        jsr     CHKNUM
        pla
        sta     FNCNAM
        pla
        sta     FNCNAM+1
        ldy     #$02
        ldx     #ERR_UNDEFFN
        lda     (FNCNAM),y
        beq     L31AF
        sta     VARPNT
        tax
        iny
        lda     (FNCNAM),y
        sta     VARPNT+1
L3219:
        lda     (VARPNT),y
        pha
        dey
        bpl     L3219
        ldy     VARPNT+1
        jsr     STORE_FAC_AT_YX_ROUNDED
        lda     TXTPTR+1
        pha
        lda     TXTPTR
        pha
        lda     (FNCNAM),y
        sta     TXTPTR
        iny
        lda     (FNCNAM),y
        sta     TXTPTR+1
        lda     VARPNT+1
        pha
        lda     VARPNT
        pha
        jsr     FRMNUM
        pla
        sta     FNCNAM
        pla
        sta     FNCNAM+1
        jsr     CHRGOT
        beq     L324A
        jmp     SYNERR
L324A:
        pla
        sta     TXTPTR
        pla
        sta     TXTPTR+1
L3250:
        ldy     #$00
        pla
        sta     (FNCNAM),y
        pla
        iny
        sta     (FNCNAM),y
        pla
        iny
        sta     (FNCNAM),y
        pla
        iny
        sta     (FNCNAM),y
        rts
; =============================================================================
; "STR$" FUNCTION
; =============================================================================

STR:
        jsr     CHKNUM
        ldy     #$00
        jsr     FOUT1
        pla
        pla
        lda     #$FF
        ldy     #$00
        beq     STRLIT

; =============================================================================
; GET SPACE AND MAKE DESCRIPTOR FOR STRING WHOSE
; ADDRESS IS IN FAC+3,4 AND WHOSE LENGTH IS IN A-REG
; =============================================================================

STRINI:
        ldx     FAC_LAST-1
        ldy     FAC_LAST
        stx     DSCPTR
        sty     DSCPTR+1

; =============================================================================
; GET SPACE AND MAKE DESCRIPTOR FOR STRING WHOSE
; ADDRESS IS IN Y,X AND WHOSE LENGTH IS IN A-REG
; =============================================================================

STRSPA:
        jsr     GETSPA
        stx     FAC+1
        sty     FAC+2
        sta     FAC
        rts

; =============================================================================
; BUILD A DESCRIPTOR FOR STRING STARTING AT Y,A
; AND TERMINATED BY $00 OR QUOTATION MARK
; RETURN WITH DESCRIPTOR IN A TEMPORARY
; AND ADDRESS OF DESCRIPTOR IN FAC+3,4
; =============================================================================

STRLIT:
        ldx     #$22
        stx     CHARAC
        stx     ENDCHR

; =============================================================================
; BUILD A DESCRIPTOR FOR STRING STARTING AT Y,A
; AND TERMINATED BY $00, (CHARAC), OR (ENDCHR)
;
; RETURN WITH DESCRIPTOR IN A TEMPORARY
; AND ADDRESS OF DESCRIPTOR IN FAC+3,4
; =============================================================================

STRLT2:
        sta     STRNG1
        sty     STRNG1+1
        sta     FAC+1
        sty     FAC+2
        ldy     #$FF
L3298:
        iny
        lda     (STRNG1),y
        beq     L32A9
        cmp     CHARAC
        beq     L32A5
        cmp     ENDCHR
        bne     L3298
L32A5:
        cmp     #$22
        beq     L32AA
L32A9:
        clc
L32AA:
        sty     FAC
        tya
        adc     STRNG1
        sta     STRNG2
        ldx     STRNG1+1
        bcc     L32B6
        inx
L32B6:
        stx     STRNG2+1
        lda     STRNG1+1
        bne     PUTNEW
        tya
        jsr     STRINI
        ldx     STRNG1
        ldy     STRNG1+1
        jsr     MOVSTR

; =============================================================================
; STORE DESCRIPTOR IN TEMPORARY DESCRIPTOR STACK
;
; THE DESCRIPTOR IS NOW IN FAC, FAC+1, FAC+2
; PUT ADDRESS OF TEMP DESCRIPTOR IN FAC+3,4
; =============================================================================

PUTNEW:
        ldx     TEMPPT
        cpx     #TEMPST+9
        bne     PUTEMP
        ldx     #ERR_FRMCPX
JERR:
        jmp     ERROR
PUTEMP:
        lda     FAC
        sta     0,x
        lda     FAC+1
        sta     1,x
        lda     FAC+2
        sta     2,x
        ldy     #$00
        stx     FAC_LAST-1
        sty     FAC_LAST
        dey
        sty     VALTYP
        stx     LASTPT
        inx
        inx
        inx
        stx     TEMPPT
        rts

; =============================================================================
; MAKE SPACE FOR STRING AT BOTTOM OF STRING SPACE
; (A)=# BYTES SPACE TO MAKE
;
; RETURN WITH (A) SAME,
;    AND Y,X = ADDRESS OF SPACE ALLOCATED
; =============================================================================

GETSPA:
        lsr     DATAFLG
L32F1:
        pha
        eor     #$FF
        sec
        adc     FRETOP
        ldy     FRETOP+1
        bcs     L32FC
        dey
L32FC:
        cpy     STREND+1
        bcc     L3311
        bne     L3306
        cmp     STREND
        bcc     L3311
L3306:
        sta     FRETOP
        sty     FRETOP+1
        sta     FRESPC
        sty     FRESPC+1
        tax
        pla
        rts
L3311:
        ldx     #ERR_MEMFULL
        lda     DATAFLG
        bmi     JERR
        jsr     GARBAG
        lda     #$80
        sta     DATAFLG
        pla
        bne     L32F1

; =============================================================================
; SHOVE ALL REFERENCED STRINGS AS HIGH AS POSSIBLE
; IN MEMORY (AGAINST HIMEM), FREEING UP SPACE
; BELOW STRING AREA DOWN TO STREND.
;
; SJ October 2013.
; Garbage collector updated with fixed code from 'Fixed BASIC3 ROM' listing
; available here: http://osiweb.org/software.html
;
; =============================================================================

 GARBAG:
           LDX     MEMSIZ
           LDA     MEMSIZ+1
 FINDHIGHESTSTRING:
           STX     FRETOP
           STA     FRETOP+1
           LDY     #$00
           STY     FNCNAM+1
           STY     FNCNAM
           LDA     STREND
           LDX     STREND+1
           STA     LOWTR
           STX     LOWTR+1
           LDA     #TEMPST
           STA     INDEX
           STY     INDEX+1
   L333D:
           CMP     TEMPPT
           BEQ     L3346
           JSR     CHECK_VARIABLE
           BEQ     L333D
   L3346:
           LDA     #BYTES_PER_VARIABLE
           STA     DSCLEN
           LDA     VARTAB
           LDX     VARTAB+1
           STA     INDEX
           STX     INDEX+1
   L3352:
           CPX     ARYTAB+1
           BNE     L335A
           CMP     ARYTAB
           BEQ     L335F
   L335A:
           JSR     CHECK_SIMPLE_VARIABLE
           BEQ     L3352
   L335F:
           STA     HIGHDS
           STX     HIGHDS+1
           LDA     #4
           STA     DSCLEN
   L3367:
           LDA     HIGHDS
           LDX     HIGHDS+1
   L336B:
           CPX     STREND+1
           BNE     L3376
           CMP     STREND
           BEQ     MOVE_HIGHEST_STRING_TO_TOP

   L3376:
           STA     INDEX
           STX     INDEX+1
           LDY     #$01
           LDA     (INDEX),Y
           PHP
           INY
           LDA     (INDEX),Y
           ADC     HIGHDS
           STA     HIGHDS
           INY
           LDA     (INDEX),Y
           ADC     HIGHDS+1
           STA     HIGHDS+1
           PLP
           BPL     L3367
           INY
           LDA     (INDEX),Y
           LDY     #$00
           ASL     A
           ADC     #$05
           ADC     INDEX
           STA     INDEX
           BCC     L33A7
           INC     INDEX+1
   L33A7:
           LDX     INDEX+1
   L33A9:
           CPX     HIGHDS+1
           BNE     L33B1
           CMP     HIGHDS
           BEQ     L336B
   L33B1:
           JSR     CHECK_VARIABLE
           BEQ     L33A9

 CHECK_SIMPLE_VARIABLE:
           INY
           LDA     (INDEX),Y
           BPL     CHECK_BUMP
           INY

 CHECK_VARIABLE:
           LDA     (INDEX),Y
           BEQ     CHECK_BUMP
           INY
           LDA     (INDEX),Y
           TAX
           INY
           LDA     (INDEX),Y
           CMP     FRETOP+1
           BCC     L33D5
           BNE     CHECK_BUMP
           CPX     FRETOP
           BCS     CHECK_BUMP
   L33D5:
           CMP     LOWTR+1
           BCC     CHECK_BUMP
           BNE     L33DF
           CPX     LOWTR
           BCC     CHECK_BUMP
   L33DF:
           STX     LOWTR
           STA     LOWTR+1
           LDA     INDEX
           LDX     INDEX+1
           STA     FNCNAM
           STX     FNCNAM+1
           DEY
           DEY
           STY     Z52
   CHECK_BUMP:
           LDA     DSCLEN
           CLC
           ADC     INDEX
           STA     INDEX
           BCC     L33FA
           INC     INDEX+1
   L33FA:
           LDX     INDEX+1
           LDY     #$00
           RTS

   MOVE_HIGHEST_STRING_TO_TOP:
           DEC     DSCLEN
           LDA     FNCNAM+1
           ORA     FNCNAM
           BEQ     L33FA
           LDY     Z52
           CLC
           LDA     (FNCNAM),Y
           ADC     LOWTR
           STA     HIGHTR
           LDA     LOWTR+1
           ADC     #$00
           STA     HIGHTR+1
           LDA     FRETOP
           LDX     FRETOP+1
           STA     HIGHDS
           STX     HIGHDS+1
           JSR     BLTU2
           LDY     Z52
           INY
           LDA     HIGHDS
           STA     (FNCNAM),Y
           TAX
           INC     HIGHDS+1
           LDA     HIGHDS+1
           INY
           STA     (FNCNAM),Y
           JMP     FINDHIGHESTSTRING

; ----------------------------------------------------------------------------
; CONCATENATE TWO STRINGS
; ----------------------------------------------------------------------------
CAT:
        lda     FAC_LAST
        pha
        lda     FAC_LAST-1
        pha
        jsr     FRM_ELEMENT
        jsr     CHKSTR
        pla
        sta     STRNG1
        pla
        sta     STRNG1+1
        ldy     #$00
        lda     (STRNG1),y
        clc
        adc     (FAC_LAST-1),y
        bcc     L3454
        ldx     #ERR_STRLONG
        jmp     ERROR
L3454:
        jsr     STRINI
        jsr     MOVINS
        lda     DSCPTR
        ldy     DSCPTR+1
        jsr     FRETMP
        jsr     MOVSTR1
        lda     STRNG1
        ldy     STRNG1+1
        jsr     FRETMP
        jsr     PUTNEW
        jmp     FRMEVL2

; =============================================================================
; GET STRING DESCRIPTOR POINTED AT BY (STRNG1)
; AND MOVE DESCRIBED STRING TO (FRESPC)
; =============================================================================

MOVINS:
        ldy     #$00
        lda     (STRNG1),y
        pha
        iny
        lda     (STRNG1),y
        tax
        iny
        lda     (STRNG1),y
        tay
        pla

; =============================================================================
; MOVE STRING AT (Y,X) WITH LENGTH (A)
; TO DESTINATION WHOSE ADDRESS IS IN FRESPC,FRESPC+1
; =============================================================================

MOVSTR:
        stx     INDEX
        sty     INDEX+1
MOVSTR1:
        tay
        beq     L3490
        pha
L3487:
        dey
        lda     (INDEX),y
        sta     (FRESPC),y
        tya
        bne     L3487
        pla
L3490:
        clc
        adc     FRESPC
        sta     FRESPC
        bcc     L3499
        inc     FRESPC+1
L3499:
        rts

; =============================================================================
; IF (FAC) IS A TEMPORARY STRING, RELEASE DESCRIPTOR
; =============================================================================

FRESTR:
        jsr     CHKSTR

; =============================================================================
; IF STRING DESCRIPTOR POINTED TO BY FAC+3,4 IS
; A TEMPORARY STRING, RELEASE IT.
; =============================================================================

FREFAC:
        lda     FAC_LAST-1
        ldy     FAC_LAST

; =============================================================================
; IF STRING DESCRIPTOR WHOSE ADDRESS IS IN Y,A IS
; A TEMPORARY STRING, RELEASE IT.
; =============================================================================

FRETMP:
        sta     INDEX
        sty     INDEX+1
        jsr     FRETMS
        php
        ldy     #$00
        lda     (INDEX),y
        pha
        iny
        lda     (INDEX),y
        tax
        iny
        lda     (INDEX),y
        tay
        pla
        plp
        bne     L34CD
        cpy     FRETOP+1
        bne     L34CD
        cpx     FRETOP
        bne     L34CD
        pha
        clc
        adc     FRETOP
        sta     FRETOP
        bcc     L34CC
        inc     FRETOP+1
L34CC:
        pla
L34CD:
        stx     INDEX
        sty     INDEX+1
        rts

; =============================================================================
; RELEASE TEMPORARY DESCRIPTOR IF Y,A = LASTPT
; =============================================================================

FRETMS:
        cpy     LASTPT+1
        bne     L34E2
        cmp     LASTPT
        bne     L34E2
        sta     TEMPPT
        sbc     #$03
        sta     LASTPT
        ldy     #$00
L34E2:
        rts

; =============================================================================
; "CHR$" FUNCTION
; =============================================================================

CHRSTR:
        jsr     CONINT
        txa
        pha
        lda     #$01		; 1 byte string.
        jsr     STRSPA		; Create space for it.
        pla
        ldy     #$00
        sta     (FAC+1),y	; Store the string at that adderess.
        pla
        pla
        jmp     PUTNEW

; =============================================================================
; "LEFT$" FUNCTION
; =============================================================================

LEFTSTR:
        jsr     SUBSTRING_SETUP
        cmp     (DSCPTR),y
        tya
SUBSTRING1:
        bcc     L3503
        lda     (DSCPTR),y
        tax
        tya
L3503:
        pha
SUBSTRING2:
        txa
SUBSTRING3:
        pha
        jsr     STRSPA
        lda     DSCPTR
        ldy     DSCPTR+1
        jsr     FRETMP
        pla
        tay
        pla
        clc
        adc     INDEX
        sta     INDEX
        bcc     L351C
        inc     INDEX+1
L351C:
        tya
        jsr     MOVSTR1
        jmp     PUTNEW

; =============================================================================
; "RIGHT$" FUNCTION
; =============================================================================

RIGHTSTR:
        jsr     SUBSTRING_SETUP
        clc
        sbc     (DSCPTR),y
        eor     #$FF
        jmp     SUBSTRING1

; =============================================================================
; "MID$" FUNCTION
; =============================================================================

MIDSTR:
        lda     #$FF
        sta     FAC_LAST
        jsr     CHRGOT
        cmp     #$29
        beq     L353F
        jsr     CHKCOM
        jsr     GETBYT
L353F:
        jsr     SUBSTRING_SETUP
        dex
        txa
        pha
        clc
        ldx     #$00
        sbc     (DSCPTR),y
        bcs     SUBSTRING2
        eor     #$FF
        cmp     FAC_LAST
        bcc     SUBSTRING3
        lda     FAC_LAST
        bcs     SUBSTRING3

; =============================================================================
; COMMON SETUP ROUTINE FOR LEFT$, RIGHT$, MID$:
; REQUIRE ")"; POP RETURN ADRS, GET DESCRIPTOR
; ADDRESS, GET 1ST PARAMETER OF COMMAND
; =============================================================================

SUBSTRING_SETUP:
        jsr     CHKCLS
        pla
        sta     JMPADRS+1
        pla
        sta     JMPADRS+2
        pla
        pla
        pla
        tax
        pla
        sta     DSCPTR
        pla
        sta     DSCPTR+1
        ldy     #$00
        txa
        beq     GOIQ
        inc     JMPADRS+1
        jmp     (JMPADRS+1)

; =============================================================================
; "LEN" FUNCTION
; =============================================================================

LEN:
        jsr     GETSTR
SNGFLT1:
        jmp     SNGFLT

; =============================================================================
; IF LAST RESULT IS A TEMPORARY STRING, FREE IT
; MAKE VALTYP NUMERIC, RETURN LENGTH IN Y-REG
; =============================================================================

GETSTR:
        jsr     FRESTR
        ldx     #$00
        stx     VALTYP
        tay
        rts

; =============================================================================
; "ASC" FUNCTION
; =============================================================================

ASC:
        jsr     GETSTR
        beq     GOIQ
        ldy     #$00
        lda     (INDEX),y
        tay
        jmp     SNGFLT1

; =============================================================================
GOIQ:
        jmp     IQERR

; =============================================================================
; SCAN TO NEXT CHARACTER AND CONVERT EXPRESSION
; TO SINGLE BYTE IN X-REG
; =============================================================================

GTBYTC:
        jsr     CHRGET

; =============================================================================
; EVALUATE EXPRESSION AT TXTPTR, AND
; CONVERT IT TO SINGLE BYTE IN X-REG
; =============================================================================

GETBYT:
        jsr     FRMNUM

; =============================================================================
; CONVERT (FAC) TO SINGLE BYTE INTEGER IN X-REG
; =============================================================================

CONINT:
        jsr     MKINT
        ldx     FAC_LAST-1
        bne     GOIQ
        ldx     FAC_LAST
        jmp     CHRGOT

; =============================================================================
; "VAL" FUNCTION
; =============================================================================

VAL:
        jsr     GETSTR
        bne     L35AC
        jmp     ZERO_FAC
L35AC:
        ldx     TXTPTR
        ldy     TXTPTR+1
        stx     STRNG2
        sty     STRNG2+1
        ldx     INDEX
        stx     TXTPTR
        clc
        adc     INDEX
        sta     DEST
        ldx     INDEX+1
        stx     TXTPTR+1
        bcc     L35C4
        inx
L35C4:
        stx     DEST+1
        ldy     #$00
        lda     (DEST),y
        pha
        lda     #$00
        sta     (DEST),y
        jsr     CHRGOT
        jsr     FIN
        pla
        ldy     #$00
        sta     (DEST),y

; =============================================================================
; COPY STRNG2 INTO TXTPTR
; =============================================================================

POINT:
        ldx     STRNG2
        ldy     STRNG2+1
        stx     TXTPTR
        sty     TXTPTR+1
        rts
; =============================================================================
; EVALUATE "EXP1,EXP2"
;
; CONVERT EXP1 TO 16-BIT NUMBER IN LINNUM
; CONVERT EXP2 TO 8-BIT NUMBER IN X-REG
; =============================================================================

GTNUM:
        jsr     FRMNUM
        jsr     GETADR

; =============================================================================
; EVALUATE ",EXPRESSION"
; CONVERT EXPRESSION TO SINGLE BYTE IN X-REG
; =============================================================================

COMBYTE:
        jsr     CHKCOM
        jmp     GETBYT

; =============================================================================
; CONVERT (FAC) TO A 16-BIT VALUE IN LINNUM
; =============================================================================

GETADR:
        lda     FACSIGN
        bmi     GOIQ
        lda     FAC
        cmp     #$91
        bcs     GOIQ
        jsr     QINT
        lda     FAC_LAST-1
        ldy     FAC_LAST
        sty     LINNUM
        sta     LINNUM+1
        rts

; =============================================================================
; "PEEK" FUNCTION
; =============================================================================

PEEK:
        jsr     GETADR
        ldy     #$00
; disallow PEEK between $C000 and $DFFF
        lda     (LINNUM),y
        tay
        jmp     SNGFLT

; =============================================================================
; "POKE" STATEMENT
; =============================================================================

POKE:
        jsr     GTNUM
        txa
        ldy     #$00
        sta     (LINNUM),y
        rts


; =============================================================================
; Normal.
; In normal mode (ls_mode = 0) the serial port isn't used.
;==============================================================================

NORMAL:
    LDA     #$00                 
    STA     ls_mode       	; Clear the load flag. 
    LDA     #%00001011      ; Set up N parity/echo off/tx int off/rts low/rx int off/dtr active.
    STA     acia1_cm        ; Write to ACIA command register.     
    LDA     acia1_d         ; Read the ACIA data register to clear it.
    LDA     acia1_s         ; Read the ACIA status register to clear it.
    JSR     Beep1
    RTS
    
; =============================================================================
; Load
; When in load mode (ls_mode = 1) we receive characters from the serial 
; port instead of the keyboard. We exit load mode (into normal mode) when 
; a control c is sent via the keyboard.
;==============================================================================

LOAD:             
    LDA     #$01            ; Load.            
    STA     ls_mode       	; Set the load flag.      
    LDA     #%00001001      ; Set up N parity/echo off/tx int off/rts low/rx int on/dtr active.
    STA     acia1_cm        ; Write to ACIA command register.  
    LDA     acia1_d         ; Read the data register to clear any errors.        
    JSR     Beep1 
    JSR     Beep2
    RTS

; =============================================================================
; Binary load.
; When in binary load mode (ls_mode = 2) we receive characters from the serial 
; port instead of the keyboard. These are put directly into memory.
; We exit bload mode (normal mode) when a control c is sent via the keyboard.
;==============================================================================

BINARY:                 
    LDA     #$02            ; Binary Load.            
    STA     ls_mode       	; Set the load flag. 
	lda		#$00			; Reset the counter.
	sta		load_count
	sta		load_count + 1 
    LDA     #%00001001      ; Set up N parity/echo off/tx int off/rts low/rx int on/dtr active.
    STA     acia1_cm        ; Write to ACIA command register.  
    LDA     acia1_d         ; Read the data register to clear any errors.
    JSR     Beep1
    JSR     Beep2 
    RTS
    
; =============================================================================
; Save.
; In save mode (ls_mode = 4) character are echoed to the serial port.
;==============================================================================

SAVE:
    LDA     #$04                 
    STA     ls_mode       	; Set the save flag. 	
    LDA     #%00001011      ; Set up N parity/echo off/tx int off/rts low/rx int off/dtr active.
    STA     acia1_cm        ; Write to ACIA command register.     
    LDA     acia1_d         ; Read the ACIA data register to clear it.
    LDA     acia1_s         ; Read the ACIA status register to clear it.
    JSR     Beep2
    JSR     Beep1
    RTS

; =============================================================================
; CLS.
; Clear the screen.
; SJ Added Nov 2013.
; =============================================================================

CLS:   
    LDA     #$0C            ; Clear screen.
    STA		char_to_send
    JSR     Send_Char_Screen
    RTS

; =============================================================================
; MOVE.
; Moves the to the co-ordinated given in x_val and y_val.
; SJ Added Apr 2014.
; =============================================================================

MOVE:   
    LDA     #$0F            	; Set row.
    STA		char_to_send
    JSR     Send_Char_Screen
    
    LDA     y_val            	; Y value.
    STA		char_to_send
    JSR     Send_Char_Screen
    
    LDA     #$0E            	; Set column.
    STA		char_to_send
    JSR     Send_Char_Screen
        
    LDA     x_val            	; X value.
    STA		char_to_send
    JSR     Send_Char_Screen
    RTS

                      
; =============================================================================
; Plot.
; Plot a pixel at this point.
; Point set by x_val, y_val. Colour set by c_val.
; SJ Added Jan 2014.
; =============================================================================

PLOT: 
	LDA		x_val				; Get X and Y values.	
	STA		X1
	LDA		y_val
	STA		Y1	      
PLOT2:

	LDA     X1            		; X value.
    CMP 	#GRAPHICSXMAX		; If either point is off the screen
    BCS 	PLOT_Done			; we don't bother to plot the point.
    LDA     Y1	            	; Y value.
    CMP 	#GRAPHICSYMAX
    BCS 	PLOT_Done
    
    LDA		c_val				; Check the colour.
    BNE		PLOT_White			; If anything but 0 set it to white.
PLOT_Black:
    LDA     #$06            	; Plot black pixel (clear).    
    JMP 	PLOT_Do
PLOT_White:
    LDA     #$05            	; Plot white pixel (set).    
PLOT_Do:    
    STA		char_to_send
    JSR     Send_Char_Screen
    
    LDA     X1            		; X value.
    STA		char_to_send
    JSR     Send_Char_Screen
    
    LDA     Y1	            	; Y value.
    STA		char_to_send
    JSR     Send_Char_Screen
 PLOT_Done:
    RTS  
    
; =============================================================================
; Line.
; Draw a line.
; Points set by x_val, y_val, a_val, b_val. Colour set by c_val.
; SJ Added Jan 2014.
; Code originally by Daryl Rictor.
; =============================================================================

LINE:          

	LDA		x_val			 ; Copy the variables as they change!
	STA 	X1
	LDA		y_val			 
	STA 	Y1	
	LDA		a_val			 
	STA 	X2
	LDA		b_val			 
	STA 	Y2	

LINE2:	
	TXA						 ; We use X and Y so save them here.
	PHA
	TYA
	PHA 
	LDA 	X2               ; Make sure X1 < X2.
	CMP 	X1					
	BCS 	LINE_CONT		 ; Branch if X2 >= X1.
	LDA 	Y2               ; If not, swap them.
	LDY 	Y1
	STA 	Y1
	STY 	Y2
	LDA 	X1
	LDY 	X2
	STY 	X1
	STA 	X2
	
LINE_CONT:              
	LDA 	X2
	SEC
	SBC 	X1	
	;LSR
	STA 	DX		            ; Set DX.	
	BNE 	LINE_NVT			; SJ rearranged code here due to 128 jump limit.

LINE_VERT:             
	SEC
	LDA 	Y2                	; Calculate DY.
	SBC 	Y1
	BCC 	LINE_VNEG
	JMP 	LINE_VP2
LINE_VP1:              
	INC 	Y1
LINE_VP2:              
	JSR 	PLOT2
	SEC
	LDA 	Y2
	SBC 	Y1
	BNE 	LINE_VP1
	PLA
	TAY
	PLA
	TAX
	RTS	
LINE_VN1:
    DEC 	Y1
LINE_VNEG:             
	JSR 	PLOT2
	SEC
	LDA 	Y2
	SBC 	Y1
	BNE 	LINE_VN1
	PLA
	TAY
	PLA
	TAX
	RTS             
                                            
LINE_NVT:              
	LDA 	#0
	STA 	IX
	STA 	IY	
	LDA 	Y2               	; Calculate DY.
	SEC
	SBC 	Y1
	BEQ 	LINE_HORZ
LINE_NHZ:              
	BCC 	LINE_YNEG		    ; Negative slope.
	JMP		LINE_YPOS			; SJ rearranged code here to get around 128 byte jump limit.	
LINE_HRZ:              
	INC 	X1
LINE_HORZ:             
	JSR 	PLOT2	
	LDA 	X2
	SEC
	SBC 	X1
	BNE 	LINE_HRZ
	PLA
	TAY
	PLA
	TAX	
	RTS		

LINE_YPOS:             
	;LSR
	STA 	DY                	; Positive slope.
	JSR 	PLOT2		        ; Plot X1, Y1.

LINE_LOOP0:            
	LDX 	#0		            ; Plot needed flag.
LINE_LOOP:             
	CLC
	LDA 	IX
	ADC 	DX
	STA 	IX
	BCC 	LINE_LOOP1
	INX			            	; Plot needed yes.
	INC 	X1
LINE_LOOP1:            
	CLC
	LDA 	IY
	ADC 	DY
	STA 	IY
	BCC 	LINE_LOOP2
	INX			            	; Plot needed yes.
	INC 	Y1
LINE_LOOP2:            
	TXA			
	BEQ 	LINE_LOOP		    ; LINE_LOOP if no plt needed.
	JSR 	PLOT2
	LDA 	X1
	CMP 	X2
	BEQ 	LINE_END               	
	LDA 	Y1
	CMP 	Y2
	BNE 	LINE_LOOP0          ; RESET X=0, LINE_LOOP again.
LINE_END:
	LDA 	X2
	STA 	X1
	LDA 	Y2
	STA 	Y1
	JSR 	PLOT2				; Plot final point.
	PLA
	TAY
	PLA
	TAX
	RTS
                  
LINE_YNEG:             
	JSR 	PLOT2				; Plot first point.
	LDA 	Y1
	SEC
	SBC 	Y2
	;LSR
	STA 	DY					; Delta Y.

LINE_LOOPX:            
	LDX 	#0
LINE_LOOP3:
	CLC
	LDA 	IX
	ADC 	DX
	STA 	IX
	BCC 	LINE_LOOP4
	INX
	INC 	X1
LINE_LOOP4:  
	CLC
	LDA 	IY
	ADC 	DY
	STA 	IY
	BCC 	LINE_LOOP5
	INX
	DEC 	Y1					; Only diff from YPOS code is here!
LINE_LOOP5:            
	TXA
	BEQ 	LINE_LOOP3          ; No adjustment yet.
	JSR 	PLOT2
	LDA 	X1                
	CMP 	X2
	BEQ 	LINE_END               	
	LDA 	Y1
	CMP 	Y2
	BEQ 	LINE_END			; If no 128 byte branch limit, fix this. 
	JMP 	LINE_LOOPX            

; =============================================================================
; Rect.
; Draw a rectangular box.
; Points set by x_val, y_val, a_val, b_val. Colour set by c_val.
; SJ Added Jan 2014.
; Line(x1,y1, x2,y1)
; Line(x2,y1, x2,y2)
; Line(x2,y2, x1,y2)
; Line(x1,y2, x1,y1)
; =============================================================================

RECT: 

	LDA		x_val			 
	STA 	X1
	LDA		y_val			 
	STA 	Y1	
	LDA		a_val			 
	STA 	X2
	LDA		y_val			 
	STA 	Y2	
	JSR		LINE2
	
	LDA		a_val			 
	STA 	X1
	LDA		y_val			 
	STA 	Y1	
	LDA		a_val			 
	STA 	X2
	LDA		b_val			 
	STA 	Y2	
	JSR		LINE2
	
	LDA		a_val			
	STA 	X1
	LDA		b_val			 
	STA 	Y1	
	LDA		x_val			 
	STA 	X2
	LDA		b_val			 
	STA 	Y2	
	JSR		LINE2
	
	LDA		x_val			
	STA 	X1
	LDA		b_val			 
	STA 	Y1	
	LDA		x_val			 
	STA 	X2
	LDA		y_val			 
	STA 	Y2	
	JSR		LINE2
	
	RTS

; =============================================================================
; Circle.
; Draw a circle.
; Points set by x_val, y_val.
; Radius set by a_val. Colour set by c_val.
; SJ Added Jan 2014. 
; Original code by Daryl Rictor. 
; Modified for 8 bit.
; =============================================================================

CIRC:

	LDA		x_val			; Store X and Y initial centre values.	
	STA		IX			
	LDA		y_val
	STA		IY	
	
	LDA		a_val			; Store radius.
	STA		Rad				
	
  	bne   	CIRCLE_C1
   	lda   	IX      		; If radius = 0, plot center point and exit.
   	sta   	X1      		; Move center point to plot point variable.
   	lda   	IY
   	STA   	Y1
	jmp   	PLOT2   		; Plot as a point and exit.
	RTS

CIRCLE_C1:
   	LDA   	Rad      		; Load radius.
   	sta   	DY   			; Y = radius.
   	LDA   	#$00			
   	STA   	DX				; X = 0.

   	sec 					; FF = 1 - radius.       
   	lda   	#$01
   	sbc   	Rad
   	sta   	FF
   
CIRCLE_C2:
   	LDA   	#$01			; X2 = 1.
   	sta   	X2
     		
   	lda   	Rad				; Y2 = -2 * radius.
   	asl         			; *2
   	sta   	Y2   
   	EOR   	#$FF
   	sta   	Y2   
   	inc   	Y2
   	bne   	CIRCLE_C3

CIRCLE_C3:
   	lda   	IX				; Plot pixel (IX, IY + DY).
   	sta   	X1
   	clc
   	lda   	IY
   	adc   	DY
   	sta  	Y1   
   	jsr   	PLOT2

   	sec						; Plot pixel (IX, IY - DY).
   	lda   	IY
   	sbc  	DY
   	sta   	Y1 
   	jsr   	PLOT2

   	clc						; Plot pixel (IX + DY, IY).
   	lda  	IX
   	adc  	DY
   	sta  	X1  
   	lda  	IY
   	sta  	Y1
   	jsr  	PLOT2

   	sec						; Plot pixel (IX - DY, IY).
   	lda   	IX
   	sbc   	DY
   	STA  	X1
   	jsr  	PLOT2

CIRCLE_CLOOP:
   	sec						; While (x < y).
   	lda   	DX
   	sbc   	DY
   	bcc   	CIRCLE_C4   	; If X < Y.
   	rts						; Done.

CIRCLE_C4:
   	LDA   	FF
   	bmi   	CIRCLE_C6
   	lda   	DY
   	
CIRCLE_C5:
   	DEC		DY
   	clc
   	lda   	Y2
   	adc   	#$02
   	sta   	Y2
   	adc   	FF
   	sta   	FF
   	
CIRCLE_C6:
   	INC   	DX
   	
CIRCLE_C7:
   	CLC
   	lda   	X2
   	adc   	#$02
   	sta   	X2   
   	adc   	FF
   	sta   	FF
   	
   	JSR		PLOT8
   	jmp   	CIRCLE_CLOOP
		

; =============================================================================
; Circle.
; Draw a circle.
; Points set by x_val, y_val.
; Radius set by a_val. Colour set by c_val.
; SJ Added Jan 2014.
; =============================================================================


; =============================================================================
; PLOT8.
; 8 way plotting routine for circle drawing.
; SJ Added Jan 2014.
; Original code by Stephen Judd from here: 
; http://www.ffd2.com/fridge/chacking/c=hacking9.txt
; =============================================================================

PLOT8:

	PHA		; Push A.
	
;40 POKE 784, DX+IX:POKE 785, DY+IY
;50 PLOT
;60 POKE 784, DY+IX:POKE 785, DX+IY
;70 PLOT

;1.
	CLC
	LDA		DX
	ADC		IX
	STA		X1
	CLC
	LDA		DY
	ADC		IY
	STA		Y1
	JSR 	PLOT2	

;2.
	CLC
	LDA		DY
	ADC		IX
	STA		X1
	CLC
	LDA		DX
	ADC		IY
	STA		Y1
	JSR 	PLOT2	

;80 POKE 784, IX-DX:POKE 785, IY+DY
;90 PLOT
;100 POKE 784, IX-DY:POKE 785, IY+DX
;110 PLOT

;3.
	LDA		IX
	SEC
	SBC		DX
	STA		X1
	CLC
	LDA		IY
	ADC		DY
	STA		Y1
	JSR 	PLOT2	

;4.
	LDA		IX
	SEC
	SBC		DY
	STA		X1
	CLC
	LDA		IY
	ADC		DX
	STA		Y1
	JSR 	PLOT2	

;120 POKE 784, IX-DX:POKE 785, IY-DY
;130 PLOT
;140 POKE 784, IX-DY:POKE 785, IY-DX
;150 PLOT

;5.
	LDA		IX
	SEC
	SBC		DX
	STA		X1
	LDA		IY
	SEC
	SBC		DY	
	STA		Y1
	JSR 	PLOT2	

;6.
	LDA		IX
	SEC
	SBC		DY
	STA		X1
	LDA		IY
	SEC
	SBC		DX	
	STA		Y1
	JSR 	PLOT2

;160 POKE 784, IX+DX:POKE 785, IY-DY
;170 PLOT
;180 POKE 784, IX+DY:POKE 785, IY-DX
;190 PLOT

;7.
	CLC
	LDA		IX
	ADC		DX
	STA		X1
	LDA		IY
	SEC
	SBC		DY	
	STA		Y1
	JSR 	PLOT2

;8.
	CLC
	LDA		IX
	ADC		DY
	STA		X1
	LDA		IY
	SEC
	SBC		DX	
	STA		Y1
	JSR 	PLOT2

	PLA		; Pop A.
	RTS

; =============================================================================
; "WAIT" STATEMENT
; =============================================================================

WAIT:
        jsr     GTNUM
        stx     FORPNT
        ldx     #$00
        jsr     CHRGOT
        beq     L3628
        jsr     COMBYTE
L3628:
        stx     FORPNT+1
        ldy     #$00
L362C:
        lda     (LINNUM),y
        eor     FORPNT+1
        and     FORPNT
        beq     L362C
RTS3:
        rts
TEMP1X = TEMP1+(5-BYTES_FP)
; =============================================================================
; ADD 0.5 TO FAC
; =============================================================================

FADDH:
        lda     #<CON_HALF
        ldy     #>CON_HALF
        jmp     FADD

; =============================================================================
; FAC = (Y,A) - FAC
; =============================================================================

FSUB:
        jsr     LOAD_ARG_FROM_YA

; =============================================================================
; FAC = ARG - FAC
; =============================================================================

FSUBT:
        lda     FACSIGN
        eor     #$FF
        sta     FACSIGN
        eor     ARGSIGN
        sta     SGNCPR
        lda     FAC
        jmp     FADDT

; =============================================================================
; SHIFT SMALLER ARGUMENT MORE THAN 7 BITS
; =============================================================================

FADD1:
        jsr     SHIFT_RIGHT
        bcc     FADD3

; =============================================================================
; FAC = (Y,A) + FAC
; =============================================================================

FADD:
        jsr     LOAD_ARG_FROM_YA

; =============================================================================
; FAC = ARG + FAC
; =============================================================================

FADDT:
        bne     L365B
        jmp     COPY_ARG_TO_FAC
L365B:
        ldx     FACEXTENSION
        stx     ARGEXTENSION
        ldx     #ARG
        lda     ARG
FADD2:
        tay
        beq     RTS3
        sec
        sbc     FAC
        beq     FADD3
        bcc     L367F
        sty     FAC
        ldy     ARGSIGN
        sty     FACSIGN
        eor     #$FF
        adc     #$00
        ldy     #$00
        sty     ARGEXTENSION
        ldx     #FAC
        bne     L3683
L367F:
        ldy     #$00
        sty     FACEXTENSION
L3683:
        cmp     #$F9
        bmi     FADD1
        tay
        lda     FACEXTENSION
        lsr     1,x
        jsr     SHIFT_RIGHT4
FADD3:
        bit     SGNCPR
        bpl     FADD4
        ldy     #FAC
        cpx     #ARG
        beq     L369B
        ldy     #ARG
L369B:
        sec
        eor     #$FF
        adc     ARGEXTENSION
        sta     FACEXTENSION
        lda     3,y
        sbc     3,x
        sta     FAC+3
        lda     2,y
        sbc     2,x
        sta     FAC+2
        lda     1,y
        sbc     1,x
        sta     FAC+1

; =============================================================================
; NORMALIZE VALUE IN FAC
; =============================================================================

NORMALIZE_FAC1:
        bcs     NORMALIZE_FAC2
        jsr     COMPLEMENT_FAC
NORMALIZE_FAC2:
        ldy     #$00
        tya
        clc
L36C7:
        ldx     FAC+1
        bne     NORMALIZE_FAC4
        ldx     FAC+2
        stx     FAC+1
        ldx     FAC+3
        stx     FAC+2
        ldx     FACEXTENSION
        stx     FAC+3
        sty     FACEXTENSION
        adc     #$08
; bugfix?
; fix does not exist on AppleSoft 2
        cmp     #MANTISSA_BYTES*8
        bne     L36C7

; =============================================================================
; SET FAC = 0
; (ONLY NECESSARY TO ZERO EXPONENT AND SIGN CELLS)
; =============================================================================

ZERO_FAC:
        lda     #$00
STA_IN_FAC_SIGN_AND_EXP:
        sta     FAC
STA_IN_FAC_SIGN:
        sta     FACSIGN
        rts

; =============================================================================
; ADD MANTISSAS OF FAC AND ARG INTO FAC
; =============================================================================

FADD4:
        adc     ARGEXTENSION
        sta     FACEXTENSION
        lda     FAC+3
        adc     ARG+3
        sta     FAC+3
        lda     FAC+2
        adc     ARG+2
        sta     FAC+2
        lda     FAC+1
        adc     ARG+1
        sta     FAC+1
        jmp     NORMALIZE_FAC5

; =============================================================================
; FINISH NORMALIZING FAC
; =============================================================================

NORMALIZE_FAC3:
        adc     #$01
        asl     FACEXTENSION
        rol     FAC+3
        rol     FAC+2
        rol     FAC+1
NORMALIZE_FAC4:
        bpl     NORMALIZE_FAC3
        sec
        sbc     FAC
        bcs     ZERO_FAC
        eor     #$FF
        adc     #$01
        sta     FAC
NORMALIZE_FAC5:
        bcc     L3764
NORMALIZE_FAC6:
        inc     FAC
        beq     OVERFLOW
        ror     FAC+1
        ror     FAC+2
        ror     FAC+3
        ror     FACEXTENSION
L3764:
        rts

; =============================================================================
; 2'S COMPLEMENT OF FAC
; =============================================================================

COMPLEMENT_FAC:
        lda     FACSIGN
        eor     #$FF
        sta     FACSIGN

; =============================================================================
; 2'S COMPLEMENT OF FAC MANTISSA ONLY
; =============================================================================

COMPLEMENT_FAC_MANTISSA:
        lda     FAC+1
        eor     #$FF
        sta     FAC+1
        lda     FAC+2
        eor     #$FF
        sta     FAC+2
        lda     FAC+3
        eor     #$FF
        sta     FAC+3
        lda     FACEXTENSION
        eor     #$FF
        sta     FACEXTENSION
        inc     FACEXTENSION
        bne     RTS12

; =============================================================================
; INCREMENT FAC MANTISSA
; =============================================================================

INCREMENT_FAC_MANTISSA:
        inc     FAC+3
        bne     RTS12
        inc     FAC+2
        bne     RTS12
        inc     FAC+1
RTS12:
        rts
OVERFLOW:
        ldx     #ERR_OVERFLOW
        jmp     ERROR

; =============================================================================
; SHIFT 1,X THRU 5,X RIGHT
; (A) = NEGATIVE OF SHIFT COUNT
; (X) = POINTER TO BYTES TO BE SHIFTED
;
; RETURN WITH (Y)=0, CARRY=0, EXTENSION BITS IN A-REG
; =============================================================================

SHIFT_RIGHT1:
        ldx     #RESULT-1
SHIFT_RIGHT2:
        ldy     3,x
        sty     FACEXTENSION
        ldy     2,x
        sty     3,x
        ldy     1,x
        sty     2,x
        ldy     SHIFTSIGNEXT
        sty     1,x

; =============================================================================
; MAIN ENTRY TO RIGHT SHIFT SUBROUTINE
; =============================================================================

SHIFT_RIGHT:
        adc     #$08
        bmi     SHIFT_RIGHT2
        beq     SHIFT_RIGHT2
        sbc     #$08
        tay
        lda     FACEXTENSION
        bcs     SHIFT_RIGHT5
LB588:
        asl     1,x
        bcc     LB58E
        inc     1,x
LB58E:
        ror     1,x
        ror     1,x

; =============================================================================
; ENTER HERE FOR SHORT SHIFTS WITH NO SIGN EXTENSION
; =============================================================================

SHIFT_RIGHT4:
        ror     2,x
        ror     3,x
        ror     a
        iny
        bne     LB588
SHIFT_RIGHT5:
        clc
        rts

; =============================================================================

CON_ONE:
        .byte   $81,$00,$00,$00
POLY_LOG:
        .byte    $02
        .byte   $80,$19,$56,$62
        .byte   $80,$76,$22,$F3
        .byte   $82,$38,$AA,$40
CON_SQR_HALF:
        .byte   $80,$35,$04,$F3
CON_SQR_TWO:
        .byte   $81,$35,$04,$F3
CON_NEG_HALF:
        .byte   $80,$80,$00,$00
CON_LOG_TWO:
        .byte   $80,$31,$72,$18

; =============================================================================
; "LOG" FUNCTION
; =============================================================================

LOG:
        jsr     SIGN
        beq     GIQ
        bpl     LOG2
GIQ:
        jmp     IQERR
LOG2:
        lda     FAC
        sbc     #$7F
        pha
        lda     #$80
        sta     FAC
        lda     #<CON_SQR_HALF
        ldy     #>CON_SQR_HALF
        jsr     FADD
        lda     #<CON_SQR_TWO
        ldy     #>CON_SQR_TWO
        jsr     FDIV
        lda     #<CON_ONE
        ldy     #>CON_ONE
        jsr     FSUB
        lda     #<POLY_LOG
        ldy     #>POLY_LOG
        jsr     POLYNOMIAL_ODD
        lda     #<CON_NEG_HALF
        ldy     #>CON_NEG_HALF
        jsr     FADD
        pla
        jsr     ADDACC
        lda     #<CON_LOG_TWO
        ldy     #>CON_LOG_TWO

; =============================================================================
; FAC = (Y,A) * FAC
; =============================================================================

FMULT:
        jsr     LOAD_ARG_FROM_YA

; =============================================================================
; FAC = ARG * FAC
; =============================================================================

FMULTT:
        beq     L3903
        jsr     ADD_EXPONENTS
        lda     #$00
        sta     RESULT
        sta     RESULT+1
        sta     RESULT+2
        lda     FACEXTENSION
        jsr     MULTIPLY1
        lda     FAC+3
        jsr     MULTIPLY1
        lda     FAC+2
        jsr     MULTIPLY1
        lda     FAC+1
        jsr     MULTIPLY2
        jmp     COPY_RESULT_INTO_FAC

; =============================================================================
; MULTIPLY ARG BY (A) INTO RESULT
; =============================================================================

MULTIPLY1:
        bne     MULTIPLY2
        jmp     SHIFT_RIGHT1
MULTIPLY2:
        lsr     a
        ora     #$80
L38A7:
        tay
        bcc     L38C3
        clc
        lda     RESULT+2
        adc     ARG+3
        sta     RESULT+2
        lda     RESULT+1
        adc     ARG+2
        sta     RESULT+1
        lda     RESULT
        adc     ARG+1
        sta     RESULT
L38C3:
        ror     RESULT
        ror     RESULT+1
; this seems to be a bad byte in the dump
        ror     RESULT+2
        ror     FACEXTENSION
        tya
        lsr     a
        bne     L38A7
L3903:
        rts

; =============================================================================
; UNPACK NUMBER AT (Y,A) INTO ARG
; =============================================================================

LOAD_ARG_FROM_YA:
        sta     INDEX
        sty     INDEX+1
        ldy     #BYTES_FP-1
        lda     (INDEX),y
        sta     ARG+3
        dey
        lda     (INDEX),y
        sta     ARG+2
        dey
        lda     (INDEX),y
        sta     ARGSIGN
        eor     FACSIGN
        sta     SGNCPR
        lda     ARGSIGN
        ora     #$80
        sta     ARG+1
        dey
        lda     (INDEX),y
        sta     ARG
        lda     FAC
        rts

; =============================================================================
; ADD EXPONENTS OF ARG AND FAC
; (CALLED BY FMULT AND FDIV)
;
; ALSO CHECK FOR OVERFLOW, AND SET RESULT SIGN
; =============================================================================

ADD_EXPONENTS:
        lda     ARG
ADD_EXPONENTS1:
        beq     ZERO
        clc
        adc     FAC
        bcc     L393C
        bmi     JOV
        clc
        .byte   $2C
L393C:
        bpl     ZERO
        adc     #$80
        sta     FAC
        bne     L3947
        jmp     STA_IN_FAC_SIGN
L3947:
        lda     SGNCPR
        sta     FACSIGN
        rts

; =============================================================================
; IF (FAC) IS POSITIVE, GIVE "OVERFLOW" ERROR
; IF (FAC) IS NEGATIVE, SET FAC=0, POP ONE RETURN, AND RTS
; CALLED FROM "EXP" FUNCTION
; =============================================================================

OUTOFRNG:
        lda     FACSIGN
        eor     #$FF
        bmi     JOV

; =============================================================================
; POP RETURN ADDRESS AND SET FAC=0
; =============================================================================

ZERO:
        pla
        pla
        jmp     ZERO_FAC
JOV:
        jmp     OVERFLOW

; =============================================================================
; MULTIPLY FAC BY 10
; =============================================================================

MUL10:
        jsr     COPY_FAC_TO_ARG_ROUNDED
        tax
        beq     L3970
        clc
        adc     #$02
        bcs     JOV
        ldx     #$00
        stx     SGNCPR
        jsr     FADD2
        inc     FAC
        beq     JOV
L3970:
        rts

; =============================================================================
CONTEN:
        .byte   $84,$20,$00,$00

; =============================================================================
; DIVIDE FAC BY 10
; =============================================================================

DIV10:
        jsr     COPY_FAC_TO_ARG_ROUNDED
        lda     #<CONTEN
        ldy     #>CONTEN
        ldx     #$00

; =============================================================================
; FAC = ARG / (Y,A)
; =============================================================================

DIV:
        stx     SGNCPR
        jsr     LOAD_FAC_FROM_YA
        jmp     FDIVT

; =============================================================================
; FAC = (Y,A) / FAC
; =============================================================================

FDIV:
        jsr     LOAD_ARG_FROM_YA
; ----------------------------------------------------------------------------
; FAC = ARG / FAC
; ----------------------------------------------------------------------------
FDIVT:
        beq     L3A02
        jsr     ROUND_FAC
        lda     #$00
        sec
        sbc     FAC
        sta     FAC
        jsr     ADD_EXPONENTS
        inc     FAC
        beq     JOV
        ldx     #-MANTISSA_BYTES
        lda     #$01
L39A1:
        ldy     ARG+1
        cpy     FAC+1
        bne     L39B7
        ldy     ARG+2
        cpy     FAC+2
        bne     L39B7
        ldy     ARG+3
        cpy     FAC+3
L39B7:
        php
        rol     a
        bcc     L39C4
        inx
        sta     RESULT_LAST-1,x
        beq     L39F2
        bpl     L39F6
        lda     #$01
L39C4:
        plp
        bcs     L39D5
L39C7:
        asl     ARG_LAST
        rol     ARG+2
        rol     ARG+1
        bcs     L39B7
        bmi     L39A1
        bpl     L39B7
L39D5:
        tay
        lda     ARG+3
        sbc     FAC+3
        sta     ARG+3
        lda     ARG+2
        sbc     FAC+2
        sta     ARG+2
        lda     ARG+1
        sbc     FAC+1
        sta     ARG+1
        tya
        jmp     L39C7
L39F2:
        lda     #$40
        bne     L39C4
L39F6:
        asl     a
        asl     a
        asl     a
        asl     a
        asl     a
        asl     a
        sta     FACEXTENSION
        plp
        jmp     COPY_RESULT_INTO_FAC
L3A02:
        ldx     #ERR_ZERODIV
        jmp     ERROR

; =============================================================================
; COPY RESULT INTO FAC MANTISSA, AND NORMALIZE
; =============================================================================

COPY_RESULT_INTO_FAC:
        lda     RESULT
        sta     FAC+1
        lda     RESULT+1
        sta     FAC+2
        lda     RESULT+2
        sta     FAC+3
        jmp     NORMALIZE_FAC2

; =============================================================================
; UNPACK (Y,A) INTO FAC
; =============================================================================

LOAD_FAC_FROM_YA:
        sta     INDEX
        sty     INDEX+1
        ldy     #MANTISSA_BYTES
        lda     (INDEX),y
        sta     FAC+3
        dey
        lda     (INDEX),y
        sta     FAC+2
        dey
        lda     (INDEX),y
        sta     FACSIGN
        ora     #$80
        sta     FAC+1
        dey
        lda     (INDEX),y
        sta     FAC
        sty     FACEXTENSION
        rts

; =============================================================================
; ROUND FAC, STORE IN TEMP2
; =============================================================================

STORE_FAC_IN_TEMP2_ROUNDED:
        ldx     #TEMP2
        .byte   $2C

; =============================================================================
; ROUND FAC, STORE IN TEMP1
; =============================================================================

STORE_FAC_IN_TEMP1_ROUNDED:
        ldx     #TEMP1X
        ldy     #$00
        beq     STORE_FAC_AT_YX_ROUNDED

; =============================================================================
; ROUND FAC, AND STORE WHERE FORPNT POINTS
; =============================================================================

SETFOR:
        ldx     FORPNT
        ldy     FORPNT+1

; =============================================================================
; ROUND FAC, AND STORE AT (Y,X)
; =============================================================================

STORE_FAC_AT_YX_ROUNDED:
        jsr     ROUND_FAC
        stx     INDEX
        sty     INDEX+1
        ldy     #MANTISSA_BYTES
        lda     FAC+3
        sta     (INDEX),y
        dey
        lda     FAC+2
        sta     (INDEX),y
        dey
        lda     FACSIGN
        ora     #$7F
        and     FAC+1
        sta     (INDEX),y
        dey
        lda     FAC
        sta     (INDEX),y
        sty     FACEXTENSION
        rts

; =============================================================================
; COPY ARG INTO FAC
; =============================================================================

COPY_ARG_TO_FAC:
        lda     ARGSIGN
MFA:
        sta     FACSIGN
        ldx     #BYTES_FP
L3A7A:
        lda     SHIFTSIGNEXT,x
        sta     EXPSGN,x
        dex
        bne     L3A7A
        stx     FACEXTENSION
        rts

; =============================================================================
; ROUND FAC AND COPY TO ARG
; =============================================================================

COPY_FAC_TO_ARG_ROUNDED:
        jsr     ROUND_FAC
MAF:
        ldx     #BYTES_FP+1
L3A89:
        lda     EXPSGN,x
        sta     SHIFTSIGNEXT,x
        dex
        bne     L3A89
        stx     FACEXTENSION
RTS14:
        rts

; =============================================================================
; ROUND FAC USING EXTENSION BYTE
; =============================================================================

ROUND_FAC:
        lda     FAC
        beq     RTS14
        asl     FACEXTENSION
        bcc     RTS14

; =============================================================================
; INCREMENT MANTISSA AND RE-NORMALIZE IF CARRY
; =============================================================================

INCREMENT_MANTISSA:
        jsr     INCREMENT_FAC_MANTISSA
        bne     RTS14
        jmp     NORMALIZE_FAC6

; =============================================================================
; TEST FAC FOR ZERO AND SIGN
;
; FAC > 0, RETURN +1
; FAC = 0, RETURN  0
; FAC < 0, RETURN -1
; =============================================================================

SIGN:
        lda     FAC
        beq     RTS15
L3AA7:
        lda     FACSIGN
SIGN2:
        rol     a
        lda     #$FF
        bcs     RTS15
        lda     #$01
RTS15:
        rts

; =============================================================================
; "SGN" FUNCTION
; =============================================================================

SGN:
        JSR     SIGN
        

; =============================================================================
; CONVERT (A) INTO FAC, AS SIGNED VALUE -128 TO +127
; =============================================================================

FLOAT:
        sta     FAC+1
        lda     #$00
        sta     FAC+2
        ldx     #$88

; =============================================================================
; FLOAT UNSIGNED VALUE IN FAC+1,2
; (X) = EXPONENT
; =============================================================================

FLOAT1:
        lda     FAC+1
        eor     #$FF
        rol     a

; =============================================================================
; FLOAT UNSIGNED VALUE IN FAC+1,2
; (X) = EXPONENT
; C=0 TO MAKE VALUE NEGATIVE
; C=1 TO MAKE VALUE POSITIVE
; =============================================================================

FLOAT2:
        lda     #$00
        sta     FAC+3
        stx     FAC
        sta     FACEXTENSION
        sta     FACSIGN
        jmp     NORMALIZE_FAC1

; =============================================================================
; "ABS" FUNCTION
; =============================================================================

ABS:
        lsr     FACSIGN
        rts

; =============================================================================
; SETCUR
; SJ Added Apr 2014.
; c_val contains the value to set to cursor to. 0 = off.
; =============================================================================

SETCUR:
        
	LDA     #$02            		; Set cursor command.
    STA		char_to_send
    JSR     Send_Char_Screen
    
    LDA		c_val					; Value from c_val.				
    STA		char_to_send
    JSR     Send_Char_Screen
    
    LDA     #$03            		; Set cursor to blink.
    STA		char_to_send
    JSR     Send_Char_Screen    
    
    RTS
        
; =============================================================================
; FONT
; SJ Added Apr 2014.
; c_val contains the value to set the font to.
; 40 Character normal = $00
; 80 Character normal = $01 
; 40 Character bold = $02
; 80 Character bold = $03
; 40 Character normal double-height = $04
; 80 Character normal double-height = $05
; 40 Character bold double-height = $06
; 80 Character bold double-height = $07
; =============================================================================

FONT:        
	LDA     #$18            		; Set cursor command.
    STA		char_to_send
    JSR     Send_Char_Screen
      
    LDA 	#$07					; If c_val <= $07 branch.
    CMP 	c_val
    BCS  	FONT_1
    LDA		#$03					; Else default to 80 column, bold.
    JMP		FONT_2
FONT_1:   			           	
   	LDA		c_val					; Value from c_val.	
FONT_2:   			           	    
    STA		char_to_send
    JSR     Send_Char_Screen    
    
    RTS
    
; =============================================================================
; COMPARE FAC WITH PACKED # AT (Y,A)
; RETURN A=1,0,-1 AS (Y,A) IS <,=,> FAC
; =============================================================================

FCOMP:
        sta     DEST

; =============================================================================
; SPECIAL ENTRY FROM "NEXT" PROCESSOR
; "DEST" ALREADY SET UP
; =============================================================================

FCOMP2:
        sty     DEST+1
        ldy     #$00
        lda     (DEST),y
        iny
        tax
        beq     SIGN
        lda     (DEST),y
        eor     FACSIGN
        bmi     L3AA7
        cpx     FAC
        bne     L3B0A
        lda     (DEST),y
        ora     #$80
        cmp     FAC+1
        bne     L3B0A
        iny
        lda     (DEST),y
        cmp     FAC+2
        bne     L3B0A
        iny
        lda     #$7F
        cmp     FACEXTENSION
        lda     (DEST),y
        sbc     FAC_LAST
        beq     L3B32
L3B0A:
        lda     FACSIGN
        bcc     L3B10
        eor     #$FF
L3B10:
        jmp     SIGN2

; =============================================================================
; QUICK INTEGER FUNCTION
;
; CONVERTS FP VALUE IN FAC TO INTEGER VALUE
; IN FAC+1...FAC+4, BY SHIFTING RIGHT WITH SIGN
; EXTENSION UNTIL FRACTIONAL BITS ARE OUT.
;
; THIS SUBROUTINE ASSUMES THE EXPONENT < 32.
; =============================================================================

QINT:
        lda     FAC
        beq     QINT3
        sec
        sbc     #120+8*BYTES_FP
        bit     FACSIGN
        bpl     L3B27
        tax
        lda     #$FF
        sta     SHIFTSIGNEXT
        jsr     COMPLEMENT_FAC_MANTISSA
        txa
L3B27:
        ldx     #FAC
        cmp     #$F9
        bpl     QINT2
        jsr     SHIFT_RIGHT
        sty     SHIFTSIGNEXT
L3B32:
        rts
QINT2:
        tay
        lda     FACSIGN
        and     #$80
        lsr     FAC+1
        ora     FAC+1
        sta     FAC+1
        jsr     SHIFT_RIGHT4
        sty     SHIFTSIGNEXT
        rts

; =============================================================================
; "INT" FUNCTION
;
; USES QINT TO CONVERT (FAC) TO INTEGER FORM,
; AND THEN REFLOATS THE INTEGER.
; =============================================================================

INT:
        lda     FAC
        cmp     #120+8*BYTES_FP
        bcs     RTS17
        jsr     QINT
        sty     FACEXTENSION
        lda     FACSIGN
        sty     FACSIGN
        eor     #$80
        rol     a
        lda     #120+8*BYTES_FP
        sta     FAC
        lda     FAC_LAST
        sta     CHARAC
        jmp     NORMALIZE_FAC1
QINT3:
        sta     FAC+1
        sta     FAC+2
        sta     FAC+3
        tay
RTS17:
        rts

; =============================================================================
; CONVERT STRING TO FP VALUE IN FAC
;
; STRING POINTED TO BY TXTPTR
; FIRST CHAR ALREADY SCANNED BY CHRGET
; (A) = FIRST CHAR, C=0 IF DIGIT.
; =============================================================================

FIN:
        ldy     #$00
        ldx     #SERLEN-TMPEXP
L3B6F:
        sty     TMPEXP,x
        dex
        bpl     L3B6F
        bcc     FIN2
        cmp     #$2D
        bne     L3B7E
        stx     SERLEN
        beq     FIN1
L3B7E:
        cmp     #$2B
        bne     FIN3
FIN1:
        jsr     CHRGET
FIN2:
        bcc     FIN9
FIN3:
        cmp     #$2E
        beq     FIN10
        cmp     #$45
        bne     FIN7
        jsr     CHRGET
        bcc     FIN5
        cmp     #TOKEN_MINUS
        beq     L3BA6
        cmp     #$2D
        beq     L3BA6
        cmp     #TOKEN_PLUS
        beq     FIN4
        cmp     #$2B
        beq     FIN4
        bne     FIN6
L3BA6:
        ror     EXPSGN
FIN4:
        jsr     CHRGET
FIN5:
        bcc     GETEXP
FIN6:
        bit     EXPSGN
        bpl     FIN7
        lda     #$00
        sec
        sbc     EXPON
        jmp     FIN8

; =============================================================================
; FOUND A DECIMAL POINT
; =============================================================================

FIN10:
        ror     LOWTR
        bit     LOWTR
        bvc     FIN1

; =============================================================================
; NUMBER TERMINATED, ADJUST EXPONENT NOW
; =============================================================================

FIN7:
        lda     EXPON
FIN8:
        sec
        sbc     INDX
        sta     EXPON
        beq     L3BEE
        bpl     L3BE7
L3BDE:
        jsr     DIV10
        inc     EXPON
        bne     L3BDE
        beq     L3BEE
L3BE7:
        jsr     MUL10
        dec     EXPON
        bne     L3BE7
L3BEE:
        lda     SERLEN
        bmi     L3BF3
        rts
L3BF3:
        jmp     NEGOP

; =============================================================================
; ACCUMULATE A DIGIT INTO FAC
; =============================================================================

FIN9:
        pha
        bit     LOWTR
        bpl     L3BFD
        inc     INDX
L3BFD:
        jsr     MUL10
        pla
        sec
        sbc     #$30
        jsr     ADDACC
        jmp     FIN1

; =============================================================================
; ADD (A) TO FAC
; =============================================================================

ADDACC:
        pha
        jsr     COPY_FAC_TO_ARG_ROUNDED
        pla
        jsr     FLOAT
        lda     ARGSIGN
        eor     FACSIGN
        sta     SGNCPR
        ldx     FAC
        jmp     FADDT

; =============================================================================
; ACCUMULATE DIGIT OF EXPONENT
; =============================================================================

GETEXP:
        lda     EXPON
        cmp     #MAX_EXPON
        bcc     L3C2C
        lda     #$64
        bit     EXPSGN
        bmi     L3C3A
        jmp     OVERFLOW
L3C2C:
        asl     a
        asl     a
        clc
        adc     EXPON
        asl     a
        clc
        ldy     #$00
        adc     (TXTPTR),y
        sec
        sbc     #$30
L3C3A:
        sta     EXPON
        jmp     FIN4

; =============================================================================
; these values are /1000 of what the labels say
CON_99999999_9:
        .byte   $91,$43,$4F,$F8
CON_999999999:
        .byte   $94,$74,$23,$F7
CON_BILLION:
        .byte   $94,$74,$24,$00

; =============================================================================
; PRINT "IN <LINE #>"
; =============================================================================

INPRT:
        lda     #<QT_IN
        ldy     #>QT_IN
        jsr     GOSTROUT2
        lda     CURLIN+1
        ldx     CURLIN

; =============================================================================
; PRINT A,X AS DECIMAL INTEGER
; =============================================================================

LINPRT:
        sta     FAC+1
        stx     FAC+2
        ldx     #$90
        sec
        jsr     FLOAT2
        jsr     FOUT
GOSTROUT2:
        jmp     STROUT

; =============================================================================
; CONVERT (FAC) TO STRING STARTING AT STACK
; RETURN WITH (Y,A) POINTING AT STRING
; =============================================================================

FOUT:
        ldy     #$01

; =============================================================================
; "STR$" FUNCTION ENTERS HERE, WITH (Y)=0
; SO THAT RESULT STRING STARTS AT STACK-1
; (THIS IS USED AS A FLAG)
; =============================================================================

FOUT1:
        lda     #$20
        bit     FACSIGN
        bpl     L3C73
        lda     #$2D
L3C73:
        sta     $FF,y
        sta     FACSIGN
        sty     STRNG2
        iny
        lda     #$30
        ldx     FAC
        bne     L3C84
        jmp     FOUT4
L3C84:
        lda     #$00
        cpx     #$80
        beq     L3C8C
        bcs     L3C95
L3C8C:
        lda     #<CON_BILLION
        ldy     #>CON_BILLION
        jsr     FMULT
        lda     #-6 ; exponent adjustment
L3C95:
        sta     INDX

; =============================================================================
; ADJUST UNTIL 1E8 <= (FAC) <1E9
; =============================================================================

L3C97:
        lda     #<CON_999999999
        ldy     #>CON_999999999
        jsr     FCOMP
        beq     L3CBE
        bpl     L3CB4
L3CA2:
        lda     #<CON_99999999_9
        ldy     #>CON_99999999_9
        jsr     FCOMP
        beq     L3CAD
        bpl     L3CBB
L3CAD:
        jsr     MUL10
        dec     INDX
        bne     L3CA2
L3CB4:
        jsr     DIV10
        inc     INDX
        bne     L3C97
L3CBB:
        jsr     FADDH
L3CBE:
        jsr     QINT

; =============================================================================
; FAC+1...FAC+4 IS NOW IN INTEGER FORM
; WITH POWER OF TEN ADJUSTMENT IN TMPEXP
;
; IF -10 < TMPEXP > 1, PRINT IN DECIMAL FORM
; OTHERWISE, PRINT IN EXPONENTIAL FORM
; =============================================================================

        ldx     #$01
        lda     INDX
        clc
        adc     #3*BYTES_FP-5
        bmi     L3CD3
        cmp     #3*BYTES_FP-4
        bcs     L3CD4
        adc     #$FF
        tax
        lda     #$02
L3CD3:
        sec
L3CD4:
        sbc     #$02
        sta     EXPON
        stx     INDX
        txa
        beq     L3CDF
        bpl     L3CF2
L3CDF:
        ldy     STRNG2
        lda     #$2E
        iny
        sta     $FF,y
        txa
        beq     L3CF0
        lda     #$30
        iny
        sta     $FF,y
L3CF0:
        sty     STRNG2

; =============================================================================
; NOW DIVIDE BY POWERS OF TEN TO GET SUCCESSIVE DIGITS
; =============================================================================

L3CF2:
        ldy     #$00
        ldx     #$80
L3CF6:
        lda     FAC_LAST
        clc
        adc     DECTBL+2,y
        sta     FAC+3
        lda     FAC+2
        adc     DECTBL+1,y
        sta     FAC+2
        lda     FAC+1
        adc     DECTBL,y
        sta     FAC+1
        inx
        bcs     L3D1A
        bpl     L3CF6
        bmi     L3D1C
L3D1A:
        bmi     L3CF6
L3D1C:
        txa
        bcc     L3D23
        eor     #$FF
        adc     #$0A
L3D23:
        adc     #$2F
        iny
        iny
        iny
        sty     VARPNT
        ldy     STRNG2
        iny
        tax
        and     #$7F
        sta     $FF,y
        dec     INDX
        bne     L3D3E
        lda     #$2E
        iny
        sta     $FF,y
L3D3E:
        sty     STRNG2
        ldy     VARPNT
        txa
        eor     #$FF
        and     #$80
        tax
        cpy     #DECTBL_END-DECTBL
        bne     L3CF6

; =============================================================================
; NINE DIGITS HAVE BEEN STORED IN STRING.  NOW LOOK
; BACK AND LOP OFF TRAILING ZEROES AND A TRAILING
; DECIMAL POINT.
; =============================================================================

        ldy     STRNG2
L3D4E:
        lda     $FF,y
        dey
        cmp     #$30
        beq     L3D4E
        cmp     #$2E
        beq     L3D5B
        iny
L3D5B:
        lda     #$2B
        ldx     EXPON
        beq     L3D8F
        bpl     L3D6B
        lda     #$00
        sec
        sbc     EXPON
        tax
        lda     #$2D
L3D6B:
        sta     STACK+1,y
        lda     #$45
        sta     STACK,y
        txa
        ldx     #$2F
        sec
L3D77:
        inx
        sbc     #$0A
        bcs     L3D77
        adc     #$3A
        sta     STACK+3,y
        txa
        sta     STACK+2,y
        lda     #$00
        sta     STACK+4,y
        beq     L3D94
FOUT4:
        sta     $FF,y
L3D8F:
        lda     #$00
        sta     STACK,y
L3D94:
        lda     #$00
        ldy     #$01
        rts

; =============================================================================
CON_HALF:
        .byte   $80,$00,$00,$00

; =============================================================================
; POWERS OF 10 FROM 1E8 DOWN TO 1,
; AS 32-BIT INTEGERS, WITH ALTERNATING SIGNS
; =============================================================================

DECTBL:
        .byte   $FE,$79,$60 ; -100000
        .byte    $00,$27,$10 ; 10000
        .byte    $FF,$FC,$18 ; -1000
        .byte    $00,$00,$64 ; 100
        .byte    $FF,$FF,$F6 ; -10
        .byte    $00,$00,$01 ; 1
DECTBL_END:

; =============================================================================
; "SQR" FUNCTION
; =============================================================================

SQR:
        jsr     COPY_FAC_TO_ARG_ROUNDED
        lda     #<CON_HALF
        ldy     #>CON_HALF
        jsr     LOAD_FAC_FROM_YA

; =============================================================================
; EXPONENTIATION OPERATION
;
; ARG ^ FAC  =  EXP( LOG(ARG) * FAC )
; =============================================================================

FPWRT:
        beq     EXP
        lda     ARG
        bne     L3DD5
        jmp     STA_IN_FAC_SIGN_AND_EXP
L3DD5:
        ldx     #TEMP3
        ldy     #$00
        jsr     STORE_FAC_AT_YX_ROUNDED
        lda     ARGSIGN
        bpl     L3DEF
        jsr     INT
        lda     #TEMP3
        ldy     #$00
        jsr     FCOMP
        bne     L3DEF
        tya
        ldy     CHARAC
L3DEF:
        jsr     MFA
        tya
        pha
        jsr     LOG
        lda     #TEMP3
        ldy     #$00
        jsr     FMULT
        jsr     EXP
        pla
        lsr     a
        bcc     L3E0F

; =============================================================================
; NEGATE VALUE IN FAC
; =============================================================================

NEGOP:
        lda     FAC
        beq     L3E0F
        lda     FACSIGN
        eor     #$FF
        sta     FACSIGN
L3E0F:
        rts

; =============================================================================

CON_LOG_E:
        .byte   $81,$38,$AA,$3B
POLY_EXP:
        .byte    $06
        .byte    $74,$63,$90,$8C
        .byte    $77,$23,$0C,$AB
        .byte    $7A,$1E,$94,$00
        .byte    $7C,$63,$42,$80
        .byte    $7E,$75,$FE,$D0
        .byte    $80,$31,$72,$15
        .byte    $81,$00,$00,$00

; =============================================================================
; "EXP" FUNCTION
;
; FAC = E ^ FAC
; =============================================================================

EXP:
        lda     #<CON_LOG_E
        ldy     #>CON_LOG_E
        jsr     FMULT
        lda     FACEXTENSION
        adc     #$50
        bcc     L3E4E
        jsr     INCREMENT_MANTISSA
L3E4E:
        sta     ARGEXTENSION
        jsr     MAF
        lda     FAC
        cmp     #$88
        bcc     L3E5C
L3E59:
        jsr     OUTOFRNG
L3E5C:
        jsr     INT
        lda     CHARAC
        clc
        adc     #$81
        beq     L3E59
        sec
        sbc     #$01
        pha
        ldx     #BYTES_FP
L3E6C:
        lda     ARG,x
        ldy     FAC,x
        sta     FAC,x
        sty     ARG,x
        dex
        bpl     L3E6C
        lda     ARGEXTENSION
        sta     FACEXTENSION
        jsr     FSUBT
        jsr     NEGOP
        lda     #<POLY_EXP
        ldy     #>POLY_EXP
        jsr     POLYNOMIAL
        lda     #$00
        sta     SGNCPR
        pla
        jsr     ADD_EXPONENTS1
        rts

; =============================================================================
; ODD POLYNOMIAL SUBROUTINE
;
; F(X) = X * P(X^2)
;
; WHERE:  X IS VALUE IN FAC
;    Y,A POINTS AT COEFFICIENT TABLE
;    FIRST BYTE OF COEFF. TABLE IS N
;    COEFFICIENTS FOLLOW, HIGHEST POWER FIRST
;
; P(X^2) COMPUTED USING NORMAL POLYNOMIAL SUBROUTINE
; =============================================================================

POLYNOMIAL_ODD:
        sta     STRNG2
        sty     STRNG2+1
        jsr     STORE_FAC_IN_TEMP1_ROUNDED
        lda     #TEMP1X
        jsr     FMULT
        jsr     SERMAIN
        lda     #TEMP1X
        ldy     #$00
        jmp     FMULT

; =============================================================================
; NORMAL POLYNOMIAL SUBROUTINE
;
; P(X) = C(0)*X^N + C(1)*X^(N-1) + ... + C(N)
;
; WHERE:  X IS VALUE IN FAC
;    Y,A POINTS AT COEFFICIENT TABLE
;    FIRST BYTE OF COEFF. TABLE IS N
;    COEFFICIENTS FOLLOW, HIGHEST POWER FIRST
; =============================================================================

POLYNOMIAL:
        sta     STRNG2
        sty     STRNG2+1
SERMAIN:
        jsr     STORE_FAC_IN_TEMP2_ROUNDED
        lda     (STRNG2),y
        sta     SERLEN
        ldy     STRNG2
        iny
        tya
        bne     L3EBA
        inc     STRNG2+1
L3EBA:
        sta     STRNG2
        ldy     STRNG2+1
L3EBE:
        jsr     FMULT
        lda     STRNG2
        ldy     STRNG2+1
        clc
        adc     #BYTES_FP
        bcc     L3ECB
        iny
L3ECB:
        sta     STRNG2
        sty     STRNG2+1
        jsr     FADD
        lda     #TEMP2
        ldy     #$00
        dec     SERLEN
        bne     L3EBE
RTS19:
        rts

; =============================================================================
; "RND" FUNCTION
; =============================================================================

CONRND1:
        .byte   $98,$35,$44,$7A
CONRND2:
        .byte   $68,$28,$B1,$46
RND:
        jsr     SIGN
        tax
        bmi     L3F01
        lda     #<RNDSEED
        ldy     #>RNDSEED
        jsr     LOAD_FAC_FROM_YA
        txa
        beq     RTS19
        lda     #<CONRND1
        ldy     #>CONRND1
        jsr     FMULT
        lda     #<CONRND2
        ldy     #>CONRND2
        jsr     FADD
L3F01:
        ldx     FAC_LAST
        lda     FAC+1
        sta     FAC_LAST
        stx     FAC+1
        lda     #$00
        sta     FACSIGN
        lda     FAC
        sta     FACEXTENSION
        lda     #$80
        sta     FAC
        jsr     NORMALIZE_FAC2
        ldx     #<RNDSEED
        ldy     #>RNDSEED
GOMOVMF:
        jmp     STORE_FAC_AT_YX_ROUNDED

; =============================================================================
; "COS" FUNCTION
; =============================================================================

COS:
        lda     #<CON_PI_HALF
        ldy     #>CON_PI_HALF
        jsr     FADD

; =============================================================================
; "SIN" FUNCTION
; =============================================================================

SIN:
        jsr     COPY_FAC_TO_ARG_ROUNDED
        lda     #<CON_PI_DOUB
        ldy     #>CON_PI_DOUB
        ldx     ARGSIGN
        jsr     DIV
        jsr     COPY_FAC_TO_ARG_ROUNDED
        jsr     INT
        lda     #$00
        sta     STRNG1
        jsr     FSUBT

; =============================================================================
; (FAC) = ANGLE AS A FRACTION OF A FULL CIRCLE
;
; NOW FOLD THE RANGE INTO A QUARTER CIRCLE
;
; <<< THERE ARE MUCH SIMPLER WAYS TO DO THIS >>>
; =============================================================================

        lda     #<QUARTER
        ldy     #>QUARTER
        jsr     FSUB
        lda     FACSIGN
        pha
        bpl     SIN1
        jsr     FADDH
        lda     FACSIGN
        bmi     L3F5B
        lda     CPRMASK
        eor     #$FF
        sta     CPRMASK

; =============================================================================
; IF FALL THRU, RANGE IS 0...1/2
; IF BRANCH HERE, RANGE IS 0...1/4
; =============================================================================

SIN1:
        jsr     NEGOP

; =============================================================================
; IF FALL THRU, RANGE IS -1/2...0
; IF BRANCH HERE, RANGE IS -1/4...0
; =============================================================================

L3F5B:
        lda     #<QUARTER
        ldy     #>QUARTER
        jsr     FADD
        pla
        bpl     L3F68
        jsr     NEGOP
L3F68:
        lda     #<POLY_SIN
        ldy     #>POLY_SIN
        jmp     POLYNOMIAL_ODD

; =============================================================================
; "TAN" FUNCTION
;
; COMPUTE TAN(X) = SIN(X) / COS(X)
; =============================================================================

TAN:
        jsr     STORE_FAC_IN_TEMP1_ROUNDED
        lda     #$00
        sta     CPRMASK
        jsr     SIN
        ldx     #TEMP3
        ldy     #$00
        jsr     GOMOVMF
        lda     #TEMP1+(5-BYTES_FP)
        ldy     #$00
        jsr     LOAD_FAC_FROM_YA
        lda     #$00
        sta     FACSIGN
        lda     CPRMASK
        jsr     TAN1
        lda     #TEMP3
        ldy     #$00
        jmp     FDIV
TAN1:
        pha
        jmp     SIN1

; =============================================================================

CON_PI_HALF:
        .byte   $81,$49,$0F,$DB
CON_PI_DOUB:
        .byte   $83,$49,$0F,$DB
QUARTER:
        .byte   $7F,$00,$00,$00
POLY_SIN:
        .byte   $04,$86,$1E,$D7,$FB,$87,$99,$26
        .byte   $65,$87,$23,$34,$58,$86,$A5,$5D
        .byte   $E1,$83,$49,$0F,$DB

; =============================================================================
; "ATN" FUNCTION
; =============================================================================

ATN:
        lda     FACSIGN
        pha
        bpl     L3FDB
        jsr     NEGOP
L3FDB:
        lda     FAC
        pha
        cmp     #$81
        bcc     L3FE9
        lda     #<CON_ONE
        ldy     #>CON_ONE
        jsr     FDIV

; =============================================================================
; 0 <= X <= 1
; 0 <= ATN(X) <= PI/8
; =============================================================================

L3FE9:
        lda     #<POLY_ATN
        ldy     #>POLY_ATN
        jsr     POLYNOMIAL_ODD
        pla
        cmp     #$81
        bcc     L3FFC
        lda     #<CON_PI_HALF
        ldy     #>CON_PI_HALF
        jsr     FSUB
L3FFC:
        pla
        bpl     L4002
        jmp     NEGOP
L4002:
        rts

; =============================================================================

POLY_ATN:
        .byte   $08
        .byte   $78,$3A,$C5,$37
        .byte   $7B,$83,$A2,$5C
        .byte   $7C,$2E,$DD,$4D
        .byte   $7D,$99,$B0,$1E
        .byte   $7D,$59,$ED,$24
        .byte   $7E,$91,$72,$00
        .byte   $7E,$4C,$B9,$73
        .byte   $7F,$AA,$AA,$53
        .byte   $81,$00,$00,$00
GENERIC_CHRGET:
        inc     TXTPTR
        bne     GENERIC_CHRGOT
        inc     TXTPTR+1
GENERIC_CHRGOT:
GENERIC_TXTPTR = GENERIC_CHRGOT + 1
        lda     $EA60
        cmp     #$3A
        bcs     L4058
GENERIC_CHRGOT2:
        cmp     #$20
        beq     GENERIC_CHRGET
        sec
        sbc     #$30
        sec
        sbc     #$D0
L4058:
        rts
GENERIC_RNDSEED:
; random number seed
        .byte   $80,$4F,$C7,$52
GENERIC_CHRGET_END:

; =============================================================================

PR_WRITTEN_BY:
        lda     #<QT_WRITTEN_BY
        ldy     #>QT_WRITTEN_BY
        jsr     STROUT
COLD_START:
        lda     #$00
        sta     ls_mode         ; Normal mode.
        sta     load_count
        sta     load_count + 1
                
        ldx     #$FF
        stx     CURLIN+1
        txs
        lda     #<COLD_START
        ldy     #>COLD_START
        sta     GORESTART+1
        sty     GORESTART+2
        sta     GOSTROUT+1
        sty     GOSTROUT+2
        lda     #<AYINT
        ldy     #>AYINT
        sta     GOAYINT
        sty     GOAYINT+1
        lda     #<GIVAYF
        ldy     #>GIVAYF
        sta     GOGIVEAYF
        sty     GOGIVEAYF+1
        lda     #$4C
        sta     GORESTART
        sta     GOSTROUT
        sta     JMPADRS
        sta     USR
        lda     #<IQERR
        ldy     #>IQERR
        sta     USR+1
        sty     USR+2
        lda     #WIDTH
        STA     Z17
        lda     #WIDTH2
        sta     Z18
        ldx     #GENERIC_CHRGET_END-GENERIC_CHRGET
L4098:
        lda     GENERIC_CHRGET-1,x
        sta     CHRGET-1,x
        dex
        bne     L4098
        txa
        sta     SHIFTSIGNEXT
        sta     LASTPT+1
        sta     Z15
        sta     POSX
        pha
        sta     Z14
        lda     #$03
        sta     DSCLEN
        lda     #$2C
        sta     LINNUM+1
        jsr     CRDO
        ldx     #TEMPST
        STX     TEMPPT

        ;lda     #<QT_MEMORY_SIZE	; SJ April 2014. Removed memory size question.
        ;ldy     #>QT_MEMORY_SIZE
        ;jsr     STROUT
        ;jsr     NXIN
        ;stx     TXTPTR
        ;sty     TXTPTR+1
        ;JSR     CHRGET
        ;cmp     #$41
        ;beq     PR_WRITTEN_BY
        ;tay
        ;bne     L40EE
        
        LDA     #<RAMSTART2
        ldy     #>RAMSTART2
        sta     LINNUM
        sty     LINNUM+1
        ldy     #$00
L40D7:
        inc     LINNUM
        bne     L40DD
        inc     LINNUM+1
L40DD:
        lda     #$92 ; 10010010 / 00100100
        sta     (LINNUM),y
        cmp     (LINNUM),y
        bne     L40FA
        asl     a
        sta     (LINNUM),y
        cmp     (LINNUM),y
        beq     L40D7; old: faster
        bne     L40FA
L40EE:
        jsr     CHRGOT
        jsr     LINGET
        tay
        beq     L40FA
        jmp     SYNERR
L40FA:
        lda     LINNUM
        ldy     LINNUM+1
        sta     MEMSIZ
        sty     MEMSIZ+1
        sta     FRETOP
        sty     FRETOP+1
L4106:

    	jsr     CRDO
        
        ;lda     #<QT_TERMINAL_WIDTH	; SJ April 2014. Removed width question.
        ;ldy     #>QT_TERMINAL_WIDTH
        ;jsr     STROUT
        ;jsr     NXIN
        ;stx     TXTPTR
        ;sty     TXTPTR+1
        ;jsr     CHRGET
        ;tay
        ;beq     L4136
        ;jsr     LINGET
        ;lda     LINNUM+1
        ;bne     L4106
        ;lda     LINNUM
        ;cmp     #$10
        ;bcc     L4106
        ;STA     Z17

L4129:
        ;sbc     #$0E
        ;bcs     L4129
        ;eor     #$FF
        ;sbc     #$0C
        ;clc
        ;adc     Z17
        ;sta     Z18
L4136:

        ldx     #<RAMSTART2
        ldy     #>RAMSTART2
        stx     TXTTAB
        sty     TXTTAB+1
        ldy     #$00
        tya
        sta     (TXTTAB),y
        inc     TXTTAB
        bne     L4192
        inc     TXTTAB+1
L4192:
        lda     TXTTAB
        ldy     TXTTAB+1
        jsr     REASON
        jsr     CRDO
        lda     MEMSIZ
        sec
        sbc     TXTTAB
        tax
        lda     MEMSIZ+1
        sbc     TXTTAB+1
        jsr     LINPRT
        lda     #<QT_BYTES_FREE
        ldy     #>QT_BYTES_FREE
        jsr     STROUT
        lda     #<STROUT
        ldy     #>STROUT
        sta     GOSTROUT+1
        sty     GOSTROUT+2
        jsr     SCRTCH
        lda     #<RESTART
        ldy     #>RESTART
        sta     GORESTART+1
        sty     GORESTART+2
        jmp     (GORESTART+1)
; OSI is compiled for ROM, but includes
; this unused string
        .byte   "WANT SIN-COS-TAN-ATN"
        .byte   0
QT_WRITTEN_BY:
        .byte   CR,LF,$0C ; FORM FEED
        .byte   "WRITTEN BY RICHARD W. WEILAND."
        .byte   CR,LF,0
QT_MEMORY_SIZE:
        .byte   "MEMORY SIZE"
        .byte   0
QT_TERMINAL_WIDTH:
        .byte   "TERMINAL WIDTH"
        .byte   0
QT_BYTES_FREE:
        .byte   " BYTES FREE"
        .byte   CR,LF,CR,LF
        .byte   "ORWELL 6502 BASIC VERSION 1.0 REV 1.8"
        .byte   CR,LF
        .byte   "COPYRIGHT 1977 BY MICROSOFT CO."
        .byte   CR,LF,0

; =============================================================================
; STARTUP AND SERIAL I/O ROUTINES
;==============================================================================

Reset:
  
    LDX     #STACK_TOP
    TXS                     ; Initialise stack pointer.
    
    CLD                     ; Clear decimal mode.
                            ; VIA1 initialisation.
                            ; Set VIA1 port a up.
    LDA     #$00            ; Load ACC with b00000000.
    STA     via1_ddra       ; Set VIA port a all input.
                            ; Set VIA1 port b up.
    LDA     #$FF            ; Load ACC with b11111111.
    STA     via1_ddrb       ; Set VIA port b all output.
                            ; Set VIA1 PCR.
    LDA     #$CC            ; Load ACC with b11001100     	
    STA     via1_pcr        ; Set up for CB1, CB2 low outputs.
                            ; Set VIA1 IER.
    LDA     #$00            ; Load ACC with b00000000.
    STA     via1_ier        ; Disable all VIA interrupts.
    
                            ; Keyboard reading initialisation.
    LDA     #$19            ; Set the debounce counter.
    STA     key_counter     ; And store it.
    LDA     #$00            ; Load accumulator with 0.
    STA     key_last_key    ; Set the last key pressed value.
    STA     key_data        ; Set the data buffer
    STA     key_pressed     ; Set the key_pressed buffer.
    STA     key_status      ; Set the status.

    STA     IO              ; Set up the IO buffer.
    
    LDA     via1_acr        ; Set up timer 1 on the VIA.
    ORA     #$C0            ; Free-run T1 with output on PB7.    
    STA     via1_acr        ; Don't enable T1 interrupts though
    LDA     #$00
    STA     via1_t1cl       ;  Set the counter values to zero.
    STA     via1_t1ch   
    LDA     #$FF            ; Set up the default beeper value.
    STA     beeper
                            ; Start the ACDs free running.
    LDA     #$0C            ; Set CA2 to output low. 
    STA     via2_pcr 
        
    JSR     Reset_ACIA      ; Initialise the ACIA for serial send/receive.   
  
    lda     #<ISR_V1        ; Set up interrupt vector hooks.
    sta     int_vector1
    ldy     #>ISR_V1
    sty     int_vector1 + 1    
    lda     #<ISR_V2
    sta     int_vector2
    ldy     #>ISR_V2
    sty     int_vector2 + 1
    
    CLI                     ; Enable interrupts.
          
    JSR     Set_font        ; Set the default font to be 80 columns.     
          
    LDY #0
ShowStartMsg:               ; Display start-up message 
    LDA     StartupMessage,Y
    BEQ     WaitForKeypress
    JSR     MONCOUT
    INY
    BNE     ShowStartMsg
    
                            
WaitForKeypress:            ; Wait for a cold/warm start selection   
    
    JSR     MONRDKEY
    BCC     WaitForKeypress

    AND     #$DF            ; Make upper case.
    CMP     #'W'            ; Compare with [W]arm start.
    BEQ     WarmStart

    CMP     #'C'            ; Compare with [C]old start.
    BNE     Reset
    
    JMP     COLD_START      ; BASIC cold start.

WarmStart:

    STA     via2_pcr    
    JMP     RESTART         ; BASIC warm start.


; =============================================================================
; Output a character.
;==============================================================================

MONCOUT: 
    PHA                         ; Push the accumulator. 
    STA		char_to_send		; Store the character we are sending. 

    LDA     #%10000000          ; If we are in a flow control condition we can't    
    AND     ls_mode           	; transmit so don't try sending anything or we 
    BNE     MONCOUT_DONE        ; get stuck polling for the transmitter to be free.
    
    LDA     #%00000001       	; If we are in load mode don't echo to the screen.
    AND     ls_mode            
    BNE     MONCOUT_DONE    
    
    LDA     #%00000100          ; If we are in save mode echo to the serial port and screen.
    AND     ls_mode            
    BEQ     MONCOUT_SEND  
    JSR     Send_Char    
          
 MONCOUT_SEND:    
    JSR		Send_Char_Screen	; Send the character to the video output.
  
 MONCOUT_DONE:
    PLA                         ; Pop the accumulator.
    RTS    

; =============================================================================
; Check if a key is pressed. Carry set if key pressed.
;==============================================================================

MONRDKEY:
    JSR Key_Scan        ; Scan the keyboard.

    LDA #$80            ; Check if the new data available flag is set.
    AND key_status      ; AND it with the keyboard status to test the bit.
    BEQ No_Key          ; If the result is equal to zero we have no data.

    LDA #$7F            ; Clear the new data available bit.
    AND key_status
    STA key_status

    LDA key_data        ; Get the data.
    SEC                 ; Carry set if key available.
    JMP Done_Key
No_Key:
    CLC                 ; Carry clear if no key pressed.
Done_Key:
    RTS

; =============================================================================
; Check if control-C is pressed. Carry set if pressed and cleared if not.
;==============================================================================

MONISCNTC:
    JSR     MONRDKEY
    BCC     Not_CTRLC   ; If no key pressed then exit.
    CMP     #3
    BNE     Not_CTRLC   ; If CTRL-C not pressed then exit.
    SEC                 ; Carry set if control-C pressed.
    RTS
Not_CTRLC:
    CLC                 ; Carry clear if control-C not pressed.
    RTS   
    
; =============================================================================
; Keyboard map.
; The keyboard is 48 keys mapped as 7 rows by 8 columns.
; There are four parts to the table for normal and shifted keys, control
; and function keys.
; =============================================================================

keyboard_map:
keyboard_map_row_n0: .BYTE "=", $20, $0D, 0, 0, 0, 0, 0 ;$20 = SPACE, $0D = CR
keyboard_map_row_n1: .BYTE "nhy6bgt5"
keyboard_map_row_n2: .BYTE "mju7vfr4"
keyboard_map_row_n3: .BYTE ",ki8cde3"
keyboard_map_row_n4: .BYTE ".lo9xsw2"
keyboard_map_row_n5: .BYTE "/;p0zaq1"
keyboard_map_row_s0: .BYTE "+", $20, $0D, 0, 0, 0, 0, 0 ;$20 = SPACE, $0D = CR
keyboard_map_row_s1: .BYTE "NHY^BGT%"
keyboard_map_row_s2: .BYTE "MJU&VFR$"
keyboard_map_row_s3: .BYTE "<KI*CDE#"
keyboard_map_row_s4: .BYTE ">LO(XSW@"
keyboard_map_row_s5: .BYTE "-:P)ZAQ!"
keyboard_map_row_c0: .BYTE "=", $20, $0D, 0, 0, 0, 0, 0 ;$20 = SPACE, $0D = CR
keyboard_map_row_c1: .BYTE "nhy6b", $07, "t5"           ;$07 = CTRL-G (bell)
keyboard_map_row_c2: .BYTE "mju7vfr4"
keyboard_map_row_c3: .BYTE ",ki8", $03, "de3"           ;$03 = CTRL-C
keyboard_map_row_c4: .BYTE ".lo9xsw2"
keyboard_map_row_c5: .BYTE "/;p0zaq1"
keyboard_map_row_f0: .BYTE "=", $20, $0D, 0, 0, 0, 0, 0 ;$20 = SPACE, $0D = CR
keyboard_map_row_f1: .BYTE "nhy6b}]5"
keyboard_map_row_f2: .BYTE "mj_7v{[4"
keyboard_map_row_f3: .BYTE ",k?8`", $09, $1B, "3"       ; $09 = TAB, $1B = ESC
keyboard_map_row_f4: .BYTE ".l'9", $0A, $08, "~2"       ; $0A = LF, $08 = BS
keyboard_map_row_f5: .BYTE "/;", $22, "0\|q1"           ; $22 = DOUBLE QUOTE

; =============================================================================
; Reset the ACIA for send and receive.
; =============================================================================

Reset_ACIA:
   
    LDA     #$00            
    STA     acia1_s         ; Reset the ACIA.    
    STA     ls_mode       	; Normal mode.          
    
    LDA     #%00011111      ; Set up for 19200/8/1.   
    STA     acia1_ct        ; Write to ACIA control register.
    
    LDA     #%00001011      ; Set up N parity/echo off/tx int off/rts low/rx int off/dtr active.
    STA     acia1_cm        ; Write to ACIA command register.

    LDA     acia1_rd_ptr    ; Set the two pointer to the same which
    STA     acia1_wr_ptr    ; indicated the buffer to be empty.   
    
    LDA     acia1_s         ; Read the status register to clear the interrupt flag.   
    LDA     acia1_d         ; Read the data register to clear any errors.       
    RTS

; =============================================================================
; Set the startup font to be 80 character bold double (best for small LCDs).
; =============================================================================

Set_font:                      
    LDA     #$03            ; Correct value. 
    STA     c_val           ; Put into the c_value.
    JSR     FONT            ; Set the font.  
    LDA     #$00            ; Clear the c_val.
    STA     c_val         
 RTS   
    
; =============================================================================
; Serial send char subroutine. Character is in char_to_send.
; =============================================================================

Send_Char:

    SEI                         ; Disable interrupts during send.

Send_Char_Wait_TX:  	
    LDA     acia1_s      	    ; Read CIA staus register.
    AND     #%00010000         	; Check if the TX buffer is empty.
  	BEQ     Send_Char_Wait_TX   ; Wait if not.
  	
    LDA		char_to_send		; Get the character we are sending.
  	STA     acia1_d             ; Send to ACIA data register.	
    
Send_Char_Done:    
    CLI                         ; Re-enable interrupts.
    RTS			                ; Return

; =============================================================================
; Video send char. Character is in char_to_send.
; =============================================================================

Send_Char_Screen:
	
    LDA		char_to_send		; Get the character we are sending.   
	
	AND		#$F0				; Get top nibble.	
	LSR							; Shift one bit to the right.		
	STA		via1_b				; Push it to port B of the VIA.

	LDA     #%10010000      	; Set up the interrupt for CB1 change.
    STA     via1_ier        	; Write to VIA interrupt enable register.	 
	
	LDA     #%11110000      	; Set up for CB2 high, CB1 interrupt on positive.
    STA     via1_pcr        	; Write to VIA control register.
    
    LDA     #$00				; Set the waiting flag to 0.
    STA		char_sending
Send_Char_Wait_First_Nib:
	LDA     char_sending      	; Check the char sending flag.
  	BEQ     Send_Char_Wait_First_Nib     	              
    
    LDA		char_to_send		; Get the character we are sending. 
	AND		#$0F				; Get bottom nibble.	
	CLC
	ROL							; Shift 3 bits left.
	ROL
	ROL
	STA		via1_b				; Push it to port B of the VIA.
	LDA     #%11000000      	; Set up for CB2 low, CB1 interrupt on negative.
    STA     via1_pcr        	; Write to VIA control register.
    
    LDA     #$00				; Set the waiting flag to 0.
    STA		char_sending
Send_Char_Wait_Second_Nib:
	LDA     char_sending      	; Check the char sending flag.
  	BEQ     Send_Char_Wait_Second_Nib     	
    
	LDA     #%00000000      	; Turn off all interrupts on VIA.
    STA     via1_ier        	; Write to VIA interrupt enable register.
    
    LDA     #$00				; Clear the port.
    STA		via1_b

Send_Char_Screen_Done:    
    RTS			                ; Return
        
; =============================================================================
; Keyboard routine. This routine will set the data available flag in the
; key_status register and return data in the key_data variable when valid data
; is available from the keyboard. This routine scans the matrix by calling
; the matrix_scan routine then handles debouncing of the value.
; =============================================================================

Key_Scan:
    TXA                     ; Preserve X.
    PHA
    TYA                     ; Preserve Y.
    PHA
    PHP                     ; Preserve status register.

    JSR     Matrix_Scan     ; Scan the matrix.

    LDA     key_pressed     ; Compare the key detected to the last key pressed.
    CMP     key_last_key    ; Is this key different?
    BNE     Key_Change      ; The keys are different so it's changing

                            ; It's the same.
    LDA     key_counter     ; If the counter is already at zero this is the same keypress.
    BEQ     Key_Scan_Done   ; If it's zero we don't do anything.

    DEC     key_counter     ; Otherwise decrement the debounce counter.
    BEQ     Key_Debounced   ; If it's zero now we have a valid key reading.
    JMP     Key_Scan_Done

Key_Change:
    LDA     #$19            ; Reset the counter.
    STA     key_counter     ; And store it.
    LDA     key_pressed     ; Update the last key pressed value with this new value.
    STA     key_last_key
    JMP     Key_Scan_Done

Key_Debounced:              ; A valid key reading was detected.
    LDA     key_pressed     ; Load up the key pressed.
    BEQ     Key_Scan_Done   ; If it was zero (no key pressed) we're done.
    STA     key_data        ; Make it available now in the data.

    LDA     #$80            ; Set the new data available bit.
    ORA     key_status
    STA     key_status      ; Store it in the status.

Key_Scan_Done:
    PLP                     ; Restore status.
    PLA                     ; Restore Y.
    TAY
    PLA                     ; Restore X.
    TAX
    RTS

; =============================================================================
; Keyboard matrix scan routine. The key_status (bit 6) will indicate if there
; is a key currently pressed.  It also indicated if the function, control,
; shift or alpha lock were on (bits 0, 1, 2 and 3)
; There are 7 rows (0-6) and 8 columns.
; Shift, control and function are on row 0. Alpha is on row 6. We handle
; these as special cases.
; The pressed key is returned in key_pressed.
; =============================================================================

Matrix_Scan:
    LDA     #$00                ; Load ACC with 0.
    STA     key_buffer          ; Clear the keyboard character buffer.
    STA     key_pressed         ; Clear the key pressed buffer.
    LDA     #$80                ; Load ACC with 10000000.
    AND     key_status          ; Clear the bottom 7 bits of the status.
    STA     key_status          ; Store it in the status.

Matrix_Scan_Special:
    LDX     #$00                ; Read row 0.
                                ; Row zero is a special case as it has the shift, control and function keys.
    TXA                         ; Move the row counter to the accumulator.
    STA     via1_b              ; Push it to the VIA port b to select the row.
    LDA     via1_a              ; Read VIA input port a.
    EOR     #$FF                ; Exclusive OR to get which bits are set (keys active low).
    BEQ     Matrix_Scan_Alpha   ; Can shortcut here. If this is zero no keys are being pressed on this row.
                                ; Just go straight to the next row.
    STA     key_buffer          ; We have some key or keys pressed. Store the port in our temp holder.
    LDA     #$07                ; Check if the special keys are pressed. Set up mask for the special keys.
    AND     key_buffer
    ORA     key_status          ; Now set those bits in the status register.
    STA     key_status          ; Store it in the status.
        
    LDA     #$E0                ; Now mask off the normal keys and check them.
    AND     key_buffer
    STA     key_buffer          ; Store it in the buffer.

Matrix_Scan_Alpha:
    LDX     #$6                 ; The alpha key on row 6 is also a special case.
    TXA                         ; Move the row counter to the accumulator.
    STA     via1_b              ; Push it to the VIA port b to select the row.
    LDA     via1_a              ; Read VIA input port a.
    EOR     #$FF                ; Exclusive OR to get which bits are set (keys active low).
    BEQ     Matrix_Scan_Rows    ; If this is non zero the alpha key is pressed.
        
    LDA     #$08                ; Set the alpha bit.
    ORA     key_status
    STA     key_status          ; Store it in the status.
        
Matrix_Scan_Rows:
    LDX     #$00                ; Reset X back to the first row.
    LDY     #$00                ; Use Y as the column counter.

Matrix_Scan_Col:
    LDA     #$80                ; Load a mask into the accumulator to check the data bit of the buffer.
    AND     key_buffer          ; AND it with the buffer to test the bit to see if a key was pressed.
    BEQ     Matrix_Scan_Col_Next; Branch if it is zero since that key wasn't pressed.
        
    LDA     #$40                ; It's set. Check if we already have a key pressed. Test bit 6 of the keyboard status.
    AND     key_status          ; AND it with the keyboard status to test the bit.
    BEQ     Matrix_Scan_Col_Data; If it's not set (result equals zero) process this new key.

Matrix_scan_Invalid:            ; It's already set so now we have an invalid state.
    LDA     #$BF                ; Clear the key pressed flag.
    AND     key_status
    STA     key_status          ; Set the status.
    JMP     Matrix_Scan_Done    ; Now finish since it was invalid.

Matrix_Scan_Col_Data:
    LDA     #$40                ; Set the key pressed bit.
    ORA     key_status
    STA     key_status          ; Store it in the status.
    STX     key_row             ; Remember the row.
    STY     key_col             ; Remember the column.

Matrix_Scan_Col_Next:
    CLC                         ; Clear the carry flag.
    ROL     key_buffer          ; Rotate the buffer left to check the next column.
    INY                         ; Increment the column count.
    CPY     #$08                ; Have we done all eight columns?
    BNE     Matrix_Scan_Col     ; Zero set if equal when we've done all eight.

Matrix_Scan_Row_Next:
    INX                         ; Go onto the next row.
    CPX     #$06                ; Have we done all remaining six rows?
    BEQ     Matrix_Scan_Rows_Done
    TXA                         ; Not done so move the row counter to the accumulator and read the next row.
    STA     via1_b              ; Push it to the VIA port b to select the row.
    LDA     via1_a              ; Read VIA input port a.
    EOR     #$FF                ; Exclusive OR to get which bits are set (keys active low).
    BEQ     Matrix_Scan_Row_Next; Can shortcut here. If this is zero no keys are being pressed on this row.
                                ; Just go straight to the next row.

    STA     key_buffer          ; We have some key or keys pressed. Store the port in our buffer.
    LDY     #$00                ; Use Y as the column counter.
    JMP     Matrix_Scan_Col     ; Check the columns for this row.

Matrix_Scan_Rows_Done:
    LDA     #$40                ; Check if the data flag is set.
    AND     key_status          ; AND it with the keyboard status to test the bit.
    BEQ     Matrix_Scan_Done    ; If the result is equal to zero we have no keys pressed so can finish.
        
Matrix_Scan_Shift:              ; We add to the row counter depending on which special keys were pressed.
    LDA     #$04                ; Check shift first.
    BIT     key_status          ; Bit three in key_status.
    BEQ     Matrix_Scan_Ctrl    ; Shift not pressed so test control next.
    LDA     #$06                ; Shift was pressed so add 6 to the row count.
    CLC                         ; Clear carry flag.
    ADC     key_row             ; Add to the row count.
    STA     key_row             ; Store the result.
    JMP     Matrix_Scan_Lookup  ; Now go look up the key.
        
Matrix_Scan_Ctrl:
    LDA     #$02                ; Check control next.
    BIT     key_status          ; Bit 2 in key_status.
    BEQ     Matrix_Scan_Fctn    ; Control not pressed so test function next.
    LDA     #$0C                ; Control was pressed so add 12 to the row count.
    CLC                         ; Clear carry flag.
    ADC     key_row             ; Add to the row count.
    STA     key_row             ; Store the result.
    JMP     Matrix_Scan_Lookup  ; Now go look up the key.
        
Matrix_Scan_Fctn:
    LDA     #$01                ; Finally check function.
    BIT     key_status          ; Bit one in key_status.
    BEQ     Matrix_Scan_Lookup  ; Function not pressed so go onto lookup the key.
    LDA     #$12                ; Function was pressed so add 18 to the row count.
    CLC                         ; Clear carry flag.
    ADC     key_row             ; Add to the row count.
    STA     key_row             ; Store the result.
        
Matrix_Scan_Lookup:             ; Now lookup the actual character.
    LDA     key_row             ; Get the row.
    CLC                         ; Clear the carry flag.
    ROL                         ; Multiple the row counter by 8 (by using ROL).
    CLC     
    ROL                         ; x4
    CLC     
    ROL                         ; x8
    CLC     
    ADC     key_col             ; Then add the column to get out offset into the table.
    TAX                         ; Transfer it into X.
    LDA     keyboard_map, X     ; Use this as the offset into our table. Add the offset to the start of the table
                                ; and get the actual character into the accumulator.
        
    STA     key_pressed         ; Store the new character.
        
    LDA     #$08                ; Check if Alpha is on.
    BIT     key_status          ; Bit four in key_status.
    BEQ     Matrix_Scan_Done    ; Alpha not pressed so just store the key.
                                ; $61 = a .. $7A = z.
        
    LDA     key_pressed
    CMP     #$61                ; Alpha is pressed so make lower case letters upper.
    BCC     Matrix_Scan_Done    ; Less than $61.
        
    CMP     #$7B
    BCS     Matrix_Scan_Done    ; More than than $7A.
        
    AND     #$DF                ; Make upper case.
    STA     key_pressed         ; Store the new character.
        
Matrix_Scan_Done:
    LDA     #$00                ; Reset the VIA port to all zero.
    STA     via1_b              ; Push it to the VIA port b.
    
    RTS


; =============================================================================
; Dumps out memory. The amount is specified in dump_number.
; =============================================================================

Dump_Memory:

    PHP
    PHA
    TXA
    PHA
    TYA
    PHA

    LDA     #$0C        ; Use X as a byte counter.
    TAX                 ; We can fit 12 bytes across the screen.

    LDA     #$0D        ; Start on a new line.
    JSR     Send_Char
    LDA     #$0A
    JSR     Send_Char
    LDY     #$00
Dump:
    LDA     RAMSTART2, Y
    JSR     Print_Byte
    INY
    TYA
    SBC     dump_number  ; Number of bytes to dump out.
    BEQ     Dump_Done    ; If we've done them all we're finished.

    DEX
    BNE     Dump

    LDA     #$0D         ; Next line.
    JSR     Send_Char
    LDA     #$0A
    JSR     Send_Char
    LDA     #$0C
    TAX                 ; Reset X.
    BNE     Dump

Dump_Done:
    LDA     #$0D         ; Next line.
    JSR     Send_Char
    LDA     #$0A
    JSR     Send_Char

    PLA
    TAY
    PLA
    TAX
    PLA
    PLP
    RTS

; =============================================================================
; Wait key.
; =============================================================================

Wait_Key:
    PHP
    PHA
    TXA
    PHA
    TYA
    PHA
Wait:
    JSR    MONRDKEY
    BCC    Wait

    PLA
    TAY
    PLA
    TAX
    PLA
    PLP
    RTS
    
; =============================================================================
; Beep
; =============================================================================
Beep:

    PHA                     ; Store A.
    TXA                     ; Store X.
    PHA
    TYA                     ; Store Y.
    PHA

    LDA     #$00            ; Use X as inner loop counter.
    TAX         
    LDA     #$C8            ; Use Y as outer loop counter.
    TAY                       
    LDA     beeper          ; Set up the timer 1 counter.
    STA     via1_t1cl       ; Start playing the beep.

Beep_2:    
Beep_1:
    INX                     ; Increment the inner loop count.
    BNE     Beep_1          ; Exit once counter overflows.        
    INY
    BNE     Beep_2          ; Exit once counter overflows.
        
    LDA     #$00            
    STA     via1_t1cl       ; Stop playing the beep.
    
    PLA
    TAY                     ; Restore Y.
    PLA
    TAX                     ; Restore X.
    PLA                     ; Restore A    

    RTS

; =============================================================================
; Beep1
; =============================================================================
Beep1:

    PHA                    
    LDA     #$FF
    STA     beeper    
    JSR     Beep       
    PLA
    RTS
    
; =============================================================================
; Beep2
; =============================================================================
Beep2:

    PHA                    
    LDA     #$88
    STA     beeper    
    JSR     Beep       
    PLA
    RTS
    
; =============================================================================
; Print a byte as ASCII digits.
; =============================================================================

Print_Byte:
    PHA             ; SAVE ACC FOR USE LATER ON
    LSR             ; SHIFT H.O. NIBBLE
    LSR             ; DOWN TO THE L.O. NIBBLE
    LSR             ; CLEARING THE H.O. NIBBLE
    LSR

Print_Nibble:
    ADC     #$30        ; CONVERT TO ASCII
    CMP     #$3A        ; IF IT IS A DIGIT FINE, OTHER-
    BCC     Print_High  ; WISE IT MUST BE CONVERTED TO A
    ADC     #$6         ; LETTER IN THE RANGE A-F
Print_High:
    JSR     Send_Char

    PLA                 ; GET ORIGINAL VALUE BACK
    AND     #$F         ; MASK H.O. NIBBLE

    ADC     #$30        ; CONVERT TO ASCII
    CMP     #$3A        ; IF IT IS A DIGIT FINE, OTHER-
    BCC     Print_Low   ; WISE IT MUST BE CONVERTED TO A
    ADC     #$6         ; LETTER IN THE RANGE A-F
Print_Low:
    JSR     Send_Char
    LDA     #$20
    JSR     Send_Char

    RTS

; =============================================================================
; Flow control on.
; This enables sending/receiving.
; =============================================================================

 Flow_On:
    SEI
    PHA   
    LDA     #%00001001      ; Set bit 3 to set RTS low.           
    STA     acia1_cm  
    LDA     #$7F            ; Clear the top bit to indicate no flow control condition.
    AND     ls_mode
    STA     ls_mode
 Flow_On_Done:
    PLA
    CLI 
    RTS
    
; =============================================================================
; Flow control off.
; This disables sending/receiving.
; =============================================================================
 
 Flow_Off:
    SEI
    PHA
    LDA     #%00000001      ; Clear bit 3 to set RTS high.       
    STA     acia1_cm 
    LDA     #$80            ; Set the top bit to indicate a flow control condition.
    ORA     ls_mode
    STA     ls_mode
 Flow_Off_Done: 
    PLA
    CLI
    RTS
        
; =============================================================================
; ACIA write buffer.
; =============================================================================

ACIA_Wr_Buffer:
    LDX     acia1_wr_ptr         ; Get the current index.
    STA     acia1_rx_buffer, X   ; Get the pointer value and store the data where it says
    INC     acia1_wr_ptr         ; then increment the pointer for the next write.
    RTS

; =============================================================================
; ACIA read buffer.
; =============================================================================

ACIA_Rd_Buffer:
    TXA                          ; Store X. 
    PHA
    LDX     acia1_rd_ptr         ; Ends with A containing the byte just read from buffer.
    LDA     acia1_rx_buffer, X   ; Get the pointer value and read the data it points to.
    STA     acia1_rx_byte        ; Store the data temporarily.
    INC     acia1_rd_ptr         ; Then increment the pointer for the next read.
    PLA                          ; Restore X.
    TAX
    LDA     acia1_rx_byte
    RTS

; =============================================================================
; ACIA buffer difference.
; =============================================================================

ACIA_Buffer_Diff:
    LDA     acia1_wr_ptr        ; Find difference between number of bytes written
    SEC                         ; and how many read.
    SBC     acia1_rd_ptr        ; Ends with A showing the number of bytes left to read.
    RTS

; =============================================================================
; ACIA buffer get character.
; =============================================================================

ACIA_Buffer_Get: 
    SEI    
    JSR     ACIA_Buffer_Diff     ; See if there is any serial data.
    BEQ     ACIA_Buffer_Get_None ; No data so exit.    
    JSR     ACIA_Rd_Buffer       ; There is data so get it.

    JSR     ACIA_Buffer_Diff     ; How many bytes are left to read?
    CMP     #$B4                 ; Is it at least 180?
    BCS     ACIA_Buffer_Got      ; If so, leave the sending end turned off.
    JSR     Flow_On              ; Else re-enable sending.          
            
ACIA_Buffer_Got: 
    LDA     acia1_rx_byte        ; Retrieve the data into A.
    SEC                          ; Carry set if character available.
    JMP     ACIA_Buffer_Get_Done   
    
ACIA_Buffer_Get_None: 
    CLC                          ; Carry clear if no character available.    
    
ACIA_Buffer_Get_Done:   
    CLI
    RTS    
    
; =============================================================================
; ACIA Interrupt.
; =============================================================================

ACIA_Interrupt:
    
    PHA                             ; Store A.   
    TXA                             ; Store X. 
    PHA  
    TYA                             ; Store Y. 
    PHA  
    
    LDA     acia1_s                 ; Take a copy of the status register.
    STA     acia1_s_copy            ; Reading it changes it! 
    
    AND     #%00001000              ; Check for the receive data buffer full.
    BEQ     ACIA_Interrupt_Done     ; If empty we are done.
    
    LDA     acia1_s_copy
    AND     #%00000111              ; Check for error conditions (lower 3 bits of status).
    BNE     ACIA_Interrupt_Error    ; If there was any error go report it.
    
    LDA     acia1_d                 ; Get the data from the ACIA.    
    JSR     ACIA_Wr_Buffer          ; Write it to the buffer.  

    JSR     ACIA_Buffer_Diff        ; Now see how full the buffer is.
    CMP     #$DC                    ; If it has less than 220 bytes unread
    BCC     ACIA_Interrupt_Done     ; just exit the ISR here. Else stop 
                                    ; the sending end.
    JSR     Flow_Off
    JMP     ACIA_Interrupt_Done     
    
ACIA_Interrupt_Error:                      
    LDA     acia1_s_copy
    STA     IO
    JSR     Beep2
    LDA     acia1_d                 ; Read the data register to clear it out. 
    JSR     NORMAL					; Try to go back to normal mode.
    
ACIA_Interrupt_Done:          
    PLA                             ; Restore Y.
    TAY            
    PLA                             ; Restore X.
    TAX
    PLA                             ; Restore A.
    
    RTI

; =============================================================================
; VIA Interrupt.
; =============================================================================

VIA_Interrupt:

	    
    PHA                             ; Store A.   
     
	LDA		via1_ifr				; Check which interrupt fired.
	ASL 	A
	ASL 	A
	ASL 	A
	BMI 	VIA_Interrupt_CB1
	JMP		VIA_Interrupt_Done

VIA_Interrupt_CB1:
	LDA		via1_b					; Read the port to clear the interrupt.    
    INC		char_sending			; Make the flag non zero.

VIA_Interrupt_Done:      
    PLA                             ; Restore A.
    RTI

; =============================================================================
; ISR
; =============================================================================

ISR:
    jmp     (int_vector1)   ; Jump to the interrupt vector handler.
ISR_V1:
    BIT     via1_ifr        ; Check 6522 VIA1's status register without loading.        
    BMI     VIA_Interrupt   ; Result negative (bit 7 set) so service VIA.            
         
    BIT     acia1_s         ; Check the status register (loads bit 7 into N and bit 6 into V).    
    BMI     ACIA_Interrupt  ; Result negative (bit 7 set) so service ACIA.            
    jmp     (int_vector2)   ; Jump to the interrupt vector handler.
ISR_V2:
    RTI

; =============================================================================

StartupMessage:
    .byte    $0D,$0A, $0A, "Cold [C] or warm [W] start?", $00


.segment "VECTS"
;.org $FFFA
    .word    Reset      ; NMI.
    .word    Reset      ; Reset.
    .word    ISR        ; Interrupt.