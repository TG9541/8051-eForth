;
;******************************************************************************
;
;	8051 eForth 1.1 by C. H. Ting, 1990
;
;	This eForth system was developed using chipForth from Forth, Inc.
;	and tested on a Micromint BCC52 single board computer.
;	The eForth Model was developed by Bill Muench and C. H. Ting.
;
;	The goal of this implementation is to show that the eForth Model
;	can be ported to a ROM based 8 bit microprocessor, Intel 8051.
;	Deviations from the original eForth Model are:
;
;		All kernel words are assembled as DB statements.
;		Memory map is tailored to a ROM based system.
;		%colon and %user are modified to compile LJMP doLIST.
;		call, compiles a LCALL with a flipped destination address.
;		USER, VARIABLE and : are modified to use above 'call,'.
;		FORTH vocabulary pointer is a pair user variables.
;		BYE is deleted.
;
;	To assemble this source file and generate a ROM image,
;	type the following commands using MASM and LINK:
;		>MASM 8051;
;		>LINK 8051;
;	The resulting 8051.EXE contains the binary image suitable
;	for PROM programming.  The actual image is offset by 200H
;	bytes from the beginning of the .EXE file.  This image
;	must be placed in a PROM from 0 to 1FFFH, and it uses a RAM
;	chip from 8000H to 9FFFH.  If your system does not have
;	this memory configuration, modify the memory pointers in
;	the source file accordingly.  Places to be modified are
;	marked by '******'.
;
;	8051 is a slow processor.  Do not expect great performance
;	of this implementation, considering that most words are in high
;	level.  Your are encouraged to recode some of the high level words
;	to optimize its performance.
;
;	Direct your questions and contributions to:
;
;		Dr. C. H. Ting
;		156 14th Avenue
;		San Mateo, CA 94402
;		(415) 571-7639
;
;******************************************************************************
;
;  Notes -
;
;    All tests and timings were done on a 8031AH running at 11.059200 Mhz.
;
;    Rearrange usage of registers slightly.  I'm not sure why the good doctor
;    used register bank one instead of zero, and then put some of the variables
;    in bank 0 space.  I moved them out of there, so that an assembly routine
;    can do a 'clr rs0', and use the bank 0 registers without wrecking the
;    forth environment.  Notice that SAME? takes advantage of that.
;
;    It's amazing how long it took to figure out why the original FIND routine
;    worked at all.  It's was all because PACK$ fills the trailing bytes with
;    null (0's).  The routine SAME? only compared words, which meant that an
;    odd size name (including the length byte) should not have matched.  But
;    becase PACK$ did fill with null, it did compare.  Not well documented.
;
;    SAME? is still in the .ASM file, but commented out.  I don't know how
;    many folks might have written words that need it.  If you do, uncomment
;    it out, and reassemble.
;
;    Removed CELLS (commented out, code still in file), and fixed system to
;    assume a word (cell) size of 16 bits.  I don't see this becoming a 32
;    bit forth anytime soon.  
;
;    Question: Will user variables ALWAYS be in RAM?  If not, fix EMIT for
;    a 'CLR A' 'MOVC A,@A+DPTR' sequence, instead of 'MOVX A,@DPTR'.  I 
;    can't really see a reason why user variables would be in ROM...  Kinda
;    defeats the purpose, right?
;
;    %sforth and SASMBLY allow switching between forth and assembly execution
;    in the middle of a word definition.  Handy for using words like EMIT in
;    TYPE.  However, they do require careful use, because registers aren't
;    saved, etc, etc, etc...
;
;    Converting TYPE to assembly improved the character output rate from 1354
;    characters per second to 1595 cps.  Speeding up TX! resulted in a speed
;    up to over 1920 characters per second.  Test was creating a string of
;    60 characters (S) and using the following word.  : TEST 0 8192 S COUNT
;    TYPE NEXT ;  (In this test, COUNT was already in assembly).
;
;    Converting UM* to assembly resulted in an incredible speed up.  The
;    test word of : TEST 0 20000 FOR $FFFF $FFFF UM* 2DROP NEXT ; took 4
;    minutes and 6 seconds.  The recoded version of UM* resulted in the
;    word taking 5 seconds to execute.   Not bad... Also improved the
;    speed */MOD.
;
;    Convert UM/MOD to assembly also improved speed.  The test word was
;    : TEST 0 20000 FOR $FFFF $0FFF $3333 UM/MOD 2DROP NEXT ; and took 5 
;    minutes and 20 seconds.  The recoded version took about 14 seconds, 
;    and cost only an additional 23 bytes.  Also dramatically improved the
;    speed of EXTRACT, which is used by DIG for numerical output.
;
;    AUTOBAUD works by measuring the duration of the start bit of a incoming
;    word.  It then takes that period and compares it to a table, which con-
;    tains timer values normalized to 11.059200mhz.  The values in the table
;    are the period of a given baud rate (say, 1200 baud), plus and minus 3%.
;    This allows small variations in timings from the host to be accounted
;    for.  The timer and SMOD bit value are read from the table, and timer 1
;    is programmed.  Also read from the table is the baud rate as a binary
;    value.  This is stored into the user variable BAUDRATE so that a program
;    may interrogate the current rate.  A small side note here.  I cannot
;    explain why you get an extra space and the ' ok' does not emit when the
;    baud rate is changed.  If you figure it out, I'd sure like to know...
;
;    CI@ and CI! will not read or write the SFR (Special Function Registers).
;    This is because that memory cannot be accessed indirectly, and CI@ and
;    CI! use indirect addressing.  It does, however, allow you to used an
;    8032/8052 with the extra 128 bytes of internal memory.
;
;    There are several words that can be enabled or disabled, depending on
;    whether you intend to embed this forth, or use it interactively.  These
;    switches are at about line 250, or so.  I have converted these words to
;    real forth, and if you need them for debugging, they can be uploaded
;    fairly quickly.
;
;    Stack picture of a double is ( dl dh -- )
;
;    HOLD take advantage of the fact that the user variables (currently) all
;    reside in one page.  Because of this, DPL is increment and decremented
;    in the routine without worrying abour DPH.  Should the user variables
;    ever start spanning a page, HOLD may need to be fixed.
;
;    Delete the words DIGIT and EXTRACT because nothing used them except
;    the HOLD and # routines.  These words don't seem to be in any of the
;    'standard' forth vocabularys, so their removal shouldn't cause any
;    problems.
;
;    Deleted the words xio, FILE, HAND, CONSOLE, I/O and PACE.  I see no
;    reason why you need a file mode here.  If you need to upload code from
;    a PC, use something like CrossTalk or SmartCommIII, and set the character
;    and line pacing modes to echo (I.e., wait for character echo on both
;    normal characters, and end of line characters.)  The 100 or so bytes
;    taken by these routines can be put to better use...  I did leave the
;    user variables 'PROMPT, 'ECHO, 'TAP, 'EXPECT, 'EMIT, '?KEY, so your
;    program can change the console handlers, if needed.
;
;    I think I understand how the vocubulary stuff works.  The word 'forth'
;    is setup with the last name in the dictionary.  Executing this word
;    returns the address of it's storage, which is 2 16 bit words.  The
;    first is the last word in the dictionary, and the second is the link
;    to another (?0 dictionary.  Initially, CONTEXT points to these 2 words,
;    which are updated by words like CREATE, :, and CONSTANT, etc.  I'd
;    like to add VOCABULARY to the system, but my understanding of how all
;    that stuff inter-relates is still a little (a lotta...) hazy...  
;    Executing the word 'FORTH' restores the CONTEXT pointer back to 'forth'.
;
;    Changed all the MOVC instructions to MOVX.  I don't see anyway that
;    this forth can be modified (without getting ridiculous, at any rate)
;    to run as a harvard architecture system.  Freed up some space in doing
;    that, mainly because I could get rid of the CLR A in front of most
;    every MOVC.  MOVC is still used in the AutoRate routine, because of the
;    table access.
;
;    Not sure what to do about LEAVE.  One version of forth (FPC 3.53) says
;    that the F83 standard requires LEAVE to immediately exit the DO..LOOP.
;    The BryteForth manuals says that LEAVE sets the loop limit to the index
;    value, and terminates at the next execution of LOOP or +LOOP.  I went
;    and implemented it as the FPC model (Mostly because it seems more useful,
;    plus I don't have a copy of the standards!)
;
;    DO..LOOP type loops have this format in memory: (DO) <x> ... (LOOP) <y>.
;    <x> is the address of the next word after <y>.  This is so that ?DO knows
;    how to skip the words between DO and LOOP.  <y> is the address of the word
;    after <x>.  This is there so that LOOP knows where to resume the loop.
;    You can confuse the compiler by building a word like this:  : BLOWUP
;    10 0 DO ." Hi!" I 5 = IF ." I=5" LOOP THEN ." I<>5" LOOP ;.  This
;    causes LOOP to store the skip address of the IF to the LOOP back address.
;    After that, THEN stores the LOOP back address to the IF skip address.
;    Finally, the second LOOP occurrence blows the stack because it was
;    expecting the value DO pushed for the LOOP skip back address.
;
;    When converting -TRAILING to assembly, I noticed that Tings original
;    version treated any character less than 32 as a space character.  Several
;    other Forths I examined treated the space character only as a space
;    character.  This seemed more correct, so I fixed this version to treat
;    only spaces as spaces.  The assembly version takes about 12 more bytes
;    but gains a lot of speed.
;
;    In retrospect, I couldn't decide which LEAVE I liked better (immediate
;    or non-immediate).  So I added support for both.  The equates IM_LEAVE
;    defines whether delayed or immediate LEAVEs happen.  I think I prefer
;    the immediate LEAVE.  Changing this equates automatically adjusts DO,
;    UNDO, etc for which ever mode.
;
;    I want to convert this Forth to Metalinks' ASM51 assembler, for 2 main
;    reasons.  The main is the generic jmp/call facility, which would free up
;    a good deal of space.  The other is that the Metalink assembler is
;    supposedly public domain (the marketing guys there were surprised to learn
;    that!).  I tried the conversion, and lo!  Within 10 minutes I had found
;    a MAJOR flaw in their assembler.  If you use ORG statements that do not
;    grow from low to high, it gets mad and generates phase error messages.  A
;    call to tech support yielded "Yea, you're right.  You found a bug we
;    didn't know about, and it's broke.  Sorry..."  Stay tuned for a new ver-
;    sion of Metalinks ASM51, folks...
;
;******************************************************************************
;
;  Revision History -
;
;     1.01 - xx/xx/90 - CHT - Written
;     1.02 - 12/31/90 - JCW - Converted To 8031 Assembler Format
;     1.03 - 01/01/90 - JCW - Add Equates, Memory Map Expectations
;     1.04 - 01/01/91 - JCW - Macro'ized PUSH/POP/PUSHIP/POPIP Sequences
;     1.05 - 01/01/91 - JCW - Add Forth Commenting To %colon Definitions
;     1.06 - 01/02/91 - JCW - Added 1 0 -1 1+ 2+ 1- 2- BYE NIP R>DROP P1@
;			      P1! P3@ P3! C, NOP
;     1.07 - 01/02/91 - JCW - Converted @EXECUTE COUNT BL CELL- CELL+ = ABS -
;			      NEGATE + NOT 2DROP To Assembly
;     1.08 - 01/03/91 - JCW - Optimized As Many Existing Assembly Words As
;			      Possible
;     1.09 - 01/04/91 - JCW - Removed Dependancy On Code Word Alignment.  Freed
;			      Up Quite A Bit Of Space.  Converted %user To
;			      Call DOUSER Directly, Rather Than Through DOLIST.
;     1.10 - 01/05/91 - JCW - Added CSAME? Which Compares 2 Buffers For 'N'
;			      Bytes (SAME? Compares Words).  Convert 'find'
;			      To Use CSAME?  Also Optimized Register Usage So
;			      That All Parameters Are Kept In Registers.
;     1.11 - 01/05/91 - JCW - Removed Dependancy On Dictionary Alignment.  Also
;			      Added DUP>R, And Fixed Colon Definitions To Use.
;			      Deleted ALIGNED And SAME? From Dictionary.
;     1.12 - 01/05/91 - JCW - Removed CELLS, Add 2* 2/.  Convert PICK And DEPTH
;			      To Assembly, And Fix To Assume A 16 Bit Cell Size.
;			      Converted 2DUP To Assembly.
;     1.13 - 01/08/91 - JCW - Convert doVAR And ROT To Assembly.  Wife Says
;			      Spending Too Much Time On 'Puter, So It's Been
;			      A Few Days Since I Could Mess With This.  Sigh...
;     1.14 - 01/09/91 - JCW - Convert DNEGATE And U< To Assembly, Add -ROT.
;     1.15 - 01/10/91 - JCW - Convert SPACE, CMOVE, FILL, ?KEY, EMIT, TYPE And 
;			      +! To Assembly.  Add %sforth, SASMBLY And >.
;     1.16 - 01/12/91 - JCW - Convert CR And _TYPE To Assembly.  Speed Up TX!.
;			      Fix PACK$ Not To Aligned To A Word Boundary.  Add
;			      EVEN And ODD.
;     1.17 - 01/12/91 - JCW - Convert UM*, * And */MOD To Assembly (See Note).
;     1.18 - 01/13/91 - JCW - Convert >NAME To Assembly.
;     1.19 - 01/18/91 - JCW - Convert NAME>, MAX, MIN, 2@, 2! And SPACES To 
;			      Assembly.
;     1.20 - 01/30/91 - JCW - Convert UM/MOD, M/MOD And /MOD To Assembly.
;     1.21 - 01/31/91 - JCW - Added CBITS, Convert EXTRACT To Assembly.
;     1.22 - 02/17/91 - JCW - Convert DIGIT? And NUMBER? To Assembly.
;     1.23 - 02/18/91 - JCW - Added AUTOBAUD And User Variable BAUD.
;     1.24 - 02/18/91 - JCW - Added CI! And CI@
;     1.25 - 02/19/91 - JCW - Added FREE, I, J, ERASE, BLANKS, JOIN, SPLIT, FLIP
;     1.26 - 02/20/91 - JCW - Added TOGGLE, UPPER, Convert Inline Push And Pop 
;			      Sequences To Subroutines.  Added PopReturn.  Also
;			      Added Conditional Assembly Flags For Some Words.
;     1.27 - 02/23/91 - JCW - Added +-, D+-, S->D, DABS, D+, 0>, 0=, MU/MOD
;     1.28 - 02/24/91 - JCW - Added TUCK, UD., UD.R, D., D.R, (.), (D.), Fixed
;			      EXTRACT, #, #S And #> To Handle Double Numbers.
;     1.29 - 02/24/91 - JCW - Added UPC, Which If Non 0 Converts Lower Case
;			      Words To Upper, But Does Not Affect Embedded Text.
;     1.30 - 02/24/91 - JCW - Removed Words DIGIT And EXTRACT.  Converted
;			      # And HOLD To Assembly.
;     1.31 - 02/28/91 - JCW - Delete xio, FILE, HAND, I/O, CONSOLE, PACE. 
;			      Change QUIT Not To Check The Prompt Address
;			      To Decide Whether Or Not To Output ' ok'
;     1.32 - 02/28/91 - JCW - Added CONSTANT, Convert do$ To Assembly, Convert
;			      All MOVC References To MOVX (See Note Above).
;			      Change Run Time Names Of $"| To ($") And ."| 
;			      To (.")
;     1.33 - 03/03/91 - JCW - Added DO, ?DO, LOOP, +LOOP, (DO), (?DO), (LOOP),
;			      (+LOOP), LEAVE, (LEAVE), ?LEAVE, (?LEAVE), UNDO.
;			      Fixed I, J To Work With DO, ?DO.
;     1.34 - 03/08/91 - JCW - Deleted FOR, AFT, NEXT.
;     1.35 - 03/10/91 - JCW - Convert -TRAILING To Assembly, Add Equate Switch
;			      For Immediate And Non-Immediate LEAVE.
;
;  Things To Do --
;
;    Convert find To Assembly, Is Space Available.
;    Fix M/MOD and UM/MOD To Return A Double Qoutient, Instead Of Single.
;    Fix (PARSE) And dm+ Not To Use (NEXT).
;    Convert (PARSE) To Assembly.
;    Use Assembler That Has Generic Jump Capability.  
;    Shift Code So That %code Routine Are At Top (For Generic Jumps).
;
;******************************************************************************
;
;  Version Control
;
Ver_Major	equ	001h		; Major Release Version
Ver_Minor	equ	035h		; Minor Extension
;
;  Option Select
;
IM_LEAVE	equ	-1		; 0 For Non Immediate LEAVE
Word_DOTS	equ	 0		; 0 For No .S Word
Word_SEE	equ	 0		; 0 For No SEE Word
Word_DUMP	equ	 0		; 0 For No DUMP Word
Word_WORDS	equ	 0		; 0 For No WORDS Word
Word_TNAME	equ	 0		; 0 For No >NAME Word
Word_AUTOBAUD	equ	-1		; 0 For No AUTOBAUD Word (Etc...)
;
; Constants
;
Lex_CO		equ	00040h		; Lexicon Compile Only Bit
Lex_IM		equ	00080h		; Lexicon Immediate Bit
Lex_LN		equ	0001fh		; Lexicon + Length Bit Mask (Keep Bits)
;
WrdLen		equ	2		; Size Of A Cell
DfltBase	equ	10		; Default Radix
VocDpt		equ	8		; Depth Of Vocabulary Stack
;
ASC_BS		equ	008h		; Back Space
ASC_LF		equ	00ah		; Line Feed
ASC_CR		equ	00dh		; Carriage Return
;
Calll		equ	0012h		; NOP CALL Opcodes
;
;******************************************************************************
;
; Memory Map -->  (Assumes ROM 0000-1FFFH, RAM 6000-8000H)
;
;   IP - N/A       - N/A            - Forth Instruction Pointer
;   TP - N/A       - N/A            - Top Of Stack Value
;   CP - 6000-  NP - Grows Lo To Hi - Non-ROM Code Area (CP Is Next Available)
;   NP - CP  -7CFF - Grows Lo To Hi - Non-ROM Name Area (NP Is Next Available)
;   SP - 7D00-7DFF - Grows Hi To Lo - Forth Parameter Stack
;   RP - 7E00-7EFF - Grows Hi To Lo - Forth Return Stack
;   UP - 7F00-7FFF - Grows Lo To Hi - User Variables (Vectored Execution Words)
;
;  Note That The Terminal Input Buffer (TIB) Shares The 256 Byte Page With The
;  Forth Return Stack.  The Return Stack Grows From High To Low, While The
;  TIB Fills From Low To High.  It Might Be A Good Idea To Move The TIB Into
;  It's Own 256 Byte Page.
;  
RamBtm		equ	06000h		; Bottom Of RAM Memory
RamLen		equ	02000h		; Length Of RAM Memory
RamEnd		equ	RamBtm+RamLen	; Top Of RAM Memory
RomBtm		equ	00000h		; Start Of ROM
RomLen		equ	02000h		; Length Of ROM
RomTop		equ	RomBtm+RomLen	; Top Of ROM
;
UP0Len		equ	00100h		; User Area Size In Cells
RP0Len		equ	00100h		; Return Stack/TIB Size
SP0Len		equ	00100h		; Data Stack Size
;
UP0Btm		equ	RamEnd-UP0Len	; Start Of User Area        (UP0)(7F00)
UP0Top		equ	UP0Btm+UP0Len-2	; Last Addr Of User Area    (UP0)(7FFF)
TIBBtm		equ	UP0Btm-RP0Len	; Terminal Input Buffer     (TIB)
RP0Btm		equ	UP0Btm-RP0Len	; Start Of Return Stack     (RP0)(7E00)
RP0Top		equ	RP0Btm+RP0Len-2	; Last Addr Of Return Stack (RP0)(7EFE)
SP0Btm		equ	RP0Btm-RP0Len	; Start Of Data Stack       (SP0)(7D00)
SP0Top		equ	SP0Btm+SP0Len-2	; Last Addr Of Parm Stack   (SP0)(7DFE)
UNmTop		equ	SP0Btm-2	; Start Of User Names       (NP)
UCdBtm		equ	RamBtm		; Start Of User Code	    (CP)
NamTop		equ	RomBtm+RomLen-2	; Initial Name Dictionary
;
; Initialize Assembly Variables
;
_link		=	0		; Force A Null Link First Time
_name		=	NamTop		; Initialize Name Pointer
_code		=	CodBtm		; Initialize Code Pointer
_user		=	4*WrdLen	; First User Variable Offset
;
;******************************************************************************
;
;  eForth Model Register Equates
;
;spl		equ	r0
;rpl		equ	r1
;sph		equ	r4
;rph		equ	r5
;tpl		equ	r2
;tph		equ	r3
;ipl		equ	r6
;iph		equ	r7
gpl		equ	010h
gph		equ	011h
stk		equ	012h
;
;******************************************************************************
;
;  Compile A Code Definition Header.  Align To A Byte Boundary
;
		macro	%code
\2		equ	$
_len		=	(\0 & 01fh)
_name		=	_name-((_len+5))
		org	_name
		dw	\2
		dw	_link
_link		=	$
		db	\0,\1
		org	\2
		endmac
;
;  Compile A Colon Definition Header.
;
		macro	%colon
		%code	\0,\1,\2
		lcall	DOLIST
		endmac
;
;  Compile a user variable header.
;
		macro	%user
		%code	\0,\1,\2
		lcall	DOUSER
		dw	_user
\2_Vec		equ	_user+UP0Btm
_user		=	_user+WrdLen
		endmac
;
;  Compile An Inline String.
;
		macro	%dmm
		dw	\0
_len		=	$
		db	0,\1
_code		=	$
		org	_len
		db	_code-_len-1
		org	_code
		endmac
;
;  During assembly execution, switch to forth.  Complements SASMBLY.
;
		macro	%sforth
		lcall	DOLIST
		endmac
;
;  Pseudo 16 bit TP load
;
		macro	%loadtp
		mov	tpl,#low(\0)
		mov	tph,#high(\0)
		endmac
;
;******************************************************************************
;
;  System Power Up And Reset Entry Point
;
		org	RomBtm
		ljmp	SysStart	; Jump Start System
		reti			; Return From IE0 Interrupt
		db	0,0,0,0,0,0,0	; Filler
		reti			; Return From TF0 Interrupt
		db	0,0,0,0,0,0,0	; Filler
		reti			; Return From IE1 Interrupt
		db	0,0,0,0,0,0,0	; Filler
		reti			; Return From TF1 Interrupt
		db	0,0,0,0,0,0,0	; Filler
		reti			; Return From RI+TI Interrupt
		db	0,0,0,0,0,0,0	; Filler
		reti			; Return From TF2+EXF2 Interrupt
;
;******************************************************************************
;
;  Defaulted User Variables.  Routine COLD Moves These To The User Area.  MUST
;  Be In The Same Order As User Variables Are Defined.
; 
UsrBtm:		dw	0		; Reserved
		dw	0		; Reserved
		dw	0		; Reserved
		dw	0		; Reserved
		dw	SP0Top		; SP0
		dw	RP0Top		; RP0
		dw	QRX		; '?KEY
		dw	TXSTORE		; 'EMIT
		dw	ACCEPT		; 'EXPECT
		dw	KTAP		; 'TAP
		dw	TXSTORE		; 'ECHO
		dw	DOTOK		; 'PROMPT
		dw	DfltBase	; BASE
		dw	0		; tmp
		dw	0		; SPAN
		dw	0		; >IN
		dw	0		; #TIB
		dw	TIBBtm		; TIB
		dw	0		; CSP
		dw	INTER		; 'EVAL
		dw	NUMBERQ		; 'NUMBER
		dw	0		; HLD
		dw	0		; HANDLER
		dw	0		; CONTEXT Pointer
		dw	0		; Vocab Stack 1 (VocDpt)
		dw	0		; Vocab Stack 2
		dw	0		; Vocab Stack 3
		dw	0		; Vocab Stack 4
		dw	0		; Vocab Stack 5
		dw	0		; Vocab Stack 6
		dw	0		; Vocab Stack 7
		dw	0		; Vocab Stack 8
		dw	0		; CURRENT Pointer
		dw	0		; Vocabulary Link Pointer
		dw	UCdBtm		; CP
		dw	UNmTop		; NP
		dw	LstNam		; LAST
		dw	LstNam		; FORTH
		dw	0		; Vocabulary Link
		dw	0		; Current Baud Rate
		dw	1		; UPC
UsrLst		equ	$		; Last Address Of User Area
UsrLen		equ	UsrLst-UsrBtm	; Length Of User Area
;
;******************************************************************************
;
; Start Of Dictionary Code
;
CodBtm:		nop
SysStart:	mov	ie,#000h	; No Interrupts
		mov	sp,#stk		; Stack Starts At 0012h Internal
		mov	psw,#008h	; Select Register Set 1
		mov	rpl,#low(RP0Top); Load Low Of Return Pointer
		mov	rph,#high(RP0Top); Load High Of Return Pointer
		mov	spl,#low(SP0Top); Load Low Of Stack Pointer
		mov	sph,#high(SP0Top); Load High Of Stack Pointer
		mov	p2,sph		; Setup Page Register For Stack High
		ljmp	COLD		; Start User Application
;
;******************************************************************************
;
;  AutoRate - Determine Baud Rate Of Serial Port By Timing Start Bit.  Tables
;	      Are Setup To Allow +-3% Variation In Timing.  If Can't Get A
;	      Good Baud Rate, Just Keep Trying.  Return B/A With Current
;	      Baud Rate.
;
		if	Word_AUTOBAUD
AutoRate:	mov	scon,#052h	; Mode 3 Serial Port
		mov	tmod,#021h	; Timer 1 Mode 2, Timer 0 Mode 1
		mov	sbuf,#000h	; Send A Null For First Character
		setb	tr1		; Start Timer 1 Up
		clr	rs0		; Switch To Register Bank 0
;
;  Wait For Start Bit, Run Timer While Start Bit High
;
Aut_0:		clr	ri		; Ignore Any Character In SBUF
		clr	tr0		; Cancel Timer 0
		mov	th0,#000h	; Clear Timer 0 High
		mov	tl0,#000h	; Clear Timer 0 Low
		jb	p3.0,$		; Wait For Start Bit High
		setb	tr0		; Start Timer 0 Running
		jnb	p3.0,$		; Wait For Start Bit To Go Lo
		clr	tr0		; Cancel Timer 0
;
;  Timer 0 Has Start Bit Period.  Test Against Table.
;
		mov	dptr,#Table	; Point To Table
Aut_1:		mov	a,#0		; Set For MOVC
		movc	a,@a+dptr	; Get High Byte From Table
		cjne	a,th0,Aut_2	; Compare
		mov	a,#1		; Set For MOVC
		movc	a,@a+dptr	; Get Low Byte From Table
		cjne	a,tl0,Aut_2	; Compare
Aut_2:		jc	Aut_4		; If Greater, Try Next Entry
		mov	a,#2		; Set For MOVC
		movc	a,@a+dptr	; Get High Byte From Table
		cjne	a,th0,Aut_3	; Compare
		mov	a,#3		; Set For MOVC
		movc	a,@a+dptr	; Get Low Byte From Table
		cjne	a,tl0,Aut_3	; Compare
		setb	c		; Flip Status
Aut_3:		jnc	Aut_4		; If Less, Try Next Entry
;
		mov	a,#4		; Point To SMOD Status Byte
		movc	a,@a+dptr	; Get SMOD Byte
		mov	pcon,a		; Store It Back
		mov	a,#5		; Point To Speed Byte
		movc	a,@a+dptr	; Get Speed Byte
		mov	th1,a		; Setup Baud Rate Timer
		mov	tl1,a		; Setup Baud Rate Timer
		jnb	ri,$		; Wait For Character
		clr	ri		; Say Character Received Isn't There
		mov	a,#6		; Offset To Baud Rate High
		movc	a,@a+dptr	; Get High Of Baud Rate In Binary
		mov	b,a		; Store In B
		mov	a,#7		; Offset To Baud Rate Low
		movc	a,@a+dptr	; Get Low Of Baud Rate In Binary
		setb	rs0		; Back To Register Bank 1
		ret			; Return To Caller
;
Aut_4:		mov	a,#8		; Number Bytes In Record
		add	a,dpl		; Add In DPL
		mov	dpl,a		; Back To DPL
		jnc	Aut_5		; If No Carry, Skip
		inc	dph		; Increment High Of DPTR
Aut_5:		movx	a,@dptr		; Get Byte
		cjne	a,#-1,Aut_1	; While Not -1, Loop
		sjmp	Aut_0		; Try Again
;
;  All Values Calculated For 11.059200Mhz
;
Table:		dw	6328, 5960, 00040h, 00150; 150
		dw	3164, 2980, 000a0h, 00300; 300
		dw	2109, 1987, 000c0h, 00450; 450
		dw	1582, 1490, 000d0h, 00600; 600
		dw	0791, 0745, 000e8h, 01200; 1200
		dw	0527, 0497, 000f0h, 01800; 1800
		dw	0396, 0372, 000f4h, 02400; 2400
		dw	0264, 0248, 000f8h, 03600; 3600
		dw	0198, 0186, 000fah, 04800; 4800
		dw	0132, 0124, 000fch, 07200; 7200
		dw	0099, 0093, 000fdh, 09600; 9600
		dw	0066, 0062, 000feh, 14400; 14400
		dw	0049, 0047, 080fdh, 19200; 19200
		dw	-1
		endif
;
;******************************************************************************
;
; RETURN - The Forth Inner Interpreter
;
PopReturn:	lcall	PopSP
Return: 	mov	dpl,ipl
		mov	dph,iph
AltReturn:	movx	a,@dptr
		mov	b,a
		inc	dptr
		movx	a,@dptr
		mov	dph,b
		mov	dpl,a
		mov	a,ipl
		add	a,#002h
		mov	ipl,a
		jnc	Ret_1
		inc	iph
Ret_1:		clr	a
		jmp	@a+dptr
;
PopSP:		inc	spl
		movx	a,@spl
		mov	tph,a
		inc	spl
		movx	a,@spl
		mov	tpl,a
		ret
;
PushSP:		mov	a,tpl
		movx	@spl,a
		dec	spl
		mov	a,tph
		movx	@spl,a
		dec	spl
		ret
;
PopDP:		inc	spl
		movx	a,@spl
		mov	dph,a
		inc	spl
		movx	a,@spl
		mov	dpl,a
		ret
;
PushDP:		mov	a,dpl
		movx	@spl,a
		dec	spl
		mov	a,dph
		movx	@spl,a
		dec	spl
		ret
;
PopIP:		mov	p2,rph
		inc	rpl
		movx	a,@rpl
		mov	iph,a
		inc	rpl
		movx	a,@rpl
		mov	ipl,a
		mov	p2,sph
		ret
;
NextIPRet: 	mov	dpl,ipl	
		mov	dph,iph
		movx	a,@dptr
		mov	iph,a
		inc	dptr
		movx	a,@dptr
		mov	ipl,a
		ljmp	Return
;
LoadNextIP:	mov	a,ipl
		add	a,#2
		mov	ipl,a
		jnc	Loa_1
		inc	iph
Loa_1:		ret
;
PushIPRS:	mov	p2,rph
		mov	a,ipl
		movx	@rpl,a
		dec	rpl
		mov	a,iph
		movx	@rpl,a
		dec	rpl
		mov	p2,sph
		ret
;
PopSPRS:	mov	p2,rph
		inc	rpl
		movx	a,@rpl
		mov	tph,a
		inc	rpl
		movx	a,@rpl
		mov	tpl,a
		mov	p2,sph
		ret
;
PopDPRS:	mov	p2,rph
		inc	rpl
		movx	a,@rpl
		mov	dph,a
		inc	rpl
		movx	a,@rpl
		mov	dpl,a
		mov	p2,sph
		ret
;
PushSPRS:	mov	p2,rph
		mov	a,tpl
		movx	@rpl,a
		dec	rpl
		mov	a,tph
		movx	@rpl,a
		dec	rpl
		mov	p2,sph
		ret
;
PushDPRS:	mov	p2,rph
		mov	a,dpl
		movx	@rpl,a
		dec	rpl
		mov	a,dph
		movx	@rpl,a
		dec	rpl
		mov	p2,sph
		ret
;
;******************************************************************************
;
; The Kernel
;
;   doLIT	( -- w )
;		Push an inline literal.
;
		%code	Lex_CO+5,'doLIT',DOLIT
		lcall	LitConCmn
		inc	dptr
		mov	ipl,dpl
		mov	iph,dph
		ljmp	AltReturn
;
;   doCON	( -- a )
;		Run time routine for CONSTANT
;
		%code	Lex_CO+5,'doCON',DOCON
		lcall	LitConCmn
		lcall	PopIP
		ljmp	Return
;
LitConCmn:	lcall	PushSP	
		mov	dpl,ipl
		mov	dph,iph
		movx	a,@dptr
		mov	tph,a
		inc	dptr
		movx	a,@dptr
		mov	tpl,a
		ret
;
;   doLIST	( a -- )
;		Process colon list.
;
		%code	Lex_CO+6,'doLIST',DOLIST
		lcall	PushIPRS
		pop	iph
		pop	ipl
		ljmp	Return
;
;   (NEXT)	( -- )
;		Run time code for the single index loop.
;		R> R> DUP IF 1 - >R @ >R EXIT THEN DROP CELL+ >R ;
;
		%code	Lex_CO+6,'(NEXT)',PNEXTP
		mov	p2,rph
		inc	rpl
		inc	rpl
		movx	a,@rpl
		clr	c
		subb	a,#001h
		movx	@rpl,a
		dec	rpl
		movx	a,@rpl
		subb	a,#000h
		movx	@rpl,a
		inc	rpl
		mov	p2,sph
		jnc	Nex_1
		lcall	LoadNextIP
		ljmp	Return
;
Nex_1:		dec	rpl
		dec	rpl
		ljmp	NextIPRet
;
;   ?branch	( f -- )
;		Branch if flag is zero.
;
		%code	Lex_CO+7,'?branch',QBRAN
		mov	a,tpl
		orl	a,tph
		jz	Qbr_1
		lcall	LoadNextIP
		ljmp	PopReturn
Qbr_1:		lcall	PopSP
		ljmp	NextIPRet
;
;   branch	( -- )
;		Branch to an inline address.
;
		%code	Lex_CO+6,'branch',BRAN
		ljmp	NextIPRet
;
;   EXECUTE	( cfa -- )
;		Execute the word at cfa.
;
		%code	7,'EXECUTE',EXECUTE
		mov	dpl,tpl
		mov	dph,tph
		lcall	PopSP
		clr	a
		jmp	@a+dptr
;
;   EXIT	( -- )
;		Terminate a colon definition.
;
		%code	4,'EXIT',EXIT
		lcall	PopIP
		ljmp	Return
;
;   !		( n1 a1 -- )
;		Pop the data stack to memory.
;
		%code	1,'!',STORE
		mov	dpl,tpl
		mov	dph,tph
		inc	spl
		movx	a,@spl
		movx	@dptr,a
		inc	dptr
		inc	spl
		movx	a,@spl
		movx	@dptr,a
		ljmp	PopReturn
;
;   @		( a -- w )
;		Push memory location to the data stack.
;
		%code	1,'@',GET
		mov	dpl,tpl
		mov	dph,tph
		movx	a,@dptr
		mov	tph,a
		inc	dptr
		movx	a,@dptr
		mov	tpl,a
		ljmp	Return
;
;   C!		( c a -- )
;		Pop the data stack to byte memory.
;
		%code	2,'C!',CSTORE
		mov	dpl,tpl
		mov	dph,tph
		inc	spl
		inc	spl
		movx	a,@spl
		movx	@dptr,a
		ljmp	PopReturn
;
;   C@		( b -- c )
;		Push byte memory location to the data stack.
;
		%code	2,'C@',CAT
		mov	dpl,tpl
		mov	dph,tph
		movx	a,@dptr
		mov	tpl,a
		mov	tph,#000h
		ljmp	Return
;
;   >R		( w -- )
;		Push the data stack to the return stack.
;
		%code	Lex_CO+2,'>R',TOR
		lcall	PushSPRS
		ljmp	PopReturn
;
;   R@		( -- w )
;		Copy top of return stack to the data stack.
;
		%code	2,'R@',RAT
		lcall	PushSP
		mov	dpl,rpl
		mov	dph,rph
		inc	dpl
		movx	a,@dptr
		mov	tph,a
		inc	dpl
		movx	a,@dptr
		mov	tpl,a
		ljmp	Return
;
;   R>		( -- w )
;		Pop the return stack to the data stack.
;
		%code	2,'R>',RFROM
		lcall	PushSP
		lcall	PopSPRS
		ljmp	Return
		ret
;
;   RP@		( -- a )
;		Push the current RP to the data stack.
;
		%code	3,'RP@',RPAT
		lcall	PushSP
		mov	tpl,rpl
		mov	tph,rph
		ljmp	Return
;
;   RP!		( a -- )
;		Set the return stack pointer.
;
		%code	Lex_CO+3,'RP!',RPSTORE
		mov	rpl,tpl
		mov	rph,tph
		ljmp	PopReturn
;
;   SP@		( -- a )
;		Push the current data stack pointer.
;
		%code	3,'SP@',SPAT
		lcall	PushSP
		mov	tpl,spl
		mov	tph,sph
		ljmp	Return
;
;   SP!		( a -- )
;		Set the data stack pointer.
;
		%code	3,'SP!',SPSTORE
		mov	spl,tpl
		mov	sph,tph
		ljmp	PopReturn
;
;   DUP		( w -- w w )
;		Duplicate the top stack item.
;
		%code	3,'DUP',DUP
		lcall	PushSP
		ljmp	Return
;
;   ?DUP	( w -- w w | 0 )
;		Dup tos if its is not zero.
;
		%code	4,'?DUP',QDUP
		mov	a,tpl
		orl	a,tph
		jz	Qdu_1
		lcall	PushSP
Qdu_1:		ljmp	Return
;
;   DROP	( w -- )
;		Discard top stack item.
;
		%code	4,'DROP',DROP
		ljmp	PopReturn
;
;   SWAP	( w1 w2 -- w2 w1 )
;		Exchange top two stack items.
;
		%code	4,'SWAP',_SWAP
		inc	spl
		movx	a,@spl
		xch	a,tph
		movx	@spl,a
		inc	spl
		movx	a,@spl
		xch	a,tpl
		movx	@spl,a
		dec	spl
		dec	spl
		ljmp	Return
;
;   OVER	( w1 w2 -- w1 w2 w1 )
;		Copy second stack item to top.
;
		%code	4,'OVER',OVER
		mov	dpl,spl
		mov	dph,sph
		lcall	PushSP
		inc	dpl
		movx	a,@dptr
		mov	tph,a
		inc	dpl
		movx	a,@dptr
		mov	tpl,a
		ljmp	Return
;
;   0<		( n -- t )
;		Return true if n is negative.
;
		%code	2,'0<',ZLESS
		mov	a,tph
		rlc	a
		clr	a
		subb	a,#000h
		mov	tph,a
		mov	tpl,a
		ljmp	Return
		mov	a,tph
		jnb	acc.7,Zle_1
		mov	a,#0ffh
		sjmp	Zle_2
Zle_1:		clr	a
Zle_2:		mov	tph,a
		mov	tpl,a
		ljmp	Return
;
;   AND		( w w -- w )
;		Bitwise AND.
;
		%code	3,'AND',AND
		inc	spl
		movx	a,@spl
		anl	tph,a
		inc	spl
		movx	a,@spl
		anl	tpl,a
		ljmp	Return
;
;   OR		( w w -- w )
;		Bitwise inclusive OR.
;
		%code	2,'OR',OR
		inc	spl
		movx	a,@spl
		orl	tph,a
		inc	spl
		movx	a,@spl
		orl	tpl,a
		ljmp	Return
;
;   XOR		( w w -- w )
;		Bitwise exclusive OR.
;
		%code	3,'XOR',XOR
		inc	spl
		movx	a,@spl
		xrl	tph,a
		inc	spl
		movx	a,@spl
		xrl	tpl,a
		ljmp	Return
;
;   UM+		( w w -- w cy )
;		Add two numbers, return the sum and carry flag.
;
		%code	3,'UM+',UMPLUS
		inc	spl
		inc	spl
		movx	a,@spl
		add	a,tpl
		movx	@spl,a
		dec	spl
		movx	a,@spl
		addc	a,tph
		movx	@spl,a
		dec	spl
		clr	a
		mov	tph,a
		addc	a,tph
		mov	tpl,a
		ljmp	Return
;
;******************************************************************************
;
;  Device Dependent I/O
;
;   !IO		( -- )
;		Initialize the serial I/O devices.
;
		%code	3,'!IO',STOIO
		if	!Word_AUTOBAUD
		mov	pcon,#080h	; Select Double Speed
		mov	scon,#052h	; Mode 3 Serial Port
		mov	tmod,#021h	; Timer 1 Mode 2, Timer 0 Mode 1
		mov	th1,#0fdh	; 19200 Baud
		mov	sbuf,#000h	; Send A Null For First Character
		setb	tr1		; Start Timer 1 Up
		endif
		ljmp	Return
;
;   ?RX		( -- c T | F )
;		Return input character and true, or a false if no input.
;
		%code	3,'?RX',QRX
		lcall	PushSP
		jnb	ri,Qrx_1
		clr	ri
		mov	tpl,sbuf
		mov	tph,#000h
		lcall	PushSP
		%loadtp	-1
		ljmp	Return
Qrx_1:		%loadtp	0
		ljmp	Return
;
;   TX!		( c -- )
;		Send character c to the output device.
;
		%code	3,'TX!',TXSTORE
		jnb	ti,$
		clr	ti
		mov	sbuf,tpl
		ljmp	PopReturn
;
;******************************************************************************
;
;  My Functions!
;
		if	1
OutSpace	push	acc
		mov	a,#' '
		lcall	OutChar
		pop	acc
		ret
;
OutDPTR		push	acc
		mov	a,dph
		lcall	OutHex2
		mov	a,dpl
		lcall	OutHex2
		pop	acc
		ret
;
OutR2R3		push	acc
		mov	a,r2
		lcall	OutHex2
		mov	a,r3
		lcall	OutHex2
		pop	acc
		ret
;
OutR4R5		push	acc
		mov	a,r4
		lcall	OutHex2
		mov	a,r5
		lcall	OutHex2
		pop	acc
		ret
;
OutR6R7		push	acc
		mov	a,r6
		lcall	OutHex2
		mov	a,r7
		lcall	OutHex2
		pop	acc
		ret
;
OutIP		push	acc
		mov	a,iph
		lcall	OutHex2
		mov	a,ipl
		lcall	OutHex2
		pop	acc
		ret
;
OutTP		push	acc
		mov	a,tph
		lcall	OutHex2
		mov	a,tpl
		lcall	OutHex2
		pop	acc
		ret
;
OutHex2		push	acc	
		swap	a
		lcall	OutHex
		pop	acc
		lcall	OutHex
		ret
;
OutHex		anl	a,#00fh
		add	a,#090h
		da	a
		addc	a,#040h
		da	a
		lcall	OutChar
		ret
;
OutChar		jnb	ti,$
		clr	ti
		mov	sbuf,a
		ret
		endif
;
;   SASMBLY	( -- )
;		During forth, switch to embedded assembly
;
		%code	7,'SASMBLY',SASMBLY
		mov	dpl,ipl
		mov	dph,iph
		lcall	PopIP
		clr	a
		jmp	@a+dptr
;
;   ONE		( -- 1 )
;		Push 1 to stack top
;
		%code	1,'1',ONE
		lcall	PushSP
		%loadtp	1
		ljmp	Return
;
;   ZERO	( -- 0 )
;		Push 0 to stack top
;
		%code	4,'ZERO',ZERO
		lcall	PushSP
		clr	a
		mov	tpl,a
		mov	tph,a
		ljmp	Return
;
;   -1		( -- 0 )
;		Push -1 to stack top
;
		%code	2,'-1',NEGONE
		lcall	PushSP
		mov	a,#0ffh
		mov	tpl,a
		mov	tph,a
		ljmp	Return
;
;   1+		( n1 -- n1+1 )
;		Add 1 to top value on stack
;
		%code	2,'1+',ONEPLUS
		mov	a,tpl
		add	a,#001h
		mov	tpl,a
		jnc	OneP_1
		inc	tph
OneP_1:		ljmp	Return
;
;   1-		( n1 -- n1-1 )
;		Subtract 1 from top value on stack
;
		%code	2,'1-',ONEMINUS
		clr	c
		mov	a,tpl
		subb	a,#001h
		mov	tpl,a
		jnc	OneM_1
		dec	tph
OneM_1:		ljmp	Return
;
;   2+		( n1 -- n1+2 )
;		Add 2 to top value on stack
;
		%code	2,'2+',TWOPLUS
		mov	a,tpl
		add	a,#002h
		mov	tpl,a
		jnc	TwoP_1
		inc	tph
TwoP_1:		ljmp	Return
;
;   2-		( n1 -- n1-2 )
;		Subtract 2 from top value on stack
;
		%code	2,'2-',TWOMINUS
		clr	c
		mov	a,tpl
		subb	a,#002h
		mov	tpl,a
		jnc	TwoM_1
		dec	tph
TwoM_1:		ljmp	Return
;
;   2*		( u1 -- u1*2 )
;		Unsigned multiply by 2
;
		%code	2,'2*',TWOMULT
		clr	c
		mov	a,tpl
		rlc	a
		mov	tpl,a
		mov	a,tph
		rlc	a
		mov	tph,a
		ljmp	Return
;
;   2/		( u1 -- u1/2 )
;		Unsigned divide by 2
;
		%code	2,'2/',TWODIVIDE
		clr	c
		mov	a,tph
		rrc	a
		mov	tph,a
		mov	a,tpl
		rrc	a
		mov	tpl,a
		ljmp	Return
;
;   DUP>R	( n1 -- n1 )
;		Duplicate and move TOS to return stack
;
		%code	Lex_CO+5,'DUP>R',DUPTOR
		lcall	PushSPRS
		ljmp	Return
;
;   RDROP	( -- )
;		Drop top value from return stack
;
		%code	6,'R>DROP',RDROP
		inc	rpl
		inc	rpl
		ljmp	Return
;
;   NIP		( n1 n2 -- n2 )
;		Discard second item on stack
;
		%code	3,'NIP',NIP
		inc	spl
		inc	spl
		ljmp	Return
;
;   -ROT	( n1 n2 n3 -- n3 n1 n2 )
;		Move top item on stack down 2
;		_SWAP >R _SWAP R> EXIT
;
		%code	4,'-ROT',MROT
		mov	b,rpl
		inc	spl
		mov	rpl,spl
		inc	rpl
		inc	rpl
		movx	a,@spl
		push	acc
		movx	a,@rpl
		movx	@spl,a
		mov	a,tph
		movx	@rpl,a
		pop	tph
		inc	rpl
		inc	spl
		movx	a,@spl
		push	acc
		movx	a,@rpl
		movx	@spl,a
		mov	a,tpl
		movx	@rpl,a
		pop	tpl
		dec	spl
		dec	spl
		mov	rpl,b
		ljmp	Return
;
;   P1@		( -- n1 )
;		Fetch port 1 value to TOS
;
		%code	3,'P1@',READP1
		mov	tpl,p1
		mov	tph,#000h
		ljmp	Return
;
;   P1!		( n1 -- )
;		Store TOS to port 1
;
		%code	3,'P1!',WRITEP1
		mov	p1,tpl
		ljmp	PopReturn
;
;   P3@		( -- n1 )
;		Fetch port 3 value to TOS
;
		%code	3,'P3@',READP3
		mov	tpl,p3
		mov	tph,#000h
		ljmp	Return
;
;   P3!		( n1 -- )
;		Store TOS to port 3
;
		%code	3,'P3!',WRITEP3
		mov	p3,tpl
		ljmp	PopReturn
;
;   C,		( c1 -- )
;		Compile a character (byte) into the code dictionary
;
		%colon	2,'C,',CCOMMA
		dw	HERE		; HERE
		dw	DUP		; DUP
		dw	ONEPLUS		; 1+
		dw	CP		; CP
		dw	STORE		; !
		dw	CSTORE		; C!
		dw	EXIT		; EXIT
;
;   NOP		( -- )
;		Doesn't do anything... Great for vectored execution hooks!
;
		%code	3,'NOP',DONOP
		ljmp	Return
;
;   CSAME?	( b1 b2 u1 -- b1 b2 -0+ )
;		Compare b1 to b2 for u1 bytes.  Return 0 if identical.
;
		%code	6,'CSAME?',CSAMEQ
		mov	dpl,tpl
		mov	dph,tph
		mov	a,spl
		clr	rs0
		mov	r7,dpl
		mov	r6,dph
		mov	spl,a
		inc	spl
		movx	a,@spl
		mov	r5,a
		inc	spl
		movx	a,@spl
		mov	r4,a
		lcall	PopDP
		mov	a,r7
		jz	Csa_1
		inc	r6
Csa_1:		movx	a,@dptr
		mov	b,a
		inc	dptr
		xch	a,dpl
		xch	a,r4
		xch	a,dpl
		xch	a,dph
		xch	a,r5
		xch	a,dph
		movx	a,@dptr
		cjne	a,b,Csa_3
		inc	dptr
		xch	a,dpl
		xch	a,r4
		xch	a,dpl
		xch	a,dph
		xch	a,r5
		xch	a,dph
Csa_2:		djnz	r7,Csa_1
		djnz	r6,Csa_1
		setb	rs0
		%loadtp	0
		ljmp	Return
Csa_3: 		setb	rs0
		clr	c
		xch	a,b
		subb	a,b
		mov	tpl,a
		clr	a
		subb	a,#000h
		mov	tph,a
		ljmp	Return
;
;   EVEN	( u1 -- f )
;		Return -1 if unsigned number is even, 0 if not
;
		%code	4,'EVEN',EVEN
		mov	a,tpl
		%loadtp	-1
		jnb	acc.0,Eve_1
		%loadtp	0
Eve_1:		ljmp	Return
;
;   ODD		( u1 -- f )
;		Return -1 if unsigned number is odd, 0 if not
;
		%code	3,'ODD',ODD
		mov	a,tpl
		%loadtp	-1
		jb	acc.0,Odd_1
		%loadtp	0
Odd_1:		ljmp	Return
;
;   CBITS	( n -- n )
;		Count number of set bits in a word
;
		%code	5,'CBITS',CBITS
		mov	b,#000h
Cbi_1:		clr	c
		mov	a,tpl
		rlc	a
		mov	tpl,a
		mov	a,tph
		rlc	a
		mov	tph,a
		jnc	Cbi_2
		inc	b
Cbi_2:		mov	a,tpl
		orl	a,tph
		jnz	Cbi_1
		mov	tph,#000h
		mov	tpl,b
		ljmp	Return
;
;   AUTOBAUD	( -- )
;		Automatically determine baud rate of console. 
;
		if	Word_AUTOBAUD
		%code	8,'AUTOBAUD',AUTOBAUD
		lcall	AutoRate
		mov	dptr,#BAUD_Vec
		xch	a,b
		movx	@dptr,a
		inc	dptr
		xch	a,b
		movx	@dptr,a
		ljmp	Return
		endif
;
;   CI@		( a -- c )
;		Fetch byte from internal memory
;
		%code	3,'CI@',CIAT
		mov	b,rpl
		mov	rpl,tpl
		mov	a,@rpl
		mov	tpl,a
		mov	tph,#000h
		mov	rpl,b
		ljmp	Return
;
;   CI!		( c a -- )
;		Store byte to internal memory
;
		%code	3,'CI!',CISTORE
		mov	b,rpl
		mov	rpl,tpl
		inc	spl
		inc	spl
		movx	a,@spl
		mov	@rpl,a
		mov	rpl,b
		ljmp	PopReturn
;
;   FREE	( -- )
;		Display bytes between 
;
		%colon	4,'FREE',FREE
		dw	NP		; NP
		dw	GET		; @
		dw	CP		; CP
		dw	GET		; @
		dw	SUB		; -
		dw	DOT		; .
		%dmm	PDOTQP,'bytes free'
		dw	CR		; CR
		dw	EXIT		; EXIT
;
;   ERASE	( b u -- )
;		Fill buffer 'b' of length 'u' with binary 0
;
		%colon	5,'ERASE',ERASE
		dw	ZERO		; 0
		dw	FILL		; FILL
		dw	EXIT		; EXIT
;
;   BLANKS	( b u -- )
;		Fill buffer 'b' of length 'u' with the space character
;
		%colon	6,'BLANKS',BLANKS
		dw	DOLIT		; LITERAL
		dw	32		; 32
		dw	FILL		; FILL
		dw	EXIT		; EXIT
;
;   JOIN	( u1 u2 -- u3 )
;		Joins the lower byte of u2 as high of u3, low of u1 as low of u3
;
		%code	4,'JOIN',JOIN
		mov	b,tpl
		lcall	PopSP
		mov	tph,b
		ljmp	Return
;
;   SPLIT	( u -- u1 u2 )
;		Splits lower byte of u to u1, upper byte of u to u2
;
		%code	5,'SPLIT',SPLIT
		mov	b,tph
		mov	tph,#000h
		lcall	PushSP
		mov	tpl,b
		ljmp	Return
;
;   FLIP	( u -- u )
;		Flips high and low nibbles of u
;
		%code	4,'FLIP',FLIP
		mov	a,tpl
		mov	tpl,tph
		mov	tph,a
		ljmp	Return
;
;   TOGGLE	( a b -- )
;		XOR byte at 'a' with bit pattern 'b'
;
		%code	6,'TOGGLE',TOGGLE
		mov	b,tpl
		lcall	PopDP
		movx	a,@dptr
		xrl	a,b
		movx	@dptr,a
		ljmp	PopReturn
;
;   UPPER	( a n -- )
;		Convert 'n' bytes starting at 'a' to uppercase
;
		%code	5,'UPPER',UPPER
		mov	a,tpl
		jz	Upp_1
		inc	tph
Upp_1:		lcall	PopDP
Upp_2:		movx	a,@dptr
		cjne	a,#'a',Upp_3
Upp_3:		jc	Upp_5
		cjne	a,#'{',Upp_4
Upp_4:		jnc	Upp_5
		anl	a,#0dfh
		movx	@dptr,a
Upp_5:		inc	dptr
		djnz	tpl,Upp_2
		djnz	tph,Upp_2
		ljmp	PopReturn
;
;   S->D	( n -- d )
;		Promote single 'n' to double 'd'
;
		%code	4,'S->D',SNGLTODBL
		lcall	PushSP
		rlc	a
		clr	a
		subb	a,#000h
		mov	tpl,a
		mov	tph,a
		ljmp	Return
;
;   +-		( n1 n2 -- n3 )
;		Apply sign of n1 to n2 and leave as n3
;
		%code	2,'+-',PLUSMINUS
		mov	a,tph
		rlc	a
		lcall	PopSP
		jnc	Plu_1
		ljmp	NEGATE
Plu_1:		ljmp	Return
;
;   D+-		( d1 n -- d2 )
;		Apply sign of n to d1 and leave as d2
;
		%code	3,'D+-',DPLUSMINUS
		mov	a,tph
		rlc	a
		lcall	PopSP
		jnc	Dpl_1
		lcall	DblNeg
Dpl_1:		ljmp	Return
;
;   DABS	( d -- d )
;		Leave absolute value of d
;
		%code	4,'DABS',DABS
		mov	a,tph
		jnb	acc.7,Dab_1
		lcall	DblNeg
Dab_1:		ljmp	Return
;
;   D+		( d1 d2 -- d3 )
;		Add d1 and d2, returing d3 as result
;
		%code	2,'D+',DPLUS
		lcall	PopDP
		mov	gph,tph
		mov	gpl,tpl
		lcall	PopSP
		push	tpl
		push	tph
		lcall	PopSP
		mov	a,dpl
		add	a,tpl
		mov	tpl,a
		mov	a,dph
		addc	a,tph
		mov	tph,a
		lcall	PushSP
		pop	dph
		pop	acc
		addc	a,gpl
		mov	tpl,a
		mov	a,dph
		addc	a,gph
		mov	tph,a
		ljmp	Return
;
;   0>		( n -- f )
;		Return -1 if n > 0, else return 0
;
		%code	2,'0>',ZGREATER
		clr	c
		mov	a,tph
		orl	a,tpl
		jz	Zgr_1
		mov	a,tph
		rlc	a
		cpl	c
Zgr_1:		clr	a
		subb	a,#000h
		mov	tph,a
		mov	tpl,a
		ljmp	Return
;
;   MU/MOD	( d u -- r dq )
;		Unsigned divide of d by u, leaving remainder r, and dbl qoutient
;		>R 0 R@ UM/MOD R> SWAP >R UM/MOD R> EXIT
;
		%code	6,'MU/MOD',MUMOD
		lcall	MUMODSub
		ljmp	Return
;
MUMODSub:	push	tph
		push	tpl
		clr	a
		movx	@spl,a
		dec	spl
		movx	@spl,a
		dec	spl
		lcall	UMMODSub
		mov	dph,tph
		mov	dpl,dpl
		pop	tpl
		pop	tph
		push	dph
		push	dpl
		lcall	UMMODSub
		lcall	PushSP
		pop	tpl
		pop	tph
		ret
;
;   TUCK	( n1 n2 -- n2 n1 n2 )
;		Copy top of stack underneath second item on stack
;
		%code	4,'TUCK',TUCK
		lcall	PopDP
		lcall	PushSP
		lcall	PushDP
		ljmp	Return
;
;   CONSTANT	( n | name -- )
;		Create a constant of name 'name', returning value n
;
		%colon	8,'CONSTANT',CONSTANT
		dw	TOKEN		; TOKEN
		dw	SNAME		; $,n
		dw	OVERT		; OVERT
		dw	DOLIT		; LITERAL
		dw	DOLIST		; doLST
		dw	CALLC		; CALL,
		dw	COMPILE		; COMPILE
		dw	DOCON		; doCON
		dw	COMMA		; ,
		dw	EXIT		; EXIT
;
;   (DO)	( l i -- )
;		Runtime routine of DO
;
		%code	Lex_CO+4,'(DO)',PDOP
		mov	dph,tph
		mov	dpl,tpl
		lcall	PopSP
Pdo_1:		if	IM_LEAVE
		lcall	PushIPRS
		lcall	LoadNextIP
		endif
		mov	a,tph
		add	a,#080h
		mov	tph,a
		lcall	PushSPRS
		clr	c
		mov	a,dpl
		subb	a,tpl
		mov	tpl,a
		mov	a,dph
		subb	a,tph
		mov	tph,a
		lcall	PushSPRS
		ljmp	PopReturn
;
;   (?DO)	( l i -- )
;		Runtime routine of ?DO
;
		%code	Lex_CO+5,'(?DO)',PQDOP
		mov	dph,tph
		mov	dpl,tpl
		lcall	PopSP
		mov	a,dph
		cjne	a,tph,Pdo_1
		mov	a,dpl
		cjne	a,tpl,Pdo_1
		lcall	PopSP
		ljmp	NextIPRet
;
;   (LOOP)	( -- )
;		Runtime routine of LOOP
;
		%code	Lex_CO+6,'(LOOP)',PLOOPP
		lcall	PopDPRS
		mov	gpl,#1
		mov	gph,#0
Plo_1:		mov	a,gpl
		add	a,dpl
		mov	dpl,a
		mov	a,gph
		addc	a,dph
		mov	dph,a
		jnb	psw.2,Plo_2
		inc	rpl
		inc	rpl
		if	IM_LEAVE
		inc	rpl
		inc	rpl
		endif
		lcall	LoadNextIP
		ljmp	Return
Plo_2:		lcall	PushDPRS
		ljmp	NextIPRet
;
;   (+LOOP)	( -- )
;		Runtime routine of +LOOP
;
		%code	Lex_CO+7,'(+LOOP)',PPLOOPP
		lcall	PopDPRS
		mov	gpl,tpl
		mov	gph,tph
		lcall	PopSP
		sjmp	Plo_1
;
;   (?LEAVE)	( f -- )
;		Runtime routine of ?LEAVE
;
		%code	Lex_CO+8,'(?LEAVE)',PQLEAVEP
		mov	a,tph
		orl	a,tpl
		mov	b,a
		lcall	PopSP
		mov	a,b
		jnz	PLEAVEP
		ljmp	Return
;
;   (LEAVE)	( -- )
;		Runtime routine of LEAVE
;
		if	IM_LEAVE
		%code	Lex_CO+7,'(LEAVE)',PLEAVEP
		inc	rpl
		inc	rpl
		inc	rpl
		inc	rpl
		lcall	PopIP
		ljmp	NextIPRet
		endif
;
;   (LEAVE)	( -- )
;		Runtime routine of LEAVE
;
		if	!IM_LEAVE
		%code	Lex_CO+7,'(LEAVE)',PLEAVEP
		lcall	PopDPRS
		mov	gph,dph
		mov	gpl,dpl
		lcall	PopDPRS
		mov	a,dpl
		add	a,gpl
		mov	dpl,a
		mov	a,dph
		addc	a,gph
		add	a,#080h
		mov	dph,a
		inc	dptr
		lcall	PushDPRS
		mov	dptr,#07fffh
		lcall	PushDPRS
		ljmp	Return
		endif
;
;   ?DO		( l i -- )
;		Compile time word for start of a DO loop
;
		%colon	Lex_IM+3,'?DO',QDO
		dw	COMPILE		; COMPILE
		dw	PQDOP		; (?DO)
		dw	ZERO		; 0
		dw	COMMA		; ,
		dw	HERE		; HERE
		dw	EXIT		; EXIT
;
;   DO		( l i -- )
;		Compile time word for start of a DO loop
;
		%colon	Lex_IM+2,'DO',DO
		dw	COMPILE		; COMPILE
		dw	PDOP		; (DO)
		dw	ZERO		; 0
		dw	COMMA		; ,
		dw	HERE		; HERE
		dw	EXIT		; EXIT
;
;   UNDO	( -- )
;		Clean stack so we exit from a DO loop, as in "UN"DO
;
		%code	4,'UNDO',UNDO
		mov	a,rpl
		if	IM_LEAVE
		add	a,#6
		else
		add	a,#4
		endif
		mov	rpl,a
		ljmp	Return
;
;   LOOP	( -- )
;		Compile time word for end of a LOOP
;
		%colon	Lex_IM+4,'LOOP',LOOP
		dw	COMPILE		; COMPILE
		dw	PLOOPP		; (LOOP)
		dw	DUP		; DUP
		dw	COMMA		; ,
		dw	HERE		; HERE
		dw	_SWAP		; SWAP
		dw	TWOMINUS	; 2-
		dw	STORE		; !
		dw	EXIT		; EXIT
;
;   +LOOP	( n -- )
;		Compile time word for end of a +LOOP
;
		%colon	Lex_IM+5,'+LOOP',PLOOP
		dw	COMPILE		; COMPILE
		dw	PPLOOPP		; (+LOOP)
		dw	DUP		; DUP
		dw	COMMA		; ,
		dw	HERE		; HERE
		dw	_SWAP		; SWAP
		dw	TWOMINUS	; 2-
		dw	STORE		; !
		dw	EXIT		; EXIT
;
;   LEAVE	( -- )
;		Compile time word for end of a LEAVE
;
		%colon	Lex_IM+5,'LEAVE',LEAVE
		dw	COMPILE		; COMPILE
		dw	PLEAVEP		; (LEAVE)
		dw	EXIT		; EXIT
;
;   ?LEAVE	( -- )
;		Compile time word for end of a ?LEAVE
;
		%colon	Lex_IM+6,'?LEAVE',QLEAVE
		dw	COMPILE		; COMPILE
		dw	PQLEAVEP	; (?LEAVE)
		dw	EXIT		; EXIT
;
;   BYE		( -- )
;		Restart system with jump to 0
;
		%code	3,'BYE',BYEBYE
		ljmp	SysStart
;
;******************************************************************************
;
;  System And User Variables
;
;   doVAR	( -- a )
;		Run time routine for VARIABLE and CREATE.
;
		%code	Lex_CO+5,'doVAR',DOVAR
		lcall	PushSP
		mov	tpl,ipl
		mov	tph,iph
		lcall	PopIP
		ljmp	Return
;
;   UP		( -- a )
;		Pointer to the user area.
;
		%colon	2,'UP',UP
		dw	DOVAR		; doVAR
		dw	UP0Btm		; UP0Btm
;
;   doUSER	( -- a )
;		Run time routine for user variables.
;		R> @ UP @ + EXIT
;
		%code	Lex_CO+6,'doUSER',DOUSER
		lcall	PushSP
		pop	dph
		pop	dpl
		movx	a,@dptr
		mov	tph,a
		inc	dptr
		movx	a,@dptr
		add	a,#low(UP0Btm)
		mov	tpl,a
		mov	a,tph
		addc	a,#high(UP0Btm)
		mov	tph,a
		ljmp	Return
;
;   SP0		( -- a )
;		Pointer to bottom of the data stack.
;
		%user	3,'SP0',SZERO
;
;   RP0		( -- a )
;		Pointer to bottom of the return stack.
;
		%user	3,'RP0',RZERO
;
;   '?KEY	( -- a )
;		Execution vector of ?KEY.
;
		%user	5,"'?KEY",TQKEY
;
;   'EMIT	( -- a )
;		Execution vector of EMIT.
;
		%user	5,"'EMIT",TEMIT
;
;   'EXPECT	( -- a )
;		Execution vector of EXPECT.
;
		%user	7,"'EXPECT",TEXPECT
;
;   'TAP	( -- a )
;		Execution vector of TAP.
;
		%user	4,"'TAP",TTAP
;
;   'ECHO	( -- a )
;		Execution vector of ECHO.
;
		%user	5,"'ECHO",TECHO
;
;   'PROMPT	( -- a )
;		Execution vector of PROMPT.
;
		%user	7,"'PROMPT",TPROM
;
;   BASE	( -- a )
;		Storage of the radix base for numeric I/O.
;
		%user	4,'BASE',BASE
;
;   tmp		( -- a )
;		A temporary storage location used in parse and find.
;
		%user	Lex_CO+3,'tmp',TEMP
;
;   SPAN	( -- a )
;		Hold character count received by EXPECT.
;
		%user	4,'SPAN',SPAN
;
;   >IN		( -- a )
;		Hold the character pointer while parsing input stream.
;
		%user	3,'>IN',INN
;
;   #TIB	( -- a )
;		Hold the current count and address of the terminal input buffer.
;
		%user	4,'#TIB',NTIB
_user		=	_user+WrdLen
;
;   CSP		( -- a )
;		Hold the stack pointer for error checking.
;
		%user	3,'CSP',CSP
;
;   'EVAL	( -- a )
;		Execution vector of EVAL.
;
		%user	5,"'EVAL",TEVAL
;
;   'NUMBER	( -- a )
;		Execution vector of NUMBER?.
;
		%user	7,"'NUMBER",TNUMBER
;
;   HLD		( -- a )
;		Hold a pointer in building a numeric output string.
;
		%user	3,'HLD',HLD
;
;   HANDLER	( -- a )
;		Hold the return stack pointer for error handling.
;
		%user	7,'HANDLER',HANDLER
;
;   CONTEXT	( -- a )
;		A area to specify vocabulary search order.
;
		%user	7,'CONTEXT',CONTEXT
_user		=	_user+VocDpt*WrdLen	;vocabulary stack
;
;   CURRENT	( -- a )
;		Point to the vocabulary to be extended.
;
		%user	7,'CURRENT',CURRENT
_user		=	_user+WrdLen		;vocabulary link pointer
;
;   CP		( -- a )
;		Point to the top of the code dictionary.
;
		%user	2,'CP',CP
;
;   NP		( -- a )
;		Point to the bottom of the name dictionary.
;
		%user	2,'NP',NP
;
;   LAST	( -- a )
;		Point to the last name in the name dictionary.
;
		%user	4,'LAST',LAST
;
;   forth	( -- a )
;		Point to the last name in the ROM name dictionary.
;
		%user	5,'forth',VFRTH
_user		=	_user+WrdLen
;
;   BAUDRATE	( -- a )
;		Point to storage of the current baud rate
;
		if	Word_AUTOBAUD
		%user	8,'BAUDRATE',BAUD
		endif
;
;   UPC		( -- a )
;		If not 0, input is converted from lower to upper case
;
		%user	3,'UPC',UPC
;
;******************************************************************************
;
;  Common functions
;
;   FORTH	( -- )
;		Make FORTH the context vocabulary.
;
		%colon	5,'FORTH',FORTH
		dw	VFRTH		; Get Addr Of Last Name In Dictionary
		dw	CONTEXT		; CONTEXT
		dw	STORE		; !
		dw	EXIT		; EXIT
;
;   ROT		( w1 w2 w3 -- w2 w3 w1 )
;		Rot 3rd item to top.
;		>R _SWAP R> _SWAP EXIT
;
		%code	3,'ROT',ROT
		lcall	ROTSub
		ljmp	Return
;
ROTSub:		mov	b,rpl
		inc	spl
		mov	rpl,spl
		inc	rpl
		inc	rpl
		movx	a,@rpl
		push	acc
		movx	a,@spl
		movx	@rpl,a
		mov	a,tph
		movx	@spl,a
		pop	tph
		inc	rpl
		inc	spl
		movx	a,@rpl
		push	acc
		movx	a,@spl
		movx	@rpl,a
		mov	a,tpl
		movx	@spl,a
		pop	tpl
		dec	spl
		dec	spl
		mov	rpl,b
		ret
;
;   2DROP	( w w -- )
;		Discard two items on stack.
;
		%code	5,'2DROP',DDROP
		inc	spl
		inc	spl
		ljmp	PopReturn
;
;   2DUP	( w1 w2 -- w1 w2 w1 w2 )
;		Duplicate top two items.
;
		%code	4,'2DUP',DDUP
		mov	dpl,spl
		mov	dph,sph
		lcall	PushSP
		inc	dpl
		inc	dpl
		movx	a,@dptr
		movx	@spl,a
		dec	spl
		dec	dpl
		movx	a,@dptr
		movx	@spl,a
		dec	spl
		ljmp	Return
;
;   +		( n1 n2 -- sum )
;		Add top two items.
;
		%code	1,'+',PLUS
		inc	spl
		inc	spl
		movx	a,@spl
		add	a,tpl
		mov	tpl,a
		dec	spl
		movx	a,@spl
		addc	a,tph
		mov	tph,a
		inc	spl
		ljmp	Return
;
;   NOT		( w -- w )
;		One's complement of tos.
;
		%code	3,'NOT',INVERT
		xrl	tpl,#0ffh
		xrl	tph,#0ffh
		ljmp	Return
;
;   NEGATE	( n -- -n )
;		Two's complement of TOS.
;
		%code	6,'NEGATE',NEGATE
		mov	a,tpl
		cpl	a
		add	a,#001h
		mov	tpl,a
		mov	a,tph
		cpl	a
		addc	a,#000h
		mov	tph,a
		ljmp	Return
;
;   DNEGATE	( d -- -d )
;		Two's complement of top double.
;
		%code	7,'DNEGATE',DNEGATE
		lcall	DblNeg
		ljmp	Return
;
DblNeg:		inc	spl
		inc	spl
		movx	a,@spl
		cpl	a
		add	a,#001h
		movx	@spl,a
		dec	spl
		movx	a,@spl
		cpl	a
		addc	a,#000h
		movx	@spl,a
		dec	spl
		mov	a,tpl
		cpl	a
		addc	a,#000h
		mov	tpl,a
		mov	a,tph
		cpl	a
		addc	a,#000h
		mov	tph,a
		ret
;
;   -		( n1 n2 -- n1-n2 )
;		Subtraction.
;
		%code	1,'-',SUB
		clr	c
		inc	spl
		inc	spl
		movx	a,@spl
		subb	a,tpl
		mov	tpl,a
		dec	spl
		movx	a,@spl
		subb	a,tph
		mov	tph,a
		inc	spl
		ljmp	Return
;
;   ABS		( n -- n )
;		Return the absolute value of n.
;
		%code	3,'ABS',ABS
		mov	a,tph
		jnb	acc.7,Abs_1
		mov	a,tpl
		cpl	a
		add	a,#001h
		mov	tpl,a
		xrl	tph,#0ffh
		jnc	Abs_1
		inc	tph
Abs_1:		ljmp	Return
;
;   0=		( n -- f)
;		Return -1 if n == 0, else return 0.  Same as NOT
;
		%code	2,'0=',ZEQUAL
		ljmp	INVERT
;
;   =		( w w -- t )
;		Return true if top two are equal.
;
		%code	1,'=',EQUAL
		inc	spl
		movx	a,@spl
		xrl	tph,a
		inc	spl
		movx	a,@spl
		xrl	a,tpl
		orl	a,tph
		mov	b,#000h
		jnz	Equ_1
		mov	b,#0ffh
Equ_1:		mov	tpl,b
		mov	tph,b
		ljmp	Return
;
;   U<		( u1 u2 -- t )
;		Unsigned compare of top two items.  Return true if n1 < n2.
;
		%code	2,'U<',ULESS
		mov	dpl,tpl
		mov	dph,tph
		inc	spl
		movx	a,@spl
		inc	spl
		cjne	a,dph,Ule_1
		movx	a,@spl
		cjne	a,dpl,Ule_1
Ule_1:		clr	a
		subb	a,#000h
		mov	tpl,a
		mov	tph,a
		ljmp	Return
;
;   <		( n1 n2 -- t )
;		Signed compare of top two items.  Return true if n1 < n2.
;		2DUP XOR 0< IF DROP 0< ELSE - 0< THEN EXIT
;
		%code	1,'<',LESS
		mov	dpl,tpl
		mov	dph,tph
		lcall	PopSP
		mov	a,dph
		xrl	a,tph
		jb	acc.7,Dle_1
		clr	c
		mov	a,tpl
		subb	a,dpl
		mov	a,tph
		subb	a,dph
		mov	tph,a
Dle_1: 		mov	a,tph
		rlc	a
		clr	a
		jnc	Dle_2
		dec	a
Dle_2:		mov	tph,a
		mov	tpl,a
		ljmp	Return
;
;   >		( n1 n2 -- t )
;		Signed compare of top two items.
;
		%colon	1,'>',GREATER
		dw	_SWAP
		dw	LESS
		dw	EXIT
;
;   MAX		( n n -- n )
;		Return the greater of two top stack items.
;		2DUP < IF _SWAP THEN DROP EXIT
;
		%code	3,'MAX',MAX
		mov	dph,tph
		mov	dpl,tpl
		lcall	PopSP
		mov	a,tph
		cjne	a,dph,Max_1
		mov	a,tpl
		cjne	a,dpl,Max_1
Max_1:		jnc	Max_2
		mov	tph,dph
		mov	tpl,dpl
Max_2:		ljmp	Return
;
;   MIN		( n n -- n )
;		Return the smaller of top two stack items.
;		2DUP _SWAP < IF _SWAP THEN DROP EXIT
;
		%code	3,'MIN',MIN
		mov	dph,tph
		mov	dpl,tpl
		lcall	PopSP
		mov	a,tph
		cjne	a,dph,Min_1
		mov	a,tpl
		cjne	a,dpl,Min_1
Min_1:		jc	Min_2
		mov	tph,dph
		mov	tpl,dpl
Min_2:		ljmp	Return
;
;   WITHIN	( u ul uh -- t )
;		Return true if u is within the range of ul and uh.
;
		%colon	6,'WITHIN',WITHIN
		dw	OVER		; OVER
		dw	SUB		; -
		dw	TOR		; >R
		dw	SUB		; -
		dw	RFROM		; R>
		dw	ULESS		; U<
		dw	EXIT		; EXIT
;
;******************************************************************************
;
; Divide
;
;   UM/MOD	( ud u -- ur uq )
;		Unsigned divide of a dbl by a single. Return mod and quotient.
;
		%code	6,'UM/MOD',UMMOD
		lcall	UMMODSub
		ljmp	Return
;
UMMODSub:	lcall	SDivLoad
		lcall	SDivide
		lcall	SDivSave
		ret
;
;   M/MOD	( d n -- r q )
;		Signed floored divide of dbl by single. Return mod and quotient.
;
		%code	5,'M/MOD',MSMOD
		lcall	SDivLoad
MsmAlt:		mov	a,dph
		mov	c,acc.7
		mov	f0,c
		jnb	f0,Msm_1
		mov	a,dpl
		cpl	a
		add	a,#001h
		mov	dpl,a
		mov	a,dph
		cpl	a
		addc	a,#000h
		mov	dph,a
		mov	a,r7
		cpl	a
		add	a,#001h
		mov	r7,a
		mov	a,r6
		cpl	a
		addc	a,#000h
		mov	r6,a
		mov	a,r5
		cpl	a
		addc	a,#000h
		mov	r5,a
		mov	a,r4
		cpl	a
		addc	a,#000h
		mov	r4,a
Msm_1:		mov	a,r4
		jnb	acc.7,Msm_2
		mov	a,r5
		add	a,dpl
		mov	r5,a
		mov	a,r4
		addc	a,dph
		mov	r4,a
Msm_2:		lcall	SDivide
		jnb	f0,Msm_3
		mov	a,r5
		cpl	a
		add	a,#001h
		mov	r5,a
		mov	a,r4
		cpl	a
		addc	a,#000h
		mov	r4,a
Msm_3:		lcall	SDivSave
		ljmp	Return
;
;   /MOD	( n n -- r q )
;		Signed divide. Return mod and quotient.
;
		%code	4,'/MOD',SLMOD
		mov	dph,tph
		mov	dpl,tpl
		mov	a,spl
		dec	spl
		dec	spl
		clr	rs0
		mov	spl,a
		inc	spl
		movx	a,@spl
		mov	r6,a
		rlc	a
		inc	spl
		movx	a,@spl
		mov	r7,a
		clr	a
		jnc	Slm_1
		dec	a
Slm_1:		mov	r4,a
		mov	r5,a
		sjmp	MsmAlt
;
;   MOD		( n n -- r )
;		Signed modulus. Return mod only.
;
		%colon	3,'MOD',MOD
		dw	SLMOD		; /MOD
		dw	DROP		; DROP
		dw	EXIT		; EXIT
;
;   /		( n n -- q )
;		Signed divide. Return quotient only.
;
		%colon	1,'/',SLASH
		dw	SLMOD		; /MOD
		dw	NIP		; NIP
		dw	EXIT		; EXIT
;
;  This loads DPH/DPL with the divisor, and R4567 with the dividend
;
SDivLoad:	mov	dph,tph
		mov	dpl,tpl
		mov	a,spl
		clr	rs0
		mov	spl,a
		inc	spl
		movx	a,@spl
		mov	r4,a
		inc	spl
		movx	a,@spl
		mov	r5,a
		inc	spl
		movx	a,@spl
		mov	r6,a
		inc	spl
		movx	a,@spl
		mov	r7,a
		ret
;
;  This stores the remainder to the stack, and quotient to TPH/TPL
;
SDivSave:	mov	a,r5
		movx	@spl,a
		dec	spl
		mov	a,r4
		movx	@spl,a
		mov	dph,r6
		mov	dpl,r7
		setb	rs0
		inc	spl
		inc	spl
		mov	tph,dph
		mov	tpl,dpl
		ret
;
;  This is the 32 / 16 bit divide, yielding 16 / 16
;
SDivide:	clr	c
		mov	a,r4
		subb	a,dph
		jc	Sdi_2
		jnz	Sdi_1
		mov	a,r5
		subb	a,dpl
		jc	Sdi_2
Sdi_1:		mov	a,#0ffh
		mov	r4,a
		mov	r7,a
		mov	r6,a
		sjmp	Sdi_6
Sdi_2:		mov	r3,#10h
Sdi_3:		clr	c
		mov	a,r7
		rlc	a
		mov	r7,a
		mov	a,r6
		rlc	a
		mov	r6,a
		mov	a,r5
		rlc	a
		mov	r5,a
		mov	a,r4
		rlc	a
		mov	r4,a
		clr	c
		mov	a,r4
		subb	a,dph
		jc	Sdi_5
		jnz	Sdi_4
		mov	a,r5
		subb	a,dpl
		jc	Sdi_5
Sdi_4:		mov	a,r5
		subb	a,dpl
		mov	r5,a
		mov	a,r4
		subb	a,dph
		mov	r4,a
		inc	r7
Sdi_5:		djnz	r3,Sdi_3
Sdi_6:		ret
;
;******************************************************************************
;
;  Multiply
;
;   UM*		( u u -- ud )
;		Unsigned multiply. Return double product.
;
		%code	3,'UM*',UMSTAR
		lcall	Multiply
		ljmp	Return
;
;   M*		( n n -- d )
;		Signed multiply.  Return double product.
;
		%code	2,'M*',MSTAR
		lcall	SMultiply
		ljmp	Return
;
;   *		( n n -- n )
;		Signed multiply.  Return single product.
;		UM* DROP EXIT
;
		%code	1,'*',STAR
		lcall	Multiply
		ljmp	PopReturn
;
;   */MOD	( n1 n2 n3 -- r q )
;		Multiply n1 and n2, then divide by n3. Return mod and quotient.
;		>R M* R> M/MOD EXIT
;
		%code	5,'*/MOD',SSMOD
		mov	gpl,tpl
		mov	gph,tph
		lcall	PopSP
		lcall	SMultiply
		lcall	PushSP
		mov	tpl,gpl
		mov	tph,gph
		%sforth
		dw	MSMOD
		dw	EXIT
;
;   */		( n1 n2 n3 -- q )
;		Multiply n1 by n2, then divide by n3. Return quotient only.
;
		%colon	2,'*/',STASL
		dw	SSMOD		; */MOD
		dw	NIP		; NIP
		dw	EXIT		; EXIT
;
;  This is the 16 x 16 to 32 signed multiply routine.  
;
SMultiply:	clr	f0
		mov	a,tph
		jnb	acc.7,Smu_1
		cpl	f0
		mov	a,tpl
		cpl	a
		add	a,#001h
		mov	tpl,a
		xrl	tph,#0ffh
		jnc	Smu_1
		inc	tph
Smu_1:		inc	spl
		movx	a,@spl
		jnb	acc.7,Smu_2
		cpl	f0
		inc	spl
		movx	a,@spl
		cpl	a
		add	a,#001h
		movx	@spl,a
		dec	spl
		movx	a,@spl
		cpl	a
		addc	a,#000h
		movx	@spl,a
Smu_2:		dec	spl
		lcall	Multiply
		jnb	f0,Smu_3
		lcall	DblNeg
Smu_3:		ret
;
;  This is the 16 x 16 to 32 unsigned multiply routine.  
;
Multiply:	mov	dpl,tpl
		mov	dph,tph
		mov	a,spl
		clr	rs0
		mov	spl,a
		lcall	PopSP
		mov	a,dpl
		mov	b,tpl
		mul	ab
		movx	@spl,a
		dec	spl
		mov	r6,b
		mov	a,dpl
		mov	b,tph
		mul	ab
		add	a,r6
		mov	r6,a
		mov	a,b
		addc	a,#000h
		mov	r5,a
		mov	a,dph
		mov	b,tpl
		mul	ab
		add	a,r6
		movx	@spl,a
		mov	a,b
		addc	a,r5
		mov	r5,a
		clr	a
		addc	a,#000h
		mov	r4,a
		mov	a,dph
		mov	b,tph
		mul	ab
		add	a,r5
		mov	dpl,a
		mov	a,b
		addc	a,r4
		mov	dph,a
		setb	rs0
		mov	tph,dph
		mov	tpl,dpl
		ret
;
;******************************************************************************
;
;   CELL+	( a -- a )
;		Add cell size in byte to address.
;
		%code	5,'CELL+',CELLP
		mov	a,tpl		; Get Low Of Stack Value
		add	a,#WrdLen	; Add Length Of Word
		mov	tpl,a		; Back To TPL
		mov	a,tph		; Get High Of Stack Value
		addc	a,#000h		; Add 0 Or 1, Depending On Carry
		mov	tph,a		; Back To TPH
		ljmp	Return		; EXIT
;
;   CELL-	( a -- a )
;		Subtract cell size in byte from address.
;
		%code	5,'CELL-',CELLM
		clr	c		; Clear Carry For Subtract
		mov	a,tpl		; Get Low Of Stack Value
		subb	a,#WrdLen	; Subtract Length Of Word
		mov	tpl,a		; Back To TPL
		mov	a,tph		; Get High Of Stack Value
		subb	a,#000h		; Subtract 0 Or 1, Depending On Carry
		mov	tph,a		; Back To TPH
		ljmp	Return		; EXIT
;
;   CELLS	( n -- n )
;		Multiply tos by cell size in bytes.
;
		if	0
		%colon	5,'CELLS',CELLS
		dw	DOLIT		; LITERAL
		dw	WrdLen		; 2
		dw	STAR		; *
		dw	EXIT		; EXIT
		endif
;
;   BL		( -- 32 )
;		Return 32, the blank character.
;
		%code	2,'BL',BLANK
		lcall	PushSP
		%loadtp	' '
		ljmp	Return
;
;   >CHAR	( c -- c )
;		Filter non-printing characters.
;
		%code	5,'>CHAR',TCHAR
		mov	tph,#000h
		mov	a,tpl
		anl	a,#07fh
		cjne	a,#07fh,Tch_1
		mov	a,#'_'
Tch_1:		cjne	a,#020h,Tch_2
Tch_2:		jnc	Tch_3
		mov	a,#'_'
Tch_3:		mov	tpl,a
		ljmp	Return
;
;   DEPTH	( -- n )
;		Return the depth of the data stack.
;
		%code	5,'DEPTH',DEPTH
		lcall	PushSP
		clr	a
		mov	a,#low(SP0Top)
		subb	a,spl
		mov	tpl,a
		mov	a,#high(SP0Top)
		subb	a,sph
		mov	tph,a
		mov	a,tph
		rrc	a
		mov	tph,a
		mov	a,tpl
		rrc	a
		mov	tpl,a
		ljmp	Return
;
;   PICK	( ... +n -- ... w )
;		Copy the nth stack item to tos.
;
		%code	4,'PICK',PICK
		clr	c
		mov	a,tpl
		rlc	a
		mov	dpl,a
		mov	a,tph
		rlc	a
		mov	dph,a
		inc	dptr
		mov	a,dpl
		add	a,spl
		mov	dpl,a
		mov	a,dph
		addc	a,sph
		mov	dph,a
		movx	a,@dptr
		mov	tph,a
		inc	dptr
		movx	a,@dptr
		mov	tpl,a
		ljmp	Return
;
;******************************************************************************
;
; Memory access
;
;   +!		( n a -- )
;		Add n to the contents at address a.
;
		%code	2,'+!',PSTORE
		mov	dpl,tpl
		mov	dph,tph
		push	dpl
		push	dph
		inc	dptr
		movx	a,@dptr
		mov	tpl,a
		inc	spl
		inc	spl
		movx	a,@spl
		add	a,tpl
		movx	@dptr,a
		pop	dph
		pop	dpl
		movx	a,@dptr
		mov	tph,a
		dec	spl
		movx	a,@spl
		addc	a,tph
		movx	@dptr,a
		inc	spl
		ljmp	PopReturn
;
;   2!		( d a -- )
;		Store the double integer to address a.
;		_SWAP OVER ! CELL+ ! EXIT
;
		%code	2,'2!',DSTORE
		mov	dph,tph
		mov	dpl,tpl
		lcall	PopSP
		mov	a,tph
		movx	@dptr,a
		inc	dptr
		mov	a,tpl
		movx	@dptr,a
		inc	dptr
		lcall	PopSP
		mov	a,tph
		movx	@dptr,a
		inc	dptr
		mov	a,tpl
		movx	@dptr,a
		ljmp	PopReturn
;
;   2@		( a -- d )
;		Fetch double integer from address a.
;		DUP CELL+ @ _SWAP @ EXIT
;
		%code	2,'2@',DAT
		mov	dph,tph
		mov	dpl,tpl
		movx	a,@dptr
		mov	gph,a
		inc	dptr
		movx	a,@dptr
		mov	gpl,a
		inc	dptr
		movx	a,@dptr
		mov	tph,a
		inc	dptr
		movx	a,@dptr
		mov	tpl,a
		lcall	PushSP
		mov	tph,gph
		mov	tpl,gpl
		ljmp	Return
;
;   COUNT	( a1 -- a1+1 l1 )
;		Return count byte of a string and add 1 to byte address.
;
		%code	5,'COUNT',COUNT
		mov	dpl,tpl
		mov	dph,tph
		movx	a,@dptr
		mov	tpl,a
		mov	tph,#000h
		inc	dptr
		lcall	PushDP
		ljmp	Return
;
;   HERE	( -- a )
;		Return the top of the code dictionary.
;
		%code	4,'HERE',HERE
		lcall	PushSP
		mov	dptr,#CP_Vec
		movx	a,@dptr
		mov	tph,a
		inc	dptr
		movx	a,@dptr
		mov	tpl,a
		ljmp	Return
;
;   PAD		( -- a )
;		Return the address of a temporary buffer.
;
		%code	3,'PAD',PAD
		lcall	PushSP
		mov	dptr,#CP_Vec
		movx	a,@dptr
		mov	tph,a
		inc	dptr
		movx	a,@dptr
		add	a,#80
		mov	tpl,a
		jnc	Pad_1
		inc	tph
Pad_1:		ljmp	Return
;
;   TIB		( -- a )
;		Return the address of the terminal input buffer.
;
		%colon	3,'TIB',TIB
		dw	NTIB		; #TIB
		dw	CELLP		; CELL+
		dw	GET		; @
		dw	EXIT		; EXIT
;
;   @EXECUTE	( a -- )
;		Execute vector stored in address a.
;
		%code	8,'@EXECUTE',ATEXECUTE
		mov	dpl,tpl
		mov	dph,tph
		movx	a,@dptr
		mov	tph,a
		inc	dptr
		movx	a,@dptr
		mov	tpl,a
		orl	a,tph
		jz	Ate_1
		mov	dpl,tpl
		mov	dph,tph
		lcall	PopSP
		clr	a
		jmp	@a+dptr
Ate_1:		ljmp	Return
;
;   CMOVE	( b1 b2 u -- )
;		Copy u bytes from b1 to b2.  If u == 0, don't move any bytes.
;
		%code	5,'CMOVE',CMOVE
		mov	a,tpl
		orl	a,tph
		jz	Cmo_3
		mov	dpl,tpl
		mov	dph,tph
		mov	a,spl
		clr	rs0
		mov	spl,a
		mov	r6,dph
		mov	r7,dpl
		inc	spl
		movx	a,@spl
		mov	r2,a
		inc	spl
		movx	a,@spl
		mov	r1,a
		lcall	PopDP
		mov	p2,r2
		mov	a,r7
		jz	Cmo_1
		inc	r6
Cmo_1:		movx	a,@dptr
		movx	@r1,a
		inc	dptr
		mov	a,r1
		add	a,#001h
		mov	r1,a
		jnc	Cmo_2
		inc	r2
		mov	p2,r2
Cmo_2:		djnz	r7,Cmo_1
		djnz	r6,Cmo_1
		mov	a,spl
		setb	rs0
		mov	spl,a
		mov	p2,sph
		sjmp	Cmo_4
Cmo_3:		inc	spl
		inc	spl
		inc	spl
		inc	spl
Cmo_4:		ljmp	PopReturn
;
;   FILL	( b u c -- )
;		Fill u bytes of character c to area beginning at b.
;
		%code	4,'FILL',FILL
		mov	b,tpl
		mov	a,spl
		clr	rs0
		mov	spl,a
		inc	spl
		movx	a,@spl
		mov	r6,a
		inc	spl
		movx	a,@spl
		mov	r7,a
		lcall	PopDP
		mov	a,r7
		orl	a,r6
		jz	Fil_3
		mov	a,r7
		jz	Fil_1
		inc	r6
Fil_1:		mov	a,b
Fil_2:		movx	@dptr,a
		inc	dptr
		djnz	r7,Fil_2
		djnz	r6,Fil_2
Fil_3:		mov	a,spl
		setb	rs0
		mov	spl,a
		ljmp	PopReturn
;
;   -TRAILING	( b u -- b u )
;		Adjust the count to eliminate trailing white space. (39)
;
		%code	9,'-TRAILING',DTRAILING
		mov	a,tph
		orl	a,tpl
		jz	Dtr_2
		lcall	PopDP
		dec	spl
		dec	spl
		mov	gph,dph
		mov	gpl,dpl
Dtr_1:		mov	a,tpl
		add	a,#0ffh
		mov	tpl,a
		mov	a,tph
		addc	a,#0ffh
		mov	tph,a
		orl	a,tpl
		jz	Dtr_2
		mov	a,tpl
		add	a,gpl
		mov	dpl,a
		mov	a,tph
		addc	a,gph
		mov	dph,a
		movx	a,@dptr
		add	a,#0e0h
		jz	Dtr_1
		mov	a,tpl
		add	a,#1
		mov	tpl,a
		jnc	Dtr_2
		inc	tph
Dtr_2:		ljmp	Return
;
;   PACK$	( b u a -- a )
;		Build a counted string with u characters from b to a.
;
		%colon	5,'PACK$',PACKS
		dw	DUPTOR		; DUP>R
		dw	DDUP		; 2DUP
		dw	CSTORE		; C!
		dw	ONEPLUS		; 1+
		dw	_SWAP		; SWAP
		dw	CMOVE		; CMOVE
		dw	RFROM		; R>
		dw	EXIT		; EXIT
;
;******************************************************************************
;
;  Numeric Output, Single Precision
;
;
;   <#		( -- )
;		Initiate the numeric output process.
;
		%colon	2,'<#',BDIGS
		dw	PAD		; PAD
		dw	HLD		; HLD
		dw	STORE		; !	
		dw	EXIT		; EXIT
;
;   HOLD	( c -- )
;		Insert a character into the numeric output string.
;
		%code	4,'HOLD',HOLD
		mov	b,tpl
		clr	rs0
		mov	p2,#high(UP0Btm)
		mov	r0,#low(HLD_Vec+1)
		movx	a,@r0
		clr	c
		subb	a,#1
		movx	@r0,a
		mov	dpl,a
		dec	r0
		movx	a,@r0
		subb	a,#0
		movx	@r0,a
		mov	dph,a
		mov	a,b
		movx	@dptr,a
		setb	rs0
		mov	p2,sph
		ljmp	PopReturn
;
;   #		( ud -- ud )
;		Extract one digit from ud and append the digit to output string.
;
		%code	1,'#',DIG
		lcall	PushSP
		mov	dptr,#BASE_Vec
		movx	a,@dptr
		mov	tph,a
		inc	dptr
		movx	a,@dptr
		mov	tpl,a
		lcall	MUMODSub
		lcall	ROTSub
		mov	b,#'0'+7
		mov	a,tph
		jnz	Dig_2
		mov	a,tpl
		cjne	a,#10,Dig_1
Dig_1:		jnc	Dig_2
		mov	b,#'0'
Dig_2:		mov	a,tpl
		add	a,b
		mov	tpl,a
		jnc	Dig_3
		inc	tph
Dig_3:		ljmp	HOLD
;
;   #S		( ud -- 0d )
;		Convert ud until all digits are added to the output string.
;
		%colon	2,'#S',DIGS
DIGS1:		dw	DIG		; #
		dw	DDUP		; 2DUP
		dw	OR		; OR
		dw	QBRAN,DIGS2	; ?branch DIGS2
		dw	BRAN,DIGS1	; Goto DIGS1
DIGS2:		dw	EXIT		; EXIT
;
;   SIGN	( n -- )
;		Add a minus sign to the numeric output string.
;
		%colon	4,'SIGN',SIGN
		dw	ZLESS		; 0<
		dw	QBRAN,SIGN1	; ?branch SIGN1
		dw	DOLIT		; LITERAL
		dw	'-'		; '-'
		dw	HOLD		; HOLD
SIGN1:		dw	EXIT		; EXIT
;
;   #>		( d -- b u )
;		Prepare the output string to be TYPE'd.
;
		%colon	2,'#>',EDIGS
		dw	DDROP		; 2DROP
		dw	HLD		; HLD
		dw	GET		; @
		dw	PAD		; PAD
		dw	OVER		; OVER
		dw	SUB		; -
		dw	EXIT		; EXIT
;
;   str		( n -- b u )
;		Convert a signed integer to a numeric string.
;
		if	0
		%colon	3,'str',STR
		dw	DUPTOR		; DUP>R
		dw	ABS		; ABS
		dw	BDIGS		; <#
		dw	DIGS		; #S
		dw	RFROM		; R>
		dw	SIGN		; SIGN
		dw	EDIGS		; #>
		dw	EXIT		; EXIT
		endif
;
;   HEX		( -- )
;		Use radix 16 as base for numeric conversions.
;
		%colon	3,'HEX',HEX
		dw	DOLIT		; LITERAL 16
		dw	16		; 16
		dw	BASE		; BASE
		dw	STORE		; !
		dw	EXIT		; EXIT
;
;   DECIMAL	( -- )
;		Use radix 10 as base for numeric conversions.
;
		%colon	7,'DECIMAL',DECIMAL
		dw	DOLIT		; LITERAL
		dw	10		; 10
		dw	BASE		; BASE
		dw	STORE		; !
		dw	EXIT		; EXIT
;
;******************************************************************************
;
;  Numeric Input, Single Precision
;
;   DIGIT?	( c base -- u t )
;		Cnvt a character to its numeric value.  A -1 indicates success.
;
		%code	6,'DIGIT?',DIGITQ
		lcall	QDigit
		ljmp	Return
;
QDigit:		inc	spl
		inc	spl
		movx	a,@spl
		clr	c
		subb	a,#'0'
		cjne	a,#10,Qdi_1
Qdi_1:		jc	Qdi_3
		subb	a,#7
		cjne	a,#10,Qdi_2
Qdi_2:		cpl	c
		jnc	Qdi_3
		mov	b,tpl
		cjne	a,b,Qdi_3
Qdi_3:		movx	@spl,a
		dec	spl
		dec	spl
		clr	a
		subb	a,#000h
		mov	tpl,a
		mov	tph,a
		ret
;
;   NUMBER?	( a -- n T | a F )
;		Convert a number string to integer. Push a flag on tos.
;
		%code	7,'NUMBER?',NUMBERQ
		mov	dptr,#BASE_Vec
		movx	a,@dptr
		mov	gph,a
		inc	dptr
		movx	a,@dptr
		mov	gpl,a
		mov	dph,tph
		mov	dpl,tpl
		mov	a,spl
		clr	rs0
		mov	spl,a
		movx	a,@dptr
		mov	r7,a
		inc	dptr
		movx	a,@dptr
		cjne	a,#'$',Num_1
		dec	r7
		mov	a,r7
		jz	Num_6
		inc	dptr
		movx	a,@dptr
		mov	gph,#000h
		mov	gpl,#010h
Num_1:		clr	f0
		cjne	a,#'-',Num_2
		setb	f0
		dec	r7
		mov	a,r7
		jz	Num_6
		inc	dptr
Num_2:		mov	r5,#000h
		mov	r6,#000h
Num_3: 		movx	a,@dptr
		movx	@spl,a
		dec	spl
		dec	spl
		mov	tph,gph
		mov	tpl,gpl
		lcall	QDigit
		mov	a,tpl
		orl	a,tph
		jz	Num_6
		inc	spl
		inc	spl
		mov	a,r5
		mov	b,gpl
		mul	ab
		mov	r5,a
		movx	a,@spl
		add	a,r5
		mov	r5,a
		push	psw
		push	b
		mov	a,r6
		mov	b,gpl
		mul	ab
		pop	b
		pop	psw
		addc	a,b
		mov	r6,a
		inc	dptr
		djnz	r7,Num_3
		mov	dpl,r5
		mov	dph,r6
		setb	rs0
		mov	a,dpl
		jnb	f0,Num_4
		cpl	a
		add	a,#001h
Num_4:		movx	@spl,a
		dec	spl
		mov	a,dph
		jnb	f0,Num_5
		cpl	a
		addc	a,#000h
Num_5:		movx	@spl,a
		dec	spl
		mov	tph,#0ffh
		mov	tpl,#0ffh
		ljmp	Return
Num_6:		setb	rs0
		lcall	PushSP
		mov	tph,#000h
		mov	tpl,#000h
		ljmp	Return
; 
;******************************************************************************
;
;  Basic I/O
;
;   ?KEY	( -- c T | F )
;		Return input character and true, or a false if no input.
;
		%code	4,'?KEY',QKEY
		mov	dptr,#TQKEY_Vec
		movx	a,@dptr
		mov	b,a
		inc	dptr
		movx	a,@dptr
		mov	dpl,a
		mov	dph,b
		clr	a
		jmp	@a+dptr
;
;   KEY		( -- c )
;		Wait for and return an input character.
;
		%colon	3,'KEY',KEY
KEY1:		dw	QKEY		; ?KEY
		dw	QBRAN,KEY1	; ?branch KEY1
		dw	EXIT		; EXIT
;
;   EMIT	( c -- )
;		Send a character to the output device.  Assume 'EMIT not 0
;
		%code	4,'EMIT',EMIT
		mov	dptr,#TEMIT_Vec
		movx	a,@dptr
		mov	b,a
		inc	dptr
		movx	a,@dptr
		mov	dpl,a
		mov	dph,b
		clr	a
		jmp	@a+dptr
;
;   NUF?	( -- t )
;		Return false if no input, else pause and if CR return true.
;
		%colon	4,'NUF?',NUFQ
		dw	QKEY		; ?KEY
		dw	DUP		; DUP
		dw	QBRAN,NUFQ1	; ?branch NUFQ1
		dw	DDROP		; 2DROP
		dw	KEY		; KEY
		dw	DOLIT		; LITERAL
		dw	ASC_CR		; CR
		dw	EQUAL		; =
NUFQ1:		dw	EXIT		; EXIT
;
;   SPACE	( -- )
;		Send the blank character to the output device.
;
		%code	5,'SPACE',SPACE
		lcall	PushSP
		%loadtp	' '
		ljmp	EMIT
;
;   SPACES	( +n -- )
;		Send n spaces to the output device.
;
		%code	6,'SPACES',SPACES
		mov	a,tph
		jb	acc.7,Spc_2
		mov	a,tph
		orl	a,tpl
		jz	Spc_2
		mov	a,tpl
		jz	Spc_1
		inc	tph
Spc_1:		%sforth
		dw	SPACE
		dw	SASMBLY
		djnz	tpl,Spc_1
		djnz	tph,Spc_1
Spc_2:		ljmp	PopReturn
;
;   TYPE	( b u -- )
;		Output u characters from b.
;		>R BEGIN DUP C@ EMIT 1+ LOOP DROP EXIT
;
		%code	4,'TYPE',TYPE
		mov	gpl,tpl
		mov	gph,tph
		lcall	PopDP
		mov	a,gpl
		orl	a,gph
		jz	Typ_2
		mov	a,gpl
		jz	Typ_1
		inc	gph
Typ_1:		lcall	PushSP
		movx	a,@dptr
		mov	tpl,a
		mov	tph,#000h
		push	dph
		push	dpl
		%sforth
		dw	EMIT
		dw	SASMBLY
		pop	dpl
		pop	dph
		inc	dptr
		djnz	gpl,Typ_1
		djnz	gph,Typ_1
Typ_2:		ljmp	PopReturn
;
;   CR		( -- )
;		Output a carriage return and a line feed.
;		13 EMIT 10 EMIT EXIT
;
		%code	2,'CR',CR
		lcall	PushSP
		%loadtp	00dh
		%sforth
		dw	EMIT
		dw	SASMBLY
		lcall	PushSP
		%loadtp	00ah
		%sforth
		dw	EMIT
		dw	SASMBLY
		ljmp	Return
;
;   do$		( -- a )
;		Return the address of a compiled string.
;
		%code	Lex_CO+3,'do$',DOSTR
		lcall	PushSP
		lcall	PopSPRS
		mov	dph,tph
		mov	dpl,tpl
		movx	a,@dptr
		inc	dptr
		add	a,dpl
		mov	dpl,a
		jnc	Dos_1
		inc	dph
Dos_1:		lcall	PushDPRS
		ljmp	Return
;
;   ($")	( -- a )
;		Run time rtn compiled by $". Return addr of a compiled string.
;
		%colon	Lex_CO+4,'($")',PSTRQP
		dw	DOSTR		; do$
		dw	EXIT		; EXIT
;
;   (.")	( -- )
;		Run time routine of .". Output a compiled string.
;
		%colon	Lex_CO+4,'(.")',PDOTQP
		dw	DOSTR		; do$
		dw	COUNT		; COUNT
		dw	TYPE		; TYPE
		dw	EXIT		; EXIT
;
;   U.		( u -- )
;		Convert an unsigned 16 bit number to a string
;
		%colon	2,'U.',UDOT
		dw	ZERO		; 0
		dw	BDIGS		; <#
		dw	DIGS		; #S
		dw	EDIGS		; #>
		dw	TYPE		; TYPE
		dw	SPACE		; SPACE
		dw	EXIT		; EXIT
;
;   U.R		( u +n -- )
;		Output as an unsigned single number right justified.
;
		%colon	3,'U.R',UDOTR
		dw	TOR		; >R
		dw	ZERO		; 0
		dw	BDIGS		; <#
		dw	DIGS		; #S
		dw	EDIGS		; #>
		dw	RFROM		; R>
		dw	OVER		; OVER
		dw	SUB		; -
		dw	SPACES		; SPACES
		dw	TYPE		; TYPE
		dw	EXIT		; EXIT
;
;   (.)		( n -- a l )
;		Convert a signed 16 bit number to a string
;
		%colon	3,'(.)',PDOTP
		dw	DUP		; DUP
		dw	ABS		; ABS
		dw	ZERO		; 0
		dw	BDIGS		; <#
		dw	DIGS		; #S
		dw	ROT		; ROT
		dw	SIGN		; SIGN
		dw	EDIGS		; #>
		dw	EXIT		; EXIT
;
;   .		( w -- )
;		Output as a single signed number with a trailing space
		%colon	1,'.',DOT
		dw	PDOTP		; (.)
		dw	TYPE		; TYPE
		dw	SPACE		; SPACE
		dw	EXIT		; EXIT
;
;   .R		( n +n -- )
;		Output as a signed number right justified
;
		%colon	2,'.R',DOTR
		dw	TOR		; >R
		dw	PDOTP		; (.)
		dw	RFROM		; R>
		dw	OVER		; OVER
		dw	SUB		; -
		dw	SPACES		; SPACES
		dw	TYPE		; TYPE
		dw	EXIT		; EXIT
;
;   UD.		( ud -- )
;		Output as an unsigned double with a trailing space
;
		%colon	3,'UD.',UDOTD
		dw	BDIGS		; <#
		dw	DIGS		; #S
		dw	EDIGS		; #>
		dw	TYPE		; TYPE
		dw	SPACE		; SPACE
		dw	EXIT		; EXIT
;
;   UD.R	( ud l -- )
;		Output as an unsigned double number right justified
;
		%colon	4,'UD.R',UDOTDR
		dw	TOR		; >R
		dw	BDIGS		; <#
		dw	DIGS		; #S
		dw	EDIGS		; #>
		dw	RFROM		; R>
		dw	OVER		; OVER
		dw	SUB		; -
		dw	SPACES		; SPACES
		dw	TYPE		; TYPE
		dw	EXIT		; EXIT
;
;   (D.)	( d -- a l )
;		Convert a signed double number to a string
;
		%colon	4,'(D.)',PDDOTP
		dw	TUCK		; TUCK
		dw	DABS		; DABS
		dw	BDIGS		; <#
		dw	DIGS		; #S
		dw	ROT		; ROT
		dw	SIGN		; SIGN
		dw	EDIGS		; #>
		dw	EXIT		; EXIT
;
;   D.		( d -- )
;		Output as a signed double number with a trailing space
;
		%colon	2,'D.',DDOT
		dw	PDDOTP		; (D.)
		dw	TYPE		; TYPE
		dw	SPACE		; SPACE
		dw	EXIT		; EXIT
;
;   D.R		( d l -- )
;		Output as a signed double number right justified
;
		%colon	3,'D.R',DDOTR
		dw	TOR		; >R
		dw	PDDOTP		; (D.)
		dw	RFROM		; R>
		dw	OVER		; OVER
		dw	SUB		; -
		dw	SPACES		; SPACES
		dw	TYPE		; TYPE
		dw	EXIT		; EXIT
;
;   ?		( a -- )
;		Display the contents in a memory cell.
;
		%colon	1,'?',QUEST
		dw	GET		; @
		dw	DOT		; .
		dw	EXIT		; EXIT
;
;******************************************************************************
;
;  Parsing
;
;   (PARSE)	( b u c -- b u delta ; <string> )
;		Scan string delimited by c.  Return found string and its offset.
;
		if	0
		%code	7,'(PARSE)',PPARSEP
		lcall	PopDP
		mov	a,dph
		orl	a,dpl
		jnz	Ppa_0
		lcall	PopSP
		dec	spl
		dec	spl
		lcall	PushDP
		ljmp	Return
;
;  Since 'u' <> 0, We Can Do So Stuff
;
Ppa_0:		clr	f0
		cjne	tpl,#' ',Ppa_00
		setb	f0
Ppa_00:		mov	a,spl
		clr	rs0
		mov	spl,a
		mov	r4,dph
		mov	r5,dpl
		mov	r6,dph
		mov	r7,dpl
		lcall	PopDP		; dptr=b
;		lcall	DecR6R7		; --u
		jnb	f0,Ppa_4
;
;  Since 'c' == ' ', Eat Any Leading Spaces
;
Ppa_1:		movx	a,@dptr
		cjne	a,#33,Ppa_2
Ppa_2:		jnc	Ppa_3a
		inc	dptr
		mov	a,r6
		orl	a,r7
		jz	Ppa_3
		lcall	DecR6R7
		sjmp	Ppa_1
;
;  Return (s, 0, 0) Since 'u' Went To 0, And All We Got Were Spaces
;
Ppa_3:		setb	rs0
		inc	spl
		inc	spl
		clr	a
		mov	tph,a
		mov	tpl,a
		lcall	PushDP
		lcall	PushSP
		ljmp	Return
;
Ppa_3a:		lcall	OutDPTR
		lcall	OutR6R7
;
;  If We Got Here, We Got A Non Space Character, Or 'c' <> ' '
;
Ppa_4:		mov	r2,dph
		mov	r3,dpl
;
Ppa_5:		movx	a,@dptr
		jnb	f0,Ppa_7
;
		clr	c
		subb	a,#33
		jc	Ppa_9
		sjmp	Ppa_8
;
Ppa_7:		xrl	a,b
		jz	Ppa_9
;
Ppa_8:		inc	dptr
		mov	a,r6
		orl	a,r7
		jz	Ppa_10
		lcall	DecR6R7
		sjmp	Ppa_5
;
;  Got Here Because We Got A Character Matching 'c' Or 'c' == ' ' And 'u' <> 0
;
Ppa_9:		lcall	DecR6R7
Ppa_10:		lcall	PushSP
		clr	c
		mov	a,dpl
		subb	a,r7
		mov	r3,a
		mov	a,dph
		subb	a,r6
		mov	r2,a
		lcall	OutR2R3
		lcall	OutSpace
		lcall	OutR4R5
		lcall	OutDPTR
		lcall	PushSP
		clr	c
		mov	a,dpl
		subb	a,r5
		mov	dpl,a
		mov	a,dph
		subb	a,r4
		mov	dph,a
		lcall	OutDPTR
		setb	rs0
		mov	tph,dph
		mov	tpl,dpl
		dec	spl
		dec	spl
		ljmp	Return
;
DecR6R7:	clr	c
		mov	a,r7
		subb	a,#1
		mov	r7,a
		jnc	Dec1
		dec	r6
Dec1:		ret
		else
		%colon	7,'(PARSE)',PPARSEP
		dw	ROT
		dw	DUP
		dw	DOT
		dw	ROT
		dw	DUP
		dw	DOT
		dw	ROT
		dw	DUP
		dw	DOT
		dw	TEMP		; TEMP		( b u c temp )
		dw	STORE		; !		( b u )
		dw	OVER		; OVER		( b u b )
		dw	TOR		; >R		( b u ) ( u )
		dw	DUP		; DUP		( b u u )
		dw	QBRAN,PARS8	; ?branch PARS8	( b u )
		dw	ONEMINUS	; 1-		( b u )
		dw	TEMP		; TEMP		( b u temp )
		dw	GET		; @		( b u c )
		dw	BLANK		; ' '		( b u c 32 )
		dw	EQUAL		; =		( b u f )
		dw	QBRAN,PARS3	; ?branch PARS3	( b u )
;
;  Do This If 'c' == ' ' (Eat Leading Spaces)
;
		dw	TOR		; >R		( s(b) ) ( u u )
PARS1:		dw	BLANK		; ' '		( s 32 )
		dw	OVER		; OVER		( s 32 s )
		dw	CAT		; C@		( s 32 x )
		dw	SUB		; -		( s 32-x )
		dw	ZLESS		; 0<		( s f )
		dw	INVERT		; NOT		( s -f )
		dw	QBRAN,PARS2	; ?branch PARS2	( s )
		dw	ONEPLUS		; 1+		( s )
		dw	PNEXTP,PARS1	; (NEXT)	( s )
		dw	RDROP		; R>DROP	( s ) ( )
		dw	ZERO		; 0		( s 0 )
		dw	DUP		; DUP		( s 0 0 )
		dw	EXIT		; EXIT		( s 0 0 )
;
;  If We Ate All The Spaces, Pull Remainder Of Loop To Data Stack
;
PARS2:		dw	RFROM		; R>		( s u ) ( u )
;
;  Here 's' Points To A Non-Space Character.  Search Until 'u' Is 0, Or If
;  'c' == ' ' Then 'c' Being > ' ', Or If 'c' <> ' ', Until We Encounter A
;  'c' Character In 'b'
;
PARS3:		dw	OVER		; OVER		( s u t )
		dw	_SWAP		; SWAP		( s t u )
		dw	TOR		; >R		( s t ) ( u u )
PARS4:		dw	TEMP		; TEMP		( s t temp )
		dw	GET		; @		( s t c )
		dw	OVER		; OVER		( s t c t )
		dw	CAT		; C@		( s t c x )
		dw	SUB		; -		( s t c-x )
		dw	TEMP		; TEMP		( s t c-x temp )
		dw	GET		; @		( s t c-x c )
		dw	BLANK		; ' '		( s t c-x c 32 )
		dw	EQUAL		; =		( s t c-x f )
		dw	QBRAN,PARS5	; ?branch PARS5	( s t c-x )
;
;  If 'c' == ' ' Fall Through
;
		dw	ZLESS		; 0<		( s t f )
;
;  We Know What We Mean, (Right?...)
;
PARS5:		dw	QBRAN,PARS6	; ?branch PARS6	( s t )
		dw	ONEPLUS		; 1+		( s t )
		dw	PNEXTP,PARS4	; (NEXT)	( s t ) ( u )
;
;  'u' Expired Before Character Being Searched For Was Found.  Return (b, u, u)
;
		dw	DUPTOR		; DUP>R		( s t ) ( u t )
		dw	BRAN,PARS7	; Goto PARS7	( s t )
;
;  Got Here If (c == ' ' && *s > c) || (c != ' ' && *s == c)
;
PARS6:		dw	RDROP		; R>DROP	( s t )	( u )
		dw	DUP		; DUP		( s t t )
		dw	ONEPLUS		; 1+		( s t t+1 )
		dw	TOR		; >R		( s t ) ( u t+1 )
;
;  Fall Through From PARS6 Or If Fell Through Search Loop
;
PARS7:		dw	OVER		; OVER		( s t s )
		dw	SUB		; -		( s t-s )
		dw	RFROM		; R>		( s t-s t+1 )
		dw	RFROM		; R>		( s t-s t+1 u )
		dw	SUB		; -		( s t-s t+1-u )
		dw	ROT
		dw	DUP
		dw	DOT
		dw	ROT
		dw	DUP
		dw	DOT
		dw	ROT
		dw	DUP
		dw	DOT
		dw	EXIT		; EXIT		( s t-s t+1-u )
;
;  If 'u' Was 0, Then Return ( b u 0 )
;
PARS8:		dw	OVER		; OVER		( b u b )
		dw	RFROM		; R>		( b u b u )
		dw	SUB		; -		( b u 0 )
		dw	EXIT		; EXIT		( b u 0 )
		endif
;
;   PARSE	( c -- b u ; <string> )
;		Scan input stream and return counted string delimited by c.
;
		%colon	5,'PARSE',PARSE
		dw	TOR		; >R
		dw	TIB		; TIB
		dw	INN		; >IN
		dw	GET		; @
		dw	PLUS		; +
		dw	NTIB		; #TIB
		dw	GET		; @
		dw	INN		; >IN
		dw	GET		; @
		dw	SUB		; -
		dw	RFROM		; R>
		dw	PPARSEP		; (PARSE)
		dw	INN		; >IN
		dw	PSTORE		; +!
		dw	EXIT		; EXIT
;
;   .(		( -- )
;		Output following string up to next ) .
;
		%colon	Lex_IM+2,'.(',DOTPAREN
		dw	DOLIT		; LITERAL
		dw	')'		; ')'
		dw	PARSE		; PARSE
		dw	TYPE		; TYPE
		dw	EXIT		; EXIT
;
;   (		( -- )
;		Ignore following string up to next ) . A comment.
;
		%colon	Lex_IM+1,'(',PAREN
		dw	DOLIT		; LITERAL
		dw	')'		; ')'
		dw	PARSE		; PARSE
		dw	DDROP		; 2DROP
		dw	EXIT		; EXIT
;
;   \		( -- )
;		Ignore following text till the end of line.
;
		%colon	Lex_IM+1,'\',BKSLASH
		dw	NTIB		; #TIB
		dw	GET		; @
		dw	INN		; >IN
		dw	STORE		; !
		dw	EXIT		; EXIT
;
;   CHAR	( -- c )
;		Parse next word and return its first character.
;
		%colon	4,'CHAR',CHAR
		dw	BLANK		; ' '
		dw	PARSE		; PARSE
		dw	DROP		; DROP
		dw	CAT		; C@
		dw	EXIT		; EXIT
;
;   TOKEN	( -- a ; <string> )
;		Parse a word from input stream and copy it to name dictionary.
;
		%colon	5,'TOKEN',TOKEN
		dw	BLANK		; ' '
		dw	PARSE		; PARSE
		dw	DOLIT		; LITERAL
		dw	31		; 31
		dw	MIN		; MIN
		dw	NP		; NP
		dw	GET		; @
		dw	OVER		; OVER
		dw	SUB		; -
		dw	CELLM		; CELL-
		dw	PACKS		; PACK$
		dw	EXIT		; EXIT
;
;   WORD	( c -- a ; <string> )
;		Parse a word from input stream and copy it to code dictionary.
;
		%colon	4,'WORD',WORD
		dw	PARSE		; PARSE
		dw	HERE		; HERE
		dw	PACKS		; PACK$
		dw	EXIT		; EXIT
;
;******************************************************************************
;
;  Dictionary Search
;
;   NAME>	( nfa -- cfa )
;		Return a code address given a name address.
;		CELL- CELL- @ EXIT
;
		%code	5,'NAME>',NAMET
		clr	c
		mov	a,tpl
		subb	a,#WrdLen*2
		mov	dpl,a
		mov	a,tph
		subb	a,#000h
		mov	dph,a
		movx	a,@dptr
		mov	tph,a
		inc	dptr
		movx	a,@dptr
		mov	tpl,a
		ljmp	Return
;
;   SAME?	( b1 b2 u1 -- b1 b2 f1 \ -0+ )
;		Compare u cells in two strings. Return 0 if identical.
;
		if	0
		%colon	5,'SAME?',SAMEQ
		dw	CELLS		; CELLS
		dw	CSAMEQ		; CSAME?
		dw	EXIT		; EXIT
		endif
;
;******************************************************************************
;
;  Miscellaneous Routines
;
;   find	( s1 v1 -- cfa nfa | s1 0 )
;		Search a vocabulary for a string. Return cfa and nfa if 
;		succeeded.  v1 is address of a variable that contains a
;		pointer to the length byte of the newest word in the
;		vocabulary in the current context.
;
		%colon	4,'find',FIND
		dw	_SWAP		; SWAP		( v1 s1 )
		dw	DUP		; DUP		( v1 s1 s1 )
		dw	CAT		; C@		( v1 s1 sl )
		dw	TOR		; >R		( v1 s1 )
		dw	ONEPLUS		; 1+		( v1 s1+1 )
		dw	_SWAP		; SWAP		( s1+1 v1 )
;
;  v1 Points Cell With Address Of Length Byte Of Last Defined Word
;
FIND1:		dw	GET		; @		( s1+1 nfa )
		dw	DUP		; DUP		( s1+1 nfa nfa )
		dw	QBRAN,FIND3	; ?branch FIND3
		dw	DUP		; DUP		( s1+1 nfa nfa )
		dw	CAT		; C@		( s1+1 nfa l1 )
		dw	DOLIT		; LITERAL
		dw	Lex_LN		; Lexicon Mask  ( s1+1 nfa l1 31 )
		dw	AND		; AND		( s1+1 nfa l1 )
		dw	RAT		; R@		( s1+1 nfa l1 sl )
		dw	XOR		; XOR		( s1+1 nfa 0|-1)
		dw	QBRAN,FIND2	; ?branch Find2
		dw	CELLM		; CELL-		( s1+1 next_nfa )
		dw	BRAN,FIND1	; Goto FIND1
;
;  Stack Is ( xx yy -- ).  Compare Strings.
;
FIND2:		dw	ONEPLUS		; 1+		( s1+1 nfa+1 )
		dw	RAT		; R@		( s1+1 nfa+1 l1 )
		dw	CSAMEQ		; CSAME?
		dw	QBRAN,FIND4	; ?branch FIND4
		dw	ONEMINUS	; 1-		( s1+1 nfa )
		dw	CELLM		; CELL-		( s1+1 nfa-2 )
		dw	BRAN,FIND1	; Goto FIND1
;
;  Not Found.  Drop RP Value, Decrement String Address, Return 0
;
FIND3:		dw	RDROP		; R>DROP
		dw	DROP		; DROP		( s1+1 nfa )
		dw	ONEMINUS	; 1-		( s1 )
		dw	ZERO		; 0		( s1 0 )
		dw	EXIT		; EXIT
;
;  Name Found, Dup NFA, Convert NFA To CFA
;
FIND4:		dw	RDROP		; R>DROP
		dw	NIP		; NIP		( nfa+1 )
		dw	ONEMINUS	; 1-		( nfa )
		dw	DUP		; DUP		( nfa nfa )
		dw	NAMET		; NAME>		( nfa cfa )
		dw	_SWAP		; SWAP		( cfa nfa )
		dw	EXIT		; EXIT
;
;******************************************************************************
;
;   NAME?	( a -- cfa nfa | a F )
;		Search all context vocabularies for a string.

		%colon	5,'NAME?',NAMEQ
		dw	UPC		; UPC
		dw	GET		; @
		dw	QBRAN,NAMQ0	; ?branch NAMQ0
		dw	DUP		; DUP
		dw	COUNT		; COUNT
		dw	UPPER		; UPPER
NAMQ0:		dw	CONTEXT		; CONTEXT
		dw	DUP		; DUP
		dw	DAT		; D@
		dw	XOR		; XOR
		dw	QBRAN,NAMQ1	; ?branch NAMQ1
		dw	CELLM		; CELL-
NAMQ1:		dw	TOR		; >R
NAMQ2:		dw	RFROM		; R>
		dw	CELLP		; CELL+
		dw	DUPTOR		; DUP>R
		dw	GET		; @
		dw	QDUP		; ?DUP
		dw	QBRAN,NAMQ3	; ?branch NAMQ3
		dw	FIND		; FIND
		dw	QDUP		; ?DUP
		dw	QBRAN,NAMQ2	; ?branch NAMQ2
		dw	RDROP		; R>DROP
		dw	EXIT		; EXIT
NAMQ3:		dw	RDROP		; R>DROP
		dw	ZERO		; 0
		dw	EXIT		; EXIT
;
;******************************************************************************
;
;  Terminal response
;
;   ^H		( bot eot cur -- bot eot cur )
;		Backup the cursor by one character.

		%colon	2,'^H',BKSP
		dw	TOR		; >R
		dw	OVER		; OVER
		dw	RFROM		; R>
		dw	_SWAP		; SWAP
		dw	OVER		; OVER
		dw	XOR		; XOR
		dw	QBRAN,BACK1	; ?branch BACK1
		dw	DOLIT		; LITERAL
		dw	ASC_BS		; ^H
		dw	TECHO		; 'ECHO
		dw	ATEXECUTE	; @EXECUTE
		dw	ONEMINUS	; 1-
		dw	BLANK		; ' '
		dw	TECHO		; 'ECHO
		dw	ATEXECUTE	; @EXECUTE
		dw	DOLIT		; LITERAL
		dw	ASC_BS		; 008h
		dw	TECHO		; 'ECHO
		dw	ATEXECUTE	; @EXECUTE
BACK1:		dw	EXIT		; EXIT
;
;   TAP		( bot eot cur c -- bot eot cur )
;		Accept and echo the key stroke and bump the cursor.
;
		%colon	3,'TAP',TAP
		dw	DUP		; DUP
		dw	TECHO		; 'ECHO
		dw	ATEXECUTE	; @EXECUTE
		dw	OVER		; OVER
		dw	CSTORE		; C!
		dw	ONEPLUS		; 1+
		dw	EXIT		; EXIT
;
;   kTAP	( bot eot cur c -- bot eot cur )
;		Process a key stroke, CR or backspace.
;
		%colon	4,'kTAP',KTAP
		dw	DUP		; DUP
		dw	DOLIT		; LITERAL
		dw	ASC_CR		; 00dh
		dw	XOR		; XOR
		dw	QBRAN,KTAP2	; ?branch KTAP2
		dw	DOLIT		; LITERAL
		dw	ASC_BS		; 008h
		dw	XOR		; XOR
		dw	QBRAN,KTAP1	; ?branch KTAP1
		dw	BLANK		; ' '
		dw	TAP		; TAP
		dw	EXIT		; EXIT
KTAP1:		dw	BKSP		; ^H
		dw	EXIT		; EXIT
KTAP2:		dw	DROP		; DROP
		dw	NIP		; NIP
		dw	DUP		; DUP
		dw	EXIT		; EXIT
;
;   accept	( b u -- b u )
;		Accept characters to input buffer. Return with actual count.
;
		%colon	6,'accept',ACCEPT
		dw	OVER		; OVER
		dw	PLUS		; +
		dw	OVER		; OVER
ACCP1:		dw	DDUP		; 2DUP
		dw	XOR		; XOR
		dw	QBRAN,ACCP4	; ?branch ACCP4
		dw	KEY		; KEY
		dw	DUP		; DUP
		dw	BLANK		; ' '
		dw	DOLIT		; LITERAL
		dw	127		; 127
		dw	WITHIN		; WITHIN
		dw	QBRAN,ACCP2	; ?branch ACCP2
		dw	TAP		; TAP
		dw	BRAN,ACCP3	; Goto ACCP3
ACCP2:		dw	TTAP		; 'TAP
		dw	ATEXECUTE	; @EXECUTE
ACCP3:		dw	BRAN,ACCP1	; Goto ACCP1
ACCP4:		dw	DROP		; DROP
		dw	OVER		; OVER
		dw	SUB		; SUB
		dw	EXIT		; EXIT
;
;   EXPECT	( b u -- )
;		Accept input stream and store count in SPAN.
;
		%colon	6,'EXPECT',EXPECT
		dw	TEXPECT		; 'EXPECT
		dw	ATEXECUTE	; @EXECUTE
		dw	SPAN		; SPAN
		dw	STORE		; !
		dw	DROP		; DROP
		dw	EXIT		; EXIT
;
;   QUERY	( -- )
;		Accept input stream to terminal input buffer.
;
		%colon	5,'QUERY',QUERY
		dw	TIB		; TIB
		dw	DOLIT		; LITERAL
		dw	80		; 80
		dw	TEXPECT		; 'EXPECT
		dw	ATEXECUTE	; @EXECUTE
		dw	NTIB		; #TIB
		dw	STORE		; !
		dw	DROP		; DROP
		dw	ZERO		; 0
		dw	INN		; >IN
		dw	STORE		; !
		dw	EXIT		; EXIT
;
;******************************************************************************
;
;  Error handling
;
;   CATCH	( cfa -- 0 | err# )
;		Execute word at cfa and set up an error frame for it.
;
		%colon	5,'CATCH',CATCH
		dw	SPAT		; SP@
		dw	TOR		; >R
		dw	HANDLER		; HANDLER
		dw	GET		; @
		dw	TOR		; >R
		dw	RPAT		; RP@
		dw	HANDLER		; HANDLER
		dw	STORE		; !
		dw	EXECUTE		; EXECUTE
		dw	RFROM		; R>
		dw	HANDLER		; HANDLER
		dw	STORE		; !
		dw	RDROP		; R>DROP
		dw	ZERO		; 0
		dw	EXIT		; EXIT
;
;   THROW	( err# -- err# )
;		Reset system to current local error frame an update error flag.
;
		%colon	5,'THROW',THROW
		dw	HANDLER		; HANDLER
		dw	GET		; @
		dw	RPSTORE		; RP!
		dw	RFROM		; R>
		dw	HANDLER		; HANDLER
		dw	STORE		; !
		dw	RFROM		; R>
		dw	_SWAP		; SWAP
		dw	TOR		; >R
		dw	SPSTORE		; SP!
		dw	DROP		; DROP
		dw	RFROM		; R>
		dw	EXIT		; EXIT
;
;   NULL$	( -- a )
;		Return address of a null string with zero count.
;
		%colon	5,'NULL$',NULLS
		dw	DOVAR		; Emulate CREATE
		dw	0		; 0
;
;   ABORT	( -- )
;		Reset data stack and jump to QUIT.
;
		%colon	5,'ABORT',ABORT
		dw	NULLS		; NULL$
		dw	THROW		; THROW
;
;   (ABORT")	( f -- )
;		Run time routine of ABORT" . Abort with a message.
;
		%colon	Lex_CO+8,'(ABORT")',PABORTQP
		dw	QBRAN,ABOR1	; ?branch ABOR1
		dw	DOSTR		; do$
		dw	THROW		; THROW
ABOR1:		dw	DOSTR		; do$
		dw	DROP		; DROP
		dw	EXIT		; EXIT
;
;******************************************************************************
;
;  The Text Interpreter
;
;   $INTERPRET	( a -- )
;		Interpret a word. If failed, try to convert it to an integer.

		%colon	10,'$INTERPRET',INTER
		dw	NAMEQ		; NAME?
		dw	QDUP		; ?DUP
		dw	QBRAN,INTE1	; ?branch INTE1
		dw	CAT		; C@
		dw	DOLIT		; LITERAL
		dw	Lex_CO		; Compile Only Bit
		dw	AND		; AND
		%dmm	PABORTQP,' compile only'
		dw	EXECUTE		; EXECUTE
		dw	EXIT		; EXIT
INTE1:		dw	TNUMBER		; 'NUMBER
		dw	ATEXECUTE	; @EXECUTE
		dw	QBRAN,INTE2	; ?branch INTE2
		dw	EXIT		; EXIT
INTE2:		dw	THROW		; Error
;
;   [		( -- )
;		Start the text interpreter.
;
		%colon	Lex_IM+1,'[',LBRAC
		dw	DOLIT		; LITERAL
		dw	INTER		; $INTERPET
		dw	TEVAL		; 'EVAL
		dw	STORE		; !
		dw	EXIT		; EXIT
;
;   .OK		( -- )
;		Display 'ok' only while interpreting.
;
		%colon	3,'.OK',DOTOK
		dw	DOLIT		; LITERAL
		dw	INTER		; $INTERPET
		dw	TEVAL		; 'EVAL
		dw	GET		; @
		dw	EQUAL		; =
		dw	QBRAN,DOTO1	; ?branch DOTO1
		%dmm	PDOTQP,' ok'	; Message Text
DOTO1:		dw	CR		; CR
		dw	EXIT		; EXIT
;
;   ?STACK	( -- )
;		Abort if the data stack underflows.
;
		%colon	6,'?STACK',QSTACK
		dw	DEPTH		; DEPTH
		dw	ZLESS		; 0<
		%dmm	PABORTQP,' underflow'
		dw	EXIT		; EXIT
;
;   EVAL	( -- )
;		Interpret the input stream.
;
		%colon	4,'EVAL',EVAL
EVAL1:		dw	TOKEN		; TOKEN
		dw	DUP		; DUP
		dw	CAT		; C@
		dw	QBRAN,EVAL2	; ?branch EVAL2
		dw	TEVAL		; 'EVAL
		dw	ATEXECUTE	; @EXECUTE
		dw	QSTACK		; ?STACK
		dw	BRAN,EVAL1	; Goto EVAL1
EVAL2:		dw	DROP		; DROP
		dw	TPROM		; 'PROMPT
		dw	ATEXECUTE	; @EXECUTE
		dw	EXIT		; EXIT
;
;******************************************************************************
;
;  Shell
;
;   PRESET	( -- )
;		Reset data stack pointer and the terminal input buffer.

		%colon	6,'PRESET',PRESET
		dw	SZERO		; SP0
		dw	GET		; @
		dw	SPSTORE		; SP!
		dw	DOLIT		; LITERAL
		dw	TIBBtm		; TIB
		dw	NTIB		; #TIB
		dw	CELLP		; CELL+
		dw	STORE		; !
		dw	EXIT		; EXIT
;
;   QUIT	( -- )
;		Reset return stack pointer and start text interpreter.
;
		%colon	4,'QUIT',QUIT
		dw	RZERO		; RP0
		dw	GET		; @
		dw	RPSTORE		; RP!
QUIT1:		dw	LBRAC		; [
QUIT2:		dw	QUERY		; QUERY
		dw	DOLIT		; LITERAL
		dw	EVAL		; EVAL
		dw	CATCH		; Execute EVAL, With Error Frame
		dw	QDUP		; ?DUP
		dw	QBRAN,QUIT2	; ?branch QUIT2
		dw	NULLS		; NULL$
		dw	OVER		; OVER
		dw	XOR		; XOR
		dw	QBRAN,QUIT3	; ?branch QUIT3
		dw	SPACE		; SPACE
		dw	COUNT		; COUNT
		dw	TYPE		; TYPE
		%dmm	PDOTQP,' ? '	; Error Prompt
QUIT3:		dw	PRESET		; PRESET
		dw	BRAN,QUIT1	; Branch To QUIT1
;
;******************************************************************************
;
;  The compiler
;
;   '		( -- cfa )
;		Search context vocabularies for the next word in input stream.
;
		%colon	1,"'",TICK
		dw	TOKEN		; TOKEN
		dw	NAMEQ		; NAME?
		dw	QBRAN,TICK1	; ?branch TICK1
		dw	EXIT		; EXIT
TICK1:		dw	THROW		; Error
;
;   ALLOT	( n -- )
;		Allocate n bytes to the code dictionary.
;
		%colon	5,'ALLOT',ALLOT
		dw	CP		; CP
		dw	PSTORE		; +!
		dw	EXIT		; EXIT
;
;   ,		( w -- )
;		Compile an integer into the code dictionary.
;
		%colon	1,',',COMMA
		dw	HERE		; HERE
		dw	DUP		; DUP
		dw	CELLP		; CELL+
		dw	CP		; CP
		dw	STORE		; !
		dw	STORE		; !
		dw	EXIT		; EXIT
;
;   [COMPILE]	( -- ; <string> )
;		Compile the next immediate word into code dictionary.
;
		%colon	Lex_IM+9,'[COMPILE]',BCOMPILE
		dw	TICK		; '
		dw	COMMA		; ,
		dw	EXIT		; EXIT
;
;   COMPILE	( -- )
;		Compile the next address in colon list to code dictionary.
;
		%colon	Lex_CO+7,'COMPILE',COMPILE
		dw	RFROM		; R>
		dw	DUP		; DUP
		dw	GET		; @
		dw	COMMA		; ,
		dw	CELLP		; CELL+
		dw	TOR		; >R
		dw	EXIT		; EXIT
;
;   LITERAL	( w -- )
;		Compile tos to code dictionary as an integer literal.
;
		%colon	Lex_IM+7,'LITERAL',LITERAL
		dw	COMPILE		; COMPILE
		dw	DOLIT		; doLIT
		dw	COMMA		; ,
		dw	EXIT		; EXIT
;
;   $,"		( -- )
;		Compile a literal string up to next " in code space
;
		%colon	3,'$,"',STRCQ
		dw	DOLIT		; LITERAL
		dw	'"'		; "
		dw	WORD		; WORD
		dw	COUNT		; COUNT
		dw	PLUS		; +
		dw	CP		; CP
		dw	STORE		; !
		dw	EXIT		; EXIT
;
;   RECURSE	( -- )
;		Make the current word available for compilation.
;
		%colon	Lex_IM+7,'RECURSE',RECURSE
		dw	LAST		; LAST
		dw	GET		; @
		dw	NAMET		; NAME>
		dw	COMMA		; ,
		dw	EXIT		; EXIT
;
;******************************************************************************
;
;  Structures
;
;   I		( -- n )
;		Return loop index of inner most loop
;
		%code	1,'I',I_INDEX
		lcall	IndexSub
		ljmp	Return
;
;   J		( -- n )
;		Return loop index of second inner most loop
;
		%code	1,'J',J_INDEX
		inc	rpl
		inc	rpl
		inc	rpl
		inc	rpl
		lcall	IndexSub
		dec	rpl
		dec	rpl
		dec	rpl
		dec	rpl
		ljmp	Return
;
IndexSub:	lcall	PushSP
		lcall	PopSPRS
		lcall	PopDPRS
		mov	a,tpl
		add	a,dpl
		mov	tpl,a
		mov	a,tph
		addc	a,dph
		mov	tph,a
		dec	rpl
		dec	rpl
		dec	rpl
		dec	rpl
		ret
;
;   BEGIN	( -- a )
;		Start an infinite or indefinite loop structure.
;
		%colon	Lex_IM+5,'BEGIN',BEGIN
		dw	HERE		; HERE
		dw	EXIT		; EXIT
;
;   UNTIL	( a -- )
;		Terminate a BEGIN-UNTIL indefinite loop structure.
;
		%colon	Lex_IM+5,'UNTIL',UNTIL
		dw	COMPILE		; COMPILE
		dw	QBRAN		; ?branch
		dw	COMMA		; ,
		dw	EXIT		; EXIT
;
;   AGAIN	( a -- )
;		Terminate a BEGIN-AGAIN infinite loop structure.
;
		%colon	Lex_IM+5,'AGAIN',AGAIN	
		dw	COMPILE		; COMPILE
		dw	BRAN		; branch
		dw	COMMA		; ,
		dw	EXIT		; EXIT
;
;   IF		( -- A )
;		Begin a conditional branch structure.
;
		%colon	Lex_IM+2,'IF',_IF
		dw	COMPILE		; COMPILE
		dw	QBRAN		; ?branch
		dw	HERE		; HERE
		dw	ZERO		; 0
		dw	COMMA		; ,
		dw	EXIT		; EXIT
;
;   AHEAD	( -- A )
;		Compile a forward branch instruction.
;
		%colon	Lex_IM+5,'AHEAD',AHEAD
		dw	COMPILE		; COMPILE
		dw	BRAN		; branch
		dw	HERE		; HERE
		dw	ZERO		; 0
		dw	COMMA		; ,
		dw	EXIT		; EXIT
;
;   REPEAT	( A a -- )
;		Terminate a BEGIN-WHILE-REPEAT indefinite loop.
;
		%colon	Lex_IM+6,'REPEAT',REPEAT
		dw	AGAIN		; AGAIN
		dw	HERE		; HERE
		dw	_SWAP		; SWAP
		dw	STORE		; !
		dw	EXIT		; EXIT
;
;   THEN	( A -- )
;		Terminate a conditional branch structure.
;
		%colon	Lex_IM+4,'THEN',_THEN
		dw	HERE		; HERE
		dw	_SWAP		; SWAP
		dw	STORE		; !
		dw	EXIT		; EXIT
;
;   ELSE	( A -- A )
;		Start the false clause in an IF-ELSE-THEN structure.
;
		%colon	Lex_IM+4,'ELSE',_ELSE
		dw	AHEAD		; AHEAD
		dw	_SWAP		; SWAP
		dw	_THEN		; THEN
		dw	EXIT		; EXIT
;
;   WHILE	( a -- A a )
;		Conditional branch out of a BEGIN-WHILE-REPEAT loop.
;
		%colon	Lex_IM+5,'WHILE',WHILE
		dw	_IF		; IF
		dw	_SWAP		; SWAP
		dw	EXIT		; EXIT
;
;   ABORT"	( -- ; <string> )
;		Conditional abort with an error message.
;
		%colon	Lex_IM+6,'ABORT"',ABORTQ
		dw	COMPILE		; COMPILE
		dw	PABORTQP	; (ABORT")
		dw	STRCQ		; $,"
		dw	EXIT		; EXIT
;
;   $"		( -- ; <string> )
;		Compile an inline string literal.
;
		%colon	Lex_IM+2,'$"',STRQ
		dw	COMPILE		; COMPILE
		dw	PSTRQP		; ($")
		dw	STRCQ		; $,"
		dw	EXIT		; EXIT
;
;   ."		( -- ; <string> )
;		Compile an inline string literal to be typed out at run time.
;
		%colon	Lex_IM+2,'."',DOTQ
		dw	COMPILE		; COMPILE
		dw	PDOTQP		; (.")
		dw	STRCQ		; $,"
		dw	EXIT		; EXIT
;
;******************************************************************************
;
;  Name Compiler
;
;   ?UNIQUE	( a -- a )
;		Display a warning message if the word already exists.
;
		%colon	7,'?UNIQUE',QUNIQUE
		dw	DUP		; DUP
		dw	NAMEQ		; ?NAME
		dw	QBRAN,QUNIQ1	; ?branch UNIQ1
		%dmm	PDOTQP,' reDef '; Warn User Of Redefinition
		dw	OVER		; OVER
		dw	COUNT		; COUNT
		dw	TYPE		; TYPE
QUNIQ1:		dw	DROP		; DROP
		dw	EXIT		; EXIT
;
;   $,n		( nfa -- )
;		Build a new dictionary name using the string at na.
;
		%colon	3,'$,n',SNAME
		dw	DUP		; DUP
		dw	CAT		; C@
		dw	QBRAN,PNAM1	; ?branch PNAM1
		dw	QUNIQUE		; ?UNIQUE
		dw	DUP		; DUP
		dw	LAST		; LAST
		dw	STORE		; !
		dw	HERE		; HERE
		dw	_SWAP		; SWAP
		dw	CELLM		; CELL-
		dw	CURRENT		; CURRENT
		dw	GET		; @
		dw	GET		; @
		dw	OVER		; OVER
		dw	STORE		; !
		dw	CELLM		; CELL-
		dw	DUP		; DUP
		dw	NP		; NP
		dw	STORE		; !
		dw	STORE		; !
		dw	EXIT		; EXIT
PNAM1:		%dmm	PSTRQP,' name'	; Null Input
		dw	THROW		; THROW
;
;******************************************************************************
;
;   FORTH compiler
;
;   $COMPILE	( a -- )
;		Compile next word to code dictionary as a token or literal.
;
		%colon	8,'$COMPILE',SCOMPILE
		dw	NAMEQ		; NAME?
		dw	QDUP		; ?DUP
		dw	QBRAN,SCOM2	; ?branch SCOM2
		dw	CAT		; C@
		dw	DOLIT		; LITERAL
		dw	Lex_IM		; Immediate Mode?
		dw	AND		; AND
		dw	QBRAN,SCOM1	; ?branch SCOM1
		dw	EXECUTE		; EXECUTE
		dw	EXIT		; EXIT
SCOM1:		dw	COMMA		; ,
		dw	EXIT		; EXIT
SCOM2:		dw	TNUMBER		; 'NUMBER
		dw	ATEXECUTE	; @EXECUTE
		dw	QBRAN,SCOM3	; ?branch SCOM3
		dw	LITERAL		; LITERAL
		dw	EXIT		; EXIT
SCOM3:		dw	THROW		; Error
;
;   OVERT	( -- )
;		Link a new word into the current vocabulary.
;
		%colon	5,'OVERT',OVERT
		dw	LAST		; LAST
		dw	GET		; @
		dw	CURRENT		; CURRENT
		dw	GET		; @
		dw	STORE		; !
		dw	EXIT		; EXIT
;
;   ;		( -- )
;		Terminate a colon definition.
;
		%colon	Lex_IM+Lex_CO+1,';',SEMIS
		dw	COMPILE		; COMPILE
		dw	EXIT		; EXIT
		dw	LBRAC		; ]
		dw	OVERT		; OVERT
		dw	EXIT		; EXIT
;
;   ]		( -- )
;		Start compiling the words in the input stream.
;
		%colon	1,']',RBRAC
		dw	DOLIT		; LITERAL
		dw	SCOMPILE	; $COMPILE
		dw	TEVAL		; 'EVAL
		dw	STORE		; !
		dw	EXIT		; EXIT
;
;   call,	( cfa -- )
;		Assemble a call instruction to cfa.
;
		%colon	5,'call,',CALLC
		dw	DOLIT		; LITERAL
		dw	Calll		; CALL
		dw	CCOMMA		; ,
		dw	COMMA		; ,
		dw	EXIT		; EXIT
;
;   :		( -- ; <string> )
;		Start a new colon definition using next word as its name.
;
		%colon	1,':',COLON
		dw	TOKEN		; TOKEN
		dw	SNAME		; $,n
		dw	DOLIT		; LITERAL
		dw	DOLIST		; doLST
		dw	CALLC		; CALL,
		dw	RBRAC		; ]
		dw	EXIT		; EXIT
;
;   IMMEDIATE	( -- )
;		Make the last compiled word an immediate word.
;
		%colon	9,'IMMEDIATE',IMMEDIATE
		dw	DOLIT		; LITERAL
		dw	Lex_IM		; Immediate Mode Bit
		dw	LAST		; LAST
		dw	GET		; @
		dw	CAT		; C@
		dw	OR		; OR
		dw	LAST		; LAST
		dw	GET		; @
		dw	CSTORE		; C!
		dw	EXIT		; EXIT
;
;******************************************************************************
;  
;  Defining Words
;
;   USER	( u -- ; <string> )
;		Compile a new user variable.
;
		%colon	4,'USER',USER
		dw	TOKEN		; TOKEN
		dw	SNAME		; $,n
		dw	OVERT		; OVERT
		dw	DOLIT		; LITERAL
		dw	DOLIST		; doLST
		dw	CALLC		; CALL,
		dw	COMPILE		; COMPILE
		dw	DOUSER		; doUSE
		dw	COMMA		; ,
		dw	EXIT		; EXIT
;
;   CREATE	( -- ; <string> )
;		Compile a new array entry without allocating code space.
;
		%colon	6,'CREATE',CREATE
		dw	TOKEN		; TOKEN
		dw	SNAME		; $,n
		dw	OVERT		; OVERT
		dw	DOLIT		; LITERAL
		dw	DOLIST		; doLST
		dw	CALLC		; CALL,
		dw	COMPILE		; COMPILE
		dw	DOVAR		; doVAR
		dw	EXIT		; EXIT
;
;   VARIABLE	( -- ; <string> )
;		Compile a new variable initialized to 0.
;
		%colon	8,'VARIABLE',VARIABLE
		dw	CREATE		; CREATE
		dw	ZERO		; 0
		dw	COMMA		; ,
		dw	EXIT		; EXIT
;
;   !CSP	( -- )
;		Save stack pointer in CSP for error checking.
;
		%colon	4,'!CSP',STCSP
		dw	SPAT		; SP@
		dw	CSP		; CSP
		dw	STORE		; !
		dw	EXIT		; EXIT
;
;   ?CSP	( -- )
;		Abort if stack pointer differs from that saved in CSP.
;
		%colon	4,'?CSP',QCSP
		dw	SPAT		; SP@
		dw	CSP		; CSP
		dw	GET		; @
		dw	XOR		; XOR
		%dmm	PABORTQP,'stacks'
		dw	EXIT		; EXIT
;
;******************************************************************************
;
;  Utility Words
;
;   _TYPE	( b u -- )
;		Display a string. Filter non-printing characters.
;		FOR DUP C@ >CHAR EMIT 1+ LOOP DROP EXIT
;
		if	Word_SEE | Word_DUMP | Word_WORDS
		%code	5,'_TYPE',UTYPE
		mov	gpl,tpl
		mov	gph,tph
		lcall	PopDP
		mov	a,gpl
		orl	a,gph
		jz	Uty_2
		mov	a,gpl
		jz	Uty_1
		inc	gph
Uty_1:		lcall	PushSP
		movx	a,@dptr
		mov	tpl,a
		mov	tph,#000h
		push	dph
		push	dpl
		%sforth
		dw	TCHAR
		dw	EMIT
		dw	SASMBLY
		pop	dpl
		pop	dph
		inc	dptr
		djnz	gpl,Uty_1
		djnz	gph,Uty_1
Uty_2:		ljmp	PopReturn
		endif
;
;   dm+		( a u -- a )
;		Dump u bytes from , leaving a+u on the stack.
;
		if	Word_DUMP
		%colon	3,'dm+',DMP
		dw	OVER		; OVER
		dw	DOLIT		; LITERAL
		dw	4		; 4
		dw	UDOTR		; U.R
		dw	SPACE		; SPACE
		dw	TOR		; >R
		dw	BRAN,PDUM2	; Goto PDUM2
PDUM1:		dw	DUP		; DUP
		dw	CAT		; C@
		dw	DOLIT		; LITERAL
		dw	3		; 3
		dw	UDOTR		; U.R
		dw	ONEPLUS		; 1+
PDUM2:		dw	PNEXTP,PDUM1	; LOOP
		dw	EXIT		; EXIT
		endif
;
;   DUMP	( a u -- )
;		Dump u bytes from a, in a formatted manner.
;
		if	Word_DUMP
		%colon	4,'DUMP',DUMP
		dw	BASE		; BASE
		dw	GET		; @
		dw	TOR		; >R
		dw	HEX		; HEX
		dw	DOLIT		; LITERAL
		dw	16		; 16
		dw	SLASH		; /
		dw	TOR		; >R
DUMP1:		dw	CR		; CR
		dw	DOLIT		; LITERAL
		dw	16		; 16
		dw	DDUP		; 2DUP
		dw	DMP		; DM+
		dw	MROT		; -ROT
		dw	SPACE		; SPACE
		dw	SPACE		; SPACE
		dw	UTYPE		; _TYPE
		dw	NUFQ		; NUF?
		dw	INVERT		; NOT
		dw	QBRAN,DUMP2	; ?branch DUMP2
		dw	PNEXTP,DUMP1	; LOOP
		dw	BRAN,DUMP3	; Goto DUMP3
DUMP2:		dw	RDROP		; R>DROP
DUMP3:		dw	DROP		; DROP
		dw	RFROM		; R>
		dw	BASE		; BASE
		dw	STORE		; !
		dw	EXIT		; EXIT
		endif
;
;   .S		( ... -- ... )
;		Display the contents of the data stack.
;
		if	Word_DOTS
		%colon	2,'.S',DOTS
		dw	CR		; CR
		dw	DEPTH		; DEPTH
		dw	TOR		; >R
		dw	BRAN,DOTS2	; Goto DOTS2
DOTS1:		dw	RAT		; R@
		dw	PICK		; PICK
		dw	DOT		; .
DOTS2:		dw	PNEXTP,DOTS1	; LOOP
		%dmm	PDOTQP,' <sp'	; Message
		dw	EXIT		; EXIT
		endif
;
;   >NAME	( cfa -- nfa | F )
;		Convert code address to a name address.
;
		if	Word_TNAME | Word_SEE
		%code	5,'>NAME',TNAME
		mov	dph,tph
		mov	dpl,tpl
		clr	rs0
		mov	r6,dph
		mov	r7,dpl
		mov	dptr,#CURRENT_Vec
Tna_1:		inc	dptr
		inc	dptr
		movx	a,@dptr
		mov	r4,a
		inc	dptr
		movx	a,@dptr
		mov	r5,a
		orl	a,r4
		jz	Tna_4
		mov	dph,r4
		mov	dpl,r5
Tna_2:		movx	a,@dptr	
		mov	r2,a
		inc	dptr
		movx	a,@dptr
		mov	r3,a
		orl	a,r2
		jnz	Tna_3
		mov	dph,r4
		mov	dpl,r5
		sjmp	Tna_1
Tna_3:		clr	c
		mov	a,r3
		subb	a,#WrdLen*2
		mov	dpl,a
		mov	a,r2
		subb	a,#000h
		mov	dph,a
		movx	a,@dptr
		inc	dptr
		mov	b,a
		movx	a,@dptr
		inc	dptr
		xrl	a,r7
		xch	a,b
		xrl	a,r6
		orl	a,b
		jnz	Tna_2
		mov	dph,r2
		mov	dpl,r3
		setb	rs0
		mov	tph,dph
		mov	tpl,dpl
		ljmp	Return
Tna_4:		setb	rs0
		%loadtp	0
		ljmp	Return
		endif
;
;   .ID		( nfa -- )
;		Display the name at address.
;
		if	Word_SEE | Word_WORDS
		%colon	3,'.ID',DOTID
		dw	QDUP		; ?DUP
		dw	QBRAN,DOTI1	; ?branch DOTI1
		dw	COUNT		; COUNT
		dw	DOLIT		; LITERAL
		dw	01FH		; 31
		dw	AND		; AND
		dw	UTYPE		; _TYPE
		dw	EXIT		; EXIT
DOTI1:		%dmm	PDOTQP,' {noName}'
		dw	EXIT		; EXIT
		endif
;
;   SEE		( -- ; <string> )
;		A simple decompiler.
;
		if	Word_SEE
		%colon	3,'SEE',SEE
		dw	TICK		; '
		dw	CR		; CR
		dw	ONEPLUS		; 1+ (Skip Call Instruction)
SEE1:		dw	CELLP		; CELL+
		dw	DUP		; DUP
		dw	GET		; @
		dw	DUP		; DUP
		dw	QBRAN,SEE2	; ?branch SEE2
		dw	TNAME		; >NAME
SEE2:		dw	QDUP		; ?DUP
		dw	QBRAN,SEE3	; ?branch SEE3
		dw	DOTID		; .ID
		dw	SPACE		; SPACE
		dw	BRAN,SEE4	; Goto SEE4
SEE3:		dw	DUP		; DUP
		dw	GET		; @
		dw	UDOT		; U.
SEE4:		dw	NUFQ		; NUF?
		dw	QBRAN,SEE1	; ?branch SEE1
		dw	DROP		; DROP
		dw	EXIT		; EXIT
		endif
;
;   WORDS	( -- )
;		Display the names in the context vocabulary.
;
		if	Word_WORDS
		%colon	5,'WORDS',WORDS
		dw	CR		; CR
		dw	CONTEXT		; CONTEXT
		dw	GET		; @
WORS1:		dw	GET		; @
		dw	QDUP		; ?DUP
		dw	QBRAN,WORS2	; ?branch WORS2
		dw	DUP		; DUP
		dw	SPACE		; SPACE
		dw	DOTID		; .ID
		dw	CELLM		; CELL-
		dw	NUFQ		; NUF?
		dw	QBRAN,WORS1	; ?branch WORS1
		dw	DROP		; DROP
WORS2:		dw	EXIT		; EXIT
		endif
;
;******************************************************************************
;
;  Startup Words
;
;   VER		( -- n )
;		Return the version number of this implementation.
;
		%colon	3,'VER',VERSION
		dw	DOLIT
		dw	Ver_Major*256+Ver_Minor
		dw	EXIT
;
;   hi		( -- )
;		Display the sign-on message of eForth.
;
		%colon	2,'hi',HI
		if	Word_AUTOBAUD
		dw	AUTOBAUD	; AUTOBAUD
		else
		dw	STOIO		; !IO
		endif
		dw	CR		; CR
		%dmm	PDOTQP,'eForth v'; Message Text
		dw	BASE		; BASE
		dw	GET		; @
		dw	HEX		; HEX
		dw	VERSION		; VER
		dw	SNGLTODBL	; S->D
		dw	BDIGS		; <#
		dw	DIG		; #
		dw	DIG		; #
		dw	DOLIT		; LITERAL
		dw	'.'		; '.'
		dw	HOLD		; HOLD
		dw	DIGS		; #S
		dw	EDIGS		; #>
		dw	TYPE		; TYPE
		dw	BASE		; BASE
		dw	STORE		; !
		dw	CR		; CR
		if	Word_AUTOBAUD
		%dmm	PDOTQP,'console rate '
		dw	BAUD		; BAUD
		dw	GET		; @
		dw	DOT		; .
		dw	CR		; CR
		endif
		dw	FREE		; FREE
		dw	EXIT		; EXIT
;
;   'BOOT	( -- a )
;		The application startup vector.
;
		%colon	5,"'BOOT",TBOOT
		dw	DOVAR		; doVAR
		dw	HI		; hi
;
;   COLD	( -- )
;		The hilevel cold start sequence.
;
		%colon	4,'COLD',COLD
Col_1:		dw	DOLIT		; LITERAL
		dw	UsrBtm		; Start Of User Area
		dw	DOLIT		; LITERAL
		dw	UP0Btm		; RAM Where User Variables Go
		dw	DOLIT		; LITERAL
		dw	UsrLen		; Length Of User Area
		dw	CMOVE		; CMOVE (From ROM To RAM)
		dw	PRESET		; PRESET
		dw	TBOOT		; 'BOOT
		dw	ATEXECUTE	; @EXECUTE
		dw	FORTH		; FORTH As Current Vocabulary
		dw	CONTEXT		; CONTEXT
		dw	GET		; @
		dw	DUP		; DUP
		dw	CURRENT		; CURRENT
		dw	DSTORE		; D!
		dw	OVERT		; (Useless?)
		dw	QUIT		; QUIT
		dw	BRAN,Col_1	; Branch Col_1 (Just In Case)
;
;******************************************************************************
;
LstNam		equ	_name+4		; Last Name Address
NamBtm		equ	_name-0		; Next Avlbl Memory In Name Dictionary
CodTop		equ	$+0		; Next Avlbl Memory In Code Dictionary
FreeROM		equ	NamBtm-CodTop	; Free Space In ROM
		end
