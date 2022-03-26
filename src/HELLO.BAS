'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

' 4.01 corrects the overflow problem with the draft program

DEFINT A-Z
DECLARE FUNCTION PadRight$ (X$, Length)

DIM D$(3), item$(18)
DIM A$(1 TO 15)

OPEN "DEV.BAS" FOR INPUT AS #1
	FOR I = 0 TO 1
	INPUT #1, D$(I)
	NEXT
CLOSE 1

CALL Comline(N, A$(), 10)     ' Get what was typed on the command line
'FOR I = 1 TO N: PRINT A$(I): NEXT I
	IF A$(1) = "2" THEN A$(1) = "": GOTO 700
	IF A$(1) = "3" THEN A$(1) = "": GOTO 800
	IF A$(1) = "4" THEN A$(1) = "": GOTO 750

1  CALL TITLE
	item$(1) = "PLAY SINGLE GAME                "
	item$(2) = "TEAM MANAGEMENT / ORGANIZATION  "
	item$(3) = "REPLAY STATISTICS AND REPORTS   "
	item$(4) = "LEADERS / STANDINGS / RECORDS   "
	item$(5) = "SCHEDULER / AUTOPLAY / BOXSCORES"
	item$(6) = "HARD DRIVE INSTALLATION         "
	item$(7) = "CONFIGURE DRIVES FOR TEAMS,STATS"
	item$(8) = "EXIT TO DOS                     "
	SELECTION% = 1
	Count% = 8
	Start% = 1
	SCANCODE% = 8
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 25

CALL box0(7, 24, 16, 57, 2, 31)
	
_MouseShow "DEFAULT"
MMenuVert item$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN END
	 IF SELECTION% = 1 THEN RUN "PROHOOP.EXE"
	IF SELECTION% = 2 THEN 700
	IF SELECTION% = 3 THEN 750
	IF SELECTION% = 4 THEN 800
	IF SELECTION% = 5 THEN RUN "SCHEDULE.EXE"
	IF SELECTION% = 6 THEN RUN "INSTALL.EXE"
	IF SELECTION% = 7 THEN 505
	IF SELECTION% = 8 THEN CLS : END

505 COLOR 15, 1
	 FG = 0
	 BG = 7
	 FOR I = 0 TO 1
	 D$(I) = LEFT$(D$(I), LEN(D$(I)) - 1)
	 NEXT
	Colr = OneColor%(FG, BG)             'pack FG and BG into a single byte
	CLS : LOCATE , 16: PRINT "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS": PRINT

510 LOCATE , 31: PRINT "PATHNAME EXAMPLES:": PRINT

515 LOCATE , 31: PRINT "A:"

520 LOCATE , 31: PRINT "C:\PROBB\TEAMS"
	LOCATE 7, 1: QPrint "Path for Teams ", 30, -1
	LOCATE , 16: QPrint PadRight$(D$(0), 30), FG, BG, -1
	LOCATE 9, 1: QPrint "Path for Stats ", 30, -1
	LOCATE , 16: QPrint PadRight$(D$(1), 30), FG, BG, -1

GetDiv1:
	LOCATE 7, 16                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 30     'specify field parameters
	CALL TextIn(D$(0), Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO GetDiv1:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done               'all done entering data
	D$(0) = _TRIM$(D$(0))
GetDiv2:
	LOCATE 9, 16                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 30    'specify field parameters
	CALL TextIn(D$(1), Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO GetDiv1:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done               'all done entering data
	D$(1) = _TRIM$(D$(1))

Done:
	LOCATE 23, 1
	PRINT "Is this okay? (Yes/No) ";
	X$ = ""
	CALL YesNoB(X$, XCode, FG, BG)
	LOCATE , 1
	ClearEOL 14, 1
	IF XCode = 1 GOTO GetDiv2
	IF X$ = "N" GOTO GetDiv1
	IF X$ <> "Y" GOTO Done               'insist on either Y or N or up-arrow

580 OPEN "DEV.BAS" FOR OUTPUT AS #1

585 FOR R = 0 TO 2: D$(R) = D$(R) + "\": NEXT R

590 PRINT #1, D$(0)

595 PRINT #1, D$(1)

600 CLOSE #1
	 GOTO 1

700 CALL TITLE
	item$(1) = "INPUT A NEW TEAM              "
	item$(2) = "EDIT AN EXISTING TEAM         "
	item$(3) = "PRINT TEAM ROSTER / REPORT    "
	item$(4) = "VIEW TEAM ROSTER / REPORT     "
	item$(5) = "DRAFT TO NEW / EXISTING TEAM  "
	item$(6) = "TRADE PLAYERS                 "
	item$(7) = "ADJUST PLAYING TIME (MIN/GAME)"
	item$(8) = "RETURN TO MAIN MENU           "
	SELECTION% = 1
	Count% = 8
	Start% = 1
	SCANCODE% = 8
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 27
CALL box0(7, 26, 16, 57, 2, 31)
	
_MouseShow "DEFAULT"
MMenuVert item$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN 1
	IF SELECTION% = 1 THEN RUN "BINPUT.EXE 1"
	IF SELECTION% = 2 THEN RUN "BINPUT.EXE 2"
	IF SELECTION% = 3 THEN RUN "BINPUT.EXE 3"
	IF SELECTION% = 4 THEN RUN "BINPUT.EXE 4"
	IF SELECTION% = 5 THEN RUN "DRAFT.EXE"
	IF SELECTION% = 6 THEN RUN "TRADE.EXE"
	IF SELECTION% = 7 THEN RUN "GPMIN.EXE"
	IF SELECTION% = 8 THEN 1

750 CALL TITLE
	item$(1) = "VIEW / PRINT COMPILED STATISTICS "
	item$(2) = "ENCYCLOPEDIA/ TEAM REPLAY OPTIONS"
	item$(3) = "COMPARE REPLAY STATS TO ACTUAL   "
	item$(4) = "MERGE SEPARATE STATISTICAL FILES "
	item$(5) = "RETURN TO MAIN MENU              "
	SELECTION% = 1
	Count% = 5
	Start% = 1
	SCANCODE% = 5
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 25

CALL box0(7, 24, 13, 58, 2, 31)
	
_MouseShow "DEFAULT"
MMenuVert item$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN 1
	IF SELECTION% = 1 THEN RUN "LOOKY.EXE"
	IF SELECTION% = 2 THEN RUN "CAREER.EXE"
	IF SELECTION% = 3 THEN RUN "COMPARE.EXE"
	IF SELECTION% = 4 THEN RUN "MERGE.EXE"
	IF SELECTION% = 5 THEN 1

800 CALL TITLE
	item$(1) = "CREATE/ EDIT A LEAGUE         "
	item$(2) = "UPDATE TOP 10'S/ STANDINGS    "
	item$(3) = "VIEW/ PRINT TOP 10'S/STANDINGS"
	item$(4) = "HEAD-TO-HEAD RESULTS AND STATS"
	item$(5) = "LEAGUE RECORD BOOKS           "
	item$(6) = "EXPANDED LEADERS/STANDINGS    "
	item$(7) = "DELETE ALL STATS FOR A LEAGUE "
	item$(8) = "RETURN TO MAIN MENU           "
	SELECTION% = 1
	Count% = 7
	Start% = 1
	SCANCODE% = 7
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 26
CALL box0(7, 25, 15, 56, 2, 31)
	
_MouseShow "DEFAULT"
MMenuVert item$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN 1
	IF SELECTION% = 1 THEN RUN "CREATE.EXE"
	IF SELECTION% = 2 THEN RUN "LEAGCMPL.EXE"
	IF SELECTION% = 3 THEN RUN "SEELEAG.EXE"
	IF SELECTION% = 4 THEN RUN "HD2HD.EXE"
	IF SELECTION% = 5 THEN RUN "REC.EXE"
	IF SELECTION% = 6 THEN RUN "NEWLDR.EXE"
	IF SELECTION% = 7 THEN RUN "BLANK.EXE"
	IF SELECTION% = 8 THEN 1

FUNCTION PadRight$ (X$, Length) 
'Essentially this pads space to the right of the string
	PadRight$ = X$ + SPACE$(Length - LEN(X$))  'for display only
END FUNCTION

SUB TITLE ()
	COLOR , 0: KEY OFF: CLS
	COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 27: COLOR 12
	PRINT "BASKETBALL: THE PRO GAME V4.1"
	LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
	PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1997"; TAB(70); LEFT$(TIME$, 5);
	LOCATE 3, 1: COLOR 2, 0
	COLOR 15: PRINT "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
	  PRINT "       FOR TECHNICAL SUPPORT, CALL (800) 477-7032 OR ecciLR@AOL.COM"
	COLOR 10
END SUB

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
