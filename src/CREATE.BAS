'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DEFINT A-Z
DECLARE SUB TITLE ()
DECLARE FUNCTION FnExt$ (X$, Length)

DIM Z1$(40), Z2$(40), Z1%(40), D$(1), T%(34), T1$(1)
DIM Q$(500), VV$(200), V%(200), item$(4)
DIM Y AS LONG

'determine if the monitor is mono or color
FG = 0
BG = 7
Colr = OneColor%(FG, BG)             'pack FG and BG into a single byte

OPEN "DEV.BAS" FOR INPUT AS #2
	FOR I = 0 TO 1
	INPUT #2, D$(I)
	NEXT
CLOSE 2
	CLS

50 FOR X = 1 TO 40
	Z1$(X) = "XXX": Z2$(X) = "XXX": Z1%(X) = -1
	NEXT
	CALL TITLE
	item$(2) = "ADD / REVIEW LEAGUE MEMBERS     "
	item$(1) = "CREATE / DEFINE NEW LEAGUES     "
	item$(3) = "RETURN TO STANDINGS/LEADERS MENU"
	SELECTION% = 1
	Count% = 3
	Start% = 1
	SCANCODE% = 3
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 25

CALL box0(7, 24, 11, 57, 2, 31)
	
_MouseShow "DEFAULT"
MMenuVert VARPTR(item$(1)), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN 1195
	I1 = SELECTION%
	IF I1 = 2 THEN 4000
	IF I1 = 3 THEN 1195

130 COLOR 15, 0: CLS
	JJ = 7
	LOCATE 6, 8: COLOR 15, 1
	PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
	PRINT " º"; STRING$(61, 32); "º "; : NEXT
	LOCATE 7 + JJ, 8
	PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
	LOCATE , 12: PRINT "ENTER NAME FOR LEAGUE: ": PRINT
	LOCATE , 12: PRINT "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
	LOCATE , 12: PRINT "SPACES."
	LOCATE , 12: PRINT "SAMPLE NAMES ARE WHA IHL NHL94-95"
	LOCATE 7, 37: INPUT LG$
	IF LG$ = "" THEN 130
	LG$ = LEFT$(LG$, 8)
	LOCATE 13, 12: PRINT "ANY CHANGE (YN) "

22  I$=GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 130

25  CLS
	COLOR 15, 1
	LOCATE 5, 1: QPrint "Name for Division 1 ", 30, -1
	LOCATE , 21: QPrint FnExt$(D1$, 10), FG, BG, -1
	LOCATE 7, 1: QPrint "Name for Division 2 ", 30, -1
	LOCATE , 21: QPrint FnExt$(D2$, 10), FG, BG, -1
	LOCATE 9, 1: QPrint "Name for Division 3 ", 30, -1
	LOCATE , 21: QPrint FnExt$(D3$, 10), FG, BG, -1
	LOCATE 11, 1: QPrint "Name for Division 4 ", 30, -1
	LOCATE , 21: QPrint FnExt$(D4$, 10), FG, BG, -1

GetDiv1:
	LOCATE 5, 21                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 10     'specify field parameters
	CALL TextIn(D1$, Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO GetDiv1:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done               'all done entering data

GetDiv2:
	LOCATE 7, 21                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 10     'specify field parameters
	CALL TextIn(D2$, Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO GetDiv1:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done               'all done entering data

GetDiv3:
	LOCATE 9, 21                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 10     'specify field parameters
	CALL TextIn(D3$, Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO GetDiv2:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done               'all done entering data

GetDiv4:
	LOCATE 11, 21                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 10     'specify field parameters
	CALL TextIn(D4$, Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO GetDiv3:           'can't back up any more, stay here

Done:
	LOCATE 23, 1
	PRINT "Is this okay? (Yes/No) ";
	X$ = ""
	CALL YesNoB(X$, XCode, Colr)
	LOCATE , 1
	ClearEOL 30
	IF XCode = 1 GOTO GetDiv4
	IF X$ = "N" GOTO GetDiv1
	IF X$ <> "Y" GOTO Done               'insist on either Y or N or up-arrow

220 COLOR 14, 0
	CLS
	PRINT
	PRINT "  "; LG$; " LEAGUE ": PRINT : COLOR 12
	LOCATE , 4: PRINT D1$; TAB(24); D2$; TAB(44); D3$; TAB(64); D4$

230 FOR I = 1 TO 10
	COLOR 2: PRINT USING "## "; I; : COLOR 15: PRINT Z1$(I);
	LOCATE , 21: COLOR 2: PRINT USING "## "; I + 10; : COLOR 15: PRINT Z1$(I + 10);
	LOCATE , 41: COLOR 2: PRINT USING "## "; I + 20; : COLOR 15: PRINT Z1$(I + 20);
	LOCATE , 61: COLOR 2: PRINT USING "## "; I + 30; : COLOR 15: PRINT Z1$(I + 30)
	NEXT I

240 LOCATE 19, 1
	COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "ADD A TEAM"
	COLOR 14: PRINT "(2) "; : COLOR 15: PRINT "REMOVE A TEAM"
	COLOR 14: PRINT "(3) "; : COLOR 15: PRINT "END AND SAVE SETTINGS"
250 I$ = GetKeyPress$

260 IF I$ = "3" THEN 950

270 LOCATE 20, 30: PRINT "SLOT # (1-40) ";

280 N = 1: INPUT B

290 IF B < 1 OR B > 40 THEN 270

300 IF I$ = "2" THEN Z1$(B) = "XXX": GOTO 220

310 GOSUB 1440

390 Z1$(B) = T1$: Z2$(B) = YN$: Z1%(B) = D

400 GOTO 220

950 CLS

970  OPEN D$(1) + LG$ + ".LIB" FOR OUTPUT AS #2
	PRINT #2, LG$: PRINT #2, D1$: PRINT #2, D2$: PRINT #2, D3$: PRINT #2, D4$
	FOR X = 1 TO 40
	PRINT #2, Z1$(X): PRINT #2, Z1%(X): PRINT #2, Z2$(X)
	NEXT
CLOSE 2
	FOR X = 1 TO 40

980 IF Z1$(X) = "XXX" THEN Z1$(X) = CHR$(42): GOTO 1040

990 FOR D = 4 TO 11
	IF MID$(Z1$(X), D, 1) = CHR$(32) OR MID$(Z1$(X), D, 1) = "." THEN MID$(Z1$(X), D, 1) = "_"

1000 NEXT D
	QQ = 8
	FOR D = 11 TO 4 STEP -1
	IF MID$(Z1$(X), D, 1) = "_" THEN QQ = D - 4: GOTO 1020

1010 GOTO 1030

1020 NEXT D

1030 L = LEN(Z1$(X))
	YR$ = LEFT$(Z1$(X), 2)
	TM$ = MID$(Z1$(X), 4, QQ)
	Z1$(X) = TM$ + "." + YR$

1040 NEXT X

1050 Q$ = LG$ + ".TMS"

1060 OPEN D$(1) + Q$ FOR OUTPUT AS #2

1070 WRITE #2, LG$: WRITE #2, D1$: WRITE #2, D2$: WRITE #2, D3$: WRITE #2, D4$

1080 FOR X = 1 TO 40
	WRITE #2, Z1$(X)
	NEXT

1090 CLOSE 2

1195 CLS
	RUN "HELLO.EXE"
	END

1100 NT = 0
OPEN D$(0) + "PROBBTMS." + YN$ FOR RANDOM AS #1 LEN = 1031
	Y = LOF(1)
	FIELD #1, 15 AS Q$(0), 15 AS Q$(1)
	FOR I = 0 TO 6
	FIELD #1, 30 + I * 2 AS X$, 2 AS Q$(I + 2)
	NEXT
	FIELD #1, 44 AS X$, 2 AS Q$(9)
	FOR I = 0 TO 34
	FIELD #1, 46 + I * 2 AS X$, 2 AS Q$(I + 10)
	NEXT
	FOR I = 0 TO 14
	FIELD #1, 116 + I * 57 AS X$, 15 AS Q$(I * 22 + 45), 2 AS Q$(I * 22 + 46)
	FOR I1 = 0 TO 19
	FIELD #1, 133 + I * 57 + I1 * 2 AS X$, 2 AS Q$(I * 22 + 47 + I1)
	NEXT
	NEXT
	FIELD #1, 971 AS X$, 5 AS Q$(375), 15 AS Q$(376), 40 AS Q$(377)
	FOR I1 = 1 TO Y / 1031
	GET #1, I1
	CK% = CVI(Q$(9))
	IF CK% <> 999 THEN 1103
	NT = NT + 1
	VV$(NT) = Q$(0): V%(NT) = I1

1103 NEXT
CLOSE 1

1102 SELECTION% = 1
	Count% = NT
	Start% = 1
	FileFGColr% = 15:FileBGColr% = 4
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Rows% = 20
	Columns% = 4
	Spacing% = 6
	Row% = 4
	Column% = 1
	COLOR , 4: CLS : LOCATE , 33: COLOR 14
	PRINT "TEAM SELECTION": COLOR 15
	LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15
	PRINT "-SELECT NEW ID  PGDN  PGUP  "; : COLOR 14
	PRINT "ENTER"; : COLOR 15: PRINT "-SELECTS TEAM";
	
_MouseShow "DEFAULT"
20   MAMenu VARPTR(VV$(1)), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF ExitCode% = 27 THEN BO% = 1: RETURN
	IF ExitCode% = -71 THEN Start% = 1: ExitCode% = 3: SELECTION% = 1: GOTO 20
	IF ExitCode% = -79 THEN Start% = NT: ExitCode% = 3: SELECTION% = NT: GOTO 20
	IF ExitCode% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
	IF ExitCode% = -81 THEN Start% = Start% + 80: IF Start% > NT THEN Start% = NT
	IF ExitCode% = -81 OR ExitCode% = -73 THEN ExitCode% = 3: SELECTION% = Start%: GOTO 20
	D = V%(SELECTION%)
	LOCATE 2, 33: COLOR 31: PRINT "LOADING..."

1300 OPEN D$(0) + "PROBBTMS." + YN$ FOR RANDOM AS #1 LEN = 1031
	FIELD #1, 15 AS Q$(0), 15 AS Q$(1)
	FOR I = 0 TO 6
	FIELD #1, 30 + I * 2 AS X$, 2 AS Q$(I + 2)
	NEXT
	FIELD #1, 44 AS X$, 2 AS Q$(9)
	FOR I = 0 TO 34
	FIELD #1, 46 + I * 2 AS X$, 2 AS Q$(I + 10)
	NEXT
	FOR I = 0 TO 14
	FIELD #1, 116 + I * 57 AS X$, 15 AS Q$(I * 22 + 45), 2 AS Q$(I * 22 + 46)
	FOR I1 = 0 TO 19
	FIELD #1, 133 + I * 57 + I1 * 2 AS X$, 2 AS Q$(I * 22 + 47 + I1)
	NEXT
	NEXT
	FIELD #1, 971 AS X$, 5 AS Q$(375), 15 AS Q$(376), 40 AS Q$(377)
GET #1, D
	T1$(0) = Q$(0)
	FOR I = 0 TO 34: T%(I) = CVI(Q$(I + 10)): NEXT
CLOSE 1
	T1$ = RTrim$(T1$(0))
	RETURN

1440 COLOR 15, 0: CLS
	JJ = 7
	LOCATE 6, 8: COLOR 15, 4
	PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
	PRINT " º"; STRING$(61, 32); "º "; : NEXT
	LOCATE 7 + JJ, 8
	PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
	LOCATE , 12: PRINT "ENTER DISK ID: ": PRINT
	LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
	LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
	LOCATE , 12: PRINT "HIT <ENTER> TO USE PREVIOUS ID"
	LOCATE 7, 27: INPUT NY$
	IF NY$ = "" THEN GOSUB 1102: GOTO 182
	YN$ = NY$
	FOR I = 1 TO 200: VV$(I) = "": NEXT

1441 GOSUB 1100

182 IF BO% = 1 THEN BO% = 0: GOTO 1440

183 LOCATE 2, 33: COLOR 15, 0: COLOR T%(33), T%(34): PRINT T1$: COLOR 15, 4
	LOCATE , 33: PRINT "ANY CHANGE (YN)";
	I$=GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 1440
	RETURN

2060 CLS : PRINT "ERROR:: UNABLE TO READ TEAM FILE": PRINT

2720 BO% = 1: RETURN

4000 Spec$ = D$(1) + "*.LIB"
	null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
	Count% = DirCount%
	IF Count% = 0 THEN 4500
	REDIM F$(0 TO Count%)
	FOR X = 1 TO Count%
	  F$(X) = SPACE$(12)
	NEXT
	F$(0) = Spec$
	CALL ReadFile(F$())
	'CALL SortStr(VARPTR(F$(1)), Count, 0)

1310 CLS
	COLOR , 1
	CLS
	LOCATE , 28
	COLOR 14
	PRINT "SELECT LEAGUE TO EDIT  ESC-TO EXIT": COLOR 15
	SELECTION% = 1
	Start% = 1
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Rows% = 20
	Columns% = 4
	Spacing% = 6
	Row% = 4
	Column% = 1
	LOCATE 25, 1
	COLOR 14
	PRINT "ENTER"; : COLOR 15
	PRINT "-SELECTS TEAM";

_MouseShow "DEFAULT"
MAMenu F$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF ExitCode% = 27 THEN 50
	F$ = F$(SELECTION%)
	LOCATE 2, 33: PRINT F$
LOCATE , 33: PRINT "ANY CHANGE (YN)";
	COLOR 15, 0
3910 I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 1310

3911 F$ = RTrim$(F$)
	LG$ = LEFT$(F$, LEN(F$) - 4)
OPEN D$(1) + LG$ + ".LIB" FOR INPUT AS #2
	INPUT #2, LG$: INPUT #2, D1$: INPUT #2, D2$: INPUT #2, D3$: INPUT #2, D4$
	FOR X = 1 TO 40
	INPUT #2, Z1$(X): INPUT #2, Z1%(X): INPUT #2, Z2$(X)
	NEXT
CLOSE 2
	COLOR 15, 1
	GOTO 25

4500 LOCATE 10, 8
	CALL CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Cnf)
	GOTO 50


FUNCTION FnExt$ (X$, Length) 
'Essentially this pads space to the right of the string
	X$ = X$ + SPACE$(Length - LEN(X$))  'for display only
END FUNCTION


SUB TITLE ()
	COLOR , 0: KEY OFF: CLS
	COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 30: COLOR 12
	PRINT "HAT TRICK HOCKEY V4.0"
	LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
	PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; TAB(70); LEFT$(TIME$, 5);
	LOCATE 3, 1: COLOR 2, 0
	COLOR 15: PRINT "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
	  PRINT "              FOR TECHNICAL SUPPORT, CALL (615) 366-8088"
	COLOR 10
END SUB

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
