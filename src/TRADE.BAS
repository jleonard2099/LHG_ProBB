'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DECLARE SUB TITLE ()

DIM B$(2, 14, 1), B%(2, 14, 19), T(1), B1(2, 14, 14), B1$(2, 14), W0(2, 14), W1(2, 14)
DIM Z(2, 15), Z1(2, 15), T%(1, 34), Q$(377), V$(500), V%(500)
DIM D$(1), item$(3), NY$(1), YN$(1), S(1), L%(1, 6), SA$(1), F$(1)
DIM A1$(1), A$(1, 6)

COLOR 3, 0

OPEN "DEV.BAS" FOR INPUT AS #1: INPUT #1, D$(0): INPUT #1, D$(1): CLOSE 1

99 CALL TITLE
	item$(1) = "TRADE PLAYERS           "
	item$(2) = "RETURN TO PRO GAME MENU "
	SELECTION% = 1
	Count% = 2
	Start% = 1
	SCANCODE% = 2
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 25

CALL box0(7, 24, 10, 49, 2, 31)
	
_MouseShow "DEFAULT"
MMenuVert item$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 OR SELECTION% = 2 THEN 1
	IF SELECTION% = 1 THEN 2111

1   RUN "HELLO.EXE 2"

2111 J = 0
	COLOR 15, 0: CLS
	JJ = 7
	LOCATE 6, 8: COLOR 15, 4
	PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
	PRINT " º"; STRING$(61, 32); "º "; : NEXT
	LOCATE 7 + JJ, 8
	PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
	LOCATE , 12: PRINT "ENTER DISK ID FOR TEAM A: ": PRINT
	LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
	LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
	LOCATE , 12: PRINT
	LOCATE , 12: PRINT "HIT <ENTER> TO USE PREVIOUS ID"
	LOCATE 7, 38: INPUT NY$(J)
	IF NY$(J) = "" THEN GOSUB 1102: GOTO 2112
	YN$(J) = NY$(J)
	FOR I = 1 TO 500: V$(I) = "": NEXT
	GOSUB 1100

2112 IF BO% = 1 THEN BO% = 0: GOTO 2111

2113 LOCATE 2, 33: COLOR T%(J, 33), T%(J, 34): PRINT A$(J, 0): COLOR 15, 4
	LOCATE , 33: PRINT "ANY CHANGE (YN)";
	I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 2111
	COLOR 15, 0
	S(0) = 0
	JJ = 1: GOSUB 9150
	U$ = "DOES THIS TEAM HAVE SAVED STATS (YN)"
	LOCATE , (80 - LEN(U$)) / 2
	PRINT U$
	COLOR 15, 0
	I$ = GetKeyPress$
	IF UCASE$(I$) = "N" THEN S(0) = 1: GOTO 2150
	IF UCASE$(LEFT$(D$(1), 1)) <> "A" AND UCASE$(LEFT$(D$(1), 1)) <> "B" THEN 2145
	CLS

2145 GOSUB 100

2150 J = 1
	COLOR 15, 0: CLS
	JJ = 7
	LOCATE 6, 8: COLOR 15, 4
	PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
	PRINT " º"; STRING$(61, 32); "º "; : NEXT
	LOCATE 7 + JJ, 8
	PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
	LOCATE , 12: PRINT "ENTER DISK ID FOR TEAM B: ": PRINT
	LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
	LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
	LOCATE , 12: PRINT
	LOCATE , 12: PRINT "HIT <ENTER> TO USE PREVIOUS ID"
	LOCATE 7, 38: INPUT NY$(J)
	IF NY$(J) = "" THEN GOSUB 1102: GOTO 2152
	YN$(J) = NY$(J)
	FOR I = 1 TO 500: V$(I) = "": NEXT
	GOSUB 1100

2152 IF BO% = 1 THEN BO% = 0: GOTO 2150

2153 LOCATE 2, 33: COLOR T%(J, 33), T%(J, 34): PRINT A$(J, 0): COLOR 15, 4
	LOCATE , 33: PRINT "ANY CHANGE (YN)";
	I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 2150
	COLOR 15, 0
	S(1) = 0
	JJ = 1: GOSUB 9150
	U$ = "DOES THIS TEAM HAVE SAVED STATS (YN)"
	LOCATE , (80 - LEN(U$)) / 2
	PRINT U$
	COLOR 15, 0
	I$ = GetKeyPress$
	IF UCASE$(I$) = "N" THEN S(1) = 1: GOTO 200
	IF UCASE$(LEFT$(D$(1), 1)) <> "A" AND UCASE$(LEFT$(D$(1), 1)) <> "B" THEN 2195
	CLS

2195 GOSUB 100

200 CLS
	COLOR 15: PRINT "T"; : COLOR 3: PRINT "RADE PLAYERS": PRINT
	COLOR 15: PRINT "N"; : COLOR 3: PRINT "O MORE TRADES"
201 I$ = GetKeyPress$
	IF UCASE$(I$) = "N" THEN 500

210 IF S(0) = 1 OR S(1) = 1 THEN 216
	CLS : PRINT : PRINT
	COLOR 14: PRINT "(T)"; : COLOR 15: PRINT " TRADE PLAYERS' COMPILED STATS"
	COLOR 14: PRINT : PRINT "(Z)"; : COLOR 15: PRINT " ZERO PLAYERS' COMPILED STATS"
300 I$ = GetKeyPress$
	IF UCASE$(I$) <> "T" AND UCASE$(I$) <> "Z" THEN 216
	IF UCASE$(I$) = "T" THEN W1 = 1
	IF UCASE$(I$) = "Z" THEN W1 = 0

216 CLS : LOCATE 3, 1: COLOR T%(0, 33), T%(0, 34): PRINT A$(0, 0); : LOCATE , 20: COLOR T%(1, 33), T%(1, 34): PRINT A$(1, 0): COLOR 3, 0
	FOR I = 0 TO 14: LOCATE I + 4, 1: PRINT I; " "; B$(0, I, 0); " "; B$(0, I, 1); : LOCATE , 20: PRINT I; " "; B$(1, I, 0); " "; B$(1, I, 1): NEXT I

218 LOCATE 19, 1: PRINT A$(0, 0); " PLAYER: ";
	INPUT I
	IF I < 0 OR I > 14 THEN 218
	COLOR T%(0, 33), T%(0, 34): PRINT B$(0, I, 0): COLOR 3, 0

223 LOCATE 21, 1: PRINT A$(1, 0); " PLAYER: ";
	INPUT I1
	IF I1 < 0 OR I1 > 14 THEN 223
	COLOR T%(1, 33), T%(1, 34): PRINT B$(1, I1, 0): COLOR 3, 0
	LOCATE 23, 1: PRINT "ANY CHANGE (YN) ";
228 I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 218
	GOSUB 600
	FOR I2 = 0 TO 1: SWAP B$(0, I, I2), B$(1, I1, I2): NEXT
	FOR I2 = 0 TO 19: SWAP B%(0, I, I2), B%(1, I1, I2): NEXT I2
	SWAP B1$(0, I), B1$(1, I1)
	IF S(0) = 1 OR S(1) = 1 OR W1 = 0 THEN 240
	FOR I2 = 0 TO 13: SWAP B1(0, I, I2), B1(1, I1, I2): NEXT I2
	SWAP W0(0, I), W0(1, I1)
	SWAP W1(0, I), W1(1, I1)
	GOSUB 610: GOTO 200

240 FOR I2 = 0 TO 13: B1(0, I, I2) = 0: B1(1, I1, I2) = 0: NEXT
	W0(1, I1) = 0: W1(1, I1) = 0: W0(0, I) = 0: W1(0, I) = 0
	GOSUB 610: GOTO 200

500 FOR J = 0 TO 1
	IF UCASE$(LEFT$(D$(0), 1)) <> "A" AND UCASE$(LEFT$(D$(0), 1)) <> "B" THEN 556
	CLS

556 GOSUB 9800
	NEXT J
	 FOR J = 0 TO 1
	IF S(J) = 1 THEN 525
	CLS

557 OPEN D$(1) + F$(J) FOR OUTPUT AS #1
	PRINT #1, A1$(J): FOR I = 0 TO 14: PRINT #1, B1$(J, I): FOR I1 = 0 TO 13: PRINT #1, B1(J, I, I1): NEXT I1: PRINT #1, W0(J, I): PRINT #1, W1(J, I): NEXT I
	FOR L = 0 TO 15: PRINT #1, Z(J, L): NEXT L
	FOR L = 0 TO 15: PRINT #1, Z1(J, L): NEXT L
CLOSE 1

525 NEXT J
	GOTO 99

600 GOSUB 700
	LOCATE 23, 1: PRINT "HIT ANY KEY TO TRADE";
602 I$ = GetKeyPress$
	RETURN

610 GOSUB 700
	LOCATE 23, 1: PRINT "HIT ANY KEY TO CONTINUE";
612 I$ = GetKeyPress$
	RETURN
	LOCATE 23, 1: PRINT "HIT ANY KEY TO TRADE";
622 I$ = GetKeyPress$
	RETURN
	LOCATE 23, 1: PRINT "HIT ANY KEY TO CONTINUE";
632 I$ = GetKeyPress$
	RETURN

700 CLS : LOCATE 3, 1: COLOR T%(0, 33), T%(0, 34): LOCATE , 10: PRINT A$(0, 0); : LOCATE , 50: COLOR T%(1, 33), T%(1, 34): PRINT A$(1, 0): COLOR 3, 0: PRINT
	LOCATE , 10: PRINT B$(0, I, 0); " "; B$(0, I, 1); TAB(50); B$(1, I1, 0); " "; B$(1, I1, 1)
	LOCATE , 5: PRINT USING "  ##   GAMES     ##"; B%(0, I, 0); B1(0, I, 0)
	LOCATE , 5: PRINT USING "####  MINUTES  ####"; B%(0, I, 1); B1(0, I, 10) / 60
	LOCATE , 5: PRINT USING "####    FGA    ####"; B%(0, I, 2); B1(0, I, 1)
	LOCATE , 5: PRINT USING "####    FGM    ####"; B%(0, I, 3); B1(0, I, 2)
	LOCATE , 5: PRINT USING "####    FTA    ####"; B%(0, I, 4); B1(0, I, 3)
	LOCATE , 5: PRINT USING "####    FTM    ####"; B%(0, I, 5); B1(0, I, 4)
	LOCATE , 5: PRINT USING "####  OFF REB  ####"; B%(0, I, 6); B1(0, I, 12)
	LOCATE , 5: PRINT USING "####  TOT REB  ####"; B%(0, I, 6) + B%(0, I, 7); B1(0, I, 5)
	LOCATE , 5: PRINT USING "####   FOULS   ####"; B%(0, I, 9); B1(0, I, 6)
	LOCATE , 5: PRINT USING "####  STEALS   ####"; B%(0, I, 11); B1(0, I, 7)
	LOCATE , 5: PRINT USING "####  ASSISTS  ####"; B%(0, I, 8); B1(0, I, 8)
	LOCATE , 5: PRINT USING "####  BLOCKS   ####"; B%(0, I, 12); B1(0, I, 9)
	LOCATE , 5: PRINT USING "     TURNOVERS ####"; B1(0, I, 11)
	LOCATE , 5: PRINT USING "####   3FGA    ####"; B%(0, I, 13); W1(0, I)
	LOCATE , 5: PRINT USING "####   3FGM    ####"; B%(0, I, 14); W0(0, I)
	LOCATE , 5: PRINT USING "  ##  HI GAME    ##"; B%(0, I, 15); B1(0, I, 13)
	LOCATE 6, 45: PRINT USING "  ##   GAMES     ##"; B%(1, I1, 0); B1(1, I1, 0)
	LOCATE , 45: PRINT USING "####  MINUTES  ####"; B%(1, I1, 1); B1(1, I1, 10) / 60
	LOCATE , 45: PRINT USING "####    FGA    ####"; B%(1, I1, 2); B1(1, I1, 1)
	LOCATE , 45: PRINT USING "####    FGM    ####"; B%(1, I1, 3); B1(1, I1, 2)
	LOCATE , 45: PRINT USING "####    FTA    ####"; B%(1, I1, 4); B1(1, I1, 3)
	LOCATE , 45: PRINT USING "####    FTM    ####"; B%(1, I1, 5); B1(1, I1, 4)
	LOCATE , 45: PRINT USING "####  OFF REB  ####"; B%(1, I1, 6); B1(1, I1, 12)
	LOCATE , 45: PRINT USING "####  TOT REB  ####"; B%(1, I1, 6) + B%(1, I1, 7); B1(1, I1, 5)
	LOCATE , 45: PRINT USING "####   FOULS   ####"; B%(1, I1, 9); B1(1, I1, 6)
	LOCATE , 45: PRINT USING "####  STEALS   ####"; B%(1, I1, 11); B1(1, I1, 7)
	LOCATE , 45: PRINT USING "####  ASSISTS  ####"; B%(1, I1, 8); B1(1, I1, 8)
	LOCATE , 45: PRINT USING "####  BLOCKS   ####"; B%(1, I1, 12); B1(1, I1, 9)
	LOCATE , 45: PRINT USING "     TURNOVERS ####"; B1(1, I1, 11)
	LOCATE , 45: PRINT USING "####   3FGA    ####"; B%(1, I1, 13); W1(1, I1)
	LOCATE , 45: PRINT USING "####   3FGM    ####"; B%(1, I1, 14); W0(1, I1)
	LOCATE , 45: PRINT USING "  ##  HI GAME    ##"; B%(1, I1, 15); B1(1, I1, 13)
	RETURN

1100 NT = 0
OPEN D$(0) + "PROBBTMS." + YN$(J) FOR RANDOM AS #1 LEN = 1031
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
	V$(NT) = Q$(0): V%(NT) = I1

1103 NEXT
CLOSE 1
	TS% = INT((NT - 1) / 80)

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
20   MAMenu V$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF EXITCODE% = 27 THEN BO% = 1: RETURN
	IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 20
	IF EXITCODE% = -79 THEN Start% = NT: EXITCODE% = 3: SELECTION% = NT: GOTO 20
	IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
	IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > NT THEN Start% = NT
	IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 20
	T(J) = V%(SELECTION%)
OPEN D$(0) + "PROBBTMS." + YN$(J) FOR RANDOM AS #1 LEN = 1031
	FIELD #1, 15 AS Q$(0), 15 AS Q$(1)
	FOR J% = 0 TO 6
	FIELD #1, 30 + J% * 2 AS X$, 2 AS Q$(J% + 2)
	NEXT
	FIELD #1, 44 AS X$, 2 AS Q$(9)
	FOR J% = 0 TO 34
	FIELD #1, 46 + J% * 2 AS X$, 2 AS Q$(J% + 10)
	NEXT
	FOR J% = 0 TO 14
	FIELD #1, 116 + J% * 57 AS X$, 15 AS Q$(J% * 22 + 45), 2 AS Q$(J% * 22 + 46)
	FOR J1 = 0 TO 19
	FIELD #1, 133 + J% * 57 + J1 * 2 AS X$, 2 AS Q$(J% * 22 + 47 + J1)
	NEXT
	NEXT
	FIELD #1, 971 AS X$, 5 AS Q$(375), 15 AS Q$(376), 40 AS Q$(377)
	GET #1, T(J)
	A$(J, 0) = Q$(0): A$(J, 1) = Q$(1)
	FOR I = 0 TO 6: L%(J, I) = CVI(Q$(I + 2)): NEXT

9002 IF RIGHT$(A$(J, 0), 1) = " " THEN A$(J, 0) = LEFT$(A$(J, 0), LEN(A$(J, 0)) - 1): GOTO 9002
	FOR I = 0 TO 34: T%(J, I) = CVI(Q$(I + 10)): NEXT
	FOR I = 0 TO 14
	B$(J, I, 0) = Q$(22 * I + 45)

9004 IF RIGHT$(B$(J, I, 0), 1) = " " THEN B$(J, I, 0) = LEFT$(B$(J, I, 0), LEN(B$(J, I, 0)) - 1): GOTO 9004
	B$(J, I, 1) = Q$(22 * I + 46)

9005 IF RIGHT$(B$(J, I, 1), 1) = " " THEN B$(J, I, 1) = LEFT$(B$(J, I, 1), LEN(B$(J, I, 1)) - 1): GOTO 9005
	FOR I1 = 0 TO 19
	B%(J, I, I1) = CVI(Q$(22 * I + 47 + I1))
	NEXT: NEXT
	A$(J, 2) = Q$(375): A$(J, 3) = Q$(376): A$(J, 4) = Q$(377)
CLOSE 1
	SA$(J) = A$(J, 0)

9006 IF RIGHT$(A$(J, 2), 1) = " " THEN A$(J, 2) = LEFT$(A$(J, 2), LEN(A$(J, 2)) - 1): GOTO 9006

9007 IF RIGHT$(A$(J, 3), 1) = " " THEN A$(J, 3) = LEFT$(A$(J, 3), LEN(A$(J, 3)) - 1): GOTO 9007

9008 IF RIGHT$(A$(J, 4), 1) = " " THEN A$(J, 4) = LEFT$(A$(J, 4), LEN(A$(J, 4)) - 1): GOTO 9008
	RETURN
	

4091 LOCATE 22, 1: PRINT "ANY KEY TO CONTINUE"
4092 I$ = GetKeyPress$
	RETURN

9800 OPEN D$(0) + "PROBBTMS." + YN$(J) FOR RANDOM AS #1 LEN = 1031
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
LSET Q$(0) = A$(J, 0): LSET Q$(1) = A$(J, 1): FOR I = 0 TO 6: LSET Q$(I + 2) = MKI$(L%(J, I)): NEXT
LSET Q$(9) = MKI$(999)
FOR I = 0 TO 34: LSET Q$(I + 10) = MKI$(T%(J, I)): NEXT
FOR I = 0 TO 14
LSET Q$(22 * I + 45) = B$(J, I, 0)
LSET Q$(22 * I + 46) = B$(J, I, 1)
FOR I1 = 0 TO 19
LSET Q$(22 * I + 47 + I1) = MKI$(B%(J, I, I1))
NEXT: NEXT
LSET Q$(375) = A$(J, 2): LSET Q$(376) = A$(J, 3): LSET Q$(377) = A$(J, 4)
PUT #1, T(J): CLOSE 1
	RETURN

9150 LOCATE 6, 8: COLOR T%(J, 33), T%(J, 34)
	PRINT " "; CHR$(201); STRING$(61, 205); CHR$(187); " ";
	FOR II = 1 TO JJ: LOCATE 6 + II, 8
	PRINT " º"; STRING$(61, 32); "º "; : NEXT
	LOCATE 7 + JJ, 8
	PRINT " "; CHR$(200); STRING$(61, 205); CHR$(188); " "; : LOCATE 7
	RETURN

100 FOR XX = 4 TO 11: IF MID$(SA$(J), XX, 1) = " " OR MID$(SA$(J), XX, 1) = "/" OR MID$(SA$(J), XX, 1) = "." THEN MID$(SA$(J), XX, 1) = "_"
	NEXT XX
	FOR XX = 12 TO 14: IF MID$(SA$(J), XX, 2) = "ST" THEN MID$(SA$(J), 9, 1) = "_": MID$(SA$(J), 10, 2) = "ST"
	NEXT XX
	QQ = 8: FOR XX = 11 TO 4 STEP -1: IF MID$(SA$(J), XX, 1) = "_" THEN QQ = XX - 4: GOTO 106
	GOTO 107

106 NEXT XX

107 YR$ = LEFT$(SA$(J), 2)
	F$(J) = MID$(SA$(J), 4, QQ) + "." + YR$
	IF NOT _FILEEXISTS(D$(1) + F$(J)) THEN S(J) = 1: GOTO 108
OPEN D$(1) + F$(J) FOR INPUT AS #1
	INPUT #1, A1$(J)
	FOR I = 0 TO 14: INPUT #1, B1$(J, I)
	FOR I1 = 0 TO 13: INPUT #1, B1(J, I, I1): NEXT I1
	INPUT #1, W0(J, I): INPUT #1, W1(J, I): NEXT I
	FOR L = 0 TO 15: INPUT #1, Z(J, L): NEXT L
	FOR L = 0 TO 15: INPUT #1, Z1(J, L): NEXT L
CLOSE 1

108 RETURN

SUB TITLE ()
	COLOR , 0: KEY OFF: CLS
	COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 25: COLOR 12
	PRINT "BASKETBALL: THE PRO GAME"
	LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
	PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; TAB(70); LEFT$(TIME$, 5);
	LOCATE 3, 1: COLOR 2, 0
	COLOR 15: PRINT "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
	  PRINT "              FOR TECHNICAL SUPPORT, CALL (615) 366-8088"
	COLOR 10
END SUB

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
