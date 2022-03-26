'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

10 DIM T$(4), B%(15, 19), B$(15, 1), T%(35), L%(6), Q$(377), v$(500), v%(500)
DIM P$(3), D$(1), ITEM$(5)

20 P$(0) = "G": P$(1) = "F": P$(2) = "C"

50 OPEN "DEV.BAS" FOR INPUT AS #1
	INPUT #1, D$(0): CLOSE #1

60 COLOR 1, 0: CLS : COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 22: COLOR 14
	PRINT "BASKETBALL:THE PRO GAME ADJUST PLAYING TIME"
	LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
	PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1991"; TAB(70); LEFT$(TIME$, 5);
	LOCATE 4, 1: COLOR 1, 0
	item$(1) = "ADJUST GAMES AND MINUTES PER GAME "
	item$(2) = "EXIT TO MAIN MENU                 "
	  SELECTION% = 1
	Count% = 2
	Start% = 1
	SCANCODE% = 2
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 25

CALL box0(7, 24, 10, 59, 2, 31)
	
_MouseShow "DEFAULT"
MMenuVert item$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	I = SELECTION%
	IF SCANCODE% = 27 THEN I = 2
	IF I = 1 THEN 181
	
5    RUN "HELLO.EXE"

100 COLOR , 0: CLS : COLOR T%(33), T%(34): PRINT T$(0): COLOR 15, 0
	PRINT "   PLAYERS                 ACTUAL           ADJUSTED"
	PRINT "                          GP   MPG          GP    MPG"
	GP = 0: MIN = 0
	FOR I = 0 TO 14
	IF B%(I, 16) >= 0 OR B%(I, 17) >= 0 THEN B%(I, 16) = B%(I, 0) * -1: B%(I, 17) = B%(I, 1) * -1
	GP = GP + B%(I, 0)
	MIN = MIN + B%(I, 17) * -1
	IF LEFT$(B$(I, 0), 3) = "XXX" THEN 105
	PRINT USING "## "; I; : PRINT B$(I, 0); " "; B$(I, 1); TAB(27);
	PRINT USING "##   ##.#         ##   ##.#"; B%(I, 0); B%(I, 1) / B%(I, 0); B%(I, 16) * -1; B%(I, 17) / B%(I, 16)

105 NEXT
	PRINT "15 END AND SAVE CHANGES"
	PRINT
	PRINT "TEAM MIN/G SHOULD BE APPROXIMATELY 240, OVERTIME WILL INCREASE THIS FIGURE"
	PRINT USING "TEAM MIN/G = ###.#"; MIN / T%(0)

560 LOCATE 23, 1

561 INPUT "ENTER SELECTION "; B1

562 IF B1 < 0 OR B1 > 15 THEN 560
	IF B1 = 15 THEN 1380
	GOSUB 2800
	PRINT B$(B1, 0); " "; : INPUT "GAMES PLAYED "; GP
	GOSUB 2800
	PRINT B$(B1, 0); " "; : INPUT "MINUTES PER GAME "; MPG
	B%(B1, 16) = GP * -1
	B%(B1, 17) = MPG * GP * -1
	GOTO 100

1380 OPEN D$(0) + "PROBBTMS." + YN$ FOR RANDOM AS #1 LEN = 1031
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
LSET Q$(0) = T$(0): LSET Q$(1) = T$(1): FOR I = 0 TO 6: LSET Q$(I + 2) = MKI$(L%(I)): NEXT
LSET Q$(9) = MKI$(999)
FOR I = 0 TO 34: LSET Q$(I + 10) = MKI$(T%(I)): NEXT
FOR I = 0 TO 14
LSET Q$(22 * I + 45) = B$(I, 0)
LSET Q$(22 * I + 46) = B$(I, 1)
FOR I1 = 0 TO 19
LSET Q$(22 * I + 47 + I1) = MKI$(B%(I, I1))
NEXT: NEXT
LSET Q$(375) = T$(2): LSET Q$(376) = T$(3): LSET Q$(377) = T$(4)
PUT #1, T1: CLOSE 1

1430 GOTO 60

181 COLOR 15, 0: CLS
	JJ = 6
	SN = 1: LOCATE 6, 8: COLOR 15, 4
	PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
	PRINT " º"; STRING$(61, 32); "º "; : NEXT
	LOCATE 7 + JJ, 8
	PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
	LOCATE , 12: PRINT "ENTER DISK ID: ": PRINT
	LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
	LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
	PRINT
	LOCATE , 12: PRINT "ESC TO RETURN TO MAIN MENU"
3057 I$ = GetKeyPress$
	IF I$ = CHR$(27) THEN 60
	IF I$ = CHR$(13) THEN GOSUB 1102: GOTO 3060
	LOCATE 7, 27: PRINT I$
	LOCATE 7, 28: LINE INPUT NY$
	NY$ = I$ + NY$
	COLOR 15, 0
	IF NY$ = "" THEN GOSUB 1102: GOTO 3060
	YN$ = NY$
	FOR I = 1 TO 500: v$(I) = "": NEXT
	GOSUB 1100

3060 IF BO% = 1 THEN BO% = 0: GOTO 181

3058 LOCATE 2, 33: COLOR T%(33), T%(34): PRINT T$(0)
	COLOR 15, 4: LOCATE , 33: PRINT "ANY CHANGE (YN)";
	I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 181
	COLOR 15, 0
	GOTO 100

1100 NTMS = 0
OPEN D$(0) + "PROBBTMS." + YN$ FOR RANDOM AS #1 LEN = 1031
	Y1 = LOF(1)
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
	FOR I1 = 1 TO Y1 / 1031
	GET #1, I1
	CK% = CVI(Q$(9))
	IF CK% <> 999 THEN 1103
	NTMS = NTMS + 1
	v$(NTMS) = Q$(0): v%(NTMS) = I1

1103 NEXT
CLOSE 1

1102 SELECTION% = 1
	Count% = NTMS
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
120   MAMenu v$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF EXITCODE% = 27 THEN BO% = 1: RETURN
	IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 120
	IF EXITCODE% = -79 THEN Start% = NTMS: EXITCODE% = 3: SELECTION% = NTMS: GOTO 120
	IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
	IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > NTMS THEN Start% = NTMS
	IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 120
	T1 = v%(SELECTION%)
OPEN D$(0) + "PROBBTMS." + YN$ FOR RANDOM AS #1 LEN = 1031
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
	GET #1, T1
T$(0) = Q$(0): T$(1) = Q$(1): FOR I = 0 TO 6: L%(I) = CVI(Q$(I + 2)): NEXT
FOR I = 0 TO 34: T%(I) = CVI(Q$(I + 10)): NEXT
FOR I = 0 TO 14
B$(I, 0) = Q$(22 * I + 45)
B$(I, 1) = Q$(22 * I + 46)
FOR I1 = 0 TO 19
B%(I, I1) = CVI(Q$(22 * I + 47 + I1))
NEXT: NEXT
T$(2) = Q$(375): T$(3) = Q$(376): T$(4) = Q$(377)
CLOSE

1486 IF RIGHT$(T$(0), 1) = " " THEN T$(0) = LEFT$(T$(0), LEN(T$(0)) - 1): GOTO 1486

1487 IF RIGHT$(T$(1), 1) = " " THEN T$(1) = LEFT$(T$(1), LEN(T$(1)) - 1): GOTO 1487

1488 IF RIGHT$(T$(2), 1) = " " THEN T$(2) = LEFT$(T$(2), LEN(T$(2)) - 1): GOTO 1488

1489 IF RIGHT$(T$(3), 1) = " " THEN T$(3) = LEFT$(T$(3), LEN(T$(3)) - 1): GOTO 1489

1490 IF RIGHT$(T$(4), 1) = " " THEN T$(4) = LEFT$(T$(4), LEN(T$(4)) - 1): GOTO 1490

1540 RETURN

2800 LOCATE 23, 1: PRINT "                                      "; : LOCATE 23, 1

2805 RETURN

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
