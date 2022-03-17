'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DIM WW(40), LL(40), RWW(40), HWW(40), RLL(40), HLL(40), LTW(40), LTL(40), TML$(40), Z1$(40)
DIM O%(100), O$(100), T%(100), H$(100), LWS(40), LLS(40), TWS(40), TLS(40), ZA$(40)
DIM D$(2), item$(3)

CLS

2300 CALL TITLE
	CLEAR
	TMR = 40
OPEN "DEV.BAS" FOR INPUT AS #2: FOR I = 0 TO 1: INPUT #2, D$(I): NEXT: CLOSE 2
	item$(1) = "VIEW EXPANDED LEAGUE STANDINGS"
	item$(2) = "RETURN TO LEAGUE LEADER MENU  "
	SELECTION% = 1
	Count% = 2
	Start% = 1
	SCANCODE% = 2
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 25

CALL box0(7, 24, 10, 55, 2, 31)
	
_MouseShow "DEFAULT"
MMenuVert VARPTR(item$(1)), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 OR SELECTION% = 2 THEN RUN "NEWLDR.EXE"

2500 Spec$ = D$(1) + "*.TMS": GOSUB 1440
	LG$ = F$
OPEN D$(1) + F$ + ".TMS" FOR INPUT AS #2
	INPUT #2, LG$
	INPUT #2, D1$: INPUT #2, D2$: INPUT #2, D3$: INPUT #2, D4$
	FOR X = 1 TO 40
	INPUT #2, Z1$(X): TML$(X) = Z1$(X)
	NEXT
CLOSE 2
	CLS
	FOR KK = 1 TO 40: YY = YY + 1
COLOR 14: LOCATE 7, 20: TMR = TMR - 1: PRINT USING "##"; TMR
IF Z1$(KK) = "*" THEN 60

10 TML$(YY) = Z1$(KK)
	LOCATE 5: COLOR 15: PRINT "COMPUTING RECORDS . . . . . . PLEASE WAIT"
	IF NOT _FILEEXISTS(D$(1) + Z1$(KK) + "S") THEN 60
OPEN D$(1) + Z1$(KK) + "S" FOR INPUT AS #1
	INPUT #1, XX%
	FOR J = 1 TO 100: INPUT #1, O%(J): NEXT
	FOR J = 1 TO 100: INPUT #1, O$(J): NEXT
	FOR J = 1 TO 100: INPUT #1, T%(J): NEXT
	FOR J = 1 TO 100: INPUT #1, H$(J): NEXT
CLOSE 1
	LWS(YY) = 0: LLS(YY) = 0: TWS(YY) = 0: TLS(YY) = 0: FOR I = 1 TO XX%
	IF O%(I) > T%(I) THEN WW(YY) = WW(YY) + 1
	IF O%(I) > T%(I) AND H$(I) = "V" THEN RWW(YY) = RWW(YY) + 1
	IF O%(I) > T%(I) AND H$(I) = "V1" THEN RWW(YY) = RWW(YY) + 1
	IF O%(I) > T%(I) AND H$(I) = "V2" THEN RWW(YY) = RWW(YY) + 1
	IF O%(I) > T%(I) AND H$(I) = "H" THEN HWW(YY) = HWW(YY) + 1
	IF O%(I) > T%(I) AND H$(I) = "H1" THEN HWW(YY) = HWW(YY) + 1
	IF O%(I) > T%(I) AND H$(I) = "H2" THEN HWW(YY) = HWW(YY) + 1
	IF T%(I) > O%(I) THEN LL(YY) = LL(YY) + 1
	IF T%(I) > O%(I) AND H$(I) = "V" THEN RLL(YY) = RLL(YY) + 1
	IF T%(I) > O%(I) AND H$(I) = "V1" THEN RLL(YY) = RLL(YY) + 1
	IF T%(I) > O%(I) AND H$(I) = "V2" THEN RLL(YY) = RLL(YY) + 1
	IF T%(I) > O%(I) AND H$(I) = "H" THEN HLL(YY) = HLL(YY) + 1
	IF T%(I) > O%(I) AND H$(I) = "H1" THEN HLL(YY) = HLL(YY) + 1
	IF T%(I) > O%(I) AND H$(I) = "H2" THEN HLL(YY) = HLL(YY) + 1
	IF O%(I) > T%(I) THEN LWS(YY) = LWS(YY) + 1: LLS(YY) = 0
	IF LWS(YY) > TWS(YY) THEN TWS(YY) = LWS(YY)
	IF O%(I) < T%(I) THEN LLS(YY) = LLS(YY) + 1: LWS(YY) = 0
	IF LLS(YY) > TLS(YY) THEN TLS(YY) = LLS(YY)
	NEXT I
	IF XX% <= 10 THEN LTL(YY) = LL(YY): LTW(YY) = WW(YY): GOTO 60
	M = XX% - 9
	FOR J = M TO XX%
	IF O%(J) > T%(J) THEN LTW(YY) = LTW(YY) + 1
	IF T%(J) > O%(J) THEN LTL(YY) = LTL(YY) + 1
	NEXT J

60 NEXT KK
OPEN D$(1) + LG$ + ".STD" FOR OUTPUT AS #1
	WRITE #1, LGF$, D1$, D2$, D3$, D4$
	FOR I = 1 TO 40: WRITE #1, WW(I), HWW(I), RWW(I), LL(I), HLL(I), RLL(I), LTW(I), LTL(I), TML$(I): NEXT
CLOSE #1
	  LOCATE 10: COLOR 12: PRINT "P"; : COLOR 7: PRINT "RINT OR "; : COLOR 12: PRINT "V"; : COLOR 7: PRINT "IEW"
2671 I$ = GetKeyPress$
	IF UCASE$(I$) = "P" THEN 3000
	GOTO 2000

950 FOR X = AA TO ZZ - 1: FOR Y = X + 1 TO ZZ: IF WW(X) - LL(X) > WW(Y) - LL(Y) THEN 985
	IF WW(X) - LL(X) = WW(Y) - LL(Y) AND WW(X) <= WW(Y) AND WW(X) > LL(X) THEN 985
	IF WW(X) - LL(X) = WW(Y) - LL(Y) AND WW(X) >= WW(Y) AND WW(X) < LL(X) THEN 985
	TN$ = TML$(X): TML$(X) = TML$(Y): TML$(Y) = TN$
	J = WW(X): WW(X) = WW(Y): WW(Y) = J
	J = LL(X): LL(X) = LL(Y): LL(Y) = J
	J = HLL(X): HLL(X) = HLL(Y): HLL(Y) = J
	J = RLL(X): RLL(X) = RLL(Y): RLL(Y) = J
	J = HWW(X): HWW(X) = HWW(Y): HWW(Y) = J
	J = RWW(X): RWW(X) = RWW(Y): RWW(Y) = J
	J = LTL(X): LTL(X) = LTL(Y): LTL(Y) = J
	J = LTW(X): LTW(X) = LTW(Y): LTW(Y) = J
	J = TWS(X): TWS(X) = TWS(Y): TWS(Y) = J
	J = TLS(X): TLS(X) = TLS(Y): TLS(Y) = J

985 NEXT Y: NEXT X
	RETURN

2000 AA = 1: ZZ = 10: GOSUB 950
	CLS : LOCATE 1, 35: COLOR 11: PRINT LGF$
	LOCATE 2: COLOR 14: PRINT TAB(20); "                     MAJ          HOME    ROAD   LAST 10"
	LOCATE 3: COLOR 14: PRINT D1$; TAB(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "
	FOR I = 1 TO 10
	IF TML$(I) = "*" THEN 2014
	COLOR 3: PRINT TML$(I); : COLOR 15: PRINT TAB(18); USING "### ### ###"; WW(I) + LL(I); WW(I); LL(I); TAB(30);
	IF LL(I) = 0 AND LL(I) + WW(I) <> 0 THEN PRINT "1.000"; : GOTO 2015
	IF WW(I) = 0 THEN PRINT " .000"; : GOTO 2015
	IF WW(I) + LL(I) <> 0 THEN PRINT USING " .###"; WW(I) / (WW(I) + LL(I)); : GOTO 2015
	IF WW(I) + LL(I) = 0 THEN PRINT " .000";

2015 IF ((WW(1) - WW(I)) + (LL(I) - LL(1))) / 2 = 0 THEN PRINT TAB(36); " ---";
	IF ((WW(1) - WW(I)) + (LL(I) - LL(1))) / 2 <> 0 THEN PRINT TAB(36); USING "##.#"; ((WW(1) - WW(I)) + (LL(I) - LL(1))) / 2;
	PRINT TAB(45); USING "##"; TWS(I); TAB(49); TLS(I);
	PRINT TAB(53); USING "##"; HWW(I); TAB(56); HLL(I); TAB(61); RWW(I); TAB(64); RLL(I); TAB(70); LTW(I); TAB(73); LTL(I)

2014 NEXT
	MJ = 83 - WW(1) - LL(2)
	IF MJ < 0 THEN MJ = 0
	LOCATE 4, 42: PRINT USING "##"; MJ
	AA = 11: ZZ = 20: GOSUB 950
	LOCATE 12: COLOR 14: PRINT TAB(20); "                     MAJ          HOME    ROAD   LAST 10"
	LOCATE 13: COLOR 14: PRINT D2$; TAB(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "
	FOR I = 11 TO 20
	IF TML$(I) = "*" THEN 2034
	COLOR 3: PRINT TML$(I); : COLOR 15: PRINT TAB(18); USING "### ### ###"; WW(I) + LL(I); WW(I); LL(I); TAB(30);
	IF LL(I) = 0 AND LL(I) + WW(I) <> 0 THEN PRINT "1.000"; : GOTO 2035
	IF WW(I) = 0 THEN PRINT " .000"; : GOTO 2035
	IF WW(I) + LL(I) <> 0 THEN PRINT USING " .###"; WW(I) / (WW(I) + LL(I)); : GOTO 2035
	IF WW(I) + LL(I) = 0 THEN PRINT " .000";

2035 IF ((WW(11) - WW(I)) + (LL(I) - LL(11))) / 2 = 0 THEN PRINT TAB(36); " ---";
	IF ((WW(11) - WW(I)) + (LL(I) - LL(11))) / 2 <> 0 THEN PRINT TAB(36); USING "##.#"; ((WW(11) - WW(I)) + (LL(I) - LL(11))) / 2;
	PRINT TAB(45); USING "##"; TWS(I); TAB(49); TLS(I);
	PRINT TAB(53); USING "##"; HWW(I); TAB(56); HLL(I); TAB(61); RWW(I); TAB(64); RLL(I); TAB(70); LTW(I); TAB(73); LTL(I)

2034 NEXT
	MJ = 83 - WW(11) - LL(12)
	IF MJ < 0 THEN MJ = 0
	LOCATE 14, 42: PRINT USING "##"; MJ
	AA = 21: ZZ = 30: GOSUB 950
	COLOR 14: LOCATE 24: PRINT "HIT ANY KEY TO CONTINUE";
2051 I$ = GetKeyPress$
	CLS
	LOCATE 2: COLOR 14: PRINT TAB(20); "                     MAJ          HOME    ROAD   LAST 10"
	LOCATE 3: COLOR 14: PRINT D3$; TAB(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "
	FOR I = 21 TO 30
	IF TML$(I) = "*" THEN 2064
	COLOR 3: PRINT TML$(I); : COLOR 15: PRINT TAB(18); USING "### ### ###"; WW(I) + LL(I); WW(I); LL(I); TAB(30);
	IF LL(I) = 0 AND LL(I) + WW(I) <> 0 THEN PRINT "1.000"; : GOTO 2065
	IF WW(I) = 0 THEN PRINT " .000"; : GOTO 2065
	IF WW(I) + LL(I) <> 0 THEN PRINT USING " .###"; WW(I) / (WW(I) + LL(I)); : GOTO 2065
	IF WW(I) + LL(I) = 0 THEN PRINT " .000";

2065 IF ((WW(21) - WW(I)) + (LL(I) - LL(21))) / 2 = 0 THEN PRINT TAB(36); " ---";
	IF ((WW(21) - WW(I)) + (LL(I) - LL(21))) / 2 <> 0 THEN PRINT TAB(36); USING "##.#"; ((WW(21) - WW(I)) + (LL(I) - LL(21))) / 2;
	PRINT TAB(45); USING "##"; TWS(I); TAB(49); TLS(I);
	PRINT TAB(53); USING "##"; HWW(I); TAB(56); HLL(I); TAB(61); RWW(I); TAB(64); RLL(I); TAB(70); LTW(I); TAB(73); LTL(I)

2064 NEXT
	MJ = 83 - WW(21) - LL(22)
	IF MJ < 0 THEN MJ = 0
	LOCATE 4, 42: PRINT USING "##"; MJ
	AA = 31: ZZ = 40: GOSUB 950
	LOCATE 12: COLOR 14: PRINT TAB(20); "                     MAJ          HOME    ROAD   LAST 10"
	LOCATE 13: COLOR 14: PRINT D4$; TAB(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "
	FOR I = 31 TO 40
	IF TML$(I) = "*" THEN 2094 ELSE 2093

2093 COLOR 3: PRINT TML$(I); : COLOR 15: PRINT TAB(18); USING "### ### ###"; WW(I) + LL(I); WW(I); LL(I); TAB(30);
	IF LL(I) = 0 AND LL(I) + WW(I) <> 0 THEN PRINT "1.000"; : GOTO 2095
	IF WW(I) = 0 THEN PRINT " .000"; : GOTO 2095
	IF WW(I) + LL(I) <> 0 THEN PRINT USING " .###"; WW(I) / (WW(I) + LL(I)); : GOTO 2095
	IF WW(I) + LL(I) = 0 THEN PRINT " .000";

2095 IF ((WW(31) - WW(I)) + (LL(I) - LL(31))) / 2 = 0 THEN PRINT TAB(36); " ---";
	IF ((WW(31) - WW(I)) + (LL(I) - LL(31))) / 2 <> 0 THEN PRINT TAB(36); USING "##.#"; ((WW(31) - WW(I)) + (LL(I) - LL(31))) / 2;
	PRINT TAB(45); USING "##"; TWS(I); TAB(49); TLS(I);
	PRINT TAB(53); USING "##"; HWW(I); TAB(56); HLL(I); TAB(61); RWW(I); TAB(64); RLL(I); TAB(70); LTW(I); TAB(73); LTL(I)

2094 NEXT
	MJ = 83 - WW(31) - LL(32)
	IF MJ < 0 THEN MJ = 0
	LOCATE 14, 42: PRINT USING "##"; MJ
	LOCATE 23: COLOR 14: PRINT "HIT ANY KEY TO CONTINUE"
2216 I$ = GetKeyPress$
	GOTO 2300

3000 LPRINT CHR$(15)
	WIDTH "LPT1:1", 130
	AA = 1: ZZ = 10: GOSUB 950
	LPRINT LGF$
	LPRINT TAB(20); "                     MAJ          HOME    ROAD   LAST 10"
	LPRINT D1$; TAB(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "
	FOR I = 1 TO 10
	IF TML$(I) = "*" THEN 3014
	LPRINT TML$(I); : LPRINT TAB(18); USING "### ### ###"; WW(I) + LL(I); WW(I); LL(I); TAB(30);
	IF LL(I) = 0 AND LL(I) + WW(I) <> 0 THEN LPRINT "1.000"; : GOTO 3015
	IF WW(I) = 0 THEN LPRINT " .000"; : GOTO 3015
	IF WW(I) + LL(I) <> 0 THEN LPRINT USING " .###"; WW(I) / (WW(I) + LL(I)); : GOTO 3015
	IF WW(I) + LL(I) = 0 THEN LPRINT " .000";

3015 IF ((WW(1) - WW(I)) + (LL(I) - LL(1))) / 2 = 0 THEN LPRINT TAB(36); " ---";
	IF ((WW(1) - WW(I)) + (LL(I) - LL(1))) / 2 <> 0 THEN LPRINT TAB(36); USING "##.#"; ((WW(1) - WW(I)) + (LL(I) - LL(1))) / 2;
	LPRINT TAB(42);
	MJ = 83 - WW(1) - LL(2)
	IF MJ < 0 THEN MJ = 0
	IF I = 1 THEN LPRINT USING "##"; MJ;
	LPRINT TAB(45); USING "##"; TWS(I); TAB(49); TLS(I);
	LPRINT TAB(53); USING "##"; HWW(I); TAB(56); HLL(I); TAB(61); RWW(I); TAB(64); RLL(I); TAB(70); LTW(I); TAB(73); LTL(I)

3014 NEXT
	AA = 11: ZZ = 20: GOSUB 950
	LPRINT : LPRINT : LPRINT TAB(20); "                     MAJ          HOME    ROAD   LAST 10"
	LPRINT D2$; TAB(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "
	FOR I = 11 TO 20
	IF TML$(I) = "*" THEN 3034
	LPRINT TML$(I); : LPRINT TAB(18); USING "### ### ###"; WW(I) + LL(I); WW(I); LL(I); TAB(30);
	IF LL(I) = 0 AND LL(I) + WW(I) <> 0 THEN LPRINT "1.000"; : GOTO 3035
	IF WW(I) = 0 THEN LPRINT " .000"; : GOTO 3035
	IF WW(I) + LL(I) <> 0 THEN LPRINT USING " .###"; WW(I) / (WW(I) + LL(I)); : GOTO 3035
	IF WW(I) + LL(I) = 0 THEN LPRINT " .000";

3035 IF ((WW(11) - WW(I)) + (LL(I) - LL(11))) / 2 = 0 THEN LPRINT TAB(36); " ---";
	IF ((WW(11) - WW(I)) + (LL(I) - LL(11))) / 2 <> 0 THEN LPRINT TAB(36); USING "##.#"; ((WW(11) - WW(I)) + (LL(I) - LL(11))) / 2;
	LPRINT TAB(42);
	MJ = 83 - WW(11) - LL(12)
	IF MJ < 0 THEN MJ = 0
	IF I = 11 THEN LPRINT USING "##"; MJ;
	LPRINT TAB(45); USING "##"; TWS(I); TAB(49); TLS(I);
	LPRINT TAB(53); USING "##"; HWW(I); TAB(56); HLL(I); TAB(61); RWW(I); TAB(64); RLL(I); TAB(70); LTW(I); TAB(73); LTL(I)

3034 NEXT: LPRINT : LPRINT
	AA = 21: ZZ = 30: GOSUB 950
	LPRINT TAB(20); "                     MAJ          HOME    ROAD   LAST 10"
	LPRINT D3$; TAB(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "
	FOR I = 21 TO 30
	IF TML$(I) = "*" THEN 3064
	LPRINT TML$(I); : LPRINT TAB(18); USING "### ### ###"; WW(I) + LL(I); WW(I); LL(I); TAB(30);
	IF LL(I) = 0 AND LL(I) + WW(I) <> 0 THEN LPRINT "1.000"; : GOTO 3065
	IF WW(I) = 0 THEN LPRINT " .000"; : GOTO 3065
	IF WW(I) + LL(I) <> 0 THEN LPRINT USING " .###"; WW(I) / (WW(I) + LL(I)); : GOTO 3065
	IF WW(I) + LL(I) = 0 THEN LPRINT " .000";

3065 IF ((WW(21) - WW(I)) + (LL(I) - LL(21))) / 2 = 0 THEN LPRINT TAB(36); " ---";
	IF ((WW(21) - WW(I)) + (LL(I) - LL(21))) / 2 <> 0 THEN LPRINT TAB(36); USING "##.#"; ((WW(21) - WW(I)) + (LL(I) - LL(21))) / 2;
	LPRINT TAB(42); : IF I = 21 THEN LPRINT USING "##"; 83 - WW(21) - LL(22);
	LPRINT TAB(45); USING "##"; TWS(I); TAB(49); TLS(I);
	LPRINT TAB(53); USING "##"; HWW(I); TAB(56); HLL(I); TAB(61); RWW(I); TAB(64); RLL(I); TAB(70); LTW(I); TAB(73); LTL(I)

3064 NEXT: LPRINT : LPRINT
	AA = 31: ZZ = 40: GOSUB 950
	LPRINT TAB(20); "                     MAJ          HOME    ROAD   LAST 10"
	LPRINT D4$; TAB(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "
	FOR I = 31 TO 40
	IF TML$(I) = "*" THEN 3094 ELSE 3093

3093 LPRINT TML$(I); : LPRINT TAB(18); USING "### ### ###"; WW(I) + LL(I); WW(I); LL(I); TAB(30);
	IF LL(I) = 0 AND LL(I) + WW(I) <> 0 THEN LPRINT "1.000"; : GOTO 3095
	IF WW(I) = 0 THEN LPRINT " .000"; : GOTO 3095
	IF WW(I) + LL(I) <> 0 THEN LPRINT USING " .###"; WW(I) / (WW(I) + LL(I)); : GOTO 3095
	IF WW(I) + LL(I) = 0 THEN LPRINT " .000";

3095 IF ((WW(31) - WW(I)) + (LL(I) - LL(31))) / 2 = 0 THEN LPRINT TAB(36); " ---";
	IF ((WW(31) - WW(I)) + (LL(I) - LL(31))) / 2 <> 0 THEN LPRINT TAB(36); USING "##.#"; ((WW(31) - WW(I)) + (LL(I) - LL(31))) / 2;
	LPRINT TAB(42);
	MJ = 83 - WW(31) - LL(32)
	IF MJ < 0 THEN MJ = 0
	IF I = 31 THEN LPRINT USING "##"; MJ;
	LPRINT TAB(45); USING "##"; TWS(I); TAB(49); TLS(I);
	LPRINT TAB(53); USING "##"; HWW(I); TAB(56); HLL(I); TAB(61); RWW(I); TAB(64); RLL(I); TAB(70); LTW(I); TAB(73); LTL(I)

3094 NEXT
	LPRINT CHR$(12)
	LPRINT CHR$(18)
	WIDTH "LPT1:1", 80
	GOTO 2300

1440 null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
	Count% = DirCount%
	IF Count% = 0 THEN 4600
REDIM F$(0 TO Count%)
	FOR X = 1 TO Count%
	F$(X) = SPACE$(12)
	NEXT
	F$(0) = Spec$
	CALL ReadFile(F$())
	'CALL SortStr(VARPTR(F$(1)), Count%, 0)

1445 CLS
	COLOR , 1
	CLS
	LOCATE , 28
	COLOR 14
	PRINT "SELECT LEAGUE": COLOR 15
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
	PRINT "-SELECTS LEAGUE";

_MouseShow "DEFAULT"
MAMenu F$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF ExitCode% = 27 THEN 2300
	F$ = F$(SELECTION%)
	LOCATE 2, 33: PRINT F$
LOCATE , 33: PRINT "ANY CHANGE (YN)";
	COLOR 15, 0
3912 I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 1445

3913 F$ = QPRTRIM$(F$)
	F$ = LEFT$(F$, LEN(F$) - 4)
	LG$ = F$
	RETURN

4600 LOCATE 10, 8
	PRINT "NO LEAGUE FILES WERE FOUND"
	PRINT "PRESS ANY KEY TO RETURN TO MAIN MENU"
	CALL GETKEY
	GOTO 2300

SUB TITLE ()
	COLOR , 0: KEY OFF: CLS
	COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 27: COLOR 12
	PRINT "BASKETBALL: THE PRO GAME V4.0"
	LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
	PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; TAB(70); LEFT$(TIME$, 5);
	LOCATE 3, 1: COLOR 2, 0
	COLOR 15: PRINT "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
	  PRINT "     FOR TECHNICAL SUPPORT, CALL (615) 366-8088 OR SportGames@AOL.COM"
	COLOR 10
END SUB

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
