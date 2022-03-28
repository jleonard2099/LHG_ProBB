'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Basketball: The Pro Game - Adjust Game Time"
''$ExeIcon:'./lhg_bbpro.ico'
'_Icon
_AllowFullScreen

10 Dim T$(4), B%(15, 19), B$(15, 1), T%(35), L%(6), Q$(377), v$(500), v%(500)
Dim P$(3), D$(1), menuItems$(5)

20 P$(0) = "G": P$(1) = "F": P$(2) = "C"

50 Open "DEV.BAS" For Input As #1
Input #1, D$(0): Close #1

60 Color 1, 0: Cls: Color , 1: Locate 1, 1: Print Tab(80);: Locate , 22: Color 14
Print "BASKETBALL:THE PRO GAME ADJUST PLAYING TIME"
Locate 24, 1: Print Tab(80);: Locate , 1
Print "(C) LANCE HAFFNER/SHANNON LYNN 1991"; Tab(70); Left$(Time$, 5);
Locate 4, 1: Color 1, 0
menuItems$(1) = "ADJUST GAMES AND MINUTES PER GAME "
menuItems$(2) = "EXIT TO MAIN MENU                 "
SELECTION% = 1
Count% = 2
Start% = 1
SCANCODE% = 2
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

Call box0(7, 24, 10, 59, 2, 31)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
I = SELECTION%
If SCANCODE% = 27 Then I = 2
If I = 1 Then 181
    
5 Run "HELLO.EXE"

100 Color , 0: Cls: Color T%(33), T%(34): Print T$(0): Color 15, 0
Print "   PLAYERS                 ACTUAL           ADJUSTED"
Print "                          GP   MPG          GP    MPG"
GP = 0: MIN = 0
For I = 0 To 14
    If B%(I, 16) >= 0 Or B%(I, 17) >= 0 Then B%(I, 16) = B%(I, 0) * -1: B%(I, 17) = B%(I, 1) * -1
    GP = GP + B%(I, 0)
    MIN = MIN + B%(I, 17) * -1
    If Left$(B$(I, 0), 3) = "XXX" Then 105
    Print Using "## "; I;: Print B$(I, 0); " "; B$(I, 1); Tab(27);
    Print Using "##   ##.#         ##   ##.#"; B%(I, 0); B%(I, 1) / B%(I, 0); B%(I, 16) * -1; B%(I, 17) / B%(I, 16)

105 Next
Print "15 END AND SAVE CHANGES"
Print
Print "TEAM MIN/G SHOULD BE APPROXIMATELY 240, OVERTIME WILL INCREASE THIS FIGURE"
Print Using "TEAM MIN/G = ###.#"; MIN / T%(0)

560 Locate 23, 1

561 Input "ENTER SELECTION "; B1

562 If B1 < 0 Or B1 > 15 Then 560
If B1 = 15 Then 1380
GoSub 2800
Print B$(B1, 0); " ";: Input "GAMES PLAYED "; GP
GoSub 2800
Print B$(B1, 0); " ";: Input "MINUTES PER GAME "; MPG
B%(B1, 16) = GP * -1
B%(B1, 17) = MPG * GP * -1
GoTo 100

1380 Open D$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031
Field #1, 15 As Q$(0), 15 As Q$(1)
For I = 0 To 6
    Field #1, 30 + I * 2 As X$, 2 As Q$(I + 2)
Next
Field #1, 44 As X$, 2 As Q$(9)
For I = 0 To 34
    Field #1, 46 + I * 2 As X$, 2 As Q$(I + 10)
Next
For I = 0 To 14
    Field #1, 116 + I * 57 As X$, 15 As Q$(I * 22 + 45), 2 As Q$(I * 22 + 46)
    For I1 = 0 To 19
        Field #1, 133 + I * 57 + I1 * 2 As X$, 2 As Q$(I * 22 + 47 + I1)
    Next
Next
Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)
LSet Q$(0) = T$(0): LSet Q$(1) = T$(1): For I = 0 To 6: LSet Q$(I + 2) = MKI$(L%(I)): Next
LSet Q$(9) = MKI$(999)
For I = 0 To 34: LSet Q$(I + 10) = MKI$(T%(I)): Next
For I = 0 To 14
    LSet Q$(22 * I + 45) = B$(I, 0)
    LSet Q$(22 * I + 46) = B$(I, 1)
    For I1 = 0 To 19
        LSet Q$(22 * I + 47 + I1) = MKI$(B%(I, I1))
Next: Next
LSet Q$(375) = T$(2): LSet Q$(376) = T$(3): LSet Q$(377) = T$(4)
Put #1, T1: Close 1

1430 GoTo 60

181 Color 15, 0: Cls
JJ = 6
SN = 1: Locate 6, 8: Color 15, 4
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Print
Locate , 12: Print "ESC TO RETURN TO MAIN MENU"
3057 I$ = GetKeyPress$
If I$ = Chr$(27) Then 60
If I$ = Chr$(13) Then GoSub 1102: GoTo 3060
Locate 7, 27: Print I$
Locate 7, 28: Line Input NY$
NY$ = I$ + NY$
Color 15, 0
If NY$ = "" Then GoSub 1102: GoTo 3060
YN$ = NY$
For I = 1 To 500: v$(I) = "": Next
GoSub 1100

3060 If BO% = 1 Then BO% = 0: GoTo 181

3058 Locate 2, 33: Color T%(33), T%(34): Print T$(0)
Color 15, 4: Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 181
Color 15, 0
GoTo 100

1100 NTMS = 0
Open D$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031
Y1 = LOF(1)
Field #1, 15 As Q$(0), 15 As Q$(1)
For I = 0 To 6
    Field #1, 30 + I * 2 As X$, 2 As Q$(I + 2)
Next
Field #1, 44 As X$, 2 As Q$(9)
For I = 0 To 34
    Field #1, 46 + I * 2 As X$, 2 As Q$(I + 10)
Next
For I = 0 To 14
    Field #1, 116 + I * 57 As X$, 15 As Q$(I * 22 + 45), 2 As Q$(I * 22 + 46)
    For I1 = 0 To 19
        Field #1, 133 + I * 57 + I1 * 2 As X$, 2 As Q$(I * 22 + 47 + I1)
    Next
Next
Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)
For I1 = 1 To Y1 / 1031
    Get #1, I1
    CK% = CVI(Q$(9))
    If CK% <> 999 Then 1103
    NTMS = NTMS + 1
    v$(NTMS) = Q$(0): v%(NTMS) = I1

1103 Next
Close 1

1102 SELECTION% = 1
Count% = NTMS
Start% = 1
FileFGColr% = 15: FileBGColr% = 4
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Color , 4: Cls: Locate , 33: Color 14
Print "TEAM SELECTION": Color 15
Locate 25, 1: Color 14: Print "ESC";: Color 15
Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
Print "ENTER";: Color 15: Print "-SELECTS TEAM";
    
_MouseShow "DEFAULT"
120 MAMenu v$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 120
If EXITCODE% = -79 Then Start% = NTMS: EXITCODE% = 3: SELECTION% = NTMS: GoTo 120
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NTMS Then Start% = NTMS
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 120
T1 = v%(SELECTION%)
Open D$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031
Field #1, 15 As Q$(0), 15 As Q$(1)
For I = 0 To 6
    Field #1, 30 + I * 2 As X$, 2 As Q$(I + 2)
Next
Field #1, 44 As X$, 2 As Q$(9)
For I = 0 To 34
    Field #1, 46 + I * 2 As X$, 2 As Q$(I + 10)
Next
For I = 0 To 14
    Field #1, 116 + I * 57 As X$, 15 As Q$(I * 22 + 45), 2 As Q$(I * 22 + 46)
    For I1 = 0 To 19
        Field #1, 133 + I * 57 + I1 * 2 As X$, 2 As Q$(I * 22 + 47 + I1)
    Next
Next
Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)
Get #1, T1
T$(0) = Q$(0): T$(1) = Q$(1): For I = 0 To 6: L%(I) = CVI(Q$(I + 2)): Next
For I = 0 To 34: T%(I) = CVI(Q$(I + 10)): Next
For I = 0 To 14
    B$(I, 0) = Q$(22 * I + 45)
    B$(I, 1) = Q$(22 * I + 46)
    For I1 = 0 To 19
        B%(I, I1) = CVI(Q$(22 * I + 47 + I1))
Next: Next
T$(2) = Q$(375): T$(3) = Q$(376): T$(4) = Q$(377)
Close

1486 If Right$(T$(0), 1) = " " Then T$(0) = Left$(T$(0), Len(T$(0)) - 1): GoTo 1486

1487 If Right$(T$(1), 1) = " " Then T$(1) = Left$(T$(1), Len(T$(1)) - 1): GoTo 1487

1488 If Right$(T$(2), 1) = " " Then T$(2) = Left$(T$(2), Len(T$(2)) - 1): GoTo 1488

1489 If Right$(T$(3), 1) = " " Then T$(3) = Left$(T$(3), Len(T$(3)) - 1): GoTo 1489

1490 If Right$(T$(4), 1) = " " Then T$(4) = Left$(T$(4), Len(T$(4)) - 1): GoTo 1490

1540 Return

2800 Locate 23, 1: Print "                                      ";: Locate 23, 1

2805 Return

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
