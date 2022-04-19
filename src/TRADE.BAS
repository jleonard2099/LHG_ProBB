'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Basketball: The Pro Game - Trade Players"
''$ExeIcon:'./lhg_bbpro.ico'
'_Icon
_AllowFullScreen

DECLARE SUB TITLE ()

Dim B$(2, 14, 1), B%(2, 14, 19), T(1), B1(2, 14, 14), B1$(2, 14), W0(2, 14), W1(2, 14)
Dim Z(2, 15), Z1(2, 15), T%(1, 34), Q$(377), V$(500), V%(500)
Dim D$(1), menuItems$(3), NY$(1), YN$(1), S(1), L%(1, 6), SA$(1), F$(1)
Dim A1$(1), A$(1, 6)

Color 3, 0

Open "DEV.BAS" For Input As #1: Input #1, D$(0): Input #1, D$(1): Close 1

99 Call TITLE
menuItems$(1) = "TRADE PLAYERS           "
menuItems$(2) = "RETURN TO PRO GAME MENU "
SELECTION% = 1
Count% = 2
Start% = 1
SCANCODE% = 2
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

CALL box0(7, 24, 10, 49, 2,  15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Or SELECTION% = 2 Then 1
If SELECTION% = 1 Then 2111

1 Run "HELLO.EXE 2"

2111 J = 0
Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
Locate 7 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID FOR TEAM A: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 38: Input NY$(J)
If NY$(J) = "" Then GoSub 1102: GoTo 2112
YN$(J) = NY$(J)
For I = 1 To 500: V$(I) = "": Next
GoSub 1100

2112 If BO% = 1 Then BO% = 0: GoTo 2111

2113 Locate 2, 33: Color T%(J, 33), T%(J, 34): Print A$(J, 0): Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 2111
Color 15, 0
S(0) = 0
JJ = 1: GoSub 9150
U$ = "DOES THIS TEAM HAVE SAVED STATS (YN)"
Locate , (80 - Len(U$)) / 2
Print U$
Color 15, 0
I$ = GetKeyPress$
If UCase$(I$) = "N" Then S(0) = 1: GoTo 2150
If UCase$(Left$(D$(1), 1)) <> "A" And UCase$(Left$(D$(1), 1)) <> "B" Then 2145
Cls

2145 GoSub 100

2150 J = 1
Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
Locate 7 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID FOR TEAM B: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 38: Input NY$(J)
If NY$(J) = "" Then GoSub 1102: GoTo 2152
YN$(J) = NY$(J)
For I = 1 To 500: V$(I) = "": Next
GoSub 1100

2152 If BO% = 1 Then BO% = 0: GoTo 2150

2153 Locate 2, 33: Color T%(J, 33), T%(J, 34): Print A$(J, 0): Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 2150
Color 15, 0
S(1) = 0
JJ = 1: GoSub 9150
U$ = "DOES THIS TEAM HAVE SAVED STATS (YN)"
Locate , (80 - Len(U$)) / 2
Print U$
Color 15, 0
I$ = GetKeyPress$
If UCase$(I$) = "N" Then S(1) = 1: GoTo 200
If UCase$(Left$(D$(1), 1)) <> "A" And UCase$(Left$(D$(1), 1)) <> "B" Then 2195
Cls

2195 GoSub 100

200 Cls
Color 15: Print "T";: Color 3: Print "RADE PLAYERS": Print
Color 15: Print "N";: Color 3: Print "O MORE TRADES"
201 I$ = GetKeyPress$
If UCase$(I$) = "N" Then 500

210 If S(0) = 1 Or S(1) = 1 Then 216
Cls: Print: Print
Color 14: Print "(T)";: Color 15: Print " TRADE PLAYERS' COMPILED STATS"
Color 14: Print: Print "(Z)";: Color 15: Print " ZERO PLAYERS' COMPILED STATS"
300 I$ = GetKeyPress$
If UCase$(I$) <> "T" And UCase$(I$) <> "Z" Then 216
If UCase$(I$) = "T" Then W1 = 1
If UCase$(I$) = "Z" Then W1 = 0

216 Cls: Locate 3, 1: Color T%(0, 33), T%(0, 34): Print A$(0, 0);: Locate , 20: Color T%(1, 33), T%(1, 34): Print A$(1, 0): Color 3, 0
For I = 0 To 14: Locate I + 4, 1: Print I; " "; B$(0, I, 0); " "; B$(0, I, 1);: Locate , 20: Print I; " "; B$(1, I, 0); " "; B$(1, I, 1): Next I

218 Locate 19, 1: Print A$(0, 0); " PLAYER: ";
Input I
If I < 0 Or I > 14 Then 218
Color T%(0, 33), T%(0, 34): Print B$(0, I, 0): Color 3, 0

223 Locate 21, 1: Print A$(1, 0); " PLAYER: ";
Input I1
If I1 < 0 Or I1 > 14 Then 223
Color T%(1, 33), T%(1, 34): Print B$(1, I1, 0): Color 3, 0
Locate 23, 1: Print "ANY CHANGE (YN) ";
228 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 218
GoSub 600
For I2 = 0 To 1: Swap B$(0, I, I2), B$(1, I1, I2): Next
For I2 = 0 To 19: Swap B%(0, I, I2), B%(1, I1, I2): Next I2
Swap B1$(0, I), B1$(1, I1)
If S(0) = 1 Or S(1) = 1 Or W1 = 0 Then 240
For I2 = 0 To 13: Swap B1(0, I, I2), B1(1, I1, I2): Next I2
Swap W0(0, I), W0(1, I1)
Swap W1(0, I), W1(1, I1)
GoSub 610: GoTo 200

240 For I2 = 0 To 13: B1(0, I, I2) = 0: B1(1, I1, I2) = 0: Next
W0(1, I1) = 0: W1(1, I1) = 0: W0(0, I) = 0: W1(0, I) = 0
GoSub 610: GoTo 200

500 For J = 0 To 1
    If UCase$(Left$(D$(0), 1)) <> "A" And UCase$(Left$(D$(0), 1)) <> "B" Then 556
    Cls

    556 GoSub 9800
Next J
For J = 0 To 1
    If S(J) = 1 Then 525
    Cls

    557 Open D$(1) + F$(J) For Output As #1
    Print #1, A1$(J): For I = 0 To 14: Print #1, B1$(J, I): For I1 = 0 To 13: Print #1, B1(J, I, I1): Next I1: Print #1, W0(J, I): Print #1, W1(J, I): Next I
    For L = 0 To 15: Print #1, Z(J, L): Next L
    For L = 0 To 15: Print #1, Z1(J, L): Next L
    Close 1

525 Next J
GoTo 99

600 GoSub 700
Locate 23, 1: Print "HIT ANY KEY TO TRADE";
602 I$ = GetKeyPress$
Return

610 GoSub 700
Locate 23, 1: Print "HIT ANY KEY TO CONTINUE";
612 I$ = GetKeyPress$
Return
Locate 23, 1: Print "HIT ANY KEY TO TRADE";
622 I$ = GetKeyPress$
Return
Locate 23, 1: Print "HIT ANY KEY TO CONTINUE";
632 I$ = GetKeyPress$
Return

700 Cls: Locate 3, 1: Color T%(0, 33), T%(0, 34): Locate , 10: Print A$(0, 0);: Locate , 50: Color T%(1, 33), T%(1, 34): Print A$(1, 0): Color 3, 0: Print
Locate , 10: Print B$(0, I, 0); " "; B$(0, I, 1); Tab(50); B$(1, I1, 0); " "; B$(1, I1, 1)
Locate , 5: Print Using "  ##   GAMES     ##"; B%(0, I, 0); B1(0, I, 0)
Locate , 5: Print Using "####  MINUTES  ####"; B%(0, I, 1); B1(0, I, 10) / 60
Locate , 5: Print Using "####    FGA    ####"; B%(0, I, 2); B1(0, I, 1)
Locate , 5: Print Using "####    FGM    ####"; B%(0, I, 3); B1(0, I, 2)
Locate , 5: Print Using "####    FTA    ####"; B%(0, I, 4); B1(0, I, 3)
Locate , 5: Print Using "####    FTM    ####"; B%(0, I, 5); B1(0, I, 4)
Locate , 5: Print Using "####  OFF REB  ####"; B%(0, I, 6); B1(0, I, 12)
Locate , 5: Print Using "####  TOT REB  ####"; B%(0, I, 6) + B%(0, I, 7); B1(0, I, 5)
Locate , 5: Print Using "####   FOULS   ####"; B%(0, I, 9); B1(0, I, 6)
Locate , 5: Print Using "####  STEALS   ####"; B%(0, I, 11); B1(0, I, 7)
Locate , 5: Print Using "####  ASSISTS  ####"; B%(0, I, 8); B1(0, I, 8)
Locate , 5: Print Using "####  BLOCKS   ####"; B%(0, I, 12); B1(0, I, 9)
Locate , 5: Print Using "     TURNOVERS ####"; B1(0, I, 11)
Locate , 5: Print Using "####   3FGA    ####"; B%(0, I, 13); W1(0, I)
Locate , 5: Print Using "####   3FGM    ####"; B%(0, I, 14); W0(0, I)
Locate , 5: Print Using "  ##  HI GAME    ##"; B%(0, I, 15); B1(0, I, 13)
Locate 6, 45: Print Using "  ##   GAMES     ##"; B%(1, I1, 0); B1(1, I1, 0)
Locate , 45: Print Using "####  MINUTES  ####"; B%(1, I1, 1); B1(1, I1, 10) / 60
Locate , 45: Print Using "####    FGA    ####"; B%(1, I1, 2); B1(1, I1, 1)
Locate , 45: Print Using "####    FGM    ####"; B%(1, I1, 3); B1(1, I1, 2)
Locate , 45: Print Using "####    FTA    ####"; B%(1, I1, 4); B1(1, I1, 3)
Locate , 45: Print Using "####    FTM    ####"; B%(1, I1, 5); B1(1, I1, 4)
Locate , 45: Print Using "####  OFF REB  ####"; B%(1, I1, 6); B1(1, I1, 12)
Locate , 45: Print Using "####  TOT REB  ####"; B%(1, I1, 6) + B%(1, I1, 7); B1(1, I1, 5)
Locate , 45: Print Using "####   FOULS   ####"; B%(1, I1, 9); B1(1, I1, 6)
Locate , 45: Print Using "####  STEALS   ####"; B%(1, I1, 11); B1(1, I1, 7)
Locate , 45: Print Using "####  ASSISTS  ####"; B%(1, I1, 8); B1(1, I1, 8)
Locate , 45: Print Using "####  BLOCKS   ####"; B%(1, I1, 12); B1(1, I1, 9)
Locate , 45: Print Using "     TURNOVERS ####"; B1(1, I1, 11)
Locate , 45: Print Using "####   3FGA    ####"; B%(1, I1, 13); W1(1, I1)
Locate , 45: Print Using "####   3FGM    ####"; B%(1, I1, 14); W0(1, I1)
Locate , 45: Print Using "  ##  HI GAME    ##"; B%(1, I1, 15); B1(1, I1, 13)
Return

1100 NT = 0
Open D$(0) + "PROBBTMS." + YN$(J) For Random As #1 Len = 1031
Y = LOF(1)
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
For I1 = 1 To Y / 1031
    Get #1, I1
    CK% = CVI(Q$(9))
    If CK% <> 999 Then 1103
    NT = NT + 1
    V$(NT) = Q$(0): V%(NT) = I1

1103 Next
Close 1
TS% = Int((NT - 1) / 80)

1102 SELECTION% = 1
Count% = NT
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
20 MAMenu V$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
If EXITCODE% = -79 Then Start% = NT: EXITCODE% = 3: SELECTION% = NT: GoTo 20
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NT Then Start% = NT
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 20
T(J) = V%(SELECTION%)
Open D$(0) + "PROBBTMS." + YN$(J) For Random As #1 Len = 1031
Field #1, 15 As Q$(0), 15 As Q$(1)
For J% = 0 To 6
    Field #1, 30 + J% * 2 As X$, 2 As Q$(J% + 2)
Next
Field #1, 44 As X$, 2 As Q$(9)
For J% = 0 To 34
    Field #1, 46 + J% * 2 As X$, 2 As Q$(J% + 10)
Next
For J% = 0 To 14
    Field #1, 116 + J% * 57 As X$, 15 As Q$(J% * 22 + 45), 2 As Q$(J% * 22 + 46)
    For J1 = 0 To 19
        Field #1, 133 + J% * 57 + J1 * 2 As X$, 2 As Q$(J% * 22 + 47 + J1)
    Next
Next
Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)
Get #1, T(J)
A$(J, 0) = Q$(0): A$(J, 1) = Q$(1)
For I = 0 To 6: L%(J, I) = CVI(Q$(I + 2)): Next

9002 If Right$(A$(J, 0), 1) = " " Then A$(J, 0) = Left$(A$(J, 0), Len(A$(J, 0)) - 1): GoTo 9002
For I = 0 To 34: T%(J, I) = CVI(Q$(I + 10)): Next
For I = 0 To 14
    B$(J, I, 0) = Q$(22 * I + 45)

    9004 If Right$(B$(J, I, 0), 1) = " " Then B$(J, I, 0) = Left$(B$(J, I, 0), Len(B$(J, I, 0)) - 1): GoTo 9004
    B$(J, I, 1) = Q$(22 * I + 46)

    9005 If Right$(B$(J, I, 1), 1) = " " Then B$(J, I, 1) = Left$(B$(J, I, 1), Len(B$(J, I, 1)) - 1): GoTo 9005
    For I1 = 0 To 19
        B%(J, I, I1) = CVI(Q$(22 * I + 47 + I1))
Next: Next
A$(J, 2) = Q$(375): A$(J, 3) = Q$(376): A$(J, 4) = Q$(377)
Close 1
SA$(J) = A$(J, 0)

9006 If Right$(A$(J, 2), 1) = " " Then A$(J, 2) = Left$(A$(J, 2), Len(A$(J, 2)) - 1): GoTo 9006

9007 If Right$(A$(J, 3), 1) = " " Then A$(J, 3) = Left$(A$(J, 3), Len(A$(J, 3)) - 1): GoTo 9007

9008 If Right$(A$(J, 4), 1) = " " Then A$(J, 4) = Left$(A$(J, 4), Len(A$(J, 4)) - 1): GoTo 9008
Return
    

4091 Locate 22, 1: Print "ANY KEY TO CONTINUE"
4092 I$ = GetKeyPress$
Return

9800 Open D$(0) + "PROBBTMS." + YN$(J) For Random As #1 Len = 1031
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
LSet Q$(0) = A$(J, 0): LSet Q$(1) = A$(J, 1): For I = 0 To 6: LSet Q$(I + 2) = MKI$(L%(J, I)): Next
LSet Q$(9) = MKI$(999)
For I = 0 To 34: LSet Q$(I + 10) = MKI$(T%(J, I)): Next
For I = 0 To 14
    LSet Q$(22 * I + 45) = B$(J, I, 0)
    LSet Q$(22 * I + 46) = B$(J, I, 1)
    For I1 = 0 To 19
        LSet Q$(22 * I + 47 + I1) = MKI$(B%(J, I, I1))
Next: Next
LSet Q$(375) = A$(J, 2): LSet Q$(376) = A$(J, 3): LSet Q$(377) = A$(J, 4)
Put #1, T(J): Close 1
Return

9150 Locate 6, 8: Color T%(J, 33), T%(J, 34)
Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
For II = 1 To JJ: Locate 6 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next
Locate 7 + JJ, 8
Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
Return

100 For XX = 4 To 11: If Mid$(SA$(J), XX, 1) = " " Or Mid$(SA$(J), XX, 1) = "/" Or Mid$(SA$(J), XX, 1) = "." Then Mid$(SA$(J), XX, 1) = "_"
Next XX
For XX = 12 To 14: If Mid$(SA$(J), XX, 2) = "ST" Then Mid$(SA$(J), 9, 1) = "_": Mid$(SA$(J), 10, 2) = "ST"
Next XX
QQ = 8: For XX = 11 To 4 Step -1: If Mid$(SA$(J), XX, 1) = "_" Then QQ = XX - 4: GoTo 106
    GoTo 107

106 Next XX

107 YR$ = Left$(SA$(J), 2)
F$(J) = Mid$(SA$(J), 4, QQ) + "." + YR$
If Not _FileExists(D$(1) + F$(J)) Then S(J) = 1: GoTo 108
Open D$(1) + F$(J) For Input As #1
Input #1, A1$(J)
For I = 0 To 14: Input #1, B1$(J, I)
    For I1 = 0 To 13: Input #1, B1(J, I, I1): Next I1
Input #1, W0(J, I): Input #1, W1(J, I): Next I
For L = 0 To 15: Input #1, Z(J, L): Next L
For L = 0 To 15: Input #1, Z1(J, L): Next L
Close 1

108 Return


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 25: Color 12
    Print "BASKETBALL: THE PRO GAME"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; Tab(70); Left$(Time$, 5);
    Locate 3, 1: Color 2, 0
    Color 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
    Print "              FOR TECHNICAL SUPPORT, CALL (615) 366-8088"
    Color 10
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
