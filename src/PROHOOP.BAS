'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
'$INCLUDE: 'GameRoutines.BI'


_Title "Basketball: The Pro Game - HOOPS!"
$ExeIcon:'./lhg_probb.ico'
_Icon
_AllowFullScreen


Color 7, 0
Cls

Randomize Timer

'ORIG  DATA 12,11,11,11,11,11,11,12,11,12,11,11,10,10,06,07,07,07,08,08,08,08,08,08,08,09,09,09,09,09,09,09,08

Data L,1,1,1,2,2,2,H,H,3,3,3,4,4,F
Data 60,61,60,58,58,59,59,59,59,58,58,58,57,58,57,51,51,52,49,50,44,45,42,47,43,43,42,39,38,38,35,34,34
Data 9,8,8,8,8,8,8,9,8,9,8,8,7,7,06,07,07,07,08,08,08,08,08,08,08,09,09,09,09,09,09,09,08
Data "REG M-M     ","SAGGING M-M ","TRAPPING M-M","TIGHT M-M   ","LOOSE M-M   "
Data -2,-2,-3,-4,-4,-5,-5,-6,-7
Data -4,-5,-5,-6,-7,-8,-9,-10,-11
Data 0,0,-1,-2,-3,-4,-5,-5,-6
Data -3,-3,-4,-5,-6,-6,-7,-8,-9
Data -6,-7,-8,-8,-8,-9,-10,-11,-12

For I = 0 To 14
    Read R$(I)
Next

For I = 0 To 32
    Read TOA%(I)
Next
For I = 0 To 32
    Read PFA%(I)
Next

NL% = 1
SC(1) = 30: SC(0) = 24
NB% = 5

Open "DEV.BAS" For Input As #1
Input #1, P$(0)
Input #1, P$(1)
Close 1

Open "DEFAULT" For Input As #1
Input #1, U5
Input #1, XY%
Input #1, SC
Input #1, N
Input #1, PO
Input #1, displayPause
Input #1, X4%
Input #1, CM%
Input #1, NB%
Input #1, RT%
Input #1, FR%
Input #1, PB%
Close 1

If Not _FileExists("TICKER") Then TMT = 0: GoTo 2
Open "TICKER" For Input As #2
For I = 1 To 32
    For I1 = 0 To 1
        For I2 = 1 To 14
            Input #2, SX%(I, I1, I2)
Next: Next: Next
For I = 1 To 32
    For I1 = 0 To 1
        Input #2, SX$(I, I1)
Next: Next
For I = 1 To 32
    Input #2, ST%(I)
Next
Close 2
TMT = 1

2 If Not _FileExists("GAME82") Then 6
Open "GAME82" For Input As #2
Input #2, FF$
For I = 0 To 2: Input #2, AP%(I): Next
Close 2
Open P$(0) + FF$ For Random As #1 Len = 48
For X = 0 To 18
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
Next
Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
Get #1, AP%(0) + AP%(2)
For X = 0 To 18: NG%(X) = CVI(Q$(X + 1)): Next
For X = 0 To 1: YN$(X) = Q$(20 + X): Next
VS% = CVI(Q$(22)): HS% = CVI(Q$(23))
Close 1

AP = 1

6 P7$(1) = "YES": P7$(0) = " NO"
T = 720
H = 1
T1(1) = 7: T1(0) = 7
PS$(0) = "G": PS$(1) = "G": PS$(2) = "F": PS$(3) = "F": PS$(4) = "C"

OY%(1) = 2: OY%(2) = -2: OX%(1) = 10: OX%(2) = -10
D2(0) = 1: D2(1) = 1

For I = 1 To 5
    Read defenseStyles$(I)
Next I

For I = 0 To 1
    For I1 = 0 To 4
        B1(I, I1) = -1
    Next I1
Next I

For I = 0 To 1
    For I1 = 1 To 9
        Read CF%(I, I1)
    Next I1
Next I

For I = 0 To 2
    For I1 = 1 To 9
        Read GF%(I, I1)
    Next I1
Next I

TS$(0) = "NO ":
TS$(1) = "YES"
SC$(0) = "24 SEC SHOT CLOCK"
SC$(1) = "30 SEC SHOT CLOCK"
FT$(0) = "CURRENT RULES            "
FT$(1) = "3 TO MAKE 2 / 2 TO MAKE 1"
FR$(0) = "ONLY IN BONUS OR ACT OF SHOOTING"
FR$(1) = "SHOOT ON ALL FOULS              "
O$(0) = "NORMAL      "
O$(1) = "AGGRESSIVE  "
O$(2) = "SAFE        "
U5$(0) = "HUMAN OPPONENT           "
U5$(1) = "COMPUTER IS VISITING TEAM"
U5$(2) = "COMPUTER IS HOME TEAM    "
U5$(3) = "COMPUTER VS COMPUTER     "
LC$(0) = "@ TEAM 1            "
LC$(1) = "HOME COURT ADVANTAGE"
LC$(2) = "NEUTRAL SITE        "
X$(0) = "A ": X$(1) = "B ": X$(2) = "C ": X$(3) = "D ": X$(4) = "E "
PO$(0) = "REGULAR SEASON"
PO$(1) = "PLAYOFF       "
CM$(0) = "COACH AND SELECT SHOTS           "
CM$(1) = "COACH ONLY AND LET COMPUTER SHOOT"
PB$(0) = "VERBOSE        "
PB$(1) = "HAFFNER CLASSIC"

If AP = 1 Then GoSub 1300: GoTo 51

24 Cls: Color 2: Print Tab(29); "SELECT OPENING OPTIONS": Print

25 Locate 2, 2: Color 14: Print "(1) ";: Color 15: Print U5$(U5)
Locate 3, 2: Color 14: Print "(2) ";: Color 15: Print "VISITING TEAM: ";: Color T(0, 33), T(0, 34): Print B$(0, 0): Color , 0
Locate 4, 2: Color 14: Print "(3) ";: Color 15: Print "HOME TEAM: ";: Color T(1, 33), T(1, 34): Print B$(1, 0): Color , 0

28 Locate 5, 2: Color 14: Print "(4) ";: Color 15: Print "3-POINT SHOT: "; TS$(XY%)

29 Locate 6, 2: Color 14: Print "(5) ";: Color 15: Print SC$(SC)

30 Locate 7, 2: Color 14: Print "(6) ";: Color 15: Print "LOCATION: "; LC$(N)

31 Locate 8, 2: Color 14: Print "(7) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#"; displayPause

32 Locate 9, 2: Color 14: Print "(8) ";: Color 15: Print "START GAME"
Locate 10, 2: Color 14: Print "(9) ";: Color 15: Print PO$(PO)
Locate 11, 2: Color 14: Print "(0) ";: Color 15: Print "SAVE SETTINGS"
Locate 12, 2: Color 14: Print "(A) ";: Color 15: Print "SHOW SHOT PERCENTAGES: "; P7$(X4%)
Locate 13, 2: Color 14: Print "(B) ";: Color 15: Print "HUMAN COACHES WILL: "; CM$(CM%);: Locate 1, 1
Locate 14, 2: Color 14: Print "(C) ";: Color 15: Print "BONUS AFTER FOUL # "; NB%
Locate 15, 2: Color 14: Print "(D) ";: Color 15: Print "NUMBER OF FREE THROWS "; FT$(RT%)
Locate 16, 2: Color 14: Print "(E) ";: Color 15: Print "FREE THROW RULES "; FR$(FR%)
Locate 17, 2: Color 14: Print "(F) ";: Color 15: Print "PLAY BY PLAY "; PB$(PB%)
Locate 18, 2: Color 14: Print "NOTE:: ";: Color 15: Print "BONUS WAS ON 7TH FOUL (UNTIL 72-73), 6TH FOUL (72-73 SEASON)"
Locate , 2: Print "       5TH FOUL (1973-74 TO PRESENT"
Locate , 2: Color 14: Print "NOTE:: ";: Color 15: Print "UP UNTIL 1972-73, WHEN A TEAM WAS IN THE BONUS, THEY HAD THE CHANCE"
Locate , 2: Print "       TO SHOOT 3 FT TO MAKE 2 OR 2 FT TO MAKE 1"
Locate , 2: Color 14: Print "NOTE:: ";: Color 15: Print "UP UNTIL 1972-73, ALL FOULS WERE SHOT...UNLIKE THE CURRENT RULES"
Locate , 2: Color 14: Print "NOTE:: ";: Color 15: Print "THE 3-PT SHOT WAS USED FOR ALL ABA SEASONS AND NBA SINCE 1979-80"
Locate , 2: Color 14: Print "NOTE:: ";: Color 15: Print "THE ABA USED A 30-SEC SHOT CLOCK THROUGH 1973-74";

35 Color 7
I$ = GetKeyPress$
If I$ = Chr$(27) Then Run "HELLO.EXE"
I = Val(I$)
If UCase$(I$) = "A" Then I = 11
If UCase$(I$) = "B" Then I = 12
If UCase$(I$) = "C" Then I = 13
If UCase$(I$) = "D" Then I = 14
If UCase$(I$) = "E" Then I = 15
If UCase$(I$) = "F" Then I = 16
If I < 0 Or I > 16 Then 35
If I = 16 Then PB% = 1 - PB%: GoTo 25
If I = 0 Then I = 10
On I GOTO 38, 40, 41, 42, 44, 46, 48, 50, 45, 43, 47, 33, 10, 11, 9

9 FR% = 1 - FR%: GoTo 25

10 NB% = NB% + 1
If NB% > 6 Then NB% = 4
GoTo 25

11 RT% = 1 - RT%: GoTo 25

38 U5 = U5 + 1: If U5 > 3 Then U5 = 0
GoTo 25

40 P9 = 0: GoSub 56: GoTo 24

41 P9 = 1: GoSub 56: GoTo 24

42 XY% = XY% + 1: If XY% > 1 Then XY% = 0
GoTo 28

43 Open "DEFAULT" For Output As #1
Print #1, U5: Print #1, XY%: Print #1, SC: Print #1, N: Print #1, PO: Print #1, displayPause: Print #1, X4%: Print #1, CM%
Print #1, NB%: Print #1, RT%: Print #1, FR%: Print #1, PB%: Close 1
GoTo 31

44 SC = 1 - SC: GoTo 29

45 PO = 1 - PO: GoTo 32

46 N = N + 1: If N > 2 Then N = 1
GoTo 30

47 X4% = 1 - X4%: GoTo 32

33 CM% = 1 - CM%: GoTo 32

48 Locate 8, 32: Input displayPause: If displayPause < 0 Then 48
GoTo 24

50 If B$(0, 0) = "" Or B$(1, 0) = "" Then 24
Cls: Color 3: JJ = 6: Call FRAMEBOX: Locate 10, 11
Print "IF YOU ARE USING THE 10 MINUTE TICKER OPTION FOR THIS GAME,"
Locate , 11: Print "ENTER STARTING TIME FOR GAME IN THIS FORMAT"
Locate , 11: Print "(EXAMPLES: 1:00P, 11:30A). OTHERWISE, JUST HIT ENTER": Print
Locate , 11: Input "START TIME: "; ST$
L = Len(ST$)
If L = 5 Then HR = Val(Left$(ST$, 1)): MIN = Val(Mid$(ST$, 3, 2))
If L = 6 Then HR = Val(Left$(ST$, 2)): MIN = Val(Mid$(ST$, 4, 2))
T$ = Right$(ST$, 1)
If UCase$(T$) = "P" And Left$(ST$, 2) <> "12" Then HR = HR + 12
TM = HR * 60 + MIN
TTS% = Int(TM / 30 - 18)
Call SETUPRATINGS

51 If U5 = 1 Then CT = 0: GoTo 55
If U5 = 2 Then U5 = 1: CT = 1: GoTo 55
If U5 = 3 Then U5 = 2

55 GoSub 62
GoTo 900

56 Color 15, 0: Cls
JJ = 6
SN = 1
Locate 6, 8
Color 15, 4

'Print box top
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

'Print box sides
JJ = 9
For II = 1 To JJ
    Locate 6 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next
Locate 7 + JJ, 8

'Print box bottom
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

Locate 7, 32

Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print "ABA SEASONS ARE FOLLOWED BY AN 'A' (IE. 75A)."
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 27: Input NY$

If NY$ = "" Then Call TEAMMENU(2, YN$): GoTo 57

YN$ = NY$
For I = 1 To 500: teamNames$(I) = "": teamIndex%(I) = 1: Next

Call TEAMMENU(BO%, YN$)

57 If BO% = 1 Then BO% = 0: GoTo 56

58 Locate 2, 33: Color T(P9, 33), T(P9, 34): Print B$(P9, 0): Color 15, 4: Locate , 33
Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 56
Color 15, 0
Call DISABLEPLAYERS
Return

62 For I = 0 To 1
    Cls
    64 Y$(I) = B$(I, 0)
    Q = I
    GoSub 6585
    M$ = Mid$(B$(I, 0), 4, QQ)
    R$ = M$ + "." + Left$(B$(I, 0), 2)
    R$ = R$ + "S"
    If Not _FileExists(P$(1) + R$) Then 66
    Open P$(1) + R$ For Input As #2
    Input #2, Y%
    For I1 = 1 To 100: Input #2, O%(I1): Next
    For I1 = 1 To 100: Input #2, HO$(I1): Next
    For I1 = 1 To 100: Input #2, HT%(I1): Next
    For I1 = 1 To 100: Input #2, HO$(I1): Next
    Close 2
    For I1 = 1 To Y%
        If O%(I1) > HT%(I1) Then Z5(I) = Z5(I) + 1
        If O%(I1) < HT%(I1) Then Z6(I) = Z6(I) + 1
    Next

    66 B$(I, 0) = Y$(I)
Next
Return

70 Call ROSTER
GoSub 105
If C1 = 1 Then GoSub 80: GoTo 73
For I = 0 To 4

    76 Locate I + 20, 1
    Color 2, 0: Print Using "# "; I;: Color 15: Print "ENTER "; PS$(I); " # ";
    72 I$ = GetKeyPress$
    If UCase$(I$) = "O" Or UCase$(I$) = "F" Or UCase$(I$) = "P" Then GoSub 100: GoTo 76
    J = Val(I$): GoSub 131
    If J < 0 Or J > 14 Or A(P9, J, 12) >= 6 Or AA$(P9, J, 0) = "XXX" Or B%(P9, J, 18) <> 0 Then 72
    For I2 = 0 To 4: If B1(P9, I2) = J Then 72
    Next I2
    B1(P9, I) = J: Z = J: GoSub 90: Print AA$(P9, J, 0);: Locate J + 3 - CV%, 3: Color T(P9, 33), T(P9, 34): Print Left$(AA$(P9, J, 0), 12): Color 15, 0
Next

73 Locate 25, 1: Color 12, 0: Print "# TO CHANGE (0-4) 6=NONE";
Locate 1, 1
74 I$ = GetKeyPress$
If UCase$(I$) = "O" Or UCase$(I$) = "F" Or UCase$(I$) = "P" Then GoSub 100: GoTo 74
I1 = Val(I$): If I1 = 6 Then Cls: Return
If I1 < 0 Or I1 > 4 Then 73
If X = 50 And I1 = B And P9 = P Then Locate 20, 29: Color 14, 0: Print AA$(P9, B1(P9, I1), 0); " IS AT THE FT LINE": GoTo 73
Locate 25, 1: Print "                         ";
J = B1(P9, I1): Z = J: GoSub 90: Locate J + 3 - CV%, 3: Color 15, 0: Print Left$(AA$(P9, J, 0), 12)

77 Locate I1 + 20, 1: Print "                          ";
B1(P9, I1) = -1
Locate , 1: Color 2: Print Using "# "; I1;: Color 15: Print "ENTER "; PS$(I1); " # ";
75 I$ = GetKeyPress$
If UCase$(I$) = "O" Or UCase$(I$) = "F" Or UCase$(I$) = "P" Then GoSub 100: GoTo 77
J = Val(I$): GoSub 131
If J < 0 Or J > 14 Or A(P9, J, 12) >= 6 Or B%(P9, J, 18) <> 0 Or AA$(P9, J, 0) = "XXX" Then 75
For I2 = 0 To 4: If B1(P9, I2) = J Then 75
Next I2
B1(P9, I1) = J: Z = J: GoSub 90: Print AA$(P9, J, 0);: Locate J + 3 - CV%, 3: Color T(P9, 33), T(P9, 34): Print Left$(AA$(P9, J, 0), 12): Color 15, 0
GoTo 73

80 For M = 0 To 4
    Locate M + 20, 1
    Color 2, 0: Print Using "# "; M;: Color 15, 0: Print "ENTER "; PS$(M); " # ";
    Z = B1(P9, M): GoSub 90
    Print AA$(P9, Z, 0);: Locate Z + 3 - CV%, 3: Color T(P9, 33), T(P9, 34): Print Left$(AA$(P9, Z, 0), 12): Color 15, 0
Next
Return

90 CV% = 0: For IP% = 0 To Z: If B%(P9, IP%, 18) <> 0 Then CV% = CV% + 1
Next: Return

100 If UCase$(I$) = "P" Then P7(P9) = 1 - P7(P9): GoSub 105: Return
If UCase$(I$) = "F" Then D2(P9) = D2(P9) + 1: If D2(P9) > 5 Then D2(P9) = 1
If UCase$(I$) = "O" Then O0%(P9) = O0%(P9) + 1: If O0%(P9) > 2 Then O0%(P9) = 0
GoSub 105: Return

105 Color 14: Locate 19, 29: Print "   SELECT OFFENSIVE AND DEFENSIVE STYLE"
Locate 21, 32: Color 15: Print "O";: Color 3: Print "FFENSE STYLE-";: Color 7: Print O$(O0%(P9))
Locate 22, 32: Color 3: Print "DE";: Color 15: Print "F";: Color 3: Print "ENSE STYLE-";: Color 7: Print defenseStyles$(D2(P9))
Locate 23, 32: Color 3: Print "FULL COURT ";: Color 15: Print "P";: Color 3: Print "RESS-";: Color 7: Print P7$(P7(P9))
Return

131 If UCase$(I$) = "A" Then J = 10
If UCase$(I$) = "B" Then J = 11
If UCase$(I$) = "C" Then J = 12
If UCase$(I$) = "D" Then J = 13
If UCase$(I$) = "E" Then J = 14
Return

150 GoSub 1200
Return

159 GoSub 698
Call NEWSCREEN
Return

170 T = 0
TF(0) = TF(0) + P2(0)
TF(1) = TF(1) + P2(1)
Call QUARTERTOTAL
H = H + 1
GoSub 275
XS = 1
P2(0) = 0
P2(1) = 0
X = 0

U$ = "END OF QUARTER!"
Select Case PB%
    Case 0: Color 7, 0: Call WINDEX: Print U$
    Case 1: If displayPause > 0 Then Call WINDEX: Print U$ ': Call MOREDELAY
End Select
GoSub 178

If H < 5 And U5 <> 0 Then GoSub 175
If H < 5 Then GoSub 159: B = -1: GoSub 220: T = 720: Call UPDATESCREEN: GoTo 250
GoSub 159

If H = 5 Then
    U$ = "END OF REGULATION"
    Select Case PB%
        Case 0: Color 7, 0: Call WINDEX: Print U$
        Case 1: If displayPause > 0 Then Call WINDEX: Print U$ ': Call MOREDELAY
    End Select
End If
Locate 2, 31: Print "END OF REGULATION"

If S(0, 0) = S(1, 0) Then
    U$ = "OVERTIME !!!"
    Select Case PB%
        Case 0: Color 7, 0: Call WINDEX: Print U$
        Case 1: Call WINDEX: Print U$: Call MOREDELAY
    End Select
    GoTo 190
End If

If Not (AP = 1 And U5 = 2) Then
    I$ = GetKeyPress
End If
GoTo 8400

175 If U5 = 1 Then P9 = CT: Call COMPUTERSUBS
If U5 = 2 Then P9 = 0: Call COMPUTERSUBS: P9 = 1: Call COMPUTERSUBS
Return

178 If H = 3 Then
    U$ = "HALFTIME!"
    Select Case PB%
        Case 0: Color 7, 0: Call WINDEX: Print U$
        Case 1: Call WINDEX: Print U$: Call MOREDELAY
    End Select
    GoSub 8515
End If
Return

190 T1(0) = T1(0) + 1: T1(1) = T1(1) + 1: T = 300: ZF(0) = 0: ZF(1) = 0: Call UPDATESCREEN: B = -1: GoSub 220: GoTo 250

210 M5 = 0: If T <= 2 Or IN = 99 Then M5 = 2: GoTo 214
If T > 2 And T <= 4 Then M5 = 2
If T > 4 And T <= 16 Then M5 = Int(Rnd(1) * 2) + 3
If T > 16 Then M5 = Int(Rnd(1) * 2) + 4: 'CHANGED AFTER DEF RB CHANGE FROM +4 TO +3

214 TE = M5: GoSub 365: T = T - M5: IN = 0: X8 = X8 - M5
If T <= .001 Then TE = T: GoSub 365: T = 0
If X8 <= 0 Then X8 = 0
Call UPDATESCREEN
Return

220 NL% = 1
A1$ = ""
B1$ = ""
C1$ = ""
D1$ = ""
E1$ = ""
F1$ = ""
G1$ = ""
H1$ = ""
GoSub 927
FO = 0
GoSub 4090
Locate 10
If U5 = 2 Then S2 = 0: P9 = P: C1 = 2: GoSub 698: Call RATINGTABLES: Return
Locate 10, 1: Color 15, 0
Print "<ENTER>-CONTINUE"
Print "1-SUB "; B$(P, 0)
Print "2-SUB "; B$(D, 0)
221 I$ = GetKeyPress$
P3 = Val(I$)
If P3 < 1 Or P3 > 2 Or U5 = 1 And P3 = 1 And P = CT Or U5 = 1 And P3 = 2 And D = CT Then S2 = 0: P9 = P: C1 = 2: GoSub 698: Call RATINGTABLES: GoTo 227
If P3 = 1 Then S2 = P
If P3 = 2 Then S2 = D
C1 = 1: P9 = S2: GoSub 70: GoSub 150: P9 = P: LC% = 1: GoTo 220

227 If LC% = 1 Then LC% = 0: GoSub 159
GoSub 4090: Return

230 Rem
For P9 = 0 To 1
    If U5 = 1 And P9 = CT Or U5 = 2 Then Call COMPUTERSUBS: GoTo 243
    GoSub 70
    GoSub 150

243 Next P9
Call RATINGTABLES
GoSub 159
Call RATINGTABLES

250 GoSub 850
If H < 1 And H > 5 Then 252
P = Int(Rnd(1) * 2): JB = P

252 If P = 0 Then D = 1
If P = 1 Then D = 0
GoSub 3000: GoSub 210

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: GoSub 365: T = 0: GoTo 170

GoSub 1033
If H = 1 Or H > 4 Then
    If PB% = 0 Then U$ = B$(JB, 0) + " GETS THE JUMP": Call DELAY
    If PB% = 1 And displayPause <> 0 Then Call WINDEX: Color T(JB, 33), T(JB, 34): Print B$(JB, 0);: Color 15, 0: Print " GETS THE JUMP": Call MOREDELAY
    P9 = JB
    P = JB
    GoTo 300
End If
If PB% = 0 Then U$ = B$(JB, 0) + " INBOUNDS THE BALL": Call DELAY
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Color T(JB, 33), T(JB, 34): Print B$(JB, 0);: Color 15, 0: Print " INBOUNDS THE BALL": Call MOREDELAY
P9 = JB
P = JB
GoTo 300

260 GoSub 4090
Locate 8, 1
If P7(D) = 1 Then
    If PB% = 0 Then U$ = B$(D, 0) + " IS PRESSING": Call DELAY
    If PB% = 1 And displayPause <> 0 Then Call WINDEX: Color T(D, 33), T(D, 34): Print B$(D, 0);: Color 15, 0: Print " IS PRESSING": Call MOREDELAY
    P1 = 1
End If
Return

275 '*** ADJUST TIMEOUTS ***
If H < 4 Then 281
For PG = 0 To 1
    If T1(PG) > 4 Then T1(PG) = 4
    If H > 4 Then T1(PG) = 3
Next

281 Return

300 ST = 0: FO = 0: LS = 0: XS = 0: If X8 > 0 And X8 < SC(SC) Then X8 = X8 - 5: GoTo 302
X8 = SC(SC): LS = 0: GoSub 835: If T < 6 And LT <> 4 Then LS = 4

302 D6 = 0: PASS% = 1: A4 = 0: A6 = 0: F = 0: If X <> 90 Then X = 0
If X <> 90 Then X = 0
GoTo 400

350 PASS% = PASS% + 1
P6 = 0
A4 = 0
D6 = 0
A6 = 1
F = 0
X = 0
GoTo 400

359 S = Int(Rnd(1) * 100) + 1
W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) + 1
A(P, B1(P, B), 7) = A(P, B1(P, B), 7) + 1
PT% = 3
Call MAKEPBP
Call DELAY
If S <= F3S% Then 363
If PB% = 1 And displayPause <> 0 Then
    Call WINDEX
    Print "3 PT FG MISSED BY "; AA$(P, B1(P, B), 0)
    Call MOREDELAY
End If
If PB% = 0 Then
    PT% = 5
    Call MAKEPBP
    Call DELAY
End If
GoSub 1033
T = T - 4
TE = 4
GoSub 365

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: GoSub 365: GoTo 170

Call UPDATESCREEN
GoTo 670

363 ' MADE 3-POINT FIELD GOAL
If PB% = 1 And displayPause <> 0 Then
    Call WINDEX
    Print "3 PT FG GOOD BY "; AA$(P, B1(P, B), 0); " !!!"
    Call MOREDELAY
End If
If PB% = 0 Then
    PT% = 4
    Call MAKEPBP
    Call DELAY
End If
Call MOREDELAY
S(P, H) = S(P, H) + 3
S(P, 0) = S(P, 0) + 3
W%(P, B1(P, B), 0) = W%(P, B1(P, B), 0) + 1
A(P, B1(P, B), 8) = A(P, B1(P, B), 8) + 1
GoSub 1054
Call UPDATESCREEN
GoTo 562

365 '*** INCREMENT MINUTES ***
For I3 = 0 To 1
    For I4 = 0 To 4
        ZZ(I3, B1(I3, I4)) = ZZ(I3, B1(I3, I4)) + TE
Next: Next
Return

400 A4 = 0: D6 = 0: P3 = 0: B = Int(Rnd(1) * 5)
GoSub 405
If P1 = 1 Then 428
R0 = Int(Rnd(1) * 1000) + 1
On D2(D) GOTO 461, 477, 494, 461, 477: Rem*** BRANCH TO AREA BASED ON DEFENSE ***

405 R0 = Int(Rnd(1) * 100) + 1
If U5 = 0 And CM% = 1 Or U5 = 1 And D = CT And CM% = 1 Or U5 = 1 And P = CT Or U5 = 2 Then 1040
If R0 <= 40 Then 424
B = Int(Rnd(1) * 5)
GoTo 427

424 If displayPause <> 0 And PB% = 0 Then U$ = "PASS TO CHOICE (0-4)": Call DELAY
If displayPause <> 0 And PB% = 1 Then
    If AP = 1 And U5 = 2 Or displayPause = 0 Then 425
    Call WINDEX
    Print "PASS TO CHOICE (0-4)"
    Call DELAY
End If
425 I$ = GetKeyPress$
If UCase$(I$) = "T" Then Call TICKER: GoTo 425
If UCase$(I$) = "S" Then Call SCOUTREPORT: GoTo 425
If UCase$(I$) = "Q" Then Run "HELLO.EXE"
If UCase$(I$) = "Z" Or I$ = "/" Then Call CHANGESTRATEGY(I$): GoTo 425
B = Val(I$): If B < 0 Or B > 4 Then 424

427 Return

428 P1 = 0: R0 = Int(Rnd(1) * 1000) + 1
If R0 <= 3 Then 511
If R0 <= 50 Then 512
If R0 <= 50 + F7%(P, 1) + F7%(D, 2) Then 515
P9 = Int(Rnd(1) * 5)
If R0 <= 40 + F7%(P, 1) + F7%(D, 2) + A(D, B1(D, P9), 5) + F7%(P, 0) Then 513
GoTo 465
'***BAD SHOT ***

437 A4 = 0: R0 = Int(Rnd(1) * 3) + 1
If A(P, B1(P, B), 0) > 60 And R0 = 1 Then F = 24
If A(P, B1(P, B), 0) > 60 And R0 = 2 Then F = 30
If A(P, B1(P, B), 0) > 60 And R0 = 3 Then F = 36
If A(P, B1(P, B), 0) <= 60 And R0 = 1 Then F = A(P, B1(P, B), 0) * .4
If A(P, B1(P, B), 0) <= 60 And R0 = 2 Then F = A(P, B1(P, B), 0) * .5
If A(P, B1(P, B), 0) <= 60 And R0 = 3 Then F = A(P, B1(P, B), 0) * .6
PZ% = 1
X = 0: GoTo 520

445 If H = 4 And T <= 120 Or H > 4 Then 446
If A(P, B1(P, B), 3) - A(P, B1(P, B), 7) - A(P, B1(P, B), 11) - A(P, B1(P, B), 12) <= 0 Then 437

446 '*** CHECKING 3-POINT SHOT ***
If XY% = 0 Then 448
F3S% = 0
RN = Int(Rnd(1) * 100) + 1
If RN <= A(P, B1(P, B), 19) Then PZ% = 4: F3S% = A(P, B1(P, B), 20) + F7%(D, 4): F3S% = F3S% + Int(Rnd(1) * 7) - 3: X = 0: GoTo 520
If P6 = 4 And Int(Rnd(1) * 100) + 1 <= 20 Then 437
If P6 = 4 Then PZ% = 4: X = 0: F3S% = .4 * A(P, B1(P, B), 20) + F7%(D, 4): GoTo 520
If H >= 4 And T < 30 And S(D, 0) - S(P, 0) > 3 And A(P, B1(P, B), 19) >= 3 Then PZ% = 4: X = 0: F3S% = A(P, B1(P, B), 20) + F7%(D, 4): GoTo 520

448 '*** CHECKING FOR ASSISTED SHOT ***
GoSub 920: A4 = Int(Rnd(1) * 41)
R0 = Int(Rnd(1) * F5%(P, 4)) + 1: RN = Int(.17 * F5%(P, 4) + .5)
If RN >= R0 Then F = F + A4: PZ% = 2: X = 0: GoTo 520
'*** CHECK PENETRATION SHOT ***
RN = Int(Rnd(1) * 100) + 1: A4 = Int(Rnd(1) * 50) + 1
If RN <= (B%(P, B1(P, B), 16) + OY%(O0%(P))) * 2 + 1 Then PZ% = 3: F = F + A4: X = 0: GoTo 520
'*** CHECK AVERAGE SHOT ***
I2 = B%(P, B1(P, B), 16): RN = Int(Rnd(1) * 2)
If Left$(AA$(P, B1(P, B), 1), 1) = "G" Or Left$(AA$(P, B1(P, B), 1), 1) = "F" Then 453
If RN = 1 Then 452
PZ% = 5: F = F + CF%(RN, I2): X = 0: GoTo 520

452 PZ% = 6: F = F + CF%(RN, I2): X = 0: GoTo 520

453 RN = Int(Rnd(1) * 3)
If RN = 1 Then 454
If RN = 2 Then 457
PZ% = 5: F = F + GF%(RN, I2): X = 0: GoTo 520

454 PZ% = 6: F = F + GF%(RN, I2): X = 0: GoTo 520

457 PZ% = 7: F = F + GF%(RN, I2): X = 0: GoTo 520

461 '*** SHOT VS REG/TIGHT M-M ***
If R0 <= 2 Then 511
If R0 <= 30 Then 512
If R0 <= 30 + F7%(P, 1) + F7%(D, 2) Then 515
P9 = Int(Rnd(1) * 5)
If R0 <= 24 + F7%(P, 1) + F7%(D, 2) + A(D, B1(D, P9), 5) + F7%(P, 0) + OX%(O0%(P)) Then 513

465 R0 = Int(Rnd(1) * 1000) + 1
If (PASS% <= 3 Or PASS% > 4) And R0 <= 320 + ((JY - 89) * 10) Then 445
If PASS% = 4 Then 445
GoTo 437

477 '*** SHOT VS LOOSE/SAG M-M ***
If R0 <= 1 Then 511
If R0 <= 10 Then 512
If R0 <= 10 + F7%(P, 1) + F7%(D, 2) Then 515
P9 = Int(Rnd(1) * 5)
If R0 <= 8 + F7%(P, 1) + F7%(D, 2) + A(D, B1(D, P9), 5) + F7%(P, 0) + OX%(O0%(P)) Then 513
GoTo 465

494 '*** SHOT VS TRAP M-M ***
If R0 <= 3 Then 511
If R0 <= 50 Then 512
If R0 <= 50 + F7%(P, 1) + F7%(D, 2) Then 515
P9 = Int(Rnd(1) * 5)
If R0 <= 40 + F7%(P, 1) + F7%(D, 2) + A(D, B1(D, P9), 5) + F7%(P, 0) + OX%(O0%(P)) Then 513
GoTo 465

511 X = 0: GoTo 580

512 GoSub 210: GoSub 1033: Call UPDATESCREEN: X = 88: GoTo 565

513 GoSub 210
GoSub 1033
A(P, B1(P, B), 23) = A(P, B1(P, B), 23) + 1
If displayPause = 0 Then 500
If PB% = 1 Then
    Call WINDEX
    Print "BALL STOLEN BY "; AA$(D, B1(D, P9), 0); "!"
    Call MOREDELAY
    GoTo 500
End If

RN = Int(Rnd(1) * 100) + 1
If RN <= 3 Then U$ = AA$(D, B1(D, P9), 0) + " QUICKLY REACHES IN!": Call DELAY
If RN > 3 And RN <= 6 Then U$ = AA$(D, B1(D, P9), 0) + " SNEAKS IN FROM BEHIND!": Call DELAY
If RN > 6 And RN <= 10 Then U$ = AA$(D, B1(D, P9), 0) + " GETS A HAND ON THE BALL!": Call DELAY
RN = Int(Rnd(1) * 20) + 1
If RN = 1 Then U$ = "PASS INTERCEPTED BY " + AA$(D, B1(D, P9), 0) + "!"
If RN = 2 Then U$ = "PASS IS PICKED OFF BY " + AA$(D, B1(D, P9), 0) + "!"
If RN = 3 Then U$ = "BALL IS STOLEN BY " + AA$(D, B1(D, P9), 0) + "!"
If RN = 4 Then U$ = AA$(D, B1(D, P9), 0) + " WITH THE STEAL!"
If RN = 5 Then U$ = AA$(D, B1(D, P9), 0) + " TAKES IT AWAY!"
If RN = 6 Then U$ = AA$(D, B1(D, P9), 0) + " STEALS IT!"
If RN = 7 Then U$ = AA$(D, B1(D, P9), 0) + " RIPS IT LOOSE!"
If RN = 8 Then U$ = AA$(D, B1(D, P9), 0) + " TAKES THE BALL AWAY!"
If RN = 9 Then U$ = AA$(D, B1(D, P9), 0) + " STEALS THE BALL!"
If RN = 10 Then U$ = AA$(D, B1(D, P9), 0) + " PICKS IT OFF!"
If RN = 11 Then U$ = AA$(D, B1(D, P9), 0) + " SWIPES THE BALL!"
If RN = 12 Then U$ = AA$(D, B1(D, P9), 0) + " WITH THE STOLEN BALL!"
If RN = 13 Then U$ = AA$(D, B1(D, P9), 0) + " STRIPS IT AWAY!"
If RN = 14 Then U$ = AA$(D, B1(D, P9), 0) + " WITH THE STEAL!"
If RN = 15 Then U$ = AA$(D, B1(D, P9), 0) + " SWIPES AND STEALS IT!"
If RN = 16 Then U$ = AA$(D, B1(D, P9), 0) + " SNATCHES IT AWAY!"
If RN = 17 Then U$ = AA$(D, B1(D, P9), 0) + " QUICKLY REACHES IN!"
If RN = 18 Then U$ = AA$(D, B1(D, P9), 0) + " TAKES IT THE OTHER WAY!"
If RN = 19 Then U$ = AA$(D, B1(D, P9), 0) + " POKES IT LOOSE!"
If RN = 20 Then U$ = AA$(D, B1(D, P9), 0) + " STRIPS IT CLEAN!"
Call DELAY

500 A(D, B1(D, P9), 13) = A(D, B1(D, P9), 13) + 1
T0(P) = T0(P) + 1
Call UPDATESCREEN
C1 = 2: P9 = D
' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: GoSub 365: GoTo 170
GoSub 3000: FB% = 1: GoTo 300

515 GoSub 210: GoSub 1033: A(P, B1(P, B), 23) = A(P, B1(P, B), 23) + 1: T0(P) = T0(P) + 1
If SV% <> 1 Then PT% = 1: Call MAKEPBP
SV% = 0
If PB% = 1 And displayPause <> 0 Then
    Call WINDEX
    Print "TURNOVER BY ";: Color T(P, 33), T(P, 34): Print B$(P, 0): Color 15, 0: Call MOREDELAY
End If
If PB% = 0 Then Call DELAY
U$ = "BALL OUT OF BOUNDS TO " + B$(D, 0)
GoSub 729
Call MOREDELAY
Call UPDATESCREEN
Locate 10, 1

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: GoSub 365: GoTo 170
GoSub 3000: GoSub 220: GoTo 300

520 GoSub 210

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: GoSub 365: GoTo 170

If PZ% = 4 And F3S% <= 0 Then F3S% = 3
If displayPause = 0 Then 529
If F > 70 And FB% = 1 And PASS% = 1 Then PT% = 6: PZ% = 8: Call MAKEPBP: Call DELAY: GoTo 529
If AA$(P, B1(P, B), 0) <> PB$ And PB$ = "" Then U$ = "PASS TO " + AA$(P, B1(P, B), 0): Call DELAY
If AA$(P, B1(P, B), 0) <> PB$ And PB$ <> "" Then PT% = 10: Call MAKEPBP: Call DELAY
If AA$(P, B1(P, B), 0) = PB$ Then PT% = 9: Call MAKEPBP: Call DELAY
If X8 <= 0 Then U$ = "SHOT CLOCK HAS EXPIRED!": SV% = 1: GoTo 515
If X8 < 5 Then U$ = "SHOT CLOCK DOWN TO" + Str$(X8): Call DELAY
PB$ = AA$(P, B1(P, B), 0)
Z3$ = Left$(AA$(P, B1(P, B), 1), 1)
Y2$ = "HE"
Y3$ = AA$(D, B1(D, B), 0)
On PZ% GOTO 521, 522, 523, 524, 525, 526, 527

521 If displayPause = 0 Then 601
RN = Int(Rnd(1) * 12) + 1
If RN = 1 Then U$ = Y2$ + "'S HEAVILY GUARDED"
If RN = 2 Then U$ = Y2$ + "'S GUARDED"
If RN = 3 Then U$ = Y2$ + "'S GUARDED BY " + Y3$
If RN = 4 Then U$ = Y2$ + " HAS A MAN ON HIM"
If RN = 5 Then U$ = Y2$ + " HAS A DEFENDER ON HIM"
If RN = 6 Then U$ = Y2$ + "'S COVERED"
If RN = 7 Then U$ = Y2$ + " CAN'T GET AROUND THE DEFENDER"
If RN = 8 Then U$ = Y2$ + "'S PRESSURED"
If RN = 9 Then U$ = Y2$ + " HAS NOWHERE TO GO"
If RN = 10 Then U$ = Y2$ + " HAS A HAND IN HIS FACE"
If RN = 11 Then U$ = Y2$ + "'S WATCHED BY " + Y3$
If RN = 12 Then U$ = Y2$ + "'S WORKING ON " + Y3$

601 Call DELAY
GoTo 529

522 U$ = Y2$ + " WORKS FREE INSIDE": Call DELAY: U$ = "HE HAS AN OPEN SHOT": Call DELAY: GoTo 529

523 If displayPause = 0 Then 529
RN = Int(Rnd(1) * 100) + 1
If Z3$ = "C" And RN <= 25 Then U$ = Y2$ + " MAKES A MOVE DOWN LOW": Call DELAY: GoTo 602
If Z3$ = "C" And RN <= 50 Then U$ = Y2$ + " POSITIONS HIMSELF DOWN LOW": Call DELAY: GoTo 602
If Z3$ = "C" And RN <= 75 Then U$ = Y2$ + " POSTS UP UNDERNEATH": Call DELAY: GoTo 602
If Z3$ = "C" Then U$ = Y2$ + " BACKS IN AGAINST " + AA$(D, B1(D, 4), 0): Call DELAY

602 If Z3$ = "C" Then U$ = "HE HAS A GOOD SHOT INSIDE": Call DELAY: GoTo 529
If Z3$ = "F" And RN <= 17 Then U$ = Y2$ + " DRIVES THE BASELINE": Call DELAY: GoTo 603
If Z3$ = "F" And RN <= 34 Then U$ = Y2$ + " PENETRATES THE LANE": Call DELAY: GoTo 603
If Z3$ = "F" And RN <= 50 Then U$ = Y2$ + " MOVES INSIDE": Call DELAY: GoTo 603
If Z3$ = "F" And RN <= 67 Then U$ = Y2$ + " DRIVES THE LANE": Call DELAY: GoTo 603
If Z3$ = "F" And RN <= 84 Then U$ = Y2$ + " MOVES INTO THE PAINT": Call DELAY: GoTo 603
If Z3$ = "F" Then U$ = Y2$ + " DRIVES UNDERNEATH": Call DELAY

603 If Z3$ = "F" Then U$ = "HE'S GOT A GOOD SHOT UNDERNEATH": Call DELAY: GoTo 529
If Z3$ = "G" And RN <= 25 Then U$ = Y2$ + " DRIVES THE LANE": Call DELAY: GoTo 604
If Z3$ = "G" And RN <= 50 Then U$ = Y2$ + " DRIVES THROUGH THE KEY": Call DELAY: U$ = "HE'S INTO THE LANE": Call DELAY: GoTo 604
If Z3$ = "G" And RN <= 75 Then U$ = Y2$ + " DRIVES TO THE BASKET": Call DELAY: GoTo 604
U$ = Y2$ + " DRIVES THROUGH AN OPENING": Call DELAY

604 U$ = "HE HAS A GOOD SHOT CLOSE IN": Call DELAY
GoTo 529

524 U$ = "HE'S OPEN FROM 3-POINT RANGE!": Call DELAY: GoTo 529

525 U$ = "HE'S OPEN FROM CLOSE RANGE": Call DELAY: GoTo 529

526 U$ = "HE'S OPEN FROM MEDIUM RANGE": Call DELAY: GoTo 529

527 U$ = "HE'S OPEN FROM LONG RANGE": Call DELAY: GoTo 529

529 If F <= 0 Then F = 1
If F >= 100 Then F = 100
If displayPause = 0 Then 528
If PB% = 1 Then
    Call WINDEX
    Print "PASS TO "; AA$(P, B1(P, B), 0); " OF ";
    Color T(P, 33), T(P, 34)
    Print B$(P, 0): Color 15, 0
End If

If PB% = 1 And PZ% <> 4 Then Locate 10: Print "SHOT % "; Int(F + .5);
If PB% = 1 And PZ% = 4 Then Locate 10: Print "3 PT SHOT % "; Int(F3S% + .5);: Print " *";: GoTo 528
If PB% = 1 And PZ% >= 2 And PZ% <= 8 Then Print "*";
If displayPause = 0 Or PB% = 1 Then 528
If PZ% <> 4 Then Locate 8, 41: Print Using "SHOT % ###   "; Int(F + .5);
If PZ% = 4 Then Locate 8, 41: Print Using "SHOT % ###"; Int(F3S% + .5);: Print " *";: GoTo 528
If PZ% >= 2 And PZ% <= 8 Then Locate , 52: Print "*";

528 Print: If PB% = 1 And displayPause <> 0 Then Call MOREDELAY
If U5 <> 0 Or CM% = 1 Then 949

530 I$ = GetKeyPress$
If UCase$(I$) = "T" Then Call TICKER: GoTo 530
If UCase$(I$) = "S" Then Call SCOUTREPORT: GoTo 530
If UCase$(I$) = "Q" Then Run "HELLO.EXE"
If UCase$(I$) = "Z" Or I$ = "/" Then Call CHANGESTRATEGY(I$): GoTo 530
P6 = Val(I$): If P6 < 1 Or P6 > 4 Then P6 = 0
If P6 = 4 Then FT% = 1

'Wait for keypress during Play-By-Play
532 I$ = InKey$
If UCase$(I$) = "T" Then Call TICKER
If UCase$(I$) = "Z" Or I$ = "/" Then Call CHANGESTRATEGY(I$)
If UCase$(I$) = "S" Then Call SCOUTREPORT
If UCase$(I$) = "Q" Then Run "HELLO.EXE"
If Val(I$) = 3 And (U5 = 0 Or U5 = 1 And P = CT) And H >= 4 And T < 240 Then P6 = 3
If Val(I$) = 2 And (U5 = 0 Or U5 = 1 And D = CT) And T1(P) > 0 Then P6 = 2
P3 = P6: If P6 = 2 And T1(P) = 0 Then 530
If P6 = 3 And H < 4 Or P6 = 3 And U5 = 1 And D = CT Then 530
If P6 = 3 And H = 4 And T > 240 Then 530
'
If (P6 = 1 Or P6 = 4) And PZ% = 4 Then P6 = 4
On P6 + 1 GOTO 350, 537, 640, 584, 359

537 '*** SHOOT!!! ***
PT% = 3: Call MAKEPBP: Call DELAY
S = 0
R0 = Int(Rnd(1) * 100) + 1
A(P, B1(P, B), 7) = A(P, B1(P, B), 7) + 1
If H > 3 And T <= 30 And S(D, 0) - S(P, 0) = 2 Or H > 3 And T <= 30 And S(D, 0) - S(P, 0) = 0 Then 540
If R0 <= A(P, B1(P, B), 16) + F7%(D, 3) Then A(P, B1(P, B), 7) = A(P, B1(P, B), 7) - 1: GoSub 1033: GoTo 589

540 S = Int(Rnd(1) * 100) + 1: R0 = (F - 2) * .1
If S <= R0 Then GoSub 1033: GoTo 610
If S <= F - 2 Then 553
R0 = Int(Rnd(1) * 100) + 1
GoTo 548

545 If PB% = 1 And displayPause <> 0 Then
    Call WINDEX
    Print "SHOT MISSED BY "; AA$(P, B1(P, B), 0)
    Call MOREDELAY
End If
If PB% = 0 Then PT% = 8: Call MAKEPBP: Call DELAY
GoSub 1033

546 T = T - 4
TE = 4
GoSub 365

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: GoSub 365: GoTo 170

Call UPDATESCREEN
GoTo 670

548 I5 = Int(Rnd(1) * 100) + 1
I3 = Int(Rnd(1) * 5)
If I5 > A(D, B1(D, I3), 6) Then 545
If displayPause = 0 Then 554
If PB% = 1 Then
    Call WINDEX
    Print "SHOT BLOCKED BY "; AA$(D, B1(D, I3), 0)
    Call MOREDELAY
    GoTo 554
End If
RN = Int(Rnd(1) * 100) + 1
If RN <= 10 Then U$ = AA$(D, B1(D, I3), 0) + " STUFFS THE SHOT!!": GoTo 549
If RN <= 15 Then U$ = "DENIED BY " + AA$(D, B1(D, I3), 0) + "!!": GoTo 549
If RN <= 20 Then U$ = "ERASED BY " + AA$(D, B1(D, I3), 0) + "!!": GoTo 549
If RN <= 25 Then U$ = "EXPUNGED BY " + AA$(D, B1(D, I3), 0) + "!!": GoTo 549
If RN <= 30 Then U$ = "REBUKED BY " + AA$(D, B1(D, I3), 0) + "!!": GoTo 549
If RN <= 35 Then U$ = "SHOT BLOCKED CLEANLY BY " + AA$(D, B1(D, I3), 0) + "!!": GoTo 549
If RN <= 70 Then U$ = "SHOT BLOCKED BY " + AA$(D, B1(D, I3), 0) + "!!": GoTo 549
If RN <= 90 Then U$ = "SHOT REJECTED BY " + AA$(D, B1(D, I3), 0) + "!!": GoTo 549
U$ = "SHOT SWATTED OUT BY " + AA$(D, B1(D, I3), 0) + "!!": GoTo 549

549 Call DELAY

554 A(D, B1(D, I3), 15) = A(D, B1(D, I3), 15) + 1
X = 98
GoTo 546

553 If DK% = 1 And PB% = 0 Then PT% = 2: Call MAKEPBP: U$ = "STUFF BY " + AA$(P, B1(P, B), 0) + " !!!": DK% = 0: GoTo 559
If DK% = 1 And PB% = 1 Then U$ = "STUFF BY " + AA$(P, B1(P, B), 0) + " !!!": DK% = 0: GoTo 559
If PB% = 0 And PZ% = 8 Then PT% = 7: Call MAKEPBP: Call DELAY: PZ% = 0: GoTo 560
U$ = "BASKET BY "
U$ = U$ + AA$(P, B1(P, B), 0) + " !!!"

559 If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print U$: Call MOREDELAY: GoTo 560
RN = Int(Rnd(1) * 100) + 1
If RN <= 50 Then 561
RN = Int(Rnd(1) * 22) + 1
If RN = 1 Then U$ = "NOTHING BUT THE BOTTOM OF THE NET!"
If RN = 2 Then U$ = "COUNT IT!"
If RN = 3 Then U$ = AA$(P, B1(P, B), 0) + " TICKLES THE TWINE!"
If RN = 4 Then U$ = "NO RIM, NO BOARD, JUST CORD!"
If RN = 5 Then U$ = "GOOD!"
If RN = 6 Then U$ = "YES!"
If RN = 7 Then U$ = "COUNT IT!"
If RN = 8 Then U$ = "IT'S GOOD!"
If RN = 9 Then U$ = "BINGO!"
If RN = 10 Then U$ = "IT FALLS!"
If RN = 11 Then U$ = "HE DRAINS IT!"
If RN = 12 Then U$ = "SWISH!"
If RN = 13 Then U$ = "ALL NET!"
If RN = 14 Then U$ = "IT'S THERE!"
If RN = 15 Then U$ = "HE BURIES IT!"
If RN = 16 Then U$ = "IT GOES IN!"
If RN = 17 Then U$ = "RING IT UP!"
If RN = 18 Then U$ = "IT GOES DOWN!"
If RN = 19 Then U$ = "BULLSEYE!"
If RN = 20 Then U$ = "HE SCORES!"
If RN = 21 Then U$ = "HE HAS IT!"
If RN = 22 Then U$ = "HE CONNECTS!"

561 If PB% = 0 Then Call DELAY

560 S(P, H) = S(P, H) + 2
S(P, 0) = S(P, 0) + 2
GoSub 1054
Call UPDATESCREEN
A(P, B1(P, B), 8) = A(P, B1(P, B), 8) + 1

562 U$ = B$(D, 0) + " BALL AFTER SCORE"
If PB% = 1 And displayPause <> 0 Then
    Locate 9, 1
    Color T(D, 33), T(D, 34)
    Print B$(D, 0);: Color 15, 0
    Print " BALL AFTER SCORE"
    Call MOREDELAY
End If
If PB% = 0 Then GoSub 871: Call DELAY
GoSub 1033
T = T - 4
TE = 4
GoSub 365

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: GoSub 365: GoTo 170
Call UPDATESCREEN: GoSub 3000: GoSub 810: GoSub 260: GoTo 300

565 GoSub 630

566 If P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then 570

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: GoSub 365: GoTo 170
If FR% = 0 Then
    GoSub 220
    GoSub 4090
    Locate 8, 1
    U$ = B$(P, 0) + " BALL AFTER FOUL"
    Call DELAY
    GoSub 871
    GoTo 350
End If
Call UPDATESCREEN
If displayPause = 0 Then 571
If PB% = 0 Then
    U$ = "1 FT: " + AA$(P, B1(P, B), 0) + Str$(A(P, B1(P, B), 1)) + " %"
    Call DELAY
    GoTo 571
End If
Call WINDEX: Color T(P, 33), T(P, 34): Print B$(P, 0): Color 15, 0
Print "1 FT: "; AA$(P, B1(P, B), 0); " "; A(P, B1(P, B), 1); " %"

571 X = 50
GoSub 220
X = 0
If displayPause = 0 Then 572
If PB% = 0 Then
    U$ = "1 FT: " + AA$(P, B1(P, B), 0) + Str$(A(P, B1(P, B), 1)) + " %"
    Call DELAY
    GoTo 572
End If
Call WINDEX: Color T(P, 33), T(P, 34): Print B$(P, 0): Color 15, 0
Print "1 FT: "; AA$(P, B1(P, B), 0); " "; A(P, B1(P, B), 1); " %"

572 A(P, B1(P, B), 9) = A(P, B1(P, B), 9) + 1
F3 = 1
S = Int(Rnd(1) * 100) + 1
FX% = A(P, B1(P, B), 1)
If S <= FX% And PB% = 0 Then Call THREEFT
If S <= FX% And PB% = 1 Then Call MADEFT
If S > FX% And PB% = 0 Then Call BADFT
If S > FX% And PB% = 1 Then Call MISSFT
LT = 0

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: GoSub 365: GoTo 170

If S > FX% Then 650
GoSub 3000
U$ = B$(P, 0) + " BALL AFTER SCORE"
Call SELECTPBP
GoSub 871
GoSub 810
B = 6
X = 0
GoSub 260
GoTo 300

570 If RT% = 1 And (P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4) Then
    U$ = AA$(P, B1(P, B), 0) + " GETS 3 FT TO MAKE 2..."
Else
    U$ = AA$(P, B1(P, B), 0) + " GETS 2 FREE THROWS..."
End If
Call DELAY
GoTo 591

580 GoSub 210
GoSub 1033
GoSub 3000
GoSub 630
P2(D) = P2(D) - 1
B = W5
If P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then 570

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: GoSub 365: GoTo 170

U$ = "OFFENSIVE FOUL"
If PB% = 0 Then Call DELAY
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print U$: Call MOREDELAY
U$ = B$(P, 0) + " BALL AFTER FOUL"
GoSub 729
GoSub 871: B = 6: Call DELAY
T0(D) = T0(D) + 1: Call UPDATESCREEN: GoSub 220: GoSub 260: GoTo 300

584 IN = 99: GoSub 210

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: GoSub 365: GoTo 170

C1 = 2: P9 = D
U$ = "DESIGNATED FOULER (0-4)"
Call SELECTPBP
If U5 = 1 And D = CT Or U5 = 2 Then W5 = Int(Rnd(1) * 5): GoTo 588
586 I$ = GetKeyPress$
W5 = Val(I$): If W5 < 0 Or W5 > 4 Then 586

588 GoSub 631: X8 = SC(SC): GoTo 566

589 T = T - 4: TE = 4
GoSub 365

Call UPDATESCREEN
GoSub 630
If displayPause = 0 Then 591
If PB% = 0 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then U$ = "3 TO MAKE 2 FT: " + AA$(P, B1(P, B), 0) + Str$(A(P, B1(P, B), 1)) + " %": Call DELAY: GoTo 591
If PB% = 0 Then U$ = "2 FT: " + AA$(P, B1(P, B), 0) + Str$(A(P, B1(P, B), 1)) + " %": Call DELAY: GoTo 591
If PB% = 1 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
    Call WINDEX
    Print B$(P, 0)
    Print "3 TO MAKE 2 FT: ";
    Print AA$(P, B1(P, B), 0); "   "; A(P, B1(P, B), 1); " %"
    GoTo 591
End If
If PB% = 1 Then Call WINDEX: Print B$(P, 0): Print "2 FT: "; AA$(P, B1(P, B), 0); "   "; A(P, B1(P, B), 1); " %"

591 X = 50
GoSub 220
X = 0
If displayPause = 0 Then 592
If PB% = 0 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then U$ = "3 TO MAKE 2 FT: " + AA$(P, B1(P, B), 0) + Str$(A(P, B1(P, B), 1)) + " %": Call DELAY: GoTo 592
If PB% = 0 Then U$ = "2 FT: " + AA$(P, B1(P, B), 0) + Str$(A(P, B1(P, B), 1)) + " %": Call DELAY: GoTo 592
If PB% = 1 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
    Call WINDEX
    Print B$(P, 0)
    Print "3 TO MAKE 2 FT: ";
    Print AA$(P, B1(P, B), 0); "   "; A(P, B1(P, B), 1); " %"
    GoTo 592
End If
If PB% = 1 Then Call WINDEX: Print B$(P, 0): Print "2 FT: "; AA$(P, B1(P, B), 0); "   "; A(P, B1(P, B), 1); " %"

592 Call MOREDELAY
NM% = 0
For F3 = 1 To 3
    S = Int(Rnd(1) * 100) + 1
    FX% = A(P, B1(P, B), 1)
    If S <= FX% And PB% = 0 Then NM% = NM% + 1: Call THREEFT
    If S <= FX% And PB% = 1 Then NM% = NM% + 1: Call MADEFT
    If S > FX% And PB% = 0 Then Call BADFT
    If S > FX% And PB% = 1 Then Call MISSFT
    A(P, B1(P, B), 9) = A(P, B1(P, B), 9) + 1
    If NM% = 2 Then 596
    If F3 = 2 And (RT% = 0 Or RT% = 1 And (P2(D) <= NB% And H <= 4 Or P2(D) <= 3 And H > 4)) Then 596
Next F3

' removed this before GOTO 170:
' Call UPDATESCREEN:
596 If T <= 0 Then TE = T: GoSub 365: GoTo 170

If S > A(P, B1(P, B), 1) Then 650
GoSub 3000
U$ = B$(P, 0) + " BALL AFTER SCORE"

Call SELECTPBP
GoSub 871
GoSub 810
B = 6
X = 0
GoSub 260
GoTo 300

610 T = T - 4: TE = 4: GoSub 365: Call UPDATESCREEN
If DK% = 1 And PB% = 0 Then PT% = 2: Call MAKEPBP: U$ = "STUFF BY " + AA$(P, B1(P, B), 0): Call DELAY: DK% = 0: GoTo 617
If DK% = 1 And PB% = 1 Then Call WINDEX: Print "STUFF BY " + AA$(P, B1(P, B), 0): Call MOREDELAY: DK% = 0: GoTo 617
If PZ% = 8 And PB% = 0 Then PT% = 7: Call MAKEPBP: PZ% = 0: Call DELAY: GoTo 617
If PB% = 0 Then U$ = "BASKET GOOD BY " + AA$(P, B1(P, B), 0): Call DELAY
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print "BASKET GOOD BY " + AA$(P, B1(P, B), 0): Call MOREDELAY

617 U$ = "... AND HE'S FOULED !!!": GoSub 729: F3 = 1

Call MOREDELAY
GoSub 1054
F3 = 1
S(P, H) = S(P, H) + 2
S(P, 0) = S(P, 0) + 2
A(P, B1(P, B), 8) = A(P, B1(P, B), 8) + 1

Call UPDATESCREEN
GoSub 630
If displayPause = 0 Then 620
If PB% = 0 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then U$ = "2 TO MAKE 1 FT: " + AA$(P, B1(P, B), 0) + Str$(A(P, B1(P, B), 1)) + " %": Call DELAY: GoTo 620
If PB% = 0 Then U$ = "1 FT: " + AA$(P, B1(P, B), 0) + Str$(A(P, B1(P, B), 1)) + " %": Call DELAY: GoTo 620
If PB% = 1 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
    Call WINDEX
    Print B$(P, 0)
    Print "2 TO MAKE 1 FT: ";
    Print AA$(P, B1(P, B), 0); "   "; A(P, B1(P, B), 1); " %"
    GoTo 620
End If
If PB% = 1 Then Call WINDEX: Print B$(P, 0): Print "1 FT: "; AA$(P, B1(P, B), 0); "   "; A(P, B1(P, B), 1); " %"

620 X = 50: GoSub 220: X = 0
If displayPause = 0 Then 621
If PB% = 0 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then U$ = "2 TO MAKE 1 FT: " + AA$(P, B1(P, B), 0) + Str$(A(P, B1(P, B), 1)) + " %": Call DELAY: GoTo 621
If PB% = 0 Then U$ = "1 FT: " + AA$(P, B1(P, B), 0) + Str$(A(P, B1(P, B), 1)) + " %": Call DELAY: GoTo 621
If PB% = 1 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
    Call WINDEX
    Print B$(P, 0)
    Print "2 TO MAKE 1 FT: ";
    Print AA$(P, B1(P, B), 0); "   "; A(P, B1(P, B), 1); " %"
    GoTo 621
End If
If PB% = 1 Then Call WINDEX: Print B$(P, 0): Print "1 FT: "; AA$(P, B1(P, B), 0); "   "; A(P, B1(P, B), 1); " %"

621 NM% = 0
For F3 = 1 To 2
    S = Int(Rnd(1) * 100) + 1
    FX% = A(P, B1(P, B), 1)
    If S <= FX% And PB% = 0 Then NM% = NM% + 1: Call THREEFT
    If S <= FX% And PB% = 1 Then NM% = NM% + 1: Call MADEFT
    If S > FX% And PB% = 0 Then Call BADFT
    If S > FX% And PB% = 1 Then Call MISSFT
    A(P, B1(P, B), 9) = A(P, B1(P, B), 9) + 1
    If NM% = 1 Then 624
    If F3 = 1 And (RT% = 0 Or RT% = 1 And (P2(D) <= NB% And H <= 4 Or P2(D) <= 3 And H > 4)) Then 624
Next F3

' removed this before GOTO 170:
' Call UPDATESCREEN:
624 If T <= 0 Then TE = T: GoSub 365: GoTo 170

X = 99: If S > FX% Then 670
GoSub 3000
U$ = B$(P, 0) + " BALL AFTER SCORE"
Call SELECTPBP
GoSub 871
GoSub 810
B = 6
X = 0
GoSub 260
GoTo 300

630 GoSub 1065

631 A(D, B1(D, W5), 12) = A(D, B1(D, W5), 12) + 1
P2(D) = P2(D) + 1
Call UPDATESCREEN
If A(D, B1(D, W5), 3) - A(D, B1(D, W5), 7) - A(D, B1(D, W5), 11) - A(D, B1(D, W5), 12) = 0 Then A(D, B1(D, W5), 3) = A(D, B1(D, W5), 3) + 1
If A(D, B1(D, W5), 12) < 6 Then GoSub 638
If A(D, B1(D, W5), 12) >= 6 Then GoSub 1000: GoSub 635
Return

635 If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print "FOUL ON "; AA$(D, B1(D, W5), 0): Call MOREDELAY
If PB% = 0 Then U$ = "FOUL CALLED ON " + AA$(D, B1(D, W5), 0): Call DELAY
U$ = "HE HAS JUST FOULED OUT !!!"
GoSub 729
Call MOREDELAY
C9 = 9
If P2(D) > NB% Then X = 50
P3 = 1
If U5 = 0 Or U5 = 1 And D = 1 - CT Then P9 = D: C1 = 1: GoSub 70: GoSub 150: GoSub 220: Call NEWSCREEN
GoSub 698
X = 0
Return

638 If PB% = 1 And displayPause <> 0 Then
    Call WINDEX
    Print "FOUL ON "; AA$(D, B1(D, W5), 0)
    Call MOREDELAY
    Print "HE HAS "; A(D, B1(D, W5), 12)
    Call MOREDELAY
End If
If PB% = 0 Then U$ = "FOUL NUMBER" + Str$(A(D, B1(D, W5), 12)) + " CALLED ON " + AA$(D, B1(D, W5), 0): Call DELAY
Call MOREDELAY
Return

640 If T1(P) <= 0 Then 648
T1(P) = T1(P) - 1
T = T + M5: TE = -M5: GoSub 365
X8 = X8 + M5
U$ = "TIMEOUT " + B$(P, 0):
Call DELAY
Call MOREDELAY
GoSub 220

648 If A6 = 0 Then GoSub 260: GoTo 300
GoTo 350

650 A6 = 1: P6 = 0: A4 = 0: D6 = 0: GoSub 1033: S = 0
R0 = Int(Rnd(1) * 100) + 1
If H > 3 And T < 30 And S(D, 0) - S(P, 0) = 2 And R0 <= 12 Or H > 3 And T < 30 And S(D, 0) - S(P, 0) = 0 And R0 <= 12 Then GoSub 790: GoTo 760
R0 = Int(Rnd(1) * 100) + 1
If R0 <= 11 Then FF% = 1: GoTo 707
If R0 <= 19 Then GoSub 790: GoTo 760
GoSub 795: GoTo 800

670 A6 = 1: P6 = 0: A4 = 0: D6 = 0: GoSub 1033: GoTo 700

698 If T <= 0 Then TE = T: GoSub 365 ': T = 0: Call UPDATESCREEN
Return

700 S = 0: A4 = 0: D6 = 0: R0 = Int(Rnd(1) * 100) + 1
GG = S(D, 0) - S(P, 0)
If R0 <= 12 And H > 3 And T < 30 And (GG = 2 Or GG = 0) Then 700
If R0 <= 1 Then 725
If R0 <= 11 Then FF% = 1: GoTo 707
If R0 <= 19 Then 750
If R0 <= 22 Then 730
If R0 <= 23 And P6 <> 4 Then 740

707 GoTo 770

710 If displayPause = 0 Then 713
If PB% = 1 Then Call WINDEX: Print AA$(D, B1(D, B), 0); " HAS THE DEF REBOUND FOR ": Print B$(D, 0); " AND IS FOULED !!!": Call MOREDELAY: GoTo 713
U$ = AA$(D, B1(D, B), 0) + " HAS THE DEFENSIVE REBOUND": Call DELAY
U$ = "HE'S FOULED !!!": Call DELAY
Call MOREDELAY

713 R3(D) = R3(D) + 1: A(D, B1(D, B), 11) = A(D, B1(D, B), 11) + 1: Call UPDATESCREEN: GoSub 3000: GoTo 565

720 U$ = AA$(P, B1(P, B), 0) + " HAS THE OFFENSIVE REBOUND"
If PB% = 0 Then Call DELAY
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print U$: Print "FOR "; B$(P, 0): Call MOREDELAY
U$ = "HE'S FOULED !!!"
GoSub 729
Call MOREDELAY
A(P, B1(P, B), 24) = A(P, B1(P, B), 24) + 1
A(P, B1(P, B), 11) = A(P, B1(P, B), 11) + 1
R3(P) = R3(P) + 1
Call UPDATESCREEN
GoSub 630
GoTo 570

725 If X = 98 Then 700
T0(P) = T0(P) + 1
U$ = "OFFENSIVE GOAL TENDING"
If PB% = 0 Then Call DELAY
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print U$: Call MOREDELAY
U$ = "BALL GOES TO " + B$(D, 0)
GoSub 729
Call MOREDELAY
GoSub 3000: GoSub 220: GoTo 300

730 If X = 99 Then 700
GoSub 790
A(P, B1(P, B), 7) = A(P, B1(P, B), 7) + 1
A(P, B1(P, B), 8) = A(P, B1(P, B), 8) + 1
A(P, B1(P, B), 11) = A(P, B1(P, B), 11) + 1
R3(P) = R3(P) + 1
S(P, H) = S(P, H) + 2
S(P, 0) = S(P, 0) + 2
Call UPDATESCREEN
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print "TIPPED IN BY "; AA$(P, B1(P, B), 0); "!!!": Call MOREDELAY
If PB% = 0 Then U$ = AA$(P, B1(P, B), 0) + " TIPS IT IN!!!": Call DELAY
Call MOREDELAY
GoSub 3000
U$ = B$(P, 0) + " BALL AFTER SCORE": GoSub 871
GoSub 729
GoSub 810: GoSub 260
GoTo 300

740 If X = 99 Then 700
U$ = "DEFENSIVE GOAL TENDING"
If PB% = 0 Then Call DELAY
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print U$: Call MOREDELAY
Call MOREDELAY: GoTo 560

750 R0 = Int(Rnd(1) * 100) + 1: If R0 <= 32 Then 752
R3(D) = R3(D) + 1
Call UPDATESCREEN
U$ = "RB KNOCKED O.B. TO " + B$(D, 0)
If PB% = 0 Then Call DELAY
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print U$: Call MOREDELAY
Call MOREDELAY
GoSub 220: GoSub 3000: C1 = 2: P9 = P: GoTo 300

752 R3(P) = R3(P) + 1
Call UPDATESCREEN
U$ = "RB KNOCKED O.B. TO " + B$(P, 0)
If PB% = 0 Then Call DELAY
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print U$: Call MOREDELAY
Call MOREDELAY
GoSub 220: GoTo 520

760 If FF% = 1 Then FF% = 0: GoTo 720

761 A(P, B1(P, B), 24) = A(P, B1(P, B), 24) + 1
A(P, B1(P, B), 11) = A(P, B1(P, B), 11) + 1
R3(P) = R3(P) + 1: Call UPDATESCREEN
U$ = AA$(P, B1(P, B), 0) + " HAS THE OFFENSIVE REBOUND"
If PB% = 0 And displayPause <> 0 Then Call DELAY
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print U$: Print "FOR " + B$(P, 0): Call MOREDELAY

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: GoSub 365: GoTo 170

PB$ = AA$(P, B1(P, B), 0)
R0 = Int(Rnd(1) * 10) + 1: GoSub 920
If H > 4 Or H = 4 And T <= 120 Then 765
If A(P, B1(P, B), 3) - A(P, B1(P, B), 7) - A(P, B1(P, B), 11) - A(P, B1(P, B), 12) <= 0 Then F = F * .6

765 If R0 <= 8 Then PZ% = 9: GoTo 537
GoTo 520

770 RB = 0
For I = 0 To 9: RB(I) = 0: Next
For I = 0 To 4
    If H > 4 Or H = 4 And T <= 120 Then 773
    If A(P, B1(P, I), 3) - A(P, B1(P, I), 7) - A(P, B1(P, I), 11) - A(P, B1(P, I), 12) <= 0 Then RB(I) = RB(I) + A(P, B1(P, I), 21) * .65: RB = RB + A(P, B1(P, I), 21) * .65: GoTo 774

    773 RB(I) = RB(I) + A(P, B1(P, I), 21): RB = RB + A(P, B1(P, I), 21)

774 Next
For I = 0 To 4
    If H > 4 Or H = 4 And T <= 120 Then 778
    If A(1 - P, B1(1 - P, I), 3) - A(1 - P, B1(1 - P, I), 7) - A(1 - P, B1(1 - P, I), 11) - A(1 - P, B1(1 - P, I), 12) <= 0 Then RB(I + 5) = RB(I + 5) + A(1 - P, B1(1 - P, I), 2) * .5: RB = RB + A(1 - P, B1(1 - P, I), 2) * .5: GoTo 779

    778 RB(I + 5) = RB(I + 5) + A(1 - P, B1(1 - P, I), 2): RB = RB + A(1 - P, B1(1 - P, I), 2)

779 Next
RN = Rnd(1) * RB
For B = 0 To 9
    If RN <= RB(B) Then 786
    RB(B + 1) = RB(B + 1) + RB(B)
Next
B = Int(Rnd(1) * 10)

786 If B <= 4 Then 761
B = B - 5: GoTo 800

790 RB = 0: For I = 0 To 4: RB(I) = 0
    XZ = A(P, B1(P, I), 21)
    If H > 2 Or H = 2 And T <= 120 Then 793
    If A(P, B1(P, I), 3) - A(P, B1(P, I), 7) - A(P, B1(P, I), 11) - A(P, B1(P, I), 12) <= 0 Then XZ = XZ * .5

    793 RB(I) = RB(I) + XZ: RB = RB + XZ
Next
RN = Rnd(1) * RB
For B = 0 To 4
    If RN <= RB(B) Then 792
    RB(B + 1) = RB(B + 1) + RB(B)
Next
B = Int(Rnd(1) * 5)

792 Return

795 RB = 0
For I = 0 To 4
    RB(I) = 0
    XZ = A(1 - P, B1(1 - P, I), 2)
    If H > 2 Or H = 2 And T <= 120 Then 798
    If A(1 - P, B1(1 - P, I), 3) - A(1 - P, B1(1 - P, I), 7) - A(1 - P, B1(1 - P, I), 11) - A(1 - P, B1(1 - P, I), 12) <= 0 Then XZ = XZ * .5

    798 RB(I) = RB(I) + XZ: RB = RB + XZ
Next
RN = Rnd(1) * RB
For B = 0 To 4
    If RN <= RB(B) Then 804
    RB(B + 1) = RB(B + 1) + RB(B)
Next
B = Int(Rnd(1) * 5)

804 Return

800 If FF% = 1 Then FF% = 0: GoTo 710
A(D, B1(D, B), 11) = A(D, B1(D, B), 11) + 1
R3(D) = R3(D) + 1: Call UPDATESCREEN
U$ = AA$(D, B1(D, B), 0) + " HAS THE DEFENSIVE REBOUND"
If PB% = 0 And displayPause <> 0 Then Call DELAY
If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print U$: Print "FOR " + B$(D, 0): Call MOREDELAY
PB$ = AA$(D, B1(D, B), 0)
C1 = 2: P9 = D: GoSub 3000: FB% = 1: GoTo 300

810 NL% = 1: A1$ = "": B1$ = "": C1$ = "": D1$ = "": E1$ = "": F1$ = "": G1$ = "": H1$ = ""
If U5 <> 0 Then GoSub 927
If U5 = 2 Or U5 = 1 And P = CT Then S2 = 0: P9 = P: C1 = 2: GoSub 698: Return
GoSub 4090: Locate 10
Color 15, 0
Print "<ENTER>-CONTINUE"
Print "1-TIMEOUT "; B$(P, 0); "-"; T1(P)
813 I$ = GetKeyPress$
P3 = Val(I$): If P3 <> 1 Then S2 = 0: P9 = P: C1 = 2: GoSub 698: GoTo 818
If P3 = 1 And T1(P) <= 0 Or P3 = 1 And P = CT And U5 = 1 Then 813
If P3 = 1 Then T1(P) = T1(P) - 1: Color 15, 0: GoSub 220

818 GoSub 4090: Locate 10: Return

835 If P = 0 Then D = 1: Return
If P = 1 Then D = 0: Return

850 If H = 2 And JB = 0 Then JB = 1: Return
If H = 2 And JB = 1 Then JB = 0
If H = 4 And JB = 0 Then JB = 1: Return
If H = 4 And JB = 1 Then JB = 0
Return

871 Locate 8, 41: Print "            ": Return

900 If AP = 1 Then N = NG%(2)
X8 = SC(SC)
C7 = 1
LS = 0
If T < 6 And LT = 4 Then LS = 4
GoTo 230

920 '*** GET SHOT PCT ***
F = A(P, B1(P, B), 0) + D1(D)
If N = P Then F = F + 2
If N <> P And N <> 2 Then F = F - 2
Return

927 If U5 = 2 Then P9 = 0: GoSub 930: P9 = 1: GoSub 930
If U5 = 1 Then P9 = CT: GoSub 930
Return

930 M1 = 0: M2 = 0: M3 = 0: M4 = 0
If U5 = 1 And P9 = CT And X = 50 Or U5 = 2 And P9 = P And X = 50 Then 939
For I1 = 0 To 14
    M6 = A(P9, I1, 3) - A(P9, I1, 7) - A(P9, I1, 11) - A(P9, I1, 12)
    If H < 4 And M6 <= 0 Or H = 4 And T > 120 And M6 <= 0 Then M1 = M1 + 1
    If AA$(P9, I1, 0) = "XXX" Or A(P9, I1, 12) >= 6 Or B%(P9, I1, 18) <> 0 Or H = 1 And A(P9, I1, 12) >= 2 Or H = 2 And A(P9, I1, 12) >= 4 Or H = 3 And A(P9, I1, 12) >= 5 Or H = 4 And T > 300 And A(P9, I1, 12) = 5 Then M2 = M2 + 1
    If I1 > 4 Then 937
    M6 = A(P9, B1(P9, I1), 3) - A(P9, B1(P9, I1), 7) - A(P9, B1(P9, I1), 11) - A(P9, B1(P9, I1), 12)
    If M6 <= 0 And H < 4 Or M6 <= 0 And H = 4 And T > 120 Then M3 = M3 + 1
    IF AA$(P9, B1(P9, I1), 0) = "XXX" OR A(P9, B1(P9, I1), 12) >= 6 OR B%(P9, B1(P9, I1), 18) <> 0 OR H = 1 AND A(P9, B1(P9, I1), 12) >= 2 OR H = 2 AND A(P9, B1(P9, I1), 12) >= 4 OR H = 3 AND A(P9, B1(P9, I1), 12) >= 5 OR H = 4 AND T > 300 AND A(P9, B1(P9, I1), 12) = 5 THEN M4 = M4 + 1

937 Next I1
If M3 >= 1 And 5 - M3 < 15 - M1 Or H = 4 And T < 120 Or H > 4 Or M4 >= 1 And M2 < 14 Then Call COMPUTERSUBS

939 If H = 4 And T <= 120 Then G9%(P9) = 1
Return

949 If H >= 4 And T <= 240 And S(P, 0) - S(D, 0) > 0 And U5 = 1 And P = CT Then 985
If U5 = 2 Or U5 = 1 And D = CT Then 980

951 If T <= 1 Or X8 <= 6 Then P6 = 1: GoTo 532
If T <= 8 And S(P, 0) - S(D, 0) <= -3 And H >= 4 Then P6 = 1: GoTo 532
If T <= 20 And S(P, 0) - S(D, 0) <= -5 And H >= 4 Then P6 = 1: GoTo 532
If PZ% > 1 Then P6 = 1: GoTo 532
P6 = 0: GoTo 532

980 If T <= 240 And H >= 4 And S(P, 0) - S(D, 0) > 10 Then 983
'IF T<=120 AND H>=4 AND S(P,0)-S(D,0)>4 THEN P6=3:GOTO 532
If T <= 30 And H >= 4 And S(P, 0) - S(D, 0) > 0 Then P6 = 3: GoTo 532

983 If (U5 = 1 Or U5 = 0) And CM% <> 1 Then 530
GoTo 951

985 U$ = Str$(A(P, B1(P, B), 1)) + " FT%  INTENTIONAL FOUL (YN)"
986 I$ = GetKeyPress$
If UCase$(I$) = "Y" Or I$ = "3" Then P6 = 3: GoTo 532
GoTo 951

1000 M9 = 0
For I3 = 0 To 14
    If I3 = B1(D, W5) Or AA$(D, I3, 1) = "XXX" Or B%(D, I3, 18) <> 0 Then 1002
    M9 = M9 + 1

1002 Next
For I3 = 0 To 14
    If I3 = B1(D, W5) Or AA$(D, I3, 1) = "XXX" Or B%(D, I3, 18) <> 0 Then 1003
    If A(D, I3, 0) <= A(D, B1(D, W5), 0) Then A(D, I3, 3) = A(D, I3, 3) + Int((A(D, B1(D, W5), 3) / M9) + .5)

1003 Next I3
Return

1033 X8 = SC(SC): LS = 4: Return

1040 CNT% = 0

1041 CNT% = CNT% + 1
If CNT% = 4 Then 427
M9 = 0
B = Int(Rnd(1) * 5)
X = Int(Rnd(1) * 100) + 1
If X <= 30 Then 1046
For I = 0 To 4
    M8 = Int(B%(P, B1(P, I), 2) / B%(P, B1(P, I), 0) + .5) - A(P, B1(P, I), 7)
    If M8 <= 0 Then M8 = 0
    If M8 <= 0 And A(P, B1(P, I), 3) - A(P, B1(P, I), 7) - A(P, B1(P, I), 11) - A(P, B1(P, I), 12) > 0 Then M8 = A(P, B1(P, I), 3) - A(P, B1(P, I), 7) - A(P, B1(P, I), 11) - A(P, B1(P, I), 12)
    If M8 >= M9 Then B = I: M9 = M8
Next I

1046 GoTo 1041

1054 'CREDIT ASSISTS
R0 = Int(Rnd(1) * 100) + 1
If R0 > F7%(P, 7) Then 1061

1055 R0 = Int(Rnd(1) * F5%(P, 2)) + 1
W5 = 0
For M8 = 0 To 4
    W5 = W5 + A(P, B1(P, M8), 25)
    If R0 <= W5 Then 1059
Next M8
M8 = Int(Rnd(1) * 5)

1059 W5 = M8: Locate 7, 25
If AA$(P, B1(P, B), 0) <> AA$(P, B1(P, M8), 0) Then
    A(P, B1(P, M8), 14) = A(P, B1(P, M8), 14) + 1
    GoTo 1061
End If
GoTo 1055

1061 Return

1065 W5 = 0: M9 = Int(Rnd(1) * F5%(D, 0)) + 1
For M8 = 0 To 4: W5 = W5 + A(D, B1(D, M8), 17)
    If M9 <= W5 Then 1069
Next M8

1069 If M8 < 0 Or M8 > 4 Then M8 = Int(Rnd(1) * 5)
W5 = M8: Return
If U5 = 1 And P = CT And H <= 3 And T < 30 And J < 3 Then GoSub 1080
If U5 = 2 And H <= 3 And T < 30 And J < 3 Then GoSub 1080
If U5 = 1 And P = CT And H >= 4 And T <= 120 And J >= 3 And J <= 9 Then GoSub 1080
If U5 = 2 And H >= 4 And T <= 120 And J >= 3 And J <= 9 Then GoSub 1080
Return

1080 For J = 0 To 3
    J1 = Int(Rnd(1) * 5)
    If A(P, B1(P, J1), 20) > 0 Then B = J1: GoTo 1084
Next J

1084 Return

1200 Cls: A = 0: S2 = 1: S3 = 1: SW = 0: For ZZ = 0 To 4
    If A(P9, B1(P9, ZZ), 22) = 50 Then A = A + 1: If SW = 0 Then Print: Print "THE FOLLOWING PLAYER(S) ARE PLAYING SAFE BUT CAN NOW PLAY NORMAL IF YOU WISH": SW = 1
    If A(P9, B1(P9, ZZ), 22) = 50 And SW = 1 And S2 < 6 Then Locate S2 + 3, 1: Print S2; "-"; AA$(P9, B1(P9, ZZ), 0): P5(A) = S2: P4(A) = ZZ: S2 = S2 + 1: S3 = S3 + 1
Next ZZ: If SW = 0 Then 1214
A = 1: Print: Print "Z - CONTINUE"
1206 I$ = GetKeyPress$
If UCase$(I$) = "Z" And MJ = 0 Then 1214
If UCase$(I$) = "Z" And MJ = 25 Then 1230
P6 = Val(I$): If P6 < 1 Or P6 > (S2 + 1) Then 1206
A = P6
If P6 = P5(P6) Then
    A(P9, B1(P9, P4(A)), 17) = A(P9, B1(P9, P4(A)), 17) * 3
    A(P9, B1(P9, P4(A)), 22) = 0
    Locate P5(A) + 3, 20
    Print AA$(P9, B1(P9, P4(A)), 0); " WILL NOW PLAY NORMAL DEFENSE!"
    Call RATINGTABLES
    D1(P9) = D1(P9) - 1
    A = A + 1
    MJ = 25
    GoTo 1206
End If

1214 A = 0: S2 = 1: For ZZ = 0 To 4
    If H <= 2 And A(P9, B1(P9, ZZ), 12) >= 3 And A(P9, B1(P9, ZZ), 22) = 0 Or A(P9, B1(P9, ZZ), 12) = 5 And A(P9, B1(P9, ZZ), 22) = 0 Then
        A = A + 1
        If S2 = 1 Then
            Cls
            Print
            Print "THE FOLLOWING PLAYER(S) CAN BE MADE TO PLAY SAFE IF YOU WISH"
            Print
        End If
    End If
    If A = 0 And ZZ > 3 Then 1230
    If S2 = A And S2 < 6 Then Print S2; "- "; AA$(P9, B1(P9, ZZ), 0): P5(A) = S2: P4(A) = ZZ: S2 = S2 + 1: S3 = S3 + 1
Next ZZ
A = 1: Print: Print "Z - CONTINUE"
1220 I$ = GetKeyPress$
If UCase$(I$) = "Z" Then 1230
P6 = Val(I$): If P6 < 1 Or P6 > (S2 + 1) Then 1220
A = P6
If P6 = P5f(P6) Then
    A(P9, B1(P9, P4(A)), 17) = Int((A(P9, B1(P9, P4(A)), 17) + 1) / 3)
    A(P9, B1(P9, P4(A)), 22) = 50
    Locate P5(A) + 3, 20
    Print AA$(P9, B1(P9, P4(A)), 0); " WILL NOW PLAY SAFE!"
    Call RATINGTABLES
    D1(P9) = D1(P9) + 1
    A = A + 1
    S3 = S3 + 1
    GoTo 1220
End If

1230 Cls: MJ = 0
FO = 99: If H = 1 And T = 720 Then FO = 0
Return

1250 If H <> 3 Then Return
For I = 0 To 1: For I1 = 0 To 14
    If A(I, I1, 22) = 50 Then A(I, I1, 22) = 0: A(I, I1, 17) = A(I, I1, 17) * 3: D1(I) = D1(I) - 1: MJ = 50
    Next I1
    If MJ = 50 Then MJ = 0: F5%(I, 0) = 0: For I1 = 0 To 4: F5%(I, 0) = F5%(I, 0) + A(I, B1(I, I1), 17): Next I1
Next I
Return

1300 XY% = NG%(3)
U5 = NG%(8)
PO = NG%(5)
SC = NG%(7)
N = NG%(2)
displayPause = NG%(4) / 10
CM% = NG%(9)
NB% = NG%(14): FR% = NG%(11): RT% = NG%(10)
For P9 = 0 To 1
    I1 = NG%(P9)
    YN$ = YN$(P9)
    Call GETATEAM(I1, YN$)
Next P9
Call SETUPRATINGS
Return
    

3000 PB$ = "": If P = 1 Then P = 0: D = 1: P9 = 0: C1 = 2: Return
If P = 0 Then P = 1: D = 0: P9 = 1: C1 = 2: Return

4090 If displayPause = 0 Then 4091
For I = 8 To 15
    Locate I, 1
    Print "                                      "
Next I

4091 Return

6585 For XX = 4 To 11: If Mid$(B$(Q, 0), XX, 1) = " " Or Mid$(B$(Q, 0), XX, 1) = "." Then Mid$(B$(Q, 0), XX, 1) = "_"
Next XX
For XX = 12 To 14: If Mid$(B$(Q, 0), XX, 2) = "ST" Then Mid$(B$(Q, 0), 9, 1) = "_": Mid$(B$(Q, 0), 10, 2) = "ST"
Next XX
QQ = 8: For XX = 11 To 4 Step -1: If Mid$(B$(Q, 0), XX, 1) = "_" Then QQ = XX - 4: GoTo 6615
    GoTo 6620

6615 Next XX

6620 Return

8400 'CHECK SPECIAL EVENTS
Call RECORDSCHECK
Call EVENTS(COUNT, SPECIAL)
If SPECIAL = 1 Then
    NM$ = "BOX" + Mid$(Str$(COUNT), 2, Len(Str$(COUNT))) + ".SPC"
    Call BOX2SPC(NM$)
End If

8500 Cls
Color 14: Print "(0) ";: Color 15: Print "STATS TO SCREEN": Print
Color 14: Print "(1) ";: Color 15: Print "STATS TO PRINTER": Print
Color 14: Print "(2) ";: Color 15: Print "STATS TO TEXT FILE"
If AP = 1 And U5 = 2 Then X = 2: GoTo 8680
8651 I$ = GetKeyPress$
X = Val(I$)

8680 If X = 1 Then Call BOX2PRN: GoTo 8532
If X = 2 Then Call BOX2FILE: GoTo 8532
Cls
If N <> 2 Then Color T(N, 33), T(N, 34): Print "@ "; B$(N, 4): Print: Color 14, 0
For I = 0 To 1: Print B$(I, 0); Tab(17); Str$(S(I, 1)); Tab(20); Str$(S(I, 2)); Tab(23); Str$(S(I, 3)); Tab(26); Str$(S(I, 4));
    If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then Print Tab(29); Str$(S(I, 5));
    If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then Print Tab(29); "- "; S(I, 0): GoTo 8513
    If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then Print Tab(32); Str$(S(I, 6));
    If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then Print Tab(32); "- "; S(I, 0): GoTo 8513
    Print Tab(35); "- "; S(I, 0): GoTo 8513

8513 Next I

8515 Call BOGUS(X)

If X = 0 Then Cls
For I = 0 To 1: Color T(I, 33), T(I, 34)
    Print B$(I, 0): Color 14, 0
    Print "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB  RB  PT  F   A   B   S  TO"
    For I1 = 0 To 14
        If ZZ(I, I1) = 0 Then 8529
        Color 15: Print AA$(I, I1, 0); Tab(14);
        If ZZ(I, I1) <> 0 Then
            Print Using "##"; Int(ZZ(I, I1) / 60);
            Print ":";
            If ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60 > 9 Then
                Print Using "##"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;
            Else
                Print "0";
                Print Using "#"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;
            End If
        End If
        Locate , 21
        Print Using "##"; A(I, I1, 8);
        Print "-";
        Print Using "##"; A(I, I1, 7);
        Print Using "  ##"; W%(I, I1, 0);
        Print "-";
        Print Using "##"; W%(I, I1, 1);
        Print Using "  ##"; A(I, I1, 10);
        Print "-";: Print Using "##"; A(I, I1, 9);
        Print Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; A(I, I1, 24); A(I, I1, 11); A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0); A(I, I1, 12); A(I, I1, 14); A(I, I1, 15); A(I, I1, 13); A(I, I1, 23);
        If ZZ(I, I1) = 0 Then Locate , 15: Print "DNP" Else Print
        V9(0) = V9(0) + A(I, I1, 8)
        V9(1) = V9(1) + A(I, I1, 7)
        V9(2) = V9(2) + A(I, I1, 10)
        V9(3) = V9(3) + A(I, I1, 9)
        V9(4) = V9(4) + A(I, I1, 11)
        V9(5) = V9(5) + (A(I, I1, 8) * 2) + A(I, I1, 10) + W%(I, I1, 0)
        V9(13) = V9(13) + W%(I, I1, 0)
        V9(14) = V9(14) + W%(I, I1, 1)
        V9(6) = TF(I): 'V9(6)+A(I,I1,12)
        V9(7) = V9(7) + A(I, I1, 14): V9(8) = V9(8) + A(I, I1, 15): V9(9) = V9(9) + A(I, I1, 13): V9(11) = V9(11) + A(I, I1, 24): V9(12) = V9(12) + A(I, I1, 23)
        V9(10) = V9(10) + ZZ(I, I1)

    8529 Next I1: If V9(3) = 0 Then V9(3) = 1
    Print: Color 14: Print "TOTALS";: Color 15: Print Tab(14); Int(V9(10) / 60);: Locate , 21: Print Using "##"; V9(0);: Print "-";: Print Using "###"; V9(1);
    Locate , 28: Print Using "##"; V9(13);: Print "-";: Print Using "##"; V9(14);
    Locate , 35: Print Using "##"; V9(2);: Print "-";: Print Using "##"; V9(3);
    Locate , 42: Print Using "##  ## ### ##  ##  ##  ##  ##"; V9(11); V9(4); V9(5); V9(6); V9(7); V9(8); V9(9); V9(12)
    Print Tab(22); Int(((V9(0) / V9(1)) * 100) + .5); "%"; Tab(35); Int(((V9(2) / V9(3)) * 100) + .5); "%": Print
    Color 14: Print "TOTAL TURNOVERS -";: Color 15: Print T0(I)
    Color 14: Print "TOTAL REBOUNDS  -";: Color 15: Print R3(I)
    For I2 = 0 To 14: V9(I2) = 0: Next I2
    Call BOGUS(X)
    If X = 0 Then Cls
Next I

If H >= 4 Then EG% = 1: Call TICKER: EG% = 0
If H = 3 Then HT% = 1: Call TICKER: HT% = 0
If H = 3 Then GoSub 1250: Return

8532 X = 0
Cls
J = 1
Locate 8, 8
Color T(1, 33), T(1, 34)

'Print box top
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

'Print box sides
For II = 1 To JJ
    Locate 8 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next

'Print box bottom
Locate 9 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

Locate 9, 32
Print "DISPLAY STATS AGAIN (YN)";
Color 15, 0
If AP = 1 And U5 = 2 Then 8535

8533 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 8500

8535 Call COMPILESTATS

Cls
J = 1
Locate 8, 8
Color T(1, 33), T(1, 34)

'Print box top
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

'Print box sides
For II = 1 To JJ
    Locate 8 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next

'Print box bottom
Locate 9 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

Locate 9, 32
Print "ANOTHER GAME (YN)";
Color 15, 0
If AP = 1 Then 8637

8632 I$ = GetKeyPress$
If UCase$(I$) <> "Y" Then Run "HELLO.EXE"

8637 Cls: Print "PLEASE WAIT ...": Run "PROHOOP.EXE"

729 If PB% = 0 Then Call DELAY
If PB% = 1 And displayPause <> 0 Then Print U$: Call MOREDELAY
Return


'$INCLUDE: 'GameRoutines.BM'
'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
