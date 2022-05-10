$Debug
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Basketball: The Pro Game - HOOPS!"
$ExeIcon:'./lhg_bbpro.ico'
_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB ADJFOUL ()
DECLARE SUB BADFT ()
DECLARE SUB BOGUS (X)
DECLARE SUB BOX2FILE ()
DECLARE SUB BOX2PRN ()
DECLARE SUB BOX2SPC (NM$)
DECLARE SUB CHANGESTRATEGY (I$)
DECLARE SUB COMPILESTATS ()
DECLARE SUB COMPUTERCOACH ()
DECLARE SUB COMPUTERSUBS ()
DECLARE SUB DELAY ()
DECLARE SUB DISABLEPLAYERS ()
DECLARE SUB DRAWBOX (I)
DECLARE SUB EVENTS (COUNT, SPECIAL)
DECLARE SUB FRAMEBOX ()
DECLARE SUB GETATEAM (I1, YN$)
DECLARE SUB MADEFT ()
DECLARE SUB MAKEPBP ()
DECLARE SUB MISSFT ()
DECLARE SUB MOREDELAY ()
DECLARE SUB NEWSCREEN ()
DECLARE SUB QUARTERTOTAL ()
DECLARE SUB RATINGTABLES ()
DECLARE SUB RECORDSCHECK ()
DECLARE SUB ROSTER ()
DECLARE SUB SAVESTATFILES ()
DECLARE SUB SCOUTREPORT ()
DECLARE SUB SELECTPBP ()
DECLARE SUB SETUPRATINGS ()
DECLARE SUB TEAMMENU (BO%, YN$)
DECLARE SUB THREEFT ()
DECLARE SUB TICKER ()
DECLARE SUB TICKERUPDATE ()
DECLARE SUB UPDATESCREEN ()
DECLARE SUB WINDEX ()


Dim Shared AD, AP, B, CT, D, F3, H, JJ, JY, N, NT
Dim Shared P, P7, P9, PO, QQ, QR, SC, T, T1, TE, TMT
Dim Shared X, X1, X8, U5, VG, VH

Dim Shared A(1, 14, 25) As Single, B1(1, 4), C1(1, 14)
Dim Shared D1(1), D2(1), D8(6, 6), F1(14), G4(14), G5(14)
Dim Shared P2(1), P4(5), P5(5), P7(1), QQ(1, 8, 14, 14), QR(1, 7, 14)
Dim Shared R3(1), RB(9), S(1, 14), S3(14), SC(1)
Dim Shared T(1, 34), T0(1), T1(1), TF(1)
Dim Shared V9(14), VG(9), VH(9), W0(1, 14), W1(1, 14), W2(1, 14), W3(1, 14)
Dim Shared X$(4), X7(1), Z5(1), Z6(1), ZF(1), ZZ(2, 14)


Dim Shared displayPause As Single, FTF As Single, GP As Single
Dim Shared M9(1) As Single, MPG As Single, REC(50, 2) As Single
Dim Shared TG As Single, Z(15) As Single, Z1(15) As Single, Z2(14, 14) As Single

Dim Shared A1$, B1$, C1$, D$, D1$, E1$, F1$, FF$, G1$, H1$, O$
Dim Shared P7$, PB$, PS$, TS$, U$, Y2$, Z3$

Dim Shared AA$(1, 14, 1), B$(3, 5), CM$(1), D$(5), FR$(1), FT$(1)
Dim Shared H$(100), HO$(100), LC$(2), O$(2), P$(1), PO$(1), P7$(1), PB$(1), PS$(4)
Dim Shared Q$(380), R$(14), RC$(50, 4), SC$(1), SX$(32, 1)
Dim Shared TS$(1), U5$(3), V$(500), Y$(1), YN$(3)

Dim Shared CF%, CM%, DK%, EG%, FB%, FR%, FT%, GF%, HH%, HT%
Dim Shared NB%, NL%, NM%, OX%, OY%, PB%, PT%, PZ%, RT%
Dim Shared TS%, TTS%, X4%, XY%

Dim Shared AP%(2), B%(1, 14, 19), CF%(1, 9), E%(13)
Dim Shared F%(1, 9), F5%(1, 4), F7%(1, 9), G9%(1), GF%(2, 9)
Dim Shared HT%(100), L%(1, 6), NG%(18)
Dim Shared N0%(2, 2, 4), OX%(2), OY%(2), O%(100), O0%(1)
Dim Shared PF%(1), PFA%(33), ST%(32), SX%(32, 1, 14)
Dim Shared TOA%(33), TOF%(1), V%(500), W%(1, 14, 1)


Clear
Color 7, 0
Cls

Randomize Timer

For I = 0 To 14
    Read R$(I)
Next

'ORIG  DATA 12,11,11,11,11,11,11,12,11,12,11,11,10,10,06,07,07,07,08,08,08,08,08,08,08,09,09,09,09,09,09,09,08

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

For I = 1 To 5: Read D$(I): Next I

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
For I = 1 To 500: V$(I) = "": V%(I) = 1: Next

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
Locate 22, 32: Color 3: Print "DE";: Color 15: Print "F";: Color 3: Print "ENSE STYLE-";: Color 7: Print D$(D2(P9))
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
    IF AA$(P9, B1(P9, I1), 0) = "XXX" OR A(P9, B1(P9, I1), 12) >= 6 OR B%(P9, B1(P9, I1), 18) <> 0 OR H = 1 AND A(P9, B1(P9, I1), 12) >= 2 OR H = 2 AND A(P9, B1(P9, I1), 12) >= 4 OR H = 3 AND A(P9, B1(P9, I1), 12) >= 5 OR H = 4 AND T > 300 AND A(P9 _
, B1(P9, I1), 12) = 5 THEN M4 = M4 + 1

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
If P6 = P5(P6) Then
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

Data L,1,1,1,2,2,2,H,H,3,3,3,4,4,F
Data 60,61,60,58,58,59,59,59,59,58,58,58,57,58,57,51,51,52,49,50,44,45,42,47,43,43,42,39,38,38,35,34,34
Data 9,8,8,8,8,8,8,9,8,9,8,8,7,7,06,07,07,07,08,08,08,08,08,08,08,09,09,09,09,09,09,09,08
Data "REG M-M     ","SAGGING M-M ","TRAPPING M-M","TIGHT M-M   ","LOOSE M-M   "
Data -2,-2,-3,-4,-4,-5,-5,-6,-7
Data -4,-5,-5,-6,-7,-8,-9,-10,-11
Data 0,0,-1,-2,-3,-4,-5,-5,-6
Data -3,-3,-4,-5,-6,-6,-7,-8,-9
Data -6,-7,-8,-8,-8,-9,-10,-11,-12


'------------------------------
'      BOGUS Subroutine
'------------------------------
' ...explanation...
Sub BOGUS (X)
    If X = 0 Then
        If Not (AP = 1 And U5 = 2) Then
            I$ = GetKeyPress
        End If
    End If
End Sub


'------------------------------
'      BOX2FILE Subroutine
'------------------------------
' ...explanation...
Sub BOX2FILE ()
    If AP = 1 Then
        NM$ = "BOX" + Mid$(Str$(AP%(0) + AP%(2)), 2, Len(Str$(AP%(0) + AP%(2))))
        GoTo 8951
    End If

    Cls
    Input "ENTER FILENAME TO SAVE GAME SUMMARY UNDER "; NM$: NM$ = NM$ + ".SPC"

    8951 Open P$(1) + NM$ For Output As #2
    If N <> 2 Then Print #2, "@ "; B$(N, 4): Print #2,

    For I = 0 To 1
        Print #2, B$(I, 0); Tab(17); Str$(S(I, 1)); Tab(20); Str$(S(I, 2)); Tab(23); Str$(S(I, 3)); Tab(26); Str$(S(I, 4));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then Print #2, Tab(29); Str$(S(I, 5));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then Print #2, Tab(29); "- "; S(I, 0): _Continue
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then Print #2, Tab(32); Str$(S(I, 6));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then Print #2, Tab(32); "- "; S(I, 0): _Continue
        Print #2, Tab(35); "- "; S(I, 0)
    Next I

    For I = 0 To 1
        Print #2,: Print #2, B$(I, 0)
        Print #2, "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB  RB  PT  F   A   B   S  TO"
        For I1 = 0 To 14
            If ZZ(I, I1) = 0 Then _Continue
            Print #2, AA$(I, I1, 0); Tab(14);
            If ZZ(I, I1) = 0 Then
                Print #2, Tab(15); "DNP";
                GoTo 8968
            End If
            Print #2, Using "##"; Int(ZZ(I, I1) / 60);: Print #2, ":";
            If ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60 > 9 Then
                Print #2, Using "##"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;
            Else
                Print #2, "0";: Print #2, Using "#"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;
            End If

            8968 Print #2, Tab(21);: Print #2, Using "##"; A(I, I1, 8);
            Print #2, "-";: Print #2, Using "##"; A(I, I1, 7);
            Print #2, Using "  ##"; W%(I, I1, 0);: Print #2, "-";
            Print #2, Using "##"; W%(I, I1, 1);
            Print #2, Using "  ##"; A(I, I1, 10);
            Print #2, "-";: Print #2, Using "##"; A(I, I1, 9);
            Print #2, Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; A(I, I1, 24); A(I, I1, 11); A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0); A(I, I1, 12); A(I, I1, 14); A(I, I1, 15); A(I, I1, 13); A(I, I1, 23)
            V9(0) = V9(0) + A(I, I1, 8)
            V9(1) = V9(1) + A(I, I1, 7)
            V9(2) = V9(2) + A(I, I1, 10)
            V9(3) = V9(3) + A(I, I1, 9)
            V9(4) = V9(4) + A(I, I1, 11)
            V9(5) = V9(5) + (A(I, I1, 8) * 2) + A(I, I1, 10) + W%(I, I1, 0)
            V9(13) = V9(13) + W%(I, I1, 0)
            V9(14) = V9(14) + W%(I, I1, 1)
            V9(6) = TF(I)
            V9(7) = V9(7) + A(I, I1, 14)
            V9(8) = V9(8) + A(I, I1, 15)
            V9(9) = V9(9) + A(I, I1, 13)
            V9(11) = V9(11) + A(I, I1, 24)
            V9(12) = V9(12) + A(I, I1, 23)
            V9(10) = V9(10) + ZZ(I, I1)

        Next I1

        If V9(3) = 0 Then V9(3) = 1
        Print #2,: Print #2, "TOTALS"; Tab(14); Int(V9(10) / 60); Tab(21);
        Print #2, Using "##"; V9(0);: Print #2, "-";: Print #2, Using "###"; V9(1);
        Print #2, Tab(28); Using "##"; V9(13);: Print #2, "-";: Print #2, Using "##"; V9(14);
        Print #2, Tab(35); Using "##"; V9(2);: Print #2, "-";: Print #2, Using "##"; V9(3);
        Print #2, Tab(42); Using "##  ## ### ##  ##  ##  ##  ##"; V9(11); V9(4); V9(5); V9(6); V9(7); V9(8); V9(9); V9(12)
        Print #2, Tab(22); Int(((V9(0) / V9(1)) * 100) + .5); "%"; Tab(35); Int(((V9(2) / V9(3)) * 100) + .5); "%"
        Print #2,
        Print #2, "TOTAL TURNOVERS -"; T0(I)
        Print #2, "TOTAL REBOUNDS  -"; R3(I)

        For I2 = 0 To 14
            V9(I2) = 0
        Next I2

        If X = 0 Then
            If AP = 1 And U5 = 2 Then
                If X = 0 Then Cls
                I$ = GetKeyPress$
            End If

            If X = 0 Then Cls
        End If
    Next I

    Print #2, Chr$(12)
    Close 2
End Sub


'------------------------------
'      BOX2PRN Subroutine
'------------------------------
' ...explanation...
Sub BOX2PRN ()
    If N <> 2 Then LPrint "@ "; B$(N, 4): LPrint
    For I = 0 To 1
        LPrint B$(I, 0); Tab(17); Str$(S(I, 1)); Tab(20); Str$(S(I, 2)); Tab(23); Str$(S(I, 3)); Tab(26); Str$(S(I, 4));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then LPrint Tab(29); Str$(S(I, 5));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then LPrint Tab(29); "- "; S(I, 0): GoTo 8913
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then LPrint Tab(32); Str$(S(I, 6));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then LPrint Tab(32); "- "; S(I, 0): GoTo 8913
        LPrint Tab(35); "- "; S(I, 0)

    8913 Next I
    Call BOGUS(X)
    If X = 0 Then Cls
    For I = 0 To 1
        LPrint: LPrint B$(I, 0)
        LPrint "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB  RB  PT  F   A   B   S  TO"
        For I1 = 0 To 14
            If ZZ(I, I1) = 0 Then 8920
            LPrint AA$(I, I1, 0); Tab(14);
            If ZZ(I, I1) = 0 Then
                LPrint Tab(15); "DNP";
                GoTo 8918
            End If
            LPrint Using "##"; Int(ZZ(I, I1) / 60);
            LPrint ":";
            If ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60 > 9 Then
                LPrint Using "##"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;
            Else
                LPrint "0";: LPrint Using "#"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;
            End If

            8918 LPrint Tab(21);
            LPrint Using "##"; A(I, I1, 8);
            LPrint "-";
            LPrint Using "##"; A(I, I1, 7);
            LPrint Using "  ##"; W%(I, I1, 0);
            LPrint "-";: LPrint Using "##"; W%(I, I1, 1);
            LPrint Using "  ##"; A(I, I1, 10);
            LPrint "-";: LPrint Using "##"; A(I, I1, 9);
            LPrint Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; A(I, I1, 24); A(I, I1, 11); A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0); A(I, I1, 12); A(I, I1, 14); A(I, I1, 15); A(I, I1, 13); A(I, I1, 23)
            V9(0) = V9(0) + A(I, I1, 8)
            V9(1) = V9(1) + A(I, I1, 7)
            V9(2) = V9(2) + A(I, I1, 10)
            V9(3) = V9(3) + A(I, I1, 9)
            V9(4) = V9(4) + A(I, I1, 11)
            V9(5) = V9(5) + (A(I, I1, 8) * 2) + A(I, I1, 10) + W%(I, I1, 0)
            V9(13) = V9(13) + W%(I, I1, 0)
            V9(14) = V9(14) + W%(I, I1, 1)
            V9(6) = TF(I)
            V9(7) = V9(7) + A(I, I1, 14)
            V9(8) = V9(8) + A(I, I1, 15)
            V9(9) = V9(9) + A(I, I1, 13)
            V9(11) = V9(11) + A(I, I1, 24)
            V9(12) = V9(12) + A(I, I1, 23)
            V9(10) = V9(10) + ZZ(I, I1)

        8920 Next I1
        If V9(3) = 0 Then V9(3) = 1
        LPrint
        LPrint "TOTALS"; Tab(14); Int(V9(10) / 60); Tab(21);
        LPrint Using "##"; V9(0);
        LPrint "-";: LPrint Using "###"; V9(1);
        LPrint Tab(28); Using "##"; V9(13);
        LPrint "-";: LPrint Using "##"; V9(14);
        LPrint Tab(35); Using "##"; V9(2);
        LPrint "-";: LPrint Using "##"; V9(3);
        LPrint Tab(42); Using "##  ## ### ##  ##  ##  ##  ##"; V9(11); V9(4); V9(5); V9(6); V9(7); V9(8); V9(9); V9(12)
        LPrint Tab(22); Int(((V9(0) / V9(1)) * 100) + .5); "%"; Tab(35); Int(((V9(2) / V9(3)) * 100) + .5); "%"
        LPrint
        LPrint "TOTAL TURNOVERS -"; T0(I): LPrint "TOTAL REBOUNDS  -"; R3(I)
        For I2 = 0 To 14
            V9(I2) = 0
        Next I2
        Call BOGUS(X)
        If X = 0 Then Cls
    Next I
    LPrint Chr$(12)
End Sub


'------------------------------
'      BOX2SPC Subroutine
'------------------------------
' ...explanation...
Sub BOX2SPC (NM$)
    Open P$(1) + NM$ For Output As #2
    If H - 1 > E%(1) Then Print #2, Using "MORE THAN ## QUARTERS"; E%(1)
    If S(1, 0) + S(0, 0) > E%(2) Then Print #2, Using "MORE THAN ### COMBINED POINTS"; E%(2)
    If R3(0) + R3(1) > E%(3) Then Print #2, Using "MORE THAN ### COMBINED REBOUNDS"; E%(3)
    If S(0, 0) > E%(4) Then Print #2, B$(0, 0);: Print #2, Using " SCORED MORE THAN ### POINTS"; E%(4)
    If S(1, 0) > E%(4) Then Print #2, B$(1, 0);: Print #2, Using " SCORED MORE THAN ### POINTS"; E%(4)
    If R3(0) > E%(5) Then Print #2, B$(0, 0);: Print #2, Using " HAD MORE THAN ### REBOUNDS"; E%(5)
    If R3(1) > E%(5) Then Print #2, B$(1, 0);: Print #2, Using " HAD MORE THAN ### REBOUNDS"; E%(5)
    For I = 0 To 1
        For I1 = 0 To 14
            If A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) > E%(6) Then Print #2, AA$(I, I1, 0);: Print #2, Using " SCORED MORE THAN ### POINTS"; E%(6)
            If A(I, I1, 11) > E%(7) Then Print #2, AA$(I, I1, 0);: Print #2, Using " HAD MORE THAN ### REBOUNDS"; E%(7)
            If A(I, I1, 14) > E%(8) Then Print #2, AA$(I, I1, 0);: Print #2, Using " HAD MORE THAN ### ASSISTS"; E%(8)
            If A(I, I1, 15) > E%(9) Then Print #2, AA$(I, I1, 0);: Print #2, Using " BLOCKED MORE THAN ### SHOTS"; E%(9)
            If A(I, I1, 13) > E%(10) Then Print #2, AA$(I, I1, 0);: Print #2, Using " HAD MORE THAN ### STEALS"; E%(10)
            If W%(I, I1, 0) > E%(11) Then Print #2, AA$(I, I1, 0);: Print #2, Using " MADE MORE THAN ### 3-PT FIELD GOALS"; E%(11)
        Next
    Next
    Print #2,
    If N = 2 Then Print #2, "NEUTRAL SITE": Print #2,
    If N <> 2 Then Print #2, "@ "; B$(N, 4): Print #2,
    For I = 0 To 1
        Print #2, B$(I, 0); Tab(17); Str$(S(I, 1)); Tab(20); Str$(S(I, 2)); Tab(23); Str$(S(I, 3)); Tab(26); Str$(S(I, 4));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then Print #2, Tab(29); Str$(S(I, 5));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then Print #2, Tab(29); "- "; S(I, 0): GoTo 8863
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then Print #2, Tab(32); Str$(S(I, 6));
        If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then Print #2, Tab(32); "- "; S(I, 0): GoTo 8863
        Print #2, Tab(35); "- "; S(I, 0)

    8863 Next I
    For I = 0 To 1
        Print #2,: Print #2, B$(I, 0)
        Print #2, "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB  RB  PT  F   A   B   S  TO"
        For I1 = 0 To 14
            If ZZ(I, I1) = 0 Then 8870
            Print #2, AA$(I, I1, 0); Tab(14);
            If ZZ(I, I1) = 0 Then
                Print #2, Tab(15); "DNP";
                GoTo 8868
            End If
            Print #2, Using "##"; Int(ZZ(I, I1) / 60);: Print #2, ":";
            If ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60 > 9 Then
                Print #2, Using "##"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;
            Else
                Print #2, "0";: Print #2, Using "#"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;
            End If

            8868 Print #2, Tab(21);: Print #2, Using "##"; A(I, I1, 8);
            Print #2, "-";: Print #2, Using "##"; A(I, I1, 7);
            Print #2, Using "  ##"; W%(I, I1, 0);: Print #2, "-";
            Print #2, Using "##"; W%(I, I1, 1);
            Print #2, Using "  ##"; A(I, I1, 10);
            Print #2, "-";: Print #2, Using "##"; A(I, I1, 9);
            Print #2, Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; A(I, I1, 24); A(I, I1, 11); A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0); A(I, I1, 12); A(I, I1, 14); A(I, I1, 15); A(I, I1, 13); A(I, I1, 23)
            V9(0) = V9(0) + A(I, I1, 8)
            V9(1) = V9(1) + A(I, I1, 7)
            V9(2) = V9(2) + A(I, I1, 10)
            V9(3) = V9(3) + A(I, I1, 9)
            V9(4) = V9(4) + A(I, I1, 11)
            V9(5) = V9(5) + (A(I, I1, 8) * 2) + A(I, I1, 10) + W%(I, I1, 0)
            V9(13) = V9(13) + W%(I, I1, 0)
            V9(14) = V9(14) + W%(I, I1, 1)
            V9(6) = TF(I)
            V9(7) = V9(7) + A(I, I1, 14)
            V9(8) = V9(8) + A(I, I1, 15)
            V9(9) = V9(9) + A(I, I1, 13)
            V9(11) = V9(11) + A(I, I1, 24)
            V9(12) = V9(12) + A(I, I1, 23)
            V9(10) = V9(10) + ZZ(I, I1)

        8870 Next I1
        If V9(3) = 0 Then V9(3) = 1
        Print #2,: Print #2, "TOTALS"; Tab(14); Int(V9(10) / 60); Tab(21);
        Print #2, Using "##"; V9(0);: Print #2, "-";: Print #2, Using "###"; V9(1);
        Print #2, Tab(28); Using "##"; V9(13);: Print #2, "-";: Print #2, Using "##"; V9(14);
        Print #2, Tab(35); Using "##"; V9(2);: Print #2, "-";: Print #2, Using "##"; V9(3);
        Print #2, Tab(42); Using "##  ## ### ##  ##  ##  ##  ##"; V9(11); V9(4); V9(5); V9(6); V9(7); V9(8); V9(9); V9(12)
        Print #2, Tab(22); Int(((V9(0) / V9(1)) * 100) + .5); "%"; Tab(35); Int(((V9(2) / V9(3)) * 100) + .5); "%"
        Print #2,
        Print #2, "TOTAL TURNOVERS -"; T0(I)
        Print #2, "TOTAL REBOUNDS  -"; R3(I)
        For I2 = 0 To 14
            V9(I2) = 0
        Next I2

        8871 If X = 0 Then
            If AP = 1 And U5 = 2 Then
                GoTo 8872
                I$ = GetKeyPress$
            End If

            8872 If X = 0 Then Cls
        End If
    Next I
    Print #2, Chr$(12)
    Close 2
End Sub


'------------------------------
'    COMPILESTATS Subroutine
'------------------------------
' ...explanation...
Sub COMPILESTATS ()
    Cls: J = 1
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

    Locate 9, 28
    Print "SAVE STATS FOR EITHER TEAM (YN)";
    Color 15, 0
    If AP = 1 Then 9506
    9501 I$ = GetKeyPress$
    If UCase$(I$) <> "Y" Then 9507
    Cls: Color 12: Print Tab(30); "COMPILATION OF STATS": Print
    Color 14: Print "(0) ";: Color 15: Print "BOTH TEAMS": Print
    Color 14: Print "(1) ";: Color T(0, 33), T(0, 34): Print B$(0, 0); " ONLY": Color , 0: Print
    Color 14: Print "(2) ";: Color T(1, 33), T(1, 34): Print B$(1, 0); " ONLY": Color , 0: Print
    9502 I$ = GetKeyPress$
    X1 = Val(I$)
    If X1 < 0 Or X1 > 2 Then 9502

    9506 Call SAVESTATFILES

9507 End Sub


'------------------------------
'   DISABLEPLAYERS Subroutine
'------------------------------
' ...explanation...
Sub DISABLEPLAYERS ()
    Cls: Print "DISABLE UP TO 6 PLAYERS FOR ";: Color T(P9, 33), T(P9, 34): Print B$(P9, 0);: Color 15, 0: Print " (YN)";
    Print: Print
    Print "NOTE:: THE COMPUTER WILL AUTOMATICALLY DISABLE PLAYERS BASED ON"
    Print "       THE RATIO OF GAMES PLAYED FOR EACH PLAYER TO THE TOTAL"
    Print "       NUMBER OF GAMES PLAYED BY THE TEAM. YOU MAY WANT TO CHECK"
    Print "       TO ACTIVATE ANY COMPUTER DEACTIVATED PLAYERS"
    9202 I$ = GetKeyPress$
    If UCase$(I$) <> "Y" Then 9210
    DX% = 0: Cls: Print "TEAM ROSTER FOR ";: Color T(P9, 33), T(P9, 34): Print B$(P9, 0): Color 15, 0
    Print "# NAME            GP"
    For I = 0 To 14
        If I <= 9 Then Print Using "#"; I;
        If I = 10 Then Print "A";
        If I = 11 Then Print "B";
        If I = 12 Then Print "C";
        If I = 13 Then Print "D";
        If I = 14 Then Print "E";
        If AA$(P9, I, 0) = "XXX" Then B%(P9, I, 18) = -1: Color T(P9, 33), T(P9, 34)
        Print " ";
        If B%(P9, I, 18) <> 0 Then Color T(P9, 33), T(P9, 34)
        Print AA$(P9, I, 0);: Color 15, 0: Print Tab(18); B%(P9, I, 0)
    Next

    9203 Locate 19, 1: Print "ENTER PLAYER # TO DISABLE/ENABLE (Z=FINISHED)"
    9205 I$ = GetKeyPress$
    If UCase$(I$) = "Z" Then 9210
    I1 = Val(I$)
    If UCase$(I$) = "A" Then I1 = 10
    If UCase$(I$) = "B" Then I1 = 11
    If UCase$(I$) = "C" Then I1 = 12
    If UCase$(I$) = "D" Then I1 = 13
    If UCase$(I$) = "E" Then I1 = 14
    If B%(P9, I1, 18) = 0 And DX% < 6 Then B%(P9, I1, 18) = -1: DX% = DX% + 1: Locate 3 + I1, 3: Color T(P9, 33), T(P9, 34): Print AA$(P9, I1, 0): Color 15, 0: GoTo 9203
    If B%(P9, I1, 18) = -1 Then B%(P9, I1, 18) = 0: DX% = DX% - 1: Locate 3 + I1, 3: Color 15, 0: Print AA$(P9, I1, 0): GoTo 9203
    GoTo 9203

9210 End Sub


'------------------------------
'      DRAWBOX Subroutine
'------------------------------
' ...explanation...
Sub DRAWBOX (I)
    JJ = 1
    Locate 6, 8: Color T(I, 33), T(I, 34)
    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
    For II = 1 To JJ: Locate 6 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
    Locate 7 + JJ, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7

End Sub


'------------------------------
'      GETATEAM Subroutine
'------------------------------
' ...explanation...
Sub GETATEAM (I1, YN$)
    Open P$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031
    Field #1, 15 As Q$(0), 15 As Q$(1)
    For J = 0 To 6
        Field #1, 30 + J * 2 As X$, 2 As Q$(J + 2)
    Next
    Field #1, 44 As X$, 2 As Q$(9)
    For J = 0 To 34
        Field #1, 46 + J * 2 As X$, 2 As Q$(J + 10)
    Next
    For J = 0 To 14
        Field #1, 116 + J * 57 As X$, 15 As Q$(J * 22 + 45), 2 As Q$(J * 22 + 46)
        For J1 = 0 To 19
            Field #1, 133 + J * 57 + J1 * 2 As X$, 2 As Q$(J * 22 + 47 + J1)
        Next
    Next
    Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)
    Get #1, I1
    B$(P9, 0) = Q$(0)
    B$(P9, 1) = Q$(1)
    For I = 0 To 6
        L%(P9, I) = CVI(Q$(I + 2))
    Next
    For I = 0 To 34
        T(P9, I) = CVI(Q$(I + 10))
    Next
    For I = 0 To 14
        AA$(P9, I, 0) = Q$(22 * I + 45)
        AA$(P9, I, 1) = Q$(22 * I + 46)
        For I1 = 0 To 19
            B%(P9, I, I1) = CVI(Q$(22 * I + 47 + I1))
        Next
    Next
    B$(P9, 2) = Q$(375)
    B$(P9, 3) = Q$(376)
    B$(P9, 4) = Q$(377)
    Close 1
    For I = 0 To 14

        9004 If Right$(AA$(P9, I, 0), 1) = " " Then AA$(P9, I, 0) = Left$(AA$(P9, I, 0), Len(AA$(P9, I, 0)) - 1): GoTo 9004

        9005 If Right$(AA$(P9, I, 1), 1) = " " Then AA$(P9, I, 1) = Left$(AA$(P9, I, 1), Len(AA$(P9, I, 1)) - 1): GoTo 9005
    Next

    9002 If Right$(B$(P9, 0), 1) = " " Then B$(P9, 0) = Left$(B$(P9, 0), Len(B$(P9, 0)) - 1): GoTo 9002

    9006 If Right$(B$(P9, 2), 1) = " " Then B$(P9, 2) = Left$(B$(P9, 2), Len(B$(P9, 2)) - 1): GoTo 9006

    9007 If Right$(B$(P9, 3), 1) = " " Then B$(P9, 3) = Left$(B$(P9, 3), Len(B$(P9, 3)) - 1): GoTo 9007

    9008 If Right$(B$(P9, 4), 1) = " " Then B$(P9, 4) = Left$(B$(P9, 4), Len(B$(P9, 4)) - 1): GoTo 9008
    For I = 0 To 14
        If B%(P9, I, 16) >= 0 Or B%(P9, I, 17) >= 0 Then 9009
        GP = B%(P9, I, 16) / B%(P9, I, 0)
        MPG = B%(P9, I, 17) / B%(P9, I, 1)
        For I1 = 2 To 14
            B%(P9, I, I1) = B%(P9, I, I1) * GP * MPG
            B%(P9, I, 0) = B%(P9, I, 16) * -1: B%(P9, I, 1) = B%(P9, I, 17) * -1
        Next I1

    9009 Next I
    If U5 = 0 Or U5 = 1 And P9 <> CT Or AP = 1 And U5 = 0 Or AP = 1 And U5 = 1 And P9 <> CT Then 9020

    9010 CNT% = 0
    For I = 0 To 14
        If B%(P9, I, 18) = -1 Then B%(P9, I, 18) = 0
        RN = Int(Rnd(1) * 100) + 1
        If UCase$(AA$(P9, I, 0)) = "XXX" Or (B%(P9, I, 18) = 0 And RN > Int(B%(P9, I, 0) / L%(P9, 0) * 100 + .5)) Then B%(P9, I, 18) = -1
    Next
    For I = 0 To 14: If B%(P9, I, 18) = 0 Then CNT% = CNT% + 1
    Next
    If CNT% < 10 Then 9010

9020 End Sub


'------------------------------
'      NEWSCREEN Subroutine
'------------------------------
' ...explanation...
Sub NEWSCREEN ()
    '*** SCREEN UPDATING ***
    Cls
    Call TICKERUPDATE
    Color 9, 0
    Locate 3, 11: Print Chr$(218); String$(5, 196); Chr$(191)
    Locate , 11: Print Chr$(179);: Locate , 17: Print Chr$(179)
    Locate , 11: Print Chr$(192); String$(5, 196); Chr$(217)
    Locate 3, 58: Print Chr$(218); String$(5, 196); Chr$(191)
    Locate , 58: Print Chr$(179);: Locate , 64: Print Chr$(179)
    Locate , 58: Print Chr$(192); String$(5, 196); Chr$(217)
    Locate 2, 1: Locate , 9: Color T(0, 33), T(0, 34): Print B$(0, 0);
    Locate 1, 37: Color 15, 0: Print Using "##"; Int(T / 60);: Print ":";
    If T - Int(T / 60) * 60 < 10 Then
        Print "0";
        Print Using "#"; T - Int(T / 60) * 60;
        GoTo 2505
    End If
    Print Using "##"; T - Int(T / 60) * 60;

    2505 Locate 2, 56: Color T(1, 33), T(1, 34): Print B$(1, 0)
    Locate 4, 13: Color 14, 0
    Print Using "###"; S(0, 0);
    Locate 2, 35: Print Using " PERIOD #"; H;
    If H > 4 Then Locate , 35: Print Using "OVERTIME #"; H - 4
    Locate 4, 60: Color 14, 0: Print Using "###"; S(1, 0)
    Color 7: Locate 3, 35
    Print Using "##  "; T0(0);: Color 12
    Print "TO  ";: Color 7: Print Using "##"; T0(1)
    Locate , 35: Print Using "##  "; R3(0);
    Color 12: Print "RB  ";: Color 7: Print Using "##"; R3(1)
    Locate , 35: Print Using "##  "; P2(0);
    Color 12: Print "TF  ";: Color 7
    Print Using "##"; P2(1);
    If P2(0) > NB% - 1 And H <= 4 Or P2(0) > 2 And H > 4 Then Locate , 46: Color 0, 2: Print "BONUS";: Color 7, 0
    If P2(1) > NB% - 1 And H <= 4 Or P2(1) > 2 And H > 4 Then Locate , 29: Color 0, 2: Print "BONUS": Color 7, 0
    Locate 6, 9: Print "TIMEOUTS:"; T1(0);: Locate , 56: Print "TIMEOUTS:"; T1(1)
    Locate 8, 58
    Color T(D, 33), T(D, 34): Print "DEF - "; D$(D2(D))
    Locate , 58: Color T(P, 33), T(P, 34): Print "OFF - "; O$(O0%(P))
    Color 15, 0
    Locate , 41: Color 14: Print "(1) ";: Color 15: Print "SHOOT        ";: Color 14: Print "ENTER";: Color 15: Print "-PASS"
    Locate , 41: Color 14: Print "(2) ";: Color 15: Print B$(P, 0); " CALL TIMEOUT     "
    Locate , 41: Color 14: Print "(3) ";: Color 15: Print "COMMIT INTL FOUL (LAST 4 MIN)"
    Locate , 41: Color 14: Print "(4) ";: Color 15: Print "SHOOT A FORCED 3-POINTER"
    Locate , 41: Color 14: Print "(S) ";: Color 15: Print "SCOUT REPORT ";
    Color 14: Print "(T) ";: Color 15: Print "10 MIN TICKER"
    Locate , 41: Color 14: Print "(Z) ";: Color 15: Print "LEFT TM STGY ";
    Color 14: Print "(/) ";: Color 15: Print "RIGHT TM STGY"
    Locate 7, 1
    Color 9, 0: Print String$(38, 205); Chr$(203); String$(38, 205)
    Locate 8, 1: For I = 1 To 8: Locate , 39: Print Chr$(186): Next
    Locate 17, 1: Color T(0, 33), T(0, 34): Print B$(0, 0);
    Print Using " [##"; Z5(0);: Print "-";: Print Using "##]"; Z6(0)
    Color 14, 0: Print "# NAME         FG FT RB CX PA DF PT  F"
    For I1 = 0 To 4
        Color 2: Print Using "# "; I1;
        Color 3
        Color 15: Print Left$(AA$(0, B1(0, I1), 0), 12);: Locate , 16
        J = (A(0, B1(0, I1), 3)) - A(0, B1(0, I1), 7) - A(0, B1(0, I1), 11) - A(0, B1(0, I1), 12)
        If J <= 0 Then J = 0
        Print Using "##"; A(0, B1(0, I1), 0);
        Print Using "### "; A(0, B1(0, I1), 1);
        If P = 0 Then Print Using "##"; A(0, B1(0, I1), 21) * 2 / 10; Else Print Using "##"; A(0, B1(0, I1), 2) * 2 / 10;
        If J <= 0 And (H = 1 Or H = 2 Or H = 3 Or H = 4 And T > 120) Then Color 12
        Print Using "### "; J;
        Color 15
        Print Using "## ##"; A(0, B1(0, I1), 4) / 10; A(0, B1(0, I1), 5) / 10;
        Print Using "###  "; A(0, B1(0, I1), 8) * 2 + A(0, B1(0, I1), 10) + W%(0, B1(0, I1), 0);
        If A(0, B1(0, I1), 12) >= 5 Then Color 12
        Print Using "#"; A(0, B1(0, I1), 12)
        Color 15
    Next I1
    Locate 17, 40: Color T(1, 33), T(1, 34): Print B$(1, 0);: Print Using " [##"; Z5(1);: Print "-";: Print Using "##]"; Z6(1)
    Color 14, 0: Locate , 40: Print "# NAME          FG FT RB CX PA DF PT  F"
    For I1 = 0 To 4: Locate , 40: Color 2: Print Using "# "; I1;
        Color 15: Print Left$(AA$(1, B1(1, I1), 0), 12);: Locate , 56
        J = (A(1, B1(1, I1), 3)) - A(1, B1(1, I1), 7) - A(1, B1(1, I1), 11) - A(1, B1(1, I1), 12)
        If J <= 0 Then J = 0
        Print Using "##"; A(1, B1(1, I1), 0);
        Print Using "### "; A(1, B1(1, I1), 1);
        If P = 1 Then Print Using "##"; A(1, B1(1, I1), 21) * 2 / 10; Else Print Using "##"; A(1, B1(1, I1), 2) * 2 / 10;
        If J <= 0 And (H = 1 Or H = 2 Or H = 3 Or H = 4 And T > 120) Then Color 12
        Print Using "### "; J;
        Color 15
        Print Using "## ##"; A(1, B1(1, I1), 4) / 10; A(1, B1(1, I1), 5) / 10;
        Print Using "###  "; A(1, B1(1, I1), 8) * 2 + A(1, B1(1, I1), 10) + W%(1, B1(1, I1), 0);
        If A(1, B1(1, I1), 12) >= 5 Then Color 12
        Print Using "#"; A(1, B1(1, I1), 12)
        Color 15
    Next I1
    Color 9, 0: For I = 1 To 9: Locate 14 + I, 39: Print Chr$(186): Next
    Color 9, 0
    Locate 16, 1: Print String$(79, 205)
    Locate 16, 39: Print Chr$(206): Color 7, 0
End Sub


'------------------------------
'   QUARTERTOTAL Subroutine
'------------------------------
' ...explanation...
Sub QUARTERTOTAL ()

    For I = 0 To 1
        For I2 = 0 To 14: V9(I2) = 0: Next
        For I1 = 0 To 14
            V9(0) = V9(0) + A(I, I1, 8)
            V9(1) = V9(1) + A(I, I1, 7)
            V9(2) = V9(2) + A(I, I1, 10)
            V9(3) = V9(3) + A(I, I1, 9)
            V9(4) = V9(4) + A(I, I1, 11)
            V9(5) = V9(5) + (A(I, I1, 8) * 2) + A(I, I1, 10) + W%(I, I1, 0)
            V9(13) = V9(13) + W%(I, I1, 0)
            V9(14) = V9(14) + W%(I, I1, 1)
            V9(6) = TF(I)
            V9(7) = V9(7) + A(I, I1, 14)
            V9(8) = V9(8) + A(I, I1, 15)
            V9(9) = V9(9) + A(I, I1, 13)
            V9(11) = V9(11) + A(I, I1, 24)
            V9(12) = V9(12) + A(I, I1, 23)
            V9(10) = V9(10) + ZZ(I, I1)
        Next
        If H = 1 Or H = 2 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2)
            Next
            For I1 = 0 To 14
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11)
                QQ(I, H, I1, 12) = A(I, I1, 24) - QQ(I, H - 1, I1, 12)
                QQ(I, H, I1, 13) = A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13)
                QQ(I, H, I1, 14) = A(I, I1, 23) - QQ(I, H - 1, I1, 14)
            Next
        End If
        If H = 3 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2)
            Next
            For I1 = 0 To 14
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11)
                QQ(I, H, I1, 12) = A(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12)
                QQ(I, H, I1, 13) = A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13)
                QQ(I, H, I1, 14) = A(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14)
            Next
        End If
        If H = 4 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2) - QR(I, H - 3, I2)
            Next
            For I1 = 0 To 14
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6) - QQ(I, H - 3, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10) - QQ(I, H - 3, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11) - QQ(I, H - 3, I1, 11)
                QQ(I, H, I1, 12) = A(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12) - QQ(I, H - 3, I1, 12)
                QQ(I, H, I1, 13) = A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13) - QQ(I, H - 3, I1, 13)
                QQ(I, H, I1, 14) = A(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14) - QQ(I, H - 3, I1, 14)
            Next
        End If
        If H = 5 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2) - QR(I, H - 3, I2) - QR(I, H - 4, I2)
            Next
            For I1 = 0 To 14
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6) - QQ(I, H - 3, I1, I2 - 6) - QQ(I, H - 4, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10) - QQ(I, H - 3, I1, 10) - QQ(I, H - 4, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11) - QQ(I, H - 3, I1, 11) - QQ(I, H - 4, I1, 11)
                QQ(I, H, I1, 12) = A(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12) - QQ(I, H - 3, I1, 12) - QQ(I, H - 4, I1, 12)
                QQ(I, H, I1, 13) = A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13) - QQ(I, H - 3, I1, 13) - QQ(I, H - 4, I1, 13)
                QQ(I, H, I1, 14) = A(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14) - QQ(I, H - 3, I1, 14) - QQ(I, H - 4, I1, 14)
            Next
        End If
        If H = 6 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2) - QR(I, H - 3, I2) - QR(I, H - 4, I2) - QR(I, H - 5, I2)
            Next
            For I1 = 0 To 14
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6) - QQ(I, H - 3, I1, I2 - 6) - QQ(I, H - 4, I1, I2 - 6) - QQ(I, H - 5, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10) - QQ(I, H - 3, I1, 10) - QQ(I, H - 4, I1, 10) - QQ(I, H - 5, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11) - QQ(I, H - 3, I1, 11) - QQ(I, H - 4, I1, 11) - QQ(I, H - 5, I1, 11)
                QQ(I, H, I1, 12) = A(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12) - QQ(I, H - 3, I1, 12) - QQ(I, H - 4, I1, 12) - QQ(I, H - 5, I1, 12)
                QQ(I, H, I1, 13) = A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13) - QQ(I, H - 3, I1, 13) - QQ(I, H - 4, I1, 13) - QQ(I, H - 5, I1, 13)
                QQ(I, H, I1, 14) = A(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14) - QQ(I, H - 3, I1, 14) - QQ(I, H - 4, I1, 14) - QQ(I, H - 5, I1, 14)
            Next
        End If
        If H = 7 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2) - QR(I, H - 3, I2) - QR(I, H - 4, I2) - QR(I, H - 5, I2) - QR(I, H - 6, I2)
            Next
            For I1 = 0 To 14
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6) - QQ(I, H - 3, I1, I2 - 6) - QQ(I, H - 4, I1, I2 - 6) - QQ(I, H - 5, I1, I2 - 6) - QQ(I, H - 6, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10) - QQ(I, H - 3, I1, 10) - QQ(I, H - 4, I1, 10) - QQ(I, H - 5, I1, 10) - QQ(I, H - 6, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11) - QQ(I, H - 3, I1, 11) - QQ(I, H - 4, I1, 11) - QQ(I, H - 5, I1, 11) - QQ(I, H - 6, I1, 11)
                QQ(I, H, I1, 12) = A(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12) - QQ(I, H - 3, I1, 12) - QQ(I, H - 4, I1, 12) - QQ(I, H - 5, I1, 12) - QQ(I, H - 6, I1, 12)
                QQ(I, H, I1, 13) = A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13) - QQ(I, H - 3, I1, 13) - QQ(I, H - 4, I1, 13) - QQ(I, H - 5, I1, 13) - QQ(I, H - 6, I1, 13)
                QQ(I, H, I1, 14) = A(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14) - QQ(I, H - 3, I1, 14) - QQ(I, H - 4, I1, 14) - QQ(I, H - 5, I1, 14) - QQ(I, H - 6, I1, 14)
            Next
        End If

    Next
    For I = 0 To 14: V9(I) = 0: Next
End Sub


'------------------------------
'    RATINGTABLES Subroutine
'------------------------------
' ...explanation...
Sub RATINGTABLES ()
    For I = 0 To 1
        F5%(I, 0) = 0
        F5%(I, 1) = 0
        F5%(I, 2) = 0
        F5%(I, 3) = 0
        F5%(I, 4) = 0
        For I1 = 0 To 4
            F5%(I, 0) = F5%(I, 0) + A(I, B1(I, I1), 17)
            F5%(I, 1) = F5%(I, 1) + B%(I, B1(I, I1), 18)
            F5%(I, 2) = F5%(I, 2) + A(I, B1(I, I1), 25)
            F5%(I, 3) = F5%(I, 3) + A(I, B1(I, I1), 5)
            F5%(I, 4) = F5%(I, 4) + A(I, B1(I, I1), 4) / 10
        Next I1
        F5%(I, 4) = Int((F5%(I, 4) / (F7%(I, 6) * .9)) * 100 + .5)
    Next
End Sub


'------------------------------
'    SAVESTATFILES Subroutine
'------------------------------
' ...explanation...
Sub SAVESTATFILES ()
    'SAVE STATS
    If Not _FileExists("GAME82") Then 6030
    Open "GAME82" For Input As #2
    Input #2, FF$
    For I = 0 To 2: Input #2, AP%(I): Next: Close 2
    If AP%(2) > 31 Then 6011
    For X = 0 To 1: SX$(AP%(2) + 1, X) = B$(X, 0): Next
    Open "TICKER" For Output As #2
    For I = 1 To 32: For I1 = 0 To 1: For I2 = 1 To 14
    Print #2, SX%(I, I1, I2): Next: Next: Next
    For I = 1 To 32: For I1 = 0 To 1: Print #2, SX$(I, I1): Next: Next
    For I = 1 To 32: Print #2, ST%(I): Next
    Close 2

    6011 Open P$(0) + FF$ For Random As #1 Len = 48
    For X = 0 To 18
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
    Next
    Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
    NG%(13) = 1
    For X = 0 To 18: LSet Q$(X + 1) = MKI$(NG%(X)): Next
    For X = 0 To 1: LSet Q$(20 + X) = YN$(X): Next
    LSet Q$(22) = MKI$(S(0, 0)): LSet Q$(23) = MKI$(S(1, 0))
    Put #1, AP%(0) + AP%(2)
    Close 1
    AP%(2) = AP%(2) + 1
    AP = 1: If AP%(1) < AP%(0) + AP%(2) Then Kill "GAME82": AD = 1: GoTo 6030
    Open "GAME82" For Output As #2
    Print #2, FF$
    For I = 0 To 2: Print #2, AP%(I): Next: Close 2

    6030 Cls
    HH% = N
    For PX = 0 To 1
        For J = 0 To 14
            A(PX, J, 16) = ZZ(PX, J)
            A(PX, J, 17) = A(PX, J, 23)
            A(PX, J, 18) = A(PX, J, 24)
        Next
    Next
    For PX = 0 To 1
        For I = 0 To 14
            For J = 1 To 12
                A(PX, I, J) = A(PX, I, J + 6)
            Next
            W0(PX, I) = W%(PX, I, 0)
            W1(PX, I) = W%(PX, I, 1)
        Next
        Y$(PX) = B$(PX, 0)
    Next
    Q = 0

    6115 C% = 0
    If AP = 1 Then 6355
    I$ = "Y"
    If X1 = 1 And Q = 1 Or X1 = 2 And Q = 0 Then I$ = "N"
    If UCase$(I$) = "N" Then C% = 1: GoTo 6355
    If UCase$(Left$(P$(1), 1)) <> "A" And UCase$(Left$(P$(1), 1)) <> "B" Then 6355
    Cls

    6355 If C% = 1 Then 6290
    For XX = 4 To 11
        If Mid$(B$(Q, 0), XX, 1) = " " Or Mid$(B$(Q, 0), XX, 1) = "." Then Mid$(B$(Q, 0), XX, 1) = "_"
    Next XX
    For XX = 12 To 14
        If Mid$(B$(Q, 0), XX, 2) = "ST" Then Mid$(B$(Q, 0), 9, 1) = "_": Mid$(B$(Q, 0), 10, 2) = "ST"
    Next XX
    QQ = 8
    For XX = 11 To 4 Step -1
        If Mid$(B$(Q, 0), XX, 1) = "_" Then QQ = XX - 4: GoTo 6616
        GoTo 6620

    6616 Next XX

    6620 F$ = Mid$(B$(Q, 0), 4, QQ)
    R$ = F$ + "." + Left$(B$(Q, 0), 2)
    If Not _FileExists(P$(1) + R$) Then 6185
    Open P$(1) + R$ For Input As #2
    Input #2, B$(Q, 0)
    For I = 0 To 14: Input #2, AA$(Q, I, 0)
        For J = 0 To 13: Input #2, Z2(I, J): Next J
    Input #2, W2(Q, I): Input #2, W3(Q, I): Next I
    For L = 0 To 15: Input #2, Z(L): Next L
    For L = 0 To 15: Input #2, Z1(L): Next L
    Close #2

    6185 TG = 0
    For I = 0 To 14
        If A(Q, I, 2) * 2 + A(Q, I, 4) + W0(Q, I) > Z2(I, 13) Then Z2(I, 13) = A(Q, I, 2) * 2 + A(Q, I, 4) + W0(Q, I)
        If A(Q, I, 10) > 0 Then Z2(I, 0) = Z2(I, 0) + 1
        For J = 1 To 12
            Z2(I, J) = Z2(I, J) + A(Q, I, J)
            If J = 5 Or J = 6 Or J = 10 Or J = 11 Then 6215
            Z(J) = Z(J) + A(Q, I, J): Z1(J) = Z1(J) + A(1 - Q, I, J)

        6215 Next J
        Z(0) = Z(0) + A(Q, I, 10)
        TG = TG + A(Q, I, 10)
    Next I
    For I = 0 To 1: V9(I) = 0: For I1 = 0 To 14: V9(I) = V9(I) + A(I, I1, 5): Next: Next
    Z(15) = Z(15) + R3(Q) - V9(Q): Z1(15) = Z1(15) + R3(1 - Q) - V9(1 - Q)
    Z(6) = Z(6) + TF(Q): Z1(6) = Z1(6) + TF(1 - Q)
    Z(5) = Z(5) + R3(Q): Z1(5) = Z1(5) + R3(1 - Q): Z(11) = Z(11) + T0(Q): Z1(11) = Z1(11) + T0(1 - Q)
    For J = 0 To 14: Z(13) = Z(13) + W0(Q, J): Z(14) = Z(14) + W1(Q, J)
        Z1(13) = Z1(13) + W0(1 - Q, J): Z1(14) = Z1(14) + W1(1 - Q, J)
    W2(Q, J) = W2(Q, J) + W0(Q, J): W3(Q, J) = W3(Q, J) + W1(Q, J): Next J
    Z(10) = Z(10) + S(Q, 0): Z1(10) = Z1(10) + S(1 - Q, 0)
    I = Q
    Call DRAWBOX(I)
    U$ = "SAVING STATS FOR " + B$(Q, 0)
    Locate , (80 - Len(U$)) / 2
    Print U$
    Color 15, 0
    Open P$(1) + R$ For Output As #1
    Print #1, Y$(Q)
    For I = 0 To 14: Print #1, AA$(Q, I, 0)
        For J = 0 To 13: Print #1, Z2(I, J): Next J
    Print #1, W2(Q, I): Print #1, W3(Q, I): Next I
    For L = 0 To 15: Print #1, Z(L): Next L
    For L = 0 To 15: Print #1, Z1(L): Next L
    Close #1
    Y% = 0: R$ = R$ + "S"
    If Not _FileExists(P$(1) + R$) Then 6405
    Open P$(1) + R$ For Input As #1
    Input #1, Y%
    For I = 1 To 100: Input #1, O%(I): Next I
    For I = 1 To 100: Input #1, HO$(I): Next I
    For I = 1 To 100: Input #1, HT%(I): Next I
    For I = 1 To 100: Input #1, H$(I): Next I
    Close #1

    6405 If Y% < 100 Then Y% = Y% + 1
    If HH% = 2 And TG < 14500 Then H$(Y%) = "N": GoTo 6540
    If HH% <> Q And TG < 14500 Then H$(Y%) = "V": GoTo 6540
    If HH% = Q And TG < 14500 Then H$(Y%) = "H": GoTo 6540
    If HH% = Q And TG < 16000 Then H$(Y%) = "H1": GoTo 6540
    If HH% = Q And TG < 17500 Then H$(Y%) = "H2": GoTo 6540
    If HH% = Q And TG < 19000 Then H$(Y%) = "H3": GoTo 6540
    If HH% = Q And TG < 20500 Then H$(Y%) = "H4": GoTo 6540
    If HH% = 2 And TG < 16000 Then H$(Y%) = "N1": GoTo 6540
    If HH% = 2 And TG < 17500 Then H$(Y%) = "N2": GoTo 6540
    If HH% = 2 And TG < 19000 Then H$(Y%) = "N3": GoTo 6540
    If HH% = 2 And TG < 20500 Then H$(Y%) = "N4": GoTo 6540
    If HH% <> Q And TG < 16000 Then H$(Y%) = "V1": GoTo 6540
    If HH% <> Q And TG < 17500 Then H$(Y%) = "V2": GoTo 6540
    If HH% <> Q And TG < 19000 Then H$(Y%) = "V3": GoTo 6540
    If HH% <> Q And TG < 20500 Then H$(Y%) = "V4": GoTo 6540

    6540 O%(Y%) = S(Q, 0): HO$(Y%) = B$(1 - Q, 0): HT%(Y%) = S(1 - Q, 0)
    Open P$(1) + R$ For Output As #1
    Print #1, Y%
    For I = 1 To 100: Print #1, O%(I): Next I
    For I = 1 To 100: Print #1, HO$(I): Next I
    For I = 1 To 100: Print #1, HT%(I): Next I
    For I = 1 To 100: Print #1, H$(I): Next I
    Close #1
    For I = 1 To 100: O%(I) = 0: HT%(I) = 0: HO$(I) = "": H$(I) = "": Next I
    For J = 0 To 14
        For K = 0 To 13
            Z2(J, K) = 0
        Next K
    Next J
    For L = 0 To 15
        Z(L) = 0
        Z1(L) = 0
    Next L

    6290 Q = Q + 1: Z(12) = 0: If Q < 2 Then 6115
    If AP = 1 And AD <> 1 Then 6292
    If AD = 1 Then Run "HELLO.EXE"

6292 End Sub


'------------------------------
'    SCOUTREPORT Subroutine
'------------------------------
' ...explanation...
Sub SCOUTREPORT ()
    ' *** SCOUTING REPORT ***
    JJ = 13
    SN = 1
    Locate 6, 8
    Color 0, 7

    ' Top of box
    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

    ' Sides of box
    For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    ' Bottom of box
    Locate 7 + JJ, 8
    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

    Locate 7, 32

    Print "SCOUTING REPORT"
    Locate 8, 14: Color T(0, 33), T(0, 34): Print B$(0, 0);
    Locate , 52: Color T(1, 33), T(1, 34): Print B$(1, 0)
    Color 0, 7
    Locate , 18: Print Using "###           POINTS PER GAME         ###"; (T(0, 6) * 2 + T(0, 8) + T(0, 18)) / T(0, 0); (T(1, 6) * 2 + T(1, 8) + T(1, 18)) / T(1, 0)
    Locate , 18: Print Using " ##             FIELD GOAL %           ##"; T(0, 6) / T(0, 5) * 100 + D1(1); T(1, 6) / T(1, 5) * 100 + D1(0)
    Locate , 18: Print Using " ##             FREE THROW %           ##"; T(0, 8) / T(0, 7) * 100; T(1, 8) / T(1, 7) * 100
    Locate , 18: Print Using " ##          REBOUNDS PER GAME         ##"; (T(0, 9) + T(0, 10)) / T(0, 0); (T(1, 9) + T(1, 10)) / T(1, 0)
    Locate , 18: Print Using " ##      TURNOVERS FORCED PER GAME     ##"; T(0, 29) / T(0, 0); T(1, 29) / T(1, 0)
    Locate , 18: Print Using " ##    TURNOVERS COMMITTED PER GAME    ##"; T(0, 15) / T(0, 0); T(1, 15) / T(1, 0)
    Locate , 18: Print Using " ##          STEALS PER GAME           ##"; T(0, 14) / T(0, 0); T(1, 14) / T(1, 0)
    Locate , 18: Print Using " ##      PERSONAL FOULS PER GAME       ##"; T(0, 12) / T(0, 0); T(1, 12) / T(1, 0)
    Locate , 18: Print Using " ##          ASSISTS PER GAME          ##"; T(0, 11) / T(0, 0); T(1, 11) / T(1, 0)
    Locate , 18: Print Using " ##          BLOCKS PER GAME           ##"; T(0, 16) / T(0, 0); T(1, 16) / T(1, 0)
    Locate , 18: Print Using " ##             3 POINT FG%            ##"; T(0, 18) / (T(0, 17) + 1) * 100 + F%(1, 4); T(1, 18) / (T(1, 17) + 1) * 100 + F%(0, 4)
    8203 I$ = GetKeyPress$
    Color 7, 0
    Call NEWSCREEN
End Sub


'------------------------------
'    SETUPRATINGS Subroutine
'------------------------------
' ...explanation...
Sub SETUPRATINGS ()
    Dim X3 As Single, JX As Single
    '*** SET UP TEAM RATINGS***
    For I = 0 To 1
        YR% = Val(Left$(B$(I, 0), 2))
        If YR% < 40 Then YR% = YR% + 100
        FTF = 1
        If YR% < 74 Then FTF = .91
        If YR% < 69 Then FTF = .83
        For I1 = 0 To 14
            B%(I, I1, 4) = B%(I, I1, 4) * FTF
            B%(I, I1, 5) = B%(I, I1, 5) * FTF
        Next I1
        T(I, 7) = T(I, 7) * FTF
        T(I, 8) = T(I, 8) * FTF
        T(I, 21) = T(I, 21) * FTF
        T(I, 22) = T(I, 22) * FTF
        YR% = YR% - 60
        If YR% < 0 Then TOF%(I) = 59: PF%(I) = 11: GoTo 9215
        If YR% > 32 Then TOF%(I) = 34: PF%(I) = 8: GoTo 9215
        TOF%(I) = TOA%(YR%)
        PF%(I) = PFA%(YR%)

    9215 Next
    If Left$(B$(1, 0), 2) = Left$(B$(0, 0), 2) Then 9253
    If AP = 1 Then I = NG%(6): GoTo 9252
    Cls: Print "THESE TWO TEAMS MAY HAVE PLAYED DURING DIFFERENT ERAS AND"
    Print "PLAYED DIFFERENT STYLES OF BASKETBALL"
    Print
    Print "PLAY STYLE OF:"
    Print
    Color 14: Print "(0) - ";: Color T(0, 33), T(0, 34): Print B$(0, 0): Color , 0
    Print
    Color 14: Print "(1) - ";: Color T(1, 33), T(1, 34): Print B$(1, 0): Color , 0
    Print
    Color 14: Print "(2) - ";: Color 3: Print "MAKE NO ADJUSTMENT": Color 15, 0
    9251 I$ = GetKeyPress$
    I = Val(I$): If I < 0 Or I > 2 Then 9251

    9252 If I = 2 Then 9253
    HN% = 1: HM% = I
    JX = (T(0, 5) + T(0, 19) + T(1, 5) + T(1, 19)) * (L%(I, 1) / L%(1 - I, 1)) / ((T(1, 0) + T(0, 0)) * 2) * 2 - (L%(I, 1) + L%(I, 1)) / 2
    JY = JX
    GoTo 9254

    9253 'JX = (  T(0, 5) + T(0, 19) + T(1, 5) + T(1, 19)  ) / ( (T(1, 0) + T(0, 0) ) * 2) * 2 - (L%(0, 1) + L%(1, 1)) / 2
    JX = (T(0, 5) + T(0, 19)) / ((T(1, 0) + T(0, 0)) * 2) + (T(1, 5) + T(1, 19)) / ((T(1, 0) + T(0, 0)) * 2)
    JX = JX * 2 - (L%(0, 1) + L%(1, 1)) / 2
    JY = JX
    If PO = 1 Then JY = JY - 7

    9254 For I = 0 To 1
        D1(I) = Int(T(I, 20) / T(I, 19) * 100 + .5) - L%(I, 3)
        X7(I) = 1440 - ((T(I, 5) / T(I, 0)) + (T(I, 19) / T(I, 0))) * 2
        X7(I) = X7(I) / 25
        F7%(I, 5) = Int(((T(I, 5) + T(I, 19)) / (T(I, 0) * 2)) + .5)
        F7%(I, 0) = Int((((T(I, 28) / T(I, 0)) / (X7(I) * 5)) * 1000) + .5) - 41
        F7%(I, 1) = Int(((T(I, 15) - T(I, 28)) / T(I, 0)) / (X7(I) * 5) * 1000 + .5)
        F7%(I, 2) = Int(((T(I, 29) - T(I, 14)) / T(I, 0)) / (X7(I) * 5) * 1000 + .5) - TOF%(I): '-41
        If T(I, 31) > 0 Then F7%(I, 4) = Int(T(I, 32) / T(I, 31) * 100 + .5) - L%(I, 5)
        If T(I, 31) = 0 Then F7%(I, 4) = 0 - L%(I, 5)
        F7%(I, 6) = Int(T(I, 6) / T(I, 0) + .5)
        X3 = (T(I, 21) - T(I, 20) * .1) - 7.9 * T(I, 0)
        X3 = X3 / 2
        F7%(I, 3) = Int(X3 / (T(I, 19) + X3) * 100 + .5) - PF%(I): ' WAS -7
        F7%(I, 7) = Int(T(I, 11) / T(I, 6) * 100 + .5)
    Next
    For I = 0 To 1
        For I1 = 0 To 14
            If B%(I, I1, 18) <> 0 Then 9260
            If B%(I, I1, 2) - B%(I, I1, 13) > 0 Then A(I, I1, 0) = Int((B%(I, I1, 3) - B%(I, I1, 14)) / (B%(I, I1, 2) - B%(I, I1, 13)) * 100 + .5)
            If PO = 1 Then A(I, I1, 0) = A(I, I1, 0) - 2
            If B%(I, I1, 4) > 0 Then A(I, I1, 1) = Int(B%(I, I1, 5) / B%(I, I1, 4) * 100 + .5)
            X3 = B%(I, I1, 7) / B%(I, I1, 1) * 48
            X3 = X3 * 29.3 / (L%(I, 2) * .677) * ((T(I, 5) + T(I, 19)) / (T(I, 0) * 2) / L%(I, 1))
            X3 = Int(X3 * 10 + .5)
            A(I, I1, 2) = X3
            X3 = B%(I, I1, 6) / B%(I, I1, 1) * 48
            X3 = X3 * 14! / (L%(I, 2) * .323) * ((T(I, 5) + T(I, 19)) / (T(I, 0) * 2) / L%(I, 1))
            X3 = Int(X3 * 10 + .5)
            A(I, I1, 21) = X3
            X3 = B%(I, I1, 8) / B%(I, I1, 1) * 48
            A(I, I1, 25) = Int(X3 * 10 + .5)
            X3 = X3 * 26 * T(I, 0) / T(I, 11)
            A(I, I1, 4) = Int(X3 * 10 + .5)
            ASSIST:
            A(I, I1, 5) = B%(I, I1, 11) / B%(I, I1, 1) * 48
            A(I, I1, 5) = Int(A(I, I1, 5) / X7(I) * 1000 + .5)
            BLOCK:
            A(I, I1, 6) = B%(I, I1, 12) / B%(I, I1, 1) * 48
            A(I, I1, 6) = A(I, I1, 6) / ((T(I, 19) - T(I, 31) - T(I, 20) + T(I, 32)) / T(I, 0) / 5)
            A(I, I1, 6) = Int(A(I, I1, 6) * .93 * 100 + .5)
            If FR% = 0 Then X3 = B%(I, I1, 4) - (B%(I, I1, 3) * .1) - 1.58 * B%(I, I1, 1) / 48
            If FR% = 1 Then X3 = B%(I, I1, 4) * .9 - (B%(I, I1, 3) * .1) - 1.58 * B%(I, I1, 1) / 48
            X3 = X3 / 2
            A(I, I1, 16) = Int(X3 / (B%(I, I1, 2) + X3) * 100 + .5)
            If A(I, I1, 16) < 0 Then A(I, I1, 16) = 0
            A(I, I1, 17) = Int(B%(I, I1, 9) / B%(I, I1, 1) * 48 * 10 + .5)
            A(I, I1, 18) = B%(I, I1, 18)
            If B%(I, I1, 2) > 0 Then A(I, I1, 19) = Int(B%(I, I1, 13) / B%(I, I1, 2) * 100 + .5)
            If B%(I, I1, 13) > 0 Then A(I, I1, 20) = Int(B%(I, I1, 14) / B%(I, I1, 13) * 100 + .5)
            X3 = Int(B%(I, I1, 2) / (B%(I, I1, 2) + B%(I, I1, 4)) * 100 + .5)
            If Left$(AA$(I, I1, 1), 1) = "F" Or Left$(AA$(I, I1, 1), 1) = "C" Then 9255
            If X3 >= 86 Then B%(I, I1, 16) = 1
            If X3 = 84 Or X3 = 85 Then B%(I, I1, 16) = 2
            If X3 = 82 Or X3 = 83 Then B%(I, I1, 16) = 3
            If X3 = 80 Or X3 = 81 Then B%(I, I1, 16) = 4
            If X3 >= 77 And X3 <= 79 Then B%(I, I1, 16) = 5
            If X3 = 75 Or X3 = 76 Then B%(I, I1, 16) = 6
            If X3 = 73 Or X3 = 74 Then B%(I, I1, 16) = 7
            If X3 = 71 Or X3 = 72 Then B%(I, I1, 16) = 8
            If X3 <= 70 Then B%(I, I1, 16) = 9
            GoTo 9260

            9255 If X3 >= 81 Then B%(I, I1, 16) = 1
            If X3 = 79 Or X3 = 80 Then B%(I, I1, 16) = 2
            If X3 = 77 Or X3 = 78 Then B%(I, I1, 16) = 3
            If X3 = 75 Or X3 = 76 Then B%(I, I1, 16) = 4
            If X3 >= 72 And X3 <= 74 Then B%(I, I1, 16) = 5
            If X3 = 70 Or X3 = 71 Then B%(I, I1, 16) = 6
            If X3 = 68 Or X3 = 69 Then B%(I, I1, 16) = 7
            If X3 = 66 Or X3 = 67 Then B%(I, I1, 16) = 8
            If X3 <= 65 Then B%(I, I1, 16) = 9

        9260 Next I1
    Next I
    For I = 0 To 1
        M9(I) = (T(I, 5) + T(I, 19) + T(I, 9) + T(I, 10)) / (T(I, 0) * 2)
        M9(I) = M9(I) + (T(I, 23) + T(I, 24) + T(I, 12) + T(I, 26)) / (T(I, 0) * 2)
        M9(I) = M9(I) * 2 - (L%(I, 1) + L%(I, 2) + L%(I, 4))
    Next I
    If HN% <> 1 Then 9264
    I = HM%
    F7%(1 - I, 1) = F7%(1 - I, 1) * TOF%(I) / TOF%(1 - I)
    M9(1 - I) = M9(1 - I) * (L%(I, 1) + L%(I, 2) + L%(I, 4)) / (L%(1 - I, 1) + L%(1 - I, 2) + L%(1 - I, 4))
    For I1 = 0 To 14
        A(1 - I, I1, 0) = A(1 - I, I1, 0) + L%(I, 3) - L%(1 - I, 3) - L%(1 - I, 6) + L%(I, 6)
    Next

    9264 S9 = Int((M9(0) + M9(1)) / 2 + .5)
    If PO = 1 Then S9 = S9 - 11
    For I = 0 To 1: M9(I) = 0
        For I1 = 0 To 14
            If B%(I, I1, 18) <> 0 Then 9265
            M9(I) = M9(I) + Int((B%(I, I1, 2) + B%(I, I1, 6) + B%(I, I1, 7) + B%(I, I1, 9)) / B%(I, I1, 0) + .5)

        9265 Next I1
    Next I
    For I = 0 To 1
        For I1 = 0 To 14
            If B%(I, I1, 18) <> 0 Then 9266
            A(I, I1, 3) = Int(((B%(I, I1, 2) + B%(I, I1, 6) + B%(I, I1, 7) + B%(I, I1, 9)) / B%(I, I1, 0)) / M9(I) * S9 + .5)
            If A(I, I1, 3) = 0 Then A(I, I1, 3) = 1

        9266 Next I1
    Next I

    9300 If AP = 1 And U5 >= 2 Then 9499
    Color 15, 0: Cls
    Print "EDIT CONTRIBUTION PERCENTAGES OF PLAYERS FROM:"
    Print
    Color 14: Print "(0) - ";: Color T(0, 33), T(0, 34): Print B$(0, 0): Color , 0
    Print
    Color 14: Print "(1) - ";: Color T(1, 33), T(1, 34): Print B$(1, 0): Color , 0
    Print
    Color 14: Print "(2) - ";: Color 3: Print "PROCEED TO GAME": Color 15, 0
    Print
    Print "NOTE:: THE CONTRIBUTION RATING CONSISTS OF THE PLAYER'S"
    Print "       FREQUENCY TO SHOOT, REBOUND, AND COMMIT FOULS."
    9301 I$ = GetKeyPress$
    I = Val(I$): If I < 0 Or I > 2 Then 9301
    If I = 2 Then 9499
    Cls
    TX = 0: For I1 = 0 To 14: TX = TX + A(I, I1, 3): Next
    For I1 = 0 To 14: A(I, I1, 3) = A(I, I1, 3) / TX * 100: Next

    9305 Locate 2, 1: Print "CONTRIBUTION RATINGS FOR ";: Color T(I, 33), T(I, 34): Print B$(I, 0): Color 15, 0
    Print: Print " #  PLAYER-POS         CX"
    TY% = 0: For I1 = 0 To 14: TY% = TY% + A(I, I1, 3)
        Print Using "## "; I1 + 1;: Print AA$(I, I1, 0); "-"; AA$(I, I1, 1);: Locate , 24: Print Using "##"; A(I, I1, 3)
    Next
    Locate , 23: Print Using "###"; TY%
    Input "SELECT PLAYER TO CHANGE (0 TO CONTINUE)"; I1: I1 = I1 - 1
    If I1 > 14 Or I1 < 0 Then 9310
    Locate 22, 1: Print "ENTER NEW CX RATING FOR "; AA$(I, I1, 0); " ";: Input A(I, I1, 3)

    9306 Locate 22, 1: Print "                                                    ": GoTo 9305

    9310 If TY% = 100 Then 9320
    Locate 22, 1: Print "TOTAL MUST ADD UP TO 100%! PRESS ANY KEY"
    9311 I$ = GetKeyPress$
    GoTo 9306

    9320 For I1 = 0 To 14: A(I, I1, 3) = A(I, I1, 3) / 100 * TX: Next
    GoTo 9300

    9499 X3 = 0
End Sub


'------------------------------
'    TICKERUPDATE Subroutine
'------------------------------
' ...explanation...
Sub TICKERUPDATE ()
    If AP <> 1 Or AP%(2) > 31 Then GoTo 2580
    If H = 1 And T <= 480 And SX%(1 + AP%(2), 0, 1) = -1 Then SX%(1 + AP%(2), 0, 1) = S(0, 0): SX%(1 + AP%(2), 1, 1) = S(1, 0): GoTo 2580
    If H = 1 And T <= 240 And SX%(1 + AP%(2), 0, 2) = -1 Then SX%(1 + AP%(2), 0, 2) = S(0, 0): SX%(1 + AP%(2), 1, 2) = S(1, 0): GoTo 2580
    If H > 1 And SX%(1 + AP%(2), 0, 3) = -1 Then SX%(1 + AP%(2), 0, 3) = S(0, 1): SX%(1 + AP%(2), 1, 3) = S(1, 1): GoTo 2580
    If H = 2 And T <= 480 And SX%(1 + AP%(2), 0, 4) = -1 Then SX%(1 + AP%(2), 0, 4) = S(0, 0): SX%(1 + AP%(2), 1, 4) = S(1, 0): GoTo 2580
    If H = 2 And T <= 240 And SX%(1 + AP%(2), 0, 5) = -1 Then SX%(1 + AP%(2), 0, 5) = S(0, 0): SX%(1 + AP%(2), 1, 5) = S(1, 0): GoTo 2580
    If H > 2 And SX%(1 + AP%(2), 0, 6) = -1 Then For I2 = 6 To 8: SX%(1 + AP%(2), 0, I2) = S(0, 2) + S(0, 1): SX%(1 + AP%(2), 1, I2) = S(1, 2) + S(1, 1): Next: GoTo 2580
    If H = 3 And T <= 480 And SX%(1 + AP%(2), 0, 9) = -1 Then SX%(1 + AP%(2), 0, 9) = S(0, 0): SX%(1 + AP%(2), 1, 9) = S(1, 0): GoTo 2580
    If H = 3 And T <= 240 And SX%(1 + AP%(2), 0, 10) = -1 Then SX%(1 + AP%(2), 0, 10) = S(0, 0): SX%(1 + AP%(2), 1, 10) = S(1, 0): GoTo 2580
    If H > 3 And SX%(1 + AP%(2), 0, 11) = -1 Then SX%(1 + AP%(2), 0, 11) = S(0, 3) + S(0, 2) + S(0, 1): SX%(1 + AP%(2), 1, 11) = S(1, 3) + S(1, 2) + S(1, 1): GoTo 2580
    If H = 4 And T <= 480 And SX%(1 + AP%(2), 0, 12) = -1 Then SX%(1 + AP%(2), 0, 12) = S(0, 0): SX%(1 + AP%(2), 1, 12) = S(1, 0): GoTo 2580
    If H = 4 And T <= 240 And SX%(1 + AP%(2), 0, 13) = -1 Then SX%(1 + AP%(2), 0, 13) = S(0, 0): SX%(1 + AP%(2), 1, 13) = S(1, 0): GoTo 2580
    SX%(1 + AP%(2), 0, 14) = S(0, 0): SX%(1 + AP%(2), 1, 14) = S(1, 0)

2580 End Sub


'------------------------------
'      ADJFOUL Subroutine
'------------------------------
' ...explanation...
Sub ADJFOUL ()
    For I = 0 To 14
        If A(P9, I, 12) >= 6 Then A(P9, I, 12) = 5: GoTo 8789
    Next

8789 End Sub


'------------------------------
'      BADFT Subroutine
'------------------------------
' ...explanation...
Sub BADFT ()
    If F3 = 1 Then U$ = "FIRST FREE THROW IS MISSED"
    If F3 = 2 Then U$ = "SECOND FREE THROW IS MISSED"
    If F3 = 3 Then U$ = "THIRD FREE THROW IS MISSED"
    Call DELAY
End Sub


'------------------------------
'   CHANGESTRATEGY Subroutine
'------------------------------
' ...explanation...
Sub CHANGESTRATEGY (I$)
    If UCase$(I$) = "Z" And U5 = 1 And CT = 0 Or U5 = 2 Then 8155
    If I$ = "/" And U5 = 1 And CT = 1 Or U5 = 2 Then 8155
    TS% = 1
    If UCase$(I$) = "Z" Then TS% = 0

    8101 JJ = 8: Call FRAMEBOX: Print B$(TS%, 0); " SELECT STRATEGY:"

    8102 Locate 12, 14
    Locate , 14: Print "0 - RETURN TO GAME"
    Locate , 14: Print "1 - SELECT OFFENSIVE STYLE OF PLAY"
    Locate , 14: Print "2 - SELECT DEFENSIVE STYLE OF PLAY"
    Locate , 14: Print "3 - FULL COURT PRESS "; P7$(P7(TS%))
    8103 I$ = GetKeyPress$
    I1 = Val(I$): If I1 < 0 Or I1 > 3 Then 8103
    If I1 = 0 Then 8155
    If I1 = 1 Then GoSub 8170: GoTo 8101
    If I1 = 2 Then GoSub 8160: GoTo 8101
    P7(TS%) = 1 - P7(TS%): GoTo 8102

    8155 Color 7, 0: Call NEWSCREEN
    GoTo 8166

    8160 'DEFENSE
    JJ = 10: Call FRAMEBOX: Print B$(TS%, 0); " SELECT DEFENSE:"
    Locate 12, 14
    Locate , 14: Print "1 - REGULAR MAN-TO-MAN"
    Locate , 14: Print "2 - SAGGING MAN-TO-MAN"
    Locate , 14: Print "3 - TRAPPING MAN-TO-MAN"
    Locate , 14: Print "4 - TIGHT MAN-TO-MAN"
    Locate , 14: Print "5 - LOOSE MAN-TO-MAN"
    8161 I$ = GetKeyPress$
    I1 = Val(I$): If I1 < 1 Or I1 > 5 Then 8161
    D2(TS%) = I1
    Return

    8170 'DEFENSE
    JJ = 8: Call FRAMEBOX: Print B$(TS%, 0); " SELECT OFFENSE:"
    Locate 12, 14
    Locate , 14: Print "0 - NORMAL"
    Locate , 14: Print "1 - AGGRESSIVE"
    Locate , 14: Print "2 - SAFE"
    8171 I$ = GetKeyPress$
    I1 = Val(I$): If I1 < 0 Or I1 > 2 Then 8171
    O0%(TS%) = I1
    Return

8166 End Sub


'------------------------------
'   COMPUTERCOACH Subroutine
'------------------------------
' ...explanation...
Sub COMPUTERCOACH ()
    If P9 = 0 Then I6 = 1
    If P9 = 1 Then I6 = 0
    If S(P9, 0) - S(I6, 0) < 0 Then I5 = S(I6, 0) - S(P9, 0): GoTo 8867

    R0 = Int(Rnd(1) * 100) + 1
    P7(P9) = 0
    If H <= 4 And R0 >= 41 Then D2(P9) = 1
    If H <= 4 And R0 > 20 And R0 <= 40 Then D2(P9) = 4
    If H <= 4 And R0 <= 20 Then D2(P9) = 2
    If H = 3 And R0 >= 50 Then D2(P9) = 1
    If H = 3 And R0 > 25 And R0 < 50 Then D2(P9) = 4
    If H = 3 And R0 <= 25 Then D2(P9) = 2
    If H = 4 And R0 >= 70 Then D2(P9) = 1
    If H = 4 And R0 >= 35 And R0 < 70 Then D2(P9) = 2
    If H = 4 And R0 < 35 Then D2(P9) = 4
    If H > 4 And R0 >= 50 Then D2(P9) = 1
    If H > 4 And R0 >= 25 And R0 < 50 Then D2(P9) = 2
    If H > 4 And R0 < 25 Then D2(P9) = 4

    8866 GoTo 8880

    8867 If H <= 2 Then D2(P9) = 1: P7(P9) = 0
    If H = 3 And I5 > 20 Then D2(P9) = 1: P7(P9) = 0
    If H = 3 And I5 >= 10 And I5 < 20 Then D2(P9) = 1: P7(P9) = 1
    If H = 3 And I5 < 10 Then D2(P9) = 1: P7(P9) = 0
    If H = 4 And T >= 120 And I5 > 20 Then D2(P9) = 1: P7(P9) = 0
    If H = 4 And T >= 120 And I5 > 10 And I5 < 20 Then D2(P9) = 3: P7(P9) = 1
    If H = 4 And T >= 120 And I5 <= 10 Then D2(P9) = 1: P7(P9) = 1
    If H = 4 And T < 120 And I5 > 10 Then D2(P9) = 1: P7(P9) = 0
    If H = 4 And T < 120 And I5 <= 10 And I5 > 4 Then D2(P9) = 3: P7(P9) = 1
    If H = 4 And T < 120 And I5 <= 4 Then D2(P9) = 1: P7(P9) = 1
    If H > 4 Then D2(P9) = 1: P7(P9) = 1
    GoTo 8866

    8880 O0%(P9) = 0: If H < 3 Then O0%(P9) = 0
    If H >= 3 And S(P9, 0) - S(I6, 0) < 0 And Int(Rnd(1) * 2) = 1 Then O0%(P9) = 1
    If H >= 3 And S(P9, 0) - S(I6, 0) >= 0 And Int(Rnd(1) * 2) = 1 Then O0%(P9) = 2
End Sub


'------------------------------
'   COMPUTERSUBS Subroutine
'------------------------------
' ...explanation...
Sub COMPUTERSUBS ()
    For I2 = 0 To 4
        B1(P9, I2) = -1
    Next
    For I2 = 0 To 1
        P$ = "G"

        8781 For I1 = 14 To 0 Step -1: M6 = 0: M7 = 0
            If Left$(AA$(P9, I1, 1), 1) <> P$ And Right$(AA$(P9, I1, 1), 1) <> P$ Then 8782
            If AA$(P9, I1, 0) = "XXX" Or A(P9, I1, 12) >= 6 Or B%(P9, I1, 18) <> 0 Or H = 1 And A(P9, I1, 12) >= 2 + FS% Or H = 2 And A(P9, I1, 12) >= 4 + FS% Or H = 3 And A(P9, I1, 12) >= 5 Or H = 4 And T > 300 And A(P9, I1, 12) = 5 Then 8782
            M6 = A(P9, I1, 3) - A(P9, I1, 7) - A(P9, I1, 11) - A(P9, I1, 12)
            If B1(P9, I2) > -1 Then M7 = A(P9, B1(P9, I2), 3) - A(P9, B1(P9, I2), 7) - A(P9, B1(P9, I2), 11) - A(P9, B1(P9, I2), 12)
            If H = 4 And T < 120 Or H > 4 Then M6 = A(P9, I1, 3)
            If (H = 4 And T < 120 Or H > 4) And B1(P9, I2) > -1 Then M7 = A(P9, B1(P9, I2), 3)
            If I2 = 1 And B1(P9, 0) = I1 Then 8782
            If B1(P9, I2) = -1 Or M6 >= M7 Then B1(P9, I2) = I1

        8782 Next I1
        If B1(P9, I2) = -1 And FS% <> 1 And P$ = "G" Then P$ = "F": GoTo 8781
        If B1(P9, I2) = -1 And FS% <> 1 And P$ = "F" Then P$ = "C": GoTo 8781
        If B1(P9, I2) = -1 And FS% <> 1 Then FS% = 1: P$ = "G": GoTo 8781
        FS% = 0
    Next I2
    For I2 = 2 To 3
        P$ = "F"

        8783 For I1 = 14 To 0 Step -1: M6 = 0: M7 = 0
            If Left$(AA$(P9, I1, 1), 1) <> P$ And Right$(AA$(P9, I1, 1), 1) <> P$ Then 8784
            If AA$(P9, I1, 0) = "XXX" Or A(P9, I1, 12) >= 6 Or B%(P9, I1, 18) <> 0 Or H = 1 And A(P9, I1, 12) >= 2 + FS% Or H = 2 And A(P9, I1, 12) >= 4 + FS% Or H = 3 And A(P9, I1, 12) >= 5 Or H = 4 And T > 300 And A(P9, I1, 12) = 5 Then 8784
            M6 = A(P9, I1, 3) - A(P9, I1, 7) - A(P9, I1, 11) - A(P9, I1, 12)
            If B1(P9, I2) > -1 Then M7 = A(P9, B1(P9, I2), 3) - A(P9, B1(P9, I2), 7) - A(P9, B1(P9, I2), 11) - A(P9, B1(P9, I2), 12)
            If H = 4 And T < 120 Or H > 4 Then M6 = A(P9, I1, 3)
            If (H = 4 And T < 120 Or H > 4) And B1(P9, I2) > -1 Then M7 = A(P9, B1(P9, I2), 3)
            If I2 = 2 And (B1(P9, 0) = I1 Or B1(P9, 1) = I1) Then 8784
            If I2 = 3 And (B1(P9, 0) = I1 Or B1(P9, 1) = I1 Or B1(P9, 2) = I1) Then 8784
            If B1(P9, I2) = -1 Or M6 >= M7 Then B1(P9, I2) = I1

        8784 Next I1
        If B1(P9, I2) = -1 And FS% <> 1 And P$ = "F" Then P$ = "C": GoTo 8783
        If B1(P9, I2) = -1 And FS% <> 1 And P$ = "C" Then P$ = "G": GoTo 8783
        If B1(P9, I2) = -1 And FS% <> 1 Then FS% = 1: P$ = "F": GoTo 8783
        FS% = 0
    Next I2
    P$ = "C"

    8785 For I1 = 14 To 0 Step -1: M6 = 0: M7 = 0
        If Left$(AA$(P9, I1, 1), 1) <> P$ And Right$(AA$(P9, I1, 1), 1) <> P$ Then 8786
        If AA$(P9, I1, 0) = "XXX" Or A(P9, I1, 12) >= 6 Or B%(P9, I1, 18) <> 0 Or H = 1 And A(P9, I1, 12) >= 2 + FS% Or H = 2 And A(P9, I1, 12) >= 4 + FS% Or H = 3 And A(P9, I1, 12) >= 5 Or H = 4 And T > 300 And A(P9, I1, 12) = 5 Then 8786
        M6 = A(P9, I1, 3) - A(P9, I1, 7) - A(P9, I1, 11) - A(P9, I1, 12)
        If B1(P9, 4) > -1 Then M7 = A(P9, B1(P9, 4), 3) - A(P9, B1(P9, 4), 7) - A(P9, B1(P9, 4), 11) - A(P9, B1(P9, 4), 12)
        If H = 4 And T < 120 Or H > 4 Then M6 = A(P9, I1, 3)
        If (H = 4 And T < 120 Or H > 4) And B1(P9, I2) > -1 Then M7 = A(P9, B1(P9, I2), 3)
        If B1(P9, 0) = I1 Or B1(P9, 1) = I1 Or B1(P9, 2) = I1 Or B1(P9, 3) = I1 Then 8786
        If B1(P9, 4) = -1 Or M6 >= M7 Then B1(P9, 4) = I1

    8786 Next I1
    If B1(P9, I2) = -1 And FS% <> 1 And P$ = "C" Then P$ = "F": GoTo 8785
    If B1(P9, I2) = -1 And FS% <> 1 And P$ = "F" Then P$ = "G": GoTo 8785
    If B1(P9, I2) = -1 And FS% <> 1 Then FS% = 1: P$ = "F": GoTo 8785
    FS% = 0
    For I2 = 0 To 4
        If B1(P9, I2) = -1 Then GoSub 8790: GoTo 8787
    Next

    8787 Call COMPUTERCOACH
    GoTo 8788

    8790 I = 15
    For I2 = 0 To 4

        8795 I = I - 1
        If I < 0 Then Call ADJFOUL: GoTo 8790
        If B%(P9, I, 18) <> 0 Or AA$(P9, I, 0) = "XXX" Or A(P9, I, 12) >= 6 Then 8795
        B1(P9, I2) = I
    Next I2
    Return

8788 End Sub


'------------------------------
'      DELAY Subroutine
'------------------------------
' ...explanation...
Sub DELAY ()

    'Original code that was modified:
    '    If displayPause = 0 Or PB% = 1 Then
    If displayPause > 0 Or PB% = 0 Then

        '-- Shouldn't we PadRight instead to 38 length
        2249 If Len(U$) < 38 Then U$ = U$ + " ": GoTo 2249
        U$ = Left$(U$, 38)

        Locate 8, 1

        If NL% = 1 Then A1$ = U$: VG(1) = T(P, 33): VH(1) = T(P, 34)
        If NL% = 2 Then B1$ = U$: VG(2) = T(P, 33): VH(2) = T(P, 34)
        If NL% = 3 Then C1$ = U$: VG(3) = T(P, 33): VH(3) = T(P, 34)
        If NL% = 4 Then D1$ = U$: VG(4) = T(P, 33): VH(4) = T(P, 34)
        If NL% = 5 Then E1$ = U$: VG(5) = T(P, 33): VH(5) = T(P, 34)
        If NL% = 6 Then F1$ = U$: VG(6) = T(P, 33): VH(6) = T(P, 34)
        If NL% = 7 Then G1$ = U$: VG(7) = T(P, 33): VH(7) = T(P, 34)
        If NL% = 8 Then H1$ = U$: VG(8) = T(P, 33): VH(8) = T(P, 34)
        If NL% > 8 Then
            A1$ = B1$
            B1$ = C1$
            C1$ = D1$
            D1$ = E1$
            E1$ = F1$
            F1$ = G1$
            G1$ = H1$
            H1$ = U$
            For I = 1 To 7
                VG(I) = VG(I + 1)
                VH(I) = VH(I + 1)
            Next
            VG(8) = T(P, 33)
            VH(8) = T(P, 34)
        End If

        NL% = NL% + 1

        Color VG(1), VH(1): Print A1$
        Color VG(2), VH(2): Print B1$
        Color VG(3), VH(3): Print C1$
        Color VG(4), VH(4): Print D1$
        Color VG(5), VH(5): Print E1$
        Color VG(6), VH(6): Print F1$
        Color VG(7), VH(7): Print G1$
        Color VG(8), VH(8): Print H1$
        Color 15, 0

        _Delay displayPause

    End If

End Sub


'------------------------------
'      EVENTS Subroutine
'------------------------------
' ...explanation...
Sub EVENTS (COUNT, SPECIAL)
    If Not _FileExists("EVENTS.BOX") Then 8450
    Open "EVENTS.BOX" For Input As #1
    For X = 1 To 13
        Input #1, E%(X)
    Next
    Close 1
    If H - 1 > E%(1) Then GoSub 7100: GoTo 8450
    If S(1, 0) + S(0, 0) > E%(2) Then GoSub 7100: GoTo 8450
    If R3(0) + R3(1) > E%(3) Then GoSub 7100: GoTo 8450
    If S(1, 0) > E%(4) Or S(0, 0) > E%(4) Then GoSub 7100: GoTo 8450
    If R3(1) > E%(5) Or R3(0) > E%(5) Then GoSub 7100: GoTo 8450
    For I = 0 To 1
        For I1 = 0 To 14
            If A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) > E%(6) Then GoSub 7100: GoTo 8450
            If A(I, I1, 11) > E%(7) Then GoSub 7100: GoTo 8450
            If A(I, I1, 14) > E%(8) Then GoSub 7100: GoTo 8450
            If A(I, I1, 15) > E%(9) Then GoSub 7100: GoTo 8450
            If A(I, I1, 13) > E%(10) Then GoSub 7100: GoTo 8450
            If W%(I, I1, 0) > E%(11) Then GoSub 7100: GoTo 8450
        Next
    Next
    GoTo 8450

    7100 SPEC$ = P$(1) + "*.SPC"
    COUNT = FCount%(SPEC$)
    COUNT = COUNT + 1
    SPECIAL = 1
    Return

8450 End Sub


'------------------------------
'      FRAMEBOX Subroutine
'------------------------------
' ...explanation...
Sub FRAMEBOX ()
    SN = 1
    Locate 8, 8
    Color T(TS%, 33), T(TS%, 34)

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

End Sub


'------------------------------
'      MADEFT Subroutine
'------------------------------
' ...explanation...
Sub MADEFT ()
    A(P, B1(P, B), 10) = A(P, B1(P, B), 10) + 1
    If displayPause <> 0 Then
        Locate F3 + 9, 1
        Call MOREDELAY
        Print F3; " GOOD"
    End If
    S(P, H) = S(P, H) + 1
    S(P, 0) = S(P, 0) + 1
    Call UPDATESCREEN
    Call MOREDELAY
End Sub


'------------------------------
'      MAKEPBP Subroutine
'------------------------------
' ...explanation...
Sub MAKEPBP ()
    'PLAY BY PLAY
    If displayPause = 0 Then GoTo 1495
    RN = Int(Rnd(1) * 100) + 1
    On PT% GOTO 1410, 1420, 1430, 1440, 1445, 1450, 1460, 1470, 1480, 1490
    U$ = AA$(P, B1(P, B), 0)

    1410 'TURNOVER
    U$ = AA$(P, B1(P, B), 0)
    RN = Int(Rnd(1) * 14) + 1
    If RN = 1 Then U$ = U$ + " TURNS THE BALL OVER"
    If RN = 2 Then U$ = U$ + " THROWS IT AWAY"
    If RN = 3 Then U$ = U$ + " IS WHISTLED FOR TRAVELING"
    If RN = 4 Then U$ = U$ + " TURNS IT OVER"
    If RN = 5 Then U$ = U$ + " LOSES CONTROL OF THE BALL"
    If RN = 6 Then U$ = U$ + " LOSES IT OUT OF BOUNDS"
    If RN = 7 Then U$ = U$ + " LOSES THE HANDLE"
    If RN = 8 Then U$ = U$ + " LOSES IT"
    If RN = 9 Then U$ = U$ + " IS CALLED FOR STEPS"
    If RN = 10 Then U$ = U$ + " TRAVELS"
    If RN = 11 Then U$ = U$ + " WALKS WITH IT"
    If RN = 12 Then U$ = U$ + " LETS IT GET AWAY"
    If RN = 13 Then U$ = U$ + " DOUBLE DRIBBLES"
    If RN = 14 Then U$ = U$ + " LOSES CONTROL"
    GoTo 1495

    1420 'DUNK
    RN = Int(Rnd(1) * 39) + 1
    If RN = 1 Then U$ = "DIPSY-DOO DUNK-A-ROO!!!"
    If RN = 2 Then U$ = AA$(P, B1(P, B), 0) + " MAKES IT HAMMER TIME!!!"
    If RN = 3 Then U$ = AA$(P, B1(P, B), 0) + " SLAMS IT IN!!!"
    If RN = 4 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE 2-HANDED SLAM!!!"
    If RN = 5 Then U$ = AA$(P, B1(P, B), 0) + " SLAMS IT HOME!!!"
    If RN = 6 Then U$ = AA$(P, B1(P, B), 0) + " JAMS IT HOME!!!"
    If RN = 7 Then U$ = AA$(P, B1(P, B), 0) + " THROWS IT DOWN!!!"
    If RN = 8 Then U$ = AA$(P, B1(P, B), 0) + " THUNDERSLAM!!!"
    If RN = 9 Then U$ = "RIM ROCKING JAM!!!"
    If RN = 10 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE MONSTER SLAM!!"
    If RN = 11 Then U$ = AA$(P, B1(P, B), 0) + "POWER SLAMS IT!!!"
    If RN = 12 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE 2-HANDED STUFF!!!"
    If RN = 13 Then U$ = AA$(P, B1(P, B), 0) + " BACKWARDS JAM!!!"
    If RN = 14 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE 1-HANDED SLAM!!!"
    If RN = 15 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE REVERSE SLAM!!!"
    If RN = 16 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE REVERSE JAM!!!"
    If RN = 17 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE WINDMILL JAM!!!"
    If RN = 18 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE WINDMILL SLAM!!!"
    If RN = 19 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE TOMAHAWK SLAM!!!"
    If RN = 20 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE 360 SLAM!!!"
    If RN = 21 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE SPINNING SLAM!!!"
    If RN = 22 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE LEFT HANDED DUNK!!!"
    If RN = 23 Then U$ = "DOUBLE CLUTCH JAM!!!"
    If RN = 24 Then U$ = AA$(P, B1(P, B), 0) + " GETS THE FLUSH!!!"
    If RN = 25 Then U$ = AA$(P, B1(P, B), 0) + " PUNCHES IT HOME!!!"
    If RN = 26 Then U$ = AA$(P, B1(P, B), 0) + "...DOUBLE PUMP SLAM!!!"
    If RN = 27 Then U$ = AA$(P, B1(P, B), 0) + "...IN YOUR FACE SLAM!!!"
    If RN = 28 Then U$ = AA$(P, B1(P, B), 0) + " IS UP": Call DELAY: U$ = "HE SPINS": Call DELAY: U$ = "BEHIND THE BACK SLAM!!!"
    If RN = 29 Then U$ = "HE SLAMS IT HOME!!"
    If RN = 30 Then U$ = "HE LAYS IT UP AND IN!!"
    If RN = 31 Then U$ = "HE BANGS IT IN!!"
    If RN = 32 Then U$ = "HE RAMS IT IN!!"
    If RN = 33 Then U$ = "HE SLAMS IT IN!!!"
    If RN = 34 Then U$ = "DIPSY-DOO DUNK-A-ROO!!!"
    If RN = 35 Then U$ = "HE MAKES IT HAMMER TIME!!!"
    If RN = 36 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE 2-HANDED SLAM!!!"
    If RN = 37 Then U$ = AA$(P, B1(P, B), 0) + " IS UP": Call DELAY: U$ = "HE SPINS": Call DELAY: U$ = "BEHIND THE BACK SLAM!!!"
    If RN = 38 Then U$ = "HE JAMS IT HOME!!!"
    If RN = 39 Then U$ = "HE HAMMERS IT HOME!"
    GoTo 1495

    1425 If Int(Rnd(1) * 100) + 1 <= 15 Then DK% = 1
    Return

    1426 If Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
    Return

    1427 If Int(Rnd(1) * 100) + 1 <= 2 Then DK% = 1
    Return

    1428 If (B = 0 Or B = 1) And Int(Rnd(1) * 100) + 1 <= 1 Then DK% = 1
    If (B = 2 Or B = 3) And Int(Rnd(1) * 100) + 1 <= 5 Then DK% = 1
    If B = 4 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
    Return

    1430 'SHOOTS...PZ%567 SML
    U$ = AA$(P, B1(P, B), 0)
    On PZ% GOTO 1431, 1432, 1433, 1434, 1435, 1436, 1436, 1438, 1439

    1431 'BAD
    If RN <= 40 Then U$ = U$ + " TAKES A TOUGH SHOT": GoTo 1495
    If RN <= 45 Then U$ = U$ + " SHOOTS OFF BALANCE": GoTo 1495
    If RN <= 50 Then U$ = U$ + " FORCES ONE UP": GoTo 1495
    If RN <= 60 Then U$ = U$ + " SHOOTS UNDER PRESSURE": GoTo 1495
    If RN <= 65 Then U$ = U$ + " WITH A BAD SHOT": GoTo 1495
    If RN <= 70 Then U$ = U$ + " SHOOTS IN HEAVY TRAFFIC": GoTo 1495
    If RN <= 75 Then U$ = U$ + " THROWS UP A PRAYER": GoTo 1495
    If RN <= 80 Then U$ = U$ + " TAKES A BAD SHOT": GoTo 1495
    If RN <= 85 Then U$ = U$ + " FORCES THE INSIDE SHOT": GoTo 1495
    U$ = U$ + " TURNS AND SHOOTS": GoTo 1495

    1432 'INSIDE W/ASSIST
    If RN <= 10 Then U$ = "HE CUTS TO THE BASKET": Call DELAY: U$ = "TAKES IT UP": GoSub 1428: GoTo 1495
    If RN <= 20 Then U$ = "HE TAKES IT TO THE HOLE": GoSub 1428: GoTo 1495
    If RN <= 30 Then U$ = "HE TAKES IT TO THE HOOP": GoSub 1428: GoTo 1495
    If RN <= 40 Then U$ = "HE LEANS IN AND SHOOTS": GoTo 1495
    If RN <= 50 Then U$ = "HE PUTS IT UP": GoTo 1495
    If RN <= 60 Then U$ = "HE TAKES IT TO THE BOARD": GoSub 1428: GoTo 1495
    If RN <= 70 Then U$ = "HE PUTS IT OFF THE GLASS": GoTo 1495
    If RN <= 80 And B = 4 Then U$ = "HE SHOOTS THE HOOK": GoTo 1495
    If RN <= 80 And B <> 4 Then U$ = "HE GOES IN FOR THE LAYUP": GoTo 1495
    If RN <= 90 Then U$ = "HE TAKES IT UP STRONG": GoSub 1428: GoTo 1495
    If RN <= 95 Then U$ = "HE POWERS IT UP": GoSub 1428: GoTo 1495
    U$ = "HE LAYS IT UP": GoTo 1495

    1433 'PENETRATE
    If Z3$ = "C" And RN <= 15 Then U$ = "HE PUTS IT OFF THE GLASS": GoTo 1495
    If Z3$ = "C" And RN <= 25 Then U$ = "HE PIVOTS TO THE BASKET": GoTo 1495
    If Z3$ = "C" And RN <= 40 Then U$ = "HE POWERS IT UP": GoTo 1495
    If Z3$ = "C" And RN <= 58 Then U$ = "HE TAKES IT TO THE HOOP": GoSub 1425: GoTo 1495
    If Z3$ = "C" And RN <= 70 Then U$ = "HE TAKES THE SHOT": GoTo 1495
    If Z3$ = "C" And RN <= 72 Then U$ = "HE CURLS OFF A PICK IN THE PAINT": Call DELAY: U$ = "TAKES IT UP": GoSub 1425: GoTo 1495
    If Z3$ = "C" And RN <= 74 Then U$ = "HE MAKES A NICE SPIN MOVE": Call DELAY: U$ = "AND SOARS TO THE HOOP": GoSub 1425: GoTo 1495
    If Z3$ = "C" Then U$ = "HE TAKES IT UP STRONG": GoSub 1425: GoTo 1495
    If Z3$ = "F" And RN <= 16 Then U$ = "HE TAKES IT UP": GoSub 1426: GoTo 1495
    If Z3$ = "F" And RN <= 32 Then U$ = "HE SPINS FREE": Call DELAY: U$ = "SHOOTS": GoSub 1426: GoTo 1495
    If Z3$ = "F" And RN <= 48 Then U$ = "HE GETS LOOSE IN THE PAINT": Call DELAY: U$ = "TAKES IT UP": GoSub 1426: GoTo 1495
    If Z3$ = "F" And RN <= 64 Then U$ = "HE PUTS IT UP": GoTo 1495
    If Z3$ = "F" And RN <= 67 Then U$ = "HE CURLS OFF A PICK IN THE PAINT": Call DELAY: U$ = "TAKES IT UP": GoSub 1426: GoTo 1495
    If Z3$ = "F" And RN <= 69 Then U$ = "HE MAKES A NICE SPIN MOVE": Call DELAY: U$ = "AND SOARS TO THE HOOP": GoSub 1426: GoTo 1495
    If Z3$ = "F" And RN <= 71 Then U$ = "HE DRIVES THE BASELINE": Call DELAY: U$ = "SPINS RIGHT AND GOES UP": GoSub 1426: GoTo 1495
    If Z3$ = "F" And RN = 72 Then U$ = "HE SLICES IN THE HOLE": Call DELAY: U$ = "UNDERHAND SCOOP SHOT": GoTo 1495
    If Z3$ = "F" And RN = 73 Then U$ = "STRONG CUT ALONG THE BASELINE": Call DELAY: U$ = "DOUBLE PUMP REVERSE LAYUP": GoTo 1495
    If Z3$ = "F" And RN = 74 Then U$ = "HE PENETRATES": Call DELAY: U$ = "GORGEOUS FINGER ROLL!": GoTo 1495
    If Z3$ = "F" And RN > 80 Then U$ = "HE SHOOTS": GoTo 1495
    If Z3$ = "F" Then U$ = "HE TAKES THE SHOT": GoTo 1495
    If RN <= 25 Then U$ = "HE SPINS FREE": Call DELAY: U$ = "SHOOTS": GoTo 1495
    If RN <= 50 Then U$ = "HE TAKES IT UP UNDER THE BASKET": GoSub 1427: GoTo 1495
    If RN <= 52 Then U$ = "HE MAKES A NICE SPIN MOVE": Call DELAY: U$ = "AND SOARS TO THE HOOP": GoSub 1427: GoTo 1495
    If RN <= 54 Then U$ = "HE BLOWS PAST TWO DEFENDERS": GoTo 1495
    If RN <= 56 Then U$ = "HE DRIVES THE BASELINE": Call DELAY: U$ = "SPINS RIGHT AND GOES UP": GoSub 1427: GoTo 1495
    If RN = 57 Then U$ = "HE SLICES IN THE HOLE": Call DELAY: U$ = "UNDERHAND SCOOP SHOT": GoTo 1495
    If RN = 58 Then U$ = "STRONG CUT ALONG THE BASELINE": Call DELAY: U$ = "DOUBLE PUMP REVERSE LAYUP": GoTo 1495
    If RN = 59 Then U$ = "HE PENETRATES": Call DELAY: U$ = "GORGEOUS FINGER ROLL!": GoTo 1495
    If RN <= 75 Then U$ = "HE TAKES THE JUMPER": GoTo 1495
    If RN <= 90 Then U$ = "HE STOPS": Call DELAY: U$ = "SHOOTS": GoTo 1495
    If RN <= 91 Then U$ = "HE LOOKS TO PASS": Call DELAY: U$ = "NO ONE IS OPEN": Call DELAY: U$ = "HE PUTS IT UP": GoTo 1495
    U$ = "HE TAKES THE SHOT": GoTo 1495

    1434 'THREE!
    If FT% = 1 And RN <= 50 Then U$ = U$ + " THROWS UP A PRAYER"
    If FT% = 1 And RN > 50 Then U$ = U$ + " LAUNCHES A THREE"
    If FT% = 1 Then FT% = 0: GoTo 1495
    If RN <= 10 Then U$ = U$ + " TRIES FOR THREE!"
    If RN > 10 And RN <= 20 Then U$ = U$ + " SHOOTS FROM 3-POINT RANGE!"
    If RN > 20 And RN <= 30 Then U$ = U$ + " HOISTS A THREE!"
    If RN > 30 And RN <= 40 Then U$ = U$ + " BOMBS A THREE FROM THE CORNER!"
    If RN > 40 And RN <= 50 Then U$ = U$ + " FIRES FROM THE 3-POINT LINE!"
    If RN > 50 And RN <= 60 Then U$ = U$ + " SHOOTS FOR THREE!"
    If RN > 60 And RN <= 70 Then U$ = U$ + " SHOOTS THE 3-POINTER!"
    If RN > 70 And RN <= 80 Then U$ = U$ + " FOR 3...!"
    If RN > 80 And RN <= 90 Then U$ = U$ + " PUTS UP THE TREY!"
    If RN > 90 Then U$ = U$ + " TAKES THE 3-POINTER!"
    GoTo 1495

    1435 'SHORT JUMP
    RN = Int(Rnd(1) * 59) + 1
    If RN = 1 Then U$ = U$ + " WITH A TWISTING JUMPER"
    If RN = 2 Then U$ = U$ + " SHOOTS THE J"
    If RN = 3 And B = 4 Then U$ = U$ + " RELEASES THE HOOK"
    If RN = 3 And B <> 4 Then U$ = U$ + " TAKES THE SHOT"
    If RN = 4 Then U$ = U$ + " WITH A SHORT JUMPER"
    If RN = 5 Then U$ = U$ + " WITH THE 10 FOOTER"
    If RN = 6 And B <> 4 Then U$ = U$ + " WITH THE RUNNER"
    If RN = 6 And B = 4 Then U$ = U$ + " WITH THE RUNNING HOOK"
    If RN = 7 And B = 4 Then U$ = U$ + " WITH A SWEEPING HOOK"
    If RN = 7 And B <> 4 Then U$ = U$ + " WORKS FREE AND SHOOTS"
    If RN = 8 And B <> 4 Then U$ = U$ + " TAKES THE SHOT"
    If RN = 8 And B = 4 Then U$ = U$ + " WITH A BABY HOOK SHOT"
    If RN = 9 Then U$ = U$ + " LEANS IN AND SHOOTS"
    If RN = 10 Then U$ = U$ + " SHOOTS"
    If RN = 11 Then U$ = U$ + " SHOOTS FALLING AWAY"
    If RN = 12 Then U$ = U$ + " LEANS IN FOR THE SHOT"
    If RN = 13 Then U$ = U$ + " TAKES THE LEFT HANDER"
    If RN = 14 Then U$ = U$ + " HARD TO THE HOLE..."
    If RN = 15 Then U$ = U$ + " FLIPS IT UP"
    If RN = 16 Then U$ = U$ + " FORCES THE INSIDE SHOT"
    If RN = 17 Then U$ = U$ + " CUTS TO THE BASKET"
    If RN = 18 Then U$ = U$ + " TAKES IT IN THE PAINT"
    If RN = 19 Then U$ = U$ + " FLOATS IT OVER HIS MAN"
    If RN = 20 Then U$ = U$ + " TAKES HIS MAN INSIDE"
    If RN = 21 Then U$ = U$ + " WHEELS IN THE LANE"
    If RN = 22 Then U$ = U$ + " CUTS INSIDE FOR THE SHOT"
    If RN = 23 Then U$ = U$ + " PENETRATES AND SHOOTS"
    If RN = 24 Then U$ = U$ + " SLICES INSIDE"
    If RN = 25 Then U$ = U$ + " LOOKS FOR TWO"
    If RN = 26 Then U$ = U$ + " DRIVES INTO THE PAINT"
    If RN = 27 Then U$ = U$ + " HAS THE BASELINE JUMPER"
    If RN = 28 Then U$ = U$ + " GLIDES TO THE HOLE"
    If RN = 29 Then U$ = U$ + " GETS INSIDE FOR THE SHOT"
    If RN = 30 Then U$ = U$ + " SPINS TOWARD THE HOOP"
    If RN = 31 Then U$ = U$ + " FROM SHORT RANGE"
    If RN = 32 Then U$ = U$ + " GOES BASELINE"
    If RN = 33 Then U$ = U$ + " PENETRATES AND SHOOTS"
    If RN = 34 Then U$ = U$ + " FROM 8 FEET"
    If RN = 35 Then U$ = U$ + " FROM 6 FEET"
    If RN = 36 Then U$ = U$ + " FROM 5 FEET"
    If RN = 37 Then U$ = U$ + " DRIVES TO THE HOLE"
    If RN = 38 Then U$ = U$ + " GLIDES TO THE HOLE"
    If RN = 39 Then U$ = U$ + " GETS INSIDE FOR THE SHOT"
    If RN = 40 Then U$ = U$ + " GOES TO THE HOLE"
    If RN = 41 Then U$ = U$ + " CUTS INSIDE"
    If RN = 42 Then U$ = U$ + " SPINS INSIDE"
    If RN = 43 Then U$ = U$ + " BREAKS INSIDE"
    If RN = 44 Then U$ = U$ + " SNEAKS INSIDE"
    If RN = 45 Then U$ = U$ + " SCOOTS INSIDE"
    If RN = 46 Then U$ = U$ + " WORKS IT INSIDE"
    If RN = 47 Then U$ = U$ + " KNIFES INSIDE"
    If RN = 48 Then U$ = U$ + " SCISSORS THROUGH"
    If RN = 49 Then U$ = U$ + " BANGS INSIDE"
    If RN = 50 Then U$ = U$ + " SLICES INSIDE"
    If RN = 51 Then U$ = U$ + " PUMPS AND SHOOTS"
    If RN = 52 Then U$ = U$ + " DOUBLE PUMPS AND SHOOTS"
    If RN = 53 Then U$ = U$ + "...ONE HANDED FADE AWAY"
    If RN = 54 Then U$ = U$ + "...LEFT HANDED FADE AWAY"
    If RN = 55 Then U$ = U$ + "...HIGH ARCHING SHOT"
    If RN = 56 Then U$ = U$ + " WITH A RUNNING LEFT HANDER"
    If RN = 57 Then U$ = U$ + " WITH A RUNNING SCOOP SHOT"
    If RN = 58 Then U$ = U$ + " WITH THE SCOOP SHOT"
    If RN = 59 Then U$ = U$ + " WITH THE FINGER ROLL"
    GoTo 1495

    1436 RN = Int(Rnd(1) * 59) + 1
    If RN = 1 Then U$ = U$ + " POPS FROM OUTSIDE"
    If RN = 2 Then U$ = U$ + " DECIDES TO SHOOT IT"
    If RN = 3 Then U$ = U$ + " FIRES THE FADE-AWAY"
    If RN = 4 Then U$ = U$ + " LETS IT FLY"
    If RN = 5 Then U$ = U$ + " LAUNCHES ONE"
    If RN = 6 Then U$ = U$ + " WITH THE OUTSIDE SHOT"
    If RN = 7 Then U$ = U$ + " OUT OF THE CORNER"
    If RN = 8 Then U$ = U$ + " PUTS IT UP FROM 18"
    If RN = 9 Then U$ = U$ + " SPOTS UP OUTSIDE"
    If RN = 10 Then U$ = U$ + " RELEASES THE JUMPER"
    If RN = 11 Then U$ = U$ + " FROM THE TOP OF THE KEY"
    If RN = 12 Then U$ = U$ + " FIRES FROM THE LEFT WING"
    If RN = 13 Then U$ = U$ + " TRIES THE 12 FOOTER"
    If RN = 14 Then U$ = U$ + " PULLS UP AND POPS"
    If RN = 15 Then U$ = U$ + " FIRES FROM 15"
    If RN = 16 Then U$ = U$ + " FROM THE PERIMETER"
    If RN = 17 Then U$ = U$ + " TURNS AND SHOOTS"
    If RN = 18 Then U$ = U$ + " SHOOTS OFF THE SCREEN"
    If RN = 19 Then U$ = U$ + " FAKES AND SHOOTS"
    If RN = 20 Then U$ = U$ + " FIRES AWAY"
    If RN = 21 Then U$ = U$ + " POPS FROM THE BASELINE"
    If RN = 22 Then U$ = U$ + " DRIVES THE LANE"
    If RN = 23 Then U$ = U$ + " TAKES THE SHOT"
    If RN = 24 Then U$ = U$ + " WORKS FREE AND FIRES"
    If RN = 25 Then U$ = U$ + " SHOOTS OVER HIS MAN"
    If RN = 26 Then U$ = U$ + " SHOOTS FALLING AWAY"
    If RN = 27 Then U$ = U$ + " FAKES AND SHOOTS"
    If RN = 28 Then U$ = U$ + " FIRES AWAY"
    If RN = 29 Then U$ = U$ + " POPS FROM THE BASELINE"
    If RN = 30 Then U$ = U$ + " DRIVES THE LANE"
    If RN = 31 Then U$ = U$ + " TAKES THE SHOT"
    If RN = 32 Then U$ = U$ + " WORKS FREE AND FIRES"
    If RN = 33 Then U$ = U$ + " SHOOTS FROM 15"
    If RN = 34 Then U$ = U$ + " SHOOTS"
    If RN = 35 Then U$ = U$ + " TAKES THE JUMPER"
    If RN = 36 Then U$ = U$ + " FROM THE RIGHT CORNER"
    If RN = 37 Then U$ = U$ + " FROM THE LEFT CORNER"
    If RN = 38 Then U$ = U$ + " FROM THE KEY"
    If RN = 39 Then U$ = U$ + " FIRES FROM OUTSIDE"
    If RN = 40 Then U$ = U$ + " PUTS IT UP FROM 16"
    If RN = 41 Then U$ = U$ + " FROM THE PERIMETER"
    If RN = 42 Then U$ = U$ + " FAKES AND SHOOTS"
    If RN = 43 Then U$ = U$ + " FROM AROUND 16 FEET"
    If RN = 44 Then U$ = U$ + " PUTS IT UP"
    If RN = 45 Then U$ = U$ + " WITH A RUNNING ONE-HANDER"
    If RN = 46 Then U$ = U$ + " PULLS UP"
    If RN = 47 Then U$ = U$ + " KISSES THE GLASS"
    If RN = 48 Then U$ = U$ + " SPINS AND SHOOTS"
    If RN = 49 Then U$ = U$ + " PUTS IT UP FROM 16"
    If RN = 50 Then U$ = U$ + " WITH A LONG JUMPER"
    If RN = 51 Then U$ = U$ + " PUMPS AND SHOOTS"
    If RN = 52 Then U$ = U$ + " DOUBLE PUMPS AND SHOOTS"
    If RN = 53 Then U$ = U$ + "...ONE HANDED FADE AWAY"
    If RN = 54 Then U$ = U$ + "...LEFT HANDED FADE AWAY"
    If RN = 55 Then U$ = U$ + "...HIGH ARCHING SHOT"
    If RN = 56 Then U$ = U$ + " WITH A RUNNING LEFT HANDER"
    If RN = 57 Then U$ = U$ + " WITH A RUNNING SCOOP SHOT"
    If RN = 58 Then U$ = U$ + " WITH THE SCOOP SHOT"
    If RN = 59 Then U$ = U$ + " WITH THE FINGER ROLL"
    GoTo 1495

    1438 If RN <= 20 Then U$ = "HE SAILS TO THE HOOP": GoTo 1495
    If RN <= 23 Then U$ = "HE TAKES IT TO THE BUCKET": GoTo 1495
    If RN <= 26 Then U$ = "HE TAKES IT TO THE HOLE": GoTo 1495
    If RN <= 29 Then U$ = "HE SLASHES TO THE HOLE": GoTo 1495
    If RN <= 32 Then U$ = "HE SLASHES TO THE BUCKET": GoTo 1495
    If RN <= 35 Then U$ = "HE SOARS TO THE BUCKET": GoTo 1495
    If RN <= 38 Then U$ = "HE SOARS TO THE HOLE": GoTo 1495
    If RN <= 40 Then U$ = "HE GLIDES TO THE HOLE": GoTo 1495
    If RN <= 60 Then U$ = "HE GOES IN STRONG": GoTo 1495
    If RN <= 80 Then U$ = "HE TAKES IT IN": GoTo 1495
    If RN <= 90 Then U$ = "HE TAKES A DEFENDER WITH HIM": GoTo 1495
    U$ = "HE GOES UP WITH IT": GoTo 1495

    1439 'BACK AFTER REB
    U$ = "HE "
    RN = Int(Rnd(1) * 11) + 1
    If RN = 1 Then U$ = U$ + "TAKES IT BACK UP"
    If RN = 2 Then U$ = U$ + "GOES BACK UP WITH IT"
    If RN = 3 Then U$ = U$ + "POWERS HIS WAY UP WITH IT"
    If RN = 4 Then U$ = U$ + "GOES STRONG TO THE BASKET"
    If RN = 5 Then U$ = U$ + "MUSCLES IT UP"
    If RN = 6 Then U$ = U$ + "SHOOTS"
    If RN = 7 Then U$ = U$ + "TAKES IT TO THE GOAL"
    If RN = 8 Then U$ = U$ + "PUTS IT UP AGAIN"
    If RN = 9 Then U$ = U$ + "SHOOTS IT AGAIN"
    If RN = 10 Then U$ = U$ + "CLAWS HIS WAY UP"
    If RN = 11 Then U$ = U$ + "SHOOTS IN TRAFFIC"
    If B = 2 Or B = 3 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
    If B = 4 And Int(Rnd(1) * 100) + 1 <= 15 Then DK% = 1
    GoTo 1495

    1440 'THREE GOOD
    U$ = "HE "
    If RN <= 10 Then U$ = U$ + "FILLS UP THE BASKET!!!": GoTo 1495
    If RN <= 20 Then U$ = U$ + "HITS THE TREY!!!": GoTo 1495
    If RN <= 30 Then U$ = "IT'S GOOOODD!!": GoTo 1495
    If RN <= 40 Then U$ = U$ + "GETS IT!!!": GoTo 1495
    If RN <= 50 Then U$ = "COUNT IT!!": GoTo 1495
    If RN <= 60 Then U$ = U$ + "RINGS IT UP!!!": GoTo 1495
    If RN <= 80 Then U$ = U$ + "DRAINS IT!!!": GoTo 1495
    If RN <= 90 Then U$ = "HE BURIES IT!!!": GoTo 1495
    U$ = "IT'S ALL NET!!!"
    GoTo 1495

    1445 'THREE BAD
    U$ = "HE "
    If RN <= 20 Then U$ = U$ + "GETS NONE": GoTo 1495
    If RN <= 25 Then U$ = "IT NEVER HAD A CHANCE": GoTo 1495
    If RN <= 35 Then U$ = "AIRRRR BALLLL!!": GoTo 1495
    If RN <= 40 Then U$ = U$ + "MISFIRES": GoTo 1495
    If RN <= 60 Then U$ = "IT'S OFF THE RIM...NO GOOD!": GoTo 1495
    If RN <= 80 Then U$ = "IT'S SHORT": GoTo 1495
    U$ = U$ + "MISSES": GoTo 1495

    1450 'FAST BREAK
    U$ = AA$(P, B1(P, B), 0)
    RN = Int(Rnd(1) * 38) + 1
    If RN = 1 Then U$ = U$ + " HAS IT ON THE FAST BREAK!"
    If RN = 2 Then U$ = U$ + " IS IN TRANSITION!"
    If RN = 3 Then U$ = U$ + " STARTS THE BREAK!"
    If RN = 4 Then U$ = U$ + " IS DOWN COURT IN A HURRY!"
    If RN = 5 Then U$ = U$ + " IS ON THE RUN!"
    If RN = 6 Then U$ = U$ + " GOES COAST TO COAST!"
    If RN = 7 Then U$ = U$ + " ON THE BREAK!"
    If RN = 8 Then U$ = U$ + " ON THE RUN!"
    If RN = 9 Then U$ = U$ + " ON THE FAST BREAK!"
    If RN = 10 Then U$ = U$ + " HAS THE FAST BREAK!"
    If RN = 11 Then U$ = U$ + " BREAKS INTO THE FORECOURT!"
    If RN = 12 Then U$ = U$ + " FLIES UPCOURT!"
    If RN = 13 Then U$ = U$ + " STARTS THE FASTBREAK!"
    If RN = 14 Then U$ = U$ + " ON THE MOVE!"
    If RN = 15 Then U$ = U$ + " IN TRANSITION!"
    If RN = 16 Then U$ = U$ + " DOWN COURT IN A HURRY!"
    If RN = 17 Then U$ = U$ + " CHARGES ACROSS THE LINE!"
    If RN = 18 Then U$ = U$ + " BREAKS QUICKLY!"
    If RN = 19 Then U$ = U$ + " BREAKS DOWN COURT!"
    If RN = 20 Then U$ = U$ + " STARTS THE RUNNING GAME!"
    If RN = 21 Then U$ = U$ + " FLIES DOWN THE FLOOR!"
    If RN = 22 Then U$ = U$ + " GOES RIGHT TO WORK!"
    If RN = 23 Then U$ = U$ + " BACK THE OTHER WAY!"
    If RN = 24 Then U$ = U$ + " QUICKLY UP THE FLOOR!"
    If RN = 25 Then U$ = U$ + " PUSHES IT UP COURT!"
    If RN = 26 Then U$ = U$ + " WASTES NO TIME!"
    If RN = 27 Then U$ = U$ + " UPS THE TEMPO!"
    If RN = 28 Then U$ = U$ + " HUSTLES DOWN COURT!"
    If RN = 29 Then U$ = U$ + " RUSHES UP THE FLOOR!"
    If RN = 30 Then U$ = U$ + " ON THE MOVE!"
    If RN = 31 Then U$ = U$ + " STREAKS UP THE COURT!"
    If RN = 32 Then U$ = U$ + " FLIES DOWN THE COURT!"
    If RN = 33 Then U$ = U$ + " JETS DOWN COURT!"
    If RN = 34 Then U$ = U$ + " QUICKLY GETS IT UP COURT!"
    If RN = 35 Then U$ = U$ + " TAKES IT COAST TO COAST!"
    If RN = 36 Then U$ = U$ + " IS RUNNING AND GUNNING!"
    If RN = 37 Then U$ = U$ + " FLIES OVER THE TIME LINE!"
    If RN = 38 Then U$ = U$ + " RUSHES IT UP THE FLOOR!"
    GoTo 1495

    1460 'FAST BREAK SCORE
    FB% = 0
    RN = Int(Rnd(1) * 39) + 1
    If RN = 1 Then U$ = "DIPSY-DOO DUNK-A-ROO!!!"
    If RN = 2 Then U$ = AA$(P, B1(P, B), 0) + " MAKES IT HAMMER TIME!!!"
    If RN = 3 Then U$ = AA$(P, B1(P, B), 0) + " SLAMS IT IN!!!"
    If RN = 4 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE 2-HANDED SLAM!!!"
    If RN = 5 Then U$ = AA$(P, B1(P, B), 0) + " SLAMS IT HOME!!!"
    If RN = 6 Then U$ = AA$(P, B1(P, B), 0) + " JAMS IT HOME!!!"
    If RN = 7 Then U$ = AA$(P, B1(P, B), 0) + " THROWS IT DOWN!!!"
    If RN = 8 Then U$ = AA$(P, B1(P, B), 0) + " THUNDERSLAM!!!"
    If RN = 9 Then U$ = "RIM ROCKING JAM!!!"
    If RN = 10 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE MONSTER SLAM!!"
    If RN = 11 Then U$ = AA$(P, B1(P, B), 0) + "POWER SLAMS IT!!!"
    If RN = 12 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE 2-HANDED STUFF!!!"
    If RN = 13 Then U$ = AA$(P, B1(P, B), 0) + " BACKWARDS JAM!!!"
    If RN = 14 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE 1-HANDED SLAM!!!"
    If RN = 15 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE REVERSE SLAM!!!"
    If RN = 16 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE REVERSE JAM!!!"
    If RN = 17 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE WINDMILL JAM!!!"
    If RN = 18 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE WINDMILL SLAM!!!"
    If RN = 19 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE TOMAHAWK SLAM!!!"
    If RN = 20 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE 360 SLAM!!!"
    If RN = 21 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE SPINNING SLAM!!!"
    If RN = 22 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE LEFT HANDED DUNK!!!"
    If RN = 23 Then U$ = "DOUBLE CLUTCH JAM!!!"
    If RN = 24 Then U$ = AA$(P, B1(P, B), 0) + " GETS THE FLUSH!!!"
    If RN = 25 Then U$ = AA$(P, B1(P, B), 0) + " PUNCHES IT HOME!!!"
    If RN = 26 Then U$ = AA$(P, B1(P, B), 0) + "...DOUBLE PUMP SLAM!!!"
    If RN = 27 Then U$ = AA$(P, B1(P, B), 0) + "...IN YOUR FACE SLAM!!!"
    If RN = 28 Then U$ = AA$(P, B1(P, B), 0) + " IS UP": Call DELAY: U$ = "HE SPINS": Call DELAY: U$ = "BEHIND THE BACK SLAM!!!"
    If RN = 29 Then U$ = "HE SLAMS IT HOME!!"
    If RN = 30 Then U$ = "HE LAYS IT UP AND IN!!"
    If RN = 31 Then U$ = "HE BANGS IT IN!!"
    If RN = 32 Then U$ = "HE RAMS IT IN!!"
    If RN = 33 Then U$ = "HE SLAMS IT IN!!!"
    If RN = 34 Then U$ = "DIPSY-DOO DUNK-A-ROO!!!"
    If RN = 35 Then U$ = "HE MAKES IT HAMMER TIME!!!"
    If RN = 36 Then U$ = AA$(P, B1(P, B), 0) + " WITH THE 2-HANDED SLAM!!!"
    If RN = 37 Then U$ = AA$(P, B1(P, B), 0) + " IS UP": Call DELAY: U$ = "HE SPINS": Call DELAY: U$ = "BEHIND THE BACK SLAM!!!"
    If RN = 38 Then U$ = "HE JAMS IT HOME!!!"
    If RN = 39 Then U$ = "HE HAMMERS IT HOME!"
    GoTo 1495

    1470 'MISS
    If PZ% = 8 Then PZ% = 0: FB% = 0: GoTo 1475
    RN = Int(Rnd(1) * 50) + 1
    If RN = 1 Then U$ = "IT RATTLES IN AND OUT...NO GOOD"
    If RN = 2 Then U$ = "OFF THE IRON"
    If RN = 3 Then U$ = "IT SPINS IN AND OUT"
    If RN = 4 Then U$ = "IT'S OFF THE GLASS"
    If RN = 5 Then U$ = "IT'S NO GOOD"
    If RN = 6 Then U$ = "IT'S OFF THE MARK"
    If RN = 7 Then U$ = "IT'S A BRICK"
    If RN = 8 Then U$ = "OFF THE RIM"
    If RN = 9 Then U$ = "NOT EVEN CLOSE!"
    If RN = 10 Then U$ = "NO GOOD"
    If RN = 11 Then U$ = "HE MISSES"
    If RN = 12 Then U$ = "DOESN'T GET THE ROLL"
    If RN = 13 Then U$ = "OFF THE GLASS"
    If RN = 14 Then U$ = "FALLS SHORT"
    If RN = 15 Then U$ = "NO GOOD"
    If RN = 16 Then U$ = "NO"
    If RN = 17 Then U$ = "HE MISSES"
    If RN = 18 Then U$ = "IN AND OUT"
    If RN = 19 Then U$ = "OFF THE RIM"
    If RN = 20 Then U$ = "OFF THE IRON"
    If RN = 21 Then U$ = "IT'S NOT THERE"
    If RN = 22 Then U$ = "OFF TARGET"
    If RN = 23 Then U$ = "IT RATTLES OUT"
    If RN = 24 Then U$ = "IT DOESN'T FALL"
    If RN = 25 Then U$ = "IT BOUNCES OFF"
    If RN = 26 Then U$ = "IT ROLLS OFF"
    If RN = 27 Then U$ = "SHORT"
    If RN = 28 Then U$ = "IT DOESN'T GO DOWN"
    If RN = 29 Then U$ = "IT'S NO GOOD"
    If RN = 30 Then U$ = "NO GOOD"
    If RN = 31 Then U$ = "HE MISSES"
    If RN = 32 Then U$ = "IT'S A BRICK"
    If RN = 33 Then U$ = "HE FAILS TO CONNECT"
    If RN = 34 Then U$ = "OFF THE SIDE OF THE RIM"
    If RN = 35 Then U$ = "IT COMES UP SHORT"
    If RN = 36 Then U$ = "IT ROLLS OUT"
    If RN = 37 Then U$ = "AROUND AND OUT"
    If RN = 38 Then U$ = "OFF THE BACK OF THE IRON"
    If RN = 39 Then U$ = "ON THE FRONT RIM...ROLLS OFF"
    If RN = 40 Then U$ = "IN THE AIR...NO GOOD"
    If RN = 41 Then U$ = "TOO SHORT"
    If RN = 42 Then U$ = "NO GOOD"
    If RN = 43 Then U$ = "IT COMES UP SHORT"
    If RN = 44 Then U$ = "IT ROLLS OUT"
    If RN = 45 Then U$ = "IN AND OUT"
    If RN = 46 Then U$ = "OFF THE FRONT OF THE IRON"
    If RN = 47 Then U$ = "ON THE RIM...ROLLS OFF"
    If RN = 48 Then U$ = "OFF THE FRONT OF THE RIM"
    If RN = 49 Then U$ = "TOO SHORT"
    If RN = 50 Then U$ = "TOO STRONG"
    GoTo 1495

    1475 U$ = "HE "
    If RN <= 20 Then U$ = U$ + "MISSES THE EASY BASKET!": GoTo 1495
    If RN <= 40 Then U$ = U$ + "JAMS IT TOO HARD!!": GoTo 1495
    If RN <= 60 Then U$ = U$ + "SLAMS IT OFF THE IRON...NO GOOD!": GoTo 1495
    If RN <= 80 Then U$ = U$ + "CAN'T GET IT DOWN!": GoTo 1495
    U$ = U$ + "BLOWS THE EASY BASKET!!": GoTo 1495

    1480 'NOPASS
    U$ = AA$(P, B1(P, B), 0) + " "
    RN = Int(Rnd(1) * 56) + 1
    If RN = 1 Then U$ = U$ + "LOOKS INSIDE"
    If RN = 2 Then U$ = U$ + "TRIES TO WORK FREE"
    If RN = 3 Then U$ = U$ + "SETS UP THE PLAY"
    If RN = 4 Then U$ = U$ + "LOOKS FOR AN OPENING"
    If RN = 5 Then U$ = U$ + "DRIBBLES AROUND HIS MAN"
    If RN = 6 Then U$ = U$ + "SIGNALS THE PLAY"
    If RN = 7 Then U$ = U$ + "IS IN CONTROL"
    If RN = 8 Then U$ = U$ + "IS IN POSSESSION"
    If RN = 9 Then U$ = U$ + "TRIES TO GET OPEN"
    If RN = 10 Then U$ = U$ + "YO-YO'S OUT TOP"
    If RN = 11 Then U$ = U$ + "GOES TO WORK"
    If RN = 12 Then U$ = U$ + "WORKS OFF THE PICK"
    If RN = 13 Then U$ = U$ + "ON THE DRIBBLE"
    If RN = 14 Then U$ = U$ + "CUTS TO THE LEFT"
    If RN = 15 Then U$ = U$ + "CUTS TO THE RIGHT"
    If RN = 16 Then U$ = U$ + "WHEELS AND DEALS"
    If RN = 17 Then U$ = U$ + "SETS UP ON TOP"
    If RN = 18 Then U$ = U$ + "SLIDES RIGHT"
    If RN = 19 Then U$ = U$ + "SLIDES LEFT"
    If RN = 20 Then U$ = U$ + "GOES TO WORK"
    If RN = 21 Then U$ = U$ + "MOVES IT LEFT-SIDE"
    If RN = 22 Then U$ = U$ + "MOVES IT RIGHT-SIDE"
    If RN = 23 Then U$ = U$ + "REVERSES THE DRIBBLE"
    If RN = 24 Then U$ = U$ + "TRIES TO GET OPEN"
    If RN = 25 Then U$ = U$ + "DRIBBLES TO THE MIDDLE"
    If RN = 26 Then U$ = U$ + "FAKES THE PASS"
    If RN = 27 Then U$ = U$ + "SETS UP ON TOP"
    If RN = 28 Then U$ = U$ + "SLIDES RIGHT"
    If RN = 29 Then U$ = U$ + "SLIDES LEFT"
    If RN = 30 Then U$ = U$ + "DRIBBLES IT BEHIND THE BACK"
    If RN = 31 Then U$ = U$ + "FAKES LEFT...GOES RIGHT"
    If RN = 32 Then U$ = U$ + "FAKES RIGHT...GOES LEFT"
    If RN = 33 Then U$ = U$ + "SPINS HARD TO THE LEFT"
    If RN = 34 Then U$ = U$ + "SPINS HARD TO THE RIGHT"
    If RN = 35 Then U$ = U$ + "LEANS LEFT...GOES RIGHT"
    If RN = 36 Then U$ = U$ + "LEANS RIGHT...GOES LEFT"
    If RN = 37 Then U$ = U$ + "LEANS LEFT...CUTS RIGHT"
    If RN = 38 Then U$ = U$ + "LEANS RIGHT...CUTS LEFT"
    If RN = 39 Then U$ = U$ + "DOUBLE REVERSES HIS DRIBBLE"
    If RN = 40 Then U$ = U$ + "BACKS IN"
    If RN = 41 Then U$ = U$ + "LEANS FORWARD"
    If RN = 42 Then U$ = U$ + "CROSSES OVER...SPINS"
    If RN = 43 Then U$ = U$ + "CUTS LEFT"
    If RN = 44 Then U$ = U$ + "CUTS RIGHT"
    If RN = 45 Then U$ = U$ + "WITH THE HESITATION DRIBBLE": Call DELAY: U$ = "HE SPINS LEFT"
    If RN = 46 Then U$ = U$ + "WITH THE HESITATION DRIBBLE": Call DELAY: U$ = "HE SPINS RIGHT"
    If RN = 47 Then U$ = U$ + "WITH THE HESITATION DRIBBLE": Call DELAY: U$ = "HE CUTS LEFT"
    If RN = 48 Then U$ = U$ + "WITH THE HESITATION DRIBBLE": Call DELAY: U$ = "HE CUTS RIGHT"
    If RN = 49 Then U$ = U$ + "STUTTER STEPS": Call DELAY: U$ = "HE MOVES LEFT"
    If RN = 50 Then U$ = U$ + "MAINTAINS HIS DRIBBLE"
    If RN = 51 Then U$ = U$ + "FAKE SPINS LEFT...GOES RIGHT"
    If RN = 52 Then U$ = U$ + "FAKE SPINS RIGHT...GOES LEFT"
    If RN = 53 Then U$ = U$ + "PALMS THE BALL": Call DELAY: U$ = "HE TAKES IT AND SPINS"
    If RN = 54 Then U$ = U$ + "SHAKES AND BAKES"
    If RN = 55 Then U$ = U$ + "DRIBBLES WITH THE LEFT HAND"
    If RN = 56 Then U$ = U$ + "DRIBBLES WITH THE RIGHT HAND"
    GoTo 1495

    1490 'PASS PB$=PREV HANDLER
    V$ = AA$(P, B1(P, B), 0)
    RN = Int(Rnd(1) * 60) + 1
    If RN = 1 Then U$ = PB$ + " TO " + V$
    If RN = 2 Then U$ = PB$ + " KICKS IT OUT": Call DELAY: U$ = V$ + " TAKES THE PASS"
    If RN = 3 Then U$ = PB$ + " FEEDS " + V$
    If RN = 4 Then U$ = PB$ + " PASSES TO " + V$
    If RN = 4 Then U$ = PB$ + " DISHES TO " + V$
    If RN = 5 Then U$ = PB$ + " GIVES IT TO " + V$
    If RN = 6 Then U$ = PB$ + " WITH A NICE GIVE TO " + V$
    If RN = 7 Then U$ = PB$ + " LOOKS": Call DELAY: U$ = "HE GETS IT TO " + V$
    If RN = 8 Then U$ = PB$ + " SENDS IT TO " + V$
    If RN = 9 Then U$ = PB$ + " TO " + V$
    If RN = 10 Then U$ = PB$ + " GETS IT TO " + V$
    If RN = 11 Then U$ = PB$ + " OVER TO " + V$
    If RN = 12 Then U$ = PB$ + " ZIPS IT TO " + V$
    If RN = 13 Then U$ = PB$ + " FINDS " + V$
    If RN = 14 Then U$ = PB$ + " LOOKS FOR " + V$
    If RN = 14 Then U$ = PB$ + " THROWS IT TO " + V$
    If RN = 15 Then U$ = PB$ + " GOES TO " + V$
    If RN = 16 Then U$ = PB$ + " SETS UP " + V$
    If RN = 17 Then U$ = PB$ + " OUT TO " + V$
    If RN = 18 Then U$ = PB$ + " FLIPS IT TO " + V$
    If RN = 19 Then U$ = "BOUNCE PASS TO " + V$
    If RN = 20 Then U$ = PB$ + " LEAVES IT FOR " + V$
    If RN = 21 Then U$ = PB$ + " LOOPS IT TO " + V$
    If RN = 22 Then U$ = PB$ + " SHOVELS IT OVER TO " + V$
    If RN = 23 Then U$ = PB$ + " GIVES IT BACK TO " + V$
    If RN = 24 Then U$ = PB$ + " FEEDS IT TO " + V$
    If RN = 25 Then U$ = PB$ + " GIVES IT OFF TO " + V$
    If RN = 26 Then U$ = "PASSES TO " + V$
    If RN = 27 Then U$ = "DISHES TO " + V$
    If RN = 28 Then U$ = "GIVES TO " + V$
    If RN = 29 Then U$ = "OVER TO " + V$
    If RN = 30 Then U$ = "SENDS IT BACK TO " + V$
    If RN = 31 Then U$ = "NO-LOOK PASS TO " + V$
    If RN = 32 Then U$ = "NO-LOOK BOUNCE PASS TO " + V$
    If RN = 33 Then U$ = "BEHIND THE BACK TO " + V$
    If RN = 34 Then U$ = PB$ + " SWINGS IT TO " + V$
    If RN = 35 Then U$ = PB$ + " FIRES A PASS TO " + V$
    If RN = 36 Then U$ = PB$ + " SNAPS A PASS TO " + V$
    If RN = 37 Then U$ = PB$ + " SWINGS A PASS TO " + V$
    If RN = 38 Then U$ = "TWO-HANDED PASS TO " + V$
    If RN = 39 Then U$ = "WRAP AROUND PASS TO " + V$
    If RN = 40 Then U$ = "OVER THE SHOULDER PASS TO " + V$
    If RN = 41 Then U$ = "BACKHAND PASS TO " + V$
    If RN = 42 Then U$ = "BACKHAND DISH TO " + V$
    If RN = 43 Then U$ = "PASS CONNECTED TO " + V$
    If RN = 44 Then U$ = PB$ + " WIRES IT TO " + V$
    If RN = 45 Then U$ = PB$ + " WIRES A PASS TO " + V$
    If RN = 46 Then U$ = PB$ + " SNAPS A PASS TO " + V$
    If RN = 47 Then U$ = PB$ + " FAKES..PASSES TO " + V$
    If RN = 48 Then U$ = PB$ + " SHUFFLES IT TO " + V$
    If RN = 49 Then U$ = PB$ + " SLINGS IT TO " + V$
    If RN = 50 Then U$ = PB$ + " FLIPS IT TO " + V$
    If RN = 51 Then U$ = PB$ + " WHIPS IT OVER TO " + V$
    If RN = 52 Then U$ = PB$ + " SLINGS A BOUNCE PASS TO " + V$
    If RN = 53 Then U$ = PB$ + " SHOVELS IT TO " + V$
    If RN = 54 Then U$ = PB$ + " PUMP FAKES A SHOT": Call DELAY: U$ = "HE SWINGS IT TO " + V$
    If RN = 55 Then U$ = PB$ + " PUMP FAKES A SHOT": Call DELAY: U$ = "HE BOUNCES IT TO " + V$
    If RN = 56 Then U$ = PB$ + " WITH A FOOT FAKE": Call DELAY: U$ = "HE SPINS AND DUMPS IT TO " + V$
    If RN = 57 Then U$ = PB$ + " CUTS ACROSS THE LANE": Call DELAY: U$ = "HE DISHES INSIDE TO " + V$
    If RN = 58 Then U$ = PB$ + " DRIVES THE BASELINE": Call DELAY: U$ = "DISHES UNDERNEATH TO " + V$
    If RN = 59 Then U$ = PB$ + " FAKES RIGHT...CUTS LEFT": Call DELAY: U$ = "SHUFFLES IT TO " + V$
    If RN = 60 Then U$ = PB$ + " HEAD FAKES": Call DELAY: U$ = "HE PUSHES IT TO " + V$

1495 End Sub


'------------------------------
'      MISSFT Subroutine
'------------------------------
' ...explanation...
Sub MISSFT ()
    If displayPause = 0 Then 3699
    Call MOREDELAY
    Locate F3 + 9, 1
    Print F3; " MISSED"
    Call MOREDELAY

    3699 Rem
End Sub


'------------------------------
'      MOREDElAY Subroutine
'------------------------------
' ...explanation...
Sub MOREDELAY ()
    'Do we really need the IF statement?
    'If displayPause > 0 Then
    _Delay displayPause
    'End If
End Sub


'------------------------------
'      ROSTER Subroutine
'------------------------------
' ...explanation...
Sub ROSTER ()
    Cls
    Color T(P9, 33), T(P9, 34)
    Print B$(P9, 0);
    Color 15, 0
    Print " TEAM ROSTER   COACH: "; B$(P9, 3); "     ";
    Print "  RECORD: ";: Print Using "##"; T(P9, 1) + T(P9, 3);: Print "-";: Print Using "##"; T(P9, 2) + T(P9, 4)
    Color 14: Print "# NAME         PS GP MG FG 3FG %3A FT  ORB  DRB  A  S  B CX CN PT F  FC  PPG"
    Locate 3, 1
    For R1 = 0 To 14
        If B%(P9, R1, 18) <> 0 Or AA$(P9, R1, 0) = "XXX" Then 71
        Color 2
        If R1 <= 9 Then Print Using "# "; R1;
        If R1 > 9 Then Print X$(R1 - 10);
        Color 15
        Print Left$(AA$(P9, R1, 0), 12);: Locate , 16
        If Len(AA$(P9, R1, 1)) = 1 Then Print AA$(P9, R1, 1);
        If Len(AA$(P9, R1, 1)) > 1 Then Print Left$(AA$(P9, R1, 1), 1); Right$(AA$(P9, R1, 1), 1);
        L = 0: Locate , 19: L = A(P9, R1, 7) + A(P9, R1, 11) + A(P9, R1, 12)
        Print Using "## ##"; B%(P9, R1, 0); B%(P9, R1, 1) / B%(P9, R1, 0);
        Print Using "### ### ## ### ##.# ##.# ## ## ##"; A(P9, R1, 0); A(P9, R1, 20); A(P9, R1, 19); A(P9, R1, 1); A(P9, R1, 21) / 10; A(P9, R1, 2) / 10; A(P9, R1, 4) / 10; A(P9, R1, 5); A(P9, R1, 6);
        Print Using "### "; A(P9, R1, 3) - L;
        J = (A(P9, R1, 3)) - A(P9, R1, 7) - A(P9, R1, 11) - A(P9, R1, 12)
        If H <= 3 And J / A(P9, R1, 3) < .25 Then Locate , 61: Print " T";
        If J / A(P9, R1, 3) < .5 And H <= 2 Then Locate , 61: Print " T";
        If H = 4 And T > 200 And J / A(P9, R1, 3) <= T / 2880 Then Locate , 61: Print " T";
        If A(P9, R1, 3) - A(P9, R1, 7) - A(P9, R1, 11) - A(P9, R1, 12) <= 0 Then Locate , 61: Print "EX";
        If H > 4 Or H = 4 And T <= 120 And T <> 0 Then Locate , 61: Print "  ";
        Locate , 63: Print Using "### # ### ##.#"; A(P9, R1, 8) * 2 + A(P9, R1, 10) + W%(P9, R1, 0); A(P9, R1, 12); A(P9, R1, 17); (B%(P9, R1, 3) * 2 + B%(P9, R1, 5) + B%(P9, R1, 14)) / B%(P9, R1, 0)

    71 Next R1
    Locate 18, 1: Color 9, 0: Print String$(27, 205); Chr$(203); String$(51, 205)
    For R1 = 1 To 7: Locate R1 + 18, 28: Print Chr$(186);: Next
    Locate 19, 1: Color 14, 0: Print "STARTING LINEUP"

End Sub


'------------------------------
'    SELECTPBP Subroutine
'------------------------------
' ...explanation...
Sub SELECTPBP ()
    If PB% = 0 Then Call DELAY
    If PB% = 1 And displayPause <> 0 Then Call WINDEX: Print U$: Call MOREDELAY
    Call MOREDELAY
End Sub


'------------------------------
'      TEAMMENU Subroutine
'------------------------------
' ...explanation...
Sub TEAMMENU (BO%, YN$)
    Dim Y2 As Long
    If BO% = 2 Then BO% = 0: GoTo 1102
    NT = 0
    Open P$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031
    Y2 = LOF(1)

    Field #1, 15 As Q$(0), 15 As Q$(1)
    Field #1, 44 As X$, 2 As Q$(9)

    For I1 = 1 To Y2 / 1031
        Get #1, I1
        CK% = CVI(Q$(9))
        If CK% <> 999 Then 1103
        NT = NT + 1
        V$(NT) = Q$(0): V%(NT) = I1

    1103 Next
    Close 1

    1102 SELECTION% = 1
    COUNT% = NT
    START% = 1
    FileFGColr% = 15: FileBGColr% = 4
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    ROWS% = 20
    COLUMNS% = 4
    SPACING% = 6
    ROW% = 4
    COLUMN% = 1
    Color , 4: Cls: Locate , 33: Color 14
    Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    20 MAMenu V$(), SELECTION%, START%, COUNT%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, ROWS%, COLUMNS%, SPACING%, ROW%, COLUMN%
    _MouseHide

    If EXITCODE% = 27 Then BO% = 1: GoTo 25
    If EXITCODE% = -71 Then START% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
    If EXITCODE% = -79 Then START% = NT: EXITCODE% = 3: SELECTION% = NT: GoTo 20
    If EXITCODE% = -73 Then START% = START% - 80: If START% < 1 Then START% = 1
    If EXITCODE% = -81 Then START% = START% + 80: If START% > NT Then START% = NT
    If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = START%: GoTo 20
    I1 = V%(SELECTION%)
    Call GETATEAM(I1, YN$)

25 End Sub


'------------------------------
'      THREEFT Subroutine
'------------------------------
' ...explanation...
Sub THREEFT ()
    A(P, B1(P, B), 10) = A(P, B1(P, B), 10) + 1
    RN = Int(Rnd(1) * 100) + 1
    If F3 = 1 And RN <= 50 Then U$ = "FREE THROW IS GOOD"
    If F3 = 1 And RN > 50 Then U$ = "FREE THROW IS GOOD"
    RN = Int(Rnd(1) * 100) + 1
    If F3 = 2 And RN <= 50 Then U$ = "SECOND FREE THROW IS GOOD"
    If F3 = 2 And RN > 50 Then U$ = "HE HITS THE SECOND ONE"
    RN = Int(Rnd(1) * 100) + 1
    If F3 = 3 And RN <= 50 Then U$ = "THIRD FREE THROW IS GOOD"
    If F3 = 3 And RN > 50 Then U$ = "HE HITS THE LAST ONE"
    Call DELAY
    S(P, H) = S(P, H) + 1
    S(P, 0) = S(P, 0) + 1
    Call UPDATESCREEN
End Sub


'------------------------------
'      TICKER Subroutine
'------------------------------
' ...explanation...
Sub TICKER ()
    '*** TICKER ***
    If AP = 1 And U5 = 2 Then 2884
    Color 14, 0: Cls
    RN = Int(Rnd(1) * 100) + 1
    If RN <= 50 And HT% = 1 And G9 = 0 Then Locate , 23: Print "PRUDENTIAL AT THE HALF SCOREBOARD": GoTo 2851
    If RN > 50 And HT% = 1 And G9 = 0 Then Locate , 28: Print "RED MAN HALFTIME REPORT": GoTo 2851
    If HT% = 1 And RN <= 50 Then Locate , 29: Print "TOYOTA HALFTIME REPORT": GoTo 2851
    If HT% = 1 And RN > 50 Then Locate , 29: Print "HONDA HALFTIME REPORT": GoTo 2851
    If EG% = 1 And G9 = 0 Then Locate , 27: Print "CHEVROLET FINAL SCOREBOARD": GoTo 2851
    If EG% = 1 And RN <= 33 Then Locate , 27: Print "USA TODAY FINAL SCOREBOARD": GoTo 2851
    If EG% = 1 And RN > 33 And RN <= 67 Then Locate , 24: Print "FEDERAL EXPRESS FINAL SCOREBOARD": GoTo 2851
    If EG% = 1 And RN > 67 Then Locate , 23: Print "ESPN SPORTSCENTER FINAL SCOREBOARD": GoTo 2851
    If HT% = 0 Then Locate , 30: Print "LANCE HAFFNER GAMES SCOREBOARD": GoTo 2851

    2851 Color 15: If TMT = 0 Then Locate 10, 28: Print "NO OTHER GAMES SCHEDULED": GoTo 2883
    If H = 1 And T < 720 And T >= 480 Then TE = 1
    If H = 1 And T < 480 And T >= 240 Then TE = 2
    If H = 1 And T < 240 Then TE = 3
    If H = 2 And T < 720 And T >= 480 Then TE = 4
    If H = 2 And T < 480 And T >= 240 Then TE = 5
    If H = 2 And T < 240 Then TE = 6
    If H = 3 And T < 720 And T >= 480 Then TE = 9
    If H = 3 And T < 480 And T >= 240 Then TE = 10
    If H = 3 And T < 240 Then TE = 11
    If H = 4 And T < 720 And T >= 480 Then TE = 12
    If H = 4 And T < 480 And T >= 240 Then TE = 13
    If H = 4 And T < 240 Then TE = 14
    If H = 5 And T < 720 And T >= 480 Then TE = 15
    If H = 5 And T < 480 And T >= 240 Then TE = 16
    If H = 5 And T < 240 Then TE = 17
    If HT% = 1 Then TE = 8
    If Left$(SX$(1, 0), 3) = "XXX" Then 2852
    Locate 2, 1: X = 1: GoSub 2885
    Print Left$(SX$(1, 0), 13);
    Locate , 15: Print Using "### "; SX%(1, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(1, 1), 13);
    Locate , 15: Print Using "### #"; SX%(1, 1, TS%)

    2852 If Left$(SX$(2, 0), 3) = "XXX" Then 2853
    Locate 5, 1: X = 2: GoSub 2885: Print Left$(SX$(2, 0), 13);: Locate , 15: Print Using "### "; SX%(2, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(2, 1), 13);: Locate , 15: Print Using "### #"; SX%(2, 1, TS%)

    2853 If Left$(SX$(3, 0), 3) = "XXX" Then 2854
    Locate 8, 1: X = 3: GoSub 2885: Print Left$(SX$(3, 0), 13);: Locate , 15: Print Using "### "; SX%(3, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(3, 1), 13);: Locate , 15: Print Using "### #"; SX%(3, 1, TS%)

    2854 If Left$(SX$(4, 0), 3) = "XXX" Then 2855
    Locate 11, 1: X = 4: GoSub 2885: Print Left$(SX$(4, 0), 13);: Locate , 15: Print Using "### "; SX%(4, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(4, 1), 13);: Locate , 15: Print Using "### #"; SX%(4, 1, TS%)

    2855 If Left$(SX$(5, 0), 3) = "XXX" Then 2856
    Locate 14, 1: X = 5: GoSub 2885: Print Left$(SX$(5, 0), 13);: Locate , 15: Print Using "### "; SX%(5, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(5, 1), 13);: Locate , 15: Print Using "### #"; SX%(5, 1, TS%)

    2856 If Left$(SX$(6, 0), 3) = "XXX" Then 2857
    Locate 17, 1: X = 6: GoSub 2885: Print Left$(SX$(6, 0), 13);: Locate , 15: Print Using "### "; SX%(6, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(6, 1), 13);: Locate , 15: Print Using "### #"; SX%(6, 1, TS%)

    2857 If Left$(SX$(7, 0), 3) = "XXX" Then 2858
    Locate 20, 1: X = 7: GoSub 2885: Print Left$(SX$(7, 0), 13);: Locate , 15: Print Using "### "; SX%(7, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(7, 1), 13);: Locate , 15: Print Using "### #"; SX%(7, 1, TS%)

    2858 If Left$(SX$(8, 0), 3) = "XXX" Then 2859
    Locate 23, 1: X = 8: GoSub 2885: Print Left$(SX$(8, 0), 13);: Locate , 15: Print Using "### "; SX%(8, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(8, 1), 13);: Locate , 15: Print Using "### #"; SX%(8, 1, TS%);

    2859 If Left$(SX$(9, 0), 3) = "XXX" Then 2860
    Locate 2, 21: X = 9: GoSub 2885: Print Left$(SX$(9, 0), 13);: Locate , 35: Print Using "### "; SX%(9, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(9, 1), 13);: Locate , 35: Print Using "### #"; SX%(9, 1, TS%)

    2860 If Left$(SX$(10, 0), 3) = "XXX" Then 2861
    Locate 5, 21: X = 10: GoSub 2885: Print Left$(SX$(10, 0), 13);: Locate , 35: Print Using "### "; SX%(10, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(10, 1), 13);: Locate , 35: Print Using "### #"; SX%(10, 1, TS%)

    2861 If Left$(SX$(11, 0), 3) = "XXX" Then 2862
    Locate 8, 21: X = 11: GoSub 2885: Print Left$(SX$(11, 0), 13);: Locate , 35: Print Using "### "; SX%(11, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(11, 1), 13);: Locate , 35: Print Using "### #"; SX%(11, 1, TS%)

    2862 If Left$(SX$(12, 0), 3) = "XXX" Then 2863
    Locate 11, 21: X = 12: GoSub 2885: Print Left$(SX$(12, 0), 13);: Locate , 35: Print Using "### "; SX%(12, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(12, 1), 13);: Locate , 35: Print Using "### #"; SX%(12, 1, TS%)

    2863 If Left$(SX$(13, 0), 3) = "XXX" Then 2864
    Locate 14, 21: X = 13: GoSub 2885: Print Left$(SX$(13, 0), 13);: Locate , 35: Print Using "### "; SX%(13, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(13, 1), 13);: Locate , 35: Print Using "### #"; SX%(13, 1, TS%)

    2864 If Left$(SX$(14, 0), 3) = "XXX" Then 2865
    Locate 17, 21: X = 14: GoSub 2885: Print Left$(SX$(14, 0), 13);: Locate , 35: Print Using "### "; SX%(14, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(14, 1), 13);: Locate , 35: Print Using "### #"; SX%(14, 1, TS%)

    2865 If Left$(SX$(15, 0), 3) = "XXX" Then 2866
    Locate 20, 21: X = 15: GoSub 2885: Print Left$(SX$(15, 0), 13);: Locate , 35: Print Using "### "; SX%(15, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(15, 1), 13);: Locate , 35: Print Using "### #"; SX%(15, 1, TS%)

    2866 If Left$(SX$(16, 0), 3) = "XXX" Then 2867
    Locate 23, 21: X = 16: GoSub 2885: Print Left$(SX$(16, 0), 13);: Locate , 35: Print Using "### "; SX%(16, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(16, 1), 13);: Locate , 35: Print Using "### #"; SX%(16, 1, TS%);

    2867 If Left$(SX$(17, 0), 3) = "XXX" Then 2868
    Locate 2, 41: X = 17: GoSub 2885: Print Left$(SX$(17, 0), 13);: Locate , 55: Print Using "### "; SX%(17, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(17, 1), 13);: Locate , 55: Print Using "### #"; SX%(17, 1, TS%)

    2868 If Left$(SX$(18, 0), 3) = "XXX" Then 2869
    Locate 5, 41: X = 18: GoSub 2885: Print Left$(SX$(18, 0), 13);: Locate , 55: Print Using "### "; SX%(18, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(18, 1), 13);: Locate , 55: Print Using "### #"; SX%(18, 1, TS%)

    2869 If Left$(SX$(19, 0), 3) = "XXX" Then 2870
    Locate 8, 41: X = 19: GoSub 2885: Print Left$(SX$(19, 0), 13);: Locate , 55: Print Using "### "; SX%(19, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(19, 1), 13);: Locate , 55: Print Using "### #"; SX%(19, 1, TS%)

    2870 If Left$(SX$(20, 0), 3) = "XXX" Then 2871
    Locate 11, 41: X = 20: GoSub 2885: Print Left$(SX$(20, 0), 13);: Locate , 55: Print Using "### "; SX%(20, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(20, 1), 13);: Locate , 55: Print Using "### #"; SX%(20, 1, TS%)

    2871 If Left$(SX$(21, 0), 3) = "XXX" Then 2872
    Locate 14, 41: X = 21: GoSub 2885: Print Left$(SX$(21, 0), 13);: Locate , 55: Print Using "### "; SX%(21, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(21, 1), 13);: Locate , 55: Print Using "### #"; SX%(21, 1, TS%)

    2872 If Left$(SX$(22, 0), 3) = "XXX" Then 2873
    Locate 17, 41: X = 22: GoSub 2885: Print Left$(SX$(22, 0), 13);: Locate , 55: Print Using "### "; SX%(22, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(22, 1), 13);: Locate , 55: Print Using "### #"; SX%(22, 1, TS%)

    2873 If Left$(SX$(23, 0), 3) = "XXX" Then 2874
    Locate 20, 41: X = 23: GoSub 2885: Print Left$(SX$(23, 0), 13);: Locate , 55: Print Using "### "; SX%(23, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(23, 1), 13);: Locate , 55: Print Using "### #"; SX%(23, 1, TS%)

    2874 If Left$(SX$(24, 0), 3) = "XXX" Then 2875
    Locate 23, 41: X = 24: GoSub 2885: Print Left$(SX$(24, 0), 13);: Locate , 55: Print Using "### "; SX%(24, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(24, 1), 13);: Locate , 55: Print Using "### #"; SX%(24, 1, TS%);

    2875 If Left$(SX$(25, 0), 3) = "XXX" Then 2876
    Locate 2, 61: X = 25: GoSub 2885: Print Left$(SX$(25, 0), 13);: Locate , 75: Print Using "### "; SX%(25, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(25, 1), 13);: Locate , 75: Print Using "### #"; SX%(25, 1, TS%)

    2876 If Left$(SX$(26, 0), 3) = "XXX" Then 2877
    Locate 5, 61: X = 26: GoSub 2885: Print Left$(SX$(26, 0), 13);: Locate , 75: Print Using "### "; SX%(26, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(26, 1), 13);: Locate , 75: Print Using "### #"; SX%(26, 1, TS%)

    2877 If Left$(SX$(27, 0), 3) = "XXX" Then 2878
    Locate 8, 61: X = 27: GoSub 2885: Print Left$(SX$(27, 0), 13);: Locate , 75: Print Using "### "; SX%(27, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(27, 1), 13);: Locate , 75: Print Using "### #"; SX%(27, 1, TS%)

    2878 If Left$(SX$(28, 0), 3) = "XXX" Then 2879
    Locate 11, 61: X = 28: GoSub 2885: Print Left$(SX$(28, 0), 13);: Locate , 75: Print Using "### "; SX%(28, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(28, 1), 13);: Locate , 75: Print Using "### #"; SX%(28, 1, TS%)

    2879 If Left$(SX$(29, 0), 3) = "XXX" Then 2880
    Locate 14, 61: X = 29: GoSub 2885: Print Left$(SX$(29, 0), 13);: Locate , 75: Print Using "### "; SX%(29, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(29, 1), 13);: Locate , 75: Print Using "### #"; SX%(29, 1, TS%)

    2880 If Left$(SX$(30, 0), 3) = "XXX" Then 2881
    Locate 17, 61: X = 30: GoSub 2885: Print Left$(SX$(30, 0), 13);: Locate , 75: Print Using "### "; SX%(30, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(30, 1), 13);: Locate , 75: Print Using "### #"; SX%(30, 1, TS%)

    2881 If Left$(SX$(31, 0), 3) = "XXX" Then 2882
    Locate 20, 61: X = 31: GoSub 2885: Print Left$(SX$(31, 0), 13);: Locate , 75: Print Using "### "; SX%(31, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(31, 1), 13);: Locate , 75: Print Using "### #"; SX%(31, 1, TS%)

    2882 If Left$(SX$(32, 0), 3) = "XXX" Then 2883
    Locate 23, 61: X = 32: GoSub 2885: Print Left$(SX$(32, 0), 13);: Locate , 75: Print Using "### "; SX%(32, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(32, 1), 13);: Locate , 75: Print Using "### #"; SX%(32, 1, TS%);
    2883 I$ = GetKeyPress$
    Call NEWSCREEN
    GoTo 2884

    2885 TS% = TE + (TTS% - ST%(X)) * 3
    If TS% < 0 Then TS% = 0
    If TS% > 14 Then TS% = 14
    If EG% = 1 Then TS% = 14
    Return

2884 End Sub


'------------------------------
'   UPDATESCREEN Subroutine
'------------------------------
' ...explanation...
Sub UPDATESCREEN ()
    'M A I N   S C R E E N
    Call TICKERUPDATE
    Locate 1, 37: Color 15, 0: Print Using "##"; Int(T / 60);: Print ":";
    If T - Int(T / 60) * 60 < 10 Then Print "0";: Print Using "#"; T - Int(T / 60) * 60;: GoTo 2171
    Print Using "##"; T - Int(T / 60) * 60;

    2171 Locate 4, 13: Color 14, 0: Print Using "###"; S(0, 0);
    Locate 2, 35: Print Using " PERIOD #"; H;
    If H > 4 Then Locate , 35: Print Using "OVERTIME #"; H - 4
    Locate 4, 60: Color 14, 0: Print Using "###"; S(1, 0)
    Color 7: Locate 3, 35: Print Using "##  "; T0(0);: Color 12: Print "TO  ";: Color 7: Print Using "##"; T0(1)
    Locate , 35: Print Using "##  "; R3(0);: Color 12: Print "RB  ";: Color 7: Print Using "##"; R3(1)
    Locate , 35: Print Using "##  "; P2(0);: Color 12: Print "TF  ";: Color 7: Print Using "##"; P2(1);
    If P2(0) > NB% - 1 And H <= 4 Or P2(0) > 2 And H > 4 Then Locate , 46: Color 0, 2: Print "BONUS";: Color 7, 0
    If P2(1) > NB% - 1 And H <= 4 Or P2(1) > 2 And H > 4 Then Locate , 29: Color 0, 2: Print "BONUS": Color 7, 0
    Locate 6, 9: Print "TIMEOUTS:"; T1(0);: Locate , 56: Print "TIMEOUTS:"; T1(1)
    Locate 6, 37: Color 15, 0: Print Using " :##"; X8
    If T < 25 And X8 >= T Or H > 4 And T = 300 Then Locate 6, 37: Print " :  "
    If displayPause = 0 Then 2250
    Locate 8, 58
    Color T(D, 33), T(D, 34): Print "DEF - "; D$(D2(D))
    Locate , 58: Color T(P, 33), T(P, 34): Print "OFF - "; O$(O0%(P))
    Color 15, 0
    Locate , 41: Color 14: Print "(1) ";: Color 15: Print "SHOOT        ";: Color 14: Print "ENTER";: Color 15: Print "-PASS"
    Locate , 41: Color 14: Print "(2) ";: Color 15: Print B$(P, 0); " CALL TIMEOUT     "
    Locate , 41: Color 14: Print "(3) ";: Color 15: Print "COMMIT INTL FOUL (LAST 4 MIN)"
    Locate , 41: Color 14: Print "(4) ";: Color 15: Print "SHOOT A FORCED 3-POINTER"
    Locate , 41: Color 14: Print "(S) ";: Color 15: Print "SCOUT REPORT ";: Color 14: Print "(T) ";: Color 15: Print "10 MIN TICKER"
    Locate , 41: Color 14: Print "(Z) ";: Color 15: Print "LEFT TM STGY ";: Color 14: Print "(/) ";: Color 15: Print "RIGHT TM STGY"
    Color 15, 0
    Locate 19, 1
    For I1 = 0 To 4
        Color 2, 0
        Print Using "# "; I1;
        Color 15
        Print Left$(AA$(0, B1(0, I1), 0), 12); Tab(16);
        J = (A(0, B1(0, I1), 3)) - A(0, B1(0, I1), 7) - A(0, B1(0, I1), 11) - A(0, B1(0, I1), 12)
        If J <= 0 Then J = 0
        If A(0, B1(0, I1), 1) >= 100 Then A(0, B1(0, I1), 1) = 99
        Print Using "## ## "; A(0, B1(0, I1), 0); A(0, B1(0, I1), 1);
        If P = 0 Then
            Print Using "##"; A(0, B1(0, I1), 21) * 2 / 10;
        Else
            Print Using "##"; A(0, B1(0, I1), 2) * 2 / 10;
        End If
        If J <= 0 And (H = 1 Or H = 2 Or H = 3 Or H = 4 And T > 120) Then Color 12
        Print Using "### "; J;
        Color 15
        Print Using "## ##"; A(0, B1(0, I1), 4) / 10; A(0, B1(0, I1), 5) / 10;
        Print Using "###  "; A(0, B1(0, I1), 8) * 2 + A(0, B1(0, I1), 10) + W%(0, B1(0, I1), 0);
        If A(0, B1(0, I1), 12) >= 5 Then Color 12
        Print Using "#"; A(0, B1(0, I1), 12)
        Color 15
    Next I1
    Locate 19, 40
    For I1 = 0 To 4
        Locate , 40
        Color 2, 0
        Print Using "# "; I1;
        Color 15: Print Left$(AA$(1, B1(1, I1), 0), 12); Tab(56);
        J = (A(1, B1(1, I1), 3)) - A(1, B1(1, I1), 7) - A(1, B1(1, I1), 11) - A(1, B1(1, I1), 12)
        If J <= 0 Then J = 0
        If A(1, B1(1, I1), 1) >= 100 Then A(1, B1(1, I1), 1) = 99
        Print Using "## ## "; A(1, B1(1, I1), 0); A(1, B1(1, I1), 1);
        If P = 1 Then Print Using "##"; A(1, B1(1, I1), 21) * 2 / 10; Else Print Using "##"; A(1, B1(1, I1), 2) * 2 / 10;
        If J <= 0 And (H = 1 Or H = 2 Or H = 3 Or H = 4 And T > 120) Then Color 12
        Print Using "### "; J;
        Color 15
        Print Using "## ##"; A(1, B1(1, I1), 4) / 10; A(1, B1(1, I1), 5) / 10;
        Print Using "###  "; A(1, B1(1, I1), 8) * 2 + A(1, B1(1, I1), 10) + W%(1, B1(1, I1), 0);
        If A(1, B1(1, I1), 12) >= 5 Then Color 12
        Print Using "#"; A(1, B1(1, I1), 12)
        Color 15
    Next I1

2250 End Sub


'------------------------------
'      WINDEX Subroutine
'------------------------------
' ...explanation...
Sub WINDEX ()
    'If displayPause > 0 Then 4094
    For I = 8 To 15
        Locate I, 1
        Print Space$(38);
    Next I
    'End If
    Locate 8, 1

End Sub


'------------------------------
'      RECORDSCHECK Subroutine
'------------------------------
' ...explanation...
Sub RECORDSCHECK ()
    'Dim RC$(50, 4), REC(50, 2) As Single
    Dim TRC$(125, 3), TREC(125, 2) As Single
    Dim V8(1, 14)
    Dim SITE$(1)
    SITE$(0) = "AT": SITE$(1) = "VS."
    If Not _FileExists(P$(1) + "RECORDS.GAM") Then
        TREC(2, 0) = 999
        TREC(4, 0) = 999
        TREC(7, 0) = 999
        TREC(9, 0) = 999
        TREC(11, 0) = 999
        TREC(13, 0) = 999
        TREC(15, 0) = 999
        TREC(17, 0) = 999
        TREC(19, 0) = 999
        TREC(21, 0) = 999
        TREC(23, 0) = 999
        TREC(25, 0) = 999
        TREC(27, 0) = 999
        TREC(29, 0) = 999
        TREC(31, 0) = 999
        TREC(33, 0) = 999
        TREC(35, 0) = 999
        TREC(37, 0) = 999
        TREC(43, 0) = 999
        TREC(45, 0) = 999
        TREC(61, 0) = 999
        TREC(63, 0) = 999
        TREC(65, 0) = 999
        TREC(67, 0) = 999
        TREC(73, 0) = 999
        TREC(75, 0) = 999
        TREC(81, 0) = 999
        TREC(83, 0) = 999
        TREC(87, 0) = 999
        TREC(89, 0) = 999
        TREC(92, 0) = 999
        TREC(94, 0) = 999
        TREC(97, 0) = 999
        TREC(99, 0) = 999
        TREC(105, 0) = 999
        TREC(107, 0) = 999
        TREC(113, 0) = 999
        TREC(115, 0) = 999
        TREC(120, 0) = 999
        TREC(123, 0) = 999
        TREC(125, 0) = 999
        GoTo 505
    End If
    Open P$(1) + "RECORDS.GAM" For Input As #1
    For I = 0 To 48
        For I1 = 0 To 2
            Input #1, REC(I, I1)
        Next
        For I1 = 0 To 4
            Input #1, RC$(I, I1)
        Next
    Next
    For I = 0 To 125
        For I1 = 0 To 2
            Input #1, TREC(I, I1)
        Next
        For I1 = 0 To 3
            Input #1, TRC$(I, I1)
        Next
    Next
    Close 1

    505 'CHECK THE INDIVIDUAL RECORDS
    For I = 0 To 1
        For I1 = 0 To 14

            ' SCORING RECORDS FOR POINTS IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) > REC(0, 0) Then
                REC(0, 0) = A(I, I1, 8) * 2 + A(I, I1, 10) + W%(I, I1, 0) ' POINTS
                REC(0, 1) = 1 ' TIMES DONE
                REC(0, 2) = 0 ' GAME
                RC$(0, 0) = AA$(I, I1, 0) ' NAME
                RC$(0, 1) = B$(I, 0) ' TEAM
                RC$(0, 2) = SITE$(I) ' AT/VS
                RC$(0, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(0, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 13) + QQ(I, 2, I1, 13) > REC(1, 0) Then
                REC(1, 0) = QQ(I, 1, I1, 13) + QQ(I, 2, I1, 13) ' POINTS
                REC(1, 1) = 1 ' TIMES DONE
                REC(1, 2) = 1 ' HALF
                RC$(1, 0) = AA$(I, I1, 0) ' NAME
                RC$(1, 1) = B$(I, 0) ' TEAM
                RC$(1, 2) = SITE$(I) ' AT/VS
                RC$(1, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(1, 4) = Date$ ' DATE
            End If
            If QQ(I, 3, I1, 13) + QQ(I, 4, I1, 13) > REC(1, 0) Then
                REC(1, 0) = QQ(I, 3, I1, 13) + QQ(I, 4, I1, 13) ' POINTS
                REC(1, 1) = 1 ' TIMES DONE
                REC(1, 2) = 2 ' HALF
                RC$(1, 0) = AA$(I, I1, 0) ' NAME
                RC$(1, 1) = B$(I, 0) ' TEAM
                RC$(1, 2) = SITE$(I) ' AT/VS
                RC$(1, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(1, 4) = Date$ ' DATE
            End If
            For I2 = 1 To 4
                If QQ(I, I2, I1, 13) > REC(2, 0) Then
                    REC(2, 0) = QQ(I, I2, I1, 13) ' POINTS
                    REC(2, 1) = 1 ' TIMES DONE
                    REC(2, 2) = I2 ' QTR
                    RC$(2, 0) = AA$(I, I1, 0) ' NAME
                    RC$(2, 1) = B$(I, 0) ' TEAM
                    RC$(2, 2) = SITE$(I) ' AT/VS
                    RC$(2, 3) = B$(1 - I, 0) ' OPPONENT
                    RC$(2, 4) = Date$ ' DATE
                End If
            Next I2
            For I2 = 5 To 8
                If QQ(I, I2, I1, 13) > REC(3, 0) Then
                    REC(3, 0) = QQ(I, I2, I1, 13) ' POINTS
                    REC(3, 1) = 1 ' TIMES DONE
                    REC(3, 2) = I2 ' QTR
                    RC$(3, 0) = AA$(I, I1, 0) ' NAME
                    RC$(3, 1) = B$(I, 0) ' TEAM
                    RC$(3, 2) = SITE$(I) ' AT/VS
                    RC$(3, 3) = B$(1 - I, 0) ' OPPONENT
                    RC$(3, 4) = Date$ ' DATE
                End If
            Next I2

            ' FIELD GOAL PERCENTAGE RECORDS FOR HIGH %, MOST W/NO MISSES, MOST W/NONE MADE
            If A(I, I1, 8) < 15 Then 900
            If A(I, I1, 8) / A(I, I1, 7) > REC(4, 0) Then
                REC(4, 0) = A(I, I1, 8) / A(I, I1, 7) ' PCTG
                REC(4, 1) = A(I, I1, 8) ' FGM
                REC(4, 2) = A(I, I1, 7) ' FGA
                RC$(4, 0) = AA$(I, I1, 0) ' NAME
                RC$(4, 1) = B$(I, 0) ' TEAM
                RC$(4, 2) = SITE$(I) ' AT/VS
                RC$(4, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(4, 4) = Date$ ' DATE
            End If

            900
            If A(I, I1, 8) = A(I, I1, 7) And A(I, I1, 7) > REC(5, 0) Then
                REC(5, 0) = A(I, I1, 7) ' MOST FGM
                RC$(5, 0) = AA$(I, I1, 0) ' NAME
                RC$(5, 1) = B$(I, 0) ' TEAM
                RC$(5, 2) = SITE$(I) ' AT/VS
                RC$(5, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(5, 4) = Date$ ' DATE
            End If
            If A(I, I1, 8) = 0 And A(I, I1, 7) > REC(6, 0) Then
                REC(6, 0) = A(I, I1, 7) ' 0 FGM /FGA
                RC$(6, 0) = AA$(I, I1, 0) ' NAME
                RC$(6, 1) = B$(I, 0) ' TEAM
                RC$(6, 2) = SITE$(I) ' AT/VS
                RC$(6, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(6, 4) = Date$ ' DATE
            End If

            ' FIELD GOALS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If A(I, I1, 8) > REC(7, 0) Then
                REC(7, 0) = A(I, I1, 8) ' FGM
                REC(7, 1) = 1 ' TIMES DONE
                REC(7, 2) = 0 ' GAME
                RC$(7, 0) = AA$(I, I1, 0) ' NAME
                RC$(7, 1) = B$(I, 0) ' TEAM
                RC$(7, 2) = SITE$(I) ' AT/VS
                RC$(7, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(7, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 2) + QQ(I, 2, I1, 2) > REC(8, 0) Then
                REC(8, 0) = QQ(I, 1, I1, 2) + QQ(I, 2, I1, 2) ' FGM
                REC(8, 1) = 1 ' TIMES DONE
                REC(8, 2) = 1 ' HALF
                RC$(8, 0) = AA$(I, I1, 0) ' NAME
                RC$(8, 1) = B$(I, 0) ' TEAM
                RC$(8, 2) = SITE$(I) ' AT/VS
                RC$(8, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(8, 4) = Date$ ' DATE
            End If
            If QQ(I, 3, I1, 2) + QQ(I, 4, I1, 2) > REC(8, 0) Then
                REC(8, 0) = QQ(I, 3, I1, 2) + QQ(I, 4, I1, 2) ' FGM
                REC(8, 1) = 1 ' TIMES DONE
                REC(8, 2) = 2 ' HALF
                RC$(8, 0) = AA$(I, I1, 0) ' NAME
                RC$(8, 1) = B$(I, 0) ' TEAM
                RC$(8, 2) = SITE$(I) ' AT/VS
                RC$(8, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(8, 4) = Date$ ' DATE
            End If
            For I2 = 1 To 4
                If QQ(I, I2, I1, 2) > REC(9, 0) Then
                    REC(9, 0) = QQ(I, I2, I1, 2) ' FGM
                    REC(9, 1) = 1 ' TIMES DONE
                    REC(9, 2) = I2 ' QTR
                    RC$(9, 0) = AA$(I, I1, 0) ' NAME
                    RC$(9, 1) = B$(I, 0) ' TEAM
                    RC$(9, 2) = SITE$(I) ' AT/VS
                    RC$(9, 3) = B$(1 - I, 0) ' OPPONENT
                    RC$(9, 4) = Date$ ' DATE
                End If
            Next I2
            For I2 = 5 To 8
                If QQ(I, I2, I1, 2) > REC(10, 0) Then
                    REC(10, 0) = QQ(I, I2, I1, 2) ' FGM
                    REC(10, 1) = 1 ' TIMES DONE
                    REC(10, 2) = I2 ' QTR
                    RC$(10, 0) = AA$(I, I1, 0) ' NAME
                    RC$(10, 1) = B$(I, 0) ' TEAM
                    RC$(10, 2) = SITE$(I) ' AT/VS
                    RC$(10, 3) = B$(1 - I, 0) ' OPPONENT
                    RC$(10, 4) = Date$ ' DATE
                End If
            Next I2

            ' FIELD GOAL ATTEMPTS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If A(I, I1, 7) > REC(11, 0) Then
                REC(11, 0) = A(I, I1, 7) ' FGA
                REC(11, 1) = 1 ' TIMES DONE
                REC(11, 2) = 0 ' GAME
                RC$(11, 0) = AA$(I, I1, 0) ' NAME
                RC$(11, 1) = B$(I, 0) ' TEAM
                RC$(11, 2) = SITE$(I) ' AT/VS
                RC$(11, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(11, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 1) + QQ(I, 2, I1, 1) > REC(12, 0) Then
                REC(12, 0) = QQ(I, 1, I1, 1) + QQ(I, 2, I1, 1) ' FGA
                REC(12, 1) = 1 ' TIMES DONE
                REC(12, 2) = 1 ' HALF
                RC$(12, 0) = AA$(I, I1, 0) ' NAME
                RC$(12, 1) = B$(I, 0) ' TEAM
                RC$(12, 2) = SITE$(I) ' AT/VS
                RC$(12, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(12, 4) = Date$ ' DATE
            End If
            If QQ(I, 3, I1, 1) + QQ(I, 4, I1, 1) > REC(12, 0) Then
                REC(12, 0) = QQ(I, 3, I1, 1) + QQ(I, 4, I1, 1) ' FGA
                REC(12, 1) = 1 ' TIMES DONE
                REC(12, 2) = 2 ' HALF
                RC$(12, 0) = AA$(I, I1, 0) ' NAME
                RC$(12, 1) = B$(I, 0) ' TEAM
                RC$(12, 2) = SITE$(I) ' AT/VS
                RC$(12, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(12, 4) = Date$ ' DATE
            End If
            For I2 = 1 To 4
                If QQ(I, I2, I1, 1) > REC(13, 0) Then
                    REC(13, 0) = QQ(I, I2, I1, 1) ' FGA
                    REC(13, 1) = 1 ' TIMES DONE
                    REC(13, 2) = I2 ' QTR
                    RC$(13, 0) = AA$(I, I1, 0) ' NAME
                    RC$(13, 1) = B$(I, 0) ' TEAM
                    RC$(13, 2) = SITE$(I) ' AT/VS
                    RC$(13, 3) = B$(1 - I, 0) ' OPPONENT
                    RC$(13, 4) = Date$ ' DATE
                End If
            Next I2
            For I2 = 5 To 8
                If QQ(I, I2, I1, 1) > REC(14, 0) Then
                    REC(14, 0) = QQ(I, I2, I1, 1) ' FGA
                    REC(14, 1) = 1 ' TIMES DONE
                    REC(14, 2) = I2 ' QTR
                    RC$(14, 0) = AA$(I, I1, 0) ' NAME
                    RC$(14, 1) = B$(I, 0) ' TEAM
                    RC$(14, 2) = SITE$(I) ' AT/VS
                    RC$(14, 3) = B$(1 - I, 0) ' OPPONENT
                    RC$(14, 4) = Date$ ' DATE
                End If
            Next I2

            ' THREE-POINT FIELD GOALS MOST MADE NO MISSES, MOST ATT NONE MADE
            If W%(I, I1, 0) = W%(I, I1, 1) And W%(I, I1, 0) > REC(15, 0) Then
                REC(15, 0) = W%(I, I1, 0) ' MOST 3-FGM
                RC$(15, 0) = AA$(I, I1, 0) ' NAME
                RC$(15, 1) = B$(I, 0) ' TEAM
                RC$(15, 2) = SITE$(I) ' AT/VS
                RC$(15, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(15, 4) = Date$ ' DATE
            End If
            If W%(I, I1, 0) = 0 And W%(I, I1, 1) > REC(16, 0) Then
                REC(16, 0) = W%(I, I1, 1) ' 0 3FGM /3FGA
                RC$(16, 0) = AA$(I, I1, 0) ' NAME
                RC$(16, 1) = B$(I, 0) ' TEAM
                RC$(16, 2) = SITE$(I) ' AT/VS
                RC$(16, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(16, 4) = Date$ ' DATE
            End If

            ' 3-FIELD GOALS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If W%(I, I1, 0) > REC(17, 0) Then
                REC(17, 0) = W%(I, I1, 0) ' 3FGM
                REC(17, 1) = 1 ' TIMES DONE
                REC(17, 2) = 0 ' GAME
                RC$(17, 0) = AA$(I, I1, 0) ' NAME
                RC$(17, 1) = B$(I, 0) ' TEAM
                RC$(17, 2) = SITE$(I) ' AT/VS
                RC$(17, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(17, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 10) + QQ(I, 2, I1, 10) > REC(18, 0) Then
                REC(18, 0) = QQ(I, 1, I1, 10) + QQ(I, 2, I1, 10) ' 3FGM
                REC(18, 1) = 1 ' TIMES DONE
                REC(18, 2) = 1 ' HALF
                RC$(18, 0) = AA$(I, I1, 0) ' NAME
                RC$(18, 1) = B$(I, 0) ' TEAM
                RC$(18, 2) = SITE$(I) ' AT/VS
                RC$(18, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(18, 4) = Date$ ' DATE
            End If
            If QQ(I, 3, I1, 10) + QQ(I, 4, I1, 10) > REC(18, 0) Then
                REC(18, 0) = QQ(I, 3, I1, 10) + QQ(I, 4, I1, 10) ' 3FGM
                REC(18, 1) = 1 ' TIMES DONE
                REC(18, 2) = 2 ' HALF
                RC$(18, 0) = AA$(I, I1, 0) ' NAME
                RC$(18, 1) = B$(I, 0) ' TEAM
                RC$(18, 2) = SITE$(I) ' AT/VS
                RC$(18, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(18, 4) = Date$ ' DATE
            End If
            For I2 = 1 To 4
                If QQ(I, I2, I1, 10) > REC(19, 0) Then
                    REC(19, 0) = QQ(I, I2, I1, 10) ' 3FGM
                    REC(19, 1) = 1 ' TIMES DONE
                    REC(19, 2) = I2 ' QTR
                    RC$(19, 0) = AA$(I, I1, 0) ' NAME
                    RC$(19, 1) = B$(I, 0) ' TEAM
                    RC$(19, 2) = SITE$(I) ' AT/VS
                    RC$(19, 3) = B$(1 - I, 0) ' OPPONENT
                    RC$(19, 4) = Date$ ' DATE
                End If
            Next I2
            For I2 = 5 To 8
                If QQ(I, I2, I1, 10) > REC(20, 0) Then
                    REC(20, 0) = QQ(I, I2, I1, 10) ' 3FGM
                    REC(20, 1) = 1 ' TIMES DONE
                    REC(20, 2) = I2 ' QTR
                    RC$(20, 0) = AA$(I, I1, 0) ' NAME
                    RC$(20, 1) = B$(I, 0) ' TEAM
                    RC$(20, 2) = SITE$(I) ' AT/VS
                    RC$(20, 3) = B$(1 - I, 0) ' OPPONENT
                    RC$(20, 4) = Date$ ' DATE
                End If
            Next I2

            ' 3-FIELD GOALS ATT IN A GAME, HALF
            If W%(I, I1, 1) > REC(21, 0) Then
                REC(21, 0) = W%(I, I1, 1) ' 3FGA
                REC(21, 1) = 1 ' TIMES DONE
                REC(21, 2) = 0 ' GAME
                RC$(21, 0) = AA$(I, I1, 0) ' NAME
                RC$(21, 1) = B$(I, 0) ' TEAM
                RC$(21, 2) = SITE$(I) ' AT/VS
                RC$(21, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(21, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 11) + QQ(I, 2, I1, 11) > REC(22, 0) Then
                REC(22, 0) = QQ(I, 1, I1, 11) + QQ(I, 2, I1, 11) ' 3FGA
                REC(22, 1) = 1 ' TIMES DONE
                REC(22, 2) = 1 ' HALF
                RC$(22, 0) = AA$(I, I1, 0) ' NAME
                RC$(22, 1) = B$(I, 0) ' TEAM
                RC$(22, 2) = SITE$(I) ' AT/VS
                RC$(22, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(22, 4) = Date$ ' DATE
            End If
            If QQ(I, 3, I1, 11) + QQ(I, 4, I1, 11) > REC(22, 0) Then
                REC(22, 0) = QQ(I, 3, I1, 10) + QQ(I, 4, I1, 10) ' 3FGA
                REC(22, 1) = 1 ' TIMES DONE
                REC(22, 2) = 2 ' HALF
                RC$(22, 0) = AA$(I, I1, 0) ' NAME
                RC$(22, 1) = B$(I, 0) ' TEAM
                RC$(22, 2) = SITE$(I) ' AT/VS
                RC$(22, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(22, 4) = Date$ ' DATE
            End If
            ' FREE THROWS -  MOST MADE NO MISSES, MOST ATT NONE MADE
            If A(I, I1, 9) = A(I, I1, 10) And A(I, I1, 10) > REC(23, 0) Then
                REC(23, 0) = A(I, I1, 10) ' MOST FTM
                RC$(23, 0) = AA$(I, I1, 0) ' NAME
                RC$(23, 1) = B$(I, 0) ' TEAM
                RC$(23, 2) = SITE$(I) ' AT/VS
                RC$(23, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(23, 4) = Date$ ' DATE
            End If
            If A(I, I1, 10) = 0 And A(I, I1, 9) > REC(24, 0) Then
                REC(24, 0) = A(I, I1, 9) ' 0 FTM /FTA
                RC$(24, 0) = AA$(I, I1, 0) ' NAME
                RC$(24, 1) = B$(I, 0) ' TEAM
                RC$(24, 2) = SITE$(I) ' AT/VS
                RC$(24, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(24, 4) = Date$ ' DATE
            End If

            ' MADE FREE THROWS IN A GAME, HALF, QUARTER
            If A(I, I1, 10) > REC(25, 0) Then
                REC(25, 0) = A(I, I1, 10) ' FTM
                REC(25, 1) = 1 ' TIMES DONE
                REC(25, 2) = 0 ' GAME
                RC$(25, 0) = AA$(I, I1, 0) ' NAME
                RC$(25, 1) = B$(I, 0) ' TEAM
                RC$(25, 2) = SITE$(I) ' AT/VS
                RC$(25, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(25, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 4) + QQ(I, 2, I1, 4) > REC(26, 0) Then
                REC(26, 0) = QQ(I, 1, I1, 4) + QQ(I, 2, I1, 4) ' FTM
                REC(26, 1) = 1 ' TIMES DONE
                REC(26, 2) = 1 ' HALF
                RC$(26, 0) = AA$(I, I1, 0) ' NAME
                RC$(26, 1) = B$(I, 0) ' TEAM
                RC$(26, 2) = SITE$(I) ' AT/VS
                RC$(26, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(26, 4) = Date$ ' DATE
            End If
            If QQ(I, 3, I1, 4) + QQ(I, 4, I1, 4) > REC(26, 0) Then
                REC(26, 0) = QQ(I, 3, I1, 4) + QQ(I, 4, I1, 4) ' FTM
                REC(26, 1) = 1 ' TIMES DONE
                REC(26, 2) = 2 ' HALF
                RC$(26, 0) = AA$(I, I1, 0) ' NAME
                RC$(26, 1) = B$(I, 0) ' TEAM
                RC$(26, 2) = SITE$(I) ' AT/VS
                RC$(26, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(26, 4) = Date$ ' DATE
            End If
            For I2 = 1 To 4
                If QQ(I, I2, I1, 4) > REC(27, 0) Then
                    REC(27, 0) = QQ(I, I2, I1, 4) ' FTM
                    REC(27, 1) = 1 ' TIMES DONE
                    REC(27, 2) = I2 ' QTR
                    RC$(27, 0) = AA$(I, I1, 0) ' NAME
                    RC$(27, 1) = B$(I, 0) ' TEAM
                    RC$(27, 2) = SITE$(I) ' AT/VS
                    RC$(27, 3) = B$(1 - I, 0) ' OPPONENT
                    RC$(27, 4) = Date$ ' DATE
                End If
            Next I2
            ' FREE THROWS ATTEMPTS IN A GAME, HALF, QUARTER
            If A(I, I1, 9) > REC(28, 0) Then
                REC(28, 0) = A(I, I1, 9) ' FTA
                REC(28, 1) = 1 ' TIMES DONE
                REC(28, 2) = 0 ' GAME
                RC$(28, 0) = AA$(I, I1, 0) ' NAME
                RC$(28, 1) = B$(I, 0) ' TEAM
                RC$(28, 2) = SITE$(I) ' AT/VS
                RC$(28, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(28, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 3) + QQ(I, 2, I1, 3) > REC(29, 0) Then
                REC(29, 0) = QQ(I, 1, I1, 3) + QQ(I, 2, I1, 3) ' FTA
                REC(29, 1) = 1 ' TIMES DONE
                REC(29, 2) = 1 ' HALF
                RC$(29, 0) = AA$(I, I1, 0) ' NAME
                RC$(29, 1) = B$(I, 0) ' TEAM
                RC$(29, 2) = SITE$(I) ' AT/VS
                RC$(29, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(29, 4) = Date$ ' DATE
            End If
            If QQ(I, 3, I1, 3) + QQ(I, 4, I1, 3) > REC(29, 0) Then
                REC(29, 0) = QQ(I, 3, I1, 3) + QQ(I, 4, I1, 3) ' FTM
                REC(29, 1) = 1 ' TIMES DONE
                REC(29, 2) = 2 ' HALF
                RC$(29, 0) = AA$(I, I1, 0) ' NAME
                RC$(29, 1) = B$(I, 0) ' TEAM
                RC$(29, 2) = SITE$(I) ' AT/VS
                RC$(29, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(29, 4) = Date$ ' DATE
            End If
            For I2 = 1 To 4
                If QQ(I, I2, I1, 3) > REC(30, 0) Then
                    REC(30, 0) = QQ(I, I2, I1, 3) ' FTM
                    REC(30, 1) = 1 ' TIMES DONE
                    REC(30, 2) = I2 ' QTR
                    RC$(30, 0) = AA$(I, I1, 0) ' NAME
                    RC$(30, 1) = B$(I, 0) ' TEAM
                    RC$(30, 2) = SITE$(I) ' AT/VS
                    RC$(30, 3) = B$(1 - I, 0) ' OPPONENT
                    RC$(30, 4) = Date$ ' DATE
                End If
            Next I2

            ' TOTAL REBOUNDS IN A GAME, HALF, QUARTER
            If A(I, I1, 11) > REC(31, 0) Then
                REC(31, 0) = A(I, I1, 11) ' FTA
                REC(31, 1) = 1 ' TIMES DONE
                REC(31, 2) = 0 ' GAME
                RC$(31, 0) = AA$(I, I1, 0) ' NAME
                RC$(31, 1) = B$(I, 0) ' TEAM
                RC$(31, 2) = SITE$(I) ' AT/VS
                RC$(31, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(31, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 5) + QQ(I, 2, I1, 5) > REC(32, 0) Then
                REC(32, 0) = QQ(I, 1, I1, 5) + QQ(I, 2, I1, 5) ' RB
                REC(32, 1) = 1 ' TIMES DONE
                REC(32, 2) = 1 ' HALF
                RC$(32, 0) = AA$(I, I1, 0) ' NAME
                RC$(32, 1) = B$(I, 0) ' TEAM
                RC$(32, 2) = SITE$(I) ' AT/VS
                RC$(32, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(32, 4) = Date$ ' DATE
            End If
            If QQ(I, 3, I1, 5) + QQ(I, 4, I1, 5) > REC(32, 0) Then
                REC(32, 0) = QQ(I, 3, I1, 5) + QQ(I, 4, I1, 5) ' RB
                REC(32, 1) = 1 ' TIMES DONE
                REC(32, 2) = 2 ' HALF
                RC$(32, 0) = AA$(I, I1, 0) ' NAME
                RC$(32, 1) = B$(I, 0) ' TEAM
                RC$(32, 2) = SITE$(I) ' AT/VS
                RC$(32, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(32, 4) = Date$ ' DATE
            End If
            For I2 = 1 To 4
                If QQ(I, I2, I1, 5) > REC(33, 0) Then
                    REC(33, 0) = QQ(I, I2, I1, 5) ' RB
                    REC(33, 1) = 1 ' TIMES DONE
                    REC(33, 2) = I2 ' QTR
                    RC$(33, 0) = AA$(I, I1, 0) ' NAME
                    RC$(33, 1) = B$(I, 0) ' TEAM
                    RC$(33, 2) = SITE$(I) ' AT/VS
                    RC$(33, 3) = B$(1 - I, 0) ' OPPONENT
                    RC$(33, 4) = Date$ ' DATE
                End If
            Next I2

            ' TOTAL OFFENSIVE REBOUNDS IN A GAME, HALF, QUARTER
            If A(I, I1, 24) > REC(34, 0) Then
                REC(34, 0) = A(I, I1, 24) ' FTA
                REC(34, 1) = 1 ' TIMES DONE
                REC(34, 2) = 0 ' GAME
                RC$(34, 0) = AA$(I, I1, 0) ' NAME
                RC$(34, 1) = B$(I, 0) ' TEAM
                RC$(34, 2) = SITE$(I) ' AT/VS
                RC$(34, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(34, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 12) + QQ(I, 2, I1, 12) > REC(35, 0) Then
                REC(35, 0) = QQ(I, 1, I1, 12) + QQ(I, 2, I1, 12) ' RB
                REC(35, 1) = 1 ' TIMES DONE
                REC(35, 2) = 1 ' HALF
                RC$(35, 0) = AA$(I, I1, 0) ' NAME
                RC$(35, 1) = B$(I, 0) ' TEAM
                RC$(35, 2) = SITE$(I) ' AT/VS
                RC$(35, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(35, 4) = Date$ ' DATE
            End If
            If QQ(I, 3, I1, 12) + QQ(I, 4, I1, 12) > REC(35, 0) Then
                REC(35, 0) = QQ(I, 3, I1, 12) + QQ(I, 4, I1, 12) ' RB
                REC(35, 1) = 1 ' TIMES DONE
                REC(35, 2) = 2 ' HALF
                RC$(35, 0) = AA$(I, I1, 0) ' NAME
                RC$(35, 1) = B$(I, 0) ' TEAM
                RC$(35, 2) = SITE$(I) ' AT/VS
                RC$(35, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(35, 4) = Date$ ' DATE
            End If
            For I2 = 1 To 4
                If QQ(I, I2, I1, 12) > REC(36, 0) Then
                    REC(36, 0) = QQ(I, I2, I1, 12) ' RB
                    REC(36, 1) = 1 ' TIMES DONE
                    REC(36, 2) = I2 ' QTR
                    RC$(36, 0) = AA$(I, I1, 0) ' NAME
                    RC$(36, 1) = B$(I, 0) ' TEAM
                    RC$(36, 2) = SITE$(I) ' AT/VS
                    RC$(36, 3) = B$(1 - I, 0) ' OPPONENT
                    RC$(36, 4) = Date$ ' DATE
                End If
            Next I2

            ' TOTAL DEFENSIVE REBOUNDS IN A GAME, HALF, QUARTER
            If A(I, I1, 11) - A(I, I1, 24) > REC(37, 0) Then
                REC(37, 0) = A(I, I1, 11) - A(I, I1, 24) ' FTA
                REC(37, 1) = 1 ' TIMES DONE
                REC(37, 2) = 0 ' GAME
                RC$(37, 0) = AA$(I, I1, 0) ' NAME
                RC$(37, 1) = B$(I, 0) ' TEAM
                RC$(37, 2) = SITE$(I) ' AT/VS
                RC$(37, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(37, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 5) + QQ(I, 2, I1, 5) - (QQ(I, 1, I1, 12) + QQ(I, 2, I1, 12)) > REC(38, 0) Then
                REC(38, 0) = QQ(I, 1, I1, 5) + QQ(I, 2, I1, 5) - (QQ(I, 1, I1, 12) + QQ(I, 2, I1, 12)) ' RB
                REC(38, 1) = 1 ' TIMES DONE
                REC(38, 2) = 1 ' HALF
                RC$(38, 0) = AA$(I, I1, 0) ' NAME
                RC$(38, 1) = B$(I, 0) ' TEAM
                RC$(38, 2) = SITE$(I) ' AT/VS
                RC$(38, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(38, 4) = Date$ ' DATE
            End If
            If QQ(I, 3, I1, 5) + QQ(I, 4, I1, 5) - (QQ(I, 3, I1, 12) + QQ(I, 4, I1, 12)) > REC(38, 0) Then
                REC(38, 0) = QQ(I, 3, I1, 5) + QQ(I, 4, I1, 5) - (QQ(I, 3, I1, 12) + QQ(I, 4, I1, 12)) ' RB
                REC(38, 1) = 1 ' TIMES DONE
                REC(38, 2) = 2 ' HALF
                RC$(38, 0) = AA$(I, I1, 0) ' NAME
                RC$(38, 1) = B$(I, 0) ' TEAM
                RC$(38, 2) = SITE$(I) ' AT/VS
                RC$(38, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(38, 4) = Date$ ' DATE
            End If
            For I2 = 1 To 4
                If QQ(I, I2, I1, 5) - QQ(I, I2, I1, 12) > REC(39, 0) Then
                    REC(39, 0) = QQ(I, I2, I1, 5) - QQ(I, I2, I1, 12) ' RB
                    REC(39, 1) = 1 ' TIMES DONE
                    REC(39, 2) = I2 ' QTR
                    RC$(39, 0) = AA$(I, I1, 0) ' NAME
                    RC$(39, 1) = B$(I, 0) ' TEAM
                    RC$(39, 2) = SITE$(I) ' AT/VS
                    RC$(39, 3) = B$(1 - I, 0) ' OPPONENT
                    RC$(39, 4) = Date$ ' DATE
                End If
            Next I2

            ' ASSISTS IN A GAME, HALF, QUARTER
            If A(I, I1, 14) > REC(40, 0) Then
                REC(40, 0) = A(I, I1, 14) ' FTA
                REC(40, 1) = 1 ' TIMES DONE
                REC(40, 2) = 0 ' GAME
                RC$(40, 0) = AA$(I, I1, 0) ' NAME
                RC$(40, 1) = B$(I, 0) ' TEAM
                RC$(40, 2) = SITE$(I) ' AT/VS
                RC$(40, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(40, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 8) + QQ(I, 2, I1, 8) > REC(41, 0) Then
                REC(41, 0) = QQ(I, 1, I1, 8) + QQ(I, 2, I1, 8) ' FTA
                REC(41, 1) = 1 ' TIMES DONE
                REC(41, 2) = 1 ' HALF
                RC$(41, 0) = AA$(I, I1, 0) ' NAME
                RC$(41, 1) = B$(I, 0) ' TEAM
                RC$(41, 2) = SITE$(I) ' AT/VS
                RC$(41, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(41, 4) = Date$ ' DATE
            End If
            If QQ(I, 3, I1, 8) + QQ(I, 4, I1, 8) > REC(41, 0) Then
                REC(41, 0) = QQ(I, 3, I1, 8) + QQ(I, 4, I1, 8) ' FTM
                REC(41, 1) = 1 ' TIMES DONE
                REC(41, 2) = 2 ' HALF
                RC$(41, 0) = AA$(I, I1, 0) ' NAME
                RC$(41, 1) = B$(I, 0) ' TEAM
                RC$(41, 2) = SITE$(I) ' AT/VS
                RC$(41, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(41, 4) = Date$ ' DATE
            End If
            For I2 = 1 To 4
                If QQ(I, I2, I1, 8) > REC(42, 0) Then
                    REC(42, 0) = QQ(I, I2, I1, 8) ' FTM
                    REC(42, 1) = 1 ' TIMES DONE
                    REC(42, 2) = I2 ' QTR
                    RC$(42, 0) = AA$(I, I1, 0) ' NAME
                    RC$(42, 1) = B$(I, 0) ' TEAM
                    RC$(42, 2) = SITE$(I) ' AT/VS
                    RC$(42, 3) = B$(1 - I, 0) ' OPPONENT
                    RC$(42, 4) = Date$ ' DATE
                End If
            Next I2

            ' STEALS IN A GAME, HALF, QUARTER
            If A(I, I1, 13) > REC(43, 0) Then
                REC(43, 0) = A(I, I1, 13) ' FTA
                REC(43, 1) = 1 ' TIMES DONE
                REC(43, 2) = 0 ' GAME
                RC$(43, 0) = AA$(I, I1, 0) ' NAME
                RC$(43, 1) = B$(I, 0) ' TEAM
                RC$(43, 2) = SITE$(I) ' AT/VS
                RC$(43, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(43, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 7) + QQ(I, 2, I1, 7) > REC(44, 0) Then
                REC(44, 0) = QQ(I, 1, I1, 7) + QQ(I, 2, I1, 7) ' FTA
                REC(44, 1) = 1 ' TIMES DONE
                REC(44, 2) = 1 ' HALF
                RC$(44, 0) = AA$(I, I1, 0) ' NAME
                RC$(44, 1) = B$(I, 0) ' TEAM
                RC$(44, 2) = SITE$(I) ' AT/VS
                RC$(44, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(44, 4) = Date$ ' DATE
            End If
            If QQ(I, 3, I1, 7) + QQ(I, 4, I1, 7) > REC(44, 0) Then
                REC(44, 0) = QQ(I, 3, I1, 7) + QQ(I, 4, I1, 7) ' FTM
                REC(44, 1) = 1 ' TIMES DONE
                REC(44, 2) = 2 ' HALF
                RC$(44, 0) = AA$(I, I1, 0) ' NAME
                RC$(44, 1) = B$(I, 0) ' TEAM
                RC$(44, 2) = SITE$(I) ' AT/VS
                RC$(44, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(44, 4) = Date$ ' DATE
            End If
            For I2 = 1 To 4
                If QQ(I, I2, I1, 7) > REC(45, 0) Then
                    REC(45, 0) = QQ(I, I2, I1, 7) ' FTM
                    REC(45, 1) = 1 ' TIMES DONE
                    REC(45, 2) = I2 ' QTR
                    RC$(45, 0) = AA$(I, I1, 0) ' NAME
                    RC$(45, 1) = B$(I, 0) ' TEAM
                    RC$(45, 2) = SITE$(I) ' AT/VS
                    RC$(45, 3) = B$(1 - I, 0) ' OPPONENT
                    RC$(45, 4) = Date$ ' DATE
                End If
            Next I2

            ' BLOCKS IN A GAME, HALF, QUARTER
            If A(I, I1, 15) > REC(46, 0) Then
                REC(46, 0) = A(I, I1, 15) ' FTA
                REC(46, 1) = 1 ' TIMES DONE
                REC(46, 2) = 0 ' GAME
                RC$(46, 0) = AA$(I, I1, 0) ' NAME
                RC$(46, 1) = B$(I, 0) ' TEAM
                RC$(46, 2) = SITE$(I) ' AT/VS
                RC$(46, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(46, 4) = Date$ ' DATE
            End If
            If QQ(I, 1, I1, 9) + QQ(I, 2, I1, 9) > REC(47, 0) Then
                REC(47, 0) = QQ(I, 1, I1, 9) + QQ(I, 2, I1, 9) ' FTA
                REC(47, 1) = 1 ' TIMES DONE
                REC(47, 2) = 1 ' HALF
                RC$(47, 0) = AA$(I, I1, 0) ' NAME
                RC$(47, 1) = B$(I, 0) ' TEAM
                RC$(47, 2) = SITE$(I) ' AT/VS
                RC$(47, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(47, 4) = Date$ ' DATE
            End If
            If QQ(I, 3, I1, 9) + QQ(I, 4, I1, 9) > REC(47, 0) Then
                REC(47, 0) = QQ(I, 3, I1, 9) + QQ(I, 4, I1, 9) ' FTM
                REC(47, 1) = 1 ' TIMES DONE
                REC(47, 2) = 2 ' HALF
                RC$(47, 0) = AA$(I, I1, 0) ' NAME
                RC$(47, 1) = B$(I, 0) ' TEAM
                RC$(47, 2) = SITE$(I) ' AT/VS
                RC$(47, 3) = B$(1 - I, 0) ' OPPONENT
                RC$(47, 4) = Date$ ' DATE
            End If
            For I2 = 1 To 4
                If QQ(I, I2, I1, 9) > REC(48, 0) Then
                    REC(48, 0) = QQ(I, I2, I1, 9) ' FTM
                    REC(48, 1) = 1 ' TIMES DONE
                    REC(48, 2) = I2 ' QTR
                    RC$(48, 0) = AA$(I, I1, 0) ' NAME
                    RC$(48, 1) = B$(I, 0) ' TEAM
                    RC$(48, 2) = SITE$(I) ' AT/VS
                    RC$(48, 3) = B$(1 - I, 0) ' OPPONENT
                    RC$(48, 4) = Date$ ' DATE
                End If
            Next I2
        Next
    Next

    ' MOST OVERTIME PERIODS
    If H - 5 > TREC(0, 0) Then
        TREC(0, 0) = H - 5
        TREC(0, 1) = S(0, 0)
        TREC(0, 2) = S(1, 0)
        TRC$(0, 0) = B$(0, 0)
        TRC$(0, 1) = B$(1, 0)
        TRC$(0, 2) = Date$
    End If

    For I = 0 To 1

        If S(I, 0) > TREC(1, 0) Then
            TREC(1, 0) = S(I, 0)
            TREC(1, 1) = 1
            TREC(1, 2) = 0
            TRC$(1, 0) = B$(I, 0)
            TRC$(1, 1) = SITE$(I)
            TRC$(1, 2) = B$(1 - I, 0)
            TRC$(1, 3) = Date$
        End If

        If S(I, 0) < TREC(2, 0) Then
            TREC(2, 0) = S(I, 0)
            TREC(2, 1) = 1
            TREC(2, 2) = 0
            TRC$(2, 0) = B$(I, 0)
            TRC$(2, 1) = SITE$(I)
            TRC$(2, 2) = B$(1 - I, 0)
            TRC$(2, 3) = Date$
        End If

        If S(I, 0) + S(1 - I, 0) > TREC(3, 0) Then
            TREC(3, 0) = S(I, 0) + S(1 - I, 0)
            TREC(3, 1) = S(I, 0)
            TREC(3, 2) = S(1 - I, 0)
            TRC$(3, 0) = B$(I, 0)
            TRC$(3, 1) = SITE$(I)
            TRC$(3, 2) = B$(1 - I, 0)
            TRC$(3, 3) = Date$
        End If

        If S(I, 0) + S(1 - I, 0) < TREC(4, 0) Then
            TREC(4, 0) = S(I, 0) + S(1 - I, 0)
            TREC(4, 1) = S(I, 0)
            TREC(4, 2) = S(1 - I, 0)
            TRC$(4, 0) = B$(I, 0)
            TRC$(4, 1) = SITE$(I)
            TRC$(4, 2) = B$(1 - I, 0)
            TRC$(4, 3) = Date$
        End If

        If S(I, 1) + S(I, 2) > TREC(6, 0) Then
            TREC(6, 0) = S(I, 1) + S(I, 2)
            TREC(6, 1) = 1
            TREC(6, 2) = 0
            TRC$(6, 0) = B$(I, 0)
            TRC$(6, 1) = SITE$(I)
            TRC$(6, 2) = B$(1 - I, 0)
            TRC$(6, 3) = Date$
        End If

        If S(I, 1) + S(I, 2) < TREC(7, 0) Then
            TREC(7, 0) = S(I, 1) + S(I, 2)
            TREC(7, 1) = 1
            TREC(7, 2) = 0
            TRC$(7, 0) = B$(I, 0)
            TRC$(7, 1) = SITE$(I)
            TRC$(7, 2) = B$(1 - I, 0)
            TRC$(7, 3) = Date$
        End If

        If S(I, 1) + S(I, 2) + S(1 - I, 1) + S(1 - I, 2) > TREC(8, 0) Then
            TREC(8, 0) = S(I, 1) + S(1 - I, 1) + S(I, 2) + S(1 - I, 2)
            TREC(8, 1) = S(I, 1) + S(I, 2)
            TREC(8, 2) = S(1 - I, 1) + S(1 - I, 2)
            TRC$(8, 0) = B$(I, 0)
            TRC$(8, 1) = SITE$(I)
            TRC$(8, 2) = B$(1 - I, 0)
            TRC$(8, 3) = Date$
        End If

        If S(I, 1) + S(I, 2) + S(1 - I, 1) + S(1 - I, 2) < TREC(9, 0) Then
            TREC(9, 0) = S(I, 1) + S(1 - I, 1) + S(I, 2) + S(1 - I, 2)
            TREC(9, 1) = S(I, 1) + S(I, 2)
            TREC(9, 2) = S(1 - I, 1) + S(1 - I, 2)
            TRC$(9, 0) = B$(I, 0)
            TRC$(9, 1) = SITE$(I)
            TRC$(9, 2) = B$(1 - I, 0)
            TRC$(9, 3) = Date$
        End If

        If S(I, 3) + S(I, 4) > TREC(10, 0) Then
            TREC(10, 0) = S(I, 3) + S(I, 4)
            TREC(10, 1) = 1
            TREC(10, 2) = 0
            TRC$(10, 0) = B$(I, 0)
            TRC$(10, 1) = SITE$(I)
            TRC$(10, 2) = B$(1 - I, 0)
            TRC$(10, 3) = Date$
        End If

        If S(I, 3) + S(I, 4) < TREC(11, 0) Then
            TREC(11, 0) = S(I, 3) + S(I, 4)
            TREC(11, 1) = 1
            TREC(11, 2) = 0
            TRC$(11, 0) = B$(I, 0)
            TRC$(11, 1) = SITE$(I)
            TRC$(11, 2) = B$(1 - I, 0)
            TRC$(11, 3) = Date$
        End If

        If S(I, 3) + S(I, 4) + S(1 - I, 3) + S(1 - I, 4) > TREC(12, 0) Then
            TREC(12, 0) = S(I, 3) + S(1 - I, 3) + S(I, 4) + S(1 - I, 4)
            TREC(12, 1) = S(I, 3) + S(I, 4)
            TREC(12, 2) = S(1 - I, 3) + S(1 - I, 4)
            TRC$(12, 0) = B$(I, 0)
            TRC$(12, 1) = SITE$(I)
            TRC$(12, 2) = B$(1 - I, 0)
            TRC$(12, 3) = Date$
        End If

        If S(I, 3) + S(I, 4) + S(1 - I, 3) + S(1 - I, 4) < TREC(13, 0) Then
            TREC(13, 0) = S(I, 3) + S(1 - I, 3) + S(I, 4) + S(1 - I, 4)
            TREC(13, 1) = S(I, 3) + S(I, 4)
            TREC(13, 2) = S(1 - I, 3) + S(1 - I, 4)
            TRC$(13, 0) = B$(I, 0)
            TRC$(13, 1) = SITE$(I)
            TRC$(13, 2) = B$(1 - I, 0)
            TRC$(13, 3) = Date$
        End If

        If S(I, 1) > TREC(14, 0) Then
            TREC(14, 0) = S(I, 1)
            TREC(14, 1) = 1
            TREC(14, 2) = 0
            TRC$(14, 0) = B$(I, 0)
            TRC$(14, 1) = SITE$(I)
            TRC$(14, 2) = B$(1 - I, 0)
            TRC$(14, 3) = Date$
        End If

        If S(I, 1) < TREC(15, 0) Then
            TREC(15, 0) = S(I, 1)
            TREC(15, 1) = 1
            TREC(15, 2) = 0
            TRC$(15, 0) = B$(I, 0)
            TRC$(15, 1) = SITE$(I)
            TRC$(15, 2) = B$(1 - I, 0)
            TRC$(15, 3) = Date$
        End If

        If S(I, 1) + S(1 - I, 1) > TREC(16, 0) Then
            TREC(16, 0) = S(I, 1) + S(1 - I, 1)
            TREC(16, 1) = S(I, 1)
            TREC(16, 2) = S(1 - I, 1)
            TRC$(16, 0) = B$(I, 0)
            TRC$(16, 1) = SITE$(I)
            TRC$(16, 2) = B$(1 - I, 0)
            TRC$(16, 3) = Date$
        End If

        If S(I, 1) + S(1 - I, 1) < TREC(17, 0) Then
            TREC(17, 0) = S(I, 1) + S(1 - I, 1)
            TREC(17, 1) = S(I, 1)
            TREC(17, 2) = S(1 - I, 1)
            TRC$(17, 0) = B$(I, 0)
            TRC$(17, 1) = SITE$(I)
            TRC$(17, 2) = B$(1 - I, 0)
            TRC$(17, 3) = Date$
        End If

        If S(I, 2) > TREC(18, 0) Then
            TREC(18, 0) = S(I, 2)
            TREC(18, 1) = 1
            TREC(18, 2) = 0
            TRC$(18, 0) = B$(I, 0)
            TRC$(18, 1) = SITE$(I)
            TRC$(18, 2) = B$(1 - I, 0)
            TRC$(18, 3) = Date$
        End If

        If S(I, 2) < TREC(19, 0) Then
            TREC(19, 0) = S(I, 2)
            TREC(19, 1) = 1
            TREC(19, 2) = 0
            TRC$(19, 0) = B$(I, 0)
            TRC$(19, 1) = SITE$(I)
            TRC$(19, 2) = B$(1 - I, 0)
            TRC$(19, 3) = Date$
        End If

        If S(I, 2) + S(1 - I, 2) > TREC(20, 0) Then
            TREC(20, 0) = S(I, 2) + S(1 - I, 2)
            TREC(20, 1) = S(I, 2)
            TREC(20, 2) = S(1 - I, 2)
            TRC$(20, 0) = B$(I, 0)
            TRC$(20, 1) = SITE$(I)
            TRC$(20, 2) = B$(1 - I, 0)
            TRC$(20, 3) = Date$
        End If

        If S(I, 2) + S(1 - I, 2) < TREC(21, 0) Then
            TREC(21, 0) = S(I, 2) + S(1 - I, 2)
            TREC(21, 1) = S(I, 2)
            TREC(21, 2) = S(1 - I, 2)
            TRC$(21, 0) = B$(I, 0)
            TRC$(21, 1) = SITE$(I)
            TRC$(21, 2) = B$(1 - I, 0)
            TRC$(21, 3) = Date$
        End If

        If S(I, 3) > TREC(22, 0) Then
            TREC(22, 0) = S(I, 3)
            TREC(22, 1) = 1
            TREC(22, 2) = 0
            TRC$(22, 0) = B$(I, 0)
            TRC$(22, 1) = SITE$(I)
            TRC$(22, 2) = B$(1 - I, 0)
            TRC$(22, 3) = Date$
        End If

        If S(I, 3) < TREC(23, 0) Then
            TREC(23, 0) = S(I, 3)
            TREC(23, 1) = 1
            TREC(23, 2) = 0
            TRC$(23, 0) = B$(I, 0)
            TRC$(23, 1) = SITE$(I)
            TRC$(23, 2) = B$(1 - I, 0)
            TRC$(23, 3) = Date$
        End If

        If S(I, 3) + S(1 - I, 3) > TREC(24, 0) Then
            TREC(24, 0) = S(I, 3) + S(1 - I, 3)
            TREC(24, 1) = S(I, 3)
            TREC(24, 2) = S(1 - I, 3)
            TRC$(24, 0) = B$(I, 0)
            TRC$(24, 1) = SITE$(I)
            TRC$(24, 2) = B$(1 - I, 0)
            TRC$(24, 3) = Date$
        End If

        If S(I, 3) + S(1 - I, 3) < TREC(25, 0) Then
            TREC(25, 0) = S(I, 3) + S(1 - I, 3)
            TREC(25, 1) = S(I, 3)
            TREC(25, 2) = S(1 - I, 3)
            TRC$(25, 0) = B$(I, 0)
            TRC$(25, 1) = SITE$(I)
            TRC$(25, 2) = B$(1 - I, 0)
            TRC$(25, 3) = Date$
        End If

        If S(I, 4) > TREC(26, 0) Then
            TREC(26, 0) = S(I, 4)
            TREC(26, 1) = 1
            TREC(26, 2) = 0
            TRC$(26, 0) = B$(I, 0)
            TRC$(26, 1) = SITE$(I)
            TRC$(26, 2) = B$(1 - I, 0)
            TRC$(26, 3) = Date$
        End If

        If S(I, 4) < TREC(27, 0) Then
            TREC(27, 0) = S(I, 4)
            TREC(27, 1) = 1
            TREC(27, 2) = 0
            TRC$(27, 0) = B$(I, 0)
            TRC$(27, 1) = SITE$(I)
            TRC$(27, 2) = B$(1 - I, 0)
            TRC$(27, 3) = Date$
        End If

        If S(I, 4) + S(1 - I, 4) > TREC(28, 0) Then
            TREC(28, 0) = S(I, 4) + S(1 - I, 4)
            TREC(28, 1) = S(I, 4)
            TREC(28, 2) = S(1 - I, 4)
            TRC$(28, 0) = B$(I, 0)
            TRC$(28, 1) = SITE$(I)
            TRC$(28, 2) = B$(1 - I, 0)
            TRC$(28, 3) = Date$
        End If

        If S(I, 4) + S(1 - I, 4) < TREC(29, 0) Then
            TREC(29, 0) = S(I, 4) + S(1 - I, 4)
            TREC(29, 1) = S(I, 4)
            TREC(29, 2) = S(1 - I, 4)
            TRC$(29, 0) = B$(I, 0)
            TRC$(29, 1) = SITE$(I)
            TRC$(29, 2) = B$(1 - I, 0)
            TRC$(29, 3) = Date$
        End If

        ' TEAM FIELD GOAL PERCENTAGES
        For I1 = 0 To 14: V8(I, I1) = 0: V8(1 - I, I1) = 0: Next
        For I2 = 0 To 1
            For I1 = 0 To 14
                V8(I2, 0) = V8(I2, 0) + A(I2, I1, 8)
                V8(I2, 1) = V8(I2, 1) + A(I2, I1, 7)
                V8(I2, 2) = V8(I2, 2) + A(I2, I1, 10)
                V8(I2, 3) = V8(I2, 3) + A(I2, I1, 9)
                V8(I2, 4) = V8(I2, 4) + A(I2, I1, 11)
                V8(I2, 5) = V8(I2, 5) + (A(I2, I1, 8) * 2) + A(I2, I1, 10) + W%(I2, I1, 0)
                V8(I2, 13) = V8(I2, 13) + W%(I2, I1, 0)
                V8(I2, 14) = V8(I2, 14) + W%(I2, I1, 1)
                V8(I2, 6) = TF(I2)
                V8(I2, 7) = V8(I2, 7) + A(I2, I1, 14)
                V8(I2, 8) = V8(I2, 8) + A(I2, I1, 15)
                V8(I2, 9) = V8(I2, 9) + A(I2, I1, 13)
                V8(I2, 11) = V8(I2, 11) + A(I2, I1, 24)
                V8(I2, 12) = V8(I2, 12) + A(I2, I1, 23)
                V8(I2, 10) = V8(I2, 10) + ZZ(I2, I1)
            Next
        Next

        If V8(I, 0) / V8(I, 1) > TREC(30, 0) Then
            TREC(30, 0) = V8(I, 0) / V8(I, 1)
            TREC(30, 1) = V8(I, 0)
            TREC(30, 2) = V8(I, 1)
            TRC$(30, 0) = B$(I, 0)
            TRC$(30, 1) = SITE$(I)
            TRC$(30, 2) = B$(1 - I, 0)
            TRC$(30, 3) = Date$
        End If

        If V8(I, 0) / V8(I, 1) < TREC(31, 0) Then
            TREC(31, 0) = V8(I, 0) / V8(I, 1)
            TREC(31, 1) = V8(I, 0)
            TREC(31, 2) = V8(I, 1)
            TRC$(31, 0) = B$(I, 0)
            TRC$(31, 1) = SITE$(I)
            TRC$(31, 2) = B$(1 - I, 0)
            TRC$(31, 3) = Date$
        End If

        If (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1)) > TREC(32, 0) Then
            TREC(32, 0) = (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1))
            TREC(32, 1) = V8(I, 0) / V8(I, 1)
            TREC(32, 2) = V8(1 - I, 0) / V8(1 - I, 1)
            TRC$(32, 0) = B$(I, 0)
            TRC$(32, 1) = SITE$(I)
            TRC$(32, 2) = B$(1 - I, 0)
            TRC$(32, 3) = Date$
        End If

        If (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1)) < TREC(33, 0) Then
            TREC(33, 0) = (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1))
            TREC(33, 1) = V8(I, 0) / V8(I, 1)
            TREC(33, 2) = V8(1 - I, 0) / V8(1 - I, 1)
            TRC$(33, 0) = B$(I, 0)
            TRC$(33, 1) = SITE$(I)
            TRC$(33, 2) = B$(1 - I, 0)
            TRC$(33, 3) = Date$
        End If

        ' TEAM FIELD GOALS MADE
        If V8(I, 0) > TREC(34, 0) Then
            TREC(34, 0) = V8(I, 0)
            TREC(34, 1) = 0
            TREC(34, 2) = 0
            TRC$(34, 0) = B$(I, 0)
            TRC$(34, 1) = SITE$(I)
            TRC$(34, 2) = B$(1 - I, 0)
            TRC$(34, 3) = Date$
        End If

        If V8(I, 0) < TREC(35, 0) Then
            TREC(35, 0) = V8(I, 0)
            TREC(35, 1) = 0
            TREC(35, 2) = 0
            TRC$(35, 0) = B$(I, 0)
            TRC$(35, 1) = SITE$(I)
            TRC$(35, 2) = B$(1 - I, 0)
            TRC$(35, 3) = Date$
        End If

        If V8(I, 0) + V8(1 - I, 0) > TREC(36, 0) Then
            TREC(36, 0) = V8(I, 0) + V8(1 - I, 0)
            TREC(36, 1) = V8(I, 0)
            TREC(36, 2) = V8(1 - I, 0)
            TRC$(36, 0) = B$(I, 0)
            TRC$(36, 1) = SITE$(I)
            TRC$(36, 2) = B$(1 - I, 0)
            TRC$(36, 3) = Date$
        End If

        If V8(I, 0) + V8(1 - I, 0) < TREC(37, 0) Then
            TREC(37, 0) = V8(I, 0) + V8(1 - I, 0)
            TREC(37, 1) = V8(I, 0)
            TREC(37, 2) = V8(1 - I, 0)
            TRC$(37, 0) = B$(I, 0)
            TRC$(37, 1) = SITE$(I)
            TRC$(37, 2) = B$(1 - I, 0)
            TRC$(37, 3) = Date$
        End If

        If QR(I, 1, 0) + QR(I, 2, 0) > TREC(38, 0) Then
            TREC(38, 0) = QR(I, 1, 0) + QR(I, 2, 0)
            TREC(38, 1) = 1
            TREC(38, 2) = 0
            TRC$(38, 0) = B$(I, 0)
            TRC$(38, 1) = SITE$(I)
            TRC$(38, 2) = B$(1 - I, 0)
            TRC$(38, 3) = Date$
        End If

        If QR(I, 3, 0) + QR(I, 4, 0) > TREC(38, 0) Then
            TREC(38, 0) = QR(I, 3, 0) + QR(I, 4, 0)
            TREC(38, 1) = 2
            TREC(38, 2) = 0
            TRC$(38, 0) = B$(I, 0)
            TRC$(38, 1) = SITE$(I)
            TRC$(38, 2) = B$(1 - I, 0)
            TRC$(38, 3) = Date$
        End If

        If QR(I, 1, 0) + QR(I, 2, 0) + QR(1 - I, 1, 0) + QR(1 - I, 2, 0) > TREC(39, 0) Then
            TREC(39, 0) = QR(I, 1, 0) + QR(I, 2, 0) + QR(1 - I, 1, 0) + QR(1 - I, 2, 0)
            TREC(39, 1) = 1
            TREC(39, 2) = 0
            TRC$(39, 0) = B$(I, 0)
            TRC$(39, 1) = SITE$(I)
            TRC$(39, 2) = B$(1 - I, 0)
            TRC$(39, 3) = Date$
        End If

        If QR(I, 3, 0) + QR(I, 4, 0) + QR(1 - I, 3, 0) + QR(1 - I, 4, 0) > TREC(39, 0) Then
            TREC(39, 0) = QR(I, 3, 0) + QR(I, 4, 0) + QR(1 - I, 3, 0) + QR(1 - I, 4, 0)
            TREC(39, 1) = 2
            TREC(39, 2) = 0
            TRC$(39, 0) = B$(I, 0)
            TRC$(39, 1) = SITE$(I)
            TRC$(39, 2) = B$(1 - I, 0)
            TRC$(39, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 0) > TREC(40, 0) Then
                TREC(40, 0) = QR(I, I2, 0)
                TREC(40, 1) = I2
                TREC(40, 2) = 0
                TRC$(40, 0) = B$(I, 0)
                TRC$(40, 1) = SITE$(I)
                TRC$(40, 2) = B$(1 - I, 0)
                TRC$(40, 3) = Date$
            End If

            If QR(I, I2, 0) + QR(1 - I, I2, 0) > TREC(41, 0) Then
                TREC(41, 0) = QR(I, I2, 0) + QR(1 - I, I2, 0)
                TREC(41, 1) = I2
                TREC(41, 2) = QR(I, I2, 0)
                TRC$(41, 0) = B$(I, 0)
                TRC$(41, 1) = SITE$(I)
                TRC$(41, 2) = B$(1 - I, 0)
                TRC$(41, 3) = Date$
            End If
        Next

        'TEAM FIELD GOAL ATTEMPTS
        If V8(I, 1) > TREC(42, 0) Then
            TREC(42, 0) = V8(I, 1)
            TREC(42, 1) = 0
            TREC(42, 2) = 0
            TRC$(42, 0) = B$(I, 0)
            TRC$(42, 1) = SITE$(I)
            TRC$(42, 2) = B$(1 - I, 0)
            TRC$(42, 3) = Date$
        End If

        If V8(I, 1) < TREC(43, 0) Then
            TREC(43, 0) = V8(I, 1)
            TREC(43, 1) = 0
            TREC(43, 2) = 0
            TRC$(43, 0) = B$(I, 0)
            TRC$(43, 1) = SITE$(I)
            TRC$(43, 2) = B$(1 - I, 0)
            TRC$(43, 3) = Date$
        End If

        If V8(I, 1) + V8(1 - I, 1) > TREC(44, 0) Then
            TREC(44, 0) = V8(I, 1) + V8(1 - I, 1)
            TREC(44, 1) = V8(I, 1)
            TREC(44, 2) = V8(1 - I, 1)
            TRC$(44, 0) = B$(I, 0)
            TRC$(44, 1) = SITE$(I)
            TRC$(44, 2) = B$(1 - I, 0)
            TRC$(44, 3) = Date$
        End If

        If V8(I, 1) + V8(1 - I, 1) < TREC(45, 0) Then
            TREC(45, 0) = V8(I, 1) + V8(1 - I, 1)
            TREC(45, 1) = V8(I, 1)
            TREC(45, 2) = V8(1 - I, 1)
            TRC$(45, 0) = B$(I, 0)
            TRC$(45, 1) = SITE$(I)
            TRC$(45, 2) = B$(1 - I, 0)
            TRC$(45, 3) = Date$
        End If

        If QR(I, 1, 1) + QR(I, 2, 1) > TREC(46, 0) Then
            TREC(46, 0) = QR(I, 1, 1) + QR(I, 2, 1)
            TREC(46, 1) = 1
            TREC(46, 2) = 0
            TRC$(46, 0) = B$(I, 0)
            TRC$(46, 1) = SITE$(I)
            TRC$(46, 2) = B$(1 - I, 0)
            TRC$(46, 3) = Date$
        End If

        If QR(I, 3, 1) + QR(I, 4, 1) > TREC(46, 0) Then
            TREC(46, 0) = QR(I, 3, 1) + QR(I, 4, 1)
            TREC(46, 1) = 2
            TREC(46, 2) = 0
            TRC$(46, 0) = B$(I, 0)
            TRC$(46, 1) = SITE$(I)
            TRC$(46, 2) = B$(1 - I, 0)
            TRC$(46, 3) = Date$
        End If

        If QR(I, 1, 1) + QR(I, 2, 1) + QR(1 - I, 1, 1) + QR(1 - I, 2, 1) > TREC(47, 0) Then
            TREC(47, 0) = QR(I, 1, 1) + QR(I, 2, 1) + QR(1 - I, 1, 1) + QR(1 - I, 2, 1)
            TREC(47, 1) = 1
            TREC(47, 2) = QR(I, 1, 1) + QR(I, 2, 1)
            TRC$(47, 0) = B$(I, 0)
            TRC$(47, 1) = SITE$(I)
            TRC$(47, 2) = B$(1 - I, 0)
            TRC$(47, 3) = Date$
        End If

        If QR(I, 3, 1) + QR(I, 4, 1) + QR(1 - I, 3, 1) + QR(1 - I, 4, 1) > TREC(47, 0) Then
            TREC(47, 0) = QR(I, 3, 1) + QR(I, 4, 1) + QR(1 - I, 3, 1) + QR(1 - I, 4, 1)
            TREC(47, 1) = 1
            TREC(47, 2) = QR(I, 3, 1) + QR(I, 4, 1)
            TRC$(47, 0) = B$(I, 0)
            TRC$(47, 1) = SITE$(I)
            TRC$(47, 2) = B$(1 - I, 0)
            TRC$(47, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 1) > TREC(48, 0) Then
                TREC(48, 0) = QR(I, I2, 1)
                TREC(48, 1) = I2
                TREC(48, 2) = 0
                TRC$(48, 0) = B$(I, 0)
                TRC$(48, 1) = SITE$(I)
                TRC$(48, 2) = B$(1 - I, 0)
                TRC$(48, 3) = Date$
            End If

            If QR(I, I2, 1) + QR(1 - I, I2, 1) > TREC(49, 0) Then
                TREC(49, 0) = QR(I, I2, 1) + QR(1 - I, I2, 1)
                TREC(49, 1) = I2
                TREC(49, 2) = QR(I, I2, 1)
                TRC$(49, 0) = B$(I, 0)
                TRC$(49, 1) = SITE$(I)
                TRC$(49, 2) = B$(1 - I, 0)
                TRC$(49, 3) = Date$
            End If
        Next

        'TEAM FIELD GOAL ATTEMPTS
        If V8(I, 13) = V8(I, 14) And V8(I, 14) > TREC(50, 0) Then
            TREC(50, 0) = V8(I, 13)
            TREC(50, 1) = 0
            TREC(50, 2) = 0
            TRC$(50, 0) = B$(I, 0)
            TRC$(50, 1) = SITE$(I)
            TRC$(50, 2) = B$(1 - I, 0)
            TRC$(50, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS MOST MADE
        If V8(I, 13) = V8(I, 14) And V8(1 - I, 13) = V8(1 - I, 14) And V8(I, 14) + V8(1 - I, 14) > TREC(51, 0) Then
            TREC(51, 0) = V8(I, 13)
            TREC(51, 1) = V8(1 - I, 13)
            TREC(51, 2) = 0
            TRC$(51, 0) = B$(I, 0)
            TRC$(51, 1) = SITE$(I)
            TRC$(51, 2) = B$(1 - I, 0)
            TRC$(51, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) = 0 And V8(I, 14) > TREC(52, 0) Then
            TREC(52, 0) = V8(I, 14)
            TREC(52, 1) = 0
            TREC(52, 2) = 0
            TRC$(52, 0) = B$(I, 0)
            TRC$(52, 1) = SITE$(I)
            TRC$(52, 2) = B$(1 - I, 0)
            TRC$(52, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) > TREC(53, 0) Then
            TREC(53, 0) = V8(I, 13)
            TREC(53, 1) = 0
            TREC(53, 2) = 0
            TRC$(53, 0) = B$(I, 0)
            TRC$(53, 1) = SITE$(I)
            TRC$(53, 2) = B$(1 - I, 0)
            TRC$(53, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) + V8(1 - I, 13) > TREC(54, 0) Then
            TREC(54, 0) = V8(I, 13) + V8(1 - I, 13)
            TREC(54, 1) = V8(I, 13)
            TREC(54, 2) = V8(1 - I, 13)
            TRC$(54, 0) = B$(I, 0)
            TRC$(54, 1) = SITE$(I)
            TRC$(54, 2) = B$(1 - I, 0)
            TRC$(54, 3) = Date$
        End If

        If QR(I, 1, 13) + QR(I, 2, 13) > TREC(55, 0) Then
            TREC(55, 0) = QR(I, 1, 13) + QR(I, 2, 13)
            TREC(55, 1) = 1
            TREC(55, 2) = 0
            TRC$(55, 0) = B$(I, 0)
            TRC$(55, 1) = SITE$(I)
            TRC$(55, 2) = B$(1 - I, 0)
            TRC$(55, 3) = Date$
        End If

        If QR(I, 3, 13) + QR(I, 4, 13) > TREC(55, 0) Then
            TREC(55, 0) = QR(I, 3, 13) + QR(I, 4, 13)
            TREC(55, 1) = 2
            TREC(55, 2) = 0
            TRC$(55, 0) = B$(I, 0)
            TRC$(55, 1) = SITE$(I)
            TRC$(55, 2) = B$(1 - I, 0)
            TRC$(55, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 13) > TREC(56, 0) Then
                TREC(56, 0) = QR(I, I2, 13)
                TREC(56, 1) = I2
                TREC(56, 2) = 0
                TRC$(56, 0) = B$(I, 0)
                TRC$(56, 1) = SITE$(I)
                TRC$(56, 2) = B$(1 - I, 0)
                TRC$(56, 3) = Date$
            End If
        Next
        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 14) > TREC(57, 0) Then
            TREC(57, 0) = V8(I, 14)
            TREC(57, 1) = 0
            TREC(57, 2) = 0
            TRC$(57, 0) = B$(I, 0)
            TRC$(57, 1) = SITE$(I)
            TRC$(57, 2) = B$(1 - I, 0)
            TRC$(57, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 14) + V8(1 - I, 14) > TREC(58, 0) Then
            TREC(58, 0) = V8(I, 14) + V8(1 - I, 14)
            TREC(58, 1) = V8(I, 14)
            TREC(58, 2) = V8(1 - I, 14)
            TRC$(58, 0) = B$(I, 0)
            TRC$(58, 1) = SITE$(I)
            TRC$(58, 2) = B$(1 - I, 0)
            TRC$(58, 3) = Date$
        End If

        If QR(I, 1, 14) + QR(I, 2, 14) > TREC(59, 0) Then
            TREC(59, 0) = QR(I, 1, 14) + QR(I, 2, 14)
            TREC(59, 1) = 1
            TREC(59, 2) = 0
            TRC$(59, 0) = B$(I, 0)
            TRC$(59, 1) = SITE$(I)
            TRC$(59, 2) = B$(1 - I, 0)
            TRC$(59, 3) = Date$
        End If

        If QR(I, 3, 14) + QR(I, 4, 14) > TREC(59, 0) Then
            TREC(59, 0) = QR(I, 3, 14) + QR(I, 4, 14)
            TREC(59, 1) = 2
            TREC(59, 2) = 0
            TRC$(59, 0) = B$(I, 0)
            TRC$(59, 1) = SITE$(I)
            TRC$(59, 2) = B$(1 - I, 0)
            TRC$(59, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS NO MISS
        If V8(I, 2) = V8(I, 3) And V8(I, 2) > TREC(60, 0) Then
            TREC(60, 0) = V8(I, 2)
            TREC(60, 1) = 0
            TREC(60, 2) = 0
            TRC$(60, 0) = B$(I, 0)
            TRC$(60, 1) = SITE$(I)
            TRC$(60, 2) = B$(1 - I, 0)
            TRC$(60, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS LOW %
        If V8(I, 2) / V8(I, 3) < TREC(61, 0) Then
            TREC(61, 0) = V8(I, 2) / V8(I, 3)
            TREC(61, 1) = V8(I, 2)
            TREC(61, 2) = V8(I, 3)
            TRC$(61, 0) = B$(I, 0)
            TRC$(61, 1) = SITE$(I)
            TRC$(61, 2) = B$(1 - I, 0)
            TRC$(61, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS COMBINED HIGH %
        If (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3)) > TREC(62, 0) Then
            TREC(62, 0) = (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3))
            TREC(62, 1) = V8(I, 2) / V8(I, 3)
            TREC(62, 2) = V8(1 - I, 2) / V8(1 - I, 3)
            TRC$(62, 0) = B$(I, 0)
            TRC$(62, 1) = SITE$(I)
            TRC$(62, 2) = B$(1 - I, 0)
            TRC$(62, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS COMBINED LOW %
        If (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3)) < TREC(63, 0) Then
            TREC(63, 0) = (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3))
            TREC(63, 1) = V8(I, 2) / V8(I, 3)
            TREC(63, 2) = V8(1 - I, 2) / V8(1 - I, 3)
            TRC$(63, 0) = B$(I, 0)
            TRC$(63, 1) = SITE$(I)
            TRC$(63, 2) = B$(1 - I, 0)
            TRC$(63, 3) = Date$
        End If

        ' TEAM FREE THROWS MADE
        If V8(I, 2) > TREC(64, 0) Then
            TREC(64, 0) = V8(I, 2)
            TREC(64, 1) = 0
            TREC(64, 2) = 0
            TRC$(64, 0) = B$(I, 0)
            TRC$(64, 1) = SITE$(I)
            TRC$(64, 2) = B$(1 - I, 0)
            TRC$(64, 3) = Date$
        End If

        If V8(I, 2) < TREC(65, 0) Then
            TREC(65, 0) = V8(I, 2)
            TREC(65, 1) = 0
            TREC(65, 2) = 0
            TRC$(65, 0) = B$(I, 0)
            TRC$(65, 1) = SITE$(I)
            TRC$(65, 2) = B$(1 - I, 0)
            TRC$(65, 3) = Date$
        End If

        If V8(I, 2) + V8(1 - I, 2) > TREC(66, 0) Then
            TREC(66, 0) = V8(I, 2) + V8(1 - I, 2)
            TREC(66, 1) = V8(I, 2)
            TREC(66, 2) = V8(1 - I, 2)
            TRC$(66, 0) = B$(I, 0)
            TRC$(66, 1) = SITE$(I)
            TRC$(66, 2) = B$(1 - I, 0)
            TRC$(66, 3) = Date$
        End If

        If V8(I, 2) + V8(1 - I, 2) < TREC(67, 0) Then
            TREC(67, 0) = V8(I, 2) + V8(1 - I, 2)
            TREC(67, 1) = V8(I, 2)
            TREC(67, 2) = V8(1 - I, 2)
            TRC$(67, 0) = B$(I, 0)
            TRC$(67, 1) = SITE$(I)
            TRC$(67, 2) = B$(1 - I, 0)
            TRC$(67, 3) = Date$
        End If

        If QR(I, 1, 2) + QR(I, 2, 2) > TREC(68, 0) Then
            TREC(68, 0) = QR(I, 1, 2) + QR(I, 2, 2)
            TREC(68, 1) = 1
            TREC(68, 2) = 0
            TRC$(68, 0) = B$(I, 0)
            TRC$(68, 1) = SITE$(I)
            TRC$(68, 2) = B$(1 - I, 0)
            TRC$(68, 3) = Date$
        End If

        If QR(I, 3, 2) + QR(I, 4, 2) > TREC(68, 0) Then
            TREC(68, 0) = QR(I, 3, 2) + QR(I, 4, 2)
            TREC(68, 1) = 2
            TREC(68, 2) = 0
            TRC$(68, 0) = B$(I, 0)
            TRC$(68, 1) = SITE$(I)
            TRC$(68, 2) = B$(1 - I, 0)
            TRC$(68, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 2) > TREC(70, 0) Then
                TREC(70, 0) = QR(I, I2, 2)
                TREC(70, 1) = I2
                TREC(70, 2) = 0
                TRC$(70, 0) = B$(I, 0)
                TRC$(70, 1) = SITE$(I)
                TRC$(70, 2) = B$(1 - I, 0)
                TRC$(70, 3) = Date$
            End If

        Next

        ' TEAM FREE THROWS ATTEMPTED
        If V8(I, 3) > TREC(72, 0) Then
            TREC(72, 0) = V8(I, 3)
            TREC(72, 1) = 0
            TREC(72, 2) = 0
            TRC$(72, 0) = B$(I, 0)
            TRC$(72, 1) = SITE$(I)
            TRC$(72, 2) = B$(1 - I, 0)
            TRC$(72, 3) = Date$
        End If

        If V8(I, 3) < TREC(73, 0) Then
            TREC(73, 0) = V8(I, 3)
            TREC(73, 1) = 0
            TREC(73, 2) = 0
            TRC$(73, 0) = B$(I, 0)
            TRC$(73, 1) = SITE$(I)
            TRC$(73, 2) = B$(1 - I, 0)
            TRC$(73, 3) = Date$
        End If

        If V8(I, 3) + V8(1 - I, 3) > TREC(74, 0) Then
            TREC(74, 0) = V8(I, 3) + V8(1 - I, 3)
            TREC(74, 1) = V8(I, 3)
            TREC(74, 2) = V8(1 - I, 3)
            TRC$(74, 0) = B$(I, 0)
            TRC$(74, 1) = SITE$(I)
            TRC$(74, 2) = B$(1 - I, 0)
            TRC$(74, 3) = Date$
        End If

        If V8(I, 3) + V8(1 - I, 3) < TREC(75, 0) Then
            TREC(75, 0) = V8(I, 3) + V8(1 - I, 3)
            TREC(75, 1) = V8(I, 3)
            TREC(75, 2) = V8(1 - I, 3)
            TRC$(75, 0) = B$(I, 0)
            TRC$(75, 1) = SITE$(I)
            TRC$(75, 2) = B$(1 - I, 0)
            TRC$(75, 3) = Date$
        End If

        If QR(I, 1, 3) + QR(I, 2, 3) > TREC(76, 0) Then
            TREC(76, 0) = QR(I, 1, 3) + QR(I, 2, 3)
            TREC(76, 1) = 1
            TREC(76, 2) = 0
            TRC$(76, 0) = B$(I, 0)
            TRC$(76, 1) = SITE$(I)
            TRC$(76, 2) = B$(1 - I, 0)
            TRC$(76, 3) = Date$
        End If

        If QR(I, 3, 3) + QR(I, 4, 3) > TREC(76, 0) Then
            TREC(76, 0) = QR(I, 3, 3) + QR(I, 4, 3)
            TREC(76, 1) = 2
            TREC(76, 2) = 0
            TRC$(76, 0) = B$(I, 0)
            TRC$(76, 1) = SITE$(I)
            TRC$(76, 2) = B$(1 - I, 0)
            TRC$(76, 3) = Date$
        End If

        If QR(I, 1, 3) + QR(I, 2, 3) + QR(1 - I, 1, 3) + QR(1 - I, 2, 3) > TREC(77, 0) Then
            TREC(77, 0) = QR(I, 1, 3) + QR(I, 2, 3) + QR(1 - I, 1, 3) + QR(1 - I, 2, 3)
            TREC(77, 1) = 1
            TREC(77, 2) = QR(I, 1, 3) + QR(I, 2, 3)
            TRC$(77, 0) = B$(I, 0)
            TRC$(77, 1) = SITE$(I)
            TRC$(77, 2) = B$(1 - I, 0)
            TRC$(77, 3) = Date$
        End If

        If QR(I, 3, 3) + QR(I, 4, 3) + QR(1 - I, 3, 3) + QR(1 - I, 4, 3) > TREC(77, 0) Then
            TREC(77, 0) = QR(I, 3, 3) + QR(I, 4, 3) + QR(1 - I, 3, 3) + QR(1 - I, 4, 3)
            TREC(77, 1) = 2
            TREC(77, 2) = QR(I, 3, 3) + QR(I, 4, 3)
            TRC$(77, 0) = B$(I, 0)
            TRC$(77, 1) = SITE$(I)
            TRC$(77, 2) = B$(1 - I, 0)
            TRC$(77, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 3) > TREC(78, 0) Then
                TREC(78, 0) = QR(I, I2, 3)
                TREC(78, 1) = I2
                TREC(78, 2) = 0
                TRC$(78, 0) = B$(I, 0)
                TRC$(78, 1) = SITE$(I)
                TRC$(78, 2) = B$(1 - I, 0)
                TRC$(78, 3) = Date$
            End If

            If QR(I, I2, 3) + QR(1 - I, I2, 3) > TREC(79, 0) Then
                TREC(79, 0) = QR(I, I2, 3) + QR(1 - I, I2, 3)
                TREC(79, 1) = I2
                TREC(79, 2) = QR(I, I2, 3)
                TRC$(79, 0) = B$(I, 0)
                TRC$(79, 1) = SITE$(I)
                TRC$(79, 2) = B$(1 - I, 0)
                TRC$(79, 3) = Date$
            End If
        Next

        If R3(I) > TREC(80, 0) Then
            TREC(80, 0) = R3(I)
            TREC(80, 1) = 0
            TREC(80, 2) = 0
            TRC$(80, 0) = B$(I, 0)
            TRC$(80, 1) = SITE$(I)
            TRC$(80, 2) = B$(1 - I, 0)
            TRC$(80, 3) = Date$
        End If

        If R3(I) < TREC(81, 0) Then
            TREC(81, 0) = R3(I)
            TREC(81, 1) = 0
            TREC(81, 2) = 0
            TRC$(81, 0) = B$(I, 0)
            TRC$(81, 1) = SITE$(I)
            TRC$(81, 2) = B$(1 - I, 0)
            TRC$(81, 3) = Date$
        End If

        If R3(I) + R3(1 - I) > TREC(82, 0) Then
            TREC(82, 0) = R3(I) + R3(1 - I)
            TREC(82, 1) = R3(I)
            TREC(82, 2) = R3(1 - I)
            TRC$(82, 0) = B$(I, 0)
            TRC$(82, 1) = SITE$(I)
            TRC$(82, 2) = B$(1 - I, 0)
            TRC$(82, 3) = Date$
        End If

        If R3(I) + R3(1 - I) < TREC(83, 0) Then
            TREC(83, 0) = R3(I) + R3(1 - I)
            TREC(83, 1) = R3(I)
            TREC(83, 2) = R3(1 - I)
            TRC$(83, 0) = B$(I, 0)
            TRC$(83, 1) = SITE$(I)
            TRC$(83, 2) = B$(1 - I, 0)
            TRC$(83, 3) = Date$
        End If

        If QR(I, 1, 4) + QR(I, 2, 4) > TREC(84, 0) Then
            TREC(84, 0) = QR(I, 1, 4) + QR(I, 2, 4)
            TREC(84, 1) = 1
            TREC(84, 2) = QR(I, 1, 4) + QR(I, 2, 4)
            TRC$(84, 0) = B$(I, 0)
            TRC$(84, 1) = SITE$(I)
            TRC$(84, 2) = B$(1 - I, 0)
            TRC$(84, 3) = Date$
        End If

        If QR(I, 3, 4) + QR(I, 4, 4) > TREC(84, 0) Then
            TREC(84, 0) = QR(I, 3, 4) + QR(I, 4, 4)
            TREC(84, 1) = 2
            TREC(84, 2) = QR(I, 3, 4) + QR(I, 4, 4)
            TRC$(84, 0) = B$(I, 0)
            TRC$(84, 1) = SITE$(I)
            TRC$(84, 2) = B$(1 - I, 0)
            TRC$(84, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 4) > TREC(85, 0) Then
                TREC(85, 0) = QR(I, I2, 4)
                TREC(85, 1) = I2
                TREC(85, 2) = 0
                TRC$(85, 0) = B$(I, 0)
                TRC$(85, 1) = SITE$(I)
                TRC$(85, 2) = B$(1 - I, 0)
                TRC$(85, 3) = Date$
            End If
        Next

        If V8(I, 11) > TREC(86, 0) Then
            TREC(86, 0) = V8(I, 11)
            TREC(86, 1) = 0
            TREC(86, 2) = 0
            TRC$(86, 0) = B$(I, 0)
            TRC$(86, 1) = SITE$(I)
            TRC$(86, 2) = B$(1 - I, 0)
            TRC$(86, 3) = Date$
        End If

        If V8(I, 11) + V8(1 - I, 11) > TREC(88, 0) Then
            TREC(88, 0) = V8(I, 11) + V8(1 - I, 11)
            TREC(88, 1) = V8(I, 11)
            TREC(88, 2) = V8(1 - I, 11)
            TRC$(88, 0) = B$(I, 0)
            TRC$(88, 1) = SITE$(I)
            TRC$(88, 2) = B$(1 - I, 0)
            TRC$(88, 3) = Date$
        End If

        If V8(I, 11) + V8(1 - I, 11) < TREC(89, 0) Then
            TREC(89, 0) = V8(I, 11) + V8(1 - I, 11)
            TREC(89, 1) = V8(I, 11)
            TREC(89, 2) = V8(1 - I, 11)
            TRC$(89, 0) = B$(I, 0)
            TRC$(89, 1) = SITE$(I)
            TRC$(89, 2) = B$(1 - I, 0)
            TRC$(89, 3) = Date$
        End If

        If QR(I, 1, 11) + QR(I, 2, 11) > TREC(90, 0) Then
            TREC(90, 0) = QR(I, 1, 11) + QR(I, 2, 11)
            TREC(90, 1) = 1
            TREC(90, 2) = QR(I, 1, 11) + QR(I, 2, 11)
            TRC$(90, 0) = B$(I, 0)
            TRC$(90, 1) = SITE$(I)
            TRC$(90, 2) = B$(1 - I, 0)
            TRC$(90, 3) = Date$
        End If

        If QR(I, 3, 11) + QR(I, 4, 11) > TREC(90, 0) Then
            TREC(90, 0) = QR(I, 3, 11) + QR(I, 4, 11)
            TREC(90, 1) = 2
            TREC(90, 2) = QR(I, 3, 11) + QR(I, 4, 11)
            TRC$(90, 0) = B$(I, 0)
            TRC$(90, 1) = SITE$(I)
            TRC$(90, 2) = B$(1 - I, 0)
            TRC$(90, 3) = Date$
        End If

        ' ASSISTS
        If V8(I, 7) > TREC(96, 0) Then
            TREC(96, 0) = V8(I, 7)
            TREC(96, 1) = 0
            TREC(96, 2) = 0
            TRC$(96, 0) = B$(I, 0)
            TRC$(96, 1) = SITE$(I)
            TRC$(96, 2) = B$(1 - I, 0)
            TRC$(96, 3) = Date$
        End If

        If V8(I, 7) < TREC(97, 0) Then
            TREC(97, 0) = V8(I, 7)
            TREC(97, 1) = 0
            TREC(97, 2) = 0
            TRC$(97, 0) = B$(I, 0)
            TRC$(97, 1) = SITE$(I)
            TRC$(97, 2) = B$(1 - I, 0)
            TRC$(97, 3) = Date$
        End If

        If V8(I, 7) + V8(1 - I, 7) > TREC(98, 0) Then
            TREC(98, 0) = V8(I, 7) + V8(1 - I, 7)
            TREC(98, 1) = V8(I, 7)
            TREC(98, 2) = V8(1 - I, 7)
            TRC$(98, 0) = B$(I, 0)
            TRC$(98, 1) = SITE$(I)
            TRC$(98, 2) = B$(1 - I, 0)
            TRC$(98, 3) = Date$
        End If

        If V8(I, 7) + V8(1 - I, 7) < TREC(99, 0) Then
            TREC(99, 0) = V8(I, 7) + V8(1 - I, 7)
            TREC(99, 1) = V8(I, 7)
            TREC(99, 2) = V8(1 - I, 7)
            TRC$(99, 0) = B$(I, 0)
            TRC$(99, 1) = SITE$(I)
            TRC$(99, 2) = B$(1 - I, 0)
            TRC$(99, 3) = Date$
        End If

        If QR(I, 1, 7) + QR(I, 2, 7) > TREC(100, 0) Then
            TREC(100, 0) = QR(I, 1, 7) + QR(I, 2, 7)
            TREC(100, 1) = 1
            TREC(100, 2) = 0
            TRC$(100, 0) = B$(I, 0)
            TRC$(100, 1) = SITE$(I)
            TRC$(100, 2) = B$(1 - I, 0)
            TRC$(100, 3) = Date$
        End If

        If QR(I, 3, 7) + QR(I, 4, 7) > TREC(100, 0) Then
            TREC(100, 0) = QR(I, 3, 7) + QR(I, 4, 7)
            TREC(100, 1) = 2
            TREC(100, 2) = 0
            TRC$(100, 0) = B$(I, 0)
            TRC$(100, 1) = SITE$(I)
            TRC$(100, 2) = B$(1 - I, 0)
            TRC$(100, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 7) > TREC(102, 0) Then
                TREC(102, 0) = QR(I, I2, 7)
                TREC(102, 1) = I2
                TREC(102, 2) = 0
                TRC$(102, 0) = B$(I, 0)
                TRC$(102, 1) = SITE$(I)
                TRC$(102, 2) = B$(1 - I, 0)
                TRC$(102, 3) = Date$
            End If
        Next

        ' FOULS
        If V8(I, 6) > TREC(104, 0) Then
            TREC(104, 0) = V8(I, 6)
            TREC(104, 1) = 0
            TREC(104, 2) = 0
            TRC$(104, 0) = B$(I, 0)
            TRC$(104, 1) = SITE$(I)
            TRC$(104, 2) = B$(1 - I, 0)
            TRC$(104, 3) = Date$
        End If

        If V8(I, 6) < TREC(105, 0) Then
            TREC(105, 0) = V8(I, 6)
            TREC(105, 1) = 0
            TREC(105, 2) = 0
            TRC$(105, 0) = B$(I, 0)
            TRC$(105, 1) = SITE$(I)
            TRC$(105, 2) = B$(1 - I, 0)
            TRC$(105, 3) = Date$
        End If

        If V8(I, 6) + V8(1 - I, 6) > TREC(106, 0) Then
            TREC(106, 0) = V8(I, 6) + V8(1 - I, 6)
            TREC(106, 1) = V8(I, 6)
            TREC(106, 2) = V8(1 - I, 6)
            TRC$(106, 0) = B$(I, 0)
            TRC$(106, 1) = SITE$(I)
            TRC$(106, 2) = B$(1 - I, 0)
            TRC$(106, 3) = Date$
        End If

        If V8(I, 6) + V8(1 - I, 6) < TREC(107, 0) Then
            TREC(107, 0) = V8(I, 6) + V8(1 - I, 6)
            TREC(107, 1) = V8(I, 6)
            TREC(107, 2) = V8(1 - I, 6)
            TRC$(107, 0) = B$(I, 0)
            TRC$(107, 1) = SITE$(I)
            TRC$(107, 2) = B$(1 - I, 0)
            TRC$(107, 3) = Date$
        End If

        If QR(I, 1, 6) + QR(I, 2, 6) > TREC(108, 0) Then
            TREC(108, 0) = QR(I, 1, 6) + QR(I, 2, 6)
            TREC(108, 1) = 1
            TREC(108, 2) = 0
            TRC$(108, 0) = B$(I, 0)
            TRC$(108, 1) = SITE$(I)
            TRC$(108, 2) = B$(1 - I, 0)
            TRC$(108, 3) = Date$
        End If

        If QR(I, 3, 6) + QR(I, 4, 6) > TREC(108, 0) Then
            TREC(108, 0) = QR(I, 3, 6) + QR(I, 4, 6)
            TREC(108, 1) = 2
            TREC(108, 2) = 0
            TRC$(108, 0) = B$(I, 0)
            TRC$(108, 1) = SITE$(I)
            TRC$(108, 2) = B$(1 - I, 0)
            TRC$(108, 3) = Date$
        End If

        If QR(I, 1, 6) + QR(I, 2, 6) + QR(1 - I, 1, 6) + QR(1 - I, 2, 6) > TREC(109, 0) Then
            TREC(109, 0) = QR(I, 1, 6) + QR(I, 2, 6) + QR(1 - I, 1, 6) + QR(1 - I, 2, 6)
            TREC(109, 1) = 1
            TREC(109, 2) = QR(I, 1, 6) + QR(I, 2, 6)
            TRC$(109, 0) = B$(I, 0)
            TRC$(109, 1) = SITE$(I)
            TRC$(109, 2) = B$(1 - I, 0)
            TRC$(109, 3) = Date$
        End If

        If QR(I, 3, 6) + QR(I, 4, 6) + QR(1 - I, 3, 6) + QR(1 - I, 4, 6) > TREC(109, 0) Then
            TREC(109, 0) = QR(I, 3, 6) + QR(I, 4, 6) + QR(1 - I, 3, 6) + QR(1 - I, 4, 6)
            TREC(109, 1) = 2
            TREC(109, 2) = QR(I, 3, 6) + QR(I, 4, 6)
            TRC$(109, 0) = B$(I, 0)
            TRC$(109, 1) = SITE$(I)
            TRC$(109, 2) = B$(1 - I, 0)
            TRC$(109, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 6) > TREC(110, 0) Then
                TREC(110, 0) = QR(I, I2, 6)
                TREC(110, 1) = I2
                TREC(110, 2) = 0
                TRC$(110, 0) = B$(I, 0)
                TRC$(110, 1) = SITE$(I)
                TRC$(110, 2) = B$(1 - I, 0)
                TRC$(110, 3) = Date$
            End If

            If QR(I, I2, 6) + QR(1 - I, I2, 6) > TREC(111, 0) Then
                TREC(111, 0) = QR(I, I2, 6) + QR(1 - I, I2, 6)
                TREC(111, 1) = I2
                TREC(111, 2) = QR(I, I2, 6)
                TRC$(111, 0) = B$(I, 0)
                TRC$(111, 1) = SITE$(I)
                TRC$(111, 2) = B$(1 - I, 0)
                TRC$(111, 3) = Date$
            End If
        Next

        ' STEALS
        If V8(I, 9) > TREC(112, 0) Then
            TREC(112, 0) = V8(I, 9)
            TREC(112, 1) = 0
            TREC(112, 2) = 0
            TRC$(112, 0) = B$(I, 0)
            TRC$(112, 1) = SITE$(I)
            TRC$(112, 2) = B$(1 - I, 0)
            TRC$(112, 3) = Date$
        End If

        If V8(I, 9) + V8(1 - I, 9) > TREC(114, 0) Then
            TREC(114, 0) = V8(I, 9) + V8(1 - I, 9)
            TREC(114, 1) = V8(I, 9)
            TREC(114, 2) = V8(1 - I, 9)
            TRC$(114, 0) = B$(I, 0)
            TRC$(114, 1) = SITE$(I)
            TRC$(114, 2) = B$(1 - I, 0)
            TRC$(114, 3) = Date$
        End If

        If QR(I, 1, 9) + QR(I, 2, 9) > TREC(116, 0) Then
            TREC(116, 0) = QR(I, 1, 9) + QR(I, 2, 9)
            TREC(116, 1) = 1
            TREC(116, 2) = 0
            TRC$(116, 0) = B$(I, 0)
            TRC$(116, 1) = SITE$(I)
            TRC$(116, 2) = B$(1 - I, 0)
            TRC$(116, 3) = Date$
        End If

        If QR(I, 3, 9) + QR(I, 4, 9) > TREC(116, 0) Then
            TREC(116, 0) = QR(I, 3, 9) + QR(I, 4, 9)
            TREC(116, 1) = 2
            TREC(116, 2) = 0
            TRC$(116, 0) = B$(I, 0)
            TRC$(116, 1) = SITE$(I)
            TRC$(116, 2) = B$(1 - I, 0)
            TRC$(116, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 9) > TREC(117, 0) Then
                TREC(117, 0) = QR(I, I2, 9)
                TREC(117, 1) = I2
                TREC(117, 2) = 0
                TRC$(117, 0) = B$(I, 0)
                TRC$(117, 1) = SITE$(I)
                TRC$(117, 2) = B$(1 - I, 0)
                TRC$(117, 3) = Date$
            End If
        Next

        ' BLOCKS
        If V8(I, 8) > TREC(118, 0) Then
            TREC(118, 0) = V8(I, 8)
            TREC(118, 1) = 0
            TREC(118, 2) = 0
            TRC$(118, 0) = B$(I, 0)
            TRC$(118, 1) = SITE$(I)
            TRC$(118, 2) = B$(1 - I, 0)
            TRC$(118, 3) = Date$
        End If

        If V8(I, 8) + V8(1 - I, 8) > TREC(119, 0) Then
            TREC(119, 0) = V8(I, 8) + V8(1 - I, 8)
            TREC(119, 1) = V8(I, 8)
            TREC(119, 2) = V8(1 - I, 8)
            TRC$(119, 0) = B$(I, 0)
            TRC$(119, 1) = SITE$(I)
            TRC$(119, 2) = B$(1 - I, 0)
            TRC$(119, 3) = Date$
        End If

        ' TURNOVERS
        If V8(I, 12) > TREC(122, 0) Then
            TREC(122, 0) = V8(I, 12)
            TREC(122, 1) = 0
            TREC(122, 2) = 0
            TRC$(122, 0) = B$(I, 0)
            TRC$(122, 1) = SITE$(I)
            TRC$(122, 2) = B$(1 - I, 0)
            TRC$(122, 3) = Date$
        End If

        If V8(I, 12) < TREC(123, 0) Then
            TREC(123, 0) = V8(I, 12)
            TREC(123, 1) = 0
            TREC(123, 2) = 0
            TRC$(123, 0) = B$(I, 0)
            TRC$(123, 1) = SITE$(I)
            TRC$(123, 2) = B$(1 - I, 0)
            TRC$(123, 3) = Date$
        End If
        If V8(I, 12) + V8(1 - I, 12) > TREC(124, 0) Then
            TREC(124, 0) = V8(I, 12) + V8(1 - I, 12)
            TREC(124, 1) = V8(I, 12)
            TREC(124, 2) = V8(1 - I, 12)
            TRC$(124, 0) = B$(I, 0)
            TRC$(124, 1) = SITE$(I)
            TRC$(124, 2) = B$(1 - I, 0)
            TRC$(124, 3) = Date$
        End If

        If V8(I, 12) + V8(1 - I, 12) < TREC(125, 0) Then
            TREC(125, 0) = V8(I, 12) + V8(1 - I, 12)
            TREC(125, 1) = V8(I, 12)
            TREC(125, 2) = V8(1 - I, 12)
            TRC$(125, 0) = B$(I, 0)
            TRC$(125, 1) = SITE$(I)
            TRC$(125, 2) = B$(1 - I, 0)
            TRC$(125, 3) = Date$
        End If

    Next

    Open P$(1) + "RECORDS.GAM" For Output As #1
    For I = 0 To 48
        For I1 = 0 To 2
            Write #1, REC(I, I1)
        Next
        For I1 = 0 To 4
            Write #1, RC$(I, I1)
        Next
    Next
    For I = 0 To 125
        For I1 = 0 To 2
            Write #1, TREC(I, I1)
        Next
        For I1 = 0 To 3
            Write #1, TRC$(I, I1)
        Next
    Next
    Close 1

End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
