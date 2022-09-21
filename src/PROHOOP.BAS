'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
'$INCLUDE: 'PadRight.BI'
'$INCLUDE: 'GameRoutines.BI'

_Title "Basketball: The Pro Game - HOOPS!"
$ExeIcon:'./lhg_probb.ico'
_Icon
_AllowFullScreen

Color 7, 0
Cls

Randomize Timer

'----------------------------------------
'    INITIALIZE ALL VARIABLES
'----------------------------------------

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
Input #1, diskPaths$(0)
Input #1, diskPaths$(1)
Close 1

Open "DEFAULT" For Input As #1
Input #1, U5
Input #1, XY%
Input #1, SC
Input #1, N
Input #1, PO
Input #1, pbpDelay
Input #1, X4%
Input #1, CM%
Input #1, NB%
Input #1, RT%
Input #1, FR%
Input #1, PB%
Close 1

If _FileExists("TICKER") Then

    Open "TICKER" For Input As #2

    For I = 1 To 32
        For I1 = 0 To 1
            For I2 = 1 To 14
                Input #2, SX%(I, I1, I2)
            Next
        Next
    Next

    For I = 1 To 32
        For I1 = 0 To 1
            Input #2, SX$(I, I1)
        Next
    Next

    For I = 1 To 32
        Input #2, ST%(I)
    Next

    Close 2

    TMT = 1
Else
    TMT = 0
End If

If _FileExists("GAME82") Then

    Open "GAME82" For Input As #2
    Input #2, FF$
    For I = 0 To 2: Input #2, AP%(I): Next
    Close 2

    Open diskPaths$(0) + FF$ For Random As #1 Len = 48
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
End If

P7$(1) = "YES"
P7$(0) = " NO"

T = 720
H = 1

T1(1) = 7
T1(0) = 7

PS$(0) = "G"
PS$(1) = "G"
PS$(2) = "F"
PS$(3) = "F"
PS$(4) = "C"


OY%(1) = 2
OY%(2) = -2
OX%(1) = 10
OX%(2) = -10
D2(0) = 1
D2(1) = 1

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
offenseStyles$(0) = "NORMAL      "
offenseStyles$(1) = "AGGRESSIVE  "
offenseStyles$(2) = "SAFE        "
U5$(0) = "HUMAN OPPONENT           "
U5$(1) = "COMPUTER IS VISITING TEAM"
U5$(2) = "COMPUTER IS HOME TEAM    "
U5$(3) = "COMPUTER VS COMPUTER     "
LC$(0) = "@ TEAM 1            "
LC$(1) = "HOME COURT ADVANTAGE"
LC$(2) = "NEUTRAL SITE        "
X$(0) = "A "
X$(1) = "B "
X$(2) = "C "
X$(3) = "D "
X$(4) = "E "
PO$(0) = "REGULAR SEASON"
PO$(1) = "PLAYOFF       "
CM$(0) = "COACH AND SELECT SHOTS           "
CM$(1) = "COACH ONLY AND LET COMPUTER SHOOT"
PB$(0) = "VERBOSE        "
PB$(1) = "HAFFNER CLASSIC"


'----------------------------------------
'    USER SELECT GAME OPTIONS
'----------------------------------------

If AP = 1 Then

    XY% = NG%(3)
    U5 = NG%(8)
    PO = NG%(5)
    SC = NG%(7)
    N = NG%(2)
    pbpDelay = NG%(4) / 10
    CM% = NG%(9)
    NB% = NG%(14): FR% = NG%(11): RT% = NG%(10)
    For P9 = 0 To 1
        I1 = NG%(P9)
        YN$ = YN$(P9)
        Call GETATEAM(I1, YN$)
    Next P9

    Call SETUPRATINGS

Else


    Do
        Cls

        Color 2: Print Tab(29); "SELECT OPENING OPTIONS"
        Print
        Locate 2, 2: Color 14: Print "(1) ";: Color 15: Print U5$(U5)
        Locate 3, 2: Color 14: Print "(2) ";: Color 15: Print "VISITING TEAM: ";: Color T(0, 33), T(0, 34): Print B$(0, 0)
        Color , 0
        Locate 4, 2: Color 14: Print "(3) ";: Color 15: Print "HOME TEAM: ";: Color T(1, 33), T(1, 34): Print B$(1, 0)
        Color , 0
        Locate 5, 2: Color 14: Print "(4) ";: Color 15: Print "3-POINT SHOT: "; TS$(XY%)
        Locate 6, 2: Color 14: Print "(5) ";: Color 15: Print SC$(SC)
        Locate 7, 2: Color 14: Print "(6) ";: Color 15: Print "LOCATION: "; LC$(N)
        Locate 8, 2: Color 14: Print "(7) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#"; pbpDelay
        Locate 9, 2: Color 14: Print "(8) ";: Color 15: Print "START GAME"
        Locate 10, 2: Color 14: Print "(9) ";: Color 15: Print PO$(PO)
        Locate 11, 2: Color 14: Print "(0) ";: Color 15: Print "SAVE SETTINGS"
        Locate 12, 2: Color 14: Print "(A) ";: Color 15: Print "SHOW SHOT PERCENTAGES: "; P7$(X4%)
        Locate 13, 2: Color 14: Print "(B) ";: Color 15: Print "HUMAN COACHES WILL: "; CM$(CM%);
        Locate 1, 1
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

        Do
            Color 7

            I$ = GetKeyPress$

            If I$ <> Chr$(27) Then
                I = Val(I$)
                If UCase$(I$) = "A" Then I = 11
                If UCase$(I$) = "B" Then I = 12
                If UCase$(I$) = "C" Then I = 13
                If UCase$(I$) = "D" Then I = 14
                If UCase$(I$) = "E" Then I = 15
                If UCase$(I$) = "F" Then I = 16
                If I = 0 Then I = 10
            End If

        Loop Until I >= 0 And I <= 16 Or I$ = Chr$(27)

        If I$ <> Chr$(27) Then

            Select Case I
                Case 1:
                    U5 = U5 + 1
                    If U5 > 3 Then U5 = 0

                Case 2:
                    P9 = 0
                    Call TeamSelect_PROHOOP(P9)

                Case 3:
                    P9 = 1
                    Call TeamSelect_PROHOOP(P9)

                Case 4:
                    XY% = XY% + 1
                    If XY% > 1 Then XY% = 0

                Case 5:
                    SC = 1 - SC

                Case 6:
                    N = N + 1
                    If N > 2 Then N = 1

                Case 7:
                    Do
                        Locate 8, 32
                        Input pbpDelay
                    Loop Until pbpDelay >= 0

                Case 8:
                    If B$(0, 0) <> "" And B$(1, 0) <> "" Then Exit Do

                Case 9:
                    PO = 1 - PO

                Case 10:
                    Open "DEFAULT" For Output As #1
                    Print #1, U5
                    Print #1, XY%
                    Print #1, SC
                    Print #1, N
                    Print #1, PO
                    Print #1, pbpDelay
                    Print #1, X4%
                    Print #1, CM%
                    Print #1, NB%
                    Print #1, RT%
                    Print #1, FR%
                    Print #1, PB%
                    Close 1

                Case 11:
                    X4% = 1 - X4%

                Case 12:
                    CM% = 1 - CM%

                Case 13:
                    NB% = NB% + 1
                    If NB% > 6 Then NB% = 4

                Case 14:
                    RT% = 1 - RT%

                Case 15:
                    FR% = 1 - FR%

                Case 16:
                    PB% = 1 - PB%

            End Select

        End If

    Loop Until (I = 8 And B$(0, 0) <> "" And B$(1, 0) <> "") Or I$ = Chr$(27)

    If I$ <> Chr$(27) Then

        Cls
        Color 3

        JJ = 6
        Call FRAMEBOX
        Locate 10, 11
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

    End If

End If

'----------------------------------------
'    AUTO-DETERMINE OTHER OPTIONS
'----------------------------------------

' Determine computer team
If U5 = 1 Then
    CT = 0
Else
    If U5 = 2 Then
        U5 = 1
        CT = 1
    Else
        If U5 = 3 Then U5 = 2
    End If
End If


' Determine stat files
62 For I = 0 To 1
    Cls
    Y$(I) = B$(I, 0)
    Q = I

    For XX = 4 To 11:
        If Mid$(B$(Q, 0), XX, 1) = " " Or Mid$(B$(Q, 0), XX, 1) = "." Then Mid$(B$(Q, 0), XX, 1) = "_"
    Next XX
    For XX = 12 To 14
        If Mid$(B$(Q, 0), XX, 2) = "ST" Then Mid$(B$(Q, 0), 9, 1) = "_": Mid$(B$(Q, 0), 10, 2) = "ST"
    Next XX
    QQ = 8
    For XX = 11 To 4 Step -1
        If Mid$(B$(Q, 0), XX, 1) = "_" Then
            QQ = XX - 4
        Else
            Exit For
        End If
    Next XX

    M$ = Mid$(B$(I, 0), 4, QQ)
    R$ = M$ + "." + Left$(B$(I, 0), 2)
    R$ = R$ + "S"

    If _FileExists(diskPaths$(1) + R$) Then
        Open diskPaths$(1) + R$ For Input As #2
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
    End If

    B$(I, 0) = Y$(I)
Next


If AP = 1 Then N = NG%(2)

X8 = SC(SC)
C7 = 1
LS = 0

If T < 6 And LT = 4 Then LS = 4

'Determine subs / play parameters
For P9 = 0 To 1
    If U5 = 1 And P9 = CT Or U5 = 2 Then
        Call COMPUTERSUBS
    Else
        Call PlayStyle
        Call PlayItSafe
    End If
Next P9

Call RATINGTABLES

Call CheckZeroTime

Call RATINGTABLES


250 '

'850
If H = 2 Then
    If JB = 0 Then
        JB = 1
    Else
        If JB = 1 Then JB = 0
    End If
End If

If H = 4 Then
    If JB = 0 Then
        JB = 1
    Else
        If H = 4 And JB = 1 Then JB = 0
    End If
End If


'251
If Not (H < 1 And H > 5) Then
    P = Int(Rnd(1) * 2)
    JB = P
End If

'252
If P = 0 Then D = 1
If P = 1 Then D = 0


PB$ = ""
If P = 1 Then
    P = 0: D = 1: P9 = 0: C1 = 2
Else
    'P = 0;
    P = 1: D = 0: P9 = 1: C1 = 2
End If

Call SomethingTime

If T <= 0 Then TE = T: Call AddMinutes: T = 0: GoTo 170

X8 = SC(SC): LS = 4

Call NEWSCREEN

'----------------------------------------
'           Tip Off!
'----------------------------------------

If H = 1 Or H > 4 Then
    If PB% = 0 Then U$ = B$(JB, 0) + " GETS THE JUMP": Call DELAY
Else
    If PB% = 0 Then U$ = B$(JB, 0) + " INBOUNDS THE BALL": Call DELAY
End If


If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Color T(JB, 33), T(JB, 34): Print B$(JB, 0);: Color 15, 0: Print U$: Call MOREDELAY

P9 = JB
P = JB

GoTo 300


'----------

170 T = 0
TF(0) = TF(0) + P2(0)
TF(1) = TF(1) + P2(1)
Call QUARTERTOTAL
H = H + 1

275 '*** ADJUST TIMEOUTS ***
If H >= 4 Then
    For PG = 0 To 1
        If T1(PG) > 4 Then T1(PG) = 4
        If H > 4 Then T1(PG) = 3
    Next
End If

XS = 1
P2(0) = 0
P2(1) = 0
X = 0

U$ = "END OF QUARTER!"
Select Case PB%
    Case 0: Color 7, 0: Call WINDEX: Print U$
    Case 1: If pbpDelay > 0 Then Call WINDEX: Print U$ ': Call MOREDELAY
End Select

178 '
If H = 3 Then
    U$ = "HALFTIME!"
    Select Case PB%
        Case 0: Color 7, 0: Call WINDEX: Print U$
        Case 1: Call WINDEX: Print U$: Call MOREDELAY
    End Select
    Call BoxToScreen
End If


If H < 5 And U5 <> 0 Then
    175 '
    If U5 = 1 Then P9 = CT: Call COMPUTERSUBS
    If U5 = 2 Then P9 = 0: Call COMPUTERSUBS: P9 = 1: Call COMPUTERSUBS
End If


If H < 5 Then

    Call CheckZeroTime

    '--- This is where the box gets drawn
    ' for periods 2+
    Call NEWSCREEN

    B = -1
    Call Substitutions
    T = 720
    Call UPDATESCREEN

    GoTo 250

End If

Call CheckZeroTime

Call NEWSCREEN

If H = 5 Then
    U$ = "END OF REGULATION"
    Select Case PB%
        Case 0: Color 7, 0: Call WINDEX: Print U$
        Case 1: If pbpDelay > 0 Then Call WINDEX: Print U$ ': Call MOREDELAY
    End Select
End If

Locate 2, 31: Print "END OF REGULATION"

If S(0, 0) = S(1, 0) Then
    U$ = "OVERTIME !!!"
    Select Case PB%
        Case 0: Color 7, 0: Call WINDEX: Print U$
        Case 1: Call WINDEX: Print U$: Call MOREDELAY
    End Select
    T1(0) = T1(0) + 1
    T1(1) = T1(1) + 1
    T = 300
    ZF(0) = 0
    ZF(1) = 0

    Call UPDATESCREEN
    B = -1

    Call Substitutions

    GoTo 250

End If



If Not (AP = 1 And U5 = 2) Then
    I$ = GetKeyPress
End If


8400 'CHECK SPECIAL EVENTS
Call RECORDSCHECK
Call EVENTS(Count, SPECIAL)

If SPECIAL = 1 Then
    NM$ = "BOX" + Mid$(Str$(Count), 2, Len(Str$(Count))) + ".SPC"
    Call BOX2SPC(NM$)
End If

Do

    Cls
    Color 14: Print "(0) ";: Color 15: Print "STATS TO SCREEN": Print
    Color 14: Print "(1) ";: Color 15: Print "STATS TO PRINTER": Print
    Color 14: Print "(2) ";: Color 15: Print "STATS TO TEXT FILE"

    If AP = 1 And U5 = 2 Then
        X = 2
    Else
        I$ = GetKeyPress$
        X = Val(I$)
    End If

    Select Case X

        Case 1: Call BOX2PRN
        Case 2: Call BOX2FILE
        Case 0:
            Cls
            If N <> 2 Then Color T(N, 33), T(N, 34): Print "@ "; B$(N, 4): Print: Color 14, 0

            For I = 0 To 1: Print B$(I, 0); Tab(17); Str$(S(I, 1)); Tab(20); Str$(S(I, 2)); Tab(23); Str$(S(I, 3)); Tab(26); Str$(S(I, 4));

                If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then
                    Print Tab(29); Str$(S(I, 5));
                End If

                If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) Then
                    Print Tab(29); "- "; S(I, 0)
                Else

                    If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) = S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then
                        Print Tab(32); Str$(S(I, 6));
                    End If

                    If S(0, 1) + S(0, 2) + S(0, 3) + S(0, 4) + S(0, 5) <> S(1, 1) + S(1, 2) + S(1, 3) + S(1, 4) + S(1, 5) Then
                        Print Tab(32); "- "; S(I, 0)
                    Else
                        Print Tab(35); "- "; S(I, 0)
                    End If
                End If

            Next I

            Call BoxToScreen

    End Select

    8532 X = 0
    Cls

    JJ = 1
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

    Locate 9, 26
    Print "DISPLAY STATS AGAIN (YN)";
    Color 15, 0

    If AP <> 1 Or U5 <> 2 Then

        Do
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    End If

Loop Until UCase$(I$) = "N" Or (AP = 1 And U5 = 2)

Call COMPILESTATS

Cls

JJ = 1
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

Locate 9, 30
Print "ANOTHER GAME (YN)";
Color 15, 0

If AP <> 1 Then

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If UCase$(I$) = "N" Then End

End If

Cls
Print "PLEASE WAIT ..."
Run "PROHOOP.EXE"

'----------------------------------------
'           END OF GAME!!!
'----------------------------------------





'================================================================================
'================================================================================
'================================================================================
'================================================================================
'================================================================================


300 '
ST = 0
FO = 0
LS = 0
XS = 0

If X8 > 0 And X8 < SC(SC) Then

    X8 = X8 - 5

Else

    X8 = SC(SC)
    LS = 0

    If P = 0 Then D = 1
    If P = 1 Then D = 0
    If T < 6 And LT <> 4 Then LS = 4

End If

D6 = 0
PASS% = 1
A4 = 0
A6 = 0
F = 0

If X <> 90 Then X = 0

GoTo 400


'----------


350 PASS% = PASS% + 1
P6 = 0
A4 = 0
D6 = 0
A6 = 1
F = 0
X = 0

GoTo 400


'----------


359 S = Int(Rnd(1) * 100) + 1
W%(P, B1(P, B), 1) = W%(P, B1(P, B), 1) + 1
A(P, B1(P, B), 7) = A(P, B1(P, B), 7) + 1
PT% = 3

Call MAKEPBP
Call DELAY

If S <= F3S% Then
    363 ' MADE 3-POINT FIELD GOAL

    If PB% = 1 And pbpDelay <> 0 Then
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

    Call CreditAssists
    Call UPDATESCREEN

    GoTo 562

End If

If PB% = 1 And pbpDelay <> 0 Then
    Call WINDEX
    Print "3 PT FG MISSED BY "; AA$(P, B1(P, B), 0)
    Call MOREDELAY
End If

If PB% = 0 Then
    PT% = 5
    Call MAKEPBP
    Call DELAY
End If

X8 = SC(SC): LS = 4
T = T - 4
TE = 4

Call AddMinutes

If T <= 0 Then TE = T: Call AddMinutes: GoTo 170

Call UPDATESCREEN

GoTo 670


'----------


400 '
A4 = 0
D6 = 0
P3 = 0

B = Int(Rnd(1) * 5)

405 R0 = Int(Rnd(1) * 100) + 1


If U5 = 0 And CM% = 1 Or U5 = 1 And D = CT And CM% = 1 Or U5 = 1 And P = CT Or U5 = 2 Then

    CNT% = 0

    While (CNT% < 4)

        1041 CNT% = CNT% + 1

        If CNT% <> 4 Then
            M9 = 0
            B = Int(Rnd(1) * 5)
            X = Int(Rnd(1) * 100) + 1

            If X > 30 Then

                For I = 0 To 4
                    M8 = Int(B%(P, B1(P, I), 2) / B%(P, B1(P, I), 0) + .5) - A(P, B1(P, I), 7)
                    If M8 <= 0 Then M8 = 0
                    If M8 <= 0 And A(P, B1(P, I), 3) - A(P, B1(P, I), 7) - A(P, B1(P, I), 11) - A(P, B1(P, I), 12) > 0 Then M8 = A(P, B1(P, I), 3) - A(P, B1(P, I), 7) - A(P, B1(P, I), 11) - A(P, B1(P, I), 12)
                    If M8 >= M9 Then B = I: M9 = M8
                Next I

            End If
        End If

    Wend

Else

    If R0 <= 40 Then
        Do
            424 '
            If pbpDelay <> 0 And PB% = 0 Then U$ = "PASS TO CHOICE (0-4)": Call DELAY

            If pbpDelay <> 0 And PB% = 1 Then

                If Not (AP = 1 And U5 = 2 Or pbpDelay = 0) Then
                    Call WINDEX
                    Print "PASS TO CHOICE (0-4)"
                    Call DELAY
                Else

                    Do
                        I$ = GetKeyPress$

                        If UCase$(I$) = "T" Then Call TICKER
                        If UCase$(I$) = "S" Then Call SCOUTREPORT
                        If UCase$(I$) = "Q" Then Run "HELLO.EXE"
                        If UCase$(I$) = "Z" Or I$ = "/" Then Call CHANGESTRATEGY(I$)

                    Loop Until UCase$(I$) <> "T" And UCase$(I$) <> "S" And UCase$(I$) <> "Q" And UCase$(I$) <> "Z" And UCase$(I$) <> "/"

                    B = Val(I$)

                End If

            End If

        Loop Until B >= 0 And B <= 4

    Else
        B = Int(Rnd(1) * 5)
    End If

End If


'427

If P1 <> 1 Then

    R0 = Int(Rnd(1) * 1000) + 1

    'On D2(D) GOTO 461, 477, 494, 461, 477: Rem*** BRANCH TO AREA BASED ON DEFENSE ***

    ' *** BRANCH TO AREA BASED ON DEFENSE ***
    Select Case D2(D)

        Case 1, 4:
            461 '*** SHOT VS REG/TIGHT M-M ***

            If R0 <= 2 Then
                X = 0
                GoTo 580
            End If

            If R0 <= 30 Then
                Call SomethingTime
                X8 = SC(SC)
                LS = 4
                Call UPDATESCREEN
                X = 88

                GoTo 565
            End If

            If R0 <= 30 + F7%(P, 1) + F7%(D, 2) Then
                GoTo 515
            End If

            P9 = Int(Rnd(1) * 5)

            If R0 <= 24 + F7%(P, 1) + F7%(D, 2) + A(D, B1(D, P9), 5) + F7%(P, 0) + OX%(O0%(P)) Then
                GoTo 513
            End If

            465 R0 = Int(Rnd(1) * 1000) + 1

            If (PASS% <= 3 Or PASS% > 4) And R0 <= 320 + ((JY - 89) * 10) Then 445
            If PASS% = 4 Then 445

            GoTo 437


        Case 2, 5
            477 '*** SHOT VS LOOSE/SAG M-M ***

            If R0 <= 1 Then
                X = 0
                GoTo 580
            End If

            If R0 <= 10 Then
                Call SomethingTime
                X8 = SC(SC)
                LS = 4
                Call UPDATESCREEN
                X = 88

                GoTo 565
            End If

            If R0 <= 10 + F7%(P, 1) + F7%(D, 2) Then
                GoTo 515
            End If

            P9 = Int(Rnd(1) * 5)

            If R0 <= 8 + F7%(P, 1) + F7%(D, 2) + A(D, B1(D, P9), 5) + F7%(P, 0) + OX%(O0%(P)) Then
                GoTo 513
            End If

            GoTo 465


        Case 3
            494 '*** SHOT VS TRAP M-M ***

            If R0 <= 3 Then
                X = 0
                GoTo 580
            End If

            If R0 <= 50 Then
                Call SomethingTime
                X8 = SC(SC)
                LS = 4
                Call UPDATESCREEN
                X = 88

                GoTo 565
            End If


            If R0 <= 50 + F7%(P, 1) + F7%(D, 2) Then
                GoTo 515
            End If

            P9 = Int(Rnd(1) * 5)

            If R0 <= 40 + F7%(P, 1) + F7%(D, 2) + A(D, B1(D, P9), 5) + F7%(P, 0) + OX%(O0%(P)) Then
                GoTo 513
            End If

            GoTo 465

    End Select


Else
    'P1 = 1

    428 P1 = 0

    R0 = Int(Rnd(1) * 1000) + 1

    If R0 <= 3 Then
        X = 0
        GoTo 580
    End If

    If R0 <= 50 Then
        'GoTo 512
        Call SomethingTime
        X8 = SC(SC)
        LS = 4
        Call UPDATESCREEN
        X = 88
        GoTo 565
    End If

    If R0 <= 50 + F7%(P, 1) + F7%(D, 2) Then
        GoTo 515
    End If

    'How do we ever get here?
    P9 = Int(Rnd(1) * 5)

    If R0 <= 40 + F7%(P, 1) + F7%(D, 2) + A(D, B1(D, P9), 5) + F7%(P, 0) Then
        GoTo 513
    End If

    GoTo 465

End If


'----------

' ***BAD SHOT ***

437 A4 = 0

R0 = Int(Rnd(1) * 3) + 1

If A(P, B1(P, B), 0) > 60 And R0 = 1 Then F = 24
If A(P, B1(P, B), 0) > 60 And R0 = 2 Then F = 30
If A(P, B1(P, B), 0) > 60 And R0 = 3 Then F = 36
If A(P, B1(P, B), 0) <= 60 And R0 = 1 Then F = A(P, B1(P, B), 0) * .4
If A(P, B1(P, B), 0) <= 60 And R0 = 2 Then F = A(P, B1(P, B), 0) * .5
If A(P, B1(P, B), 0) <= 60 And R0 = 3 Then F = A(P, B1(P, B), 0) * .6

PZ% = 1
X = 0

GoTo 520


'----------


445 If Not (H = 4 And T <= 120 Or H > 4) Then
    If A(P, B1(P, B), 3) - A(P, B1(P, B), 7) - A(P, B1(P, B), 11) - A(P, B1(P, B), 12) <= 0 Then 437
End If

446 '*** CHECKING 3-POINT SHOT ***
If XY% <> 0 Then

    F3S% = 0

    RN = Int(Rnd(1) * 100) + 1

    If RN <= A(P, B1(P, B), 19) Then
        PZ% = 4
        F3S% = A(P, B1(P, B), 20) + F7%(D, 4)
        F3S% = F3S% + Int(Rnd(1) * 7) - 3
        X = 0
        GoTo 520
    End If

    If P6 = 4 Then
        If Int(Rnd(1) * 100) + 1 <= 20 Then
            GoTo 437
        Else
            PZ% = 4: X = 0: F3S% = .4 * A(P, B1(P, B), 20) + F7%(D, 4)
            GoTo 520
        End If
    Else
        If H >= 4 And T < 30 And S(D, 0) - S(P, 0) > 3 And A(P, B1(P, B), 19) >= 3 Then
            PZ% = 4: X = 0: F3S% = A(P, B1(P, B), 20) + F7%(D, 4)
            GoTo 520
        End If

    End If

End If

448 '*** CHECKING FOR ASSISTED SHOT ***

'*** GET SHOT PCT ***
F = A(P, B1(P, B), 0) + D1(D)

If N = P Then F = F + 2
If N <> P And N <> 2 Then F = F - 2

A4 = Int(Rnd(1) * 41)
R0 = Int(Rnd(1) * F5%(P, 4)) + 1
RN = Int(.17 * F5%(P, 4) + .5)

If RN >= R0 Then
    F = F + A4
    PZ% = 2: X = 0
Else

    '*** CHECK PENETRATION SHOT ***
    RN = Int(Rnd(1) * 100) + 1
    A4 = Int(Rnd(1) * 50) + 1

    If RN <= (B%(P, B1(P, B), 16) + OY%(O0%(P))) * 2 + 1 Then
        PZ% = 3
        F = F + A4
        X = 0
    Else
        '*** CHECK AVERAGE SHOT ***
        I2 = B%(P, B1(P, B), 16): RN = Int(Rnd(1) * 2)

        If Left$(AA$(P, B1(P, B), 1), 1) = "G" Or Left$(AA$(P, B1(P, B), 1), 1) = "F" Then

            RN = Int(Rnd(1) * 3)

            Select Case RN
                Case 1: PZ% = 6: F = F + GF%(RN, I2): X = 0
                Case 2: PZ% = 7: F = F + GF%(RN, I2): X = 0
                Case 3: PZ% = 5: F = F + GF%(RN, I2): X = 0
            End Select

        Else

            If RN = 1 Then
                PZ% = 6: F = F + CF%(RN, I2): X = 0
            Else
                PZ% = 5: F = F + CF%(RN, I2): X = 0
            End If
        End If

    End If

End If

GoTo 520


'----------


513 '
Call SomethingTime
X8 = SC(SC)
LS = 4
A(P, B1(P, B), 23) = A(P, B1(P, B), 23) + 1

If pbpDelay <> 0 Then

    If PB% = 1 Then
        Call WINDEX
        Print "BALL STOLEN BY "; AA$(D, B1(D, P9), 0); "!"
        Call MOREDELAY
    Else
        RN = Int(Rnd(1) * 100) + 1

        Select Case RN
            Case 1 TO 3: U$ = AA$(D, B1(D, P9), 0) + " QUICKLY REACHES IN!": Call DELAY
            Case 4 TO 6: U$ = AA$(D, B1(D, P9), 0) + " SNEAKS IN FROM BEHIND!": Call DELAY
            Case 7 TO 10: U$ = AA$(D, B1(D, P9), 0) + " GETS A HAND ON THE BALL!": Call DELAY
        End Select

        RN = Int(Rnd(1) * 20) + 1

        Select Case RN
            Case 1: U$ = "PASS INTERCEPTED BY " + AA$(D, B1(D, P9), 0) + "!"
            Case 2: U$ = "PASS IS PICKED OFF BY " + AA$(D, B1(D, P9), 0) + "!"
            Case 3: U$ = "BALL IS STOLEN BY " + AA$(D, B1(D, P9), 0) + "!"
            Case 4: U$ = AA$(D, B1(D, P9), 0) + " WITH THE STEAL!"
            Case 5: U$ = AA$(D, B1(D, P9), 0) + " TAKES IT AWAY!"
            Case 6: U$ = AA$(D, B1(D, P9), 0) + " STEALS IT!"
            Case 7: U$ = AA$(D, B1(D, P9), 0) + " RIPS IT LOOSE!"
            Case 8: U$ = AA$(D, B1(D, P9), 0) + " TAKES THE BALL AWAY!"
            Case 9: U$ = AA$(D, B1(D, P9), 0) + " STEALS THE BALL!"
            Case 10: U$ = AA$(D, B1(D, P9), 0) + " PICKS IT OFF!"
            Case 11: U$ = AA$(D, B1(D, P9), 0) + " SWIPES THE BALL!"
            Case 12: U$ = AA$(D, B1(D, P9), 0) + " WITH THE STOLEN BALL!"
            Case 13: U$ = AA$(D, B1(D, P9), 0) + " STRIPS IT AWAY!"
            Case 14: U$ = AA$(D, B1(D, P9), 0) + " WITH THE STEAL!"
            Case 15: U$ = AA$(D, B1(D, P9), 0) + " SWIPES AND STEALS IT!"
            Case 16: U$ = AA$(D, B1(D, P9), 0) + " SNATCHES IT AWAY!"
            Case 17: U$ = AA$(D, B1(D, P9), 0) + " QUICKLY REACHES IN!"
            Case 18: U$ = AA$(D, B1(D, P9), 0) + " TAKES IT THE OTHER WAY!"
            Case 19: U$ = AA$(D, B1(D, P9), 0) + " POKES IT LOOSE!"
            Case 20: U$ = AA$(D, B1(D, P9), 0) + " STRIPS IT CLEAN!"
        End Select
        Call DELAY
    End If

End If


500 '
A(D, B1(D, P9), 13) = A(D, B1(D, P9), 13) + 1
T0(P) = T0(P) + 1
Call UPDATESCREEN

C1 = 2
P9 = D

If T <= 0 Then TE = T: Call AddMinutes: GoTo 170
PB$ = ""

If P = 1 Then
    P = 0: D = 1: P9 = 0: C1 = 2
Else
    'P = 0;
    P = 1: D = 0: P9 = 1: C1 = 2
End If

FB% = 1

GoTo 300


'----------


515 Call SomethingTime
X8 = SC(SC)
LS = 4
A(P, B1(P, B), 23) = A(P, B1(P, B), 23) + 1
T0(P) = T0(P) + 1

If SV% <> 1 Then PT% = 1: Call MAKEPBP

SV% = 0

If PB% = 1 And pbpDelay <> 0 Then
    Call WINDEX
    Print "TURNOVER BY ";: Color T(P, 33), T(P, 34): Print B$(P, 0): Color 15, 0: Call MOREDELAY
End If

If PB% = 0 Then Call DELAY
U$ = "BALL OUT OF BOUNDS TO " + B$(D, 0)
If PB% = 0 Then Call DELAY
If PB% = 1 And pbpDelay <> 0 Then Print U$: Call MOREDELAY

Call MOREDELAY
Call UPDATESCREEN
Locate 10, 1

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: Call AddMinutes: GoTo 170

PB$ = ""
If P = 1 Then
    P = 0: D = 1: P9 = 0: C1 = 2
Else
    'P = 0;
    P = 1: D = 0: P9 = 1: C1 = 2
End If

Call Substitutions

GoTo 300


'----------


520 Call SomethingTime

If T <= 0 Then TE = T: Call AddMinutes: GoTo 170

If PZ% = 4 And F3S% <= 0 Then F3S% = 3

If pbpDelay <> 0 Then

    If F > 70 And FB% = 1 And PASS% = 1 Then
        PT% = 6: PZ% = 8: Call MAKEPBP: Call DELAY
    Else

        If AA$(P, B1(P, B), 0) <> PB$ And PB$ = "" Then U$ = "PASS TO " + AA$(P, B1(P, B), 0): Call DELAY
        If AA$(P, B1(P, B), 0) <> PB$ And PB$ <> "" Then PT% = 10: Call MAKEPBP: Call DELAY
        If AA$(P, B1(P, B), 0) = PB$ Then PT% = 9: Call MAKEPBP: Call DELAY

        If X8 <= 0 Then U$ = "SHOT CLOCK HAS EXPIRED!": SV% = 1: GoTo 515

        If X8 < 5 Then U$ = "SHOT CLOCK DOWN TO" + Str$(X8): Call DELAY

        PB$ = AA$(P, B1(P, B), 0)
        Z3$ = Left$(AA$(P, B1(P, B), 1), 1)
        Y2$ = "HE"
        Y3$ = AA$(D, B1(D, B), 0)


        'On PZ% GOTO 521, 522, 523, 524, 525, 526, 527

        Select Case PZ%

            Case 1:
                If pbpDelay <> 0 Then
                    RN = Int(Rnd(1) * 12) + 1

                    Select Case RN
                        Case 1: U$ = Y2$ + "'S HEAVILY GUARDED"
                        Case 2: U$ = Y2$ + "'S GUARDED"
                        Case 3: U$ = Y2$ + "'S GUARDED BY " + Y3$
                        Case 4: U$ = Y2$ + " HAS A MAN ON HIM"
                        Case 5: U$ = Y2$ + " HAS A DEFENDER ON HIM"
                        Case 6: U$ = Y2$ + "'S COVERED"
                        Case 7: U$ = Y2$ + " CAN'T GET AROUND THE DEFENDER"
                        Case 8: U$ = Y2$ + "'S PRESSURED"
                        Case 9: U$ = Y2$ + " HAS NOWHERE TO GO"
                        Case 10: U$ = Y2$ + " HAS A HAND IN HIS FACE"
                        Case 11: U$ = Y2$ + "'S WATCHED BY " + Y3$
                        Case 12: U$ = Y2$ + "'S WORKING ON " + Y3$
                    End Select
                End If

                Call DELAY

            Case 2:
                U$ = Y2$ + " WORKS FREE INSIDE": Call DELAY
                U$ = "HE HAS AN OPEN SHOT": Call DELAY

            Case 3:
                If pbpDelay <> 0 Then

                    RN = Int(Rnd(1) * 100) + 1

                    If Z3$ = "C" Then

                        Select Case RN
                            Case 1 TO 25: U$ = Y2$ + " MAKES A MOVE DOWN LOW":
                            Case 26 TO 50: U$ = Y2$ + " POSITIONS HIMSELF DOWN LOW"
                            Case 51 TO 75: U$ = Y2$ + " POSTS UP UNDERNEATH"
                            Case 76 TO 100: U$ = Y2$ + " BACKS IN AGAINST " + AA$(D, B1(D, 4), 0)
                        End Select

                        Call DELAY
                        U$ = "HE HAS A GOOD SHOT INSIDE"
                        Call DELAY

                    End If


                    602 '
                    If Z3$ = "F" Then
                        Select Case RN
                            Case 1 TO 17: U$ = Y2$ + " DRIVES THE BASELINE"
                            Case 18 TO 34: U$ = Y2$ + " PENETRATES THE LANE"
                            Case 35 TO 50: U$ = Y2$ + " MOVES INSIDE"
                            Case 51 TO 67: U$ = Y2$ + " DRIVES THE LANE"
                            Case 68 TO 84: U$ = Y2$ + " MOVES INTO THE PAINT"
                            Case 85 TO 100: U$ = Y2$ + " DRIVES UNDERNEATH"
                        End Select

                        Call DELAY
                        U$ = "HE'S GOT A GOOD SHOT UNDERNEATH"
                        Call DELAY
                    End If


                    If Z3$ = "G" Then
                        Select Case RN
                            Case 1 TO 25: U$ = Y2$ + " DRIVES THE LANE": Call DELAY: U$ = "HE HAS A GOOD SHOT CLOSE IN"
                            Case 26 TO 50: U$ = Y2$ + " DRIVES THROUGH THE KEY": Call DELAY: U$ = "HE'S INTO THE LANE": Call DELAY: U$ = "HE HAS A GOOD SHOT CLOSE IN"
                            Case 51 TO 75: U$ = Y2$ + " DRIVES TO THE BASKET": Call DELAY: U$ = "HE HAS A GOOD SHOT CLOSE IN"
                        End Select

                        Call DELAY
                    Else
                        U$ = Y2$ + " DRIVES THROUGH AN OPENING": Call DELAY
                    End If

                End If

            Case 4:
                U$ = "HE'S OPEN FROM 3-POINT RANGE!": Call DELAY

            Case 5:
                U$ = "HE'S OPEN FROM CLOSE RANGE": Call DELAY

            Case 6:
                U$ = "HE'S OPEN FROM MEDIUM RANGE": Call DELAY

            Case 7:
                U$ = "HE'S OPEN FROM LONG RANGE": Call DELAY

        End Select


    End If

End If

'GoTo 529

529 If F <= 0 Then F = 1

If F >= 100 Then F = 100

If pbpDelay <> 0 Then

    If PB% = 1 Then
        Call WINDEX
        Print "PASS TO "; AA$(P, B1(P, B), 0); " OF ";
        Color T(P, 33), T(P, 34)
        Print B$(P, 0): Color 15, 0
    End If

    If PB% = 1 And PZ% <> 4 Then Locate 10: Print "SHOT % "; Int(F + .5);

    If PB% = 1 And PZ% = 4 Then
        Locate 10: Print "3 PT SHOT % "; Int(F3S% + .5);: Print " *";
    Else
        'PZ% = 2-3, 5-8
        If PB% = 1 And PZ% >= 2 And PZ% <= 8 Then Print "*";

        If pbpDelay <> 0 Or PB% = 1 Then

            If PZ% <> 4 Then Locate 8, 41: Print Using "SHOT % ###   "; Int(F + .5);

            If PZ% = 4 Then
                Locate 8, 41: Print Using "SHOT % ###"; Int(F3S% + .5);: Print " *";
            Else
                If PZ% >= 2 And PZ% <= 8 Then Locate , 52: Print "*";
            End If
        End If
    End If

End If

528 Print

If PB% = 1 And pbpDelay <> 0 Then Call MOREDELAY

If U5 <> 0 Or CM% = 1 Then
    949 '
    If H >= 4 And T <= 240 And S(P, 0) - S(D, 0) > 0 And U5 = 1 And P = CT Then

        U$ = Str$(A(P, B1(P, B), 1)) + " FT%  INTENTIONAL FOUL (YN)"

        Do
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "3"

        If UCase$(I$) = "Y" Or I$ = "3" Then P6 = 3: GoTo 532

    Else

        If U5 = 2 Or U5 = 1 And D = CT Then

            980 '
            If T <= 240 And H >= 4 And S(P, 0) - S(D, 0) > 10 Then

                If (U5 = 1 Or U5 = 0) And CM% <> 1 Then 530

                GoTo 951

            Else

                If T <= 30 And H >= 4 And S(P, 0) - S(D, 0) > 0 Then P6 = 3: GoTo 532

            End If

        End If
    End If

    951 If T <= 1 Or X8 <= 6 Then P6 = 1: GoTo 532
    If T <= 8 And S(P, 0) - S(D, 0) <= -3 And H >= 4 Then P6 = 1: GoTo 532
    If T <= 20 And S(P, 0) - S(D, 0) <= -5 And H >= 4 Then P6 = 1: GoTo 532
    If PZ% > 1 Then P6 = 1: GoTo 532
    P6 = 0

    GoTo 532

End If


530 I$ = GetKeyPress$
If UCase$(I$) = "T" Then Call TICKER: GoTo 530
If UCase$(I$) = "S" Then Call SCOUTREPORT: GoTo 530
If UCase$(I$) = "Q" Then Run "HELLO.EXE"
If UCase$(I$) = "Z" Or I$ = "/" Then Call CHANGESTRATEGY(I$): GoTo 530
P6 = Val(I$)
If P6 < 1 Or P6 > 4 Then P6 = 0
If P6 = 4 Then FT% = 1


'Wait for keypress during Play-By-Play
532 I$ = InKey$
If UCase$(I$) = "T" Then Call TICKER
If UCase$(I$) = "Z" Or I$ = "/" Then Call CHANGESTRATEGY(I$)
If UCase$(I$) = "S" Then Call SCOUTREPORT
If UCase$(I$) = "Q" Then Run "HELLO.EXE"
If Val(I$) = 3 And (U5 = 0 Or U5 = 1 And P = CT) And H >= 4 And T < 240 Then P6 = 3
If Val(I$) = 2 And (U5 = 0 Or U5 = 1 And D = CT) And T1(P) > 0 Then P6 = 2
P3 = P6
If P6 = 2 And T1(P) = 0 Then 530
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
If R0 <= A(P, B1(P, B), 16) + F7%(D, 3) Then
    A(P, B1(P, B), 7) = A(P, B1(P, B), 7) - 1
    X8 = SC(SC): LS = 4
    GoTo 589
End If


540 S = Int(Rnd(1) * 100) + 1: R0 = (F - 2) * .1
If S <= R0 Then
    X8 = SC(SC)
    LS = 4

    610 T = T - 4: TE = 4: Call AddMinutes: Call UPDATESCREEN

    ' --- is this 553 ???
    If DK% = 1 And PB% = 0 Then PT% = 2: Call MAKEPBP: U$ = "STUFF BY " + AA$(P, B1(P, B), 0): Call DELAY: DK% = 0: GoTo 617
    If DK% = 1 And PB% = 1 Then Call WINDEX: Print "STUFF BY " + AA$(P, B1(P, B), 0): Call MOREDELAY: DK% = 0: GoTo 617
    If PZ% = 8 And PB% = 0 Then PT% = 7: Call MAKEPBP: PZ% = 0: Call DELAY: GoTo 617
    If PB% = 0 Then U$ = "BASKET GOOD BY " + AA$(P, B1(P, B), 0): Call DELAY
    If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Print "BASKET GOOD BY " + AA$(P, B1(P, B), 0): Call MOREDELAY

    617 U$ = "... AND HE'S FOULED !!!"
    If PB% = 0 Then Call DELAY
    If PB% = 1 And pbpDelay <> 0 Then Print U$: Call MOREDELAY

    F3 = 1

    Call MOREDELAY

    Call CreditAssists

    F3 = 1
    S(P, H) = S(P, H) + 2
    S(P, 0) = S(P, 0) + 2
    A(P, B1(P, B), 8) = A(P, B1(P, B), 8) + 1

    Call UPDATESCREEN

    Call Fouls(0)

    If pbpDelay <> 0 Then
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
    End If

    620 '
    X = 50
    Call Substitutions
    X = 0

    If pbpDelay <> 0 Then

        If PB% = 0 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
            U$ = "2 TO MAKE 1 FT: " + AA$(P, B1(P, B), 0) + Str$(A(P, B1(P, B), 1)) + " %"
            Call DELAY
            GoTo 621
        End If

        If PB% = 0 Then
            U$ = "1 FT: " + AA$(P, B1(P, B), 0) + Str$(A(P, B1(P, B), 1)) + " %"
            Call DELAY
            GoTo 621
        End If

        If PB% = 1 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
            Call WINDEX
            Print B$(P, 0)
            Print "2 TO MAKE 1 FT: ";
            Print AA$(P, B1(P, B), 0); "   "; A(P, B1(P, B), 1); " %"
            GoTo 621
        End If

        If PB% = 1 Then Call WINDEX: Print B$(P, 0): Print "1 FT: "; AA$(P, B1(P, B), 0); "   "; A(P, B1(P, B), 1); " %"

    End If

    621 NM% = 0

    For F3 = 1 To 2

        S = Int(Rnd(1) * 100) + 1

        FX% = A(P, B1(P, B), 1)

        If S <= FX% And PB% = 0 Then NM% = NM% + 1: Call THREEFT
        If S <= FX% And PB% = 1 Then NM% = NM% + 1: Call MADEFT
        If S > FX% And PB% = 0 Then Call BADFT
        If S > FX% And PB% = 1 Then Call MISSFT

        A(P, B1(P, B), 9) = A(P, B1(P, B), 9) + 1

        If NM% = 1 Then Exit For
        If F3 = 1 And (RT% = 0 Or RT% = 1 And (P2(D) <= NB% And H <= 4 Or P2(D) <= 3 And H > 4)) Then Exit For

    Next F3

    624 If T <= 0 Then TE = T: Call AddMinutes: GoTo 170

    X = 99

    If S > FX% Then 670

    PB$ = ""

    If P = 1 Then
        P = 0: D = 1: P9 = 0: C1 = 2
    Else
        'P = 0;
        P = 1: D = 0: P9 = 1: C1 = 2
    End If

    U$ = B$(P, 0) + " BALL AFTER SCORE"
    Call SELECTPBP
    Locate 8, 41: Print Space$(12)
    Call Choices

    B = 6
    X = 0

    If pbpDelay <> 0 Then
        For I = 8 To 15
            Locate I, 1
            Print Space$(38)
        Next I
    End If


    Locate 8, 1

    If P7(D) = 1 Then
        If PB% = 0 Then U$ = B$(D, 0) + " IS PRESSING": Call DELAY

        If PB% = 1 And pbpDelay <> 0 Then
            Call WINDEX
            Color T(D, 33), T(D, 34): Print B$(D, 0);
            Color 15, 0: Print " IS PRESSING"
            Call MOREDELAY
        End If

        P1 = 1
    End If

    GoTo 300

End If


If S <= F - 2 Then 553

R0 = Int(Rnd(1) * 100) + 1


548 '
I5 = Int(Rnd(1) * 100) + 1
I3 = Int(Rnd(1) * 5)

If I5 > A(D, B1(D, I3), 6) Then 545

If pbpDelay <> 0 Then

    If PB% = 1 Then
        Call WINDEX
        Print "SHOT BLOCKED BY "; AA$(D, B1(D, I3), 0)
        Call MOREDELAY
    Else

        RN = Int(Rnd(1) * 100) + 1

        Select Case RN
            Case 1 TO 10: U$ = AA$(D, B1(D, I3), 0) + " STUFFS THE SHOT!!"
            Case 11 TO 15: U$ = "DENIED BY " + AA$(D, B1(D, I3), 0) + "!!"
            Case 16 TO 20: U$ = "ERASED BY " + AA$(D, B1(D, I3), 0) + "!!"
            Case 21 TO 25: U$ = "EXPUNGED BY " + AA$(D, B1(D, I3), 0) + "!!"
            Case 26 TO 30: U$ = "REBUKED BY " + AA$(D, B1(D, I3), 0) + "!!"
            Case 31 TO 35: U$ = "SHOT BLOCKED CLEANLY BY " + AA$(D, B1(D, I3), 0) + "!!"
            Case 36 TO 70: U$ = "SHOT BLOCKED BY " + AA$(D, B1(D, I3), 0) + "!!"
            Case 71 TO 90: U$ = "SHOT REJECTED BY " + AA$(D, B1(D, I3), 0) + "!!"
            Case 91 TO 100: U$ = "SHOT SWATTED OUT BY " + AA$(D, B1(D, I3), 0) + "!!"
        End Select

        Call DELAY

    End If

End If

554 A(D, B1(D, I3), 15) = A(D, B1(D, I3), 15) + 1
X = 98

GoTo 546


'----------


545 '
If PB% = 1 And pbpDelay <> 0 Then
    Call WINDEX
    Print "SHOT MISSED BY "; AA$(P, B1(P, B), 0)
    Call MOREDELAY
End If

If PB% = 0 Then PT% = 8: Call MAKEPBP: Call DELAY

X8 = SC(SC)
LS = 4

546 '
T = T - 4
TE = 4
Call AddMinutes

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: Call AddMinutes: GoTo 170

Call UPDATESCREEN

GoTo 670


'----------


553 '

If DK% = 1 Then
    If PB% = 0 Then
        PT% = 2
        Call MAKEPBP
        U$ = "STUFF BY " + AA$(P, B1(P, B), 0) + " !!!": DK% = 0
    Else
        'PB% = 1 Then
        U$ = "STUFF BY " + AA$(P, B1(P, B), 0) + " !!!": DK% = 0
    End If
Else
    If PB% = 0 And PZ% = 8 Then
        PT% = 7: Call MAKEPBP: Call DELAY: PZ% = 0: skipShotPBP% = 1
    Else
        U$ = "BASKET BY "
        U$ = U$ + AA$(P, B1(P, B), 0) + " !!!"
    End If
End If

If skipShotPBP% = 0 Then

    If PB% = 1 And pbpDelay <> 0 Then
        Call WINDEX: Print U$: Call MOREDELAY
    Else

        RN = Int(Rnd(1) * 100) + 1

        If RN > 50 Then

            RN = Int(Rnd(1) * 22) + 1

            Select Case RN
                Case 1: U$ = "NOTHING BUT THE BOTTOM OF THE NET!"
                Case 2: U$ = "Count IT!"
                Case 3: U$ = AA$(P, B1(P, B), 0) + " TICKLES THE TWINE!"
                Case 4: U$ = "NO RIM, NO BOARD, JUST CORD!"
                Case 5: U$ = "GOOD!"
                Case 6: U$ = "YES!"
                Case 7: U$ = "COUNT IT!"
                Case 8: U$ = "IT'S GOOD!"
                Case 9: U$ = "BINGO!"
                Case 10: U$ = "IT FALLS!"
                Case 11: U$ = "HE DRAINS IT!"
                Case 12: U$ = "SWISH!"
                Case 13: U$ = "ALL NET!"
                Case 14: U$ = "IT'S THERE!"
                Case 15: U$ = "HE BURIES IT!"
                Case 16: U$ = "IT GOES IN!"
                Case 17: U$ = "RING IT UP!"
                Case 18: U$ = "IT GOES DOWN!"
                Case 19: U$ = "BULLSEYE!"
                Case 20: U$ = "HE SCORES!"
                Case 21: U$ = "HE HAS IT!"
                Case 22: U$ = "HE CONNECTS!"
            End Select

        End If

        If PB% = 0 Then Call DELAY

    End If

End If

560 '
S(P, H) = S(P, H) + 2
S(P, 0) = S(P, 0) + 2

Call CreditAssists

Call UPDATESCREEN

A(P, B1(P, B), 8) = A(P, B1(P, B), 8) + 1

'---> possibly a new sub
562 U$ = B$(D, 0) + " BALL AFTER SCORE"

If PB% = 1 And pbpDelay <> 0 Then
    Locate 9, 1
    Color T(D, 33), T(D, 34)
    Print B$(D, 0);: Color 15, 0
    Print " BALL AFTER SCORE"
    Call MOREDELAY
End If
If PB% = 0 Then Locate 8, 41: Print Space$(12): Call DELAY
X8 = SC(SC): LS = 4
T = T - 4
TE = 4
Call AddMinutes

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: Call AddMinutes: GoTo 170

Call UPDATESCREEN

PB$ = ""
If P = 1 Then
    P = 0: D = 1: P9 = 0: C1 = 2
Else
    'P = 0;
    P = 1: D = 0: P9 = 1: C1 = 2
End If

Call Choices

If pbpDelay <> 0 Then
    For I = 8 To 15
        Locate I, 1
        Print Space$(38)
    Next I
End If


Locate 8, 1
If P7(D) = 1 Then
    If PB% = 0 Then U$ = B$(D, 0) + " IS PRESSING": Call DELAY
    If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Color T(D, 33), T(D, 34): Print B$(D, 0);: Color 15, 0: Print " IS PRESSING": Call MOREDELAY
    P1 = 1
End If


GoTo 300

'----------

565 Call Fouls(0)

566 If P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then 570

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: Call AddMinutes: GoTo 170
If FR% = 0 Then
    Call Substitutions

    If pbpDelay <> 0 Then
        For I = 8 To 15
            Locate I, 1
            Print Space$(38)
        Next I
    End If

    Locate 8, 1
    U$ = B$(P, 0) + " BALL AFTER FOUL"
    Call DELAY
    Locate 8, 41: Print Space$(12)
    GoTo 350
End If

Call UPDATESCREEN

If pbpDelay <> 0 Then
    If PB% = 0 Then
        U$ = "1 FT: " + AA$(P, B1(P, B), 0) + Str$(A(P, B1(P, B), 1)) + " %"
        Call DELAY
        GoTo 571
    End If

    Call WINDEX
    Color T(P, 33), T(P, 34): Print B$(P, 0)
    Color 15, 0

    Print "1 FT: "; AA$(P, B1(P, B), 0); " "; A(P, B1(P, B), 1); " %"

End If

571 '
X = 50
Call Substitutions
X = 0

If pbpDelay <> 0 Then

    If PB% = 0 Then
        U$ = "1 FT: " + AA$(P, B1(P, B), 0) + Str$(A(P, B1(P, B), 1)) + " %"
        Call DELAY
        GoTo 572
    End If

    Call WINDEX
    Color T(P, 33), T(P, 34): Print B$(P, 0)
    Color 15, 0
    Print "1 FT: "; AA$(P, B1(P, B), 0); " "; A(P, B1(P, B), 1); " %"

End If

572 '
A(P, B1(P, B), 9) = A(P, B1(P, B), 9) + 1
F3 = 1
S = Int(Rnd(1) * 100) + 1
FX% = A(P, B1(P, B), 1)

If S <= FX% And PB% = 0 Then Call THREEFT
If S <= FX% And PB% = 1 Then Call MADEFT
If S > FX% And PB% = 0 Then Call BADFT
If S > FX% And PB% = 1 Then Call MISSFT
LT = 0

If T <= 0 Then TE = T: Call AddMinutes: GoTo 170

If S > FX% Then 650

PB$ = ""
If P = 1 Then
    P = 0: D = 1: P9 = 0: C1 = 2
Else
    'P = 0;
    P = 1: D = 0: P9 = 1: C1 = 2
End If

U$ = B$(P, 0) + " BALL AFTER SCORE"
Call SELECTPBP
Locate 8, 41: Print Space$(12)
Call Choices
B = 6
X = 0

If pbpDelay <> 0 Then
    For I = 8 To 15
        Locate I, 1
        Print Space$(38)
    Next I
End If


Locate 8, 1
If P7(D) = 1 Then
    If PB% = 0 Then U$ = B$(D, 0) + " IS PRESSING": Call DELAY
    If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Color T(D, 33), T(D, 34): Print B$(D, 0);: Color 15, 0: Print " IS PRESSING": Call MOREDELAY
    P1 = 1
End If

GoTo 300


'----------


570 If RT% = 1 And (P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4) Then
    U$ = AA$(P, B1(P, B), 0) + " GETS 3 FT TO MAKE 2..."
Else
    U$ = AA$(P, B1(P, B), 0) + " GETS 2 FREE THROWS..."
End If
Call DELAY

GoTo 591


'----------


580 Call SomethingTime
X8 = SC(SC): LS = 4

PB$ = ""
If P = 1 Then
    P = 0: D = 1: P9 = 0: C1 = 2
Else
    'P = 0;
    P = 1: D = 0: P9 = 1: C1 = 2
End If

Call Fouls(0)
P2(D) = P2(D) - 1
B = W5
If P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then 570

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: Call AddMinutes: GoTo 170

U$ = "OFFENSIVE FOUL"
If PB% = 0 Then Call DELAY
If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Print U$: Call MOREDELAY
U$ = B$(P, 0) + " BALL AFTER FOUL"
If PB% = 0 Then Call DELAY
If PB% = 1 And pbpDelay <> 0 Then Print U$: Call MOREDELAY

Locate 8, 41: Print Space$(12)
B = 6
Call DELAY
T0(D) = T0(D) + 1
Call UPDATESCREEN
Call Substitutions

If pbpDelay <> 0 Then
    For I = 8 To 15
        Locate I, 1
        Print Space$(38)
    Next I
End If


Locate 8, 1
If P7(D) = 1 Then
    If PB% = 0 Then U$ = B$(D, 0) + " IS PRESSING": Call DELAY
    If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Color T(D, 33), T(D, 34): Print B$(D, 0);: Color 15, 0: Print " IS PRESSING": Call MOREDELAY
    P1 = 1
End If


GoTo 300

'----------

584 IN = 99

Call SomethingTime

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: Call AddMinutes: GoTo 170

C1 = 2: P9 = D
U$ = "DESIGNATED FOULER (0-4)"
Call SELECTPBP

If U5 = 1 And D = CT Or U5 = 2 Then W5 = Int(Rnd(1) * 5): GoTo 588
586 I$ = GetKeyPress$

W5 = Val(I$)

If W5 < 0 Or W5 > 4 Then 586

588 Call Fouls(1)

X8 = SC(SC)

GoTo 566

'----------

589 '
T = T - 4
TE = 4

Call AddMinutes
Call UPDATESCREEN
Call Fouls(0)

If pbpDelay <> 0 Then
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
End If

591 '
X = 50
Call Substitutions
X = 0

If pbpDelay <> 0 Then
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
End If

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
596 If T <= 0 Then TE = T: Call AddMinutes: GoTo 170

If S > A(P, B1(P, B), 1) Then 650

PB$ = ""
If P = 1 Then
    P = 0: D = 1: P9 = 0: C1 = 2
Else
    'P = 0;
    P = 1: D = 0: P9 = 1: C1 = 2
End If

U$ = B$(P, 0) + " BALL AFTER SCORE"

Call SELECTPBP
Locate 8, 41: Print Space$(12)
Call Choices
B = 6
X = 0

If pbpDelay <> 0 Then
    For I = 8 To 15
        Locate I, 1
        Print Space$(38)
    Next I
End If


Locate 8, 1
If P7(D) = 1 Then
    If PB% = 0 Then U$ = B$(D, 0) + " IS PRESSING": Call DELAY
    If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Color T(D, 33), T(D, 34): Print B$(D, 0);: Color 15, 0: Print " IS PRESSING": Call MOREDELAY
    P1 = 1
End If

GoTo 300

'----------


'----------


640 If T1(P) <= 0 Then 648
T1(P) = T1(P) - 1
T = T + M5: TE = -M5: Call AddMinutes
X8 = X8 + M5
U$ = "TIMEOUT " + B$(P, 0):

Call DELAY
Call MOREDELAY
Call Substitutions

648 If A6 = 0 Then

    If pbpDelay <> 0 Then
        For I = 8 To 15
            Locate I, 1
            Print Space$(38)
        Next I
    End If

    Locate 8, 1

    If P7(D) = 1 Then
        If PB% = 0 Then U$ = B$(D, 0) + " IS PRESSING": Call DELAY
        If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Color T(D, 33), T(D, 34): Print B$(D, 0);: Color 15, 0: Print " IS PRESSING": Call MOREDELAY
        P1 = 1
    End If

    GoTo 300
End If

GoTo 350

'----------

650 A6 = 1: P6 = 0: A4 = 0: D6 = 0
X8 = SC(SC): LS = 4

S = 0
R0 = Int(Rnd(1) * 100) + 1
If H > 3 And T < 30 And S(D, 0) - S(P, 0) = 2 And R0 <= 12 Or H > 3 And T < 30 And S(D, 0) - S(P, 0) = 0 And R0 <= 12 Then
    Call ReboundRoutine1
    GoTo 760
End If

R0 = Int(Rnd(1) * 100) + 1

Select Case R0
    Case 1 TO 11: FF% = 1: GoTo 707
    Case 12 TO 19: Call ReboundRoutine1: GoTo 760
    Case 20 TO 110: Call ReboundRoutine2: GoTo 800
End Select


'----------


670 A6 = 1
P6 = 0
A4 = 0
D6 = 0
X8 = SC(SC)
LS = 4

700 '
S = 0
A4 = 0
D6 = 0

R0 = Int(Rnd(1) * 100) + 1

GG = S(D, 0) - S(P, 0)

'---> This is a case/select <---

If R0 <= 12 And H > 3 And T < 30 And (GG = 2 Or GG = 0) Then 700

Select Case R0

    Case 1
        If X = 98 Then 700

        T0(P) = T0(P) + 1
        U$ = "OFFENSIVE GOAL TENDING"
        If PB% = 0 Then Call DELAY
        If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Print U$: Call MOREDELAY
        U$ = "BALL GOES TO " + B$(D, 0)
        If PB% = 0 Then Call DELAY
        If PB% = 1 And pbpDelay <> 0 Then Print U$: Call MOREDELAY

        Call MOREDELAY

        PB$ = ""
        If P = 1 Then
            P = 0: D = 1: P9 = 0: C1 = 2
        Else
            'P = 0;
            P = 1: D = 0: P9 = 1: C1 = 2
        End If

        Call Substitutions

        GoTo 300

    Case 2 TO 11
        FF% = 1
        GoTo 707
    Case 12 TO 19
        R0 = Int(Rnd(1) * 100) + 1

        If R0 <= 32 Then 752

        R3(D) = R3(D) + 1
        Call UPDATESCREEN

        U$ = "RB KNOCKED O.B. TO " + B$(D, 0)
        If PB% = 0 Then Call DELAY
        If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Print U$: Call MOREDELAY

        Call MOREDELAY

        Call Substitutions

        PB$ = ""

        If P = 1 Then
            P = 0: D = 1: P9 = 0: C1 = 2
        Else
            'P = 0;
            P = 1: D = 0: P9 = 1: C1 = 2
        End If

        C1 = 2: P9 = P

        GoTo 300

    Case 20 TO 22
        If X = 99 Then 700

        Call ReboundRoutine1

        A(P, B1(P, B), 7) = A(P, B1(P, B), 7) + 1
        A(P, B1(P, B), 8) = A(P, B1(P, B), 8) + 1
        A(P, B1(P, B), 11) = A(P, B1(P, B), 11) + 1
        R3(P) = R3(P) + 1
        S(P, H) = S(P, H) + 2
        S(P, 0) = S(P, 0) + 2

        Call UPDATESCREEN

        If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Print "TIPPED IN BY "; AA$(P, B1(P, B), 0); "!!!": Call MOREDELAY
        If PB% = 0 Then U$ = AA$(P, B1(P, B), 0) + " TIPS IT IN!!!": Call DELAY

        Call MOREDELAY

        PB$ = ""
        If P = 1 Then
            P = 0: D = 1: P9 = 0: C1 = 2
        Else
            'P = 0;
            P = 1: D = 0: P9 = 1: C1 = 2
        End If

        U$ = B$(P, 0) + " BALL AFTER SCORE"
        Locate 8, 41: Print Space$(12)

        If PB% = 0 Then Call DELAY
        If PB% = 1 And pbpDelay <> 0 Then Print U$: Call MOREDELAY

        Call Choices

        If pbpDelay <> 0 Then
            For I = 8 To 15
                Locate I, 1
                Print Space$(38)
            Next I
        End If


        Locate 8, 1

        If P7(D) = 1 Then

            If PB% = 0 Then U$ = B$(D, 0) + " IS PRESSING": Call DELAY
            If PB% = 1 And pbpDelay <> 0 Then
                Call WINDEX
                Color T(D, 33), T(D, 34): Print B$(D, 0);
                Color 15, 0: Print " IS PRESSING"
                Call MOREDELAY
            End If

            P1 = 1
        End If


        GoTo 300

    Case 23:
        If P6 <> 4 Then
            If X = 99 Then 700
            U$ = "DEFENSIVE GOAL TENDING"
            If PB% = 0 Then Call DELAY
            If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Print U$: Call MOREDELAY
            Call MOREDELAY

            GoTo 560
        End If

End Select

707 'GoTo 770

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
    If RN <= RB(B) Then skipRandom% = 1: Exit For
    RB(B + 1) = RB(B + 1) + RB(B)
Next

If skipRandom% = 0 Then
    B = Int(Rnd(1) * 10)
End If

786 '
If B <= 4 Then 761
B = B - 5

GoTo 800


'----------


720 U$ = AA$(P, B1(P, B), 0) + " HAS THE OFFENSIVE REBOUND"
If PB% = 0 Then Call DELAY
If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Print U$: Print "FOR "; B$(P, 0): Call MOREDELAY

U$ = "HE'S FOULED !!!"
If PB% = 0 Then Call DELAY
If PB% = 1 And pbpDelay <> 0 Then Print U$: Call MOREDELAY


Call MOREDELAY
A(P, B1(P, B), 24) = A(P, B1(P, B), 24) + 1
A(P, B1(P, B), 11) = A(P, B1(P, B), 11) + 1
R3(P) = R3(P) + 1
Call UPDATESCREEN

Call Fouls(0)

GoTo 570


'----------


752 R3(P) = R3(P) + 1

Call UPDATESCREEN

U$ = "RB KNOCKED O.B. TO " + B$(P, 0)

If PB% = 0 Then Call DELAY
If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Print U$: Call MOREDELAY

Call MOREDELAY
Call Substitutions

GoTo 520


'----------


760 If FF% = 1 Then FF% = 0: GoTo 720

761 A(P, B1(P, B), 24) = A(P, B1(P, B), 24) + 1
A(P, B1(P, B), 11) = A(P, B1(P, B), 11) + 1
R3(P) = R3(P) + 1: Call UPDATESCREEN
U$ = AA$(P, B1(P, B), 0) + " HAS THE OFFENSIVE REBOUND"

If PB% = 0 And pbpDelay <> 0 Then Call DELAY
If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Print U$: Print "FOR " + B$(P, 0): Call MOREDELAY

' removed this before GOTO 170:
' Call UPDATESCREEN:
If T <= 0 Then TE = T: Call AddMinutes: GoTo 170

PB$ = AA$(P, B1(P, B), 0)
R0 = Int(Rnd(1) * 10) + 1
'*** GET SHOT PCT ***
F = A(P, B1(P, B), 0) + D1(D)

If N = P Then F = F + 2
If N <> P And N <> 2 Then F = F - 2
If H > 4 Or H = 4 And T <= 120 Then 765

If A(P, B1(P, B), 3) - A(P, B1(P, B), 7) - A(P, B1(P, B), 11) - A(P, B1(P, B), 12) <= 0 Then F = F * .6

765 If R0 <= 8 Then PZ% = 9: GoTo 537

GoTo 520


'----------


800 If FF% = 1 Then
    FF% = 0
    If pbpDelay <> 0 Then
        If PB% = 1 Then Call WINDEX: Print AA$(D, B1(D, B), 0); " HAS THE DEF REBOUND FOR ": Print B$(D, 0); " AND IS FOULED !!!": Call MOREDELAY: GoTo 713
        U$ = AA$(D, B1(D, B), 0) + " HAS THE DEFENSIVE REBOUND": Call DELAY
        U$ = "HE'S FOULED !!!": Call DELAY
        Call MOREDELAY
    End If

    713 R3(D) = R3(D) + 1
    A(D, B1(D, B), 11) = A(D, B1(D, B), 11) + 1
    Call UPDATESCREEN

    PB$ = ""

    If P = 1 Then
        P = 0: D = 1: P9 = 0: C1 = 2
    Else
        'P = 0;
        P = 1: D = 0: P9 = 1: C1 = 2
    End If

    GoTo 565
End If

A(D, B1(D, B), 11) = A(D, B1(D, B), 11) + 1
R3(D) = R3(D) + 1: Call UPDATESCREEN
U$ = AA$(D, B1(D, B), 0) + " HAS THE DEFENSIVE REBOUND"

If PB% = 0 And pbpDelay <> 0 Then Call DELAY
If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Print U$: Print "FOR " + B$(D, 0): Call MOREDELAY

PB$ = AA$(D, B1(D, B), 0)
C1 = 2: P9 = D

PB$ = ""

If P = 1 Then
    P = 0: D = 1: P9 = 0: C1 = 2
Else
    'P = 0;
    P = 1: D = 0: P9 = 1: C1 = 2
End If

FB% = 1

GoTo 300


'================================================================================
'================================================================================
'================================================================================
'================================================================================
'================================================================================



'----------------------------------------
' This doesn't appear to be used
' XXX
If U5 = 1 And P = CT And H <= 3 And T < 30 And J < 3 Then GoSub 1080
If U5 = 2 And H <= 3 And T < 30 And J < 3 Then GoSub 1080
If U5 = 1 And P = CT And H >= 4 And T <= 120 And J >= 3 And J <= 9 Then GoSub 1080
If U5 = 2 And H >= 4 And T <= 120 And J >= 3 And J <= 9 Then GoSub 1080

Return


1080 '
For J = 0 To 3
    J1 = Int(Rnd(1) * 5)
    If A(P, B1(P, J1), 20) > 0 Then
        B = J1
        Exit For
    End If
Next J

1084 Return

' XXX
' Need to find out what to do with it
'----------------------------------------


'----------------------------------------
'    TeamSelect_PROHOOP Subroutine
'----------------------------------------
'...explanation...
Sub TeamSelect_PROHOOP (teamIndicator)

    Do
        ' ----- Team Selection
        Color 15, 0: Cls
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

        If NY$ = "" And YN$ <> "" Then
            Call TEAMMENU(2, YN$)
        Else
            YN$ = NY$
            Erase teamNames$
            For I = 1 To 500: teamIndex%(I) = 1: Next
            Call TEAMMENU(BO%, YN$)
        End If

        If BO% = 1 Then
            BO% = 0
            '56
        Else
            Locate 2, 33: Color T(P9, 33), T(P9, 34): Print B$(P9, 0)
            Color 15, 4: Locate , 33
            Print "ANY CHANGE (YN)";
            I$ = GetKeyPress$
        End If

    Loop Until UCase$(I$) = "N"

    Color 15, 0

    Call DISABLEPLAYERS

End Sub


'----------------------------------------
'           PlayStyle Subroutine
'----------------------------------------
'...explanation...
Sub PlayStyle ()

    '70
    Call ROSTER

    Color 14: Locate 19, 29: Print "   SELECT OFFENSIVE AND DEFENSIVE STYLE"
    Locate 21, 32: Color 15: Print "O";: Color 3: Print "FFENSE STYLE-";: Color 7: Print offenseStyles$(O0%(P9))
    Locate 22, 32: Color 3: Print "DE";: Color 15: Print "F";: Color 3: Print "ENSE STYLE-";: Color 7: Print defenseStyles$(D2(P9))
    Locate 23, 32: Color 3: Print "FULL COURT ";: Color 15: Print "P";: Color 3: Print "RESS-";: Color 7: Print P7$(P7(P9))

    If C1 = 1 Then

        For M = 0 To 4
            Locate M + 20, 1
            Color 2, 0: Print Using "# "; M;
            Color 15, 0: Print "ENTER "; PS$(M); " # ";
            Z = B1(P9, M)
            CV% = 0

            For IP% = 0 To Z
                If B%(P9, IP%, 18) <> 0 Then CV% = CV% + 1
            Next

            Print AA$(P9, Z, 0);
            Locate Z + 3 - CV%, 3: Color T(P9, 33), T(P9, 34): Print Left$(AA$(P9, Z, 0), 12)
            Color 15, 0
        Next

    Else

        For I = 0 To 4

            Do
                Locate I + 20, 1
                Color 2, 0: Print Using "# "; I;
                Color 15: Print "ENTER "; PS$(I); " # ";

                I$ = GetKeyPress$

                If UCase$(I$) = "O" Or UCase$(I$) = "F" Or UCase$(I$) = "P" Then
                    '100
                    If UCase$(I$) = "P" Then P7(P9) = 1 - P7(P9)
                    If UCase$(I$) = "F" Then D2(P9) = D2(P9) + 1: If D2(P9) > 5 Then D2(P9) = 1
                    If UCase$(I$) = "O" Then O0%(P9) = O0%(P9) + 1: If O0%(P9) > 2 Then O0%(P9) = 0

                    '105
                    Color 14: Locate 19, 29: Print "   SELECT OFFENSIVE AND DEFENSIVE STYLE"
                    Locate 21, 32: Color 15: Print "O";: Color 3: Print "FFENSE STYLE-";
                    Color 7: Print offenseStyles$(O0%(P9))
                    Locate 22, 32: Color 3: Print "DE";: Color 15: Print "F";: Color 3: Print "ENSE STYLE-";
                    Color 7: Print defenseStyles$(D2(P9))
                    Locate 23, 32: Color 3: Print "FULL COURT ";: Color 15: Print "P";: Color 3: Print "RESS-";
                    Color 7: Print P7$(P7(P9))

                Else

                    J = Val(I$)
                    If UCase$(I$) = "A" Then J = 10
                    If UCase$(I$) = "B" Then J = 11
                    If UCase$(I$) = "C" Then J = 12
                    If UCase$(I$) = "D" Then J = 13
                    If UCase$(I$) = "E" Then J = 14

                    If J < 0 Or J > 14 Or A(P9, J, 12) >= 6 Or AA$(P9, J, 0) = "XXX" Or B%(P9, J, 18) <> 0 Then
                        'I didn't want to work out the reverse of the above, or just NOT it
                    Else
                        For I2 = 0 To 4
                            If B1(P9, I2) = J Then Exit For
                        Next I2
                    End If

                End If

                'This terminating condition may not be accurate
                'Need to test during gameplay if 14 is the last choice
            Loop Until J = 14

            B1(P9, I) = J
            Z = J
            CV% = 0
            For IP% = 0 To Z
                If B%(P9, IP%, 18) <> 0 Then
                    CV% = CV% + 1
                End If
            Next

            Print AA$(P9, J, 0);
            Locate J + 3 - CV%, 3: Color T(P9, 33), T(P9, 34): Print Left$(AA$(P9, J, 0), 12)
            Color 15, 0

        Next

    End If

    Do

        Do
            Locate 25, 1
            Color 12, 0: Print "# TO CHANGE (0-4) 6=NONE";
            Locate 1, 1

            I$ = GetKeyPress$
            I1 = Val(I$)

            If UCase$(I$) = "O" Or UCase$(I$) = "F" Or UCase$(I$) = "P" Then
                If UCase$(I$) = "P" Then P7(P9) = 1 - P7(P9) ': GoSub 105
                If UCase$(I$) = "F" Then D2(P9) = D2(P9) + 1: If D2(P9) > 5 Then D2(P9) = 1
                If UCase$(I$) = "O" Then O0%(P9) = O0%(P9) + 1: If O0%(P9) > 2 Then O0%(P9) = 0
            End If

        Loop Until I1 <= 0 And I1 <= 4


        If I1 <> 6 Then

            If X = 50 And I1 = B And P9 = P Then
                Locate 20, 29: Color 14, 0: Print AA$(P9, B1(P9, I1), 0); " IS AT THE FT LINE"
            Else
                Locate 25, 1: Print Space$(25);
                J = B1(P9, I1)
                Z = J

                CV% = 0
                For IP% = 0 To Z:
                    If B%(P9, IP%, 18) <> 0 Then CV% = CV% + 1
                Next

                Locate J + 3 - CV%, 3
                Color 15, 0: Print Left$(AA$(P9, J, 0), 12)

                Do
                    Locate I1 + 20, 1: Print Space$(26);
                    B1(P9, I1) = -1
                    Locate , 1
                    Color 2: Print Using "# "; I1;
                    Color 15: Print "ENTER "; PS$(I1); " # ";

                    I$ = GetKeyPress$

                    If UCase$(I$) = "O" Or UCase$(I$) = "F" Or UCase$(I$) = "P" Then
                        If UCase$(I$) = "P" Then P7(P9) = 1 - P7(P9) ': GoSub 105
                        If UCase$(I$) = "F" Then D2(P9) = D2(P9) + 1: If D2(P9) > 5 Then D2(P9) = 1
                        If UCase$(I$) = "O" Then O0%(P9) = O0%(P9) + 1: If O0%(P9) > 2 Then O0%(P9) = 0
                    Else
                        J = Val(I$)
                        If UCase$(I$) = "A" Then J = 10
                        If UCase$(I$) = "B" Then J = 11
                        If UCase$(I$) = "C" Then J = 12
                        If UCase$(I$) = "D" Then J = 13
                        If UCase$(I$) = "E" Then J = 14

                        If Not (J < 0 Or J > 14 Or A(P9, J, 12) >= 6 Or B%(P9, J, 18) <> 0 Or AA$(P9, J, 0) = "XXX") Then
                            For I2 = 0 To 4:
                                If B1(P9, I2) = J Then Exit For
                            Next I2
                        End If

                    End If

                    'This terminating condition may not be accurate
                    'Need to test during gameplay if 14 is the last choice
                Loop Until J = 14

                B1(P9, I1) = J
                Z = J
                CV% = 0
                For IP% = 0 To Z
                    If B%(P9, IP%, 18) <> 0 Then CV% = CV% + 1
                Next
                Print AA$(P9, J, 0);
                Locate J + 3 - CV%, 3
                Color T(P9, 33), T(P9, 34): Print Left$(AA$(P9, J, 0), 12)
                Color 15, 0

            End If

        End If

    Loop Until I1 = 6

    Cls

End Sub


'----------------------------------------
'       PlayItSafe Subroutine
'----------------------------------------
'...explanation...
Sub PlayItSafe ()
    1200 '
    Cls
    A = 0
    S2 = 1
    S3 = 1
    SW = 0

    For ZZ = 0 To 4
        If A(P9, B1(P9, ZZ), 22) = 50 Then A = A + 1: If SW = 0 Then Print: Print "THE FOLLOWING PLAYER(S) ARE PLAYING SAFE BUT CAN NOW PLAY NORMAL IF YOU WISH": SW = 1
        If A(P9, B1(P9, ZZ), 22) = 50 And SW = 1 And S2 < 6 Then Locate S2 + 3, 1: Print S2; "-"; AA$(P9, B1(P9, ZZ), 0): P5(A) = S2: P4(A) = ZZ: S2 = S2 + 1: S3 = S3 + 1
    Next ZZ

    If SW = 0 Then 1214

    A = 1
    Print
    Print "Z - CONTINUE"

    Do

        Do
            '1206
            I$ = GetKeyPress$

            If UCase$(I$) = "Z" Then
                If MJ = 0 Then 1214
                If MJ = 25 Then 1230
            Else
                P6 = Val(I$)
            End If

        Loop Until P6 <= 1 And P6 <= (S2 + 1)

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
        End If

    Loop Until P6 <> P5(6)


    1214 '
    A = 0: S2 = 1
    For ZZ = 0 To 4
        If H <= 2 And A(P9, B1(P9, ZZ), 12) >= 3 And A(P9, B1(P9, ZZ), 22) = 0 Or A(P9, B1(P9, ZZ), 12) = 5 And A(P9, B1(P9, ZZ), 22) = 0 Then
            A = A + 1
            If S2 = 1 Then
                Cls
                Print
                Print "THE FOLLOWING PLAYER(S) CAN BE MADE TO PLAY SAFE IF YOU WISH"
                Print
            End If
        End If
        If A = 0 And ZZ > 3 Then
            skipKey% = 1
        Else
            If S2 = A And S2 < 6 Then
                Print S2; "- "; AA$(P9, B1(P9, ZZ), 0)
                P5(A) = S2
                P4(A) = ZZ
                S2 = S2 + 1
                S3 = S3 + 1
            End If
        End If
    Next ZZ

    If skipKey% = 0 Then
        A = 1
        Print
        Print "Z - CONTINUE"

        Do
            I$ = GetKeyPress$

            P6 = Val(I$)

            If UCase$(I$) <> "Z" Then

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
                End If

            End If

        Loop Until P6 >= 1 And P6 <= (S2 + 1)
    End If

    1230 '
    Cls
    MJ = 0
    FO = 99
    If H = 1 And T = 720 Then FO = 0

End Sub


'----------------------------------------
'       CheckZeroTime Subroutine
'----------------------------------------
'...explanation...
Sub CheckZeroTime ()

    If T <= 0 Then
        TE = T
        Call AddMinutes
        T = 0
        Call UPDATESCREEN
    End If

End Sub


'----------------------------------------
'       AddMinutes Subroutine
'----------------------------------------
'...explanation...
Sub AddMinutes ()

    ' *** INCREMENT MINUTES ***
    For I3 = 0 To 1
        For I4 = 0 To 4
            ZZ(I3, B1(I3, I4)) = ZZ(I3, B1(I3, I4)) + TE
        Next
    Next

End Sub


'----------------------------------------
'           SomeCalcs Subroutine
'----------------------------------------
'...explanation...
Sub SomeCalcs

    M1 = 0
    M2 = 0
    M3 = 0
    M4 = 0

    If Not (U5 = 1 And P9 = CT And X = 50 Or U5 = 2 And P9 = P And X = 50) Then

        For I1 = 0 To 14

            M6 = A(P9, I1, 3) - A(P9, I1, 7) - A(P9, I1, 11) - A(P9, I1, 12)

            If H < 4 And M6 <= 0 Or H = 4 And T > 120 And M6 <= 0 Then M1 = M1 + 1

            If AA$(P9, I1, 0) = "XXX" Or A(P9, I1, 12) >= 6 Or B%(P9, I1, 18) <> 0 Or H = 1 And A(P9, I1, 12) >= 2 Or H = 2 And A(P9, I1, 12) >= 4 Or H = 3 And A(P9, I1, 12) >= 5 Or H = 4 And T > 300 And A(P9, I1, 12) = 5 Then M2 = M2 + 1

            If I1 <= 4 Then
                M6 = A(P9, B1(P9, I1), 3) - A(P9, B1(P9, I1), 7) - A(P9, B1(P9, I1), 11) - A(P9, B1(P9, I1), 12)

                If M6 <= 0 And H < 4 Or M6 <= 0 And H = 4 And T > 120 Then M3 = M3 + 1

                If AA$(P9, B1(P9, I1), 0) = "XXX" Or A(P9, B1(P9, I1), 12) >= 6 Or B%(P9, B1(P9, I1), 18) <> 0 Or H = 1 And A(P9, B1(P9, I1), 12) >= 2 Or H = 2 And A(P9, B1(P9, I1), 12) >= 4 Or H = 3 And A(P9, B1(P9, I1), 12) >= 5 Or H = 4 And T > 300 And A(P9, B1(P9, I1), 12) = 5 Then M4 = M4 + 1

            End If
        Next I1

        If M3 >= 1 And 5 - M3 < 15 - M1 Or H = 4 And T < 120 Or H > 4 Or M4 >= 1 And M2 < 14 Then Call COMPUTERSUBS

    End If

    If H = 4 And T <= 120 Then G9%(P9) = 1

End Sub


'----------------------------------------
'       CreditAssists Subroutine
'----------------------------------------
'...explanation...
Sub CreditAssists

    1054 'CREDIT ASSISTS
    R0 = Int(Rnd(1) * 100) + 1

    If R0 > F7%(P, 7) Then

        1055 '
        R0 = Int(Rnd(1) * F5%(P, 2)) + 1

        W5 = 0

        For M8 = 0 To 4
            W5 = W5 + A(P, B1(P, M8), 25)
            If R0 <= W5 Then 1059
        Next M8

        M8 = Int(Rnd(1) * 5)

        1059 '
        W5 = M8
        Locate 7, 25

        If AA$(P, B1(P, B), 0) <> AA$(P, B1(P, M8), 0) Then
            A(P, B1(P, M8), 14) = A(P, B1(P, M8), 14) + 1
        Else
            GoTo 1055
        End If

    End If

End Sub


'----------------------------------------
'    ReboundRoutine1 Subroutine
'----------------------------------------
'...explanation...
Sub ReboundRoutine1

    790 '
    RB = 0

    For I = 0 To 4:

        RB(I) = 0

        '--- I believe this is the only line different from ReboundRoutine2 !!!
        'I assume the P refers to a team indicator, so 1-P is the opposite team
        'That is, the difference was intended to be defensive/offensive
        XZ = A(P, B1(P, I), 21)

        If Not (H > 2 Or H = 2 And T <= 120) Then
            If A(P, B1(P, I), 3) - A(P, B1(P, I), 7) - A(P, B1(P, I), 11) - A(P, B1(P, I), 12) <= 0 Then XZ = XZ * .5
        End If

        RB(I) = RB(I) + XZ
        RB = RB + XZ
    Next

    RN = Rnd(1) * RB

    For B = 0 To 4
        If RN <= RB(B) Then
            skipRandom% = 1
            Exit For
        Else
            RB(B + 1) = RB(B + 1) + RB(B)
        End If
    Next

    If skipRandom% = 0 Then
        B = Int(Rnd(1) * 5)
    End If

End Sub


'----------------------------------------
'    ReboundRoutine2 Subroutine
'----------------------------------------
'...explanation...
Sub ReboundRoutine2

    795 '
    RB = 0

    For I = 0 To 4
        RB(I) = 0
        XZ = A(1 - P, B1(1 - P, I), 2)

        If Not (H > 2 Or H = 2 And T <= 120) Then
            If A(1 - P, B1(1 - P, I), 3) - A(1 - P, B1(1 - P, I), 7) - A(1 - P, B1(1 - P, I), 11) - A(1 - P, B1(1 - P, I), 12) <= 0 Then XZ = XZ * .5
        End If

        RB(I) = RB(I) + XZ
        RB = RB + XZ
    Next

    RN = Rnd(1) * RB

    For B = 0 To 4
        If RN <= RB(B) Then
            skipRandom% = 1
            Exit For
        Else
            RB(B + 1) = RB(B + 1) + RB(B)
        End If
    Next

    If skipRandom% = 0 Then
        B = Int(Rnd(1) * 5)
    End If

End Sub


'----------------------------------------
'       Choices Subroutine
'----------------------------------------
'...explanation...
Sub Choices ()

    810 '
    NL% = 1
    A1$ = ""
    B1$ = ""
    C1$ = ""
    D1$ = ""
    E1$ = ""
    F1$ = ""
    G1$ = ""
    H1$ = ""

    If U5 <> 0 Then
        If U5 = 2 Then
            P9 = 0
            Call SomeCalcs
            P9 = 1
            Call SomeCalcs
        End If

        If U5 = 1 Then
            P9 = CT
            Call SomeCalcs
        End If
    End If

    If U5 = 2 Or U5 = 1 And P = CT Then
        S2 = 0
        P9 = P
        C1 = 2
        If T <= 0 Then TE = T: Call AddMinutes
    Else

        If pbpDelay <> 0 Then
            For I = 8 To 15
                Locate I, 1
                Print Space$(38)
            Next I
        End If

        Locate 10
        Color 15, 0
        Print "<ENTER>-CONTINUE"
        Print "1-TIMEOUT "; B$(P, 0); "-"; T1(P)

        Do

            813 I$ = GetKeyPress$
            P3 = Val(I$)

            If P3 <> 1 Then
                S2 = 0
                P9 = P
                C1 = 2
                If T <= 0 Then TE = T: Call AddMinutes
                Exit Do
            End If

        Loop Until Not (P3 = 1 And T1(P) <= 0 Or P3 = 1 And P = CT And U5 = 1)

        'If P3 = 1 And T1(P) <= 0 Or P3 = 1 And P = CT And U5 = 1 Then 813

        If P3 = 1 Then T1(P) = T1(P) - 1: Color 15, 0: Call Substitutions

        818 '
        If pbpDelay <> 0 Then
            For I = 8 To 15
                Locate I, 1
                Print Space$(38)
            Next I
        End If

        Locate 10

    End If

End Sub


'----------------------------------------
'       Substitutions Subroutine
'----------------------------------------
'...explanation...
Sub Substitutions ()

    Do

        220 '
        NL% = 1
        A1$ = ""
        B1$ = ""
        C1$ = ""
        D1$ = ""
        E1$ = ""
        F1$ = ""
        G1$ = ""
        H1$ = ""

        If U5 = 2 Then
            P9 = 0
            Call SomeCalcs
            P9 = 1
            Call SomeCalcs
        End If

        If U5 = 1 Then
            P9 = CT
            Call SomeCalcs
        End If

        FO = 0

        If pbpDelay <> 0 Then
            For I = 8 To 15
                Locate I, 1
                Print Space$(38)
            Next I
        End If

        Locate 10

        If U5 = 2 Then

            S2 = 0: P9 = P: C1 = 2

            If T <= 0 Then TE = T: Call AddMinutes

            Call RATINGTABLES

        Else

            Locate 10, 1: Color 15, 0
            Print "<ENTER>-CONTINUE"
            Print "1-SUB "; B$(P, 0)
            Print "2-SUB "; B$(D, 0)

            Do
                I$ = GetKeyPress$
                P3 = Val(I$)
            Loop Until P3 >= 1 And P3 <= 2 Or P3 = 13

            If P3 < 1 Or P3 > 2 Or U5 = 1 And P3 = 1 And P = CT Or U5 = 1 And P3 = 2 And D = CT Then

                S2 = 0
                P9 = P
                C1 = 2

                If T <= 0 Then TE = T: Call AddMinutes

                Call RATINGTABLES

                If LC% = 1 Then

                    LC% = 0
                    Call CheckZeroTime
                    Call NEWSCREEN

                End If

                If pbpDelay <> 0 Then

                    For I = 8 To 15
                        Locate I, 1
                        Print Space$(38)
                    Next I

                End If

            Else

                If P3 = 1 Then S2 = P
                If P3 = 2 Then S2 = D

                C1 = 1
                P9 = S2

                Call PlayStyle
                Call PlayItSafe

                P9 = P
                LC% = 1

            End If 'Done checking parameters

        End If 'Done checking computer control

    Loop Until LC% = 0 Or U5 = 2

End Sub


'----------------------------------------
'       Fouls Subroutine
'----------------------------------------
'...explanation...
Sub Fouls (skipRandom%)


    If skipRandom% = 0 Then
        630 '
        W5 = 0
        M9 = Int(Rnd(1) * F5%(D, 0)) + 1

        For M8 = 0 To 4: W5 = W5 + A(D, B1(D, M8), 17)
            If M9 <= W5 Then
                Exit For
            End If
        Next M8

        If M8 < 0 Or M8 > 4 Then M8 = Int(Rnd(1) * 5)
        W5 = M8
    End If

    631 '
    A(D, B1(D, W5), 12) = A(D, B1(D, W5), 12) + 1
    P2(D) = P2(D) + 1

    Call UPDATESCREEN

    If A(D, B1(D, W5), 3) - A(D, B1(D, W5), 7) - A(D, B1(D, W5), 11) - A(D, B1(D, W5), 12) = 0 Then A(D, B1(D, W5), 3) = A(D, B1(D, W5), 3) + 1

    If A(D, B1(D, W5), 12) < 6 Then

        If PB% = 1 And pbpDelay <> 0 Then
            Call WINDEX
            Print "FOUL ON "; AA$(D, B1(D, W5), 0)
            Call MOREDELAY
            Print "HE HAS "; A(D, B1(D, W5), 12)
            Call MOREDELAY
        End If

        If PB% = 0 Then U$ = "FOUL NUMBER" + Str$(A(D, B1(D, W5), 12)) + " CALLED ON " + AA$(D, B1(D, W5), 0): Call DELAY

        Call MOREDELAY

    Else
        'A(D, B1(D, W5), 12) >= 6 Then
        M9 = 0

        For I3 = 0 To 14
            If I3 <> B1(D, W5) And AA$(D, I3, 1) <> "XXX" And B%(D, I3, 18) = 0 Then
                M9 = M9 + 1
            End If
        Next

        For I3 = 0 To 14
            If I3 <> B1(D, W5) And AA$(D, I3, 1) <> "XXX" And B%(D, I3, 18) = 0 Then
                If A(D, I3, 0) <= A(D, B1(D, W5), 0) Then A(D, I3, 3) = A(D, I3, 3) + Int((A(D, B1(D, W5), 3) / M9) + .5)
            End If
        Next I3

        If PB% = 1 And pbpDelay <> 0 Then Call WINDEX: Print "FOUL ON "; AA$(D, B1(D, W5), 0): Call MOREDELAY
        If PB% = 0 Then U$ = "FOUL CALLED ON " + AA$(D, B1(D, W5), 0): Call DELAY
        U$ = "HE HAS JUST FOULED OUT !!!"

        If PB% = 0 Then Call DELAY
        If PB% = 1 And pbpDelay <> 0 Then Print U$: Call MOREDELAY

        Call MOREDELAY
        C9 = 9

        If P2(D) > NB% Then X = 50

        P3 = 1

        If U5 = 0 Or U5 = 1 And D = 1 - CT Then
            P9 = D: C1 = 1
            Call PlayStyle
            Call PlayItSafe
            Call Substitutions
            Call NEWSCREEN
        End If

        If T <= 0 Then TE = T: Call AddMinutes

        X = 0

    End If

End Sub


'----------------------------------------
'       SomethingTime Subroutine
'----------------------------------------
'...explanation...
Sub SomethingTime

    210 '

    M5 = 0

    If T <= 2 Or IN = 99 Then
        M5 = 2
    Else
        If T > 2 And T <= 4 Then M5 = 2
        If T > 4 And T <= 16 Then M5 = Int(Rnd(1) * 2) + 3
        If T > 16 Then M5 = Int(Rnd(1) * 2) + 4: 'CHANGED AFTER DEF RB CHANGE FROM +4 TO +3
    End If

    TE = M5

    Call AddMinutes

    T = T - M5
    IN = 0
    X8 = X8 - M5

    If T <= .001 Then TE = T: Call AddMinutes: T = 0

    If X8 <= 0 Then X8 = 0

    Call UPDATESCREEN

End Sub


'----------------------------------------
'          BoxToScreen Subroutine
'----------------------------------------
'...explanation...
Sub BoxToScreen

    8515 '
    Call BOGUS(X)

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

        8529 Next I1

        If V9(3) = 0 Then V9(3) = 1

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

    If H = 3 Then
        For I = 0 To 1
            For I1 = 0 To 14
                If A(I, I1, 22) = 50 Then A(I, I1, 22) = 0: A(I, I1, 17) = A(I, I1, 17) * 3: D1(I) = D1(I) - 1: MJ = 50
            Next I1
            If MJ = 50 Then
                MJ = 0
                F5%(I, 0) = 0
                For I1 = 0 To 4: F5%(I, 0) = F5%(I, 0) + A(I, B1(I, I1), 17): Next I1
            End If
        Next I
    End If

End Sub


'$INCLUDE: 'PadRight.BM'
'$INCLUDE: 'GameRoutines.BM'
'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'

