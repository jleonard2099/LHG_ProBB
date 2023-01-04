$Debug
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
'$INCLUDE: 'PadRight.BI'
'$INCLUDE: 'GameRoutines.BI'

On Error GoTo Errhandler

'----------------------------------------
'           GAME CODE
'----------------------------------------
_Title "Basketball: The Pro Game - HOOPS!"
$ExeIcon:'./lhg_probb.ico'
_Icon
_AllowFullScreen

Color 7, 0
Cls

Randomize Timer

'----------------------------------------
'          READ IN ALL DATA
'----------------------------------------

Data L,1,1,1,2,2,2,H,H,3,3,3,4,4,F

For I = 0 To 14
    Read R$(I)
Next

Data 60,61,60,58,58,59,59,59,59,58,58,58,57,58,57,51,51,52,49,50,44,45,42,47,43,43,42,39,38,38,35,34,34

For I = 0 To 32
    Read TOA%(I)
Next

Data 9,8,8,8,8,8,8,9,8,9,8,8,7,7,06,07,07,07,08,08,08,08,08,08,08,09,09,09,09,09,09,09,08

For I = 0 To 32
    Read PFA%(I)
Next

Data "REG M-M     ","SAGGING M-M ","TRAPPING M-M","TIGHT M-M   ","LOOSE M-M   "
For I = 1 To 5
    Read defenseStyles$(I)
Next I

Data -2,-2,-3,-4,-4,-5,-5,-6,-7
Data -4,-5,-5,-6,-7,-8,-9,-10,-11
Data 0,0,-1,-2,-3,-4,-5,-5,-6
Data -3,-3,-4,-5,-6,-6,-7,-8,-9
Data -6,-7,-8,-8,-8,-9,-10,-11,-12

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

'----------------------------------------
'    INITIALIZE ALL VARIABLES
'----------------------------------------

For I = 0 To 1
    For I1 = 0 To 4
        B1!(I, I1) = -1
    Next I1
Next I

NL% = 1
SC(1) = 30
SC(0) = 24
NB% = 5

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
'       DETERMINE GAME OPTIONS
'----------------------------------------

If Not _FileExists("DEV.BAS") Then
    Open "DEV.BAS" For Output As #1
    For I = 0 To 1
        Print #1, _CWD$ + "\"
    Next
    Close 1
End If

Open "DEV.BAS" For Input As #1
For I = 0 To 1
    Input #1, diskPaths$(I)
Next
Close 1


If Not _FileExists("DEFAULT") Then
    Open "DEFAULT" For Output As #1
    For I = 1 To 12:
        Print #1, 0
    Next
    Close 1
End If

Open "DEFAULT" For Input As #1
Input #1, U5
Input #1, XY%
Input #1, SC
Input #1, N
Input #1, PO
Input #1, pbpDelay!
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


'Load game in-progress, if there is one
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

    VS% = CVI(Q$(22))
    HS% = CVI(Q$(23))

    Close 1

    AP = 1
End If

If AP = 1 Then
    'AUTO-PLAY OPTIONS
    XY% = NG%(3)
    U5 = NG%(8)
    PO = NG%(5)
    SC = NG%(7)
    N = NG%(2)
    pbpDelay! = NG%(4) / 10
    CM% = NG%(9)
    NB% = NG%(14): FR% = NG%(11): RT% = NG%(10)
    For P9 = 0 To 1
        I1 = NG%(P9)
        YN$ = YN$(P9)
        Call GETATEAM(I1, YN$)
    Next P9

    Call SETUPRATINGS

Else

    N = 1
    ' USER SELECTION OF OPTIONS

    Do
        Color 15, 0
        Cls
        Color 2

        Print Tab(29); "SELECT OPENING OPTIONS"
        Print
        Locate 2, 2: Color 14
        Print "(1) ";: Color 15: Print U5$(U5)
        Locate 3, 2: Color 14
        Print "(2) ";: Color 15: Print "VISITING TEAM: ";: Color T(0, 33), T(0, 34): Print B$(0)
        Color , 0
        Locate 4, 2: Color 14
        Print "(3) ";: Color 15: Print "HOME TEAM: ";: Color T(1, 33), T(1, 34): Print B$(1)
        Color , 0
        Locate 5, 2: Color 14
        Print "(4) ";: Color 15: Print "3-POINT SHOT: "; TS$(XY%)
        Locate 6, 2: Color 14
        Print "(5) ";: Color 15: Print SC$(SC)
        Locate 7, 2: Color 14
        Print "(6) ";: Color 15: Print "LOCATION: "; LC$(N)
        Locate 8, 2: Color 14
        Print "(7) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#"; pbpDelay!
        Locate 9, 2: Color 14
        Print "(8) ";: Color 15: Print "START GAME"
        Locate 10, 2: Color 14
        Print "(9) ";: Color 15: Print PO$(PO)
        Locate 11, 2: Color 14
        Print "(0) ";: Color 15: Print "SAVE SETTINGS"
        Locate 12, 2: Color 14
        Print "(A) ";: Color 15: Print "SHOW SHOT PERCENTAGES: "; P7$(X4%)
        Locate 13, 2: Color 14
        Print "(B) ";: Color 15: Print "HUMAN COACHES WILL: "; CM$(CM%);
        Locate 1, 1
        Locate 14, 2: Color 14
        Print "(C) ";: Color 15: Print "BONUS AFTER FOUL # "; NB%
        Locate 15, 2: Color 14
        Print "(D) ";: Color 15: Print "NUMBER OF FREE THROWS "; FT$(RT%)
        Locate 16, 2: Color 14
        Print "(E) ";: Color 15: Print "FREE THROW RULES "; FR$(FR%)
        Locate 17, 2: Color 14
        Print "(F) ";: Color 15: Print "PLAY BY PLAY "; PB$(PB%)
        Locate 18, 2: Color 14
        Print "NOTE:: ";: Color 15: Print "BONUS WAS ON 7TH FOUL (UNTIL 72-73), 6TH FOUL (72-73 SEASON)"
        Locate , 2: Print "       5TH FOUL (1973-74 TO PRESENT)"
        Locate , 2: Color 14
        Print "NOTE:: ";: Color 15: Print "UP UNTIL 1972-73, WHEN A TEAM WAS IN THE BONUS, THEY HAD THE CHANCE"
        Locate , 2: Print "       TO SHOOT 3 FT TO MAKE 2 OR 2 FT TO MAKE 1"
        Locate , 2: Color 14
        Print "NOTE:: ";: Color 15: Print "UP UNTIL 1972-73, ALL FOULS WERE SHOT...UNLIKE THE CURRENT RULES"
        Locate , 2: Color 14
        Print "NOTE:: ";: Color 15: Print "THE 3-PT SHOT WAS USED FOR ALL ABA SEASONS AND NBA SINCE 1979-80"
        Locate , 2: Color 14
        Print "NOTE:: ";: Color 15: Print "THE ABA USED A 30-SEC SHOT CLOCK THROUGH 1973-74";

        'Locate 18, 2: Color 14
        'Print "NOTE:: "
        'Locate , 2: Color 15: Print "       BONUS ON 7TH FOUL UNTIL 72-73, 6TH FOUL (72-73 SEASON), 5TH FOUL (1973-74 ON)"
        'Locate , 2: Color 15: Print "       UNTIL 1972-73, WHEN A TEAM WAS IN BONUS, THEY HAD A CHOICE TO"
        'Locate , 2: Color 15: Print "              SHOOT 3 FT TO MAKE 2, OR 2 FT TO MAKE 1"
        'Locate , 2: Color 15: Print "       UNTIL 1972-73, ALL FOULS WERE SHOT"
        'Locate , 2: Color 15: Print "       3-PT SHOT WAS USED FOR ALL ABA SEASONS AND NBA SINCE 1979-80"
        'Locate , 2: Color 15: Print "       ABA USED A 30-SEC SHOT CLOCK THROUGH 1973-74";

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

        Loop Until I >= 0 And I <= 16 Or I$ = Chr$(27) Or UCase$(I$) = "Q"

        If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

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
                        Input pbpDelay!
                    Loop Until pbpDelay! >= 0

                Case 8:
                    If B$(0) <> "" And B$(1) <> "" Then Exit Do

                Case 9:
                    PO = 1 - PO

                Case 10:
                    Open "DEFAULT" For Output As #1
                    Print #1, U5
                    Print #1, XY%
                    Print #1, SC
                    Print #1, N
                    Print #1, PO
                    Print #1, pbpDelay!
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

    Loop Until (I = 8 And B$(0) <> "" And B$(1) <> "") Or I$ = Chr$(27)

    If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

        'TICKER PROMPT
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

    Else

        End
        System

    End If

End If

' --> we never do anything with ESC
'   in the original it exits back out

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

Call LOADSTATS

'900
If AP = 1 Then N = NG%(2)

X8 = SC(SC)
C7 = 1
LS = 0

If T < 6 And LT = 4 Then LS = 4

'Determine subs / play parameters
'230
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

'GoSub 3000
'AKA ChangePosession
PB$ = ""
If P = 1 Then
    P = 0: D = 1: P9 = 0: C1 = 2
Else
    'P = 0
    P = 1: D = 0: P9 = 1: C1 = 2
End If

'GoSub 210
Call LastShot

'TIMEEXP
If T <= 0 Then TE = T: Call PlayingTime: T = 0: GoTo 170

X8 = SC(SC)
LS = 4

Call NEWSCREEN

'----------------------------------------
'           Tip Off!
'----------------------------------------

If H = 1 Or H > 4 Then
    If PB% = 0 Then
        U$ = B$(JB) + " GETS THE JUMP": Call DELAY
    Else
        'PB% = 1
        If pbpDelay! <> 0 Then
            U$ = " GETS THE JUMP"
            Call WINDEX
            Color T(JB, 33), T(JB, 34): Print B$(JB);
            Color 15, 0
            Print U$
            Call MOREDELAY
        End If
    End If
Else
    If PB% = 0 Then
        U$ = B$(JB) + " INBOUNDS THE BALL": Call DELAY
    Else
        'PB% = 1
        If pbpDelay! <> 0 Then
            U$ = " INBOUNDS THE BALL"
            Call WINDEX
            Color T(JB, 33), T(JB, 34): Print B$(JB);
            Color 15, 0
            Print U$
            Call MOREDELAY
        End If
    End If
End If


P9 = JB
P = JB

GoTo 300

'----- NOODLE -----
170 '
T = 0
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
    Case 1: If pbpDelay! > 0 Then Call WINDEX: Print U$ ': Call MOREDELAY
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
        Case 1: If pbpDelay! > 0 Then Call WINDEX: Print U$ ': Call MOREDELAY
    End Select
End If

Locate 2, 31: Print "END OF REGULATION"

If S!(0, 0) = S!(1, 0) Then
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

        Case 2: Call BOX2FILE
        Case 1: Call BOX2PRN
        Case 0:
            Cls
            If N <> 2 Then Color T(N, 33), T(N, 34): Print "@ "; S$(N): Print: Color 14, 0

            For I = 0 To 1: Print B$(I); Tab(17); Str$(S!(I, 1)); Tab(20); Str$(S!(I, 2)); Tab(23); Str$(S!(I, 3)); Tab(26); Str$(S!(I, 4));

                If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) Then
                    Print Tab(29); Str$(S!(I, 5));
                End If

                If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) <> S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) Then
                    Print Tab(29); "- "; S!(I, 0)
                Else

                    If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) + S!(0, 5) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) + S!(1, 5) Then
                        Print Tab(32); Str$(S!(I, 6));
                    End If

                    If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) + S!(0, 5) <> S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) + S!(1, 5) Then
                        Print Tab(32); "- "; S!(I, 0)
                    Else
                        Print Tab(35); "- "; S!(I, 0)
                    End If
                End If

            Next I

            Call BoxToScreen

    End Select

    8532 X = 0
    Cls

    If AP <> 1 Or U5 <> 2 Then

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

        Do
            Locate 9, 26
            Print "DISPLAY STATS AGAIN (YN)";
            Color 15, 0
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


If AP <> 1 Then

    Do
        Locate 9, 30

        ' Prompt to play another game
        U$ = "PLAY ANOTHER GAME (YN)"
        Locate , (80 - Len(U$)) / 2
        Print U$

        Color 15, 0
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If UCase$(I$) = "N" Then
        End
        System

    End If

End If

Cls
Print "PLEASE WAIT ..."
Run "PROHOOP.EXE"

'----------------------------------------
'           END OF GAME!!!
'----------------------------------------

'================================================================================

Errhandler:
Open "errlog" For Append As #9
Print #9, "Error occurred! " '; Date$; " "; Time$
Print #9, "Error #"; Err; "on program file line"; _ErrorLine
Print #9, ""
Close #9
Resume Next ' moves program to code following the error.

'================================================================================
' Below are all the different GoTo branches for the gameplay
' These have not yet been worked out into a different flow
' yet for the game's logic.
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

    'GoSub 835
    If P = 0 Then D = 1
    If P = 1 Then D = 0
    If T < 6 And LT <> 4 Then LS = 4

End If

'302
D6 = 0
PASS% = 1
A4 = 0
A6 = 0
F = 0

If X <> 90 Then X = 0

GoTo 400

'----- NOODLE -----

350 PASS% = PASS% + 1
P6 = 0
A4 = 0
D6 = 0
A6 = 1
F = 0
X = 0

GoTo 400

'----- NOODLE -----

359 S = Int(Rnd(1) * 100) + 1
W%(P, B1!(P, B), 1) = W%(P, B1!(P, B), 1) + 1
A!(P, B1!(P, B), 7) = A!(P, B1!(P, B), 7) + 1
PT% = 3

Call MAKEPBP
Call DELAY

If S <= F3S% Then
    363 ' MADE 3-POINT FIELD GOAL

    If PB% = 1 And pbpDelay! <> 0 Then
        Call WINDEX
        Print "3 PT FG GOOD BY "; AA$(P, B1!(P, B), 0); " !!!"
        Call MOREDELAY
    End If

    If PB% = 0 Then
        PT% = 4
        Call MAKEPBP
        Call DELAY
    End If

    Call MOREDELAY

    S!(P, H) = S!(P, H) + 3
    S!(P, 0) = S!(P, 0) + 3
    W%(P, B1!(P, B), 0) = W%(P, B1!(P, B), 0) + 1
    A!(P, B1!(P, B), 8) = A!(P, B1!(P, B), 8) + 1

    Call CreditAssists
    Call UPDATESCREEN

    GoTo 562

End If

If PB% = 1 And pbpDelay! <> 0 Then
    Call WINDEX
    Print "3 PT FG MISSED BY "; AA$(P, B1!(P, B), 0)
    Call MOREDELAY
End If

If PB% = 0 Then
    PT% = 5
    Call MAKEPBP
    Call DELAY
End If

X8 = SC(SC)
LS = 4
T = T - 4
TE = 4

Call PlayingTime
Call CheckZeroTime: If T = 0 Then GoTo 170
Call UPDATESCREEN

GoTo 670

'----- NOODLE -----

400 '
A4 = 0
D6 = 0
P3 = 0

B = Int(Rnd(1) * 5)

'--- comparing here

405 '
R0 = Int(Rnd(1) * 100) + 1

If U5 = 0 And CM% = 1 Or U5 = 1 And D = CT And CM% = 1 Or U5 = 1 And P = CT Or U5 = 2 Then

    CNT% = 0

    While (CNT% < 4)

        CNT% = CNT% + 1

        If CNT% <> 4 Then

            M9 = 0
            B = Int(Rnd(1) * 5)
            X = Int(Rnd(1) * 100) + 1

            If X > 30 Then

                For I = 0 To 4
                    M8 = Int(B%(P, B1!(P, I), 2) / B%(P, B1!(P, I), 0) + .5) - A!(P, B1!(P, I), 7)
                    If M8 <= 0 Then M8 = 0
                    If M8 <= 0 And A!(P, B1!(P, I), 3) - A!(P, B1!(P, I), 7) - A!(P, B1!(P, I), 11) - A!(P, B1!(P, I), 12) > 0 Then M8 = A!(P, B1!(P, I), 3) - A!(P, B1!(P, I), 7) - A!(P, B1!(P, I), 11) - A!(P, B1!(P, I), 12)
                    If M8 >= M9 Then B = I: M9 = M8
                Next I

            End If

        End If

    Wend

Else

    If R0 <= 40 Then

        Do
            424 '
            If pbpDelay! <> 0 Then
                If PB% = 0 Then
                    U$ = "PASS TO CHOICE (0-4)": Call DELAY
                Else
                    PB% = 1

                    If Not (AP = 1 And U5 = 2 Or pbpDelay! = 0) Then
                        Call WINDEX
                        Print "PASS TO CHOICE (0-4)"
                        Call DELAY
                    End If

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

'427 = Return

If P1 <> 1 Then

    R0 = Int(Rnd(1) * 1000) + 1

    ' *** BRANCH TO AREA BASED ON DEFENSE ***
    Select Case D2(D)

        Case 1, 4:
            461 '*** SHOT VS REG/TIGHT M-M ***

            If R0 <= 2 Then
                X = 0
                GoTo 580
            End If

            If R0 <= 30 Then
                Call LastShot
                X8 = SC(SC)
                LS = 4
                Call UPDATESCREEN
                X = 88

                GoTo 565
            End If

            If R0 <= 30 + F7%(P, 1) + F7%(D, 2) Then
                GoTo 515
            End If

            '-----

            P9 = Int(Rnd(1) * 5)

            If R0 <= 24 + F7%(P, 1) + F7%(D, 2) + A!(D, B1!(D, P9), 5) + F7%(P, 0) + OX%(O0%(P)) Then
                GoTo 513
            Else

                R0 = Int(Rnd(1) * 1000) + 1

                If (PASS% <= 3 Or PASS% > 4) And R0 <= 320 + ((JY - 89) * 10) Then 445
                If PASS% = 4 Then 445
                GoTo 437

            End If

        Case 2, 5
            477 '*** SHOT VS LOOSE/SAG M-M ***

            If R0 <= 1 Then
                X = 0
                GoTo 580
            End If

            If R0 <= 10 Then
                Call LastShot
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

            If R0 <= 8 + F7%(P, 1) + F7%(D, 2) + A!(D, B1!(D, P9), 5) + F7%(P, 0) + OX%(O0%(P)) Then
                GoTo 513
            Else
                R0 = Int(Rnd(1) * 1000) + 1

                If (PASS% <= 3 Or PASS% > 4) And R0 <= 320 + ((JY - 89) * 10) Then 445
                If PASS% = 4 Then 445
                GoTo 437
            End If

        Case 3
            494 '*** SHOT VS TRAP M-M ***

            If R0 <= 3 Then
                X = 0
                GoTo 580
            End If

            If R0 <= 50 Then
                Call LastShot
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

            If R0 <= 40 + F7%(P, 1) + F7%(D, 2) + A!(D, B1!(D, P9), 5) + F7%(P, 0) + OX%(O0%(P)) Then
                GoTo 513
            Else
                R0 = Int(Rnd(1) * 1000) + 1

                If (PASS% <= 3 Or PASS% > 4) And R0 <= 320 + ((JY - 89) * 10) Then 445
                If PASS% = 4 Then 445
                GoTo 437
            End If

    End Select

Else
    'P1 = 1

    428 '
    P1 = 0

    R0 = Int(Rnd(1) * 1000) + 1

    If R0 <= 3 Then
        X = 0
        GoTo 580
    End If

    If R0 <= 50 Then
        'GoTo 512
        Call LastShot
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

    If R0 <= 40 + F7%(P, 1) + F7%(D, 2) + A!(D, B1!(D, P9), 5) + F7%(P, 0) Then
        GoTo 513
    Else
        R0 = Int(Rnd(1) * 1000) + 1

        If (PASS% <= 3 Or PASS% > 4) And R0 <= 320 + ((JY - 89) * 10) Then 445
        If PASS% = 4 Then 445
        GoTo 437
    End If

End If

'----- NOODLE -----
' ***BAD SHOT ***

437 A4 = 0

R0 = Int(Rnd(1) * 3) + 1

If A!(P, B1!(P, B), 0) > 60 And R0 = 1 Then F = 24
If A!(P, B1!(P, B), 0) > 60 And R0 = 2 Then F = 30
If A!(P, B1!(P, B), 0) > 60 And R0 = 3 Then F = 36
If A!(P, B1!(P, B), 0) <= 60 And R0 = 1 Then F = A!(P, B1!(P, B), 0) * .4
If A!(P, B1!(P, B), 0) <= 60 And R0 = 2 Then F = A!(P, B1!(P, B), 0) * .5
If A!(P, B1!(P, B), 0) <= 60 And R0 = 3 Then F = A!(P, B1!(P, B), 0) * .6

PZ% = 1
X = 0

GoTo 520

'----- NOODLE -----

445 '
If Not (H = 4 And T <= 120 Or H > 4) Then
    If A!(P, B1!(P, B), 3) - A!(P, B1!(P, B), 7) - A!(P, B1!(P, B), 11) - A!(P, B1!(P, B), 12) <= 0 Then 437
End If

446 '*** CHECKING 3-POINT SHOT ***
If XY% <> 0 Then

    F3S% = 0

    RN = Int(Rnd(1) * 100) + 1

    If RN <= A!(P, B1!(P, B), 19) Then
        PZ% = 4
        F3S% = A!(P, B1!(P, B), 20) + F7%(D, 4)
        F3S% = F3S% + Int(Rnd(1) * 7) - 3
        X = 0
        GoTo 520
    End If

    If P6 = 4 Then
        If Int(Rnd(1) * 100) + 1 <= 20 Then
            GoTo 437
        Else
            PZ% = 4
            X = 0
            F3S% = .4 * A!(P, B1!(P, B), 20) + F7%(D, 4)
            GoTo 520
        End If
    Else
        If H >= 4 And T < 30 And S!(D, 0) - S!(P, 0) > 3 And A!(P, B1!(P, B), 19) >= 3 Then
            PZ% = 4
            X = 0
            F3S% = A!(P, B1!(P, B), 20) + F7%(D, 4)
            GoTo 520
        End If

    End If

End If

448 '*** CHECKING FOR ASSISTED SHOT ***

'*** GET SHOT PCT ***
F = A!(P, B1!(P, B), 0) + D1(D)

If N = P Then F = F + 2
If N <> P And N <> 2 Then F = F - 2

A4 = Int(Rnd(1) * 41)
R0 = Int(Rnd(1) * F5%(P, 4)) + 1
RN = Int(.17 * F5%(P, 4) + .5)

If RN >= R0 Then
    F = F + A4
    PZ% = 2
    X = 0
Else

    '*** CHECK PENETRATION SHOT ***
    RN = Int(Rnd(1) * 100) + 1
    A4 = Int(Rnd(1) * 50) + 1

    If RN <= (B%(P, B1!(P, B), 16) + OY%(O0%(P))) * 2 + 1 Then
        PZ% = 3
        F = F + A4
        X = 0
    Else
        '*** CHECK AVERAGE SHOT ***
        I2 = B%(P, B1!(P, B), 16)
        RN = Int(Rnd(1) * 2)

        If Left$(AA$(P, B1!(P, B), 1), 1) = "G" Or Left$(AA$(P, B1!(P, B), 1), 1) = "F" Then

            RN = Int(Rnd(1) * 3)

            Select Case RN
                Case 1: PZ% = 6
                Case 2: PZ% = 7
                Case 3: PZ% = 5
            End Select

        Else

            If RN = 1 Then
                PZ% = 6
            Else
                PZ% = 5
            End If

        End If

        F = F + GF%(RN, I2)
        X = 0

    End If

End If

GoTo 520

'----- NOODLE -----

'-- Without the GoTo 300, this could be a subroutine
'-- Perhaps it still could be if we add the GoTo 300 after each call
513 '
Call LastShot
X8 = SC(SC)
LS = 4

A!(P, B1!(P, B), 23) = A!(P, B1!(P, B), 23) + 1

If pbpDelay! <> 0 Then

    If PB% = 1 Then
        Call WINDEX
        Print "BALL STOLEN BY "; AA$(D, B1!(D, P9), 0); "!"
        Call MOREDELAY
    Else
        RN = Int(Rnd(1) * 100) + 1

        Select Case RN
            Case 1 To 3: U$ = AA$(D, B1!(D, P9), 0) + " QUICKLY REACHES IN!"
            Case 4 To 6: U$ = AA$(D, B1!(D, P9), 0) + " SNEAKS IN FROM BEHIND!"
            Case 7 To 10: U$ = AA$(D, B1!(D, P9), 0) + " GETS A HAND ON THE BALL!"
        End Select

        Call DELAY
        RN = Int(Rnd(1) * 20) + 1

        Select Case RN
            Case 1: U$ = "PASS INTERCEPTED BY " + AA$(D, B1!(D, P9), 0) + "!"
            Case 2: U$ = "PASS IS PICKED OFF BY " + AA$(D, B1!(D, P9), 0) + "!"
            Case 3: U$ = "BALL IS STOLEN BY " + AA$(D, B1!(D, P9), 0) + "!"
            Case 4: U$ = AA$(D, B1!(D, P9), 0) + " WITH THE STEAL!"
            Case 5: U$ = AA$(D, B1!(D, P9), 0) + " TAKES IT AWAY!"
            Case 6: U$ = AA$(D, B1!(D, P9), 0) + " STEALS IT!"
            Case 7: U$ = AA$(D, B1!(D, P9), 0) + " RIPS IT LOOSE!"
            Case 8: U$ = AA$(D, B1!(D, P9), 0) + " TAKES THE BALL AWAY!"
            Case 9: U$ = AA$(D, B1!(D, P9), 0) + " STEALS THE BALL!"
            Case 10: U$ = AA$(D, B1!(D, P9), 0) + " PICKS IT OFF!"
            Case 11: U$ = AA$(D, B1!(D, P9), 0) + " SWIPES THE BALL!"
            Case 12: U$ = AA$(D, B1!(D, P9), 0) + " WITH THE STOLEN BALL!"
            Case 13: U$ = AA$(D, B1!(D, P9), 0) + " STRIPS IT AWAY!"
            Case 14: U$ = AA$(D, B1!(D, P9), 0) + " WITH THE STEAL!"
            Case 15: U$ = AA$(D, B1!(D, P9), 0) + " SWIPES AND STEALS IT!"
            Case 16: U$ = AA$(D, B1!(D, P9), 0) + " SNATCHES IT AWAY!"
            Case 17: U$ = AA$(D, B1!(D, P9), 0) + " QUICKLY REACHES IN!"
            Case 18: U$ = AA$(D, B1!(D, P9), 0) + " TAKES IT THE OTHER WAY!"
            Case 19: U$ = AA$(D, B1!(D, P9), 0) + " POKES IT LOOSE!"
            Case 20: U$ = AA$(D, B1!(D, P9), 0) + " STRIPS IT CLEAN!"
        End Select

        Call DELAY

    End If

End If

500 '
A!(D, B1!(D, P9), 13) = A!(D, B1!(D, P9), 13) + 1
T0(P) = T0(P) + 1

Call UPDATESCREEN

C1 = 2
P9 = D

Call CheckZeroTime: If T = 0 Then GoTo 170

'GoSub 3000
'AKA ChangePosession
PB$ = ""

If P = 1 Then
    P = 0
    D = 1
    P9 = 0
    C1 = 2
Else
    'P = 0;
    P = 1
    D = 0
    P9 = 1
    C1 = 2
End If

FB% = 1

GoTo 300

'----- NOODLE -----

515 Call LastShot
X8 = SC(SC)
LS = 4

A!(P, B1!(P, B), 23) = A!(P, B1!(P, B), 23) + 1
T0(P) = T0(P) + 1

If SV% <> 1 Then PT% = 1: Call MAKEPBP

SV% = 0

If PB% = 1 And pbpDelay! <> 0 Then
    Call WINDEX
    Print "TURNOVER BY ";: Color T(P, 33), T(P, 34): Print B$(P)
    Color 15, 0: Call MOREDELAY
End If

If PB% = 0 Then Call DELAY
U$ = "BALL OUT OF BOUNDS TO " + B$(D)

If PB% = 0 Then
    Call DELAY
Else
    'PB% = 1 And
    If pbpDelay! <> 0 Then Print U$: Call MOREDELAY
End If

Call MOREDELAY
Call UPDATESCREEN

Locate 10, 1

Call CheckZeroTime: If T = 0 Then GoTo 170

'GoSub 3000
'AKA ChangePosession
PB$ = ""
If P = 1 Then
    P = 0
    D = 1
    P9 = 0
    C1 = 2
Else
    'P = 0;
    P = 1
    D = 0
    P9 = 1
    C1 = 2
End If
'End Sub

Call Substitutions

GoTo 300

'----- NOODLE -----

520 '
Call LastShot
Call CheckZeroTime: If T = 0 Then GoTo 170

If PZ% = 4 And F3S% <= 0 Then F3S% = 3

If pbpDelay! <> 0 Then

    If F > 70 And FB% = 1 And PASS% = 1 Then
        PT% = 6
        PZ% = 8
        Call MAKEPBP
        Call DELAY
    Else

        If AA$(P, B1!(P, B), 0) <> PB$ Then
            If PB$ = "" Then
                U$ = "PASS TO " + AA$(P, B1!(P, B), 0): Call DELAY
            Else
                'PB$ <> ""
                PT% = 10: Call MAKEPBP: Call DELAY
            End If
        Else
            'AA$(P, B1!(P, B), 0) = PB$
            PT% = 9: Call MAKEPBP: Call DELAY
        End If

        If X8 <= 0 Then U$ = "SHOT CLOCK HAS EXPIRED!": SV% = 1: GoTo 515
        If X8 < 5 Then U$ = "SHOT CLOCK DOWN TO" + Str$(X8): Call DELAY

        PB$ = AA$(P, B1!(P, B), 0)
        Z3$ = Left$(AA$(P, B1!(P, B), 1), 1)
        Y2$ = "HE"
        Y3$ = AA$(D, B1!(D, B), 0)

        'On PZ% GOTO 521, 522, 523, 524, 525, 526, 527

        Select Case PZ%

            Case 1:
                If pbpDelay! <> 0 Then
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
                If pbpDelay! <> 0 Then

                    RN = Int(Rnd(1) * 100) + 1

                    If Z3$ = "C" Then

                        Select Case RN
                            Case 1 To 25: U$ = Y2$ + " MAKES A MOVE DOWN LOW":
                            Case 26 To 50: U$ = Y2$ + " POSITIONS HIMSELF DOWN LOW"
                            Case 51 To 75: U$ = Y2$ + " POSTS UP UNDERNEATH"
                            Case 76 To 100: U$ = Y2$ + " BACKS IN AGAINST " + AA$(D, B1!(D, 4), 0)
                        End Select

                        Call DELAY
                        U$ = "HE HAS A GOOD SHOT INSIDE"
                        Call DELAY

                    End If

                    602 '
                    If Z3$ = "F" Then
                        Select Case RN
                            Case 1 To 17: U$ = Y2$ + " DRIVES THE BASELINE"
                            Case 18 To 34: U$ = Y2$ + " PENETRATES THE LANE"
                            Case 35 To 50: U$ = Y2$ + " MOVES INSIDE"
                            Case 51 To 67: U$ = Y2$ + " DRIVES THE LANE"
                            Case 68 To 84: U$ = Y2$ + " MOVES INTO THE PAINT"
                            Case 85 To 100: U$ = Y2$ + " DRIVES UNDERNEATH"
                        End Select

                        Call DELAY
                        U$ = "HE'S GOT A GOOD SHOT UNDERNEATH"
                        Call DELAY
                    End If

                    If Z3$ = "G" Then
                        Select Case RN
                            Case 1 To 25: U$ = Y2$ + " DRIVES THE LANE": Call DELAY: U$ = "HE HAS A GOOD SHOT CLOSE IN"
                            Case 26 To 50: U$ = Y2$ + " DRIVES THROUGH THE KEY": Call DELAY: U$ = "HE'S INTO THE LANE": Call DELAY: U$ = "HE HAS A GOOD SHOT CLOSE IN"
                            Case 51 To 75: U$ = Y2$ + " DRIVES TO THE BASKET": Call DELAY: U$ = "HE HAS A GOOD SHOT CLOSE IN"
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

529 '
If F <= 0 Then F = 1
If F >= 100 Then F = 100

If pbpDelay! <> 0 Then

    If PB% = 1 Then
        Call WINDEX
        Print "PASS TO "; AA$(P, B1!(P, B), 0); " OF ";
        Color T(P, 33), T(P, 34)
        Print B$(P)
        Color 15, 0
    End If

    If PB% = 1 Then
        If PZ% <> 4 Then
            Locate 10: Print "SHOT % "; Int(F + .5);
        Else
            Locate 10: Print "3 PT SHOT % "; Int(F3S% + .5);
            Print " *";
            GoTo 528
        End If
    End If

    If PB% = 1 And PZ% >= 2 And PZ% <= 8 Then Print "*";

    If pbpDelay! <> 0 And PB% <> 1 Then

        If PZ% <> 4 Then
            Locate 8, 41: Print Using "SHOT % ###   "; Int(F + .5);
        Else
            Locate 8, 41: Print Using "SHOT % ###"; Int(F3S% + .5);
            Print " *";
            GoTo 528
        End If

        If PZ% >= 2 And PZ% <= 8 Then Locate , 52: Print "*";

    End If

End If

528 '
Print

If PB% = 1 And pbpDelay! <> 0 Then Call MOREDELAY

If U5 <> 0 Or CM% = 1 Then

    If H >= 4 And T <= 240 And S!(P, 0) - S!(D, 0) > 0 And U5 = 1 And P = CT Then

        '985
        U$ = Str$(A!(P, B1!(P, B), 1)) + " FT%  INTENTIONAL FOUL (YN)"

        Do
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "3"

        If UCase$(I$) = "Y" Or I$ = "3" Then P6 = 3: GoTo 532

    Else

        If U5 = 2 Or U5 = 1 And D = CT Then

            '980
            If T <= 240 And H >= 4 And S!(P, 0) - S!(D, 0) > 10 Then
                If (U5 = 1 Or U5 = 0) And CM% <> 1 Then 530
            Else
                If T <= 30 And H >= 4 And S!(P, 0) - S!(D, 0) > 0 Then P6 = 3: GoTo 532
                If (U5 = 1 Or U5 = 0) And CM% <> 1 Then 530
            End If

        End If
    End If


    951 '
    If T <= 1 Or X8 <= 6 Then
        P6 = 1
    Else
        If T <= 8 And S!(P, 0) - S!(D, 0) <= -3 And H >= 4 Then
            P6 = 1
        Else
            If T <= 20 And S!(P, 0) - S!(D, 0) <= -5 And H >= 4 Then
                P6 = 1
            Else
                If PZ% > 1 Then
                    P6 = 1
                Else
                    P6 = 0
                End If
            End If
        End If

    End If

    GoTo 532

End If

530 '
I$ = GetKeyPress$
If UCase$(I$) = "T" Then Call TICKER: GoTo 530
If UCase$(I$) = "S" Then Call SCOUTREPORT: GoTo 530
If UCase$(I$) = "Q" Then Run "HELLO.EXE"
If UCase$(I$) = "Z" Or I$ = "/" Then Call CHANGESTRATEGY(I$): GoTo 530
P6 = Val(I$)
If P6 < 1 Or P6 > 4 Then P6 = 0

If P6 = 4 Then FT% = 1

'----------------------------------------------------
'--- THIS IS CRITCAL FOR PBP MOVING AUTOMATICALLY ---
'----------------------------------------------------
' essentially it allows the user to press a key
' during game play to activate certain functions
' or special menus, etc.
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
If (P6 = 1 Or P6 = 4) And PZ% = 4 Then P6 = 4

caseVar = P6 + 1

Select Case caseVar
    Case 1
        'Pass!
        GoTo 350

    Case 2
        'Shoot!
        GoTo 537

    Case 3
        'Timeout
        GoTo 640

    Case 4
        'Intentional Foul
        GoTo 584

    Case 5
        'What ?
        GoTo 359

End Select

'--- technically this branch ends here ---

537 '*** SHOOT!!! ***
PT% = 3
Call MAKEPBP
Call DELAY
S = 0
R0 = Int(Rnd(1) * 100) + 1
A!(P, B1!(P, B), 7) = A!(P, B1!(P, B), 7) + 1

If H > 3 And T <= 30 And S!(D, 0) - S!(P, 0) = 2 Or H > 3 And T <= 30 And S!(D, 0) - S!(P, 0) = 0 Then 540

If R0 <= A!(P, B1!(P, B), 16) + F7%(D, 3) Then
    A!(P, B1!(P, B), 7) = A!(P, B1!(P, B), 7) - 1
    X8 = SC(SC)
    LS = 4
    GoTo 589
End If

540 '
S = Int(Rnd(1) * 100) + 1
R0 = (F - 2) * .1

'--- Shot Branch 1 ---
If S <= R0 Then
    X8 = SC(SC)
    LS = 4

    T = T - 4
    TE = 4
    Call PlayingTime
    Call UPDATESCREEN


    If DK% = 1 Then
        If PB% = 0 Then
            PT% = 2: Call MAKEPBP: U$ = "STUFF BY " + AA$(P, B1!(P, B), 0): Call DELAY: DK% = 0: GoTo 617
        Else
            'PB% = 1
            Call WINDEX: Print "STUFF BY " + AA$(P, B1!(P, B), 0): Call MOREDELAY: DK% = 0: GoTo 617
        End If
    End If

    If PZ% = 8 And PB% = 0 Then PT% = 7: Call MAKEPBP: PZ% = 0: Call DELAY: GoTo 617

    If PB% = 0 Then
        U$ = "BASKET GOOD BY " + AA$(P, B1!(P, B), 0): Call DELAY
    Else
        'PB% = 1
        If pbpDelay! <> 0 Then Call WINDEX: Print "BASKET GOOD BY " + AA$(P, B1!(P, B), 0): Call MOREDELAY
    End If

    617 '
    U$ = "... AND HE'S FOULED !!!"

    If PB% = 0 Then
        Call DELAY
    Else
        'PB% = 1
        If pbpDelay! <> 0 Then Print U$: Call MOREDELAY
    End If

    F3 = 1

    Call MOREDELAY
    Call CreditAssists

    F3 = 1
    S!(P, H) = S!(P, H) + 2
    S!(P, 0) = S!(P, 0) + 2
    A!(P, B1!(P, B), 8) = A!(P, B1!(P, B), 8) + 1

    Call UPDATESCREEN
    Call Fouls(0)

    If pbpDelay! <> 0 Then

        If PB% = 0 Then
            If RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
                U$ = "2 TO MAKE 1 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %": Call DELAY
            Else
                U$ = "1 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %": Call DELAY
            End If
        Else
            'PB% = 1
            If RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
                Call WINDEX
                Print B$(P)
                U$ = "2 TO MAKE 1 FT: " + AA$(P, B1!(P, B), 0) + "   " + Str$(A!(P, B1!(P, B), 1)) + " %"

            Else
                Call WINDEX
                Print B$(P)
                Print "1 FT: "; AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
            End If

        End If

    End If

    '620
    X = 50
    Call Substitutions
    X = 0

    If pbpDelay! <> 0 Then

        If PB% = 0 Then
            If RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
                U$ = "2 TO MAKE 1 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %"
            Else
                U$ = "1 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %"
            End If
            Call DELAY
        End If

        If PB% = 1 Then
            Call WINDEX
            Print B$(P)
            If RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
                Print "2 TO MAKE 1 FT: ";
                Print AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
            Else
                Print "1 FT: "; AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
            End If

        End If

    End If

    '621
    NM% = 0

    For F3 = 1 To 2

        S = Int(Rnd(1) * 100) + 1

        FX% = A!(P, B1!(P, B), 1)

        If S <= FX% Then
            If PB% = 0 Then
                NM% = NM% + 1: Call THREEFT
            Else
                'PB% = 1 Then
                NM% = NM% + 1: Call MADEFT
            End If
        Else
            'S > FX% And
            If PB% = 0 Then
                Call BADFT
            Else
                'PB% = 1
                Call MISSFT

            End If
        End If

        A!(P, B1!(P, B), 9) = A!(P, B1!(P, B), 9) + 1

        If NM% = 1 Then Exit For

        If F3 = 1 And (RT% = 0 Or RT% = 1 And (P2(D) <= NB% And H <= 4 Or P2(D) <= 3 And H > 4)) Then Exit For

    Next F3

    624 '
    Call CheckZeroTime: If T = 0 Then GoTo 170

    X = 99

    If S > FX% Then 670

    'GoSub 3000
    'AKA ChangePosession
    PB$ = ""

    If P = 1 Then
        P = 0
        D = 1
        P9 = 0
        C1 = 2
    Else
        'P = 0;
        P = 1
        D = 0
        P9 = 1
        C1 = 2
    End If
    'End Sub

    U$ = B$(P) + " BALL AFTER SCORE"
    Call SELECTPBP
    Locate 8, 41: Print Space$(12)
    Call Choices

    B = 6
    X = 0

    If pbpDelay! <> 0 Then
        For I = 8 To 15
            Locate I, 1
            Print Space$(38)
        Next I
    End If

    Locate 8, 1

    'PRESSING
    If P7(D) = 1 Then
        If PB% = 0 Then U$ = B$(D) + " IS PRESSING": Call DELAY

        If PB% = 1 And pbpDelay! <> 0 Then
            Call WINDEX
            Color T(D, 33), T(D, 34): Print B$(D);
            Color 15, 0: Print " IS PRESSING"
            Call MOREDELAY
        End If

        P1 = 1
    End If

    GoTo 300

End If

'--- Shot Branch 2 ---
If S <= F - 2 Then

    553 '
    If DK% = 1 Then
        If PB% = 0 Then
            PT% = 2
            Call MAKEPBP
            U$ = "STUFF BY " + AA$(P, B1!(P, B), 0) + " !!!": DK% = 0
        Else
            'PB% = 1 Then
            U$ = "STUFF BY " + AA$(P, B1!(P, B), 0) + " !!!": DK% = 0
        End If
    Else
        If PB% = 0 And PZ% = 8 Then
            PT% = 7: Call MAKEPBP: Call DELAY: PZ% = 0: skipShotPBP% = 1
        Else
            U$ = "BASKET BY "
            U$ = U$ + AA$(P, B1!(P, B), 0) + " !!!"
        End If
    End If

    If skipShotPBP% = 0 Then

        If PB% = 1 And pbpDelay! <> 0 Then
            Call WINDEX: Print U$: Call MOREDELAY
        Else

            RN = Int(Rnd(1) * 100) + 1

            If RN > 50 Then

                RN = Int(Rnd(1) * 22) + 1

                Select Case RN
                    Case 1: U$ = "NOTHING BUT THE BOTTOM OF THE NET!"
                    Case 2: U$ = "Count IT!"
                    Case 3: U$ = AA$(P, B1!(P, B), 0) + " TICKLES THE TWINE!"
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
    S!(P, H) = S!(P, H) + 2
    S!(P, 0) = S!(P, 0) + 2

    Call CreditAssists

    Call UPDATESCREEN

    A!(P, B1!(P, B), 8) = A!(P, B1!(P, B), 8) + 1

    '---> possibly a new sub
    562 U$ = B$(D) + " BALL AFTER SCORE"

    If PB% = 1 And pbpDelay! <> 0 Then
        Locate 9, 1
        Color T(D, 33), T(D, 34)
        Print B$(D);: Color 15, 0
        Print " BALL AFTER SCORE"
        Call MOREDELAY
    End If
    If PB% = 0 Then Locate 8, 41: Print Space$(12): Call DELAY
    X8 = SC(SC): LS = 4
    T = T - 4
    TE = 4

    Call PlayingTime
    Call CheckZeroTime: If T = 0 Then GoTo 170
    Call UPDATESCREEN

    'GoSub 3000
    'AKA ChangePosession
    PB$ = ""
    If P = 1 Then
        P = 0: D = 1: P9 = 0: C1 = 2
    Else
        'P = 0;
        P = 1: D = 0: P9 = 1: C1 = 2
    End If

    Call Choices

    If pbpDelay! <> 0 Then
        For I = 8 To 15
            Locate I, 1
            Print Space$(38)
        Next I
    End If

    Locate 8, 1
    If P7(D) = 1 Then
        If PB% = 0 Then
            U$ = B$(D) + " IS PRESSING": Call DELAY
        Else
            'PB% = 1
            If pbpDelay! <> 0 Then
                Call WINDEX
                Color T(D, 33), T(D, 34): Print B$(D);
                Color 15, 0: Print " IS PRESSING": Call MOREDELAY
            End If
        End If
        P1 = 1
    End If

    GoTo 300

End If

'--- Shot Branch 3 ---
R0 = Int(Rnd(1) * 100) + 1

548 '
I5 = Int(Rnd(1) * 100) + 1
I3 = Int(Rnd(1) * 5)

If I5 > A!(D, B1!(D, I3), 6) Then
    545 '
    If PB% = 1 And pbpDelay! <> 0 Then
        Call WINDEX
        Print "SHOT MISSED BY "; AA$(P, B1!(P, B), 0)
        Call MOREDELAY
    End If

    If PB% = 0 Then PT% = 8: Call MAKEPBP: Call DELAY

    X8 = SC(SC)
    LS = 4

Else
    If pbpDelay! <> 0 Then

        If PB% = 1 Then
            Call WINDEX
            Print "SHOT BLOCKED BY "; AA$(D, B1!(D, I3), 0)
            Call MOREDELAY
        Else

            RN = Int(Rnd(1) * 100) + 1

            Select Case RN
                Case 1 To 10: U$ = AA$(D, B1!(D, I3), 0) + " STUFFS THE SHOT!!"
                Case 11 To 15: U$ = "DENIED BY " + AA$(D, B1!(D, I3), 0) + "!!"
                Case 16 To 20: U$ = "ERASED BY " + AA$(D, B1!(D, I3), 0) + "!!"
                Case 21 To 25: U$ = "EXPUNGED BY " + AA$(D, B1!(D, I3), 0) + "!!"
                Case 26 To 30: U$ = "REBUKED BY " + AA$(D, B1!(D, I3), 0) + "!!"
                Case 31 To 35: U$ = "SHOT BLOCKED CLEANLY BY " + AA$(D, B1!(D, I3), 0) + "!!"
                Case 36 To 70: U$ = "SHOT BLOCKED BY " + AA$(D, B1!(D, I3), 0) + "!!"
                Case 71 To 90: U$ = "SHOT REJECTED BY " + AA$(D, B1!(D, I3), 0) + "!!"
                Case 91 To 100: U$ = "SHOT SWATTED OUT BY " + AA$(D, B1!(D, I3), 0) + "!!"
            End Select

            Call DELAY

        End If

    End If

    554 A!(D, B1!(D, I3), 15) = A!(D, B1!(D, I3), 15) + 1
    X = 98

End If

'GoTo 546

546 '
T = T - 4
TE = 4

Call PlayingTime
Call CheckZeroTime: If T = 0 Then GoTo 170
Call UPDATESCREEN

GoTo 670

'----- NOODLE -----

565 '
Call Fouls(0)

566 '
If P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then 570

Call CheckZeroTime: If T = 0 Then GoTo 170

If FR% = 0 Then

    Call Substitutions

    If pbpDelay! <> 0 Then
        For I = 8 To 15
            Locate I, 1
            Print Space$(38)
        Next I
    End If

    Locate 8, 1
    U$ = B$(P) + " BALL AFTER FOUL"
    Call DELAY

    Locate 8, 41: Print Space$(12)
    GoTo 350

End If

Call UPDATESCREEN

If pbpDelay! <> 0 Then
    If PB% = 0 Then
        U$ = "1 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %"
        Call DELAY
        GoTo 571
    End If

    Call WINDEX
    Color T(P, 33), T(P, 34): Print B$(P)
    Color 15, 0

    Print "1 FT: "; AA$(P, B1!(P, B), 0); " "; A!(P, B1!(P, B), 1); " %"

End If

571 '
X = 50
Call Substitutions
X = 0

If pbpDelay! <> 0 Then

    If PB% = 0 Then
        U$ = "1 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %"
        Call DELAY
    Else
        Call WINDEX
        Color T(P, 33), T(P, 34): Print B$(P)
        Color 15, 0
        Print "1 FT: "; AA$(P, B1!(P, B), 0); " "; A!(P, B1!(P, B), 1); " %"
    End If
End If

572 '
A!(P, B1!(P, B), 9) = A!(P, B1!(P, B), 9) + 1
F3 = 1
S = Int(Rnd(1) * 100) + 1
FX% = A!(P, B1!(P, B), 1)

If S <= FX% Then
    If PB% = 0 Then
        Call THREEFT
    Else
        'PB% = 1
        Call MADEFT
    End If
Else
    'S > FX%
    If PB% = 0 Then
        Call BADFT
    Else
        'PB% = 1
        Call MISSFT
    End If
End If

LT = 0

Call CheckZeroTime: If T = 0 Then GoTo 170

If S > FX% Then 650

'GoSub 3000
'AKA ChangePosession
PB$ = ""
If P = 1 Then
    P = 0: D = 1: P9 = 0: C1 = 2
Else
    'P = 0;
    P = 1: D = 0: P9 = 1: C1 = 2
End If

U$ = B$(P) + " BALL AFTER SCORE"
Call SELECTPBP
Locate 8, 41: Print Space$(12)
Call Choices
B = 6
X = 0

If pbpDelay! <> 0 Then
    For I = 8 To 15
        Locate I, 1
        Print Space$(38)
    Next I
End If

Locate 8, 1

If P7(D) = 1 Then
    If PB% = 0 Then
        U$ = B$(D) + " IS PRESSING": Call DELAY
    Else
        'PB% = 1
        If pbpDelay! <> 0 Then
            Call WINDEX
            Color T(D, 33), T(D, 34)
            Print B$(D);: Color 15, 0
            Print " IS PRESSING"
            Call MOREDELAY
        End If

        P1 = 1
    End If
End If

GoTo 300

'----- NOODLE -----

570 '
If RT% = 1 And (P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4) Then
    U$ = AA$(P, B1!(P, B), 0) + " GETS 3 FT TO MAKE 2..."
Else
    U$ = AA$(P, B1!(P, B), 0) + " GETS 2 FREE THROWS..."
End If
Call DELAY

GoTo 591

'----- NOODLE -----

580 '
Call LastShot

X8 = SC(SC)
LS = 4

'GoSub 3000
'AKA ChangePosession
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

Call CheckZeroTime: If T = 0 Then GoTo 170

U$ = "OFFENSIVE FOUL"

If PB% = 0 Then
    Call DELAY
Else
    'PB% = 1
    If pbpDelay! <> 0 Then Call WINDEX: Print U$: Call MOREDELAY
End If

U$ = B$(P) + " BALL AFTER FOUL"
If PB% = 0 Then
    Call DELAY
Else
    'PB% = 1
    If pbpDelay! <> 0 Then Print U$: Call MOREDELAY
End If

Locate 8, 41: Print Space$(12)

B = 6
Call DELAY
T0(D) = T0(D) + 1
Call UPDATESCREEN
Call Substitutions

If pbpDelay! <> 0 Then
    For I = 8 To 15
        Locate I, 1
        Print Space$(38)
    Next I
End If

Locate 8, 1

If P7(D) = 1 Then

    If PB% = 0 Then
        U$ = B$(D) + " IS PRESSING": Call DELAY
    Else
        'PB% = 1
        If pbpDelay! <> 0 Then
            Call WINDEX
            Color T(D, 33), T(D, 34): Print B$(D);
            Color 15, 0: Print " IS PRESSING"
            Call MOREDELAY
        End If
    End If

    P1 = 1

End If

GoTo 300

'----- NOODLE -----

584 '
IN = 99

Call LastShot
Call CheckZeroTime: If T = 0 Then GoTo 170

C1 = 2
P9 = D
U$ = "DESIGNATED FOULER (0-4)"
Call SELECTPBP

If U5 = 1 And D = CT Or U5 = 2 Then
    W5 = Int(Rnd(1) * 5)
Else

    Do
        I$ = GetKeyPress$
        W5 = Val(I$)
    Loop Until W5 >= 0 And W5 <= 4
End If

Call Fouls(1)

X8 = SC(SC)

GoTo 566

'----- NOODLE -----
589 '
T = T - 4
TE = 4

Call PlayingTime
Call UPDATESCREEN
Call Fouls(0)

If pbpDelay! <> 0 Then

    If PB% = 0 Then
        If RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
            U$ = "3 TO MAKE 2 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %": Call DELAY
        Else
            U$ = "2 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %": Call DELAY
        End If
    Else
        'PB% = 1
        If RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
            Call WINDEX
            Print B$(P)
            Print "3 TO MAKE 2 FT: ";
            Print AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
        Else
            Call WINDEX: Print B$(P): Print "2 FT: "; AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
        End If
    End If

End If

591 '
X = 50
Call Substitutions
X = 0

If pbpDelay! <> 0 Then

    If PB% = 0 Then

        If RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
            U$ = "3 TO MAKE 2 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %":
        Else
            U$ = "2 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %"
        End If

        Call DELAY

    Else
        'PB% = 1
        Call WINDEX
        Print B$(P)

        If RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
            Print "3 TO MAKE 2 FT: ";
            Print AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
        Else
            Print "2 FT: "; AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
        End If

    End If

End If

592 '
Call MOREDELAY
NM% = 0

For F3 = 1 To 3
    S = Int(Rnd(1) * 100) + 1
    FX% = A!(P, B1!(P, B), 1)
    If S <= FX% Then
        If PB% = 0 Then
            NM% = NM% + 1: Call THREEFT
        Else
            'PB% = 1
            NM% = NM% + 1: Call MADEFT
        End If
    Else
        'S > FX%
        If PB% = 0 Then
            Call BADFT
        Else
            Call MISSFT
        End If

        A!(P, B1!(P, B), 9) = A!(P, B1!(P, B), 9) + 1

        If NM% = 2 Then Exit For
        If F3 = 2 And (RT% = 0 Or RT% = 1 And (P2(D) <= NB% And H <= 4 Or P2(D) <= 3 And H > 4)) Then Exit For

    End If

Next F3

596 '
Call CheckZeroTime: If T = 0 Then GoTo 170

If S > A!(P, B1!(P, B), 1) Then 650

'GoSub 3000
'AKA ChangePosession
PB$ = ""
If P = 1 Then
    P = 0: D = 1: P9 = 0: C1 = 2
Else
    'P = 0;
    P = 1: D = 0: P9 = 1: C1 = 2
End If

U$ = B$(P) + " BALL AFTER SCORE"

Call SELECTPBP
Locate 8, 41: Print Space$(12)
Call Choices
B = 6
X = 0

If pbpDelay! <> 0 Then
    For I = 8 To 15
        Locate I, 1
        Print Space$(38)
    Next I
End If

Locate 8, 1
If P7(D) = 1 Then
    If PB% = 0 Then U$ = B$(D) + " IS PRESSING": Call DELAY
    If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Color T(D, 33), T(D, 34): Print B$(D);: Color 15, 0: Print " IS PRESSING": Call MOREDELAY
    P1 = 1
End If

GoTo 300

'----- NOODLE -----

640 '
If T1(P) > 0 Then
    T1(P) = T1(P) - 1
    T = T + M5
    TE = -M5
    Call PlayingTime
    X8 = X8 + M5
    U$ = "TIMEOUT " + B$(P):

    Call DELAY
    Call MOREDELAY
    Call Substitutions
End If

648 '
If A6 = 0 Then

    If pbpDelay! <> 0 Then
        For I = 8 To 15
            Locate I, 1
            Print Space$(38)
        Next I
    End If

    Locate 8, 1

    If P7(D) = 1 Then
        If PB% = 0 Then
            U$ = B$(D) + " IS PRESSING"
            Call DELAY
        Else
            'PB% = 1
            If pbpDelay! <> 0 Then Call WINDEX: Color T(D, 33), T(D, 34): Print B$(D);: Color 15, 0: Print " IS PRESSING": Call MOREDELAY
        End If
        P1 = 1
    End If

    GoTo 300

Else

    GoTo 350

End If


'----- NOODLE -----
650 '
A6 = 1
P6 = 0
A4 = 0
D6 = 0
X8 = SC(SC)
LS = 4

S = 0
R0 = Int(Rfnd(1) * 100) + 1

If H > 3 And T < 30 And S!(D, 0) - S!(P, 0) = 2 And R0 <= 12 Or H > 3 And T < 30 And S!(D, 0) - S!(P, 0) = 0 And R0 <= 12 Then
    Call ReboundRoutine1
    GoTo 760
End If

R0 = Int(Rnd(1) * 100) + 1

Select Case R0
    Case 1 To 11: FF% = 1: GoTo 707
    Case 12 To 19: Call ReboundRoutine1: GoTo 760
    Case 20 To 110: Call ReboundRoutine2: GoTo 800
End Select

'----- NOODLE -----

670 '
A6 = 1
P6 = 0
A4 = 0
D6 = 0
X8 = SC(SC)
LS = 4

'-- Start Loop
700 '
S = 0
A4 = 0
D6 = 0

R0 = Int(Rnd(1) * 100) + 1

GG = S!(D, 0) - S!(P, 0)

If R0 <= 12 And H > 3 And T < 30 And (GG = 2 Or GG = 0) Then 700
'-- End Loop

Select Case R0

    Case 1
        If X = 98 Then 700

        T0(P) = T0(P) + 1
        U$ = "OFFENSIVE GOAL TENDING"
        If PB% = 0 Then
            Call DELAY
        Else
            'PB% = 1
            If pbpDelay! <> 0 Then Call WINDEX: Print U$: Call MOREDELAY
        End If

        U$ = "BALL GOES TO " + B$(D)
        If PB% = 0 Then
            Call DELAY
        Else
            'PB% = 1
            If pbpDelay! <> 0 Then Call WINDEX: Print U$: Call MOREDELAY
        End If

        Call MOREDELAY

        'GoSub 3000
        'AKA ChangePosession
        PB$ = ""
        If P = 1 Then
            P = 0: D = 1: P9 = 0: C1 = 2
        Else
            'P = 0;
            P = 1: D = 0: P9 = 1: C1 = 2
        End If

        Call Substitutions

        GoTo 300

    Case 2 To 11
        FF% = 1
        GoTo 707

    Case 12 To 19
        '750
        R0 = Int(Rnd(1) * 100) + 1

        If R0 <= 32 Then 752

        R3(D) = R3(D) + 1
        Call UPDATESCREEN

        U$ = "RB KNOCKED O.B. TO " + B$(D)
        If PB% = 0 Then Call DELAY
        If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print U$: Call MOREDELAY

        Call MOREDELAY

        Call Substitutions

        'GoSub 3000
        'AKA ChangePosession
        PB$ = ""

        If P = 1 Then
            P = 0: D = 1: P9 = 0: C1 = 2
        Else
            'P = 0;
            P = 1: D = 0: P9 = 1: C1 = 2
        End If

        C1 = 2: P9 = P

        GoTo 300

    Case 20 To 22
        '730
        If X = 99 Then 700

        Call ReboundRoutine1

        A!(P, B1!(P, B), 7) = A!(P, B1!(P, B), 7) + 1
        A!(P, B1!(P, B), 8) = A!(P, B1!(P, B), 8) + 1
        A!(P, B1!(P, B), 11) = A!(P, B1!(P, B), 11) + 1
        R3(P) = R3(P) + 1
        S!(P, H) = S!(P, H) + 2
        S!(P, 0) = S!(P, 0) + 2

        Call UPDATESCREEN

        If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print "TIPPED IN BY "; AA$(P, B1!(P, B), 0); "!!!": Call MOREDELAY
        If PB% = 0 Then U$ = AA$(P, B1!(P, B), 0) + " TIPS IT IN!!!": Call DELAY

        Call MOREDELAY

        'GoSub 3000
        'AKA ChangePosession
        PB$ = ""
        If P = 1 Then
            P = 0: D = 1: P9 = 0: C1 = 2
        Else
            'P = 0;
            P = 1: D = 0: P9 = 1: C1 = 2
        End If

        U$ = B$(P) + " BALL AFTER SCORE"
        Locate 8, 41: Print Space$(12)

        If PB% = 0 Then Call DELAY
        If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call MOREDELAY

        Call Choices

        If pbpDelay! <> 0 Then
            For I = 8 To 15
                Locate I, 1
                Print Space$(38)
            Next I
        End If

        Locate 8, 1

        If P7(D) = 1 Then

            If PB% = 0 Then U$ = B$(D) + " IS PRESSING": Call DELAY
            If PB% = 1 And pbpDelay! <> 0 Then
                Call WINDEX
                Color T(D, 33), T(D, 34): Print B$(D);
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
            If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print U$: Call MOREDELAY
            Call MOREDELAY

            GoTo 560
        End If

End Select

707 'GoTo 770

770 RB = 0

For I = 0 To 9: RB(I) = 0: Next

For I = 0 To 4
    If H > 4 Or H = 4 And T <= 120 Then 773
    If A!(P, B1!(P, I), 3) - A!(P, B1!(P, I), 7) - A!(P, B1!(P, I), 11) - A!(P, B1!(P, I), 12) <= 0 Then RB(I) = RB(I) + A!(P, B1!(P, I), 21) * .65: RB = RB + A!(P, B1!(P, I), 21) * .65: GoTo 774
    773 RB(I) = RB(I) + A!(P, B1!(P, I), 21): RB = RB + A!(P, B1!(P, I), 21)
774 Next

For I = 0 To 4
    If H > 4 Or H = 4 And T <= 120 Then 778
    If A!(1 - P, B1!(1 - P, I), 3) - A!(1 - P, B1!(1 - P, I), 7) - A!(1 - P, B1!(1 - P, I), 11) - A!(1 - P, B1!(1 - P, I), 12) <= 0 Then RB(I + 5) = RB(I + 5) + A!(1 - P, B1!(1 - P, I), 2) * .5: RB = RB + A!(1 - P, B1!(1 - P, I), 2) * .5: GoTo 779
    778 RB(I + 5) = RB(I + 5) + A!(1 - P, B1!(1 - P, I), 2): RB = RB + A!(1 - P, B1!(1 - P, I), 2)
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

'----- NOODLE -----

720 U$ = AA$(P, B1!(P, B), 0) + " HAS THE OFFENSIVE REBOUND"
If PB% = 0 Then Call DELAY
If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print U$: Print "FOR "; B$(P): Call MOREDELAY

U$ = "HE'S FOULED !!!"
If PB% = 0 Then Call DELAY
If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call MOREDELAY

Call MOREDELAY
A!(P, B1!(P, B), 24) = A!(P, B1!(P, B), 24) + 1
A!(P, B1!(P, B), 11) = A!(P, B1!(P, B), 11) + 1
R3(P) = R3(P) + 1
Call UPDATESCREEN

Call Fouls(0)

GoTo 570

'----- NOODLE -----

752 R3(P) = R3(P) + 1

Call UPDATESCREEN

U$ = "RB KNOCKED O.B. TO " + B$(P)

If PB% = 0 Then Call DELAY
If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print U$: Call MOREDELAY

Call MOREDELAY
Call Substitutions

GoTo 520

'----- NOODLE -----

760 If FF% = 1 Then FF% = 0: GoTo 720

761 A!(P, B1!(P, B), 24) = A!(P, B1!(P, B), 24) + 1
A!(P, B1!(P, B), 11) = A!(P, B1!(P, B), 11) + 1
R3(P) = R3(P) + 1: Call UPDATESCREEN
U$ = AA$(P, B1!(P, B), 0) + " HAS THE OFFENSIVE REBOUND"

If PB% = 0 And pbpDelay! <> 0 Then Call DELAY
If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print U$: Print "FOR " + B$(P): Call MOREDELAY

Call CheckZeroTime: If T = 0 Then GoTo 170

PB$ = AA$(P, B1!(P, B), 0)
R0 = Int(Rnd(1) * 10) + 1
'*** GET SHOT PCT ***
F = A!(P, B1!(P, B), 0) + D1(D)

If N = P Then F = F + 2
If N <> P And N <> 2 Then F = F - 2
If Not (H > 4 Or H = 4 And T <= 120) Then
    If A!(P, B1!(P, B), 3) - A!(P, B1!(P, B), 7) - A!(P, B1!(P, B), 11) - A!(P, B1!(P, B), 12) <= 0 Then F = F * .6
End If

If R0 <= 8 Then
    PZ% = 9
    GoTo 537
End If

GoTo 520

'----- NOODLE -----

800 If FF% = 1 Then
    FF% = 0
    If pbpDelay! <> 0 Then
        If PB% = 1 Then
            Call WINDEX
            Print AA$(D, B1!(D, B), 0); " HAS THE DEF REBOUND FOR ": Print B$(D); " AND IS FOULED !!!"
            Call MOREDELAY
        Else
            U$ = AA$(D, B1!(D, B), 0) + " HAS THE DEFENSIVE REBOUND": Call DELAY
            U$ = "HE'S FOULED !!!": Call DELAY
            Call MOREDELAY
        End If
    End If

    R3(D) = R3(D) + 1
    A!(D, B1!(D, B), 11) = A!(D, B1!(D, B), 11) + 1
    Call UPDATESCREEN

    'GoSub 3000
    'AKA ChangePosession
    PB$ = ""

    If P = 1 Then
        P = 0: D = 1: P9 = 0: C1 = 2
    Else
        'P = 0;
        P = 1: D = 0: P9 = 1: C1 = 2
    End If

    GoTo 565
End If

A!(D, B1!(D, B), 11) = A!(D, B1!(D, B), 11) + 1
R3(D) = R3(D) + 1: Call UPDATESCREEN
U$ = AA$(D, B1!(D, B), 0) + " HAS THE DEFENSIVE REBOUND"

If PB% = 0 And pbpDelay! <> 0 Then Call DELAY
If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print U$: Print "FOR " + B$(D): Call MOREDELAY

PB$ = AA$(D, B1!(D, B), 0)
C1 = 2: P9 = D

'GoSub 3000
'AKA ChangePosession
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

'---------- THIS CODE IS NOT USED ? ----------

If U5 = 1 And P = CT And H <= 3 And T < 30 And J < 3 Then GoSub 1080
If U5 = 2 And H <= 3 And T < 30 And J < 3 Then GoSub 1080
If U5 = 1 And P = CT And H >= 4 And T <= 120 And J >= 3 And J <= 9 Then GoSub 1080
If U5 = 2 And H >= 4 And T <= 120 And J >= 3 And J <= 9 Then GoSub 1080

Return

1080 '
For J = 0 To 3
    J1 = Int(Rnd(1) * 5)
    If A!(P, B1!(P, J1), 20) > 0 Then
        B = J1
        Exit For
    End If
Next J

1084 Return

'---------- THIS CODE IS NOT USED ? ----------

'================================================================================

'In order to use this subroutine,
' we would need to mvoe the code
' that is relevant, and then
' we could shorten up some of
' logic flow / spaghetti strains

Sub ShotTest ()

    '    If R0 <= 8 + F7%(P, 1) + F7%(D, 2) + A!(D, B1!(D, P9), 5) + F7%(P, 0) + OX%(O0%(P)) Then
    '        GoTo 513
    '    Else
    '        R0 = Int(Rnd(1) * 1000) + 1

    '        If (PASS% <= 3 Or PASS% > 4) And R0 <= 320 + ((JY - 89) * 10) Then 445
    '        If PASS% = 4 Then 445
    '        GoTo 437
    '    End If

End Sub


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
'$INCLUDE: 'PadRight.BM'
'INCLUDE: 'GameRoutines.BM'



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
            Locate 2, 33
            Color T(P9, 33), T(P9, 34): Print B$(P9)

            Color 15, 4

            Do
                Locate , 33
                Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"
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

    Locate 19, 1
    Color 14, 0
    Print "STARTING LINEUP"

    Color 14
    Locate 19, 29: Print "   SELECT OFFENSIVE AND DEFENSIVE STYLE"
    Locate 21, 32: Color 15: Print "O";: Color 3: Print "FFENSE STYLE-";: Color 7: Print offenseStyles$(O0%(P9))
    Locate 22, 32: Color 3: Print "DE";: Color 15: Print "F";: Color 3: Print "ENSE STYLE-";: Color 7: Print defenseStyles$(D2(P9))
    Locate 23, 32: Color 3: Print "FULL COURT ";: Color 15: Print "P";: Color 3: Print "RESS-";: Color 7: Print P7$(P7(P9))

    If C1 = 1 Then

        For M = 0 To 4
            Locate M + 20, 1
            Color 2, 0: Print Using "# "; M;
            Color 15, 0: Print "ENTER "; PS$(M); " # ";
            Z = B1!(P9, M)
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
                J = Val(I$)
                Select Case UCase$(I$)
                    Case "A": J = 10
                    Case "B": J = 11
                    Case "C": J = 12
                    Case "D": J = 13
                    Case "E": J = 14
                    Case "F", "O", "P": J = 15
                End Select

                If J = 15 Then
                    '100
                    If UCase$(I$) = "P" Then P7(P9) = 1 - P7(P9)
                    If UCase$(I$) = "F" Then D2(P9) = D2(P9) + 1: If D2(P9) > 5 Then D2(P9) = 1
                    If UCase$(I$) = "O" Then O0%(P9) = O0%(P9) + 1: If O0%(P9) > 2 Then O0%(P9) = 0

                    '105
                    Color 14
                    Locate 19, 29: Print "   SELECT OFFENSIVE AND DEFENSIVE STYLE"
                    Locate 21, 32
                    Color 15: Print "O";
                    Color 3: Print "FFENSE STYLE-";
                    Color 7: Print offenseStyles$(O0%(P9))
                    Locate 22, 32
                    Color 3: Print "DE";
                    Color 15: Print "F";
                    Color 3: Print "ENSE STYLE-";
                    Color 7: Print defenseStyles$(D2(P9))
                    Locate 23, 32
                    Color 3: Print "FULL COURT ";
                    Color 15: Print "P";
                    Color 3: Print "RESS-";
                    Color 7: Print P7$(P7(P9))

                Else

                    If J >= 0 And J <= 14 And A!(P9, J, 12) < 6 And AA$(P9, J, 0) <> "XXX" And B%(P9, J, 18) = 0 Then
                        For I2 = 0 To 4
                            If B1!(P9, I2) = J Then Exit For
                        Next I2
                    End If

                End If

            Loop Until J >= 0 And J <= 14

            B1!(P9, I) = J
            Z = J
            CV% = 0
            For IP% = 0 To Z
                If B%(P9, IP%, 18) <> 0 Then
                    CV% = CV% + 1
                End If
            Next

            Print AA$(P9, J, 0);
            Locate J + 3 - CV%, 3
            Color T(P9, 33), T(P9, 34): Print Left$(AA$(P9, J, 0), 12)
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
            Select Case UCase$(I$)
                Case "O": I1 = 5
                Case "F": I1 = 5
                Case "P": I1 = 5
            End Select

        Loop Until I1 >= 0 And I1 <= 6

        If I1 = 5 Then
            If UCase$(I$) = "P" Then P7(P9) = 1 - P7(P9) ': GoSub 105
            If UCase$(I$) = "F" Then D2(P9) = D2(P9) + 1: If D2(P9) > 5 Then D2(P9) = 1
            If UCase$(I$) = "O" Then O0%(P9) = O0%(P9) + 1: If O0%(P9) > 2 Then O0%(P9) = 0

            '105
            Color 14
            Locate 19, 29: Print "   SELECT OFFENSIVE AND DEFENSIVE STYLE"
            Locate 21, 32
            Color 15: Print "O";
            Color 3: Print "FFENSE STYLE-";
            Color 7: Print offenseStyles$(O0%(P9))
            Locate 22, 32
            Color 3: Print "DE";
            Color 15: Print "F";
            Color 3: Print "ENSE STYLE-";
            Color 7: Print defenseStyles$(D2(P9))
            Locate 23, 32
            Color 3: Print "FULL COURT ";
            Color 15: Print "P";
            Color 3: Print "RESS-";
            Color 7: Print P7$(P7(P9))

        End If

        If I1 <= 4 Then

            If X = 50 And I1 = B And P9 = P Then
                Locate 20, 29: Color 14, 0: Print AA$(P9, B1!(P9, I1), 0); " IS AT THE FT LINE"
            Else
                Locate 25, 1: Print Space$(25);
                J = B1!(P9, I1)
                Z = J

                CV% = 0
                For IP% = 0 To Z:
                    If B%(P9, IP%, 18) <> 0 Then CV% = CV% + 1
                Next

                Locate J + 3 - CV%, 3
                Color 15, 0: Print Left$(AA$(P9, J, 0), 12)

                '77
                Do
                    Locate I1 + 20, 1: Print Space$(26);
                    B1!(P9, I1) = -1
                    Locate , 1
                    Color 2: Print Using "# "; I1;
                    Color 15: Print "ENTER "; PS$(I1); " # ";

                    I$ = GetKeyPress$
                    J = Val(I$)

                    Select Case UCase$(I$)
                        Case "A": J = 10
                        Case "B": J = 11
                        Case "C": J = 12
                        Case "D": J = 13
                        Case "E": J = 14
                        Case "F", "O", "P": J = 15
                    End Select

                    If J = 15 Then
                        If UCase$(I$) = "P" Then P7(P9) = 1 - P7(P9) ': GoSub 105
                        If UCase$(I$) = "F" Then D2(P9) = D2(P9) + 1: If D2(P9) > 5 Then D2(P9) = 1
                        If UCase$(I$) = "O" Then O0%(P9) = O0%(P9) + 1: If O0%(P9) > 2 Then O0%(P9) = 0
                    Else

                        If J >= 0 And J <= 14 And A!(P9, J, 12) < 6 And AA$(P9, J, 0) <> "XXX" And B%(P9, J, 18) = 0 Then
                            For I2 = 0 To 4:
                                If B1!(P9, I2) = J Then Exit For
                            Next I2
                        End If

                    End If

                Loop Until J >= 0 And J <= 14

                B1!(P9, I1) = J
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

    'Cls

End Sub

'----------------------------------------
'       PlayItSafe Subroutine
'----------------------------------------
'...explanation...
Sub PlayItSafe ()

    Cls
    A = 0
    S2 = 1
    S3 = 1
    SW = 0

    For ZZ = 0 To 4
        If A!(P9, B1!(P9, ZZ), 22) = 50 Then A = A + 1: If SW = 0 Then Print: Print "THE FOLLOWING PLAYER(S) ARE PLAYING SAFE BUT CAN NOW PLAY NORMAL IF YOU WISH": SW = 1
        If A!(P9, B1!(P9, ZZ), 22) = 50 And SW = 1 And S2 < 6 Then Locate S2 + 3, 1: Print S2; "-"; AA$(P9, B1!(P9, ZZ), 0): P5(A) = S2: P4(A) = ZZ: S2 = S2 + 1: S3 = S3 + 1
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
            A!(P9, B1!(P9, P4(A)), 17) = A!(P9, B1!(P9, P4(A)), 17) * 3
            A!(P9, B1!(P9, P4(A)), 22) = 0
            Locate P5(A) + 3, 20
            Print AA$(P9, B1!(P9, P4(A)), 0); " WILL NOW PLAY NORMAL DEFENSE!"
            Call RATINGTABLES
            D1(P9) = D1(P9) - 1
            A = A + 1
            MJ = 25
        End If

    Loop Until P6 <> P5(6)

    1214 '
    A = 0
    S2 = 1

    For ZZ = 0 To 4
        If H <= 2 And A!(P9, B1!(P9, ZZ), 12) >= 3 And A!(P9, B1!(P9, ZZ), 22) = 0 Or A!(P9, B1!(P9, ZZ), 12) = 5 And A!(P9, B1!(P9, ZZ), 22) = 0 Then
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
                Print S2; "- "; AA$(P9, B1!(P9, ZZ), 0)
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
                    A!(P9, B1!(P9, P4(A)), 17) = Int((A!(P9, B1!(P9, P4(A)), 17) + 1) / 3)
                    A!(P9, B1!(P9, P4(A)), 22) = 50
                    Locate P5(A) + 3, 20
                    Print AA$(P9, B1!(P9, P4(A)), 0); " WILL NOW PLAY SAFE!"
                    Call RATINGTABLES
                    D1(P9) = D1(P9) + 1
                    A = A + 1
                    S3 = S3 + 1
                End If

            End If

        Loop Until P6 >= 1 And P6 <= (S2 + 1)
    End If

    1230 '
    'Cls
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
        Call PlayingTime
        T = 0
        Call UPDATESCREEN
    End If

End Sub

'----------------------------------------
'       PlayingTime Subroutine
'----------------------------------------
'...explanation...
Sub PlayingTime ()

    ' *** INCREMENT MINUTES ***
    For I3 = 0 To 1
        For I4 = 0 To 4
            ZZ(I3, B1!(I3, I4)) = ZZ(I3, B1!(I3, I4)) + TE
        Next
    Next

End Sub

'----------------------------------------
'           SomeCalcs Subroutine
'----------------------------------------
'...explanation...
Sub SomeCalcs ()

    M1 = 0
    M2 = 0
    M3 = 0
    M4 = 0

    If Not (U5 = 1 And P9 = CT And X = 50 Or U5 = 2 And P9 = P And X = 50) Then

        For I1 = 0 To 14

            M6 = A!(P9, I1, 3) - A!(P9, I1, 7) - A!(P9, I1, 11) - A!(P9, I1, 12)

            If H < 4 And M6 <= 0 Or H = 4 And T > 120 And M6 <= 0 Then M1 = M1 + 1

            If AA$(P9, I1, 0) = "XXX" Or A!(P9, I1, 12) >= 6 Or B%(P9, I1, 18) <> 0 Or H = 1 And A!(P9, I1, 12) >= 2 Or H = 2 And A!(P9, I1, 12) >= 4 Or H = 3 And A!(P9, I1, 12) >= 5 Or H = 4 And T > 300 And A!(P9, I1, 12) = 5 Then M2 = M2 + 1

            If I1 <= 4 Then
                M6 = A!(P9, B1!(P9, I1), 3) - A!(P9, B1!(P9, I1), 7) - A!(P9, B1!(P9, I1), 11) - A!(P9, B1!(P9, I1), 12)

                If M6 <= 0 And H < 4 Or M6 <= 0 And H = 4 And T > 120 Then M3 = M3 + 1

                If AA$(P9, B1!(P9, I1), 0) = "XXX" Or A!(P9, B1!(P9, I1), 12) >= 6 Or B%(P9, B1!(P9, I1), 18) <> 0 Or H = 1 And A!(P9, B1!(P9, I1), 12) >= 2 Or H = 2 And A!(P9, B1!(P9, I1), 12) >= 4 Or H = 3 And A!(P9, B1!(P9, I1), 12) >= 5 Or H = 4 And T > 300 And A!(P9, B1!(P9, I1), 12) = 5 Then M4 = M4 + 1

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
Sub CreditAssists ()

    1054 'CREDIT ASSISTS
    R0 = Int(Rnd(1) * 100) + 1

    If R0 > F7%(P, 7) Then

        1055 '
        R0 = Int(Rnd(1) * F5%(P, 2)) + 1

        W5 = 0

        For M8 = 0 To 4
            W5 = W5 + A!(P, B1!(P, M8), 25)
            If R0 <= W5 Then 1059
        Next M8

        M8 = Int(Rnd(1) * 5)

        1059 '
        W5 = M8
        Locate 7, 25

        If AA$(P, B1!(P, B), 0) <> AA$(P, B1!(P, M8), 0) Then
            A!(P, B1!(P, M8), 14) = A!(P, B1!(P, M8), 14) + 1
        Else
            GoTo 1055
        End If

    End If

End Sub

'----------------------------------------
'    ReboundRoutine1 Subroutine
'----------------------------------------
'...explanation...
Sub ReboundRoutine1 ()

    790 '
    RB = 0

    For I = 0 To 4:

        RB(I) = 0

        'ReboundRoutine2 is practically identical
        'in everyway except the reference below
        'to array index 21 is 2, and it's clearly
        'operating for the other team as P is
        'replaced by 1-P (switching 1 to 0,
        'and vice versa)
        XZ = A!(P, B1!(P, I), 21)

        If Not (H > 2 Or H = 2 And T <= 120) Then
            'This line, referencing P, is the other one changed
            'between the 2 routines
            If A!(P, B1!(P, I), 3) - A!(P, B1!(P, I), 7) - A!(P, B1!(P, I), 11) - A!(P, B1!(P, I), 12) <= 0 Then XZ = XZ * .5
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
Sub ReboundRoutine2 ()

    795 '
    RB = 0

    For I = 0 To 4
        RB(I) = 0
        XZ = A!(1 - P, B1!(1 - P, I), 2)

        If Not (H > 2 Or H = 2 And T <= 120) Then
            If A!(1 - P, B1!(1 - P, I), 3) - A!(1 - P, B1!(1 - P, I), 7) - A!(1 - P, B1!(1 - P, I), 11) - A!(1 - P, B1!(1 - P, I), 12) <= 0 Then XZ = XZ * .5
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
        If T <= 0 Then TE = T: Call PlayingTime
    Else

        If pbpDelay! <> 0 Then
            For I = 8 To 15
                Locate I, 1
                Print Space$(38)
            Next I
        End If

        Locate 10
        Color 15, 0
        Print "<ENTER>-CONTINUE"
        Print "1-TIMEOUT "; B$(P); " -"; T1(P)

        Do

            813 I$ = GetKeyPress$
            P3 = Val(I$)

            If P3 <> 1 Then
                S2 = 0
                P9 = P
                C1 = 2
                If T <= 0 Then TE = T: Call PlayingTime
            Else
                If Not (T1(P) <= 0 Or P3 = 1 And P = CT And U5 = 1) Then T1(P) = T1(P) - 1: Color 15, 0: Call Substitutions
                'Exit Do
            End If

        Loop Until Not (P3 = 1 And T1(P) <= 0 Or P3 = 1 And P = CT And U5 = 1)

        'If P3 = 1 And T1(P) <= 0 Or P3 = 1 And P = CT And U5 = 1 Then 813

        818 '
        If pbpDelay! <> 0 Then
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

        If pbpDelay! <> 0 Then
            For I = 8 To 15
                Locate I, 1
                Print Space$(38)
            Next I
        End If

        Locate 10

        If U5 = 2 Then

            S2 = 0
            P9 = P
            C1 = 2

            If T <= 0 Then TE = T: Call PlayingTime

            Call RATINGTABLES

        Else

            Call NEWSCREEN
            Locate 10, 1
            Color 15, 0
            Print "<ENTER>-CONTINUE"
            Print "1-SUB "; B$(P)
            Print "2-SUB "; B$(D)

            Do
                I$ = GetKeyPress$
                P3 = Val(I$)
            Loop Until P3 >= 1 And P3 <= 2 Or I$ = Chr$(13)

            If P3 < 1 Or P3 > 2 Or U5 = 1 And P3 = 1 And P = CT Or U5 = 1 And P3 = 2 And D = CT Then

                S2 = 0
                P9 = P
                C1 = 2

                If T <= 0 Then TE = T: Call PlayingTime

                Call RATINGTABLES

                If LC% = 1 Then

                    LC% = 0
                    Call CheckZeroTime
                    Call NEWSCREEN

                    If pbpDelay! <> 0 Then

                        For I = 8 To 15
                            Locate I, 1
                            Print Space$(38)
                        Next I

                    End If
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

        For M8 = 0 To 4: W5 = W5 + A!(D, B1!(D, M8), 17)
            If M9 <= W5 Then
                Exit For
            End If
        Next M8

        If M8 < 0 Or M8 > 4 Then M8 = Int(Rnd(1) * 5)
        W5 = M8
    End If

    631 '
    A!(D, B1!(D, W5), 12) = A!(D, B1!(D, W5), 12) + 1
    P2(D) = P2(D) + 1

    Call UPDATESCREEN

    If A!(D, B1!(D, W5), 3) - A!(D, B1!(D, W5), 7) - A!(D, B1!(D, W5), 11) - A!(D, B1!(D, W5), 12) = 0 Then A!(D, B1!(D, W5), 3) = A!(D, B1!(D, W5), 3) + 1

    If A!(D, B1!(D, W5), 12) < 6 Then

        If PB% = 1 And pbpDelay! <> 0 Then
            Call WINDEX
            Print "FOUL ON "; AA$(D, B1!(D, W5), 0)
            Call MOREDELAY
            Print "HE HAS "; A!(D, B1!(D, W5), 12)
            Call MOREDELAY
        End If

        If PB% = 0 Then U$ = "FOUL NUMBER" + Str$(A!(D, B1!(D, W5), 12)) + " CALLED ON " + AA$(D, B1!(D, W5), 0): Call DELAY

        Call MOREDELAY

    Else
        'A!(D, B1!(D, W5), 12) >= 6 Then
        M9 = 0

        For I3 = 0 To 14
            If I3 <> B1!(D, W5) And AA$(D, I3, 1) <> "XXX" And B%(D, I3, 18) = 0 Then
                M9 = M9 + 1
            End If
        Next

        For I3 = 0 To 14
            If I3 <> B1!(D, W5) And AA$(D, I3, 1) <> "XXX" And B%(D, I3, 18) = 0 Then
                If A!(D, I3, 0) <= A!(D, B1!(D, W5), 0) Then A!(D, I3, 3) = A!(D, I3, 3) + Int((A!(D, B1!(D, W5), 3) / M9) + .5)
            End If
        Next I3

        If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print "FOUL ON "; AA$(D, B1!(D, W5), 0): Call MOREDELAY
        If PB% = 0 Then U$ = "FOUL CALLED ON " + AA$(D, B1!(D, W5), 0): Call DELAY
        U$ = "HE HAS JUST FOULED OUT !!!"

        If PB% = 0 Then Call DELAY
        If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call MOREDELAY

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

        If T <= 0 Then TE = T: Call PlayingTime

        X = 0

    End If

End Sub

'----------------------------------------
'       LastShot Subroutine
'----------------------------------------
'...explanation...
Sub LastShot ()

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

    Call PlayingTime

    T = T - M5
    IN = 0

    X8 = X8 - M5

    Call ZeroCheck

End Sub

'----------------------------------------
'          ZeroCheck Subroutine
'----------------------------------------
'...explanation...
Sub ZeroCheck ()

    If T <= .001 Then TE = T: Call PlayingTime: T = 0

    If X8 <= 0 Then X8 = 0

    Call UPDATESCREEN

End Sub

'----------------------------------------
'          BoxToScreen Subroutine
'----------------------------------------
'...explanation...
Sub BoxToScreen ()

    8515 '
    Call BOGUS(X)

    If X = 0 Then Cls

    For I = 0 To 1: Color T(I, 33), T(I, 34)

        Print B$(I): Color 14, 0
        Print "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB  RB  PT  F   A   B   S  TO"

        For I1 = 0 To 14
            If ZZ(I, I1) <> 0 Then
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
                Print Using "##"; A!(I, I1, 8);
                Print "-";
                Print Using "##"; A!(I, I1, 7);
                Print Using "  ##"; W%(I, I1, 0);
                Print "-";
                Print Using "##"; W%(I, I1, 1);
                Print Using "  ##"; A!(I, I1, 10);
                Print "-";: Print Using "##"; A!(I, I1, 9);
                Print Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; A!(I, I1, 24); A!(I, I1, 11); A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0); A!(I, I1, 12); A!(I, I1, 14); A!(I, I1, 15); A!(I, I1, 13); A!(I, I1, 23);

                If ZZ(I, I1) = 0 Then Locate , 15: Print "DNP" Else Print

                V9(0) = V9(0) + A!(I, I1, 8)
                V9(1) = V9(1) + A!(I, I1, 7)
                V9(2) = V9(2) + A!(I, I1, 10)
                V9(3) = V9(3) + A!(I, I1, 9)
                V9(4) = V9(4) + A!(I, I1, 11)
                V9(5) = V9(5) + (A!(I, I1, 8) * 2) + A!(I, I1, 10) + W%(I, I1, 0)
                V9(13) = V9(13) + W%(I, I1, 0)
                V9(14) = V9(14) + W%(I, I1, 1)
                V9(6) = TF(I): 'V9(6)+A!(I,I1,12)
                V9(7) = V9(7) + A!(I, I1, 14): V9(8) = V9(8) + A!(I, I1, 15): V9(9) = V9(9) + A!(I, I1, 13): V9(11) = V9(11) + A!(I, I1, 24): V9(12) = V9(12) + A!(I, I1, 23)
                V9(10) = V9(10) + ZZ(I, I1)
            End If
        Next I1

        If V9(3) = 0 Then V9(3) = 1

        Print
        Color 14: Print "TOTALS";
        Color 15: Print Tab(14); Int(V9(10) / 60);
        Locate , 21: Print Using "##"; V9(0);: Print "-";: Print Using "###"; V9(1);
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
                If A!(I, I1, 22) = 50 Then A!(I, I1, 22) = 0: A!(I, I1, 17) = A!(I, I1, 17) * 3: D1(I) = D1(I) - 1: MJ = 50
            Next I1
            If MJ = 50 Then
                MJ = 0
                F5%(I, 0) = 0
                For I1 = 0 To 4: F5%(I, 0) = F5%(I, 0) + A!(I, B1!(I, I1), 17): Next I1
            End If
        Next I
    End If

End Sub

'------------------------------
'    STATCONVERT Subroutine
'------------------------------
' ...explanation...
Sub STATCONVERT (Q, QQ)

    For XX = 4 To 11
        If Mid$(B$(Q), XX, 1) = " " Or Mid$(B$(Q), XX, 1) = "." Then Mid$(B$(Q), XX, 1) = "_"
    Next XX

    For XX = 12 To 14
        If Mid$(B$(Q), XX, 2) = "ST" Then Mid$(B$(Q), 9, 1) = "_": Mid$(B$(Q), 10, 2) = "ST"
    Next XX

    QQ = 8

    For XX = 11 To 4 Step -1
        If Mid$(B$(Q), XX, 1) = "_" Then
            QQ = XX - 4
        Else
            Exit For
        End If
    Next XX

End Sub

'------------------------------
'    LOADSTATS Subroutine
'------------------------------
' ...explanation...
Sub LOADSTATS ()

    For I = 0 To 1

        Cls

        Y$(I) = B$(I)
        Q = I

        Call STATCONVERT(Q, QQ)

        M$ = Mid$(B$(I), 4, QQ)
        R$ = M$ + "." + Left$(B$(I), 2)
        R$ = R$ + "S"

        If _FileExists(diskPaths$(1) + R$) Then

            Open diskPaths$(1) + R$ For Input As #2
            Input #2, YY%
            For I1 = 1 To 100: Input #2, O%(I1): Next
            For I1 = 1 To 100: Input #2, HO$(I1): Next
            For I1 = 1 To 100: Input #2, HT%(I1): Next
            For I1 = 1 To 100: Input #2, HO$(I1): Next
            Close 2

            For I1 = 1 To YY%
                If O%(I1) > HT%(I1) Then Z5(I) = Z5(I) + 1
                If O%(I1) < HT%(I1) Then Z6(I) = Z6(I) + 1
            Next

        End If

        B$(I) = Y$(I)

    Next

End Sub

'------------------------------
'      BOGUS Subroutine
'------------------------------
' ...explanation...
Sub BOGUS (X)

    If X = 0 Then
        If Not (AP = 1 And U5 = 2) Then
            I$ = GetKeyPress$
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
    Else
        Cls
        Input "ENTER FILENAME TO SAVE GAME SUMMARY UNDER "; NM$: NM$ = NM$ + ".SPC"
    End If

    Open diskPaths$(1) + NM$ For Output As #2

    If N <> 2 Then Print #2, "@ "; S$(N): Print #2,

    For I = 0 To 1

        Print #2, B$(I); Tab(17); Str$(S!(I, 1)); Tab(20); Str$(S!(I, 2)); Tab(23); Str$(S!(I, 3)); Tab(26); Str$(S!(I, 4));

        team0Score = S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4)
        team1Score = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4)

        If team0Score = team1Score Then
            Print #2, Tab(29); Str$(S!(I, 5));
        Else
            'team0Score <> team1Score
            Print #2, Tab(29); "- "; S!(I, 0)
            _Continue
        End If

        team0Score_OT = team1Score + S!(0, 5)
        team1Score_OT = team1Score + S!(1, 5)

        If team0Score_OT = team1Score_OT Then
            Print #2, Tab(32); Str$(S!(I, 6));
        Else
            'team0Score_OT <> team1Score_OT
            Print #2, Tab(32); "- "; S!(I, 0)
            _Continue
        End If

        Print #2, Tab(35); "- "; S!(I, 0)
    Next I

    For I = 0 To 1
        Print #2,: Print #2, B$(I)
        Print #2, "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB  RB  PT  F   A   B   S  TO"
        For I1 = 0 To 14

            If ZZ(I, I1) <> 0 Then

                Print #2, AA$(I, I1, 0); Tab(14);
                If ZZ(I, I1) = 0 Then
                    Print #2, Tab(15); "DNP";
                Else
                    Print #2, Using "##"; Int(ZZ(I, I1) / 60);: Print #2, ":";
                    If ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60 > 9 Then
                        Print #2, Using "##"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;
                    Else
                        Print #2, "0";: Print #2, Using "#"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;
                    End If
                End If

                Print #2, Tab(21);: Print #2, Using "##"; A!(I, I1, 8);
                Print #2, "-";: Print #2, Using "##"; A!(I, I1, 7);
                Print #2, Using "  ##"; W%(I, I1, 0);: Print #2, "-";
                Print #2, Using "##"; W%(I, I1, 1);
                Print #2, Using "  ##"; A!(I, I1, 10);
                Print #2, "-";: Print #2, Using "##"; A!(I, I1, 9);
                Print #2, Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; A!(I, I1, 24); A!(I, I1, 11); A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0); A!(I, I1, 12); A!(I, I1, 14); A!(I, I1, 15); A!(I, I1, 13); A!(I, I1, 23)
                V9(0) = V9(0) + A!(I, I1, 8)
                V9(1) = V9(1) + A!(I, I1, 7)
                V9(2) = V9(2) + A!(I, I1, 10)
                V9(3) = V9(3) + A!(I, I1, 9)
                V9(4) = V9(4) + A!(I, I1, 11)
                V9(5) = V9(5) + (A!(I, I1, 8) * 2) + A!(I, I1, 10) + W%(I, I1, 0)
                V9(13) = V9(13) + W%(I, I1, 0)
                V9(14) = V9(14) + W%(I, I1, 1)
                V9(6) = TF(I)
                V9(7) = V9(7) + A!(I, I1, 14)
                V9(8) = V9(8) + A!(I, I1, 15)
                V9(9) = V9(9) + A!(I, I1, 13)
                V9(11) = V9(11) + A!(I, I1, 24)
                V9(12) = V9(12) + A!(I, I1, 23)
                V9(10) = V9(10) + ZZ(I, I1)

            End If

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

    If N <> 2 Then LPrint "@ "; S$(N): LPrint

    For I = 0 To 1
        LPrint B$(I); Tab(17); Str$(S!(I, 1)); Tab(20); Str$(S!(I, 2)); Tab(23); Str$(S!(I, 3)); Tab(26); Str$(S!(I, 4));

        If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) Then LPrint Tab(29); Str$(S!(I, 5));

        If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) <> S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) Then
            LPrint Tab(29); "- "; S!(I, 0)
        Else
            If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) + S!(0, 5) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) + S!(1, 5) Then LPrint Tab(32); Str$(S!(I, 6));
            If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) + S!(0, 5) <> S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) + S!(1, 5) Then
                LPrint Tab(32); "- "; S!(I, 0)
            Else
                LPrint Tab(35); "- "; S!(I, 0)
            End If
        End If

    Next I

    Call BOGUS(X)

    If X = 0 Then Cls

    For I = 0 To 1
        LPrint: LPrint B$(I)
        LPrint "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB  RB  PT  F   A   B   S  TO"

        For I1 = 0 To 14

            If ZZ(I, I1) <> 0 Then

                LPrint AA$(I, I1, 0); Tab(14);

                If ZZ(I, I1) = 0 Then
                    LPrint Tab(15); "DNP";
                Else
                    LPrint Using "##"; Int(ZZ(I, I1) / 60);
                    LPrint ":";
                    If ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60 > 9 Then
                        LPrint Using "##"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;
                    Else
                        LPrint "0";: LPrint Using "#"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;
                    End If
                End If

                8918 LPrint Tab(21);
                LPrint Using "##"; A!(I, I1, 8);
                LPrint "-";
                LPrint Using "##"; A!(I, I1, 7);
                LPrint Using "  ##"; W%(I, I1, 0);
                LPrint "-";: LPrint Using "##"; W%(I, I1, 1);
                LPrint Using "  ##"; A!(I, I1, 10);
                LPrint "-";: LPrint Using "##"; A!(I, I1, 9);
                LPrint Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; A!(I, I1, 24); A!(I, I1, 11); A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0); A!(I, I1, 12); A!(I, I1, 14); A!(I, I1, 15); A!(I, I1, 13); A!(I, I1, 23)

                V9(0) = V9(0) + A!(I, I1, 8)
                V9(1) = V9(1) + A!(I, I1, 7)
                V9(2) = V9(2) + A!(I, I1, 10)
                V9(3) = V9(3) + A!(I, I1, 9)
                V9(4) = V9(4) + A!(I, I1, 11)
                V9(5) = V9(5) + (A!(I, I1, 8) * 2) + A!(I, I1, 10) + W%(I, I1, 0)
                V9(13) = V9(13) + W%(I, I1, 0)
                V9(14) = V9(14) + W%(I, I1, 1)
                V9(6) = TF(I)
                V9(7) = V9(7) + A!(I, I1, 14)
                V9(8) = V9(8) + A!(I, I1, 15)
                V9(9) = V9(9) + A!(I, I1, 13)
                V9(11) = V9(11) + A!(I, I1, 24)
                V9(12) = V9(12) + A!(I, I1, 23)
                V9(10) = V9(10) + ZZ(I, I1)

            End If

        Next I1

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
    Open diskPaths$(1) + NM$ For Output As #2

    If H - 1 > E%(1) Then Print #2, Using "MORE THAN ## QUARTERS"; E%(1)
    If S!(1, 0) + S!(0, 0) > E%(2) Then Print #2, Using "MORE THAN ### COMBINED POINTS"; E%(2)
    If R3(0) + R3(1) > E%(3) Then Print #2, Using "MORE THAN ### COMBINED REBOUNDS"; E%(3)
    If S!(0, 0) > E%(4) Then Print #2, B$(0);: Print #2, Using " SCORED MORE THAN ### POINTS"; E%(4)
    If S!(1, 0) > E%(4) Then Print #2, B$(1);: Print #2, Using " SCORED MORE THAN ### POINTS"; E%(4)
    If R3(0) > E%(5) Then Print #2, B$(0);: Print #2, Using " HAD MORE THAN ### REBOUNDS"; E%(5)
    If R3(1) > E%(5) Then Print #2, B$(1);: Print #2, Using " HAD MORE THAN ### REBOUNDS"; E%(5)

    For I = 0 To 1
        For I1 = 0 To 14
            If A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) > E%(6) Then Print #2, AA$(I, I1, 0);: Print #2, Using " SCORED MORE THAN ### POINTS"; E%(6)
            If A!(I, I1, 11) > E%(7) Then Print #2, AA$(I, I1, 0);: Print #2, Using " HAD MORE THAN ### REBOUNDS"; E%(7)
            If A!(I, I1, 14) > E%(8) Then Print #2, AA$(I, I1, 0);: Print #2, Using " HAD MORE THAN ### ASSISTS"; E%(8)
            If A!(I, I1, 15) > E%(9) Then Print #2, AA$(I, I1, 0);: Print #2, Using " BLOCKED MORE THAN ### SHOTS"; E%(9)
            If A!(I, I1, 13) > E%(10) Then Print #2, AA$(I, I1, 0);: Print #2, Using " HAD MORE THAN ### STEALS"; E%(10)
            If W%(I, I1, 0) > E%(11) Then Print #2, AA$(I, I1, 0);: Print #2, Using " MADE MORE THAN ### 3-PT FIELD GOALS"; E%(11)
        Next
    Next

    Print #2,

    If N = 2 Then Print #2, "NEUTRAL SITE": Print #2,
    If N <> 2 Then Print #2, "@ "; S$(N): Print #2,

    For I = 0 To 1
        Print #2, B$(I); Tab(17); Str$(S!(I, 1)); Tab(20); Str$(S!(I, 2)); Tab(23); Str$(S!(I, 3)); Tab(26); Str$(S!(I, 4));

        If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) Then Print #2, Tab(29); Str$(S!(I, 5));
        If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) <> S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) Then
            Print #2, Tab(29); "- "; S!(I, 0)
        Else
            If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) + S!(0, 5) = S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) + S!(1, 5) Then Print #2, Tab(32); Str$(S!(I, 6));
            If S!(0, 1) + S!(0, 2) + S!(0, 3) + S!(0, 4) + S!(0, 5) <> S!(1, 1) + S!(1, 2) + S!(1, 3) + S!(1, 4) + S!(1, 5) Then
                Print #2, Tab(32); "- "; S!(I, 0)
            Else
                Print #2, Tab(35); "- "; S!(I, 0)
            End If
        End If
    Next I

    For I = 0 To 1

        Print #2,: Print #2, B$(I)
        Print #2, "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB  RB  PT  F   A   B   S  TO"

        For I1 = 0 To 14

            If ZZ(I, I1) <> 0 Then

                Print #2, AA$(I, I1, 0); Tab(14);

                If ZZ(I, I1) = 0 Then
                    Print #2, Tab(15); "DNP";
                Else
                    Print #2, Using "##"; Int(ZZ(I, I1) / 60);: Print #2, ":";
                    If ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60 > 9 Then
                        Print #2, Using "##"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;
                    Else
                        Print #2, "0";: Print #2, Using "#"; ZZ(I, I1) - Int(ZZ(I, I1) / 60) * 60;
                    End If
                End If

                8868 '
                Print #2, Tab(21);: Print #2, Using "##"; A!(I, I1, 8);
                Print #2, "-";: Print #2, Using "##"; A!(I, I1, 7);
                Print #2, Using "  ##"; W%(I, I1, 0);: Print #2, "-";
                Print #2, Using "##"; W%(I, I1, 1);
                Print #2, Using "  ##"; A!(I, I1, 10);
                Print #2, "-";: Print #2, Using "##"; A!(I, I1, 9);
                Print #2, Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; A!(I, I1, 24); A!(I, I1, 11); A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0); A!(I, I1, 12); A!(I, I1, 14); A!(I, I1, 15); A!(I, I1, 13); A!(I, I1, 23)

                V9(0) = V9(0) + A!(I, I1, 8)
                V9(1) = V9(1) + A!(I, I1, 7)
                V9(2) = V9(2) + A!(I, I1, 10)
                V9(3) = V9(3) + A!(I, I1, 9)
                V9(4) = V9(4) + A!(I, I1, 11)
                V9(5) = V9(5) + (A!(I, I1, 8) * 2) + A!(I, I1, 10) + W%(I, I1, 0)
                V9(13) = V9(13) + W%(I, I1, 0)
                V9(14) = V9(14) + W%(I, I1, 1)
                V9(6) = TF(I)
                V9(7) = V9(7) + A!(I, I1, 14)
                V9(8) = V9(8) + A!(I, I1, 15)
                V9(9) = V9(9) + A!(I, I1, 13)
                V9(11) = V9(11) + A!(I, I1, 24)
                V9(12) = V9(12) + A!(I, I1, 23)
                V9(10) = V9(10) + ZZ(I, I1)
            End If
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

        8871 '
        If X = 0 Then
            If AP = 1 And U5 = 2 Then
                If X = 0 Then Cls
            Else
                I$ = GetKeyPress$
            End If
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

    Locate 9, 23
    Print "SAVE STATS FOR EITHER TEAM (YN)";
    Color 15, 0

    If AP <> 1 Then
        Do
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        If UCase$(I$) = "Y" Then
            Cls
            Color 12: Print Tab(30); "COMPILATION OF STATS"
            Print
            Color 14: Print "(0) ";: Color 15: Print "BOTH TEAMS"
            Print
            Color 14: Print "(1) ";: Color T(0, 33), T(0, 34): Print B$(0); " ONLY"
            Color , 0: Print
            Color 14: Print "(2) ";: Color T(1, 33), T(1, 34): Print B$(1); " ONLY"
            Color , 0: Print

            Do
                J$ = GetKeyPress$
                X1 = Val(J$)
            Loop Until X1 >= 0 And X1 <= 2

        End If

    End If

    If UCase$(I$) = "Y" Or AP = 1 Then
        Call SAVESTATFILES
    End If

End Sub

'------------------------------
'   DISABLEPLAYERS Subroutine
'------------------------------
' ...explanation...
Sub DISABLEPLAYERS ()
    Cls
    Print "DISABLE UP TO 6 PLAYERS FOR ";: Color T(P9, 33), T(P9, 34): Print B$(P9);
    Color 15, 0: Print " (YN)";
    Print
    Print
    Print "NOTE:: THE COMPUTER WILL AUTOMATICALLY DISABLE PLAYERS BASED ON"
    Print "       THE RATIO OF GAMES PLAYED FOR EACH PLAYER TO THE TOTAL"
    Print "       NUMBER OF GAMES PLAYED BY THE TEAM. YOU MAY WANT TO CHECK"
    Print "       TO ACTIVATE ANY COMPUTER DEACTIVATED PLAYERS"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If UCase$(I$) = "Y" Then

        DX% = 0
        Cls
        Print "TEAM ROSTER FOR ";: Color T(P9, 33), T(P9, 34): Print B$(P9)
        Color 15, 0
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

        Do
            9203 Locate 19, 1: Print "ENTER PLAYER # TO DISABLE/ENABLE (Z=FINISHED)"

            I$ = GetKeyPress$

            If UCase$(I$) <> "Z" Then

                I1 = Val(I$)
                If UCase$(I$) = "A" Then I1 = 10
                If UCase$(I$) = "B" Then I1 = 11
                If UCase$(I$) = "C" Then I1 = 12
                If UCase$(I$) = "D" Then I1 = 13
                If UCase$(I$) = "E" Then I1 = 14

                If B%(P9, I1, 18) = 0 And DX% < 6 Then
                    B%(P9, I1, 18) = -1
                    DX% = DX% + 1
                    Locate 3 + I1, 3: Color T(P9, 33), T(P9, 34): Print AA$(P9, I1, 0)
                    Color 15, 0
                Else
                    If B%(P9, I1, 18) = -1 Then
                        B%(P9, I1, 18) = 0
                        DX% = DX% - 1
                        Locate 3 + I1, 3: Color 15, 0: Print AA$(P9, I1, 0)
                    End If
                End If
            End If

        Loop Until UCase$(I$) = "Z"

    End If

End Sub

'------------------------------
'      DRAWBOX Subroutine
'------------------------------
' ...explanation...
Sub DRAWBOX (I)
    JJ = 1

    Locate 6, 8: Color T(I, 33), T(I, 34)

    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

    For II = 1 To JJ
        Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    Locate 7 + JJ, 8

    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";
    Locate 7

End Sub

'------------------------------
'      GETATEAM Subroutine
'------------------------------
' ...explanation...
Sub GETATEAM (I1, YN$)

    'Games Played, Minutes Per Game
    Dim GP!, MPG!

    Open diskPaths$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031

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

    B$(P9) = RTrim$(Q$(0))

    'Mascot / Alt Name
    A1$(P9) = Q$(1)

    For I = 0 To 6
        L%(P9, I) = CVI(Q$(I + 2))
    Next

    For I = 0 To 34
        T(P9, I) = CVI(Q$(I + 10))
    Next

    For I = 0 To 14
        AA$(P9, I, 0) = RTrim$(Q$(22 * I + 45))
        AA$(P9, I, 1) = RTrim$(Q$(22 * I + 46))
        For I1 = 0 To 19
            B%(P9, I, I1) = CVI(Q$(22 * I + 47 + I1))
        Next
    Next

    'Team Abbreviation
    ABB$(P9) = RTrim$(Q$(375))

    'Coach
    C$(P9) = RTrim$(Q$(376))

    'Arena
    S$(P9) = RTrim$(Q$(377))

    Close 1

    For I = 0 To 14

        If B%(P9, I, 16) < 0 And B%(P9, I, 17) < 0 Then
            GP! = B%(P9, I, 16) / B%(P9, I, 0)
            MPG! = B%(P9, I, 17) / B%(P9, I, 1)

            For I1 = 2 To 14
                B%(P9, I, I1) = B%(P9, I, I1) * GP! * MPG!
                B%(P9, I, 0) = B%(P9, I, 16) * -1: B%(P9, I, 1) = B%(P9, I, 17) * -1

            Next I1

        End If

    Next I

    If Not (U5 = 0 Or U5 = 1 And P9 <> CT Or AP = 1 And U5 = 0 Or AP = 1 And U5 = 1 And P9 <> CT) Then

        While (CNT% < 10)
            CNT% = 0
            For I = 0 To 14
                If B%(P9, I, 18) = -1 Then B%(P9, I, 18) = 0
                RN = Int(Rnd(1) * 100) + 1
                If UCase$(AA$(P9, I, 0)) = "XXX" Or (B%(P9, I, 18) = 0 And RN > Int(B%(P9, I, 0) / L%(P9, 0) * 100 + .5)) Then B%(P9, I, 18) = -1
            Next

            For I = 0 To 14:
                If B%(P9, I, 18) = 0 Then CNT% = CNT% + 1
            Next
        Wend

    End If

End Sub

'------------------------------
'      NEWSCREEN Subroutine
'------------------------------
' ...explanation...
Sub NEWSCREEN ()

    ' *** SCREEN UPDATING ***
    Cls

    Call TICKERUPDATE

    Color 9, 0

    Locate 3, 11: Print Chr$(218); String$(5, 196); Chr$(191)
    Locate , 11: Print Chr$(179);: Locate , 17: Print Chr$(179)
    Locate , 11: Print Chr$(192); String$(5, 196); Chr$(217)
    Locate 3, 58: Print Chr$(218); String$(5, 196); Chr$(191)
    Locate , 58: Print Chr$(179);: Locate , 64: Print Chr$(179)
    Locate , 58: Print Chr$(192); String$(5, 196); Chr$(217)
    Locate 2, 1
    Locate , 9: Color T(0, 33), T(0, 34): Print B$(0);
    Locate 1, 37
    Color 15, 0: Print Using "##"; Int(T / 60);: Print ":";

    If T - Int(T / 60) * 60 < 10 Then
        Print "0";
        Print Using "#"; T - Int(T / 60) * 60;
    Else
        Print Using "##"; T - Int(T / 60) * 60;
    End If

    Locate 2, 56: Color T(1, 33), T(1, 34): Print B$(1)
    Locate 4, 13: Color 14, 0
    Print Using "###"; S!(0, 0);
    Locate 2, 35: Print Using " PERIOD #"; H;

    If H > 4 Then Locate , 35: Print Using "OVERTIME #"; H - 4
    Locate 4, 60: Color 14, 0: Print Using "###"; S!(1, 0)
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

    Locate 6, 9: Print "TIMEOUTS:"; T1(0);
    Locate , 56: Print "TIMEOUTS:"; T1(1)
    Locate 8, 58
    Color T(D, 33), T(D, 34): Print "DEF - "; defenseStyles$(D2(D))
    Locate , 58
    Color T(P, 33), T(P, 34): Print "OFF - "; offenseStyles$(O0%(P))
    Color 15, 0
    Locate , 41: Color 14: Print "(1) ";: Color 15: Print "SHOOT        ";: Color 14: Print "ENTER";: Color 15: Print "-PASS"
    Locate , 41: Color 14: Print "(2) ";: Color 15: Print B$(P); " CALL TIMEOUT     "
    Locate , 41: Color 14: Print "(3) ";: Color 15: Print "COMMIT INTL FOUL (LAST 4 MIN)"
    Locate , 41: Color 14: Print "(4) ";: Color 15: Print "SHOOT A FORCED 3-POINTER"
    Locate , 41: Color 14: Print "(S) ";: Color 15: Print "SCOUT REPORT ";
    Color 14: Print "(T) ";: Color 15: Print "10 MIN TICKER"
    Locate , 41: Color 14: Print "(Z) ";: Color 15: Print "LEFT TM STGY ";
    Color 14: Print "(/) ";: Color 15: Print "RIGHT TM STGY"

    Locate 7, 1
    Color 9, 0: Print String$(38, 205); Chr$(203); String$(38, 205)
    Locate 8, 1
    For I = 1 To 8: Locate , 39: Print Chr$(186): Next
    Locate 17, 1: Color T(0, 33), T(0, 34): Print B$(0);
    Print Using " [##"; Z5(0);: Print "-";: Print Using "##]"; Z6(0)
    Color 14, 0: Print "# NAME         FG FT RB CX PA DF PT  F"

    For I1 = 0 To 4
        Color 2: Print Using "# "; I1;
        Color 3
        Color 15: Print Left$(AA$(0, B1!(0, I1), 0), 12);: Locate , 16
        J = (A!(0, B1!(0, I1), 3)) - A!(0, B1!(0, I1), 7) - A!(0, B1!(0, I1), 11) - A!(0, B1!(0, I1), 12)
        If J <= 0 Then J = 0
        Print Using "##"; A!(0, B1!(0, I1), 0);
        Print Using "### "; A!(0, B1!(0, I1), 1);
        If P = 0 Then Print Using "##"; A!(0, B1!(0, I1), 21) * 2 / 10; Else Print Using "##"; A!(0, B1!(0, I1), 2) * 2 / 10;
        If J <= 0 And (H = 1 Or H = 2 Or H = 3 Or H = 4 And T > 120) Then Color 12
        Print Using "### "; J;
        Color 15
        Print Using "## ##"; A!(0, B1!(0, I1), 4) / 10; A!(0, B1!(0, I1), 5) / 10;
        Print Using "###  "; A!(0, B1!(0, I1), 8) * 2 + A!(0, B1!(0, I1), 10) + W%(0, B1!(0, I1), 0);
        If A!(0, B1!(0, I1), 12) >= 5 Then Color 12
        Print Using "#"; A!(0, B1!(0, I1), 12)
        Color 15
    Next I1

    Locate 17, 40
    Color T(1, 33), T(1, 34): Print B$(1);: Print Using " [##"; Z5(1);: Print "-";: Print Using "##]"; Z6(1)
    Color 14, 0
    Locate , 40: Print "# NAME          FG FT RB CX PA DF PT  F"

    For I1 = 0 To 4: Locate , 40: Color 2: Print Using "# "; I1;
        Color 15
        Print Left$(AA$(1, B1!(1, I1), 0), 12);
        Locate , 56

        J = (A!(1, B1!(1, I1), 3)) - A!(1, B1!(1, I1), 7) - A!(1, B1!(1, I1), 11) - A!(1, B1!(1, I1), 12)

        If J <= 0 Then J = 0

        Print Using "##"; A!(1, B1!(1, I1), 0);
        Print Using "### "; A!(1, B1!(1, I1), 1);

        If P = 1 Then Print Using "##"; A!(1, B1!(1, I1), 21) * 2 / 10; Else Print Using "##"; A!(1, B1!(1, I1), 2) * 2 / 10;
        If J <= 0 And (H = 1 Or H = 2 Or H = 3 Or H = 4 And T > 120) Then Color 12

        Print Using "### "; J;
        Color 15
        Print Using "## ##"; A!(1, B1!(1, I1), 4) / 10; A!(1, B1!(1, I1), 5) / 10;
        Print Using "###  "; A!(1, B1!(1, I1), 8) * 2 + A!(1, B1!(1, I1), 10) + W%(1, B1!(1, I1), 0);
        If A!(1, B1!(1, I1), 12) >= 5 Then Color 12
        Print Using "#"; A!(1, B1!(1, I1), 12)
        Color 15
    Next I1

    Color 9, 0
    For I = 1 To 9: Locate 14 + I, 39: Print Chr$(186): Next
    Color 9, 0

    Locate 16, 1: Print String$(79, 205)
    Locate 16, 39: Print Chr$(206)

    Color 7, 0

End Sub

'------------------------------
'   QUARTERTOTAL Subroutine
'------------------------------
' ...explanation...
Sub QUARTERTOTAL ()

    For I = 0 To 1
        For I2 = 0 To 14: V9(I2) = 0: Next
        For I1 = 0 To 14
            V9(0) = V9(0) + A!(I, I1, 8)
            V9(1) = V9(1) + A!(I, I1, 7)
            V9(2) = V9(2) + A!(I, I1, 10)
            V9(3) = V9(3) + A!(I, I1, 9)
            V9(4) = V9(4) + A!(I, I1, 11)
            V9(5) = V9(5) + (A!(I, I1, 8) * 2) + A!(I, I1, 10) + W%(I, I1, 0)
            V9(13) = V9(13) + W%(I, I1, 0)
            V9(14) = V9(14) + W%(I, I1, 1)
            V9(6) = TF(I)
            V9(7) = V9(7) + A!(I, I1, 14)
            V9(8) = V9(8) + A!(I, I1, 15)
            V9(9) = V9(9) + A!(I, I1, 13)
            V9(11) = V9(11) + A!(I, I1, 24)
            V9(12) = V9(12) + A!(I, I1, 23)
            V9(10) = V9(10) + ZZ(I, I1)
        Next
        If H = 1 Or H = 2 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2)
            Next
            For I1 = 0 To 14
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A!(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11)
                QQ(I, H, I1, 12) = A!(I, I1, 24) - QQ(I, H - 1, I1, 12)
                QQ(I, H, I1, 13) = A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13)
                QQ(I, H, I1, 14) = A!(I, I1, 23) - QQ(I, H - 1, I1, 14)
            Next
        End If
        If H = 3 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2)
            Next
            For I1 = 0 To 14
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A!(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11)
                QQ(I, H, I1, 12) = A!(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12)
                QQ(I, H, I1, 13) = A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13)
                QQ(I, H, I1, 14) = A!(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14)
            Next
        End If
        If H = 4 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2) - QR(I, H - 3, I2)
            Next
            For I1 = 0 To 14
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A!(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6) - QQ(I, H - 3, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10) - QQ(I, H - 3, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11) - QQ(I, H - 3, I1, 11)
                QQ(I, H, I1, 12) = A!(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12) - QQ(I, H - 3, I1, 12)
                QQ(I, H, I1, 13) = A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13) - QQ(I, H - 3, I1, 13)
                QQ(I, H, I1, 14) = A!(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14) - QQ(I, H - 3, I1, 14)
            Next
        End If
        If H = 5 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2) - QR(I, H - 3, I2) - QR(I, H - 4, I2)
            Next
            For I1 = 0 To 14
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A!(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6) - QQ(I, H - 3, I1, I2 - 6) - QQ(I, H - 4, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10) - QQ(I, H - 3, I1, 10) - QQ(I, H - 4, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11) - QQ(I, H - 3, I1, 11) - QQ(I, H - 4, I1, 11)
                QQ(I, H, I1, 12) = A!(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12) - QQ(I, H - 3, I1, 12) - QQ(I, H - 4, I1, 12)
                QQ(I, H, I1, 13) = A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13) - QQ(I, H - 3, I1, 13) - QQ(I, H - 4, I1, 13)
                QQ(I, H, I1, 14) = A!(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14) - QQ(I, H - 3, I1, 14) - QQ(I, H - 4, I1, 14)
            Next
        End If
        If H = 6 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2) - QR(I, H - 3, I2) - QR(I, H - 4, I2) - QR(I, H - 5, I2)
            Next
            For I1 = 0 To 14
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A!(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6) - QQ(I, H - 3, I1, I2 - 6) - QQ(I, H - 4, I1, I2 - 6) - QQ(I, H - 5, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10) - QQ(I, H - 3, I1, 10) - QQ(I, H - 4, I1, 10) - QQ(I, H - 5, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11) - QQ(I, H - 3, I1, 11) - QQ(I, H - 4, I1, 11) - QQ(I, H - 5, I1, 11)
                QQ(I, H, I1, 12) = A!(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12) - QQ(I, H - 3, I1, 12) - QQ(I, H - 4, I1, 12) - QQ(I, H - 5, I1, 12)
                QQ(I, H, I1, 13) = A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13) - QQ(I, H - 3, I1, 13) - QQ(I, H - 4, I1, 13) - QQ(I, H - 5, I1, 13)
                QQ(I, H, I1, 14) = A!(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14) - QQ(I, H - 3, I1, 14) - QQ(I, H - 4, I1, 14) - QQ(I, H - 5, I1, 14)
            Next
        End If
        If H = 7 Then
            For I2 = 0 To 14
                QR(I, H, I2) = V9(I2) - QR(I, H - 1, I2) - QR(I, H - 2, I2) - QR(I, H - 3, I2) - QR(I, H - 4, I2) - QR(I, H - 5, I2) - QR(I, H - 6, I2)
            Next
            For I1 = 0 To 14
                For I2 = 7 To 15
                    QQ(I, H, I1, I2 - 6) = A!(I, I1, I2) - QQ(I, H - 1, I1, I2 - 6) - QQ(I, H - 2, I1, I2 - 6) - QQ(I, H - 3, I1, I2 - 6) - QQ(I, H - 4, I1, I2 - 6) - QQ(I, H - 5, I1, I2 - 6) - QQ(I, H - 6, I1, I2 - 6)
                Next
                QQ(I, H, I1, 10) = W%(I, I1, 0) - QQ(I, H - 1, I1, 10) - QQ(I, H - 2, I1, 10) - QQ(I, H - 3, I1, 10) - QQ(I, H - 4, I1, 10) - QQ(I, H - 5, I1, 10) - QQ(I, H - 6, I1, 10)
                QQ(I, H, I1, 11) = W%(I, I1, 1) - QQ(I, H - 1, I1, 11) - QQ(I, H - 2, I1, 11) - QQ(I, H - 3, I1, 11) - QQ(I, H - 4, I1, 11) - QQ(I, H - 5, I1, 11) - QQ(I, H - 6, I1, 11)
                QQ(I, H, I1, 12) = A!(I, I1, 24) - QQ(I, H - 1, I1, 12) - QQ(I, H - 2, I1, 12) - QQ(I, H - 3, I1, 12) - QQ(I, H - 4, I1, 12) - QQ(I, H - 5, I1, 12) - QQ(I, H - 6, I1, 12)
                QQ(I, H, I1, 13) = A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) - QQ(I, H - 1, I1, 13) - QQ(I, H - 2, I1, 13) - QQ(I, H - 3, I1, 13) - QQ(I, H - 4, I1, 13) - QQ(I, H - 5, I1, 13) - QQ(I, H - 6, I1, 13)
                QQ(I, H, I1, 14) = A!(I, I1, 23) - QQ(I, H - 1, I1, 14) - QQ(I, H - 2, I1, 14) - QQ(I, H - 3, I1, 14) - QQ(I, H - 4, I1, 14) - QQ(I, H - 5, I1, 14) - QQ(I, H - 6, I1, 14)
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
            F5%(I, 0) = F5%(I, 0) + A!(I, B1!(I, I1), 17)
            F5%(I, 1) = F5%(I, 1) + B%(I, B1!(I, I1), 18)
            F5%(I, 2) = F5%(I, 2) + A!(I, B1!(I, I1), 25)
            F5%(I, 3) = F5%(I, 3) + A!(I, B1!(I, I1), 5)
            F5%(I, 4) = F5%(I, 4) + A!(I, B1!(I, I1), 4) / 10
        Next I1
        F5%(I, 4) = Int((F5%(I, 4) / (F7%(I, 6) * .9)) * 100 + .5)
    Next

End Sub

'------------------------------
'    SAVESTATFILES Subroutine
'------------------------------
' ...explanation...
Sub SAVESTATFILES ()

    Dim TG!

    'SAVE STATS
    If _FileExists("GAME82") Then
        Open "GAME82" For Input As #2
        Input #2, FF$
        For I = 0 To 2: Input #2, AP%(I): Next
        Close 2

        If AP%(2) <= 31 Then
            For X = 0 To 1: SX$(AP%(2) + 1, X) = B$(X): Next

            Open "TICKER" For Output As #2
            For I = 1 To 32: For I1 = 0 To 1: For I2 = 1 To 14
            Print #2, SX%(I, I1, I2): Next: Next: Next
            For I = 1 To 32: For I1 = 0 To 1: Print #2, SX$(I, I1): Next: Next
            For I = 1 To 32: Print #2, ST%(I): Next
            Close 2
        End If

        Open diskPaths$(0) + FF$ For Random As #1 Len = 48
        For X = 0 To 18
            Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
        Next

        Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)

        NG%(13) = 1

        For X = 0 To 18: LSet Q$(X + 1) = MKI$(NG%(X)): Next
        For X = 0 To 1: LSet Q$(20 + X) = YN$(X): Next

        LSet Q$(22) = MKI$(S!(0, 0))
        LSet Q$(23) = MKI$(S!(1, 0))
        Put #1, AP%(0) + AP%(2)
        Close 1

        AP%(2) = AP%(2) + 1
        AP = 1
        If AP%(1) < AP%(0) + AP%(2) Then
            Kill "GAME82"
            AD = 1
        Else
            Open "GAME82" For Output As #2
            Print #2, FF$
            For I = 0 To 2: Print #2, AP%(I): Next
            Close 2
        End If
    End If

    Cls
    HH% = N

    For PX = 0 To 1
        For J = 0 To 14
            A!(PX, J, 16) = ZZ(PX, J)
            A!(PX, J, 17) = A!(PX, J, 23)
            A!(PX, J, 18) = A!(PX, J, 24)
        Next
    Next

    For PX = 0 To 1
        For I = 0 To 14
            For J = 1 To 12
                A!(PX, I, J) = A!(PX, I, J + 6)
            Next
            W0!(PX, I) = W%(PX, I, 0)
            W1!(PX, I) = W%(PX, I, 1)
        Next
        Y$(PX) = B$(PX)
    Next

    Q = 0

    Do
        6115 '
        C% = 0

        If AP <> 1 Then
            I$ = "Y"

            If X1 = 1 And Q = 1 Or X1 = 2 And Q = 0 Then I$ = "N"

            If UCase$(I$) = "N" Then
                C% = 1
            Else
                Cls
            End If
        End If

        If C% <> 1 Then

            For XX = 4 To 11
                If Mid$(B$(Q), XX, 1) = " " Or Mid$(B$(Q), XX, 1) = "." Then Mid$(B$(Q), XX, 1) = "_"
            Next XX

            For XX = 12 To 14
                If Mid$(B$(Q), XX, 2) = "ST" Then Mid$(B$(Q), 9, 1) = "_": Mid$(B$(Q), 10, 2) = "ST"
            Next XX

            QQ = 8

            For XX = 11 To 4 Step -1
                If Mid$(B$(Q), XX, 1) = "_" Then
                    QQ = XX - 4
                Else
                    Exit For
                End If
            Next XX

            F$ = Mid$(B$(Q), 4, QQ)
            R$ = F$ + "." + Left$(B$(Q), 2)

            If _FileExists(diskPaths$(1) + R$) Then
                Open diskPaths$(1) + R$ For Input As #2
                Input #2, B$(Q)
                For I = 0 To 14: Input #2, AA$(Q, I, 0)
                    For J = 0 To 13: Input #2, Z2!(I, J): Next J
                Input #2, W2(Q, I): Input #2, W3(Q, I): Next I
                For L = 0 To 15: Input #2, Z!(L): Next L
                For L = 0 To 15: Input #2, Z1!(L): Next L
                Close #2
            End If

            TG! = 0

            For I = 0 To 14
                If A!(Q, I, 2) * 2 + A!(Q, I, 4) + W0!(Q, I) > Z2!(I, 13) Then Z2!(I, 13) = A!(Q, I, 2) * 2 + A!(Q, I, 4) + W0!(Q, I)
                If A!(Q, I, 10) > 0 Then Z2!(I, 0) = Z2!(I, 0) + 1

                For J = 1 To 12
                    Z2!(I, J) = Z2!(I, J) + A!(Q, I, J)

                    If Not (J = 5 Or J = 6 Or J = 10 Or J = 11) Then
                        Z!(J) = Z!(J) + A!(Q, I, J): Z1!(J) = Z1!(J) + A!(1 - Q, I, J)
                    End If
                Next J

                Z!(0) = Z!(0) + A!(Q, I, 10)
                TG! = TG! + A!(Q, I, 10)
            Next I

            For I = 0 To 1: V9(I) = 0: For I1 = 0 To 14: V9(I) = V9(I) + A!(I, I1, 5): Next: Next

            Z!(15) = Z!(15) + R3(Q) - V9(Q): Z1!(15) = Z1!(15) + R3(1 - Q) - V9(1 - Q)
            Z!(6) = Z!(6) + TF(Q): Z1!(6) = Z1!(6) + TF(1 - Q)
            Z!(5) = Z!(5) + R3(Q): Z1!(5) = Z1!(5) + R3(1 - Q): Z!(11) = Z!(11) + T0(Q): Z1!(11) = Z1!(11) + T0(1 - Q)

            For J = 0 To 14:
                Z!(13) = Z!(13) + W0!(Q, J)
                Z!(14) = Z!(14) + W1!(Q, J)
                Z1!(13) = Z1!(13) + W0!(1 - Q, J)
                Z1!(14) = Z1!(14) + W1!(1 - Q, J)
                W2(Q, J) = W2(Q, J) + W0!(Q, J)
                W3(Q, J) = W3(Q, J) + W1!(Q, J)
            Next J

            Z!(10) = Z!(10) + S!(Q, 0)
            Z1!(10) = Z1!(10) + S!(1 - Q, 0)
            I = Q

            Call DRAWBOX(I)

            U$ = "SAVING STATS FOR " + B$(Q)
            Locate , (80 - Len(U$)) / 2
            Print U$
            Color 15, 0

            Open diskPaths$(1) + R$ For Output As #1

            Print #1, Y$(Q)

            For I = 0 To 14: Print #1, AA$(Q, I, 0)
                For J = 0 To 13: Print #1, Z2!(I, J): Next J
                Print #1, W2(Q, I)
                Print #1, W3(Q, I)
            Next I

            For L = 0 To 15
                Print #1, Z!(L)
            Next L

            For L = 0 To 15: Print #1, Z1!(L): Next L

            Close #1

            Y% = 0
            R$ = R$ + "S"

            If _FileExists(diskPaths$(1) + R$) Then
                Open diskPaths$(1) + R$ For Input As #1
                Input #1, Y%
                For I = 1 To 100: Input #1, O%(I): Next I
                For I = 1 To 100: Input #1, HO$(I): Next I
                For I = 1 To 100: Input #1, HT%(I): Next I
                For I = 1 To 100: Input #1, H$(I): Next I
                Close #1
            End If

            If Y% < 100 Then Y% = Y% + 1

            Select Case HH%
                Case 2
                    Select Case TG!
                        Case Is < 14500
                            H$(Y%) = "N"
                        Case 14501 To 16000
                            H$(Y%) = "N1"
                        Case 16001 To 17500
                            H$(Y%) = "N2"
                        Case 17501 To 19000
                            H$(Y%) = "N3"
                        Case 19001 To 20500
                            H$(Y%) = "N4"
                    End Select

                Case Is = Q
                    Select Case TG!
                        Case Is < 14500
                            H$(Y%) = "H"
                        Case 14501 To 16000
                            H$(Y%) = "H1"
                        Case 16001 To 17500
                            H$(Y%) = "H2"
                        Case 17501 To 19000
                            H$(Y%) = "H3"
                        Case 19001 To 20500
                            H$(Y%) = "H4"
                    End Select

                Case Is <> Q
                    Select Case TG!
                        Case Is < 14500
                            H$(Y%) = "V"
                        Case 14501 To 16000
                            H$(Y%) = "V1"
                        Case 16001 To 17500
                            H$(Y%) = "V2"
                        Case 17501 To 19000
                            H$(Y%) = "V3"
                        Case 19001 To 20500
                            H$(Y%) = "V4"
                    End Select

            End Select

            O%(Y%) = S!(Q, 0)
            HO$(Y%) = B$(1 - Q)
            HT%(Y%) = S!(1 - Q, 0)

            Open diskPaths$(1) + R$ For Output As #1
            Print #1, Y%
            For I = 1 To 100: Print #1, O%(I): Next I
            For I = 1 To 100: Print #1, HO$(I): Next I
            For I = 1 To 100: Print #1, HT%(I): Next I
            For I = 1 To 100: Print #1, H$(I): Next I
            Close #1

            For I = 1 To 100: O%(I) = 0: HT%(I) = 0: HO$(I) = "": H$(I) = "": Next I

            For J = 0 To 14
                For K = 0 To 13
                    Z2!(J, K) = 0
                Next K
            Next J

            For L = 0 To 15
                Z!(L) = 0
                Z1!(L) = 0
            Next L

        End If

        6290 '
        Q = Q + 1
        Z!(12) = 0

    Loop Until Q = 2

    If AP <> 1 Or AD = 1 Then
        If AD = 1 Then Run "HELLO.EXE"
    End If

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
    Locate 8, 14: Color T(0, 33), T(0, 34): Print B$(0);
    Locate , 52: Color T(1, 33), T(1, 34): Print B$(1)
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

    I$ = GetKeyPress$

    Color 7, 0

    Call NEWSCREEN

End Sub

'------------------------------
'    SETUPRATINGS Subroutine
'------------------------------
' ...explanation...
Sub SETUPRATINGS ()

    Dim FTF!, X3!, JX!

    ' *** SET UP TEAM RATINGS***

    For I = 0 To 1

        YR% = Val(Left$(B$(I), 2))

        If YR% < 40 Then YR% = YR% + 100

        FTF! = 1

        If YR% < 74 Then FTF! = .91
        If YR% < 69 Then FTF! = .83

        For I1 = 0 To 14
            B%(I, I1, 4) = B%(I, I1, 4) * FTF!
            B%(I, I1, 5) = B%(I, I1, 5) * FTF!
        Next I1

        T(I, 7) = T(I, 7) * FTF!
        T(I, 8) = T(I, 8) * FTF!
        T(I, 21) = T(I, 21) * FTF!
        T(I, 22) = T(I, 22) * FTF!
        YR% = YR% - 60

        If YR% < 0 Then
            TOF%(I) = 59
            PF%(I) = 11
        Else
            If YR% > 32 Then
                TOF%(I) = 34
                PF%(I) = 8
            Else
                TOF%(I) = TOA%(YR%)
                PF%(I) = PFA%(YR%)
            End If
        End If

    Next

    'Handle game play for different eras
    If Left$(B$(1), 2) <> Left$(B$(0), 2) Then

        If AP = 1 Then
            I = NG%(6)
        Else
            Color 15, 0
            Cls
            Print "THESE TWO TEAMS MAY HAVE PLAYED DURING DIFFERENT ERAS AND"
            Print "PLAYED DIFFERENT STYLES OF BASKETBALL"
            Print
            Print "PLAY STYLE OF:"
            Print
            Color 14: Print "(0) - ";: Color T(0, 33), T(0, 34): Print B$(0)
            Color , 0
            Print
            Color 14: Print "(1) - ";: Color T(1, 33), T(1, 34): Print B$(1)
            Color , 0
            Print
            Color 14: Print "(2) - ";: Color 3: Print "MAKE NO ADJUSTMENT"
            Color 15, 0
            Do
                I$ = GetKeyPress$
                I = Val(I$)
            Loop Until I >= 0 And I <= 2
        End If

        If I <> 2 Then
            HN% = 1
            HM% = I
            JX! = (T(0, 5) + T(0, 19) + T(1, 5) + T(1, 19)) * (L%(I, 1) / L%(1 - I, 1)) / ((T(1, 0) + T(0, 0)) * 2) * 2 - (L%(I, 1) + L%(I, 1)) / 2
            JY = JX!
        End If

    Else
        'I = 3
        'JX! = (  T(0, 5) + T(0, 19) + T(1, 5) + T(1, 19)  ) / ( (T(1, 0) + T(0, 0) ) * 2) * 2 - (L%(0, 1) + L%(1, 1)) / 2
        JX! = (T(0, 5) + T(0, 19)) / ((T(1, 0) + T(0, 0)) * 2) + (T(1, 5) + T(1, 19)) / ((T(1, 0) + T(0, 0)) * 2)
        JX! = JX! * 2 - (L%(0, 1) + L%(1, 1)) / 2
        JY = JX!
        If PO = 1 Then JY = JY - 7
    End If 'Done checking for different eras

    9254 '
    For I = 0 To 1
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
        X3! = (T(I, 21) - T(I, 20) * .1) - 7.9 * T(I, 0)
        X3! = X3! / 2
        F7%(I, 3) = Int(X3! / (T(I, 19) + X3!) * 100 + .5) - PF%(I): ' WAS -7
        F7%(I, 7) = Int(T(I, 11) / T(I, 6) * 100 + .5)
    Next

    For I = 0 To 1

        For I1 = 0 To 14

            If B%(I, I1, 18) = 0 Then

                'Start assignment of player ratings
                If B%(I, I1, 2) - B%(I, I1, 13) > 0 Then A!(I, I1, 0) = Int((B%(I, I1, 3) - B%(I, I1, 14)) / (B%(I, I1, 2) - B%(I, I1, 13)) * 100 + .5)
                If PO = 1 Then A!(I, I1, 0) = A!(I, I1, 0) - 2
                If B%(I, I1, 4) > 0 Then A!(I, I1, 1) = Int(B%(I, I1, 5) / B%(I, I1, 4) * 100 + .5)

                X3! = B%(I, I1, 7) / B%(I, I1, 1) * 48
                X3! = X3! * 29.3 / (L%(I, 2) * .677) * ((T(I, 5) + T(I, 19)) / (T(I, 0) * 2) / L%(I, 1))
                X3! = Int(X3! * 10 + .5)

                A!(I, I1, 2) = X3!

                X3! = B%(I, I1, 6) / B%(I, I1, 1) * 48
                X3! = X3! * 14! / (L%(I, 2) * .323) * ((T(I, 5) + T(I, 19)) / (T(I, 0) * 2) / L%(I, 1))
                X3! = Int(X3! * 10 + .5)

                A!(I, I1, 21) = X3!

                X3! = B%(I, I1, 8) / B%(I, I1, 1) * 48

                A!(I, I1, 25) = Int(X3! * 10 + .5)

                X3! = X3! * 26 * T(I, 0) / T(I, 11)

                A!(I, I1, 4) = Int(X3! * 10 + .5)

                ASSIST:
                A!(I, I1, 5) = B%(I, I1, 11) / B%(I, I1, 1) * 48
                A!(I, I1, 5) = Int(A!(I, I1, 5) / X7(I) * 1000 + .5)

                BLOCK:
                A!(I, I1, 6) = B%(I, I1, 12) / B%(I, I1, 1) * 48
                A!(I, I1, 6) = A!(I, I1, 6) / ((T(I, 19) - T(I, 31) - T(I, 20) + T(I, 32)) / T(I, 0) / 5)
                A!(I, I1, 6) = Int(A!(I, I1, 6) * .93 * 100 + .5)

                If FR% = 0 Then X3! = B%(I, I1, 4) - (B%(I, I1, 3) * .1) - 1.58 * B%(I, I1, 1) / 48
                If FR% = 1 Then X3! = B%(I, I1, 4) * .9 - (B%(I, I1, 3) * .1) - 1.58 * B%(I, I1, 1) / 48

                X3! = X3! / 2

                A!(I, I1, 16) = Int(X3! / (B%(I, I1, 2) + X3!) * 100 + .5)
                If A!(I, I1, 16) < 0 Then A!(I, I1, 16) = 0

                A!(I, I1, 17) = Int(B%(I, I1, 9) / B%(I, I1, 1) * 48 * 10 + .5)
                A!(I, I1, 18) = B%(I, I1, 18)

                If B%(I, I1, 2) > 0 Then A!(I, I1, 19) = Int(B%(I, I1, 13) / B%(I, I1, 2) * 100 + .5)
                If B%(I, I1, 13) > 0 Then A!(I, I1, 20) = Int(B%(I, I1, 14) / B%(I, I1, 13) * 100 + .5)

                X3! = Int(B%(I, I1, 2) / (B%(I, I1, 2) + B%(I, I1, 4)) * 100 + .5)

                If Left$(AA$(I, I1, 1), 1) <> "F" And Left$(AA$(I, I1, 1), 1) <> "C" Then
                    If X3! >= 86 Then B%(I, I1, 16) = 1
                    If X3! = 84 Or X3! = 85 Then B%(I, I1, 16) = 2
                    If X3! = 82 Or X3! = 83 Then B%(I, I1, 16) = 3
                    If X3! = 80 Or X3! = 81 Then B%(I, I1, 16) = 4
                    If X3! >= 77 And X3! <= 79 Then B%(I, I1, 16) = 5
                    If X3! = 75 Or X3! = 76 Then B%(I, I1, 16) = 6
                    If X3! = 73 Or X3! = 74 Then B%(I, I1, 16) = 7
                    If X3! = 71 Or X3! = 72 Then B%(I, I1, 16) = 8
                    If X3! <= 70 Then B%(I, I1, 16) = 9
                Else
                    If X3! >= 81 Then B%(I, I1, 16) = 1
                    If X3! = 79 Or X3! = 80 Then B%(I, I1, 16) = 2
                    If X3! = 77 Or X3! = 78 Then B%(I, I1, 16) = 3
                    If X3! = 75 Or X3! = 76 Then B%(I, I1, 16) = 4
                    If X3! >= 72 And X3! <= 74 Then B%(I, I1, 16) = 5
                    If X3! = 70 Or X3! = 71 Then B%(I, I1, 16) = 6
                    If X3! = 68 Or X3! = 69 Then B%(I, I1, 16) = 7
                    If X3! = 66 Or X3! = 67 Then B%(I, I1, 16) = 8
                    If X3! <= 65 Then B%(I, I1, 16) = 9
                End If

            End If

            9260 '
        Next I1
    Next I

    For I = 0 To 1
        M9!(I) = (T(I, 5) + T(I, 19) + T(I, 9) + T(I, 10)) / (T(I, 0) * 2)
        M9!(I) = M9!(I) + (T(I, 23) + T(I, 24) + T(I, 12) + T(I, 26)) / (T(I, 0) * 2)
        M9!(I) = M9!(I) * 2 - (L%(I, 1) + L%(I, 2) + L%(I, 4))
    Next I

    If HN% = 1 Then

        I = HM%
        F7%(1 - I, 1) = F7%(1 - I, 1) * TOF%(I) / TOF%(1 - I)
        M9!(1 - I) = M9!(1 - I) * (L%(I, 1) + L%(I, 2) + L%(I, 4)) / (L%(1 - I, 1) + L%(1 - I, 2) + L%(1 - I, 4))

        For I1 = 0 To 14
            A!(1 - I, I1, 0) = A!(1 - I, I1, 0) + L%(I, 3) - L%(1 - I, 3) - L%(1 - I, 6) + L%(I, 6)
        Next

    End If

    S9 = Int((M9!(0) + M9!(1)) / 2 + .5)

    If PO = 1 Then S9 = S9 - 11

    For I = 0 To 1:
        M9!(I) = 0
        For I1 = 0 To 14
            If B%(I, I1, 18) = 0 Then
                M9!(I) = M9!(I) + Int((B%(I, I1, 2) + B%(I, I1, 6) + B%(I, I1, 7) + B%(I, I1, 9)) / B%(I, I1, 0) + .5)
            End If
        Next I1
    Next I

    For I = 0 To 1
        For I1 = 0 To 14
            If B%(I, I1, 18) = 0 Then
                A!(I, I1, 3) = Int(((B%(I, I1, 2) + B%(I, I1, 6) + B%(I, I1, 7) + B%(I, I1, 9)) / B%(I, I1, 0)) / M9!(I) * S9 + .5)
                If A!(I, I1, 3) = 0 Then A!(I, I1, 3) = 1
            End If
        Next I1
    Next I

    Do

        9300 '
        If Not (AP = 1 And U5 >= 2) Then

            Color 15, 0
            Cls
            Print "EDIT CONTRIBUTION PERCENTAGES OF PLAYERS FROM:"
            Print
            Color 14: Print "(0) - ";: Color T(0, 33), T(0, 34): Print B$(0): Color , 0
            Print
            Color 14: Print "(1) - ";: Color T(1, 33), T(1, 34): Print B$(1): Color , 0
            Print
            Color 14: Print "(2) - ";: Color 3: Print "PROCEED TO GAME": Color 15, 0
            Print
            Print "NOTE:: THE CONTRIBUTION RATING CONSISTS OF THE PLAYER'S"
            Print "       FREQUENCY TO SHOOT, REBOUND, AND COMMIT FOULS."

            Do
                I$ = GetKeyPress$
                I = Val(I$)
            Loop Until I >= 0 And I <= 2

            If I <> 2 Then
                Cls
                TX = 0
                For I1 = 0 To 14: TX = TX + A!(I, I1, 3): Next
                For I1 = 0 To 14: A!(I, I1, 3) = A!(I, I1, 3) / TX * 100: Next

                Do
                    Locate 2, 1: Print "CONTRIBUTION RATINGS FOR ";
                    Color T(I, 33), T(I, 34): Print B$(I): Color 15, 0
                    Print
                    Print " #  PLAYER-POS         CX"
                    TY% = 0
                    For I1 = 0 To 14: TY% = TY% + A!(I, I1, 3)
                        Print Using "## "; I1 + 1;: Print AA$(I, I1, 0); "-"; AA$(I, I1, 1);: Locate , 24: Print Using "##"; A!(I, I1, 3)
                    Next

                    Locate , 23: Print Using "###"; TY%
                    Input "SELECT PLAYER TO CHANGE (0 TO CONTINUE)"; I1
                    I1 = I1 - 1

                    If I1 <= 14 And I1 >= 0 Then
                        Locate 22, 1: Print "ENTER NEW CX RATING FOR "; AA$(I, I1, 0); " ";: Input A!(I, I1, 3)
                        Locate 22, 1: Print Space$(52)
                    Else
                        If TY% <> 100 Then
                            Locate 22, 1: Print "TOTAL MUST ADD UP TO 100%! PRESS ANY KEY"
                            I$ = GetKeyPress$
                            Locate 22, 1: Print Space$(52)
                        End If
                    End If

                Loop Until I1 < 0

                For I1 = 0 To 14: A!(I, I1, 3) = A!(I, I1, 3) / 100 * TX: Next

            End If

        End If

    Loop Until I = 2

    X3! = 0

End Sub

'------------------------------
'    TICKERUPDATE Subroutine
'------------------------------
' ...explanation...
Sub TICKERUPDATE ()

    If AP = 1 And AP%(2) <= 31 Then

        If H = 1 And T <= 480 And SX%(1 + AP%(2), 0, 1) = -1 Then SX%(1 + AP%(2), 0, 1) = S!(0, 0): SX%(1 + AP%(2), 1, 1) = S!(1, 0): GoTo 2580
        If H = 1 And T <= 240 And SX%(1 + AP%(2), 0, 2) = -1 Then SX%(1 + AP%(2), 0, 2) = S!(0, 0): SX%(1 + AP%(2), 1, 2) = S!(1, 0): GoTo 2580
        If H > 1 And SX%(1 + AP%(2), 0, 3) = -1 Then SX%(1 + AP%(2), 0, 3) = S!(0, 1): SX%(1 + AP%(2), 1, 3) = S!(1, 1): GoTo 2580

        If H = 2 And T <= 480 And SX%(1 + AP%(2), 0, 4) = -1 Then SX%(1 + AP%(2), 0, 4) = S!(0, 0): SX%(1 + AP%(2), 1, 4) = S!(1, 0): GoTo 2580
        If H = 2 And T <= 240 And SX%(1 + AP%(2), 0, 5) = -1 Then SX%(1 + AP%(2), 0, 5) = S!(0, 0): SX%(1 + AP%(2), 1, 5) = S!(1, 0): GoTo 2580
        If H > 2 And SX%(1 + AP%(2), 0, 6) = -1 Then For I2 = 6 To 8: SX%(1 + AP%(2), 0, I2) = S!(0, 2) + S!(0, 1): SX%(1 + AP%(2), 1, I2) = S!(1, 2) + S!(1, 1): Next: GoTo 2580

        If H = 3 And T <= 480 And SX%(1 + AP%(2), 0, 9) = -1 Then SX%(1 + AP%(2), 0, 9) = S!(0, 0): SX%(1 + AP%(2), 1, 9) = S!(1, 0): GoTo 2580
        If H = 3 And T <= 240 And SX%(1 + AP%(2), 0, 10) = -1 Then SX%(1 + AP%(2), 0, 10) = S!(0, 0): SX%(1 + AP%(2), 1, 10) = S!(1, 0): GoTo 2580
        If H > 3 And SX%(1 + AP%(2), 0, 11) = -1 Then SX%(1 + AP%(2), 0, 11) = S!(0, 3) + S!(0, 2) + S!(0, 1): SX%(1 + AP%(2), 1, 11) = S!(1, 3) + S!(1, 2) + S!(1, 1): GoTo 2580

        If H = 4 And T <= 480 And SX%(1 + AP%(2), 0, 12) = -1 Then SX%(1 + AP%(2), 0, 12) = S!(0, 0): SX%(1 + AP%(2), 1, 12) = S!(1, 0): GoTo 2580
        If H = 4 And T <= 240 And SX%(1 + AP%(2), 0, 13) = -1 Then SX%(1 + AP%(2), 0, 13) = S!(0, 0): SX%(1 + AP%(2), 1, 13) = S!(1, 0): GoTo 2580

        SX%(1 + AP%(2), 0, 14) = S!(0, 0): SX%(1 + AP%(2), 1, 14) = S!(1, 0)

    End If

    2580 '
End Sub

'------------------------------
'      ADJFOUL Subroutine
'------------------------------
' ...explanation...
Sub ADJFOUL ()

    For I = 0 To 14
        If A!(P9, I, 12) >= 6 Then A!(P9, I, 12) = 5: Exit For
    Next

End Sub

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

    If Not (UCase$(I$) = "Z" And U5 = 1 And CT = 0 Or U5 = 2) Then

        If Not (I$ = "/" And U5 = 1 And CT = 1 Or U5 = 2) Then

            TS% = 1

            If UCase$(I$) = "Z" Then TS% = 0

            Do
                8101 JJ = 8
                Call FRAMEBOX
                Print B$(TS%); " SELECT STRATEGY:"

                8102 Locate 12, 14
                Locate , 14: Print "0 - RETURN TO GAME"
                Locate , 14: Print "1 - SELECT OFFENSIVE STYLE OF PLAY"
                Locate , 14: Print "2 - SELECT DEFENSIVE STYLE OF PLAY"
                Locate , 14: Print "3 - FULL COURT PRESS "; P7$(P7(TS%))

                Do
                    I$ = GetKeyPress$
                    I1 = Val(I$)
                Loop Until I1 >= 0 And I1 <= 3

                Select Case I1
                    Case 1:
                        8170 'DEFENSE
                        JJ = 8
                        Call FRAMEBOX: Print B$(TS%); " SELECT OFFENSE:"
                        Locate 12, 14
                        Locate , 14: Print "0 - NORMAL"
                        Locate , 14: Print "1 - AGGRESSIVE"
                        Locate , 14: Print "2 - SAFE"

                        Do
                            I$ = GetKeyPress$
                            I1 = Val(I$)
                        Loop Until I1 >= 0 And I1 <= 2

                        O0%(TS%) = I1

                    Case 2:
                        8160 'DEFENSE
                        JJ = 10
                        Call FRAMEBOX
                        Print B$(TS%); " SELECT DEFENSE:"
                        Locate 12, 14
                        Locate , 14: Print "1 - REGULAR MAN-TO-MAN"
                        Locate , 14: Print "2 - SAGGING MAN-TO-MAN"
                        Locate , 14: Print "3 - TRAPPING MAN-TO-MAN"
                        Locate , 14: Print "4 - TIGHT MAN-TO-MAN"
                        Locate , 14: Print "5 - LOOSE MAN-TO-MAN"

                        Do
                            I$ = GetKeyPress$
                            I1 = Val(I$)
                        Loop Until I1 >= 1 And I1 <= 5

                        D2(TS%) = I1

                    Case 3
                        P7(TS%) = 1 - P7(TS%)

                End Select

            Loop Until I1 = 0

        End If

    End If

    '8155
    Color 7, 0
    Call NEWSCREEN

8166 End Sub

'------------------------------
'   COMPUTERCOACH Subroutine
'------------------------------
' ...explanation...
Sub COMPUTERCOACH ()

    If P9 = 0 Then I6 = 1
    If P9 = 1 Then I6 = 0

    If S!(P9, 0) - S!(I6, 0) < 0 Then

        I5 = S!(I6, 0) - S!(P9, 0)

        If H <= 2 Then D2(P9) = 1: P7(P9) = 0
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

    Else

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

    End If

    O0%(P9) = 0

    If H < 3 Then O0%(P9) = 0
    If H >= 3 And S!(P9, 0) - S!(I6, 0) < 0 And Int(Rnd(1) * 2) = 1 Then O0%(P9) = 1
    If H >= 3 And S!(P9, 0) - S!(I6, 0) >= 0 And Int(Rnd(1) * 2) = 1 Then O0%(P9) = 2

End Sub

'------------------------------
'   COMPUTERSUBS Subroutine
'------------------------------
' ...explanation...
Sub COMPUTERSUBS ()

    For I2 = 0 To 4
        B1!(P9, I2) = -1
    Next

    For I2 = 0 To 1
        P$ = "G"

        8781 '
        For I1 = 14 To 0 Step -1:
            M6 = 0: M7 = 0

            If Left$(AA$(P9, I1, 1), 1) = P$ Or Right$(AA$(P9, I1, 1), 1) = P$ Then

                If Not (AA$(P9, I1, 0) = "XXX" Or A!(P9, I1, 12) >= 6 Or B%(P9, I1, 18) <> 0 Or H = 1 And A!(P9, I1, 12) >= 2 + FS% Or H = 2 And A!(P9, I1, 12) >= 4 + FS% Or H = 3 And A!(P9, I1, 12) >= 5 Or H = 4 And T > 300 And A!(P9, I1, 12) = 5) Then

                    M6 = A!(P9, I1, 3) - A!(P9, I1, 7) - A!(P9, I1, 11) - A!(P9, I1, 12)

                    If B1!(P9, I2) > -1 Then M7 = A!(P9, B1!(P9, I2), 3) - A!(P9, B1!(P9, I2), 7) - A!(P9, B1!(P9, I2), 11) - A!(P9, B1!(P9, I2), 12)

                    If H = 4 And T < 120 Or H > 4 Then M6 = A!(P9, I1, 3)

                    If (H = 4 And T < 120 Or H > 4) And B1!(P9, I2) > -1 Then M7 = A!(P9, B1!(P9, I2), 3)

                    If I2 <> 1 Or B1!(P9, 0) <> I1 Then
                        If B1!(P9, I2) = -1 Or M6 >= M7 Then B1!(P9, I2) = I1
                    End If

                End If

            End If

        Next I1

        If B1!(P9, I2) = -1 And FS% <> 1 Then
            If P$ = "G" Then P$ = "F": GoTo 8781

            If P$ = "F" Then
                P$ = "C": GoTo 8781
            Else
                FS% = 1
                P$ = "G"
                GoTo 8781
            End If
        End If

        FS% = 0

    Next I2

    For I2 = 2 To 3
        P$ = "F"

        8783 For I1 = 14 To 0 Step -1:
            M6 = 0: M7 = 0

            If Left$(AA$(P9, I1, 1), 1) = P$ Or Right$(AA$(P9, I1, 1), 1) = P$ Then

                If Not (AA$(P9, I1, 0) = "XXX" Or A!(P9, I1, 12) >= 6 Or B%(P9, I1, 18) <> 0 Or H = 1 And A!(P9, I1, 12) >= 2 + FS% Or H = 2 And A!(P9, I1, 12) >= 4 + FS% Or H = 3 And A!(P9, I1, 12) >= 5 Or H = 4 And T > 300 And A!(P9, I1, 12) = 5) Then

                    M6 = A!(P9, I1, 3) - A!(P9, I1, 7) - A!(P9, I1, 11) - A!(P9, I1, 12)

                    If B1!(P9, I2) > -1 Then M7 = A!(P9, B1!(P9, I2), 3) - A!(P9, B1!(P9, I2), 7) - A!(P9, B1!(P9, I2), 11) - A!(P9, B1!(P9, I2), 12)
                    If H = 4 And T < 120 Or H > 4 Then M6 = A!(P9, I1, 3)
                    If (H = 4 And T < 120 Or H > 4) And B1!(P9, I2) > -1 Then M7 = A!(P9, B1!(P9, I2), 3)

                    If Not (I2 = 2 And (B1!(P9, 0) = I1 Or B1!(P9, 1) = I1)) Then
                        If Not (I2 = 3 And (B1!(P9, 0) = I1 Or B1!(P9, 1) = I1 Or B1!(P9, 2) = I1)) Then
                            If B1!(P9, I2) = -1 Or M6 >= M7 Then B1!(P9, I2) = I1
                        End If
                    End If

                End If

            End If

        8784 Next I1

        If B1!(P9, I2) = -1 And FS% <> 1 Then
            If P$ = "F" Then P$ = "C": GoTo 8783

            If P$ = "C" Then
                P$ = "G": GoTo 8783
            Else
                FS% = 1
                P$ = "F"
                GoTo 8783
            End If
        End If

        FS% = 0

    Next I2

    P$ = "C"

    8785 For I1 = 14 To 0 Step -1
        M6 = 0: M7 = 0
        If Left$(AA$(P9, I1, 1), 1) = P$ Or Right$(AA$(P9, I1, 1), 1) = P$ Then

            If Not (AA$(P9, I1, 0) = "XXX" Or A!(P9, I1, 12) >= 6 Or B%(P9, I1, 18) <> 0 Or H = 1 And A!(P9, I1, 12) >= 2 + FS% Or H = 2 And A!(P9, I1, 12) >= 4 + FS% Or H = 3 And A!(P9, I1, 12) >= 5 Or H = 4 And T > 300 And A!(P9, I1, 12) = 5) Then
                M6 = A!(P9, I1, 3) - A!(P9, I1, 7) - A!(P9, I1, 11) - A!(P9, I1, 12)
                If B1!(P9, 4) > -1 Then M7 = A!(P9, B1!(P9, 4), 3) - A!(P9, B1!(P9, 4), 7) - A!(P9, B1!(P9, 4), 11) - A!(P9, B1!(P9, 4), 12)
                If H = 4 And T < 120 Or H > 4 Then M6 = A!(P9, I1, 3)
                If (H = 4 And T < 120 Or H > 4) And B1!(P9, I2) > -1 Then M7 = A!(P9, B1!(P9, I2), 3)

                If Not (B1!(P9, 0) = I1 Or B1!(P9, 1) = I1 Or B1!(P9, 2) = I1 Or B1!(P9, 3) = I1) Then
                    If B1!(P9, 4) = -1 Or M6 >= M7 Then B1!(P9, 4) = I1
                End If

            End If

        End If

    8786 Next I1

    If B1!(P9, I2) = -1 And FS% <> 1 Then
        If P$ = "C" Then P$ = "F": GoTo 8785

        If P$ = "F" Then
            P$ = "G": GoTo 8785
        Else
            FS% = 1
            P$ = "F"
            GoTo 8785
        End If
    End If

    FS% = 0

    For I2 = 0 To 4
        If B1!(P9, I2) = -1 Then
            8790 I = 15

            For I2 = 0 To 4
                8795 I = I - 1
                If I < 0 Then Call ADJFOUL: GoTo 8790
                If B%(P9, I, 18) <> 0 Or AA$(P9, I, 0) = "XXX" Or A!(P9, I, 12) >= 6 Then 8795
                B1!(P9, I2) = I
            Next I2

            Exit For
        End If
    Next

    Call COMPUTERCOACH

End Sub

'------------------------------
'      DELAY Subroutine
'------------------------------
' ...explanation...
Sub DELAY ()

    'Positive delay or Verbose
    If pbpDelay! > 0 And PB% = 0 Then

        U$ = PadRight$(U$, 38)

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

        _Delay pbpDelay!

    End If

End Sub

'------------------------------
'      EVENTS Subroutine
'------------------------------
' ...explanation...
Sub EVENTS (Count, SPECIAL)

    If _FileExists("EVENTS.BOX") Then

        Open "EVENTS.BOX" For Input As #1
        For X = 1 To 13
            Input #1, E%(X)
        Next
        Close 1

        If H - 1 > E%(1) Then
            checkSpecial% = 1
        Else
            If S!(1, 0) + S!(0, 0) > E%(2) Then
                checkSpecial% = 1
            Else
                If R3(0) + R3(1) > E%(3) Then
                    checkSpecial% = 1
                Else
                    If S!(1, 0) > E%(4) Or S!(0, 0) > E%(4) Then
                        checkSpecial% = 1
                    Else
                        If R3(1) > E%(5) Or R3(0) > E%(5) Then
                            checkSpecial% = 1
                        End If
                    End If
                End If
            End If
        End If

        If checkSpecial% <> 1 Then
            For I = 0 To 1

                For I1 = 0 To 14

                    If A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) > E%(6) Then
                        checkSpecial% = 1
                    Else
                        If A!(I, I1, 11) > E%(7) Then
                            checkSpecial% = 1
                        Else
                            If A!(I, I1, 14) > E%(8) Then
                                checkSpecial% = 1
                            Else
                                If A!(I, I1, 15) > E%(9) Then
                                    checkSpecial% = 1
                                Else
                                    If A!(I, I1, 13) > E%(10) Then
                                        checkSpecial% = 1
                                    Else
                                        If W%(I, I1, 0) > E%(11) Then
                                            checkSpecial% = 1
                                        End If
                                    End If
                                End If
                            End If
                        End If
                    End If

                Next

            Next 'done checking if special event already found

        End If

    End If 'Done checking EVENTS.BOX file

    If checkSpecial% = 1 Then
        Spec$ = diskPaths$(1) + "*.SPC"
        Count = FCount%(Spec$)
        Count = Count + 1
        SPECIAL = 1
    End If

End Sub

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

    A!(P, B1!(P, B), 10) = A!(P, B1!(P, B), 10) + 1

    If pbpDelay! <> 0 Then
        Locate F3 + 9, 1
        Call MOREDELAY
        Print F3; " GOOD"
    End If

    S!(P, H) = S!(P, H) + 1
    S!(P, 0) = S!(P, 0) + 1

    Call UPDATESCREEN
    Call MOREDELAY

End Sub

'------------------------------
'      MAKEPBP Subroutine
'------------------------------
' ...explanation...
Sub MAKEPBP ()

    'PLAY BY PLAY
    If pbpDelay! <> 0 Then

        RN = Int(Rnd(1) * 100) + 1

        Select Case PT%
            Case 1
                1410 'TURNOVER
                U$ = AA$(P, B1!(P, B), 0)
                RN = Int(Rnd(1) * 14) + 1

                Select Case RN
                    Case 1: U$ = U$ + " TURNS THE BALL OVER"
                    Case 2: U$ = U$ + " THROWS IT AWAY"
                    Case 3: U$ = U$ + " IS WHISTLED FOR TRAVELING"
                    Case 4: U$ = U$ + " TURNS IT OVER"
                    Case 5: U$ = U$ + " LOSES CONTROL OF THE BALL"
                    Case 6: U$ = U$ + " LOSES IT OUT OF BOUNDS"
                    Case 7: U$ = U$ + " LOSES THE HANDLE"
                    Case 8: U$ = U$ + " LOSES IT"
                    Case 9: U$ = U$ + " IS CALLED FOR STEPS"
                    Case 10: U$ = U$ + " TRAVELS"
                    Case 11: U$ = U$ + " WALKS WITH IT"
                    Case 12: U$ = U$ + " LETS IT GET AWAY"
                    Case 13: U$ = U$ + " DOUBLE DRIBBLES"
                    Case 14: U$ = U$ + " LOSES CONTROL"
                End Select

            Case 2
                1420 'DUNK
                RN = Int(Rnd(1) * 39) + 1

                Select Case RN
                    Case 1: U$ = "DIPSY-DOO DUNK-A-ROO!!!"
                    Case 2: U$ = AA$(P, B1!(P, B), 0) + " MAKES IT HAMMER TIME!!!"
                    Case 3: U$ = AA$(P, B1!(P, B), 0) + " SLAMS IT IN!!!"
                    Case 4: U$ = AA$(P, B1!(P, B), 0) + " WITH THE 2-HANDED SLAM!!!"
                    Case 5: U$ = AA$(P, B1!(P, B), 0) + " SLAMS IT HOME!!!"
                    Case 6: U$ = AA$(P, B1!(P, B), 0) + " JAMS IT HOME!!!"
                    Case 7: U$ = AA$(P, B1!(P, B), 0) + " THROWS IT DOWN!!!"
                    Case 8: U$ = AA$(P, B1!(P, B), 0) + " THUNDERSLAM!!!"
                    Case 9: U$ = "RIM ROCKING JAM!!!"
                    Case 10: U$ = AA$(P, B1!(P, B), 0) + " WITH THE MONSTER SLAM!!"
                    Case 11: U$ = AA$(P, B1!(P, B), 0) + "POWER SLAMS IT!!!"
                    Case 12: U$ = AA$(P, B1!(P, B), 0) + " WITH THE 2-HANDED STUFF!!!"
                    Case 13: U$ = AA$(P, B1!(P, B), 0) + " BACKWARDS JAM!!!"
                    Case 14: U$ = AA$(P, B1!(P, B), 0) + " WITH THE 1-HANDED SLAM!!!"
                    Case 15: U$ = AA$(P, B1!(P, B), 0) + " WITH THE REVERSE SLAM!!!"
                    Case 16: U$ = AA$(P, B1!(P, B), 0) + " WITH THE REVERSE JAM!!!"
                    Case 17: U$ = AA$(P, B1!(P, B), 0) + " WITH THE WINDMILL JAM!!!"
                    Case 18: U$ = AA$(P, B1!(P, B), 0) + " WITH THE WINDMILL SLAM!!!"
                    Case 19: U$ = AA$(P, B1!(P, B), 0) + " WITH THE TOMAHAWK SLAM!!!"
                    Case 20: U$ = AA$(P, B1!(P, B), 0) + " WITH THE 360 SLAM!!!"
                    Case 21: U$ = AA$(P, B1!(P, B), 0) + " WITH THE SPINNING SLAM!!!"
                    Case 22: U$ = AA$(P, B1!(P, B), 0) + " WITH THE LEFT HANDED DUNK!!!"
                    Case 23: U$ = "DOUBLE CLUTCH JAM!!!"
                    Case 24: U$ = AA$(P, B1!(P, B), 0) + " GETS THE FLUSH!!!"
                    Case 25: U$ = AA$(P, B1!(P, B), 0) + " PUNCHES IT HOME!!!"
                    Case 26: U$ = AA$(P, B1!(P, B), 0) + "...DOUBLE PUMP SLAM!!!"
                    Case 27: U$ = AA$(P, B1!(P, B), 0) + "...IN YOUR FACE SLAM!!!"
                    Case 28: U$ = AA$(P, B1!(P, B), 0) + " IS UP": Call DELAY: U$ = "HE SPINS": Call DELAY: U$ = "BEHIND THE BACK SLAM!!!"
                    Case 29: U$ = "HE SLAMS IT HOME!!"
                    Case 30: U$ = "HE LAYS IT UP AND IN!!"
                    Case 31: U$ = "HE BANGS IT IN!!"
                    Case 32: U$ = "HE RAMS IT IN!!"
                    Case 33: U$ = "HE SLAMS IT IN!!!"
                    Case 34: U$ = "DIPSY-DOO DUNK-A-ROO!!!"
                    Case 35: U$ = "HE MAKES IT HAMMER TIME!!!"
                    Case 36: U$ = AA$(P, B1!(P, B), 0) + " WITH THE 2-HANDED SLAM!!!"
                    Case 37: U$ = AA$(P, B1!(P, B), 0) + " IS UP": Call DELAY: U$ = "HE SPINS": Call DELAY: U$ = "BEHIND THE BACK SLAM!!!"
                    Case 38: U$ = "HE JAMS IT HOME!!!"
                    Case 39: U$ = "HE HAMMERS IT HOME!"
                End Select

            Case 3
                1430 'SHOOTS...PZ%567 SML
                U$ = AA$(P, B1!(P, B), 0)

                Select Case PZ%

                    Case 1
                        1431 'BAD

                        Select Case RN
                            Case 1 To 40: U$ = U$ + " TAKES A TOUGH SHOT"
                            Case 41 To 45: U$ = U$ + " SHOOTS OFF BALANCE"
                            Case 46 To 50: U$ = U$ + " FORCES ONE UP"
                            Case 51 To 60: U$ = U$ + " SHOOTS UNDER PRESSURE"
                            Case 61 To 65: U$ = U$ + " WITH A BAD SHOT"
                            Case 66 To 70: U$ = U$ + " SHOOTS IN HEAVY TRAFFIC"
                            Case 71 To 75: U$ = U$ + " THROWS UP A PRAYER"
                            Case 76 To 80: U$ = U$ + " TAKES A BAD SHOT"
                            Case 81 To 85: U$ = U$ + " FORCES THE INSIDE SHOT"
                            Case 86 To 100: U$ = U$ + " TURNS AND SHOOTS"
                        End Select

                    Case 2
                        1432 'INSIDE W/ASSIST

                        Select Case RN
                            Case 1 To 10
                                U$ = "HE CUTS TO THE BASKET"
                                Call DELAY
                                U$ = "TAKES IT UP"
                                If (B = 0 Or B = 1) And Int(Rnd(1) * 100) + 1 <= 1 Then DK% = 1
                                If (B = 2 Or B = 3) And Int(Rnd(1) * 100) + 1 <= 5 Then DK% = 1
                                If B = 4 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1

                            Case 11 To 20
                                U$ = "HE TAKES IT TO THE HOLE"
                                If (B = 0 Or B = 1) And Int(Rnd(1) * 100) + 1 <= 1 Then DK% = 1
                                If (B = 2 Or B = 3) And Int(Rnd(1) * 100) + 1 <= 5 Then DK% = 1
                                If B = 4 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1

                            Case 21 To 30
                                U$ = "HE TAKES IT TO THE HOOP"
                                If (B = 0 Or B = 1) And Int(Rnd(1) * 100) + 1 <= 1 Then DK% = 1
                                If (B = 2 Or B = 3) And Int(Rnd(1) * 100) + 1 <= 5 Then DK% = 1
                                If B = 4 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1

                            Case 31 To 40
                                U$ = "HE LEANS IN AND SHOOTS"

                            Case 41 To 50
                                U$ = "HE PUTS IT UP"

                            Case 51 To 60
                                U$ = "HE TAKES IT TO THE BOARD"
                                If (B = 0 Or B = 1) And Int(Rnd(1) * 100) + 1 <= 1 Then DK% = 1
                                If (B = 2 Or B = 3) And Int(Rnd(1) * 100) + 1 <= 5 Then DK% = 1
                                If B = 4 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1

                            Case 61 To 70
                                U$ = "HE PUTS IT OFF THE GLASS"

                            Case 71 To 80
                                If B = 4 Then
                                    U$ = "HE SHOOTS THE HOOK"
                                Else
                                    U$ = "HE GOES IN FOR THE LAYUP"
                                End If

                            Case 81 To 90
                                U$ = "HE TAKES IT UP STRONG"
                                If (B = 0 Or B = 1) And Int(Rnd(1) * 100) + 1 <= 1 Then DK% = 1
                                If (B = 2 Or B = 3) And Int(Rnd(1) * 100) + 1 <= 5 Then DK% = 1
                                If B = 4 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1

                            Case 91 To 95
                                U$ = "HE POWERS IT UP"
                                If (B = 0 Or B = 1) And Int(Rnd(1) * 100) + 1 <= 1 Then DK% = 1
                                If (B = 2 Or B = 3) And Int(Rnd(1) * 100) + 1 <= 5 Then DK% = 1
                                If B = 4 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1

                            Case 96 To 100
                                U$ = "HE LAYS IT UP"
                        End Select

                    Case 3
                        1433 'PENETRATE
                        If Z3$ = "C" Then
                            Select Case RN
                                Case 1 To 15: U$ = "HE PUTS IT OFF THE GLASS"
                                Case 16 To 25: U$ = "HE PIVOTS TO THE BASKET"
                                Case 26 To 40: U$ = "HE POWERS IT UP"
                                Case 41 To 58: U$ = "HE TAKES IT TO THE HOOP": If Int(Rnd(1) * 100) + 1 <= 15 Then DK% = 1
                                Case 59 To 70: U$ = "HE TAKES THE SHOT"
                                Case 71 To 72: U$ = "HE CURLS OFF A PICK IN THE PAINT": Call DELAY: U$ = "TAKES IT UP": If Int(Rnd(1) * 100) + 1 <= 15 Then DK% = 1
                                Case 73 To 74: U$ = "HE MAKES A NICE SPIN MOVE": Call DELAY: U$ = "AND SOARS TO THE HOOP": If Int(Rnd(1) * 100) + 1 <= 15 Then DK% = 1
                                Case 75 To 100:
                                    U$ = "HE TAKES IT UP STRONG": If Int(Rnd(1) * 100) + 1 <= 15 Then DK% = 1
                            End Select
                        End If

                        If Z3$ = "F" Then
                            Select Case RN
                                Case 1 To 16: U$ = "HE TAKES IT UP": If Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
                                Case 17 To 32: U$ = "HE SPINS FREE": Call DELAY: U$ = "SHOOTS": If Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
                                Case 33 To 48: U$ = "HE GETS LOOSE IN THE PAINT": Call DELAY: U$ = "TAKES IT UP": If Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
                                Case 49 To 64: U$ = "HE PUTS IT UP"
                                Case 65 To 67: U$ = "HE CURLS OFF A PICK IN THE PAINT": Call DELAY: U$ = "TAKES IT UP": If Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
                                Case 68 To 69: U$ = "HE MAKES A NICE SPIN MOVE": Call DELAY: U$ = "AND SOARS TO THE HOOP": If Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
                                Case 70 To 71: U$ = "HE DRIVES THE BASELINE": Call DELAY: U$ = "SPINS RIGHT AND GOES UP": If Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
                                Case 72: U$ = "HE SLICES IN THE HOLE": Call DELAY: U$ = "UNDERHAND SCOOP SHOT"
                                Case 73: U$ = "STRONG CUT ALONG THE BASELINE": Call DELAY: U$ = "DOUBLE PUMP REVERSE LAYUP"
                                Case 74: U$ = "HE PENETRATES": Call DELAY: U$ = "GORGEOUS FINGER ROLL!"
                                Case 75 To 80: U$ = "HE SHOOTS"
                                Case 81 To 100:
                                    U$ = "HE TAKES THE SHOT"
                            End Select
                        Else

                            Select Case RN
                                Case 1 To 25: U$ = "HE SPINS FREE": Call DELAY: U$ = "SHOOTS"
                                Case 26 To 50: U$ = "HE TAKES IT UP UNDER THE BASKET": If Int(Rnd(1) * 100) + 1 <= 2 Then DK% = 1
                                Case 51 To 52: U$ = "HE MAKES A NICE SPIN MOVE": Call DELAY: U$ = "AND SOARS TO THE HOOP": If Int(Rnd(1) * 100) + 1 <= 2 Then DK% = 1
                                Case 53 To 54: U$ = "HE BLOWS PAST TWO DEFENDERS"
                                Case 55 To 56: U$ = "HE DRIVES THE BASELINE": Call DELAY: U$ = "SPINS RIGHT AND GOES UP": If Int(Rnd(1) * 100) + 1 <= 2 Then DK% = 1
                                Case 57: U$ = "HE SLICES IN THE HOLE": Call DELAY: U$ = "UNDERHAND SCOOP SHOT"
                                Case 58: U$ = "STRONG CUT ALONG THE BASELINE": Call DELAY: U$ = "DOUBLE PUMP REVERSE LAYUP"
                                Case 59: U$ = "HE PENETRATES": Call DELAY: U$ = "GORGEOUS FINGER ROLL!"
                                Case 60 To 75: U$ = "HE TAKES THE JUMPER"
                                Case 76 To 90: U$ = "HE STOPS": Call DELAY: U$ = "SHOOTS"
                                Case 91: U$ = "HE LOOKS TO PASS": Call DELAY: U$ = "NO ONE IS OPEN": Call DELAY: U$ = "HE PUTS IT UP"
                                Case 92 To 100:
                                    U$ = "HE TAKES THE SHOT"
                            End Select
                        End If

                    Case 4
                        1434 'THREE!
                        If FT% = 1 Then
                            If RN <= 50 Then
                                U$ = U$ + " THROWS UP A PRAYER"
                            Else
                                U$ = U$ + " LAUNCHES A THREE"
                            End If

                            FT% = 0
                        Else
                            Select Case RN
                                Case 1 To 10: U$ = U$ + " TRIES FOR THREE!"
                                Case 11 To 20: U$ = U$ + " SHOOTS FROM 3-POINT RANGE!"
                                Case 21 To 30: U$ = U$ + " HOISTS A THREE!"
                                Case 31 To 40: U$ = U$ + " BOMBS A THREE FROM THE CORNER!"
                                Case 41 To 50: U$ = U$ + " FIRES FROM THE 3-POINT LINE!"
                                Case 51 To 60: U$ = U$ + " SHOOTS FOR THREE!"
                                Case 61 To 70: U$ = U$ + " SHOOTS THE 3-POINTER!"
                                Case 71 To 80: U$ = U$ + " FOR 3...!"
                                Case 81 To 90: U$ = U$ + " PUTS UP THE TREY!"
                                Case 91 To 100: U$ = U$ + " TAKES THE 3-POINTER!"
                            End Select
                        End If

                    Case 5
                        1435 'SHORT JUMP
                        RN = Int(Rnd(1) * 59) + 1

                        Select Case RN
                            Case 1: U$ = U$ + " WITH A TWISTING JUMPER"
                            Case 2: U$ = U$ + " SHOOTS THE J"

                            Case 3:
                                If B = 4 Then
                                    U$ = U$ + " RELEASES THE HOOK"
                                Else
                                    U$ = U$ + " TAKES THE SHOT"
                                End If

                            Case 4: U$ = U$ + " WITH A SHORT JUMPER"
                            Case 5: U$ = U$ + " WITH THE 10 FOOTER"

                            Case 6:
                                If B = 4 Then
                                    U$ = U$ + " WITH THE RUNNING HOOK"
                                Else
                                    U$ = U$ + " WITH THE RUNNER"
                                End If

                            Case 7:
                                If B = 4 Then
                                    U$ = U$ + " WITH A SWEEPING HOOK"
                                Else
                                    U$ = U$ + " WORKS FREE AND SHOOTS"
                                End If

                            Case 8:
                                If B = 4 Then
                                    U$ = U$ + " WITH A BABY HOOK SHOT"
                                Else
                                    U$ = U$ + " TAKES THE SHOT"
                                End If

                            Case 9: U$ = U$ + " LEANS IN AND SHOOTS"
                            Case 10: U$ = U$ + " SHOOTS"
                            Case 11: U$ = U$ + " SHOOTS FALLING AWAY"
                            Case 12: U$ = U$ + " LEANS IN FOR THE SHOT"
                            Case 13: U$ = U$ + " TAKES THE LEFT HANDER"
                            Case 14: U$ = U$ + " HARD TO THE HOLE..."
                            Case 15: U$ = U$ + " FLIPS IT UP"
                            Case 16: U$ = U$ + " FORCES THE INSIDE SHOT"
                            Case 17: U$ = U$ + " CUTS TO THE BASKET"
                            Case 18: U$ = U$ + " TAKES IT IN THE PAINT"
                            Case 19: U$ = U$ + " FLOATS IT OVER HIS MAN"
                            Case 20: U$ = U$ + " TAKES HIS MAN INSIDE"
                            Case 21: U$ = U$ + " WHEELS IN THE LANE"
                            Case 22: U$ = U$ + " CUTS INSIDE FOR THE SHOT"
                            Case 23: U$ = U$ + " PENETRATES AND SHOOTS"
                            Case 24: U$ = U$ + " SLICES INSIDE"
                            Case 25: U$ = U$ + " LOOKS FOR TWO"
                            Case 26: U$ = U$ + " DRIVES INTO THE PAINT"
                            Case 27: U$ = U$ + " HAS THE BASELINE JUMPER"
                            Case 28: U$ = U$ + " GLIDES TO THE HOLE"
                            Case 29: U$ = U$ + " GETS INSIDE FOR THE SHOT"
                            Case 30: U$ = U$ + " SPINS TOWARD THE HOOP"
                            Case 31: U$ = U$ + " FROM SHORT RANGE"
                            Case 32: U$ = U$ + " GOES BASELINE"
                            Case 33: U$ = U$ + " PENETRATES AND SHOOTS"
                            Case 34: U$ = U$ + " FROM 8 FEET"
                            Case 35: U$ = U$ + " FROM 6 FEET"
                            Case 36: U$ = U$ + " FROM 5 FEET"
                            Case 37: U$ = U$ + " DRIVES TO THE HOLE"
                            Case 38: U$ = U$ + " GLIDES TO THE HOLE"
                            Case 39: U$ = U$ + " GETS INSIDE FOR THE SHOT"
                            Case 40: U$ = U$ + " GOES TO THE HOLE"
                            Case 41: U$ = U$ + " CUTS INSIDE"
                            Case 42: U$ = U$ + " SPINS INSIDE"
                            Case 43: U$ = U$ + " BREAKS INSIDE"
                            Case 44: U$ = U$ + " SNEAKS INSIDE"
                            Case 45: U$ = U$ + " SCOOTS INSIDE"
                            Case 46: U$ = U$ + " WORKS IT INSIDE"
                            Case 47: U$ = U$ + " KNIFES INSIDE"
                            Case 48: U$ = U$ + " SCISSORS THROUGH"
                            Case 49: U$ = U$ + " BANGS INSIDE"
                            Case 50: U$ = U$ + " SLICES INSIDE"
                            Case 51: U$ = U$ + " PUMPS AND SHOOTS"
                            Case 52: U$ = U$ + " DOUBLE PUMPS AND SHOOTS"
                            Case 53: U$ = U$ + "...ONE HANDED FADE AWAY"
                            Case 54: U$ = U$ + "...LEFT HANDED FADE AWAY"
                            Case 55: U$ = U$ + "...HIGH ARCHING SHOT"
                            Case 56: U$ = U$ + " WITH A RUNNING LEFT HANDER"
                            Case 57: U$ = U$ + " WITH A RUNNING SCOOP SHOT"
                            Case 58: U$ = U$ + " WITH THE SCOOP SHOT"
                            Case 59: U$ = U$ + " WITH THE FINGER ROLL"
                        End Select

                    Case 6
                        1436 '
                        RN = Int(Rnd(1) * 59) + 1

                        Select Case RN
                            Case 1: U$ = U$ + " POPS FROM OUTSIDE"
                            Case 2: U$ = U$ + " DECIDES TO SHOOT IT"
                            Case 3: U$ = U$ + " FIRES THE FADE-AWAY"
                            Case 4: U$ = U$ + " LETS IT FLY"
                            Case 5: U$ = U$ + " LAUNCHES ONE"
                            Case 6: U$ = U$ + " WITH THE OUTSIDE SHOT"
                            Case 7: U$ = U$ + " OUT OF THE CORNER"
                            Case 8: U$ = U$ + " PUTS IT UP FROM 18"
                            Case 9: U$ = U$ + " SPOTS UP OUTSIDE"
                            Case 10: U$ = U$ + " RELEASES THE JUMPER"
                            Case 11: U$ = U$ + " FROM THE TOP OF THE KEY"
                            Case 12: U$ = U$ + " FIRES FROM THE LEFT WING"
                            Case 13: U$ = U$ + " TRIES THE 12 FOOTER"
                            Case 14: U$ = U$ + " PULLS UP AND POPS"
                            Case 15: U$ = U$ + " FIRES FROM 15"
                            Case 16: U$ = U$ + " FROM THE PERIMETER"
                            Case 17: U$ = U$ + " TURNS AND SHOOTS"
                            Case 18: U$ = U$ + " SHOOTS OFF THE SCREEN"
                            Case 19: U$ = U$ + " FAKES AND SHOOTS"
                            Case 20: U$ = U$ + " FIRES AWAY"
                            Case 21: U$ = U$ + " POPS FROM THE BASELINE"
                            Case 22: U$ = U$ + " DRIVES THE LANE"
                            Case 23: U$ = U$ + " TAKES THE SHOT"
                            Case 24: U$ = U$ + " WORKS FREE AND FIRES"
                            Case 25: U$ = U$ + " SHOOTS OVER HIS MAN"
                            Case 26: U$ = U$ + " SHOOTS FALLING AWAY"
                            Case 27: U$ = U$ + " FAKES AND SHOOTS"
                            Case 28: U$ = U$ + " FIRES AWAY"
                            Case 29: U$ = U$ + " POPS FROM THE BASELINE"
                            Case 30: U$ = U$ + " DRIVES THE LANE"
                            Case 31: U$ = U$ + " TAKES THE SHOT"
                            Case 32: U$ = U$ + " WORKS FREE AND FIRES"
                            Case 33: U$ = U$ + " SHOOTS FROM 15"
                            Case 34: U$ = U$ + " SHOOTS"
                            Case 35: U$ = U$ + " TAKES THE JUMPER"
                            Case 36: U$ = U$ + " FROM THE RIGHT CORNER"
                            Case 37: U$ = U$ + " FROM THE LEFT CORNER"
                            Case 38: U$ = U$ + " FROM THE KEY"
                            Case 39: U$ = U$ + " FIRES FROM OUTSIDE"
                            Case 40: U$ = U$ + " PUTS IT UP FROM 16"
                            Case 41: U$ = U$ + " FROM THE PERIMETER"
                            Case 42: U$ = U$ + " FAKES AND SHOOTS"
                            Case 43: U$ = U$ + " FROM AROUND 16 FEET"
                            Case 44: U$ = U$ + " PUTS IT UP"
                            Case 45: U$ = U$ + " WITH A RUNNING ONE-HANDER"
                            Case 46: U$ = U$ + " PULLS UP"
                            Case 47: U$ = U$ + " KISSES THE GLASS"
                            Case 48: U$ = U$ + " SPINS AND SHOOTS"
                            Case 49: U$ = U$ + " PUTS IT UP FROM 16"
                            Case 50: U$ = U$ + " WITH A LONG JUMPER"
                            Case 51: U$ = U$ + " PUMPS AND SHOOTS"
                            Case 52: U$ = U$ + " DOUBLE PUMPS AND SHOOTS"
                            Case 53: U$ = U$ + "...ONE HANDED FADE AWAY"
                            Case 54: U$ = U$ + "...LEFT HANDED FADE AWAY"
                            Case 55: U$ = U$ + "...HIGH ARCHING SHOT"
                            Case 56: U$ = U$ + " WITH A RUNNING LEFT HANDER"
                            Case 57: U$ = U$ + " WITH A RUNNING SCOOP SHOT"
                            Case 58: U$ = U$ + " WITH THE SCOOP SHOT"
                            Case 59: U$ = U$ + " WITH THE FINGER ROLL"
                        End Select

                    Case 7
                        1438 '

                        Select Case RN
                            Case 1 To 20: U$ = "HE SAILS TO THE HOOP"
                            Case 21 To 23: U$ = "HE TAKES IT TO THE BUCKET"
                            Case 24 To 26: U$ = "HE TAKES IT TO THE HOLE"
                            Case 27 To 29: U$ = "HE SLASHES TO THE HOLE"
                            Case 30 To 32: U$ = "HE SLASHES TO THE BUCKET"
                            Case 33 To 35: U$ = "HE SOARS TO THE BUCKET"
                            Case 36 To 38: U$ = "HE SOARS TO THE HOLE"
                            Case 39 To 40: U$ = "HE GLIDES TO THE HOLE"
                            Case 41 To 60: U$ = "HE GOES IN STRONG"
                            Case 61 To 80: U$ = "HE TAKES IT IN"
                            Case 81 To 90: U$ = "HE TAKES A DEFENDER WITH HIM"
                            Case 81 To 100: U$ = "HE GOES UP WITH IT"
                        End Select

                    Case 8
                        1439 'BACK AFTER REB
                        U$ = "HE "
                        RN = Int(Rnd(1) * 11) + 1

                        Select Case RN
                            Case 1: U$ = U$ + "TAKES IT BACK UP"
                            Case 2: U$ = U$ + "GOES BACK UP WITH IT"
                            Case 3: U$ = U$ + "POWERS HIS WAY UP WITH IT"
                            Case 4: U$ = U$ + "GOES STRONG TO THE BASKET"
                            Case 5: U$ = U$ + "MUSCLES IT UP"
                            Case 6: U$ = U$ + "SHOOTS"
                            Case 7: U$ = U$ + "TAKES IT TO THE GOAL"
                            Case 8: U$ = U$ + "PUTS IT UP AGAIN"
                            Case 9: U$ = U$ + "SHOOTS IT AGAIN"
                            Case 10: U$ = U$ + "CLAWS HIS WAY UP"
                            Case 11: U$ = U$ + "SHOOTS IN TRAFFIC"
                        End Select

                        If B = 2 Or B = 3 And Int(Rnd(1) * 100) + 1 <= 7 Then DK% = 1
                        If B = 4 And Int(Rnd(1) * 100) + 1 <= 15 Then DK% = 1

                End Select

            Case 4
                1440 'THREE GOOD
                U$ = "HE "

                Select Case RN
                    Case 1 To 10: U$ = U$ + "FILLS UP THE BASKET!!!"
                    Case 11 To 20: U$ = U$ + "HITS THE TREY!!!"
                    Case 21 To 30: U$ = "IT'S GOOOODD!!"
                    Case 31 To 40: U$ = U$ + "GETS IT!!!"
                    Case 41 To 50: U$ = "COUNT IT!!"
                    Case 51 To 60: U$ = U$ + "RINGS IT UP!!!"
                    Case 61 To 80: U$ = U$ + "DRAINS IT!!!"
                    Case 81 To 90: U$ = "HE BURIES IT!!!"
                    Case 91 To 100: U$ = "IT'S ALL NET!!!"
                End Select

            Case 5
                1445 'THREE BAD
                U$ = "HE "

                Select Case RN
                    Case 1 To 20: U$ = U$ + "GETS NONE"
                    Case 21 To 25: U$ = "IT NEVER HAD A CHANCE"
                    Case 26 To 35: U$ = "AIRRRR BALLLL!!"
                    Case 36 To 40: U$ = U$ + "MISFIRES"
                    Case 41 To 60: U$ = "IT'S OFF THE RIM...NO GOOD!"
                    Case 61 To 80: U$ = "IT'S SHORT"
                    Case 81 To 100: U$ = U$ + "MISSES"
                End Select

            Case 6
                1450 'FAST BREAK
                U$ = AA$(P, B1!(P, B), 0)
                RN = Int(Rnd(1) * 38) + 1

                Select Case RN
                    Case 1: U$ = U$ + " HAS IT ON THE FAST BREAK!"
                    Case 2: U$ = U$ + " IS IN TRANSITION!"
                    Case 3: U$ = U$ + " STARTS THE BREAK!"
                    Case 4: U$ = U$ + " IS DOWN COURT IN A HURRY!"
                    Case 5: U$ = U$ + " IS ON THE RUN!"
                    Case 6: U$ = U$ + " GOES COAST TO COAST!"
                    Case 7: U$ = U$ + " ON THE BREAK!"
                    Case 8: U$ = U$ + " ON THE RUN!"
                    Case 9: U$ = U$ + " ON THE FAST BREAK!"
                    Case 10: U$ = U$ + " HAS THE FAST BREAK!"
                    Case 11: U$ = U$ + " BREAKS INTO THE FORECOURT!"
                    Case 12: U$ = U$ + " FLIES UPCOURT!"
                    Case 13: U$ = U$ + " STARTS THE FASTBREAK!"
                    Case 14: U$ = U$ + " ON THE MOVE!"
                    Case 15: U$ = U$ + " IN TRANSITION!"
                    Case 16: U$ = U$ + " DOWN COURT IN A HURRY!"
                    Case 17: U$ = U$ + " CHARGES ACROSS THE LINE!"
                    Case 18: U$ = U$ + " BREAKS QUICKLY!"
                    Case 19: U$ = U$ + " BREAKS DOWN COURT!"
                    Case 20: U$ = U$ + " STARTS THE RUNNING GAME!"
                    Case 21: U$ = U$ + " FLIES DOWN THE FLOOR!"
                    Case 22: U$ = U$ + " GOES RIGHT TO WORK!"
                    Case 23: U$ = U$ + " BACK THE OTHER WAY!"
                    Case 24: U$ = U$ + " QUICKLY UP THE FLOOR!"
                    Case 25: U$ = U$ + " PUSHES IT UP COURT!"
                    Case 26: U$ = U$ + " WASTES NO TIME!"
                    Case 27: U$ = U$ + " UPS THE TEMPO!"
                    Case 28: U$ = U$ + " HUSTLES DOWN COURT!"
                    Case 29: U$ = U$ + " RUSHES UP THE FLOOR!"
                    Case 30: U$ = U$ + " ON THE MOVE!"
                    Case 31: U$ = U$ + " STREAKS UP THE COURT!"
                    Case 32: U$ = U$ + " FLIES DOWN THE COURT!"
                    Case 33: U$ = U$ + " JETS DOWN COURT!"
                    Case 34: U$ = U$ + " QUICKLY GETS IT UP COURT!"
                    Case 35: U$ = U$ + " TAKES IT COAST TO COAST!"
                    Case 36: U$ = U$ + " IS RUNNING AND GUNNING!"
                    Case 37: U$ = U$ + " FLIES OVER THE TIME LINE!"
                    Case 38: U$ = U$ + " RUSHES IT UP THE FLOOR!"
                End Select

            Case 7
                1460 'FAST BREAK SCORE
                FB% = 0
                RN = Int(Rnd(1) * 39) + 1

                Select Case RN
                    Case 1: U$ = "DIPSY-DOO DUNK-A-ROO!!!"
                    Case 2: U$ = AA$(P, B1!(P, B), 0) + " MAKES IT HAMMER TIME!!!"
                    Case 3: U$ = AA$(P, B1!(P, B), 0) + " SLAMS IT IN!!!"
                    Case 4: U$ = AA$(P, B1!(P, B), 0) + " WITH THE 2-HANDED SLAM!!!"
                    Case 5: U$ = AA$(P, B1!(P, B), 0) + " SLAMS IT HOME!!!"
                    Case 6: U$ = AA$(P, B1!(P, B), 0) + " JAMS IT HOME!!!"
                    Case 7: U$ = AA$(P, B1!(P, B), 0) + " THROWS IT DOWN!!!"
                    Case 8: U$ = AA$(P, B1!(P, B), 0) + " THUNDERSLAM!!!"
                    Case 9: U$ = "RIM ROCKING JAM!!!"
                    Case 10: U$ = AA$(P, B1!(P, B), 0) + " WITH THE MONSTER SLAM!!"
                    Case 11: U$ = AA$(P, B1!(P, B), 0) + "POWER SLAMS IT!!!"
                    Case 12: U$ = AA$(P, B1!(P, B), 0) + " WITH THE 2-HANDED STUFF!!!"
                    Case 13: U$ = AA$(P, B1!(P, B), 0) + " BACKWARDS JAM!!!"
                    Case 14: U$ = AA$(P, B1!(P, B), 0) + " WITH THE 1-HANDED SLAM!!!"
                    Case 15: U$ = AA$(P, B1!(P, B), 0) + " WITH THE REVERSE SLAM!!!"
                    Case 16: U$ = AA$(P, B1!(P, B), 0) + " WITH THE REVERSE JAM!!!"
                    Case 17: U$ = AA$(P, B1!(P, B), 0) + " WITH THE WINDMILL JAM!!!"
                    Case 18: U$ = AA$(P, B1!(P, B), 0) + " WITH THE WINDMILL SLAM!!!"
                    Case 19: U$ = AA$(P, B1!(P, B), 0) + " WITH THE TOMAHAWK SLAM!!!"
                    Case 20: U$ = AA$(P, B1!(P, B), 0) + " WITH THE 360 SLAM!!!"
                    Case 21: U$ = AA$(P, B1!(P, B), 0) + " WITH THE SPINNING SLAM!!!"
                    Case 22: U$ = AA$(P, B1!(P, B), 0) + " WITH THE LEFT HANDED DUNK!!!"
                    Case 23: U$ = "DOUBLE CLUTCH JAM!!!"
                    Case 24: U$ = AA$(P, B1!(P, B), 0) + " GETS THE FLUSH!!!"
                    Case 25: U$ = AA$(P, B1!(P, B), 0) + " PUNCHES IT HOME!!!"
                    Case 26: U$ = AA$(P, B1!(P, B), 0) + "...DOUBLE PUMP SLAM!!!"
                    Case 27: U$ = AA$(P, B1!(P, B), 0) + "...IN YOUR FACE SLAM!!!"
                    Case 28: U$ = AA$(P, B1!(P, B), 0) + " IS UP": Call DELAY: U$ = "HE SPINS": Call DELAY: U$ = "BEHIND THE BACK SLAM!!!"
                    Case 29: U$ = "HE SLAMS IT HOME!!"
                    Case 30: U$ = "HE LAYS IT UP AND IN!!"
                    Case 31: U$ = "HE BANGS IT IN!!"
                    Case 32: U$ = "HE RAMS IT IN!!"
                    Case 33: U$ = "HE SLAMS IT IN!!!"
                    Case 34: U$ = "DIPSY-DOO DUNK-A-ROO!!!"
                    Case 35: U$ = "HE MAKES IT HAMMER TIME!!!"
                    Case 36: U$ = AA$(P, B1!(P, B), 0) + " WITH THE 2-HANDED SLAM!!!"
                    Case 37: U$ = AA$(P, B1!(P, B), 0) + " IS UP": Call DELAY: U$ = "HE SPINS": Call DELAY: U$ = "BEHIND THE BACK SLAM!!!"
                    Case 38: U$ = "HE JAMS IT HOME!!!"
                    Case 39: U$ = "HE HAMMERS IT HOME!"
                End Select

            Case 8
                1470 'MISS
                If PZ% = 8 Then
                    PZ% = 0
                    FB% = 0
                    U$ = "HE "

                    Select Case RN
                        Case 1 To 20: U$ = U$ + "MISSES THE EASY BASKET!"
                        Case 21 To 40: U$ = U$ + "JAMS IT TOO HARD!!"
                        Case 41 To 60: U$ = U$ + "SLAMS IT OFF THE IRON...NO GOOD!"
                        Case 61 To 80: U$ = U$ + "CAN'T GET IT DOWN!"
                        Case 81 To 100: U$ = U$ + "BLOWS THE EASY BASKET!!"
                    End Select

                Else

                    RN = Int(Rnd(1) * 50) + 1

                    Select Case RN
                        Case 1: U$ = "IT RATTLES IN AND OUT...NO GOOD"
                        Case 2: U$ = "OFF THE IRON"
                        Case 3: U$ = "IT SPINS IN AND OUT"
                        Case 4: U$ = "IT'S OFF THE GLASS"
                        Case 5: U$ = "IT'S NO GOOD"
                        Case 6: U$ = "IT'S OFF THE MARK"
                        Case 7: U$ = "IT'S A BRICK"
                        Case 8: U$ = "OFF THE RIM"
                        Case 9: U$ = "NOT EVEN CLOSE!"
                        Case 10: U$ = "NO GOOD"
                        Case 11: U$ = "HE MISSES"
                        Case 12: U$ = "DOESN'T GET THE ROLL"
                        Case 13: U$ = "OFF THE GLASS"
                        Case 14: U$ = "FALLS SHORT"
                        Case 15: U$ = "NO GOOD"
                        Case 16: U$ = "NO"
                        Case 17: U$ = "HE MISSES"
                        Case 18: U$ = "IN AND OUT"
                        Case 19: U$ = "OFF THE RIM"
                        Case 20: U$ = "OFF THE IRON"
                        Case 21: U$ = "IT'S NOT THERE"
                        Case 22: U$ = "OFF TARGET"
                        Case 23: U$ = "IT RATTLES OUT"
                        Case 24: U$ = "IT DOESN'T FALL"
                        Case 25: U$ = "IT BOUNCES OFF"
                        Case 26: U$ = "IT ROLLS OFF"
                        Case 27: U$ = "SHORT"
                        Case 28: U$ = "IT DOESN'T GO DOWN"
                        Case 29: U$ = "IT'S NO GOOD"
                        Case 30: U$ = "NO GOOD"
                        Case 31: U$ = "HE MISSES"
                        Case 32: U$ = "IT'S A BRICK"
                        Case 33: U$ = "HE FAILS TO CONNECT"
                        Case 34: U$ = "OFF THE SIDE OF THE RIM"
                        Case 35: U$ = "IT COMES UP SHORT"
                        Case 36: U$ = "IT ROLLS OUT"
                        Case 37: U$ = "AROUND AND OUT"
                        Case 38: U$ = "OFF THE BACK OF THE IRON"
                        Case 39: U$ = "ON THE FRONT RIM...ROLLS OFF"
                        Case 40: U$ = "IN THE AIR...NO GOOD"
                        Case 41: U$ = "TOO SHORT"
                        Case 42: U$ = "NO GOOD"
                        Case 43: U$ = "IT COMES UP SHORT"
                        Case 44: U$ = "IT ROLLS OUT"
                        Case 45: U$ = "IN AND OUT"
                        Case 46: U$ = "OFF THE FRONT OF THE IRON"
                        Case 47: U$ = "ON THE RIM...ROLLS OFF"
                        Case 48: U$ = "OFF THE FRONT OF THE RIM"
                        Case 49: U$ = "TOO SHORT"
                        Case 50: U$ = "TOO STRONG"
                    End Select

                End If

            Case 9
                1480 'NOPASS
                U$ = AA$(P, B1!(P, B), 0) + " "
                RN = Int(Rnd(1) * 56) + 1

                Select Case RN
                    Case 1: U$ = U$ + "LOOKS INSIDE"
                    Case 2: U$ = U$ + "TRIES TO WORK FREE"
                    Case 3: U$ = U$ + "SETS UP THE PLAY"
                    Case 4: U$ = U$ + "LOOKS FOR AN OPENING"
                    Case 5: U$ = U$ + "DRIBBLES AROUND HIS MAN"
                    Case 6: U$ = U$ + "SIGNALS THE PLAY"
                    Case 7: U$ = U$ + "IS IN CONTROL"
                    Case 8: U$ = U$ + "IS IN POSSESSION"
                    Case 9: U$ = U$ + "TRIES TO GET OPEN"
                    Case 10: U$ = U$ + "YO-YO'S OUT TOP"
                    Case 11: U$ = U$ + "GOES TO WORK"
                    Case 12: U$ = U$ + "WORKS OFF THE PICK"
                    Case 13: U$ = U$ + "ON THE DRIBBLE"
                    Case 14: U$ = U$ + "CUTS TO THE LEFT"
                    Case 15: U$ = U$ + "CUTS TO THE RIGHT"
                    Case 16: U$ = U$ + "WHEELS AND DEALS"
                    Case 17: U$ = U$ + "SETS UP ON TOP"
                    Case 18: U$ = U$ + "SLIDES RIGHT"
                    Case 19: U$ = U$ + "SLIDES LEFT"
                    Case 20: U$ = U$ + "GOES TO WORK"
                    Case 21: U$ = U$ + "MOVES IT LEFT-SIDE"
                    Case 22: U$ = U$ + "MOVES IT RIGHT-SIDE"
                    Case 23: U$ = U$ + "REVERSES THE DRIBBLE"
                    Case 24: U$ = U$ + "TRIES TO GET OPEN"
                    Case 25: U$ = U$ + "DRIBBLES TO THE MIDDLE"
                    Case 26: U$ = U$ + "FAKES THE PASS"
                    Case 27: U$ = U$ + "SETS UP ON TOP"
                    Case 28: U$ = U$ + "SLIDES RIGHT"
                    Case 29: U$ = U$ + "SLIDES LEFT"
                    Case 30: U$ = U$ + "DRIBBLES IT BEHIND THE BACK"
                    Case 31: U$ = U$ + "FAKES LEFT...GOES RIGHT"
                    Case 32: U$ = U$ + "FAKES RIGHT...GOES LEFT"
                    Case 33: U$ = U$ + "SPINS HARD TO THE LEFT"
                    Case 34: U$ = U$ + "SPINS HARD TO THE RIGHT"
                    Case 35: U$ = U$ + "LEANS LEFT...GOES RIGHT"
                    Case 36: U$ = U$ + "LEANS RIGHT...GOES LEFT"
                    Case 37: U$ = U$ + "LEANS LEFT...CUTS RIGHT"
                    Case 38: U$ = U$ + "LEANS RIGHT...CUTS LEFT"
                    Case 39: U$ = U$ + "DOUBLE REVERSES HIS DRIBBLE"
                    Case 40: U$ = U$ + "BACKS IN"
                    Case 41: U$ = U$ + "LEANS FORWARD"
                    Case 42: U$ = U$ + "CROSSES OVER...SPINS"
                    Case 43: U$ = U$ + "CUTS LEFT"
                    Case 44: U$ = U$ + "CUTS RIGHT"
                    Case 45: U$ = U$ + "WITH THE HESITATION DRIBBLE": Call DELAY: U$ = "HE SPINS LEFT"
                    Case 46: U$ = U$ + "WITH THE HESITATION DRIBBLE": Call DELAY: U$ = "HE SPINS RIGHT"
                    Case 47: U$ = U$ + "WITH THE HESITATION DRIBBLE": Call DELAY: U$ = "HE CUTS LEFT"
                    Case 48: U$ = U$ + "WITH THE HESITATION DRIBBLE": Call DELAY: U$ = "HE CUTS RIGHT"
                    Case 49: U$ = U$ + "STUTTER STEPS": Call DELAY: U$ = "HE MOVES LEFT"
                    Case 50: U$ = U$ + "MAINTAINS HIS DRIBBLE"
                    Case 51: U$ = U$ + "FAKE SPINS LEFT...GOES RIGHT"
                    Case 52: U$ = U$ + "FAKE SPINS RIGHT...GOES LEFT"
                    Case 53: U$ = U$ + "PALMS THE BALL": Call DELAY: U$ = "HE TAKES IT AND SPINS"
                    Case 54: U$ = U$ + "SHAKES AND BAKES"
                    Case 55: U$ = U$ + "DRIBBLES WITH THE LEFT HAND"
                    Case 56: U$ = U$ + "DRIBBLES WITH THE RIGHT HAND"
                End Select

            Case 10
                1490 'PASS PB$=PREV HANDLER
                V$ = AA$(P, B1!(P, B), 0)

                RN = Int(Rnd(1) * 60) + 1

                Select Case RN
                    Case 1: U$ = PB$ + " TO " + V$
                    Case 2: U$ = PB$ + " KICKS IT OUT": Call DELAY: U$ = V$ + " TAKES THE PASS"
                    Case 3: U$ = PB$ + " FEEDS " + V$
                    Case 4: U$ = PB$ + " PASSES TO " + V$
                    Case 4: U$ = PB$ + " DISHES TO " + V$
                    Case 5: U$ = PB$ + " GIVES IT TO " + V$
                    Case 6: U$ = PB$ + " WITH A NICE GIVE TO " + V$
                    Case 7: U$ = PB$ + " LOOKS": Call DELAY: U$ = "HE GETS IT TO " + V$
                    Case 8: U$ = PB$ + " SENDS IT TO " + V$
                    Case 9: U$ = PB$ + " TO " + V$
                    Case 10: U$ = PB$ + " GETS IT TO " + V$
                    Case 11: U$ = PB$ + " OVER TO " + V$
                    Case 12: U$ = PB$ + " ZIPS IT TO " + V$
                    Case 13: U$ = PB$ + " FINDS " + V$
                    Case 14: U$ = PB$ + " LOOKS FOR " + V$
                    Case 14: U$ = PB$ + " THROWS IT TO " + V$
                    Case 15: U$ = PB$ + " GOES TO " + V$
                    Case 16: U$ = PB$ + " SETS UP " + V$
                    Case 17: U$ = PB$ + " OUT TO " + V$
                    Case 18: U$ = PB$ + " FLIPS IT TO " + V$
                    Case 19: U$ = "BOUNCE PASS TO " + V$
                    Case 20: U$ = PB$ + " LEAVES IT FOR " + V$
                    Case 21: U$ = PB$ + " LOOPS IT TO " + V$
                    Case 22: U$ = PB$ + " SHOVELS IT OVER TO " + V$
                    Case 23: U$ = PB$ + " GIVES IT BACK TO " + V$
                    Case 24: U$ = PB$ + " FEEDS IT TO " + V$
                    Case 25: U$ = PB$ + " GIVES IT OFF TO " + V$
                    Case 26: U$ = "PASSES TO " + V$
                    Case 27: U$ = "DISHES TO " + V$
                    Case 28: U$ = "GIVES TO " + V$
                    Case 29: U$ = "OVER TO " + V$
                    Case 30: U$ = "SENDS IT BACK TO " + V$
                    Case 31: U$ = "NO-LOOK PASS TO " + V$
                    Case 32: U$ = "NO-LOOK BOUNCE PASS TO " + V$
                    Case 33: U$ = "BEHIND THE BACK TO " + V$
                    Case 34: U$ = PB$ + " SWINGS IT TO " + V$
                    Case 35: U$ = PB$ + " FIRES A PASS TO " + V$
                    Case 36: U$ = PB$ + " SNAPS A PASS TO " + V$
                    Case 37: U$ = PB$ + " SWINGS A PASS TO " + V$
                    Case 38: U$ = "TWO-HANDED PASS TO " + V$
                    Case 39: U$ = "WRAP AROUND PASS TO " + V$
                    Case 40: U$ = "OVER THE SHOULDER PASS TO " + V$
                    Case 41: U$ = "BACKHAND PASS TO " + V$
                    Case 42: U$ = "BACKHAND DISH TO " + V$
                    Case 43: U$ = "PASS CONNECTED TO " + V$
                    Case 44: U$ = PB$ + " WIRES IT TO " + V$
                    Case 45: U$ = PB$ + " WIRES A PASS TO " + V$
                    Case 46: U$ = PB$ + " SNAPS A PASS TO " + V$
                    Case 47: U$ = PB$ + " FAKES..PASSES TO " + V$
                    Case 48: U$ = PB$ + " SHUFFLES IT TO " + V$
                    Case 49: U$ = PB$ + " SLINGS IT TO " + V$
                    Case 50: U$ = PB$ + " FLIPS IT TO " + V$
                    Case 51: U$ = PB$ + " WHIPS IT OVER TO " + V$
                    Case 52: U$ = PB$ + " SLINGS A BOUNCE PASS TO " + V$
                    Case 53: U$ = PB$ + " SHOVELS IT TO " + V$
                    Case 54: U$ = PB$ + " PUMP FAKES A SHOT": Call DELAY: U$ = "HE SWINGS IT TO " + V$
                    Case 55: U$ = PB$ + " PUMP FAKES A SHOT": Call DELAY: U$ = "HE BOUNCES IT TO " + V$
                    Case 56: U$ = PB$ + " WITH A FOOT FAKE": Call DELAY: U$ = "HE SPINS AND DUMPS IT TO " + V$
                    Case 57: U$ = PB$ + " CUTS ACROSS THE LANE": Call DELAY: U$ = "HE DISHES INSIDE TO " + V$
                    Case 58: U$ = PB$ + " DRIVES THE BASELINE": Call DELAY: U$ = "DISHES UNDERNEATH TO " + V$
                    Case 59: U$ = PB$ + " FAKES RIGHT...CUTS LEFT": Call DELAY: U$ = "SHUFFLES IT TO " + V$
                    Case 60: U$ = PB$ + " HEAD FAKES": Call DELAY: U$ = "HE PUSHES IT TO " + V$
                End Select

            Case Else
                U$ = AA$(P, B1!(P, B), 0)

        End Select

    End If

End Sub

'------------------------------
'      MISSFT Subroutine
'------------------------------
' ...explanation...
Sub MISSFT ()

    If pbpDelay! <> 0 Then
        Call MOREDELAY
        Locate F3 + 9, 1
        Print F3; " MISSED"
        Call MOREDELAY
    End If

End Sub

'------------------------------
'      MOREDELAY Subroutine
'------------------------------
' ...explanation...
Sub MOREDELAY ()

    'Do we really need the IF statement?
    'I understand why it's there but
    'does it save any time?
    'If pbpDelay! > 0 Then

    _Delay pbpDelay!

    'End If

End Sub

'------------------------------
'      ROSTER Subroutine
'------------------------------
' ...explanation...
Sub ROSTER ()

    Cls
    Color T(P9, 33), T(P9, 34)
    Print B$(P9);
    Color 15, 0
    Print " TEAM ROSTER   COACH: "; C$(P9); "     ";
    Print "  RECORD: ";: Print Using "##"; T(P9, 1) + T(P9, 3);: Print "-";: Print Using "##"; T(P9, 2) + T(P9, 4)
    Color 14: Print "# NAME         PS GP MG FG 3FG %3A FT  ORB  DRB  A  S  B CX CN PT F  FC  PPG"
    Locate 3, 1

    For R1 = 0 To 14

        If B%(P9, R1, 18) <> 0 Or AA$(P9, R1, 0) <> "XXX" Then
            Color 2

            If R1 <= 9 Then Print Using "# "; R1;
            If R1 > 9 Then Print X$(R1 - 10);

            Color 15

            Print Left$(AA$(P9, R1, 0), 12);: Locate , 16

            If Len(AA$(P9, R1, 1)) = 1 Then Print AA$(P9, R1, 1);
            If Len(AA$(P9, R1, 1)) > 1 Then Print Left$(AA$(P9, R1, 1), 1); Right$(AA$(P9, R1, 1), 1);

            L = 0: Locate , 19: L = A!(P9, R1, 7) + A!(P9, R1, 11) + A!(P9, R1, 12)

            Print Using "## ##"; B%(P9, R1, 0); B%(P9, R1, 1) / B%(P9, R1, 0);
            Print Using "### ### ## ### ##.# ##.# ## ## ##"; A!(P9, R1, 0); A!(P9, R1, 20); A!(P9, R1, 19); A!(P9, R1, 1); A!(P9, R1, 21) / 10; A!(P9, R1, 2) / 10; A!(P9, R1, 4) / 10; A!(P9, R1, 5); A!(P9, R1, 6);
            Print Using "### "; A!(P9, R1, 3) - L;

            J = (A!(P9, R1, 3)) - A!(P9, R1, 7) - A!(P9, R1, 11) - A!(P9, R1, 12)

            If H <= 3 And J / A!(P9, R1, 3) < .25 Then Locate , 61: Print " T";
            If J / A!(P9, R1, 3) < .5 And H <= 2 Then Locate , 61: Print " T";
            If H = 4 And T > 200 And J / A!(P9, R1, 3) <= T / 2880 Then Locate , 61: Print " T";
            If A!(P9, R1, 3) - A!(P9, R1, 7) - A!(P9, R1, 11) - A!(P9, R1, 12) <= 0 Then Locate , 61: Print "EX";
            If H > 4 Or H = 4 And T <= 120 And T <> 0 Then Locate , 61: Print "  ";

            Locate , 63: Print Using "### # ### ##.#"; A!(P9, R1, 8) * 2 + A!(P9, R1, 10) + W%(P9, R1, 0); A!(P9, R1, 12); A!(P9, R1, 17); (B%(P9, R1, 3) * 2 + B%(P9, R1, 5) + B%(P9, R1, 14)) / B%(P9, R1, 0)
        End If

    Next R1

    Locate 18, 1
    Color 9, 0: Print String$(27, 205); Chr$(203); String$(51, 205)

    For R1 = 1 To 7: Locate R1 + 18, 28: Print Chr$(186);: Next

End Sub

'------------------------------
'    SELECTPBP Subroutine
'------------------------------
' ...explanation...
Sub SELECTPBP ()

    If PB% = 0 Then Call DELAY
    If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print U$: Call MOREDELAY

    Call MOREDELAY

End Sub

'------------------------------
'      TEAMMENU Subroutine
'------------------------------
' ...explanation...
Sub TEAMMENU (BO%, YN$)

    Dim fileLength&

    If BO% = 2 Then
        BO% = 0
    Else
        NTMS = 0
        Open diskPaths$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031
        fileLength& = LOF(1)

        Field #1, 15 As Q$(0), 15 As Q$(1)
        Field #1, 44 As X$, 2 As Q$(9)

        For I1 = 1 To fileLength& / 1031
            Get #1, I1
            CK% = CVI(Q$(9))
            If CK% = 999 Then
                NTMS = NTMS + 1
                teamNames$(NTMS) = Q$(0)
                teamIndex%(NTMS) = I1
            End If
        Next
        Close 1
    End If

    1102 SELECTION% = 1
    Count% = NTMS
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
    MAMenu teamNames$(), SELECTION%, START%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, ROWS%, COLUMNS%, SPACING%, ROW%, COLUMN%
    _MouseHide

    If ExitCode% = 27 Then
        BO% = 1
    Else
        I1 = teamIndex%(SELECTION%)
        Call GETATEAM(I1, YN$)
    End If

End Sub

'------------------------------
'      THREEFT Subroutine
'------------------------------
' ...explanation...
Sub THREEFT ()
    A!(P, B1!(P, B), 10) = A!(P, B1!(P, B), 10) + 1

    RN = Int(Rnd(1) * 100) + 1

    Select Case F3

        Case 1
            If RN <= 50 Then
                U$ = "FREE THROW IS GOOD"
            Else
                U$ = "HE HITS THE FIRST FREE THROW"
            End If

        Case 2
            If RN <= 50 Then
                U$ = "SECOND FREE THROW IS GOOD"
            Else
                U$ = "HE HITS THE SECOND ONE"
            End If

        Case 3
            If RN <= 50 Then
                U$ = "THIRD FREE THROW IS GOOD"
            Else
                U$ = "HE HITS THE LAST ONE"
            End If
    End Select

    Call DELAY

    S!(P, H) = S!(P, H) + 1
    S!(P, 0) = S!(P, 0) + 1

    Call UPDATESCREEN

End Sub

'------------------------------
'      TICKER Subroutine
'------------------------------
' ...explanation...
Sub TICKER ()
    ' *** TICKER ***

    If AP <> 1 Or U5 <> 2 Then

        Color 14, 0: Cls

        RN = Int(Rnd(1) * 100) + 1

        If HT% = 1 Then
            If G9 = 0 Then
                If RN <= 50 Then
                    Locate , 23: Print "PRUDENTIAL AT THE HALF SCOREBOARD"
                Else
                    Locate , 28: Print "RED MAN HALFTIME REPORT"
                End If
            Else
                If RN <= 50 Then
                    Locate , 29: Print "TOYOTA HALFTIME REPORT"
                Else
                    Locate , 29: Print "HONDA HALFTIME REPORT"
                End If
            End If
        Else

            If EG% = 1 Then
                If G9 = 0 Then
                    Locate , 27: Print "CHEVROLET FINAL SCOREBOARD"
                Else
                    Select Case RN
                        Case 1 To 33: Locate , 27: Print "USA TODAY FINAL SCOREBOARD"
                        Case 34 To 67: Locate , 24: Print "FEDERAL EXPRESS FINAL SCOREBOARD"
                        Case 67 To 100: Locate , 23: Print "ESPN SPORTSCENTER FINAL SCOREBOARD"
                    End Select
                End If
            Else
                If HT% = 0 Then Locate , 30: Print "LANCE HAFFNER GAMES SCOREBOARD"
            End If

        End If

        2851 Color 15

        If TMT = 0 Then
            Locate 10, 28: Print "NO OTHER GAMES SCHEDULED"
        Else
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

            If Left$(SX$(1, 0), 3) <> "XXX" Then
                Locate 2, 1
                X = 1
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(1, 0), 13);
                Locate , 15: Print Using "### "; SX%(1, 0, TS%);: Print R$(TS%)
                Print Left$(SX$(1, 1), 13);
                Locate , 15: Print Using "### #"; SX%(1, 1, TS%)
            End If

            2852 If Left$(SX$(2, 0), 3) <> "XXX" Then
                Locate 5, 1: X = 2
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(2, 0), 13);: Locate , 15: Print Using "### "; SX%(2, 0, TS%);: Print R$(TS%)
                Print Left$(SX$(2, 1), 13);: Locate , 15: Print Using "### #"; SX%(2, 1, TS%)
            End If

            2853 If Left$(SX$(3, 0), 3) <> "XXX" Then
                Locate 8, 1: X = 3
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(3, 0), 13);: Locate , 15: Print Using "### "; SX%(3, 0, TS%);: Print R$(TS%)
                Print Left$(SX$(3, 1), 13);: Locate , 15: Print Using "### #"; SX%(3, 1, TS%)
            End If

            2854 If Left$(SX$(4, 0), 3) <> "XXX" Then
                Locate 11, 1: X = 4
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(4, 0), 13);: Locate , 15: Print Using "### "; SX%(4, 0, TS%);: Print R$(TS%)
                Print Left$(SX$(4, 1), 13);: Locate , 15: Print Using "### #"; SX%(4, 1, TS%)
            End If

            2855 If Left$(SX$(5, 0), 3) <> "XXX" Then
                Locate 14, 1: X = 5
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(5, 0), 13);: Locate , 15: Print Using "### "; SX%(5, 0, TS%);: Print R$(TS%)
                Print Left$(SX$(5, 1), 13);: Locate , 15: Print Using "### #"; SX%(5, 1, TS%)
            End If

            2856 If Left$(SX$(6, 0), 3) <> "XXX" Then
                Locate 17, 1: X = 6
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(6, 0), 13);: Locate , 15: Print Using "### "; SX%(6, 0, TS%);: Print R$(TS%)
                Print Left$(SX$(6, 1), 13);: Locate , 15: Print Using "### #"; SX%(6, 1, TS%)
            End If

            2857 If Left$(SX$(7, 0), 3) <> "XXX" Then
                Locate 20, 1: X = 7
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(7, 0), 13);: Locate , 15: Print Using "### "; SX%(7, 0, TS%);: Print R$(TS%)
                Print Left$(SX$(7, 1), 13);: Locate , 15: Print Using "### #"; SX%(7, 1, TS%)
            End If

            2858 If Left$(SX$(8, 0), 3) <> "XXX" Then
                Locate 23, 1: X = 8
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(8, 0), 13);: Locate , 15: Print Using "### "; SX%(8, 0, TS%);: Print R$(TS%)
                Print Left$(SX$(8, 1), 13);: Locate , 15: Print Using "### #"; SX%(8, 1, TS%);
            End If

            2859 If Left$(SX$(9, 0), 3) <> "XXX" Then
                Locate 2, 21: X = 9
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(9, 0), 13);: Locate , 35: Print Using "### "; SX%(9, 0, TS%);: Print R$(TS%)
                Locate , 21: Print Left$(SX$(9, 1), 13);: Locate , 35: Print Using "### #"; SX%(9, 1, TS%)
            End If

            2860 If Left$(SX$(10, 0), 3) <> "XXX" Then
                Locate 5, 21: X = 10
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(10, 0), 13);: Locate , 35: Print Using "### "; SX%(10, 0, TS%);: Print R$(TS%)
                Locate , 21: Print Left$(SX$(10, 1), 13);: Locate , 35: Print Using "### #"; SX%(10, 1, TS%)
            End If

            2861 If Left$(SX$(11, 0), 3) <> "XXX" Then
                Locate 8, 21: X = 11
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(11, 0), 13);: Locate , 35: Print Using "### "; SX%(11, 0, TS%);: Print R$(TS%)
                Locate , 21: Print Left$(SX$(11, 1), 13);: Locate , 35: Print Using "### #"; SX%(11, 1, TS%)
            End If

            2862 If Left$(SX$(12, 0), 3) <> "XXX" Then
                Locate 11, 21: X = 12
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(12, 0), 13);: Locate , 35: Print Using "### "; SX%(12, 0, TS%);: Print R$(TS%)
                Locate , 21: Print Left$(SX$(12, 1), 13);: Locate , 35: Print Using "### #"; SX%(12, 1, TS%)
            End If

            2863 If Left$(SX$(13, 0), 3) <> "XXX" Then
                Locate 14, 21: X = 13
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(13, 0), 13);: Locate , 35: Print Using "### "; SX%(13, 0, TS%);: Print R$(TS%)
                Locate , 21: Print Left$(SX$(13, 1), 13);: Locate , 35: Print Using "### #"; SX%(13, 1, TS%)
            End If

            2864 If Left$(SX$(14, 0), 3) <> "XXX" Then
                Locate 17, 21: X = 14
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(14, 0), 13);: Locate , 35: Print Using "### "; SX%(14, 0, TS%);: Print R$(TS%)
                Locate , 21: Print Left$(SX$(14, 1), 13);: Locate , 35: Print Using "### #"; SX%(14, 1, TS%)
            End If

            2865 If Left$(SX$(15, 0), 3) <> "XXX" Then
                Locate 20, 21: X = 15
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(15, 0), 13);: Locate , 35: Print Using "### "; SX%(15, 0, TS%);: Print R$(TS%)
                Locate , 21: Print Left$(SX$(15, 1), 13);: Locate , 35: Print Using "### #"; SX%(15, 1, TS%)
            End If

            2866 If Left$(SX$(16, 0), 3) <> "XXX" Then
                Locate 23, 21: X = 16
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(16, 0), 13);: Locate , 35: Print Using "### "; SX%(16, 0, TS%);: Print R$(TS%)
                Locate , 21: Print Left$(SX$(16, 1), 13);: Locate , 35: Print Using "### #"; SX%(16, 1, TS%);
            End If

            2867 If Left$(SX$(17, 0), 3) <> "XXX" Then
                Locate 2, 41: X = 17
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(17, 0), 13);: Locate , 55: Print Using "### "; SX%(17, 0, TS%);: Print R$(TS%)
                Locate , 41: Print Left$(SX$(17, 1), 13);: Locate , 55: Print Using "### #"; SX%(17, 1, TS%)
            End If

            2868 If Left$(SX$(18, 0), 3) <> "XXX" Then
                Locate 5, 41: X = 18
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(18, 0), 13);: Locate , 55: Print Using "### "; SX%(18, 0, TS%);: Print R$(TS%)
                Locate , 41: Print Left$(SX$(18, 1), 13);: Locate , 55: Print Using "### #"; SX%(18, 1, TS%)
            End If

            2869 If Left$(SX$(19, 0), 3) <> "XXX" Then
                Locate 8, 41: X = 19
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(19, 0), 13);: Locate , 55: Print Using "### "; SX%(19, 0, TS%);: Print R$(TS%)
                Locate , 41: Print Left$(SX$(19, 1), 13);: Locate , 55: Print Using "### #"; SX%(19, 1, TS%)
            End If

            2870 If Left$(SX$(20, 0), 3) <> "XXX" Then
                Locate 11, 41: X = 20
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(20, 0), 13);: Locate , 55: Print Using "### "; SX%(20, 0, TS%);: Print R$(TS%)
                Locate , 41: Print Left$(SX$(20, 1), 13);: Locate , 55: Print Using "### #"; SX%(20, 1, TS%)
            End If

            2871 If Left$(SX$(21, 0), 3) <> "XXX" Then
                Locate 14, 41: X = 21
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(21, 0), 13);: Locate , 55: Print Using "### "; SX%(21, 0, TS%);: Print R$(TS%)
                Locate , 41: Print Left$(SX$(21, 1), 13);: Locate , 55: Print Using "### #"; SX%(21, 1, TS%)
            End If

            2872 If Left$(SX$(22, 0), 3) <> "XXX" Then
                Locate 17, 41: X = 22
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(22, 0), 13);: Locate , 55: Print Using "### "; SX%(22, 0, TS%);: Print R$(TS%)
                Locate , 41: Print Left$(SX$(22, 1), 13);: Locate , 55: Print Using "### #"; SX%(22, 1, TS%)
            End If

            2873 If Left$(SX$(23, 0), 3) <> "XXX" Then
                Locate 20, 41: X = 23
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(23, 0), 13);: Locate , 55: Print Using "### "; SX%(23, 0, TS%);: Print R$(TS%)
                Locate , 41: Print Left$(SX$(23, 1), 13);: Locate , 55: Print Using "### #"; SX%(23, 1, TS%)
            End If

            2874 If Left$(SX$(24, 0), 3) <> "XXX" Then
                Locate 23, 41: X = 24
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(24, 0), 13);: Locate , 55: Print Using "### "; SX%(24, 0, TS%);: Print R$(TS%)
                Locate , 41: Print Left$(SX$(24, 1), 13);: Locate , 55: Print Using "### #"; SX%(24, 1, TS%);
            End If

            2875 If Left$(SX$(25, 0), 3) <> "XXX" Then
                Locate 2, 61: X = 25
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(25, 0), 13);: Locate , 75: Print Using "### "; SX%(25, 0, TS%);: Print R$(TS%)
                Locate , 61: Print Left$(SX$(25, 1), 13);: Locate , 75: Print Using "### #"; SX%(25, 1, TS%)
            End If

            2876 If Left$(SX$(26, 0), 3) <> "XXX" Then
                Locate 5, 61: X = 26
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(26, 0), 13);: Locate , 75: Print Using "### "; SX%(26, 0, TS%);: Print R$(TS%)
                Locate , 61: Print Left$(SX$(26, 1), 13);: Locate , 75: Print Using "### #"; SX%(26, 1, TS%)
            End If

            2877 If Left$(SX$(27, 0), 3) <> "XXX" Then
                Locate 8, 61: X = 27
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(27, 0), 13);: Locate , 75: Print Using "### "; SX%(27, 0, TS%);: Print R$(TS%)
                Locate , 61: Print Left$(SX$(27, 1), 13);: Locate , 75: Print Using "### #"; SX%(27, 1, TS%)
            End If

            2878 If Left$(SX$(28, 0), 3) <> "XXX" Then
                Locate 11, 61: X = 28
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(28, 0), 13);: Locate , 75: Print Using "### "; SX%(28, 0, TS%);: Print R$(TS%)
                Locate , 61: Print Left$(SX$(28, 1), 13);: Locate , 75: Print Using "### #"; SX%(28, 1, TS%)
            End If

            2879 If Left$(SX$(29, 0), 3) <> "XXX" Then
                Locate 14, 61: X = 29
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(29, 0), 13);: Locate , 75: Print Using "### "; SX%(29, 0, TS%);: Print R$(TS%)
                Locate , 61: Print Left$(SX$(29, 1), 13);: Locate , 75: Print Using "### #"; SX%(29, 1, TS%)
            End If

            2880 If Left$(SX$(30, 0), 3) <> "XXX" Then
                Locate 17, 61: X = 30
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(30, 0), 13);: Locate , 75: Print Using "### "; SX%(30, 0, TS%);: Print R$(TS%)
                Locate , 61: Print Left$(SX$(30, 1), 13);: Locate , 75: Print Using "### #"; SX%(30, 1, TS%)
            End If

            2881 If Left$(SX$(31, 0), 3) <> "XXX" Then
                Locate 20, 61: X = 31
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(31, 0), 13);: Locate , 75: Print Using "### "; SX%(31, 0, TS%);: Print R$(TS%)
                Locate , 61: Print Left$(SX$(31, 1), 13);: Locate , 75: Print Using "### #"; SX%(31, 1, TS%)
            End If

            2882 If Left$(SX$(32, 0), 3) <> "XXX" Then
                Locate 23, 61: X = 32
                TS% = TE + (TTS% - ST%(X)) * 3
                If TS% < 0 Then TS% = 0
                If TS% > 14 Then TS% = 14
                If EG% = 1 Then TS% = 14

                Print Left$(SX$(32, 0), 13);: Locate , 75: Print Using "### "; SX%(32, 0, TS%);: Print R$(TS%)
                Locate , 61: Print Left$(SX$(32, 1), 13);: Locate , 75: Print Using "### #"; SX%(32, 1, TS%);
            End If

        End If

        2883 I$ = GetKeyPress$

        Call NEWSCREEN

    End If

End Sub

'------------------------------
'      WINDEX Subroutine
'------------------------------
' ...explanation...
Sub WINDEX ()

    For I = 8 To 15
        Locate I, 1
        Print Space$(38);
    Next I

    Locate 8, 1

End Sub

'------------------------------
'      RECORDSCHECK Subroutine
'------------------------------
' ...explanation...
Sub RECORDSCHECK ()

    'Dim RC$(50, 4), REC!(50, 2) As Single
    Dim TRC$(125, 3), TREC(125, 2) As Single
    Dim V8(1, 14)
    Dim SITE$(1)

    SITE$(0) = "AT"
    SITE$(1) = "VS."

    If Not _FileExists(diskPaths$(1) + "RECORDS.GAM") Then
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
    Else
        Open diskPaths$(1) + "RECORDS.GAM" For Input As #1

        For I = 0 To 48
            For I1 = 0 To 2
                Input #1, REC!(I, I1)
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
    End If

    505 'CHECK THE INDIVIDUAL RECORDS
    For I = 0 To 1

        For I1 = 0 To 14

            ' SCORING RECORDS FOR POINTS IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) > REC!(0, 0) Then
                REC!(0, 0) = A!(I, I1, 8) * 2 + A!(I, I1, 10) + W%(I, I1, 0) ' POINTS
                REC!(0, 1) = 1 ' TIMES DONE
                REC!(0, 2) = 0 ' GAME
                RC$(0, 0) = AA$(I, I1, 0) ' NAME
                RC$(0, 1) = B$(I) ' TEAM
                RC$(0, 2) = SITE$(I) ' AT/VS
                RC$(0, 3) = B$(1 - I) ' OPPONENT
                RC$(0, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 13) + QQ(I, 2, I1, 13) > REC!(1, 0) Then
                REC!(1, 0) = QQ(I, 1, I1, 13) + QQ(I, 2, I1, 13) ' POINTS
                REC!(1, 1) = 1 ' TIMES DONE
                REC!(1, 2) = 1 ' HALF
                RC$(1, 0) = AA$(I, I1, 0) ' NAME
                RC$(1, 1) = B$(I) ' TEAM
                RC$(1, 2) = SITE$(I) ' AT/VS
                RC$(1, 3) = B$(1 - I) ' OPPONENT
                RC$(1, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 13) + QQ(I, 4, I1, 13) > REC!(1, 0) Then
                REC!(1, 0) = QQ(I, 3, I1, 13) + QQ(I, 4, I1, 13) ' POINTS
                REC!(1, 1) = 1 ' TIMES DONE
                REC!(1, 2) = 2 ' HALF
                RC$(1, 0) = AA$(I, I1, 0) ' NAME
                RC$(1, 1) = B$(I) ' TEAM
                RC$(1, 2) = SITE$(I) ' AT/VS
                RC$(1, 3) = B$(1 - I) ' OPPONENT
                RC$(1, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 13) > REC!(2, 0) Then
                    REC!(2, 0) = QQ(I, I2, I1, 13) ' POINTS
                    REC!(2, 1) = 1 ' TIMES DONE
                    REC!(2, 2) = I2 ' QTR
                    RC$(2, 0) = AA$(I, I1, 0) ' NAME
                    RC$(2, 1) = B$(I) ' TEAM
                    RC$(2, 2) = SITE$(I) ' AT/VS
                    RC$(2, 3) = B$(1 - I) ' OPPONENT
                    RC$(2, 4) = Date$ ' DATE
                End If
            Next I2

            For I2 = 5 To 8
                If QQ(I, I2, I1, 13) > REC!(3, 0) Then
                    REC!(3, 0) = QQ(I, I2, I1, 13) ' POINTS
                    REC!(3, 1) = 1 ' TIMES DONE
                    REC!(3, 2) = I2 ' QTR
                    RC$(3, 0) = AA$(I, I1, 0) ' NAME
                    RC$(3, 1) = B$(I) ' TEAM
                    RC$(3, 2) = SITE$(I) ' AT/VS
                    RC$(3, 3) = B$(1 - I) ' OPPONENT
                    RC$(3, 4) = Date$ ' DATE
                End If
            Next I2

            ' FIELD GOAL PERCENTAGE RECORDS FOR HIGH %, MOST W/NO MISSES, MOST W/NONE MADE
            If A!(I, I1, 8) >= 15 Then
                If A!(I, I1, 8) / A!(I, I1, 7) > REC!(4, 0) Then
                    REC!(4, 0) = A!(I, I1, 8) / A!(I, I1, 7) ' PCTG
                    REC!(4, 1) = A!(I, I1, 8) ' FGM
                    REC!(4, 2) = A!(I, I1, 7) ' FGA
                    RC$(4, 0) = AA$(I, I1, 0) ' NAME
                    RC$(4, 1) = B$(I) ' TEAM
                    RC$(4, 2) = SITE$(I) ' AT/VS
                    RC$(4, 3) = B$(1 - I) ' OPPONENT
                    RC$(4, 4) = Date$ ' DATE
                End If
            End If

            900
            If A!(I, I1, 8) = A!(I, I1, 7) And A!(I, I1, 7) > REC!(5, 0) Then
                REC!(5, 0) = A!(I, I1, 7) ' MOST FGM
                RC$(5, 0) = AA$(I, I1, 0) ' NAME
                RC$(5, 1) = B$(I) ' TEAM
                RC$(5, 2) = SITE$(I) ' AT/VS
                RC$(5, 3) = B$(1 - I) ' OPPONENT
                RC$(5, 4) = Date$ ' DATE
            End If

            If A!(I, I1, 8) = 0 And A!(I, I1, 7) > REC!(6, 0) Then
                REC!(6, 0) = A!(I, I1, 7) ' 0 FGM /FGA
                RC$(6, 0) = AA$(I, I1, 0) ' NAME
                RC$(6, 1) = B$(I) ' TEAM
                RC$(6, 2) = SITE$(I) ' AT/VS
                RC$(6, 3) = B$(1 - I) ' OPPONENT
                RC$(6, 4) = Date$ ' DATE
            End If

            ' FIELD GOALS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If A!(I, I1, 8) > REC!(7, 0) Then
                REC!(7, 0) = A!(I, I1, 8) ' FGM
                REC!(7, 1) = 1 ' TIMES DONE
                REC!(7, 2) = 0 ' GAME
                RC$(7, 0) = AA$(I, I1, 0) ' NAME
                RC$(7, 1) = B$(I) ' TEAM
                RC$(7, 2) = SITE$(I) ' AT/VS
                RC$(7, 3) = B$(1 - I) ' OPPONENT
                RC$(7, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 2) + QQ(I, 2, I1, 2) > REC!(8, 0) Then
                REC!(8, 0) = QQ(I, 1, I1, 2) + QQ(I, 2, I1, 2) ' FGM
                REC!(8, 1) = 1 ' TIMES DONE
                REC!(8, 2) = 1 ' HALF
                RC$(8, 0) = AA$(I, I1, 0) ' NAME
                RC$(8, 1) = B$(I) ' TEAM
                RC$(8, 2) = SITE$(I) ' AT/VS
                RC$(8, 3) = B$(1 - I) ' OPPONENT
                RC$(8, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 2) + QQ(I, 4, I1, 2) > REC!(8, 0) Then
                REC!(8, 0) = QQ(I, 3, I1, 2) + QQ(I, 4, I1, 2) ' FGM
                REC!(8, 1) = 1 ' TIMES DONE
                REC!(8, 2) = 2 ' HALF
                RC$(8, 0) = AA$(I, I1, 0) ' NAME
                RC$(8, 1) = B$(I) ' TEAM
                RC$(8, 2) = SITE$(I) ' AT/VS
                RC$(8, 3) = B$(1 - I) ' OPPONENT
                RC$(8, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 2) > REC!(9, 0) Then
                    REC!(9, 0) = QQ(I, I2, I1, 2) ' FGM
                    REC!(9, 1) = 1 ' TIMES DONE
                    REC!(9, 2) = I2 ' QTR
                    RC$(9, 0) = AA$(I, I1, 0) ' NAME
                    RC$(9, 1) = B$(I) ' TEAM
                    RC$(9, 2) = SITE$(I) ' AT/VS
                    RC$(9, 3) = B$(1 - I) ' OPPONENT
                    RC$(9, 4) = Date$ ' DATE
                End If
            Next I2

            For I2 = 5 To 8
                If QQ(I, I2, I1, 2) > REC!(10, 0) Then
                    REC!(10, 0) = QQ(I, I2, I1, 2) ' FGM
                    REC!(10, 1) = 1 ' TIMES DONE
                    REC!(10, 2) = I2 ' QTR
                    RC$(10, 0) = AA$(I, I1, 0) ' NAME
                    RC$(10, 1) = B$(I) ' TEAM
                    RC$(10, 2) = SITE$(I) ' AT/VS
                    RC$(10, 3) = B$(1 - I) ' OPPONENT
                    RC$(10, 4) = Date$ ' DATE
                End If
            Next I2

            ' FIELD GOAL ATTEMPTS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If A!(I, I1, 7) > REC!(11, 0) Then
                REC!(11, 0) = A!(I, I1, 7) ' FGA
                REC!(11, 1) = 1 ' TIMES DONE
                REC!(11, 2) = 0 ' GAME
                RC$(11, 0) = AA$(I, I1, 0) ' NAME
                RC$(11, 1) = B$(I) ' TEAM
                RC$(11, 2) = SITE$(I) ' AT/VS
                RC$(11, 3) = B$(1 - I) ' OPPONENT
                RC$(11, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 1) + QQ(I, 2, I1, 1) > REC!(12, 0) Then
                REC!(12, 0) = QQ(I, 1, I1, 1) + QQ(I, 2, I1, 1) ' FGA
                REC!(12, 1) = 1 ' TIMES DONE
                REC!(12, 2) = 1 ' HALF
                RC$(12, 0) = AA$(I, I1, 0) ' NAME
                RC$(12, 1) = B$(I) ' TEAM
                RC$(12, 2) = SITE$(I) ' AT/VS
                RC$(12, 3) = B$(1 - I) ' OPPONENT
                RC$(12, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 1) + QQ(I, 4, I1, 1) > REC!(12, 0) Then
                REC!(12, 0) = QQ(I, 3, I1, 1) + QQ(I, 4, I1, 1) ' FGA
                REC!(12, 1) = 1 ' TIMES DONE
                REC!(12, 2) = 2 ' HALF
                RC$(12, 0) = AA$(I, I1, 0) ' NAME
                RC$(12, 1) = B$(I) ' TEAM
                RC$(12, 2) = SITE$(I) ' AT/VS
                RC$(12, 3) = B$(1 - I) ' OPPONENT
                RC$(12, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 1) > REC!(13, 0) Then
                    REC!(13, 0) = QQ(I, I2, I1, 1) ' FGA
                    REC!(13, 1) = 1 ' TIMES DONE
                    REC!(13, 2) = I2 ' QTR
                    RC$(13, 0) = AA$(I, I1, 0) ' NAME
                    RC$(13, 1) = B$(I) ' TEAM
                    RC$(13, 2) = SITE$(I) ' AT/VS
                    RC$(13, 3) = B$(1 - I) ' OPPONENT
                    RC$(13, 4) = Date$ ' DATE
                End If
            Next I2

            For I2 = 5 To 8
                If QQ(I, I2, I1, 1) > REC!(14, 0) Then
                    REC!(14, 0) = QQ(I, I2, I1, 1) ' FGA
                    REC!(14, 1) = 1 ' TIMES DONE
                    REC!(14, 2) = I2 ' QTR
                    RC$(14, 0) = AA$(I, I1, 0) ' NAME
                    RC$(14, 1) = B$(I) ' TEAM
                    RC$(14, 2) = SITE$(I) ' AT/VS
                    RC$(14, 3) = B$(1 - I) ' OPPONENT
                    RC$(14, 4) = Date$ ' DATE
                End If
            Next I2

            ' THREE-POINT FIELD GOALS MOST MADE NO MISSES, MOST ATT NONE MADE
            If W%(I, I1, 0) = W%(I, I1, 1) And W%(I, I1, 0) > REC!(15, 0) Then
                REC!(15, 0) = W%(I, I1, 0) ' MOST 3-FGM
                RC$(15, 0) = AA$(I, I1, 0) ' NAME
                RC$(15, 1) = B$(I) ' TEAM
                RC$(15, 2) = SITE$(I) ' AT/VS
                RC$(15, 3) = B$(1 - I) ' OPPONENT
                RC$(15, 4) = Date$ ' DATE
            End If

            If W%(I, I1, 0) = 0 And W%(I, I1, 1) > REC!(16, 0) Then
                REC!(16, 0) = W%(I, I1, 1) ' 0 3FGM /3FGA
                RC$(16, 0) = AA$(I, I1, 0) ' NAME
                RC$(16, 1) = B$(I) ' TEAM
                RC$(16, 2) = SITE$(I) ' AT/VS
                RC$(16, 3) = B$(1 - I) ' OPPONENT
                RC$(16, 4) = Date$ ' DATE
            End If

            ' 3-FIELD GOALS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If W%(I, I1, 0) > REC!(17, 0) Then
                REC!(17, 0) = W%(I, I1, 0) ' 3FGM
                REC!(17, 1) = 1 ' TIMES DONE
                REC!(17, 2) = 0 ' GAME
                RC$(17, 0) = AA$(I, I1, 0) ' NAME
                RC$(17, 1) = B$(I) ' TEAM
                RC$(17, 2) = SITE$(I) ' AT/VS
                RC$(17, 3) = B$(1 - I) ' OPPONENT
                RC$(17, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 10) + QQ(I, 2, I1, 10) > REC!(18, 0) Then
                REC!(18, 0) = QQ(I, 1, I1, 10) + QQ(I, 2, I1, 10) ' 3FGM
                REC!(18, 1) = 1 ' TIMES DONE
                REC!(18, 2) = 1 ' HALF
                RC$(18, 0) = AA$(I, I1, 0) ' NAME
                RC$(18, 1) = B$(I) ' TEAM
                RC$(18, 2) = SITE$(I) ' AT/VS
                RC$(18, 3) = B$(1 - I) ' OPPONENT
                RC$(18, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 10) + QQ(I, 4, I1, 10) > REC!(18, 0) Then
                REC!(18, 0) = QQ(I, 3, I1, 10) + QQ(I, 4, I1, 10) ' 3FGM
                REC!(18, 1) = 1 ' TIMES DONE
                REC!(18, 2) = 2 ' HALF
                RC$(18, 0) = AA$(I, I1, 0) ' NAME
                RC$(18, 1) = B$(I) ' TEAM
                RC$(18, 2) = SITE$(I) ' AT/VS
                RC$(18, 3) = B$(1 - I) ' OPPONENT
                RC$(18, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 10) > REC!(19, 0) Then
                    REC!(19, 0) = QQ(I, I2, I1, 10) ' 3FGM
                    REC!(19, 1) = 1 ' TIMES DONE
                    REC!(19, 2) = I2 ' QTR
                    RC$(19, 0) = AA$(I, I1, 0) ' NAME
                    RC$(19, 1) = B$(I) ' TEAM
                    RC$(19, 2) = SITE$(I) ' AT/VS
                    RC$(19, 3) = B$(1 - I) ' OPPONENT
                    RC$(19, 4) = Date$ ' DATE
                End If
            Next I2

            For I2 = 5 To 8
                If QQ(I, I2, I1, 10) > REC!(20, 0) Then
                    REC!(20, 0) = QQ(I, I2, I1, 10) ' 3FGM
                    REC!(20, 1) = 1 ' TIMES DONE
                    REC!(20, 2) = I2 ' QTR
                    RC$(20, 0) = AA$(I, I1, 0) ' NAME
                    RC$(20, 1) = B$(I) ' TEAM
                    RC$(20, 2) = SITE$(I) ' AT/VS
                    RC$(20, 3) = B$(1 - I) ' OPPONENT
                    RC$(20, 4) = Date$ ' DATE
                End If
            Next I2

            ' 3-FIELD GOALS ATT IN A GAME, HALF
            If W%(I, I1, 1) > REC!(21, 0) Then
                REC!(21, 0) = W%(I, I1, 1) ' 3FGA
                REC!(21, 1) = 1 ' TIMES DONE
                REC!(21, 2) = 0 ' GAME
                RC$(21, 0) = AA$(I, I1, 0) ' NAME
                RC$(21, 1) = B$(I) ' TEAM
                RC$(21, 2) = SITE$(I) ' AT/VS
                RC$(21, 3) = B$(1 - I) ' OPPONENT
                RC$(21, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 11) + QQ(I, 2, I1, 11) > REC!(22, 0) Then
                REC!(22, 0) = QQ(I, 1, I1, 11) + QQ(I, 2, I1, 11) ' 3FGA
                REC!(22, 1) = 1 ' TIMES DONE
                REC!(22, 2) = 1 ' HALF
                RC$(22, 0) = AA$(I, I1, 0) ' NAME
                RC$(22, 1) = B$(I) ' TEAM
                RC$(22, 2) = SITE$(I) ' AT/VS
                RC$(22, 3) = B$(1 - I) ' OPPONENT
                RC$(22, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 11) + QQ(I, 4, I1, 11) > REC!(22, 0) Then
                REC!(22, 0) = QQ(I, 3, I1, 10) + QQ(I, 4, I1, 10) ' 3FGA
                REC!(22, 1) = 1 ' TIMES DONE
                REC!(22, 2) = 2 ' HALF
                RC$(22, 0) = AA$(I, I1, 0) ' NAME
                RC$(22, 1) = B$(I) ' TEAM
                RC$(22, 2) = SITE$(I) ' AT/VS
                RC$(22, 3) = B$(1 - I) ' OPPONENT
                RC$(22, 4) = Date$ ' DATE
            End If

            ' FREE THROWS -  MOST MADE NO MISSES, MOST ATT NONE MADE
            If A!(I, I1, 9) = A!(I, I1, 10) And A!(I, I1, 10) > REC!(23, 0) Then
                REC!(23, 0) = A!(I, I1, 10) ' MOST FTM
                RC$(23, 0) = AA$(I, I1, 0) ' NAME
                RC$(23, 1) = B$(I) ' TEAM
                RC$(23, 2) = SITE$(I) ' AT/VS
                RC$(23, 3) = B$(1 - I) ' OPPONENT
                RC$(23, 4) = Date$ ' DATE
            End If

            If A!(I, I1, 10) = 0 And A!(I, I1, 9) > REC!(24, 0) Then
                REC!(24, 0) = A!(I, I1, 9) ' 0 FTM /FTA
                RC$(24, 0) = AA$(I, I1, 0) ' NAME
                RC$(24, 1) = B$(I) ' TEAM
                RC$(24, 2) = SITE$(I) ' AT/VS
                RC$(24, 3) = B$(1 - I) ' OPPONENT
                RC$(24, 4) = Date$ ' DATE
            End If

            ' MADE FREE THROWS IN A GAME, HALF, QUARTER
            If A!(I, I1, 10) > REC!(25, 0) Then
                REC!(25, 0) = A!(I, I1, 10) ' FTM
                REC!(25, 1) = 1 ' TIMES DONE
                REC!(25, 2) = 0 ' GAME
                RC$(25, 0) = AA$(I, I1, 0) ' NAME
                RC$(25, 1) = B$(I) ' TEAM
                RC$(25, 2) = SITE$(I) ' AT/VS
                RC$(25, 3) = B$(1 - I) ' OPPONENT
                RC$(25, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 4) + QQ(I, 2, I1, 4) > REC!(26, 0) Then
                REC!(26, 0) = QQ(I, 1, I1, 4) + QQ(I, 2, I1, 4) ' FTM
                REC!(26, 1) = 1 ' TIMES DONE
                REC!(26, 2) = 1 ' HALF
                RC$(26, 0) = AA$(I, I1, 0) ' NAME
                RC$(26, 1) = B$(I) ' TEAM
                RC$(26, 2) = SITE$(I) ' AT/VS
                RC$(26, 3) = B$(1 - I) ' OPPONENT
                RC$(26, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 4) + QQ(I, 4, I1, 4) > REC!(26, 0) Then
                REC!(26, 0) = QQ(I, 3, I1, 4) + QQ(I, 4, I1, 4) ' FTM
                REC!(26, 1) = 1 ' TIMES DONE
                REC!(26, 2) = 2 ' HALF
                RC$(26, 0) = AA$(I, I1, 0) ' NAME
                RC$(26, 1) = B$(I) ' TEAM
                RC$(26, 2) = SITE$(I) ' AT/VS
                RC$(26, 3) = B$(1 - I) ' OPPONENT
                RC$(26, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 4) > REC!(27, 0) Then
                    REC!(27, 0) = QQ(I, I2, I1, 4) ' FTM
                    REC!(27, 1) = 1 ' TIMES DONE
                    REC!(27, 2) = I2 ' QTR
                    RC$(27, 0) = AA$(I, I1, 0) ' NAME
                    RC$(27, 1) = B$(I) ' TEAM
                    RC$(27, 2) = SITE$(I) ' AT/VS
                    RC$(27, 3) = B$(1 - I) ' OPPONENT
                    RC$(27, 4) = Date$ ' DATE
                End If
            Next I2

            ' FREE THROWS ATTEMPTS IN A GAME, HALF, QUARTER
            If A!(I, I1, 9) > REC!(28, 0) Then
                REC!(28, 0) = A!(I, I1, 9) ' FTA
                REC!(28, 1) = 1 ' TIMES DONE
                REC!(28, 2) = 0 ' GAME
                RC$(28, 0) = AA$(I, I1, 0) ' NAME
                RC$(28, 1) = B$(I) ' TEAM
                RC$(28, 2) = SITE$(I) ' AT/VS
                RC$(28, 3) = B$(1 - I) ' OPPONENT
                RC$(28, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 3) + QQ(I, 2, I1, 3) > REC!(29, 0) Then
                REC!(29, 0) = QQ(I, 1, I1, 3) + QQ(I, 2, I1, 3) ' FTA
                REC!(29, 1) = 1 ' TIMES DONE
                REC!(29, 2) = 1 ' HALF
                RC$(29, 0) = AA$(I, I1, 0) ' NAME
                RC$(29, 1) = B$(I) ' TEAM
                RC$(29, 2) = SITE$(I) ' AT/VS
                RC$(29, 3) = B$(1 - I) ' OPPONENT
                RC$(29, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 3) + QQ(I, 4, I1, 3) > REC!(29, 0) Then
                REC!(29, 0) = QQ(I, 3, I1, 3) + QQ(I, 4, I1, 3) ' FTM
                REC!(29, 1) = 1 ' TIMES DONE
                REC!(29, 2) = 2 ' HALF
                RC$(29, 0) = AA$(I, I1, 0) ' NAME
                RC$(29, 1) = B$(I) ' TEAM
                RC$(29, 2) = SITE$(I) ' AT/VS
                RC$(29, 3) = B$(1 - I) ' OPPONENT
                RC$(29, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 3) > REC!(30, 0) Then
                    REC!(30, 0) = QQ(I, I2, I1, 3) ' FTM
                    REC!(30, 1) = 1 ' TIMES DONE
                    REC!(30, 2) = I2 ' QTR
                    RC$(30, 0) = AA$(I, I1, 0) ' NAME
                    RC$(30, 1) = B$(I) ' TEAM
                    RC$(30, 2) = SITE$(I) ' AT/VS
                    RC$(30, 3) = B$(1 - I) ' OPPONENT
                    RC$(30, 4) = Date$ ' DATE
                End If
            Next I2

            ' TOTAL REBOUNDS IN A GAME, HALF, QUARTER
            If A!(I, I1, 11) > REC!(31, 0) Then
                REC!(31, 0) = A!(I, I1, 11) ' FTA
                REC!(31, 1) = 1 ' TIMES DONE
                REC!(31, 2) = 0 ' GAME
                RC$(31, 0) = AA$(I, I1, 0) ' NAME
                RC$(31, 1) = B$(I) ' TEAM
                RC$(31, 2) = SITE$(I) ' AT/VS
                RC$(31, 3) = B$(1 - I) ' OPPONENT
                RC$(31, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 5) + QQ(I, 2, I1, 5) > REC!(32, 0) Then
                REC!(32, 0) = QQ(I, 1, I1, 5) + QQ(I, 2, I1, 5) ' RB
                REC!(32, 1) = 1 ' TIMES DONE
                REC!(32, 2) = 1 ' HALF
                RC$(32, 0) = AA$(I, I1, 0) ' NAME
                RC$(32, 1) = B$(I) ' TEAM
                RC$(32, 2) = SITE$(I) ' AT/VS
                RC$(32, 3) = B$(1 - I) ' OPPONENT
                RC$(32, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 5) + QQ(I, 4, I1, 5) > REC!(32, 0) Then
                REC!(32, 0) = QQ(I, 3, I1, 5) + QQ(I, 4, I1, 5) ' RB
                REC!(32, 1) = 1 ' TIMES DONE
                REC!(32, 2) = 2 ' HALF
                RC$(32, 0) = AA$(I, I1, 0) ' NAME
                RC$(32, 1) = B$(I) ' TEAM
                RC$(32, 2) = SITE$(I) ' AT/VS
                RC$(32, 3) = B$(1 - I) ' OPPONENT
                RC$(32, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 5) > REC!(33, 0) Then
                    REC!(33, 0) = QQ(I, I2, I1, 5) ' RB
                    REC!(33, 1) = 1 ' TIMES DONE
                    REC!(33, 2) = I2 ' QTR
                    RC$(33, 0) = AA$(I, I1, 0) ' NAME
                    RC$(33, 1) = B$(I) ' TEAM
                    RC$(33, 2) = SITE$(I) ' AT/VS
                    RC$(33, 3) = B$(1 - I) ' OPPONENT
                    RC$(33, 4) = Date$ ' DATE
                End If
            Next I2

            ' TOTAL OFFENSIVE REBOUNDS IN A GAME, HALF, QUARTER
            If A!(I, I1, 24) > REC!(34, 0) Then
                REC!(34, 0) = A!(I, I1, 24) ' FTA
                REC!(34, 1) = 1 ' TIMES DONE
                REC!(34, 2) = 0 ' GAME
                RC$(34, 0) = AA$(I, I1, 0) ' NAME
                RC$(34, 1) = B$(I) ' TEAM
                RC$(34, 2) = SITE$(I) ' AT/VS
                RC$(34, 3) = B$(1 - I) ' OPPONENT
                RC$(34, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 12) + QQ(I, 2, I1, 12) > REC!(35, 0) Then
                REC!(35, 0) = QQ(I, 1, I1, 12) + QQ(I, 2, I1, 12) ' RB
                REC!(35, 1) = 1 ' TIMES DONE
                REC!(35, 2) = 1 ' HALF
                RC$(35, 0) = AA$(I, I1, 0) ' NAME
                RC$(35, 1) = B$(I) ' TEAM
                RC$(35, 2) = SITE$(I) ' AT/VS
                RC$(35, 3) = B$(1 - I) ' OPPONENT
                RC$(35, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 12) + QQ(I, 4, I1, 12) > REC!(35, 0) Then
                REC!(35, 0) = QQ(I, 3, I1, 12) + QQ(I, 4, I1, 12) ' RB
                REC!(35, 1) = 1 ' TIMES DONE
                REC!(35, 2) = 2 ' HALF
                RC$(35, 0) = AA$(I, I1, 0) ' NAME
                RC$(35, 1) = B$(I) ' TEAM
                RC$(35, 2) = SITE$(I) ' AT/VS
                RC$(35, 3) = B$(1 - I) ' OPPONENT
                RC$(35, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 12) > REC!(36, 0) Then
                    REC!(36, 0) = QQ(I, I2, I1, 12) ' RB
                    REC!(36, 1) = 1 ' TIMES DONE
                    REC!(36, 2) = I2 ' QTR
                    RC$(36, 0) = AA$(I, I1, 0) ' NAME
                    RC$(36, 1) = B$(I) ' TEAM
                    RC$(36, 2) = SITE$(I) ' AT/VS
                    RC$(36, 3) = B$(1 - I) ' OPPONENT
                    RC$(36, 4) = Date$ ' DATE
                End If
            Next I2

            ' TOTAL DEFENSIVE REBOUNDS IN A GAME, HALF, QUARTER
            If A!(I, I1, 11) - A!(I, I1, 24) > REC!(37, 0) Then
                REC!(37, 0) = A!(I, I1, 11) - A!(I, I1, 24) ' FTA
                REC!(37, 1) = 1 ' TIMES DONE
                REC!(37, 2) = 0 ' GAME
                RC$(37, 0) = AA$(I, I1, 0) ' NAME
                RC$(37, 1) = B$(I) ' TEAM
                RC$(37, 2) = SITE$(I) ' AT/VS
                RC$(37, 3) = B$(1 - I) ' OPPONENT
                RC$(37, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 5) + QQ(I, 2, I1, 5) - (QQ(I, 1, I1, 12) + QQ(I, 2, I1, 12)) > REC!(38, 0) Then
                REC!(38, 0) = QQ(I, 1, I1, 5) + QQ(I, 2, I1, 5) - (QQ(I, 1, I1, 12) + QQ(I, 2, I1, 12)) ' RB
                REC!(38, 1) = 1 ' TIMES DONE
                REC!(38, 2) = 1 ' HALF
                RC$(38, 0) = AA$(I, I1, 0) ' NAME
                RC$(38, 1) = B$(I) ' TEAM
                RC$(38, 2) = SITE$(I) ' AT/VS
                RC$(38, 3) = B$(1 - I) ' OPPONENT
                RC$(38, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 5) + QQ(I, 4, I1, 5) - (QQ(I, 3, I1, 12) + QQ(I, 4, I1, 12)) > REC!(38, 0) Then
                REC!(38, 0) = QQ(I, 3, I1, 5) + QQ(I, 4, I1, 5) - (QQ(I, 3, I1, 12) + QQ(I, 4, I1, 12)) ' RB
                REC!(38, 1) = 1 ' TIMES DONE
                REC!(38, 2) = 2 ' HALF
                RC$(38, 0) = AA$(I, I1, 0) ' NAME
                RC$(38, 1) = B$(I) ' TEAM
                RC$(38, 2) = SITE$(I) ' AT/VS
                RC$(38, 3) = B$(1 - I) ' OPPONENT
                RC$(38, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 5) - QQ(I, I2, I1, 12) > REC!(39, 0) Then
                    REC!(39, 0) = QQ(I, I2, I1, 5) - QQ(I, I2, I1, 12) ' RB
                    REC!(39, 1) = 1 ' TIMES DONE
                    REC!(39, 2) = I2 ' QTR
                    RC$(39, 0) = AA$(I, I1, 0) ' NAME
                    RC$(39, 1) = B$(I) ' TEAM
                    RC$(39, 2) = SITE$(I) ' AT/VS
                    RC$(39, 3) = B$(1 - I) ' OPPONENT
                    RC$(39, 4) = Date$ ' DATE
                End If
            Next I2

            ' ASSISTS IN A GAME, HALF, QUARTER
            If A!(I, I1, 14) > REC!(40, 0) Then
                REC!(40, 0) = A!(I, I1, 14) ' FTA
                REC!(40, 1) = 1 ' TIMES DONE
                REC!(40, 2) = 0 ' GAME
                RC$(40, 0) = AA$(I, I1, 0) ' NAME
                RC$(40, 1) = B$(I) ' TEAM
                RC$(40, 2) = SITE$(I) ' AT/VS
                RC$(40, 3) = B$(1 - I) ' OPPONENT
                RC$(40, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 8) + QQ(I, 2, I1, 8) > REC!(41, 0) Then
                REC!(41, 0) = QQ(I, 1, I1, 8) + QQ(I, 2, I1, 8) ' FTA
                REC!(41, 1) = 1 ' TIMES DONE
                REC!(41, 2) = 1 ' HALF
                RC$(41, 0) = AA$(I, I1, 0) ' NAME
                RC$(41, 1) = B$(I) ' TEAM
                RC$(41, 2) = SITE$(I) ' AT/VS
                RC$(41, 3) = B$(1 - I) ' OPPONENT
                RC$(41, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 8) + QQ(I, 4, I1, 8) > REC!(41, 0) Then
                REC!(41, 0) = QQ(I, 3, I1, 8) + QQ(I, 4, I1, 8) ' FTM
                REC!(41, 1) = 1 ' TIMES DONE
                REC!(41, 2) = 2 ' HALF
                RC$(41, 0) = AA$(I, I1, 0) ' NAME
                RC$(41, 1) = B$(I) ' TEAM
                RC$(41, 2) = SITE$(I) ' AT/VS
                RC$(41, 3) = B$(1 - I) ' OPPONENT
                RC$(41, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 8) > REC!(42, 0) Then
                    REC!(42, 0) = QQ(I, I2, I1, 8) ' FTM
                    REC!(42, 1) = 1 ' TIMES DONE
                    REC!(42, 2) = I2 ' QTR
                    RC$(42, 0) = AA$(I, I1, 0) ' NAME
                    RC$(42, 1) = B$(I) ' TEAM
                    RC$(42, 2) = SITE$(I) ' AT/VS
                    RC$(42, 3) = B$(1 - I) ' OPPONENT
                    RC$(42, 4) = Date$ ' DATE
                End If
            Next I2

            ' STEALS IN A GAME, HALF, QUARTER
            If A!(I, I1, 13) > REC!(43, 0) Then
                REC!(43, 0) = A!(I, I1, 13) ' FTA
                REC!(43, 1) = 1 ' TIMES DONE
                REC!(43, 2) = 0 ' GAME
                RC$(43, 0) = AA$(I, I1, 0) ' NAME
                RC$(43, 1) = B$(I) ' TEAM
                RC$(43, 2) = SITE$(I) ' AT/VS
                RC$(43, 3) = B$(1 - I) ' OPPONENT
                RC$(43, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 7) + QQ(I, 2, I1, 7) > REC!(44, 0) Then
                REC!(44, 0) = QQ(I, 1, I1, 7) + QQ(I, 2, I1, 7) ' FTA
                REC!(44, 1) = 1 ' TIMES DONE
                REC!(44, 2) = 1 ' HALF
                RC$(44, 0) = AA$(I, I1, 0) ' NAME
                RC$(44, 1) = B$(I) ' TEAM
                RC$(44, 2) = SITE$(I) ' AT/VS
                RC$(44, 3) = B$(1 - I) ' OPPONENT
                RC$(44, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 7) + QQ(I, 4, I1, 7) > REC!(44, 0) Then
                REC!(44, 0) = QQ(I, 3, I1, 7) + QQ(I, 4, I1, 7) ' FTM
                REC!(44, 1) = 1 ' TIMES DONE
                REC!(44, 2) = 2 ' HALF
                RC$(44, 0) = AA$(I, I1, 0) ' NAME
                RC$(44, 1) = B$(I) ' TEAM
                RC$(44, 2) = SITE$(I) ' AT/VS
                RC$(44, 3) = B$(1 - I) ' OPPONENT
                RC$(44, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 7) > REC!(45, 0) Then
                    REC!(45, 0) = QQ(I, I2, I1, 7) ' FTM
                    REC!(45, 1) = 1 ' TIMES DONE
                    REC!(45, 2) = I2 ' QTR
                    RC$(45, 0) = AA$(I, I1, 0) ' NAME
                    RC$(45, 1) = B$(I) ' TEAM
                    RC$(45, 2) = SITE$(I) ' AT/VS
                    RC$(45, 3) = B$(1 - I) ' OPPONENT
                    RC$(45, 4) = Date$ ' DATE
                End If
            Next I2

            ' BLOCKS IN A GAME, HALF, QUARTER
            If A!(I, I1, 15) > REC!(46, 0) Then
                REC!(46, 0) = A!(I, I1, 15) ' FTA
                REC!(46, 1) = 1 ' TIMES DONE
                REC!(46, 2) = 0 ' GAME
                RC$(46, 0) = AA$(I, I1, 0) ' NAME
                RC$(46, 1) = B$(I) ' TEAM
                RC$(46, 2) = SITE$(I) ' AT/VS
                RC$(46, 3) = B$(1 - I) ' OPPONENT
                RC$(46, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 9) + QQ(I, 2, I1, 9) > REC!(47, 0) Then
                REC!(47, 0) = QQ(I, 1, I1, 9) + QQ(I, 2, I1, 9) ' FTA
                REC!(47, 1) = 1 ' TIMES DONE
                REC!(47, 2) = 1 ' HALF
                RC$(47, 0) = AA$(I, I1, 0) ' NAME
                RC$(47, 1) = B$(I) ' TEAM
                RC$(47, 2) = SITE$(I) ' AT/VS
                RC$(47, 3) = B$(1 - I) ' OPPONENT
                RC$(47, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 9) + QQ(I, 4, I1, 9) > REC!(47, 0) Then
                REC!(47, 0) = QQ(I, 3, I1, 9) + QQ(I, 4, I1, 9) ' FTM
                REC!(47, 1) = 1 ' TIMES DONE
                REC!(47, 2) = 2 ' HALF
                RC$(47, 0) = AA$(I, I1, 0) ' NAME
                RC$(47, 1) = B$(I) ' TEAM
                RC$(47, 2) = SITE$(I) ' AT/VS
                RC$(47, 3) = B$(1 - I) ' OPPONENT
                RC$(47, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 9) > REC!(48, 0) Then
                    REC!(48, 0) = QQ(I, I2, I1, 9) ' FTM
                    REC!(48, 1) = 1 ' TIMES DONE
                    REC!(48, 2) = I2 ' QTR
                    RC$(48, 0) = AA$(I, I1, 0) ' NAME
                    RC$(48, 1) = B$(I) ' TEAM
                    RC$(48, 2) = SITE$(I) ' AT/VS
                    RC$(48, 3) = B$(1 - I) ' OPPONENT
                    RC$(48, 4) = Date$ ' DATE
                End If
            Next I2
        Next
    Next

    ' MOST OVERTIME PERIODS
    If H - 5 > TREC(0, 0) Then
        TREC(0, 0) = H - 5
        TREC(0, 1) = S!(0, 0)
        TREC(0, 2) = S!(1, 0)
        TRC$(0, 0) = B$(0)
        TRC$(0, 1) = B$(1)
        TRC$(0, 2) = Date$
    End If

    For I = 0 To 1

        If S!(I, 0) > TREC(1, 0) Then
            TREC(1, 0) = S!(I, 0)
            TREC(1, 1) = 1
            TREC(1, 2) = 0
            TRC$(1, 0) = B$(I)
            TRC$(1, 1) = SITE$(I)
            TRC$(1, 2) = B$(1 - I)
            TRC$(1, 3) = Date$
        End If

        If S!(I, 0) < TREC(2, 0) Then
            TREC(2, 0) = S!(I, 0)
            TREC(2, 1) = 1
            TREC(2, 2) = 0
            TRC$(2, 0) = B$(I)
            TRC$(2, 1) = SITE$(I)
            TRC$(2, 2) = B$(1 - I)
            TRC$(2, 3) = Date$
        End If

        If S!(I, 0) + S!(1 - I, 0) > TREC(3, 0) Then
            TREC(3, 0) = S!(I, 0) + S!(1 - I, 0)
            TREC(3, 1) = S!(I, 0)
            TREC(3, 2) = S!(1 - I, 0)
            TRC$(3, 0) = B$(I)
            TRC$(3, 1) = SITE$(I)
            TRC$(3, 2) = B$(1 - I)
            TRC$(3, 3) = Date$
        End If

        If S!(I, 0) + S!(1 - I, 0) < TREC(4, 0) Then
            TREC(4, 0) = S!(I, 0) + S!(1 - I, 0)
            TREC(4, 1) = S!(I, 0)
            TREC(4, 2) = S!(1 - I, 0)
            TRC$(4, 0) = B$(I)
            TRC$(4, 1) = SITE$(I)
            TRC$(4, 2) = B$(1 - I)
            TRC$(4, 3) = Date$
        End If

        If S!(I, 1) + S!(I, 2) > TREC(6, 0) Then
            TREC(6, 0) = S!(I, 1) + S!(I, 2)
            TREC(6, 1) = 1
            TREC(6, 2) = 0
            TRC$(6, 0) = B$(I)
            TRC$(6, 1) = SITE$(I)
            TRC$(6, 2) = B$(1 - I)
            TRC$(6, 3) = Date$
        End If

        If S!(I, 1) + S!(I, 2) < TREC(7, 0) Then
            TREC(7, 0) = S!(I, 1) + S!(I, 2)
            TREC(7, 1) = 1
            TREC(7, 2) = 0
            TRC$(7, 0) = B$(I)
            TRC$(7, 1) = SITE$(I)
            TRC$(7, 2) = B$(1 - I)
            TRC$(7, 3) = Date$
        End If

        If S!(I, 1) + S!(I, 2) + S!(1 - I, 1) + S!(1 - I, 2) > TREC(8, 0) Then
            TREC(8, 0) = S!(I, 1) + S!(1 - I, 1) + S!(I, 2) + S!(1 - I, 2)
            TREC(8, 1) = S!(I, 1) + S!(I, 2)
            TREC(8, 2) = S!(1 - I, 1) + S!(1 - I, 2)
            TRC$(8, 0) = B$(I)
            TRC$(8, 1) = SITE$(I)
            TRC$(8, 2) = B$(1 - I)
            TRC$(8, 3) = Date$
        End If

        If S!(I, 1) + S!(I, 2) + S!(1 - I, 1) + S!(1 - I, 2) < TREC(9, 0) Then
            TREC(9, 0) = S!(I, 1) + S!(1 - I, 1) + S!(I, 2) + S!(1 - I, 2)
            TREC(9, 1) = S!(I, 1) + S!(I, 2)
            TREC(9, 2) = S!(1 - I, 1) + S!(1 - I, 2)
            TRC$(9, 0) = B$(I)
            TRC$(9, 1) = SITE$(I)
            TRC$(9, 2) = B$(1 - I)
            TRC$(9, 3) = Date$
        End If

        If S!(I, 3) + S!(I, 4) > TREC(10, 0) Then
            TREC(10, 0) = S!(I, 3) + S!(I, 4)
            TREC(10, 1) = 1
            TREC(10, 2) = 0
            TRC$(10, 0) = B$(I)
            TRC$(10, 1) = SITE$(I)
            TRC$(10, 2) = B$(1 - I)
            TRC$(10, 3) = Date$
        End If

        If S!(I, 3) + S!(I, 4) < TREC(11, 0) Then
            TREC(11, 0) = S!(I, 3) + S!(I, 4)
            TREC(11, 1) = 1
            TREC(11, 2) = 0
            TRC$(11, 0) = B$(I)
            TRC$(11, 1) = SITE$(I)
            TRC$(11, 2) = B$(1 - I)
            TRC$(11, 3) = Date$
        End If

        If S!(I, 3) + S!(I, 4) + S!(1 - I, 3) + S!(1 - I, 4) > TREC(12, 0) Then
            TREC(12, 0) = S!(I, 3) + S!(1 - I, 3) + S!(I, 4) + S!(1 - I, 4)
            TREC(12, 1) = S!(I, 3) + S!(I, 4)
            TREC(12, 2) = S!(1 - I, 3) + S!(1 - I, 4)
            TRC$(12, 0) = B$(I)
            TRC$(12, 1) = SITE$(I)
            TRC$(12, 2) = B$(1 - I)
            TRC$(12, 3) = Date$
        End If

        If S!(I, 3) + S!(I, 4) + S!(1 - I, 3) + S!(1 - I, 4) < TREC(13, 0) Then
            TREC(13, 0) = S!(I, 3) + S!(1 - I, 3) + S!(I, 4) + S!(1 - I, 4)
            TREC(13, 1) = S!(I, 3) + S!(I, 4)
            TREC(13, 2) = S!(1 - I, 3) + S!(1 - I, 4)
            TRC$(13, 0) = B$(I)
            TRC$(13, 1) = SITE$(I)
            TRC$(13, 2) = B$(1 - I)
            TRC$(13, 3) = Date$
        End If

        If S!(I, 1) > TREC(14, 0) Then
            TREC(14, 0) = S!(I, 1)
            TREC(14, 1) = 1
            TREC(14, 2) = 0
            TRC$(14, 0) = B$(I)
            TRC$(14, 1) = SITE$(I)
            TRC$(14, 2) = B$(1 - I)
            TRC$(14, 3) = Date$
        End If

        If S!(I, 1) < TREC(15, 0) Then
            TREC(15, 0) = S!(I, 1)
            TREC(15, 1) = 1
            TREC(15, 2) = 0
            TRC$(15, 0) = B$(I)
            TRC$(15, 1) = SITE$(I)
            TRC$(15, 2) = B$(1 - I)
            TRC$(15, 3) = Date$
        End If

        If S!(I, 1) + S!(1 - I, 1) > TREC(16, 0) Then
            TREC(16, 0) = S!(I, 1) + S!(1 - I, 1)
            TREC(16, 1) = S!(I, 1)
            TREC(16, 2) = S!(1 - I, 1)
            TRC$(16, 0) = B$(I)
            TRC$(16, 1) = SITE$(I)
            TRC$(16, 2) = B$(1 - I)
            TRC$(16, 3) = Date$
        End If

        If S!(I, 1) + S!(1 - I, 1) < TREC(17, 0) Then
            TREC(17, 0) = S!(I, 1) + S!(1 - I, 1)
            TREC(17, 1) = S!(I, 1)
            TREC(17, 2) = S!(1 - I, 1)
            TRC$(17, 0) = B$(I)
            TRC$(17, 1) = SITE$(I)
            TRC$(17, 2) = B$(1 - I)
            TRC$(17, 3) = Date$
        End If

        If S!(I, 2) > TREC(18, 0) Then
            TREC(18, 0) = S!(I, 2)
            TREC(18, 1) = 1
            TREC(18, 2) = 0
            TRC$(18, 0) = B$(I)
            TRC$(18, 1) = SITE$(I)
            TRC$(18, 2) = B$(1 - I)
            TRC$(18, 3) = Date$
        End If

        If S!(I, 2) < TREC(19, 0) Then
            TREC(19, 0) = S!(I, 2)
            TREC(19, 1) = 1
            TREC(19, 2) = 0
            TRC$(19, 0) = B$(I)
            TRC$(19, 1) = SITE$(I)
            TRC$(19, 2) = B$(1 - I)
            TRC$(19, 3) = Date$
        End If

        If S!(I, 2) + S!(1 - I, 2) > TREC(20, 0) Then
            TREC(20, 0) = S!(I, 2) + S!(1 - I, 2)
            TREC(20, 1) = S!(I, 2)
            TREC(20, 2) = S!(1 - I, 2)
            TRC$(20, 0) = B$(I)
            TRC$(20, 1) = SITE$(I)
            TRC$(20, 2) = B$(1 - I)
            TRC$(20, 3) = Date$
        End If

        If S!(I, 2) + S!(1 - I, 2) < TREC(21, 0) Then
            TREC(21, 0) = S!(I, 2) + S!(1 - I, 2)
            TREC(21, 1) = S!(I, 2)
            TREC(21, 2) = S!(1 - I, 2)
            TRC$(21, 0) = B$(I)
            TRC$(21, 1) = SITE$(I)
            TRC$(21, 2) = B$(1 - I)
            TRC$(21, 3) = Date$
        End If

        If S!(I, 3) > TREC(22, 0) Then
            TREC(22, 0) = S!(I, 3)
            TREC(22, 1) = 1
            TREC(22, 2) = 0
            TRC$(22, 0) = B$(I)
            TRC$(22, 1) = SITE$(I)
            TRC$(22, 2) = B$(1 - I)
            TRC$(22, 3) = Date$
        End If

        If S!(I, 3) < TREC(23, 0) Then
            TREC(23, 0) = S!(I, 3)
            TREC(23, 1) = 1
            TREC(23, 2) = 0
            TRC$(23, 0) = B$(I)
            TRC$(23, 1) = SITE$(I)
            TRC$(23, 2) = B$(1 - I)
            TRC$(23, 3) = Date$
        End If

        If S!(I, 3) + S!(1 - I, 3) > TREC(24, 0) Then
            TREC(24, 0) = S!(I, 3) + S!(1 - I, 3)
            TREC(24, 1) = S!(I, 3)
            TREC(24, 2) = S!(1 - I, 3)
            TRC$(24, 0) = B$(I)
            TRC$(24, 1) = SITE$(I)
            TRC$(24, 2) = B$(1 - I)
            TRC$(24, 3) = Date$
        End If

        If S!(I, 3) + S!(1 - I, 3) < TREC(25, 0) Then
            TREC(25, 0) = S!(I, 3) + S!(1 - I, 3)
            TREC(25, 1) = S!(I, 3)
            TREC(25, 2) = S!(1 - I, 3)
            TRC$(25, 0) = B$(I)
            TRC$(25, 1) = SITE$(I)
            TRC$(25, 2) = B$(1 - I)
            TRC$(25, 3) = Date$
        End If

        If S!(I, 4) > TREC(26, 0) Then
            TREC(26, 0) = S!(I, 4)
            TREC(26, 1) = 1
            TREC(26, 2) = 0
            TRC$(26, 0) = B$(I)
            TRC$(26, 1) = SITE$(I)
            TRC$(26, 2) = B$(1 - I)
            TRC$(26, 3) = Date$
        End If

        If S!(I, 4) < TREC(27, 0) Then
            TREC(27, 0) = S!(I, 4)
            TREC(27, 1) = 1
            TREC(27, 2) = 0
            TRC$(27, 0) = B$(I)
            TRC$(27, 1) = SITE$(I)
            TRC$(27, 2) = B$(1 - I)
            TRC$(27, 3) = Date$
        End If

        If S!(I, 4) + S!(1 - I, 4) > TREC(28, 0) Then
            TREC(28, 0) = S!(I, 4) + S!(1 - I, 4)
            TREC(28, 1) = S!(I, 4)
            TREC(28, 2) = S!(1 - I, 4)
            TRC$(28, 0) = B$(I)
            TRC$(28, 1) = SITE$(I)
            TRC$(28, 2) = B$(1 - I)
            TRC$(28, 3) = Date$
        End If

        If S!(I, 4) + S!(1 - I, 4) < TREC(29, 0) Then
            TREC(29, 0) = S!(I, 4) + S!(1 - I, 4)
            TREC(29, 1) = S!(I, 4)
            TREC(29, 2) = S!(1 - I, 4)
            TRC$(29, 0) = B$(I)
            TRC$(29, 1) = SITE$(I)
            TRC$(29, 2) = B$(1 - I)
            TRC$(29, 3) = Date$
        End If

        ' TEAM FIELD GOAL PERCENTAGES
        For I1 = 0 To 14: V8(I, I1) = 0: V8(1 - I, I1) = 0: Next
        For I2 = 0 To 1
            For I1 = 0 To 14
                V8(I2, 0) = V8(I2, 0) + A!(I2, I1, 8)
                V8(I2, 1) = V8(I2, 1) + A!(I2, I1, 7)
                V8(I2, 2) = V8(I2, 2) + A!(I2, I1, 10)
                V8(I2, 3) = V8(I2, 3) + A!(I2, I1, 9)
                V8(I2, 4) = V8(I2, 4) + A!(I2, I1, 11)
                V8(I2, 5) = V8(I2, 5) + (A!(I2, I1, 8) * 2) + A!(I2, I1, 10) + W%(I2, I1, 0)
                V8(I2, 13) = V8(I2, 13) + W%(I2, I1, 0)
                V8(I2, 14) = V8(I2, 14) + W%(I2, I1, 1)
                V8(I2, 6) = TF(I2)
                V8(I2, 7) = V8(I2, 7) + A!(I2, I1, 14)
                V8(I2, 8) = V8(I2, 8) + A!(I2, I1, 15)
                V8(I2, 9) = V8(I2, 9) + A!(I2, I1, 13)
                V8(I2, 11) = V8(I2, 11) + A!(I2, I1, 24)
                V8(I2, 12) = V8(I2, 12) + A!(I2, I1, 23)
                V8(I2, 10) = V8(I2, 10) + ZZ(I2, I1)
            Next
        Next

        If V8(I, 0) / V8(I, 1) > TREC(30, 0) Then
            TREC(30, 0) = V8(I, 0) / V8(I, 1)
            TREC(30, 1) = V8(I, 0)
            TREC(30, 2) = V8(I, 1)
            TRC$(30, 0) = B$(I)
            TRC$(30, 1) = SITE$(I)
            TRC$(30, 2) = B$(1 - I)
            TRC$(30, 3) = Date$
        End If

        If V8(I, 0) / V8(I, 1) < TREC(31, 0) Then
            TREC(31, 0) = V8(I, 0) / V8(I, 1)
            TREC(31, 1) = V8(I, 0)
            TREC(31, 2) = V8(I, 1)
            TRC$(31, 0) = B$(I)
            TRC$(31, 1) = SITE$(I)
            TRC$(31, 2) = B$(1 - I)
            TRC$(31, 3) = Date$
        End If

        If (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1)) > TREC(32, 0) Then
            TREC(32, 0) = (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1))
            TREC(32, 1) = V8(I, 0) / V8(I, 1)
            TREC(32, 2) = V8(1 - I, 0) / V8(1 - I, 1)
            TRC$(32, 0) = B$(I)
            TRC$(32, 1) = SITE$(I)
            TRC$(32, 2) = B$(1 - I)
            TRC$(32, 3) = Date$
        End If

        If (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1)) < TREC(33, 0) Then
            TREC(33, 0) = (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1))
            TREC(33, 1) = V8(I, 0) / V8(I, 1)
            TREC(33, 2) = V8(1 - I, 0) / V8(1 - I, 1)
            TRC$(33, 0) = B$(I)
            TRC$(33, 1) = SITE$(I)
            TRC$(33, 2) = B$(1 - I)
            TRC$(33, 3) = Date$
        End If

        ' TEAM FIELD GOALS MADE
        If V8(I, 0) > TREC(34, 0) Then
            TREC(34, 0) = V8(I, 0)
            TREC(34, 1) = 0
            TREC(34, 2) = 0
            TRC$(34, 0) = B$(I)
            TRC$(34, 1) = SITE$(I)
            TRC$(34, 2) = B$(1 - I)
            TRC$(34, 3) = Date$
        End If

        If V8(I, 0) < TREC(35, 0) Then
            TREC(35, 0) = V8(I, 0)
            TREC(35, 1) = 0
            TREC(35, 2) = 0
            TRC$(35, 0) = B$(I)
            TRC$(35, 1) = SITE$(I)
            TRC$(35, 2) = B$(1 - I)
            TRC$(35, 3) = Date$
        End If

        If V8(I, 0) + V8(1 - I, 0) > TREC(36, 0) Then
            TREC(36, 0) = V8(I, 0) + V8(1 - I, 0)
            TREC(36, 1) = V8(I, 0)
            TREC(36, 2) = V8(1 - I, 0)
            TRC$(36, 0) = B$(I)
            TRC$(36, 1) = SITE$(I)
            TRC$(36, 2) = B$(1 - I)
            TRC$(36, 3) = Date$
        End If

        If V8(I, 0) + V8(1 - I, 0) < TREC(37, 0) Then
            TREC(37, 0) = V8(I, 0) + V8(1 - I, 0)
            TREC(37, 1) = V8(I, 0)
            TREC(37, 2) = V8(1 - I, 0)
            TRC$(37, 0) = B$(I)
            TRC$(37, 1) = SITE$(I)
            TRC$(37, 2) = B$(1 - I)
            TRC$(37, 3) = Date$
        End If

        If QR(I, 1, 0) + QR(I, 2, 0) > TREC(38, 0) Then
            TREC(38, 0) = QR(I, 1, 0) + QR(I, 2, 0)
            TREC(38, 1) = 1
            TREC(38, 2) = 0
            TRC$(38, 0) = B$(I)
            TRC$(38, 1) = SITE$(I)
            TRC$(38, 2) = B$(1 - I)
            TRC$(38, 3) = Date$
        End If

        If QR(I, 3, 0) + QR(I, 4, 0) > TREC(38, 0) Then
            TREC(38, 0) = QR(I, 3, 0) + QR(I, 4, 0)
            TREC(38, 1) = 2
            TREC(38, 2) = 0
            TRC$(38, 0) = B$(I)
            TRC$(38, 1) = SITE$(I)
            TRC$(38, 2) = B$(1 - I)
            TRC$(38, 3) = Date$
        End If

        If QR(I, 1, 0) + QR(I, 2, 0) + QR(1 - I, 1, 0) + QR(1 - I, 2, 0) > TREC(39, 0) Then
            TREC(39, 0) = QR(I, 1, 0) + QR(I, 2, 0) + QR(1 - I, 1, 0) + QR(1 - I, 2, 0)
            TREC(39, 1) = 1
            TREC(39, 2) = 0
            TRC$(39, 0) = B$(I)
            TRC$(39, 1) = SITE$(I)
            TRC$(39, 2) = B$(1 - I)
            TRC$(39, 3) = Date$
        End If

        If QR(I, 3, 0) + QR(I, 4, 0) + QR(1 - I, 3, 0) + QR(1 - I, 4, 0) > TREC(39, 0) Then
            TREC(39, 0) = QR(I, 3, 0) + QR(I, 4, 0) + QR(1 - I, 3, 0) + QR(1 - I, 4, 0)
            TREC(39, 1) = 2
            TREC(39, 2) = 0
            TRC$(39, 0) = B$(I)
            TRC$(39, 1) = SITE$(I)
            TRC$(39, 2) = B$(1 - I)
            TRC$(39, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 0) > TREC(40, 0) Then
                TREC(40, 0) = QR(I, I2, 0)
                TREC(40, 1) = I2
                TREC(40, 2) = 0
                TRC$(40, 0) = B$(I)
                TRC$(40, 1) = SITE$(I)
                TRC$(40, 2) = B$(1 - I)
                TRC$(40, 3) = Date$
            End If

            If QR(I, I2, 0) + QR(1 - I, I2, 0) > TREC(41, 0) Then
                TREC(41, 0) = QR(I, I2, 0) + QR(1 - I, I2, 0)
                TREC(41, 1) = I2
                TREC(41, 2) = QR(I, I2, 0)
                TRC$(41, 0) = B$(I)
                TRC$(41, 1) = SITE$(I)
                TRC$(41, 2) = B$(1 - I)
                TRC$(41, 3) = Date$
            End If
        Next

        'TEAM FIELD GOAL ATTEMPTS
        If V8(I, 1) > TREC(42, 0) Then
            TREC(42, 0) = V8(I, 1)
            TREC(42, 1) = 0
            TREC(42, 2) = 0
            TRC$(42, 0) = B$(I)
            TRC$(42, 1) = SITE$(I)
            TRC$(42, 2) = B$(1 - I)
            TRC$(42, 3) = Date$
        End If

        If V8(I, 1) < TREC(43, 0) Then
            TREC(43, 0) = V8(I, 1)
            TREC(43, 1) = 0
            TREC(43, 2) = 0
            TRC$(43, 0) = B$(I)
            TRC$(43, 1) = SITE$(I)
            TRC$(43, 2) = B$(1 - I)
            TRC$(43, 3) = Date$
        End If

        If V8(I, 1) + V8(1 - I, 1) > TREC(44, 0) Then
            TREC(44, 0) = V8(I, 1) + V8(1 - I, 1)
            TREC(44, 1) = V8(I, 1)
            TREC(44, 2) = V8(1 - I, 1)
            TRC$(44, 0) = B$(I)
            TRC$(44, 1) = SITE$(I)
            TRC$(44, 2) = B$(1 - I)
            TRC$(44, 3) = Date$
        End If

        If V8(I, 1) + V8(1 - I, 1) < TREC(45, 0) Then
            TREC(45, 0) = V8(I, 1) + V8(1 - I, 1)
            TREC(45, 1) = V8(I, 1)
            TREC(45, 2) = V8(1 - I, 1)
            TRC$(45, 0) = B$(I)
            TRC$(45, 1) = SITE$(I)
            TRC$(45, 2) = B$(1 - I)
            TRC$(45, 3) = Date$
        End If

        If QR(I, 1, 1) + QR(I, 2, 1) > TREC(46, 0) Then
            TREC(46, 0) = QR(I, 1, 1) + QR(I, 2, 1)
            TREC(46, 1) = 1
            TREC(46, 2) = 0
            TRC$(46, 0) = B$(I)
            TRC$(46, 1) = SITE$(I)
            TRC$(46, 2) = B$(1 - I)
            TRC$(46, 3) = Date$
        End If

        If QR(I, 3, 1) + QR(I, 4, 1) > TREC(46, 0) Then
            TREC(46, 0) = QR(I, 3, 1) + QR(I, 4, 1)
            TREC(46, 1) = 2
            TREC(46, 2) = 0
            TRC$(46, 0) = B$(I)
            TRC$(46, 1) = SITE$(I)
            TRC$(46, 2) = B$(1 - I)
            TRC$(46, 3) = Date$
        End If

        If QR(I, 1, 1) + QR(I, 2, 1) + QR(1 - I, 1, 1) + QR(1 - I, 2, 1) > TREC(47, 0) Then
            TREC(47, 0) = QR(I, 1, 1) + QR(I, 2, 1) + QR(1 - I, 1, 1) + QR(1 - I, 2, 1)
            TREC(47, 1) = 1
            TREC(47, 2) = QR(I, 1, 1) + QR(I, 2, 1)
            TRC$(47, 0) = B$(I)
            TRC$(47, 1) = SITE$(I)
            TRC$(47, 2) = B$(1 - I)
            TRC$(47, 3) = Date$
        End If

        If QR(I, 3, 1) + QR(I, 4, 1) + QR(1 - I, 3, 1) + QR(1 - I, 4, 1) > TREC(47, 0) Then
            TREC(47, 0) = QR(I, 3, 1) + QR(I, 4, 1) + QR(1 - I, 3, 1) + QR(1 - I, 4, 1)
            TREC(47, 1) = 1
            TREC(47, 2) = QR(I, 3, 1) + QR(I, 4, 1)
            TRC$(47, 0) = B$(I)
            TRC$(47, 1) = SITE$(I)
            TRC$(47, 2) = B$(1 - I)
            TRC$(47, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 1) > TREC(48, 0) Then
                TREC(48, 0) = QR(I, I2, 1)
                TREC(48, 1) = I2
                TREC(48, 2) = 0
                TRC$(48, 0) = B$(I)
                TRC$(48, 1) = SITE$(I)
                TRC$(48, 2) = B$(1 - I)
                TRC$(48, 3) = Date$
            End If

            If QR(I, I2, 1) + QR(1 - I, I2, 1) > TREC(49, 0) Then
                TREC(49, 0) = QR(I, I2, 1) + QR(1 - I, I2, 1)
                TREC(49, 1) = I2
                TREC(49, 2) = QR(I, I2, 1)
                TRC$(49, 0) = B$(I)
                TRC$(49, 1) = SITE$(I)
                TRC$(49, 2) = B$(1 - I)
                TRC$(49, 3) = Date$
            End If
        Next

        'TEAM FIELD GOAL ATTEMPTS
        If V8(I, 13) = V8(I, 14) And V8(I, 14) > TREC(50, 0) Then
            TREC(50, 0) = V8(I, 13)
            TREC(50, 1) = 0
            TREC(50, 2) = 0
            TRC$(50, 0) = B$(I)
            TRC$(50, 1) = SITE$(I)
            TRC$(50, 2) = B$(1 - I)
            TRC$(50, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS MOST MADE
        If V8(I, 13) = V8(I, 14) And V8(1 - I, 13) = V8(1 - I, 14) And V8(I, 14) + V8(1 - I, 14) > TREC(51, 0) Then
            TREC(51, 0) = V8(I, 13)
            TREC(51, 1) = V8(1 - I, 13)
            TREC(51, 2) = 0
            TRC$(51, 0) = B$(I)
            TRC$(51, 1) = SITE$(I)
            TRC$(51, 2) = B$(1 - I)
            TRC$(51, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) = 0 And V8(I, 14) > TREC(52, 0) Then
            TREC(52, 0) = V8(I, 14)
            TREC(52, 1) = 0
            TREC(52, 2) = 0
            TRC$(52, 0) = B$(I)
            TRC$(52, 1) = SITE$(I)
            TRC$(52, 2) = B$(1 - I)
            TRC$(52, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) > TREC(53, 0) Then
            TREC(53, 0) = V8(I, 13)
            TREC(53, 1) = 0
            TREC(53, 2) = 0
            TRC$(53, 0) = B$(I)
            TRC$(53, 1) = SITE$(I)
            TRC$(53, 2) = B$(1 - I)
            TRC$(53, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) + V8(1 - I, 13) > TREC(54, 0) Then
            TREC(54, 0) = V8(I, 13) + V8(1 - I, 13)
            TREC(54, 1) = V8(I, 13)
            TREC(54, 2) = V8(1 - I, 13)
            TRC$(54, 0) = B$(I)
            TRC$(54, 1) = SITE$(I)
            TRC$(54, 2) = B$(1 - I)
            TRC$(54, 3) = Date$
        End If

        If QR(I, 1, 13) + QR(I, 2, 13) > TREC(55, 0) Then
            TREC(55, 0) = QR(I, 1, 13) + QR(I, 2, 13)
            TREC(55, 1) = 1
            TREC(55, 2) = 0
            TRC$(55, 0) = B$(I)
            TRC$(55, 1) = SITE$(I)
            TRC$(55, 2) = B$(1 - I)
            TRC$(55, 3) = Date$
        End If

        If QR(I, 3, 13) + QR(I, 4, 13) > TREC(55, 0) Then
            TREC(55, 0) = QR(I, 3, 13) + QR(I, 4, 13)
            TREC(55, 1) = 2
            TREC(55, 2) = 0
            TRC$(55, 0) = B$(I)
            TRC$(55, 1) = SITE$(I)
            TRC$(55, 2) = B$(1 - I)
            TRC$(55, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 13) > TREC(56, 0) Then
                TREC(56, 0) = QR(I, I2, 13)
                TREC(56, 1) = I2
                TREC(56, 2) = 0
                TRC$(56, 0) = B$(I)
                TRC$(56, 1) = SITE$(I)
                TRC$(56, 2) = B$(1 - I)
                TRC$(56, 3) = Date$
            End If
        Next
        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 14) > TREC(57, 0) Then
            TREC(57, 0) = V8(I, 14)
            TREC(57, 1) = 0
            TREC(57, 2) = 0
            TRC$(57, 0) = B$(I)
            TRC$(57, 1) = SITE$(I)
            TRC$(57, 2) = B$(1 - I)
            TRC$(57, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 14) + V8(1 - I, 14) > TREC(58, 0) Then
            TREC(58, 0) = V8(I, 14) + V8(1 - I, 14)
            TREC(58, 1) = V8(I, 14)
            TREC(58, 2) = V8(1 - I, 14)
            TRC$(58, 0) = B$(I)
            TRC$(58, 1) = SITE$(I)
            TRC$(58, 2) = B$(1 - I)
            TRC$(58, 3) = Date$
        End If

        If QR(I, 1, 14) + QR(I, 2, 14) > TREC(59, 0) Then
            TREC(59, 0) = QR(I, 1, 14) + QR(I, 2, 14)
            TREC(59, 1) = 1
            TREC(59, 2) = 0
            TRC$(59, 0) = B$(I)
            TRC$(59, 1) = SITE$(I)
            TRC$(59, 2) = B$(1 - I)
            TRC$(59, 3) = Date$
        End If

        If QR(I, 3, 14) + QR(I, 4, 14) > TREC(59, 0) Then
            TREC(59, 0) = QR(I, 3, 14) + QR(I, 4, 14)
            TREC(59, 1) = 2
            TREC(59, 2) = 0
            TRC$(59, 0) = B$(I)
            TRC$(59, 1) = SITE$(I)
            TRC$(59, 2) = B$(1 - I)
            TRC$(59, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS NO MISS
        If V8(I, 2) = V8(I, 3) And V8(I, 2) > TREC(60, 0) Then
            TREC(60, 0) = V8(I, 2)
            TREC(60, 1) = 0
            TREC(60, 2) = 0
            TRC$(60, 0) = B$(I)
            TRC$(60, 1) = SITE$(I)
            TRC$(60, 2) = B$(1 - I)
            TRC$(60, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS LOW %
        If V8(I, 2) / V8(I, 3) < TREC(61, 0) Then
            TREC(61, 0) = V8(I, 2) / V8(I, 3)
            TREC(61, 1) = V8(I, 2)
            TREC(61, 2) = V8(I, 3)
            TRC$(61, 0) = B$(I)
            TRC$(61, 1) = SITE$(I)
            TRC$(61, 2) = B$(1 - I)
            TRC$(61, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS COMBINED HIGH %
        If (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3)) > TREC(62, 0) Then
            TREC(62, 0) = (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3))
            TREC(62, 1) = V8(I, 2) / V8(I, 3)
            TREC(62, 2) = V8(1 - I, 2) / V8(1 - I, 3)
            TRC$(62, 0) = B$(I)
            TRC$(62, 1) = SITE$(I)
            TRC$(62, 2) = B$(1 - I)
            TRC$(62, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS COMBINED LOW %
        If (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3)) < TREC(63, 0) Then
            TREC(63, 0) = (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3))
            TREC(63, 1) = V8(I, 2) / V8(I, 3)
            TREC(63, 2) = V8(1 - I, 2) / V8(1 - I, 3)
            TRC$(63, 0) = B$(I)
            TRC$(63, 1) = SITE$(I)
            TRC$(63, 2) = B$(1 - I)
            TRC$(63, 3) = Date$
        End If

        ' TEAM FREE THROWS MADE
        If V8(I, 2) > TREC(64, 0) Then
            TREC(64, 0) = V8(I, 2)
            TREC(64, 1) = 0
            TREC(64, 2) = 0
            TRC$(64, 0) = B$(I)
            TRC$(64, 1) = SITE$(I)
            TRC$(64, 2) = B$(1 - I)
            TRC$(64, 3) = Date$
        End If

        If V8(I, 2) < TREC(65, 0) Then
            TREC(65, 0) = V8(I, 2)
            TREC(65, 1) = 0
            TREC(65, 2) = 0
            TRC$(65, 0) = B$(I)
            TRC$(65, 1) = SITE$(I)
            TRC$(65, 2) = B$(1 - I)
            TRC$(65, 3) = Date$
        End If

        If V8(I, 2) + V8(1 - I, 2) > TREC(66, 0) Then
            TREC(66, 0) = V8(I, 2) + V8(1 - I, 2)
            TREC(66, 1) = V8(I, 2)
            TREC(66, 2) = V8(1 - I, 2)
            TRC$(66, 0) = B$(I)
            TRC$(66, 1) = SITE$(I)
            TRC$(66, 2) = B$(1 - I)
            TRC$(66, 3) = Date$
        End If

        If V8(I, 2) + V8(1 - I, 2) < TREC(67, 0) Then
            TREC(67, 0) = V8(I, 2) + V8(1 - I, 2)
            TREC(67, 1) = V8(I, 2)
            TREC(67, 2) = V8(1 - I, 2)
            TRC$(67, 0) = B$(I)
            TRC$(67, 1) = SITE$(I)
            TRC$(67, 2) = B$(1 - I)
            TRC$(67, 3) = Date$
        End If

        If QR(I, 1, 2) + QR(I, 2, 2) > TREC(68, 0) Then
            TREC(68, 0) = QR(I, 1, 2) + QR(I, 2, 2)
            TREC(68, 1) = 1
            TREC(68, 2) = 0
            TRC$(68, 0) = B$(I)
            TRC$(68, 1) = SITE$(I)
            TRC$(68, 2) = B$(1 - I)
            TRC$(68, 3) = Date$
        End If

        If QR(I, 3, 2) + QR(I, 4, 2) > TREC(68, 0) Then
            TREC(68, 0) = QR(I, 3, 2) + QR(I, 4, 2)
            TREC(68, 1) = 2
            TREC(68, 2) = 0
            TRC$(68, 0) = B$(I)
            TRC$(68, 1) = SITE$(I)
            TRC$(68, 2) = B$(1 - I)
            TRC$(68, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 2) > TREC(70, 0) Then
                TREC(70, 0) = QR(I, I2, 2)
                TREC(70, 1) = I2
                TREC(70, 2) = 0
                TRC$(70, 0) = B$(I)
                TRC$(70, 1) = SITE$(I)
                TRC$(70, 2) = B$(1 - I)
                TRC$(70, 3) = Date$
            End If

        Next

        ' TEAM FREE THROWS ATTEMPTED
        If V8(I, 3) > TREC(72, 0) Then
            TREC(72, 0) = V8(I, 3)
            TREC(72, 1) = 0
            TREC(72, 2) = 0
            TRC$(72, 0) = B$(I)
            TRC$(72, 1) = SITE$(I)
            TRC$(72, 2) = B$(1 - I)
            TRC$(72, 3) = Date$
        End If

        If V8(I, 3) < TREC(73, 0) Then
            TREC(73, 0) = V8(I, 3)
            TREC(73, 1) = 0
            TREC(73, 2) = 0
            TRC$(73, 0) = B$(I)
            TRC$(73, 1) = SITE$(I)
            TRC$(73, 2) = B$(1 - I)
            TRC$(73, 3) = Date$
        End If

        If V8(I, 3) + V8(1 - I, 3) > TREC(74, 0) Then
            TREC(74, 0) = V8(I, 3) + V8(1 - I, 3)
            TREC(74, 1) = V8(I, 3)
            TREC(74, 2) = V8(1 - I, 3)
            TRC$(74, 0) = B$(I)
            TRC$(74, 1) = SITE$(I)
            TRC$(74, 2) = B$(1 - I)
            TRC$(74, 3) = Date$
        End If

        If V8(I, 3) + V8(1 - I, 3) < TREC(75, 0) Then
            TREC(75, 0) = V8(I, 3) + V8(1 - I, 3)
            TREC(75, 1) = V8(I, 3)
            TREC(75, 2) = V8(1 - I, 3)
            TRC$(75, 0) = B$(I)
            TRC$(75, 1) = SITE$(I)
            TRC$(75, 2) = B$(1 - I)
            TRC$(75, 3) = Date$
        End If

        If QR(I, 1, 3) + QR(I, 2, 3) > TREC(76, 0) Then
            TREC(76, 0) = QR(I, 1, 3) + QR(I, 2, 3)
            TREC(76, 1) = 1
            TREC(76, 2) = 0
            TRC$(76, 0) = B$(I)
            TRC$(76, 1) = SITE$(I)
            TRC$(76, 2) = B$(1 - I)
            TRC$(76, 3) = Date$
        End If

        If QR(I, 3, 3) + QR(I, 4, 3) > TREC(76, 0) Then
            TREC(76, 0) = QR(I, 3, 3) + QR(I, 4, 3)
            TREC(76, 1) = 2
            TREC(76, 2) = 0
            TRC$(76, 0) = B$(I)
            TRC$(76, 1) = SITE$(I)
            TRC$(76, 2) = B$(1 - I)
            TRC$(76, 3) = Date$
        End If

        If QR(I, 1, 3) + QR(I, 2, 3) + QR(1 - I, 1, 3) + QR(1 - I, 2, 3) > TREC(77, 0) Then
            TREC(77, 0) = QR(I, 1, 3) + QR(I, 2, 3) + QR(1 - I, 1, 3) + QR(1 - I, 2, 3)
            TREC(77, 1) = 1
            TREC(77, 2) = QR(I, 1, 3) + QR(I, 2, 3)
            TRC$(77, 0) = B$(I)
            TRC$(77, 1) = SITE$(I)
            TRC$(77, 2) = B$(1 - I)
            TRC$(77, 3) = Date$
        End If

        If QR(I, 3, 3) + QR(I, 4, 3) + QR(1 - I, 3, 3) + QR(1 - I, 4, 3) > TREC(77, 0) Then
            TREC(77, 0) = QR(I, 3, 3) + QR(I, 4, 3) + QR(1 - I, 3, 3) + QR(1 - I, 4, 3)
            TREC(77, 1) = 2
            TREC(77, 2) = QR(I, 3, 3) + QR(I, 4, 3)
            TRC$(77, 0) = B$(I)
            TRC$(77, 1) = SITE$(I)
            TRC$(77, 2) = B$(1 - I)
            TRC$(77, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 3) > TREC(78, 0) Then
                TREC(78, 0) = QR(I, I2, 3)
                TREC(78, 1) = I2
                TREC(78, 2) = 0
                TRC$(78, 0) = B$(I)
                TRC$(78, 1) = SITE$(I)
                TRC$(78, 2) = B$(1 - I)
                TRC$(78, 3) = Date$
            End If

            If QR(I, I2, 3) + QR(1 - I, I2, 3) > TREC(79, 0) Then
                TREC(79, 0) = QR(I, I2, 3) + QR(1 - I, I2, 3)
                TREC(79, 1) = I2
                TREC(79, 2) = QR(I, I2, 3)
                TRC$(79, 0) = B$(I)
                TRC$(79, 1) = SITE$(I)
                TRC$(79, 2) = B$(1 - I)
                TRC$(79, 3) = Date$
            End If
        Next

        If R3(I) > TREC(80, 0) Then
            TREC(80, 0) = R3(I)
            TREC(80, 1) = 0
            TREC(80, 2) = 0
            TRC$(80, 0) = B$(I)
            TRC$(80, 1) = SITE$(I)
            TRC$(80, 2) = B$(1 - I)
            TRC$(80, 3) = Date$
        End If

        If R3(I) < TREC(81, 0) Then
            TREC(81, 0) = R3(I)
            TREC(81, 1) = 0
            TREC(81, 2) = 0
            TRC$(81, 0) = B$(I)
            TRC$(81, 1) = SITE$(I)
            TRC$(81, 2) = B$(1 - I)
            TRC$(81, 3) = Date$
        End If

        If R3(I) + R3(1 - I) > TREC(82, 0) Then
            TREC(82, 0) = R3(I) + R3(1 - I)
            TREC(82, 1) = R3(I)
            TREC(82, 2) = R3(1 - I)
            TRC$(82, 0) = B$(I)
            TRC$(82, 1) = SITE$(I)
            TRC$(82, 2) = B$(1 - I)
            TRC$(82, 3) = Date$
        End If

        If R3(I) + R3(1 - I) < TREC(83, 0) Then
            TREC(83, 0) = R3(I) + R3(1 - I)
            TREC(83, 1) = R3(I)
            TREC(83, 2) = R3(1 - I)
            TRC$(83, 0) = B$(I)
            TRC$(83, 1) = SITE$(I)
            TRC$(83, 2) = B$(1 - I)
            TRC$(83, 3) = Date$
        End If

        If QR(I, 1, 4) + QR(I, 2, 4) > TREC(84, 0) Then
            TREC(84, 0) = QR(I, 1, 4) + QR(I, 2, 4)
            TREC(84, 1) = 1
            TREC(84, 2) = QR(I, 1, 4) + QR(I, 2, 4)
            TRC$(84, 0) = B$(I)
            TRC$(84, 1) = SITE$(I)
            TRC$(84, 2) = B$(1 - I)
            TRC$(84, 3) = Date$
        End If

        If QR(I, 3, 4) + QR(I, 4, 4) > TREC(84, 0) Then
            TREC(84, 0) = QR(I, 3, 4) + QR(I, 4, 4)
            TREC(84, 1) = 2
            TREC(84, 2) = QR(I, 3, 4) + QR(I, 4, 4)
            TRC$(84, 0) = B$(I)
            TRC$(84, 1) = SITE$(I)
            TRC$(84, 2) = B$(1 - I)
            TRC$(84, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 4) > TREC(85, 0) Then
                TREC(85, 0) = QR(I, I2, 4)
                TREC(85, 1) = I2
                TREC(85, 2) = 0
                TRC$(85, 0) = B$(I)
                TRC$(85, 1) = SITE$(I)
                TRC$(85, 2) = B$(1 - I)
                TRC$(85, 3) = Date$
            End If
        Next

        If V8(I, 11) > TREC(86, 0) Then
            TREC(86, 0) = V8(I, 11)
            TREC(86, 1) = 0
            TREC(86, 2) = 0
            TRC$(86, 0) = B$(I)
            TRC$(86, 1) = SITE$(I)
            TRC$(86, 2) = B$(1 - I)
            TRC$(86, 3) = Date$
        End If

        If V8(I, 11) + V8(1 - I, 11) > TREC(88, 0) Then
            TREC(88, 0) = V8(I, 11) + V8(1 - I, 11)
            TREC(88, 1) = V8(I, 11)
            TREC(88, 2) = V8(1 - I, 11)
            TRC$(88, 0) = B$(I)
            TRC$(88, 1) = SITE$(I)
            TRC$(88, 2) = B$(1 - I)
            TRC$(88, 3) = Date$
        End If

        If V8(I, 11) + V8(1 - I, 11) < TREC(89, 0) Then
            TREC(89, 0) = V8(I, 11) + V8(1 - I, 11)
            TREC(89, 1) = V8(I, 11)
            TREC(89, 2) = V8(1 - I, 11)
            TRC$(89, 0) = B$(I)
            TRC$(89, 1) = SITE$(I)
            TRC$(89, 2) = B$(1 - I)
            TRC$(89, 3) = Date$
        End If

        If QR(I, 1, 11) + QR(I, 2, 11) > TREC(90, 0) Then
            TREC(90, 0) = QR(I, 1, 11) + QR(I, 2, 11)
            TREC(90, 1) = 1
            TREC(90, 2) = QR(I, 1, 11) + QR(I, 2, 11)
            TRC$(90, 0) = B$(I)
            TRC$(90, 1) = SITE$(I)
            TRC$(90, 2) = B$(1 - I)
            TRC$(90, 3) = Date$
        End If

        If QR(I, 3, 11) + QR(I, 4, 11) > TREC(90, 0) Then
            TREC(90, 0) = QR(I, 3, 11) + QR(I, 4, 11)
            TREC(90, 1) = 2
            TREC(90, 2) = QR(I, 3, 11) + QR(I, 4, 11)
            TRC$(90, 0) = B$(I)
            TRC$(90, 1) = SITE$(I)
            TRC$(90, 2) = B$(1 - I)
            TRC$(90, 3) = Date$
        End If

        ' ASSISTS
        If V8(I, 7) > TREC(96, 0) Then
            TREC(96, 0) = V8(I, 7)
            TREC(96, 1) = 0
            TREC(96, 2) = 0
            TRC$(96, 0) = B$(I)
            TRC$(96, 1) = SITE$(I)
            TRC$(96, 2) = B$(1 - I)
            TRC$(96, 3) = Date$
        End If

        If V8(I, 7) < TREC(97, 0) Then
            TREC(97, 0) = V8(I, 7)
            TREC(97, 1) = 0
            TREC(97, 2) = 0
            TRC$(97, 0) = B$(I)
            TRC$(97, 1) = SITE$(I)
            TRC$(97, 2) = B$(1 - I)
            TRC$(97, 3) = Date$
        End If

        If V8(I, 7) + V8(1 - I, 7) > TREC(98, 0) Then
            TREC(98, 0) = V8(I, 7) + V8(1 - I, 7)
            TREC(98, 1) = V8(I, 7)
            TREC(98, 2) = V8(1 - I, 7)
            TRC$(98, 0) = B$(I)
            TRC$(98, 1) = SITE$(I)
            TRC$(98, 2) = B$(1 - I)
            TRC$(98, 3) = Date$
        End If

        If V8(I, 7) + V8(1 - I, 7) < TREC(99, 0) Then
            TREC(99, 0) = V8(I, 7) + V8(1 - I, 7)
            TREC(99, 1) = V8(I, 7)
            TREC(99, 2) = V8(1 - I, 7)
            TRC$(99, 0) = B$(I)
            TRC$(99, 1) = SITE$(I)
            TRC$(99, 2) = B$(1 - I)
            TRC$(99, 3) = Date$
        End If

        If QR(I, 1, 7) + QR(I, 2, 7) > TREC(100, 0) Then
            TREC(100, 0) = QR(I, 1, 7) + QR(I, 2, 7)
            TREC(100, 1) = 1
            TREC(100, 2) = 0
            TRC$(100, 0) = B$(I)
            TRC$(100, 1) = SITE$(I)
            TRC$(100, 2) = B$(1 - I)
            TRC$(100, 3) = Date$
        End If

        If QR(I, 3, 7) + QR(I, 4, 7) > TREC(100, 0) Then
            TREC(100, 0) = QR(I, 3, 7) + QR(I, 4, 7)
            TREC(100, 1) = 2
            TREC(100, 2) = 0
            TRC$(100, 0) = B$(I)
            TRC$(100, 1) = SITE$(I)
            TRC$(100, 2) = B$(1 - I)
            TRC$(100, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 7) > TREC(102, 0) Then
                TREC(102, 0) = QR(I, I2, 7)
                TREC(102, 1) = I2
                TREC(102, 2) = 0
                TRC$(102, 0) = B$(I)
                TRC$(102, 1) = SITE$(I)
                TRC$(102, 2) = B$(1 - I)
                TRC$(102, 3) = Date$
            End If
        Next

        ' FOULS
        If V8(I, 6) > TREC(104, 0) Then
            TREC(104, 0) = V8(I, 6)
            TREC(104, 1) = 0
            TREC(104, 2) = 0
            TRC$(104, 0) = B$(I)
            TRC$(104, 1) = SITE$(I)
            TRC$(104, 2) = B$(1 - I)
            TRC$(104, 3) = Date$
        End If

        If V8(I, 6) < TREC(105, 0) Then
            TREC(105, 0) = V8(I, 6)
            TREC(105, 1) = 0
            TREC(105, 2) = 0
            TRC$(105, 0) = B$(I)
            TRC$(105, 1) = SITE$(I)
            TRC$(105, 2) = B$(1 - I)
            TRC$(105, 3) = Date$
        End If

        If V8(I, 6) + V8(1 - I, 6) > TREC(106, 0) Then
            TREC(106, 0) = V8(I, 6) + V8(1 - I, 6)
            TREC(106, 1) = V8(I, 6)
            TREC(106, 2) = V8(1 - I, 6)
            TRC$(106, 0) = B$(I)
            TRC$(106, 1) = SITE$(I)
            TRC$(106, 2) = B$(1 - I)
            TRC$(106, 3) = Date$
        End If

        If V8(I, 6) + V8(1 - I, 6) < TREC(107, 0) Then
            TREC(107, 0) = V8(I, 6) + V8(1 - I, 6)
            TREC(107, 1) = V8(I, 6)
            TREC(107, 2) = V8(1 - I, 6)
            TRC$(107, 0) = B$(I)
            TRC$(107, 1) = SITE$(I)
            TRC$(107, 2) = B$(1 - I)
            TRC$(107, 3) = Date$
        End If

        If QR(I, 1, 6) + QR(I, 2, 6) > TREC(108, 0) Then
            TREC(108, 0) = QR(I, 1, 6) + QR(I, 2, 6)
            TREC(108, 1) = 1
            TREC(108, 2) = 0
            TRC$(108, 0) = B$(I)
            TRC$(108, 1) = SITE$(I)
            TRC$(108, 2) = B$(1 - I)
            TRC$(108, 3) = Date$
        End If

        If QR(I, 3, 6) + QR(I, 4, 6) > TREC(108, 0) Then
            TREC(108, 0) = QR(I, 3, 6) + QR(I, 4, 6)
            TREC(108, 1) = 2
            TREC(108, 2) = 0
            TRC$(108, 0) = B$(I)
            TRC$(108, 1) = SITE$(I)
            TRC$(108, 2) = B$(1 - I)
            TRC$(108, 3) = Date$
        End If

        If QR(I, 1, 6) + QR(I, 2, 6) + QR(1 - I, 1, 6) + QR(1 - I, 2, 6) > TREC(109, 0) Then
            TREC(109, 0) = QR(I, 1, 6) + QR(I, 2, 6) + QR(1 - I, 1, 6) + QR(1 - I, 2, 6)
            TREC(109, 1) = 1
            TREC(109, 2) = QR(I, 1, 6) + QR(I, 2, 6)
            TRC$(109, 0) = B$(I)
            TRC$(109, 1) = SITE$(I)
            TRC$(109, 2) = B$(1 - I)
            TRC$(109, 3) = Date$
        End If

        If QR(I, 3, 6) + QR(I, 4, 6) + QR(1 - I, 3, 6) + QR(1 - I, 4, 6) > TREC(109, 0) Then
            TREC(109, 0) = QR(I, 3, 6) + QR(I, 4, 6) + QR(1 - I, 3, 6) + QR(1 - I, 4, 6)
            TREC(109, 1) = 2
            TREC(109, 2) = QR(I, 3, 6) + QR(I, 4, 6)
            TRC$(109, 0) = B$(I)
            TRC$(109, 1) = SITE$(I)
            TRC$(109, 2) = B$(1 - I)
            TRC$(109, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 6) > TREC(110, 0) Then
                TREC(110, 0) = QR(I, I2, 6)
                TREC(110, 1) = I2
                TREC(110, 2) = 0
                TRC$(110, 0) = B$(I)
                TRC$(110, 1) = SITE$(I)
                TRC$(110, 2) = B$(1 - I)
                TRC$(110, 3) = Date$
            End If

            If QR(I, I2, 6) + QR(1 - I, I2, 6) > TREC(111, 0) Then
                TREC(111, 0) = QR(I, I2, 6) + QR(1 - I, I2, 6)
                TREC(111, 1) = I2
                TREC(111, 2) = QR(I, I2, 6)
                TRC$(111, 0) = B$(I)
                TRC$(111, 1) = SITE$(I)
                TRC$(111, 2) = B$(1 - I)
                TRC$(111, 3) = Date$
            End If
        Next

        ' STEALS
        If V8(I, 9) > TREC(112, 0) Then
            TREC(112, 0) = V8(I, 9)
            TREC(112, 1) = 0
            TREC(112, 2) = 0
            TRC$(112, 0) = B$(I)
            TRC$(112, 1) = SITE$(I)
            TRC$(112, 2) = B$(1 - I)
            TRC$(112, 3) = Date$
        End If

        If V8(I, 9) + V8(1 - I, 9) > TREC(114, 0) Then
            TREC(114, 0) = V8(I, 9) + V8(1 - I, 9)
            TREC(114, 1) = V8(I, 9)
            TREC(114, 2) = V8(1 - I, 9)
            TRC$(114, 0) = B$(I)
            TRC$(114, 1) = SITE$(I)
            TRC$(114, 2) = B$(1 - I)
            TRC$(114, 3) = Date$
        End If

        If QR(I, 1, 9) + QR(I, 2, 9) > TREC(116, 0) Then
            TREC(116, 0) = QR(I, 1, 9) + QR(I, 2, 9)
            TREC(116, 1) = 1
            TREC(116, 2) = 0
            TRC$(116, 0) = B$(I)
            TRC$(116, 1) = SITE$(I)
            TRC$(116, 2) = B$(1 - I)
            TRC$(116, 3) = Date$
        End If

        If QR(I, 3, 9) + QR(I, 4, 9) > TREC(116, 0) Then
            TREC(116, 0) = QR(I, 3, 9) + QR(I, 4, 9)
            TREC(116, 1) = 2
            TREC(116, 2) = 0
            TRC$(116, 0) = B$(I)
            TRC$(116, 1) = SITE$(I)
            TRC$(116, 2) = B$(1 - I)
            TRC$(116, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 9) > TREC(117, 0) Then
                TREC(117, 0) = QR(I, I2, 9)
                TREC(117, 1) = I2
                TREC(117, 2) = 0
                TRC$(117, 0) = B$(I)
                TRC$(117, 1) = SITE$(I)
                TRC$(117, 2) = B$(1 - I)
                TRC$(117, 3) = Date$
            End If
        Next

        ' BLOCKS
        If V8(I, 8) > TREC(118, 0) Then
            TREC(118, 0) = V8(I, 8)
            TREC(118, 1) = 0
            TREC(118, 2) = 0
            TRC$(118, 0) = B$(I)
            TRC$(118, 1) = SITE$(I)
            TRC$(118, 2) = B$(1 - I)
            TRC$(118, 3) = Date$
        End If

        If V8(I, 8) + V8(1 - I, 8) > TREC(119, 0) Then
            TREC(119, 0) = V8(I, 8) + V8(1 - I, 8)
            TREC(119, 1) = V8(I, 8)
            TREC(119, 2) = V8(1 - I, 8)
            TRC$(119, 0) = B$(I)
            TRC$(119, 1) = SITE$(I)
            TRC$(119, 2) = B$(1 - I)
            TRC$(119, 3) = Date$
        End If

        ' TURNOVERS
        If V8(I, 12) > TREC(122, 0) Then
            TREC(122, 0) = V8(I, 12)
            TREC(122, 1) = 0
            TREC(122, 2) = 0
            TRC$(122, 0) = B$(I)
            TRC$(122, 1) = SITE$(I)
            TRC$(122, 2) = B$(1 - I)
            TRC$(122, 3) = Date$
        End If

        If V8(I, 12) < TREC(123, 0) Then
            TREC(123, 0) = V8(I, 12)
            TREC(123, 1) = 0
            TREC(123, 2) = 0
            TRC$(123, 0) = B$(I)
            TRC$(123, 1) = SITE$(I)
            TRC$(123, 2) = B$(1 - I)
            TRC$(123, 3) = Date$
        End If
        If V8(I, 12) + V8(1 - I, 12) > TREC(124, 0) Then
            TREC(124, 0) = V8(I, 12) + V8(1 - I, 12)
            TREC(124, 1) = V8(I, 12)
            TREC(124, 2) = V8(1 - I, 12)
            TRC$(124, 0) = B$(I)
            TRC$(124, 1) = SITE$(I)
            TRC$(124, 2) = B$(1 - I)
            TRC$(124, 3) = Date$
        End If

        If V8(I, 12) + V8(1 - I, 12) < TREC(125, 0) Then
            TREC(125, 0) = V8(I, 12) + V8(1 - I, 12)
            TREC(125, 1) = V8(I, 12)
            TREC(125, 2) = V8(1 - I, 12)
            TRC$(125, 0) = B$(I)
            TRC$(125, 1) = SITE$(I)
            TRC$(125, 2) = B$(1 - I)
            TRC$(125, 3) = Date$
        End If

    Next

    Open diskPaths$(1) + "RECORDS.GAM" For Output As #1

    For I = 0 To 48
        For I1 = 0 To 2
            Write #1, REC!(I, I1)
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

'------------------------------
'   UPDATESCREEN Subroutine
'------------------------------
' ...explanation...
Sub UPDATESCREEN ()

    'M A I N   S C R E E N
    Call TICKERUPDATE

    Locate 1, 37: Color 15, 0: Print Using "##"; Int(T / 60);: Print ":";

    If T - Int(T / 60) * 60 < 10 Then
        Print "0";: Print Using "#"; T - Int(T / 60) * 60;
    Else
        Print Using "##"; T - Int(T / 60) * 60;
    End If

    Locate 4, 13: Color 14, 0: Print Using "###"; S!(0, 0);
    Locate 2, 35: Print Using " PERIOD #"; H;

    If H > 4 Then Locate , 35: Print Using "OVERTIME #"; H - 4

    Locate 4, 60: Color 14, 0: Print Using "###"; S!(1, 0)
    Color 7: Locate 3, 35: Print Using "##  "; T0(0);: Color 12: Print "TO  ";: Color 7: Print Using "##"; T0(1)
    Locate , 35: Print Using "##  "; R3(0);: Color 12: Print "RB  ";: Color 7: Print Using "##"; R3(1)
    Locate , 35: Print Using "##  "; P2(0);: Color 12: Print "TF  ";: Color 7: Print Using "##"; P2(1);

    If P2(0) > NB% - 1 And H <= 4 Or P2(0) > 2 And H > 4 Then Locate , 46: Color 0, 2: Print "BONUS";: Color 7, 0
    If P2(1) > NB% - 1 And H <= 4 Or P2(1) > 2 And H > 4 Then Locate , 29: Color 0, 2: Print "BONUS": Color 7, 0

    Locate 6, 9: Print "TIMEOUTS:"; T1(0);: Locate , 56: Print "TIMEOUTS:"; T1(1)
    Locate 6, 37: Color 15, 0: Print Using " :##"; X8

    If T < 25 And X8 >= T Or H > 4 And T = 300 Then Locate 6, 37: Print " :  "

    If pbpDelay! <> 0 Then
        Locate 8, 58
        Color T(D, 33), T(D, 34): Print "DEF - "; defenseStyles$(D2(D))
        Locate , 58
        Color T(P, 33), T(P, 34): Print "OFF - "; offenseStyles$(O0%(P))
        Color 15, 0
        Locate , 41: Color 14: Print "(1) ";: Color 15: Print "SHOOT        ";: Color 14: Print "ENTER";: Color 15: Print "-PASS"
        Locate , 41: Color 14: Print "(2) ";: Color 15: Print B$(P); " CALL TIMEOUT     "
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
            Print Left$(AA$(0, B1!(0, I1), 0), 12); Tab(16);
            J = (A!(0, B1!(0, I1), 3)) - A!(0, B1!(0, I1), 7) - A!(0, B1!(0, I1), 11) - A!(0, B1!(0, I1), 12)

            If J <= 0 Then J = 0
            If A!(0, B1!(0, I1), 1) >= 100 Then A!(0, B1!(0, I1), 1) = 99

            Print Using "## ## "; A!(0, B1!(0, I1), 0); A!(0, B1!(0, I1), 1);

            If P = 0 Then
                Print Using "##"; A!(0, B1!(0, I1), 21) * 2 / 10;
            Else
                Print Using "##"; A!(0, B1!(0, I1), 2) * 2 / 10;
            End If

            If J <= 0 And (H = 1 Or H = 2 Or H = 3 Or H = 4 And T > 120) Then Color 12

            Print Using "### "; J;
            Color 15
            Print Using "## ##"; A!(0, B1!(0, I1), 4) / 10; A!(0, B1!(0, I1), 5) / 10;
            Print Using "###  "; A!(0, B1!(0, I1), 8) * 2 + A!(0, B1!(0, I1), 10) + W%(0, B1!(0, I1), 0);

            If A!(0, B1!(0, I1), 12) >= 5 Then Color 12

            Print Using "#"; A!(0, B1!(0, I1), 12)
            Color 15

        Next I1

        Locate 19, 40

        For I1 = 0 To 4
            Locate , 40
            Color 2, 0
            Print Using "# "; I1;
            Color 15: Print Left$(AA$(1, B1!(1, I1), 0), 12); Tab(56);
            J = (A!(1, B1!(1, I1), 3)) - A!(1, B1!(1, I1), 7) - A!(1, B1!(1, I1), 11) - A!(1, B1!(1, I1), 12)
            If J <= 0 Then J = 0
            If A!(1, B1!(1, I1), 1) >= 100 Then A!(1, B1!(1, I1), 1) = 99
            Print Using "## ## "; A!(1, B1!(1, I1), 0); A!(1, B1!(1, I1), 1);
            If P = 1 Then Print Using "##"; A!(1, B1!(1, I1), 21) * 2 / 10; Else Print Using "##"; A!(1, B1!(1, I1), 2) * 2 / 10;
            If J <= 0 And (H = 1 Or H = 2 Or H = 3 Or H = 4 And T > 120) Then Color 12
            Print Using "### "; J;
            Color 15
            Print Using "## ##"; A!(1, B1!(1, I1), 4) / 10; A!(1, B1!(1, I1), 5) / 10;
            Print Using "###  "; A!(1, B1!(1, I1), 8) * 2 + A!(1, B1!(1, I1), 10) + W%(1, B1!(1, I1), 0);
            If A!(1, B1!(1, I1), 12) >= 5 Then Color 12
            Print Using "#"; A!(1, B1!(1, I1), 12)
            Color 15
        Next I1

    End If

End Sub

