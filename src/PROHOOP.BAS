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

    ' USER SELECTION OF OPTIONS
    Do
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
        Print "NOTE:: "
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

'----- comparing here

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
    If PB% = 0 Then U$ = B$(JB) + " GETS THE JUMP": Call DELAY
Else
    If PB% = 0 Then U$ = B$(JB) + " INBOUNDS THE BALL": Call DELAY
End If

If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Color T(JB, 33), T(JB, 34): Print B$(JB);: Color 15, 0: Print U$: Call MOREDELAY

P9 = JB
P = JB

GoTo 300

'----- NOODLE -----
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

X8 = SC(SC): LS = 4
T = T - 4
TE = 4

Call PlayingTime

If T <= 0 Then TE = T: Call PlayingTime: GoTo 170

Call UPDATESCREEN

GoTo 670

'----- NOODLE -----

400 '
A4 = 0
D6 = 0
P3 = 0

B = Int(Rnd(1) * 5)

405 R0 = Int(Rnd(1) * 100) + 1

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
            If pbpDelay! <> 0 And PB% = 0 Then U$ = "PASS TO CHOICE (0-4)": Call DELAY

            If pbpDelay! <> 0 And PB% = 1 Then

                If Not (AP = 1 And U5 = 2 Or pbpDelay! = 0) Then
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

                465 R0 = Int(Rnd(1) * 1000) + 1

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
                GoTo 465
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
                GoTo 465
            End If

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
        GoTo 465
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

445 If Not (H = 4 And T <= 120 Or H > 4) Then
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
            PZ% = 4: X = 0: F3S% = .4 * A!(P, B1!(P, B), 20) + F7%(D, 4)
            GoTo 520
        End If
    Else
        If H >= 4 And T < 30 And S!(D, 0) - S!(P, 0) > 3 And A!(P, B1!(P, B), 19) >= 3 Then
            PZ% = 4: X = 0: F3S% = A!(P, B1!(P, B), 20) + F7%(D, 4)
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
    PZ% = 2: X = 0
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
        I2 = B%(P, B1!(P, B), 16): RN = Int(Rnd(1) * 2)

        If Left$(AA$(P, B1!(P, B), 1), 1) = "G" Or Left$(AA$(P, B1!(P, B), 1), 1) = "F" Then

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

'----- NOODLE -----

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
            Case 1 TO 3: U$ = AA$(D, B1!(D, P9), 0) + " QUICKLY REACHES IN!": Call DELAY
            Case 4 TO 6: U$ = AA$(D, B1!(D, P9), 0) + " SNEAKS IN FROM BEHIND!": Call DELAY
            Case 7 TO 10: U$ = AA$(D, B1!(D, P9), 0) + " GETS A HAND ON THE BALL!": Call DELAY
        End Select

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

If T <= 0 Then TE = T: Call PlayingTime: GoTo 170
PB$ = ""

If P = 1 Then
    P = 0: D = 1: P9 = 0: C1 = 2
Else
    'P = 0;
    P = 1: D = 0: P9 = 1: C1 = 2
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
    Print "TURNOVER BY ";: Color T(P, 33), T(P, 34): Print B$(P): Color 15, 0: Call MOREDELAY
End If

If PB% = 0 Then Call DELAY
U$ = "BALL OUT OF BOUNDS TO " + B$(D)
If PB% = 0 Then Call DELAY
If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call MOREDELAY

Call MOREDELAY
Call UPDATESCREEN
Locate 10, 1

If T <= 0 Then TE = T: Call PlayingTime: GoTo 170

PB$ = ""
If P = 1 Then
    P = 0: D = 1: P9 = 0: C1 = 2
Else
    'P = 0;
    P = 1: D = 0: P9 = 1: C1 = 2
End If

Call Substitutions

GoTo 300

'----- NOODLE -----

520 Call LastShot

If T <= 0 Then TE = T: Call PlayingTime: GoTo 170

If PZ% = 4 And F3S% <= 0 Then F3S% = 3

If pbpDelay! <> 0 Then

    If F > 70 And FB% = 1 And PASS% = 1 Then
        PT% = 6: PZ% = 8: Call MAKEPBP: Call DELAY
    Else

        If AA$(P, B1!(P, B), 0) <> PB$ And PB$ = "" Then U$ = "PASS TO " + AA$(P, B1!(P, B), 0): Call DELAY
        If AA$(P, B1!(P, B), 0) <> PB$ And PB$ <> "" Then PT% = 10: Call MAKEPBP: Call DELAY
        If AA$(P, B1!(P, B), 0) = PB$ Then PT% = 9: Call MAKEPBP: Call DELAY

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
                            Case 1 TO 25: U$ = Y2$ + " MAKES A MOVE DOWN LOW":
                            Case 26 TO 50: U$ = Y2$ + " POSITIONS HIMSELF DOWN LOW"
                            Case 51 TO 75: U$ = Y2$ + " POSTS UP UNDERNEATH"
                            Case 76 TO 100: U$ = Y2$ + " BACKS IN AGAINST " + AA$(D, B1!(D, 4), 0)
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

If pbpDelay! <> 0 Then

    If PB% = 1 Then
        Call WINDEX
        Print "PASS TO "; AA$(P, B1!(P, B), 0); " OF ";
        Color T(P, 33), T(P, 34)
        Print B$(P): Color 15, 0
    End If

    If PB% = 1 And PZ% <> 4 Then Locate 10: Print "SHOT % "; Int(F + .5);

    If PB% = 1 And PZ% = 4 Then
        Locate 10: Print "3 PT SHOT % "; Int(F3S% + .5);: Print " *";
    Else
        'PZ% = 2-3, 5-8
        If PB% = 1 And PZ% >= 2 And PZ% <= 8 Then Print "*";

        If pbpDelay! <> 0 Or PB% = 1 Then

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

If PB% = 1 And pbpDelay! <> 0 Then Call MOREDELAY

If U5 <> 0 Or CM% = 1 Then
    949 '
    If H >= 4 And T <= 240 And S!(P, 0) - S!(D, 0) > 0 And U5 = 1 And P = CT Then

        U$ = Str$(A!(P, B1!(P, B), 1)) + " FT%  INTENTIONAL FOUL (YN)"

        Do
			'-- shouldn't we be calling PBP here?
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "3"

        If UCase$(I$) = "Y" Or I$ = "3" Then P6 = 3: GoTo 532

    Else

        If U5 = 2 Or U5 = 1 And D = CT Then

            980 '
            If T <= 240 And H >= 4 And S!(P, 0) - S!(D, 0) > 10 Then

                If (U5 = 1 Or U5 = 0) And CM% <> 1 Then 530

            Else

                If T <= 30 And H >= 4 And S!(P, 0) - S!(D, 0) > 0 Then P6 = 3: GoTo 532

            End If

        End If
    End If

    951 If T <= 1 Or X8 <= 6 Then P6 = 1: GoTo 532
    If T <= 8 And S!(P, 0) - S!(D, 0) <= -3 And H >= 4 Then P6 = 1: GoTo 532
    If T <= 20 And S!(P, 0) - S!(D, 0) <= -5 And H >= 4 Then P6 = 1: GoTo 532
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
'----------------------------------------------------

P3 = P6

If P6 = 2 And T1(P) = 0 Then 530
If P6 = 3 And H < 4 Or P6 = 3 And U5 = 1 And D = CT Then 530
If P6 = 3 And H = 4 And T > 240 Then 530
If (P6 = 1 Or P6 = 4) And PZ% = 4 Then P6 = 4

'On P6 + 1 GOTO 350, 537, 640, 584, 359
caseVar = P6 + 1

Select Case caseVar
    Case 1
        GoTo 350

    Case 2
        GoTo 537

    Case 3
        GoTo 640

    Case 4
        GoTo 584

    Case 5
        GoTo 359

End Select

'--- technically this branch ends here ---

537 '*** SHOOT!!! ***
PT% = 3: Call MAKEPBP: Call DELAY
S = 0
R0 = Int(Rnd(1) * 100) + 1
A!(P, B1!(P, B), 7) = A!(P, B1!(P, B), 7) + 1
If H > 3 And T <= 30 And S!(D, 0) - S!(P, 0) = 2 Or H > 3 And T <= 30 And S!(D, 0) - S!(P, 0) = 0 Then 540
If R0 <= A!(P, B1!(P, B), 16) + F7%(D, 3) Then
    A!(P, B1!(P, B), 7) = A!(P, B1!(P, B), 7) - 1
    X8 = SC(SC): LS = 4
    GoTo 589
End If

540 S = Int(Rnd(1) * 100) + 1: R0 = (F - 2) * .1

'--- Shot Branch 1 ---
If S <= R0 Then
    X8 = SC(SC)
    LS = 4

    610 T = T - 4: TE = 4: Call PlayingTime: Call UPDATESCREEN

    If DK% = 1 And PB% = 0 Then PT% = 2: Call MAKEPBP: U$ = "STUFF BY " + AA$(P, B1!(P, B), 0): Call DELAY: DK% = 0: GoTo 617
    If DK% = 1 And PB% = 1 Then Call WINDEX: Print "STUFF BY " + AA$(P, B1!(P, B), 0): Call MOREDELAY: DK% = 0: GoTo 617
    If PZ% = 8 And PB% = 0 Then PT% = 7: Call MAKEPBP: PZ% = 0: Call DELAY: GoTo 617
    If PB% = 0 Then U$ = "BASKET GOOD BY " + AA$(P, B1!(P, B), 0): Call DELAY
    If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print "BASKET GOOD BY " + AA$(P, B1!(P, B), 0): Call MOREDELAY

    617 U$ = "... AND HE'S FOULED !!!"
    If PB% = 0 Then Call DELAY
    If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call MOREDELAY

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
        If PB% = 0 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then U$ = "2 TO MAKE 1 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %": Call DELAY: GoTo 620
        If PB% = 0 Then U$ = "1 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %": Call DELAY: GoTo 620
        If PB% = 1 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
            Call WINDEX
            Print B$(P)
            Print "2 TO MAKE 1 FT: ";
            Print AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
            GoTo 620
        End If

        If PB% = 1 Then Call WINDEX: Print B$(P): Print "1 FT: "; AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
    End If

    620 '
    X = 50
    Call Substitutions
    X = 0

    If pbpDelay! <> 0 Then

        If PB% = 0 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
            U$ = "2 TO MAKE 1 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %"
            Call DELAY
            GoTo 621
        End If

        If PB% = 0 Then
            U$ = "1 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %"
            Call DELAY
            GoTo 621
        End If

        If PB% = 1 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
            Call WINDEX
            Print B$(P)
            Print "2 TO MAKE 1 FT: ";
            Print AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
            GoTo 621
        End If

        If PB% = 1 Then Call WINDEX: Print B$(P): Print "1 FT: "; AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"

    End If

    621 NM% = 0

    For F3 = 1 To 2

        S = Int(Rnd(1) * 100) + 1

        FX% = A!(P, B1!(P, B), 1)

        If S <= FX% And PB% = 0 Then NM% = NM% + 1: Call THREEFT
        If S <= FX% And PB% = 1 Then NM% = NM% + 1: Call MADEFT
        If S > FX% And PB% = 0 Then Call BADFT
        If S > FX% And PB% = 1 Then Call MISSFT

        A!(P, B1!(P, B), 9) = A!(P, B1!(P, B), 9) + 1

        If NM% = 1 Then Exit For
        If F3 = 1 And (RT% = 0 Or RT% = 1 And (P2(D) <= NB% And H <= 4 Or P2(D) <= 3 And H > 4)) Then Exit For

    Next F3

    624 If T <= 0 Then TE = T: Call PlayingTime: GoTo 170

    X = 99

    If S > FX% Then 670

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

    If T <= 0 Then TE = T: Call PlayingTime: GoTo 170

    Call UPDATESCREEN

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
        If PB% = 0 Then U$ = B$(D) + " IS PRESSING": Call DELAY
        If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Color T(D, 33), T(D, 34): Print B$(D);: Color 15, 0: Print " IS PRESSING": Call MOREDELAY
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
                Case 1 TO 10: U$ = AA$(D, B1!(D, I3), 0) + " STUFFS THE SHOT!!"
                Case 11 TO 15: U$ = "DENIED BY " + AA$(D, B1!(D, I3), 0) + "!!"
                Case 16 TO 20: U$ = "ERASED BY " + AA$(D, B1!(D, I3), 0) + "!!"
                Case 21 TO 25: U$ = "EXPUNGED BY " + AA$(D, B1!(D, I3), 0) + "!!"
                Case 26 TO 30: U$ = "REBUKED BY " + AA$(D, B1!(D, I3), 0) + "!!"
                Case 31 TO 35: U$ = "SHOT BLOCKED CLEANLY BY " + AA$(D, B1!(D, I3), 0) + "!!"
                Case 36 TO 70: U$ = "SHOT BLOCKED BY " + AA$(D, B1!(D, I3), 0) + "!!"
                Case 71 TO 90: U$ = "SHOT REJECTED BY " + AA$(D, B1!(D, I3), 0) + "!!"
                Case 91 TO 100: U$ = "SHOT SWATTED OUT BY " + AA$(D, B1!(D, I3), 0) + "!!"
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

If T <= 0 Then TE = T: Call PlayingTime: GoTo 170

Call UPDATESCREEN

GoTo 670

'----- NOODLE -----

565 Call Fouls(0)

566 If P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then 570

If T <= 0 Then TE = T: Call PlayingTime: GoTo 170

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
        GoTo 572
    End If

    Call WINDEX
    Color T(P, 33), T(P, 34): Print B$(P)
    Color 15, 0
    Print "1 FT: "; AA$(P, B1!(P, B), 0); " "; A!(P, B1!(P, B), 1); " %"

End If

572 '
A!(P, B1!(P, B), 9) = A!(P, B1!(P, B), 9) + 1
F3 = 1
S = Int(Rnd(1) * 100) + 1
FX% = A!(P, B1!(P, B), 1)

If S <= FX% And PB% = 0 Then Call THREEFT
If S <= FX% And PB% = 1 Then Call MADEFT
If S > FX% And PB% = 0 Then Call BADFT
If S > FX% And PB% = 1 Then Call MISSFT
LT = 0

If T <= 0 Then TE = T: Call PlayingTime: GoTo 170

If S > FX% Then 650

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

570 If RT% = 1 And (P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4) Then
    U$ = AA$(P, B1!(P, B), 0) + " GETS 3 FT TO MAKE 2..."
Else
    U$ = AA$(P, B1!(P, B), 0) + " GETS 2 FREE THROWS..."
End If
Call DELAY

GoTo 591

'----- NOODLE -----

580 Call LastShot
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

If T <= 0 Then TE = T: Call PlayingTime: GoTo 170

U$ = "OFFENSIVE FOUL"
If PB% = 0 Then Call DELAY
If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print U$: Call MOREDELAY
U$ = B$(P) + " BALL AFTER FOUL"
If PB% = 0 Then Call DELAY
If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call MOREDELAY

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
    If PB% = 0 Then U$ = B$(D) + " IS PRESSING": Call DELAY
    If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Color T(D, 33), T(D, 34): Print B$(D);: Color 15, 0: Print " IS PRESSING": Call MOREDELAY
    P1 = 1
End If

GoTo 300

'----- NOODLE -----

584 IN = 99

Call LastShot

If T <= 0 Then TE = T: Call PlayingTime: GoTo 170

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

'----- NOODLE -----
589 '
T = T - 4
TE = 4

Call PlayingTime
Call UPDATESCREEN
Call Fouls(0)

If pbpDelay! <> 0 Then
    If PB% = 0 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then U$ = "3 TO MAKE 2 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %": Call DELAY: GoTo 591
    If PB% = 0 Then U$ = "2 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %": Call DELAY: GoTo 591
    If PB% = 1 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
        Call WINDEX
        Print B$(P)
        Print "3 TO MAKE 2 FT: ";
        Print AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
        GoTo 591
    End If
    If PB% = 1 Then Call WINDEX: Print B$(P): Print "2 FT: "; AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
End If

591 '
X = 50
Call Substitutions
X = 0

If pbpDelay! <> 0 Then
    If PB% = 0 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then U$ = "3 TO MAKE 2 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %": Call DELAY: GoTo 592
    If PB% = 0 Then U$ = "2 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %": Call DELAY: GoTo 592
    If PB% = 1 And RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
        Call WINDEX
        Print B$(P)
        Print "3 TO MAKE 2 FT: ";
        Print AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
        GoTo 592
    End If
    If PB% = 1 Then Call WINDEX: Print B$(P): Print "2 FT: "; AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
End If

592 Call MOREDELAY
NM% = 0

For F3 = 1 To 3
    S = Int(Rnd(1) * 100) + 1
    FX% = A!(P, B1!(P, B), 1)
    If S <= FX% And PB% = 0 Then NM% = NM% + 1: Call THREEFT
    If S <= FX% And PB% = 1 Then NM% = NM% + 1: Call MADEFT
    If S > FX% And PB% = 0 Then Call BADFT
    If S > FX% And PB% = 1 Then Call MISSFT
    A!(P, B1!(P, B), 9) = A!(P, B1!(P, B), 9) + 1
    If NM% = 2 Then Exit For
    If F3 = 2 And (RT% = 0 Or RT% = 1 And (P2(D) <= NB% And H <= 4 Or P2(D) <= 3 And H > 4)) Then Exit For
Next F3

596 If T <= 0 Then TE = T: Call PlayingTime: GoTo 170

If S > A!(P, B1!(P, B), 1) Then 650

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

640 If T1(P) <= 0 Then 648
T1(P) = T1(P) - 1
T = T + M5: TE = -M5: Call PlayingTime
X8 = X8 + M5
U$ = "TIMEOUT " + B$(P):

Call DELAY
Call MOREDELAY
Call Substitutions

648 If A6 = 0 Then

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
End If

GoTo 350

'----- NOODLE -----
650 A6 = 1: P6 = 0: A4 = 0: D6 = 0
X8 = SC(SC): LS = 4

S = 0
R0 = Int(Rnd(1) * 100) + 1
If H > 3 And T < 30 And S!(D, 0) - S!(P, 0) = 2 And R0 <= 12 Or H > 3 And T < 30 And S!(D, 0) - S!(P, 0) = 0 And R0 <= 12 Then
    Call ReboundRoutine1
    GoTo 760
End If

R0 = Int(Rnd(1) * 100) + 1

Select Case R0
    Case 1 TO 11: FF% = 1: GoTo 707
    Case 12 TO 19: Call ReboundRoutine1: GoTo 760
    Case 20 TO 110: Call ReboundRoutine2: GoTo 800
End Select

'----- NOODLE -----

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

GG = S!(D, 0) - S!(P, 0)

If R0 <= 12 And H > 3 And T < 30 And (GG = 2 Or GG = 0) Then 700

Select Case R0

    Case 1
        If X = 98 Then 700

        T0(P) = T0(P) + 1
        U$ = "OFFENSIVE GOAL TENDING"
        If PB% = 0 Then Call DELAY
        If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print U$: Call MOREDELAY
        U$ = "BALL GOES TO " + B$(D)
        If PB% = 0 Then Call DELAY
        If PB% = 1 And pbpDelay! <> 0 Then Print U$: Call MOREDELAY

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

        U$ = "RB KNOCKED O.B. TO " + B$(D)
        If PB% = 0 Then Call DELAY
        If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Print U$: Call MOREDELAY

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

If T <= 0 Then TE = T: Call PlayingTime: GoTo 170

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
    '        GoTo 465
    '    End If

End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
'$INCLUDE: 'PadRight.BM'
'$INCLUDE: 'GameRoutines.BM'

