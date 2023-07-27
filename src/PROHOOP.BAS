Sub PROHOOP

    _Title "Basketball: The Pro Game v" + GAME_VERSION$ + " - HOOPS!"

    'Open "pbplog" For Output As #7

    Shared scheduleFile$
    Shared tickerStart

    Shared coachMode$(), MO$()
    Shared sClockOpt$(), yesNo$()

    Color 7, 0
    Cls

    Randomize Timer

    Call InitVar

    '----------------------------------------
    '         DETERMINE GAME OPTIONS
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
        For I = 1 To 8:
            Print #1, 0
        Next
        Print #1, 4
        For I = 10 To 12:
            Print #1, 0
        Next
        Close 1
    End If

    Open "DEFAULT" For Input As #1
    Input #1, U5
    Input #1, XY%
    Input #1, sClockVal
    Input #1, N
    Input #1, PO
    Input #1, pbpDelay!
    Input #1, X4%
    Input #1, coachOpt
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
        Input #2, scheduleFile$
        For I = 0 To 2:
            Input #2, AP%(I)
        Next
        Close 2

        Open diskPaths$(0) + scheduleFile$ For Random As #1 Len = 48

        For X = 0 To 18
            Field #1, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
        Next

        Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)

        Get #1, AP%(0) + AP%(2)

        For X = 0 To 18:
            NG%(X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 1:
            YN$(X) = Q$(20 + X)
        Next

        VS% = CVI(Q$(22))
        HS% = CVI(Q$(23))

        Close 1

        AP = 1

    End If

    If AP = 1 Then

        Call AUTOSETUP
        Call SETUPRATINGS

    Else

        ' USER SELECTION OF OPTIONS
        'N = 1 is neutral site
        N = 1

        Do

            Color 15, 0
            Cls

            Color 2
            Print Tab(29); "SELECT OPENING OPTIONS"
            Print
            Locate 2, 2: Color 14
            Print "(1) ";: Color 15: Print MO$(U5)
            Locate 3, 2: Color 14
            Print "(2) ";: Color 15: Print "VISITING TEAM: ";: Color T(0, 33), T(0, 34): Print B$(0)
            Color , 0
            Locate 4, 2: Color 14
            Print "(3) ";: Color 15: Print "HOME TEAM: ";: Color T(1, 33), T(1, 34): Print B$(1)
            Color , 0
            Locate 5, 2: Color 14
            Print "(4) ";: Color 15: Print "3-POINT SHOT: "; TS$(XY%)
            Locate 6, 2: Color 14
            Print "(5) ";: Color 15: Print sClockOpt$(sClockVal)
            Locate 7, 2: Color 14
            Print "(6) ";: Color 15: Print "LOCATION: "; location$(N)
            Locate 8, 2: Color 14
            Print "(7) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#"; pbpDelay!
            Locate 9, 2: Color 14
            Print "(8) ";: Color 15: Print "START GAME"
            Locate 10, 2: Color 14
            Print "(9) ";: Color 15: Print playoffGame$(PO)
            Locate 11, 2: Color 14
            Print "(0) ";: Color 15: Print "SAVE SETTINGS"
            Locate 12, 2: Color 14
            Print "(A) ";: Color 15: Print "SHOW SHOT PERCENTAGES: "; TS$(X4%)
            Locate 13, 2: Color 14
            Print "(B) ";: Color 15: Print "HUMAN COACHES WILL: "; coachMode$(coachOpt);
            Locate 1, 1
            Locate 14, 2: Color 14
            Print "(C) ";: Color 15: Print "BONUS AFTER FOUL # "; NB%
            Locate 15, 2: Color 14
            Print "(D) ";: Color 15: Print "NUMBER OF FREE THROWS "; freeThrowOpt$(RT%)
            Locate 16, 2: Color 14
            Print "(E) ";: Color 15: Print "FREE THROW RULES "; foulOption$(FR%)
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
                        'Call ChooseGameTeam(P9)
                        Call ChooseGameTeam(YN$, NTMS, teamNbr, B$(P9), P9)
                        If teamNbr <> 0 Then Call DISABLEPLAYERS

                    Case 3:
                        P9 = 1
                        'Call ChooseGameTeam(P9)
                        Call ChooseGameTeam(YN$, NTMS, teamNbr, B$(P9), P9)
                        If teamNbr <> 0 Then Call DISABLEPLAYERS

                    Case 4:
                        XY% = XY% + 1
                        If XY% > 1 Then XY% = 0

                    Case 5:
                        sClockVal = 1 - sClockVal

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
                        Print #1, sClockVal
                        Print #1, N
                        Print #1, PO
                        Print #1, pbpDelay!
                        Print #1, X4%
                        Print #1, coachOpt
                        Print #1, NB%
                        Print #1, RT%
                        Print #1, FR%
                        Print #1, PB%
                        Close 1

                    Case 11:
                        X4% = 1 - X4%

                    Case 12:
                        coachOpt = 1 - coachOpt

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

            Call TEAMBOX(7, 1)

            Locate 7, 11

            Print "IF YOU ARE USING THE 10 MINUTE TICKER OPTION FOR THIS GAME,"
            Locate , 11: Print "ENTER STARTING TIME FOR GAME IN THIS FORMAT"
            Locate , 11: Print "(EXAMPLES: 1:00P, 11:30A). OTHERWISE, JUST HIT ENTER"
            Print
            Locate , 11: Input "START TIME: "; startTime$

            L = Len(startTime$)

            If L < 4 Then
                startTime$ = "1:00P"
            Else
                If L = 5 Then HR = Val(Left$(startTime$, 1)): MIN = Val(Mid$(startTime$, 3, 2))
                If L = 6 Then HR = Val(Left$(startTime$, 2)): MIN = Val(Mid$(startTime$, 4, 2))
            End If

            tickerMeridian$ = Right$(startTime$, 1)

            If UCase$(tickerMeridian$) = "P" And Left$(startTime$, 2) <> "12" Then HR = HR + 12

            TM = HR * 60 + MIN
            tickerStart = Int(TM / 30) - 18

            Call SETUPRATINGS

        Else

            'End
            'System
            Exit Sub

        End If

    End If

    'Determine attendance
    'If N = 2 Or avgAttendance&(1) = 0 Then
    '    actualAttendance& = Int(Rnd(1) * 2000) + 9000
    'Else
    '    actualAttendance& = Int(Rnd(1) * 2000) + avgAttendance&(1) - 1000
    'End If

    '----------------------------------------
    '    AUTO-DETERMINE OTHER OPTIONS
    '----------------------------------------

    ' Determine computer team
    Select Case U5

        Case 1: 
            CT = 0
        Case 2: 
            U5 = 1
            CT = 1
        Case 3: 
            U5 = 2
    
    End Select

    Call LOADSTATS

    '900
    If AP = 1 Then N = NG%(2)

    LS = 0

    Select Case sClockVal
        Case 0: shotClock = 24
        Case 1: shotClock = 30
    End Select

    If gameClock! < 6 And LT = 4 Then LS = 4

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

    Call LastShot
    Call CheckZeroTime

    If gameClock! = 0 Then 170

    Call ResetShotClock
    Call NEWSCREEN

    '----------------------------------------
    '           Tip Off!
    '----------------------------------------

    If H = 1 Or H > 4 Then
        If PB% = 0 Then
            U$ = B$(JB) + " GETS THE JUMP": Call PBP
        Else
            'PB% = 1
            If pbpDelay! <> 0 Then
                U$ = " GETS THE JUMP"
                Call WINDEX
                Color T(JB, 33), T(JB, 34): Print B$(JB);
                Color 15, 0
                Print U$
                Call DELAY
            End If
        End If
    Else
        If PB% = 0 Then
            U$ = B$(JB) + " INBOUNDS THE BALL": Call PBP
        Else
            'PB% = 1
            If pbpDelay! <> 0 Then
                U$ = " INBOUNDS THE BALL"
                Call WINDEX
                Color T(JB, 33), T(JB, 34): Print B$(JB);
                Color 15, 0
                Print U$
                Call DELAY
            End If
        End If
    End If


    P9 = JB
    P = JB

    GoTo 300

    '----- NOODLE -----
    170 '
    gameClock! = 0
    TF(0) = TF(0) + P2(0)
    TF(1) = TF(1) + P2(1)
    Call QUARTERTOTAL
    H = H + 1

    ' *** ADJUST TIMEOUTS ***
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
        Case 1: If pbpDelay! > 0 Then Call WINDEX: Print U$ ': Call DELAY
    End Select

    178 '
    If H = 3 Then

        U$ = "HALFTIME!"

        Select Case PB%
            Case 0: Color 7, 0: Call WINDEX: Print U$
            Case 1: Call WINDEX: Print U$: Call DELAY
        End Select

        If Not (AP = 1 And U5 = 2) Then
            I$ = GetKeyPress$
        End If

        halfTime = 1
        Call BOXOPTIONS(0)
        endGame = 0
        Call TICKER
        halfTime = 0

        For I = 0 To 1

            For I1 = 0 To 14
                If A!(I, I1, 22) = 50 Then
                    A!(I, I1, 22) = 0
                    A!(I, I1, 17) = A!(I, I1, 17) * 3
                    D1(I) = D1(I) - 1
                    MJ = 50
                End If
            Next I1

            If MJ = 50 Then
                MJ = 0
                F5%(I, 0) = 0
                For I1 = 0 To 4: F5%(I, 0) = F5%(I, 0) + A!(I, B1!(I, I1), 17): Next I1
            End If

        Next I

    End If

    If H < 5 And U5 <> 0 Then
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
        gameClock! = 720
        Call UPDATESCREEN

        GoTo 250

    End If

    Call CheckZeroTime

    Call NEWSCREEN

    If H = 5 Then
        U$ = "END OF REGULATION"
        Select Case PB%
            Case 0: Color 7, 0: Call WINDEX: Print U$
            Case 1: If pbpDelay! > 0 Then Call WINDEX: Print U$ ': Call DELAY
        End Select
    End If

    Locate 2, 31: Print "END OF REGULATION"

    If S!(0, 0) = S!(1, 0) Then
        U$ = "OVERTIME !!!"
        Select Case PB%
            Case 0: Color 7, 0: Call WINDEX: Print U$
            Case 1: Call WINDEX: Print U$: Call DELAY
        End Select
        T1(0) = T1(0) + 1
        T1(1) = T1(1) + 1
        gameClock! = 300
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

    '--Start "EndOfGame" routine
    Call POSTGAME

    ' Prompt to play another game
    Cls

    If AP <> 1 Then

        'Using Home Team's color for the box
        Call TEAMBOX(1, 1)

        Do
            U$ = "PLAY ANOTHER GAME (YN)"
            Locate 7, (80 - Len(U$)) / 2
            Print U$

            Color 15, 0
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    End If

    If UCase$(I$) = "Y" Or (AP = 1 And endAllGames <> 1) Then

        Cls
        Print "PLEASE WAIT ..."
        Call PROHOOP

    End If

    '--End "EndOfGame" routine

    Exit Sub

    '----------------------------------------
    '           END OF GAME!!!
    '----------------------------------------

    '================================================================================
    ' Below are all the different GoTo branches for the gameplay
    ' These have not been worked out into a different flow
    ' yet for the game's logic.
    '================================================================================

    300 '
    'ST = 0
    FO = 0
    LS = 0
    XS = 0

    If shotClock > 0 And shotClock < SC(sClockVal) Then
        shotClock = shotClock - 5

    Else

        Select Case sClockVal
            Case 0: shotClock = 24
            Case 1: shotClock = 30
        End Select

        LS = 0

        'GoSub 835
        If P = 0 Then D = 1
        If P = 1 Then D = 0

        If gameClock! < 6 And LT <> 4 Then LS = 4

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
    Call PBP

    If S <= F3S% Then

        ' MADE 3-POINT FIELD GOAL

        If PB% = 1 And pbpDelay! <> 0 Then
            Call WINDEX
            Print "3 PT FG GOOD BY "; AA$(P, B1!(P, B), 0); " !!!"
            Call DELAY
        End If

        If PB% = 0 Then
            PT% = 4
            Call MAKEPBP
            Call PBP
        End If

        Call DELAY

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
        Call DELAY
    End If

    If PB% = 0 Then
        PT% = 5
        Call MAKEPBP
        Call PBP
    End If

    Call ResetShotClock

    gameClock! = gameClock! - 4
    timeElapsed! = 4

    Call AdjustPlayTime
    Call CheckZeroTime
    If gameClock! = 0 Then 170

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

    If U5 = 0 And coachOpt = 1 Or U5 = 1 And D = CT And coachOpt = 1 Or U5 = 1 And P = CT Or U5 = 2 Then

        Call WHOGETSBALL

    Else

        If R0 <= 40 Then

            Do

                If pbpDelay! <> 0 Then

                    If PB% = 0 Then
                        U$ = "PASS TO CHOICE (0-4)": Call PBP
                    Else
                        PB% = 1

                        If Not (AP = 1 And U5 = 2 Or pbpDelay! = 0) Then
                            Call WINDEX
                            Print "PASS TO CHOICE (0-4)"
                            Call PBP
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


    If P1 <> 1 Then

        R0 = Int(Rnd(1) * 1000) + 1

        ' *** BRANCH TO AREA BASED ON DEFENSE ***
        Select Case D2(D)

            Case 1, 4:
                ' *** SHOT VS REG/TIGHT M-M ***
                Select Case R0

                    Case 1, 2

                        X = 0
                        GoTo 580

                    Case 3 To 30
                        Call LastShot
                        Call ResetShotClock
                        Call UPDATESCREEN
                        X = 88
                        GoTo 565

                    Case 31 To 30 + F7%(P, 1) + F7%(D, 2)
                        GoTo 515

                    Case 30 + F7%(P, 1) + F7%(D, 2) To 1000

                        P9 = Int(Rnd(1) * 5)

                        If R0 <= 24 + F7%(P, 1) + F7%(D, 2) + A!(D, B1!(D, P9), 5) + F7%(P, 0) + OX%(O0%(P)) Then
                            GoTo 513
                        Else
                            R0 = Int(Rnd(1) * 1000) + 1

                            If (PASS% <= 3 Or PASS% > 4) And R0 <= 320 + ((JY - 89) * 10) Then 445
                            If PASS% = 4 Then 445
                            GoTo 437
                        End If

                End Select

            Case 2, 5
                ' *** SHOT VS LOOSE/SAG M-M ***
                Select Case R0

                    Case 1
                        X = 0
                        GoTo 580

                    Case 2 To 10
                        Call LastShot
                        Call ResetShotClock
                        Call UPDATESCREEN
                        X = 88
                        GoTo 565

                    Case 10 To 10 + F7%(P, 1) + F7%(D, 2)
                        GoTo 515

                    Case 10 + F7%(P, 1) + F7%(D, 2) To 1000

                        P9 = Int(Rnd(1) * 5)

                        If R0 <= 8 + F7%(P, 1) + F7%(D, 2) + A!(D, B1!(D, P9), 5) + F7%(P, 0) + OX%(O0%(P)) Then
                            GoTo 513
                        Else
                            R0 = Int(Rnd(1) * 1000) + 1

                            If (PASS% <= 3 Or PASS% > 4) And R0 <= 320 + ((JY - 89) * 10) Then 445
                            If PASS% = 4 Then 445
                            GoTo 437
                        End If

                End Select

            Case 3
                ' *** SHOT VS TRAP M-M ***
                Select Case R0

                    Case 1 To 3
                        X = 0
                        GoTo 580

                    Case 4 To 50
                        Call LastShot
                        Call ResetShotClock
                        Call UPDATESCREEN
                        X = 88

                        GoTo 565

                    Case 50 To 50 + F7%(P, 1) + F7%(D, 2)
                        GoTo 515

                    Case 50 + F7%(P, 1) + F7%(D, 2) To 1000
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

        End Select

    Else

        P1 = 0
        R0 = Int(Rnd(1) * 1000) + 1

        Select Case R0

            Case 1 To 3
                X = 0
                GoTo 580

            Case 4 To 50
                Call LastShot
                Call ResetShotClock
                Call UPDATESCREEN
                X = 88
                GoTo 565

            Case 50 To 50 + F7%(P, 1) + F7%(D, 2)
                GoTo 515

            Case 50 + F7%(P, 1) + F7%(D, 2) To 1000

                P9 = Int(Rnd(1) * 5)

                If R0 <= 40 + F7%(P, 1) + F7%(D, 2) + A!(D, B1!(D, P9), 5) + F7%(P, 0) Then
                    GoTo 513
                Else
                    R0 = Int(Rnd(1) * 1000) + 1

                    If (PASS% <= 3 Or PASS% > 4) And R0 <= 320 + ((JY - 89) * 10) Then 445
                    If PASS% = 4 Then 445

                    GoTo 437

                End If
        End Select

    End If


    '----- NOODLE -----

    ' ***BAD SHOT ***

    437 '
    A4 = 0

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
    If Not (H = 4 And gameClock! <= 120 Or H > 4) Then
        If A!(P, B1!(P, B), 3) - A!(P, B1!(P, B), 7) - A!(P, B1!(P, B), 11) - A!(P, B1!(P, B), 12) <= 0 Then 437
    End If

    446 '
    ' *** CHECKING 3-POINT SHOT ***
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
            If H >= 4 And gameClock! < 30 And S!(D, 0) - S!(P, 0) > 3 And A!(P, B1!(P, B), 19) >= 3 Then
                PZ% = 4
                X = 0
                F3S% = A!(P, B1!(P, B), 20) + F7%(D, 4)
                GoTo 520
            End If

        End If

    End If

    448 '
    ' *** CHECKING FOR ASSISTED SHOT ***

    ' *** GET SHOT PCT ***
    F = A!(P, B1!(P, B), 0) + D1(D)

    If N = P Then
        F = F + 2
    Else
        If N <> 2 Then F = F - 2
    End If

    A4 = Int(Rnd(1) * 41)
    R0 = Int(Rnd(1) * F5%(P, 4)) + 1
    RN = Int(.17 * F5%(P, 4) + .5)

    If RN >= R0 Then
        F = F + A4
        PZ% = 2
        X = 0
    Else

        ' *** CHECK PENETRATION SHOT ***
        RN = Int(Rnd(1) * 100) + 1
        A4 = Int(Rnd(1) * 50) + 1

        If RN <= (B%(P, B1!(P, B), 16) + OY%(O0%(P))) * 2 + 1 Then

            PZ% = 3
            F = F + A4
            X = 0

        Else

            ' *** CHECK AVERAGE SHOT ***
            I2 = B%(P, B1!(P, B), 16)
            RN = Int(Rnd(1) * 2)

            If Left$(AA$(P, B1!(P, B), 1), 1) = "G" Or Left$(AA$(P, B1!(P, B), 1), 1) = "F" Then

                'For a Guard or Forward
                R2 = Int(Rnd(1) * 3)

                Select Case R2
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
    Call ResetShotClock

    A!(P, B1!(P, B), 23) = A!(P, B1!(P, B), 23) + 1

    If pbpDelay! <> 0 Then

        If PB% = 1 Then
            Call WINDEX
            Print "BALL STOLEN BY "; AA$(D, B1!(D, P9), 0); "!"
            Call DELAY
        Else
            RN = Int(Rnd(1) * 100) + 1

            Select Case RN
                Case 1 To 3: U$ = AA$(D, B1!(D, P9), 0) + " QUICKLY REACHES IN!"
                Case 4 To 6: U$ = AA$(D, B1!(D, P9), 0) + " SNEAKS IN FROM BEHIND!"
                Case 7 To 10: U$ = AA$(D, B1!(D, P9), 0) + " GETS A HAND ON THE BALL!"
            End Select

            Call PBP
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

            Call PBP

        End If

    End If

    500 '
    A!(D, B1!(D, P9), 13) = A!(D, B1!(D, P9), 13) + 1
    T0(P) = T0(P) + 1

    Call UPDATESCREEN

    C1 = 2
    P9 = D

    Call CheckZeroTime
    If gameClock! = 0 Then 170

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
    Call ResetShotClock

    A!(P, B1!(P, B), 23) = A!(P, B1!(P, B), 23) + 1
    T0(P) = T0(P) + 1

    If SV% <> 1 Then PT% = 1: Call MAKEPBP

    SV% = 0

    If PB% = 1 And pbpDelay! <> 0 Then
        Call WINDEX
        Print "TURNOVER BY ";: Color T(P, 33), T(P, 34): Print B$(P)
        Color 15, 0: Call DELAY
    End If

    If PB% = 0 Then Call PBP
    U$ = "BALL OUT OF BOUNDS TO " + B$(D)

    If PB% = 0 Then
        Call PBP
    Else
        'PB% = 1
        If pbpDelay! <> 0 Then Print U$: Call DELAY
    End If

    Call DELAY
    Call UPDATESCREEN

    Locate 10, 1

    Call CheckZeroTime
    If gameClock! = 0 Then 170

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
    Call CheckZeroTime
    If gameClock! = 0 Then 170

    If PZ% = 4 And F3S% <= 0 Then F3S% = 3

    If pbpDelay! <> 0 Then

        If F > 70 And FB% = 1 And PASS% = 1 Then
            PT% = 6
            PZ% = 8
            Call MAKEPBP
            Call PBP
        Else

            If AA$(P, B1!(P, B), 0) <> PB$ Then
                If PB$ = "" Then
                    U$ = "PASS TO " + AA$(P, B1!(P, B), 0): Call PBP
                Else
                    'PB$ <> ""
                    PT% = 10: Call MAKEPBP: Call PBP
                End If
            Else
                'AA$(P, B1!(P, B), 0) = PB$
                PT% = 9: Call MAKEPBP: Call PBP
            End If

            If shotClock <= 0 Then U$ = "SHOT CLOCK HAS EXPIRED!": SV% = 1: GoTo 515
            If shotClock < 5 Then U$ = "SHOT CLOCK DOWN TO" + Str$(shotClock): Call PBP

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

                    Call PBP

                Case 2:
                    U$ = Y2$ + " WORKS FREE INSIDE": Call PBP
                    U$ = "HE HAS AN OPEN SHOT": Call PBP

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

                            Call PBP
                            U$ = "HE HAS A GOOD SHOT INSIDE"
                            Call PBP

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

                            Call PBP
                            U$ = "HE'S GOT A GOOD SHOT UNDERNEATH"
                            Call PBP
                        End If

                        If Z3$ = "G" Then
                            Select Case RN
                                Case 1 To 25: U$ = Y2$ + " DRIVES THE LANE": Call PBP: U$ = "HE HAS A GOOD SHOT CLOSE IN"
                                Case 26 To 50: U$ = Y2$ + " DRIVES THROUGH THE KEY": Call PBP: U$ = "HE'S INTO THE LANE": Call PBP: U$ = "HE HAS A GOOD SHOT CLOSE IN"
                                Case 51 To 75: U$ = Y2$ + " DRIVES TO THE BASKET": Call PBP: U$ = "HE HAS A GOOD SHOT CLOSE IN"
                            End Select

                            Call PBP
                        Else
                            U$ = Y2$ + " DRIVES THROUGH AN OPENING": Call PBP
                        End If

                    End If

                Case 4:
                    U$ = "HE'S OPEN FROM 3-POINT RANGE!": Call PBP

                Case 5:
                    U$ = "HE'S OPEN FROM CLOSE RANGE": Call PBP

                Case 6:
                    U$ = "HE'S OPEN FROM MEDIUM RANGE": Call PBP

                Case 7:
                    U$ = "HE'S OPEN FROM LONG RANGE": Call PBP

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

    If PB% = 1 And pbpDelay! <> 0 Then Call DELAY

    If U5 <> 0 Or coachOpt = 1 Then

        If H >= 4 And gameClock! <= 240 And S!(P, 0) - S!(D, 0) > 0 And U5 = 1 And P = CT Then

            '985
            U$ = Str$(A!(P, B1!(P, B), 1)) + " FT%  INTENTIONAL FOUL (YN)"

            Do
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "3"

            If UCase$(I$) = "Y" Or I$ = "3" Then P6 = 3: GoTo 532

        Else

            If U5 = 2 Or U5 = 1 And D = CT Then

                '980
                If gameClock! <= 240 And H >= 4 And S!(P, 0) - S!(D, 0) > 10 Then
                    If (U5 = 1 Or U5 = 0) And coachOpt <> 1 Then 530
                Else
                    If gameClock! <= 30 And H >= 4 And S!(P, 0) - S!(D, 0) > 0 Then P6 = 3: GoTo 532
                    If (U5 = 1 Or U5 = 0) And coachOpt <> 1 Then 530
                End If

            End If
        End If


        951 '
        If gameClock! <= 1 Or shotClock <= 6 Then
            P6 = 1
        Else
            If gameClock! <= 8 And S!(P, 0) - S!(D, 0) <= -3 And H >= 4 Then
                P6 = 1
            Else
                If gameClock! <= 20 And S!(P, 0) - S!(D, 0) <= -5 And H >= 4 Then
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
    If Val(I$) = 3 And (U5 = 0 Or U5 = 1 And P = CT) And H >= 4 And gameClock! < 240 Then P6 = 3
    If Val(I$) = 2 And (U5 = 0 Or U5 = 1 And D = CT) And T1(P) > 0 Then P6 = 2

    P3 = P6

    If P6 = 2 And T1(P) = 0 Then 530
    If P6 = 3 And H < 4 Or P6 = 3 And U5 = 1 And D = CT Then 530
    If P6 = 3 And H = 4 And gameClock! > 240 Then 530
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

    537 '
    ' *** SHOOT!!! ***
    PT% = 3
    Call MAKEPBP
    Call PBP
    S = 0
    R0 = Int(Rnd(1) * 100) + 1
    A!(P, B1!(P, B), 7) = A!(P, B1!(P, B), 7) + 1

    If H > 3 And gameClock! <= 30 And S!(D, 0) - S!(P, 0) = 2 Or H > 3 And gameClock! <= 30 And S!(D, 0) - S!(P, 0) = 0 Then 540

    If R0 <= A!(P, B1!(P, B), 16) + F7%(D, 3) Then
        A!(P, B1!(P, B), 7) = A!(P, B1!(P, B), 7) - 1
        Call ResetShotClock
        GoTo 589
    End If

    540 '
    S = Int(Rnd(1) * 100) + 1
    R0 = (F - 2) * .1

    '--- Shot Branch 1 ---
    If S <= R0 Then

        Call ResetShotClock

        gameClock! = gameClock! - 4
        timeElapsed! = 4

        Call AdjustPlayTime
        Call UPDATESCREEN

        If DK% = 1 Then
            If PB% = 0 Then
                PT% = 2: Call MAKEPBP: U$ = "STUFF BY " + AA$(P, B1!(P, B), 0): Call PBP: DK% = 0: GoTo 617
            Else
                'PB% = 1
                Call WINDEX: Print "STUFF BY " + AA$(P, B1!(P, B), 0): Call DELAY: DK% = 0: GoTo 617
            End If
        End If

        If PZ% = 8 And PB% = 0 Then PT% = 7: Call MAKEPBP: PZ% = 0: Call PBP: GoTo 617

        If PB% = 0 Then
            U$ = "BASKET GOOD BY " + AA$(P, B1!(P, B), 0): Call PBP
        Else
            'PB% = 1
            If pbpDelay! <> 0 Then Call WINDEX: Print "BASKET GOOD BY " + AA$(P, B1!(P, B), 0): Call DELAY
        End If

        617 '
        U$ = "... AND HE'S FOULED !!!"

        If PB% = 0 Then
            Call PBP
        Else
            'PB% = 1
            If pbpDelay! <> 0 Then Print U$: Call DELAY
        End If


        F3 = 1

        Call DELAY
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
                    U$ = "2 TO MAKE 1 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %"
                    Call PBP
                Else
                    U$ = "1 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %"
                    Call PBP
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
                Call PBP
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

        621 '
        NM% = 0

        'Two FT Attempts
        For F3 = 1 To 2

            'Increase FT ATT
            A!(P, B1!(P, B), 9) = A!(P, B1!(P, B), 9) + 1

            S = Int(Rnd(1) * 100) + 1

            'FT Success Chance
            FX% = A!(P, B1!(P, B), 1)

            If S <= FX% Then

                If PB% = 0 Then
                    NM% = NM% + 1
                    Call THREEFT
                Else
                    'PB% = 1
                    NM% = NM% + 1
                    Call MADEFT
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

            If NM% = 1 Then Exit For
            If F3 = 1 And (RT% = 0 Or RT% = 1 And (P2(D) <= NB% And H <= 4 Or P2(D) <= 3 And H > 4)) Then Exit For

        Next F3

        624 '
        Call CheckZeroTime
        If gameClock! = 0 Then 170

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
            If PB% = 0 Then U$ = B$(D) + " IS PRESSING": Call PBP

            If PB% = 1 And pbpDelay! <> 0 Then
                Call WINDEX
                Color T(D, 33), T(D, 34): Print B$(D);
                Color 15, 0: Print " IS PRESSING"
                Call DELAY
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
                'PB% = 1
                U$ = "STUFF BY " + AA$(P, B1!(P, B), 0) + " !!!": DK% = 0
            End If
        Else
            If PB% = 0 And PZ% = 8 Then
                PT% = 7: Call MAKEPBP: Call PBP: PZ% = 0: skipShotPBP% = 1
            Else
                U$ = "BASKET BY "
                U$ = U$ + AA$(P, B1!(P, B), 0) + " !!!"
            End If
        End If

        If skipShotPBP% = 0 Then

            If PB% = 1 And pbpDelay! <> 0 Then
                Call WINDEX: Print U$: Call DELAY
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

                If PB% = 0 Then Call PBP

            End If

        End If

        560 '
        S!(P, H) = S!(P, H) + 2
        S!(P, 0) = S!(P, 0) + 2

        Call CreditAssists

        Call UPDATESCREEN

        A!(P, B1!(P, B), 8) = A!(P, B1!(P, B), 8) + 1

        '---> possibly a new sub
        562 '
        U$ = B$(D) + " BALL AFTER SCORE"

        If PB% = 1 And pbpDelay! <> 0 Then
            Locate 9, 1
            Color T(D, 33), T(D, 34)
            Print B$(D);: Color 15, 0
            Print " BALL AFTER SCORE"
            Call DELAY
        End If

        If PB% = 0 Then Locate 8, 41: Print Space$(12): Call PBP

        Call ResetShotClock
        gameClock! = gameClock! - 4
        timeElapsed! = 4

        Call AdjustPlayTime
        Call CheckZeroTime

        If gameClock! = 0 Then 170

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
                U$ = B$(D) + " IS PRESSING": Call PBP
            Else
                'PB% = 1
                If pbpDelay! <> 0 Then
                    Call WINDEX
                    Color T(D, 33), T(D, 34): Print B$(D);
                    Color 15, 0: Print " IS PRESSING": Call DELAY
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
            Call DELAY
        End If

        If PB% = 0 Then PT% = 8: Call MAKEPBP: Call PBP

        Call ResetShotClock

    Else
        If pbpDelay! <> 0 Then

            If PB% = 1 Then
                Call WINDEX
                Print "SHOT BLOCKED BY "; AA$(D, B1!(D, I3), 0)
                Call DELAY
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

                Call PBP

            End If

        End If

        554 A!(D, B1!(D, I3), 15) = A!(D, B1!(D, I3), 15) + 1
        X = 98

    End If

    'GoTo 546

    546 '
    gameClock! = gameClock! - 4
    timeElapsed! = 4

    Call AdjustPlayTime
    Call CheckZeroTime
    If gameClock! = 0 Then 170

    Call UPDATESCREEN

    GoTo 670

    '----- NOODLE -----

    565 '
    Call Fouls(0)

    566 '
    If P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then 570

    Call CheckZeroTime
    If gameClock! = 0 Then 170

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
        Call PBP

        Locate 8, 41: Print Space$(12)
        GoTo 350

    End If

    Call UPDATESCREEN

    If pbpDelay! <> 0 Then
        If PB% = 0 Then
            U$ = "1 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %"
            Call PBP
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
            Call PBP
        Else
            Call WINDEX
            Color T(P, 33), T(P, 34): Print B$(P)
            Color 15, 0
            Print "1 FT: "; AA$(P, B1!(P, B), 0); " "; A!(P, B1!(P, B), 1); " %"
        End If
    End If

    572 '

    'One FT Attempt
    F3 = 1

    'Increase FT ATT
    A!(P, B1!(P, B), 9) = A!(P, B1!(P, B), 9) + 1

    S = Int(Rnd(1) * 100) + 1

    'FT Success Chance
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

    Call CheckZeroTime
    If gameClock! = 0 Then 170

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
            U$ = B$(D) + " IS PRESSING": Call PBP
        Else
            'PB% = 1
            If pbpDelay! <> 0 Then
                Call WINDEX
                Color T(D, 33), T(D, 34)
                Print B$(D);: Color 15, 0
                Print " IS PRESSING"
                Call DELAY
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
    Call PBP

    GoTo 591

    '----- NOODLE -----

    580 '
    Call LastShot

    Call ResetShotClock


    PB$ = ""

    If P = 1 Then
        P = 0
        D = 1
        P9 = 0
        C1 = 2
    Else
        'P = 0
        P = 1
        D = 0
        P9 = 1
        C1 = 2
    End If

    Call Fouls(0)

    P2(D) = P2(D) - 1
    B = W5

    If P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then 570

    Call CheckZeroTime
    If gameClock! = 0 Then 170

    U$ = "OFFENSIVE FOUL"

    If PB% = 0 Then
        Call PBP
    Else
        'PB% = 1
        If pbpDelay! <> 0 Then Call WINDEX: Print U$: Call DELAY
    End If

    U$ = B$(P) + " BALL AFTER FOUL"
    If PB% = 0 Then
        Call PBP
    Else
        'PB% = 1
        If pbpDelay! <> 0 Then Print U$: Call DELAY
    End If

    Locate 8, 41: Print Space$(12)

    B = 6
    Call PBP
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
            U$ = B$(D) + " IS PRESSING": Call PBP
        Else
            'PB% = 1
            If pbpDelay! <> 0 Then
                Call WINDEX
                Color T(D, 33), T(D, 34): Print B$(D);
                Color 15, 0: Print " IS PRESSING"
                Call DELAY
            End If
        End If

        P1 = 1

    End If

    GoTo 300

    '----- NOODLE -----

    584 '
    IN = 99

    Call LastShot
    Call CheckZeroTime
    If gameClock! = 0 Then 170

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

    Select Case sClockVal
        Case 0: shotClock = 24
        Case 1: shotClock = 30
    End Select

    GoTo 566

    '----- NOODLE -----
    589 '
    gameClock! = gameClock! - 4
    timeElapsed! = 4

    Call AdjustPlayTime
    Call UPDATESCREEN
    Call Fouls(0)

    If pbpDelay! <> 0 Then

        If PB% = 0 Then
            If RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
                U$ = "3 TO MAKE 2 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %": Call PBP
            Else
                U$ = "2 FT: " + AA$(P, B1!(P, B), 0) + Str$(A!(P, B1!(P, B), 1)) + " %": Call PBP
            End If
        Else
            'PB% = 1
            Call WINDEX
            Print B$(P)

            If RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
                Print "3 TO MAKE 2 FT: ";
                Print AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
            Else
                Print "2 FT: ";
                Print AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
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

            Call PBP

        Else
            'PB% = 1
            Call WINDEX
            Print B$(P)

            If RT% = 1 And P2(D) > NB% And H <= 4 Or P2(D) > 3 And H > 4 Then
                Print "3 TO MAKE 2 FT: ";
                Print AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
            Else
                Print "2 FT: ";
                Print AA$(P, B1!(P, B), 0); "   "; A!(P, B1!(P, B), 1); " %"
            End If

        End If

    End If

    592 '
    Call DELAY
    NM% = 0

    'Three FT Attempts
    For F3 = 1 To 3

        'Increase FTA
        A!(P, B1!(P, B), 9) = A!(P, B1!(P, B), 9) + 1

        S = Int(Rnd(1) * 100) + 1

        'FT Success Chance
        FX% = A!(P, B1!(P, B), 1)

        If S <= FX% Then
            If PB% = 0 Then
                NM% = NM% + 1
                Call THREEFT
            Else
                'PB% = 1
                NM% = NM% + 1
                Call MADEFT
            End If
        Else
            'S > FX%
            If PB% = 0 Then
                Call BADFT
            Else
                Call MISSFT
            End If
        End If

        'Only have to make 2 (3 to make 2 rules)
        If NM% = 2 Then Exit For

        'On 2nd attempt
        If F3 = 2 Then
            'Modern rules - just exit (only 2 FT)
            '3 to make 2 - if 2 misses then done
            If RT% = 0 Or (RT% = 1 And (P2(D) <= NB% And H <= 4 Or P2(D) <= 3 And H > 4)) Then Exit For
        End If

    Next F3

    596 '
    Call CheckZeroTime
    If gameClock! = 0 Then 170

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
        If PB% = 0 Then U$ = B$(D) + " IS PRESSING": Call PBP
        If PB% = 1 And pbpDelay! <> 0 Then Call WINDEX: Color T(D, 33), T(D, 34): Print B$(D);: Color 15, 0: Print " IS PRESSING": Call DELAY
        P1 = 1
    End If

    GoTo 300

    '----- NOODLE -----

    640 '
    If T1(P) > 0 Then
        T1(P) = T1(P) - 1
        gameClock! = gameClock! + M5
        timeElapsed! = -M5
        Call AdjustPlayTime
        shotClock = shotClock + M5
        U$ = "TIMEOUT " + B$(P):

        Call PBP
        Call DELAY
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
                Call PBP
            Else
                'PB% = 1
                If pbpDelay! <> 0 Then Call WINDEX: Color T(D, 33), T(D, 34): Print B$(D);: Color 15, 0: Print " IS PRESSING": Call DELAY
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

    Call ResetShotClock

    S = 0

    R0 = Int(Rnd(1) * 100) + 1

    If H > 3 And gameClock! < 30 And S!(D, 0) - S!(P, 0) = 2 And R0 <= 12 Or H > 3 And gameClock! < 30 And S!(D, 0) - S!(P, 0) = 0 And R0 <= 12 Then
        Call ReboundRoutine1
        GoTo 760
    End If

    R0 = Int(Rnd(1) * 100) + 1

    Select Case R0

        Case 1 To 11:
            FF% = 1
            GoTo 770

        Case 12 To 19:
            Call ReboundRoutine1
            GoTo 760

        Case 20 To 100:
            Call ReboundRoutine2
            GoTo 800

    End Select


    '----- NOODLE -----

    670 '
    A6 = 1
    P6 = 0
    A4 = 0
    D6 = 0

    Call ResetShotClock

    '-- Start Loop
    700 '
    S = 0
    A4 = 0
    D6 = 0

    R0 = Int(Rnd(1) * 100) + 1

    GG = S!(D, 0) - S!(P, 0)

    If R0 <= 12 And H > 3 And gameClock! < 30 And (GG = 2 Or GG = 0) Then 700
    '-- End Loop

    Select Case R0

        Case 1

            If X = 98 Then 700

            T0(P) = T0(P) + 1
            U$ = "OFFENSIVE GOAL TENDING"
            If PB% = 0 Then
                Call PBP
            Else
                'PB% = 1
                If pbpDelay! <> 0 Then Call WINDEX: Print U$: Call DELAY
            End If

            U$ = "BALL GOES TO " + B$(D)
            If PB% = 0 Then
                Call PBP
            Else
                'PB% = 1
                If pbpDelay! <> 0 Then Call WINDEX: Print U$: Call DELAY
            End If

            Call DELAY

            PB$ = ""

            If P = 1 Then
                P = 0
                D = 1
                P9 = 0
                C1 = 2
            Else
                'P = 0
                P = 1
                D = 0
                P9 = 1
                C1 = 2
            End If

            Call Substitutions

            GoTo 300

        Case 2 To 11
            FF% = 1
            GoTo 770

        Case 12 To 19

            R0 = Int(Rnd(1) * 100) + 1

            If R0 <= 32 Then

                R3(P) = R3(P) + 1

                Call UPDATESCREEN

                U$ = "RB KNOCKED O.B. TO " + B$(P)

                If PB% = 0 Then
                    Call PBP
                Else
                    'PB% = 1
                    If pbpDelay! <> 0 Then Call WINDEX: Print U$: Call DELAY
                End If

                Call DELAY
                Call Substitutions

                GoTo 520

            End If

            R3(D) = R3(D) + 1
            Call UPDATESCREEN

            U$ = "RB KNOCKED O.B. TO " + B$(D)
            If PB% = 0 Then
                Call PBP
            Else
                'PB% = 1
                If pbpDelay! <> 0 Then Call WINDEX: Print U$: Call DELAY
            End If

            Call DELAY

            Call Substitutions

            PB$ = ""

            If P = 1 Then
                P = 0
                D = 1
                P9 = 0
                C1 = 2
            Else
                'P = 0
                P = 1
                D = 0
                P9 = 1
                C1 = 2
            End If

            C1 = 2
            P9 = P

            GoTo 300

        Case 20 To 22

            If X = 99 Then 700

            Call ReboundRoutine1

            A!(P, B1!(P, B), 7) = A!(P, B1!(P, B), 7) + 1
            A!(P, B1!(P, B), 8) = A!(P, B1!(P, B), 8) + 1
            A!(P, B1!(P, B), 11) = A!(P, B1!(P, B), 11) + 1
            R3(P) = R3(P) + 1
            S!(P, H) = S!(P, H) + 2
            S!(P, 0) = S!(P, 0) + 2

            Call UPDATESCREEN

            If PB% = 1 Then
                If pbpDelay! <> 0 Then Call WINDEX: Print "TIPPED IN BY "; AA$(P, B1!(P, B), 0); "!!!": Call DELAY
            Else
                'PB% = 0
                U$ = AA$(P, B1!(P, B), 0) + " TIPS IT IN!!!": Call PBP
            End If

            Call DELAY

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

            U$ = B$(P) + " BALL AFTER SCORE"
            Locate 8, 41: Print Space$(12)

            If PB% = 0 Then
                Call PBP
            Else
                'PB% = 1
                If pbpDelay! <> 0 Then Print U$: Call DELAY
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
                    U$ = B$(D) + " IS PRESSING": Call PBP
                Else
                    'PB% = 1
                    If pbpDelay! <> 0 Then
                        Call WINDEX
                        Color T(D, 33), T(D, 34): Print B$(D);
                        Color 15, 0: Print " IS PRESSING"
                        Call DELAY
                    End If

                    P1 = 1
                End If

            End If

            GoTo 300

        Case 23:

            If P6 <> 4 Then

                If X = 99 Then 700

                U$ = "DEFENSIVE GOAL TENDING"

                If PB% = 0 Then
                    Call PBP
                Else
                    'PB% = 1
                    If pbpDelay! <> 0 Then Call WINDEX: Print U$: Call DELAY
                    Call DELAY
                End If

            End If

            GoTo 560


    End Select

    ' Case 24 to 100

    770 '
    RB = 0

    For I = 0 To 9:
        RB(I) = 0
    Next

    For I = 0 To 4

        If H > 4 Or H = 4 And gameClock! <= 120 Then 773

        If A!(P, B1!(P, I), 3) - A!(P, B1!(P, I), 7) - A!(P, B1!(P, I), 11) - A!(P, B1!(P, I), 12) <= 0 Then
            RB(I) = RB(I) + A!(P, B1!(P, I), 21) * .65: RB = RB + A!(P, B1!(P, I), 21) * .65
        Else
            773 RB(I) = RB(I) + A!(P, B1!(P, I), 21): RB = RB + A!(P, B1!(P, I), 21)
        End If

    Next

    For I = 0 To 4

        If H > 4 Or H = 4 And gameClock! <= 120 Then
            RB(I + 5) = RB(I + 5) + A!(1 - P, B1!(1 - P, I), 2)
            RB = RB + A!(1 - P, B1!(1 - P, I), 2)
        Else
            If A!(1 - P, B1!(1 - P, I), 3) - A!(1 - P, B1!(1 - P, I), 7) - A!(1 - P, B1!(1 - P, I), 11) - A!(1 - P, B1!(1 - P, I), 12) <= 0 Then
                RB(I + 5) = RB(I + 5) + A!(1 - P, B1!(1 - P, I), 2) * .5
                RB = RB + A!(1 - P, B1!(1 - P, I), 2) * .5
            Else
                RB(I + 5) = RB(I + 5) + A!(1 - P, B1!(1 - P, I), 2)
                RB = RB + A!(1 - P, B1!(1 - P, I), 2)
            End If
        End If

    Next

    RN = Rnd(1) * RB

    For B = 0 To 9
        If RN <= RB(B) Then skipRandom% = 1: Exit For
        RB(B + 1) = RB(B + 1) + RB(B)
    Next

    If skipRandom% = 0 Then
        B = Int(Rnd(1) * 10)
    End If

    If B <= 4 Then 761
    B = B - 5

    GoTo 800

    '----- NOODLE -----

    760 '
    If FF% = 1 Then

        FF% = 0

        U$ = AA$(P, B1!(P, B), 0) + " HAS THE OFFENSIVE REBOUND"

        If PB% = 0 Then
            Call PBP
        Else
            'PB% = 1
            If pbpDelay! <> 0 Then Call WINDEX: Print U$: Print "FOR "; B$(P): Call DELAY
        End If

        U$ = "HE'S FOULED !!!"
        If PB% = 0 Then
            Call PBP
        Else
            'PB% = 1
            If pbpDelay! <> 0 Then Print U$: Call DELAY
        End If

        Call DELAY

        A!(P, B1!(P, B), 24) = A!(P, B1!(P, B), 24) + 1
        A!(P, B1!(P, B), 11) = A!(P, B1!(P, B), 11) + 1
        R3(P) = R3(P) + 1

        Call UPDATESCREEN
        Call Fouls(0)

        GoTo 570

    Else

        761 '
        A!(P, B1!(P, B), 24) = A!(P, B1!(P, B), 24) + 1
        A!(P, B1!(P, B), 11) = A!(P, B1!(P, B), 11) + 1
        R3(P) = R3(P) + 1
        Call UPDATESCREEN
        U$ = AA$(P, B1!(P, B), 0) + " HAS THE OFFENSIVE REBOUND"

        If pbpDelay! <> 0 Then
            If PB% = 0 Then
                Call PBP
            Else
                'PB% = 1
                Call WINDEX: Print U$: Print "FOR " + B$(P): Call DELAY
            End If
        End If

        Call CheckZeroTime
        If gameClock! = 0 Then 170

        PB$ = AA$(P, B1!(P, B), 0)
        R0 = Int(Rnd(1) * 10) + 1

        ' *** GET SHOT PCT ***
        F = A!(P, B1!(P, B), 0) + D1(D)

        If N = P Then
            F = F + 2
        Else
            If N <> 2 Then F = F - 2
        End If

        If Not (H > 4 Or H = 4 And gameClock! <= 120) Then
            If A!(P, B1!(P, B), 3) - A!(P, B1!(P, B), 7) - A!(P, B1!(P, B), 11) - A!(P, B1!(P, B), 12) <= 0 Then F = F * .6
        End If

        If R0 <= 8 Then
            PZ% = 9
            GoTo 537
        Else
            GoTo 520
        End If

    End If

    '----- NOODLE -----

    800 '
    If FF% = 1 Then

        FF% = 0

        If pbpDelay! <> 0 Then
            If PB% = 1 Then
                Call WINDEX
                Print AA$(D, B1!(D, B), 0); " HAS THE DEF REBOUND FOR ": Print B$(D); " AND IS FOULED !!!"
                Call DELAY
            Else
                U$ = AA$(D, B1!(D, B), 0) + " HAS THE DEFENSIVE REBOUND": Call PBP
                U$ = "HE'S FOULED !!!": Call PBP
                Call DELAY
            End If
        End If

        'Increase foul count
        A!(D, B1!(D, B), 11) = A!(D, B1!(D, B), 11) + 1
        R3(D) = R3(D) + 1
        Call UPDATESCREEN

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

        GoTo 565
    Else

        A!(D, B1!(D, B), 11) = A!(D, B1!(D, B), 11) + 1
        R3(D) = R3(D) + 1
        Call UPDATESCREEN

        U$ = AA$(D, B1!(D, B), 0) + " HAS THE DEFENSIVE REBOUND"

        If pbpDelay! <> 0 Then
            If PB% = 0 Then
                Call PBP
            Else
                'PB% = 1
                Call WINDEX
                Print U$
                Print "FOR " + B$(D)
                Call DELAY
            End If
        End If

        PB$ = AA$(D, B1!(D, B), 0)
        C1 = 2
        P9 = D

        'Change posession
        PB$ = ""

        If P = 1 Then
            P = 0
            D = 1
            P9 = 0
            C1 = 2
        Else
            P = 1
            D = 0
            P9 = 1
            C1 = 2
        End If

        FB% = 1

        GoTo 300

    End If

End Sub

