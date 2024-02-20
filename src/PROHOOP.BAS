'----------------------------------------
'           PROHOOP Subroutine
'----------------------------------------
'This subroutine serves as the main "source"
'for the action in the game. It steps the
'user through choosing game parameters and
'teams, as well as determining players to
'disable, etc.
Sub PROHOOP ()

    _Title "Basketball: The Pro Game v" + GAME_VERSION$ + " - HOOPS!"

    Shared actualAttendance&
    Shared avgAttendance&()

    Shared scheduleFile$

    Shared tickerStart

    Shared coachMode$(), foulOption$(), freeThrowOpt$()
    Shared location$()
    Shared playMode$(), playoffGame$()
    Shared sClockOpt$(), yesNoText$()

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

        Close #1

    End If

    Open "DEV.BAS" For Input As #2

    For I = 0 To 1
        Input #2, diskPaths$(I)
    Next

    Close #2

    If Not _FileExists("DEFAULT") Then

        Open "DEFAULT" For Output As #3

        For I = 1 To 8:
            Print #3, 0
        Next

        Print #3, 4

        For I = 10 To 12:
            Print #3, 0
        Next

        Close #3

    End If

    Open "DEFAULT" For Input As #4
    Input #4, playerOpt
    Input #4, threePtOpt
    Input #4, sClockVal
    Input #4, gameLoc
    Input #4, playoffOpt
    Input #4, pbpDelay!
    Input #4, shotPctOpt
    Input #4, coachOpt
    Input #4, bonusFoulNum
    Input #4, freeThrowVal
    Input #4, ftRulesOpt
    Input #4, pbpOpt
    Close #4

    If _FileExists("TICKER") Then

        Open "TICKER" For Input As #5

        For I = 1 To 32
            For I1 = 0 To 1
                For I2 = 1 To 14
                    Input #5, SX%(I, I1, I2)
                Next
            Next
        Next

        For I = 1 To 32
            For I1 = 0 To 1
                Input #5, SX$(I, I1)
            Next
        Next

        For I = 1 To 32
            Input #5, ST%(I)
        Next

        Close #5

        TMT = 1
    Else
        TMT = 0
    End If

    'Load game in-progress, if there is one
    If _FileExists("GAME82") Then

        Open "GAME82" For Input As #6

        Input #6, scheduleFile$

        For I = 0 To 2:
            Input #6, schedGame(I)
        Next

        Close #6

        currGame = schedGame(0) + schedGame(2)

        Open "pbplog" + Str$(currGame) For Append As #11

        Open diskPaths$(0) + scheduleFile$ For Random As #7 Len = 48

        For X = 0 To 18
            Field #7, X * 2 As X$, 2 As Q$(X + 1), 48 - 2 - 2 * X As X$
        Next

        Field #7, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)

        Get #7, schedGame(0) + schedGame(2)

        For X = 0 To 18:
            NG%(X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 1:
            diskIDs$(X) = Q$(20 + X)
        Next

        VS% = CVI(Q$(22))
        HS% = CVI(Q$(23))

        Close #7

        autoPlay = 1

    Else

        Open "pbplog" For Append As #11

    End If

    If autoPlay = 1 Then

        Call AUTOSETUP

    Else

        ' USER SELECTION OF OPTIONS
        'gameLoc = 1 is neutral site
        'gameLoc = 0 is not used
        If gameLoc = 0 Then gameLoc = 1

        Do

            Color 15, 0
            Cls

            Color 2
            Print Tab(29); "SELECT OPENING OPTIONS"

            Locate 2, 2: Color 14
            Print "(1) ";: Color 15: Print playMode$(playerOpt)
            Locate 3, 2: Color 14
            Print "(2) ";: Color 15: Print "VISITING TEAM: ";
            Color teamRat_GAME(0, 33), teamRat_GAME(0, 34): Print gameTeams$(0)
            Locate 4, 2: Color 14, 0
            Print "(3) ";: Color 15: Print "HOME TEAM: ";
            Color teamRat_GAME(1, 33), teamRat_GAME(1, 34): Print gameTeams$(1)
            Locate 5, 2: Color 14, 0
            Print "(4) ";: Color 15: Print "GAME LOCATION: "; location$(gameLoc)
            Locate 6, 2: Color 14
            Print "(5) ";: Color 15: Print "SHOT CLOCK: "; sClockOpt$(sClockVal)
            Locate 7, 2: Color 14
            Print "(6) ";: Color 15: Print "3-POINT SHOT: "; yesNoText$(threePtOpt)
            Locate 8, 2: Color 14
            Print "(7) ";: Color 15: Print "SHOW SHOT PERCENTAGES: "; yesNoText$(shotPctOpt)
            Locate 9, 2: Color 14
            Print "(8) ";: Color 15: Print "GAME TYPE: "; playoffGame$(playoffOpt)
            Locate 10, 2: Color 14
            Print "(9) ";: Color 15: Print "START GAME"
            Locate 11, 2: Color 14
            Print "(0) ";: Color 15: Print "SAVE SETTINGS"
            Locate 12, 2: Color 14
            Print "(A) ";: Color 15: Print Using "PLAY-BY-PLAY DELAY (SECS): ##.#"; pbpDelay!
            Locate 13, 2: Color 14
            Print "(B) ";: Color 15: Print "HUMAN COACHES: "; coachMode$(coachOpt);
            Locate 14, 2: Color 14
            Print "(C) ";: Color 15: Print "BONUS AFTER FOUL #: "; bonusFoulNum
            Locate 15, 2: Color 14
            Print "(D) ";: Color 15: Print "NUMBER OF FREE THROWS: "; freeThrowOpt$(freeThrowVal)
            Locate 16, 2: Color 14
            Print "(E) ";: Color 15: Print "FREE THROW RULES: "; foulOption$(ftRulesOpt)
            Locate 17, 2: Color 14
            Print "(F) ";: Color 15: Print "PLAY-BY-PLAY STYLE: "; pbpType$(pbpOpt)

            'Locate 18, 2: Color 14
            'Print "NOTE:: ";: Color 15: Print "BONUS WAS ON 7TH FOUL (UNTIL 72-73), 6TH FOUL (72-73 SEASON)"
            'Locate , 2: Print "       5TH FOUL (1973-74 TO PRESENT)"
            'Locate , 2: Color 14
            'Print "NOTE:: ";: Color 15: Print "UP UNTIL 1972-73, WHEN A TEAM WAS IN THE BONUS, THEY HAD THE CHANCE"
            'Locate , 2: Print "       TO SHOOT 3 FT TO MAKE 2 OR 2 FT TO MAKE 1"
            'Locate , 2: Color 14
            'Print "NOTE:: ";: Color 15: Print "UP UNTIL 1972-73, ALL FOULS WERE SHOT...UNLIKE THE CURRENT RULES"
            'Locate , 2: Color 14
            'Print "NOTE:: ";: Color 15: Print "THE 3-PT SHOT WAS USED FOR ALL ABA SEASONS AND NBA SINCE 1979-80"
            'Locate , 2: Color 14
            'Print "NOTE:: ";: Color 15: Print "THE ABA USED A 30-SEC SHOT CLOCK THROUGH 1973-74";

            Locate 19, 2: Color 14
            Print "NOTE:: "
            Color 15
            Locate , 2: Print "* BONUS ON 7TH FOUL UNTIL 72-73, 6TH FOUL (72-73 SEASON), 5TH FOUL (73-74+)"
            Locate , 2: Print "* UNTIL 72-73, A TEAM IN BONUS CHOSE 3 FT TO MAKE 2 OR 2 FT TO MAKE 1"
            Locate , 2: Print "* UNTIL 72-73, ALL FOULS WERE SHOT"
            Locate , 2: Print "* 3-PT SHOT WAS USED FOR ALL ABA SEASONS AND NBA SINCE 79-80"
            Locate , 2: Print "* ABA HAD 30-SEC SHOT CLOCK THROUGH 73-74; NBA BEGAN 24-SEC SOT CLOCK IN 1954";

            Do

                Color 7

                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    iVal = Val(I$)
                    If UCase$(I$) = "A" Then iVal = 10
                    If UCase$(I$) = "B" Then iVal = 11
                    If UCase$(I$) = "C" Then iVal = 12
                    If UCase$(I$) = "D" Then iVal = 13
                    If UCase$(I$) = "E" Then iVal = 14
                    If UCase$(I$) = "F" Then iVal = 15
                End If

            Loop Until iVal >= 0 And iVal <= 15 Or I$ = Chr$(27) Or UCase$(I$) = "Q"

            If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

                Select Case iVal

                    Case 0:
                        Open "DEFAULT" For Output As #8
                        Print #8, playerOpt
                        Print #8, threePtOpt
                        Print #8, sClockVal
                        Print #8, gameLoc
                        Print #8, playoffOpt
                        Print #8, pbpDelay!
                        Print #8, shotPctOpt
                        Print #8, coachOpt
                        Print #8, bonusFoulNum
                        Print #8, freeThrowVal
                        Print #8, ftRulesOpt
                        Print #8, pbpOpt
                        Close #8

                    Case 1:
                        playerOpt = playerOpt + 1
                        If playerOpt > 3 Then playerOpt = 0

                    Case 2:
                        Call ChooseGameTeam(YN$, NTMS, teamNbr, gameTeams$(0), 0)
                        If teamNbr <> 0 Then Call DISABLEPLAYERS(0)

                    Case 3:
                        Call ChooseGameTeam(YN$, NTMS, teamNbr, gameTeams$(1), 1)
                        If teamNbr <> 0 Then Call DISABLEPLAYERS(1)

                    Case 4:
                        gameLoc = gameLoc + 1
                        If gameLoc > 2 Then gameLoc = 1

                    Case 5:
                        sClockVal = sClockVal + 1
                        If sClockVal > 1 Then sClockVal = 0
                        'If sClockVal > 2 Then sClockVal = 0

                    Case 6:
                        threePtOpt = threePtOpt + 1
                        If threePtOpt > 1 Then threePtOpt = 0

                    Case 7:
                        shotPctOpt = 1 - shotPctOpt

                    Case 8:
                        playoffOpt = 1 - playoffOpt

                    Case 9:
                        '

                    Case 10:
                        Do
                            Locate 12, 32
                            Input pbpDelay!
                        Loop Until pbpDelay! >= 0

                    Case 11:
                        coachOpt = 1 - coachOpt

                    Case 12:
                        bonusFoulNum = bonusFoulNum + 1
                        If bonusFoulNum > 6 Then bonusFoulNum = 4

                    Case 13:
                        freeThrowVal = 1 - freeThrowVal

                    Case 14:
                        ftRulesOpt = 1 - ftRulesOpt

                    Case 15:
                        pbpOpt = 1 - pbpOpt

                End Select

            End If

        Loop Until (iVal = 9 And gameTeams$(0) <> "" And gameTeams$(1) <> "") Or I$ = Chr$(27)

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

        Else

            'End
            'System
            Exit Sub

        End If

    End If

    '----------------------------------------
    '    AUTO-DETERMINE OTHER OPTIONS
    '----------------------------------------

    ' Determine computer team
    Select Case playerOpt

        Case 1:
            playerMode = 1
            compTeam = 0

        Case 2:
            playerMode = 1
            compTeam = 1

        Case 3:
            playerMode = 2

    End Select

    'playerMode represents the following values:
    '   0 = Human
    '   1 = Computer Team
    '   2 = PC vs. PC

    For I = 0 To 1:
        YR%(I) = GetTeamYear%(Left$(gameTeams$(I), 2))
    Next I

    Call SETUPRATINGS

    Call ChangeContributions

    Call LOADSTATS

    If autoPlay = 1 Then gameLoc = NG%(2)

    'Determine attendance
    If gameLoc = 2 Or avgAttendance&(1) = 0 Then
        'Neutral Site
        'Random # 12,000 - 22,000
        actualAttendance& = Int(Rnd(1) * 10000) + 12000
    Else
        'Average attendance +/ 5,000
        actualAttendance& = Int(Rnd(1) * 10000) + avgAttendance&(1) - 5000
    End If

    LS = 0

    Select Case sClockVal
        Case 0: shotClock = 24
        Case 1: shotClock = 30
    End Select

    If gameClock! < 6 And LT = 4 Then LS = 4

    'Determine subs / play parameters
    For P9 = 0 To 1
    
        If playerMode = 1 And P9 = compTeam Or playerMode = 2 Then
            Call COMPUTERSUBS
        Else
            Call PlayStyle
            Call PlayItSafe
        End If
        
    Next P9

    Call RATINGTABLES
    Call CheckGameClock
    Call RATINGTABLES

    250 '
    If quarter = 2 Then
        If jumpBall = 0 Then
            jumpBall = 1
        Else
            If jumpBall = 1 Then jumpBall = 0
        End If
    End If

    If quarter = 4 Then
        If jumpBall = 0 Then
            jumpBall = 1
        Else
            If quarter = 4 And jumpBall = 1 Then jumpBall = 0
        End If
    End If

    '251
    If Not (quarter < 1 And quarter > 5) Then
        'Determine random team for jump ball
        P = Int(Rnd(1) * 2)
        jumpBall = P
    End If

    '252
    If P = 0 Then D = 1
    If P = 1 Then D = 0

    prevBall$ = ""
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

    Call LastShot
    Call CheckGameClock

    If gameClock! = 0 Then 170

    Call ResetShotClock
    Call SCOREBOARD

    '----------------------------------------
    '           Tip Off!
    '----------------------------------------

    If quarter = 1 Or quarter > 4 Then
        If pbpOpt = 0 Then
            pbpString$ = gameTeams$(jumpBall) + " GETS THE JUMP"
            Call PBP
        Else
            'pbpOpt = 1
            If pbpDelay! <> 0 Then
                pbpString$ = " GETS THE JUMP"
                Call WINDEX
                Color teamRat_GAME(jumpBall, 33), teamRat_GAME(jumpBall, 34): Print gameTeams$(jumpBall);
                Color 15, 0
                Print pbpString$
                Call DELAY
            End If
        End If
    Else
        If pbpOpt = 0 Then
            pbpString$ = gameTeams$(jumpBall) + " INBOUNDS THE BALL"
            Call PBP
        Else
            'pbpOpt = 1
            If pbpDelay! <> 0 Then
                pbpString$ = " INBOUNDS THE BALL"
                Call WINDEX
                Color teamRat_GAME(jumpBall, 33), teamRat_GAME(jumpBall, 34): Print gameTeams$(jumpBall);
                Color 15, 0
                Print pbpString$
                Call DELAY
            End If
        End If
    End If

    P9 = jumpBall
    P = jumpBall

    GoTo 300

    '----- NOODLE -----

    170 '
    gameClock! = 0
    teamFouls(0) = teamFouls(0) + P2(0)
    teamFouls(1) = teamFouls(1) + P2(1)
    Call QUARTERTOTAL
    quarter = quarter + 1

    ' *** ADJUST TIMEOUTS ***
    If quarter >= 4 Then

        For PG = 0 To 1
            If timeouts(PG) > 4 Then timeouts(PG) = 4
            If quarter > 4 Then timeouts(PG) = 3
        Next

    End If

    XS = 1
    P2(0) = 0
    P2(1) = 0
    xFactor = 0

    pbpString$ = "END OF QUARTER!"

    Select Case pbpOpt
        Case 0:
            Color 7, 0
            Call WINDEX
            Print pbpString$
        Case 1:
            If pbpDelay! > 0 Then Call WINDEX: Print pbpString$ ': Call DELAY
    End Select

    If quarter = 3 Then

        pbpString$ = "HALFTIME!"

        Select Case pbpOpt
            Case 0:
                Color 7, 0
                Call WINDEX
                Print pbpString$
            Case 1:
                Call WINDEX
                Print pbpString$
                Call DELAY
        End Select

        If Not (autoPlay = 1 And playerMode = 2) Then
            I$ = GetKeyPress$
        End If

        halfTime = 1
        Call BOXOPTIONS(0)
        endGame = 0
        Call TICKER
        halfTime = 0

        For I = 0 To 1

            For I1 = 0 To 14
                If gameRatings!(I, I1, 22) = 50 Then
                    gameRatings!(I, I1, 22) = 0
                    gameRatings!(I, I1, 17) = gameRatings!(I, I1, 17) * 3
                    D1(I) = D1(I) - 1
                    MJ = 50
                End If
            Next I1

            If MJ = 50 Then
                MJ = 0
                F5%(I, 0) = 0
                For I1 = 0 To 4:
                    F5%(I, 0) = F5%(I, 0) + gameRatings!(I, gameB1!(I, I1), 17)
                Next I1
            End If

        Next I

    End If

    If quarter < 5 And playerMode <> 0 Then
        If playerMode = 1 Then
            P9 = compTeam
            Call COMPUTERSUBS
        End If

        If playerMode = 2 Then
            P9 = 0
            Call COMPUTERSUBS
            P9 = 1
            Call COMPUTERSUBS
        End If
    End If

    If quarter < 5 Then

        Call CheckGameClock

        Call SCOREBOARD

        ballCarrier = -1
        Call Substitutions
        gameClock! = 720

        Call UPDATESCREEN

        GoTo 250

    End If

    Call CheckGameClock

    Call SCOREBOARD

    If quarter = 5 Then

        pbpString$ = "END OF REGULATION"

        Select Case pbpOpt
            Case 0:
                Color 7, 0
                Call WINDEX
                Print pbpString$
            Case 1:
                If pbpDelay! > 0 Then
                    Call WINDEX
                    Print pbpString$ ': Call DELAY
                End If
        End Select

    End If

    Locate 2, 31: Print "END OF REGULATION"

    If score(0, 0) = score(1, 0) Then

        pbpString$ = "OVERTIME !!!"

        Select Case pbpOpt
            Case 0:
                Color 7, 0
                Call WINDEX: Print pbpString$
            Case 1:
                Call WINDEX: Print pbpString$
                Call DELAY
        End Select

        timeouts(0) = timeouts(0) + 1
        timeouts(1) = timeouts(1) + 1
        gameClock! = 300

        Call UPDATESCREEN
        ballCarrier = -1

        Call Substitutions

        GoTo 250

    End If

    If Not (autoPlay = 1 And playerMode = 2) Then
        I$ = GetKeyPress
    End If

    '--Start "EndOfGame" routine
    Call POSTGAME

    ' Prompt to play another game
    Cls

    If autoPlay <> 1 Then

        'Using Home Team's color for the box
        Call TEAMBOX(1, 1)

        Do
            pbpString$ = "PLAY ANOTHER GAME? (Y/N)"
            Locate 7, (80 - Len(pbpString$)) / 2
            Print pbpString$

            Color 15, 0
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    End If

    Close #11

    If UCase$(I$) = "Y" Or (autoPlay = 1 And endAllGames <> 1) Then

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

    If shotClock > 0 And sClockVal < 2 And shotClock < scSettings(sClockVal) Then
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

    If xFactor <> 90 Then xFactor = 0

    GoTo 400

    '----- NOODLE -----

    350 '

    PASS% = PASS% + 1
    P6 = 0
    A4 = 0
    D6 = 0
    A6 = 1
    F = 0
    xFactor = 0

    400 '
    A4 = 0
    D6 = 0
    P3 = 0

    'Random ball carrier
    'B = Int(Rnd(1) * 5)
    Call d100(RN0)

    If (playerMode = 0 And coachOpt = 1) Or (playerMode = 1 And D = compTeam And coachOpt = 1) Or (playerMode = 1 And P = compTeam) Or playerMode = 2 Then

        Call WHOGETSBALL

    Else

        If RN0 > 40 Then

            'random ball carrier
            ballCarrier = Int(Rnd(1) * 5)

        Else

            Do

                If pbpDelay! <> 0 Then

                    If pbpOpt = 0 Then
                        pbpString$ = "PASS TO CHOICE (0-4)"
                        Call PBP
                    Else
                        'pbpOpt = 1
                        If Not (autoPlay = 1 And playerMode = 2 Or pbpDelay! = 0) Then
                            Call WINDEX
                            Print "PASS TO CHOICE (0-4)"
                            Call PBP
                        End If
                    End If

                    Do

                        I$ = GetKeyPress$

                        Select Case UCase$(I$)

                            Case "S"
                                Call SCOUTREPORT

                            Case "T"
                                Call TICKER

                            Case "Q"
                                Exit Sub

                            Case "O"
                                Call OPTIONS

                            Case "Z", "/"
                                Call CHANGESTRATEGY(I$)

                            Case Else
                                ballCarrier = Val(I$)

                        End Select

                    Loop Until ballCarrier >= 0 And ballCarrier <= 4

                End If

            Loop Until ballCarrier >= 0 And ballCarrier <= 4

        End If

    End If


    If P1 <> 1 Then

        RN0 = Int(Rnd(1) * 1000) + 1

        ' *** BRANCH TO AREA BASED ON DEFENSE ***
        Select Case defStyles(D)

            Case 1, 4:

                ' *** SHOT VS REG/TIGHT M-M ***
                Select Case RN0

                    Case 1, 2
                        xFactor = 0
                        GoTo 580

                    Case 3 To 30
                        Call LastShot
                        Call ResetShotClock
                        Call UPDATESCREEN
                        xFactor = 88
                        GoTo 565

                    Case 31 To 30 + F7%(P, 1) + F7%(D, 2)
                        GoTo 515

                    Case 30 + F7%(P, 1) + F7%(D, 2) To 1000
                        P9 = Int(Rnd(1) * 5)

                        If RN0 <= 24 + F7%(P, 1) + F7%(D, 2) + gameRatings!(D, gameB1!(D, P9), 5) + F7%(P, 0) + OX%(offStyles(P)) Then
                            GoTo 513
                        Else
                            RN1 = Int(Rnd(1) * 1000) + 1
                            If (PASS% <= 3 Or PASS% > 4) And RN1 <= 320 + ((JY - 89) * 10) Then 445
                            If PASS% = 4 Then 445
                            GoTo 437
                        End If

                End Select

            Case 2, 5
                ' *** SHOT VS LOOSE/SAG M-M ***
                Select Case RN0

                    Case 1
                        xFactor = 0
                        GoTo 580

                    Case 2 To 10
                        Call LastShot
                        Call ResetShotClock
                        Call UPDATESCREEN
                        xFactor = 88
                        GoTo 565

                    Case 10 To 10 + F7%(P, 1) + F7%(D, 2)
                        GoTo 515

                    Case 10 + F7%(P, 1) + F7%(D, 2) To 1000

                        P9 = Int(Rnd(1) * 5)

                        If RN0 <= 8 + F7%(P, 1) + F7%(D, 2) + gameRatings!(D, gameB1!(D, P9), 5) + F7%(P, 0) + OX%(offStyles(P)) Then
                            GoTo 513
                        Else
                            RN1 = Int(Rnd(1) * 1000) + 1
                            If (PASS% <= 3 Or PASS% > 4) And RN1 <= 320 + ((JY - 89) * 10) Then 445
                            If PASS% = 4 Then 445
                            GoTo 437
                        End If

                End Select

            Case 3
                ' *** SHOT VS TRAP M-M ***
                Select Case RN0

                    Case 1 To 3
                        xFactor = 0
                        GoTo 580

                    Case 4 To 50
                        Call LastShot
                        Call ResetShotClock
                        Call UPDATESCREEN
                        xFactor = 88

                        GoTo 565

                    Case 50 To 50 + F7%(P, 1) + F7%(D, 2)
                        GoTo 515

                    Case 50 + F7%(P, 1) + F7%(D, 2) To 1000
                        P9 = Int(Rnd(1) * 5)

                        If RN0 <= 40 + F7%(P, 1) + F7%(D, 2) + gameRatings!(D, gameB1!(D, P9), 5) + F7%(P, 0) + OX%(offStyles(P)) Then
                            GoTo 513
                        Else
                            RN1 = Int(Rnd(1) * 1000) + 1
                            If (PASS% <= 3 Or PASS% > 4) And RN1 <= 320 + ((JY - 89) * 10) Then 445
                            If PASS% = 4 Then 445
                            GoTo 437
                        End If

                End Select

        End Select

    Else

        P1 = 0
        RN0 = Int(Rnd(1) * 1000) + 1

        Select Case RN0

            Case 1 To 3
                xFactor = 0
                GoTo 580

            Case 4 To 50
                Call LastShot
                Call ResetShotClock
                Call UPDATESCREEN
                xFactor = 88
                GoTo 565

            Case 50 To 50 + F7%(P, 1) + F7%(D, 2)
                GoTo 515

            Case 50 + F7%(P, 1) + F7%(D, 2) To 1000
                P9 = Int(Rnd(1) * 5)

                If RN0 <= 40 + F7%(P, 1) + F7%(D, 2) + gameRatings!(D, gameB1!(D, P9), 5) + F7%(P, 0) Then
                    GoTo 513
                Else
                    RN1 = Int(Rnd(1) * 1000) + 1
                    If (PASS% <= 3 Or PASS% > 4) And RN1 <= 320 + ((JY - 89) * 10) Then 445
                    If PASS% = 4 Then 445

                    GoTo 437

                End If

        End Select

    End If


    '----- NOODLE -----

    ' ***BAD SHOT ***

    437 '

    A4 = 0
    RN0 = Int(Rnd(1) * 3) + 1

    If gameRatings!(P, gameB1!(P, ballCarrier), 0) > 60 Then

        Select Case RN0
            Case 1
                F = 24
            Case 2
                F = 30
            Case 3
                F = 36
        End Select

    Else

        Select Case RN0
            Case 1
                F = gameRatings!(P, gameB1!(P, ballCarrier), 0) * .4
            Case 2
                F = gameRatings!(P, gameB1!(P, ballCarrier), 0) * .5
            Case 3
                F = gameRatings!(P, gameB1!(P, ballCarrier), 0) * .6
        End Select

    End If

    PZ% = 1
    xFactor = 0

    GoTo 520

    '----- NOODLE -----

    445 '

    If Not (quarter = 4 And gameClock! <= 120 Or quarter > 4) Then
        If gameRatings!(P, gameB1!(P, ballCarrier), 3) - gameRatings!(P, gameB1!(P, ballCarrier), 7) - gameRatings!(P, gameB1!(P, ballCarrier), 11) - gameRatings!(P, gameB1!(P, ballCarrier), 12) <= 0 Then 437
    End If

    ' *** CHECKING 3-POINT SHOT ***
    If threePtOpt <> 0 Then

        F3S% = 0

        Call d100(RN0)

        If RN0 <= gameRatings!(P, gameB1!(P, ballCarrier), 19) Then
            PZ% = 4
            F3S% = gameRatings!(P, gameB1!(P, ballCarrier), 20) + F7%(D, 4)
            'Adjust shot chance by -3 to 3
            F3S% = F3S% + Int(Rnd(1) * 7) - 3
            xFactor = 0

            GoTo 520

        End If

        If P6 = 4 Then

            Call d100(RN1)

            If RN0 <= 20 Then
                GoTo 437
            Else
                PZ% = 4
                xFactor = 0
                F3S% = .4 * gameRatings!(P, gameB1!(P, ballCarrier), 20) + F7%(D, 4)
                GoTo 520
            End If

        Else

            If quarter >= 4 And gameClock! < 30 And score(D, 0) - score(P, 0) > 3 And gameRatings!(P, gameB1!(P, ballCarrier), 19) >= 3 Then
                PZ% = 4
                xFactor = 0
                F3S% = gameRatings!(P, gameB1!(P, ballCarrier), 20) + F7%(D, 4)
                GoTo 520
            End If

        End If

    End If

    ' *** CHECKING FOR ASSISTED SHOT ***

    ' *** GET SHOT PCT ***
    F = gameRatings!(P, gameB1!(P, ballCarrier), 0) + D1(D)

    If gameLoc = P Then
        F = F + 2
    Else
        If gameLoc <> 2 Then F = F - 2
    End If

    A4 = Int(Rnd(1) * 41)
    RN0 = Int(Rnd(1) * F5%(P, 4)) + 1
    RN1 = Int(.17 * F5%(P, 4) + .5)

    If RN1 >= RN0 Then
        F = F + A4
        PZ% = 2
        xFactor = 0
    Else

        ' *** CHECK PENETRATION SHOT ***
        Call d100(RN2)
        A4 = Int(Rnd(1) * 50) + 1

        If RN2 <= (plyrRat_GAME(P, gameB1!(P, ballCarrier), 16) + OY%(offStyles(P))) * 2 + 1 Then

            PZ% = 3
            F = F + A4
            xFactor = 0

        Else

            ' *** CHECK AVERAGE SHOT ***
            I2 = plyrRat_GAME(P, gameB1!(P, ballCarrier), 16)
            rndTeam = Int(Rnd(1) * 2)

            If Left$(players_GAME$(P, gameB1!(P, ballCarrier), 1), 1) = "G" Or Left$(players_GAME$(P, gameB1!(P, ballCarrier), 1), 1) = "F" Then

                'For a Guard or Forward
                R2 = Int(Rnd(1) * 3)

                Select Case R2
                    Case 1: PZ% = 6
                    Case 2: PZ% = 7
                    Case 3: PZ% = 5
                End Select

            Else

                If rndTeam = 1 Then
                    PZ% = 6
                Else
                    PZ% = 5
                End If

            End If

            F = F + GF%(rndTeam, I2)
            xFactor = 0

        End If

    End If

    GoTo 520

    '----- NOODLE -----

    '-- Without the GoTo 300, this could be a subroutine
    '-- Perhaps it still could be if we add the GoTo 300 after each call
    513 '

    Call LastShot
    Call ResetShotClock

    gameRatings!(P, gameB1!(P, ballCarrier), 23) = gameRatings!(P, gameB1!(P, ballCarrier), 23) + 1

    If pbpDelay! <> 0 Then

        If pbpOpt = 1 Then
            Call WINDEX
            Print "BALL STOLEN BY "; players_GAME$(D, gameB1!(D, P9), 0); "!"
            Call DELAY
        Else
            Call d100(RN0)

            Select Case RN0
                Case 1 To 3: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " QUICKLY REACHES IN!"
                Case 4 To 6: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " SNEAKS IN FROM BEHIND!"
                Case 7 To 10: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " GETS A HAND ON THE BALL!"
            End Select

            Call PBP
            RN0 = Int(Rnd(1) * 20) + 1

            Select Case RN0
                Case 1: pbpString$ = "PASS INTERCEPTED BY " + players_GAME$(D, gameB1!(D, P9), 0) + "!"
                Case 2: pbpString$ = "PASS IS PICKED OFF BY " + players_GAME$(D, gameB1!(D, P9), 0) + "!"
                Case 3: pbpString$ = "BALL IS STOLEN BY " + players_GAME$(D, gameB1!(D, P9), 0) + "!"
                Case 4: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " WITH THE STEAL!"
                Case 5: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " TAKES IT AWAY!"
                Case 6: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " STEALS IT!"
                Case 7: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " RIPS IT LOOSE!"
                Case 8: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " TAKES THE BALL AWAY!"
                Case 9: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " STEALS THE BALL!"
                Case 10: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " PICKS IT OFF!"
                Case 11: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " SWIPES THE BALL!"
                Case 12: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " WITH THE STOLEN BALL!"
                Case 13: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " STRIPS IT AWAY!"
                Case 14: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " WITH THE STEAL!"
                Case 15: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " SWIPES AND STEALS IT!"
                Case 16: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " SNATCHES IT AWAY!"
                Case 17: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " QUICKLY REACHES IN!"
                Case 18: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " TAKES IT THE OTHER WAY!"
                Case 19: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " POKES IT LOOSE!"
                Case 20: pbpString$ = players_GAME$(D, gameB1!(D, P9), 0) + " STRIPS IT CLEAN!"
            End Select

            Call PBP

        End If

    End If

    gameRatings!(D, gameB1!(D, P9), 13) = gameRatings!(D, gameB1!(D, P9), 13) + 1
    turnovers(P) = turnovers(P) + 1

    Call UPDATESCREEN

    C1 = 2
    P9 = D

    Call CheckGameClock
    If gameClock! = 0 Then 170

    prevBall$ = ""

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

    fastBreak = 1

    GoTo 300

    '----- NOODLE -----

    515 '

    Call LastShot
    Call ResetShotClock

    gameRatings!(P, gameB1!(P, ballCarrier), 23) = gameRatings!(P, gameB1!(P, ballCarrier), 23) + 1
    turnovers(P) = turnovers(P) + 1

    If SV% <> 1 Then
        PT% = 1
        Call MAKEPBP
    End If

    SV% = 0

    If pbpOpt = 1 And pbpDelay! <> 0 Then
        Call WINDEX
        Print "TURNOVER BY ";
        Color teamRat_GAME(P, 33), teamRat_GAME(P, 34): Print gameTeams$(P)
        Call DELAY
        Color 15, 0
    End If

    If pbpOpt = 0 Then Call PBP
    pbpString$ = "BALL OUT OF BOUNDS TO " + gameTeams$(D)

    If pbpOpt = 0 Then
        Call PBP
    Else
        'pbpOpt = 1
        If pbpDelay! <> 0 Then Print pbpString$: Call DELAY
    End If

    Call DELAY
    Call UPDATESCREEN

    Locate 10, 1

    Call CheckGameClock
    If gameClock! = 0 Then 170

    'GoSub 3000
    'AKA ChangePosession
    prevBall$ = ""
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
    Call CheckGameClock
    If gameClock! = 0 Then 170

    If PZ% = 4 And F3S% <= 0 Then F3S% = 3

    If pbpDelay! <> 0 Then

        If F > 70 And fastBreak = 1 And PASS% = 1 Then
            PT% = 6
            PZ% = 8
            Call MAKEPBP
            Call PBP
        Else

            If players_GAME$(P, gameB1!(P, ballCarrier), 0) <> prevBall$ Then
                If prevBall$ = "" Then
                    pbpString$ = "PASS TO " + players_GAME$(P, gameB1!(P, ballCarrier), 0)
                Else
                    PT% = 10
                    Call MAKEPBP
                End If
            Else
                'players_GAME$(P, gameB1!(P, ballCarrier), 0) = prevBall$
                PT% = 9
                Call MAKEPBP
            End If

            Call PBP

            If shotClock <= 0 And sClockVal < 2 Then
                pbpString$ = "SHOT CLOCK HAS EXPIRED!"
                SV% = 1
                GoTo 515
            End If

            If shotClock < 5 And sClockVal < 2 Then
                pbpString$ = "SHOT CLOCK DOWN TO" + Str$(shotClock)
                Call PBP
            End If

            prevBall$ = players_GAME$(P, gameB1!(P, ballCarrier), 0)
            Z3$ = Left$(players_GAME$(P, gameB1!(P, ballCarrier), 1), 1)
            Y2$ = "HE"
            Y3$ = players_GAME$(D, gameB1!(D, ballCarrier), 0)

            'On PZ% GOTO 521, 522, 523, 524, 525, 526, 527

            Select Case PZ%

                Case 1:
                    If pbpDelay! <> 0 Then
                        RN0 = Int(Rnd(1) * 12) + 1

                        Select Case RN0
                            Case 1: pbpString$ = Y2$ + "'S HEAVILY GUARDED"
                            Case 2: pbpString$ = Y2$ + "'S GUARDED"
                            Case 3: pbpString$ = Y2$ + "'S GUARDED BY " + Y3$
                            Case 4: pbpString$ = Y2$ + " HAS A MAN ON HIM"
                            Case 5: pbpString$ = Y2$ + " HAS A DEFENDER ON HIM"
                            Case 6: pbpString$ = Y2$ + "'S COVERED"
                            Case 7: pbpString$ = Y2$ + " CAN'T GET AROUND THE DEFENDER"
                            Case 8: pbpString$ = Y2$ + "'S PRESSURED"
                            Case 9: pbpString$ = Y2$ + " HAS NOWHERE TO GO"
                            Case 10: pbpString$ = Y2$ + " HAS A HAND IN HIS FACE"
                            Case 11: pbpString$ = Y2$ + "'S WATCHED BY " + Y3$
                            Case 12: pbpString$ = Y2$ + "'S WORKING ON " + Y3$
                        End Select
                    End If

                    Call PBP

                Case 2:
                    pbpString$ = Y2$ + " WORKS FREE INSIDE"
                    Call PBP
                    pbpString$ = "HE HAS AN OPEN SHOT"
                    Call PBP

                Case 3:
                    If pbpDelay! <> 0 Then

                        Call d100(RN0)

                        If Z3$ = "C" Then

                            Select Case RN0
                                Case 1 To 25: pbpString$ = Y2$ + " MAKES A MOVE DOWN LOW":
                                Case 26 To 50: pbpString$ = Y2$ + " POSITIONS HIMSELF DOWN LOW"
                                Case 51 To 75: pbpString$ = Y2$ + " POSTS UP UNDERNEATH"
                                Case 76 To 100: pbpString$ = Y2$ + " BACKS IN AGAINST " + players_GAME$(D, gameB1!(D, 4), 0)
                            End Select

                            Call PBP
                            pbpString$ = "HE HAS A GOOD SHOT INSIDE"
                            Call PBP

                        End If

                        If Z3$ = "F" Then
                            Select Case RN0
                                Case 1 To 17: pbpString$ = Y2$ + " DRIVES THE BASELINE"
                                Case 18 To 34: pbpString$ = Y2$ + " PENETRATES THE LANE"
                                Case 35 To 50: pbpString$ = Y2$ + " MOVES INSIDE"
                                Case 51 To 67: pbpString$ = Y2$ + " DRIVES THE LANE"
                                Case 68 To 84: pbpString$ = Y2$ + " MOVES INTO THE PAINT"
                                Case 85 To 100: pbpString$ = Y2$ + " DRIVES UNDERNEATH"
                            End Select

                            Call PBP
                            pbpString$ = "HE'S GOT A GOOD SHOT UNDERNEATH"
                            Call PBP
                        End If

                        If Z3$ = "G" Then
                            Select Case RN0
                                Case 1 To 25: pbpString$ = Y2$ + " DRIVES THE LANE": Call PBP: pbpString$ = "HE HAS A GOOD SHOT CLOSE IN"
                                Case 26 To 50: pbpString$ = Y2$ + " DRIVES THROUGH THE KEY": Call PBP: pbpString$ = "HE'S INTO THE LANE": Call PBP: pbpString$ = "HE HAS A GOOD SHOT CLOSE IN"
                                Case 51 To 75: pbpString$ = Y2$ + " DRIVES TO THE BASKET": Call PBP: pbpString$ = "HE HAS A GOOD SHOT CLOSE IN"
                            End Select

                            Call PBP
                        Else
                            pbpString$ = Y2$ + " DRIVES THROUGH AN OPENING"
                            Call PBP
                        End If

                    End If

                Case 4:
                    pbpString$ = "HE'S OPEN FROM 3-POINT RANGE!"
                    Call PBP

                Case 5:
                    pbpString$ = "HE'S OPEN FROM CLOSE RANGE"
                    Call PBP

                Case 6:
                    pbpString$ = "HE'S OPEN FROM MEDIUM RANGE"
                    Call PBP

                Case 7:
                    pbpString$ = "HE'S OPEN FROM LONG RANGE"
                    Call PBP

            End Select

        End If

    End If

    If F <= 0 Then F = 1
    If F >= 100 Then F = 100

    If pbpDelay! <> 0 Then

        If pbpOpt = 1 Then
            Call WINDEX
            Print "PASS TO "; players_GAME$(P, gameB1!(P, ballCarrier), 0); " OF ";
            Color teamRat_GAME(P, 33), teamRat_GAME(P, 34)
            Print gameTeams$(P)
            Color 15, 0
        End If

        If pbpOpt = 1 Then
            If PZ% <> 4 Then
                Locate 10: Print "SHOT % "; Int(F + .5);
            Else
                Locate 10: Print "3 PT SHOT % "; Int(F3S% + .5);
                Print " *";
                GoTo 528
            End If
        End If

        If pbpOpt = 1 And PZ% >= 2 And PZ% <= 8 Then Print "*";

        If pbpDelay! <> 0 And pbpOpt <> 1 Then

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

    If pbpOpt = 1 And pbpDelay! <> 0 Then Call DELAY

    If playerMode <> 0 Or coachOpt = 1 Then

        If quarter >= 4 And gameClock! <= 240 And score(P, 0) - score(D, 0) > 0 And playerMode = 1 And P = compTeam Then

            '985
            pbpString$ = Str$(gameRatings!(P, gameB1!(P, ballCarrier), 1)) + " FT%  INTENTIONAL FOUL? (Y/N)"

            Do
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "3"

            If UCase$(I$) = "Y" Or I$ = "3" Then P6 = 3: GoTo 532

        Else

            If playerMode = 2 Or playerMode = 1 And D = compTeam Then

                '980
                If gameClock! <= 240 And quarter >= 4 And score(P, 0) - score(D, 0) > 10 Then
                    If (playerMode = 1 Or playerMode = 0) And coachOpt <> 1 Then 530
                Else
                    If gameClock! <= 30 And quarter >= 4 And score(P, 0) - score(D, 0) > 0 Then P6 = 3: GoTo 532
                    If (playerMode = 1 Or playerMode = 0) And coachOpt <> 1 Then 530
                End If

            End If
        End If

        If gameClock! <= 1 Or shotClock <= 6 Then
            P6 = 1
        Else
            If gameClock! <= 8 And score(P, 0) - score(D, 0) <= -3 And quarter >= 4 Then
                P6 = 1
            Else
                If gameClock! <= 20 And score(P, 0) - score(D, 0) <= -5 And quarter >= 4 Then
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

    Select Case UCase$(I$)

        Case "S"
            Call SCOUTREPORT
            GoTo 530

        Case "T"
            Call TICKER
            GoTo 530

        Case "Q"
            Exit Sub

        Case "O"
            Call OPTIONS
            GoTo 530

        Case "Z", "/"
            Call CHANGESTRATEGY(I$)
            GoTo 530

        Case Else
            P6 = Val(I$)

    End Select

    'Is this for choosing a designated fouler?
    'If so then we need to update for looping logic
    If P6 < 1 Or P6 > 4 Then P6 = 0
    If P6 = 4 Then FT% = 1

    '----------------------------------------------------
    '--- THIS IS CRITCAL FOR PBP MOVING AUTOMATICALLY ---
    '----------------------------------------------------
    ' essentially it allows the user to press a key
    ' during game play to activate certain functions
    ' or special menus, etc.
    532 '
    I$ = InKey$

    Select Case UCase$(I$)

        Case "S"
            Call SCOUTREPORT

        Case "T"
            Call TICKER

        Case "Q"
            Exit Sub

        Case "O"
            Call OPTIONS

        Case "Z", "/"
            Call CHANGESTRATEGY(I$)

    End Select

    If Val(I$) = 3 And (playerMode = 0 Or playerMode = 1 And P = compTeam) And quarter >= 4 And gameClock! < 240 Then P6 = 3
    If Val(I$) = 2 And (playerMode = 0 Or playerMode = 1 And D = compTeam) And timeouts(P) > 0 Then P6 = 2

    P3 = P6

    If P6 = 2 And timeouts(P) = 0 Then 530
    If P6 = 3 And quarter < 4 Or P6 = 3 And playerMode = 1 And D = compTeam Then 530
    If P6 = 3 And quarter = 4 And gameClock! > 240 Then 530
    If (P6 = 1 Or P6 = 4) And PZ% = 4 Then P6 = 4

    caseVar = P6 + 1

    Select Case caseVar

        Case 1
            'Pass!
            GoTo 350

        Case 2
            'Shoot it!
            GoTo 537

        Case 3
            'Timeout
            640 '
            If timeouts(P) > 0 Then
                timeouts(P) = timeouts(P) - 1
                gameClock! = gameClock! + M5
                timeElapsed! = -M5
                Call AdjustPlayTime
                shotClock = shotClock + M5
                pbpString$ = "TIMEOUT " + gameTeams$(P):

                Call PBP
                Call DELAY
                Call Substitutions
            End If

            If A6 = 0 Then

                If pbpDelay! <> 0 Then
                    For I = 8 To 15
                        Locate I, 1
                        Print Space$(38)
                    Next I
                End If

                Locate 8, 1

                If fullCtOpt(D) = 1 Then
                    If pbpOpt = 0 Then
                        pbpString$ = gameTeams$(D) + " IS PRESSING"
                        Call PBP
                    Else
                        'pbpOpt = 1
                        If pbpDelay! <> 0 Then
                            Call WINDEX
                            Color teamRat_GAME(D, 33), teamRat_GAME(D, 34): Print gameTeams$(D);
                            Color 15, 0
                            Print " IS PRESSING"
                            Call DELAY
                        End If
                    End If
                    P1 = 1
                End If

                GoTo 300

            Else

                GoTo 350

            End If


        Case 4
            'Intentional Foul
            584 '
            IN = 99

            Call LastShot
            Call CheckGameClock
            If gameClock! = 0 Then 170

            C1 = 2
            P9 = D
            pbpString$ = "DESIGNATED FOULER (0-4)"
            Call SELECTPBP

            If playerMode = 1 And D = compTeam Or playerMode = 2 Then
                fouler = Int(Rnd(1) * 5)
            Else

                Do
                    I$ = GetKeyPress$
                    fouler = Val(I$)
                Loop Until fouler >= 0 And fouler <= 4
            End If

            Call Fouls(1)

            Select Case sClockVal
                Case 0: shotClock = 24
                Case 1: shotClock = 30
            End Select

            GoTo 566

        Case 5
            359 '
            Call d100(S)
            W%(P, gameB1!(P, ballCarrier), 1) = W%(P, gameB1!(P, ballCarrier), 1) + 1
            gameRatings!(P, gameB1!(P, ballCarrier), 7) = gameRatings!(P, gameB1!(P, ballCarrier), 7) + 1
            PT% = 3

            Call MAKEPBP
            Call PBP

            If S <= F3S% Then

                ' MADE 3-POINT FIELD GOAL

                If pbpOpt = 1 And pbpDelay! <> 0 Then
                    Call WINDEX
                    Print "3 PT FG GOOD BY "; players_GAME$(P, gameB1!(P, ballCarrier), 0); " !!!"
                    Call DELAY
                End If

                If pbpOpt = 0 Then
                    PT% = 4
                    Call MAKEPBP
                    Call PBP
                End If

                Call DELAY

                score(P, quarter) = score(P, quarter) + 3
                score(P, 0) = score(P, 0) + 3
                W%(P, gameB1!(P, ballCarrier), 0) = W%(P, gameB1!(P, ballCarrier), 0) + 1
                gameRatings!(P, gameB1!(P, ballCarrier), 8) = gameRatings!(P, gameB1!(P, ballCarrier), 8) + 1

                Call CreditAssists
                Call UPDATESCREEN

                GoTo 562

            End If

            If pbpOpt = 1 And pbpDelay! <> 0 Then
                Call WINDEX
                Print "3 PT FG MISSED BY "; players_GAME$(P, gameB1!(P, ballCarrier), 0)
                Call DELAY
            End If

            If pbpOpt = 0 Then
                PT% = 5
                Call MAKEPBP
                Call PBP
            End If

            Call ResetShotClock

            gameClock! = gameClock! - 4
            timeElapsed! = 4

            Call AdjustPlayTime
            Call CheckGameClock
            If gameClock! = 0 Then 170

            Call UPDATESCREEN

            GoTo 670

    End Select

    '--- technically this branch ends here ---

    537 '
    ' *** SHOOT !!! ***

    PT% = 3
    Call MAKEPBP
    Call PBP

    S = 0
    Call d100(RN0)
    gameRatings!(P, gameB1!(P, ballCarrier), 7) = gameRatings!(P, gameB1!(P, ballCarrier), 7) + 1

    If Not (quarter > 3 And gameClock! <= 30 And score(D, 0) - score(P, 0) = 2 Or quarter > 3 And gameClock! <= 30 And score(D, 0) - score(P, 0) = 0) Then

        If RN0 <= gameRatings!(P, gameB1!(P, ballCarrier), 16) + F7%(D, 3) Then

            gameRatings!(P, gameB1!(P, ballCarrier), 7) = gameRatings!(P, gameB1!(P, ballCarrier), 7) - 1
            Call ResetShotClock

            589 '
            gameClock! = gameClock! - 4
            timeElapsed! = 4

            Call AdjustPlayTime
            Call UPDATESCREEN
            Call Fouls(0)

            If pbpDelay! <> 0 Then

                If pbpOpt = 0 Then
                    If freeThrowVal = 1 And P2(D) > bonusFoulNum And quarter <= 4 Or P2(D) > 3 And quarter > 4 Then
                        pbpString$ = "3 TO MAKE 2 FT: " + players_GAME$(P, gameB1!(P, ballCarrier), 0) + Str$(gameRatings!(P, gameB1!(P, ballCarrier), 1)) + " %"
                        Call PBP
                    Else
                        pbpString$ = "2 FT: " + players_GAME$(P, gameB1!(P, ballCarrier), 0) + Str$(gameRatings!(P, gameB1!(P, ballCarrier), 1)) + " %"
                        Call PBP
                    End If
                Else
                    'pbpOpt = 1
                    Call WINDEX
                    Print gameTeams$(P)

                    If freeThrowVal = 1 And P2(D) > bonusFoulNum And quarter <= 4 Or P2(D) > 3 And quarter > 4 Then
                        Print "3 TO MAKE 2 FT: ";
                        Print players_GAME$(P, gameB1!(P, ballCarrier), 0); "   "; gameRatings!(P, gameB1!(P, ballCarrier), 1); " %"
                    Else
                        Print "2 FT: ";
                        Print players_GAME$(P, gameB1!(P, ballCarrier), 0); "   "; gameRatings!(P, gameB1!(P, ballCarrier), 1); " %"
                    End If
                End If

            End If

            GoTo 591

        End If

    End If

    '540
    Call d100(S)
    RN0 = (F - 2) * .1

    '--- Shot Branch 1 ---
    If S <= RN0 Then

        Call ResetShotClock

        gameClock! = gameClock! - 4
        timeElapsed! = 4

        Call AdjustPlayTime
        Call UPDATESCREEN

        If DK% = 1 Then
            If pbpOpt = 0 Then
                PT% = 2

                Call MAKEPBP
                pbpString$ = "STUFF BY " + players_GAME$(P, gameB1!(P, ballCarrier), 0)
                Call PBP

                DK% = 0
                GoTo 617
            Else
                'pbpOpt = 1
                Call WINDEX
                Print "STUFF BY " + players_GAME$(P, gameB1!(P, ballCarrier), 0)
                Call DELAY
                DK% = 0
                GoTo 617
            End If
        End If

        If PZ% = 8 And pbpOpt = 0 Then
            PT% = 7
            Call MAKEPBP
            PZ% = 0
            Call PBP
            GoTo 617
        End If

        If pbpOpt = 0 Then
            pbpString$ = "BASKET GOOD BY " + players_GAME$(P, gameB1!(P, ballCarrier), 0)
            Call PBP
        Else
            'pbpOpt = 1
            If pbpDelay! <> 0 Then
                Call WINDEX
                Print "BASKET GOOD BY " + players_GAME$(P, gameB1!(P, ballCarrier), 0)
                Call DELAY
            End If
        End If

        617 '
        pbpString$ = "... AND HE'S FOULED !!!"

        If pbpOpt = 0 Then
            Call PBP
        Else
            'pbpOpt = 1
            If pbpDelay! <> 0 Then
                Print pbpString$
                Call DELAY
            End If
        End If

        F3 = 1

        Call DELAY
        Call CreditAssists

        F3 = 1
        score(P, quarter) = score(P, quarter) + 2
        score(P, 0) = score(P, 0) + 2
        gameRatings!(P, gameB1!(P, ballCarrier), 8) = gameRatings!(P, gameB1!(P, ballCarrier), 8) + 1

        Call UPDATESCREEN
        Call Fouls(0)

        If pbpDelay! <> 0 Then

            If pbpOpt = 0 Then
                If freeThrowVal = 1 And P2(D) > bonusFoulNum And quarter <= 4 Or P2(D) > 3 And quarter > 4 Then
                    pbpString$ = "2 TO MAKE 1 FT: " + players_GAME$(P, gameB1!(P, ballCarrier), 0) + Str$(gameRatings!(P, gameB1!(P, ballCarrier), 1)) + " %"
                    Call PBP
                Else
                    pbpString$ = "1 FT: " + players_GAME$(P, gameB1!(P, ballCarrier), 0) + Str$(gameRatings!(P, gameB1!(P, ballCarrier), 1)) + " %"
                    Call PBP
                End If
            Else
                'pbpOpt = 1
                If freeThrowVal = 1 And P2(D) > bonusFoulNum And quarter <= 4 Or P2(D) > 3 And quarter > 4 Then
                    Call WINDEX
                    Print gameTeams$(P)
                    pbpString$ = "2 TO MAKE 1 FT: " + players_GAME$(P, gameB1!(P, ballCarrier), 0) + "   " + Str$(gameRatings!(P, gameB1!(P, ballCarrier), 1)) + " %"

                Else
                    Call WINDEX
                    Print gameTeams$(P)
                    Print "1 FT: "; players_GAME$(P, gameB1!(P, ballCarrier), 0); "   "; gameRatings!(P, gameB1!(P, ballCarrier), 1); " %"
                End If

            End If

        End If

        '620
        xFactor = 50
        Call Substitutions
        xFactor = 0

        If pbpDelay! <> 0 Then

            If pbpOpt = 0 Then
                If freeThrowVal = 1 And P2(D) > bonusFoulNum And quarter <= 4 Or P2(D) > 3 And quarter > 4 Then
                    pbpString$ = "2 TO MAKE 1 FT: " + players_GAME$(P, gameB1!(P, ballCarrier), 0) + Str$(gameRatings!(P, gameB1!(P, ballCarrier), 1)) + " %"
                Else
                    pbpString$ = "1 FT: " + players_GAME$(P, gameB1!(P, ballCarrier), 0) + Str$(gameRatings!(P, gameB1!(P, ballCarrier), 1)) + " %"
                End If
                Call PBP
            End If

            If pbpOpt = 1 Then
                Call WINDEX
                Print gameTeams$(P)
                If freeThrowVal = 1 And P2(D) > bonusFoulNum And quarter <= 4 Or P2(D) > 3 And quarter > 4 Then
                    Print "2 TO MAKE 1 FT: ";
                    Print players_GAME$(P, gameB1!(P, ballCarrier), 0); "   "; gameRatings!(P, gameB1!(P, ballCarrier), 1); " %"
                Else
                    Print "1 FT: "; players_GAME$(P, gameB1!(P, ballCarrier), 0); "   "; gameRatings!(P, gameB1!(P, ballCarrier), 1); " %"
                End If

            End If

        End If

        NM% = 0

        'Two FT Attempts
        For F3 = 1 To 2

            'Increase FT ATT
            gameRatings!(P, gameB1!(P, ballCarrier), 9) = gameRatings!(P, gameB1!(P, ballCarrier), 9) + 1

            Call d100(S)

            'FT Success Chance
            FX% = gameRatings!(P, gameB1!(P, ballCarrier), 1)

            If S <= FX% Then

                If pbpOpt = 0 Then
                    NM% = NM% + 1
                    Call THREEFT
                Else
                    'pbpOpt = 1
                    NM% = NM% + 1
                    Call MADEFT
                End If
            Else
                'S > FX% And
                If pbpOpt = 0 Then
                    Call BADFT
                Else
                    'pbpOpt = 1
                    Call MISSFT
                End If

            End If

            If NM% = 1 Then Exit For
            If F3 = 1 And (freeThrowVal = 0 Or freeThrowVal = 1 And (P2(D) <= bonusFoulNum And quarter <= 4 Or P2(D) <= 3 And quarter > 4)) Then Exit For

        Next F3

        Call CheckGameClock
        If gameClock! = 0 Then 170

        xFactor = 99

        If S > FX% Then 670

        'GoSub 3000
        'AKA ChangePosession
        prevBall$ = ""

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

        pbpString$ = gameTeams$(P) + " BALL AFTER SCORE"
        Call SELECTPBP
        Locate 8, 41: Print Space$(12)
        Call Choices

        ballCarrier = 6
        xFactor = 0

        If pbpDelay! <> 0 Then
            For I = 8 To 15
                Locate I, 1
                Print Space$(38)
            Next I
        End If

        Locate 8, 1

        'PRESSING
        If fullCtOpt(D) = 1 Then
            If pbpOpt = 0 Then
                pbpString$ = gameTeams$(D) + " IS PRESSING"
                Call PBP
            End If

            If pbpOpt = 1 And pbpDelay! <> 0 Then
                Call WINDEX
                Color teamRat_GAME(D, 33), teamRat_GAME(D, 34): Print gameTeams$(D);
                Color 15, 0: Print " IS PRESSING"
                Call DELAY
            End If

            P1 = 1
        End If

        GoTo 300

    End If

    '--- Shot Branch 2 ---
    If S <= F - 2 Then

        If DK% = 1 Then
            If pbpOpt = 0 Then
                PT% = 2
                Call MAKEPBP
                pbpString$ = "STUFF BY " + players_GAME$(P, gameB1!(P, ballCarrier), 0) + " !!!"
                DK% = 0
            Else
                'pbpOpt = 1
                pbpString$ = "STUFF BY " + players_GAME$(P, gameB1!(P, ballCarrier), 0) + " !!!"
                DK% = 0
            End If
        Else
            If pbpOpt = 0 And PZ% = 8 Then
                PT% = 7
                Call MAKEPBP
                Call PBP
                PZ% = 0
                skipShotPBP% = 1
            Else
                pbpString$ = "BASKET BY "
                pbpString$ = pbpString$ + players_GAME$(P, gameB1!(P, ballCarrier), 0) + " !!!"
            End If
        End If

        If skipShotPBP% = 0 Then

            If pbpOpt = 1 And pbpDelay! <> 0 Then
                Call WINDEX: Print pbpString$: Call DELAY
            Else

                Call d100(RN0)

                If RN0 > 50 Then

                    RN1 = Int(Rnd(1) * 22) + 1

                    Select Case RN1
                        Case 1: pbpString$ = "NOTHING BUT THE BOTTOM OF THE NET!"
                        Case 2: pbpString$ = "Count IT!"
                        Case 3: pbpString$ = players_GAME$(P, gameB1!(P, ballCarrier), 0) + " TICKLES THE TWINE!"
                        Case 4: pbpString$ = "NO RIM, NO BOARD, JUST CORD!"
                        Case 5: pbpString$ = "GOOD!"
                        Case 6: pbpString$ = "YES!"
                        Case 7: pbpString$ = "COUNT IT!"
                        Case 8: pbpString$ = "IT'S GOOD!"
                        Case 9: pbpString$ = "BINGO!"
                        Case 10: pbpString$ = "IT FALLS!"
                        Case 11: pbpString$ = "HE DRAINS IT!"
                        Case 12: pbpString$ = "SWISH!"
                        Case 13: pbpString$ = "ALL NET!"
                        Case 14: pbpString$ = "IT'S THERE!"
                        Case 15: pbpString$ = "HE BURIES IT!"
                        Case 16: pbpString$ = "IT GOES IN!"
                        Case 17: pbpString$ = "RING IT UP!"
                        Case 18: pbpString$ = "IT GOES DOWN!"
                        Case 19: pbpString$ = "BULLSEYE!"
                        Case 20: pbpString$ = "HE SCORES!"
                        Case 21: pbpString$ = "HE HAS IT!"
                        Case 22: pbpString$ = "HE CONNECTS!"
                    End Select

                End If

                If pbpOpt = 0 Then Call PBP

            End If

        End If

        560 '
        score(P, quarter) = score(P, quarter) + 2
        score(P, 0) = score(P, 0) + 2

        Call CreditAssists

        Call UPDATESCREEN

        gameRatings!(P, gameB1!(P, ballCarrier), 8) = gameRatings!(P, gameB1!(P, ballCarrier), 8) + 1

        '---> possibly a new sub
        562 '
        pbpString$ = gameTeams$(D) + " BALL AFTER SCORE"

        If pbpOpt = 1 And pbpDelay! <> 0 Then
            Locate 9, 1
            Color teamRat_GAME(D, 33), teamRat_GAME(D, 34)
            Print gameTeams$(D);: Color 15, 0
            Print " BALL AFTER SCORE"
            Call DELAY
        End If

        If pbpOpt = 0 Then
            Locate 8, 41: Print Space$(12)
            Call PBP
        End If

        Call ResetShotClock
        gameClock! = gameClock! - 4
        timeElapsed! = 4

        Call AdjustPlayTime
        Call CheckGameClock

        If gameClock! = 0 Then 170

        Call UPDATESCREEN

        'GoSub 3000
        'AKA ChangePosession
        prevBall$ = ""
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

        Call Choices

        If pbpDelay! <> 0 Then

            For I = 8 To 15
                Locate I, 1
                Print Space$(38)
            Next I

        End If

        Locate 8, 1

        If fullCtOpt(D) = 1 Then
            If pbpOpt = 0 Then
                pbpString$ = gameTeams$(D) + " IS PRESSING"
                Call PBP
            Else
                'pbpOpt = 1
                If pbpDelay! <> 0 Then
                    Call WINDEX
                    Color teamRat_GAME(D, 33), teamRat_GAME(D, 34): Print gameTeams$(D);
                    Color 15, 0
                    Print " IS PRESSING"
                    Call DELAY
                End If
            End If
            P1 = 1
        End If

        GoTo 300

    End If

    '--- Shot Branch 3 ---
    Call d100(RN0)
    Call d100(shotChance)
    rndDef = Int(Rnd(1) * 5)

    If shotChance > gameRatings!(D, gameB1!(D, rndDef), 6) Then
        545 '
        If pbpOpt = 1 And pbpDelay! <> 0 Then
            Call WINDEX
            Print "SHOT MISSED BY "; players_GAME$(P, gameB1!(P, ballCarrier), 0)
            Call DELAY
        End If

        If pbpOpt = 0 Then
            PT% = 8
            Call MAKEPBP
            Call PBP
        End If

        Call ResetShotClock

    Else
        If pbpDelay! <> 0 Then

            If pbpOpt = 1 Then
                Call WINDEX
                Print "SHOT BLOCKED BY "; players_GAME$(D, gameB1!(D, rndDef), 0)
                Call DELAY
            Else

                Call d100(RN1)

                Select Case RN1
                    Case 1 To 10: pbpString$ = players_GAME$(D, gameB1!(D, rndDef), 0) + " STUFFS THE SHoffTotals!!"
                    Case 11 To 15: pbpString$ = "DENIED BY " + players_GAME$(D, gameB1!(D, rndDef), 0) + "!!"
                    Case 16 To 20: pbpString$ = "ERASED BY " + players_GAME$(D, gameB1!(D, rndDef), 0) + "!!"
                    Case 21 To 25: pbpString$ = "EXPUNGED BY " + players_GAME$(D, gameB1!(D, rndDef), 0) + "!!"
                    Case 26 To 30: pbpString$ = "REBUKED BY " + players_GAME$(D, gameB1!(D, rndDef), 0) + "!!"
                    Case 31 To 35: pbpString$ = "SHOT BLOCKED CLEANLY BY " + players_GAME$(D, gameB1!(D, rndDef), 0) + "!!"
                    Case 36 To 70: pbpString$ = "SHOT BLOCKED BY " + players_GAME$(D, gameB1!(D, rndDef), 0) + "!!"
                    Case 71 To 90: pbpString$ = "SHOT REJECTED BY " + players_GAME$(D, gameB1!(D, rndDef), 0) + "!!"
                    Case 91 To 100: pbpString$ = "SHOT SWATTED OUT BY " + players_GAME$(D, gameB1!(D, rndDef), 0) + "!!"
                End Select

                Call PBP

            End If

        End If

        554 '
        gameRatings!(D, gameB1!(D, rndDef), 15) = gameRatings!(D, gameB1!(D, rndDef), 15) + 1
        xFactor = 98

    End If

    gameClock! = gameClock! - 4
    timeElapsed! = 4

    Call AdjustPlayTime
    Call CheckGameClock
    If gameClock! = 0 Then 170

    Call UPDATESCREEN

    GoTo 670

    '----- NOODLE -----

    565 '

    Call Fouls(0)

    566 '
    If P2(D) > bonusFoulNum And quarter <= 4 Or P2(D) > 3 And quarter > 4 Then 570

    Call CheckGameClock
    If gameClock! = 0 Then 170

    If ftRulesOpt = 0 Then

        Call Substitutions

        If pbpDelay! <> 0 Then
            For I = 8 To 15
                Locate I, 1
                Print Space$(38)
            Next I
        End If

        Locate 8, 1
        pbpString$ = gameTeams$(P) + " BALL AFTER FOUL"
        Call PBP

        Locate 8, 41: Print Space$(12)

        GoTo 350

    End If

    Call UPDATESCREEN

    If pbpDelay! <> 0 Then

        If pbpOpt = 0 Then
            pbpString$ = "1 FT: " + players_GAME$(P, gameB1!(P, ballCarrier), 0) + Str$(gameRatings!(P, gameB1!(P, ballCarrier), 1)) + " %"
            Call PBP
        Else

            Call WINDEX
            Color teamRat_GAME(P, 33), teamRat_GAME(P, 34): Print gameTeams$(P)
            Color 15, 0

            Print "1 FT: "; players_GAME$(P, gameB1!(P, ballCarrier), 0); " "; gameRatings!(P, gameB1!(P, ballCarrier), 1); " %"
        End If

    End If

    xFactor = 50
    Call Substitutions
    xFactor = 0

    If pbpDelay! <> 0 Then

        If pbpOpt = 0 Then
            pbpString$ = "1 FT: " + players_GAME$(P, gameB1!(P, ballCarrier), 0) + Str$(gameRatings!(P, gameB1!(P, ballCarrier), 1)) + " %"
            Call PBP
        Else
            Call WINDEX
            Color teamRat_GAME(P, 33), teamRat_GAME(P, 34): Print gameTeams$(P)
            Color 15, 0
            Print "1 FT: "; players_GAME$(P, gameB1!(P, ballCarrier), 0); " "; gameRatings!(P, gameB1!(P, ballCarrier), 1); " %"
        End If
    End If

    'One FT Attempt
    F3 = 1

    'Increase FT ATT
    gameRatings!(P, gameB1!(P, ballCarrier), 9) = gameRatings!(P, gameB1!(P, ballCarrier), 9) + 1

    Call d100(S)

    'FT Success Chance
    FX% = gameRatings!(P, gameB1!(P, ballCarrier), 1)

    If S <= FX% Then
        If pbpOpt = 0 Then
            Call THREEFT
        Else
            'pbpOpt = 1
            Call MADEFT
        End If
    Else
        'S > FX%
        If pbpOpt = 0 Then
            Call BADFT
        Else
            'pbpOpt = 1
            Call MISSFT
        End If
    End If

    LT = 0

    Call CheckGameClock
    If gameClock! = 0 Then 170

    'Rebound after missed FT
    If S > FX% Then 650

    'GoSub 3000
    'AKA ChangePosession
    prevBall$ = ""
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

    pbpString$ = gameTeams$(P) + " BALL AFTER SCORE"
    Call SELECTPBP
    Locate 8, 41: Print Space$(12)
    Call Choices
    ballCarrier = 6
    xFactor = 0

    If pbpDelay! <> 0 Then
        For I = 8 To 15
            Locate I, 1
            Print Space$(38)
        Next I
    End If

    Locate 8, 1

    If fullCtOpt(D) = 1 Then
        If pbpOpt = 0 Then
            pbpString$ = gameTeams$(D) + " IS PRESSING"
            Call PBP
        Else
            'pbpOpt = 1
            If pbpDelay! <> 0 Then
                Call WINDEX
                Color teamRat_GAME(D, 33), teamRat_GAME(D, 34)
                Print gameTeams$(D);: Color 15, 0
                Print " IS PRESSING"
                Call DELAY
            End If

            P1 = 1
        End If
    End If

    GoTo 300

    '----- NOODLE -----

    570 '
    If freeThrowVal = 1 And (P2(D) > bonusFoulNum And quarter <= 4 Or P2(D) > 3 And quarter > 4) Then
        pbpString$ = players_GAME$(P, gameB1!(P, ballCarrier), 0) + " GETS 3 FT TO MAKE 2..."
    Else
        pbpString$ = players_GAME$(P, gameB1!(P, ballCarrier), 0) + " GETS 2 FREE THROWS..."
    End If
    Call PBP

    GoTo 591

    '----- NOODLE -----

    580 '
    Call LastShot

    Call ResetShotClock

    prevBall$ = ""

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
    ballCarrier = fouler

    If P2(D) > bonusFoulNum And quarter <= 4 Or P2(D) > 3 And quarter > 4 Then 570

    Call CheckGameClock
    If gameClock! = 0 Then 170

    pbpString$ = "OFFENSIVE FOUL"

    If pbpOpt = 0 Then
        Call PBP
    Else
        'pbpOpt = 1
        If pbpDelay! <> 0 Then
            Call WINDEX
            Print pbpString$
            Call DELAY
        End If
    End If

    pbpString$ = gameTeams$(P) + " BALL AFTER FOUL"
    If pbpOpt = 0 Then
        Call PBP
    Else
        'pbpOpt = 1
        If pbpDelay! <> 0 Then
            Print pbpString$
            Call DELAY
        End If
    End If

    Locate 8, 41: Print Space$(12)

    ballCarrier = 6
    Call PBP
    turnovers(D) = turnovers(D) + 1
    Call UPDATESCREEN
    Call Substitutions

    If pbpDelay! <> 0 Then
        For I = 8 To 15
            Locate I, 1
            Print Space$(38)
        Next I
    End If

    Locate 8, 1

    If fullCtOpt(D) = 1 Then

        If pbpOpt = 0 Then
            pbpString$ = gameTeams$(D) + " IS PRESSING"
            Call PBP
        Else
            'pbpOpt = 1
            If pbpDelay! <> 0 Then
                Call WINDEX
                Color teamRat_GAME(D, 33), teamRat_GAME(D, 34): Print gameTeams$(D);
                Color 15, 0: Print " IS PRESSING"
                Call DELAY
            End If
        End If

        P1 = 1

    End If

    GoTo 300

    '----- NOODLE -----

    591 '

    xFactor = 50
    Call Substitutions
    xFactor = 0

    If pbpDelay! <> 0 Then

        If pbpOpt = 0 Then

            If freeThrowVal = 1 And P2(D) > bonusFoulNum And quarter <= 4 Or P2(D) > 3 And quarter > 4 Then
                pbpString$ = "3 TO MAKE 2 FT: " + players_GAME$(P, gameB1!(P, ballCarrier), 0) + Str$(gameRatings!(P, gameB1!(P, ballCarrier), 1)) + " %":
            Else
                pbpString$ = "2 FT: " + players_GAME$(P, gameB1!(P, ballCarrier), 0) + Str$(gameRatings!(P, gameB1!(P, ballCarrier), 1)) + " %"
            End If

            Call PBP

        Else
            'pbpOpt = 1
            Call WINDEX
            Print gameTeams$(P)

            If freeThrowVal = 1 And P2(D) > bonusFoulNum And quarter <= 4 Or P2(D) > 3 And quarter > 4 Then
                Print "3 TO MAKE 2 FT: ";
                Print players_GAME$(P, gameB1!(P, ballCarrier), 0); "   "; gameRatings!(P, gameB1!(P, ballCarrier), 1); " %"
            Else
                Print "2 FT: ";
                Print players_GAME$(P, gameB1!(P, ballCarrier), 0); "   "; gameRatings!(P, gameB1!(P, ballCarrier), 1); " %"
            End If

        End If

    End If

    Call DELAY
    NM% = 0

    'Three FT Attempts
    For F3 = 1 To 3

        'Increase FTA
        gameRatings!(P, gameB1!(P, ballCarrier), 9) = gameRatings!(P, gameB1!(P, ballCarrier), 9) + 1

        Call d100(S)

        'FT Success Chance
        FX% = gameRatings!(P, gameB1!(P, ballCarrier), 1)

        If S <= FX% Then
            If pbpOpt = 0 Then
                NM% = NM% + 1
                Call THREEFT
            Else
                'pbpOpt = 1
                NM% = NM% + 1
                Call MADEFT
            End If
        Else
            'S > FX%
            If pbpOpt = 0 Then
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
            If freeThrowVal = 0 Or (freeThrowVal = 1 And (P2(D) <= bonusFoulNum And quarter <= 4 Or P2(D) <= 3 And quarter > 4)) Then Exit For
        End If

    Next F3

    Call CheckGameClock

    If gameClock! = 0 Then 170

    If S > gameRatings!(P, gameB1!(P, ballCarrier), 1) Then 650

    'GoSub 3000
    'AKA ChangePosession

    prevBall$ = ""
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

    pbpString$ = gameTeams$(P) + " BALL AFTER SCORE"

    Call SELECTPBP
    Locate 8, 41: Print Space$(12)
    Call Choices

    ballCarrier = 6
    xFactor = 0

    If pbpDelay! <> 0 Then
        For I = 8 To 15
            Locate I, 1
            Print Space$(38)
        Next I
    End If

    Locate 8, 1
    If fullCtOpt(D) = 1 Then

        If pbpOpt = 0 Then
            pbpString$ = gameTeams$(D) + " IS PRESSING"
            Call PBP
        End If

        If pbpOpt = 1 And pbpDelay! <> 0 Then
            Call WINDEX
            Color teamRat_GAME(D, 33), teamRat_GAME(D, 34): Print gameTeams$(D);
            Color 15, 0
            Print " IS PRESSING"
            Call DELAY
        End If

        P1 = 1

    End If

    GoTo 300


    '----- NOODLE -----

    650 '

    A6 = 1
    P6 = 0
    A4 = 0
    D6 = 0

    Call ResetShotClock

    S = 0

    Call d100(RN0)

    scoreDiff = score(D, 0) - score(P, 0)
    If (quarter > 3 And gameClock! < 30 And RN0 <= 12) And (scoreDiff = 2 Or scoreDiff = 0) Then

        Call ReboundOffense
        Call pbplog(3753)
        GoTo 760

    Else

        Call d100(RN1)

        Select Case RN1

            Case 1 To 11:
                foulOnReb = 1
                Call pbplog(3764)
                GoTo 770

            Case 12 To 19:
                Call pbplog(3768)
                Call ReboundOffense
                GoTo 760

            Case 20 To 100:
                Call pbplog(3773)
                Call ReboundDefense
                GoTo 800

        End Select

    End If

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

    Call d100(RN0)

    scoreDiff = score(D, 0) - score(P, 0)

    If RN0 <= 12 And quarter > 3 And gameClock! < 30 And (scoreDiff = 2 Or scoreDiff = 0) Then

        Call pbplog(3806)
        GoTo 700

    Else

        Select Case RN0

            Case 1

                If xFactor = 98 Then
                    Call pbplog(3815)
                    GoTo 700

                Else

                    turnovers(P) = turnovers(P) + 1
                    pbpString$ = "OFFENSIVE GOAL TENDING"
                    If pbpOpt = 0 Then
                        Call PBP
                    Else
                        'pbpOpt = 1
                        If pbpDelay! <> 0 Then
                            Call WINDEX
                            Print pbpString$
                            Call DELAY
                        End If
                    End If

                    pbpString$ = "BALL GOES TO " + gameTeams$(D)
                    If pbpOpt = 0 Then
                        Call PBP
                    Else
                        'pbpOpt = 1
                        If pbpDelay! <> 0 Then
                            Call WINDEX
                            Print pbpString$
                            Call DELAY
                        End If
                    End If

                    Call DELAY

                    prevBall$ = ""

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

                    Call pbplog(3863)
                    GoTo 300

                End If

            Case 2 To 11
                foulOnReb = 1
                Call pbplog(3870)
                GoTo 770

            Case 12 To 19

                Call d100(RN1)

                If RN1 <= 32 Then

                    'Increase total rebounds
                    totRebounds(P) = totRebounds(P) + 1

                    Call UPDATESCREEN

                    pbpString$ = "RB KNOCKED O.B. TO " + gameTeams$(P)

                    If pbpOpt = 0 Then
                        Call PBP
                    Else
                        'pbpOpt = 1
                        If pbpDelay! <> 0 Then
                            Call WINDEX
                            Print pbpString$
                            Call DELAY
                        End If
                    End If

                    Call DELAY
                    Call Substitutions

                    Call pbplog(3900)
                    GoTo 520

                Else

                    'Increase total rebounds
                    totRebounds(D) = totRebounds(D) + 1

                    Call UPDATESCREEN

                    pbpString$ = "RB KNOCKED O.B. TO " + gameTeams$(D)

                    If pbpOpt = 0 Then
                        Call PBP
                    Else
                        'pbpOpt = 1
                        If pbpDelay! <> 0 Then
                            Call WINDEX
                            Print pbpString$
                            Call DELAY
                        End If
                    End If

                    Call DELAY
                    Call Substitutions

                    prevBall$ = ""

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

                    Call pbplog(3944)
                    GoTo 300

                End If

            Case 20 To 22

                If xFactor = 99 Then

                    Call pbplog(3953)
                    GoTo 700

                Else

                    Call ReboundOffense

                    gameRatings!(P, gameB1!(P, ballCarrier), 7) = gameRatings!(P, gameB1!(P, ballCarrier), 7) + 1
                    gameRatings!(P, gameB1!(P, ballCarrier), 8) = gameRatings!(P, gameB1!(P, ballCarrier), 8) + 1

                    'Increase Total rebounds
                    gameRatings!(P, gameB1!(P, ballCarrier), 11) = gameRatings!(P, gameB1!(P, ballCarrier), 11) + 1
                    totRebounds(P) = totRebounds(P) + 1

                    score(P, quarter) = score(P, quarter) + 2
                    score(P, 0) = score(P, 0) + 2

                    Call UPDATESCREEN

                    If pbpOpt = 1 Then
                        If pbpDelay! <> 0 Then
                            Call WINDEX
                            Print "TIPPED IN BY "; players_GAME$(P, gameB1!(P, ballCarrier), 0); "!!!"
                            Call DELAY
                        End If
                    Else
                        'pbpOpt = 0
                        pbpString$ = players_GAME$(P, gameB1!(P, ballCarrier), 0) + " TIPS IT IN!!!"
                        Call PBP
                    End If

                    Call DELAY

                    prevBall$ = ""
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

                    pbpString$ = gameTeams$(P) + " BALL AFTER SCORE"
                    Locate 8, 41: Print Space$(12)

                    If pbpOpt = 0 Then
                        Call PBP
                    Else
                        'pbpOpt = 1
                        If pbpDelay! <> 0 Then Print pbpString$: Call DELAY
                    End If

                    Call Choices

                    If pbpDelay! <> 0 Then
                        For I = 8 To 15
                            Locate I, 1
                            Print Space$(38)
                        Next I
                    End If

                    Locate 8, 1

                    If fullCtOpt(D) = 1 Then

                        If pbpOpt = 0 Then
                            pbpString$ = gameTeams$(D) + " IS PRESSING"
                            Call PBP
                        Else
                            'pbpOpt = 1
                            If pbpDelay! <> 0 Then
                                Call WINDEX
                                Color teamRat_GAME(D, 33), teamRat_GAME(D, 34): Print gameTeams$(D);
                                Color 15, 0: Print " IS PRESSING"
                                Call DELAY
                            End If

                            P1 = 1
                        End If

                    End If

                    Call pbplog(4040)
                    GoTo 300

                End If

            Case 23:

                If P6 <> 4 Then

                    If xFactor = 99 Then

                        Call pbplog(4051)
                        GoTo 700

                    Else
                        pbpString$ = "DEFENSIVE GOAL TENDING"

                        If pbpOpt = 0 Then
                            Call PBP
                        Else
                            'pbpOpt = 1
                            If pbpDelay! <> 0 Then
                                Call WINDEX
                                Print pbpString$
                                Call DELAY
                            End If
                            Call DELAY
                        End If

                        Call pbplog(4071)
                        GoTo 560

                    End If

                End If

            Case 24 To 100

                770 '
                'Calculate rebound contributions
                '-- RB is a total of "rebound ratings" for both teams
                '-- rebRatings is an array to track "rebound ratings" for each player
                '   0 - 4 is for team 1; 5 - 9 is for team 2
                RB = 0

                Erase rebRatings

                'Team 1
                For I = 0 To 4

                    If quarter > 4 Or quarter = 4 And gameClock! <= 120 Then
                        rebRatings(I) = rebRatings(I) + gameRatings!(P, gameB1!(P, I), 21)
                        RB = RB + gameRatings!(P, gameB1!(P, I), 21)
                    Else
                        If gameRatings!(P, gameB1!(P, I), 3) - gameRatings!(P, gameB1!(P, I), 7) - gameRatings!(P, gameB1!(P, I), 11) - gameRatings!(P, gameB1!(P, I), 12) <= 0 Then
                            rebRatings(I) = rebRatings(I) + gameRatings!(P, gameB1!(P, I), 21) * .65
                            RB = RB + gameRatings!(P, gameB1!(P, I), 21) * .65
                        Else
                            rebRatings(I) = rebRatings(I) + gameRatings!(P, gameB1!(P, I), 21)
                            RB = RB + gameRatings!(P, gameB1!(P, I), 21)
                        End If
                    End If

                Next

                'Team 2
                For I = 0 To 4

                    If quarter > 4 Or quarter = 4 And gameClock! <= 120 Then
                        rebRatings(I + 5) = rebRatings(I + 5) + gameRatings!(1 - P, gameB1!(1 - P, I), 2)
                        RB = RB + gameRatings!(1 - P, gameB1!(1 - P, I), 2)
                    Else
                        If gameRatings!(1 - P, gameB1!(1 - P, I), 3) - gameRatings!(1 - P, gameB1!(1 - P, I), 7) - gameRatings!(1 - P, gameB1!(1 - P, I), 11) - gameRatings!(1 - P, gameB1!(1 - P, I), 12) <= 0 Then
                            rebRatings(I + 5) = rebRatings(I + 5) + gameRatings!(1 - P, gameB1!(1 - P, I), 2) * .5
                            RB = RB + gameRatings!(1 - P, gameB1!(1 - P, I), 2) * .5
                        Else
                            rebRatings(I + 5) = rebRatings(I + 5) + gameRatings!(1 - P, gameB1!(1 - P, I), 2)
                            RB = RB + gameRatings!(1 - P, gameB1!(1 - P, I), 2)
                        End If
                    End If

                Next

                rbChance = Rnd(1) * RB

                skipRandom% = 0

                For ballCarrier = 0 To 9

                    If rbChance <= rebRatings(ballCarrier) Then
                        Call pbplog(4131)
                        skipRandom% = 1
                        Exit For
                    End If

                    rebRatings(ballCarrier + 1) = rebRatings(ballCarrier + 1) + rebRatings(ballCarrier)

                Next

                If skipRandom% = 0 Then
                    ballCarrier = Int(Rnd(1) * 10)
                End If

                If ballCarrier <= 4 Then
                    foulOnReb = 0
                    Call pbplog(4147)
                    '-- GoTo 761 is implied
                Else
                    ballCarrier = ballCarrier - 5
                    Call pbplog(4151)
                    GoTo 800
                End If

        End Select

    End If


    '----- NOODLE -----
    'Offensive Rebound

    760 '
    If foulOnReb = 1 Then

        foulOnReb = 0

        pbpString$ = players_GAME$(P, gameB1!(P, ballCarrier), 0) + " HAS THE OFFENSIVE REBOUND"

        If pbpOpt = 0 Then
            Call PBP
        Else
            'pbpOpt = 1
            If pbpDelay! <> 0 Then
                Call WINDEX
                Print pbpString$
                Print "FOR "; gameTeams$(P)
                Call DELAY
            End If
        End If

        pbpString$ = "HE'S FOULED !!!"
        If pbpOpt = 0 Then
            Call PBP
        Else
            'pbpOpt = 1
            If pbpDelay! <> 0 Then
                Print pbpString$
                Call DELAY
            End If
        End If

        Call DELAY

        'Increase Off rebounds
        gameRatings!(P, gameB1!(P, ballCarrier), 24) = gameRatings!(P, gameB1!(P, ballCarrier), 24) + 1

        'Increase Total rebounds
        gameRatings!(P, gameB1!(P, ballCarrier), 11) = gameRatings!(P, gameB1!(P, ballCarrier), 11) + 1
        totRebounds(P) = totRebounds(P) + 1

        Call UPDATESCREEN
        Call Fouls(0)

        Call pbplog(4206)
        GoTo 570

    Else

        761 '

        '-- Offensive Rebound

        'Increase Off rebounds
        gameRatings!(P, gameB1!(P, ballCarrier), 24) = gameRatings!(P, gameB1!(P, ballCarrier), 24) + 1

        'Increase Total rebounds
        gameRatings!(P, gameB1!(P, ballCarrier), 11) = gameRatings!(P, gameB1!(P, ballCarrier), 11) + 1
        totRebounds(P) = totRebounds(P) + 1

        Call UPDATESCREEN

        pbpString$ = players_GAME$(P, gameB1!(P, ballCarrier), 0) + " HAS THE OFFENSIVE REBOUND"

        If pbpDelay! <> 0 Then
            If pbpOpt = 0 Then
                Call PBP
            Else
                'pbpOpt = 1
                Call WINDEX
                Print pbpString$
                Print "FOR " + gameTeams$(P)
                Call DELAY
            End If
        End If

        Call CheckGameClock
        If gameClock! = 0 Then 170

        prevBall$ = players_GAME$(P, gameB1!(P, ballCarrier), 0)
        RN0 = Int(Rnd(1) * 10) + 1

        ' *** GET SHOT PCT ***
        F = gameRatings!(P, gameB1!(P, ballCarrier), 0) + D1(D)

        If gameLoc = P Then
            F = F + 2
        Else
            If gameLoc <> 2 Then F = F - 2
        End If

        If Not (quarter > 4 Or quarter = 4 And gameClock! <= 120) Then
            If gameRatings!(P, gameB1!(P, ballCarrier), 3) - gameRatings!(P, gameB1!(P, ballCarrier), 7) - gameRatings!(P, gameB1!(P, ballCarrier), 11) - gameRatings!(P, gameB1!(P, ballCarrier), 12) <= 0 Then F = F * .6
        End If

        Call pbplog(4259)
        If RN0 <= 8 Then
            PZ% = 9
            GoTo 537
        Else
            GoTo 520
        End If

    End If

    '----- NOODLE -----
    'Defensive Rebound

    800 '
    If foulOnReb = 1 Then

        foulOnReb = 0

        If pbpDelay! <> 0 Then
            If pbpOpt = 1 Then
                Call WINDEX
                Print players_GAME$(D, gameB1!(D, ballCarrier), 0); " HAS THE DEF REBOUND FOR "
                Print gameTeams$(D); " AND IS FOULED !!!"
                Call DELAY
            Else
                pbpString$ = players_GAME$(D, gameB1!(D, ballCarrier), 0) + " HAS THE DEFENSIVE REBOUND"
                Call PBP
                pbpString$ = "HE'S FOULED !!!"
                Call PBP
                Call DELAY
            End If
        End If

        'Increase Total rebounds
        gameRatings!(D, gameB1!(D, ballCarrier), 11) = gameRatings!(D, gameB1!(D, ballCarrier), 11) + 1
        totRebounds(D) = totRebounds(D) + 1

        Call UPDATESCREEN

        prevBall$ = ""

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

        'Increase Total rebounds
        gameRatings!(D, gameB1!(D, ballCarrier), 11) = gameRatings!(D, gameB1!(D, ballCarrier), 11) + 1
        totRebounds(D) = totRebounds(D) + 1

        Call UPDATESCREEN
        pbpString$ = players_GAME$(D, gameB1!(D, ballCarrier), 0) + " HAS THE DEFENSIVE REBOUND"

        If pbpDelay! <> 0 Then
            If pbpOpt = 0 Then
                Call PBP
            Else
                'pbpOpt = 1
                Call WINDEX
                Print pbpString$
                Print "FOR " + gameTeams$(D)
                Call DELAY
            End If
        End If

        prevBall$ = players_GAME$(D, gameB1!(D, ballCarrier), 0)
        C1 = 2
        P9 = D

        'Change posession
        prevBall$ = ""

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

        fastBreak = 1

        GoTo 300

    End If

End Sub

