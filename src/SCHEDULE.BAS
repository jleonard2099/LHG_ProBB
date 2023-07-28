Sub SCHEDULE ()

    Dim scheduleFile$
    ReDim menuChoices$(1 To 7)

    Shared NS%
    Shared scheduleAP%(), scheduleNG%()

    _Title "Basketball: The Pro Game - Scheduler"

    scheduleNG%(0, 2) = 1
    scheduleNG%(0, 3) = 1
    scheduleNG%(0, 7) = 0
    scheduleNG%(0, 8) = 0
    scheduleNG%(0, 14) = 4

    Do

        Color 7, 0
        Cls

        Call TITLE_SCHEDULER
        menuChoices$(1) = "ENTER A NEW SCHEDULE                             "
        menuChoices$(2) = "RETRIEVE/EDIT/PLAY AN EXISTING SCHEDULE          "
        menuChoices$(3) = "EXTRACT ONE TEAM'S SCHEDULE FROM LEAGUE SCHEDULE "
        menuChoices$(4) = "CHANGE THE MODE OF PLAY FOR A TEAM IN A SCHEDULE "
        menuChoices$(5) = "MAKE GLOBAL CHANGES TO A SCHEDULE                "
        menuChoices$(6) = "BOXSCORE MAINTENANCE / SAVE BOXSCORE PARAMETERS  "
        menuChoices$(7) = "EXIT AUTO SCHEDULER TO PRO GAME MAIN MENU        "
        Selection% = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 16

        Call box0(7, 15, 15, 65, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1
                    'number of games in schedule
                    scheduleAP%(0) = 0
                    NS% = 0

                    Do
                        'Get name of new schedule
                        Color 15, 0
                        Cls

                        JJ = 7

                        Locate 6, 8
                        Color 15, 1

                        'Print box top
                        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

                        'Print box sides
                        For II = 1 To JJ
                            Locate 6 + II, 8
                            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                        Next

                        'Print box bottom
                        Locate 7 + JJ, 8
                        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

                        Locate 7, 32
                        Locate , 12: Print "ENTER NAME FOR SCHEDULE: ": Print
                        Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPACES"
                        Locate , 12: Print "DO NOT ADD A FILE EXTENSION; THE PROGRAM"
                        Locate , 12: Print "WILL AUTOMATICALLY ADD .SCD"
                        Locate , 12: Print "SAMPLE NAMES ARE ABA73-74, BOS93, NBA83-84, ETC"
                        Locate 7, 37: Input scheduleFile$

                        If scheduleFile$ <> "" Then

                            Do
                                Locate 13, 12: Print "ANY CHANGE (YN) "
                                I$ = GetKeyPress$
                            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                        End If

                    Loop Until UCase$(I$) = "N" And scheduleFile$ <> ""

                    Color 15, 0

                    Call NewSchedule(scheduleFile$)

                Case 2 To 5:
                    'Have the user pick a schedule
                    Call LoadSchedule(scheduleFile$)

                    If scheduleFile$ <> "" Then
                        Cls

                        'Edit / Extract / Change
                        If Selection% >= 3 And Selection% <= 5 Then
                            Call ExtractSchedule(Selection%, scheduleFile$)
                            If Selection% >= 4 Then Call SaveScheduleFile(scheduleFile$)
                        Else
                            'Retrieve / Edit Play
                            Color 15, 0
                            Call NewSchedule(scheduleFile$)
                        End If

                    End If

                Case 6:
                    Call BoxscoreMaintenance

                Case 7:
                    Cls

            End Select

        End If

    Loop Until Selection% = 7 Or ExitCode% = 27

End Sub 'Menu

'----------------------------------------
'       NewSchedule SubRoutine
'----------------------------------------
' Facilitate user creation of a schedule
' Provides all options required and displays
' schedule while editing
Sub NewSchedule (scheduleFile$)

    Shared BS%, NS%
    Shared N$

    Shared homeScores(), visitorScores()
    Shared scheduleAP%(), scheduleNG%()
    Shared scheduleH$(), scheduleV$()
    Shared scheduleYN$()

    Shared modeAbbrev$(), sClockOpt$(), yesNo$()

    Do

        Cls
        Locate 23, 1: Color , 1: Print Tab(80);
        
        Locate 23, 1
        Color 13: Print "SC";: Color 14: Print "-SHOT CLOCK  ";
        Color 13: Print "MA";: Color 14: Print "-MODE OF PLAY  ";
        Color 11: Print " G";: Color 14: Print "-GOTO GAME";
        Color 11: Print " B";: Color 14: Print "-BOXSCORE OPTIONS"

        Locate 24, 1: Color , 1: Print Tab(80);: Locate 24, 1
        Color 11: Print "C";: Color 14: Print "-SAVE AND CONTINUE  ";
        Color 11: Print "A";: Color 14: Print "-ADD A GAME  ";
        Color 11: Print "D";: Color 14: Print "-DELETE A GAME  ";
        Color 11: Print "E";: Color 14: Print "-EDIT A GAME  ";
        Color 11: Print "PGUP  PGDN";

        Locate 25, 1: Color , 1: Print Tab(80);: Locate 25, 1
        Color 11: Print "S";: Color 14: Print "-SORT GAMES  ";
        Color 11: Print "P";: Color 14: Print "-PLAY GAMES   ";
        Color 11: Print "X";: Color 14: Print "-SAVE AND EXIT  ";
        Color 11: Print "Q";: Color 14: Print "-QUIT  ";
        Color 11: Print "R";: Color 14: Print "-RESET SCORES";

        Locate 1, 1: Print Tab(80);
        Locate 1, 1: Print scheduleFile$
        Locate 1, 33: Color 14: Print "AUTO SCHEDULER"

        Color 15, 0

        Locate 2, 1
        Color 14
        Print "GAME  DATE      TIME    VISITING TEAM        HOME TEAM            MA  HC  3  SC"
        Color 15

        'Show 20 schedule entries on the screen at once
        For X = 1 To 20

            Locate X + 2, 1

            If X + NS% * 20 <= scheduleAP%(0) Then

                T1 = scheduleNG%(X + NS% * 20, 0)
                YN$ = scheduleYN$(X + NS% * 20, 0)
                Call ReadTeam (YN$, N$, T1)
                scheduleV$(X) = N$

                T1 = scheduleNG%(X + NS% * 20, 1)
                YN$ = scheduleYN$(X + NS% * 20, 1)
                Call ReadTeam (YN$, N$, T1)
                scheduleH$(X) = N$

                'Game number
                Print Using "####  "; X + NS% * 20;

                'Date
                Print Using "##/##/##"; scheduleNG%(X + NS% * 20, 17); scheduleNG%(X + NS% * 20, 16); scheduleNG%(X + NS% * 20, 18);

                'Time
                Locate , 17
                I$ = "A"
                HR% = scheduleNG%(X + NS% * 20, 15) / 100
                MIN% = scheduleNG%(X + NS% * 20, 15) - HR% * 100

                If HR% = 12 Then I$ = "P"
                If HR% > 12 Then HR% = HR% - 12: I$ = "P"

                TM$ = Right$(Str$(HR%), 2) + ":"

                If MIN% < 10 Then TM$ = TM$ + "0" + Right$(Str$(MIN%), 1)
                If MIN% >= 10 Then TM$ = TM$ + Right$(Str$(MIN%), 2)

                'Time
                TM$ = TM$ + I$
                Print TM$;

                'Visiting Team
                Locate , 25: Print scheduleV$(X); " ";
                If scheduleNG%(X + NS% * 20, 13) = 1 Then Print Using "###"; visitorScores(X + NS% * 20);

                'Home Team
                Locate , 46: Print scheduleH$(X); " ";
                If scheduleNG%(X + NS% * 20, 13) = 1 Then Print Using "###"; homeScores(X + NS% * 20);

                'Mode of Play
                Locate , 67: Print modeAbbrev$(scheduleNG%(X + NS% * 20, 8));

                'Home Court
                Locate , 72: If scheduleNG%(X + NS% * 20, 2) = 1 Then Print "Y";: Else Print "N";

                '3 Pt Option
                Locate , 75: Print yesNo$(scheduleNG%(X + NS% * 20, 3));

                'Shot Clock
                Locate , 78: Print Left$(sclockOpt$(scheduleNG%(X + NS% * 20, 7)), 2)

            End If

        Next

        I$ = GetKeyPress$

        Select Case UCase$(I$)

            Case "A"
                ' *** ADD A GAME TO THE SCHEDULE ***
                V$ = ""
                H$ = ""

                'number of games in schedule
                scheduleAP%(0) = scheduleAP%(0) + 1
                targetGame% = scheduleAP%(0)

                BS% = Int((targetGame% - 1) / 20)
                If NS% < BS% Then NS% = BS%

                For X = 0 To 18
                    If X <> 13 Then
                        scheduleNG%(targetGame%, X) = scheduleNG%(targetGame% - 1, X)
                    End If
                Next

                For X = 0 To 1: 
                    scheduleYN$(targetGame%, X) = scheduleYN$(targetGame% - 1, X)
                Next

                DT$ = "10-01-91"
                TM$ = "07:30P"
                Call NewGame(V$, H$, YN$, NTMS, targetGame%)

            Case "E"
                Call EditGame

            Case "B":
                'ReDim array$(500) 'holds the main editor string array
                'Do
                '   Locate 23, 1: Print "ENTER GAME # TO GO TO        "
                '   Locate 23, 1: Input "ENTER GAME # TO GO TO "; targetGame%
                'Loop Until targetGame% >= 1 And targetGame% <= scheduleAP%(0)
                'NS% = Int((targetGame%- 1) / 20)
                'BN$ = diskPaths$(1) + "box" + Mid$(Str$(targetGame%, 2, Len(Str$(targetGame%))
                'Call EditBoxscore (BN$)

            Case "D"
                ' *** DELETE A GAME FROM THE SCHEDULE ***
                Do
                    Locate 24, 1: Input "ENTER GAME # TO DELETE "; targetGame%
                Loop Until targetGame% >= 1 And targetGame% <= scheduleAP%(0)

                For X = targetGame% To scheduleAP%(0) - 1
                    For X1 = 0 To 18
                        scheduleNG%(X, X1) = scheduleNG%(X + 1, X1)
                    Next
                    
                    visitorScores(X) = visitorScores(X + 1)
                    homeScores(X) = homeScores(X + 1)
                    
                    For X1 = 0 To 1 
                        scheduleYN$(X, X1) = scheduleYN$(X + 1, X1)
                    Next
                Next

                scheduleAP%(0) = scheduleAP%(0) - 1
                BS% = Int((scheduleAP%(0) - 1) / 20)
                If NS% > BS% Then NS% = BS%

            Case "G":
                'GOTO A SPECIFIC PAGE
                Do
                    Locate 23, 1: Print "ENTER GAME # TO GO TO        "
                    Locate 23, 1: Input "ENTER GAME # TO GO TO "; targetGame%
                Loop Until targetGame% >= 1 And targetGame% <= scheduleAP%(0)

                NS% = Int((targetGame% - 1) / 20)

            Case "S":
                Call SortSchedule

            Case "C", "X":
                Call SaveScheduleFile(scheduleFile$)

            Case "Q", Chr$(27): '

            Case "P":
                Call SaveScheduleFile(scheduleFile$)

                ' *** PLAY GAMES ***
                Color , 0
                Cls

                JJ = 8

                Locate 6, 8: Color 15, 4

                'Print box top
                Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

                'Print box sides
                For II = 1 To JJ
                    Locate 6 + II, 8
                    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                Next

                'Print box bottom
                Locate 7 + JJ, 8
                Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

                Do
                    'Locate 7, 32

                    Locate 8, 12: Print "TOTAL GAMES ="; scheduleAP%(0)
                    Locate 9, 12: Print "SELECT GAME # TO START AND END WITH": Print
                    Locate 10, 12: Input "ENTER START GAME #: "; SG%: Print
                    Locate 11, 12: Input "ENTER END GAME #:"; EG%: Print

                    Do
                        Locate 13, 12: Print "ANY CHANGE (YN)"
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                Loop Until UCase$(I$) = "N"

                If SG% <= 0 Then SG% = 1
                If EG% > scheduleAP%(0) Then EG% = scheduleAP%(0)

                Open "GAME82" For Output As #2
                Print #2, scheduleFile$ + ".SCD"
                Print #2, SG% 'Start Game
                Print #2, EG% 'End Game
                Print #2, 0
                Close 2

                For I = 1 To scheduleAP%(0)
                    HR% = Int(scheduleNG%(I, 15) / 100)
                    MIN% = scheduleNG%(I, 15) - HR% * 100
                    TM% = HR% * 60 + MIN%
                    scheduleNG%(I, 12) = Int(TM% / 30) - 18
                Next

                Open "TICKER" For Output As #2
                For I = 1 To 32
                    For I1 = 0 To 1
                        For I2 = 1 To 14
                            Print #2, -1
                        Next
                    Next
                Next

                For I = 1 To 32
                    For I1 = 0 To 1
                        Print #2, "XXX"
                    Next
                Next

                For I = SG% To SG% + 31
                    Print #2, scheduleNG%(I, 12)
                Next

                Close 2

                Call PROHOOP

                Call LoadScheduleInfo (scheduleFile$)

            Case "R"
                ' *** RESET GAMES ***
                For I = 1 To scheduleAP%(0)
                    scheduleNG%(I, 13) = 0
                Next

            Case Chr$(0) + "Q"
                ' *** PAGE DOWN ***
                If NS% >= BS% Then
                    NS% = BS%
                Else
                    NS% = NS% + 1
                End If

            Case Chr$(0) + "I"
                ' *** PAGE UP ***
                If NS% <> 0 Then
                    NS% = NS% - 1
                End If

        End Select

    Loop Until UCase$(I$) = "Q" Or UCase$(I$) = "X"

End Sub

'----------------------------------------
'       EditGame SubRoutine
'----------------------------------------
'...explanation...
Sub EditGame ()

    Shared scheduleAP%()
    Shared scheduleH$(), scheduleV$()
    Shared scheduleYN$()
    Shared scheduleNG%()

    Dim DT$, TM$

    ' *** EDIT A GAME ***
    Do
        Locate 23, 1: Print "ENTER GAME # TO EDIT        "
        Locate 23, 1: Input "ENTER GAME # TO EDIT "; targetGame%
    Loop Until targetGame% >= 1 And targetGame% <= scheduleAP%(0)

    If targetGame% - Int(targetGame% / 20) * 20 = 0 Then
        V$ = scheduleV$(20)
        H$ = scheduleH$(20)
        YN$ = scheduleYN$(20, 0)
    Else
        V$ = scheduleV$(targetGame% - Int(targetGame% / 20) * 20)
        H$ = scheduleH$(targetGame% - Int(targetGame% / 20) * 20)
        YN$ = scheduleYN$(targetGame% - Int(targetGame% / 20) * 20, 0)
    End If

    I$ = "A"
    HR% = Int(scheduleNG%(targetGame%, 15) / 100)
    MIN% = scheduleNG%(targetGame%, 15) - HR% * 100

    If HR% = 12 Then I$ = "P"
    If HR% > 12 Then HR% = HR% - 12: I$ = "P"
    TM$ = Right$(Str$(HR%), 2) + ":"

    If MIN% < 10 Then
        TM$ = TM$ + "0" + Right$(Str$(MIN%), 1)
    Else
        TM$ = TM$ + Right$(Str$(MIN%), 2)
    End If

    TM$ = TM$ + I$
    DT$ = Right$(Str$(scheduleNG%(targetGame%, 17)), 2) + "/"
    DYMO% = Val(Right$(Str$(scheduleNG%(targetGame%, 16)), 2))

    If DYMO% < 10 Then
        DYMO$ = "0" + Right$(Str$(scheduleNG%(targetGame%, 16)), 1)
    Else
        DYMO$ = Right$(Str$(scheduleNG%(targetGame%, 16)), 2)
    End If

    DT$ = DT$ + DYMO$ + "/" + Right$(Str$(scheduleNG%(targetGame%, 18)), 2)

    Call NewGame(V$, H$, YN$, NTMS, targetGame%)

End Sub

'----------------------------------------
'           NewGame SubRoutine
'----------------------------------------
'...explanation...
Sub NewGame (visitor$, home$, YN$, NTMS, gameIndex%)

    Dim displayPause!
    
    Shared N$
    
    Shared scheduleNG%()

    Shared scheduleH$(), scheduleV$(), scheduleYN$()
    
    Shared coachMode$(), MO$(), modeAbbrev$()
    Shared sClockOpt$(), teamEraOpt$()
    Shared yesNo$()


    Do

        ' ** Enter New Game **
        Cls
        Color 2: Print Tab(29); "SELECT GAME OPTIONS"
        Print

        Locate 2, 2: Color 14: Print "(1) ";: Color 15: Print MO$(scheduleNG%(gameIndex%, 8))
        Locate 3, 2: Color 14: Print "(2) ";: Color 15: Print "VISITING TEAM: "; visitor$
        Locate 4, 2: Color 14: Print "(3) ";: Color 15: Print "HOME TEAM: "; home$
        Locate 5, 2: Color 14: Print "(4) ";: Color 15: Print "3-POINT SHOT: "; yesNo$(scheduleNG%(gameIndex%, 3))
        Locate 6, 2: Color 14: Print "(5) ";: Color 15: Print "SHOT CLOCK "; sClockOpt$(scheduleNG%(gameIndex%, 7))
        Locate 7, 2: Color 14: Print "(6) ";: Color 15: Print "HOME COURT ADVANTAGE: "; location$(scheduleNG%(gameIndex%, 2))
        Locate 8, 2: Color 14: Print "(7) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#  "; scheduleNG%(gameIndex%, 4) / 10
        Locate 9, 2: Color 14: Print "(8) ";: Color 15: Print playoffGame$(scheduleNG%(gameIndex%, 5))
        Locate 10, 2: Color 14: Print "(9) ";: Color 15: Print "HUMAN COACHES WILL: "; coachMode$(scheduleNG%(gameIndex%, 9))
        Locate 11, 2: Color 14: Print "(A) ";: Color 15: Print "GAME DATE (XX-XX-XX) "; DT$; "   "
        Locate 12, 2: Color 14: Print "(B) ";: Color 15: Print "GAME TIME (XX:XXP) "; TM$; "   "
        Locate 13, 2: Color 14: Print "(C) ";: Color 15: Print "PLAY IN ERA OF TEAM (IF DIFFERENT) "; teamEraOpt$(scheduleNG%(gameIndex%, 6))
        Locate 14, 2: Color 14: Print "(D) ";: Color 15: Print "BONUS AFTER FOUL # "; scheduleNG%(gameIndex%, 14)
        Locate 15, 2: Color 14: Print "(E) ";: Color 15: Print "NUMBER OF FREE THROWS "; freeThrowOpt$(scheduleNG%(gameIndex%, 10))
        Locate 16, 2: Color 14: Print "(F) ";: Color 15: Print "FREE THROW RULES "; foulOption$(scheduleNG%(gameIndex%, 11))
        Locate 17, 2: Color 14: Print "(0) ";: Color 15: Print "ACCEPT SETTINGS"

        Do
            I$ = GetKeyPress$

            Select Case UCase$(I$)
                Case "A": I$ = "11"
                Case "B": I$ = "12"
                Case "C": I$ = "13"
                Case "D": I$ = "14"
                Case "E": I$ = "15"
                Case "F": I$ = "16"
            End Select

            setting = Val(I$)

            Select Case setting
                Case 1
                    scheduleNG%(gameIndex%, 8) = scheduleNG%(gameIndex%, 8) + 1
                    If scheduleNG%(gameIndex%, 8) > 3 Then scheduleNG%(gameIndex%, 8) = 0

                Case 2, 3:

                    Call ChooseTeam (YN$, NTMS, teamNbr, team$)
                    
                    If teamNbr <> 0 Then

                        Color 15, 0
                        scheduleNG%(gameIndex%, setting - 2) = teamNbr
                        scheduleYN$(gameIndex%, setting - 2) = YN$

                        If setting = 2 Then
                            visitor$ = team$
                            scheduleV$(gameIndex% - Int(gameIndex% / 20) * 20) = team$
                        End If

                        If setting = 3 Then
                            home$ = team$
                            scheduleH$(gameIndex% - Int(gameIndex% / 20) * 20) = team$
                        End If

                    End If

                Case 4
                    scheduleNG%(gameIndex%, 3) = 1 - scheduleNG%(gameIndex%, 3)

                Case 5
                    scheduleNG%(gameIndex%, 7) = 1 - scheduleNG%(gameIndex%, 7)

                Case 6
                    N = 1 - (scheduleNG%(gameIndex%, 2) - 1)
                    scheduleNG%(gameIndex%, 2) = N + 1

                Case 7
                    Locate 7, 32: Input displayPause!
                    scheduleNG%(gameIndex%, 4) = displayPause! * 10

                Case 8
                    scheduleNG%(gameIndex%, 5) = 1 - scheduleNG%(gameIndex%, 5)

                Case 9
                    scheduleNG%(gameIndex%, 9) = 1 - scheduleNG%(gameIndex%, 9)

                Case 10: ' Back to beginning of loop

                Case 11
                    Locate 11, 27: Input DT$

                Case 12
                    Locate 12, 25: Input TM$

                Case 13
                    scheduleNG%(gameIndex%, 6) = scheduleNG%(gameIndex%, 6) + 1
                    If scheduleNG%(gameIndex%, 6) > 2 Then scheduleNG%(gameIndex%, 6) = 0

                Case 14
                    scheduleNG%(gameIndex%, 14) = scheduleNG%(gameIndex%, 14) + 1
                    If scheduleNG%(gameIndex%, 14) > 6 Then scheduleNG%(gameIndex%, 14) = 4

                Case 15
                    scheduleNG%(gameIndex%, 10) = 1 - scheduleNG%(gameIndex%, 10)

                Case 16
                    scheduleNG%(gameIndex%, 11) = 1 - scheduleNG%(gameIndex%, 11)

            End Select

        Loop Until (setting >= 0 And setting <= 16)

        If visitor$ <> "" And home$ <> "" And TM$ <> "" And DT$ <> "" Then

            I$ = Right$(TM$, 1)
            TM$ = Left$(TM$, Len(TM$) - 1)
            MIN% = Val(Right$(TM$, 2))
            TM$ = Left$(TM$, Len(TM$) - 3)
            HR% = Val(TM$)

            If UCase$(I$) = "P" And HR% < 12 Then HR% = HR% + 12

            scheduleNG%(gameIndex%, 15) = HR% * 100 + MIN%
            scheduleNG%(gameIndex%, 17) = Val(Left$(DT$, 2))
            scheduleNG%(gameIndex%, 18) = Val(Mid$((DT$), 7, 2))
            scheduleNG%(gameIndex%, 16) = Val(Mid$((DT$), 4, 2))

        End If

    Loop Until setting = 0

End Sub 'New Game

'----------------------------------------
'       ExtractSchedule SubRoutine
'----------------------------------------
'...explanation...
Sub ExtractSchedule (scheduleAction%, scheduleFile$)
    'scheduleAction% = 3: EXTRACT ONE TEAM'S SCHEDULE FROM LEAGUE SCHEDULE
    'scheduleAction% = 4: CHANGE THE MODE OF PLAY FOR A TEAM IN A SCHEDULE
    'scheduleAction% = 5: MAKE GLOBAL CHANGES TO A SCHEDULE
    Dim displayPause!

    Shared N$

    Shared homeScores(), visitorScores()

    Shared scheduleAP%()
    Shared scheduleNG%()
    Shared teamRatings%()
    Shared scheduleYN$()

    Select Case scheduleAction%

        Case 5
            ' GLOBAL SETTINGS FOR A SCHEDULE
            U5% = 3
            LO% = 1
            SC% = 0
            coachOpt = 1
            RC% = 1
            PO% = 0
            NF% = 4
            FT% = 0
            FR% = 0
            Color 15, 0
            Cls
            Print Tab(30); "GLOBAL SETTINGS FOR SCHEDULE"
            Call ChangeGlobalSettings
            Call SaveScheduleFile(scheduleFile$)
            Call NewSchedule(scheduleFile$)


        Case 3, 4

            If scheduleAction% = 3 Then

                Color 15, 0
                Cls

                JJ = 7

                Locate 6, 8: Color 15, 1

                'Print box top
                Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

                'Print box sides
                For II = 1 To JJ
                    Locate 6 + II, 8
                    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                Next

                'Print box bottom
                Locate 7 + JJ, 8
                Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

                Do
                    Locate 7, 32
                    Locate , 12: Print "ENTER NAME FOR SUB-SCHEDULE: ": Print
                    Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPACES"
                    Locate , 12: Print "DO NOT ADD A FILE EXTENSION; THE PROGRAM"
                    Locate , 12: Print "WILL AUTOMATICALLY ADD .SCD"
                    Locate , 12: Print "SAMPLE NAMES ARE INDIANA, BOS68, CHI-76, ETC      "
                    Locate 7, 37: Input subSchedFile$

                    If subSchedFile$ <> "" Then
                        Do
                            Locate 13, 12: Print "ANY CHANGE (YN) "
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)
                    End If

                Loop Until (UCase$(I$) = "N" And subSchedFile$ <> "") Or I$ = Chr$(27)

            End If

            If I$ <> Chr$(27) Or scheduleAction% = 4 Then

                Call ChooseTeam (YN$, numberTeams, T1, N$)

                If T1 <> 0 Then

                    Color 15, 0
                    Cls

                    'Change mode of play
                    If scheduleAction% = 4 Then
                        Color teamRatings%(33), teamRatings%(34): Print N$;
                        Color 15, 0: Print " IS TO BE:"
                        Print
                        Print "(0) MANUALLY CONTROLLED": Print
                        Print "(1) COMPUTER CONTROLLED"

                        Do
                            I$ = GetKeyPress$
                            I1 = Val(I$)
                        Loop Until I1 = 0 Or I1 = 1

                        Input "ENTER DISPLAY PAUSE FOR THESE GAMES "; displayPause!

                    End If

                    For I = 1 To scheduleAP%(0)

                        scheduleYN$(I, 0) = RTrim$(scheduleYN$(I, 0))
                        scheduleYN$(I, 1) = RTrim$(scheduleYN$(I, 1))

                        If T1 = scheduleNG%(I, 0) And YN$ = scheduleYN$(I, 0) Or T1 = scheduleNG%(I, 1) And YN$ = scheduleYN$(I, 1) Then

                            ' CHANGE SETTINGS FOR A TEAM
                            If T1 = scheduleNG%(I, 0) And YN$ = scheduleYN$(I, 0) Then scheduleNG%(I, 8) = 2
                            If T1 = scheduleNG%(I, 1) And YN$ = scheduleYN$(I, 1) Then scheduleNG%(I, 8) = 1
                            If (T1 = scheduleNG%(I, 0) And YN$ = scheduleYN$(I, 0) Or T1 = scheduleNG%(I, 1) And YN$ = scheduleYN$(I, 1)) And I1 = 1 And scheduleAction% = 4 Then scheduleNG%(I, 8) = 3
                            scheduleNG%(I, 4) = displayPause! * 10

                            'Extract only
                            If Not (scheduleAction% = 4) Then
                                'Write matching games
                                Locate 1, 1: Print numberGames% + 1; " MATCHES"

                                Open diskPaths$(0) + subSchedFile$ + ".SCD" For Random As #1 Len = SCHEDULE_SIZE_BYTES
                                fileLength& = LOF(1)

                                numberGames% = fileLength& / SCHEDULE_SIZE_BYTES

                                For X = 0 To 18
                                    Field #1, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
                                Next

                                Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)

                                For X = 0 To 18: 
                                    LSet Q$(X + 1) = MKI$(scheduleNG%(I, X))
                                Next
                                
                                For X = 0 To 1: 
                                    LSet Q$(20 + X) = scheduleYN$(I, X)
                                Next

                                LSet Q$(22) = MKI$(visitorScores(I)): LSet Q$(23) = MKI$(homeScores(I))

                                Put #1, numberGames% + 1

                                Close 1

                            End If 'Done checking schedule action

                        End If 'Done checking for matches

                    Next 'Done looping through games

                End If

            End If 'Done checking for ESC

    End Select

End Sub

'----------------------------------------
'       EditParameters SubRoutine
'----------------------------------------
'...explanation...
Sub EditParameters ()

    Dim E%(13)

    'Read in existing parameters if there are any
    If _FileExists("EVENTS.BOX") Then
        Open "EVENTS.BOX" For Input As #1
        For X = 1 To 13
            Input #1, E%(X)
        Next
        Close 1
    End If

    Cls
    Color 14
    Print "PARAMETERS FOR SAVING SPECIAL EVENTS BOXSCORES"

    X% = 2
    Color 15

    Print " 1. QUARTERS ARE OVER "; E%(1)
    Print " 2. TOTAL POINTS GREATER THAN "; E%(2)
    Print " 3. TOTAL REBOUNDS GREATER THAN "; E%(3)
    Print " 4. TEAM POINTS GREATER THAN "; E%(4)
    Print " 5. TEAM REBOUNDS GREATER THAN "; E%(5)
    Print " 6. PLAYER'S POINTS MORE THAN "; E%(6)
    Print " 7. PLAYER'S REBOUNDS MORE THAN "; E%(7)
    Print " 8. PLAYER'S ASSISTS MORE THAN "; E%(8)
    Print " 9. PLAYER'S BLOCKS MORE THAN "; E%(9)
    Print "10. PLAYER'S STEALS MORE THAN "; E%(10)
    Print "11. PLAYER'S 3-FGM MORE THAN "; E%(11)

    C1 = 1

    Do

        Locate 23, 1: Print "SELECT # TO CHANGE 1-11 (0 TO CONTINUE) ";
        Input B9

        Select Case B9

            Case 1: Locate 2: Input " 1. QUARTERS ARE OVER "; E%(1)
            Case 2: Locate 3: Input " 2. TOTAL POINTS GREATER THAN "; E%(2)
            Case 3: Locate 4: Input " 3. TOTAL REBOUNDS GREATER THAN "; E%(3)
            Case 4: Locate 5: Input " 4. TEAM POINTS GREATER THAN "; E%(4)
            Case 5: Locate 6: Input " 5. TEAM REBOUNDS GREATER THAN "; E%(5)
            Case 6: Locate 7: Input " 6. PLAYER'S POINTS MORE THAN "; E%(6)
            Case 7: Locate 8: Input " 7. PLAYER'S REBOUNDS MORE THAN "; E%(7)
            Case 8: Locate 9: Input " 8. PLAYER'S ASSISTS MORE THAN "; E%(8)
            Case 9: Locate 10: Input " 9. PLAYER'S BLOCKS MORE THAN "; E%(9)
            Case 10: Locate 11: Input "10. PLAYER'S STEALS MORE THAN "; E%(10)
            Case 11: Locate 12: Input "11. PLAYER'S 3-FGM MORE THAN "; E%(11)

        End Select

    Loop Until B9 = 0 'ENTER also counts as 0

    'Save all changes
    Open "EVENTS.BOX" For Output As #1

    For X = 1 To 13
        Print #1, E%(X)
    Next

    Close 1

End Sub

'----------------------------------------
'    BoxscoreMaintenance SubRoutine
'----------------------------------------
'...explanation...
Sub BoxscoreMaintenance ()

    ReDim menuChoices$(1 To 6)

    menuChoices$(1) = "SET PARAMETERS TO SAVE SPECIAL EVENTS BOXSCORES  "
    menuChoices$(2) = "ACCESS USER SAVED AND SPECIAL EVENTS BOXSCORES   "
    menuChoices$(3) = "DELETE ALL SCHEDULE GENERATED BOXSCORES          "
    menuChoices$(4) = "DELETE ALL USER SAVED, & SPECIAL BOXSCORES       "
    menuChoices$(5) = "DELETE ALL BOXSCORES (SCHEDULE, USER, &  SPECIAL)"
    menuChoices$(6) = "RETURN TO MAIN AUTO SCHEDULER MENU               "
    Selection% = 1
    Count% = 6
    Start% = 1
    ExitCode% = 6
    FileFGColr% = 15: FileBGColr% = 1
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Spacing% = 1
    Row% = 8
    Column% = 16

    Do

        Call TITLE_SCHEDULER

        Call box0(6, 15, 13, 65, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            Select Case Selection%
                Case 1:
                    Call EditParameters

                Case 2
                    Spec$ = diskPaths$(1) + "*.SPC"
                    Count% = FCount%(Spec$)

                    If Count% = 0 Then
                        Locate 10, 8
                        Call CSMsgBox("NO SPECIAL EVENTS BOXSCORES WERE FOUND PRESS ANY KEY TO RETURN TO MENU", 26, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)
                    Else
                        ReDim boxFiles$(0 To Count%)
                        For X = 1 To Count%
                            boxFiles$(X) = Space$(12)
                        Next
                        
                        boxFiles$(0) = Spec$
                        Call ReadFile(boxFiles$())

                        Do
                            Cls
                            Color , 1
                            Cls
                            Locate , 28
                            Color 14
                            Print "SELECT BOXSCORE TO VIEW": Color 15
                            Selection% = 1
                            Start% = 1
                            FileFGColr% = 15: FileBGColr% = 1
                            HiLiteFGColr% = 0: HiLiteBGColr% = 7
                            Rows% = 20
                            Columns% = 4
                            Spacing% = 6
                            Row% = 4
                            Column% = 1
                            Locate 25, 1
                            Color 14
                            Print "ENTER";: Color 15
                            Print "-SELECTS BOXSCORE";

                            _MouseShow "DEFAULT"
                            MAMenu boxFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                            _MouseHide

                            If ExitCode% <> 27 Then
                                Color 15, 1
                                targetFile$ = boxFiles$(Selection%)
                                Locate 2, 33: Print targetFile$

                                Do
                                    Locate 3, 33: Print "ANY CHANGE (YN)";
                                    I$ = GetKeyPress$
                                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                                Color 15, 0

                                If UCase$(I$) = "N" Then
                                    boxName$ = diskPaths$(1) + targetFile$
                                    'ReDim array$(500) 'holds the main editor string array
                                End If
                            End If

                        Loop Until UCase$(I$) = "N" Or ExitCode% = 27

                    End If

                Case 3
                    Spec$ = diskPaths$(1) + "BOX*."
                    Call DeleteSchedules(Selection%, Spec$)
                    '-- we should give the user a confirmation message

                Case 4
                    Spec$ = diskPaths$(1) + "*.SPC"
                    Call DeleteSchedules(Selection%, Spec$)
                    '-- we should give the user a confirmation message

                Case 5
                    Spec$ = diskPaths$(1) + "*.SPC"
                    Call DeleteSchedules(Selection%, Spec$)
                    '-- we should give the user a confirmation message

                    Spec$ = diskPaths$(1) + "BOX*."
                    Call DeleteSchedules(Selection%, Spec$)
                    '-- we should give the user a confirmation message

                Case 6:
                    Cls

            End Select

        End If

    Loop Until Selection% = 6 Or ExitCode% = 27

End Sub

'----------------------------------------
'       SaveScheduleFile SubRoutine
'----------------------------------------
'...explanation...
Sub SaveScheduleFile (saveFile$)

    Shared homeScores(), visitorScores()
    Shared scheduleAP%(), scheduleNG%()
    Shared scheduleYN$()

    If _FileExists(diskPaths$(0) + saveFile$ + ".SCD") Then Kill diskPaths$(0) + saveFile$ + ".SCD"

    Open diskPaths$(0) + saveFile$ + ".SCD" For Random As #1 Len = SCHEDULE_SIZE_BYTES

    For X = 0 To 18
        Field #1, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
    Next

    Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)

    For I = 1 To scheduleAP%(0)

        For X = 0 To 18
            LSet Q$(X + 1) = MKI$(scheduleNG%(I, X))
        Next

        For X = 0 To 1
            LSet Q$(20 + X) = RTrim$(scheduleYN$(I, X))
        Next

        LSet Q$(22) = MKI$(visitorScores(I))
        LSet Q$(23) = MKI$(homeScores(I))

        Put #1, I
    Next

    Close 1

End Sub

'----------------------------------------
'       SortSchedule SubRoutine
'----------------------------------------
'...explanation...
Sub SortSchedule ()

    Dim schedDates&(MAX_GAMES)
    Dim D&, M&, Y&

    Shared homeScores(), visitorScores()
    Shared scheduleAP%(), scheduleNG%()
    Shared scheduleYN$()

    For I = 1 To scheduleAP%(0)
        HR% = Int(scheduleNG%(I, 15) / 100)
        MIN% = scheduleNG%(I, 15) - HR% * 100
        TM% = HR% * 60 + MIN%
        scheduleNG%(I, 12) = Int(TM% / 30 - 18)
        D& = (scheduleNG%(I, 18) - 1)
        D& = D& * 372
        M& = (scheduleNG%(I, 17) - 1)
        M& = M& * 31
        Y& = scheduleNG%(I, 16)
        schedDates&(I) = (D& + M& + Y&) * 24 * 60 + TM%
    Next

    For Z = 1 To scheduleAP%(0) - 1:
        For Z1 = Z + 1 To scheduleAP%(0)
            Locate 1, 1: Print Z, Z1
            If scheduleNG%(Z, 12) > scheduleNG%(Z1, 12) Then
                For I = 0 To 18: Swap scheduleNG%(Z, I), scheduleNG%(Z1, I): Next
                For I = 0 To 1: Swap scheduleYN$(Z, I), scheduleYN$(Z1, I): Next
                Swap schedDates&(Z), schedDates&(Z1)
                Swap visitorScores(Z), visitorScores(Z1)
                Swap homeScores(Z), homeScores(Z1)
            End If
        Next Z1
    Next Z

    For Z = 1 To scheduleAP%(0) - 1:
        For Z1 = Z + 1 To scheduleAP%(0)
            If schedDates&(Z) > schedDates&(Z1) Then
                For I = 0 To 18: Swap scheduleNG%(Z, I), scheduleNG%(Z1, I): Next
                For I = 0 To 1: Swap scheduleYN$(Z, I), scheduleYN$(Z1, I): Next
                Swap schedDates&(Z), schedDates&(Z1)
                Swap visitorScores(Z), visitorScores(Z1)
                Swap homeScores(Z), homeScores(Z1)
            End If
        Next Z1
    Next Z

End Sub


'----------------------------------------
'    ChangeGlobalSettings SubRoutine
'----------------------------------------
'...explanation...
Sub ChangeGlobalSettings ()

    Shared scheduleAP%(), scheduleNG%()

    Shared coachMode$(), MO$(), modeAbbrev$()
    Shared sClockOpt$()
    Shared yesNo$()

    Dim displayPause!

    DT$ = "10-01-91"
    TM$ = "07:30P"        

    Do
        Color 15, 0
        Cls
        Print Tab(30); "GLOBAL SETTINGS FOR SCHEDULE"

        Locate 3, 2: Color 14: Print "(1) ";: Color 15: Print "TYPE OF GAME: "; MO$(U5%)
        Locate 4, 2: Color 14: Print "(2) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#  "; displayPause!
        Locate 5, 2: Color 14: Print "(3) ";: Color 15: Print "HOME COURT ADVANTAGE: "; location$(LO%)
        Locate 6, 2: Color 14: Print "(4) ";: Color 15: Print "SHOT CLOCK: "; sClockOpt$(SC%)
        Locate 7, 2: Color 14: Print "(5) ";: Color 15: Print "HUMAN PLAYERS: "; coachMode$(coachOpt)
        Locate 8, 2: Color 14: Print "(6) ";: Color 15: Print "3-POINT SHOT: "; yesNo$(RC%)
        Locate 9, 2: Color 14: Print "(7) ";: Color 15: Print "TYPE OF GAME: "; playoffGame$(PO%)
        Locate 10, 2: Color 14: Print "(8) ";: Color 15: Print Using "BONUS AFTER FOUL: ##"; NF%
        Locate 11, 2: Color 14: Print "(9) ";: Color 15: Print "NUMBER OF FREE THROWS "; freeThrowOpt$(FT%)
        Locate 12, 2: Color 14: Print "(A) ";: Color 15: Print "FREE THROW RULES "; foulOption$(FR%)
        Locate 13, 2: Color 14: Print "(0) ";: Color 15: Print "ACCEPT GLOBAL SETTINGS"

        Do
            I$ = GetKeyPress$
            If UCase$(I$) = "A" Then I$ = "10"
            I = Val(I$)
        Loop Until I >= 0 And I <= 10

        Select Case I

            Case 1
                U5% = U5% + 1
                If U5% > 3 Then U5% = 0

            Case 2
                Input "DISPLAY VARIABLE "; displayPause!

            Case 3
                LO% = 1 + LO%
                If LO% > 2 Then LO% = 1

            Case 4:
                SC% = 1 - SC%

            Case 5:
                coachOpt = 1 - coachOpt

            Case 6:
                RC% = 1 - RC%

            Case 7:
                PO% = 1 - PO%

            Case 8
                NF% = NF% + 1
                If NF% > 6 Then NF% = 4

            Case 9:
                FT% = 1 - FT%

            Case 10:
                FR% = 1 - FR%

        End Select

    Loop Until I = 0

    For I = 1 To scheduleAP%(0)
        scheduleNG%(I, 8) = U5%
        scheduleNG%(I, 4) = displayPause! * 10
        scheduleNG%(I, 2) = LO%
        scheduleNG%(I, 7) = SC%
        scheduleNG%(I, 9) = coachOpt
        scheduleNG%(I, 3) = RC%
        scheduleNG%(I, 5) = PO%
        scheduleNG%(I, 10) = FT%
        scheduleNG%(I, 11) = FR%
    Next

End Sub

'----------------------------------------
'       LoadSchedule SubRoutine
'----------------------------------------
'...explanation...
Sub LoadSchedule (targetFile$)

    Do
        'Display list of files...
        Spec$ = diskPaths$(0) + "*.SCD"
        Count% = FCount%(Spec$)

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO SCHEDULE FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else
            ReDim scheduleFiles$(0 To Count%)

            For X = 1 To Count%
                scheduleFiles$(X) = Space$(12)
            Next

            scheduleFiles$(0) = Spec$
            Call ReadFile(scheduleFiles$())
            Call SelectScheduleFile(targetFile$, scheduleFiles$(), Count%, menuExit%)

        End If

    Loop Until Count% = 0 Or targetFile$ <> "" Or menuExit% = 27

End Sub


'----------------------------------------
'       DeleteSchedules Subroutine       
'----------------------------------------
'...explanation...
Sub DeleteSchedules (fileChoice%, fileSpec$)

    Spec$ = diskPaths$(0) + fileSpec$
    FileCount% = FCount%(Spec$)

    If FileCount% = 0 Then

        Select Case fileChoice%

            Case 3
                Locate 10, 8
                Call CSMsgBox("NO BOXSCORES WERE FOUND PRESS ANY KEY TO RETURN TO MENU", 26, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)

            Case 4
                Locate 10, 8
                Call CSMsgBox("NO SPECIAL EVENTS BOXSCORES WERE FOUND PRESS ANY KEY TO RETURN TO MENU", 26, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)

            Case 5
                ' Exit Sub

        End Select

    Else

        ReDim boxFiles$(0 To FileCount%)

        For X = 1 To FileCount%
            boxFiles$(X) = Space$(12)
        Next

        boxFiles$(0) = fileSpec$
        Call ReadFile(boxFiles$())

        For X = 1 To FileCount%
            Kill (diskPaths$(1) + boxFiles$(X))
            Locate 12, 34: Print "DELETING "; boxFiles$(X)
        Next

    End If

End Sub


'----------------------------------------
'     SelectScheduleFile SubRoutine
'----------------------------------------
'...explanation...
Sub SelectScheduleFile (targetFile$, scheduleFiles$(), FileCount%, ExitCode%)

    Do
        Cls
        Color , 1
        Cls
        Locate , 28
        Color 14
        Print "SELECT SCHEDULE TO VIEW"
        Color 15
        Selection% = 1
        Start% = 1
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Rows% = 20
        Columns% = 4
        Spacing% = 6
        Row% = 4
        Column% = 1
        Locate 25, 1
        Color 14
        Print "ESC";: Color 15
        Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
        Print "ENTER";: Color 15
        Print "-SELECTS SCHEDULE";

        _MouseShow "DEFAULT"
        MAMenu scheduleFiles$(), Selection%, Start%, FileCount%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then
            Color 15, 1
            targetFile$ = RTrim$(scheduleFiles$(Selection%))
            Locate 2, 33: Print targetFile$

            Do
                Locate 3, 33: Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            Color 15, 0

        End If

    Loop Until ExitCode% = 27 Or UCase$(I$) = "N"

    If ExitCode% <> 27 Then

        targetFile$ = RTrim$(targetFile$)

        '-- This is where we strip the file extension
        '-- If we remove this, we can drop the .SCD in the rest of the code
        fileNoExt$ = Left$(targetFile$, Len(targetFile$) - 4)
        targetFile$ = fileNoExt$
        Color 15, 0
        
        Call LoadScheduleInfo (targetFile$)

    End If

End Sub


'----------------------------------------
'      LoadScheduleInfo SubRoutine
'----------------------------------------
'...explanation...
Sub LoadScheduleInfo (targetFile$)

    Shared BS%, NS%

    Shared scheduleAP%(), scheduleNG%()
    Shared homeScores(), visitorScores()
    Shared scheduleYN$()

    Open diskPaths$(0) + targetFile$ + ".SCD" For Random As #1 Len = SCHEDULE_SIZE_BYTES
    fileLength& = LOF(1)

    scheduleAP%(0) = fileLength& / SCHEDULE_SIZE_BYTES
    BS% = Int((scheduleAP%(0) - 1) / 20)

    For X = 0 To 18
        Field #1, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
    Next

    Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)

    For I = 1 To scheduleAP%(0)
        Get #1, I

        For X = 0 To 18: 
            scheduleNG%(I, X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 1: 
            scheduleYN$(I, X) = RTrim$(Q$(20 + X))
        Next

        visitorScores(I) = CVI(Q$(22))
        homeScores(I) = CVI(Q$(23))

    Next

    Close 1

    NS% = 0

End Sub


'----------------------------------------
'       EditBoxscore SubRoutine
'----------------------------------------
'...explanation...
Sub EditBoxscore (boxName$)

    F2$ = Chr$(0) + Chr$(60) 'extended code for the F2 function key
    F3$ = Chr$(0) + Chr$(61) 'extended code for the F3 function key

    If Not _FileExists(boxName$) Then
        Locate 10, 8
        Call CSMsgBox("BOXSCORE DOES NOT EXIST PRESS ANY KEY TO RETURN TO MAIN MENU", 23, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
    Else

        Open boxName$ For Input As #1
        CURLINE = 1
        While Not EOF(1)
            Line Input #1, array$(CURLINE)
            CURLINE = CURLINE + 1
        Wend
        Close #1

        Cls '  show how QEdit saves the screen

        Color 15, 0 'print instructions on bottom of screen
        Locate 25, 1
        Print " ESC - Quit    F1 - Help    F2 - Print Boxscore   F3 - Save Boxscore"; Tab(79);

        '----- fill in editing parameters
        Select Case Monitor% 'determine the monitor type
            Case 3, 5, 7, 9, 10, 12 'CGA, EGA, VGA monitors
                Ed.AColor = 23 'white on blue
            Case Else 'all other monitors
                Ed.AColor = 7 'white on black
        End Select
        Ed.Wrap = 0 'word wrap at column 65 (use 0 for to disable word wrap)
        Ed.Frame = 0 'frame code (1 = show ruler line & scroll bars, 0 = don't)
        Ed.Rows = 24 'number of text lines to display on the screen (3 - 25)
        Ed.Wide = 80 'width of the active screen (10 - 80)
        Ed.HTab = 4 'set tab stops to 4 spaces
        Locate 1, 1 'locate the upper left corner of the edit window frame
        Action = 1 'Action 1 to save underlying screen and
        '  display text, operate in polled mode

        '----- main loop for editing
        Do
            '----- call QEdit in polled mode
            'CALL QEdit(array$(), Ky$, Action, Ed)
            '----- F2 function key pressed? - change margin
            If Ky$ = F2$ Then: For I = 1 To CURLINE - 1: LPrint array$(I): Next
            If Ky$ = F3$ Then
                Open BN$ For Output As #1
                For I = 1 To CURLINE - 1
                    Print #1, array$(I)
                Next
                Close 1

                Call CSMsgBox("REVISED BOXSCORE HAS BEEN SAVED...PRESS ANY KEY TO CONTINUE", 31, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)

            End If

        Loop Until Ky$ = Chr$(27) 'keep calling QEdit until Esc is pressed

        '----- call QEdit again with an Action flag of 5 to restore the screen
        'CALL QEdit(array$(), Ky$, 5, Ed)

    End If

End Sub

