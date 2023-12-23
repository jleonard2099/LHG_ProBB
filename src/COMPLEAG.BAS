'----------------------------------------
'        CompileLeague SubRoutine
'----------------------------------------
'This subroutine reads in existing records,
'looks for stats matching each team in the
'league, and then relies on subroutines to
'read stats and compare to any existing
'leaders and records
Sub CompileLeague ()

    _Title "Basketball: The Pro Game - Update League Stats"

    Shared teamScore(), oppScore()

    Shared plyrRecords!()
    Shared DT!(), DL!()
    Shared LD!(), leagS!()
    Shared O1!(), O2!(), OT!(), OL!()
    Shared offRecords!(), defRecords!(), statsW0!(), statsW1!()

    Shared plyrRecDesc$(), DL$(), H1$()
    Shared N$(), OL$()
    Shared locIndicator$(), oppName$()
    Shared statsZ1$(), offRecDesc$(), defRecDesc$()
    Shared Z2$(), Z3$()

    'Read in any existing records first
    If _FileExists(diskPaths$(1) + "RECORDS.PBB") Then

        Open diskPaths$(1) + "RECORDS.PBB" For Input As #1

        For I = 1 To 25
            Input #1, plyrRecords!(I)
            Input #1, plyrRecDesc$(I, 0)
            Input #1, plyrRecDesc$(I, 1)
        Next

        For I = 1 To 21
            Input #1, offRecords!(I)
            Input #1, offRecDesc$(I)
        Next

        For I = 1 To 21
            Input #1, defRecords!(I)
            Input #1, defRecDesc$(I)
        Next

        Close #1

    Else

        Erase plyrRecords!, offRecords!, defRecords!
        Erase plyrRecDesc$, offRecDesc$, defRecDesc$

    End If

    'Initialize values first

    'These are defensive record values, so they should be really high
    'To determine if a "record" was broken, the stat values will be
    'compared to see if they are lower than this.
    'If we initialized to 0, the "record" would always be 0
    offRecords!(14) = 19999
    offRecords!(18) = 19999

    For I = 1 To 13:
        defRecords!(I) = 19999
    Next

    For I = 15 To 17
        defRecords!(I) = 19999
    Next

    For I = 19 To 20:
        defRecords!(I) = 19999
    Next

    For X = 1 To TEAMS_PER_CONFERENCE:
        DL!(X, 19) = 9999
    Next

    Erase LD!, OL!
    Erase Z2$, Z3$

    C$ = ","

    leagueFile$ = ChooseFile$(diskPaths$(1) + "*.LIB", "LEAGUE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Do
            Color 15, 0
            Cls
            Print "WOULD YOU LIKE TO POST SEASON LEADERS? (Y/N)";
            I$ = GetKeyPress$
            finalUpdate$ = UCase$(I$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

        Open diskPaths$(1) + leagueFile$ + ".TMS" For Input As #2

        Input #2, league$, D1$, D2$, D3$, D4$

        For XW = 1 To TEAMS_PER_CONFERENCE
            Input #2, H1$(XW)
            statsZ1$(XW) = H1$(XW)
        Next

        Close #2

        For XW = 1 To TEAMS_PER_CONFERENCE

            'Look for * as terminating char
            If H1$(XW) <> Chr$(42) Then

                haveStats = FindStatsLeague%(H1$(XW), totalGames, fullWins, fullLosses)

                If (haveStats) Then

                    L = Len(H1$(XW))
                    WT$ = ""

                    For X = 1 To L
                        A = Asc(Mid$(H1$(XW), X, 1))
                        WT$ = WT$ + Chr$(A)
                    Next X

                    confGame = 0

                    For X = 1 To TEAMS_PER_CONFERENCE

                        If statsZ1$(X) = WT$ Then

                            If O1!(X) = fullWins And O2!(X) = fullLosses Then
                                confGame = 1
                            Else
                                O1!(X) = fullWins
                                O2!(X) = fullLosses
                            End If

                        End If

                        If (confGame) Then Exit For

                    Next X

                    If confGame <> 1 And _FileExists(diskPaths$(1) + H1$(XW)) Then

                        Erase DT!, OT!
                        Erase leagS!, statsW0!, statsW1!
                        Erase N$

                        Call ReadStatsPro(H1$(XW), TN$)
                        Call StatsByCategoryPro(TN$, finalUpdate$, fullWins, fullLosses)
                        totalGames = fullWins + fullLosses
                        Call CheckStatsPro(TN$, totalGames)

                    End If 'Done checking for stat file

                End If ' Done checking if we have stats

            End If 'Done checking H1$(XW)

        Next XW

        Call SortLeadersPro
        Call PostLeadersPro(leagueFile$)

    End If

End Sub


'----------------------------------------
'       FindStatsLeague% Function
'----------------------------------------
'...explanation...
Function FindStatsLeague% (statFile$, totalGames, fullWins, fullLosses)

    Shared oppScore(), teamScore()

    Shared H1$(), locIndicator$(), oppName$()

    If Not _FileExists(diskPaths$(1) + statFile$ + "S") Then

        Print "NO STATS FOUND FOR "; statFile$
        FindStatsLeague% = 0

    Else

        Open diskPaths$(1) + statFile$ + "S" For Input As #1

        Input #1, totalGames

        For I = 1 To MAX_SCHED_STATS:
            Input #1, teamScore(I)
        Next I

        For I = 1 To MAX_SCHED_STATS:
            Input #1, oppName$(I)
        Next I

        For I = 1 To MAX_SCHED_STATS:
            Input #1, oppScore(I)
        Next I

        Close #1

        fullWins = 0
        fullLosses = 0
        confWins = 0
        confLosses = 0

        For I = 1 To totalGames

            statName$ = nameStatFile$(oppName$(I))
            oppName$(I) = targetFile$

            confGame = 0

            For EC = 1 To TEAMS_PER_CONFERENCE

                If statName$ = H1$(EC) Then
                    confGame = 1
                    Exit For
                End If

            Next EC

            If confGame <> 1 Then
                If teamScore(I) > oppScore(I) Then fullWins = fullWins + 1
                If teamScore(I) < oppScore(I) Then fullLosses = fullLosses + 1
            Else
                If teamScore(I) > oppScore(I) Then
                    confWins = confWins + 1
                    fullWins = fullWins + 1
                End If
                If teamScore(I) < oppScore(I) Then
                    confLosses = confLosses + 1
                    fullLosses = fullLosses + 1
                End If
            End If

            EC = 0

        Next I

        Cls

        Locate 4, 5

        Color 15
        Print "FINDING ";
        Color 14
        Print statFile$;
        Color 15
        Print " STATS            "

        FindStatsLeague% = 1

    End If

End Function


'----------------------------------------
'       ReadStatsPro Subroutine
'----------------------------------------
'This subroutine reads stats in for a
'single team in the league
Sub ReadStatsPro (statFile$, TN$)

    Shared DT!(), leagS!(), OT!()
    Shared statsW0!(), statsW1!()

    Shared N$()

    Open diskPaths$(1) + statFile$ For Input As #1

    Input #1, TN$

    For I = 0 To 14

        Input #1, N$(I)

        For J = 0 To 13:
            Input #1, leagS!(I, J)
        Next

        Input #1, statsW0!(I)
        Input #1, statsW1!(I)

    Next I

    For L = 1 To 16:
        Input #1, OT!(L)
    Next

    For L = 1 To 16:
        Input #1, DT!(L)
    Next

    Close #1

End Sub


'----------------------------------------
'     CheckStatsPro Subroutine
'----------------------------------------
'This subroutine compares pre-loaded stats
'to existing leaders and updates as needed
Sub CheckStatsPro (TN$, totalGames)

    Shared DT!(), OT!()
    Shared DL!(), OL!()
    Shared LD!(), leagS!()
    Shared statsW0!(), statsW1!()

    Shared DL$(), N$(), OL$()
    Shared Z2$(), Z3$()

    For X = 0 To 14:

        Print "CHECKING STATS OF ";
        Print N$(X)

        'Pts / Game
        '--- Must have played 70% of team's games
        If leagS!(X, 22) > LD!(20, 1) And (leagS!(X, 0) >= (totalGames * 0.7)) Then

            LD!(20, 1) = leagS!(X, 22)
            LD!(20, 2) = 0
            Z2$(20) = N$(X)
            Z3$(20) = TN$

            For Z = 1 To 19
                For Z1 = Z + 1 To 20

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: Swap LD!(Z1, M), LD!(Z, M): Next
                    End If

                Next Z1
            Next Z

        End If

        'Off Reb
        '--- Must have played 70% of team's games
        If leagS!(X, 12) > LD!(40, 1) And (leagS!(X, 0) >= (totalGames * 0.7)) Then

            LD!(40, 1) = leagS!(X, 12)
            LD!(40, 2) = 0
            Z2$(40) = N$(X)
            Z3$(40) = TN$

            For Z = 21 To 39
                For Z1 = Z + 1 To TEAMS_PER_CONFERENCE

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'FG%
        '-- Must have 300 FGM
        If leagS!(X, 19) > LD!(60, 1) And leagS!(X, 2) >= 300 Then

            LD!(60, 1) = leagS!(X, 19) 'FG%
            LD!(60, 2) = leagS!(X, 1) 'FGA
            Z2$(60) = N$(X)
            Z3$(60) = TN$

            For Z = 41 To 59
                For Z1 = Z + 1 To 60

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'FT%
        '-- Must mave 125 FTM
        If leagS!(X, 20) > LD!(80, 1) And (leagS!(X, 4) >= 125) Then

            LD!(80, 1) = leagS!(X, 20) 'FT%
            LD!(80, 2) = leagS!(X, 3) 'FTA
            Z2$(80) = N$(X)
            Z3$(80) = TN$

            For Z = 61 To 79
                For Z1 = Z + 1 To 80

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Steals / Game
        '--- Must have played 70% of team's games
        If leagS!(X, 15) > LD!(90, 1) And (leagS!(X, 0) >= (totalGames * 0.7)) Then

            LD!(90, 1) = leagS!(X, 15)
            LD!(90, 2) = leagS!(X, 7)
            Z2$(90) = N$(X)
            Z3$(90) = TN$

            For Z = 81 To 89
                For Z1 = Z + 1 To 90

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Steals
        '--- Must have played 70% of team's games
        If leagS!(X, 7) > LD!(100, 1) And (leagS!(X, 0) >= (totalGames * 0.7)) Then

            LD!(100, 1) = leagS!(X, 7)
            LD!(100, 2) = 0
            Z2$(100) = N$(X)
            Z3$(100) = TN$

            For Z = 91 To 99
                For Z1 = Z + 1 To 100
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Assists / Game
        '--- Must have played 70% of team's games
        If leagS!(X, 16) > LD!(110, 1) And (leagS!(X, 0) >= (totalGames * 0.7)) Then

            LD!(110, 1) = leagS!(X, 16)
            LD!(110, 2) = leagS!(X, 8)
            Z2$(110) = N$(X)
            Z3$(110) = TN$

            For Z = 101 To 109
                For Z1 = Z + 1 To 110

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Assists
        '--- Must have played 70% of team's games
        If leagS!(X, 8) > LD!(120, 1) Then

            LD!(120, 1) = leagS!(X, 8)
            LD!(120, 2) = leagS!(X, 8)
            Z2$(120) = N$(X)
            Z3$(120) = TN$

            For Z = 111 To 119
                For Z1 = Z + 1 To 120

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        '3 FG %
        '-- Must have 82 3FG's made; assumes 82 game schedule
        If leagS!(X, 21) > LD!(130, 1) And statsW0!(X) >= totalGames Then

            LD!(130, 1) = leagS!(X, 21)
            LD!(130, 2) = statsW1!(X)
            Z2$(130) = N$(X)
            Z3$(130) = TN$

            For Z = 121 To 129
                For Z1 = Z + 1 To 130
                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        '3 FGM
        '-- Must have 82 3FG's made; assumes 82 game schedule
        If statsW0!(X) > LD!(140, 1) And statsW0!(X) >= totalGames Then

            LD!(140, 1) = statsW0!(X)
            LD!(140, 2) = 0
            Z2$(140) = N$(X)
            Z3$(140) = TN$

            For Z = 131 To 139
                For Z1 = Z + 1 To 140

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Minutes / Game
        '--- Must have played 70% of team's games
        If leagS!(X, 18) > LD!(150, 1) And (leagS!(X, 0) >= (totalGames * 0.7)) Then

            LD!(150, 1) = leagS!(X, 18)
            LD!(150, 2) = 0
            Z2$(150) = N$(X)
            Z3$(150) = TN$

            For Z = 141 To 149
                For Z1 = Z + 1 To 150

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Total Minutes
        '--- Must have played 70% of team's games
        If leagS!(X, 10) / 60 > LD!(160, 1) And (leagS!(X, 0) >= (totalGames * 0.7)) Then

            LD!(160, 1) = leagS!(X, 10) / 60
            LD!(160, 2) = 0
            Z2$(160) = N$(X)
            Z3$(160) = TN$

            For Z = 151 To 159
                For Z1 = Z + 1 To 160

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Rebounds / Game
        '--- Must have played 70% of team's games
        If leagS!(X, 13) > LD!(170, 1) And (leagS!(X, 0) >= (totalGames * 0.7)) Then

            LD!(170, 1) = leagS!(X, 13)
            LD!(170, 2) = leagS!(X, 5)
            Z2$(170) = N$(X)
            Z3$(170) = TN$

            For Z = 161 To 169
                For Z1 = Z + 1 To 170

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Avg Rebounds
        '--- Must have played 70% of team's games
        If leagS!(X, 5) > LD!(180, 1) And (leagS!(X, 0) >= (totalGames * 0.7)) Then

            LD!(180, 1) = leagS!(X, 5)
            LD!(18, 2) = 0
            Z2$(180) = N$(X)
            Z3$(180) = TN$

            For Z = 171 To 179
                For Z1 = Z + 1 To 180

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Blocks / Game
        '--- Must have played 70% of team's games
        If leagS!(X, 17) > LD!(190, 1) And (leagS!(X, 0) >= (totalGames * 0.7)) Then

            LD!(190, 1) = leagS!(X, 17)
            LD!(190, 2) = 0
            Z2$(190) = N$(X)
            Z3$(190) = TN$

            For Z = 181 To 189
                For Z1 = Z + 1 To 190

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Blocks
        '--- Must have played 70% of team's games
        If leagS!(X, 9) > LD!(200, 1) And (leagS!(X, 0) >= (totalGames * 0.7)) Then

            LD!(200, 1) = leagS!(X, 9)
            LD!(200, 2) = 0
            Z2$(200) = N$(X)
            Z3$(200) = TN$

            For Z = 191 To 199
                For Z1 = Z + 1 To 200

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z
        End If

        'Field Goals Made
        If leagS!(X, 2) > LD!(210, 1) Then

            LD!(210, 1) = leagS!(X, 2)
            LD!(210, 2) = 0
            Z2$(210) = N$(X)
            Z3$(210) = TN$

            For Z = 201 To 209
                For Z1 = Z + 1 To 210

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Free Throws Made
        If leagS!(X, 4) > LD!(220, 1) Then

            LD!(220, 1) = leagS!(X, 4)
            LD!(220, 2) = 0
            Z2$(220) = N$(X)
            Z3$(220) = TN$

            For Z = 211 To 219
                For Z1 = Z + 1 To 220

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Fouls / Game
        If leagS!(X, 14) > LD!(230, 1) Then

            LD!(230, 1) = leagS!(X, 14)
            LD!(230, 2) = 0
            Z2$(230) = N$(X)
            Z3$(230) = TN$

            For Z = 221 To 229
                For Z1 = Z + 1 To 230

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Fouls
        If leagS!(X, 6) > LD!(240, 1) Then

            LD!(240, 1) = leagS!(X, 6)
            LD!(240, 2) = 0
            Z2$(240) = N$(X)
            Z3$(240) = TN$

            For Z = 231 To 239
                For Z1 = Z + 1 To 240

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'MVP Points
        If leagS!(X, 24) > LD!(250, 1) Then

            LD!(250, 1) = leagS!(X, 24)
            LD!(250, 2) = 0
            Z2$(250) = N$(X)
            Z3$(250) = TN$

            For Z = 241 To 249
                For Z1 = Z + 1 To 250

                    If LD!(Z, 1) < LD!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap LD!(Z1, M), LD!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z
        End If

    Next X

    'Offensive Totals
    'FOR X=1 TO TEAMS_PER_CONFERENCE
    If OT!(19) > OL!(40, 19) Then

        For M = 1 To 20
            OL!(40, M) = OT!(M)
        Next

        OL$(40) = TN$

        For X = 1 To 39
            For X1 = 1 + X To TEAMS_PER_CONFERENCE

                If OL!(X, 19) < OL!(X1, 19) Then
                    Swap OL$(X1), OL$(X)
                    For M = 1 To 20
                        Swap OL!(X1, M), OL!(X, M)
                    Next
                End If

            Next X1
        Next X

    End If

    'Defensive Totals
    'FOR X=1 TO 15
    If DT!(19) < DL!(40, 19) Then

        For M = 1 To 20
            DL!(40, M) = DT!(M)
        Next

        DL$(40) = TN$

        For X = 1 To 39
            For X1 = 1 + X To TEAMS_PER_CONFERENCE

                If DL!(X, 19) > DL!(X1, 19) Then
                    Swap DL$(X1), DL$(X)
                    For M = 1 To 20
                        Swap DL!(X1, M), DL!(X, M)
                    Next
                End If

            Next X1
        Next X

    End If

End Sub


'----------------------------------------
'     StatsByCategoryPro Subroutine
'----------------------------------------
'This subroutine compares all pre-loaded
'stats to each of the record categories to
'see if any should be updated
Sub StatsByCategoryPro (TN$, finalUpdate$, fullWins, fullLosses)

    Shared DT!(), plyrRecords!(), leagS!(), OT!()
    Shared statsW0!(), statsW1!(), offRecords!(), defRecords!()

    Shared plyrRecDesc$(), N$(), offRecDesc$(), defRecDesc$()

    totalGames = fullWins + fullLosses

    For I = 0 To 14

        leagS!(I, 25) = leagS!(I, 13)
        leagS!(I, 26) = leagS!(I, 12)
        leagS!(I, 12) = leagS!(I, 2) * 2 + statsW0!(I) + leagS!(I, 4): ' *** PTS ***

        If leagS!(I, 0) <> 0 Then

            leagS!(I, 13) = leagS!(I, 5) / leagS!(I, 0): ' *** RB/G ***
            leagS!(I, 14) = leagS!(I, 6) / leagS!(I, 0): ' *** FOULS/G ***
            leagS!(I, 15) = leagS!(I, 7) / leagS!(I, 0): ' *** STEALS/G ***
            leagS!(I, 16) = leagS!(I, 8) / leagS!(I, 0): ' *** ASSISTS/G ***
            leagS!(I, 17) = leagS!(I, 9) / leagS!(I, 0): ' *** BLOCKS/G ***
            leagS!(I, 24) = leagS!(I, 5) + leagS!(I, 12) - leagS!(I, 6) + leagS!(I, 8) + leagS!(I, 9) + leagS!(I, 7) - 2 * leagS!(I, 11)
            leagS!(I, 18) = leagS!(I, 10) / 60 / leagS!(I, 0): ' *** MIN/G ***

            If Not (leagS!(I, 1) = 0 Or (leagS!(I, 2) / leagS!(I, 0)) < 3.65) Then
                leagS!(I, 19) = leagS!(I, 2) / leagS!(I, 1): ' *** FG% ***
            End If

            If Not (leagS!(I, 3) = 0 Or (leagS!(I, 4) / leagS!(I, 0)) < 1.52) Then
                leagS!(I, 20) = leagS!(I, 4) / leagS!(I, 3): ' *** FT% ***
            End If

            If Not (statsW1!(I) = 0 Or statsW0!(I) / leagS!(I, 0) < .3) Then
                leagS!(I, 21) = statsW0!(I) / statsW1!(I): ' *** 3FG% ***
            End If

            leagS!(I, 22) = leagS!(I, 12) / leagS!(I, 0): ' *** PTS/G ***
            If leagS!(I, 0) > plyrRecords!(1) Then plyrRecords!(1) = leagS!(I, 0): plyrRecDesc$(1, 0) = N$(I): plyrRecDesc$(1, 1) = TN$
            If leagS!(I, 1) > plyrRecords!(2) Then plyrRecords!(2) = leagS!(I, 1): plyrRecDesc$(2, 0) = N$(I): plyrRecDesc$(2, 1) = TN$
            If leagS!(I, 2) > plyrRecords!(3) Then plyrRecords!(3) = leagS!(I, 2): plyrRecDesc$(3, 0) = N$(I): plyrRecDesc$(3, 1) = TN$
            If leagS!(I, 19) > plyrRecords!(4) And UCase$(finalUpdate$) = "Y" And leagS!(I, 2) / leagS!(I, 0) >= 3.65 Then plyrRecords!(4) = leagS!(I, 19): plyrRecDesc$(4, 0) = N$(I): plyrRecDesc$(4, 1) = TN$

            If statsW1!(I) > plyrRecords!(5) Then plyrRecords!(5) = statsW1!(I): plyrRecDesc$(5, 0) = N$(I): plyrRecDesc$(5, 1) = TN$
            If statsW0!(I) > plyrRecords!(6) Then plyrRecords!(6) = statsW0!(I): plyrRecDesc$(6, 0) = N$(I): plyrRecDesc$(6, 1) = TN$
            If leagS!(I, 21) > plyrRecords!(7) And UCase$(finalUpdate$) = "Y" And statsW0!(I) / leagS!(I, 0) >= .3 Then plyrRecords!(7) = leagS!(I, 21): plyrRecDesc$(7, 0) = N$(I): plyrRecDesc$(7, 1) = TN$

            If leagS!(I, 3) > plyrRecords!(8) Then plyrRecords!(8) = leagS!(I, 3): plyrRecDesc$(8, 0) = N$(I): plyrRecDesc$(8, 1) = TN$
            If leagS!(I, 4) > plyrRecords!(9) Then plyrRecords!(9) = leagS!(I, 4): plyrRecDesc$(9, 0) = N$(I): plyrRecDesc$(9, 1) = TN$
            If leagS!(I, 20) > plyrRecords!(10) And UCase$(finalUpdate$) = "Y" And leagS!(I, 4) / leagS!(I, 0) >= 1.52 Then plyrRecords!(10) = leagS!(I, 20): plyrRecDesc$(10, 0) = N$(I): plyrRecDesc$(10, 1) = TN$

            If leagS!(I, 10) / 60 > plyrRecords!(11) Then plyrRecords!(11) = leagS!(I, 10) / 60: plyrRecDesc$(11, 0) = N$(I): plyrRecDesc$(11, 1) = TN$
            If leagS!(I, 12) > plyrRecords!(12) Then plyrRecords!(12) = leagS!(I, 12): plyrRecDesc$(12, 0) = N$(I): plyrRecDesc$(12, 1) = TN$
            If leagS!(I, 22) > plyrRecords!(13) And UCase$(finalUpdate$) = "Y" Then plyrRecords!(13) = leagS!(I, 22): plyrRecDesc$(13, 0) = N$(I): plyrRecDesc$(13, 1) = TN$

            If leagS!(I, 25) > plyrRecords!(14) Then plyrRecords!(14) = leagS!(I, 25): plyrRecDesc$(14, 0) = N$(I): plyrRecDesc$(14, 1) = TN$
            If leagS!(I, 26) > plyrRecords!(15) Then plyrRecords!(15) = leagS!(I, 26): plyrRecDesc$(15, 0) = N$(I): plyrRecDesc$(15, 1) = TN$
            If leagS!(I, 5) - leagS!(I, 26) > plyrRecords!(16) Then plyrRecords!(16) = leagS!(I, 5) - leagS!(I, 26): plyrRecDesc$(16, 0) = N$(I): plyrRecDesc$(16, 1) = TN$
            If leagS!(I, 5) > plyrRecords!(17) Then plyrRecords!(17) = leagS!(I, 5): plyrRecDesc$(17, 0) = N$(I): plyrRecDesc$(17, 1) = TN$
            If leagS!(I, 13) > plyrRecords!(18) And UCase$(finalUpdate$) = "Y" Then plyrRecords!(18) = leagS!(I, 13): plyrRecDesc$(18, 0) = N$(I): plyrRecDesc$(18, 1) = TN$

            If leagS!(I, 6) > plyrRecords!(19) Then plyrRecords!(19) = leagS!(I, 6): plyrRecDesc$(19, 0) = N$(I): plyrRecDesc$(19, 1) = TN$
            If leagS!(I, 7) > plyrRecords!(20) Then plyrRecords!(20) = leagS!(I, 7): plyrRecDesc$(20, 0) = N$(I): plyrRecDesc$(20, 1) = TN$
            If leagS!(I, 8) > plyrRecords!(21) Then plyrRecords!(21) = leagS!(I, 8): plyrRecDesc$(21, 0) = N$(I): plyrRecDesc$(21, 1) = TN$
            If leagS!(I, 9) > plyrRecords!(22) Then plyrRecords!(22) = leagS!(I, 9): plyrRecDesc$(22, 0) = N$(I): plyrRecDesc$(22, 1) = TN$
            If leagS!(I, 11) > plyrRecords!(23) Then plyrRecords!(23) = leagS!(I, 11): plyrRecDesc$(23, 0) = N$(I): plyrRecDesc$(23, 1) = TN$
            If leagS!(I, 18) > plyrRecords!(24) And UCase$(finalUpdate$) = "Y" Then plyrRecords!(24) = leagS!(I, 18): plyrRecDesc$(24, 0) = N$(I): plyrRecDesc$(24, 1) = TN$

            If leagS!(I, 24) > plyrRecords!(25) Then plyrRecords!(25) = leagS!(I, 24): plyrRecDesc$(25, 0) = N$(I): plyrRecDesc$(25, 1) = TN$

        End If 'Done checking leagS!(I, 0)

    Next

    OT!(16) = totalGames
    DT!(16) = totalGames

    OT!(17) = OT!(3) / OT!(2)
    DT!(17) = DT!(3) / DT!(2)
    OT!(18) = OT!(5) / OT!(4)
    DT!(18) = DT!(5) / DT!(4)
    OT!(20) = OT!(11)
    DT!(20) = DT!(11)

    If OT!(15) <> 0 Then OT!(21) = OT!(14) / OT!(15) Else OT!(21) = 0
    If DT!(15) <> 0 Then DT!(21) = DT!(14) / DT!(15) Else DT!(21) = 999

    OT!(19) = OT!(20) / totalGames
    DT!(19) = DT!(20) / totalGames

    If OT!(2) > offRecords!(1) Then offRecords!(1) = OT!(2): offRecDesc$(1) = TN$
    If OT!(3) > offRecords!(2) Then offRecords!(2) = OT!(3): offRecDesc$(2) = TN$
    If OT!(17) > offRecords!(3) And UCase$(finalUpdate$) = "Y" Then offRecords!(3) = OT!(17): offRecDesc$(3) = TN$

    If OT!(15) > offRecords!(4) Then offRecords!(4) = OT!(15): offRecDesc$(4) = TN$
    If OT!(14) > offRecords!(5) Then offRecords!(5) = OT!(14): offRecDesc$(5) = TN$
    If OT!(21) > offRecords!(6) And UCase$(finalUpdate$) = "Y" Then offRecords!(6) = OT!(21): offRecDesc$(6) = TN$

    If OT!(4) > offRecords!(7) Then offRecords!(7) = OT!(4): offRecDesc$(7) = TN$
    If OT!(5) > offRecords!(8) Then offRecords!(8) = OT!(5): offRecDesc$(8) = TN$
    If OT!(18) > offRecords!(9) And UCase$(finalUpdate$) = "Y" Then offRecords!(9) = OT!(18): offRecDesc$(9) = TN$

    If OT!(13) > offRecords!(10) Then offRecords!(10) = OT!(13): offRecDesc$(10) = TN$
    If OT!(6) - OT!(13) > offRecords!(11) Then offRecords!(11) = OT!(6) - OT!(13): offRecDesc$(11) = TN$
    If OT!(6) > offRecords!(12) Then offRecords!(12) = OT!(6): offRecDesc$(12) = TN$
    If OT!(6) / totalGames > offRecords!(13) And UCase$(finalUpdate$) = "Y" Then offRecords!(13) = OT!(6) / totalGames: offRecDesc$(13) = TN$

    If OT!(7) < offRecords!(14) And UCase$(finalUpdate$) = "Y" Then offRecords!(14) = OT!(7): offRecDesc$(14) = TN$

    If OT!(8) > offRecords!(15) Then offRecords!(15) = OT!(8): offRecDesc$(15) = TN$
    If OT!(9) > offRecords!(16) Then offRecords!(16) = OT!(9): offRecDesc$(16) = TN$
    If OT!(10) > offRecords!(17) Then offRecords!(17) = OT!(10): offRecDesc$(17) = TN$
    If OT!(12) < offRecords!(18) And UCase$(finalUpdate$) = "Y" Then offRecords!(18) = OT!(12): offRecDesc$(18) = TN$

    If OT!(11) > offRecords!(19) Then offRecords!(19) = OT!(11): offRecDesc$(19) = TN$
    If OT!(11) / totalGames > offRecords!(20) And UCase$(finalUpdate$) = "Y" Then offRecords!(20) = OT!(11) / totalGames: offRecDesc$(20) = TN$
    If fullWins > offRecords!(21) Then offRecords!(21) = fullWins: offRecDesc$(21) = TN$

    'FG's
    If DT!(2) < defRecords!(1) Then defRecords!(1) = DT!(2): defRecDesc$(1) = TN$
    If DT!(3) < defRecords!(2) Then defRecords!(2) = DT!(3): defRecDesc$(2) = TN$
    If DT!(17) < defRecords!(3) And UCase$(finalUpdate$) = "Y" Then defRecords!(3) = DT!(17): defRecDesc$(3) = TN$

    '3 FGs
    If DT!(15) < defRecords!(4) Then defRecords!(4) = DT!(15): defRecDesc$(4) = TN$
    If DT!(14) < defRecords!(5) Then defRecords!(5) = DT!(14): defRecDesc$(5) = TN$
    If DT!(21) < defRecords!(6) And DT!(15) <> 0 And UCase$(finalUpdate$) = "Y" Then defRecords!(6) = DT!(21): defRecDesc$(6) = TN$

    If DT!(4) < defRecords!(7) Then defRecords!(7) = DT!(4): defRecDesc$(7) = TN$
    If DT!(5) < defRecords!(8) Then defRecords!(8) = DT!(5): defRecDesc$(8) = TN$
    If DT!(18) < defRecords!(9) And UCase$(finalUpdate$) = "Y" Then defRecords!(9) = DT!(18): defRecDesc$(9) = TN$

    If DT!(13) < defRecords!(10) Then defRecords!(10) = DT!(13): defRecDesc$(10) = TN$
    If DT!(6) - DT!(13) < defRecords!(11) Then defRecords!(11) = DT!(6) - DT!(13): defRecDesc$(11) = TN$
    If DT!(6) < defRecords!(12) Then defRecords!(12) = DT!(6): defRecDesc$(12) = TN$
    If DT!(6) / totalGames < defRecords!(13) And UCase$(finalUpdate$) = "Y" Then defRecords!(13) = DT!(6) / totalGames: defRecDesc$(13) = TN$

    If DT!(7) > defRecords!(14) And UCase$(finalUpdate$) = "Y" Then defRecords!(14) = DT!(7): defRecDesc$(14) = TN$

    If DT!(8) < defRecords!(15) Then defRecords!(15) = DT!(8): defRecDesc$(15) = TN$
    If DT!(9) < defRecords!(16) Then defRecords!(16) = DT!(9): defRecDesc$(16) = TN$
    If DT!(10) < defRecords!(17) Then defRecords!(17) = DT!(10): defRecDesc$(17) = TN$
    If DT!(12) > defRecords!(18) And UCase$(finalUpdate$) = "Y" Then defRecords!(18) = DT!(12): defRecDesc$(18) = TN$

    If DT!(11) < defRecords!(19) Then defRecords!(19) = DT!(11): defRecDesc$(19) = TN$
    If DT!(11) / totalGames < defRecords!(20) And UCase$(finalUpdate$) = "Y" Then defRecords!(20) = DT!(11) / totalGames: defRecDesc$(20) = TN$

    If fullLosses > defRecords!(21) Then defRecords!(21) = fullLosses: defRecDesc$(21) = TN$

End Sub


'----------------------------------------
'      SortLeadersPro Subroutine
'----------------------------------------
'This subroutine sorts the leaders in the
'target league
Sub SortLeadersPro ()

    Shared O1!(), O2!()

    Shared statsZ1$()

    For Z = 0 To 3

        AA = (Z * 10) + 1
        ZZ = (Z * 10) + 10

        For X = AA To ZZ - 1

            For Y = X + 1 To ZZ

                diffX = O1!(X) - O2!(X)
                diffY = O1!(Y) - O2!(Y)

                If diffX <= diffY Then

                    If diffX <> diffY Then

                        If Not (O1!(X) <= O1!(Y) And O1!(X) > O2!(X)) Or Not (O1!(X) >= O1!(Y) And O1!(X) < O2!(X)) Then
                            Swap statsZ1$(X), statsZ1$(Y)
                            Swap O1!(X), O1!(Y)
                            Swap O2!(X), O2!(Y)
                        End If

                    End If

                End If

            Next Y

        Next X

    Next Z

End Sub


'----------------------------------------
'       PostLeadersPro Subroutine
'----------------------------------------
'This subroutine opens the existing leader
'and record files for the league, and
'writes the updated stats to the file
Sub PostLeadersPro (leaderFile$)

    Shared plyrRecords!(), DL!(), LD!()
    Shared O1!(), O2!(), OL!()
    Shared offRecords!(), defRecords!()

    Shared plyrRecDesc$(), DL$(), OL$()
    Shared statsZ1$(), offRecDesc$(), defRecDesc$()
    Shared Z2$(), Z3$()

    Cls

    Print

    Open diskPaths$(1) + leaderFile$ + ".LDR" For Output As #1

    Print "    POSTING NEW "; leaderFile$; " LEADERS"

    For X = 1 To TEAMS_PER_CONFERENCE
        Write #1, statsZ1$(X), O1!(X), O2!(X)
    Next

    For X = 1 To 250
        Write #1, Z2$(X), Z3$(X), LD!(X, 1), LD!(X, 2)
    Next X

    For X = 1 To TEAMS_PER_CONFERENCE:
        Write #1, OL$(X)
        For Y = 1 To 20:
            Write #1, OL!(X, Y)
        Next
    Next

    For X = 1 To TEAMS_PER_CONFERENCE:
        Write #1, DL$(X)
        For Y = 1 To 20:
            Write #1, DL!(X, Y)
        Next
    Next

    Close #1

    Open diskPaths$(1) + "RECORDS.PBB" For Output As #2

    For I = 1 To 25
        Print #2, plyrRecords!(I)
        Print #2, plyrRecDesc$(I, 0)
        Print #2, plyrRecDesc$(I, 1)
    Next

    For I = 1 To 21
        Print #2, offRecords!(I)
        Print #2, offRecDesc$(I)
    Next

    For I = 1 To 21
        Print #2, defRecords!(I)
        Print #2, defRecDesc$(I)
    Next

    Close #2

End Sub

