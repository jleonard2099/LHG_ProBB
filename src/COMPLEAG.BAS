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
    Shared defTotals!(), defLeaders!()
    Shared leaderVals!(), leagS!()
    Shared O1!(), O2!(), offTotals!(), offLeaders!()
    Shared offRecords!(), defRecords!(), statsW0!(), statsW1!()

    Shared plyrRecDesc$()
    Shared defLeaderNames$(), offLeaderNames$()
    Shared statPlyrNames$()
    Shared locIndicator$(), oppName$()
    Shared memberTeams$(), statFileNames$()
    Shared Z2$(), Z3$()

    Shared offRecDesc$(), defRecDesc$()

    Shared confWins, confLosses, fullWins, fullLosses

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

    For X = 1 To TEAMS_PER_LEAGUE:
        defLeaders!(X, 19) = 9999
    Next

    Erase leaderVals!, offLeaders!
    Erase Z2$, Z3$

    C$ = ","

    leagueFile$ = ChooseFile$(diskPaths$(2) + "*.TMI", "LEAGUE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Do
            Color 15, 0
            Cls
            Print "WOULD YOU LIKE TO POST SEASON RECORDS? (Y/N)";
            I$ = GetKeyPress$
            finalUpdate$ = UCase$(I$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

        Open diskPaths$(2) + leagueFile$ + ".TMS" For Input As #2

        Input #2, league$, div1$, div2$, div3$, div4$

        For XW = 1 To TEAMS_PER_LEAGUE
            Input #2, statFileNames$(XW)
            memberTeams$(XW) = statFileNames$(XW)
        Next

        Close #2

        For XW = 1 To TEAMS_PER_LEAGUE

            'Look for * as terminating char
            If statFileNames$(XW) <> Chr$(42) Then

                haveStats = FindStatsLeague%(statFileNames$(XW), numberGames)

                If (haveStats) Then

                    L = Len(statFileNames$(XW))
                    WT$ = ""

                    For X = 1 To L
                        A = Asc(Mid$(statFileNames$(XW), X, 1))
                        WT$ = WT$ + Chr$(A)
                    Next X

                    nonConfGame = 0

                    For X = 1 To TEAMS_PER_LEAGUE

                        If memberTeams$(X) = WT$ Then

                            If O1!(X) = fullWins And O2!(X) = fullLosses Then

                                nonConfGame = 1
                                Exit For

                            Else

                                O1!(X) = fullWins
                                O2!(X) = fullLosses

                            End If

                        End If

                    Next X

                    If nonConfGame <> 1 And _FileExists(diskPaths$(1) + statFileNames$(XW)) Then

                        Call ReadStatsPro(statFileNames$(XW), TN$)
                        Call CalcStatTotalsPro(numberGames, TN$, finalUpdate$)
                        Call CalcStatLeadersPro(TN$, numberGames)

                    End If

                End If ' Done checking if we have stats

            End If 'Done checking valid conference

        Next XW 'Done "checking stats"

        Call SortStandingsLeague
        Call PostLeadersPro(leagueFile$)

    End If

End Sub


'----------------------------------------
'       ReadStatsPro Subroutine
'----------------------------------------
'This subroutine reads stats in for a
'single team in the league
Sub ReadStatsPro (statFile$, teamName$)

    Shared defTotals!(), leagS!(), offTotals!()
    Shared statsW0!(), statsW1!()

    Shared statPlyrNames$()

    Erase defTotals!, offTotals!
    Erase leagS!, statsW0!, statsW1!
    Erase statPlyrNames$

    Open diskPaths$(1) + statFile$ For Input As #1

    Input #1, teamName$

    For I = 0 To 14

        Input #1, statPlyrNames$(I)

        For J = 0 To 13:
            Input #1, leagS!(I, J)
        Next

        Input #1, statsW0!(I)
        Input #1, statsW1!(I)

    Next I

    For L = 1 To 16:
        Input #1, offTotals!(L)
    Next

    For L = 1 To 16:
        Input #1, defTotals!(L)
    Next

    Close #1

End Sub


'----------------------------------------
'     CalcStatLeadersPro Subroutine
'----------------------------------------
'This subroutine compares pre-loaded stats
'to existing leaders and updates as needed
Sub CalcStatLeadersPro (teamName$, numberGames)

    Shared defTotals!(), offTotals!()
    Shared defLeaders!(), offLeaders!()
    Shared leaderVals!(), leagS!()
    Shared statsW0!(), statsW1!()

    Shared defLeaderNames$(), offLeaderNames$()
    Shared statPlyrNames$()
    Shared Z2$(), Z3$()

    For X = 0 To 14:

        'Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print statPlyrNames$(X)

        'Points Per Game
        '--- Must have played 70% of team's games
        If leagS!(X, 22) > leaderVals!(20, 1) And (leagS!(X, 0) >= (numberGames * 0.7)) Then

            leaderVals!(20, 1) = leagS!(X, 22)
            leaderVals!(20, 2) = 0
            Z2$(20) = statPlyrNames$(X)
            Z3$(20) = teamName$

            For Z = 1 To 19
                For Z1 = Z + 1 To 20

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2: 
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Offensive Rebounds
        '--- Must have played 70% of team's games
        If leagS!(X, 12) > leaderVals!(40, 1) And (leagS!(X, 0) >= (numberGames * 0.7)) Then

            leaderVals!(40, 1) = leagS!(X, 12)
            leaderVals!(40, 2) = 0
            Z2$(40) = statPlyrNames$(X)
            Z3$(40) = teamName$

            For Z = 21 To 39
                For Z1 = Z + 1 To TEAMS_PER_LEAGUE

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Field Goal %
        '-- Must have 300 FGM
        If leagS!(X, 19) > leaderVals!(60, 1) And leagS!(X, 2) >= 300 Then

            leaderVals!(60, 1) = leagS!(X, 19) 'FG%
            leaderVals!(60, 2) = leagS!(X, 1) 'FGA
            Z2$(60) = statPlyrNames$(X)
            Z3$(60) = teamName$

            For Z = 41 To 59
                For Z1 = Z + 1 To 60

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Free Throw %
        '-- Must mave 125 FTM
        If leagS!(X, 20) > leaderVals!(80, 1) And (leagS!(X, 4) >= 125) Then

            leaderVals!(80, 1) = leagS!(X, 20) 'FT%
            leaderVals!(80, 2) = leagS!(X, 3) 'FTA
            Z2$(80) = statPlyrNames$(X)
            Z3$(80) = teamName$

            For Z = 61 To 79
                For Z1 = Z + 1 To 80

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Steals / Game
        '--- Must have played 70% of team's games
        If leagS!(X, 15) > leaderVals!(90, 1) And (leagS!(X, 0) >= (numberGames * 0.7)) Then

            leaderVals!(90, 1) = leagS!(X, 15)
            leaderVals!(90, 2) = leagS!(X, 7)
            Z2$(90) = statPlyrNames$(X)
            Z3$(90) = teamName$

            For Z = 81 To 89
                For Z1 = Z + 1 To 90

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Steals
        '--- Must have played 70% of team's games
        If leagS!(X, 7) > leaderVals!(100, 1) And (leagS!(X, 0) >= (numberGames * 0.7)) Then

            leaderVals!(100, 1) = leagS!(X, 7)
            leaderVals!(100, 2) = 0
            Z2$(100) = statPlyrNames$(X)
            Z3$(100) = teamName$

            For Z = 91 To 99
                For Z1 = Z + 1 To 100
                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Assists Per Game
        '--- Must have played 70% of team's games
        If leagS!(X, 16) > leaderVals!(110, 1) And (leagS!(X, 0) >= (numberGames * 0.7)) Then

            leaderVals!(110, 1) = leagS!(X, 16)
            leaderVals!(110, 2) = leagS!(X, 8)
            Z2$(110) = statPlyrNames$(X)
            Z3$(110) = teamName$

            For Z = 101 To 109
                For Z1 = Z + 1 To 110

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Assists
        '--- Must have played 70% of team's games
        If leagS!(X, 8) > leaderVals!(120, 1) Then

            leaderVals!(120, 1) = leagS!(X, 8)
            leaderVals!(120, 2) = leagS!(X, 8)
            Z2$(120) = statPlyrNames$(X)
            Z3$(120) = teamName$

            For Z = 111 To 119
                For Z1 = Z + 1 To 120

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        '3 Field Goal %
        '-- Must have 82 3FG's made; assumes 82 game schedule
        If leagS!(X, 21) > leaderVals!(130, 1) And statsW0!(X) >= numberGames Then

            leaderVals!(130, 1) = leagS!(X, 21)
            leaderVals!(130, 2) = statsW1!(X)
            Z2$(130) = statPlyrNames$(X)
            Z3$(130) = teamName$

            For Z = 121 To 129
                For Z1 = Z + 1 To 130
                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        '3 Field Goals Made
        '-- Must have 82 3FG's made; assumes 82 game schedule
        If statsW0!(X) > leaderVals!(140, 1) And statsW0!(X) >= numberGames Then

            leaderVals!(140, 1) = statsW0!(X)
            leaderVals!(140, 2) = 0
            Z2$(140) = statPlyrNames$(X)
            Z3$(140) = teamName$

            For Z = 131 To 139
                For Z1 = Z + 1 To 140

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Minutes Per Game
        '--- Must have played 70% of team's games
        If leagS!(X, 18) > leaderVals!(150, 1) And (leagS!(X, 0) >= (numberGames * 0.7)) Then

            leaderVals!(150, 1) = leagS!(X, 18)
            leaderVals!(150, 2) = 0
            Z2$(150) = statPlyrNames$(X)
            Z3$(150) = teamName$

            For Z = 141 To 149
                For Z1 = Z + 1 To 150

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Total Minutes
        '--- Must have played 70% of team's games
        If leagS!(X, 10) / 60 > leaderVals!(160, 1) And (leagS!(X, 0) >= (numberGames * 0.7)) Then

            leaderVals!(160, 1) = leagS!(X, 10) / 60
            leaderVals!(160, 2) = 0
            Z2$(160) = statPlyrNames$(X)
            Z3$(160) = teamName$

            For Z = 151 To 159
                For Z1 = Z + 1 To 160

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Rebounds Per Game
        '--- Must have played 70% of team's games
        If leagS!(X, 13) > leaderVals!(170, 1) And (leagS!(X, 0) >= (numberGames * 0.7)) Then

            leaderVals!(170, 1) = leagS!(X, 13)
            leaderVals!(170, 2) = leagS!(X, 5)
            Z2$(170) = statPlyrNames$(X)
            Z3$(170) = teamName$

            For Z = 161 To 169
                For Z1 = Z + 1 To 170

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Avg Rebounds
        '--- Must have played 70% of team's games
        If leagS!(X, 5) > leaderVals!(180, 1) And (leagS!(X, 0) >= (numberGames * 0.7)) Then

            leaderVals!(180, 1) = leagS!(X, 5)
            leaderVals!(18, 2) = 0
            Z2$(180) = statPlyrNames$(X)
            Z3$(180) = teamName$

            For Z = 171 To 179
                For Z1 = Z + 1 To 180

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Blocks Per Game
        '--- Must have played 70% of team's games
        If leagS!(X, 17) > leaderVals!(190, 1) And (leagS!(X, 0) >= (numberGames * 0.7)) Then

            leaderVals!(190, 1) = leagS!(X, 17)
            leaderVals!(190, 2) = 0
            Z2$(190) = statPlyrNames$(X)
            Z3$(190) = teamName$

            For Z = 181 To 189
                For Z1 = Z + 1 To 190

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Blocks
        '--- Must have played 70% of team's games
        If leagS!(X, 9) > leaderVals!(200, 1) And (leagS!(X, 0) >= (numberGames * 0.7)) Then

            leaderVals!(200, 1) = leagS!(X, 9)
            leaderVals!(200, 2) = 0
            Z2$(200) = statPlyrNames$(X)
            Z3$(200) = teamName$

            For Z = 191 To 199
                For Z1 = Z + 1 To 200

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z
        End If

        'Field Goals Made
        If leagS!(X, 2) > leaderVals!(210, 1) Then

            leaderVals!(210, 1) = leagS!(X, 2)
            leaderVals!(210, 2) = 0
            Z2$(210) = statPlyrNames$(X)
            Z3$(210) = teamName$

            For Z = 201 To 209
                For Z1 = Z + 1 To 210

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Free Throws Made
        If leagS!(X, 4) > leaderVals!(220, 1) Then

            leaderVals!(220, 1) = leagS!(X, 4)
            leaderVals!(220, 2) = 0
            Z2$(220) = statPlyrNames$(X)
            Z3$(220) = teamName$

            For Z = 211 To 219
                For Z1 = Z + 1 To 220

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Fouls Per Game
        If leagS!(X, 14) > leaderVals!(230, 1) Then

            leaderVals!(230, 1) = leagS!(X, 14)
            leaderVals!(230, 2) = 0
            Z2$(230) = statPlyrNames$(X)
            Z3$(230) = teamName$

            For Z = 221 To 229
                For Z1 = Z + 1 To 230

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'Fouls
        If leagS!(X, 6) > leaderVals!(240, 1) Then

            leaderVals!(240, 1) = leagS!(X, 6)
            leaderVals!(240, 2) = 0
            Z2$(240) = statPlyrNames$(X)
            Z3$(240) = teamName$

            For Z = 231 To 239
                For Z1 = Z + 1 To 240

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z

        End If

        'MVP Points
        If leagS!(X, 24) > leaderVals!(250, 1) Then

            leaderVals!(250, 1) = leagS!(X, 24)
            leaderVals!(250, 2) = 0
            Z2$(250) = statPlyrNames$(X)
            Z3$(250) = teamName$

            For Z = 241 To 249
                For Z1 = Z + 1 To 250

                    If leaderVals!(Z, 1) < leaderVals!(Z1, 1) Then
                        Swap Z2$(Z1), Z2$(Z)
                        Swap Z3$(Z1), Z3$(Z)
                        For M = 1 To 2
                            Swap leaderVals!(Z1, M), leaderVals!(Z, M)
                        Next
                    End If

                Next Z1
            Next Z
        End If

    Next X

    'Offensive Totals
    If offTotals!(19) > offLeaders!(40, 19) Then

        For M = 1 To 20
            offLeaders!(40, M) = offTotals!(M)
        Next

        offLeaderNames$(40) = teamName$

        For X = 1 To 39
            For X1 = 1 + X To TEAMS_PER_LEAGUE

                If offLeaders!(X, 19) < offLeaders!(X1, 19) Then
                    Swap offLeaderNames$(X1), offLeaderNames$(X)
                    For M = 1 To 20
                        Swap offLeaders!(X1, M), offLeaders!(X, M)
                    Next
                End If

            Next X1
        Next X

    End If

    'Defensive Totals
    If defTotals!(19) < defLeaders!(40, 19) Then

        For M = 1 To 20
            defLeaders!(40, M) = defTotals!(M)
        Next

        defLeaderNames$(40) = teamName$

        For X = 1 To 39
            For X1 = 1 + X To TEAMS_PER_LEAGUE

                If defLeaders!(X, 19) > defLeaders!(X1, 19) Then
                    Swap defLeaderNames$(X1), defLeaderNames$(X)
                    For M = 1 To 20
                        Swap defLeaders!(X1, M), defLeaders!(X, M)
                    Next
                End If

            Next X1
        Next X

    End If

End Sub


'----------------------------------------
'     CalcStatTotalsPro Subroutine
'----------------------------------------
'This subroutine compares all pre-loaded
'stats to each of the record categories to
'see if any should be updated
Sub CalcStatTotalsPro (numberGames, teamName$, finalUpdate$)

    Shared defTotals!(), offTotals!()
    Shared plyrRecords!(), leagS!()
    Shared offRecords!(), defRecords!()
    Shared statsW0!(), statsW1!()

    Shared plyrRecDesc$(), statPlyrNames$()
    Shared offRecDesc$(), defRecDesc$()

    Shared fullWins, fullLosses

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

            If leagS!(I, 0) > plyrRecords!(1) Then plyrRecords!(1) = leagS!(I, 0): plyrRecDesc$(1, 0) = statPlyrNames$(I): plyrRecDesc$(1, 1) = teamName$
            If leagS!(I, 1) > plyrRecords!(2) Then plyrRecords!(2) = leagS!(I, 1): plyrRecDesc$(2, 0) = statPlyrNames$(I): plyrRecDesc$(2, 1) = teamName$
            If leagS!(I, 2) > plyrRecords!(3) Then plyrRecords!(3) = leagS!(I, 2): plyrRecDesc$(3, 0) = statPlyrNames$(I): plyrRecDesc$(3, 1) = teamName$
            If leagS!(I, 19) > plyrRecords!(4) And UCase$(finalUpdate$) = "Y" And leagS!(I, 2) / leagS!(I, 0) >= 3.65 Then plyrRecords!(4) = leagS!(I, 19): plyrRecDesc$(4, 0) = statPlyrNames$(I): plyrRecDesc$(4, 1) = teamName$

            If statsW1!(I) > plyrRecords!(5) Then plyrRecords!(5) = statsW1!(I): plyrRecDesc$(5, 0) = statPlyrNames$(I): plyrRecDesc$(5, 1) = teamName$
            If statsW0!(I) > plyrRecords!(6) Then plyrRecords!(6) = statsW0!(I): plyrRecDesc$(6, 0) = statPlyrNames$(I): plyrRecDesc$(6, 1) = teamName$
            If leagS!(I, 21) > plyrRecords!(7) And UCase$(finalUpdate$) = "Y" And statsW0!(I) / leagS!(I, 0) >= .3 Then plyrRecords!(7) = leagS!(I, 21): plyrRecDesc$(7, 0) = statPlyrNames$(I): plyrRecDesc$(7, 1) = teamName$

            If leagS!(I, 3) > plyrRecords!(8) Then plyrRecords!(8) = leagS!(I, 3): plyrRecDesc$(8, 0) = statPlyrNames$(I): plyrRecDesc$(8, 1) = teamName$
            If leagS!(I, 4) > plyrRecords!(9) Then plyrRecords!(9) = leagS!(I, 4): plyrRecDesc$(9, 0) = statPlyrNames$(I): plyrRecDesc$(9, 1) = teamName$
            If leagS!(I, 20) > plyrRecords!(10) And UCase$(finalUpdate$) = "Y" And leagS!(I, 4) / leagS!(I, 0) >= 1.52 Then plyrRecords!(10) = leagS!(I, 20): plyrRecDesc$(10, 0) = statPlyrNames$(I): plyrRecDesc$(10, 1) = teamName$

            If leagS!(I, 10) / 60 > plyrRecords!(11) Then plyrRecords!(11) = leagS!(I, 10) / 60: plyrRecDesc$(11, 0) = statPlyrNames$(I): plyrRecDesc$(11, 1) = teamName$
            If leagS!(I, 12) > plyrRecords!(12) Then plyrRecords!(12) = leagS!(I, 12): plyrRecDesc$(12, 0) = statPlyrNames$(I): plyrRecDesc$(12, 1) = teamName$
            If leagS!(I, 22) > plyrRecords!(13) And UCase$(finalUpdate$) = "Y" Then plyrRecords!(13) = leagS!(I, 22): plyrRecDesc$(13, 0) = statPlyrNames$(I): plyrRecDesc$(13, 1) = teamName$

            If leagS!(I, 25) > plyrRecords!(14) Then plyrRecords!(14) = leagS!(I, 25): plyrRecDesc$(14, 0) = statPlyrNames$(I): plyrRecDesc$(14, 1) = teamName$
            If leagS!(I, 26) > plyrRecords!(15) Then plyrRecords!(15) = leagS!(I, 26): plyrRecDesc$(15, 0) = statPlyrNames$(I): plyrRecDesc$(15, 1) = teamName$
            If leagS!(I, 5) - leagS!(I, 26) > plyrRecords!(16) Then plyrRecords!(16) = leagS!(I, 5) - leagS!(I, 26): plyrRecDesc$(16, 0) = statPlyrNames$(I): plyrRecDesc$(16, 1) = teamName$
            If leagS!(I, 5) > plyrRecords!(17) Then plyrRecords!(17) = leagS!(I, 5): plyrRecDesc$(17, 0) = statPlyrNames$(I): plyrRecDesc$(17, 1) = teamName$
            If leagS!(I, 13) > plyrRecords!(18) And UCase$(finalUpdate$) = "Y" Then plyrRecords!(18) = leagS!(I, 13): plyrRecDesc$(18, 0) = statPlyrNames$(I): plyrRecDesc$(18, 1) = teamName$

            If leagS!(I, 6) > plyrRecords!(19) Then plyrRecords!(19) = leagS!(I, 6): plyrRecDesc$(19, 0) = statPlyrNames$(I): plyrRecDesc$(19, 1) = teamName$
            If leagS!(I, 7) > plyrRecords!(20) Then plyrRecords!(20) = leagS!(I, 7): plyrRecDesc$(20, 0) = statPlyrNames$(I): plyrRecDesc$(20, 1) = teamName$
            If leagS!(I, 8) > plyrRecords!(21) Then plyrRecords!(21) = leagS!(I, 8): plyrRecDesc$(21, 0) = statPlyrNames$(I): plyrRecDesc$(21, 1) = teamName$
            If leagS!(I, 9) > plyrRecords!(22) Then plyrRecords!(22) = leagS!(I, 9): plyrRecDesc$(22, 0) = statPlyrNames$(I): plyrRecDesc$(22, 1) = teamName$
            If leagS!(I, 11) > plyrRecords!(23) Then plyrRecords!(23) = leagS!(I, 11): plyrRecDesc$(23, 0) = statPlyrNames$(I): plyrRecDesc$(23, 1) = teamName$
            If leagS!(I, 18) > plyrRecords!(24) And UCase$(finalUpdate$) = "Y" Then plyrRecords!(24) = leagS!(I, 18): plyrRecDesc$(24, 0) = statPlyrNames$(I): plyrRecDesc$(24, 1) = teamName$

            If leagS!(I, 24) > plyrRecords!(25) Then plyrRecords!(25) = leagS!(I, 24): plyrRecDesc$(25, 0) = statPlyrNames$(I): plyrRecDesc$(25, 1) = teamName$

        End If 'Done checking leagS!(I, 0)

    Next

    offTotals!(16) = numberGames
    defTotals!(16) = numberGames

    offTotals!(17) = offTotals!(3) / offTotals!(2)
    defTotals!(17) = defTotals!(3) / defTotals!(2)
    offTotals!(18) = offTotals!(5) / offTotals!(4)
    defTotals!(18) = defTotals!(5) / defTotals!(4)
    offTotals!(20) = offTotals!(11)
    defTotals!(20) = defTotals!(11)

    If offTotals!(15) <> 0 Then offTotals!(21) = offTotals!(14) / offTotals!(15) Else offTotals!(21) = 0
    If defTotals!(15) <> 0 Then defTotals!(21) = defTotals!(14) / defTotals!(15) Else defTotals!(21) = 999

    offTotals!(19) = offTotals!(20) / numberGames
    defTotals!(19) = defTotals!(20) / numberGames

    If offTotals!(2) > offRecords!(1) Then offRecords!(1) = offTotals!(2): offRecDesc$(1) = teamName$
    If offTotals!(3) > offRecords!(2) Then offRecords!(2) = offTotals!(3): offRecDesc$(2) = teamName$
    If offTotals!(17) > offRecords!(3) And UCase$(finalUpdate$) = "Y" Then offRecords!(3) = offTotals!(17): offRecDesc$(3) = teamName$

    If offTotals!(15) > offRecords!(4) Then offRecords!(4) = offTotals!(15): offRecDesc$(4) = teamName$
    If offTotals!(14) > offRecords!(5) Then offRecords!(5) = offTotals!(14): offRecDesc$(5) = teamName$
    If offTotals!(21) > offRecords!(6) And UCase$(finalUpdate$) = "Y" Then offRecords!(6) = offTotals!(21): offRecDesc$(6) = teamName$

    If offTotals!(4) > offRecords!(7) Then offRecords!(7) = offTotals!(4): offRecDesc$(7) = teamName$
    If offTotals!(5) > offRecords!(8) Then offRecords!(8) = offTotals!(5): offRecDesc$(8) = teamName$
    If offTotals!(18) > offRecords!(9) And UCase$(finalUpdate$) = "Y" Then offRecords!(9) = offTotals!(18): offRecDesc$(9) = teamName$

    If offTotals!(13) > offRecords!(10) Then offRecords!(10) = offTotals!(13): offRecDesc$(10) = teamName$
    If offTotals!(6) - offTotals!(13) > offRecords!(11) Then offRecords!(11) = offTotals!(6) - offTotals!(13): offRecDesc$(11) = teamName$
    If offTotals!(6) > offRecords!(12) Then offRecords!(12) = offTotals!(6): offRecDesc$(12) = teamName$
    If offTotals!(6) / numberGames > offRecords!(13) And UCase$(finalUpdate$) = "Y" Then offRecords!(13) = offTotals!(6) / numberGames: offRecDesc$(13) = teamName$

    If offTotals!(7) < offRecords!(14) And UCase$(finalUpdate$) = "Y" Then offRecords!(14) = offTotals!(7): offRecDesc$(14) = teamName$

    If offTotals!(8) > offRecords!(15) Then offRecords!(15) = offTotals!(8): offRecDesc$(15) = teamName$
    If offTotals!(9) > offRecords!(16) Then offRecords!(16) = offTotals!(9): offRecDesc$(16) = teamName$
    If offTotals!(10) > offRecords!(17) Then offRecords!(17) = offTotals!(10): offRecDesc$(17) = teamName$
    If offTotals!(12) < offRecords!(18) And UCase$(finalUpdate$) = "Y" Then offRecords!(18) = offTotals!(12): offRecDesc$(18) = teamName$

    If offTotals!(11) > offRecords!(19) Then offRecords!(19) = offTotals!(11): offRecDesc$(19) = teamName$
    If offTotals!(11) / numberGames > offRecords!(20) And UCase$(finalUpdate$) = "Y" Then offRecords!(20) = offTotals!(11) / numberGames: offRecDesc$(20) = teamName$
    If fullWins > offRecords!(21) Then offRecords!(21) = fullWins: offRecDesc$(21) = teamName$

    'FG's
    If defTotals!(2) < defRecords!(1) Then defRecords!(1) = defTotals!(2): defRecDesc$(1) = teamName$
    If defTotals!(3) < defRecords!(2) Then defRecords!(2) = defTotals!(3): defRecDesc$(2) = teamName$
    If defTotals!(17) < defRecords!(3) And UCase$(finalUpdate$) = "Y" Then defRecords!(3) = defTotals!(17): defRecDesc$(3) = teamName$

    '3 FGs
    If defTotals!(15) < defRecords!(4) Then defRecords!(4) = defTotals!(15): defRecDesc$(4) = teamName$
    If defTotals!(14) < defRecords!(5) Then defRecords!(5) = defTotals!(14): defRecDesc$(5) = teamName$
    If defTotals!(21) < defRecords!(6) And defTotals!(15) <> 0 And UCase$(finalUpdate$) = "Y" Then defRecords!(6) = defTotals!(21): defRecDesc$(6) = teamName$

    If defTotals!(4) < defRecords!(7) Then defRecords!(7) = defTotals!(4): defRecDesc$(7) = teamName$
    If defTotals!(5) < defRecords!(8) Then defRecords!(8) = defTotals!(5): defRecDesc$(8) = teamName$
    If defTotals!(18) < defRecords!(9) And UCase$(finalUpdate$) = "Y" Then defRecords!(9) = defTotals!(18): defRecDesc$(9) = teamName$

    If defTotals!(13) < defRecords!(10) Then defRecords!(10) = defTotals!(13): defRecDesc$(10) = teamName$
    If defTotals!(6) - defTotals!(13) < defRecords!(11) Then defRecords!(11) = defTotals!(6) - defTotals!(13): defRecDesc$(11) = teamName$
    If defTotals!(6) < defRecords!(12) Then defRecords!(12) = defTotals!(6): defRecDesc$(12) = teamName$
    If defTotals!(6) / numberGames < defRecords!(13) And UCase$(finalUpdate$) = "Y" Then defRecords!(13) = defTotals!(6) / numberGames: defRecDesc$(13) = teamName$

    If defTotals!(7) > defRecords!(14) And UCase$(finalUpdate$) = "Y" Then defRecords!(14) = defTotals!(7): defRecDesc$(14) = teamName$

    If defTotals!(8) < defRecords!(15) Then defRecords!(15) = defTotals!(8): defRecDesc$(15) = teamName$
    If defTotals!(9) < defRecords!(16) Then defRecords!(16) = defTotals!(9): defRecDesc$(16) = teamName$
    If defTotals!(10) < defRecords!(17) Then defRecords!(17) = defTotals!(10): defRecDesc$(17) = teamName$
    If defTotals!(12) > defRecords!(18) And UCase$(finalUpdate$) = "Y" Then defRecords!(18) = defTotals!(12): defRecDesc$(18) = teamName$

    If defTotals!(11) < defRecords!(19) Then defRecords!(19) = defTotals!(11): defRecDesc$(19) = teamName$
    If defTotals!(11) / numberGames < defRecords!(20) And UCase$(finalUpdate$) = "Y" Then defRecords!(20) = defTotals!(11) / numberGames: defRecDesc$(20) = teamName$

    If fullLosses > defRecords!(21) Then defRecords!(21) = fullLosses: defRecDesc$(21) = teamName$

End Sub


'----------------------------------------
'     SortStandingsLeague Subroutine
'----------------------------------------
'This subroutine sorts the leaders in the
'target league. Currently setup using a 
'bubble sort algorithm.
Sub SortStandingsLeague ()

    Shared O1!(), O2!()

    Shared memberTeams$()

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
                            Swap memberTeams$(X), memberTeams$(Y)
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

    Shared defLeaderNames$(), offLeaderNames$()
    Shared defLeaders!(), offLeaders!()
    Shared leaderVals!()
    Shared O1!(), O2!()

    Shared memberTeams$()
    Shared Z2$(), Z3$()

    Cls

    Print

    Open diskPaths$(2) + leaderFile$ + ".LDR" For Output As #1

    Print "    POSTING NEW "; leaderFile$; " LEADERS"

    For X = 1 To TEAMS_PER_LEAGUE
        Write #1, memberTeams$(X), O1!(X), O2!(X)
    Next

    For X = 1 To 250
        Write #1, Z2$(X), Z3$(X), leaderVals!(X, 1), leaderVals!(X, 2)
    Next X

    For X = 1 To TEAMS_PER_LEAGUE:
        Write #1, offLeaderNames$(X)
        For Y = 1 To 20:
            Write #1, offLeaders!(X, Y)
        Next
    Next

    For X = 1 To TEAMS_PER_LEAGUE:
        Write #1, defLeaderNames$(X)
        For Y = 1 To 20:
            Write #1, defLeaders!(X, Y)
        Next
    Next

    Close #1
    
    Call PostRecordsPro

End Sub


'----------------------------------------
'         PostRecordsColl Subroutine
'----------------------------------------
'This subroutine writes the calculated
'records to the records file
Sub PostRecordsPro()

    Shared plyrRecords!()    
    Shared offRecords!(), defRecords!()

    Shared defRecDesc$(), offRecDesc$()
    Shared plyrRecDesc$()

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


'----------------------------------------
'       FindStatsLeague% Function
'----------------------------------------
'...explanation...
Function FindStatsLeague% (statFile$, numberGames)

    Shared oppScore(), teamScore()

    Shared locIndicator$(), oppName$(), statFileNames$()

    Shared confWins, confLosses, fullWins, fullLosses

    findFile$ = diskPaths$(1) + statFile$ + "S"

    If Not _FileExists(findFile$) Then

        Print "NO STATS FOUND FOR "; statFile$
        FindStatsLeague% = 0

    Else

        Open findFile$ For Input As #1

        Input #1, numberGames

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

        confWins = 0
        confLosses = 0

        fullWins = 0
        fullLosses = 0

        'RATINGS
        For I = 1 To numberGames

            statName$ = nameStatFile$(oppName$(I))
            oppName$(I) = targetFile$

            nonConfGame = 0

            For EC = 1 To TEAMS_PER_LEAGUE

                If statName$ = statFileNames$(EC) Then
                    nonConfGame = 1
                    Exit For
                End If

            Next EC

            If nonConfGame <> 1 Then

                If teamScore(I) > oppScore(I) Then 
                    fullWins = fullWins + 1
                End If

                If teamScore(I) < oppScore(I) Then 
                    fullLosses = fullLosses + 1
                End If

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
