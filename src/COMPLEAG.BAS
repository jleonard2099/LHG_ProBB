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

    Shared leagueT%()
    Shared leagBRC!()

    Shared O%()

    Shared LD!(), O1!(), O2!(), leagS!()
    Shared TRC!(), TRC1!(), statsW0!(), statsW1!()

    Shared BRC$(), DL$(), H1$()
    Shared N$(), O$(), OL$()
    Shared TRC$(), TRC1$(), Z2$(), Z3$()

    Shared DT!(), DL!()
    Shared OT!()

    'Initialize values first
    TRC!(14) = 19999
    TRC!(18) = 19999

    'These are defensive record values, so they should be really high
    'To determine if a "record" was broken, the stat values will be
    'compared to see if they are lower than this.
    'If we initialized to 0, the "record" would always be 0
    For I = 1 To 13: TRC1!(I) = 19999: Next
    For I = 15 To 17: TRC1!(I) = 19999: Next
    For I = 19 To 20: TRC1!(I) = 19999: Next

    If _FileExists(diskPaths$(1) + "RECORDS.PBB") Then

        Open diskPaths$(1) + "RECORDS.PBB" For Input As #1

        For I = 1 To 25
            Input #1, leagBRC!(I)
            Input #1, BRC$(I, 0)
            Input #1, BRC$(I, 1)
        Next

        For I = 1 To 21
            Input #1, TRC!(I)
            Input #1, TRC$(I)
        Next

        For I = 1 To 21
            Input #1, TRC1!(I)
            Input #1, TRC1$(I)
        Next

        Close #1

    End If

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
            Z1$(XW) = H1$(XW)
        Next

        Close #2

        For X = 1 To TEAMS_PER_CONFERENCE: DL!(X, 19) = 9999: Next

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

                        If Z1$(X) = WT$ Then

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

                        Erase leagS!

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

    Shared leagueT%(), O%()

    Shared H1$(), O$()

    If Not _FileExists(diskPaths$(1) + statFile$ + "S") Then

        Print "NO STATS FOUND FOR "; statFile$
        FindStatsLeague% = 0

    Else

        Open diskPaths$(1) + statFile$ + "S" For Input As #1

        Input #1, totalGames

        For I = 1 to NUM_STATRECORDS: Input #1, O%(I): Next I

        For I = 1 to NUM_STATRECORDS: Input #1, O$(I): Next I

        For I = 1 to NUM_STATRECORDS: Input #1, leagueT%(I): Next I

        Close #1

        fullWins = 0
        fullLosses = 0
        confWins = 0
        confLosses = 0

        For I = 1 To totalGames

            targetFile$ = nameStatFile$(O$(I))
            O$(I) = targetFile$

            confGame = 0

            For EC = 1 To TEAMS_PER_CONFERENCE

                If O$(I) = H1$(EC) Then
                    confGame = 1
                    Exit For
                End If

            Next EC

            If confGame <> 1 Then
                If O%(I) > leagueT%(I) Then fullWins = fullWins + 1
                If O%(I) < leagueT%(I) Then fullLosses = fullLosses + 1
            Else
                If O%(I) > leagueT%(I) Then confWins = confWins + 1: fullWins = fullWins + 1
                If O%(I) < leagueT%(I) Then confLosses = confLosses + 1: fullLosses = fullLosses + 1
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
    Shared LD!(), leagS!(), statsW0!(), statsW1!()

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

            LD!(60, 1) = leagS!(X, 19)     'FG%
            LD!(60, 2) = leagS!(X, 1)      'FGA
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

            LD!(80, 1) = leagS!(X, 20)  'FT%
            LD!(80, 2) = leagS!(X, 3)   'FTA
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

    Shared DT!(), leagBRC!(), leagS!(), OT!()
    Shared statsW0!(), statsW1!(), TRC!(), TRC1!()

    Shared BRC$(), N$(), TRC$(), TRC1$()

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
            If leagS!(I, 0) > leagBRC!(1) Then leagBRC!(1) = leagS!(I, 0): BRC$(1, 0) = N$(I): BRC$(1, 1) = TN$
            If leagS!(I, 1) > leagBRC!(2) Then leagBRC!(2) = leagS!(I, 1): BRC$(2, 0) = N$(I): BRC$(2, 1) = TN$
            If leagS!(I, 2) > leagBRC!(3) Then leagBRC!(3) = leagS!(I, 2): BRC$(3, 0) = N$(I): BRC$(3, 1) = TN$
            If leagS!(I, 19) > leagBRC!(4) And UCase$(finalUpdate$) = "Y" And leagS!(I, 2) / leagS!(I, 0) >= 3.65 Then leagBRC!(4) = leagS!(I, 19): BRC$(4, 0) = N$(I): BRC$(4, 1) = TN$

            If statsW1!(I) > leagBRC!(5) Then leagBRC!(5) = statsW1!(I): BRC$(5, 0) = N$(I): BRC$(5, 1) = TN$
            If statsW0!(I) > leagBRC!(6) Then leagBRC!(6) = statsW0!(I): BRC$(6, 0) = N$(I): BRC$(6, 1) = TN$
            If leagS!(I, 21) > leagBRC!(7) And UCase$(finalUpdate$) = "Y" And statsW0!(I) / leagS!(I, 0) >= .3 Then leagBRC!(7) = leagS!(I, 21): BRC$(7, 0) = N$(I): BRC$(7, 1) = TN$

            If leagS!(I, 3) > leagBRC!(8) Then leagBRC!(8) = leagS!(I, 3): BRC$(8, 0) = N$(I): BRC$(8, 1) = TN$
            If leagS!(I, 4) > leagBRC!(9) Then leagBRC!(9) = leagS!(I, 4): BRC$(9, 0) = N$(I): BRC$(9, 1) = TN$
            If leagS!(I, 20) > leagBRC!(10) And UCase$(finalUpdate$) = "Y" And leagS!(I, 4) / leagS!(I, 0) >= 1.52 Then leagBRC!(10) = leagS!(I, 20): BRC$(10, 0) = N$(I): BRC$(10, 1) = TN$

            If leagS!(I, 10) / 60 > leagBRC!(11) Then leagBRC!(11) = leagS!(I, 10) / 60: BRC$(11, 0) = N$(I): BRC$(11, 1) = TN$
            If leagS!(I, 12) > leagBRC!(12) Then leagBRC!(12) = leagS!(I, 12): BRC$(12, 0) = N$(I): BRC$(12, 1) = TN$
            If leagS!(I, 22) > leagBRC!(13) And UCase$(finalUpdate$) = "Y" Then leagBRC!(13) = leagS!(I, 22): BRC$(13, 0) = N$(I): BRC$(13, 1) = TN$

            If leagS!(I, 25) > leagBRC!(14) Then leagBRC!(14) = leagS!(I, 25): BRC$(14, 0) = N$(I): BRC$(14, 1) = TN$
            If leagS!(I, 26) > leagBRC!(15) Then leagBRC!(15) = leagS!(I, 26): BRC$(15, 0) = N$(I): BRC$(15, 1) = TN$
            If leagS!(I, 5) - leagS!(I, 26) > leagBRC!(16) Then leagBRC!(16) = leagS!(I, 5) - leagS!(I, 26): BRC$(16, 0) = N$(I): BRC$(16, 1) = TN$
            If leagS!(I, 5) > leagBRC!(17) Then leagBRC!(17) = leagS!(I, 5): BRC$(17, 0) = N$(I): BRC$(17, 1) = TN$
            If leagS!(I, 13) > leagBRC!(18) And UCase$(finalUpdate$) = "Y" Then leagBRC!(18) = leagS!(I, 13): BRC$(18, 0) = N$(I): BRC$(18, 1) = TN$

            If leagS!(I, 6) > leagBRC!(19) Then leagBRC!(19) = leagS!(I, 6): BRC$(19, 0) = N$(I): BRC$(19, 1) = TN$
            If leagS!(I, 7) > leagBRC!(20) Then leagBRC!(20) = leagS!(I, 7): BRC$(20, 0) = N$(I): BRC$(20, 1) = TN$
            If leagS!(I, 8) > leagBRC!(21) Then leagBRC!(21) = leagS!(I, 8): BRC$(21, 0) = N$(I): BRC$(21, 1) = TN$
            If leagS!(I, 9) > leagBRC!(22) Then leagBRC!(22) = leagS!(I, 9): BRC$(22, 0) = N$(I): BRC$(22, 1) = TN$
            If leagS!(I, 11) > leagBRC!(23) Then leagBRC!(23) = leagS!(I, 11): BRC$(23, 0) = N$(I): BRC$(23, 1) = TN$
            If leagS!(I, 18) > leagBRC!(24) And UCase$(finalUpdate$) = "Y" Then leagBRC!(24) = leagS!(I, 18): BRC$(24, 0) = N$(I): BRC$(24, 1) = TN$

            If leagS!(I, 24) > leagBRC!(25) Then leagBRC!(25) = leagS!(I, 24): BRC$(25, 0) = N$(I): BRC$(25, 1) = TN$

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

    If OT!(2) > TRC!(1) Then TRC!(1) = OT!(2): TRC$(1) = TN$
    If OT!(3) > TRC!(2) Then TRC!(2) = OT!(3): TRC$(2) = TN$
    If OT!(17) > TRC!(3) And UCase$(finalUpdate$) = "Y" Then TRC!(3) = OT!(17): TRC$(3) = TN$

    If OT!(15) > TRC!(4) Then TRC!(4) = OT!(15): TRC$(4) = TN$
    If OT!(14) > TRC!(5) Then TRC!(5) = OT!(14): TRC$(5) = TN$
    If OT!(21) > TRC!(6) And UCase$(finalUpdate$) = "Y" Then TRC!(6) = OT!(21): TRC$(6) = TN$

    If OT!(4) > TRC!(7) Then TRC!(7) = OT!(4): TRC$(7) = TN$
    If OT!(5) > TRC!(8) Then TRC!(8) = OT!(5): TRC$(8) = TN$
    If OT!(18) > TRC!(9) And UCase$(finalUpdate$) = "Y" Then TRC!(9) = OT!(18): TRC$(9) = TN$

    If OT!(13) > TRC!(10) Then TRC!(10) = OT!(13): TRC$(10) = TN$
    If OT!(6) - OT!(13) > TRC!(11) Then TRC!(11) = OT!(6) - OT!(13): TRC$(11) = TN$
    If OT!(6) > TRC!(12) Then TRC!(12) = OT!(6): TRC$(12) = TN$
    If OT!(6) / totalGames > TRC!(13) And UCase$(finalUpdate$) = "Y" Then TRC!(13) = OT!(6) / totalGames: TRC$(13) = TN$

    If OT!(7) < TRC!(14) And UCase$(finalUpdate$) = "Y" Then TRC!(14) = OT!(7): TRC$(14) = TN$

    If OT!(8) > TRC!(15) Then TRC!(15) = OT!(8): TRC$(15) = TN$
    If OT!(9) > TRC!(16) Then TRC!(16) = OT!(9): TRC$(16) = TN$
    If OT!(10) > TRC!(17) Then TRC!(17) = OT!(10): TRC$(17) = TN$
    If OT!(12) < TRC!(18) And UCase$(finalUpdate$) = "Y" Then TRC!(18) = OT!(12): TRC$(18) = TN$

    If OT!(11) > TRC!(19) Then TRC!(19) = OT!(11): TRC$(19) = TN$
    If OT!(11) / totalGames > TRC!(20) And UCase$(finalUpdate$) = "Y" Then TRC!(20) = OT!(11) / totalGames: TRC$(20) = TN$
    If fullWins > TRC!(21) Then TRC!(21) = fullWins: TRC$(21) = TN$

    'FG's
    If DT!(2) < TRC1!(1) Then TRC1!(1) = DT!(2): TRC1$(1) = TN$
    If DT!(3) < TRC1!(2) Then TRC1!(2) = DT!(3): TRC1$(2) = TN$
    If DT!(17) < TRC1!(3) And UCase$(finalUpdate$) = "Y" Then TRC1!(3) = DT!(17): TRC1$(3) = TN$

    '3 FGs
    If DT!(15) < TRC1!(4) Then TRC1!(4) = DT!(15): TRC1$(4) = TN$
    If DT!(14) < TRC1!(5) Then TRC1!(5) = DT!(14): TRC1$(5) = TN$
    If DT!(21) < TRC1!(6) And DT!(15) <> 0 And UCase$(finalUpdate$) = "Y" Then TRC1!(6) = DT!(21): TRC1$(6) = TN$

    If DT!(4) < TRC1!(7) Then TRC1!(7) = DT!(4): TRC1$(7) = TN$
    If DT!(5) < TRC1!(8) Then TRC1!(8) = DT!(5): TRC1$(8) = TN$
    If DT!(18) < TRC1!(9) And UCase$(finalUpdate$) = "Y" Then TRC1!(9) = DT!(18): TRC1$(9) = TN$

    If DT!(13) < TRC1!(10) Then TRC1!(10) = DT!(13): TRC1$(10) = TN$
    If DT!(6) - DT!(13) < TRC1!(11) Then TRC1!(11) = DT!(6) - DT!(13): TRC1$(11) = TN$
    If DT!(6) < TRC1!(12) Then TRC1!(12) = DT!(6): TRC1$(12) = TN$
    If DT!(6) / totalGames < TRC1!(13) And UCase$(finalUpdate$) = "Y" Then TRC1!(13) = DT!(6) / totalGames: TRC1$(13) = TN$

    If DT!(7) > TRC1!(14) And UCase$(finalUpdate$) = "Y" Then TRC1!(14) = DT!(7): TRC1$(14) = TN$

    If DT!(8) < TRC1!(15) Then TRC1!(15) = DT!(8): TRC1$(15) = TN$
    If DT!(9) < TRC1!(16) Then TRC1!(16) = DT!(9): TRC1$(16) = TN$
    If DT!(10) < TRC1!(17) Then TRC1!(17) = DT!(10): TRC1$(17) = TN$
    If DT!(12) > TRC1!(18) And UCase$(finalUpdate$) = "Y" Then TRC1!(18) = DT!(12): TRC1$(18) = TN$

    If DT!(11) < TRC1!(19) Then TRC1!(19) = DT!(11): TRC1$(19) = TN$
    If DT!(11) / totalGames < TRC1!(20) And UCase$(finalUpdate$) = "Y" Then TRC1!(20) = DT!(11) / totalGames: TRC1$(20) = TN$

    If fullLosses > TRC1!(21) Then TRC1!(21) = fullLosses: TRC1$(21) = TN$

End Sub


'----------------------------------------
'      SortLeadersPro Subroutine
'----------------------------------------
'This subroutine sorts the leaders in the
'target league
Sub SortLeadersPro ()

    Shared O1!(), O2!()
    'Shared Z1$()

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
                            Swap Z1$(X), Z1$(Y)
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

    Shared DL!(), LD!(), leagBRC!()
    Shared O1!(), O2!(), OL!()
    Shared TRC!(), TRC1!()

    Shared BRC$(), DL$(), OL$()
    Shared TRC$(), TRC1$()
    Shared Z1$(), Z2$(), Z3$()

    Cls

    Print

    Open diskPaths$(1) + leaderFile$ + ".LDR" For Output As #1

    Print "    POSTING NEW "; leaderFile$; " LEADERS"

    For X = 1 To TEAMS_PER_CONFERENCE
        Write #1, Z1$(X), O1!(X), O2!(X)
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
        Print #2, leagBRC!(I)
        Print #2, BRC$(I, 0)
        Print #2, BRC$(I, 1)
    Next

    For I = 1 To 21
        Print #2, TRC!(I)
        Print #2, TRC$(I)
    Next

    For I = 1 To 21
        Print #2, TRC1!(I)
        Print #2, TRC1$(I)
    Next

    Close #2

End Sub

