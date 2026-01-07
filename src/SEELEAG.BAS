'----------------------------------------
'           SEELEAG Subroutine
'----------------------------------------
'This subroutine lets the user choose a
'league stat file to view, then calastLoseStreak
'other routines to both load the Stats
'and then provide options for display
Sub SEELEAG ()

    _Title "Basketball: The Pro Game - View League Stats"

    leaderFile$ = _OpenFileDialog$("Choose League Stats file", diskPaths$(2), "*.LDR", "League Stats files [.LDR]")
    'ChooseFile$(diskPaths$(2) + "*.LDR", "LEADER", ExitCode%)

    If leaderFile$ <> "" Then

        Call LoadLeaderFiles_League(leaderFile$)
        Call StatOptions_League

    End If


End Sub


'----------------------------------------
'     LoadLeaderFiles_League Subroutine
'----------------------------------------
'This subroutine loads the conference stats
'from the file into several arrays
Sub LoadLeaderFiles_League (targetFile$)

    Shared divWins(), divLosses()
    Shared defLeaders!(), offLeaders!()
    Shared compLgStatVal!(), compLgStatAtt!()

    Shared defLeaderTeams$(), offLeaderNames$()
    Shared compLdrTeam$(), leagLdrPlayer$(), leagLdrTeam$()

    Shared leagueName$, div1Name$, div2Name$, div3Name$, div4Name$

    Open diskPaths$(2) + GetFileBaseName$(targetFile$) + ".TMS" For Input As #1
    Input #1, leagueName$, div1Name$, div2Name$, div3Name$, div4Name$
    Close #1

    div1Name$ = PadRight$(div1Name$, 8)
    div2Name$ = PadRight$(div2Name$, 8)
    div3Name$ = PadRight$(div3Name$, 8)
    div4Name$ = PadRight$(div4Name$, 8)

    Cls

    Locate 9, 6
    Print "PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

    Open targetFile$ For Input As #2

    For X = 1 To TEAMS_PER_LEAGUE
        Input #2, compLdrTeam$(X), divWins(X), divLosses(X)
    Next

    For X = 1 To 250
        Input #2, leagLdrPlayer$(X), leagLdrTeam$(X), compLgStatVal!(X), compLgStatAtt!(X)
    Next

    For X = 1 To TEAMS_PER_LEAGUE
        Input #2, offLeaderNames$(X)
        For M = 1 To 20
            Input #2, offLeaders!(X, M)
        Next
    Next

    For X = 1 To TEAMS_PER_LEAGUE
        Input #2, defLeaderTeams$(X)
        For M = 1 To 20
            Input #2, defLeaders!(X, M)
        Next
    Next

    Close #2

End Sub


'----------------------------------------
'       StatOptions_League Subroutine
'----------------------------------------
'This subroutine gives the user various
'options for viewing the previously loaded
'stats, and then calastLoseStreak routines for each
Sub StatOptions_League ()

    Shared leagueName$

    Do

        Cls
        Call TITLE_STATS

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F":
                    userFile$ = _SaveFileDialog$("Save League Report", _CWD$ + "\Report", "*.txt", "Text File")
                    Call LeaderStats_Report(0, userFile$)

                Case "P":
                    Call LeaderStats_Report(0, "REPORT")

                Case "S":
                    Call LeagStdgs_Screen
                    Call LeaderStats_Screen

            End Select

            Color 15, 0
            Cls

            Call box0(6, 20, 10, 60, 2, 15, 1)

            Do
                Color 15, 1
                Locate 8
                U$ = "VIEW LEAGUE STATS AGAIN? (Y/N)"
                Locate , (80 - Len(U$)) / 2
                Print U$
                Color 15, 0
                A$ = GetKeyPress$
            Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

        End If

    Loop Until UCase$(A$) = "N" Or A$ = Chr$(27) Or userKey$ = Chr$(27)

End Sub


'----------------------------------------
'     LeagStdgs_Screen Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to the end
'user display.
Sub LeagStdgs_Screen ()

    Shared divWins(), divLosses()

    Shared compLdrTeam$()
    Shared leagueName$, div1Name$, div2Name$, div3Name$, div4Name$

    Color 15, 0

    Cls

    Color 12

    Locate 2, Int((80 - Len(leagueName$) - 17) / 2)
    Print leagueName$; " LEAGUE STANDINGS"

    'Cycle through divisions
    For division = 1 To 4

        Select Case division

            Case 1
                divisionName$ = RTrim$(div1Name$)
                yCoord = 4
                xCoord = 1

            Case 2
                divisionName$ = RTrim$(div2Name$)
                yCoord = 4
                xCoord = 41

            Case 3
                divisionName$ = RTrim$(div3Name$)
                yCoord = 17
                xCoord = 1

            Case 4
                divisionName$ = RTrim$(div4Name$)
                yCoord = 17
                xCoord = 41

        End Select

        'If division = 1 Or (division > 1 And divisionName$ <> "") Then

        Locate yCoord, xCoord

        Color 2
        Print "    " + divisionName$;

        Locate yCoord, xCoord + 17

        Color 14
        Print " W   L    PCT   GB"

        Color 15

        RT = 0
        first = ((division - 1) * 10) + 1
        last = division * 10

        D1 = divWins(first) - divLosses(first)

        For P = first To last

            If compLdrTeam$(P) = "" Or compLdrTeam$(P) = "*" Then
                RT = RT + 1
            Else

                'Games Behind
                D = divWins(P) - divLosses(P)
                D = D1 - D

                BK$ = "   " + Str$(Int(D / 2))

                If D = 0 Then BK$ = "   ---"
                If D = 1 Then BK$ = "     "
                If D > 19 Then
                    BK$ = Right$(BK$, Len(BK$) - 1)
                End If
                If D / 2 <> Int(D / 2) Then
                    BK$ = BK$ + " 1/2"
                End If

                Select Case division

                    Case 1
                        Locate (P + 4 - RT), 1

                    Case 2
                        Locate (P - 6 - RT), 41

                    Case 3
                        Locate (P - 3 - RT), 1

                    Case 4
                        Locate (P - 13 - RT), 41

                End Select

                Print "    "; PadRight$(compLdrTeam$(P), 13); Using "##  ##  "; divWins(P); divLosses(P);

                'This will throw errors if there are 0 games!!!
                PCT! = divWins(P) / (divWins(P) + divLosses(P))

                Print Using "#.###"; PCT!;
                Print BK$

            End If

        Next P

        'End If

    Next division

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

End Sub


'----------------------------------------
'     LeaderStats_Screen Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to the end
'user display.
Sub LeaderStats_Screen ()

    Shared defLeaders!(), offLeaders!()
    Shared compLgStatVal!(), compLgStatAtt!()

    Shared defLeaderTeams$(), offLeaderNames$(), offLeaders!()
    Shared compLdrTeam$(), leagLdrPlayer$(), leagLdrTeam$()

    Shared leagueName$, div1Name$, div2Name$, div3Name$, div4Name$

    Cls

    Locate 2, 1
    Color 14
    Print "FIELD GOAL % (MIN 5 FGA/G)"
    Print "                            PCT    FGA"
    Color 15

    For P = 41 To 60
        If leagLdrPlayer$(P) <> "*" And leagLdrPlayer$(P) <> "" Then
            Print Left$(leagLdrPlayer$(P), 10) + ", " + leagLdrTeam$(P);
            Locate , 28
            Print Using "##.#  ####"; compLgStatVal!(P) * 100; compLgStatAtt!(P)
        End If
    Next

    Color 14
    Locate 2, 41
    Print "FREE THROW % (MIN 2 FTA/G)"
    Locate , 41
    Print "                            PCT   FTA"
    Color 15

    For P = 61 To 80
        If leagLdrPlayer$(P) <> "*" And leagLdrPlayer$(P) <> "" Then
            Locate , 41
            Print Left$(leagLdrPlayer$(P), 10) + ", " + leagLdrTeam$(P);
            Locate , 68
            Print Using "##.#  ####"; compLgStatVal!(P) * 100; compLgStatAtt!(P)
        End If
    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Locate 2, 1
    Print "SCORING"
    Print Tab(33); "PTS/G"
    Color 15

    For P = 1 To 20
        If leagLdrPlayer$(P) <> "*" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34
            Print Using "###.#"; compLgStatVal!(P)
        End If
    Next

    Locate 2, 41
    Color 14
    Print "TOTAL POINTS"
    Locate , 75
    Print "PTS"
    Color 15

    For P = 21 To 40
        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74
            Print Using "####"; compLgStatVal!(P)
        End If
    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 14
    Print "STEALS PER GAME"
    Color 15

    For P = 81 To 90
        If leagLdrPlayer$(P) <> "*" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34
            Print Using "###.#"; compLgStatVal!(P)
        End If
    Next

    Locate 2, 41
    Color 14
    Print "STEALS"
    Color 15

    For P = 91 To 100
        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74
            Print Using "####"; compLgStatVal!(P)
        End If
    Next

    Color 14
    Locate 14: Print "ASSISTS PER GAME"
    Color 15

    For P = 101 To 110
        If leagLdrPlayer$(P) <> "*" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34
            Print Using "###.#"; compLgStatVal!(P)
        End If
    Next

    Locate 14, 41
    Color 14
    Print "ASSISTS"
    Color 15

    For P = 111 To 120
        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74
            Print Using "####"; compLgStatVal!(P)
        End If
    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 14
    Print "THREE FIELD GOAL %"
    Color 15

    For P = 121 To 130
        If leagLdrPlayer$(P) <> "" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34
            Print Using "###.#"; compLgStatVal!(P) * 100
        End If
    Next P

    Locate 2, 41
    Color 14
    Print "MINUTES PER GAME"
    Color 15

    For P = 141 To 150
        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74
            Print Using "###.#"; compLgStatVal!(P)
        End If
    Next P

    Locate 14
    Color 14
    Print "THREE POINT FIELD GOALS MADE"
    Color 15

    For P = 131 To 140
        If leagLdrPlayer$(P) <> "" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34: Print Using "####"; compLgStatVal!(P)
        End If
    Next

    Locate 14, 41
    Color 14
    Print "TOTAL MINUTES"
    Color 15

    For P = 151 To 160
        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74
            Print Using "####"; compLgStatVal!(P)
        End If
    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 14
    Print "REBOUNDS PER GAME   "
    Color 15

    For P = 161 To 170
        If leagLdrPlayer$(P) <> "*" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34: Print Using "###.#"; compLgStatVal!(P)
        End If
    Next

    Locate 2, 41
    Color 14
    Print "REBOUNDS"
    Color 15

    For P = 171 To 180
        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74
            Print Using "####"; compLgStatVal!(P)
        End If
    Next

    Locate 14
    Color 14
    Print "BLOCKS PER GAME"
    Color 15

    For P = 181 To 190
        If leagLdrPlayer$(P) <> "*" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34
            Print Using "###.#"; compLgStatVal!(P)
        End If
    Next

    Locate 14, 41
    Color 14
    Print "BLOCKS"
    Color 15

    For P = 191 To 200
        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74
            Print Using "####"; compLgStatVal!(P)
        End If
    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 14
    Print "FIELD GOALS MADE "
    Color 15

    For P = 201 To 210
        If leagLdrPlayer$(P) <> "*" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34
            Print Using "####"; compLgStatVal!(P)
        End If
    Next

    Locate 2, 41
    Color 14
    Print "FREE THROWS MADE"
    Color 15

    For P = 211 To 220
        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74
            Print Using "####"; compLgStatVal!(P)
        End If
    Next

    Locate 14
    Color 14
    Print "FOULS PER GAME"
    Color 15

    For P = 221 To 230
        If leagLdrPlayer$(P) <> "*" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34
            Print Using "###.#"; compLgStatVal!(P)
        End If
    Next

    Locate 14, 41
    Color 14
    Print "FOULS"
    Color 15

    For P = 231 To 240
        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74
            Print Using "####"; compLgStatVal!(P)
        End If
    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 14
    Print "MOST VALUABLE PLAYER POINTS"
    Color 15

    For P = 241 To 250
        If leagLdrPlayer$(P) <> "*" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34
            Print Using "####"; compLgStatVal!(P)
        End If
    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 12
    Print Tab(32); "OFFENSIVE TOTALS"

    Locate 4, 1
    Color 14
    Print Tab(13); "G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 1 To 20

        If offLeaderNames$(X) <> "" And offLeaderNames$(X) <> "*" And offLeaderNames$(X) <> " " Then

            Print Left$(offLeaderNames$(X), 11); Tab(13);

            'Games
            Print Using "##"; offLeaders!(X, 16);
            Print Tab(16);

            'FGA
            Print Using "####"; offLeaders!(X, 1);
            Print Tab(21);

            'FGM
            Print Using "####"; offLeaders!(X, 2);
            Print Tab(26);

            'FG %
            Print Using ".###"; offLeaders!(X, 17);
            Print Tab(31);

            'FTA / FTM / FT%
            Print Using "#### #### .###"; offLeaders!(X, 3); offLeaders!(X, 4); offLeaders!(X, 18);
            Print Tab(46);

            'Rebounds
            Print Using "####"; offLeaders!(X, 5);
            Print Tab(51);

            'Assists
            Print Using "####"; offLeaders!(X, 8);
            Print Tab(56);

            'Personal Fouls
            Print Using "####"; offLeaders!(X, 6);
            Print Tab(61);

            'Steals
            Print Using "###"; offLeaders!(X, 7);
            Print Tab(65);

            'Blocks
            Print Using "###"; offLeaders!(X, 9);
            Print Tab(69);

            'Points
            Print Using "#####"; offLeaders!(X, 20);
            Print Tab(75);

            'Average
            Print Using "###.#"; offLeaders!(X, 19)

        End If

    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls


    Locate 2, 1
    Color 12
    Print Tab(27); "OFFENSIVE TOTALS (cont'd)"
    Locate 4, 1
    Color 14
    Print Tab(13); "G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 21 To 40

        If offLeaderNames$(X) <> "" And offLeaderNames$(X) <> "*" And offLeaderNames$(X) <> " " Then

            Print Left$(offLeaderNames$(X), 11); Tab(13);

            'Games
            Print Using "##"; offLeaders!(X, 16);
            Print Tab(16);

            'FGA
            Print Using "####"; offLeaders!(X, 1);
            Print Tab(21);

            'FGM
            Print Using "####"; offLeaders!(X, 2);
            Print Tab(26);

            'FG %
            Print Using ".###"; offLeaders!(X, 17);
            Print Tab(31);

            'FTA / FTM / FT%
            Print Using "#### #### .###"; offLeaders!(X, 3); offLeaders!(X, 4); offLeaders!(X, 18);
            Print Tab(46);

            'Rebounds
            Print Using "####"; offLeaders!(X, 5);
            Print Tab(51);

            'Assists
            Print Using "####"; offLeaders!(X, 8);
            Print Tab(56);

            'Personal Fouls
            Print Using "####"; offLeaders!(X, 6);
            Print Tab(61);

            'Steals
            Print Using "###"; offLeaders!(X, 7);
            Print Tab(65);

            'Blocks
            Print Using "###"; offLeaders!(X, 9);
            Print Tab(69);

            'Points
            Print Using "#####"; offLeaders!(X, 20);
            Print Tab(75);

            'Average
            Print Using "###.#"; offLeaders!(X, 19)

        End If

    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 12
    Print Tab(32); "DEFENSIVE TOTALS"

    Locate 4, 1
    Color 14
    Print Tab(13); "G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 1 To 20

        If defLeaderTeams$(X) <> "" And defLeaderTeams$(X) <> "*" And defLeaderTeams$(X) <> " " Then

            Print Left$(defLeaderTeams$(X), 11); Tab(13);

            'Games
            Print Using "##"; defLeaders!(X, 16);
            Print Tab(16);

            'FGA
            Print Using "####"; defLeaders!(X, 1);
            Print Tab(21);

            'FGM
            Print Using "####"; defLeaders!(X, 2);
            Print Tab(26);

            'FG PCT
            Print Using ".###"; defLeaders!(X, 17);
            Print Tab(31);

            'FTA / FTM / FT %
            Print Using "#### #### .###"; defLeaders!(X, 3); defLeaders!(X, 4); defLeaders!(X, 18);
            Print Tab(46);

            'Rebounds
            Print Using "####"; defLeaders!(X, 5);
            Print Tab(51);

            'Assists
            Print Using "####"; defLeaders!(X, 8);
            Print Tab(56);

            'Personal FOuls
            Print Using "####"; defLeaders!(X, 6);
            Print Tab(61);

            'Steals
            Print Using "###"; defLeaders!(X, 7);
            Print Tab(65);

            'Blocks
            Print Using "###"; defLeaders!(X, 9);
            Print Tab(69);

            'Points
            Print Using "#####"; defLeaders!(X, 20);
            Print Tab(75);

            'Average
            Print Using "###.#"; defLeaders!(X, 19)

        End If

    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Locate 2, 1
    Color 12
    Print Tab(27); "DEFENSIVE TOTALS (cont'd)"

    Locate 4, 1
    Color 14
    Print Tab(13); "G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 21 To 40

        If defLeaderTeams$(X) <> "" And defLeaderTeams$(X) <> "*" And defLeaderTeams$(X) <> " " Then

            Print Left$(defLeaderTeams$(X), 11); Tab(13);

            'Games
            Print Using "##"; defLeaders!(X, 16);
            Print Tab(16);

            'FGA
            Print Using "####"; defLeaders!(X, 1);
            Print Tab(21);

            'FGM
            Print Using "####"; defLeaders!(X, 2);
            Print Tab(26);

            'FG PCT
            Print Using ".###"; defLeaders!(X, 17);
            Print Tab(31);

            'FTA / FTM / FT %
            Print Using "#### #### .###"; defLeaders!(X, 3); defLeaders!(X, 4); defLeaders!(X, 18);
            Print Tab(46);

            'Rebounds
            Print Using "####"; defLeaders!(X, 5);
            Print Tab(51);

            'Assists
            Print Using "####"; defLeaders!(X, 8);
            Print Tab(56);

            'Personal FOuls
            Print Using "####"; defLeaders!(X, 6);
            Print Tab(61);

            'Steals
            Print Using "###"; defLeaders!(X, 7);
            Print Tab(65);

            'Blocks
            Print Using "###"; defLeaders!(X, 9);
            Print Tab(69);

            'Points
            Print Using "#####"; defLeaders!(X, 20);
            Print Tab(75);

            'Average
            Print Using "###.#"; defLeaders!(X, 19)

        End If

    Next

    '-----
    Color 12
    Locate LAST_ROW - 1, 28: Print "(HIT ANY KEY TO CONTINUE)";
    Color 15
    QK$ = GetKeyPress$
    '-----

End Sub


'----------------------------------------
'      LeaderStats_Report Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to a file.
'This file can then be looped through to
'send to the default printer

'Destination
'   0 = File
'   1 = Printer
Sub LeaderStats_Report (printDest, destFile$)

    Shared divWins(), divLosses()
    Shared defLeaders!(), offLeaders!()
    Shared compLgStatVal!(), compLgStatAtt!()

    Shared offLeaderNames$(), defLeaderTeams$(), compLdrTeam$()
    Shared leagLdrPlayer$(), leagLdrTeam$()

    Shared leagueName$, div1Name$, div2Name$, div3Name$, div4Name$

    Color 0, 15
    Cls

    Open destFile$ For Output As #1

    XCoord = Int((80 - Len(leagueName$) - 17) / 2)
    Print #1,
    Print #1, Tab(XCoord); leagueName$; " LEAGUE STANDINGS"
    Print #1,

    For division = 1 To 4

        Select Case division

            Case 1
                divisionName$ = RTrim$(div1Name$)

            Case 2
                divisionName$ = RTrim$(div2Name$)

            Case 3
                divisionName$ = RTrim$(div3Name$)

            Case 4
                divisionName$ = RTrim$(div4Name$)

        End Select

        'If division = 1 Or (division > 1 And divisionName$ <> "") Then

        Print #1, "    "; PadRight$(divisionName$,15);
        Print #1, "W    L     PCT    GB"

        first = ((division - 1) * 10) + 1
        last = division * 10

        D1 = divWins(first) - divLosses(first)

        For P = first To last

            If compLdrTeam$(P) <> "" And compLdrTeam$(P) <> "*" Then

                'Win %
                PCT! = divWins(P) / (divWins(P) + divLosses(P))

                'Games Behind
                D = divWins(P) - divLosses(P)
                D = D1 - D

                BK$ = "   " + Str$(Int(D / 2))

                If D = 0 Then BK$ = "   ---"
                If D = 1 Then BK$ = "     "
                If D > 19 Then
                    BK$ = Right$(BK$, Len(BK$) - 1)
                End If
                If D / 2 <> Int(D / 2) Then
                    BK$ = BK$ + " 1/2"
                End If

                Print #1, "    "; compLdrTeam$(P); Tab(16);

                Print #1, Using "   ##   ##  "; divWins(P); divLosses(P);

                PCT! = divWins(P) / (divWins(P) + divLosses(P))
                
                Print #1, Using " #.### "; PCT!;
                Print #1, BK$

            End If

        Next P

    Next division

    Print #1,
    If printDest = 1 Then Print #1, Chr$(12)
    Print #1,

    Print #1, "FIELD GOAL % (MIN 5 FGA/G)"; Tab(41); "FREE THROW % (MIN 2 FTA/G)"
    Print #1,
    Print #1, "                            PCT   FGA"; Tab(41); "                            PCT   FTA"

    For P = 41 To 60

        If leaderName$(P) <> "" Then
            Print #1, Left$(leagLdrPlayer$(P), 10) + ", " + leagLdrTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "##.#  ####"; compLgStatVal!(P) * 100; compLgStatAtt!(P);
        End If

		Print #1, Tab(41);
        If leagLdrPlayer$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, Left$(leagLdrPlayer$(P + 20), 10) + ", " + leagLdrTeam$(P + 20);
            Print #1, Tab(68);
            Print #1, Using "##.#  ####"; compLgStatVal!(P + 20) * 100; compLgStatAtt!(P)
        End If

    Next

    Print #1,
    Print #1, "SCORING"; Tab(41); "TOTAL POINTS"

    For P = 1 To 20

        If leaderName$(P) <> "" Then
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(33);
            Print #1, Using "###.#"; compLgStatVal!(P);
        End If

		Print #1, Tab(41);
        If leagLdrPlayer$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, leagLdrPlayer$(P + 20) + ", " + leagLdrTeam$(P + 20);
            Print #1, Tab(74);
            Print #1, Using "####"; compLgStatVal!(P + 20)
        End If

    Next

    Print #1,
    If printDest = 1 Then Print #1, Chr$(12)
    Print #1,

    Print #1, "STEALS PER GAME"; Tab(41); "STEALS"

    For P = 81 To 90

        If leaderName$(P) <> "" Then
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(33);
            Print #1, Using "###.#"; compLgStatVal!(P);
        End If

		Print #1, Tab(41);
        If leagLdrPlayer$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, leagLdrPlayer$(P + 10) + ", " + leagLdrTeam$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; compLgStatVal!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "ASSISTS PER GAME"; Tab(41); "ASSISTS"

    For P = 101 To 110

        If leaderName$(P) <> "" Then
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(33);
            Print #1, Using "###.#"; compLgStatVal!(P);
        End If

		Print #1, Tab(41);
        If leagLdrPlayer$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, leagLdrPlayer$(P + 10) + ", " + leagLdrTeam$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; compLgStatVal!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "THREE FIELD GOAL %"; Tab(41); "MINUTES PER GAME"

    For P = 121 To 130

        If leaderName$(P) <> "" Then
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(33);
            Print #1, Using "###.#"; compLgStatVal!(P) * 100;
        End If

		Print #1, Tab(41);
        If leagLdrPlayer$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, leagLdrPlayer$(P + 20) + ", " + leagLdrTeam$(P + 20);
            Print #1, Tab(74);
            Print #1, Using "###.#"; compLgStatVal!(P + 20)
        End If

    Next

    Print #1,
    Print #1, "THREE POINT FIELD GOALS MADE"; Tab(41); "MINUTES"

    For P = 131 To 140

        If leaderName$(P) <> "" Then
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "####"; compLgStatVal!(P);
        End If

		Print #1, Tab(41);
        If leagLdrPlayer$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, leagLdrPlayer$(P + 20) + ", " + leagLdrTeam$(P + 20);
            Print #1, Tab(74);
            Print #1, Using "####"; compLgStatVal!(P + 20)
        End If

    Next

    Print #1,
    If printDest = 1 Then Print #1, Chr$(12)
    Print #1,
    
    Print #1, "REBOUNDS PER GAME"; Tab(41); "REBOUNDS"

    For P = 161 To 170

        If leaderName$(P) <> "" Then
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(33);
            Print #1, Using "###.#"; compLgStatVal!(P);
        End If

		Print #1, Tab(41);
        If leagLdrPlayer$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, leagLdrPlayer$(P + 10) + ", " + leagLdrTeam$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; compLgStatVal!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "BLOCKS PER GAME"; Tab(41); "BLOCKS"

    For P = 181 To 190

        If leaderName$(P) <> "" Then
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(33);
            Print #1, Using "###.#"; compLgStatVal!(P);
        End If

		Print #1, Tab(41);
        If leagLdrPlayer$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, leagLdrPlayer$(P + 10) + ", " + leagLdrTeam$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; compLgStatVal!(P + 20)
        End If

    Next

    Print #1,
    Print #1, "FIELD GOALS MADE"; Tab(41); "FREE THROWS MADE"

    For P = 201 To 210

        If leaderName$(P) <> "" Then
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "####"; compLgStatVal!(P);
        End If

		Print #1, Tab(41);
        If leagLdrPlayer$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, leagLdrPlayer$(P + 10) + ", " + leagLdrTeam$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; compLgStatVal!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "FOULS PER GAME"; Tab(41); "FOULS"

    For P = 221 To 230

        If leaderName$(P) <> "" Then
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(33);
            Print #1, Using "###.#"; compLgStatVal!(P);
        End If

		Print #1, Tab(41);
        If leagLdrPlayer$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, leagLdrPlayer$(P + 10) + ", " + leagLdrTeam$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; compLgStatVal!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "MOST VALUABLE PLAYER"

    For P = 241 To 250
        If leagLdrPlayer$(P) <> "" Then
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "####"; compLgStatVal!(P)
        End If
    Next

    Print #1,
    If printDest = 1 Then Print #1, Chr$(12)
    Print #1,

    Print #1, Tab(32); "OFFENSIVE TOTALS"
    Print #1, Tab(13); "G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG": Color 15

    For X = 1 To TEAMS_PER_LEAGUE

        If offLeaderNames$(X) <> "" And offLeaderNames$(X) <> "*" And offLeaderNames$(X) <> " " Then

            'Player
            Print #1, offLeaderTeams$(X);
            
            'Games
            Print #1, Tab(16);
            Print #1, Using "##"; offLeaders!(X, 16);

            'FGA
            Print #1, Tab(19);
            Print #1, Using "####"; offLeaders!(X, 1);

            'FGM
            Print #1, Tab(24);
            Print #1, Using "####"; offLeaders!(X, 2);

            'FG %
            Print #1, Tab(29);
            Print #1, Using ".###"; offLeaders!(X, 17);

            'FTA / FTM / FT%
            Print #1, Tab(34);
            Print #1, Using "#### #### .###"; offLeaders!(X, 3); offLeaders!(X, 4); offLeaders!(X, 18);

            'Rebounds
            Print #1, Tab(49);
            Print #1, Using "####"; offLeaders!(X, 5);

            'Assists
            Print #1, Tab(54);
            Print #1, Using "####"; offLeaders!(X, 8);

            'Personal Fouls
            Print #1, Tab(59);
            Print #1, Using "####"; offLeaders!(X, 6);

            'Steals
            Print #1, Tab(64);
            Print #1, Using "###"; offLeaders!(X, 7);

            'Blocks
            Print #1, Tab(68);
            Print #1, Using "###"; offLeaders!(X, 9);

            'Points
            Print #1, Tab(72);
            Print #1, Using "#####"; offLeaders!(X, 20);

            'Average
            Print #1, Tab(78);
            Print #1, Using "###.#"; offLeaders!(X, 19)

        End If

    Next

    Print #1,
    If printDest = 1 Then Print #1, Chr$(12)
    Print #1,
    
    Print #1, Tab(32); "DEFENSIVE TOTALS"
    Print #1, Tab(13); "G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG": Color 15
    Color 15

    For X = 1 To TEAMS_PER_LEAGUE

        If defLeaderTeams$(X) <> "" And defLeaderTeams$(X) <> "*" And defLeaderTeams$(X) <> " " Then

            'Player
            Print #1, defLeaderTeams$(X); 

            'Games
            Print #1, Tab(16);
            Print #1, Using "##"; defLeaders!(X, 16);

            'FGA
            Print #1, Tab(19);
            Print #1, Using "####"; defLeaders!(X, 1);

            'FGM
            Print #1, Tab(24);
            Print #1, Using "####"; defLeaders!(X, 2);

            'FG %
            Print #1, Tab(29);
            Print #1, Using ".###"; defLeaders!(X, 17);

            'FTA / FTM / FT%
            Print #1, Tab(34);
            Print #1, Using "#### #### .###"; defLeaders!(X, 3); defLeaders!(X, 4); defLeaders!(X, 18);

            'Rebounds
            Print #1, Tab(49);
            Print #1, Using "####"; defLeaders!(X, 5);

            'Assists
            Print #1, Tab(54);
            Print #1, Using "####"; defLeaders!(X, 8);

            'Personal Fouls
            Print #1, Tab(59);
            Print #1, Using "####"; defLeaders!(X, 6);

            'Steals
            Print #1, Tab(64);
            Print #1, Using "###"; defLeaders!(X, 7);

            'Blocks
            Print #1, Tab(68);
            Print #1, Using "###"; defLeaders!(X, 9);

            'Points
            Print #1, Tab(72);
            Print #1, Using "#####"; defLeaders!(X, 20);

            'Average
            Print #1, Tab(78);
            Print #1, Using "###.#"; defLeaders!(X, 19)

        End If

    Next

    Close #1

    If printDest = 1 Then

        Open destFile$ For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop

        Close #2

        LPrint Chr$(12)

        Kill destFile$

    End If

End Sub
