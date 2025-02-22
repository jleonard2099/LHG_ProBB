'----------------------------------------
'           SEELEAG Subroutine
'----------------------------------------
'This subroutine lets the user choose a
'league stat file to view, then calastLoseStreak
'other routines to both load the Stats
'and then provide options for display
Sub SEELEAG ()

    _Title "Basketball: The Pro Game - View League Stats"

    leaderFile$ = ChooseFile$(diskPaths$(2) + "*.LDR", "LEADER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Call LoadLeaderFiles_Pro(leaderFile$)
        Call StatOptions_Pro

    End If


End Sub


'----------------------------------------
'     LoadLeaderFiles_Pro Subroutine
'----------------------------------------
'This subroutine loads the conference stats
'from the file into several arrays
Sub LoadLeaderFiles_Pro (targetFile$)

    Shared divWins(), divLosses()
    Shared defLeaders!(), offLeaders!()
    Shared compLgStatVal!(), compLgStatAtt!()

    Shared defLeaderTeams$(), offLeaderNames$()
    Shared compLdrTeam$(), leagLdrPlayer$(), leagLdrTeam$()

    Shared leagueName$, div1Name$, div2Name$, div3Name$, div4Name$

    Open diskPaths$(2) + targetFile$ + ".TMS" For Input As #1
    Input #1, leagueName$, div1Name$, div2Name$, div3Name$, div4Name$
    Close #1

    div1Name$ = PadRight$(div1Name$, 8)
    div2Name$ = PadRight$(div2Name$, 8)
    div3Name$ = PadRight$(div3Name$, 8)
    div4Name$ = PadRight$(div4Name$, 8)

    Cls

    Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

    Open diskPaths$(2) + targetFile$ + ".LDR" For Input As #2

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
'       StatOptions_Pro Subroutine
'----------------------------------------
'This subroutine gives the user various
'options for viewing the previously loaded
'stats, and then calastLoseStreak routines for each
Sub StatOptions_Pro ()

    Do

        Cls
        Call TITLE_STATS

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F":
                    userFile$ = _SaveFileDialog$("Save League Report", _CWD$ + "\Report", "*.txt", "Text File")
                    Call SavePrintLeagueLeaders(0, userFile$)

                Case "P":
                    Call SavePrintLeagueLeaders(0, "REPORT")

                Case "S":
                    Call LeaderStats_Screen

            End Select

            Color 15, 0
            Cls

            Call box0(6, 8, 11, 70, 2, 15, 1)

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
'     LeaderStats_Screen Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to the end
'user display.
Sub LeaderStats_Screen ()

    Shared divWins(), divLosses()

    Shared defLeaders!(), offLeaders!()
    Shared compLgStatVal!(), compLgStatAtt!()

    Shared defLeaderTeams$(), offLeaderNames$(), offLeaders!()
    Shared compLdrTeam$(), leagLdrPlayer$(), leagLdrTeam$()

    Shared leagueName$, div1Name$, div2Name$, div3Name$, div4Name$

    Color 15, 0
    Cls

    Color 12
    Locate , Int((80 - Len(leagueName$) - 17) / 2)
    Print leagueName$; " LEAGUE STANDINGS"
    Print

    ' Division 1 Standings
    Color 2
    If div1Name$ <> "" Then
        Print "    " + RTrim$(div1Name$) + " DIVISION"
    End If

    Print

    Color 14
    Print "                   W    L     PCT   GB"
    Color 15

    For P = 1 To 10

        If compLdrTeam$(P) <> "" And compLdrTeam$(P) <> "*" Then

            D1 = divWins(1) - divLosses(1)
            D = divWins(P) - divLosses(P)
            D = D1 - D
            BK$ = "     " + Str$(Int(D / 2))
            If D = 0 Then BK$ = "   ---"
            If D = 1 Then BK$ = "     "
            If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

            Print "    "; compLdrTeam$(P); Tab(16);

            'This is WIN/LOSS PCT; will throw errors if there are 0 games!!!
            PCT! = divWins(P) / (divWins(P) + divLosses(P))

            Print Using "   ##   ##   #.###"; divWins(P); divLosses(P); PCT!;
            Print BK$

        End If

    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Locate , Int((80 - Len(leagueName$) - 17) / 2)
    Print leagueName$; " LEAGUE STANDINGS"
    Print

    ' Division 2 Standings

    Color 2
    If div2Name$ <> "" Then Print "    " + RTrim$(div2Name$) + " DIVISION"
    Print

    Color 14
    Print "                   W    L     PCT   GB"
    Color 15

    For P = 11 To 20

        If compLdrTeam$(P) <> "" And compLdrTeam$(P) <> "*" Then

            D1 = divWins(11) - divLosses(11)
            D = divWins(P) - divLosses(P)
            D = D1 - D
            BK$ = "     " + Str$(Int(D / 2))
            If D = 0 Then BK$ = "   ---"
            If D = 1 Then BK$ = "     "
            If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

            Print "    "; compLdrTeam$(P); Tab(16);

            'This is WIN/LOSS PCT; will throw errors if there are 0 games!!!
            PCT! = divWins(P) / (divWins(P) + divLosses(P))

            Print Using "   ##   ##   #.###"; divWins(P); divLosses(P); PCT!;
            Print BK$

        End If

    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Locate , Int((80 - Len(leagueName$) - 17) / 2)
    Print leagueName$; " LEAGUE STANDINGS"
    Print

    ' Division 3 Standings

    Color 2
    If div3Name$ <> "" Then Print "    " + RTrim$(div3Name$) + " DIVISION"
    Print

    Color 14
    Print "                   W    L     PCT   GB"
    Color 15

    For P = 21 To 30

        If compLdrTeam$(P) <> "" And compLdrTeam$(P) <> "*" Then
            D1 = divWins(21) - divLosses(21)
            D = divWins(P) - divLosses(P)
            D = D1 - D
            BK$ = "     " + Str$(Int(D / 2))
            If D = 0 Then BK$ = "   ---"
            If D = 1 Then BK$ = "     "
            If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

            'This is WIN/LOSS PCT; will throw errors if there are 0 games!!!
            PCT! = divWins(P) / (divWins(P) + divLosses(P))

            Print "    "; compLdrTeam$(P); Tab(16);
            Print Using "   ##   ##   #.###"; divWins(P); divLosses(P); PCT!;
            Print BK$

        End If

    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Locate , Int((80 - Len(leagueName$) - 17) / 2)
    Print leagueName$; " LEAGUE STANDINGS"
    Print

    ' Division 4 Standings

    Color 2
    If div4Name$ <> "" Then Print "    " + RTrim$(div4Name$) + " DIVISION"
    Print

    Color 14
    Print "                   W    L     PCT   GB"
    Color 15

    For P = 31 To 40

        If compLdrTeam$(P) <> "" And compLdrTeam$(P) <> "*" Then
            D1 = divWins(31) - divLosses(31)
            D = divWins(P) - divLosses(P)
            D = D1 - D
            BK$ = "     " + Str$(Int(D / 2))
            If D = 0 Then BK$ = "   ---"
            If D = 1 Then BK$ = "     "
            If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

            'This is WIN/LOSS PCT; will throw errors if there are 0 games!!!
            PCT! = divWins(P) / (divWins(P) + divLosses(P))

            Print "    "; compLdrTeam$(P); Tab(16);
            Print Using "   ##   ##   #.###"; divWins(P); divLosses(P); PCT!;
            Print BK$
        End If
    Next P

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "FIELD GOAL % (MIN 5 FGA/G)"
    Print "                            PCT   FGA"
    Color 15

    For P = 41 To 60

        If leagLdrPlayer$(P) <> "*" And leagLdrPlayer$(P) <> "" Then
            Print Left$(leagLdrPlayer$(P), 10) + ", " + leagLdrTeam$(P);
            Locate , 28
            Print Using "##.#  ####"; compLgStatVal!(P) * 100; compLgStatAtt!(P)
        End If

    Next

    Color 14
    Locate 1, 41
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
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "SCORING"
    Print "                                PTS/G"
    Color 15

    For P = 1 To 20

        If leagLdrPlayer$(P) <> "*" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34: Print Using "###.#"; compLgStatVal!(P)
        End If

    Next

    Color 14
    Locate 1, 41
    Print "TOTAL POINTS"
    Locate , 41
    Print "                                  PTS"
    Color 15

    For P = 21 To 40
        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74: Print Using "####"; compLgStatVal!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "STEALS PER GAME"
    Color 15

    For P = 81 To 90

        If leagLdrPlayer$(P) <> "*" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34: Print Using "###.#"; compLgStatVal!(P)
        End If

    Next

    Locate 1, 41
    Color 14
    Print "STEALS"
    Color 15

    For P = 91 To 100

        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74: Print Using "####"; compLgStatVal!(P)
        End If

    Next

    Locate 13
    Color 14
    Print "ASSISTS PER GAME"
    Color 15

    For P = 101 To 110

        If leagLdrPlayer$(P) <> "*" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34: Print Using "###.#"; compLgStatVal!(P)
        End If

    Next

    Locate 13, 41
    Color 14
    Print "ASSISTS"
    Color 15

    For P = 111 To 120

        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74: Print Using "####"; compLgStatVal!(P)
        End If

    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "THREE FIELD GOAL %"
    Color 15

    For P = 121 To 130

        If leagLdrPlayer$(P) <> "" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34: Print Using "###.#"; compLgStatVal!(P) * 100
        End If

    Next P

    Locate 1, 41
    Color 14
    Print "MINUTES PER GAME"
    Color 15

    For P = 141 To 150

        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74: Print Using "###.#"; compLgStatVal!(P)
        End If

    Next P

    Locate 13
    Color 14
    Print "THREE POINT FIELD GOALS MADE"
    Color 15

    For P = 131 To 140

        If leagLdrPlayer$(P) <> "" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34: Print Using "####"; compLgStatVal!(P)
        End If

    Next

    Locate 13, 41
    Color 14
    Print "TOTAL MINUTES"
    Color 15

    For P = 151 To 160

        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74: Print Using "####"; compLgStatVal!(P)
        End If

    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "REBOUNDS PER GAME   "
    Color 15

    For P = 161 To 170

        If leagLdrPlayer$(P) <> "*" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34: Print Using "###.#"; compLgStatVal!(P)
        End If

    Next

    Locate 1, 41
    Color 14
    Print "REBOUNDS"
    Color 15

    For P = 171 To 180

        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74: Print Using "####"; compLgStatVal!(P)
        End If

    Next

    Locate 13
    Color 14
    Print "BLOCKS PER GAME"
    Color 15

    For P = 181 To 190

        If leagLdrPlayer$(P) <> "*" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34: Print Using "###.#"; compLgStatVal!(P)
        End If

    Next

    Locate 13, 41
    Color 14
    Print "BLOCKS"
    Color 15

    For P = 191 To 200

        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74: Print Using "####"; compLgStatVal!(P)
        End If

    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls
    Color 14
    Print "FIELD GOALS MADE "
    Color 15

    For P = 201 To 210

        If leagLdrPlayer$(P) <> "*" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34: Print Using "####"; compLgStatVal!(P)
        End If

    Next

    Locate 1, 41
    Color 14
    Print "FREE THROWS MADE"
    Color 15

    For P = 211 To 220

        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74: Print Using "####"; compLgStatVal!(P)
        End If

    Next

    Locate 13
    Color 14
    Print "FOULS PER GAME"
    Color 15

    For P = 221 To 230

        If leagLdrPlayer$(P) <> "*" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34: Print Using "###.#"; compLgStatVal!(P)
        End If

    Next

    Locate 13, 41
    Color 14
    Print "FOULS"
    Color 15

    For P = 231 To 240

        If leagLdrPlayer$(P) <> "*" Then
            Locate , 41: Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 74: Print Using "####"; compLgStatVal!(P)
        End If

    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "MOST VALUABLE PLAYER POINTS"
    Color 15

    For P = 241 To 250

        If leagLdrPlayer$(P) <> "*" Then
            Print leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Locate , 34: Print Using "####"; compLgStatVal!(P)
        End If

    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Print Tab(32); "OFFENSIVE TOTALS"
    Color 14
    Print
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
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Print Tab(27); "OFFENSIVE TOTALS (cont'd)"
    Color 14
    Print
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
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Print Tab(32); "DEFENSIVE TOTALS"
    Color 14
    Print
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
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 12
    Print Tab(27); "DEFENSIVE TOTALS (cont'd)"
    Color 14
    Print
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
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

End Sub


'----------------------------------------
'    SavePrintLeagueLeaders Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to a file.
'This file can then be looped through to
'send to the default printer

'Destination
'   0 = File
'   1 = Printer
Sub SavePrintLeagueLeaders (printDest, destFile$)

    Shared divWins(), divLosses()
    Shared defLeaders!(), offLeaders!()
    Shared compLgStatVal!(), compLgStatAtt!()

    Shared offLeaderNames$(), defLeaderTeams$(), compLdrTeam$()
    Shared leagLdrPlayer$(), leagLdrTeam$()

    Shared leagueName$, div1Name$, div2Name$, div3Name$, div4Name$

    Color 0, 15
    Cls

    Open destFile$ For Output As #1

    Print #1, "                        "; leagueName$; " LEAGUE STANDINGS"
    Print #1,
    Print #1, "    "; div1Name$; " DIVISION"
    Print #1,
    Print #1, "                   W    L     PCT   GB"

    For P = 1 To 10

        If compLdrTeam$(P) <> "" And compLdrTeam$(P) <> "*" Then
            D1 = divWins(1) - divLosses(1)
            D = divWins(P) - divLosses(P)
            D = D1 - D
            BK$ = "   " + Str$(Int(D / 2))
            If D = 0 Then BK$ = "   ---"
            If D = 1 Then BK$ = "     "
            If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

            PCT! = divWins(P) / (divWins(P) + divLosses(P))

            Print #1, "    "; compLdrTeam$(P); Tab(16);
            Print #1, Using "   ##   ##   #.###"; divWins(P); divLosses(P); PCT!;
            Print #1, BK$
        End If

    Next P

    Print #1,
    Print #1, "    "; div2Name$; " DIVISION"
    Print #1,
    Print #1, "                   W    L     PCT   GB"

    For P = 11 To 20

        If compLdrTeam$(P) <> "" And compLdrTeam$(P) <> "*" Then
            D1 = divWins(11) - divLosses(11)
            D = divWins(P) - divLosses(P)
            D = D1 - D
            BK$ = "   " + Str$(Int(D / 2))
            If D = 0 Then BK$ = "   ---"
            If D = 1 Then BK$ = "     "
            If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

            PCT! = divWins(P) / (divWins(P) + divLosses(P))

            Print #1, "    "; compLdrTeam$(P); Tab(16);
            Print #1, Using "   ##   ##   #.###"; divWins(P); divLosses(P); PCT!;
            Print #1, BK$
        End If

    Next P

    Print #1,
    Print #1, "    "; div3Name$; " DIVISION"
    Print #1,
    Print #1, "                   W    L     PCT   GB"

    For P = 21 To 30

        If compLdrTeam$(P) <> "" And compLdrTeam$(P) <> "*" Then
            D1 = divWins(21) - divLosses(21)
            D = divWins(P) - divLosses(P)
            D = D1 - D
            BK$ = "   " + Str$(Int(D / 2))
            If D = 0 Then BK$ = "   ---"
            If D = 1 Then BK$ = "     "
            If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

            PCT! = divWins(P) / (divWins(P) + divLosses(P))

            Print #1, "    "; compLdrTeam$(P); Tab(16);
            Print #1, Using "   ##   ##   #.###"; divWins(P); divLosses(P); PCT!;
            Print #1, BK$

        End If

    Next P

    Print #1,
    Print #1, "    "; div4Name$; " DIVISION"
    Print #1,
    Print #1, "                   W    L     PCT   GB"

    For P = 31 To 40

        If compLdrTeam$(P) <> "" And compLdrTeam$(P) <> "*" Then
            D1 = divWins(31) - divLosses(31)
            D = divWins(P) - divLosses(P)
            D = D1 - D
            BK$ = "   " + Str$(Int(D / 2))
            If D = 0 Then BK$ = "   ---"
            If D = 1 Then BK$ = "     "
            If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

            PCT! = divWins(P) / (divWins(P) + divLosses(P))

            Print #1, "    "; compLdrTeam$(P); Tab(16);
            Print #1, Using "   ##   ##   #.###"; divWins(P); divLosses(P); PCT!;
            Print #1, BK$
        End If

    Next P

    Print #1, Chr$(12)
    Print #1, "FIELD GOAL % (MIN 5 FGA/G)"; Tab(41); "FREE THROW % (MIN 2 FTA/G)"
    Print #1,
    Print #1, "                            PCT   FGA"; Tab(41); "                            PCT   FTA"

    For P = 41 To 60

        If leagLdrPlayer$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, Left$(leagLdrPlayer$(P), 10) + ", " + leagLdrTeam$(P);
            Print #1, Tab(28);
            Print #1, Using "##.#  ####"; compLgStatVal!(P) * 100; compLgStatAtt!(P);
            Print #1, Tab(41);
        End If

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

        If leagLdrPlayer$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "###.#"; compLgStatVal!(P);
            Print #1, Tab(41);
        End If

        If leagLdrPlayer$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, leagLdrPlayer$(P + 20) + ", " + leagLdrTeam$(P + 20);
            Print #1, Tab(74);
            Print #1, Using "####"; compLgStatVal!(P + 20)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "STEALS PER GAME"; Tab(41); "STEALS"

    For P = 81 To 90

        If leagLdrPlayer$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "###.#"; compLgStatVal!(P);
            Print #1, Tab(41);
        End If

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

        If leagLdrPlayer$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "###.#"; compLgStatVal!(P);
            Print #1, Tab(41);
        End If

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

        If leagLdrPlayer$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "###.#"; compLgStatVal!(P) * 100;
            Print #1, Tab(41);
        End If

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

        If leagLdrPlayer$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "####"; compLgStatVal!(P);
            Print #1, Tab(41);
        End If

        If leagLdrPlayer$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, leagLdrPlayer$(P + 20) + ", " + leagLdrTeam$(P + 20);
            Print #1, Tab(74);
            Print #1, Using "####"; compLgStatVal!(P + 20)
        End If

    Next

    Print #1,
    Print #1, Chr$(12)
    Print #1, "REBOUNDS PER GAME"; Tab(41); "REBOUNDS"

    For P = 161 To 170

        If leagLdrPlayer$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "###.#"; compLgStatVal!(P);
            Print #1, Tab(41);
        End If

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

        If leagLdrPlayer$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "###.#"; compLgStatVal!(P);
            Print #1, Tab(41);
        End If

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

        If leagLdrPlayer$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "####"; compLgStatVal!(P);
            Print #1, Tab(41);
        End If

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

        If leagLdrPlayer$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, leagLdrPlayer$(P) + ", " + leagLdrTeam$(P);
            Print #1, Tab(34);
            Print #1, Using "###.#"; compLgStatVal!(P);
            Print #1, Tab(41);
        End If

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
    Print #1, Chr$(12)
    Print #1, Tab(32); "OFFENSIVE TOTALS"
    Print #1, Tab(13); "G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG": Color 15

    For X = 1 To TEAMS_PER_LEAGUE

        If offLeaderNames$(X) <> "" And offLeaderNames$(X) <> "*" And offLeaderNames$(X) <> " " Then

            'Player
            Print #1, Left$(offLeaderNames$(X), 11); Tab(13);

            'Games
            Print #1, Using "##"; offLeaders!(X, 16);
            Print #1, Tab(16);

            'FGA
            Print #1, Using "####"; offLeaders!(X, 1);
            Print #1, Tab(21);

            'FGM
            Print #1, Using "####"; offLeaders!(X, 2);
            Print #1, Tab(26);

            'FG %
            Print #1, Using ".###"; offLeaders!(X, 17);
            Print #1, Tab(31);

            'FTA / FTM / FT%
            Print #1, Using "#### #### .###"; offLeaders!(X, 3); offLeaders!(X, 4); offLeaders!(X, 18);
            Print #1, Tab(46);

            'Rebounds
            Print #1, Using "####"; offLeaders!(X, 5);
            Print #1, Tab(51);

            'Assists
            Print #1, Using "####"; offLeaders!(X, 8);
            Print #1, Tab(56);

            'Personal Fouls
            Print #1, Using "####"; offLeaders!(X, 6);
            Print #1, Tab(61);

            'Steals
            Print #1, Using "###"; offLeaders!(X, 7);
            Print #1, Tab(65);

            'Blocks
            Print #1, Using "###"; offLeaders!(X, 9);
            Print #1, Tab(69);

            'Points
            Print #1, Using "#####"; offLeaders!(X, 20);
            Print #1, Tab(75);

            'Average
            Print #1, Using "###.#"; offLeaders!(X, 19)

        End If

    Next

    Print #1, Chr$(12)
    Print #1, Tab(32); "DEFENSIVE TOTALS"
    Print #1, Tab(13); "G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG": Color 15
    Color 15

    For X = 1 To TEAMS_PER_LEAGUE

        If defLeaderTeams$(X) <> "" And defLeaderTeams$(X) <> "*" And defLeaderTeams$(X) <> " " Then

            'Player
            Print #1, Left$(defLeaderTeams$(X), 11); Tab(13);

            'Games
            Print #1, Using "##"; defLeaders!(X, 16);
            Print #1, Tab(16);

            'FGA
            Print #1, Using "####"; defLeaders!(X, 1);
            Print #1, Tab(21);

            'FGM
            Print #1, Using "####"; defLeaders!(X, 2);
            Print #1, Tab(26);

            'FG %
            Print #1, Using ".###"; defLeaders!(X, 17);
            Print #1, Tab(31);

            'FTA / FTM / FT%
            Print #1, Using "#### #### .###"; defLeaders!(X, 3); defLeaders!(X, 4); defLeaders!(X, 18);
            Print #1, Tab(46);

            'Rebounds
            Print #1, Using "####"; defLeaders!(X, 5);
            Print #1, Tab(51);

            'Assists
            Print #1, Using "####"; defLeaders!(X, 8);
            Print #1, Tab(56);

            'Personal Fouls
            Print #1, Using "####"; defLeaders!(X, 6);
            Print #1, Tab(61);

            'Steals
            Print #1, Using "###"; defLeaders!(X, 7);
            Print #1, Tab(65);

            'Blocks
            Print #1, Using "###"; defLeaders!(X, 9);
            Print #1, Tab(69);

            'Points
            Print #1, Using "#####"; defLeaders!(X, 20);
            Print #1, Tab(75);

            'Average
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
