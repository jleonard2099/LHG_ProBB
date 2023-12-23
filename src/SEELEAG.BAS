'----------------------------------------
'           SEELEAG Subroutine
'----------------------------------------
'This subroutine lets the user choose a
'league stat file to view, then calls
'other routines to both load the Stats
'and then provide options for display
Sub SEELEAG ()

    _Title "Basketball: The Pro Game - View League Stats"

    leaderFile$ = ChooseFile$(diskPaths$(1) + "*.LDR", "LEADER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Call LoadLeaderFiles_Pro(leaderFile$)
        Call StatOptions

    End If


End Sub


'----------------------------------------
'     LoadLeaderFiles_Pro Subroutine
'----------------------------------------
'This subroutine loads the conference stats
'from the file into several arrays
Sub LoadLeaderFiles_Pro (targetFile$)

    Shared O1!(), O2!()
    Shared DL!(), OL!()
    Shared seeZ!(), seeZ1!()

    Shared DL$(), OL$()
    Shared leagT$(), Z2$(), Z3$()

    Shared LG$, div1$, div2$, div3$, div4$

    Open diskPaths$(1) + targetFile$ + ".TMS" For Input As #1

    Input #1, LG$, div1$, div2$, div3$, div4$

    Close #1

    div1$ = PadRight$(div1$, 8)
    div2$ = PadRight$(div2$, 8)
    div3$ = PadRight$(div3$, 8)
    div4$ = PadRight$(div4$, 8)

    Cls

    Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

    Open diskPaths$(1) + targetFile$ + ".LDR" For Input As #2

    For X = 1 To TEAMS_PER_CONFERENCE
        Input #2, leagT$(X), O1!(X), O2!(X)
    Next

    For X = 1 To 250
        Input #2, Z2$(X), Z3$(X), seeZ!(X), seeZ1!(X)
    Next

    For X = 1 To TEAMS_PER_CONFERENCE
        Input #2, OL$(X)
        For M = 1 To 20
            Input #2, OL!(X, M)
        Next
    Next

    For X = 1 To TEAMS_PER_CONFERENCE
        Input #2, DL$(X)
        For M = 1 To 20
            Input #2, DL!(X, M)
        Next
    Next

    Close #2


End Sub


'----------------------------------------
'          StatOptions Subroutine
'----------------------------------------
'This subroutine gives the user various
'options for viewing the previously loaded
'stats, and then calls routines for each
Sub StatOptions ()

    Do

        Cls
        Call TITLE_STATS

        Call MenuBox(8, 8, 61, 5, 15, 1)

        Do

            Locate 10, 15
            Color 7, 1

            Locate , 30: Print "STATS TO ";
            Color 15: Print "S";: Color 7: Print "CREEN"

            Locate , 30: Print "STATS TO ";
            Color 15: Print "P";: Color 7: Print "RINTER"

            Locate , 30: Print "STATS TO ";
            Color 15: Print "F";: Color 7: Print "ILE"

            I$ = GetKeyPress$
            Color 14, 0

        Loop Until UCase$(I$) = "S" Or UCase$(I$) = "P" Or UCase$(I$) = "F" Or I$ = Chr$(27)

        If I$ <> Chr$(27) Then

            Select Case UCase$(I$)

                Case "F":
                    Call ReportFilePrompt_League(SF$)
                    Call SavePrintLeagueLeaders(SF$, 1)

                Case "P":
                    Call SavePrintLeagueLeaders(SF$, 0)

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

    Loop Until UCase$(A$) = "N" Or A$ = Chr$(27)

End Sub


'----------------------------------------
'   ReportFilePrompt_League Subroutine
'----------------------------------------
'This subroutine prompts the user for
'the name of the report file to be saved
Sub ReportFilePrompt_League (reportFile$)

    Do

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 1)

        Locate 7, 32

        Locate , 12: Print "ENTER NAME FOR FILE: ": Print
        Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS"
        Locate , 12: Print "DO NOT ADD A FILE EXTENSION; THE PROGRAM"
        Locate , 12: Print "WILL AUTOMATICALLY ADD .RPT"

        Locate 7, 37: Input reportFile$

        If reportFile$ <> "" Then

            Locate 13, 12: Print "ANY CHANGE? (Y/N) "
            I$ = GetKeyPress$

        End If

    Loop Until UCase$(I$) = "N" And reportFile$ <> ""

    Color 15, 0

End Sub


'----------------------------------------
'     LeaderStats_Screen Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to the end
'user display.
Sub LeaderStats_Screen ()

    Shared O1!(), O2!()

    Shared DL!(), OL!()
    Shared seeZ!(), seeZ1!()

    Shared DL$(), OL$(), OL!()
    Shared leagT$(), Z2$(), Z3$()

    Shared LG$, div1$, div2$, div3$, div4$

    Color 15, 0
    Cls

    Color 12
    Locate , Int((80 - Len(LG$) - 17) / 2)
    Print LG$; " LEAGUE STANDINGS"
    Print

    ' Division 1 Standings
    Color 2
    If div1$ <> "" Then Print "    " + RTrim$(div1$) + " DIVISION"
    Print

    Color 14
    Print "                   W    L     PCT   GB"
    Color 15

    For P = 1 To 10

        If leagT$(P) <> "" And leagT$(P) <> "*" Then

            D1 = O1!(1) - O2!(1)
            D = O1!(P) - O2!(P)
            D = D1 - D
            BK$ = "     " + Str$(Int(D / 2))
            If D = 0 Then BK$ = "   ---"
            If D = 1 Then BK$ = "     "
            If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

            Print "    "; leagT$(P); Tab(16);

            'This is WIN/LOSS PCT; will throw errors if there are 0 games!!!
            PCT! = O1!(P) / (O1!(P) + O2!(P))

            Print Using "   ##   ##   #.###"; O1!(P); O2!(P); PCT!;
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
    Locate , Int((80 - Len(LG$) - 17) / 2)
    Print LG$; " LEAGUE STANDINGS"
    Print

    ' Division 2 Standings

    Color 2
    If div2$ <> "" Then Print "    " + RTrim$(div2$) + " DIVISION"
    Print

    Color 14
    Print "                   W    L     PCT   GB"
    Color 15

    For P = 11 To 20

        If leagT$(P) <> "" And leagT$(P) <> "*" Then

            D1 = O1!(11) - O2!(11)
            D = O1!(P) - O2!(P)
            D = D1 - D
            BK$ = "     " + Str$(Int(D / 2))
            If D = 0 Then BK$ = "   ---"
            If D = 1 Then BK$ = "     "
            If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

            Print "    "; leagT$(P); Tab(16);

            'This is WIN/LOSS PCT; will throw errors if there are 0 games!!!
            PCT! = O1!(P) / (O1!(P) + O2!(P))

            Print Using "   ##   ##   #.###"; O1!(P); O2!(P); PCT!;
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
    Locate , Int((80 - Len(LG$) - 17) / 2)
    Print LG$; " LEAGUE STANDINGS"
    Print

    ' Division 3 Standings

    Color 2
    If div3$ <> "" Then Print "    " + RTrim$(div3$) + " DIVISION"
    Print

    Color 14
    Print "                   W    L     PCT   GB"
    Color 15

    For P = 21 To 30

        If leagT$(P) <> "" And leagT$(P) <> "*" Then
            D1 = O1!(21) - O2!(21)
            D = O1!(P) - O2!(P)
            D = D1 - D
            BK$ = "     " + Str$(Int(D / 2))
            If D = 0 Then BK$ = "   ---"
            If D = 1 Then BK$ = "     "
            If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

            'This is WIN/LOSS PCT; will throw errors if there are 0 games!!!
            PCT! = O1!(P) / (O1!(P) + O2!(P))

            Print "    "; leagT$(P); Tab(16);
            Print Using "   ##   ##   #.###"; O1!(P); O2!(P); PCT!;
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
    Locate , Int((80 - Len(LG$) - 17) / 2)
    Print LG$; " LEAGUE STANDINGS"
    Print

    ' Division 4 Standings

    Color 2
    If div4$ <> "" Then Print "    " + RTrim$(div4$) + " DIVISION"
    Print

    Color 14
    Print "                   W    L     PCT   GB"
    Color 15

    For P = 31 To 40

        If leagT$(P) <> "" And leagT$(P) <> "*" Then
            D1 = O1!(31) - O2!(31)
            D = O1!(P) - O2!(P)
            D = D1 - D
            BK$ = "     " + Str$(Int(D / 2))
            If D = 0 Then BK$ = "   ---"
            If D = 1 Then BK$ = "     "
            If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

            'This is WIN/LOSS PCT; will throw errors if there are 0 games!!!
            PCT! = O1!(P) / (O1!(P) + O2!(P))

            Print "    "; leagT$(P); Tab(16);
            Print Using "   ##   ##   #.###"; O1!(P); O2!(P); PCT!;
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

        If Z2$(P) <> "*" And Z2$(P) <> "" Then
            Print Left$(Z2$(P), 10) + ", " + Z3$(P);
            Locate , 28
            Print Using "##.#  ####"; seeZ!(P) * 100; seeZ1!(P)
        End If

    Next

    Color 14
    Locate 1, 41
    Print "FREE THROW % (MIN 2 FTA/G)"
    Locate , 41
    Print "                            PCT   FTA"
    Color 15

    For P = 61 To 80

        If Z2$(P) <> "*" And Z2$(P) <> "" Then
            Locate , 41
            Print Left$(Z2$(P), 10) + ", " + Z3$(P);
            Locate , 68
            Print Using "##.#  ####"; seeZ!(P) * 100; seeZ1!(P)
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

        If Z2$(P) <> "*" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "###.#"; seeZ!(P)
        End If

    Next

    Color 14
    Locate 1, 41
    Print "TOTAL POINTS"
    Locate , 41
    Print "                                  PTS"
    Color 15

    For P = 21 To 40
        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "####"; seeZ!(P)
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

        If Z2$(P) <> "*" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "###.#"; seeZ!(P)
        End If

    Next

    Locate 1, 41
    Color 14
    Print "STEALS"
    Color 15

    For P = 91 To 100

        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "####"; seeZ!(P)
        End If

    Next

    Locate 13
    Color 14
    Print "ASSISTS PER GAME"
    Color 15

    For P = 101 To 110

        If Z2$(P) <> "*" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "###.#"; seeZ!(P)
        End If

    Next

    Locate 13, 41
    Color 14
    Print "ASSISTS"
    Color 15

    For P = 111 To 120

        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "####"; seeZ!(P)
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

        If Z2$(P) <> "" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "###.#"; seeZ!(P) * 100
        End If

    Next P

    Locate 1, 41
    Color 14
    Print "MINUTES PER GAME"
    Color 15

    For P = 141 To 150

        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "###.#"; seeZ!(P)
        End If

    Next P

    Locate 13
    Color 14
    Print "THREE POINT FIELD GOALS MADE"
    Color 15

    For P = 131 To 140

        If Z2$(P) <> "" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "####"; seeZ!(P)
        End If

    Next

    Locate 13, 41
    Color 14
    Print "TOTAL MINUTES"
    Color 15

    For P = 151 To 160

        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "####"; seeZ!(P)
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

        If Z2$(P) <> "*" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "###.#"; seeZ!(P)
        End If

    Next

    Locate 1, 41
    Color 14
    Print "REBOUNDS"
    Color 15

    For P = 171 To 180

        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "####"; seeZ!(P)
        End If

    Next

    Locate 13
    Color 14
    Print "BLOCKS PER GAME"
    Color 15

    For P = 181 To 190

        If Z2$(P) <> "*" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "###.#"; seeZ!(P)
        End If

    Next

    Locate 13, 41
    Color 14
    Print "BLOCKS"
    Color 15

    For P = 191 To 200

        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "####"; seeZ!(P)
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

        If Z2$(P) <> "*" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "####"; seeZ!(P)
        End If

    Next

    Locate 1, 41
    Color 14
    Print "FREE THROWS MADE"
    Color 15

    For P = 211 To 220

        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "####"; seeZ!(P)
        End If

    Next

    Locate 13
    Color 14
    Print "FOULS PER GAME"
    Color 15

    For P = 221 To 230

        If Z2$(P) <> "*" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "###.#"; seeZ!(P)
        End If

    Next

    Locate 13, 41
    Color 14
    Print "FOULS"
    Color 15

    For P = 231 To 240

        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "####"; seeZ!(P)
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

        If Z2$(P) <> "*" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "####"; seeZ!(P)
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
    Print "                         OFFENSIVE TOTALS"
    Color 14
    Print "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 1 To 20

        If Not (OL$(X) = "" Or OL$(X) = "*" Or OL$(X) = " ") Then
            'Team
            Print Left$(OL$(X), 11); Tab(13);
            'G
            Print Using "##"; OL!(X, 16);: Print Tab(16);
            'FGA
            Print Using "#####"; OL!(X, 2);: Print Tab(21);
            'FGM
            Print Using "#####"; OL!(X, 3);: Print Tab(26);
            'FG PCT
            Print Using ".###"; OL!(X, 17);: Print Tab(31);
            'FTA / FTM / FT PCT
            Print Using "#### #### .###"; OL!(X, 4); OL!(X, 5); OL!(X, 18);: Print Tab(46);
            'REB
            Print Using "####"; OL!(X, 6);: Print Tab(51);
            'AST
            Print Using "####"; OL!(X, 9);: Print Tab(56);
            'Personal Fouls
            Print Using "####"; OL!(X, 7);: Print Tab(61);
            'STL
            Print Using "###"; OL!(X, 8);: Print Tab(65);
            'BLK
            Print Using "###"; OL!(X, 10);: Print Tab(69);
            'PTS
            Print Using "#####"; OL!(X, 20);: Print Tab(75);
            'AVG
            Print Using "###.#"; OL!(X, 19)

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
    Print "                      OFFENSIVE TOTALS (cont)"
    Color 14
    Print "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 21 To 40

        If OL$(X) <> "" And OL$(X) <> "*" And OL$(X) <> " " Then
            Print Left$(OL$(X), 11); Tab(13);
            'G
            Print Using "##"; OL!(X, 16);: Print Tab(16);
            'FGA
            Print Using "#####"; OL!(X, 2);: Print Tab(21);
            'FGM
            Print Using "#####"; OL!(X, 3);: Print Tab(26);
            'FG PCT
            Print Using ".###"; OL!(X, 17);: Print Tab(31);
            'FTA / FTM / FT PCT
            Print Using "#### #### .###"; OL!(X, 4); OL!(X, 5); OL!(X, 18);: Print Tab(46);
            'REB
            Print Using "####"; OL!(X, 6);: Print Tab(51);
            'AST
            Print Using "####"; OL!(X, 9);: Print Tab(56);
            'Personal Fouls
            Print Using "####"; OL!(X, 7);: Print Tab(61);
            'STL
            Print Using "###"; OL!(X, 8);: Print Tab(65);
            'BLK
            Print Using "###"; OL!(X, 10);: Print Tab(69);
            'PTS
            Print Using "#####"; OL!(X, 20);: Print Tab(75);
            'AVG
            Print Using "###.#"; OL!(X, 19)
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
    Print "                         DEFENSIVE TOTALS"
    Color 14
    Print "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 1 To 20

        If DL$(X) <> "" And DL$(X) <> "*" And DL$(X) <> " " Then
            'Team
            Print Left$(DL$(X), 11); Tab(13);
            'Games
            Print Using "##"; DL!(X, 16);: Print Tab(16);
            'FGA
            Print Using "####"; DL!(X, 2);: Print Tab(21);
            'FGM
            Print Using "####"; DL!(X, 3);: Print Tab(26);
            'FG PCT
            Print Using ".###"; DL!(X, 17);: Print Tab(31);
            'FTA / FTM / FT PCT
            Print Using "#### #### .###"; DL!(X, 4); DL!(X, 5); DL!(X, 18);: Print Tab(46);
            'REB / AST
            Print Using "#### ####"; DL!(X, 6); DL!(X, 9);: Print Tab(56);
            'Fouls
            Print Using "####"; DL!(X, 7);: Print Tab(61);
            'STL
            Print Using "###"; DL!(X, 8);: Print Tab(65);
            'BLK
            Print Using "###"; DL!(X, 10);: Print Tab(69);
            'PTS
            Print Using "#####"; DL!(X, 20);: Print Tab(75);
            'AVG
            Print Using "###.#"; DL!(X, 19)

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
    Print "                      DEFENSIVE TOTALS (cont)"
    Color 14
    Print "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 21 To 40

        If DL$(X) <> "" And DL$(X) <> "*" And DL$(X) <> " " Then
            Print Left$(DL$(X), 11); Tab(13);: Print Using "##"; DL!(X, 16);
            Print Tab(16);: Print Using "####"; DL!(X, 2);
            Print Tab(21);: Print Using "####"; DL!(X, 3);
            Print Tab(26);: Print Using ".###"; DL!(X, 17);
            Print Tab(31);: Print Using "#### #### .###"; DL!(X, 4); DL!(X, 5); DL!(X, 18);
            Print Tab(46);: Print Using "#### ####"; DL!(X, 6); DL!(X, 9);
            Print Tab(56);: Print Using "####"; DL!(X, 7);
            Print Tab(61);: Print Using "###"; DL!(X, 8);
            Print Tab(65);: Print Using "###"; DL!(X, 10);
            Print Tab(69);: Print Using "#####"; DL!(X, 20);
            Print Tab(75);: Print Using "###.#"; DL!(X, 19)
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
'   1 = File
'   !1 = Printer (creates file and prints it)
Sub SavePrintLeagueLeaders (statFile$, destination)

    Shared O1!(), O2!()
    Shared DL!(), OL!()
    Shared seeZ!(), seeZ1!()

    Shared OL$(), DL$(), leagT$()
    Shared Z2$(), Z3$()

    Shared LG$, div1$, div2$, div3$, div4$

    If destination <> 1 Then
        Open "REPORT" For Output As #1
    Else
        Open statFile$ + ".RPT" For Output As #1
    End If

    Color 0, 15
    Cls

    Print #1, "                        "; LG$; " LEAGUE STANDINGS"
    Print #1,
    Print #1, "    "; div1$; " DIVISION"
    Print #1,
    Print #1, "                   W    L     PCT   GB"

    For P = 1 To 10

        If leagT$(P) <> "" And leagT$(P) <> "*" Then
            D1 = O1!(1) - O2!(1)
            D = O1!(P) - O2!(P)
            D = D1 - D
            BK$ = "   " + Str$(Int(D / 2))
            If D = 0 Then BK$ = "   ---"
            If D = 1 Then BK$ = "     "
            If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

            PCT! = O1!(P) / (O1!(P) + O2!(P))

            Print #1, "    "; leagT$(P); Tab(16);
            Print #1, Using "   ##   ##   #.###"; O1!(P); O2!(P); PCT!;
            Print #1, BK$
        End If

    Next P

    Print #1,
    Print #1, "    "; div2$; " DIVISION"
    Print #1,
    Print #1, "                   W    L     PCT   GB"

    For P = 11 To 20

        If leagT$(P) <> "" And leagT$(P) <> "*" Then
            D1 = O1!(11) - O2!(11)
            D = O1!(P) - O2!(P)
            D = D1 - D
            BK$ = "   " + Str$(Int(D / 2))
            If D = 0 Then BK$ = "   ---"
            If D = 1 Then BK$ = "     "
            If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

            PCT! = O1!(P) / (O1!(P) + O2!(P))

            Print #1, "    "; leagT$(P); Tab(16);
            Print #1, Using "   ##   ##   #.###"; O1!(P); O2!(P); PCT!;
            Print #1, BK$
        End If

    Next P

    Print #1,
    Print #1, "    "; div3$; " DIVISION"
    Print #1,
    Print #1, "                   W    L     PCT   GB"

    For P = 21 To 30

        If leagT$(P) <> "" And leagT$(P) <> "*" Then
            D1 = O1!(21) - O2!(21)
            D = O1!(P) - O2!(P)
            D = D1 - D
            BK$ = "   " + Str$(Int(D / 2))
            If D = 0 Then BK$ = "   ---"
            If D = 1 Then BK$ = "     "
            If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

            PCT! = O1!(P) / (O1!(P) + O2!(P))

            Print #1, "    "; leagT$(P); Tab(16);
            Print #1, Using "   ##   ##   #.###"; O1!(P); O2!(P); PCT!;
            Print #1, BK$

        End If

    Next P

    Print #1,
    Print #1, "    "; div4$; " DIVISION"
    Print #1,
    Print #1, "                   W    L     PCT   GB"

    For P = 31 To 40

        If leagT$(P) <> "" And leagT$(P) <> "*" Then
            D1 = O1!(31) - O2!(31)
            D = O1!(P) - O2!(P)
            D = D1 - D
            BK$ = "   " + Str$(Int(D / 2))
            If D = 0 Then BK$ = "   ---"
            If D = 1 Then BK$ = "     "
            If D > 19 Then L = Len(BK$): BK$ = Right$(BK$, L - 1)
            If D / 2 <> Int(D / 2) Then BK$ = BK$ + H$

            PCT! = O1!(P) / (O1!(P) + O2!(P))

            Print #1, "    "; leagT$(P); Tab(16);
            Print #1, Using "   ##   ##   #.###"; O1!(P); O2!(P); PCT!;
            Print #1, BK$
        End If

    Next P

    Print #1, Chr$(12)
    Print #1, "FIELD GOAL % (MIN 5 FGA/G)"; Tab(41); "FREE THROW % (MIN 2 FTA/G)"
    Print #1,
    Print #1, "                            PCT   FGA"; Tab(41); "                            PCT   FTA"

    For P = 41 To 60

        If Z2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, Left$(Z2$(P), 10) + ", " + Z3$(P);
            Print #1, Tab(28);
            Print #1, Using "##.#  ####"; seeZ!(P) * 100; seeZ1!(P);
            Print #1, Tab(41);
        End If

        If Z2$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, Left$(Z2$(P + 20), 10) + ", " + Z3$(P + 20);
            Print #1, Tab(68);
            Print #1, Using "##.#  ####"; seeZ!(P + 20) * 100; seeZ1!(P)
        End If

    Next

    Print #1,
    Print #1, "SCORING"; Tab(41); "TOTAL POINTS"

    For P = 1 To 20

        If Z2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, Z2$(P) + ", " + Z3$(P);
            Print #1, Tab(34);
            Print #1, Using "###.#"; seeZ!(P);
            Print #1, Tab(41);
        End If

        If Z2$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, Z2$(P + 20) + ", " + Z3$(P + 20);
            Print #1, Tab(74);
            Print #1, Using "####"; seeZ!(P + 20)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "STEALS PER GAME"; Tab(41); "STEALS"

    For P = 81 To 90

        If Z2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, Z2$(P) + ", " + Z3$(P);
            Print #1, Tab(34);
            Print #1, Using "###.#"; seeZ!(P);
            Print #1, Tab(41);
        End If

        If Z2$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, Z2$(P + 10) + ", " + Z3$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; seeZ!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "ASSISTS PER GAME"; Tab(41); "ASSISTS"

    For P = 101 To 110

        If Z2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, Z2$(P) + ", " + Z3$(P);
            Print #1, Tab(34);
            Print #1, Using "###.#"; seeZ!(P);
            Print #1, Tab(41);
        End If

        If Z2$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, Z2$(P + 10) + ", " + Z3$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; seeZ!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "THREE FIELD GOAL %"; Tab(41); "MINUTES PER GAME"

    For P = 121 To 130

        If Z2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, Z2$(P) + ", " + Z3$(P);
            Print #1, Tab(34);
            Print #1, Using "###.#"; seeZ!(P) * 100;
            Print #1, Tab(41);
        End If

        If Z2$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, Z2$(P + 20) + ", " + Z3$(P + 20);
            Print #1, Tab(74);
            Print #1, Using "###.#"; seeZ!(P + 20)
        End If

    Next

    Print #1,
    Print #1, "THREE POINT FIELD GOALS MADE"; Tab(41); "MINUTES"

    For P = 131 To 140

        If Z2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, Z2$(P) + ", " + Z3$(P);
            Print #1, Tab(34);
            Print #1, Using "####"; seeZ!(P);
            Print #1, Tab(41);
        End If

        If Z2$(P + 20) = "" Then
            Print #1,
        Else
            Print #1, Z2$(P + 20) + ", " + Z3$(P + 20);
            Print #1, Tab(74);
            Print #1, Using "####"; seeZ!(P + 20)
        End If

    Next

    Print #1,
    Print #1, Chr$(12)
    Print #1, "REBOUNDS PER GAME"; Tab(41); "REBOUNDS"

    For P = 161 To 170

        If Z2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, Z2$(P) + ", " + Z3$(P);
            Print #1, Tab(34);
            Print #1, Using "###.#"; seeZ!(P);
            Print #1, Tab(41);
        End If

        If Z2$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, Z2$(P + 10) + ", " + Z3$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; seeZ!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "BLOCKS PER GAME"; Tab(41); "BLOCKS"

    For P = 181 To 190

        If Z2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, Z2$(P) + ", " + Z3$(P);
            Print #1, Tab(34);
            Print #1, Using "###.#"; seeZ!(P);
            Print #1, Tab(41);
        End If

        If Z2$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, Z2$(P + 10) + ", " + Z3$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; seeZ!(P + 20)
        End If

    Next

    Print #1,
    Print #1, "FIELD GOALS MADE"; Tab(41); "FREE THROWS MADE"

    For P = 201 To 210

        If Z2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, Z2$(P) + ", " + Z3$(P);
            Print #1, Tab(34);
            Print #1, Using "####"; seeZ!(P);
            Print #1, Tab(41);
        End If

        If Z2$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, Z2$(P + 10) + ", " + Z3$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; seeZ!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "FOULS PER GAME"; Tab(41); "FOULS"

    For P = 221 To 230

        If Z2$(P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, Z2$(P) + ", " + Z3$(P);
            Print #1, Tab(34);
            Print #1, Using "###.#"; seeZ!(P);
            Print #1, Tab(41);
        End If

        If Z2$(P + 10) = "" Then
            Print #1,
        Else
            Print #1, Z2$(P + 10) + ", " + Z3$(P + 10);
            Print #1, Tab(74);
            Print #1, Using "####"; seeZ!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "MOST VALUABLE PLAYER"

    For P = 241 To 250
        If Z2$(P) <> "" Then
            Print #1, Z2$(P) + ", " + Z3$(P);
            Print #1, Tab(34);
            Print #1, Using "####"; seeZ!(P)
        End If
    Next

    Print #1,
    Print #1, Chr$(12)
    Print #1, "                         OFFENSIVE TOTALS"
    Print #1, "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"

    For X = 1 To TEAMS_PER_CONFERENCE

        If OL$(X) <> "" And OL$(X) <> "*" And OL$(X) <> " " Then
            Print #1, Left$(OL$(X), 11); Tab(13);
            Print #1, Using "##"; OL!(X, 16);: Print #1, Tab(16);
            Print #1, Using "####"; OL!(X, 2);: Print #1, Tab(21);
            Print #1, Using "####"; OL!(X, 3);: Print #1, Tab(26);
            Print #1, Using ".###"; OL!(X, 17);: Print #1, Tab(31);
            Print #1, Using "#### #### .###"; OL!(X, 4); OL!(X, 5); OL!(X, 18);
            Print #1, Tab(46);
            Print #1, Using "####"; OL!(X, 6);: Print #1, Tab(51);
            Print #1, Using "####"; OL!(X, 9);: Print #1, Tab(56);
            Print #1, Using "####"; OL!(X, 7);: Print #1, Tab(61);
            Print #1, Using "###"; OL!(X, 8);: Print #1, Tab(65);
            Print #1, Using "###"; OL!(X, 10);: Print #1, Tab(69);
            Print #1, Using "#####"; OL!(X, 20);
            Print #1, Tab(75);: Print #1, Using "###.#"; OL!(X, 19)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "                         DEFENSIVE TOTALS"
    Print #1, "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 1 To TEAMS_PER_CONFERENCE

        If DL$(X) <> "" And DL$(X) <> "*" And DL$(X) <> " " Then
            Print #1, Left$(DL$(X), 11); Tab(13);
            Print #1, Using "##"; DL!(X, 16);: Print #1, Tab(16);
            Print #1, Using "####"; DL!(X, 2);: Print #1, Tab(21);
            Print #1, Using "####"; DL!(X, 3);: Print #1, Tab(26);
            Print #1, Using ".###"; DL!(X, 17);: Print #1, Tab(31);
            Print #1, Using "#### #### .###"; DL!(X, 4); DL!(X, 5); DL!(X, 18);
            Print #1, Tab(46);
            Print #1, Using "#### ####"; DL!(X, 6); DL!(X, 9);: Print #1, Tab(56);
            Print #1, Using "####"; DL!(X, 7);: Print #1, Tab(61);
            Print #1, Using "###"; DL!(X, 8);: Print #1, Tab(65);
            Print #1, Using "###"; DL!(X, 10);: Print #1, Tab(69);
            Print #1, Using "#####"; DL!(X, 20);
            Print #1, Tab(75);: Print #1, Using "###.#"; DL!(X, 19)
        End If

    Next

    Close #1

    'Print the file we created
    If destination <> 1 Then

        Open "REPORT" For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop

        Close #2

        LPrint Chr$(12)

    End If

End Sub

