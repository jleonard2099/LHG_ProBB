Sub SEELEAG ()

    Shared O1!(), O2!()

    Shared DL!(), OL!()
    Shared Z!(), Z1!()

    Shared DL$(), OL$()
    Shared leagT$(), Z2$(), Z3$()

    Shared LG$, D1$, D2$, D3$, D4$

    _Title "Basketball: The Pro Game - View League"

    H$ = " 1/2"

    Spec$ = diskPaths$(1) + "*.LDR"
    Count% = FCount%(Spec$)

    If Count% = 0 Then
        Locate , 12
        Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
        Exit Sub
        'Call LEADERS_MENU 'Run "HELLO.EXE 3"
    Else
        ReDim leagueLeaderFiles$(0 To Count%)
        For X = 1 To Count%
            leagueLeaderFiles$(X) = Space$(12)
        Next
        leagueLeaderFiles$(0) = Spec$
        Call ReadFile(leagueLeaderFiles$())

        Do
            Cls
            Color , 1
            Cls
            Locate , 28
            Color 14
            Print "SELECT LEAGUE TO VIEW"
            Color 15
            Selection% = 1
            START% = 1
            FileFGColr% = 15: FileBGColr% = 1
            HiLiteFGColr% = 0: HiLiteBGColr% = 7
            Rows% = 20
            Columns% = 4
            Spacing% = 6
            Row% = 4
            Column% = 1
            Locate 25, 1
            Color 14
            Print "ENTER";
            Color 15
            Print "-SELECTS LEAGUE";

            _MouseShow "DEFAULT"
            MAMenu leagueLeaderFiles$(), Selection%, START%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide

            If ExitCode% <> 27 Then

                Color 15, 1
                targetFile$ = RTrim$(leagueLeaderFiles$(Selection%))
                Locate 2, 33: Print targetFile$

                Do
                    Locate , 33: Print "ANY CHANGE (YN)";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                Color 15, 0

                If UCase$(I$) = "N" Then

                    WE$ = Left$(targetFile$, Len(targetFile$) - 4)
                    WE$ = Left$(WE$, 8)

                    Open diskPaths$(1) + WE$ + ".TMS" For Input As #2
                    Input #2, FF$, D1$, D2$, D3$, D4$
                    Close 2

                    D1$ = PadRight$(D1$, 8)
                    D2$ = PadRight$(D2$, 8)
                    D3$ = PadRight$(D3$, 8)
                    D4$ = PadRight$(D4$, 8)

                    Cls
                    Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

                    Open diskPaths$(1) + WE$ + ".LDR" For Input As #2

                    For X = 1 To TEAMS_PER_CONFERENCE
                        Input #2, leagT$(X), O1!(X), O2!(X)
                    Next

                    For X = 1 To 250
                        Input #2, Z2$(X), Z3$(X), Z!(X), Z1!(X)
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

                    Do
                        Cls
                        Call TITLE_STATS

						Do
							J = 5

							Locate 8, 8

							Color 15, 1
							
							Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

							For II = 1 To J
								Locate 8 + II, 8
								Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
							Next

							Locate 9 + J, 8: Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";
							
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
								Case "P":
									Call SavePrintLeagueStats(SF$, 0)

								Case "F":
									Do
										Color 15, 0
										Cls

										JJ = 7

										Locate 6, 8

										Color 15, 1

										Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";
										
										For II = 1 To JJ: 
											Locate 6 + II, 8
											Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
										Next

										Locate 7 + JJ, 8
										Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

										Locate 7, 32
										Locate , 12: Print "ENTER NAME FOR FILE: ": Print
										Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
										Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE PROGRAM"
										Locate , 12: Print "WILL AUTOMATICALLY ADD .RPT"
										Locate 7, 37: Input SF$

										If SF$ <> "" Then
											Locate 13, 12: Print "ANY CHANGE (YN) "
											I$ = GetKeyPress$
										End If

									Loop Until UCase$(I$) = "N" And SF$ <> ""

									Color 15, 0
									Call SavePrintLeagueStats(SF$, 1)

								Case "S":
									Call LeagueStatsToScreen

									Color 15, 0
									Cls

									JJ = 4

									Locate 6, 8

									Color 15, 1

									Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

									For II = 1 To JJ: 
										Locate 6 + II, 8
										Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
									Next

									Locate 7 + JJ, 8

									Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";

									Do
										Color 15, 1
										Locate 8
										U$ = "VIEW LEAGUE STATS AGAIN (YN)"
										Locate , (80 - Len(U$)) / 2
										Print U$
										Color 15, 0
										A$ = GetKeyPress$
									Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

							End Select

						End If

                    Loop Until UCase$(A$) = "N" Or A$ = Chr$(27) Or I$ = Chr$(27) 

                End If 'End checking if we are viewing stats again

            End If 'Done checking for ESC

        Loop Until UCase$(I$) = "N" Or ExitCode% = 27 Or I$ = Chr$(27) 

    End If 'End checking for files

End Sub

'----------------------------------------
'     LeagueStatsToScreen Subroutine
'----------------------------------------
'... explain here ...
Sub LeagueStatsToScreen ()

    Shared O1!(), O2!()

    Shared DL!(), OL!()
    Shared Z!(), Z1!()

    Shared DL$(), OL$(), OL!()
    Shared leagT$(), Z2$(), Z3$()

    Shared LG$, D1$, D2$, D3$, D4$

    Cls
    Color 12: Print "                        "; LG$; " LEAGUE STANDINGS"
    Print
    Color 14: Print "    "; D1$; " DIVISION"
    Print
    Color 2: Print "                   W    L     PCT   GB"
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

    HZ = 1
    VT = 24
    Locate VT, HZ: Print Space$(26);
    QK$ = GetKeyPress$

    Cls
    Color 12: Print "                        "; LG$; " LEAGUE STANDINGS"
    Print
    Color 14: Print "    "; D2$; " DIVISION"
    Print
    Color 2: Print "                   W    L     PCT   GB"
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

    HZ = 1
    VT = 24
    Locate VT, HZ: Print Space$(26);
    QK$ = GetKeyPress$

    Cls
    Color 12: Print "                        "; LG$; " LEAGUE STANDINGS"
    Print
    Color 14: Print "    "; D3$; " DIVISION"
    Print
    Color 2: Print "                   W    L     PCT   GB"
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

    HZ = 1
    VT = 24
    Locate VT, HZ: Print Space$(26);
    QK$ = GetKeyPress$

    Cls
    Color 12: Print "                        "; LG$; " LEAGUE STANDINGS"
    Print
    Color 14: Print "    "; D4$; " DIVISION"
    Print
    Color 2: Print "                   W    L     PCT   GB"
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

    HZ = 1
    VT = 24
    Locate VT, HZ: Print Space$(26);
    QK$ = GetKeyPress$

    Cls
    Color 14
    Print "FIELD GOAL % (MIN 5 FGA/G)"; Tab(41); "FREE THROW % (MIN 2 FTA/G)"
	'Print
    Print "                            PCT   FGA"; Tab(41); "                            PCT   FTA"
    Color 15

    For P = 41 To 60
        If Z2$(P) <> "*" And Z2$(P) <> "" Then
            Print Left$(Z2$(P), 10) + ", " + Z3$(P);: Locate , 28
            Print Using "##.#  ####"; Z!(P) * 100; Z1!(P)
        End If
    Next

    Locate 4

    For P = 61 To 80
        If Z2$(P) <> "*" And Z2$(P) <> "" Then
            Locate , 41: Print Left$(Z2$(P), 10) + ", " + Z3$(P);: Locate , 68
            Print Using "##.#  ####"; Z!(P) * 100; Z1!(P)
        End If
    Next

    HZ = 1
    VT = 24
    Locate VT, HZ: Print Space$(26);
    QK$ = GetKeyPress$

    Cls
    Color 14: Print "SCORING"
    Color 14: Print "                                PTS/G"
    Color 15

    For P = 1 To 20
        If Z2$(P) <> "*" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "###.#"; Z!(P)
        End If
    Next

    Color 14
    Locate 1, 41: Print "TOTAL POINTS"
    Locate , 41: Print "                                  PTS"
    Color 15

    For P = 21 To 40
        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "####"; Z!(P)
        End If
    Next

    HZ = 1
    VT = 24
    Locate VT, HZ: Print Space$(26);
    QK$ = GetKeyPress$

    Cls
    Color 14: Print "STEALS PER GAME"
    Color 15

    For P = 81 To 90
        If Z2$(P) <> "*" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "###.#"; Z!(P)
        End If
    Next

    Locate 1, 41: Color 14: Print "STEALS"
    Color 15

    For P = 91 To 100
        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "####"; Z!(P)
        End If
    Next

    Locate 13: Color 14: Print "ASSISTS PER GAME"
    Color 15

    For P = 101 To 110
        If Z2$(P) <> "*" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "###.#"; Z!(P)
        End If
    Next

    Locate 13, 41: Color 14: Print "ASSISTS"
    Color 15

    For P = 111 To 120
        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "####"; Z!(P)
        End If
    Next

    HZ = 1
    VT = 24
    Locate VT, HZ: Print Space$(26);
    QK$ = GetKeyPress$

    Cls

    Color 14: Print "THREE FIELD GOAL%"
    Color 15
    For P = 121 To 130
        If Z2$(P) <> "" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "###.#"; Z!(P) * 100
        End If
    Next P

    Locate 1, 41
    Color 14: Print "MINUTES PER GAME"
    Color 15

    For P = 141 To 150
        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "###.#"; Z!(P)
        End If
    Next P

    Locate 13
    Color 14: Print "THREE POINT FIELD GOALS MADE"
    Color 15

    For P = 131 To 140
        If Z2$(P) <> "" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "####"; Z!(P)
        End If
    Next

    Locate 13, 41
    Color 14: Print "TOTAL MINUTES"
    Color 15

    For P = 151 To 160
        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "####"; Z!(P)
        End If
    Next

    HZ = 1
    VT = 24
    Locate VT, HZ: Print Space$(27);
    QK$ = GetKeyPress$

    Cls
    Color 14: Print "REBOUNDS PER GAME   "
    Color 15

    For P = 161 To 170
        If Z2$(P) <> "*" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "###.#"; Z!(P)
        End If
    Next

    Locate 1, 41: Color 14: Print "REBOUNDS"
    Color 15

    For P = 171 To 180
        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "####"; Z!(P)
        End If
    Next

    Locate 13: Color 14: Print "BLOCKS PER GAME"
    Color 15

    For P = 181 To 190
        If Z2$(P) <> "*" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "###.#"; Z!(P)
        End If
    Next

    Locate 13, 41: Color 14: Print "BLOCKS"
    Color 15

    For P = 191 To 200
        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "####"; Z!(P)
        End If
    Next

    HZ = 1
    VT = 24
    Locate VT, HZ: Print Space$(26);
    QK$ = GetKeyPress$

    Cls
    Color 14: Print "FIELD GOALS MADE "
    Color 15

    For P = 201 To 210
        If Z2$(P) <> "*" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "####"; Z!(P)
        End If
    Next

    Locate 1, 41
    Color 14: Print "FREE THROWS MADE"
    Color 15

    For P = 211 To 220
        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "####"; Z!(P)
        End If
    Next

    Locate 13
    Color 14: Print "FOULS PER GAME"
    Color 15

    For P = 221 To 230
        If Z2$(P) <> "*" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "###.#"; Z!(P)
        End If
    Next

    Locate 13, 41: Color 14: Print "FOULS"
    Color 15

    For P = 231 To 240
        If Z2$(P) <> "*" Then
            Locate , 41: Print Z2$(P) + ", " + Z3$(P);
            Locate , 74: Print Using "####"; Z!(P)
        End If
    Next

    HZ = 1
    VT = 24
    Locate VT, HZ: Print Space$(26);
    QK$ = GetKeyPress$

    Cls
    Color 14: Print "MOST VALUABLE PLAYER POINTS"
    Color 15

    For P = 241 To 250
        If Z2$(P) <> "*" Then
            Print Z2$(P) + ", " + Z3$(P);
            Locate , 34: Print Using "####"; Z!(P)
        End If
    Next

    HZ = 1
    VT = 24
    Locate VT, HZ: Print Space$(26);
    QK$ = GetKeyPress$

    Cls
    Color 12: Print "                         OFFENSIVE TOTALS"
    Color 14: Print "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 1 To 20

        If Not (OL$(X) = "" Or OL$(X) = "*" Or OL$(X) = " ") Then

            Print Left$(OL$(X), 11); Tab(13);
            Print Using "##"; OL!(X, 16);: Print Tab(16);
            Print Using "####"; OL!(X, 2);: Print Tab(21);
            Print Using "####"; OL!(X, 3);: Print Tab(26);
            Print Using ".###"; OL!(X, 17);: Print Tab(31);
            Print Using "#### #### .###"; OL!(X, 4); OL!(X, 5); OL!(X, 18);: Print Tab(46);
            Print Using "####"; OL!(X, 6);: Print Tab(51);
            Print Using "####"; OL!(X, 9);: Print Tab(56);
            Print Using "####"; OL!(X, 7);: Print Tab(61);
            Print Using "###"; OL!(X, 8);: Print Tab(65);
            Print Using "###"; OL!(X, 10);: Print Tab(69);
            Print Using "#####"; OL!(X, 20);: Print Tab(75);
            Print Using "###.#"; OL!(X, 19)

        End If

    Next

    HZ = 1
    VT = 24
    Locate VT, HZ: Print Space$(26);
    QK$ = GetKeyPress$

    Cls
    Color 12: Print "                      OFFENSIVE TOTALS (cont)"
    Color 14: Print "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 21 To 40
        If OL$(X) <> "" And OL$(X) <> "*" And OL$(X) <> " " Then
            Print Left$(OL$(X), 11); Tab(13);
            Print Using "##"; OL!(X, 16);: Print Tab(16);
            Print Using "####"; OL!(X, 2);: Print Tab(21);
            Print Using "####"; OL!(X, 3);: Print Tab(26);
            Print Using ".###"; OL!(X, 17);: Print Tab(31);
            Print Using "#### #### .###"; OL!(X, 4); OL!(X, 5); OL!(X, 18);: Print Tab(46);
            Print Using "####"; OL!(X, 6);: Print Tab(51);
            Print Using "####"; OL!(X, 9);: Print Tab(56);
            Print Using "####"; OL!(X, 7);: Print Tab(61);
            Print Using "###"; OL!(X, 8);: Print Tab(65);
            Print Using "###"; OL!(X, 10);: Print Tab(69);
            Print Using "#####"; OL!(X, 20);: Print Tab(75);
            Print Using "###.#"; OL!(X, 19)
        End If
    Next

    HZ = 1
    VT = 24
    Locate VT, HZ: Print Space$(26);
    QK$ = GetKeyPress$

    Cls
    Color 12: Print "                         DEFENSIVE TOTALS"
    Color 14: Print "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 1 To 20

        If DL$(X) <> "" And DL$(X) <> "*" And DL$(X) <> " " Then
            Print Left$(DL$(X), 11); Tab(13);
            Print Using "##"; DL!(X, 16);: Print Tab(16);
            Print Using "####"; DL!(X, 2);: Print Tab(21);
            Print Using "####"; DL!(X, 3);: Print Tab(26);
            Print Using ".###"; DL!(X, 17);: Print Tab(31);
            Print Using "#### #### .###"; DL!(X, 4); DL!(X, 5); DL!(X, 18);: Print Tab(46);
            Print Using "#### ####"; DL!(X, 6); DL!(X, 9);: Print Tab(56);
            Print Using "####"; DL!(X, 7);: Print Tab(61);
            Print Using "###"; DL!(X, 8);: Print Tab(65);
            Print Using "###"; DL!(X, 10);: Print Tab(69);
            Print Using "#####"; DL!(X, 20);: Print Tab(75);
            Print Using "###.#"; DL!(X, 19)
        End If

    Next

    HZ = 1
    VT = 24
    Locate VT, HZ: Print Space$(26);
    QK$ = GetKeyPress$

    Cls
    Color 12: Print "                      DEFENSIVE TOTALS (cont)"
    Color 14: Print "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 21 To 40
        If DL$(X) <> "" And DL$(X) <> "*" And DL$(X) <> " " Then
            Print Left$(DL$(X), 11); Tab(13);: Print Using "##"; DL!(X, 16);: Print Tab(16);: Print Using "####"; DL!(X, 2);: Print Tab(21);: Print Using "####"; DL!(X, 3);: Print Tab(26);
            Print Using ".###"; DL!(X, 17);: Print Tab(31);: Print Using "#### #### .###"; DL!(X, 4); DL!(X, 5); DL!(X, 18);
            Print Tab(46);: Print Using "#### ####"; DL!(X, 6); DL!(X, 9);: Print Tab(56);: Print Using "####"; DL!(X, 7);: Print Tab(61);
            Print Using "###"; DL!(X, 8);: Print Tab(65);: Print Using "###"; DL!(X, 10);: Print Tab(69);: Print Using "#####"; DL!(X, 20);
            Print Tab(75);: Print Using "###.#"; DL!(X, 19)
        End If
    Next

    HZ = 1
    VT = 24
    Locate VT, HZ: Print Space$(26);
    QK$ = GetKeyPress$

End Sub

'----------------------------------------
'     SavePrintLeagueStats Subroutine
'----------------------------------------
'Destination
'   = 1 is File
'   <> 1 is Printer (creates file and prints it)
Sub SavePrintLeagueStats (statFile$, destination)

    Shared O1!(), O2!()
    Shared DL!(), OL!()
    Shared Z!(), Z1!()

    Shared OL$(), DL$(), leagT$()
	Shared Z2$(), Z3$()

    Shared LG$, D1$, D2$, D3$, D4$

    'Save Report File
    Cls
	
    If destination <> 1 Then
        Open "REPORT" For Output As #2
    Else
        Open statFile$ + ".RPT" For Output As #2
    End If

    Print #2, "                        "; LG$; " LEAGUE STANDINGS"
    Print #2,
    Print #2, "    "; D1$; " DIVISION"
    Print #2,
    Print #2, "                   W    L     PCT   GB"
	
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
            Print #2, "    "; leagT$(P); Tab(16);
			Print #2, Using "   ##   ##   #.###"; O1!(P); O2!(P); PCT!;
            Print #2, BK$
        End If
    Next P

    Print #2,
    Print #2, "    "; D2$; " DIVISION"
    Print #2,
    Print #2, "                   W    L     PCT   GB"

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
            Print #2, "    "; leagT$(P); Tab(16);
            PCT! = O1!(P) / (O1!(P) + O2!(P))
            Print #2, Using "   ##   ##   #.###"; O1!(P); O2!(P); PCT!;
            Print #2, BK$
        End If
    Next P

    Print #2,
    Print #2, "    "; D3$; " DIVISION"
    Print #2,
    Print #2, "                   W    L     PCT   GB"
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
            Print #2, "    "; leagT$(P); Tab(16);: Print #2, Using "   ##   ##   #.###"; O1!(P); O2!(P); PCT!;
            Print #2, BK$
        End If
    Next P

    Print #2,
    Print #2, "    "; D4$; " DIVISION"
    Print #2,
    Print #2, "                   W    L     PCT   GB"

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
            Print #2, "    "; leagT$(P); Tab(16);
            PCT! = O1!(P) / (O1!(P) + O2!(P))
            Print #2, Using "   ##   ##   #.###"; O1!(P); O2!(P); PCT!;
            Print #2, BK$
        End If
    Next P

    Print #2, Chr$(12)
    Print #2, "FIELD GOAL % (MIN 5 FGA/G)"; Tab(41); "FREE THROW % (MIN 2 FTA/G)"
    Print #2,
    Print #2, "                            PCT   FGA"; Tab(41); "                            PCT   FTA"

    For P = 41 To 60
        If Z2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, Left$(Z2$(P), 10) + ", " + Z3$(P);
            Print #2, Tab(28);
            Print #2, Using "##.#  ####"; Z!(P) * 100; Z1!(P);
            Print #2, Tab(41);
        End If
        If Z2$(P + 20) = "" Then
            Print #2,
        Else
            Print #2, Left$(Z2$(P + 20), 10) + ", " + Z3$(P + 20);
            Print #2, Tab(68);
            Print #2, Using "##.#  ####"; Z!(P + 20) * 100; Z1!(P)
        End If

    Next

    Print #2,
    Print #2, "SCORING"; Tab(41); "TOTAL POINTS"

    For P = 1 To 20
        If Z2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, Z2$(P) + ", " + Z3$(P);
            Print #2, Tab(34);
            Print #2, Using "###.#"; Z!(P);
            Print #2, Tab(41);
        End If
        If Z2$(P + 20) = "" Then
            Print #2,
        Else
            Print #2, Z2$(P + 20) + ", " + Z3$(P + 20);
            Print #2, Tab(74);
            Print #2, Using "####"; Z!(P + 20)
        End If
    Next

    Print #2, Chr$(12)
    Print #2, "STEALS PER GAME"; Tab(41); "STEALS"

    For P = 81 To 90
        If Z2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, Z2$(P) + ", " + Z3$(P);
            Print #2, Tab(34);
            Print #2, Using "###.#"; Z!(P);
            Print #2, Tab(41);
        End If
        If Z2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; Z!(P + 10)
        End If
    Next

    Print #2,
    Print #2, "ASSISTS PER GAME"; Tab(41); "ASSISTS"

    For P = 101 To 110
        If Z2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, Z2$(P) + ", " + Z3$(P);
            Print #2, Tab(34);
            Print #2, Using "###.#"; Z!(P);
            Print #2, Tab(41);
        End If
        If Z2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; Z!(P + 10)
        End If
    Next

    Print #2,
    Print #2, "THREE FIELD GOAL %"; Tab(41); "MINUTES PER GAME"

    For P = 121 To 130
        If Z2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, Z2$(P) + ", " + Z3$(P);
            Print #2, Tab(34);
            Print #2, Using "###.#"; Z!(P) * 100;
            Print #2, Tab(41);
        End If
        If Z2$(P + 20) = "" Then
            Print #2,
        Else
            Print #2, Z2$(P + 20) + ", " + Z3$(P + 20);
            Print #2, Tab(74);
            Print #2, Using "###.#"; Z!(P + 20)
        End If
    Next

    Print #2,
    Print #2, "THREE POINT FIELD GOALS MADE"; Tab(41); "MINUTES"

    For P = 131 To 140
        If Z2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, Z2$(P) + ", " + Z3$(P);
            Print #2, Tab(34);
            Print #2, Using "####"; Z!(P);
            Print #2, Tab(41);
        End If
        If Z2$(P + 20) = "" Then
            Print #2,
        Else
            Print #2, Z2$(P + 20) + ", " + Z3$(P + 20);
            Print #2, Tab(74);
            Print #2, Using "####"; Z!(P + 20)
        End If
    Next

    Print #2,
    Print #2, Chr$(12)
    Print #2, "REBOUNDS PER GAME"; Tab(41); "REBOUNDS"
    For P = 161 To 170
        If Z2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, Z2$(P) + ", " + Z3$(P);
            Print #2, Tab(34);
            Print #2, Using "###.#"; Z!(P);
            Print #2, Tab(41);
        End If
        If Z2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; Z!(P + 10)
        End If
    Next

    Print #2,
    Print #2, "BLOCKS PER GAME"; Tab(41); "BLOCKS"

    For P = 181 To 190
        If Z2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, Z2$(P) + ", " + Z3$(P);
            Print #2, Tab(34);
            Print #2, Using "###.#"; Z!(P);
            Print #2, Tab(41);
        End If
        If Z2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; Z!(P + 20)
        End If
    Next

    Print #2,
    Print #2, "FIELD GOALS MADE"; Tab(41); "FREE THROWS MADE"

    For P = 201 To 210
        If Z2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, Z2$(P) + ", " + Z3$(P);
            Print #2, Tab(34);
            Print #2, Using "####"; Z!(P);
            Print #2, Tab(41);
        End If
        If Z2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; Z!(P + 10)
        End If
    Next

    Print #2,
    Print #2, "FOULS PER GAME"; Tab(41); "FOULS"

    For P = 221 To 230
        If Z2$(P) = "" Then
            Print #2, Tab(41);
        Else
            Print #2, Z2$(P) + ", " + Z3$(P);
            Print #2, Tab(34);
            Print #2, Using "###.#"; Z!(P);
            Print #2, Tab(41);
        End If
        If Z2$(P + 10) = "" Then
            Print #2,
        Else
            Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
            Print #2, Tab(74);
            Print #2, Using "####"; Z!(P + 10)
        End If
    Next

    Print #2,
    Print #2, "MOST VALUABLE PLAYER"

    For P = 241 To 250
        If Z2$(P) <> "" Then
            Print #2, Z2$(P) + ", " + Z3$(P);
            Print #2, Tab(34);
            Print #2, Using "####"; Z!(P)
        End If
    Next

    Print #2,
    Print #2, Chr$(12)
    Print #2, "                         OFFENSIVE TOTALS"
    Print #2, "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"

    For X = 1 To TEAMS_PER_CONFERENCE
        If OL$(X) <> "" And OL$(X) <> "*" And OL$(X) <> " " Then
            Print #2, Left$(OL$(X), 11); Tab(13);
            Print #2, Using "##"; OL!(X, 16);: Print #2, Tab(16);
            Print #2, Using "####"; OL!(X, 2);: Print #2, Tab(21);
            Print #2, Using "####"; OL!(X, 3);: Print #2, Tab(26);
            Print #2, Using ".###"; OL!(X, 17);: Print #2, Tab(31);
            Print #2, Using "#### #### .###"; OL!(X, 4); OL!(X, 5); OL!(X, 18);
            Print #2, Tab(46);
            Print #2, Using "####"; OL!(X, 6);: Print #2, Tab(51);
            Print #2, Using "####"; OL!(X, 9);: Print #2, Tab(56);
            Print #2, Using "####"; OL!(X, 7);: Print #2, Tab(61);
            Print #2, Using "###"; OL!(X, 8);: Print #2, Tab(65);
            Print #2, Using "###"; OL!(X, 10);: Print #2, Tab(69);
            Print #2, Using "#####"; OL!(X, 20);
            Print #2, Tab(75);: Print #2, Using "###.#"; OL!(X, 19)
        End If
    Next

    Print #2, Chr$(12)
    Print #2, "                         DEFENSIVE TOTALS"
    Print #2, "             G  FGA  FGM  PCT  FTA  FTM  PCT  REB  AST   PF STL BLK   PTS   AVG"
    Color 15

    For X = 1 To TEAMS_PER_CONFERENCE
        If DL$(X) <> "" And DL$(X) <> "*" And DL$(X) <> " " Then
            Print #2, Left$(DL$(X), 11); Tab(13);
            Print #2, Using "##"; DL!(X, 16);: Print #2, Tab(16);
            Print #2, Using "####"; DL!(X, 2);: Print #2, Tab(21);
            Print #2, Using "####"; DL!(X, 3);: Print #2, Tab(26);
            Print #2, Using ".###"; DL!(X, 17);: Print #2, Tab(31);
            Print #2, Using "#### #### .###"; DL!(X, 4); DL!(X, 5); DL!(X, 18);
            Print #2, Tab(46);
            Print #2, Using "#### ####"; DL!(X, 6); DL!(X, 9);: Print #2, Tab(56);
            Print #2, Using "####"; DL!(X, 7);: Print #2, Tab(61);
            Print #2, Using "###"; DL!(X, 8);: Print #2, Tab(65);
            Print #2, Using "###"; DL!(X, 10);: Print #2, Tab(69);
            Print #2, Using "#####"; DL!(X, 20);
            Print #2, Tab(75);: Print #2, Using "###.#"; DL!(X, 19)
        End If
    Next

    Close 2

    'Print the file we created
    If destination <> 1 Then

        Open "REPORT" For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop

        Close 2

		LPrint Chr$(12)

    End If

End Sub
