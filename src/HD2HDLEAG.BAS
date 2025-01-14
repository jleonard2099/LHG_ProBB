'----------------------------------------
'          HD2HDLEAG Subroutine
'----------------------------------------
'This subroutine reads in stats from the
'selected team and then presents the user
'a menu of options for viewing Head-to-Head
'team comparisons.
Sub HD2HDLEAG ()

    _Title "Basketball: The Pro Game - Head-to-Head Stats"

    Shared memberIdx(), teamScore(), oppScore(), teamRatings()

    Shared awayWins!(),awayLosses(), homeLosses!(),homeWins!()
    Shared homeScoreTeam!(), homeScoreOpp!(), awayScoreTeam!(), awayScoreOpp!()
    Shared totAwayLosses!(), totAwayWin!(), totHomeLosses!(), totHomeWin!()
    Shared totHomeScoreTeam!(), totHomeScoreOpp!(), totAwayScoreTeam!(), totAwayScoreOpp!()

    Shared oppName$(), locIndicator$()
    Shared memberName$(), memberYears$()

    Shared leagueName$, div1Name$, div2Name$, div3Name$, div4Name$

    newTeam = 1

    Do

        If newTeam = 1 Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr > 0 Then

                statFile$ = nameStatFile$(team$)

                If Not _FileExists(diskPaths$(1) + statFile$ + "S") Then

                    MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
                    BoxWidth = 24
                    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                    Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                    Call CSMsgBox(MsgText$, BoxWidth, Pass)
                    J$ = GetKeyPress$
                    Call CSMsgBox("", 0, Pass)

                Else

                    Open diskPaths$(1) + statFile$ + "S" For Input As #1

                    Input #1, nbrGames

                    For J = 1 To MAX_SCHED_STATS:
                        Input #1, teamScore(J)
                    Next

                    For J = 1 To MAX_SCHED_STATS:
                        Input #1, oppName$(J)
                    Next

                    For J = 1 To MAX_SCHED_STATS:
                        Input #1, oppScore(J)
                    Next

                    For J = 1 To MAX_SCHED_STATS:
                        Input #1, locIndicator$(J)
                    Next

                    Close #1

                    'Check if same league
                    If sameLeague <> 1 Then
                        leagueFile$ = ChooseFile$(diskPaths$(2) + "*.TMI", "LEAGUE", ExitCode%)
                    End If

                End If

            Else
                'If a team wasn't chosen, treat it like ESC
                ExitCode% = 27

            End If 'Done checking for valid team
 
        End If 'Done checking for new team

        Color 7, 0
        Cls

        If teamNbr > 0 And ExitCode% <> 27 And ExitCode% <> 0 Then

            userKey$ = GetOutputOptions$

            If userKey$ <> Chr$(27) Then

                Select Case userKey$

                    Case "F"                   
                        printChoice = 0
                        VPP = 0
                    Case "P"
                        printChoice = 1
                        VPP = 0
                    Case "S"
                        VPP = 1

                End Select

                'Haven't checked that file exists
                Open diskPaths$(2) + leagueFile$ + ".TMI" For Input As #2

                Input #2, leagueName$
                Input #2, div1Name$
                Input #2, div2Name$
                Input #2, div3Name$
                Input #2, div4Name$

                For X = 1 To TEAMS_PER_LEAGUE
                    Input #2, memberName$(X)
                    Input #2, memberIdx(X)
                    Input #2, memberName$(X)
                Next

                Close #2

                Cls
                Color 15
                Locate 18, 18: Print "COMPUTING W/L/T RECORDS . . . PLEASE WAIT"

                'Initialization for 1st league
                Erase awayLosses, awayWins!, homeLosses!, homeWins!, homeScoreTeam!, homeScoreOpp!, awayScoreTeam!, awayScoreOpp!
                Erase totAwayLosses!, totAwayWin!, totHomeLosses!, totHomeWin!, totHomeScoreTeam!, totHomeScoreOpp!, totAwayScoreTeam!, totAwayScoreOpp!

                'Calculations for 1st league
                For K = 1 To TEAMS_PER_LEAGUE

                    If memberName$(K) <> "XXX" Then

                        For I = 1 To nbrGames

                            If Left$(locIndicator$(I), 1) = "V" And oppName$(I) = memberName$(K) Then

                                If teamScore(I) > oppScore(I) Then
                                    awayWins!(K) = awayWins!(K) + 1
                                End If

                                If teamScore(I) < oppScore(I) And Left$(locIndicator$(I), 1) = "V" And oppName$(I) = memberName$(K) Then
                                    awayLosses(K) = awayLosses(K) + 1
                                End If

                                awayScoreTeam!(K) = awayScoreTeam!(K) + teamScore(I)
                                awayScoreOpp!(K) = awayScoreOpp!(K) + oppScore(I)

                            End If

                            If (Left$(locIndicator$(I), 1) = "H" Or Left$(locIndicator$(I), 1) = "N") And oppName$(I) = memberName$(K) Then

                                If teamScore(I) > oppScore(I) Then
                                    homeWins!(K) = homeWins!(K) + 1
                                End If

                                If teamScore(I) < oppScore(I) Then
                                    homeLosses!(K) = homeLosses!(K) + 1
                                End If

                                homeScoreTeam!(K) = homeScoreTeam!(K) + teamScore(I)
                                homeScoreOpp!(K) = homeScoreOpp!(K) + oppScore(I)

                            End If

                        Next I

                    End If

                Next K

                'Generate output
                If VPP = 0 Then
                    Call Hd2HdPro_Report(team$, printChoice)
                Else
                    Call Hd2HdPro_Screen(team$)
                End If

                Cls

                Call TITLE_UTILS

                Call MenuBox(8, 17, 40, 6, 15, 1)

                Do

                    Locate 10, 15

                    Color 3: Locate , 24: Print "SELECT OPTION:"
                    Color 7: Locate , 24: Print "VIEW ";
                    Color 15: Print "S";
                    Color 7: Print "AME TEAM AGAIN"
                    Color 7: Locate , 24: Print "VIEW ";
                    Color 15: Print "A";
                    Color 7: Print "NOTHER TEAM (SAME LEAGUE)"
                    Color 15: Locate , 24: Print "R";
                    Color 7: Print "ETURN TO OPTIONS"

                    K$ = GetKeyPress$

                    Select Case UCase$(K$)

                        Case "A"
                            newTeam = 1
                            sameLeague = 1
                            RDD = 0

                        Case "R"
                            newTeam = 0
                            sameLeague = 0
                            RDD = 0

                        Case "S"
                            newTeam = 0
                            sameLeague = 1
                            RDD = 0
                            
                            'Re-initialize for the next loop
                            Erase totAwayLosses!, totAwayWin!, totHomeLosses!, totHomeWin!
                            Erase totHomeScoreTeam!, totHomeScoreOpp!, totAwayScoreTeam!, totAwayScoreOpp!  

                    End Select

                Loop Until UCase$(K$) = "A" Or UCase$(K$) = "R" Or UCase$(K$) = "S" Or K$ = Chr$(27)

            End If 'Done checking for stats file

        End If

    Loop Until UCase$(K$) = "R" Or K$ = Chr$(27) Or userKey$ = Chr$(27) Or ExitCode% = 27

End Sub


'----------------------------------------
'       Hd2HdPro_Report Subroutine
'----------------------------------------
'This subroutine outputs the calculated
'comparison values to the default printer
Sub Hd2HdPro_Report (teamName$, printDest)

    Shared awayLosses(), awayWins!(), homeLosses!(), homeWins!()
    Shared homeScoreTeam!(), homeScoreOpp!(), awayScoreTeam!(), awayScoreOpp!()
    Shared totAwayLosses!(), totAwayWin!(), totHomeLosses!(), totHomeWin!()
    Shared totHomeScoreTeam!(), totHomeScoreOpp!(), totAwayScoreTeam!(), totAwayScoreOpp!()

    Shared memberName$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    Dim DV$(1 To 4)

    If printDest = 0 Then
        destFile$ = _SaveFileDialog$("Save Comparison Report", _CWD$ + "\Comparison", "*.txt", "Text File")
    Else
        destFile$ = "Comparison"
    End If

    Color 15, 0
    Cls

    DV$(1) = div1Name$
    DV$(2) = div2Name$
    DV$(3) = div3Name$
    DV$(4) = div4Name$

    Open destFile$ For Output As #1

    div = 0

    Do

        RDD = div + 1

        Print #1, Tab(55); teamName$
        'Print #1, Tab(35); teamName$
        Print #1,
        Print #1, Tab(22); "--------- HOME ----------"; Tab(54); "--------- AWAY ----------"; Tab(86); "---------- TOTALS ----------"
        'Print #1, Tab(15); "--------- HOME ----------"; Tab(36); "--------- AWAY ----------"; Tab(58); "---------- TOTALS ----------"

        Print #1, Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "PF/G"; Tab(43); "PA/G"; Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "PF/G"; Tab(75); "PA/G"; Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "PF/G"; Tab(110); "PA/G"
        'Print #1, Tab(15); "W"; Tab(17); "L"; Tab(21); "PCT."; Tab(26); "PF/G"; Tab(32); "PA/G"; Tab(37); "W"; Tab(39); "L"; Tab(43); "PCT."; Tab(48); "PF/G"; Tab(54); "PA/G"; Tab(59); "W"; Tab(61); "L"; Tab(65); "PCT."; Tab(70); "PF/G"; Tab(76); "PA/G"

        DD = ((RDD - 1) * TEAMS_PER_DIV) + 1
        EE = RDD * TEAMS_PER_DIV

        'If DV$(RDD) <> "XXX" And DV$(RDD) <> "" Then

        Print #1, Tab(2); DV$(RDD)

        For K = DD To EE

            If memberName$(K) <> "XXX" And memberName$(K) <> teamName$ Then

                Print #1, memberName$(K);
                Print #1, Tab(22); Using "#"; homeWins!(K);
                Print #1, Tab(26); Using "#"; homeLosses!(K);

                If homeWins!(K) = homeWins!(K) + homeLosses!(K) And homeWins!(K) <> 0 Then
                    Print #1, Tab(28); "1.000";
                End If
                If homeWins!(K) = 0 Then
                    Print #1, Tab(29); ".000";
                End If
                If homeWins!(K) > 0 And homeWins!(K) < homeWins!(K) + homeLosses!(K) Then
                    Print #1, Tab(29); Using ".###"; homeWins!(K) / (homeWins!(K) + homeLosses!(K));
                End if
                If homeScoreTeam!(K) = 0 Then
                    Print #1, Tab(35); "  0.0";
                End if
                If homeScoreTeam!(K) > 0 Then
                    Print #1, Tab(35); Using "###.#"; homeScoreTeam!(K) / (homeWins!(K) + homeLosses!(K));
                End if
                If homeScoreOpp!(K) = 0 Then
                    Print #1, Tab(42); "  0.0";
                End if
                If homeScoreOpp!(K) > 0 Then
                    Print #1, Tab(42); Using "###.#"; homeScoreOpp!(K) / (homeWins!(K) + homeLosses!(K));
                End if

                Print #1, Tab(54); Using "#"; awayWins!(K);
                Print #1, Tab(58); Using "#";awayLosses(K);

                If awayWins!(K) = awayWins!(K) + awayLosses(K) And awayWins!(K) <> 0 Then
                    Print #1, Tab(60); "1.000";
                End if
                If awayWins!(K) = 0 Then
                    Print #1, Tab(61); ".000";
                End if
                If awayWins!(K) > 0 And awayWins!(K) < awayWins!(K) + awayLosses(K) Then
                    Print #1, Tab(61); Using ".###"; (awayWins!(K) / (awayWins!(K) + awayLosses(K)));
                End if
                If awayScoreTeam!(K) = 0 Then
                    Print #1, Tab(67); "  0.0";
                End if
                If awayScoreTeam!(K) > 0 Then
                    Print #1, Tab(67); Using "###.#"; awayScoreTeam!(K) / (awayWins!(K) + awayLosses(K));
                End if
                If awayScoreOpp!(K) = 0 Then
                    Print #1, Tab(74); "  0.0";
                End if
                If awayScoreOpp!(K) > 0 Then
                    Print #1, Tab(74); Using "###.#"; awayScoreOpp!(K) / (awayWins!(K) + awayLosses(K));
                End if

                Print #1, Tab(86); Using "##"; homeWins!(K) + awayWins!(K);
                Print #1, Tab(91); Using "##"; homeLosses!(K) + awayLosses(K);

                If homeWins!(K) + awayWins!(K) = homeWins!(K) + awayWins!(K) + homeLosses!(K) + awayLosses(K) And homeWins!(K) + awayWins!(K) <> 0 Then
                    Print #1, Tab(95); "1.000";
                End if
                If homeWins!(K) + awayWins!(K) = 0 Then
                    Print #1, Tab(96); ".000";
                End if
                If homeWins!(K) + awayWins!(K) > 0 And (homeWins!(K) + awayWins!(K)) < (homeWins!(K) + awayWins!(K) + homeLosses!(K) + awayLosses(K)) Then
                    Print #1, Tab(96); Using ".###"; (homeWins!(K) + awayWins!(K)) / (homeWins!(K) + awayWins!(K) + homeLosses!(K) + awayLosses(K));
                End if
                If homeScoreTeam!(K) + awayScoreTeam!(K) = 0 Then
                    Print #1, Tab(102); "  0.0";
                End if
                If homeScoreTeam!(K) + awayScoreTeam!(K) > 0 Then
                    Print #1, Tab(102); Using "###.#"; (homeScoreTeam!(K) + awayScoreTeam!(K)) / (homeWins!(K) + homeLosses!(K) + awayWins!(K) + awayLosses(K));
                End if
                If homeScoreOpp!(K) + awayScoreOpp!(K) = 0 Then
                    Print #1, Tab(109); "  0.0"
                End if
                If homeScoreOpp!(K) + awayScoreOpp!(K) > 0 Then
                    Print #1, Tab(109); Using "###.#"; (homeScoreOpp!(K) + awayScoreOpp!(K)) / (homeWins!(K) + homeLosses!(K) + awayWins!(K) + awayLosses(K))
                End if

                totHomeWin!(RDD) = totHomeWin!(RDD) + homeWins!(K)
                totHomeLosses!(RDD) = totHomeLosses!(RDD) + homeLosses!(K)
                totAwayWin!(RDD) = totAwayWin!(RDD) + awayWins!(K)
                totAwayLosses!(RDD) = totAwayLosses!(RDD) + awayLosses(K)
                totHomeScoreTeam!(RDD) = totHomeScoreTeam!(RDD) + homeScoreTeam!(K)
                totHomeScoreOpp!(RDD) = totHomeScoreOpp!(RDD) + homeScoreOpp!(K)
                totAwayScoreTeam!(RDD) = totAwayScoreTeam!(RDD) + awayScoreTeam!(K)
                totAwayScoreOpp!(RDD) = totAwayScoreOpp!(RDD) + awayScoreOpp!(K)
            End If
        Next K

        Print #1, Tab(5); "TOTALS";
        Print #1, Tab(21); Using "##"; totHomeWin!(RDD);
        Print #1, Tab(25); Using "##"; totHomeLosses!(RDD);

        If totHomeWin!(RDD) = totHomeWin!(RDD) + totHomeLosses!(RDD) And totHomeWin!(RDD) <> 0 Then 
            Print #1, Tab(28); "1.000";
        End If
        If totHomeWin!(RDD) = 0 Then 
            Print #1, Tab(29); ".000";
        End If
        If totHomeWin!(RDD) > 0 And totHomeWin!(RDD) < totHomeWin!(RDD) + totHomeLosses!(RDD) Then 
            Print #1, Tab(29); Using ".###"; (totHomeWin!(RDD) / (totHomeWin!(RDD) + totHomeLosses!(RDD)));
        End If
        If totHomeScoreTeam!(RDD) = 0 Then 
            Print #1, Tab(35); "  0.0";
        End If
        If totHomeScoreTeam!(RDD) > 0 Then 
            Print #1, Tab(35); Using "###.#"; totHomeScoreTeam!(RDD) / (totHomeWin!(RDD) + totHomeLosses!(RDD));
        End If
        If totHomeScoreOpp!(RDD) = 0 Then 
            Print #1, Tab(42); "  0.0";
        End If
        If totHomeScoreOpp!(RDD) > 0 Then 
            Print #1, Tab(42); Using "###.#"; totHomeScoreOpp!(RDD) / (totHomeWin!(RDD) + totHomeLosses!(RDD));
        End If

        Print #1, Tab(53); Using "##"; totAwayWin!(RDD);
        Print #1, Tab(57); Using "##"; totAwayLosses!(RDD);

        If totAwayWin!(RDD) = totAwayWin!(RDD) + totAwayLosses!(RDD) And totAwayWin!(RDD) <> 0 Then 
            Print #1, Tab(60); "1.000";
        End If
        If totAwayWin!(RDD) = 0 Then 
            Print #1, Tab(61); ".000";
        End If
        If totAwayWin!(RDD) > 0 And totAwayWin!(RDD) < totAwayWin!(RDD) + totAwayLosses!(RDD) Then 
            Print #1, Tab(61); Using ".###"; (totAwayWin!(RDD) / (totAwayWin!(RDD) + totAwayLosses!(RDD)));
        End If
        If totAwayScoreTeam!(RDD) = 0 Then 
            Print #1, Tab(67); "  0.0";
        End If
        If totAwayScoreTeam!(RDD) > 0 Then 
            Print #1, Tab(67); Using "###.#"; totAwayScoreTeam!(RDD) / (totAwayWin!(RDD) + totAwayLosses!(RDD));
        End If
        If totAwayScoreOpp!(RDD) = 0 Then 
            Print #1, Tab(74); "  0.0";
        End If
        If totAwayScoreOpp!(RDD) > 0 Then 
            Print #1, Tab(74); Using "###.#"; totAwayScoreOpp!(RDD) / (totAwayWin!(RDD) + totAwayLosses!(RDD));
        End If

        Print #1, Tab(84); Using "###"; totHomeWin!(RDD) + totAwayWin!(RDD);
        Print #1, Tab(89); Using "###"; totHomeLosses!(RDD) + totAwayLosses!(RDD);

        If totHomeWin!(RDD) + totAwayWin!(RDD) = totHomeWin!(RDD) + totAwayWin!(RDD) + totHomeLosses!(RDD) + totAwayLosses!(RDD) And totHomeWin!(RDD) + totAwayWin!(RDD) <> 0 Then
            Print #1, Tab(95); "1.000";
        End If
        If totHomeWin!(RDD) + totAwayWin!(RDD) = 0 Then 
            Print #1, Tab(96); ".000";
        End If
        If totHomeWin!(RDD) + totAwayWin!(RDD) > 0 And totHomeWin!(RDD) + totAwayWin!(RDD) < totHomeWin!(RDD) + totAwayWin!(RDD) + totHomeLosses!(RDD) + totAwayLosses!(RDD) Then
            Print #1, Tab(96); Using ".###"; ((totHomeWin!(RDD) + totAwayWin!(RDD)) / (totHomeWin!(RDD) + totAwayWin!(RDD) + totHomeLosses!(RDD) + totAwayLosses!(RDD)));
        End If
        If totHomeScoreTeam!(RDD) + totAwayScoreTeam!(RDD) = 0 Then 
            Print #1, Tab(102); "  0.0";
        End If
        If totHomeScoreTeam!(RDD) + totAwayScoreTeam!(RDD) > 0 Then 
            Print #1, Tab(102); Using "###.#"; (totHomeScoreTeam!(RDD) + totAwayScoreTeam!(RDD)) / (totHomeWin!(RDD) + totHomeLosses!(RDD) + totAwayWin!(RDD) + totAwayLosses!(RDD));
        End If
        If totHomeScoreOpp!(RDD) + totAwayScoreOpp!(RDD) = 0 Then 
            Print #1, Tab(109); "  0.0"
        End If
        If totHomeScoreOpp!(RDD) + totAwayScoreOpp!(RDD) > 0 Then 
            Print #1, Tab(109); Using "###.#"; (totHomeScoreOpp!(RDD) + totAwayScoreOpp!(RDD)) / (totHomeWin!(RDD) + totHomeLosses!(RDD) + totAwayWin!(RDD) + totAwayLosses!(RDD))
        End If

        Print #1,

        div = div + 1

    Loop Until div > DIVS_PER_LEAGUE Or DV$(div) = "XXX" Or DV$(div) = ""

    Print #1,
    Print #1,
    Print #1, Tab(22); "--------- HOME ----------"; Tab(54); "--------- AWAY ----------"; Tab(86); "---------- TOTALS ----------"
    Print #1, Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "PF/G"; Tab(43); "PA/G"; Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "PF/G"; Tab(75); "PA/G"; Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "PF/G"; Tab(110); "PA/G"
    Print #1,

    For W = 1 To DIVS_PER_LEAGUE

        If DV$(W) <> "XXX" Then

            Print #1, Tab(2); DV$(W);
            Print #1, Tab(21); Using "##"; totHomeWin!(W);
            Print #1, Tab(25); Using "##"; totHomeLosses!(W);

            If totHomeWin!(W) = totHomeWin!(W) + totHomeLosses!(W) And totHomeWin!(W) <> 0 Then 
                Print #1, Tab(28); "1.000";
            End If
            If totHomeWin!(W) = 0 Then 
                Print #1, Tab(29); ".000";
            End If
            If totHomeWin!(W) > 0 And totHomeWin!(W) < totHomeWin!(W) + totHomeLosses!(W) Then 
                Print #1, Tab(29); Using ".###"; (totHomeWin!(W) / (totHomeWin!(W) + totHomeLosses!(W)));
            End If
            If totHomeScoreTeam!(W) = 0 Then 
                Print #1, Tab(35); "  0.0";
            End If
            If totHomeScoreTeam!(W) > 0 Then 
                Print #1, Tab(35); Using "###.#"; totHomeScoreTeam!(W) / (totHomeWin!(W) + totHomeLosses!(W));
            End If
            If totHomeScoreOpp!(W) = 0 Then 
                Print #1, Tab(42); "  0.0";
            End If
            If totHomeScoreOpp!(W) > 0 Then 
                Print #1, Tab(42); Using "###.#"; totHomeScoreOpp!(W) / (totHomeWin!(W) + totHomeLosses!(W));
            End If

            Print #1, Tab(53); Using "##"; totAwayWin!(W);
            Print #1, Tab(57); Using "##"; totAwayLosses!(W);

            If totAwayWin!(W) = totAwayWin!(W) + totAwayLosses!(W) And totAwayWin!(W) <> 0 Then 
                Print #1, Tab(60); "1.000";
            End If
            If totAwayWin!(W) = 0 Then 
                Print #1, Tab(61); ".000";
            End If
            If totAwayWin!(W) > 0 And totAwayWin!(W) < totAwayWin!(W) + totAwayLosses!(W) Then 
                Print #1, Tab(61); Using ".###"; (totAwayWin!(W) / (totAwayWin!(W) + totAwayLosses!(W)));
            End If
            If totAwayScoreTeam!(W) = 0 Then 
                Print #1, Tab(67); "  0.0";
            End If
            If totAwayScoreTeam!(W) > 0 Then
                Print #1, Tab(67); Using "###.#"; totAwayScoreTeam!(W) / (totAwayWin!(W) + totAwayLosses!(W));
            End If
            If totAwayScoreOpp!(W) = 0 Then 
                Print #1, Tab(74); "  0.0";
            End If
            If totAwayScoreOpp!(W) > 0 Then 
                Print #1, Tab(74); Using "###.#"; totAwayScoreOpp!(W) / (totAwayWin!(W) + totAwayLosses!(W));
            End If

            Print #1, Tab(84); Using "###"; totHomeWin!(W) + totAwayWin!(W);
            Print #1, Tab(89); Using "###"; totHomeLosses!(W) + totAwayLosses!(W);

            If totHomeWin!(W) + totAwayWin!(W) = totHomeWin!(W) + totAwayWin!(W) + totHomeLosses!(W) + totAwayLosses!(W) And totHomeWin!(W) + totAwayWin!(W) <> 0 Then 
                Print #1, Tab(95); "1.000";
            End If
            If (totHomeWin!(W) + totAwayWin!(W)) = 0 Then 
                Print #1, Tab(96); ".000";
            End If
            If (totHomeWin!(W) + totAwayWin!(W)) > 0 And totHomeWin!(W) + totAwayWin!(W) < totHomeWin!(W) + totAwayWin!(W) + totHomeLosses!(W) + totAwayLosses!(W) Then 
                Print #1, Tab(96); Using ".###"; ((totHomeWin!(W) + totAwayWin!(W)) / (totHomeWin!(W) + totAwayWin!(W) + totHomeLosses!(W) + totAwayLosses!(W)));
            End If
            If totHomeScoreTeam!(W) + totAwayScoreTeam!(W) = 0 Then 
                Print #1, Tab(102); "  0.0";
            End If
            If totHomeScoreTeam!(W) + totAwayScoreTeam!(W) > 0 Then 
                Print #1, Tab(102); Using "###.#"; (totHomeScoreTeam!(W) + totAwayScoreTeam!(W)) / (totHomeWin!(W) + totHomeLosses!(W) + totAwayWin!(W) + totAwayLosses!(W));
            End If
            If totHomeScoreOpp!(W) + totAwayScoreOpp!(W) = 0 Then 
                Print #1, Tab(109); "  0.0";
            End If
            If totHomeScoreOpp!(W) + totAwayScoreOpp!(W) > 0 Then 
                Print #1, Tab(109); Using "###.#"; (totHomeScoreOpp!(W) + totAwayScoreOpp!(W)) / (totHomeWin!(W) + totHomeLosses!(W) + totAwayWin!(W) + totAwayLosses!(W))
            End If

        End If

    Next W

    TtotHomeWins = 0
    TtotHomeLosses = 0
    TAteamTotWins = 0
    TAll = 0

    For I = 1 To DIVS_PER_LEAGUE
        TtotHomeWins = TtotHomeWins + totHomeWin!(I)
        TtotHomeLosses = TtotHomeLosses + totHomeLosses!(I)
        TAteamTotWins = TAteamTotWins + totAwayWin!(I)
        TAll = TAll + totAwayLosses!(I)
    Next

    TR11 = 0
    TR22 = 0
    TR33 = 0
    TR44 = 0

    For I = 1 To DIVS_PER_LEAGUE
        TR11 = TR11 + totHomeScoreTeam!(I)
        TR22 = TR22 + totHomeScoreOpp!(I)
        TR33 = TR33 + totAwayScoreTeam!(I)
        TR44 = TR44 + totAwayScoreOpp!(I)
    Next

    Print #1, Tab(5); "TOTALS";
    Print #1, Tab(20); Using "###"; TtotHomeWins;
    Print #1, Tab(24); Using "###"; TtotHomeLosses;

    If TtotHomeWins = TtotHomeWins + TtotHomeLosses And TtotHomeWins <> 0 Then 
        Print #1, Tab(28); "1.000";
    End If
    If TtotHomeWins = 0 Then 
        Print #1, Tab(29); ".000";
    End If
    If TtotHomeWins > 0 And TtotHomeWins < TtotHomeWins + TtotHomeLosses Then 
        statPCT! = TtotHomeWins / (TtotHomeWins + TtotHomeLosses)
        Print #1, Tab(29); Using ".###"; statPCT!;
    End If

    If TR11 = 0 Then 
        Print #1, Tab(35); "  0.0";
    End If
    If TR11 > 0 Then 
        Print #1, Tab(35); Using "###.#"; TR11 / (TtotHomeWins + TtotHomeLosses);
    End If
    If TR22 = 0 Then 
        Print #1, Tab(42); "  0.0";
    End If
    If TR22 > 0 Then 
        Print #1, Tab(42); Using "###.#"; TR22 / (TtotHomeWins + TtotHomeLosses);
    End If

    Print #1, Tab(52); Using "###"; TAteamTotWins;
    Print #1, Tab(56); Using "###"; TAll;

    If TAteamTotWins = TAteamTotWins + TAll And TAteamTotWins <> 0 Then 
        Print #1, Tab(60); "1.000";
    End If
    If TAteamTotWins = 0 Then 
        Print #1, Tab(61); ".000";
    End If
    If TAteamTotWins > 0 And TAteamTotWins < TAteamTotWins + TAll Then 
        statPCT! = TAteamTotWins / (TAteamTotWins + TAll)
        Print #1, Tab(61); Using ".###"; statPCT!;
    End If
    
    If TR33 = 0 Then 
        Print #1, Tab(67); "  0.0";
    End If
    If TR33 > 0 Then 
        Print #1, Tab(67); Using "###.#"; TR33 / (TAteamTotWins + TAll);
    End If
    If TR44 = 0 Then 
        Print #1, Tab(74); "  0.0";
    End If
    If TR44 > 0 Then 
        Print #1, Tab(74); Using "###.#"; TR44 / (TAteamTotWins + TAll);
    End If

    Print #1, Tab(84); Using "###"; TtotHomeWins + TAteamTotWins;
    Print #1, Tab(89); Using "###"; TtotHomeLosses + TAll;

    If TtotHomeWins + TAteamTotWins = TtotHomeWins + TAteamTotWins + TtotHomeLosses + TAll And TtotHomeWins + TAteamTotWins <> 0 Then 
        Print #1, Tab(95); "1.000";
    End If
    If TtotHomeWins + TAteamTotWins = 0 Then 
        Print #1, Tab(96); ".000";
    End If
    If TtotHomeWins + TAteamTotWins > 0 And TtotHomeWins + TAteamTotWins < TtotHomeWins + TAteamTotWins + TtotHomeLosses + TAll Then 
        statPCT! = (TtotHomeWins + TAteamTotWins) / (TtotHomeWins + TAteamTotWins + TtotHomeLosses + TAll)
        Print #1, Tab(96); Using ".###"; statPCT!;
    End If

    If TR11 + TR33 = 0 Then 
        Print #1, Tab(102); "  0.0";
    End If
    If TR11 + TR33 > 0 Then 
        Print #1, Tab(102); Using "###.#"; (TR11 + TR33) / (TtotHomeWins + TtotHomeLosses + TAteamTotWins + TAll);
    End If
    If TR22 + TR44 = 0 Then 
        Print #1, Tab(109); "  0.0";
    End If
    If TR22 + TR44 > 0 Then 
        Print #1, Tab(109); Using "###.#"; (TR22 + TR44) / (TtotHomeWins + TtotHomeLosses + TAteamTotWins + TAll)
    End If

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


'----------------------------------------
'      Hd2HdPro_Screen Subroutine
'----------------------------------------
'This subroutine outputs the calculated
'comparison values to the end user display
Sub Hd2HdPro_Screen (teamName$)

    Shared teamRatings()

    Shared awayLosses(), awayWins!(), homeLosses!(), homeWins!()
    Shared homeScoreTeam!(), homeScoreOpp!(), awayScoreTeam!(), awayScoreOpp!()
    Shared totAwayLosses!(), totAwayWin!(), totHomeLosses!(), totHomeWin!()
    Shared totHomeScoreTeam!(), totHomeScoreOpp!(), totAwayScoreTeam!(), totAwayScoreOpp!()

    Shared memberName$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    Dim DV$(1 To 4)

    Color 15, 0

    DV$(1) = div1Name$
    DV$(2) = div2Name$
    DV$(3) = div3Name$
    DV$(4) = div4Name$

    div = 0

    Do

        RDD = div + 1

        Cls

        Locate 2, 35
        Color teamRatings(33), teamRatings(34): Print teamName$
        Color 3, 0

        'Table header
        Locate 4
        Print Tab(15); String$(7, 196); " HOME "; String$(8, 196); Tab(36); Chr$(194); String$(7, 196); " AWAY "; String$(8, 196); Tab(58); Chr$(194); String$(6, 196); " TOTALS "; String$(7, 196)
        Locate 5
        Print Tab(15); "W"; Tab(17); "L"; Tab(21); "PCT."; Tab(26); "PF/G"; Tab(32); "PA/G"; Tab(36); Chr$(179); Tab(37); "W"; Tab(39); "L"; Tab(43); "PCT."; Tab(48); "PF/G"; Tab(54); "PA/G"; Tab(58); Chr$(179); Tab(59); "W"; Tab(61); "L"; Tab(65); "PCT."; Tab(70); "PF/G"; Tab(76); "PA/G"

        DD = ((RDD - 1) * TEAMS_PER_DIV) + 1
        EE = RDD * TEAMS_PER_DIV

        'If DV$(RDD) <> "XXX" And DV$(RDD) <> "" Then
        Color 11
        Print Left$(DV$(RDD), 15);
        'End If

        Color 3: Print Tab(36); Chr$(179); Tab(58); Chr$(179)

        For K = DD To EE

            If memberName$(K) <> "XXX" And memberName$(K) <> teamName$ And memberName$(K) <> "*" Then

                Color 15
                Locate , 1: Print Left$(memberName$(K), 13);

                Color 7
                Locate , 15: Print Using "#"; homeWins!(K);
                Locate , 17: Print Using "#"; homeLosses!(K);

                If homeWins!(K) = homeWins!(K) + homeLosses!(K) And homeWins!(K) <> 0 Then 
                    Locate , 19: Print "1.000";
                End If
                If homeWins!(K) = 0 Then 
                    Locate , 20: Print ".000";
                End If
                If homeWins!(K) > 0 And homeWins!(K) < homeWins!(K) + homeLosses!(K) Then 
                    statPCT! = homeWins!(K) / (homeWins!(K) + homeLosses!(K))
                    Locate , 20: Print Using ".###"; statPCT!;
                End If
                
                If homeScoreTeam!(K) = 0 Then 
                    Locate , 25: Print "  0.0";
                End If
                If homeScoreTeam!(K) > 0 Then 
                    Locate , 25: Print Using "###.#"; homeScoreTeam!(K) / (homeWins!(K) + homeLosses!(K));
                End If
                If homeScoreOpp!(K) = 0 Then 
                    Locate , 31: Print "  0.0";
                End If
                If homeScoreOpp!(K) > 0 Then 
                    Locate , 31: Print Using "###.#"; homeScoreOpp!(K) / (homeWins!(K) + homeLosses!(K));
                End If

                Color 3: Print Tab(36); Chr$(179);
                Color 7
                Locate , 37: Print Using "#"; awayWins!(K);
                Locate , 39: Print Using "#";awayLosses(K);

                If awayWins!(K) = awayWins!(K) + awayLosses(K) And awayWins!(K) <> 0 Then 
                    Locate , 41: Print "1.000";
                End If
                If awayWins!(K) = 0 Then 
                    Locate , 42: Print ".000";
                End If
                If awayWins!(K) > 0 And awayWins!(K) < awayWins!(K) + awayLosses(K) Then 
                    statPCT! = (awayWins!(K) / (awayWins!(K) + awayLosses(K)))
                    Locate , 42: Print Using ".###"; statPCT!;
                End If

                If awayScoreTeam!(K) = 0 Then 
                    Locate , 47: Print "  0.0";
                End If
                If awayScoreTeam!(K) > 0 Then 
                    Locate , 47: Print Using "###.#"; awayScoreTeam!(K) / (awayWins!(K) + awayLosses(K));
                End If
                If awayScoreOpp!(K) = 0 Then 
                    Locate , 53: Print "  0.0";
                End If
                If awayScoreOpp!(K) > 0 Then 
                    Locate , 53: Print Using "###.#"; awayScoreOpp!(K) / (awayWins!(K) + awayLosses(K));
                End If

                Color 3: Print Tab(58); Chr$(179);
                Color 7
                Locate , 59: Print Using "##"; homeWins!(K) + awayWins!(K);
                Locate , 61: Print Using "##"; homeLosses!(K) + awayLosses(K);

                If (homeWins!(K) + awayWins!(K)) = (homeWins!(K) + awayWins!(K) + homeLosses!(K) + awayLosses(K)) And (homeWins!(K) + awayWins!(K)) <> 0 Then
                    Locate , 63: Print "1.000";
                End If
                If (homeWins!(K) + awayWins!(K)) = 0 Then 
                    Locate , 64: Print ".000";
                End If
                If (homeWins!(K) + awayWins!(K)) > 0 And (homeWins!(K) + awayWins!(K)) < (homeWins!(K) + awayWins!(K) + homeLosses!(K) + awayLosses(K)) Then
                    statPCT! = (homeWins!(K) + awayWins!(K)) / (homeWins!(K) + awayWins!(K) + homeLosses!(K) + awayLosses(K))
                    Locate , 64: Print Using ".###"; statPCT!;
                End If

                If homeScoreTeam!(K) + awayScoreTeam!(K) = 0 Then 
                    Locate , 69: Print "  0.0";
                End If
                If homeScoreTeam!(K) + awayScoreTeam!(K) > 0 Then 
                    Locate , 69: Print Using "###.#"; (homeScoreTeam!(K) + awayScoreTeam!(K)) / (homeWins!(K) + homeLosses!(K) + awayWins!(K) + awayLosses(K));
                End If
                If homeScoreOpp!(K) + awayScoreOpp!(K) = 0 Then 
                    Locate , 75: Print "  0.0"
                End If
                If homeScoreOpp!(K) + awayScoreOpp!(K) > 0 Then 
                    Locate , 75: Print Using "###.#"; (homeScoreOpp!(K) + awayScoreOpp!(K)) / (homeWins!(K) + homeLosses!(K) + awayWins!(K) + awayLosses(K))
                End If

                Color 3: Print Tab(36); Chr$(179); Tab(58); Chr$(179);

                totHomeWin!(RDD) = totHomeWin!(RDD) + homeWins!(K)
                totHomeLosses!(RDD) = totHomeLosses!(RDD) + homeLosses!(K)
                totAwayWin!(RDD) = totAwayWin!(RDD) + awayWins!(K)
                totAwayLosses!(RDD) = totAwayLosses!(RDD) + awayLosses(K)
                totHomeScoreTeam!(RDD) = totHomeScoreTeam!(RDD) + homeScoreTeam!(K)
                totHomeScoreOpp!(RDD) = totHomeScoreOpp!(RDD) + homeScoreOpp!(K)
                totAwayScoreTeam!(RDD) = totAwayScoreTeam!(RDD) + awayScoreTeam!(K)
                totAwayScoreOpp!(RDD) = totAwayScoreOpp!(RDD) + awayScoreOpp!(K)

            End If

        Next K

        'Division Team Totals
        Print

        Color 11
        Locate , 14: Print Using "##"; totHomeWin!(RDD);

        If totHomeWin!(RDD) = totHomeWin!(RDD) + totHomeLosses!(RDD) And totHomeWin!(RDD) <> 0 Then 
            Locate , 19: Print "1.000";
        End If
        If totHomeWin!(RDD) = 0 Then 
            Locate , 20: Print ".000";
        End If
        If totHomeWin!(RDD) > 0 And totHomeWin!(RDD) < totHomeWin!(RDD) + totHomeLosses!(RDD) Then 
            statPCT! = (totHomeWin!(RDD) / (totHomeWin!(RDD) + totHomeLosses!(RDD)))
            Locate , 20: Print Using ".###"; statPCT!;
        End If

        If totHomeScoreOpp!(RDD) < 1 Then 
            Locate , 32: Print " 0.0";
        End If
        If totHomeScoreOpp!(RDD) > 0 Then 
            Locate , 31: Print Using "###.#"; totHomeScoreOpp!(RDD) / (totHomeWin!(RDD) + totHomeLosses!(RDD));
        End If

        Locate , 38: Print Using "##"; totAwayLosses!(RDD);

        If totAwayScoreTeam!(RDD) < 1 Then 
            Locate , 48: Print " 0.0";
        End If
        If totAwayScoreTeam!(RDD) > 0 Then 
            Locate , 47: Print Using "###.#"; totAwayScoreTeam!(RDD) / (totAwayWin!(RDD) + totAwayLosses!(RDD));
        End If

        Locate , 57: Print Using "###"; totHomeWin!(RDD) + totAwayWin!(RDD);
        Color 11

        If totHomeWin!(RDD) + totAwayWin!(RDD) = totHomeWin!(RDD) + totHomeLosses!(RDD) + totAwayWin!(RDD) + totAwayLosses!(RDD) And totHomeWin!(RDD) + totAwayWin!(RDD) <> 0 Then
            Locate , 63: Print "1.000";
        End If
        If totHomeWin!(RDD) + totAwayWin!(RDD) = 0 Then 
            Locate , 64: Print ".000";
        End If
        If totHomeWin!(RDD) + totAwayWin!(RDD) > 0 And totHomeWin!(RDD) + totAwayWin!(RDD) < totHomeWin!(RDD) + totAwayWin!(RDD) + totHomeLosses!(RDD) + totAwayLosses!(RDD) Then 
            statPCT! = (totHomeWin!(RDD) + totAwayWin!(RDD)) / (totHomeWin!(RDD) + totAwayWin!(RDD) + totHomeLosses!(RDD) + totAwayLosses!(RDD))
            '---Why duplicate here? it's okay to end the line?
            Locate , 64: Print Using ".###"; statPCT!;
            Locate , 64: Print Using ".###"; statPCT!
        End If

        If totHomeScoreOpp!(RDD) + totAwayScoreOpp!(RDD) < 1 Then 
            Locate , 76: Print " 0.0"
        End If
        If totHomeScoreOpp!(RDD) + totAwayScoreOpp!(RDD) > 0 Then 
            Locate , 75: Print Using "###.#"; (totHomeScoreOpp!(RDD) + totAwayScoreOpp!(RDD)) / (totHomeWin!(RDD) + totHomeLosses!(RDD) + totAwayWin!(RDD) + totAwayLosses!(RDD))
        End If

        Locate , 5: Print "TOTALS";

        If totHomeScoreTeam!(RDD) < 1 Then 
            Locate , 26: Print " 0.0";
        End If
        If totHomeScoreTeam!(RDD) > 0 Then 
            Locate , 25: Print Using "###.#"; totHomeScoreTeam!(RDD) / (totHomeWin!(RDD) + totHomeLosses!(RDD));
        End If

        Color 11

        If totAwayWin!(RDD) = totAwayWin!(RDD) + totAwayLosses!(RDD) And totAwayWin!(RDD) <> 0 Then 
            Locate , 41: Print "1.000";
        End If
        If totAwayWin!(RDD) = 0 Then 
            Locate , 42: Print ".000";
        End If
        If totAwayWin!(RDD) > 0 And totAwayWin!(RDD) < totAwayWin!(RDD) + totAwayLosses!(RDD) Then 
            statPCT! = (totAwayWin!(RDD) / (totAwayWin!(RDD) + totAwayLosses!(RDD)))
            Locate , 42: Print Using ".###"; statPCT!;
        End If

        If totAwayScoreOpp!(RDD) < 1 Then 
            Locate , 54: Print " 0.0";
        End If
        If totAwayScoreOpp!(RDD) > 0 Then 
            Locate , 53: Print Using "###.#"; totAwayScoreOpp!(RDD) / (totAwayWin!(RDD) + totAwayLosses!(RDD));
        End If
        If totHomeScoreTeam!(RDD) + totAwayScoreTeam!(RDD) = 0 Then 
            Locate , 70: Print " 0.0";
        End If
        If totHomeScoreTeam!(RDD) + totAwayScoreTeam!(RDD) > 0 Then 
            Locate , 69: Print Using "###.#"; (totHomeScoreTeam!(RDD) + totAwayScoreTeam!(RDD)) / (totHomeWin!(RDD) + totHomeLosses!(RDD) + totAwayWin!(RDD) + totAwayLosses!(RDD));
        End If

        Locate , 16: Print Using "##"; totHomeLosses!(RDD);
        Locate , 36: Print Using "##"; totAwayWin!(RDD);
        Locate , 59: Print Using "###"; totHomeLosses!(RDD) + totAwayLosses!(RDD)

        Color 14
        Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"

        I$ = GetKeyPress$

        div = div + 1

    Loop Until div > DIVS_PER_LEAGUE Or DV$(div) = "XXX" Or DV$(div) = ""

    Cls

    Locate 2, 35
    Color teamRatings(33), teamRatings(34): Print teamName$
    Color 3, 0

    Locate 4
    Print Tab(9); String$(8, 196); " HOME "; String$(9, 196); Tab(32); Chr$(194); String$(8, 196); " AWAY "; String$(9, 196); Tab(56); Chr$(194); String$(7, 196); " TOTALS "; String$(8, 196)
    Locate 5
    Print Tab(10); "W"; Tab(13); "L"; Tab(17); "PCT."; Tab(22); "PF/G"; Tab(28); "PA/G"; Tab(32); Chr$(179); Tab(34); "W"; Tab(37); "L"; Tab(41); "PCT."; Tab(46); "PF/G"; Tab(52); "PA/G"; Tab(56); Chr$(179); Tab(58); "W"; Tab(61); "L"; Tab(65); "PCT."; Tab(70); "PF/G"; Tab(76); "PA/G"

    Color 3: Print Tab(32); Chr$(179); Tab(56); Chr$(179)

    For W = 1 To DIVS_PER_LEAGUE

        If DV$(W) <> "XXX" Then

            Color 15

            'We have to limit this to 8 because we start stats on column 0
            Locate , 1: Print Left$(DV$(W), 8);

            Color 7
            Locate , 9: Print Using "##"; totHomeWin!(W);
            Locate , 12: Print Using "##"; totHomeLosses!(W);

            If totHomeWin!(W) = totHomeWin!(W) + totHomeLosses!(W) And totHomeWin!(W) <> 0 Then 
                Locate , 15: Print "1.000";
            End If
            If totHomeWin!(W) = 0 Then 
                Locate , 16: Print ".000";
            End If
            If totHomeWin!(W) > 0 And totHomeWin!(W) < totHomeWin!(W) + totHomeLosses!(W) Then 
                statPCT! = (totHomeWin!(W) / (totHomeWin!(W) + totHomeLosses!(W)))
                Locate , 16: Print Using ".###"; statPCT!;
            End If
            
            If totHomeScoreTeam!(W) = 0 Then 
                Locate , 21: Print "  0.0";
            End If
            If totHomeScoreTeam!(W) > 0 Then 
                Locate , 21: Print Using "###.#"; totHomeScoreTeam!(W) / (totHomeWin!(W) + totHomeLosses!(W));
            End If
            If totHomeScoreOpp!(W) = 0 Then 
                Locate , 27: Print "  0.0";
            End If
            If totHomeScoreOpp!(W) > 0 Then 
                Locate , 27: Print Using "###.#"; totHomeScoreOpp!(W) / (totHomeWin!(W) + totHomeLosses!(W));
            End If

            Color 3: Print Tab(32); Chr$(179);

            Color 7
            Locate , 33: Print Using "##"; totAwayWin!(W);
            Locate , 36: Print Using "##"; totAwayLosses!(W);

            If totAwayWin!(W) = totAwayWin!(W) + totAwayLosses!(W) And totAwayWin!(W) <> 0 Then 
                Locate , 39: Print "1.000";
            End If
            If totAwayWin!(W) = 0 Then 
                Locate , 40: Print ".000";
            End If
            If totAwayWin!(W) > 0 And totAwayWin!(W) < totAwayWin!(W) + totAwayLosses!(W) Then 
                statPCT! = (totAwayWin!(W) / (totAwayWin!(W) + totAwayLosses!(W)))
                Locate , 40: Print Using ".###"; statPCT!;
            End If

            If totAwayScoreTeam!(W) = 0 Then 
                Locate , 45: Print "  0.0";
            End If
            If totAwayScoreTeam!(W) > 0 Then 
                Locate , 45: Print Using "###.#"; totAwayScoreTeam!(W) / (totAwayWin!(W) + totAwayLosses!(W));
            End If
            If totAwayScoreOpp!(W) = 0 Then 
                Locate , 51: Print "  0.0";
            End If
            If totAwayScoreOpp!(W) > 0 Then 
                Locate , 51: Print Using "###.#"; totAwayScoreOpp!(W) / (totAwayWin!(W) + totAwayLosses!(W));
            End If

            Color 3: Print Tab(56); Chr$(179);

            Color 7
            Locate , 57: Print Using "##"; totHomeWin!(W) + totAwayWin!(W);
            Locate , 60: Print Using "##"; totHomeLosses!(W) + totAwayLosses!(W);

            If totHomeWin!(W) + totAwayWin!(W) = totHomeWin!(W) + totAwayWin!(W) + totHomeLosses!(W) + totAwayLosses!(W) And totHomeWin!(W) + totAwayWin!(W) <> 0 Then 
                Locate , 63: Print "1.000";
            End If
            If (totHomeWin!(W) + totAwayWin!(W)) = 0 Then 
                Locate , 64: Print ".000";
            End If
            If (totHomeWin!(W) + totAwayWin!(W)) > 0 And totHomeWin!(W) + totAwayWin!(W) < totHomeWin!(W) + totAwayWin!(W) + totHomeLosses!(W) + totAwayLosses!(W) Then 
                statPCT! = ((totHomeWin!(W) + totAwayWin!(W)) / (totHomeWin!(W) + totAwayWin!(W) + totHomeLosses!(W) + totAwayLosses!(W)))
                Locate , 64: Print Using ".###"; statPCT!;
            End If

            If totHomeScoreTeam!(W) + totAwayScoreTeam!(W) = 0 Then 
                Locate , 69: Print "  0.0";
            End If
            If totHomeScoreTeam!(W) + totAwayScoreTeam!(W) > 0 Then 
                Locate , 69: Print Using "###.#"; (totHomeScoreTeam!(W) + totAwayScoreTeam!(W)) / (totHomeWin!(W) + totHomeLosses!(W) + totAwayWin!(W) + totAwayLosses!(W));
            End If

            If totHomeScoreOpp!(W) + totAwayScoreOpp!(W) = 0 Then 
                Locate , 75: Print "  0.0"
            End If
            If totHomeScoreOpp!(W) + totAwayScoreOpp!(W) > 0 Then 
                Locate , 75: Print Using "###.#"; (totHomeScoreOpp!(W) + totAwayScoreOpp!(W)) / (totHomeWin!(W) + totHomeLosses!(W) + totAwayWin!(W) + totAwayLosses!(W))
            End If

            Color 3: Print Tab(32); Chr$(179); Tab(56); Chr$(179);

        End If

    Next W

    'Calculate TOTALS for Division
    TtotHomeWins = 0
    TtotHomeLosses = 0
    TAteamTotWins = 0
    TAll = 0

    For I = 1 To DIVS_PER_LEAGUE
        TtotHomeWins = TtotHomeWins + totHomeWin!(I)
        TtotHomeLosses = TtotHomeLosses + totHomeLosses!(I)
        TAteamTotWins = TAteamTotWins + totAwayWin!(I)
        TAll = TAll + totAwayLosses!(I)
    Next

    TR11 = 0
    TR22 = 0
    TR33 = 0
    TR44 = 0

    For I = 1 To DIVS_PER_LEAGUE
        TR11 = TR11 + totHomeScoreTeam!(I)
        TR22 = TR22 + totHomeScoreOpp!(I)
        TR33 = TR33 + totAwayScoreTeam!(I)
        TR44 = TR44 + totAwayScoreOpp!(I)
    Next

    'Division Totals
    Color 11
    Print "TOTALS";

    Locate , 9: Print Using "##"; TtotHomeWins;
    Locate , 12: Print Using "##"; TtotHomeLosses;

    If TtotHomeWins = TtotHomeWins + TtotHomeLosses And TtotHomeWins <> 0 Then 
        Locate , 15: Print "1.000";
    End If
    If TtotHomeWins = 0 Then 
        Locate , 16: Print ".000";
    End If
    If TtotHomeWins > 0 And TtotHomeWins < TtotHomeWins + TtotHomeLosses Then 
        Locate , 16: Print Using ".###"; TtotHomeWins / (TtotHomeWins + TtotHomeLosses);
    End If

    If TR11 = 0 Then 
        Locate , 21: Print "  0.0";
    End If
    If TR11 > 0 Then 
        Locate , 21: Print Using "###.#"; TR11 / (TtotHomeWins + TtotHomeLosses);
    End If
    If TR22 = 0 Then 
        Locate , 27: Print "  0.0";
    End If
    If TR22 > 0 Then 
        Locate , 27: Print Using "###.#"; TR22 / (TtotHomeWins + TtotHomeLosses);
    End If

    Color 3: Print Tab(32); Chr$(179);
    Locate , 33
    Color 11: Print Using "##"; TAteamTotWins;
    Locate , 36
    Print Using "##"; TAll;

    If TAteamTotWins = TAteamTotWins + TAll And TAteamTotWins <> 0 Then 
        Locate , 39: Print "1.000";
    End If
    If TAteamTotWins = 0 Then 
        Locate , 40: Print ".000";
    End If
    If TAteamTotWins > 0 And TAteamTotWins < TAteamTotWins + TAll Then 
        statPCT! = TAteamTotWins / (TAteamTotWins + TAll)
        Locate , 40: Print Using ".###"; statPCT!;
    End If

    If TR33 = 0 Then 
        Locate , 45: Print "  0.0";
    End If
    If TR33 > 0 Then 
        Locate , 45: Print Using "###.#"; TR33 / (TAteamTotWins + TAll);
    End If
    If TR44 = 0 Then 
        Locate , 51: Print "  0.0";
    End If
    If TR44 > 0 Then 
        Locate , 51: Print Using "###.#"; TR44 / (TAteamTotWins + TAll);
    End If

    Color 3: Print Tab(56); Chr$(179);
    Locate , 57
    Color 11: Print Using "##"; TtotHomeWins + TAteamTotWins;
    Locate , 60
    Print Using "##"; TtotHomeLosses + TAll;

    If TtotHomeWins + TAteamTotWins = TtotHomeWins + TAteamTotWins + TtotHomeLosses + TAll And TtotHomeWins + TAteamTotWins <> 0 Then 
        Locate , 63: Print "1.000";
    End If
    If TtotHomeWins + TAteamTotWins = 0 Then 
        Locate , 64: Print ".000";
    End If
    If TtotHomeWins + TAteamTotWins > 0 And TtotHomeWins + TAteamTotWins < TtotHomeWins + TAteamTotWins + TtotHomeLosses + TAll Then 
        statPCT! = (TtotHomeWins + TAteamTotWins) / (TtotHomeWins + TAteamTotWins + TtotHomeLosses + TAll)
        Locate , 64: Print Using ".###"; statPCT!;
    End If

    If TR11 + TR33 = 0 Then 
        Locate , 69: Print "  0.0";
    End If
    If TR11 + TR33 > 0 Then 
        Locate , 69: Print Using "###.#"; (TR11 + TR33) / (TtotHomeWins + TtotHomeLosses + TAteamTotWins + TAll);
    End If
    If TR22 + TR44 = 0 Then 
        Locate , 75: Print "  0.0"
    End If
    If TR22 + TR44 > 0 Then 
        Locate , 75: Print Using "###.#"; (TR22 + TR44) / (TtotHomeWins + TtotHomeLosses + TAteamTotWins + TAll)
    End If

    I$ = GetKeyPress$

End Sub

