'----------------------------------------
'           COMPARE Subroutine
'----------------------------------------
'This subroutine loads in existing stats
'for a team and provides different options
'to the end user for formatting stats
Sub COMPARE ()

    _Title "Basketball: The Pro Game - Stats Comparison"

    Shared roadLosses, roadWins, homeLosses, homeWins

    Shared leagueRatings%(), teamRatings%()
    Shared teamScore(), oppScore()

    Shared compareA!(), statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()

    Shared statPlyrNames$()
    Shared playerNames$()

    Shared oppName$(), locIndicator$()

    Dim posn$(0 To 14)

    Do

        teamNbr = 0
        Call ChooseTeam(YN$, NTMS, teamNbr, team$)

        If teamNbr <> 0 Then

            haveStats = 1

            statFile$ = nameStatFile$(team$)

            If Not _FileExists(diskPaths$(1) + statFile$) Then

                haveStats = 0

                MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
                BoxWidth = 24
                BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                Call CSMsgBox(MsgText$, BoxWidth, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)

            Else

                Open diskPaths$(1) + statFile$ For Input As #1

                Input #1, team$

                For I = 0 To 14

                    Input #1, statPlyrNames$(I)

                    For J = 0 To 13
                        Input #1, compareA!(I, J)
                    Next

                    Input #1, statsW0!(I)

                    Input #1, statsW1!(I)

                Next

                For L = 0 To 15:
                    Input #1, statsZ!(L)
                Next

                For L = 0 To 15:
                    Input #1, statsZ1!(L)
                Next

                Close #1

                For I = 0 To 14
                    If playerNames$(I, 1) = "F" Then posn$(I) = "FORWARD"
                    If playerNames$(I, 1) = "C" Then posn$(I) = "CENTER"
                    If playerNames$(I, 1) = "G" Then posn$(I) = "GUARD"
                    If playerNames$(I, 1) = "FC" Then posn$(I) = "FORWARD/GUARD"
                    If playerNames$(I, 1) = "FG" Then posn$(I) = "FORWARD/CENTER"
                    If playerNames$(I, 1) = "GF" Then posn$(I) = "GUARD/FORWARD"
                    If playerNames$(I, 1) = "CF" Then posn$(I) = "CENTER/FORWARD"
                Next

                Open diskPaths$(1) + statFile$ + "S" For Input As #2

                Input #2, Y

                For J = 1 To MAX_SCHED_STATS: 
                    Input #2, teamScore(J)
                Next

                For J = 1 To MAX_SCHED_STATS: 
                    Input #2, oppName$(J)
                Next

                For J = 1 To MAX_SCHED_STATS: 
                    Input #2, oppScore(J)
                Next

                For J = 1 To MAX_SCHED_STATS: 
                    Input #2, locIndicator$(J)
                Next

                Close #2

                homeWins = 0
                homeLosses = 0
                roadWins = 0
                roadLosses = 0

                For I = 1 To Y
                    If teamScore(I) < oppScore(I) And Left$(locIndicator$(I), 1) = "H" Then 
                        homeLosses = homeLosses + 1
                    End If
                    If teamScore(I) > oppScore(I) And Left$(locIndicator$(I), 1) = "H" Then 
                        homeWins = homeWins + 1
                    End If
                    If teamScore(I) < oppScore(I) And Left$(locIndicator$(I), 1) = "V" Then 
                        roadLosses = roadLosses + 1
                    End If
                    If teamScore(I) > oppScore(I) And Left$(locIndicator$(I), 1) = "V" Then 
                        roadWins = roadWins + 1
                    End If
                Next

                Color 15, 0
                Cls

                Call MenuBox(8, 25, 24, 5, 15, 1)

                Do

                    Locate 10
                    Color 7, 1

                    Locate , 32: Print "SEND TO ";
                    Color 15: Print "F";
                    Color 7: Print "ILE"
                    Locate , 32: Print "SEND TO ";
                    Color 15: Print "P";
                    Color 7: Print "RINTER"
                    Locate , 32: Print "SEND TO ";
                    Color 15: Print "S";
                    Color 7: Print "CREEN"
                    J$ = GetKeyPress$

                Loop Until UCase$(J$) = "F" Or UCase$(J$) = "P" Or UCase$(J$) = "S" Or J$ = Chr$(27)

                backToMenu = 0

                If J$ <> Chr$(27) Then

                    Select Case UCase$(J$)

                        Case "F":
                            userFile$ = _SAVEFILEDIALOG$("Save Comparison Report",_CWD$ + "\Comparison","*.txt","Text File")
                            If userFile$ <> "" Then
                                Call Comparisons_Report(0, userFile$, team$, posn$())
                            End If

                        Case "P":
                            Call Comparisons_Report(1, "COMPARISON", team$, posn$())

                        Case "S":
                            Call Comparisons_Screen(team$, backToMenu, posn$())

                    End Select

                End If

            End If 'Done checking for valid stat file

        End If 'Done checking for valid team

    Loop Until backToMenu = 0 Or J$ = Chr$(27) Or teamNbr = 0 Or haveStats = 0

End Sub


'----------------------------------------
'     Comparisons_Screen Subroutine
'----------------------------------------
'This subroutine provides a brief menu of
'player comparisons that can be done, and
'then formats stats for those options for
'use with the user display
Sub Comparisons_Screen (teamName$, backToMenu, posn$())

    Shared playerRatings%(), teamRatings%()

    Shared statsZ!(), statsZ1!()

    Shared statPlyrNames$()

    Cls

    For dc = 1 To 22
        Color 15, 0
        Locate dc
        Print String$(79, 177)
    Next

    Call TeamComparisons_Screen(teamName$)

    Locate 23, 28
    Color 15, 0: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Do

        Color 0, 0
        Cls

        Color 9
        Locate 3, 12: Print "STAT COMPARISONS FOR ";
        Color 15: Print teamName$

        'Print names of all 14 characters
        For I = O To 14:
            Color 12
            Locate I + 5, 15
            Print Using "##"; I + 1;
            Print ". ";
            Color 7: Print statPlyrNames$(I)
        Next

        Color 12
        Locate 5, 35: Print "16. ";
        Color 3: Print "View Players In Sequence"
        Color 12
        Locate 6, 35: Print "17. ";
        Color 3: Print "View Another Team"
        Color 12
        Locate 7, 35: Print "18. ";
        Color 3: Print "Return To Main Menu"

        Do

            Color 15
            Locate 23, 15
            Input "ENTER # TO VIEW "; B9

            Select Case B9

                Case 1 To 15:
                    playerIdx = B9 - 1

                    Cls

                    For dc = 1 To 19
                        Color 15, 0: Locate dc
                        Print String$(78, 177)
                    Next

                    Call PlayerComparisons_Screen(playerIdx, posn$(playerIdx), teamName$)

                    Color 15, 0: Locate 22: Print "HIT ENTER TO CONTINUE"
                    idx$ = GetKeyPress$

                Case 16
                    Cls

                    For dc = 1 To 19
                        Color 15, 0: Locate dc
                        Print String$(78, 177)
                    Next

                    playerIdx = 0

                    Do

                        If statPlyrNames$(playerIdx) <> "XXX" And statPlyrNames$(playerIdx) <> "END" And statPlyrNames$(playerIdx) <> "" Then

                            Call PlayerComparisons_Screen(playerIdx, posn$(playerIdx), teamName$)

                            Color 15, 0
                            Locate 22: Print "HIT ENTER TO CONTINUE"
                            Locate 22, 23: Print "OR ESC TO RETURN TO MENU"
                            I$ = GetKeyPress$

                            If I$ = Chr$(27) Then menuExit = 1

                        End If 'done checking for null player

                        playerIdx = playerIdx + 1

                    Loop Until playerIdx = 15 Or menuExit = 1

                Case 17:
                    backToMenu = 1

                Case 18: '
                    Exit Sub

            End Select

        Loop Until B9 >= 1 And B9 <= 18 Or menuExit = 1

    Loop Until B9 = 18 Or backToMenu = 1

    If backToMenu <> 1 Then
        Color 15, 0
        Locate 23, 28: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$
    End If

End Sub


'----------------------------------------
'   TeamComparisons_Screen Subroutine
'----------------------------------------
'This subroutine focuses on the formatting
'of team stats for comparison. The stats
'are output to the end user display
Sub TeamComparisons_Screen (teamName$)

    Shared roadLosses, roadWins, homeLosses, homeWins

    Shared statPlyrNames$()
    Shared playerRatings%(), teamRatings%()
    Shared statsZ!(), statsZ1!()

    Locate 11, 31: Print Space$(18)

    Color 11
    Locate 11, 33: Print teamName$

    Color 15, 2
    Locate 5, 30: Print "      HOME    W - L ";
    Locate 6, 30: Print "  ACTUAL   ";
    Locate 7, 30: Print "  REPLAY   ";
    Locate 15, 30: Print "      ROAD    W - L ";
    Locate 16, 30: Print "  ACTUAL   ";
    Locate 17, 30: Print "  REPLAY   ";

    Color 15, 1
    Locate 2, 3: Print "   OFFENSIVE STATS    "
    Locate 3, 3: Print "       ACTUAL   REPLAY"
    Locate 4, 3: Print " FGM   ";
    Locate 5, 3: Print " FGA   ";
    Locate 6, 3: Print " FG PCT";
    Locate 7, 3: Print " FTM   ";
    Locate 8, 3: Print " FTA   ";
    Locate 9, 3: Print " FT PCT";
    Locate 10, 3: Print " O.RB  ";
    Locate 11, 3: Print " D.RB  ";
    Locate 12, 3: Print " T.REB ";
    Locate 13, 3: Print " ASS'T ";
    Locate 14, 3: Print " FLS   ";
    Locate 15, 3: Print "  ST   ";
    Locate 16, 3: Print "  TO   ";
    Locate 17, 3: Print " BLK   ";
    Locate 18, 3: Print " 3-FGM ";
    Locate 19, 3: Print " 3-FGA ";
    Locate 20, 3: Print " 3-PCT ";

    Color 15, 12
    Locate 2, 55: Print "   DEFENSIVE STATS   "
    Locate 3, 55: Print "       ACTUAL  REPLAY"
    Locate 4, 55: Print " FGM   ";
    Locate 5, 55: Print " FGA   ";
    Locate 6, 55: Print " FG PCT";
    Locate 7, 55: Print " FTM   ";
    Locate 8, 55: Print " FTA   ";
    Locate 9, 55: Print " FT PCT";
    Locate 10, 55: Print " O.RB  ";
    Locate 11, 55: Print " D.RB  ";
    Locate 12, 55: Print " T.REB ";
    Locate 13, 55: Print " ASS'T ";
    Locate 14, 55: Print " FLS   ";
    Locate 15, 55: Print "  ST   ";
    Locate 16, 55: Print "  TO   ";
    Locate 17, 55: Print " BLK   ";
    Locate 18, 55: Print " 3-FGM ";
    Locate 19, 55: Print " 3-FGA ";
    Locate 20, 55: Print " 3-PCT ";

    For I = 3 To 21:
        Locate I, 25
        Color 7, 0
        Print String$(2, 177)
    Next

    For I = 3 To 21:
        Locate I, 76
        Print String$(2, 177)
    Next

    Locate 21, 5: Print String$(20, 177)
    Locate 21, 57: Print String$(20, 177)

    For I = 6 To 8:
        Locate I, 50
        Print String$(2, 177)
    Next

    For I = 16 To 18
        Locate I, 50
        Print String$(2, 177)
    Next

    Locate 8, 32: Print String$(18, 177)
    Locate 18, 32: Print String$(18, 177)

    Color 3, 0
    Locate 6, 41: Print Using "####"; teamRatings%(1);
    Locate 6, 45: Print Using "#### "; teamRatings%(2);

    Color 14, 0
    Locate 7, 41: Print Using "####"; homeWins;
    Locate 7, 45: Print Using "#### "; homeLosses;

    Color 3, 0
    Locate 4, 10: Print Using "######"; teamRatings%(6);
    Locate 5, 10: Print Using "######"; teamRatings%(5);

    Locate 6, 10
    If teamRatings%(6) = 0 Then 
        Print "   0.0";
    End If
    If teamRatings%(6) > 0 Then 
        Print Using "  .###"; (teamRatings%(6) / teamRatings%(5));
    End If

    Locate 7, 10: Print Using "######"; teamRatings%(8);
    Locate 8, 10: Print Using "######"; teamRatings%(7);

    Locate 9, 10
    If teamRatings%(7) = 0 Then 
        Print "   0.0";
    End If
    If teamRatings%(7) > 0 Then 
        Print Using " .###"; (teamRatings%(8) / teamRatings%(7));
    End If

    Locate 10, 10: Print Using "######"; teamRatings%(9);
    Locate 11, 10: Print Using "######"; teamRatings%(10);
    Locate 12, 10: Print Using "######"; teamRatings%(9) + teamRatings%(10);
    Locate 13, 10: Print Using "######"; teamRatings%(11);
    Locate 14, 10: Print Using "######"; teamRatings%(12);
    Locate 15, 10: Print Using "######"; teamRatings%(14);
    Locate 16, 10: Print Using "######"; teamRatings%(15);
    Locate 17, 10: Print Using "######"; teamRatings%(16);
    Locate 18, 10: Print Using "######"; teamRatings%(18);
    Locate 19, 10: Print Using "######"; teamRatings%(17);

    Color 3, 0
    Locate 4, 62: Print Using "######"; teamRatings%(20);
    Locate 5, 62: Print Using "######"; teamRatings%(19);


    Locate 6, 62
    If teamRatings%(19) = 0 Then 
        Print "   0.0";
    End If
    If teamRatings%(19) > 0 Then 
        Print Using "  .###"; (teamRatings%(20) / teamRatings%(19));
    End If

    Locate 7, 62: Print Using "######"; teamRatings%(22);
    Locate 8, 62: Print Using "######"; teamRatings%(21);

    Locate 9, 62
    If teamRatings%(21) = 0 Then 
        Print "   0.0";
    End If
    If teamRatings%(21) > 0 Then 
        Print Using "  .###"; (teamRatings%(22) / teamRatings%(21));
    End If

    Locate 10, 62: Print Using "######"; teamRatings%(23);
    Locate 11, 62: Print Using "######"; teamRatings%(24);
    Locate 12, 62: Print Using "######"; teamRatings%(23) + teamRatings%(24);
    Locate 13, 62: Print Using "######"; teamRatings%(25);
    Locate 14, 62: Print Using "######"; teamRatings%(26);
    Locate 15, 62: Print Using "######"; teamRatings%(28);
    Locate 16, 62: Print Using "######"; teamRatings%(29);
    Locate 17, 62: Print Using "######"; teamRatings%(30);
    Locate 18, 62: Print Using "######"; teamRatings%(32);
    Locate 19, 62: Print Using "######"; teamRatings%(31);

    Locate 20, 62
    If teamRatings%(31) = 0 Then 
        Print "   0.0";
    End If
    If teamRatings%(31) > 0 Then 
        Print Using "  .###"; (teamRatings%(32) / teamRatings%(31));
    End If

    Locate 20, 10
    If teamRatings%(17) = 0 Then 
        Print "   0.0";
    End If
    If teamRatings%(17) > 0 Then 
        Print Using "  .###"; (teamRatings%(18) / teamRatings%(17));
    End If

    Color 3, 0
    Locate 16, 41: Print Using "####"; teamRatings%(3);
    Locate 16, 45: Print Using "#### "; teamRatings%(4);

    Color 14, 0
    Locate 17, 41: Print Using "####"; roadWins;
    Locate 17, 45: Print Using "#### "; roadLosses;
    Locate 4, 16: Print Using "#########"; statsZ!(2);
    Locate 5, 16: Print Using "#########"; statsZ!(1);

    Locate 6, 16
    If statsZ!(1) = 0 Then 
        Print "      0.0";
    End If
    If statsZ!(1) > 0 Then 
        Print Using "     .###"; (statsZ!(2) / statsZ!(1));
    End If

    Locate 7, 16: Print Using "#########"; statsZ!(4);
    Locate 8, 16: Print Using "#########"; statsZ!(3);

    Locate 9, 16
    If statsZ!(3) = 0 Then 
        Print "      0.0";
    End If
    If statsZ!(3) > 0 Then
        Print Using "     .###"; (statsZ!(4) / statsZ!(3));
    End If

    Locate 10, 16: Print Using "#########"; statsZ!(12);
    Locate 11, 16: Print Using "#########"; statsZ!(5) - statsZ!(12) - statsZ!(15);
    Locate 12, 16: Print Using "#########"; statsZ!(5) - statsZ!(15);
    Locate 13, 16: Print Using "#########"; statsZ!(8);
    Locate 14, 16: Print Using "#########"; statsZ!(6);
    Locate 15, 16: Print Using "#########"; statsZ!(7);
    Locate 16, 16: Print Using "#########"; statsZ!(11);
    Locate 17, 16: Print Using "#########"; statsZ!(9);
    Locate 18, 16: Print Using "#########"; statsZ!(13);
    Locate 19, 16: Print Using "#########"; statsZ!(14);
    Locate 4, 68: Print Using "########"; statsZ1!(2);
    Locate 5, 68: Print Using "########"; statsZ1!(1);

    Locate 6, 68
    If statsZ1!(1) = 0 Then 
        Print "     0.0";
    End If
    If statsZ1!(1) > 0 Then 
        statPCT! = (statsZ1!(2) / statsZ1!(1))
        Print Using "    .###"; statPCT!;
    End If

    Locate 7, 68: Print Using "########"; statsZ1!(4);
    Locate 8, 68: Print Using "########"; statsZ1!(3);

    Locate 9, 68
    If statsZ1!(13) = 0 Then 
        Print "     0.0";
    End If
    If statsZ1!(13) > 0 Then 
        statPCT! = (statsZ1!(4) / statsZ1!(3))
        Print Using "    .###"; statPCT!;
    End If

    Locate 10, 68: Print Using "########"; statsZ1!(12);
    Locate 11, 68: Print Using "########"; statsZ1!(5) - statsZ1!(12) - statsZ1!(15);
    Locate 12, 68: Print Using "########"; statsZ1!(5) - statsZ1!(15);
    Locate 13, 68: Print Using "########"; statsZ1!(8);
    Locate 14, 68: Print Using "########"; statsZ1!(6);
    Locate 15, 68: Print Using "########"; statsZ1!(7);
    Locate 16, 68: Print Using "########"; statsZ1!(11);
    Locate 17, 68: Print Using "########"; statsZ1!(9);
    Locate 18, 68: Print Using "########"; statsZ1!(13);
    Locate 19, 68: Print Using "########"; statsZ1!(14);

    Locate 20, 68
    If statsZ1!(13) = 0 Then 
        Print "     0.0";
    End If
    If statsZ1!(13) > 0 Then 
        statPCT! = (statsZ1!(13) / statsZ1!(14))
        Print Using "    .###"; statPCT!;
    End If

    Locate 20, 16
    If statsZ!(13) = 0 Then 
        Print "      0.0";
    End If
    If statsZ!(13) > 0 Then 
        statPCT! = (statsZ!(13) / statsZ!(14))
        Print Using "     .###"; statPCT!;
    End If
    
End Sub


'----------------------------------------
'  PlayerComparisons_Screen Subroutine
'----------------------------------------
'This subroutine focuses on the formatting
'of team stats for comparison. The stats
'are output to the end user display
Sub PlayerComparisons_Screen (idx, teamName$, position$)

    Shared playerRatings%(), teamRatings%()

    Shared compareA!()
    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()

    Shared statPlyrNames$()

    Color 11
    Locate 1, 2: Print teamName$

    Color 15, 0
    Locate 1, 14: Print String$(34, 177)

    Color 14

    'Player Name / Position
    Locate 2, 14: Print statPlyrNames$(idx);
    Print " - ";
    Color 11: Print position$

    Color 12
    Locate 5, 2: Print "ACTUAL  "
    Locate 6, 2: Print "REPLAY  ";

    Color 7, 0: Print String$(2, 177)
    Locate 7, 4: Print String$(8, 177)

    Color 0, 11
    Locate 4, 14: Print "  G ";
    Locate , 18: Print " MIN ";
    Locate , 23: Print " FGM ";
    Locate , 28: Print " FGA ";
    Locate , 33: Print " FG-PCT ";
    Locate , 41: Print " FTM ";
    Locate , 46: Print " FTA ";
    Locate , 51: Print " FT-PCT ";
    Locate , 59: Print " O.REB ";
    Locate , 65: Print "  O.REB/G"

    Color 12, 0
    Locate 10, 2: Print "ACTUAL  "
    Locate 11, 2: Print "REPLAY  ";

    Color 7, 0: Print String$(2, 177)
    Locate 12, 4: Print String$(8, 177)

    Color 0, 11
    Locate 9, 14: Print " D.REB ";
    Locate , 21: Print " D.REB/G ";
    Locate , 30: Print " ASST ";
    Locate , 36: Print " ASST/G ";
    Locate , 44: Print " FOULS ";
    Locate , 51: Print " F/G ";
    Locate , 56: Print " TREB ";
    Locate , 62: Print " STL ";
    Locate , 67: Print " STL/G"

    Color 12, 0
    Locate 15, 2: Print "ACTUAL  "
    Locate 16, 2: Print "REPLAY  ";

    Color 7, 0: Print String$(2, 177)
    Locate 12, 4: Print String$(8, 177)

    Color 0, 11
    Locate 14, 14: Print " BLK ";
    Locate , 19: Print " BLK/G ";
    Locate , 26: Print " 3-FGM ";
    Locate , 33: Print " 3-FGA ";
    Locate , 40: Print " 3-FG-PCT ";
    Locate , 50: Print " HI-GM ";
    Locate , 57: Print "  PTS";
    Locate , 62: Print " PTS/G ";
    Locate , 69: Print " MIN/G"

    Color 1, 11
    Locate 5, 14: Print Using "###"; playerRatings%(idx, 0);
    Locate , 17: Print Using "#####"; playerRatings%(idx, 1);
    Locate , 22: Print Using "#####"; playerRatings%(idx, 3);
    Locate , 27: Print Using "#####"; playerRatings%(idx, 2);

    Locate , 32
    If playerRatings%(idx, 3) = 0 Then
		Print "    0.0%";
    End If
    If playerRatings%(idx, 3) > 0 Then
        Print Using "#####.#"; (playerRatings%(idx, 3) / playerRatings%(idx, 2)) * 100;
        Print "%";
    End If

    Locate , 40: Print Using "#####"; playerRatings%(idx, 5);
    Locate , 45: Print Using "#####"; playerRatings%(idx, 4);

    Locate , 50
    If playerRatings%(idx, 5) = 0 Then
		Print "    0.0%";
    End If
    If playerRatings%(idx, 5) > 0 Then
        Print Using "#####.#"; (playerRatings%(idx, 5) / playerRatings%(idx, 4)) * 100;
        Print "%";
    End If

    Locate , 58: Print Using "#######"; playerRatings%(idx, 6);

    Locate , 65
    If playerRatings%(idx, 0) = 0 Or playerRatings%(idx, 6) = 0 Or (playerRatings%(idx, 0) > 0 And playerRatings%(idx, 6) = 0) Then 
        Print "      0.0";
    End If
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 6) > 0 Then 
        Print Using "#######.#"; playerRatings%(idx, 6) / playerRatings%(idx, 0);
    End If

    Color 7, 0: Print String$(2, 177)

    Color 1, 11
    Locate 10, 14: Print Using "######"; playerRatings%(idx, 7);

    Locate , 20
    If playerRatings%(idx, 0) = 0 Or playerRatings%(idx, 7) = 0 Or (playerRatings%(idx, 0) > 0 And playerRatings%(idx, 7) = 0) Then 
        Print "      0.0";
    End If
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 7) > 0 Then 
        Print Using "#######.#"; playerRatings%(idx, 7) / playerRatings%(idx, 0);
    End If

    Locate , 29: Print Using "######"; playerRatings%(idx, 8);

    Locate , 35
    If playerRatings%(idx, 0) = 0 Or playerRatings%(idx, 8) = 0 Or (playerRatings%(idx, 0) > 0 And playerRatings%(idx, 8) = 0) Then 
        Print "     0.0";
    End If
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 8) > 0 Then 
        Print Using "######.#"; playerRatings%(idx, 8) / playerRatings%(idx, 0);
    End If

    Locate , 43: Print Using "#######"; playerRatings%(idx, 9);

    Locate , 50
    If playerRatings%(idx, 0) = 0 Or playerRatings%(idx, 9) = 0 Or (playerRatings%(idx, 0) > 0 And playerRatings%(idx, 9) = 0) Then 
        Print "  0.0";
    End If
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 9) > 0 Then 
        Print Using "###.#"; playerRatings%(idx, 9) / playerRatings%(idx, 0);
    End If

    Locate , 55: Print Using "######"; playerRatings%(idx, 6) + playerRatings%(idx, 7);
    Locate , 61: Print Using "#####"; playerRatings%(idx, 11);

    Locate , 66
    If playerRatings%(idx, 0) = 0 Or playerRatings%(idx, 11) = 0 Or (playerRatings%(idx, 0) > 0 And playerRatings%(idx, 11) = 0) Then 
        Print "    0.0";
    End If
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 11) > 0 Then 
        Print Using "#####.#"; playerRatings%(idx, 11) / playerRatings%(idx, 0);
    End If

    Color 7, 0: Print String$(2, 177)

    Color 1, 11
    Locate 15, 14: Print Using "####"; playerRatings%(idx, 12);

    Locate , 18
    If playerRatings%(idx, 0) = 0 Or playerRatings%(idx, 12) = 0 Or (playerRatings%(idx, 0) > 0 And playerRatings%(idx, 12) = 0) Then 
        Print "    0.0";
    End If
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 12) > 0 Then 
        Print Using "#####.#"; playerRatings%(idx, 12) / playerRatings%(idx, 0);
    End If

    Locate , 25: Print Using "#######"; playerRatings%(idx, 14);
    Locate , 32: Print Using "#######"; playerRatings%(idx, 13);

    Locate , 39
    If playerRatings%(idx, 14) = 0 Then 
        Print "      0.0%";
    End If
    If playerRatings%(idx, 14) > 0 Then
        Print Using "#######.#"; (playerRatings%(idx, 14) / playerRatings%(idx, 13)) * 100;
        Print "%";
    End If

    Locate , 49: Print Using "#######"; playerRatings%(idx, 15);
    Locate , 56: Print Using "######"; (playerRatings%(idx, 3) * 2) + playerRatings%(idx, 5) + playerRatings%(idx, 14);

    Locate , 62
    If (playerRatings%(idx, 3) * 2) + playerRatings%(idx, 5) + playerRatings%(idx, 14) = 0 Then 
        Print "   0.0";
    End If
    If (playerRatings%(idx, 3) * 2) + playerRatings%(idx, 5) + playerRatings%(idx, 14) > 0 Then 
        Print Using "####.#"; ((playerRatings%(idx, 3) * 2) + playerRatings%(idx, 5) + playerRatings%(idx, 14)) / playerRatings%(idx, 0);
    End If

    Locate , 68
    If playerRatings%(idx, 1) = 0 Then 
        Print "    0.0";
    End If
    If playerRatings%(idx, 1) > 0 Then 
        Print Using "#####.#"; playerRatings%(idx, 1) / playerRatings%(idx, 0);
    End If

    Color 7, 0: Print String$(2, 177)

    Color 14, 11
    Locate 6, 14: Print Using "###"; compareA!(idx, 0);

    Locate , 17
    If compareA!(idx, 10) = 0 Then 
        Print "    0";
    End If
    If compareA!(idx, 10) > 0 Then 
        Print Using "#####"; (compareA!(idx, 10) + 30) / 60;
    End If

    Locate , 22: Print Using "#####"; compareA!(idx, 2);
    Locate , 27: Print Using "#####"; compareA!(idx, 1);

    Locate , 32
    If compareA!(idx, 2) = 0 Then 
        Print "    0.0%";
    End If
    If compareA!(idx, 2) > 0 Then
        Print Using "#####.#"; (compareA!(idx, 2) / compareA!(idx, 1)) * 100;
        Print "%";
    End If

    Locate 6, 40: Print Using "#####"; compareA!(idx, 4);
    Locate 6, 45: Print Using "#####"; compareA!(idx, 3);

    Locate , 50
    If compareA!(idx, 4) = 0 Then 
        Print "    0.0%";
    End If
    If compareA!(idx, 4) > 0 Then
        Print Using "#####.#"; compareA!(idx, 4) / compareA!(idx, 3) * 100;
        Print "%";
    End If

    Locate , 58: Print Using "#######"; compareA!(idx, 12);

    Locate , 65
    If compareA!(idx, 0) = 0 Or compareA!(idx, 12) = 0 Or (compareA!(idx, 0) > 0 And compareA!(idx, 12) = 0) Then
        Print "      0.0";
    End If
    If compareA!(idx, 0) > 0 And compareA!(idx, 12) > 0 Then
		Print Using "#######.#"; compareA!(idx, 12) / compareA!(idx, 0);
    End If

    Color 7, 0: Print String$(2, 177)
    Locate , 16: Print String$(60, 177)

    Color 14, 11
    Locate 11, 14: Print Using "######"; compareA!(idx, 5) - compareA!(idx, 12);

    Locate , 20
    If compareA!(idx, 0) = 0 Or (compareA!(idx, 5) - compareA!(idx, 12)) = 0 Or (compareA!(idx, 0) > 0 And (compareA!(idx, 5) - compareA!(idx, 12)) = 0) Then
		Print "      0.0";
    End If
    If compareA!(idx, 0) > 0 And (compareA!(idx, 5) - compareA!(idx, 12)) > 0 Then
		Print Using "#######.#"; (compareA!(idx, 5) - compareA!(idx, 12)) / compareA!(idx, 0);
    End If

    Locate , 29: Print Using "######"; compareA!(idx, 8);

    Locate , 35
    If compareA!(idx, 0) = 0 Or compareA!(idx, 8) = 0 Or (compareA!(idx, 0) > 0 And compareA!(idx, 8) = 0) Then
		Print "     0.0";
    End If
    If compareA!(idx, 0) > 0 And compareA!(idx, 8) > 0 Then
		Print Using "######.#"; compareA!(idx, 8) / compareA!(idx, 0);
    End If

    Locate , 43: Print Using "#######"; compareA!(idx, 6);

    Locate , 50
    If compareA!(idx, 0) = 0 Or compareA!(idx, 6) = 0 Or (compareA!(idx, 0) > 0 And compareA!(idx, 6) = 0) Then
		Print "  0.0";
    End If
    If compareA!(idx, 0) > 0 And compareA!(idx, 6) > 0 Then
		Print Using "###.#"; compareA!(idx, 6) / compareA!(idx, 0);
    End If

    Locate , 55: Print Using "######"; compareA!(idx, 5);
    Locate , 61: Print Using "#####"; compareA!(idx, 7);

    Locate , 66
    If compareA!(idx, 0) = 0 Or compareA!(idx, 7) = 0 Or (compareA!(idx, 0) > 0 And compareA!(idx, 7) = 0) Then
		Print "    0.0";
    End If
    If compareA!(idx, 0) > 0 And compareA!(idx, 7) > 0 Then
		Print Using "#####.#"; compareA!(idx, 7) / compareA!(idx, 0);
    End If

    Color 7, 0: Print String$(2, 177)
    Locate , 16: Print String$(59, 177)

    Color 14, 11
    Locate 16, 14: Print Using "####"; compareA!(idx, 9);

    Locate , 18
    If compareA!(idx, 0) = 0 Or compareA!(idx, 9) = 0 Or (compareA!(idx, 0) > 0 And compareA!(idx, 9) = 0) Then
		Print "    0.0";
    End If
    If compareA!(idx, 0) > 0 And compareA!(idx, 9) > 0 Then
		Print Using "#####.#"; compareA!(idx, 9) / compareA!(idx, 0);
    End If

    Locate , 25: Print Using "#######"; statsW0!(idx);
    Locate , 32: Print Using "#######"; statsW1!(idx);

    Locate , 39
    If statsW0!(idx) = 0 Then
		Print "      0.0%";
    End If
    If statsW0!(idx) > 0 Then
        Print Using "#######.#"; statsW0!(idx) / statsW1!(idx) * 100;
        Print "%";
    End If

    Locate , 49: Print Using "#######"; compareA!(idx, 13);
    Locate , 56: Print Using "######"; (compareA!(idx, 2) * 2) + compareA!(idx, 4) + statsW0!(idx);

    Locate , 62
    If (compareA!(idx, 2) * 2) + compareA!(idx, 4) + statsW0!(idx) = 0 Then
		Print "   0.0";
    End If
    If (compareA!(idx, 2) * 2) + compareA!(idx, 4) + statsW0!(idx) > 0 Then
		Print Using "####.#"; ((compareA!(idx, 2) * 2) + compareA!(idx, 4) + statsW0!(idx)) / compareA!(idx, 0);
    End If

    Locate , 68
    If compareA!(idx, 10) = 0 Then
		Print "    0.0";
    End If
    If compareA!(idx, 10) > 0 Then
		Print Using "#####.#"; ((compareA!(idx, 10) + 30) / 60) / compareA!(idx, 0);
    End If

    Color 7, 0: Print String$(2, 177)
    Locate , 16: Print String$(61, 177)

End Sub


'----------------------------------------
'   Comparisons_Report Subroutine
'----------------------------------------
'This subroutine focuses on the formatting
'of team stats for comparison. The stats
'are output to the end user display
Sub Comparisons_Report (printDest, destFile$, teamName$, posn$())

    Shared roadLosses, roadWins, homeLosses, homeWins

    Shared statPlyrNames$()

    Shared playerRatings%(), teamRatings%()

    Shared compareA!()
    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()

    Shared statPlyrNames$()

    Color 15, 0
    Cls

    '--------------------
    '  Team Comparisons
    '--------------------

    Open destFile$ For Output As #1

    Print #1,
    Print #1, Tab(33); teamName$
    Print #1,

    'Locate 2
    Print #1, Tab(3); "   OFFENSIVE STATS    ";
    Print #1, Tab(55); "   DEFENSIVE STATS   "
    Print #1,

    'Locate 3
    Print #1, Tab(3); "       ACTUAL   REPLAY";
    Print #1, Tab(55); "       ACTUAL  REPLAY"

    'Locate 4
    Print #1, Tab(3); " FGM   ";
    Print #1, Tab(10); Using "######"; teamRatings%(6);
    Print #1, Tab(16); Using "#########"; statsZ!(2);

    Print #1, Tab(55); " FGM   ";
    Print #1, Tab(62); Using "######"; teamRatings%(20);
    Print #1, Tab(68); Using "########"; statsZ1!(2)

    'Locate 5
    Print #1, Tab(3); " FGA   ";
    Print #1, Tab(10); Using "######"; teamRatings%(5);
    Print #1, Tab(16); Using "#########"; statsZ!(1);

    Print #1, Tab(30); "      HOME    W - L ";

    Print #1, Tab(55); " FGA   ";
    Print #1, Tab(62); Using "######"; teamRatings%(19);
    Print #1, Tab(68); Using "########"; statsZ1!(1)

    'Locate 6
    Print #1, Tab(3); " FG PCT";

    If teamRatings%(6) = 0 Then
		Print #1, Tab(10); "   0.0";
    End If
    If teamRatings%(6) > 0 Then
		Print #1, Tab(10); "  "; Using ".###"; (teamRatings%(6) / teamRatings%(5));
    End If

    If statsZ!(1) = 0 Then
		Print #1, Tab(16); "      0.0";
    End If
    If statsZ!(1) > 0 Then
		Print #1, Tab(16); "     "; Using ".###"; (statsZ!(2) / statsZ!(1));
    End If

    Print #1, Tab(30); "  ACTUAL   ";
    Print #1, Tab(41); Using "####"; teamRatings%(1);
    Print #1, Tab(45); Using "####"; teamRatings%(2);

    Print #1, Tab(55); " FG PCT";
    If teamRatings%(19) = 0 Then
		Print #1, Tab(62); "   0.0";
    End If
    If teamRatings%(19) > 0 Then
		Print #1, Tab(62); "  "; Using ".###"; (teamRatings%(20) / teamRatings%(19));
    End If

    statPCT! = (statsZ1!(2) / statsZ1!(1))
    If statsZ1!(1) = 0 Then
		Print #1, Tab(68); "     0.0"
    End If
    If statsZ1!(1) > 0 Then
		Print #1, Tab(68); "    "; Using ".###"; statPCT!
    End If

    'Locate 7
    Print #1, Tab(3); " FTM   ";
    Print #1, Tab(10); Using "######"; teamRatings%(8);
    Print #1, Tab(16); Using "#########"; statsZ!(4);

    Print #1, Tab(30); "  REPLAY   ";
    Print #1, Tab(41); Using "####"; homeWins;
    Print #1, Tab(45); Using "####"; homeLosses;

    Print #1, Tab(55); " FTM   ";
    Print #1, Tab(62); Using "######"; teamRatings%(22);
    Print #1, Tab(68); Using "########"; statsZ1!(4)

    'Locate 8
    Print #1, Tab(3); " FTA   ";
    Print #1, Tab(10); Using "######"; teamRatings%(7);
    Print #1, Tab(16); Using "#########"; statsZ!(3);

    Print #1, Tab(55); " FTA   ";
    Print #1, Tab(62); Using "######"; teamRatings%(21);
    Print #1, Tab(68); Using "########"; statsZ1!(3)

    'Locate 9
    Print #1, Tab(3); " FT PCT";

    If teamRatings%(7) = 0 Then
		Print #1, Tab(10); "   0.0";
    End If
    If teamRatings%(7) > 0 Then
		Print #1, Tab(10); "  "; Using ".###"; (teamRatings%(8) / teamRatings%(7));
    End If

    If statsZ!(3) = 0 Then
		Print #1, Tab(16); "      0.0";
    End If
    If statsZ!(3) > 0 Then
		Print #1, Tab(16); "     "; Using ".###"; (statsZ!(4) / statsZ!(3));
    End If

    Print #1, Tab(55); " FT PCT";

    If teamRatings%(21) = 0 Then
		Print #1, Tab(62); "   0.0";
    End If
    If teamRatings%(21) > 0 Then
		Print #1, Tab(62); "  "; Using ".###"; (teamRatings%(22) / teamRatings%(21));
    End If

    statPCT! = (statsZ1!(4) / statsZ1!(3))
    If statsZ1!(13) = 0 Then
		Print #1, Tab(68); "     0.0"
    End If
    If statsZ1!(13) > 0 Then
		Print #1, Tab(68); "    "; Using ".###"; statPCT!
    End If

    'Locate 10
    Print #1, Tab(3); " O.RB  ";
    Print #1, Tab(10); Using "######"; teamRatings%(9);
    Print #1, Tab(16); Using "#########"; statsZ!(12);

    Print #1, Tab(55); " O.RB  ";
    Print #1, Tab(62); Using "######"; teamRatings%(23);
    Print #1, Tab(68); Using "########"; statsZ1!(12)

    'Locate 11
    Print #1, Tab(3); " D.RB  ";
    Print #1, Tab(10); Using "######"; teamRatings%(10);
    Print #1, Tab(16); Using "#########"; statsZ!(5) - statsZ!(12) - statsZ!(15);

    Print #1, Tab(55); " D.RB  ";
    Print #1, Tab(62); Using "######"; teamRatings%(24);
    Print #1, Tab(68); Using "########"; statsZ1!(5) - statsZ1!(12) - statsZ1!(15)

    'Locate 12
    Print #1, Tab(3); " T.REB ";
    Print #1, Tab(10); Using "######"; teamRatings%(9) + teamRatings%(10);
    Print #1, Tab(16); Using "#########"; statsZ!(5) - statsZ!(15);

    Print #1, Tab(55); " T.REB ";
    Print #1, Tab(62); Using "######"; teamRatings%(23) + teamRatings%(24);
    Print #1, Tab(68); Using "########"; statsZ1!(5) - statsZ1!(15)

    'Locate 13
    Print #1, Tab(3); " ASS'T ";
    Print #1, Tab(10); Using "######"; teamRatings%(11);
    Print #1, Tab(16); Using "#########"; statsZ!(8);

    Print #1, Tab(55); " ASS'T ";
    Print #1, Tab(62); Using "######"; teamRatings%(25);
    Print #1, Tab(68); Using "########"; statsZ1!(8)

    'Locate 14
    Print #1, Tab(3); " FLS   ";
    Print #1, Tab(10); Using "######"; teamRatings%(12);
    Print #1, Tab(16); Using "#########"; statsZ!(6);

    Print #1, Tab(55); " FLS   ";
    Print #1, Tab(62); Using "######"; teamRatings%(26);
    Print #1, Tab(68); Using "########"; statsZ1!(6)

    'Locate 15
    Print #1, Tab(3); "  ST   ";
    Print #1, Tab(10); Using "######"; teamRatings%(14);
    Print #1, Tab(16); Using "#########"; statsZ!(7);

    Print #1, Tab(30); "      ROAD    W - L ";

    Print #1, Tab(55); "  ST   ";
    Print #1, Tab(62); Using "######"; teamRatings%(28);
    Print #1, Tab(68); Using "########"; statsZ1!(7)

    'Locate 16
    Print #1, Tab(3); "  TO   ";
    Print #1, Tab(10); Using "######"; teamRatings%(15);
    Print #1, Tab(16); Using "#########"; statsZ!(11);

    Print #1, Tab(30); "  ACTUAL   ";
    Print #1, Tab(41); Using "####"; teamRatings%(3);
    Print #1, Tab(45); Using "####"; teamRatings%(4);

    Print #1, Tab(55); "  TO   ";
    Print #1, Tab(62); Using "######"; teamRatings%(29);
    Print #1, Tab(68); Using "########"; statsZ1!(11)

    'Locate 17
    Print #1, Tab(3); " BLK   ";
    Print #1, Tab(10); Using "######"; teamRatings%(16);
    Print #1, Tab(16); Using "#########"; statsZ!(9);

    Print #1, Tab(30); "  REPLAY   ";
    Print #1, Tab(41); Using "####"; roadWins;
    Print #1, Tab(45); Using "####"; roadLosses;

    Print #1, Tab(55); " BLK   ";
    Print #1, Tab(62); Using "######"; teamRatings%(30);
    Print #1, Tab(68); Using "########"; statsZ1!(9)

    'Locate 18
    Print #1, Tab(3); " 3-FGM ";
    Print #1, Tab(10); Using "######"; teamRatings%(18);
    Print #1, Tab(16); Using "#########"; statsZ!(13);

    Print #1, Tab(55); " 3-FGM ";
    Print #1, Tab(62); Using "######"; teamRatings%(32);
    Print #1, Tab(68); Using "########"; statsZ1!(13)

    'Locate 19
    Print #1, Tab(3); " 3-FGA ";
    Print #1, Tab(10); Using "######"; teamRatings%(17);
    Print #1, Tab(16); Using "#########"; statsZ!(14);

    Print #1, Tab(55); " 3-FGA ";
    Print #1, Tab(62); Using "######"; teamRatings%(31);
    Print #1, Tab(68); Using "########"; statsZ1!(14)

    'Locate 20
    Print #1, Tab(3); " 3-PCT ";
    If teamRatings%(17) = 0 Then
		Print #1, Tab(10); "   0.0";
    End If
    If teamRatings%(17) > 0 Then
		Print #1, Tab(10); "  "; Using ".###"; (teamRatings%(18) / teamRatings%(17));
    End If

    statPCT! = (statsZ!(13) / statsZ!(14))
    If statsZ!(13) = 0 Then
		Print #1, Tab(16); "      0.0";
    End If
    If statsZ!(13) > 0 Then
		Print #1, Tab(16); "     "; Using ".###"; statPCT!;
    End If

    Print #1, Tab(55); " 3-PCT ";

    If teamRatings%(31) = 0 Then
		Print #1, Tab(62); "   0.0";
    End If
    If teamRatings%(31) > 0 Then
		Print #1, Tab(62); "  "; Using ".###"; (teamRatings%(32) / teamRatings%(31));
    End If

    statPCT! = (statsZ1!(13) / statsZ1!(14))
    If statsZ1!(13) = 0 Then
		Print #1, Tab(68); "     0.0"
    End If
    If statsZ1!(13) > 0 Then
		Print #1, Tab(68); "    "; Using ".###"; statPCT!
    End If

    Print #1, Chr$(12)


    '--------------------
    ' Player Comparisons
    '--------------------

    playerIdx = 0

    For playerIdx = 0 To 14

        If statPlyrNames$(playerIdx) <> "XXX" And statPlyrNames$(playerIdx) <> "END" And statPlyrNames$(playerIdx) <> "" Then

            If playerIdx Mod 3 = 0 Then
                Print #1,
                Print #1, Tab(22); "STAT COMPARISONS FOR "; teamName$
                Print #1,
            End If

            'Player Name
            Print #1, statPlyrNames$(playerIdx); " - "; posn$(playerIdx)
            Print #1,

            Print #1, Tab(14); "  G ";
            Print #1, Tab(18); " MIN ";
            Print #1, Tab(23); " FGM ";
            Print #1, Tab(28); " FGA ";
            Print #1, Tab(33); " FG-PCT ";
            Print #1, Tab(41); " FTM ";
            Print #1, Tab(46); " FTA ";
            Print #1, Tab(51); " FT-PCT ";
            Print #1, Tab(59); " O.REB ";
            Print #1, Tab(66); " O.REB/G"

            'Locate 5
            Print #1, " ACTUAL  ";
            Print #1, Tab(14); Using "###"; playerRatings%(playerIdx, 0);
            Print #1, Tab(17); Using "#####"; playerRatings%(playerIdx, 1);
            Print #1, Tab(22); Using "#####"; playerRatings%(playerIdx, 3);
            Print #1, Tab(27); Using "#####"; playerRatings%(playerIdx, 2);

            Print #1, Tab(32);
            If playerRatings%(playerIdx, 3) = 0 Then
                Print #1, "    0.0%";
            End If
            If playerRatings%(playerIdx, 3) > 0 Then
                Print #1, Using "#####.#"; (playerRatings%(playerIdx, 3) / playerRatings%(playerIdx, 2)) * 100;
                Print #1, "%";
            End If

            Print #1, Tab(40); Using "#####"; playerRatings%(playerIdx, 5);
            Print #1, Tab(45); Using "#####"; playerRatings%(playerIdx, 4);

            Print #1, Tab(50);
            If playerRatings%(playerIdx, 5) = 0 Then
                Print #1, "    0.0%";
            End If
            If playerRatings%(playerIdx, 5) > 0 Then
                Print #1, Using "#####.#"; (playerRatings%(playerIdx, 5) / playerRatings%(playerIdx, 4)) * 100;
                Print #1, "%";
            End If

            Print #1, Tab(58); Using "#######"; playerRatings%(playerIdx, 6);

            Print #1, Tab(65);
            If playerRatings%(playerIdx, 0) = 0 Or playerRatings%(playerIdx, 6) = 0 Or (playerRatings%(playerIdx, 0) > 0 And playerRatings%(playerIdx, 6) = 0) Then
                Print #1, "      0.0"
            End If
            If playerRatings%(playerIdx, 0) > 0 And playerRatings%(playerIdx, 6) > 0 Then
                Print #1, Using "#######.#"; playerRatings%(playerIdx, 6) / playerRatings%(playerIdx, 0)
            End If

            '-----

            'Locate 6
            Print #1, " REPLAY  ";
            Print #1, Tab(14); Using "###"; compareA!(playerIdx, 0);

            Print #1, Tab(17);
            If compareA!(playerIdx, 10) = 0 Then
                Print #1, "    0";
            End If
            If compareA!(playerIdx, 10) > 0 Then
                Print #1, Using "#####"; (compareA!(playerIdx, 10) + 30) / 60;
            End If

            Print #1, Tab(22); Using "#####"; compareA!(playerIdx, 2);
            Print #1, Tab(27); Using "#####"; compareA!(playerIdx, 1);

            Print #1, Tab(32);
            If compareA!(playerIdx, 2) = 0 Then
                Print #1, "    0.0%";
            End If
            If compareA!(playerIdx, 2) > 0 Then
                Print #1, Using "#####.#"; (compareA!(playerIdx, 2) / compareA!(playerIdx, 1)) * 100;
                Print #1, "%";
            End If

            Print #1, Tab(40); Using "#####"; compareA!(playerIdx, 4);
            Print #1, Tab(45); Using "#####"; compareA!(playerIdx, 3);

            Print #1, Tab(50);
            If compareA!(playerIdx, 4) = 0 Then
                Print #1, "    0.0%";
            End If
            If compareA!(playerIdx, 4) > 0 Then
                Print #1, Using "#####.#"; compareA!(playerIdx, 4) / compareA!(playerIdx, 3) * 100;
                Print #1, "%";
            End If

            Print #1, Tab(58); Using "#######"; compareA!(playerIdx, 12);

            Print #1, Tab(65);
            If compareA!(playerIdx, 0) = 0 Or compareA!(playerIdx, 12) = 0 Or (compareA!(playerIdx, 0) > 0 And compareA!(playerIdx, 12) = 0) Then
                Print #1, "      0.0";
            End If
            If compareA!(playerIdx, 0) > 0 And compareA!(playerIdx, 12) > 0 Then
                Print #1, Using "#######.#"; compareA!(playerIdx, 12) / compareA!(playerIdx, 0);
            End If

            Print #1,
            Print #1,

            '-----

            Print #1, Tab(14); " D.REB ";
            Print #1, Tab(21); " D.REB/G ";
            Print #1, Tab(30); " ASST ";
            Print #1, Tab(36); " ASST/G ";
            Print #1, Tab(44); " FOULS ";
            Print #1, Tab(51); " F/G ";
            Print #1, Tab(56); " TREB ";
            Print #1, Tab(62); " STL ";
            Print #1, Tab(67); " STL/G"

            '-----

            'Locate 10
            Print #1, " ACTUAL ";
            Print #1, Tab(14); Using "######"; playerRatings%(playerIdx, 7);

            Print #1, Tab(20);
            If playerRatings%(playerIdx, 0) = 0 Or playerRatings%(playerIdx, 7) = 0 Or (playerRatings%(playerIdx, 0) > 0 And playerRatings%(playerIdx, 7) = 0) Then
                Print #1, "      0.0";
            End If
            If playerRatings%(playerIdx, 0) > 0 And playerRatings%(playerIdx, 7) > 0 Then
                Print #1, Using "#######.#"; playerRatings%(playerIdx, 7) / playerRatings%(playerIdx, 0);
            End If

            Print #1, Tab(29); Using "######"; playerRatings%(playerIdx, 8);

            Locate , 35
            If playerRatings%(playerIdx, 0) = 0 Or playerRatings%(playerIdx, 8) = 0 Or (playerRatings%(playerIdx, 0) > 0 And playerRatings%(playerIdx, 8) = 0) Then
                Print #1, "     0.0";
            End If
            If playerRatings%(playerIdx, 0) > 0 And playerRatings%(playerIdx, 8) > 0 Then
                Print #1, Using "######.#"; playerRatings%(playerIdx, 8) / playerRatings%(playerIdx, 0);
            End If

            Locate , 43: Print #1, Using "#######"; playerRatings%(playerIdx, 9);

            Locate , 50
            If playerRatings%(playerIdx, 0) = 0 Or playerRatings%(playerIdx, 9) = 0 Or (playerRatings%(playerIdx, 0) > 0 And playerRatings%(playerIdx, 9) = 0) Then
                Print #1, "  0.0";
            End If
            If playerRatings%(playerIdx, 0) > 0 And playerRatings%(playerIdx, 9) > 0 Then
                Print #1, Using "###.#"; playerRatings%(playerIdx, 9) / playerRatings%(playerIdx, 0);
            End If

            Locate , 55: Print #1, Using "######"; playerRatings%(playerIdx, 6) + playerRatings%(playerIdx, 7);
            Locate , 61: Print #1, Using "#####"; playerRatings%(playerIdx, 11);

            Locate , 66
            If playerRatings%(playerIdx, 0) = 0 Or playerRatings%(playerIdx, 11) = 0 Or (playerRatings%(playerIdx, 0) > 0 And playerRatings%(playerIdx, 11) = 0) Then
                Print #1, "    0.0"
            End If
            If playerRatings%(playerIdx, 0) > 0 And playerRatings%(playerIdx, 11) > 0 Then
                Print #1, Using "#####.#"; playerRatings%(playerIdx, 11) / playerRatings%(playerIdx, 0)
            End If

            '-----

            'Locate 11
            Print #1, " REPLAY  ";
            Print #1, Tab(14); Using "######"; compareA!(playerIdx, 5) - compareA!(playerIdx, 12);

            Print #1, Tab(20);
            If compareA!(playerIdx, 0) = 0 Or (compareA!(playerIdx, 5) - compareA!(playerIdx, 12)) = 0 Or (compareA!(playerIdx, 0) > 0 And (compareA!(playerIdx, 5) - compareA!(playerIdx, 12)) = 0) Then
                Print #1, "      0.0";
            End If
            If compareA!(playerIdx, 0) > 0 And (compareA!(playerIdx, 5) - compareA!(playerIdx, 12)) > 0 Then
                Print #1, Using "#######.#"; (compareA!(playerIdx, 5) - compareA!(playerIdx, 12)) / compareA!(playerIdx, 0);
            End If

            Print #1, Tab(29);
            Print #1, Using "######"; compareA!(playerIdx, 8);

            Print #1, Tab(35);
            If compareA!(playerIdx, 0) = 0 Or compareA!(playerIdx, 8) = 0 Or (compareA!(playerIdx, 0) > 0 And compareA!(playerIdx, 8) = 0) Then
                Print #1, "     0.0";
            End If
            If compareA!(playerIdx, 0) > 0 And compareA!(playerIdx, 8) > 0 Then
                Print #1, Using "######.#"; compareA!(playerIdx, 8) / compareA!(playerIdx, 0);
            End If

            Print #1, Tab(43);
            Print #1, Using "#######"; compareA!(playerIdx, 6);

            Print #1, Tab(50);
            If compareA!(playerIdx, 0) = 0 Or compareA!(playerIdx, 6) = 0 Or (compareA!(playerIdx, 0) > 0 And compareA!(playerIdx, 6) = 0) Then
                Print #1, "  0.0";
            End If
            If compareA!(playerIdx, 0) > 0 And compareA!(playerIdx, 6) > 0 Then
                Print #1, Using "###.#"; compareA!(playerIdx, 6) / compareA!(playerIdx, 0);
            End If

            Print #1, Tab(55);
            Print #1, Using "######"; compareA!(playerIdx, 5);
            Print #1, Tab(61);
            Print #1, Using "#####"; compareA!(playerIdx, 7);

            Print #1, Tab(66);
            If compareA!(playerIdx, 0) = 0 Or compareA!(playerIdx, 7) = 0 Or (compareA!(playerIdx, 0) > 0 And compareA!(playerIdx, 7) = 0) Then
                Print #1, "    0.0";
            End If
            If compareA!(playerIdx, 0) > 0 And compareA!(playerIdx, 7) > 0 Then
                Print #1, Using "#####.#"; compareA!(playerIdx, 7) / compareA!(playerIdx, 0);
            End If

            '-----

            Print #1,
            Print #1,

            'Locate 14
            Print #1, Tab(14); " BLK ";
            Print #1, Tab(19); " BLK/G ";
            Print #1, Tab(26); " 3-FGM ";
            Print #1, Tab(33); " 3-FGA ";
            Print #1, Tab(40); " 3-FG-PCT ";
            Print #1, Tab(50); " HI-GM ";
            Print #1, Tab(57); "  PTS";
            Print #1, Tab(62); " PTS/G ";
            Print #1, Tab(69); " MIN/G"

            '-----

            'Locate 15
            Print #1, " ACTUAL  ";
            Print #1, Tab(14); Using "####"; playerRatings%(playerIdx, 12);

            Print #1, Tab(18);
            If playerRatings%(playerIdx, 0) = 0 Or playerRatings%(playerIdx, 12) = 0 Or (playerRatings%(playerIdx, 0) > 0 And playerRatings%(playerIdx, 12) = 0) Then
                Print #1, "    0.0";
            End If
            If playerRatings%(playerIdx, 0) > 0 And playerRatings%(playerIdx, 12) > 0 Then
                Print #1, Using "#####.#"; playerRatings%(playerIdx, 12) / playerRatings%(playerIdx, 0);
            End If

            Print #1, Tab(25);
            Print #1, Using "#######"; playerRatings%(playerIdx, 14);
            Print #1, Tab(32);
            Print #1, Using "#######"; playerRatings%(playerIdx, 13);

            Print #1, Tab(39);
            If playerRatings%(playerIdx, 14) = 0 Then
                Print #1, "      0.0%";
            End If
            If playerRatings%(playerIdx, 14) > 0 Then
                Print #1, Using "#######.#"; (playerRatings%(playerIdx, 14) / playerRatings%(playerIdx, 13)) * 100;
                Print #1, "%";
            End If

            Print #1, Tab(49);
            Print #1, Using "#######"; playerRatings%(playerIdx, 15);
            Print #1, Tab(56);
            Print #1, Using "######"; (playerRatings%(playerIdx, 3) * 2) + playerRatings%(playerIdx, 5) + playerRatings%(playerIdx, 14);

            Print #1, Tab(62);
            If (playerRatings%(playerIdx, 3) * 2) + playerRatings%(playerIdx, 5) + playerRatings%(playerIdx, 14) = 0 Then
                Print #1, "   0.0";
            End If
            If (playerRatings%(playerIdx, 3) * 2) + playerRatings%(playerIdx, 5) + playerRatings%(playerIdx, 14) > 0 Then
                Print #1, Using "####.#"; ((playerRatings%(playerIdx, 3) * 2) + playerRatings%(playerIdx, 5) + playerRatings%(playerIdx, 14)) / playerRatings%(playerIdx, 0);
            End If

            Print #1, Tab(68);
            If playerRatings%(playerIdx, 1) = 0 Then
                Print #1, "    0.0"
            End If
            If playerRatings%(playerIdx, 1) > 0 Then
                Print #1, Using "#####.#"; playerRatings%(playerIdx, 1) / playerRatings%(playerIdx, 0)
            End If

            '-----

            'Locate 16
            Print #1, " REPLAY  ";
            Print #1, Tab(14); Using "####"; compareA!(playerIdx, 9);

            Print #1, Tab(18);
            If compareA!(playerIdx, 0) = 0 Or compareA!(playerIdx, 9) = 0 Or (compareA!(playerIdx, 0) > 0 And compareA!(playerIdx, 9) = 0) Then
                Print #1, "    0.0";
            End If
            If compareA!(playerIdx, 0) > 0 And compareA!(playerIdx, 9) > 0 Then
                Print #1, Using "#####.#"; compareA!(playerIdx, 9) / compareA!(playerIdx, 0);
            End If

            Print #1, Tab(25);
            Print #1, Using "#######"; statsW0!(playerIdx);
            Print #1, Tab(32);
            Print #1, Using "#######"; statsW1!(playerIdx);

            Print #1, Tab(39);
            If statsW0!(playerIdx) = 0 Then
                Print #1, "      0.0%";
            End If
            If statsW0!(playerIdx) > 0 Then
                Print #1, Using "#######.#"; statsW0!(playerIdx) / statsW1!(playerIdx) * 100;
                Print #1, "%";
            End If

            Print #1, Tab(49);
            Print #1, Using "#######"; compareA!(playerIdx, 13);
            Print #1, Tab(56);
            Print #1, Using "######"; (compareA!(playerIdx, 2) * 2) + compareA!(playerIdx, 4) + statsW0!(playerIdx);

            Print #1, Tab(62);
            If (compareA!(playerIdx, 2) * 2) + compareA!(playerIdx, 4) + statsW0!(playerIdx) = 0 Then
                Print #1, "   0.0";
            End If
            If (compareA!(playerIdx, 2) * 2) + compareA!(playerIdx, 4) + statsW0!(playerIdx) > 0 Then
                Print #1, Using "####.#"; ((compareA!(playerIdx, 2) * 2) + compareA!(playerIdx, 4) + statsW0!(playerIdx)) / compareA!(playerIdx, 0);
            End If

            Print #1, Tab(68);
            If compareA!(playerIdx, 10) = 0 Then
                Print #1, "    0.0"
            End If
            If compareA!(playerIdx, 10) > 0 Then
                Print #1, Using "#####.#"; ((compareA!(playerIdx, 10) + 30) / 60) / compareA!(playerIdx, 0)
            End If

            Print #1,
            Print #1,

            If playerIdx Mod 3 = 2 Then
                Print #1, Chr$(12)
            End If

        End If 'done checking for null player

    Next playerIdx

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

