'----------------------------------------
'           COMPARE Subroutine
'----------------------------------------
'This subroutine loads in existing stats
'for a team and provides different options 
'to the end user for formatting stats 
Sub COMPARE ()

    _Title "Basketball: The Pro Game - Stats Comparison"

    Shared AL%, AW%, HL%, HW%

    Shared leagueRatings%(), teamRatings%()

    Shared compareA!(), compareW0!(), compareW1!()
    Shared compareZ!(), compareZ1!()

    Shared compareA$(), compareT$()
    Shared playerNames$()

    Dim HP%(100), OP%(100)

    Dim comparePS$(15)
    Dim HP$(100), OP$(100)

    Do

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

                    Input #1, compareA$(I)

                    For J = 0 To 13
                        Input #1, compareA!(I, J)
                    Next

                    Input #1, compareW0!(I)

                    Input #1, compareW1!(I)

                Next

                For L = 0 To 15:
                    Input #1, compareZ!(L)
                Next

                For L = 0 To 15:
                    Input #1, compareZ1!(L)
                Next

                Close #1

                For I = 0 To 14
                    If playerNames$(I, 1) = "F" Then comparePS$(I) = "FORWARD"
                    If playerNames$(I, 1) = "C" Then comparePS$(I) = "CENTER"
                    If playerNames$(I, 1) = "G" Then comparePS$(I) = "GUARD"
                    If playerNames$(I, 1) = "FC" Then comparePS$(I) = "FORWARD/GUARD"
                    If playerNames$(I, 1) = "FG" Then comparePS$(I) = "FORWARD/CENTER"
                    If playerNames$(I, 1) = "GF" Then comparePS$(I) = "GUARD/FORWARD"
                    If playerNames$(I, 1) = "CF" Then comparePS$(I) = "CENTER/FORWARD"
                Next

                Open diskPaths$(1) + statFile$ + "S" For Input As #2

                Input #2, Y

                For J = 1 To 100: Input #2, OP%(J): Next
                For J = 1 To 100: Input #2, OP$(J): Next
                For J = 1 To 100: Input #2, HP%(J): Next
                For J = 1 To 100: Input #2, HP$(J): Next

                Close #2

                HW% = 0
                HL% = 0
                AW% = 0
                AL% = 0

                For I = 1 To Y
                    If OP%(I) < HP%(I) And Left$(HP$(I), 1) = "H" Then HL% = HL% + 1
                    If OP%(I) > HP%(I) And Left$(HP$(I), 1) = "H" Then HW% = HW% + 1
                    If OP%(I) < HP%(I) And Left$(HP$(I), 1) = "V" Then AL% = AL% + 1
                    If OP%(I) > HP%(I) And Left$(HP$(I), 1) = "V" Then AW% = AW% + 1
                Next

                Color 15, 0
                Cls

                JJ = 4

                Locate 8, 8: Color 14, 1

                Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

                For II = 1 To JJ:
                    Locate 8 + II, 8
                    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                Next

                Locate 9 + JJ, 8

                Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";

                Do
                    Locate 10
                    Color 7, 1

                    Locate , 30: Print "STATS TO ";
                    Color 15: Print "S";: Color 7: Print "CREEN"
                    Locate , 30: Print "STATS TO ";
                    Color 15: Print "P";: Color 7: Print "RINTER"
                    J$ = GetKeyPress$

                Loop Until UCase$(J$) = "P" Or UCase$(J$) = "S" Or J$ = Chr$(27)

                backToMenu = 0

                If J$ <> Chr$(27) Then

                    Select Case UCase$(J$)

                        Case "P":
                            Call Comparisons_Printer(team$)

                        Case "S":
                            Call Comparisons_Screen(team$, backToMenu, comparePS$())

                    End Select

                End If

            End If 'Done checking file count

        End If 'Done checking for ESC

    Loop Until backToMenu = 0 Or J$ = Chr$(27) Or teamNbr = 0 Or haveStats = 0

End Sub


'----------------------------------------
'     Comparisons_Printer Subroutine
'----------------------------------------
'This subroutine formats team stats for
'output to the default printer
Sub Comparisons_Printer (teamName$)

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

    Shared compareZ!(), compareZ1!()

    Shared compareA$(), compareT$()

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
            Color 7: Print compareA$(I)
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

                    Call PlayerComparisons_Screen(playerIdx, posn$(playerIdx))

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

                        If compareA$(playerIdx) <> "XXX" And compareA$(playerIdx) <> "END" And compareA$(playerIdx) <> "" Then

                            Call PlayerComparisons_Screen(playerIdx, posn$(playerIdx))

                            Color 15, 0: Locate 22: Print "HIT ENTER TO CONTINUE"
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
Sub TeamComparisons_Screen (T$)

    Shared AL%, AW%, HL%, HW%

    Shared compareA$()
    Shared playerRatings%(), teamRatings%()
    Shared compareT$()
    Shared compareZ!(), compareZ1!()

    Locate 11, 31: Print Space$(18)

    Color 11
    Locate 11, 33: Print T$

    Color 15, 12

    Color 15, 2
    Locate 5, 30: Print "      HOME    W - L ";
    Locate 6, 30: Print "  SEASON   ";
    Locate 7, 30: Print "  LEAGUE   ";
    Locate 15, 30: Print "      ROAD    W - L ";
    Locate 16, 30: Print "  SEASON   ";
    Locate 17, 30: Print "  LEAGUE   ";
    Color 15, 1
    Locate 2, 3: Print "   OFFENSIVE STATS    "
    Locate 3, 3: Print "       SEASON   LEAGUE"
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
    Locate 3, 55: Print "       SEASON  LEAGUE"
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
    Locate 6, 45: Print Using "####"; teamRatings%(2);: Print " ";
    Color 14, 0
    Locate 7, 41: Print Using "####"; HW%;
    Locate 7, 45: Print Using "####"; HL%;: Print " ";
    Color 3, 0
    Locate 4, 10: Print Using "######"; teamRatings%(6);
    Locate 5, 10: Print Using "######"; teamRatings%(5);

    If teamRatings%(6) = 0 Then Locate 6, 10: Print "   0.0";
    If teamRatings%(6) > 0 Then Locate 6, 10: Print "  ";: Print Using ".###"; (teamRatings%(6) / teamRatings%(5));

    Locate 7, 10: Print Using "######"; teamRatings%(8);
    Locate 8, 10: Print Using "######"; teamRatings%(7);

    If teamRatings%(7) = 0 Then Locate 9, 10: Print "   0.0";
    If teamRatings%(7) > 0 Then Locate 9, 10: Print "  ";: Print Using ".###"; (teamRatings%(8) / teamRatings%(7));

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

    If teamRatings%(19) = 0 Then Locate 6, 62: Print "   0.0";
    If teamRatings%(19) > 0 Then Locate 6, 62: Print "  ";: Print Using ".###"; (teamRatings%(20) / teamRatings%(19));

    Locate 7, 62: Print Using "######"; teamRatings%(22);
    Locate 8, 62: Print Using "######"; teamRatings%(21);

    If teamRatings%(21) = 0 Then Locate 9, 62: Print "   0.0";
    If teamRatings%(21) > 0 Then Locate 9, 62: Print "  ";: Print Using ".###"; (teamRatings%(22) / teamRatings%(21));

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

    If teamRatings%(31) = 0 Then Locate 20, 62: Print "   0.0";
    If teamRatings%(31) > 0 Then Locate 20, 62: Print "  ";: Print Using ".###"; (teamRatings%(32) / teamRatings%(31));

    If teamRatings%(17) = 0 Then Locate 20, 10: Print "   0.0";
    If teamRatings%(17) > 0 Then Locate 20, 10: Print "  ";: Print Using ".###"; (teamRatings%(18) / teamRatings%(17));

    Color 3, 0
    Locate 16, 41: Print Using "####"; teamRatings%(3);
    Locate 16, 45: Print Using "####"; teamRatings%(4);: Print " ";

    Color 14, 0
    Locate 17, 41: Print Using "####"; AW%;
    Locate 17, 45: Print Using "####"; AL%;: Print " ";
    Locate 4, 16: Print Using "#########"; compareZ!(2);
    Locate 5, 16: Print Using "#########"; compareZ!(1);

    If compareZ!(1) = 0 Then Locate 6, 16: Print "      0.0";
    If compareZ!(1) > 0 Then Locate 6, 16: Print "     ";: Print Using ".###"; (compareZ!(2) / compareZ!(1));

    Locate 7, 16: Print Using "#########"; compareZ!(4);
    Locate 8, 16: Print Using "#########"; compareZ!(3);

    If compareZ!(3) = 0 Then Locate 9, 16: Print "      0.0";
    If compareZ!(3) > 0 Then Locate 9, 16: Print "     ";: Print Using ".###"; (compareZ!(4) / compareZ!(3));

    Locate 10, 16: Print Using "#########"; compareZ!(12);
    Locate 11, 16: Print Using "#########"; compareZ!(5) - compareZ!(12) - compareZ!(15);
    Locate 12, 16: Print Using "#########"; compareZ!(5) - compareZ!(15);
    Locate 13, 16: Print Using "#########"; compareZ!(8);
    Locate 14, 16: Print Using "#########"; compareZ!(6);
    Locate 15, 16: Print Using "#########"; compareZ!(7);
    Locate 16, 16: Print Using "#########"; compareZ!(11);
    Locate 17, 16: Print Using "#########"; compareZ!(9);
    Locate 18, 16: Print Using "#########"; compareZ!(13);
    Locate 19, 16: Print Using "#########"; compareZ!(14);
    Locate 4, 68: Print Using "########"; compareZ1!(2);
    Locate 5, 68: Print Using "########"; compareZ1!(1);

    If compareZ1!(1) = 0 Then Locate 6, 68: Print "     0.0";
    If compareZ1!(1) > 0 Then statPCT! = (compareZ1!(2) / compareZ1!(1)): Locate 6, 68: Print "    ";: Print Using ".###"; statPCT!;

    Locate 7, 68: Print Using "########"; compareZ1!(4);
    Locate 8, 68: Print Using "########"; compareZ1!(3);

    If compareZ1!(13) = 0 Then Locate 9, 68: Print "     0.0";
    If compareZ1!(13) > 0 Then statPCT! = (compareZ1!(4) / compareZ1!(3)): Locate 9, 68: Print "    ";: Print Using ".###"; statPCT!;

    Locate 10, 68: Print Using "########"; compareZ1!(12);
    Locate 11, 68: Print Using "########"; compareZ1!(5) - compareZ1!(12) - compareZ1!(15);
    Locate 12, 68: Print Using "########"; compareZ1!(5) - compareZ1!(15);
    Locate 13, 68: Print Using "########"; compareZ1!(8);
    Locate 14, 68: Print Using "########"; compareZ1!(6);
    Locate 15, 68: Print Using "########"; compareZ1!(7);
    Locate 16, 68: Print Using "########"; compareZ1!(11);
    Locate 17, 68: Print Using "########"; compareZ1!(9);
    Locate 18, 68: Print Using "########"; compareZ1!(13);
    Locate 19, 68: Print Using "########"; compareZ1!(14);

    If compareZ1!(13) = 0 Then Locate 20, 68: Print "     0.0";
    If compareZ1!(13) > 0 Then statPCT! = (compareZ1!(13) / compareZ1!(14)): Locate 20, 68: Print "    ";: Print Using ".###"; statPCT!;

    If compareZ!(13) = 0 Then Locate 20, 16: Print "      0.0";
    If compareZ!(13) > 0 Then statPCT! = (compareZ!(13) / compareZ!(14)): Locate 20, 16: Print "     ";: Print Using ".###"; statPCT!;

End Sub


'----------------------------------------
'  PlayerComparisons_Screen Subroutine
'----------------------------------------
'This subroutine focuses on the formatting
'of team stats for comparison. The stats
'are output to the end user display
Sub PlayerComparisons_Screen (idx, position$)

    Shared playerRatings%(), teamRatings%()

    Shared compareA!()
    Shared compareW0!(), compareW1!()
    Shared compareZ!(), compareZ1!()

    Shared compareA$()
    Shared compareT$()

    Color 11
    Locate 2, 2: Print compareT$(0)

    Color 15, 0
    Locate 2, 14: Print String$(34, 177)
    Color 14
    Locate 2, 14: Print compareA$(idx);
    Print " - ";
    Color 11: Print position$

    Color 12
    Locate 5, 2: Print "REGULAR "
    Locate 6, 2: Print "LEAGUE  ";
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
    Locate 10, 2: Print "REGULAR "
    Locate 11, 2: Print "LEAGUE  ";

    Color 7, 0
    Print String$(2, 177)
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
    Locate 15, 2: Print "REGULAR "
    Locate 16, 2: Print "LEAGUE  ";

    Color 7, 0
    Print String$(2, 177)
    Locate 17, 4: Print String$(8, 177)

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

    If playerRatings%(idx, 3) = 0 Then Locate , 32: Print "    0.0";: Print "%";
    If playerRatings%(idx, 3) > 0 Then Locate , 32: Print Using "#####.#"; (playerRatings%(idx, 3) / playerRatings%(idx, 2)) * 100;: Print "%";

    Locate , 40: Print Using "#####"; playerRatings%(idx, 5);
    Locate , 45: Print Using "#####"; playerRatings%(idx, 4);

    If playerRatings%(idx, 5) = 0 Then Locate , 50: Print "    0.0";: Print "%";
    If playerRatings%(idx, 5) > 0 Then Locate , 50: Print Using "#####.#"; (playerRatings%(idx, 5) / playerRatings%(idx, 4)) * 100;: Print "%";

    Locate , 58: Print Using "#######"; playerRatings%(idx, 6);

    If playerRatings%(idx, 0) = 0 Or playerRatings%(idx, 6) = 0 Then Locate , 65: Print "      0.0";
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 6) = 0 Then Locate , 65: Print "      0.0";
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 6) > 0 Then Locate , 65: Print Using "#######.#"; playerRatings%(idx, 6) / playerRatings%(idx, 0);

    Color 7, 0: Print String$(2, 177)
    Color 1, 11
    Locate 10, 14: Print Using "######"; playerRatings%(idx, 7);

    If playerRatings%(idx, 0) = 0 Or playerRatings%(idx, 7) = 0 Then Locate , 20: Print "      0.0";
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 7) = 0 Then Locate , 20: Print "      0.0";
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 7) > 0 Then Locate , 20: Print Using "#######.#"; playerRatings%(idx, 7) / playerRatings%(idx, 0);

    Locate , 29: Print Using "######"; playerRatings%(idx, 8);

    If playerRatings%(idx, 0) = 0 Or playerRatings%(idx, 8) = 0 Then Locate , 35: Print "     0.0";
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 8) = 0 Then Locate , 35: Print "     0.0";
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 8) > 0 Then Locate , 35: Print Using "######.#"; playerRatings%(idx, 8) / playerRatings%(idx, 0);

    Locate , 43: Print Using "#######"; playerRatings%(idx, 9);

    If playerRatings%(idx, 0) = 0 Or playerRatings%(idx, 9) = 0 Then Locate , 50: Print "  0.0";
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 9) = 0 Then Locate , 50: Print "  0.0";
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 9) > 0 Then Locate , 50: Print Using "###.#"; playerRatings%(idx, 9) / playerRatings%(idx, 0);

    Locate , 55: Print Using "######"; playerRatings%(idx, 6) + playerRatings%(idx, 7);
    Locate , 61: Print Using "#####"; playerRatings%(idx, 11);

    If playerRatings%(idx, 0) = 0 Or playerRatings%(idx, 11) = 0 Then Locate , 66: Print "    0.0";
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 11) = 0 Then Locate , 66: Print "    0.0";
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 11) > 0 Then Locate , 66: Print Using "#####.#"; playerRatings%(idx, 11) / playerRatings%(idx, 0);

    Color 7, 0: Print String$(2, 177)
    Color 1, 11
    Locate 15, 14: Print Using "####"; playerRatings%(idx, 12);

    If playerRatings%(idx, 0) = 0 Or playerRatings%(idx, 12) = 0 Then Locate , 18: Print "    0.0";
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 12) = 0 Then Locate , 18: Print "    0.0";
    If playerRatings%(idx, 0) > 0 And playerRatings%(idx, 12) > 0 Then Locate , 18: Print Using "#####.#"; playerRatings%(idx, 12) / playerRatings%(idx, 0);

    Locate , 25: Print Using "#######"; playerRatings%(idx, 14);
    Locate , 32: Print Using "#######"; playerRatings%(idx, 13);

    If playerRatings%(idx, 14) = 0 Then Locate , 39: Print "      0.0";: Print "%";
    If playerRatings%(idx, 14) > 0 Then Locate , 39: Print Using "#######.#"; (playerRatings%(idx, 14) / playerRatings%(idx, 13)) * 100;: Print "%";

    Locate , 49: Print Using "#######"; playerRatings%(idx, 15);
    Locate , 56: Print Using "######"; (playerRatings%(idx, 3) * 2) + playerRatings%(idx, 5) + playerRatings%(idx, 14);

    If (playerRatings%(idx, 3) * 2) + playerRatings%(idx, 5) + playerRatings%(idx, 14) = 0 Then Locate , 62: Print "   0.0";
    If (playerRatings%(idx, 3) * 2) + playerRatings%(idx, 5) + playerRatings%(idx, 14) > 0 Then Locate , 62: Print Using "####.#"; ((playerRatings%(idx, 3) * 2) + playerRatings%(idx, 5) + playerRatings%(idx, 14)) / playerRatings%(idx, 0);

    If playerRatings%(idx, 1) = 0 Then Locate , 68: Print "    0.0";
    If playerRatings%(idx, 1) > 0 Then Locate , 68: Print Using "#####.#"; playerRatings%(idx, 1) / playerRatings%(idx, 0);

    Color 7, 0: Print String$(2, 177)
    Color 14, 11
    Locate 6, 14: Print Using "###"; compareA!(idx, 0);

    If compareA!(idx, 10) = 0 Then Locate , 17: Print "    0";
    If compareA!(idx, 10) > 0 Then Locate , 17: Print Using "#####"; (compareA!(idx, 10) + 30) / 60;

    Locate , 22: Print Using "#####"; compareA!(idx, 2);
    Locate , 27: Print Using "#####"; compareA!(idx, 1);

    If compareA!(idx, 2) = 0 Then Locate , 32: Print "    0.0";: Print "%";
    If compareA!(idx, 2) > 0 Then Locate , 32: Print Using "#####.#"; (compareA!(idx, 2) / compareA!(idx, 1)) * 100;: Print "%";

    Locate 6, 40: Print Using "#####"; compareA!(idx, 4);
    Locate 6, 45: Print Using "#####"; compareA!(idx, 3);

    If compareA!(idx, 4) = 0 Then Locate , 50: Print "    0.0";: Print "%";
    If compareA!(idx, 4) > 0 Then Locate , 50: Print Using "#####.#"; compareA!(idx, 4) / compareA!(idx, 3) * 100;: Print "%";

    Locate , 58: Print Using "#######"; compareA!(idx, 12);

    If compareA!(idx, 0) = 0 Or compareA!(idx, 12) = 0 Then Locate , 65: Print "      0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 12) = 0 Then Locate , 65: Print "      0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 12) > 0 Then Locate , 65: Print Using "#######.#"; compareA!(idx, 12) / compareA!(idx, 0);

    Color 7, 0: Print String$(2, 177)
    Locate , 16: Print String$(60, 177)
    Color 14, 11
    Locate 11, 14: Print Using "######"; compareA!(idx, 5) - compareA!(idx, 12);

    If compareA!(idx, 0) = 0 Or (compareA!(idx, 5) - compareA!(idx, 12)) = 0 Then Locate , 20: Print "      0.0";
    If compareA!(idx, 0) > 0 And (compareA!(idx, 5) - compareA!(idx, 12)) = 0 Then Locate , 20: Print "      0.0";
    If compareA!(idx, 0) > 0 And (compareA!(idx, 5) - compareA!(idx, 12)) > 0 Then Locate , 20: Print Using "#######.#"; (compareA!(idx, 5) - compareA!(idx, 12)) / compareA!(idx, 0);

    Locate , 29: Print Using "######"; compareA!(idx, 8);

    If compareA!(idx, 0) = 0 Or compareA!(idx, 8) = 0 Then Locate , 35: Print "     0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 8) = 0 Then Locate , 35: Print "     0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 8) > 0 Then Locate , 35: Print Using "######.#"; compareA!(idx, 8) / compareA!(idx, 0);

    Locate , 43: Print Using "#######"; compareA!(idx, 6);

    If compareA!(idx, 0) = 0 Or compareA!(idx, 6) = 0 Then Locate , 50: Print "  0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 6) = 0 Then Locate , 50: Print "  0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 6) > 0 Then Locate , 50: Print Using "###.#"; compareA!(idx, 6) / compareA!(idx, 0);

    Locate , 55: Print Using "######"; compareA!(idx, 5);
    Locate , 61: Print Using "#####"; compareA!(idx, 7);

    If compareA!(idx, 0) = 0 Or compareA!(idx, 7) = 0 Then Locate , 66: Print "    0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 7) = 0 Then Locate , 66: Print "    0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 7) > 0 Then Locate , 66: Print Using "#####.#"; compareA!(idx, 7) / compareA!(idx, 0);

    Color 7, 0: Print String$(2, 177)
    Locate , 16: Print String$(59, 177)
    Color 14, 11
    Locate 16, 14: Print Using "####"; compareA!(idx, 9);

    If compareA!(idx, 0) = 0 Or compareA!(idx, 9) = 0 Then Locate , 18: Print "    0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 9) = 0 Then Locate , 18: Print "    0.0";
    If compareA!(idx, 0) > 0 And compareA!(idx, 9) > 0 Then Locate , 18: Print Using "#####.#"; compareA!(idx, 9) / compareA!(idx, 0);

    Locate , 25: Print Using "#######"; compareW0!(idx);
    Locate , 32: Print Using "#######"; compareW1!(idx);

    If compareW0!(idx) = 0 Then Locate , 39: Print "      0.0";: Print "%";
    If compareW0!(idx) > 0 Then Locate , 39: Print Using "#######.#"; compareW0!(idx) / compareW1!(idx) * 100;: Print "%";

    Locate , 49: Print Using "#######"; compareA!(idx, 13);
    Locate , 56: Print Using "######"; (compareA!(idx, 2) * 2) + compareA!(idx, 4) + compareW0!(idx);

    If (compareA!(idx, 2) * 2) + compareA!(idx, 4) + compareW0!(idx) = 0 Then Locate , 62: Print "   0.0";
    If (compareA!(idx, 2) * 2) + compareA!(idx, 4) + compareW0!(idx) > 0 Then Locate , 62: Print Using "####.#"; ((compareA!(idx, 2) * 2) + compareA!(idx, 4) + compareW0!(idx)) / compareA!(idx, 0);

    If compareA!(idx, 10) = 0 Then Locate , 68: Print "    0.0";
    If compareA!(idx, 10) > 0 Then Locate , 68: Print Using "#####.#"; ((compareA!(idx, 10) + 30) / 60) / compareA!(idx, 0);

    Color 7, 0: Print String$(2, 177)
    Locate , 16: Print String$(61, 177)

End Sub
