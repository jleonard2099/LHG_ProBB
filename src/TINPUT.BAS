'----------------------------------------
'           InputTeam Subroutine
'----------------------------------------
'This subroutine provodes the user with
'several options for managing a team and
'it's roster.
Sub InputTeam (menuShortcut%)

    _Title "Basketball: The Pro Game - Team Input"

    ReDim menuChoices$(1 To 5)

    If menuShortcut% > 0 Then

        Call DoTheInput(menuShortcut%)

    Else

        Do

            Cls
            Color 15, 0

            Call TITLE

            menuChoices$(1) = "INPUT A NEW TEAM     "
            menuChoices$(2) = "EDIT AN EXISTING TEAM"
            menuChoices$(3) = "PRINT A TEAM ROSTER  "
            menuChoices$(4) = "VIEW A TEAM ROSTER   "
            menuChoices$(5) = "EXIT TO MAIN MENU    "
            SELECTION% = 1
            Count% = 5
            Start% = 1
            ExitCode% = 5
            FileFGColor = 15: FileBGColor = 1
            HiLiteFGColor% = 0: HiLiteBGColor% = 7
            Spacing% = 1
            Row% = 8
            Column% = 25
            menuWidth% = Len(menuChoices$(1))
            Column% = (80 - menuWidth% - 2) / 2

            Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

            _MouseShow "DEFAULT"
            MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
            _MouseHide

            If (SELECTION% >= 1) And (SELECTION% <= 4) Then Call DoTheInput(SELECTION%)

        Loop Until SELECTION% = 5 Or ExitCode% = 27

    End If

End Sub

'----------------------------------------
'         DoTheInput Subroutine
'----------------------------------------
'This subroutine assists the user with
'processing the chosen action from the
'menu
Sub DoTheInput (action%)

    Shared playerRatings!(), teamRatings()
    Shared playerNames$(), teamInfo$()

    Dim P$(2)

    Dim NTMS As Integer

    Color , 0

    'Positions
    P$(0) = "G"
    P$(1) = "F"
    P$(2) = "C"

    Select Case action%

        '1 = ENTER A NEW TEAM
        Case 1:

            Do

                Cls

                Print
                Color 12: Print "DO YOU HAVE THE FOLLOWING INFORMATION :"

                Print
                Color 14: Print "(1) ";
                Color 15: Print "TEAM/INDIVID STEALS? (Y/N): ";

                Do
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

                If UCase$(I$) = "Y" Then I = 1
                If UCase$(I$) = "N" Then I = 0

                If I$ <> Chr$(27) Then

                    Print UCase$(I$)

                    ST = I
                    Print
                    Color 14: Print "(2) ";
                    Color 15: Print "TEAM/INDIVID BLOCKS? (Y/N): ";

                    Do
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

                    If I$ <> Chr$(27) Then

                        If UCase$(I$) = "Y" Then I = 1
                        If UCase$(I$) = "N" Then I = 0

                        Print UCase$(I$)

                        BK = I

                        Print
                        Color 14: Print "(3) ";
                        Color 15: Print "TEAM/INDIVID OFF/DEF REB? (Y/N): ";

                        Do
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

                        If UCase$(I$) = "Y" Then I = 1
                        If UCase$(I$) = "N" Then I = 0

                        If I$ <> Chr$(27) Then

                            Print UCase$(I$)

                            RB = I

                            Print
                            Color 14: Print "(4) ";
                            Color 15: Print "TEAM TURNOVERS? (Y/N): ";

                            Do
                                I$ = GetKeyPress$
                            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

                            If UCase$(I$) = "Y" Then I = 1
                            If UCase$(I$) = "N" Then I = 0

                            If I$ <> Chr$(27) Then

                                Print UCase$(I$)

                                'We never do anything with this???
                                T0 = I
                                Print

                                Do
                                    Locate 12, 1
                                    Color 11
                                    Print "ANY CHANGE?";
                                    Color 15: Print " (Y/N)";
                                    I$ = GetKeyPress$
                                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

                                If UCase$(I$) = "Y" Then I = 1
                                If UCase$(I$) = "N" Then I = 0

                            End If

                        End If

                    End If

                End If

                If I$ = Chr$(27) Then action% = 5: Exit Sub

            Loop Until I = 0 'User wants no change

            Call InputEditTeam(1, year$, teamNbr, ST, BK)
            Call SaveTeamFile(year$, teamNbr, action%)

            '2 = CHANGE OR EDIT AN EXISTING TEAM
        Case 2:
            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr <> 0 Then
                Call InputEditTeam(2, YN$, teamNbr, ST, BK)
                Call SaveTeamFile(YN$, teamNbr, action%)
            End If

            '3 = PRINT A TEAM ROSTER
        Case 3:
            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr <> 0 Then

                userKey$ = GetPrintOptions$

                If userKey$ <> Chr$(27) Then

                    Select Case userKey$

                        Case "F"
                            userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                            If userFile$ <> "" Then
                                Call PrintRoster(0, userFile$, YN$)
                            End If

                        Case "P"
                            Call PrintRoster(1, "REPORT", YN$)

                    End Select

                End If

            End If

            '4 = VIEW A TEAM ROSTER
        Case 4:
            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr <> 0 Then
                Call ViewRoster
            End If

    End Select

End Sub


'----------------------------------------
'       PrintRoster Subroutine
'----------------------------------------
'This subroutine sends the selected
'team's roster to the default printer
Sub PrintRoster (printDest, destFile$, YN$)

    Shared playerRatings!(), teamRatings()
    Shared playerNames$(), teamInfo$()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    XCoord = (80 - Len(teamInfo$(0))) / 2

    Print #1,
    Print #1, Tab(XCoord); teamInfo$(0); " "; teamInfo$(1)
    Print #1, Tab(20);
    Print #1, "TEXT COLOR:"; teamRatings(33); Tab(38);
    Print #1, "BACKGROUND COLOR:"; teamRatings(34)
    Print #1, Tab(30); "WON - ";
    Print #1, Using "##"; teamRatings(1) + teamRatings(3);
    Print #1, Tab(43); "LOST - ";
    Print #1, Using "##"; teamRatings(2) + teamRatings(4)

    'Print #1, Using "RECORD ##"; teamRatings(1) + teamRatings(3);
    'Print #1, "-";
    'Print #1, Using "##    HOME ##"; teamRatings(2) + teamRatings(4); teamRatings(1);
    'Print #1, "-";
    'Print #1, Using "##    ROAD ##"; teamRatings(2); teamRatings(3);
    'Print #1, "-";
    'Print #1, Using "##"; teamRatings(4)
    Print #1,
    Print #1, "NAME          POS   G  MIN  FGA  FGM  PCT  FTA  FTM  PCT OREB DREB ASST  FLS"

    For I = 0 To 14

        If playerNames$(I, 0) <> "" And playerNames$(I, 0) <> "XXX" Then

            'Name
            Print #1, Left$(playerNames$(I, 0), 14);

            'Position
            Print #1, Tab(15); playerNames$(I, 1);

            'Games
            Print #1, Tab(20); Using "##"; playerRatings!(I, 0);

            'Minutes
            Print #1, Tab(23); Using "####"; playerRatings!(I, 1);

            'FGA
            Print #1, Tab(28); Using "####"; playerRatings!(I, 2);

            'FGM
            Print #1, Tab(33); Using "####"; playerRatings!(I, 3);

            'FG PCT
            If playerRatings!(I, 3) = 0 Then
                Print #1, Tab(38); ".000";
            End If
            If playerRatings!(I, 3) > 0 Then
                fgPCT! = playerRatings!(I, 3) / playerRatings!(I, 2)
                Print #1, Tab(38); Using ".###"; fgPCT!;
            End If

            'FTA
            Print #1, Tab(43); Using "####"; playerRatings!(I, 4);

            'FTM
            Print #1, Tab(48); Using "####"; playerRatings!(I, 5);

            'FT PCT
            If playerRatings!(I, 5) = 0 Then
                Print #1, Tab(53); ".000";
            End If
            If playerRatings!(I, 5) > 0 Then
                ftPCT! = playerRatings!(I, 5) / playerRatings!(I, 4)
                Print #1, Tab(53); Using ".###"; ftPCT!;
            End If

            'Off Reb
            Print #1, Tab(58); Using "####"; playerRatings!(I, 6);

            'Def Reb
            Print #1, Tab(63); Using "####"; playerRatings!(I, 7);

            'Assists
            Print #1, Tab(68); Using "####"; playerRatings!(I, 8);

            'Fouls
            Print #1, Tab(73); Using "####"; playerRatings!(I, 9)

        End If

    Next

    Print #1,
    Print #1, "TOTALS - OFF";

    'Games
    Print #1, Tab(20); Using "##"; leagueRatings(0);

    'FGA
    Print #1, Tab(28); Using "####"; teamRatings(5);

    'FGM
    Print #1, Tab(33); Using "####"; teamRatings(6);

    'FG PCT
    fgPCT! = teamRatings(6) / teamRatings(5)
    Print #1, Tab(38); Using ".###"; fgPCT!;

    'FTA
    Print #1, Tab(43); Using "####"; teamRatings(7);

    'FTM
    Print #1, Tab(48); Using "####"; teamRatings(8);

    'FT PCT
    ftPCT! = teamRatings(8) / teamRatings(7)
    Print #1, Tab(53); Using ".###"; ftPCT!;

    'Off Reb
    Print #1, Tab(58); Using "####"; teamRatings(9);

    'Def Reb
    Print #1, Tab(63); Using "####"; teamRatings(10);

    'Assists
    Print #1, Tab(68); Using "####"; teamRatings(11);

    'Fouls
    Print #1, Tab(73); Using "####"; teamRatings(12)

    Print #1, "TOTALS - DEF";

    'FGA
    Print #1, Tab(28); Using "####"; teamRatings(19);

    'FGM
    Print #1, Tab(33); Using "####"; teamRatings(20);

    'FG PCT
    fgPCT! = teamRatings(20) / teamRatings(19)
    Print #1, Tab(38); Using ".###"; fgPCT!;

    'FTA
    Print #1, Tab(43); Using "####"; teamRatings(21);

    'FTM
    Print #1, Tab(48); Using "####"; teamRatings(22);

    ftPCT! = teamRatings(22) / teamRatings(21)
    Print #1, Tab(53); Using ".###"; ftPCT!;

    'Off Reb
    Print #1, Tab(58); Using "####"; teamRatings(23);

    'Def Reb
    Print #1, Tab(63); Using "####"; teamRatings(24);

    'Assists
    Print #1, Tab(68); Using "####"; teamRatings(25);

    'Fouls
    Print #1, Tab(73); Using "####"; teamRatings(26)

    'NEW PAGE
    Print #1, Chr$(12)

    Print #1,
    Print #1, Tab(XCoord); teamInfo$(0); " "; teamInfo$(1)
    Print #1, Tab(30); "WON - ";
    Print #1, Using "##"; teamRatings(1) + teamRatings(3);
    Print #1, Tab(43); "LOST - ";
    Print #1, Using "##"; teamRatings(2) + teamRatings(4)

    ' Second page of ratings
    Print #1, "NAME           STL  BLK 3FGA 3FGM  PCT   HI-GAME"
    For I = 0 To 14

        If Left$(playerNames$(I, 0), 3) <> "XXX" Then

            'Name
            Print #1, Left$(playerNames$(I, 0), 14);

            'Steals
            Print #1, Tab(15); Using "####"; playerRatings!(I, 11);

            'Blocks
            Print #1, Tab(20); Using "####"; playerRatings!(I, 12);

            '3FGA
            Print #1, Tab(25); Using "####"; playerRatings!(I, 13);

            '3FGM
            Print #1, Tab(30); Using "####"; playerRatings!(I, 14);

            '3FG PCT
            If playerRatings!(I, 14) = 0 Then
                Print #1, Tab(35); ".000";
            End If
            If playerRatings!(I, 14) > 0 Then
                threeFGPCT! = playerRatings!(I, 14) / playerRatings!(I, 13)
                Print #1, Tab(35); Using ".###"; threeFGPCT!;
            End If

            'HI GAME
            Print #1, Tab(43); Using "####"; playerRatings!(I, 15)
            'Print #1, Tab(53); USING "####"; playerRatings!(I, 16);
            'Print #1, Tab(65); USING "####"; playerRatings!(I, 17)

        End If

    Next

    Print #1,
    Print #1, "TOTALS - OFF";

    'Steals
    Print #1, Tab(15); Using "####"; teamRatings(14);

    'Blocks
    Print #1, Tab(20); Using "####"; teamRatings(16);

    '3FGA
    Print #1, Tab(25); Using "####"; teamRatings(17);

    '3FGM
    Print #1, Tab(30); Using "####"; teamRatings(18);

    '3FG PCT
    If teamRatings(18) = 0 Then Print #1, Tab(35); ".000"

    If teamRatings(18) > 0 Then
        statPCT! = teamRatings(18) / teamRatings(17)
        Print #1, Tab(35); Using ".###"; statPCT!
    End If

    Print #1, "TOTALS - DEF";

    'Steals
    Print #1, Tab(15); Using "####"; teamRatings(28);

    'Blocks
    Print #1, Tab(20); Using "####"; teamRatings(30);

    '3FGA
    Print #1, Tab(25); Using "####"; teamRatings(31);

    '3FGM
    Print #1, Tab(30); Using "####"; teamRatings(32);

    '3FG PCT
    If teamRatings(32) = 0 Then Print #1, Tab(35); ".000"

    If teamRatings(32) > 0 Then
        statPCT! = teamRatings(32) / teamRatings(31)
        Print #1, Tab(35); Using ".###"; statPCT!
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
'       ViewRoster Subroutine
'----------------------------------------
'This subroutine displays the selected
'team's roster on the end user display
Sub ViewRoster ()

    Shared playerRatings!(), leagueRatings(), teamRatings()
    Shared playerNames$(), teamInfo$()

    Do

        Color 0, 0
        Cls

        XCoord = (80 - Len(teamInfo$(0))) / 2
        Locate 2, XCoord
        Color teamRatings(33), teamRatings(34): Print teamInfo$(0); " "; teamInfo$(1)

        Color 14, 0
        Print Tab(30); "WON - ";
        Color 15
        Print Using "##"; teamRatings(1) + teamRatings(3);

        Color 14
        Print Tab(43); "LOST - ";
        Color 15
        Print Using "##"; teamRatings(2) + teamRatings(4)

        Color 15, 0
        Locate , 20
        Print "TEXT COLOR      : "; teamRatings(33); Tab(38);
        Print "BACKGROUND COLOR: "; teamRatings(34)

        Locate 6, 1
        Color 14
        Print "NAME          POS   G  MIN  FGA  FGM  PCT  FTA  FTM  PCT OREB DREB ASST  FLS"
        Color 15

        For I = 0 To 14

            If Left$(playerNames$(I, 0), 3) <> "XXX" Then

                'Name
                Color 3: Print Left$(playerNames$(I, 0), 14);

                'Position
                Color 7: Print Tab(15); playerNames$(I, 1);

                'Games
                Print Tab(20); Using "##"; playerRatings!(I, 0);

                'Minutes
                Print Tab(23); Using "####"; playerRatings!(I, 1);

                'FGA
                Print Tab(28); Using "####"; playerRatings!(I, 2);

                'FGM
                Print Tab(33); Using "####"; playerRatings!(I, 3);

                'FG PCT
                If playerRatings!(I, 3) = 0 Then
                    Print Tab(38); ".000";
                End If
                If playerRatings!(I, 3) > 0 Then
                    fgPCT! = playerRatings!(I, 3) / playerRatings!(I, 2)
                    Print Tab(38); Using ".###"; fgPCT!;
                End If

                'FTA
                Print Tab(43); Using "####"; playerRatings!(I, 4);

                'FTM
                Print Tab(48); Using "####"; playerRatings!(I, 5);

                'FT PCT
                If playerRatings!(I, 5) = 0 Then
                    Print Tab(53); ".000";
                End If
                If playerRatings!(I, 5) > 0 Then
                    ftPCT! = playerRatings!(I, 5) / playerRatings!(I, 4)
                    Print Tab(53); Using ".###"; ftPCT!;
                End If

                'Off Reb
                Print Tab(58); Using "####"; playerRatings!(I, 6);

                'Def Reb
                Print Tab(63); Using "####"; playerRatings!(I, 7);

                'Assists
                Print Tab(68); Using "####"; playerRatings!(I, 8);

                'Fouls
                Print Tab(73); Using "####"; playerRatings!(I, 9)

            End If

        Next

        Color 12
        Print
        Print "TOTALS - OFF";

        'Games
        Print Tab(20); Using "##"; leagueRatings(0);

        'FGA
        Print Tab(28); Using "####"; teamRatings(5);

        'FGM
        Print Tab(33); Using "####"; teamRatings(6);

        'FG PCT
        fgPCT! = teamRatings(6) / teamRatings(5)
        Print Tab(38); Using ".###"; fgPCT!;

        'FTA
        Print Tab(43); Using "####"; teamRatings(7);

        'FTM
        Print Tab(48); Using "####"; teamRatings(8);

        'FT PCT
        ftPCT! = teamRatings(8) / teamRatings(7)
        Print Tab(53); Using ".###"; ftPCT!;

        'Off Reb
        Print Tab(58); Using "####"; teamRatings(9);

        'Def Reb
        Print Tab(63); Using "####"; teamRatings(10);

        'Assists
        Print Tab(68); Using "####"; teamRatings(11);

        'Fouls
        Print Tab(73); Using "####"; teamRatings(12)

        Color 6
        Print "TOTALS - DEF";

        'FGA
        Print Tab(28); Using "####"; teamRatings(19);

        'FGM
        Print Tab(33); Using "####"; teamRatings(20);

        'FG PCT
        fgPCT! = teamRatings(20) / teamRatings(19)
        Print Tab(38); Using ".###"; fgPCT!;

        'FTA
        Print Tab(43); Using "####"; teamRatings(21);

        'FTM
        Print Tab(48); Using "####"; teamRatings(22);

        ftPCT! = teamRatings(22) / teamRatings(21)
        Print Tab(53); Using ".###"; ftPCT!;

        'Off Reb
        Print Tab(58); Using "####"; teamRatings(23);

        'Def Reb
        Print Tab(63); Using "####"; teamRatings(24);

        'Assists
        Print Tab(68); Using "####"; teamRatings(25);

        'Fouls
        Print Tab(73); Using "####"; teamRatings(26)

        Color 14
        Locate LAST_ROW - 2, 28
        Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

        Cls

        XCoord = (80 - Len(teamInfo$(0))) / 2
        Locate 2, XCoord
        Color teamRatings(33), teamRatings(34): Print teamInfo$(0); " "; teamInfo$(1)

        Color 14, 0
        Print Tab(30); "WON - ";
        Color 15
        Print Using "##"; teamRatings(1) + teamRatings(3);

        Color 14
        Print Tab(43); "LOST - ";
        Color 15
        Print Using "##"; teamRatings(2) + teamRatings(4)

        Color 15, 0
        Locate , 15
        Print "TEXT COLOR      : "; teamRatings(33); Tab(45);
        Print "BACKGROUND COLOR: "; teamRatings(34)

        Locate 6, 1

        Color 14
        Print "NAME           STL  BLK 3FGA 3FGM  PCT   HI-GAME"
        Color 15

        For I = 0 To 14

            If Left$(playerNames$(I, 0), 3) <> "XXX" Then

                'Name
                Color 3: Print Left$(playerNames$(I, 0), 14);

                'Steals
                Color 7: Print Tab(15); Using "####"; playerRatings!(I, 11);

                'Blocks
                Print Tab(20); Using "####"; playerRatings!(I, 12);

                '3FGA
                Print Tab(25); Using "####"; playerRatings!(I, 13);

                '3FGM
                Print Tab(30); Using "####"; playerRatings!(I, 14);

                '3FG PCT
                If playerRatings!(I, 14) = 0 Then
                    Print Tab(35); ".000";
                End If
                If playerRatings!(I, 14) > 0 Then
                    threeFGPCT! = playerRatings!(I, 14) / playerRatings!(I, 13)
                    Print Tab(35); Using ".###"; threeFGPCT!;
                End If

                'HI GAME
                Print Tab(43); Using "####"; playerRatings!(I, 15)
                'PRINT TAB(53); USING "####"; playerRatings!(I, 16);
                'PRINT TAB(65); USING "####"; playerRatings!(I, 17)

            End If

        Next

        Color 12
        Print
        Print "TOTALS - OFF";

        'Steals
        Print Tab(15); Using "####"; teamRatings(14);

        'Blocks
        Print Tab(20); Using "####"; teamRatings(16);

        '3FGA
        Print Tab(25); Using "####"; teamRatings(17);

        '3FGM
        Print Tab(30); Using "####"; teamRatings(18);

        '3FG PCT
        If teamRatings(18) = 0 Then Print Tab(35); ".000"

        If teamRatings(18) > 0 Then
            statPCT! = teamRatings(18) / teamRatings(17)
            Print Tab(35); Using ".###"; statPCT!
        End If

        Color 6
        Print "TOTALS - DEF";

        'Steals
        Print Tab(15); Using "####"; teamRatings(28);

        'Blocks
        Print Tab(20); Using "####"; teamRatings(30);

        '3FGA
        Print Tab(25); Using "####"; teamRatings(31);

        '3FGM
        Print Tab(30); Using "####"; teamRatings(32);

        '3FG PCT
        If teamRatings(32) = 0 Then Print Tab(35); ".000"

        If teamRatings(32) > 0 Then
            statPCT! = teamRatings(32) / teamRatings(31)
            Print Tab(35); Using ".###"; statPCT!
        End If

        Do
            Color 15
            Locate LAST_ROW - 2, 31
            Color 14
            Print "VIEW AGAIN? (Y/N)"
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

End Sub


'----------------------------------------
'        InputEditTeam Subroutine
'----------------------------------------
'This subroutine assists the user with the
'steps for entering or editing a team.

'inputAction%
'   1 = New Team
'   2 = Edit Team

'NOTE: <ENTER> key is valued as 0 for when
'a user is prompted to press keys to CONTINUE
'This improvement over the original makes it
'faster and easier to proceed through screens
Sub InputEditTeam (inputAction%, yearNumber$, teamIdx, haveSteals, haveBlocks)

    Shared teamAttendance&

    Shared playerRatings!(), leagueRatings(), teamRatings()

    Shared playerNames$(), teamInfo$()

    Color 15, 0
    Cls

    '--------------------
    ' League Info
    '--------------------
    Locate 2, 1

    If inputAction% = 1 Then
        'Input Team
        Color 14
        Print "LEAGUE INFORMATION"
        Color 15

        Locate 4, 1: Input "1. NUMBER OF GAMES "; leagueRatings(0)
        Locate 4, 20: Print " "
        Locate 5, 1: Input "2. AVG FGA/G "; leagueRatings(1)
        Locate 5, 14: Print " "
        Locate 6, 1: Input "3. AVG RB/G "; leagueRatings(2)
        Locate 6, 13: Print " "
        Locate 7, 1: Input "4. LEAGUE FG% "; leagueRatings(3)
        Locate 7, 15: Print " "
        Locate 8, 1: Input "5. AVG PF/G "; leagueRatings(4)
        Locate 8, 13: Print " "
        Locate 9, 1: Input "6. LEAGUE 3FG% "; leagueRatings(5)
        Locate 9, 16: Print " "
        Locate 10, 1: Input "7. LEAGUE POWER ADJ "; leagueRatings(6)
        Locate 10, 21: Print " "

    Else
        'Modify Team
        Color 14
        Print "CHANGE LEAGUE INFORMATION"
        Color 15

        Locate 4, 1
        Print "1. NUMBER OF GAMES "; leagueRatings(0)
        Print "2. AVG FGA/G "; leagueRatings(1)
        Print "3. AVG RB/G "; leagueRatings(2)
        Print "4. LEAGUE FG% "; leagueRatings(3)
        Print "5. AVG PF/G "; leagueRatings(4)
        Print "6. LEAGUE 3FG% "; leagueRatings(5)
        Print "7. LEAGUE POWER ADJ "; leagueRatings(6)

    End If

    Do

        Do
            Locate 12, 29
            Print Space$(29)
            Color 11
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
            Color 15
        Loop Until B9 >= 0 And B9 <= 7

        Select Case B9

            Case 1
                Locate 4, 19: Print Space$(10)
                Locate 4, 1: Input "1. NUMBER OF GAMES"; leagueRatings(0)
                Locate 4, 19: Print " "

            Case 2
                Locate 5, 13: Print Space$(10)
                Locate 5, 1: Input "2. AVG FGA/G"; leagueRatings(1)
                Locate 5, 13: Print " "

            Case 3
                Locate 6, 12: Print Space$(10)
                Locate 6, 1: Input "3. AVG RB/G"; leagueRatings(2)
                Locate 6, 12: Print " "

            Case 4
                Locate 7, 14: Print Space$(10)
                Locate 7, 1: Input "4. LEAGUE FG%"; leagueRatings(3)
                Locate 7, 14: Print " "

            Case 5
                Locate 8, 12: Print Space$(10)
                Locate 8, 1: Input "5. AVG PF/G"; leagueRatings(4)
                Locate 8, 12: Print " "

            Case 6
                Locate 9, 15: Print Space$(10)
                Locate 9, 1: Input "6. LEAGUE 3FG%"; leagueRatings(5)
                Locate 9, 15: Print " "

            Case 7
                Locate 10, 20: Print Space$(10)
                Locate 10, 1: Input "7. LEAGUE POWER ADJ"; leagueRatings(6)
                Locate 10, 20: Print " "

        End Select

    Loop Until B9 = 0

    '----------
    ' Team Info
    '----------
    If inputAction% = 1 Then

        Cls

        Locate 2, 1
        Color 14
        Print "TEAM INFORMATION"
        Color 15

        Locate 4: Input "1. TEAM NUMBER (BLANK TO AUTO-ASSIGN) "; teamIdx
        Locate 4, 39: Print " "
        Locate 5: Input "2. DISK ID "; yearNumber$
        Locate 5, 12: Print " "
        Locate 6: Input "3. TEAM NAME "; teamInfo$(0)
        Locate 6, 14: Print " "
        Locate 7: Input "4. TEAM NICKNAME "; teamInfo$(1)
        Locate 7, 18: Print " "
        Locate 8: Input "5. TEAM GAMES PLAYED "; teamRatings(0)
        Locate 8, 22: Print " "
        Locate 9: Input "6. HOME WINS "; teamRatings(1)
        Locate 9, 14: Print " "
        Locate 10: Input "7. HOME LOSSES "; teamRatings(2)
        Locate 10, 16: Print " "
        Locate 11: Input "8. ROAD WINS "; teamRatings(3)
        Locate 11, 14: Print " "
        Locate 12: Input "9. ROAD LOSSES "; teamRatings(4)
        Locate 12, 16: Print " "

        Locate 13: Input "10. TEXT COLOR (0-15) "; teamRatings(33)
        Locate 13, 23: Print " "
        Locate 14: Input "11. BACKGROUND COLOR (0-7) "; teamRatings(34)
        Locate 14, 28: Print " "
        Locate 20
        Print "TEAM APPEARANCE: ";
        Color teamRatings(33), teamRatings(34)
        Print RTrim$(teamInfo$(0)) + " " + RTrim$(teamInfo$(1))
        Color 15, 0

        Locate 15: Input "12. TEAM ABBREV (IE. BOS) "; teamInfo$(2)
        Locate 15, 27: Print " "
        Locate 16: Input "13. COACH'S NAME "; teamInfo$(3)
        Locate 16, 18: Print " "
        Locate 17: Input "14. ARENA NAME "; teamInfo$(4)
        Locate 17, 16: Print " "
        Locate 18: Input "15. ATTENDANCE "; teamAttendance&
        Locate 18, 16: Print " "

    Else

        Cls

        Locate 2, 1
        Color 14
        Print "CHANGE TEAM INFORMATION"
        Color 15

        Locate 4, 1
        Print "1. TEAM NUMBER "; teamIdx
        Print "2. DISK ID  "; yearNumber$
        Print "3. TEAM NAME  "; teamInfo$(0)
        Print "4. TEAM NICKNAME  "; teamInfo$(1)
        Print "5. TEAM GAMES PLAYED "; teamRatings(0)
        Print "6. HOME WINS "; teamRatings(1)
        Print "7. HOME LOSSES "; teamRatings(2)
        Print "8. ROAD WINS "; teamRatings(3)
        Print "9. ROAD LOSSES "; teamRatings(4)
        Print "10. TEXT COLOR (0-15) "; teamRatings(33)
        Print "11. BACKGROUND COLOR (0-7) "; teamRatings(34)
        Print "12. TEAM ABBREV (IE. BOS) "; teamInfo$(2)
        Print "13. COACH'S NAME  "; teamInfo$(3)
        Print "14. ARENA NAME  "; teamInfo$(4)
        Print "15. ATTENDANCE  "; teamAttendance&

        Locate 20
        Print "TEAM APPEARANCE: ";
        Color teamRatings(33), teamRatings(34)
        Print RTrim$(teamInfo$(0)) + " " + RTrim$(teamInfo$(1))
        Color 15, 0

    End If

    Do

        Do
            Locate 22, 28
            Print Space$(10)
            Color 11
            Locate 22, 1
            Input "# TO CHANGE (0 TO CONTINUE)"; B9
            Color 15
        Loop Until B9 >= 0 And B9 <= 15

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 4, 15: Print Space$(10)
                    Locate 4, 1: Input "1. TEAM NUMBER"; teamIdx
                    Locate 4, 15: Print " "

                Case 2
                    Locate 5, 11: Print Space$(10)
                    Locate 5, 1: Input "2. DISK ID"; yearNumber$
                    Locate 5, 11: Print " "

                Case 3
                    Locate 6, 13: Print Space$(20)
                    Locate 6, 1: Input "3. TEAM NAME"; teamInfo$(0)
                    Locate 6, 13: Print " "

                    Locate 20
                    Print "TEAM APPEARANCE: ";
                    Color teamRatings(33), teamRatings(34)
                    Print RTrim$(teamInfo$(0)) + " " + RTrim$(teamInfo$(1))
                    Color 15, 0

                Case 4
                    Locate 7, 17: Print Space$(15)
                    Locate 7, 1: Input "4. TEAM NICKNAME"; teamInfo$(1)
                    Locate 7, 17: Print " "

                    Locate 20
                    Print "TEAM APPEARANCE: ";
                    Color teamRatings(33), teamRatings(34)
                    Print RTrim$(teamInfo$(0)) + " " + RTrim$(teamInfo$(1))
                    Color 15, 0

                Case 5
                    Locate 8, 21: Print Space$(10)
                    Locate 8, 1: Input "5. TEAM GAMES PLAYED"; teamRatings(0)
                    Locate 8, 21: Print " "

                Case 6
                    Locate 9, 13: Print Space$(10)
                    Locate 9, 1: Input "6. HOME WINS"; teamRatings(1)
                    Locate 9, 13: Print " "

                Case 7
                    Locate 10, 15: Print Space$(10)
                    Locate 10, 1: Input "7. HOME LOSSES"; teamRatings(2)
                    Locate 10, 15: Print " "

                Case 8
                    Locate 11, 13: Print Space$(10)
                    Locate 11, 1: Input "8. ROAD WINS"; teamRatings(3)
                    Locate 11, 13: Print " "

                Case 9
                    Locate 12, 15: Print Space$(10)
                    Locate 12, 1: Input "9. ROAD LOSSES"; teamRatings(4)
                    Locate 12, 15: Print " "

                Case 10
                    Locate 13, 22: Print Space$(10)
                    Locate 13, 1: Input "10. TEXT COLOR (0-15)"; teamRatings(33)
                    Locate 13, 22: Print " "

                    Locate 20
                    Print "TEAM APPEARANCE: ";
                    Color teamRatings(33), teamRatings(34)
                    Print RTrim$(teamInfo$(0)) + " " + RTrim$(teamInfo$(1))
                    Color 15, 0

                Case 11
                    Locate 14, 27: Print Space$(10)
                    Locate 14, 1: Input "11. BACKGROUND COLOR (0-7)"; teamRatings(34)
                    Locate 14, 27: Print " "

                    Locate 20
                    Print "TEAM APPEARANCE: ";
                    Color teamRatings(33), teamRatings(34)
                    Print RTrim$(teamInfo$(0)) + " " + RTrim$(teamInfo$(1))
                    Color 15, 0

                Case 12
                    Locate 15, 26: Print Space$(10)
                    Locate 15, 1: Input "12. TEAM ABBREV (IE. BOS)"; teamInfo$(2)
                    Locate 15, 26: Print " "

                Case 13
                    Locate 16, 17: Print Space$(20)
                    Locate 16, 1: Input "13. COACH'S NAME"; teamInfo$(3)
                    Locate 16, 17: Print " "

                Case 14
                    Locate 17, 15: Print Space$(40)
                    Locate 17, 1: Input "14. ARENA NAME"; teamInfo$(4)
                    Locate 17, 15: Print " "

                Case 15
                    Locate 18, 15: Print Space$(10)
                    Locate 18, 1: Input "15. ATTENDANCE"; teamAttendance&
                    Locate 18, 15: Print " "

            End Select

        End If

    Loop Until B9 = 0


    '---------------
    ' Team Stats
    '---------------
    If inputAction% = 1 Then

        Cls

        Locate 2, 12
        Color 12
        Print "TEAM STATISTICS"
        Color 14
        Locate 4, 15: Print " OFFENSE"; Tab(30); " DEFENSE"
        Color 15

        Locate 5, 1: Print " 1. FGM ";
        Locate 5, 14: Input teamRatings(6)
        Locate 5, 14: Print " "
        Locate 5, 29: Input teamRatings(20)
        Locate 5, 29: Print " "

        Locate 6, 1: Print " 2. FGA ";
        Locate 6, 14: Input teamRatings(5)
        Locate 6, 14: Print " "
        Locate 6, 29: Input teamRatings(19)
        Locate 6, 29: Print " "

        Locate 7, 1: Print " 3. 3-FGM ";
        Locate 7, 14: Input teamRatings(18)
        Locate 7, 14: Print " "
        Locate 7, 29: Input teamRatings(32)
        Locate 7, 29: Print " "

        Locate 8, 1: Print " 4. 3-FGA ";
        Locate 8, 14: Input teamRatings(17)
        Locate 8, 14: Print " "
        Locate 8, 29: Input teamRatings(31)
        Locate 8, 29: Print " "

        Locate 9, 1: Print " 5. FTM ";
        Locate 9, 14: Input teamRatings(8)
        Locate 9, 14: Print " "
        Locate 9, 29: Input teamRatings(22)
        Locate 9, 29: Print " "

        Locate 10, 1: Print " 6. FTA ";
        Locate 10, 14: Input teamRatings(7)
        Locate 10, 14: Print " "
        Locate 10, 29: Input teamRatings(21)
        Locate 10, 29: Print " "

        Locate 11, 1: Print " 7. OFF REB ";
        Locate 11, 14: Input teamRatings(9)
        Locate 11, 14: Print " "
        Locate 11, 29: Input teamRatings(23)
        Locate 11, 29: Print " "

        Locate 12, 1: Print " 8. DEF REB ";
        Locate 12, 14: Input teamRatings(10)
        Locate 12, 14: Print " "
        Locate 12, 29: Input teamRatings(24)
        Locate 12, 29: Print " "

        'Calculate Rebounds if missing
        If teamRatings(9) = 0 And RB = 0 Then

            'Off Rebounds = ~ Def Rebounds / 3
            teamRatings(9) = .32 * teamRatings(10)
            teamRatings(10) = teamRatings(10) - teamRatings(9)

            Locate 11, 1: Print " 7. OFF REB ";
            Locate 11, 14: Input teamRatings(9)
            Locate 11, 14: Print " "
            Locate 11, 29: Input teamRatings(23)
            Locate 11, 29: Print " "

            Locate 12, 1: Print " 8. DEF REB ";
            Locate 12, 14: Input teamRatings(10)
            Locate 12, 14: Print " "
            Locate 12, 29: Input teamRatings(24)
            Locate 12, 29: Print " "

        End If

        Locate 13, 1: Print " 9. ASSISTS ";
        Locate 13, 14: Input teamRatings(11)
        Locate 13, 14: Print " "
        Locate 13, 29: Input teamRatings(25)
        Locate 13, 29: Print " "

        Locate 14, 1: Print "10. STEALS ";
        Locate 14, 14: Input teamRatings(14)
        Locate 14, 14: Print " "
        Locate 14, 29: Input teamRatings(28)
        Locate 14, 29: Print " "

        Locate 15, 1: Print "11. BLK SHOTS ";
        Locate 15, 14: Input teamRatings(16)
        Locate 15, 14: Print " "
        Locate 15, 29: Input teamRatings(30)
        Locate 15, 29: Print " "

        Locate 16, 1: Print "12. TURNOVERS ";
        Locate 16, 14: Input teamRatings(15)
        Locate 16, 14: Print " "
        Locate 16, 29: Input teamRatings(29)
        Locate 16, 29: Print " "

        Locate 17, 1: Print "13. FOULS ";
        Locate 17, 14: Input teamRatings(12)
        Locate 17, 14: Print " "
        Locate 17, 29: Input teamRatings(26)
        Locate 17, 29: Print " "

        Locate 18, 1: Print "14. DISQ ";
        Locate 18, 14: Input teamRatings(13)
        Locate 18, 14: Print " "
        Locate 18, 29: Input teamRatings(27)
        Locate 18, 29: Print " "

        '-- We currently don't have a a calculation for if this data is missing

    Else

        Cls

        Locate 2, 8
        Color 12
        Print "CHANGE TEAM STATISTICS"
        Color 14
        Locate 4, 15: Print "OFFENSE"; Tab(30); "DEFENSE"
        Color 15

        Print " 1. FGM "; Tab(15); teamRatings(6); Tab(30); teamRatings(20)
        Print " 2. FGA "; Tab(15); teamRatings(5); Tab(30); teamRatings(19)
        Print " 3. 3-FGM "; Tab(15); teamRatings(18); Tab(30); teamRatings(32)
        Print " 4. 3-FGA "; Tab(15); teamRatings(17); Tab(30); teamRatings(31)
        Print " 5. FTM "; Tab(15); teamRatings(8); Tab(30); teamRatings(22)
        Print " 6. FTA "; Tab(15); teamRatings(7); Tab(30); teamRatings(21)
        Print " 7. OFF REB "; Tab(15); teamRatings(9); Tab(30); teamRatings(23)
        Print " 8. DEF REB "; Tab(15); teamRatings(10); Tab(30); teamRatings(24)
        Print " 9. ASSISTS "; Tab(15); teamRatings(11); Tab(30); teamRatings(25)
        Print "10. STEALS "; Tab(15); teamRatings(14); Tab(30); teamRatings(28)
        Print "11. BLK SHOTS "; Tab(15); teamRatings(16); Tab(30); teamRatings(30)
        Print "12. TURNOVERS "; Tab(15); teamRatings(15); Tab(30); teamRatings(29)
        Print "13. FOULS "; Tab(15); teamRatings(12); Tab(30); teamRatings(26)
        Print "14. DISQ "; Tab(15); teamRatings(13); Tab(30); teamRatings(27)

    End If

    Do

        Do
            Locate 20, 28
            Print Space$(10)
            Locate 20, 1
            Color 11
            Input "# TO CHANGE (0 TO CONTINUE)"; B9
            Color 15
        Loop Until B9 >= 0 And B9 <= 14

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 5, 1: Print " 1. FGM ";
                    Locate 5, 14: Print Space$(10)
                    Locate 5, 14: Input teamRatings(6)
                    Locate 5, 14: Print " "
                    Locate 5, 29: Print Space$(10)
                    Locate 5, 29: Input teamRatings(20)
                    Locate 5, 29: Print " "

                Case 2
                    Locate 6, 1: Print " 2. FGA ";
                    Locate 6, 14: Print Space$(10)
                    Locate 6, 14: Input teamRatings(5)
                    Locate 6, 14: Print " "
                    Locate 6, 29: Print Space$(10)
                    Locate 6, 29: Input teamRatings(19)
                    Locate 6, 29: Print " "

                Case 3
                    Locate 7, 1: Print " 3. 3-FGM ";
                    Locate 7, 14: Print Space$(10)
                    Locate 7, 14: Input teamRatings(18)
                    Locate 7, 14: Print " "
                    Locate 7, 29: Print Space$(10)
                    Locate 7, 29: Input teamRatings(32)
                    Locate 7, 29: Print " "

                Case 4
                    Locate 8, 1: Print " 4. 3-FGA ";
                    Locate 8, 14: Print Space$(10)
                    Locate 8, 14: Input teamRatings(17)
                    Locate 8, 14: Print " "
                    Locate 8, 29: Print Space$(10)
                    Locate 8, 29: Input teamRatings(31)
                    Locate 8, 29: Print " "

                Case 5
                    Locate 9, 1: Print " 5. FTM ";
                    Locate 9, 14: Print Space$(10)
                    Locate 9, 14: Input teamRatings(8)
                    Locate 9, 14: Print " "
                    Locate 9, 29: Print Space$(10)
                    Locate 9, 29: Input teamRatings(22)
                    Locate 9, 29: Print " "

                Case 6
                    Locate 10, 1: Print " 6. FTA ";
                    Locate 10, 14: Print Space$(10)
                    Locate 10, 14: Input teamRatings(7)
                    Locate 10, 14: Print " "
                    Locate 10, 29: Print Space$(10)
                    Locate 10, 29: Input teamRatings(21)
                    Locate 10, 29: Print " "

                Case 7
                    Locate 11, 1: Print " 7. OFF REB ";
                    Locate 11, 14: Print Space$(10)
                    Locate 11, 14: Input teamRatings(9)
                    Locate 11, 14: Print " "
                    Locate 11, 29: Print Space$(10)
                    Locate 11, 29: Input teamRatings(23)
                    Locate 11, 29: Print " "

                Case 8
                    Locate 12, 1: Print " 8. DEF REB ";
                    Locate 12, 14: Print Space$(10)
                    Locate 12, 14: Input teamRatings(10)
                    Locate 12, 14: Print " "
                    Locate 12, 29: Input teamRatings(24)
                    Locate 12, 29: Print Space$(10)
                    Locate 12, 29: Print " "

                    'Calculate Rebounds if missing
                    If teamRatings(9) = 0 And RB = 0 Then
                        'Off Rebounds = ~ Def Rebounds / 3
                        teamRatings(9) = .32 * teamRatings(10)
                        teamRatings(10) = teamRatings(10) - teamRatings(9)
                        Locate 11, 1: Print " 7. OFF REB ";
                        Locate 11, 14: Print Space$(10)
                        Locate 11, 14: Input teamRatings(9)
                        Locate 11, 14: Print " "
                        Locate 11, 29: Input teamRatings(23)
                        Locate 11, 29: Print Space$(10)
                        Locate 11, 29: Print " "

                        Locate 12, 1: Print " 8. DEF REB ";
                        Locate 12, 14: Print Space$(10)
                        Locate 12, 14: Input teamRatings(10)
                        Locate 12, 14: Print " "
                        Locate 12, 29: Input teamRatings(24)
                        Locate 12, 29: Print Space$(10)
                        Locate 12, 29: Print " "

                    End If

                Case 9
                    Locate 13, 1: Print " 9. ASSISTS ";
                    Locate 13, 14: Print Space$(10)
                    Locate 13, 14: Input teamRatings(11)
                    Locate 13, 14: Print " "
                    Locate 13, 29: Input teamRatings(25)
                    Locate 13, 29: Print Space$(10)
                    Locate 13, 29: Print " "

                Case 10
                    Locate 14, 1: Print "10. STEALS ";
                    Locate 14, 14: Print Space$(10)
                    Locate 14, 14: Input teamRatings(14)
                    Locate 14, 14: Print " "
                    Locate 14, 29: Input teamRatings(28)
                    Locate 14, 29: Print Space$(10)
                    Locate 14, 29: Print " "

                Case 11
                    Locate 15, 1: Print "11. BLK SHOTS ";
                    Locate 15, 14: Print Space$(10)
                    Locate 15, 14: Input teamRatings(16)
                    Locate 15, 14: Print " "
                    Locate 15, 14: Print Space$(10)
                    Locate 15, 29: Input teamRatings(30)
                    Locate 15, 29: Print Space$(10)
                    Locate 15, 29: Print " "

                Case 12
                    Locate 16, 1: Print "12. TURNOVERS ";
                    Locate 16, 14: Print Space$(10)
                    Locate 16, 14: Input teamRatings(15)
                    Locate 16, 14: Print " "
                    Locate 16, 29: Print Space$(10)
                    Locate 16, 29: Input teamRatings(29)
                    Locate 16, 29: Print " "

                Case 13
                    Locate 17, 1: Print "13. FOULS ";
                    Locate 17, 14: Print Space$(10)
                    Locate 17, 14: Input teamRatings(12)
                    Locate 17, 14: Print " "
                    Locate 17, 29: Print Space$(10)
                    Locate 17, 29: Input teamRatings(26)
                    Locate 17, 29: Print " "

                Case 14
                    Locate 18, 1: Print "14. DISQ ";
                    Locate 18, 14: Print Space$(10)
                    Locate 18, 14: Input teamRatings(13)
                    Locate 18, 14: Print " "
                    Locate 18, 29: Print Space$(10)
                    Locate 18, 29: Input teamRatings(27)
                    Locate 18, 29: Print " "

            End Select

        End If

    Loop Until B9 = 0


    '---------------
    ' Player Stats
    '---------------
    Cls

    If inputAction% = 2 Then

        'Edit Team
        Locate 2, 1
        Color 14

        Print "CHANGE PLAYER STATS"
        Color 15

        Do

            Locate 4, 1
            Print Space$(40)

            For I = 0 To 14:
                Locate 4 + I, 1
                Print Using "##"; I + 1;
                Print "  "; playerNames$(I, 0)
            Next

            Print

            Do
                Locate 26, 21
                Color 11
                Print "# TO CHANGE (1-15) (0 TO CONTINUE) ";
                Color 15
                Input idx
                Locate 1, 1

                If idx <> 0 Then

                    Cls

                    Color 12
                    Locate 2, 35: Print "PLAYER - "; idx + 1

                    Color 14
                    Print "CHANGE PLAYER STATS"

                    Color 15

                    Call EditPlayer(idx - 1, inputAction%)

                End If

            Loop Until idx >= 0 And idx <= 15

        Loop Until idx = 0

    Else

        'Calculate Steals/Blocks if missing
        'Def Steals
        If teamRatings(28) = 0 And haveSteals = 0 Then
            teamRatings(28) = teamRatings(5) * .1 'Off FGA / 10
        End If
        'Off Steals
        If teamRatings(14) = 0 And haveSteals = 0 Then
            teamRatings(14) = teamRatings(19) * .1 'Def FGA / 10
        End If
        'Def Blocks
        If teamRatings(30) = 0 And haveBlocks = 0 Then
            teamRatings(30) = teamRatings(5) * .06 'Off FGA / ~16
        End If
        'Off Blocks
        If teamRatings(16) = 0 And haveBlocks = 0 Then
            teamRatings(16) = teamRatings(19) * .06 'Def FGA / ~16
        End If

        'Input from scratch
        For I = 0 To 14

            Cls

            Color 12
            Locate 2, 35: Print "PLAYER - "; I + 1

            Color 14
            Print "ENTER PLAYER STATS"
            Color 15

            Locate 4: Input " 1. NAME (XXX TO QUIT) "; playerNames$(I, 0)
            Locate 4, 24: Print " "

            If playerNames$(I, 0) <> "XXX" Then

                Locate 5: Input " 2. POSITION "; playerNames$(I, 1)
                position$ = Left$(playerNames$(I, 1), 1)
                Locate 5, 14: Print " "

                Locate 6: Input " 3. GAMES "; playerRatings!(I, 0)
                Locate 6, 11: Print " "

                Locate 7: Input " 4. MINUTES "; playerRatings!(I, 1)
                Locate 7, 13: Print " "

                Locate 8: Input " 5. FGM "; playerRatings!(I, 3)
                Locate 8, 9: Print " "

                Locate 9: Input " 6. FGA "; playerRatings!(I, 2)
                Locate 9, 9: Print " "

                Locate 10: Input " 7. 3-FGM "; playerRatings!(I, 14)
                Locate 10, 11: Print " "

                Locate 11: Input " 8. 3-FGA "; playerRatings!(I, 13)
                Locate 11, 11: Print " "

                Locate 12, 1: Input " 9. FTM "; playerRatings!(I, 5)
                Locate 12, 9: Print " "

                Locate 13, 1: Input "10. FTA "; playerRatings!(I, 4)
                Locate 13, 9: Print " "

                Locate 14, 1: Input "11. OFF REB "; playerRatings!(I, 6)
                Locate 14, 13: Print " "

                Locate 15, 1: Input "12. DEF REB "; playerRatings!(I, 7)
                Locate 15, 13: Print " "

                'Calculate Rebounds if missing
                If playerRatings!(I, 6) = 0 And RB = 0 Then
                    'Off Rebounds = ~ Def Rebounds / 3
                    playerRatings!(I, 6) = .32 * playerRatings!(I, 7)
                    playerRatings!(I, 7) = playerRatings!(I, 7) - playerRatings!(I, 6)
                    Locate 14: Print "11. OFF REB "; playerRatings!(I, 6)
                    Locate 15: Print "12. DEF REB "; playerRatings!(I, 7)
                End If

                Locate 16, 1: Input "13. ASSISTS "; playerRatings!(I, 8)
                Locate 16, 13: Print " "

                If haveSteals = 1 Then
                    'Input steals
                    Locate 17: Input "14. STEALS "; playerRatings!(I, 11)
                    Locate 17, 12: Print " "

                Else
                    'Calculate steals based on position
                    Select Case position$

                        Case "C":
                            playerRatings!(I, 11) = playerRatings!(I, 1) / 48 * 1.2
                            Locate 17: Print "14. STEALS "; playerRatings!(I, 11)

                        Case "F":
                            playerRatings!(I, 11) = playerRatings!(I, 1) / 48 * 1.55
                            Locate 17: Print "14. STEALS "; playerRatings!(I, 11)

                        Case "G":
                            playerRatings!(I, 11) = playerRatings!(I, 1) / 48 * 2.3
                            Locate 17: Print "14. STEALS "; playerRatings!(I, 11)

                    End Select

                End If

                If haveBlocks = 1 Then
                    'Input blocks
                    Locate 18: Input "15. BLOCKS "; playerRatings!(I, 12)
                    Locate 18, 12: Print " "

                Else
                    'Calculate blocks based on position
                    Select Case position$

                        Case "C":
                            playerRatings!(I, 12) = playerRatings!(I, 1) / 48 * 2.5
                            Locate 18: Print "15. BLOCKS "; playerRatings!(I, 12)

                        Case "F":
                            playerRatings!(I, 12) = playerRatings!(I, 1) / 48 * 1.2
                            Locate 18: Print "15. BLOCKS "; playerRatings!(I, 12)

                        Case "G":
                            playerRatings!(I, 12) = playerRatings!(I, 1) / 48 * .4
                            Locate 18: Print "15. BLOCKS "; playerRatings!(I, 12)

                    End Select

                End If

                Locate 19: Input "16. FOULS "; playerRatings!(I, 9)
                Locate 19, 11: Print " "

                Locate 20: Input "17. DISQ "; playerRatings!(I, 10)
                Locate 20, 10: Print " "

                Locate 21: Input "18. HI-GAME "; playerRatings!(I, 15)
                Locate 21, 13: Print " "

                'Locate 22: Input "19. PENETRATION (1-9) "; playerRatings!(I, 16)
                'Locate 23: Input "20. DEF ADJ (-5 TO 5) "; playerRatings!(I, 17)
                Locate 22: Input "19. ACTIVE FLAG (0-YES 1-NO) "; playerRatings!(I, 18)
                Locate 22, 30: Print " "

                Call EditPlayer(I, inputAction%)

            Else

                Exit For

            End If 'Done with initial player params

        Next I

    End If

    For J = I To 14
        playerNames$(J, 0) = "XXX"
        playerNames$(J, 1) = "X"
        For I1 = 0 To 19:
            playerRatings!(J, I1) = -1
        Next
    Next

End Sub


'----------------------------------------
'         EditPlayer Subroutine
'----------------------------------------
'This subroutine gives thet user an inter-
'face for editing stats for a single player
Sub EditPlayer (playerIdx, action)

    Shared playerRatings!()

    Shared playerNames$()

    If action = 2 Then

        'Edit Player
        Locate 4, 1
        Print " 1. NAME (XXX TO QUIT) "; playerNames$(playerIdx, 0)
        Print " 2. POSITION  "; playerNames$(playerIdx, 1)
        Print " 3. GAMES "; playerRatings!(playerIdx, 0)
        Print " 4. MINUTES "; playerRatings!(playerIdx, 1)
        Print " 5. FGM "; playerRatings!(playerIdx, 3)
        Print " 6. FGA "; playerRatings!(playerIdx, 2)
        Print " 7. 3-FGM "; playerRatings!(playerIdx, 14)
        Print " 8. 3-FGA "; playerRatings!(playerIdx, 13)
        Print " 9. FTM "; playerRatings!(playerIdx, 5)
        Print "10. FTA "; playerRatings!(playerIdx, 4)
        Print "11. OFF REB "; playerRatings!(playerIdx, 6)
        Print "12. DEF REB "; playerRatings!(playerIdx, 7)
        Print "13. ASSISTS "; playerRatings!(playerIdx, 8)
        Print "14. STEALS "; playerRatings!(playerIdx, 11)
        Print "15. BLOCKS "; playerRatings!(playerIdx, 12)
        Print "16. FOULS "; playerRatings!(playerIdx, 9)
        Print "17. DISQ "; playerRatings!(playerIdx, 10)
        Print "18. HI-GAME "; playerRatings!(playerIdx, 15)
        'Print "19. PENETRATION (1-9) "; playerRatings!(playerIdx, 16)
        'Print "20. DEF ADJ (-5 TO 5) "; playerRatings!(playerIdx, 17)
        Print "19. ACTIVE FLAG (0-YES 1-NO) "; playerRatings!(playerIdx, 18)

    End If

    Do

        Do
            Locate 26, 29
            Print Space$(29)
            Locate 26, 1
            Color 11
            Input "# TO CHANGE (0 TO CONTINUE)"; B9
            Color 15
        Loop Until B9 >= 0 And B9 <= 19

        Select Case B9

            Case 1
                Locate 4, 24: Print Space$(10)
                Locate 4, 1: Input " 1. NAME (XXX TO QUIT) "; playerNames$(playerIdx, 0)
                Locate 4, 24: Print " "

            Case 2
                Locate 5, 14: Print Space$(10)
                Locate 5, 1: Input " 2. POSITION "; playerNames$(playerIdx, 1)
                Locate 5, 14: Print " "

            Case 3
                Locate 6, 11: Print Space$(10)
                Locate 6, 1: Input " 3. GAMES "; playerRatings!(playerIdx, 0)
                Locate 6, 11: Print " "

            Case 4
                Locate 7, 13: Print Space$(10)
                Locate 7, 1: Input " 4. MINUTES "; playerRatings!(playerIdx, 1)
                Locate 7, 13: Print " "

            Case 5
                Locate 8, 9: Print Space$(10)
                Locate 8, 1: Input " 5. FGM "; playerRatings!(playerIdx, 3)
                Locate 8, 9: Print " "

            Case 6
                Locate 9, 9: Print Space$(10)
                Locate 9, 1: Input " 6. FGA "; playerRatings!(playerIdx, 2)
                Locate 9, 9: Print " "

            Case 7
                Locate 10, 11: Print Space$(10)
                Locate 10, 1: Input " 7. 3-FGM "; playerRatings!(playerIdx, 14)
                Locate 10, 11: Print " "

            Case 8
                Locate 11, 11: Print Space$(10)
                Locate 11, 1: Input " 8. 3-FGA "; playerRatings!(playerIdx, 13)
                Locate 11, 11: Print " "

            Case 9
                Locate 12, 9: Print Space$(10)
                Locate 12, 1: Input " 9. FTM "; playerRatings!(playerIdx, 5)
                Locate 12, 9: Print " "

            Case 10
                Locate 13, 9: Print Space$(10)
                Locate 13, 1: Input "10. FTA "; playerRatings!(playerIdx, 4)
                Locate 13, 9: Print " "

            Case 11
                Locate 14, 13: Print Space$(10)
                Locate 14, 1: Input "11. OFF REB "; playerRatings!(playerIdx, 6)
                Locate 14, 13: Print " "

            Case 12
                Locate 15, 13: Print Space$(10)
                Locate 15, 1: Input "12. DEF REB "; playerRatings!(playerIdx, 7)
                Locate 15, 13: Print " "

                'Calculate Rebounds if missing
                If playerRatings!(playerIdx, 6) = 0 And RB = 0 Then
                    'Off Rebounds = ~ Def Rebounds / 3
                    playerRatings!(playerIdx, 6) = .32 * playerRatings!(playerIdx, 7)
                    playerRatings!(playerIdx, 7) = playerRatings!(playerIdx, 7) - playerRatings!(playerIdx, 6)
                    Locate 14: Print "11. OFF REB "; playerRatings!(playerIdx, 6)
                    Locate 15: Print "12. DEF REB "; playerRatings!(playerIdx, 7)
                End If

            Case 13
                Locate 16, 13: Print Space$(10)
                Locate 16, 1: Input "13. ASSISTS "; playerRatings!(playerIdx, 8)
                Locate 16, 13: Print " "

            Case 14
                Locate 17, 12: Print Space$(10)
                Locate 17, 1: Input "14. STEALS "; playerRatings!(playerIdx, 11)
                Locate 17, 12: Print " "

            Case 15
                Locate 18, 12: Print Space$(10)
                Locate 18, 1: Input "15. BLOCKS "; playerRatings!(playerIdx, 12)
                Locate 18, 12: Print " "

            Case 16
                Locate 19, 11: Print Space$(10)
                Locate 19, 1: Input "16. FOULS "; playerRatings!(playerIdx, 9)
                Locate 19, 11: Print " "

            Case 17
                Locate 20, 10: Print Space$(10)
                Locate 20, 1: Input "17. DISQ "; playerRatings!(playerIdx, 10)
                Locate 20, 10: Print " "

            Case 18
                Locate 21, 13: Print Space$(10)
                Locate 21, 1: Input "18. HI-GAME "; playerRatings!(playerIdx, 15)
                Locate 21, 13: Print " "

            Case 19:
                Locate 22, 30: Print Space$(10)
                Locate 22, 1: Input "19. ACTIVE FLAG (0-YES 1-NO) "; playerRatings!(playerIdx, 18)
                Locate 22, 30: Print " "

                'Locate 22, 23: Print Space$(10)
                'Locate 22, 1: Input "19. PENETRATION (1-9) "; playerRatings!(playerIdx, 16)
                'Locate 22, 23: Print " "

                'Locate 23, 23: Print Space$(10)
                'Locate 23, 1: Input "20. DEF ADJ (-5 TO 5) "; playerRatings!(playerIdx, 17)
                'Locate 23, 23: Print " "

        End Select

    Loop Until B9 = 0

    playerRatings!(playerIdx, 16) = 5
    playerRatings!(playerIdx, 17) = 0

End Sub


'----------------------------------------
'     SaveTeamFile Subroutine
'----------------------------------------
'This subroutine saves the modifed team
'data (from corresponding arrays) to an
'appropriate spot in the select team file

'action%
'   1 = New Team
'   2 = Edit Team

Sub SaveTeamFile (yearNumber$, teamIdx, action%)

    Shared teamAttendance&

    Shared playerRatings!(), leagueRatings(), teamRatings()
    Shared playerNames$(), teamInfo$()

    Call CSMsgBox("SAVING TEAM FILES", 20, Pass)

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

    fileLength& = LOF(1)

    If action% <> 2 Then
        teamIdx = (fileLength& / TEAM_SIZE_BYTES) + 1
    End If

    Field #1, 15 As Q$(0), 15 As Q$(1)

    For I = 0 To 6
        Field #1, 30 + I * 2 As X$, 2 As Q$(I + 2)
    Next

    Field #1, 44 As X$, 2 As Q$(9)

    For I = 0 To 34
        Field #1, 46 + I * 2 As X$, 2 As Q$(I + 10)
    Next

    For I = 0 To 14
        Field #1, 116 + I * 57 As X$, 15 As Q$(I * 22 + 45), 2 As Q$(I * 22 + 46)

        For I1 = 0 To 19
            Field #1, 133 + I * 57 + I1 * 2 As X$, 2 As Q$(I * 22 + 47 + I1)
        Next
    Next

    Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377), 4 As Q$(378)

    LSet Q$(0) = teamInfo$(0)
    LSet Q$(1) = teamInfo$(1)

    For I = 0 To 6
        LSet Q$(I + 2) = MKI$(leagueRatings(I))
    Next

    LSet Q$(9) = MKI$(999)

    For I = 0 To 34:
        LSet Q$(I + 10) = MKI$(teamRatings(I))
    Next

    For I = 0 To 14

        LSet Q$(22 * I + 45) = playerNames$(I, 0)
        LSet Q$(22 * I + 46) = playerNames$(I, 1)

        For I1 = 0 To 19
            LSet Q$(22 * I + 47 + I1) = MKI$(playerRatings!(I, I1))
        Next

    Next

    LSet Q$(375) = teamInfo$(2)
    LSet Q$(376) = teamInfo$(3)
    LSet Q$(377) = teamInfo$(4)
    LSet Q$(378) = MKL$(teamAttendance&)

    Put #1, teamIdx

    Close #1

    Call CSMsgBox("", 0, Pass)

End Sub
