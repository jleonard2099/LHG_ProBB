Sub TINPUT (menuShortcut%)

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
            Selection% = 1
            Count% = 5
            Start% = 1
            ExitCode% = 5
            FileFGColr% = 15: FileBGColr% = 1
            HiLiteFGColr% = 0: HiLiteBGColr% = 7
            Spacing% = 1
            Row% = 8
            Column% = 25

            Call box0(7, 24, 13, 46, 2, 15, 1)

            _MouseShow "DEFAULT"
            MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
            _MouseHide

            If (Selection% >= 1) And (Selection% <= 4) Then Call DoTheInput(Selection%)

        Loop Until Selection% = 5 Or ExitCode% = 27

    End If

End Sub

'----------------------------------------
'         DoTheInput Subroutine
'----------------------------------------
'... explain here ...
Sub DoTheInput (action%)

    Shared playerRatings%(), leagueRatings%(), teamRatings%()
    Shared playerNames$(), teamInfo$()

    Dim P$(2)

    ReDim menuChoices$(1 To 5)

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
                Color 15: Print "TEAM/INDIVID STEALS (Y/N): ";

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
                    Color 15: Print "TEAM/INDIVID BLOCKS (Y/N): ";

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
                        Color 15: Print "TEAM/INDIVID OFF/DEF REB (Y/N): ";

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
                            Color 15: Print "TEAM TURNOVERS (Y/N): ";

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

                                    Print "ANY CHANGE (Y/N)";
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

            Call InputEditTeam(1, year$, teamNbr)
            Call SaveTeamFile(year$, teamNbr, action%)

            '2 = CHANGE OR EDIT AN EXISTING TEAM
        Case 2:
            Call ChooseTeam (year$, numberTeams, teamNbr, A$)
            If teamNbr <> 0 Then
                Call InputEditTeam(2, year$, teamNbr)
                Call SaveTeamFile(year$, teamNbr, action%)
            End If

            '3 = PRINT A TEAM ROSTER
        Case 3:
            Call ChooseTeam (year$, numberTeams, teamNbr, A$)
            If teamNbr <> 0 Then
                Call PrintRoster(year$)
            End If

            '4 = VIEW A TEAM ROSTER
        Case 4:
            Call ChooseTeam (year$, numberTeams, teamNbr, A$)
            If teamNbr <> 0 Then
                Call ViewRoster(A$, teamNbr)
            End If

    End Select

End Sub


'----------------------------------------
'       PrintRoster Subroutine
'----------------------------------------
'...explanation...
Sub PrintRoster (YN$)

    Shared playerRatings%(), leagueRatings%(), teamRatings%()
    Shared playerNames$(), teamInfo$()

    LPrint
    LPrint "DISK ID:"; YN$
    LPrint "TEAM NAME: "; teamInfo$(0); " "; teamInfo$(1)
    LPrint Using "RECORD ##"; teamRatings%(1) + teamRatings%(3);
    LPrint "-";
    LPrint Using "##    HOME ##"; teamRatings%(2) + teamRatings%(4); teamRatings%(1);
    LPrint "-";
    LPrint Using "##    ROAD ##"; teamRatings%(2); teamRatings%(3);
    LPrint "-";
    LPrint Using "##"; teamRatings%(4)
    LPrint
    LPrint "                                                      OFF DEF  TOT"
    LPrint "PLAYER        PS  G  MIN  FGA  FGM  PCT  FTA FTM  PCT REB REB  REB  AST  PF STL BLK  PTS  AVG HI"

    For I = 0 To 14

        If playerNames$(I, 0) <> "" And playerNames$(I, 0) <> "XXX" Then

            'Player Name / PS
            LPrint Left$(playerNames$(I, 0), 14); Tab(15); playerNames$(I, 1); Tab(18);

            'G, MIN, FGA, FGM
            LPrint Using "## #### #### #### "; playerRatings%(I, 0); playerRatings%(I, 1); playerRatings%(I, 2); playerRatings%(I, 3);

            'FG PCT
            If playerRatings%(I, 2) = playerRatings%(I, 3) And playerRatings%(I, 3) <> 0 Then LPrint "1.000";
            If playerRatings%(I, 2) <> playerRatings%(I, 3) Then fgPCT! = playerRatings%(I, 3) / playerRatings%(I, 2): LPrint Using ".###"; fgPCT!;

            LPrint Tab(42);
            'FTA, FTM
            LPrint Using "### ### "; playerRatings%(I, 4); playerRatings%(I, 5);

            'FT PCT
            If playerRatings%(I, 4) = playerRatings%(I, 5) And playerRatings%(I, 5) <> 0 Then LPrint "1.000 ";
            If playerRatings%(I, 4) <> playerRatings%(I, 5) Then ftPCT! = playerRatings%(I, 5) / playerRatings%(I, 4): LPrint Using ".### "; ftPCT!;

            LPrint Tab(55);
            'Rebounds / AST / PF / STL / BLK / PTS / AVG HI
            LPrint Using "### ### #### #### ### ### ### #### "; playerRatings%(I, 6); playerRatings%(I, 7); playerRatings%(I, 6) + playerRatings%(I, 7); playerRatings%(I, 8); playerRatings%(I, 9); playerRatings%(I, 11); playerRatings%(I, 12); playerRatings%(I, 3) * 2 + playerRatings%(I, 5) + playerRatings%(I, 14);
            LPrint Using "###.# ##"; (playerRatings%(I, 3) * 2 + playerRatings%(I, 5) + playerRatings%(I, 14)) / playerRatings%(I, 0); playerRatings%(I, 15)

        End If

    Next

    LPrint Chr$(12)

End Sub


'----------------------------------------
'       ViewRoster Subroutine
'----------------------------------------
'...explanation...
Sub ViewRoster (A$, T1)

    Shared playerRatings%(), leagueRatings%(), teamRatings%()
    Shared playerNames$(), teamInfo$()

    Do

        Color 0, 0
        Cls

        Color teamRatings%(33), teamRatings%(34): Print teamInfo$(0); " "; teamInfo$(1);

        Color 14, 0
        Print Tab(40); "WON - ";
        Color 15
        Print Using "##"; teamRatings%(1) + teamRatings%(3);

        Color 14
        Print Tab(50); "LOST - ";
        Color 15
        Print Using "##"; teamRatings%(2) + teamRatings%(4)

        Color 15

        Locate 3
        Print "NAME          POS   G  MIN  FGA  FGM  PCT  FTA  FTM  PCT OREB DREB ASST  FLS"

        Locate 4

        For I = 0 To 14

            If Left$(playerNames$(I, 0), 3) <> "XXX" Then

                'Name
                Color 3: Print Left$(playerNames$(I, 0), 14);

                'Position
                Color 7: Print Tab(15); playerNames$(I, 1);

                'Games
                Print Tab(20); Using "##"; playerRatings%(I, 0);

                'Minutes
                Print Tab(23); Using "####"; playerRatings%(I, 1);

                'FGA
                Print Tab(28); Using "####"; playerRatings%(I, 2);

                'FGM
                Print Tab(33); Using "####"; playerRatings%(I, 3);

                'FG PCT
                If playerRatings%(I, 3) = 0 Then Print Tab(38); ".000";
                If playerRatings%(I, 3) > 0 Then fgPCT! = playerRatings%(I, 3) / playerRatings%(I, 2): Print Tab(38); Using ".###"; fgPCT!;

                'FTA
                Print Tab(43); Using "####"; playerRatings%(I, 4);

                'FTM
                Print Tab(48); Using "####"; playerRatings%(I, 5);

                'FT PCT
                If playerRatings%(I, 5) = 0 Then Print Tab(53); ".000";
                If playerRatings%(I, 5) > 0 Then ftPCT! = playerRatings%(I, 5) / playerRatings%(I, 4): Print Tab(53); Using ".###"; ftPCT!;

                'Off Reb
                Print Tab(58); Using "####"; playerRatings%(I, 6);

                'Def Reb
                Print Tab(63); Using "####"; playerRatings%(I, 7);

                'Assists
                Print Tab(68); Using "####"; playerRatings%(I, 8);

                'Fouls
                Print Tab(73); Using "####"; playerRatings%(I, 9)

            End If

        Next

        Color 14
        Print
        Print "TOTALS-OFF";

        'Games
        Print Tab(20); Using "##"; leagueRatings%(0);

        'FGA
        Print Tab(28); Using "####"; teamRatings%(5);

        'FGM
        Print Tab(33); Using "####"; teamRatings%(6);

        'FG PCT
        fgPCT! = teamRatings%(6) / teamRatings%(5)
        Print Tab(38); Using ".###"; fgPCT!;

        'FTA
        Print Tab(43); Using "####"; teamRatings%(7);

        'FTM
        Print Tab(48); Using "####"; teamRatings%(8);

        'FT PCT
        ftPCT! = teamRatings%(8) / teamRatings%(7)
        Print Tab(53); Using ".###"; ftPCT!;

        'Off Reb
        Print Tab(58); Using "####"; teamRatings%(9);

        'Def Reb
        Print Tab(63); Using "####"; teamRatings%(10);

        'Assists
        Print Tab(68); Using "####"; teamRatings%(11);

        'Fouls
        Print Tab(73); Using "####"; teamRatings%(12)

        Color 11: Print "TOTALS-DEF";

        'FGA
        Print Tab(28); Using "####"; teamRatings%(19);

        'FGM
        Print Tab(33); Using "####"; teamRatings%(20);

        'FG PCT
        fgPCT! = teamRatings%(20) / teamRatings%(19)
        Print Tab(38); Using ".###"; fgPCT!;

        'FTM
        Print Tab(43); Using "####"; teamRatings%(21);

        'FTA
        Print Tab(48); Using "####"; teamRatings%(22);

        ftPCT! = teamRatings%(22) / teamRatings%(21)
        Print Tab(53); Using ".###"; ftPCT!;

        'Off Reb
        Print Tab(58); Using "####"; teamRatings%(23);

        'Def Reb
        Print Tab(63); Using "####"; teamRatings%(24);

        'Assists
        Print Tab(68); Using "####"; teamRatings%(25);

        'Fouls
        Print Tab(73); Using "####"; teamRatings%(26)

        Color 15

        Locate 23: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

        Cls
        Color teamRatings%(33), teamRatings%(34): Print teamInfo$(0); " "; teamInfo$(1)

        Color 15, 0

        Locate 3
        Print "NAME           STL  BLK 3FGA 3FGM  PCT   HI-GAME"
        Locate 4

        For I = 0 To 14

            If Left$(playerNames$(I, 0), 3) <> "XXX" Then

                'Name
                Color 3: Print Left$(playerNames$(I, 0), 14);

                'Steals
                Color 7: Print Tab(15); Using "####"; playerRatings%(I, 11);

                'Blocks
                Print Tab(20); Using "####"; playerRatings%(I, 12);

                '3FGA
                Print Tab(25); Using "####"; playerRatings%(I, 13);

                '3FGM
                Print Tab(30); Using "####"; playerRatings%(I, 14);

                '3FG PCT
                If playerRatings%(I, 14) = 0 Then Print Tab(35); ".000";
                If playerRatings%(I, 14) > 0 Then threeFGPCT! = playerRatings%(I, 14) / playerRatings%(I, 13): Print Tab(35); Using ".###"; threeFGPCT!;

                'HI GAME
                Print Tab(43); Using "####"; playerRatings%(I, 15)
                'PRINT TAB(53); USING "####"; playerRatings%(I, 16);
                'PRINT TAB(65); USING "####"; playerRatings%(I, 17)

            End If

        Next

        Color 14
        Print
        Print "TOTALS-OFF";

        'Steals
        Print Tab(15); Using "####"; teamRatings%(14);

        'Blocks
        Print Tab(20); Using "####"; teamRatings%(16);

        '3FGA
        Print Tab(25); Using "####"; teamRatings%(17);

        '3FGM
        Print Tab(30); Using "####"; teamRatings%(18);

        '3FG PCT
        If teamRatings%(18) = 0 Then Print Tab(35); ".000"

        If teamRatings%(18) > 0 Then
            statPCT! = teamRatings%(18) / teamRatings%(17)
            Print Tab(35); Using ".###"; statPCT!
        End If

        Color 11
        Print "TOTALS-DEF";

        'Steals
        Print Tab(15); Using "####"; teamRatings%(28);

        'Blocks
        Print Tab(20); Using "####"; teamRatings%(30);

        '3FGA
        Print Tab(25); Using "####"; teamRatings%(31);

        '3FGM
        Print Tab(30); Using "####"; teamRatings%(32);

        '3FG PCT
        If teamRatings%(32) = 0 Then Print Tab(35); ".000"

        If teamRatings%(32) > 0 Then
            statPCT! = teamRatings%(32) / teamRatings%(31)
            Print Tab(35); Using ".###"; statPCT!
        End If

        Color 15
        Print
        Print "VIEW AGAIN (Y/N)"

        Do
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

End Sub


'----------------------------------------
'        InputEditTeam Subroutine
'----------------------------------------
'Steps user through screens for adjusting
'team data and players
'
'NOTE: <ENTER> key is valued as 0 for when
'a user is prompted to press keys to CONTINUE
'This improvement over the original makes it
'faster and easier to proceed through screens

'inputAction% = 1: New Team
'inputAction% = 2: Edit Team
Sub InputEditTeam (inputAction%, yearNumber$, teamIdx)

    Shared playerRatings%(), leagueRatings%(), teamRatings%()
    Shared playerNames$(), teamInfo$()

    Color 15, 0
    Cls

    '--------------------
    ' League Info
    '--------------------
    If inputAction% = 1 Then
        'Input Team

        Color 14
        Print "LEAGUE INFORMATION"
        Color 15

        Locate 3: Input "1. NUMBER OF GAMES "; leagueRatings%(0)
        Locate 4: Input "2. AVG FGA/G "; leagueRatings%(1)
        Locate 5: Input "3. AVG RB/G "; leagueRatings%(2)
        Locate 6: Input "4. LEAGUE FG% "; leagueRatings%(3)
        Locate 7: Input "5. AVG PF/G "; leagueRatings%(4)
        Locate 8: Input "6. LEAGUE 3FG% "; leagueRatings%(5)
        Locate 9: Input "7. LEAGUE POWER ADJ "; leagueRatings%(6)

    Else
        'Modify Team

        Color 14
        Print "CHANGE LEAGUE INFORMATION"
        Color 15

        Locate 3: Print "1. NUMBER OF GAMES "; leagueRatings%(0)
        Locate 4: Print "2. AVG FGA/G "; leagueRatings%(1)
        Locate 5: Print "3. AVG RB/G "; leagueRatings%(2)
        Locate 6: Print "4. LEAGUE FG% "; leagueRatings%(3)
        Locate 7: Print "5. AVG PF/G "; leagueRatings%(4)
        Locate 8: Print "6. LEAGUE 3FG% "; leagueRatings%(5)
        Locate 9: Print "7. LEAGUE POWER ADJ "; leagueRatings%(6)

    End If

    Do

        Do
            Locate 11
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 7

        Select Case B9

            Case 1
                Locate 3: Input "1. NUMBER OF GAMES "; leagueRatings%(0)

            Case 2
                Locate 4: Input "2. AVG FGA/G "; leagueRatings%(1)

            Case 3
                Locate 5: Input "3. AVG RB/G "; leagueRatings%(2)

            Case 4
                Locate 6: Input "4. LEAGUE FG% "; leagueRatings%(3)

            Case 5
                Locate 7: Input "5. AVG PF/G "; leagueRatings%(4)

            Case 6
                Locate 8: Input "6. LEAGUE 3FG% "; leagueRatings%(5)

            Case 7
                Locate 9: Input "7. LEAGUE POWER ADJ "; leagueRatings%(6)

        End Select

    Loop Until B9 = 0

    '----------
    ' Team Info
    '----------
    If inputAction% = 1 Then

        Cls

        Color 14
        Print "TEAM INFORMATION"
        Color 15

        Locate 3: Input "1. TEAM NUMBER (BLANK TO AUTO-ASSIGN)"; teamIdx
        Locate 4: Input "2. DISK ID "; yearNumber$
        Locate 5: Input "3. TEAM NAME "; teamInfo$(0)
        Locate 6: Input "4. TEAM NICKNAME "; teamInfo$(1)
        Locate 7: Input "5. TEAM GAMES PLAYED "; teamRatings%(0)
        Locate 8: Input "6. HOME WINS "; teamRatings%(1)
        Locate 9: Input "7. HOME LOSSES "; teamRatings%(2)
        Locate 10: Input "8. ROAD WINS "; teamRatings%(3)
        Locate 11: Input "9. ROAD LOSSES "; teamRatings%(4)

        Locate 12: Input "10. TEXT COLOR (0-15) "; teamRatings%(33)
        Locate 13: Input "11. BACKGROUND COLOR (0-7) "; teamRatings%(34)
        Locate 18
        Print "TEAM APPEARANCE: ";
        Color teamRatings%(33), teamRatings%(34)
        Print RTrim$(teamInfo$(0)) + " " + RTrim$(teamInfo$(1))
        Color 15, 0

        Locate 14: Input "12. TEAM ABBREV (IE. BOS) "; teamInfo$(2)
        Locate 15: Input "13. COACH'S NAME "; teamInfo$(3)
        Locate 16: Input "14. ARENA NAME "; teamInfo$(4)

    Else

        Cls
        Color 14
        Print "CHANGE TEAM INFORMATION"
        Color 15
        Print
        Print "1. TEAM NUMBER "; teamIdx
        Print "2. DISK ID "; yearNumber$
        Print "3. TEAM NAME "; teamInfo$(0)
        Print "4. TEAM NICKNAME "; teamInfo$(1)
        Print "5. TEAM GAMES PLAYED "; teamRatings%(0)
        Print "6. HOME WINS "; teamRatings%(1)
        Print "7. HOME LOSSES "; teamRatings%(2)
        Print "8. ROAD WINS "; teamRatings%(3)
        Print "9. ROAD LOSSES "; teamRatings%(4)
        Print "10. TEXT COLOR (0-15) "; teamRatings%(33)
        Print "11. BCKGRND COLOR (0-7) "; teamRatings%(34)
        Print "12. TEAM ABBREV (IE. BOS) "; teamInfo$(2)
        Print "13. COACH'S NAME "; teamInfo$(3)
        Print "14. ARENA NAME "; teamInfo$(4)

        Locate 18
        Print "TEAM APPEARANCE: ";
        Color teamRatings%(33), teamRatings%(34)
        Print RTrim$(teamInfo$(0)) + " " + RTrim$(teamInfo$(1))
        Color 15, 0

    End If

    Do

        Do
            Locate 20
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 14

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 3: Input "1. TEAM NUMBER "; teamIdx

                Case 2
                    Locate 4: Input "2. DISK ID "; yearNumber$

                Case 3
                    Locate 5: Input "3. TEAM NAME "; teamInfo$(0)

                Case 4
                    Locate 6: Input "4. TEAM NICKNAME "; teamInfo$(1)

                Case 5
                    Locate 7: Input "5. TEAM GAMES PLAYED "; teamRatings%(0)

                Case 6
                    Locate 8: Input "6. HOME WINS "; teamRatings%(1)

                Case 7
                    Locate 9: Input "7. HOME LOSSES "; teamRatings%(2)

                Case 8
                    Locate 10: Input "8. ROAD WINS "; teamRatings%(3)

                Case 9
                    Locate 11: Input "9. ROAD LOSSES "; teamRatings%(4)

                Case 10
                    Locate 12: Input "10. TEXT COLOR (0-15) "; teamRatings%(33)
                    Locate 18
                    Print "TEAM APPEARANCE: ";
                    Color teamRatings%(33), teamRatings%(34)
                    Print RTrim$(teamInfo$(0)) + " " + RTrim$(teamInfo$(1))
                    Color 15, 0

                Case 11
                    Locate 13: Input "11. BACKGROUND COLOR (0-7) "; teamRatings%(34)
                    Locate 18
                    Print "TEAM APPEARANCE: ";
                    Color teamRatings%(33), teamRatings%(34)
                    Print RTrim$(teamInfo$(0)) + " " + RTrim$(teamInfo$(1))
                    Color 15, 0

                Case 12
                    Locate 14: Input "12. TEAM ABBREV (IE. BOS) "; teamInfo$(2)

                Case 13
                    Locate 15: Input "13. COACH'S NAME "; teamInfo$(3)

                Case 14
                    Locate 16: Input "14. ARENA NAME "; teamInfo$(4)

            End Select

        End If

    Loop Until B9 = 0


    '---------------
    ' Team Offense
    '---------------
    If inputAction% = 1 Then

        Cls

        Color 14
        Print "TEAM OFFENSIVE INFORMATION"
        Color 15

        Locate 3: Input " 1. FGM "; teamRatings%(6)
        Locate 4: Input " 2. FGA "; teamRatings%(5)
        Locate 5: Input " 3. FTM "; teamRatings%(8)
        Locate 6: Input " 4. FTA "; teamRatings%(7)

        Locate 7: Input " 5. OFF REB "; teamRatings%(9)
        Locate 8: Input " 6. DEF REB "; teamRatings%(10)

        'Calculate Rebounds if missing
        If teamRatings%(9) = 0 And RB = 0 Then
            'Off Rebounds = ~ Def Rebounds / 3
            teamRatings%(9) = .32 * teamRatings%(10)
            teamRatings%(10) = teamRatings%(10) - teamRatings%(9)
            Locate 7: Print " 5. OFF REB "; teamRatings%(9)
            Locate 8: Print " 6. DEF REB "; teamRatings%(10)
        End If

        Locate 9: Input " 7. ASSISTS "; teamRatings%(11)
        Locate 10: Input " 8. FOULS "; teamRatings%(12)
        Locate 11: Input " 9. DISQ "; teamRatings%(13)
        Locate 12: Input "10. STEALS "; teamRatings%(14)
        '-- We currently don't have a a calculation for if this data is missing
        Locate 13: Input "11. TURNOVERS "; teamRatings%(15)
        Locate 14: Input "12. BLK SHOTS "; teamRatings%(16)
        Locate 15: Input "13. 3-FGM "; teamRatings%(18)
        Locate 16: Input "14. 3-FGA "; teamRatings%(17)

    Else

        Cls

        Color 14
        Print "CHANGE TEAM OFFENSIVE INFORMATION"
        Color 15
        Print
        Print " 1. FGM "; teamRatings%(6)
        Print " 2. FGA "; teamRatings%(5)
        Print " 3. FTM "; teamRatings%(8)
        Print " 4. FTA "; teamRatings%(7)
        Print " 5. OFF REB "; teamRatings%(9)
        Print " 6. DEF REB "; teamRatings%(10)
        Print " 7. ASSISTS "; teamRatings%(11)
        Print " 8. FOULS "; teamRatings%(12)
        Print " 9. DISQ "; teamRatings%(13)
        Print "10. STEALS "; teamRatings%(14)
        Print "11. TURNOVERS "; teamRatings%(15)
        Print "12. BLK SHOTS "; teamRatings%(16)
        Print "13. 3-FGM "; teamRatings%(18)
        Print "14. 3-FGA "; teamRatings%(17)

    End If

    Do

        Do
            Locate 18
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 14

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 3: Input " 1. FGM "; teamRatings%(6)

                Case 2
                    Locate 4: Input " 2. FGA "; teamRatings%(5)

                Case 3
                    Locate 5: Input " 3. FTM "; teamRatings%(8)

                Case 4
                    Locate 6: Input " 4. FTA "; teamRatings%(7)

                Case 5
                    Locate 7: Input " 5. OFF REB "; teamRatings%(9)

                Case 6
                    Locate 8: Input " 6. DEF REB "; teamRatings%(10)

                    'Calculate Rebounds if missing
                    If teamRatings%(9) = 0 And RB = 0 Then
                        'Off Rebounds = ~ Def Rebounds / 3
                        teamRatings%(9) = .32 * teamRatings%(10)
                        teamRatings%(10) = teamRatings%(10) - teamRatings%(9)
                        Locate 7: Print " 5. OFF REB "; teamRatings%(9)
                        Locate 8: Print " 6. DEF REB "; teamRatings%(10)
                    End If

                Case 7
                    Locate 9: Input " 7. ASSISTS "; teamRatings%(11)

                Case 8
                    Locate 10: Input " 8. FOULS "; teamRatings%(12)

                Case 9
                    Locate 11: Input " 9. DISQ "; teamRatings%(13)

                Case 10
                    Locate 12: Input "10. STEALS "; teamRatings%(14)

                Case 11
                    Locate 13: Input "11. TURNOVERS "; teamRatings%(15)

                Case 12
                    Locate 14: Input "12. BLK SHOTS "; teamRatings%(16)

                Case 13
                    Locate 15: Input "13. 3-FGM "; teamRatings%(18)

                Case 14
                    Locate 16: Input "14. 3-FGA "; teamRatings%(17)

            End Select

        End If

    Loop Until B9 = 0


    '---------------
    ' Team Defense
    '---------------
    If inputAction% = 1 Then

        Cls

        Color 14
        Print "TEAM DEFENSIVE INFORMATION"
        Color 15

        Locate 3: Input " 1. FGM "; teamRatings%(20)
        Locate 4: Input " 2. FGA "; teamRatings%(19)
        Locate 5: Input " 3. FTM "; teamRatings%(22)
        Locate 6: Input " 4. FTA "; teamRatings%(21)
        Locate 7: Input " 5. OFF REB "; teamRatings%(23)
        Locate 8: Input " 6. DEF REB "; teamRatings%(24)

        'Calculate Rebounds if missing
        If teamRatings%(23) = 0 And RB = 0 Then
            'Off Rebounds = ~ Def Rebounds / 3
            teamRatings%(23) = .35 * teamRatings%(24)
            teamRatings%(24) = teamRatings%(24) - teamRatings%(23)
            Locate 6: Print " 5. OFF REB "; teamRatings%(23)
            Locate 7: Print " 6. DEF REB "; teamRatings%(24)
        End If

        Locate 9: Input " 7. ASSISTS "; teamRatings%(25)
        Locate 10: Input " 8. FOULS "; teamRatings%(26)
        Locate 11: Input " 9. DISQ "; teamRatings%(27)
        Locate 12: Input "10. STEALS "; teamRatings%(28)
        '-- We currently don't have a a calculation for if this data is missing
        Locate 13: Input "11. TURNOVERS "; teamRatings%(29)
        Locate 14: Input "12. BLK SHOTS "; teamRatings%(30)
        Locate 15: Input "13. 3-FGM "; teamRatings%(32)
        Locate 16: Input "14. 3-FGA "; teamRatings%(31)

    Else

        Cls

        Color 14
        Print "CHANGE TEAM DEFENSIVE INFORMATION"
        Color 15
        Print
        Print " 1. FGM "; teamRatings%(20)
        Print " 2. FGA "; teamRatings%(19)
        Print " 3. FTM "; teamRatings%(22)
        Print " 4. FTA "; teamRatings%(21)
        Print " 5. OFF REB "; teamRatings%(23)
        Print " 6. DEF REB "; teamRatings%(24)
        Print " 7. ASSISTS "; teamRatings%(25)
        Print " 8. FOULS "; teamRatings%(26)
        Print " 9. DISQ "; teamRatings%(27)
        Print "10. STEALS "; teamRatings%(28)
        Print "11. TURNOVERS "; teamRatings%(29)
        Print "12. BLK SHOTS "; teamRatings%(30)
        Print "13. 3-FGM "; teamRatings%(32)
        Print "14. 3-FGA "; teamRatings%(31)

    End If

    Do

        Do
            Locate 18
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 14

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 3: Input " 1. FGM "; teamRatings%(20)

                Case 2
                    Locate 4: Input " 2. FGA "; teamRatings%(19)

                Case 3
                    Locate 5: Input " 3. FTM "; teamRatings%(22)

                Case 4
                    Locate 6: Input " 4. FTA "; teamRatings%(21)

                Case 5
                    Locate 7: Input " 5. OFF REB "; teamRatings%(23)

                Case 6
                    Locate 8: Input " 6. DEF REB "; teamRatings%(24)

                    'Calculate Rebounds if missing
                    If teamRatings%(23) = 0 And RB = 0 Then
                        'Off Rebounds = ~ Def Rebounds / 3
                        teamRatings%(23) = .35 * teamRatings%(24)
                        teamRatings%(24) = teamRatings%(24) - teamRatings%(23)
                        Locate 7: Print " 5. OFF REB "; teamRatings%(23)
                        Locate 8: Print " 6. DEF REB "; teamRatings%(24)
                    End If

                Case 7
                    Locate 9: Input " 7. ASSISTS "; teamRatings%(25)

                Case 8
                    Locate 10: Input " 8. FOULS "; teamRatings%(26)

                Case 9
                    Locate 11: Input " 9. DISQ "; teamRatings%(27)

                Case 10
                    Locate 12: Input "10. STEALS "; teamRatings%(28)

                Case 11
                    Locate 13: Input "11. TURNOVERS "; teamRatings%(29)

                Case 12
                    Locate 14: Input "12. BLK SHOTS "; teamRatings%(30)

                Case 13
                    Locate 15: Input "13. 3-FGM "; teamRatings%(32)

                Case 14
                    Locate 16: Input "14. 3-FGA "; teamRatings%(31)

            End Select

        End If

    Loop Until B9 = 0



    '---------------
    ' Player Stats
    '---------------

    If inputAction% = 2 Then
        'Edit Team
        Do

            Cls
            Color 14
            Print "CHANGE PLAYER STATS"
            Color 15

            For I = 0 To 14:
                Print Using "##"; I + 1;
                Print "  "; playerNames$(I, 0)
            Next

            Print

            Do
                Print "# TO CHANGE (1-15) (0 TO CONTINUE) ";: Input idx
                Locate 1, 1

                If idx <> 0 Then

                    Call EditPlayer(idx - 1, inputAction%)

                End If

            Loop Until idx >= 0 And idx <= 15

        Loop Until idx = 0

    Else

        'Calculate Steals/Blocks if missing
        'Def Steals
        If teamRatings%(28) = 0 And ST = 0 Then teamRatings%(28) = teamRatings%(5) * .1     'Off FGA / 10
        'Off Steals
        If teamRatings%(14) = 0 And ST = 0 Then teamRatings%(14) = teamRatings%(19) * .1    'Def FGA / 10
        'Def Blocks
        If teamRatings%(30) = 0 And BK = 0 Then teamRatings%(30) = teamRatings%(5) * .06    'Off FGA / ~16
        'Off Blocks
        If teamRatings%(16) = 0 And BK = 0 Then teamRatings%(16) = teamRatings%(19) * .06   'Def FGA / ~16

        'Input team
        For I = 0 To 14

            Cls

            Locate 1, 1

            Color 14
            Print Tab(31); "CHANGE PLAYER INFORMATION"
            Color 15

            Locate 2: Input " 1. NAME (XXX TO QUIT) "; playerNames$(I, 0)

            If playerNames$(I, 0) <> "XXX" Then

                Locate 3: Input " 2. POSITION "; playerNames$(I, 1)
                Locate 4: Input " 3. GAMES "; playerRatings%(I, 0)
                Locate 5: Input " 4. MINUTES "; playerRatings%(I, 1)
                Locate 6: Input " 5. FGM "; playerRatings%(I, 3)
                Locate 7: Input " 6. FGA "; playerRatings%(I, 2)
                Locate 8: Input " 7. FTM "; playerRatings%(I, 5)
                Locate 9: Input " 8. FTA "; playerRatings%(I, 4)
                Locate 10: Input " 9. OFF REB "; playerRatings%(I, 6)
                Locate 11: Input "10. DEF REB "; playerRatings%(I, 7)

                'Calculate Rebounds if missing
                If playerRatings%(I, 6) = 0 And RB = 0 Then
                    'Off Rebounds = ~ Def Rebounds / 3
                    playerRatings%(I, 6) = .32 * playerRatings%(I, 7)
                    playerRatings%(I, 7) = playerRatings%(I, 7) - playerRatings%(I, 6)
                    Locate 10: Print " 9. OFF REB "; playerRatings%(I, 6)
                    Locate 11: Print "10. DEF REB "; playerRatings%(I, 7)
                End If

                Locate 12: Input "11. ASSISTS "; playerRatings%(I, 8)
                Locate 13: Input "12. FOULS "; playerRatings%(I, 9)

                position$ = Left$(playerNames$(I, 1), 1)

                Locate 14: Input "13. DISQ "; playerRatings%(I, 10)

                If ST = 1 Then

                    Locate 15: Input "14. STEALS   "; playerRatings%(I, 11)

                Else
                    Select Case position$

                        Case "C":
                            playerRatings%(I, 11) = playerRatings%(I, 1) / 48 * 1.2
                            Locate 15: Print "14. STEALS "; playerRatings%(I, 11)

                        Case "F":
                            playerRatings%(I, 11) = playerRatings%(I, 1) / 48 * 1.55
                            Locate 15: Print "14. STEALS "; playerRatings%(I, 11)

                        Case "G":
                            playerRatings%(I, 11) = playerRatings%(I, 1) / 48 * 2.3
                            Locate 15: Print "14. STEALS "; playerRatings%(I, 11)

                    End Select

                End If

                If BK = 1 Then
                    Locate 16: Input "15. BLOCKS "; playerRatings%(I, 12)
                Else

                    Select Case position$

                        Case "C":
                            playerRatings%(I, 12) = playerRatings%(I, 1) / 48 * 2.5
                            Locate 16: Print "15. BLOCKS "; playerRatings%(I, 12)

                        Case "F":
                            playerRatings%(I, 12) = playerRatings%(I, 1) / 48 * 1.2
                            Locate 16: Print "15. BLOCKS "; playerRatings%(I, 12)

                        Case "G":
                            playerRatings%(I, 12) = playerRatings%(I, 1) / 48 * .4
                            Locate 16: Print "15. BLOCKS "; playerRatings%(I, 12)

                    End Select

                End If

                Locate 17: Input "16. 3-FGM "; playerRatings%(I, 14)
                Locate 18: Input "17. 3-FGA "; playerRatings%(I, 13)
                Locate 19: Input "18. HI-GAME "; playerRatings%(I, 15)
                'LOCATE 20:INPUT "19. PENETRATION (1-9) ";playerRatings%(I,16)
                'LOCATE 21:INPUT "20. DEF ADJ (-5 TO 5) ";playerRatings%(I,17)
                Locate 22: Input "21. ACTIVE FLAG (0-YES 1-NO) "; playerRatings%(I, 18)

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
            playerRatings%(J, I1) = -1
        Next
    Next

End Sub


'----------------------------------------
'         EditPlayer Subroutine
'----------------------------------------
'...explanation...
Sub EditPlayer (J, action)

    Shared playerRatings%()

    Shared playerNames$()

    Locate 1, 1

    Color 14
    Print Tab(31); "CHANGE PLAYER INFORMATION"
    Color 15

    If action = 2 Then

        Print " 1. NAME (XXX TO QUIT) "; playerNames$(J, 0)
        Print " 2. POSITION "; playerNames$(J, 1)
        Print " 3. GAMES "; playerRatings%(J, 0)
        Print " 4. MINUTES "; playerRatings%(J, 1)
        Print " 5. FGM "; playerRatings%(J, 3)
        Print " 6. FGA "; playerRatings%(J, 2)
        Print " 7. FTM "; playerRatings%(J, 5)
        Print " 8. FTA "; playerRatings%(J, 4)
        Print " 9. OFF REB "; playerRatings%(J, 6)
        Print "10. DEF REB "; playerRatings%(J, 7)
        Print "11. ASSISTS "; playerRatings%(J, 8)
        Print "12. FOULS "; playerRatings%(J, 9)
        Print "13. DISQ "; playerRatings%(J, 10)
        Print "14. STEALS "; playerRatings%(J, 11)
        Print "15. BLOCKS "; playerRatings%(J, 12)
        Print "16. 3-FGM "; playerRatings%(J, 14)
        Print "17. 3-FGA "; playerRatings%(J, 13)
        Print "18. HI-GAME "; playerRatings%(J, 15)
        Print 'PRINT "19. PENETRATION (1-9) ";playerRatings%(I,16)
        Print 'PRINT "20. DEF ADJ (-5 TO 5) ";playerRatings%(I,17)
        Print "21. ACTIVE FLAG (0-YES 1-NO) "; playerRatings%(J, 18)
    End If

    Do

        Do
            Locate 23
            Input "# TO CHANGE (0 TO CONTINUE ) "; B9
        Loop Until B9 >= 0 And B9 <= 21

        Select Case B9

            Case 1
                Locate 2: Input " 1. NAME (XXX TO QUIT) "; playerNames$(I, 0)

            Case 2
                Locate 3: Input " 2. POSITION "; playerNames$(I, 1)

            Case 3
                Locate 4: Input " 3. GAMES "; playerRatings%(I, 0)

            Case 4
                Locate 5: Input " 4. MINUTES "; playerRatings%(I, 1)

            Case 5
                Locate 6: Input " 5. FGM "; playerRatings%(I, 3)

            Case 6
                Locate 7: Input " 6. FGA "; playerRatings%(I, 2)

            Case 7
                Locate 8: Input " 7. FTM "; playerRatings%(I, 5)

            Case 8
                Locate 9: Input " 8. FTA "; playerRatings%(I, 4)

            Case 9
                Locate 10: Input " 9. OFF REB "; playerRatings%(I, 6)

            Case 10
                Locate 11: Input "10. DEF REB "; playerRatings%(I, 7)

                'Calculate Rebounds if missing
                If playerRatings%(I, 6) = 0 And RB = 0 Then
                    'Off Rebounds = ~ Def Rebounds / 3
                    playerRatings%(I, 6) = .32 * playerRatings%(I, 7)
                    playerRatings%(I, 7) = playerRatings%(I, 7) - playerRatings%(I, 6)
                    Locate 10: Print " 9. OFF REB "; playerRatings%(I, 6)
                    Locate 11: Print "10. DEF REB "; playerRatings%(I, 7)
                End If

            Case 11
                Locate 12: Input "11. ASSISTS "; playerRatings%(I, 8)

            Case 12
                Locate 13: Input "12. FOULS "; playerRatings%(I, 9)

            Case 13
                Locate 14: Input "13. DISQ "; playerRatings%(I, 10)

            Case 14
                Locate 15: Input "14. STEALS   "; playerRatings%(I, 11)

            Case 15
                Locate 16: Input "15. BLOCKS "; playerRatings%(I, 12)

            Case 16
                Locate 17: Input "16. 3-FGM "; playerRatings%(I, 14)

            Case 17
                Locate 18: Input "17. 3-FGA "; playerRatings%(I, 13)

            Case 18
                Locate 19: Input "18. HI-GAME "; playerRatings%(I, 15)

            Case 19:
                'LOCATE 20:INPUT "19. PENETRATION (1-9) ";playerRatings%(I,16)

            Case 20:
                'LOCATE 21:INPUT "20. DEF ADJ (-5 TO 5) ";playerRatings%(I,17)

            Case 21:
                Locate 22: Input "21. ACTIVE FLAG (0-YES 1-NO) "; playerRatings%(I, 18)

        End Select

    Loop Until B9 = 0



End Sub

'----------------------------------------
'      GetTeam_CNGTM Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_CNGTM (A$, yearNumber$, teamIdx)

    Shared teamRatings%()

    Shared teamInfo$()

    Do

        Color 15, 0

        Cls

        JJ = 7
        Locate 6, 8: Color 15, 4
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print "ABA SEASONS ARE FOLLOWED BY AN 'A' (IE. 75A)."
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 27: Input NY$
        If NY$ = "" And yearNumber$ <> "" Then
            Call SelectTeam_CNGTM(yearNumber$, NTMS, teamIdx)
        Else
            yearNumber$ = RTrim$(NY$)
            Erase teamNames$
            NTMS = 0

            Open diskPaths$(0) + "PROBBTMS." + yearNumber$ For Random As #1 Len = 1031

            fileLength& = LOF(1)

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

            Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)

            For I1 = 1 To fileLength& / 1031
                Get #1, I1
                CK% = CVI(Q$(9))
                If CK% = 999 Then
                    NTMS = NTMS + 1
                    teamNames$(NTMS) = Q$(0)
                    teamIndex%(NTMS) = I1
                End If
            Next
            Close 1

            TS% = Int((NTMS - 1) / 80)

            Call SelectTeam_CNGTM(yearNumber$, NTMS, teamIdx)

        End If

        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33
            Color teamRatings%(33), teamRatings%(34): Print teamInfo$(0)
            Color 15, 4

            Do
                Locate 3, 33
                Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"
        End If

    Loop Until UCase$(I$) = "N"

End Sub


'----------------------------------------
'     SelectTeam_CNGTM Subroutine
'----------------------------------------
'... explain here ...
Sub SelectTeam_CNGTM (yearNumber$, numberTeams, teamIdx)

    Shared playerRatings%(), leagueRatings%(), teamRatings%()
    Shared playerNames$(), teamInfo$()

    Selection% = 1
    Count% = numberTeams
    Start% = 1
    FileFGColr% = 15: FileBGColr% = 4
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    Color , 4: Cls: Locate , 33: Color 14
    Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        teamIdx = teamIndex%(Selection%)
        teamYear$ = RTrim$(yearNumber$)

        Open diskPaths$(0) + "PROBBTMS." + teamYear$ For Random As #1 Len = 1031

        fileLength& = LOF(1)

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

        Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)

        Get #1, teamIdx

        teamInfo$(0) = RTrim$(Q$(0))
        teamInfo$(1) = RTrim$(Q$(1))

        For I = 0 To 6
            leagueRatings%(I) = CVI(Q$(I + 2))
        Next

        For I = 0 To 34
            teamRatings%(I) = CVI(Q$(I + 10))
        Next

        For I = 0 To 14
            playerNames$(I, 0) = Q$(22 * I + 45)
            playerNames$(I, 1) = Q$(22 * I + 46)
            For I1 = 0 To 19
                playerRatings%(I, I1) = CVI(Q$(22 * I + 47 + I1))
            Next
        Next

        teamInfo$(2) = RTrim$(Q$(375))
        teamInfo$(3) = RTrim$(Q$(376))
        teamInfo$(4) = RTrim$(Q$(377))

        Close 1

    Else
        BO% = 1
    End If

End Sub

'----------------------------------------
'     SaveTeamFile Subroutine
'----------------------------------------
'... explain here ...
Sub SaveTeamFile (yearNumber$, teamIdx, action%)

    Shared playerRatings%(), leagueRatings%(), teamRatings%()
    Shared playerNames$(), teamInfo$()

    Open diskPaths$(0) + "PROBBTMS." + yearNumber$ For Random As #1 Len = 1031

    fileLength& = LOF(1)

    If action% <> 2 Then teamIdx = fileLength& / 1031 + 1

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

    Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)

    LSet Q$(0) = teamInfo$(0)
    LSet Q$(1) = teamInfo$(1)

    For I = 0 To 6
        LSet Q$(I + 2) = MKI$(leagueRatings%(I))
    Next

    LSet Q$(9) = MKI$(999)

    For I = 0 To 34: 
        LSet Q$(I + 10) = MKI$(teamRatings%(I))
    Next

    For I = 0 To 14

        LSet Q$(22 * I + 45) = playerNames$(I, 0)
        LSet Q$(22 * I + 46) = playerNames$(I, 1)

        For I1 = 0 To 19
            LSet Q$(22 * I + 47 + I1) = MKI$(playerRatings%(I, I1))
        Next

    Next

    LSet Q$(375) = teamInfo$(2)
    LSet Q$(376) = teamInfo$(3)
    LSet Q$(377) = teamInfo$(4)

    Put #1, teamIdx

    Close 1

End Sub

