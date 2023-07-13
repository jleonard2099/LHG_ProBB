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

    Shared inputB%(), inputL%(), inputT%()
    Shared inputB$(), inputT$()

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

                    Print I$

                    ST = I
                    Print
                    Color 14: Print "(2) ";
                    Color 15: Print "TEAM/INDIVID BLOCKS (Y/N): ";

                    Do
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)

                    If UCase$(I$) = "Y" Then I = 1
                    If UCase$(I$) = "N" Then I = 0

                    If I$ <> Chr$(27) Then

                        Print I$
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

                            Print I$
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

                                Print I$

                                T0 = I
                                Print

                                'Do

                                Print "ANY CHANGE (Y/N)";
                                I$ = GetKeyPress$

                                'Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

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
            Call GetTeam_CNGTM(A$, year$, teamNbr)
            Call InputEditTeam(2, year$, teamNbr)
            Call SaveTeamFile(year$, teamNbr, action%)

            '3 = PRINT A TEAM ROSTER
        Case 3:
            Call GetTeam_CNGTM(A$, year$, teamNbr)
            Call PrintRoster(year$)

            '4 = VIEW A TEAM ROSTER
        Case 4:
            Call GetTeam_CNGTM(A$, year$, teamNbr)
            Call ViewRoster(A$, teamNbr)

    End Select

End Sub


'----------------------------------------
'       PrintRoster Subroutine
'----------------------------------------
'...explanation...
Sub PrintRoster (YN$)

    Shared inputB%(), inputL%(), inputT%()
    Shared inputB$(), inputT$()

    LPrint
    LPrint "DISK ID:"; YN$
    LPrint "TEAM NAME: "; inputT$(0); " "; inputT$(1)
    LPrint Using "RECORD ##"; inputT%(1) + inputT%(3);
    LPrint "-";
    LPrint Using "##    HOME ##"; inputT%(2) + inputT%(4); inputT%(1);
    LPrint "-";
    LPrint Using "##    ROAD ##"; inputT%(2); inputT%(3);
    LPrint "-";
    LPrint Using "##"; inputT%(4)
    LPrint
    LPrint "                                                      OFF DEF  TOT"
    LPrint "PLAYER        PS  G  MIN  FGA  FGM  PCT  FTA FTM  PCT REB REB  REB  AST  PF STL BLK  PTS  AVG HI"

    For I = 0 To 14

        If inputB$(I, 0) <> "" And inputB$(I, 0) <> "XXX" Then

            'Player Name / PS
            LPrint Left$(inputB$(I, 0), 14); Tab(15); inputB$(I, 1); Tab(18);

            'G, MIN, FGA, FGM
            LPrint Using "## #### #### #### "; inputB%(I, 0); inputB%(I, 1); inputB%(I, 2); inputB%(I, 3);

            'FG PCT
            If inputB%(I, 2) = inputB%(I, 3) And inputB%(I, 3) <> 0 Then LPrint "1.000";
            If inputB%(I, 2) <> inputB%(I, 3) Then fgPCT! = inputB%(I, 3) / inputB%(I, 2): LPrint Using ".###"; fgPCT!;

            LPrint Tab(42);
            'FTA, FTM
            LPrint Using "### ### "; inputB%(I, 4); inputB%(I, 5);

            'FT PCT
            If inputB%(I, 4) = inputB%(I, 5) And inputB%(I, 5) <> 0 Then LPrint "1.000 ";
            If inputB%(I, 4) <> inputB%(I, 5) Then ftPCT! = inputB%(I, 5) / inputB%(I, 4): LPrint Using ".### "; ftPCT!;

            LPrint Tab(55);
            'Rebounds / AST / PF / STL / BLK / PTS / AVG HI
            LPrint Using "### ### #### #### ### ### ### #### "; inputB%(I, 6); inputB%(I, 7); inputB%(I, 6) + inputB%(I, 7); inputB%(I, 8); inputB%(I, 9); inputB%(I, 11); inputB%(I, 12); inputB%(I, 3) * 2 + inputB%(I, 5) + inputB%(I, 14);
            LPrint Using "###.# ##"; (inputB%(I, 3) * 2 + inputB%(I, 5) + inputB%(I, 14)) / inputB%(I, 0); inputB%(I, 15)

        End If

    Next

    LPrint Chr$(12)

End Sub


'----------------------------------------
'       ViewRoster Subroutine
'----------------------------------------
'...explanation...
Sub ViewRoster (A$, T1)

    Shared inputB%(), inputL%(), inputT%()
    Shared inputB$(), inputT$()

    Do

        Color 0, 0
        Cls

        Color inputT%(33), inputT%(34): Print inputT$(0); " "; inputT$(1);

        Color 14, 0
        Print Tab(40); "WON - ";
        Color 15
        Print Using "##"; inputT%(1) + inputT%(3);

        Color 14
        Print Tab(50); "LOST - ";
        Color 15
        Print Using "##"; inputT%(2) + inputT%(4)

        Color 15

        Locate 3
        Print "NAME          POS   G  MIN  FGA  FGM  PCT  FTA  FTM  PCT OREB DREB ASST  FLS"

        Locate 4

        For I = 0 To 14

            If Left$(inputB$(I, 0), 3) <> "XXX" Then

                'Name
                Color 3: Print Left$(inputB$(I, 0), 14);

                'Position
                Color 7: Print Tab(15); inputB$(I, 1);

                'Games
                Print Tab(20); Using "##"; inputB%(I, 0);

                'Minutes
                Print Tab(23); Using "####"; inputB%(I, 1);

                'FGA
                Print Tab(28); Using "####"; inputB%(I, 2);

                'FGM
                Print Tab(33); Using "####"; inputB%(I, 3);

                'FG PCT
                If inputB%(I, 3) = 0 Then Print Tab(38); ".000";
                If inputB%(I, 3) > 0 Then fgPCT! = inputB%(I, 3) / inputB%(I, 2): Print Tab(38); Using ".###"; fgPCT!;

                'FTA
                Print Tab(43); Using "####"; inputB%(I, 4);

                'FTM
                Print Tab(48); Using "####"; inputB%(I, 5);

                'FT PCT
                If inputB%(I, 5) = 0 Then Print Tab(53); ".000";
                If inputB%(I, 5) > 0 Then ftPCT! = inputB%(I, 5) / inputB%(I, 4): Print Tab(53); Using ".###"; ftPCT!;

                'Off Reb
                Print Tab(58); Using "####"; inputB%(I, 6);

                'Def Reb
                Print Tab(63); Using "####"; inputB%(I, 7);

                'Assists
                Print Tab(68); Using "####"; inputB%(I, 8);

                'Fouls
                Print Tab(73); Using "####"; inputB%(I, 9)

            End If

        Next

        Color 14
        Print
        Print "TOTALS-OFF";

        'Games
        Print Tab(20); Using "##"; inputL%(0);

        'FGA
        Print Tab(28); Using "####"; inputT%(5);

        'FGM
        Print Tab(33); Using "####"; inputT%(6);

        'FG PCT
        fgPCT! = inputT%(6) / inputT%(5)
        Print Tab(38); Using ".###"; fgPCT!;

        'FTA
        Print Tab(43); Using "####"; inputT%(7);

        'FTM
        Print Tab(48); Using "####"; inputT%(8);

        'FT PCT
        ftPCT! = inputT%(8) / inputT%(7)
        Print Tab(53); Using ".###"; ftPCT!;

        'Off Reb
        Print Tab(58); Using "####"; inputT%(9);

        'Def Reb
        Print Tab(63); Using "####"; inputT%(10);

        'Assists
        Print Tab(68); Using "####"; inputT%(11);

        'Fouls
        Print Tab(73); Using "####"; inputT%(12)

        Color 11: Print "TOTALS-DEF";

        'FGA
        Print Tab(28); Using "####"; inputT%(19);

        'FGM
        Print Tab(33); Using "####"; inputT%(20);

        'FG PCT
        fgPCT! = inputT%(20) / inputT%(19)
        Print Tab(38); Using ".###"; fgPCT!;

        'FTM
        Print Tab(43); Using "####"; inputT%(21);

        'FTA
        Print Tab(48); Using "####"; inputT%(22);

        ftPCT! = inputT%(22) / inputT%(21)
        Print Tab(53); Using ".###"; ftPCT!;

        'Off Reb
        Print Tab(58); Using "####"; inputT%(23);

        'Def Reb
        Print Tab(63); Using "####"; inputT%(24);

        'Assists
        Print Tab(68); Using "####"; inputT%(25);

        'Fouls
        Print Tab(73); Using "####"; inputT%(26)

        Color 15

        Locate 23: Print "HIT ANY KEY TO CONTINUE"
        I$ = GetKeyPress$

        Cls
        Color inputT%(33), inputT%(34): Print inputT$(0); " "; inputT$(1)

        Color 15, 0

        Locate 3
        Print "NAME           STL  BLK 3FGA 3FGM  PCT   HI-GAME"
        Locate 4

        For I = 0 To 14

            If Left$(inputB$(I, 0), 3) <> "XXX" Then

                'Name
                Color 3: Print Left$(inputB$(I, 0), 14);

                'Steals
                Color 7: Print Tab(15); Using "####"; inputB%(I, 11);

                'Blocks
                Print Tab(20); Using "####"; inputB%(I, 12);

                '3FGA
                Print Tab(25); Using "####"; inputB%(I, 13);

                '3FGM
                Print Tab(30); Using "####"; inputB%(I, 14);

                '3FG PCT
                If inputB%(I, 14) = 0 Then Print Tab(35); ".000";
                If inputB%(I, 14) > 0 Then threeFGPCT! = inputB%(I, 14) / inputB%(I, 13): Print Tab(35); Using ".###"; threeFGPCT!;

                'HI GAME
                Print Tab(43); Using "####"; inputB%(I, 15)
                'PRINT TAB(53); USING "####"; inputB%(I, 16);
                'PRINT TAB(65); USING "####"; inputB%(I, 17)

            End If

        Next

        Color 14
        Print
        Print "TOTALS-OFF";

        'Steals
        Print Tab(15); Using "####"; inputT%(14);

        'Blocks
        Print Tab(20); Using "####"; inputT%(16);

        '3FGA
        Print Tab(25); Using "####"; inputT%(17);

        '3FGM
        Print Tab(30); Using "####"; inputT%(18);

        '3FG PCT
        If inputT%(18) = 0 Then Print Tab(35); ".000"

        If inputT%(18) > 0 Then
            statPCT! = inputT%(18) / inputT%(17)
            Print Tab(35); Using ".###"; statPCT!
        End If

        Color 11
        Print "TOTALS-DEF";

        'Steals
        Print Tab(15); Using "####"; inputT%(28);

        'Blocks
        Print Tab(20); Using "####"; inputT%(30);

        '3FGA
        Print Tab(25); Using "####"; inputT%(31);

        '3FGM
        Print Tab(30); Using "####"; inputT%(32);

        '3FG PCT
        If inputT%(32) = 0 Then Print Tab(35); ".000"

        If inputT%(32) > 0 Then
            statPCT! = inputT%(32) / inputT%(31)
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
'        InputEditTeam Subroutine               s
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

    Shared inputB%(), inputL%(), inputT%()
    Shared inputB$(), inputT$()

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

        Locate 3: Input "1. NUMBER OF GAMES "; inputL%(0)
        Locate 4: Input "2. AVG FGA/G "; inputL%(1)
        Locate 5: Input "3. AVG RB/G "; inputL%(2)
        Locate 6: Input "4. LEAGUE FG% "; inputL%(3)
        Locate 7: Input "5. AVG PF/G "; inputL%(4)
        Locate 8: Input "6. LEAGUE 3FG% "; inputL%(5)
        Locate 9: Input "7. LEAGUE POWER ADJ "; inputL%(6)
        Locate 10: Input "8. DISK ID "; yearNumber$
        Locate 11: Input "9. TEAM NUMBER (BLANK TO AUTO-ASSIGN)"; teamIdx

    Else
        'Modify Team

        Color 14
        Print "CHANGE LEAGUE INFORMATION"
        Color 15

        Locate 3: Print "1. NUMBER OF GAMES "; inputL%(0)
        Locate 4: Print "2. AVG FGA/G "; inputL%(1)
        Locate 5: Print "3. AVG RB/G "; inputL%(2)
        Locate 6: Print "4. LEAGUE FG% "; inputL%(3)
        Locate 7: Print "5. AVG PF/G "; inputL%(4)
        Locate 8: Print "6. LEAGUE 3FG% "; inputL%(5)
        Locate 9: Print "7. LEAGUE POWER ADJ "; inputL%(6)

    End If

    Do

        Do
            Locate 13
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 7

        Select Case B9

            Case 1
                Locate 3: Input "1. NUMBER OF GAMES "; inputL%(0)

            Case 2
                Locate 4: Input "2. AVG FGA/G "; inputL%(1)

            Case 3
                Locate 5: Input "3. AVG RB/G "; inputL%(2)

            Case 4
                Locate 6: Input "4. LEAGUE FG% "; inputL%(3)

            Case 5
                Locate 7: Input "5. AVG PF/G "; inputL%(4)

            Case 6
                Locate 8: Input "6. LEAGUE 3FG% "; inputL%(5)

            Case 7
                Locate 9: Input "7. LEAGUE POWER ADJ "; inputL%(6)

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
        Locate 5: Input "3. TEAM NAME "; inputT$(0)
        Locate 6: Input "4. TEAM NICKNAME "; inputT$(1)
        Locate 7: Input "5. TEAM GAMES PLAYED "; inputT%(0)
        Locate 8: Input "6. HOME WINS "; inputT%(1)
        Locate 9: Input "7. HOME LOSSES "; inputT%(2)
        Locate 10: Input "8. ROAD WINS "; inputT%(3)
        Locate 11: Input "9. ROAD LOSSES "; inputT%(4)

        Locate 12: Input "10. TEXT COLOR (0-15) "; inputT%(33)
        Locate 13: Input "11. BCKGRND COLOR (0-7) "; inputT%(34)
        Locate 18
        Print "TEAM APPEARANCE: ";
        Color inputT%(33), inputT%(34)
        Print inputT$(0) + " " + inputT$(1)
        Color 15, 0

        Locate 14: Input "12. TEAM ABBREV (IE. BOS) "; inputT$(2)
        Locate 15: Input "13. COACH'S NAME "; inputT$(3)
        Locate 16: Input "14. ARENA NAME "; inputT$(4)

    Else

        Cls
        Color 14
        Print "CHANGE TEAM INFORMATION"
        Color 15
        Print
        Print "1. TEAM NUMBER "; teamIdx
        Print "2. DISK ID "; yearNumber$
        Print "3. TEAM NAME "; inputT$(0)
        Print "4. TEAM NICKNAME "; inputT$(1)
        Print "5. TEAM GAMES PLAYED "; inputT%(0)
        Print "6. HOME WINS "; inputT%(1)
        Print "7. HOME LOSSES "; inputT%(2)
        Print "8. ROAD WINS "; inputT%(3)
        Print "9. ROAD LOSSES "; inputT%(4)
        Print "10. TEXT COLOR (0-15) "; inputT%(33)
        Print "11. BCKGRND COLOR (0-7) "; inputT%(34)
        Print "12. TEAM ABBREV (IE. BOS) "; inputT$(2)
        Print "13. COACH'S NAME "; inputT$(3)
        Print "14. ARENA NAME "; inputT$(4)

        Locate 16
        Print "TEAM APPEARANCE: ";
        Color inputT%(33), inputT%(34)
        Print inputT$(0) + " " + inputT$(1)
        Color 15, 0

    End If

    Do

        Do
            Locate 18
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 14

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 3: Input "1. TEAM NUMBER "; teamIdx

                Case 2
                    Locate 4: Input "2. DISK ID "; yearNumber$

                Case 3
                    Locate 5: Input "3. TEAM NAME "; inputT$(0)

                Case 4
                    Locate 6: Input "4. TEAM NICKNAME "; inputT$(1)

                Case 5
                    Locate 7: Input "5. TEAM GAMES PLAYED "; inputT%(0)

                Case 6
                    Locate 8: Input "6. HOME WINS "; inputT%(1)

                Case 7
                    Locate 9: Input "7. HOME LOSSES "; inputT%(2)

                Case 8
                    Locate 10: Input "8. ROAD WINS "; inputT%(3)

                Case 9
                    Locate 11: Input "9. ROAD LOSSES "; inputT%(4)

                Case 10
                    Locate 12: Input "10. TEXT COLOR (0-15) "; inputT%(33)
                    Locate 18
                    Print "TEAM APPEARANCE: ";
                    Color inputT%(33), inputT%(34)
                    Print inputT$(0)
                    Color 15, 0

                Case 11
                    Locate 13: Input "11. BCKGRND COLOR (0-7) "; inputT%(34)
                    Locate 18
                    Print "TEAM APPEARANCE: ";
                    Color inputT%(33), inputT%(34)
                    Print inputT$(0)
                    Color 15, 0

                Case 12
                    Locate 14: Input "12. TEAM ABBREV (IE. BOS) "; inputT$(2)

                Case 13
                    Locate 15: Input "13. COACH'S NAME "; inputT$(3)

                Case 14
                    Locate 16: Input "14. ARENA NAME "; inputT$(4)

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

        Locate 3: Input " 1. FGM "; inputT%(6)
        Locate 4: Input " 2. FGA "; inputT%(5)
        Locate 5: Input " 3. FTM "; inputT%(8)
        Locate 6: Input " 4. FTA "; inputT%(7)

        Locate 7: Input " 5. OFF REB "; inputT%(9)
        Locate 8: Input " 6. DEF REB "; inputT%(10)

        If inputT%(9) = 0 And RB = 0 Then
            inputT%(9) = .32 * inputT%(10)
            inputT%(10) = inputT%(10) - inputT%(9)
            Locate 7: Print " 5. OFF REB "; inputT%(9)
            Locate 8: Print " 6. DEF REB "; inputT%(10)
        End If

        Locate 9: Input " 7. ASSISTS "; inputT%(11)
        Locate 10: Input " 8. FOULS "; inputT%(12)
        Locate 11: Input " 9. DISQ "; inputT%(13)
        Locate 12: Input "10. STEALS "; inputT%(14)
        Locate 13: Input "11. TURNOVERS "; inputT%(15)
        Locate 14: Input "12. BLK SHOTS "; inputT%(16)
        Locate 15: Input "13. 3-FGM "; inputT%(18)
        Locate 16: Input "14. 3-FGA "; inputT%(17)

    Else

        Cls

        Color 14
        Print "CHANGE TEAM OFFENSIVE INFORMATION"
        Color 15
        Print
        Print " 1. FGM "; inputT%(6)
        Print " 2. FGA "; inputT%(5)
        Print " 3. FTM "; inputT%(8)
        Print " 4. FTA "; inputT%(7)
        Print " 5. OFF REB "; inputT%(9)
        Print " 6. DEF REB "; inputT%(10)
        Print " 7. ASSISTS "; inputT%(11)
        Print " 8. FOULS "; inputT%(12)
        Print " 9. DISQ "; inputT%(13)
        Print "10. STEALS "; inputT%(14)
        Print "11. TURNOVERS "; inputT%(15)
        Print "12. BLK SHOTS "; inputT%(16)
        Print "13. 3-FGM "; inputT%(18)
        Print "14. 3-FGA "; inputT%(17)

    End If

    Do

        Do
            Locate 18
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 14

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 3: Input " 1. FGM "; inputT%(6)

                Case 2
                    Locate 4: Input " 2. FGA "; inputT%(5)

                Case 3
                    Locate 5: Input " 3. FTM "; inputT%(8)

                Case 4
                    Locate 6: Input " 4. FTA "; inputT%(7)

                Case 5
                    Locate 7: Input " 5. OFF REB "; inputT%(9)

                Case 6
                    Locate 8: Input " 6. DEF REB "; inputT%(10)

                    If inputT%(9) = 0 And RB = 0 Then
                        inputT%(9) = .32 * inputT%(10)
                        inputT%(10) = inputT%(10) - inputT%(9)
                        Locate 7: Print " 5. OFF REB "; inputT%(9)
                        Locate 8: Print " 6. DEF REB "; inputT%(10)
                    End If

                Case 7
                    Locate 9: Input " 7. ASSISTS "; inputT%(11)

                Case 8
                    Locate 10: Input " 8. FOULS "; inputT%(12)

                Case 9
                    Locate 11: Input " 9. DISQ "; inputT%(13)

                Case 10
                    Locate 12: Input "10. STEALS "; inputT%(14)

                Case 11
                    Locate 13: Input "11. TURNOVERS "; inputT%(15)

                Case 12
                    Locate 14: Input "12. BLK SHOTS "; inputT%(16)

                Case 13
                    Locate 15: Input "13. 3-FGM "; inputT%(18)

                Case 14
                    Locate 16: Input "14. 3-FGA "; inputT%(17)

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

        Locate 3: Input " 1. FGM "; inputT%(20)
        Locate 4: Input " 2. FGA "; inputT%(19)
        Locate 5: Input " 3. FTM "; inputT%(22)
        Locate 6: Input " 4. FTA "; inputT%(21)
        Locate 7: Input " 5. OFF REB "; inputT%(23)
        Locate 8: Input " 6. DEF REB "; inputT%(24)

        If inputT%(23) = 0 And RB = 0 Then
            inputT%(23) = .35 * inputT%(24): inputT%(24) = inputT%(24) - inputT%(23)
            Locate 6: Print " 5. OFF REB "; inputT%(23)
            Locate 7: Print " 6. DEF REB "; inputT%(24)
        End If

        Locate 9: Input " 7. ASSISTS "; inputT%(25)
        Locate 10: Input " 8. FOULS "; inputT%(26)
        Locate 11: Input " 9. DISQ "; inputT%(27)
        Locate 12: Input "10. STEALS "; inputT%(28)
        Locate 13: Input "11. TURNOVERS "; inputT%(29)
        Locate 14: Input "12. BLK SHOTS "; inputT%(30)
        Locate 15: Input "13. 3-FGM "; inputT%(32)
        Locate 16: Input "14. 3-FGA "; inputT%(31)

    Else

        Cls

        Color 14
        Print "CHANGE TEAM DEFENSIVE INFORMATION"
        Color 15
        Print
        Print " 1. FGM "; inputT%(20)
        Print " 2. FGA "; inputT%(19)
        Print " 3. FTM "; inputT%(22)
        Print " 4. FTA "; inputT%(21)
        Print " 5. OFF REB "; inputT%(23)
        Print " 6. DEF REB "; inputT%(24)
        Print " 7. ASSISTS "; inputT%(25)
        Print " 8. FOULS "; inputT%(26)
        Print " 9. DISQ "; inputT%(27)
        Print "10. STEALS "; inputT%(28)
        Print "11. TURNOVERS "; inputT%(29)
        Print "12. BLK SHOTS "; inputT%(30)
        Print "13. 3-FGM "; inputT%(32)
        Print "14. 3-FGA "; inputT%(31)

    End If

    Do

        Do
            Locate 18
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 14

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 3: Input " 1. FGM "; inputT%(20)

                Case 2
                    Locate 4: Input " 2. FGA "; inputT%(19)

                Case 3
                    Locate 5: Input " 3. FTM "; inputT%(22)

                Case 4
                    Locate 6: Input " 4. FTA "; inputT%(21)

                Case 5
                    Locate 7: Input " 5. OFF REB "; inputT%(23)

                Case 6
                    Locate 8: Input " 6. DEF REB "; inputT%(24)

                    If inputT%(23) = 0 And RB = 0 Then
                        inputT%(23) = .35 * inputT%(24): inputT%(24) = inputT%(24) - inputT%(23)
                        Locate 7: Print " 5. OFF REB "; inputT%(23)
                        Locate 8: Print " 6. DEF REB "; inputT%(24)
                    End If

                Case 7
                    Locate 9: Input " 7. ASSISTS "; inputT%(25)

                Case 8
                    Locate 10: Input " 8. FOULS "; inputT%(26)

                Case 9
                    Locate 11: Input " 9. DISQ "; inputT%(27)

                Case 10
                    Locate 12: Input "10. STEALS "; inputT%(28)

                Case 11
                    Locate 13: Input "11. TURNOVERS "; inputT%(29)

                Case 12
                    Locate 14: Input "12. BLK SHOTS "; inputT%(30)

                Case 13
                    Locate 15: Input "13. 3-FGM "; inputT%(32)

                Case 14
                    Locate 16: Input "14. 3-FGA "; inputT%(31)

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
                Print "  "; inputB$(I, 0)
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

        'Input team
        If inputT%(28) = 0 And ST = 0 Then inputT%(28) = inputT%(5) * .1
        If inputT%(14) = 0 And ST = 0 Then inputT%(14) = inputT%(19) * .1
        If inputT%(16) = 0 And BK = 0 Then inputT%(16) = inputT%(19) * .06
        If inputT%(30) = 0 And BK = 0 Then inputT%(30) = inputT%(5) * .06

        For I = 0 To 14

            Cls

            Locate 1, 1

            Color 14
            Print Tab(31); "CHANGE PLAYER INFORMATION"
            Color 15

            Locate 2: Input " 1. NAME (XXX TO QUIT) "; inputB$(I, 0)

            If inputB$(I, 0) <> "XXX" Then

                Locate 3: Input " 2. POSITION "; inputB$(I, 1)
                Locate 4: Input " 3. GAMES "; inputB%(I, 0)
                Locate 5: Input " 4. MINUTES "; inputB%(I, 1)
                Locate 6: Input " 5. FGM "; inputB%(I, 3)
                Locate 7: Input " 6. FGA "; inputB%(I, 2)
                Locate 8: Input " 7. FTM "; inputB%(I, 5)
                Locate 9: Input " 8. FTA "; inputB%(I, 4)
                Locate 10: Input " 9. OFF REB "; inputB%(I, 6)
                Locate 11: Input "10. DEF REB "; inputB%(I, 7)

                If inputB%(I, 6) = 0 And RB = 0 Then
                    inputB%(I, 6) = .32 * inputB%(I, 7)
                    inputB%(I, 7) = inputB%(I, 7) - inputB%(I, 6)
                    Locate 10: Print " 9. OFF REB "; inputB%(I, 6)
                    Locate 11: Print "10. DEF REB "; inputB%(I, 7)
                End If

                Locate 12: Input "11. ASSISTS "; inputB%(I, 8)
                Locate 13: Input "12. FOULS "; inputB%(I, 9)

                position$ = Left$(inputB$(I, 1), 1)

                Locate 14: Input "13. DISQ "; inputB%(I, 10)

                If ST = 1 Then

                    Locate 15: Input "14. STEALS   "; inputB%(I, 11)

                Else
                    Select Case position$

                        Case "C":
                            inputB%(I, 11) = inputB%(I, 1) / 48 * 1.2
                            Locate 15: Print "14. STEALS "; inputB%(I, 11)

                        Case "F":
                            inputB%(I, 11) = inputB%(I, 1) / 48 * 1.55
                            Locate 15: Print "14. STEALS "; inputB%(I, 11)

                        Case "G":
                            inputB%(I, 11) = inputB%(I, 1) / 48 * 2.3
                            Locate 15: Print "14. STEALS "; inputB%(I, 11)

                    End Select

                End If

                If BK = 1 Then
                    Locate 16: Input "15. BLOCKS "; inputB%(I, 12)
                Else

                    Select Case position$

                        Case "C":
                            inputB%(I, 12) = inputB%(I, 1) / 48 * 2.5
                            Locate 16: Print "15. BLOCKS "; inputB%(I, 12)

                        Case "F":
                            inputB%(I, 12) = inputB%(I, 1) / 48 * 1.2
                            Locate 16: Print "15. BLOCKS "; inputB%(I, 12)

                        Case "G":
                            inputB%(I, 12) = inputB%(I, 1) / 48 * .4
                            Locate 16: Print "15. BLOCKS "; inputB%(I, 12)

                    End Select

                End If

                Locate 17: Input "16. 3-FGM "; inputB%(I, 14)
                Locate 18: Input "17. 3-FGA "; inputB%(I, 13)
                Locate 19: Input "18. HI-GAME "; inputB%(I, 15)
                'LOCATE 20:INPUT "19. PENETRATION (1-9) ";inputB%(I,16)
                'LOCATE 21:INPUT "20. DEF ADJ (-5 TO 5) ";inputB%(I,17)
                Locate 22: Input "21. ACTIVE FLAG (0-YES 1-NO) "; inputB%(I, 18)

                Call EditPlayer(I, inputAction%)

            Else

                Exit For

            End If 'Done with initial player params

        Next I

    End If

    For J = I To 14
        inputB$(J, 0) = "XXX"
        inputB$(J, 1) = "X"
        For I1 = 0 To 19: inputB%(J, I1) = -1: Next
    Next

End Sub


'----------------------------------------
'         EditPlayer Subroutine
'----------------------------------------
'...explanation...
Sub EditPlayer (J, action)

    Shared inputB%()

    Shared inputB$()

    Locate 1, 1

    Color 14
    Print Tab(31); "CHANGE PLAYER INFORMATION"
    Color 15

    If action = 2 Then

        Print " 1. NAME (XXX TO QUIT) "; inputB$(J, 0)
        Print " 2. POSITION "; inputB$(J, 1)
        Print " 3. GAMES "; inputB%(J, 0)
        Print " 4. MINUTES "; inputB%(J, 1)
        Print " 5. FGM "; inputB%(J, 3)
        Print " 6. FGA "; inputB%(J, 2)
        Print " 7. FTM "; inputB%(J, 5)
        Print " 8. FTA "; inputB%(J, 4)
        Print " 9. OFF REB "; inputB%(J, 6)
        Print "10. DEF REB "; inputB%(J, 7)
        Print "11. ASSISTS "; inputB%(J, 8)
        Print "12. FOULS "; inputB%(J, 9)
        Print "13. DISQ "; inputB%(J, 10)
        Print "14. STEALS "; inputB%(J, 11)
        Print "15. BLOCKS "; inputB%(J, 12)
        Print "16. 3-FGM "; inputB%(J, 14)
        Print "17. 3-FGA "; inputB%(J, 13)
        Print "18. HI-GAME "; inputB%(J, 15)
        Print 'PRINT "19. PENETRATION (1-9) ";inputB%(I,16)
        Print 'PRINT "20. DEF ADJ (-5 TO 5) ";inputB%(I,17)
        Print "21. ACTIVE FLAG (0-YES 1-NO) "; inputB%(J, 18)
    End If

    Do

        Do
            Locate 23
            Input "# TO CHANGE (0 TO CONTINUE ) "; B9
        Loop Until B9 >= 0 And B9 <= 21

        Select Case B9

            Case 1
                Locate 2: Input " 1. NAME (XXX TO QUIT) "; inputB$(I, 0)

            Case 2
                Locate 3: Input " 2. POSITION "; inputB$(I, 1)

            Case 3
                Locate 4: Input " 3. GAMES "; inputB%(I, 0)

            Case 4
                Locate 5: Input " 4. MINUTES "; inputB%(I, 1)

            Case 5
                Locate 6: Input " 5. FGM "; inputB%(I, 3)

            Case 6
                Locate 7: Input " 6. FGA "; inputB%(I, 2)

            Case 7
                Locate 8: Input " 7. FTM "; inputB%(I, 5)

            Case 8
                Locate 9: Input " 8. FTA "; inputB%(I, 4)

            Case 9
                Locate 10: Input " 9. OFF REB "; inputB%(I, 6)

            Case 10
                Locate 11: Input "10. DEF REB "; inputB%(I, 7)

                If inputB%(I, 6) = 0 And RB = 0 Then
                    inputB%(I, 6) = .32 * inputB%(I, 7): inputB%(I, 7) = inputB%(I, 7) - inputB%(I, 6)
                    Locate 10: Print " 9. OFF REB "; inputB%(I, 6)
                    Locate 11: Print "10. DEF REB "; inputB%(I, 7)
                End If

            Case 11
                Locate 12: Input "11. ASSISTS "; inputB%(I, 8)

            Case 12
                Locate 13: Input "12. FOULS "; inputB%(I, 9)

            Case 13
                Locate 14: Input "13. DISQ "; inputB%(I, 10)

            Case 14
                Locate 15: Input "14. STEALS   "; inputB%(I, 11)

            Case 15
                Locate 16: Input "15. BLOCKS "; inputB%(I, 12)

            Case 16
                Locate 17: Input "16. 3-FGM "; inputB%(I, 14)

            Case 17
                Locate 18: Input "17. 3-FGA "; inputB%(I, 13)

            Case 18
                Locate 19: Input "18. HI-GAME "; inputB%(I, 15)

            Case 19:
                'LOCATE 20:INPUT "19. PENETRATION (1-9) ";inputB%(I,16)

            Case 20:
                'LOCATE 21:INPUT "20. DEF ADJ (-5 TO 5) ";inputB%(I,17)

            Case 21:
                Locate 22: Input "21. ACTIVE FLAG (0-YES 1-NO) "; inputB%(I, 18)

        End Select

    Loop Until B9 = 0



End Sub

'----------------------------------------
'      GetTeam_CNGTM Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_CNGTM (A$, yearNumber$, teamIdx)

    Shared inputT%()

    Shared inputT$()

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
            Color inputT%(33), inputT%(34): Print inputT$(0)
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

    Shared inputB%(), inputL%(), inputT%()
    Shared inputB$(), inputT$()

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

        inputT$(0) = RTrim$(Q$(0))
        inputT$(1) = RTrim$(Q$(1))

        For I = 0 To 6
            inputL%(I) = CVI(Q$(I + 2))
        Next

        For I = 0 To 34
            inputT%(I) = CVI(Q$(I + 10))
        Next

        For I = 0 To 14
            inputB$(I, 0) = Q$(22 * I + 45)
            inputB$(I, 1) = Q$(22 * I + 46)
            For I1 = 0 To 19
                inputB%(I, I1) = CVI(Q$(22 * I + 47 + I1))
            Next
        Next

        inputT$(2) = RTrim$(Q$(375))
        inputT$(3) = RTrim$(Q$(376))
        inputT$(4) = RTrim$(Q$(377))

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

    Shared inputB%(), inputL%(), inputT%()
    Shared inputB$(), inputT$()

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

    LSet Q$(0) = inputT$(0)
    LSet Q$(1) = inputT$(1)

    For I = 0 To 6
        LSet Q$(I + 2) = MKI$(inputL%(I))
    Next

    LSet Q$(9) = MKI$(999)

    For I = 0 To 34: LSet Q$(I + 10) = MKI$(inputT%(I)): Next

    For I = 0 To 14
        LSet Q$(22 * I + 45) = inputB$(I, 0)
        LSet Q$(22 * I + 46) = inputB$(I, 1)
        For I1 = 0 To 19
            LSet Q$(22 * I + 47 + I1) = MKI$(inputB%(I, I1))
        Next
    Next

    LSet Q$(375) = inputT$(2)
    LSet Q$(376) = inputT$(3)
    LSet Q$(377) = inputT$(4)

    Put #1, teamIdx

    Close 1

End Sub

