'----------------------------------------
'            LOOKY Subroutine
'----------------------------------------
'This subroutine guides the user through
'choosing a team file and then calls other
'routines to load and view the stats
Sub LOOKY ()

    _Title "Basketball: The Pro Game - View Stats"

    Shared categories$(), lookyA$()

    menuExit = 0

    Call ChooseTeam(YN$, NTMS, teamNbr, team$)
    
    If teamNbr > 0 Then

        statFile$ = nameStatFile$(team$)

        If Not _FileExists(diskPaths$(1) + statFile$) Then

            MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
            BoxWidth = 24
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            Call LoadTeamStats(statFile$, totalGames%)
            Call ViewStatSummary(statFile$, team$, totalGames%, menuExit)

        End If

    End If

End Sub

'----------------------------------------
'        LoadTeamStats Subroutine
'----------------------------------------
'This subroutine loads the selected team's 
'stats into arrays
Sub LoadTeamStats (statFile$, totalGames%)

    Shared HL%, HW%, L%, NL%, NW%, VL%, VW%, W%, Z%

    Dim P%(0 To 100)

    Shared statO%(), statsL%()

    Shared categories$(), lookyA$()
    Shared O$(), statsH$()

    Shared compareZ!(), compareZ1!()
    Shared values!(), statW0!(), statW1!()

    ' *** READ IN TEAM STATS ***

    P%(0) = 0
    P%(1) = 0

    L% = 0
    W% = 0
    Z% = 0
    HW% = 0
    VW% = 0
    NW% = 0
    HL% = 0
    VL% = 0
    NL% = 0

    Open diskPaths$(1) + statFile$ + "S" For Input As #1

    Input #1, totalGames%

    For I = 1 To 100: 
        Input #1, statO%(I)
    Next I

    For I = 1 To 100: 
        Input #1, O$(I)
    Next I

    For I = 1 To 100: 
        Input #1, statsL%(I)
    Next I

    For I = 1 To 100: 
        Input #1, statsH$(I)
    Next I

    Close #1

    For I = 1 To totalGames%
        If statO%(I) > statsL%(I) Then W% = W% + 1
        If statO%(I) > statsL%(I) And Left$(statsH$(I), 1) = "H" Then HW% = HW% + 1
        If statO%(I) > statsL%(I) And Left$(statsH$(I), 1) = "V" Then VW% = VW% + 1
        If statO%(I) > statsL%(I) And Left$(statsH$(I), 1) = "N" Then NW% = NW% + 1
        If statO%(I) < statsL%(I) Then L% = L% + 1
        If statO%(I) < statsL%(I) And Left$(statsH$(I), 1) = "H" Then HL% = HL% + 1
        If statO%(I) < statsL%(I) And Left$(statsH$(I), 1) = "V" Then VL% = VL% + 1
        If statO%(I) < statsL%(I) And Left$(statsH$(I), 1) = "N" Then NL% = NL% + 1
        If statO%(I) = statsL%(I) Then Z% = Z% + 1
        P%(0) = P%(0) + statO%(I)
        P%(1) = P%(1) + statsL%(I)
    Next

    Open diskPaths$(1) + statFile$ For Input As #2

    Input #2, B$

    For I = 0 To 14

        Input #2, lookyA$(I)

        For J = 0 To 13
            Input #2, values!(I, J)
        Next

        Input #2, statW0!(I)

        Input #2, statW1!(I)

    Next

    For K = 0 To 15: 
        Input #2, compareZ!(K)
    Next

    For K = 0 To 15: 
        Input #2, compareZ1!(K)
    Next

    Close #2

    For I = 0 To 14

        If values!(I, 0) <> 0 Then 
            values!(I, 14) = values!(I, 10) / values!(I, 0)
            values!(I, 20) = (values!(I, 2) * 2 + values!(I, 4) + statW0!(I)) / values!(I, 0)
            values!(I, 21) = values!(I, 5) / values!(I, 0)
        End If

        values!(I, 22) = values!(I, 2) * 2 + values!(I, 4) + statW0!(I)
        values!(I, 23) = values!(I, 5) - values!(I, 12)

        If values!(I, 1) <> 0 Then values!(I, 15) = values!(I, 2) / values!(I, 1)

        values!(I, 18) = statW0!(I)
        values!(I, 19) = statW1!(I)

        If values!(I, 19) <> 0 Then values!(I, 16) = values!(I, 18) / values!(I, 19)
        If values!(I, 3) <> 0 Then values!(I, 17) = values!(I, 4) / values!(I, 3)

    Next

End Sub

'----------------------------------------
'       ViewStatSummary Subroutine
'----------------------------------------
'This subroutine formats and outputs the 
'selected team's record to the display,
'then provides routines for output of
'the other stat categories
Sub ViewStatSummary (statFile$, teamName$, totalGames%, menuExit)

    Shared HL%, HW%, L%, NL%, NW%, VL%, VW%, W%

    Shared statO%(), statsL%()

    Shared categories$()
    Shared O$(), statsH$()

    Color , 0
    Cls

    Locate 1, 1
    Color 14, 1

    Print Tab(80);
    Xcoord = Len(teamName$)
    Xcoord = Int((80 - Xcoord) / 2)
    Locate , Xcoord
    Print teamName$

    Locate 25, 1: Print Tab(80);
    Locate 2, 1
    Color 15, 0

    For I = 1 To totalGames%

        Color 7

        If I / 2 = Int(I / 2) Then Color 15

        If I > 54 Then Exit For

        If I > 36 Then
            Locate I - 35, 53: Print Using "###"; statO%(I);
            Print Tab(57); Left$(O$(I), 14);
            Print Tab(72); Using "###"; statsL%(I);
            Print Tab(76); statsH$(I)

        Else
            If I > 18 Then
                Locate I - 17, 27: Print Using "###"; statO%(I);
                Print Tab(31); Left$(O$(I), 14);
                Print Tab(46); Using "###"; statsL%(I);
                Print Tab(50); statsH$(I)

            Else
                Print Using "###"; statO%(I);
                Print Tab(5); Left$(O$(I), 14);
                Print Tab(20); Using "###"; statsL%(I);
                Print Tab(24); statsH$(I)
            End If

        End If

    Next I

    Print

    Locate 21, 27
    Color 14: Print "OVERALL RECORD ";
    Color 15: Print "("; W%; "-"; L%; ")"
    Locate , 27
    Color 14: Print "HOME RECORD    ";
    Color 15: Print "("; HW%; "-"; HL%; ")"
    Locate , 27
    Color 14: Print "ROAD RECORD    ";
    Color 15: Print "("; VW%; "-"; VL%; ")"
    Locate , 27
    Color 14: Print "NEUTRAL RECORD ";
    Color 15: Print "("; NW%; "-"; NL%; ")";

    Color 3
    Locate 25, 1
    Color 15, 1: Print "INDIVIDUAL STATS ";
    Color 14: Print "[Pg DN]  ";
    Color 15: Print "SELECT ANOTHER TEAM ";
    Color 14: Print "[ESC]  ";
    Color 15: Print "PRINT STATS ";
    Color 14: Print "[P]";

    Locate 3, 1

    Do

        I$ = GetKeyPress$

        Select Case UCase$(I$)
            'Pg Down
            Case Chr$(0) + "Q":
                Color , 0
                Cls
                Call ExtendedStatSummary(statFile$, teamName$, totalGames%)

            Case "P":
                Color 0, 15

                Call PrintStatsSchedule(teamName$, totalGames%)
                'I$ = GetKeyPress$
                Call PrintStatsSummary(teamName$, totalGames%)

            Case "D":
                Kill diskPaths$(1) + statFile$
                Kill diskPaths$(1) + statFile$ + "S"
                'Esc

        End Select

    Loop Until I$ = Chr$(0) + "Q" Or I$ = Chr$(27)

    If I$ = Chr$(27) Then menuExit = 1

End Sub


'----------------------------------------
'     ExtendedStatSummary Subroutine
'----------------------------------------
'This subroutine formats and outputs the 
'team's schedule stats to the  user's display
Sub ExtendedStatSummary (statFile$, teamName$, totalGames%)

    Shared HL%, HW%, L%, NL%, NW%, VL%, VW%, W%

    Shared statsL%()

    Shared categories$()
    Shared O$(), statsH$()

    Color , 0
    Cls

    Locate 1, 1
    Color 14, 1

    Print Tab(80);
    Xcoord = Len(teamName$)
    Xcoord = Int((80 - Xcoord) / 2)
    Locate , Xcoord
    Print teamName$

    Locate 25, 1: Print Tab(80);
    Locate 2, 1
    Color 15, 0

    For I = 1 To totalGames%
        Color 7
        If I / 2 = Int(I / 2) Then Color 15
        If I >= 55 Then
            If I <= 72 Then
                Print Using "###"; statO%(I);
                Print Tab(5); Left$(O$(I), 14);
                Print Tab(20); Using "###"; statsL%(I);
                Print Tab(24); statsH$(I)
            Else
                Locate I - 71, 27
                Print Using "###"; statO%(I);
                Print Tab(31); Left$(O$(I), 14);
                Print Tab(46); Using "###"; statsL%(I);
                Print Tab(50); statsH$(I)
            End If
        End If

    Next I

    Print
    Locate 21, 27
    Color 14: Print "OVERALL RECORD ";: Color 15: Print "("; W%; "-"; L%; ")"
    Locate , 27: Color 14: Print "HOME RECORD    ";: Color 15: Print "("; HW%; "-"; HL%; ")"
    Locate , 27: Color 14: Print "ROAD RECORD    ";: Color 15: Print "("; VW%; "-"; VL%; ")"
    Locate , 27: Color 14: Print "NEUTRAL RECORD ";: Color 15: Print "("; NW%; "-"; NL%; ")";
    Color 3
    Locate 25, 1
    Color 15, 1: Print "INDIVIDUAL STATS ";
    Color 14: Print "[Pg DN]  ";
    Color 15: Print "SELECT ANOTHER TEAM ";
    Color 14: Print "[ESC]  ";
    Color 15: Print "PRINT STATS ";
    Color 14: Print "[P]";
    Locate 3, 1

    Do
        I$ = GetKeyPress$

        Select Case UCase$(I$)
            'Pg Down
            Case Chr$(0) + "Q":
                Color , 0
                Cls
                Call ViewPlayerStats(statFile$, teamName$, CP%, totalGames%, menuExit)

                'Pg Up
            Case Chr$(0) + "I":
                Color , 0
                Cls
                Call ViewStatSummary(statFile$, teamName$, totalGames%, menuExit)

                'Delete
            Case "d","D":
                Kill diskPaths$(1) + statFile$
                Kill diskPaths$(1) + statFile$ + "S"

                'Print
            Case "p","P":

                Call PrintStatsSchedule(teamName$, totalGames%)
                'I$ = GetKeyPress$
                Call PrintStatsSummary(teamName$, totalGames%)

        End Select

    Loop Until I$ = Chr$(27) Or I$ = Chr$(0) + "Q" Or menuExit = 1

End Sub


'----------------------------------------
'       ViewPlayerStats Subroutine
'----------------------------------------
'This subroutine formats and outputs the 
'team's player stats to the user display
Sub ViewPlayerStats (statFile$, teamName$, CP%, totalGames%, menuExit)

    Shared HL%, HW%, L%, NL%, NW%, VL%, VW%, W%

    Shared categories$(), lookyA$()

    Shared compareZ!(), compareZ1!()
    Shared values!(), statW0!(), statW1!()

    Do

        Call TITLE_HEADER

        Color , 1
        Locate 23, 1

        Color 15, 1: Print "SCHEDULE ";
        Color 14: Print "[Pg UP]  ";
        Color 15: Print "MORE STATS ";
        Color 14: Print "[Pg DN]  ";
        Color 15: Print "NEW TEAM ";
        Color 14: Print "[ESC]  ";
        Color 15: Print "SORT ";
        Color 14: Print "<-,-> and [ENTER]    "
        Color 15: Print "PRINT ";
        Color 14: Print "[P]  ";
        Color 15: Print "DELETE STATS ";
        Color 14: Print "[D]";
        Print Space$(53);

        Color 12, 0
        Locate 2, 1: Print "SORT BY: "; categories$(CP%); Tab(40);

        Locate 3, 1
        Color 14, 0: Print teamName$
        Print "NAME"; Tab(15);
        Print " G  MIN AVG   FGM  FGA  PCT  3FGM 3FGA  PCT   FTM  FTA  PCT"

        For I = 0 To 14

            If lookyA$(I) <> "XXX" Then
                Color 7
                If I / 2 = Int(I / 2) Then Color 15
                Print Left$(lookyA$(I), 13); Tab(15);

                'Games
                Print Using "## "; values!(I, 0);

                'Min, Avg
                If values!(I, 10) > 0 Then Locate , 18: Print Using "####"; Int((values!(I, 10) + 30) / 60);: Locate , 24: Print Using "##"; Int(((values!(I, 10) / 60) / values!(I, 0)) + .5);

                'FGM, FGA, FG PCT
                Locate , 28: Print Using "#### ####"; values!(I, 2); values!(I, 1);: Locate , 38

                If values!(I, 1) = 0 Then
                    Print ".---";
                Else
                    If values!(I, 2) = values!(I, 1) Then
                        Print "1.00";
                    Else
                        If values!(I, 2) = 0 Then
                            Print ".000";
                        Else
                            fgPCT! = values!(I, 2) / values!(I, 1)
                            Print Using ".###"; fgPCT!;
                        End If
                    End If
                End If

                '3FGM, 3FGA, 3FG PCT
                Locate , 44: Print Using "#### ####"; statW0!(I); statW1!(I);: Locate , 54

                If statW1!(I) = 0 Then
                    Print ".---";
                Else
                    If statW1!(I) = statW0!(I) Then
                        Print "1.00";
                    Else
                        If statW0!(I) = 0 Then
                            Print ".000";
                        Else
                            threeFGPCT! = statW0!(I) / statW1!(I)
                            Print Using ".###"; threeFGPCT!;
                        End If
                    End If
                End If

                'FTM, FTA, FT PCT
                Locate , 60: Print Using "#### ####"; values!(I, 4); values!(I, 3);

                Locate , 70

                If values!(I, 3) = 0 Then
                    Print ".---"
                Else

                    If values!(I, 3) = values!(I, 4) Then
                        Print "1.00"
                    Else

                        If values!(I, 4) = 0 Then
                            Print ".000"
                        Else
                            ftPCT! = values!(I, 4) / values!(I, 3)
                            Print Using ".###"; ftPCT!
                        End If

                    End If

                End If

            End If 'done checking player name

        Next

        Color 12: Print: Print "TOTALS";: Locate , 15

        'Games
        Print Using "##"; totalGames%;

        '-- Should be MIN / AVG points but is excluded; perhaps because AVG cannot be a TOTAL
        '-- and presumably total MIN is not meaningful
        '-- However it could be updated to still do, and use an average of the "AVG" values

        'FGM / FGA
        Locate , 28: Print Using "#### ####"; compareZ!(2); compareZ!(1);
        If compareZ!(1) <> 0 Then fgPCT! = compareZ!(2) / compareZ!(1): Locate , 38: Print Using ".###"; fgPCT!;

        '3FGM / 3FGA
        Locate , 44: Print Using "#### ####"; compareZ!(13); compareZ!(14);
        If compareZ!(14) <> 0 Then threeFGPCT! = compareZ!(13) / compareZ!(14): Locate , 54: Print Using ".###"; threeFGPCT!;

        'FTM / FTA
        Locate , 60: Print Using "#### ####"; compareZ!(4); compareZ!(3);
        If compareZ!(3) <> 0 Then ftPCT! = compareZ!(4) / compareZ!(3): Locate , 70: Print Using ".###"; ftPCT!

        '2nd Row
        'Opponents???

        'Games
        Color 12: Locate , 15: Print Using "##"; totalGames%;

        'FGM / FGA
        Locate , 28: Print Using "#### ####"; compareZ1!(2); compareZ1!(1);
        If compareZ1!(1) <> 0 Then fgPCT! = compareZ1!(2) / compareZ1!(1): Locate , 38: Print Using ".###"; fgPCT!;

        '3FGM / 3FGA
        Locate , 44: Print Using "#### ####"; compareZ1!(13); compareZ1!(14);
        If compareZ1!(14) <> 0 Then threeFGPCT! = compareZ1!(13) / compareZ1!(14): Locate , 54: Print Using ".###"; threeFGPCT!;

        'FTM / FTA
        Locate , 60: Print Using "#### ####"; compareZ1!(4); compareZ1!(3);
        If compareZ1!(3) <> 0 Then ftPCT! = compareZ1!(4) / compareZ1!(3): Locate , 70: Print Using ".###"; ftPCT!

        Do
            I$ = GetKeyPress$

            Select Case I$

                'Pg Down
                Case Chr$(0) + "Q"
                    Call ViewTeamStats(statFile$, teamName$, CP%, totalGames%, menuExit)

                    'Pg Up
                Case Chr$(0) + "I"
                    Cls
                    Call ExtendedStatSummary(statFile$, teamName$, totalGames%)

                    'Right Arrow
                Case Chr$(0) + "M"
                    CP% = CP% + 1
                    If CP% > 12 Then CP% = 1
                    Color 12
                    Locate 2, 1: Print "SORT BY: "; categories$(CP%); Tab(40);

                    'Left Arrow
                Case Chr$(0) + "K"
                    CP% = CP% - 1
                    If CP% < 1 Then CP% = 12
                    Color 12
                    Locate 2, 1: Print "SORT BY: "; categories$(CP%); Tab(40);

                    'Enter
                Case Chr$(13)
                    Call SortStats(CP%)

                    'Delete
                Case "d", "D"
                    Kill diskPaths$(1) + statFile$
                    Kill diskPaths$(1) + statFile$ + "S"

                    'Print
                Case "p", "P"

                    Call PrintStatsSchedule(teamName$, totalGames%)
                    'I$ = GetKeyPress$
                    Call PrintStatsSummary(teamName$, totalGames%)

            End Select

        Loop Until I$ = Chr$(0) + "Q" Or I$ = Chr$(27) Or I$ = Chr$(13)

        If I$ = Chr$(27) Then menuExit = 1

    Loop Until I$ <> Chr$(13)

End Sub


'----------------------------------------
'     ViewTeamStats Subroutine
'----------------------------------------
'This subroutine formats and outputs the 
'team's OFF/DEF stats to the user display
Sub ViewTeamStats (statFile$, teamName$, CP%, totalGames%, menuExit)

    Shared HL%, HW%, L%, NL%, NW%, VL%, VW%, W%

    Shared categories$(), lookyA$()

    Shared compareZ!(), compareZ1!()
    Shared values!(), statW0!(), statW1!()

    CP% = 13

    Do

        Cls

        Call TITLE_HEADER

        Color , 1

        Locate 23, 1

        Color 15: Print "MORE STATS ";
        Color 14: Print "[Pg UP]  ";
        Color 15: Print "SELECT NEW TEAM ";
        Color 14: Print "[ESC]  ";
        Color 15: Print "SORT ";
        Color 14: Print "<-,-> and [ENTER]  ";
        Color 15: Print "PRINT ";
        Color 14: Print "[P]    ";
        Color 15: Print "DELETE STATS ";
        Color 14: Print "[D]";
        Print Space$(64);

        Color 3, 0
        Locate 2, 1: Print "SORT BY: "; categories$(CP%); Tab(40);

        'Player Stats
        Locate 3, 1
        Color 14, 0: Print teamName$;
        Locate , 15
        Print "               OFF  DEF  TOT": Print "NAME"; Tab(15);
        Print " PTS   AVG HI  REB  REB  REB   AVG   PF  AST  BLK  STL   TO"

        For I = 0 To 14

            If lookyA$(I) <> "XXX" Then

                Color 7
                If I / 2 = Int(I / 2) Then Color 15

                'Name
                Print Left$(lookyA$(I), 13);: Locate , 15

                'Points
                Print Using "####"; values!(I, 2) * 2 + values!(I, 4) + statW0!(I);

                'Average
                If values!(I, 0) > 0 Then Locate , 21: Print Using "##.#"; (values!(I, 2) * 2 + values!(I, 4) + statW0!(I)) / values!(I, 0);

                'Highest Points, Rebounds
                Locate , 26: Print Using "## #### #### ####"; values!(I, 13); values!(I, 12); values!(I, 5) - values!(I, 12); values!(I, 5);

                'Average
                If values!(I, 0) > 0 Then Locate , 45: Print Using "##.#"; values!(I, 5) / values!(I, 0);

                'PF AST   BLK  STL  TO
                Locate , 51: Print Using "### ####  ###  ###  ###"; values!(I, 6); values!(I, 8); values!(I, 9); values!(I, 7); values!(I, 11)
            End If

        Next I

        Print "TEAM";: Locate , 39: Print Using "####"; compareZ!(15)

        '1st Row
        Color 12: Print "TOTALS";

        'Points
        If totalGames% > 0 Then ptsCalc! = compareZ!(10) / totalGames% Else ptsCalc! = 0
        Locate , 14: Print Using "##### ###.#"; compareZ!(10); ptsCalc!;

        'Rebounds
        If totalGames% > 0 Then rebCalc! = compareZ!(5) / totalGames% Else rebCalc! = 0
        Locate , 29: Print Using "#### #### ####  ##.#"; compareZ!(12); compareZ!(5) - compareZ!(12) - compareZ!(15); compareZ!(5); rebCalc!;

        'PF  AST  BLK  STL  TO
        Print Using " #### #### #### #### ####"; compareZ!(6); compareZ!(8); compareZ!(9); compareZ!(7); compareZ!(11)

        '2nd Row
        'Opponents???

        'Points
        If totalGames% > 0 Then ptsCalc! = compareZ1!(10) / totalGames% Else ptsCalc! = 0
        Locate , 14: Print Using "##### ###.#"; compareZ1!(10); ptsCalc!;

        'Rebounds
        If totalGames% > 0 Then rebCalc! = compareZ1!(5) / totalGames% Else rebCalc! = 0
        Locate , 29: Print Using "#### #### ####  ##.#"; compareZ1!(12); compareZ1!(5) - compareZ1!(12) - compareZ1!(15); compareZ1!(5); rebCalc!;

        'PF  AST  BLK  STL  TO
        Print Using " #### #### #### #### ####"; compareZ1!(6); compareZ1!(8); compareZ1!(9); compareZ1!(7); compareZ1!(11)

        Do

            J$ = GetKeyPress$

            Select Case J$

                'Pg Up
                Case Chr$(0) + "I":
                    Cls
                    Call ViewPlayerStats(statFile$, teamName$, CP%, totalGames%, menuExit)

                    'Right Arrow
                Case Chr$(0) + "M":
                    CP% = CP% + 1: If CP% > 25 Then CP% = 13
                    Color 3
                    Locate 2, 1: Print "SORT BY: "; categories$(CP%); Tab(40);

                    'Left Arrow
                Case Chr$(0) + "K":
                    CP% = CP% - 1
                    If CP% < 13 Then CP% = 25
                    Color 3
                    Locate 2, 1: Print "SORT BY: "; categories$(CP%); Tab(40);

                Case Chr$(13):
                    Call SortStats(CP%)

                Case "d","D":
                    Kill diskPaths$(1) + statFile$
                    Kill diskPaths$(1) + statFile$ + "S"

                Case "p","P":
                    Call PrintStatsSchedule(teamName$, totalGames%)
                    'K$ = GetKeyPress$
                    Call PrintStatsSummary(teamName$, totalGames%)

            End Select

        Loop Until J$ = Chr$(27) Or J$ = Chr$(13)

    Loop Until J$ = Chr$(27) Or J$ = Chr$(0) + "I"


End Sub


'----------------------------------------
'    PrintStatsSchedule Subroutine
'----------------------------------------
'This subroutine formats and outputs the 
'team's schedule results to the default printer
Sub PrintStatsSchedule (teamName$, totalGames%)

    Shared HL%, HW%, L%, NL%, NW%, VL%, VW%, W%

    Shared O%(), statsL%()

    Shared statsH$(), O$()

    LPrint "SCHEDULE RESULTS FOR "; teamName$
    LPrint

    For I = 1 To 28
        LPrint Using "###"; statO%(I);
        LPrint Tab(5); Left$(O$(I), 14);
        LPrint Tab(20); Using "###"; statsL%(I);
        LPrint Tab(24); statsH$(I); Tab(27);

        If I + 28 > totalGames% Then
            LPrint
        Else
            LPrint Using "###"; statO%(I + 28);
            LPrint Tab(31); Left$(O$(I + 28), 14);
            LPrint Tab(46); Using "###"; statsL%(I + 28);
            LPrint Tab(50); statsH$(I + 28); Tab(53);

            If I + 56 > totalGames% Then
                LPrint
            Else
                LPrint Using "###"; statO%(I + 56);
                LPrint Tab(57); Left$(O$(I + 56), 14);
                LPrint Tab(72); Using "###"; statsL%(I + 56);
                LPrint Tab(76); statsH$(I + 56)
            End If
        End If

    Next I

    LPrint Chr$(12)

    LPrint
    LPrint Tab(27); "OVERALL RECORD ("; W%; "-"; L%; ")"
    LPrint Tab(27); "HOME RECORD    ("; HW%; "-"; HL%; ")"
    LPrint Tab(27); "ROAD RECORD    ("; VW%; "-"; VL%; ")"
    LPrint Tab(27); "NEUTRAL RECORD ("; NW%; "-"; NL%; ")"

    LPrint Chr$(12)

End Sub

'----------------------------------------
'      PrintStatsSummary Subroutine
'----------------------------------------
'This subroutine formats and outputs the 
'team's stats to the default printer
Sub PrintStatsSummary (teamName$, totalGames%)

    Shared compareZ!(), compareZ1!()
    Shared values!(), statW0!(), statW1!()

    Shared lookyA$()

    LPrint "STATISTICS SUMMARY FOR "; teamName$
    LPrint "NAME"; Tab(15);
    LPrint " G  MIN AVG   FGM  FGA  PCT  3FGM 3FGA  PCT   FTM  FTA  PCT"

    For I = 0 To 14
        LPrint Left$(lookyA$(I), 13); Tab(15);

        'Games
        LPrint Using "## "; values!(I, 0);

        'Min, Avg
        If values!(I, 10) > 0 Then LPrint Tab(18); Using "####"; Int((values!(I, 10) + 30) / 60);: LPrint Tab(24); Using "##"; Int(((values!(I, 10) / 60) / values!(I, 0)) + .5);

        'FGM, FGA, FG PCT
        LPrint Tab(28); Using "#### ####"; values!(I, 2); values!(I, 1);
        LPrint Tab(38);

        If values!(I, 1) = 0 Then
            LPrint ".---";
        Else
            If values!(I, 2) = values!(I, 1) Then
                LPrint "1.00";
            Else
                If values!(I, 2) = 0 Then
                    LPrint ".000";
                Else
                    fgPCT! = values!(I, 2) / values!(I, 1)
                    LPrint Using ".###"; fgPCT!;
                End If
            End If
        End If

        '3FGM, 3FGA, 3FG PCT
        LPrint Tab(44); Using "#### ####"; statW0!(I); statW1!(I);: LPrint Tab(54);

        If statW1!(I) = 0 Then
            LPrint ".---";
        Else
            If statW1!(I) = statW0!(I) Then
                LPrint "1.00";
            Else

                If statW0!(I) = 0 Then
                    LPrint ".000";
                Else
                    threeFGPCT! = statW0!(I) / statW1!(I)
                    LPrint Using ".###"; threeFGPCT!;
                End If
            End If
        End If

        'FTM, FTA, FT PCT
        LPrint Tab(60); Using "#### ####"; values!(I, 4); values!(I, 3);
        LPrint Tab(70);

        If values!(I, 3) = 0 Then
            LPrint ".---"
        Else
            If values!(I, 3) = values!(I, 4) Then
                LPrint "1.00"
            Else
                If values!(I, 4) = 0 Then
                    LPrint ".000"
                Else
                    ftPCT! = values!(I, 4) / values!(I, 3)
                    LPrint Using ".###"; ftPCT!
                End If
            End If
        End If

    Next

    LPrint
    LPrint "TOTALS"; Tab(15);

    'Games
    LPrint Using "##"; totalGames%;

    '-- Should be MIN / AVG points but is excluded; perhaps because AVG cannot be a TOTAL
    '-- and presumably total MIN is not meaningful
    '-- However it could be updated to still do, and use an average of the "AVG" values
    'If compareZ!(0) > 0 Then LPrint Tab(18); Using "####"; Int((compareZ!(10) + 30) / 60);: LPrint Tab(24); Using "##"; Int(((compareZ!(10) / 60) / compareZ!(0)) + .5);

    'FGM / FGA / FGPCT
    LPrint Tab(28); Using "#### ####"; compareZ!(2); compareZ!(1);
    If compareZ!(1) <> 0 Then LPrint Tab(38); Using ".###"; compareZ!(2) / compareZ!(1);

    '3FGM / 3FGA / 3FGPCT
    LPrint Tab(44); Using "#### ####"; compareZ!(13); compareZ!(14);
    If compareZ!(14) <> 0 Then LPrint Tab(54); Using ".###"; compareZ!(13) / compareZ!(14);

    'FTM / FTA / FTPCT
    LPrint Tab(60); Using "#### ####"; compareZ!(4); compareZ!(3);
    If compareZ!(3) <> 0 Then LPrint Tab(70); Using ".###"; compareZ!(4) / compareZ!(3)

    'Games
    LPrint Tab(15); Using "##"; totalGames%;

    'FGM / FGA / FGPCT
    LPrint Tab(28); Using "#### ####"; compareZ1!(2); compareZ1!(1);
    If compareZ1!(1) <> 0 Then LPrint Tab(38); Using ".###"; compareZ1!(2) / compareZ1!(1);

    '3FGM / 3FGA / 3FGPCT
    LPrint Tab(44); Using "#### ####"; compareZ1!(13); compareZ1!(14);
    If compareZ1!(14) <> 0 Then LPrint Tab(54); Using ".###"; compareZ1!(13) / compareZ1!(14);

    'FTM / FTA / FTPCT
    LPrint Tab(60); Using "#### ####"; compareZ1!(4); compareZ1!(3);
    If compareZ1!(3) <> 0 Then LPrint Tab(70); Using ".###"; compareZ1!(4) / compareZ1!(3)

    'Player Stats
    LPrint
    LPrint Tab(15);
    LPrint "               OFF  DEF  TOT": LPrint Tab(15);
    LPrint " PTS   AVG HI  REB  REB  REB   AVG   PF  AST  BLK  STL   TO"

    For I = 0 To 14
        'Player Name
        LPrint Left$(lookyA$(I), 13);: LPrint Tab(15);

        'Points
        LPrint Using "####"; values!(I, 2) * 2 + values!(I, 4) + statW0!(I);

        'Average
        If values!(I, 0) > 0 Then LPrint Tab(21); Using "##.#"; (values!(I, 2) * 2 + values!(I, 4) + statW0!(I)) / values!(I, 0);

        'Highest Points, Rebounds
        LPrint Tab(26); Using "## #### #### ####"; values!(I, 13); values!(I, 12); values!(I, 5) - values!(I, 12); values!(I, 5);

        'Average Rebounds
        If values!(I, 0) > 0 Then LPrint Tab(45); Using "##.#"; values!(I, 5) / values!(I, 0);

        'PF / AST / BLK / STL / TO
        LPrint Tab(51); Using "### ####  ###  ###  ###"; values!(I, 6); values!(I, 8); values!(I, 9); values!(I, 7); values!(I, 11)
    Next I

    LPrint "TEAM";: LPrint Tab(39); Using "####"; compareZ!(15)

    LPrint
    LPrint "TOTALS";

    'Points
    If totalGames% > 0 Then ptsCalc! = compareZ!(5) / totalGames% Else ptsCalc! = 0
    LPrint Tab(14); Using "##### ###.#"; compareZ!(10); ptsCalc;

    'Rebounds
    If totalGames% > 0 Then rebCalc! = compareZ!(5) / totalGames% Else rebCalc! = 0
    LPrint Tab(29); Using "#### #### ####  ##.#"; compareZ!(12); compareZ!(5) - compareZ!(12) - compareZ!(15); compareZ!(5); rebCalc!;

    'PF  AST  BLK  STL  TO
    LPrint Using " #### #### #### #### ####"; compareZ!(6); compareZ!(8); compareZ!(9); compareZ!(7); compareZ!(11)

    'Points
    If totalGames% > 0 Then rebCalc! = compareZ1!(10) / totalGames% Else ptsCalc! = 0
    LPrint Tab(14); Using "##### ###.#"; compareZ1!(10); ptsCalc!;

    '-- Should be HI points but is excluded; perhaps because it is not a TOTAL
    '-- however it could be updated to do a MAX of the "HI" values
    'LPrint Tab(18); Using "## "; compareZ1!(13)

    'Rebounds
    If totalGames% > 0 Then rebCalc! = compareZ1!(5) / totalGames% Else rebCalc! = 0
    LPrint Tab(29); Using "#### #### ####  ##.#"; compareZ1!(12); compareZ1!(5) - compareZ1!(12) - compareZ1!(15); compareZ1!(5); rebCalc!;

    'PF  AST  BLK  STL  TO
    LPrint Using " #### #### #### #### ####"; compareZ1!(6); compareZ1!(8); compareZ1!(9); compareZ1!(7); compareZ1!(11)

    LPrint Chr$(12)

End Sub


'----------------------------------------
'       SortStats Subroutine
'----------------------------------------
'This subroutine sorts the associate stats
'based on the chosen category
Sub SortStats (CP%)

    Shared values!(), statW0!(), statW1!()
    Shared lookyA$()

    'SORT BASED ON CATEGORY
    If CP% = 1 Then SG% = 0
    If CP% = 2 Then SG% = 10
    If CP% = 3 Then SG% = 14
    If CP% = 4 Then SG% = 2
    If CP% = 5 Then SG% = 1
    If CP% = 6 Then SG% = 15
    If CP% = 7 Then SG% = 18
    If CP% = 8 Then SG% = 19
    If CP% = 9 Then SG% = 16
    If CP% = 10 Then SG% = 4
    If CP% = 11 Then SG% = 3
    If CP% = 12 Then SG% = 17
    If CP% = 14 Then SG% = 22
    If CP% = 15 Then SG% = 20
    If CP% = 17 Then SG% = 12
    If CP% = 16 Then SG% = 13
    If CP% = 18 Then SG% = 23
    If CP% = 19 Then SG% = 5
    If CP% = 20 Then SG% = 21
    If CP% = 21 Then SG% = 6
    If CP% = 22 Then SG% = 8
    If CP% = 23 Then SG% = 9
    If CP% = 24 Then SG% = 7
    If CP% = 25 Then SG% = 11

    For Z = 0 To 12

        For Z1 = Z + 1 To 14

            If values!(Z, SG%) < values!(Z1, SG%) Then

                For I = 0 To 24: Swap values!(Z, I), values!(Z1, I): Next

                Swap statW0!(Z), statW0!(Z1)
                Swap statW1!(Z), statW1!(Z1)
                Swap lookyA$(Z), lookyA$(Z1)

            End If

        Next Z1

    Next Z

End Sub


'----------------------------------------
'       ResetStatPrompt Subroutine
'----------------------------------------
'This subroutine prompts the user with 
'a choice to reset stats. If they chose, 
'and confirm, it will then proceed to 
'delete the files.
Sub ResetStatPrompt (statFile$)

    Cls

    Call MenuBox (6, 8, 61, 1, 15, 1)

    Do
        Locate 7
        U$ = "RESET COMPILED STATS TO 0? (Y/N)"
        Locate , (80 - Len(U$)) / 2
        Print U$
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Color 15, 0

    If UCase$(I$) = "Y" Then

		Call MenuBox (6, 8, 61, 1, 15, 1)

        Locate 7

        Do
            U$ = "ARE YOU SURE? (Y/N)"
            Locate , (80 - Len(U$)) / 2
            Print U$
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        Color 15, 0

        If UCase$(I$) = "Y" Then
            Kill diskPaths$(1) + statFile$
            Kill diskPaths$(1) + statFile$ + "S"
            If _FileExists(diskPaths$(1) + statFile$ + "R") Then Kill diskPaths$(1) + statFile$ + "R"
        End If

    End If 'Done checking if reset stats

End Sub

