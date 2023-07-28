Sub TRADE ()

    _Title "Basketball: The Pro Game - Trade Players"

    ReDim menuChoices$(1 To 2)

    Shared tradeT!()

    Shared teamYears$(), tmInfo_TRADE$()

    tradeIdx = 0
    Call ChooseTradeTeam(teamYears$(tradeIdx), NTMS, teamNbr, tmInfo_TRADE$(tradeIdx, 0), tradeIdx)
    tradeT!(tradeIdx) = teamNbr
    Call LoadStatsForTrade(tradeIdx)

    tradeIdx = 1
    Call ChooseTradeTeam(teamYears$(tradeIdx), NTMS, teamNbr, tmInfo_TRADE$(tradeIdx, 0), tradeIdx)
    tradeT!(tradeIdx) = teamNbr
    Call LoadStatsForTrade(tradeIdx)

    'Teams / Stats are loaded
    Call ContinueTrade

End Sub


'----------------------------------------
'         LoadStatsForTrade Subroutine
'----------------------------------------
'... explain here ...
Sub LoadStatsForTrade (tradeTeam)

    Shared statFiles$()

    Shared teamSettings%(), plyrRat_TRADE(), leagRat_TRADE()
    Shared tradeZZ(), tradeZZ1()

    Shared haveStats(), tradeT!()
    Shared tradeB1!(), tradeW0!(), tradeW1!()

    Shared B1$(), teamYears$(), statTeam$()
    Shared tradeA1$(), tmInfo_TRADE$(), players_TRADE$()

    Color 15, 0
    Cls

    haveStats(tradeTeam) = 0

    JJ = 1

    Locate 6, 8
    Color teamSettings%(tradeTeam, 33), teamSettings%(tradeTeam, 34)

    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

    For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    Locate 7 + JJ, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";

    Do
        Locate 7
        U$ = "DOES THIS TEAM HAVE SAVED STATS (YN)"
        Locate , (80 - Len(U$)) / 2
        Print U$
        Color 15, 0
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If UCase$(I$) = "N" Then

        haveStats(tradeTeam) = 1

    Else

        statFiles$(tradeTeam) = nameStatFile$(statTeam$(tradeTeam))

        If _FileExists(diskPaths$(1) + statFiles$(tradeTeam)) Then

            Open diskPaths$(1) + statFiles$(tradeTeam) For Input As #1

            Input #1, tradeA1$(tradeTeam)

            For I = 0 To 14:

                Input #1, B1$(tradeTeam, I)

                For I1 = 0 To 13:
                    Input #1, tradeB1!(tradeTeam, I, I1)
                Next I1

                Input #1, tradeW0!(tradeTeam, I)
                Input #1, tradeW1!(tradeTeam, I)

            Next I

            For L = 0 To 15:
                Input #1, tradeZZ(tradeTeam, L)
            Next L

            For L = 0 To 15:
                Input #1, tradeZZ1(tradeTeam, L)
            Next L

            Close 1

        Else
            haveStats(tradeTeam) = 1
            Locate 10, 8
            Call CSMsgBox("NO COMPILED STATS FOR TEAM WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        End If

    End If

End Sub


'----------------------------------------
'         ContinueTrade Subroutine
'----------------------------------------
'... explain here ...
Sub ContinueTrade ()

    Shared teamSettings%(), plyrRat_TRADE(), leagRat_TRADE()
    Shared tradeZZ(), tradeZZ1()

    Shared tradeB1!(), tradeW0!(), tradeW1!()
    Shared haveStats(), tradeT!()

    Shared B1$(), teamYears$()
    Shared tradeA1$(), tmInfo_TRADE$(), players_TRADE$()

    Do

        Call TITLE

        For XY = 2 To 23:
            Locate XY, 1: Print Tab(79);
        Next

        Do
            Locate 3

            Color 12: Print Tab(35); "TRADE MENU"
            Print
            Locate , 5
            Color 15: Print "(1) ";
            Color 3: Print "TRADE PLAYERS"
            Print
            Locate , 5
            Color 15: Print "(2) ";
            Color 3: Print "END AND SAVE TRADES"
            Print
            Locate , 5
            Color 15: Print "(3) ";
            Color 3: Print "END WITHOUT SAVING TRADES"

            I$ = GetKeyPress$

        Loop Until Val(I$) > 0 And Val(I$) <= 3

        Select Case Val(I$)

            Case 1

                If haveStats(0) <> 1 Or haveStats(1) <> 1 Then

                    Do
                        Call TITLE

                        For XY = 2 To 23:
                            Locate XY, 1: Print Tab(79);
                        Next

                        Locate 5, 5
                        Color 15: Print "T";: Color 3: Print "RADE PLAYERS' REPLAY STATS"
                        Locate , 5: Color 15: Print "Z";: Color 3: Print "ERO PLAYERS' STATS"

                        J$ = GetKeyPress$

                    Loop Until UCase$(I$) = "T" Or UCase$(I$) = "Z"

                    If UCase$(J$) = "T" Then W1 = 1
                    If UCase$(J$) = "Z" Then W1 = 0

                End If

                Cls
                Locate 3, 1
                Color teamSettings%(0, 33), teamSettings%(0, 34): Print tmInfo_TRADE$(0, 0);
                Locate , 20
                Color teamSettings%(1, 33), teamSettings%(1, 34): Print tmInfo_TRADE$(1, 0)

                For I = 0 To 14

                    Color 3, 0
                    Locate I + 4, 1
                    Print I; " ";
                    Color 7: Print players_TRADE$(0, I, 0); " "; players_TRADE$(0, I, 1);

                    Locate , 20

                    Color 3
                    Print I; " ";
                    Color 7: Print players_TRADE$(1, I, 0); " "; players_TRADE$(1, I, 1)

                Next I

                Do

                    Do
                        Color 15
                        Locate 19, 1: Print tmInfo_TRADE$(0, 0); " PLAYER: ";
                        Input I1
                    Loop Until I1 >= 0 And I1 <= 14

                    Color teamSettings%(0, 33), teamSettings%(0, 34): Print players_TRADE$(0, I1, 0)

                    Color 3, 0

                    Do
                        Color 15
                        Locate 21, 1: Print tmInfo_TRADE$(1, 0); " PLAYER: ";
                        Input I2
                    Loop Until I2 >= 0 And I2 <= 14

                    Color teamSettings%(1, 33), teamSettings%(1, 34): Print players_TRADE$(1, I2, 0)
                    Color 15, 0

                    Do
                        Locate 23, 1: Print "ANY CHANGE (YN) ";
                        J$ = GetKeyPress$
                    Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

                Loop Until UCase$(J$) = "N"

                Call PrintTradeSummary(I1, I2)

                Color 15
                Locate 23, 1: Print "HIT ANY KEY TO TRADE";
                K$ = GetKeyPress$

                For I3 = 0 To 1:
                    Swap players_TRADE$(0, I1, I3), players_TRADE$(1, I2, I3)
                Next

                For I3 = 0 To 19:
                    Swap plyrRat_TRADE(0, I1, I3), plyrRat_TRADE(1, I2, I3)
                Next I3

                Swap B1$(0, I1), B1$(1, I2)

                If haveStats(0) = 1 Or haveStats(1) = 1 Or W1 = 0 Then

                    For I3 = 0 To 13:
                        tradeB1!(0, I1, I3) = 0
                        tradeB1!(1, I2, I3) = 0
                    Next

                    tradeW0!(1, I2) = 0
                    tradeW1!(1, I2) = 0
                    tradeW0!(0, I1) = 0
                    tradeW1!(0, I1) = 0

                Else

                    For I3 = 0 To 13:
                        Swap tradeB1!(0, I1, I3), tradeB1!(1, I2, I3)
                    Next I3

                    Swap tradeW0!(0, I1), tradeW0!(1, I2)
                    Swap tradeW1!(0, I1), tradeW1!(1, I2)

                End If

                Call PrintTradeSummary(I1, I2)

                Color 15
                Locate 23, 1: Print "HIT ANY KEY TO CONTINUE";
                K$ = GetKeyPress$

            Case 2
                Call SaveTrades

        End Select

    Loop Until UCase$(I$) <> "1" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'           SaveTrades Subroutine
'----------------------------------------
'... explain here ...
Sub SaveTrades ()

    Shared teamSettings%(), plyrRat_TRADE(), leagRat_TRADE()
    Shared tradeZZ(), tradeZZ1()

    Shared tradeB1!(), haveStats(), tradeT!()
    Shared tradeW0!(), tradeW1!()

    Shared B1$(), teamYears$(), statFiles$()
    Shared tradeA1$(), tmInfo_TRADE$(), players_TRADE$()

    For J = 0 To 1

        teamYear$ = RTrim$(teamYears$(J))

        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

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

        LSet Q$(0) = tmInfo_TRADE$(J, 0)
        LSet Q$(1) = tmInfo_TRADE$(J, 1)

        For I = 0 To 6:
            LSet Q$(I + 2) = MKI$(leagRat_TRADE(J, I))
        Next

        LSet Q$(9) = MKI$(999)

        For I = 0 To 34:
            LSet Q$(I + 10) = MKI$(teamSettings%(J, I))
        Next

        For I = 0 To 14

            LSet Q$(22 * I + 45) = players_TRADE$(J, I, 0)
            LSet Q$(22 * I + 46) = players_TRADE$(J, I, 1)

            For I1 = 0 To 19
                LSet Q$(22 * I + 47 + I1) = MKI$(plyrRat_TRADE(J, I, I1))
            Next

        Next

        LSet Q$(375) = tmInfo_TRADE$(J, 2)
        LSet Q$(376) = tmInfo_TRADE$(J, 3)
        LSet Q$(377) = tmInfo_TRADE$(J, 4)

        Put #1, tradeT!(J)

        Close 1

    Next J

    For J = 0 To 1

        If haveStats(J) <> 1 Then

            Open diskPaths$(1) + RTrim$(statFiles$(J)) For Output As #1

            Print #1, tradeA1$(J)

            For I = 0 To 14:

                Print #1, B1$(J, I)

                For I1 = 0 To 13:
                    Print #1, tradeB1!(J, I, I1)
                Next I1

                Print #1, tradeW0!(J, I)
                Print #1, tradeW1!(J, I)

            Next I

            For L = 0 To 15:
                Print #1, tradeZZ(J, L)
            Next L

            For L = 0 To 15:
                Print #1, tradeZZ1(J, L)
            Next L

            Close 1

        End If

    Next J

End Sub


'----------------------------------------
'      PrintTradeSummary Subroutine
'----------------------------------------
'Prints a summary of stats and players
'that will be swapped as a result of
'the trade that is being proposed
Sub PrintTradeSummary (p1Idx, p2Idx)

    Shared tradeB1!(), tradeW0!(), tradeW1!()
    Shared teamSettings%(), plyrRat_TRADE()
    Shared tmInfo_TRADE$(), players_TRADE$()

    Cls
    Locate 3, 1

    Locate , 10
    Color teamSettings%(0, 33), teamSettings%(0, 34)
    Print tmInfo_TRADE$(0, 0);

    Locate , 50
    Color teamSettings%(1, 33), teamSettings%(1, 34)
    Print tmInfo_TRADE$(1, 0)

    Color 15, 0

    Print
    Locate , 10
    Print players_TRADE$(0, p1Idx, 0); " "; players_TRADE$(0, p1Idx, 1); Tab(50); players_TRADE$(1, p2Idx, 0); " "; players_TRADE$(1, p2Idx, 1)

    Color 7, 0
    Locate , 5: Print Using "  ##   GAMES     ##"; plyrRat_TRADE(0, p1Idx, 0); tradeB1!(0, p1Idx, 0)
    Locate , 5: Print Using "####  MINUTES  ####"; plyrRat_TRADE(0, p1Idx, 1); tradeB1!(0, p1Idx, 10) / 60
    Locate , 5: Print Using "####    FGA    ####"; plyrRat_TRADE(0, p1Idx, 2); tradeB1!(0, p1Idx, 1)
    Locate , 5: Print Using "####    FGM    ####"; plyrRat_TRADE(0, p1Idx, 3); tradeB1!(0, p1Idx, 2)
    Locate , 5: Print Using "####    FTA    ####"; plyrRat_TRADE(0, p1Idx, 4); tradeB1!(0, p1Idx, 3)
    Locate , 5: Print Using "####    FTM    ####"; plyrRat_TRADE(0, p1Idx, 5); tradeB1!(0, p1Idx, 4)
    Locate , 5: Print Using "####  OFF REB  ####"; plyrRat_TRADE(0, p1Idx, 6); tradeB1!(0, p1Idx, 12)
    Locate , 5: Print Using "####  TOT REB  ####"; plyrRat_TRADE(0, p1Idx, 6) + plyrRat_TRADE(0, p1Idx, 7); tradeB1!(0, p1Idx, 5)
    Locate , 5: Print Using "####   FOULS   ####"; plyrRat_TRADE(0, p1Idx, 9); tradeB1!(0, p1Idx, 6)
    Locate , 5: Print Using "####  STEALS   ####"; plyrRat_TRADE(0, p1Idx, 11); tradeB1!(0, p1Idx, 7)
    Locate , 5: Print Using "####  ASSISTS  ####"; plyrRat_TRADE(0, p1Idx, 8); tradeB1!(0, p1Idx, 8)
    Locate , 5: Print Using "####  BLOCKS   ####"; plyrRat_TRADE(0, p1Idx, 12); tradeB1!(0, p1Idx, 9)
    Locate , 5: Print Using "     TURNOVERS ####"; tradeB1!(0, p1Idx, 11)
    Locate , 5: Print Using "####   3FGA    ####"; plyrRat_TRADE(0, p1Idx, 13); tradeW1!(0, p1Idx)
    Locate , 5: Print Using "####   3FGM    ####"; plyrRat_TRADE(0, p1Idx, 14); tradeW0!(0, p1Idx)
    Locate , 5: Print Using "  ##  HI GAME    ##"; plyrRat_TRADE(0, p1Idx, 15); tradeB1!(0, p1Idx, 13)

    Locate 6, 45: Print Using "  ##   GAMES     ##"; plyrRat_TRADE(1, p2Idx, 0); tradeB1!(1, p2Idx, 0)
    Locate , 45: Print Using "####  MINUTES  ####"; plyrRat_TRADE(1, p2Idx, 1); tradeB1!(1, p2Idx, 10) / 60
    Locate , 45: Print Using "####    FGA    ####"; plyrRat_TRADE(1, p2Idx, 2); tradeB1!(1, p2Idx, 1)
    Locate , 45: Print Using "####    FGM    ####"; plyrRat_TRADE(1, p2Idx, 3); tradeB1!(1, p2Idx, 2)
    Locate , 45: Print Using "####    FTA    ####"; plyrRat_TRADE(1, p2Idx, 4); tradeB1!(1, p2Idx, 3)
    Locate , 45: Print Using "####    FTM    ####"; plyrRat_TRADE(1, p2Idx, 5); tradeB1!(1, p2Idx, 4)
    Locate , 45: Print Using "####  OFF REB  ####"; plyrRat_TRADE(1, p2Idx, 6); tradeB1!(1, p2Idx, 12)
    Locate , 45: Print Using "####  TOT REB  ####"; plyrRat_TRADE(1, p2Idx, 6) + plyrRat_TRADE(1, p2Idx, 7); tradeB1!(1, p2Idx, 5)
    Locate , 45: Print Using "####   FOULS   ####"; plyrRat_TRADE(1, p2Idx, 9); tradeB1!(1, p2Idx, 6)
    Locate , 45: Print Using "####  STEALS   ####"; plyrRat_TRADE(1, p2Idx, 11); tradeB1!(1, p2Idx, 7)
    Locate , 45: Print Using "####  ASSISTS  ####"; plyrRat_TRADE(1, p2Idx, 8); tradeB1!(1, p2Idx, 8)
    Locate , 45: Print Using "####  BLOCKS   ####"; plyrRat_TRADE(1, p2Idx, 12); tradeB1!(1, p2Idx, 9)
    Locate , 45: Print Using "     TURNOVERS ####"; tradeB1!(1, p2Idx, 11)
    Locate , 45: Print Using "####   3FGA    ####"; plyrRat_TRADE(1, p2Idx, 13); tradeW1!(1, p2Idx)
    Locate , 45: Print Using "####   3FGM    ####"; plyrRat_TRADE(1, p2Idx, 14); tradeW0!(1, p2Idx)
    Locate , 45: Print Using "  ##  HI GAME    ##"; plyrRat_TRADE(1, p2Idx, 15); tradeB1!(1, p2Idx, 13)

End Sub


'----------------------------------------
'         ChooseTradeTeam Subroutine
'----------------------------------------
'... explain here ...
Sub ChooseTradeTeam (yearNumber$, numberTeams, chosenIdx, teamName$, tradeIdx)

    Shared teamSettings%()

    Shared previousYear$

    Do

        Color 15, 0
        Cls

        JJ = 7

        Locate 6, 8
        Color 15, 4

        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        For II = 1 To JJ: Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32
        Locate , 12

        If tradeIdx = 0 Then
            Print "ENTER DISK ID FOR TEAM 1: "
        Else
            Print "ENTER DISK ID FOR TEAM 2: "
        End If

        Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ <> Chr$(13) Then

                Locate 7, 38: Print I$
                Locate 7, 39: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If Not (NY$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(NY$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        Locate 10, 8

                        Call CSMsgBox("NO TEAMS WERE FOUND MATCHING THAT ID... PRESS ANY KEY TO SELECT ANOTHER ID", 20, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)

                    Else

                        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

                        fileLength& = LOF(1)

                        Field #1, 15 As Q$(0)

                        numberTeams = fileLength& / TEAM_SIZE_BYTES

                        For teamIdx = 1 To numberTeams
                            Get #1, teamIdx
                            'numberTeams = numberTeams + 1
                            teamNames$(teamIdx) = Q$(0)
                            teamIndex%(teamIdx) = teamIdx
                        Next teamIdx

                        Close 1

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

                previousYear$ = yearNumber$

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectTradeTeam(numberTeams, yearNumber$, chosenIdx, teamName$, ExitCode%, tradeIdx)

                If ExitCode% <> 27 Then

                    Do
                        Locate 2, 33
                        Color teamSettings%(tradeIdx, 33), teamSettings%(tradeIdx, 34): Print teamName$

                        Color 15, 4
                        Locate 3, 33: Print "ANY CHANGE? (YN)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N"

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'     SelectTradeTeam Subroutine
'----------------------------------------
'... explain here ...
Sub SelectTradeTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, tradeIdx)

    If numberTeams > 0 Then

        SELECTION% = 1
        Count% = numberTeams
        Start% = 1
        FileFGColr% = 15: FileBGColr% = 4
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Rows% = 20
        Columns% = 4
        Spacing% = 6
        Row% = 4
        Column% = 1

        Color , 4
        Cls
        Color 14
        Locate , 33: Print "TEAM SELECTION"
        Locate 25, 1
        Color 14: Print "ESC";: Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";
        Color 14: Print "ENTER";: Color 15: Print "-SELECTS TEAM";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex%(SELECTION%)

            Locate 2, 33: Color 3
            Print "LOADING.."

            Call ReadTradeTeam(yearNumber$, teamName$, selectedIdx, tradeIdx)

        End If

    End If

End Sub


'------------------------------
'   ReadTradeTeam Subroutine
'------------------------------
'...explanation...
Sub ReadTradeTeam (teamYear$, targetTeam$, teamIdx, tradeIdx)

    Shared teamSettings%()
    Shared plyrRat_TRADE(), leagRat_TRADE()

    Shared tradeT!()

    Shared statTeam$(), tmInfo_TRADE$(), players_TRADE$()

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    Field #1, 15 As Q$(0), 15 As Q$(1)

    For J0 = 0 To 6
        Field #1, 30 + J0 * 2 As X$, 2 As Q$(J0 + 2)
    Next

    Field #1, 44 As X$, 2 As Q$(9)

    For J0 = 0 To 34
        Field #1, 46 + J0 * 2 As X$, 2 As Q$(J0 + 10)
    Next

    For J0 = 0 To 14
        Field #1, 116 + J0 * 57 As X$, 15 As Q$(J0 * 22 + 45), 2 As Q$(J0 * 22 + 46)
        For J1 = 0 To 19
            Field #1, 133 + J0 * 57 + J1 * 2 As X$, 2 As Q$(J0 * 22 + 47 + J1)
        Next
    Next

    Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)

    Get #1, teamIdx

    tmInfo_TRADE$(tradeIdx, 0) = RTrim$(Q$(0))
    statTeam$(tradeIdx) = tmInfo_TRADE$(tradeIdx, 0)

    tmInfo_TRADE$(tradeIdx, 1) = RTrim$(Q$(1))

    For I = 0 To 6
        leagRat_TRADE(tradeIdx, I) = CVI(Q$(I + 2))
    Next

    For I = 0 To 34
        teamSettings%(tradeIdx, I) = CVI(Q$(I + 10))
    Next

    For I = 0 To 14
        players_TRADE$(tradeIdx, I, 0) = RTrim$(Q$(22 * I + 45))
        players_TRADE$(tradeIdx, I, 1) = RTrim$(Q$(22 * I + 46))

        For I1 = 0 To 19
            plyrRat_TRADE(tradeIdx, I, I1) = CVI(Q$(22 * I + 47 + I1))
        Next
    Next

    tmInfo_TRADE$(tradeIdx, 2) = RTrim$(Q$(375))
    tmInfo_TRADE$(tradeIdx, 3) = RTrim$(Q$(376))
    tmInfo_TRADE$(tradeIdx, 4) = RTrim$(Q$(377))

    Close 1

End Sub

