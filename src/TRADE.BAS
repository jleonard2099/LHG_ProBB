'----------------------------------------
'            TRADE Subroutine
'----------------------------------------
'This subroutine relies on several other
'routines to proceed through the general
'steps for trading players.
'First, 2 teams are chosen and their Stats
'are loaded.
'Then, the player choices are made and the
'corresponding team files are updated.
Sub TRADE ()

    _Title "Basketball: The Pro Game - Trade Players"

    Shared tmYears_DRAFT$()

    Shared teamIndexes()

    Shared tmInfo_TRADE$()

    tradeIdx = 0
    Call ChooseTradeTeam(tmYears_DRAFT$(tradeIdx), NTMS, teamNbr, tmInfo_TRADE$(tradeIdx, 0), tradeIdx)

    If teamNbr > 0 Then

        teamIndexes(tradeIdx) = teamNbr

        Call LoadStatsForTrade(tradeIdx)

        tradeIdx = 1
        Call ChooseTradeTeam(tmYears_DRAFT$(tradeIdx), NTMS, teamNbr, tmInfo_TRADE$(tradeIdx, 0), tradeIdx)
        teamIndexes(tradeIdx) = teamNbr
        Call LoadStatsForTrade(tradeIdx)

        'Teams / Stats are loaded
        Call ContinueTrade

    End If

End Sub


'----------------------------------------
'      LoadStatsForTrade Subroutine
'----------------------------------------
'This subroutine prompts the user in reg-
'ards to the trading of stats associated
'with the players. If chosen to trade them,
'the stats are loaded from the team stats.
Sub LoadStatsForTrade (tradeIdx)

    Shared tmRat_TRADE(), plyrRat_TRADE(), leagRat_TRADE()
    Shared tradeStatsOffTot!(), tradeStatsDefTot!()

    Shared tradeStatPlyrVal!(), trade3FGM!(), trade3FGA!()

    Shared tradeStatsPlyrName$(), statTeam$()
    Shared tradeStatsTeamName$(), tmInfo_TRADE$(), players_TRADE$()

    Shared haveStats()

    Color 15, 0
    Cls

    haveStats(tradeIdx) = 0

    Call box0 (6, 8, 8, 70, 2, 15, 1)

    Do
        Locate 7
        U$ = "DOES THIS TEAM HAVE SAVED STATS? (Y/N)"
        Locate , (80 - Len(U$)) / 2
        Print U$
        Color 15, 0
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If UCase$(I$) = "N" Then

        haveStats(tradeIdx) = 1

    Else

        statTeam$(tradeIdx) = tmInfo_TRADE$(tradeIdx, 0)
        statFile$ = nameStatFile$(statTeam$(tradeIdx))

        If _FileExists(diskPaths$(1) + statFile$) Then

            Open diskPaths$(1) + statFile$ For Input As #1

            Input #1, tradeStatsTeamName$(tradeIdx)

            For I = 0 To 14:

                Input #1, tradeStatsPlyrName$(tradeIdx, I)

                For I1 = 0 To 13:
                    Input #1, tradeStatPlyrVal!(tradeIdx, I, I1)
                Next I1

                Input #1, trade3FGM!(tradeIdx, I)
                Input #1, trade3FGA!(tradeIdx, I)

            Next I

            For L = 0 To 15:
                Input #1, tradeStatsOffTot!(tradeIdx, L)
            Next L

            For L = 0 To 15:
                Input #1, tradeStatsDefTot!(tradeIdx, L)
            Next L

            Close #1

        Else

            haveStats(tradeIdx) = 1
            MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
            BoxWidth = 24
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        End If

    End If

End Sub


'----------------------------------------
'       ContinueTrade Subroutine
'----------------------------------------
'This subroutine actually presents the user
'a menu for choosing players to trade. It
'relies on separate routines to summarize
'the trade and then save the changes.
Sub ContinueTrade ()

    Shared tmRat_TRADE(), plyrRat_TRADE(), leagRat_TRADE()
    Shared tradeStatsOffTot!(), tradeStatsDefTot!()

    Shared tradeStatPlyrVal!(), trade3FGM!(), trade3FGA!()
    Shared haveStats()

    Shared tradeStatsPlyrName$()
    Shared tradeStatsTeamName$(), tmInfo_TRADE$(), players_TRADE$()

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
                Color tmRat_TRADE(0, 33), tmRat_TRADE(0, 34): Print tmInfo_TRADE$(0, 0);
                Locate , 20
                Color tmRat_TRADE(1, 33), tmRat_TRADE(1, 34): Print tmInfo_TRADE$(1, 0)

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

                    Color tmRat_TRADE(0, 33), tmRat_TRADE(0, 34): Print players_TRADE$(0, I1, 0)

                    Color 3, 0

                    Do
                        Color 15
                        Locate 21, 1: Print tmInfo_TRADE$(1, 0); " PLAYER: ";
                        Input I2
                    Loop Until I2 >= 0 And I2 <= 14

                    Color tmRat_TRADE(1, 33), tmRat_TRADE(1, 34): Print players_TRADE$(1, I2, 0)
                    Color 15, 0

                    Do
                        Locate LAST_ROW - 2, 1: Print "ANY CHANGE? (Y/N) ";
                        J$ = GetKeyPress$
                    Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

                Loop Until UCase$(J$) = "N"

                Call PrintTradeSummary(I1, I2)

                Color 15
                Locate LAST_ROW - 2, 1: Print "HIT ANY KEY TO TRADE";
                K$ = GetKeyPress$

                For I3 = 0 To 1:
                    Swap players_TRADE$(0, I1, I3), players_TRADE$(1, I2, I3)
                Next

                For I3 = 0 To 19:
                    Swap plyrRat_TRADE(0, I1, I3), plyrRat_TRADE(1, I2, I3)
                Next I3

                Swap tradeStatsPlyrName$(0, I1), tradeStatsPlyrName$(1, I2)

                If haveStats(0) = 1 Or haveStats(1) = 1 Or W1 = 0 Then

                    For I3 = 0 To 13:
                        tradeStatPlyrVal!(0, I1, I3) = 0
                        tradeStatPlyrVal!(1, I2, I3) = 0
                    Next

                    trade3FGM!(1, I2) = 0
                    trade3FGA!(1, I2) = 0
                    trade3FGM!(0, I1) = 0
                    trade3FGA!(0, I1) = 0

                Else

                    For I3 = 0 To 13:
                        Swap tradeStatPlyrVal!(0, I1, I3), tradeStatPlyrVal!(1, I2, I3)
                    Next I3

                    Swap trade3FGM!(0, I1), trade3FGM!(1, I2)
                    Swap trade3FGA!(0, I1), trade3FGA!(1, I2)

                End If

                Call PrintTradeSummary(I1, I2)

                Color 15
                Locate LAST_ROW - 2, 1: Print "HIT ANY KEY TO CONTINUE";
                K$ = GetKeyPress$

            Case 2
                Call SaveTrades

        End Select

    Loop Until UCase$(I$) <> "1" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'         SaveTrades Subroutine
'----------------------------------------
'This subroutine saves the modified team
'data from the trade to the team files.
Sub SaveTrades ()

    Shared tmAtt_TRADE&()
    
    Shared tmRat_TRADE(), plyrRat_TRADE(), leagRat_TRADE()
    Shared tradeStatsOffTot!(), tradeStatsDefTot!()

    Shared tradeStatPlyrVal!(), haveStats(), teamIndexes()
    Shared trade3FGM!(), trade3FGA!()

    Shared tradeStatsPlyrName$(), statTeam$(), tmYears_DRAFT$()
    Shared tradeStatsTeamName$(), tmInfo_TRADE$(), players_TRADE$()

    For J = 0 To 1

        teamYear$ = RTrim$(tmYears_DRAFT$(J))

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
            LSet Q$(I + 10) = MKI$(tmRat_TRADE(J, I))
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
        LSet Q$(378) = MKL$(tmAtt_TRADE&(J))

        Put #1, teamIndexes(J)

        Close #1

    Next J

    For J = 0 To 1

        If haveStats(J) <> 1 Then

            statFile$ = nameStatFile$(statTeam$(tradeIdx))

            Open diskPaths$(1) + statFile$ For Output As #2

            Print #2, tradeStatsTeamName$(J)

            For I = 0 To 14:

                Print #2, tradeStatsPlyrName$(J, I)

                For I1 = 0 To 13:
                    Print #2, tradeStatPlyrVal!(J, I, I1)
                Next I1

                Print #2, trade3FGM!(J, I)
                Print #2, trade3FGA!(J, I)

            Next I

            For L = 0 To 15:
                Print #2, tradeStatsOffTot!(J, L)
            Next L

            For L = 0 To 15:
                Print #2, tradeStatsDefTot!(J, L)
            Next L

            Close #2

        End If

    Next J

End Sub


'----------------------------------------
'      PrintTradeSummary Subroutine
'----------------------------------------
'This subroutine prints a summary of stats 
'and players that will be swapped as a 
'result of the trade being proposed
Sub PrintTradeSummary (p1Idx, p2Idx)

    Shared tradeStatPlyrVal!(), trade3FGM!(), trade3FGA!()
    Shared tmRat_TRADE(), plyrRat_TRADE()
    Shared tmInfo_TRADE$(), players_TRADE$()

    Cls
    Locate 3, 1

    Locate , 10
    Color tmRat_TRADE(0, 33), tmRat_TRADE(0, 34)
    Print tmInfo_TRADE$(0, 0);

    Locate , 50
    Color tmRat_TRADE(1, 33), tmRat_TRADE(1, 34)
    Print tmInfo_TRADE$(1, 0)

    Color 15, 0

    Print
    Locate , 10
    Print players_TRADE$(0, p1Idx, 0); " "; players_TRADE$(0, p1Idx, 1); Tab(50); players_TRADE$(1, p2Idx, 0); " "; players_TRADE$(1, p2Idx, 1)

    Color 7, 0
    Locate , 5: Print Using "  ##   GAMES     ##"; plyrRat_TRADE(0, p1Idx, 0); tradeStatPlyrVal!(0, p1Idx, 0)
    Locate , 5: Print Using "####  MINUTES  ####"; plyrRat_TRADE(0, p1Idx, 1); tradeStatPlyrVal!(0, p1Idx, 10) / 60
    Locate , 5: Print Using "####    FGA    ####"; plyrRat_TRADE(0, p1Idx, 2); tradeStatPlyrVal!(0, p1Idx, 1)
    Locate , 5: Print Using "####    FGM    ####"; plyrRat_TRADE(0, p1Idx, 3); tradeStatPlyrVal!(0, p1Idx, 2)
    Locate , 5: Print Using "####    FTA    ####"; plyrRat_TRADE(0, p1Idx, 4); tradeStatPlyrVal!(0, p1Idx, 3)
    Locate , 5: Print Using "####    FTM    ####"; plyrRat_TRADE(0, p1Idx, 5); tradeStatPlyrVal!(0, p1Idx, 4)
    Locate , 5: Print Using "####  OFF REB  ####"; plyrRat_TRADE(0, p1Idx, 6); tradeStatPlyrVal!(0, p1Idx, 12)
    Locate , 5: Print Using "####  TOT REB  ####"; plyrRat_TRADE(0, p1Idx, 6) + plyrRat_TRADE(0, p1Idx, 7); tradeStatPlyrVal!(0, p1Idx, 5)
    Locate , 5: Print Using "####   FOULS   ####"; plyrRat_TRADE(0, p1Idx, 9); tradeStatPlyrVal!(0, p1Idx, 6)
    Locate , 5: Print Using "####  STEALS   ####"; plyrRat_TRADE(0, p1Idx, 11); tradeStatPlyrVal!(0, p1Idx, 7)
    Locate , 5: Print Using "####  ASSISTS  ####"; plyrRat_TRADE(0, p1Idx, 8); tradeStatPlyrVal!(0, p1Idx, 8)
    Locate , 5: Print Using "####  BLOCKS   ####"; plyrRat_TRADE(0, p1Idx, 12); tradeStatPlyrVal!(0, p1Idx, 9)
    Locate , 5: Print Using "     TURNOVERS ####"; tradeStatPlyrVal!(0, p1Idx, 11)
    Locate , 5: Print Using "####   3FGA    ####"; plyrRat_TRADE(0, p1Idx, 13); trade3FGA!(0, p1Idx)
    Locate , 5: Print Using "####   3FGM    ####"; plyrRat_TRADE(0, p1Idx, 14); trade3FGM!(0, p1Idx)
    Locate , 5: Print Using "  ##  HI GAME    ##"; plyrRat_TRADE(0, p1Idx, 15); tradeStatPlyrVal!(0, p1Idx, 13)

    Locate 6, 45: Print Using "  ##   GAMES     ##"; plyrRat_TRADE(1, p2Idx, 0); tradeStatPlyrVal!(1, p2Idx, 0)
    Locate , 45: Print Using "####  MINUTES  ####"; plyrRat_TRADE(1, p2Idx, 1); tradeStatPlyrVal!(1, p2Idx, 10) / 60
    Locate , 45: Print Using "####    FGA    ####"; plyrRat_TRADE(1, p2Idx, 2); tradeStatPlyrVal!(1, p2Idx, 1)
    Locate , 45: Print Using "####    FGM    ####"; plyrRat_TRADE(1, p2Idx, 3); tradeStatPlyrVal!(1, p2Idx, 2)
    Locate , 45: Print Using "####    FTA    ####"; plyrRat_TRADE(1, p2Idx, 4); tradeStatPlyrVal!(1, p2Idx, 3)
    Locate , 45: Print Using "####    FTM    ####"; plyrRat_TRADE(1, p2Idx, 5); tradeStatPlyrVal!(1, p2Idx, 4)
    Locate , 45: Print Using "####  OFF REB  ####"; plyrRat_TRADE(1, p2Idx, 6); tradeStatPlyrVal!(1, p2Idx, 12)
    Locate , 45: Print Using "####  TOT REB  ####"; plyrRat_TRADE(1, p2Idx, 6) + plyrRat_TRADE(1, p2Idx, 7); tradeStatPlyrVal!(1, p2Idx, 5)
    Locate , 45: Print Using "####   FOULS   ####"; plyrRat_TRADE(1, p2Idx, 9); tradeStatPlyrVal!(1, p2Idx, 6)
    Locate , 45: Print Using "####  STEALS   ####"; plyrRat_TRADE(1, p2Idx, 11); tradeStatPlyrVal!(1, p2Idx, 7)
    Locate , 45: Print Using "####  ASSISTS  ####"; plyrRat_TRADE(1, p2Idx, 8); tradeStatPlyrVal!(1, p2Idx, 8)
    Locate , 45: Print Using "####  BLOCKS   ####"; plyrRat_TRADE(1, p2Idx, 12); tradeStatPlyrVal!(1, p2Idx, 9)
    Locate , 45: Print Using "     TURNOVERS ####"; tradeStatPlyrVal!(1, p2Idx, 11)
    Locate , 45: Print Using "####   3FGA    ####"; plyrRat_TRADE(1, p2Idx, 13); trade3FGA!(1, p2Idx)
    Locate , 45: Print Using "####   3FGM    ####"; plyrRat_TRADE(1, p2Idx, 14); trade3FGM!(1, p2Idx)
    Locate , 45: Print Using "  ##  HI GAME    ##"; plyrRat_TRADE(1, p2Idx, 15); tradeStatPlyrVal!(1, p2Idx, 13)

End Sub


'----------------------------------------
'       ChooseTradeTeam Subroutine
'----------------------------------------
'This subroutine provides an interface for 
'choosing the team to trade players from
Sub ChooseTradeTeam (yearNumber$, numberTeams, chosenIdx, teamName$, tradeIdx)

    Shared tmRat_TRADE()

    Shared previousYear$

    Do

        chosenIdx = 0

        Color 15, 0
        Cls

        Call MenuBox (6, 8, 61, 7, 15, 4)

        Locate 7, 12

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

                Locate 7, 27: Print I$;

                NY$ = ""

                Do Until NY$ = Chr$(13)

                    NY$ = GetKeyPress$

                    If NY$ = Chr$(8) And Len(I$) >= 1 Then
                        col% = Pos(0)
                        I$ = Left$(I$, (Len(I$) - 1))
                        Locate , col% - 1
                        Print " ";
                        Locate , col% - 1
                    Else
                        I$ = I$ + NY$
                        Print NY$;
                    End If

                Loop

                I$ = Left$(I$, (Len(I$) - 1))

                If Not (I$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(I$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        MsgText$ = "NO TEAM FILES COULD BE FOUND MATCHING THIS ID. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
                        BoxWidth = 24
                        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                        Call CSMsgBox(MsgText$, BoxWidth, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)
                        Exit Sub

                    Else

                        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

                        fileLength& = LOF(1)

                        Field #1, 15 As Q$(0)

                        numberTeams = fileLength& / TEAM_SIZE_BYTES

                        For teamIdx = 1 To numberTeams
                            Get #1, teamIdx
                            'numberTeams = numberTeams + 1
                            teamNames$(teamIdx) = Q$(0)
                            teamIndex(teamIdx) = teamIdx
                        Next teamIdx

                        Close #1

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

                previousYear$ = yearNumber$

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectTradeTeam (numberTeams, yearNumber$, chosenIdx, ExitCode%, tradeIdx)

                If ExitCode% <> 27 And numberTeams <> 0 And chosenIdx > 0 Then

                    Do
                        Locate 2, 33
                        Color tmRat_TRADE(tradeIdx, 33), tmRat_TRADE(tradeIdx, 34): Print teamName$

                        Color 15, 4
                        Locate 3, 32: Print "ANY CHANGE? (Y/N)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N" Or chosenIdx = -1

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    'If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'       SelectTradeTeam Subroutine
'----------------------------------------
'This subroutine provides an interface for 
'selecting the team to trade players from
Sub SelectTradeTeam (numberTeams, yearNumber$, selectedIdx, ExitCode%, tradeIdx)

    ExitCode% = -1
    validData = 0

    If numberTeams > 0 And CheckString%(teamNames$(numberTeams)) = 0 Then

        SELECTION% = 1
        Count% = numberTeams
        Start% = 1
        FileFGColor = 15: FileBGColor = 4
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Rows% = LAST_ROW - 6
        Columns% = 4
        Spacing% = 6
        Row% = 5
        Column% = 1

        Color , 4
        Cls

        Color 14
        Locate , 33: Print "TEAM SELECTION"
        Locate LAST_ROW, 1
        Color 14
        Print Tab(3); "PG DN";
        Print Tab(18); "ENTER";
        Color 15: Print "-SELECTS TEAM";
        Color 14
        Print Tab(39); "ESC";
        Color 15: Print "-SELECT NEW ID";
        Color 14
        Print Tab(72); "PG UP";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex(SELECTION%)

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Call ReadTradeTeam(yearNumber$, selectedIdx, tradeIdx, validData)

        End If

    End If

    If validData = 0 And ExitCode% <> 27 Then

        selectedIdx = -1
        Locate 9,
        Call CSMsgBox("THERE WAS AN ISSUE READING TEAM DATA... PRESS ANY KEY AND TRY ANOTHER DISK ID", 20, PASS)
        K$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    End If

End Sub


'----------------------------------------
'        ReadTradeTeam Subroutine
'----------------------------------------
'This subroutine reads team data into 
'arrays for the selected team
Sub ReadTradeTeam (teamYear$, teamIdx, tradeIdx, dataOK)

    Shared tmAtt_TRADE&()

    Shared plyrRat_TRADE(), leagRat_TRADE(), tmRat_TRADE()

    Shared tmInfo_TRADE$(), players_TRADE$()

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    Field #1, 15 As Q$(0), 15 As Q$(1)

    For J = 0 To 6
        Field #1, 30 + J * 2 As X$, 2 As Q$(J + 2)
    Next

    Field #1, 44 As X$, 2 As Q$(9)

    For J = 0 To 34
        Field #1, 46 + J * 2 As X$, 2 As Q$(J + 10)
    Next

    For J = 0 To 14
        Field #1, 116 + J * 57 As X$, 15 As Q$(J * 22 + 45), 2 As Q$(J * 22 + 46)
        For J1 = 0 To 19
            Field #1, 133 + J * 57 + J1 * 2 As X$, 2 As Q$(J * 22 + 47 + J1)
        Next
    Next

    Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377), 4 As Q$(378)

    Get #1, teamIdx

    tmInfo_TRADE$(tradeIdx, 0) = RTrim$(Q$(0))
    tmInfo_TRADE$(tradeIdx, 1) = RTrim$(Q$(1))

    For I = 0 To 6
        leagRat_TRADE(tradeIdx, I) = CVI(Q$(I + 2))
    Next

    For I = 0 To 34
        tmRat_TRADE(tradeIdx, I) = CVI(Q$(I + 10))
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
    tmAtt_TRADE&(tradeIdx) = CVL(Q$(378))
    
    Close #1

    If (CheckString%(tmInfo_TRADE$(draftIdx, 4)) = 1) Then

        dataOK = 0
        
    Else
    
        dataOK = 1
        
    End If

End Sub
