Sub TRADE ()

    _Title "Basketball: The Pro Game - Trade Players"

    ReDim menuChoices$(1 To 2)

    Shared draftYN$()

    Do

        Call TITLE

        menuChoices$(1) = "TRADE PLAYERS           "
        menuChoices$(2) = "RETURN TO PRO GAME MENU "
        Selection% = 1
        Count% = 2
        Start% = 1
        ExitCode% = 2
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 10, 49, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    teamIdx = 0
                    Call GetTeam_TRADE(draftYN$(teamIdx), teamIdx)
                    Call LoadStatsForTrade(teamIdx)
                    teamIdx = 1
                    Call GetTeam_TRADE(draftYN$(teamIdx), teamIdx)
                    Call LoadStatsForTrade(teamIdx)
                    Call TradePlayers

                Case 2:
                    Cls

            End Select

        End If

    Loop Until Selection% = 2 Or ExitCode% = 27

End Sub


'----------------------------------------
'         TradePlayers Subroutine
'----------------------------------------
'... explain here ...
Sub GetTeam_TRADE (draftYear$, tradeTeam)

    Shared teamSettings%()

    Shared tradeAA$()

    Do
        Color 15, 0: Cls
        JJ = 7
        Locate 6, 8: Color 15, 4
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        For II = 1 To JJ: Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32
        Locate , 12
        If tradeTeam = 0 Then
            Print "ENTER DISK ID FOR TEAM 1: "
        Else
            Print "ENTER DISK ID FOR TEAM 2: "
        End If
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 38: Input NY$

        If NY$ = "" Then
            Call SelectTeam_TRADE(tradeTeam, NTMS, draftYear$)
        Else
            draftYear$ = RTrim$(NY$)
            Erase teamNames$
            NTMS = 0

            Open diskPaths$(0) + "PROBBTMS." + draftYear$ For Random As #1 Len = 1031

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

            Call SelectTeam_TRADE(tradeTeam, NTMS, draftYear$)
        End If

        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33
            Color teamSettings%(tradeTeam, 33), teamSettings%(tradeTeam, 34): Print tradeAA$(tradeTeam, 0)

            Do
                Color 15, 4
                Locate 3, 33: Print "ANY CHANGE (YN)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N"

End Sub


'----------------------------------------
'         LoadStatsForTrade Subroutine
'----------------------------------------
'... explain here ...
Sub LoadStatsForTrade (tradeTeam)

    Shared statFiles$()

    Shared teamSettings%(), tradeB%(), tradeL%()
    Shared tradeZZ(), tradeZZ1()

    Shared tradeS(), tradeT!()
    Shared tradeB1!(), tradeW0!(), tradeW1!()

    Shared B1$(), draftYN$(), SA$()
    Shared tradeA1$(), tradeAA$(), tradeB$()

    Color 15, 0

    Cls

    tradeS(tradeTeam) = 0

    JJ = 1

    Locate 6, 8: Color teamSettings%(tradeTeam, 33), teamSettings%(tradeTeam, 34)

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
        tradeS(tradeTeam) = 1
    Else
        For XX = 4 To 11
            If Mid$(SA$(tradeTeam), XX, 1) = " " Or Mid$(SA$(tradeTeam), XX, 1) = "/" Or Mid$(SA$(tradeTeam), XX, 1) = "." Then Mid$(SA$(tradeTeam), XX, 1) = "_"
        Next XX

        For XX = 12 To 14
            If Mid$(SA$(tradeTeam), XX, 2) = "ST" Then Mid$(SA$(tradeTeam), 9, 1) = "_": Mid$(SA$(tradeTeam), 10, 2) = "ST"
        Next XX

        QQ = 8

        For XX = 11 To 4 Step -1

            If Not (Mid$(SA$(tradeTeam), XX, 1) = "_") Then

                YR$ = Left$(SA$(tradeTeam), 2)

                statFiles$(tradeTeam) = Mid$(SA$(tradeTeam), 4, QQ) + "." + YR$

                If _FileExists(diskPaths$(1) + statFiles$(tradeTeam)) Then

                    Open diskPaths$(1) + statFiles$(tradeTeam) For Input As #1

                    Input #1, tradeA1$(tradeTeam)

                    For I = 0 To 14:
                        Input #1, B1$(tradeTeam, I)
                        For I1 = 0 To 13: Input #1, tradeB1!(tradeTeam, I, I1): Next I1
                        Input #1, tradeW0!(tradeTeam, I)
                        Input #1, tradeW1!(tradeTeam, I)
                    Next I

                    For L = 0 To 15: Input #1, tradeZZ(tradeTeam, L): Next L
                    For L = 0 To 15: Input #1, tradeZZ1(tradeTeam, L): Next L

                    Close 1

                Else
                    tradeS(tradeTeam) = 1
                End If

            Else
                QQ = XX - 4
            End If

        Next XX
    End If


End Sub


'----------------------------------------
'         TradePlayers Subroutine
'----------------------------------------
'... explain here ...
Sub TradePlayers ()

    Shared teamSettings%(), tradeB%(), tradeL%()
    Shared tradeZZ(), tradeZZ1()

    Shared tradeB1!(), tradeW0!(), tradeW1!()
    Shared tradeS(), tradeT!()

    Shared B1$(), draftYN$(), SA$()
    Shared tradeA1$(), tradeAA$(), tradeB$()

    Do

        Call TITLE

        For XY = 2 To 23:
            Locate XY, 1: Print Tab(79);
        Next

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

        Do
            I$ = GetKeyPress$
        Loop Until Val(I$) > 0 And Val(I$) <= 3

        Select Case I$

            Case "1"

                If tradeS(0) <> 1 Or tradeS(1) <> 1 Then

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
                Color teamSettings%(0, 33), teamSettings%(0, 34): Print tradeAA$(0, 0);
                Locate , 20
                Color teamSettings%(1, 33), teamSettings%(1, 34): Print tradeAA$(1, 0)

                For I = 0 To 14

                    Color 3, 0
                    Locate I + 4, 1
                    Print I; " ";
                    Color 7: Print tradeB$(0, I, 0); " "; tradeB$(0, I, 1);

                    Locate , 20

                    Color 3
                    Print I; " ";
                    Color 7: Print tradeB$(1, I, 0); " "; tradeB$(1, I, 1)

                Next I

                Do

                    Do
                        Color 15
                        Locate 19, 1: Print tradeAA$(0, 0); " PLAYER: ";
                        Input I1
                    Loop Until I1 >= 0 And I1 <= 14

                    Color teamSettings%(0, 33), teamSettings%(0, 34): Print tradeB$(0, I1, 0)

                    Color 3, 0

                    Do
                        Color 15
                        Locate 21, 1: Print tradeAA$(1, 0); " PLAYER: ";
                        Input I2
                    Loop Until I2 >= 0 And I2 <= 14

                    Color teamSettings%(1, 33), teamSettings%(1, 34): Print tradeB$(1, I2, 0)
                    Color 15, 0

                    Do
                        Locate 23, 1: Print "ANY CHANGE (YN) ";
                        J$ = GetKeyPress$
                    Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

                Loop Until UCase$(J$) = "N"

                Call PrintTradeSummary(I1, I2)

                Locate 23, 1: Print "HIT ANY KEY TO TRADE";
                K$ = GetKeyPress$

                For I3 = 0 To 1:
                    Swap tradeB$(0, I1, I3), tradeB$(1, I2, I3)
                Next

                For I3 = 0 To 19:
                    Swap tradeB%(0, I1, I3), tradeB%(1, I2, I3)
                Next I3

                Swap B1$(0, I1), B1$(1, I2)

                If tradeS(0) = 1 Or tradeS(1) = 1 Or W1 = 0 Then

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

                Locate 23, 1: Print "HIT ANY KEY TO CONTINUE";
                K$ = GetKeyPress$

            Case "2"
                Call SaveTrades

            Case "3"
                Cls

        End Select

    Loop Until UCase$(I$) <> "1" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'           SaveTrades Subroutine
'----------------------------------------
'... explain here ...
Sub SaveTrades ()

    Shared teamSettings%(), tradeB%(), tradeL%()
    Shared tradeZZ(), tradeZZ1()

    Shared tradeB1!(), tradeS(), tradeT!()
    Shared tradeW0!(), tradeW1!()

    Shared B1$(), draftYN$(), statFiles$()
    Shared tradeA1$(), tradeAA$(), tradeB$()


    For J = 0 To 1

        teamYear$ = RTrim$(draftYN$(J))
        Open diskPaths$(0) + "PROBBTMS." + teamYear$ For Random As #1 Len = 1031

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

        LSet Q$(0) = tradeAA$(J, 0)
        LSet Q$(1) = tradeAA$(J, 1)

        For I = 0 To 6:
            LSet Q$(I + 2) = MKI$(tradeL%(J, I))
        Next

        LSet Q$(9) = MKI$(999)

        For I = 0 To 34:
            LSet Q$(I + 10) = MKI$(teamSettings%(J, I))
        Next

        For I = 0 To 14

            LSet Q$(22 * I + 45) = tradeB$(J, I, 0)
            LSet Q$(22 * I + 46) = tradeB$(J, I, 1)

            For I1 = 0 To 19
                LSet Q$(22 * I + 47 + I1) = MKI$(tradeB%(J, I, I1))
            Next

        Next

        LSet Q$(375) = tradeAA$(J, 2)
        LSet Q$(376) = tradeAA$(J, 3)
        LSet Q$(377) = tradeAA$(J, 4)

        Put #1, tradeT!(J)

        Close 1

    Next J

    For J = 0 To 1

        If tradeS(J) <> 1 Then

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

            For L = 0 To 15: Print #1, tradeZZ(J, L): Next L
            For L = 0 To 15: Print #1, tradeZZ1(J, L): Next L

            Close 1

        End If

    Next J

End Sub


'----------------------------------------
'     SelectTeam_TRADE Subroutine
'----------------------------------------
'... explain here ...
Sub SelectTeam_TRADE (tradeTeam, numberTeams, yearNumber$)

    Shared tradeT!()
    Shared SA$(), tradeAA$(), tradeB$()
    Shared teamSettings%()
    Shared tradeB%(), tradeL%()

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

        tradeT!(tradeTeam) = teamIndex%(Selection%)
        teamYear$ = RTrim$(yearNumber$)

        Open diskPaths$(0) + "PROBBTMS." + teamYear$ For Random As #1 Len = 1031

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

        Get #1, tradeT!(tradeTeam)

        tradeAA$(tradeTeam, 0) = RTrim$(Q$(0))
        tradeAA$(tradeTeam, 1) = Q$(1)

        For I = 0 To 6
            tradeL%(tradeTeam, I) = CVI(Q$(I + 2))
        Next

        For I = 0 To 34
            teamSettings%(tradeTeam, I) = CVI(Q$(I + 10))
        Next

        For I = 0 To 14
            tradeB$(tradeTeam, I, 0) = RTrim$(Q$(22 * I + 45))
            tradeB$(tradeTeam, I, 1) = RTrim$(Q$(22 * I + 46))

            For I1 = 0 To 19
                tradeB%(tradeTeam, I, I1) = CVI(Q$(22 * I + 47 + I1))
            Next
        Next

        tradeAA$(tradeTeam, 2) = RTrim$(Q$(375))
        tradeAA$(tradeTeam, 3) = RTrim$(Q$(376))
        tradeAA$(tradeTeam, 4) = RTrim$(Q$(377))

        Close 1

        SA$(tradeTeam) = tradeAA$(tradeTeam, 0)

    Else
        BO% = 1
    End If

End Sub

'----------------------------------------
'      PrintTradeSummary Subroutine
'----------------------------------------
'Prints a summary of stats and players
'that will be swapped as a result of
'the trade that is being proposed
Sub PrintTradeSummary (p1Idx, p2Idx)

    Shared tradeB1!(), tradeW0!(), tradeW1!()
    Shared teamSettings%(), tradeB%()
    Shared tradeAA$(), tradeB$()

    Cls
    Locate 3, 1: Color teamSettings%(0, 33), teamSettings%(0, 34)
    Locate , 10: Print tradeAA$(0, 0);
    Locate , 50: Color teamSettings%(1, 33), teamSettings%(1, 34): Print tradeAA$(1, 0)

    Color 15, 0
    Print
    Locate , 10: Print tradeB$(0, p1Idx, 0); " "; tradeB$(0, p1Idx, 1); Tab(50); tradeB$(1, p2Idx, 0); " "; tradeB$(1, p2Idx, 1)

    Color 7, 0
    Locate , 5: Print Using "  ##   GAMES     ##"; tradeB%(0, p1Idx, 0); tradeB1!(0, p1Idx, 0)
    Locate , 5: Print Using "####  MINUTES  ####"; tradeB%(0, p1Idx, 1); tradeB1!(0, p1Idx, 10) / 60
    Locate , 5: Print Using "####    FGA    ####"; tradeB%(0, p1Idx, 2); tradeB1!(0, p1Idx, 1)
    Locate , 5: Print Using "####    FGM    ####"; tradeB%(0, p1Idx, 3); tradeB1!(0, p1Idx, 2)
    Locate , 5: Print Using "####    FTA    ####"; tradeB%(0, p1Idx, 4); tradeB1!(0, p1Idx, 3)
    Locate , 5: Print Using "####    FTM    ####"; tradeB%(0, p1Idx, 5); tradeB1!(0, p1Idx, 4)
    Locate , 5: Print Using "####  OFF REB  ####"; tradeB%(0, p1Idx, 6); tradeB1!(0, p1Idx, 12)
    Locate , 5: Print Using "####  TOT REB  ####"; tradeB%(0, p1Idx, 6) + tradeB%(0, p1Idx, 7); tradeB1!(0, p1Idx, 5)
    Locate , 5: Print Using "####   FOULS   ####"; tradeB%(0, p1Idx, 9); tradeB1!(0, p1Idx, 6)
    Locate , 5: Print Using "####  STEALS   ####"; tradeB%(0, p1Idx, 11); tradeB1!(0, p1Idx, 7)
    Locate , 5: Print Using "####  ASSISTS  ####"; tradeB%(0, p1Idx, 8); tradeB1!(0, p1Idx, 8)
    Locate , 5: Print Using "####  BLOCKS   ####"; tradeB%(0, p1Idx, 12); tradeB1!(0, p1Idx, 9)
    Locate , 5: Print Using "     TURNOVERS ####"; tradeB1!(0, p1Idx, 11)
    Locate , 5: Print Using "####   3FGA    ####"; tradeB%(0, p1Idx, 13); tradeW1!(0, p1Idx)
    Locate , 5: Print Using "####   3FGM    ####"; tradeB%(0, p1Idx, 14); tradeW0!(0, p1Idx)
    Locate , 5: Print Using "  ##  HI GAME    ##"; tradeB%(0, p1Idx, 15); tradeB1!(0, p1Idx, 13)

    Locate 6, 45: Print Using "  ##   GAMES     ##"; tradeB%(1, p2Idx, 0); tradeB1!(1, p2Idx, 0)
    Locate , 45: Print Using "####  MINUTES  ####"; tradeB%(1, p2Idx, 1); tradeB1!(1, p2Idx, 10) / 60
    Locate , 45: Print Using "####    FGA    ####"; tradeB%(1, p2Idx, 2); tradeB1!(1, p2Idx, 1)
    Locate , 45: Print Using "####    FGM    ####"; tradeB%(1, p2Idx, 3); tradeB1!(1, p2Idx, 2)
    Locate , 45: Print Using "####    FTA    ####"; tradeB%(1, p2Idx, 4); tradeB1!(1, p2Idx, 3)
    Locate , 45: Print Using "####    FTM    ####"; tradeB%(1, p2Idx, 5); tradeB1!(1, p2Idx, 4)
    Locate , 45: Print Using "####  OFF REB  ####"; tradeB%(1, p2Idx, 6); tradeB1!(1, p2Idx, 12)
    Locate , 45: Print Using "####  TOT REB  ####"; tradeB%(1, p2Idx, 6) + tradeB%(1, p2Idx, 7); tradeB1!(1, p2Idx, 5)
    Locate , 45: Print Using "####   FOULS   ####"; tradeB%(1, p2Idx, 9); tradeB1!(1, p2Idx, 6)
    Locate , 45: Print Using "####  STEALS   ####"; tradeB%(1, p2Idx, 11); tradeB1!(1, p2Idx, 7)
    Locate , 45: Print Using "####  ASSISTS  ####"; tradeB%(1, p2Idx, 8); tradeB1!(1, p2Idx, 8)
    Locate , 45: Print Using "####  BLOCKS   ####"; tradeB%(1, p2Idx, 12); tradeB1!(1, p2Idx, 9)
    Locate , 45: Print Using "     TURNOVERS ####"; tradeB1!(1, p2Idx, 11)
    Locate , 45: Print Using "####   3FGA    ####"; tradeB%(1, p2Idx, 13); tradeW1!(1, p2Idx)
    Locate , 45: Print Using "####   3FGM    ####"; tradeB%(1, p2Idx, 14); tradeW0!(1, p2Idx)
    Locate , 45: Print Using "  ##  HI GAME    ##"; tradeB%(1, p2Idx, 15); tradeB1!(1, p2Idx, 13)

End Sub

