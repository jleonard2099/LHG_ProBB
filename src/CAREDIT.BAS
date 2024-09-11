'----------------------------------------
'           CAREDIT Subroutine
'----------------------------------------
'This routine allows the user to choose a
'career file, then loads in the stats and
'gives the users options for editing and
'saving the file.
Sub CAREDIT ()

    _Title "Basketball: The Pro Game - Edit Career Files"

    Color 15, 0
    Cls

    careerFile$ = ChooseFile$(diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Call LoadCareerFile(careerFile$)

        Do

            Cls

            Color 7: Print "EDIT ";
            Color 12: Print "P";
            Color 7: Print "LAYERS"

            Color 12: Print "U";
            Color 7: Print "PDATE CARER FILE"

            Color 12: Print "E";
            Color 7: Print "XIT WITHOUT UPDATING"

            I$ = GetKeyPress$

            Select Case UCase$(I$)

                Case "P":
                    Call EditPlayers(careerFile$)

                Case "E", "X": '

                Case "U":
                    Call UpdateCareerFile(careerFile$)

            End Select

        Loop Until UCase$(I$) = "E" Or UCase$(I$) = "X"

    End If

End Sub


'----------------------------------------
'         EditPlayers Subroutine
'----------------------------------------
'This routine gives the user a choice of
'players to edit from the career stats,
'then presents options for handling them
'before deletions or edits are processed.
Sub EditPlayers (careerFile$)

    Shared careerPlayers$()

    Shared ARS!(), W0S!(), W1S!()

    Do

        Cls
        Color 12: Print careerFile$

        For I = 0 To 15:
            Locate I + 2
            Color 11: Print Using "##"; I;
            Print " ";
            Color 7: Print Left$(careerPlayers$(I), 12)
        Next

        For I = 16 To 31:
            Locate I - 14, 20
            Color 11: Print Using "##"; I;
            Print " ";
            Color 7: Print Left$(careerPlayers$(I), 12)
        Next

        For I = 32 To 46:
            Locate I - 30, 40
            Color 11: Print Using "##"; I;
            Print " ";
            Color 7: Print Left$(careerPlayers$(I), 12)
        Next

        For I = 47 To 61:
            Locate I - 45, 60
            Color 11: Print Using "##"; I;
            Print " ";
            Color 7: Print Left$(careerPlayers$(I), 12)
        Next

        Print

        Do
            Locate 22: Color 15
            Input "ENTER NUMBER OF PLAYER TO DELETE (62 TO EXIT)"; B9
        Loop Until B9 >= 0 And B9 <= 62

        If B9 <> 62 Then

            If B9 < 61 Then

                Cls

                Color 15
                Locate 5, 20: Print "PRINT CAREER STATS OF ";
                Color 12: Print careerPlayers$(B9)
                Color 15
                Locate , 20: Print "BEFORE DELETING THEM? (Y/N)"

                Color 14
                Print
                Locate , 20: Print "IF YOU DO NOT WISH TO DELETE THE STATS"
                Locate , 20: Print "OF ";
                Color 12: Print careerPlayers$(B9);
                Color 14: Print " HIT 'E', 'X' or 'Esc'"

                Do
                    I$ = GetKeyPress$

                    Select Case UCase$(I$)

                        Case "Y":
                            userKey$ = GetPrintOptions$

                            If userKey$ <> Chr$(27) Then

                                Select Case userKey$

                                    Case "F"
                                        userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                                        If userFile$ <> "" Then
                                            Call CareerStatsPlayer_Report(0, userFile$, careerFile$, B9)
                                        End If

                                    Case "P"
                                        Call CareerStatsPlayer_Report(1, "REPORT", careerFile$, B9)

                                End Select

                            End If

                        Case "N":
                            Cls

                        Case "E", "X", Chr$(27):
                            Exit Sub

                    End Select

                Loop Until UCase$(I$) = "N"

                Cls
                Locate 5, 15: Print "CHANGING PLAYER'S FILES . . . . . PLEASE WAIT"

                For I = B9 To 60:
                    careerPlayers$(I) = careerPlayers$(I + 1)
                Next

                UPP = (61 - B9) * 15
                I = B9

                For J = 1 To 15
                    For I = B9 To 60
                        For I1 = 0 To 15
                            ARS!(J, I + 1, I1) = ARS!(J, I + 1, I1)
                        Next I1

                        W0S!(N, I) = W0S!(N, I + 1)
                        W1S!(N, I) = W0S!(N, I + 1)

                        Locate 7, 36
                        Color 12: UPP = UPP - 1
                        Print Using "####"; UPP
                    Next
                Next

            End If

            If B9 < 62 Then 'we have to ignore if they entered over 62
                careerPlayers$(61) = ""
                For J = 1 To 15
                    For I1 = 0 To 15
                        ARS!(J, 61, I1) = 0
                        W0S!(J, 61) = 0
                        W1S!(J, 61) = 0
                    Next
                Next
            End If

        End If

    Loop Until B9 = 62

End Sub


'----------------------------------------
'  CareerStatsPlayer_Report Subroutine
'----------------------------------------
'This routine formats and prints career
'stats associated with the team's players
Sub CareerStatsPlayer_Report (printDest, destFile$, careerFile$, playerIdx)

    Shared careerPlayers$()

    Shared ARS!(), W0S!(), W1S!()

    B9 = playerIdx

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1, careerFile$

    'Process player
    TT1 = 0
    TT2 = 0
    TT3 = 0
    TT4 = 0
    TT5 = 0
    TT6 = 0
    TT7 = 0
    TT8 = 0
    TT9 = 0
    TT10 = 0
    TT11 = 0
    TT12 = 0
    TT13 = 0
    TT14 = 0
    TT15 = 0
    TT16 = 0
    TT17 = 0

    VV = ARS!(1, B9, 14) + ARS!(2, B9, 14) + ARS!(3, B9, 14) + ARS!(4, B9, 14) + ARS!(5, B9, 14) + ARS!(6, B9, 14) + ARS!(7, B9, 14) + ARS!(8, B9, 14) + ARS!(9, B9, 14) + ARS!(10, B9, 14) + ARS!(11, B9, 14) + ARS!(12, B9, 14) + ARS!(13, B9, 14) + ARS!(14, B9, 14) + ARS!(15, B9, 14)

    Print #1, Left$(careerPlayers$(B9), 11);

    For V = 1 To VV
        PTN = PTN + 1
        Print #1, Tab(12); ARS!(V, B9, 15);
        Print #1, Tab(19); Using "##"; ARS!(V, B9, 0);
        Print #1, Tab(23); Using "###"; ARS!(V, B9, 2);
        Print #1, Tab(28); Using "####"; ARS!(V, B9, 1);

        If ARS!(V, B9, 2) = 0 Then Print #1, Tab(34); "--.-";
        If ARS!(V, B9, 2) > 0 Then Print #1, Tab(33); Using "###.#"; (ARS!(V, B9, 2) / ARS!(V, B9, 1)) * 100;

        Print #1, Tab(39); Using "###"; ARS!(V, B9, 4);
        Print #1, Tab(45); Using "###"; ARS!(V, B9, 3);

        If ARS!(V, B9, 4) = 0 Then Print #1, Tab(50); "--.-";
        If ARS!(V, B9, 4) > 0 Then Print #1, Tab(49); Using "###.#"; (ARS!(V, B9, 4) / ARS!(V, B9, 3)) * 100;

        Print #1, Tab(55); Using "####"; ARS!(V, B9, 2) * 2 + ARS!(V, B9, 4) + W0S!(V, B9);

        If ARS!(V, B9, 2) * 2 + ARS!(V, B9, 4) + W0S!(V, B9) = 0 Then Print #1, Tab(61); " 0.0";
        If ARS!(V, B9, 2) * 2 + ARS!(V, B9, 4) + W0S!(V, B9) > 0 Then Print #1, Tab(61); Using "##.#"; (ARS!(V, B9, 2) * 2 + ARS!(V, B9, 4) + W0S!(V, B9)) / ARS!(V, B9, 0);

        Print #1, Tab(68); Using "###"; W0S!(V, B9);
        Print #1, Tab(73); Using "###"; W1S!(V, B9);

        If W0S!(V, B9) = 0 Then Print #1, Tab(78); "--.-";
        If W0S!(V, B9) > 0 Then Print #1, Tab(77); Using "###.#"; (W0S!(V, B9) / W1S!(V, B9)) * 100;

        Print #1, Tab(83); Using "###"; ARS!(V, B9, 5);

        If ARS!(V, B9, 5) = 0 Then Print #1, Tab(88); " 0.0";
        If ARS!(V, B9, 5) > 0 Then Print #1, Tab(88); Using "##.#"; ARS!(V, B9, 5) / ARS!(V, B9, 0);

        Print #1, Tab(94); Using "###"; ARS!(V, B9, 6);
        Print #1, Tab(99); Using "###"; ARS!(V, B9, 8);
        Print #1, Tab(104); Using "###"; ARS!(V, B9, 9);
        Print #1, Tab(109); Using "###"; ARS!(V, B9, 7);
        Print #1, Tab(113); Using "###"; ARS!(V, B9, 13);

        If ARS!(V, B9, 10) = 0 Then Print #1, Tab(118); "   0";
        If ARS!(V, B9, 10) > 0 Then Print #1, Tab(118); Using "####"; Int((ARS!(V, B9, 10) + 30) / 60);
        If ARS!(V, B9, 10) = 0 Then Print #1, Tab(125); " 0"
        If ARS!(V, B9, 10) > 0 Then Print #1, Tab(125); Using "##"; Int(((ARS!(V, B9, 10) / 60) / ARS!(V, B9, 0)) + .5)

    Next V

    For V = 1 To VV
        TT1 = TT1 + ARS!(V, B9, 0)
        TT2 = TT2 + ARS!(V, B9, 2)
        TT3 = TT3 + ARS!(V, B9, 1)
        TT4 = TT4 + ARS!(V, B9, 4)
        TT5 = TT5 + ARS!(V, B9, 3)
        TT6 = TT6 + (ARS!(V, B9, 2) * 2 + ARS!(V, B9, 4) + W0S!(V, B9))
        TT7 = TT7 + W0S!(V, B9)
        TT8 = TT8 + W1S!(V, B9)
        TT9 = TT9 + ARS!(V, B9, 5)
        TT10 = TT10 + ARS!(V, B9, 6)
        TT11 = TT11 + ARS!(V, B9, 8)
        TT12 = TT12 + ARS!(V, B9, 9)
        TT13 = TT13 + ARS!(V, B9, 7)
        TT14 = TT14 + ARS!(V, B9, 10)
        TT16 = TT16 + ARS!(V, B9, 11)
        TT17 = TT17 + ARS!(V, B9, 12)
        If ARS!(V, B9, 13) > TT15 Then TT15 = ARS!(V, B9, 13)
    Next V

    Print #1, "  TOTALS";
    Print #1, Tab(15); Using "##"; VV;
    Print #1, Tab(17); Using "####"; TT1;
    Print #1, Tab(21); Using "#####"; TT2;
    Print #1, Tab(27); Using "#####"; TT3;

    If TT2 = 0 Then Print #1, Tab(34); "--.-";
    If TT2 > 0 Then Print #1, Tab(34); Using "###.#"; (TT2 / TT3) * 100;

    Print #1, Tab(37); Using "#####"; TT4;
    Print #1, Tab(43); Using "#####"; TT5;

    If TT4 = 0 Then Print #1, Tab(50); "--.-";
    If TT4 > 0 Then Print #1, Tab(50); Using "###.#"; (TT4 / TT5) * 100;

    Print #1, Tab(55); Using "####"; TT6;

    If TT6 = 0 Then Print #1, Tab(61); " 0.0";
    If TT6 > 0 Then Print #1, Tab(61); Using "##.#"; TT6 / TT1;

    Print #1, Tab(67); Using "####"; TT7;
    Print #1, Tab(72); Using "####"; TT8;

    If TT7 = 0 Then Print #1, Tab(78); "--.-";
    If TT7 > 0 Then Print #1, Tab(78); Using "###.#"; (TT7 / TT8) * 100;

    Print #1, Tab(81); Using "#####"; TT9;

    If TT9 = 0 Then Print #1, Tab(88); " 0.0";
    If TT9 > 0 Then Print #1, Tab(88); Using "##.#"; TT9 / TT1;

    Print #1, Tab(93); Using "####"; TT10;
    Print #1, Tab(98); Using "####"; TT11;
    Print #1, Tab(103); Using "####"; TT12;
    Print #1, Tab(109); Using "###"; TT13;
    Print #1, Tab(114); Using "##"; TT15;

    If TT14 = 0 Then Print #1, Tab(117); "    0";
    If TT14 > 0 Then Print #1, Tab(117); Using "#####"; Int((TT14 + 30) / 60);
    If TT14 = 0 Then Print #1, Tab(125); " 0"
    If TT14 > 0 Then Print #1, Tab(125); Using "##"; Int((TT14 / 60) / TT1) + .5

    Print #1, Chr$(12)

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
