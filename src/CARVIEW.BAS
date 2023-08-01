Sub CARVIEW ()

    _Title "Basketball: The Pro Game - View Careers"

    Dim A$(62)

    Shared ars!(), W0S!(), W1S!()

    careerFile$ = ChooseFile$ (diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Color 15, 0
        Cls

        TMR = 930

        Locate 12, 10: Print "LOADING CAREER FILE . . . . PLEASE WAIT"

        Open diskPaths$(1) + careerFile$ + ".CAR" For Input As #2

        Input #2, NB, B$

        For N = 1 To 15

            For I = 0 To 61

                Input #2, A$(I)

                For J = 0 To 15
                    Input #2, ars!(N, I, J)
                Next J

                Input #2, W0S!(N, I)
                Input #2, W1S!(N, I)

                TMR = TMR - 1

                Locate 12, 52
                Color 5: Print Using "####"; TMR

            Next I

        Next N

        Close #2

        Color 15, 0
        Cls

        Do
            Locate 15, 10
            Color 12: Print "V";: Color 7: Print "iew or ";
            Color 12: Print "P";: Color 7: Print "rint"
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V"

        Select Case UCase$(I$)

            Case "P":
                Call CareerToPrinter(A$(), Z$)

            Case "V":
                Call CareerToScreen(A$(), Z$)

        End Select

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'       CareerToScreen subroutine
'----------------------------------------
'...explanation...
Sub CareerToScreen (A$(), Z$)

    Do
        Color 15, 0
        Cls

        Color 7: Print "VIEW ";
        Color 12: Print "P";: Color 7: Print "LAYERS, ";
        Color 12: Print "R";: Color 7: Print "OSTER IN SEQUENCE, OR ";
        Color 12: Print "E";: Color 7: Print "XIT TO MAIN MENU"
        I$ = GetKeyPress$

        Select Case UCase$(I$)

            Case "P":
                Call ViewPlayer(A$(), Z$)

            Case "R":
                Call ViewRosterSequence(A$(), Z$)

        End Select

    Loop Until UCase$(I$) = "E" Or UCase$(I$) = "X" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       CareerToPrinter subroutine
'----------------------------------------
'...explanation...
Sub CareerToPrinter (A$(), Z$)

    Color 0, 15

    PTN = 0

    LPrint Tab(59); Z$
    LPrint
    LPrint "PLAYER      YEAR   G  FGM   FGA  PCT  FTM   FTA  PCT   PTS  AVG.  3FGM 3FGA  PCT   RB   AVG   PF  AST  BLK   ST  HI   MIN  AVG."

    For I = 0 To 61

        If A$(I) <> "" And A$(I) <> "XXX" Then

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
            VV = ars!(1, I, 14) + ars!(2, I, 14) + ars!(3, I, 14) + ars!(4, I, 14) + ars!(5, I, 14) + ars!(6, I, 14) + ars!(7, I, 14) + ars!(8, I, 14) + ars!(9, I, 14) + ars!(10, I, 14) + ars!(11, I, 14) + ars!(12, I, 14) + ars!(13, I, 14) + ars!(14, I, 14) + ars!(15, I, 14)

            If 55 - PTN < VV Then
                LPrint Chr$(12)
                LPrint Tab(59); Z$
                LPrint
                LPrint "PLAYER      YEAR   G  FGM   FGA  PCT  FTM   FTA  PCT   PTS  AVG.  3FGM 3FGA  PCT   RB   AVG   PF  AST  BLK   ST  HI   MIN  AVG."
                PTN = 0
            Else
                LPrint Left$(A$(I), 11);

                For V = 1 To VV
                    PTN = PTN + 1
                    LPrint Tab(12); ars!(V, I, 15);
                    LPrint Tab(19); Using "##"; ars!(V, I, 0);
                    LPrint Tab(23); Using "###"; ars!(V, I, 2);
                    LPrint Tab(28); Using "####"; ars!(V, I, 1);

                    If ars!(V, I, 2) = 0 Then LPrint Tab(34); "--.-";
                    If ars!(V, I, 2) > 0 Then LPrint Tab(33); Using "###.#"; (ars!(V, I, 2) / ars!(V, I, 1)) * 100;

                    LPrint Tab(39); Using "###"; ars!(V, I, 4);
                    LPrint Tab(45); Using "###"; ars!(V, I, 3);

                    If ars!(V, I, 4) = 0 Then LPrint Tab(50); "--.-";
                    If ars!(V, I, 4) > 0 Then LPrint Tab(49); Using "###.#"; (ars!(V, I, 4) / ars!(V, I, 3)) * 100;

                    LPrint Tab(55); Using "####"; ars!(V, I, 2) * 2 + ars!(V, I, 4) + W0S!(V, I);

                    If ars!(V, I, 2) * 2 + ars!(V, I, 4) + W0S!(V, I) = 0 Then LPrint Tab(61); " 0.0";
                    If ars!(V, I, 2) * 2 + ars!(V, I, 4) + W0S!(V, I) > 0 Then LPrint Tab(61); Using "##.#"; (ars!(V, I, 2) * 2 + ars!(V, I, 4) + W0S!(V, I)) / ars!(V, I, 0);

                    LPrint Tab(68); Using "###"; W0S!(V, I);
                    LPrint Tab(73); Using "###"; W1S!(V, I);

                    If W0S!(V, I) = 0 Then LPrint Tab(78); "--.-";
                    If W0S!(V, I) > 0 Then LPrint Tab(77); Using "###.#"; (W0S!(V, I) / W1S!(V, I)) * 100;

                    LPrint Tab(83); Using "###"; ars!(V, I, 5);

                    If ars!(V, I, 5) = 0 Then LPrint Tab(88); " 0.0";
                    If ars!(V, I, 5) > 0 Then LPrint Tab(88); Using "##.#"; ars!(V, I, 5) / ars!(V, I, 0);

                    LPrint Tab(94); Using "###"; ars!(V, I, 6);
                    LPrint Tab(99); Using "###"; ars!(V, I, 8);
                    LPrint Tab(104); Using "###"; ars!(V, I, 9);
                    LPrint Tab(109); Using "###"; ars!(V, I, 7);
                    LPrint Tab(113); Using "###"; ars!(V, I, 13);

                    If ars!(V, I, 10) = 0 Then LPrint Tab(118); "   0";
                    If ars!(V, I, 10) > 0 Then LPrint Tab(118); Using "####"; Int((ars!(V, I, 10) + 30) / 60);
                    If ars!(V, I, 10) = 0 Then LPrint Tab(125); " 0"
                    If ars!(V, I, 10) > 0 Then LPrint Tab(125); Using "##"; Int(((ars!(V, I, 10) / 60) / ars!(V, I, 0)) + .5)

                Next V

                VV = ars!(1, I, 14) + ars!(2, I, 14) + ars!(3, I, 14) + ars!(4, I, 14) + ars!(5, I, 14) + ars!(6, I, 14) + ars!(7, I, 14) + ars!(8, I, 14) + ars!(9, I, 14) + ars!(10, I, 14) + ars!(11, I, 14) + ars!(12, I, 14) + ars!(13, I, 14) + ars!(14, I, 14) + ars!(15, I, 14)

                For V = 1 To VV
                    TT1 = TT1 + ars!(V, I, 0)
                    TT2 = TT2 + ars!(V, I, 2)
                    TT3 = TT3 + ars!(V, I, 1)
                    TT4 = TT4 + ars!(V, I, 4)
                    TT5 = TT5 + ars!(V, I, 3)
                    TT6 = TT6 + (ars!(V, I, 2) * 2 + ars!(V, I, 4) + W0S!(V, I))
                    TT7 = TT7 + W0S!(V, I)
                    TT8 = TT8 + W1S!(V, I)
                    TT9 = TT9 + ars!(V, I, 5)
                    TT10 = TT10 + ars!(V, I, 6)
                    TT11 = TT11 + ars!(V, I, 8)
                    TT12 = TT12 + ars!(V, I, 9)
                    TT13 = TT13 + ars!(V, I, 7)
                    TT14 = TT14 + ars!(V, I, 10)
                    TT16 = TT16 + ars!(V, I, 11)
                    TT17 = TT17 + ars!(V, I, 12)
                    If ars!(V, I, 13) > TT15 Then TT15 = ars!(V, I, 13)
                Next V

                LPrint "  TOTALS";
                LPrint Tab(15); Using "##"; VV;
                LPrint Tab(17); Using "####"; TT1;
                LPrint Tab(21); Using "#####"; TT2;
                LPrint Tab(27); Using "#####"; TT3;

                If TT2 = 0 Then LPrint Tab(34); "--.-";
                If TT2 > 0 Then LPrint Tab(34); Using "###.#"; (TT2 / TT3) * 100;

                LPrint Tab(37); Using "#####"; TT4;
                LPrint Tab(43); Using "#####"; TT5;

                If TT4 = 0 Then LPrint Tab(50); "--.-";
                If TT4 > 0 Then LPrint Tab(50); Using "###.#"; (TT4 / TT5) * 100;

                LPrint Tab(55); Using "####"; TT6;

                If TT6 = 0 Then LPrint Tab(61); " 0.0";
                If TT6 > 0 Then LPrint Tab(61); Using "##.#"; TT6 / TT1;

                LPrint Tab(67); Using "####"; TT7;
                LPrint Tab(72); Using "####"; TT8;

                If TT7 = 0 Then LPrint Tab(78); "--.-";
                If TT7 > 0 Then LPrint Tab(78); Using "###.#"; (TT7 / TT8) * 100;

                LPrint Tab(81); Using "#####"; TT9;

                If TT9 = 0 Then LPrint Tab(88); " 0.0";
                If TT9 > 0 Then LPrint Tab(88); Using "##.#"; TT9 / TT1;

                LPrint Tab(93); Using "####"; TT10;
                LPrint Tab(98); Using "####"; TT11;
                LPrint Tab(103); Using "####"; TT12;
                LPrint Tab(109); Using "###"; TT13;
                LPrint Tab(114); Using "##"; TT15;

                If TT14 = 0 Then LPrint Tab(117); "    0";
                If TT14 > 0 Then LPrint Tab(117); Using "#####"; Int((TT14 + 30) / 60);
                If TT14 = 0 Then LPrint Tab(125); " 0"
                If TT14 > 0 Then LPrint Tab(125); Using "##"; Int((TT14 / 60) / TT1) + .5

                LPrint Chr$(12)

                PTN = PTN + 2

            End If 'Done comparing PTN to VV

        End If ' Done checking for legitimate player

    Next I

End Sub

'----------------------------------------
'       ViewPlayer Subroutine
'----------------------------------------
'...explanation...
Sub ViewPlayer (A$(), Z$)

    Shared ars!(), W0S!(), W1S!()

    Cls
    Color 12: Print Z$

    For I = 0 To 15:
        Locate I + 2: Color 3
        Print Using "##"; I;
        Print " ";
        Color 7: Print A$(I)
    Next

    For I = 16 To 31:
        Locate I - 14, 20: Color 3
        Print Using "##"; I;
        Print " ";
        Color 7: Print A$(I)
    Next

    For I = 32 To 46:
        Locate I - 30, 40: Color 3
        Print Using "##"; I;
        Print " ";
        Color 7: Print A$(I):
    Next

    For I = 47 To 61:
        Locate I - 45, 60: Color 3
        Print Using "##"; I;
        Print " ";
        Color 7: Print A$(I)
    Next

    Print
    Color 15
    Locate 22, 48: Input "ENTER # OF PLAYER TO VIEW"; B9

    Cls

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

    Color 11
    Locate 1: Print Z$
    Color 12
    Locate 2: Print "PLAYER         YEAR  G  FGM  FGA  PCT FTM  FTA  PCT 3FGM 3FGA  PCT  PTS  AVG."
    Color 15
    Locate 3: Print Left$(A$(B9), 13);

    VV = ars!(1, B9, 14) + ars!(2, B9, 14) + ars!(3, B9, 14) + ars!(4, B9, 14) + ars!(5, B9, 14) + ars!(6, B9, 14) + ars!(7, B9, 14) + ars!(8, B9, 14) + ars!(9, B9, 14) + ars!(10, B9, 14) + ars!(11, B9, 14) + ars!(12, B9, 14) + ars!(13, B9, 14) + ars!(14, B9, 14) + ars!(15, B9, 14)

    For V = 1 To VV
        Color 7

        'Year
        Print Tab(15); ars!(V, B9, 15);
        'G
        Print Tab(21); Using "##"; ars!(V, B9, 0);
        'FGM
        Print Tab(24); Using "####"; ars!(V, B9, 2);
        'FGA
        Print Tab(29); Using "####"; ars!(V, B9, 1);

        'FG PCT
        If ars!(V, B9, 2) = 0 Then Print Tab(35); "--.-";
        If ars!(V, B9, 2) > 0 Then Print Tab(34); Using "###.#"; (ars!(V, B9, 2) / ars!(V, B9, 1)) * 100;

        'FTM
        Print Tab(40); Using "###"; ars!(V, B9, 4);
        'FTA
        Print Tab(44); Using "###"; ars!(V, B9, 3);

        'FT PCT
        If ars!(V, B9, 4) = 0 Then Print Tab(49); "--.-";
        If ars!(V, B9, 4) > 0 Then Print Tab(48); Using "###.#"; (ars!(V, B9, 4) / ars!(V, B9, 3)) * 100;

        '3FGM
        Print Tab(54); Using "###"; W0S!(V, B9);
        '3FGA
        Print Tab(59); Using "###"; W1S!(V, B9);

        '3FG PCT
        If W0S!(V, B9) = 0 Then Print Tab(64); "--.-";
        If W0S!(V, B9) > 0 Then Print Tab(63); Using "###.#"; (W0S!(V, B9) / W1S!(V, B9)) * 100;

        'PTS
        Print Tab(69); Using "####"; ars!(V, B9, 2) * 2 + ars!(V, B9, 4) + W0S!(V, B9);

        'AVG
        If ars!(V, B9, 2) * 2 + ars!(V, B9, 4) + W0S!(V, B9) = 0 Then Print Tab(74); " 0.0"
        If ars!(V, B9, 2) * 2 + ars!(V, B9, 4) + W0S!(V, B9) > 0 Then Print Tab(74); Using "##.#"; (ars!(V, B9, 2) * 2 + ars!(V, B9, 4) + W0S!(V, B9)) / ars!(V, B9, 0)

    Next V

    For V = 1 To VV

        TT1 = TT1 + ars!(V, B9, 0)
        TT2 = TT2 + ars!(V, B9, 2)
        TT3 = TT3 + ars!(V, B9, 1)
        TT4 = TT4 + ars!(V, B9, 4)
        TT5 = TT5 + ars!(V, B9, 3)
        TT6 = TT6 + (ars!(V, B9, 2) * 2 + ars!(V, B9, 4) + W0S!(V, B9))
        TT7 = TT7 + W0S!(V, B9)
        TT8 = TT8 + W1S!(V, B9)
    Next V

    Print
    Color 3
    Print
    Print Tab(18); Using "##"; VV;
    Print Tab(23); Using "#####"; TT2;

    If TT2 = 0 Then Print Tab(35); "--.-";
    If TT2 > 0 Then Print Tab(35); Using "##.#"; (TT2 / TT3) * 100;

    Print Tab(42); Using "#####"; TT5;
    Print Tab(53); Using "####"; TT7;

    If TT7 = 0 Then Print Tab(64); "--.-";
    If TT7 > 0 Then Print Tab(64); Using "##.#"; (TT7 / TT8) * 100;
    If TT6 = 0 Then Print Tab(74); " 0.0"
    If TT6 > 0 Then Print Tab(74); Using "##.#"; TT6 / TT1

    Print "TOTALS";
    Print Tab(19); Using "####"; TT1;
    Print Tab(28); Using "#####"; TT3;
    Print Tab(37); Using "#####"; TT4;

    If TT4 = 0 Then Print Tab(49); "--.-";
    If TT4 > 0 Then Print Tab(49); Using "##.#"; (TT4 / TT5) * 100;

    Print Tab(58); Using "####"; TT8;
    Print Tab(68); Using "####"; TT6
    Print
    Print
    Print
    Color 14: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

    Cls
    Color 11
    Locate 1: Print Z$
    Color 12: Print "PLAYER         YEAR OREB DREB  TRB  AVG  PF  AST BLK  ST  TO HI   MIN  AVG."
    Color 15
    Locate 3: Print Left$(A$(B9), 13);

    For V = 1 To VV

        Color 7
        Print Tab(15); ars!(V, B9, 15);

        Print Tab(21); Using "####"; ars!(V, B9, 12);
        Print Tab(26); Using "####"; ars!(V, B9, 5) - ars!(V, B9, 12);
        Print Tab(31); Using "####"; ars!(V, B9, 5);

        If ars!(V, B9, 5) = 0 Then Print Tab(36); " 0.0";
        If ars!(V, B9, 5) > 0 Then Print Tab(36); Using "##.#"; ars!(V, B9, 5) / ars!(V, B9, 0);

        Print Tab(41); Using "###"; ars!(V, B9, 6);
        Print Tab(46); Using "###"; ars!(V, B9, 8);
        Print Tab(50); Using "###"; ars!(V, B9, 9);
        Print Tab(54); Using "###"; ars!(V, B9, 7);
        Print Tab(58); Using "###"; ars!(V, B9, 11);
        Print Tab(61); Using "###"; ars!(V, B9, 13);

        If ars!(V, B9, 10) = 0 Then Print Tab(65); "   0";
        If ars!(V, B9, 10) > 0 Then Print Tab(66); Using "####"; Int((ars!(V, B9, 10) + 30) / 60);
        If ars!(V, B9, 10) = 0 Then Print Tab(73); " 0";
        If ars!(V, B9, 10) > 0 Then Print Tab(73); Using "##"; Int(((ars!(V, B9, 10) / 60) / ars!(V, B9, 0)) + .5);

    Next V

    For V = 1 To VV
        TT9 = TT9 + ars!(V, B9, 5)
        TT10 = TT10 + ars!(V, B9, 6)
        TT11 = TT11 + ars!(V, B9, 8)
        TT12 = TT12 + ars!(V, B9, 9)
        TT13 = TT13 + ars!(V, B9, 7)
        TT14 = TT14 + ars!(V, B9, 10)
        TT16 = TT16 + ars!(V, B9, 11)
        TT17 = TT17 + ars!(V, B9, 12)
        If ars!(V, B9, 13) > TT15 Then TT15 = ars!(V, B9, 13)
    Next V

    Print
    Color 3
    Print
    Print Tab(18); Using "##"; VV;
    Print Tab(26); Using "####"; TT9 - TT17;

    If TT9 = 0 Then Print Tab(36); " 0.0";
    If TT9 > 0 Then Print Tab(36); Using "##.#"; TT9 / TT1;

    Print Tab(45); Using "####"; TT11;
    Print Tab(54); Using "###"; TT13;
    Print Tab(62); Using "##"; TT15;

    If TT14 = 0 Then Print Tab(73); "--.-"
    If TT14 > 0 Then Print Tab(73); Using "##"; Int((TT14 / 60) / TT1) + .5

    Print
    Print "TOTALS";
    Print Tab(21); Using "####"; TT17;
    Print Tab(30); Using "#####"; TT9;
    Print Tab(40); Using "####"; TT10;
    Print Tab(49); Using "####"; TT12;
    Print Tab(58); Using "###"; TT16;

    If TT14 = 0 Then Print Tab(65); "  --.-"
    If TT14 > 0 Then Print Tab(65); Using "#####"; Int((TT14 + 30) / 60)

    Print
    Print
    Color 14
    Input "HIT ENTER TO CONTINUE"; DUMMY$

End Sub

'----------------------------------------
'       ViewRosterSequence Subroutine
'----------------------------------------
'...explanation...
Sub ViewRosterSequence (A$(), Z$)

    Shared ars!(), W0S!(), W1S!()

    Do

        For I = 0 To 61

            'Check for valid player
            If A$(I) <> "" And A$(I) <> "XXX" Then

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

                Cls

                Color 11
                Locate 1: Print Z$
                Color 12
                Locate 2: Print "PLAYER         YEAR  G FGM  FGA   PCT FTM FTA   PCT 3FGM 3FGA PCT  PTS  AVG"
                Color 15
                Locate 3: Print Left$(A$(I), 13);

                VV = ars!(1, I, 14) + ars!(2, I, 14) + ars!(3, I, 14) + ars!(4, I, 14) + ars!(5, I, 14) + ars!(6, I, 14) + ars!(7, I, 14) + ars!(8, I, 14) + ars!(9, I, 14) + ars!(10, I, 14) + ars!(11, I, 14) + ars!(12, I, 14) + ars!(13, I, 14) + ars!(14, I, 14) + ars!(15, I, 14)

                For V = 1 To VV

                    Color 7
                    'Year
                    Print Tab(15); ars!(V, I, 15);

                    'Games
                    Print Tab(21); Using "##"; ars!(V, I, 0);
                    'FGM
                    Print Tab(24); Using "####"; ars!(V, I, 2);
                    'FGA
                    Print Tab(29); Using "####"; ars!(V, I, 1);

                    'FG PCT
                    If ars!(V, I, 2) = 0 Then Print Tab(33); " --.-";
                    If ars!(V, I, 2) > 0 Then Print Tab(33); Using "###.#"; (ars!(V, I, 2) / ars!(V, I, 1)) * 100;

                    'FTM
                    Print Tab(39); Using "###"; ars!(V, I, 4);
                    'FTA
                    Print Tab(43); Using "###"; ars!(V, I, 3);

                    'FT PCT
                    If ars!(V, I, 4) = 0 Then Print Tab(47); " --.-";
                    If ars!(V, I, 4) > 0 Then Print Tab(47); Using "###.#"; (ars!(V, I, 4) / ars!(V, I, 3)) * 100;

                    '3FGM
                    Print Tab(53); Using "###"; W0S!(V, I);
                    '3FGA
                    Print Tab(57); Using "###"; W1S!(V, I);

                    '3FG PCT
                    If W0S!(V, I) = 0 Then Print Tab(61); " --.-";
                    If W0S!(V, I) > 0 Then Print Tab(61); Using "###.#"; (W0S!(V, I) / W1S!(V, I)) * 100;

                    'PTS
                    Print Tab(67); Using "####"; ars!(V, I, 2) * 2 + ars!(V, I, 4) + W0S!(V, I);

                    'AVG
                    If ars!(V, I, 2) * 2 + ars!(V, I, 4) + W0S!(V, I) = 0 Then Print Tab(72); " 0.0"
                    If ars!(V, I, 2) * 2 + ars!(V, I, 4) + W0S!(V, I) > 0 Then Print Tab(72); Using "##.#"; (ars!(V, I, 2) * 2 + ars!(V, I, 4) + W0S!(V, I)) / ars!(V, I, 0)

                Next V

                VV = ars!(1, I, 14) + ars!(2, I, 14) + ars!(3, I, 14) + ars!(4, I, 14) + ars!(5, I, 14) + ars!(6, I, 14) + ars!(7, I, 14) + ars!(8, I, 14) + ars!(9, I, 14) + ars!(10, I, 14) + ars!(11, I, 14) + ars!(12, I, 14) + ars!(13, I, 14) + ars!(14, I, 14) + ars!(15, I, 14)

                For V = 1 To VV
                    TT1 = TT1 + ars!(V, I, 0)
                    TT2 = TT2 + ars!(V, I, 2)
                    TT3 = TT3 + ars!(V, I, 1)
                    TT4 = TT4 + ars!(V, I, 4)
                    TT5 = TT5 + ars!(V, I, 3)
                    TT6 = TT6 + (ars!(V, I, 2) * 2 + ars!(V, I, 4) + W0S!(V, I))
                    TT7 = TT7 + W0S!(V, I)
                    TT8 = TT8 + W1S!(V, I)
                Next V

                Print
                Color 3
                Print
                Print Tab(18); Using "##"; VV;
                Print Tab(22); Using "#####"; TT2;

                If TT2 = 0 Then Print Tab(33); " --.-";
                If TT2 > 0 Then Print Tab(33); Using "###.#"; (TT2 / TT3) * 100;

                Print Tab(41); Using "#####"; TT5;
                Print Tab(52); Using "####"; TT7;

                If TT7 = 0 Then Print Tab(61); " --.-";
                If TT7 > 0 Then Print Tab(61); Using "###.#"; (TT7 / TT8) * 100;
                If TT6 = 0 Then Print Tab(72); " 0.0"
                If TT6 > 0 Then Print Tab(72); Using "##.#"; TT6 / TT1

                Print "TOTALS";
                Print Tab(19); Using "####"; TT1;
                Print Tab(27); Using "#####"; TT3;
                Print Tab(37); Using "#####"; TT4;

                If TT4 = 0 Then Print Tab(47); " --.-";
                If TT4 > 0 Then Print Tab(47); Using "###.#"; (TT4 / TT5) * 100;

                Print Tab(56); Using "####"; TT8;
                Print Tab(67); Using "####"; TT6

                Print
                Print
                Print
                Color 14: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

                Cls

                Color 11
                Locate 1: Print Z$
                Color 12
                Print "PLAYER         YEAR OREB DREB  TRB  AVG  PF  AST BLK  ST  TO HI   MIN  AVG."
                Color 15
                Locate 3: Print Left$(A$(I), 13);

                For V = 1 To VV

                    Color 7
                    Print Tab(15); ars!(V, I, 15);

                    Print Tab(21); Using "####"; ars!(V, I, 12);
                    Print Tab(26); Using "####"; ars!(V, I, 5) - ars!(V, I, 12);
                    Print Tab(31); Using "####"; ars!(V, I, 5);

                    If ars!(V, I, 5) = 0 Then Print Tab(36); " 0.0";
                    If ars!(V, I, 5) > 0 Then Print Tab(36); Using "##.#"; ars!(V, I, 5) / ars!(V, I, 0);

                    Print Tab(41); Using "###"; ars!(V, I, 6);
                    Print Tab(46); Using "###"; ars!(V, I, 8);
                    Print Tab(50); Using "###"; ars!(V, I, 9);
                    Print Tab(54); Using "###"; ars!(V, I, 7);
                    Print Tab(58); Using "###"; ars!(V, I, 11);
                    Print Tab(61); Using "###"; ars!(V, I, 13);

                    If ars!(V, I, 10) = 0 Then Print Tab(65); "   0";
                    If ars!(V, I, 10) > 0 Then Print Tab(66); Using "####"; Int((ars!(V, I, 10) + 30) / 60);

                    If ars!(V, I, 10) = 0 Then Print Tab(73); " 0"
                    If ars!(V, I, 10) > 0 Then Print Tab(73); Using "##"; Int(((ars!(V, I, 10) / 60) / ars!(V, I, 0)) + .5)

                Next V

                VV = ars!(1, I, 14) + ars!(2, I, 14) + ars!(3, I, 14) + ars!(4, I, 14) + ars!(5, I, 14) + ars!(6, I, 14) + ars!(7, I, 14) + ars!(8, I, 14) + ars!(9, I, 14) + ars!(10, I, 14) + ars!(11, I, 14) + ars!(12, I, 14) + ars!(13, I, 14) + ars!(14, I, 14) + ars!(15, I, 14)

                For V = 1 To VV

                    TT9 = TT9 + ars!(V, I, 5)
                    TT10 = TT10 + ars!(V, I, 6)
                    TT11 = TT11 + ars!(V, I, 8)
                    TT12 = TT12 + ars!(V, I, 9)
                    TT13 = TT13 + ars!(V, I, 7)
                    TT14 = TT14 + ars!(V, I, 10)
                    TT16 = TT16 + ars!(V, I, 11)
                    TT17 = TT17 + ars!(V, I, 12)

                    If ars!(V, I, 13) > TT15 Then TT15 = ars!(V, I, 13)

                Next V

                Print
                Color 3
                Print
                Print Tab(18); Using "##"; VV;
                Print Tab(26); Using "####"; TT9 - TT17;

                If TT9 = 0 Then Print Tab(36); " 0.0";
                If TT9 > 0 Then Print Tab(36); Using "##.#"; TT9 / TT1;

                Print Tab(45); Using "####"; TT11;
                Print Tab(54); Using "###"; TT13;
                Print Tab(62); Using "##"; TT15;

                If TT14 = 0 Then Print Tab(73); " 0"
                If TT14 > 0 Then Print Tab(73); Using "##"; Int((TT14 / 60) / TT1) + .5

                Print
                Print "TOTALS";
                Print Tab(21); Using "####"; TT17;
                Print Tab(30); Using "#####"; TT9;
                Print Tab(40); Using "####"; TT10;
                Print Tab(49); Using "####"; TT12;
                Print Tab(58); Using "###"; TT16;

                If TT14 = 0 Then Print Tab(65); "    0"
                If TT14 > 0 Then Print Tab(65); Using "#####"; Int((TT14 + 30) / 60)

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

                Print
                Print
                Print
                Color 14
                Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                I$ = GetKeyPress$

                If I$ = Chr$(27) Then Exit Sub

            End If 'Done checking for valid player

        Next I

        Cls

        Color 14

        Do
            Locate 5, 20
            Print "VIEW AGAIN (Y/N)"
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

    Loop Until UCase$(I$) = "N"

End Sub

