Sub HD2HD ()

    Dim DV$(4), O$(100), H$(1 To 100)
    Dim Z2$(TEAMS_PER_CONFERENCE)
    ReDim menuChoices$(1 To 2)

    Dim intT%(100)

	Shared statO%(), intZ1%()
	
    Shared AW!(), hd2hdAL!(), hd2hdT!(), HL!(), HW!()
    Shared R1!(), R2!(), R3!(), R4!()
    Shared TAL!(), TAW!(), THL!(), THW!()
    Shared TR1!(), TR2!(), TR3!(), TR4!()

    _Title "Basketball: The Pro Game - Head-to-Head Stats"

    Do
        Cls

        Call TITLE
        'Erase menuChoices$
        menuChoices$(1) = "VIEW TEAM VS LEAGUE    "
        menuChoices$(2) = "RETURN TO PRO GAME MENU"
        Selection% = 1
        Count% = 2
        Start% = 1
        ExitCode% = 2
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 10, 48, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:

                    RDD = 0

                    Call GetTeam_HD2HD (TM$)

                    B$ = QQ$(1)

                    For XX = 4 To 11:
                        If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
                    Next XX
                    For XX = 12 To 14:
                        If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
                    Next XX
                    QQ = 8
                    For XX = 11 To 4 Step -1:
                        If Mid$(B$, XX, 1) = "_" Then
                            QQ = XX - 4
                        Else
                            Exit For
                        End If
                    Next XX

                    Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)

                    If Not _FileExists(diskPaths$(1) + Z$ + "S") Then
                        Cls
                        Locate 10, 18
                        Call CSMsgBox("NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)
                        Color , 0
                    Else
                        Open diskPaths$(1) + Z$ + "S" For Input As #4
                        Input #4, Y
                        For J = 1 To 100: Input #4, statO%(J): Next
                        For J = 1 To 100: Input #4, O$(J): Next
                        For J = 1 To 100: Input #4, intT%(J): Next
                        For J = 1 To 100: Input #4, H$(J): Next
                        Close #4
                        Call SelectLeague_HD2HD(LG$)

                        If LG$ <> "" Then
                            Cls
                            J = 5
                            Locate 8, 8
                            Color 15, 1: Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";
                            For II = 1 To J
                                Locate 8 + II, 8: Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
                            Next
                            Locate 9 + J, 8: Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

                            Do
                                Locate 10, 15
                                Color 7, 1

                                Locate , 30: Print "STATS TO ";: Color 15: Print "S";: Color 7: Print "CREEN"
                                Locate , 30: Print "STATS TO ";: Color 15: Print "P";: Color 7: Print "RINTER"
                                I$ = GetKeyPress$
                                Color 14, 0

                            Loop Until UCase$(I$) = "S" Or UCase$(I$) = "P"

                            VPP = 0

                            If UCase$(I$) = "P" Then
                                X1 = 1
                            Else
                                VPP = 1
                            End If

                            W$ = LG$ + ".LIB"

                            Open diskPaths$(1) + W$ For Input As #1
                            Input #1, LG$
                            Input #1, D1$: Input #1, D2$: Input #1, D3$: Input #1, D4$
                            For X = 1 To TEAMS_PER_CONFERENCE
                                Input #1, Z1$(X): Input #1, intZ1%(X): Input #1, Z2$(X)
                            Next

                            NTMS = TEAMS_PER_CONFERENCE
                            Close 1

                            Cls
                            Color 15
                            Locate 18, 18: Print "COMPUTING W/L/T RECORDS . . . PLEASE WAIT"

                            For Q = 1 To TEAMS_PER_CONFERENCE
                                AW!(Q) = 0
                                HW!(Q) = 0
                                hd2hdAL!(Q) = 0
                                HL!(Q) = 0
                                R1!(Q) = 0
                                R2!(Q) = 0
                                R3!(Q) = 0
                                R4!(Q) = 0
                            Next

                            For K = 1 To DIVS_PER_CONFERENCE
                                THW!(K) = 0
                                THL!(K) = 0
                                TAW!(K) = 0
                                TAL!(K) = 0
                                TR1!(K) = 0
                                TR2!(K) = 0
                                TR3!(K) = 0
                                TR4!(K) = 0
                            Next

                            For K = 1 To TEAMS_PER_CONFERENCE
                                If Z1$(K) <> "XXX" Then
                                    For I = 1 To Y
                                        If statO%(I) > intT%(I) And Left$(H$(I), 1) = "V" And O$(I) = Z1$(K) Then AW!(K) = AW!(K) + 1
                                        If statO%(I) > intT%(I) And (Left$(H$(I), 1) = "H" Or Left$(H$(I), 1) = "N") And O$(I) = Z1$(K) Then HW!(K) = HW!(K) + 1
                                        If statO%(I) < intT%(I) And Left$(H$(I), 1) = "V" And O$(I) = Z1$(K) Then hd2hdAL!(K) = hd2hdAL!(K) + 1
                                        If statO%(I) < intT%(I) And (Left$(H$(I), 1) = "H" Or Left$(H$(I), 1) = "N") And O$(I) = Z1$(K) Then HL!(K) = HL!(K) + 1
                                        If O$(I) = Z1$(K) And (Left$(H$(I), 1) = "H" Or Left$(H$(I), 1) = "N") Then R2!(K) = R2!(K) + intT%(I)
                                        If O$(I) = Z1$(K) And (Left$(H$(I), 1) = "H" Or Left$(H$(I), 1) = "N") Then R1!(K) = R1!(K) + statO%(I)
                                        If O$(I) = Z1$(K) And Left$(H$(I), 1) = "V" Then R4!(K) = R4!(K) + intT%(I)
                                        If O$(I) = Z1$(K) And Left$(H$(I), 1) = "V" Then R3!(K) = R3!(K) + statO%(I)
                                    Next I
                                End If
                            Next K

                            If VPP <> 1 Then
                                Call PrintHD2HD
                            Else
                                'Not sending to printer

                                '--This should be a sub
                                DV$(1) = D1$
                                DV$(2) = D2$
                                DV$(3) = D3$
                                DV$(4) = D4$

                                Do
                                    Cls
                                    Locate 2, 35
                                    Color hd2hdT!(33), hd2hdT!(34): Print TM$
                                    Color 3, 0

                                    'Table header
                                    Locate 4
                                    Print Tab(15); String$(7, 196); " HOME "; String$(8, 196); Tab(36); Chr$(194); String$(7, 196); " AWAY "; String$(8, 196); Tab(58); Chr$(194); String$(6, 196); " TOTALS "; String$(7, 196)
                                    Locate 5
                                    Print Tab(15); "W"; Tab(17); "L"; Tab(21); "PCT."; Tab(26); "PF/G"; Tab(32); "PA/G"; Tab(36); Chr$(179); Tab(37); "W"; Tab(39); "L"; Tab(43); "PCT."; Tab(48); "PF/G"; Tab(54); "PA/G"; Tab(58); Chr$(179); Tab(59); "W"; Tab(61); "L"; Tab(65); "PCT."; Tab(70); "PF/G"; Tab(76); "PA/G"

                                    RDD = RDD + 1

                                    DD = ((RDD - 1) * 10) + 1
                                    EE = RDD * 10

                                    If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then Exit Do

                                    Color 11: Print DV$(RDD);
                                    Color 3: Print Tab(36); Chr$(179); Tab(58); Chr$(179)

                                    For K = DD To EE

                                        If Z1$(K) <> "XXX" And Z1$(K) <> Q$(0) Then

                                            Color 15
                                            Locate , 1: Print Left$(Z1$(K), 13);
                                            Color 7
                                            Locate , 15: Print Using "#"; HW!(K);
                                            Locate , 17: Print Using "#"; HL!(K);

                                            If HW!(K) = HW!(K) + HL!(K) And HW!(K) <> 0 Then Locate , 19: Print "1.000";
                                            If HW!(K) = 0 Then Locate , 20: Print ".000";
                                            If HW!(K) > 0 And HW!(K) < HW!(K) + HL!(K) Then Locate , 20: Print Using ".###"; (HW!(K) / (HW!(K) + HL!(K)));
                                            If R1!(K) = 0 Then Locate , 25: Print "  0.0";
                                            If R1!(K) > 0 Then Locate , 25: Print Using "###.#"; R1!(K) / (HW!(K) + HL!(K));
                                            If R2!(K) = 0 Then Locate , 31: Print "  0.0";
                                            If R2!(K) > 0 Then Locate , 31: Print Using "###.#"; R2!(K) / (HW!(K) + HL!(K));
                                            Color 3: Print Tab(36); Chr$(179);
                                            Color 7
                                            Locate , 37: Print Using "#"; AW!(K);
                                            Locate , 39: Print Using "#"; hd2hdAL!(K);

                                            If AW!(K) = AW!(K) + hd2hdAL!(K) And AW!(K) <> 0 Then Locate , 41: Print "1.000";
                                            If AW!(K) = 0 Then Locate , 42: Print ".000";
                                            If AW!(K) > 0 And AW!(K) < AW!(K) + hd2hdAL!(K) Then Locate , 42: Print Using ".###"; (AW!(K) / (AW!(K) + hd2hdAL!(K)));
                                            If R3!(K) = 0 Then Locate , 47: Print "  0.0";
                                            If R3!(K) > 0 Then Locate , 47: Print Using "###.#"; R3!(K) / (AW!(K) + hd2hdAL!(K));
                                            If R4!(K) = 0 Then Locate , 53: Print "  0.0";
                                            If R4!(K) > 0 Then Locate , 53: Print Using "###.#"; R4!(K) / (AW!(K) + hd2hdAL!(K));

                                            Color 3: Print Tab(58); Chr$(179);
                                            Color 7
                                            Locate , 59: Print Using "##"; HW!(K) + AW!(K);
                                            Locate , 61: Print Using "##"; HL!(K) + hd2hdAL!(K);

                                            If (HW!(K) + AW!(K)) = (HW!(K) + AW!(K) + HL!(K) + hd2hdAL!(K)) And (HW!(K) + AW!(K)) <> 0 Then Locate , 63: Print "1.000";
                                            If (HW!(K) + AW!(K)) = 0 Then Locate , 64: Print ".000";
                                            If (HW!(K) + AW!(K)) > 0 And (HW!(K) + AW!(K)) < (HW!(K) + AW!(K) + HL!(K) + hd2hdAL!(K)) Then Locate , 64: Print Using ".###"; ((HW!(K) + AW!(K)) / (HW!(K) + AW!(K) + HL!(K) + hd2hdAL!(K)));
                                            If R1!(K) + R3!(K) = 0 Then Locate , 69: Print "  0.0";
                                            If R1!(K) + R3!(K) > 0 Then Locate , 69: Print Using "###.#"; (R1!(K) + R3!(K)) / (HW!(K) + HL!(K) + AW!(K) + hd2hdAL!(K));
                                            If R2!(K) + R4!(K) = 0 Then Locate , 75: Print "  0.0";
                                            If R2!(K) + R4!(K) > 0 Then Locate , 75: Print Using "###.#"; (R2!(K) + R4!(K)) / (HW!(K) + HL!(K) + AW!(K) + hd2hdAL!(K))

                                            Color 3: Print Tab(36); Chr$(179); Tab(58); Chr$(179);

                                            THW!(RDD) = THW!(RDD) + HW!(K)
                                            THL!(RDD) = THL!(RDD) + HL!(K)
                                            TAW!(RDD) = TAW!(RDD) + AW!(K)
                                            TAL!(RDD) = TAL!(RDD) + hd2hdAL!(K)
                                            TR1!(RDD) = TR1!(RDD) + R1!(K)
                                            TR2!(RDD) = TR2!(RDD) + R2!(K)
                                            TR3!(RDD) = TR3!(RDD) + R3!(K)
                                            TR4!(RDD) = TR4!(RDD) + R4!(K)

                                        End If

                                    Next K

                                    Print

                                    Color 11
                                    Locate , 14: Print Using "##"; THW!(RDD);

                                    If THW!(RDD) = THW!(RDD) + THL!(RDD) And THW!(RDD) <> 0 Then Locate , 19: Print "1.000";
                                    If THW!(RDD) = 0 Then Locate , 20: Print ".000";
                                    If THW!(RDD) > 0 And THW!(RDD) < THW!(RDD) + THL!(RDD) Then Locate , 20: Print Using ".###"; (THW!(RDD) / (THW!(RDD) + THL!(RDD)));
                                    If TR2!(RDD) < 1 Then Locate , 32: Print " 0.0";
                                    If TR2!(RDD) > 0 Then Locate , 31: Print Using "###.#"; TR2!(RDD) / (THW!(RDD) + THL!(RDD));

                                    Locate , 38: Print Using "##"; TAL!(RDD);

                                    If TR3!(RDD) < 1 Then Locate , 48: Print " 0.0";
                                    If TR3!(RDD) > 0 Then Locate , 47: Print Using "###.#"; TR3!(RDD) / (TAW!(RDD) + TAL!(RDD));

                                    Locate , 57: Print Using "###"; THW!(RDD) + TAW!(RDD);
                                    Color 11

                                    If THW!(RDD) + TAW!(RDD) = THW!(RDD) + THL!(RDD) + TAW!(RDD) + TAL!(RDD) And THW!(RDD) + TAW!(RDD) <> 0 Then Locate , 63: Print "1.000";
                                    If THW!(RDD) + TAW!(RDD) = 0 Then Locate , 64: Print ".000";
                                    If THW!(RDD) + TAW!(RDD) > 0 And THW!(RDD) + TAW!(RDD) < THW!(RDD) + TAW!(RDD) + THL!(RDD) + TAL!(RDD) Then Locate , 64: Print Using ".###"; (THW!(RDD) + TAW!(RDD)) / (THW!(RDD) + TAW!(RDD) + THL!(RDD) + TAL!(RDD));
                                    If TR2!(RDD) + TR4!(RDD) < 1 Then Locate , 76: Print " 0.0"
                                    If TR2!(RDD) + TR4!(RDD) > 0 Then Locate , 75: Print Using "###.#"; (TR2!(RDD) + TR4!(RDD)) / (THW!(RDD) + THL!(RDD) + TAW!(RDD) + TAL!(RDD))

                                    Locate , 5: Print "TOTALS";

                                    If TR1!(RDD) < 1 Then Locate , 26: Print " 0.0";
                                    If TR1!(RDD) > 0 Then Locate , 25: Print Using "###.#"; TR1!(RDD) / (THW!(RDD) + THL!(RDD));

                                    Color 11: If TAW!(RDD) = TAW!(RDD) + TAL!(RDD) And TAW!(RDD) <> 0 Then Locate , 41: Print "1.000";

                                    If TAW!(RDD) = 0 Then Locate , 42: Print ".000";
                                    If TAW!(RDD) > 0 And TAW!(RDD) < TAW!(RDD) + TAL!(RDD) Then Locate , 42: Print Using ".###"; (TAW!(RDD) / (TAW!(RDD) + TAL!(RDD)));
                                    If TR4!(RDD) < 1 Then Locate , 54: Print " 0.0";
                                    If TR4!(RDD) > 0 Then Locate , 53: Print Using "###.#"; TR4!(RDD) / (TAW!(RDD) + TAL!(RDD));
                                    If TR1!(RDD) + TR3!(RDD) = 0 Then Locate , 70: Print " 0.0";
                                    If TR1!(RDD) + TR3!(RDD) > 0 Then Locate , 69: Print Using "###.#"; (TR1!(RDD) + TR3!(RDD)) / (THW!(RDD) + THL!(RDD) + TAW!(RDD) + TAL!(RDD));

                                    Locate , 16: Print Using "##"; THL!(RDD);
                                    Locate , 36: Print Using "##"; TAW!(RDD);
                                    Locate , 59: Print Using "###"; THL!(RDD) + TAL!(RDD)
                                    Locate 20, 5: Color 14: Input "HIT ENTER TO CONTINUE"; DUMMY$

                                Loop Until RDD = 4

                                Cls
                                Locate 2, 35
                                Color hd2hdT!(33), hd2hdT!(34): Print TM$
                                Color 3, 0

                                Locate 4
                                Print Tab(9); String$(8, 196); " HOME "; String$(9, 196); Tab(32); Chr$(194); String$(8, 196); " AWAY "; String$(9, 196); Tab(56); Chr$(194); String$(7, 196); " TOTALS "; String$(8, 196)
                                Locate 5
                                Print Tab(10); "W"; Tab(13); "L"; Tab(17); "PCT."; Tab(22); "PF/G"; Tab(28); "PA/G"; Tab(32); Chr$(179); Tab(34); "W"; Tab(37); "L"; Tab(41); "PCT."; Tab(46); "PF/G"; Tab(52); "PA/G"; Tab(56); Chr$(179); Tab(58); "W"; Tab(61); "L"; Tab(65); "PCT."; Tab(70); "PF/G"; Tab(76); "PA/G"

                                Color 3: Print Tab(32); Chr$(179); Tab(56); Chr$(179)

                                For W = 1 To DIVS_PER_CONFERENCE

                                    If DV$(W) <> "XXX" And DV$(W) <> "" Then

                                        Color 3: Print Tab(32); Chr$(179); Tab(56); Chr$(179);

                                        Color 15: Locate , 1: Print Left$(DV$(W), 10);
                                        Color 7: Locate , 9: Print Using "##"; THW!(W);
                                        Locate , 12: Print Using "##"; THL!(W);

                                        If THW!(W) = THW!(W) + THL!(W) And THW!(W) <> 0 Then Locate , 15: Print "1.000";
                                        If THW!(W) = 0 Then Locate , 16: Print ".000";
                                        If THW!(W) > 0 And THW!(W) < THW!(W) + THL!(W) Then Locate , 16: Print Using ".###"; (THW!(W) / (THW!(W) + THL!(W)));
                                        If TR1!(W) = 0 Then Locate , 21: Print "  0.0";
                                        If TR1!(W) > 0 Then Locate , 21: Print Using "###.#"; TR1!(W) / (THW!(W) + THL!(W));
                                        If TR2!(W) = 0 Then Locate , 27: Print "  0.0";
                                        If TR2!(W) > 0 Then Locate , 27: Print Using "###.#"; TR2!(W) / (THW!(W) + THL!(W));

                                        Color 3: Print Tab(32); Chr$(179);
                                        Color 7: Locate , 33: Print Using "##"; TAW!(W);
                                        Locate , 36: Print Using "##"; TAL!(W);

                                        If TAW!(W) = TAW!(W) + TAL!(W) And TAW!(W) <> 0 Then Locate , 39: Print "1.000";
                                        If TAW!(W) = 0 Then Locate , 40: Print ".000";
                                        If TAW!(W) > 0 And TAW!(W) < TAW!(W) + TAL!(W) Then Locate , 40: Print Using ".###"; (TAW!(W) / (TAW!(W) + TAL!(W)));
                                        If TR3!(W) = 0 Then Locate , 45: Print "  0.0";
                                        If TR3!(W) > 0 Then Locate , 45: Print Using "###.#"; TR3!(W) / (TAW!(W) + TAL!(W));
                                        If TR4!(W) = 0 Then Locate , 51: Print "  0.0";
                                        If TR4!(W) > 0 Then Locate , 51: Print Using "###.#"; TR4!(W) / (TAW!(W) + TAL!(W));

                                        Color 3: Print Tab(56); Chr$(179);
                                        Color 7
                                        Locate , 57: Print Using "##"; THW!(W) + TAW!(W);
                                        Locate , 60: Print Using "##"; THL!(W) + TAL!(W);

                                        If THW!(W) + TAW!(W) = THW!(W) + TAW!(W) + THL!(W) + TAL!(W) And THW!(W) + TAW!(W) <> 0 Then Locate , 63: Print "1.000";
                                        If (THW!(W) + TAW!(W)) = 0 Then Locate , 64: Print ".000";
                                        If (THW!(W) + TAW!(W)) > 0 And THW!(W) + TAW!(W) < THW!(W) + TAW!(W) + THL!(W) + TAL!(W) Then Locate , 64: Print Using ".###"; ((THW!(W) + TAW!(W)) / (THW!(W) + TAW!(W) + THL!(W) + TAL!(W)));
                                        If TR1!(W) + TR3!(W) = 0 Then Locate , 69: Print "  0.0";
                                        If TR1!(W) + TR3!(W) > 0 Then Locate , 69: Print Using "###.#"; (TR1!(W) + TR3!(W)) / (THW!(W) + THL!(W) + TAW!(W) + TAL!(W));
                                        If TR2!(W) + TR4!(W) = 0 Then Locate , 75: Print "  0.0";
                                        If TR2!(W) + TR4!(W) > 0 Then Locate , 75: Print Using "###.#"; (TR2!(W) + TR4!(W)) / (THW!(W) + THL!(W) + TAW!(W) + TAL!(W))

                                    End If

                                Next W

                                'Locate CsrLin + 1, 1
                                Print
                                Color 3: Print Tab(32); Chr$(179); Tab(56); Chr$(179)

                                THWW = 0
                                THLL = 0
                                TAWW = 0
                                TALL = 0

                                For I = 1 To DIVS_PER_CONFERENCE
                                    THWW = THWW + THW!(I)
                                    THLL = THLL + THL!(I)
                                    TAWW = TAWW + TAW!(I)
                                    TALL = TALL + TAL!(I)
                                Next

                                TR11 = 0
                                TR22 = 0
                                TR33 = 0
                                TR44 = 0

                                For I = 1 To DIVS_PER_CONFERENCE
                                    TR11 = TR11 + TR1!(I)
                                    TR22 = TR22 + TR2!(I)
                                    TR33 = TR33 + TR3!(I)
                                    TR44 = TR44 + TR4!(I)
                                Next

                                Color 11: Print "TOTALS";
                                Locate , 9: Print Using "##"; THWW;
                                Locate , 12: Print Using "##"; THLL;

                                If THWW = THWW + THLL And THWW <> 0 Then Locate , 15: Print "1.000";
                                If THWW = 0 Then Locate , 16: Print ".000";
                                If THWW > 0 And THWW < THWW + THLL Then Locate , 16: Print Using ".###"; THWW / (THWW + THLL);
                                If TR11 = 0 Then Locate , 21: Print "  0.0";
                                If TR11 > 0 Then Locate , 21: Print Using "###.#"; TR11 / (THWW + THLL);
                                If TR22 = 0 Then Locate , 27: Print "  0.0";
                                If TR22 > 0 Then Locate , 27: Print Using "###.#"; TR22 / (THWW + THLL);

                                Color 3: Print Tab(32); Chr$(179);
                                Color 11: Locate , 33: Print Using "##"; TAWW;
                                Locate , 36: Print Using "##"; TALL;

                                If TAWW = TAWW + TALL And TAWW <> 0 Then Locate , 39: Print "1.000";
                                If TAWW = 0 Then Locate , 40: Print ".000";
                                If TAWW > 0 And TAWW < TAWW + TALL Then Locate , 40: Print Using ".###"; TAWW / (TAWW + TALL);
                                If TR33 = 0 Then Locate , 45: Print "  0.0";
                                If TR33 > 0 Then Locate , 45: Print Using "###.#"; TR33 / (TAWW + TALL);
                                If TR44 = 0 Then Locate , 51: Print "  0.0";
                                If TR44 > 0 Then Locate , 51: Print Using "###.#"; TR44 / (TAWW + TALL);

                                Color 3: Print Tab(56); Chr$(179);
                                Color 11: Locate , 57: Print Using "##"; THWW + TAWW;
                                Locate , 60: Print Using "##"; THLL + TALL;

                                If THWW + TAWW = THWW + TAWW + THLL + TALL And THWW + TAWW <> 0 Then Locate , 63: Print "1.000";
                                If THWW + TAWW = 0 Then Locate , 64: Print ".000";
                                If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + THLL + TALL Then Locate , 64: Print Using ".###"; (THWW + TAWW) / (THWW + TAWW + THLL + TALL);
                                If TR11 + TR33 = 0 Then Locate , 69: Print "  0.0";
                                If TR11 + TR33 > 0 Then Locate , 69: Print Using "###.#"; (TR11 + TR33) / (THWW + THLL + TAWW + TALL);
                                If TR22 + TR44 = 0 Then Locate , 75: Print "  0.0";
                                If TR22 + TR44 > 0 Then Locate , 75: Print Using "###.#"; (TR22 + TR44) / (THWW + THLL + TAWW + TALL)
                                I$ = GetKeyPress$

                            End If 'Done checking if we are sending to printer

                        End If 'Done checking league file

                    End If 'Done checking stat file

                Case 2: Cls

            End Select

        End If 'Done checking for ESC

    Loop Until Selection% = 2 Or ExitCode% = 27 Or LG$ = ""

End Sub

'----------------------------------------
'     PrintHD2HD Subroutine
'----------------------------------------
'... explain here ...
Sub PrintHD2HD ()

    Shared hd2hdAL!(), AW!(), HL!(), HW!(), R1!(), R2!(), R3!(), R4!()
    Shared TAL!(), TAW!(), THL!(), THW!()
    Shared TR1!(), TR2!(), TR3!(), TR4!()

    Dim DV$(1 To 4)

    Color 0, 15

    DV$(1) = D1$
    DV$(2) = D2$
    DV$(3) = D3$
    DV$(4) = D4$

    LPrint Tab(55); Z$
    LPrint
    LPrint Tab(22); "--------- HOME ----------"; Tab(54); "--------- AWAY ----------"; Tab(86); "---------- TOTALS ----------"
    'LPrint Tab(15); "--------- HOME ----------"; Tab(36); "--------- AWAY ----------"; Tab(58); "---------- TOTALS ----------"

    LPrint Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "PF/G"; Tab(43); "PA/G"; Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "PF/G"; Tab(75); "PA/G"; Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "PF/G"; Tab(110); "PA/G"
    'LPrint Tab(15); "W"; Tab(17); "L"; Tab(21); "PCT."; Tab(26); "PF/G"; Tab(32); "PA/G"; Tab(37); "W"; Tab(39); "L"; Tab(43); "PCT."; Tab(48); "PF/G"; Tab(54); "PA/G"; Tab(59); "W"; Tab(61); "L"; Tab(65); "PCT."; Tab(70); "PF/G"; Tab(76); "PA/G"

    Do
        RDD = RDD + 1

        Select Case RDD
            Case 1:
                DD = 1: EE = 10
                If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then Exit Do
            Case 2:
                DD = 11: EE = 20
                If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then Exit Do
            Case 3:
                DD = 21: EE = 30
                If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then Exit Do
            Case 4:
                DD = 31: EE = 40
                If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then Exit Do
        End Select

        LPrint Tab(2); DV$(RDD)
        For K = DD To EE

            If Z1$(K) <> "XXX" And Z1$(K) <> Q$(0) Then
                LPrint Z1$(K);
                LPrint Tab(22); Using "#"; HW!(K);
                LPrint Tab(26); Using "#"; HL!(K);

                If HW!(K) = HW!(K) + HL!(K) And HW!(K) <> 0 Then LPrint Tab(28); "1.000";
                If HW!(K) = 0 Then LPrint Tab(29); ".000";
                If HW!(K) > 0 And HW!(K) < HW!(K) + HL!(K) Then LPrint Tab(29); Using ".###"; (HW!(K) / (HW!(K) + HL!(K)));
                If R1!(K) = 0 Then LPrint Tab(35); "  0.0";
                If R1!(K) > 0 Then LPrint Tab(35); Using "###.#"; R1!(K) / (HW!(K) + HL!(K));
                If R2!(K) = 0 Then LPrint Tab(42); "  0.0";
                If R2!(K) > 0 Then LPrint Tab(42); Using "###.#"; R2!(K) / (HW!(K) + HL!(K));

                LPrint Tab(54); Using "#"; AW!(K);
                LPrint Tab(58); Using "#"; hd2hdAL!(K);

                If AW!(K) = AW!(K) + hd2hdAL!(K) And AW!(K) <> 0 Then LPrint Tab(60); "1.000";
                If AW!(K) = 0 Then LPrint Tab(61); ".000";
                If AW!(K) > 0 And AW!(K) < AW!(K) + hd2hdAL!(K) Then LPrint Tab(61); Using ".###"; (AW!(K) / (AW!(K) + hd2hdAL!(K)));
                If R3!(K) = 0 Then LPrint Tab(67); "  0.0";
                If R3!(K) > 0 Then LPrint Tab(67); Using "###.#"; R3!(K) / (AW!(K) + hd2hdAL!(K));
                If R4!(K) = 0 Then LPrint Tab(74); "  0.0";
                If R4!(K) > 0 Then LPrint Tab(74); Using "###.#"; R4!(K) / (AW!(K) + hd2hdAL!(K));

                LPrint Tab(86); Using "##"; HW!(K) + AW!(K);
                LPrint Tab(91); Using "##"; HL!(K) + hd2hdAL!(K);

                If HW!(K) + AW!(K) = HW!(K) + AW!(K) + HL!(K) + hd2hdAL!(K) And HW!(K) + AW!(K) <> 0 Then LPrint Tab(95); "1.000";
                If (HW!(K) + AW!(K)) = 0 Then LPrint Tab(96); ".000";
                If (HW!(K) + AW!(K)) > 0 And (HW!(K) + AW!(K)) < (HW!(K) + AW!(K) + HL!(K) + hd2hdAL!(K)) Then LPrint Tab(96); Using ".###"; ((HW!(K) + AW!(K)) / (HW!(K) + AW!(K) + HL!(K) + hd2hdAL!(K)));
                If R1!(K) + R3!(K) = 0 Then LPrint Tab(102); "  0.0";
                If R1!(K) + R3!(K) > 0 Then LPrint Tab(102); Using "###.#"; (R1!(K) + R3!(K)) / (HW!(K) + HL!(K) + AW!(K) + hd2hdAL!(K));
                If R2!(K) + R4!(K) = 0 Then LPrint Tab(109); "  0.0"
                If R2!(K) + R4!(K) > 0 Then LPrint Tab(109); Using "###.#"; (R2!(K) + R4!(K)) / (HW!(K) + HL!(K) + AW!(K) + hd2hdAL!(K))

                THW!(RDD) = THW!(RDD) + HW!(K)
                THL!(RDD) = THL!(RDD) + HL!(K)
                TAW!(RDD) = TAW!(RDD) + AW!(K)
                TAL!(RDD) = TAL!(RDD) + hd2hdAL!(K)
                TR1!(RDD) = TR1!(RDD) + R1!(K)
                TR2!(RDD) = TR2!(RDD) + R2!(K)
                TR3!(RDD) = TR3!(RDD) + R3!(K)
                TR4!(RDD) = TR4!(RDD) + R4!(K)
            End If
        Next K

        LPrint Tab(5); "TOTALS";
        LPrint Tab(21); Using "##"; THW!(RDD);
        LPrint Tab(25); Using "##"; THL!(RDD);

        If THW!(RDD) = THW!(RDD) + THL!(RDD) And THW!(RDD) <> 0 Then LPrint Tab(28); "1.000";
        If THW!(RDD) = 0 Then LPrint Tab(29); ".000";
        If THW!(RDD) > 0 And THW!(RDD) < THW!(RDD) + THL!(RDD) Then LPrint Tab(29); Using ".###"; (THW!(RDD) / (THW!(RDD) + THL!(RDD)));
        If TR1!(RDD) = 0 Then LPrint Tab(35); "  0.0";
        If TR1!(RDD) > 0 Then LPrint Tab(35); Using "###.#"; TR1!(RDD) / (THW!(RDD) + THL!(RDD));
        If TR2!(RDD) = 0 Then LPrint Tab(42); "  0.0";
        If TR2!(RDD) > 0 Then LPrint Tab(42); Using "###.#"; TR2!(RDD) / (THW!(RDD) + THL!(RDD));

        LPrint Tab(53); Using "##"; TAW!(RDD);
        LPrint Tab(57); Using "##"; TAL!(RDD);

        If TAW!(RDD) = TAW!(RDD) + TAL!(RDD) And TAW!(RDD) <> 0 Then LPrint Tab(60); "1.000";
        If TAW!(RDD) = 0 Then LPrint Tab(61); ".000";
        If TAW!(RDD) > 0 And TAW!(RDD) < TAW!(RDD) + TAL!(RDD) Then LPrint Tab(61); Using ".###"; (TAW!(RDD) / (TAW!(RDD) + TAL!(RDD)));
        If TR3!(RDD) = 0 Then LPrint Tab(67); "  0.0";
        If TR3!(RDD) > 0 Then LPrint Tab(67); Using "###.#"; TR3!(RDD) / (TAW!(RDD) + TAL!(RDD));
        If TR4!(RDD) = 0 Then LPrint Tab(74); "  0.0";
        If TR4!(RDD) > 0 Then LPrint Tab(74); Using "###.#"; TR4!(RDD) / (TAW!(RDD) + TAL!(RDD));

        LPrint Tab(84); Using "###"; THW!(RDD) + TAW!(RDD);
        LPrint Tab(89); Using "###"; THL!(RDD) + TAL!(RDD);

        If THW!(RDD) + TAW!(RDD) = THW!(RDD) + TAW!(RDD) + THL!(RDD) + TAL!(RDD) And THW!(RDD) + TAW!(RDD) <> 0 Then LPrint Tab(95); "1.000";
        If THW!(RDD) + TAW!(RDD) = 0 Then LPrint Tab(96); ".000";
        If THW!(RDD) + TAW!(RDD) > 0 And THW!(RDD) + TAW!(RDD) < THW!(RDD) + TAW!(RDD) + THL!(RDD) + TAL!(RDD) Then LPrint Tab(96); Using ".###"; ((THW!(RDD) + TAW!(RDD)) / (THW!(RDD) + TAW!(RDD) + THL!(RDD) + TAL!(RDD)));
        If TR1!(RDD) + TR3!(RDD) = 0 Then LPrint Tab(102); "  0.0";
        If TR1!(RDD) + TR3!(RDD) > 0 Then LPrint Tab(102); Using "###.#"; (TR1!(RDD) + TR3!(RDD)) / (THW!(RDD) + THL!(RDD) + TAW!(RDD) + TAL!(RDD));
        If TR2!(RDD) + TR4!(RDD) = 0 Then LPrint Tab(109); "  0.0"
        If TR2!(RDD) + TR4!(RDD) > 0 Then LPrint Tab(109); Using "###.#"; (TR2!(RDD) + TR4!(RDD)) / (THW!(RDD) + THL!(RDD) + TAW!(RDD) + TAL!(RDD))
        LPrint

    Loop Until RDD = 4

    LPrint
    LPrint
    LPrint Tab(22); "--------- HOME ----------"; Tab(54); "--------- AWAY ----------"; Tab(86); "---------- TOTALS ----------"
    LPrint Tab(22); "W"; Tab(26); "L"; Tab(30); "PCT."; Tab(36); "PF/G"; Tab(43); "PA/G"; Tab(54); "W"; Tab(58); "L"; Tab(62); "PCT."; Tab(68); "PF/G"; Tab(75); "PA/G"; Tab(86); "W"; Tab(91); "L"; Tab(97); "PCT."; Tab(103); "PF/G"; Tab(110); "PA/G"
    LPrint

    For W = 1 To DIVS_PER_CONFERENCE

        If DV$(W) <> "XXX" And DV$(W) <> "" Then
            LPrint Tab(2); DV$(W);
            LPrint Tab(21); Using "##"; THW!(W);
            LPrint Tab(25); Using "##"; THL!(W);
            If THW!(W) = THW!(W) + THL!(W) And THW!(W) <> 0 Then LPrint Tab(28); "1.000";
            If THW!(W) = 0 Then LPrint Tab(29); ".000";
            If THW!(W) > 0 And THW!(W) < THW!(W) + THL!(W) Then LPrint Tab(29); Using ".###"; (THW!(W) / (THW!(W) + THL!(W)));
            If TR1!(W) = 0 Then LPrint Tab(35); "  0.0";
            If TR1!(W) > 0 Then LPrint Tab(35); Using "###.#"; TR1!(W) / (THW!(W) + THL!(W));
            If TR2!(W) = 0 Then LPrint Tab(42); "  0.0";
            If TR2!(W) > 0 Then LPrint Tab(42); Using "###.#"; TR2!(W) / (THW!(W) + THL!(W));
            LPrint Tab(53); Using "##"; TAW!(W);
            LPrint Tab(57); Using "##"; TAL!(W);
            If TAW!(W) = TAW!(W) + TAL!(W) And TAW!(W) <> 0 Then LPrint Tab(60); "1.000";
            If TAW!(W) = 0 Then LPrint Tab(61); ".000";
            If TAW!(W) > 0 And TAW!(W) < TAW!(W) + TAL!(W) Then LPrint Tab(61); Using ".###"; (TAW!(W) / (TAW!(W) + TAL!(W)));
            If TR3!(W) = 0 Then LPrint Tab(67); "  0.0";
            If TR3!(W) > 0 Then LPrint Tab(67); Using "###.#"; TR3!(W) / (TAW!(W) + TAL!(W));
            If TR4!(W) = 0 Then LPrint Tab(74); "  0.0";
            If TR4!(W) > 0 Then LPrint Tab(74); Using "###.#"; TR4!(W) / (TAW!(W) + TAL!(W));
            LPrint Tab(84); Using "###"; THW!(W) + TAW!(W);
            LPrint Tab(89); Using "###"; THL!(W) + TAL!(W);
            If THW!(W) + TAW!(W) = THW!(W) + TAW!(W) + THL!(W) + TAL!(W) And THW!(W) + TAW!(W) <> 0 Then LPrint Tab(95); "1.000";
            If (THW!(W) + TAW!(W)) = 0 Then LPrint Tab(96); ".000";
            If (THW!(W) + TAW!(W)) > 0 And THW!(W) + TAW!(W) < THW!(W) + TAW!(W) + THL!(W) + TAL!(W) Then LPrint Tab(96); Using ".###"; ((THW!(W) + TAW!(W)) / (THW!(W) + TAW!(W) + THL!(W) + TAL!(W)));
            If TR1!(W) + TR3!(W) = 0 Then LPrint Tab(102); "  0.0";
            If TR1!(W) + TR3!(W) > 0 Then LPrint Tab(102); Using "###.#"; (TR1!(W) + TR3!(W)) / (THW!(W) + THL!(W) + TAW!(W) + TAL!(W));
            If TR2!(W) + TR4!(W) = 0 Then LPrint Tab(109); "  0.0";
            If TR2!(W) + TR4!(W) > 0 Then LPrint Tab(109); Using "###.#"; (TR2!(W) + TR4!(W)) / (THW!(W) + THL!(W) + TAW!(W) + TAL!(W))
        End If

    Next W

    THWW = 0: THLL = 0: TAWW = 0: TALL = 0

    For I = 1 To DIVS_PER_CONFERENCE
        THWW = THWW + THW!(I): THLL = THLL + THL!(I): TAWW = TAWW + TAW!(I): TALL = TALL + TAL!(I)
    Next

    TR11 = 0: TR22 = 0: TR33 = 0: TR44 = 0

    For I = 1 To DIVS_PER_CONFERENCE
        TR11 = TR11 + TR1!(I): TR22 = TR22 + TR2!(I): TR33 = TR33 + TR3!(I): TR44 = TR44 + TR4!(I)
    Next

    LPrint Tab(5); "TOTALS";
    LPrint Tab(20); Using "###"; THWW;
    LPrint Tab(24); Using "###"; THLL;

    If THWW = THWW + THLL And THWW <> 0 Then LPrint Tab(28); "1.000";
    If THWW = 0 Then LPrint Tab(29); ".000";
    If THWW > 0 And THWW < THWW + THLL Then LPrint Tab(29); Using ".###"; THWW / (THWW + THLL);
    If TR11 = 0 Then LPrint Tab(35); "  0.0";
    If TR11 > 0 Then LPrint Tab(35); Using "###.#"; TR11 / (THWW + THLL);
    If TR22 = 0 Then LPrint Tab(42); "  0.0";
    If TR22 > 0 Then LPrint Tab(42); Using "###.#"; TR22 / (THWW + THLL);

    LPrint Tab(52); Using "###"; TAWW;
    LPrint Tab(56); Using "###"; TALL;

    If TAWW = TAWW + TALL And TAWW <> 0 Then LPrint Tab(60); "1.000";
    If TAWW = 0 Then LPrint Tab(61); ".000";
    If TAWW > 0 And TAWW < TAWW + TALL Then LPrint Tab(61); Using ".###"; TAWW / (TAWW + TALL);
    If TR33 = 0 Then LPrint Tab(67); "  0.0";
    If TR33 > 0 Then LPrint Tab(67); Using "###.#"; TR33 / (TAWW + TALL);
    If TR44 = 0 Then LPrint Tab(74); "  0.0";
    If TR44 > 0 Then LPrint Tab(74); Using "###.#"; TR44 / (TAWW + TALL);

    LPrint Tab(84); Using "###"; THWW + TAWW;
    LPrint Tab(89); Using "###"; THLL + TALL;

    If THWW + TAWW = THWW + TAWW + THLL + TALL And THWW + TAWW <> 0 Then LPrint Tab(95); "1.000";
    If THWW + TAWW = 0 Then LPrint Tab(96); ".000";
    If THWW + TAWW > 0 And THWW + TAWW < THWW + TAWW + THLL + TALL Then LPrint Tab(96); Using ".###"; (THWW + TAWW) / (THWW + TAWW + THLL + TALL);
    If TR11 + TR33 = 0 Then LPrint Tab(102); "  0.0";
    If TR11 + TR33 > 0 Then LPrint Tab(102); Using "###.#"; (TR11 + TR33) / (THWW + THLL + TAWW + TALL);
    If TR22 + TR44 = 0 Then LPrint Tab(109); "  0.0";
    If TR22 + TR44 > 0 Then LPrint Tab(109); Using "###.#"; (TR22 + TR44) / (THWW + THLL + TAWW + TALL)

    LPrint Chr$(12)

    Color 15, 0

End Sub

'----------------------------------------
'     GetTeam_HD2HD Subroutine
'----------------------------------------
'... explain here ...
Sub GetTeam_HD2HD (TM$)

    Shared hd2hdT!()

    Do
        Color 15, 0: Cls
        JJ = 6
        Locate 6, 8: Color 15, 4
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "ESC TO RETURN TO MAIN MENU"
        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then
            If I$ = Chr$(13) Then
                Call SelectTeam_HD2HD(NY$, numberTeams, TM$)
            Else
                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If NY$ = "" And yearNumber$ <> "" Then
                    'NY$ = yearNumber$
                    Call SelectTeam_HD2HD(yearNumber$, numberTeams, TM$)
                Else
                    yearNumber$ = NY$
                    Erase teamNames$
                    Call ReadTeamData_HD2HD(yearNumber$, numberTeams)
                    Call SelectTeam_HD2HD(yearNumber$, numberTeams, TM$)
                End If

            End If

            If BO% = 1 Then
                BO% = 0
            Else
                Locate 2, 33
                Color hd2hdT!(33), hd2hdT!(34): Print QQ$(1)

                Color 15, 4

                Do
                    Locate 3, 33: Print "ANY CHANGE (YN)";
                    A$ = GetKeyPress$
                Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

            End If

        End If

    Loop Until UCase$(A$) = "N" Or I$ = Chr$(27)

End Sub

'----------------------------------------
'     SelectLeague_HD2HD Subroutine
'----------------------------------------
'... explain here ...
Sub SelectLeague_HD2HD (targetFile$)

    Do
        Spec$ = diskPaths$(1) + "*.LIB"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else
            ReDim leagueFiles$(0 To Count%)
            For X = 1 To Count%
                leagueFiles$(X) = Space$(12)
            Next
            leagueFiles$(0) = Spec$
            Call ReadFile(leagueFiles$())

            Do
                Cls
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT LEAGUE": Color 15
                Selection% = 1
                Start% = 1
                FileFGColr% = 15: FileBGColr% = 1
                HiLiteFGColr% = 0: HiLiteBGColr% = 7
                Rows% = 20
                Columns% = 4
                Spacing% = 6
                Row% = 4
                Column% = 1
                Locate 25, 1
                Color 14
                Print "ENTER";: Color 15
                Print "-SELECTS LEAGUE";

                _MouseShow "DEFAULT"
                MAMenu leagueFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then
                    Color 15, 1
                    targetFile$ = RTrim$(leagueFiles$(Selection%))
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate 3, 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then
                targetFile$ = Left$(targetFile$, Len(targetFile$) - 4)
            End If

        End If

    Loop Until ExitCode% = 27 Or Count% = 0 Or UCase$(I$) = "N"

End Sub

'----------------------------------------
'       ReadTeamData_HD2HD Subroutine
'----------------------------------------
'...explanation...
Sub ReadTeamData_HD2HD (yearNumber$, numberTeams)

    numberTeams = 0

    Open diskPaths$(0) + "PROBBTMS." + yearNumber$ For Random As #1 Len = 1031

    Y = LOF(1)

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

    For I1 = 1 To Y / 1031
        Get #1, I1
        CK% = CVI(Q$(9))
        If CK% = 999 Then
            numberTeams = numberTeams + 1
            teamNames$(numberTeams) = Q$(0)
            teamIndex%(numberTeams) = I1
        End If

    Next
    Close 1
    
End Sub

'----------------------------------------
'       SelectTeam_HD2HD Subroutine
'----------------------------------------
'...explanation...

Sub SelectTeam_HD2HD (yearNumber$, numberTeams, TM$)

    Shared hd2hdT!()

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

        T1 = teamIndex%(Selection%)
        Open diskPaths$(0) + "PROBBTMS." + yearNumber$ For Random As #1 Len = 1031

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
        Get #1, T1

        TM$ = Q$(0)
        QQ$(1) = RTrim$(Q$(0))

        For I = 0 To 34: hd2hdT!(I) = CVI(Q$(I + 10)): Next

        Close 1

        Q$(0) = QQ$(1)

    Else
        BO% = 1
    End If

End Sub
