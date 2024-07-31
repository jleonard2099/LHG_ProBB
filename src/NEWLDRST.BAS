
'----------------------------------------
'    UpdateExpandedStandings Subroutine
'----------------------------------------
'This subroutine takes the stats read in
'for a league, then calculates the standings
'for that league and writes a  corresponding
'.STD file.
Sub UpdateExpandedStandings (league$)

    Shared HLL(), HWW()
    Shared LL(), LLS()
    Shared LTL(), LTW(), LWS()
    Shared RLL(), RWW()
    Shared TLS(), TWS(), WW()

    Shared teamScore(), oppScore()

    Shared locIndicator$(), oppName$()
    Shared memberTeams$(), TML$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$, league$

    TMR = 40
    YY = 0

    For KK = 1 To TEAMS_PER_LEAGUE

        YY = YY + 1
        Color 14
        Locate 7, 39
        TMR = TMR - 1
        Print Using "##"; TMR

        If memberTeams$(KK) <> "*" Then

            TML$(YY) = memberTeams$(KK)

            Color 15, 0
            Cls

            Locate 5: Print "COMPUTING RECORDS . . . . . . PLEASE WAIT"

            If _FileExists(diskPaths$(1) + memberTeams$(KK) + "S") Then

                Open diskPaths$(1) + memberTeams$(KK) + "S" For Input As #2

                Input #2, XX%

                For J = 1 to MAX_SCHED_STATS: 
                    Input #2, teamScore(J)
                Next
                
                For J = 1 to MAX_SCHED_STATS: 
                    Input #2, oppName$(J)
                Next
                
                For J = 1 to MAX_SCHED_STATS: 
                    Input #2, oppScore(J)
                Next
                
                For J = 1 to MAX_SCHED_STATS:
                    Input #2, locIndicator$(J)
                Next

                Close #2

                LWS(YY) = 0
                LLS(YY) = 0
                TWS(YY) = 0
                TLS(YY) = 0

                For I = 1 To XX%

                    If teamScore(I) > oppScore(I) Then
                        WW(YY) = WW(YY) + 1
                        LWS(YY) = LWS(YY) + 1
                        LLS(YY) = 0
                        If locIndicator$(I) = "V" Then RWW(YY) = RWW(YY) + 1
                        If locIndicator$(I) = "V1" Then RWW(YY) = RWW(YY) + 1
                        If locIndicator$(I) = "V2" Then RWW(YY) = RWW(YY) + 1
                        If locIndicator$(I) = "H" Then HWW(YY) = HWW(YY) + 1
                        If locIndicator$(I) = "H1" Then HWW(YY) = HWW(YY) + 1
                        If locIndicator$(I) = "H2" Then HWW(YY) = HWW(YY) + 1
                    End If

                    If teamScore(I) < oppScore(I) Then 
                        LL(YY) = LL(YY) + 1
                        LLS(YY) = LLS(YY) + 1
                        LWS(YY) = 0
                        If locIndicator$(I) = "V" Then RLL(YY) = RLL(YY) + 1
                        If locIndicator$(I) = "V1" Then RLL(YY) = RLL(YY) + 1
                        If locIndicator$(I) = "V2" Then RLL(YY) = RLL(YY) + 1
                        If locIndicator$(I) = "H" Then HLL(YY) = HLL(YY) + 1
                        If locIndicator$(I) = "H1" Then HLL(YY) = HLL(YY) + 1
                        If locIndicator$(I) = "H2" Then HLL(YY) = HLL(YY) + 1
                    End If

                    If LWS(YY) > TWS(YY) Then TWS(YY) = LWS(YY)
                    If LLS(YY) > TLS(YY) Then TLS(YY) = LLS(YY)

                Next I

                If XX% <= 10 Then
                    LTL(YY) = LL(YY)
                    LTW(YY) = WW(YY)
                Else
                    M = XX% - 9

                    For J = M To XX%
                        If teamScore(J) > oppScore(J) Then LTW(YY) = LTW(YY) + 1
                        If teamScore(J) < oppScore(J) Then LTL(YY) = LTL(YY) + 1
                    Next J
                End If

            End If

        End If 'Done checking for *

    Next KK

    Open diskPaths$(1) + league$ + ".STD" For Output As #3

    Write #3, league$, div1Name$, div2Name$, div3Name$, div4Name$

    For I = 1 To TEAMS_PER_LEAGUE
        Write #3, WW(I), HWW(I), RWW(I), LL(I), HLL(I), RLL(I), LTW(I), LTL(I), TML$(I)
    Next

    Close #3

End Sub

'----------------------------------------
'       NewStandings_Screen Subroutine
'----------------------------------------
'This subroutine sorts the expanded league
'standings and outputs them to the user
'display in a formatted view
Sub NewStandings_Screen (league$)

    Shared HLL(), HWW()
    Shared LL(), LLS()
    Shared LTL(), LTW(), LWS()
    Shared RLL(), RWW()
    Shared TLS(), TWS(), WW()

    Shared TML$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    MJ = 0

    For NT = 1 To 4

        AA = ((NT - 1) * 10) + 1
        ZZ = NT * 10

        Call SortStandings(AA, ZZ)

        Cls

        Color 11
        Locate 1, 35: Print league$
        Color 14
        Locate 2: Print Tab(20); "                     MAJ          HOME    ROAD   LAST 10"
        Locate 3

        Select Case NT

            Case 1
                Print #1, div1Name$;

            Case 2
                Print #1, div2Name$;

            Case 3
                Print #1, div3Name$;

            Case 4
                Print #1, div4Name$;

        End Select

        Print Tab(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "

        For I = AA To ZZ

            If TML$(I) <> "" And TML$(I) <> "*" Then

                Color 3
                Print TML$(I);
                Color 15

                'G / W / L
                Print Tab(18); Using "### ### ###"; WW(I) + LL(I); WW(I); LL(I); Tab(30);

                ' PCT
                If LL(I) = 0 And LL(I) + WW(I) <> 0 Then
                    Print "1.000";
                Else
                    If WW(I) = 0 Then
                        Print " .000";
                    Else
                        If WW(I) + LL(I) <> 0 Then
                            wlPCT! = WW(I) / (WW(I) + LL(I))
                            Print Using " .###"; wlPCT!;
                        Else
                            Print " .000";
                        End If
                    End If
                End If

                ' GB
                calcGB! = ((WW(AA) - WW(I)) + (LL(I) - LL(AA))) / 2
                If calcGB! = 0 Then
                    Print Tab(36); " ---";
                Else
                    Print Tab(36); Using "##.#"; calcGB!;
                End If

                Print Tab(45); Using "##"; TWS(I); Tab(49); TLS(I);

                ' Records: Home / Road / Last 10
                Print Tab(53); Using "##"; HWW(I); Tab(56); HLL(I); Tab(61); RWW(I); Tab(64); RLL(I); Tab(70); LTW(I); Tab(73); LTL(I)
            End If

        Next

        'MJ #
        MJ = 83 - WW(AA) - LL(AA + 1)
        If MJ < 0 Then MJ = 0

        Locate 4, 42
        Print Using "##"; MJ;

        Color 14
        Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$

    Next NT

End Sub


'----------------------------------------
'       NewStandings_Report Subroutine
'----------------------------------------
'This subroutine sorts the expanded league
'standings and outputs them to the default
'printer in a formatted view
Sub NewStandings_Report (printDest, destFile$, league$)

    Shared HLL(), HWW()
    Shared LL(), LLS()
    Shared LTL(), LTW(), LWS()
    Shared RLL(), RWW()
    Shared TLS(), TWS(), WW()

    Shared TML$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    MJ = 0

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    For NT = 1 To 4

        AA = ((NT - 1) * 10) + 1
        ZZ = NT * 10

        Call SortStandings(AA, ZZ)

        Print #1, league$
        Print #1, Tab(20); "                     MAJ          HOME    ROAD   LAST 10"

        Select Case NT

            Case 1
                Print #1, div1Name$;

            Case 2
                Print #1, div2Name$;

            Case 3
                Print #1, div3Name$;

            Case 4
                Print #1, div4Name$;

        End Select

        Print #1, Tab(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "

        For I = AA To ZZ

            If TML$(I) <> "" And TML$(I) <> "*" Then

                Print #1, TML$(I);
                Print #1, Tab(18); Using "### ### ###"; WW(I) + LL(I); WW(I); LL(I); Tab(30);

                'PCT
                If LL(I) = 0 And LL(I) + WW(I) <> 0 Then
                    Print #1, "1.000";
                Else
                    If WW(I) = 0 Then
                        Print #1, " .000";
                    Else
                        If WW(I) + LL(I) = 0 Then
                            Print #1, " .000";
                        Else
                            wlPCT! = WW(I) / (WW(I) + LL(I))
                            Print #1, Using " .###"; wlPCT!;
                        End If
                    End If
                End If

                'Games Behind?
                If ((WW(AA) - WW(I)) + (LL(I) - LL(AA))) / 2 = 0 Then
                    Print #1, Tab(36); " ---";
                Else
                    Print #1, Tab(36); Using "##.#"; ((WW(AA) - WW(I)) + (LL(I) - LL(AA))) / 2;
                End If

                'Magic Number?
                If I = AA Then
                    MJ = 83 - WW(AA) - LL(AA + 1)
                    If MJ < 0 Then MJ = 0
                    Print #1, Tab(42); Using "##"; MJ;
                End If

                Print #1, Tab(45); Using "##"; TWS(I); Tab(49); TLS(I);
                Print #1, Tab(53); Using "##"; HWW(I); Tab(56); HLL(I); Tab(61); RWW(I); Tab(64); RLL(I); Tab(70); LTW(I); Tab(73); LTL(I)

            End If

        Next

        Print #1,
        Print #1,

    Next NT

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
'         SortStandings Subroutine
'----------------------------------------
'This subroutine simply sorts the league
'standings records so that teams that are
'leading can be displayed first.
'This sorting is only done on the arrays
'holding the standing data, so no changes
'are permanent to the underlying files.
Sub SortStandings (AA, ZZ)

    Shared HLL(), HWW()
    Shared LL(), LLS()
    Shared LTL(), LTW(), LWS()
    Shared RLL(), RWW()
    Shared TLS(), TWS(), WW()

    Shared TML$()

    For X = AA To ZZ - 1

        For Y = X + 1 To ZZ

            diffX = WW(X) - LL(X)
            diffY = WW(Y) - LL(Y)

            If diffX <= diffY Then

                If diffX <> diffY Then

                    If Not (WW(X) <= WW(Y) And WW(X) > LL(X)) Or Not (WW(X) >= WW(Y) And WW(X) < LL(X)) Then

                        Swap TML$(X), TML$(Y)
                        Swap WW(X), WW(Y)
                        Swap LL(X), LL(Y)
                        Swap HLL(X), HLL(Y)
                        Swap RLL(X), RLL(Y)
                        Swap HWW(X), HWW(Y)
                        Swap RWW(X), RWW(Y)
                        Swap LTL(X), LTL(Y)
                        Swap LTW(X), LTW(Y)
                        Swap TWS(X), TWS(Y)
                        Swap TLS(X), TLS(Y)

                    End If

                End If

            End If

        Next Y

    Next X

End Sub

