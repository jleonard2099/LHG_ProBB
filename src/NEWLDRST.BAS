'----------------------------------------
'     NewStandingOptions Subroutine
'----------------------------------------
'This subroutine lets the user choose a
'league for viewing "Expanded" standings,
'then reads in the data from the League
'stats file (.TMS). It then presents the
'display options and calls separate routines
'for handling each choice.
Sub NewStandingOptions ()

    _Title "Basketball: The Pro Game - League Standings"

    Cls

    Do
        Locate 15, 10
        Color 12: Print "V";: Color 7: Print "iew or ";
        Color 12: Print "P";: Color 7: Print "rint"
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V"

    'I started passing league$ as a parameter
    'To start working towards removing the need
    'for Dim Shared in so many places. However
    'it has not been consistently done, so I
    'realize it's currently awkward there.
    Select Case UCase$(I$)

        Case "V":
            Call ViewNewStandings(league$)

        Case "P"
            Call PrintNewStandings(league$)

    End Select

End Sub


'----------------------------------------
'    UpdateLeagueStandings Subroutine
'----------------------------------------
'This subroutine takes the stats read in
'for a league, then calculates the standings
'for that league and writes a  corresponding
'.STD file.
Sub UpdateLeagueStandings (league$)

    Shared HLL(), HWW()
    Shared LL(), LLS()
    Shared LTL(), LTW(), LWS()
    Shared RLL(), RWW()
    Shared TLS(), TWS(), WW()

    Shared statO%()

    Shared statsO$(), statsH$()
    Shared TML$(), statsZ1$()

    Dim statsT%(NUM_STATRECORDS)

    Shared div1$, div2$, div3$, div4$, league$

    TMR = 40
    YY = 0

    For KK = 1 To TEAMS_PER_CONFERENCE

        YY = YY + 1
        Color 14
        Locate 7, 39
        TMR = TMR - 1: Print Using "##"; TMR

        If statsZ1$(KK) <> "*" Then

            TML$(YY) = statsZ1$(KK)

            Color 15, 0
            Cls

            Locate 5: Print "COMPUTING RECORDS . . . . . . PLEASE WAIT"

            If _FileExists(diskPaths$(1) + statsZ1$(KK) + "S") Then

                Open diskPaths$(1) + statsZ1$(KK) + "S" For Input As #2

                Input #2, XX%

                For J = 1 to NUM_STATRECORDS: Input #2, statO%(J): Next
                For J = 1 to NUM_STATRECORDS: Input #2, statsO$(J): Next
                For J = 1 to NUM_STATRECORDS: Input #2, statsT%(J): Next
                For J = 1 to NUM_STATRECORDS: Input #2, statsH$(J): Next

                Close #2

                LWS(YY) = 0
                LLS(YY) = 0
                TWS(YY) = 0
                TLS(YY) = 0

                For I = 1 To XX%
                    If statO%(I) > statsT%(I) Then WW(YY) = WW(YY) + 1
                    If statO%(I) > statsT%(I) And statsH$(I) = "V" Then RWW(YY) = RWW(YY) + 1
                    If statO%(I) > statsT%(I) And statsH$(I) = "V1" Then RWW(YY) = RWW(YY) + 1
                    If statO%(I) > statsT%(I) And statsH$(I) = "V2" Then RWW(YY) = RWW(YY) + 1
                    If statO%(I) > statsT%(I) And statsH$(I) = "H" Then HWW(YY) = HWW(YY) + 1
                    If statO%(I) > statsT%(I) And statsH$(I) = "H1" Then HWW(YY) = HWW(YY) + 1
                    If statO%(I) > statsT%(I) And statsH$(I) = "H2" Then HWW(YY) = HWW(YY) + 1
                    If statsT%(I) > statO%(I) Then LL(YY) = LL(YY) + 1
                    If statsT%(I) > statO%(I) And statsH$(I) = "V" Then RLL(YY) = RLL(YY) + 1
                    If statsT%(I) > statO%(I) And statsH$(I) = "V1" Then RLL(YY) = RLL(YY) + 1
                    If statsT%(I) > statO%(I) And statsH$(I) = "V2" Then RLL(YY) = RLL(YY) + 1
                    If statsT%(I) > statO%(I) And statsH$(I) = "H" Then HLL(YY) = HLL(YY) + 1
                    If statsT%(I) > statO%(I) And statsH$(I) = "H1" Then HLL(YY) = HLL(YY) + 1
                    If statsT%(I) > statO%(I) And statsH$(I) = "H2" Then HLL(YY) = HLL(YY) + 1
                    If statO%(I) > statsT%(I) Then LWS(YY) = LWS(YY) + 1: LLS(YY) = 0
                    If LWS(YY) > TWS(YY) Then TWS(YY) = LWS(YY)
                    If statO%(I) < statsT%(I) Then LLS(YY) = LLS(YY) + 1: LWS(YY) = 0
                    If LLS(YY) > TLS(YY) Then TLS(YY) = LLS(YY)
                Next I

                If XX% <= 10 Then
                    LTL(YY) = LL(YY): LTW(YY) = WW(YY)
                Else
                    M = XX% - 9

                    For J = M To XX%
                        If statO%(J) > statsT%(J) Then LTW(YY) = LTW(YY) + 1
                        If statsT%(J) > statO%(J) Then LTL(YY) = LTL(YY) + 1
                    Next J
                End If

            End If

        End If 'Done checking for *

    Next KK

    Open diskPaths$(1) + league$ + ".STD" For Output As #3

    Write #3, league$, div1$, div2$, div3$, div4$

    For I = 1 To TEAMS_PER_CONFERENCE
        Write #3, WW(I), HWW(I), RWW(I), LL(I), HLL(I), RLL(I), LTW(I), LTL(I), TML$(I)
    Next

    Close #3

End Sub

'----------------------------------------
'       ViewNewStandings Subroutine
'----------------------------------------
'This subroutine sorts the expanded league
'standings and outputs them to the user
'display in a formatted view
Sub ViewNewStandings (league$)

    Shared HLL(), HWW()
    Shared LL(), LLS()
    Shared LTL(), LTW(), LWS()
    Shared RLL(), RWW()
    Shared TLS(), TWS(), WW()

    Shared statO%()

    Shared TML$()

    Shared div1$, div2$, div3$, div4$

    MJ = 0

    For NT = 1 To 4

        AA = (NT * 10) - 9
        ZZ = NT * 10

        Call SortStandings(AA, ZZ)

        Cls

        Color 11
        Locate 1, 35: Print league$
        Color 14
        Locate 2: Print Tab(20); "                     MAJ          HOME    ROAD   LAST 10"
        Locate 3

        If NT = 1 Then Print div1$;
        If NT = 2 Then Print div2$;
        If NT = 3 Then Print div3$;
        If NT = 4 Then Print div4$;

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
'       PrintNewStandings Subroutine
'----------------------------------------
'This subroutine sorts the expanded league
'standings and outputs them to the default
'printer in a formatted view
'...explanation...
Sub PrintNewStandings (league$)

    Shared HLL(), HWW()
    Shared LL(), LLS()
    Shared LTL(), LTW(), LWS()
    Shared RLL(), RWW()
    Shared TLS(), TWS(), WW()

    Shared statO%()

    Shared TML$()

    Shared div1$, div2$, div3$, div4$

    MJ = 0

    For NT = 1 To 4

        AA = (NT * 10) - 9
        ZZ = NT * 10

        Call SortStandings(AA, ZZ)

        LPrint league$
        LPrint Tab(20); "                     MAJ          HOME    ROAD   LAST 10"

        If NT = 1 Then LPrint div1$;
        If NT = 2 Then LPrint div2$;
        If NT = 3 Then LPrint div3$;
        If NT = 4 Then LPrint div4$;

        LPrint Tab(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "

        For I = AA To ZZ

            If TML$(I) <> "" And TML$(I) <> "*" Then

                LPrint TML$(I);
                LPrint Tab(18); Using "### ### ###"; WW(I) + LL(I); WW(I); LL(I); Tab(30);

                'PCT
                If LL(I) = 0 And LL(I) + WW(I) <> 0 Then
                    LPrint "1.000";
                Else
                    If WW(I) = 0 Then
                        LPrint " .000";
                    Else
                        If WW(I) + LL(I) = 0 Then
                            LPrint " .000";
                        Else
                            wlPCT! = WW(I) / (WW(I) + LL(I))
                            LPrint Using " .###"; wlPCT!;
                        End If
                    End If
                End If

                'GB
                If ((WW(AA) - WW(I)) + (LL(I) - LL(AA))) / 2 = 0 Then
                    LPrint Tab(36); " ---";
                Else
                    LPrint Tab(36); Using "##.#"; ((WW(AA) - WW(I)) + (LL(I) - LL(AA))) / 2;
                End If

                'MJ
                If I = AA Then
                    MJ = 83 - WW(AA) - LL(AA + 1)
                    If MJ < 0 Then MJ = 0
                    LPrint Tab(42); Using "##"; MJ;
                End If

                LPrint Tab(45); Using "##"; TWS(I); Tab(49); TLS(I);
                LPrint Tab(53); Using "##"; HWW(I); Tab(56); HLL(I); Tab(61); RWW(I); Tab(64); RLL(I); Tab(70); LTW(I); Tab(73); LTL(I)

            End If

        Next

        LPrint
        LPrint

    Next NT

    LPrint Chr$(12)

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

