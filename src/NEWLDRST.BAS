
Sub NEWLDRST ()

    Dim HLL(40), HWW(40), LL(40), LLS(40), LWS(40)
    Dim RLL(40), RWW(40), LTL(40), LTW(40)
    Dim TLS(40), TWS(40), WW(40)

    Dim O%(100), statsT%(100)

    Dim H$(100), O$(100), TML$(40), strZ1$(40)
    ReDim menuChoices$(1 To 2)

    Shared YY

    _Title "Basketball: The Pro Game - League Standings"

    2300 Cls
    Call TITLE

    YY = 0
    TMR = 40

    'Erase menuChoices$
    menuChoices$(1) = "VIEW EXPANDED LEAGUE STANDINGS"
    menuChoices$(2) = "RETURN TO LEAGUE LEADER MENU  "
    SELECTION% = 1
    Count% = 2
    Start% = 1
    SCANCODE% = 2
    FileFGColr% = 15: FileBGColr% = 1
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Spacing% = 1
    Row% = 8
    Column% = 25

    Call box0(7, 24, 10, 55, 2, 15, 1)
    _MouseShow "DEFAULT"
    MMenuVert menuChoices$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
    _MouseHide

    Color , 0
    If SCANCODE% = 27 Or SELECTION% = 2 Then Exit Sub

    2500 Spec$ = D$(1) + "*.TMS"

    1440 null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
    Count% = DirCount%
    If Count% = 0 Then
        Locate 10, 8
        Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
        Call NEWLDRST
    End If

    ReDim leagueFiles$(0 To Count%)
    For X = 1 To Count%
        leagueFiles$(X) = Space$(12)
    Next
    leagueFiles$(0) = Spec$
    Call ReadFile(leagueFiles$())
    'CALL SortStr(VARPTR(leagueFiles$(1)), Count%, 0)

    1445 Cls
    Color , 1
    Cls
    Locate , 28
    Color 14
    Print "SELECT LEAGUE": Color 15
    SELECTION% = 1
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
    MAMenu leagueFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% = 27 Then 2300
    targetFile$ = leagueFiles$(SELECTION%)
    Locate 2, 33: Print targetFile$
    Locate , 33: Print "ANY CHANGE (YN)";
    Color 15, 0
    3912 I$ = GetKeyPress$
    If UCase$(I$) = "Y" Then 1445



    3913 targetFile$ = RTrim$(targetFile$)
    targetFile$ = Left$(targetFile$, Len(targetFile$) - 4)
    LG$ = targetFile$

    Open D$(1) + targetFile$ + ".TMS" For Input As #2
    Input #2, LG$
    Input #2, D1$: Input #2, D2$: Input #2, D3$: Input #2, D4$
    For X = 1 To 40
        Input #2, strZ1$(X): TML$(X) = strZ1$(X)
    Next
    Close 2

    Cls

    For KK = 1 To 40
        'values of YY are ???
        YY = YY + 1
        Color 14: Locate 7, 20: TMR = TMR - 1: Print Using "##"; TMR
        If strZ1$(KK) = "*" Then 60

        10 TML$(YY) = strZ1$(KK)
        Locate 5: Color 15: Print "COMPUTING RECORDS . . . . . . PLEASE WAIT"
        If Not _FileExists(D$(1) + strZ1$(KK) + "S") Then 60
        Open D$(1) + strZ1$(KK) + "S" For Input As #1
        Input #1, XX%
        For J = 1 To 100: Input #1, O%(J): Next
        For J = 1 To 100: Input #1, O$(J): Next
        For J = 1 To 100: Input #1, statsT%(J): Next
        For J = 1 To 100: Input #1, H$(J): Next
        Close 1
        LWS(YY) = 0
        LLS(YY) = 0
        TWS(YY) = 0
        TLS(YY) = 0
        For I = 1 To XX%
            If O%(I) > statsT%(I) Then WW(YY) = WW(YY) + 1
            If O%(I) > statsT%(I) And H$(I) = "V" Then RWW(YY) = RWW(YY) + 1
            If O%(I) > statsT%(I) And H$(I) = "V1" Then RWW(YY) = RWW(YY) + 1
            If O%(I) > statsT%(I) And H$(I) = "V2" Then RWW(YY) = RWW(YY) + 1
            If O%(I) > statsT%(I) And H$(I) = "H" Then HWW(YY) = HWW(YY) + 1
            If O%(I) > statsT%(I) And H$(I) = "H1" Then HWW(YY) = HWW(YY) + 1
            If O%(I) > statsT%(I) And H$(I) = "H2" Then HWW(YY) = HWW(YY) + 1
            If statsT%(I) > O%(I) Then LL(YY) = LL(YY) + 1
            If statsT%(I) > O%(I) And H$(I) = "V" Then RLL(YY) = RLL(YY) + 1
            If statsT%(I) > O%(I) And H$(I) = "V1" Then RLL(YY) = RLL(YY) + 1
            If statsT%(I) > O%(I) And H$(I) = "V2" Then RLL(YY) = RLL(YY) + 1
            If statsT%(I) > O%(I) And H$(I) = "H" Then HLL(YY) = HLL(YY) + 1
            If statsT%(I) > O%(I) And H$(I) = "H1" Then HLL(YY) = HLL(YY) + 1
            If statsT%(I) > O%(I) And H$(I) = "H2" Then HLL(YY) = HLL(YY) + 1
            If O%(I) > statsT%(I) Then LWS(YY) = LWS(YY) + 1: LLS(YY) = 0
            If LWS(YY) > TWS(YY) Then TWS(YY) = LWS(YY)
            If O%(I) < statsT%(I) Then LLS(YY) = LLS(YY) + 1: LWS(YY) = 0
            If LLS(YY) > TLS(YY) Then TLS(YY) = LLS(YY)
        Next I

        If XX% <= 10 Then LTL(YY) = LL(YY): LTW(YY) = WW(YY): GoTo 60
        M = XX% - 9

        For J = M To XX%
            If O%(J) > statsT%(J) Then LTW(YY) = LTW(YY) + 1
            If statsT%(J) > O%(J) Then LTL(YY) = LTL(YY) + 1
        Next J

    60 Next KK

    Open D$(1) + LG$ + ".STD" For Output As #1
    Write #1, LGF$, D1$, D2$, D3$, D4$
    For I = 1 To 40: Write #1, WW(I), HWW(I), RWW(I), LL(I), HLL(I), RLL(I), LTW(I), LTL(I), TML$(I): Next
    Close #1

    Locate 10: Color 12: Print "P";: Color 7: Print "RINT OR ";: Color 12: Print "V";: Color 7: Print "IEW"
    2671 I$ = GetKeyPress$

    If UCase$(I$) = "P" Then 3000
    GoTo 2000


    950 For X = AA To ZZ - 1: For Y = X + 1 To ZZ: If WW(X) - LL(X) > WW(Y) - LL(Y) Then 985
        If WW(X) - LL(X) = WW(Y) - LL(Y) And WW(X) <= WW(Y) And WW(X) > LL(X) Then 985
            If WW(X) - LL(X) = WW(Y) - LL(Y) And WW(X) >= WW(Y) And WW(X) < LL(X) Then 985
            TN$ = TML$(X): TML$(X) = TML$(Y): TML$(Y) = TN$
            J = WW(X): WW(X) = WW(Y): WW(Y) = J
            J = LL(X): LL(X) = LL(Y): LL(Y) = J
            J = HLL(X): HLL(X) = HLL(Y): HLL(Y) = J
            J = RLL(X): RLL(X) = RLL(Y): RLL(Y) = J
            J = HWW(X): HWW(X) = HWW(Y): HWW(Y) = J
            J = RWW(X): RWW(X) = RWW(Y): RWW(Y) = J
            J = LTL(X): LTL(X) = LTL(Y): LTL(Y) = J
            J = LTW(X): LTW(X) = LTW(Y): LTW(Y) = J
            J = TWS(X): TWS(X) = TWS(Y): TWS(Y) = J
            J = TLS(X): TLS(X) = TLS(Y): TLS(Y) = J

    985 Next Y: Next X
    Return


    2000 AA = 1: ZZ = 10: GoSub 950
    Cls: Locate 1, 35: Color 11: Print LGF$
    Locate 2: Color 14: Print Tab(20); "                     MAJ          HOME    ROAD   LAST 10"
    Locate 3: Color 14: Print D1$; Tab(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "
    For I = 1 To 10
        If TML$(I) = "*" Then 2014
        Color 3: Print TML$(I);: Color 15: Print Tab(18); Using "### ### ###"; WW(I) + LL(I); WW(I); LL(I); Tab(30);
        If LL(I) = 0 And LL(I) + WW(I) <> 0 Then Print "1.000";: GoTo 2015
        If WW(I) = 0 Then Print " .000";: GoTo 2015
        If WW(I) + LL(I) <> 0 Then Print Using " .###"; WW(I) / (WW(I) + LL(I));: GoTo 2015
        If WW(I) + LL(I) = 0 Then Print " .000";

        2015 If ((WW(1) - WW(I)) + (LL(I) - LL(1))) / 2 = 0 Then Print Tab(36); " ---";
        If ((WW(1) - WW(I)) + (LL(I) - LL(1))) / 2 <> 0 Then Print Tab(36); Using "##.#"; ((WW(1) - WW(I)) + (LL(I) - LL(1))) / 2;
        Print Tab(45); Using "##"; TWS(I); Tab(49); TLS(I);
        Print Tab(53); Using "##"; HWW(I); Tab(56); HLL(I); Tab(61); RWW(I); Tab(64); RLL(I); Tab(70); LTW(I); Tab(73); LTL(I)

    2014 Next
    MJ = 83 - WW(1) - LL(2)
    If MJ < 0 Then MJ = 0
    Locate 4, 42: Print Using "##"; MJ
    AA = 11: ZZ = 20: GoSub 950
    Locate 12: Color 14: Print Tab(20); "                     MAJ          HOME    ROAD   LAST 10"
    Locate 13: Color 14: Print D2$; Tab(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "
    For I = 11 To 20
        If TML$(I) = "*" Then 2034
        Color 3: Print TML$(I);: Color 15: Print Tab(18); Using "### ### ###"; WW(I) + LL(I); WW(I); LL(I); Tab(30);
        If LL(I) = 0 And LL(I) + WW(I) <> 0 Then Print "1.000";: GoTo 2035
        If WW(I) = 0 Then Print " .000";: GoTo 2035
        If WW(I) + LL(I) <> 0 Then Print Using " .###"; WW(I) / (WW(I) + LL(I));: GoTo 2035
        If WW(I) + LL(I) = 0 Then Print " .000";

        2035 If ((WW(11) - WW(I)) + (LL(I) - LL(11))) / 2 = 0 Then Print Tab(36); " ---";
        If ((WW(11) - WW(I)) + (LL(I) - LL(11))) / 2 <> 0 Then Print Tab(36); Using "##.#"; ((WW(11) - WW(I)) + (LL(I) - LL(11))) / 2;
        Print Tab(45); Using "##"; TWS(I); Tab(49); TLS(I);
        Print Tab(53); Using "##"; HWW(I); Tab(56); HLL(I); Tab(61); RWW(I); Tab(64); RLL(I); Tab(70); LTW(I); Tab(73); LTL(I)

    2034 Next
    MJ = 83 - WW(11) - LL(12)
    If MJ < 0 Then MJ = 0
    Locate 14, 42: Print Using "##"; MJ
    AA = 21: ZZ = 30: GoSub 950
    Color 14: Locate 24: Print "HIT ANY KEY TO CONTINUE";
    2051 I$ = GetKeyPress$

    Cls
    Locate 2: Color 14: Print Tab(20); "                     MAJ          HOME    ROAD   LAST 10"
    Locate 3: Color 14: Print D3$; Tab(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "
    For I = 21 To 30
        If TML$(I) = "*" Then 2064
        Color 3: Print TML$(I);: Color 15: Print Tab(18); Using "### ### ###"; WW(I) + LL(I); WW(I); LL(I); Tab(30);
        If LL(I) = 0 And LL(I) + WW(I) <> 0 Then Print "1.000";: GoTo 2065
        If WW(I) = 0 Then Print " .000";: GoTo 2065
        If WW(I) + LL(I) <> 0 Then Print Using " .###"; WW(I) / (WW(I) + LL(I));: GoTo 2065
        If WW(I) + LL(I) = 0 Then Print " .000";

        2065 If ((WW(21) - WW(I)) + (LL(I) - LL(21))) / 2 = 0 Then Print Tab(36); " ---";
        If ((WW(21) - WW(I)) + (LL(I) - LL(21))) / 2 <> 0 Then Print Tab(36); Using "##.#"; ((WW(21) - WW(I)) + (LL(I) - LL(21))) / 2;
        Print Tab(45); Using "##"; TWS(I); Tab(49); TLS(I);
        Print Tab(53); Using "##"; HWW(I); Tab(56); HLL(I); Tab(61); RWW(I); Tab(64); RLL(I); Tab(70); LTW(I); Tab(73); LTL(I)

    2064 Next
    MJ = 83 - WW(21) - LL(22)
    If MJ < 0 Then MJ = 0
    Locate 4, 42: Print Using "##"; MJ
    AA = 31: ZZ = 40: GoSub 950
    Locate 12: Color 14: Print Tab(20); "                     MAJ          HOME    ROAD   LAST 10"
    Locate 13: Color 14: Print D4$; Tab(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "
    For I = 31 To 40
        If TML$(I) = "*" Then 2094 Else GoTo 2093

        2093 Color 3: Print TML$(I);: Color 15: Print Tab(18); Using "### ### ###"; WW(I) + LL(I); WW(I); LL(I); Tab(30);
        If LL(I) = 0 And LL(I) + WW(I) <> 0 Then Print "1.000";: GoTo 2095
        If WW(I) = 0 Then Print " .000";: GoTo 2095
        If WW(I) + LL(I) <> 0 Then Print Using " .###"; WW(I) / (WW(I) + LL(I));: GoTo 2095
        If WW(I) + LL(I) = 0 Then Print " .000";

        2095 If ((WW(31) - WW(I)) + (LL(I) - LL(31))) / 2 = 0 Then Print Tab(36); " ---";
        If ((WW(31) - WW(I)) + (LL(I) - LL(31))) / 2 <> 0 Then Print Tab(36); Using "##.#"; ((WW(31) - WW(I)) + (LL(I) - LL(31))) / 2;
        Print Tab(45); Using "##"; TWS(I); Tab(49); TLS(I);
        Print Tab(53); Using "##"; HWW(I); Tab(56); HLL(I); Tab(61); RWW(I); Tab(64); RLL(I); Tab(70); LTW(I); Tab(73); LTL(I)

    2094 Next
    MJ = 83 - WW(31) - LL(32)
    If MJ < 0 Then MJ = 0
    Locate 14, 42: Print Using "##"; MJ
    Locate 23: Color 14: Print "HIT ANY KEY TO CONTINUE"
    2216 I$ = GetKeyPress$
    GoTo 2300

    3000 LPrint Chr$(15)
    '    WIDTH "LPT1:1", 130
    AA = 1: ZZ = 10: GoSub 950
    LPrint LGF$
    LPrint Tab(20); "                     MAJ          HOME    ROAD   LAST 10"
    LPrint D1$; Tab(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "
    For I = 1 To 10
        If TML$(I) = "*" Then 3014
        LPrint TML$(I);: LPrint Tab(18); Using "### ### ###"; WW(I) + LL(I); WW(I); LL(I); Tab(30);
        If LL(I) = 0 And LL(I) + WW(I) <> 0 Then LPrint "1.000";: GoTo 3015
        If WW(I) = 0 Then LPrint " .000";: GoTo 3015
        If WW(I) + LL(I) <> 0 Then LPrint Using " .###"; WW(I) / (WW(I) + LL(I));: GoTo 3015
        If WW(I) + LL(I) = 0 Then LPrint " .000";

        3015 If ((WW(1) - WW(I)) + (LL(I) - LL(1))) / 2 = 0 Then LPrint Tab(36); " ---";
        If ((WW(1) - WW(I)) + (LL(I) - LL(1))) / 2 <> 0 Then LPrint Tab(36); Using "##.#"; ((WW(1) - WW(I)) + (LL(I) - LL(1))) / 2;
        LPrint Tab(42);
        MJ = 83 - WW(1) - LL(2)
        If MJ < 0 Then MJ = 0
        If I = 1 Then LPrint Using "##"; MJ;
        LPrint Tab(45); Using "##"; TWS(I); Tab(49); TLS(I);
        LPrint Tab(53); Using "##"; HWW(I); Tab(56); HLL(I); Tab(61); RWW(I); Tab(64); RLL(I); Tab(70); LTW(I); Tab(73); LTL(I)

    3014 Next
    AA = 11: ZZ = 20: GoSub 950
    LPrint: LPrint: LPrint Tab(20); "                     MAJ          HOME    ROAD   LAST 10"
    LPrint D2$; Tab(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "
    For I = 11 To 20
        If TML$(I) = "*" Then 3034
        LPrint TML$(I);: LPrint Tab(18); Using "### ### ###"; WW(I) + LL(I); WW(I); LL(I); Tab(30);
        If LL(I) = 0 And LL(I) + WW(I) <> 0 Then LPrint "1.000";: GoTo 3035
        If WW(I) = 0 Then LPrint " .000";: GoTo 3035
        If WW(I) + LL(I) <> 0 Then LPrint Using " .###"; WW(I) / (WW(I) + LL(I));: GoTo 3035
        If WW(I) + LL(I) = 0 Then LPrint " .000";

        3035 If ((WW(11) - WW(I)) + (LL(I) - LL(11))) / 2 = 0 Then LPrint Tab(36); " ---";
        If ((WW(11) - WW(I)) + (LL(I) - LL(11))) / 2 <> 0 Then LPrint Tab(36); Using "##.#"; ((WW(11) - WW(I)) + (LL(I) - LL(11))) / 2;
        LPrint Tab(42);
        MJ = 83 - WW(11) - LL(12)
        If MJ < 0 Then MJ = 0
        If I = 11 Then LPrint Using "##"; MJ;
        LPrint Tab(45); Using "##"; TWS(I); Tab(49); TLS(I);
        LPrint Tab(53); Using "##"; HWW(I); Tab(56); HLL(I); Tab(61); RWW(I); Tab(64); RLL(I); Tab(70); LTW(I); Tab(73); LTL(I)

    3034 Next: LPrint: LPrint
    AA = 21: ZZ = 30: GoSub 950
    LPrint Tab(20); "                     MAJ          HOME    ROAD   LAST 10"
    LPrint D3$; Tab(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "
    For I = 21 To 30
        If TML$(I) = "*" Then 3064
        LPrint TML$(I);: LPrint Tab(18); Using "### ### ###"; WW(I) + LL(I); WW(I); LL(I); Tab(30);
        If LL(I) = 0 And LL(I) + WW(I) <> 0 Then LPrint "1.000";: GoTo 3065
        If WW(I) = 0 Then LPrint " .000";: GoTo 3065
        If WW(I) + LL(I) <> 0 Then LPrint Using " .###"; WW(I) / (WW(I) + LL(I));: GoTo 3065
        If WW(I) + LL(I) = 0 Then LPrint " .000";

        3065 If ((WW(21) - WW(I)) + (LL(I) - LL(21))) / 2 = 0 Then LPrint Tab(36); " ---";
        If ((WW(21) - WW(I)) + (LL(I) - LL(21))) / 2 <> 0 Then LPrint Tab(36); Using "##.#"; ((WW(21) - WW(I)) + (LL(I) - LL(21))) / 2;
        LPrint Tab(42);: If I = 21 Then LPrint Using "##"; 83 - WW(21) - LL(22);
        LPrint Tab(45); Using "##"; TWS(I); Tab(49); TLS(I);
        LPrint Tab(53); Using "##"; HWW(I); Tab(56); HLL(I); Tab(61); RWW(I); Tab(64); RLL(I); Tab(70); LTW(I); Tab(73); LTL(I)

    3064 Next: LPrint: LPrint
    AA = 31: ZZ = 40: GoSub 950
    LPrint Tab(20); "                     MAJ          HOME    ROAD   LAST 10"
    LPrint D4$; Tab(20); "G   W   L   PCT   GB  #  LWS LLS  W- L    W- L     W- L "
    For I = 31 To 40
        If TML$(I) = "*" Then 3094 Else GoTo 3093

        3093 LPrint TML$(I);: LPrint Tab(18); Using "### ### ###"; WW(I) + LL(I); WW(I); LL(I); Tab(30);
        If LL(I) = 0 And LL(I) + WW(I) <> 0 Then LPrint "1.000";: GoTo 3095
        If WW(I) = 0 Then LPrint " .000";: GoTo 3095
        If WW(I) + LL(I) <> 0 Then LPrint Using " .###"; WW(I) / (WW(I) + LL(I));: GoTo 3095
        If WW(I) + LL(I) = 0 Then LPrint " .000";

        3095 If ((WW(31) - WW(I)) + (LL(I) - LL(31))) / 2 = 0 Then LPrint Tab(36); " ---";
        If ((WW(31) - WW(I)) + (LL(I) - LL(31))) / 2 <> 0 Then LPrint Tab(36); Using "##.#"; ((WW(31) - WW(I)) + (LL(I) - LL(31))) / 2;
        LPrint Tab(42);
        MJ = 83 - WW(31) - LL(32)
        If MJ < 0 Then MJ = 0
        If I = 31 Then LPrint Using "##"; MJ;
        LPrint Tab(45); Using "##"; TWS(I); Tab(49); TLS(I);
        LPrint Tab(53); Using "##"; HWW(I); Tab(56); HLL(I); Tab(61); RWW(I); Tab(64); RLL(I); Tab(70); LTW(I); Tab(73); LTL(I)

    3094 Next
    LPrint Chr$(12)
    LPrint Chr$(18)
    '    WIDTH "LPT1:1", 80
    GoTo 2300


End Sub
