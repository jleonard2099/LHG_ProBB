Sub CREATE ()

    _Title "Basketball: The Pro Game - Manage Leagues"

    Shared intZ1%()

    Shared memberTeams$(), memberYears$()

    ReDim menuChoices$(1 To 3)

    Do

        Color 15, 0
        Cls

        Call TITLE

        menuChoices$(1) = "CREATE / DEFINE NEW LEAGUES     "
        menuChoices$(2) = "ADD / REVIEW LEAGUE MEMBERS     "
        menuChoices$(3) = "RETURN TO STANDINGS/LEADERS MENU"
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 11, 57, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            For X = 1 To TEAMS_PER_CONFERENCE
                intZ1%(X) = -1
                memberTeams$(X) = "XXX"
                memberYears$(X) = "XXX"
            Next
            
            div1$ = ""
            div2$ = ""
            div3$ = ""
            div4$ = ""

            Select Case Selection%

                Case 1:
                    Call DefineLeaguePro(LG$, div1$, div2$, div3$, div4$)
                    Call ReviewEditLeague(LG$, div1$, div2$, div3$, div4$)

                Case 2:
                    Call ChooseLeaguePro(LG$)
                    Call ReviewEditLeague(LG$, div1$, div2$, div3$, div4$)

            End Select

        End If 'Done checking for ESC

    Loop Until Selection% = 3 Or ExitCode% = 27

    Cls

End Sub


'----------------------------------------
'       DefineLeaguePro Subroutine
'----------------------------------------
'...explanation...
Sub DefineLeaguePro (LG$, div1$, div2$, div3$, div4$)

    Do

        Color 15, 0
        Cls

        JJ = 7

        Locate 6, 8: Color 15, 1

        'Print box top
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        'Print box sides
        For II = 1 To JJ
            Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        'Print box bottom
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Do

            Locate 7, 32

            Locate , 12: Print "ENTER NAME FOR LEAGUE: ": Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPACES."
            Locate , 12: Print "SAMPLE NAMES ARE WHA IHL NHL94-95"
            Locate 7, 37: Input LG$

            If LG$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE (YN) "
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"


    FG = 0
    BG = 7

    Cls

    Color 15, 1

    Locate 5, 1: QPrint "Name for Division 1 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div1$, 10), FG, BG, -1
    Locate 7, 1: QPrint "Name for Division 2 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div2$, 10), FG, BG, -1
    Locate 9, 1: QPrint "Name for Division 3 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div3$, 10), FG, BG, -1
    Locate 11, 1: QPrint "Name for Division 4 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div4$, 10), FG, BG, -1

    GetDiv1:
    Locate 5, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 10 'specify field parameters
    Call TextIn(div1$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv2:
    Locate 7, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 10 'specify field parameters
    Call TextIn(div2$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv3:
    Locate 9, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 10 'specify field parameters
    Call TextIn(div3$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv4:
    Locate 11, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 10 'specify field parameters
    Call TextIn(div4$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv3: 'can't back up any more, stay here

    Done:
    Color 14, 1
    Locate 23, 1
    Print "Is this okay? (Yes/No) ";
    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 GoTo GetDiv4
    If X$ = "N" GoTo GetDiv1
    If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

End Sub


'----------------------------------------
'      ChooseLeaguePro Subroutine
'----------------------------------------
'...explanation...
Sub ChooseLeaguePro (league$)

    Shared memberTeams$(), memberYears$()

    Do
        'Display list of files...
        Spec$ = diskPaths$(1) + "*.LIB"
        Count% = FCount%(Spec$)

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

                Color , 1
                Cls

                Locate , 28
                Color 14
                Print "SELECT LEAGUE TO EDIT": Color 15
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
                targetFile$ = RTrim$(targetFile$)
                league$ = Left$(targetFile$, Len(targetFile$) - 4)
            End If

        End If 'Done checking file count

    Loop Until UCase$(I$) = "N" Or ExitCode% = 27 Or Count% = 0

End Sub


'----------------------------------------
'       ReviewEditLeague Subroutine
'----------------------------------------
'...explanation...
Sub ReviewEditLeague (league$, div1$, div2$, div3$, div4$)

    Shared intZ1%()

    Shared memberTeams$(), memberYears$()

    If _FileExists(diskPaths$(1) + league$ + ".LIB") Then

        Open diskPaths$(1) + league$ + ".LIB" For Input As #2

        Input #2, league$, div1$, div2$, div3$, div4$

        For X = 1 To TEAMS_PER_CONFERENCE
            Input #2, memberTeams$(X)
            Input #2, intZ1%(X)
            Input #2, memberYears$(X)
        Next

        Close 2

    End If

    Do

        Color 14, 0
        Cls

        Print
        Print "  "; league$; " LEAGUE "
        Print

        Color 12
        Locate , 4
        Print div1$; Tab(24); div2$; Tab(44); div3$; Tab(64); div4$
        Print

        For I = 1 To 10

            Color 2
            Print Using "## "; I;
            Color 15: Print memberTeams$(I);

            Locate , 21
            Color 2: Print Using "## "; I + 10;
            Color 15: Print memberTeams$(I + 10);

            Locate , 41
            Color 2: Print Using "## "; I + 20;
            Color 15: Print memberTeams$(I + 20);

            Locate , 61
            Color 2: Print Using "## "; I + 30;
            Color 15: Print memberTeams$(I + 30)

        Next I

        Do

            Locate 21, 1

            Color 14: Print "(1) ";
            Color 15: Print "ADD A TEAM"
            Color 14: Print "(2) ";
            Color 15: Print "REMOVE A TEAM"
            Color 14: Print "(3) ";
            Color 15: Print "END AND SAVE LEAGUE"

            I$ = GetKeyPress$
            choice = Val(I$)
        Loop Until choice >= 1 And choice <= 3

        If choice <> 3 Then

            Do
                Locate 22, 35
                Print "SLOT # (1-" + Str$(TEAMS_PER_CONFERENCE) + ") ";
                Input B
            Loop Until B >= 1 And B <= TEAMS_PER_CONFERENCE

            If choice = 1 Then
                'Add team
                Call ChooseTeam(memberYears$(B), NTMS, intZ1%(B), memberTeams$(B))

            Else
                'Remove team
                memberTeams$(B) = "XXX"
            End If

        End If 'Done checking if ready to save

    Loop Until choice = 3

    Call SaveLeague(league$, div1$, div2$, div3$, div4$)

End Sub


'----------------------------------------
'        SaveLeague Subroutine
'----------------------------------------
'...explanation...
Sub SaveLeague (league$, div1$, div2$, div3$, div4$)

    Shared intZ1%()

    Shared memberTeams$(), memberYears$()

    Open diskPaths$(1) + league$ + ".LIB" For Output As #2

    Print #2, league$
    Print #2, div1$
    Print #2, div2$
    Print #2, div3$
    Print #2, div4$

    For X = 1 To TEAMS_PER_CONFERENCE
        Print #2, memberTeams$(X)
        Print #2, intZ1%(X)
        Print #2, memberYears$(X)
    Next

    Close 2

    For X = 1 To TEAMS_PER_CONFERENCE

        If memberTeams$(X) = "XXX" Then
            memberTeams$(X) = Chr$(42)
        Else

            For D = 4 To 11
                If Mid$(memberTeams$(X), D, 1) = Chr$(32) Or Mid$(memberTeams$(X), D, 1) = "." Then
                    Mid$(memberTeams$(X), D, 1) = "_"
                End If
            Next D

            QQ = 8

            For D = 11 To 4 Step -1
                If Mid$(memberTeams$(X), D, 1) = "_" Then
                    QQ = D - 4
                Else
                    Exit For
                End If
            Next D

            L = Len(memberTeams$(X))
            YR$ = Left$(memberTeams$(X), 2)
            TM$ = Mid$(memberTeams$(X), 4, QQ)
            memberTeams$(X) = TM$ + "." + YR$

        End If

    Next X

    Open diskPaths$(1) + league$ + ".TMS" For Output As #2

    Write #2, league$, div1$, div2$, div3$, div4$

    For X = 1 To TEAMS_PER_CONFERENCE
        Write #2, memberTeams$(X)
    Next

    Close 2

End Sub


'----------------------------------------
'        GetTeam_CREATE Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_CREATE (T1$, YN$, D)

    Shared teamRatings%()

    Do

        Color 15, 0
        Cls

        JJ = 7

        Locate 6, 8: Color 15, 4

        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32

        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 27: Input NY$

        If NY$ = "" And YN$ <> "" Then
            'YN$ = NY$
            Call SelectTeam_CREATE(YN$, NTMS, D)
        Else
            YN$ = RTrim$(NY$)
            Erase teamNames$
            NTMS = 0

            Open diskPaths$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031
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

            Call SelectTeam_CREATE(YN$, NTMS, D)

        End If

        T1$ = RTrim$(Q$(0))

        For I = 0 To 34:
            teamRatings%(I) = CVI(Q$(I + 10))
        Next

        Close 1

        Locate 2, 33: Color 31: Print "LOADING..."

        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33
            Color teamRatings%(33), teamRatings%(34): Print T1$

            Do
                Color 15, 4
                Locate 3, 33: Print "ANY CHANGE (YN)";
                A$ = GetKeyPress$
            Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

        End If

    Loop Until UCase$(A$) = "N"

End Sub


'----------------------------------------
'      SelectTeam_CREATE Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_CREATE (yearNumber$, numberTeams, selectedIndex)

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

        selectedIndex = teamIndex%(Selection%)

        teamYear$ = RTrim$(yearNumber$)
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

        Get #1, selectedIndex
    Else
        BO% = 1
    End If

End Sub

