'----------------------------------------
'         CREATPRO Subroutine
'----------------------------------------
'This subroutine provides a menu for the
'user to choose options related to the
'creation and editing of leagues
Sub CREATPRO ()

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
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 11, 57, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
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
                    Call DefineLeaguePro(league$, div1$, div2$, div3$, div4$)
                    Call ReviewEditLeague(league$, div1$, div2$, div3$, div4$)

                Case 2:
                    league$ = ChooseFile$(diskPaths$(1) + "*.LIB", "LEAGUE", FileExit%)

                    If FileExit% <> 27 And FileExit% <> 0 Then
                        Call ReviewEditLeague(league$, div1$, div2$, div3$, div4$)
                    End If

            End Select

        End If 'Done checking for ESC

    Loop Until Selection% = 3 Or ExitCode% = 27

    Cls

End Sub


'----------------------------------------
'       DefineLeaguePro Subroutine
'----------------------------------------
'This subroutine assists the user with the
'input of paramters related to the league's
'general naming and setup
Sub DefineLeaguePro (league$, div1$, div2$, div3$, div4$)

    Do

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 1)

        Do

            Locate 7

            Locate , 12: Print "ENTER NAME FOR LEAGUE: "
            Print
            Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS"
            Locate , 12: Print "SAMPLE NAMES ARE NBA WBA NBA94-95"

            Locate 7, 33: Input league$

            If league$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE? (Y/N) "
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
    Locate , 21: QPrint PadRight$(div1$, 50), FG, BG, -1
    Locate 7, 1: QPrint "Name for Division 2 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div2$, 50), FG, BG, -1
    Locate 9, 1: QPrint "Name for Division 3 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div3$, 50), FG, BG, -1
    Locate 11, 1: QPrint "Name for Division 4 ", 14, 1, -1
    Locate , 21: QPrint PadRight$(div4$, 50), FG, BG, -1

    GetDiv1:
    Locate 5, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
    Call TextIn(div1$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv2:
    Locate 7, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
    Call TextIn(div2$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv3:
    Locate 9, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
    Call TextIn(div3$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data

    GetDiv4:
    Locate 11, 21 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 50 'specify field parameters
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
'      ReviewEditLeague Subroutine
'----------------------------------------
'This subroutine provides an interface to
'the user allowing them to choose / clear
'teams from the league definition.
Sub ReviewEditLeague (league$, div1$, div2$, div3$, div4$)

    Shared intZ1%()

    Shared memberTeams$(), memberYears$()

    If _FileExists(diskPaths$(1) + league$ + ".LIB") Then

        Open diskPaths$(1) + league$ + ".LIB" For Input As #1

        Input #1, league$, div1$, div2$, div3$, div4$

        For X = 1 To TEAMS_PER_CONFERENCE
            Input #1, memberTeams$(X)
            Input #1, intZ1%(X)
            Input #1, memberYears$(X)
        Next

        Close #1

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
'This subroutine writes the league team
'memberships and definition to each of
'the corresponding files (.LIB, .TMS)
Sub SaveLeague (league$, div1$, div2$, div3$, div4$)

    Shared intZ1%()

    Shared memberTeams$(), memberYears$(), statNames$()

    Open diskPaths$(1) + league$ + ".LIB" For Output As #1

    Print #1, league$
    Print #1, div1$
    Print #1, div2$
    Print #1, div3$
    Print #1, div4$

    For X = 1 To TEAMS_PER_CONFERENCE
        Print #1, memberTeams$(X)
        Print #1, intZ1%(X)
        Print #1, memberYears$(X)
    Next

    Close #1

    For X = 1 To TEAMS_PER_CONFERENCE

        If memberTeams$(X) = "XXX" Then
            statNames$(X) = Chr$(42)
        Else
            statNames$(X) = nameStatFile$(memberTeams$(X))
        End If

    Next X

    Open diskPaths$(1) + league$ + ".TMS" For Output As #2

    Write #2, league$, div1$, div2$, div3$, div4$

    For X = 1 To TEAMS_PER_CONFERENCE
        Write #2, statNames$(X)
    Next

    Close #2

End Sub

