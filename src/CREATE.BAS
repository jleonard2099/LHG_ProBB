Sub CREATE ()
 
    ReDim menuChoices$(1 To 3)
	Dim memberTeams$(1 to 40), memberYears$(1 to 40)
    Dim Z2$(40)
    Dim createT%(34)

	Shared intZ1%()

	'40 is number of teams in a conference

    Dim Y As Long

    _Title "Basketball: The Pro Game - Manage Leagues"

    'determine if the monitor is mono or color

    Do

        Cls

        50 For X = 1 To 40
            memberTeams$(X) = "XXX"
            memberYears$(X) = "XXX"
            intZ1%(X) = -1
        Next

    Call TITLE
    'Erase menuChoices$
    menuChoices$(1) = "CREATE / DEFINE NEW LEAGUES     "
    menuChoices$(2) = "ADD / REVIEW LEAGUE MEMBERS     "
    menuChoices$(3) = "RETURN TO STANDINGS/LEADERS MENU"
    SELECTION% = 1
    Count% = 3
    Start% = 1
    EXITCODE% = 3
    FileFGColr% = 15: FileBGColr% = 1
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Spacing% = 1
    Row% = 8
    Column% = 25

    CALL box0(7, 24, 11, 57, 2,  15, 1)
    _MouseShow "DEFAULT"
    MMenuVert menuChoices$(), SELECTION%, Start%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
    _MouseHide

    Color , 0
	newLeague = 0

    If Not (EXITCODE% = 27) Then
        I1 = SELECTION%
        Select Case I1
            Case 1: 
                'I = 1
                Do
                    130 Color 15, 0: Cls
                    JJ = 7
                    Locate 6, 8: Color 15, 1
                    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
                    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
                    Locate 7 + JJ, 8
                    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
                    Locate , 12: Print "ENTER NAME FOR LEAGUE: ": Print
                    Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
                    Locate , 12: Print "SPACES."
                    Locate , 12: Print "SAMPLE NAMES ARE WHA IHL NHL94-95"
                    Locate 7, 37: Input LG$
                    If LG$ <> "" Then
                        LG$ = Left$(LG$, 8)
                        Locate 13, 12: Print "ANY CHANGE (YN) "
                        I$ = GetKeyPress$
                    End If
                    'If UCase$(I$) = "Y" Then 1310
                Loop Until UCase$(I$) = "N"
				
				newLeague = 1
                Call CreateEditLeague (memberYears$(), memberTeams$(), newLeague)
                
            Case 2: 
                Spec$ = D$(1) + "*.LIB"
                null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
                Count% = DirCount%
                If Count% = 0 Then 
                    Locate 10, 8
                    Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
                    I$ = GetKeyPress$
                    Call CSMsgBox("", 0, Pass)
                    Call Create
                End If

                ReDim leagueFiles$(0 To Count%)
                For X = 1 To Count%
                    leagueFiles$(X) = Space$(12)
                Next
                leagueFiles$(0) = Spec$
                Call ReadFile(leagueFiles$())
                'CALL SortStr(VARPTR(leagueFiles$(1)), Count, 0)

                Do

                    Cls
                    Color , 1
                    Cls
                    Locate , 28
                    Color 14
                    Print "SELECT LEAGUE TO EDIT  ESC-TO EXIT": Color 15
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
                    Print "-SELECTS TEAM";

                    _MouseShow "DEFAULT"
                    MAMenu leagueFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                    _MouseHide

                    If Not (ExitCode% = 27) Then
						targetFile$ = leagueFiles$(SELECTION%)
						Locate 2, 33: Print F$
						Locate , 33: Print "ANY CHANGE (YN)";
						Color 15, 0
						I$ = GetKeyPress$
					End If
                    'If UCase$(I$) = "Y" Then 1310
                Loop Until UCase$(I$) = "N"

                targetFile$ = RTrim$(targetFile$)
                LG$ = Left$(targetFile$, Len(targetFile$) - 4)

                Open D$(1) + LG$ + ".LIB" For Input As #2
                Input #2, LG$
                Input #2, D1$
                Input #2, D2$
                Input #2, D3$
                Input #2, D4$
                For X = 1 To 40
                    Input #2, memberTeams$(X)
                    Input #2, intZ1%(X)
                    Input #2, memberYears$(X)
                Next
                Close 2

                Color 15, 1

                Call CreateEditLeague (memberYears$(), memberTeams$(), newLeague)
                
        End Select

    End If

Loop Until SELECTION% = 3

Cls
'Run "HELLO.EXE"

End Sub


'----------------------------------------
'       CreateEditLeague Subroutine
'----------------------------------------
'...explanation...
Sub CreateEditLeague (yearNumbers$(), memberTeams$(), newLeague)
    
	Dim T1$(1)
	Shared intZ1%()

    FG = 0
    BG = 7

	If (newLeague = 1) Then
		
		Cls
		
		Color 15, 1
		
		Locate 5, 1: QPrint "Name for Division 1 ", 14, 1, -1
		Locate , 21: QPrint PadRight$(D1$, 10), FG, BG, -1
		Locate 7, 1: QPrint "Name for Division 2 ", 14, 1, -1
		Locate , 21: QPrint PadRight$(D2$, 10), FG, BG, -1
		Locate 9, 1: QPrint "Name for Division 3 ", 14, 1, -1
		Locate , 21: QPrint PadRight$(D3$, 10), FG, BG, -1
		Locate 11, 1: QPrint "Name for Division 4 ", 14, 1, -1
		Locate , 21: QPrint PadRight$(D4$, 10), FG, BG, -1

		GetDiv1:
		Locate 5, 21 'location of beginning of field
		CapsOn = 0: NumOnly = 0: Max = 10 'specify field parameters
		Call TextIn(D1$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
		If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
		If XCode = 2 GoTo Done 'all done entering data

		GetDiv2:
		Locate 7, 21 'location of beginning of field
		CapsOn = 0: NumOnly = 0: Max = 10 'specify field parameters
		Call TextIn(D2$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
		If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
		If XCode = 2 GoTo Done 'all done entering data

		GetDiv3:
		Locate 9, 21 'location of beginning of field
		CapsOn = 0: NumOnly = 0: Max = 10 'specify field parameters
		Call TextIn(D3$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
		If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here
		If XCode = 2 GoTo Done 'all done entering data

		GetDiv4:
		Locate 11, 21 'location of beginning of field
		CapsOn = 0: NumOnly = 0: Max = 10 'specify field parameters
		Call TextIn(D4$, Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
		If XCode = 1 GoTo GetDiv3: 'can't back up any more, stay here

		Done:
		Locate 23, 1
		Print "Is this okay? (Yes/No) ";
		X$ = ""
		Call YesNoB(X$, XCode, FG, BG)
		Locate , 1
		ClearEOL 14, 1
		If XCode = 1 GoTo GetDiv4
		If X$ = "N" GoTo GetDiv1
		If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow
	End If


	'Edit / Review Members
    Do    
        Color 14, 0
        Cls
        Print
        Print "  "; LG$; " LEAGUE ": Print: Color 12
        Locate , 4: Print D1$; Tab(24); D2$; Tab(44); D3$; Tab(64); D4$

        For I = 1 To 10
            Color 2: Print Using "## "; I;: Color 15: Print memberTeams$(I);
            Locate , 21: Color 2: Print Using "## "; I + 10;: Color 15: Print memberTeams$(I + 10);
            Locate , 41: Color 2: Print Using "## "; I + 20;: Color 15: Print memberTeams$(I + 20);
            Locate , 61: Color 2: Print Using "## "; I + 30;: Color 15: Print memberTeams$(I + 30)
        Next I

        Locate 19, 1
        Color 14: Print "(1) ";: Color 15: Print "ADD A TEAM"
        Color 14: Print "(2) ";: Color 15: Print "REMOVE A TEAM"
        Color 14: Print "(3) ";: Color 15: Print "END AND SAVE SETTINGS"
        I$ = GetKeyPress$

		If I$ <> "3" Then
			Do
				Locate 20, 30: Print "SLOT # (1-40) ";
				N = 1: Input B
			Loop Until B >= 1 And B <= 40
		End If

        Select Case UCase$(I$)
            Case "1": 
            
                Do                  
                    '310 GoSub 1440
                    1440 Color 15, 0: Cls
                    JJ = 7
                    Locate 6, 8: Color 15, 4
                    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
                    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
                    Locate 7 + JJ, 8
                    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
                    Locate , 12: Print "ENTER DISK ID: ": Print
                    Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
                    Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
                    Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
                    Locate 7, 27: Input NY$
                    If NY$ = "" Then 
                        GoSub 1102
                        GoTo 182
                    Else
                        YN$ = NY$
                        ERASE teamNames$
                    
                        '1441 GoSub 1100
                        1100 NT = 0
                        Open D$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031
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
                                NT = NT + 1
                                teamNames$(NT) = Q$(0): teamIndex%(NT) = I1
                            End If
                        1103 Next
                        Close 1
                    
                    End If
                    
                    1102 SELECTION% = 1
                    Count% = NT
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
                    20 MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                    _MouseHide

                    Select Case EXITCODE%
                        Case 27: BO% = 1: Exit Sub
                        Case -71: 
                            Start% = 1
                            EXITCODE% = 3
                            SELECTION% = 1
                            GoTo 20
                        Case -79: 
                            Start% = Count%
                            EXITCODE% = 3
                            SELECTION% = Count%
                            GoTo 20
                        Case -73: 
                            Start% = Start% - 80
                            If Start% < 1 Then Start% = 1
                            EXITCODE% = 3
                            SELECTION% = Start%
                            GoTo 20
                        Case -81: 
                            Start% = Start% + 80
                            If Start% > Count% Then Start% = Count%
                            EXITCODE% = 3
                            SELECTION% = Start%
                            GoTo 20
                    End Select

                    D = teamIndex%(SELECTION%)
                    Locate 2, 33: Color 31: Print "LOADING..."

                    1300 Open D$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031
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

                    Get #1, D
                    T1$(0) = Q$(0)

                    For I = 0 To 34: createT%(I) = CVI(Q$(I + 10)): Next
                    Close 1

                    T1$ = RTrim$(T1$(0))

                    182 If BO% = 1 Then 
                        BO% = 0
                    Else
                        183 Locate 2, 33: Color 15, 0: Color createT%(33), createT%(34): Print T1$: Color 15, 4
                        Locate , 33: Print "ANY CHANGE (YN)";
                        A$ = GetKeyPress$
                    End If
                Loop Until UCase$(A$) = "N"

                390 memberTeams$(B) = T1$
                yearNumbers$(B) = YN$
                intZ1%(B) = D

                '400 GoTo 220

            Case "2": memberTeams$(B) = "XXX"
            Case "3": '
        End Select

    Loop Until I$ = "3"

    950 Cls

    970 Open D$(1) + LG$ + ".LIB" For Output As #2
    Print #2, LG$: Print #2, D1$: Print #2, D2$: Print #2, D3$: Print #2, D4$
    For X = 1 To 40
        Print #2, memberTeams$(X)
		Print #2, intZ1%(X)
		Print #2, yearNumbers$(X)
    Next
    Close 2

    '-- quick edits here
    For X = 1 To 40

        980 If memberTeams$(X) = "XXX" Then memberTeams$(X) = Chr$(42): GoTo 1040

        990 For D = 4 To 11
            If Mid$(memberTeams$(X), D, 1) = Chr$(32) Or Mid$(memberTeams$(X), D, 1) = "." Then Mid$(memberTeams$(X), D, 1) = "_"

        1000 Next D
        QQ = 8
        For D = 11 To 4 Step -1
            If Mid$(memberTeams$(X), D, 1) = "_" Then QQ = D - 4: GoTo 1020

            1010 GoTo 1030

        1020 Next D

        1030 L = Len(memberTeams$(X))
        YR$ = Left$(memberTeams$(X), 2)
        TM$ = Mid$(memberTeams$(X), 4, QQ)
        memberTeams$(X) = TM$ + "." + YR$

    1040 Next X
    '-- quick edits here

    1050 Q$ = LG$ + ".TMS"

    1060 Open D$(1) + Q$ For Output As #2

    1070 Write #2, LG$: Write #2, D1$: Write #2, D2$: Write #2, D3$: Write #2, D4$

    1080 For X = 1 To 40
        Write #2, memberTeams$(X)
    Next

1090 Close 2

End Sub

'2060 Cls: Print "ERROR:: UNABLE TO READ TEAM FILE": Print

'2720 BO% = 1: Return
