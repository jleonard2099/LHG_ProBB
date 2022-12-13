Sub NEWLDR ()

    _Title "Basketball: The Pro Game - Stats / Standings"

    ReDim menuChoices$(1 To 3)

    Do
        Cls

        Call TITLE
        'Erase menuChoices$
        menuChoices$(1) = "VIEW EXPANDED LEAGUE STATISTICS "
        menuChoices$(2) = "VIEW EXPANDED STANDINGS         "
        menuChoices$(3) = "RETURN TO LEAGUE LEADER MENU    "
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
            Select Case Selection%
                Case 1: Call ViewLeagueStats
                Case 2: Call NEWLDRST
                Case 3: Cls
            End Select
        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'       ViewLeagueStats Subroutine
'----------------------------------------
'... explain here ...
Sub ViewLeagueStats ()

    Shared printOrView%

    printOrView% = 0

    For X = 1 To TEAMS_PER_CONFERENCE
        Z1$(X) = "XXX"
    Next
    Spec$ = diskPaths$(1) + "*.TMS"

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
				targetFile$ = RTrim$(leagueFiles$(Selection%))
				Locate 2, 33: Print targetFile$
				Locate , 33: Print "ANY CHANGE (YN)";
				Color 15, 0
				I$ = GetKeyPress$

				If UCase$(I$) = "N" Then

					targetFile$ = Left$(targetFile$, Len(targetFile$) - 4)
					LG$ = targetFile$

					LG$ = targetFile$
					Open diskPaths$(1) + targetFile$ + ".TMS" For Input As #2
					Input #2, LG$
					Input #2, D1$
					Input #2, D2$
					Input #2, D3$
					Input #2, D4$
					For X = 1 To TEAMS_PER_CONFERENCE
						Input #2, Z1$(X)
					Next
					Close 2

					Call BUILDSTATS(targetFile$)

					ReDim menuChoices$(1 to 3)

					Do
						Cls

						Call TITLE
						'Erase menuChoices$
						menuChoices$(1) = "VIEW INDIVIDUAL PLAYER LEADERS  "
						menuChoices$(2) = "VIEW TEAM TOTALS                "
						menuChoices$(3) = "RETURN TO LEAGUE LEADER MENU    "
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
							Select Case Selection%
								Case 1: Call ViewInidividualLeaders
								Case 2: Call ViewTotalLeaders
								Case 3: Cls
							End Select
						End If

					Loop Until Selection% = 3

				End If ' End of checking if Any Change Y/N

			End If 'End of checking return code from menu

		Loop Until UCase$(I$) = "N" Or ExitCode% = 27

	End If 'Done checking file count

End Sub

'----------------------------------------
'    ViewInidividualLeaders Subroutine
'----------------------------------------
'... explain here ...
Sub ViewInidividualLeaders ()

    Dim JB, TS

    Dim AL(600, 14), GM(TEAMS_PER_CONFERENCE), GM1(TEAMS_PER_CONFERENCE)
    Dim TT!(TEAMS_PER_CONFERENCE, 15), TT1!(TEAMS_PER_CONFERENCE, 15), W0L(600), W1L(600)

    Dim AL$(600)
    ReDim menuChoices$(1 To 3)

    '--> replace 600 with a CONST
    Dim TMA$(600), TT$(TEAMS_PER_CONFERENCE, 15), TT1$(TEAMS_PER_CONFERENCE, 15)
    ReDim GMA!(600)

    Shared TMM$(), TP$(), TP1$()
    Shared printOrView%, YY%

    Color , 0
    Cls
    Print
    Print
    Print
    Print

    J = 20
    ZZ = 1

    Locate 2, 4
    Color 15, 4: Print " " + Chr$(214); String$(66, 196); "· ";
    For II = 1 To J
        Locate 2 + II, 4
        Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";
    Next
    Locate 3 + J, 4
    Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";
    Locate 10, 15

    Do

        For I = 0 To 15
            Color 14
            Locate I + 3, 8: Print Using "##"; I;: Print " ";
            Color 0: Print TP$(I);
            Color 14: Print Tab(40); Using "##"; I + 16;: Print " ";
            Color 0: Print TP$(I + 16)
        Next

        Print
        Locate , 23: Input "SELECT NUMBER TO VIEW"; B9

        Select Case B9
            Case 0: X = 1: stpl = 0: PPP = 0
            Case 1: X = 11: stpl = 0: PPP = 0
            Case 2: X = 3: stpl = 0: PPP = 0
            Case 3: X = 2: stpl = 0: PPP = 0
            Case 4: X = 3: X1 = 2: stpl = 0: PPP = 0
            Case 5: stpl = 0: PPP = 0
            Case 6: stpl = 0: PPP = 0
            Case 7: stpl = 0: PPP = 0
            Case 8: X = 5: stpl = 0: PPP = 0
            Case 9: X = 4: stpl = 0: PPP = 0
            Case 10: 
                X = 5: X1 = 4: stpl = 0: PPP = 0
                Call SelectOptions_NEWLDR(printOrView%, sortType%)
                Cls
                PPP = 0

                For I = 1 To JB

                    If AL(I, X) >= GMA!(I) * 1.52 Then
                        PPP = PPP + 1
                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                        TPP$(PPP) = AL$(I)
                        TMM$(PPP) = TMA$(I)
                        TYP!(PPP) = AL(I, X) / AL(I, X1)
                    End If

                Next I

                Call SortPlayer (PPP, stpl, sortType%)

                TVN = 2
                QW = PPP
                Call GetListLength (QW, length)

                If printOrView% = 1 Then 
                    Call DisplayLeaders (QW)
                Else
                    'printOrView% = 2
                    Call PrintLeaders (length)
                End If

            Case 11: X = 3: X1 = 5: stpl = 0: PPP = 0
            Case 12: X = 3: X1 = 5: X2 = 1: stpl = 0: PPP = 0
            Case 13: X = 14: stpl = 0: PPP = 0
            Case 14: X = 13: stpl = 0: PPP = 0
            Case 15: X = 6: X1 = 13: stpl = 0: PPP = 0
            Case 16: X = 6: stpl = 0: PPP = 0
            Case 17: X = 6: X1 = 1: stpl = 0: PPP = 0
            Case 18: X = 7: stpl = 0: PPP = 0
            Case 19: X = 9: stpl = 0: PPP = 0
            Case 20: X = 10: stpl = 0: PPP = 0
            Case 21: X = 8: stpl = 0: PPP = 0
            Case 22: X = 12: stpl = 0: PPP = 0
            Case 23: X = 3: X1 = 5: X2 = 11: stpl = 0: PPP = 0
            Case 24: X = 6: X1 = 11: stpl = 0: PPP = 0
            Case 25: X = 7: X1 = 11: stpl = 0: PPP = 0
            Case 26: X = 9: X1 = 11: stpl = 0: PPP = 0
            Case 27: X = 10: X1 = 11: stpl = 0: PPP = 0
            Case 28: X = 8: X1 = 11: stpl = 0: PPP = 0
            Case 29: X = 12: X1 = 11: stpl = 0: PPP = 0
            Case 30: X = 11: X1 = 1: stpl = 0: PPP = 0
            Case 31: Exit Sub
            Case Is > 31: '
        End Select

    Loop Until B9 >= 0 And B9 <= 31

    If B9 <> 10 Then Call SelectOptions_NEWLDR(printOrView%, sortType%)

    Cls
    PPP = 0

    '--- need to confirm how this should work
    Select Case B9
        Case 0, 2 to 3, 8 to 9, 13 to 14, 16, 18 to 22:

            For I = 1 To JB
                If AL(I, X) <> 0 Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = AL(I, X)
                End If
            Next I

            Call SortPlayer (PPP, stpl, sortType%)

            TVN = 1
            QW = PPP
            Call GetListLength (QW, length)
            
            If printOrView% = 1 Then 
                Call DisplayLeaders (QW)
            Else
                'printOrView% = 2
                Call PrintLeaders (length)
            End If

        Case 1: 
            For I = 1 To JB
                If AL(I, X) <> 0 Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = (AL(I, X) + 30) / 60
                End If
            Next I

            Call SortPlayer (PPP, stpl, sortType%)

            TVN = 1
            QW = PPP
            Call GetListLength (QW, length)
            
            If printOrView% = 1 Then 
                Call DisplayLeaders (QW)
            Else
                'printOrView% = 2
                Call PrintLeaders (length)
            End If

        Case 4: 
            For I = 1 To JB
                If AL(I, X) >= GMA!(I) * 3.65 Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = AL(I, X) / AL(I, X1)
                End If

            Next I
            
            Call SortPlayer (PPP, stpl, sortType%)

            Call DisplayLeaders (QW)

        Case 5: 
            For I = 1 To JB
                If W0L(I) <> 0 Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = AL$(I)
                    TMM$(PPP) = TMA$(I)
                    TYP!(PPP) = W0L(I)
                End If
            Next I

            Call SortPlayer (PPP, stpl, sortType%)

            TVN = 1
            QW = PPP
            Call GetListLength (QW, length)
            
            If printOrView% = 1 Then 
                Call DisplayLeaders (QW)
            Else
                'printOrView% = 2
                Call PrintLeaders (length)
            End If

        Case 6: 
            For I = 1 To JB
                If W1L(I) <> 0 Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = W1L(I)
                End If
            Next I

            Call SortPlayer (PPP, stpl, sortType%)

            TVN = 1
            QW = PPP
            Call GetListLength (QW, length)
            
            If printOrView% = 1 Then 
                Call DisplayLeaders (QW)
            Else
                'printOrView% = 2
                Call PrintLeaders (length)
            End If

        Case 7: 
            For I = 1 To JB
                If W0L(I) >= GMA!(I) * .3 Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = AL$(I)
                    TMM$(PPP) = TMA$(I)
                    TYP!(PPP) = W0L(I) / W1L(I)
                End If
            Next I

            Call SortPlayer (PPP, stpl, sortType%)

            TVN = 2
            QW = PPP
            Call GetListLength (QW, length)

            If printOrView% = 1 Then 
                Call DisplayLeaders (QW)
            Else
                'printOrView% = 2
                Call PrintLeaders (length)
            End If

        Case 11: 
            For I = 1 To JB
                If (AL(I, X) * 2) + AL(I, X1) + W0L(I) <> 0 Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = AL$(I)
                    TMM$(PPP) = TMA$(I)
                    TYP!(PPP) = (AL(I, X) * 2) + AL(I, X1) + W0L(I)
                    TPP$(PPP) = AL$(I)
                    TMM$(PPP) = TMA$(I): TYP!(PPP) = 0
                End If
            Next I
            
            Call SortPlayer (PPP, stpl, sortType%)
            
            TVN = 1
            QW = PPP
            Call GetListLength (QW, length)
            
            If printOrView% = 1 Then 
                Call DisplayLeaders (QW)
            Else
                'printOrView% = 2
                Call PrintLeaders (length)
            End If

        Case 12:

            For I = 1 To JB
                If Not ((AL(I, X) * 2) + AL(I, X1) + W0L(I) = 0 Or AL(I, X2) = 0) Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = ((AL(I, X) * 2) + AL(I, X1) + W0L(I)) / AL(I, X2)
                End If
            Next I

            Call SortPlayer (PPP, stpl, sortType%)

            TVN = 3
            QW = PPP
            Call GetListLength (QW, length)

            If printOrView% = 1 Then 
                Call DisplayLeaders (QW)
            Else
                'printOrView% = 2
                Call PrintLeaders (length)
            End If

        Case 15: 
            For I = 1 To JB
                If AL(I, X) - AL(I, X1) <> 0 Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = AL(I, X) - AL(I, X1)
                End If
            Next I

            Call SortPlayer (PPP, stpl, sortType%)

            TVN = 1
            QW = PPP
            Call GetListLength (QW, length)
            
            If printOrView% = 1 Then 
                Call DisplayLeaders (QW)
            Else
                'printOrView% = 2
                Call PrintLeaders (length)
            End If

        Case 17:

            For I = 1 To JB
                If AL(I, X) <> 0 Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = AL(I, X) / AL(I, X1)
                End If
            Next I

            Call SortPlayer (PPP, stpl, sortType%)

            TVN = 3
            QW = PPP
            Call GetListLength (QW, length)

            If printOrView% = 1 Then 
                Call DisplayLeaders (QW)
            Else
                'printOrView% = 2
                Call PrintLeaders (length)
            End If

        Case 23: 
            For I = 1 To JB
                If Not ((AL(I, X) * 2) + AL(I, X1) + W0L(I) = 0 Or AL(I, X2) = 0) Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = (((AL(I, X) * 2) + AL(I, X1) + W0L(I))) * 48 / ((AL(I, X2) + 30) / 60)
                End If
            Next I

            Call SortPlayer (PPP, stpl, sortType%)

            TVN = 3
            QW = PPP
            Call GetListLength (QW, length)

            If printOrView% = 1 Then 
                Call DisplayLeaders (QW)
            Else
                'printOrView% = 2
                Call PrintLeaders (length)
            End If

        Case 24 to 29: 
            For I = 1 To JB
                If Not(AL(I, X) = 0 Or AL(I, X1) = 0) Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = (AL(I, X)) * 48 / ((AL(I, X1) + 30) / 60)
                End If
            Next I

            Call SortPlayer (PPP, stpl, sortType%)

            TVN = 3
            QW = PPP
            Call GetListLength (QW, length)

            If printOrView% = 1 Then 
                Call DisplayLeaders (QW)
            Else
                'printOrView% = 2
                Call PrintLeaders (length)
            End If

        Case 30: 
            For I = 1 To JB
                If Not (AL(I, X) = 0 Or AL(I, X1) = 0) Then
                    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                    TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP!(PPP) = ((AL(I, X) + 30) / 60) / AL(I, X1)
                End If
            Next I

            Call SortPlayer (PPP, stpl, sortType%)

            TVN = 3
            QW = PPP
            Call GetListLength (QW, length)

            If printOrView% = 1 Then 
                Call DisplayLeaders (QW)
            Else
                'printOrView% = 2
                Call PrintLeaders (length)
            End If

    End Select

End Sub

'----------------------------------------
'       ViewLeaderScreen Subroutine
'----------------------------------------
'... explain here ...
Sub DisplayLeaders (QW)

    Shared TMM$(), TPP$(), TP$(), TYP!()

    Do
        Color 14, 0
        Cls
        Print "     PLAYER                  TEAM                     "; TP$(B9)

        For W = 1 To QW

            If (W <> 0 AND W MOD 20 = 0) Then
                Locate 22
                Color 15
                Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                I$ = GetKeyPress$
                If I$ <> Chr$(27) Then
                    Cls
                    Color 14: Print "     PLAYER                  TEAM                     "; TP$(B9)
                End If
            End If

            Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Color 15: Print Tab(25); TMM$(W);

            Select Case TVN
                Case 1: Color 15: Print Tab(58); Using "#####"; TYP!(W)
                Case 2: Color 15: Print Tab(58); Using ".###"; TYP!(W)
                Case 3: Color 15: Print Tab(58); Using "###.#"; TYP!(W)
            End Select

        Next W

        Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then 
            Locate 22: Print Space$(64)
            Locate 22: Color 14: Print "VIEW AGAIN (Y/N)"
            I$ = GetKeyPress$
        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub

'----------------------------------------
'         PrintLeaders Subroutine
'----------------------------------------
'... explain here ...
Sub PrintLeaders (LG)
    Shared TMM$(), TP$(), TYP!()

    LPrint Chr$(15)
    '    WIDTH "LPT1:1", 130

    Color 14, 0: Cls: LPrint "     PLAYER                  TEAM                     "; TP$(B9)

    For W = 1 To LG

        If (W <> 0 AND W MOD 50 = 0) Then 
            LPrint Chr$(12)
            LPrint "     PLAYER                  TEAM                     "; TP$(XX)
        End If

        LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);

        Select Case TVN
            Case 1: LPrint Tab(58); Using "#####"; TYP!(W)
            Case 2: LPrint Tab(58); Using ".###"; TYP!(W)
            Case 3: LPrint Tab(58); Using "###.#"; TYP!(W)
        End Select

    Next W

    LPrint Chr$(18)

End Sub

'----------------------------------------
'       ViewTotalLeaders Subroutine
'----------------------------------------
'... explain here ...
Sub ViewTotalLeaders ()
    Shared TYP!()
    Shared TMM$()

    Do
        Color , 0
        Cls
        Print
        Print
        Print
        Print
        J = 21
        ZZ = 3

        Locate 1, 4
        Color 15, 6: Print " " + Chr$(214); String$(66, 196); "· ";

        For II = 1 To J: Locate 1 + II, 4: Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";: Next

        Locate 2 + J, 4: Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";: Locate 10, 15

        For I = 0 To 19: Locate I + 2, 8: Color 11: Print Using "##"; I;: Print " ";: Color 0: Print TP1$(I): Next
        For I = 20 To 38: Locate I - 18, 38: Color 11: Print Using "##"; I;: Print " ";: Color 0: Print TP1$(I): Next

        Do
            Color 14
            Locate 22, 23: Input "SELECT NUMBER TO VIEW"; B9
        Loop Until B9 >= 0 and B9 <= 38

        Select Case B9
            Case 0: X = 1: stpl = 0
            Case 1: X = 2: stpl = 0
            Case 2: X = 2: X1 = 1: stpl = 0
            Case 3: X = 14: stpl = 0
            Case 4: X = 13: stpl = 0
            Case 5: X = 13: X1 = 14: stpl = 0
            Case 6: X = 3: stpl = 0
            Case 7: X = 4: stpl = 0
            Case 8: X = 4: X1 = 3: stpl = 0
            Case 9: X = 12: stpl = 0
            Case 10: X = 5: X1 = 12: stpl = 0
            Case 11: X = 5: stpl = 0
            Case 12: X = 5: stpl = 0
            Case 13: X = 6: stpl = 0
            Case 14: X = 8: stpl = 0
            Case 15: X = 9: stpl = 0
            Case 16: X = 7: stpl = 0
            Case 17: X = 11: stpl = 0
            Case 18: X = 2: X1 = 13: X2 = 4: stpl = 0
            Case 19: X = 1: stpl = 0
            Case 20: X = 2: stpl = 0
            Case 21: X = 2: X1 = 1: stpl = 0
            Case 22: X = 14: stpl = 0
            Case 23: X = 13: stpl = 0
            Case 24: X = 13: X1 = 14: stpl = 0
            Case 25: X = 3: stpl = 0
            Case 26: X = 4: stpl = 0
            Case 27: X = 4: X1 = 3: stpl = 0
            Case 28: X = 12: stpl = 0
            Case 29: X = 5: X1 = 12: stpl = 0
            Case 30: X = 5: stpl = 0
            Case 31: X = 5: stpl = 0
            Case 32: X = 6: stpl = 0
            Case 33: X = 8: stpl = 0
            Case 34: X = 9: stpl = 0
            Case 35: X = 7: stpl = 0
            Case 36: X = 11: stpl = 0
            Case 37: X = 2: X1 = 13: X2 = 4: stpl = 0
        End Select

        Call SelectOptions_NEWLDR (printOrView%, sortType%)

        Cls

        Select Case B9
            Case 0 to 1, 3 to 4, 6 to 7, 9, 11, 13 to 17: 
                For I = 1 To TS
                    TMM$(I) = TT$(I, X): TYP!(I) = TT!(I, X)
                Next

                For I = 2 To TS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
                    For J = TS To I Step -1
                        If sortType% = 1 Then If TYP!(J - 1) < TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
                        If sortType% = 2 Then If TYP!(J - 1) > TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
                    Next J
                Next I

                TVN = 1
                QW = TS
                Call DisplayTeamView_NEWLDR (printOrView%, QW, TVN)

            Case 2, 5, 8: 
                For I = 1 To TS
                    If TT!(I, X1) = 0 Then 
                        TMM$(I) = TT$(I, X)
                        TYP!(I) = 0
                    Else 
                        TMM$(I) = TT$(I, X)
                        TYP!(I) = TT!(I, X) / TT!(I, X1)
                    End If

                Next

                For I = 2 To TS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
                    For J = TS To I Step -1
                        If sortType% = 1 Then If TYP!(J - 1) < TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
                        If sortType% = 2 Then If TYP!(J - 1) > TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
                    Next J
                Next I

                TVN = 2
                QW = TS
                Call DisplayTeamView_NEWLDR (printOrView%, QW, TVN)

            Case 10: 
                For I = 1 To TS
                    TMM$(I) = TT$(I, X): TYP!(I) = TT!(I, X) - TT!(I, X1)
                Next

                For I = 2 To TS
                    stpl = stpl + 1
                    Locate 5, 5: Print "sorting team "; stpl
                    For J = TS To I Step -1
                        If sortType% = 1 Then If TYP!(J - 1) < TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
                        If sortType% = 2 Then If TYP!(J - 1) > TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
                    Next J
                Next I

                TVN = 1
                QW = TS
                Call DisplayTeamView_NEWLDR (printOrView%, QW, TVN)

            Case 12: 
                For I = 1 To TS
                    TMM$(I) = TT$(I, X)
                    TYP!(I) = TT!(I, X) / GM(I)
                Next

                For I = 2 To TS
                    stpl = stpl + 1
                    Locate 5, 5: Print "sorting team "; stpl
                    For J = TS To I Step -1
                        If sortType% = 1 Then If TYP!(J - 1) < TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
                        If sortType% = 2 Then If TYP!(J - 1) > TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
                    Next J
                Next I

                TVN = 3
                QW = TS
                Call DisplayTeamView_NEWLDR (printOrView%, QW, TVN)

            Case 18:
                For I = 1 To TS
                    TMM$(I) = TT$(I, X)
                    TYP!(I) = ((TT!(I, X) * 2) + TT!(I, X1) + TT!(I, X2)) / GM(I)
                Next

                For I = 2 To TS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
                    For J = TS To I Step -1
                        If sortType% = 1 Then If TYP!(J - 1) < TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
                        If sortType% = 2 Then If TYP!(J - 1) > TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
                    Next J
                Next I

                TVN = 3
                QW = TS
                Call DisplayTeamView_NEWLDR (printOrView%, QW, TVN)

            Case 19 to 20, 22 to 23, 25 to 26, 28, 30, 32 to 36:
                For I = 1 To TTS
                    TMM$(I) = TT1$(I, X)
                    TYP!(I) = TT1!(I, X)
                Next

                Call SortTeam (TTS, stpl, sortType%)

                TVN = 1
                QW = TTS
                Call DisplayTeamView_NEWLDR (printOrView%, QW, TVN)

            Case 21, 24, 27:
                For I = 1 To TTS
                    If TT1!(I, X1) = 0 Then 
                        TMM$(I) = TT1$(I, X)
                        TYP!(I) = 0 
                    Else 
                        TMM$(I) = TT1$(I, X)
                        TYP!(I) = TT1!(I, X) / TT1!(I, X1)
                    End If
                Next

                Call SortTeam (TTS, stpl, sortType%)

                TVN = 2
                QW = TTS
                Call DisplayTeamView_NEWLDR (printOrView%, QW, TVN)

            Case 29: 
                For I = 1 To TTS
                    TMM$(I) = TT1$(I, X)
                    TYP!(I) = TT1!(I, X) - TT1!(I, X1)
                Next

                Call SortTeam (TTS, stpl, sortType%)

                TVN = 1
                QW = TTS
                Call DisplayTeamView_NEWLDR (printOrView%, QW, TVN)

            Case 31: 
                For I = 1 To TTS
                    TMM$(I) = TT1$(I, X)
                    TYP!(I) = TT1!(I, X) / GM1(I)
                Next

                Call SortTeam (TTS, stpl, sortType%)

                TVN = 3
                QW = TTS
                Call DisplayTeamView_NEWLDR (printOrView%, QW, TVN)

            Case 37: 
                For I = 1 To TTS
                    TMM$(I) = TT1$(I, X)
                    TYP!(I) = ((TT1!(I, X) * 2) + TT1!(I, X1) + TT1!(I, X2)) / GM1(I)
                Next

                Call SortTeam (TTS, stpl, sortType%)

                TVN = 3
                QW = TTS
                Call DisplayTeamView_NEWLDR (printOrView%, QW, TVN)

            Case 38: Cls 
        End Select

    Loop Until B9 = 38

End Sub

'----------------------------------------
'    DisplayTeamView_NEWLDR Subroutine
'----------------------------------------
'... explain here ...
Sub DisplayTeamView_NEWLDR (printType, QW, TVN)

    Shared TYP!(), TP1$ ()
    Shared TMM$()

    Select Case printType
        Case 1:
            Do
                Color 14, 0
                Cls
                Print "     TEAM                     "; TP1$(B9)
                For W = 1 To QW
                    If W = 20 Or W = 40 Then 
                        Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                        I$ = GetKeyPress$
                        If Not(I$ = Chr$(27)) Then 
                            Cls
                            Color 14: Print "     TEAM                     "; TP1$(B9)
                            Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);
                            Select Case TVN
                                Case 1: Color 15: Print Tab(32); Using "#####"; TYP!(W)
                                Case 2: Color 15: Print Tab(33); Using ".###"; TYP!(W)
                                Case 3: Color 15: Print Tab(33); Using "###.#"; TYP!(W)
                            End Select
                        Else
                            Exit Do
                        End If
                    End If
                Next W

                Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    Locate 22: Print Space$(64)
                    Locate 22: Color 14: Print "VIEW AGAIN (Y/N)"
                    I$ = GetKeyPress$
                End If

            Loop Until I$ = "N"

        Case 2:
            LPrint Chr$(15)
            'WIDTH "LPT1:1", 130
            LPrint "     TEAM                     "; TP1$(B9)

            For W = 1 To QW
                Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);
                Select Case TVN
                    Case 1: Color 15: Print Tab(32); Using "#####"; TYP!(W)
                    Case 2: Color 15: Print Tab(33); Using ".###"; TYP!(W)
                    Case 3: Color 15: Print Tab(33); Using "###.#"; TYP!(W)
                End Select
            Next W

            LPrint Chr$(12)
            LPrint Chr$(18)
            ' WIDTH "LPT1:1", 80

    End Select

End Sub

'----------------------------------------
'    SelectOptions_NEWLDR Subroutine
'----------------------------------------
'... explain here ...
Sub SelectOptions_NEWLDR (printType, sortType%)

    J = 6
    Locate 8, 8: Color 15, 1: Print " " + Chr$(214); String$(57, 196); "· ";

    For II = 1 To J: Locate 8 + II, 8: Print " " + Chr$(186); String$(57, 32); Chr$(186) + " ";: Next

    Locate 9 + J, 8: Print " " + Chr$(211); String$(57, 196); Chr$(189) + " ";: Locate 10, 15

    Color 3: Locate , 30: Print "SELECT OPTION:"
    Locate , 30: Color 15: Print "H";: Color 7: Print "IGHEST TO LOWEST"
    Locate , 30: Color 15: Print "L";: Color 7: Print "OWEST TO HIGHEST"
    Locate , 30: Color 15: Print "R";: Color 7: Print "E-SELECT CATEGORY"

    Do
        I$ = GetKeyPress$

        Select Case I$

            Case "H":
                Call PrintViewReselect(I2$)
                Select Case I2$
                    Case "P": printType = 2
                    Case "V": printType = 1
                    Case "R": 
                        If ZZ = 1 Or ZZ = 3 Then Exit Sub

                End Select
                sortType% = 1

            Case "L": 
                Call PrintViewReselect(I2$)
                Select Case I2$
                Case "P": printType = 2
                Case "V": printType = 1
                Case "R":

                    If ZZ = 1 Or ZZ = 3 Then Exit Sub 
                End Select
                sortType% = 2

            Case "R": 
                If ZZ = 1 Or ZZ = 3 Then Exit Sub

        End Select

    Loop Until UCase$(I$) = "H" Or UCase$(I$) = "L" Or UCase$(I$) = "R"

End Sub

'----------------------------------------
'     PrintViewReselect Subroutine
'----------------------------------------
'... explain here ...
Sub PrintViewReselect (I$)

    Print
    Print
    Print
    Print
    Print

    J = 4
    Locate 9, 15: Color 15, 0: Print " " + Chr$(214); String$(40, 196); "· ";

    For II = 1 To J
        Locate 9 + II, 15: Print " " + Chr$(186); String$(40, 32); Chr$(186) + " ";
    Next

    Locate 10 + J, 15: Print " " + Chr$(211); String$(40, 196); "½ ": Locate 10, 10

    Color 12: Locate , 30: Print "SELECT OPTION:"
    Locate , 30: Color 14: Print "P";: Color 7: Print "RINT"
    Locate , 30: Color 14: Print "V";: Color 7: Print "IEW"
    Locate , 30: Color 14: Print "R";: Color 7: Print "E-SELECT OPTION"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V" Or UCase$(I$) = "R"

End Sub

'----------------------------------------
'         GetListLength Subroutine
'----------------------------------------
'... explain here ...
Sub GetListLength (maxQW, listLength)

    Shared printOrView%

    Do
        Color , 0
        Cls

        If printOrView% <> 1 Then
            Print
            Print
            Print
            Print
            Print
            J = 2

            Locate 10, 20
            Color 4, 15: Print " " + Chr$(214); String$(30, 196); Chr$(183) + " ";

            For II = 1 To J
                Locate 10 + II, 20: Print " " + Chr$(186); String$(30, 32); Chr$(186) + " ";
            Next

            Locate 11 + J, 20: Print " " + Chr$(211); String$(30, 196); Chr$(189) + " ": Locate 11

            Locate , 25: Color 1: Print "MAXIMUM = "; maxQW
            Locate , 25: Input "ENTER LENGTH OF LIST"; listLength
        End If

    Loop Until listLength <= maxQW Or printOrView% = 1

End Sub

'------------------------------
'     BUILDSTATS Subroutine
'------------------------------
' This creates an .LDS file for your league
Sub BUILDSTATS (statFile$)

    Dim buildA$(0 to 14), AL$(600)
    Dim TMA$(600), TT$(40, 15), TT1$(40, 15)

    Dim AL(600, 14)
    Dim GM(40), GM1(40), TT!(40, 15), TT1!(40, 15)
    Dim W0!(15), W1!(15)
    Dim Z!(15), Z1!(15)

    Dim W0L(600), W1L(600)
    ReDim GMA!(600), TYP!(600)

    Dim JB, TBN

    Shared A!()
    Shared YY%

    TMR% = 1280

    For KK = 1 To 40: YY% = YY% + 1
        If Z1$(KK) <> "*" Then

            Print "CONVERTING STAT FILE OF "; Z1$(KK)
            If _FileExists(D$(1) + Z1$(KK)) Then

                Open D$(1) + Z1$(KK) For Input As #1
                Input #1, B$
                For I = 0 To 14
                    Input #1, buildA$(I)
                    For J = 0 To 13
                        Input #1, A!(I, J)
                    Next
                    Input #1, W0!(I)
                    Input #1, W1!(I)
                Next

                'This currently causes an overflow
                'Are the stat files not getting created correctly??
                For L = 0 To 15: Input #1, Z!(L): Next
                For L = 0 To 15: Input #1, Z1!(L): Next
                Close #1

                Open D$(1) + Z1$(KK) + "S" For Input As #1
                Input #1, Y
                Close #1

                TS = TS + 1
                For I = 0 To 14: TT!(TS, I) = Z!(I): TT$(TS, I) = B$: GM(TS) = Y: Next
                TTS = TTS + 1
                For I = 0 To 14: TT1!(TTS, I) = Z1!(I): TT1$(TTS, I) = B$: GM1(TTS) = Y: Next
                For I = 0 To 14
                    If buildA$(I) = "XXX" Then 35

                    33 JB = JB + 1
                    AL$(JB) = buildA$(I)
                    TBN = TBN + 1
                    TMA$(TBN) = B$
                    GMA!(JB) = Y
                    K1 = 0
                    For I1 = 0 To 13
                        K1 = K1 + 1
                        AL(JB, K1) = A!(I, I1)
                    35 Next I1
                    W0L(JB) = W0!(I): W1L(JB) = W1!(I)
                Next I

            End If

        End If

    Next KK

    Print "SAVING LEADER FILES . . . . PLEASE WAIT"

    Open D$(1) + statFile$ + ".LDS" For Output As #1

    Write #1, JB, TS, TTS

    For I = 1 To 600: 
        Write #1, AL$(I), TMA$(I), GMA!(I)
        Color 14: Locate 18, 45
        TMR% = TMR% - 1
        Print Using "#####"; TMR%
        For I1 = 1 To 14:
            Write #1, AL(I, I1)
        Next I1

        Write #1, W0L(I)
        Write #1, W1L(I)
        Color 14: Locate 18, 45
        TMR% = TMR% - 1
        Print Using "#####"; TMR%: 
    Next I

    For I = 1 To 40: 
        For I1 = 0 To 14: 
            Write #1, TT!(I, I1), TT$(I, I1)
        Next

        Color 14: Locate 18, 45
        TMR% = TMR% - 1
        Print Using "#####"; TMR%
        Write #1, GM(I)
    Next

    For I = 1 To 40: 
        For I1 = 0 To 14: 
            Write #1, TT1!(I, I1), TT1$(I, I1)
        Next

        Color 14: Locate 18, 45
        TMR% = TMR% - 1
        Print Using "#####"; TMR%
        Write #1, GM1(I)
    Next

    Close #1

End Sub


'------------------------------
'     SortTeam Subroutine
'------------------------------
'...explanation...
Sub SortTeam (TTS, stpl, sortType%)

    Shared TYP!()
    Shared TMM$()

    For I = 2 To TTS
        stpl = stpl + 1
        Locate 5, 5: Print "sorting team "; stpl

        For J = TTS To I Step -1

            If sortType% = 1 Then 
                If TYP!(J - 1) < TYP!(J) Then 
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
                End If
            End If

            If sortType% = 2 Then 
                If TYP!(J - 1) > TYP!(J) Then 
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
                End If
            End If
            
        Next J

    Next I

End Sub


'------------------------------
'     SortPlayer Subroutine
'------------------------------
'...explanation...
Sub SortPlayer (PPP, stpl, sortType%)

    Shared TYP!()
    Shared TMM$(), TPP$()

    For I = 2 To PPP

        stpl = stpl + 1
        Locate 5, 5: Print "sorting player "; stpl

        For J = PPP To I Step -1

            If sortType% = 1 Then 
                If TYP!(J - 1) < TYP!(J) Then 
                    Swap TPP$(J - 1), TPP$(J)
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
                End If
            End If
            
            If sortType% = 2 Then 
                If TYP!(J - 1) > TYP!(J) Then 
                    Swap TPP$(J - 1), TPP$(J)
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
                End If
            End If
        Next J

    Next I

End Sub
