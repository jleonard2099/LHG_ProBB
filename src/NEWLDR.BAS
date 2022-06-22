Sub NEWLDR ()

    _Title "Basketball: The Pro Game - Stats / Standings"

	Do
		Cls

		5 Call TITLE
		'Erase menuChoices$
		menuChoices$(1) = "VIEW EXPANDED LEAGUE STATISTICS "
		menuChoices$(2) = "VIEW EXPANDED STANDINGS         "
		menuChoices$(3) = "RETURN TO LEAGUE LEADER MENU    "
		SELECTION% = 1
		Count% = 3
		Start% = 1
		SCANCODE% = 3
		FileFGColr% = 15: FileBGColr% = 1
		HiLiteFGColr% = 0: HiLiteBGColr% = 7
		Spacing% = 1
		Row% = 8
		Column% = 25

		Call box0(7, 24, 11, 57, 2, 15, 1)
		_MouseShow "DEFAULT"
		MMenuVert menuChoices$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
		_MouseHide

		Color , 0
		If Not (SCANCODE% = 27) Then
			Select Case SELECTION%
				Case 1: Call ViewLeagueStats
				Case 2: Call NEWLDRST
				Case 3: Cls
			End Select
			
	Loop Until SELECTION% = 3

End Sub


'----------------------------------------
'     	ViewLeagueStats Subroutine
'----------------------------------------
'... explain here ...
Sub ViewLeagueStats ()

	Shared PV

    K = 0
    K1 = 0
    PK = 0
    PK1 = 0
    JB = 0
    JP = 0
    TBN = 0
    TPN = 0
    TMR = 1280
    TS = 0
    PV = 0

    2500 For X = 1 To 40
        Z1$(X) = "XXX"
    Next
    Spec$ = D$(1) + "*.TMS"

    1440 null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
    Count% = DirCount%
    If Count% = 0 Then
        Locate 10, 8
        Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
        Exit Sub
    End If

    ReDim leagueFiles$(0 To Count%)
    For X = 1 To Count%
        leagueFiles$(X) = Space$(12)
    Next
    leagueFiles$(0) = Spec$
    Call ReadFile(leagueFiles$())
    'CALL SortStr(VARPTR(leagueFiles$1)), Count%, 0)

	Do

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

		If Not (ExitCode% = 27) Then
			targetFile$ = leagueFiles$(SELECTION%)
			Locate 2, 33: Print targetFile$
			Locate , 33: Print "ANY CHANGE (YN)";
			Color 15, 0
			3912 I$ = GetKeyPress$

			If UCase$(I$) = "N" Then

				3913 targetFile$ = RTrim$(targetFile$)
				targetFile$ = Left$(targetFile$, Len(targetFile$) - 4)
				LG$ = targetFile$

				LG$ = targetFile$
				Open D$(1) + targetFile$ + ".TMS" For Input As #2
				Input #2, LG$
				Input #2, D1$
				Input #2, D2$
				Input #2, D3$
				Input #2, D4$
				For X = 1 To 40
					Input #2, Z1$(X)
				Next
				Close 2

				Call BUILDSTATS(targetFile$)

				Do
					Cls

					70 Call TITLE
					'Erase menuChoices$
					menuChoices$(1) = "VIEW INDIVIDUAL PLAYER LEADERS  "
					menuChoices$(2) = "VIEW TEAM TOTALS                "
					menuChoices$(3) = "RETURN TO LEAGUE LEADER MENU    "
					SELECTION% = 1
					Count% = 3
					Start% = 1
					SCANCODE% = 3
					FileFGColr% = 15: FileBGColr% = 1
					HiLiteFGColr% = 0: HiLiteBGColr% = 7
					Spacing% = 1
					Row% = 8
					Column% = 25

					Call box0(7, 24, 11, 57, 2, 15, 1)
					_MouseShow "DEFAULT"
					MMenuVert menuChoices$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
					_MouseHide

					Color , 0
					If Not (SCANCODE% = 27 Then
						Select Case SELECTION%
							Case 1: Call ViewInidividualLeaders
							Case 2: Call ViewTotalLeaders
							Case 3: Cls
						End Select
					End If

				Loop Until SELECTION% = 3
				
			End If ' End of checking if Any Change Y/N
					
		End If 'End of checking return code from menu
		
	Loop Until UCase$(I$) = "N" Or ExitCode% = 27
	
End Sub



'----------------------------------------
'    ViewInidividualLeaders Subroutine
'----------------------------------------
'... explain here ...
Sub ViewInidividualLeaders ()

    Dim JB, TBN, TS

    Dim AL(600, 14), GM(40), GM1(40)
    Dim TT(40, 15), TT1(40, 15), W0L(600), W1L(600)

    Dim AL$(600)
    ReDim menuChoices$(1 To 3)

    '--> replace 600 with a CONST
    Dim TMA$(600), TT$(40, 15), TT1$(40, 15)
    ReDim GMA(600)

    Shared TMM$(), TP$(), TP1$()
	Shared PV, YY


    80 Color , 0
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

    200 For I = 0 To 15
        Color 14
        Locate I + 3, 8: Print Using "##"; I;: Print " ";
        Color 0: Print TP$(I);
        Color 14: Print Tab(40); Using "##"; I + 16;: Print " ";
        Color 0: Print TP$(I + 16)
    Next
    Print: Locate , 23: Input "SELECT NUMBER TO VIEW"; B9

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
			Call SelectOptions_NEWLDR(PV)
			Cls: PPP = 0
			For I = 1 To JB
				If AL(I, X) < GMA(I) * 1.52 Then 959 Else GoTo 955

				955 PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
				TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = AL(I, X) / AL(I, X1): GoTo 959

			959 Next I
			For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
				For J = PPP To I Step -1
					If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
					If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
				Next J
			Next I

			TVN = 2: QW = PPP
			Call GetListLength (QW, LG)
			Call ViewLeadersScreen (PV)

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
		Case 31: 70
		Case Is > 31: GoTo 200
	End Select

	If B9 <> 10 Then Call SelectOptions_NEWLDR(PV)

	Select Case B9
		Case 0: GoTo 400
		Case 1: GoTo 420
		Case 2 to 3: GoTo 400
		Case 4: GoTo 450
		Case 5: GoTo 660
		Case 6: GoTo 680
		Case 7: GoTo 480
		Case 8 to 9: GoTo 400
		Case 11: GoTo 500
		Case 12: GoTo 580
		Case 13 to 14: GoTo 400
		Case 15: GoTo 620
		Case 16: GoTo 400
		Case 17: GoTo 640
		Case 18 to 22: GoTo 400
		Case 23: GoTo 520
		Case 24 to 29: GoTo 540
		Case 30: GoTo 560
	End Select

    400 Cls: PPP = 0
    For I = 1 To JB
        If AL(I, X) <> 0 Then
			PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
			TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = AL(I, X)
		End If
    Next I
    For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
        For J = PPP To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 1: QW = PPP
	Call GetListLength (QW, LG)
	Call ViewLeadersScreen (PV)
    	
	420 Cls: PPP = 0
    For I = 1 To JB
        If AL(I, X) <> 0 Then
			PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
			TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = (AL(I, X) + 30) / 60
		End If
    Next I
    For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
        For J = PPP To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 1: QW = PPP
	Call GetListLength (QW, LG)
	Call ViewLeadersScreen (PV)


    450 Cls: PPP = 0
    For I = 1 To JB
        If AL(I, X) >= GMA(I) * 3.65 Then 
			PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
			TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = AL(I, X) / AL(I, X1): GoTo 459
		End If		
    Next I
    For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
        For J = PPP To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

	Call ViewLeadersScreen (PV)


    480 Cls: PPP = 0
    For I = 1 To JB
        If W0L(I) >= GMA(I) * .3 Then
			PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
			TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = W0L(I) / W1L(I)
		End If
    Next I
    For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
        For J = PPP To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 2: QW = PPP
	Call GetListLength (QW, LG)
	Call ViewLeadersScreen (PV)



    500 Cls: PPP = 0
    For I = 1 To JB
        If (AL(I, X) * 2) + AL(I, X1) + W0L(I) <> 0 Then
			PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
			TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = (AL(I, X) * 2) + AL(I, X1) + W0L(I): GoTo 509
			TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = 0
		End If
    Next I
    For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
        For J = PPP To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 1: QW = PPP
	Call GetListLength (QW, LG)
	Call ViewLeadersScreen (PV)


    520 Cls: PPP = 0
    For I = 1 To JB
        If Not ((AL(I, X) * 2) + AL(I, X1) + W0L(I) = 0 Or AL(I, X2) = 0) Then
			PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
			TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = (((AL(I, X) * 2) + AL(I, X1) + W0L(I))) * 48 / ((AL(I, X2) + 30) / 60)
		End If
    Next I
    For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
        For J = PPP To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 3: QW = PPP
	Call GetListLength (QW, LG)
	Call ViewLeadersScreen (PV)


    540 Cls: PPP = 0
    For I = 1 To JB
        If Not(AL(I, X) = 0 Or AL(I, X1) = 0) Then
			PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
			TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = (AL(I, X)) * 48 / ((AL(I, X1) + 30) / 60)
		End If
    Next I
    For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
        For J = PPP To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 3: QW = PPP
	Call GetListLength (QW, LG)
	Call ViewLeadersScreen (PV)


    560 Cls: PPP = 0
    For I = 1 To JB
        If Not (AL(I, X) = 0 Or AL(I, X1) = 0) Then
			PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
			TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = ((AL(I, X) + 30) / 60) / AL(I, X1)
		End If
    Next I
    For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
        For J = PPP To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 3: QW = PPP
	Call GetListLength (QW, LG)
	Call ViewLeadersScreen (PV)


    580 Cls: PPP = 0
    For I = 1 To JB
        If Not ((AL(I, X) * 2) + AL(I, X1) + W0L(I) = 0 Or AL(I, X2) = 0) Then
			PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
			TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = ((AL(I, X) * 2) + AL(I, X1) + W0L(I)) / AL(I, X2)
		End If
    Next I
    For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
        For J = PPP To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 3: QW = PPP
	Call GetListLength (QW, LG)
	Call ViewLeadersScreen (PV)


    620 Cls: PPP = 0
    For I = 1 To JB
        If AL(I, X) - AL(I, X1) <> 0 Then
			PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
			TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = AL(I, X) - AL(I, X1)
		End If
    Next I
    For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
        For J = PPP To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 1: QW = PPP
	Call GetListLength (QW, LG)
	Call ViewLeadersScreen (PV)


    640 Cls: PPP = 0
    For I = 1 To JB
        If AL(I, X) <> 0 Then
			PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
			TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = AL(I, X) / AL(I, X1)
		End If
    Next I
    For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
        For J = PPP To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 3: QW = PPP
	Call GetListLength (QW, LG)
	Call ViewLeadersScreen (PV)


    660 Cls: PPP = 0
    For I = 1 To JB
        If W0L(I) <> 0 Then
			PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
			TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = W0L(I)
		End If
    Next I
    For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
        For J = PPP To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 1: QW = PPP
	Call GetListLength (QW, LG)
	Call ViewLeadersScreen (PV)


    680 Cls: PPP = 0
    For I = 1 To JB
        If W1L(I) <> 0 Then
			PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
			TPP$(PPP) = AL$(I): TMM$(PPP) = TMA$(I): TYP(PPP) = W1L(I)
		End If
    Next I
    For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
        For J = PPP To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 1: QW = PPP
	Call GetListLength (QW, LG)
	Call ViewLeadersScreen (PV)

End Sub


'----------------------------------------
'     	ViewLeaderScreen Subroutine
'----------------------------------------
'... explain here ...
Sub ViewLeadersScreen ()

	Shared TMM$(), TPP$(), TP$(), TYP()

	Do
		700 Color 14, 0: Cls: Print "     PLAYER                  TEAM                     "; TP$(B9)
		For W = 1 To QW

			If W = 20 Or W = 40 Or W = 60 Or W = 80 Or W = 100 Or W = 120 Or W = 140 Or W = 160 Or W = 180 Or W = 200 Then 
				Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
				I$ = GetKeyPress$
				If Not (I$ = Chr$(27)) Then
					Cls
					Color 14: Print "     PLAYER                  TEAM                     "; TP$(B9)
				Else
					GoTo 80
				End If
			End If

			If W = 220 Or W = 240 Or W = 260 Or W = 280 Or W = 300 Or W = 320 Or W = 340 Or W = 360 Or W = 380 Or W = 400 Then 
				Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
				I$ = GetKeyPress$
				If Not (I$ = Chr$(27)) Then
					Cls
					Color 14: Print "     PLAYER                  TEAM                     "; TP$(B9)
				Else
					GoTo 80
				End If
			End If

			If W = 420 Or W = 440 Or W = 460 Or W = 480 Or W = 500 Or W = 520 Or W = 540 Or W = 560 Or W = 580 Or W = 600 Then 
				Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
				I$ = GetKeyPress$
				If Not (I$ = Chr$(27)) Then
					Cls
					Color 14: Print "     PLAYER                  TEAM                     "; TP$(B9)
				Else
					GoTo 80
				End If
			End If

			If W = 620 Or W = 640 Or W = 660 Then 
				Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
				I$ = GetKeyPress$
				If Not (I$ = Chr$(27)) Then
					Cls
					Color 14: Print "     PLAYER                  TEAM                     "; TP$(B9)
				Else
					GoTo 80
				End If
			End If

			Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Color 15: Print Tab(25); TMM$(W);
			Case Select TVN
				Case 1: Color 15: Print Tab(58); Using "#####"; TYP(W)
				Case 2: Color 15: Print Tab(58); Using ".###"; TYP(W)
				Case 3: Color 15: Print Tab(58); Using "###.#"; TYP(W)
			End Select
		Next W

		Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
		I$ = GetKeyPress$
		If Not (I$ = Chr$(27)) Then 
			Locate 22: Print "                                                                "
			Locate 22: Color 14: Print "VIEW AGAIN (Y/N)"
			I$ = GetKeyPress$
		End If
	
	Loop Until I$ = "N" Or I$ = Chr$(27)
    '719 GoTo 80


End Sub


'----------------------------------------
'     	  PrintLeaders Subroutine
'----------------------------------------
'... explain here ...
Sub PrintLeaders ()
    Shared TMM$(), TP$(), TYP()
	Shared LG

    850 LPrint Chr$(15)
    '    WIDTH "LPT1:1", 130
    Color 14, 0: Cls: LPrint "     PLAYER                  TEAM                     "; TP$(B9)
    For W = 1 To LG
        If W = 50 Or W = 100 Or W = 150 Or W = 200 Then 
			LPrint Chr$(12)
			LPrint "     PLAYER                  TEAM                     "; TP$(XX)
		End If
        If W = 250 Or W = 300 Or W = 350 Or W = 400 Then 
			LPrint Chr$(12)
			LPrint "     PLAYER                  TEAM                     "; TP$(XX)
		End If
        If W = 450 Or W = 500 Or W = 550 Or W = 600 Then
			LPrint Chr$(12)
			LPrint "     PLAYER                  TEAM                     "; TP$(XX)
		End If
        If W = 650 Or W = 700 Or W = 750 Or W = 800 Then
			LPrint Chr$(12)
			LPrint "     PLAYER                  TEAM                     "; TP$(XX)
		End If
        If W = 850 Or W = 900 Then
			LPrint Chr$(12)
			LPrint "     PLAYER                  TEAM                     "; TP$(XX)
		End If
		
		LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);
		
		Select Case TVN
			Case 1: LPrint Tab(58); Using "#####";
			Case 2: LPrint Tab(58); Using ".###"; TYP(W)
			Case 3: LPrint Tab(58); Using "###.#"; TYP(W)
		End Select
    Next W
    LPrint Chr$(18)
    '    WIDTH "LPT1:1", 80
    GoTo 70

End Sub


'----------------------------------------
'     	ViewTotalLeaders Subroutine
'----------------------------------------
'... explain here ...
Sub ViewTotalLeaders ()
    Shared TYP()
	Shared TMM$()

    2880 Color , 0: Cls: Print: Print: Print: Print: J = 21: ZZ = 3
    '
    Locate 1, 4: Color 15, 6: Print " " + Chr$(214); String$(66, 196); "· ";
    For II = 1 To J: Locate 1 + II, 4: Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";: Next
    Locate 2 + J, 4: Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";: Locate 10, 15
    For I = 0 To 19: Locate I + 2, 8: Color 11: Print Using "##"; I;: Print " ";: Color 0: Print TP1$(I): Next
    For I = 20 To 38: Locate I - 18, 38: Color 11: Print Using "##"; I;: Print " ";: Color 0: Print TP1$(I): Next

    3050 Color 14: Locate 22, 23: Input "SELECT NUMBER TO VIEW"; B9
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
	
	Call SelectOptions_NEWLDR (PV)
	
	Select Case B9
		Case 0 to 1: GoTo 3500
		Case 2: GoTo 3510
		Case 3 to 4: GoTo 3500
		Case 5: GoTo 3510
		Case 6 to 7: GoTo 3500
		Case 8: GoTo 3510
		Case 9: GoTo 3500
		Case 10: GoTo 3520
		Case 11: GoTo 3500
		Case 12: GoTo 3530
		Case 13 to 17: GoTo 3500
		Case 18: GoTo 3540
		Case 19 to 20: GoTo 3600
		Case 21: GoTo 3610
		Case 22 to 23: GoTo 3600
		Case 24: GoTo 3610
		Case 25 to 26: GoTo 3600
		Case 27: GoTo 3610
		Case 28: GoTo 3600
		Case 29: GoTo 3620
		Case 30: GoTo 3600
		Case 31: GoTo 3630
		Case 32 to 36: GoTo 3600
		Case 37: GoTo 3640
		Case 38: GoTo 70
		Case IS > 38: GoTo 3050
	End Select


    3500 Cls: For I = 1 To TS
    TMM$(I) = TT$(I, X): TYP(I) = TT(I, X): Next

    For I = 2 To TS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
        For J = TS To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 1: QW = TS
	Call PrintTeamView_NEWLDR (PV, QW, TVN)

    3510 Cls: For I = 1 To TS
        If TT(I, X1) = 0 Then 3513 Else GoTo 3512

        3512 TMM$(I) = TT$(I, X): TYP(I) = TT(I, X) / TT(I, X1): GoTo 3514

        3513 TMM$(I) = TT$(I, X): TYP(I) = 0

    3514 Next

    For I = 2 To TS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
        For J = TS To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 2: QW = TS
	Call PrintTeamView_NEWLDR (PV, QW, TVN)

    3520 Cls: For I = 1 To TS
    TMM$(I) = TT$(I, X): TYP(I) = TT(I, X) - TT(I, X1): Next

    For I = 2 To TS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
        For J = TS To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 1: QW = TS
	Call PrintTeamView_NEWLDR (PV, QW, TVN)
    3530 Cls: For I = 1 To TS
    TMM$(I) = TT$(I, X): TYP(I) = TT(I, X) / GM(I): Next

    For I = 2 To TS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
        For J = TS To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 3: QW = TS
	Call PrintTeamView_NEWLDR (PV, QW, TVN)
    3540 Cls: For I = 1 To TS
    TMM$(I) = TT$(I, X): TYP(I) = ((TT(I, X) * 2) + TT(I, X1) + TT(I, X2)) / GM(I): Next

    For I = 2 To TS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
        For J = TS To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        Next J
    Next I

    TVN = 3: QW = TS
	Call PrintTeamView_NEWLDR (PV, QW, TVN)
    3600 Cls: For I = 1 To TTS
    TMM$(I) = TT1$(I, X): TYP(I) = TT1(I, X): Next

    For I = 2 To TTS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
        For J = TTS To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
		Next J
    Next I

    TVN = 1: QW = TTS
	Call PrintTeamView_NEWLDR (PV, QW, TVN)
    3610 Cls
	For I = 1 To TTS
        If TT1(I, X1) = 0 Then 
			TMM$(I) = TT1$(I, X): TYP(I) = 0 
		Else 
			TMM$(I) = TT1$(I, X): TYP(I) = TT1(I, X) / TT1(I, X1)
		End If
    Next

    For I = 2 To TTS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
        For J = TTS To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
		Next J
    Next I

    TVN = 2: QW = TTS
	Call PrintTeamView_NEWLDR (PV, QW, TVN)

    3620 Cls: For I = 1 To TTS
    TMM$(I) = TT1$(I, X): TYP(I) = TT1(I, X) - TT1(I, X1): Next

    For I = 2 To TTS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
        For J = TTS To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
		Next J
    Next I

    TVN = 1: QW = TTS
	Call PrintTeamView_NEWLDR (PV, QW, TVN)
    3630 Cls: For I = 1 To TTS
    TMM$(I) = TT1$(I, X): TYP(I) = TT1(I, X) / GM1(I): Next

    For I = 2 To TTS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
        For J = TTS To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
		Next J
    Next I

    TVN = 3: QW = TTS
	Call PrintTeamView_NEWLDR (PV, QW, TVN)
    3640 Cls: For I = 1 To TTS
    TMM$(I) = TT1$(I, X): TYP(I) = ((TT1(I, X) * 2) + TT1(I, X1) + TT1(I, X2)) / GM1(I): Next

    For I = 2 To TTS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
        For J = TTS To I Step -1
            If JX = 1 Then If TYP(J - 1) < TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
            If JX = 2 Then If TYP(J - 1) > TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
		Next J
    Next I

    TVN = 3: QW = TTS
	Call PrintTeamView_NEWLDR (PV, QW, TVN)
    '3719 GoTo 2880
    'Return

End Sub


'----------------------------------------
'    PrintTeamView_NEWLDR Subroutine
'----------------------------------------
'... explain here ...
Sub PrintTeamView_NEWLDR (printType, QW, TVN)
	Shared TYP (), TP1$ ()
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
								Case 1: Color 15: Print Tab(32); Using "#####"; TYP(W)
								Case 2: Color 15: Print Tab(33); Using ".###"; TYP(W)
								Case 3: Color 15: Print Tab(33); Using "###.#"; TYP(W)
							End Select
						Else
							Exit Do
						End If
					End If
				Next W

				Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
				I$ = GetKeyPress$

				If Not (I$ = Chr$(27)) Then
					Locate 22: Print "                                                                "
					Locate 22: Color 14: Print "VIEW AGAIN (Y/N)"
					I$ = GetKeyPress$
				End If

			Loop Until I$ = "N"

			GoTo 2880
				
		Case 2:
			LPrint Chr$(15)
			'    WIDTH "LPT1:1", 130
			LPrint "     TEAM                     "; TP1$(B9)
			For W = 1 To QW
				Color 12: Print Using "###"; W;: Print " ";: Color 3: Print TMM$(W);
				Select Case TVN
					Case 1: Color 15: Print Tab(32); Using "#####"; TYP(W)
					Case 2: Color 15: Print Tab(33); Using ".###"; TYP(W)
					Case 3: Color 15: Print Tab(33); Using "###.#"; TYP(W)
				End Select
			Next W
			LPrint Chr$(12)
			LPrint Chr$(18)
			'    WIDTH "LPT1:1", 80
			GoTo 70

	End Select

End Sub


'----------------------------------------
'    SelectOptions_NEWLDR Subroutine
'----------------------------------------
'... explain here ...
Sub SelectOptions_NEWLDR (printType)

	5000 J = 6
	Locate 8, 8: Color 15, 1: Print " " + Chr$(214); String$(57, 196); "· ";
	For II = 1 To J: Locate 8 + II, 8: Print " " + Chr$(186); String$(57, 32); Chr$(186) + " ";: Next
	Locate 9 + J, 8: Print " " + Chr$(211); String$(57, 196); Chr$(189) + " ";: Locate 10, 15
	Color 3: Locate , 30: Print "SELECT OPTION:"
	Locate , 30: Color 15: Print "H";: Color 7: Print "IGHEST TO LOWEST"
	Locate , 30: Color 15: Print "L";: Color 7: Print "OWEST TO HIGHEST"
	Locate , 30: Color 15: Print "R";: Color 7: Print "E-SELECT CATEGORY"
	
	Do
		5002 I$ = GetKeyPress$
			Case "H": 
				Call PrintViewReselect(I2$)
				Select Case I2$
					Case "P": printType = 2	': GoTo 7015
					Case "V": printType = 1 	': GoTo 7015
					Case "R": 
						'GoTo 5017
						If ZZ = 1 GoTo 80
						If ZZ = 3 GoTo 2880
				End Select
				JX = 1
			Case "L": 
				Call PrintViewReselect(I2$)
				Select Case I2$
				Case "P": printType = 2	': GoTo 7015
				Case "V": printType = 1 	': GoTo 7015
				Case "R": 
					'GoTo 5017
					If ZZ = 1 GoTo 80
					If ZZ = 3 GoTo 2880
				End Select
				JX = 2
			Case "R": 
				If ZZ = 1 GoTo 80
				If ZZ = 3 GoTo 2880

    Loop Until I$ = "H" Or I$ = "L" Or I$ = "R"

End Sub


'----------------------------------------
'     PrintViewReselect Subroutine
'----------------------------------------
'... explain here ...
Sub PrintViewReselect (I$)

    Print: Print: Print: Print: Print
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
	Loop Until I$ = "P" Or I$ = "V" Or I$ = "R"
	
End Sub


'----------------------------------------
'     	  GetListLength Subroutine
'----------------------------------------
'... explain here ...
Sub GetListLength (maxQW, listLength)
'-- where do we get PV from?
	Shared PV, LG
	
	Do
		Color , 0
		Cls
		If PV <> 1 Then
			Print: Print: Print: Print: Print: J = 2

			8007 '
			Locate 10, 20
			Color 4, 15: Print " " + Chr$(214); String$(30, 196); "· ";
			For II = 1 To J
				Locate 10 + II, 20: Print " " + Chr$(186); String$(30, 32); Chr$(186) + " ";
			Next
			Locate 11 + J, 20: Print " " + Chr$(211); String$(30, 196); "½ ": Locate 11

			Locate , 25: Color 1: Print "MAXIMUM = "; maxQW
			Locate , 25: Input "ENTER LENGTH OF LIST"; listLength
		End If
		
    Loop Until LG <= QW Or PV = 1

    'GoTo 8015
    '8015 Return
	
End Sub
