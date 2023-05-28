Sub GPMIN ()

    ReDim menuChoices$(1 To 2)

    _Title "Basketball: The Pro Game - Adjust Game Time"

    ' Postions (Guard / Forward / Center)
    Dim P$(0 To 2)

    P$(0) = "G"
	P$(1) = "F"
	P$(2) = "C"

    Selection% = 1

    Do

        Call TITLE

        'Erase menuChoices$
        menuChoices$(1) = "ADJUST GAMES AND MINUTES PER GAME "
        menuChoices$(2) = "EXIT TO MAIN MENU                 "
        Count% = 2
        Start% = 1
        ExitCode% = 2
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 10, 59, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            If Selection% = 1 Then Call AdjustGames (T1)
        Else
            Selection% = 2
        End If

    Loop Until Selection% = 2 Or ExitCode% = 27

End Sub

'----------------------------------------
'		 AdjustGames subroutine
'----------------------------------------
'...explanation...
Sub AdjustGames (selectedIndex)

	Shared adjustB$(), adjustT$()
	Shared adjustB%(), adjustL%(), adjustT%()

	'-- this is basically a team selection routine
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
				YN$ = RTrim$(YN$)
				Call SelectTeam_GPMIN(YN$, NTMS, selectedIndex)
			Else
				Locate 7, 27: Print I$
				Locate 7, 28: Line Input NY$
				NY$ = I$ + NY$
				Color 15, 0
				If NY$ = "" And YN$ <> "" Then 
					'NY$ = YN$
					YN$ = RTrim$(YN$)
					Call SelectTeam_GPMIN(YN$, NTMS, selectedIndex)
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

					For I1 = 1 to fileLength& / 1031
						Get #1, I1
						CK% = CVI(Q$(9))
						If CK% = 999 Then
							NTMS = NTMS + 1
							teamNames$(NTMS) = Q$(0)
							teamIndex%(NTMS) = I1
						End If
					Next
					Close 1

					Call SelectTeam_GPMIN(YN$, NTMS, selectedIndex)

				End If

			End If

			If BO% = 1 Then 
				BO% = 0
			Else
				Locate 2, 33
				Color adjustT%(33), adjustT%(34): Print adjustT$(0)

				Color 15, 4

				Do
					Locate 3, 33: Print "ANY CHANGE (YN)";					
					A$ = GetKeyPress$
				Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

			End If

		End If 'Done checking for ESC

	Loop Until UCase$(A$) = "N" Or I$ = Chr$(27)

	If I$ <> Chr$(27) Then
		Color 15, 0

		Do

			Color , 0
			Cls
			Color adjustT%(33), adjustT%(34): Print adjustT$(0)
			Color 15, 0
			Print "   PLAYERS                 ACTUAL           ADJUSTED"
			Print "                          GP   MPG          GP    MPG"

			gamesPlayed = 0
			MIN = 0

			For I = 0 To 14
				If adjustB%(I, 16) >= 0 Or adjustB%(I, 17) >= 0 Then 
					adjustB%(I, 16) = adjustB%(I, 0) * -1
					adjustB%(I, 17) = adjustB%(I, 1) * -1
				End If

				gamesPlayed = gamesPlayed + adjustB%(I, 0)
				MIN = MIN + adjustB%(I, 17) * -1

				If Left$(adjustB$(I, 0), 3) <> "XXX" Then
					Print Using "## "; I;: Print adjustB$(I, 0); " "; adjustB$(I, 1); Tab(27);
					actualMPG! = adjustB%(I, 1) / adjustB%(I, 0)
					adjustedMPG! = adjustB%(I, 17) / adjustB%(I, 16)
					adjustedGP! = adjustB%(I, 16) * -1
					'Print Using "##   ##.#         ##   ##.#"; adjustB%(I, 0); actualMPG!; adjustedGP; adjustedMPG!
					Print Using "##"; adjustB%(I, 0);: Print "  ";
					Print Using "##.#"; actualMPG!;: Print "         ";
					Print Using "##"; adjustedGP!;: Print "   ";
					Print Using "##.#"; adjustedMPG!
				End If
			Next

			Print "15 END AND SAVE CHANGES"
			Print
			Print "TEAM MIN/G SHOULD BE APPROXIMATELY 240, OVERTIME WILL INCREASE THIS FIGURE"
			teamTotalMins! = MIN / adjustT%(0)
			Print Using "TEAM MIN/G = ###.#"; teamTotalMins!

			Do
				Locate 23, 1
				Input "ENTER SELECTION "; B1

				If B1 < 15 Then 
					Locate 23, 1: Print Space$(38);
					Locate 23, 1: Print adjustB$(B1, 0); " ";
					Input "GAMES PLAYED "; gamesPlayed

					Locate 23, 1: Print Space$(38);
					Locate 23, 1: Print adjustB$(B1, 0); " ";
					Input "MINUTES PER GAME "; minutesPerGame

					adjustB%(B1, 16) = gamesPlayed * -1
					adjustB%(B1, 17) = minutesPerGame * gamesPlayed * -1
				Else
					YN$ = RTrim$(YN$)

					Open diskPaths$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031

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
					LSet Q$(0) = adjustT$(0): LSet Q$(1) = adjustT$(1): For I = 0 To 6: LSet Q$(I + 2) = MKI$(adjustL%(I)): Next
					LSet Q$(9) = MKI$(999)
					For I = 0 To 34: LSet Q$(I + 10) = MKI$(adjustT%(I)): Next
					For I = 0 To 14
						LSet Q$(22 * I + 45) = adjustB$(I, 0)
						LSet Q$(22 * I + 46) = adjustB$(I, 1)
						For I1 = 0 To 19
							LSet Q$(22 * I + 47 + I1) = MKI$(adjustB%(I, I1))
					Next: Next
					LSet Q$(375) = adjustT$(2): LSet Q$(376) = adjustT$(3): LSet Q$(377) = adjustT$(4)
					Put #1, selectedIndex

					Close 1

				End If

			Loop Until B1 >= 0 And B1 <= 15

		Loop Until B1 = 15

	End If 'Done checking for ESC

End Sub

'----------------------------------------
'		 AdjustGames subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_GPMIN (yearNumber$, numberTeams, selectedIndex)

	Shared adjustB$(), adjustT$()
	Shared adjustB%(), adjustL%(), adjustT%()

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

		adjustT$(0) = RTrim$(Q$(0))
		adjustT$(1) = RTrim$(Q$(1))

		For I = 0 To 6: adjustL%(I) = CVI(Q$(I + 2)): Next
		For I = 0 To 34: adjustT%(I) = CVI(Q$(I + 10)): Next
		For I = 0 To 14
			adjustB$(I, 0) = Q$(22 * I + 45)
			adjustB$(I, 1) = Q$(22 * I + 46)
			For I1 = 0 To 19
				adjustB%(I, I1) = CVI(Q$(22 * I + 47 + I1))
			Next
		Next

		adjustT$(2) = RTrim$(Q$(375))
		adjustT$(3) = RTrim$(Q$(376))
		adjustT$(4) = RTrim$(Q$(377))

		Close

	Else
		BO% = 1
	End If

End Sub
