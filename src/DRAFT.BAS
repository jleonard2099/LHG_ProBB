Sub DRAFT ()

    ReDim menuChoices$(1 To 3)
    Shared draftAA$(), draftB$(), draftYears$()
    Shared draftB%(), draftL%(), draftLT%(), draftT%()
    Dim TTL%(34)

    _Title "Basketball: The Pro Game - Team Draft"

    Do
        Selection% = 1
        Count% = 52
        Start% = 1
        FileFGColr% = 15: FileBGColr% = 4
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Rows% = 11
        Columns% = 5
        Spacing% = 10
        Row% = 6
        Column% = 1
        Color , 4
        Cls
        Locate , 16
        Color 14: Print "SELECT YEAR TO NORMALIZE DRAFTED PLAYERS TO"
        Color 15

        Do
            _MouseShow "DEFAULT"
            MAMenu draftYears$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide
        Loop Until ExitCode% = 27 Or ExitCode% = 13

        If ExitCode% = 27 Then 
            BO% = 1
        Else

            Do
                Locate 3, 33
                Color 15, 0
				Print draftYears$(Selection%)

                Color 15, 4

                Locate , 30: Print "ANY CHANGE (YN)";: I$ = GetKeyPress$
			
            Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

            YR% = Selection%
        End If

		If ExitCode% = 27 Then Exit Sub

    Loop Until UCase$(I$) = "N" Or Exitcode% = 27

	If ExitCode% <> 27 Then

		For I = 0 To 1: draftAA$(0, I) = "XXX": Next

		For I = 0 To 6: draftL%(0, I) = 0: Next

		For I = 0 To 34
			draftT%(0, I) = 0
			TTL%(I) = 0
		Next

		For I = 0 To 14
			For I1 = 0 To 34
				draftLT%(I, I1) = 0
			Next
		Next

		For I = 0 To 14
			draftB$(0, I, 0) = "XXX"
			draftB$(0, I, 1) = "X"
			For I1 = 0 To 19
				draftB%(0, I, I1) = 0
			Next
		Next

		Do
			Call TITLE
			'Erase menuChoices$
			menuChoices$(1) = "DRAFT A NEW TEAM         "
			menuChoices$(2) = "DRAFT TO AN EXISTING TEAM"
			menuChoices$(3) = "EXIT TO MAIN MENU        "
			Selection% = 1
			Count% = 3
			Start% = 1
			ExitCode% = 3
			FileFGColr% = 15: FileBGColr% = 1
			HiLiteFGColr% = 0: HiLiteBGColr% = 7
			Spacing% = 1
			Row% = 8
			Column% = 25

			Call box0(7, 24, 11, 50, 2, 15, 1)
			_MouseShow "DEFAULT"
			MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
			_MouseHide

			Color , 0

			If ExitCode% <> 27 Then
				If Selection% = 1 Then DNT% = 0: Call DraftNewTeam (TTL%())
				If Selection% = 2 Then DNT% = 1: Call DraftExistingTeam (TTL%())
			Else
				Selection% = 3
			End If

			'WE SHOULD NEVER GET HERE
			'1 Run "HELLO.EXE 2"
		Loop Until Selection% = 3

	End If 'Done checking for ESC

End Sub

'----------------------------------------
'       DraftNewTeam SubRoutine
'----------------------------------------
'...explanation...
Sub DraftNewTeam (TTL%())

    Shared draftAA$(), draftYN$()
    Shared draftL%(), draftLG%(), draftT%()

    Cls

    Color 14: Print "LEAGUE INFORMATION"
	Color 15

	draftL%(0, 0) = 80
    draftL%(0, 1) = draftLG%(YR%, 1)
    draftL%(0, 2) = draftLG%(YR%, 2)
    draftL%(0, 3) = draftLG%(YR%, 3)
    draftL%(0, 4) = draftLG%(YR%, 4)
    draftL%(0, 5) = draftLG%(YR%, 5)
    draftL%(0, 6) = 0

    130 Locate 2: Input "1. DISK ID "; draftYN$(0): If C1 = 1 Then 250

    205 Locate 3: Input "2. TEAM NAME "; draftAA$(0, 0): If C1 = 1 Then 250

    210 Locate 4: Input "3. TEAM NICKNAME "; draftAA$(0, 1): If C1 = 1 Then 250

    240 Locate 5: Input "4. TEXT COLOR (0-15) "; draftT%(0, 33): If C1 = 1 Then 250

    245 Locate 6: Input "5. BCKGRND COLOR (0-7) "; draftT%(0, 34): If C1 = 1 Then 250

    246 Locate 7: Input "6. TEAM ABBREV (IE. BOS) "; draftAA$(0, 2): If C1 = 1 Then 250

    247 Locate 8: Input "7. COACH'S NAME "; draftAA$(0, 3): If C1 = 1 Then 250

    248 Locate 9: Input "8. ARENA NAME "; draftAA$(0, 4): If C1 = 1 Then 250

    250 Locate 10: Color draftT%(0, 33), draftT%(0, 34): Print "SAMPLE TEXT"
    Color 7, 0

    255 C1 = 1: Locate 12: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 130, 205, 210, 240, 245, 246, 247, 248

    258 C1 = 0

    Call DoTheDraft (TTL%())

End Sub

'----------------------------------------
'      DraftExistingTeam SubRoutine
'----------------------------------------
'...explanation...
Sub DraftExistingTeam (TTL%())

    Shared draftAA$(), draftB$(), draftYears$(), draftYN$()
    Shared draftB%(), draftL%(), draftLT%(), draftT%()

    Do

        Color 15, 0: Cls
        JJ = 7
        Locate 6, 8: Color 15, 4
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

        Locate 7, 32
        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print "ABA SEASONS ARE FOLLOWED BY AN 'A' (IE. 75A)."
        Locate 7, 27: Input NY$

		If NY$ = "" And draftYN$(0) <> "" Then 
			'NY$ = draftYN$(0)
            Call SelectTeam_Draft(NTMS)
        Else
            draftYN$(0) = NY$
            Erase teamNames$

            NTMS = 0
            Open diskPaths$(0) + "PROBBTMS." + draftYN$(0) For Random As #1 Len = 1031

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
                    NTMS = NTMS + 1
                    teamNames$(NTMS) = Q$(0)
                    teamIndex%(NTMS) = I1
                End If
            Next
            Close 1

            'TS% = Int((NTMS - 1) / 80)

            Selection% = 1
            Count% = NTMS
            Start% = 1
            FileFGColr% = 15: FileBGColr% = 4
            HiLiteFGColr% = 0: HiLiteBGColr% = 7
            Rows% = 20
            Columns% = 4
            Spacing% = 6
            Row% = 4
            Column% = 1
            Color , 4: Cls
            Locate , 33: Color 14
            Print "TEAM 1 SELECTION": Color 15
            Locate 25, 1: Color 14
            Print "ESC";: Color 15
            Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
            Print "ENTER";: Color 15
            Print "-SELECTS TEAM";

            _MouseShow "DEFAULT"
            MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide

            If ExitCode% <> 27 Then

				T = teamIndex%(Selection%)

				Open diskPaths$(0) + "PROBBTMS." + draftYN$(0) For Random As #1 Len = 1031

				Field #1, 15 As Q$(0), 15 As Q$(1)

				For J% = 0 To 6
					Field #1, 30 + J% * 2 As X$, 2 As Q$(J% + 2)
				Next

				Field #1, 44 As X$, 2 As Q$(9)

				For J% = 0 To 34
					Field #1, 46 + J% * 2 As X$, 2 As Q$(J% + 10)
				Next

				For J% = 0 To 14
					Field #1, 116 + J% * 57 As X$, 15 As Q$(J% * 22 + 45), 2 As Q$(J% * 22 + 46)
					For J1 = 0 To 19
						Field #1, 133 + J% * 57 + J1 * 2 As X$, 2 As Q$(J% * 22 + 47 + J1)
					Next
				Next

				Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)

				Get #1, T

				draftAA$(0, 0) = RTrim$(Q$(0))
				draftAA$(0, 1) = Q$(1)

				For I = 0 To 6: 
					draftL%(0, I) = CVI(Q$(I + 2))
				Next

				For I = 0 To 34
					draftT%(0, I) = CVI(Q$(I + 10))
				Next

				For I = 0 To 14
					draftB$(0, I, 0) = RTrim$(Q$(22 * I + 45))
					draftB$(0, I, 1) = RTrim$(Q$(22 * I + 46))

					For I1 = 0 To 19
						draftB%(0, I, I1) = CVI(Q$(22 * I + 47 + I1))
					Next
				Next

				draftAA$(0, 2) = RTrim$(Q$(375))
				draftAA$(0, 3) = RTrim$(Q$(376))
				draftAA$(0, 4) = RTrim$(Q$(377))

				Close 1

				For I1 = 0 To 14
					For I = 0 To 34
						draftLT%(I1, I) = draftT%(0, I)
					Next
				Next

			End If 'Done checking for ESC

		End If 'Done checking empty input

        If BO% = 1 Then 
            BO% = 0
        Else
            Locate 2, 33
			Color draftT%(0, 33), draftT%(0, 34): Print draftAA$(0, 0)

			Do
				Locate , 33
				Color 15, 4: Print "ANY CHANGE (YN)"
				I$ = GetKeyPress$
			Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"
        End If

    Loop Until UCase$(I$) = "N"

    Call DoTheDraft (TTL%())

End Sub

'----------------------------------------
'   	Draft_Unknown SubRoutine
'----------------------------------------
' I can't tell that this code was ever used
Sub Draft_Unknown ()

    260 Cls: Color 14: Print "TEAM OFFENSIVE INFORMATION": Color 15

    265 Locate 2: Input " 1. FGM "; draftT%(0, 6): If C1 = 1 Then 335

    270 Locate 3: Input " 2. FGA "; draftT%(0, 5): If C1 = 1 Then 335

    275 Locate 4: Input " 3. FTM "; draftT%(0, 8): If C1 = 1 Then 335

    280 Locate 5: Input " 4. FTA "; draftT%(0, 7): If C1 = 1 Then 335

    285 Locate 6: Input " 5. OFF REB "; draftT%(0, 9): If C1 = 1 Then 335

    290 Locate 7: Input " 6. DEF REB "; draftT%(0, 10): If C1 = 1 Then 335

    291 If draftT%(0, 9) = 0 And RB = 0 Then draftT%(0, 9) = .32 * draftT%(0, 10): draftT%(0, 10) = draftT%(0, 10) - draftT%(0, 9): Locate 6: Print " 5. OFF REB "; draftT%(0, 9): Locate 7: Print " 6. DEF REB "; draftT%(0, 10)

    295 Locate 8: Input " 7. ASSISTS "; draftT%(0, 11): If C1 = 1 Then 335

    300 Locate 9: Input " 8. FOULS "; draftT%(0, 12): If C1 = 1 Then 335

    305 Locate 10: Input " 9. DISQ "; draftT%(0, 13): If C1 = 1 Then 335

    310 Locate 11: Input "10. STEALS "; draftT%(0, 14): If C1 = 1 Then 335

    315 Locate 12: Input "11. TURNOVERS "; draftT%(0, 15): If C1 = 1 Then 335

    320 Locate 13: Input "12. BLK SHOTS "; draftT%(0, 16): If C1 = 1 Then 335

    325 Locate 14: Input "13. 3-FGM "; draftT%(0, 18): If C1 = 1 Then 335

    330 Locate 15: Input "14. 3-FGA "; draftT%(0, 17): If C1 = 1 Then 335

    335 C1 = 1: Locate 17: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 265, 270, 275, 280, 285, 290, 295, 300, 305, 310, 315, 320, 325, 330

	'---
    336 C1 = 0

    340 Cls: Color 14: Print "TEAM DEFENSIVE INFORMATION": Color 15

    345 Locate 2: Input " 1. FGM "; draftT%(0, 20): If C1 = 1 Then 415

    350 Locate 3: Input " 2. FGA "; draftT%(0, 19): If C1 = 1 Then 415

    355 Locate 4: Input " 3. FTM "; draftT%(0, 22): If C1 = 1 Then 415

    360 Locate 5: Input " 4. FTA "; draftT%(0, 21): If C1 = 1 Then 415

    365 Locate 6: Input " 5. OFF REB "; draftT%(0, 23): If C1 = 1 Then 415

    370 Locate 7: Input " 6. DEF REB "; draftT%(0, 24): If C1 = 1 Then 415

    371 If draftT%(0, 23) = 0 And RB = 0 Then draftT%(0, 23) = .35 * draftT%(0, 24): draftT%(0, 24) = draftT%(0, 24) - draftT%(0, 23): Locate 6: Print " 5. OFF REB "; draftT%(0, 23): Locate 7: Print " 6. DEF REB "; draftT%(0, 24)

    375 Locate 8: Input " 7. ASSISTS "; draftT%(0, 25): If C1 = 1 Then 415

    380 Locate 9: Input " 8. FOULS "; draftT%(0, 26): If C1 = 1 Then 415

    385 Locate 10: Input " 9. DISQ "; draftT%(0, 27): If C1 = 1 Then 415

    390 Locate 11: Input "10. STEALS "; draftT%(0, 28): If C1 = 1 Then 415

    395 Locate 12: Input "11. TURNOVERS "; draftT%(0, 29): If C1 = 1 Then 415

    400 Locate 13: Input "12. BLK SHOTS "; draftT%(0, 30): If C1 = 1 Then 415

    405 Locate 14: Input "13. 3-FGM "; draftT%(0, 32): If C1 = 1 Then 415

    410 Locate 15: Input "14. 3-FGA "; draftT%(0, 31): If C1 = 1 Then 415

    415 C1 = 1: Locate 17: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 345, 350, 355, 360, 365, 370, 375, 380, 385, 390, 395, 400, 405, 410

	'---
    416 C1 = 0

    417 If draftT%(0, 28) = 0 And ST = 0 Then draftT%(0, 28) = draftT%(0, 5) * .1

    418 If draftT%(0, 14) = 0 And ST = 0 Then draftT%(0, 14) = draftT%(0, 19) * .1

    419 If draftT%(0, 16) = 0 And BK = 0 Then draftT%(0, 16) = draftT%(0, 19) * .06

    420 If draftT%(0, 30) = 0 And BK = 0 Then draftT%(0, 30) = draftT%(0, 5) * .06

    421 If draftT%(0, 29) = 0 And T0 = 0 Then draftT%(0, 29) = 18 * draftL%(0, 0): draftT%(0, 15) = draftT%(0, 29)

End Sub

'----------------------------------------
'       DoTheDraft SubRoutine
'----------------------------------------
'...explanation...
Sub DoTheDraft (TTL%())

    Dim NP

    Shared draftAA$(), draftB$(), draftYears$(), draftYN$()
    Shared draftB%(), draftL%(), draftLG%(), draftLT%(), draftT%()

    Do

        Color 0, 0
        Cls
        Color draftT%(0, 33), draftT%(0, 34): Print draftAA$(0, 0);
        Color 14, 0: Print "     (DRAFTING)"

        Print
        Print "PLAYERS"

        For I = 0 To 14
            Color 2: Print Using "## "; I;
            Color 15: Print draftB$(0, I, 0); " "; draftB$(0, I, 1)
        Next

        Locate 21, 1
        Color 14: Print "(1) ";: Color 15: Print "DRAFT PLAYER"
        Color 14: Print "(2) ";: Color 15: Print "SAVE TEAM"
        I$ = GetKeyPress$

        Select Case (I$)
            Case "1": 
                'Drafting
                Do
                    Locate 21, 20: Print "SLOT # (0-14) ";
                    Input B
                Loop Until B >= 1 And B <= 14

                Do
                    1440 Color 15, 0: Cls
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
                    Locate , 12: Print "ABA SEASONS ARE FOLLOWED BY AN 'A' (IE. 75A)."
                    Locate 7, 27: Input NY$
					If NY$ = "" And draftYN$(1) <> "" Then 
						'NY$ = draftYN$(1)
                        Call SelectTeam_Draft (NTMS)
					Else
                        draftYN$(1) = NY$
                        Erase teamNames$

                        NTMS = 0

                        Open diskPaths$(0) + "PROBBTMS." + draftYN$(1) For Random As #1 Len = 1031
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

                        'Somehow this isn't populating all teams?
                        For I1 = 1 To Y / 1031
                            Get #1, I1
                            CK% = CVI(Q$(9))
                            If CK% = 999 Then
                                NTMS = NTMS + 1
                                teamNames$(NTMS) = Q$(0)
                                teamIndex%(NTMS) = I1
                            End If
                        Next

                        Close 1

                        Call SelectTeam_Draft (NTMS)
                    End If

                    If BO% = 1 Then 
                        BO% = 0
                    Else
                        Locate 2, 33
						Color draftT%(1, 33), draftT%(1, 34): Print draftAA$(1, 0)
						
						Color 15, 4

						Do
							Locate , 33: Print "ANY CHANGE (YN)";
							I$ = GetKeyPress$
						Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"
                    End If

                Loop Until UCase$(I$) = "N"

                Color , 0: Cls
				Color draftT%(1, 33), draftT%(1, 34): Print draftAA$(1, 0)
				Color 14, 0

                Print
				Print "PLAYERS"

                For I = 0 To 14
                    Color 2: Print Using "## "; I;
					Color 15: Print draftB$(1, I, 0); " "; draftB$(1, I, 1)
                Next

                Do
                    Color 14
					Locate 21, 1: Print "SELECT PLAYER # TO DRAFT ";

                    Input B1

                Loop Until B1 >= 0 And B1 <= 14

                For I = 0 To 1: draftB$(0, B, I) = draftB$(1, B1, I): Next
                For I = 0 To 19: draftB%(0, B, I) = draftB%(1, B1, I): Next
                For I = 0 To 34: draftLT%(B, I) = draftT%(1, I): Next

                draftB%(0, B, 2) = draftB%(0, B, 2) * (draftLG%(YR%, 1) / draftL%(1, 1))
                draftB%(0, B, 6) = draftB%(0, B, 6) * (draftLG%(YR%, 2) / draftL%(1, 2))
                draftB%(0, B, 7) = draftB%(0, B, 7) * (draftLG%(YR%, 2) / draftL%(1, 2))

                FG = Int(draftB%(1, B1, 3) / draftB%(1, B1, 2) * 100 + .5)
                FG = FG * (draftLG%(YR%, 3) / draftL%(1, 3))

                draftB%(0, B, 3) = FG / 100 * draftB%(0, B, 2)
                draftLT%(B, 5) = (draftLG%(YR%, 1) / draftL%(1, 1)) * draftLT%(B, 5)
                draftLT%(B, 19) = draftLT%(B, 19) * (draftLG%(YR%, 1) / draftL%(1, 1))
                draftLT%(B, 9) = draftLT%(B, 9) * (draftLG%(YR%, 2) / draftL%(1, 2))
                draftLT%(B, 10) = draftLT%(B, 10) * (draftLG%(YR%, 2) / draftL%(1, 2))
                draftLT%(B, 23) = draftLT%(B, 23) * (draftLG%(YR%, 2) / draftL%(1, 2))
                draftLT%(B, 24) = draftLT%(B, 24) * (draftLG%(YR%, 2) / draftL%(1, 2))
                FG = Int(draftT%(1, 6) / draftT%(1, 5) * 100 + .5)
                FG = FG * (draftLG%(YR%, 3) / draftL%(1, 3))

                draftT%(0, 6) = FG / 100 * draftT%(0, 5)
                FG = Int(draftT%(1, 20) / draftT%(1, 19) * 100 + .5)
                FG = FG * (draftLG%(YR%, 3) / draftL%(1, 3))

                draftT%(0, 20) = FG / 100 * draftT%(0, 19)

            Case "2": 
                ' Save Team
                Cls

                NP = 0
                For I = 0 To 14
                    If draftB$(0, I, 0) <> "XXX" Then NP = NP + 1
                Next
                For I = 0 To 14
                    If draftB$(0, I, 0) <> "XXX" Then
                        For I1 = 0 To 32
                            TTL%(I1) = TTL%(I1) + draftLT%(I, I1)
                        Next I1
                    End If
                Next I

                For I1 = 0 To 32
                    TTL%(I1) = TTL%(I1) / NP
                    draftT%(0, I1) = TTL%(I1)
                Next

                Open diskPaths$(0) + "PROBBTMS." + draftYN$(0) For Random As #1 Len = 1031

                Y = LOF(1)

                If DNT% = 0 Then teamIdx = Y / 1031 + 1
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

                LSet Q$(0) = draftAA$(0, 0): LSet Q$(1) = draftAA$(0, 1): For I = 0 To 6: LSet Q$(I + 2) = MKI$(draftL%(0, I)): Next
                LSet Q$(9) = MKI$(999)

                For I = 0 To 34: LSet Q$(I + 10) = MKI$(draftT%(0, I)): Next

                For I = 0 To 14
                    LSet Q$(22 * I + 45) = draftB$(0, I, 0)
                    LSet Q$(22 * I + 46) = draftB$(0, I, 1)
                    For I1 = 0 To 19
                        LSet Q$(22 * I + 47 + I1) = MKI$(draftB%(0, I, I1))
                    Next
                Next

                LSet Q$(375) = draftAA$(0, 2): LSet Q$(376) = draftAA$(0, 3): LSet Q$(377) = draftAA$(0, 4)
                Put #1, teamIdx
                Close 1

        End Select

    Loop Until I$ = "2" 'Or I$ = "1"

End Sub

'----------------------------------------
'   SelectTeam_Draft SubRoutine
'----------------------------------------
'...explanation...
Sub SelectTeam_Draft (numberTeams)

    Shared draftAA$(), draftB$(), draftYears$(), draftYN$()
    Shared draftB%(), draftL%(), draftT%()

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
    Print "TEAM 2 SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

		teamIdx = teamIndex%(Selection%)

		Open diskPaths$(0) + "PROBBTMS." + draftYN$(1) For Random As #1 Len = 1031

		Field #1, 15 As Q$(0), 15 As Q$(1)

		For J% = 0 To 6
			Field #1, 30 + J% * 2 As X$, 2 As Q$(J% + 2)
		Next

		Field #1, 44 As X$, 2 As Q$(9)

		For J% = 0 To 34
			Field #1, 46 + J% * 2 As X$, 2 As Q$(J% + 10)
		Next

		For J% = 0 To 14
			Field #1, 116 + J% * 57 As X$, 15 As Q$(J% * 22 + 45), 2 As Q$(J% * 22 + 46)
			For J1 = 0 To 19
				Field #1, 133 + J% * 57 + J1 * 2 As X$, 2 As Q$(J% * 22 + 47 + J1)
			Next
		Next

		Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)
		Get #1, teamIdx

		draftAA$(1, 0) = RTrim$(Q$(0))
		draftAA$(1, 1) = Q$(1)

		For I = 0 To 6: draftL%(1, I) = CVI(Q$(I + 2)): Next

		For I = 0 To 34
			draftT%(1, I) = CVI(Q$(I + 10))
		Next

		For I = 0 To 14
			draftB$(1, I, 0) = RTrim$(Q$(22 * I + 45))
			draftB$(1, I, 1) = RTrim$(Q$(22 * I + 46))

			For I1 = 0 To 19
				draftB%(1, I, I1) = CVI(Q$(22 * I + 47 + I1))
			Next
		Next

		draftAA$(1, 2) = RTrim$(Q$(375))
		draftAA$(1, 3) = RTrim$(Q$(376))
		draftAA$(1, 4) = RTrim$(Q$(377))

		Close 1

	Else
		BO% = 1
	End If

End Sub
