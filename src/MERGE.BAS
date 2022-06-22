Sub MERGE ()

    ReDim menuChoices$(1 To 3)

    _Title "Basketball: The Pro Game - Align Stats"

	Do
		Cls
		1 Call TITLE
		'Erase menuChoices$
		menuChoices$(1) = "MERGE STATISTICAL FILES           "
		menuChoices$(2) = "ALIGN STATISTICAL FILES           "
		menuChoices$(3) = "EXIT TO MAIN MENU                 "
		SELECTION% = 1
		Count% = 3
		Start% = 1
		SCANCODE% = 3
		FileFGColr% = 15: FileBGColr% = 1
		HiLiteFGColr% = 0: HiLiteBGColr% = 7
		Spacing% = 1
		Row% = 8
		Column% = 25

		Call box0(7, 24, 11, 59, 2, 15, 1)
		_MouseShow "DEFAULT"
		MMenuVert menuChoices$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
		_MouseHide

		Color , 0
		If Not (SCANCODE% = 27) Then
			Select Case SELECTION%
				Case 1: Call MergeStats
				Case 2: Call ALIGN
			End Select
		End If

	Loop Until SCANCODE% = 27 Or SELECTION% = 3
    '-- this should be a "case else" type scenario
    '-- then the code ends
    '5 Run "HELLO.EXE 4"
    '5 Call REPLAY_STATS_MENU

End Sub


'----------------------------------------
'     	  MergeStats Subroutine
'----------------------------------------
'... explain here ...
Sub MergeStats ()

    Dim ORD$(0 To 100), O$(0 To 100)
    Dim A$(0 To 15), ARD$(0 To 14), mergeB$(0 To 26), H$(0 To 100), HRD$(0 To 100)

    Dim O%(0 To 100), ORD%(0 To 100), mergeT%(0 To 100), TRD%(0 To 100)

    Dim ARD(0 To 15, 0 To 14), W0(0 To 15), W1(0 To 15), W0RD(0 To 15), W1RD(0 To 15)
    Dim Z(0 To 15), Z1(0 To 15), ZRD(0 To 16), ZRD1(0 To 16)

    14 Color 12, 0: Cls: Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO MERGE"

    23 Color 15: Locate 4, 5: Print "EXAMPLE - A:"

    24 Locate , 5: Print "          C:\HAFFNER"

    25 Locate , 5: Print "          C:\HAFFNER\1980"

    26 Locate , 5: Print "          D:\BASKET"

	Do
		27 For J = 10 To 13: Locate J

		28 Print "                                                            ": Next

		35 Color 10: Locate 10, 5: Line Input "ROAD STATS:     "; PR$

		36 Locate , 5: Locate , 5: Line Input "CURRENT STATS:  "; PC$

		37 Color 14: Locate 13, 5: Print "ANY CHANGE (Y/N)"
		38 I$ = GetKeyPress$

    Loop Until UCase$(I$) = "N"

	Do
		3056 Color 15, 0: Cls
		JJ = 6
		SN = 1: Locate 6, 8: Color 15, 4
		Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
		Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
		Locate 7 + JJ, 8
		Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
		Locate , 12: Print "ENTER DISK ID: ": Print
		Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
		Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
		Print
		Locate , 12: Print "ESC TO RETURN TO MAIN MENU"
		3057 I$ = GetKeyPress$

		If I$ = Chr$(27) Then Z$ = "END": Cls: 'Exit Sub
		If I$ = Chr$(13) Then 
			Call SelectTeam_MERGE (YN$, NTMS)
			For I = 0 To 34
				mergeT%(I) = CVI(Q$(I + 10))
			Next
			Close 1

			9005 If Right$(A$, 1) = " " Then A$ = Left$(A$, Len(A$) - 1): GoTo 9005
			B$ = A$

		Else
			Locate 7, 27: Print I$
			Locate 7, 28: Line Input NY$
			NY$ = I$ + NY$
			Color 15, 0

			If NY$ = "" Then
				Call SelectTeam_MERGE (YN$, NTMS)
				For I = 0 To 34
					mergeT%(I) = CVI(Q$(I + 10))
				Next
				Close 1

				9005 If Right$(A$, 1) = " " Then A$ = Left$(A$, Len(A$) - 1): GoTo 9005
				B$ = A$
			Else
				YN$ = NY$
				'Erase teamNames$
				1101 NTMS = 0
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
						NTMS = NTMS + 1
						teamNames$(NTMS) = Q$(0): teamIndex%(NTMS) = I1
					End If
				Next
				Close 1

				Call SelectTeam_MERGE (YN$, NTMS)
				For I = 0 To 34
					mergeT%(I) = CVI(Q$(I + 10))
				Next
				Close 1

				9005 If Right$(A$, 1) = " " Then A$ = Left$(A$, Len(A$) - 1): GoTo 9005
				B$ = A$
			End If
		End If

		3060 If BO% = 1 Then 
			BO% = 0
		Else
			3058 Locate 2, 33: Color mergeT%(33), mergeT%(34): Print A$
			Color 15, 4: Locate , 33: Print "ANY CHANGE (YN)";
			I$ = GetKeyPress$
		End If
    Loop Until UCase$(I$) = "N"

    Color 15, 0

    B$ = A$

    For XX = 4 To 11: If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
    Next XX

    For XX = 12 To 14: If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
    Next XX

    QQ = 8
    For XX = 11 To 4 Step -1
        If Not (Mid$(B$, XX, 1) = "_") Then
            Exit For
        End If
    6615 Next XX

    Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)

	Do
		Cls
		Locate 10, 15: Print "ROAD STATS ARE ON DRIVE "; PR$
		Locate , 15: Print "MASTER STATS ARE ON DRIVE "; PC$
		Locate , 15: Print "TEAM TO MERGE STATS FOR IS "; A$
		Locate 14, 15: Print "ANY CHANGE (YN)"
		60 I$ = GetKeyPress$
		If UCase$(I$) = "N" Then

			If Not _FileExists(PR$ + "\" + Z$) Then 
				Cls
				Locate 2, 10
				Color 7: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
				Print
				Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$
			Else

				120 Open PR$ + "\" + Z$ For Input As #1

				121 Input #1, B$

				122 For I = 0 To 14
					Input #1, ARD$(I)

					123 For J = 0 To 13
						Input #1, ARD(I, J)
					Next
					Input #1, W0RD(I)
					Input #1, W1RD(I)
				Next

				124 For L = 0 To 15: Input #1, ZRD(L): Next

				125 For L = 0 To 15: Input #1, ZRD1(L): Next

				130 Close #1

				'180 GoTo 500

				500 Open PR$ + "\" + Z$ + "S" For Input As #1

				501 Input #1, YR%

				502 For I = 1 To 100: Input #1, ORD%(I): Next I

				503 For I = 1 To 100: Input #1, ORD$(I): Next I

				504 For I = 1 To 100: Input #1, TRD%(I): Next I

				505 For I = 1 To 100: Input #1, HRD$(I): Next I

				506 Close #1


				200 For J = 21 To 23
					Locate J
					1104 Print Space$(64)
				1105 Next

				If Not _FileExists(PC$ + "\" + Z$) Then 
					Cls
					Locate 2, 10
					Color 7: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
					Print
					Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$
				Else
					220 Open PC$ + "\" + Z$ For Input As #1

					221 Input #1, B$

					222 For I = 0 To 14: Input #1, A$(I)

					223 For J = 0 To 13: Input #1, A(I, J): Next: Input #1, W0(I): Input #1, W1(I): Next

					224 For L = 0 To 15: Input #1, Z(L): Next

					225 For L = 0 To 15: Input #1, Z1(L): Next

					230 Close #1


					600 Open PR$ + "\" + Z$ + "S" For Input As #1

					601 Input #1, Y%

					602 For I = 1 To 100: Input #1, O%(I): Next I

					603 For I = 1 To 100: Input #1, O$(I): Next I

					604 For I = 1 To 100: Input #1, mergeT%(I): Next I

					605 For I = 1 To 100: Input #1, H$(I): Next I

					606 Close #1

					'650 GoTo 1200

					1200 SEQ = 0

					1201 For K = 0 To 14

						1202 If ARD$(K) <> A$(K) Then SEQ = SEQ + 1

					1220 Next

					If SEQ = 0 Then 

						700 For K = 1 To YR%

							701 Y% = Y% + 1

							705 O%(Y%) = ORD%(K)

							710 O$(Y%) = ORD$(K)

							715 mergeT%(Y%) = TRD%(K)

							720 H$(Y%) = HRD$(K)

						750 Next

						'790 GoTo 319
						319 Locate 18, 20: Color 12: Print "MERGING STAT FILE AND SCHEDULE RESULTS . . . ."

						324 For I = 0 To 14
								For J = 0 To 12
									A(I, J) = A(I, J) + ARD(I, J)
								Next J
								W0(I) = W0(I) + W0RD(I)
								W1(I) = W1(I) + W1RD(I)
								If A(I, 13) < ARD(I, 13) Then A(I, 13) = ARD(I, 13) Else A(I, 13) = A(I, 13)
							Next I

						For L = 0 To 14
							Z(L) = Z(L) + ZRD(L): Z1(L) = Z1(L) + ZRD1(L)
						Next

						'326 GoTo 420
						
						420 Open PC$ + "\" + Z$ For Output As #1

						421 Write #1, B$

						422 For I = 0 To 14: Write #1, A$(I)

						423 For J = 0 To 13: Write #1, A(I, J): Next: Write #1, W0(I): Write #1, W1(I): Next

						424 For L = 0 To 15: Write #1, Z(L): Next

						425 For L = 0 To 15: Write #1, Z1(L): Next

						460 Close #1

						'495 GoTo 800

						800 Open PC$ + "\" + Z$ + "S" For Output As #1

						801 Write #1, Y%

						805 For I = 1 To 100: Write #1, O%(I): Next I

						810 For I = 1 To 100: Write #1, O$(I): Next I

						815 For I = 1 To 100: Write #1, mergeT%(I): Next I

						820 For I = 1 To 100: Write #1, H$(I): Next I

						840 Close #1

						850 Y = 0

						860 Locate 20, 20: Color 12: Print "RESULTS SUCCESSFULLY MERGED!!"

						'861 For J = 1 To 5000: Next J

						'875 GoTo 950
						'-- Do we need this???
						950 For J = 13 To 23
							951 Locate J
							952 Print Space$(65)
						953 Next

						985 Exit Sub
					Else
						Cls
						Color 14: Locate 5, 27: Print "PLAYERS NOT IN SEQUENCE !!"; DUMMY$

						Color 15: Locate 7, 20: Print "STATS WILL BE DISTORTED UNLESS THE ROAD"

						Locate , 19: Print "FILE LISTS THE PLAYERS IN THE SAME SEQUENCE"

						Locate , 24: Print "AS YOUR CURRENT PLAYER STAT FILE."

						Locate 11, 24: Color 12: Print "YOU MUST REALIGN ROAD STAT FILE OR"

						Locate , 31: Print "ENTER STATS MANUALLY."

						Locate 15, 29: Color 13: Line Input "HIT ENTER TO GO TO MENU"; DUMMY$
					
					End if 'Done checking sequence

				End If 'Done second check if file exists
				
			End If 'Done first check if file exists

		End If 'Done checking if we are continued 
	
	Loop Until UCase$(I$) = "N" Or SEQ = 0
    '1290 Exit Sub
	
End Sub


'----------------------------------------
'     SelectTeam_MERGE Subroutine
'----------------------------------------
'... explain here ...

Sub SelectTeam_MERGE (yearNumber$, numberTeams)

    1102 SELECTION% = 1
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
    20 MAMenu teamNames$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

	Select Case EXITCODE%
		Case 27: BO% = 1: Exit Sub 'Return
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

    T1 = teamIndex%(SELECTION%)

    Open D$(0) + "PROBBTMS." + yearNumber$ For Random As #1 Len = 1031
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
    Get #1, T1
    A$ = Q$(0)

End Sub
