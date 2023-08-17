'----------------------------------------
'          CAREER Subroutine
'----------------------------------------
'This routine gives the user a menu from 
'which they can choose several options
'for managing career files
Sub CAREER ()

    _Title "Basketball: The Pro Game - Career Stats"

    ReDim menuChoices$(1 To 6)

    Do

        Color 15, 0
        Cls

        Call TITLE_CAREER

        menuChoices$(1) = "CREATE AND MERGE INITIAL STAT FILE"
        menuChoices$(2) = "MERGE / UPDATE CAREER STATS       "
        'menuChoices$(3) = "VIEW / PRINT CAREER STATS        "
        menuChoices$(3) = "VIEW CAREER FILE                  "
        menuChoices$(4) = "PRINT CAREER FILE                 "
        menuChoices$(5) = "VIEW / PRINT CAREER RECORDS       "
        menuChoices$(6) = "EXIT TO MAIN MENU                 "
        Selection% = 1
        Count% = 6
        Start% = 1
        ExitCode% = 6
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 14, 59, 2, 15, 1)
        
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1: 
                    Call CreateCareerFile

                Case 2: 
                    Call MergeCareerFiles

                Case 3, 4: 
                    Call CARVIEW

                Case 5: 
                    Call ViewPrintCareerRecords

            End Select

        End If

    Loop Until Selection% = 6 Or ExitCode% = 27

End Sub


'----------------------------------------
'       CreateCareerFile Subroutine
'----------------------------------------
'This routine assists the user with the
'creation of a team's first career file.
'This naturally means it will guide them 
'through also adding the first year's
'stats to the file.
Sub CreateCareerFile ()

    Do

        Cls
        Color 14

        Locate 5, 27: Print " - - - C A U T I O N - - -"

        Color 15
        Locate 8, 18: Print "THIS OPTION SHOULD ONLY BE USED WHEN CREATING"
        Locate , 18: Print "AND MERGING THE INITIAL STAT FILE.  IF YOU ARE"
        Locate , 18: Print "ADDING AN ADDITIONAL YEAR TO AN EXISTING FILE,"
        Locate , 18: Print "            USE OPTION #2."
        Print
        Print
        Print

        Color 12
        Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"

        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

    If UCase$(I$) = "Y" Then
        
        Call MergeFirstYear

    End If 'Done checking if user is ready

End Sub


'----------------------------------------
'        MergeFirstYear Subroutine
'----------------------------------------
'This routine actually prompts the user for 
'parameters to be used for merging the first
'year's worth of stats to the career file
Sub MergeFirstYear ()

    Dim initA!(62, 15)
    Dim initA$(62)
    Dim careerW0!(62), careerW1!(62)

    Shared AR$()
    Shared ars!(), compareZ!(), compareZ1!()
    Shared W0S!(), W1S!()


    NB = -1

	Do

		Cls

		For J = 15 To 18: 
			Locate J
			Print Space$(68)
		Next

		Color 11
		Locate 15, 5
		Input "ENTER NAME OF CAREER STAT FILE "; careerFile$

		Call ChooseTeam(YN$, NTMS, teamNbr, team$)

		If teamNbr <> 0 Then

			statFile$ = nameStatFile$(team$)

			Do

				Color 15, 0
				Cls

				Locate 8, 25: Input "ENTER YEAR TO ADD (EX. - 1961)"; statYear
				Locate 9, 25: Print "ANY CHANGE? (Y/N)"

				J$ = GetKeyPress$

			Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N" Or I$ = Chr$(27)

			If UCase$(J$) = "N" Then

				Do
                    Locate , 5
                    Print "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"
					K$ = GetKeyPress$
				Loop Until UCase$(K$) = "Y" Or UCase$(K$) = "N" Or K$ = Chr$(27)

				If UCase$(K$) = "Y" Then finalUpdate% = 1 Else finalUpdate% = 0

				If Not _FileExists(diskPaths$(1) + statFile$) Then

                    MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
                    BoxWidth = 24
                    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                    Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                    Call CSMsgBox(MsgText$, BoxWidth, Pass)
                    I$ = GetKeyPress$
                    Call CSMsgBox("", 0, Pass)

				Else

					Locate 20, 5
					Color 15
					Print "MERGING CAREER STATS AND UPDATING CAREER LEADER FILE . . . . PLEASE WAIT"

                    TMR = 945

					Open diskPaths$(1) + statFile$ For Input As #1

					Input #1, B$

					For I = 0 To 14:

						Input #1, initA$(I)

						For J = 0 To 13: Input #1, initA!(I, J): Next

						Input #1, careerW0!(I)
						Input #1, careerW1!(I)

						Color 5
						Locate 21, 38
						TMR = TMR - 1
						Print Using "####"; TMR

					Next

					For L = 0 To 15: Input #1, compareZ!(L): Next

					For L = 0 To 15: Input #1, compareZ1!(L): Next

					Close #1

					For I = 0 To 61

						N = 1

						If initA$(I) = "END" Or initA$(I) = "XXX" Or initA$(I) = "" Then
							initA$(I) = ""
						Else
							NB = NB + 1
						End If

						For J = 0 To 13
							ars!(N, I, J) = initA!(I, J)
						Next J

						ars!(N, I, 14) = 1
						ars!(N, I, 15) = statYear
						W0S!(N, I) = careerW0!(I)
						W1S!(N, I) = careerW1!(I)

					Next I

					For I = 0 To 61: AR$(I) = initA$(I): Next

					If finalUpdate% = 1 Then Call UpdateCareerLeaders(careerFile$)

					Open diskPaths$(1) + careerFile$ + ".CAR" For Output As #2

					Write #2, NB, B$

					For N = 1 To 15

						For I = 0 To 61

							Write #2, AR$(I)

							For J = 0 To 15: 
								Write #2, ars!(N, I, J)
							Next J

							Write #2, W0S!(N, I)
							Write #2, W1S!(N, I)

							Color 5
							Locate 21, 38
							TMR = TMR - 1: Print Using "####"; TMR

						Next I

					Next N

					Close #2

				End If 'Done checking for stat file

			End If 'Done checking for changes

		End If 'Done checking if team was chosen

	Loop Until UCase$(J$) = "N"

End Sub


'----------------------------------------
'    ViewPrintCareerRecords Subroutine
'----------------------------------------
'This routine loads in leader records from
'the .CLD file (which were compiled from  
'the career file). It then gives the user
'an option to View or Print results, AND
'formats the results for them as directed.
Sub ViewPrintCareerRecords ()

    Dim BL!(62), BLYR!(21)
    Dim BL$(0 To 20)

    recordFile$ = ChooseFile$ (diskPaths$(1) + "*.CLD", "CAREER RECORD", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Color 15, 0
        Cls

        Open diskPaths$(1) + recordFile$ + ".CLD" For Input As #1

        For I = 0 To 20
            Input #1, BLYR!(I), BL$(I), BL!(I)
        Next

        Close #1

        Do
            Color 15, 0
            Cls

            Do
                Locate 15, 10
                Color 12: Print "V";: Color 7: Print "iew or ";
                Color 12: Print "P";: Color 7: Print "rint"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V"

            Select Case UCase$(I$)

                Case "P":

                    Color 0, 15

                    LPrint "CATEGORY                     RECORD           PLAYER              YEAR"
                    LPrint
                    LPrint "FIELD GOALS MADE";: LPrint Tab(33); Using "####"; BL!(0);: LPrint Tab(47); BL$(0);: LPrint Tab(66); BLYR!(0)
                    LPrint "FIELD GOAL ATTEMPTS";: LPrint Tab(33); Using "####"; BL!(1);: LPrint Tab(47); BL$(1);: LPrint Tab(66); BLYR!(1)
                    LPrint "FIELD GOAL PCT.";: LPrint Tab(32); Using "#.###"; BL!(2);: LPrint Tab(47); BL$(2);: LPrint Tab(66); BLYR!(2)
                    LPrint "FREE THROWS MADE";: LPrint Tab(33); Using "####"; BL!(3);: LPrint Tab(47); BL$(3);: LPrint Tab(66); BLYR!(3)
                    LPrint "FREE THROW ATTEMPTS";: LPrint Tab(33); Using "####"; BL!(4);: LPrint Tab(47); BL$(4);: LPrint Tab(66); BLYR!(4)
                    LPrint "FREE THROW PCT.";: LPrint Tab(32); Using "#.###"; BL!(5);: LPrint Tab(47); BL$(5);: LPrint Tab(66); BLYR!(5)
                    LPrint "3 PT. FIELD GOALS MADE";: LPrint Tab(33); Using "####"; BL!(6);: LPrint Tab(47); BL$(6);: LPrint Tab(66); BLYR!(6)
                    LPrint "3 PT. FILED GOAL ATTEMPTS";: LPrint Tab(33); Using "####"; BL!(7);: LPrint Tab(47); BL$(7);: LPrint Tab(66); BLYR!(7)
                    LPrint "3 PT. FIELD GOAL PCT.";: LPrint Tab(32); Using "#.###"; BL!(8);: LPrint Tab(47); BL$(8);: LPrint Tab(66); BLYR!(8)
                    LPrint "POINTS";: LPrint Tab(32); Using "#####"; BL!(9);: LPrint Tab(47); BL$(9);: LPrint Tab(66); BLYR!(9)
                    LPrint "POINTS PER GAME";: LPrint Tab(32); Using "###.#"; BL!(10);: LPrint Tab(47); BL$(10);: LPrint Tab(66); BLYR!(10)
                    LPrint "OFFENSIVE REBOUNDS";: LPrint Tab(32); Using "#####"; BL!(11);: LPrint Tab(47); BL$(11);: LPrint Tab(66); BLYR!(11)
                    LPrint "DEFENSIVE REBOUNDS";: LPrint Tab(32); Using "#####"; BL!(12);: LPrint Tab(47); BL$(12);: LPrint Tab(66); BLYR!(12)
                    LPrint "TOTAL REBOUNDS";: LPrint Tab(32); Using "#####"; BL!(13);: LPrint Tab(47); BL$(13);: LPrint Tab(66); BLYR!(13)
                    LPrint "REBOUNDS PER GAME";: LPrint Tab(32); Using "###.#"; BL!(14);: LPrint Tab(47); BL$(14);: LPrint Tab(66); BLYR!(14)
                    LPrint "PERSONAL FOULS";: LPrint Tab(33); Using "####"; BL!(15);: LPrint Tab(47); BL$(15);: LPrint Tab(66); BLYR!(15)
                    LPrint "ASSISTS";: LPrint Tab(32); Using "#####"; BL!(16);: LPrint Tab(47); BL$(16);: LPrint Tab(66); BLYR!(16)
                    LPrint "SHOTS BLOCKED";: LPrint Tab(33); Using "####"; BL!(17);: LPrint Tab(47); BL$(17);: LPrint Tab(66); BLYR!(17)
                    LPrint "STEALS";: LPrint Tab(32); Using "#####"; BL!(18);: LPrint Tab(47); BL$(18);: LPrint Tab(66); BLYR!(18)
                    LPrint "TURNOVERS";: LPrint Tab(32); Using "#####"; BL!(19);: LPrint Tab(47); BL$(19);: LPrint Tab(66); BLYR!(19)
                    LPrint "HI-GAME (POINTS)";: LPrint Tab(33); Using "####"; BL!(20);: LPrint Tab(47); BL$(20);: LPrint Tab(66); BLYR!(20)

                    LPrint Chr$(12)

                Case "V":

                    Do
                        Cls
                        Color 15, 0

                        Color 12: Print "CATEGORY                     RECORD           PLAYER              YEAR"
                        Print

                        Color 3: Print "FIELD GOALS MADE";: Color 7: Print Tab(33); Using "####"; BL!(0);: Print Tab(47); BL$(0);: Print Tab(66); BLYR!(0)
                        Color 3: Print "FIELD GOAL ATTEMPTS";: Color 7: Print Tab(33); Using "####"; BL!(1);: Print Tab(47); BL$(1);: Print Tab(66); BLYR!(1)
                        Color 3: Print "FIELD GOAL PCT.";: Color 7: Print Tab(32); Using "#.###"; BL!(2);: Print Tab(47); BL$(2);: Print Tab(66); BLYR!(2)
                        Color 3: Print "FREE THROWS MADE";: Color 7: Print Tab(33); Using "####"; BL!(3);: Print Tab(47); BL$(3);: Print Tab(66); BLYR!(3)
                        Color 3: Print "FREE THROW ATTEMPTS";: Color 7: Print Tab(33); Using "####"; BL!(4);: Print Tab(47); BL$(4);: Print Tab(66); BLYR!(4)
                        Color 3: Print "FREE THROW PCT.";: Color 7: Print Tab(32); Using "#.###"; BL!(5);: Print Tab(47); BL$(5);: Print Tab(66); BLYR!(5)
                        Color 3: Print "3 PT. FIELD GOALS MADE";: Color 7: Print Tab(33); Using "####"; BL!(6);: Print Tab(47); BL$(6);: Print Tab(66); BLYR!(6)
                        Color 3: Print "3 PT. FILED GOAL ATTEMPTS";: Color 7: Print Tab(33); Using "####"; BL!(7);: Print Tab(47); BL$(7);: Print Tab(66); BLYR!(7)
                        Color 3: Print "3 PT. FIELD GOAL PCT.";: Color 7: Print Tab(32); Using "#.###"; BL!(8);: Print Tab(47); BL$(8);: Print Tab(66); BLYR!(8)
                        Color 3: Print "POINTS";: Color 7: Print Tab(32); Using "#####"; BL!(9);: Print Tab(47); BL$(9);: Print Tab(66); BLYR!(9)
                        Color 3: Print "POINTS PER GAME";: Color 7: Print Tab(32); Using "###.#"; BL!(10);: Print Tab(47); BL$(10);: Print Tab(66); BLYR!(10)
                        Color 3: Print "OFFENSIVE REBOUNDS";: Color 7: Print Tab(32); Using "#####"; BL!(11);: Print Tab(47); BL$(11);: Print Tab(66); BLYR!(11)
                        Color 3: Print "DEFENSIVE REBOUNDS";: Color 7: Print Tab(32); Using "#####"; BL!(12);: Print Tab(47); BL$(12);: Print Tab(66); BLYR!(12)
                        Color 3: Print "TOTAL REBOUNDS";: Color 7: Print Tab(32); Using "#####"; BL!(13);: Print Tab(47); BL$(13);: Print Tab(66); BLYR!(13)
                        Color 3: Print "REBOUNDS PER GAME";: Color 7: Print Tab(32); Using "###.#"; BL!(14);: Print Tab(47); BL$(14);: Print Tab(66); BLYR!(14)

                        Color 15
                        Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"
                        A$ = GetKeyPress$

                        Cls
                        Color 12: Print "CATEGORY                     RECORD           PLAYER              YEAR"
                        Print
                        Color 3: Print "PERSONAL FOULS";: Color 7: Print Tab(33); Using "####"; BL!(15);: Print Tab(47); BL$(15);: Print Tab(66); BLYR!(15)
                        Color 3: Print "ASSISTS";: Color 7: Print Tab(32); Using "#####"; BL!(16);: Print Tab(47); BL$(16);: Print Tab(66); BLYR!(16)
                        Color 3: Print "SHOTS BLOCKED";: Color 7: Print Tab(33); Using "####"; BL!(17);: Print Tab(47); BL$(17);: Print Tab(66); BLYR!(17)
                        Color 3: Print "STEALS";: Color 7: Print Tab(32); Using "#####"; BL!(18);: Print Tab(47); BL$(18);: Print Tab(66); BLYR!(18)
                        Color 3: Print "TURNOVERS";: Color 7: Print Tab(32); Using "#####"; BL!(19);: Print Tab(47); BL$(19);: Print Tab(66); BLYR!(19)
                        Color 3: Print "HI-GAME (POINTS)";: Color 7: Print Tab(33); Using "####"; BL!(20);: Print Tab(47); BL$(20);: Print Tab(66); BLYR!(20)

                        Color 15

                        Do
                            Locate 22: Print "VIEW AGAIN? (Y/N)"
                            A$ = GetKeyPress$
                        Loop Until UCase$(A$) = "N" Or UCase$(A$) = "Y"

                    Loop Until UCase$(A$) = "N"

            End Select

        Loop Until UCase$(I$) = "V" Or UCase$(I$) = "P" Or I$ = Chr$(27)

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'       MergeCareerFiles Subroutine
'----------------------------------------
'This routine assists thet user with the
'addition of career stats from any extra 
'season years that have been compiled.
Sub MergeCareerFiles ()

    _Title "Basketball: The Pro Game - Update Career Files"

    Do
        Color 14, 0
        Cls

        Locate 5, 27
        Print "- - - C A U T I O N - - -"

        Color 15
        Locate 8, 1
        
        Print "THIS OPTION SHOULD ONLY BE USED WHEN ADDING YEARS TO AN EXISTING STAT"
        Print "FILE. IF YOU ARE CREATING A NEW CAREER FILE, USE THE CREATE OPTION."
        Print
        Print "IF YOU ARE ADDING AN ADDITIONAL CAREER YEAR, MAKE SURE THAT ANY UNUSED PLAYER"
        Print "SLOTS (XXX, END, ETC.) ARE AT THE END OF THE SEASON STAT LIST BEFORE YOU MERGE."
        Print
        Print "ALSO MAKE SURE THAT THE PLAYERS NAMES IN THE FILE YOU ARE ADDING, MATCH"
        Print "THE EXACT SPELLING OF THE NAME OF THE SAME PLAYER IN THE CAREER FILE FOR THE"
        Print "PLAYERS THAT HAD PLAYED IN PREVIOUS YEARS.  EXAMPLE - IF BILLY WILLIAMS IS"
        Print "LISTED AS 'WILLIAMS' IN THE CAREER FILE AND THE SEASON STATS HAD HIM AS"
        Print "'B.WILLIAMS', HE SHOULD BE CHANGED TO 'WILLIAMS' IN THE SEASON FILE BEFORE"
        Print "MERGING, OR YOU COULD CHANGE HIM TO 'B.WILLIAMS' IN THE CAREER FILE"
        Print
        Print

        Color 12
        Locate , 25
        Print "DO YOU WISH TO CONTINUE? (Y/N)"

        I$ = GetKeyPress$
    
    Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y" Or I$ = Chr$(27)

    If UCase$(I$) = "Y" Then

        careerFile$ = ChooseFile$ (diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

        If ExitCode% <> 27 And ExitCode% <> 0 Then

            Call ChooseTeam(YN$, NTMS, teamNbr, team$)
            statFile$ = nameStatFile$(team$)

            If Not _FileExists(diskPaths$(1) + statFile$) Then 

                MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
                BoxWidth = 24
                BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                Call CSMsgBox(MsgText$, BoxWidth, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)

            Else

                Call MergeAddYear(careerFile$, statFile$)

            End If

        End If 'Done checking for ESC

    End If 'Done confirming user choice

End Sub

'----------------------------------------
'         MergeAddYear Subroutine
'----------------------------------------
'This routine actually prompts the user
'for parameters for the team / year to
'be updated. It then loads the existing 
'career stats, adds the team's stats to 
'them, then overwrites the career file 
'with the new values. 
Sub MergeAddYear (selectedFile$, statFile$)

    Dim initA$(62)

    Dim mergeA!(15, 62)
    Dim careerW0!(62), careerW1!(62)

    Shared AR$()

    Shared ars!(), compareZ!(), compareZ1!()
    Shared W0S!(), W1S!()

    'needs to be a constant???
    TMR1% = 1875

    Color 15, 0
    Cls

    Do
        Locate 8, 25: Input "ENTER YEAR TO ADD (EX. - 1961)"; statYear
        Locate 9, 25: Print "ANY CHANGE? (Y/N)"
        J$ = GetKeyPress$
    Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

    If UCase$(J$) = "N" Then

        Do
            Locate , 5
            Print "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"
            K$ = GetKeyPress$
            finalUpdate$ = UCase$(K$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N" Or K$ = Chr$(27)

        If finalUpdate$ = "Y" Then finalupdate% = 1 Else finalupdate% = 0

        Locate 20, 5
        Print "LOADING CAREER STATS AND LEADER FILE . . . . PLEASE WAIT"

        Open diskPaths$(1) + selectedFile$ + ".CAR" For Input As #2

        Input #2, NB, B$

        For N = 1 To 15

            For I = 0 To 61

                Input #2, AR$(I)

                For J = 0 To 15: 
                    Input #2, ars!(N, I, J)
                Next J

                Input #2, W0S!(N, I)
                Input #2, W1S!(N, I)

                Color 5
                Locate 21, 38
                TMR1% = TMR1% - 1
                Print Using "####"; TMR1%

            Next I

        Next N

        Close #2

        Locate 20, 5
        Print "LOADING STATS FILE . . . . PLEASE WAIT"

        Open diskPaths$(1) + statFile$ For Input As #1

        Input #1, B$

        For I = 0 To 14

            Input #1, initA$(I)

            For J = 0 To 13: 
                Input #1, mergeA!(I, J)
            Next
            
            Input #1, careerW0!(I)
            Input #1, careerW1!(I)

            Color 5
            Locate 21, 38
            TMR1% = TMR1% - 1
            Print Using "####"; TMR1%

        Next

        For L = 0 To 15: Input #1, compareZ!(L): Next

        For L = 0 To 15: Input #1, compareZ1!(L): Next

        Close #1

        For I = 0 To 14
            RR = 0
            CHK = 0

            For J = 0 To 61

                If AR$(J) = initA$(I) Then

                    For W = 1 To 15
                        If ars!(W, J, 15) = statYear Then
                            1412 CHK = CHK + 1
                        End If
                    Next W

                    If CHK <= 0 Then
                        N = ars!(1, J, 14) + ars!(2, J, 14) + ars!(3, J, 14) + ars!(4, J, 14)
                        N = N + ars!(5, J, 14) + ars!(6, J, 14) + ars!(7, J, 14) + ars!(8, J, 14)
                        N = N + ars!(9, J, 14) + ars!(10, J, 14) + ars!(11, J, 14)
                        N = N + ars!(12, J, 14) + ars!(13, J, 14) + ars!(14, J, 14) + ars!(15, J, 14) + 1
                    Else
                        N = ars!(1, J, 14) + ars!(2, J, 14) + ars!(3, J, 14) + ars!(4, J, 14)
                        N = N + ars!(5, J, 14) + ars!(6, J, 14) + ars!(7, J, 14) + ars!(8, J, 14)
                        N = N + ars!(9, J, 14) + ars!(10, J, 14) + ars!(11, J, 14)
                        N = N + ars!(12, J, 14) + ars!(13, J, 14) + ars!(14, J, 14) + ars!(15, J, 14)
                    End If

                    For K = 0 To 13
                        ars!(N, J, K) = mergeA!(I, K)
                    Next K

                    ars!(N, J, 14) = 1
                    ars!(N, J, 15) = statYear
                    W0S!(N, J) = careerW0!(J)
                    W1S!(N, J) = careerW1!(J)

                Else
                    RR = RR + 1
                    If RR = 62 Then

                        If initA$(I) <> "END" And initA$(I) <> "XXX" And initA$(I) <> "" Then

                            NB = NB + 1
                            AR$(NB) = initA$(I)
                            N = 1

                            For K = 0 To 13: ars!(N, NB, K) = mergeA!(I, K): Next K

                            ars!(N, NB, 14) = 1
                            ars!(N, NB, 15) = statYear
                            W0S!(N, NB) = careerW0!(NB)
                            W1S!(N, NB) = careerW1!(NB)
                        End If

                    End If

                End If

            Next J

        Next I

        If finalUpdate% = 1 Then Call UpdateCareerLeaders(selectedFile$)

        Locate 20, 5
        Print "MERGING CAREER STATS . . . . PLEASE WAIT"

        Open diskPaths$(1) + selectedFile$ + ".CAR" For Output As #2

        Write #2, NB, B$

        For N = 1 To 15

            For I = 0 To 61: 

                Write #2, AR$(I)

                For J = 0 To 15: 
                    Write #2, ars!(N, I, J)
                Next J
                
                Write #2, W0S!(N, I)
                Write #2, W1S!(N, I)
                
                Color 5
                Locate 21, 38
                TMR1% = TMR1% - 1
                Print Using "####"; TMR1%

            Next I

        Next N

        Close #2

    End If 'Done checking user confirmation

End Sub


'----------------------------------------
'     UpdateCareerLeaders Subroutine
'----------------------------------------
'This routine reads in any existing career 
'leader files (.CLD), adds the new stats,
'and then saves the changes back to the
'career leader file (.CLD)
Sub UpdateCareerLeaders (careerFile$)

    Dim BL!(62), BLYR!(21)
    Dim initA$(62)
    Dim BL$(0 To 20)

    Shared AR$()
    Shared ars!(), W0S!(), W1S!()

    If _FileExists(diskPaths$(1) + careerFile$ + ".CLD") Then

        Open diskPaths$(1) + careerFile$ + ".CLD" For Input As #1

        For I = 0 To 20
            Input #1, BLYR!(I), BL$(I), BL!(I)
        Next

        Close #1

    End If

    For I = 0 To 61

        If AR$(I) <> "" And AR$(I) <> "XXX" And AR$(I) <> "END" Then

            For J = 1 To 15

                If ars!(J, I, 15) <> 0 Then

                    If ars!(J, I, 1) > BL!(0) Then
                        BL!(0) = ars!(J, I, 1)
                        BL$(0) = AR$(I)
                        BLYR!(0) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 2) > BL!(1) Then
                        BL!(1) = ars!(J, I, 2)
                        BL$(1) = AR$(I)
                        BLYR!(1) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 1) <> 0 And ars!(J, I, 1) / ars!(J, I, 0) >= 3.65 Then
                        If ars!(J, I, 2) / ars!(J, I, 1) > BL!(2) Then
                            BL!(2) = ars!(J, I, 2) / ars!(J, I, 1)
                            BL$(2) = AR$(I)
                            BLYR!(2) = ars!(J, I, 15)
                        End If
                    End If

                    If ars!(J, I, 4) > BL!(3) Then
                        BL!(3) = ars!(J, I, 4)
                        BL$(3) = AR$(I)
                        BLYR!(3) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 3) > BL!(4) Then
                        BL!(4) = ars!(J, I, 3)
                        BL$(4) = AR$(I)
                        BLYR!(4) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 3) <> 0 And ars!(J, I, 3) / ars!(J, I, 0) >= 1.5 Then
                        If ars!(J, I, 4) / ars!(J, I, 3) > BL!(5) Then
                            BL!(5) = ars!(J, I, 4) / ars!(J, I, 3)
                            BL$(5) = AR$(I)
                            BLYR!(5) = ars!(J, I, 15)
                        End If
                    End If

                    If W0S!(J, I) > BL!(6) Then
                        BL!(6) = W0S!(J, I)
                        BL$(6) = AR$(I)
                        BLYR!(6) = ars!(J, I, 15)
                    End If

                    If W1S!(J, I) > BL!(7) Then
                        BL!(7) = W1S!(J, I)
                        BL$(7) = AR$(I)
                        BLYR!(7) = ars!(J, I, 15)
                    End If

                    If W1S!(J, I) <> 0 And W1S!(J, I) / ars!(J, I, 0) >= 0.3 Then
                        If W0S!(J, I) / W1S!(J, I) > BL!(8) Then
                            BL!(8) = W0S!(J, I) / W1S!(J, I)
                            BL$(8) = AR$(I)
                            BLYR!(8) = ars!(J, I, 15)
                        End If
                    End If

                    If ars!(J, I, 2) * 2 + ars!(J, I, 4) + W0S!(J, I) > BL!(9) Then
                        BL!(9) = ars!(J, I, 2) * 2 + ars!(J, I, 4) + W0S!(J, I)
                        BL$(9) = AR$(I)
                        BLYR!(9) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 0) <> 0 Then
                        If (ars!(J, I, 2) * 2 + ars!(J, I, 4) + W0S!(J, I)) / ars!(J, I, 0) > BL!(10) Then
                            BL!(10) = (ars!(J, I, 2) * 2 + ars!(J, I, 4) + W0S!(J, I)) / ars!(J, I, 0)
                            BL$(10) = AR$(I)
                            BLYR!(10) = ars!(J, I, 15)
                        End If
                    End If

                    If ars!(J, I, 12) > BL!(11) Then
                        BL!(11) = ars!(J, I, 12)
                        BL$(11) = AR$(I)
                        BLYR!(11) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 5) - ars!(J, I, 12) > BL!(12) Then
                        BL!(12) = ars!(J, I, 5) - ars!(J, I, 12)
                        BL$(12) = AR$(I)
                        BLYR!(12) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 5) > BL!(13) Then
                        BL!(13) = ars!(J, I, 5)
                        BL$(13) = AR$(I)
                        BLYR!(13) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 0) <> 0 Then
                        If ars!(J, I, 5) / ars!(J, I, 0) > BL!(14) Then
                            BL!(14) = ars!(J, I, 5) / ars!(J, I, 0)
                            BL$(14) = AR$(I)
                            BLYR!(14) = ars!(J, I, 15)
                        End If
                    End If

                    If ars!(J, I, 6) > BL!(15) Then
                        BL!(15) = ars!(J, I, 6)
                        BL$(15) = AR$(I)
                        BLYR!(15) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 8) > BL!(16) Then
                        BL!(16) = ars!(J, I, 8)
                        BL$(16) = AR$(I)
                        BLYR!(16) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 9) > BL!(17) Then
                        BL!(17) = ars!(J, I, 9)
                        BL$(17) = AR$(I)
                        BLYR!(17) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 7) > BL!(18) Then
                        BL!(18) = ars!(J, I, 7)
                        BL$(18) = AR$(I)
                        BLYR!(18) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 11) > BL!(19) Then
                        BL!(19) = ars!(J, I, 11)
                        BL$(19) = AR$(I)
                        BLYR!(19) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 13) > BL!(20) Then
                        BL!(20) = ars!(J, I, 13)
                        BL$(20) = AR$(I)
                        BLYR!(20) = ars!(J, I, 15)
                    End If

                End If

            Next J

        End If

    Next I

    Open diskPaths$(1) + careerFile$ + ".CLD" For Output As #1

    For I = 0 To 20
        Write #1, BLYR!(I), BL$(I), BL!(I)
    Next

    Close #1

End Sub
