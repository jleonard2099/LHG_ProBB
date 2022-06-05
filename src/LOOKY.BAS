Sub LOOKY ()
	
	Dim teamRatings%(0 To 100)
 
	Shared categories$(), lookyA$()
	
    DefInt A-Z

    _Title "Basketball: The Pro Game - View Stats"

    Color 15, 0
    Cls

    Call TITLE_HEADER


	Do
		'Pick a team to view stats for
		3056 Color 15, 0
		Cls
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

		'Check first character to see if ESC/ENTER
		3057 I$ = GetKeyPress$

		'ESC = return to menu
		If Not( I$ = Chr$(27)) Then 

			'ENTER = perform selection on file with NO extension
			If I$ = Chr$(13) Then 
				Call TeamSelection_LOOKY(YN$, NTMS, teamRatings%(), BO%)
				If BO% = 1 Then BO% = 0: GoTo 3056
			End If

			'At this point we assume they're typing more than one 1 char
			Locate 7, 27: Print I$
			Locate 7, 28: Line Input NY$
			NY$ = I$ + NY$
			Color 15, 0

			If NY$ = "" Then Call TeamSelection_LOOKY(YN$, NTMS, teamRatings%(), BO%): If BO% = 1 Then BO% = 0: GoTo 3056

			YN$ = NY$
			Erase teamNames$

			1100 NTMS = 0
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
				If CK% <> 999 Then 1103
				NTMS = NTMS + 1
				teamNames$(NTMS) = Q$(0): teamIndex%(NTMS) = I1

			1103 Next
			Close 1

			'This is where most calls to the function will happen
			Call TeamSelection_LOOKY(YN$, NTMS, teamRatings%(), BO%)

			3060 If BO% = 1 Then 
				BO% = 0
				GoTo 3056
			Else
				A$ = Q$(0)
				9005 If Right$(A$, 1) = " " Then A$ = Left$(A$, Len(A$) - 1): GoTo 9005
				B$ = A$

				3058 Locate 2, 33: Color teamRatings%(33), teamRatings%(34): Print A$
				Color 15, 4
				Locate , 33: Print "ANY CHANGE (YN)";
				I$ = GetKeyPress$
				If UCase$(I$) = "Y" Then 3056

				Color 15, 0
				'Look for special char's in team name
				6585 For XX = 4 To 11: If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
				Next XX

				For XX = 12 To 14: If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
				Next XX

				QQ = 8

				For XX = 11 To 4 Step -1
					If Not (Mid$(B$, XX, 1) = "_") Then
						Exit For
					End If
				6615 Next XX

				'Determine stats file name
				Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)

				' 90 If UCase$(Z$) = "END" Then Run "HELLO.EXE 4"

				Call ReadStats (Z$, teamRatings%())
				
			End If

		End If
		
	Loop Until I$ = Chr$(27)
	
End Sub





'------------------------------
Sub ReadStats (Z$, teamRatings%())

    Dim O%(0 To 100), P%(0 To 100)
    Dim H$(0 To 100), O$(0 To 100)
    Dim Z(0 To 15), Z1(0 To 15)

	Shared categories$(), lookyA$()
	Shared values(), W0(), W1()

    ' *** READ IN TEAM STATS ***
    '-- update to be IF (FileExists) Do Stuff ELSE Print Error and Call (at end of SUB)
    If Not _FileExists(D$(1) + Z$) Then
        Cls
        Print "ERROR::: UNABLE TO LOCATE COMPILED STATS FILE"
        Exit Sub
    End If

	CP% = 1
    PX = 0
    P%(0) = 0
    P%(1) = 0
    L% = 0
    W% = 0
    X% = 0
    HW% = 0
    VW% = 0
    NW% = 0
    HL% = 0
    VL% = 0
    NL% = 0

    Open D$(1) + Z$ + "S" For Input As #2
    Input #2, totalGames%
    For I = 1 To 100: Input #2, O%(I): Next I
    For I = 1 To 100: Input #2, O$(I): Next I
    For I = 1 To 100: Input #2, teamRatings%(I): Next I
    For I = 1 To 100: Input #2, H$(I): Next I
    Close #2

    For I = 1 To totalGames%
        If O%(I) > teamRatings%(I) Then W% = W% + 1
        If O%(I) > teamRatings%(I) And Left$(H$(I), 1) = "H" Then HW% = HW% + 1
        If O%(I) > teamRatings%(I) And Left$(H$(I), 1) = "V" Then VW% = VW% + 1
        If O%(I) > teamRatings%(I) And Left$(H$(I), 1) = "N" Then NW% = NW% + 1
        If O%(I) < teamRatings%(I) Then L% = L% + 1
        If O%(I) < teamRatings%(I) And Left$(H$(I), 1) = "H" Then HL% = HL% + 1
        If O%(I) < teamRatings%(I) And Left$(H$(I), 1) = "V" Then VL% = VL% + 1
        If O%(I) < teamRatings%(I) And Left$(H$(I), 1) = "N" Then NL% = NL% + 1
        If O%(I) = teamRatings%(I) Then X% = X% + 1
        P%(0) = P%(0) + O%(I): P%(1) = P%(1) + teamRatings%(I)
    Next

    Open D$(1) + Z$ For Input As #1
    Input #1, B$
    For I = 0 To 14
        Input #1, lookyA$(I)
        For J = 0 To 13
            Input #1, values(I, J)
        Next
        Input #1, W0(I)
        Input #1, W1(I)
    Next

    For K = 0 To 15: Input #1, Z(K): Next
    For K = 0 To 15: Input #1, Z1(K): Next
    Close #1

    For I = 0 To 14
        If values(I, 0) <> 0 Then values(I, 14) = values(I, 10) / values(I, 0): values(I, 20) = (values(I, 2) * 2 + values(I, 4) + W0(I)) / values(I, 0): values(I, 21) = values(I, 5) / values(I, 0)
        values(I, 22) = values(I, 2) * 2 + values(I, 4) + W0(I)
        values(I, 23) = values(I, 5) - values(I, 12)
        If values(I, 1) <> 0 Then values(I, 15) = values(I, 2) / values(I, 1)
        values(I, 18) = W0(I): values(I, 19) = W1(I)
        If values(I, 19) <> 0 Then values(I, 16) = values(I, 18) / values(I, 19)
        If values(I, 3) <> 0 Then values(I, 17) = values(I, 4) / values(I, 3)
    Next




	'-----
    'Print stats to screen
    815 Call TITLE_HEADER

    Color , 1
    Locate 1, 1: Print Tab(80);: Locate , 31: Color 14
    Ycoord = Len(B$): Ycoord = Int((80 - Ycoord) / 2): Locate , Ycoord: Print B$
    Locate 25, 1: Print Tab(80);: Locate , 1
    Locate 2, 1
    Color , 0

    For I = 1 To totalGames%
        Color 7
        If I / 2 = Int(I / 2) Then Color 15
        If I > 54 Then 860
        If I > 36 Then 827
        If I > 18 Then 826
        Print Using "###"; O%(I);
        Print Tab(5); Left$(O$(I), 14);
        Print Tab(20); Using "###"; teamRatings%(I);
        Print Tab(24); H$(I)
        GoTo 830

        826 Locate I - 17, 27: Print Using "###"; O%(I);
        Print Tab(31); Left$(O$(I), 14);
        Print Tab(46); Using "###"; teamRatings%(I);
        Print Tab(50); H$(I)
        GoTo 830

        827 Locate I - 35, 53: Print Using "###"; O%(I);
        Print Tab(57); Left$(O$(I), 14);
        Print Tab(72); Using "###"; teamRatings%(I);
        Print Tab(76); H$(I)

    830 Next I
    Print

    860 Locate 21, 27
    Color 14: Print "OVERALL RECORD ";: Color 15: Print "("; W%; "-"; L%; ")"
    Locate , 27: Color 14: Print "HOME RECORD    ";: Color 15: Print "("; HW%; "-"; HL%; ")"
    Locate , 27: Color 14: Print "ROAD RECORD    ";: Color 15: Print "("; VW%; "-"; VL%; ")"
    Locate , 27: Color 14: Print "NEUTRAL RECORD ";: Color 15: Print "("; NW%; "-"; NL%; ")";
    Color 3
    Locate 25, 1
    Color 15, 1: Print "INDIVIDUAL STATS ";
    Color 14: Print "[Pg DN]  ";
    Color 15: Print "SELECT ANOTHER TEAM ";
    Color 14: Print "[ESC]  ";
    Color 15: Print "PRINT STATS ";
    Color 14: Print "[P]";
    Locate 3, 1

    870 I$ = GetKeyPress$
    'Pg Down
    If I$ = Chr$(0) + "Q" Then 
		Color , 0
		Cls
		GoTo 915
	End If

	'Print
    If UCase$(I$) = "P" Then 
		Call PrintStatsSchedule(totalGames%, teamRatings%(), O%(), O$(), H$())
		Call PrintStatsSummary(totalGames%, Z(), Z1())
		GoTo 870
	End If

	'Delete
    If UCase$(I$) = "D" Then 
		Kill D$(1) + Z$
		Kill D$(1) + Z$ + "S"
		GoTo 870
	End If

	'Esc
    If I$ = Chr$(27) Then Exit Sub

    GoTo 870





	'-----
    915 Color , 1: Locate 1, 1: Print Tab(80);: Locate , 31: Color 14
    Ycoord = Len(B$): Ycoord = Int((80 - Ycoord) / 2): Locate , Ycoord: Print B$
    Locate 25, 1: Print Tab(80);: Locate , 1
    Locate 2, 1
    Color , 0
    For I = 1 To totalGames%
        Color 7
        If I / 2 = Int(I / 2) Then Color 15
        If I < 55 Then 930
        If I > 72 Then 926
        Print Using "###"; O%(I);
        Print Tab(5); Left$(O$(I), 14);
        Print Tab(20); Using "###"; teamRatings%(I);
        Print Tab(24); H$(I)
        GoTo 930

        926 Locate I - 71, 27: Print Using "###"; O%(I);
        Print Tab(31); Left$(O$(I), 14);
        Print Tab(46); Using "###"; teamRatings%(I);
        Print Tab(50); H$(I)
        GoTo 930

    930 Next I

    Print
    Locate 21, 27
    Color 14: Print "OVERALL RECORD ";: Color 15: Print "("; W%; "-"; L%; ")"
    Locate , 27: Color 14: Print "HOME RECORD    ";: Color 15: Print "("; HW%; "-"; HL%; ")"
    Locate , 27: Color 14: Print "ROAD RECORD    ";: Color 15: Print "("; VW%; "-"; VL%; ")"
    Locate , 27: Color 14: Print "NEUTRAL RECORD ";: Color 15: Print "("; NW%; "-"; NL%; ")";
    Color 3
    Locate 25, 1
    Color 15, 1: Print "INDIVIDUAL STATS ";
    Color 14: Print "[Pg DN]  ";
    Color 15: Print "SELECT ANOTHER TEAM ";
    Color 14: Print "[ESC]  ";
    Color 15: Print "PRINT STATS ";
    Color 14: Print "[P]";
    Locate 3, 1
    
	970 I$ = GetKeyPress$

    'Pg Down
    If I$ = Chr$(0) + "Q" Then 
		Color , 0
		Cls
		GoTo 142
	End If

    'Pg Up
    If I$ = Chr$(0) + "I" Then 
		Color , 0
		Cls
		GoTo 815
	End If
	
	'Print
    If UCase$(I$) = "P" Then
		Call PrintStatsSchedule(totalGames%, teamRatings%(), O%(), O$(), H$())
		Call PrintStatsSummary(totalGames%, Z(), Z1())
		GoTo 970
	End If

	'Delete
    If UCase$(I$) = "D" Then 
		Kill D$(1) + Z$
		Kill D$(1) + Z$ + "S"
		GoTo 970
	End If

	'Esc
    If I$ = Chr$(27) Then Exit Sub
    GoTo 970




 
	'-----
    ' *** DISPLAYING STATS ***
    142 Call TITLE_HEADER
    Color , 1
    Locate 24, 1: Print Tab(80);: Locate , 1
    Color 15, 1: Print "SCHEDULE ";
    Color 14: Print "[Pg UP]  ";
    Color 15: Print "MORE STATS ";
    Color 14: Print "[Pg DN]  ";
    Color 15: Print "NEW TEAM ";
    Color 14: Print "[ESC]  ";
    Color 15: Print "SORT ";
    Color 14: Print "<-,-> and [ENTER] ";
    Locate 25, 1
    Color 15: Print "PRINT ";: Color 14: Print "[P]  ";
    Color 15: Print "DELETE STATS ";: Color 14: Print "[D]                    ";
    Color 12, 0: Locate 2, 1: Print "SORT BY: "; categories$(CP%); Tab(40);
    Locate 3, 1: Color 2, 0
    Color 14: Print B$
    Print "NAME"; Tab(15);
    Print " G  MIN AVG   FGM  FGA  PCT  3FGM 3FGA  PCT   FTM  FTA  PCT"
    For I = 0 To 14
        If lookyA$(I) = "XXX" Then 165
        Color 7
        If I / 2 = Int(I / 2) Then Color 15
        Print Left$(lookyA$(I), 13); Tab(15);

        'Games
        Print Using "## "; values(I, 0);

        'Min, Avg
        If values(I, 10) > 0 Then Locate , 18: Print Using "####"; Int((values(I, 10) + 30) / 60);: Locate , 24: Print Using "##"; Int(((values(I, 10) / 60) / values(I, 0)) + .5);

        'FGM, FGA, FG PCT
        Locate , 28: Print Using "#### ####"; values(I, 2); values(I, 1);: Locate , 38
        If values(I, 1) = 0 Then Print ".---";: GoTo 160
        If values(I, 2) = values(I, 1) Then Print "1.00";: GoTo 160
        If values(I, 2) = 0 Then Print ".000";: GoTo 160
        fgPCT! = values(I, 2) / values(I, 1)
        Print Using ".###"; fgPCT!;

        '3FGM, 3FGA, 3FG PCT
        160 Locate , 44: Print Using "#### ####"; W0(I); W1(I);: Locate , 54
        If W1(I) = 0 Then Print ".---";: GoTo 161
        If W1(I) = W0(I) Then Print "1.00";: GoTo 161
        If W0(I) = 0 Then Print ".000";: GoTo 161
        threeFGPCT! = W0(I) / W1(I)
        Print Using ".###"; threeFGPCT!;

        'FTM, FTA, FT PCT
        161 Locate , 60: Print Using "#### ####"; values(I, 4); values(I, 3);: Locate , 70
        If values(I, 3) = 0 Then Print ".---": GoTo 165
        If values(I, 3) = values(I, 4) Then Print "1.00": GoTo 165
        If values(I, 4) = 0 Then Print ".000": GoTo 165
        ftPCT! = values(I, 4) / values(I, 3)
        Print Using ".###"; ftPCT!

    165 Next

    Color 12: Print: Print "TOTALS";: Locate , 15
    'Games
    Print Using "##"; totalGames%;

    'FGM / FGA
    Locate , 28: Print Using "#### ####"; Z(2); Z(1);
    If Z(1) <> 0 Then fgPCT! = Z(2) / Z(1): Locate , 38: Print Using ".###"; fgPCT!;

    '3FGM / 3FGA
    Locate , 44: Print Using "#### ####"; Z(13); Z(14);
    If Z(14) <> 0 Then threeFGPCT! = Z(13) / Z(14): Locate , 54: Print Using ".###"; threeFGPCT!;

    'FTM / FTA
    Locate , 60: Print Using "#### ####"; Z(4); Z(3);
    If Z(3) <> 0 Then ftPCT! = Z(4) / Z(3): Locate , 70: Print Using ".###"; ftPCT!;

    'Games
    Color 12: Locate , 15: Print Using "##"; totalGames%;

    'FGM / FGA
    Locate , 28: Print Using "#### ####"; Z1(2); Z1(1);
    If Z1(1) <> 0 Then fgPCT! = Z1(2) / Z1(1): Locate , 38: Print Using ".###"; fgPCT!;

    '3FGM / 3FGA
    Locate , 44: Print Using "#### ####"; Z1(13); Z1(14);
    If Z1(14) <> 0 Then threeFGPCT! = Z1(13) / Z1(14): Locate , 54: Print Using ".###"; threeFGPCT!;

    'FTM / FTA
    Locate , 60: Print Using "#### ####"; Z1(4); Z1(3);
    If Z1(3) <> 0 Then ftPCT! = Z1(4) / Z1(3): Locate , 70: Print Using ".###"; ftPCT!



    230 I$ = GetKeyPress$

    'Pg Down
    If I$ = Chr$(0) + "Q" Then 
		CP% = 14
		GoTo 235
	End If
	
    'Pg Up
    If I$ = Chr$(0) + "I" Then
		Cls
		GoTo 915
	End If

    'Right Arrow
    If I$ = Chr$(0) + "M" Then
        650 CP% = CP% + 1: If CP% > 12 Then CP% = 1
        Locate 2, 1: Print "SORT BY: "; categories$(CP%); Tab(40);
        GoTo 230
    End If

    'Left Arrow
    If I$ = Chr$(0) + "K" Then
        660 CP% = CP% - 1: If CP% < 1 Then CP% = 12
        Locate 2, 1: Print "SORT BY: "; categories$(CP%); Tab(40);
        GoTo 230
    End If

	'Delete
    If UCase$(I$) = "D" Then 
		Kill D$(1) + Z$
		Kill D$(1) + Z$ + "S"
		GoTo 230
	End If

	'Print
    If UCase$(I$) = "P" Then 	
		Call PrintStatsSchedule(totalGames%, teamRatings%(), O%(), O$(), H$())
		Call PrintStatsSummary(totalGames%, Z(), Z1())
		GoTo 230
	End If	

	'Enter
    If I$ = Chr$(13) Then 
		Call SortStats(CP%)
		GoTo 142
	End If

    If I$ = Chr$(27) Then Exit Sub
    GoTo 230





	'-----
    235 Cls
    Call TITLE_HEADER
    Color , 1
    Locate 24, 1: Print Tab(80);: Locate , 1
    Locate 24, 1: Color 14, 1
    Color 15: Print "MORE STATS ";
    Color 14: Print "[Pg UP]  ";
    Color 15: Print "SELECT NEW TEAM ";
    Color 14: Print "[ESC]  ";
    Color 15: Print "SORT ";
    Color 14: Print "<-,-> and [ENTER]  ";
    Color 15: Print "PRINT ";
    Color 14: Print "[P]";
    Locate 25, 1
    Color 15: Print "DELETE STATS ";: Color 14: Print "[D]                     ";
    Color 12, 0: Locate 2, 1: Print "SORT BY: "; categories$(CP%); Tab(40);

    'Player Stats
    Locate 3, 1: Color 2, 0
    Color 14: Print B$;: Locate , 15
    Print "               OFF  DEF  TOT": Print "NAME"; Tab(15);
    Print " PTS   AVG HI  REB  REB  REB   AVG   PF  AST  BLK  STL   TO"
    For I = 0 To 14
        If lookyA$(I) = "XXX" Then 266
        Color 7
        If I / 2 = Int(I / 2) Then Color 15

        'Name
        Print Left$(lookyA$(I), 13);: Locate , 15
        'Points
        Print Using "####"; values(I, 2) * 2 + values(I, 4) + W0(I);
        'Average
        If values(I, 0) > 0 Then Locate , 21: Print Using "##.#"; (values(I, 2) * 2 + values(I, 4) + W0(I)) / values(I, 0);
        'Highest Points, Rebounds
        Locate , 26: Print Using "## #### #### ####"; values(I, 13); values(I, 12); values(I, 5) - values(I, 12); values(I, 5);
        'Average
        If values(I, 0) > 0 Then Locate , 45: Print Using "##.#"; values(I, 5) / values(I, 0);
        'PF AST   BLK  STL  TO
        Locate , 51: Print Using "### ####  ###  ###  ###"; values(I, 6); values(I, 8); values(I, 9); values(I, 7); values(I, 11)

    266 Next I

    Print "TEAM";: Locate , 39: Print Using "####"; Z(15)
    Color 12: Print "TOTALS";
    'Points
    Locate , 14: Print Using "##### ###.#"; Z(10); Z(10) / totalGames%;
    'Rebounds
    Locate , 29: Print Using "#### #### ####  ##.#"; Z(12); Z(5) - Z(12) - Z(15); Z(5); Z(5) / totalGames%;
    'PF  AST  BLK  STL  TO
    Print Using " #### #### #### #### ####"; Z(6); Z(8); Z(9); Z(7); Z(11)
    'Points
    Locate , 14: Print Using "##### ###.#"; Z1(10); Z1(10) / totalGames%;
    'Rebounds
    Locate , 29: Print Using "#### #### ####  ##.#"; Z1(12); Z1(5) - Z1(12) - Z1(15); Z1(5); Z1(5) / totalGames%;
    'PF  AST  BLK  STL  TO
    Print Using " #### #### #### #### ####"; Z1(6); Z1(8); Z1(9); Z1(7); Z1(11): Color 3


	Do
		370 I$ = GetKeyPress$
		
		'Pg Up
		If I$ = Chr$(0) + "I" Then 
			Cls
			CP% = 1
			GoTo 142
		End If
		
		'Right Arrow
		If I$ = Chr$(0) + "M" Then
			655 CP% = CP% + 1: If CP% > 25 Then CP% = 14
			Locate 2, 1: Print "SORT BY: "; categories$(CP%); Tab(40);
			GoTo 370
		End If

		'Left Arrow
		If I$ = Chr$(0) + "K" Then
			665 CP% = CP% - 1: If CP% < 14 Then CP% = 25
			Locate 2, 1: Print "SORT BY: "; categories$(CP%); Tab(40);
			GoTo 370
		End If

		If I$ = Chr$(13) Then 
			Call SortStats(CP%)
			GoTo 235
		End If

		If UCase$(I$) = "P" Then 
			Call PrintStatsSchedule(totalGames%, teamRatings%(), O%(), O$(), H$())
			Call PrintStatsSummary(totalGames%, Z(), Z1())
			GoTo 370
		End If

		If UCase$(I$) = "D" Then
			Kill D$(1) + Z$
			Kill D$(1) + Z$ + "S"
			GoTo 370
		End If

	Loop Until I$ = Chr$(27)

Exit Sub


    '--- I don't think we ever get here
'    Cls
'    Call TITLE_HEADER
'    Print "RESET STATS TO 0 ? (YN)";:
'    455 I$ = GetKeyPress$
'    If UCase$(I$) <> "Y" Then 45

'    Print
'    Print "ARE YOU SURE ? (YN) ";
'    470 I$ = GetKeyPress$
'    If UCase$(I$) = "N" Then Exit Sub

'   Cls
    'Exit Sub
    '--- I don't think we ever get here



End Sub 'LOOKY






'------------------------------
Sub SortStats (CP%)
	Shared values(), W0(), W1()
	Shared lookyA$()
	
    600 'SORT BASED ON CATEGORY
    If CP% = 1 Then SG% = 0
    If CP% = 2 Then SG% = 10
    If CP% = 3 Then SG% = 14
    If CP% = 4 Then SG% = 2
    If CP% = 5 Then SG% = 1
    If CP% = 6 Then SG% = 15
    If CP% = 7 Then SG% = 18
    If CP% = 8 Then SG% = 19
    If CP% = 9 Then SG% = 16
    If CP% = 10 Then SG% = 4
    If CP% = 11 Then SG% = 3
    If CP% = 12 Then SG% = 17
    If CP% = 14 Then SG% = 22
    If CP% = 15 Then SG% = 20
    If CP% = 17 Then SG% = 12
    If CP% = 16 Then SG% = 13
    If CP% = 18 Then SG% = 23
    If CP% = 19 Then SG% = 5
    If CP% = 20 Then SG% = 21
    If CP% = 21 Then SG% = 6
    If CP% = 22 Then SG% = 8
    If CP% = 23 Then SG% = 9
    If CP% = 24 Then SG% = 7
    If CP% = 25 Then SG% = 11
    For Z = 0 To 12
        For Z1 = Z + 1 To 14
            If values(Z, SG%) >= values(Z1, SG%) Then 605
            For I = 0 To 24: Swap values(Z, I), values(Z1, I): Next
            Swap W0(Z), W0(Z1)
            Swap W1(Z), W1(Z1)
            Swap lookyA$(Z), lookyA$(Z1)

    605 Next Z1: Next Z

End Sub





'------------------------------
Sub TeamSelection_LOOKY (yearNumber$, numberTeams, teamInfo%(), BO%)

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

    If EXITCODE% = 27 Then BO% = 1: Exit Sub
    If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
    If EXITCODE% = -79 Then Start% = NTMS: EXITCODE% = 3: SELECTION% = NTMS: GoTo 20
    If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
    If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NTMS Then Start% = NTMS
    If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 20
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

    For I = 0 To 34:
        teamInfo%(I) = CVI(Q$(I + 10))
    Next
    Close 1

End Sub





'------------------------------
Sub PrintStatsSchedule (totalGames%, teamRatings%(), O%(), O$(), H$())

		LPrint "SCHEDULE RESULTS FOR "; B$
		LPrint
		For I = 1 To 28
			LPrint Using "###"; O%(I);
			LPrint Tab(5); Left$(O$(I), 14);
			LPrint Tab(20); Using "###"; teamRatings%(I);
			LPrint Tab(24); H$(I); Tab(27);
			If I + 28 > totalGames% Then LPrint: GoTo 880
			LPrint Using "###"; O%(I + 28);
			LPrint Tab(31); Left$(O$(I + 28), 14);
			LPrint Tab(46); Using "###"; teamRatings%(I + 28);
			LPrint Tab(50); H$(I + 28); Tab(53);
			If I + 56 > totalGames% Then LPrint: GoTo 880
			LPrint Using "###"; O%(I + 56);
			LPrint Tab(57); Left$(O$(I + 56), 14);
			LPrint Tab(72); Using "###"; teamRatings%(I + 56);
			LPrint Tab(76); H$(I + 56)

		880 Next I
		LPrint
		LPrint Tab(27); "OVERALL RECORD ("; W%; "-"; L%; ")"
		LPrint Tab(27); "HOME RECORD    ("; HW%; "-"; HL%; ")"
		LPrint Tab(27); "ROAD RECORD    ("; VW%; "-"; VL%; ")"
		LPrint Tab(27); "NEUTRAL RECORD ("; NW%; "-"; NL%; ")"
		LPrint Chr$(12)

		125 I$ = GetKeyPress$

End Sub





'------------------------------
Sub PrintStatsSummary (totalGames%, Z(), Z1())

	Shared values(), W0(), W1()
	Shared lookyA$()
	
    530 LPrint "STATISTICS SUMMARY FOR "; B$
    LPrint "NAME"; Tab(15);
    LPrint " G  MIN AVG   FGM  FGA  PCT  3FGM 3FGA  PCT   FTM  FTA  PCT"

    For I = 0 To 14
        LPrint Left$(lookyA$(I), 13); Tab(15);
        'Games
        LPrint Using "## "; values(I, 0);

        'Min, Avg
        If values(I, 10) > 0 Then LPrint Tab(18); Using "####"; Int((values(I, 10) + 30) / 60);: LPrint Tab(24); Using "##"; Int(((values(I, 10) / 60) / values(I, 0)) + .5);

        'FGM, FGA, FG PCT
        LPrint Tab(28); Using "#### ####"; values(I, 2); values(I, 1);
        LPrint Tab(38);
        If values(I, 1) = 0 Then LPrint ".---";: GoTo 531
        If values(I, 2) = values(I, 1) Then LPrint "1.00";: GoTo 531
        If values(I, 2) = 0 Then LPrint ".000";: GoTo 531
        fgPCT! = values(I, 2) / values(I, 1)
        LPrint Using ".###"; fgPCT!;

        '3FGM, 3FGA, 3FG PCT
        531 LPrint Tab(44); Using "#### ####"; W0(I); W1(I);: LPrint Tab(54);
        If W1(I) = 0 Then LPrint ".---";: GoTo 532
        If W1(I) = W0(I) Then LPrint "1.00";: GoTo 532
        If W0(I) = 0 Then LPrint ".000";: GoTo 532
        threeFGPCT! = W0(I) / W1(I)
        LPrint Using ".###"; threeFGPCT!;

        'FTM, FTA, FT PCT
        532 LPrint Tab(60); Using "#### ####"; values(I, 4); values(I, 3);: LPrint Tab(70);
        If values(I, 3) = 0 Then LPrint ".---": GoTo 533
        If values(I, 3) = values(I, 4) Then LPrint "1.00": GoTo 533
        If values(I, 4) = 0 Then LPrint ".000": GoTo 533
        ftPCT! = values(I, 4) / values(I, 3)
        LPrint Using ".###"; ftPCT!

    533 Next

    LPrint: LPrint "TOTALS"; Tab(15);
    'Games
    LPrint Using "##"; totalGames%;

    'FGM / FGA / FGPCT
    LPrint Tab(28); Using "#### ####"; Z(2); Z(1);
    If Z(1) <> 0 Then LPrint Tab(38); Using ".###"; Z(2) / Z(1);

    '3FGM / 3FGA / 3FGPCT
    LPrint Tab(44); Using "#### ####"; Z(13); Z(14);
    If Z(14) <> 0 Then LPrint Tab(54); Using ".###"; Z(13) / Z(14);

    'FTM / FTA / FTPCT
    LPrint Tab(60); Using "#### ####"; Z(4); Z(3);
    If Z(3) <> 0 Then LPrint Tab(70); Using ".###"; Z(4) / Z(3)

    'Games
    LPrint Tab(15); Using "##"; totalGames%;

    'FGM / FGA / FGPCT
    LPrint Tab(28); Using "#### ####"; Z1(2); Z1(1);
    If Z1(1) <> 0 Then LPrint Tab(38); Using ".###"; Z1(2) / Z1(1);

    '3FGM / 3FGA / 3FGPCT
    LPrint Tab(44); Using "#### ####"; Z1(13); Z1(14);
    If Z1(14) <> 0 Then LPrint Tab(54); Using ".###"; Z1(13) / Z1(14);

    'FTM / FTA / FTPCT
    LPrint Tab(60); Using "#### ####"; Z1(4); Z1(3);
    If Z1(3) <> 0 Then LPrint Tab(70); Using ".###"; Z1(4) / Z1(3)

    'Player Stats
    LPrint
    LPrint Tab(15);
    LPrint "               OFF  DEF  TOT": LPrint Tab(15);
    LPrint " PTS   AVG HI  REB  REB  REB   AVG   PF  AST  BLK  STL   TO"

    For I = 0 To 14
        'Player Name
        LPrint Left$(lookyA$(I), 13);: LPrint Tab(15);

        'Points
        LPrint Using "####"; values(I, 2) * 2 + values(I, 4) + W0(I);

        'Average
        If values(I, 0) > 0 Then LPrint Tab(21); Using "##.#"; (values(I, 2) * 2 + values(I, 4) + W0(I)) / values(I, 0);

        'Highest Points, Rebounds
        LPrint Tab(26); Using "## #### #### ####"; values(I, 13); values(I, 12); values(I, 5) - values(I, 12); values(I, 5);

        'Average Rebounds
        If values(I, 0) > 0 Then LPrint Tab(45); Using "##.#"; values(I, 5) / values(I, 0);

        'PF / AST / BLK / STL / TO
        LPrint Tab(51); Using "### ####  ###  ###  ###"; values(I, 6); values(I, 8); values(I, 9); values(I, 7); values(I, 11)
    Next I

    LPrint "TEAM";: LPrint Tab(39); Using "####"; Z(15)

    LPrint: LPrint "TOTALS";
    'Points
    LPrint Tab(14); Using "##### ###.#"; Z(10); Z(10) / totalGames%;
    'Rebounds
    LPrint Tab(29); Using "#### #### ####  ##.#"; Z(12); Z(5) - Z(12) - Z(15); Z(5); Z(5) / totalGames%;
    'PF  AST  BLK  STL  TO
    LPrint Using " #### #### #### #### ####"; Z(6); Z(8); Z(9); Z(7); Z(11)

    'Points
    LPrint Tab(14); Using "##### ###.#"; Z1(10); Z1(10) / totalGames%;
    'Rebounds
    LPrint Tab(29); Using "#### #### ####  ##.#"; Z1(12); Z1(5) - Z1(12) - Z1(15); Z1(5); Z1(5) / totalGames%;
    'PF  AST  BLK  STL  TO
    LPrint Using " #### #### #### #### ####"; Z1(6); Z1(8); Z1(9); Z1(7); Z1(11)

    LPrint Chr$(12)

End Sub
