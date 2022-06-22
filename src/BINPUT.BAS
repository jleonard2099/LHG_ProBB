Sub BINPUT (menuShortcut%)

	ReDim menuChoices$(1 to 5)
	Dim RN
	
    _Title "Basketball: The Pro Game - Team Input"

    'Call Comline(N, cliParam$(), 10) ' Get what was typed on the command line

    Select Case menuShortcut%
        Case 1: RN = 1: Call DoTheInput(RN): Exit Sub 'Skip menu selection
        Case 2: RN = 2: Call DoTheInput(RN): Exit Sub 'Skip menu selection
        Case 3: RN = 3: Call DoTheInput(RN): Exit Sub 'Skip menu selection
        Case 4: RN = 4: Call DoTheInput(RN): Exit Sub 'Skip menu selection
    End Select

	Do
		100 Call TITLE
		'Erase menuChoices$
		menuChoices$(1) = "ENTER A NEW TEAM               "
		menuChoices$(2) = "CHANGE OR EDIT AN EXISTING TEAM"
		menuChoices$(3) = "PRINT A TEAM ROSTER            "
		menuChoices$(4) = "VIEW A TEAM ROSTER             "
		menuChoices$(5) = "EXIT TO MAIN MENU              "
		SELECTION% = 1
		Count% = 5
		Start% = 1
		SCANCODE% = 5
		FileFGColr% = 15: FileBGColr% = 1
		HiLiteFGColr% = 0: HiLiteBGColr% = 7
		Spacing% = 1
		Row% = 8
		Column% = 25

		Call box0(7, 24, 13, 56, 2, 15, 1)
		_MouseShow "DEFAULT"
		MMenuVert menuChoices$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
		_MouseHide

		RN = SELECTION%
		If (SELECTION% >= 1) And (SELECTION% <= 4) Then Call DoTheInput (RN)

	Loop Until SELECTION% = 5

End Sub



'----------------------------------------
'     	  DoTheInput Subroutine
'----------------------------------------
'... explain here ...
Sub DoTheInput (action)
	Shared inputB%(), inputL%(), inputT%()
	Shared inputB$()

	Dim P$(2), T$(4)
	
    ReDim menuChoices$(1 To 5)

    Dim NT As Integer

	Color , 0

	'Positions
    P$(0) = "G"
	P$(1) = "F"
	P$(2) = "C"

    900 Q = action - 1
	'0 = ENTER A NEW TEAM
	'1 = CHANGE OR EDIT AN EXISTING TEAM
	'2 = PRINT A TEAM ROSTER
	'3 = VIEW A TEAM ROSTER

	If Q > 0 Then 

		Do
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
			Locate , 12: Print "ABA SEASONS ARE FOLLOWED BY AN 'A' (IE. 75A)."
			Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
			Locate 7, 27: Input NY$
			If NY$ = "" Then 
				Call TeamSelect_BINPUT (NY$, NT)
			Else
				YN$ = NY$
				Erase teamNames$
				1100 NT = 0
				Erase teamNames$
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
					NT = NT + 1
					teamNames$(NT) = Q$(0)
					teamIndex%(NT) = I1

				1103 Next
				Close 1

				TS% = Int((NT - 1) / 80)

				Call TeamSelect_BINPUT (YN$, NT, inputT%(), inputB$())

			End If
			
			182 If BO% = 1 Then BO% = 0: GoTo 1440

			183 Locate 2, 33: Color inputT%(33), inputT%(34): Print T$(0): Color 15, 4
			Locate , 33: Print "ANY CHANGE (YN)";
			I$ = GetKeyPress$
		Loop Until UCase$(I$) = "N"
		'GoTo 1530

		1530 Color , 0

		Select Case Q
			Case 2:
				1740 LPrint Chr$(15) ': WIDTH "LPT1:", 137
				LPrint: LPrint "DISK ID:"; YN$
				LPrint "TEAM NAME: "; T$(0); " "; T$(1)
				LPrint Using "RECORD ##"; inputT%(1) + inputT%(3);: LPrint "-";: LPrint Using "##    HOME ##"; inputT%(2) + inputT%(4); inputT%(1);
				LPrint "-";: LPrint Using "##    ROAD ##"; inputT%(2); inputT%(3);: LPrint "-";: LPrint Using "##"; inputT%(4): LPrint
				LPrint "                                                      OFF DEF  TOT"
				LPrint "PLAYER        PS  G  MIN  FGA  FGM  PCT  FTA FTM  PCT REB REB  REB  AST  PF STL BLK  PTS  AVG HI"

				For I = 0 To 14
					If inputB$(I, 0) = "" Or inputB$(I, 0) = "XXX" Then 1860
					'Player Name / PS
					LPrint Left$(inputB$(I, 0), 14); Tab(15); inputB$(I, 1); Tab(18);
					'G, MIN, FGA, FGM
					LPrint Using "## #### #### #### "; inputB%(I, 0); inputB%(I, 1); inputB%(I, 2); inputB%(I, 3);

					'FG PCT
					If inputB%(I, 2) = inputB%(I, 3) And inputB%(I, 3) <> 0 Then LPrint "1.000";
					If inputB%(I, 2) <> inputB%(I, 3) Then fgPCT! = inputB%(I, 3) / inputB%(I, 2): LPrint Using ".###"; fgPCT!;

					LPrint Tab(42);
					'FTA, FTM
					LPrint Using "### ### "; inputB%(I, 4); inputB%(I, 5);

					'FT PCT
					If inputB%(I, 4) = inputB%(I, 5) And inputB%(I, 5) <> 0 Then LPrint "1.000 ";
					If inputB%(I, 4) <> inputB%(I, 5) Then ftPCT! = inputB%(I, 5) / inputB%(I, 4): LPrint Using ".### "; ftPCT!;

					LPrint Tab(55);
					'Rebounds / AST / PF / STL / BLK / PTS / AVG HI
					LPrint Using "### ### #### #### ### ### ### #### "; inputB%(I, 6); inputB%(I, 7); inputB%(I, 6) + inputB%(I, 7); inputB%(I, 8); inputB%(I, 9); inputB%(I, 11); inputB%(I, 12); inputB%(I, 3) * 2 + inputB%(I, 5) + inputB%(I, 14);
					LPrint Using "##.# ##"; (inputB%(I, 3) * 2 + inputB%(I, 5) + inputB%(I, 14)) / inputB%(I, 0); inputB%(I, 15)

				1860 Next

				LPrint Chr$(18): LPrint Chr$(12)
				'Exit Sub
			
				'Done with action Q=2

			Case 3: 

				Do

					2000 Color 0, 0: Cls
					Color inputT%(33), inputT%(34): Print T$(0); " "; T$(1);
					Color 14, 0: Print Tab(40); "WON - ";: Color 15
					Print Using "##"; inputT%(1) + inputT%(3);: Color 14
					Print Tab(50); "LOST - ";: Color 15: Print Using "##"; inputT%(2) + inputT%(4)

					61 Locate 3: Color 15: Print "NAME          POS   G  MIN  FGA  FGM  PCT  FTA  FTM  PCT OREB DREB ASST  FLS"

					62 Locate 4: For I = 0 To 14

						63 If Left$(inputB$(I, 0), 3) = "XXX" Then 1001

						'Name  / Pos
						65 Color 3: Print Left$(inputB$(I, 0), 14);
						Color 7: Print Tab(15); inputB$(I, 1);

						'Games
						66 Print Tab(20); Using "##"; inputB%(I, 0);

						'Minutes
						67 Print Tab(23); Using "####"; inputB%(I, 1);

						'FGA
						68 Print Tab(28); Using "####"; inputB%(I, 2);

						'FGM
						69 Print Tab(33); Using "####"; inputB%(I, 3);

						'FG PCT
						70 If inputB%(I, 3) = 0 Then Print Tab(38); ".000";
						If inputB%(I, 3) > 0 Then fgPCT! = inputB%(I, 3) / inputB%(I, 2): Print Tab(38); Using ".###"; fgPCT!;

						'FTA
						71 Print Tab(43); Using "####"; inputB%(I, 4);

						'FTM
						72 Print Tab(48); Using "####"; inputB%(I, 5);

						'FT PCT
						73 If inputB%(I, 5) = 0 Then Print Tab(53); ".000";
						If inputB%(I, 5) > 0 Then ftPCT! = inputB%(I, 5) / inputB%(I, 4): Print Tab(53); Using ".###"; ftPCT!;

						'Off Reb
						74 Print Tab(58); Using "####"; inputB%(I, 6);

						'Def Reb
						75 Print Tab(63); Using "####"; inputB%(I, 7);

						'Assists
						76 Print Tab(68); Using "####"; inputB%(I, 8);

						'Fouls
						77 Print Tab(73); Using "####"; inputB%(I, 9)

					1001 Next

					Color 14: Print: Print "TOTALS-OFF";

					'Games
					102 Print Tab(20); Using "##"; inputL%(0);

					'FGA
					103 Print Tab(28); Using "####"; inputT%(5);

					'FGM
					104 Print Tab(33); Using "####"; inputT%(6);

					'FG PCT
					fgPCT! = inputT%(6) / inputT%(5)
					105 Print Tab(38); Using ".###"; fgPCT!;

					'FTA
					106 Print Tab(43); Using "####"; inputT%(7);

					'FTM
					107 Print Tab(48); Using "####"; inputT%(8);

					'FT PCT
					ftPCT! = inputT%(8) / inputT%(7)
					108 Print Tab(53); Using ".###"; ftPCT!;

					'Off Reb
					109 Print Tab(58); Using "####"; inputT%(9);

					'Def Reb
					110 Print Tab(63); Using "####"; inputT%(10);

					'Assists
					111 Print Tab(68); Using "####"; inputT%(11);

					'Fouls
					112 Print Tab(73); Using "####"; inputT%(12)

					121 Color 11: Print "TOTALS-DEF";

					'FGA
					123 Print Tab(28); Using "####"; inputT%(19);

					'FGM
					124 Print Tab(33); Using "####"; inputT%(20);

					'FG PCT
					fgPCT! = inputT%(20) / inputT%(19)
					125 Print Tab(38); Using ".###"; fgPCT!;

					'FTM
					126 Print Tab(43); Using "####"; inputT%(21);

					'FTA
					127 Print Tab(48); Using "####"; inputT%(22);

					ftPCT! = inputT%(22) / inputT%(21)
					128 Print Tab(53); Using ".###"; ftPCT!;

					'Off Reb
					129 Print Tab(58); Using "####"; inputT%(23);

					'Def Reb
					Print Tab(63); Using "####"; inputT%(24);

					'Assists
					131 Print Tab(68); Using "####"; inputT%(25);

					'Fouls
					132 Print Tab(73); Using "####"; inputT%(26)

					Color 15: Locate 23: Print "HIT ANY KEY TO CONTINUE"
					141 I$ = GetKeyPress$
					Cls: Color inputT%(33), inputT%(34): Print T$(0); " "; T$(1): Color , 0

					151 Locate 3: Color 15: Print "NAME           STL  BLK 3FGA 3FGM  PCT   HI-GAME"

					152 Locate 4: For I = 0 To 14

						153 If Left$(inputB$(I, 0), 3) = "XXX" Then 200

						'Name
						154 Color 3: Print Left$(inputB$(I, 0), 14);

						'Steals
						155 Color 7: Print Tab(15); Using "####"; inputB%(I, 11);

						'Blocks
						156 Print Tab(20); Using "####"; inputB%(I, 12);

						'3FGA
						157 Print Tab(25); Using "####"; inputB%(I, 13);

						'3FGM
						158 Print Tab(30); Using "####"; inputB%(I, 14);

						'3FG PCT
						159 If inputB%(I, 14) = 0 Then Print Tab(35); ".000";
						If inputB%(I, 14) > 0 Then threeFGPCT! = inputB%(I, 14) / inputB%(I, 13): Print Tab(35); Using ".###"; threeFGPCT!;

						'HI GAME
						Print Tab(43); Using "####"; inputB%(I, 15)
						'161 PRINT TAB(53); USING "####"; inputB%(I, 16);
						'162 PRINT TAB(65); USING "####"; inputB%(I, 17)

					200 Next

					201 Color 14: Print: Print "TOTALS-OFF";

					'Steals
					202 Print Tab(15); Using "####"; inputT%(14);

					'Blocks
					203 Print Tab(20); Using "####"; inputT%(16);

					'3FGA
					204 Print Tab(25); Using "####"; inputT%(17);

					'3FGM
					Print Tab(30); Using "####"; inputT%(18);

					'3FG PCT
					208 If inputT%(18) = 0 Then Print Tab(35); ".000"
					If inputT%(18) > 0 Then 
						statPCT! = inputT%(18) / inputT%(17)
						Print Tab(35); Using ".###"; statPCT!
					End If

					221 Color 11: Print "TOTALS-DEF";

					'Steals
					223 Print Tab(15); Using "####"; inputT%(28);

					'Blocks
					224 Print Tab(20); Using "####"; inputT%(30);

					'3FGA
					Print Tab(25); Using "####"; inputT%(31);

					'3FGM
					226 Print Tab(30); Using "####"; inputT%(32);

					'3FG PCT
					228 If inputT%(32) = 0 Then Print Tab(35); ".000"
					If inputT%(32) > 0 Then 
						statPCT! = inputT%(32) / inputT%(31)
						Print Tab(35); Using ".###"; statPCT!
					End If
					
					Color 15: Print: Print "VIEW AGAIN (Y/N)"
					251 I$ = GetKeyPress$

				Loop Until UCase$(I$) = "N"
			
			End Select
	
	End If

	'This is where we enter / edit a team
	Do
		Cls
		Print: Color 12: Print "DO YOU HAVE THE FOLLOWING INFORMATION :"

		Print: Color 14: Print "(1) ";: Color 15: Print "TEAM/INDIVID STEALS ";
		Do
			I$ = GetKeyPress$
		Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)
		If UCase$(I$) = "Y" Then I = 1
		If UCase$(I$) = "N" Then I = 0	
		If I$ = Chr$(27) Then action = 5: Exit Sub
		
		Print I$: ST = I
		Print: Color 14: Print "(2) ";: Color 15: Print "TEAM/INDIVID BLOCKS ";
		Do
			I$ = GetKeyPress$
		Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)
		If UCase$(I$) = "Y" Then I = 1
		If UCase$(I$) = "N" Then I = 0	
		If I$ = Chr$(27) Then action = 5: Exit Sub
		
		Print I$: BK = I
		Print: Color 14: Print "(3) ";: Color 15: Print "TEAM/INDIVID OFF/DEF REB ";
		Do
			I$ = GetKeyPress$
		Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)
		If UCase$(I$) = "Y" Then I = 1
		If UCase$(I$) = "N" Then I = 0	
		If I$ = Chr$(27) Then action = 5: Exit Sub
		
		Print I$: RB = I
		Print: Color 14: Print "(4) ";: Color 15: Print "TEAM TURNOVERS ";
		Do
			I$ = GetKeyPress$
		Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)
		If UCase$(I$) = "Y" Then I = 1
		If UCase$(I$) = "N" Then I = 0	
		If I$ = Chr$(27) Then action = 5: Exit Sub
		
		Print I$: T0 = I
		Print: Print "ANY CHANGE (YN)";
		Do
			I$ = GetKeyPress$
		Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = Chr$(27)
		If UCase$(I$) = "Y" Then I = 1
		If UCase$(I$) = "N" Then I = 0	
		If I$ = Chr$(27) Then action = 5: Exit Sub
	
    'If I = 1 Then 101
	Loop Until I = 2
	
    Cls: Color 14: Print "LEAGUE INFORMATION": Color 15

    120 Locate 3: Input "1. NUMBER OF GAMES "; inputL%(0): If C1 = 1 Then 190

    130 Locate 4: Input "2. AVG FGA/G "; inputL%(1): If C1 = 1 Then 190

    140 Locate 5: Input "3. AVG RB/G "; inputL%(2): If C1 = 1 Then 190

    150 Locate 6: Input "4. LEAGUE FG% "; inputL%(3): If C1 = 1 Then 190

    160 Locate 7: Input "5. AVG PF/G "; inputL%(4): If C1 = 1 Then 190

    170 Locate 8: Input "6. LEAGUE 3FG% "; inputL%(5): If C1 = 1 Then 190

    171 Locate 9: Input "7. LEAGUE POWER ADJ "; inputL%(6): If C1 = 1 Then 190

    172 Locate 10: Input "8. DISK ID "; YN$: If C1 = 1 Then 190

    173 Locate 11: Input "9. TEAM NUMBER "; T: If C1 = 1 Then 190

    190 C1 = 1

	'-----

	Locate 13: Input "# TO CHANGE (0 TO CONTINUE) "; B9	
	On B9 GOTO 120, 130, 140, 150, 160, 170, 171, 172, 173
    C1 = 0
	If X% > 0 Then 
		700 Cls: Color 14: Print "CHANGE TEAM INFORMATION": Color 15
		Print "1. TEAM NAME "; T$(0)
		Print "2. TEAM NICKNAME "; T$(1)
		Print "3. TEAM GAMES PLAYED "; inputT%(0)
		Print "4. HOME WINS "; inputT%(1)
		Print "5. HOME LOSSES "; inputT%(2)
		Print "6. ROAD WINS "; inputT%(3)
		Print "7. ROAD LOSSES "; inputT%(4)
		Print "8. TEXT COLOR (0-15) "; inputT%(33)
		Print "9. BCKGRND COLOR (0-7) "; inputT%(34)
		Print "10. TEAM ABBREV (IE. BOS) "; T$(2)
		Print "11. COACH'S NAME "; T$(3)
		Print "12. ARENA NAME "; T$(4)
		Locate 15: Color inputT%(33), inputT%(34): Print "SAMPLE TEXT": Color 15, 0
		GoTo 255
	End If

    C1 = 0

	'-----

    Cls
	Color 14: Print "TEAM INFORMATION": Color 15

    205 Locate 2: Input "1. TEAM NAME "; T$(0): If C1 = 1 Then 250

    210 Locate 3: Input "2. TEAM NICKNAME "; T$(1): If C1 = 1 Then 250

    215 Locate 4: Input "3. TEAM GAMES PLAYED "; inputT%(0): If C1 = 1 Then 250

    220 Locate 5: Input "4. HOME WINS "; inputT%(1): If C1 = 1 Then 250

    225 Locate 6: Input "5. HOME LOSSES "; inputT%(2): If C1 = 1 Then 250

    230 Locate 7: Input "6. ROAD WINS "; inputT%(3): If C1 = 1 Then 250

    235 Locate 8: Input "7. ROAD LOSSES "; inputT%(4): If C1 = 1 Then 250

    240 Locate 9: Input "8. TEXT COLOR (0-15) "; inputT%(33): If C1 = 1 Then 250

    245 Locate 10: Input "9. BCKGRND COLOR (0-7) "; inputT%(34): If C1 = 1 Then 250

    246 Locate 11: Input "10. TEAM ABBREV (IE. BOS) "; T$(2): If C1 = 1 Then 250

    247 Locate 12: Input "11. COACH'S NAME "; T$(3): If C1 = 1 Then 250

    248 Locate 13: Input "12. ARENA NAME "; T$(4): If C1 = 1 Then 250

    250 Locate 15: Color inputT%(33), inputT%(34): Print "SAMPLE TEXT": Color 15, 0

    255 C1 = 1

	'-----

	Locate 17: Input "# TO CHANGE (0 TO CONTINUE) "; B9
	On B9 GOTO 205, 210, 215, 220, 225, 230, 235, 240, 245, 246, 247, 248
    C1 = 0
	If X% > 0 Then 
		760 Cls: Color 14: Print "CHANGE TEAM OFFENSIVE INFORMATION": X% = 2: Color 15
		Print " 1. FGM "; inputT%(6)
		Print " 2. FGA "; inputT%(5)
		Print " 3. FTM "; inputT%(8)
		Print " 4. FTA "; inputT%(7)
		Print " 5. OFF REB "; inputT%(9)
		Print " 6. DEF REB "; inputT%(10)
		Print " 7. ASSISTS "; inputT%(11)
		Print " 8. FOULS "; inputT%(12)
		Print " 9. DISQ "; inputT%(13)
		Print "10. STEALS "; inputT%(14)
		Print "11. TURNOVERS "; inputT%(15)
		Print "12. BLK SHOTS "; inputT%(16)
		Print "13. 3-FGM "; inputT%(18)
		Print "14. 3-FGA "; inputT%(17)
		GoTo 335
	End If
    
	Cls
	Color 14: Print "TEAM OFFENSIVE INFORMATION": Color 15

    265 Locate 2: Input " 1. FGM "; inputT%(6): If C1 = 1 Then 335

    270 Locate 3: Input " 2. FGA "; inputT%(5): If C1 = 1 Then 335

    275 Locate 4: Input " 3. FTM "; inputT%(8): If C1 = 1 Then 335

    280 Locate 5: Input " 4. FTA "; inputT%(7): If C1 = 1 Then 335

    285 Locate 6: Input " 5. OFF REB "; inputT%(9): If C1 = 1 Then 335

    290 Locate 7: Input " 6. DEF REB "; inputT%(10): If C1 = 1 Then 335
    If inputT%(9) = 0 And RB = 0 Then inputT%(9) = .32 * inputT%(10): inputT%(10) = inputT%(10) - inputT%(9): Locate 6: Print " 5. OFF REB "; inputT%(9): Locate 7: Print " 6. DEF REB "; inputT%(10)

    295 Locate 8: Input " 7. ASSISTS "; inputT%(11): If C1 = 1 Then 335

    300 Locate 9: Input " 8. FOULS "; inputT%(12): If C1 = 1 Then 335

    305 Locate 10: Input " 9. DISQ "; inputT%(13): If C1 = 1 Then 335

    310 Locate 11: Input "10. STEALS "; inputT%(14): If C1 = 1 Then 335

    315 Locate 12: Input "11. TURNOVERS "; inputT%(15): If C1 = 1 Then 335

    320 Locate 13: Input "12. BLK SHOTS "; inputT%(16): If C1 = 1 Then 335

    325 Locate 14: Input "13. 3-FGM "; inputT%(18): If C1 = 1 Then 335

    330 Locate 15: Input "14. 3-FGA "; inputT%(17): If C1 = 1 Then 335

    335 C1 = 1
	Locate 17: Input "# TO CHANGE (0 TO CONTINUE) "; B9
	On B9 GOTO 265, 270, 275, 280, 285, 290, 295, 300, 305, 310, 315, 320, 325, 330

	'-----

    C1 = 0
	If X% > 0 Then 
		840 Cls: Color 14: Print "CHANGE TEAM DEFENSIVE INFORMATION": X% = 2: Color 15
		Print " 1. FGM "; inputT%(20)
		Print " 2. FGA "; inputT%(19)
		Print " 3. FTM "; inputT%(22)
		Print " 4. FTA "; inputT%(21)
		Print " 5. OFF REB "; inputT%(23)
		Print " 6. DEF REB "; inputT%(24)
		Print " 7. ASSISTS "; inputT%(25)
		Print " 8. FOULS "; inputT%(26)
		Print " 9. DISQ "; inputT%(27)
		Print "10. STEALS "; inputT%(28)
		Print "11. TURNOVERS "; inputT%(29)
		Print "12. BLK SHOTS "; inputT%(30)
		Print "13. 3-FGM "; inputT%(32)
		Print "14. 3-FGA "; inputT%(31)
		GoTo 415
	End If
			
    Cls
	Color 14: Print "TEAM DEFENSIVE INFORMATION": Color 15

    345 Locate 2: Input " 1. FGM "; inputT%(20): If C1 = 1 Then 415

    350 Locate 3: Input " 2. FGA "; inputT%(19): If C1 = 1 Then 415

    355 Locate 4: Input " 3. FTM "; inputT%(22): If C1 = 1 Then 415

    360 Locate 5: Input " 4. FTA "; inputT%(21): If C1 = 1 Then 415

    365 Locate 6: Input " 5. OFF REB "; inputT%(23): If C1 = 1 Then 415

    370 Locate 7: Input " 6. DEF REB "; inputT%(24): If C1 = 1 Then 415
    If inputT%(23) = 0 And RB = 0 Then inputT%(23) = .35 * inputT%(24): inputT%(24) = inputT%(24) - inputT%(23): Locate 6: Print " 5. OFF REB "; inputT%(23): Locate 7: Print " 6. DEF REB "; inputT%(24)

    375 Locate 8: Input " 7. ASSISTS "; inputT%(25): If C1 = 1 Then 415

    380 Locate 9: Input " 8. FOULS "; inputT%(26): If C1 = 1 Then 415

    385 Locate 10: Input " 9. DISQ "; inputT%(27): If C1 = 1 Then 415

    390 Locate 11: Input "10. STEALS "; inputT%(28): If C1 = 1 Then 415

    395 Locate 12: Input "11. TURNOVERS "; inputT%(29): If C1 = 1 Then 415

    400 Locate 13: Input "12. BLK SHOTS "; inputT%(30): If C1 = 1 Then 415

    405 Locate 14: Input "13. 3-FGM "; inputT%(32): If C1 = 1 Then 415

    410 Locate 15: Input "14. 3-FGA "; inputT%(31): If C1 = 1 Then 415

    415 C1 = 1
	Locate 17: Input "# TO CHANGE (0 TO CONTINUE) "; B9
	On B9 GOTO 345, 350, 355, 360, 365, 370, 375, 380, 385, 390, 395, 400, 405, 410
    C1 = 0

	If X% > 0 Then 
		Cls
		Color 14: Print "CHANGE PLAYER STATS": X% = 2: Color 15
		For I = 0 To 14: Print Using "##"; I + 1;: Print "  "; inputB$(I, 0)
		Next: Print: Print "# TO CHANGE (1-15) (0 TO CONTINUE) ";: Input I
		Locate 1, 1
		
		'Continue
		If I = 0 Then 
			X% = 0
			Call SaveTeamFile (yearNumber$)
		End If
		
		I = I - 1
		Locate 1, 1
		Cls
		Color 14: Print Tab(31); "CHANGE PLAYER INFORMATION": Color 15
		Print " 1. NAME (END TO QUIT) "; inputB$(I, 0)
		Print " 2. POSITION "; inputB$(I, 1)
		Print " 3. GAMES "; inputB%(I, 0)
		Print " 4. MINUTES "; inputB%(I, 1)
		Print " 5. FGM "; inputB%(I, 3)
		Print " 6. FGA "; inputB%(I, 2)
		Print " 7. FTM "; inputB%(I, 5)
		Print " 8. FTA "; inputB%(I, 4)
		Print " 9. OFF REB "; inputB%(I, 6)
		Print "10. DEF REB "; inputB%(I, 7)
		Print "11. ASSISTS "; inputB%(I, 8)
		Print "12. FOULS "; inputB%(I, 9)
		Print "13. DISQ "; inputB%(I, 10)
		Print "14. STEALS "; inputB%(I, 11)
		Print "15. BLOCKS "; inputB%(I, 12)
		Print "16. 3-FGM "; inputB%(I, 14)
		Print "17. 3-FGA "; inputB%(I, 13)
		Print "18. HI-GAME "; inputB%(I, 15)
		Print: 'PRINT "19. PENETRATION (1-9) ";inputB%(I,16)
		Print: 'PRINT "20. DEF ADJ (-5 TO 5) ";inputB%(I,17)
		Print "21. ACTIVE FLAG (0-YES 1-NO) "; inputB%(I, 18)

		GoTo 520

	End If

'-- How can 520 be in the goto but also inside a for loop?
'-- only if it's a subroutine!!!!
    If inputT%(28) = 0 And ST = 0 Then inputT%(28) = inputT%(5) * .1
    If inputT%(14) = 0 And ST = 0 Then inputT%(14) = inputT%(19) * .1
    If inputT%(16) = 0 And BK = 0 Then inputT%(16) = inputT%(19) * .06
    If inputT%(30) = 0 And BK = 0 Then inputT%(30) = inputT%(5) * .06
	
    For I = 0 To 14: 
		Cls
        Color 14: Print Tab(25); "PLAYER INFORMATION FOR PLAYER "; I + 1: Color 15

        424 Locate 2: Input " 1. NAME (END TO QUIT) "; inputB$(I, 0)
		
		If C1 <> 1 Then 
		
			If inputB$(I, 0) = "END" Then 
				For J = I To 14
					inputB$(J, 0) = "XXX"
					inputB$(J, 1) = "X"
					For I1 = 0 To 19: inputB%(J, I1) = -1: Next
				Next

				600 Cls: Color 14: Print "CHANGE LEAGUE INFORMATION": X% = 2: Color 15: Print
				Print "1. NUMBER OF GAMES "; inputL%(0)
				Print "2. AVG FGA/G "; inputL%(1)
				Print "3. AVG RB/G "; inputL%(2)
				Print "4. LEAGUE FG% "; inputL%(3)
				Print "5. AVG PF/G "; inputL%(4)
				Print "6. LEAGUE 3FG% "; inputL%(5)
				Print "7. LEAGUE POWER ADJ "; inputL%(6)
				Print "8. DISK ID "; YN$
				Print "9. TEAM NUMBER "; T
				GoTo 190

			End If
			
			426 Locate 3: Input " 2. POSITION "; inputB$(I, 1): If C1 = 1 Then 520

			430 Locate 4: Input " 3. GAMES "; inputB%(I, 0): If C1 = 1 Then 520

			435 Locate 5: Input " 4. MINUTES "; inputB%(I, 1): If C1 = 1 Then 520

			440 Locate 6: Input " 5. FGM "; inputB%(I, 3): If C1 = 1 Then 520

			445 Locate 7: Input " 6. FGA "; inputB%(I, 2): If C1 = 1 Then 520

			450 Locate 8: Input " 7. FTM "; inputB%(I, 5): If C1 = 1 Then 520

			455 Locate 9: Input " 8. FTA "; inputB%(I, 4): If C1 = 1 Then 520

			460 Locate 10: Input " 9. OFF REB "; inputB%(I, 6): If C1 = 1 Then 520

			465 Locate 11: Input "10. DEF REB "; inputB%(I, 7): If C1 = 1 Then 520
			If inputB%(I, 6) = 0 And RB = 0 Then inputB%(I, 6) = .32 * inputB%(I, 7): inputB%(I, 7) = inputB%(I, 7) - inputB%(I, 6): Locate 10: Print " 9. OFF REB "; inputB%(I, 6): Locate 11: Print "10. DEF REB "; inputB%(I, 7)

			470 Locate 12: Input "11. ASSISTS "; inputB%(I, 8): If C1 = 1 Then 520

			475 Locate 13: Input "12. FOULS "; inputB%(I, 9): If C1 = 1 Then 520

			480 Locate 14: Input "13. DISQ "; inputB%(I, 10): If C1 = 1 Then 520
			If ST = 1 Then 485
			If Left$(inputB$(I, 1), 1) = "C" Then inputB%(I, 11) = inputB%(I, 1) / 48 * 1.2: Locate 15: Print "14. STEALS "; inputB%(I, 11): GoTo 486
			If Left$(inputB$(I, 1), 1) = "F" Then inputB%(I, 11) = inputB%(I, 1) / 48 * 1.55: Locate 15: Print "14. STEALS "; inputB%(I, 11): GoTo 486
			If Left$(inputB$(I, 1), 1) = "G" Then inputB%(I, 11) = inputB%(I, 1) / 48 * 2.3: Locate 15: Print "14. STEALS "; inputB%(I, 11): GoTo 486

			485 Locate 15: Input "14. STEALS   "; inputB%(I, 11): If C1 = 1 Then 520

			486 If BK = 1 Then 490
			If Left$(inputB$(I, 1), 1) = "C" Then inputB%(I, 12) = inputB%(I, 1) / 48 * 2.5: Locate 16: Print "15. BLOCKS "; inputB%(I, 12): GoTo 495
			If Left$(inputB$(I, 1), 1) = "F" Then inputB%(I, 12) = inputB%(I, 1) / 48 * 1.2: Locate 16: Print "15. BLOCKS "; inputB%(I, 12): GoTo 495
			If Left$(inputB$(I, 1), 1) = "G" Then inputB%(I, 12) = inputB%(I, 1) / 48 * .4: Locate 16: Print "15. BLOCKS "; inputB%(I, 12): GoTo 495

			490 Locate 16: Input "15. BLOCKS "; inputB%(I, 12): If C1 = 1 Then 520

			495 Locate 17: Input "16. 3-FGM "; inputB%(I, 14): If C1 = 1 Then 520

			500 Locate 18: Input "17. 3-FGA "; inputB%(I, 13): If C1 = 1 Then 520

			505 Locate 19: Input "18. HI-GAME "; inputB%(I, 15): If C1 = 1 Then 520
			'LOCATE 20:INPUT "19. PENETRATION (1-9) ";inputB%(I,16):IF C1=1 THEN 520
			'LOCATE 21:INPUT "20. DEF ADJ (-5 TO 5) ";inputB%(I,17):IF C1=1 THEN 520

			516 Locate 22: Input "21. ACTIVE FLAG (0-YES 1-NO) "; inputB%(I, 18): If C1 = 1 Then 520
		End If


		520 '
		Do 
			
			521 C1 = 1
			Locate 23: Print "# TO CHANGE (0 TO CONTINUE)";: Input B9
			On B9 GOTO 424, 426, 430, 435, 440, 445, 450, 455, 460, 465, 470, 475, 480, 485, 490, 495, 500, 505, 521, 521, 516
			C1 = 0

			If X% > 0 Then 

				Cls
				Color 14: Print "CHANGE PLAYER STATS": X% = 2: Color 15
				For I = 0 To 14: Print Using "##"; I + 1;: Print "  "; inputB$(I, 0)
				Next: Print: Print "# TO CHANGE (1-15) (0 TO CONTINUE) ";: Input I
				Locate 1, 1
				
				'Continue
				If I = 0 Then 
					X% = 0
					Call SaveTeamFile (yearNumber$)
				End If
				
				I = I - 1
				Locate 1, 1
				Cls
				Color 14: Print Tab(31); "CHANGE PLAYER INFORMATION": Color 15
				Print " 1. NAME (END TO QUIT) "; inputB$(I, 0)
				Print " 2. POSITION "; inputB$(I, 1)
				Print " 3. GAMES "; inputB%(I, 0)
				Print " 4. MINUTES "; inputB%(I, 1)
				Print " 5. FGM "; inputB%(I, 3)
				Print " 6. FGA "; inputB%(I, 2)
				Print " 7. FTM "; inputB%(I, 5)
				Print " 8. FTA "; inputB%(I, 4)
				Print " 9. OFF REB "; inputB%(I, 6)
				Print "10. DEF REB "; inputB%(I, 7)
				Print "11. ASSISTS "; inputB%(I, 8)
				Print "12. FOULS "; inputB%(I, 9)
				Print "13. DISQ "; inputB%(I, 10)
				Print "14. STEALS "; inputB%(I, 11)
				Print "15. BLOCKS "; inputB%(I, 12)
				Print "16. 3-FGM "; inputB%(I, 14)
				Print "17. 3-FGA "; inputB%(I, 13)
				Print "18. HI-GAME "; inputB%(I, 15)
				Print: 'PRINT "19. PENETRATION (1-9) ";inputB%(I,16)
				Print: 'PRINT "20. DEF ADJ (-5 TO 5) ";inputB%(I,17)
				Print "21. ACTIVE FLAG (0-YES 1-NO) "; inputB%(I, 18)

				'GoTo 520
			
			Loop Until B9 = 0
			
		Next I

	End If

End Sub


'----------------------------------------
'     TeamSelect_BINPUT Subroutine
'----------------------------------------
'... explain here ...
Sub TeamSelect_BINPUT (yearNumber$, numberTeams)

	Shared inputL%(), inputT%()
	Shared inputB$()

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

    T = teamIndex%(SELECTION%)

    1480 Open D$(0) + "PROBBTMS." + yearNumber$ For Random As #1 Len = 1031
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

    Get #1, T

    T$(0) = Q$(0): T$(1) = Q$(1): For I = 0 To 6: inputL%(I) = CVI(Q$(I + 2)): Next

    For I = 0 To 34
		inputT%(I) = CVI(Q$(I + 10))
	Next

    For I = 0 To 14
        inputB$(I, 0) = Q$(22 * I + 45)
        inputB$(I, 1) = Q$(22 * I + 46)
        For I1 = 0 To 19
            inputB%(I, I1) = CVI(Q$(22 * I + 47 + I1))
    Next: Next

    T$(2) = Q$(375): T$(3) = Q$(376): T$(4) = Q$(377)

    Close 1


    1486 If Right$(T$(0), 1) = " " Then T$(0) = Left$(T$(0), Len(T$(0)) - 1): GoTo 1486

    1487 If Right$(T$(1), 1) = " " Then T$(1) = Left$(T$(1), Len(T$(1)) - 1): GoTo 1487

    1488 If Right$(T$(2), 1) = " " Then T$(2) = Left$(T$(2), Len(T$(2)) - 1): GoTo 1488

    1489 If Right$(T$(3), 1) = " " Then T$(3) = Left$(T$(3), Len(T$(3)) - 1): GoTo 1489

    1490 If Right$(T$(4), 1) = " " Then T$(4) = Left$(T$(4), Len(T$(4)) - 1): GoTo 1490

	Call SaveTeamFile (yearNumber$)

    'GoTo 600

End Sub


'----------------------------------------
'     SaveTeamFile Subroutine
'----------------------------------------
'... explain here ...
Sub SaveTeamFile (yearNumber$)

	Shared inputL%(), inputT%()
	Shared inputB$()

    Cls
	
    Open D$(0) + "PROBBTMS." + yearNumber$ For Random As #1 Len = 1031
	
    Y = LOF(1)

    If Q = 0 Then T = Y / 1031 + 1

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

    LSet Q$(0) = T$(0): LSet Q$(1) = T$(1): For I = 0 To 6: LSet Q$(I + 2) = MKI$(inputL%(I)): Next
    LSet Q$(9) = MKI$(999)

    For I = 0 To 34: LSet Q$(I + 10) = MKI$(inputT%(I)): Next

    For I = 0 To 14
        LSet Q$(22 * I + 45) = inputB$(I, 0)
        LSet Q$(22 * I + 46) = inputB$(I, 1)
        For I1 = 0 To 19
            LSet Q$(22 * I + 47 + I1) = MKI$(inputB%(I, I1))
		Next
	Next

    LSet Q$(375) = T$(2): LSet Q$(376) = T$(3): LSet Q$(377) = T$(4)

    Put #1, T
	
	Close 1

End Sub
