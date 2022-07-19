
SUB ALIGN ()

	Dim alignA$(15), AN$(15), AR$(15)

	' Stats are working b/c these are all singles
	' We are not using Def Int
	Dim AN(15, 14), AR(15, 14)
	Dim W0(15), W1(15), W0R(15), WR1(15), W0N(15), WN1(15)
	Dim Z(15), Z1(15), ZR(15), ZR1(15)

	_Title "Basketball: The Pro Game - Align Stats"

	Cls

	Color 12, 0: Cls
	Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO ALIGN"
	Color 15
	Locate 4, 5: Print "EXAMPLE - A:"
	Locate , 5: Print "          C:\HAFFNER"
	Locate , 5: Print "          C:\HAFFNER\1960"
	Locate , 5: Print "          D:\BASKET"

	Do
		'-- can't this just be blank lines?
		For J = 10 To 13: 
			Locate J
			Print Space$(62)
		Next

		Color 10
		Locate 10, 5: Line Input "ROAD STATS:     "; PR$
		Locate , 5: Line Input "CURRENT STATS:  "; PC$
		Color 14
		Locate 13, 5: Print "ANY CHANGE (Y/N)"
		I$ = GetKeyPress$
	Loop Until UCase$(I$) = "N"


	Do
		Locate 15, 5: Print Space$(55)
		Locate 16, 5: Print Space$(55)
		Color 11
		Locate 15, 5: Input "ENTER NAME OF STAT FILE (EX.- MINNESOT.90)"; Z$

		Locate , 5: Print "ANY CHANGE (YN)"

		I$ = GetKeyPress$

		If UCase$(I$) = "N" Then
		
			If Not _FileExists(PR$ + "\" + Z$) Then 
				Cls: Locate 2, 10: Color 7: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
				Print: Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$
			Else

				Open PR$ + "\" + Z$ For Input As #1

				Input #1, BR$

				For I = 0 To 14: Input #1, AR$(I)
					For J = 0 To 13: Input #1, AR(I, J): Next
					Input #1, W0R(I)
					Input #1, WR1(I)
				Next
				For L = 0 To 15: Input #1, ZR(L): Next
				For L = 0 To 15: Input #1, ZR1(L): Next

				Close #1


				If Not _FileExists(PC$ + "\" + Z$) Then 
					Cls: Locate 2, 10: Color 7: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
					Print: Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$
				Else
					Open PC$ + "\" + Z$ For Input As #1

					Input #1, B$

					For I = 0 To 14: Input #1, alignA$(I)
						For J = 0 To 13: Input #1, A(I, J): Next
						Input #1, W0(I)
						Input #1, W1(I)
					Next
					For L = 0 To 15: Input #1, Z(L): Next
					For L = 0 To 15: Input #1, Z1(L): Next

					Close #1

					Cls
					Locate 1, 32
					Color 12: Print "STAT ROSTERS"

					Locate , 26
					Color 15: Print "CURRENT  ";
					Color 7: Print "             ROAD"

					For I = O To 14
						Locate I + 4, 22
						Color 12: Print Using "##"; I;: Print " ";
						Color 15: Print alignA$(I);: Color 12: Print Tab(44); Using "##"; I;: Print " ";
						Color 7: Print AR$(I)
					Next

					Color 11: Locate 22: Line Input "HIT ENTER TO ALIGN"; DUMMY$
					Color 11: Locate 22: Print "ALIGNING FILES . . .             "

					N = 0

					Do 

						For J = 0 To 14
							If alignA$(N) <> AR$(J) Then
								AN$(N) = AR$(J)

								For K = 0 To 13
									AN(N, K) = AR(J, K): W0N(N) = W0R(J): WN1(N) = WR1(J)
								Next K
							End If
						Next J

						N = N + 1

						If N = 15 Then 
							KKK = 1
							Locate 22: Print Space$(53)

							SEQ = 0

							For K = 0 To 14
								If AN$(K) <> alignA$(K) Then 
									SEQ = SEQ + 1
								End If						
							Next

							If SEQ = 0 Then 
								Cls
								Locate 1, 32: Color 12: Print "STAT ROSTERS"
								Locate , 26: Color 15: Print "CURRENT  ";
								Color 7: Print "             ROAD"

								For I = O To 14: 
									Locate I + 4, 22: Color 12: Print Using "##"; I;: Print " ";
									Color 15: Print alignA$(I);
									Color 12: Print Tab(44); Using "##"; I;: Print " ";
									Color 7: Print AN$(I)
								Next

								Color 11: Locate 22: Line Input "HIT ENTER TO ALIGN"; DUMMY$

								Open PR$ + "\" + Z$ For Output As #1
								Write #1, BR$
								For I = 0 To 14: Write #1, AN$(I)
								For J = 0 To 13: Write #1, AN(I, J): Next: Write #1, W0N(I): Write #1, WN1(I): Next
								For L = 0 To 15: Write #1, ZR(L): Next
								For L = 0 To 15: Write #1, ZR1(L): Next
								Close #1

								Locate 19, 20: Print "                                                  "

								Color 15
								Cls
								Locate 8, 20: Print "RE-ALIGNMENT COMPLETED"

								Print
								Locate 12, 20: Color 12: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

							Else 
								Locate 20
								Color 14: Print "WARNING !! - - ROAD STAT FILE CONTAINS PLAYERS NOT IN EXISTING STATS.  YOU MUST"
								Print "CHANGE ROAD STAT FILE USING OPTION #6 ON GAME MENU BEFORE ATTEMPTING STAT MERGE."
								Locate 23, 30
								Color 13: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

							End If 'Done checking if players are in sequence
									
						End If 'Done checking N = 15

					Loop Until N = 15

				End If 'Done checking if home file exists

			End If 'Done checking if road file exists
		
		End If 'Done checking if ANY CHANGE = Y/N
		
	Loop Until UCase$(I$) = "N"
	
END SUB
