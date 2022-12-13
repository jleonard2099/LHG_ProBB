Sub BLANK ()

    Dim H1$(40)

    _Title "Basketball: The Pro Game - Erase League"

    Cls

    Do
        'Display list of files...

        Spec$ = diskPaths$(1) + "*.LIB"
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
				Print "SELECT LEAGUE TO ERASE": Color 15
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
				Print "-SELECTS LEAGUE   ";
				Color 14
				Print "HIT ESCAPE KEY TO EXIT";
				Color 15

				_MouseShow "DEFAULT"
				MAMenu leagueFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
				_MouseHide

				If ExitCode% <> 27 Then

					targetFile$ = RTrim$(leagueFiles$(Selection%))
					Locate 2, 33: Print targetFile$
					Locate , 33: Print "ANY CHANGE (YN)";
					Color 15, 0

					Do
						I$ = GetKeyPress$
					Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

					If UCase$(I$) = "Y" Then
					'proceed with erasing all files
						WE$ = Left$(targetFile$, Len(targetFile$) - 4)
						LH$ = Left$(WE$, 8)

						Cls
						Locate 10, 8
						Call CSMsgBox("WARNING!!!::: THIS WILL ERASE ALL COMPILED STATS FOR THIS LEAGUE. IF YOU STILL WISH TO ERASE ALL STATS, HIT 'Y' FOR YES", 26, Pass)
						A$ = GetKeyPress$
						Call CSMsgBox("", 0, Pass)

						If A$ = "Y" Then

							Open diskPaths$(1) + LH$ + ".TMS" For Input As #2
							Input #2, LG$, D1$, D2$, D3$, D4$
							For XW = 1 To TEAMS_PER_CONFERENCE
								Input #2, H1$(XW)
							Next
							Close 2

							For XW = 1 To TEAMS_PER_CONFERENCE
								If H1$(XW) <> Chr$(42) Then

									Print "DELETING "; H1$(XW); " STATS"
									Kill (diskPaths$(1) + H1$(XW))
									Kill (diskPaths$(1) + H1$(XW) + "R")
									Kill (diskPaths$(1) + H1$(XW) + "S")
									Kill (diskPaths$(1) + H1$(XW) + "D")
								End If
							Next
							Cls
						End If

					End If 'Done checking for Any Change

				End If 'Done checking for ESC

			'Done looping through league selection / confirmation
			Loop Until ExitCode% = 27

		End If 'Done checking file count

	'...Until we press ESC
	Loop Until ExitCode% = 27 Or Count% = 0

END SUB
