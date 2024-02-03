'----------------------------------------
'            BLANK Subroutine
'----------------------------------------
'This routine assists the user in choosing
'a league, and then proceeds to delete all 
'stats files for all the associated teams
Sub BLANK ()

    _Title "Basketball: The Pro Game - Erase League Stats"

    Shared statFiles$()

	leagueFile$ = ChooseFile$ (diskPaths$(1) + "*.LIB", "LEAGUE", ExitCode%)

	If ExitCode% <> 27 And ExitCode% <> 0 Then

        MsgText$ = "WARNING!!! THIS WILL ERASE ALL COMPILED STATS FOR THIS LEAGUE. IF YOU STILL WISH TO ERASE ALL STATS, HIT 'Y' FOR YES"
        BoxWidth = 27
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        A$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

		If UCase$(A$) = "Y" Then

        	Open diskPaths$(1) + leagueFile$ + ".TMS" For Input As #1

        	Input #1, league$, D1$, D2$, D3$, D4$

        	For XW = 1 To TEAMS_PER_CONFERENCE
        		Input #1, statFiles$(XW)
        	Next

        	Close #1

        	For XW = 1 To TEAMS_PER_CONFERENCE
        		If statFiles$(XW) <> Chr$(42) Then
                    statFile$ = diskPaths$(1) + statFiles$(XW)
        			Print "DELETING "; statFiles$(XW); " STATS"
                    If _FileExists (statFile$) Then Kill statFile$
                    If _FileExists (statFile$ + "R") Then Kill statFile$ + "R"
                    If _FileExists (statFile$ + "S") Then Kill statFile$ + "S"
                    If _FileExists (statFile$ + "D") Then Kill statFile$ + "D"
        		End If
        	Next

		End If 'Done with keypress from CsMsgBox

	End If
	
END SUB
