'----------------------------------------
'            CARLDR Subroutine
'----------------------------------------
'This routine provides a menu for the user
'that allows them to Update and View the
'Career leaders for a specific team
Sub CARLDR ()

    _Title "Basketball: The Pro Game - Career Leaders"

    Do

        Color 15, 0
        Cls

        Call TITLE

        ReDim menuChoices$(1 To 3)
        menuChoices$(1) = "UPDATE CAREER LEADERS      "
        menuChoices$(2) = "VIEW / PRINT CAREER LEADERS"
        menuChoices$(3) = "RETURN TO UTILITIES MENU   "
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 6
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call UpdateLeaders

                Case 2:
                    Call CareerLeaderOptions

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'       UpdateLeaders Subroutine
'----------------------------------------
'This routine reads in the current stats, 
'calculates the latest values (based on
'the criteria given) and then creates /
'overwrites the TCL file with the newest
'calculated values.
Sub UpdateLeaders ()


End Sub


'----------------------------------------
'     CareerLeaderOptions Subroutine
'----------------------------------------
'This routine reads in previously compiled
'career stats (.TCL file) and then provides
'the user options to print or view the stats.
Sub CareerLeaderOptions ()


End Sub


'----------------------------------------
'       PrintCareerLeaders Subroutine
'----------------------------------------
'This routine sends formatted career stats 
'(loaded from the .TCL file) to the user's 
'default printer
Sub PrintCareerLeaders (printDest, destFile$)


	If printDest = 1 Then

		Open destFile$ For Input As #2

		Do While Not EOF(2)
			Line Input #2, X$
			LPrint X$
		Loop

		Close #2

		LPrint Chr$(12)

		Kill destFile$

	End If

End Sub

'----------------------------------------
'      ViewCareerLeaders Subroutine
'----------------------------------------
'This routine displays formatted career 
'stats (loaded from the .TCL file) to the 
'primary screen
Sub ViewCareerLeaders ()


End Sub
