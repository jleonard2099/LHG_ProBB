'----------------------------------------
'          EditStats Subroutine
'----------------------------------------
'This subroutine presents a menu of options
'for the user to edit replay stats.
Sub EditStats ()

    _Title "Basketball: The Pro Game - Stats Input"

    ReDim menuChoices$(1 To 3)

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "EDIT / CHANGE REPLAY STATS    "
        menuChoices$(2) = "EDIT / CHANGE SCHEDULE RESULTS"
        menuChoices$(3) = "RETURN TO MAIN MENU           "
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1, 2:
                    Call EditStatsSchedules(SELECTION%)

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'     EditStatsSchedules Subroutine
'----------------------------------------
'This subroutine lets the user pick a team
'and then calls the corresponding routine
'for the user to edit Stats or Schedule
'Results.
Sub EditStatsSchedules (action%)

    teamNbr = 0
    Call ChooseTeam(YN$, NTMS, teamNbr, team$)
    statFile$ = nameStatFile$(team$)

    If action% = 2 Then

        Call ChangeScheduleStats(statFile$)

    Else
        'action% = 1
        If Not _FileExists(diskPaths$(1) + statFile$) Then

            MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
            BoxWidth = 24
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            Call ModifyTeamStats(statFile$)

        End If

    End If

End Sub


'----------------------------------------
'       ModifyTeamStats Subroutine
'----------------------------------------
'This subroutine inputs stats from the
'chosen stat file and then gives the user
'an interface for modifying the stats.
Sub ModifyTeamStats (statFile$)

    Shared stats3FGM!(), stats3FGA!()
    Shared statsOffTot!(), statsDefTot!()
    Shared statPlyrVal!()

    Shared statPlyrNames$()

    Call ReadTeamStats(teamName$, statFile$)

    '---------------
    ' Team Stats
    '---------------
    Color 15, 0
    Cls

    X$ = "OFFENSIVE TEAM STATS"
    Call TITLE_EDITSTATS(2, X$)

    Locate 3, 1
    Print " 1. STAT FILE NAME  "; statFile$
    Print " 2. TM MINUTES ";
    Print Using "#####"; Int(statsOffTot!(0) / 60)
    Print " 3. TM FGA "; statsOffTot!(1)
    Print " 4. TM FGM "; statsOffTot!(2)
    Print " 5. TM 3FGA "; statsOffTot!(14)
    Print " 6. TM 3FGM "; statsOffTot!(13)
    Print " 7. TM FTA "; statsOffTot!(3)
    Print " 8. TM FTM "; statsOffTot!(4)
    Print " 9. TM OFF RB "; statsOffTot!(12)
    Print "10. TM TOT RB "; statsOffTot!(5)
    Print "11. TM FOULS "; statsOffTot!(6)
    Print "12. TM STEALS "; statsOffTot!(7)
    Print "13. TM ASSISTS "; statsOffTot!(8)
    Print "14. TM BLOCKS "; statsOffTot!(9)
    Print "15. TM TURNOVERS "; statsOffTot!(11)
    Print "16. TM POINTS "; statsOffTot!(10)

    Do

        Do
            Locate 22, 29
            Print Space$(10)
            Locate 22, 1
            Input "# TO CHANGE (0 TO CONTINUE)"; B9
        Loop Until B9 >= 0 And B9 <= 16

        Select Case B9

            Case 1:
                Locate 3, 1: Input " 1. STAT FILE NAME "; statFile$
                Locate 3, 19: Print " "

            Case 2:
                Locate 4, 15: Print Space$(10)
                Locate 4, 1: Input " 2. TM MINUTES"; statsOffTot!(0)
                Locate 4, 15: Print " "
                statsOffTot!(0) = statsOffTot!(0) * 60

            Case 3:
                Locate 5, 11: Print Space$(10)
                Locate 5, 1: Input " 3. TM FGA"; statsOffTot!(1)
                Locate 5, 11: Print " "

            Case 4:
                Locate 6, 11: Print Space$(10)
                Locate 6, 1: Input " 4. TM FGM"; statsOffTot!(2)
                Locate 6, 11: Print " "

            Case 5:
                Locate 7, 12: Print Space$(10)
                Locate 7, 1: Input " 5. TM 3FGA"; statsOffTot!(14)
                Locate 7, 12: Print " "

            Case 6:
                Locate 8, 12: Print Space$(10)
                Locate 8, 1: Input " 6. TM 3FGM"; statsOffTot!(13)
                Locate 8, 12: Print " "

            Case 7:
                Locate 9, 11: Print Space$(10)
                Locate 9, 1: Input " 7. TM FTA"; statsOffTot!(3)
                Locate 9, 11: Print " "

            Case 8:
                Locate 10, 11: Print Space$(10)
                Locate 10, 1: Input " 8. TM FTM"; statsOffTot!(4)
                Locate 10, 11: Print " "

            Case 9:
                Locate 11, 14: Print Space$(10)
                Locate 11, 1: Input " 9. TM OFF RB"; statsOffTot!(12)
                Locate 11, 14: Print " "

            Case 10:
                Locate 12, 14: Print Space$(10)
                Locate 12, 1: Input "10. TM TOT RB"; statsOffTot!(5)
                Locate 12, 14: Print " "

            Case 11:
                Locate 13, 13: Print Space$(10)
                Locate 13, 1: Input "11. TM FOULS"; statsOffTot!(6)
                Locate 13, 13: Print " "

            Case 12:
                Locate 14, 14: Print Space$(10)
                Locate 14, 1: Input "12. TM STEALS"; statsOffTot!(7)
                Locate 14, 14: Print " "

            Case 13:
                Locate 15, 15: Print Space$(10)
                Locate 15, 1: Input "13. TM ASSISTS"; statsOffTot!(8)
                Locate 15, 15: Print " "

            Case 14:
                Locate 16, 14: Print Space$(10)
                Locate 16, 1: Input "14. TM BLOCKS"; statsOffTot!(9)
                Locate 16, 14: Print " "

            Case 15:
                Locate 17, 17: Print Space$(10)
                Locate 17, 1: Input "15. TM TURNOVERS"; statsOffTot!(11)
                Locate 17, 17: Print " "

            Case 16:
                Locate 18, 14: Print Space$(10)
                Locate 18, 1: Input "16. TM POINTS"; statsOffTot!(10)
                Locate 18, 14: Print " "

        End Select

    Loop Until B9 = 0

    '---------------
    ' Defense Stats
    '---------------
    Cls
    X$ = "DEFENSIVE TEAM STATS"
    Call TITLE_EDITSTATS(2, X$)

    Locate 3, 1
    Print " 1. OPP FGA "; statsDefTot!(1)
    Print " 2. OPP FGM "; statsDefTot!(2)
    Print " 3. OPP 3FGA "; statsDefTot!(14)
    Print " 4. OPP 3FGM "; statsDefTot!(13)
    Print " 5. OPP FTA "; statsDefTot!(3)
    Print " 6. OPP FTM "; statsDefTot!(4)
    Print " 7. OPP OFF RB "; statsDefTot!(12)
    Print " 8. OPP TOT RB "; statsDefTot!(5)
    Print " 9. OPP FOULS "; statsDefTot!(6)
    Print "10. OPP STEALS "; statsDefTot!(7)
    Print "11. OPP ASSISTS "; statsDefTot!(8)
    Print "12. OPP BLOCKS "; statsDefTot!(9)
    Print "13. OPP TURNOVERS "; statsDefTot!(11)
    Print "14. OPP POINTS "; statsDefTot!(10)

    Do

        Do
            Locate 22, 29
            Print Space$(10)
            Locate 22, 1
            Input "# TO CHANGE (0 TO CONTINUE)"; B9
        Loop Until B9 >= 0 And B9 <= 14

        Select Case B9

            Case 1
                Locate 3, 12: Print Space$(10)
                Locate 3, 1: Input " 1. OPP FGA"; statsDefTot!(1)
                Locate 3, 12: Print " "

            Case 2
                Locate 4, 12: Print Space$(10)
                Locate 4, 1: Input " 2. OPP FGM"; statsDefTot!(2)
                Locate 4, 12: Print " "

            Case 3
                Locate 5, 13: Print Space$(10)
                Locate 5, 1: Input " 3. OPP 3FGA"; statsDefTot!(14)
                Locate 5, 13: Print " "

            Case 4
                Locate 6, 13: Print Space$(10)
                Locate 6, 1: Input " 4. OPP 3FGM"; statsDefTot!(13)
                Locate 6, 13: Print " "

            Case 5
                Locate 7, 12: Print Space$(10)
                Locate 7, 1: Input " 5. OPP FTA"; statsDefTot!(3)
                Locate 7, 12: Print " "

            Case 6
                Locate 8, 12: Print Space$(10)
                Locate 8, 1: Input " 6. OPP FTM"; statsDefTot!(4)
                Locate 8, 12: Print " "

            Case 7
                Locate 9, 15: Print Space$(10)
                Locate 9, 1: Input " 7. OPP OFF RB"; statsDefTot!(12)
                Locate 9, 15: Print " "

            Case 8
                Locate 10, 15: Print Space$(10)
                Locate 10, 1: Input " 8. OPP TOT RB"; statsDefTot!(5)
                Locate 10, 15: Print " "

            Case 9
                Locate 11, 14: Print Space$(10)
                Locate 11, 1: Input " 9. OPP FOULS"; statsDefTot!(6)
                Locate 11, 14: Print " "

            Case 10
                Locate 12, 15: Print Space$(10)
                Locate 12, 1: Input "10. OPP STEALS"; statsDefTot!(7)
                Locate 12, 15: Print " "

            Case 11
                Locate 13, 16: Print Space$(10)
                Locate 13, 1: Input "11. OPP ASSISTS"; statsDefTot!(8)
                Locate 13, 16: Print " "

            Case 12
                Locate 14, 15: Print Space$(10)
                Locate 14, 1: Input "12. OPP BLOCKS"; statsDefTot!(9)
                Locate 14, 15: Print " "

            Case 13
                Locate 15, 18: Print Space$(10)
                Locate 15, 1: Input "13. OPP TURNOVERS"; statsDefTot!(11)
                Locate 15, 18: Print " "

            Case 14
                Locate 16, 15: Print Space$(10)
                Locate 16, 1: Input "14. OPP POINTS"; statsDefTot!(10)
                Locate 16, 15: Print " "

        End Select

    Loop Until B9 = 0


    '---------------
    ' Player Stats
    '---------------
    Do

        Cls
        X$ = "CHANGE PLAYERS"
        Call TITLE_EDITSTATS(2, X$)

        Locate 3, 1
        For I = 0 To 13
            Color 7
            If (I Mod 2) = 0 Then Color 15
            Print (I + 1);
            Print " "; statPlyrNames$(I)
        Next I

        
        Do
            Locate 19, 1
            Color 15
            Input "# TO CHANGE (1-14) (0 TO CONTINUE)"; I
        Loop Until I >= 0 And I <= 14

        If I > 0 Then

            I = I - 1

            Cls
            Call TITLE_EDITSTATS(2, X$)

            Color 12
            Locate 3, 1: Print "PLAYER - "; Str$(I + 1)
            Color 15
            Print " 1. NAME "; statPlyrNames$(I)
            Print " 2. GAMES "; statPlyrVal!(I, 0)
            Print " 3. MINUTES "; CInt(statPlyrVal!(I, 10) / 60)
            Print " 4. FGA "; statPlyrVal!(I, 1)
            Print " 5. FGM "; statPlyrVal!(I, 2); ""
            Print " 6. FTA "; statPlyrVal!(I, 3)
            Print " 7. FTM "; statPlyrVal!(I, 4)
            Print " 8. OFF REB "; statPlyrVal!(I, 12)
            Print " 9. TOT REB "; statPlyrVal!(I, 5)
            Print "10. FOULS "; statPlyrVal!(I, 6)
            Print "11. STEALS "; statPlyrVal!(I, 7)
            Print "12. ASSISTS "; statPlyrVal!(I, 8)
            Print "13. BLOCKS "; statPlyrVal!(I, 9)
            Print "14. TURNOVERS "; statPlyrVal!(I, 11)
            Print "15. 3FGA "; stats3FGA!(I)
            Print "16. 3FGM "; stats3FGM!(I)
            Print "17. HI-GAME (PTS) "; statPlyrVal!(I, 13)

            Do

                Do
                    Locate 22, 29
                    Print Space$(10)
                    Locate 22, 1
                    Input "# TO CHANGE (0 TO CONTINUE) "; B9
                Loop Until B9 >= 0 And B9 <= 17

                Select Case B9

                    Case 1
                        Locate 4, 1: Input " 1. NAME"; statPlyrNames$(I)
                        Locate 4, 9: Print " "

                    Case 2
                        Locate 5, 10: Print Space$(10)
                        Locate 5, 1: Input " 2. GAMES"; statPlyrVal!(I, 0)
                        Locate 5, 10: Print " "

                    Case 3
                        Locate 6, 12: Print Space$(10)
                        Locate 6, 1: Input " 3. MINUTES"; statPlyrVal!(I, 10)
                        Locate 6, 12: Print " "
                        statPlyrVal!(I, 10) = statPlyrVal!(I, 10) * 60

                    Case 4
                        Locate 7, 8: Print Space$(10)
                        Locate 7, 1: Input " 4. FGA"; statPlyrVal!(I, 1)
                        Locate 7, 8: Print " "

                    Case 5
                        Locate 8, 8: Print Space$(10)
                        Locate 8, 1: Input " 5. FGM"; statPlyrVal!(I, 2)
                        Locate 8, 8: Print " "

                    Case 6
                        Locate 9, 8: Print Space$(10)
                        Locate 9, 1: Input " 6. FTA"; statPlyrVal!(I, 3)
                        Locate 9, 8: Print " "

                    Case 7
                        Locate 10, 8: Print Space$(10)
                        Locate 10, 1: Input " 7. FTM"; statPlyrVal!(I, 4)
                        Locate 10, 8: Print " "

                    Case 8
                        Locate 11, 12: Print Space$(10)
                        Locate 11, 1: Input " 8. OFF REB"; statPlyrVal!(I, 12)
                        Locate 11, 12: Print " "

                    Case 9
                        Locate 12, 12: Print Space$(10)
                        Locate 12, 1: Input " 9. TOT REB"; statPlyrVal!(I, 5)
                        Locate 12, 12: Print " "

                    Case 10
                        Locate 13, 10: Print Space$(10)
                        Locate 13, 1: Input "10. FOULS"; statPlyrVal!(I, 6)
                        Locate 13, 10: Print " "

                    Case 11
                        Locate 14, 11: Print Space$(10)
                        Locate 14, 1: Input "11. STEALS"; statPlyrVal!(I, 7)
                        Locate 14, 11: Print " "

                    Case 12
                        Locate 15, 12: Print Space$(10)
                        Locate 15, 1: Input "12. ASSISTS"; statPlyrVal!(I, 8)
                        Locate 15, 12: Print " "

                    Case 13
                        Locate 16, 11: Print Space$(10)
                        Locate 16, 1: Input "13. BLOCKS"; statPlyrVal!(I, 9)
                        Locate 16, 11: Print " "

                    Case 14
                        Locate 17, 14: Print Space$(10)
                        Locate 17, 1: Input "14. TURNOVERS"; statPlyrVal!(I, 11)
                        Locate 17, 14: Print " "

                    Case 15
                        Locate 18, 9: Print Space$(10)
                        Locate 18, 1: Input "15. 3FGA"; stats3FGA!(I)
                        Locate 18, 9: Print " "

                    Case 16
                        Locate 19, 9: Print Space$(10)
                        Locate 19, 1: Input "16. 3FGM"; stats3FGM!(I)
                        Locate 19, 9: Print " "

                    Case 17
                        Locate 20, 18: Print Space$(10)
                        Locate 20, 1: Input "17. HI-GAME (PTS)"; statPlyrVal!(I, 13)
                        Locate 20, 18: Print " "

                End Select

            Loop Until B9 = 0

        End If

    Loop Until I = 0


    '---------------
    ' Save Edits
    '---------------
    Open diskPaths$(1) + statFile$ For Output As #2

    Print #2, teamName$

    For I = 0 To 14:
        Print #2, statPlyrNames$(I)

        For J = 0 To 13:
            Print #2, statPlyrVal!(I, J)
        Next J

        Print #2, stats3FGM!(I)

        Print #2, stats3FGA!(I)

    Next I

    For L = 0 To 15:
        Print #2, statsOffTot!(L)
    Next L

    For L = 0 To 15:
        Print #2, statsDefTot!(L)
    Next L

    Close #2

End Sub


'----------------------------------------
'       ChangeScheduleStats Subroutine
'----------------------------------------
'This subroutine inputs stats from the
'chosen Stat Schedule file and then gives
'the user an interface for modifying the
'schedule results.
Sub ChangeScheduleStats (teamName$)

    Shared gameSite$()
    Shared locIndicator$(), oppName$()

    Shared oppScore(), teamScore()

    Shared gameAttendance&()

    If Not _FileExists(diskPaths$(1) + teamName$ + "S") Then

        MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        K$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Call ReadScheduleStats(teamName$, nbrGames)

        Do

            Color 15, 0
            Cls

            X$ = "TEAM SCHEDULE STATS"
            Call TITLE_EDITSTATS(2, X$)

            homeScore = 0
            oppTeam$ = ""
            oppPts = 0
            homeOrAway$ = ""
            attendance& = 0
            site$ = ""

            Do

                Locate 2, 1: Print "1. SCORE FOR "; teamName$; "  "; homeScore
                Locate 3, 1: Print "2. OPPONENT'S NAME  "; oppTeam$
                Locate 4, 1: Print "3. OPPONENT'S SCORE  "; oppPts
                Locate 5, 1: Print "4. (H)OME, (V)ISITOR OR (N)EUTRAL  "; homeOrAway$
                Locate 6, 1: Print "5. ATTENDANCE  "; attendance&
                Locate 7, 1: Print "6. GAME SITE  "; site$

                Do
                    Locate 9, 1: Print "ENTER # TO CHANGE (1-4) OR 0 TO CONTINUE"
                    J$ = GetKeyPress$
                    D = Val(J$)
                Loop Until D >= 0 And D <= 6

                Select Case D

                    Case 1:
                        Locate 2, 1: Print "1. SCORE FOR "; teamName$; " ";
                        Input homeScore
                        Locate 2, 13 + Len(teamName$): Print " "

                    Case 2:
                        Locate 3, 1: Input "2. OPPONENT'S NAME"; oppTeam$
                        Locate 3, 20: Print " "

                    Case 3:
                        Locate 4, 20: Print Space$(10)
                        Locate 4, 1: Input "3. OPPONENT'S SCORE"; oppPts
                        Locate 4, 20: Print " "

                    Case 4:
                        Do
                            Locate 5, 1: Input "4. (H)OME, (V)ISITOR OR (N)EUTRAL"; homeOrAway$
                            Locate 5, 36: Print " "
                        Loop Until homeOrAway$ = "H" Or homeOrAway$ = "V" Or homeOrAway$ = "N"

                    Case 5:
                        Locate 6, 14: Print Space$(10)
                        Locate 6, 1: Input "5. ATTENDANCE"; attendance&
                        Locate 6, 14: Print " "

                    Case 6:
                        Locate 7, 1: Input "6. GAME SITE"; site$
                        Locate 7, 13: Print " "

                End Select

            Loop Until D = 0

            nbrGames = nbrGames + 1
            teamScore(nbrGames) = homeScore
            oppName$(nbrGames) = oppTeam$
            oppScore(nbrGames) = oppPts
            locIndicator$(nbrGames) = homeOrAway$
            gameAttendance&(nbrGames) = attendance&
            gameSite$(nbrGames) = site$

            Do
                Locate 11, 1
                Print "ADD ANOTHER GAME? (Y/N)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        Loop Until UCase$(I$) = "N"

        Call SaveScheduleStats(teamName$, nbrGames)

        Cls

    End If 'Done checking if file exists

End Sub
