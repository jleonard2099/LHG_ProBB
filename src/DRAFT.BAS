'----------------------------------------
'            DRAFT Subroutine
'----------------------------------------
'This subroutine presents a menu to the
'user with choices for drafting players.
Sub DRAFT ()

    _Title "Basketball: The Pro Game - Team Draft"

    Shared leagTot_DRAFT(), leagRat_DRAFT(), plyrRat_DRAFT(), teamRat_DRAFT()

    Shared draftYears$(), plyrNames_DRAFT$(), tmYears_DRAFT$(), tmInfo_DRAFT$()

    Dim tmTotal_DRAFT(34)

    ReDim menuChoices$(1 To 3)

    Do

        SELECTION% = 1
        Count% = 79
        Start% = 1
        FileFGColor = 15: FileBGColor = 4
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Rows% = 16
        Columns% = 5
        Spacing% = 8
        Row% = 5
        Column% = 1
        Color , 4
        Cls
        Locate , 16
        Color 14: Print "SELECT YEAR TO NORMALIZE DRAFTED PLAYERS TO"
        Color 15

        Do
            _MouseShow "DEFAULT"
            MAMenu draftYears$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide
        Loop Until ExitCode% = 27 Or ExitCode% = 13

        If ExitCode% <> 27 Then

            Do
                Locate 3, 33
                Color 15, 0
                Print draftYears$(SELECTION%)

                Color 15, 4

                Locate , 30: Print "ANY CHANGE? (Y/N)";

                I$ = GetKeyPress$

            Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

            yearIdx = SELECTION%

        End If

        If ExitCode% = 27 Then Exit Sub

    Loop Until UCase$(I$) = "N" Or ExitCode% = 27

    If ExitCode% <> 27 Then

        Do

            Call TITLE

            menuChoices$(1) = "DRAFT A NEW TEAM         "
            menuChoices$(2) = "DRAFT TO AN EXISTING TEAM"
            menuChoices$(3) = "EXIT TO MAIN MENU        "
            SELECTION% = 1
            Count% = 3
            Start% = 1
            ExitCode% = 3
            FileFGColor = 15: FileBGColor = 1
            HiLiteFGColor% = 0: HiLiteBGColor% = 7
            Spacing% = 1
            Row% = 8
            Column% = 25
            menuWidth% = Len(menuChoices$(1))
            Column% = (80 - menuWidth% - 2) / 2

            Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

            _MouseShow "DEFAULT"
            MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
            _MouseHide

            Color , 0

            If ExitCode% <> 27 Then

                Select Case SELECTION%

                    Case 1

                        For I = 0 To 1:
                            tmInfo_DRAFT$(0, I) = "XXX"
                        Next

                        For I = 0 To 6:
                            leagRat_DRAFT(0, I) = 0
                        Next

                        For I = 0 To 34
                            teamRat_DRAFT(0, I) = 0
                        Next

                        Erase leagTot_DRAFT, tmTotal_DRAFT

                        For I = 0 To 14
                            plyrNames_DRAFT$(0, I, 0) = "XXX"
                            plyrNames_DRAFT$(0, I, 1) = "X"
                            For I1 = 0 To 19
                                plyrRat_DRAFT(0, I, I1) = 0
                            Next
                        Next

                        Call NewTeamParameters_Base(tmYears_DRAFT$(0), tmInfo_DRAFT$(0, 0), yearIdx)
                        Call DraftChoices(1, doneDraft, teamIdx, tmTotal_DRAFT(), yearIdx)

                    Case 2
                        draftIdx = 0
                        Call ChooseDraftTeam(tmYears_DRAFT$(draftIdx), NTMS, teamIdx, tmInfo_DRAFT$(draftIdx, 0), draftIdx)
                        If teamIdx > 0 Then Call DraftChoices(0, doneDraft, teamIdx, tmTotal_DRAFT(), yearIdx)

                End Select

                If doneDraft = 1 Then SELECTION% = 3

            End If

        Loop Until SELECTION% = 3 Or ExitCode% = 27

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'   NewTeamParameters_Base Subroutine
'----------------------------------------
'This subroutine prompts the user for the
'minimum required parameters to create a
'new team that players will be drafted to.
Sub NewTeamParameters_Base (teamYear$, teamName$, normYrIdx)

    Shared tmInfo_DRAFT$()
    Shared leagRat_DRAFT(), leagueAverages%(), teamRat_DRAFT()

    Color 15, 0
    Cls

    Color 14
    Print "TEAM INFORMATION"
    Color 15

    'Assign stats from year chosen to 'normalize' to
    '# of Games
    leagRat_DRAFT(0, 0) = 82
    'AVG FGA/G
    leagRat_DRAFT(0, 1) = leagueAverages%(normYrIdx, 1)
    'AVG RB/G
    leagRat_DRAFT(0, 2) = leagueAverages%(normYrIdx, 2)
    'LEAGUE FG%
    leagRat_DRAFT(0, 3) = leagueAverages%(normYrIdx, 3)
    'AVG PF/G
    leagRat_DRAFT(0, 4) = leagueAverages%(normYrIdx, 4)
    'LEAGUE 3FG%
    leagRat_DRAFT(0, 5) = leagueAverages%(normYrIdx, 5)
    'LEAGUE POWER ADJ
    leagRat_DRAFT(0, 6) = 0

    Locate 3: Input "1. DISK ID "; teamYear$
    Locate 4: Input "2. TEAM NAME "; teamName$
    Locate 5: Input "3. TEAM NICKNAME "; tmInfo_DRAFT$(0, 1)
    Locate 6: Input "4. TEAM ABBREV (IE. BOS) "; tmInfo_DRAFT$(0, 2)
    Locate 7: Input "5. COACH'S NAME "; tmInfo_DRAFT$(0, 3)
    Locate 8: Input "6. ARENA NAME "; tmInfo_DRAFT$(0, 4)
    Locate 9: Input "7. TEXT COLOR (0-15) "; teamRat_DRAFT(0, 33)
    Locate 10: Input "8. BACKGROUND COLOR (0-7) "; teamRat_DRAFT(0, 34)

    If teamRat_DRAFT(0, 33) >= 0 And teamRat_DRAFT(0, 34) >= 0 Then
        Locate 12
        Print "TEAM APPEARANCE"
        Color teamRat_DRAFT(0, 33), teamRat_DRAFT(0, 34)
        Print RTrim$(teamName$) + " " + RTrim$(tmInfo_DRAFT$(0, 1))
        Color 15, 0
    End If

    Do

        Do
            Locate 20, 25
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 8

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 3, 1: Input "1. DISK ID "; teamYear$
                    Locate 3, 12: Print " "

                Case 2
                    Locate 4, 1: Input "2. TEAM NAME "; teamName$
                    Locate 4, 14: Print " "

                Case 3
                    Locate 5, 1: Input "3. TEAM NICKNAME "; tmInfo_DRAFT$(0, 1)
                    Locate 5, 18: Print " "

                Case 4
                    Locate 6, 1: Input "4. TEAM ABBREV (IE. BOS) "; tmInfo_DRAFT$(0, 2)
                    Locate 6, 26: Print " "

                Case 5
                    Locate 7, 1: Input "5. COACH'S NAME "; tmInfo_DRAFT$(0, 3)
                    Locate 7, 17: Print " "

                Case 6
                    Locate 8, 1: Input "6. ARENA NAME "; tmInfo_DRAFT$(0, 4)
                    Locate 8, 15: Print " "

                Case 7
                    Locate 9, 22: Print Space$(10)
                    Locate 9, 1: Input "7. TEXT COLOR (0-15) "; teamRat_DRAFT(0, 33)
                    Locate 9, 22: Print " "

                    If teamRat_DRAFT(0, 33) >= 0 And teamRat_DRAFT(0, 34) >= 0 Then
                        Locate 12, 1
                        Print "TEAM APPEARANCE"
                        Color teamRat_DRAFT(0, 33), teamRat_DRAFT(0, 34)
                        Print RTrim$(teamName$) + " " + RTrim$(tmInfo_DRAFT$(0, 1))
                        Color 15, 0
                    End If

                Case 8
                    Locate 10, 27: Print Space$(10)
                    Locate 10, 1: Input "8. BACKGROUND COLOR (0-7) "; teamRat_DRAFT(0, 34)
                    Locate 10, 27: Print " "

                    If teamRat_DRAFT(0, 33) >= 0 And teamRat_DRAFT(0, 34) >= 0 Then
                        Locate 12, 1
                        Print "TEAM APPEARANCE"
                        Color teamRat_DRAFT(0, 33), teamRat_DRAFT(0, 34)
                        Print RTrim$(teamName$) + " " + RTrim$(tmInfo_DRAFT$(0, 1))
                        Color 15, 0
                    End If

            End Select

            Locate 20, 53
            Print Space$(10)

        End If

    Loop Until B9 = 0
    
    'prompts
    Color 15, 0
    Cls

    Do
        Locate , 5
        Print "WOULD YOU LIKE TO ENTER STATS (ACTUALS) FOR THIS TEAM? (Y/N)"
        K$ = GetKeyPress$
        response$ = UCase$(K$)
    Loop Until response$ = "Y" Or response$ = "N" Or K$ = Chr$(27)
    
    If response$ = "Y" Then
        Call NewTeamParameters_OffDef
    End If

End Sub


'----------------------------------------
'   NewTeamParameters_OffDef SubRoutine
'----------------------------------------
' There original code does not access this,
' nor does the original game. However, it
' was clearly written to help finish the
' new team creation process by allowing
' users to complete input of all ratings.
Sub NewTeamParameters_OffDef ()

    Shared tmInfo_DRAFT$()
    Shared leagRat_DRAFT(), leagueAverages%(), teamRat_DRAFT()

    Color 15, 0
    Cls

    Color 14
    Print "TEAM OFFENSIVE INFORMATION"
    Color 15

    Locate 3: Input " 1. FGM "; teamRat_DRAFT(0, 6)
    Locate 4: Input " 2. FGA "; teamRat_DRAFT(0, 5)
    Locate 5: Input " 3. FTM "; teamRat_DRAFT(0, 8)
    Locate 6: Input " 4. FTA "; teamRat_DRAFT(0, 7)
    Locate 7: Input " 5. OFF REB "; teamRat_DRAFT(0, 9)
    Locate 8: Input " 6. DEF REB "; teamRat_DRAFT(0, 10)

    If teamRat_DRAFT(0, 9) = 0 And RB = 0 Then
        teamRat_DRAFT(0, 9) = .32 * teamRat_DRAFT(0, 10): teamRat_DRAFT(0, 10) = teamRat_DRAFT(0, 10) - teamRat_DRAFT(0, 9)
        Locate 7: Print " 5. OFF REB "; teamRat_DRAFT(0, 9)
        Locate 8: Print " 6. DEF REB "; teamRat_DRAFT(0, 10)
    End If

    Locate 9: Input " 7. ASSISTS "; teamRat_DRAFT(0, 11)
    Locate 10: Input " 8. FOULS "; teamRat_DRAFT(0, 12)
    Locate 11: Input " 9. DISQ "; teamRat_DRAFT(0, 13)
    Locate 12: Input "10. STEALS "; teamRat_DRAFT(0, 14)
    Locate 13: Input "11. TURNOVERS "; teamRat_DRAFT(0, 15)
    Locate 14: Input "12. BLK SHOTS "; teamRat_DRAFT(0, 16)
    Locate 15: Input "13. 3-FGM "; teamRat_DRAFT(0, 18)
    Locate 16: Input "14. 3-FGA "; teamRat_DRAFT(0, 17)

    Do

        Do
            Locate 18
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 14

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 3, 8: Print Space$(10)
                    Locate 3, 1: Input " 1. FGM"; teamRat_DRAFT(0, 6)
                    Locate 3, 8: Print " "

                Case 2
                    Locate 4, 8: Print Space$(10)
                    Locate 4, 1: Input " 2. FGA"; teamRat_DRAFT(0, 5)
                    Locate 4, 8: Print " "

                Case 3
                    Locate 5, 8: Print Space$(10)
                    Locate 5, 1: Input " 3. FTM"; teamRat_DRAFT(0, 8)
                    Locate 5, 8: Print " "

                Case 4
                    Locate 6, 8: Print Space$(10)
                    Locate 6, 1: Input " 4. FTA"; teamRat_DRAFT(0, 7)
                    Locate 6, 8: Print " "

                Case 5
                    Locate 7, 12: Print Space$(10)
                    Locate 7, 1: Input " 5. OFF REB"; teamRat_DRAFT(0, 9)
                    Locate 7, 12: Print " "

                Case 6
                    Locate 8, 12: Print Space$(10)
                    Locate 8, 1: Input " 6. DEF REB"; teamRat_DRAFT(0, 10)
                    Locate 8, 12: Print " "

                    'Calculate Rebounds if missing
                    If teamRat_DRAFT(0, 9) = 0 And RB = 0 Then
                        'Off Rebounds = ~ Def Rebounds / 3
                        teamRat_DRAFT(0, 9) = .32 * teamRat_DRAFT(0, 10)
                        teamRat_DRAFT(0, 10) = teamRat_DRAFT(0, 10) - teamRat_DRAFT(0, 9)
                        Locate 7, 1: Print " 5. OFF REB "; teamRat_DRAFT(0, 9)
                        Locate 8, 1: Print " 6. DEF REB "; teamRat_DRAFT(0, 10)
                    End If

                Case 7
                    Locate 9, 12: Print Space$(10)
                    Locate 9, 1: Input " 7. ASSISTS"; teamRat_DRAFT(0, 11)
                    Locate 9, 12: Print " "

                Case 8
                    Locate 10, 10: Print Space$(10)
                    Locate 10, 1: Input " 8. FOULS"; teamRat_DRAFT(0, 12)
                    Locate 10, 10: Print " "

                Case 9
                    Locate 11, 9: Print Space$(10)
                    Locate 11, 1: Input " 9. DISQ"; teamRat_DRAFT(0, 13)
                    Locate 11, 9: Print " "

                Case 10
                    Locate 12, 11: Print Space$(10)
                    Locate 12, 1: Input "10. STEALS"; teamRat_DRAFT(0, 14)
                    Locate 12, 11: Print " "

                Case 11
                    Locate 13, 14: Print Space$(10)
                    Locate 13, 1: Input "11. TURNOVERS"; teamRat_DRAFT(0, 15)
                    Locate 13, 14: Print " "

                Case 12
                    Locate 14, 14: Print Space$(10)
                    Locate 14, 1: Input "12. BLK SHOTS"; teamRat_DRAFT(0, 16)
                    Locate 14, 14: Print " "

                Case 13
                    Locate 15, 10: Print Space$(10)
                    Locate 15, 1: Input "13. 3-FGM"; teamRat_DRAFT(0, 18)
                    Locate 15, 10: Print " "

                Case 14
                    Locate 16, 10: Print Space$(10)
                    Locate 16, 1: Input "14. 3-FGA"; teamRat_DRAFT(0, 17)
                    Locate 16, 10: Print " "

            End Select

            Locate 18, 29
            Print Space$(10)
            
        End If

    Loop Until B9 = 0

    '-----

    Cls

    Color 14
    Print "TEAM DEFENSIVE INFORMATION"
    Color 15

    Locate 3: Input " 1. FGM "; teamRat_DRAFT(0, 20)
    Locate 4: Input " 2. FGA "; teamRat_DRAFT(0, 19)
    Locate 5: Input " 3. FTM "; teamRat_DRAFT(0, 22)
    Locate 6: Input " 4. FTA "; teamRat_DRAFT(0, 21)
    Locate 7: Input " 5. OFF REB "; teamRat_DRAFT(0, 23)
    Locate 8: Input " 6. DEF REB "; teamRat_DRAFT(0, 24)

    If teamRat_DRAFT(0, 23) = 0 And RB = 0 Then
        teamRat_DRAFT(0, 23) = .35 * teamRat_DRAFT(0, 24)
        teamRat_DRAFT(0, 24) = teamRat_DRAFT(0, 24) - teamRat_DRAFT(0, 23)
        Locate 7: Print " 5. OFF REB "; teamRat_DRAFT(0, 23)
        Locate 8: Print " 6. DEF REB "; teamRat_DRAFT(0, 24)
    End If

    Locate 9: Input " 7. ASSISTS "; teamRat_DRAFT(0, 25)
    Locate 10: Input " 8. FOULS "; teamRat_DRAFT(0, 26)
    Locate 11: Input " 9. DISQ "; teamRat_DRAFT(0, 27)
    Locate 12: Input "10. STEALS "; teamRat_DRAFT(0, 28)
    Locate 13: Input "11. TURNOVERS "; teamRat_DRAFT(0, 29)
    Locate 14: Input "12. BLK SHOTS "; teamRat_DRAFT(0, 30)
    Locate 15: Input "13. 3-FGM "; teamRat_DRAFT(0, 32)
    Locate 16: Input "14. 3-FGA "; teamRat_DRAFT(0, 31)

    Do

        Do
            Locate 18
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 14

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 3, 8: Print Space$(10)
                    Locate 3, 1: Input " 1. FGM"; teamRat_DRAFT(0, 20)
                    Locate 3, 8: Print " "

                Case 2
                    Locate 4, 8: Print Space$(10)
                    Locate 4, 1: Input " 2. FGA"; teamRat_DRAFT(0, 19)
                    Locate 4, 8: Print " "

                Case 3
                    Locate 5, 8: Print Space$(10)
                    Locate 5, 1: Input " 3. FTM"; teamRat_DRAFT(0, 22)
                    Locate 5, 8: Print " "

                Case 4
                    Locate 6, 8: Print Space$(10)
                    Locate 6, 1: Input " 4. FTA"; teamRat_DRAFT(0, 21)
                    Locate 6, 8: Print " "

                Case 5
                    Locate 7, 12: Print Space$(10)
                    Locate 7, 1: Input " 5. OFF REB"; teamRat_DRAFT(0, 23)
                    Locate 7, 12: Print " "

                Case 6
                    Locate 8, 12: Print Space$(10)
                    Locate 8, 1: Input " 6. DEF REB"; teamRat_DRAFT(0, 24)
                    Locate 8, 12: Print " "

                    'Calculate Rebounds if missing
                    If teamRat_DRAFT(0, 23) = 0 And RB = 0 Then
                        'Off Rebounds = ~ Def Rebounds / 3
                        teamRat_DRAFT(0, 23) = .35 * teamRat_DRAFT(0, 24)
                        teamRat_DRAFT(0, 24) = teamRat_DRAFT(0, 24) - teamRat_DRAFT(0, 23)
                        Locate 7: Print " 5. OFF REB "; teamRat_DRAFT(0, 23)
                        Locate 8: Print " 6. DEF REB "; teamRat_DRAFT(0, 24)
                    End If

                Case 7
                    Locate 9, 12: Print Space$(10)
                    Locate 9, 1: Input " 7. ASSISTS"; teamRat_DRAFT(0, 25)
                    Locate 9, 12: Print " "

                Case 8
                    Locate 10, 10: Print Space$(10)
                    Locate 10, 1: Input " 8. FOULS"; teamRat_DRAFT(0, 26)
                    Locate 11, 10: Print " "

                Case 9
                    Locate 11, 9: Print Space$(10)
                    Locate 11, 1: Input " 9. DISQ"; teamRat_DRAFT(0, 27)
                    Locate 11, 9: Print " "

                Case 10
                    Locate 12, 11: Print Space$(10)
                    Locate 12, 1: Input "10. STEALS"; teamRat_DRAFT(0, 28)
                    Locate 12, 11: Print " "

                Case 11
                    Locate 13, 14: Print Space$(10)
                    Locate 13, 1: Input "11. TURNOVERS"; teamRat_DRAFT(0, 29)
                    Locate 13, 14: Print " "

                Case 12
                    Locate 14, 14: Print Space$(10)
                    Locate 14, 1: Input "12. BLK SHOTS"; teamRat_DRAFT(0, 30)
                    Locate 14, 14: Print " "

                Case 13
                    Locate 15, 10: Print Space$(10)
                    Locate 15, 1: Input "13. 3-FGM"; teamRat_DRAFT(0, 32)
                    Locate 15, 10: Print " "

                Case 14
                    Locate 16, 10: Print Space$(10)
                    Locate 16, 1: Input "14. 3-FGA"; teamRat_DRAFT(0, 31)
                    Locate 16, 10: Print " "

            End Select

            Locate 18, 29
            Print Space$(10)

        End If

    Loop Until B9 = 0

    'This is ripped from the Team Input routine
    'It assumes we don't have data; not that these
    'values would ever be 0 intentionally
    If teamRat_DRAFT(0, 28) = 0 Then
        teamRat_DRAFT(0, 28) = teamRat_DRAFT(0, 5) * .1
    End If
    If teamRat_DRAFT(0, 14) = 0 Then
        teamRat_DRAFT(0, 14) = teamRat_DRAFT(0, 19) * .1
    End If
    If teamRat_DRAFT(0, 16) = 0 Then
        teamRat_DRAFT(0, 16) = teamRat_DRAFT(0, 19) * .06
    End If
    If teamRat_DRAFT(0, 30) = 0 Then
        teamRat_DRAFT(0, 30) = teamRat_DRAFT(0, 5) * .06
    End If
    If teamRat_DRAFT(0, 29) = 0 Then
        teamRat_DRAFT(0, 29) = 18 * leagRat_DRAFT(0, 0)
        teamRat_DRAFT(0, 15) = teamRat_DRAFT(0, 29)
    End If


End Sub


'----------------------------------------
'       DraftChoices SubRoutine
'----------------------------------------
'This subroutine provides an interface the
'user can operate to choose their draft
'selections.
'
'draftNew
'  0 = Existing Team
'  1 = New Team
Sub DraftChoices (draftNew, doneDrafting, teamIdx, tmTotal_DRAFT(), normYrIdx)

    Dim nbrPlayers

    Shared tmInfo_DRAFT$(), plyrNames_DRAFT$(), tmYears_DRAFT$()
    Shared plyrRat_DRAFT(), leagRat_DRAFT(), leagueAverages%(), leagTot_DRAFT(), teamRat_DRAFT()

    Do

        Color 0, 0
        Cls
        Color teamRat_DRAFT(0, 33), teamRat_DRAFT(0, 34): Print tmInfo_DRAFT$(0, 0);

        Color 14, 0
        Print "     (DRAFTING)"
        Print
        Print "PLAYERS"

        For I = 0 To 14
            Color 2: Print Using "## "; I;
            Color 15: Print plyrNames_DRAFT$(0, I, 0); " "; plyrNames_DRAFT$(0, I, 1)
        Next

        Do
            Locate 21, 1
            Color 14: Print "(1) ";
            Color 15: Print "DRAFT PLAYER"
            Color 14: Print "(2) ";
            Color 15: Print "SAVE TEAM"

            I$ = GetKeyPress$
            I0 = Val(I$)
        Loop Until I0 >= 1 And I0 <= 2

        Select Case I0

            Case 1
                'Drafting
                Do
                    Locate 21, 20: Print "SLOT # (0-14) ";
                    Input B
                Loop Until B >= 0 And B <= 14

                draftIdx = 1
                Call ChooseDraftTeam(tmYears_DRAFT$(draftIdx), NTMS, destIdx, tmInfo_DRAFT$(draftIdx, 0), draftIdx)

                Color , 0
                Cls
                Color teamRat_DRAFT(1, 33), teamRat_DRAFT(1, 34): Print tmInfo_DRAFT$(draftIdx, 0)
                Color 14, 0

                Print
                Print "PLAYERS"

                For I = 0 To 14
                    Color 2: Print Using "## "; I;
                    Color 15: Print plyrNames_DRAFT$(draftIdx, I, 0); " "; plyrNames_DRAFT$(draftIdx, I, 1)
                Next

                Do
                    Color 14
                    Locate 21, 1: Print "SELECT PLAYER # TO DRAFT ";
                    Input B1
                Loop Until B1 >= 0 And B1 <= 14

                For I = 0 To 1:
                    plyrNames_DRAFT$(0, B, I) = plyrNames_DRAFT$(1, B1, I)
                Next

                For I = 0 To 19:
                    plyrRat_DRAFT(0, B, I) = plyrRat_DRAFT(1, B1, I)
                Next

                For I = 0 To 34:
                    leagTot_DRAFT(B, I) = teamRat_DRAFT(1, I)
                Next

                plyrRat_DRAFT(0, B, 2) = plyrRat_DRAFT(0, B, 2) * (leagueAverages%(normYrIdx, 1) / leagRat_DRAFT(1, 1))
                plyrRat_DRAFT(0, B, 6) = plyrRat_DRAFT(0, B, 6) * (leagueAverages%(normYrIdx, 2) / leagRat_DRAFT(1, 2))
                plyrRat_DRAFT(0, B, 7) = plyrRat_DRAFT(0, B, 7) * (leagueAverages%(normYrIdx, 2) / leagRat_DRAFT(1, 2))

                FG = Int(plyrRat_DRAFT(1, B1, 3) / plyrRat_DRAFT(1, B1, 2) * 100 + .5)
                FG = FG * (leagueAverages%(normYrIdx, 3) / leagRat_DRAFT(1, 3))

                plyrRat_DRAFT(0, B, 3) = FG / 100 * plyrRat_DRAFT(0, B, 2)

                leagTot_DRAFT(B, 5) = (leagueAverages%(normYrIdx, 1) / leagRat_DRAFT(1, 1)) * leagTot_DRAFT(B, 5)
                leagTot_DRAFT(B, 19) = leagTot_DRAFT(B, 19) * (leagueAverages%(normYrIdx, 1) / leagRat_DRAFT(1, 1))
                leagTot_DRAFT(B, 9) = leagTot_DRAFT(B, 9) * (leagueAverages%(normYrIdx, 2) / leagRat_DRAFT(1, 2))
                leagTot_DRAFT(B, 10) = leagTot_DRAFT(B, 10) * (leagueAverages%(normYrIdx, 2) / leagRat_DRAFT(1, 2))
                leagTot_DRAFT(B, 23) = leagTot_DRAFT(B, 23) * (leagueAverages%(normYrIdx, 2) / leagRat_DRAFT(1, 2))
                leagTot_DRAFT(B, 24) = leagTot_DRAFT(B, 24) * (leagueAverages%(normYrIdx, 2) / leagRat_DRAFT(1, 2))

                FG = Int(teamRat_DRAFT(1, 6) / teamRat_DRAFT(1, 5) * 100 + .5)
                FG = FG * (leagueAverages%(normYrIdx, 3) / leagRat_DRAFT(1, 3))

                teamRat_DRAFT(0, 6) = FG / 100 * teamRat_DRAFT(0, 5)

                FG = Int(teamRat_DRAFT(1, 20) / teamRat_DRAFT(1, 19) * 100 + .5)
                FG = FG * (leagueAverages%(normYrIdx, 3) / leagRat_DRAFT(1, 3))

                teamRat_DRAFT(0, 20) = FG / 100 * teamRat_DRAFT(0, 19)

            Case 2

                ' Save Team
                Cls

                nbrPlayers = 0

                For I = 0 To 14
                    If plyrNames_DRAFT$(0, I, 0) <> "XXX" Then nbrPlayers = nbrPlayers + 1
                Next

                For I = 0 To 14
                    If plyrNames_DRAFT$(0, I, 0) <> "XXX" Then
                        For I1 = 0 To 32
                            tmTotal_DRAFT(I1) = tmTotal_DRAFT(I1) + leagTot_DRAFT(I, I1)
                        Next I1
                    End If
                Next I

                For I1 = 0 To 32
                    tmTotal_DRAFT(I1) = tmTotal_DRAFT(I1) / nbrPlayers
                    teamRat_DRAFT(0, I1) = tmTotal_DRAFT(I1)
                Next

                Call SaveTeamDraft(draftNew, tmYears_DRAFT$(0), teamIdx)

        End Select

    Loop Until I0 = 2

    Color 15, 0
    Cls

    Call box0 (6, 8, 8, 70, 2, 15, 1)

    Locate 7

    U$ = "DRAFT ANOTHER TEAM? (Y/N)"

    Do
        Locate 7, (80 - Len(U$)) / 2
        Print U$
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If UCase$(I$) = "N" Then
        doneDrafting = 1
    End If

End Sub


'----------------------------------------
'       SaveTeamDraft Subroutine
'----------------------------------------
'This subroutine updates the source team
'with the newly assigned player / stat
'values after the draft.
'
'draftNew
'  0 = Existing Team
'  1 = New Team
Sub SaveTeamDraft (draftNew, yearNumber$, teamIdx)

    Shared tmAtt_DRAFT&()

    Shared plyrRat_DRAFT(), leagRat_DRAFT(), teamRat_DRAFT()

    Shared tmInfo_DRAFT$(), plyrNames_DRAFT$()

    teamYear$ = RTrim$(yearNumber$)

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    fileLength& = LOF(1)

    If draftNew = 1 Then 
        teamIdx = (fileLength& / TEAM_SIZE_BYTES) + 1
    End If

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

    LSet Q$(0) = tmInfo_DRAFT$(0, 0)
    LSet Q$(1) = tmInfo_DRAFT$(0, 1)

    For I = 0 To 6: 
        LSet Q$(I + 2) = MKI$(leagRat_DRAFT(0, I))
    Next

    LSet Q$(9) = MKI$(999)

    For I = 0 To 34: 
        LSet Q$(I + 10) = MKI$(teamRat_DRAFT(0, I))
    Next

    For I = 0 To 14

        LSet Q$(22 * I + 45) = plyrNames_DRAFT$(0, I, 0)
        LSet Q$(22 * I + 46) = plyrNames_DRAFT$(0, I, 1)

        For I1 = 0 To 19
            LSet Q$(22 * I + 47 + I1) = MKI$(plyrRat_DRAFT(0, I, I1))
        Next

    Next

    LSet Q$(375) = tmInfo_DRAFT$(0, 2)
    LSet Q$(376) = tmInfo_DRAFT$(0, 3)
    LSet Q$(377) = tmInfo_DRAFT$(0, 4)
    LSet Q$(378) = MKL$(tmAtt_DRAFT&(0))

    Put #1, teamIdx

    Close #1

End Sub


'----------------------------------------
'       ChooseDraftTeam SubRoutine
'----------------------------------------
'This subroutine provides an interface for
'choosing the team to draft players from
Sub ChooseDraftTeam (yearNumber$, numberTeams, chosenIdx, teamName$, draftIdx)

    Shared teamRat_DRAFT()

    Shared previousYear$

    Do

        chosenIdx = 0

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 4)

        Locate 7, 32

        Locate , 12: Print "ENTER DISK ID: "
        Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ <> Chr$(13) Then

                Locate 7, 27: Print I$;

                NY$ = ""

                Do Until NY$ = Chr$(13)

                    NY$ = GetKeyPress$

                    If NY$ = Chr$(8) And Len(I$) >= 1 Then
                        col% = Pos(0)
                        I$ = Left$(I$, (Len(I$) - 1))
                        Locate , col% - 1
                        Print " ";
                        Locate , col% - 1
                    Else
                        I$ = I$ + NY$
                        Print NY$;
                    End If

                Loop

                I$ = Left$(I$, (Len(I$) - 1))

                If Not (I$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(I$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        MsgText$ = "NO TEAM FILES COULD BE FOUND MATCHING THIS ID. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
                        BoxWidth = 24
                        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                        Call CSMsgBox(MsgText$, BoxWidth, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)
                        Exit Sub

                    Else

                        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

                        fileLength& = LOF(1)

                        Field #1, 15 As Q$(0)

                        numberTeams = fileLength& / TEAM_SIZE_BYTES

                        For teamIdx = 1 To numberTeams
                            Get #1, teamIdx
                            'numberTeams = numberTeams + 1
                            teamNames$(teamIdx) = Q$(0)
                            teamIndex%(teamIdx) = teamIdx
                        Next teamIdx

                        Close #1

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

                previousYear$ = yearNumber$

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectDraftTeam(numberTeams, yearNumber$, chosenIdx, ExitCode%, draftIdx)

                If ExitCode% <> 27 Then

                    Do
                        Locate 2, 33
                        Color teamRat_DRAFT(draftIdx, 33), teamRat_DRAFT(draftIdx, 34): Print teamName$

                        Color 15, 4
                        Locate 3, 32: Print "ANY CHANGE? (Y/N)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N" Or chosenIdx = -1

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    'If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'       SelectDraftTeam SubRoutine
'----------------------------------------
'This subroutine provides an interface for
'selecting the team to draft players from
Sub SelectDraftTeam (numberTeams, yearNumber$, selectedIdx, ExitCode%, draftIdx)

    ExitCode% = -1
    validData = 0

    If numberTeams > 0 And CheckString%(teamNames$(numberTeams)) = 0 Then

        SELECTION% = 1
        Count% = numberTeams
        Start% = 1
        FileFGColor = 15: FileBGColor = 4
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Rows% = 20
        Columns% = 4
        Spacing% = 6
        Row% = 4
        Column% = 1

        Color , 4
        Cls
        Color 14
        teamNumber = draftIdx + 1
        Print Using "TEAM # SELECTION"; teamNumber
        Locate 25, 1
        Color 14: Print "ESC";
        Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";
        Color 14: Print "ENTER";
        Color 15: Print "-SELECTS TEAM";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex%(SELECTION%)

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Call ReadDraftTeam(yearNumber$, selectedIdx, draftIdx, validData)

        End If

    End If

    If validData = 0 And ExitCode% <> 27 Then

        selectedIdx = -1
        Locate 9,
        Call CSMsgBox("THERE WAS AN ISSUE READING TEAM DATA... PRESS ANY KEY AND TRY ANOTHER DISK ID", 20, PASS)
        K$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    End If

End Sub


'----------------------------------------
'       ReadDraftTeam SubRoutine
'----------------------------------------
'This subroutine reads team data into
'arrays for the selected team
Sub ReadDraftTeam (teamYear$, teamIdx, draftIdx, dataOK)

    Shared tmAtt_DRAFT&()
    
    Shared plyrRat_DRAFT(), leagRat_DRAFT(), teamRat_DRAFT()

    Shared tmInfo_DRAFT$(), plyrNames_DRAFT$()

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    Field #1, 15 As Q$(0), 15 As Q$(1)

    For J = 0 To 6
        Field #1, 30 + J * 2 As X$, 2 As Q$(J + 2)
    Next

    Field #1, 44 As X$, 2 As Q$(9)

    For J = 0 To 34
        Field #1, 46 + J * 2 As X$, 2 As Q$(J + 10)
    Next

    For J = 0 To 14
        Field #1, 116 + J * 57 As X$, 15 As Q$(J * 22 + 45), 2 As Q$(J * 22 + 46)
        For J1 = 0 To 19
            Field #1, 133 + J * 57 + J1 * 2 As X$, 2 As Q$(J * 22 + 47 + J1)
        Next
    Next

    Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377), 4 As Q$(378)

    Get #1, teamIdx

    tmInfo_DRAFT$(draftIdx, 0) = RTrim$(Q$(0))
    tmInfo_DRAFT$(draftIdx, 1) = RTrim$(Q$(1))

    For I = 0 To 6:
        leagRat_DRAFT(draftIdx, I) = CVI(Q$(I + 2))
    Next

    For I = 0 To 34
        teamRat_DRAFT(draftIdx, I) = CVI(Q$(I + 10))
    Next

    For I = 0 To 14

        plyrNames_DRAFT$(draftIdx, I, 0) = RTrim$(Q$(22 * I + 45))
        plyrNames_DRAFT$(draftIdx, I, 1) = RTrim$(Q$(22 * I + 46))

        For I1 = 0 To 19
            plyrRat_DRAFT(draftIdx, I, I1) = CVI(Q$(22 * I + 47 + I1))
        Next

    Next

    tmInfo_DRAFT$(draftIdx, 2) = RTrim$(Q$(375))
    tmInfo_DRAFT$(draftIdx, 3) = RTrim$(Q$(376))
    tmInfo_DRAFT$(draftIdx, 4) = RTrim$(Q$(377))
    tmAtt_DRAFT&(draftIdx) = CVL(Q$(378))

    Close #1

    If (CheckString%(tmInfo_DRAFT$(draftIdx, 4)) = 1) Then

        dataOK = 0
        
    Else
    
        dataOK = 1
        
    End If

End Sub

