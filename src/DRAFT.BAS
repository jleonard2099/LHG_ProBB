Sub DRAFT ()

    _Title "Basketball: The Pro Game - Team Draft"

    Shared leagTot_DRAFT(), leagRat_DRAFT%(), plyrRat_DRAFT%(), tmRat_DRAFT%()

    Shared draftYears$(), teamYears$(), players_DRAFT$(), tmInfo_DRAFT$()

    Dim TTL%(34)

    ReDim menuChoices$(1 To 3)

    Do

        Selection% = 1
        Count% = 52
        Start% = 1
        FileFGColr% = 15: FileBGColr% = 4
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Rows% = 11
        Columns% = 5
        Spacing% = 10
        Row% = 6
        Column% = 1
        Color , 4
        Cls
        Locate , 16
        Color 14: Print "SELECT YEAR TO NORMALIZE DRAFTED PLAYERS TO"
        Color 15

        Do
            _MouseShow "DEFAULT"
            MAMenu draftYears$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide
        Loop Until ExitCode% = 27 Or ExitCode% = 13

        If ExitCode% = 27 Then
            BO% = 1
        Else

            Do
                Locate 3, 33
                Color 15, 0
                Print draftYears$(Selection%)

                Color 15, 4

                Locate , 30: Print "ANY CHANGE (YN)";

                I$ = GetKeyPress$

            Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

            YR% = Selection%

        End If

        If ExitCode% = 27 Then Exit Sub

    Loop Until UCase$(I$) = "N" Or ExitCode% = 27

    If ExitCode% <> 27 Then

        Do

            Call TITLE

            menuChoices$(1) = "DRAFT A NEW TEAM         "
            menuChoices$(2) = "DRAFT TO AN EXISTING TEAM"
            menuChoices$(3) = "EXIT TO MAIN MENU        "
            Selection% = 1
            Count% = 3
            Start% = 1
            ExitCode% = 3
            FileFGColr% = 15: FileBGColr% = 1
            HiLiteFGColr% = 0: HiLiteBGColr% = 7
            Spacing% = 1
            Row% = 8
            Column% = 25

            Call box0(7, 24, 11, 50, 2, 15, 1)

            _MouseShow "DEFAULT"
            MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
            _MouseHide

            Color , 0

            If ExitCode% <> 27 Then

                Select Case Selection%

                    Case 1

                        For I = 0 To 1:
                            tmInfo_DRAFT$(0, I) = "XXX"
                        Next

                        For I = 0 To 6:
                            leagRat_DRAFT%(0, I) = 0
                        Next

                        For I = 0 To 34
                            tmRat_DRAFT%(0, I) = 0
                            TTL%(I) = 0
                        Next

                        For I = 0 To 14
                            For I1 = 0 To 34
                                leagTot_DRAFT(I, I1) = 0
                            Next
                        Next

                        For I = 0 To 14
                            players_DRAFT$(0, I, 0) = "XXX"
                            players_DRAFT$(0, I, 1) = "X"
                            For I1 = 0 To 19
                                plyrRat_DRAFT%(0, I, I1) = 0
                            Next
                        Next

                        Call DraftNewTeam(teamYears$(0), tmInfo_DRAFT$(0, 0), teamIdx)
                        Call DraftChoices(1, doneDraft, teamIdx, TTL%())

                    Case 2
                        draftIdx = 0
                        Call ChooseDraftTeam(teamYears$(draftIdx), NTMS, teamIdx, tmInfo_DRAFT$(draftIdx, 0), draftIdx)
                        Call DraftChoices(0, doneDraft, teamIdx, TTL%())

                End Select

                If doneDraft = 1 Then Selection% = 3

            End If

        Loop Until Selection% = 3 Or ExitCode% = 27

    End If 'Done checking for ESC

End Sub

'----------------------------------------
'       DraftNewTeam SubRoutine
'----------------------------------------
'...explanation...
Sub DraftNewTeam (teamYear$, teamName$, teamIdx)

    Shared tmInfo_DRAFT$(), teamYears$()
    Shared leagRat_DRAFT%(), leagueAverages%(), tmRat_DRAFT%()

    Color 15, 0
    Cls

    Color 14
    Print "TEAM INFORMATION"
    Color 15

    'Assign stats from year chosen to 'normalize' to
    leagRat_DRAFT%(0, 0) = 80
    ' FG
    leagRat_DRAFT%(0, 1) = leagueAverages%(YR%, 1)
    'RB
    leagRat_DRAFT%(0, 2) = leagueAverages%(YR%, 2)
    'FG%
    leagRat_DRAFT%(0, 3) = leagueAverages%(YR%, 3)
    'PF
    leagRat_DRAFT%(0, 4) = leagueAverages%(YR%, 4)
    '3FG%
    leagRat_DRAFT%(0, 5) = leagueAverages%(YR%, 5)
    leagRat_DRAFT%(0, 6) = 0

    Locate 3: Input "1. DISK ID "; teamYear$
    Locate 4: Input "2. TEAM NAME "; teamName$
    Locate 5: Input "3. TEAM NICKNAME "; tmInfo_DRAFT$(0, 1)
    Locate 6: Input "4. TEAM ABBREV (IE. BOS) "; tmInfo_DRAFT$(0, 2)
    Locate 7: Input "5. COACH'S NAME "; tmInfo_DRAFT$(0, 3)
    Locate 8: Input "6. ARENA NAME "; tmInfo_DRAFT$(0, 4)
    Locate 9: Input "7. TEXT COLOR (0-15) "; tmRat_DRAFT%(0, 33)
    Locate 10: Input "8. BCKGRND COLOR (0-7) "; tmRat_DRAFT%(0, 34)
    Locate 12
    Print "TEAM APPEARANCE";
    Color tmRat_DRAFT%(0, 33), tmRat_DRAFT%(0, 34)
    Print RTrim$(teamName$) + " " + RTrim$(tmInfo_DRAFT$(0, 1))
    Color 15, 0

    Do

        Do
            Locate 20
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 8

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 3: Input "1. DISK ID "; teamYear$

                Case 2
                    Locate 4: Input "2. TEAM NAME "; teamName$

                Case 3
                    Locate 5: Input "3. TEAM NICKNAME "; tmInfo_DRAFT$(0, 1)

                Case 4
                    Locate 6: Input "4. TEAM ABBREV (IE. BOS) "; tmInfo_DRAFT$(0, 2)

                Case 5
                    Locate 7: Input "5. COACH'S NAME "; tmInfo_DRAFT$(0, 3)

                Case 6
                    Locate 8: Input "6. ARENA NAME "; tmInfo_DRAFT$(0, 4)

                Case 7
                    Locate 9: Input "7. TEXT COLOR (0-15) "; tmRat_DRAFT%(0, 33)
                    Locate 12
                    Print "TEAM APPEARANCE";
                    Color tmRat_DRAFT%(0, 33), tmRat_DRAFT%(0, 34)
                    Print RTrim$(teamName$) + " " + RTrim$(tmInfo_DRAFT$(0, 1))
                    Color 15, 0

                Case 8
                    Locate 10: Input "8. BCKGRND COLOR (0-7) "; tmRat_DRAFT%(0, 34)
                    Locate 12
                    Print "TEAM APPEARANCE";
                    Color tmRat_DRAFT%(0, 33), tmRat_DRAFT%(0, 34)
                    Print RTrim$(teamName$) + " " + RTrim$(tmInfo_DRAFT$(0, 1))
                    Color 15, 0

            End Select

        End If

    Loop Until B9 = 0

End Sub


'----------------------------------------
'     DraftNewTeam_OffDef SubRoutine
'----------------------------------------
' There original code does not access
' this, nor does the original game.
' However, it clearly was written to
' help finish the new team creation
' process by finishing the ratings.
Sub DraftNewTeam_OffDef ()

    Cls

    Color 14
    Print "TEAM OFFENSIVE INFORMATION"
    Color 15

    Locate 3: Input " 1. FGM "; tmRat_DRAFT%(0, 6)
    Locate 4: Input " 2. FGA "; tmRat_DRAFT%(0, 5)
    Locate 5: Input " 3. FTM "; tmRat_DRAFT%(0, 8)
    Locate 6: Input " 4. FTA "; tmRat_DRAFT%(0, 7)
    Locate 7: Input " 5. OFF REB "; tmRat_DRAFT%(0, 9)
    Locate 8: Input " 6. DEF REB "; tmRat_DRAFT%(0, 10)

    If tmRat_DRAFT%(0, 9) = 0 And RB = 0 Then
        tmRat_DRAFT%(0, 9) = .32 * tmRat_DRAFT%(0, 10): tmRat_DRAFT%(0, 10) = tmRat_DRAFT%(0, 10) - tmRat_DRAFT%(0, 9)
        Locate 7: Print " 5. OFF REB "; tmRat_DRAFT%(0, 9)
        Locate 8: Print " 6. DEF REB "; tmRat_DRAFT%(0, 10)
    End If

    Locate 9: Input " 7. ASSISTS "; tmRat_DRAFT%(0, 11)
    Locate 10: Input " 8. FOULS "; tmRat_DRAFT%(0, 12)
    Locate 11: Input " 9. DISQ "; tmRat_DRAFT%(0, 13)
    Locate 12: Input "10. STEALS "; tmRat_DRAFT%(0, 14)
    Locate 13: Input "11. TURNOVERS "; tmRat_DRAFT%(0, 15)
    Locate 14: Input "12. BLK SHOTS "; tmRat_DRAFT%(0, 16)
    Locate 15: Input "13. 3-FGM "; tmRat_DRAFT%(0, 18)
    Locate 16: Input "14. 3-FGA "; tmRat_DRAFT%(0, 17)

    Do

        Do
            Locate 18
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 14

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 3: Input " 1. FGM "; tmRat_DRAFT%(0, 6)

                Case 2
                    Locate 4: Input " 2. FGA "; tmRat_DRAFT%(0, 5)

                Case 3
                    Locate 5: Input " 3. FTM "; tmRat_DRAFT%(0, 8)

                Case 4
                    Locate 6: Input " 4. FTA "; tmRat_DRAFT%(0, 7)

                Case 5
                    Locate 7: Input " 5. OFF REB "; tmRat_DRAFT%(0, 9)

                Case 6
                    Locate 8: Input " 6. DEF REB "; tmRat_DRAFT%(0, 10)

                    'Calculate Rebounds if missing
                    If tmRat_DRAFT%(0, 9) = 0 And RB = 0 Then
                        'Off Rebounds = ~ Def Rebounds / 3
                        tmRat_DRAFT%(0, 9) = .32 * tmRat_DRAFT%(0, 10)
                        tmRat_DRAFT%(0, 10) = tmRat_DRAFT%(0, 10) - tmRat_DRAFT%(0, 9)
                        Locate 7: Print " 5. OFF REB "; tmRat_DRAFT%(0, 9)
                        Locate 8: Print " 6. DEF REB "; tmRat_DRAFT%(0, 10)
                    End If

                Case 7
                    Locate 9: Input " 7. ASSISTS "; tmRat_DRAFT%(0, 11)

                Case 8
                    Locate 10: Input " 8. FOULS "; tmRat_DRAFT%(0, 12)

                Case 9
                    Locate 11: Input " 9. DISQ "; tmRat_DRAFT%(0, 13)

                Case 10
                    Locate 12: Input "10. STEALS "; tmRat_DRAFT%(0, 14)

                Case 11
                    Locate 13: Input "11. TURNOVERS "; tmRat_DRAFT%(0, 15)

                Case 12
                    Locate 14: Input "12. BLK SHOTS "; tmRat_DRAFT%(0, 16)

                Case 13
                    Locate 15: Input "13. 3-FGM "; tmRat_DRAFT%(0, 18)

                Case 14
                    Locate 16: Input "14. 3-FGA "; tmRat_DRAFT%(0, 17)

            End Select

        End If

    Loop Until B9 = 0

    '-----

    Cls

    Color 14
    Print "TEAM DEFENSIVE INFORMATION"
    Color 15

    Locate 3: Input " 1. FGM "; tmRat_DRAFT%(0, 20)
    Locate 4: Input " 2. FGA "; tmRat_DRAFT%(0, 19)
    Locate 5: Input " 3. FTM "; tmRat_DRAFT%(0, 22)
    Locate 6: Input " 4. FTA "; tmRat_DRAFT%(0, 21)
    Locate 7: Input " 5. OFF REB "; tmRat_DRAFT%(0, 23)
    Locate 8: Input " 6. DEF REB "; tmRat_DRAFT%(0, 24)

    If tmRat_DRAFT%(0, 23) = 0 And RB = 0 Then
        tmRat_DRAFT%(0, 23) = .35 * tmRat_DRAFT%(0, 24)
        tmRat_DRAFT%(0, 24) = tmRat_DRAFT%(0, 24) - tmRat_DRAFT%(0, 23)
        Locate 7: Print " 5. OFF REB "; tmRat_DRAFT%(0, 23)
        Locate 8: Print " 6. DEF REB "; tmRat_DRAFT%(0, 24)
    End If

    Locate 9: Input " 7. ASSISTS "; tmRat_DRAFT%(0, 25)
    Locate 10: Input " 8. FOULS "; tmRat_DRAFT%(0, 26)
    Locate 11: Input " 9. DISQ "; tmRat_DRAFT%(0, 27)
    Locate 12: Input "10. STEALS "; tmRat_DRAFT%(0, 28)
    Locate 13: Input "11. TURNOVERS "; tmRat_DRAFT%(0, 29)
    Locate 14: Input "12. BLK SHOTS "; tmRat_DRAFT%(0, 30)
    Locate 15: Input "13. 3-FGM "; tmRat_DRAFT%(0, 32)
    Locate 16: Input "14. 3-FGA "; tmRat_DRAFT%(0, 31)

    Do

        Do
            Locate 18
            Input "# TO CHANGE (0 TO CONTINUE) "; B9
        Loop Until B9 >= 0 And B9 <= 14

        If B9 <> 0 Then

            Select Case B9

                Case 1
                    Locate 3: Input " 1. FGM "; tmRat_DRAFT%(0, 20)

                Case 2
                    Locate 4: Input " 2. FGA "; tmRat_DRAFT%(0, 19)

                Case 3
                    Locate 5: Input " 3. FTM "; tmRat_DRAFT%(0, 22)

                Case 4
                    Locate 6: Input " 4. FTA "; tmRat_DRAFT%(0, 21)

                Case 5
                    Locate 7: Input " 5. OFF REB "; tmRat_DRAFT%(0, 23)

                Case 6
                    Locate 8: Input " 6. DEF REB "; tmRat_DRAFT%(0, 24)

                    'Calculate Rebounds if missing
                    If tmRat_DRAFT%(0, 23) = 0 And RB = 0 Then
                        'Off Rebounds = ~ Def Rebounds / 3
                        tmRat_DRAFT%(0, 23) = .35 * tmRat_DRAFT%(0, 24)
                        tmRat_DRAFT%(0, 24) = tmRat_DRAFT%(0, 24) - tmRat_DRAFT%(0, 23)
                        Locate 7: Print " 5. OFF REB "; tmRat_DRAFT%(0, 23)
                        Locate 8: Print " 6. DEF REB "; tmRat_DRAFT%(0, 24)
                    End If

                Case 7
                    Locate 9: Input " 7. ASSISTS "; tmRat_DRAFT%(0, 25)

                Case 8
                    Locate 10: Input " 8. FOULS "; tmRat_DRAFT%(0, 26)

                Case 9
                    Locate 11: Input " 9. DISQ "; tmRat_DRAFT%(0, 27)

                Case 10
                    Locate 12: Input "10. STEALS "; tmRat_DRAFT%(0, 28)

                Case 11
                    Locate 13: Input "11. TURNOVERS "; tmRat_DRAFT%(0, 29)

                Case 12
                    Locate 14: Input "12. BLK SHOTS "; tmRat_DRAFT%(0, 30)

                Case 13
                    Locate 15: Input "13. 3-FGM "; tmRat_DRAFT%(0, 32)

                Case 14
                    Locate 16: Input "14. 3-FGA "; tmRat_DRAFT%(0, 31)

            End Select

        End If

    Loop Until B9 = 0

    'This is ripped from the Team Input routine
    'ST / BK / T0 represent whether the user has
    'Steals / Blocks / Turnover data to enter
    'If tmRat_DRAFT%(0, 28) = 0 And ST = 0 Then tmRat_DRAFT%(0, 28) = tmRat_DRAFT%(0, 5) * .1
    'If tmRat_DRAFT%(0, 14) = 0 And ST = 0 Then tmRat_DRAFT%(0, 14) = tmRat_DRAFT%(0, 19) * .1
    'If tmRat_DRAFT%(0, 16) = 0 And BK = 0 Then tmRat_DRAFT%(0, 16) = tmRat_DRAFT%(0, 19) * .06
    'If tmRat_DRAFT%(0, 30) = 0 And BK = 0 Then tmRat_DRAFT%(0, 30) = tmRat_DRAFT%(0, 5) * .06
    'If tmRat_DRAFT%(0, 29) = 0 And T0 = 0 Then tmRat_DRAFT%(0, 29) = 18 * leagRat_DRAFT%(0, 0): tmRat_DRAFT%(0, 15) = tmRat_DRAFT%(0, 29)


End Sub



'----------------------------------------
'       DraftChoices SubRoutine
'----------------------------------------
'...explanation...
Sub DraftChoices (draftNew, doneDrafting, teamIdx, TTL%())

    Dim NP

    Shared tmInfo_DRAFT$(), players_DRAFT$(), draftYears$(), teamYears$()
    Shared plyrRat_DRAFT%(), leagRat_DRAFT%(), leagueAverages%(), leagTot_DRAFT(), tmRat_DRAFT%()

    Do

        Color 0, 0
        Cls
        Color tmRat_DRAFT%(0, 33), tmRat_DRAFT%(0, 34): Print tmInfo_DRAFT$(0, 0);

        Color 14, 0
        Print "     (DRAFTING)"
        Print
        Print "PLAYERS"

        For I = 0 To 14
            Color 2: Print Using "## "; I;
            Color 15: Print players_DRAFT$(0, I, 0); " "; players_DRAFT$(0, I, 1)
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
                Call ChooseDraftTeam(teamYears$(draftIdx), NTMS, destIdx, tmInfo_DRAFT$(draftIdx, 0), draftIdx)

                Color , 0
                Cls
                Color tmRat_DRAFT%(1, 33), tmRat_DRAFT%(1, 34): Print tmInfo_DRAFT$(draftIdx, 0)
                Color 14, 0

                Print
                Print "PLAYERS"

                For I = 0 To 14
                    Color 2: Print Using "## "; I;
                    Color 15: Print players_DRAFT$(draftIdx, I, 0); " "; players_DRAFT$(draftIdx, I, 1)
                Next

                Do
                    Color 14
                    Locate 21, 1: Print "SELECT PLAYER # TO DRAFT ";

                    Input B1

                Loop Until B1 >= 0 And B1 <= 14

                For I = 0 To 1:
                    players_DRAFT$(0, B, I) = players_DRAFT$(1, B1, I)
                Next

                For I = 0 To 19:
                    plyrRat_DRAFT%(0, B, I) = plyrRat_DRAFT%(1, B1, I)
                Next

                For I = 0 To 34:
                    leagTot_DRAFT(B, I) = tmRat_DRAFT%(1, I)
                Next

                plyrRat_DRAFT%(0, B, 2) = plyrRat_DRAFT%(0, B, 2) * (leagueAverages%(YR%, 1) / leagRat_DRAFT%(1, 1))
                plyrRat_DRAFT%(0, B, 6) = plyrRat_DRAFT%(0, B, 6) * (leagueAverages%(YR%, 2) / leagRat_DRAFT%(1, 2))
                plyrRat_DRAFT%(0, B, 7) = plyrRat_DRAFT%(0, B, 7) * (leagueAverages%(YR%, 2) / leagRat_DRAFT%(1, 2))

                FG = Int(plyrRat_DRAFT%(1, B1, 3) / plyrRat_DRAFT%(1, B1, 2) * 100 + .5)
                FG = FG * (leagueAverages%(YR%, 3) / leagRat_DRAFT%(1, 3))

                plyrRat_DRAFT%(0, B, 3) = FG / 100 * plyrRat_DRAFT%(0, B, 2)

                leagTot_DRAFT(B, 5) = (leagueAverages%(YR%, 1) / leagRat_DRAFT%(1, 1)) * leagTot_DRAFT(B, 5)
                leagTot_DRAFT(B, 19) = leagTot_DRAFT(B, 19) * (leagueAverages%(YR%, 1) / leagRat_DRAFT%(1, 1))
                leagTot_DRAFT(B, 9) = leagTot_DRAFT(B, 9) * (leagueAverages%(YR%, 2) / leagRat_DRAFT%(1, 2))
                leagTot_DRAFT(B, 10) = leagTot_DRAFT(B, 10) * (leagueAverages%(YR%, 2) / leagRat_DRAFT%(1, 2))
                leagTot_DRAFT(B, 23) = leagTot_DRAFT(B, 23) * (leagueAverages%(YR%, 2) / leagRat_DRAFT%(1, 2))
                leagTot_DRAFT(B, 24) = leagTot_DRAFT(B, 24) * (leagueAverages%(YR%, 2) / leagRat_DRAFT%(1, 2))

                FG = Int(tmRat_DRAFT%(1, 6) / tmRat_DRAFT%(1, 5) * 100 + .5)
                FG = FG * (leagueAverages%(YR%, 3) / leagRat_DRAFT%(1, 3))

                tmRat_DRAFT%(0, 6) = FG / 100 * tmRat_DRAFT%(0, 5)

                FG = Int(tmRat_DRAFT%(1, 20) / tmRat_DRAFT%(1, 19) * 100 + .5)
                FG = FG * (leagueAverages%(YR%, 3) / leagRat_DRAFT%(1, 3))

                tmRat_DRAFT%(0, 20) = FG / 100 * tmRat_DRAFT%(0, 19)

            Case 2

                ' Save Team
                Cls

                NP = 0

                For I = 0 To 14
                    If players_DRAFT$(0, I, 0) <> "XXX" Then NP = NP + 1
                Next

                For I = 0 To 14
                    If players_DRAFT$(0, I, 0) <> "XXX" Then
                        For I1 = 0 To 32
                            TTL%(I1) = TTL%(I1) + leagTot_DRAFT(I, I1)
                        Next I1
                    End If
                Next I

                For I1 = 0 To 32
                    TTL%(I1) = TTL%(I1) / NP
                    tmRat_DRAFT%(0, I1) = TTL%(I1)
                Next

                Call SaveTeamDraft(draftNew, teamYears$(0), teamIdx)

        End Select

    Loop Until I0 = 2

    Cls

    JJ = 1

    Locate 6, 8: Color 15, 1

    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

    For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    Locate 7 + JJ, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";

    Locate 7

    U$ = "DRAFT ANOTHER TEAM (YN)"

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
'...explanation...
Sub SaveTeamDraft (draftNew, yearNumber$, teamIdx)

    Shared plyrRat_DRAFT%(), leagRat_DRAFT%(), tmRat_DRAFT%()

    Shared tmInfo_DRAFT$(), players_DRAFT$()

    teamYear$ = RTrim$(yearNumber$)

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    fileLength& = LOF(1)

    If draftNew = 1 Then teamIdx = fileLength& / TEAM_SIZE_BYTES + 1

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

    For I = 0 To 6: LSet Q$(I + 2) = MKI$(leagRat_DRAFT%(0, I)): Next

    LSet Q$(9) = MKI$(999)

    For I = 0 To 34: LSet Q$(I + 10) = MKI$(tmRat_DRAFT%(0, I)): Next

    For I = 0 To 14

        LSet Q$(22 * I + 45) = players_DRAFT$(0, I, 0)

        LSet Q$(22 * I + 46) = players_DRAFT$(0, I, 1)

        For I1 = 0 To 19
            LSet Q$(22 * I + 47 + I1) = MKI$(plyrRat_DRAFT%(0, I, I1))
        Next

    Next

    LSet Q$(375) = tmInfo_DRAFT$(0, 2)
    LSet Q$(376) = tmInfo_DRAFT$(0, 3)
    LSet Q$(377) = tmInfo_DRAFT$(0, 4)

    Put #1, teamIdx

    Close 1

End Sub


'----------------------------------------
'       ChooseDraftTeam SubRoutine
'----------------------------------------
'Provides interface for choosing the
'team to draft players from
Sub ChooseDraftTeam (yearNumber$, numberTeams, chosenIdx, teamName$, draftIdx)

    Shared tmRat_DRAFT%()

    Shared previousYear$

    Do

        Color 15, 0
        Cls

        JJ = 7

        Locate 6, 8
        Color 15, 4

        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        For II = 1 To JJ
            Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

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

                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If Not (NY$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(NY$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        Locate 10, 8

                        Call CSMsgBox("NO TEAMS WERE FOUND MATCHING THAT ID... PRESS ANY KEY TO SELECT ANOTHER ID", 20, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)

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

                        Close 1

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

                previousYear$ = yearNumber$

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectDraftTeam(numberTeams, yearNumber$, chosenIdx, teamName$, ExitCode%, draftIdx)

                If ExitCode% <> 27 Then

                    Do
                        Locate 2, 33
                        Color tmRat_DRAFT%(draftIdx, 33), tmRat_DRAFT%(draftIdx, 34): Print teamName$

                        Color 15, 4
                        Locate 3, 33: Print "ANY CHANGE? (YN)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N"

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'       SelectDraftTeam SubRoutine
'----------------------------------------
'Provides interface for selecting the
'team to draft players from
Sub SelectDraftTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, draftIdx)

    If numberTeams > 0 Then

        SELECTION% = 1
        Count% = numberTeams
        Start% = 1
        FileFGColr% = 15: FileBGColr% = 4
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
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
        Color 14: Print "ESC";: Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";
        Color 14: Print "ENTER";: Color 15: Print "-SELECTS TEAM";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex%(SELECTION%)

            Locate 2, 33: Color 3
            Print "LOADING.."

            Call ReadDraftTeam(yearNumber$, teamName$, selectedIdx, draftIdx)

        End If

    End If

End Sub


'----------------------------------------
'       ReadDraftTeam SubRoutine
'----------------------------------------
'Reads team data into arrays for the
'selected team
Sub ReadDraftTeam (teamYear$, targetTeam$, teamIdx, draftIdx)

    Shared tmInfo_DRAFT$(), players_DRAFT$(), draftYears$(), teamYears$()
    Shared plyrRat_DRAFT%(), leagRat_DRAFT%(), tmRat_DRAFT%()

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    Field #1, 15 As Q$(0), 15 As Q$(1)

    For J% = 0 To 6
        Field #1, 30 + J% * 2 As X$, 2 As Q$(J% + 2)
    Next

    Field #1, 44 As X$, 2 As Q$(9)

    For J% = 0 To 34
        Field #1, 46 + J% * 2 As X$, 2 As Q$(J% + 10)
    Next

    For J% = 0 To 14
        Field #1, 116 + J% * 57 As X$, 15 As Q$(J% * 22 + 45), 2 As Q$(J% * 22 + 46)
        For J1 = 0 To 19
            Field #1, 133 + J% * 57 + J1 * 2 As X$, 2 As Q$(J% * 22 + 47 + J1)
        Next
    Next

    Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)

    Get #1, teamIdx

    tmInfo_DRAFT$(draftIdx, 0) = RTrim$(Q$(0))
    tmInfo_DRAFT$(draftIdx, 1) = RTrim$(Q$(1))

    For I = 0 To 6:
        leagRat_DRAFT%(draftIdx, I) = CVI(Q$(I + 2))
    Next

    For I = 0 To 34
        tmRat_DRAFT%(draftIdx, I) = CVI(Q$(I + 10))
    Next

    For I = 0 To 14

        players_DRAFT$(draftIdx, I, 0) = RTrim$(Q$(22 * I + 45))
        players_DRAFT$(draftIdx, I, 1) = RTrim$(Q$(22 * I + 46))

        For I1 = 0 To 19
            plyrRat_DRAFT%(draftIdx, I, I1) = CVI(Q$(22 * I + 47 + I1))
        Next

    Next

    tmInfo_DRAFT$(draftIdx, 2) = RTrim$(Q$(375))
    tmInfo_DRAFT$(draftIdx, 3) = RTrim$(Q$(376))
    tmInfo_DRAFT$(draftIdx, 4) = RTrim$(Q$(377))

    Close 1

End Sub

