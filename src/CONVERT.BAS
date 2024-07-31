'----------------------------------------
'         ReadTeamOld Subroutine
'----------------------------------------
'Reads in all stats and data for the team
'that was selected, given it's index in
'the team file.
'This routine is highly customized TO
'  the specific game for which it has
'  been included for.
Sub ReadTeamOld (teamYear$, targetTeam$, teamIdx, dataOK)

    Shared playerRatings!(), leagueRatings(), teamRatings()

    Shared playerNames$(), teamInfo$()

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = (TEAM_SIZE_BYTES - 4)

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

    Get #1, teamIdx

    'Name
    teamInfo$(0) = RTrim$(Q$(0))
    targetTeam$ = teamInfo$(0)

    'Mascot
    teamInfo$(1) = RTrim$(Q$(1))

    For I = 0 To 6
        leagueRatings(I) = CVI(Q$(I + 2))
    Next

    For I = 0 To 34
        teamRatings(I) = CVI(Q$(I + 10))
    Next

    For I = 0 To 14

        playerNames$(I, 0) = Q$(22 * I + 45)
        playerNames$(I, 1) = Q$(22 * I + 46)

        For I1 = 0 To 19
            playerRatings!(I, I1) = CVI(Q$(22 * I + 47 + I1))
        Next

    Next

    'Abbreviation
    teamInfo$(2) = RTrim$(Q$(375))

    'Coach
    teamInfo$(3) = RTrim$(Q$(376))

    'Arena
    teamInfo$(4) = RTrim$(Q$(377))

    'Close team file
    Close #1

    If (CheckString%(teamInfo$(4)) = 1) Then

        dataOK = 0

    Else

        dataOK = 1

    End If

End Sub


'----------------------------------------
'       SaveTeamFileOld Subroutine
'----------------------------------------
'This subroutine saves the modifed team
'data (from corresponding arrays) to an
'appropriate spot in the select team file

'action%
'   1 = New Team
'   2 = Edit Team

Sub SaveTeamFileOld (yearNumber$, teamIdx, action%)

    Shared playerRatings!(), leagueRatings(), teamRatings()

    Shared playerNames$(), teamInfo$()

    Call CSMsgBox("SAVING TEAM FILES", 20, Pass)

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = (TEAM_SIZE_BYTES - 4)

    fileLength& = LOF(1)

    If action% <> 2 Then teamIdx = fileLength& / (TEAM_SIZE_BYTES - 4) + 1

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

    Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377), 4 As Q$(378)

    LSet Q$(0) = teamInfo$(0)
    LSet Q$(1) = teamInfo$(1)

    For I = 0 To 6
        LSet Q$(I + 2) = MKI$(leagueRatings(I))
    Next

    LSet Q$(9) = MKI$(999)

    For I = 0 To 34:
        LSet Q$(I + 10) = MKI$(teamRatings(I))
    Next

    For I = 0 To 14

        LSet Q$(22 * I + 45) = playerNames$(I, 0)
        LSet Q$(22 * I + 46) = playerNames$(I, 1)

        For I1 = 0 To 19
            LSet Q$(22 * I + 47 + I1) = MKI$(playerRatings!(I, I1))
        Next

    Next

    LSet Q$(375) = teamInfo$(2)
    LSet Q$(376) = teamInfo$(3)
    LSet Q$(377) = teamInfo$(4)

    Put #1, teamIdx

    Close #1

    Call CSMsgBox("", 0, Pass)

End Sub


'----------------------------------------
'       SaveTeamFileNew Subroutine
'----------------------------------------
'This routine saves the modified data back
'to the original team file
Sub SaveTeamFileNew (fileName$, teamIdx, action%)

    Shared teamAttendance&

    Shared playerRatings!(), leagueRatings(), teamRatings()
    Shared playerNames$(), teamInfo$()

    Call CSMsgBox("SAVING TEAM FILES", 20, Pass)

    Open fileName$ For Random As #1 Len = TEAM_SIZE_BYTES

    fileLength& = LOF(1)

    If action% <> 2 Then
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

    Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377), 4 As Q$(378)

    LSet Q$(0) = teamInfo$(0)
    LSet Q$(1) = teamInfo$(1)

    For I = 0 To 6
        LSet Q$(I + 2) = MKI$(leagueRatings(I))
    Next

    LSet Q$(9) = MKI$(999)

    For I = 0 To 34:
        LSet Q$(I + 10) = MKI$(teamRatings(I))
    Next

    For I = 0 To 14

        LSet Q$(22 * I + 45) = playerNames$(I, 0)
        LSet Q$(22 * I + 46) = playerNames$(I, 1)

        For I1 = 0 To 19
            LSet Q$(22 * I + 47 + I1) = MKI$(playerRatings!(I, I1))
        Next

    Next

    LSet Q$(375) = teamInfo$(2)
    LSet Q$(376) = teamInfo$(3)
    LSet Q$(377) = teamInfo$(4)
    LSet Q$(378) = MKL$(teamAttendance&)

    Put #1, teamIdx

    Close #1

    Call CSMsgBox("", 0, Pass)

End Sub


'----------------------------------------
'       ConvertTeam4to5 Subroutine
'----------------------------------------
' Reads in existing team data, one team
' at-a-time. For each team, it tries
' to calculate what actual stats it can.
' From there, the updated data is saved.
Sub ConvertTeam4to5 (targetFile$, silent)

    Shared teamAttendance&

    diskID$ = Right$(targetFile$, Len(targetFile$) - InStr(targetFile$, "PROBBTMS.") - 8)
    newFile$ = targetFile$ + ".NEW"

    If targetFile$ <> "" Then

        Open targetFile$ For Random As #1
        fileLength& = LOF(1)
        Close #1

        numberTeams = fileLength& / (TEAM_SIZE_BYTES - 4)

        For currTeam = 1 To numberTeams

            Call ReadTeamOld(diskID$, nothing$, currTeam, validData)

            teamAttendance& = 0

            Call SaveTeamFileNew(newFile$, currTeam, 2)

        Next currTeam

        Kill targetFile$

        Call FCopy(newFile$, targetFile$, Buff$, copyErr%)

        Kill newFile$

        If silent = 0 Then
            result& = _MessageBox("Success!", "The team file has been converted for use", "ok", "info", 1)
        End If

    End If

End Sub


'----------------------------------------
'       ConvertTeam5to4 Subroutine
'----------------------------------------
Sub ConvertTeam5to4 (targetFile$, silent)

    diskID$ = Right$(targetFile$, Len(targetFile$) - InStr(targetFile$, "PROBBTMS.") - 8)
    oldFile$ = targetFile$ + ".OLD"

    If targetFile$ <> "" Then

        Open targetFile$ For Random As #1
        fileLength& = LOF(1)
        Close #1

        numberTeams = fileLength& / TEAM_SIZE_BYTES

        For currTeam = 1 To numberTeams

            Call ReadTeam(diskID$, nothing$, currTeam, validData)

            'To convert to 4.0, let's just not write the new fields!!!
            If validData = 1 Then
                Call SaveTeamFileOld(diskID$, currTeam, 2)
            End If

        Next currTeam

        Kill targetFile$

        Call FCopy(oldFile$, targetFile$, Buff$, copyErr%)

        Kill oldFile$

        If silent = 0 Then
            result& = _MessageBox("Success!", "The team file has been converted for use", "ok", "info", 1)
        End If

    End If

End Sub

