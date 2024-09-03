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

    Call FCopy(targetFile$, oldFile$, Buff$, copyErr%)

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

        Kill oldFile$

        If silent = 0 Then
            result& = _MessageBox("Success!", "The team file has been converted for use", "ok", "info", 1)
        End If

    End If

End Sub


'----------------------------------------
'         ReadSchedOld Subroutine
'----------------------------------------
'Reads in all schedule data from FILES
'compatible with the DOS game.
'This routine is highly customized TO
'  the specific game for which it has
'  been included for.
Sub ReadSchedOld (targetFile$)

    Shared BS%, NS%

    Shared scheduleAP%()
    Shared homeScores(), visitorScores()
    Shared scheduleNG%(), scheduleYN$()

    'Open diskPaths$(3) + targetFile$ + ".SCD" For Random As #1 Len = SCHEDULE_SIZE_BYTES - 4
    Open targetFile$ For Random As #1 Len = SCHEDULE_SIZE_BYTES - 8

    fileLength& = LOF(1)
    scheduleAP%(0) = fileLength& / (SCHEDULE_SIZE_BYTES - 8)
    BS% = Int((scheduleAP%(0) - 1) / 20)

    For X = 0 To 18
        Field #1, X * 2 As X$, 2 As Q$(X + 1), (SCHEDULE_SIZE_BYTES - 8) - 2 - 2 * X As X$
    Next

    Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)

    For I = 1 To scheduleAP%(0)

        Get #1, I

        For X = 0 To 18:
            scheduleNG%(I, X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 1:
            scheduleYN$(I, X) = RTrim$(Q$(20 + X))
        Next

        visitorScores(I) = CVI(Q$(22))
        homeScores(I) = CVI(Q$(23))

    Next

    Close #1

    NS% = 0

End Sub


'----------------------------------------
'         SaveSchedOld Subroutine
'----------------------------------------
'This subroutine saves schedule data
'(from corresponding arrays) to an
'the select schedule file.
'This routine is highly customized TO
'  the specific game for which it has
'  been included for.
Sub SaveSchedOld (saveFile$)

    Shared homeScores(), visitorScores()
    Shared scheduleAP%(), scheduleNG%()
    Shared scheduleYN$()

    If _FileExists(diskPaths$(3) + saveFile$ + ".SCD") Then Kill diskPaths$(3) + saveFile$ + ".SCD"

    Open diskPaths$(3) + saveFile$ + ".SCD" For Random As #1 Len = SCHEDULE_SIZE_BYTES - 8

    For X = 0 To 18
        Field #1, X * 2 As X$, 2 As Q$(X + 1), (SCHEDULE_SIZE_BYTES - 8) - 2 - 2 * X As X$
    Next

    Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 5 As Q$(20), 5 As Q$(21)

    For I = 1 To scheduleAP%(0)

        For X = 0 To 18
            LSet Q$(X + 1) = MKI$(scheduleNG%(I, X))
        Next

        For X = 0 To 1
            LSet Q$(20 + X) = RTrim$(scheduleYN$(I, X))
        Next

        LSet Q$(22) = MKI$(visitorScores(I))
        LSet Q$(23) = MKI$(homeScores(I))

        Put #1, I

    Next

    Close #1

End Sub


'----------------------------------------
'    SaveSchedFileConverted SubRoutine
'----------------------------------------
'This subroutine writes all scheduled data,
'as stored in the relevant arrays, back to
'the target schedule file.
Sub SaveSchedFileConverted (saveFile$, numberGames)

    Shared homeScores(), visitorScores()
    Shared scheduleAP%(), scheduleNG%()
    Shared scheduleYN$()

    If _FileExists(saveFile$) Then Kill saveFile$

    Open saveFile$ For Random As #1 Len = SCHEDULE_SIZE_BYTES

    For X = 0 To 20
        Field #1, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
    Next

    Field #1, 42 As X$, 2 As Q$(24), 2 As Q$(25), 5 As Q$(22), 5 As Q$(23)

    For I = 1 To numberGames

        For X = 0 To 20
            LSet Q$(X + 1) = MKI$(scheduleNG%(I, X))
        Next

        For X = 0 To 1
            LSet Q$(22 + X) = RTrim$(scheduleYN$(I, X))
        Next

        LSet Q$(24) = MKI$(visitorScores(I))
        LSet Q$(25) = MKI$(homeScores(I))

        Put #1, I

    Next

    Close #1

End Sub


'----------------------------------------
'       ConvertSched4to5 Subroutine
'----------------------------------------
' Reads in existing schedule data.
' For each game in the schedule, it
' will migrate to the new format and
' populate with fixed defaults.
' From there, the updated data is saved.
Sub ConvertSched4to5 (targetFile$, silent)

    Shared scheduleNG%()

    newFile$ = targetFile$ + ".NEW"

    If targetFile$ <> "" Then

        Open targetFile$ For Random As #1
        fileLength& = LOF(1)
        Close #1

        numberGames = fileLength& / (SCHEDULE_SIZE_BYTES - 4)

        Call ReadSchedOld(targetFile$)

        For currGame = 1 To numberGames

            'Assign defaults
            scheduleNG%(currGame, 19) = 0
            scheduleNG%(currGame, 20) = 0

        Next currGame

        'Call SaveSchedFileConverted(newFile$, numberGames)

        'Kill targetFile$

        'Call FCopy(newFile$, targetFile$, Buff$, copyErr%)

        'Kill newFile$

        Kill targetFile$

        Call SaveSchedFileConverted(targetFile$, numberGames)

        If silent = 0 Then
            result& = _MessageBox("Success!", "The schedule file has been converted for use", "ok", "info", 1)
        End If

    End If

End Sub


'----------------------------------------
'       ConvertAllTeam Subroutine
'----------------------------------------
' Quick and dirty script to convert all
' team files in the "Schedule" folder
Sub ConvertAllTeam ()

    fileSpec$ = diskPaths$(0) + "PROBBTMS.*"

    Count% = FileCount%(fileSpec$)
    ReDim foundFiles$(0 To Count%)
    foundFiles$(0) = fileSpec$
    Call ReadFile(foundFiles$())

    For X = 1 To Count%
        target$ = diskPaths$(0) + foundFiles$(X)
        Print "Converting "; target$
        Call ConvertTeam4to5(target$, 1)
    Next X

End Sub


'----------------------------------------
'       ConvertAllSched Subroutine
'----------------------------------------
' Quick and dirty script to convert all
' schedule files in the "Schedule" folder
Sub ConvertAllSched ()

    fileSpec$ = diskPaths$(3) + "*.SCD"

    Count% = FileCount%(fileSpec$)
    ReDim foundFiles$(0 To Count%)
    foundFiles$(0) = fileSpec$
    Call ReadFile(foundFiles$())

    For X = 1 To Count%
        target$ = diskPaths$(3) + foundFiles$(X)
        Print "Converting "; target$
        Call ConvertSched4to5(target$, 1)
    Next X

End Sub

