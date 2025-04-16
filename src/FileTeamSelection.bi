Declare Sub ChooseTeam (yearNumber$, numberTeams, chosenIdx, teamName$)
Declare Sub SelectTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%)
Declare Sub ReadTeam (teamYear$, targetTeam$, teamIdx, dataOK)
Declare Sub UserFilePrompt (reportFile$)
Declare Sub ScheduleFilePrompt (scheduleFile$)

Declare Sub ReadTeamStats (teamName$, statFile$)
Declare Sub ReadScheduleStats (statFile$, numberGames)
Declare Sub SaveScheduleStats (saveFile$, numberGames)
Declare Sub ReadSeasonRecords (recordFile$)
Declare Sub ReadGameRecords (recordFile$)
Declare Sub ReadLeagueIndex (indexFile$)
Declare Sub ReadLeagueStats (statFile$)

Declare Function nameStatFile$ (teamName$)
Declare Function CheckString% (targetString$)
Declare Function ChooseFile$(fileSpec$, fileDesc$)

Const TEAM_SIZE_BYTES = 1035
Const TEAM_FILE_NAME$ = "PROBBTMS"

Dim previousYear$

'-- transition away from this
Dim Shared fileLength&

Dim CK

Dim teamAttendance&

Dim leagueRatings(0 To 6), teamRatings(0 To 34)

Dim playerRatings!(0 To 14, 0 To 19)

Dim playerNames$(0 To 14, 0 To 1), teamInfo$(4)
Dim statPlyrNames$(0 To 15)
