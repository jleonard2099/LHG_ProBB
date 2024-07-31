Declare Sub ChooseTeam (yearNumber$, numberTeams, teamIdx)
Declare Sub SelectTeam (selectedIdx)
Declare Sub ReadTeam (teamYear$, targetTeam$, teamIdx)
Declare Sub ReadExtraData (teamYear$, targetTeam$, teamIdx)
Declare Sub UserFilePrompt (reportFile$)

Declare Function nameStatFile$ (teamName$)
Declare Function ChooseFile$(fileSpec$, fileDesc$)

Const TEAM_SIZE_BYTES = 1035
Const TEAM_FILE_NAME$ = "PROBBTMS"

Dim previousYear$

'-- transition away from this
Dim Shared fileLength&

Dim leagueRatings(0 To 6), teamRatings(0 To 34)

Dim playerRatings!(0 To 14, 0 To 19)

Dim playerNames$(0 To 14, 0 To 1), teamInfo$(4)
