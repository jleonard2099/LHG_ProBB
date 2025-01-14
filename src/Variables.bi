
Dim opSysType$

Dim Shared diskPaths$(0 To 3), Q$(0 To 378)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

' *** Reading Stat Data ***
' -------------------------
Dim teamNameStats$

Dim gameAttendance&(MAX_SCHED_STATS)

Dim oppScore(MAX_SCHED_STATS), teamScore(MAX_SCHED_STATS)

Dim gameSite$(MAX_SCHED_STATS), locIndicator$(MAX_SCHED_STATS), oppName$(MAX_SCHED_STATS)

Dim stats3FGM!(0 To 14), stats3FGA!(0 To 14)
Dim statsOffTot!(0 To 15), statsDefTot!(0 To 15)
Dim statPlyrVal!(0 To 14, 0 To 24)

Dim gameStatPlyrVal!(0 To 14, 0 To 13)

'Record / Standings
Dim confWins, confLosses
Dim fullWins, fullLosses
Dim homeLosses, homeWins, neutralLosses, neutralWins
Dim roadLosses, roadWins, totalLosses, totalWins
Dim tiedGames

' Conferences / Orgs
Dim memberIdx(TEAMS_PER_LEAGUE)
Dim memberName$(TEAMS_PER_LEAGUE)
Dim memberYears$(TEAMS_PER_LEAGUE)

'-- For Road Data
Dim gameAtt_Road&(MAX_SCHED_STATS)

Dim oppScore_Road(MAX_SCHED_STATS), teamScore_Road(MAX_SCHED_STATS)

Dim gameSite_Road$(MAX_SCHED_STATS), locIndicator_Road$(MAX_SCHED_STATS), oppName_Road$(MAX_SCHED_STATS)


' *** Schedule Data ***
' -------------------------
Dim BS%, NS%

ReDim scheduleAP%(MAX_SCHEDULE_GAMES), scheduleNG%(MAX_SCHEDULE_GAMES, 21)
ReDim homeScores(MAX_SCHEDULE_GAMES), visitorScores(MAX_SCHEDULE_GAMES)

ReDim scheduleH$(MAX_SCHEDULE_GAMES), scheduleV$(MAX_SCHEDULE_GAMES)
'ReDim homeTeam$(MAX_SCHEDULE_GAMES), visitingTeam$(MAX_SCHEDULE_GAMES)
ReDim scheduleYN$(MAX_SCHEDULE_GAMES, 1)
'ReDim yearNumber$(MAX_SCHEDULE_GAMES, 1)


' *** Game Options ***
' -------------------------
Dim Shared DT$, TM$

Dim eventSettings(13)

Dim coachMode$(0 To 1), foulOption$(1), freeThrowOpt$(1)
Dim location$(0 To 2), modeAbbrev$(0 To 3)
Dim playoffGame$(1), playMode$(3)
Dim quarterLength$(0 To 1)
Dim sClockOpt$(0 To 2), teamEraOpt$(0 To 2)
Dim teamIndicator$(1), timeoutSetting$(0 To 4)
Dim yesNo$(0 To 1), yesNoText$(1)


'----------------------------------------
' Used in ALIGN / MERGE routines
'----------------------------------------
Dim plyrStat_Merge!(15, 14)
Dim plyrStat_Align!(15, 14), plyrStat_Road!(15, 14)

Dim threeFGM_Align!(15), threeFGA_Align!(15)
Dim threeFGM_Road!(15), threeFGA_Road!(15)
Dim threeFGM_Merge!(15), threeFGA_Merge!(15)

Dim statsZ_Road!(16), statsDefTot_Road!(16)

Dim plyrName_Road$(15), plyrName_Align$(14)


'----------------------------------------
' Used in CAREER / LEADER routines
'----------------------------------------
'-- 600 = 15 * 40
Dim NB, NP

Dim careerPlayers$(62), BL$(0 To 20)

Dim car3FGM!(MAX_CAREER_YEARS, 62), car3FGA!(MAX_CAREER_YEARS, 62)
Dim carPlyrStat!(MAX_CAREER_YEARS, 62, 15)
Dim leader3FGM!(600), leader3FGA!(600)

Dim carLdrPlyrName$(19, 20)
Dim carLdrVal!(19, 20)

Dim carRecVal!(62), carRecYear!(21)

Dim plyLeaderTeams$(TEAMS_PER_LEAGUE, 15), tmLeaderTeams$(TEAMS_PER_LEAGUE, 15)

Dim plyrLdrStats!(600, 14)
Dim plyrLeaderYears!(TEAMS_PER_LEAGUE), tmLeaderYears!(TEAMS_PER_LEAGUE)
Dim plyrLdrVal!(TEAMS_PER_LEAGUE, 15), teamLdrVal!(TEAMS_PER_LEAGUE, 15)

' For Expanded Leaders
Dim expIndCategory$(0 To 31), expTeamCategory$(0 To 38)
Dim expPlyrLdrName$(600), expPlyrLdrTeam$(600), expLdrTeam$(600), expLdrPlyrName$(600)

Dim totGames!(600), expLdrVal!(600)

'For Expanded Standings
Dim lastTenLosses(TEAMS_PER_LEAGUE), lastTenWins(TEAMS_PER_LEAGUE)

Dim teamLoseStreak(TEAMS_PER_LEAGUE), teamWinStreak(TEAMS_PER_LEAGUE)
Dim teamTotWins(TEAMS_PER_LEAGUE)

Dim totHomeLosses(TEAMS_PER_LEAGUE), totHomeWins(TEAMS_PER_LEAGUE)
Dim totRoadLosses(TEAMS_PER_LEAGUE), totRoadWins(TEAMS_PER_LEAGUE)

Dim totTeamLosses(TEAMS_PER_LEAGUE), lastLoseStreak(TEAMS_PER_LEAGUE), lastWinStreak(TEAMS_PER_LEAGUE)

Dim expStdLeagName$(TEAMS_PER_LEAGUE)


'----------------------------------------
' Used in COMPILE / SEE routines
'----------------------------------------
Dim defTotals!(21), offTotals!(21)
Dim defLeaders!(TEAMS_PER_LEAGUE, 20), offLeaders!(TEAMS_PER_LEAGUE, 20)
Dim leaderVals!(0 To 250, 0 To 2)

Dim compLdrTeam$(TEAMS_PER_LEAGUE)

Dim compLeagWins!(TEAMS_PER_LEAGUE), compLeagLosses!(TEAMS_PER_LEAGUE)
Dim compLgStatVal!(260), compLgStatAtt!(260)
Dim compStats!(14, 26)

Dim defLeaderNames$(TEAMS_PER_LEAGUE), offLeaderNames$(TEAMS_PER_LEAGUE)
Dim statCategoryAbbr$(0 To 25)

Dim leagLdrPlayer$(260), leagLdrTeam$(260)

'----------------------------------------
' Used in DRAFT routines
'---------------------------------------
Dim leagueAverages%(1 To 80, 0 To 5)

Dim tmAtt_DRAFT&(0 To 1)

Dim leagRat_DRAFT(0 To 1, 0 To 6), leagTot_DRAFT(0 To 14, 0 To 34)
Dim plyrRat_DRAFT(0 To 1, 0 To 14, 0 To 19), teamRat_DRAFT(0 To 1, 0 To 34)

Dim draftYears$(1 To 80)
Dim plyrNames_DRAFT$(1, 14, 1)
Dim tmInfo_DRAFT$(1, 4), tmYears_DRAFT$(0 To 1)


'----------------------------------------
' Used in TRADE routines
'----------------------------------------
Dim haveStats(1)

' ** Team File **
Dim tmAtt_TRADE&(0 To 1)

Dim leagRat_TRADE(0 To 1, 0 To 6)
Dim plyrRat_TRADE(0 To 1, 0 To 14, 0 To 19)
Dim tmRat_TRADE(1, 34)

Dim players_TRADE$(0 To 1, 0 To 14, 0 To 1)
Dim tmInfo_TRADE$(1, 6)

' ** Stat File **

Dim teamIndexes(1)
Dim tradeStatsOffTot!(2, 15), tradeStatsDefTot!(2, 15)

Dim tradeStatPlyrVal!(2, 14, 14)
Dim trade3FGM!(2, 14), trade3FGA!(2, 14)

Dim tradeStatsPlyrName$(2, 14), tradeStatsTeamName$(1)
Dim statTeam$(1)


'----------------------------------------
'   Used in HD2HD routines
'----------------------------------------
Dim leagueName$, div1Name$, div2Name$, div3Name$, div4Name$

Dim awayLosses(50), awayWins!(50)
Dim awayScoreTeam!(50), awayScoreOpp!(50)

Dim homeLosses!(50), homeWins!(50)
Dim homeScoreTeam!(50), homeScoreOpp!(50)

Dim totAwayLosses!(40), totAwayWin!(40)
Dim totAwayScoreTeam!(40), totAwayScoreOpp!(40)
Dim totHomeLosses!(40), totHomeWin!(40)
Dim totHomeScoreTeam!(40), totHomeScoreOpp!(40)


'----------------------------------------
' Used in RECORD routines
'----------------------------------------
Dim plyrRecords!(1 To 25), offRecords!(1 To 21), defRecords!(1 To 21)
Dim plyrRecDesc$(25, 1), offRecDesc$(1 To 21), defRecDesc$(1 To 21)

Dim indRecords!(50, 2), teamRecords!(125, 2)
Dim indRecordDesc$(25), teamRecordDesc$(20)
Dim indRecDesc$(50, 4), teamRecDesc$(125, 3)


'----------------------------------------
' Used in Game Routines
'----------------------------------------
Dim scheduleFile$

Dim actualAttendance&
Dim avgAttendance&(1)

Dim tickerStart

Dim alpha$(4), tickerPeriod$(14)

Dim Shared D, P, P9
Dim Shared assistShotBoost, autoPlay, ballCarrier, bonusFoulNum
Dim Shared coachOpt, compTeam, currGame
Dim Shared defPress, gameOver, endAllGames, eraAdj, eraChoice
Dim Shared fastBreak, forcedThree, ftRulesOpt, freeThrowVal, fullCtOpt
Dim Shared gameLoc, halfTime, JY, maxTimeouts, MJ, nbrFTMade, nbrLines
Dim Shared offStatus, playerMode, playerOpt, playoffOpt, pbpOpt, playChoice
Dim Shared quarter, quarterOpt, rebFoulChance
Dim Shared sClockVal, shotClock, shotPctOpt, shotType
Dim Shared threePtOpt, T1, timeoutOpt, TMT, twoTimeouts, willDunk

Dim Shared GF%, HH%, J8%, OX%, OY%
Dim Shared MINS_PER_GAME

Dim Shared gameClock!, pbpDelay!, shotChance, timeElapsed!

Dim Shared A1$, B1$, C1$, D1$, E1$, F1$, G1$, H1$
Dim Shared activePosition$, pbpString$, prevBall$, TS$

Dim Shared CF%(1, 9)
Dim Shared F%(1, 9), G9%(1), GF%(2, 9)
Dim Shared NG%(21), N0%(2, 2, 4)
Dim Shared OX%(2), penetrationOffAdj(2), offStyles(1)
Dim Shared ST%(32), SX%(32, 1, 14)

Dim Shared C1(1, 14)
Dim Shared D8(6, 6), dFGPA(1), defStyles(1), F1(14), fullCtOpt(0 To 1)
Dim Shared G4(14), G5(14), leagRat_GAME(1, 6)
Dim Shared gameStat3FGM(1, 14), gameStat3FGA(1, 14)
Dim Shared mandatoryTO(0 To 1), miscAdj(1, 4)
Dim Shared P4(5), P5(5), pbpFG(1 To 8), pbpBG(1 To 8), perFoulAdj(0 To 32)
Dim Shared plyrRat_GAME(1, 14, 19)
DIm Shared QQ(1, 8, 14, 14), QR(1, 7, 14), rebRatings(0 To 10)
Dim Shared rosterStatus(1, 13)
Dim Shared S3(14), score(1, 14), schedGame(2), scSettings(0 To 2), statTotals(14)
Dim Shared teamAdj(1, 9), teamFouls(1), teamRat_GAME(0 To 1, 0 To 34), teamYears(1)
Dim Shared timePlayed(2, 14), timeouts(1), totalFouls(1), threeFG(1, 14, 1)
Dim Shared totRebounds(1), turnoverAdj(0 To 32), turnovers(1)
Dim Shared X7(1), teamRecWins(1), teamRecLoss(1)

'Dim Shared gameRatings!(0 To 1, 0 To 14, 0 To 25), lineupIdx(0 To 1, 0 To 4), M9!(1)
Dim game3FGM!(1, 14), game3FGA!(1, 14)

Dim Shared defStyleDesc$(9), diskIDs$(0 To 1)
Dim Shared gameCoach$(3), gameMascots$(3), gameArena$(3), gameTeams$(3)
Dim Shared offStyleDesc$(2), pbpStyle$(1), players_GAME$(1, 14, 1), posnName$(0 To 4)
Dim Shared SX$(32, 1)
Dim Shared teamAbbrev$(3), teamGender$(0 To 1)
Dim Shared Y$(1)
