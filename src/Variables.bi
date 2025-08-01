'----------------------------------------
' Used in more than one source file
'----------------------------------------
Dim Shared menu&
Dim opSysType$
Dim Shared dirSeparator$

Dim Shared diskPaths$(0 To 3), Q$(0 To 378)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex(MAX_TEAMS)

' *** Reading Stat Data ***
' -------------------------
Dim teamNameStats$

Dim gameAttendance&(MAX_SCHED_STATS)

Dim oppScore(MAX_SCHED_STATS), teamScore(MAX_SCHED_STATS)

Dim gameSite$(MAX_SCHED_STATS), locIndicator$(MAX_SCHED_STATS), oppName$(MAX_SCHED_STATS)

Dim stats3FGM!(0 To 14), stats3FGA!(0 To 14)
Dim statsOffTot!(0 To 15), statsDefTot!(0 To 15)
Dim statPlyrVal!(0 To 14, 0 To 26)

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
Dim memberYear$(TEAMS_PER_LEAGUE)

'-- For Road Data
Dim gameAtt_Road&(MAX_SCHED_STATS)

Dim oppScore_Road(MAX_SCHED_STATS), teamScore_Road(MAX_SCHED_STATS)

Dim gameSite_Road$(MAX_SCHED_STATS), locIndicator_Road$(MAX_SCHED_STATS), oppName_Road$(MAX_SCHED_STATS)


' *** Schedule Data ***
' -------------------------
Dim BS%, NS%

ReDim scheduleAP%(MAX_SCHEDULE_GAMES), scheduleNG%(MAX_SCHEDULE_GAMES, 22)
ReDim homeScores(MAX_SCHEDULE_GAMES), visitorScores(MAX_SCHEDULE_GAMES)

ReDim scheduleH$(MAX_SCHEDULE_GAMES), scheduleV$(MAX_SCHEDULE_GAMES)
ReDim scheduleYN$(MAX_SCHEDULE_GAMES, 1)
'ReDim yearNumber$(MAX_SCHEDULE_GAMES, 1)

'-- These are used for tournaments
'ReDim homeTeam$(MAX_SCHEDULE_GAMES), visitingTeam$(MAX_SCHEDULE_GAMES)


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

Dim plyLeaderTeams$(40, 15), tmLeaderTeams$(40, 15)

Dim plyrLdrStats!(0 To 600, 0 To 14)
Dim plyrLeaderGames!(40), tmLeaderGames!(40)
Dim plyrLdrVal!(40, 15), teamLdrVal!(40, 15)

' For Expanded Leaders
Dim expIndCategory$(0 To 31), expTeamCategory$(0 To 38)
Dim expPlyrLdrName$(600), expPlyrLdrTeam$(600), expLdrTeam$(600), expLdrPlyrName$(600)

Dim totGames!(600), expLdrVal!(600)

'For Expanded Standings
Dim lastLoseStreak(TEAMS_PER_LEAGUE), lastWinStreak(TEAMS_PER_LEAGUE)
Dim lastTenLosses(TEAMS_PER_LEAGUE), lastTenWins(TEAMS_PER_LEAGUE)

Dim teamLoseStreak(TEAMS_PER_LEAGUE), teamWinStreak(TEAMS_PER_LEAGUE)

Dim totHomeLosses(TEAMS_PER_LEAGUE), totHomeWins(TEAMS_PER_LEAGUE)
Dim totAwayLosses(TEAMS_PER_LEAGUE), totAwayWins(TEAMS_PER_LEAGUE)
Dim totTeamLosses(TEAMS_PER_LEAGUE), totTeamWins(TEAMS_PER_LEAGUE)

Dim expStdLeagName$(TEAMS_PER_LEAGUE)


'----------------------------------------
' Used in COMPILE / SEE routines
'----------------------------------------
Dim defTotals!(21), offTotals!(21)
Dim defLeaders!(TEAMS_PER_LEAGUE, 20), offLeaders!(TEAMS_PER_LEAGUE, 20)
Dim leaderVals!(0 To 250, 0 To 2)

Dim compLdrTeam$(TEAMS_PER_LEAGUE)

Dim divWins(TEAMS_PER_LEAGUE), divLosses(TEAMS_PER_LEAGUE)
Dim compLgStatVal!(260), compLgStatAtt!(260)
Dim compStats!(14, 26)

Dim defLeaderTeams$(TEAMS_PER_LEAGUE), offLeaderNames$(TEAMS_PER_LEAGUE)
Dim statCategoryAbbr$(0 To 25)

Dim leagLdrPlayer$(260), leagLdrTeam$(260)

'----------------------------------------
' Used in DRAFT routines
'---------------------------------------
Dim leagueAverages%(1 To DRAFT_NBR_YEARS, 0 To 5)

Dim tmAtt_DRAFT&(0 To 1)

Dim leagRat_DRAFT(0 To 1, 0 To 6), leagTot_DRAFT(0 To 14, 0 To 34)
Dim plyrRat_DRAFT(0 To 1, 0 To 14, 0 To 19), teamRat_DRAFT(0 To 1, 0 To 34)

Dim draftYears$(1 To DRAFT_NBR_YEARS)
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

Dim awayLosses(50), awayWins(50)
Dim awayScoreTeam!(50), awayScoreOpp!(50)

Dim homeLosses(50), homeWins(50)
Dim homeScoreTeam!(50), homeScoreOpp!(50)

Dim totAwayScoreTeam!(40), totAwayScoreOpp!(40)
Dim totHomeScoreTeam!(40), totHomeScoreOpp!(40)


'----------------------------------------
' Used in RECORD routines
'----------------------------------------
Dim plyrRecords!(1 To 25), offRecords!(1 To 21), defRecords!(1 To 21)
Dim plyrRecDesc$(25, 1), offRecDesc$(1 To 21), defRecDesc$(1 To 21)

Dim indRecords!(0 To 48, 0 To 2), teamRecords!(0 To 125, 0 To 2)
Dim indRecDesc$(0 To 48, 0 To 4), teamRecDesc$(125, 0 To 3)
Dim indRecordDesc$(25), teamRecordDesc$(20)


'----------------------------------------
' Used in Game Routines
'----------------------------------------
Dim scheduleFile$

Dim actualAttendance&

Dim JY, tickerStart

Dim alpha$(4), tickerPeriod$(14)

Dim avgAttendance&(1)

' Shared / Global
Dim Shared activePosition$, pbpString$, prevBall$

Dim Shared C1, D, MJ, P, P9

Dim Shared assistShotBoost, autoPlay, ballCarrier, bonusFoulNum
Dim Shared coachOpt, compTeam, currGame, defPress
Dim Shared gameOver, earlyExit, endAllGames, eraAdj, eraChoice
Dim Shared ft3PtFoul, fastBreak, forcedThree, foulPlayer
Dim Shared freeThrowVal, ftRulesOpt, fullCtOpt
Dim Shared gameLoc, goaltendTOs
Dim Shared halfTime, maxTimeouts, nbrLines, offStatus
Dim Shared passes, periodEnd, playerMode, playerOpt, playoffOpt, pbpOpt
Dim Shared quarter, quarterOpt
Dim Shared rebFoulChance, ruleOptType
Dim Shared sClockVal, shotChance, shotClock, shotPctOpt, shotType, sndOpt
Dim Shared threePtOpt, threePtRange, tickerGames, timeoutOpt, twoTimeouts, willDunk


Dim Shared MINS_PER_GAME

Dim Shared gameClock!, pbpDelay!, possTime!, timeElapsed!

Dim Shared pbpText$(1 To MAX_PBP_LINES)

Dim Shared ST%(32), SX%(32, 1, 14)

Dim Shared d3FG_Pct_Adj(1), dFG_Pct_Adj(1)
Dim Shared defStyles(1), fullCtOpt(0 To 1)

Dim Shared gameStat3FGM(1, 14), gameStat3FGA(1, 14)
Dim Shared leagRat_GAME(1, 6), lineupIdx(0 To 1, 0 To 4)
Dim Shared mandatoryTO(0 To 1), miscAdj(1, 4), offStyles(1)
Dim Shared pbpFG(1 To MAX_PBP_LINES), pbpBG(1 To MAX_PBP_LINES)
Dim Shared penetrationDefAdj(2, 9), penetrationOffAdj!(0 To 8)
Dim Shared periodPlayerTot(1, 8, 14, 14), periodTeamTot(1, 7, 14)
Dim Shared perFoulAdj(0 To 32), plyrRat_GAME(1, 14, 19)
Dim Shared plyrRebIdx(14), plyrRebVal(14), plyrStaminaGame(14)
Dim Shared rebRatings(0 To 10), rosterIdx(14), rosterStatus(1, 13)
Dim Shared schedGame(2), schedOptions(21)
Dim Shared score(1, 14), scSettings(0 To 2), statTotals(14)
Dim Shared teamAdj(1, 9), teamFouls(1)
Dim Shared teamRat_GAME(0 To 1, 0 To 34), teamYears(1)
Dim Shared threeFG(1, 14, 1), timeouts(1), timePlayed(2, 0 To 14)
Dim Shared totalFouls(1), teamRebounds(1), turnoverAdj(0 To 32), turnovers(1)
Dim Shared teamRecWins(1), teamRecLoss(1)

Dim Shared defStealAdj!(1 To 9), gameRatings!(0 To 1, 0 To 14, 0 To 25)
Dim Shared offRebPct!(0 To 1), offStealAdj!(0 To 8), rebAdj!(1)

Dim game3FGM!(1, 14), game3FGA!(1, 14)

Dim Shared defStyleDesc$(9), diskIDs$(0 To 1)
Dim Shared gameCoach$(3), gameMascots$(3), gameArena$(3), gameTeams$(3)
Dim Shared offStyleDesc$(2), offStylesNew$(0 To 8)
Dim Shared pbpStyle$(1), players_GAME$(1, 14, 1), posnName$(0 To 4)

Dim Shared SX$(32, 1)
Dim Shared teamAbbrev$(3), teamGender$(0 To 1), teamLeague$(0 To 1)
