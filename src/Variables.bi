
Dim opSysType$

Dim Shared diskPaths$(0 To 3), Q$(0 To 378)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

' *** Reading Stat Data ***
' -------------------------
Dim gameAttendance&(MAX_SCHED_STATS)

Dim oppScore(MAX_SCHED_STATS), teamScore(MAX_SCHED_STATS)

Dim gameSite$(MAX_SCHED_STATS), locIndicator$(MAX_SCHED_STATS), oppName$(MAX_SCHED_STATS)

Dim statsW0!(0 To 14), statsW1!(0 To 14)
Dim statsZ!(0 To 15), statsZ1!(0 To 15)
Dim statsZ2!(0 To 14, 0 To 13)
Dim values!(0 To 14, 0 To 24)

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
Dim statFiles$(TEAMS_PER_LEAGUE)

'-- For Road Data
Dim gameAtt_Road&(MAX_SCHED_STATS)

Dim oppScore_Road(MAX_SCHED_STATS), teamScore_Road(MAX_SCHED_STATS)

Dim gameSite_Road$(MAX_SCHED_STATS), locIndicator_Road$(MAX_SCHED_STATS), oppName_Road$(MAX_SCHED_STATS)


' *** Schedule Data ***
' -------------------------
Dim BS%, NS%

ReDim scheduleAP%(MAX_SCHEDULE_GAMES), scheduleNG%(MAX_SCHEDULE_GAMES, 20)
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
Dim teamIndicator$(1), timeoutSetting$(0 To 2)
Dim yesNo$(0 To 1), yesNoText$(1)


'----------------------------------------
' Used in ALIGN / MERGE routines
'----------------------------------------
Dim ARD!(15, 14)

Dim AN!(15, 14), AR!(15, 14)
Dim W0N!(15), WN1!(15)
Dim W0R!(15), WR1!(15)
Dim W0RD!(15), W1RD!(15)
Dim ZR!(15), ZR1!(15)
Dim ZRD!(16), ZRD1!(16)

Dim roadPlyrNames$(15), alignName$(14)


'----------------------------------------
' Used in CAREER / LEADER routines
'----------------------------------------
Dim ARS!(15, 62, 15)
Dim BL!(62), BLYR!(21)
Dim W0S!(15, 62), W1S!(15, 62)

Dim plyrStatLeaders!(600, 14)
Dim plyrLeaderYears!(TEAMS_PER_LEAGUE), tmLeaderYears!(TEAMS_PER_LEAGUE)
Dim TT!(TEAMS_PER_LEAGUE, 15), TT1!(TEAMS_PER_LEAGUE, 15)
Dim GMA!(600), TYP!(600)
Dim W0L!(600), W1L!(600)

Dim AL$(600), AR$(62), BL$(0 To 20)
Dim expIndCategory$(0 To 31), expTeamCategory$(0 To 38)
Dim plyLeaderTeams$(TEAMS_PER_LEAGUE, 15), tmLeaderTeams$(TEAMS_PER_LEAGUE, 15)
Dim TMA$(600)
Dim TMM$(600), TPP$(600)

'For Expanded Standings
Dim HLL(TEAMS_PER_LEAGUE), HWW(TEAMS_PER_LEAGUE)
Dim LL(TEAMS_PER_LEAGUE), LLS(TEAMS_PER_LEAGUE), LWS(TEAMS_PER_LEAGUE)
Dim LTL(TEAMS_PER_LEAGUE), LTW(TEAMS_PER_LEAGUE)
Dim RLL(TEAMS_PER_LEAGUE), RWW(TEAMS_PER_LEAGUE)
Dim TLS(TEAMS_PER_LEAGUE), TWS(TEAMS_PER_LEAGUE), WW(TEAMS_PER_LEAGUE)

Dim TML$(TEAMS_PER_LEAGUE)


'----------------------------------------
' Used in COMPARE / COMPILE / SEE
'----------------------------------------
Dim defTotals!(21), offTotals!(21)
Dim leaderVals!(0 To 250, 0 To 2)
Dim plyrRat_COMP!(14, 26)

Dim defLeaders!(TEAMS_PER_LEAGUE, 20), offLeaders!(TEAMS_PER_LEAGUE, 20)
Dim O1!(TEAMS_PER_LEAGUE), O2!(TEAMS_PER_LEAGUE)
Dim seeZ!(260), seeZ1!(260)

Dim defLeaderNames$(TEAMS_PER_LEAGUE)
Dim offLeaderNames$(TEAMS_PER_LEAGUE)
Dim statCategoryAbbr$(0 To 25)
Dim Z2$(260), Z3$(260)

Dim seeT$(TEAMS_PER_LEAGUE)

'----------------------------------------
' Used in DRAFT routines
'---------------------------------------
Dim leagueAverages%(1 To 80, 0 To 5)

Dim tmAtt_DRAFT&(0 To 1)

Dim leagRat_DRAFT(0 To 1, 0 To 6), leagTot_DRAFT(0 To 14, 0 To 34)
Dim plyrRat_DRAFT(0 To 1, 0 To 14, 0 To 19), teamRat_DRAFT(0 To 1, 0 To 34)

Dim draftYears$(1 To 80)
Dim plyrNames_DRAFT$(1, 14, 1), tmInfo_DRAFT$(1, 4)
Dim tmYears_DRAFT$(0 To 1)


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

Dim tradeZZ0(2, 15), tradeZZ1(2, 15)

Dim tradeB1!(2, 14, 14), teamIndexes(1)
Dim tradeW0!(2, 14), tradeW1!(2, 14)

Dim B1$(2, 14), A1$(1)

Dim statTeam$(1)


'----------------------------------------
'   Used in HD2HD routines
'----------------------------------------
Dim league$, div1Name$, div2Name$, div3Name$, div4Name$

Dim awayLosses(50), awayWins!(50)
Dim homeLosses!(50), homeWins!(50)
Dim homeScoreTeam!(50), homeScoreOpp!(50)
Dim awayScoreTeam!(50), awayScoreOpp!(50)
Dim totAwayLosses!(40), totAwayWin!(40)
Dim totHomeLosses!(40), totHomeWin!(40)
Dim totHomeScoreTeam!(40), totHomeScoreOpp!(40)
Dim totAwayScoreTeam!(40), totAwayScoreOpp!(40)



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
Dim Shared defPress, endGame, endAllGames
Dim Shared fastBreak, forcedThree, ftRulesOpt, freeThrowVal, fullCtOpt
Dim Shared gameLoc, halfTime, JY, MJ, nbrFTMade, nbrLines, offStatus
Dim Shared playerMode, playerOpt, playoffOpt, pbpOpt, playChoice
Dim Shared quarter, quarterOpt, rebFoulChance
Dim Shared sClockVal, shotClock, shotPctOpt, shotType
'Dim Shared threePtOpt, T1, timeoutOpt, TMT, twoTimeouts, willDunk

Dim Shared GF%, HH%, J8%, OX%, OY%

Dim Shared gameClock!, pbpDelay!, shotChance, timeElapsed!

Dim Shared A1$, B1$, C1$, D1$, E1$, F1$, G1$, H1$
Dim Shared activePosition$, pbpString$, prevBall$, TS$

Dim Shared CF%(1, 9)
Dim Shared F%(1, 9), G9%(1), GF%(2, 9)
Dim Shared NG%(18), N0%(2, 2, 4)
Dim Shared OX%(2), penetrationOffAdj(2), offStyles(1)
Dim Shared ST%(32), SX%(32, 1, 14)

'Dim Shared C1(1, 14)
Dim Shared D8(6, 6), dFGPA(1), defStyles(1), F1(14), fullCtOpt(0 To 1)
Dim Shared G4(14), G5(14), leagRat_GAME(1, 6), lineupIdx(0 To 1, 0 To 4)
'Dim Shared mandatoryTO(0 To 1), miscAdj(1, 4)
Dim Shared P4(5), P5(5), pbpFG(1 To 8), pbpBG(1 To 8), perFoulAdj(0 To 32)
Dim Shared plyrRat_GAME(1, 14, 19), QQ(1, 8, 14, 14), QR(1, 7, 14), rebRatings(0 To 10)
Dim Shared S3(14), score(1, 14), schedGame(2), scSettings(0 To 2), statTotals(14)
Dim Shared teamAdj(1, 9), teamFouls(1), teamRat_GAME(0 To 1, 0 To 34), teamYears(1)
Dim Shared timePlayed(2, 14), timeouts(1), totalFouls(1), threeFG(1, 14, 1)
Dim Shared totRebounds(1), turnoverAdj(0 To 32), turnovers(1)
Dim Shared W2(1, 14), W3(1, 14), X7(1), teamRecWins(1), teamRecLoss(1)

Dim Shared gameRatings!(0 To 1, 0 To 14, 0 To 25), M9!(1)
Dim gameW0!(1, 14), gameW1!(1, 14)

Dim Shared defStyleDesc$(5), diskIDs$(0 To 1)
Dim Shared gameCoach$(3), gameMascots$(3), gameArena$(3), gameTeams$(3)
Dim Shared offStyleDesc$(2), pbpStyle$(1), players_GAME$(1, 14, 1), posnName$(0 To 4)
Dim Shared SX$(32, 1)
Dim Shared teamAbbrev$(3), teamGender$(0 To 1)
Dim Shared Y$(1)
