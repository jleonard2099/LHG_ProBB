'----------------------------------------
' Used in more than one source file
'----------------------------------------
Dim opSysType$

Dim Shared diskPaths$(0 To 3), Q$(0 To 377)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

' *** Reading Stat Data ***
' -------------------------
Dim ORD%(0 To NUM_STATRECORDS), TRD%(0 To NUM_STATRECORDS)
Dim statO%(0 To NUM_STATRECORDS), statsL%(0 To NUM_STATRECORDS), statsT%(0 To NUM_STATRECORDS)

Dim statsW0!(0 To 14), statsW1!(0 To 14)
Dim statsZ!(0 To 15), statsZ1!(0 To 15), statsZ2!(0 To 14, 0 To 13)

Dim HRD$(0 To NUM_STATRECORDS), ORD$(0 To NUM_STATRECORDS)
Dim HO$(NUM_STATRECORDS), statsH$(0 To NUM_STATRECORDS), statsO$(0 To NUM_STATRECORDS)
Dim statsZ1$(TEAMS_PER_CONFERENCE)

' *** Schedule Data ***
' -------------------------
Dim homeScores(MAX_GAMES), visitorScores(MAX_GAMES)

' *** Game Options ***
' -------------------------
Dim Shared DT$, TM$

Dim coachMode$(0 To 1), foulOption$(1), freeThrowOpt$(1)
Dim location$(0 To 2), modeAbbrev$(0 To 3)
Dim playoffGame$(1), playMode$(3)
Dim sClockOpt$(0 To 2), teamEraOpt$(0 To 2)
Dim teamIndicator$(1)
Dim yesNo$(0 To 1), yesNoText$(1)


' *** Miscellaneous Use ***
' -------------------------
Dim Shared BO%

Dim compareA!(15, 14)

'----------------------------------------
' Used in ALIGN / MERGE routines
'----------------------------------------
Dim Ycurr%, Yroad%

Dim roadPlyrNames$(15)
Dim AN$(0 To 14)

Dim AN!(15, 14), AR!(15, 14), ARD!(0 To 15, 0 To 14)
Dim W0N!(15), WN1!(15)
Dim W0RD!(0 To 15), W1RD!(0 To 15)
Dim W0R!(15), WR1!(15)
Dim ZR!(15), ZR1!(15)
Dim ZRD!(0 To 16), ZRD1!(0 To 16)


'----------------------------------------
' Used in CAREER routines
'----------------------------------------
'---> should we need these ?!?!
Dim ars!(15, 62, 15), W0S!(15, 62), W1S!(15, 62)
Dim AR$(62)


'----------------------------------------
' Used in CREATE routines
'----------------------------------------
Dim intZ1%(TEAMS_PER_CONFERENCE)
Dim memberTeams$(TEAMS_PER_CONFERENCE)
Dim memberYears$(TEAMS_PER_CONFERENCE)
Dim statNames$(TEAMS_PER_CONFERENCE)


'----------------------------------------
' Used in DRAFT routines
'----------------------------------------
Dim leagRat_DRAFT%(0 To 1, 0 To 6), leagTot_DRAFT(0 To 14, 0 To 34)
Dim plyrRat_DRAFT%(0 To 1, 0 To 14, 0 To 19), tmRat_DRAFT%(0 To 1, 0 To 34)

Dim players_DRAFT$(1, 14, 1), tmInfo_DRAFT$(1, 4)

Dim draftYears$(1 To 79)
Dim leagueAverages%(1 To 79, 0 To 5)
Dim teamYears$(0 To 1)

'----------------------------------------
' Used in TRADE routines
'----------------------------------------
Dim haveStats(1)

' ** Team File **
Dim leagRat_TRADE(0 To 1, 0 To 6)
Dim plyrRat_TRADE(0 To 1, 0 To 14, 0 To 19)
Dim teamSettings%(1, 34)

Dim players_TRADE$(0 To 1, 0 To 14, 0 To 1)
Dim tmInfo_TRADE$(1, 6)

' ** Stat File **

Dim tradeZZ(2, 15), tradeZZ1(2, 15)

Dim tradeB1!(2, 14, 14), tradeT!(1)
Dim tradeW0!(2, 14), tradeW1!(2, 14)

Dim B1$(2, 14), A1$(1)
Dim statFiles$(1), statTeam$(1)


'----------------------------------------
'   Used in Head-To-Head routines
'----------------------------------------
Dim AL!(50), AW!(50), HL!(50), HW!(50)
Dim R1!(50), R2!(50), R3!(50), R4!(50)
Dim TAL!(40), TAW!(40), THL!(40), THW!(40)
Dim TR1!(40), TR2!(40), TR3!(40), TR4!(40)

Dim league$, div1$, div2$, div3$, div4$

'----------------------------------------
' Used in STAT / INPUT routines
'----------------------------------------
Dim leagueT%(100)

Dim DL!(40, 20), LD!(250, 2), leagS!(14, 26)
Dim DT!(21), OT!(21)
Dim O1!(40), O2!(40), OL!(40, 20)
Dim seeZ!(260), seeZ1!(260)
Dim values!(0 To 14, 0 To 24)

Dim categories$(0 To 25), statPlyrNames$(0 To 14)

Dim DL$(40), N$(14), Z2$(260), Z3$(260)
Dim H1$(40), OL$(40), leagT$(40)

Dim AL%, AW%, HL%, HW%, L%, NL%, NW%, VL%, VW%, W%, Z%

'----------------------------------------
' Used in LEADER routines
'----------------------------------------
Dim HLL(TEAMS_PER_CONFERENCE), HWW(TEAMS_PER_CONFERENCE)
Dim LL(TEAMS_PER_CONFERENCE), LLS(TEAMS_PER_CONFERENCE), LWS(TEAMS_PER_CONFERENCE)
Dim LTL(TEAMS_PER_CONFERENCE), LTW(TEAMS_PER_CONFERENCE)
Dim RLL(TEAMS_PER_CONFERENCE), RWW(TEAMS_PER_CONFERENCE)
Dim TLS(TEAMS_PER_CONFERENCE), TWS(TEAMS_PER_CONFERENCE), WW(TEAMS_PER_CONFERENCE)

Dim plyrStatLeaders!(600, 14)
Dim GM!(40), GM1!(40)
Dim GMA!(600), TYP!(600)
Dim TT!(40, 15), TT1!(40, 15)
Dim W0L!(600), W1L!(600)

Dim AL$(600)
Dim TMA$(600), TML$(TEAMS_PER_CONFERENCE)
Dim TP$(0 To 31), TP1$(0 To 38)
Dim TPP$(600), TMM$(600)
Dim TT$(40, 15), TT1$(40, 15)


'----------------------------------------
' Used in RECORD routines
'----------------------------------------
Dim teamRecords$(20), indRecords$(25)
Dim BRC$(25, 1), TRC$(21), TRC1$(21)
Dim BRC!(1 To 25), TRC!(21), TRC1!(21)

Dim indRecCategory$(50, 4), teamRecCategory$(125, 3)
Dim allRecords!(50, 2), teamRecords!(125, 2)


'----------------------------------------
' Used in SCHEDULE routines
'----------------------------------------
Dim BS%, NS%

Dim scheduleAP%(1), scheduleNG%(MAX_GAMES, 18)

ReDim scheduleH$(0 To 20), scheduleV$(0 To 20)
Dim scheduleYN$(MAX_GAMES, 1)


'----------------------------------------
' Used in Game Routines
'----------------------------------------
Dim tickerStart

Dim scheduleFile$

Dim actualAttendance&
Dim avgAttendance&(1)

Dim Shared autoPlay, ballCarrier, bonusFoulNum, coachOpt, compTeam
Dim Shared D, endGame, endAllGames, ftRulesOpt, F3, freeThrowVal
Dim Shared gameLoc, halfTime, JY, nbrLines
Dim Shared P, P7, P9, playerMode, playerOpt, playoffOpt, pbpOpt
Dim Shared quarter, sClockVal, shotClock, shotPctOpt

'I have no clue what xFactor (previously "X") does
Dim Shared threePtOpt, T1, TMT, xFactor

Dim Shared DK%, FB%, FT%, GF%, HH%
Dim Shared NM%, OX%, OY%, PT%, PZ%

Dim Shared gameClock!, pbpDelay!, timeElapsed!

Dim Shared A1$, B1$, C1$, D1$, E1$, F1$, G1$, H1$
Dim Shared P7$, prevBall$, PS$, TS$, pbpString$, Y2$, Z3$

Dim Shared CF%(1, 9), eventSettings(13)
Dim Shared F%(1, 9), F5%(1, 4), F7%(1, 9), FY%(0 To 1), G9%(1), GF%(2, 9)
Dim Shared HT%(100), NG%(18), N0%(2, 2, 4)
Dim Shared OX%(2), OY%(2), O0%(1)
Dim Shared PF%(1), PFA%(33), ST%(32), SX%(32, 1, 14)
Dim Shared TOA%(33), TOF%(1), W%(1, 14, 1), YR%(1)

'Dim Shared C1(1, 14)
Dim Shared D1(1), D2(1), D8(6, 6), F1(14), G4(14), G5(14)
Dim Shared leagRat_GAME(1, 6), plyrRat_GAME(1, 14, 19)
Dim Shared P2(1), P4(5), P5(5), P7(1), QQ(1, 8, 14, 14), QR(1, 7, 14)
Dim Shared R3(1), RB(0 To 10), S3(14), scSettings(0 To 2)
Dim Shared score(1, 14), schedGame(2)
Dim Shared timePlayed(2, 14), timeouts(1), tmRat_GAME(1, 34)
'                   teamFouls
Dim Shared turnovers(1), teamFouls(1)
Dim Shared statTotals(14), pbpFG(9), pbpBG(9), W2(1, 14), W3(1, 14)
Dim Shared X7(1), Z5(1), Z6(1)

Dim Shared gameB1!(0 To 1, 0 To 4), gameRatings!(0 to 1, 0 to 14, 0 to 25)
Dim Shared indRecords!(50, 2), M9!(1)
Dim gameW0!(1, 14), gameW1!(1, 14)

Dim alpha$(4)
Dim Shared defenseStyles$(5)
Dim Shared gameCoach$(3), gameMascots$(3), gameStadium$(3), gameTeams$(3)
Dim Shared offenseStyles$(2), pbpType$(1), players_GAME$(1, 14, 1), PS$(4)
Dim Shared RC$(50, 4), SX$(32, 1)
Dim Shared teamAbbrev$(3), tickerPeriod$(14), Y$(1), YN$(0 To 1)
