'----------------------------------------
' Used across more than one source file
'----------------------------------------
Dim opSysType$

Dim Shared BO%
Dim Shared TMR%, TMR1%

Dim Shared fileLength&

Dim Shared A$, YN$, Z1$
Dim Shared DT$, TM$

Dim Shared compareA!(15, 14)
Dim Shared scheduleT!(0 To 34)

Dim Shared Z1$(40)

Dim Shared diskPaths$(3), Q$(0 To 377)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

' Game options
Dim Shared foulOption$(1), freeThrowOpt$(1)
Dim Shared location$(2), playoffGame$(1), TS$(1)

Dim coachOpt$(0 To 1)
Dim modeAbbrev$(0 To 3), MO$(3)
Dim sClockOpt$(0 To 1), teamEraOpt$(0 To 2)
Dim yesNo$(0 To 1)


'----------------------------------------
' Used across ALIGN, MERGE routines
'----------------------------------------
Dim Ycurr%, Yroad%

Dim alignA$(15), alignAR$(15)
Dim AN$(0 To 14)
Dim ORD$(0 To 100)
Dim HRD$(0 To 100)

Dim AN!(15, 14), AR!(15, 14), ARD!(0 To 15, 0 To 14)
Dim alignZ!(0 To 15), alignZ1!(0 To 15)
Dim W0N!(15), WN1!(15)
Dim W0RD!(0 To 15), W1RD!(0 To 15)
Dim W0R!(15), WR1!(15)
Dim ZR!(15), ZR1!(15)
Dim ZRD!(0 To 16), ZRD1!(0 To 16)

Dim ORD%(0 To 100), mergeT%(0 To 100), TRD%(0 To 100)


'----------------------------------------
' Used across BINPUT routines
'----------------------------------------
Dim inputB%(0 To 14, 0 To 19), inputT%(35), inputL%(0 To 6)
Dim inputB$(15, 1), inputT$(4)

'----------------------------------------
' Used across CAREER routines
'----------------------------------------
Dim ars!(15, 62, 15), W0S!(15, 62), W1S!(15, 62)
Dim AR$(62)

'----------------------------------------
' Used across COMPARE routines
'----------------------------------------
Dim compareA$(15), compareAA$(15, 1), compareB$(14)
Dim compareT$(0)

Dim compareB%(15, 19), compareL%(6), compareT%(34)
Dim compareW0!(15), compareW1!(15)
Dim compareZ!(0 To 15), compareZ1!(0 To 15)

'----------------------------------------
' Used across CREATE routines
'----------------------------------------
Dim intZ1%(TEAMS_PER_CONFERENCE)

'----------------------------------------
' Used across DRAFT routines
'----------------------------------------
Dim draftB%(0 To 1, 0 To 14, 0 To 19), draftL%(0 To 1, 0 To 6)
Dim draftLG%(0 To 52, 0 To 5), draftLT%(0 To 14, 0 To 34)
Dim draftT%(0 To 1, 0 To 34)
Dim draftAA$(1, 4), draftB$(1, 14, 1), draftYears$(1 To 52), draftYN$(0 To 1)

'----------------------------------------
' Used across GPMIN routines
'----------------------------------------
Dim adjustB$(15, 1), adjustT$(0 To 4)
Dim adjustB%(0 To 14, 19), adjustL%(0 To 6), adjustT%(0 To 34)

'----------------------------------------
' Used across HD2HD routines
'----------------------------------------
Dim hd2hdT!(34)
Dim hd2hdAL!(50), AW!(50), HL!(50), HW!(50), R1!(50), R2!(50), R3!(50), R4!(50)
Dim TAL!(40), TAW!(40), THL!(40), THW!(40)
Dim TR1!(40), TR2!(40), TR3!(40), TR4!(40)

Dim LG$, div1$, div2$, div3$, div4$

'----------------------------------------
' Used across LOOKY,
'   LEAGCMPL routines
'----------------------------------------
Dim leagueT%(100), statO%(0 To 100), statsL%(0 To 100)

Dim leagBRC!(32), statW0!(0 To 14), statW1!(0 To 14)
Dim values!(0 To 14, 0 To 24)

Dim categories$(0 To 25), statsH$(0 To 100), lookyA$(0 To 14), O$(0 To 100)

'----------------------------------------
' Used across LEAGCMPL,
'   SEELEAG routines
'----------------------------------------
Dim DL!(40, 20), LD!(250, 2), leagS!(14, 26)
Dim O1!(40), O2!(40), OL!(40, 20)
Dim P!(40), statZ!(260), statZ1!(260)

Dim DL$(40), N$(14), Z2$(260), Z3$(260)
Dim H1$(40), OL$(40), leagT$(40)

Dim DT!(21), OT!(21)

Dim AL%, AW%, HL%, HW%, L%, NL%, NW%, VL%, VW%, W%, Z%

'----------------------------------------
' Used across NEWLDR routines
'----------------------------------------
Dim HLL(TEAMS_PER_CONFERENCE), HWW(TEAMS_PER_CONFERENCE)
Dim LL(TEAMS_PER_CONFERENCE), LLS(TEAMS_PER_CONFERENCE), LWS(TEAMS_PER_CONFERENCE)
Dim LTL(TEAMS_PER_CONFERENCE), LTW(TEAMS_PER_CONFERENCE)
Dim RLL(TEAMS_PER_CONFERENCE), RWW(TEAMS_PER_CONFERENCE)
Dim TLS(TEAMS_PER_CONFERENCE), TWS(TEAMS_PER_CONFERENCE), WW(TEAMS_PER_CONFERENCE)


Dim AL!(600, 14)
Dim GM!(40), GM1!(40)
Dim GMA!(600), TYP!(600)
Dim TT!(40, 15), TT1!(40, 15)
Dim W0L!(600), W1L!(600)

Dim AL$(600)

Dim strZ1$(TEAMS_PER_CONFERENCE)
Dim TMA$(600), TML$(TEAMS_PER_CONFERENCE)
Dim TT$(40, 15), TT1$(40, 15)
Dim TP$(0 To 31), TP1$(0 To 38)
Dim TPP$(600), TMM$(600)


'----------------------------------------
' Used across REC routines
'----------------------------------------
Dim teamRecords$(20), indRecords$(25)
Dim BRC$(25, 1), TRC$(21), TRC1$(21)
Dim BRC!(25), TRC!(21), TRC1!(21)

'----------------------------------------
' Used across RECORDS routines
'----------------------------------------
Dim indRecCategory$(50, 4), teamRecCategory$(125, 3)
Dim allRecords!(50, 2), teamRecords!(125, 2)

'----------------------------------------
' Used across SCHEDULE routines
'----------------------------------------
Dim BS%, NS%
Dim N$
Dim scheduleAP%(1), homeScores(MAX_GAMES), visitorScores(MAX_GAMES)

'NG = Number of Games; not sure about the 18
Dim scheduleNG%(MAX_GAMES, 18)

ReDim scheduleH$(0 To 20), scheduleV$(0 To 20)
Dim scheduleYN$(MAX_GAMES, 1)

'----------------------------------------
' Used across TRADE routines
'----------------------------------------
Dim teamSettings%(1, 34)
Dim tradeB%(0 To 1, 0 To 14, 0 To 19), tradeL%(0 To 1, 0 To 6)
Dim tradeS(1), tradeZZ(2, 15), tradeZZ1(2, 15)

Dim tradeB1!(2, 14, 14), tradeT!(1)
Dim tradeW0!(2, 14), tradeW1!(2, 14)

Dim B1$(2, 14), tradeA1$(1), tradeAA$(1, 6)
Dim tradeB$(0 To 1, 0 To 14, 0 To 1), SA$(1), statFiles$(1)

'----------------------------------------
' Used across Game Routines
'----------------------------------------
Dim scheduleFile$

Dim tickerStart

Dim Shared AD, AP, B, CT, D, endGame, endAllGames, F3, H, halfTime, JY
Dim Shared N, NTMS, P, P7, P9, PO, QQ, QR, sClockVal, shotClock
Dim Shared T1, TMT, X, U5, VG, VH

Dim Shared CF%, CM%, DK%, FB%, FR%, FT%, GF%, HH%
Dim Shared NB%, nbrLines%, NM%, OX%, OY%, PB%, PT%, PZ%
Dim Shared RT%, X4%, XY%

Dim Shared gameClock!, pbpDelay!, timeElapsed!

Dim Shared actualAttendance&
Dim Shared avgAttendance&(1)

Dim Shared A1$, B1$, C1$, D$, D1$, E1$, F1$, G1$, H1$, O$
Dim Shared P7$, PB$, PS$, TS$, U$, Y2$, Z3$

Dim Shared AP%(2), B%(1, 14, 19), CF%(1, 9), E%(13)
Dim Shared F%(1, 9), F5%(1, 4), F7%(1, 9), G9%(1), GF%(2, 9)
Dim Shared HT%(100), L%(1, 6), NG%(18)
Dim Shared N0%(2, 2, 4), OX%(2), OY%(2), O%(100), O0%(1)
Dim Shared PF%(1), PFA%(33), ST%(32), SX%(32, 1, 14)
Dim Shared TOA%(33), TOF%(1), W%(1, 14, 1)

Dim Shared C1(1, 14)
Dim Shared D1(1), D2(1), D8(6, 6), F1(14), G4(14), G5(14)
Dim Shared P2(1), P4(5), P5(5), P7(1), QQ(1, 8, 14, 14), QR(1, 7, 14)
Dim Shared R3(1), RB(0 To 10), S3(14), SC(0 To 1)
Dim Shared T(1, 34), T0(1), T1(1), TF(1)
Dim Shared V9(14), VG(9), VH(9), W2(1, 14), W3(1, 14)
Dim Shared X$(4), X7(1), Z5(1), Z6(1), ZF(1), timePlayed(2, 14)

Dim Shared A!(1, 14, 25), B1!(1, 4), M9!(1), indRecords!(50, 2)
Dim Shared S!(1, 14), W0!(1, 14), W1!(1, 14)
Dim Shared Z!(15), Z1!(15), Z2!(14, 14)

Dim Shared AA$(1, 14, 1), ABB$(3), A1$(3), B$(3)
Dim Shared defenseStyles$(5)
Dim Shared gameCoach$(3), gameStadium$(3)
Dim Shared H$(100), HO$(100), offenseStyles$(2)
Dim Shared PB$(1), PS$(4)
Dim Shared R$(14), RC$(50, 4), SX$(32, 1)
Dim Shared Y$(1), YN$(3)

