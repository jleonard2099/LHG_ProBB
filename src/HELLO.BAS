$Debug

Const GAME_VERSION$ = "4.37"
Const MAX_SCHEDULE_GAMES = 1500
Const MAX_TEAMS = 1000
Const MAX_FILES = 10240
'optimium = 1048576 (but slows code)
Const MAX_CONFERENCES = 40
Const TEAMS_PER_DIV = 10
Const DIVS_PER_CONFERENCE = 4
Const TEAMS_PER_CONFERENCE = 40

Const MAX_SCHED_STATS = 100
Const SCHEDULE_SIZE_BYTES = 48

DefInt A-Z

' General purpose
'$INCLUDE: 'KeyInput.bi'
'$INCLUDE: 'QPProEqu.bi'
'$INCLUDE: 'PadRight.bi'
'$INCLUDE: 'QBWRITE.bi'

' For Utils
'$INCLUDE: 'BoxRoutines.bi'
'$INCLUDE: 'FileTeamSelection.bi'
'INCLUDE: 'Variables.bi'


'----------------------------------------
' Used in more than one source file
'----------------------------------------
Dim opSysType$

Dim Shared diskPaths$(0 To 3), Q$(0 To 377)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

' *** Reading Stat Data ***
' -------------------------
Dim teamScore(0 To MAX_SCHED_STATS), oppScore(0 To MAX_SCHED_STATS)

Dim locIndicator$(0 To MAX_SCHED_STATS), oppName$(MAX_SCHED_STATS)

Dim statsZ1$(TEAMS_PER_CONFERENCE)

Dim statsW0!(0 To 14), statsW1!(0 To 14)
Dim statsZ!(0 To 15), statsZ1!(0 To 15), statsZ2!(0 To 14, 0 To 13)

'-- For Road Data
Dim ORD%(0 To MAX_SCHED_STATS), TRD%(0 To MAX_SCHED_STATS)
Dim HRD$(0 To MAX_SCHED_STATS), ORD$(0 To MAX_SCHED_STATS)


' *** Schedule Data ***
' -------------------------
Dim homeScores(MAX_SCHEDULE_GAMES), visitorScores(MAX_SCHEDULE_GAMES)

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
Dim nbrGamesCurr, nbrGamesRoad

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
Dim plyrRecords!(1 To 25), offRecords!(1 To 21), defRecords!(1 To 21)
Dim plyrRecDesc$(25, 1), offRecDesc$(1 To 21), defRecDesc$(1 To 21)

Dim indRecords!(50, 2), teamRecords!(125, 2)
Dim indRecords$(25), teamRecords$(20)
Dim indRecDesc$(50, 4), teamRecDesc$(125, 3)


'----------------------------------------
' Used in SCHEDULE routines
'----------------------------------------
Dim BS%, NS%

Dim scheduleAP%(1), scheduleNG%(MAX_SCHEDULE_GAMES, 18)

ReDim scheduleH$(0 To 20), scheduleV$(0 To 20)
Dim scheduleYN$(MAX_SCHEDULE_GAMES, 1)


'----------------------------------------
' Used in Game Routines
'----------------------------------------
Dim scheduleFile$

Dim actualAttendance&
Dim avgAttendance&(1)

Dim tickerStart

Dim alpha$(4), tickerPeriod$(14)

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
Dim Shared P7$, prevBall$, TS$, pbpString$, Y2$, Z3$

Dim Shared CF%(1, 9), eventSettings(13)
Dim Shared F%(1, 9), F5%(1, 4), F7%(1, 9), FY%(0 To 1), G9%(1), GF%(2, 9)
Dim Shared NG%(18), N0%(2, 2, 4)
Dim Shared OX%(2), OY%(2), O0%(1)
Dim Shared PF%(1), PFA%(33), ST%(32), SX%(32, 1, 14)
Dim Shared TOA%(33), TOF%(1), W%(1, 14, 1), YR%(1)

'Dim Shared C1(1, 14)
Dim Shared D1(1), D2(1), D8(6, 6), F1(14), G4(14), G5(14)
Dim Shared leagRat_GAME(1, 6), rebRatings(0 To 10), plyrRat_GAME(1, 14, 19)
Dim Shared P2(1), P4(5), P5(5), P7(0 To 1), QQ(1, 8, 14, 14), QR(1, 7, 14)
Dim Shared S3(14), scSettings(0 To 2)
Dim Shared score(1, 14), schedGame(2)
Dim Shared teamFouls(1), timePlayed(2, 14), timeouts(1)
Dim Shared tmRat_GAME(0 To 1, 0 To 34), totRebounds(1), turnovers(1)
Dim Shared statTotals(14), pbpFG(1 To 8), pbpBG(1 To 8), W2(1, 14), W3(1, 14)
Dim Shared X7(1), Z5(1), Z6(1)

Dim Shared gameB1!(0 To 1, 0 To 4), gameRatings!(0 To 1, 0 To 14, 0 To 25)
Dim Shared M9!(1)
Dim gameW0!(1, 14), gameW1!(1, 14)

Dim Shared defenseStyles$(5), diskIDs$(0 To 1)
Dim Shared gameCoach$(3), gameMascots$(3), gameStadium$(3), gameTeams$(3)
Dim Shared offenseStyles$(2), pbpType$(1), players_GAME$(1, 14, 1), posnName$(0 To 4)
Dim Shared SX$(32, 1)
Dim Shared teamAbbrev$(3), Y$(1)



'$INCLUDE: 'MainRoutines.bi'
'$INCLUDE: 'TITLE.bi'

' For Game
'$INCLUDE: 'GameRoutines.bi'


On Error GoTo Errhandler


'----------------------------------------
'          READ IN ALL DATA
'----------------------------------------
Data 1954,1955,1956,1957,1958,1959
Data 1960,1961,1962,1963,1964,1965,1966,1967,1968,"1968 ABA",1969,"1969 ABA"
Data 1970,"1970 ABA",1971,"1971 ABA",1972,"1972 ABA",1973,"1973 ABA",1974,"1974 ABA"
Data 1975,"1975 ABA",1976,"1976 ABA",1977,1978,1979
Data 1980,1981,1982,1983,1984,1985,1986,1987,1988,1989
Data 1990,1991,1992,1993,1994,1995,1996,1997,1998,1999
Data 2000,2001,2002,2003,2004,2005,2006,2007,2008,2009
Data 2010,2011,2012,2013,2014,2015,2016,2017,2018,2019
Data 2020,2021,2022,2023

For I = 1 To 79
    Read draftYears$(I)
    While Len(draftYears$(I)) <= 8
        draftYears$(I) = draftYears$(I) + " "
    Wend
Next

' Stats / Year
' -- FGA,TRB,FG%,PF,3FG%
'1953-54 season is the start
Data 75,47,37,25,0
Data 86,52,39,25,0
Data 91,55,39,26,0
Data 95,56,38,25,0
Data 102,62,38,25,0
Data 102,60,39,26,0
Data 109,64,41,26,0
Data 111,54,41,27,0
Data 108,62,43,26,0
Data 101,58,44,26,0
Data 99,58,43,27,0
Data 100,59,43,26,0
Data 102,60,43,26,0
Data 103,59,44,26,0
Data 101,66,45,26,0
Data 97,55,42,25,29
Data 99,57,44,25,0
Data 98,54,44,27,30
Data 99,53,46,25,0
Data 97,54,44,26,29
Data 98,53,45,24,0
Data 100,54,46,26,30
Data 96,51,45,24,0
Data 95,52,46,25,30
Data 97,51,46,23,0
Data 92,49,47,26,29
Data 94,48,46,24,0
Data 93,48,46,24,28
Data 91,47,46,24,0
Data 93,48,47,23,29
Data 92,47,46,25,0
Data 96,50,47,25,29
Data 92,47,47,25,0
Data 93,47,47,25,0
Data 92,45,49,25,0
Data 91,45,48,24,28
Data 88,43,39,25,25
Data 88,43,39,26,26
Data 90,44,49,26,24
Data 88,43,49,26,25
Data 89,43,49,25,28
Data 89,44,49,25,28
Data 89,44,48,25,30
Data 88,43,48,24,32
Data 89,44,48,24,32
Data 87,43,48,23,33
Data 87,43,47,23,32
Data 87,44,47,22,33
Data 86,43,47,23,34
Data 84,43,47,22,33
Data 82,42,47,24,36
Data 80,41,46,23,37
Data 79,41,46,22,36
Data 80,42,45,22,35
Data 78,42,44,22,34
Data 82,43,45,23,35
Data 81,43,44,22,35
Data 81,42,45,21,35
Data 81,42,44,22,35
Data 80,42,44,21,35
Data 80,42,45,23,36
Data 79,41,45,23,36
Data 80,41,46,22,36
Data 82,42,46,21,36
Data 81,41,46,21,37
Data 82,42,46,21,36
Data 81,41,46,21,36
Data 81,42,45,20,35
Data 82,42,45,20,36
Data 83,43,45,21,36
Data 84,43,45,20,35
Data 85,44,45,20,35
Data 85,44,46,20,36
Data 86,44,46,20,36
Data 89,45,46,21,36
Data 89,45,46,21,36
Data 88,44,47,19,37
Data 88,45,46,20,35
Data 88,43,48,20,36

For I = 1 To 79
    For I1 = 1 To 5
        Read leagueAverages%(I, I1)
    Next
Next


Data NAME,G,MIN,MIN/G,FGM,FGA,FG PCT,3FGM,3FGA,3FG PCT,FTM,FTA,FT PCT
Data NAME,PTS,PTG/G,HI,OFF REB,DEF REB,TOT REB,REB/G,PF,AST,BLK,STL,TO
For I = 0 To 25: Read categories$(I): Next

Data GAMES,MINUTES,FIELD GOALS MADE,FIELD GOAL ATTEMPS,F.G. PCT. (3.65 FGM/GAME),THREE POINTS MADE,THREE POINT ATTEMPTS,3FG (.30 3FGM/GAME),FREE THROWS MADE,FREE THROWS ATTEMPTED,F.T. PCT. (1.52 FTM/GAME)
Data POINTS,SCORING AVERAGE,HI-GAME,OFFENSIVE REBOUNDS,DEFENSIVE REBOUNDS,TOTAL REBOUNDS,AVERAGE REBOUNDS PER GAME,FOULS,ASSISTS,BLOCKED SHOTS,STEALS,TURNOVERS
Data POINTS/48 MINUTES,REBOUNDS/48 MINUTES,FOULS/48 MINUTES,ASSISTS/48 MINUTES,BLOCKS/48 MINUTES,STEALS/48 MINUTES,TURNOVERS/48 MINUTES,MINUTES/GAME,GO TO MAIN MENU
For I = 0 To 31
    Read TP$(I)
Next

Data FIELD GOAL ATTEMPTS,FIELD GOALS MADE,F.G. PCT.,THREE POINT ATTEMPTS,THREE POINTS MADE,THREE POINT PERCENTAGE,FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW PERCENTAGE,OFFENSIVE REBOUNDS
Data DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME,FOULS,ASSISTS,BLOCKS,STEALS,TURNOVERS,POINTS PER GAME
Data OPP. FIELD GOAL ATTEMPTS,OPP. FIELD GOALS MADE,OPP. FIELD GOAL PERCENTAGE,OPP. THREE POINTERS ATTEMPTED,OPP. THREE POINTERS MADE,OPP. THREE POINT PERCENTAGE,OPP. FREE THROWS ATTEMPTED,OPP. FREE THROWS MADE,OPP. FREE THROW PERCENTAGE
Data OPP. OFFENSIVE REBOUNDS,OPP. DEFENSIVE REBOUNDS,OPP. TOTAL REBOUNDS,OPP. REBOUNDS PER GAME,OPP. FOULS,OPP. ASSISTS,OPP. BLOCKS,OPP. STEALS,OPP. TURNOVERS,OPP. POINTS PER GAME,RETURN TO MAIN MENU
For I = 0 To 38
    Read TP1$(I)
Next

Data GAMES PLAYED,FIELD GOALS ATTEMPTED,FIELD GOALS MADE,FIELD GOAL %,3-FIELD GOALS ATTEMPTED,3-FIELD GOALS MADE,3-FIELD GOAL %
Data FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW %,MINUTES PLAYED,POINTS,SCORING AVERAGE,HI-GAME,OFFENSIVE REBOUNDS
Data DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME,PERSONAL FOULS,STEALS,ASSISTS,BLOCKS,TURNOVERS,MINUTES PER GAME,MVP VOTING
For I = 1 To 25: Read indRecords$(I): Next

Data FIELD GOALS ATTEMPTED,FIELD GOALS MADE,FIELD GOAL %,3-FIELD GOALS ATTEMPTED,3-FIELD GOALS MADE,3-FIELD GOAL %
Data FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW %,OFFENSIVE REBOUNDS,DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME
Data PERSONAL FOULS,STEALS,ASSISTS,BLOCKS,TURNOVERS,POINTS,POINTS PER GAME
For I = 1 To 20: Read teamRecords$(I): Next

Data L,1,1,1,2,2,2,H,H,3,3,3,4,4,F
For I = 0 To 14
    Read tickerPeriod$(I)
Next

Data 60,61,60,58,58,59,59,59,59,58,58,58,57,58,57,51,51,52,49,50,44,45,42,47,43,43,42,39,38,38,35,34,34
For I = 0 To 32
    Read TOA%(I)
Next

Data 9,8,8,8,8,8,8,9,8,9,8,8,7,7,06,07,07,07,08,08,08,08,08,08,08,09,09,09,09,09,09,09,08
For I = 0 To 32
    Read PFA%(I)
Next

Data "REG M-M     ","SAGGING M-M ","TRAPPING M-M","TIGHT M-M   ","LOOSE M-M   "
For I = 1 To 5
    Read defenseStyles$(I)
Next I

Data -2,-2,-3,-4,-4,-5,-5,-6,-7
Data -4,-5,-5,-6,-7,-8,-9,-10,-11

Data 0,0,-1,-2,-3,-4,-5,-5,-6
Data -3,-3,-4,-5,-6,-6,-7,-8,-9
Data -6,-7,-8,-8,-8,-9,-10,-11,-12
For I = 0 To 1
    For I1 = 1 To 9
        Read CF%(I, I1)
    Next I1
Next I

For I = 0 To 2
    For I1 = 1 To 9
        Read GF%(I, I1)
    Next I1
Next I

' Game options
coachMode$(0) = "COACH AND SELECT SHOTS           "
coachMode$(1) = "COACH ONLY AND LET COMPUTER SHOOT"

foulOption$(0) = "ONLY IN BONUS OR ACT OF SHOOTING"
foulOption$(1) = "SHOOT ON ALL FOULS              "

freeThrowOpt$(0) = "CURRENT RULES            "
freeThrowOpt$(1) = "3 TO MAKE 2 / 2 TO MAKE 1"

location$(0) = "@ TEAM 1            "
location$(1) = "HOME COURT ADVANTAGE"
location$(2) = "NEUTRAL SITE        "

modeAbbrev$(0) = "HH"
modeAbbrev$(1) = "CV"
modeAbbrev$(2) = "CH"
modeAbbrev$(3) = "CC"

playMode$(0) = "HUMAN OPPONENT           "
playMode$(1) = "COMPUTER IS VISITING TEAM"
playMode$(2) = "COMPUTER IS HOME TEAM    "
playMode$(3) = "COMPUTER VS COMPUTER     "

playoffGame$(0) = "REGULAR SEASON"
playoffGame$(1) = "PLAYOFF       "

sClockOpt$(0) = "24 SEC SHOT CLOCK"
sClockOpt$(1) = "30 SEC SHOT CLOCK"
sClockOpt$(2) = "NO SHOT CLOCK    "

teamEraOpt$(0) = "VISITING     "
teamEraOpt$(1) = "HOME         "
teamEraOpt$(2) = "NO ADJUSTMENT"

yesNoText$(0) = "NO "
yesNoText$(1) = "YES"

yesNo$(0) = "N"
yesNo$(1) = "Y"


'----------------------------------------
'               MAIN CODE
'----------------------------------------
_Title "Basketball: The Pro Game - Menu"
$ExeIcon:'./lhg_probb.ico'
_Icon
_AllowFullScreen

opSysType$ = Left$(_OS$, InStr(2, _OS$, "[") - 1)

If Not _FileExists("DEV.BAS") Then

    Open "DEV.BAS" For Output As #1

    For I = 0 To 1

        Select Case opSysType$
            Case "[WINDOWS]": Print #1, _CWD$ + "\"
            Case "[MAC]", "[MACOSX]", "[LINUX]": Print #1, _CWD$ + "/"
        End Select

    Next

    Close #1

End If

Open "DEV.BAS" For Input As #2

For I = 0 To 1
    Input #2, diskPaths$(I)
Next

Close #2

Selection% = 1

Do

    Call Main_Menu(Selection%)

Loop Until Selection% = 7

End

System


'----------------------------------------

Errhandler:
Open "errlog" For Append As #10
Print #10, "Error #"; Err; "on program file line"; _ErrorLine
If _FileExists("GAME82") Then
    Print #10, "Schedule game # "; Using "###"; schedGame(0) + schedGame(2)
    Print #10, gameTeams$(0) + " vs. " + gameTeams$(1)
End If
Print #10,
Close #10

Resume Next ' moves program to code following the error.

'----------------------------------------
'       Main_Menu Subroutine
'----------------------------------------
Sub Main_Menu (menuSelection%)

    Do

        _Title "Basketball: The Pro Game - Menu"

        Cls

        Call TITLE

        ReDim menuChoices$(1 To 7)

        menuChoices$(1) = "PLAY SINGLE GAME                "
        menuChoices$(2) = "TEAM MANAGEMENT / ORGANIZATION  "
        menuChoices$(3) = "REPLAY STATISTICS AND REPORTS   "
        menuChoices$(4) = "LEADERS / STANDINGS / RECORDS   "
        menuChoices$(5) = "SCHEDULER / AUTOPLAY / BOXSCORES"
        'menuChoices$(6) = "HARD DRIVE INSTALLATION         "
        menuChoices$(6) = "CONFIGURE DRIVES FOR TEAMS,STATS"
        menuChoices$(7) = "EXIT TO DOS                     "

        menuSelection% = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), menuSelection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case menuSelection%

                Case 1:
                    Call PROHOOP

                Case 2:
                    Call TeamMgt_Menu

                Case 3:
                    Call Replay_Menu

                Case 4:
                    Call Leaders_Menu

                Case 5:
                    Call SCHEDULE

                Case 6:
                    Call ConfigureDrives

            End Select

        End If

    Loop Until menuSelection% = 7 Or ExitCode% = 27

    If ExitCode% = 27 Then menuSelection% = 7

End Sub


'----------------------------------------
'       ConfigureDrives Subroutine
'----------------------------------------
'...explanation...
Sub ConfigureDrives ()

    Shared opSysType$

    Color 15, 1
    FG = 0
    BG = 7

    For I = 0 To 1
        diskPaths$(I) = Left$(diskPaths$(I), Len(diskPaths$(I)) - 1)
    Next

    Cls

    Locate , 16: Print "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS"
    Print
    Locate , 31: Print "PATHNAME EXAMPLES:"
    Select Case opSysType$
        Case "[WINDOWS]":
            Locate , 31: Print "A:"
            Locate , 31: Print "C:\PROBB\TEAMS"
        Case "[LINUX]":
            Locate , 31: Print "/mnt/floppy"
            Locate , 31: Print "/usr/bin/probb/teams"
        Case "[MAC]", "[MACOSX]":
            Locate , 31: Print "/Volumes/floppy"
            Locate , 31: Print "/Applications/probb/teams"
    End Select

    Print
    Locate 7, 1: QPrint "Path for Teams ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(0), 61), FG, BG, -1
    Locate 9, 1: QPrint "Path for Stats ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(1), 61), FG, BG, -1

    GetDiv1:
    Locate 7, 16 'location of beginning of field
    CapsOn = 0
    NumOnly = 0
    Max = 50
    Call TextIn(diskPaths$(0), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    diskPaths$(0) = _Trim$(diskPaths$(0))

    GetDiv2:
    Locate 9, 16 'location of beginning of field
    CapsOn = 0
    NumOnly = 0
    Max = 50
    Call TextIn(diskPaths$(1), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    diskPaths$(1) = _Trim$(diskPaths$(1))

    Done:
    Color 14, 1
    Locate 23, 1
    Print "Is this okay? (Yes/No) ";
    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 GoTo GetDiv2
    If X$ = "N" GoTo GetDiv1
    If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

    Open "DEV.BAS" For Output As #1

    For R = 0 To 1:

        Select Case opSysType$
            Case "[WINDOWS]": diskPaths$(R) = diskPaths$(R) + "\"
            Case "[MAC]", "[MACOSX]", "[LINUX]": diskPaths$(R) = diskPaths$(R) + "/"
        End Select

        Print #1, diskPaths$(R)

    Next R

    Close #1

End Sub


'----------------------------------------
'        TeamMgmt_Menu Subroutine
'----------------------------------------
'...explanation...
Sub TeamMgt_Menu ()

    Do

        Selection% = 1

        Call TITLE

        ReDim menuChoices$(1 To 8)

        menuChoices$(1) = "INPUT A NEW TEAM              "
        menuChoices$(2) = "EDIT AN EXISTING TEAM         "
        menuChoices$(3) = "PRINT TEAM ROSTER / REPORT    "
        menuChoices$(4) = "VIEW TEAM ROSTER / REPORT     "
        menuChoices$(5) = "DRAFT TO NEW / EXISTING TEAM  "
        menuChoices$(6) = "TRADE PLAYERS                 "
        menuChoices$(7) = "ADJUST PLAYING TIME (MIN/GAME)"
        menuChoices$(8) = "RETURN TO MAIN MENU           "
        Count% = 8
        Start% = 1
        ExitCode% = 8
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1: Call TINPUT(1)
                Case 2: Call TINPUT(2)
                Case 3: Call TINPUT(3)
                Case 4: Call TINPUT(4)
                Case 5: Call DRAFT
                Case 6: Call TRADE
                Case 7: Call GPMIN

            End Select

        End If

    Loop Until Selection% = 8 Or ExitCode% = 27

End Sub


'----------------------------------------
'         Replay_Menu Subroutine
'----------------------------------------
'This subroutine provodes the user with
'several options for managing replay
'data, from statistics to reports and
'team careers
Sub Replay_Menu ()

    Do

        Selection% = 1

        Call TITLE

        _Title "Basketball: The Pro Game - Stats Menu"

        ReDim menuChoices$(1 To 5)

        menuChoices$(1) = "VIEW / PRINT COMPILED STATS       "
        menuChoices$(2) = "ENCYCLOPEDIA / TEAM REPLAY OPTIONS"
        menuChoices$(3) = "COMPARE REPLAY STATS TO ACTUAL    "
        'menuChoices$(4) = "MODIFY REPLAY STATS               "
        menuChoices$(4) = "MERGE SEPARATE STATISTICAL FILES  "
        menuChoices$(5) = "RETURN TO MAIN MENU               "
        Selection% = 1
        Count% = 5
        Start% = 1
        ExitCode% = 5
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    Call LOOKY

                Case 2:
                    Call CAREER

                Case 3:
                    Call COMPARE

                Case 4:
                    Call MERGE

                Case 5:
                    '

            End Select

        End If

        Color 15, 0
        Cls

    Loop Until Selection% = 5 Or ExitCode% = 27

End Sub


'----------------------------------------
'        Leaders_Menu Subroutine
'----------------------------------------
'This subroutine provodes the user with
'several options for managing team
'leaders, stats and standings
Sub Leaders_Menu ()

    Do

        Call TITLE

        _Title "Basketball: The Pro Game - League Menu"

        ReDim menuChoices$(1 To 8)

        menuChoices$(1) = "CREATE / EDIT A LEAGUE               "
        menuChoices$(2) = "UPDATE STATS / STANDINGS FOR A LEAGUE"
        menuChoices$(3) = "VIEW / PRINT RESULTS FOR A LEAGUE    "
        'menuChoices$(3) = "VIEW RESULTS FOR A LEAGUE            "
        'menuChoices$(4) = "PRINT RESULTS FOR A LEAGUE           "
        menuChoices$(4) = "HEAD-TO-HEAD RESULTS AND STATS       "
        menuChoices$(5) = "LEAGUE RECORD BOOKS                  "
        menuChoices$(6) = "EXPANDED LEADERS / STANDINGS         "
        menuChoices$(7) = "DELETE ALL STATS FOR A LEAGUE        "
        menuChoices$(8) = "RETURN TO MAIN MENU                  "
        Selection% = 1
        Count% = 8
        Start% = 1
        ExitCode% = 8
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    Call CREATPRO

                Case 2:
                    Call CompileLeague

                Case 3:
                    Call SEELEAG

                Case 4:
                    Call HD2HDPRO

                Case 5:
                    Call Records_Menu

                Case 6:
                    Call NEWLDR

                Case 7:
                    Call BLANK

            End Select

        End If

    Loop Until Selection% = 8 Or ExitCode% = 27

End Sub


' General purpose
'$INCLUDE: 'KeyInput.bm'
'$INCLUDE: 'QPProEqu.bm'
'$INCLUDE: 'PadRight.bm'
'$INCLUDE: 'QBWRITE.bm'

' For Utils
'$INCLUDE: 'BoxRoutines.bm'
'$INCLUDE: 'FileTeamSelection.bm'
'$INCLUDE: 'TITLE.bm'
'$INCLUDE: 'ALIGN.BAS'
'$INCLUDE: 'BLANK.BAS'
'$INCLUDE: 'CAREER.BAS'
'$INCLUDE: 'CARVIEW.BAS'
'$INCLUDE: 'COMPARE.BAS'
'$INCLUDE: 'COMPLEAG.BAS'
'$INCLUDE: 'CREATPRO.BAS'
'$INCLUDE: 'DRAFT.BAS'
'$INCLUDE: 'GPMIN.BAS'
'$INCLUDE: 'HD2HDPRO.BAS'
'$INCLUDE: 'LOOKY.BAS'
'$INCLUDE: 'MERGE.BAS'
'$INCLUDE: 'NEWLDR.BAS'
'$INCLUDE: 'NEWLDRST.BAS'
'$INCLUDE: 'REC.BAS'
'$INCLUDE: 'RECORDS.BAS'
'$INCLUDE: 'SCHEDULE.BAS'
'$INCLUDE: 'SEELEAG.BAS'
'$INCLUDE: 'SINPUT.BAS'
'$INCLUDE: 'TINPUT.BAS'
'$INCLUDE: 'TRADE.BAS'

' For Game
'$INCLUDE: 'PROHOOP.BAS'
'$INCLUDE: 'GameRoutines.bm'

