f$Debug
$Color:32

Const GAME_VERSION$ = "5.1"
Const MAX_SCHEDULE_GAMES = 1500
Const MAX_TOURNAMENT_GAMES = 200
Const MAX_TEAMS = 1000
Const MAX_FILES = 10240
'optimium = 1048576 (but slows code)
Const TEAMS_PER_DIV = 10
Const DIVS_PER_LEAGUE = 4
Const TEAMS_PER_LEAGUE = 40
Const MAX_CAREER_YEARS = 15
'Const MAX_CONFERENCES = 40

Const MAX_SCHED_STATS = 100
Const SCHEDULE_SIZE_BYTES = 58

Const FOULS_TO_DQ = 6

DefInt A-Z

' General purpose
'$Include: 'KeyInput.bi'
'$Include: 'QPProEqu.bi'
'$Include: 'PadRight.bi'
'$Include: 'QBWRITE.bi'

' For Utils
'$Include: 'BoxRoutines.bi'
'$Include: 'FileTeamSelection.bi'
'$Include: 'Variables.bi'
'$Include: 'MainRoutines.bi'
'$Include: 'TITLE.bi'

' For Game
'$Include: 'GameRoutines.bi'


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
Data 2020,2021,2022,2023,2024

For I = 1 To 80
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
Data 88,43,48,20,36

For I = 1 To 80
    For I1 = 1 To 5
        Read leagueAverages%(I, I1)
    Next
Next


'Defaults for Event Settings
Data 5,250,120,135,60,30,12,9,6,5,6,0,0
For X = 1 To 13:
    Read eventSettings(X)
Next


Data NAME,G,MIN,MIN/G,FGM,FGA,FG PCT,3FGM,3FGA,3FG PCT,FTM,FTA,FT PCT
Data NAME,PTS,PTG/G,HI,OFF REB,DEF REB,TOT REB,REB/G,PF,AST,BLK,STL,TO
For I = 0 To 25:
    Read statCategoryAbbr$(I)
Next

Data GAMES,MINUTES,FIELD GOALS MADE,FIELD GOAL ATTEMPS,F.G. PCT. (3.65 FGM/GAME),THREE POINTS MADE,THREE POINT ATTEMPTS,3FG (.30 3FGM/GAME),FREE THROWS MADE,FREE THROWS ATTEMPTED,F.T. PCT. (1.52 FTM/GAME)
Data POINTS,SCORING AVERAGE,HI-GAME,OFFENSIVE REBOUNDS,DEFENSIVE REBOUNDS,TOTAL REBOUNDS,AVERAGE REBOUNDS PER GAME,FOULS,ASSISTS,BLOCKED SHOTS,STEALS,TURNOVERS
Data POINTS/48 MINUTES,REBOUNDS/48 MINUTES,FOULS/48 MINUTES,ASSISTS/48 MINUTES,BLOCKS/48 MINUTES,STEALS/48 MINUTES,TURNOVERS/48 MINUTES,MINUTES/GAME,GO TO MAIN MENU
For I = 0 To 31
    Read expIndCategory$(I)
Next

Data FIELD GOAL ATTEMPTS,FIELD GOALS MADE,F.G. PCT.,THREE POINT ATTEMPTS,THREE POINTS MADE,THREE POINT PERCENTAGE,FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW PERCENTAGE,OFFENSIVE REBOUNDS
Data DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME,FOULS,ASSISTS,BLOCKS,STEALS,TURNOVERS,POINTS PER GAME
Data OPP. FIELD GOAL ATTEMPTS,OPP. FIELD GOALS MADE,OPP. FIELD GOAL PERCENTAGE,OPP. THREE POINTERS ATTEMPTED,OPP. THREE POINTERS MADE,OPP. THREE POINT PERCENTAGE,OPP. FREE THROWS ATTEMPTED,OPP. FREE THROWS MADE,OPP. FREE THROW PERCENTAGE
Data OPP. OFFENSIVE REBOUNDS,OPP. DEFENSIVE REBOUNDS,OPP. TOTAL REBOUNDS,OPP. REBOUNDS PER GAME,OPP. FOULS,OPP. ASSISTS,OPP. BLOCKS,OPP. STEALS,OPP. TURNOVERS,OPP. POINTS PER GAME,RETURN TO MAIN MENU
For I = 0 To 38
    Read expTeamCategory$(I)
Next

Data GAMES PLAYED,FIELD GOALS ATTEMPTED,FIELD GOALS MADE,FIELD GOAL %,3-FIELD GOALS ATTEMPTED,3-FIELD GOALS MADE,3-FIELD GOAL %
Data FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW %,MINUTES PLAYED,POINTS,SCORING AVERAGE,HI-GAME,OFFENSIVE REBOUNDS
Data DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME,PERSONAL FOULS,STEALS,ASSISTS,BLOCKS,TURNOVERS,MINUTES PER GAME,MVP VOTING
For I = 1 To 25:
    Read indRecordDesc$(I)
Next

Data FIELD GOALS ATTEMPTED,FIELD GOALS MADE,FIELD GOAL %,3-FIELD GOALS ATTEMPTED,3-FIELD GOALS MADE,3-FIELD GOAL %
Data FREE THROWS ATTEMPTED,FREE THROWS MADE,FREE THROW %,OFFENSIVE REBOUNDS,DEFENSIVE REBOUNDS,TOTAL REBOUNDS,REBOUNDS PER GAME
Data PERSONAL FOULS,STEALS,ASSISTS,BLOCKS,TURNOVERS,POINTS,POINTS PER GAME
For I = 1 To 20:
    Read teamRecordDesc$(I)
Next

Data L,1,1,1,2,2,2,H,H,3,3,3,4,4,F
For I = 0 To 14
    Read tickerPeriod$(I)
Next

Data 60,61,60,58,58,59,59,59,59,58,58,58,57,58,57,51,51,52,49,50,48,45,42,47,43,43,42,39,38,38,35,34,34
For I = 0 To 32
    Read turnoverAdj(I)
Next

Data 9,8,8,8,8,8,8,9,8,9,8,8,7,7,6,7,7,7,8,8,8,8,8,8,8,9,9,9,9,9,9,9,8
For I = 0 To 32
    Read perFoulAdj(I)
Next

Data "REG M-M     "
Data "SAGGING M-M "
Data "TRAPPING M-M"
Data "TIGHT M-M   "
Data "LOOSE M-M   "
Data "1-2-2 ZONE  "
Data "1-3-1 ZONE  "
Data "2-3 ZONE    "
Data "3-2 ZONE    "

For I = 1 To 9
    Read defStyleDesc$(I)
Next I

' This is not currently used
'-- would it have been something with
'-- contribution factor adjustments?
Data -2,-2,-3,-4,-4,-5,-5,-6,-7
Data -4,-5,-5,-6,-7,-8,-9,-10,-11
For I = 0 To 1
    For I1 = 1 To 9
        Read CF%(I, I1)
    Next I1
Next I

'This is used to adjust the chance
'for a shot based on the defender's
'penetration rating
Data 0,0,-1,-2,-3,-4,-5,-5,-6
Data -3,-3,-4,-5,-6,-6,-7,-8,-9
Data -6,-7,-8,-8,-8,-9,-10,-11,-12
For I = 0 To 2
    For I1 = 1 To 9
        Read penetrationDefAdj(I, I1)
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

quarterLength$(0) = "12-MINUTE"
quarterLength$(1) = "10-MINUTE"

sClockOpt$(0) = "24 SEC SHOT CLOCK"
sClockOpt$(1) = "30 SEC SHOT CLOCK"
sClockOpt$(2) = "NO SHOT CLOCK    "

teamEraOpt$(0) = "VISITING     "
teamEraOpt$(1) = "HOME         "
teamEraOpt$(2) = "NO ADJUSTMENT"

timeoutSetting$(0) = "MANDATORY @ 7 / 10 MIN; LIMIT 4 IN Q4 (PRE 2001)"
timeoutSetting$(1) = "MANDATORY @ 6 / 9 MIN; LIMIT 4 IN Q4 (2001-2017)"
timeoutSetting$(2) = "MANDATORY @ 7 / 3 MIN, LIMIT 4 IN Q4 (2017-18)"
timeoutSetting$(3) = "MANDATORY @ 7 / 3 MIN, LIMIT 2 IN Q4 (2018+)"
timeoutSetting$(4) = "MANDATORY @ 5 / 3 MIN, LIMIT 2 IN Q4 (WNBA)"

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

'Default new folders to current path
For I = 0 To 3

    Select Case opSysType$
        Case "[WINDOWS]":
            diskPaths$(I) = _CWD$ + "\"
        Case "[MAC]", "[MACOSX]", "[LINUX]":
            diskPaths$(I) = _CWD$ + "/"
    End Select

Next

'Read-in existing DEV.BAS
If _FileExists("DEV.BAS") Then

    Open "DEV.BAS" For Input As #1

    'DEV.BAS will not have the "Schedule" / "Leader" folders configured
    For I = 0 To 1
        Input #1, diskPaths$(I)
    Next

    Close #1

    Kill "DEV.BAS"

End If

'Write FOLDERS.CFG
If Not _FileExists("FOLDERS.CFG") Then

    Open "FOLDERS.CFG" For Output As #2

    For I = 0 To 3
        Print #2, diskPaths$(I)
    Next

    Close #2

End If

'Read in path values
Open "FOLDERS.CFG" For Input As #3

For I = 0 To 3
    Input #3, diskPaths$(I)
Next

Close #3

'Run main menu
SELECTION% = 1

Do

    Call Main_Menu(SELECTION%)

Loop Until SELECTION% = 7

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
Sub Main_Menu (menuSelection)

    Do

        _Title "Basketball: The Pro Game - Menu"

        Cls

        Call TITLE

        ReDim menuChoices$(1 To 7)

        menuChoices$(1) = "PLAY SINGLE GAME       "
        menuChoices$(2) = "TEAM MANAGEMENT        "
        menuChoices$(3) = "REPLAY ASSESSMENT      "
        menuChoices$(4) = "LEADERS AND RECORDS    "
        menuChoices$(5) = "SCHEDULES AND BOXSCORES"
        'menuChoices$(6) = "HARD DRIVE INSTALLATION  "
        menuChoices$(6) = "CONFIGURE GAME FOLDERS "
        menuChoices$(7) = "EXIT PROGRAM           "

        menuSelection = 1
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
        MMenuVert menuChoices$(), menuSelection, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case menuSelection

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

    Loop Until menuSelection = 7 Or ExitCode% = 27

    If ExitCode% = 27 Then menuSelection = 7

End Sub


'----------------------------------------
'        TeamMgmt_Menu Subroutine
'----------------------------------------
'This subroutine provodes the user with
'several options for managing a team and
'it's roster.
Sub TeamMgt_Menu ()

    Do

        SELECTION% = 1

        Call TITLE

        ReDim menuChoices$(1 To 9)

        menuChoices$(1) = "INPUT A NEW TEAM          "
        menuChoices$(2) = "EDIT AN EXISTING TEAM     "
        menuChoices$(3) = "PRINT TEAM ROSTER / REPORT"
        menuChoices$(4) = "VIEW TEAM ROSTER / REPORT "
        menuChoices$(5) = "DRAFT PLAYERS             "
        menuChoices$(6) = "TRADE PLAYERS             "
        menuChoices$(7) = "ADJUST PLAYING TIME       "
        menuChoices$(8) = "CONVERT DATA FILE FROM 4.0"
        menuChoices$(9) = "RETURN TO MAIN MENU       "
        Count% = 9
        Start% = 1
        ExitCode% = 9
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call InputTeam(1)

                Case 2:
                    Call InputTeam(2)

                Case 3:
                    Call InputTeam(3)

                Case 4:
                    Call InputTeam(4)

                Case 5:
                    Call DRAFT

                Case 6:
                    Call TRADE

                Case 7:
                    Call GPMIN

                Case 8:
                    targetFile$ = _OpenFileDialog$("Select team file", diskPaths$(0), "PROBBTMS.*", "Team files")

                    Call ConvertTeam4to5(targetFile$, 0)

            End Select

        End If

    Loop Until SELECTION% = 9 Or ExitCode% = 27

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

        SELECTION% = 1

        Call TITLE

        _Title "Basketball: The Pro Game - Stats Menu"

        ReDim menuChoices$(1 To 6)

        menuChoices$(1) = "VIEW / PRINT COMPILED STATS     "
        menuChoices$(2) = "TEAM CAREER OPTIONS             "
        menuChoices$(3) = "COMPARE REPLAY STATS TO ACTUAL  "
        menuChoices$(4) = "MODIFY REPLAY STATS             "
        menuChoices$(5) = "MERGE SEPARATE STATISTICAL FILES"
        menuChoices$(6) = "RETURN TO MAIN MENU             "
        SELECTION% = 1
        Count% = 6
        Start% = 1
        ExitCode% = 6
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call LOOKY

                Case 2:
                    Call CAREER

                Case 3:
                    Call COMPARE

                Case 4:
                    Call EditStats

                Case 5
                    Call MERGE

                Case 6:
                    '

            End Select

        End If

        Color 15, 0
        Cls

    Loop Until SELECTION% = 6 Or ExitCode% = 27

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

        menuChoices$(1) = "MANAGE LEAGUES                 "
        menuChoices$(2) = "UPDATE LEAGUE STATS / STANDINGS"
        menuChoices$(3) = "ACCESS LEAGUE REPORTS          "
        menuChoices$(4) = "HEAD-TO-HEAD RESULTS           "
        menuChoices$(5) = "LEAGUE RECORD BOOKS            "
        menuChoices$(6) = "EXPANDED LEADERS / STANDINGS   "
        menuChoices$(7) = "DELETE ALL STATS FOR A LEAGUE  "
        menuChoices$(8) = "RETURN TO MAIN MENU            "
        SELECTION% = 1
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
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call CREATLEAG

                Case 2:
                    Call CompileLeague

                Case 3:
                    Call SEELEAG

                Case 4:
                    Call HD2HDLEAG

                Case 5:
                    Call Records_Menu

                Case 6:
                    Call NEWLDR

                Case 7:
                    Call BLANKPRO

            End Select

        End If

    Loop Until SELECTION% = 8 Or ExitCode% = 27

End Sub


' General purpose
'$Include: 'KeyInput.bm'
'$Include: 'QPProEqu.bm'
'$Include: 'PadRight.bm'
'$Include: 'QBWRITE.bm'

' For Utils
'$Include: 'BoxRoutines.bm'
'$Include: 'ConfigureDrives.bm'
'$Include: 'FileTeamSelection.bm'
'$Include: 'CONVERT.BAS'
'$Include: 'TITLE.bm'
'$Include: 'ALIGN.BAS'
'$Include: 'BLANKPRO.BAS'
'$Include: 'CAREER.BAS'
'$Include: 'CAREDIT.BAS'
'$Include: 'CARLDR.BAS'
'$Include: 'CARVIEW.BAS'
'$Include: 'COMPARE.BAS'
'$Include: 'COMPLEAG.BAS'
'$Include: 'CREATLEAG.BAS'
'$Include: 'DRAFT.BAS'
'$Include: 'GPMIN.BAS'
'$Include: 'HD2HDLEAG.BAS'
'$Include: 'LOOKY.BAS'
'$Include: 'MERGE.BAS'
'$Include: 'NEWLDR.BAS'
'$Include: 'NEWLDRST.BAS'
'$Include: 'REC.BAS'
'$Include: 'RECORDS.BAS'
'$Include: 'SCHEDULE.BAS'
'$Include: 'SEELEAG.BAS'
'$Include: 'SINPUT.BAS'
'$Include: 'TINPUT.BAS'
'$Include: 'TRADE.BAS'

' For Game
'Include: 'PROHOOP.BAS'


'----------------------------------------
'           PROHOOP Subroutine
'----------------------------------------
'This subroutine serves as the main "source"
'for the action in the game. It steps the
'user through choosing game parameters and
'teams, as well as determining players to
'disable, etc.
Sub PROHOOP ()

    _Title "Basketball: The Pro Game v" + GAME_VERSION$ + " - HOOPS!"

    Shared actualAttendance&
    Shared avgAttendance&()

    Shared scheduleFile$

    Shared tickerStart

    Shared coachMode$(), foulOption$(), freeThrowOpt$()
    Shared location$(), playMode$(), playoffGame$()
    Shared quarterLength$(), sClockOpt$(), timeoutSetting$()
    Shared yesNoText$()

    Color 7, 0
    Cls

    Randomize Timer

    Call InitVar

    '----------------------------------------
    '         DETERMINE GAME OPTIONS
    '----------------------------------------

    If Not _FileExists("DEFAULT") Then

        Open "DEFAULT" For Output As #3

        'Player option
        Print #3, 3

        '3 Pt Option
        Print #3, 1

        'Shot Clock Setting
        Print #3, 0

        'Home Court
        Print #3, 1

        'Playoff Opt
        Print #3, 0

        'PBP Delay
        Print #3, 1.2

        'Show shot %?
        Print #3, 1

        'Coach option
        Print #3, 0

        'Bons Foul #
        Print #3, 6

        'FT Rules, PBP Option, Timeouts / Quarters
        For I = 1 To 5:
            Print #3, 0
        Next

        '# of timeouts
        Print #3, 7

        Close #3

    End If

    Open "DEFAULT" For Input As #4
    Input #4, playerOpt
    Input #4, threePtOpt
    Input #4, sClockVal
    Input #4, gameLoc
    Input #4, playoffOpt
    Input #4, pbpDelay!
    Input #4, shotPctOpt
    Input #4, coachOpt
    Input #4, bonusFoulNum
    Input #4, freeThrowVal
    Input #4, ftRulesOpt
    Input #4, pbpOpt
    If Not EOF(4) Then
        Input #4, timeoutOpt
        Input #4, quarterOpt
        Input #4, maxTimeouts
    End If
    Close #4

    If _FileExists("TICKER") Then

        Open "TICKER" For Input As #5

        For I = 1 To 32
            For I1 = 0 To 1
                For I2 = 1 To 14
                    Input #5, SX%(I, I1, I2)
                Next
            Next
        Next

        For I = 1 To 32
            For I1 = 0 To 1
                Input #5, SX$(I, I1)
            Next
        Next

        For I = 1 To 32
            Input #5, ST%(I)
        Next

        Close #5

        tickerGames = 1

    Else

        tickerGames = 0

    End If

    'Load game in-progress, if there is one
    If _FileExists("GAME82") Then

        Open "GAME82" For Input As #6

        Input #6, scheduleFile$

        For I = 0 To 2:
            Input #6, schedGame(I)
        Next

        Close #6

        currGame = schedGame(0) + schedGame(2)

        'Open "pbplog" + Str$(currGame) For Append As #11
        'Open "pbplog" For Append As #11

        If Not _FileExists(diskPaths$(3) + scheduleFile$) Then

            'Notify user that game won't execute
            MsgText$ = "CAN'T FIND OR LOAD TARGET SCHEDULE.  PRESS ANY KEY TO RETURN TO MENU"
            BoxWidth = 18
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)

            earlyExit = 1

        Else

            Open diskPaths$(3) + scheduleFile$ For Random As #7 Len = SCHEDULE_SIZE_BYTES

            For X = 0 To 21
                Field #7, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
            Next

            Field #7, 44 As X$, 2 As Q$(25), 2 As Q$(26), 5 As Q$(23), 5 As Q$(24)

            Get #7, schedGame(0) + schedGame(2)

            For X = 0 To 21:
                schedOptions(X) = CVI(Q$(X + 1))
            Next

            For X = 0 To 1:
                diskIDs$(X) = Q$(23 + X)
            Next

            VS% = CVI(Q$(25))
            HS% = CVI(Q$(26))

            Close #7

            autoPlay = 1

        End If

    Else

        'Open "pbplog" For Append As #11

    End If

    If autoPlay = 1 Then

        Call AUTOSETUP(goodTeam)

        If goodTeam = 0 Then

            'Notify user that game won't execute
            MsgText$ = "ATTEMPTED TO LOAD AN INVALID TEAM. PRESS ANY KEY TO RETURN TO MENU"
            BoxWidth = 18
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)

            earlyExit = 1

        End If

    Else

        ' USER SELECTION OF OPTIONS
        'gameLoc = 1 is neutral site
        'gameLoc = 0 is not used
        If gameLoc = 0 Then gameLoc = 1

        Do

            Color 15, 0
            Cls

            Color 2
            Print Tab(29); "SELECT OPENING OPTIONS"

            Locate 2, 2: Color 14
            Print "(1) ";
            Color 15: Print playMode$(playerOpt)
            Locate 3, 2: Color 14
            Print "(2) ";
            Color 15: Print "VISITING TEAM: ";
            Color teamRat_GAME(0, 33), teamRat_GAME(0, 34): Print gameTeams$(0)
            Locate 4, 2: Color 14, 0
            Print "(3) ";
            Color 15: Print "HOME TEAM: ";
            Color teamRat_GAME(1, 33), teamRat_GAME(1, 34): Print gameTeams$(1)
            Locate 5, 2: Color 14, 0
            Print "(4) ";
            Color 15: Print "GAME LOCATION: "; location$(gameLoc)
            Locate 6, 2: Color 14
            Print "(5) ";
            Color 15: Print "SHOT CLOCK: "; sClockOpt$(sClockVal)
            Locate 7, 2: Color 14
            Print "(6) ";
            Color 15: Print "QUARTER LENGTH: "; quarterLength$(quarterOpt)
            Locate 8, 2: Color 14
            Print "(7) ";
            Color 15: Print "NUMBER TIMEOUTS: "; maxTimeouts
            Locate 9, 2: Color 14
            Print "(8) ";
            Color 15: Print "TIMEOUT OPTIONS: "; timeoutSetting$(timeoutOpt)
            Locate 10, 2: Color 14
            Print "(9) ";
            Color 15: Print "START GAME"
            Locate 11, 2: Color 14
            Print "(0) ";
            Color 15: Print "SAVE SETTINGS"
            Locate 12, 2: Color 14
            Print "(A) ";
            Color 15: Print Using "PLAY-BY-PLAY DELAY (SECS): ##.#"; pbpDelay!
            Locate 13, 2: Color 14
            Print "(B) ";
            Color 15: Print "HUMAN COACHES: "; coachMode$(coachOpt)
            Locate 14, 2: Color 14
            Print "(C) ";
            Color 15: Print "GAME TYPE: "; playoffGame$(playoffOpt)
            Locate 15, 2: Color 14
            Print "(D) ";
            Color 15: Print "3-POINT SHOT: "; yesNoText$(threePtOpt)
            Locate 16, 2: Color 14
            Print "(E) ";
            Color 15: Print "FREE THROW RULES: "; foulOption$(ftRulesOpt)
            Locate 17, 2: Color 14
            Print "(F) ";
            Color 15: Print "NUMBER OF FREE THROWS: "; freeThrowOpt$(freeThrowVal)
            Locate 18, 2: Color 14
            Print "(G) ";
            Color 15: Print "BONUS ON FOUL #: "; bonusFoulNum
            Locate 19, 2: Color 14
            Print "(H) ";
            Color 15: Print "PLAY-BY-PLAY STYLE: "; pbpStyle$(pbpOpt)

            'Locate 18, 2: Color 14
            'Print "NOTE:: ";: Color 15: Print "BONUS WAS ON 7TH FOUL (UNTIL 72-73), 6TH FOUL (72-73 SEASON)"
            'Locate , 2: Print "       5TH FOUL (1973-74 TO PRESENT)"
            'Locate , 2: Color 14
            'Print "NOTE:: ";: Color 15: Print "UP UNTIL 1972-73, WHEN A TEAM WAS IN THE BONUS, THEY HAD THE CHANCE"
            'Locate , 2: Print "       TO SHOOT 3 FT TO MAKE 2 OR 2 FT TO MAKE 1"
            'Locate , 2: Color 14
            'Print "NOTE:: ";: Color 15: Print "UP UNTIL 1972-73, ALL FOULS WERE SHOT...UNLIKE THE CURRENT RULES"
            'Locate , 2: Color 14
            'Print "NOTE:: ";: Color 15: Print "THE 3-PT SHOT WAS USED FOR ALL ABA SEASONS AND NBA SINCE 1979-80"
            'Locate , 2: Color 14
            'Print "NOTE:: ";: Color 15: Print "THE ABA USED A 30-SEC SHOT CLOCK THROUGH 1973-74";

            Locate 20, 2: Color 14
            Print "NOTE:: "
            Color 15
            Locate , 2: Print "* BONUS ON 7TH FOUL UNTIL 72-73, 6TH FOUL (72-73 SEASON), 5TH FOUL (73-74+)"
            Locate , 2: Print "* UNTIL 72-73, BONUS FTs WERE 3 TO MAKE 2 | 2 TO MAKE 1; ALL FOULS SHOT"
            Locate , 2: Print "* 3-PT SHOT WAS USED FOR ALL ABA SEASONS AND NBA SINCE 79-80"
            Locate , 2: Print "* ABA HAD 30-SEC SHOT CLOCK THROUGH 73-74; NBA BEGAN 24-SEC SOT CLOCK IN 1954";

            Do

                Color 7

                I$ = GetKeyPress$

                Call LetterToNumber(I$, setting)

            Loop Until setting >= 0 And setting <= 17 Or I$ = Chr$(27) Or UCase$(I$) = "Q"

            If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

                Select Case setting

                    Case 0:
                        Open "DEFAULT" For Output As #8
                        Print #8, playerOpt
                        Print #8, threePtOpt
                        Print #8, sClockVal
                        Print #8, gameLoc
                        Print #8, playoffOpt
                        Print #8, pbpDelay!
                        Print #8, shotPctOpt
                        Print #8, coachOpt
                        Print #8, bonusFoulNum
                        Print #8, freeThrowVal
                        Print #8, ftRulesOpt
                        Print #8, pbpOpt
                        Print #8, timeoutOpt
                        Print #8, quarterOpt
                        Print #8, maxTimeouts
                        Close #8

                    Case 1:
                        playerOpt = playerOpt + 1
                        If playerOpt > 3 Then playerOpt = 0

                    Case 2:
                        Call ChooseGameTeam(diskIDs$(0), NTMS, teamNbr, gameTeams$(0), 0)
                        If teamNbr <> 0 Then Call DISABLEPLAYERS(0)

                    Case 3:
                        Call ChooseGameTeam(diskIDs$(1), NTMS, teamNbr, gameTeams$(1), 1)
                        If teamNbr <> 0 Then Call DISABLEPLAYERS(1)

                    Case 4:
                        gameLoc = gameLoc + 1
                        If gameLoc > 2 Then gameLoc = 1

                    Case 5:
                        sClockVal = sClockVal + 1
                        If sClockVal > 2 Then sClockVal = 0

                    Case 6:
                        quarterOpt = 1 - quarterOpt

                    Case 7:
                        maxTimeouts = maxTimeouts + 1
                        If maxTimeouts > 7 Then maxTimeouts = 4

                    Case 8:
                        timeoutOpt = timeoutOpt + 1
                        If timeoutOpt > 4 Then timeoutOpt = 0

                    Case 9:
                        '

                    Case 10:
                        Do
                            Locate 12, 32
                            Input pbpDelay!
                        Loop Until pbpDelay! >= 0

                    Case 11:
                        coachOpt = 1 - coachOpt
                        'shotPctOpt = 1 - shotPctOpt

                    Case 12:
                        playoffOpt = 1 - playoffOpt

                    Case 13:
                        threePtOpt = threePtOpt + 1
                        If threePtOpt > 1 Then threePtOpt = 0

                    Case 14:
                        ftRulesOpt = 1 - ftRulesOpt

                    Case 15:
                        freeThrowVal = 1 - freeThrowVal

                    Case 16:
                        bonusFoulNum = bonusFoulNum + 1
                        If bonusFoulNum > 7 Then bonusFoulNum = 5

                    Case 17:
                        pbpOpt = 1 - pbpOpt
 
                End Select

            End If

        Loop Until (setting = 9 And gameTeams$(0) <> "" And gameTeams$(1) <> "") Or I$ = Chr$(27)

        If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

            'TICKER PROMPT
            Cls
            Color 3

            Call TEAMBOX(7, 1)

            Locate 7, 11

            Print "IF YOU ARE USING THE 10 MINUTE TICKER OPTION FOR THIS GAME,"
            Locate , 11: Print "ENTER STARTING TIME FOR GAME IN THIS FORMAT"
            Locate , 11: Print "(EXAMPLES: 1:00P, 11:30A). OTHERWISE, JUST HIT ENTER"
            Print
            Locate , 11: Input "START TIME: "; startTime$

            L = Len(startTime$)

            If L < 4 Then
                startTime$ = "1:00P"
            Else
                If L = 5 Then HR = Val(Left$(startTime$, 1)): MIN = Val(Mid$(startTime$, 3, 2))
                If L = 6 Then HR = Val(Left$(startTime$, 2)): MIN = Val(Mid$(startTime$, 4, 2))
            End If

            tickerMeridian$ = Right$(startTime$, 1)

            If UCase$(tickerMeridian$) = "P" And Left$(startTime$, 2) <> "12" Then HR = HR + 12

            TM = HR * 60 + MIN
            tickerStart = Int(TM / 30) - 18

        Else

            earlyExit = 1

        End If

    End If

    'If we've run into issues so far...
    If earlyExit = 1 Then Exit Sub

    '----------------------------------------
    '    AUTO-DETERMINE OTHER OPTIONS
    '----------------------------------------

    ' Determine computer team
    Select Case playerOpt

        Case 1:
            playerMode = 1
            compTeam = 0

        Case 2:
            playerMode = 1
            compTeam = 1

        Case 3:
            playerMode = 2

    End Select

    'playerMode represents the following values:
    '   0 = Human
    '   1 = Computer Team
    '   2 = PC vs. PC

    For I = 0 To 1:
        'If disk ID has alpha-char, or is only 3 characters,
        'then assume the year is the first 2 chars
        If CheckString%(diskIDs$(I)) = 1 Or Len(diskIDs$(I)) <= 3 Then
            teamYears(I) = GetTeamYear%(Left$(gameTeams$(I), 2))
        Else
            'If we have 4 or more characters,
            'then Val will return the first 4 digits
            teamYears(I) = Val(RTrim$(diskIDs$(I)))
        End If

    Next I

    If quarterOpt = 0 Then
        MINS_PER_GAME = 48
    Else
        MINS_PER_GAME = 40
    End If

    Call SETUPRATINGS(rulesStyle)
    Call ChangeContributions
    Call LOADSTATS

    If teamRat_GAME(0, 33) = teamRat_GAME(1, 33) And teamRat_GAME(0, 34) = teamRat_GAME(1, 34) Then
        Call ColorChange
    End If

    'Determine attendance
    If gameLoc = 2 Or avgAttendance&(1) = 0 Then
        'Neutral Site
        'Random # 12,000 - 22,000
        actualAttendance& = Int(Rnd(1) * 10000) + 12000
    Else
        'Average attendance +/ 10%
        'College attendance ranges 80% to 100% of max capacity
        margin = Int(avgAttendance&(1) * .10)
        actualAttendance& = avgAttendance&(1) + Int(Rnd(1) * margin * 2) - margin

        'Average attendance +/- 5,000
        'actualAttendance& = avgAttendance&(1) + Int(Rnd(1) * 10000) - 5000
    End If

    LS = 0

    Select Case sClockVal
        Case 0: shotClock = 24
        Case 1: shotClock = 30
    End Select

    If gameClock! < 6 And LT <> 4 Then
        LS = 4
    End If

    'Determine subs / play parameters
    For P9 = 0 To 1

        If playerMode = 1 And P9 = compTeam Or playerMode = 2 Then
            Call ComputerCoach
        Else
            Call PlayStyle
            Call PlayItSafe
        End If

    Next P9

    Call StealAdjustments

    If autoPlay <> 1 And playerMode <> 2 Then
        whistle& = _SndOpen("whistle.mp3")
        swish& = _SndOpen("swish.mp3")
        backboard& = _SndOpen("backboard.mp3")
    End If

    '10 minutes quarters have fewer timeouts.
    'Currently only WNBA rules are considered,
    'although other BB scenarios have 10-min quarters.
    If quarterOpt = 0 Then
        timeouts(0) = maxTimeouts
        timeouts(1) = maxTimeouts
    Else
        timeouts(0) = maxTimeouts - 2
        timeouts(1) = maxTimeouts - 2
    End If

    'Set game clock
    If quarterOpt = 0 Then
        gameClock! = 720
    Else
        gameClock! = 600
    End If

    Call CheckGameClock

    'Figure out which team to favor
    'for trends happening over decades.
    If gameLoc = 2 Then
        impactTeam = 0
    Else
        impactTeam = gameLoc
    End If

    250 '
    'Jump Ball!
    Select Case quarter

        Case 1, Is >= 5
            jumpBall = Int(Rnd(1) * 2)
            P = jumpBall
            D = 1 - jumpBall

        Case 2, 3
            If teamYears(rulesStyle) >= 1975 Then
                P = 1 - jumpBall
                D = 1 - P
            Else
                'NBA used jump ball every quarter before 1975
                jumpBall = Int(Rnd(1) * 2)
                P = jumpBall
                D = 1 - jumpBall
            End If

        Case 4
            If teamYears(rulesStyle) >= 1975 Then
                P = jumpBall
                D = 1 - jumpBall
            Else
                'NBA used jump ball every quarter before 1975
                jumpBall = Int(Rnd(1) * 2)
                P = jumpBall
                D = 1 - jumpBall
            End If

    End Select

    Call ChangePossession

    Call SCOREBOARD

    Call LastShot
    Call CheckGameClock
    If gameClock! = 0 Then 170

    Call ResetShotClock

    '----------------------------------------
    '                Tip Off!
    '----------------------------------------

    If quarter = 1 Or quarter > 4 Then
        pbpString$ = gameTeams$(jumpBall) + " GETS THE JUMP"
    Else
        pbpString$ = gameTeams$(jumpBall) + " INBOUNDS THE BALL"
    End If

    If (autoPlay = 0 Or pbpDelay! > 0) Then
        Call SELECTPBP(jumpBall, 1)
    End If

    P = jumpBall
    P9 = jumpBall

    GoTo 300

    '----- NOODLE -----

    170 '
    totalFouls(0) = totalFouls(0) + teamFouls(0)
    totalFouls(1) = totalFouls(1) + teamFouls(1)
    Erase teamFouls

    gameClock! = 0
    Call QUARTERTOTAL

    quarter = quarter + 1

    ' *** ADJUST TIMEOUTS ***
    For PG = 0 To 1

        Select Case quarter

            Case 4
                '# 4th quarter timeouts (Men's Basketball)
                If timeouts(PG) > (maxTimeouts - 3) And teamGender$(eraChoice) = "HE" Then
                    timeouts(PG) = (maxTimeouts - 3)
                End If

            Case Is > 4
                '# Overtime timeouts
                If (teamYears(0) >= 1985 And teamYears(1) >= 1985) And (teamYears(0) <= 2000 And teamYears(1) <= 2000) Then
                    'Between 1984-85 And 1999-2000
                    timeouts(PG) = 3
                Else
                    timeouts(PG) = 2
                End If

        End Select

        'Reset mandatoryTO flag
        mandatoryTO(PG) = 0

    Next

    offStatus = 0
    XS = 1

    pbpString$ = "END OF QUARTER!"

    Select Case pbpOpt

        Case 0:
            Color 7, 0
            Call WINDEX
            Print pbpString$

        Case 1:
            If pbpDelay! > 0 Then
                Call WINDEX
                Print pbpString$
            End If

    End Select

    If quarter = 3 Then

        pbpString$ = "HALFTIME!"

        Select Case pbpOpt

            Case 0:
                Color 7, 0
                Call WINDEX
                Print pbpString$

            Case 1:
                Call WINDEX
                Print pbpString$
                Call DELAY

        End Select

        Call GetKeyWhenNoDelay

        halfTime = 1
        Call BOXOPTIONS(0)
        gameOver = 0
        Call TICKER
        halfTime = 0

        Call FOULHARD

    End If

    If quarter <= 4 And playerMode <> 0 Then

        If playerMode = 1 Then
            P9 = compTeam
            Call ComputerCoach
        End If

        If playerMode = 2 Then
            P9 = 0
            Call ComputerCoach
            P9 = 1
            Call ComputerCoach
        End If

    End If

    'New quarter
    If quarter <= 4 Then

        If quarterOpt = 0 Then
            gameClock! = 720
        Else
            gameClock! = 600
        End If

        Call CheckGameClock
        Call SCOREBOARD

        ballCarrier = -1
        Call Substitutions

        Call UPDATESCREEN

        GoTo 250

    End If

    Call CheckGameClock

    Call SCOREBOARD

    If quarter = 5 Then

        pbpString$ = "END OF REGULATION"

        Select Case pbpOpt

            Case 0:
                Color 7, 0
                Call WINDEX
                Print pbpString$

            Case 1:
                If pbpDelay! > 0 Then
                    Call WINDEX
                    Print pbpString$
                End If

        End Select

    End If

    Locate 2, 31
    Print "END OF REGULATION"

    'If scores are tied, it's Overtime!
    If score(0, 0) = score(1, 0) Then

        pbpString$ = "OVERTIME !!!"

        Select Case pbpOpt

            Case 0:
                Color 7, 0
                Call WINDEX
                Print pbpString$

            Case 1:
                Call WINDEX
                Print pbpString$
                Call DELAY

        End Select

        gameClock! = 300
        Call UPDATESCREEN

        ballCarrier = -1
        Call Substitutions

        GoTo 250

    End If

    Call GetKeyWhenNoDelay
    Call EndOfGame

    Exit Sub

    '----------------------------------------
    '           END OF GAME!!!
    '----------------------------------------

    '================================================================================
    ' Below are all the different GoTo branches for the gameplay
    ' These have not been worked out into a different flow
    ' yet for the game's logic.
    '================================================================================

    300 '
    FO = 0
    LS = 0
    XS = 0

    'Pro Basketball does not have the "last 5 seconds" dynamic
    'that is in college basketball

    'If there is time on the shot clock
    If sClockVal < 2 And shotClock > 0 And shotClock < scSettings(sClockVal) Then

        shotClock = shotClock - 5

    Else

        'Reset shot clock
        Select Case sClockVal
            Case 0: shotClock = 24
            Case 1: shotClock = 30
        End Select

        LS = 0

        D = 1 - P

        If gameClock! < 6 And LT <> 4 Then
            LS = 4
        End If

    End If

    passes = 1
    assistShotBoost = 0
    shotChance = 0

    D6 = 0
    A6 = 0

    If offStatus <> 90 Then offStatus = 0

    400 '
    D6 = 0
    P3 = 0
    assistShotBoost = 0

    If (playerMode = 0 And coachOpt = 1) Or (playerMode = 1 And D = compTeam And coachOpt = 1) Or (playerMode = 1 And P = compTeam) Or playerMode = 2 Then

        Call WHOGETSBALL

    Else

        If d100% > 40 Then
            'random ball carrier
            ballCarrier = Int(Rnd(1) * 5)
        Else
            Call GetPassChoice
        End If

    End If

    If defPress = 0 Then

        RN0 = Int(Rnd(1) * 1000) + 1

        ' *** BRANCH TO AREA BASED ON DEFENSE ***
        Select Case defStyles(D)

            Case 1, 4, 7:
                ' *** SHOT VS REG M-M, TIGHT M-M, 1-3-1 ZONE ***

                Select Case RN0

                    Case 1, 2
                        'Offensive Foul
                        offStatus = 0
                        GoTo 580

                    Case 3 To 30
                        'Personal Foul
                        Call LastShot
                        Call ResetShotClock
                        Call UPDATESCREEN
                        offStatus = 88
                        Call FoulCalled(1)

                        GoTo 566

                    Case 31 To 31 + teamAdj(P, 1) + teamAdj(D, 2)
                        'Turnover
                        GoTo 515

                    Case 32 + teamAdj(P, 1) + teamAdj(D, 2) To 1000
                        P9 = Int(Rnd(1) * 5)

                        'Check for steal
                        Select Case defStyles(D)
                            
                            Case 1, 4
                                stealBaseAdj = 24
                                
                            Case 7
                                stealBaseAdj = 45

                        End Select
                        
                        'Compare roll to 24 + several adjustments for steals
                        If RN0 <= stealBaseAdj + teamAdj(P, 0) + teamAdj(P, 1) + teamAdj(D, 2) + gameRatings!(D, lineupIdx(D, P9), 5) + stealAdj(offStyles(P)) Then

                            GoTo 513

                        Else

                            If DecideShot% = 1 Then
                                'We're either shooting based on team pace
                                'Shoot!
                                GoTo 445
                            Else
                                'Difficult Shot
                                GoTo 437
                            End If

                        End If

                End Select
            
            Case 2, 5, 6, 8, 9
                ' *** SHOT VS LOOSE M-M, SAG M-M, ALL ZONE EXCEPT 1-3-1 ***

                Select Case RN0

                    Case 1
                        'Offensive Foul
                        offStatus = 0
                        GoTo 580

                    Case 2 To 10
                        'Personal Foul
                        Call LastShot
                        Call ResetShotClock
                        Call UPDATESCREEN
                        offStatus = 88
                        Call FoulCalled(1)

                        GoTo 566

                    Case 10 To 10 + teamAdj(P, 1) + teamAdj(D, 2)
                        'Turnover
                        GoTo 515

                    Case 11 + teamAdj(P, 1) + teamAdj(D, 2) To 1000
                        P9 = Int(Rnd(1) * 5)

                        'Check for steal
                        Select Case defStyles(D)

                            Case 2, 5
                                stealBaseAdj = 8

                            Case 6
                                stealBaseAdj = 24

                            Case 8
                                stealBaseAdj = 16

                            Case 9
                                stealBaseAdj = 28

                        End Select

                        stealChance = stealBaseAdj + teamAdj(P, 0) + teamAdj(P, 1) + teamAdj(D, 2) + gameRatings!(D, lineupIdx(D, P9), 5) + stealAdj(offStyles(P))
                        If RN0 <= stealChance Then

                            GoTo 513

                        Else

                            If DecideShot% = 1 Then
                                'Shoot!
                                GoTo 445
                            End If

                        End If

                End Select

            Case 3
                ' *** SHOT VS TRAP M-M ***

                Select Case RN0

                    Case 1 To 3
                        'Offensive Foul
                        offStatus = 0
                        GoTo 580

                    Case 4 To 50
                        'Personal Foul
                        Call LastShot
                        Call ResetShotClock
                        Call UPDATESCREEN
                        offStatus = 88
                        Call FoulCalled(1)

                        GoTo 566

                    Case 50 To 50 + teamAdj(P, 1) + teamAdj(D, 2)
                        'Turnover
                        GoTo 515

                    Case 51 + teamAdj(P, 1) + teamAdj(D, 2) To 1000
                        P9 = Int(Rnd(1) * 5)

                        'Check for steal
                        stealBaseAdj = 40

                        stealChance = stealBaseAdj + teamAdj(P, 1) + teamAdj(D, 2) + gameRatings!(D, lineupIdx(D, P9), 5) + teamAdj(P, 0) + stealAdj(offStyles(P))
                        If RN0 <= stealChance Then

                            GoTo 513

                        Else

                            If DecideShot% = 1 Then
                                'Shoot!
                                GoTo 445
                            End If

                        End If

                End Select

        End Select

    Else

        'Choices for a PRESS
        defPress = 0
        RN0 = Int(Rnd(1) * 1000) + 1

        Select Case RN0

            Case 1 To 3
                'Offensive Foul
                offStatus = 0
                GoTo 580

            Case 4 To 50
                'Personal Foul
                Call LastShot
                Call ResetShotClock
                Call UPDATESCREEN
                offStatus = 88
                Call FoulCalled(1)
                
                GoTo 566

            Case 50 To 50 + teamAdj(P, 1) + teamAdj(D, 2)
                'Turnover
                GoTo 515

            Case 50 + teamAdj(P, 1) + teamAdj(D, 2) To 1000
                P9 = Int(Rnd(1) * 5)

                'Check for steal
                stealChance = 40 + teamAdj(P, 0) + teamAdj(P, 1) + teamAdj(D, 2) + gameRatings!(D, lineupIdx(D, P9), 5)
                If RN0 <= stealChance Then

                    GoTo 513

                Else

                    If DecideShot% = 1 Then
                        'Shoot!
                        GoTo 445
                    End If

                End If

        End Select

    End If


    ' ***BAD SHOT ***

    437 '
    assistShotBoost = 0
    shotType = 1
    offStatus = 0

    threePtShotChance = Int(Rnd(1) * 7) + 3

    RN0 = Int(Rnd(1) * 3) + 1

    'Adjust shot % based on player's FG%
    If gameRatings!(P, lineupIdx(P, ballCarrier), 0) > 60 Then

        Select Case RN0
            Case 1
                shotChance = 24
            Case 2
                shotChance = 30
            Case 3
                shotChance = 36
        End Select

    Else

        Select Case RN0
            Case 1
                shotChance = gameRatings!(P, lineupIdx(P, ballCarrier), 0) * .4
            Case 2
                shotChance = gameRatings!(P, lineupIdx(P, ballCarrier), 0) * .5
            Case 3
                shotChance = gameRatings!(P, lineupIdx(P, ballCarrier), 0) * .6
        End Select

    End If

    '-- Missing code for TIEUP

    GoTo 520

    '----- NOODLE -----

    445 '
    If Not ((quarter = 4 And gameClock! <= 120) Or quarter > 4) Then

        If playerFatigue%(P, lineupIdx(P, ballCarrier)) <= 0 Then
            'Fatigued - No Shot / Difficult Shot
            GoTo 437
        End If

    End If

    ' *** CHECKING 3-POINT SHOT ***

    If threePtOpt = 1 Then

        'Shot chance based on user's 3FG make %
        'For reasons I haven't determined, without the +15
        'the shot percentages are consistently low by .100
        threePtShotChance = gameRatings!(P, lineupIdx(P, ballCarrier), 20) + teamAdj(D, 4) + 15

        If teamYears(eraChoice) < 2013 Then
            'Without subtracting 3, players always shoot 25% more
            'frequent than they should. Given that FGAs are accurate,
            'it's not clear to me why this happens.
            threeFGChance = gameRatings!(P, lineupIdx(P, ballCarrier), 19) - 3
        Else
            'Favor modern outcomes if teams are
            'from different eras
            threeFGChance = gameRatings!(P, lineupIdx(P, ballCarrier), 19)
        End If

        'More frequent 3FG's with 10 min quarters (faster paced)
        'This is just a general observation that needs more confirmation
        'It could be that women's basketball is why it happens more frequent
        If quarterOpt = 1 Then threeFGChance = threeFGChance + 8

        If d100% <= threeFGChance Then
            'Take 3FGA within player's frequency
            shotType = 4
            offStatus = 0
            threePtShotChance = threePtShotChance + Int(Rnd(1) * 7) - 3
            GoTo 520
        End If

        '3 Point Shot chosen by user
        If playChoice = 4 Then

            'Forced 3FGA = harder shot
            If d100% <= 20 Then

                'No Shot
                GoTo 437

            Else
                'Difficult shot - reduced chance to hit
                shotType = 4
                offStatus = 0
                threePtShotChance = .33 * threePtShotChance
                GoTo 520
            End If

        Else

            'A 3-pointer wasn't chosen but we're in tight circumstances
            '...behind with 30 secs left and our ball carrier has shot 3FGA
            If quarter >= 4 And gameClock! < 30 And score(D, 0) - score(P, 0) >= 3 And gameRatings!(P, lineupIdx(P, ballCarrier), 19) >= 3 Then

                shotType = 4
                offStatus = 0
                GoTo 520

            End If

        End If

    End If

    ' *** CHECKING FOR ASSISTED SHOT ***
    'playChoice = 1
    Call SHOTPERCENT(1)

    GoTo 520

    '----- NOODLE -----


    ' *** SUCCESSFUL STEAL ***

    513 '

    Call LastShot
    Call ResetShotClock

    gameRatings!(P, lineupIdx(P, ballCarrier), 23) = gameRatings!(P, lineupIdx(P, ballCarrier), 23) + 1

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        If pbpOpt = 1 Then

            Call WINDEX
            Print "BALL STOLEN BY "; players_GAME$(D, lineupIdx(D, P9), 0); "!"
            Call DELAY

        Else

            Call Rolld100(RN0)

            Select Case RN0

                Case 1 To 3:
                    pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " QUICKLY REACHES IN!"

                Case 4 To 6:
                    pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " SNEAKS IN FROM BEHIND!"

                Case 7 To 10:
                    pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " GETS A HAND ON THE BALL!"

            End Select

            Call PBP(D)

            RN0 = Int(Rnd(1) * 20) + 1

            Select Case RN0

                Case 1: pbpString$ = "PASS INTERCEPTED BY " + players_GAME$(D, lineupIdx(D, P9), 0) + "!"

                Case 2: pbpString$ = "PASS IS PICKED OFF BY " + players_GAME$(D, lineupIdx(D, P9), 0) + "!"

                Case 3: pbpString$ = "BALL IS STOLEN BY " + players_GAME$(D, lineupIdx(D, P9), 0) + "!"

                Case 4: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " WITH THE STEAL!"

                Case 5: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " TAKES IT AWAY!"

                Case 6: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " STEALS IT!"

                Case 7: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " RIPS IT LOOSE!"

                Case 8: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " TAKES THE BALL AWAY!"

                Case 9: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " STEALS THE BALL!"

                Case 10: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " PICKS IT OFF!"

                Case 11: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " SWIPES THE BALL!"

                Case 12: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " WITH THE STOLEN BALL!"

                Case 13: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " STRIPS IT AWAY!"

                Case 14: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " WITH THE STEAL!"

                Case 15: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " SWIPES AND STEALS IT!"

                Case 16: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " SNATCHES IT AWAY!"

                Case 17: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " QUICKLY REACHES IN!"

                Case 18: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " TAKES IT THE OTHER WAY!"

                Case 19: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " POKES IT LOOSE!"

                Case 20: pbpString$ = players_GAME$(D, lineupIdx(D, P9), 0) + " STRIPS IT CLEAN!"

            End Select

            Call PBP(D)

        End If

    End If

    'Increase steals (as well as team TOs)
    gameRatings!(D, lineupIdx(D, P9), 13) = gameRatings!(D, lineupIdx(D, P9), 13) + 1
    turnovers(P) = turnovers(P) + 1

    Call UPDATESCREEN

    C1 = 2

    Call CheckGameClock
    If gameClock! = 0 Then 170

    Call ChangePossession
    P9 = D

    fastBreak = 1

    GoTo 300

    '----- NOODLE -----

    515 '

    ' *** TURNOVER ***

    Call LastShot
    Call ResetShotClock

    'Increase player and team TO's
    gameRatings!(P, lineupIdx(P, ballCarrier), 23) = gameRatings!(P, lineupIdx(P, ballCarrier), 23) + 1
    turnovers(P) = turnovers(P) + 1

    If shotViolation <> 1 Then
        pbpType = 1
        Call MAKEPBP(pbpType)
    End If

    shotViolation = 0

    pbpString$ = "TURNOVER BY " + gameTeams$(P)
    Call SELECTPBP(P, 1)

    pbpString$ = "BALL OUT OF BOUNDS TO " + gameTeams$(D)
    Call SELECTPBP(D, 0)
    Call DELAY

    Call UPDATESCREEN

    Locate 10, 1

    Call CheckGameClock
    If gameClock! = 0 Then 170

    Call ChangePossession

    Call Substitutions

    GoTo 300

    '----- NOODLE -----

    520 '

    Call LastShot

    Call CheckGameClock
    If gameClock! = 0 Then 170

    If shotType = 4 And threePtShotChance <= 0 Then
        threePtShotChance = 3
    End If

    '--> only set this if we need to track where code is going
    'If autoPlay = 1 Then
    If (autoPlay = 0 Or pbpDelay! > 0) Then

        If shotChance > 70 And fastBreak = 1 And passes = 1 Then

            pbpType = 6
            shotType = 8
            Call MAKEPBP(pbpType)
            Call PBP(P)

        Else

            If players_GAME$(P, lineupIdx(P, ballCarrier), 0) <> prevBall$ Then

                If prevBall$ = "" Then
                    pbpString$ = "PASS TO " + players_GAME$(P, lineupIdx(P, ballCarrier), 0)
                Else
                    pbpType = 10
                    Call MAKEPBP(pbpType)
                End If

            Else

                pbpType = 9
                Call MAKEPBP(pbpType)

            End If

            Call PBP(P)

            If sClockVal < 2 And shotClock <= 0 Then
                shotViolation = 1
                pbpString$ = "SHOT CLOCK HAS EXPIRED!"
                GoTo 515
            End If

            If sClockVal < 2 And shotClock < 5 Then
                pbpString$ = "SHOT CLOCK DOWN TO" + Str$(shotClock)
                Call PBP(P)
            End If

            prevBall$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0)
            activePosition$ = Left$(players_GAME$(P, lineupIdx(P, ballCarrier), 1), 1)

            If activePosition$ = "S" Or activePosition$ = "P" Then
                activePosition$ = Right$(players_GAME$(P, lineupIdx(P, ballCarrier), 1), 1)
            End If

            tgtDefender$ = players_GAME$(D, lineupIdx(D, ballCarrier), 0)

            Select Case shotType

                Case 1:

                    If (autoPlay = 0 Or pbpDelay! > 0) Then

                        RN0 = Int(Rnd(1) * 12) + 1

                        Select Case RN0
                            Case 1:
                                pbpString$ = teamGender$(P) + "'S HEAVILY GUARDED"
                            Case 2:
                                pbpString$ = teamGender$(P) + "'S GUARDED"
                            Case 3:
                                pbpString$ = teamGender$(P) + "'S GUARDED BY " + tgtDefender$
                            Case 4, 5:
                                If teamGender$(P) = "SHE" Then
                                    pbpString$ = teamGender$(P) + " HAS A DEFENDER ON HER"
                                Else
                                    pbpString$ = teamGender$(P) + " HAS A MAN ON HIM"
                                End If
                            Case 6:
                                pbpString$ = teamGender$(P) + "'S COVERED"
                            Case 7:
                                pbpString$ = teamGender$(P) + " CAN'T GET AROUND THE DEFENDER"
                            Case 8:
                                pbpString$ = teamGender$(P) + "'S PRESSURED"
                            Case 9:
                                pbpString$ = teamGender$(P) + " HAS NOWHERE TO GO"
                            Case 10:
                                If teamGender$(P) = "SHE" Then
                                    pbpString$ = teamGender$(P) + " HAS A HAND IN HER FACE"
                                Else
                                    pbpString$ = teamGender$(P) + " HAS A HAND IN HIS FACE"
                                End If
                            Case 11:
                                pbpString$ = teamGender$(P) + "'S WATCHED BY " + tgtDefender$
                            Case 12:
                                pbpString$ = teamGender$(P) + "'S WORKING ON " + tgtDefender$
                        End Select
                    End If

                    Call PBP(P)

                Case 2:
                    pbpString$ = teamGender$(P) + " WORKS FREE INSIDE"
                    Call PBP(P)
                    pbpString$ = teamGender$(P) + " HAS AN OPEN SHOT"
                    Call PBP(P)

                Case 3:

                    If (autoPlay = 0 Or pbpDelay! > 0) Then

                        Call Rolld100(RN0)

                        If activePosition$ = "C" Then

                            Select Case RN0

                                Case 1 To 25:
                                    pbpString$ = teamGender$(P) + " MAKES A MOVE DOWN LOW":

                                Case 26 To 50:
                                    If teamGender$(P) = "SHE" Then
                                        pbpString$ = teamGender$(P) + " POSITIONS HERSELF DOWN LOW"
                                    Else
                                        pbpString$ = teamGender$(P) + " POSITIONS HIMSELF DOWN LOW"
                                    End If

                                Case 51 To 75:
                                    pbpString$ = teamGender$(P) + " POSTS UP UNDERNEATH"

                                Case 76 To 100:
                                    pbpString$ = teamGender$(P) + " BACKS IN AGAINST " + players_GAME$(D, lineupIdx(D, 4), 0)

                            End Select

                            Call PBP(P)
                            pbpString$ = teamGender$(P) + " HAS A GOOD SHOT INSIDE"
                            Call PBP(P)

                        End If

                        If activePosition$ = "F" Then

                            Select Case RN0

                                Case 1 To 17:
                                    pbpString$ = teamGender$(P) + " DRIVES THE BASELINE"

                                Case 18 To 34:
                                    pbpString$ = teamGender$(P) + " PENETRATES THE LANE"

                                Case 35 To 50:
                                    pbpString$ = teamGender$(P) + " MOVES INSIDE"

                                Case 51 To 67:
                                    pbpString$ = teamGender$(P) + " DRIVES THE LANE"

                                Case 68 To 84:
                                    pbpString$ = teamGender$(P) + " MOVES INTO THE PAINT"

                                Case 85 To 100:
                                    pbpString$ = teamGender$(P) + " DRIVES UNDERNEATH"

                            End Select

                            Call PBP(P)
                            pbpString$ = teamGender$(P) + "'S GOT A GOOD SHOT UNDERNEATH"
                            Call PBP(P)

                        End If

                        If activePosition$ = "G" Then

                            Select Case RN0

                                Case 1 To 25:
                                    pbpString$ = teamGender$(P) + " DRIVES THE LANE"
                                    Call PBP(P)
                                    pbpString$ = teamGender$(P) + " HAS A GOOD SHOT CLOSE IN"

                                Case 26 To 50:
                                    pbpString$ = teamGender$(P) + " DRIVES THROUGH THE KEY"
                                    Call PBP(P)
                                    pbpString$ = teamGender$(P) + "'S INTO THE LANE"
                                    Call PBP(P)
                                    pbpString$ = teamGender$(P) + " HAS A GOOD SHOT CLOSE IN"

                                Case 51 To 75
                                    pbpString$ = teamGender$(P) + " DRIVES TO THE BASKET"
                                    Call PBP(P)
                                    pbpString$ = teamGender$(P) + " HAS A GOOD SHOT CLOSE IN"
                            End Select

                            Call PBP(P)

                        Else
                            pbpString$ = teamGender$(P) + " DRIVES THROUGH AN OPENING"
                            Call PBP(P)

                        End If

                    End If

                Case 4:
                    pbpString$ = teamGender$(P) + "'S OPEN FROM 3-POINT RANGE!"
                    Call PBP(P)

                Case 5:
                    pbpString$ = teamGender$(P) + "'S OPEN FROM CLOSE RANGE"
                    Call PBP(P)

                Case 6:
                    pbpString$ = teamGender$(P) + "'S OPEN FROM MEDIUM RANGE"
                    Call PBP(P)

                Case 7:
                    pbpString$ = teamGender$(P) + "'S OPEN FROM LONG RANGE"
                    Call PBP(P)

            End Select

        End If

    End If

    529 '
    If shotChance <= 0 Then shotChance = 1
    If shotChance >= 100 Then shotChance = 99

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        If pbpOpt = 1 Then

            Call WINDEX
            passString$ = "PASS TO " + players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " OF "
            Print passString$;
            Color teamRat_GAME(P, 33), teamRat_GAME(P, 34)

            If Len(passString$) + Len(gameTeams$(P)) > 38 Then
                Print Left$(gameTeams$(P), 38 - Len(passString$))
            Else
                Print gameTeams$(P)
            End If

            Color 15, 0

            Locate 10
            If shotType = 4 Then
                Print "3 PT SHOT %"; CInt(threePtShotChance);
                Print "*";
            Else
                Print "SHOT %"; CInt(shotChance);
                If shotType >= 2 And shotType <= 8 Then
                    Print "*"; Space$(5);
                End If
            End If

            Call DELAY

        Else

            Locate 8, 41
            If shotType = 4 Then
                Print Using "SHOT % ##"; CInt(threePtShotChance);
                Print "*";
            Else
                Print Using "SHOT % ##"; CInt(shotChance);
                If shotType >= 2 And shotType <= 8 Then
                    Print "*";
                End If
            End If

        End If

    End If

    Print

    'Determine some late game scenarios
    scoreDiff = score(P, 0) - score(D, 0)

    'Computer Opponent or "Let Computer Shoot"
    If playerMode <> 0 Or coachOpt = 1 Then

        'NOT (4th Q | OT, 4 mins left, Offense Ahead, Computer is Visitor / On Offense)
        If Not (quarter >= 4 And gameClock! <= 240 And scoreDiff > 0 And playerMode = 1 And P = compTeam) Then

            'PC vs PC, Computer on Defense
            If playerMode = 2 Or playerMode = 1 And D = compTeam Then

                '4th quarter | OT, 4 mins left, offense ahead by 10+
                If quarter >= 4 And gameClock! <= 240 And scoreDiff > 10 Then
                    'Prompt for choices
                    If (playerMode = 1 Or playerMode = 0) And coachOpt <> 1 Then 530
                Else
                    If quarter >= 4 And gameClock! <= 30 And scoreDiff > 0 Then
                        'Less than 30 secs left in Q4 / OT and Off is ahead - INTL FOUL
                        playChoice = 3
                        GoTo 532
                    Else
                        'Human Opponent | PC is Visitor AND Coach Shots
                        If (playerMode = 1 Or playerMode = 0) And coachOpt <> 1 Then
                            'Get user input
                            GoTo 530
                        End If
                    End If
                End If

            End If

        Else

            '985
            pbpString$ = Str$(gameRatings!(P, lineupIdx(P, ballCarrier), 1)) + " FT%  INTENTIONAL FOUL? (Y/N)"

            Do
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "3"

            If UCase$(I$) = "Y" Or I$ = "3" Then
                playChoice = 3
                GoTo 532
            End If

        End If

        'Determine when to shoot in late scenarios
        If gameClock! <= 1 Or (sClockVal < 2 And shotClock <= 6) Or (sClockVal = 1 And passes >= 4) Or (sClockVal = 2 And passes >= 5) Then

            'This is where ~1/2 of FGA's are decided
            'Simply by waiting until the clock is low
            playChoice = 1

        Else

            If quarter >= 4 Then

                If (gameClock! <= 8 And scoreDiff <= -3) Or (gameClock! <= 20 And scoreDiff <= -5) Then

                    playChoice = 1

                Else

                    If shotType > 1 Then
                        playChoice = 1
                    Else
                        playChoice = 0
                    End If

                End If

            End If

        End If

        'Determine when PC will take TO
        If playChoice <> 1 Then

            'Human Opponent, Computer Shoots | Computer is Visitor AND on Defense, Computer Shoots | Computer is Visitor AND On Offense | Computer is Home Team
            If (playerMode = 0 And coachOpt = 1 Or playerMode = 1 And D = compTeam And coachOpt = 1 Or playerMode = 1 And P = compTeam Or playerMode = 2) Then

                If gameClock! <= 660 And gameClock! >= 540 And quarter = 2 And timeouts(P) >= 6 Then
                    playChoice = 2
                    computerTimeout = 1
                Else
                    If gameClock! <= 240 And gameClock! >= 120 And quarter = 2 And timeouts(P) >= 5 Then
                        playChoice = 2
                        computerTimeout = 1
                    Else
                        If gameClock! <= 840 And gameClock! >= 720 And quarter = 4 And timeouts(P) >= 4 Then
                            playChoice = 2
                            computerTimeout = 1
                        Else
                            If gameClock! <= 300 And gameClock! >= 180 And quarter = 4 And timeouts(P) >= 3 Then
                                playChoice = 2
                                computerTimeout = 1
                            End If
                        End If
                    End If

                End If

            End If

        End If

    Else

        530 '
        Do

            I$ = GetKeyPress$

            Select Case UCase$(I$)

                Case "B"
                    Call BOXOPTIONS(0)
                    Call SCOREBOARD

                Case "O"
                    Call OPTIONS

                Case "S"
                    Call SCOUTREPORT

                Case "T"
                    Call TICKER
                    Call SCOREBOARD

                Case "Q"
                    Exit Sub

                Case "Z", "/"
                    Call CHANGESTRATEGY(I$)

            End Select

            playChoice = Val(I$)

        Loop Until UCase$(I$) = "Q" Or UCase$(I$) = "B" Or (playChoice >= 0 And playChoice <= 4)

        If playChoice = 4 Then
            forcedThree = 1
        End If

    End If

    'Loop Until playerMode <> 0 Or coachOpt = 1 Or (UCase$(I$) <> "T" And UCase$(I$) <> "D" And UCase$(I$) <> "O")

    '----------------------------------------------------
    '--- THIS IS CRITCAL FOR PBP MOVING AUTOMATICALLY ---
    '----------------------------------------------------
    ' essentially it allows the user to press a key
    ' during game play to activate certain functions
    ' or special menus, etc.
    532 '
    I$ = InKey$


    Select Case UCase$(I$)

        Case "B"
            Call BOXOPTIONS(0)
            Call SCOREBOARD

        Case "O"
            Call OPTIONS
            GoTo 529

        Case "S"
            Call SCOUTREPORT
            GoTo 529

        Case "T"
            Call TICKER
            Call SCOREBOARD
            GoTo 529

        Case "Q"
            Exit Sub

        Case "Z", "/"
            Call CHANGESTRATEGY(I$)
            GoTo 529

    End Select


    ' *** Determine value for playChoice ***
    'playChoice
    '0 = Pass
    '1 = Shoot (2FGA)
    '2 = Timeout
    '3 = Foul
    '4 = Shoot (3FGA)
    
    'There's alot here missing!!!!

    'Foul was chosen | Computer on offense and last 4 minutes of game
    If Val(I$) = 3 And (playerMode = 0 Or playerMode = 1 And P = compTeam) And quarter >= 4 And gameClock! < 240 Then
        playChoice = 3
    End If

    'Timeout
    If Val(I$) = 2 And (playerMode = 0 Or playerMode = 1 And D = compTeam) And timeouts(P) > 0 Then
        playChoice = 2
    End If

    P3 = playChoice

    If playerMode < 2 Then
        If (playChoice = 2 And timeouts(P) = 0) Or (playChoice = 3 And quarter < 4) Or (playChoice = 3 And quarter = 4 And gameClock! > 240) Or (playerMode = 1 And playChoice = 3 And D = compTeam) Then
            'Take shot with no timeouts | Take timeout with more than 4 mins left | Take timeout before Q4 | Take timeout with PC on Defense
            GoTo 530
        End If
    End If

    ' *** Decide if 3FGA ***
    scoreDiff = score(D, 0) - score(P, 0)

    'Try for 3 near end of game when down by 3
    If quarter >= 4 And gameClock! < 30 And scoreDiff >= 3 And ((playerMode = 1 And P = compTeam) Or playerMode = 2) Then
        If threePtOpt = 1 Then
            playChoice = 4
        End If
    End If

    'Shoot and in 3 pt range
    If (playChoice = 1 Or playChoice = 4) And shotType = 4 Then
        '-- This is where the vast majority of 3-pointers are decided --
        If threePtOpt = 1 Then
            playChoice = 4
        End If
    End If

    ' *** Done determining value for playChoice ***
    playType = playChoice + 1

    Select Case playType

        Case 1
            ' *** PASS ***
            A6 = 1
            passes = passes + 1

            playChoice = 0
            shotChance = 0

            offStatus = 0

            GoTo 400

        Case 2
            537 '
            ' *** SHOOT !!! ***
            pbpType = 3
            Call MAKEPBP(pbpType)
            Call PBP(P)

            shotRoll = 0
            Call Rolld100(foulRoll)

            'Increase FGA
            gameRatings!(P, lineupIdx(P, ballCarrier), 7) = gameRatings!(P, lineupIdx(P, ballCarrier), 7) + 1

            scoreDiff = score(D, 0) - score(P, 0)

            If Not (quarter > 3 And gameClock! <= 30 And scoreDiff = 2 Or quarter > 3 And gameClock! <= 30 And scoreDiff = 0) Then

                'Player attempt to draw FOUL
                '-- Compare roll to Foul Draw Rating + Def Foul Adj
                If foulRoll <= gameRatings!(P, lineupIdx(P, ballCarrier), 16) + teamAdj(D, 3) Then

                    'FOUL!!!
                    gameRatings!(P, lineupIdx(P, ballCarrier), 7) = gameRatings!(P, lineupIdx(P, ballCarrier), 7) - 1

                    Call ResetShotClock
                    Call AdjustClockTime(4)
                    Call AdjustPlayTime

                    Call UPDATESCREEN

                    Call FoulCalled(1)

                    'Free Throws
                    GoTo 591

                End If

            End If

            Call Rolld100(foulRoll)

            'Foul chance is about 10% of the shot chance
            foulChance = (shotChance - 2) * .1

            If foulRoll <= foulChance Then
                'Foul on the shot

                Call ResetShotClock
                Call AdjustClockTime(4)
                Call AdjustPlayTime

                Call UPDATESCREEN

                If willDunk = 1 Then

                    'Call pbplog(2508)

                    If pbpOpt = 1 Then
                        Call WINDEX
                    Else
                        'Women dunk on 0.15% of FGAs
                        '(as opposed to 2.31% for men)
                        If teamGender$(P) = "SHE" And d100% > 5 Then
                            pbpType = 3
                        Else
                            pbpType = 2
                        End If

                        Call MAKEPBP(pbpType)

                    End If

                    pbpString$ = "STUFF BY " + players_GAME$(P, lineupIdx(P, ballCarrier), 0)
                    Call PBP(D)
                    Call DELAY

                    willDunk = 0

                Else

                    If shotType = 8 And pbpOpt = 0 Then
                        pbpType = 7
                        Call MAKEPBP(pbpType)
                        shotType = 0
                        Call PBP(P)
                    Else
                        pbpString$ = "BASKET GOOD BY " + players_GAME$(P, lineupIdx(P, ballCarrier), 0)
                        Call SELECTPBP(P, 1)
                    End If

                End If

                617 '
                pbpString$ = "... AND " + teamGender$(P) + "'S FOULED !!!"
                Call SELECTPBP(P, 0)
                Call DELAY

                Call CreditAssists

                'Update score and FGA
                score(P, quarter) = score(P, quarter) + 2
                score(P, 0) = score(P, 0) + 2

                'Inrease FGM
                gameRatings!(P, lineupIdx(P, ballCarrier), 8) = gameRatings!(P, lineupIdx(P, ballCarrier), 8) + 1

                Call UPDATESCREEN

                Call FoulCalled(1)

                '620
                'Call pbplog(3283)
                Call FreeThrow_Make1(missedLast)

                Call CheckGameClock
                If gameClock! = 0 Then 170

                offStatus = 99

                If missedLast = 1 Then
                    'Missed last FT - REBOUND
                    GoTo 670
                Else

                    Call ChangePossession

                    pbpString$ = gameTeams$(P) + " BALL AFTER SCORE"
                    Call SELECTPBP(P, 1)

                    Call DEADBALLOPTIONS(deadballTO)

                    If (deadballTO = 1) Then
                        GoTo 640
                    Else
                        ballCarrier = 6
                        offStatus = 0
                        Call PRESSGUARD
                        GoTo 300
                    End If

                End If

            Else

                'No foul - process shot

                Call Rolld100(shotRoll)

                If shotRoll <= shotChance Then

                    'Shot Good
                    If willDunk = 1 Then

                        'Call pbplog(2604)

                        If pbpOpt = 1 Then
                            Call WINDEX
                        Else
                            'Women dunk on 0.15% of FGAs
                            '(as opposed to 2.31% for men)
                            '-- "1%" factor = 20
                            If teamGender$(P) = "SHE" And d100% > 5 Then
                                pbpType = 3
                            Else
                                pbpType = 2
                            End If

                            pbpType = 2

                            Call MAKEPBP(pbpType)

                        End If

                        Call PBP(P)
                        Call DELAY

                        willDunk = 0

                    Else

                        If pbpOpt = 0 And shotType = 8 Then

                            pbpType = 7
                            Call MAKEPBP(pbpType)
                            Call PBP(P)
                            shotType = 0

                        Else

                            pbpString$ = "BASKET BY " + players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " !!!"

                            If pbpOpt = 1 Then

                                If pbpDelay! > 0 Then
                                    Call WINDEX
                                    Print pbpString$
                                    Call DELAY
                                End If

                            Else

                                If d100% > 50 Then

                                    RN1 = Int(Rnd(1) * 22) + 1

                                    Select Case RN1
                                        Case 1:
                                            pbpString$ = "NOTHING BUT THE BOTTOM OF THE NET!"
                                        Case 2:
                                            pbpString$ = "COUNT IT!"
                                        Case 3:
                                            pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " TICKLES THE TWINE!"
                                        Case 4:
                                            pbpString$ = "NO RIM, NO BOARD, JUST CORD!"
                                        Case 5:
                                            pbpString$ = "GOOD!"
                                        Case 6:
                                            pbpString$ = "YES!"
                                        Case 7:
                                            pbpString$ = "COUNT IT!"
                                        Case 8:
                                            pbpString$ = "IT'S GOOD!"
                                        Case 9:
                                            pbpString$ = "BINGO!"
                                        Case 10:
                                            pbpString$ = "IT FALLS!"
                                        Case 11:
                                            pbpString$ = teamGender$(P) + " DRAINS IT!"
                                        Case 12:
                                            pbpString$ = "SWISH!"
                                        Case 13:
                                            pbpString$ = "ALL NET!"
                                        Case 14:
                                            pbpString$ = "IT'S THERE!"
                                        Case 15:
                                            pbpString$ = teamGender$(P) + " BURIES IT!"
                                        Case 16:
                                            pbpString$ = "IT GOES IN!"
                                        Case 17:
                                            pbpString$ = "RING IT UP!"
                                        Case 18:
                                            pbpString$ = "IT GOES DOWN!"
                                        Case 19:
                                            pbpString$ = "BUlastLoseStreakEYE!"
                                        Case 20:
                                            pbpString$ = teamGender$(P) + " SCORES!"
                                        Case 21:
                                            pbpString$ = teamGender$(P) + " HAS IT!"
                                        Case 22:
                                            pbpString$ = teamGender$(P) + " CONNECTS!"
                                    End Select

                                End If

                                Call PBP(P)

                            End If

                        End If

                    End If

                    560 '
                    score(P, quarter) = score(P, quarter) + 2
                    score(P, 0) = score(P, 0) + 2

                    Call CreditAssists

                    Call UPDATESCREEN

                    'Increase FGM
                    gameRatings!(P, lineupIdx(P, ballCarrier), 8) = gameRatings!(P, lineupIdx(P, ballCarrier), 8) + 1

                    562 '
                    Call ResetShotClock
                    Call AdjustClockTime(4)
                    Call AdjustPlayTime
                    Call CheckGameClock

                    If gameClock! = 0 Then 170

                    Call UPDATESCREEN

                    Call ChangePossession

                    pbpString$ = gameTeams$(P) + " BALL AFTER SCORE"
                    Call SELECTPBP(P, 1)
                    Call DELAY

                    Call DEADBALLOPTIONS(deadballTO)

                    If (deadballTO = 1) Then
                        GoTo 640
                    Else
                        Call PRESSGUARD
                        GoTo 300
                    End If

                Else

                    'Shot Missed
                    Call Rolld100(blockRoll)
                    rndPlayer = Int(Rnd(1) * 5)
                    blockRating = gameRatings!(D, lineupIdx(D, rndPlayer), 6)

                    'Check player's blocks / game
                    If blockRoll > blockRating Then

                        If pbpOpt = 1 Then
                            If pbpDelay! > 0 Then
                                pbpString$ = "SHOT MISSED BY " + players_GAME$(P, lineupIdx(P, ballCarrier), 0)
                            End If
                        Else
                            pbpType = 8
                            Call MAKEPBP(pbpType)
                        End If

                        Call SELECTPBP(P, 1)

                        If autoPlay <> 1 Or playerMode <> 2 And backboard& And d100% < 10 Then
                            _SndPlay backboard&
                        End If

                        Call ResetShotClock

                    Else

                        'Players with 0 rating, need a slight chance
                        'Players with 1 rating will have too frequent a chance
                        If (blockRating = 0 And d100% <= 10) Or (blockRating = 1 And d100% <= 70) Or blockRating > 1 Then

                            If (autoPlay = 0 Or pbpDelay! > 0) Then

                                If pbpOpt = 1 Then
                                    Call WINDEX
                                    Print "SHOT BLOCKED BY "; players_GAME$(D, lineupIdx(D, rndPlayer), 0)
                                    Call DELAY
                                Else

                                    Call Rolld100(RN1)

                                    Select Case RN1
                                        Case 1 To 10:
                                            pbpString$ = players_GAME$(D, lineupIdx(D, rndPlayer), 0) + " STUFFS THE SHOT!!"
                                        Case 11 To 15:
                                            pbpString$ = "DENIED BY " + players_GAME$(D, lineupIdx(D, rndPlayer), 0) + "!!"
                                        Case 16 To 20:
                                            pbpString$ = "ERASED BY " + players_GAME$(D, lineupIdx(D, rndPlayer), 0) + "!!"
                                        Case 21 To 25:
                                            pbpString$ = "EXPUNGED BY " + players_GAME$(D, lineupIdx(D, rndPlayer), 0) + "!!"
                                        Case 26 To 30:
                                            pbpString$ = "REBUKED BY " + players_GAME$(D, lineupIdx(D, rndPlayer), 0) + "!!"
                                        Case 31 To 35:
                                            pbpString$ = "SHOT BLOCKED CLEANLY BY " + players_GAME$(D, lineupIdx(D, rndPlayer), 0) + "!!"
                                        Case 36 To 70:
                                            pbpString$ = "SHOT BLOCKED BY " + players_GAME$(D, lineupIdx(D, rndPlayer), 0) + "!!"
                                        Case 71 To 90:
                                            pbpString$ = "SHOT REJECTED BY " + players_GAME$(D, lineupIdx(D, rndPlayer), 0) + "!!"
                                        Case 91 To 100:
                                            pbpString$ = "SHOT SWATTED OUT BY " + players_GAME$(D, lineupIdx(D, rndPlayer), 0) + "!!"
                                    End Select

                                    Call PBP(D)

                                End If

                            End If

                            offStatus = 98

                            'Increase blocks
                            gameRatings!(D, lineupIdx(D, rndPlayer), 15) = gameRatings!(D, lineupIdx(D, rndPlayer), 15) + 1

                        End If

                    End If

                    Call AdjustClockTime(4)
                    Call AdjustPlayTime
                    Call CheckGameClock

                    If gameClock! = 0 Then
                        '-- the below lines are in CBB
                        'timeElapsed! = gameClock!
                        'Call AdjustPlayTime
                        was3PtShot = 0
                        GoTo 170
                    End If

                    Call UPDATESCREEN

                    GoTo 670

                End If

            End If

            '--- done with "routines" for taking a shot

        Case 3
            ' *** TIMEOUT ***
            640 '
            Call TimeoutCalled(P)

            If A6 = 0 Then
                Call PRESSGUARD
                GoTo 300
            Else
                A6 = 1
                passes = passes + 1

                playChoice = 0
                shotChance = 0

                offStatus = 0

                GoTo 400
            End If


        Case 4
            ' *** FOUL ***
            584 '
            IN = 99

            Call LastShot
            Call CheckGameClock
            If gameClock! = 0 Then 170

            C1 = 2
            P9 = D

            pbpString$ = "DESIGNATED FOULER (0-4)"
            Call SELECTPBP(P, 1)

            If playerMode = 1 And D = compTeam Or playerMode = 2 Then
                desigFouler = Int(Rnd(1) * 5)
            Else
                Do
                    I$ = GetKeyPress$
                    desigFouler = Val(I$)
                Loop Until desigFouler >= 0 And desigFouler <= 4
            End If

            foulPlayer = desigFouler
            Call FoulCalled(0)

            Select Case sClockVal
                Case 0: shotClock = 24
                Case 1: shotClock = 30
            End Select

            GoTo 566

        Case 5
            ' *** 3FG ATTEMPT ***
            359 '
            Call Rolld100(shotRoll)

            'Increase 3FGA
            threeFG(P, lineupIdx(P, ballCarrier), 1) = threeFG(P, lineupIdx(P, ballCarrier), 1) + 1

            'Increase FGA
            gameRatings!(P, lineupIdx(P, ballCarrier), 7) = gameRatings!(P, lineupIdx(P, ballCarrier), 7) + 1

            pbpType = 3
            Call MAKEPBP(pbpType)
            Call PBP(P)

            Call Rolld100(foulChance)

            'Essentialy a 4% chance of being fouled on 3-pointer
            If foulChance <= 4 Then

                'FOUL!

                'Remove FGA attempt from record
                gameRatings!(P, lineupIdx(P, ballCarrier), 8) = gameRatings!(P, lineupIdx(P, ballCarrier), 8) - 1

                'Remove 3FGA attempt from record
                threeFG(P, lineupIdx(P, ballCarrier), 0) = threeFG(P, lineupIdx(P, ballCarrier), 0) - 1

                Call ResetShotClock
                Call AdjustClockTime(4)
                Call AdjustPlayTime
                Call CheckGameClock

                If gameClock! = 0 Then 170

                Call UPDATESCREEN

                If shotRoll > threePtShotChance Then

                    'Would have been a miss
                    ' *** Take 3 free throws ***
                    'Call pbplog(3696)
                    Call FreeThrow_Three(missedLast)

                Else

                    'Would have been made; shot still counts
                    score(P, quarter) = score(P, quarter) + 3
                    score(P, 0) = score(P, 0) + 3

                    'Call pbplog(3705)
                    Call FreeThrow_One(missedLast)

                End If

                offStatus = 99

                If missedLast = 1 Then
                    'Missed last FT - REBOUND
                    GoTo 670
                Else

                    Call ChangePossession

                    pbpString$ = gameTeams$(P) + " BALL AFTER SCORE"
                    Call SELECTPBP(P, 1)

                    Call DEADBALLOPTIONS(deadballTO)

                    If (deadballTO = 1) Then
                        GoTo 640
                    Else
                        ballCarrier = 6
                        offStatus = 0
                        Call PRESSGUARD
                        GoTo 300
                    End If

                End If

            Else

                If shotRoll <= threePtShotChance Then

                    'MADE 3-POINT FIELD GOAL
                    If pbpOpt = 1 Then

                        If pbpDelay! > 0 Then
                            Call WINDEX
                            Print "3 PT FG GOOD BY "; players_GAME$(P, lineupIdx(P, ballCarrier), 0); " !!!"
                            Call DELAY
                        End If

                    Else

                        pbpType = 4
                        Call MAKEPBP(pbpType)
                        Call PBP(P)

                    End If

                    Call DELAY

                    If autoPlay <> 1 Or playerMode <> 2 And swish& Then
                        _SndPlay swish&
                    End If

                    If XX% = 1 Then
                        'Foot was on the line - it's only a 2FGA
                        XX% = 0
                        score(P, quarter) = score(P, quarter) + 2
                        score(P, 0) = score(P, 0) + 2
                    Else
                        'Reward 3 pts
                        score(P, quarter) = score(P, quarter) + 3
                        score(P, 0) = score(P, 0) + 3

                        'Increase 3FGM
                        threeFG(P, lineupIdx(P, ballCarrier), 0) = threeFG(P, lineupIdx(P, ballCarrier), 0) + 1
                    End If

                    'Increase FGM
                    gameRatings!(P, lineupIdx(P, ballCarrier), 8) = gameRatings!(P, lineupIdx(P, ballCarrier), 8) + 1

                    Call CreditAssists
                    Call UPDATESCREEN

                    GoTo 562

                Else

                    If pbpOpt = 1 Then
                        If pbpDelay! > 0 Then
                            Call WINDEX
                            Print "3 PT FG MISSED BY "; players_GAME$(P, lineupIdx(P, ballCarrier), 0)
                            Call DELAY
                        End If
                    Else
                        was3PtShot = 1
                        pbpType = 5
                        Call MAKEPBP(pbpType)
                    End If

                    Call SELECTPBP(P, 0)

                    If autoPlay <> 1 Or playerMode <> 2 And backboard& And d100% < 10 Then
                        _SndPlay backboard&
                    End If

                    Call ResetShotClock
                    Call AdjustClockTime(4)
                    Call AdjustPlayTime
                    Call CheckGameClock

                    If gameClock! = 0 Then 170

                    Call UPDATESCREEN

                    GoTo 670

                End If


            End If

    End Select


    '----- NOODLE -----

    566 '

    ' *** PERSONAL FOULS ***
    'Call pbplog(3822)

    'Check if in BONUS
    If teamFouls(D) >= bonusFoulNum And quarter <= 4 Or teamFouls(D) > 3 And quarter > 4 Then

        If freeThrowVal = 1 Then
            pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " GETS 3 FT TO MAKE 2..."
        Else
            pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " GETS 2 FREE THROWS..."
        End If

        Call PBP(P)

        'Take free throws
        GoTo 591

    Else

        'Not in bonus
        Call CheckGameClock
        If gameClock! = 0 Then 170

        'FT only in Bonus or Shooting
        If ftRulesOpt = 0 Then

            'Time to inbound the ball
            Call Substitutions

            pbpString$ = gameTeams$(P) + " BALL AFTER FOUL"
            Call PBP(P)

            Call DELAY

            A6 = 1
            passes = passes + 1

            playChoice = 0
            shotChance = 0

            offStatus = 0

            GoTo 400

        Else

            'FT on all fouls
            Call UPDATESCREEN

            'Not in bonus = 1 free throw
            'Call pbplog(3876)
            Call FreeThrow_One(missedLast)

            LT = 0

            Call CheckGameClock
            If gameClock! = 0 Then 170

            If missedLast = 1 Then
                'Rebound
                GoTo 650

            Else

                Call ChangePossession

                pbpString$ = gameTeams$(P) + " BALL AFTER SCORE"
                Call SELECTPBP(P, 1)
                DELAY

                Call DEADBALLOPTIONS(deadballTO)

                If (deadballTO = 1) Then
                    GoTo 640
                Else
                    ballCarrier = 6
                    offStatus = 0
                    Call PRESSGUARD
                    GoTo 300
                End If

            End If

        End If

    End If

    '----- NOODLE -----

    ' *** TEAM FOULS ***

    580 '
    Call LastShot

    Call ResetShotClock

    'FoulCalled always targets the DEF
    'Switch teams before we handle
    Call ChangePossession

    Call FoulCalled(0)

    ballCarrier = foulPlayer

    'Check if in BONUS
    If teamFouls(D) >= bonusFoulNum And quarter <= 4 Or teamFouls(D) > 3 And quarter > 4 Then

        If freeThrowVal = 1 Then
            pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " GETS 3 FT TO MAKE 2..."
        Else
            pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " GETS 2 FREE THROWS..."
        End If

        Call PBP(P)

        'Take free throws
        GoTo 591

    Else

        'Not in bonus
        'Offensive foul
        'Call pbplog(3939)

        Call CheckGameClock
        If gameClock! = 0 Then 170

        pbpString$ = "OFFENSIVE FOUL"
        Call SELECTPBP(P, 1)

        pbpString$ = gameTeams$(P) + " BALL AFTER FOUL"
        Call SELECTPBP(P, 1)

        Locate 8, 41: Print Space$(12)

        ballCarrier = 6
        Call DELAY

        turnovers(D) = turnovers(D) + 1

        Call UPDATESCREEN

        Call Substitutions

        Call PRESSGUARD

        GoTo 300

    End If


    '----- NOODLE -----

    591 '
    'Shooting fouls?

    ' *** FREE THROWS IN BONUS / 2 FREE THROWS ***
    'Call pbplog(3979)
    Call FreeThrow_Make2(missedLast)

    Call CheckGameClock

    If gameClock! = 0 Then 170

    If missedLast = 1 Then

        GoTo 650

    Else

        Call ChangePossession

        pbpString$ = gameTeams$(P) + " BALL AFTER SCORE"
        Call SELECTPBP(P, 1)
        Call DELAY

        Call DEADBALLOPTIONS(deadballTO)

        If (deadballTO = 1) Then
            GoTo 640
        Else
            ballCarrier = 6
            offStatus = 0
            Call PRESSGUARD
            GoTo 300
        End If

    End If


    '----- NOODLE -----

    ' ***** CHECK FOR REBOUNDS *****

    650 '
    A6 = 1
    playChoice = 0
    assistShotBoost = 0
    D6 = 0

    Call ResetShotClock

    scoreDiff = score(D, 0) - score(P, 0)

    If (quarter > 3 And gameClock! < 30 And d100% <= 12) And (scoreDiff = 2 Or scoreDiff = 0) Then

        Call ReboundOffense
        GoTo 760

    Else

        Call Rolld100(RN1)

        Select Case teamYears(eraChoice)

            Case Is < 1993:
                rebMax = 47

            Case 1993 To 2017:
                rebMax = 42

            Case Is >= 2018:
                rebMax = 42

        End Select

        Select Case RN1

            Case 1 To rebFoulChance:
                foulOnReb = 1
                GoTo 770

            Case rebFoulChance + 1 To rebMax:
                Call ReboundOffense
                GoTo 760

            Case rebMax + 1 To 100:
                Call ReboundDefense
                GoTo 800

        End Select

    End If


    '----- NOODLE -----

    670 '
    A6 = 1
    playChoice = 0
    assistShotBoost = 0
    D6 = 0

    Call ResetShotClock

    '-- Start Loop
    700 '
    shotRoll = 0
    assistShotBoost = 0
    D6 = 0

    Call Rolld100(RN0)

    scoreDiff = score(D, 0) - score(P, 0)
    If quarter > 3 And gameClock! < 30 And (scoreDiff = 2 Or scoreDiff = 0 Or scoreDiff = -2) And RN0 <= 12 Then
        GoTo 700
    End If

    If was3PtShot = 1 And RN0 <= 19 Then
        'why would we do this?
        'either it was a 3 pointer or not?
        'was3PtShot = 0
    End If

    Select Case RN0

        Case 1

            If offStatus = 98 Then

                GoTo 700

            Else
                turnovers(P) = turnovers(P) + 1

                pbpString$ = "OFFENSIVE GOAL TENDING"
                Call SELECTPBP(P, 1)

                pbpString$ = "BALL GOES TO " + gameTeams$(D)
                Call SELECTPBP(P, 1)
                Call DELAY

                Call ChangePossession

                Call Substitutions

                GoTo 300

            End If

        Case 2 To rebFoulChance
            foulOnReb = 1
            GoTo 770

        Case rebFoulChance + 1 To rebFoulChance + 12
            'Ball knocked out-of-bounds
            If d100% > 35 Then

                totRebounds(D) = totRebounds(D) + 1

                Call UPDATESCREEN

                pbpString$ = "RB KNOCKED O.B. TO " + gameTeams$(D)
                Call SELECTPBP(D, 1)
                Call DELAY

                Call Substitutions

                Call ChangePossession

                GoTo 300

            Else

                totRebounds(P) = totRebounds(P) + 1

                Call UPDATESCREEN

                pbpString$ = "RB KNOCKED O.B. TO " + gameTeams$(P)
                Call SELECTPBP(P, 1)
                Call DELAY

                Call Substitutions

                GoTo 520

            End If

        Case rebFoulChance + 13 To rebFoulChance + 15

            If offStatus = 99 Then

                GoTo 700

            Else

                Call ReboundOffense

                'Increase FGA
                gameRatings!(P, lineupIdx(P, ballCarrier), 7) = gameRatings!(P, lineupIdx(P, ballCarrier), 7) + 1

                'Increase FGM
                gameRatings!(P, lineupIdx(P, ballCarrier), 8) = gameRatings!(P, lineupIdx(P, ballCarrier), 8) + 1

                'Increase rebounds
                gameRatings!(P, lineupIdx(P, ballCarrier), 11) = gameRatings!(P, lineupIdx(P, ballCarrier), 11) + 1
                totRebounds(P) = totRebounds(P) + 1

                score(P, quarter) = score(P, quarter) + 2
                score(P, 0) = score(P, 0) + 2

                Call UPDATESCREEN

                If pbpOpt = 1 Then
                    If pbpDelay! > 0 Then
                        Call WINDEX
                        Print "TIPPED IN BY "; players_GAME$(P, lineupIdx(P, ballCarrier), 0)
                        Print "   BASKET GOOD !!!"
                        Call DELAY
                    End If
                Else
                    pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " TIPS IT IN!!!"
                    Call PBP(P)
                End If

                Call ChangePossession

                pbpString$ = gameTeams$(P) + " BALL AFTER SCORE"
                Call SELECTPBP(P, 1)
                Call DELAY

                Call DEADBALLOPTIONS(deadballTO)

                If (deadballTO = 1) Then
                    GoTo 640
                Else
                    Call PRESSGUARD
                    GoTo 300
                End If

            End If

        Case rebFoulChance + 16:

            If playChoice <> 4 Then

                If offStatus = 99 Then

                    GoTo 700

                Else
                    pbpString$ = "DEFENSIVE GOAL TENDING"
                    Call SELECTPBP(P, 1)

                    GoTo 560

                End If

            End If

        Case rebFoulChance + 17 To 100:
            770 '
            'Calculate rebound contributions
            '-- RB is a total of "rebound ratings" for both teams
            '-- rebRatings is an array to track "rebound ratings" for each player
            '   0 - 4 is for team 1; 5 - 9 is for team 2
            was3PtShot = 0
            RB = 0

            Erase rebRatings

            'Team 1
            For I = 0 To 4

                If quarter > 4 Or quarter = 4 And gameClock! <= 120 Then
                    rebRatings(I) = rebRatings(I) + gameRatings!(P, lineupIdx(P, I), 21)
                    RB = RB + gameRatings!(P, lineupIdx(P, I), 21)
                Else
                    If playerFatigue%(P, lineupIdx(P, I)) <= 0 Then
                        rebRatings(I) = rebRatings(I) + gameRatings!(P, lineupIdx(P, I), 21) * .65
                        RB = RB + gameRatings!(P, lineupIdx(P, I), 21) * .65
                    Else
                        rebRatings(I) = rebRatings(I) + gameRatings!(P, lineupIdx(P, I), 21)
                        RB = RB + gameRatings!(P, lineupIdx(P, I), 21)
                    End If
                End If

            Next

            'Team 2
            For I = 0 To 4

                If quarter > 4 Or quarter = 4 And gameClock! <= 120 Then
                    rebRatings(I + 5) = rebRatings(I + 5) + gameRatings!(1 - P, lineupIdx(1 - P, I), 2)
                    RB = RB + gameRatings!(1 - P, lineupIdx(1 - P, I), 2)
                Else
                    If playerFatigue%(1 - P, lineupIdx(1 - P, I)) <= 0 Then
                        rebRatings(I + 5) = rebRatings(I + 5) + gameRatings!(1 - P, lineupIdx(1 - P, I), 2) * .5
                        RB = RB + gameRatings!(1 - P, lineupIdx(1 - P, I), 2) * .5
                    Else
                        rebRatings(I + 5) = rebRatings(I + 5) + gameRatings!(1 - P, lineupIdx(1 - P, I), 2)
                        RB = RB + gameRatings!(1 - P, lineupIdx(1 - P, I), 2)
                    End If
                End If

            Next

            rbChance = Rnd(1) * RB

            skipRandom% = 0

            For ballCarrier = 0 To 9

                If rbChance <= rebRatings(ballCarrier) Then
                    skipRandom% = 1
                    Exit For
                End If

                rebRatings(ballCarrier + 1) = rebRatings(ballCarrier + 1) + rebRatings(ballCarrier)

            Next

            If skipRandom% = 0 Then
                ballCarrier = Int(Rnd(1) * 10)
            End If

            If ballCarrier <= 4 Then
                'Offensive lineup
                foulOnReb = 0
                'GoTo 760
            Else
                'Defensive lineup
                ballCarrier = ballCarrier - 5
                GoTo 800
            End If

    End Select


    '----- NOODLE -----

    ' *** OFFENSIVE REBOUND ***

    760 '
    'Earlier we reset the shot clock to 24
    'Starting in 2018, this was changed to
    'be only 14 secondss
    If teamYears(eraChoice) >= 2018 Then
        shotClock! = 14
    End If

    'Increase rebounds
    gameRatings!(P, lineupIdx(P, ballCarrier), 11) = gameRatings!(P, lineupIdx(P, ballCarrier), 11) + 1
    gameRatings!(P, lineupIdx(P, ballCarrier), 24) = gameRatings!(P, lineupIdx(P, ballCarrier), 24) + 1
    totRebounds(P) = totRebounds(P) + 1

    Call UPDATESCREEN

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " HAS THE OFFENSIVE REBOUND"
        Call SELECTPBP(P, 1)

        If pbpOpt = 1 Then
            Print "FOR " + gameTeams$(P)
            Call DELAY
        End If

    End If

    If foulOnReb = 1 Then

        pbpString$ = teamGender$(P) + "'S FOULED !!!"
        Call SELECTPBP(P, 1)

        Call UPDATESCREEN

        Call FoulCalled(0)

        If freeThrowVal = 1 And (teamFouls(D) >= bonusFoulNum And quarter <= 4 Or teamFouls(D) > 3 And quarter > 4) Then
            pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " GETS 3 FT TO MAKE 2..."
        Else
            pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " GETS 2 FREE THROWS..."
        End If

        Call PBP(P)

        foulOnReb = 0

        'Take free throws
        GoTo 591

    Else

        Call CheckGameClock

        If gameClock! = 0 Then

            GoTo 170

        Else

            'RN0 = Int(Rnd(1) * 130) + 1
            prevBall$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0)

            '-- If RN0 = 32 Then

            '    GoTo RBTIEUP

            '-- Else

            rebShotRoll = Int(Rnd(1) * 10) + 1

            Call SHOTPERCENT(0)

            If rebShotRoll <= 8 Then
                shotType = 9
                '-- playChoice = 1
                GoTo 537
            Else
                GoTo 520
            End If

            '-- End If

        End If

    End If


    '----- NOODLE -----

    ' *** DEFENSIVE REBOUND ***

    800 '
    gameRatings!(D, lineupIdx(D, ballCarrier), 11) = gameRatings!(D, lineupIdx(D, ballCarrier), 11) + 1
    totRebounds(D) = totRebounds(D) + 1

    Call UPDATESCREEN

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        If pbpOpt = 1 Then
            Call WINDEX
            pbpString$ = players_GAME$(D, lineupIdx(D, ballCarrier), 0) + " HAS THE DEF REBOUND FOR "
            Print pbpString$
        Else
            pbpString$ = players_GAME$(D, lineupIdx(D, ballCarrier), 0) + " HAS THE DEF REBOUND"
            Call PBP(D)
        End If

    End If

    If foulOnReb = 1 Then

        If (autoPlay = 0 Or pbpDelay! > 0) Then

            If pbpOpt = 1 Then
                pbpString$ = gameTeams$(D) + " AND IS FOULED !!!"
                Print pbpString$
                Call DELAY
            Else
                pbpString$ = teamGender$(P) + "'S FOULED !!!"
                Call PBP(D)
            End If

        End If

        Call UPDATESCREEN

        'FoulCalled always targets the DEF
        'Switch teams before we handle
        Call ChangePossession
        Call FoulCalled(1)

        foulOnReb = 0

        GoTo 566

    Else

        'New defender
        prevBall$ = players_GAME$(D, lineupIdx(D, ballCarrier), 0)

        Call ChangePossession
        P9 = D

        fastBreak = 1

        'No chance to choose new strategy?

        GoTo 300

    End If

End Sub


'Include: 'GameRoutines.bm'

'----------------------------------------
'            INFO Subroutine
'----------------------------------------
'This subroutine displays a simple header
'and footer for the game options menu.
Sub INFO ()

    Cls

    Color , 1
    Locate 1, 1
    Print Tab(80); " ";

    Locate , 33
    Color 14
    Print "GAME OPTIONS MENU"

    Locate 24, 1
    Print Tab(80); " ";
    Locate , 1
    Print "    LANCE HAFFNER / JASON LEONARD  1986-2024"; Tab(70); Left$(Time$, 5);
    Locate 3, 1
    Color , 0

End Sub


'----------------------------------------
'           InitVar Subroutine
'----------------------------------------
' Initializes variables for the game
' Does not process any that will be read
' in from file or assigned elsewhere
Sub InitVar ()

    Shared actualAttendance&

    Shared scheduleFile$

    Shared avgAttendance&()

    Shared eventSettings()
    Shared teamScore(), oppScore()

    Shared game3FGM!(), game3FGA!()
    Shared statsOffTot!(), statsDefTot!(), gameStatPlyrVal!()
    Shared indRecords!(), teamRecords!()

    Shared alpha$()
    Shared gameSite$(), oppName$(), locIndicator$()
    Shared indRecDesc$(), teamRecDesc$()
    Shared teamIndicator$()

    possTime! = 0

    For I = 0 To 1
        For I1 = 0 To 4
            lineupIdx(I, I1) = -1
        Next I1
    Next I

    teamIndicator$(0) = "VISITING TEAM"
    teamIndicator$(1) = "HOME TEAM"

    offStyleDesc$(0) = "NORMAL      "
    offStyleDesc$(1) = "AGGRESSIVE  "
    offStyleDesc$(2) = "SAFE        "

    alpha$(0) = "A "
    alpha$(1) = "B "
    alpha$(2) = "C "
    alpha$(3) = "D "
    alpha$(4) = "E "

    pbpStyle$(0) = "VERBOSE        "
    pbpStyle$(1) = "HAFFNER CLASSIC"

    posnName$(0) = "G"
    posnName$(1) = "G"
    posnName$(2) = "F"
    posnName$(3) = "F"
    posnName$(4) = "C"

    scSettings(0) = 24
    scSettings(1) = 30
    scSettings(2) = 0

    penetrationOffAdj(1) = 2
    penetrationOffAdj(2) = -2

    '1 = Aggressive offense style
    '2 = Safe offense style
    stealAdj(1) = 10
    stealAdj(2) = -10

    defStyles(0) = 1
    defStyles(1) = 1

    mandatoryTO(0) = 0
    mandatoryTO(1) = 0

    assigned2TOs = 0
    autoPlay = 0
    ballCarrier = 0
    bonusFoulNum = 5
    compTeam = 0
    D = 0
    endAllGames = 0
    gameOver = 0
    fastBreak = 0
    forcedThree = 0
    freeThrowNbr = 0
    fullCtOpt = 0
    halfTime = 0
    JY = 0
    maxTimeouts = 7
    nbrFTMade = 0
    nbrLines = 1
    offStatus = 0
    P = 0
    P9 = 0
    passes = 0
    pbpType = 0
    quarter = 1
    quarterOpt = 0
    shotClock = 0
    shotType = 0
    statSpecial = 0
    threePtShotChance = 0
    timeoutOpt = 3
    tickerGames = 0
    willDunk = 0

    penetrationOffAdj = 0

    timeElapsed! = 0

    A1$ = ""
    B1$ = ""
    C1$ = ""
    D1$ = ""
    E1$ = ""
    F1$ = ""
    G1$ = ""
    H1$ = ""
    activePosition$ = ""
    pbpString$ = ""
    prevBall$ = ""
    scheduleFile$ = ""
    TS$ = ""

    'These are for reading in team data
    'Erase gameCoach$, gameMascots$, gameArena$, players_GAME$
    Erase leagRat_GAME, plyrRat_GAME, teamRat_GAME

    'Erase C1

    Erase dFGPA, eventSettings, fullCtOpt
    Erase gameStat3FGM, gameStat3FGA
    Erase miscAdj, offStyles, oppScore
    Erase paceAdj, P4, P5
    Erase pbpFG, pbpBG, QQ, QR, rebRatings
    Erase score, schedGame, schedOptions, statTotals
    Erase teamAdj, teamIndex
    Erase teamFouls, teamScore, threeFG, timePlayed
    Erase teamRecWins, teamRecLoss
    Erase totRebounds, totalFouls, turnovers

    Erase gameRatings!, game3FGM!, game3FGA!
    Erase rebAdj!
    Erase statsOffTot!, statsDefTot!, gameStatPlyrVal!
    Erase indRecords!, teamRecords!

    Erase diskIDs$, gameSite$, gameTeams$
    Erase indRecDesc$, locIndicator$, oppName$
    Erase Q$, SX$
    Erase teamAbbrev$, teamNames$, teamRecDesc$

    Erase F%, G9%
    Erase ST%, SX%

    actualAttendance& = 0
    Erase avgAttendance&

End Sub


'----------------------------------------
'          AUTOSETUP Subroutine
'----------------------------------------
'This subroutine auto-assigns game param-
'eters from a schedule file's data.
Sub AUTOSETUP (validTeam)

    'AUTO-PLAY OPTIONS
    gameLoc = schedOptions(2)
    threePtOpt = schedOptions(3)
    pbpDelay! = schedOptions(4) / 10
    playoffOpt = schedOptions(5)
    sClockVal = schedOptions(7)
    playerOpt = schedOptions(8)
    coachOpt = schedOptions(9)
    freeThrowVal = schedOptions(10)
    ftRulesOpt = schedOptions(11)
    tickerStart = schedOptions(12)
    bonusFoulNum = schedOptions(14)
    timeoutOpt = schedOptions(19)
    quarterOpt = schedOptions(20)

    For tgtTeam = 0 To 1

        tgtId = schedOptions(tgtTeam)
        tgtYear$ = RTrim$(diskIDs$(tgtTeam))

        Call ReadGameTeam(tgtYear$, gameTeams$(tgtTeam), tgtId, tgtTeam, validTeam)
        teamGender$(tgtTeam) = GetTeamGender$(tgtYear$)

        schedOptions(tgtTeam) = tgtId

        If tgtId = 0 Then
            'In case chosen team ends up with 0 value
            validTeam = 0
        End If

    Next tgtTeam

End Sub


'----------------------------------------
'       LetterToNumber Subroutine
'----------------------------------------
'This subroutine converts an alphabetic
'character to a numeric equivalent,
'where A = 10, B = 11, etc.
'This is useful for long menus that have
'more than 10 options (0 - 9) and a single
'key is desired for a choice.
Sub LetterToNumber (promptVal$, J)

    J = Val(promptVal$)

    Select Case UCase$(promptVal$)

        Case "A": J = 10
        Case "B": J = 11
        Case "C": J = 12
        Case "D": J = 13
        Case "E": J = 14
        Case "F": J = 15
        Case "G": J = 16
        Case "H": J = 17
        Case "I": J = 18

    End Select

End Sub

'----------------------------------------
'           TEAMBOX Subroutine
'----------------------------------------
'This subroutine draws an ASCII box using
'team colors. The box starts at row 6 and
'is approx. 60 char's wide. When done, the
'cursor is set at the first printable row
'and column inside the box.
Sub TEAMBOX (boxHeight, teamIdx)

    Locate 6, 8
    Color teamRat_GAME(teamIdx, 33), teamRat_GAME(teamIdx, 34)

    'Print box top
    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

    'Print box sides
    For II = 1 To boxHeight
        Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    'Print box bottom
    Locate 7 + boxHeight, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";

    Locate 7, 11

End Sub


'----------------------------------------
'        TimeoutCalled Subroutine
'----------------------------------------
'This subroutine handles the PBP and
'variable values for when a team takes
'a timeout. This includes resetting the
'shot clock and adjusting play time.
Sub TimeoutCalled (chargeTeam)

    If timeouts(chargeTeam) > 0 Then

        If mandatoryTO(chargeTeam) = 0 Then
            mandatoryTO(chargeTeam) = 1
        End If

        timeouts(chargeTeam) = timeouts(chargeTeam) - 1

        'Restore time on clock
        Call AdjustClockTime(-M5)
        Call AdjustPlayTime

        If sClockVal < 2 Then
            shotClock = shotClock + M5
        End If

        pbpString$ = "TIMEOUT " + gameTeams$(chargeTeam):
        Call SELECTPBP(chargeTeam, 1)
        Call DELAY

        Call Substitutions

    End If

End Sub


'----------------------------------------
'         FRAMEOPTION Subroutine
'----------------------------------------
'This subroutine draws an ASCII box using
'team colors. The box starts at row 8 and
'is approx. 60 char's wide. When done, the
'cursor is set at the first printable row
'and column inside the box.
Sub FRAMEOPTION (boxHeight, teamIdx)

    PCopy 0, 1
    Screen , , 1, 1

    Locate 8, 8
    Color teamRat_GAME(teamIdx, 33), teamRat_GAME(teamIdx, 34)

    'Print box top
    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

    'Print box sides
    For II = 1 To boxHeight
        Locate 8 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    'Print box bottom
    Locate 9 + boxHeight, 8
    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

    Locate 9, 12

End Sub


'----------------------------------------
'         Rolld100 Subroutine
'----------------------------------------
'This subroutine assigns a random number
'between 1 and 100 to the variable that
'it is called with
Sub Rolld100 (randNumVar)

    randNumVar = Int(Rnd(1) * 100) + 1

End Sub


'----------------------------------------
'            d100 Subroutine
'----------------------------------------
'This subroutine assigns a random number
'between 1 and 100 to the variable that
'it is called with
Function d100% ()

    d100% = Int(Rnd(1) * 100) + 1

End Function


'----------------------------------------
'       DISABLEPLAYERS Subroutine
'----------------------------------------
'This subroutine gives the user an inter-
'face for disabling team players. It will
'then take care of updating the associated
'arrays for the available roster.
Sub DISABLEPLAYERS (P9)

    Do

        Color 15, 0
        Cls

        Call TEAMBOX(1, P9)

        pbpString$ = "DISABLE PLAYERS FOR " + gameTeams$(P9) + "? (Y/N)"
        Locate , (80 - Len(pbpString$)) / 2
        Print pbpString$
        Color 15, 0

        Print
        Print
        Print "NOTE:: THE COMPUTER WIll AUTOMATICALLY DISABLE PLAYERS BASED ON"
        Print "       THE RATIO OF GAMES PLAYED FOR EACH PLAYER TO THE TOTAL"
        Print "       NUMBER OF GAMES PLAYED BY THE TEAM. YOU MAY WANT TO CHECK"
        Print "       TO ACTIVATE ANY COMPUTER DEACTIVATED PLAYERS"
        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If UCase$(I$) = "Y" Then

        DX% = 0

        Cls
        Print "TEAM ROSTER FOR ";
        Color teamRat_GAME(P9, 33), teamRat_GAME(P9, 34): Print gameTeams$(P9)
        Color 15, 0
        '-- Print "# NAME              POS GP"
        Print "# NAME              GP"

        For I = 0 To 14

            If I <= 9 Then Print Using "#"; I;
            If I = 10 Then Print "A";
            If I = 11 Then Print "B";
            If I = 12 Then Print "C";
            If I = 13 Then Print "D";
            If I = 14 Then Print "E";

            Print " ";

            'Automatically disable "non-existent" players
            If players_GAME$(P9, I, 0) = "XXX" Then
                plyrRat_GAME(P9, I, 18) = -1
                Color teamRat_GAME(P9, 33), teamRat_GAME(P9, 34)
            End If

            'Color players who have been disabled
            If plyrRat_GAME(P9, I, 18) <> 0 Then Color teamRat_GAME(P9, 33), teamRat_GAME(P9, 34)

            'Print player name
            Print players_GAME$(P9, I, 0);
            Color 15, 0

            'Print player position, GP
            '-- POSITION = posnName$(lineupIdx(P9, I1)) ???
            Print Tab(20); plyrRat_GAME(P9, I, 0)

        Next

        Do
            Locate 19, 1: Print "ENTER PLAYER # TO DISABLE/ENABLE (Z=FINISHED)"

            I$ = GetKeyPress$

            If UCase$(I$) <> "Z" Then

                I1 = Val(I$)
                Call LetterToNumber(I$, I1)

                If plyrRat_GAME(P9, I1, 18) = 0 And DX% < 6 Then
                    plyrRat_GAME(P9, I1, 18) = -1
                    DX% = DX% + 1
                    Locate 3 + I1, 3
                    Color teamRat_GAME(P9, 33), teamRat_GAME(P9, 34): Print players_GAME$(P9, I1, 0)
                    Color 15, 0
                Else
                    If plyrRat_GAME(P9, I1, 18) = -1 Then
                        plyrRat_GAME(P9, I1, 18) = 0
                        DX% = DX% - 1
                        Locate 3 + I1, 3
                        Color 15, 0
                        Print " "; players_GAME$(P9, I1, 0)
                    End If
                End If
            End If

        Loop Until UCase$(I$) = "Z"

    End If

End Sub


'----------------------------------------
'            DELAY Subroutine
'----------------------------------------
'This subroutine simply serves as a place-
'holder for the command to initiate the
'pbp delay.
Sub DELAY ()

    _Delay pbpDelay!

End Sub


'----------------------------------------
'             PBP Subroutine
'----------------------------------------
'This is the classic Haffner Play-by-play
'subroutine. Given the target text, it will
'place it in the correct spot in the pbp
'text window. No text is placed if the
'delay is 0.
Sub PBP (teamIdx)

    M$ = InKey$

    If UCase$(M$) = "O" Then
        Call OPTIONS
    End If

    If UCase$(M$) = "Q" Then
        Cls
        End
    End If

    'Positive delay
    If (autoPlay = 0 Or pbpDelay! > 0) And pbpOpt = 0 Then

        pbpString$ = PadRight$(pbpString$, 38)
        'Print #11, pbpString$

        Locate 8, 1

        Select Case nbrLines

            Case 1
                A1$ = pbpString$

            Case 2
                B1$ = pbpString$

            Case 3
                C1$ = pbpString$

            Case 4
                D1$ = pbpString$

            Case 5
                E1$ = pbpString$

            Case 6
                F1$ = pbpString$

            Case 7
                G1$ = pbpString$

            Case 8
                H1$ = pbpString$

            Case Is > 8
                A1$ = B1$
                B1$ = C1$
                C1$ = D1$
                D1$ = E1$
                E1$ = F1$
                F1$ = G1$
                G1$ = H1$
                H1$ = pbpString$

                For I = 1 To 7
                    pbpFG(I) = pbpFG(I + 1)
                    pbpBG(I) = pbpBG(I + 1)
                Next

        End Select

        If nbrLines < 8 Then
            pbpFG(nbrLines) = teamRat_GAME(teamIdx, 33)
            pbpBG(nbrLines) = teamRat_GAME(teamIdx, 34)
        Else
            pbpFG(8) = teamRat_GAME(teamIdx, 33)
            pbpBG(8) = teamRat_GAME(teamIdx, 34)
        End If

        nbrLines = nbrLines + 1

        Color pbpFG(1), pbpBG(1): Print Left$(A1$, 38)
        Color pbpFG(2), pbpBG(2): Print Left$(B1$, 38)
        Color pbpFG(3), pbpBG(3): Print Left$(C1$, 38)
        Color pbpFG(4), pbpBG(4): Print Left$(D1$, 38)
        Color pbpFG(5), pbpBG(5): Print Left$(E1$, 38)
        Color pbpFG(6), pbpBG(6): Print Left$(F1$, 38)
        Color pbpFG(7), pbpBG(7): Print Left$(G1$, 38)
        Color pbpFG(8), pbpBG(8): Print Left$(H1$, 38)

        Color 15, 0
        Call DELAY

    End If

End Sub



'----------------------------------------
'          SELECTPBP Subroutine
'----------------------------------------
'This subroutine essentially handles the
'prep for PBP depending on the style that
'was chosen for the game (classic / verbose)
Sub SELECTPBP (teamIdx, clean)

    Select Case pbpOpt

        Case 0:
            Call PBP(teamIdx)

        Case 1:
            If pbpDelay! > 0 Then
                If clean = 1 Then Call WINDEX
                Print pbpString$
                Call DELAY
            End If

    End Select

End Sub


'----------------------------------------
'          MAKEPBP Subroutine
'----------------------------------------
'This subroutine helps build strings for
'the Play-by-Play text, depending on the
'context given (assigned as pbpType)
Sub MAKEPBP (pbpType)

    'PLAY BY PLAY
    '--> only set this if we need to track where code is going
    'If autoPlay = 1 Then
    If (autoPlay = 0 Or pbpDelay! > 0) Then

        Call Rolld100(RN0)

        Select Case pbpType

            Case 1
                'TURNOVER
                pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0)
                RN1 = Int(Rnd(1) * 14) + 1

                Select Case RN1

                    Case 1: pbpString$ = pbpString$ + " TURNS THE BALL OVER"
                    Case 2: pbpString$ = pbpString$ + " THROWS IT AWAY"
                    Case 3: pbpString$ = pbpString$ + " IS WHISTLED FOR TRAVELING"
                    Case 4: pbpString$ = pbpString$ + " TURNS IT OVER"
                    Case 5: pbpString$ = pbpString$ + " LOSES CONTROL OF THE BALL"
                    Case 6: pbpString$ = pbpString$ + " LOSES IT OUT OF BOUNDS"
                    Case 7: pbpString$ = pbpString$ + " LOSES THE HANDLE"
                    Case 8: pbpString$ = pbpString$ + " LOSES IT"
                    Case 9: pbpString$ = pbpString$ + " IS CALLED FOR STEPS"
                    Case 10: pbpString$ = pbpString$ + " TRAVELS"
                    Case 11: pbpString$ = pbpString$ + " WALKS WITH IT"
                    Case 12: pbpString$ = pbpString$ + " LETS IT GET AWAY"
                    Case 13: pbpString$ = pbpString$ + " DOUBLE DRIBBLES"
                    Case 14: pbpString$ = pbpString$ + " LOSES CONTROL"

                End Select

            Case 2
                'DUNK
                RN1 = Int(Rnd(1) * 39) + 1

                Select Case RN1

                    Case 1: pbpString$ = "DIPSY-DOO DUNK-A-ROO!!!"
                    Case 2: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " MAKES IT HAMMER TIME!!!"
                    Case 3: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " SLAMS IT IN!!!"
                    Case 4: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE 2-HANDED SLAM!!!"
                    Case 5: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " SLAMS IT HOME!!!"
                    Case 6: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " JAMS IT HOME!!!"
                    Case 7: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " THROWS IT DOWN!!!"
                    Case 8: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " THUNDERSLAM!!!"
                    Case 9: pbpString$ = "RIM ROCKING JAM!!!"
                    Case 10: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE MONSTER SLAM!!"
                    Case 11: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + "POWER SLAMS IT!!!"
                    Case 12: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE 2-HANDED STUFF!!!"
                    Case 13: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " BACKWARDS JAM!!!"
                    Case 14: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE 1-HANDED SLAM!!!"
                    Case 15: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE REVERSE SLAM!!!"
                    Case 16: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE REVERSE JAM!!!"
                    Case 17: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE WINDMIll JAM!!!"
                    Case 18: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE WINDMIll SLAM!!!"
                    Case 19: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE TOMAHAWK SLAM!!!"
                    Case 20: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE 360 SLAM!!!"
                    Case 21: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE SPINNING SLAM!!!"
                    Case 22: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE LEFT HANDED DUNK!!!"
                    Case 23: pbpString$ = "DOUBLE CLUTCH JAM!!!"
                    Case 24: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " GETS THE FLUSH!!!"
                    Case 25: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " PUNCHES IT HOME!!!"
                    Case 26: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + "...DOUBLE PUMP SLAM!!!"
                    Case 27: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + "...IN YOUR FACE SLAM!!!"
                    Case 28, 37:
                        pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " IS UP"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " SPINS"
                        Call PBP(P)
                        pbpString$ = "BEHIND THE BACK SLAM!!!"
                    Case 29:
                        pbpString$ = teamGender$(P) + " SLAMS IT HOME!!"
                    Case 30:
                        pbpString$ = teamGender$(P) + " LAYS IT UP AND IN!!"
                    Case 31:
                        pbpString$ = teamGender$(P) + " BANGS IT IN!!"
                    Case 32:
                        pbpString$ = teamGender$(P) + " RAMS IT IN!!"
                    Case 33:
                        pbpString$ = teamGender$(P) + " SLAMS IT IN!!!"
                    Case 34:
                        pbpString$ = "DIPSY-DOO DUNK-A-ROO!!!"
                    Case 35:
                        pbpString$ = teamGender$(P) + " MAKES IT HAMMER TIME!!!"
                    Case 36:
                        pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE 2-HANDED SLAM!!!"
                    Case 38:
                        pbpString$ = teamGender$(P) + " JAMS IT HOME!!!"
                    Case 39:
                        pbpString$ = teamGender$(P) + " HAMMERS IT HOME!"

                End Select

            Case 3
                'SHOOTS...
                pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0)

                Select Case shotType

                    Case 1
                        'BAD

                        Select Case RN0

                            Case 1 To 40: pbpString$ = pbpString$ + " TAKES A TOUGH SHOT"
                            Case 41 To 45: pbpString$ = pbpString$ + " SHOOTS OFF BALANCE"
                            Case 46 To 50: pbpString$ = pbpString$ + " FORCES ONE UP"
                            Case 51 To 60: pbpString$ = pbpString$ + " SHOOTS UNDER PRESSURE"
                            Case 61 To 65: pbpString$ = pbpString$ + " WITH A BAD SHOT"
                            Case 66 To 70: pbpString$ = pbpString$ + " SHOOTS IN HEAVY TRAFFIC"
                            Case 71 To 75: pbpString$ = pbpString$ + " THROWS UP A PRAYER"
                            Case 76 To 80: pbpString$ = pbpString$ + " TAKES A BAD SHOT"
                            Case 81 To 85: pbpString$ = pbpString$ + " FORCES THE INSIDE SHOT"
                            Case 86 To 100: pbpString$ = pbpString$ + " TURNS AND SHOOTS"

                        End Select

                    Case 2
                        'INSIDE W/ASSIST
                        checkForDunk = 0

                        Select Case RN0

                            Case 1 To 10
                                pbpString$ = teamGender$(P) + " CUTS TO THE BASKET"
                                Call PBP(P)
                                pbpString$ = "TAKES IT UP"

                                checkForDunk = 1

                            Case 11 To 20
                                pbpString$ = teamGender$(P) + " TAKES IT TO THE HOLE"

                                checkForDunk = 1

                            Case 21 To 30
                                pbpString$ = teamGender$(P) + " TAKES IT TO THE HOOP"

                                checkForDunk = 1

                            Case 31 To 40
                                pbpString$ = teamGender$(P) + " LEANS IN AND SHOOTS"

                            Case 41 To 50
                                pbpString$ = teamGender$(P) + " PUTS IT UP"

                            Case 51 To 60
                                pbpString$ = teamGender$(P) + " TAKES IT TO THE BOARD"

                                checkForDunk = 1

                            Case 61 To 70
                                pbpString$ = teamGender$(P) + " PUTS IT OFF THE GLASS"

                            Case 71 To 80
                                If ballCarrier = 4 Then
                                    pbpString$ = teamGender$(P) + " SHOOTS THE HOOK"
                                Else
                                    pbpString$ = teamGender$(P) + " GOES IN FOR THE LAYUP"
                                End If

                            Case 81 To 90
                                pbpString$ = teamGender$(P) + " TAKES IT UP STRONG"

                                checkForDunk = 1

                            Case 91 To 95
                                pbpString$ = teamGender$(P) + " POWERS IT UP"

                                checkForDunk = 1

                            Case 96 To 100
                                pbpString$ = teamGender$(P) + " LAYS IT UP"

                        End Select

                        If checkForDunk = 1 Then

                            Call Rolld100(RN1)

                            If teamYears(eraChoice) <= 2000 Then

                                If (ballCarrier = 0 Or ballCarrier = 1) And RN1 <= 19 Then
                                    willDunk = 1
                                End If

                                If (ballCarrier = 2 Or ballCarrier = 3) And RN1 <= 99 Then
                                    willDunk = 1
                                End If

                                If ballCarrier = 4 And RN1 <= 19 Then
                                    willDunk = 1
                                End If

                            Else

                                'Favor modern outcomes if teams are
                                'from different eras
                                If (ballCarrier = 0 Or ballCarrier = 1) And RN1 <= 36 Then
                                    willDunk = 1
                                End If

                                If (ballCarrier = 2 Or ballCarrier = 3) And RN1 <= 84 Then
                                    willDunk = 1
                                End If

                                If ballCarrier = 4 And RN1 <= 55 Then
                                    willDunk = 1
                                End If

                            End If

                        End If

                    Case 3
                        'PENETRATE
                        checkForDunk = 0

                        If activePosition$ = "G" Then

                            Select Case RN0

                                Case 1 To 25:
                                    pbpString$ = teamGender$(P) + " SPINS FREE"

                                    Call PBP(P)
                                    pbpString$ = "SHOOTS"

                                Case 26 To 50:
                                    pbpString$ = teamGender$(P) + " TAKES IT UP UNDER THE BASKET"

                                    Call Rolld100(RN1)

                                    checkForDunk = 1

                                Case 51 To 52:
                                    pbpString$ = teamGender$(P) + " MAKES A NICE SPIN MOVE"

                                    Call PBP(P)
                                    pbpString$ = "AND SOARS TO THE HOOP"

                                    checkForDunk = 1

                                Case 53 To 54:
                                    pbpString$ = teamGender$(P) + " BLOWS PAST TWO DEFENDERS"

                                Case 55 To 56:
                                    pbpString$ = teamGender$(P) + " DRIVES THE BASELINE"

                                    Call PBP(P)
                                    pbpString$ = "SPINS RIGHT AND GOES UP"

                                    checkForDunk = 1

                                Case 57:
                                    pbpString$ = teamGender$(P) + " SLICES IN THE HOLE"

                                    Call PBP(P)
                                    pbpString$ = "UNDERHAND SCOOP SHOT"

                                Case 58:
                                    pbpString$ = "STRONG CUT ALONG THE BASELINE"

                                    Call PBP(P)
                                    pbpString$ = "DOUBLE PUMP REVERSE LAYUP"

                                Case 59:
                                    pbpString$ = teamGender$(P) + " PENETRATES"

                                    Call PBP(P)
                                    pbpString$ = "GORGEOUS FINGER ROll!"

                                Case 60 To 75:
                                    pbpString$ = teamGender$(P) + " TAKES THE JUMPER"

                                Case 76 To 90:
                                    pbpString$ = teamGender$(P) + " STOPS"

                                    Call PBP(P)
                                    pbpString$ = "SHOOTS"

                                Case 91:
                                    pbpString$ = teamGender$(P) + " LOOKS TO PASS"

                                    Call PBP(P)
                                    pbpString$ = "NO ONE IS OPEN"

                                    Call PBP(P)
                                    pbpString$ = teamGender$(P) + " PUTS IT UP"

                                Case 92 To 100:
                                    pbpString$ = teamGender$(P) + " TAKES THE SHOT"

                            End Select


                            If checkForDunk = 1 Then

                                Call Rolld100(RN1)

                                If teamYears(eraChoice) <= 2000 Then

                                    If RN1 <= 31 Then
                                        willDunk = 1
                                    End If

                                Else

                                    'Favor modern outcomes if teams are
                                    'from different eras
                                    If RN1 <= 46 Then
                                        willDunk = 1
                                    End If

                                End If

                            End If


                        End If

                        If activePosition$ = "F" Then

                            Select Case RN0

                                Case 1 To 16:
                                    pbpString$ = teamGender$(P) + " TAKES IT UP"

                                    checkForDunk = 1

                                Case 17 To 32:
                                    pbpString$ = teamGender$(P) + " SPINS FREE"
                                    Call PBP(P)
                                    pbpString$ = "SHOOTS"

                                    checkForDunk = 1

                                Case 33 To 48:
                                    pbpString$ = teamGender$(P) + " GETS LOOSE IN THE PAINT"
                                    Call PBP(P)
                                    pbpString$ = "TAKES IT UP"

                                    checkForDunk = 1

                                Case 49 To 64:
                                    pbpString$ = teamGender$(P) + " PUTS IT UP"

                                Case 65 To 67:
                                    pbpString$ = teamGender$(P) + " CURLS OFF A PICK IN THE PAINT"

                                    Call PBP(P)
                                    pbpString$ = "TAKES IT UP"

                                    checkForDunk = 1

                                Case 68 To 69:
                                    pbpString$ = teamGender$(P) + " MAKES A NICE SPIN MOVE"

                                    Call PBP(P)
                                    pbpString$ = "AND SOARS TO THE HOOP"

                                    checkForDunk = 1

                                Case 70 To 71:
                                    pbpString$ = teamGender$(P) + " DRIVES THE BASELINE"

                                    Call PBP(P)
                                    pbpString$ = "SPINS RIGHT AND GOES UP"

                                    checkForDunk = 1

                                Case 72:
                                    pbpString$ = teamGender$(P) + " SLICES IN THE HOLE"

                                    Call PBP(P)
                                    pbpString$ = "UNDERHAND SCOOP SHOT"

                                Case 73:
                                    pbpString$ = "STRONG CUT ALONG THE BASELINE"

                                    Call PBP(P)
                                    pbpString$ = "DOUBLE PUMP REVERSE LAYUP"

                                Case 74:
                                    pbpString$ = teamGender$(P) + " PENETRATES"

                                    Call PBP(P)
                                    pbpString$ = "GORGEOUS FINGER ROll!"

                                Case 75 To 80:
                                    pbpString$ = teamGender$(P) + " SHOOTS"

                                Case 81 To 100:
                                    pbpString$ = teamGender$(P) + " TAKES THE SHOT"

                            End Select

                            If checkForDunk = 1 Then

                                Call Rolld100(RN1)

                                If teamYears(eraChoice) <= 2000 Then

                                    If RN1 <= 77 Then
                                        willDunk = 1
                                    End If

                                Else

                                    'Favor modern outcomes if teams are
                                    'from different eras
                                    If RN1 <= 99 Then
                                        willDunk = 1
                                    End If

                                End If

                            End If

                        End If

                        If activePosition$ = "C" Then

                            Select Case RN0

                                Case 1 To 15:
                                    pbpString$ = teamGender$(P) + " PUTS IT OFF THE GLASS"

                                Case 16 To 25:
                                    pbpString$ = teamGender$(P) + " PIVOTS TO THE BASKET"

                                Case 26 To 40:
                                    pbpString$ = teamGender$(P) + " POWERS IT UP"

                                Case 41 To 58:
                                    pbpString$ = teamGender$(P) + " TAKES IT TO THE HOOP"

                                    checkForDunk = 1

                                Case 59 To 70:
                                    pbpString$ = teamGender$(P) + " TAKES THE SHOT"

                                Case 71 To 72:
                                    pbpString$ = teamGender$(P) + " CURLS OFF A PICK IN THE PAINT"
                                    Call PBP(P)
                                    pbpString$ = "TAKES IT UP"

                                    checkForDunk = 1

                                Case 73 To 74:
                                    pbpString$ = teamGender$(P) + " MAKES A NICE SPIN MOVE"
                                    Call PBP(P)
                                    pbpString$ = "AND SOARS TO THE HOOP"

                                    checkForDunk = 1

                                Case 75 To 100:
                                    pbpString$ = teamGender$(P) + " TAKES IT UP STRONG"

                                    checkForDunk = 1

                            End Select

                            If checkForDunk = 1 Then

                                Call Rolld100(RN1)

                                If teamYears(eraChoice) <= 2000 Then

                                    If RN1 <= 23 Then
                                        willDunk = 1
                                    End If

                                Else

                                    'Favor modern outcomes if teams are
                                    'from different eras
                                    If RN1 <= 38 Then
                                        willDunk = 1
                                    End If

                                End If

                            End If

                        End If

                    Case 4
                        'THREE!
                        If forcedThree = 1 Then

                            If RN0 <= 50 Then
                                pbpString$ = pbpString$ + " THROWS UP A PRAYER"
                            Else
                                pbpString$ = pbpString$ + " LAUNCHES A THREE"
                            End If

                            forcedThree = 0
                        Else

                            Select Case RN0

                                Case 1 To 10: pbpString$ = pbpString$ + " TRIES FOR THREE!"
                                Case 11 To 20: pbpString$ = pbpString$ + " SHOOTS FROM 3-POINT RANGE!"
                                Case 21 To 30: pbpString$ = pbpString$ + " HOISTS A THREE!"
                                Case 31 To 40: pbpString$ = pbpString$ + " LOBS A THREE FROM THE CORNER!"
                                Case 41 To 50: pbpString$ = pbpString$ + " FIRES FROM THE 3-POINT LINE!"
                                Case 51 To 60: pbpString$ = pbpString$ + " SHOOTS FOR THREE!"
                                Case 61 To 70: pbpString$ = pbpString$ + " SHOOTS THE 3-POINTER!"
                                Case 71 To 80: pbpString$ = pbpString$ + " FOR 3...!"
                                Case 81 To 90: pbpString$ = pbpString$ + " PUTS UP THE TREY!"
                                Case 91 To 100: pbpString$ = pbpString$ + " TAKES THE 3-POINTER!"

                            End Select

                        End If

                    Case 5
                        'SHORT JUMP
                        RN1 = Int(Rnd(1) * 59) + 1

                        Select Case RN1

                            Case 1: pbpString$ = pbpString$ + " WITH A TWISTING JUMPER"

                            Case 2: pbpString$ = pbpString$ + " SHOOTS THE J"

                            Case 3:
                                If ballCarrier = 4 Then
                                    pbpString$ = pbpString$ + " RELEASES THE HOOK"
                                Else
                                    pbpString$ = pbpString$ + " TAKES THE SHOT"
                                End If

                            Case 4: pbpString$ = pbpString$ + " WITH A SHORT JUMPER"

                            Case 5: pbpString$ = pbpString$ + " WITH THE 10 FOOTER"

                            Case 6:
                                If ballCarrier = 4 Then
                                    pbpString$ = pbpString$ + " WITH THE RUNNING HOOK"
                                Else
                                    pbpString$ = pbpString$ + " WITH THE RUNNER"
                                End If

                            Case 7:
                                If ballCarrier = 4 Then
                                    pbpString$ = pbpString$ + " WITH A SWEEPING HOOK"
                                Else
                                    pbpString$ = pbpString$ + " WORKS FREE AND SHOOTS"
                                End If

                            Case 8:
                                If ballCarrier = 4 Then
                                    pbpString$ = pbpString$ + " WITH A BABY HOOK SHOT"
                                Else
                                    pbpString$ = pbpString$ + " TAKES THE SHOT"
                                End If

                            Case 9: pbpString$ = pbpString$ + " LEANS IN AND SHOOTS"
                            Case 10: pbpString$ = pbpString$ + " SHOOTS"
                            Case 11: pbpString$ = pbpString$ + " SHOOTS FALLING AWAY"
                            Case 12: pbpString$ = pbpString$ + " LEANS IN FOR THE SHOT"
                            Case 13: pbpString$ = pbpString$ + " TAKES THE LEFT HANDER"
                            Case 14: pbpString$ = pbpString$ + " HARD TO THE HOLE..."
                            Case 15: pbpString$ = pbpString$ + " FLIPS IT UP"
                            Case 16: pbpString$ = pbpString$ + " FORCES THE INSIDE SHOT"
                            Case 17: pbpString$ = pbpString$ + " CUTS TO THE BASKET"
                            Case 18: pbpString$ = pbpString$ + " TAKES IT IN THE PAINT"

                            Case 19:
                                If teamGender$(P) = "SHE" Then
                                    pbpString$ = teamGender$(P) + " FLOATS IT OVER HER DEFENDER"
                                Else
                                    pbpString$ = teamGender$(P) + " FLOATS IT OVER HIS MAN"
                                End If

                            Case 20:
                                If teamGender$(P) = "SHE" Then
                                    pbpString$ = teamGender$(P) + " TAKES HER DEFENDER INSIDE"
                                Else
                                    pbpString$ = teamGender$(P) + " TAKES HIS MAN INSIDE"
                                End If

                            Case 21: pbpString$ = pbpString$ + " WHEELS IN THE LANE"
                            Case 22: pbpString$ = pbpString$ + " CUTS INSIDE FOR THE SHOT"
                            Case 23: pbpString$ = pbpString$ + " PENETRATES AND SHOOTS"
                            Case 24: pbpString$ = pbpString$ + " SLICES INSIDE"
                            Case 25: pbpString$ = pbpString$ + " LOOKS FOR TWO"
                            Case 26: pbpString$ = pbpString$ + " DRIVES INTO THE PAINT"
                            Case 27: pbpString$ = pbpString$ + " HAS THE BASELINE JUMPER"
                            Case 28: pbpString$ = pbpString$ + " GLIDES TO THE HOLE"
                            Case 29: pbpString$ = pbpString$ + " GETS INSIDE FOR THE SHOT"
                            Case 30: pbpString$ = pbpString$ + " SPINS TOWARD THE HOOP"
                            Case 31: pbpString$ = pbpString$ + " FROM SHORT RANGE"
                            Case 32: pbpString$ = pbpString$ + " GOES BASELINE"
                            Case 33: pbpString$ = pbpString$ + " PENETRATES AND SHOOTS"
                            Case 34: pbpString$ = pbpString$ + " FROM 8 FEET"
                            Case 35: pbpString$ = pbpString$ + " FROM 6 FEET"
                            Case 36: pbpString$ = pbpString$ + " FROM 5 FEET"
                            Case 37: pbpString$ = pbpString$ + " DRIVES TO THE HOLE"
                            Case 38: pbpString$ = pbpString$ + " GLIDES TO THE HOLE"
                            Case 39: pbpString$ = pbpString$ + " GETS INSIDE FOR THE SHOT"
                            Case 40: pbpString$ = pbpString$ + " GOES TO THE HOLE"
                            Case 41: pbpString$ = pbpString$ + " CUTS INSIDE"
                            Case 42: pbpString$ = pbpString$ + " SPINS INSIDE"
                            Case 43: pbpString$ = pbpString$ + " BREAKS INSIDE"
                            Case 44: pbpString$ = pbpString$ + " SNEAKS INSIDE"
                            Case 45: pbpString$ = pbpString$ + " SCOOTS INSIDE"
                            Case 46: pbpString$ = pbpString$ + " WORKS IT INSIDE"
                            Case 47: pbpString$ = pbpString$ + " KNIFES INSIDE"
                            Case 48: pbpString$ = pbpString$ + " SCISSORS THROUGH"
                            Case 49: pbpString$ = pbpString$ + " BANGS INSIDE"
                            Case 50: pbpString$ = pbpString$ + " SLICES INSIDE"
                            Case 51: pbpString$ = pbpString$ + " PUMPS AND SHOOTS"
                            Case 52: pbpString$ = pbpString$ + " DOUBLE PUMPS AND SHOOTS"
                            Case 53: pbpString$ = pbpString$ + "...ONE HANDED FADE AWAY"
                            Case 54: pbpString$ = pbpString$ + "...LEFT HANDED FADE AWAY"
                            Case 55: pbpString$ = pbpString$ + "...HIGH ARCHING SHOT"
                            Case 56: pbpString$ = pbpString$ + " WITH A RUNNING LEFT HANDER"
                            Case 57: pbpString$ = pbpString$ + " WITH A RUNNING SCOOP SHOT"
                            Case 58: pbpString$ = pbpString$ + " WITH THE SCOOP SHOT"
                            Case 59: pbpString$ = pbpString$ + " WITH THE FINGER ROll"

                        End Select

                    Case 6, 7

                        RN1 = Int(Rnd(1) * 59) + 1

                        Select Case RN1
                            Case 1: pbpString$ = pbpString$ + " POPS FROM OUTSIDE"
                            Case 2: pbpString$ = pbpString$ + " DECIDES TO SHOOT IT"
                            Case 3: pbpString$ = pbpString$ + " FIRES THE FADE-AWAY"
                            Case 4: pbpString$ = pbpString$ + " LETS IT FLY"
                            Case 5: pbpString$ = pbpString$ + " LAUNCHES ONE"
                            Case 6: pbpString$ = pbpString$ + " WITH THE OUTSIDE SHOT"
                            Case 7: pbpString$ = pbpString$ + " OUT OF THE CORNER"
                            Case 8: pbpString$ = pbpString$ + " PUTS IT UP FROM 18"
                            Case 9: pbpString$ = pbpString$ + " SPOTS UP OUTSIDE"
                            Case 10: pbpString$ = pbpString$ + " RELEASES THE JUMPER"
                            Case 11: pbpString$ = pbpString$ + " FROM THE TOP OF THE KEY"
                            Case 12: pbpString$ = pbpString$ + " FIRES FROM THE LEFT WING"
                            Case 13: pbpString$ = pbpString$ + " TRIES THE 12 FOOTER"
                            Case 14: pbpString$ = pbpString$ + " PUlastLoseStreak UP AND POPS"
                            Case 15: pbpString$ = pbpString$ + " FIRES FROM 15"
                            Case 16: pbpString$ = pbpString$ + " FROM THE PERIMETER"
                            Case 17: pbpString$ = pbpString$ + " TURNS AND SHOOTS"
                            Case 18: pbpString$ = pbpString$ + " SHOOTS OFF THE SCREEN"
                            Case 19: pbpString$ = pbpString$ + " FAKES AND SHOOTS"
                            Case 20: pbpString$ = pbpString$ + " FIRES AWAY"
                            Case 21: pbpString$ = pbpString$ + " POPS FROM THE BASELINE"
                            Case 22: pbpString$ = pbpString$ + " DRIVES THE LANE"
                            Case 23: pbpString$ = pbpString$ + " TAKES THE SHOT"
                            Case 24: pbpString$ = pbpString$ + " WORKS FREE AND FIRES"

                            Case 25:
                                If teamGender$(P) = "SHE" Then
                                    pbpString$ = teamGender$(P) + " SHOOTS OVER HER DEFENDER"
                                Else
                                    pbpString$ = teamGender$(P) + " SHOOTS OVER HIS MAN"
                                End If

                            Case 26: pbpString$ = pbpString$ + " SHOOTS FALLING AWAY"
                            Case 27: pbpString$ = pbpString$ + " FAKES AND SHOOTS"
                            Case 28: pbpString$ = pbpString$ + " FIRES AWAY"
                            Case 29: pbpString$ = pbpString$ + " POPS FROM THE BASELINE"
                            Case 30: pbpString$ = pbpString$ + " DRIVES THE LANE"
                            Case 31: pbpString$ = pbpString$ + " TAKES THE SHOT"
                            Case 32: pbpString$ = pbpString$ + " WORKS FREE AND FIRES"
                            Case 33: pbpString$ = pbpString$ + " SHOOTS FROM 15"
                            Case 34: pbpString$ = pbpString$ + " SHOOTS"
                            Case 35: pbpString$ = pbpString$ + " TAKES THE JUMPER"
                            Case 36: pbpString$ = pbpString$ + " FROM THE RIGHT CORNER"
                            Case 37: pbpString$ = pbpString$ + " FROM THE LEFT CORNER"
                            Case 38: pbpString$ = pbpString$ + " FROM THE KEY"
                            Case 39: pbpString$ = pbpString$ + " FIRES FROM OUTSIDE"
                            Case 40: pbpString$ = pbpString$ + " PUTS IT UP FROM 16"
                            Case 41: pbpString$ = pbpString$ + " FROM THE PERIMETER"
                            Case 42: pbpString$ = pbpString$ + " FAKES AND SHOOTS"
                            Case 43: pbpString$ = pbpString$ + " FROM AROUND 16 FEET"
                            Case 44: pbpString$ = pbpString$ + " PUTS IT UP"
                            Case 45: pbpString$ = pbpString$ + " WITH A RUNNING ONE-HANDER"
                            Case 46: pbpString$ = pbpString$ + " PUlastLoseStreak UP"
                            Case 47: pbpString$ = pbpString$ + " KISSES THE GLASS"
                            Case 48: pbpString$ = pbpString$ + " SPINS AND SHOOTS"
                            Case 49: pbpString$ = pbpString$ + " PUTS IT UP FROM 16"
                            Case 50: pbpString$ = pbpString$ + " WITH A LONG JUMPER"
                            Case 51: pbpString$ = pbpString$ + " PUMPS AND SHOOTS"
                            Case 52: pbpString$ = pbpString$ + " DOUBLE PUMPS AND SHOOTS"
                            Case 53: pbpString$ = pbpString$ + "...ONE HANDED FADE AWAY"
                            Case 54: pbpString$ = pbpString$ + "...LEFT HANDED FADE AWAY"
                            Case 55: pbpString$ = pbpString$ + "...HIGH ARCHING SHOT"
                            Case 56: pbpString$ = pbpString$ + " WITH A RUNNING LEFT HANDER"
                            Case 57: pbpString$ = pbpString$ + " WITH A RUNNING SCOOP SHOT"
                            Case 58: pbpString$ = pbpString$ + " WITH THE SCOOP SHOT"
                            Case 59: pbpString$ = pbpString$ + " WITH THE FINGER ROll"

                        End Select

                    Case 8

                        Select Case RN0

                            Case 1 To 20: pbpString$ = teamGender$(P) + " SAILS TO THE HOOP"
                            Case 21 To 23: pbpString$ = teamGender$(P) + " TAKES IT TO THE BUCKET"
                            Case 24 To 26: pbpString$ = teamGender$(P) + " TAKES IT TO THE HOLE"
                            Case 27 To 29: pbpString$ = teamGender$(P) + " SLASHES TO THE HOLE"
                            Case 30 To 32: pbpString$ = teamGender$(P) + " SLASHES TO THE BUCKET"
                            Case 33 To 35: pbpString$ = teamGender$(P) + " SOARS TO THE BUCKET"
                            Case 36 To 38: pbpString$ = teamGender$(P) + " SOARS TO THE HOLE"
                            Case 39 To 40: pbpString$ = teamGender$(P) + " GLIDES TO THE HOLE"
                            Case 41 To 60: pbpString$ = teamGender$(P) + " GOES IN STRONG"
                            Case 61 To 80: pbpString$ = teamGender$(P) + " TAKES IT IN"
                            Case 81 To 90:
                                If teamGender$(P) = "SHE" Then
                                    pbpString$ = teamGender$(P) + " TAKES A DEFENDER WITH HER"
                                Else
                                    pbpString$ = teamGender$(P) + " TAKES A DEFENDER WITH HIM"
                                End If

                            Case 81 To 100: pbpString$ = teamGender$(P) + " GOES UP WITH IT"

                        End Select

                    Case 9
                        'BACK AFTER REB
                        pbpString$ = teamGender$(P) + " "
                        RN1 = Int(Rnd(1) * 11) + 1

                        Select Case RN1

                            Case 1:
                                pbpString$ = pbpString$ + "TAKES IT BACK UP"
                            Case 2:
                                pbpString$ = pbpString$ + "GOES BACK UP WITH IT"
                            Case 3:
                                If teamGender$(P) = "SHE" Then
                                    pbpString$ = pbpString$ + "POWERS HERS WAY UP WITH IT"
                                Else
                                    pbpString$ = pbpString$ + "POWERS HIS WAY UP WITH IT"
                                End If

                            Case 4:
                                pbpString$ = pbpString$ + "GOES STRONG TO THE BASKET"
                            Case 5:
                                pbpString$ = pbpString$ + "MUSCLES IT UP"
                            Case 6:
                                pbpString$ = pbpString$ + "SHOOTS"
                            Case 7:
                                pbpString$ = pbpString$ + "TAKES IT TO THE GOAL"
                            Case 8:
                                pbpString$ = pbpString$ + "PUTS IT UP AGAIN"
                            Case 9:
                                pbpString$ = pbpString$ + "SHOOTS IT AGAIN"
                            Case 10:
                                If teamGender$(P) = "SHE" Then
                                    pbpString$ = pbpString$ + "CLAWS HER WAY UP"
                                Else
                                    pbpString$ = pbpString$ + "CLAWS HIS WAY UP"
                                End If


                            Case 11: pbpString$ = pbpString$ + "SHOOTS IN TRAFFIC"

                        End Select

                        Call Rolld100(RN2)

                        If teamYears(eraChoice) <= 2000 Then

                            If (ballCarrier = 2 Or ballCarrier = 3) And Int(Rnd(1) * 1000) <= 33 Then
                                willDunk = 1
                            End If

                            If ballCarrier = 4 And RN2 <= 10 Then
                                willDunk = 1
                            End If

                        Else

                            'Favor modern outcomes if teams are
                            'from different eras
                            If (ballCarrier = 2 Or ballCarrier = 3) And Int(Rnd(1) * 1000) <= 45 Then
                                willDunk = 1
                            End If

                            If ballCarrier = 4 And RN2 <= 14 Then
                                willDunk = 1
                            End If

                        End If

                End Select

            Case 4
                'THREE GOOD
                pbpString$ = teamGender$(P)

                Select Case RN0

                    Case 1 To 6
                        pbpString$ = teamGender$(P) + " DRAINS THE THREE!"
                    Case 7 To 10
                        pbpString$ = teamGender$(P) + " FIlastLoseStreak UP THE BASKET!!!"
                    Case 11 To 20:
                        pbpString$ = pbpString$ + " HITS THE TREY!!!"
                    Case 21 To 30:
                        pbpString$ = "IT'S GOOOODD!!"
                    Case 31 To 40:
                        pbpString$ = pbpString$ + " GETS IT!!!"
                    Case 41 To 50:
                        pbpString$ = "COUNT IT!!"
                    Case 51 To 60:
                        pbpString$ = pbpString$ + " RINGS IT UP!!!"
                    Case 61 To 80:
                        pbpString$ = pbpString$ + " DRAINS IT!!!"
                    Case 81 To 90:
                        pbpString$ = pbpString$ + " BURIES IT!!!"
                    Case 91 To 100:
                        pbpString$ = "IT'S ALL NET!!!"

                End Select

                If d100% <= 1 Then

                    pbpString$ = "NO!"
                    Call PBP(P)

                    If teamGender$(P) = "HE" Then
                        pbpString$ = "HIS"
                    Else
                        pbpString$ = "HER"
                    End If

                    pbpString$ = pbpString$ + " FOOT IS ON THE 3-POINT LINE"

                    XX% = 1

                End If

            Case 5
                'THREE BAD
                pbpString$ = teamGender$(P) + " "

                Select Case RN0

                    Case 1 To 20:
                        pbpString$ = pbpString$ + "GETS NONE"
                    Case 21 To 25:
                        pbpString$ = "IT NEVER HAD A CHANCE"
                    Case 26 To 35:
                        pbpString$ = "AIRRRR BALLtotTeamLosses!"
                    Case 36 To 40:
                        pbpString$ = pbpString$ + "MISFIRES"
                    Case 41 To 60:
                        pbpString$ = "IT'S OFF THE RIM...NO GOOD!"
                    Case 61 To 80:
                        pbpString$ = "IT'S SHORT"
                    Case 81 To 100:
                        pbpString$ = pbpString$ + "MISSES"

                End Select

            Case 6
                'FAST BREAK
                pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0)
                RN1 = Int(Rnd(1) * 38) + 1

                Select Case RN1

                    Case 1: pbpString$ = pbpString$ + " HAS IT ON THE FAST BREAK!"
                    Case 2: pbpString$ = pbpString$ + " IS IN TRANSITION!"
                    Case 3: pbpString$ = pbpString$ + " STARTS THE BREAK!"
                    Case 4: pbpString$ = pbpString$ + " IS DOWN COURT IN A HURRY!"
                    Case 5: pbpString$ = pbpString$ + " IS ON THE RUN!"
                    Case 6: pbpString$ = pbpString$ + " GOES COAST TO COAST!"
                    Case 7: pbpString$ = pbpString$ + " ON THE BREAK!"
                    Case 8: pbpString$ = pbpString$ + " ON THE RUN!"
                    Case 9: pbpString$ = pbpString$ + " ON THE FAST BREAK!"
                    Case 10: pbpString$ = pbpString$ + " HAS THE FAST BREAK!"
                    Case 11: pbpString$ = pbpString$ + " BREAKS INTO THE FORECOURT!"
                    Case 12: pbpString$ = pbpString$ + " FLIES UPCOURT!"
                    Case 13: pbpString$ = pbpString$ + " STARTS THE FASTBREAK!"
                    Case 14: pbpString$ = pbpString$ + " ON THE MOVE!"
                    Case 15: pbpString$ = pbpString$ + " IN TRANSITION!"
                    Case 16: pbpString$ = pbpString$ + " DOWN COURT IN A HURRY!"
                    Case 17: pbpString$ = pbpString$ + " CHARGES ACROSS THE LINE!"
                    Case 18: pbpString$ = pbpString$ + " BREAKS QUICKLY!"
                    Case 19: pbpString$ = pbpString$ + " BREAKS DOWN COURT!"
                    Case 20: pbpString$ = pbpString$ + " STARTS THE RUNNING GAME!"
                    Case 21: pbpString$ = pbpString$ + " FLIES DOWN THE FLOOR!"
                    Case 22: pbpString$ = pbpString$ + " GOES RIGHT TO WORK!"
                    Case 23: pbpString$ = pbpString$ + " BACK THE OTHER WAY!"
                    Case 24: pbpString$ = pbpString$ + " QUICKLY UP THE FLOOR!"
                    Case 25: pbpString$ = pbpString$ + " PUSHES IT UP COURT!"
                    Case 26: pbpString$ = pbpString$ + " WASTES NO TIME!"
                    Case 27: pbpString$ = pbpString$ + " UPS THE TEMPO!"
                    Case 28: pbpString$ = pbpString$ + " HUSTLES DOWN COURT!"
                    Case 29: pbpString$ = pbpString$ + " RUSHES UP THE FLOOR!"
                    Case 30: pbpString$ = pbpString$ + " ON THE MOVE!"
                    Case 31: pbpString$ = pbpString$ + " STREAKS UP THE COURT!"
                    Case 32: pbpString$ = pbpString$ + " FLIES DOWN THE COURT!"
                    Case 33: pbpString$ = pbpString$ + " JETS DOWN COURT!"
                    Case 34: pbpString$ = pbpString$ + " QUICKLY GETS IT UP COURT!"
                    Case 35: pbpString$ = pbpString$ + " TAKES IT COAST TO COAST!"
                    Case 36: pbpString$ = pbpString$ + " IS RUNNING AND GUNNING!"
                    Case 37: pbpString$ = pbpString$ + " FLIES OVER THE TIME LINE!"
                    Case 38: pbpString$ = pbpString$ + " RUSHES IT UP THE FLOOR!"

                End Select

            Case 7
                'FAST BREAK SCORE
                fastBreak = 0
                RN1 = Int(Rnd(1) * 39) + 1

                Select Case RN1

                    Case 1: pbpString$ = "DIPSY-DOO DUNK-A-ROO!!!"
                    Case 2: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " MAKES IT HAMMER TIME!!!"
                    Case 3: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " SLAMS IT IN!!!"
                    Case 4: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE 2-HANDED SLAM!!!"
                    Case 5: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " SLAMS IT HOME!!!"
                    Case 6: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " JAMS IT HOME!!!"
                    Case 7: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " THROWS IT DOWN!!!"
                    Case 8: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " THUNDERSLAM!!!"
                    Case 9: pbpString$ = "RIM ROCKING JAM!!!"
                    Case 10: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE MONSTER SLAM!!"
                    Case 11: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + "POWER SLAMS IT!!!"
                    Case 12: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE 2-HANDED STUFF!!!"
                    Case 13: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " BACKWARDS JAM!!!"
                    Case 14: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE 1-HANDED SLAM!!!"
                    Case 15: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE REVERSE SLAM!!!"
                    Case 16: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE REVERSE JAM!!!"
                    Case 17: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE WINDMIll JAM!!!"
                    Case 18: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE WINDMIll SLAM!!!"
                    Case 19: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE TOMAHAWK SLAM!!!"
                    Case 20: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE 360 SLAM!!!"
                    Case 21: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE SPINNING SLAM!!!"
                    Case 22: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE LEFT HANDED DUNK!!!"
                    Case 23: pbpString$ = "DOUBLE CLUTCH JAM!!!"
                    Case 24: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " GETS THE FLUSH!!!"
                    Case 25: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " PUNCHES IT HOME!!!"
                    Case 26: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + "...DOUBLE PUMP SLAM!!!"
                    Case 27: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + "...IN YOUR FACE SLAM!!!"
                    Case 28, 37:
                        pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " IS UP"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " SPINS"
                        Call PBP(P)
                        pbpString$ = "BEHIND THE BACK SLAM!!!"
                    Case 29: pbpString$ = teamGender$(P) + " SLAMS IT HOME!!"
                    Case 30: pbpString$ = teamGender$(P) + " LAYS IT UP AND IN!!"
                    Case 31: pbpString$ = teamGender$(P) + " BANGS IT IN!!"
                    Case 32: pbpString$ = teamGender$(P) + " RAMS IT IN!!"
                    Case 33: pbpString$ = teamGender$(P) + " SLAMS IT IN!!!"
                    Case 34: pbpString$ = "DIPSY-DOO DUNK-A-ROO!!!"
                    Case 35: pbpString$ = teamGender$(P) + " MAKES IT HAMMER TIME!!!"
                    Case 36: pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " WITH THE 2-HANDED SLAM!!!"
                    Case 38: pbpString$ = teamGender$(P) + " JAMS IT HOME!!!"
                    Case 39: pbpString$ = teamGender$(P) + " HAMMERS IT HOME!"

                End Select

            Case 8
                'MISS
                If shotType = 8 Then

                    shotType = 0
                    fastBreak = 0
                    pbpString$ = teamGender$(P) + " "

                    Select Case RN0

                        Case 1 To 20: pbpString$ = pbpString$ + "MISSES THE EASY BASKET!"
                        Case 21 To 40: pbpString$ = pbpString$ + "JAMS IT TOO HARD!!"
                        Case 41 To 60: pbpString$ = pbpString$ + "SLAMS IT OFF THE IRON...NO GOOD!"
                        Case 61 To 80: pbpString$ = pbpString$ + "CAN'T GET IT DOWN!"
                        Case 81 To 100: pbpString$ = pbpString$ + "BLOWS THE EASY BASKET!!"

                    End Select

                Else

                    RN1 = Int(Rnd(1) * 50) + 1

                    Select Case RN1

                        Case 1: pbpString$ = "IT RATTLES IN AND OUT...NO GOOD"
                        Case 2: pbpString$ = "OFF THE IRON"
                        Case 3: pbpString$ = "IT SPINS IN AND OUT"
                        Case 4: pbpString$ = "IT'S OFF THE GLASS"
                        Case 5: pbpString$ = "IT'S NO GOOD"
                        Case 6: pbpString$ = "IT'S OFF THE MARK"
                        Case 7: pbpString$ = "IT'S A BRICK"
                        Case 8: pbpString$ = "OFF THE RIM"
                        Case 9: pbpString$ = "NOT EVEN CLOSE!"
                        Case 10: pbpString$ = "NO GOOD"
                        Case 11: pbpString$ = teamGender$(P) + " MISSES"
                        Case 12: pbpString$ = "DOESN'T GET THE ROll"
                        Case 13: pbpString$ = "OFF THE GLASS"
                        Case 14: pbpString$ = "FAlastLoseStreak SHORT"
                        Case 15: pbpString$ = "NO GOOD"
                        Case 16: pbpString$ = "NO"
                        Case 17: pbpString$ = teamGender$(P) + " MISSES"
                        Case 18: pbpString$ = "IN AND OUT"
                        Case 19: pbpString$ = "OFF THE RIM"
                        Case 20: pbpString$ = "OFF THE IRON"
                        Case 21: pbpString$ = "IT'S NOT THERE"
                        Case 22: pbpString$ = "OFF TARGET"
                        Case 23: pbpString$ = "IT RATTLES OUT"
                        Case 24: pbpString$ = "IT DOESN'T FALL"
                        Case 25: pbpString$ = "IT BOUNCES OFF"
                        Case 26: pbpString$ = "IT ROlastLoseStreak OFF"
                        Case 27: pbpString$ = "SHORT"
                        Case 28: pbpString$ = "IT DOESN'T GO DOWN"
                        Case 29: pbpString$ = "IT'S NO GOOD"
                        Case 30: pbpString$ = "NO GOOD"
                        Case 31: pbpString$ = teamGender$(P) + " MISSES"
                        Case 32: pbpString$ = "IT'S A BRICK"
                        Case 33: pbpString$ = teamGender$(P) + " FAILS TO CONNECT"
                        Case 34: pbpString$ = "OFF THE SIDE OF THE RIM"
                        Case 35: pbpString$ = "IT COMES UP SHORT"
                        Case 36: pbpString$ = "IT ROlastLoseStreak OUT"
                        Case 37: pbpString$ = "AROUND AND OUT"
                        Case 38: pbpString$ = "OFF THE BACK OF THE IRON"
                        Case 39: pbpString$ = "ON THE FRONT RIM...ROlastLoseStreak OFF"
                        Case 40: pbpString$ = "IN THE AIR...NO GOOD"
                        Case 41: pbpString$ = "TOO SHORT"
                        Case 42: pbpString$ = "NO GOOD"
                        Case 43: pbpString$ = "IT COMES UP SHORT"
                        Case 44: pbpString$ = "IT ROlastLoseStreak OUT"
                        Case 45: pbpString$ = "IN AND OUT"
                        Case 46: pbpString$ = "OFF THE FRONT OF THE IRON"
                        Case 47: pbpString$ = "ON THE RIM...ROlastLoseStreak OFF"
                        Case 48: pbpString$ = "OFF THE FRONT OF THE RIM"
                        Case 49: pbpString$ = "TOO SHORT"
                        Case 50: pbpString$ = "TOO STRONG"

                    End Select

                End If

            Case 9
                'NO PASS
                pbpString$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0) + " "
                RN1 = Int(Rnd(1) * 56) + 1

                Select Case RN1

                    Case 1: pbpString$ = pbpString$ + "LOOKS INSIDE"
                    Case 2: pbpString$ = pbpString$ + "TRIES TO WORK FREE"
                    Case 3: pbpString$ = pbpString$ + "SETS UP THE PLAY"
                    Case 4: pbpString$ = pbpString$ + "LOOKS FOR AN OPENING"
                    Case 5:
                        If teamGender$(P) = "SHE" Then
                            pbpString$ = pbpString$ + "DRIBBLES AROUND HER MAN"
                        Else
                            pbpString$ = pbpString$ + "DRIBBLES AROUND HIS MAN"
                        End If

                    Case 6: pbpString$ = pbpString$ + "SIGNALS THE PLAY"
                    Case 7: pbpString$ = pbpString$ + "IS IN CONTROL"
                    Case 8: pbpString$ = pbpString$ + "IS IN POSSESSION"
                    Case 9: pbpString$ = pbpString$ + "TRIES TO GET OPEN"
                    Case 10: pbpString$ = pbpString$ + "YO-YO'S OUT TOP"
                    Case 11: pbpString$ = pbpString$ + "GOES TO WORK"
                    Case 12: pbpString$ = pbpString$ + "WORKS OFF THE PICK"
                    Case 13: pbpString$ = pbpString$ + "ON THE DRIBBLE"
                    Case 14: pbpString$ = pbpString$ + "CUTS TO THE LEFT"
                    Case 15: pbpString$ = pbpString$ + "CUTS TO THE RIGHT"
                    Case 16: pbpString$ = pbpString$ + "WHEELS AND DEALS"
                    Case 17: pbpString$ = pbpString$ + "SETS UP ON TOP"
                    Case 18: pbpString$ = pbpString$ + "SLIDES RIGHT"
                    Case 19: pbpString$ = pbpString$ + "SLIDES LEFT"
                    Case 20: pbpString$ = pbpString$ + "GOES TO WORK"
                    Case 21: pbpString$ = pbpString$ + "MOVES IT LEFT-SIDE"
                    Case 22: pbpString$ = pbpString$ + "MOVES IT RIGHT-SIDE"
                    Case 23: pbpString$ = pbpString$ + "REVERSES THE DRIBBLE"
                    Case 24: pbpString$ = pbpString$ + "TRIES TO GET OPEN"
                    Case 25: pbpString$ = pbpString$ + "DRIBBLES TO THE MIDDLE"
                    Case 26: pbpString$ = pbpString$ + "FAKES THE PASS"
                    Case 27: pbpString$ = pbpString$ + "SETS UP ON TOP"
                    Case 28: pbpString$ = pbpString$ + "SLIDES RIGHT"
                    Case 29: pbpString$ = pbpString$ + "SLIDES LEFT"
                    Case 30: pbpString$ = pbpString$ + "DRIBBLES IT BEHIND THE BACK"
                    Case 31: pbpString$ = pbpString$ + "FAKES LEFT...GOES RIGHT"
                    Case 32: pbpString$ = pbpString$ + "FAKES RIGHT...GOES LEFT"
                    Case 33: pbpString$ = pbpString$ + "SPINS HARD TO THE LEFT"
                    Case 34: pbpString$ = pbpString$ + "SPINS HARD TO THE RIGHT"
                    Case 35: pbpString$ = pbpString$ + "LEANS LEFT...GOES RIGHT"
                    Case 36: pbpString$ = pbpString$ + "LEANS RIGHT...GOES LEFT"
                    Case 37: pbpString$ = pbpString$ + "LEANS LEFT...CUTS RIGHT"
                    Case 38: pbpString$ = pbpString$ + "LEANS RIGHT...CUTS LEFT"
                    Case 39: pbpString$ = pbpString$ + "DOUBLE REVERSES THE DRIBBLE"
                    Case 40: pbpString$ = pbpString$ + "BACKS IN"
                    Case 41: pbpString$ = pbpString$ + "LEANS FORWARD"
                    Case 42: pbpString$ = pbpString$ + "CROSSES OVER...SPINS"
                    Case 43: pbpString$ = pbpString$ + "CUTS LEFT"
                    Case 44: pbpString$ = pbpString$ + "CUTS RIGHT"
                    Case 45:
                        pbpString$ = pbpString$ + "WITH THE HESITATION DRIBBLE"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " SPINS LEFT"
                    Case 46:
                        pbpString$ = pbpString$ + "WITH THE HESITATION DRIBBLE"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " SPINS RIGHT"
                    Case 47:
                        pbpString$ = pbpString$ + "WITH THE HESITATION DRIBBLE"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " CUTS LEFT"
                    Case 48:
                        pbpString$ = pbpString$ + "WITH THE HESITATION DRIBBLE"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " CUTS RIGHT"
                    Case 49:
                        pbpString$ = pbpString$ + "STUTTER STEPS"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " MOVES LEFT"
                    Case 50:
                        pbpString$ = pbpString$ + "MAINTAINS A DRIBBLE"
                                                
                    Case 51: pbpString$ = pbpString$ + "FAKE SPINS LEFT...GOES RIGHT"
                    Case 52: pbpString$ = pbpString$ + "FAKE SPINS RIGHT...GOES LEFT"
                    Case 53:
                        pbpString$ = pbpString$ + "PALMS THE BALL"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " TAKES IT AND SPINS"
                    Case 54: pbpString$ = pbpString$ + "SHAKES AND BAKES"
                    Case 55: pbpString$ = pbpString$ + "DRIBBLES WITH THE LEFT HAND"
                    Case 56: pbpString$ = pbpString$ + "DRIBBLES WITH THE RIGHT HAND"

                End Select

            Case 10
                'PASS prevBall$=PREV HANDLER
                V$ = players_GAME$(P, lineupIdx(P, ballCarrier), 0)

                RN1 = Int(Rnd(1) * 60) + 1

                Select Case RN1

                    Case 1: pbpString$ = prevBall$ + " TO " + V$
                    Case 2:
                        pbpString$ = prevBall$ + " KICKS IT OUT"
                        Call PBP(P)
                        pbpString$ = V$ + " TAKES THE PASS"
                    Case 3: pbpString$ = prevBall$ + " FEEDS " + V$
                    Case 4: pbpString$ = prevBall$ + " PASSES TO " + V$
                    Case 4: pbpString$ = prevBall$ + " DISHES TO " + V$
                    Case 5: pbpString$ = prevBall$ + " GIVES IT TO " + V$
                    Case 6: pbpString$ = prevBall$ + " WITH A NICE GIVE TO " + V$
                    Case 7:
                        pbpString$ = prevBall$ + " LOOKS"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " GETS IT TO " + V$
                    Case 8: pbpString$ = prevBall$ + " SENDS IT TO " + V$
                    Case 9: pbpString$ = prevBall$ + " TO " + V$
                    Case 10: pbpString$ = prevBall$ + " GETS IT TO " + V$
                    Case 11: pbpString$ = prevBall$ + " OVER TO " + V$
                    Case 12: pbpString$ = prevBall$ + " ZIPS IT TO " + V$
                    Case 13: pbpString$ = prevBall$ + " FINDS " + V$
                    Case 14: pbpString$ = prevBall$ + " LOOKS FOR " + V$
                    Case 14: pbpString$ = prevBall$ + " THROWS IT TO " + V$
                    Case 15: pbpString$ = prevBall$ + " GOES TO " + V$
                    Case 16: pbpString$ = prevBall$ + " SETS UP " + V$
                    Case 17: pbpString$ = prevBall$ + " OUT TO " + V$
                    Case 18: pbpString$ = prevBall$ + " FLIPS IT TO " + V$
                    Case 19: pbpString$ = "BOUNCE PASS TO " + V$
                    Case 20: pbpString$ = prevBall$ + " LEAVES IT FOR " + V$
                    Case 21: pbpString$ = prevBall$ + " LOOPS IT TO " + V$
                    Case 22: pbpString$ = prevBall$ + " SHOVELS IT OVER TO " + V$
                    Case 23: pbpString$ = prevBall$ + " GIVES IT BACK TO " + V$
                    Case 24: pbpString$ = prevBall$ + " FEEDS IT TO " + V$
                    Case 25: pbpString$ = prevBall$ + " GIVES IT OFF TO " + V$
                    Case 26: pbpString$ = "PASSES TO " + V$
                    Case 27: pbpString$ = "DISHES TO " + V$
                    Case 28: pbpString$ = "GIVES TO " + V$
                    Case 29: pbpString$ = "OVER TO " + V$
                    Case 30: pbpString$ = "SENDS IT BACK TO " + V$
                    Case 31: pbpString$ = "NO-LOOK PASS TO " + V$
                    Case 32: pbpString$ = "NO-LOOK BOUNCE PASS TO " + V$
                    Case 33: pbpString$ = "BEHIND THE BACK TO " + V$
                    Case 34: pbpString$ = prevBall$ + " SWINGS IT TO " + V$
                    Case 35: pbpString$ = prevBall$ + " FIRES A PASS TO " + V$
                    Case 36: pbpString$ = prevBall$ + " SNAPS A PASS TO " + V$
                    Case 37: pbpString$ = prevBall$ + " SWINGS A PASS TO " + V$
                    Case 38: pbpString$ = "TWO-HANDED PASS TO " + V$
                    Case 39: pbpString$ = "WRAP AROUND PASS TO " + V$
                    Case 40: pbpString$ = "OVER THE SHOULDER PASS TO " + V$
                    Case 41: pbpString$ = "BACKHAND PASS TO " + V$
                    Case 42: pbpString$ = "BACKHAND DISH TO " + V$
                    Case 43: pbpString$ = "PASS CONNECTED TO " + V$
                    Case 44: pbpString$ = prevBall$ + " WIRES IT TO " + V$
                    Case 45: pbpString$ = prevBall$ + " WIRES A PASS TO " + V$
                    Case 46: pbpString$ = prevBall$ + " SNAPS A PASS TO " + V$
                    Case 47: pbpString$ = prevBall$ + " FAKES..PASSES TO " + V$
                    Case 48: pbpString$ = prevBall$ + " SHUFFLES IT TO " + V$
                    Case 49: pbpString$ = prevBall$ + " SLINGS IT TO " + V$
                    Case 50: pbpString$ = prevBall$ + " FLIPS IT TO " + V$
                    Case 51: pbpString$ = prevBall$ + " WHIPS IT OVER TO " + V$
                    Case 52: pbpString$ = prevBall$ + " SLINGS A BOUNCE PASS TO " + V$
                    Case 53: pbpString$ = prevBall$ + " SHOVELS IT TO " + V$
                    Case 54:
                        pbpString$ = prevBall$ + " PUMP FAKES A SHOT"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " SWINGS IT TO " + V$
                    Case 55:
                        pbpString$ = prevBall$ + " PUMP FAKES A SHOT"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " BOUNCES IT TO " + V$
                    Case 56:
                        pbpString$ = prevBall$ + " WITH A FOOT FAKE"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " SPINS AND DUMPS IT TO " + V$
                    Case 57:
                        pbpString$ = prevBall$ + " CUTS ACROSS THE LANE"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " DISHES INSIDE TO " + V$
                    Case 58:
                        pbpString$ = prevBall$ + " DRIVES THE BASELINE"
                        Call PBP(P)
                        pbpString$ = "DISHES UNDERNEATH TO " + V$
                    Case 59:
                        pbpString$ = prevBall$ + " FAKES RIGHT...CUTS LEFT"
                        Call PBP(P)
                        pbpString$ = "SHUFFLES IT TO " + V$
                    Case 60:
                        pbpString$ = prevBall$ + " HEAD FAKES"
                        Call PBP(P)
                        pbpString$ = teamGender$(P) + " PUSHES IT TO " + V$

                End Select

        End Select

    End If

End Sub


'----------------------------------------
'           WINDEX Subroutine
'----------------------------------------
'This subroutine cleans the "window" used
'for the play-by-play text.
Sub WINDEX ()

    For I = 8 To 15
        Locate I, 1
        Call BLANKLINE
    Next I

    Locate 8, 1

End Sub


'----------------------------------------
'       BLANKLINE Subroutine
'----------------------------------------
'This subroutine literally prints blank
'spaces to clear a line of the PBP box.
Sub BLANKLINE ()

    Print Space$(38)

End Sub

'----------------------------------------
'         CLEARPBPBOX Subroutine
'----------------------------------------
'This subroutine is similar to windex,
'but only cleans the Play-by-Play area
'if there is a PBP delay. It also places
'the cursor further down when done.
Sub CLEARPBPBOX ()

    For I = 8 To 15:
        Locate I, 1
        Call BLANKLINE
    Next I

    Locate 10, 1

End Sub


'----------------------------------------
'          CLEARPBP Subroutine
'----------------------------------------
'This subroutine clears all the strings
'used for each line of PBP display
Sub CLEARPBP ()

    If pbpDelay! > 0 And pbpOpt <> 1 Then
        nbrLines = 1
        A1$ = ""
        B1$ = ""
        C1$ = ""
        D1$ = ""
        E1$ = ""
        F1$ = ""
        G1$ = ""
        H1$ = ""
    End If

End Sub


'----------------------------------------
'           TICKER Subroutine
'----------------------------------------
'This subroutine displays the score of all
'current games being tracked by the ticker.
'For each game it will find the score that
'matches the current game time / period.
Sub TICKER ()

    Shared tickerPeriod$()

    If autoPlay <> 1 Or playerMode <> 2 Then

        Color 14, 0
        Cls

        Call Rolld100(RN)

        If halfTime = 1 Then

            If G9 = 0 Then
                If RN <= 50 Then
                    Locate , 23: Print "PRUDENTIAL AT THE HALF SCOREBOARD"
                Else
                    Locate , 28: Print "RED MAN HALFTIME REPORT"
                End If
            Else
                If RN <= 50 Then
                    Locate , 29: Print "TOYOTA HALFTIME REPORT"
                Else
                    Locate , 29: Print "HONDA HALFTIME REPORT"
                End If
            End If

        Else

            If gameOver = 1 Then

                If G9 = 0 Then
                    Locate , 27: Print "CHEVROLET FINAL SCOREBOARD"
                Else
                    Select Case RN
                        Case 1 To 33: Locate , 27: Print "USA TODAY FINAL SCOREBOARD"
                        Case 34 To 67: Locate , 24: Print "FEDERAL EXPRESS FINAL SCOREBOARD"
                        Case 67 To 100: Locate , 23: Print "ESPN SPORTSCENTER FINAL SCOREBOARD"
                    End Select
                End If
            Else
                If halfTime = 0 Then Locate , 30: Print "LANCE HAFFNER GAMES SCOREBOARD"
            End If

        End If

        Color 15

        If tickerGames = 0 Then

            Locate 10, 28: Print "NO OTHER GAMES SCHEDULED"

        Else

            If quarter = 1 And gameClock! < 720 And gameClock! >= 480 Then timeInterval = 1
            If quarter = 1 And gameClock! < 480 And gameClock! >= 240 Then timeInterval = 2
            If quarter = 1 And gameClock! < 240 Then timeInterval = 3

            If quarter = 2 And gameClock! < 720 And gameClock! >= 480 Then timeInterval = 4
            If quarter = 2 And gameClock! < 480 And gameClock! >= 240 Then timeInterval = 5
            If quarter = 2 And gameClock! < 240 Then timeInterval = 6

            If quarter = 3 And gameClock! < 720 And gameClock! >= 480 Then timeInterval = 9
            If quarter = 3 And gameClock! < 480 And gameClock! >= 240 Then timeInterval = 10
            If quarter = 3 And gameClock! < 240 Then timeInterval = 11

            If quarter = 4 And gameClock! < 720 And gameClock! >= 480 Then timeInterval = 12
            If quarter = 4 And gameClock! < 480 And gameClock! >= 240 Then timeInterval = 13
            If quarter = 4 And gameClock! < 240 Then timeInterval = 14

            If quarter = 5 And gameClock! < 720 And gameClock! >= 480 Then timeInterval = 15
            If quarter = 5 And gameClock! < 480 And gameClock! >= 240 Then timeInterval = 16
            If quarter = 5 And gameClock! < 240 Then timeInterval = 17

            If oppScore = 1 Then timeInterval = 8

            If Left$(SX$(1, 0), 3) <> "XXX" Then
                Locate 2, 1
                X = 1
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(1, 0), 13);
                Locate , 15: Print Using "### "; SX%(1, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(1, 1), 13);
                Locate , 15: Print Using "### #"; SX%(1, 1, tickInterval)
            End If

            If Left$(SX$(2, 0), 3) <> "XXX" Then
                Locate 5, 1
                X = 2
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(2, 0), 13);
                Locate , 15: Print Using "### "; SX%(2, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(2, 1), 13);
                Locate , 15: Print Using "### #"; SX%(2, 1, tickInterval)
            End If

            If Left$(SX$(3, 0), 3) <> "XXX" Then
                Locate 8, 1
                X = 3
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(3, 0), 13);
                Locate , 15: Print Using "### "; SX%(3, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(3, 1), 13);
                Locate , 15: Print Using "### #"; SX%(3, 1, tickInterval)
            End If

            If Left$(SX$(4, 0), 3) <> "XXX" Then
                Locate 11, 1
                X = 4
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(4, 0), 13);
                Locate , 15: Print Using "### "; SX%(4, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(4, 1), 13);
                Locate , 15: Print Using "### #"; SX%(4, 1, tickInterval)
            End If

            If Left$(SX$(5, 0), 3) <> "XXX" Then
                Locate 14, 1
                X = 5
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(5, 0), 13);
                Locate , 15: Print Using "### "; SX%(5, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(5, 1), 13);
                Locate , 15: Print Using "### #"; SX%(5, 1, tickInterval)
            End If

            If Left$(SX$(6, 0), 3) <> "XXX" Then
                Locate 17, 1
                X = 6
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(6, 0), 13);
                Locate , 15: Print Using "### "; SX%(6, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(6, 1), 13);
                Locate , 15: Print Using "### #"; SX%(6, 1, tickInterval)
            End If

            If Left$(SX$(7, 0), 3) <> "XXX" Then
                Locate 20, 1
                X = 7
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(7, 0), 13);
                Locate , 15: Print Using "### "; SX%(7, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(7, 1), 13);
                Locate , 15: Print Using "### #"; SX%(7, 1, tickInterval)
            End If

            If Left$(SX$(8, 0), 3) <> "XXX" Then
                Locate 23, 1
                X = 8
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(8, 0), 13);
                Locate , 15: Print Using "### "; SX%(8, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(8, 1), 13);
                Locate , 15: Print Using "### #"; SX%(8, 1, tickInterval);
            End If

            If Left$(SX$(9, 0), 3) <> "XXX" Then
                Locate 2, 21
                X = 9
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(9, 0), 13);
                Locate , 35: Print Using "### "; SX%(9, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(9, 1), 13);
                Locate , 35: Print Using "### #"; SX%(9, 1, tickInterval)
            End If

            If Left$(SX$(10, 0), 3) <> "XXX" Then
                Locate 5, 21
                X = 10
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(10, 0), 13);
                Locate , 35: Print Using "### "; SX%(10, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(10, 1), 13);
                Locate , 35: Print Using "### #"; SX%(10, 1, tickInterval)
            End If

            If Left$(SX$(11, 0), 3) <> "XXX" Then
                Locate 8, 21
                X = 11
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(11, 0), 13);
                Locate , 35: Print Using "### "; SX%(11, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(11, 1), 13);
                Locate , 35: Print Using "### #"; SX%(11, 1, tickInterval)
            End If

            If Left$(SX$(12, 0), 3) <> "XXX" Then
                Locate 11, 21
                X = 12
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(12, 0), 13);
                Locate , 35: Print Using "### "; SX%(12, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(12, 1), 13);
                Locate , 35: Print Using "### #"; SX%(12, 1, tickInterval)
            End If

            If Left$(SX$(13, 0), 3) <> "XXX" Then
                Locate 14, 21
                X = 13
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(13, 0), 13);
                Locate , 35: Print Using "### "; SX%(13, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(13, 1), 13);
                Locate , 35: Print Using "### #"; SX%(13, 1, tickInterval)
            End If

            If Left$(SX$(14, 0), 3) <> "XXX" Then
                Locate 17, 21
                X = 14
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(14, 0), 13);
                Locate , 35: Print Using "### "; SX%(14, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(14, 1), 13);
                Locate , 35: Print Using "### #"; SX%(14, 1, tickInterval)
            End If

            If Left$(SX$(15, 0), 3) <> "XXX" Then
                Locate 20, 21
                X = 15
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(15, 0), 13);
                Locate , 35: Print Using "### "; SX%(15, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(15, 1), 13);
                Locate , 35: Print Using "### #"; SX%(15, 1, tickInterval)
            End If

            If Left$(SX$(16, 0), 3) <> "XXX" Then
                Locate 23, 21
                X = 16
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(16, 0), 13);
                Locate , 35: Print Using "### "; SX%(16, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(16, 1), 13);
                Locate , 35: Print Using "### #"; SX%(16, 1, tickInterval);
            End If

            If Left$(SX$(17, 0), 3) <> "XXX" Then
                Locate 2, 41
                X = 17
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(17, 0), 13);
                Locate , 55: Print Using "### "; SX%(17, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(17, 1), 13);
                Locate , 55: Print Using "### #"; SX%(17, 1, tickInterval)
            End If

            If Left$(SX$(18, 0), 3) <> "XXX" Then
                Locate 5, 41
                X = 18
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(18, 0), 13);
                Locate , 55: Print Using "### "; SX%(18, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(18, 1), 13);
                Locate , 55: Print Using "### #"; SX%(18, 1, tickInterval)
            End If

            If Left$(SX$(19, 0), 3) <> "XXX" Then
                Locate 8, 41
                X = 19
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(19, 0), 13);
                Locate , 55: Print Using "### "; SX%(19, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(19, 1), 13);
                Locate , 55: Print Using "### #"; SX%(19, 1, tickInterval)
            End If

            If Left$(SX$(20, 0), 3) <> "XXX" Then
                Locate 11, 41
                X = 20
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(20, 0), 13);
                Locate , 55: Print Using "### "; SX%(20, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(20, 1), 13);
                Locate , 55: Print Using "### #"; SX%(20, 1, tickInterval)
            End If

            If Left$(SX$(21, 0), 3) <> "XXX" Then
                Locate 14, 41
                X = 21
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(21, 0), 13);
                Locate , 55: Print Using "### "; SX%(21, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41
                Print Left$(SX$(21, 1), 13);
                Locate , 55: Print Using "### #"; SX%(21, 1, tickInterval)
            End If

            If Left$(SX$(22, 0), 3) <> "XXX" Then
                Locate 17, 41
                X = 22
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(22, 0), 13);
                Locate , 55: Print Using "### "; SX%(22, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(22, 1), 13);
                Locate , 55: Print Using "### #"; SX%(22, 1, tickInterval)
            End If

            If Left$(SX$(23, 0), 3) <> "XXX" Then
                Locate 20, 41
                X = 23
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(23, 0), 13);
                Locate , 55: Print Using "### "; SX%(23, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(23, 1), 13);
                Locate , 55: Print Using "### #"; SX%(23, 1, tickInterval)
            End If

            If Left$(SX$(24, 0), 3) <> "XXX" Then
                Locate 23, 41
                X = 24
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(24, 0), 13);
                Locate , 55: Print Using "### "; SX%(24, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(24, 1), 13);
                Locate , 55: Print Using "### #"; SX%(24, 1, tickInterval);
            End If

            If Left$(SX$(25, 0), 3) <> "XXX" Then
                Locate 2, 61
                X = 25
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(25, 0), 13);
                Locate , 75: Print Using "### "; SX%(25, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(25, 1), 13);
                Locate , 75: Print Using "### #"; SX%(25, 1, tickInterval)
            End If

            If Left$(SX$(26, 0), 3) <> "XXX" Then
                Locate 5, 61
                X = 26
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(26, 0), 13);
                Locate , 75: Print Using "### "; SX%(26, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(26, 1), 13);
                Locate , 75: Print Using "### #"; SX%(26, 1, tickInterval)
            End If

            If Left$(SX$(27, 0), 3) <> "XXX" Then
                Locate 8, 61
                X = 27
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(27, 0), 13);
                Locate , 75: Print Using "### "; SX%(27, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(27, 1), 13);
                Locate , 75: Print Using "### #"; SX%(27, 1, tickInterval)
            End If

            If Left$(SX$(28, 0), 3) <> "XXX" Then
                Locate 11, 61
                X = 28
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(28, 0), 13);
                Locate , 75: Print Using "### "; SX%(28, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(28, 1), 13);
                Locate , 75: Print Using "### #"; SX%(28, 1, tickInterval)
            End If

            If Left$(SX$(29, 0), 3) <> "XXX" Then
                Locate 14, 61
                X = 29
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(29, 0), 13);
                Locate , 75: Print Using "### "; SX%(29, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(29, 1), 13);
                Locate , 75: Print Using "### #"; SX%(29, 1, tickInterval)
            End If

            If Left$(SX$(30, 0), 3) <> "XXX" Then
                Locate 17, 61
                X = 30
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(30, 0), 13);
                Locate , 75: Print Using "### "; SX%(30, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(30, 1), 13);
                Locate , 75: Print Using "### #"; SX%(30, 1, tickInterval)
            End If

            If Left$(SX$(31, 0), 3) <> "XXX" Then
                Locate 20, 61
                X = 31
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(31, 0), 13);
                Locate , 75: Print Using "### "; SX%(31, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(31, 1), 13);
                Locate , 75: Print Using "### #"; SX%(31, 1, tickInterval)
            End If

            If Left$(SX$(32, 0), 3) <> "XXX" Then
                Locate 23, 61
                X = 32
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(32, 0), 13);
                Locate , 75: Print Using "### "; SX%(32, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(32, 1), 13);
                Locate , 75: Print Using "### #"; SX%(32, 1, tickInterval);
            End If

        End If

        I$ = GetKeyPress$

        Call SCOREBOARD

    End If

End Sub


'----------------------------------------
'          TickerTime Function
'----------------------------------------
'This subroutine determines which of the
'pre-determined time intervals the ticker
'display should be using.
Function TickerTime% (timeInterval, X)

    Shared tickerStart

    tickerIdx = timeInterval + (tickerStart - ST%(X)) * 3

    If tickerIdx < 0 Then tickerIdx = 0
    If tickerIdx > 14 Then tickerIdx = 14
    If gameOver = 1 Then tickerIdx = 14

    TickerTime% = tickerIdx

End Function


'----------------------------------------
'        TICKERUPDATE Subroutine
'----------------------------------------
'This subroutine calculates the updated
'scores for the ticker games.
Sub TICKERUPDATE ()

    If autoPlay = 1 And schedGame(2) <= 31 Then

        If quarter = 1 And gameClock! <= 480 And SX%(1 + schedGame(2), 0, 1) = -1 Then SX%(1 + schedGame(2), 0, 1) = score(0, 0): SX%(1 + schedGame(2), 1, 1) = score(1, 0): GoTo 2580
        If quarter = 1 And gameClock! <= 240 And SX%(1 + schedGame(2), 0, 2) = -1 Then SX%(1 + schedGame(2), 0, 2) = score(0, 0): SX%(1 + schedGame(2), 1, 2) = score(1, 0): GoTo 2580

        If quarter > 1 And SX%(1 + schedGame(2), 0, 3) = -1 Then SX%(1 + schedGame(2), 0, 3) = score(0, 1): SX%(1 + schedGame(2), 1, 3) = score(1, 1): GoTo 2580

        If quarter = 2 And gameClock! <= 480 And SX%(1 + schedGame(2), 0, 4) = -1 Then SX%(1 + schedGame(2), 0, 4) = score(0, 0): SX%(1 + schedGame(2), 1, 4) = score(1, 0): GoTo 2580
        If quarter = 2 And gameClock! <= 240 And SX%(1 + schedGame(2), 0, 5) = -1 Then SX%(1 + schedGame(2), 0, 5) = score(0, 0): SX%(1 + schedGame(2), 1, 5) = score(1, 0): GoTo 2580

        If quarter > 2 And SX%(1 + schedGame(2), 0, 6) = -1 Then For I2 = 6 To 8: SX%(1 + schedGame(2), 0, I2) = score(0, 2) + score(0, 1): SX%(1 + schedGame(2), 1, I2) = score(1, 2) + score(1, 1): Next: GoTo 2580

        If quarter = 3 And gameClock! <= 480 And SX%(1 + schedGame(2), 0, 9) = -1 Then SX%(1 + schedGame(2), 0, 9) = score(0, 0): SX%(1 + schedGame(2), 1, 9) = score(1, 0): GoTo 2580
        If quarter = 3 And gameClock! <= 240 And SX%(1 + schedGame(2), 0, 10) = -1 Then SX%(1 + schedGame(2), 0, 10) = score(0, 0): SX%(1 + schedGame(2), 1, 10) = score(1, 0): GoTo 2580

        If quarter > 3 And SX%(1 + schedGame(2), 0, 11) = -1 Then SX%(1 + schedGame(2), 0, 11) = score(0, 3) + score(0, 2) + score(0, 1): SX%(1 + schedGame(2), 1, 11) = score(1, 3) + score(1, 2) + score(1, 1): GoTo 2580

        If quarter = 4 And gameClock! <= 480 And SX%(1 + schedGame(2), 0, 12) = -1 Then SX%(1 + schedGame(2), 0, 12) = score(0, 0): SX%(1 + schedGame(2), 1, 12) = score(1, 0): GoTo 2580
        If quarter = 4 And gameClock! <= 240 And SX%(1 + schedGame(2), 0, 13) = -1 Then SX%(1 + schedGame(2), 0, 13) = score(0, 0): SX%(1 + schedGame(2), 1, 13) = score(1, 0): GoTo 2580

        SX%(1 + schedGame(2), 0, 14) = score(0, 0): SX%(1 + schedGame(2), 1, 14) = score(1, 0)

    End If

    2580 '

End Sub


'----------------------------------------
'       TickerSchedule Subroutine
'----------------------------------------
'This subroutine reads in the scores from
'the ticker and uses them to update the
'corresponding schedule file.
Sub TickerSchedule

    Shared scheduleFile$

    If schedGame(2) <= 31 Then

        For X = 0 To 1:
            SX$(schedGame(2) + 1, X) = gameTeams$(X)
        Next

        Open "TICKER" For Output As #1

        For I = 1 To 32:
            For I1 = 0 To 1:
                For I2 = 1 To 14
                    Print #1, SX%(I, I1, I2)
                Next
            Next
        Next

        For I = 1 To 32:
            For I1 = 0 To 1:
                Print #1, SX$(I, I1)
            Next
        Next

        For I = 1 To 32:
            Print #1, ST%(I)
        Next

        Close #1

    End If

    Open diskPaths$(3) + scheduleFile$ For Random As #2 Len = SCHEDULE_SIZE_BYTES

    For X = 0 To 21
        Field #2, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
    Next

    Field #2, 44 As X$, 2 As Q$(25), 2 As Q$(26), 5 As Q$(23), 5 As Q$(24)

    schedOptions(13) = 1

    For X = 0 To 21:
        LSet Q$(X + 1) = MKI$(schedOptions(X))
    Next

    For X = 0 To 1:
        LSet Q$(23 + X) = diskIDs$(X)
    Next

    LSet Q$(25) = MKI$(score(0, 0))
    LSet Q$(26) = MKI$(score(1, 0))

    Put #2, schedGame(0) + schedGame(2)

    Close #2

    schedGame(2) = schedGame(2) + 1
    autoPlay = 1

End Sub


'----------------------------------------
'          PlayStyle Subroutine
'----------------------------------------
'This subroutine walks the user through
'the selection of play styles for their
'team on both offense and defense.
Sub PlayStyle ()

    Shared yesNoText$()

    Call LINEUPS

    Locate 19, 1
    Color 14, 0
    Print "STARTING LINEUP"

    Color 14
    Locate 19, 29: Print "   SELECT OFFENSIVE AND DEFENSIVE STYLE"
    Locate 21, 32: Color 15: Print "O";
    Color 3: Print "FFENSE STYLE-";
    Color 7: Print offStyleDesc$(offStyles(P9))

    Locate 22, 32: Color 3: Print "DE";
    Color 15: Print "F";
    Color 3: Print "ENSE STYLE-";
    Color 7: Print defStyleDesc$(defStyles(P9))

    Locate 23, 32: Color 3: Print "FUll COURT ";
    Color 15: Print "P";
    Color 3: Print "RESS-";
    Color 7: Print yesNoText$(fullCtOpt(P9))

    If C1 = 1 Then

        For M = 0 To 4

            Locate M + 20, 1
            Color 2, 0
            Print Using "# "; M;
            Color 15, 0
            Print "ENTER "; posnName$(M); " # ";

            Z = lineupIdx(P9, M)
            CV% = 0

            For IP% = 0 To Z
                '-- College BB also checks for player Name
                '   ... Or players_GAME$(P9, IP%, 0) = "" Or players_GAME$(P9, IP%, 0) = "XXX"
                If plyrRat_GAME(P9, IP%, 18) <> 0 Then CV% = CV% + 1
            Next

            Print players_GAME$(P9, Z, 0);
            Locate Z + 3 - CV%, 3
            Color teamRat_GAME(P9, 33), teamRat_GAME(P9, 34)
            Print Left$(players_GAME$(P9, Z, 0), 12)
            Color 15, 0

        Next

    Else

        For I = 0 To 4

            Do
                Locate I + 20, 1
                Color 2, 0: Print Using "# "; I;
                Color 15: Print "ENTER "; posnName$(I); " # ";

                I$ = GetKeyPress$
                Call LetterToNumber(I$, J)

                If UCase$(I$) = "O" Or UCase$(I$) = "P" Then
                    J = 15
                End If

                If J = 15 Then

                    If UCase$(I$) = "P" Then
                        fullCtOpt(P9) = 1 - fullCtOpt(P9)
                    End If

                    If UCase$(I$) = "F" Then

                        If teamYears(eraChoice) >= 2001 Then
                            'Zone options available
                            maxChoice = 9
                        Else
                            maxChoice = 5
                        End If

                        defStyles(P9) = defStyles(P9) + 1
                        If defStyles(P9) > maxChoice Then defStyles(P9) = 1

                    End If

                    If UCase$(I$) = "O" Then
                        offStyles(P9) = offStyles(P9) + 1
                        If offStyles(P9) > 2 Then offStyles(P9) = 0
                    End If

                    Color 14
                    Locate 19, 29: Print "   SELECT OFFENSIVE AND DEFENSIVE STYLE"

                    Locate 21, 32
                    Color 15: Print "O";
                    Color 3: Print "FFENSE STYLE-";
                    Color 7: Print offStyleDesc$(offStyles(P9))

                    Locate 22, 32
                    Color 3: Print "DE";
                    Color 15: Print "F";
                    Color 3: Print "ENSE STYLE-";
                    Color 7: Print defStyleDesc$(defStyles(P9))

                    Locate 23, 32
                    Color 3: Print "FULL COURT ";
                    Color 15: Print "P";
                    Color 3: Print "RESS-";
                    Color 7: Print yesNoText$(fullCtOpt(P9))

                Else

                    If J >= 0 And J <= 14 And gameRatings!(P9, J, 12) < FOULS_TO_DQ And players_GAME$(P9, J, 0) <> "XXX" And plyrRat_GAME(P9, J, 18) = 0 Then
                        For I2 = 0 To 4
                            If lineupIdx(P9, I2) = J Then Exit For
                        Next I2
                    End If

                End If

            Loop Until J >= 0 And J <= 14

            lineupIdx(P9, I) = J
            Z = J
            CV% = 0

            For IP% = 0 To Z
                '--College BB also checks for player name
                '    Or players_GAME$(P9, IP%, 0) = "" Or players_GAME$(P9, IP%, 0) = "XXX"
                If plyrRat_GAME(P9, IP%, 18) <> 0 Then
                    CV% = CV% + 1
                End If
            Next

            Print players_GAME$(P9, J, 0);
            Locate J + 3 - CV%, 3
            Color teamRat_GAME(P9, 33), teamRat_GAME(P9, 34): Print Left$(players_GAME$(P9, J, 0), 12)
            Color 15, 0

        Next

    End If

    Do

        Do
            Locate 25, 1
            Color 12, 0: Print "# TO CHANGE (0-4) 6=NONE";
            Locate 1, 1

            I$ = GetKeyPress$
            I1 = Val(I$)

            Select Case UCase$(I$)
                Case "O": I1 = 5
                Case "F": I1 = 5
                Case "P": I1 = 5
            End Select

        Loop Until I1 >= 0 And I1 <= 4 Or I1 = 6

        If I1 = 5 Then
            If UCase$(I$) = "P" Then
                fullCtOpt(P9) = 1 - fullCtOpt(P9) ': GoSub 105
            End If

            If UCase$(I$) = "F" Then
                defStyles(P9) = defStyles(P9) + 1
                If defStyles(P9) > 5 Then defStyles(P9) = 1
            End If

            If UCase$(I$) = "O" Then
                offStyles(P9) = offStyles(P9) + 1
                If offStyles(P9) > 2 Then offStyles(P9) = 0
            End If

            Color 14
            Locate 19, 29: Print "   SELECT OFFENSIVE AND DEFENSIVE STYLE"

            Locate 21, 32
            Color 15: Print "O";
            Color 3: Print "FFENSE STYLE-";
            Color 7: Print offStyleDesc$(offStyles(P9))

            Locate 22, 32
            Color 3: Print "DE";
            Color 15: Print "F";
            Color 3: Print "ENSE STYLE-";
            Color 7: Print defStyleDesc$(defStyles(P9))

            Locate 23, 32
            Color 3: Print "FUll COURT ";
            Color 15: Print "P";
            Color 3: Print "RESS-";
            Color 7: Print yesNoText$(fullCtOpt(P9))

        End If

        If I1 <= 4 Then

            If offStatus = 50 And I1 = ballCarrier And P9 = P Then

                Locate 20, 29
                Color 14, 0: Print players_GAME$(P9, lineupIdx(P9, I1), 0); " IS AT THE FT LINE"

            Else

                Locate 25, 1: Print Space$(25);

                J = lineupIdx(P9, I1)
                Z = J
                CV% = 0

                For IP% = 0 To Z:
                    '-- College BB also checks for player Name
                    '   ... Or players_GAME$(P9, IP%, 0) = "" Or players_GAME$(P9, IP%, 0) = "XXX"
                    If plyrRat_GAME(P9, IP%, 18) <> 0 Then CV% = CV% + 1
                Next

                Locate J + 3 - CV%, 3
                Color 15, 0: Print Left$(players_GAME$(P9, J, 0), 12)

                Do

                    Locate I1 + 20, 1: Print Space$(26);

                    lineupIdx(P9, I1) = -1

                    Locate , 1
                    Color 2: Print Using "# "; I1;
                    Color 15: Print "ENTER "; posnName$(I1); " # ";

                    I$ = GetKeyPress$
                    Call LetterToNumber(I$, J)

                    If UCase$(I$) = "O" Or UCase$(I$) = "P" Then
                        J = 15
                    End If

                    If J = 15 Then

                        If UCase$(I$) = "P" Then
                            fullCtOpt(P9) = 1 - fullCtOpt(P9)
                        End If

                        If UCase$(I$) = "F" Then
                            defStyles(P9) = defStyles(P9) + 1
                            If defStyles(P9) > 5 Then defStyles(P9) = 1
                        End If

                        If UCase$(I$) = "O" Then
                            offStyles(P9) = offStyles(P9) + 1
                            If offStyles(P9) > 2 Then offStyles(P9) = 0
                        End If

                    Else

                        If J >= 0 And J <= 14 And gameRatings!(P9, J, 12) < FOULS_TO_DQ And players_GAME$(P9, J, 0) <> "XXX" And plyrRat_GAME(P9, J, 18) = 0 Then
                            For I2 = 0 To 4:
                                If lineupIdx(P9, I2) = J Then Exit For
                            Next I2
                        End If

                    End If

                Loop Until J >= 0 And J <= 14

                lineupIdx(P9, I1) = J
                Z = J
                CV% = 0

                For IP% = 0 To Z
                    '-- College BB also checks for player Name
                    '   ... Or players_GAME$(P9, IP%, 0) = "" Or players_GAME$(P9, IP%, 0) = "XXX"
                    If plyrRat_GAME(P9, IP%, 18) <> 0 Then CV% = CV% + 1
                Next

                Print players_GAME$(P9, J, 0);
                Locate J + 3 - CV%, 3
                Color teamRat_GAME(P9, 33), teamRat_GAME(P9, 34): Print Left$(players_GAME$(P9, J, 0), 12)
                Color 15, 0

            End If

        End If

    Loop Until I1 = 6

    'Cls

End Sub


'----------------------------------------
'         PlayItSafe Subroutine
'----------------------------------------
'This subroutine lets the user review all
'players who are taking it safe on defense
'and determine if they should play normal.
Sub PlayItSafe ()

    skipSELECTION% = 0
    skipPlaySafe% = 0

    Cls

    A = 0
    S2 = 1
    S3 = 1
    SW = 0

    For ZZ = 0 To 4

        If gameRatings!(P9, lineupIdx(P9, ZZ), 22) = 50 Then
            A = A + 1
            If SW = 0 Then Print: Print "THE FOLLOWING PLAYER(S) ARE PLAYING SAFE BUT CAN NOW PLAY NORMAL IF YOU WISH": SW = 1
        End If

        If gameRatings!(P9, lineupIdx(P9, ZZ), 22) = 50 And SW = 1 And S2 < 6 Then
            Locate S2 + 3, 1
            Print S2; "-"; players_GAME$(P9, lineupIdx(P9, ZZ), 0)
            P5(A) = S2
            P4(A) = ZZ
            S2 = S2 + 1
            S3 = S3 + 1
        End If

    Next ZZ

    If SW <> 0 Then

        A = 1
        Print
        Print "Z - CONTINUE"

        Do

            Do

                I$ = GetKeyPress$

                If UCase$(I$) = "Z" Then
                    If MJ = 0 Then skipSELECTION% = 1
                    If MJ = 25 Then skipPlaySafe% = 1
                Else
                    playChoice = Val(I$)
                End If

            Loop Until playChoice <= 1 And playChoice <= (S2 + 1) Or UCase$(I$) = "Z"

            If skipSELECTION% <> 1 Then

                A = playChoice

                If playChoice = P5(playChoice) Then

                    gameRatings!(P9, lineupIdx(P9, P4(A)), 17) = gameRatings!(P9, lineupIdx(P9, P4(A)), 17) * 3
                    gameRatings!(P9, lineupIdx(P9, P4(A)), 22) = 0

                    Locate P5(A) + 3, 20
                    Print players_GAME$(P9, lineupIdx(P9, P4(A)), 0); " WIll NOW PLAY NORMAL DEFENSE!"
                    Call StealAdjustments

                    dFGPA(P9) = dFGPA(P9) - 1
                    A = A + 1
                    MJ = 25

                End If

            End If

        Loop Until playChoice <> P5(6) Or UCase$(I$) = "Z"

    End If

    If skipPlaySafe% <> 1 Then

        A = 0
        S2 = 1
        skipKey% = 0

        For ZZ = 0 To 4

            If (quarter <= 2 And gameRatings!(P9, lineupIdx(P9, ZZ), 12) >= FOULS_TO_DQ - 3 And gameRatings!(P9, lineupIdx(P9, ZZ), 22) = 0) Or (gameRatings!(P9, lineupIdx(P9, ZZ), 12) = FOULS_TO_DQ - 1 And gameRatings!(P9, lineupIdx(P9, ZZ), 22) = 0) Then

                A = A + 1

                If S2 = 1 Then
                    Cls
                    Print
                    Print "THE FOLLOWING PLAYER(S) CAN BE MADE TO PLAY SAFE IF YOU WISH"
                    Print
                End If

            End If

            If A = 0 And ZZ > 3 Then
                skipKey% = 1
            Else
                If S2 = A And S2 < 6 Then
                    Print S2; "- "; players_GAME$(P9, lineupIdx(P9, ZZ), 0)
                    P5(A) = S2
                    P4(A) = ZZ
                    S2 = S2 + 1
                    S3 = S3 + 1
                End If
            End If

        Next ZZ

        If skipKey% <> 1 Then

            A = 1
            Print
            Print "Z - CONTINUE"

            Do

                I$ = GetKeyPress$

                playChoice = Val(I$)

                If UCase$(I$) <> "Z" Then

                    A = playChoice

                    If playChoice = P5(playChoice) Then

                        gameRatings!(P9, lineupIdx(P9, P4(A)), 17) = Int((gameRatings!(P9, lineupIdx(P9, P4(A)), 17) + 1) / 3)
                        gameRatings!(P9, lineupIdx(P9, P4(A)), 22) = 50

                        Locate P5(A) + 3, 20
                        Print players_GAME$(P9, lineupIdx(P9, P4(A)), 0); " WIll NOW PLAY SAFE!"
                        Call StealAdjustments

                        dFGPA(P9) = dFGPA(P9) + 1
                        A = A + 1
                        S3 = S3 + 1

                    End If

                End If

            Loop Until playChoice >= 1 And playChoice <= (S2 + 1) Or UCase$(I$) = "Z"

        End If

    End If

    'Cls
    MJ = 0
    FO = 99

    If quarter = 1 And ((quarterOpt = 0 And gameClock! = 720) Or (quarterOpt = 0 And gameClock! = 600)) Then FO = 0

End Sub


'----------------------------------------
'          FOULHARD Subroutine
'----------------------------------------
' ...explanation...
Sub FOULHARD

    For I = 0 To 1

        For I1 = 0 To 14

            If gameRatings!(I, I1, 22) = 50 Then
                gameRatings!(I, I1, 22) = 0
                gameRatings!(I, I1, 17) = gameRatings!(I, I1, 17) * 3
                dFGPA(I) = dFGPA(I) - 1
                MJ = 50
            End If

        Next I1

        If MJ = 50 Then

            MJ = 0
            miscAdj(I, 0) = 0

            For I1 = 0 To 4:
                'Sum of all player foul ratings
                miscAdj(I, 0) = miscAdj(I, 0) + gameRatings!(I, lineupIdx(I, I1), 17)
            Next I1

        End If

    Next I

End Sub


'----------------------------------------
'         WHOGESTBALL Subroutine
'----------------------------------------
'Determine ball carrier for the computer
'control when a pass is happening
Sub WHOGETSBALL

    CNT% = 0

    While (CNT% < 4)

        CNT% = CNT% + 1

        If CNT% <> 4 Then

            M9 = 0
            ballCarrier = Int(Rnd(1) * 5)
            Call Rolld100(RN0)

            If RN0 > 30 Then

                For I = 0 To 4

                    M8 = CInt(plyrRat_GAME(P, lineupIdx(P, I), 2) / plyrRat_GAME(P, lineupIdx(P, I), 0)) - gameRatings!(P, lineupIdx(P, I), 7)

                    If M8 <= 0 Then
                        M8 = 0
                        If playerFatigue%(P, lineupIdx(P, I)) > 0 Then
                            M8 = playerFatigue%(P, lineupIdx(P, I))
                        End If
                    End If

                    If M8 >= M9 Then
                        ballCarrier = I
                        M9 = M8
                    End If

                Next I

            End If

        End If

    Wend

End Sub


'----------------------------------------
'        GetPassChoice Subroutine
'----------------------------------------
'This routine prompts the user for a
'player to pass to.
Sub GetPassChoice ()

    Do

        If autoPlay <> 1 Then

            If pbpOpt = 1 Then
                If (playerMode <> 2 And pbpDelay! > 0) Then
                    Call WINDEX
                    Print "PASS TO CHOICE (0-4)"
                    Call PBP(P)
                End If
            Else
                pbpString$ = "PASS TO CHOICE (0-4)"
                Call PBP(P)
            End If

        End If

        Do

            I$ = GetKeyPress$

            Select Case UCase$(I$)

                Case "S"
                    Call SCOUTREPORT

                Case "T"
                    Call TICKER

                Case "Q"
                    Exit Sub

                Case "O"
                    Call OPTIONS

                Case "Z", "/"
                    Call CHANGESTRATEGY(I$)

                Case Else
                    ballCarrier = Val(I$)

            End Select

        Loop Until ballCarrier >= 0 And ballCarrier <= 4

    Loop Until ballCarrier >= 0 And ballCarrier <= 4

End Sub


'----------------------------------------
'         COMPUTERSUBS Subroutine
'----------------------------------------
'This subroutine does some kind of changes
'to players related to substitutions.
Sub COMPUTERSUBS ()

    fatiguePlayers = 0
    disabledPlayers = 0
    tiredPlayers = 0
    foulPlayers = 0

    'This translates close to, but not exactly:
    'If human player on offense and offStatus = 50
    If Not (offStatus = 50 And (playerMode = 1 And P9 = compTeam) Or (playerMode = 2 And P9 = P)) Then

        For I1 = 0 To 14

            M6 = playerFatigue%(P9, I1)

            If quarter < 4 And M6 <= 0 Or quarter = 4 And gameClock! > 120 And M6 <= 0 Then
                fatiguePlayers = fatiguePlayers + 1
            End If

            'Disabled player or inactive spot
            'Based on roster name, disable status or # of fouls so far
            'plyrRat_GAME(P9, I1, 18) = Roster Status (1 = Disabled)

            If goodPlayer%(P9, I1, 0) = 0 Then
                '(plyrRat_GAME(P9, I1, 18) <> 0 Or players_GAME$(P9, I1, 0) = "XXX" Or gameRatings!(P9, I1, 12) >= FOULS_TO_DQ Or (gameRatings!(P9, I1, 12) >= FOULS_TO_DQ - 4 And quarter = 1) Or (gameRatings!(P9, I1, 12) >= FOULS_TO_DQ - 2 And quarter = 2) Or (gameRatings!(P9, I1, 12) >= FOULS_TO_DQ - 1 And quarter = 3) Or (gameRatings!(P9, I1, 12) = FOULS_TO_DQ - 1 And quarter = 4 And gameClock! > 300))
                disabledPlayers = disabledPlayers + 1
            End If

            If I1 <= 4 Then

                M6 = playerFatigue%(P9, I1)

                If M6 <= 0 And (quarter < 4 Or (quarter = 4 And gameClock! > 120)) Then
                    tiredPlayers = tiredPlayers + 1
                End If

                If goodPlayer%(P9, I1, 0) = 0 Then
                    'plyrRat_GAME(P9, lineupIdx(P9, I1), 18) <> 0 Or players_GAME$(P9, lineupIdx(P9, I1), 0) = "XXX" Or gameRatings!(P9, lineupIdx(P9, I1), 12) >= FOULS_TO_DQ Or (quarter = 1 And gameRatings!(P9, lineupIdx(P9, I1), 12) >= FOULS_TO_DQ - 4) Or (quarter = 2 And gameRatings!(P9, lineupIdx(P9, I1), 12) >= FOULS_TO_DQ - 2) Or (quarter = 3 And gameRatings!(P9, lineupIdx(P9, I1), 12) >= FOULS_TO_DQ - 1) Or (quarter = 4 And gameClock! > 300 And gameRatings!(P9, lineupIdx(P9, I1), 12) = FOULS_TO_DQ - 1)
                    foulPlayers = foulPlayers + 1
                End If

            End If

        Next I1

        '-- Why don't we use G9% here like in College BB?
        If (tiredPlayers >= 1 And 5 - tiredPlayers < 15 - fatiguePlayers) Or (foulPlayers >= 1 And disabledPlayers < 14) Or (quarter = 4 And gameClock! < 120) Or quarter > 4 Or computerTimeout = 1 Then
            computerTimeout = 0
            Call ComputerCoach
        End If

    End If

    If quarter = 4 And gameClock! <= 120 Then
        G9%(P9) = 1
    End If

End Sub


'----------------------------------------
'        CreditAssists Subroutine
'----------------------------------------
'This subroutine almost certainly does
'something to credit players with assists,
'but it's not clear why it needs to use
'a random number.
Sub CreditAssists ()

    Call Rolld100(RN0)

    If RN0 <= teamAdj(P, 7) Then

        iterations = 0

        Do

            skipM8% = 0

            totalAssisst = 0

            RN1 = Int(Rnd(1) * miscAdj(P, 2)) + 1

            For M8 = 0 To 4

                totalAssisst = totalAssisst + gameRatings!(P, lineupIdx(P, M8), 25)

                If RN1 <= totalAssisst Then
                    skipM8% = 1
                    Exit For
                End If

            Next M8

            If skipM8% = 0 Then M8 = Int(Rnd(1) * 5)

            totalAssisst = M8

            Locate 7, 25

            If players_GAME$(P, lineupIdx(P, ballCarrier), 0) <> players_GAME$(P, lineupIdx(P, M8), 0) Then
                gameRatings!(P, lineupIdx(P, M8), 14) = gameRatings!(P, lineupIdx(P, M8), 14) + 1
            End If

            iterations = iterations + 1

            '-- Currently I'm preventing an infinite loop by counting iterations
            '-- Infinite loops happen very rarely: about 0.5% of games. However,
            '-- regardless of what causes the issue, there's no reason it should
            '-- take more than 5 passes through this. (there are 5 ball carriers)

            '-- In the future I hope to find the root cause of the loops.
            '-- Currently I know it's because this loop condition below is not met.
            '-- That is, M8 stays as value 0 when the ball carrier is 0, so the
            '-- player names are never different.

        Loop Until players_GAME$(P, lineupIdx(P, ballCarrier), 0) <> players_GAME$(P, lineupIdx(P, M8), 0) Or iterations = 5

    End If

End Sub


'----------------------------------------
'            HORN Subroutine
'----------------------------------------
'This subroutine plays a sound on the PC
'speaker that is close to the buzzer / horn
'commonly associated with basketball.

'A replacement for the near future would be
'to load and play a sound file
Sub HORN ()

    If _FileExists("buzzer.mp3") Then
        _SndPlayFile ("buzzer.mp3")
    Else
        For PP = 1 To 70
            Sound 50, .05
            Sound 100, .05
            Sound 317, .3
        Next PP
    End If

End Sub


'----------------------------------------
'       ReboundOffense Subroutine
'----------------------------------------
'This subroutine determines something to
'do with rebounds for one team (Offense?)
Sub ReboundOffense ()

    790 '
    RB = 0

    For I = 0 To 4:

        rebRatings(I) = 0
        XZ = gameRatings!(P, lineupIdx(P, I), 21)

        If Not (quarter > 2 Or (quarter = 2 And gameClock! <= 120)) Then
            If playerFatigue%(P, lineupIdx(P, I)) <= 0 Then
                XZ = XZ * .5
            End If
        End If

        rebRatings(I) = rebRatings(I) + XZ
        RB = RB + XZ

    Next

    rndReb = Rnd(1) * RB

    For ballCarrier = 0 To 4
        If rndReb <= rebRatings(ballCarrier) Then
            skipRandom% = 1
            Exit For
        Else
            rebRatings(ballCarrier + 1) = rebRatings(ballCarrier + 1) + rebRatings(ballCarrier)
        End If
    Next

    If skipRandom% = 0 Then
        ballCarrier = Int(Rnd(1) * 5)
    End If

End Sub

'----------------------------------------
'       ReboundDefense Subroutine
'----------------------------------------
'This subroutine determines something to
'do with rebounds for one team (Defense?)
Sub ReboundDefense ()

    RB = 0

    For I = 0 To 4

        rebRatings(I) = 0
        XZ = gameRatings!(1 - P, lineupIdx(1 - P, I), 2)

        If Not (quarter > 2 Or (quarter = 2 And gameClock! <= 120)) Then
            If playerFatigue%(1 - P, lineupIdx(1 - P, I)) <= 0 Then
                XZ = XZ * .5
            End If
        End If

        rebRatings(I) = rebRatings(I) + XZ
        RB = RB + XZ

    Next

    rndReb = Rnd(1) * RB

    For ballCarrier = 0 To 4
        If rndReb <= rebRatings(ballCarrier) Then
            skipRandom% = 1
            Exit For
        Else
            rebRatings(ballCarrier + 1) = rebRatings(ballCarrier + 1) + rebRatings(ballCarrier)
        End If
    Next

    If skipRandom% = 0 Then
        ballCarrier = Int(Rnd(1) * 5)
    End If

End Sub


'----------------------------------------
'       DEADBALLOPTIONS Subroutine
'----------------------------------------
'This subroutine handles any deadball
'situations. If not computer controlled,
'the user is given a chance to handle
'any changes to lineup, strategy, players.
Sub DEADBALLOPTIONS (userTakeTO)

    userTakeTO = 0

    Call CLEARPBP

    Select Case timeoutOpt

        Case 0
            '6 minutes
            clockTime1 = 420
            '9 minutes
            clockTime2 = 600
        Case 1
            '6 minutes
            clockTime1 = 360
            '9 minutes
            clockTime2 = 540
        Case 2, 3
            '7 minutes
            clockTime1 = 420
            '3 minutes
            clockTime2 = 180

        Case 4
            '5 | 6 minutes, dep. on quarter
            If quarter = 2 Or quarter = 4 Then
                clockTime1 = 360
            Else
                clockTime1 = 300
            End If
            '3 minutes
            clockTime2 = 180

    End Select

    If (mandatoryTO(0) = 0 Or mandatoryTO(1) = 0) Then

        'Mandatory timeout # 1 - every Quarter
        If gameClock! <= clockTime1 Then

            If mandatoryTO(0) = 0 And mandatoryTO(1) = 0 Then
                'Neither team has taken timeout
                'Home Team is charged
                chargeTeam = 1
            Else
                If mandatoryTO(0) = 0 Then
                    chargeTeam = 0
                Else
                    chargeTeam = 1
                End If
            End If

            Call WINDEX

            pbpString$ = gameTeams$(chargeTeam) + " WIll BE CHARGED"
            Call SELECTPBP(chargeTeam, 0)
            pbpString$ = "WITH A MANDATORY TIMEOUT"
            Call SELECTPBP(chargeTeam, 0)

            timeouts(chargeTeam) = timeouts(chargeTeam) - 1
            mandatoryTO(chargeTeam) = 1

            Call UPDATESCREEN

        End If

        'Mandatory timeout # 2
        'Older TO options were only Q2 / Q4
        'Newer TO options are every quarter
        If gameClock! <= clockTime2 And ((timeoutOpt >= 2) Or (timeoutOpt < 2 And (quarter = 2 Or quarter = 4))) Then

            If mandatoryTO(0) = 0 Then
                chargeTeam = 0
            Else
                chargeTeam = 1
            End If

            Call WINDEX

            pbpString$ = gameTeams$(chargeTeam) + " WIll BE CHARGED"
            Call SELECTPBP(chargeTeam, 0)
            pbpString$ = "WITH A MANDATORY TIMEOUT"
            Call SELECTPBP(chargeTeam, 0)

            timeouts(chargeTeam) = timeouts(chargeTeam) - 1
            mandatoryTO(chargeTeam) = 1

            Call UPDATESCREEN

        End If

    End If

    'Substitutions
    If playerMode <> 0 Then

        If playerMode = 2 Then
            P9 = 0
            Call COMPUTERSUBS
            P9 = 1
            Call COMPUTERSUBS
        End If

        If playerMode = 1 Then
            P9 = compTeam
            Call COMPUTERSUBS
        End If

    End If

    'User options to continue play
    If playerMode = 2 Or playerMode = 1 And P = compTeam Then

        S2 = 0
        P9 = P
        C1 = 2
        Call CheckGameClock

    Else

        Call CLEARPBPBOX
        Color 14, 0
        Print "<ENTER>";
        Color 15, 0
        Print "-CONTINUE"
        Print "1-TIMEOUT "; gameTeams$(P); " -"; timeouts(P)

        Do

            I$ = GetKeyPress$
            P3 = Val(I$)

            If P3 = 1 Then

                If Not (timeouts(P) <= 0 Or P3 = 1 And P = compTeam And playerMode = 1) Then
                    userTakeTO = 1
                End If

            Else

                S2 = 0
                P9 = P
                C1 = 2
                Call CheckGameClock

            End If

        Loop Until Not ((P3 = 1 And timeouts(P) <= 0) Or (P3 = 1 And P = compTeam And playerMode = 1))

        Call WINDEX

    End If

End Sub

'----------------------------------------
'        Substitutions Subroutine
'----------------------------------------
'This subroutine seems very similar to
'"Choices", but is different in other
'ways. The names were given very much
'without knowing anything of what the
'routine was doing.
Sub Substitutions ()

    Do

        Call CLEARPBP

        'Substitutions for computer teams
        If playerMode = 2 Then
            P9 = 0
            Call COMPUTERSUBS
            P9 = 1
            Call COMPUTERSUBS
        End If

        If playerMode = 1 Then
            'Computer vs. Human
            P9 = compTeam
            Call COMPUTERSUBS
        End If

        FO = 0

        Call CLEARPBPBOX

        'Present user options
        If playerMode = 2 Then
            S2 = 0
            P9 = P
            C1 = 2
            Call CheckGameClock
            Call StealAdjustments

        Else

            'At least 1 human
            Call SCOREBOARD
            Locate 10, 1
            Color 14, 0
            Print "<ENTER>";
            Color 15, 0
            Print "-CONTINUE"
            Print "1-SUB "; gameTeams$(P)
            Print "2-SUB "; gameTeams$(D)

            Do
                I$ = GetKeyPress$
                P3 = Val(I$)
            Loop Until P3 >= 1 And P3 <= 2 Or I$ = Chr$(13)

            If P3 < 1 Or P3 > 2 Or playerMode = 1 And P3 = 1 And P = compTeam Or playerMode = 1 And P3 = 2 And D = compTeam Then

                S2 = 0
                P9 = P
                C1 = 2

                Call CheckGameClock
                Call StealAdjustments

                If LC% = 1 Then

                    LC% = 0
                    Call CheckGameClock
                    Call SCOREBOARD

                    If pbpDelay! > 0 Then
                        Call WINDEX
                    End If

                End If

            Else

                If P3 = 1 Then
                    S2 = P
                End If
                If P3 = 2 Then
                    S2 = D
                End If

                C1 = 1
                P9 = S2

                Call PlayStyle
                Call PlayItSafe

                P9 = P
                LC% = 1

            End If 'Done checking parameters

            Call WINDEX

        End If 'Done checking computer control

    Loop Until LC% = 0 Or playerMode = 2

End Sub


'----------------------------------------
'         PRESSGUARD Subroutine
'----------------------------------------
'This subroutine handles PBP and stats
'for when a team is pressing
Sub PRESSGUARD ()

    If (autoPlay = 0 Or pbpDelay! > 0) Then
        Call WINDEX
    End If

    If fullCtOpt(D) = 1 Then

        pbpString$ = gameTeams$(D) + " IS PRESSING"
        Call SELECTPBP(D, 0)

        defPress = 1

    End If

End Sub

'----------------------------------------
'      ChangePossession Subroutine
'----------------------------------------
'This subroutine handles swapping of
'variables related to which team is
'in possession of the ball
Sub ChangePossession

    prevBall$ = ""
    P = 1 - P
    D = 1 - D
    C1 = 2
    P9 = P

End Sub


'----------------------------------------
'         FoulCalled Subroutine
'----------------------------------------
'This subroutione handles calculations and
'Play-by-Play for when a player is fouled
'This will always process as the DEF team
'Finally, it handles the aftermath of a
'foul / posession change.
Sub FoulCalled (randomFouler)

    If autoPlay <> 1 Or playerMode <> 2 And whistle& Then
        _SndPlay whistle&
    End If

    If randomFouler = 1 Then

        foulPlayer = 0
        'Random # from 1 to FC rating
        M9 = Int(Rnd(1) * miscAdj(D, 0)) + 1

        'Determine fouler based on % contribution
        For M8 = 0 To 4:
            foulPlayer = foulPlayer + gameRatings!(D, lineupIdx(D, M8), 17)
            If M9 <= foulPlayer Then
                Exit For
            End If
        Next M8

        If M8 < 0 Or M8 > 4 Then
            M8 = Int(Rnd(1) * 5)
        End If

        foulPlayer = M8

    End If

    'Increase player / team fouls
    gameRatings!(D, lineupIdx(D, foulPlayer), 12) = gameRatings!(D, lineupIdx(D, foulPlayer), 12) + 1
    teamFouls(D) = teamFouls(D) + 1

    Call UPDATESCREEN

    If playerFatigue%(D, lineupIdx(D, foulPlayer)) = 0 Then
        gameRatings!(D, lineupIdx(D, foulPlayer), 3) = gameRatings!(D, lineupIdx(D, foulPlayer), 3) + 1
    End If

    If gameRatings!(D, lineupIdx(D, foulPlayer), 12) < FOULS_TO_DQ Then

        If pbpOpt = 1 And pbpDelay! > 0 Then
            Call WINDEX
            Print "FOUL ON "; players_GAME$(D, lineupIdx(D, foulPlayer), 0)
            Call DELAY
            Print teamGender$(P) + " HAS"; gameRatings!(D, lineupIdx(D, foulPlayer), 12)
            Call DELAY
        Else
            pbpString$ = "FOUL NUMBER" + Str$(gameRatings!(D, lineupIdx(D, foulPlayer), 12)) + " CALLED ON " + players_GAME$(D, lineupIdx(D, foulPlayer), 0)
            Call PBP(D)
        End If

        Call DELAY

    Else

        M9 = 0

        For I3 = 0 To 14
            If I3 <> lineupIdx(D, foulPlayer) And players_GAME$(D, I3, 1) <> "XXX" And plyrRat_GAME(D, I3, 18) = 0 Then
                M9 = M9 + 1
            End If
        Next

        For I3 = 0 To 14

            If I3 <> lineupIdx(D, foulPlayer) And players_GAME$(D, I3, 1) <> "XXX" And plyrRat_GAME(D, I3, 18) = 0 Then
                If gameRatings!(D, I3, 0) <= gameRatings!(D, lineupIdx(D, foulPlayer), 0) Then
                    gameRatings!(D, I3, 3) = gameRatings!(D, I3, 3) + CInt((gameRatings!(D, lineupIdx(D, foulPlayer), 3) / M9))
                End If
            End If

        Next I3

        If pbpOpt = 1 Then
            If pbpDelay! > 0 Then
                Call WINDEX
                Print "FOUL ON "; players_GAME$(D, lineupIdx(D, foulPlayer), 0)
                Call DELAY
            End If
        Else
            pbpString$ = "FOUL CALLED ON " + players_GAME$(D, lineupIdx(D, foulPlayer), 0)
            Call PBP(P)
        End If

        pbpString$ = teamGender$(P) + " HAS JUST FOULED OUT !!!"

        Call SELECTPBP(D, 1)
        Call DELAY

        'Disable player from roster
        plyrRat_GAME(D, lineupIdx(D, foulPlayer), 18) = -1

        If teamFouls(D) >= bonusFoulNum Then
            offStatus = 50
        End If

        P3 = 1

        If playerMode = 0 Or playerMode = 1 And D = 1 - compTeam Then

            P9 = D
            C1 = 1
            Call PlayStyle
            Call PlayItSafe

        End If

        Call Substitutions
        Call SCOREBOARD
        Call CheckGameClock

        offStatus = 0

    End If

End Sub


'----------------------------------------
'          LOADSTATS Subroutine
'----------------------------------------
'This subroutine loads team schedule stats
'data into arrays for each team from their
'corresponding stat file.
Sub LOADSTATS ()

    Dim nbrGames

    Shared teamScore(), oppScore()
    
    Shared gameAttendance&()
    
    Shared gameSite$(), locIndicator$(), oppName$()

    For I = 0 To 1

        Cls

        statFile$ = nameStatFile$(gameTeams$(I))

        If _FileExists(diskPaths$(1) + statFile$ + "S") Then

            Call ReadScheduleStats(statFile$, nbrGames)

            For I1 = 1 To nbrGames
                If teamScore(I1) > oppScore(I1) Then
                    teamRecWins(I) = teamRecWins(I) + 1
                End If
                If teamScore(I1) < oppScore(I1) Then
                    teamRecLoss(I) = teamRecLoss(I) + 1
                End If
            Next

        End If

    Next

End Sub


'----------------------------------------
'          SCOREBOARD Subroutine
'----------------------------------------
'This subroutine draws up the main screen
'for the game interface, including a frame,
'scoreboard, team options and rosters
Sub SCOREBOARD ()

    Cls

    Call TICKERUPDATE

    'Frames
    Color 9, 0

    Locate 3, 11: Print Chr$(218); String$(5, 196); Chr$(191)
    Locate , 11: Print Chr$(179);: Locate , 17: Print Chr$(179)
    Locate , 11: Print Chr$(192); String$(5, 196); Chr$(217)

    Locate 3, 58: Print Chr$(218); String$(5, 196); Chr$(191)
    Locate , 58: Print Chr$(179);: Locate , 64: Print Chr$(179)
    Locate , 58: Print Chr$(192); String$(5, 196); Chr$(217)

    'Team 1
    Locate 2, 1
    Locate , 9
    Color teamRat_GAME(0, 33), teamRat_GAME(0, 34): Print gameTeams$(0);

    'Game Clock - Minutes
    Color 15, 0
    Locate 1, 37
    Print Using "##"; Int(gameClock! / 60);
    Print ":";

    'Game Clock - Seconds
    '(Let's just round it to 2 places first)
    gcSeconds! = gameClock! - Int(gameClock! / 60) * 60
    gcSeconds! = Fix((gcSeconds! * 100 + Sgn(gcSeconds!) * .5)) / 100

    If gcSeconds! < 10 Then
        Print "0";
        Print Using "#"; gcSeconds!;
    Else
        Print Using "##"; gcSeconds!;
    End If

    'Team 2
    Locate 2, 56
    Color teamRat_GAME(1, 33), teamRat_GAME(1, 34): Print gameTeams$(1)

    'Score Team 1
    Color 14, 0
    Locate 4, 13: Print Using "###"; score(0, 0);

    'Period Designation
    Locate 2, 35
    If quarter > 4 Then
        Print Using "OVERTIME #"; quarter - 4
    Else
        Print Using " PERIOD #"; quarter;
    End If

    'Score Team 2
    Color 14, 0
    Locate 4, 60: Print Using "###"; score(1, 0)

    'Turnovers
    Color 7
    Locate 3, 35
    Print Using "##  "; turnovers(0);
    Color 12: Print "TO  ";
    Color 7
    Print Using "##"; turnovers(1)

    'Rebounds
    Locate , 35: Print Using "##  "; totRebounds(0);
    Color 12: Print "RB  ";
    Color 7: Print Using "##"; totRebounds(1)

    'Fouls
    Locate , 35: Print Using "##  "; teamFouls(0);
    Color 12: Print "TF  ";
    Color 7: Print Using "##"; teamFouls(1);

    'Bonus
    If teamFouls(0) >= bonusFoulNum And quarter <= 4 Or teamFouls(0) > 2 And quarter > 4 Then
        Color 0, 2
        Locate , 46: Print "BONUS";
        Color 7, 0
    End If

    If teamFouls(1) >= bonusFoulNum And quarter <= 4 Or teamFouls(1) > 2 And quarter > 4 Then
        Color 0, 2
        Locate , 29: Print "BONUS"
        Color 7, 0
    End If

    'Timeouts
    Locate 6, 9: Print "TIMEOUTS:"; timeouts(0);
    Locate , 56: Print "TIMEOUTS:"; timeouts(1)

    'Team strategies / decisions
    If (autoPlay = 0 Or pbpDelay! > 0) Then

        Locate 8, 58
        Color teamRat_GAME(D, 33), teamRat_GAME(D, 34)
        Print "DEF - "; defStyleDesc$(defStyles(D))
        Locate , 58
        Color teamRat_GAME(P, 33), teamRat_GAME(P, 34)
        Print "OFF - "; offStyleDesc$(offStyles(P))

        Color 15, 0

        Locate , 41
        Color 14: Print "(1) ";
        Color 15: Print "SHOOT        ";
        Color 14: Print "ENTER";
        Color 15: Print "-PASS"
        Locate , 41:
        Color 14: Print "(2) ";
        Color 15: Print gameTeams$(P);
        Print " CALL TIMEOUT      "
        Locate , 41:
        Color 14: Print "(3) ";
        Color 15: Print "COMMIT INTL FOUL (LAST 4 MIN)"
        Locate , 41
        Color 14: Print "(4) ";
        Color 15: Print "SHOOT A FORCED 3-POINTER"
        Locate , 41:
        Color 14: Print "(S) ";
        Color 15: Print "SCOUT REPORT ";
        Color 14: Print "(T) ";
        Color 15: Print "10 MIN TICKER"
        Locate , 41
        Color 14: Print "(Z) ";
        Color 15: Print "LEFT TM STGY ";
        Color 14: Print "(/) ";
        Color 15: Print "RIGHT TM STGY"

        Color 9, 0
        Locate 7, 1
        Print String$(38, 205); Chr$(203); String$(38, 205)

        Locate 8, 1
        For I = 1 To 8:
            Locate , 39: Print Chr$(186)
        Next

        'Team 1 Overview
        Locate 17, 1
        Color teamRat_GAME(0, 33), teamRat_GAME(0, 34): Print gameTeams$(0);

        'Win/Loss record from current stats files
        Print Using " [##"; teamRecWins(0);: Print "-";: Print Using "##]"; teamRecLoss(0)

        Color 14, 0
        Print "# NAME         FG FT RB CX PA DS PT F"

        Call PrintActiveLineup(0)

        'Team 2 Overview
        Locate 17, 40
        Color teamRat_GAME(1, 33), teamRat_GAME(1, 34): Print gameTeams$(1);

        'Win/Loss record from current stats files
        Print Using " [##"; teamRecWins(1);: Print "-";: Print Using "##]"; teamRecLoss(1)

        Color 14, 0
        Locate , 40: Print "# NAME          FG FT RB CX PA DS PT F"
        Call PrintActiveLineup(1)

        Color 9, 0
        For I = 1 To 9:
            Locate 14 + I, 39: Print Chr$(186)
        Next

        'Color 9, 0
        Locate 16, 1: Print String$(79, 205)
        Locate 16, 39: Print Chr$(206)
        Color 7, 0

    End If

End Sub


'----------------------------------------
'      PrintActiveLineup Subroutine
'----------------------------------------
'This subroutine displays the summary of
'the current lineup at the bottom of the
'game screen
Sub PrintActiveLineup (teamIdx)

    'Team Roster
    For I1 = 0 To 4

        If teamIdx = 1 Then Locate , 40

        '#
        Color 2: Print Using "# "; I1;

        'Player Name
        Color 15: Print Left$(players_GAME$(teamIdx, lineupIdx(teamIdx, I1), 0), 12);
        If teamIdx = 0 Then
            Locate , 16
        Else
            Locate , 56
        End If

        'Field Goals
        Print Using "##"; gameRatings!(teamIdx, lineupIdx(teamIdx, I1), 0);

        'Free Throw %
        ftPct = gameRatings!(teamIdx, lineupIdx(1, I1), 1)
        If ftPct >= 100 Then ftPct = 99
        Print Using "### "; ftPct;

        'Rebounds
        If P = teamIdx Then
            Print Using "##"; gameRatings!(teamIdx, lineupIdx(teamIdx, I1), 21) * 2 / 10;
        Else
            Print Using "##"; gameRatings!(teamIdx, lineupIdx(teamIdx, I1), 2) * 2 / 10;
        End If

        'Stamina
        J = 0
        J = playerFatigue%(teamIdx, lineupIdx(teamIdx, I1))
        If J <= 0 Then
            J = 0
            If (quarter = 1 Or quarter = 2 Or quarter = 3 Or (quarter = 4 And gameClock! > 120)) Then Color 12
        End If
        Print Using "### "; J;

        Color 15

        'Pass Rating / Steals?
        Print Using "## ##"; gameRatings!(teamIdx, lineupIdx(teamIdx, I1), 4) / 10; gameRatings!(teamIdx, lineupIdx(teamIdx, I1), 5) / 10;

        'Total Points
        Print Using "### "; gameRatings!(teamIdx, lineupIdx(teamIdx, I1), 8) * 2 + gameRatings!(teamIdx, lineupIdx(teamIdx, I1), 10) + threeFG(teamIdx, lineupIdx(teamIdx, I1), 0);

        'Fouls
        If gameRatings!(teamIdx, lineupIdx(teamIdx, I1), 12) >= (FOULS_TO_DQ - 1) Then Color 12
        Print Using "#"; gameRatings!(teamIdx, lineupIdx(teamIdx, I1), 12)

        Color 15

    Next I1

End Sub


'----------------------------------------
'         ColorChange Subroutine
'----------------------------------------
'This routine checks to see if both teams
'have conflicting primary colors.
'If so, it will substitute the secondary
'color with white/grey and reverse them.
'In essence, it's the digital equivalent
'of assigning an alternate jersey color.
Sub ColorChange ()

    If teamRat_GAME(1, 33) = 15 And teamRat_GAME(1, 34) = 4 Then
        teamRat_GAME(1, 33) = 4
        teamRat_GAME(1, 34) = 7
    End If

    If teamRat_GAME(1, 33) = 15 And teamRat_GAME(1, 34) = 1 Then
        teamRat_GAME(1, 33) = 1
        teamRat_GAME(1, 34) = 7
    End If

    If teamRat_GAME(1, 33) = 0 And teamRat_GAME(1, 34) = 4 Then
        teamRat_GAME(1, 33) = 12
        teamRat_GAME(1, 34) = 0
    End If

    If teamRat_GAME(1, 33) = 1 And teamRat_GAME(1, 34) = 7 Then
        teamRat_GAME(1, 33) = 15
        teamRat_GAME(1, 34) = 1
    End If

    If teamRat_GAME(1, 33) = 4 And teamRat_GAME(1, 34) = 7 Then
        teamRat_GAME(1, 33) = 15
        teamRat_GAME(1, 34) = 4
    End If

End Sub


'----------------------------------------
'        QUARTERTOTAL Subroutine
'----------------------------------------
'This subroutine calculates quarter totals
'for all stat categories right before the
'period end
Sub QUARTERTOTAL ()

    For I = 0 To 1

        Erase statTotals

        For I1 = 0 To 14
            statTotals(0) = statTotals(0) + gameRatings!(I, I1, 8)
            statTotals(1) = statTotals(1) + gameRatings!(I, I1, 7)
            statTotals(2) = statTotals(2) + gameRatings!(I, I1, 10)
            statTotals(3) = statTotals(3) + gameRatings!(I, I1, 9)
            statTotals(4) = statTotals(4) + gameRatings!(I, I1, 11)
            statTotals(5) = statTotals(5) + (gameRatings!(I, I1, 8) * 2) + gameRatings!(I, I1, 10) + threeFG(I, I1, 0)
            statTotals(13) = statTotals(13) + threeFG(I, I1, 0)
            statTotals(14) = statTotals(14) + threeFG(I, I1, 1)
            statTotals(6) = totalFouls(I)
            statTotals(7) = statTotals(7) + gameRatings!(I, I1, 14)
            statTotals(8) = statTotals(8) + gameRatings!(I, I1, 15)
            statTotals(9) = statTotals(9) + gameRatings!(I, I1, 13)
            statTotals(11) = statTotals(11) + gameRatings!(I, I1, 24)
            statTotals(12) = statTotals(12) + gameRatings!(I, I1, 23)
            statTotals(10) = statTotals(10) + timePlayed(I, I1)
        Next

        If quarter = 1 Or quarter = 2 Then

            For I2 = 0 To 14
                QR(I, quarter, I2) = statTotals(I2) - QR(I, quarter - 1, I2)
            Next

            For I1 = 0 To 14

                For I2 = 7 To 15
                    QQ(I, quarter, I1, I2 - 6) = gameRatings!(I, I1, I2) - QQ(I, quarter - 1, I1, I2 - 6)
                Next

                QQ(I, quarter, I1, 10) = threeFG(I, I1, 0) - QQ(I, quarter - 1, I1, 10)
                QQ(I, quarter, I1, 11) = threeFG(I, I1, 1) - QQ(I, quarter - 1, I1, 11)
                QQ(I, quarter, I1, 12) = gameRatings!(I, I1, 24) - QQ(I, quarter - 1, I1, 12)
                QQ(I, quarter, I1, 13) = gameRatings!(I, I1, 8) * 2 + gameRatings!(I, I1, 10) + threeFG(I, I1, 0) - QQ(I, quarter - 1, I1, 13)
                QQ(I, quarter, I1, 14) = gameRatings!(I, I1, 23) - QQ(I, quarter - 1, I1, 14)

            Next

        End If

        If quarter = 3 Then

            For I2 = 0 To 14
                QR(I, quarter, I2) = statTotals(I2) - QR(I, quarter - 1, I2) - QR(I, quarter - 2, I2)
            Next

            For I1 = 0 To 14

                For I2 = 7 To 15
                    QQ(I, quarter, I1, I2 - 6) = gameRatings!(I, I1, I2) - QQ(I, quarter - 1, I1, I2 - 6) - QQ(I, quarter - 2, I1, I2 - 6)
                Next

                QQ(I, quarter, I1, 10) = threeFG(I, I1, 0) - QQ(I, quarter - 1, I1, 10) - QQ(I, quarter - 2, I1, 10)
                QQ(I, quarter, I1, 11) = threeFG(I, I1, 1) - QQ(I, quarter - 1, I1, 11) - QQ(I, quarter - 2, I1, 11)
                QQ(I, quarter, I1, 12) = gameRatings!(I, I1, 24) - QQ(I, quarter - 1, I1, 12) - QQ(I, quarter - 2, I1, 12)
                QQ(I, quarter, I1, 13) = gameRatings!(I, I1, 8) * 2 + gameRatings!(I, I1, 10) + threeFG(I, I1, 0) - QQ(I, quarter - 1, I1, 13) - QQ(I, quarter - 2, I1, 13)
                QQ(I, quarter, I1, 14) = gameRatings!(I, I1, 23) - QQ(I, quarter - 1, I1, 14) - QQ(I, quarter - 2, I1, 14)

            Next

        End If

        If quarter = 4 Then

            For I2 = 0 To 14
                QR(I, quarter, I2) = statTotals(I2) - QR(I, quarter - 1, I2) - QR(I, quarter - 2, I2) - QR(I, quarter - 3, I2)
            Next

            For I1 = 0 To 14

                For I2 = 7 To 15
                    QQ(I, quarter, I1, I2 - 6) = gameRatings!(I, I1, I2) - QQ(I, quarter - 1, I1, I2 - 6) - QQ(I, quarter - 2, I1, I2 - 6) - QQ(I, quarter - 3, I1, I2 - 6)
                Next

                QQ(I, quarter, I1, 10) = threeFG(I, I1, 0) - QQ(I, quarter - 1, I1, 10) - QQ(I, quarter - 2, I1, 10) - QQ(I, quarter - 3, I1, 10)
                QQ(I, quarter, I1, 11) = threeFG(I, I1, 1) - QQ(I, quarter - 1, I1, 11) - QQ(I, quarter - 2, I1, 11) - QQ(I, quarter - 3, I1, 11)
                QQ(I, quarter, I1, 12) = gameRatings!(I, I1, 24) - QQ(I, quarter - 1, I1, 12) - QQ(I, quarter - 2, I1, 12) - QQ(I, quarter - 3, I1, 12)
                QQ(I, quarter, I1, 13) = gameRatings!(I, I1, 8) * 2 + gameRatings!(I, I1, 10) + threeFG(I, I1, 0) - QQ(I, quarter - 1, I1, 13) - QQ(I, quarter - 2, I1, 13) - QQ(I, quarter - 3, I1, 13)
                QQ(I, quarter, I1, 14) = gameRatings!(I, I1, 23) - QQ(I, quarter - 1, I1, 14) - QQ(I, quarter - 2, I1, 14) - QQ(I, quarter - 3, I1, 14)

            Next

        End If

        If quarter = 5 Then

            For I2 = 0 To 14
                QR(I, quarter, I2) = statTotals(I2) - QR(I, quarter - 1, I2) - QR(I, quarter - 2, I2) - QR(I, quarter - 3, I2) - QR(I, quarter - 4, I2)
            Next

            For I1 = 0 To 14

                For I2 = 7 To 15
                    QQ(I, quarter, I1, I2 - 6) = gameRatings!(I, I1, I2) - QQ(I, quarter - 1, I1, I2 - 6) - QQ(I, quarter - 2, I1, I2 - 6) - QQ(I, quarter - 3, I1, I2 - 6) - QQ(I, quarter - 4, I1, I2 - 6)
                Next

                QQ(I, quarter, I1, 10) = threeFG(I, I1, 0) - QQ(I, quarter - 1, I1, 10) - QQ(I, quarter - 2, I1, 10) - QQ(I, quarter - 3, I1, 10) - QQ(I, quarter - 4, I1, 10)
                QQ(I, quarter, I1, 11) = threeFG(I, I1, 1) - QQ(I, quarter - 1, I1, 11) - QQ(I, quarter - 2, I1, 11) - QQ(I, quarter - 3, I1, 11) - QQ(I, quarter - 4, I1, 11)
                QQ(I, quarter, I1, 12) = gameRatings!(I, I1, 24) - QQ(I, quarter - 1, I1, 12) - QQ(I, quarter - 2, I1, 12) - QQ(I, quarter - 3, I1, 12) - QQ(I, quarter - 4, I1, 12)
                QQ(I, quarter, I1, 13) = gameRatings!(I, I1, 8) * 2 + gameRatings!(I, I1, 10) + threeFG(I, I1, 0) - QQ(I, quarter - 1, I1, 13) - QQ(I, quarter - 2, I1, 13) - QQ(I, quarter - 3, I1, 13) - QQ(I, quarter - 4, I1, 13)
                QQ(I, quarter, I1, 14) = gameRatings!(I, I1, 23) - QQ(I, quarter - 1, I1, 14) - QQ(I, quarter - 2, I1, 14) - QQ(I, quarter - 3, I1, 14) - QQ(I, quarter - 4, I1, 14)

            Next

        End If

        If quarter = 6 Then

            For I2 = 0 To 14
                QR(I, quarter, I2) = statTotals(I2) - QR(I, quarter - 1, I2) - QR(I, quarter - 2, I2) - QR(I, quarter - 3, I2) - QR(I, quarter - 4, I2) - QR(I, quarter - 5, I2)
            Next

            For I1 = 0 To 14
                For I2 = 7 To 15
                    QQ(I, quarter, I1, I2 - 6) = gameRatings!(I, I1, I2) - QQ(I, quarter - 1, I1, I2 - 6) - QQ(I, quarter - 2, I1, I2 - 6) - QQ(I, quarter - 3, I1, I2 - 6) - QQ(I, quarter - 4, I1, I2 - 6) - QQ(I, quarter - 5, I1, I2 - 6)
                Next
                
                QQ(I, quarter, I1, 10) = threeFG(I, I1, 0) - QQ(I, quarter - 1, I1, 10) - QQ(I, quarter - 2, I1, 10) - QQ(I, quarter - 3, I1, 10) - QQ(I, quarter - 4, I1, 10) - QQ(I, quarter - 5, I1, 10)
                QQ(I, quarter, I1, 11) = threeFG(I, I1, 1) - QQ(I, quarter - 1, I1, 11) - QQ(I, quarter - 2, I1, 11) - QQ(I, quarter - 3, I1, 11) - QQ(I, quarter - 4, I1, 11) - QQ(I, quarter - 5, I1, 11)
                QQ(I, quarter, I1, 12) = gameRatings!(I, I1, 24) - QQ(I, quarter - 1, I1, 12) - QQ(I, quarter - 2, I1, 12) - QQ(I, quarter - 3, I1, 12) - QQ(I, quarter - 4, I1, 12) - QQ(I, quarter - 5, I1, 12)
                QQ(I, quarter, I1, 13) = gameRatings!(I, I1, 8) * 2 + gameRatings!(I, I1, 10) + threeFG(I, I1, 0) - QQ(I, quarter - 1, I1, 13) - QQ(I, quarter - 2, I1, 13) - QQ(I, quarter - 3, I1, 13) - QQ(I, quarter - 4, I1, 13) - QQ(I, quarter - 5, I1, 13)
                QQ(I, quarter, I1, 14) = gameRatings!(I, I1, 23) - QQ(I, quarter - 1, I1, 14) - QQ(I, quarter - 2, I1, 14) - QQ(I, quarter - 3, I1, 14) - QQ(I, quarter - 4, I1, 14) - QQ(I, quarter - 5, I1, 14)
            Next

        End If

        If quarter = 7 Then

            For I2 = 0 To 14
                QR(I, quarter, I2) = statTotals(I2) - QR(I, quarter - 1, I2) - QR(I, quarter - 2, I2) - QR(I, quarter - 3, I2) - QR(I, quarter - 4, I2) - QR(I, quarter - 5, I2) - QR(I, quarter - 6, I2)
            Next

            For I1 = 0 To 14

                For I2 = 7 To 15
                    QQ(I, quarter, I1, I2 - 6) = gameRatings!(I, I1, I2) - QQ(I, quarter - 1, I1, I2 - 6) - QQ(I, quarter - 2, I1, I2 - 6) - QQ(I, quarter - 3, I1, I2 - 6) - QQ(I, quarter - 4, I1, I2 - 6) - QQ(I, quarter - 5, I1, I2 - 6) - QQ(I, quarter - 6, I1, I2 - 6)
                Next

                QQ(I, quarter, I1, 10) = threeFG(I, I1, 0) - QQ(I, quarter - 1, I1, 10) - QQ(I, quarter - 2, I1, 10) - QQ(I, quarter - 3, I1, 10) - QQ(I, quarter - 4, I1, 10) - QQ(I, quarter - 5, I1, 10) - QQ(I, quarter - 6, I1, 10)
                QQ(I, quarter, I1, 11) = threeFG(I, I1, 1) - QQ(I, quarter - 1, I1, 11) - QQ(I, quarter - 2, I1, 11) - QQ(I, quarter - 3, I1, 11) - QQ(I, quarter - 4, I1, 11) - QQ(I, quarter - 5, I1, 11) - QQ(I, quarter - 6, I1, 11)
                QQ(I, quarter, I1, 12) = gameRatings!(I, I1, 24) - QQ(I, quarter - 1, I1, 12) - QQ(I, quarter - 2, I1, 12) - QQ(I, quarter - 3, I1, 12) - QQ(I, quarter - 4, I1, 12) - QQ(I, quarter - 5, I1, 12) - QQ(I, quarter - 6, I1, 12)
                QQ(I, quarter, I1, 13) = gameRatings!(I, I1, 8) * 2 + gameRatings!(I, I1, 10) + threeFG(I, I1, 0) - QQ(I, quarter - 1, I1, 13) - QQ(I, quarter - 2, I1, 13) - QQ(I, quarter - 3, I1, 13) - QQ(I, quarter - 4, I1, 13) - QQ(I, quarter - 5, I1, 13) - QQ(I, quarter - 6, I1, 13)
                QQ(I, quarter, I1, 14) = gameRatings!(I, I1, 23) - QQ(I, quarter - 1, I1, 14) - QQ(I, quarter - 2, I1, 14) - QQ(I, quarter - 3, I1, 14) - QQ(I, quarter - 4, I1, 14) - QQ(I, quarter - 5, I1, 14) - QQ(I, quarter - 6, I1, 14)

            Next

        End If

    Next

    Erase statTotals

End Sub


'----------------------------------------
'          STEALADJ Subroutine
'----------------------------------------
'Uses data from all player ratings to help
'calculate several team-based adjustments
'for frequency of steals, etc.
Sub StealAdjustments ()

    For I = 0 To 1

        'Player Fouls Adj's
        miscAdj(I, 0) = 0

        'Player Status Adj's
        miscAdj(I, 1) = 0

        'Player Assist Adj's
        miscAdj(I, 2) = 0
        miscAdj(I, 4) = 0

        'Player Steal Adj's
        miscAdj(I, 3) = 0

        'Process each ball carrier
        For I1 = 0 To 4
            
            miscAdj(I, 0) = miscAdj(I, 0) + gameRatings!(I, lineupIdx(I, I1), 17)
            miscAdj(I, 1) = miscAdj(I, 1) + plyrRat_GAME(I, lineupIdx(I, I1), 18)
            miscAdj(I, 2) = miscAdj(I, 2) + gameRatings!(I, lineupIdx(I, I1), 25)
            miscAdj(I, 3) = miscAdj(I, 3) + gameRatings!(I, lineupIdx(I, I1), 5)
            miscAdj(I, 4) = miscAdj(I, 4) + gameRatings!(I, lineupIdx(I, I1), 4) / 10
        Next I1

        miscAdj(I, 4) = CInt((miscAdj(I, 4) / (teamAdj(I, 6) * .9)) * 100)

    Next

End Sub


'----------------------------------------
'         SCOUTREPORT Subroutine
'----------------------------------------
'This subroutine clears the screen so it
'can print a quick scouting report
Sub SCOUTREPORT ()

    Call MenuBox(6, 8, 61, 13, 0, 7)

    Locate 7, 32

    Print "SCOUTING REPORT"
    Locate 8, 14: Color teamRat_GAME(0, 33), teamRat_GAME(0, 34): Print gameTeams$(0);
    Locate , 52: Color teamRat_GAME(1, 33), teamRat_GAME(1, 34): Print gameTeams$(1)
    Color 0, 7
    Locate , 18: Print Using "###           POINTS PER GAME         ###"; (teamRat_GAME(0, 6) * 2 + teamRat_GAME(0, 8) + teamRat_GAME(0, 18)) / teamRat_GAME(0, 0); (teamRat_GAME(1, 6) * 2 + teamRat_GAME(1, 8) + teamRat_GAME(1, 18)) / teamRat_GAME(1, 0)
    Locate , 18: Print Using " ##             FIELD GOAL %           ##"; teamRat_GAME(0, 6) / teamRat_GAME(0, 5) * 100 + dFGPA(1); teamRat_GAME(1, 6) / teamRat_GAME(1, 5) * 100 + dFGPA(0)
    Locate , 18: Print Using " ##             FREE THROW %           ##"; teamRat_GAME(0, 8) / teamRat_GAME(0, 7) * 100; teamRat_GAME(1, 8) / teamRat_GAME(1, 7) * 100
    Locate , 18: Print Using " ##          REBOUNDS PER GAME         ##"; (teamRat_GAME(0, 9) + teamRat_GAME(0, 10)) / teamRat_GAME(0, 0); (teamRat_GAME(1, 9) + teamRat_GAME(1, 10)) / teamRat_GAME(1, 0)
    Locate , 18: Print Using " ##      TURNOVERS FORCED PER GAME     ##"; teamRat_GAME(0, 29) / teamRat_GAME(0, 0); teamRat_GAME(1, 29) / teamRat_GAME(1, 0)
    Locate , 18: Print Using " ##    TURNOVERS COMMITTED PER GAME    ##"; teamRat_GAME(0, 15) / teamRat_GAME(0, 0); teamRat_GAME(1, 15) / teamRat_GAME(1, 0)
    Locate , 18: Print Using " ##          STEALS PER GAME           ##"; teamRat_GAME(0, 14) / teamRat_GAME(0, 0); teamRat_GAME(1, 14) / teamRat_GAME(1, 0)
    Locate , 18: Print Using " ##      PERSONAL FOULS PER GAME       ##"; teamRat_GAME(0, 12) / teamRat_GAME(0, 0); teamRat_GAME(1, 12) / teamRat_GAME(1, 0)
    Locate , 18: Print Using " ##          ASSISTS PER GAME          ##"; teamRat_GAME(0, 11) / teamRat_GAME(0, 0); teamRat_GAME(1, 11) / teamRat_GAME(1, 0)
    Locate , 18: Print Using " ##          BLOCKS PER GAME           ##"; teamRat_GAME(0, 16) / teamRat_GAME(0, 0); teamRat_GAME(1, 16) / teamRat_GAME(1, 0)
    Locate , 18: Print Using " ##             3 POINT FG%            ##"; teamRat_GAME(0, 18) / (teamRat_GAME(0, 17) + 1) * 100 + F%(1, 4); teamRat_GAME(1, 18) / (teamRat_GAME(1, 17) + 1) * 100 + F%(0, 4)

    I$ = GetKeyPress$

    Color 7, 0

    Call SCOREBOARD

End Sub

'----------------------------------------
'        SETUPRATINGS Subroutine
'----------------------------------------
'This subroutine lets the user choose the
'play style of teams (based on era) and
'adjust contribution ratings for players
Sub SETUPRATINGS (teamStyle)

    Dim FTF!, JX!, X3!
    Dim turnoverFreq(1)
    Dim perFoulRat(1)

    teamStyle = 0

    For I = 0 To 1

        'Adjust Free Throw Frequency
        Select Case teamYears(I)

            Case 1938 To 1954
                FTF! = .83

            Case 1954 To 1968
                FTF! = .83

            Case 1969 To 1972
                FTF! = .91

            Case 1973 To 2009
                FTF! = 1

            Case Is >= 2010
                FTF! = 1

        End Select

        For I1 = 0 To 14
            'Player FTA / FTM
            plyrRat_GAME(I, I1, 4) = plyrRat_GAME(I, I1, 4) * FTF!
            plyrRat_GAME(I, I1, 5) = plyrRat_GAME(I, I1, 5) * FTF!
        Next I1

        'Team FTM
        teamRat_GAME(I, 8) = teamRat_GAME(I, 8) * FTF!
        'Team FTA
        teamRat_GAME(I, 7) = teamRat_GAME(I, 7) * FTF!
        'Opp FTM
        teamRat_GAME(I, 22) = teamRat_GAME(I, 22) * FTF!
        'Opp FTA
        teamRat_GAME(I, 21) = teamRat_GAME(I, 21) * FTF!

        Select Case teamYears(I)

            Case Is <= 1959
                turnoverFreq(I) = 59
                perFoulRat(I) = 11

            Case 1960 To 1992
                turnoverFreq(I) = turnoverAdj(teamYears(I) - 1960)
                perFoulRat(I) = perFoulAdj(teamYears(I) - 1960) - 3

            Case Is >= 1993
                turnoverFreq(I) = 34
                perFoulRat(I) = 6

        End Select

        '-- added for testing
        '-- perhaps not by gender? but quarterOpt?
        '-- Olympic teams need tested too
        '(was a 0 value)
        If teamGender$(I) = "SHE" Then
            turnoverFreq(I) = 26
            perFoulRat(I) = 0
        End If

    Next

    'Handle game play for different eras
    If teamYears(0) <> teamYears(1) Then

        If autoPlay = 1 Then
            I = schedOptions(6)
        Else
            Color 15, 0
            Cls
            Print "THESE TWO TEAMS MAY HAVE PLAYED DURING DIFFERENT ERAS AND"
            Print "PLAYED DIFFERENT STYLES OF BASKETBALL"
            Print
            Print "PLAY STYLE OF:"
            Print
            Color 14: Print "(0) - ";
            Color teamRat_GAME(0, 33), teamRat_GAME(0, 34): Print gameTeams$(0)
            Color , 0
            Print
            Color 14: Print "(1) - ";
            Color teamRat_GAME(1, 33), teamRat_GAME(1, 34): Print gameTeams$(1)
            Color , 0
            Print
            Color 14: Print "(2) - ";: Color 3: Print "MAKE NO ADJUSTMENT"
            Color 15, 0
            Do
                I$ = GetKeyPress$
                eraAdj = Val(I$)
            Loop Until eraAdj >= 0 And eraAdj <= 2
        End If

        If eraAdj <> 2 Then

            HN% = 1
            teamStyle = I

            'Adjust FGA / G for different eras; favors the team chosen
            JX! = (teamRat_GAME(0, 5) + teamRat_GAME(0, 19) + teamRat_GAME(1, 5) + teamRat_GAME(1, 19)) * (leagRat_GAME(I, 1) / leagRat_GAME(1 - I, 1)) / ((teamRat_GAME(1, 0) + teamRat_GAME(0, 0)) * 2) * 2 - (leagRat_GAME(I, 1) + leagRat_GAME(I, 1)) / 2
            JY = JX!

        End If

    Else

        eraAdj = 3
        'Both Teams Are Same Year
        'Adjust FGA / G for both teams evenly
        JX! = (teamRat_GAME(0, 5) + teamRat_GAME(0, 19)) / ((teamRat_GAME(1, 0) + teamRat_GAME(0, 0)) * 2) + (teamRat_GAME(1, 5) + teamRat_GAME(1, 19)) / ((teamRat_GAME(1, 0) + teamRat_GAME(0, 0)) * 2)
        JY = JX! * 2 - (leagRat_GAME(0, 1) + leagRat_GAME(1, 1)) / 2

        If playoffOpt = 1 Then
            JY = JY - 7
        End If

    End If 'Done checking for different eras

    'Check if we should favor ratings (rebounds, dunks, etc.)
    'towards one team or another
    If eraAdj > 2 Then
        'No favored era: assume teams are same era
        'Use Home Team's era (in case they are not)
        eraChoice = 1
    Else
        'Favored team chosen
        eraChoice = eraAdj
    End If

    For I = 0 To 1

        '           FG % - League FG%
        dFGPA(I) = CInt(teamRat_GAME(I, 20) / teamRat_GAME(I, 19) * 100) - leagRat_GAME(I, 3)

        '1440 - ((Team FGA / G + Opp FGA / Game) * 2)
        '1440 = 24 * 60 (seconds in a half
        paceAdj(I) = 1440 - ((teamRat_GAME(I, 5) / teamRat_GAME(I, 0)) + (teamRat_GAME(I, 19) / teamRat_GAME(I, 0))) * 2
        paceAdj(I) = paceAdj(I) / 25

        '(Def TOs - Off Steals) / Game / (adjustment)
        teamAdj(I, 0) = CInt(((teamRat_GAME(I, 28) / teamRat_GAME(I, 0)) / (paceAdj(I) * 5) * 1000)) - 37
        '-- Perhaps this is more about quarterOpt???
        If teamGender$(I) = "SHE" Then
            teamAdj(I, 0) = CInt(((teamRat_GAME(I, 28) / teamRat_GAME(I, 0)) / (paceAdj(I) * 5) * 1000)) - 19
        End If

        '(Off TOs - Def Steals) / Game  / (adjustment)
        teamAdj(I, 1) = CInt(((teamRat_GAME(I, 15) - teamRat_GAME(I, 28)) / teamRat_GAME(I, 0)) / (paceAdj(I) * 5) * 1000)

        '(Def TOs - Off Steals) / Game / (adjustment) - turnoverFreq
        teamAdj(I, 2) = CInt(((teamRat_GAME(I, 29) - teamRat_GAME(I, 14)) / teamRat_GAME(I, 0)) / (paceAdj(I) * 5) * 1000) - turnoverFreq(I)

        ' ** Foul Rating **

        '(Opp FTA - Opp FGM)/10 - 7.9 * Games Played
        '-- should this be based on quarter length???
        If teamGender$(I) = "HE" Then
            X3! = (teamRat_GAME(I, 21) - teamRat_GAME(I, 20) * .1) - 7.9 * teamRat_GAME(I, 0)
        Else
            X3! = (teamRat_GAME(I, 21) - teamRat_GAME(I, 20) * .1) - 0.9 * teamRat_GAME(I, 0)
        End If

        X3! = X3! / 2

        'X3! / (Opp FGA + X3!) - PF%
        teamAdj(I, 3) = CInt(X3! / (teamRat_GAME(I, 19) + X3!) * 100) - perFoulRat(I)

        ' ** Def 3FGA **
        If teamRat_GAME(I, 31) > 0 Then
            'Def 3FGA% - League 3FGA%
            teamAdj(I, 4) = CInt(teamRat_GAME(I, 32) / teamRat_GAME(I, 31) * 100) - leagRat_GAME(I, 5)
        End If

        ' ** Team 3FGA **
        If teamRat_GAME(I, 31) = 0 Then
            teamAdj(I, 4) = 0 - leagRat_GAME(I, 5)
        End If

        ' ** Team Pace **
        '(Off FGA + Def FGA) / (Games Played * 2)
        teamAdj(I, 5) = CInt(((teamRat_GAME(I, 5) + teamRat_GAME(I, 19)) / (teamRat_GAME(I, 0) * 2)))

        ' ** FGM / Game **
        teamAdj(I, 6) = CInt(teamRat_GAME(I, 6) / teamRat_GAME(I, 0))

        ' ** Off Assists / FGM **
        teamAdj(I, 7) = CInt(teamRat_GAME(I, 11) / teamRat_GAME(I, 6) * 100)

    Next

    For I = 0 To 1

        For I1 = 0 To 14

            'Active Flag = YES
            '-- don't ask me why 0 was chosen for YES
            If plyrRat_GAME(I, I1, 18) = 0 Then

                'Start assignment of player ratings
                If plyrRat_GAME(I, I1, 2) - plyrRat_GAME(I, I1, 13) > 0 Then
                    '(FGM - 3FGM) / (FGA - 3FGA), as a %
                    gameRatings!(I, I1, 0) = CInt((plyrRat_GAME(I, I1, 3) - plyrRat_GAME(I, I1, 14)) / (plyrRat_GAME(I, I1, 2) - plyrRat_GAME(I, I1, 13)) * 100)
                End If

                If playoffOpt = 1 Then
                    gameRatings!(I, I1, 0) = gameRatings!(I, I1, 0) - 2
                End If

                If plyrRat_GAME(I, I1, 4) > 0 Then
                    'FT %
                    gameRatings!(I, I1, 1) = CInt(plyrRat_GAME(I, I1, 5) / plyrRat_GAME(I, I1, 4) * 100)
                End If

                'Def Reb / Game
                X3! = plyrRat_GAME(I, I1, 7) / plyrRat_GAME(I, I1, 1) * MINS_PER_GAME

                X3! = X3! * 29.3 / (leagRat_GAME(I, 2) * .677) * ((teamRat_GAME(I, 5) + teamRat_GAME(I, 19)) / (teamRat_GAME(I, 0) * 2) / leagRat_GAME(I, 1))
                X3! = CInt(X3! * 10)

                gameRatings!(I, I1, 2) = X3!

                'Off Reb / Game
                X3! = plyrRat_GAME(I, I1, 6) / plyrRat_GAME(I, I1, 1) * MINS_PER_GAME

                'Off Reb * 14 / (Avg Reb / G * .323) * ( (Team FGA + Opp FGA) / (Games Played * 2) / Avg FGA/G )
                'Off Reb / Game * 14 Players * (Avg Games / Reb) * Expected FGA / Game)
                X3! = X3! * 14 / (leagRat_GAME(I, 2) * .323) * ((teamRat_GAME(I, 5) + teamRat_GAME(I, 19)) / (teamRat_GAME(I, 0) * 2) / leagRat_GAME(I, 1))
                X3! = CInt(X3! * 10)

                gameRatings!(I, I1, 21) = X3!

                '### ASSISTS ###
                'Player Assists / Game
                X3! = plyrRat_GAME(I, I1, 8) / plyrRat_GAME(I, I1, 1) * MINS_PER_GAME
                'Assists / 10 Mins
                gameRatings!(I, I1, 25) = CInt(X3! * 10)

                '(Player Assists / Game) * 26  * (Games Played / Team Assists)
                X3! = X3! * 26 * teamRat_GAME(I, 0) / teamRat_GAME(I, 11)

                If quarterOpt = 0 Then
                    '--- is this accurate?
                    X3! = X3! * 22 * teamRat_GAME(I, 0) / teamRat_GAME(I, 11)
                Else
                    X3! = X3! * 26 * teamRat_GAME(I, 0) / teamRat_GAME(I, 11)
                End If

                gameRatings!(I, I1, 4) = CInt(X3! * 10)

                '### STEALS ###
                'Player Steals / Game
                gameRatings!(I, I1, 5) = plyrRat_GAME(I, I1, 11) / plyrRat_GAME(I, I1, 1) * MINS_PER_GAME
                gameRatings!(I, I1, 5) = CInt(gameRatings!(I, I1, 5) / paceAdj(I) * 1000)

                '### BLOCKS ###
                'Player Blocks / Game
                gameRatings!(I, I1, 6) = plyrRat_GAME(I, I1, 12) / plyrRat_GAME(I, I1, 1) * MINS_PER_GAME

                '(Player Blocks / Game) / (((Opp FGA - Opp 3FGA - Team FGM + Team 3FGM) / Games Played) / 5)
                gameRatings!(I, I1, 6) = gameRatings!(I, I1, 6) / ((teamRat_GAME(I, 19) - teamRat_GAME(I, 31) - teamRat_GAME(I, 20) + teamRat_GAME(I, 32)) / teamRat_GAME(I, 0) / 5)
                gameRatings!(I, I1, 6) = CInt(gameRatings!(I, I1, 6) * .96 * 100)

                'Player FTA - Player FGM - 1.58 * Player Mins / Game
                Select Case ftRulesOpt
                    Case 0
                        X3! = plyrRat_GAME(I, I1, 4) - (plyrRat_GAME(I, I1, 3) * .1) - 1.58 * plyrRat_GAME(I, I1, 1) / MINS_PER_GAME
                    Case 1
                        X3! = plyrRat_GAME(I, I1, 4) * 0.9 - (plyrRat_GAME(I, I1, 3) * .1) - 1.58 * plyrRat_GAME(I, I1, 1) / MINS_PER_GAME
                End Select

                X3! = X3! / 2

                'X3! / FGA + X3!, as %
                gameRatings!(I, I1, 16) = CInt(X3! / (plyrRat_GAME(I, I1, 2) + X3!) * 100)
                If gameRatings!(I, I1, 16) < 0 Then gameRatings!(I, I1, 16) = 0

                'Player Fouls / 10 Minutes
                gameRatings!(I, I1, 17) = CInt(plyrRat_GAME(I, I1, 9) / plyrRat_GAME(I, I1, 1) * MINS_PER_GAME * 10)

                'Active Flag
                gameRatings!(I, I1, 18) = plyrRat_GAME(I, I1, 18)

                'If Player FGA > 0
                If plyrRat_GAME(I, I1, 2) > 0 Then
                    'Calculate 3FGA frequency
                    gameRatings!(I, I1, 19) = CInt((plyrRat_GAME(I, I1, 13) / plyrRat_GAME(I, I1, 2) * 100))
                End If

                'If Player 3FGA > 0
                If plyrRat_GAME(I, I1, 13) > 0 Then
                    'Calculate 3FGM %
                    gameRatings!(I, I1, 20) = CInt(plyrRat_GAME(I, I1, 14) / plyrRat_GAME(I, I1, 13) * 100)
                End If

                'Player FGA / (Player FGA + Player FTA) as %
                '-- Why is this suddenly an overflow???
                X3! = CInt(plyrRat_GAME(I, I1, 2) / (plyrRat_GAME(I, I1, 2) + plyrRat_GAME(I, I1, 4)) * 100)

                'Calculate penetration rating
                If Left$(players_GAME$(I, I1, 1), 1) <> "F" And Left$(players_GAME$(I, I1, 1), 1) <> "C" Then
                    'Guards
                    Select Case X3!

                        Case Is <= 70
                            plyrRat_GAME(I, I1, 16) = 9
                        Case 71 To 72
                            plyrRat_GAME(I, I1, 16) = 8
                        Case 73 To 74
                            plyrRat_GAME(I, I1, 16) = 7
                        Case 75 To 76
                            plyrRat_GAME(I, I1, 16) = 6
                        Case 77 To 729
                            plyrRat_GAME(I, I1, 16) = 5
                        Case 80 To 81
                            plyrRat_GAME(I, I1, 16) = 4
                        Case 82 To 83
                            plyrRat_GAME(I, I1, 16) = 3
                        Case 84 To 85
                            plyrRat_GAME(I, I1, 16) = 2
                        Case Is >= 86
                            plyrRat_GAME(I, I1, 16) = 1

                    End Select
                    
                Else
                    'Forwards / Centers
                    Select Case X3!

                        Case Is <= 65
                            plyrRat_GAME(I, I1, 16) = 9
                        Case 66 To 67
                            plyrRat_GAME(I, I1, 16) = 8
                        Case 68 To 69
                            plyrRat_GAME(I, I1, 16) = 7
                        Case 70 To 71
                            plyrRat_GAME(I, I1, 16) = 6
                        Case 72 To 74
                            plyrRat_GAME(I, I1, 16) = 5
                        Case 75 To 76
                            plyrRat_GAME(I, I1, 16) = 4
                        Case 77 To 78
                            plyrRat_GAME(I, I1, 16) = 3
                        Case 79 To 80
                            plyrRat_GAME(I, I1, 16) = 2
                        Case Is >= 81
                            plyrRat_GAME(I, I1, 16) = 1
                    
                    End Select

                End If

            End If

        Next I1

    Next I

    For I = 0 To 1
        'Tm FGA + Opp FGA + Off Reb + Team Reb / (Games Played * 2)
        rebAdj!(I) = (teamRat_GAME(I, 5) + teamRat_GAME(I, 19) + teamRat_GAME(I, 9) + teamRat_GAME(I, 10)) / (teamRat_GAME(I, 0) * 2)
        'ADD Opp Off Reb + Opp Def Reb + Team Fouls + Opp Fouls / (Games Played * 2)
        rebAdj!(I) = rebAdj!(I) + (teamRat_GAME(I, 23) + teamRat_GAME(I, 24) + teamRat_GAME(I, 12) + teamRat_GAME(I, 26)) / (teamRat_GAME(I, 0) * 2)
        '               * 2 - (League FGA/G + League Reb/G + League PF/G)
        rebAdj!(I) = rebAdj!(I) * 2 - (leagRat_GAME(I, 1) + leagRat_GAME(I, 2) + leagRat_GAME(I, 4))
    Next I

    If HN% = 1 Then

        I = teamStyle

        teamAdj(1 - I, 1) = teamAdj(1 - I, 1) * turnoverFreq(I) / turnoverFreq(1 - I)
        rebAdj!(1 - I) = rebAdj!(1 - I) * (leagRat_GAME(I, 1) + leagRat_GAME(I, 2) + leagRat_GAME(I, 4)) / (leagRat_GAME(1 - I, 1) + leagRat_GAME(1 - I, 2) + leagRat_GAME(1 - I, 4))

        For I1 = 0 To 14
            'ADD League FG% (for both teams) - League Power Adj (for both teams)
            gameRatings!(1 - I, I1, 0) = gameRatings!(1 - I, I1, 0) + leagRat_GAME(I, 3) - leagRat_GAME(1 - I, 3) - leagRat_GAME(1 - I, 6) + leagRat_GAME(I, 6)
        Next

    End If

    S9 = CInt((rebAdj!(0) + rebAdj!(1)) / 2)

    If playoffOpt = 1 Then
        S9 = S9 - 11
    End If

    Erase rebAdj!

    For I = 0 To 1:
        For I1 = 0 To 14
            If plyrRat_GAME(I, I1, 18) = 0 Then
                'ADD Player FGA + Player Off Reb + Player Def Reb + Player Fouls / Player Minutes
                rebAdj!(I) = rebAdj!(I) + CInt((plyrRat_GAME(I, I1, 2) + plyrRat_GAME(I, I1, 6) + plyrRat_GAME(I, I1, 7) + plyrRat_GAME(I, I1, 9)) / plyrRat_GAME(I, I1, 0))
            End If
        Next I1
    Next I

    For I = 0 To 1
        For I1 = 0 To 14
            If plyrRat_GAME(I, I1, 18) = 0 Then
                'Player FGA + Player Off Reb + Player Def RB + Player Fouls / (Minutes / (M9(I) * S9))
                gameRatings!(I, I1, 3) = CInt(((plyrRat_GAME(I, I1, 2) + plyrRat_GAME(I, I1, 6) + plyrRat_GAME(I, I1, 7) + plyrRat_GAME(I, I1, 9)) / plyrRat_GAME(I, I1, 0)) / rebAdj!(I) * S9)
                If gameRatings!(I, I1, 3) = 0 Then gameRatings!(I, I1, 3) = 1
            End If
        Next I1
    Next I

    X3! = 0

    If sClockVal = 2 Then
        'Fouls on rebound were more common
        'in the no-shot-clock era
        rebFoulChance = 25
    Else

        'Favor modern outcomes if teams are
        'from different eras
        Select Case teamYears(eraChoice)

            Case Is < 1993
                rebFoulChance = 7

            Case 1993 To 2017
                'Personal fouls trend higher in 1993 and on
                'Jump shots became less common so Basketball
                'got more physical
                'rebFoulChance = 20
                rebFoulChance = 0

            Case Is >= 2018
                rebFoulChance = 26

        End Select

    End If

End Sub


'----------------------------------------
'        DecideShot% Function
'----------------------------------------
'This function calculates the likelihood
'of taking a shot. It is based on the
'need to pace the game for the 24-sec
'shot clock.
Function DecideShot%

    'In general, adjusting paceFactor
    'can work as follows:
    'value +10 yields -5% chance
    'this can yield 1-2 FGA/G less

    Call Rolld100(RN0)

    Select Case sClockVal

        Case 0
            '24-sec shot clock
            If teamYears(P) >= 1993 Then
                'Average - 70% chance
                paceFactor = 60
            Else
                '40; Average - 80% chance
                paceFactor = 40
            End If

        Case 1
            '30 sec shot clock
            'Average - 75% chance
            paceFactor = 65

        Case 2
            'No shot clock
            'Average - 90% chance
            'Higher chance of a good shot is needed
            'because teams are passing more often
            paceFactor = 50

    End Select

    baseValue = MINS_PER_GAME

    If quarterOpt = 1 Then
        baseValue = baseValue + 10
    End If

    'Call pbplog(8442)

    If RN0 <= baseValue + CInt(((JY - paceFactor) * .8)) Then
        DecideShot% = 1
    Else
        DecideShot% = 0
    End If

End Function


'----------------------------------------
'     ChangeContributions Subroutine
'----------------------------------------
'This subroutine lets the user adjust the
'contribution ratings for players
Sub ChangeContributions

    If autoPlay <> 1 Or playerMode <> 2 Then

        Do

            Color 15, 0
            Cls
            Print "EDIT CONTRIBUTION PERCENTAGES OF PLAYERS FROM:"
            Print
            Color 14: Print "(0) - ";: Color teamRat_GAME(0, 33), teamRat_GAME(0, 34)
            Print gameTeams$(0): Color , 0
            Print
            Color 14: Print "(1) - ";: Color teamRat_GAME(1, 33), teamRat_GAME(1, 34)
            Print gameTeams$(1)
            Color , 0
            Print
            Color 14: Print "(2) - ";: Color 3: Print "PROCEED TO GAME"
            Color 15, 0
            Print
            Print "NOTE:: THE CONTRIBUTION RATING CONSISTS OF THE PLAYER'S"
            Print "       FREQUENCY TO SHOOT, REBOUND, AND COMMIT FOULS."

            Do
                I$ = GetKeyPress$
                I = Val(I$)
            Loop Until I >= 0 And I <= 2

            If I <> 2 Then

                Cls

                TX = 0

                For I1 = 0 To 14:
                    TX = TX + gameRatings!(I, I1, 3)
                Next

                For I1 = 0 To 14:
                    gameRatings!(I, I1, 3) = gameRatings!(I, I1, 3) / TX * 100
                Next

                Do

                    Locate 2, 1
                    Print "CONTRIBUTION RATINGS FOR ";
                    Color teamRat_GAME(I, 33), teamRat_GAME(I, 34): Print gameTeams$(I)
                    Color 15, 0
                    Print
                    Print " #  PLAYER-POS         CX"
                    TY% = 0

                    For I1 = 0 To 14:

                        TY% = TY% + gameRatings!(I, I1, 3)

                        Print Using "## "; I1 + 1;
                        Print players_GAME$(I, I1, 0); "-"; players_GAME$(I, I1, 1);
                        Locate , 24: Print Using "##"; gameRatings!(I, I1, 3)

                    Next

                    Locate , 23: Print Using "###"; TY%

                    Input "SELECT PLAYER TO CHANGE (0 TO CONTINUE)"; I1
                    I1 = I1 - 1

                    If I1 <= 14 And I1 >= 0 Then
                        Locate 22, 1
                        Print "ENTER NEW CX RATING FOR "; players_GAME$(I, I1, 0); " ";
                        Input gameRatings!(I, I1, 3)
                        Locate 22, 1: Print Space$(52)
                    Else

                        If TY% <> 100 Then
                            Locate 22, 1
                            Print "TOTAL MUST ADD UP TO 100%! PRESS ANY KEY"
                            I$ = GetKeyPress$
                            Locate 22, 1: Print Space$(52)
                        End If

                    End If

                Loop Until I1 < 0

                For I1 = 0 To 14:
                    gameRatings!(I, I1, 3) = gameRatings!(I, I1, 3) / 100 * TX
                Next

            End If

        Loop Until I = 2

    End If

End Sub


'----------------------------------------
'           ADJFOUL Subroutine
'----------------------------------------
'Make sure no player has any more than 5
'fouls recorded to them
Sub ADJFOUL ()

    For I = 0 To 14
        If gameRatings!(P9, I, 12) >= FOULS_TO_DQ Then
            gameRatings!(P9, I, 12) = FOULS_TO_DQ - 1
            Exit For
        End If
    Next

End Sub


'----------------------------------------
'            BADFT Subroutine
'----------------------------------------
'This subroutine handles Play-by-Play for
'missed FT's when there are 3 attempts.
Sub BADFT ()

    Select Case freeThrowNbr
        Case 1
            pbpString$ = "FIRST FREE THROW IS MISSED"
        Case 2
            pbpString$ = "SECOND FREE THROW IS MISSED"
        Case 3
            pbpString$ = "THIRD FREE THROW IS MISSED"
    End Select

    Call PBP(P)

End Sub

'----------------------------------------
'       CHANGESTRATEGY Subroutine
'----------------------------------------
'This subroutine presents the user a menu
'for changing both Def / Off strategy
Sub CHANGESTRATEGY (I$)

    Shared yesNoText$()

    If Not (UCase$(I$) = "Z" And playerMode = 1 And compTeam = 0 Or playerMode = 2) Then

        If Not (I$ = "/" And playerMode = 1 And compTeam = 1 Or playerMode = 2) Then

            teamSelected = 1

            If UCase$(I$) = "Z" Then teamSelected = 0

            Do
                Call FRAMEOPTION(8, teamSelected)
                Locate 10, 15
                Print gameTeams$(teamSelected); " SELECT STRATEGY:"

                Locate 12, 14
                Locate , 14: Print "0 - RETURN TO GAME"
                Locate , 14: Print "1 - SELECT OFFENSIVE STYLE OF PLAY"
                Locate , 14: Print "2 - SELECT DEFENSIVE STYLE OF PLAY"
                Locate , 14: Print "3 - FUll COURT PRESS "; yesNoText$(fullCtOpt(teamSelected))

                Do
                    I$ = GetKeyPress$
                    I1 = Val(I$)
                Loop Until I1 >= 0 And I1 <= 3

                Select Case I1
                    Case 1:
                        'DEFENSE
                        Call FRAMEOPTION(8, teamSelected)
                        Locate 10, 15
                        Print gameTeams$(teamSelected); " SELECT OFFENSE:"

                        Locate 12, 14
                        Locate , 14: Print "0 - NORMAL"
                        Locate , 14: Print "1 - AGGRESSIVE"
                        Locate , 14: Print "2 - SAFE"

                        Do
                            I$ = GetKeyPress$
                            I1 = Val(I$)
                        Loop Until I1 >= 0 And I1 <= 2

                        offStyles(teamSelected) = I1

                    Case 2:
                        'DEFENSE
                        Call FRAMEOPTION(13, teamSelected)
                        Locate 10, 15
                        Print gameTeams$(teamSelected); " SELECT DEFENSE:"

                        Locate 12, 14
                        Locate , 14: Print "1 - REGULAR MAN-TO-MAN"
                        Locate , 14: Print "2 - SAGGING MAN-TO-MAN"
                        Locate , 14: Print "3 - TRAPPING MAN-TO-MAN"
                        Locate , 14: Print "4 - TIGHT MAN-TO-MAN"
                        Locate , 14: Print "5 - LOOSE MAN-TO-MAN"
                        maxChoice = 5

                        If teamYears(teamSelected) >= 2001 Then
                            Locate , 14: Print "6 - 1-2-2 ZONE"
                            Locate , 14: Print "7 - 1-3-1 ZONE"
                            Locate , 14: Print "8 - 2-3 ZONE"
                            Locate , 14: Print "9 - 3-2 ZONE"
                            maxChoice = 9
                        End If

                        Do
                            I$ = GetKeyPress$
                            I1 = Val(I$)
                        Loop Until I1 >= 1 And I1 <= maxChoice

                        defStyles(teamSelected) = I1

                        Screen , , 0, 0

                    Case 3
                        fullCtOpt(teamSelected) = 1 - fullCtOpt(teamSelected)

                End Select

            Loop Until I1 = 0

        End If

    End If

    Color 7, 0
    Call SCOREBOARD

End Sub


'----------------------------------------
'      ComputerStrategy Subroutine
'----------------------------------------
'This subroutine guides the computer AI
'in how to handle it's strategy.
Sub ComputerStrategy ()

    If P9 = 0 Then
        defTeam = 1
    Else
        defTeam = 0
    End If

    ' **************************
    ' *** Defensive Strategy ***
    ' **************************

    'Where is our score compared to opponent?
    scoreDiffTm = score(P9, 0) - score(defTeam, 0)
    
    If scoreDiffTm < 0 Then
        'Current team is behind is behind

        'Where is opponent's score compared to us?
        scoreDiffOpp = score(defTeam, 0) - score(P9, 0)

        Call Rolld100(RN0)

        Select Case quarter

            Case 1, 2
                '1st Half Strategy
                fullCtOpt(P9) = 0

                If teamYears(eraChoice) < 2001 Then
                    'Zone Defense Prohibited
                    defStyles(P9) = 1
                Else

                    Select Case RN0

                        Case 1 To 75
                            defStyles(P9) = 1

                        Case 76 To 93
                            '2-3 Zone
                            defStyles(P9) = 8

                        Case 94 To 96
                            '3-2 Zone
                            defStyles(P9) = 9

                        Case 97 To 99
                            '1-2-2 Zone
                            defStyles(P9) = 6

                        Case 100
                            '1-3-1 Zone
                            defStyles(P9) = 7

                    End Select

                End If

            Case 3
                '3rd Quarter Strategy
                If teamYears(eraChoice) < 2001 Then
                    'Zone Defense Prohibited
                    defStyles(P9) = 1
                Else

                    Select Case scoreDiffOpp

                        Case Is < 10
                            'Opponent behind or within 10
                            fullCtOpt(P9) = 0

                            Select Case RN0

                                Case 1 To 80
                                    defStyles(P9) = 1

                                Case 81 To 84
                                    defStyles(P9) = 1
                                    fullCtOpt(P9) = 1

                                Case 85 To 89
                                    defStyles(P9) = 8

                                Case 90 To 96
                                    defStyles(P9) = 9

                                Case 97 To 100
                                    defStyles(P9) = 6

                            End Select

                        Case 10 To 19
                            'Opponent ahead by 10-20
                            fullCtOpt(P9) = 1

                            Select Case RN0

                                Case 1 To 71
                                    defStyles(P9) = 1

                                Case 72 To 100
                                    defStyles(P9) = 1
                                    fullCtOpt(P9) = 1

                            End Select

                        Case Is > 20
                            'Opponent ahead by 21+
                            fullCtOpt(P9) = 0

                            Select Case RN0

                                Case 1 To 88
                                    defStyles(P9) = 1

                                Case 89 To 100
                                    defStyles(P9) = 1
                                    fullCtOpt(P9) = 1

                            End Select

                    End Select

                End If

            Case 4
                '4th Quarter Strategy

                If gameClock! >= 120 Then
                    'Outside final 2 minutes

                    Select Case scoreDiffOpp

                        Case 1 To 10
                            If teamYears(eraChoice) < 2001 Then
                                'Zone Defense Prohibited
                                defStyles(P9) = 1
                                fullCtOpt(P9) = 1
                            Else

                                Select Case RN0

                                    Case 1 To 80
                                        defStyles(P9) = 1

                                    Case 81 To 95
                                        defStyles(P9) = 8

                                    Case 96 To 100
                                        defStyles(P9) = 9

                                End Select

                            End If

                        Case 11 To 12
                            If teamYears(eraChoice) < 2001 Then
                                'Zone Defense Prohibited
                                defStyles(P9) = 5
                                fullCtOpt(P9) = 0
                            Else

                                Select Case RN0

                                    Case 1 To 80
                                        defStyles(P9) = 5

                                    Case 81 To 90
                                        defStyles(P9) = 5
                                        fullCtOpt(P9) = 1
                                
                                    Case 91 To 100
                                        defStyles(P9) = 8

                                End Select
                            
                            End If

                        Case 13 To 22
                            If teamYears(eraChoice) < 2001 Then
                                'Zone Defense Prohibited
                                defStyles(P9) = 3
                                fullCtOpt(P9) = 1
                            Else

                                Select Case RN0

                                    Case 1 To 80
                                        defStyles(P9) = 3

                                    Case 81 To 90
                                        defStyles(P9) = 3
                                        fullCtOpt(P9) = 1

                                    Case 91 To 100
                                        defStyles(P9) = 8

                                End Select
                            
                            End If

                        Case Is > 22
                            If teamYears(eraChoice) < 2001 Then
                                'Zone Defense Prohibited
                                defStyles(P9) = 1
                                fullCtOpt(P9) = 0
                            Else

                                Select Case RN0
                                
                                    Case 1 To 84
                                        defStyles(P9) = 1

                                    Case 85 To 94
                                        defStyles(P9) = 1
                                        fullCtOpt = 1

                                    Case 95 To 97
                                        defStyles(P9) = 6

                                    Case 98 To 100
                                        defStyles(P9) = 7
                                
                                End Select
                            
                            End If

                    End Select

                Else
                    'Inside final 2 minutes

                    Select Case scoreDiffOpp

                        Case 1 To 4
                            If teamYears(eraChoice) < 2001 Then
                                'Zone Defense Prohibited
                                defStyles(P9) = 1
                                fullCtOpt(P9) = 1
                            Else

                                Select Case RN0

                                    Case 1 To 42
                                        defStyles(P9) = 1
                                        fullCtOpt(P9) = 1

                                    Case 43 To 79
                                        defStyles(P9) = 1

                                    Case 80 To 90
                                        defStyles(P9) = 6

                                    Case 91 To 100
                                        defStyles(P9) = 7

                                End Select

                            End If

                        Case 5
                            'Zone Defense Prohibited
                            defStyles(P9) = 5
                            fullCtOpt(P9) = 0

                        Case 6 To 12
                            'Zone Defense Prohibited
                            defStyles(P9) = 3
                            fullCtOpt(P9) = 1

                        Case Is > 12
                            If teamYears(eraChoice) < 2001 Then
                                'Zone Defense Prohibited
                                defStyles(P9) = 1
                                fullCtOpt(P9) = 0
                            Else

                                Select Case RN0
                                
                                    Case 1 To 67
                                        defStyles(P9) = 1

                                    Case 68 To 80
                                        defStyles(P9) = 7
                                
                                    Case 81 To 100
                                        defStyles(P9) = 9
                                        
                                End Select
                            
                            End If

                    End Select
                                        

                End If

            Case Is > 4
                'Overtime Strategy
                If teamYears(eraChoice) < 2001 Then
                    'Zone Defense Prohibited
                    defStyles(P9) = 1
                    fullCtOpt(P9) = 1
                Else

                    Select Case RN0
                    
                        Case 0 To 50
                            defStyles(P9) = 1

                        Case 51 To 96
                            defStyles(P9) = 8

                        Case 97 To 100
                            defStyles(P9) = 9
                    
                    End Select
                
                End If

        End Select

    Else
    
        'Team is ahead

        Call Rolld100(RN0)
        fullCtOpt(P9) = 0

        If quarter <= 4 Then

            Select Case RN0

                Case Is <= 16
                    defStyles(P9) = 2

                Case 17 To 32
                    defStyles(P9) = 4

                Case 33 To 37
                    defStyles(P9) = 5

                Case Is >= 38
                    defStyles(P9) = 1

            End Select

        End If

        If quarter = 3 Then

            Select Case RN0

                Case Is <= 21
                    defStyles(P9) = 2

                Case 22 To 41
                    defStyles(P9) = 4

                Case 42 To 45
                    defStyles(P9) = 5

                Case Is >= 46
                    defStyles(P9) = 1

            End Select

        End If

        If quarter = 4 Then

            Select Case RN0

                Case Is < 31
                    defStyles(P9) = 4

                Case 32 To 61
                    defStyles(P9) = 2

                Case 62 To 65
                    defStyles(P9) = 5

                Case Is >= 66
                    defStyles(P9) = 1

            End Select

        End If

        If quarter > 4 Then

            Select Case RN0

                Case Is < 21
                    defStyles(P9) = 4

                Case 22 To 41
                    defStyles(P9) = 2

                Case 42 To 45
                    defStyles(P9) = 5

                Case Is >= 46
                    defStyles(P9) = 1

            End Select

        End If

    End If

    ' **************************
    ' *** Offensive Strategy ***
    ' **************************

    'Default to Normal
    offStyles(P9) = 0

    If quarter < 3 Then
        'First Half
        offStyles(P9) = 0
    Else
        'Second Half
        '50% chance of using Normal / Aggressive
        If Int(Rnd(1) * 2) = 1 Then

            If score(P9, 0) - score(defTeam, 0) < 0 Then
                'Offense is behind = Aggressive
                offStyles(P9) = 1
            Else
                'Offense is ahead = Safe
                offStyles(P9) = 2
            End If

        End If

    End If

    'Call pbplog(11798)

End Sub


'----------------------------------------
'        ComputerCoach Subroutine
'----------------------------------------
'This subroutine essentially handles the
'AI for the computer coach. After an
'evaluation of substitutions, it then calastLoseStreak
'another routine to decide on strategies
Sub ComputerCoach ()

    For I2 = 0 To 4
        lineupIdx(P9, I2) = -1
    Next

    'Assign fatigue values to each index in the lineup
    For I2 = 0 To 1

        P$ = "G"

        8781 '
        For I1 = 14 To 0 Step -1:

            M6 = 0
            M7 = 0

            If Left$(players_GAME$(P9, I1, 1), 1) = P$ Or Right$(players_GAME$(P9, I1, 1), 1) = P$ Then

                If goodPlayer%(P9, I1, FS%) = 1 Then
                    'Not (plyrRat_GAME(P9, I1, 18) <> 0 Or players_GAME$(P9, I1, 0) = "XXX" Or gameRatings!(P9, I1, 12) >= FOULS_TO_DQ Or (quarter = 1 And gameRatings!(P9, I1, 12) >= FOULS_TO_DQ - 4 + FS%) Or (quarter = 2 And gameRatings!(P9, I1, 12) >= FOULS_TO_DQ - 2 + FS%) Or (quarter = 3 And gameRatings!(P9, I1, 12) >= FOULS_TO_DQ - 1) Or (quarter = 4 And gameClock! > 300 And gameRatings!(P9, I1, 12) = FOULS_TO_DQ - 1))

                    M6 = playerFatigue%(P9, I1)

                    If lineupIdx(P9, I2) > -1 Then
                        M7 = playerFatigue%(P9, I2)
                    End If

                    If quarter = 4 And gameClock! < 120 Or quarter > 4 Then
                        M6 = gameRatings!(P9, I1, 3)
                    End If

                    If (quarter = 4 And gameClock! < 120 Or quarter > 4) And lineupIdx(P9, I2) > -1 Then
                        M7 = gameRatings!(P9, lineupIdx(P9, I2), 3)
                    End If

                    If I2 <> 1 Or lineupIdx(P9, 0) <> I1 Then
                        If lineupIdx(P9, I2) = -1 Or M6 >= M7 Then
                            lineupIdx(P9, I2) = I1
                        End If
                    End If

                End If

            End If

        Next I1

        If lineupIdx(P9, I2) = -1 And FS% <> 1 Then

            If P$ = "G" Then
                P$ = "F"
            Else
                If P$ = "F" Then
                    P$ = "C"
                Else
                    'Condition: P$ = "C"
                    P$ = "G"
                    FS% = 1
                End If
            End If

            GoTo 8781

        Else
            FS% = 0
        End If

        'Loop Until FS% = 1 And lineupIdx(P9, I2) <> -1

    Next I2

    For I2 = 2 To 3

        P$ = "F"

        8783 '
        For I1 = 14 To 0 Step -1:

            M6 = 0
            M7 = 0

            If Left$(players_GAME$(P9, I1, 1), 1) = P$ Or Right$(players_GAME$(P9, I1, 1), 1) = P$ Then

                If goodPlayer%(P9, I1, FS%) = 1 Then
                    'Not (plyrRat_GAME(P9, I1, 18) <> 0 Or players_GAME$(P9, I1, 0) = "XXX" Or gameRatings!(P9, I1, 12) >= FOULS_TO_DQ Or (quarter = 1 And gameRatings!(P9, I1, 12) >= FOULS_TO_DQ - 4 + FS%) Or (quarter = 2 And gameRatings!(P9, I1, 12) >= FOULS_TO_DQ - 2 + FS%) Or (quarter = 3 And gameRatings!(P9, I1, 12) >= FOULS_TO_DQ - 1) Or (quarter = 4 And gameClock! > 300 And gameRatings!(P9, I1, 12) = FOULS_TO_DQ - 1))

                    M6 = playerFatigue%(P9, I1)

                    If lineupIdx(P9, I2) > -1 Then
                        M7 = playerFatigue%(P9, I2)
                    End If

                    If quarter = 4 And gameClock! < 120 Or quarter > 4 Then
                        M6 = gameRatings!(P9, I1, 3)
                    End If

                    If (quarter = 4 And gameClock! < 120 Or quarter > 4) And lineupIdx(P9, I2) > -1 Then
                        M7 = gameRatings!(P9, lineupIdx(P9, I2), 3)
                    End If

                    If Not (I2 = 2 And (lineupIdx(P9, 0) = I1 Or lineupIdx(P9, 1) = I1)) Then
                        If Not (I2 = 3 And (lineupIdx(P9, 0) = I1 Or lineupIdx(P9, 1) = I1 Or lineupIdx(P9, 2) = I1)) Then
                            If lineupIdx(P9, I2) = -1 Or M6 >= M7 Then lineupIdx(P9, I2) = I1
                        End If
                    End If

                End If

            End If

        8784 Next I1

        If lineupIdx(P9, I2) = -1 And FS% <> 1 Then
            If P$ = "F" Then
                P$ = "C"
                GoTo 8783
            End If

            If P$ = "C" Then
                P$ = "G"
                GoTo 8783
            Else
                FS% = 1
                P$ = "F"
                GoTo 8783
            End If
        End If

        FS% = 0

    Next I2

    P$ = "C"

    8785 '
    For I1 = 14 To 0 Step -1

        M6 = 0
        M7 = 0

        If Left$(players_GAME$(P9, I1, 1), 1) = P$ Or Right$(players_GAME$(P9, I1, 1), 1) = P$ Then

            If goodPlayer%(P9, I1, FS%) = 1 Then
                'Not (plyrRat_GAME(P9, I1, 18) <> 0 Or players_GAME$(P9, I1, 0) = "XXX" Or gameRatings!(P9, I1, 12) >= FOULS_TO_DQ Or (quarter = 1 And gameRatings!(P9, I1, 12) >= FOULS_TO_DQ - 4 + FS%) Or (quarter = 2 And gameRatings!(P9, I1, 12) >= FOULS_TO_DQ - 2 + FS%) Or (quarter = 3 And gameRatings!(P9, I1, 12) >= FOULS_TO_DQ - 1) Or (quarter = 4 And gameClock! > 300 And gameRatings!(P9, I1, 12) = FOULS_TO_DQ - 1))

                M6 = playerFatigue%(P9, I1)
                
                If lineupIdx(P9, 4) > -1 Then
                    M7 = playerFatigue%(P9, 4)
                End If

                If quarter = 4 And gameClock! < 120 Or quarter > 4 Then
                    M6 = gameRatings!(P9, I1, 3)
                End If

                If (quarter = 4 And gameClock! < 120 Or quarter > 4) And lineupIdx(P9, I2) > -1 Then
                    M7 = gameRatings!(P9, lineupIdx(P9, I2), 3)
                End If

                If Not (lineupIdx(P9, 0) = I1 Or lineupIdx(P9, 1) = I1 Or lineupIdx(P9, 2) = I1 Or lineupIdx(P9, 3) = I1) Then
                    If lineupIdx(P9, 4) = -1 Or M6 >= M7 Then lineupIdx(P9, 4) = I1
                End If

            End If

        End If

    Next I1

    If lineupIdx(P9, I2) = -1 And FS% <> 1 Then

        If P$ = "C" Then
            P$ = "F"
            GoTo 8785
        End If
        If P$ = "F" Then
            P$ = "G"
            GoTo 8785
        Else
            FS% = 1
            P$ = "F"
            GoTo 8785
        End If

    End If

    FS% = 0

    'Assign members to lineup
    For I2 = 0 To 4

        If lineupIdx(P9, I2) = -1 Then

            8790 '
            I = 15

            For I2 = 0 To 4
                8795 '
                I = I - 1

                If I < 0 Then
                    Call ADJFOUL
                    GoTo 8790
                End If

                If plyrRat_GAME(P9, I, 18) <> 0 Or players_GAME$(P9, I, 0) = "XXX" Or gameRatings!(P9, I, 12) >= FOULS_TO_DQ Then
                    GoTo 8795
                End If

                lineupIdx(P9, I2) = I

            Next I2

            Exit For

        End If

    Next

    Call ComputerStrategy

End Sub


'----------------------------------------
'           MADEFT Subroutine
'----------------------------------------
'This subroutine handles score, stats and
'Play-by-Play for a successful free throw
Sub MADEFT (currFreeThrow)

    'Increase FTM
    gameRatings!(P, lineupIdx(P, ballCarrier), 10) = gameRatings!(P, lineupIdx(P, ballCarrier), 10) + 1

    If (pbpDelay! > 0) Then
        Locate currFreeThrow + 9, 1
        Call DELAY
        Print currFreeThrow; " GOOD"
    End If

    score(P, quarter) = score(P, quarter) + 1
    score(P, 0) = score(P, 0) + 1

    Call UPDATESCREEN
    Call DELAY

End Sub


'----------------------------------------
'           MISSFT Subroutine
'----------------------------------------
'This subroutine handles Play-by-Play
'for a missed free throw
Sub MISSFT (currFreeThrow)

    If (pbpDelay! > 0) Then

        Call DELAY
        Locate currFreeThrow + 9, 1
        Print currFreeThrow; " MISSED"

        Call UPDATESCREEN
        Call DELAY

    End If

End Sub


'----------------------------------------
'           LINEUPS Subroutine
'----------------------------------------
'This subroutine displays the starting
'lineup for each team and gives the user
'an interface for changing the lineup
Sub LINEUPS ()

    Shared alpha$()

    Cls
    Color teamRat_GAME(P9, 33), teamRat_GAME(P9, 34): Print gameTeams$(P9);

    Color 15, 0
    Print " TEAM ROSTER   COACH: "; gameCoach$(P9); "     ";

    Print "  RECORD: ";
    Print Using "##"; teamRat_GAME(P9, 1) + teamRat_GAME(P9, 3);
    Print "-";
    Print Using "##"; teamRat_GAME(P9, 2) + teamRat_GAME(P9, 4)

    Color 14
    Print "# NAME         PS GP MG FG 3FG %3A FT  ORB  DRB PA DS B  CX CN PT F  FC PTS/G"

    Locate 3, 1

    For R1 = 0 To 14

        If plyrRat_GAME(P9, R1, 18) <> 0 Or players_GAME$(P9, R1, 0) <> "XXX" Then

            Color 2

            '#
            If R1 <= 9 Then
                Print Using "# "; R1;
            Else
                Print alpha$(R1 - 10);
            End If

            Color 15

            'Name
            Print Left$(players_GAME$(P9, R1, 0), 12);
            Locate , 16

            'Position
            If Len(players_GAME$(P9, R1, 1)) = 1 Then Print players_GAME$(P9, R1, 1);
            If Len(players_GAME$(P9, R1, 1)) > 1 Then Print Left$(players_GAME$(P9, R1, 1), 1); Right$(players_GAME$(P9, R1, 1), 1);

            Locate , 19

            'Games Played, Minutes / Game
            Print Using "## ##"; plyrRat_GAME(P9, R1, 0); plyrRat_GAME(P9, R1, 1) / plyrRat_GAME(P9, R1, 0);

            'FG, 3FG , %3A, FT, ORB, DRB, Pass Rating, Steals, Blocks
            Print Using "### ### ## ### ##.# ##.# ## ## ##"; gameRatings!(P9, R1, 0); gameRatings!(P9, R1, 20); gameRatings!(P9, R1, 19); gameRatings!(P9, R1, 1); gameRatings!(P9, R1, 21) / 10; gameRatings!(P9, R1, 2) / 10; gameRatings!(P9, R1, 4) / 10; gameRatings!(P9, R1, 5); gameRatings!(P9, R1, 6);

            'Contribution
            L = 0
            L = gameRatings!(P9, R1, 7) + gameRatings!(P9, R1, 11) + gameRatings!(P9, R1, 12)
            Print Using "### "; gameRatings!(P9, R1, 3) - L;

            'Condition
            J = playerFatigue%(P9, R1)

            If quarter <= 3 And J / gameRatings!(P9, R1, 3) < .25 Then
                Locate , 61: Print " T";
            End If

            If J / gameRatings!(P9, R1, 3) < .5 And quarter <= 2 Then
                Locate , 61: Print " T";
            End If

            If quarter = 4 And gameClock! > 200 And J / gameRatings!(P9, R1, 3) <= gameClock! / 2880 Then
                Locate , 61: Print " T";
            End If

            If gameRatings!(P9, R1, 3) - gameRatings!(P9, R1, 7) - gameRatings!(P9, R1, 11) - gameRatings!(P9, R1, 12) <= 0 Then
                Locate , 61: Print "EX";
            End If

            If quarter > 4 Or quarter = 4 And gameClock! <= 120 And gameClock! <> 0 Then
                Locate , 61: Print "  ";
            End If

            'Points, Fouls, Foul Commit, Pts / Game
            Locate , 63: Print Using "### # ### ##.#"; gameRatings!(P9, R1, 8) * 2 + gameRatings!(P9, R1, 10) + threeFG(P9, R1, 0); gameRatings!(P9, R1, 12); gameRatings!(P9, R1, 17); (plyrRat_GAME(P9, R1, 3) * 2 + plyrRat_GAME(P9, R1, 5) + plyrRat_GAME(P9, R1, 14)) / plyrRat_GAME(P9, R1, 0)

        End If

    Next R1

    Locate 18, 1
    Color 9, 0
    Print String$(27, 205); Chr$(203); String$(51, 205)

    For R1 = 1 To 7:
        Locate R1 + 18, 28: Print Chr$(186);
    Next

End Sub


'----------------------------------------
'         SHOTPERCENT Subroutine
'----------------------------------------
'This subroutine calculates the chance of
'a shot being made based on the active
'defense / offense / players.
Sub SHOTPERCENT (useAssist)

    shotChance = gameRatings!(P, lineupIdx(P, ballCarrier), 0) + dFGPA(D)

    'Shift shot chance towards team with homefield advantage
    If gameLoc = P Then
        shotChance = shotChance + 2
    Else
        If gameLoc <> 2 Then
            shotChance = shotChance - 2
        End If
    End If

    If useAssist = 1 Then

        assistShotBoost = Int(Rnd(1) * 41)
        assistChance = Int(Rnd(1) * miscAdj(P, 4)) + 1
        assistRoll = CInt(.17 * miscAdj(P, 4))

        ' *** CHECK ASSIST SHOT ***
        If assistRoll >= assistChance Then

            'Assisted shot
            shotChance = shotChance + assistShotBoost
            shotType = 2
            offStatus = 0

        Else

            ' *** CHECK PENETRATION SHOT ***
            assistShotBoost = Int(Rnd(1) * 50) + 1

            'Call Rolld100(penetratRoll)
            penetratRoll = Int(Rnd(1) * 60) + 1

            penetratChance = (plyrRat_GAME(P, lineupIdx(P, ballCarrier), 16) + penetrationOffAdj(offStyles(P))) * 2 + 1

            If penetratRoll <= penetratChance Then

                shotType = 3
                'playChoice = 1
                offStatus = 0

                shotChance = shotChance + assistShotBoost

            Else

                ' *** CHECK AVERAGE SHOT ***
                I2 = plyrRat_GAME(P, lineupIdx(P, ballCarrier), 16)
                rndTeam = Int(Rnd(1) * 2)

                If Left$(players_GAME$(P, lineupIdx(P, ballCarrier), 1), 1) = "G" Or Left$(players_GAME$(P, lineupIdx(P, ballCarrier), 1), 1) = "F" Then

                    'For a Guard or Forward
                    R2 = Int(Rnd(1) * 3)

                    Select Case R2
                        Case 1: shotType = 6
                        Case 2: shotType = 7
                        Case 3: shotType = 5
                    End Select

                Else

                    If rndTeam = 1 Then
                        shotType = 6
                    Else
                        shotType = 5
                    End If

                End If

                '-- This sometimes throws an error
                shotChance = shotChance + penetrationDefAdj(rndTeam, I2)

                offStatus = 0

            End If

        End If

    Else

        If Not (quarter > 4 Or quarter = 4 And gameClock! <= 120) Then
            If playerFatigue%(P, lineupIdx(P, ballCarrier)) <= 0 Then
                shotChance = shotChance * .6
            End If
        End If

    End If

End Sub


'----------------------------------------
'           THREEFT Subroutine
'----------------------------------------
'This subroutine walks the user through
'taking 3 free throw attempts.
Sub THREEFT (currFreeThrow)

    'Increase FTM
    gameRatings!(P, lineupIdx(P, ballCarrier), 10) = gameRatings!(P, lineupIdx(P, ballCarrier), 10) + 1

    Call Rolld100(RN)

    Select Case currFreeThrow

        Case 1
            If RN <= 50 Then
                pbpString$ = "FREE THROW IS GOOD"
            Else
                pbpString$ = teamGender$(P) + " HITS THE FIRST FREE THROW"
            End If

        Case 2
            If RN <= 50 Then
                pbpString$ = "SECOND FREE THROW IS GOOD"
            Else
                pbpString$ = teamGender$(P) + " HITS THE SECOND ONE"
            End If

        Case 3
            If RN <= 50 Then
                pbpString$ = "THIRD FREE THROW IS GOOD"
            Else
                pbpString$ = teamGender$(P) + " HITS THE LAST ONE"
            End If

    End Select

    Call PBP(P)

    score(P, quarter) = score(P, quarter) + 1
    score(P, 0) = score(P, 0) + 1

    Call UPDATESCREEN

End Sub


'----------------------------------------
'          LastShot Subroutine
'----------------------------------------
'This subroutine handles calculations for
'the clock and playtime with the last shot.
Sub LastShot ()

    M5 = 0

    If gameClock! <= 2 Or IN = 99 Then

        M5 = 2

    Else

        Select Case gameClock!

            Case 3 To 4
                M5 = 2

            Case 5 To 16
                M5 = Int(Rnd(1) * 2) + 3

            Case Else
                M5 = Int(Rnd(1) * 2) + 4

        End Select

    End If

    Call AdjustClockTime(M5)
    Call AdjustPlayTime

    IN = 0

    If sClockVal < 2 Then
        shotClock = shotClock - M5
    End If

    Call CheckGameShotClock

End Sub


'----------------------------------------
'       CheckGameClock Subroutine
'----------------------------------------
'This subroutine checks the game clock
'and resets it if expired
Sub CheckGameClock ()

    'Check for late game timeouts (last 3 minutes).
    'Limit teams to 2 for 2018+ seasons
    If timeoutOpt >= 2 And assigned2TOs = 0 And quarter = 4 And gameClock! <= 180 Then
        If timeouts(0) > 2 Then
            timeouts(0) = 2
        End If
        If timeouts(1) > 2 Then
            timeouts(1) = 2
        End If
        assigned2TOs = 1
    End If

    If gameClock! <= 0 Then

        timeElapsed! = gameClock!
        Call AdjustPlayTime
        gameClock! = 0

        If autoPlay <> 1 And playerMode <> 2 Then
            Call HORN
        End If

        Call UPDATESCREEN

    End If

End Sub


'----------------------------------------
'     CheckGameShotClock Subroutine
'----------------------------------------
'This subroutine checks if either the game
'clock and shot clock have expired, and
'zeros them out if so. UPDATESCREEN is
'called to update the clock on the board.
Sub CheckGameShotClock ()

    If gameClock! <= .001 Then
        timeElapsed! = gameClock!
        Call AdjustPlayTime
        gameClock! = 0
    End If

    If shotClock <= 0 Then
        shotClock = 0
    End If

    Call UPDATESCREEN

End Sub


'----------------------------------------
'       AdjustClockTime Subroutine
'----------------------------------------
'This subroutine updates the clock for
'the play, based on the time specified
Sub AdjustClockTime (timeVal!)

    timeElapsed! = timeVal!

    gameClock! = gameClock! - timeElapsed!

End Sub


'----------------------------------------
'       AdjustPlayTime Subroutine
'----------------------------------------
'This subroutine increases the play time
'for each active player
Sub AdjustPlayTime ()

    ' *** INCREMENT MINUTES ***
    For I3 = 0 To 1

        For I4 = 0 To 4
            timePlayed(I3, lineupIdx(I3, I4)) = timePlayed(I3, lineupIdx(I3, I4)) + timeElapsed!
        Next

    Next

End Sub


'----------------------------------------
'       ResetShotClock Subroutine
'----------------------------------------
'This subroutine resets the shot clock.
'It uses defined setting value in order
'to determine how much time to add.
'A possible rewrite of the function
'would see it simply accept the amount
'of time as a parameter instead, but
'that would be harder to control for any
'invalid values that might get passed.
Sub ResetShotClock ()

    If sClockVal < 2 Then

        Select Case sClockVal

            Case 0:
                possTime! = possTime! + (24 - shotClock)
                shotClock = 24

            Case 1:
                possTime! = possTime! + (30 - shotClock)
                shotClock = 30

        End Select

        LS = 4

    End If

End Sub


'----------------------------------------
'        FreeThrow_One Subroutine
'----------------------------------------
'This routine walks through play-by-play
'for 1 free throw attempt.
'It also calculates the stat adjustments
'and score afterward.
Sub FreeThrow_One (missedLastFT)

    offStatus = 50
    Call Substitutions
    offStatus = 0

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        If pbpOpt = 1 Then
            Call WINDEX
            Print gameTeams$(P)
        End If

        pbpString$ = "1 FT: " + players_GAME$(P, lineupIdx(P, ballCarrier), 0) + Str$(gameRatings!(P, lineupIdx(P, ballCarrier), 1)) + " %"
        Call SELECTPBP(P, 0)

    End If

    missedLastFT = 0

    'Increase FT ATT
    gameRatings!(P, lineupIdx(P, ballCarrier), 9) = gameRatings!(P, lineupIdx(P, ballCarrier), 9) + 1

    'One FT Attempt
    freeThrowNbr = 1

    Call Rolld100(shotRoll)

    'FT Success Chance
    ftshotChance = gameRatings!(P, lineupIdx(P, ballCarrier), 1)

    If shotRoll <= ftshotChance Then

        If pbpOpt = 1 Then
            Call MADEFT(freeThrowNbr)
        Else
            Call THREEFT(freeThrowNbr)
        End If

        If autoPlay <> 1 Or playerMode <> 2 And swish& And d100% < 10 Then
            _SndPlay swish&
        End If

        missedLastFT = 0

    Else

        If pbpOpt = 1 Then
            Call MISSFT(freeThrowNbr)
        Else
            Call BADFT
        End If

        If autoPlay <> 1 Or playerMode <> 2 And swish& And d100% < 10 Then
            _SndPlay backboard&
        End If

        missedLastFT = 1

    End If

End Sub


'----------------------------------------
'       FreeThrow_Three Subroutine
'----------------------------------------
'This routine walks through play-by-play
'for 3 free throw attempts.
'It also calculates the stat adjustments
'and score afterward.
Sub FreeThrow_Three (missedLastFT)

    offStatus = 50
    Call Substitutions
    offStatus = 0

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        If pbpOpt = 1 Then
            Call WINDEX
            Print gameTeams$(P)
        End If

        pbpString$ = "3 FT: " + players_GAME$(P, lineupIdx(P, ballCarrier), 0) + Str$(gameRatings!(P, lineupIdx(P, ballCarrier), 1)) + " %"
        Call SELECTPBP(P, 0)

    End If

    missedLast = 0

    For freeThrowNbr = 1 To 3

        'Increase FTA
        gameRatings!(P, lineupIdx(P, ballCarrier), 9) = gameRatings!(P, lineupIdx(P, ballCarrier), 9) + 1

        Call Rolld100(shotRoll)

        'FT Success Chance
        ftshotChance = gameRatings!(P, lineupIdx(P, ballCarrier), 1)

        If shotRoll <= ftshotChance Then

            If pbpOpt = 1 Then
                Call MADEFT(freeThrowNbr)
            Else
                Call THREEFT(freeThrowNbr)
            End If

            If autoPlay <> 1 Or playerMode <> 2 And swish& And d100% < 10 Then
                _SndPlay swish&
            End If

        Else

            If pbpOpt = 1 Then
                Call MISSFT(freeThrowNbr)
            Else
                Call BADFT
            End If

            If autoPlay <> 1 Or playerMode <> 2 And swish& And d100% < 10 Then
                _SndPlay backboard&
            End If

        End If

    Next freeThrowNbr

    If shotRoll > ftshotChance Then
        missedLastFT = 1
    Else
        missedLastFT = 0
    End If

End Sub


'----------------------------------------
'      FreeThrow_Make1 Subroutine
'----------------------------------------
'This routine walks through play-by-play
'for 2 attempts to make 1 free throws.
'It also calculates the stat adjustments
'and score afterward.
Sub FreeThrow_Make1 (missedLastFT)

    offStatus = 50
    Call Substitutions
    offStatus = 0

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        If pbpOpt = 1 Then
            Call WINDEX
            Print gameTeams$(P)
        End If

        If freeThrowVal = 1 And teamFouls(D) >= bonusFoulNum And quarter <= 4 Or teamFouls(D) > 3 And quarter > 4 Then
            pbpString$ = "2 TO MAKE 1 FT: " + players_GAME$(P, lineupIdx(P, ballCarrier), 0) + Str$(gameRatings!(P, lineupIdx(P, ballCarrier), 1)) + " %"
        Else
            pbpString$ = "1 FT: " + players_GAME$(P, lineupIdx(P, ballCarrier), 0) + Str$(gameRatings!(P, lineupIdx(P, ballCarrier), 1)) + " %"
        End If

        Call SELECTPBP(P, 0)

    End If

    Call DELAY
    nbrFTMade = 0

    missedLastFT = 0

    'Two FT Attempts
    For freeThrowNbr = 1 To 2

        'Increase FT ATT
        gameRatings!(P, lineupIdx(P, ballCarrier), 9) = gameRatings!(P, lineupIdx(P, ballCarrier), 9) + 1

        Call Rolld100(shotRoll)

        'FT Success Chance
        ftshotChance = gameRatings!(P, lineupIdx(P, ballCarrier), 1)

        If shotRoll <= ftshotChance Then

            'track number FT's made
            nbrFTMade = nbrFTMade + 1

            If pbpOpt = 1 Then
                Call MADEFT(freeThrowNbr)
            Else
                Call THREEFT(freeThrowNbr)
            End If

            If autoPlay <> 1 Or playerMode <> 2 And swish& And d100% < 10 Then
                _SndPlay swish&
            End If

        Else

            If pbpOpt = 1 Then
                Call MISSFT(freeThrowNbr)
            Else
                Call BADFT
            End If

            If autoPlay <> 1 Or playerMode <> 2 And backboard& And d100% < 10 Then
                _SndPlay backboard&
            End If

        End If

        If nbrFTMade = 1 Then
            Exit For
        End If

        If freeThrowNbr = 1 And (freeThrowVal = 0 Or freeThrowVal = 1 And (teamFouls(D) <= bonusFoulNum And quarter <= 4 Or teamFouls(D) <= 3 And quarter > 4)) Then
            Exit For
        End If

    Next freeThrowNbr

    If shotRoll > ftshotChance Then
        missedLastFT = 1
    Else
        missedLastFT = 0
    End If

End Sub


'----------------------------------------
'      FreeThrow_Make2 Subroutine
'----------------------------------------
'This routine walks through play-by-play
'for 3 attempts to make 2 free throws.
'It also calculates the stat adjustments
'and score afterward.
Sub FreeThrow_Make2 (missedLastFT)

    offStatus = 50
    Call Substitutions
    offStatus = 0

    If (autoPlay = 0 Or pbpDelay! > 0) Then

        If pbpOpt = 1 Then
            Call WINDEX
            Print gameTeams$(P)
        End If

        If freeThrowVal = 1 And teamFouls(D) >= bonusFoulNum And quarter <= 4 Or teamFouls(D) > 3 And quarter > 4 Then
            pbpString$ = "3 TO MAKE 2 FT: " + players_GAME$(P, lineupIdx(P, ballCarrier), 0) + Str$(gameRatings!(P, lineupIdx(P, ballCarrier), 1)) + " %":
        Else
            pbpString$ = "2 FT: " + players_GAME$(P, lineupIdx(P, ballCarrier), 0) + Str$(gameRatings!(P, lineupIdx(P, ballCarrier), 1)) + " %"
        End If

        Call SELECTPBP(P, 0)

    End If

    Call DELAY
    nbrFTMade = 0

    missedLastFT = 0

    'Three FT Attempts
    'We will quit either after 2nd attempt (modern rules)
    '(or after the 2nd one is made (old rules)
    For freeThrowNbr = 1 To 3

        'Increase FTA
        gameRatings!(P, lineupIdx(P, ballCarrier), 9) = gameRatings!(P, lineupIdx(P, ballCarrier), 9) + 1

        Call Rolld100(shotRoll)

        'FT Success Chance
        ftshotChance = gameRatings!(P, lineupIdx(P, ballCarrier), 1)

        If shotRoll <= ftshotChance Then

            nbrFTMade = nbrFTMade + 1

            If pbpOpt = 1 Then
                Call MADEFT(freeThrowNbr)
            Else
                Call THREEFT(freeThrowNbr)
            End If

            If autoPlay <> 1 Or playerMode <> 2 And swish& And d100% < 10 Then
                _SndPlay swish&
            End If

        Else

            If pbpOpt = 1 Then
                Call MISSFT(freeThrowNbr)
            Else
                Call BADFT
            End If

            If autoPlay <> 1 Or playerMode <> 2 And swish& And d100% < 10 Then
                _SndPlay backboard&
            End If

        End If

        'Only have to make 2 (3 to make 2 rules)
        If nbrFTMade = 2 Then
            Exit For
        End If

        'On 2nd attempt
        If freeThrowNbr = 2 Then
            'Modern rules - just exit (only 2 FT)
            '3 to make 2 - if 2 misses then done
            If freeThrowVal = 0 Or (freeThrowVal = 1 And (teamFouls(D) <= bonusFoulNum And quarter <= 4 Or teamFouls(D) <= 3 And quarter > 4)) Then
                Exit For
            End If
        End If

    Next freeThrowNbr

    If shotRoll > ftshotChance Then
        missedLastFT = 1
    Else
        missedLastFT = 0
    End If

End Sub


'----------------------------------------
'           OPTIONS Subroutine
'----------------------------------------
'This subroutine gives you a small menu
'for changing some of the game options
'(such as pbp delay, coaching mode) in
'the middle of game play.
Sub OPTIONS ()

    Shared coachMode$(), playMode$()

    Screen , , 0, 0

    Call INFO

    Do

        Color , 0
        Locate 3, 1
        Color 14: Print "(1) ";
        Color 15: Print playMode$(playerOpt)
        Color 14: Print "(2) ";
        Color 15: Print Using "PLAY-BY-PLAY DELAY: #.# "; pbpDelay!
        Color 14: Print "(3) ";
        Color 15: Print "HUMAN COACHES: "; coachMode$(coachOpt)

        Print
        Color 14: Print "<ENTER> ";
        Color 15: Print "RETURN TO GAME PLAY"

        Do
            Color 7, 0
            'Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 0 And I <= 3

        Select Case I

            Case 1
                playerOpt = playerOpt + 1
                If playerOpt > 3 Then playerOpt = 0

            Case 2
                Locate 4, 23: Input pbpDelay!

            Case 3
                coachOpt = 1 - coachOpt

        End Select

        ' Determine computer team
        Select Case playerOpt

            Case 1:
                playerMode = 1
                compTeam = 0

            Case 2:
                playerMode = 1
                compTeam = 1

            Case 3:
                playerMode = 2

        End Select

    Loop Until I$ = Chr$(13)

    Screen , , 0, 0
    Call SCOREBOARD

End Sub


'----------------------------------------
'           EVENTS Subroutine
'----------------------------------------
'This subroutine works with another to
'look for any events that meet the limits
'for "special". If found, it will create
'a corresponding "special" box score
Sub EVENTS (specialCount, specialStat)

    Shared eventSettings()

    If _FileExists("EVENTS.BOX") Then

        Open "EVENTS.BOX" For Input As #1

        For X = 1 To 13
            Input #1, eventSettings(X)
        Next

        Close #1

        checkSpecial% = 0

        If quarter - 1 > eventSettings(1) Then
            checkSpecial% = 1
        Else
            '--Combine into multiple Or
            If score(1, 0) + score(0, 0) > eventSettings(2) Then
                checkSpecial% = 1
            Else
                If totRebounds(0) + totRebounds(1) > eventSettings(3) Then
                    checkSpecial% = 1
                Else
                    If score(1, 0) > eventSettings(4) Or score(0, 0) > eventSettings(4) Then
                        checkSpecial% = 1
                    Else
                        If totRebounds(1) > eventSettings(5) Or totRebounds(0) > eventSettings(5) Then
                            checkSpecial% = 1
                        End If
                    End If
                End If
            End If
        End If

        If checkSpecial% <> 1 Then

            For I = 0 To 1

                For I1 = 0 To 14

                    If gameRatings!(I, I1, 8) * 2 + gameRatings!(I, I1, 10) + threeFG(I, I1, 0) > eventSettings(6) Then
                        checkSpecial% = 1
                    Else
                        If gameRatings!(I, I1, 11) > eventSettings(7) Or gameRatings!(I, I1, 14) > eventSettings(8) Or gameRatings!(I, I1, 15) > eventSettings(9) Or gameRatings!(I, I1, 13) > eventSettings(10) Or threeFG(I, I1, 0) > eventSettings(11) Then
                            checkSpecial% = 1
                        End If
                    End If

                    If checkSpecial% = 1 Then Exit For

                Next

            Next 'done checking if special event already found

        End If

    End If 'Done checking EVENTS.BOX file

    If checkSpecial% = 1 Then
        Call FoundSpecial(specialCount, specialStat)
    End If

End Sub


'----------------------------------------
'        FoundSpecial Subroutine
'----------------------------------------
'This subroutine simply counts how many
'special boxscore files already exist, so
'that it an assign a unique number to the
'boxscore about to be created.
Sub FoundSpecial (specialCount, specialStat)

    'Get count of SPC files to determine
    'number to assign next one
    Spec$ = diskPaths$(1) + "*.BSS"
    'Spec$ = diskPaths$(1) + "*.SPC"
    specialCount = FileCount%(Spec$)
    specialCount = specialCount + 1
    specialStat = 1

End Sub


'----------------------------------------
'        UPDATESCREEN Subroutine
'----------------------------------------
'This subroutine refreshes all the values
'for the main screen, including the
'scoreboard, team options and rosters.
'The frame will have already been drawn
'by the SCOREBOARD routine.
Sub UPDATESCREEN ()

    Call TICKERUPDATE

    'Team 2
    '---

    'Game Clock - Minutes
    Color 15, 0
    Locate 1, 37
    Print Using "##"; Int(gameClock! / 60);
    Print ":";

    'Game Clock - Seconds
    '(Let's just round it to 2 places first)
    gcSeconds! = gameClock! - Int(gameClock! / 60) * 60
    gcSeconds! = Fix((gcSeconds! * 100 + Sgn(gcSeconds!) * .5)) / 100

    If gcSeconds! < 10 Then
        Print "0";
        Print Using "#"; gcSeconds!;
    Else
        Print Using "##"; gcSeconds!;
    End If

    'Team 2
    '---

    'Score Team 1
    Color 14, 0
    Locate 4, 13: Print Using "###"; score(0, 0);

    'Period Designation
    Locate 2, 35: Print Using " PERIOD #"; quarter;
    If quarter > 4 Then Locate , 35: Print Using "OVERTIME #"; quarter - 4

    'Score Team 2
    Color 14, 0
    Locate 4, 60: Print Using "###"; score(1, 0)

    'Turnovers
    Color 7
    Locate 3, 35
    Print Using "##  "; turnovers(0);
    Color 12: Print "TO  ";
    Color 7
    Print Using "##"; turnovers(1)

    'Rebounds
    Locate , 35: Print Using "##  "; totRebounds(0);
    Color 12: Print "RB  ";
    Color 7: Print Using "##"; totRebounds(1)

    'Fouls
    Locate , 35: Print Using "##  "; teamFouls(0);
    Color 12: Print "TF  ";
    Color 7
    Print Using "##"; teamFouls(1);

    'Bonus
    If teamFouls(0) >= bonusFoulNum And quarter <= 4 Or teamFouls(0) > 2 And quarter > 4 Then
        Color 0, 2
        Locate , 46: Print "BONUS";
        Color 7, 0
    End If

    If teamFouls(1) >= bonusFoulNum And quarter <= 4 Or teamFouls(1) > 2 And quarter > 4 Then
        Color 0, 2
        Locate , 29: Print "BONUS";
        Color 7, 0
    End If

    'Timeouts
    Locate 6, 9: Print "TIMEOUTS:"; timeouts(0);
    Locate , 56: Print "TIMEOUTS:"; timeouts(1)

    Color 15, 0

    'Shot Clock
    'If no shot clock, or shot clock expired
    If sClockVal = 2 Or (sClockVal < 2 And gameClock! < scSettings(sClockVal) And shotClock >= gameClock!) Or (quarter > 4 And gameClock! = 300) Then
        Locate 6, 37: Print " :  "
    Else
        Locate 6, 37: Print Using " :##"; shotClock
    End If

    'Team strategies / decisions
    If (autoPlay = 0 Or pbpDelay! > 0) Then

        'Team Play Styles
        Locate 8, 58
        Color teamRat_GAME(D, 33), teamRat_GAME(D, 34)
        Print "DEF - "; defStyleDesc$(defStyles(D))
        Locate , 58
        Color teamRat_GAME(P, 33), teamRat_GAME(P, 34)
        Print "OFF - "; offStyleDesc$(offStyles(P))

        Color 15, 0
        Locate , 41
        Color 14: Print "(1) ";
        Color 15: Print "SHOOT        ";
        Color 14: Print "ENTER";
        Color 15: Print "-PASS"
        Locate , 41
        Color 14: Print "(2) ";
        Color 15: Print gameTeams$(P);
        Print " CALL TIMEOUT      "
        Locate , 41
        Color 14: Print "(3) ";
        Color 15: Print "COMMIT INTL FOUL (LAST 4 MIN)"
        Locate , 41
        Color 14: Print "(4) ";
        Color 15: Print "SHOOT A FORCED 3-POINTER"
        Locate , 41
        Color 14: Print "(S) ";
        Color 15: Print "SCOUT REPORT ";
        Color 14: Print "(T) ";
        Color 15: Print "10 MIN TICKER"
        Locate , 41
        Color 14: Print "(Z) ";
        Color 15: Print "LEFT TM STGY ";
        Color 14: Print "(/) ";
        Color 15: Print "RIGHT TM STGY"

        Color 15, 0

        Locate 19, 1
        Call PrintActiveLineup(0)
        Locate 19, 40
        Call PrintActiveLineup(1)

    End If

End Sub


'----------------------------------------
'          POSTGAME Subroutine
'----------------------------------------
'This subroutine processes everything that
'is necessary after a game is complete.
'It checks for records / special events,
'handles a prompt for stats, and saving
'of stats.
Sub POSTGAME ()

    Call CheckRecords
    Call EVENTS(specialCount, specialStat)
    gameOver = 1

    Do

        Color 15, 0
        Cls

        Color 14: Print "(0) ";
        Color 15: Print "STATS TO SCREEN"
        Print

        Color 14: Print "(1) ";
        Color 15: Print "STATS TO PRINTER"
        Print

        Color 14: Print "(2) ";
        Color 15: Print "STATS TO TEXT FILE"

        If autoPlay = 1 And playerMode = 2 Then
            choice = 2
        Else
            Do
                I$ = GetKeyPress$
                choice = Val(I$)
            Loop Until choice >= 0 And choice <= 2
        End If

        Call BOXOPTIONS(choice)
        Call TICKER

        'Reset option for next call
        If choice <> 0 Then choice = 0

        'Prompt to display stats again
        Color 15, 0
        Cls

        If Not (autoPlay = 1 And playerMode = 2) Then

            Do
                'Using Home Team's color for the box
                Call TEAMBOX(1, 1)
                pbpString$ = "DISPLAY STATS AGAIN? (Y/N)"
                Locate 7, (80 - Len(pbpString$)) / 2
                Print pbpString$
                Color 15, 0
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" Or (autoPlay = 1 And playerMode = 2)

    'If we had special events
    If specialStat = 1 Then

        boxName$ = "BOXSCORE" + Mid$(Str$(specialCount), 2, Len(Str$(specialCount))) + ".BSS"
        'boxName$ = "BOX" + Mid$(Str$(specialCount), 2, Len(Str$(specialCount))) + ".SPC"

        'Append special events
        Call BOX2SPC(boxName$)

        Call BOX2FILE(2, boxName$, specialStat)

    End If

    Call SAVESTATS

    gameOver = 0
    specialStat = 0

End Sub


'----------------------------------------
'         BOXOPTIONS Subroutine
'----------------------------------------
'This subroutine takes appropriate action
'for the output of the box file, which
'depends on the choice given to it.

' PARAMETER outputChoice
'    represents menu choice
'       0 = Display on screen
'       1 = Send to printer
'       2 = Save to file

'The option to send to printer will first
'create a temporary file, then loop THROUGH
'it to print each line.
Sub BOXOPTIONS (outputChoice)

    Shared scheduleFile$

    Select Case outputChoice

        Case 0:
            Call BOX2SCREEN

        Case 1, 2:
            If outputChoice = 1 Then
                'Printer = temporary file
                boxName$ = "TEMPBOX"
            Else
                'File = prompt | auto-create
                If autoPlay <> 1 Then
                    Cls
                    Print "ENTER FILENAME FOR BOXSCORE (NO SPACES OR PERIODS)";
                    Input boxName$
                    boxName$ = boxName$ + ".BSS"
                    'boxName$ = boxName$ + ".SPC"
                Else
                    'ScheduleName_GameXXX.BS
                    boxName$ = Mid$(scheduleFile$, 1, Len(scheduleFile$) - 4) + "_GAME" + Mid$(Str$(schedGame(0) + schedGame(2)), 2, Len(Str$(schedGame(0) + schedGame(2)))) + ".BS"
                    'boxName$ = "BOX" + Mid$(Str$(schedGame(0) + schedGame(2)), 2, Len(Str$(schedGame(0) + schedGame(2))))
                End If

            End If

            'Includes a special option to send to printer when done
            Call BOX2FILE(outputChoice, boxName$, 0)

    End Select

End Sub


'----------------------------------------
'          BOX2FILE Subroutine
'----------------------------------------
'This subroutine formats the boxscore for
'an output file.

' PARAMETER printChoice
'    choice to print file vs. saving it

'The option to send to printer will first
'create a temporary file, then loop THROUGH
'it to print each line.
Sub BOX2FILE (printChoice, boxName$, hasSpecial)

    Shared actualAttendance&

    If hasSpecial = 1 Then
        Open (diskPaths$(1) + boxName$) For Append As #1
    Else
        Open (diskPaths$(1) + boxName$) For Output As #1
    End If

    If gameLoc = 2 Then
        Print #1, "SITE: NEUTRAL"
    Else
        Print #1, "SITE: "; gameArena$(gameLoc)
    End If

    Print #1, Using "  ATTENDANCE: ###,###"; actualAttendance&
    Print #1,

    Print #1, ; Tab(17); "P1  P2  P3  P4  ";

    If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) Then
        Print #1, ; Tab(33); "OT   ";

        If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) + score(0, 5) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) + score(1, 5) Then
            Print #1, ; Tab(37); "O2"; Tab(42); "GAME"
        Else
            Print #1, ; Tab(38); "GAME"
        End If
    Else
        Print #1, ; Tab(34); "GAME"
    End If

    For I = 0 To 1

        Print #1, gameTeams$(I); Tab(16); Str$(score(I, 1)); Tab(20); Str$(score(I, 2)); Tab(24); Str$(score(I, 3)); Tab(28); Str$(score(I, 4));

        team0Score = score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4)
        teafatiguePlayersScore = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4)

        If team0Score = teafatiguePlayersScore Then

            Print #1, Tab(32); Str$(score(I, 5));

            team0Score_OT = teafatiguePlayersScore + score(0, 5)
            teafatiguePlayersScore_OT = teafatiguePlayersScore + score(1, 5)

            If team0Score_OT = teafatiguePlayersScore_OT Then
                Print #1, Tab(36); Str$(score(I, 6)); Tab(39); " -  "; score(I, 0)
            Else
                'team0Score_OT <> teafatiguePlayersScore_OT
                Print #1, Tab(35); " - "; score(I, 0)
            End If
        Else
            'team0Score <> teafatiguePlayersScore
            Print #1, Tab(31); " - "; score(I, 0)

        End If

    Next I

    For I = 0 To 1

        Print #1,
        Print #1, gameTeams$(I)
        Print #1, "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB  RB  PT  F   A   B   S  TO"

        Erase statTotals

        For I1 = 0 To 14

            If players_GAME$(I, I1, 0) <> "XXX" Then

                'Player Name
                Print #1, players_GAME$(I, I1, 0); Tab(14);

                'Player Time
                If timePlayed(I, I1) = 0 Then
                    Print #1, Tab(15); "DNP";
                Else
                    Print #1, Using "##"; Int(timePlayed(I, I1) / 60);: Print #1, ":";

                    If timePlayed(I, I1) - Int(timePlayed(I, I1) / 60) * 60 > 9 Then
                        Print #1, Using "##"; timePlayed(I, I1) - Int(timePlayed(I, I1) / 60) * 60;
                    Else
                        Print #1, "0";: Print #1, Using "#"; timePlayed(I, I1) - Int(timePlayed(I, I1) / 60) * 60;
                    End If
                End If

                'FGM - FGA
                Print #1, Tab(21);: Print #1, Using "##"; gameRatings!(I, I1, 8);
                Print #1, "-";
                Print #1, Using "##"; gameRatings!(I, I1, 7);

                '3FGM - 3FGA
                Print #1, Using "  ##"; threeFG(I, I1, 0);
                Print #1, "-";
                Print #1, Using "##"; threeFG(I, I1, 1);

                'FTM - FTA
                Print #1, Using "  ##"; gameRatings!(I, I1, 10);
                Print #1, "-";
                Print #1, Using "##"; gameRatings!(I, I1, 9);

                'ORB / RB / Points / Fouls / Assists / Blocks / Steals / Turnovers
                Print #1, Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; gameRatings!(I, I1, 24); gameRatings!(I, I1, 11); gameRatings!(I, I1, 8) * 2 + gameRatings!(I, I1, 10) + threeFG(I, I1, 0); gameRatings!(I, I1, 12); gameRatings!(I, I1, 14); gameRatings!(I, I1, 15); gameRatings!(I, I1, 13); gameRatings!(I, I1, 23)

                statTotals(0) = statTotals(0) + gameRatings!(I, I1, 8)
                statTotals(1) = statTotals(1) + gameRatings!(I, I1, 7)
                statTotals(2) = statTotals(2) + gameRatings!(I, I1, 10)
                statTotals(3) = statTotals(3) + gameRatings!(I, I1, 9)
                statTotals(4) = statTotals(4) + gameRatings!(I, I1, 11)
                statTotals(5) = statTotals(5) + (gameRatings!(I, I1, 8) * 2) + gameRatings!(I, I1, 10) + threeFG(I, I1, 0)
                statTotals(13) = statTotals(13) + threeFG(I, I1, 0)
                statTotals(14) = statTotals(14) + threeFG(I, I1, 1)
                statTotals(6) = totalFouls(I)
                statTotals(7) = statTotals(7) + gameRatings!(I, I1, 14)
                statTotals(8) = statTotals(8) + gameRatings!(I, I1, 15)
                statTotals(9) = statTotals(9) + gameRatings!(I, I1, 13)
                statTotals(11) = statTotals(11) + gameRatings!(I, I1, 24)
                statTotals(12) = statTotals(12) + gameRatings!(I, I1, 23)
                statTotals(10) = statTotals(10) + timePlayed(I, I1)

            End If

        Next I1

        If statTotals(3) = 0 Then statTotals(3) = 1

        Print #1,
        Print #1, "TOTALS"; Tab(14); Int(statTotals(10) / 60); Tab(21);

        'FGM - FGA
        Print #1, Using "##"; statTotals(0);
        Print #1, "-";
        Print #1, Using "###"; statTotals(1);

        '3FGM - 3FGA
        Print #1, Tab(28); Using "##"; statTotals(13);
        Print #1, "-";
        Print #1, Using "##"; statTotals(14);

        'FTM - FTA
        Print #1, Tab(35); Using "##"; statTotals(2);
        Print #1, "-";
        Print #1, Using "##"; statTotals(3);

        Print #1, Tab(42);
        'ORB / RB / Points / Fouls / Assists / Blocks / Steals / Turnovers
        Print Using "##  ## ### ##  ##  ##  ##  ##"; statTotals(11); statTotals(4); statTotals(5); statTotals(6); statTotals(7); statTotals(8); statTotals(9); statTotals(12)

        'FG %
        Print #1, ; Tab(22);
        If statTotals(1) > 0 Then Print #1, ; CInt(((statTotals(0) / statTotals(1)) * 100)); "%";

        '3FG %
        Print #1, ; Tab(28);
        If statTotals(14) > 0 Then Print #1, ; CInt(((statTotals(13) / statTotals(14)) * 100)); "%";

        'FT %
        Print #1, ; Tab(35);
        If statTotals(3) > 0 Then Print #1, ; CInt(((statTotals(2) / statTotals(3)) * 100)); "%"

        Print #1,
        Print #1, "TOTAL TURNOVERS -"; turnovers(I)
        Print #1, "TOTAL REBOUNDS  -"; totRebounds(I)

    Next I

    'Print #1,
    'Print #1, "BENCH WARMERS"

    'For I = 0 To 1

    '    For I1 = 0 To 13

    'Automatically disable "non-existent" players
    '       If players_GAME$(I, I1, 0) <> "XXX" And plyrRat_GAME(I, I1, 18) = -1 Then
    '           Print #1, players_GAME$(I, I1, 0); " ["; gameTeams$(I); "]"
    '       End If

    '   Next I1

    'Next I

    If printChoice = 1 Then Print #1, Chr$(12)

    Close #1

    If printChoice = 1 Then

        Open diskPaths$(1) + boxName$ For Input As #2

        Do
            Line Input #2, X$
            LPrint X$
        Loop While Left$(X$, 1) <> Chr$(12)

        Close #2

        Kill "TEMPBOX"

    End If

End Sub


'----------------------------------------
'         BOX2SCREEN Subroutine
'----------------------------------------
'This subroutine formats the boxscore for
'output to the end user display.
Sub BOX2SCREEN ()

    Shared actualAttendance&

    Cls

    Color 14
    If gameLoc = 2 Then
        Print "SITE: ";
        Color 15
        Print "NEUTRAL"
    Else
        Print "SITE: ";
        Color 15
        Print gameArena$(gameLoc)
    End If

    Color 14
    Print "ATTENDANCE: ";
    Color 15
    Print Using "###,###"; actualAttendance&
    Print

    Call ScoreSummary

    If Not (autoPlay = 1 And playerMode = 2) Then
        Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$
    End If

    Call STATS2SCREEN

End Sub


'----------------------------------------
'        ScoreSummary Subroutine
'----------------------------------------
'This subroutine prints a score by period
'for each team.
Sub ScoreSummary ()

    Locate 4, 17
    Color 14

    If halfTime = 1 Then
        Print "P1  P2  "; Tab(35); "GAME"
    Else
        Print "P1  P2  P3  P4  ";

        If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) Then

            Locate , 34
            Print "OT  ";

            If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) + score(0, 5) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) + score(1, 5) Then
                Locate , 38
                Print "O2  "
            Else
                Locate , 39
                Print "GAME"
            End If
        Else
            Locate , 35
            Print "GAME"
        End If

    End If

    Color 15, 0

    For I = 0 To 1:
        Locate 5 + I,
        Color teamRat_GAME(I, 33), teamRat_GAME(I, 34)
        Print gameTeams$(I);

        Color 15, 0

        If halfTime = 1 Then
            Print Tab(16); Str$(score(I, 1)); Tab(20); Str$(score(I, 2)); Tab(32); " - "; Str$(score(I, 0))

        Else
            Print Tab(16); Str$(score(I, 1)); Tab(20); Str$(score(I, 2));
            Print Tab(24); Str$(score(I, 3)); Tab(28); Str$(score(I, 4));

            If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) Then
                Locate , 32: Print Str$(score(I, 5));

                If score(0, 1) + score(0, 2) + score(0, 3) + score(0, 4) + score(0, 5) = score(1, 1) + score(1, 2) + score(1, 3) + score(1, 4) + score(1, 5) Then
                    Locate , 36: Print Str$(score(I, 6));
                    Locate , 40: Print " - "; Str$(score(I, 0))
                Else
                    Locate , 35: Print " - "; Str$(score(I, 0))
                End If

            Else

                Locate , 32: Print " - "; Str$(score(I, 0))

            End If

        End If 'Done checking if half time

    Next I

End Sub


'----------------------------------------
'        STATS2SCREEN Subroutine
'----------------------------------------
'This subroutine formats the game stats
'for output to the end user display
Sub STATS2SCREEN

    For I = 0 To 1:

        Cls

        Color teamRat_GAME(I, 33), teamRat_GAME(I, 34)
        Print gameTeams$(I)

        Color 14, 0
        Print "NAME"; Tab(14); " MIN   FG- A 3FG- A  FT- A ORB REB PTS PF  AST BLK STL TO"

        Erase statTotals

        For I1 = 0 To 14

            If players_GAME$(I, I1, 0) <> "XXX" And timePlayed(I, I1) <> 0 Then

                Color 15

                'Name
                Print Left$(players_GAME$(I, I1, 0), 13); Tab(14);

                'Minutes
                If timePlayed(I, I1) <> 0 Then
                    Print Using "##"; Int(timePlayed(I, I1) / 60);
                    Print ":";
                    If timePlayed(I, I1) - Int(timePlayed(I, I1) / 60) * 60 > 9 Then
                        Print Using "##"; timePlayed(I, I1) - Int(timePlayed(I, I1) / 60) * 60;
                    Else
                        Print "0";
                        Print Using "#"; timePlayed(I, I1) - Int(timePlayed(I, I1) / 60) * 60;
                    End If
                End If

                Locate , 21

                'FG
                Print Using "##"; gameRatings!(I, I1, 8);
                Print "-";
                Print Using "##"; gameRatings!(I, I1, 7);

                '3FG
                Print Using "  ##"; threeFG(I, I1, 0);
                Print "-";
                Print Using "##"; threeFG(I, I1, 1);

                'FT
                Print Using "  ##"; gameRatings!(I, I1, 10);
                Print "-";
                Print Using "##"; gameRatings!(I, I1, 9);

                'ORB / RB / PT / F / A /  B / S / TO
                Print Using "  ##  ## ###  #  ##  ##  ##  ##  ##  ##"; gameRatings!(I, I1, 24); gameRatings!(I, I1, 11); gameRatings!(I, I1, 8) * 2 + gameRatings!(I, I1, 10) + threeFG(I, I1, 0); gameRatings!(I, I1, 12); gameRatings!(I, I1, 14); gameRatings!(I, I1, 15); gameRatings!(I, I1, 13); gameRatings!(I, I1, 23);

                If timePlayed(I, I1) = 0 Then Locate , 15: Print "DNP" Else Print

                statTotals(0) = statTotals(0) + gameRatings!(I, I1, 8)
                statTotals(1) = statTotals(1) + gameRatings!(I, I1, 7)
                statTotals(2) = statTotals(2) + gameRatings!(I, I1, 10)
                statTotals(3) = statTotals(3) + gameRatings!(I, I1, 9)
                statTotals(4) = statTotals(4) + gameRatings!(I, I1, 11)
                statTotals(5) = statTotals(5) + (gameRatings!(I, I1, 8) * 2) + gameRatings!(I, I1, 10) + threeFG(I, I1, 0)
                statTotals(13) = statTotals(13) + threeFG(I, I1, 0)
                statTotals(14) = statTotals(14) + threeFG(I, I1, 1)
                statTotals(6) = totalFouls(I)
                statTotals(7) = statTotals(7) + gameRatings!(I, I1, 14)
                statTotals(8) = statTotals(8) + gameRatings!(I, I1, 15)
                statTotals(9) = statTotals(9) + gameRatings!(I, I1, 13)
                statTotals(11) = statTotals(11) + gameRatings!(I, I1, 24)
                statTotals(12) = statTotals(12) + gameRatings!(I, I1, 23)
                statTotals(10) = statTotals(10) + timePlayed(I, I1)

            End If

        Next I1

        If statTotals(3) = 0 Then statTotals(3) = 1

        Print
        Color 14
        Print "TOTALS";

        Color 15
        'Minutes
        Print Tab(14); Int(statTotals(10) / 60);

        'FGM / FGA
        Locate , 21: Print Using "##"; statTotals(0);
        Print "-";
        Print Using "###"; statTotals(1);

        '3FGM / 3FGA
        Locate , 28: Print Using "##"; statTotals(13);
        Print "-";
        Print Using "##"; statTotals(14);

        'FTM / FTA
        Locate , 35: Print Using "##"; statTotals(2);
        Print "-";
        Print Using "##"; statTotals(3);

        'ORB / RB / PT / F / A / B / S / TO
        Locate , 42: Print Using "##  ## ### ##  ##  ##  ##  ##"; statTotals(11); statTotals(4); statTotals(5); statTotals(6); statTotals(7); statTotals(8); statTotals(9); statTotals(12)

        'FG %
        Print Tab(22);
        If statTotals(1) > 0 Then Print CInt(((statTotals(0) / statTotals(1)) * 100)); "%";

        '3FG %
        Print Tab(28);
        If statTotals(14) > 0 Then Print CInt(((statTotals(13) / statTotals(14)) * 100)); "%";

        'FT %
        Print Tab(35);
        If statTotals(3) > 0 Then Print CInt(((statTotals(2) / statTotals(3)) * 100)); "%"

        Print
        Color 14: Print "TOTAL TURNOVERS -";
        Color 15: Print turnovers(I)

        Color 14: Print "TOTAL REBOUNDS  -";
        Color 15: Print totRebounds(I)

        If Not (autoPlay = 1 And playerMode = 2) Then
            Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
            I$ = GetKeyPress$
        End If

    Next I

End Sub


'----------------------------------------
'           BOX2SPC Subroutine
'----------------------------------------
' Adds special event information
' to a boxscore file
Sub BOX2SPC (boxName$)

    Shared eventSettings()

    Open diskPaths$(1) + boxName$ For Output As #1

    Print #1,

    'Output any special events
    If quarter - 1 > eventSettings(1) Then
        Print #1, Using "MORE THAN ## QUARTERS"; eventSettings(1)
    End If

    If score(1, 0) + score(0, 0) > eventSettings(2) Then
        Print #1, Using "MORE THAN ### COMBINED POINTS"; eventSettings(2)
    End If

    If totRebounds(0) + totRebounds(1) > eventSettings(3) Then
        Print #1, Using "MORE THAN ### COMBINED REBOUNDS"; eventSettings(3)
    End If

    If score(0, 0) > eventSettings(4) Then
        Print #1, gameTeams$(0);
        Print #1, Using " SCORED MORE THAN ### POINTS"; eventSettings(4)
    End If

    If score(1, 0) > eventSettings(4) Then
        Print #1, gameTeams$(1);
        Print #1, Using " SCORED MORE THAN ### POINTS"; eventSettings(4)
    End If

    If totRebounds(0) > eventSettings(5) Then
        Print #1, gameTeams$(0);
        Print #1, Using " HAD MORE THAN ### REBOUNDS"; eventSettings(5)
    End If

    If totRebounds(1) > eventSettings(5) Then
        Print #1, gameTeams$(1);
        Print #1, Using " HAD MORE THAN ### REBOUNDS"; eventSettings(5)
    End If

    For I = 0 To 1

        For I1 = 0 To 14

            If gameRatings!(I, I1, 8) * 2 + gameRatings!(I, I1, 10) + threeFG(I, I1, 0) > eventSettings(6) Then
                Print #1, players_GAME$(I, I1, 0);
                Print #1, Using " SCORED MORE THAN ### POINTS"; eventSettings(6)
            End If

            If gameRatings!(I, I1, 11) > eventSettings(7) Then
                Print #1, players_GAME$(I, I1, 0);
                Print #1, Using " HAD MORE THAN ### REBOUNDS"; eventSettings(7)
            End If

            If gameRatings!(I, I1, 14) > eventSettings(8) Then
                Print #1, players_GAME$(I, I1, 0);
                Print #1, Using " HAD MORE THAN ### ASSISTS"; eventSettings(8)
            End If

            If gameRatings!(I, I1, 15) > eventSettings(9) Then
                Print #1, players_GAME$(I, I1, 0);
                Print #1, Using " BLOCKED MORE THAN ### SHOTS"; eventSettings(9)
            End If

            If gameRatings!(I, I1, 13) > eventSettings(10) Then
                Print #1, players_GAME$(I, I1, 0);
                Print #1, Using " HAD MORE THAN ### STEALS"; eventSettings(10)
            End If

            If threeFG(I, I1, 0) > eventSettings(11) Then
                Print #1, players_GAME$(I, I1, 0);
                Print #1, Using " MADE MORE THAN ### 3-PT FIELD GOALS"; eventSettings(11)
            End If

        Next

    Next

    Print #1,

    Close #1

End Sub



'----------------------------------------
'          SAVESTATS Subroutine
'----------------------------------------
'This subroutine prompts the user in ORDER
'to determine which teams to save satts For
'(autoplay will always save both teams).
'It also updates the GAME82 progress file,
'since at this point only can the game be
'considered fully complete.
Sub SAVESTATS ()

    Shared scheduleFile$

    Cls

    If autoPlay <> 1 Then

        Do
            Call TEAMBOX(1, 1)
            pbpString$ = "SAVE STATS FOR EITHER TEAM? (Y/N)"
            Locate 7, (80 - Len(pbpString$)) / 2
            Print pbpString$
            Color 15, 0
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        If UCase$(I$) = "Y" Then
            Cls
            Color 12: Print Tab(30); "COMPILATION OF STATS"
            Print
            Color 14: Print "(0) ";: Color 15: Print "BOTH TEAMS"
            Print
            Color 14: Print "(1) ";: Color teamRat_GAME(0, 33), teamRat_GAME(0, 34): Print gameTeams$(0); " ONLY"
            Color , 0: Print
            Color 14: Print "(2) ";: Color teamRat_GAME(1, 33), teamRat_GAME(1, 34): Print gameTeams$(1); " ONLY"
            Color , 0: Print

            Do
                J$ = GetKeyPress$
                tmStatOpt = Val(J$)
            Loop Until tmStatOpt >= 0 And tmStatOpt <= 2

        End If

    End If

    ' Update schedule, ticker, etc.
    If UCase$(I$) = "Y" Or autoPlay = 1 Then

        If _FileExists("GAME82") Then

            Call TickerSchedule

            If schedGame(1) < schedGame(0) + schedGame(2) Then
                Kill "GAME82"
                endAllGames = 1
            Else
                Open "GAME82" For Output As #2

                Print #2, scheduleFile$

                For I = 0 To 2:
                    Print #2, schedGame(I)
                Next

                Close #2

            End If

        End If

        Call COMPILESTATFILES(tmStatOpt)

    End If

End Sub


'----------------------------------------
'      COMPILESTATFILES Subroutine
'----------------------------------------
'This subroutine updates the various stat
'files for the corresponding team based on
'the stats generated for a completed game.
Sub COMPILESTATFILES (compileOpt)

    'Somehow related to FTM???
    Dim TG!

    Shared actualAttendance&

    Shared gameAttendance&()

    Shared teamScore(), oppScore()

    Shared game3FGM!(), game3FGA!(), gameStatPlyrVal!()
    Shared gameStat3FGM(), gameStat3FGA()
    Shared statsOffTot!(), statsDefTot!()

    Shared gameSite$(), locIndicator$(), oppName$()

    Cls

    For PX = 0 To 1

        For J = 0 To 14
            gameRatings!(PX, J, 16) = timePlayed(PX, J)
            gameRatings!(PX, J, 17) = gameRatings!(PX, J, 23)
            gameRatings!(PX, J, 18) = gameRatings!(PX, J, 24)
        Next

    Next

    For PX = 0 To 1

        For I = 0 To 14

            For J = 1 To 12
                gameRatings!(PX, I, J) = gameRatings!(PX, I, J + 6)
            Next

            game3FGM!(PX, I) = threeFG(PX, I, 0)
            game3FGA!(PX, I) = threeFG(PX, I, 1)

        Next

    Next

    teamIdx = 0

    Do

        dontCompile = 0

        If autoPlay = 0 Then

            'If player is Visiting team but we chose Home | player is Home but we chose Visitor
            If compileOpt = 1 And teamIdx = 1 Or compileOpt = 2 And teamIdx = 0 Then
                I$ = "N"
            Else
                I$ = "Y"
            End If
            
            If UCase$(I$) = "N" Then
                dontCompile = 1
            Else
                Cls
            End If

        End If

        If dontCompile = 0 Then

            statFile$ = nameStatFile$(gameTeams$(teamIdx))

            If _FileExists(diskPaths$(1) + statFile$) Then

                Open diskPaths$(1) + statFile$ For Input As #1

                Input #1, gameTeams$(teamIdx)

                For I = 0 To 14:

                    Input #1, players_GAME$(teamIdx, I, 0)

                    For J = 0 To 13:
                        Input #1, gameStatPlyrVal!(I, J)
                    Next J

                    Input #1, gameStat3FGM(teamIdx, I)
                    Input #1, gameStat3FGA(teamIdx, I)

                Next I

                For L = 0 To 15:
                    Input #1, statsOffTot!(L)
                Next L

                For L = 0 To 15:
                    Input #1, statsDefTot!(L)
                Next L

                Close #1

            End If

            TG! = 0

            For I = 0 To 14

                If gameRatings!(teamIdx, I, 2) * 2 + gameRatings!(teamIdx, I, 4) + game3FGM!(teamIdx, I) > gameStatPlyrVal!(I, 13) Then
                    gameStatPlyrVal!(I, 13) = gameRatings!(teamIdx, I, 2) * 2 + gameRatings!(teamIdx, I, 4) + game3FGM!(teamIdx, I)
                End If

                If gameRatings!(teamIdx, I, 10) > 0 Then
                    gameStatPlyrVal!(I, 0) = gameStatPlyrVal!(I, 0) + 1
                End If

                For J = 1 To 12

                    gameStatPlyrVal!(I, J) = gameStatPlyrVal!(I, J) + gameRatings!(teamIdx, I, J)

                    If Not (J = 5 Or J = 6 Or J = 10 Or J = 11) Then
                        statsOffTot!(J) = statsOffTot!(J) + gameRatings!(teamIdx, I, J)
                        statsDefTot!(J) = statsDefTot!(J) + gameRatings!(1 - teamIdx, I, J)
                    End If
                Next J

                statsOffTot!(0) = statsOffTot!(0) + gameRatings!(teamIdx, I, 10)
                TG! = TG! + gameRatings!(teamIdx, I, 10)

            Next I

            Erase statTotals

            For I = 0 To 1
                For I1 = 0 To 14
                    statTotals(I) = statTotals(I) + gameRatings!(I, I1, 5)
                Next
            Next

            statsOffTot!(15) = statsOffTot!(15) + totRebounds(teamIdx) - statTotals(teamIdx)
            statsDefTot!(15) = statsDefTot!(15) + totRebounds(1 - teamIdx) - statTotals(1 - teamIdx)

            statsOffTot!(6) = statsOffTot!(6) + totalFouls(teamIdx)
            statsDefTot!(6) = statsDefTot!(6) + totalFouls(1 - teamIdx)

            statsOffTot!(5) = statsOffTot!(5) + totRebounds(teamIdx)
            statsDefTot!(5) = statsDefTot!(5) + totRebounds(1 - teamIdx)

            statsOffTot!(11) = statsOffTot!(11) + turnovers(teamIdx)
            statsDefTot!(11) = statsDefTot!(11) + turnovers(1 - teamIdx)

            For J = 0 To 14:
                statsOffTot!(13) = statsOffTot!(13) + game3FGM!(teamIdx, J)
                statsOffTot!(14) = statsOffTot!(14) + game3FGA!(teamIdx, J)
                statsDefTot!(13) = statsDefTot!(13) + game3FGM!(1 - teamIdx, J)
                statsDefTot!(14) = statsDefTot!(14) + game3FGA!(1 - teamIdx, J)
                gameStat3FGM(teamIdx, J) = gameStat3FGM(teamIdx, J) + game3FGM!(teamIdx, J)
                gameStat3FGA(teamIdx, J) = gameStat3FGA(teamIdx, J) + game3FGA!(teamIdx, J)
            Next J

            statsOffTot!(10) = statsOffTot!(10) + score(teamIdx, 0)
            statsDefTot!(10) = statsDefTot!(10) + score(1 - teamIdx, 0)

            Call TEAMBOX(1, teamIdx)

            pbpString$ = "SAVING STATS FOR " + gameTeams$(teamIdx)
            Locate , (80 - Len(pbpString$)) / 2
            Print pbpString$
            Color 15, 0

            Open diskPaths$(1) + statFile$ For Output As #2

            Print #2, gameTeams$(teamIdx)

            For I = 0 To 14:

                Print #2, players_GAME$(teamIdx, I, 0)

                For J = 0 To 13:
                    Print #2, gameStatPlyrVal!(I, J)
                Next J

                Print #2, gameStat3FGM(teamIdx, I)
                Print #2, gameStat3FGA(teamIdx, I)

            Next I

            For L = 0 To 15
                Print #2, statsOffTot!(L)
            Next L

            For L = 0 To 15:
                Print #2, statsDefTot!(L)
            Next L

            Close #2

            numberGames = 0

            If _FileExists(diskPaths$(1) + statFile$ + "S") Then
                Call ReadScheduleStats(statFile$, numberGames)
            End If

            If numberGames < MAX_SCHED_STATS Then
                numberGames = numberGames + 1
            End If

            Select Case gameLoc

                Case 2
                    Select Case TG!
                        Case Is < 14500
                            locIndicator$(numberGames) = "N"
                        Case 14501 To 16000
                            locIndicator$(numberGames) = "N1"
                        Case 16001 To 17500
                            locIndicator$(numberGames) = "N2"
                        Case 17501 To 19000
                            locIndicator$(numberGames) = "N3"
                        Case 19001 To 20500
                            locIndicator$(numberGames) = "N4"
                    End Select

                Case teamIdx
                    Select Case TG!
                        Case Is < 14500
                            locIndicator$(numberGames) = "H"
                        Case 14501 To 16000
                            locIndicator$(numberGames) = "H1"
                        Case 16001 To 17500
                            locIndicator$(numberGames) = "H2"
                        Case 17501 To 19000
                            locIndicator$(numberGames) = "H3"
                        Case 19001 To 20500
                            locIndicator$(numberGames) = "H4"
                    End Select

                Case (1 - teamIdx)
                    Select Case TG!
                        Case Is < 14500
                            locIndicator$(numberGames) = "V"
                        Case 14501 To 16000
                            locIndicator$(numberGames) = "V1"
                        Case 16001 To 17500
                            locIndicator$(numberGames) = "V2"
                        Case 17501 To 19000
                            locIndicator$(numberGames) = "V3"
                        Case 19001 To 20500
                            locIndicator$(numberGames) = "V4"
                    End Select

            End Select

            teamScore(numberGames) = score(teamIdx, 0)
            oppName$(numberGames) = gameTeams$(1 - teamIdx)
            oppScore(numberGames) = score(1 - teamIdx, 0)
            gameAttendance&(numberGames) = actualAttendance&

            If gameArena$(1) = "" Then
                gameArena$(1) = "MEMORIAL COLISEUM"
            End If

            If gameLoc = 2 Then
                gameSite$(numberGames) = "NEUTRAL SITE"
            Else
                gameSite$(numberGames) = gameArena$(1)
            End If

            Call SaveScheduleStats(statFile$, numberGames)

            'Reset arrays for next iteration
            Erase teamScore, oppScore
            Erase gameAttendance&
            Erase statsOffTot!, statsDefTot!, gameStatPlyrVal!
            Erase gameSite$, locIndicator$, oppName$

        End If

        teamIdx = teamIdx + 1
        statsOffTot!(12) = 0

    Loop Until teamIdx = 2

End Sub


'----------------------------------------
'            pbplog Routine
'----------------------------------------
'This subroutine is designed to help me
'print information about where the game
'is at during the code. The general idea
'was originally to be called everywhere
'there is PBP. This helps me debug where
'issues are happening in a game.
Sub pbplog (lineNumber)

    Print #11, "Called from line # "; lineNumber
    Print #11, gameTeams$(0); " vs "; gameTeams$(1)
    'Print #11, pbpString$

    'Print #11, currGame; ","; ballCarrier; ","; lineNumber
    'Print #11, lineNumber
    'Print #11, lineNumber, ballCarrier
    'Print #11, defStyles(P9)
    'Print #11,

End Sub


'----------------------------------------
'       ChooseGameTeam Subroutine
'----------------------------------------
'This subroutine provides interface for
'choosing a team for the game
Sub ChooseGameTeam (yearNumber$, numberTeams, chosenIdx, teamName$, gameIdx)

    Shared previousYear$

    Shared teamIndicator$()

    Do

        chosenIdx = 0

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 4)

        Locate 7, 32

        Locate , 12: Print teamIndicator$(gameIdx)
        Locate , 12: Print "ENTER DISK ID: "
        Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print "ABA SEASONS ARE FOLLOWED BY AN 'A' (IE. 75A)."
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 8,

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ <> Chr$(13) Then

                Locate 8, 27: Print I$;

                NY$ = ""

                Do Until NY$ = Chr$(13)

                    NY$ = GetKeyPress$

                    If NY$ = Chr$(8) And Len(I$) >= 1 Then
                        col% = Pos(0)
                        I$ = Left$(I$, (Len(I$) - 1))
                        Locate , col% - 1
                        Print " ";
                        Locate , col% - 1
                    Else
                        I$ = I$ + NY$
                        Print NY$;
                    End If

                Loop

                I$ = Left$(I$, (Len(I$) - 1))

                If Not (I$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(I$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        MsgText$ = "NO TEAMS WERE FOUND MATCHING THAT ID... PRESS ANY KEY TO SELECT ANOTHER ID"
                        BoxWidth = 20
                        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                        Call CSMsgBox(MsgText$, BoxWidth, PASS)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, PASS)
                        Exit Sub

                    Else

                        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

                        fileLength& = LOF(1)

                        Field #1, 15 As Q$(0)

                        numberTeams = fileLength& / TEAM_SIZE_BYTES

                        For teamIdx = 1 To numberTeams
                            Get #1, teamIdx
                            'numberTeams = numberTeams + 1
                            teamNames$(teamIdx) = Q$(0)
                            teamIndex(teamIdx) = teamIdx
                        Next teamIdx

                        Close #1

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

                previousYear$ = yearNumber$

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectGameTeam(numberTeams, yearNumber$, chosenIdx, teamName$, ExitCode%, gameIdx)
                teamGender$(gameIdx) = GetTeamGender$(yearNumber$)

                If ExitCode% <> 27 And numberTeams <> 0 And chosenIdx > 0 Then

                    Do

                        YCoord = Int((80 - Len(teamName$)) / 2)
                        Locate 2, YCoord
                        Color teamRat_GAME(gameIdx, 33), teamRat_GAME(gameIdx, 34): Print teamName$ 'gameTeams$(gameIdx)

                        Color 15, 4
                        Locate 3, 32: Print "ANY CHANGE? (Y/N)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

                'chosenIdx = -1 means there was invalid data
            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N" Or chosenIdx = -1

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    'If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'       SelectGameTeam Subroutine
'----------------------------------------
'This subroutine provides an interface for
'selecting a team from the list loaded by
'the disk ID.
Sub SelectGameTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, gameIdx)

    If numberTeams > 0 Then

        SELECTION% = 1
        Count% = numberTeams
        Start% = 1
        FileFGColor = 15: FileBGColor = 4
        HiLiteFGColor = 0: HiLiteBGColor = 7
        Rows = 20
        Columns% = 4
        Spacing% = 6
        Row% = 4
        Column% = 1

        Color , 4
        Cls
        Color 14
        Locate , 33: Print "TEAM SELECTION"
        Locate 25, 1
        Color 14: Print "ESC";
        Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";
        Color 14: Print "ENTER";
        Color 15: Print "-SELECTS TEAM";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor, HiLiteBGColor, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex(SELECTION%)

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Call ReadGameTeam(yearNumber$, teamName$, selectedIdx, gameIdx, validData)

        End If

    End If

    If validData = 0 And ExitCode% <> 27 Then

        selectedIdx = -1
        Locate 9,
        Call CSMsgBox("THERE WAS AN ISSUE READING TEAM DATA... PRESS ANY KEY AND TRY ANOTHER DISK ID", 20, PASS)
        K$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    End If

End Sub


'----------------------------------------
'        ReadGameTeam Subroutine
'----------------------------------------
'This subroutine reads in the data for the
'selected tema into each of the data arrays
'needed by the game.
Sub ReadGameTeam (teamYear$, targetTeam$, teamIdx, gameIdx, dataOK)

    Shared CK

    Dim gamesPlayed, minsPerGame!

    Shared avgAttendance&()

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    Field #1, 15 As Q$(0), 15 As Q$(1)

    For J = 0 To 6
        Field #1, 30 + J * 2 As X$, 2 As Q$(J + 2)
    Next

    Field #1, 44 As X$, 2 As Q$(9)

    For J = 0 To 34
        Field #1, 46 + J * 2 As X$, 2 As Q$(J + 10)
    Next

    For J = 0 To 14
        Field #1, 116 + J * 57 As X$, 15 As Q$(J * 22 + 45), 2 As Q$(J * 22 + 46)
        For J1 = 0 To 19
            Field #1, 133 + J * 57 + J1 * 2 As X$, 2 As Q$(J * 22 + 47 + J1)
        Next
    Next

    Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377), 4 As Q$(378)

    Get #1, teamIdx

    targetTeam$ = RTrim$(Q$(0))
    gameMascots$(gameIdx) = Q$(1)

    For I = 0 To 6
        leagRat_GAME(gameIdx, I) = CVI(Q$(I + 2))
    Next

    CK = CVI(Q$(9))

    For I = 0 To 34
        teamRat_GAME(gameIdx, I) = CVI(Q$(I + 10))
    Next

    For I = 0 To 14

        players_GAME$(gameIdx, I, 0) = RTrim$(Q$(22 * I + 45))
        players_GAME$(gameIdx, I, 1) = RTrim$(Q$(22 * I + 46))

        For I1 = 0 To 19
            plyrRat_GAME(gameIdx, I, I1) = CVI(Q$(22 * I + 47 + I1))
        Next

    Next

    'Team Abbreviation
    teamAbbrev$(gameIdx) = RTrim$(Q$(375))

    'Coach
    gameCoach$(gameIdx) = RTrim$(Q$(376))

    'Arena
    gameArena$(gameIdx) = RTrim$(Q$(377))

    'Attendance
    avgAttendance&(gameIdx) = CVL(Q$(378))

    Close #1

    test$ = gameArena$(gameIdx)

    If (CK <> 999) Then

        dataOK = 0

    Else

        For I = 0 To 14

            If plyrRat_GAME(gameIdx, I, 16) < 0 And plyrRat_GAME(gameIdx, I, 17) < 0 Then

                gamesPlayed = plyrRat_GAME(gameIdx, I, 16) / plyrRat_GAME(gameIdx, I, 0)
                minsPerGame! = plyrRat_GAME(gameIdx, I, 17) / plyrRat_GAME(gameIdx, I, 1)

                For I1 = 2 To 14
                    plyrRat_GAME(gameIdx, I, I1) = plyrRat_GAME(gameIdx, I, I1) * gamesPlayed * minsPerGame!
                    plyrRat_GAME(gameIdx, I, 0) = plyrRat_GAME(gameIdx, I, 16) * -1: plyrRat_GAME(gameIdx, I, 1) = plyrRat_GAME(gameIdx, I, 17) * -1
                Next I1

            End If

        Next I

        If Not (playerMode = 0 Or playerMode = 1 And gameIdx <> compTeam Or autoPlay = 1 And playerMode = 0 Or autoPlay = 1 And playerMode = 1 And gameIdx <> compTeam) Then

            While (CNT% < 10)

                CNT% = 0

                For I = 0 To 14
                    If plyrRat_GAME(gameIdx, I, 18) = -1 Then plyrRat_GAME(gameIdx, I, 18) = 0

                    Call Rolld100(RN)

                    'Auto disable specific players
                    If UCase$(players_GAME$(gameIdx, I, 0)) = "XXX" Or (plyrRat_GAME(gameIdx, I, 18) = 0 And RN > CInt(plyrRat_GAME(gameIdx, I, 0) / leagRat_GAME(gameIdx, 0) * 100)) Then
                        plyrRat_GAME(gameIdx, I, 18) = -1
                    End If

                Next

                For I = 0 To 14:
                    If plyrRat_GAME(gameIdx, I, 18) = 0 Then CNT% = CNT% + 1
                Next

            Wend

        End If

        dataOK = 1

    End If

End Sub


'----------------------------------------
'      GetKeyWhenNoDelay Subroutine
'----------------------------------------
'Allows user to silently press a key
'(no prompt) when the game is not autoplay
Sub GetKeyWhenNoDelay ()

    'If we're not in autoplay
    If Not (autoPlay = 1 And playerMode = 2) Then
        I$ = GetKeyPress$
    End If

End Sub

'----------------------------------------
'          EndOfGame Subroutine
'----------------------------------------
'This subroutine serves as the final portion
'of a post game routine. At this point,
'stats have been saved and we are simply
'looking to determine whether a new game
'should be started.
Sub EndOfGame

    Call POSTGAME

    If autoPlay <> 1 Then

        Cls

        'Using Home Team's color for the box
        Call TEAMBOX(1, 1)

        Do
            pbpString$ = "PLAY ANOTHER GAME? (Y/N)"
            Locate 7, (80 - Len(pbpString$)) / 2
            Print pbpString$

            Color 15, 0
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    End If

    'Close #11

    If UCase$(I$) = "Y" Or (autoPlay = 1 And endAllGames <> 1) Then

        Cls
        Print "PLEASE WAIT ..."
        Call PROHOOP

    End If

End Sub


'----------------------------------------
'        CheckRecords Subroutine
'----------------------------------------
'This subroutine checks all stats to see
'if any records were broken. Any that have
'been reached will be assigned to arrays
'for later updating the files.
Sub CheckRecords ()

    Shared indRecDesc$(), teamRecDesc$()
    Shared indRecords!(), teamRecords!()

    Dim V8(1, 14)
    Dim VERSUS$(1)

    VERSUS$(0) = "AT"
    VERSUS$(1) = "VS."

    If _FileExists(diskPaths$(1) + "RECORDS.GAM") Then

        Open diskPaths$(1) + "RECORDS.GAM" For Input As #2

        For I = 0 To 48
            For I1 = 0 To 2
                Input #2, indRecords!(I, I1)
            Next
            For I1 = 0 To 4
                Input #2, indRecDesc$(I, I1)
            Next
        Next

        For I = 0 To 125
            For I1 = 0 To 2
                Input #2, teamRecords!(I, I1)
            Next
            For I1 = 0 To 3
                Input #2, teamRecDesc$(I, I1)
            Next
        Next

        Close #2
    Else

        teamRecords!(2, 0) = 999
        teamRecords!(4, 0) = 999
        teamRecords!(7, 0) = 999
        teamRecords!(9, 0) = 999
        teamRecords!(11, 0) = 999
        teamRecords!(13, 0) = 999
        teamRecords!(15, 0) = 999
        teamRecords!(17, 0) = 999
        teamRecords!(19, 0) = 999
        teamRecords!(21, 0) = 999
        teamRecords!(23, 0) = 999
        teamRecords!(25, 0) = 999
        teamRecords!(27, 0) = 999
        teamRecords!(29, 0) = 999
        teamRecords!(31, 0) = 999
        teamRecords!(33, 0) = 999
        teamRecords!(35, 0) = 999
        teamRecords!(37, 0) = 999
        teamRecords!(43, 0) = 999
        teamRecords!(45, 0) = 999
        teamRecords!(61, 0) = 999
        teamRecords!(63, 0) = 999
        teamRecords!(65, 0) = 999
        teamRecords!(67, 0) = 999
        teamRecords!(73, 0) = 999
        teamRecords!(75, 0) = 999
        teamRecords!(81, 0) = 999
        teamRecords!(83, 0) = 999
        teamRecords!(87, 0) = 999
        teamRecords!(89, 0) = 999
        teamRecords!(92, 0) = 999
        teamRecords!(94, 0) = 999
        teamRecords!(97, 0) = 999
        teamRecords!(99, 0) = 999
        teamRecords!(105, 0) = 999
        teamRecords!(107, 0) = 999
        teamRecords!(113, 0) = 999
        teamRecords!(115, 0) = 999
        teamRecords!(120, 0) = 999
        teamRecords!(123, 0) = 999
        teamRecords!(125, 0) = 999

    End If

    'CHECK THE INDIVIDUAL RECORDS
    For I = 0 To 1

        For I1 = 0 To 14

            ' SCORING RECORDS FOR POINTS IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If gameRatings!(I, I1, 8) * 2 + gameRatings!(I, I1, 10) + threeFG(I, I1, 0) > indRecords!(0, 0) Then
                indRecords!(0, 0) = gameRatings!(I, I1, 8) * 2 + gameRatings!(I, I1, 10) + threeFG(I, I1, 0) ' POINTS
                indRecords!(0, 1) = 1 ' TIMES DONE
                indRecords!(0, 2) = 0 ' GAME
                indRecDesc$(0, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(0, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(0, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(0, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(0, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 13) + QQ(I, 2, I1, 13) > indRecords!(1, 0) Then
                indRecords!(1, 0) = QQ(I, 1, I1, 13) + QQ(I, 2, I1, 13) ' POINTS
                indRecords!(1, 1) = 1 ' TIMES DONE
                indRecords!(1, 2) = 1 ' HALF
                indRecDesc$(1, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(1, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(1, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(1, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(1, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 13) + QQ(I, 4, I1, 13) > indRecords!(1, 0) Then
                indRecords!(1, 0) = QQ(I, 3, I1, 13) + QQ(I, 4, I1, 13) ' POINTS
                indRecords!(1, 1) = 1 ' TIMES DONE
                indRecords!(1, 2) = 2 ' HALF
                indRecDesc$(1, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(1, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(1, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(1, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(1, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 13) > indRecords!(2, 0) Then
                    indRecords!(2, 0) = QQ(I, I2, I1, 13) ' POINTS
                    indRecords!(2, 1) = 1 ' TIMES DONE
                    indRecords!(2, 2) = I2 ' QTR
                    indRecDesc$(2, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(2, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(2, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(2, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(2, 4) = Date$ ' DATE
                End If
            Next I2

            For I2 = 5 To 8
                If QQ(I, I2, I1, 13) > indRecords!(3, 0) Then
                    indRecords!(3, 0) = QQ(I, I2, I1, 13) ' POINTS
                    indRecords!(3, 1) = 1 ' TIMES DONE
                    indRecords!(3, 2) = I2 ' QTR
                    indRecDesc$(3, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(3, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(3, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(3, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(3, 4) = Date$ ' DATE
                End If
            Next I2

            ' FIELD GOAL PERCENTAGE RECORDS FOR HIGH %, MOST W/NO MISSES, MOST W/NONE MADE
            If gameRatings!(I, I1, 8) >= 15 Then
                If gameRatings!(I, I1, 8) / gameRatings!(I, I1, 7) > indRecords!(4, 0) Then
                    indRecords!(4, 0) = gameRatings!(I, I1, 8) / gameRatings!(I, I1, 7) ' PCTG
                    indRecords!(4, 1) = gameRatings!(I, I1, 8) ' FGM
                    indRecords!(4, 2) = gameRatings!(I, I1, 7) ' FGA
                    indRecDesc$(4, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(4, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(4, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(4, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(4, 4) = Date$ ' DATE
                End If
            End If

            If gameRatings!(I, I1, 8) = gameRatings!(I, I1, 7) And gameRatings!(I, I1, 7) > indRecords!(5, 0) Then
                indRecords!(5, 0) = gameRatings!(I, I1, 7) ' MOST FGM
                indRecDesc$(5, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(5, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(5, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(5, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(5, 4) = Date$ ' DATE
            End If

            If gameRatings!(I, I1, 8) = 0 And gameRatings!(I, I1, 7) > indRecords!(6, 0) Then
                indRecords!(6, 0) = gameRatings!(I, I1, 7) ' 0 FGM /FGA
                indRecDesc$(6, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(6, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(6, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(6, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(6, 4) = Date$ ' DATE
            End If

            ' FIELD GOALS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If gameRatings!(I, I1, 8) > indRecords!(7, 0) Then
                indRecords!(7, 0) = gameRatings!(I, I1, 8) ' FGM
                indRecords!(7, 1) = 1 ' TIMES DONE
                indRecords!(7, 2) = 0 ' GAME
                indRecDesc$(7, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(7, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(7, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(7, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(7, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 2) + QQ(I, 2, I1, 2) > indRecords!(8, 0) Then
                indRecords!(8, 0) = QQ(I, 1, I1, 2) + QQ(I, 2, I1, 2) ' FGM
                indRecords!(8, 1) = 1 ' TIMES DONE
                indRecords!(8, 2) = 1 ' HALF
                indRecDesc$(8, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(8, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(8, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(8, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(8, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 2) + QQ(I, 4, I1, 2) > indRecords!(8, 0) Then
                indRecords!(8, 0) = QQ(I, 3, I1, 2) + QQ(I, 4, I1, 2) ' FGM
                indRecords!(8, 1) = 1 ' TIMES DONE
                indRecords!(8, 2) = 2 ' HALF
                indRecDesc$(8, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(8, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(8, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(8, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(8, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 2) > indRecords!(9, 0) Then
                    indRecords!(9, 0) = QQ(I, I2, I1, 2) ' FGM
                    indRecords!(9, 1) = 1 ' TIMES DONE
                    indRecords!(9, 2) = I2 ' QTR
                    indRecDesc$(9, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(9, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(9, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(9, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(9, 4) = Date$ ' DATE
                End If
            Next I2

            For I2 = 5 To 8
                If QQ(I, I2, I1, 2) > indRecords!(10, 0) Then
                    indRecords!(10, 0) = QQ(I, I2, I1, 2) ' FGM
                    indRecords!(10, 1) = 1 ' TIMES DONE
                    indRecords!(10, 2) = I2 ' QTR
                    indRecDesc$(10, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(10, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(10, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(10, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(10, 4) = Date$ ' DATE
                End If
            Next I2

            ' FIELD GOAL ATTEMPTS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If gameRatings!(I, I1, 7) > indRecords!(11, 0) Then
                indRecords!(11, 0) = gameRatings!(I, I1, 7) ' FGA
                indRecords!(11, 1) = 1 ' TIMES DONE
                indRecords!(11, 2) = 0 ' GAME
                indRecDesc$(11, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(11, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(11, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(11, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(11, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 1) + QQ(I, 2, I1, 1) > indRecords!(12, 0) Then
                indRecords!(12, 0) = QQ(I, 1, I1, 1) + QQ(I, 2, I1, 1) ' FGA
                indRecords!(12, 1) = 1 ' TIMES DONE
                indRecords!(12, 2) = 1 ' HALF
                indRecDesc$(12, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(12, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(12, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(12, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(12, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 1) + QQ(I, 4, I1, 1) > indRecords!(12, 0) Then
                indRecords!(12, 0) = QQ(I, 3, I1, 1) + QQ(I, 4, I1, 1) ' FGA
                indRecords!(12, 1) = 1 ' TIMES DONE
                indRecords!(12, 2) = 2 ' HALF
                indRecDesc$(12, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(12, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(12, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(12, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(12, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 1) > indRecords!(13, 0) Then
                    indRecords!(13, 0) = QQ(I, I2, I1, 1) ' FGA
                    indRecords!(13, 1) = 1 ' TIMES DONE
                    indRecords!(13, 2) = I2 ' QTR
                    indRecDesc$(13, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(13, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(13, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(13, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(13, 4) = Date$ ' DATE
                End If
            Next I2

            For I2 = 5 To 8
                If QQ(I, I2, I1, 1) > indRecords!(14, 0) Then
                    indRecords!(14, 0) = QQ(I, I2, I1, 1) ' FGA
                    indRecords!(14, 1) = 1 ' TIMES DONE
                    indRecords!(14, 2) = I2 ' QTR
                    indRecDesc$(14, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(14, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(14, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(14, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(14, 4) = Date$ ' DATE
                End If
            Next I2

            ' THREE-POINT FIELD GOALS MOST MADE NO MISSES, MOST ATT NONE MADE
            If threeFG(I, I1, 0) = threeFG(I, I1, 1) And threeFG(I, I1, 0) > indRecords!(15, 0) Then
                indRecords!(15, 0) = threeFG(I, I1, 0) ' MOST 3-FGM
                indRecDesc$(15, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(15, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(15, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(15, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(15, 4) = Date$ ' DATE
            End If

            If threeFG(I, I1, 0) = 0 And threeFG(I, I1, 1) > indRecords!(16, 0) Then
                indRecords!(16, 0) = threeFG(I, I1, 1) ' 0 3FGM /3FGA
                indRecDesc$(16, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(16, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(16, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(16, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(16, 4) = Date$ ' DATE
            End If

            ' 3-FIELD GOALS MADE IN A GAME, HALF, QUARTER AND OVERTIME PERIOD
            If threeFG(I, I1, 0) > indRecords!(17, 0) Then
                indRecords!(17, 0) = threeFG(I, I1, 0) ' 3FGM
                indRecords!(17, 1) = 1 ' TIMES DONE
                indRecords!(17, 2) = 0 ' GAME
                indRecDesc$(17, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(17, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(17, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(17, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(17, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 10) + QQ(I, 2, I1, 10) > indRecords!(18, 0) Then
                indRecords!(18, 0) = QQ(I, 1, I1, 10) + QQ(I, 2, I1, 10) ' 3FGM
                indRecords!(18, 1) = 1 ' TIMES DONE
                indRecords!(18, 2) = 1 ' HALF
                indRecDesc$(18, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(18, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(18, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(18, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(18, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 10) + QQ(I, 4, I1, 10) > indRecords!(18, 0) Then
                indRecords!(18, 0) = QQ(I, 3, I1, 10) + QQ(I, 4, I1, 10) ' 3FGM
                indRecords!(18, 1) = 1 ' TIMES DONE
                indRecords!(18, 2) = 2 ' HALF
                indRecDesc$(18, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(18, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(18, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(18, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(18, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 10) > indRecords!(19, 0) Then
                    indRecords!(19, 0) = QQ(I, I2, I1, 10) ' 3FGM
                    indRecords!(19, 1) = 1 ' TIMES DONE
                    indRecords!(19, 2) = I2 ' QTR
                    indRecDesc$(19, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(19, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(19, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(19, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(19, 4) = Date$ ' DATE
                End If
            Next I2

            For I2 = 5 To 8
                If QQ(I, I2, I1, 10) > indRecords!(20, 0) Then
                    indRecords!(20, 0) = QQ(I, I2, I1, 10) ' 3FGM
                    indRecords!(20, 1) = 1 ' TIMES DONE
                    indRecords!(20, 2) = I2 ' QTR
                    indRecDesc$(20, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(20, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(20, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(20, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(20, 4) = Date$ ' DATE
                End If
            Next I2

            ' 3-FIELD GOALS ATT IN A GAME, HALF
            If threeFG(I, I1, 1) > indRecords!(21, 0) Then
                indRecords!(21, 0) = threeFG(I, I1, 1) ' 3FGA
                indRecords!(21, 1) = 1 ' TIMES DONE
                indRecords!(21, 2) = 0 ' GAME
                indRecDesc$(21, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(21, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(21, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(21, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(21, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 11) + QQ(I, 2, I1, 11) > indRecords!(22, 0) Then
                indRecords!(22, 0) = QQ(I, 1, I1, 11) + QQ(I, 2, I1, 11) ' 3FGA
                indRecords!(22, 1) = 1 ' TIMES DONE
                indRecords!(22, 2) = 1 ' HALF
                indRecDesc$(22, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(22, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(22, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(22, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(22, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 11) + QQ(I, 4, I1, 11) > indRecords!(22, 0) Then
                indRecords!(22, 0) = QQ(I, 3, I1, 10) + QQ(I, 4, I1, 10) ' 3FGA
                indRecords!(22, 1) = 1 ' TIMES DONE
                indRecords!(22, 2) = 2 ' HALF
                indRecDesc$(22, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(22, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(22, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(22, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(22, 4) = Date$ ' DATE
            End If

            ' FREE THROWS -  MOST MADE NO MISSES, MOST ATT NONE MADE
            If gameRatings!(I, I1, 9) = gameRatings!(I, I1, 10) And gameRatings!(I, I1, 9) > indRecords!(23, 0) Then
                indRecords!(23, 0) = gameRatings!(I, I1, 9) ' MOST FTM
                indRecDesc$(23, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(23, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(23, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(23, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(23, 4) = Date$ ' DATE
            End If

            If gameRatings!(I, I1, 10) = 0 And gameRatings!(I, I1, 9) > indRecords!(24, 0) Then
                indRecords!(24, 0) = gameRatings!(I, I1, 10) ' 0 FTM /FTA
                indRecDesc$(24, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(24, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(24, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(24, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(24, 4) = Date$ ' DATE
            End If

            ' MADE FREE THROWS IN A GAME, HALF, QUARTER
            If gameRatings!(I, I1, 10) > indRecords!(25, 0) Then
                indRecords!(25, 0) = gameRatings!(I, I1, 10) ' FTM
                indRecords!(25, 1) = 1 ' TIMES DONE
                indRecords!(25, 2) = 0 ' GAME
                indRecDesc$(25, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(25, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(25, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(25, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(25, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 4) + QQ(I, 2, I1, 4) > indRecords!(26, 0) Then
                indRecords!(26, 0) = QQ(I, 1, I1, 4) + QQ(I, 2, I1, 4) ' FTM
                indRecords!(26, 1) = 1 ' TIMES DONE
                indRecords!(26, 2) = 1 ' HALF
                indRecDesc$(26, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(26, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(26, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(26, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(26, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 4) + QQ(I, 4, I1, 4) > indRecords!(26, 0) Then
                indRecords!(26, 0) = QQ(I, 3, I1, 4) + QQ(I, 4, I1, 4) ' FTM
                indRecords!(26, 1) = 1 ' TIMES DONE
                indRecords!(26, 2) = 2 ' HALF
                indRecDesc$(26, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(26, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(26, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(26, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(26, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 4) > indRecords!(27, 0) Then
                    indRecords!(27, 0) = QQ(I, I2, I1, 4) ' FTM
                    indRecords!(27, 1) = 1 ' TIMES DONE
                    indRecords!(27, 2) = I2 ' QTR
                    indRecDesc$(27, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(27, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(27, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(27, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(27, 4) = Date$ ' DATE
                End If
            Next I2

            ' FREE THROWS ATTEMPTS IN A GAME, HALF, QUARTER
            If gameRatings!(I, I1, 9) > indRecords!(28, 0) Then
                indRecords!(28, 0) = gameRatings!(I, I1, 9) ' FTA
                indRecords!(28, 1) = 1 ' TIMES DONE
                indRecords!(28, 2) = 0 ' GAME
                indRecDesc$(28, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(28, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(28, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(28, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(28, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 3) + QQ(I, 2, I1, 3) > indRecords!(29, 0) Then
                indRecords!(29, 0) = QQ(I, 1, I1, 3) + QQ(I, 2, I1, 3) ' FTA
                indRecords!(29, 1) = 1 ' TIMES DONE
                indRecords!(29, 2) = 1 ' HALF
                indRecDesc$(29, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(29, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(29, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(29, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(29, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 3) + QQ(I, 4, I1, 3) > indRecords!(29, 0) Then
                indRecords!(29, 0) = QQ(I, 3, I1, 3) + QQ(I, 4, I1, 3) ' FTM
                indRecords!(29, 1) = 1 ' TIMES DONE
                indRecords!(29, 2) = 2 ' HALF
                indRecDesc$(29, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(29, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(29, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(29, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(29, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 3) > indRecords!(30, 0) Then
                    indRecords!(30, 0) = QQ(I, I2, I1, 3) ' FTM
                    indRecords!(30, 1) = 1 ' TIMES DONE
                    indRecords!(30, 2) = I2 ' QTR
                    indRecDesc$(30, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(30, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(30, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(30, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(30, 4) = Date$ ' DATE
                End If
            Next I2

            ' TOTAL REBOUNDS IN A GAME, HALF, QUARTER
            If gameRatings!(I, I1, 11) > indRecords!(31, 0) Then
                indRecords!(31, 0) = gameRatings!(I, I1, 11) ' FTA
                indRecords!(31, 1) = 1 ' TIMES DONE
                indRecords!(31, 2) = 0 ' GAME
                indRecDesc$(31, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(31, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(31, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(31, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(31, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 5) + QQ(I, 2, I1, 5) > indRecords!(32, 0) Then
                indRecords!(32, 0) = QQ(I, 1, I1, 5) + QQ(I, 2, I1, 5) ' RB
                indRecords!(32, 1) = 1 ' TIMES DONE
                indRecords!(32, 2) = 1 ' HALF
                indRecDesc$(32, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(32, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(32, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(32, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(32, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 5) + QQ(I, 4, I1, 5) > indRecords!(32, 0) Then
                indRecords!(32, 0) = QQ(I, 3, I1, 5) + QQ(I, 4, I1, 5) ' RB
                indRecords!(32, 1) = 1 ' TIMES DONE
                indRecords!(32, 2) = 2 ' HALF
                indRecDesc$(32, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(32, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(32, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(32, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(32, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 5) > indRecords!(33, 0) Then
                    indRecords!(33, 0) = QQ(I, I2, I1, 5) ' RB
                    indRecords!(33, 1) = 1 ' TIMES DONE
                    indRecords!(33, 2) = I2 ' QTR
                    indRecDesc$(33, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(33, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(33, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(33, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(33, 4) = Date$ ' DATE
                End If
            Next I2

            ' TOTAL OFFENSIVE REBOUNDS IN A GAME, HALF, QUARTER
            If gameRatings!(I, I1, 24) > indRecords!(34, 0) Then
                indRecords!(34, 0) = gameRatings!(I, I1, 24) ' FTA
                indRecords!(34, 1) = 1 ' TIMES DONE
                indRecords!(34, 2) = 0 ' GAME
                indRecDesc$(34, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(34, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(34, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(34, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(34, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 12) + QQ(I, 2, I1, 12) > indRecords!(35, 0) Then
                indRecords!(35, 0) = QQ(I, 1, I1, 12) + QQ(I, 2, I1, 12) ' RB
                indRecords!(35, 1) = 1 ' TIMES DONE
                indRecords!(35, 2) = 1 ' HALF
                indRecDesc$(35, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(35, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(35, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(35, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(35, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 12) + QQ(I, 4, I1, 12) > indRecords!(35, 0) Then
                indRecords!(35, 0) = QQ(I, 3, I1, 12) + QQ(I, 4, I1, 12) ' RB
                indRecords!(35, 1) = 1 ' TIMES DONE
                indRecords!(35, 2) = 2 ' HALF
                indRecDesc$(35, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(35, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(35, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(35, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(35, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 12) > indRecords!(36, 0) Then
                    indRecords!(36, 0) = QQ(I, I2, I1, 12) ' RB
                    indRecords!(36, 1) = 1 ' TIMES DONE
                    indRecords!(36, 2) = I2 ' QTR
                    indRecDesc$(36, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(36, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(36, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(36, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(36, 4) = Date$ ' DATE
                End If
            Next I2

            ' TOTAL DEFENSIVE REBOUNDS IN A GAME, HALF, QUARTER
            If gameRatings!(I, I1, 11) - gameRatings!(I, I1, 24) > indRecords!(37, 0) Then
                indRecords!(37, 0) = gameRatings!(I, I1, 11) - gameRatings!(I, I1, 24) ' FTA
                indRecords!(37, 1) = 1 ' TIMES DONE
                indRecords!(37, 2) = 0 ' GAME
                indRecDesc$(37, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(37, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(37, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(37, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(37, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 5) + QQ(I, 2, I1, 5) - (QQ(I, 1, I1, 12) + QQ(I, 2, I1, 12)) > indRecords!(38, 0) Then
                indRecords!(38, 0) = QQ(I, 1, I1, 5) + QQ(I, 2, I1, 5) - (QQ(I, 1, I1, 12) + QQ(I, 2, I1, 12)) ' RB
                indRecords!(38, 1) = 1 ' TIMES DONE
                indRecords!(38, 2) = 1 ' HALF
                indRecDesc$(38, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(38, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(38, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(38, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(38, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 5) + QQ(I, 4, I1, 5) - (QQ(I, 3, I1, 12) + QQ(I, 4, I1, 12)) > indRecords!(38, 0) Then
                indRecords!(38, 0) = QQ(I, 3, I1, 5) + QQ(I, 4, I1, 5) - (QQ(I, 3, I1, 12) + QQ(I, 4, I1, 12)) ' RB
                indRecords!(38, 1) = 1 ' TIMES DONE
                indRecords!(38, 2) = 2 ' HALF
                indRecDesc$(38, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(38, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(38, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(38, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(38, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 5) - QQ(I, I2, I1, 12) > indRecords!(39, 0) Then
                    indRecords!(39, 0) = QQ(I, I2, I1, 5) - QQ(I, I2, I1, 12) ' RB
                    indRecords!(39, 1) = 1 ' TIMES DONE
                    indRecords!(39, 2) = I2 ' QTR
                    indRecDesc$(39, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(39, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(39, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(39, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(39, 4) = Date$ ' DATE
                End If
            Next I2

            ' ASSISTS IN A GAME, HALF, QUARTER
            If gameRatings!(I, I1, 14) > indRecords!(40, 0) Then
                indRecords!(40, 0) = gameRatings!(I, I1, 14) ' FTA
                indRecords!(40, 1) = 1 ' TIMES DONE
                indRecords!(40, 2) = 0 ' GAME
                indRecDesc$(40, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(40, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(40, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(40, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(40, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 8) + QQ(I, 2, I1, 8) > indRecords!(41, 0) Then
                indRecords!(41, 0) = QQ(I, 1, I1, 8) + QQ(I, 2, I1, 8) ' FTA
                indRecords!(41, 1) = 1 ' TIMES DONE
                indRecords!(41, 2) = 1 ' HALF
                indRecDesc$(41, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(41, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(41, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(41, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(41, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 8) + QQ(I, 4, I1, 8) > indRecords!(41, 0) Then
                indRecords!(41, 0) = QQ(I, 3, I1, 8) + QQ(I, 4, I1, 8) ' FTM
                indRecords!(41, 1) = 1 ' TIMES DONE
                indRecords!(41, 2) = 2 ' HALF
                indRecDesc$(41, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(41, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(41, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(41, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(41, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 8) > indRecords!(42, 0) Then
                    indRecords!(42, 0) = QQ(I, I2, I1, 8) ' FTM
                    indRecords!(42, 1) = 1 ' TIMES DONE
                    indRecords!(42, 2) = I2 ' QTR
                    indRecDesc$(42, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(42, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(42, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(42, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(42, 4) = Date$ ' DATE
                End If
            Next I2

            ' STEALS IN A GAME, HALF, QUARTER
            If gameRatings!(I, I1, 13) > indRecords!(43, 0) Then
                indRecords!(43, 0) = gameRatings!(I, I1, 13) ' FTA
                indRecords!(43, 1) = 1 ' TIMES DONE
                indRecords!(43, 2) = 0 ' GAME
                indRecDesc$(43, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(43, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(43, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(43, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(43, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 7) + QQ(I, 2, I1, 7) > indRecords!(44, 0) Then
                indRecords!(44, 0) = QQ(I, 1, I1, 7) + QQ(I, 2, I1, 7) ' FTA
                indRecords!(44, 1) = 1 ' TIMES DONE
                indRecords!(44, 2) = 1 ' HALF
                indRecDesc$(44, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(44, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(44, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(44, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(44, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 7) + QQ(I, 4, I1, 7) > indRecords!(44, 0) Then
                indRecords!(44, 0) = QQ(I, 3, I1, 7) + QQ(I, 4, I1, 7) ' FTM
                indRecords!(44, 1) = 1 ' TIMES DONE
                indRecords!(44, 2) = 2 ' HALF
                indRecDesc$(44, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(44, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(44, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(44, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(44, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 7) > indRecords!(45, 0) Then
                    indRecords!(45, 0) = QQ(I, I2, I1, 7) ' FTM
                    indRecords!(45, 1) = 1 ' TIMES DONE
                    indRecords!(45, 2) = I2 ' QTR
                    indRecDesc$(45, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(45, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(45, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(45, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(45, 4) = Date$ ' DATE
                End If
            Next I2

            ' BLOCKS IN A GAME, HALF, QUARTER
            If gameRatings!(I, I1, 15) > indRecords!(46, 0) Then
                indRecords!(46, 0) = gameRatings!(I, I1, 15) ' FTA
                indRecords!(46, 1) = 1 ' TIMES DONE
                indRecords!(46, 2) = 0 ' GAME
                indRecDesc$(46, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(46, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(46, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(46, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(46, 4) = Date$ ' DATE
            End If

            If QQ(I, 1, I1, 9) + QQ(I, 2, I1, 9) > indRecords!(47, 0) Then
                indRecords!(47, 0) = QQ(I, 1, I1, 9) + QQ(I, 2, I1, 9) ' FTA
                indRecords!(47, 1) = 1 ' TIMES DONE
                indRecords!(47, 2) = 1 ' HALF
                indRecDesc$(47, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(47, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(47, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(47, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(47, 4) = Date$ ' DATE
            End If

            If QQ(I, 3, I1, 9) + QQ(I, 4, I1, 9) > indRecords!(47, 0) Then
                indRecords!(47, 0) = QQ(I, 3, I1, 9) + QQ(I, 4, I1, 9) ' FTM
                indRecords!(47, 1) = 1 ' TIMES DONE
                indRecords!(47, 2) = 2 ' HALF
                indRecDesc$(47, 0) = players_GAME$(I, I1, 0) ' NAME
                indRecDesc$(47, 1) = gameTeams$(I) ' TEAM
                indRecDesc$(47, 2) = VERSUS$(I) ' AT/VS
                indRecDesc$(47, 3) = gameTeams$(1 - I) ' OPPONENT
                indRecDesc$(47, 4) = Date$ ' DATE
            End If

            For I2 = 1 To 4
                If QQ(I, I2, I1, 9) > indRecords!(48, 0) Then
                    indRecords!(48, 0) = QQ(I, I2, I1, 9) ' FTM
                    indRecords!(48, 1) = 1 ' TIMES DONE
                    indRecords!(48, 2) = I2 ' QTR
                    indRecDesc$(48, 0) = players_GAME$(I, I1, 0) ' NAME
                    indRecDesc$(48, 1) = gameTeams$(I) ' TEAM
                    indRecDesc$(48, 2) = VERSUS$(I) ' AT/VS
                    indRecDesc$(48, 3) = gameTeams$(1 - I) ' OPPONENT
                    indRecDesc$(48, 4) = Date$ ' DATE
                End If
            Next I2
        Next
    Next

    ' MOST OVERTIME PERIODS
    If quarter - 5 > teamRecords!(0, 0) Then
        teamRecords!(0, 0) = quarter - 5
        teamRecords!(0, 1) = score(0, 0)
        teamRecords!(0, 2) = score(1, 0)
        teamRecDesc$(0, 0) = gameTeams$(0)
        teamRecDesc$(0, 1) = gameTeams$(1)
        teamRecDesc$(0, 2) = Date$
    End If

    For I = 0 To 1

        If score(I, 0) > teamRecords!(1, 0) Then
            teamRecords!(1, 0) = score(I, 0)
            teamRecords!(1, 1) = 1
            teamRecords!(1, 2) = 0
            teamRecDesc$(1, 0) = gameTeams$(I)
            teamRecDesc$(1, 1) = VERSUS$(I)
            teamRecDesc$(1, 2) = gameTeams$(1 - I)
            teamRecDesc$(1, 3) = Date$
        End If

        If score(I, 0) < teamRecords!(2, 0) Then
            teamRecords!(2, 0) = score(I, 0)
            teamRecords!(2, 1) = 1
            teamRecords!(2, 2) = 0
            teamRecDesc$(2, 0) = gameTeams$(I)
            teamRecDesc$(2, 1) = VERSUS$(I)
            teamRecDesc$(2, 2) = gameTeams$(1 - I)
            teamRecDesc$(2, 3) = Date$
        End If

        If score(I, 0) + score(1 - I, 0) > teamRecords!(3, 0) Then
            teamRecords!(3, 0) = score(I, 0) + score(1 - I, 0)
            teamRecords!(3, 1) = score(I, 0)
            teamRecords!(3, 2) = score(1 - I, 0)
            teamRecDesc$(3, 0) = gameTeams$(I)
            teamRecDesc$(3, 1) = VERSUS$(I)
            teamRecDesc$(3, 2) = gameTeams$(1 - I)
            teamRecDesc$(3, 3) = Date$
        End If

        If score(I, 0) + score(1 - I, 0) < teamRecords!(4, 0) Then
            teamRecords!(4, 0) = score(I, 0) + score(1 - I, 0)
            teamRecords!(4, 1) = score(I, 0)
            teamRecords!(4, 2) = score(1 - I, 0)
            teamRecDesc$(4, 0) = gameTeams$(I)
            teamRecDesc$(4, 1) = VERSUS$(I)
            teamRecDesc$(4, 2) = gameTeams$(1 - I)
            teamRecDesc$(4, 3) = Date$
        End If

        If score(I, 1) + score(I, 2) > teamRecords!(6, 0) Then
            teamRecords!(6, 0) = score(I, 1) + score(I, 2)
            teamRecords!(6, 1) = 1
            teamRecords!(6, 2) = 0
            teamRecDesc$(6, 0) = gameTeams$(I)
            teamRecDesc$(6, 1) = VERSUS$(I)
            teamRecDesc$(6, 2) = gameTeams$(1 - I)
            teamRecDesc$(6, 3) = Date$
        End If

        If score(I, 1) + score(I, 2) < teamRecords!(7, 0) Then
            teamRecords!(7, 0) = score(I, 1) + score(I, 2)
            teamRecords!(7, 1) = 1
            teamRecords!(7, 2) = 0
            teamRecDesc$(7, 0) = gameTeams$(I)
            teamRecDesc$(7, 1) = VERSUS$(I)
            teamRecDesc$(7, 2) = gameTeams$(1 - I)
            teamRecDesc$(7, 3) = Date$
        End If

        If score(I, 1) + score(I, 2) + score(1 - I, 1) + score(1 - I, 2) > teamRecords!(8, 0) Then
            teamRecords!(8, 0) = score(I, 1) + score(1 - I, 1) + score(I, 2) + score(1 - I, 2)
            teamRecords!(8, 1) = score(I, 1) + score(I, 2)
            teamRecords!(8, 2) = score(1 - I, 1) + score(1 - I, 2)
            teamRecDesc$(8, 0) = gameTeams$(I)
            teamRecDesc$(8, 1) = VERSUS$(I)
            teamRecDesc$(8, 2) = gameTeams$(1 - I)
            teamRecDesc$(8, 3) = Date$
        End If

        If score(I, 1) + score(I, 2) + score(1 - I, 1) + score(1 - I, 2) < teamRecords!(9, 0) Then
            teamRecords!(9, 0) = score(I, 1) + score(1 - I, 1) + score(I, 2) + score(1 - I, 2)
            teamRecords!(9, 1) = score(I, 1) + score(I, 2)
            teamRecords!(9, 2) = score(1 - I, 1) + score(1 - I, 2)
            teamRecDesc$(9, 0) = gameTeams$(I)
            teamRecDesc$(9, 1) = VERSUS$(I)
            teamRecDesc$(9, 2) = gameTeams$(1 - I)
            teamRecDesc$(9, 3) = Date$
        End If

        If score(I, 3) + score(I, 4) > teamRecords!(10, 0) Then
            teamRecords!(10, 0) = score(I, 3) + score(I, 4)
            teamRecords!(10, 1) = 1
            teamRecords!(10, 2) = 0
            teamRecDesc$(10, 0) = gameTeams$(I)
            teamRecDesc$(10, 1) = VERSUS$(I)
            teamRecDesc$(10, 2) = gameTeams$(1 - I)
            teamRecDesc$(10, 3) = Date$
        End If

        If score(I, 3) + score(I, 4) < teamRecords!(11, 0) Then
            teamRecords!(11, 0) = score(I, 3) + score(I, 4)
            teamRecords!(11, 1) = 1
            teamRecords!(11, 2) = 0
            teamRecDesc$(11, 0) = gameTeams$(I)
            teamRecDesc$(11, 1) = VERSUS$(I)
            teamRecDesc$(11, 2) = gameTeams$(1 - I)
            teamRecDesc$(11, 3) = Date$
        End If

        If score(I, 3) + score(I, 4) + score(1 - I, 3) + score(1 - I, 4) > teamRecords!(12, 0) Then
            teamRecords!(12, 0) = score(I, 3) + score(1 - I, 3) + score(I, 4) + score(1 - I, 4)
            teamRecords!(12, 1) = score(I, 3) + score(I, 4)
            teamRecords!(12, 2) = score(1 - I, 3) + score(1 - I, 4)
            teamRecDesc$(12, 0) = gameTeams$(I)
            teamRecDesc$(12, 1) = VERSUS$(I)
            teamRecDesc$(12, 2) = gameTeams$(1 - I)
            teamRecDesc$(12, 3) = Date$
        End If

        If score(I, 3) + score(I, 4) + score(1 - I, 3) + score(1 - I, 4) < teamRecords!(13, 0) Then
            teamRecords!(13, 0) = score(I, 3) + score(1 - I, 3) + score(I, 4) + score(1 - I, 4)
            teamRecords!(13, 1) = score(I, 3) + score(I, 4)
            teamRecords!(13, 2) = score(1 - I, 3) + score(1 - I, 4)
            teamRecDesc$(13, 0) = gameTeams$(I)
            teamRecDesc$(13, 1) = VERSUS$(I)
            teamRecDesc$(13, 2) = gameTeams$(1 - I)
            teamRecDesc$(13, 3) = Date$
        End If

        If score(I, 1) > teamRecords!(14, 0) Then
            teamRecords!(14, 0) = score(I, 1)
            teamRecords!(14, 1) = 1
            teamRecords!(14, 2) = 0
            teamRecDesc$(14, 0) = gameTeams$(I)
            teamRecDesc$(14, 1) = VERSUS$(I)
            teamRecDesc$(14, 2) = gameTeams$(1 - I)
            teamRecDesc$(14, 3) = Date$
        End If

        If score(I, 1) < teamRecords!(15, 0) Then
            teamRecords!(15, 0) = score(I, 1)
            teamRecords!(15, 1) = 1
            teamRecords!(15, 2) = 0
            teamRecDesc$(15, 0) = gameTeams$(I)
            teamRecDesc$(15, 1) = VERSUS$(I)
            teamRecDesc$(15, 2) = gameTeams$(1 - I)
            teamRecDesc$(15, 3) = Date$
        End If

        If score(I, 1) + score(1 - I, 1) > teamRecords!(16, 0) Then
            teamRecords!(16, 0) = score(I, 1) + score(1 - I, 1)
            teamRecords!(16, 1) = score(I, 1)
            teamRecords!(16, 2) = score(1 - I, 1)
            teamRecDesc$(16, 0) = gameTeams$(I)
            teamRecDesc$(16, 1) = VERSUS$(I)
            teamRecDesc$(16, 2) = gameTeams$(1 - I)
            teamRecDesc$(16, 3) = Date$
        End If

        If score(I, 1) + score(1 - I, 1) < teamRecords!(17, 0) Then
            teamRecords!(17, 0) = score(I, 1) + score(1 - I, 1)
            teamRecords!(17, 1) = score(I, 1)
            teamRecords!(17, 2) = score(1 - I, 1)
            teamRecDesc$(17, 0) = gameTeams$(I)
            teamRecDesc$(17, 1) = VERSUS$(I)
            teamRecDesc$(17, 2) = gameTeams$(1 - I)
            teamRecDesc$(17, 3) = Date$
        End If

        If score(I, 2) > teamRecords!(18, 0) Then
            teamRecords!(18, 0) = score(I, 2)
            teamRecords!(18, 1) = 1
            teamRecords!(18, 2) = 0
            teamRecDesc$(18, 0) = gameTeams$(I)
            teamRecDesc$(18, 1) = VERSUS$(I)
            teamRecDesc$(18, 2) = gameTeams$(1 - I)
            teamRecDesc$(18, 3) = Date$
        End If

        If score(I, 2) < teamRecords!(19, 0) Then
            teamRecords!(19, 0) = score(I, 2)
            teamRecords!(19, 1) = 1
            teamRecords!(19, 2) = 0
            teamRecDesc$(19, 0) = gameTeams$(I)
            teamRecDesc$(19, 1) = VERSUS$(I)
            teamRecDesc$(19, 2) = gameTeams$(1 - I)
            teamRecDesc$(19, 3) = Date$
        End If

        If score(I, 2) + score(1 - I, 2) > teamRecords!(20, 0) Then
            teamRecords!(20, 0) = score(I, 2) + score(1 - I, 2)
            teamRecords!(20, 1) = score(I, 2)
            teamRecords!(20, 2) = score(1 - I, 2)
            teamRecDesc$(20, 0) = gameTeams$(I)
            teamRecDesc$(20, 1) = VERSUS$(I)
            teamRecDesc$(20, 2) = gameTeams$(1 - I)
            teamRecDesc$(20, 3) = Date$
        End If

        If score(I, 2) + score(1 - I, 2) < teamRecords!(21, 0) Then
            teamRecords!(21, 0) = score(I, 2) + score(1 - I, 2)
            teamRecords!(21, 1) = score(I, 2)
            teamRecords!(21, 2) = score(1 - I, 2)
            teamRecDesc$(21, 0) = gameTeams$(I)
            teamRecDesc$(21, 1) = VERSUS$(I)
            teamRecDesc$(21, 2) = gameTeams$(1 - I)
            teamRecDesc$(21, 3) = Date$
        End If

        If score(I, 3) > teamRecords!(22, 0) Then
            teamRecords!(22, 0) = score(I, 3)
            teamRecords!(22, 1) = 1
            teamRecords!(22, 2) = 0
            teamRecDesc$(22, 0) = gameTeams$(I)
            teamRecDesc$(22, 1) = VERSUS$(I)
            teamRecDesc$(22, 2) = gameTeams$(1 - I)
            teamRecDesc$(22, 3) = Date$
        End If

        If score(I, 3) < teamRecords!(23, 0) Then
            teamRecords!(23, 0) = score(I, 3)
            teamRecords!(23, 1) = 1
            teamRecords!(23, 2) = 0
            teamRecDesc$(23, 0) = gameTeams$(I)
            teamRecDesc$(23, 1) = VERSUS$(I)
            teamRecDesc$(23, 2) = gameTeams$(1 - I)
            teamRecDesc$(23, 3) = Date$
        End If

        If score(I, 3) + score(1 - I, 3) > teamRecords!(24, 0) Then
            teamRecords!(24, 0) = score(I, 3) + score(1 - I, 3)
            teamRecords!(24, 1) = score(I, 3)
            teamRecords!(24, 2) = score(1 - I, 3)
            teamRecDesc$(24, 0) = gameTeams$(I)
            teamRecDesc$(24, 1) = VERSUS$(I)
            teamRecDesc$(24, 2) = gameTeams$(1 - I)
            teamRecDesc$(24, 3) = Date$
        End If

        If score(I, 3) + score(1 - I, 3) < teamRecords!(25, 0) Then
            teamRecords!(25, 0) = score(I, 3) + score(1 - I, 3)
            teamRecords!(25, 1) = score(I, 3)
            teamRecords!(25, 2) = score(1 - I, 3)
            teamRecDesc$(25, 0) = gameTeams$(I)
            teamRecDesc$(25, 1) = VERSUS$(I)
            teamRecDesc$(25, 2) = gameTeams$(1 - I)
            teamRecDesc$(25, 3) = Date$
        End If

        If score(I, 4) > teamRecords!(26, 0) Then
            teamRecords!(26, 0) = score(I, 4)
            teamRecords!(26, 1) = 1
            teamRecords!(26, 2) = 0
            teamRecDesc$(26, 0) = gameTeams$(I)
            teamRecDesc$(26, 1) = VERSUS$(I)
            teamRecDesc$(26, 2) = gameTeams$(1 - I)
            teamRecDesc$(26, 3) = Date$
        End If

        If score(I, 4) < teamRecords!(27, 0) Then
            teamRecords!(27, 0) = score(I, 4)
            teamRecords!(27, 1) = 1
            teamRecords!(27, 2) = 0
            teamRecDesc$(27, 0) = gameTeams$(I)
            teamRecDesc$(27, 1) = VERSUS$(I)
            teamRecDesc$(27, 2) = gameTeams$(1 - I)
            teamRecDesc$(27, 3) = Date$
        End If

        If score(I, 4) + score(1 - I, 4) > teamRecords!(28, 0) Then
            teamRecords!(28, 0) = score(I, 4) + score(1 - I, 4)
            teamRecords!(28, 1) = score(I, 4)
            teamRecords!(28, 2) = score(1 - I, 4)
            teamRecDesc$(28, 0) = gameTeams$(I)
            teamRecDesc$(28, 1) = VERSUS$(I)
            teamRecDesc$(28, 2) = gameTeams$(1 - I)
            teamRecDesc$(28, 3) = Date$
        End If

        If score(I, 4) + score(1 - I, 4) < teamRecords!(29, 0) Then
            teamRecords!(29, 0) = score(I, 4) + score(1 - I, 4)
            teamRecords!(29, 1) = score(I, 4)
            teamRecords!(29, 2) = score(1 - I, 4)
            teamRecDesc$(29, 0) = gameTeams$(I)
            teamRecDesc$(29, 1) = VERSUS$(I)
            teamRecDesc$(29, 2) = gameTeams$(1 - I)
            teamRecDesc$(29, 3) = Date$
        End If

        ' TEAM FIELD GOAL PERCENTAGES
        For I1 = 0 To 14: V8(I, I1) = 0: V8(1 - I, I1) = 0: Next

        For I2 = 0 To 1
            For I1 = 0 To 14
                V8(I2, 0) = V8(I2, 0) + gameRatings!(I2, I1, 8)
                V8(I2, 1) = V8(I2, 1) + gameRatings!(I2, I1, 7)
                V8(I2, 2) = V8(I2, 2) + gameRatings!(I2, I1, 10)
                V8(I2, 3) = V8(I2, 3) + gameRatings!(I2, I1, 9)
                V8(I2, 4) = V8(I2, 4) + gameRatings!(I2, I1, 11)
                V8(I2, 5) = V8(I2, 5) + (gameRatings!(I2, I1, 8) * 2) + gameRatings!(I2, I1, 10) + threeFG(I2, I1, 0)
                V8(I2, 13) = V8(I2, 13) + threeFG(I2, I1, 0)
                V8(I2, 14) = V8(I2, 14) + threeFG(I2, I1, 1)
                V8(I2, 6) = totalFouls(I2)
                V8(I2, 7) = V8(I2, 7) + gameRatings!(I2, I1, 14)
                V8(I2, 8) = V8(I2, 8) + gameRatings!(I2, I1, 15)
                V8(I2, 9) = V8(I2, 9) + gameRatings!(I2, I1, 13)
                V8(I2, 11) = V8(I2, 11) + gameRatings!(I2, I1, 24)
                V8(I2, 12) = V8(I2, 12) + gameRatings!(I2, I1, 23)
                V8(I2, 10) = V8(I2, 10) + timePlayed(I2, I1)
            Next
        Next

        If V8(I, 0) / V8(I, 1) > teamRecords!(30, 0) Then
            teamRecords!(30, 0) = V8(I, 0) / V8(I, 1)
            teamRecords!(30, 1) = V8(I, 0)
            teamRecords!(30, 2) = V8(I, 1)
            teamRecDesc$(30, 0) = gameTeams$(I)
            teamRecDesc$(30, 1) = VERSUS$(I)
            teamRecDesc$(30, 2) = gameTeams$(1 - I)
            teamRecDesc$(30, 3) = Date$
        End If

        If V8(I, 0) / V8(I, 1) < teamRecords!(31, 0) Then
            teamRecords!(31, 0) = V8(I, 0) / V8(I, 1)
            teamRecords!(31, 1) = V8(I, 0)
            teamRecords!(31, 2) = V8(I, 1)
            teamRecDesc$(31, 0) = gameTeams$(I)
            teamRecDesc$(31, 1) = VERSUS$(I)
            teamRecDesc$(31, 2) = gameTeams$(1 - I)
            teamRecDesc$(31, 3) = Date$
        End If

        If (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1)) > teamRecords!(32, 0) Then
            teamRecords!(32, 0) = (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1))
            teamRecords!(32, 1) = V8(I, 0) / V8(I, 1)
            teamRecords!(32, 2) = V8(1 - I, 0) / V8(1 - I, 1)
            teamRecDesc$(32, 0) = gameTeams$(I)
            teamRecDesc$(32, 1) = VERSUS$(I)
            teamRecDesc$(32, 2) = gameTeams$(1 - I)
            teamRecDesc$(32, 3) = Date$
        End If

        If (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1)) < teamRecords!(33, 0) Then
            teamRecords!(33, 0) = (V8(I, 0) + V8(1 - I, 0)) / (V8(I, 1) + V8(1 - I, 1))
            teamRecords!(33, 1) = V8(I, 0) / V8(I, 1)
            teamRecords!(33, 2) = V8(1 - I, 0) / V8(1 - I, 1)
            teamRecDesc$(33, 0) = gameTeams$(I)
            teamRecDesc$(33, 1) = VERSUS$(I)
            teamRecDesc$(33, 2) = gameTeams$(1 - I)
            teamRecDesc$(33, 3) = Date$
        End If

        ' TEAM FIELD GOALS MADE
        If V8(I, 0) > teamRecords!(34, 0) Then
            teamRecords!(34, 0) = V8(I, 0)
            teamRecords!(34, 1) = 0
            teamRecords!(34, 2) = 0
            teamRecDesc$(34, 0) = gameTeams$(I)
            teamRecDesc$(34, 1) = VERSUS$(I)
            teamRecDesc$(34, 2) = gameTeams$(1 - I)
            teamRecDesc$(34, 3) = Date$
        End If

        If V8(I, 0) < teamRecords!(35, 0) Then
            teamRecords!(35, 0) = V8(I, 0)
            teamRecords!(35, 1) = 0
            teamRecords!(35, 2) = 0
            teamRecDesc$(35, 0) = gameTeams$(I)
            teamRecDesc$(35, 1) = VERSUS$(I)
            teamRecDesc$(35, 2) = gameTeams$(1 - I)
            teamRecDesc$(35, 3) = Date$
        End If

        If V8(I, 0) + V8(1 - I, 0) > teamRecords!(36, 0) Then
            teamRecords!(36, 0) = V8(I, 0) + V8(1 - I, 0)
            teamRecords!(36, 1) = V8(I, 0)
            teamRecords!(36, 2) = V8(1 - I, 0)
            teamRecDesc$(36, 0) = gameTeams$(I)
            teamRecDesc$(36, 1) = VERSUS$(I)
            teamRecDesc$(36, 2) = gameTeams$(1 - I)
            teamRecDesc$(36, 3) = Date$
        End If

        If V8(I, 0) + V8(1 - I, 0) < teamRecords!(37, 0) Then
            teamRecords!(37, 0) = V8(I, 0) + V8(1 - I, 0)
            teamRecords!(37, 1) = V8(I, 0)
            teamRecords!(37, 2) = V8(1 - I, 0)
            teamRecDesc$(37, 0) = gameTeams$(I)
            teamRecDesc$(37, 1) = VERSUS$(I)
            teamRecDesc$(37, 2) = gameTeams$(1 - I)
            teamRecDesc$(37, 3) = Date$
        End If

        If QR(I, 1, 0) + QR(I, 2, 0) > teamRecords!(38, 0) Then
            teamRecords!(38, 0) = QR(I, 1, 0) + QR(I, 2, 0)
            teamRecords!(38, 1) = 1
            teamRecords!(38, 2) = 0
            teamRecDesc$(38, 0) = gameTeams$(I)
            teamRecDesc$(38, 1) = VERSUS$(I)
            teamRecDesc$(38, 2) = gameTeams$(1 - I)
            teamRecDesc$(38, 3) = Date$
        End If

        If QR(I, 3, 0) + QR(I, 4, 0) > teamRecords!(38, 0) Then
            teamRecords!(38, 0) = QR(I, 3, 0) + QR(I, 4, 0)
            teamRecords!(38, 1) = 2
            teamRecords!(38, 2) = 0
            teamRecDesc$(38, 0) = gameTeams$(I)
            teamRecDesc$(38, 1) = VERSUS$(I)
            teamRecDesc$(38, 2) = gameTeams$(1 - I)
            teamRecDesc$(38, 3) = Date$
        End If

        If QR(I, 1, 0) + QR(I, 2, 0) + QR(1 - I, 1, 0) + QR(1 - I, 2, 0) > teamRecords!(39, 0) Then
            teamRecords!(39, 0) = QR(I, 1, 0) + QR(I, 2, 0) + QR(1 - I, 1, 0) + QR(1 - I, 2, 0)
            teamRecords!(39, 1) = 1
            teamRecords!(39, 2) = 0
            teamRecDesc$(39, 0) = gameTeams$(I)
            teamRecDesc$(39, 1) = VERSUS$(I)
            teamRecDesc$(39, 2) = gameTeams$(1 - I)
            teamRecDesc$(39, 3) = Date$
        End If

        If QR(I, 3, 0) + QR(I, 4, 0) + QR(1 - I, 3, 0) + QR(1 - I, 4, 0) > teamRecords!(39, 0) Then
            teamRecords!(39, 0) = QR(I, 3, 0) + QR(I, 4, 0) + QR(1 - I, 3, 0) + QR(1 - I, 4, 0)
            teamRecords!(39, 1) = 2
            teamRecords!(39, 2) = 0
            teamRecDesc$(39, 0) = gameTeams$(I)
            teamRecDesc$(39, 1) = VERSUS$(I)
            teamRecDesc$(39, 2) = gameTeams$(1 - I)
            teamRecDesc$(39, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 0) > teamRecords!(40, 0) Then
                teamRecords!(40, 0) = QR(I, I2, 0)
                teamRecords!(40, 1) = I2
                teamRecords!(40, 2) = 0
                teamRecDesc$(40, 0) = gameTeams$(I)
                teamRecDesc$(40, 1) = VERSUS$(I)
                teamRecDesc$(40, 2) = gameTeams$(1 - I)
                teamRecDesc$(40, 3) = Date$
            End If

            If QR(I, I2, 0) + QR(1 - I, I2, 0) > teamRecords!(41, 0) Then
                teamRecords!(41, 0) = QR(I, I2, 0) + QR(1 - I, I2, 0)
                teamRecords!(41, 1) = I2
                teamRecords!(41, 2) = QR(I, I2, 0)
                teamRecDesc$(41, 0) = gameTeams$(I)
                teamRecDesc$(41, 1) = VERSUS$(I)
                teamRecDesc$(41, 2) = gameTeams$(1 - I)
                teamRecDesc$(41, 3) = Date$
            End If
        Next

        'TEAM FIELD GOAL ATTEMPTS
        If V8(I, 1) > teamRecords!(42, 0) Then
            teamRecords!(42, 0) = V8(I, 1)
            teamRecords!(42, 1) = 0
            teamRecords!(42, 2) = 0
            teamRecDesc$(42, 0) = gameTeams$(I)
            teamRecDesc$(42, 1) = VERSUS$(I)
            teamRecDesc$(42, 2) = gameTeams$(1 - I)
            teamRecDesc$(42, 3) = Date$
        End If

        If V8(I, 1) < teamRecords!(43, 0) Then
            teamRecords!(43, 0) = V8(I, 1)
            teamRecords!(43, 1) = 0
            teamRecords!(43, 2) = 0
            teamRecDesc$(43, 0) = gameTeams$(I)
            teamRecDesc$(43, 1) = VERSUS$(I)
            teamRecDesc$(43, 2) = gameTeams$(1 - I)
            teamRecDesc$(43, 3) = Date$
        End If

        If V8(I, 1) + V8(1 - I, 1) > teamRecords!(44, 0) Then
            teamRecords!(44, 0) = V8(I, 1) + V8(1 - I, 1)
            teamRecords!(44, 1) = V8(I, 1)
            teamRecords!(44, 2) = V8(1 - I, 1)
            teamRecDesc$(44, 0) = gameTeams$(I)
            teamRecDesc$(44, 1) = VERSUS$(I)
            teamRecDesc$(44, 2) = gameTeams$(1 - I)
            teamRecDesc$(44, 3) = Date$
        End If

        If V8(I, 1) + V8(1 - I, 1) < teamRecords!(45, 0) Then
            teamRecords!(45, 0) = V8(I, 1) + V8(1 - I, 1)
            teamRecords!(45, 1) = V8(I, 1)
            teamRecords!(45, 2) = V8(1 - I, 1)
            teamRecDesc$(45, 0) = gameTeams$(I)
            teamRecDesc$(45, 1) = VERSUS$(I)
            teamRecDesc$(45, 2) = gameTeams$(1 - I)
            teamRecDesc$(45, 3) = Date$
        End If

        If QR(I, 1, 1) + QR(I, 2, 1) > teamRecords!(46, 0) Then
            teamRecords!(46, 0) = QR(I, 1, 1) + QR(I, 2, 1)
            teamRecords!(46, 1) = 1
            teamRecords!(46, 2) = 0
            teamRecDesc$(46, 0) = gameTeams$(I)
            teamRecDesc$(46, 1) = VERSUS$(I)
            teamRecDesc$(46, 2) = gameTeams$(1 - I)
            teamRecDesc$(46, 3) = Date$
        End If

        If QR(I, 3, 1) + QR(I, 4, 1) > teamRecords!(46, 0) Then
            teamRecords!(46, 0) = QR(I, 3, 1) + QR(I, 4, 1)
            teamRecords!(46, 1) = 2
            teamRecords!(46, 2) = 0
            teamRecDesc$(46, 0) = gameTeams$(I)
            teamRecDesc$(46, 1) = VERSUS$(I)
            teamRecDesc$(46, 2) = gameTeams$(1 - I)
            teamRecDesc$(46, 3) = Date$
        End If

        If QR(I, 1, 1) + QR(I, 2, 1) + QR(1 - I, 1, 1) + QR(1 - I, 2, 1) > teamRecords!(47, 0) Then
            teamRecords!(47, 0) = QR(I, 1, 1) + QR(I, 2, 1) + QR(1 - I, 1, 1) + QR(1 - I, 2, 1)
            teamRecords!(47, 1) = 1
            teamRecords!(47, 2) = QR(I, 1, 1) + QR(I, 2, 1)
            teamRecDesc$(47, 0) = gameTeams$(I)
            teamRecDesc$(47, 1) = VERSUS$(I)
            teamRecDesc$(47, 2) = gameTeams$(1 - I)
            teamRecDesc$(47, 3) = Date$
        End If

        If QR(I, 3, 1) + QR(I, 4, 1) + QR(1 - I, 3, 1) + QR(1 - I, 4, 1) > teamRecords!(47, 0) Then
            teamRecords!(47, 0) = QR(I, 3, 1) + QR(I, 4, 1) + QR(1 - I, 3, 1) + QR(1 - I, 4, 1)
            teamRecords!(47, 1) = 1
            teamRecords!(47, 2) = QR(I, 3, 1) + QR(I, 4, 1)
            teamRecDesc$(47, 0) = gameTeams$(I)
            teamRecDesc$(47, 1) = VERSUS$(I)
            teamRecDesc$(47, 2) = gameTeams$(1 - I)
            teamRecDesc$(47, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 1) > teamRecords!(48, 0) Then
                teamRecords!(48, 0) = QR(I, I2, 1)
                teamRecords!(48, 1) = I2
                teamRecords!(48, 2) = 0
                teamRecDesc$(48, 0) = gameTeams$(I)
                teamRecDesc$(48, 1) = VERSUS$(I)
                teamRecDesc$(48, 2) = gameTeams$(1 - I)
                teamRecDesc$(48, 3) = Date$
            End If

            If QR(I, I2, 1) + QR(1 - I, I2, 1) > teamRecords!(49, 0) Then
                teamRecords!(49, 0) = QR(I, I2, 1) + QR(1 - I, I2, 1)
                teamRecords!(49, 1) = I2
                teamRecords!(49, 2) = QR(I, I2, 1)
                teamRecDesc$(49, 0) = gameTeams$(I)
                teamRecDesc$(49, 1) = VERSUS$(I)
                teamRecDesc$(49, 2) = gameTeams$(1 - I)
                teamRecDesc$(49, 3) = Date$
            End If
        Next

        'TEAM FIELD GOAL ATTEMPTS
        If V8(I, 13) = V8(I, 14) And V8(I, 14) > teamRecords!(50, 0) Then
            teamRecords!(50, 0) = V8(I, 13)
            teamRecords!(50, 1) = 0
            teamRecords!(50, 2) = 0
            teamRecDesc$(50, 0) = gameTeams$(I)
            teamRecDesc$(50, 1) = VERSUS$(I)
            teamRecDesc$(50, 2) = gameTeams$(1 - I)
            teamRecDesc$(50, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS MOST MADE
        If V8(I, 13) = V8(I, 14) And V8(1 - I, 13) = V8(1 - I, 14) And V8(I, 14) + V8(1 - I, 14) > teamRecords!(51, 0) Then
            teamRecords!(51, 0) = V8(I, 13)
            teamRecords!(51, 1) = V8(1 - I, 13)
            teamRecords!(51, 2) = 0
            teamRecDesc$(51, 0) = gameTeams$(I)
            teamRecDesc$(51, 1) = VERSUS$(I)
            teamRecDesc$(51, 2) = gameTeams$(1 - I)
            teamRecDesc$(51, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) = 0 And V8(I, 14) > teamRecords!(52, 0) Then
            teamRecords!(52, 0) = V8(I, 14)
            teamRecords!(52, 1) = 0
            teamRecords!(52, 2) = 0
            teamRecDesc$(52, 0) = gameTeams$(I)
            teamRecDesc$(52, 1) = VERSUS$(I)
            teamRecDesc$(52, 2) = gameTeams$(1 - I)
            teamRecDesc$(52, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) > teamRecords!(53, 0) Then
            teamRecords!(53, 0) = V8(I, 13)
            teamRecords!(53, 1) = 0
            teamRecords!(53, 2) = 0
            teamRecDesc$(53, 0) = gameTeams$(I)
            teamRecDesc$(53, 1) = VERSUS$(I)
            teamRecDesc$(53, 2) = gameTeams$(1 - I)
            teamRecDesc$(53, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 13) + V8(1 - I, 13) > teamRecords!(54, 0) Then
            teamRecords!(54, 0) = V8(I, 13) + V8(1 - I, 13)
            teamRecords!(54, 1) = V8(I, 13)
            teamRecords!(54, 2) = V8(1 - I, 13)
            teamRecDesc$(54, 0) = gameTeams$(I)
            teamRecDesc$(54, 1) = VERSUS$(I)
            teamRecDesc$(54, 2) = gameTeams$(1 - I)
            teamRecDesc$(54, 3) = Date$
        End If

        If QR(I, 1, 13) + QR(I, 2, 13) > teamRecords!(55, 0) Then
            teamRecords!(55, 0) = QR(I, 1, 13) + QR(I, 2, 13)
            teamRecords!(55, 1) = 1
            teamRecords!(55, 2) = 0
            teamRecDesc$(55, 0) = gameTeams$(I)
            teamRecDesc$(55, 1) = VERSUS$(I)
            teamRecDesc$(55, 2) = gameTeams$(1 - I)
            teamRecDesc$(55, 3) = Date$
        End If

        If QR(I, 3, 13) + QR(I, 4, 13) > teamRecords!(55, 0) Then
            teamRecords!(55, 0) = QR(I, 3, 13) + QR(I, 4, 13)
            teamRecords!(55, 1) = 2
            teamRecords!(55, 2) = 0
            teamRecDesc$(55, 0) = gameTeams$(I)
            teamRecDesc$(55, 1) = VERSUS$(I)
            teamRecDesc$(55, 2) = gameTeams$(1 - I)
            teamRecDesc$(55, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 13) > teamRecords!(56, 0) Then
                teamRecords!(56, 0) = QR(I, I2, 13)
                teamRecords!(56, 1) = I2
                teamRecords!(56, 2) = 0
                teamRecDesc$(56, 0) = gameTeams$(I)
                teamRecDesc$(56, 1) = VERSUS$(I)
                teamRecDesc$(56, 2) = gameTeams$(1 - I)
                teamRecDesc$(56, 3) = Date$
            End If
        Next
        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 14) > teamRecords!(57, 0) Then
            teamRecords!(57, 0) = V8(I, 14)
            teamRecords!(57, 1) = 0
            teamRecords!(57, 2) = 0
            teamRecDesc$(57, 0) = gameTeams$(I)
            teamRecDesc$(57, 1) = VERSUS$(I)
            teamRecDesc$(57, 2) = gameTeams$(1 - I)
            teamRecDesc$(57, 3) = Date$
        End If

        'TEAM FIELD GOAL ATTEMPTS NONE MADE
        If V8(I, 14) + V8(1 - I, 14) > teamRecords!(58, 0) Then
            teamRecords!(58, 0) = V8(I, 14) + V8(1 - I, 14)
            teamRecords!(58, 1) = V8(I, 14)
            teamRecords!(58, 2) = V8(1 - I, 14)
            teamRecDesc$(58, 0) = gameTeams$(I)
            teamRecDesc$(58, 1) = VERSUS$(I)
            teamRecDesc$(58, 2) = gameTeams$(1 - I)
            teamRecDesc$(58, 3) = Date$
        End If

        If QR(I, 1, 14) + QR(I, 2, 14) > teamRecords!(59, 0) Then
            teamRecords!(59, 0) = QR(I, 1, 14) + QR(I, 2, 14)
            teamRecords!(59, 1) = 1
            teamRecords!(59, 2) = 0
            teamRecDesc$(59, 0) = gameTeams$(I)
            teamRecDesc$(59, 1) = VERSUS$(I)
            teamRecDesc$(59, 2) = gameTeams$(1 - I)
            teamRecDesc$(59, 3) = Date$
        End If

        If QR(I, 3, 14) + QR(I, 4, 14) > teamRecords!(59, 0) Then
            teamRecords!(59, 0) = QR(I, 3, 14) + QR(I, 4, 14)
            teamRecords!(59, 1) = 2
            teamRecords!(59, 2) = 0
            teamRecDesc$(59, 0) = gameTeams$(I)
            teamRecDesc$(59, 1) = VERSUS$(I)
            teamRecDesc$(59, 2) = gameTeams$(1 - I)
            teamRecDesc$(59, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS NO MISS
        If V8(I, 2) = V8(I, 3) And V8(I, 2) > teamRecords!(60, 0) Then
            teamRecords!(60, 0) = V8(I, 2)
            teamRecords!(60, 1) = 0
            teamRecords!(60, 2) = 0
            teamRecDesc$(60, 0) = gameTeams$(I)
            teamRecDesc$(60, 1) = VERSUS$(I)
            teamRecDesc$(60, 2) = gameTeams$(1 - I)
            teamRecDesc$(60, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS LOW %
        If V8(I, 2) / V8(I, 3) < teamRecords!(61, 0) Then
            teamRecords!(61, 0) = V8(I, 2) / V8(I, 3)
            teamRecords!(61, 1) = V8(I, 2)
            teamRecords!(61, 2) = V8(I, 3)
            teamRecDesc$(61, 0) = gameTeams$(I)
            teamRecDesc$(61, 1) = VERSUS$(I)
            teamRecDesc$(61, 2) = gameTeams$(1 - I)
            teamRecDesc$(61, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS COMBINED HIGH %
        If (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3)) > teamRecords!(62, 0) Then
            teamRecords!(62, 0) = (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3))
            teamRecords!(62, 1) = V8(I, 2) / V8(I, 3)
            teamRecords!(62, 2) = V8(1 - I, 2) / V8(1 - I, 3)
            teamRecDesc$(62, 0) = gameTeams$(I)
            teamRecDesc$(62, 1) = VERSUS$(I)
            teamRecDesc$(62, 2) = gameTeams$(1 - I)
            teamRecDesc$(62, 3) = Date$
        End If

        ' FREE THROW ATTEMPTS COMBINED LOW %
        If (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3)) < teamRecords!(63, 0) Then
            teamRecords!(63, 0) = (V8(I, 2) + V8(1 - I, 2)) / (V8(I, 3) + V8(1 - I, 3))
            teamRecords!(63, 1) = V8(I, 2) / V8(I, 3)
            teamRecords!(63, 2) = V8(1 - I, 2) / V8(1 - I, 3)
            teamRecDesc$(63, 0) = gameTeams$(I)
            teamRecDesc$(63, 1) = VERSUS$(I)
            teamRecDesc$(63, 2) = gameTeams$(1 - I)
            teamRecDesc$(63, 3) = Date$
        End If

        ' TEAM FREE THROWS MADE
        If V8(I, 2) > teamRecords!(64, 0) Then
            teamRecords!(64, 0) = V8(I, 2)
            teamRecords!(64, 1) = 0
            teamRecords!(64, 2) = 0
            teamRecDesc$(64, 0) = gameTeams$(I)
            teamRecDesc$(64, 1) = VERSUS$(I)
            teamRecDesc$(64, 2) = gameTeams$(1 - I)
            teamRecDesc$(64, 3) = Date$
        End If

        If V8(I, 2) < teamRecords!(65, 0) Then
            teamRecords!(65, 0) = V8(I, 2)
            teamRecords!(65, 1) = 0
            teamRecords!(65, 2) = 0
            teamRecDesc$(65, 0) = gameTeams$(I)
            teamRecDesc$(65, 1) = VERSUS$(I)
            teamRecDesc$(65, 2) = gameTeams$(1 - I)
            teamRecDesc$(65, 3) = Date$
        End If

        If V8(I, 2) + V8(1 - I, 2) > teamRecords!(66, 0) Then
            teamRecords!(66, 0) = V8(I, 2) + V8(1 - I, 2)
            teamRecords!(66, 1) = V8(I, 2)
            teamRecords!(66, 2) = V8(1 - I, 2)
            teamRecDesc$(66, 0) = gameTeams$(I)
            teamRecDesc$(66, 1) = VERSUS$(I)
            teamRecDesc$(66, 2) = gameTeams$(1 - I)
            teamRecDesc$(66, 3) = Date$
        End If

        If V8(I, 2) + V8(1 - I, 2) < teamRecords!(67, 0) Then
            teamRecords!(67, 0) = V8(I, 2) + V8(1 - I, 2)
            teamRecords!(67, 1) = V8(I, 2)
            teamRecords!(67, 2) = V8(1 - I, 2)
            teamRecDesc$(67, 0) = gameTeams$(I)
            teamRecDesc$(67, 1) = VERSUS$(I)
            teamRecDesc$(67, 2) = gameTeams$(1 - I)
            teamRecDesc$(67, 3) = Date$
        End If

        If QR(I, 1, 2) + QR(I, 2, 2) > teamRecords!(68, 0) Then
            teamRecords!(68, 0) = QR(I, 1, 2) + QR(I, 2, 2)
            teamRecords!(68, 1) = 1
            teamRecords!(68, 2) = 0
            teamRecDesc$(68, 0) = gameTeams$(I)
            teamRecDesc$(68, 1) = VERSUS$(I)
            teamRecDesc$(68, 2) = gameTeams$(1 - I)
            teamRecDesc$(68, 3) = Date$
        End If

        If QR(I, 3, 2) + QR(I, 4, 2) > teamRecords!(68, 0) Then
            teamRecords!(68, 0) = QR(I, 3, 2) + QR(I, 4, 2)
            teamRecords!(68, 1) = 2
            teamRecords!(68, 2) = 0
            teamRecDesc$(68, 0) = gameTeams$(I)
            teamRecDesc$(68, 1) = VERSUS$(I)
            teamRecDesc$(68, 2) = gameTeams$(1 - I)
            teamRecDesc$(68, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 2) > teamRecords!(70, 0) Then
                teamRecords!(70, 0) = QR(I, I2, 2)
                teamRecords!(70, 1) = I2
                teamRecords!(70, 2) = 0
                teamRecDesc$(70, 0) = gameTeams$(I)
                teamRecDesc$(70, 1) = VERSUS$(I)
                teamRecDesc$(70, 2) = gameTeams$(1 - I)
                teamRecDesc$(70, 3) = Date$
            End If

        Next

        ' TEAM FREE THROWS ATTEMPTED
        If V8(I, 3) > teamRecords!(72, 0) Then
            teamRecords!(72, 0) = V8(I, 3)
            teamRecords!(72, 1) = 0
            teamRecords!(72, 2) = 0
            teamRecDesc$(72, 0) = gameTeams$(I)
            teamRecDesc$(72, 1) = VERSUS$(I)
            teamRecDesc$(72, 2) = gameTeams$(1 - I)
            teamRecDesc$(72, 3) = Date$
        End If

        If V8(I, 3) < teamRecords!(73, 0) Then
            teamRecords!(73, 0) = V8(I, 3)
            teamRecords!(73, 1) = 0
            teamRecords!(73, 2) = 0
            teamRecDesc$(73, 0) = gameTeams$(I)
            teamRecDesc$(73, 1) = VERSUS$(I)
            teamRecDesc$(73, 2) = gameTeams$(1 - I)
            teamRecDesc$(73, 3) = Date$
        End If

        If V8(I, 3) + V8(1 - I, 3) > teamRecords!(74, 0) Then
            teamRecords!(74, 0) = V8(I, 3) + V8(1 - I, 3)
            teamRecords!(74, 1) = V8(I, 3)
            teamRecords!(74, 2) = V8(1 - I, 3)
            teamRecDesc$(74, 0) = gameTeams$(I)
            teamRecDesc$(74, 1) = VERSUS$(I)
            teamRecDesc$(74, 2) = gameTeams$(1 - I)
            teamRecDesc$(74, 3) = Date$
        End If

        If V8(I, 3) + V8(1 - I, 3) < teamRecords!(75, 0) Then
            teamRecords!(75, 0) = V8(I, 3) + V8(1 - I, 3)
            teamRecords!(75, 1) = V8(I, 3)
            teamRecords!(75, 2) = V8(1 - I, 3)
            teamRecDesc$(75, 0) = gameTeams$(I)
            teamRecDesc$(75, 1) = VERSUS$(I)
            teamRecDesc$(75, 2) = gameTeams$(1 - I)
            teamRecDesc$(75, 3) = Date$
        End If

        If QR(I, 1, 3) + QR(I, 2, 3) > teamRecords!(76, 0) Then
            teamRecords!(76, 0) = QR(I, 1, 3) + QR(I, 2, 3)
            teamRecords!(76, 1) = 1
            teamRecords!(76, 2) = 0
            teamRecDesc$(76, 0) = gameTeams$(I)
            teamRecDesc$(76, 1) = VERSUS$(I)
            teamRecDesc$(76, 2) = gameTeams$(1 - I)
            teamRecDesc$(76, 3) = Date$
        End If

        If QR(I, 3, 3) + QR(I, 4, 3) > teamRecords!(76, 0) Then
            teamRecords!(76, 0) = QR(I, 3, 3) + QR(I, 4, 3)
            teamRecords!(76, 1) = 2
            teamRecords!(76, 2) = 0
            teamRecDesc$(76, 0) = gameTeams$(I)
            teamRecDesc$(76, 1) = VERSUS$(I)
            teamRecDesc$(76, 2) = gameTeams$(1 - I)
            teamRecDesc$(76, 3) = Date$
        End If

        If QR(I, 1, 3) + QR(I, 2, 3) + QR(1 - I, 1, 3) + QR(1 - I, 2, 3) > teamRecords!(77, 0) Then
            teamRecords!(77, 0) = QR(I, 1, 3) + QR(I, 2, 3) + QR(1 - I, 1, 3) + QR(1 - I, 2, 3)
            teamRecords!(77, 1) = 1
            teamRecords!(77, 2) = QR(I, 1, 3) + QR(I, 2, 3)
            teamRecDesc$(77, 0) = gameTeams$(I)
            teamRecDesc$(77, 1) = VERSUS$(I)
            teamRecDesc$(77, 2) = gameTeams$(1 - I)
            teamRecDesc$(77, 3) = Date$
        End If

        If QR(I, 3, 3) + QR(I, 4, 3) + QR(1 - I, 3, 3) + QR(1 - I, 4, 3) > teamRecords!(77, 0) Then
            teamRecords!(77, 0) = QR(I, 3, 3) + QR(I, 4, 3) + QR(1 - I, 3, 3) + QR(1 - I, 4, 3)
            teamRecords!(77, 1) = 2
            teamRecords!(77, 2) = QR(I, 3, 3) + QR(I, 4, 3)
            teamRecDesc$(77, 0) = gameTeams$(I)
            teamRecDesc$(77, 1) = VERSUS$(I)
            teamRecDesc$(77, 2) = gameTeams$(1 - I)
            teamRecDesc$(77, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 3) > teamRecords!(78, 0) Then
                teamRecords!(78, 0) = QR(I, I2, 3)
                teamRecords!(78, 1) = I2
                teamRecords!(78, 2) = 0
                teamRecDesc$(78, 0) = gameTeams$(I)
                teamRecDesc$(78, 1) = VERSUS$(I)
                teamRecDesc$(78, 2) = gameTeams$(1 - I)
                teamRecDesc$(78, 3) = Date$
            End If

            If QR(I, I2, 3) + QR(1 - I, I2, 3) > teamRecords!(79, 0) Then
                teamRecords!(79, 0) = QR(I, I2, 3) + QR(1 - I, I2, 3)
                teamRecords!(79, 1) = I2
                teamRecords!(79, 2) = QR(I, I2, 3)
                teamRecDesc$(79, 0) = gameTeams$(I)
                teamRecDesc$(79, 1) = VERSUS$(I)
                teamRecDesc$(79, 2) = gameTeams$(1 - I)
                teamRecDesc$(79, 3) = Date$
            End If
        Next

        If totRebounds(I) > teamRecords!(80, 0) Then
            teamRecords!(80, 0) = totRebounds(I)
            teamRecords!(80, 1) = 0
            teamRecords!(80, 2) = 0
            teamRecDesc$(80, 0) = gameTeams$(I)
            teamRecDesc$(80, 1) = VERSUS$(I)
            teamRecDesc$(80, 2) = gameTeams$(1 - I)
            teamRecDesc$(80, 3) = Date$
        End If

        If totRebounds(I) < teamRecords!(81, 0) Then
            teamRecords!(81, 0) = totRebounds(I)
            teamRecords!(81, 1) = 0
            teamRecords!(81, 2) = 0
            teamRecDesc$(81, 0) = gameTeams$(I)
            teamRecDesc$(81, 1) = VERSUS$(I)
            teamRecDesc$(81, 2) = gameTeams$(1 - I)
            teamRecDesc$(81, 3) = Date$
        End If

        If totRebounds(I) + totRebounds(1 - I) > teamRecords!(82, 0) Then
            teamRecords!(82, 0) = totRebounds(I) + totRebounds(1 - I)
            teamRecords!(82, 1) = totRebounds(I)
            teamRecords!(82, 2) = totRebounds(1 - I)
            teamRecDesc$(82, 0) = gameTeams$(I)
            teamRecDesc$(82, 1) = VERSUS$(I)
            teamRecDesc$(82, 2) = gameTeams$(1 - I)
            teamRecDesc$(82, 3) = Date$
        End If

        If totRebounds(I) + totRebounds(1 - I) < teamRecords!(83, 0) Then
            teamRecords!(83, 0) = totRebounds(I) + totRebounds(1 - I)
            teamRecords!(83, 1) = totRebounds(I)
            teamRecords!(83, 2) = totRebounds(1 - I)
            teamRecDesc$(83, 0) = gameTeams$(I)
            teamRecDesc$(83, 1) = VERSUS$(I)
            teamRecDesc$(83, 2) = gameTeams$(1 - I)
            teamRecDesc$(83, 3) = Date$
        End If

        If QR(I, 1, 4) + QR(I, 2, 4) > teamRecords!(84, 0) Then
            teamRecords!(84, 0) = QR(I, 1, 4) + QR(I, 2, 4)
            teamRecords!(84, 1) = 1
            teamRecords!(84, 2) = QR(I, 1, 4) + QR(I, 2, 4)
            teamRecDesc$(84, 0) = gameTeams$(I)
            teamRecDesc$(84, 1) = VERSUS$(I)
            teamRecDesc$(84, 2) = gameTeams$(1 - I)
            teamRecDesc$(84, 3) = Date$
        End If

        If QR(I, 3, 4) + QR(I, 4, 4) > teamRecords!(84, 0) Then
            teamRecords!(84, 0) = QR(I, 3, 4) + QR(I, 4, 4)
            teamRecords!(84, 1) = 2
            teamRecords!(84, 2) = QR(I, 3, 4) + QR(I, 4, 4)
            teamRecDesc$(84, 0) = gameTeams$(I)
            teamRecDesc$(84, 1) = VERSUS$(I)
            teamRecDesc$(84, 2) = gameTeams$(1 - I)
            teamRecDesc$(84, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 4) > teamRecords!(85, 0) Then
                teamRecords!(85, 0) = QR(I, I2, 4)
                teamRecords!(85, 1) = I2
                teamRecords!(85, 2) = 0
                teamRecDesc$(85, 0) = gameTeams$(I)
                teamRecDesc$(85, 1) = VERSUS$(I)
                teamRecDesc$(85, 2) = gameTeams$(1 - I)
                teamRecDesc$(85, 3) = Date$
            End If
        Next

        If V8(I, 11) > teamRecords!(86, 0) Then
            teamRecords!(86, 0) = V8(I, 11)
            teamRecords!(86, 1) = 0
            teamRecords!(86, 2) = 0
            teamRecDesc$(86, 0) = gameTeams$(I)
            teamRecDesc$(86, 1) = VERSUS$(I)
            teamRecDesc$(86, 2) = gameTeams$(1 - I)
            teamRecDesc$(86, 3) = Date$
        End If

        If V8(I, 11) + V8(1 - I, 11) > teamRecords!(88, 0) Then
            teamRecords!(88, 0) = V8(I, 11) + V8(1 - I, 11)
            teamRecords!(88, 1) = V8(I, 11)
            teamRecords!(88, 2) = V8(1 - I, 11)
            teamRecDesc$(88, 0) = gameTeams$(I)
            teamRecDesc$(88, 1) = VERSUS$(I)
            teamRecDesc$(88, 2) = gameTeams$(1 - I)
            teamRecDesc$(88, 3) = Date$
        End If

        If V8(I, 11) + V8(1 - I, 11) < teamRecords!(89, 0) Then
            teamRecords!(89, 0) = V8(I, 11) + V8(1 - I, 11)
            teamRecords!(89, 1) = V8(I, 11)
            teamRecords!(89, 2) = V8(1 - I, 11)
            teamRecDesc$(89, 0) = gameTeams$(I)
            teamRecDesc$(89, 1) = VERSUS$(I)
            teamRecDesc$(89, 2) = gameTeams$(1 - I)
            teamRecDesc$(89, 3) = Date$
        End If

        If QR(I, 1, 11) + QR(I, 2, 11) > teamRecords!(90, 0) Then
            teamRecords!(90, 0) = QR(I, 1, 11) + QR(I, 2, 11)
            teamRecords!(90, 1) = 1
            teamRecords!(90, 2) = QR(I, 1, 11) + QR(I, 2, 11)
            teamRecDesc$(90, 0) = gameTeams$(I)
            teamRecDesc$(90, 1) = VERSUS$(I)
            teamRecDesc$(90, 2) = gameTeams$(1 - I)
            teamRecDesc$(90, 3) = Date$
        End If

        If QR(I, 3, 11) + QR(I, 4, 11) > teamRecords!(90, 0) Then
            teamRecords!(90, 0) = QR(I, 3, 11) + QR(I, 4, 11)
            teamRecords!(90, 1) = 2
            teamRecords!(90, 2) = QR(I, 3, 11) + QR(I, 4, 11)
            teamRecDesc$(90, 0) = gameTeams$(I)
            teamRecDesc$(90, 1) = VERSUS$(I)
            teamRecDesc$(90, 2) = gameTeams$(1 - I)
            teamRecDesc$(90, 3) = Date$
        End If

        ' ASSISTS
        If V8(I, 7) > teamRecords!(96, 0) Then
            teamRecords!(96, 0) = V8(I, 7)
            teamRecords!(96, 1) = 0
            teamRecords!(96, 2) = 0
            teamRecDesc$(96, 0) = gameTeams$(I)
            teamRecDesc$(96, 1) = VERSUS$(I)
            teamRecDesc$(96, 2) = gameTeams$(1 - I)
            teamRecDesc$(96, 3) = Date$
        End If

        If V8(I, 7) < teamRecords!(97, 0) Then
            teamRecords!(97, 0) = V8(I, 7)
            teamRecords!(97, 1) = 0
            teamRecords!(97, 2) = 0
            teamRecDesc$(97, 0) = gameTeams$(I)
            teamRecDesc$(97, 1) = VERSUS$(I)
            teamRecDesc$(97, 2) = gameTeams$(1 - I)
            teamRecDesc$(97, 3) = Date$
        End If

        If V8(I, 7) + V8(1 - I, 7) > teamRecords!(98, 0) Then
            teamRecords!(98, 0) = V8(I, 7) + V8(1 - I, 7)
            teamRecords!(98, 1) = V8(I, 7)
            teamRecords!(98, 2) = V8(1 - I, 7)
            teamRecDesc$(98, 0) = gameTeams$(I)
            teamRecDesc$(98, 1) = VERSUS$(I)
            teamRecDesc$(98, 2) = gameTeams$(1 - I)
            teamRecDesc$(98, 3) = Date$
        End If

        If V8(I, 7) + V8(1 - I, 7) < teamRecords!(99, 0) Then
            teamRecords!(99, 0) = V8(I, 7) + V8(1 - I, 7)
            teamRecords!(99, 1) = V8(I, 7)
            teamRecords!(99, 2) = V8(1 - I, 7)
            teamRecDesc$(99, 0) = gameTeams$(I)
            teamRecDesc$(99, 1) = VERSUS$(I)
            teamRecDesc$(99, 2) = gameTeams$(1 - I)
            teamRecDesc$(99, 3) = Date$
        End If

        If QR(I, 1, 7) + QR(I, 2, 7) > teamRecords!(100, 0) Then
            teamRecords!(100, 0) = QR(I, 1, 7) + QR(I, 2, 7)
            teamRecords!(100, 1) = 1
            teamRecords!(100, 2) = 0
            teamRecDesc$(100, 0) = gameTeams$(I)
            teamRecDesc$(100, 1) = VERSUS$(I)
            teamRecDesc$(100, 2) = gameTeams$(1 - I)
            teamRecDesc$(100, 3) = Date$
        End If

        If QR(I, 3, 7) + QR(I, 4, 7) > teamRecords!(100, 0) Then
            teamRecords!(100, 0) = QR(I, 3, 7) + QR(I, 4, 7)
            teamRecords!(100, 1) = 2
            teamRecords!(100, 2) = 0
            teamRecDesc$(100, 0) = gameTeams$(I)
            teamRecDesc$(100, 1) = VERSUS$(I)
            teamRecDesc$(100, 2) = gameTeams$(1 - I)
            teamRecDesc$(100, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 7) > teamRecords!(102, 0) Then
                teamRecords!(102, 0) = QR(I, I2, 7)
                teamRecords!(102, 1) = I2
                teamRecords!(102, 2) = 0
                teamRecDesc$(102, 0) = gameTeams$(I)
                teamRecDesc$(102, 1) = VERSUS$(I)
                teamRecDesc$(102, 2) = gameTeams$(1 - I)
                teamRecDesc$(102, 3) = Date$
            End If
        Next

        ' FOULS
        If V8(I, 6) > teamRecords!(104, 0) Then
            teamRecords!(104, 0) = V8(I, 6)
            teamRecords!(104, 1) = 0
            teamRecords!(104, 2) = 0
            teamRecDesc$(104, 0) = gameTeams$(I)
            teamRecDesc$(104, 1) = VERSUS$(I)
            teamRecDesc$(104, 2) = gameTeams$(1 - I)
            teamRecDesc$(104, 3) = Date$
        End If

        If V8(I, 6) < teamRecords!(105, 0) Then
            teamRecords!(105, 0) = V8(I, 6)
            teamRecords!(105, 1) = 0
            teamRecords!(105, 2) = 0
            teamRecDesc$(105, 0) = gameTeams$(I)
            teamRecDesc$(105, 1) = VERSUS$(I)
            teamRecDesc$(105, 2) = gameTeams$(1 - I)
            teamRecDesc$(105, 3) = Date$
        End If

        If V8(I, 6) + V8(1 - I, 6) > teamRecords!(106, 0) Then
            teamRecords!(106, 0) = V8(I, 6) + V8(1 - I, 6)
            teamRecords!(106, 1) = V8(I, 6)
            teamRecords!(106, 2) = V8(1 - I, 6)
            teamRecDesc$(106, 0) = gameTeams$(I)
            teamRecDesc$(106, 1) = VERSUS$(I)
            teamRecDesc$(106, 2) = gameTeams$(1 - I)
            teamRecDesc$(106, 3) = Date$
        End If

        If V8(I, 6) + V8(1 - I, 6) < teamRecords!(107, 0) Then
            teamRecords!(107, 0) = V8(I, 6) + V8(1 - I, 6)
            teamRecords!(107, 1) = V8(I, 6)
            teamRecords!(107, 2) = V8(1 - I, 6)
            teamRecDesc$(107, 0) = gameTeams$(I)
            teamRecDesc$(107, 1) = VERSUS$(I)
            teamRecDesc$(107, 2) = gameTeams$(1 - I)
            teamRecDesc$(107, 3) = Date$
        End If

        If QR(I, 1, 6) + QR(I, 2, 6) > teamRecords!(108, 0) Then
            teamRecords!(108, 0) = QR(I, 1, 6) + QR(I, 2, 6)
            teamRecords!(108, 1) = 1
            teamRecords!(108, 2) = 0
            teamRecDesc$(108, 0) = gameTeams$(I)
            teamRecDesc$(108, 1) = VERSUS$(I)
            teamRecDesc$(108, 2) = gameTeams$(1 - I)
            teamRecDesc$(108, 3) = Date$
        End If

        If QR(I, 3, 6) + QR(I, 4, 6) > teamRecords!(108, 0) Then
            teamRecords!(108, 0) = QR(I, 3, 6) + QR(I, 4, 6)
            teamRecords!(108, 1) = 2
            teamRecords!(108, 2) = 0
            teamRecDesc$(108, 0) = gameTeams$(I)
            teamRecDesc$(108, 1) = VERSUS$(I)
            teamRecDesc$(108, 2) = gameTeams$(1 - I)
            teamRecDesc$(108, 3) = Date$
        End If

        If QR(I, 1, 6) + QR(I, 2, 6) + QR(1 - I, 1, 6) + QR(1 - I, 2, 6) > teamRecords!(109, 0) Then
            teamRecords!(109, 0) = QR(I, 1, 6) + QR(I, 2, 6) + QR(1 - I, 1, 6) + QR(1 - I, 2, 6)
            teamRecords!(109, 1) = 1
            teamRecords!(109, 2) = QR(I, 1, 6) + QR(I, 2, 6)
            teamRecDesc$(109, 0) = gameTeams$(I)
            teamRecDesc$(109, 1) = VERSUS$(I)
            teamRecDesc$(109, 2) = gameTeams$(1 - I)
            teamRecDesc$(109, 3) = Date$
        End If

        If QR(I, 3, 6) + QR(I, 4, 6) + QR(1 - I, 3, 6) + QR(1 - I, 4, 6) > teamRecords!(109, 0) Then
            teamRecords!(109, 0) = QR(I, 3, 6) + QR(I, 4, 6) + QR(1 - I, 3, 6) + QR(1 - I, 4, 6)
            teamRecords!(109, 1) = 2
            teamRecords!(109, 2) = QR(I, 3, 6) + QR(I, 4, 6)
            teamRecDesc$(109, 0) = gameTeams$(I)
            teamRecDesc$(109, 1) = VERSUS$(I)
            teamRecDesc$(109, 2) = gameTeams$(1 - I)
            teamRecDesc$(109, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 6) > teamRecords!(110, 0) Then
                teamRecords!(110, 0) = QR(I, I2, 6)
                teamRecords!(110, 1) = I2
                teamRecords!(110, 2) = 0
                teamRecDesc$(110, 0) = gameTeams$(I)
                teamRecDesc$(110, 1) = VERSUS$(I)
                teamRecDesc$(110, 2) = gameTeams$(1 - I)
                teamRecDesc$(110, 3) = Date$
            End If

            If QR(I, I2, 6) + QR(1 - I, I2, 6) > teamRecords!(111, 0) Then
                teamRecords!(111, 0) = QR(I, I2, 6) + QR(1 - I, I2, 6)
                teamRecords!(111, 1) = I2
                teamRecords!(111, 2) = QR(I, I2, 6)
                teamRecDesc$(111, 0) = gameTeams$(I)
                teamRecDesc$(111, 1) = VERSUS$(I)
                teamRecDesc$(111, 2) = gameTeams$(1 - I)
                teamRecDesc$(111, 3) = Date$
            End If
        Next

        ' STEALS
        If V8(I, 9) > teamRecords!(112, 0) Then
            teamRecords!(112, 0) = V8(I, 9)
            teamRecords!(112, 1) = 0
            teamRecords!(112, 2) = 0
            teamRecDesc$(112, 0) = gameTeams$(I)
            teamRecDesc$(112, 1) = VERSUS$(I)
            teamRecDesc$(112, 2) = gameTeams$(1 - I)
            teamRecDesc$(112, 3) = Date$
        End If

        If V8(I, 9) + V8(1 - I, 9) > teamRecords!(114, 0) Then
            teamRecords!(114, 0) = V8(I, 9) + V8(1 - I, 9)
            teamRecords!(114, 1) = V8(I, 9)
            teamRecords!(114, 2) = V8(1 - I, 9)
            teamRecDesc$(114, 0) = gameTeams$(I)
            teamRecDesc$(114, 1) = VERSUS$(I)
            teamRecDesc$(114, 2) = gameTeams$(1 - I)
            teamRecDesc$(114, 3) = Date$
        End If

        If QR(I, 1, 9) + QR(I, 2, 9) > teamRecords!(116, 0) Then
            teamRecords!(116, 0) = QR(I, 1, 9) + QR(I, 2, 9)
            teamRecords!(116, 1) = 1
            teamRecords!(116, 2) = 0
            teamRecDesc$(116, 0) = gameTeams$(I)
            teamRecDesc$(116, 1) = VERSUS$(I)
            teamRecDesc$(116, 2) = gameTeams$(1 - I)
            teamRecDesc$(116, 3) = Date$
        End If

        If QR(I, 3, 9) + QR(I, 4, 9) > teamRecords!(116, 0) Then
            teamRecords!(116, 0) = QR(I, 3, 9) + QR(I, 4, 9)
            teamRecords!(116, 1) = 2
            teamRecords!(116, 2) = 0
            teamRecDesc$(116, 0) = gameTeams$(I)
            teamRecDesc$(116, 1) = VERSUS$(I)
            teamRecDesc$(116, 2) = gameTeams$(1 - I)
            teamRecDesc$(116, 3) = Date$
        End If

        For I2 = 1 To 4
            If QR(I, I2, 9) > teamRecords!(117, 0) Then
                teamRecords!(117, 0) = QR(I, I2, 9)
                teamRecords!(117, 1) = I2
                teamRecords!(117, 2) = 0
                teamRecDesc$(117, 0) = gameTeams$(I)
                teamRecDesc$(117, 1) = VERSUS$(I)
                teamRecDesc$(117, 2) = gameTeams$(1 - I)
                teamRecDesc$(117, 3) = Date$
            End If
        Next

        ' BLOCKS
        If V8(I, 8) > teamRecords!(118, 0) Then
            teamRecords!(118, 0) = V8(I, 8)
            teamRecords!(118, 1) = 0
            teamRecords!(118, 2) = 0
            teamRecDesc$(118, 0) = gameTeams$(I)
            teamRecDesc$(118, 1) = VERSUS$(I)
            teamRecDesc$(118, 2) = gameTeams$(1 - I)
            teamRecDesc$(118, 3) = Date$
        End If

        If V8(I, 8) + V8(1 - I, 8) > teamRecords!(119, 0) Then
            teamRecords!(119, 0) = V8(I, 8) + V8(1 - I, 8)
            teamRecords!(119, 1) = V8(I, 8)
            teamRecords!(119, 2) = V8(1 - I, 8)
            teamRecDesc$(119, 0) = gameTeams$(I)
            teamRecDesc$(119, 1) = VERSUS$(I)
            teamRecDesc$(119, 2) = gameTeams$(1 - I)
            teamRecDesc$(119, 3) = Date$
        End If

        ' TURNOVERS
        If V8(I, 12) > teamRecords!(122, 0) Then
            teamRecords!(122, 0) = V8(I, 12)
            teamRecords!(122, 1) = 0
            teamRecords!(122, 2) = 0
            teamRecDesc$(122, 0) = gameTeams$(I)
            teamRecDesc$(122, 1) = VERSUS$(I)
            teamRecDesc$(122, 2) = gameTeams$(1 - I)
            teamRecDesc$(122, 3) = Date$
        End If

        If V8(I, 12) < teamRecords!(123, 0) Then
            teamRecords!(123, 0) = V8(I, 12)
            teamRecords!(123, 1) = 0
            teamRecords!(123, 2) = 0
            teamRecDesc$(123, 0) = gameTeams$(I)
            teamRecDesc$(123, 1) = VERSUS$(I)
            teamRecDesc$(123, 2) = gameTeams$(1 - I)
            teamRecDesc$(123, 3) = Date$
        End If

        If V8(I, 12) + V8(1 - I, 12) > teamRecords!(124, 0) Then
            teamRecords!(124, 0) = V8(I, 12) + V8(1 - I, 12)
            teamRecords!(124, 1) = V8(I, 12)
            teamRecords!(124, 2) = V8(1 - I, 12)
            teamRecDesc$(124, 0) = gameTeams$(I)
            teamRecDesc$(124, 1) = VERSUS$(I)
            teamRecDesc$(124, 2) = gameTeams$(1 - I)
            teamRecDesc$(124, 3) = Date$
        End If

        If V8(I, 12) + V8(1 - I, 12) < teamRecords!(125, 0) Then
            teamRecords!(125, 0) = V8(I, 12) + V8(1 - I, 12)
            teamRecords!(125, 1) = V8(I, 12)
            teamRecords!(125, 2) = V8(1 - I, 12)
            teamRecDesc$(125, 0) = gameTeams$(I)
            teamRecDesc$(125, 1) = VERSUS$(I)
            teamRecDesc$(125, 2) = gameTeams$(1 - I)
            teamRecDesc$(125, 3) = Date$
        End If

    Next

    Open diskPaths$(1) + "RECORDS.GAM" For Output As #3

    For I = 0 To 48

        For I1 = 0 To 2
            Write #3, indRecords!(I, I1)
        Next

        For I1 = 0 To 4
            Write #3, indRecDesc$(I, I1)
        Next

    Next

    For I = 0 To 125

        For I1 = 0 To 2
            Write #3, teamRecords!(I, I1)
        Next

        For I1 = 0 To 3
            Write #3, teamRecDesc$(I, I1)
        Next

    Next

    Close #3

End Sub


'----------------------------------------
'          GetTeamYear Function
'----------------------------------------
'This subroutine determines the 4-digit
'team year from the team loaded by the
'file. It uses the 2 digit year to make
'assumptions. (00 to 24 are year 2000.)
'The NBL was formed in 1937 and became
'part of the NBA in 1949. Any teams from
'00 to 37 are safe to assume are from 2000's
Function GetTeamYear% (yearFromName$)

    teamYear% = Val(RTrim$(yearFromName$))

    'For 2-digit years
    If teamYear% >= 0 And teamYear% < 37 Then
        GetTeamYear% = 2000 + teamYear%
    Else
        GetTeamYear% = 1900 + teamYear%
    End If

End Function


'----------------------------------------
'          goodPlayer% Function
'----------------------------------------
'This function returns whether the player
'for the specified team / roster index
'is valid for the game. This is based on
'several factors such as roster status,
'player name, or how much the player
'has fouled
Function goodPlayer% (teamIdx, playerIdx, FS%)

    playerFouls = gameRatings!(teamIdx, playerIdx, 12)

    If (plyrRat_GAME(teamIdx, playerIdx, 18) <> 0 Or players_GAME$(teamIdx, playerIdx, 0) = "XXX" Or playerFouls >= FOULS_TO_DQ Or (quarter = 1 And playerFouls >= FOULS_TO_DQ - 4 + FS%) Or (quarter = 2 And playerFouls >= FOULS_TO_DQ - 2 + FS%) Or (quarter = 3 And playerFouls >= FOULS_TO_DQ - 1) Or (quarter = 4 And gameClock! > 300 And playerFouls = FOULS_TO_DQ - 1)) Then
        goodPlayer% = 0
    Else
        goodPlayer% = 1
    End If

End Function


'----------------------------------------
'         playerFatigue% Function
'----------------------------------------
'This functions calculates the player's
'current fatigue level based on their
'existing contribution to the game
Function playerFatigue% (team, plyrIdx)

    'CX = (FGA + Off Reb + Def RB + Fouls) / (Game Minutes)
    playerCX! = gameRatings!(team, plyrIdx, 3)

    'If sClockVal = 2 Then playerCX! = playerCX! * 1.1

    'Fatigue = CX - Curr FGA - Curr Rebounds - Curr Fouls
    playerFatigue% = playerCX! - gameRatings!(team, plyrIdx, 7) - gameRatings!(team, plyrIdx, 11) - gameRatings!(team, plyrIdx, 12)

End Function

'----------------------------------------
'        GetTeamGender$ Function
'----------------------------------------
'This function attempts to determine if
'the team has been loaded from a disk of
'women's teams only". It is only able to
'do this by checking if the last letter
'(ie, 2024W) or first letter (ie, WNBA)
' is a "W", as has been common to do in
'the modding community.
'This obviously would not be useful for
'leagues that would choose a W arbitrarily
'(ie, for a "World" league, etc.)
Function GetTeamGender$ (diskID$)

    If Right$(diskID$, 1) = "W" Or Left$(diskID$, 1) = "W" Then
        GetTeamGender$ = "SHE"
    Else
        GetTeamGender$ = "HE"
    End If

End Function

