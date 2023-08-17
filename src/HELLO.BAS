$Debug

Const GAME_VERSION$ = "4.32"
Const MAX_TEAMS = 1000
Const MAX_GAMES = 1500
Const MAX_FILES = 10240
'optimium = 1048576 (but slows code)
Const MAX_CONFERENCES = 40
Const TEAMS_PER_DIV = 10
Const DIVS_PER_CONFERENCE = 4
Const TEAMS_PER_CONFERENCE = 40

Const NUM_STATRECORDS = 100
Const SCHEDULE_SIZE_BYTES = 48

DefInt A-Z

'$INCLUDE: 'FileTeamSelection.bi'
'$INCLUDE: 'KeyInput.bi'
'$INCLUDE: 'QPProEqu.bi'
'$INCLUDE: 'PadRight.bi'
'$INCLUDE: 'Variables.bi'

' For Utils
'$INCLUDE: 'MainRoutines.bi'
'$INCLUDE: 'TITLE.bi'

' For Game
'$INCLUDE: 'GameRoutines.bi'


On Error GoTo Errhandler


'----------------------------------------
'          READ IN ALL DATA
'----------------------------------------
Data 1954,1955,1956,1957,1958,1959,1960,1961,1962,1963
Data 1964,1965,1966,1967,1968,"1968 A",1969,"1969 A",1970,"1970 A"
Data 1971,"1971 A",1972,"1972 A",1973,"1973 A",1974,"1974 A",1975,"1975 A"
Data 1976,"1976 A",1977,1978,1979,1980,1981,1982,1983,1984
Data 1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996
For I = 1 To 52
    Read draftYears$(I)
    While Len(draftYears$(I)) <> 6
        draftYears$(I) = draftYears$(I) + " "
    Wend
Next

' Stats / Year
' FG/RB/FG%/PF/3FG%
'1954 is the 1st year below
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
Data 101,58,45,26,0
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
Data 81,42,47,24,36
'1996 values are below
Data 80,41,46,23,37
For I = 1 To 52
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
    Read R$(I)
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
MO$(0) = "HUMAN OPPONENT           "
MO$(1) = "COMPUTER IS VISITING TEAM"
MO$(2) = "COMPUTER IS HOME TEAM    "
MO$(3) = "COMPUTER VS COMPUTER     "

modeAbbrev$(0) = "HH"
modeAbbrev$(1) = "CV"
modeAbbrev$(2) = "CH"
modeAbbrev$(3) = "CC"

coachMode$(0) = "COACH AND SELECT SHOTS           "
coachMode$(1) = "COACH ONLY AND LET COMPUTER SHOOT"

foulOption$(0) = "ONLY IN BONUS OR ACT OF SHOOTING"
foulOption$(1) = "SHOOT ON ALL FOULS              "

freeThrowOpt$(0) = "CURRENT RULES            "
freeThrowOpt$(1) = "3 TO MAKE 2 / 2 TO MAKE 1"

playoffGame$(0) = "REGULAR SEASON"
playoffGame$(1) = "PLAYOFF       "

sClockOpt$(0) = "24 SEC SHOT CLOCK"
sClockOpt$(1) = "30 SEC SHOT CLOCK"

teamEraOpt$(0) = "VISITING     "
teamEraOpt$(1) = "HOME         "
teamEraOpt$(2) = "NO ADJUSTMENT"

TS$(0) = "NO "
TS$(1) = "YES"

yesNo$(0) = "N"
yesNo$(1) = "Y"

location$(0) = "@ TEAM 1            "
location$(1) = "HOME COURT ADVANTAGE"
location$(2) = "NEUTRAL SITE        "


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
    Close 1
End If

Open "DEV.BAS" For Input As #1
For I = 0 To 1
    Input #1, diskPaths$(I)
Next
Close 1

Selection% = 1

Do

    Call Main_Menu(Selection%)

Loop Until Selection% = 8

End

System


'----------------------------------------

Errhandler:
Open "errlog" For Append As #9
Print #9, "Error #"; Err; "on program file line"; _ErrorLine
If _FileExists("GAME82") Then
    Print #9, "Schedule game # "; Using "###"; AP%(0) + AP%(2)
    Print #9, B$(0) + " vs. " + B$(1)
End If
Print #9,
Close #9
Resume Next ' moves program to code following the error.

'----------------------------------------
'       Main_Menu Subroutine
'----------------------------------------
Sub Main_Menu (menuSelection%)

    ReDim menuChoices$(1 To 8)

    Do

        _Title "Basketball: The Pro Game - Menu"

        Cls

        Call TITLE

        menuChoices$(1) = "PLAY SINGLE GAME                "
        menuChoices$(2) = "TEAM MANAGEMENT / ORGANIZATION  "
        menuChoices$(3) = "REPLAY STATISTICS AND REPORTS   "
        menuChoices$(4) = "LEADERS / STANDINGS / RECORDS   "
        menuChoices$(5) = "SCHEDULER / AUTOPLAY / BOXSCORES"
        menuChoices$(6) = "HARD DRIVE INSTALLATION         "
        menuChoices$(7) = "CONFIGURE DRIVES FOR TEAMS,STATS"
        menuChoices$(8) = "EXIT TO DOS                     "

        menuSelection% = 1
        Count% = 8
        Start% = 1
        'ExitCode% = 8
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 16, 57, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), menuSelection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case menuSelection%
                Case 1: Call PROHOOP
                Case 2: Call TeamMgt_Menu
                Case 3: Call Replay_Menu
                Case 4: Call Leaders_Menu
                Case 5: Call SCHEDULE
                    'Case 6: Run "INSTALL.EXE"
                Case 7: Call ConfigureDrives
                Case 8: Cls
            End Select
        End If

    Loop Until menuSelection% = 8 Or ExitCode% = 27

    If ExitCode% = 27 Then menuSelection% = 8

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

    ReDim menuChoices$(1 To 8)

    Do

        Selection% = 1

        Call TITLE
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
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 27
        Call box0(7, 26, 16, 57, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
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
                Case 8: Cls
            End Select
        End If

    Loop Until Selection% = 8 Or ExitCode% = 27

End Sub


'----------------------------------------
'         Replay_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Replay_Menu ()
    ReDim menuChoices$(1 To 5)

    Selection% = 1

    Do
        Call TITLE
        _Title "Basketball: The Pro Game - Stats Menu"
        menuChoices$(1) = "VIEW / PRINT COMPILED STATISTICS "
        menuChoices$(2) = "ENCYCLOPEDIA/ TEAM REPLAY OPTIONS"
        menuChoices$(3) = "COMPARE REPLAY STATS TO ACTUAL   "
        menuChoices$(4) = "MERGE SEPARATE STATISTICAL FILES "
        menuChoices$(5) = "RETURN TO MAIN MENU              "
        Count% = 5
        Start% = 1
        ExitCode% = 5
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 13, 58, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0
        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call LOOKY
                Case 2: Call CAREER
                Case 3: Call COMPARE
                Case 4: Call MERGE
                Case 5: Cls
            End Select
        End If

    Loop Until Selection% = 5 Or ExitCode% = 27

End Sub


'----------------------------------------
'        Leaders_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Leaders_Menu ()

    ReDim menuChoices$(1 To 8)

    Do

        Call TITLE

        _Title "Basketball: The Pro Game - League Menu"

        menuChoices$(1) = "CREATE/ EDIT A LEAGUE               "
        menuChoices$(2) = "UPDATE STATS/ STANDINGS FOR A LEAGUE"
        menuChoices$(3) = "VIEW/PRINT RESULTS FOR A LEAGUE     "
        menuChoices$(4) = "HEAD-TO-HEAD RESULTS AND STATS      "
        menuChoices$(5) = "LEAGUE RECORD BOOKS                 "
        menuChoices$(6) = "EXPANDED LEADERS/STANDINGS          "
        menuChoices$(7) = "DELETE ALL STATS FOR A LEAGUE       "
        menuChoices$(8) = "RETURN TO MAIN MENU                 "
        Selection% = 1
        Count% = 8
        Start% = 1
        ExitCode% = 8
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 22
        Call box0(7, 21, 16, 58, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1: Call CREATE
                Case 2: Call CompileLeague
                Case 3: Call SEELEAG
                Case 4: Call HD2HDPRO
                Case 5: Call RecordsMenu
                Case 6: Call NEWLDR
                Case 7: Call BLANK

            End Select

        End If

    Loop Until Selection% = 8 Or ExitCode% = 27

End Sub


'$INCLUDE: 'FileTeamSelection.bm'
'$INCLUDE: 'KeyInput.bm'
'$INCLUDE: 'QPProEqu.bm'
'$INCLUDE: 'PadRight.bm'

' For Utils
'$INCLUDE: 'TITLE.bm'
'$INCLUDE: 'ALIGN.BAS'
'$INCLUDE: 'BLANK.BAS'
'$INCLUDE: 'CAREER.BAS'
'$INCLUDE: 'CARVIEW.BAS'
'$INCLUDE: 'COMPARE.BAS'
'$INCLUDE: 'COMPLEAG.BAS'
'$INCLUDE: 'CREATE.BAS'
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

