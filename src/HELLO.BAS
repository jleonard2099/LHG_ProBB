'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

' 4.01 corrects the overflow problem with the draft program

_Title "Basketball: The Pro Game - Menu"
$ExeIcon:'./lhg_bbpro.ico'
_Icon
_AllowFullScreen

DefInt A-Z
DECLARE FUNCTION PadRight$ (X$, Length)

Dim D$(3), menuItems$(9)
Dim A$(1 To 15)

Open "DEV.BAS" For Input As #1
For I = 0 To 1
    Input #1, D$(I)
Next
Close 1

Call Comline(N, A$(), 10) ' Get what was typed on the command line
'FOR I = 1 TO N: PRINT A$(I): NEXT I
If A$(1) = "2" Then A$(1) = "": GoTo 700
If A$(1) = "3" Then A$(1) = "": GoTo 800
If A$(1) = "4" Then A$(1) = "": GoTo 750

1 Call TITLE
ERASE menuItems$
menuItems$(1) = "PLAY SINGLE GAME                "
menuItems$(2) = "TEAM MANAGEMENT / ORGANIZATION  "
menuItems$(3) = "REPLAY STATISTICS AND REPORTS   "
menuItems$(4) = "LEADERS / STANDINGS / RECORDS   "
menuItems$(5) = "SCHEDULER / AUTOPLAY / BOXSCORES"
menuItems$(6) = "HARD DRIVE INSTALLATION         "
menuItems$(7) = "CONFIGURE DRIVES FOR TEAMS,STATS"
menuItems$(8) = "EXIT TO DOS                     "
SELECTION% = 1
Count% = 8
Start% = 1
SCANCODE% = 8
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

Call box0(7, 24, 16, 57, 2, 31)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then End
If SELECTION% = 1 Then Run "PROHOOP.EXE"
If SELECTION% = 2 Then 700
If SELECTION% = 3 Then 750
If SELECTION% = 4 Then 800
If SELECTION% = 5 Then Run "SCHEDULE.EXE"
'If SELECTION% = 6 Then Run "INSTALL.EXE"
If SELECTION% = 7 Then 505
If SELECTION% = 8 Then Cls: End

505 Color 15, 1
FG = 0
BG = 7
For I = 0 To 1
    D$(I) = Left$(D$(I), Len(D$(I)) - 1)
Next
Colr = OneColor%(FG, BG) 'pack FG and BG into a single byte
Cls: Locate , 16: Print "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS": Print

510 Locate , 31: Print "PATHNAME EXAMPLES:": Print

515 Locate , 31: Print "A:"

520 Locate , 31: Print "C:\PROBB\TEAMS"
Locate 7, 1: QPrint "Path for Teams ", 14, 1, -1
Locate , 16: QPrint PadRight$(D$(0), 30), FG, BG, -1
Locate 9, 1: QPrint "Path for Stats ", 14, 1, -1
Locate , 16: QPrint PadRight$(D$(1), 30), FG, BG, -1

GetDiv1:
Locate 7, 16 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
Call TextIn(D$(0), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
If XCode = 2 GoTo Done 'all done entering data
D$(0) = _Trim$(D$(0))
GetDiv2:
Locate 9, 16 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
Call TextIn(D$(1), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
If XCode = 2 GoTo Done 'all done entering data
D$(1) = _Trim$(D$(1))

Done:
Locate 23, 1
Print "Is this okay? (Yes/No) ";
X$ = ""
Call YesNoB(X$, XCode, FG, BG)
Locate , 1
ClearEOL 14, 1
If XCode = 1 GoTo GetDiv2
If X$ = "N" GoTo GetDiv1
If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

580 Open "DEV.BAS" For Output As #1

585 For R = 0 To 2: D$(R) = D$(R) + "\": Next R

590 Print #1, D$(0)

595 Print #1, D$(1)

600 Close #1
GoTo 1

700 Call TITLE
ERASE menuItems$
menuItems$(1) = "INPUT A NEW TEAM              "
menuItems$(2) = "EDIT AN EXISTING TEAM         "
menuItems$(3) = "PRINT TEAM ROSTER / REPORT    "
menuItems$(4) = "VIEW TEAM ROSTER / REPORT     "
menuItems$(5) = "DRAFT TO NEW / EXISTING TEAM  "
menuItems$(6) = "TRADE PLAYERS                 "
menuItems$(7) = "ADJUST PLAYING TIME (MIN/GAME)"
menuItems$(8) = "RETURN TO MAIN MENU           "
SELECTION% = 1
Count% = 8
Start% = 1
SCANCODE% = 8
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 27
Call box0(7, 26, 16, 57, 2, 31)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then 1
If SELECTION% = 1 Then Run "BINPUT.EXE 1"
If SELECTION% = 2 Then Run "BINPUT.EXE 2"
If SELECTION% = 3 Then Run "BINPUT.EXE 3"
If SELECTION% = 4 Then Run "BINPUT.EXE 4"
If SELECTION% = 5 Then Run "DRAFT.EXE"
If SELECTION% = 6 Then Run "TRADE.EXE"
If SELECTION% = 7 Then Run "GPMIN.EXE"
If SELECTION% = 8 Then 1

750 Call TITLE
ERASE menuItems$
menuItems$(1) = "VIEW / PRINT COMPILED STATISTICS "
menuItems$(2) = "ENCYCLOPEDIA/ TEAM REPLAY OPTIONS"
menuItems$(3) = "COMPARE REPLAY STATS TO ACTUAL   "
menuItems$(4) = "MERGE SEPARATE STATISTICAL FILES "
menuItems$(5) = "RETURN TO MAIN MENU              "
SELECTION% = 1
Count% = 5
Start% = 1
SCANCODE% = 5
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

Call box0(7, 24, 13, 58, 2, 31)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then 1
If SELECTION% = 1 Then Run "LOOKY.EXE"
If SELECTION% = 2 Then Run "CAREER.EXE"
If SELECTION% = 3 Then Run "COMPARE.EXE"
If SELECTION% = 4 Then Run "MERGE.EXE"
If SELECTION% = 5 Then 1

800 Call TITLE
ERASE menuItems$
menuItems$(1) = "CREATE/ EDIT A LEAGUE         "
menuItems$(2) = "UPDATE TOP 10'S/ STANDINGS    "
menuItems$(3) = "VIEW/ PRINT TOP 10'S/STANDINGS"
menuItems$(4) = "HEAD-TO-HEAD RESULTS AND STATS"
menuItems$(5) = "LEAGUE RECORD BOOKS           "
menuItems$(6) = "EXPANDED LEADERS/STANDINGS    "
menuItems$(7) = "DELETE ALL STATS FOR A LEAGUE "
menuItems$(8) = "RETURN TO MAIN MENU           "
SELECTION% = 1
Count% = 7
Start% = 1
SCANCODE% = 7
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 26
Call box0(7, 25, 15, 56, 2, 31)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then 1
If SELECTION% = 1 Then Run "CREATE.EXE"
If SELECTION% = 2 Then Run "LEAGCMPL.EXE"
If SELECTION% = 3 Then Run "SEELEAG.EXE"
If SELECTION% = 4 Then Run "HD2HD.EXE"
If SELECTION% = 5 Then Run "REC.EXE"
If SELECTION% = 6 Then Run "NEWLDR.EXE"
If SELECTION% = 7 Then Run "BLANK.EXE"
If SELECTION% = 8 Then 1


'------------------------------
'      PadRight Function
'------------------------------
' Pads string to the right with spaces
Function PadRight$ (X$, Length)
    PadRight$ = X$ + Space$(Length - Len(X$)) 'for display only
End Function


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 27: Color 12
    Print "BASKETBALL: THE PRO GAME V4.1"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1997"; Tab(70); Left$(Time$, 5);
    Locate 3, 1: Color 2, 0
    Color 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
    Print "       FOR TECHNICAL SUPPORT, CALL (800) 477-7032 OR SportGames@AOL.COM"
    Color 10
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
