SUB BLANK ()

Dim H1$(40)

_Title "Basketball: The Pro Game - Erase League"

5 Cls

APP = 0

4000 Spec$ = D$(1) + "*.LIB"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then
	Locate 10, 8
	Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
	I$ = GetKeyPress$
	Call CSMsgBox("", 0, Pass) 
	Exit Sub
End If

ReDim leagueFiles$(0 To Count%)
For X = 1 To Count%
    leagueFiles$(X) = Space$(12)
Next
leagueFiles$(0) = Spec$
Call ReadFile(leagueFiles$())
'CALL SortStr(VARPTR(leagueFiles$(1)), Count%, 0)

1310 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT LEAGUE TO ERASE": Color 15
SELECTION% = 1
Start% = 1
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Locate 25, 1
Color 14
Print "ENTER";: Color 15
Print "-SELECTS LEAGUE   ";
Color 14
Print "HIT ESCAPE KEY TO EXIT";
Color 15

_MouseShow "DEFAULT"
MAMenu leagueFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then Exit Sub 'Run "HELLO.EXE"
targetFile$ = leagueFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

3911 targetFile$ = RTrim$(targetFile$)
WE$ = Left$(targetFile$, Len(targetFile$) - 4)
LH$ = Left$(WE$, 8)

15 Cls
Locate 10, 8
Call CSMsgBox("WARNING!!!::: THIS WILL ERASE ALL COMPILED STATS AND LEAGUE FILES FOR THIS LEAGUE. IF YOU STILL WISH TO ERASE ALL LEAGUE FILES AND STATS, HIT 'Y' FOR YES", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
If I$ <> "Y" Then 1310

30 Open D$(1) + LH$ + ".TMS" For Input As #2
Input #2, LG$, D1$, D2$, D3$, D4$
For XW = 1 To 40

    35 Input #2, H1$(XW)
Next
Close 2

71 For XW = 1 To 40
    If H1$(XW) = Chr$(42) Then 74

    801 Print "DELETING "; H1$(XW); " STATS"
    Kill (D$(1) + H1$(XW))
    Kill (D$(1) + H1$(XW) + "R")
    Kill (D$(1) + H1$(XW) + "S")
    Kill (D$(1) + H1$(XW) + "D")

74 Next
Cls
GoTo 4000

END SUB
