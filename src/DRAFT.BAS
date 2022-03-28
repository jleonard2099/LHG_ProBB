'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Basketball: The Pro Game - Team Draft"
''$ExeIcon:'./lhg_bbpro.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB TITLE ()

Dim L%(1, 6), T%(1, 34), B%(1, 14, 19), B$(1, 14, 1), A$(1, 4), Q$(377), V$(500), V%(500)
Dim YR$(52), LG%(52, 5), LT%(14, 34), menuItems$(3), D$(2), YN$(2)
Dim TTL(34) As Single, Y As Long
GoSub 8500

Open "DEV.BAS" For Input As #1: Input #1, D$(0): Close 1

10 For I = 0 To 1: A$(0, I) = "XXX": Next
For I = 0 To 6: L%(0, I) = 0: Next
For I = 0 To 34: T%(0, I) = 0: TTL(I) = 0: Next
For I = 0 To 14: For I1 = 0 To 34: LT%(I, I1) = 0: Next: Next
For I = 0 To 14
    B$(0, I, 0) = "XXX"
    B$(0, I, 1) = "X"
    For I1 = 0 To 19
        B%(0, I, I1) = 0
Next: Next
Call TITLE
menuItems$(1) = "DRAFT A NEW TEAM         "
menuItems$(2) = "DRAFT TO AN EXISTING TEAM"
menuItems$(3) = "EXIT TO MAIN MENU        "
SELECTION% = 1
Count% = 3
Start% = 1
SCANCODE% = 3
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

Call box0(7, 24, 11, 50, 2, 31)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Or SELECTION% = 3 Then 1
If SELECTION% = 1 Then DNT% = 0: GoTo 100
If SELECTION% = 2 Then DNT% = 1: GoTo 2000

1 Run "HELLO.EXE 2"

100 Cls

112 Color 14: Print "LEAGUE INFORMATION": Color 15
L%(0, 0) = 80
L%(0, 1) = LG%(YR%, 1)
L%(0, 2) = LG%(YR%, 2)
L%(0, 3) = LG%(YR%, 3)
L%(0, 4) = LG%(YR%, 4)
L%(0, 5) = LG%(YR%, 5)
L%(0, 6) = 0

130 Locate 2: Input "1. DISK ID "; YN$(0): If C1 = 1 Then 250

205 Locate 3: Input "2. TEAM NAME "; A$(0, 0): If C1 = 1 Then 250

210 Locate 4: Input "3. TEAM NICKNAME "; A$(0, 1): If C1 = 1 Then 250

240 Locate 5: Input "4. TEXT COLOR (0-15) "; T%(0, 33): If C1 = 1 Then 250

245 Locate 6: Input "5. BCKGRND COLOR (0-7) "; T%(0, 34): If C1 = 1 Then 250

246 Locate 7: Input "6. TEAM ABBREV (IE. BOS) "; A$(0, 2): If C1 = 1 Then 250

247 Locate 8: Input "7. COACH'S NAME "; A$(0, 3): If C1 = 1 Then 250

248 Locate 9: Input "8. ARENA NAME "; A$(0, 4): If C1 = 1 Then 250

250 Locate 10: Color T%(0, 33), T%(0, 34): Print "SAMPLE TEXT": Color 7, 0

255 C1 = 1: Locate 12: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 130, 205, 210, 240, 245, 246, 247, 248

258 C1 = 0
GoTo 450

260 Cls: Color 14: Print "TEAM OFFENSIVE INFORMATION": Color 15

265 Locate 2: Input " 1. FGM "; T%(0, 6): If C1 = 1 Then 335

270 Locate 3: Input " 2. FGA "; T%(0, 5): If C1 = 1 Then 335

275 Locate 4: Input " 3. FTM "; T%(0, 8): If C1 = 1 Then 335

280 Locate 5: Input " 4. FTA "; T%(0, 7): If C1 = 1 Then 335

285 Locate 6: Input " 5. OFF REB "; T%(0, 9): If C1 = 1 Then 335

290 Locate 7: Input " 6. DEF REB "; T%(0, 10): If C1 = 1 Then 335

291 If T%(0, 9) = 0 And RB = 0 Then T%(0, 9) = .32 * T%(0, 10): T%(0, 10) = T%(0, 10) - T%(0, 9): Locate 6: Print " 5. OFF REB "; T%(0, 9): Locate 7: Print " 6. DEF REB "; T%(0, 10)

295 Locate 8: Input " 7. ASSISTS "; T%(0, 11): If C1 = 1 Then 335

300 Locate 9: Input " 8. FOULS "; T%(0, 12): If C1 = 1 Then 335

305 Locate 10: Input " 9. DISQ "; T%(0, 13): If C1 = 1 Then 335

310 Locate 11: Input "10. STEALS "; T%(0, 14): If C1 = 1 Then 335

315 Locate 12: Input "11. TURNOVERS "; T%(0, 15): If C1 = 1 Then 335

320 Locate 13: Input "12. BLK SHOTS "; T%(0, 16): If C1 = 1 Then 335

325 Locate 14: Input "13. 3-FGM "; T%(0, 18): If C1 = 1 Then 335

330 Locate 15: Input "14. 3-FGA "; T%(0, 17): If C1 = 1 Then 335

335 C1 = 1: Locate 17: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 265, 270, 275, 280, 285, 290, 295, 300, 305, 310, 315, 320, 325, 330

336 C1 = 0

340 Cls: Color 14: Print "TEAM DEFENSIVE INFORMATION": Color 15

345 Locate 2: Input " 1. FGM "; T%(0, 20): If C1 = 1 Then 415

350 Locate 3: Input " 2. FGA "; T%(0, 19): If C1 = 1 Then 415

355 Locate 4: Input " 3. FTM "; T%(0, 22): If C1 = 1 Then 415

360 Locate 5: Input " 4. FTA "; T%(0, 21): If C1 = 1 Then 415

365 Locate 6: Input " 5. OFF REB "; T%(0, 23): If C1 = 1 Then 415

370 Locate 7: Input " 6. DEF REB "; T%(0, 24): If C1 = 1 Then 415

371 If T%(0, 23) = 0 And RB = 0 Then T%(0, 23) = .35 * T%(0, 24): T%(0, 24) = T%(0, 24) - T%(0, 23): Locate 6: Print " 5. OFF REB "; T%(0, 23): Locate 7: Print " 6. DEF REB "; T%(0, 24)

375 Locate 8: Input " 7. ASSISTS "; T%(0, 25): If C1 = 1 Then 415

380 Locate 9: Input " 8. FOULS "; T%(0, 26): If C1 = 1 Then 415

385 Locate 10: Input " 9. DISQ "; T%(0, 27): If C1 = 1 Then 415

390 Locate 11: Input "10. STEALS "; T%(0, 28): If C1 = 1 Then 415

395 Locate 12: Input "11. TURNOVERS "; T%(0, 29): If C1 = 1 Then 415

400 Locate 13: Input "12. BLK SHOTS "; T%(0, 30): If C1 = 1 Then 415

405 Locate 14: Input "13. 3-FGM "; T%(0, 32): If C1 = 1 Then 415

410 Locate 15: Input "14. 3-FGA "; T%(0, 31): If C1 = 1 Then 415

415 C1 = 1: Locate 17: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 345, 350, 355, 360, 365, 370, 375, 380, 385, 390, 395, 400, 405, 410

416 C1 = 0

417 If T%(0, 28) = 0 And ST = 0 Then T%(0, 28) = T%(0, 5) * .1

418 If T%(0, 14) = 0 And ST = 0 Then T%(0, 14) = T%(0, 19) * .1

419 If T%(0, 16) = 0 And BK = 0 Then T%(0, 16) = T%(0, 19) * .06

420 If T%(0, 30) = 0 And BK = 0 Then T%(0, 30) = T%(0, 5) * .06

421 If T%(0, 29) = 0 And T0 = 0 Then T%(0, 29) = 18 * L%(0, 0): T%(0, 15) = T%(0, 29)

450 Color 0, 0: Cls: Color T%(0, 33), T%(0, 34): Print A$(0, 0);: Color 14, 0: Print "     (DRAFTING)"

451 Print: Print "PLAYERS"

452 For I = 0 To 14
    Color 2: Print Using "## "; I;: Color 15: Print B$(0, I, 0); " "; B$(0, I, 1)
Next

454 Locate 21, 1
Color 14: Print "(1) ";: Color 15: Print "DRAFT PLAYER"
Color 14: Print "(Z) ";: Color 15: Print "SAVE TEAM"
455 I$ = GetKeyPress$

456 If UCase$(I$) = "Z" Then 1000

457 If I$ = "1" Then 470

458 GoTo 454

470 Locate 21, 20: Print "SLOT # (0-14) ";

471 N = 1: Input B

472 If B < 0 Or B > 14 Then 470

1440 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print "ABA SEASONS ARE FOLLOWED BY AN 'A' (IE. 75A)."
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 1102: GoTo 182
YN$(1) = NY$
For I = 1 To 500: V$(I) = "": Next
GoSub 1100

182 If BO% = 1 Then BO% = 0: GoTo 1440

183 Locate 2, 33: Color T%(1, 33), T%(1, 34): Print A$(1, 0): Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1440

550 Color , 0: Cls: Color T%(1, 33), T%(1, 34): Print A$(1, 0): Color 14, 0

551 Print: Print "PLAYERS"

552 For I = 0 To 14
    Color 2: Print Using "## "; I;: Color 15: Print B$(1, I, 0); " "; B$(1, I, 1)
Next

560 Color 14: Locate 21, 1: Print "SELECT PLAYER # TO DRAFT ";

561 Input B1

562 If B1 < 0 Or B1 > 14 Then 560

571 For I = 0 To 1: B$(0, B, I) = B$(1, B1, I): Next

572 For I = 0 To 19: B%(0, B, I) = B%(1, B1, I): Next
For I = 0 To 34: LT%(B, I) = T%(1, I): Next
B%(0, B, 2) = B%(0, B, 2) * (LG%(YR%, 1) / L%(1, 1))
B%(0, B, 6) = B%(0, B, 6) * (LG%(YR%, 2) / L%(1, 2))
B%(0, B, 7) = B%(0, B, 7) * (LG%(YR%, 2) / L%(1, 2))
FG = Int(B%(1, B1, 3) / B%(1, B1, 2) * 100 + .5)
FG = FG * (LG%(YR%, 3) / L%(1, 3))
B%(0, B, 3) = FG / 100 * B%(0, B, 2)
LT%(B, 5) = (LG%(YR%, 1) / L%(1, 1)) * LT%(B, 5)
LT%(B, 19) = LT%(B, 19) * (LG%(YR%, 1) / L%(1, 1))
LT%(B, 9) = LT%(B, 9) * (LG%(YR%, 2) / L%(1, 2))
LT%(B, 10) = LT%(B, 10) * (LG%(YR%, 2) / L%(1, 2))
LT%(B, 23) = LT%(B, 23) * (LG%(YR%, 2) / L%(1, 2))
LT%(B, 24) = LT%(B, 24) * (LG%(YR%, 2) / L%(1, 2))
FG = Int(T%(1, 6) / T%(1, 5) * 100 + .5)
FG = FG * (LG%(YR%, 3) / L%(1, 3))
T%(0, 6) = FG / 100 * T%(0, 5)
FG = Int(T%(1, 20) / T%(1, 19) * 100 + .5)
FG = FG * (LG%(YR%, 3) / L%(1, 3))
T%(0, 20) = FG / 100 * T%(0, 19)

574 GoTo 450

1000 Cls

1002 GoSub 9800

1008 GoTo 10

1100 NT = 0
Open D$(0) + "PROBBTMS." + YN$(1) For Random As #1 Len = 1031
Y = LOF(1)
Field #1, 15 As Q$(0), 15 As Q$(1)
For I = 0 To 6
    Field #1, 30 + I * 2 As X$, 2 As Q$(I + 2)
Next
Field #1, 44 As X$, 2 As Q$(9)
For I = 0 To 34
    Field #1, 46 + I * 2 As X$, 2 As Q$(I + 10)
Next
For I = 0 To 14
    Field #1, 116 + I * 57 As X$, 15 As Q$(I * 22 + 45), 2 As Q$(I * 22 + 46)
    For I1 = 0 To 19
        Field #1, 133 + I * 57 + I1 * 2 As X$, 2 As Q$(I * 22 + 47 + I1)
    Next
Next
Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)
For I1 = 1 To Y / 1031
    Get #1, I1
    CK% = CVI(Q$(9))
    If CK% <> 999 Then 1103
    NT = NT + 1
    V$(NT) = Q$(0): V%(NT) = I1

1103 Next
Close 1

1102 SELECTION% = 1
Count% = NT
Start% = 1
FileFGColr% = 15: FileBGColr% = 4
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Color , 4: Cls: Locate , 33: Color 14
Print "TEAM SELECTION": Color 15
Locate 25, 1: Color 14: Print "ESC";: Color 15
Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
Print "ENTER";: Color 15: Print "-SELECTS TEAM";
    
_MouseShow "DEFAULT"
120 MAMenu V$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 120
If EXITCODE% = -79 Then Start% = NT: EXITCODE% = 3: SELECTION% = NT: GoTo 120
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NT Then Start% = NT
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 120
D = V%(SELECTION%)
GoTo 9000

1200 NT = 0
Open D$(0) + "PROBBTMS." + YN$(0) For Input As #1
Close 1
Open D$(0) + "PROBBTMS." + YN$(0) For Random As #1 Len = 1031
Y = LOF(1)
Field #1, 15 As Q$(0), 15 As Q$(1)
For I = 0 To 6
    Field #1, 30 + I * 2 As X$, 2 As Q$(I + 2)
Next
Field #1, 44 As X$, 2 As Q$(9)
For I = 0 To 34
    Field #1, 46 + I * 2 As X$, 2 As Q$(I + 10)
Next
For I = 0 To 14
    Field #1, 116 + I * 57 As X$, 15 As Q$(I * 22 + 45), 2 As Q$(I * 22 + 46)
    For I1 = 0 To 19
        Field #1, 133 + I * 57 + I1 * 2 As X$, 2 As Q$(I * 22 + 47 + I1)
    Next
Next
Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)
For I1 = 1 To Y / 1031
    Get #1, I1
    CK% = CVI(Q$(9))
    If CK% <> 999 Then 1203
    NT = NT + 1
    V$(NT) = Q$(0): V%(NT) = I1

1203 Next
Close 1
TS% = Int((NT - 1) / 80)

1202 SELECTION% = 1
Count% = NT
Start% = 1
FileFGColr% = 15: FileBGColr% = 4
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Color , 4: Cls: Locate , 33: Color 14
Print "TEAM SELECTION": Color 15
Locate 25, 1: Color 14: Print "ESC";: Color 15
Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
Print "ENTER";: Color 15: Print "-SELECTS TEAM";
    
_MouseShow "DEFAULT"
220 MAMenu V$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 220
If EXITCODE% = -79 Then Start% = NT: EXITCODE% = 3: SELECTION% = NT: GoTo 220
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NT Then Start% = NT
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 220
T = V%(SELECTION%)
GoTo 9100

2000 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print "ABA SEASONS ARE FOLLOWED BY AN 'A' (IE. 75A)."
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 1102: GoTo 2001
YN$(0) = NY$
For I = 1 To 500: V$(I) = "": Next
GoSub 1200

2001 If BO% = 1 Then BO% = 0: GoTo 2000

2002 Locate 2, 33: Color T%(0, 33), T%(0, 34): Print A$(0, 0): Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)"
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 2000
GoTo 450
4000 I$ = GetKeyPress$

4001 If UCase$(I$) = "Y" Then I = 1: GoTo 4005

4002 If UCase$(I$) = "N" Then I = 0: GoTo 4005

4003 GoTo 4000

4005 Return

9000 Open D$(0) + "PROBBTMS." + YN$(1) For Random As #1 Len = 1031
Field #1, 15 As Q$(0), 15 As Q$(1)
For J% = 0 To 6
    Field #1, 30 + J% * 2 As X$, 2 As Q$(J% + 2)
Next
Field #1, 44 As X$, 2 As Q$(9)
For J% = 0 To 34
    Field #1, 46 + J% * 2 As X$, 2 As Q$(J% + 10)
Next
For J% = 0 To 14
    Field #1, 116 + J% * 57 As X$, 15 As Q$(J% * 22 + 45), 2 As Q$(J% * 22 + 46)
    For J1 = 0 To 19
        Field #1, 133 + J% * 57 + J1 * 2 As X$, 2 As Q$(J% * 22 + 47 + J1)
    Next
Next
Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)
Get #1, D
A$(1, 0) = Q$(0): A$(1, 1) = Q$(1)
For I = 0 To 6: L%(1, I) = CVI(Q$(I + 2)): Next

9002 If Right$(A$(1, 0), 1) = " " Then A$(1, 0) = Left$(A$(1, 0), Len(A$(1, 0)) - 1): GoTo 9002
For I = 0 To 34: T%(1, I) = CVI(Q$(I + 10)): Next
For I = 0 To 14
    B$(1, I, 0) = Q$(22 * I + 45)

    9004 If Right$(B$(1, I, 0), 1) = " " Then B$(1, I, 0) = Left$(B$(1, I, 0), Len(B$(1, I, 0)) - 1): GoTo 9004
    B$(1, I, 1) = Q$(22 * I + 46)

    9005 If Right$(B$(1, I, 1), 1) = " " Then B$(1, I, 1) = Left$(B$(1, I, 1), Len(B$(1, I, 1)) - 1): GoTo 9005
    For I1 = 0 To 19
        B%(1, I, I1) = CVI(Q$(22 * I + 47 + I1))
Next: Next
A$(1, 2) = Q$(375): A$(1, 3) = Q$(376): A$(1, 4) = Q$(377)
Close 1

9006 If Right$(A$(1, 2), 1) = " " Then A$(1, 2) = Left$(A$(1, 2), Len(A$(1, 2)) - 1): GoTo 9006

9007 If Right$(A$(1, 3), 1) = " " Then A$(1, 3) = Left$(A$(1, 3), Len(A$(1, 3)) - 1): GoTo 9007

9008 If Right$(A$(1, 4), 1) = " " Then A$(1, 4) = Left$(A$(1, 4), Len(A$(1, 4)) - 1): GoTo 9008
Return

9100 Open D$(0) + "PROBBTMS." + YN$(0) For Random As #1 Len = 1031
Field #1, 15 As Q$(0), 15 As Q$(1)
For J% = 0 To 6
    Field #1, 30 + J% * 2 As X$, 2 As Q$(J% + 2)
Next
Field #1, 44 As X$, 2 As Q$(9)
For J% = 0 To 34
    Field #1, 46 + J% * 2 As X$, 2 As Q$(J% + 10)
Next
For J% = 0 To 14
    Field #1, 116 + J% * 57 As X$, 15 As Q$(J% * 22 + 45), 2 As Q$(J% * 22 + 46)
    For J1 = 0 To 19
        Field #1, 133 + J% * 57 + J1 * 2 As X$, 2 As Q$(J% * 22 + 47 + J1)
    Next
Next
Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)
Get #1, T
A$(0, 0) = Q$(0): A$(0, 1) = Q$(1)
For I = 0 To 6: L%(0, I) = CVI(Q$(I + 2)): Next

9102 If Right$(A$(0, 0), 1) = " " Then A$(0, 0) = Left$(A$(0, 0), Len(A$(0, 0)) - 1): GoTo 9102
For I = 0 To 34: T%(0, I) = CVI(Q$(I + 10)): Next
For I = 0 To 14
    B$(0, I, 0) = Q$(22 * I + 45)

    9104 If Right$(B$(0, I, 0), 1) = " " Then B$(0, I, 0) = Left$(B$(0, I, 0), Len(B$(0, I, 0)) - 1): GoTo 9104
    B$(0, I, 1) = Q$(22 * I + 46)

    9105 If Right$(B$(0, I, 1), 1) = " " Then B$(0, I, 1) = Left$(B$(0, I, 1), Len(B$(0, I, 1)) - 1): GoTo 9105
    For I1 = 0 To 19
        B%(0, I, I1) = CVI(Q$(22 * I + 47 + I1))
Next: Next
A$(0, 2) = Q$(375): A$(0, 3) = Q$(376): A$(0, 4) = Q$(377)
Close 1

9106 If Right$(A$(0, 2), 1) = " " Then A$(0, 2) = Left$(A$(0, 2), Len(A$(0, 2)) - 1): GoTo 9106

9107 If Right$(A$(0, 3), 1) = " " Then A$(0, 3) = Left$(A$(0, 3), Len(A$(0, 3)) - 1): GoTo 9107

9108 If Right$(A$(0, 4), 1) = " " Then A$(0, 4) = Left$(A$(0, 4), Len(A$(0, 4)) - 1): GoTo 9108
For I1 = 0 To 14
    For I = 0 To 34
        LT%(I1, I) = T%(0, I)
    Next
Next
Return

9800 NP = 0
For I = 0 To 14
    If B$(0, I, 0) <> "XXX" Then NP = NP + 1
Next
For I = 0 To 14
    If B$(0, I, 0) = "XXX" Then 9802
    For I1 = 0 To 32
        TTL(I1) = TTL(I1) + LT%(I, I1)
    Next I1

9802 Next I
For I1 = 0 To 32
    TTL(I1) = TTL(I1) / NP
    T%(0, I1) = TTL(I1)
Next
Open D$(0) + "PROBBTMS." + YN$(0) For Random As #1 Len = 1031
Y = LOF(1)
If DNT% = 0 Then T = Y / 1031 + 1
Field #1, 15 As Q$(0), 15 As Q$(1)
For I = 0 To 6
    Field #1, 30 + I * 2 As X$, 2 As Q$(I + 2)
Next
Field #1, 44 As X$, 2 As Q$(9)
For I = 0 To 34
    Field #1, 46 + I * 2 As X$, 2 As Q$(I + 10)
Next
For I = 0 To 14
    Field #1, 116 + I * 57 As X$, 15 As Q$(I * 22 + 45), 2 As Q$(I * 22 + 46)
    For I1 = 0 To 19
        Field #1, 133 + I * 57 + I1 * 2 As X$, 2 As Q$(I * 22 + 47 + I1)
    Next
Next
Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)
LSet Q$(0) = A$(0, 0): LSet Q$(1) = A$(0, 1): For I = 0 To 6: LSet Q$(I + 2) = MKI$(L%(0, I)): Next
LSet Q$(9) = MKI$(999)
For I = 0 To 34: LSet Q$(I + 10) = MKI$(T%(0, I)): Next
For I = 0 To 14
    LSet Q$(22 * I + 45) = B$(0, I, 0)
    LSet Q$(22 * I + 46) = B$(0, I, 1)
    For I1 = 0 To 19
        LSet Q$(22 * I + 47 + I1) = MKI$(B%(0, I, I1))
Next: Next
LSet Q$(375) = A$(0, 2): LSet Q$(376) = A$(0, 3): LSet Q$(377) = A$(0, 4)
Put #1, T: Close 1

9821 Return

Data 1954,1955,1956,1957,1958,1959,1960,1961,1962,1963
Data 1964,1965,1966,1967,1968,"1968 A",1969,"1969 A",1970,"1970 A"
Data 1971,"1971 A",1972,"1972 A",1973,"1973 A",1974,"1974 A",1975,"1975 A"
Data 1976,"1976 A",1977,1978,1979,1980,1981,1982,1983,1984
Data 1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996

8500 For I = 1 To 52
    Read YR$(I)

    8501 If Len(YR$(I)) <> 6 Then YR$(I) = YR$(I) + " ": GoTo 8501
Next
For I = 1 To 52: For I1 = 1 To 5
    Read LG%(I, I1)
Next: Next
' FG/RB/FG%/PF/3FG%
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
Data 80,41,46,23,37

8000 SELECTION% = 1
Count% = 52
Start% = 1
FileFGColr% = 15: FileBGColr% = 4
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 11
Columns% = 5
Spacing% = 10
Row% = 6
Column% = 1
Color , 4: Cls: Locate , 16: Color 14: Print "SELECT YEAR TO NORMALIZE DRAFTED PLAYERS TO": Color 15

8001
_MouseShow "DEFAULT"
MAMenu YR$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% <> 27 And EXITCODE% <> 13 Then 8001
If EXITCODE% = 27 Then BO% = 1: Return

184 Locate 3, 33: Color 15, 0: Print YR$(SELECTION%): Color 15, 4
Locate , 30: Print "ANY CHANGE (YN)";: I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 8000
If UCase$(I$) <> "N" Then 184
YR% = SELECTION%
Return


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 27: Color 12
    Print "BASKETBALL: THE PRO GAME V4.01"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1997"; Tab(70); Left$(Time$, 5);
    Locate 3, 1: Color 2, 0
    Color 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
    Print "     FOR TECHNICAL SUPPORT, CALL (800) 477-7032 OR SportGames@AOL.COM"
    Color 10
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
