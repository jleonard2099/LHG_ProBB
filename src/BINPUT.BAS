'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Basketball: The Pro Game - Team Input"
''$ExeIcon:'./lhg_bbpro.ico'
'_Icon
_AllowFullScreen

DECLARE SUB TITLE ()

Dim B%(15, 19), B$(15, 1), T%(35), L%(6), Q$(377), teamItems$(500), V%(500), T$(4)
Dim D$(1), P$(2), A$(5), menuItems$(6)
P$(0) = "G": P$(1) = "F": P$(2) = "C"

Open "DEV.BAS" For Input As #1
Input #1, D$(0): Close #1
Call Comline(N, A$(), 10) ' Get what was typed on the command line
'FOR I = 1 TO N: PRINT A$(I): NEXT I
If A$(1) = "1" Then RN = 1: GoTo 900
If A$(1) = "2" Then RN = 2: GoTo 900
If A$(1) = "3" Then RN = 3: GoTo 900
If A$(1) = "4" Then RN = 4: GoTo 900

100 Call TITLE
menuItems$(1) = "ENTER A NEW TEAM               "
menuItems$(2) = "CHANGE OR EDIT AN EXISTING TEAM"
menuItems$(3) = "PRINT A TEAM ROSTER            "
menuItems$(4) = "VIEW A TEAM ROSTER             "
menuItems$(5) = "EXIT TO MAIN MENU              "
SELECTION% = 1
Count% = 5
Start% = 1
SCANCODE% = 5
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

CALL box0(7, 24, 13, 56, 2,  15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
RN = SELECTION%
If SCANCODE% = 27 Then RN = 5

901 If RN = 5 And D% >= 67 Then Run "HELLO.EXE 2"
If RN = 5 Then Run "HELLO.EXE 2"

900 Q = RN - 1
If Q > 0 Then 1435

101 Cls: Print: Color 12: Print "DO YOU HAVE THE FOLLOWING INFORMATION :"
Print: Color 14: Print "(1) ";: Color 15: Print "TEAM/INDIVID STEALS ";: GoSub 4000
If I$ = Chr$(27) Then RN = 5: GoTo 901
Print I$: ST = I
Print: Color 14: Print "(2) ";: Color 15: Print "TEAM/INDIVID BLOCKS ";: GoSub 4000
If I$ = Chr$(27) Then RN = 5: GoTo 901
Print I$: BK = I
Print: Color 14: Print "(3) ";: Color 15: Print "TEAM/INDIVID OFF/DEF REB ";: GoSub 4000
If I$ = Chr$(27) Then RN = 5: GoTo 901
Print I$: RB = I
Print: Color 14: Print "(4) ";: Color 15: Print "TEAM TURNOVERS ";: GoSub 4000
If I$ = Chr$(27) Then RN = 5: GoTo 901
Print I$: T0 = I
Print: Print "ANY CHANGE (YN)";: GoSub 4000
If I$ = Chr$(27) Then RN = 5: GoTo 901
If I = 1 Then 101
Cls: Color 14: Print "LEAGUE INFORMATION": Color 15

120 Locate 3: Input "1. NUMBER OF GAMES "; L%(0): If C1 = 1 Then 190

130 Locate 4: Input "2. AVG FGA/G "; L%(1): If C1 = 1 Then 190

140 Locate 5: Input "3. AVG RB/G "; L%(2): If C1 = 1 Then 190

150 Locate 6: Input "4. LEAGUE FG% "; L%(3): If C1 = 1 Then 190

160 Locate 7: Input "5. AVG PF/G "; L%(4): If C1 = 1 Then 190

170 Locate 8: Input "6. LEAGUE 3FG% "; L%(5): If C1 = 1 Then 190

171 Locate 9: Input "7. LEAGUE POWER ADJ "; L%(6): If C1 = 1 Then 190

172 Locate 10: Input "8. DISK ID "; YN$: If C1 = 1 Then 190

173 Locate 11: Input "9. TEAM NUMBER "; T: If C1 = 1 Then 190

190 C1 = 1: Locate 13: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 120, 130, 140, 150, 160, 170, 171, 172, 173
C1 = 0: If X% > 0 Then 700
C1 = 0
Cls: Color 14: Print "TEAM INFORMATION": Color 15

205 Locate 2: Input "1. TEAM NAME "; T$(0): If C1 = 1 Then 250

210 Locate 3: Input "2. TEAM NICKNAME "; T$(1): If C1 = 1 Then 250

215 Locate 4: Input "3. TEAM GAMES PLAYED "; T%(0): If C1 = 1 Then 250

220 Locate 5: Input "4. HOME WINS "; T%(1): If C1 = 1 Then 250

225 Locate 6: Input "5. HOME LOSSES "; T%(2): If C1 = 1 Then 250

230 Locate 7: Input "6. ROAD WINS "; T%(3): If C1 = 1 Then 250

235 Locate 8: Input "7. ROAD LOSSES "; T%(4): If C1 = 1 Then 250

240 Locate 9: Input "8. TEXT COLOR (0-15) "; T%(33): If C1 = 1 Then 250

245 Locate 10: Input "9. BCKGRND COLOR (0-7) "; T%(34): If C1 = 1 Then 250

246 Locate 11: Input "10. TEAM ABBREV (IE. BOS) "; T$(2): If C1 = 1 Then 250

247 Locate 12: Input "11. COACH'S NAME "; T$(3): If C1 = 1 Then 250

248 Locate 13: Input "12. ARENA NAME "; T$(4): If C1 = 1 Then 250

250 Locate 15: Color T%(33), T%(34): Print "SAMPLE TEXT": Color 15, 0

255 C1 = 1: Locate 17: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 205, 210, 215, 220, 225, 230, 235, 240, 245, 246, 247, 248
C1 = 0: If X% > 0 Then 760
Cls: Color 14: Print "TEAM OFFENSIVE INFORMATION": Color 15

265 Locate 2: Input " 1. FGM "; T%(6): If C1 = 1 Then 335

270 Locate 3: Input " 2. FGA "; T%(5): If C1 = 1 Then 335

275 Locate 4: Input " 3. FTM "; T%(8): If C1 = 1 Then 335

280 Locate 5: Input " 4. FTA "; T%(7): If C1 = 1 Then 335

285 Locate 6: Input " 5. OFF REB "; T%(9): If C1 = 1 Then 335

290 Locate 7: Input " 6. DEF REB "; T%(10): If C1 = 1 Then 335
If T%(9) = 0 And RB = 0 Then T%(9) = .32 * T%(10): T%(10) = T%(10) - T%(9): Locate 6: Print " 5. OFF REB "; T%(9): Locate 7: Print " 6. DEF REB "; T%(10)

295 Locate 8: Input " 7. ASSISTS "; T%(11): If C1 = 1 Then 335

300 Locate 9: Input " 8. FOULS "; T%(12): If C1 = 1 Then 335

305 Locate 10: Input " 9. DISQ "; T%(13): If C1 = 1 Then 335

310 Locate 11: Input "10. STEALS "; T%(14): If C1 = 1 Then 335

315 Locate 12: Input "11. TURNOVERS "; T%(15): If C1 = 1 Then 335

320 Locate 13: Input "12. BLK SHOTS "; T%(16): If C1 = 1 Then 335

325 Locate 14: Input "13. 3-FGM "; T%(18): If C1 = 1 Then 335

330 Locate 15: Input "14. 3-FGA "; T%(17): If C1 = 1 Then 335

335 C1 = 1: Locate 17: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 265, 270, 275, 280, 285, 290, 295, 300, 305, 310, 315, 320, 325, 330
C1 = 0: If X% > 0 Then 840
Cls: Color 14: Print "TEAM DEFENSIVE INFORMATION": Color 15

345 Locate 2: Input " 1. FGM "; T%(20): If C1 = 1 Then 415

350 Locate 3: Input " 2. FGA "; T%(19): If C1 = 1 Then 415

355 Locate 4: Input " 3. FTM "; T%(22): If C1 = 1 Then 415

360 Locate 5: Input " 4. FTA "; T%(21): If C1 = 1 Then 415

365 Locate 6: Input " 5. OFF REB "; T%(23): If C1 = 1 Then 415

370 Locate 7: Input " 6. DEF REB "; T%(24): If C1 = 1 Then 415
If T%(23) = 0 And RB = 0 Then T%(23) = .35 * T%(24): T%(24) = T%(24) - T%(23): Locate 6: Print " 5. OFF REB "; T%(23): Locate 7: Print " 6. DEF REB "; T%(24)

375 Locate 8: Input " 7. ASSISTS "; T%(25): If C1 = 1 Then 415

380 Locate 9: Input " 8. FOULS "; T%(26): If C1 = 1 Then 415

385 Locate 10: Input " 9. DISQ "; T%(27): If C1 = 1 Then 415

390 Locate 11: Input "10. STEALS "; T%(28): If C1 = 1 Then 415

395 Locate 12: Input "11. TURNOVERS "; T%(29): If C1 = 1 Then 415

400 Locate 13: Input "12. BLK SHOTS "; T%(30): If C1 = 1 Then 415

405 Locate 14: Input "13. 3-FGM "; T%(32): If C1 = 1 Then 415

410 Locate 15: Input "14. 3-FGA "; T%(31): If C1 = 1 Then 415

415 C1 = 1: Locate 17: Input "# TO CHANGE (0 TO CONTINUE) "; B9: On B9 GOTO 345, 350, 355, 360, 365, 370, 375, 380, 385, 390, 395, 400, 405, 410
C1 = 0: If X% > 0 Then 920
If T%(28) = 0 And ST = 0 Then T%(28) = T%(5) * .1
If T%(14) = 0 And ST = 0 Then T%(14) = T%(19) * .1
If T%(16) = 0 And BK = 0 Then T%(16) = T%(19) * .06
If T%(30) = 0 And BK = 0 Then T%(30) = T%(5) * .06
For I = 0 To 14: Cls
    Color 14: Print Tab(25); "PLAYER INFORMATION FOR PLAYER "; I + 1: Color 15

    424 Locate 2: Input " 1. NAME (END TO QUIT) "; B$(I, 0): If C1 = 1 Then 520
    If B$(I, 0) = "END" Then For J = I To 14: B$(J, 0) = "XXX": B$(J, 1) = "X": For I1 = 0 To 19: B%(J, I1) = -1: Next: Next: GoTo 600

    426 Locate 3: Input " 2. POSITION "; B$(I, 1): If C1 = 1 Then 520

    430 Locate 4: Input " 3. GAMES "; B%(I, 0): If C1 = 1 Then 520

    435 Locate 5: Input " 4. MINUTES "; B%(I, 1): If C1 = 1 Then 520

    440 Locate 6: Input " 5. FGM "; B%(I, 3): If C1 = 1 Then 520

    445 Locate 7: Input " 6. FGA "; B%(I, 2): If C1 = 1 Then 520

    450 Locate 8: Input " 7. FTM "; B%(I, 5): If C1 = 1 Then 520

    455 Locate 9: Input " 8. FTA "; B%(I, 4): If C1 = 1 Then 520

    460 Locate 10: Input " 9. OFF REB "; B%(I, 6): If C1 = 1 Then 520

    465 Locate 11: Input "10. DEF REB "; B%(I, 7): If C1 = 1 Then 520
    If B%(I, 6) = 0 And RB = 0 Then B%(I, 6) = .32 * B%(I, 7): B%(I, 7) = B%(I, 7) - B%(I, 6): Locate 10: Print " 9. OFF REB "; B%(I, 6): Locate 11: Print "10. DEF REB "; B%(I, 7)

    470 Locate 12: Input "11. ASSISTS "; B%(I, 8): If C1 = 1 Then 520

    475 Locate 13: Input "12. FOULS "; B%(I, 9): If C1 = 1 Then 520

    480 Locate 14: Input "13. DISQ "; B%(I, 10): If C1 = 1 Then 520
    If ST = 1 Then 485
    If Left$(B$(I, 1), 1) = "C" Then B%(I, 11) = B%(I, 1) / 48 * 1.2: GoSub 4010: GoTo 486
    If Left$(B$(I, 1), 1) = "F" Then B%(I, 11) = B%(I, 1) / 48 * 1.55: GoSub 4010: GoTo 486
    If Left$(B$(I, 1), 1) = "G" Then B%(I, 11) = B%(I, 1) / 48 * 2.3: GoSub 4010: GoTo 486

    485 Locate 15: Input "14. STEALS   "; B%(I, 11): If C1 = 1 Then 520

    486 If BK = 1 Then 490
    If Left$(B$(I, 1), 1) = "C" Then B%(I, 12) = B%(I, 1) / 48 * 2.5: GoSub 4015: GoTo 495
    If Left$(B$(I, 1), 1) = "F" Then B%(I, 12) = B%(I, 1) / 48 * 1.2: GoSub 4015: GoTo 495
    If Left$(B$(I, 1), 1) = "G" Then B%(I, 12) = B%(I, 1) / 48 * .4: GoSub 4015: GoTo 495

    490 Locate 16: Input "15. BLOCKS "; B%(I, 12): If C1 = 1 Then 520

    495 Locate 17: Input "16. 3-FGM "; B%(I, 14): If C1 = 1 Then 520

    500 Locate 18: Input "17. 3-FGA "; B%(I, 13): If C1 = 1 Then 520

    505 Locate 19: Input "18. HI-GAME "; B%(I, 15): If C1 = 1 Then 520
    'LOCATE 20:INPUT "19. PENETRATION (1-9) ";B%(I,16):IF C1=1 THEN 520
    'LOCATE 21:INPUT "20. DEF ADJ (-5 TO 5) ";B%(I,17):IF C1=1 THEN 520

    516 Locate 22: Input "21. ACTIVE FLAG (0-YES 1-NO) "; B%(I, 18): If C1 = 1 Then 520

    520 C1 = 1: Locate 23: Print "# TO CHANGE (0 TO CONTINUE)";: Input B9
    On B9 GOTO 424, 426, 430, 435, 440, 445, 450, 455, 460, 465, 470, 475, 480, 485, 490, 495, 500, 505, 520, 520, 516
    C1 = 0: If X% > 0 Then 920
Next I

600 Cls: Color 14: Print "CHANGE LEAGUE INFORMATION": X% = 2: Color 15: Print
Print "1. NUMBER OF GAMES "; L%(0)
Print "2. AVG FGA/G "; L%(1)
Print "3. AVG RB/G "; L%(2)
Print "4. LEAGUE FG% "; L%(3)
Print "5. AVG PF/G "; L%(4)
Print "6. LEAGUE 3FG% "; L%(5)
Print "7. LEAGUE POWER ADJ "; L%(6)
Print "8. DISK ID "; YN$
Print "9. TEAM NUMBER "; T
GoTo 190

700 Cls: Color 14: Print "CHANGE TEAM INFORMATION": Color 15
Print "1. TEAM NAME "; T$(0)
Print "2. TEAM NICKNAME "; T$(1)
Print "3. TEAM GAMES PLAYED "; T%(0)
Print "4. HOME WINS "; T%(1)
Print "5. HOME LOSSES "; T%(2)
Print "6. ROAD WINS "; T%(3)
Print "7. ROAD LOSSES "; T%(4)
Print "8. TEXT COLOR (0-15) "; T%(33)
Print "9. BCKGRND COLOR (0-7) "; T%(34)
Print "10. TEAM ABBREV (IE. BOS) "; T$(2)
Print "11. COACH'S NAME "; T$(3)
Print "12. ARENA NAME "; T$(4)
Locate 15: Color T%(33), T%(34): Print "SAMPLE TEXT": Color 15, 0
GoTo 255

760 Cls: Color 14: Print "CHANGE TEAM OFFENSIVE INFORMATION": X% = 2: Color 15
Print " 1. FGM "; T%(6)
Print " 2. FGA "; T%(5)
Print " 3. FTM "; T%(8)
Print " 4. FTA "; T%(7)
Print " 5. OFF REB "; T%(9)
Print " 6. DEF REB "; T%(10)
Print " 7. ASSISTS "; T%(11)
Print " 8. FOULS "; T%(12)
Print " 9. DISQ "; T%(13)
Print "10. STEALS "; T%(14)
Print "11. TURNOVERS "; T%(15)
Print "12. BLK SHOTS "; T%(16)
Print "13. 3-FGM "; T%(18)
Print "14. 3-FGA "; T%(17)
GoTo 335

840 Cls: Color 14: Print "CHANGE TEAM DEFENSIVE INFORMATION": X% = 2: Color 15
Print " 1. FGM "; T%(20)
Print " 2. FGA "; T%(19)
Print " 3. FTM "; T%(22)
Print " 4. FTA "; T%(21)
Print " 5. OFF REB "; T%(23)
Print " 6. DEF REB "; T%(24)
Print " 7. ASSISTS "; T%(25)
Print " 8. FOULS "; T%(26)
Print " 9. DISQ "; T%(27)
Print "10. STEALS "; T%(28)
Print "11. TURNOVERS "; T%(29)
Print "12. BLK SHOTS "; T%(30)
Print "13. 3-FGM "; T%(32)
Print "14. 3-FGA "; T%(31)
GoTo 415

920 Cls: Color 14: Print "CHANGE PLAYER STATS": X% = 2: Color 15
For I = 0 To 14: Print Using "##"; I + 1;: Print "  "; B$(I, 0)
Next: Print: Print "# TO CHANGE (1-15) (0 TO CONTINUE) ";: Input I
Locate 1, 1: If I = 0 Then X% = 0: GoTo 1380
I = I - 1: Locate 1, 1
Cls: Color 14: Print Tab(31); "CHANGE PLAYER INFORMATION": Color 15
Print " 1. NAME (END TO QUIT) "; B$(I, 0)
Print " 2. POSITION "; B$(I, 1)
Print " 3. GAMES "; B%(I, 0)
Print " 4. MINUTES "; B%(I, 1)
Print " 5. FGM "; B%(I, 3)
Print " 6. FGA "; B%(I, 2)
Print " 7. FTM "; B%(I, 5)
Print " 8. FTA "; B%(I, 4)
Print " 9. OFF REB "; B%(I, 6)
Print "10. DEF REB "; B%(I, 7)
Print "11. ASSISTS "; B%(I, 8)
Print "12. FOULS "; B%(I, 9)
Print "13. DISQ "; B%(I, 10)
Print "14. STEALS "; B%(I, 11)
Print "15. BLOCKS "; B%(I, 12)
Print "16. 3-FGM "; B%(I, 14)
Print "17. 3-FGA "; B%(I, 13)
Print "18. HI-GAME "; B%(I, 15)
Print: 'PRINT "19. PENETRATION (1-9) ";B%(I,16)
Print: 'PRINT "20. DEF ADJ (-5 TO 5) ";B%(I,17)
Print "21. ACTIVE FLAG (0-YES 1-NO) "; B%(I, 18)
GoTo 520

1100 NT = 0
Open D$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031
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
    teamItems$(NT) = Q$(0): V%(NT) = I1

1103 Next
Close 1
TS% = Int((NT - 1) / 80)

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
20 MAMenu teamItems$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
If EXITCODE% = -79 Then Start% = NT: EXITCODE% = 3: SELECTION% = NT: GoTo 20
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NT Then Start% = NT
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 20
T = V%(SELECTION%)
GoTo 1480

1380 GoSub 2725
Open D$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031
Y = LOF(1)
If Q = 0 Then T = Y / 1031 + 1
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
LSet Q$(0) = T$(0): LSet Q$(1) = T$(1): For I = 0 To 6: LSet Q$(I + 2) = MKI$(L%(I)): Next
LSet Q$(9) = MKI$(999)
For I = 0 To 34: LSet Q$(I + 10) = MKI$(T%(I)): Next
For I = 0 To 14
    LSet Q$(22 * I + 45) = B$(I, 0)
    LSet Q$(22 * I + 46) = B$(I, 1)
    For I1 = 0 To 19
        LSet Q$(22 * I + 47 + I1) = MKI$(B%(I, I1))
Next: Next
LSet Q$(375) = T$(2): LSet Q$(376) = T$(3): LSet Q$(377) = T$(4)
Put #1, T: Close 1
GoTo 100

1435 '

1440 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
Locate 7 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print "ABA SEASONS ARE FOLLOWED BY AN 'A' (IE. 75A)."
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 1102: GoTo 182
YN$ = NY$
ERASE teamItems$
GoSub 1100

182 If BO% = 1 Then BO% = 0: GoTo 1440

183 Locate 2, 33: Color T%(33), T%(34): Print T$(0): Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1440
GoTo 1530

1480 Open D$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031
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
Get #1, T
T$(0) = Q$(0): T$(1) = Q$(1): For I = 0 To 6: L%(I) = CVI(Q$(I + 2)): Next
For I = 0 To 34: T%(I) = CVI(Q$(I + 10)): Next
For I = 0 To 14
    B$(I, 0) = Q$(22 * I + 45)
    B$(I, 1) = Q$(22 * I + 46)
    For I1 = 0 To 19
        B%(I, I1) = CVI(Q$(22 * I + 47 + I1))
Next: Next
T$(2) = Q$(375): T$(3) = Q$(376): T$(4) = Q$(377)
Close 1

1486 If Right$(T$(0), 1) = " " Then T$(0) = Left$(T$(0), Len(T$(0)) - 1): GoTo 1486

1487 If Right$(T$(1), 1) = " " Then T$(1) = Left$(T$(1), Len(T$(1)) - 1): GoTo 1487

1488 If Right$(T$(2), 1) = " " Then T$(2) = Left$(T$(2), Len(T$(2)) - 1): GoTo 1488

1489 If Right$(T$(3), 1) = " " Then T$(3) = Left$(T$(3), Len(T$(3)) - 1): GoTo 1489

1490 If Right$(T$(4), 1) = " " Then T$(4) = Left$(T$(4), Len(T$(4)) - 1): GoTo 1490
Return

1530 Color , 0
If Q = 2 Then 1740
If Q = 3 Then 2000
GoTo 600

1740 LPrint Chr$(15) ': WIDTH "LPT1:", 137
LPrint: LPrint "DISK ID:"; YN$
LPrint "TEAM NAME: "; T$(0); " "; T$(1)
LPrint Using "RECORD ##"; T%(1) + T%(3);: LPrint "-";: LPrint Using "##    HOME ##"; T%(2) + T%(4); T%(1);
LPrint "-";: LPrint Using "##    ROAD ##"; T%(2); T%(3);: LPrint "-";: LPrint Using "##"; T%(4): LPrint
LPrint "                                                      OFF DEF  TOT"
LPrint "PLAYER        PS  G  MIN  FGA  FGM  PCT  FTA FTM  PCT REB REB  REB  AST  PF STL BLK  PTS  AVG HI"
For I = 0 To 14
    If B$(I, 0) = "" Or B$(I, 0) = "XXX" Then 1860
    LPrint Left$(B$(I, 0), 14); Tab(15); B$(I, 1); Tab(18);
    LPrint Using "## #### #### #### "; B%(I, 0); B%(I, 1); B%(I, 2); B%(I, 3);
    If B%(I, 2) = B%(I, 3) And B%(I, 3) <> 0 Then LPrint "1.000";
    If B%(I, 2) <> B%(I, 3) Then LPrint Using ".###"; B%(I, 3) / B%(I, 2);
    LPrint Tab(42);
    LPrint Using "### ### "; B%(I, 4); B%(I, 5);
    If B%(I, 4) = B%(I, 5) And B%(I, 5) <> 0 Then LPrint "1.000 ";
    If B%(I, 4) <> B%(I, 5) Then LPrint Using ".### "; B%(I, 5) / B%(I, 4);
    LPrint Tab(55);
    LPrint Using "### ### #### #### ### ### ### #### "; B%(I, 6); B%(I, 7); B%(I, 6) + B%(I, 7); B%(I, 8); B%(I, 9); B%(I, 11); B%(I, 12); B%(I, 3) * 2 + B%(I, 5) + B%(I, 14);
    LPrint Using "##.# ##"; (B%(I, 3) * 2 + B%(I, 5) + B%(I, 14)) / B%(I, 0); B%(I, 15)

1860 Next
LPrint Chr$(18): LPrint Chr$(12)
GoTo 100
4000 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then I = 1: GoTo 4005
If UCase$(I$) = "N" Then I = 0: GoTo 4005
If I$ = Chr$(27) Then 4005
GoTo 4000

4005 Return

4010 Locate 15: Print "14. STEALS "; B%(I, 11): Return

4015 Locate 16: Print "15. BLOCKS "; B%(I, 12): Return

2725 Cls
Return

2000 Color 0, 0: Cls
Color T%(33), T%(34): Print T$(0); " "; T$(1);
Color 14, 0: Print Tab(40); "WON - ";: Color 15
Print Using "##"; T%(1) + T%(3);: Color 14
Print Tab(50); "LOST - ";: Color 15: Print Using "##"; T%(2) + T%(4)

61 Locate 3: Color 15: Print "NAME          POS   G  MIN  FGA  FGM  PCT  FTA  FTM  PCT OREB DREB ASST  FLS"

62 Locate 4: For I = 0 To 14

    63 If Left$(B$(I, 0), 3) = "XXX" Then 1001

    65 Color 3: Print Left$(B$(I, 0), 14);
    Color 7: Print Tab(15); B$(I, 1);

    66 Print Tab(20); Using "##"; B%(I, 0);

    67 Print Tab(23); Using "####"; B%(I, 1);

    68 Print Tab(28); Using "####"; B%(I, 2);

    69 Print Tab(33); Using "####"; B%(I, 3);

    70 If B%(I, 3) = 0 Then Print Tab(38); ".000";
    If B%(I, 3) > 0 Then Print Tab(38); Using ".###"; B%(I, 3) / B%(I, 2);

    71 Print Tab(43); Using "####"; B%(I, 4);

    72 Print Tab(48); Using "####"; B%(I, 5);

    73 If B%(I, 5) = 0 Then Print Tab(53); ".000";
    If B%(I, 5) > 0 Then Print Tab(53); Using ".###"; B%(I, 5) / B%(I, 4);

    74 Print Tab(58); Using "####"; B%(I, 6);

    75 Print Tab(63); Using "####"; B%(I, 7);

    76 Print Tab(68); Using "####"; B%(I, 8);

    77 Print Tab(73); Using "####"; B%(I, 9)

1001 Next
Color 14: Print: Print "TOTALS-OFF";

102 Print Tab(20); Using "##"; L%(0);

103 Print Tab(28); Using "####"; T%(5);

104 Print Tab(33); Using "####"; T%(6);

105 Print Tab(38); Using ".###"; T%(6) / T%(5);

106 Print Tab(43); Using "####"; T%(7);

107 Print Tab(48); Using "####"; T%(8);

108 Print Tab(53); Using ".###"; T%(8) / T%(7);

109 Print Tab(58); Using "####"; T%(9);

110 Print Tab(63); Using "####"; T%(10);

111 Print Tab(68); Using "####"; T%(11);

112 Print Tab(73); Using "####"; T%(12)

121 Color 11: Print "TOTALS-DEF";

123 Print Tab(28); Using "####"; T%(19);

124 Print Tab(33); Using "####"; T%(20);

125 Print Tab(38); Using ".###"; T%(20) / T%(19);

126 Print Tab(43); Using "####"; T%(21);

127 Print Tab(48); Using "####"; T%(22);

128 Print Tab(53); Using ".###"; T%(22) / T%(21);

129 Print Tab(58); Using "####"; T%(23);
Print Tab(63); Using "####"; T%(24);

131 Print Tab(68); Using "####"; T%(25);

132 Print Tab(73); Using "####"; T%(26)
Color 15: Locate 23: Print "HIT ANY KEY TO CONTINUE"
141 I$ = GetKeyPress$
Cls: Color T%(33), T%(34): Print T$(0); " "; T$(1): Color , 0

151 Locate 3: Color 15: Print "NAME           STL  BLK 3FGA 3FGM  PCT   HI-GAME"

152 Locate 4: For I = 0 To 14

    153 If Left$(B$(I, 0), 3) = "XXX" Then 200

    154 Color 3: Print Left$(B$(I, 0), 14);

    155 Color 7: Print Tab(15); Using "####"; B%(I, 11);

    156 Print Tab(20); Using "####"; B%(I, 12);

    157 Print Tab(25); Using "####"; B%(I, 13);

    158 Print Tab(30); Using "####"; B%(I, 14);

    159 If B%(I, 14) = 0 Then Print Tab(35); ".000";
    If B%(I, 14) > 0 Then Print Tab(35); Using ".###"; B%(I, 14) / B%(I, 13);
    Print Tab(43); Using "####"; B%(I, 15)
    '161 PRINT TAB(53); USING "####"; B%(I, 16);
    '162 PRINT TAB(65); USING "####"; B%(I, 17)

200 Next

201 Color 14: Print: Print "TOTALS-OFF";

202 Print Tab(15); Using "####"; T%(14);

203 Print Tab(20); Using "####"; T%(16);

204 Print Tab(25); Using "####"; T%(17);
Print Tab(30); Using "####"; T%(18);

208 If T%(18) = 0 Then Print Tab(35); ".000"
If T%(18) > 0 Then Print Tab(35); Using ".###"; T%(18) / T%(17)

221 Color 11: Print "TOTALS-DEF";

223 Print Tab(15); Using "####"; T%(28);

224 Print Tab(20); Using "####"; T%(30);
Print Tab(25); Using "####"; T%(31);

226 Print Tab(30); Using "####"; T%(32);

228 If T%(32) = 0 Then Print Tab(35); ".000"
If T%(32) > 0 Then Print Tab(35); Using ".###"; T%(32) / T%(31)
Color 15: Print: Print "VIEW AGAIN (Y/N)"
251 I$ = GetKeyPress$

252 If UCase$(I$) = "Y" Then 2000
GoTo 100


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 27: Color 12
    Print "BASKETBALL: THE PRO GAME V4.0"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; Tab(70); Left$(Time$, 5);
    Locate 3, 1: Color 2, 0
    Color 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
    Print "     FOR TECHNICAL SUPPORT, CALL (615) 366-8088 OR SportGames@AOL.COM"
    Color 10
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
