'$INCLUDE: 'KeyInput.BI'

_Title "Basketball: The Pro Game - Stats Input"
''$ExeIcon:'./lhg_bbpro.ico'
'_Icon
_AllowFullScreen

5 PX = 0

Open "UDEV.BAS" For Input As #1
Input #1, D$(0): Input #1, D$(1)
Close #1

15 Dim W0(14), W1(14), A$(14), Z2(14, 14), Z(15), Z1(15)
'Dim A(14, 14)
Color 7, 0: KEY Off: On Error GoTo 0: Cls: GoSub 2200

55 Cls: GoSub 2200

65 Color 15: Print "C";: Color 3: Print "HANGE/INPUT COMPILED TEAM STATS"
Print
Color 15: Print "R";: Color 3: Print "ETURN TO MAIN MENU"

75 I$ = GetKeyPress$

85 If UCase$(I$) <> "R" Then Q = 1
If UCase$(I$) = "R" Then Q = 2

95 If Q < 1 Or Q > 2 Then 75

105 If Q = 1 Then 2015

115 If Q = 2 Then Cls: GoSub 2200

145 Run "UTIL.EXE": End

155 Print

165 Cls: X$ = "OFFENSIVE TEAM STATS": GoSub 2205

175 Locate 3, 1: Input "1. STAT FILE NAME "; Z$: If C1 = 1 Then 210

177 Locate 4, 1: Input "2. TM MINUTES "; Z(0): Z(0) = Z(0) * 60: If C1 = 1 Then 210

179 Locate 5, 1: Input "3. TM FGA "; Z(1): If C1 = 1 Then 210

181 Locate 6, 1: Input "4. TM FGM "; Z(2): If C1 = 1 Then 210

183 Locate 7, 1: Input "5. TM 3FGA "; Z(14): If C1 = 1 Then 210

185 Locate 8, 1: Input "6. TM 3FGM "; Z(13): If C1 = 1 Then 210

187 Locate 9, 1: Input "7. TM FTA "; Z(3): If C1 = 1 Then 210

189 Locate 10, 1: Input "8. TM FTM "; Z(4): If C1 = 1 Then 210

191 Locate 11, 1: Input "9. TM OFF RB  "; Z(12): If C1 = 1 Then 210

193 Locate 12, 1: Input "10. TM TOT RB "; Z(5): If C1 = 1 Then 210

195 Locate 13, 1: Input "11. TM FOULS "; Z(6): If C1 = 1 Then 210

197 Locate 14, 1: Input "12. TM STEALS "; Z(7): If C1 = 1 Then 210

199 Locate 15, 1: Input "13. TM ASSISTS "; Z(8): If C1 = 1 Then 210

201 Locate 16, 1: Input "14. TM BLOCKS "; Z(9): If C1 = 1 Then 210

203 Locate 17, 1: Input "15. TM TURNOVERS "; Z(11): If C1 = 1 Then 210

205 Locate 18, 1: Input "16. TM POINTS  "; Z(10): If C1 = 1 Then 210

210 C1 = 1

211 Locate 20, 1: Input "# TO CHANGE (0 TO CONTINUE) "; B9

212 If B9 < 0 Or B9 > 16 Then 211

213 On B9 GOTO 175, 177, 179, 181, 183, 185, 187, 189, 191, 193, 195, 197, 199, 201, 203, 205

215 C1 = 0

217 If X% > 0 Then 1580

365 Cls: X$ = "DEFENSIVE TEAM STATS": GoSub 2205

379 Locate 3, 1: Input "1. OPP FGA "; Z1(1): If C1 = 1 Then 410

381 Locate 4, 1: Input "2. OPP FGM "; Z1(2): If C1 = 1 Then 410

383 Locate 5, 1: Input "3. OPP 3FGA "; Z1(14): If C1 = 1 Then 410

385 Locate 6, 1: Input "4. OPP 3FGM "; Z1(13): If C1 = 1 Then 410

387 Locate 7, 1: Input "5. OPP FTA "; Z1(3): If C1 = 1 Then 410

389 Locate 8, 1: Input "6. OPP FTM "; Z1(4): If C1 = 1 Then 410

391 Locate 9, 1: Input "7. OPP OFF RB  "; Z1(12): If C1 = 1 Then 410

393 Locate 10, 1: Input "8. OPP TOT RB "; Z1(5): If C1 = 1 Then 410

395 Locate 11, 1: Input "9. OPP FOULS "; Z1(6): If C1 = 1 Then 410

397 Locate 12, 1: Input "10. OPP STEALS "; Z1(7): If C1 = 1 Then 410

399 Locate 13, 1: Input "11. OPP ASSISTS "; Z1(8): If C1 = 1 Then 410

401 Locate 14, 1: Input "12. OPP BLOCKS "; Z1(9): If C1 = 1 Then 410

403 Locate 15, 1: Input "13. OPP TURNOVERS "; Z1(11): If C1 = 1 Then 410

405 Locate 16, 1: Input "14. OPP POINTS  "; Z1(10): If C1 = 1 Then 410

410 C1 = 1

411 Locate 20, 1: Input "# TO CHANGE (0 TO CONTINUE) "; B9

412 If B9 < 0 Or B9 > 14 Then 411

413 On B9 GOTO 379, 381, 383, 385, 387, 389, 391, 393, 395, 397, 399, 401, 403, 405

645 C1 = 0

655 If X% > 0 Then 1605

665 For I = 0 To 14: Cls: X$ = "PLAYER -" + Str$(I + 1): GoSub 2205

    675 Locate 3, 1: Input "1. NAME "; A$(I): If C1 = 1 Then 1225

    705 Locate 4, 1: Input "2. GAMES "; Z2(I, 0): If C1 = 1 Then 1225

    745 Locate 5, 1: Input "3. MINUTES "; Z2(I, 10): Z2(I, 10) = Z2(I, 10) * 60: If C1 = 1 Then 1225

    785 Locate 6, 1: Input "4. FGA "; Z2(I, 1): If C1 = 1 Then 1225

    825 Locate 7, 1: Input "5. FGM "; Z2(I, 2): If C1 = 1 Then 1225

    865 Locate 8, 1: Input "6. FTA "; Z2(I, 3): If C1 = 1 Then 1225

    905 Locate 9, 1: Input "7. FTM "; Z2(I, 4): If C1 = 1 Then 1225

    906 Locate 10, 1: Input "8. OFF REB "; Z2(I, 12): If C1 = 1 Then 1225

    945 Locate 11, 1: Input "9. TOT REB  "; Z2(I, 5): If C1 = 1 Then 1225

    985 Locate 12, 1: Input "10. FOULS "; Z2(I, 6): If C1 = 1 Then 1225

    1025 Locate 13, 1: Input "11. STEALS "; Z2(I, 7): If C1 = 1 Then 1225

    1065 Locate 14, 1: Input "12. ASSISTS "; Z2(I, 8): If C1 = 1 Then 1225

    1105 Locate 15, 1: Input "13. BLOCKS "; Z2(I, 9): If C1 = 1 Then 1225

    1106 Locate 16, 1: Input "14. TURNOVERS "; Z2(I, 11): If C1 = 1 Then 1225

    1145 Locate 17, 1: Input "15. 3FGA "; W1(I): If C1 = 1 Then 1225

    1185 Locate 18, 1: Input "16. 3FGM "; W0(I): If C1 = 1 Then 1225

    1186 Locate 19, 1: Input "17. HI-GAME (PTS) "; Z2(I, 13): If C1 = 1 Then 1225

    1225 C1 = 1

    1235 Locate 21, 1

    1245 Input "# TO CHANGE (0 TO CONTINUE) "; B9

    1255 If B9 < 0 Or B9 > 17 Then 1235

    1265 On B9 GOTO 675, 705, 745, 785, 825, 865, 905, 906, 945, 985, 1025, 1065, 1105, 1106, 1145, 1185, 1186

    1275 C1 = 0

    1285 If X% > 0 Then 1605

1295 Next I

1305 Cls: X$ = "OFFENSIVE TEAM STATS": GoSub 2205

1335 X% = 2

1345 Locate 3, 1
Print "1. STAT FILE NAME "; Z$
Print "2. TM MINUTES ";: Print Using "####"; Int(Z(0) / 60)
Print "3. TM FGA "; Z(1)
Print "4. TM FGM "; Z(2)
Print "5. TM 3FGA "; Z(14)
Print "6. TM 3FGM "; Z(13)
Print "7. TM FTA "; Z(3)
Print "8. TM FTM "; Z(4)
Print "9. TM OFF RB  "; Z(12)
Print "10. TM TOT RB "; Z(5)
Print "11. TM FOULS "; Z(6)
Print "12. TM STEALS "; Z(7)
Print "13. TM ASSISTS "; Z(8)
Print "14. TM BLOCKS "; Z(9)
Print "15. TM TURNOVERS "; Z(11)
Print "16. TM POINTS  "; Z(10)

1575 GoTo 210

1580 Cls: X$ = "DEFENSIVE TEAM STATS": GoSub 2205
X% = 2
Locate 3, 1
Print "1. OPP FGA "; Z1(1)
Print "2. OPP FGM "; Z1(2)
Print "3. OPP 3FGA "; Z1(14)
Print "4. OPP 3FGM "; Z1(13)
Print "5. OPP FTA "; Z1(3)
Print "6. OPP FTM "; Z1(4)
Print "7. OPP OFF RB  "; Z1(12)
Print "8. OPP TOT RB "; Z1(5)
Print "9. OPP FOULS "; Z1(6)
Print "10. OPP STEALS "; Z1(7)
Print "11. OPP ASSISTS "; Z1(8)
Print "12. OPP BLOCKS "; Z1(9)
Print "13. OPP TURNOVERS "; Z1(11)
Print "14. OPP POINTS  "; Z1(10)
GoTo 410

1605 Cls: GoSub 2200: Locate 3, 1: Color 14: Print "CHANGE PLAYERS "

1615 Print

1625 X% = 2

1635 For I = 0 To 14
    Color 7
    If I / 2 = Int(I / 2) Then Color 15

    1645 Print (I + 1);

1655 Print " "; A$(I): Next I

1665 Print

1675 Print "# TO CHANGE (0 TO CONTINUE) ";

1685 Input I: I = I - 1

1695 If I > 14 Or I < 0 Then X% = 0: GoTo 1865

1705 Cls
X$ = "PLAYER -" + Str$(I + 1): GoSub 2205
Locate 3, 1
Print "1. NAME "; A$(I)
Print "2. GAMES "; Z2(I, 0)
Print "3. MINUTES "; Int(Z2(I, 10) / 60 + .5)
Print "4. FGA "; Z2(I, 1)
Print "5. FGM "; Z2(I, 2); ""
Print "6. FTA "; Z2(I, 3)
Print "7. FTM "; Z2(I, 4)
Print "8. OFF REB "; Z2(I, 12)
Print "9. TOT REB  "; Z2(I, 5)
Print "10. FOULS "; Z2(I, 6)
Print "11. STEALS "; Z2(I, 7)
Print "12. ASSISTS "; Z2(I, 8)
Print "13. BLOCKS "; Z2(I, 9)
Print "14. TURNOVERS "; Z2(I, 11)
Print "15. 3FGA "; W1(I)
Print "16. 3FGM "; W0(I)
Print "17. HI-GAME (PTS) "; Z2(I, 13)
GoTo 1225

1865 GoTo 1935

1875 Print

1885 Print "# TO CHANGE (1-12) (0 TO CONTINUE) ";

1895 Input I

1905 I = I - 1

1915 If I > 10 Or I < 0 Then X% = 0: GoTo 1935

1925 Cls: GoSub 2200

1935 Open D$(1) + Z$ For Output As #1

1945 Print #1, A$

1955 For I = 0 To 14: Print #1, A$(I)

1965 For J = 0 To 13: Print #1, Z2(I, J): Next J: Print #1, W0(I): Print #1, W1(I): Next I

1975 For L = 0 To 15: Print #1, Z(L): Next L

1985 For L = 0 To 15: Print #1, Z1(L): Next L

1995 Close 1

2005 GoTo 55

2015 Cls: X$ = "OFFENSIVE TEAM STATS": GoSub 2205: Locate 3, 1

2025 Input "1. STAT FILE NAME "; Z$

2035 Open D$(1) + Z$ For Input As #1

2045 Input #1, A$

2055 For I = 0 To 14: Input #1, A$(I)

2065 For J = 0 To 13: Input #1, Z2(I, J): Next J: Input #1, W0(I): Input #1, W1(I): Next I

2075 For L = 0 To 15: Input #1, Z(L): Next L

2085 For L = 0 To 15: Input #1, Z1(L): Next L

2095 Close 1

2105 X% = 2: GoTo 1305

2115 Print "                              ";

2125 Return

' ----- THE BELOW ARE ESSENTIALLY TITLE ROUTINES??? -----
2200 Color , 1: Locate 1, 1: Print Tab(80);: Locate , 28: Color 14
Print "EDIT COMPILED STATISTICS"
Locate 24, 1: Print Tab(80);: Locate , 1
Print "(C) LANCE HAFFNER/SHANNON LYNN 1991"; Tab(70); Left$(Time$, 5);
Locate 4, 1: Color 15, 0
Return

2205 Color , 1: Locate 1, 1: Print Tab(80);: Locate , Int(80 - Len(X$)) / 2: Color 14
Print X$
Locate 24, 1: Print Tab(80);: Locate , 1
Print "(C) LANCE HAFFNER/SHANNON LYNN 1991"; Tab(70); Left$(Time$, 5);
Locate 4, 1: Color 15, 0
Return

'$INCLUDE: 'KeyInput.BM'
