'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Basketball: The Pro Game - Compare Stats"
''$ExeIcon:'./lhg_bbpro.ico'
'_Icon
_AllowFullScreen

DECLARE SUB TITLE ()

Dim Q$(377), T%(34), B%(14, 19), B$(14), OP%(100), OP$(100), HP%(100), HP$(100)
Dim teamItems$(500), A$(15), A(15, 14), Z(15), Z1(15), W0(15), W1(15), AA$(15, 1), PS$(15), V%(500)
Dim D$(1), menuItems$(3), L%(6), T$(0)
Open "DEV.BAS" For Input As #2: For I = 0 To 1: Input #2, D$(I): Next
Close #2

10 Call TITLE
menuItems$(1) = "VIEW/PRINT STATISTICAL COMPARISONS"
menuItems$(2) = "RETURN TO BASKETBALL PRO GAME MENU"
SELECTION% = 1
Count% = 2
Start% = 1
SCANCODE% = 2
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 9
Column% = 24
CALL box0(8, 23, 11, 58, 2,  15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
I1 = SELECTION%
If SCANCODE% = 27 Or I1 = 2 Then 1
If I1 = 1 Then 40

1 Run "HELLO.EXE 4"

40 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
Locate 7 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 1102: GoTo 182
YN$ = NY$
ERASE teamItems$

1441 GoSub 1100

182 If BO% = 1 Then BO% = 0: GoTo 10

183 Locate 2, 33: Color 15, 0: Color T%(33), T%(34)
Print B$(0): Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 40
GoTo 170

1100 NTMS = 0
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
    NTMS = NTMS + 1
    teamItems$(NTMS) = Q$(0): V%(NTMS) = I1

1103 Next
Close 1

1102 SELECTION% = 1
Count% = NTMS
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
If EXITCODE% = -79 Then Start% = NTMS: EXITCODE% = 3: SELECTION% = NTMS: GoTo 20
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NTMS Then Start% = NTMS
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 20
T1 = V%(SELECTION%)
Open D$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031
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
Get #1, T1
B$(0) = Q$(0): B$(1) = Q$(1): For I = 0 To 6: L%(I) = CVI(Q$(I + 2)): Next
For I = 0 To 34: T%(I) = CVI(Q$(I + 10)): Next
For I = 0 To 14
    AA$(I, 0) = Q$(22 * I + 45)

    9004 If Right$(AA$(I, 0), 1) = " " Then AA$(I, 0) = Left$(AA$(I, 0), Len(AA$(I, 0)) - 1): GoTo 9004
    AA$(I, 1) = Q$(22 * I + 46)

    9005 If Right$(AA$(I, 1), 1) = " " Then AA$(I, 1) = Left$(AA$(I, 1), Len(AA$(I, 1)) - 1): GoTo 9005
    For I1 = 0 To 19
        B%(I, I1) = CVI(Q$(22 * I + 47 + I1))
Next: Next
B$(2) = Q$(375): B$(3) = Q$(376): B$(4) = Q$(377)
Close 1

1486 If Right$(B$(0), 1) = " " Then B$(0) = Left$(B$(0), Len(B$(0)) - 1): GoTo 1486

1487 If Right$(B$(1), 1) = " " Then B$(1) = Left$(B$(1), Len(B$(1)) - 1): GoTo 1487

1488 If Right$(B$(2), 1) = " " Then B$(2) = Left$(B$(2), Len(B$(2)) - 1): GoTo 1488

1489 If Right$(B$(3), 1) = " " Then B$(3) = Left$(B$(3), Len(B$(3)) - 1): GoTo 1489

1490 If Right$(B$(4), 1) = " " Then B$(4) = Left$(B$(4), Len(B$(4)) - 1): GoTo 1490
B$ = B$(0)
For XX = 4 To 11
    If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
Next XX
For XX = 12 To 14: If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
Next XX
QQ = 8: For XX = 11 To 4 Step -1: If Mid$(B$, XX, 1) = "_" Then QQ = XX - 4: GoTo 6615
    GoTo 6620

6615 Next XX

6620 Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)
If Not _FileExists(D$(1) + Z$) Then 2060

120 Open D$(1) + Z$ For Input As #1

122 Input #1, B$

123 For I = 0 To 14: Input #1, A$(I)

124 For J = 0 To 13: Input #1, A(I, J): Next: Input #1, W0(I): Input #1, W1(I): Next

125 For L = 0 To 15: Input #1, Z(L): Next

126 For L = 0 To 15: Input #1, Z1(L): Next

160 Close #1
GoSub 9000

9100 HW = 0: HL = 0: AW = 0: AL = 0

9103 Open D$(1) + Z$ + "S" For Input As #1

9105 Input #1, Y

9110 For J = 1 To 100: Input #1, OP%(J): Next

9112 For J = 1 To 100: Input #1, OP$(J): Next

9113 For J = 1 To 100: Input #1, HP%(J): Next

9114 For J = 1 To 100: Input #1, HP$(J): Next

9120 Close #1

9130 For I = 1 To Y

    9131 If OP%(I) < HP%(I) And Left$(HP$(I), 1) = "H" Then HL = HL + 1

    9132 If OP%(I) > HP%(I) And Left$(HP$(I), 1) = "H" Then HW = HW + 1

    9133 If OP%(I) < HP%(I) And Left$(HP$(I), 1) = "V" Then AL = AL + 1

    9134 If OP%(I) > HP%(I) And Left$(HP$(I), 1) = "V" Then AW = AW + 1

9135 Next
Return

170 Color 15, 0: Cls
JJ = 4: GoSub 9150: Color 7
Locate , 30: Print "STATS TO ";: Color 15: Print "S";: Color 7: Print "CREEN"
Locate , 30: Print "STATS TO ";: Color 15: Print "P";: Color 7: Print "RINTER"
8651 I$ = GetKeyPress$
If I$ = Chr$(27) Then 10
If UCase$(I$) <> "P" And UCase$(I$) <> "S" Then 8651
Color 14, 0
If UCase$(I$) = "S" Then 200
If UCase$(I$) = "P" Then 700

200 GoTo 2000

201 Cls: Color 9: Locate 3, 12: Print "STAT COMPARISONS FOR ";: Color 15: Print B$(0)

202 For I = O To 14: Color 12: Locate I + 5, 15: Print Using "##"; I + 1;: Print ". ";: Color 7: Print A$(I): Next

203 Color 12: Locate 5, 35: Print "16. ";: Color 3: Print "View Players In Sequence"

204 Color 12: Locate 6, 35: Print "17. ";: Color 3: Print "View Another Team"

206 Color 12: Locate 7, 35: Print "18. ";: Color 3: Print "Return To Main Menu"

219 Color 15: Locate 23, 15: Input "ENTER # TO VIEW "; B9

220 If B9 < 1 Or B9 > 18 Then GoTo 201

221 If B9 >= 1 And B9 <= 15 Then I = B9 - 1: Cls: GoSub 5000: GoTo 300

236 If B9 = 16 Then BI = 1: Cls: GoTo 260

237 If B9 = 17 Then GoTo 40

239 If B9 = 18 Then GoTo 1

260 GoSub 5000

262 I = I + 1

263 For I = 0 To 14

    264 If A$(I) = "XXX" Or A$(I) = "END" Or A$(I) = "" Then 591

    300 Color 11: Locate 2, 2: Print T$(0)

    301 Color 15, 0: Locate 2, 14: Print String$(34, 177): Color 14: Locate 2, 14: Print A$(I);: Print " - ";: Color 11: Print PS$(I)

    302 Color 12: Locate 5, 2: Print "REGULAR "

    303 Locate 6, 2: Print "LEAGUE  ";: Color 7, 0: Print String$(2, 177)
    Locate 7, 4: Print String$(8, 177)

    305 Color 0, 11: Locate 4, 14: Print "  G ";

    306 Locate , 18: Print " MIN ";

    307 Locate , 23: Print " FGM ";

    308 Locate , 28: Print " FGA ";

    309 Locate , 33: Print " FG-PCT ";

    310 Locate , 41: Print " FTM ";

    311 Locate , 46: Print " FTA ";

    312 Locate , 51: Print " FT-PCT ";

    313 Locate , 59: Print " O.REB ";

    314 Locate , 65: Print "  O.REB/G"
    Color 12, 0: Locate 10, 2: Print "REGULAR "
    Locate 11, 2: Print "LEAGUE  ";: Color 7, 0: Print String$(2, 177)
    Locate 12, 4: Print String$(8, 177)

    315 Color 0, 11: Locate 9, 14: Print " D.REB ";

    316 Locate , 21: Print " D.REB/G ";

    317 Locate , 30: Print " ASST ";

    318 Locate , 36: Print " ASST/G ";

    319 Locate , 44: Print " FOULS ";
    Locate , 51: Print " F/G ";
    Locate , 56: Print " TREB ";
    Locate , 62: Print " STL ";
    Locate , 67: Print " STL/G"
    Color 12, 0: Locate 15, 2: Print "REGULAR "
    Locate 16, 2: Print "LEAGUE  ";: Color 7, 0: Print String$(2, 177)
    Locate 17, 4: Print String$(8, 177)
    Color 0, 11: Locate 14, 14: Print " BLK ";
    Locate , 19: Print " BLK/G ";
    Locate , 26: Print " 3-FGM ";
    Locate , 33: Print " 3-FGA ";
    Locate , 40: Print " 3-FG-PCT ";
    Locate , 50: Print " HI-GM ";
    Locate , 57: Print "  PTS";
    Locate , 62: Print " PTS/G ";
    Locate , 69: Print " MIN/G"

    321 Color 1, 11: Locate 5, 14: Print Using "###"; B%(I, 0);
    Locate , 17: Print Using "#####"; B%(I, 1);
    Locate , 22: Print Using "#####"; B%(I, 3);
    Locate , 27: Print Using "#####"; B%(I, 2);
    If B%(I, 3) = 0 Then Locate , 32: Print "    0.0";: Print "%";
    If B%(I, 3) > 0 Then Locate , 32: Print Using "#####.#"; (B%(I, 3) / B%(I, 2)) * 100;: Print "%";
    Locate , 40: Print Using "#####"; B%(I, 5);
    Locate , 45: Print Using "#####"; B%(I, 4);
    If B%(I, 5) = 0 Then Locate , 50: Print "    0.0";: Print "%";
    If B%(I, 5) > 0 Then Locate , 50: Print Using "#####.#"; (B%(I, 5) / B%(I, 4)) * 100;: Print "%";
    Locate , 58: Print Using "#######"; B%(I, 6);
    If B%(I, 0) = 0 Or B%(I, 6) = 0 Then Locate , 65: Print "      0.0";
    If B%(I, 0) > 0 And B%(I, 6) = 0 Then Locate , 65: Print "      0.0";
    If B%(I, 0) > 0 And B%(I, 6) > 0 Then Locate , 65: Print Using "#######.#"; B%(I, 6) / B%(I, 0);
    Color 7, 0: Print String$(2, 177)
    Color 1, 11: Locate 10, 14: Print Using "######"; B%(I, 7);
    If B%(I, 0) = 0 Or B%(I, 7) = 0 Then Locate , 20: Print "      0.0";
    If B%(I, 0) > 0 And B%(I, 7) = 0 Then Locate , 20: Print "      0.0";
    If B%(I, 0) > 0 And B%(I, 7) > 0 Then Locate , 20: Print Using "#######.#"; B%(I, 7) / B%(I, 0);
    Locate , 29: Print Using "######"; B%(I, 8);
    If B%(I, 0) = 0 Or B%(I, 8) = 0 Then Locate , 35: Print "     0.0";
    If B%(I, 0) > 0 And B%(I, 8) = 0 Then Locate , 35: Print "     0.0";
    If B%(I, 0) > 0 And B%(I, 8) > 0 Then Locate , 35: Print Using "######.#"; B%(I, 8) / B%(I, 0);
    Locate , 43: Print Using "#######"; B%(I, 9);
    If B%(I, 0) = 0 Or B%(I, 9) = 0 Then Locate , 50: Print "  0.0";
    If B%(I, 0) > 0 And B%(I, 9) = 0 Then Locate , 50: Print "  0.0";
    If B%(I, 0) > 0 And B%(I, 9) > 0 Then Locate , 50: Print Using "###.#"; B%(I, 9) / B%(I, 0);
    Locate , 55: Print Using "######"; B%(I, 6) + B%(I, 7);
    Locate , 61: Print Using "#####"; B%(I, 11);

    330 If B%(I, 0) = 0 Or B%(I, 11) = 0 Then Locate , 66: Print "    0.0";

    331 If B%(I, 0) > 0 And B%(I, 11) = 0 Then Locate , 66: Print "    0.0";

    332 If B%(I, 0) > 0 And B%(I, 11) > 0 Then Locate , 66: Print Using "#####.#"; B%(I, 11) / B%(I, 0);
    Color 7, 0: Print String$(2, 177)

    335 Color 1, 11: Locate 15, 14: Print Using "####"; B%(I, 12);

    337 If B%(I, 0) = 0 Or B%(I, 12) = 0 Then Locate , 18: Print "    0.0";

    338 If B%(I, 0) > 0 And B%(I, 12) = 0 Then Locate , 18: Print "    0.0";

    339 If B%(I, 0) > 0 And B%(I, 12) > 0 Then Locate , 18: Print Using "#####.#"; B%(I, 12) / B%(I, 0);

    341 Locate , 25: Print Using "#######"; B%(I, 14);

    344 Locate , 32: Print Using "#######"; B%(I, 13);

    346 If B%(I, 14) = 0 Then Locate , 39: Print "      0.0";: Print "%";

    347 If B%(I, 14) > 0 Then Locate , 39: Print Using "#######.#"; (B%(I, 14) / B%(I, 13)) * 100;: Print "%";

    351 Locate , 49: Print Using "#######"; B%(I, 15);

    353 Locate , 56: Print Using "######"; (B%(I, 3) * 2) + B%(I, 5) + B%(I, 14);

    357 If (B%(I, 3) * 2) + B%(I, 5) + B%(I, 14) = 0 Then Locate , 62: Print "   0.0";

    358 If (B%(I, 3) * 2) + B%(I, 5) + B%(I, 14) > 0 Then Locate , 62: Print Using "####.#"; ((B%(I, 3) * 2) + B%(I, 5) + B%(I, 14)) / B%(I, 0);

    367 If B%(I, 1) = 0 Then Locate , 68: Print "    0.0";

    368 If B%(I, 1) > 0 Then Locate , 68: Print Using "#####.#"; B%(I, 1) / B%(I, 0);
    Color 7, 0: Print String$(2, 177)

    400 Color 14, 11: Locate 6, 14: Print Using "###"; A(I, 0);
    If A(I, 10) = 0 Then Locate , 17: Print "    0";
    If A(I, 10) > 0 Then Locate , 17: Print Using "#####"; (A(I, 10) + 30) / 60;
    Locate , 22: Print Using "#####"; A(I, 2);
    Locate , 27: Print Using "#####"; A(I, 1);
    If A(I, 2) = 0 Then Locate , 32: Print "    0.0";: Print "%";
    If A(I, 2) > 0 Then Locate , 32: Print Using "#####.#"; (A(I, 2) / A(I, 1)) * 100;: Print "%";
    Locate 6, 40: Print Using "#####"; A(I, 4);
    Locate 6, 45: Print Using "#####"; A(I, 3);
    If A(I, 4) = 0 Then Locate , 50: Print "    0.0";: Print "%";
    If A(I, 4) > 0 Then Locate , 50: Print Using "#####.#"; A(I, 4) / A(I, 3) * 100;: Print "%";
    Locate , 58: Print Using "#######"; A(I, 12);
    If A(I, 0) = 0 Or A(I, 12) = 0 Then Locate , 65: Print "      0.0";
    If A(I, 0) > 0 And A(I, 12) = 0 Then Locate , 65: Print "      0.0";
    If A(I, 0) > 0 And A(I, 12) > 0 Then Locate , 65: Print Using "#######.#"; A(I, 12) / A(I, 0);
    Color 7, 0: Print String$(2, 177)
    Locate , 16: Print String$(60, 177)
    Color 14, 11: Locate 11, 14: Print Using "######"; A(I, 5) - A(I, 12);
    If A(I, 0) = 0 Or (A(I, 5) - A(I, 12)) = 0 Then Locate , 20: Print "      0.0";
    If A(I, 0) > 0 And (A(I, 5) - A(I, 12)) = 0 Then Locate , 20: Print "      0.0";
    If A(I, 0) > 0 And (A(I, 5) - A(I, 12)) > 0 Then Locate , 20: Print Using "#######.#"; (A(I, 5) - A(I, 12)) / A(I, 0);
    Locate , 29: Print Using "######"; A(I, 8);
    If A(I, 0) = 0 Or A(I, 8) = 0 Then Locate , 35: Print "     0.0";
    If A(I, 0) > 0 And A(I, 8) = 0 Then Locate , 35: Print "     0.0";
    If A(I, 0) > 0 And A(I, 8) > 0 Then Locate , 35: Print Using "######.#"; A(I, 8) / A(I, 0);
    Locate , 43: Print Using "#######"; A(I, 6);
    If A(I, 0) = 0 Or A(I, 6) = 0 Then Locate , 50: Print "  0.0";
    If A(I, 0) > 0 And A(I, 6) = 0 Then Locate , 50: Print "  0.0";
    If A(I, 0) > 0 And A(I, 6) > 0 Then Locate , 50: Print Using "###.#"; A(I, 6) / A(I, 0);

    406 Locate , 55: Print Using "######"; A(I, 5);

    408 Locate , 61: Print Using "#####"; A(I, 7);

    410 If A(I, 0) = 0 Or A(I, 7) = 0 Then Locate , 66: Print "    0.0";

    411 If A(I, 0) > 0 And A(I, 7) = 0 Then Locate , 66: Print "    0.0";

    412 If A(I, 0) > 0 And A(I, 7) > 0 Then Locate , 66: Print Using "#####.#"; A(I, 7) / A(I, 0);
    Color 7, 0: Print String$(2, 177)
    Locate , 16: Print String$(59, 177)
    Color 14, 11: Locate 16, 14: Print Using "####"; A(I, 9);

    417 If A(I, 0) = 0 Or A(I, 9) = 0 Then Locate , 18: Print "    0.0";

    418 If A(I, 0) > 0 And A(I, 9) = 0 Then Locate , 18: Print "    0.0";

    419 If A(I, 0) > 0 And A(I, 9) > 0 Then Locate , 18: Print Using "#####.#"; A(I, 9) / A(I, 0);

    421 Locate , 25: Print Using "#######"; W0(I);

    424 Locate , 32: Print Using "#######"; W1(I);

    426 If W0(I) = 0 Then Locate , 39: Print "      0.0";: Print "%";

    427 If W0(I) > 0 Then Locate , 39: Print Using "#######.#"; W0(I) / W1(I) * 100;: Print "%";

    431 Locate , 49: Print Using "#######"; A(I, 13);

    433 Locate , 56: Print Using "######"; (A(I, 2) * 2) + A(I, 4) + W0(I);

    437 If (A(I, 2) * 2) + A(I, 4) + W0(I) = 0 Then Locate , 62: Print "   0.0";

    438 If (A(I, 2) * 2) + A(I, 4) + W0(I) > 0 Then Locate , 62: Print Using "####.#"; ((A(I, 2) * 2) + A(I, 4) + W0(I)) / A(I, 0);

    440 If A(I, 10) = 0 Then Locate , 68: Print "    0.0";

    441 If A(I, 10) > 0 Then Locate , 68: Print Using "#####.#"; ((A(I, 10) + 30) / 60) / A(I, 0);
    Color 7, 0: Print String$(2, 177)
    Locate , 16: Print String$(61, 177)

    578 Color 15, 0: Locate 22: Print "HIT ENTER TO CONTINUE"

    579 If BI = 1 Then 580 Else GoTo 581

    580 Locate 22, 23: Print "OR ESC TO RETURN TO MENU"
    581 I$ = GetKeyPress$

    582 If I$ = Chr$(0) Then 591

    583 If I$ = Chr$(27) Then 588

    584 If BI = 0 Then 587

    585 If BI = 1 Then 591

    586 If I < 14 Then 591 Else GoTo 201

    587 Cls: GoTo 201

    588 BI = 0: GoTo 201

    590 If BI = 1 Then 591 Else GoTo 201

591 Next I

592 If BI = 1 Then GoTo 201

1000 Cls: Color 15: Print "MAKE SURE LEAGUE UTILITIES DISK IS IN DRVE"

1001 Locate 23: Print "HIT ANY KEY TO CONTINUE"
1002 I$ = GetKeyPress$

1003 Run "LEAGUTIL.EXE"

2060 Color 15, 0: Cls
JJ = 4: GoSub 9150: Color 15
Locate , 30: Print "NO COMPILED STATS FOR TEAM": Color 7
8652 I$ = GetKeyPress$

2720 BO% = 1: Return

2000 Cls

2001 GoSub 6000

2002 Locate 11, 31: Print "                  "

2009 Color 11: Locate 11, 33: Print B$(0)

2020 Color 15, 12

2021 Color 15, 2: Locate 5, 30: Print "      HOME    W - L ";
Locate 6, 30: Print "  SEASON   ";
Locate 7, 30: Print "  LEAGUE   ";
Locate 15, 30: Print "      ROAD    W - L ";
Locate 16, 30: Print "  SEASON   ";
Locate 17, 30: Print "  LEAGUE   ";
Color 15, 1: Locate 2, 3: Print "   OFFENSIVE STATS    "
Locate 3, 3: Print "       SEASON   LEAGUE"
Locate 4, 3: Print " FGM   ";
Locate 5, 3: Print " FGA   ";
Locate 6, 3: Print " FG PCT";
Locate 7, 3: Print " FTM   ";
Locate 8, 3: Print " FTA   ";
Locate 9, 3: Print " FT PCT";
Locate 10, 3: Print " O.RB  ";
Locate 11, 3: Print " D.RB  ";
Locate 12, 3: Print " T.REB ";
Locate 13, 3: Print " ASS'T ";
Locate 14, 3: Print " FLS   ";
Locate 15, 3: Print "  ST   ";
Locate 16, 3: Print "  TO   ";
Locate 17, 3: Print " BLK   ";
Locate 18, 3: Print " 3-FGM ";
Locate 19, 3: Print " 3-FGA ";
Locate 20, 3: Print " 3-PCT ";
Color 15, 12: Locate 2, 55: Print "   DEFENSIVE STATS   "
Locate 3, 55: Print "       SEASON  LEAGUE"
Locate 4, 55: Print " FGM   ";
Locate 5, 55: Print " FGA   ";
Locate 6, 55: Print " FG PCT";
Locate 7, 55: Print " FTM   ";
Locate 8, 55: Print " FTA   ";
Locate 9, 55: Print " FT PCT";
Locate 10, 55: Print " O.RB  ";
Locate 11, 55: Print " D.RB  ";
Locate 12, 55: Print " T.REB ";
Locate 13, 55: Print " ASS'T ";
Locate 14, 55: Print " FLS   ";
Locate 15, 55: Print "  ST   ";
Locate 16, 55: Print "  TO   ";
Locate 17, 55: Print " BLK   ";
Locate 18, 55: Print " 3-FGM ";
Locate 19, 55: Print " 3-FGA ";
Locate 20, 55: Print " 3-PCT ";
For I = 3 To 21: Locate I, 25: Color 7, 0: Print String$(2, 177): Next
For I = 3 To 21: Locate I, 76: Print String$(2, 177): Next
Locate 21, 5: Print String$(20, 177)
Locate 21, 57: Print String$(20, 177)
For I = 6 To 8: Locate I, 50: Print String$(2, 177): Next
For I = 16 To 18: Locate I, 50: Print String$(2, 177): Next
Locate 8, 32: Print String$(18, 177)
Locate 18, 32: Print String$(18, 177)

2100 Color 3, 0: Locate 6, 41: Print Using "####"; T%(1);
Locate 6, 45: Print Using "####"; T%(2);: Print " ";
Color 14, 0: Locate 7, 41: Print Using "####"; HW;
Locate 7, 45: Print Using "####"; HL;: Print " ";
Color 3, 0: Locate 4, 10: Print Using "######"; T%(6);
Locate 5, 10: Print Using "######"; T%(5);

2110 If T%(6) = 0 Then Locate 6, 10: Print "   0.0";

2111 If T%(6) > 0 Then Locate 6, 10: Print "  ";: Print Using ".###"; (T%(6) / T%(5));
Locate 7, 10: Print Using "######"; T%(8);
Locate 8, 10: Print Using "######"; T%(7);

2120 If T%(7) = 0 Then Locate 9, 10: Print "   0.0";

2121 If T%(7) > 0 Then Locate 9, 10: Print "  ";: Print Using ".###"; (T%(8) / T%(7));
Locate 10, 10: Print Using "######"; T%(9);
Locate 11, 10: Print Using "######"; T%(10);
Locate 12, 10: Print Using "######"; T%(9) + T%(10);
Locate 13, 10: Print Using "######"; T%(11);
Locate 14, 10: Print Using "######"; T%(12);
Locate 15, 10: Print Using "######"; T%(14);
Locate 16, 10: Print Using "######"; T%(15);
Locate 17, 10: Print Using "######"; T%(16);
Locate 18, 10: Print Using "######"; T%(18);
Locate 19, 10: Print Using "######"; T%(17);
Color 3, 0: Locate 4, 62: Print Using "######"; T%(20);
Locate 5, 62: Print Using "######"; T%(19);
If T%(19) = 0 Then Locate 6, 62: Print "   0.0";
If T%(19) > 0 Then Locate 6, 62: Print "  ";: Print Using ".###"; (T%(20) / T%(19));
Locate 7, 62: Print Using "######"; T%(22);
Locate 8, 62: Print Using "######"; T%(21);
If T%(21) = 0 Then Locate 9, 62: Print "   0.0";
If T%(21) > 0 Then Locate 9, 62: Print "  ";: Print Using ".###"; (T%(22) / T%(21));
Locate 10, 62: Print Using "######"; T%(23);
Locate 11, 62: Print Using "######"; T%(24);
Locate 12, 62: Print Using "######"; T%(23) + T%(24);
Locate 13, 62: Print Using "######"; T%(25);
Locate 14, 62: Print Using "######"; T%(26);
Locate 15, 62: Print Using "######"; T%(28);
Locate 16, 62: Print Using "######"; T%(29);
Locate 17, 62: Print Using "######"; T%(30);
Locate 18, 62: Print Using "######"; T%(32);
Locate 19, 62: Print Using "######"; T%(31);
If T%(31) = 0 Then Locate 20, 62: Print "   0.0";
If T%(31) > 0 Then Locate 20, 62: Print "  ";: Print Using ".###"; (T%(32) / T%(31));

2130 If T%(17) = 0 Then Locate 20, 10: Print "   0.0";

2131 If T%(17) > 0 Then Locate 20, 10: Print "  ";: Print Using ".###"; (T%(18) / T%(17));

2200 Locate 16, 41: Color 3, 0: Print Using "####"; T%(3);
Locate 16, 45: Print Using "####"; T%(4);: Print " ";
Color 14, 0: Locate 17, 41: Print Using "####"; AW;
Locate 17, 45: Print Using "####"; AL;: Print " ";
Locate 4, 16: Print Using "#########"; Z(2);
Locate 5, 16: Print Using "#########"; Z(1);

2210 If Z(1) = 0 Then Locate 6, 16: Print "      0.0";

2211 If Z(1) > 0 Then Locate 6, 16: Print "     ";: Print Using ".###"; (Z(2) / Z(1));
Locate 7, 16: Print Using "#########"; Z(4);
Locate 8, 16: Print Using "#########"; Z(3);

2220 If Z(3) = 0 Then Locate 9, 16: Print "      0.0";

2221 If Z(3) > 0 Then Locate 9, 16: Print "     ";: Print Using ".###"; (Z(4) / Z(3));
Locate 10, 16: Print Using "#########"; Z(12);
Locate 11, 16: Print Using "#########"; Z(5) - Z(12) - Z(15);
Locate 12, 16: Print Using "#########"; Z(5) - Z(15);
Locate 13, 16: Print Using "#########"; Z(8);
Locate 14, 16: Print Using "#########"; Z(6);
Locate 15, 16: Print Using "#########"; Z(7);
Locate 16, 16: Print Using "#########"; Z(11);
Locate 17, 16: Print Using "#########"; Z(9);
Locate 18, 16: Print Using "#########"; Z(13);
Locate 19, 16: Print Using "#########"; Z(14);
Locate 4, 68: Print Using "########"; Z1(2);
Locate 5, 68: Print Using "########"; Z1(1);
If Z1(1) = 0 Then Locate 6, 68: Print "     0.0";
If Z1(1) > 0 Then Locate 6, 68: Print "    ";: Print Using ".###"; (Z1(2) / Z1(1));
Locate 7, 68: Print Using "########"; Z1(4);
Locate 8, 68: Print Using "########"; Z1(3);
If Z1(13) = 0 Then Locate 9, 68: Print "     0.0";
If Z1(13) > 0 Then Locate 9, 68: Print "    ";: Print Using ".###"; (Z1(4) / Z1(3));
Locate 10, 68: Print Using "########"; Z1(12);
Locate 11, 68: Print Using "########"; Z1(5) - Z1(12) - Z1(15);
Locate 12, 68: Print Using "########"; Z1(5) - Z1(15);
Locate 13, 68: Print Using "########"; Z1(8);
Locate 14, 68: Print Using "########"; Z1(6);
Locate 15, 68: Print Using "########"; Z1(7);
Locate 16, 68: Print Using "########"; Z1(11);
Locate 17, 68: Print Using "########"; Z1(9);
Locate 18, 68: Print Using "########"; Z1(13);
Locate 19, 68: Print Using "########"; Z1(14);
If Z1(13) = 0 Then Locate 20, 68: Print "     0.0";
If Z1(13) > 0 Then Locate 20, 68: Print "    ";: Print Using ".###"; (Z1(13) / Z1(14));

2230 If Z(13) = 0 Then Locate 20, 16: Print "      0.0";

2231 If Z(13) > 0 Then Locate 20, 16: Print "     ";: Print Using ".###"; (Z(13) / Z(14));

2800 Locate 23, 28: Color 15, 0: Print "HIT ANY KEY TO CONTINUE"
2801 I$ = GetKeyPress$

2900 GoTo 201

5000 For dc = 1 To 19

    5001 Color 15, 0: Locate dc

    5002 Print String$(78, 177)

5003 Next

5004 Return

6000 For dc = 1 To 22

    6001 Color 15, 0: Locate dc

    6002 Print String$(79, 177)

6003 Next

6004 Return

9000 For I = 0 To 14
    If AA$(I, 1) = "F" Then PS$(I) = "FORWARD"
    If AA$(I, 1) = "C" Then PS$(I) = "CENTER"
    If AA$(I, 1) = "G" Then PS$(I) = "GUARD"
    If AA$(I, 1) = "FC" Then PS$(I) = "FORWARD/GUARD"
    If AA$(I, 1) = "FG" Then PS$(I) = "FORWARD/CENTER"
    If AA$(I, 1) = "GF" Then PS$(I) = "GUARD/FORWARD"
    If AA$(I, 1) = "CF" Then PS$(I) = "CENTER/FORWARD"
Next

9015 Return

9200 Color 15, 0: Cls

9207 Return

9150 Locate 8, 8: Color 14, 1
Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
For II = 1 To JJ: Locate 8 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next
Locate 9 + JJ, 8
Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 10
Return

700


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
