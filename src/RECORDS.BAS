Sub RECORDS (menuChoice%)

    Dim RC$(50, 4), TRC$(125, 3), Args$(5)
	Dim allRecords(50, 2), TotalRecords(125, 2)
    Dim HALF$(2), QTR$(4)

	_Title "Basketball: The Pro Game - View Leaders"

    HALF$(1) = " (1ST HALF)": HALF$(2) = " (2ND HALF)"
    QTR$(1) = " (1ST QTR)"
    QTR$(2) = " (2ND QTR)"
    QTR$(3) = " (3RD QTR)"
    QTR$(4) = " (4TH QTR)"

    Open D$(1) + "RECORDS.GAM" For Input As #1

    For I = 0 To 48
        For I1 = 0 To 2
            Input #1, allRecords(I, I1)
        Next
        For I1 = 0 To 4
            Input #1, RC$(I, I1)
        Next
    Next
	
    For I = 0 To 125
        For I1 = 0 To 2
            Input #1, TotalRecords(I, I1)
        Next
        For I1 = 0 To 3
            Input #1, TRC$(I, I1)
        Next
    Next
    Close 1

'Call Comline(N, Args$(), 10) ' Get what was typed on the command line
If menuChoice% = 2 Then 200

Color 15, 1
Cls

Color 12: Print "                            INDIVIDUAL RECORDS"
Print
Color 14: Print "MOST POINTS, GAME": Color 15
Print allRecords(0, 0); "- "; RC$(0, 0); ", "; RC$(0, 1) + " " + RC$(0, 2) + " " + RC$(0, 3) + ", " + RC$(0, 4)
Print

Color 14: Print "MOST POINTS, ONE HALF": Color 15
Print allRecords(1, 0); "- "; RC$(1, 0); ", "; RC$(1, 1) + " " + RC$(1, 2) + " " + RC$(1, 3) + ", " + RC$(1, 4); HALF$(allRecords(1, 2))
Print

Color 14: Print "MOST POINTS, ONE QUARTER": Color 15
Print allRecords(2, 0); "- "; RC$(2, 0); ", "; RC$(2, 1) + " " + RC$(2, 2) + " " + RC$(2, 3) + ", " + RC$(2, 4); QTR$(allRecords(2, 2))
Print

Color 14: Print "MOST POINTS, OVERTIME PERIOD": Color 15
If allRecords(3, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(3, 0); "- "; RC$(3, 0); ", "; RC$(3, 1) + " " + RC$(3, 2) + " " + RC$(3, 3) + ", " + RC$(3, 4)
End If
Print

Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, GAME (MINIMUM 15 FIELD GOALS)": Color 15
If allRecords(4, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
    GoTo 10
End If
If allRecords(4, 0) = 1 Then
    Print "1.000 ";
Else
    Print Using " .### "; allRecords(4, 0);
End If
Print "- "; RC$(4, 0); ", "; RC$(4, 1) + " " + RC$(4, 2) + " " + RC$(4, 3) + ", " + RC$(4, 4);
Print Using " (##/##)"; allRecords(4, 1); allRecords(4, 2)

10 Print

Color 14: Print "MOST FIELD GOALS, NO MISSES, GAME": Color 15
If allRecords(5, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(5, 0); "- "; RC$(5, 0); ", "; RC$(5, 1) + " " + RC$(5, 2) + " " + RC$(5, 3) + ", " + RC$(5, 4)
End If
Print

Color 14: Print "MOST FIELD GOAL ATTEMPTS, NONE MADE, GAME": Color 15
If allRecords(6, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(6, 0); "- "; RC$(6, 0); ", "; RC$(6, 1) + " " + RC$(6, 2) + " " + RC$(6, 3) + ", " + RC$(6, 4)
End If
I$ = GetKeyPress$

Cls
Color 12: Print "                            INDIVIDUAL RECORDS"
Print
Color 14: Print "MOST FIELD GOALS, GAME": Color 15
Print allRecords(7, 0); "- "; RC$(7, 0); ", "; RC$(7, 1) + " " + RC$(7, 2) + " " + RC$(7, 3) + ", " + RC$(7, 4)
Print

Color 14: Print "MOST FIELD GOALS, ONE HALF": Color 15
Print allRecords(8, 0); "- "; RC$(8, 0); ", "; RC$(8, 1) + " " + RC$(8, 2) + " " + RC$(8, 3) + ", " + RC$(8, 4); HALF$(allRecords(8, 2))
Print

Color 14: Print "MOST FIELD GOALS, ONE QUARTER": Color 15
Print allRecords(9, 0); "- "; RC$(9, 0); ", "; RC$(9, 1) + " " + RC$(9, 2) + " " + RC$(9, 3) + ", " + RC$(9, 4); QTR$(allRecords(9, 2))
Print

Color 14: Print "MOST FIELD GOALS, OVERTIME": Color 15
If allRecords(10, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(10, 0); "- "; RC$(10, 0); ", "; RC$(10, 1) + " " + RC$(10, 2) + " " + RC$(10, 3) + ", " + RC$(10, 4)
End If
Print

Color 14: Print "MOST FIELD GOAL ATTEMPTS, GAME": Color 15
Print allRecords(11, 0); "- "; RC$(11, 0); ", "; RC$(11, 1) + " " + RC$(11, 2) + " " + RC$(11, 3) + ", " + RC$(11, 4)
Print

Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE HALF": Color 15
Print allRecords(12, 0); "- "; RC$(12, 0); ", "; RC$(12, 1) + " " + RC$(12, 2) + " " + RC$(12, 3) + ", " + RC$(12, 4); HALF$(allRecords(12, 2))
Print

Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE QUARTER": Color 15
Print allRecords(13, 0); "- "; RC$(13, 0); ", "; RC$(13, 1) + " " + RC$(13, 2) + " " + RC$(13, 3) + ", " + RC$(13, 4); QTR$(allRecords(13, 2))
Print
I$ = GetKeyPress$

Cls
Color 12: Print "                            INDIVIDUAL RECORDS"
Print
Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME": Color 15
If allRecords(15, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(15, 0); "- "; RC$(15, 0); ", "; RC$(15, 1) + " " + RC$(15, 2) + " " + RC$(15, 3) + ", " + RC$(15, 4)
End If
Print

Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME": Color 15
If allRecords(16, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(16, 0); "- "; RC$(16, 0); ", "; RC$(16, 1) + " " + RC$(16, 2) + " " + RC$(16, 3) + ", " + RC$(16, 4)
End If
Print

Color 14: Print "MOST THREE-POINT FIELD GOALS, GAME": Color 15
If allRecords(17, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(17, 0); "- "; RC$(17, 0); ", "; RC$(17, 1) + " " + RC$(17, 2) + " " + RC$(17, 3) + ", " + RC$(17, 4)
End If
Print

Color 14: Print "MOST THREE-POINT FIELD GOALS MADE, ONE HALF": Color 15
If allRecords(18, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(18, 0); "- "; RC$(18, 0); ", "; RC$(18, 1) + " " + RC$(18, 2) + " " + RC$(18, 3) + ", " + RC$(18, 4); HALF$(allRecords(18, 2))
End If
Print

Color 14: Print "MOST THREE-POINT FIELD GOALS MADE, ONE QUARTER": Color 15
If allRecords(19, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(19, 0); "- "; RC$(19, 0); ", "; RC$(19, 1) + " " + RC$(19, 2) + " " + RC$(19, 3) + ", " + RC$(19, 4); QTR$(allRecords(19, 2))
End If
Print

Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME": Color 15
If allRecords(21, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(21, 0); "- "; RC$(21, 0); ", "; RC$(21, 1) + " " + RC$(21, 2) + " " + RC$(21, 3) + ", " + RC$(21, 4)
End If
Print

Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF": Color 15
If allRecords(22, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(22, 0); "- "; RC$(22, 0); ", "; RC$(22, 1) + " " + RC$(22, 2) + " " + RC$(22, 3) + ", " + RC$(22, 4); HALF$(allRecords(22, 2))
End If
Print

I$ = GetKeyPress$

Cls
Color 12: Print "                            INDIVIDUAL RECORDS"
Color 14: Print "MOST FREE THROWS MADE, NO MISSES, GAME": Color 15
If allRecords(23, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(23, 0); "- "; RC$(23, 0); ", "; RC$(23, 1) + " " + RC$(23, 2) + " " + RC$(23, 3) + ", " + RC$(23, 4)
End If
Print

Color 14: Print "MOST FREE THROW ATTEMPTS, NONE MADE, GAME": Color 15
If allRecords(24, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(24, 0); "- "; RC$(24, 0); ", "; RC$(24, 1) + " " + RC$(24, 2) + " " + RC$(24, 3) + ", " + RC$(24, 4)
End If
Print

Color 14: Print "MOST FREE THROWS MADE, GAME": Color 15
If allRecords(25, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(25, 0); "- "; RC$(25, 0); ", "; RC$(25, 1) + " " + RC$(25, 2) + " " + RC$(25, 3) + ", " + RC$(25, 4)
End If
Print

Color 14: Print "MOST FREE THROWS MADE, ONE HALF": Color 15
If allRecords(26, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(26, 0); "- "; RC$(26, 0); ", "; RC$(26, 1) + " " + RC$(26, 2) + " " + RC$(26, 3) + ", " + RC$(26, 4); HALF$(allRecords(26, 2))
End If
Print

Color 14: Print "MOST FREE THROWS MADE, ONE QUARTER": Color 15
If allRecords(27, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(27, 0); "- "; RC$(27, 0); ", "; RC$(27, 1) + " " + RC$(27, 2) + " " + RC$(27, 3) + ", " + RC$(27, 4); QTR$(allRecords(27, 2))
End If
Print

Color 14: Print "MOST FREE THROW ATTEMPTS, GAME": Color 15
If allRecords(28, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(28, 0); "- "; RC$(28, 0); ", "; RC$(28, 1) + " " + RC$(28, 2) + " " + RC$(28, 3) + ", " + RC$(28, 4)
End If
Print

Color 14: Print "MOST FREE THROW ATTEMPTS, ONE HALF": Color 15
If allRecords(29, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(29, 0); "- "; RC$(29, 0); ", "; RC$(29, 1) + " " + RC$(29, 2) + " " + RC$(29, 3) + ", " + RC$(29, 4); HALF$(allRecords(29, 2))
End If
Print

Color 14: Print "MOST FREE THROW ATTEMPTS, ONE QUARTER": Color 15
If allRecords(30, 0) = 0 Then
    Print "NO RECORD AT THIS TIME";
Else
    Print allRecords(30, 0); "- "; RC$(30, 0); ", "; RC$(30, 1) + " " + RC$(30, 2) + " " + RC$(30, 3) + ", " + RC$(30, 4); QTR$(allRecords(30, 2));
End If
I$ = GetKeyPress$

Cls: Color 12
Print "                            INDIVIDUAL RECORDS"
Print
Color 14: Print "MOST REBOUNDS, GAME": Color 15
If allRecords(31, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(31, 0); "- "; RC$(31, 0); ", "; RC$(31, 1) + " " + RC$(31, 2) + " " + RC$(31, 3) + ", " + RC$(31, 4)
End If
Print

Color 14: Print "MOST REBOUNDS, ONE HALF": Color 15
If allRecords(32, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(32, 0); "- "; RC$(32, 0); ", "; RC$(32, 1) + " " + RC$(32, 2) + " " + RC$(32, 3) + ", " + RC$(32, 4); HALF$(allRecords(32, 2))
End If
Print

Color 14: Print "MOST REBOUNDS, ONE QUARTER": Color 15
If allRecords(33, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(33, 0); "- "; RC$(33, 0); ", "; RC$(33, 1) + " " + RC$(33, 2) + " " + RC$(33, 3) + ", " + RC$(33, 4); QTR$(allRecords(33, 2))
End If
Print

Color 14: Print "MOST OFFENSIVE REBOUNDS, GAME": Color 15
If allRecords(34, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(34, 0); "- "; RC$(34, 0); ", "; RC$(34, 1) + " " + RC$(34, 2) + " " + RC$(34, 3) + ", " + RC$(34, 4)
End If
Print

Color 14: Print "MOST OFFENSIVE REBOUNDS, ONE HALF": Color 15
If allRecords(35, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(35, 0); "- "; RC$(35, 0); ", "; RC$(35, 1) + " " + RC$(35, 2) + " " + RC$(35, 3) + ", " + RC$(35, 4); HALF$(allRecords(35, 2))
End If
Print

Color 14: Print "MOST OFFENSIVE REBOUNDS, ONE QUARTER": Color 15
If allRecords(36, 0) = 0 Then
    Print "NO RECORD AT THIS TIME";
Else
    Print allRecords(36, 0); "- "; RC$(36, 0); ", "; RC$(36, 1) + " " + RC$(36, 2) + " " + RC$(36, 3) + ", " + RC$(36, 4); QTR$(allRecords(36, 2));
End If
I$ = GetKeyPress$

Cls
Color 12
Print "                            INDIVIDUAL RECORDS"
Print
Color 14: Print "MOST DEFENSIVE REBOUNDS, GAME": Color 15
If allRecords(37, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(37, 0); "- "; RC$(37, 0); ", "; RC$(37, 1) + " " + RC$(37, 2) + " " + RC$(37, 3) + ", " + RC$(37, 4)
End If
Print

Color 14: Print "MOST DEFENSIVE REBOUNDS, ONE HALF": Color 15
If allRecords(38, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(38, 0); "- "; RC$(38, 0); ", "; RC$(38, 1) + " " + RC$(38, 2) + " " + RC$(38, 3) + ", " + RC$(38, 4); HALF$(allRecords(38, 2))
End If
Print

Color 14: Print "MOST DEFENSIVE REBOUNDS, ONE QUARTER": Color 15
If allRecords(39, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(39, 0); "- "; RC$(39, 0); ", "; RC$(39, 1) + " " + RC$(39, 2) + " " + RC$(39, 3) + ", " + RC$(39, 4); QTR$(allRecords(39, 2))
End If
Print

Color 14: Print "MOST ASSISTS, GAME": Color 15
If allRecords(40, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(40, 0); "- "; RC$(40, 0); ", "; RC$(40, 1) + " " + RC$(40, 2) + " " + RC$(40, 3) + ", " + RC$(40, 4)
End If
Print

Color 14: Print "MOST ASSISTS, ONE HALF": Color 15
If allRecords(41, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(41, 0); "- "; RC$(41, 0); ", "; RC$(41, 1) + " " + RC$(41, 2) + " " + RC$(41, 3) + ", " + RC$(41, 4); HALF$(allRecords(41, 2))
End If
Print

Color 14: Print "MOST ASSISTS, ONE QUARTER": Color 15
If allRecords(42, 0) = 0 Then
    Print "NO RECORD AT THIS TIME";
Else
    Print allRecords(42, 0); "- "; RC$(42, 0); ", "; RC$(42, 1) + " " + RC$(42, 2) + " " + RC$(42, 3) + ", " + RC$(42, 4); QTR$(allRecords(42, 2));
End If
I$ = GetKeyPress$

Cls: Color 12
Print "                            INDIVIDUAL RECORDS"
Print
Color 14: Print "MOST STEALS, GAME": Color 15
If allRecords(43, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(43, 0); "- "; RC$(43, 0); ", "; RC$(43, 1) + " " + RC$(43, 2) + " " + RC$(43, 3) + ", " + RC$(43, 4)
End If
Print

Color 14: Print "MOST STEALS, ONE HALF": Color 15
If allRecords(44, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(44, 0); "- "; RC$(44, 0); ", "; RC$(44, 1) + " " + RC$(44, 2) + " " + RC$(44, 3) + ", " + RC$(44, 4); HALF$(allRecords(44, 2))
End If
Print

Color 14: Print "MOST STEALS, ONE QUARTER": Color 15
If allRecords(45, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(45, 0); "- "; RC$(45, 0); ", "; RC$(45, 1) + " " + RC$(45, 2) + " " + RC$(45, 3) + ", " + RC$(45, 4); QTR$(allRecords(45, 2))
End If
Print

Color 14: Print "MOST BLOCKS, GAME": Color 15
If allRecords(46, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(46, 0); "- "; RC$(46, 0); ", "; RC$(46, 1) + " " + RC$(46, 2) + " " + RC$(46, 3) + ", " + RC$(46, 4)
End If
Print

Color 14: Print "MOST BLOCKS, ONE HALF": Color 15
If allRecords(47, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print allRecords(47, 0); "- "; RC$(47, 0); ", "; RC$(47, 1) + " " + RC$(47, 2) + " " + RC$(47, 3) + ", " + RC$(47, 4); HALF$(allRecords(47, 2))
End If
Print

Color 14: Print "MOST BLOCKS, ONE QUARTER": Color 15
If allRecords(48, 0) = 0 Then
    Print "NO RECORD AT THIS TIME";
Else
    Print allRecords(48, 0); "- "; RC$(48, 0); ", "; RC$(48, 1) + " " + RC$(48, 2) + " " + RC$(48, 3) + ", " + RC$(48, 4); QTR$(allRecords(48, 2));
End If
I$ = GetKeyPress$

Cls: Color 12
Print "                            TEAM RECORDS"
Print
Color 14: Print "MOST OVERTIME PERIODS": Color 15
If TotalRecords(0, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print TotalRecords(0, 0); "- "; TRC$(0, 1);
    Print Using " (###) AT "; TotalRecords(0, 1);
    Print TRC$(0, 2);: Print Using " (###)"; TotalRecords(0, 2);: Print ", " + TRC$(0, 3)
End If
Print

Color 14: Print "MOST POINTS, GAME": Color 15
Print TotalRecords(1, 0); "- "; TRC$(1, 0) + " " + TRC$(1, 1) + " " + TRC$(1, 2) + ", " + TRC$(1, 3)
Print

Color 14: Print "FEWEST POINTS, GAME": Color 15
Print TotalRecords(2, 0); "- "; TRC$(2, 0) + " " + TRC$(2, 1) + " " + TRC$(2, 2) + ", " + TRC$(2, 3)
Print

Color 14: Print "MOST POINTS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(3, 0); "- "; TRC$(3, 0);
Print Using " (###) "; TotalRecords(3, 1);: Print TRC$(3, 1) + " ";
Print TRC$(3, 2);: Print Using " (###)"; TotalRecords(3, 2);: Print ", " + TRC$(3, 3)
Print

Color 14: Print "FEWEST POINTS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(4, 0); "- "; TRC$(4, 0);
Print Using " (###) "; TotalRecords(4, 1);: Print TRC$(4, 1) + " ";
Print TRC$(4, 2);: Print Using " (###)"; TotalRecords(4, 2);: Print ", " + TRC$(4, 3)
Print

Color 14: Print "MOST POINTS, FIRST HALF": Color 15
Print TotalRecords(6, 0); "- "; TRC$(6, 0) + " " + TRC$(6, 1) + " " + TRC$(6, 2) + ", " + TRC$(6, 3)
Print

Color 14: Print "FEWEST POINTS, FIRST HALF": Color 15
Print TotalRecords(7, 0); "- "; TRC$(7, 0) + " " + TRC$(7, 1) + " " + TRC$(7, 2) + ", " + TRC$(7, 3)
Print
I$ = GetKeyPress$

Cls
Color 12
Print "                            TEAM RECORDS"
Print

Color 14: Print "MOST POINTS, BOTH TEAMS, FIRST HALF": Color 15
Print TotalRecords(8, 0); "- "; TRC$(8, 0);
Print Using " (###) "; TotalRecords(8, 1);: Print TRC$(8, 1) + " ";
Print TRC$(8, 2);: Print Using " (###)"; TotalRecords(8, 2);: Print ", " + TRC$(8, 3)
Print

Color 14: Print "FEWEST POINTS, BOTH TEAMS, FIRST HALF": Color 15
Print TotalRecords(9, 0); "- "; TRC$(9, 0);
Print Using " (###) "; TotalRecords(9, 1);: Print TRC$(9, 1) + " ";
Print TRC$(9, 2);: Print Using " (###)"; TotalRecords(9, 2);: Print ", " + TRC$(9, 3)
Print

Color 14: Print "MOST POINTS, SECOND HALF": Color 15
Print TotalRecords(10, 0); "- "; TRC$(10, 0) + " " + TRC$(10, 1) + " " + TRC$(10, 2) + ", " + TRC$(10, 3)
Print

Color 14: Print "FEWEST POINTS, SECOND HALF": Color 15
Print TotalRecords(11, 0); "- "; TRC$(11, 0) + " " + TRC$(11, 1) + " " + TRC$(11, 2) + ", " + TRC$(11, 3)
Print

Color 14: Print "MOST POINTS, BOTH TEAMS, SECOND HALF": Color 15
Print TotalRecords(12, 0); "- "; TRC$(12, 0);
Print Using " (###) "; TotalRecords(12, 1);: Print TRC$(12, 1) + " ";
Print TRC$(12, 2);: Print Using " (###)"; TotalRecords(12, 2);: Print ", " + TRC$(12, 3)
Print

Color 14: Print "FEWEST POINTS, BOTH TEAMS, SECOND HALF": Color 15
Print TotalRecords(13, 0); "- "; TRC$(13, 0);
Print Using " (###) "; TotalRecords(13, 1);: Print TRC$(13, 1) + " ";
Print TRC$(13, 2);: Print Using " (###)"; TotalRecords(13, 2);: Print ", " + TRC$(13, 3)
Print
I$ = GetKeyPress$

Cls
Color 12
Print "                            TEAM RECORDS"

Color 14: Print "MOST POINTS, FIRST QUARTER": Color 15
Print TotalRecords(14, 0); "- "; TRC$(14, 0) + " " + TRC$(14, 1) + " " + TRC$(14, 2) + ", " + TRC$(14, 3)
Print

Color 14: Print "FEWEST POINTS, FIRST QUARTER": Color 15
Print TotalRecords(15, 0); "- "; TRC$(15, 0) + " " + TRC$(15, 1) + " " + TRC$(15, 2) + ", " + TRC$(15, 3)
Print

Color 14: Print "MOST POINTS, BOTH TEAMS, FIRST QUARTER": Color 15
Print TotalRecords(16, 0); "- "; TRC$(16, 0);
Print Using " (###) "; TotalRecords(16, 1);: Print TRC$(16, 1) + " ";
Print TRC$(16, 2);: Print Using " (###)"; TotalRecords(16, 2);: Print ", " + TRC$(16, 3)
Print

Color 14: Print "FEWEST POINTS, BOTH TEAMS, FIRST QUARTER": Color 15
Print TotalRecords(17, 0); "- "; TRC$(17, 0);
Print Using " (###) "; TotalRecords(17, 1);: Print TRC$(17, 1) + " ";
Print TRC$(17, 2);: Print Using " (###)"; TotalRecords(17, 2);: Print ", " + TRC$(17, 3)
Print

Color 14: Print "MOST POINTS, SECOND QUARTER": Color 15
Print TotalRecords(18, 0); "- "; TRC$(18, 0) + " " + TRC$(18, 1) + " " + TRC$(18, 2) + ", " + TRC$(18, 3)
Print

Color 14: Print "FEWEST POINTS, SECOND QUARTER": Color 15
Print TotalRecords(19, 0); "- "; TRC$(19, 0) + " " + TRC$(19, 1) + " " + TRC$(19, 2) + ", " + TRC$(19, 3)
Print

Color 14: Print "MOST POINTS, BOTH TEAMS, SECOND QUARTER": Color 15
Print TotalRecords(20, 0); "- "; TRC$(20, 0);
Print Using " (###) "; TotalRecords(20, 1);: Print TRC$(20, 1) + " ";
Print TRC$(20, 2);: Print Using " (###)"; TotalRecords(20, 2);: Print ", " + TRC$(20, 3)
Print

Color 14: Print "FEWEST POINTS, BOTH TEAMS, SECOND QUARTER": Color 15
Print TotalRecords(21, 0); "- "; TRC$(21, 0);
Print Using " (###) "; TotalRecords(21, 1);: Print TRC$(21, 1) + " ";
Print TRC$(21, 2);: Print Using " (###)"; TotalRecords(21, 2);: Print ", " + TRC$(21, 3);
I$ = GetKeyPress$

Cls
Color 12
Print "                            TEAM RECORDS"

Color 14: Print "MOST POINTS, THIRD QUARTER": Color 15
Print TotalRecords(22, 0); "- "; TRC$(22, 0) + " " + TRC$(22, 1) + " " + TRC$(22, 2) + ", " + TRC$(22, 3)
Print

Color 14: Print "FEWEST POINTS, THIRD QUARTER": Color 15
Print TotalRecords(23, 0); "- "; TRC$(23, 0) + " " + TRC$(23, 1) + " " + TRC$(23, 2) + ", " + TRC$(23, 3)
Print

Color 14: Print "MOST POINTS, BOTH TEAMS, THIRD QUARTER": Color 15
Print TotalRecords(24, 0); "- "; TRC$(24, 0);
Print Using " (###) "; TotalRecords(24, 1);: Print TRC$(24, 1) + " ";
Print TRC$(24, 2);: Print Using " (###)"; TotalRecords(24, 2);: Print ", " + TRC$(24, 3)
Print

Color 14: Print "FEWEST POINTS, BOTH TEAMS, THIRD QUARTER": Color 15
Print TotalRecords(25, 0); "- "; TRC$(25, 0);
Print Using " (###) "; TotalRecords(25, 1);: Print TRC$(25, 1) + " ";
Print TRC$(25, 2);: Print Using " (###)"; TotalRecords(25, 2);: Print ", " + TRC$(25, 3)
Print

Color 14: Print "MOST POINTS, FOURTH QUARTER": Color 15
Print TotalRecords(26, 0); "- "; TRC$(26, 0) + " " + TRC$(26, 1) + " " + TRC$(26, 2) + ", " + TRC$(26, 3)
Print

Color 14: Print "FEWEST POINTS, FOURTH QUARTER": Color 15
Print TotalRecords(27, 0); "- "; TRC$(27, 0) + " " + TRC$(27, 1) + " " + TRC$(27, 2) + ", " + TRC$(27, 3)
Print

Color 14: Print "MOST POINTS, BOTH TEAMS, FOURTH QUARTER": Color 15
Print TotalRecords(28, 0); "- "; TRC$(28, 0);
Print Using " (###) "; TotalRecords(28, 1);: Print TRC$(28, 1) + " ";
Print TRC$(28, 2);: Print Using " (###)"; TotalRecords(28, 2);: Print ", " + TRC$(28, 3)
Print

Color 14: Print "FEWEST POINTS, BOTH TEAMS, FOURTH QUARTER": Color 15
Print TotalRecords(29, 0); "- "; TRC$(29, 0);
Print Using " (###) "; TotalRecords(29, 1);: Print TRC$(29, 1) + " ";
Print TRC$(29, 2);: Print Using " (###)"; TotalRecords(29, 2);: Print ", " + TRC$(29, 3);
I$ = GetKeyPress$

Cls: Color 12
Print "                            TEAM RECORDS"

Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, GAME": Color 15
Print Using ".### "; TotalRecords(30, 0);: Print "- "; TRC$(30, 0) + " " + TRC$(30, 1) + " " + TRC$(30, 2) + ", " + TRC$(30, 3);: Print Using "###/###"; TotalRecords(30, 1); TotalRecords(30, 2)
Print

Color 14: Print "LOWEST FIELD GOAL PERCENTAGE, GAME": Color 15
Print Using ".### "; TotalRecords(31, 0);: Print "- "; TRC$(31, 0) + " " + TRC$(31, 1) + " " + TRC$(31, 2) + ", " + TRC$(31, 3);: Print Using "###/###"; TotalRecords(31, 1); TotalRecords(31, 2)
Print

Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME": Color 15
Print Using ".### "; TotalRecords(32, 0);: Print "- "; TRC$(32, 0);
Print Using " (.###) "; TotalRecords(32, 1);: Print TRC$(32, 1) + " ";
Print TRC$(32, 2);: Print Using " (.###)"; TotalRecords(32, 2);: Print ", " + TRC$(32, 3)
Print

Color 14: Print "LOWEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME": Color 15
Print Using ".### "; TotalRecords(33, 0);: Print "- "; TRC$(33, 0);
Print Using " (.###) "; TotalRecords(33, 1);: Print TRC$(33, 1) + " ";
Print TRC$(33, 2);: Print Using " (.###)"; TotalRecords(33, 2);: Print ", " + TRC$(33, 3)
Print

Color 14: Print "MOST FIELD GOALS, GAME": Color 15
Print TotalRecords(34, 0); "- "; TRC$(34, 0) + " " + TRC$(34, 1) + " " + TRC$(34, 2) + ", " + TRC$(34, 3)
Print

Color 14: Print "FEWEST FIELD GOALS, GAME": Color 15
Print TotalRecords(35, 0); "- "; TRC$(35, 0) + " " + TRC$(35, 1) + " " + TRC$(35, 2) + ", " + TRC$(35, 3)
Print

Color 14: Print "MOST FIELD GOALS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(36, 0); "- "; TRC$(36, 0);
Print Using " (###) "; TotalRecords(36, 1);: Print TRC$(36, 1) + " ";
Print TRC$(36, 2);: Print Using " (###)"; TotalRecords(36, 2);: Print ", " + TRC$(36, 3)
Print

Color 14: Print "FEWEST FIELD GOALS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(37, 0); "- "; TRC$(37, 0);
Print Using " (###) "; TotalRecords(37, 1);: Print TRC$(37, 1) + " ";
Print TRC$(37, 2);: Print Using " (###)"; TotalRecords(37, 2);: Print ", " + TRC$(37, 3);
I$ = GetKeyPress$

Cls: Color 12
Print "                            TEAM RECORDS"

Color 14: Print "MOST FIELD GOALS, ONE HALF": Color 15
Print TotalRecords(38, 0); "- "; TRC$(38, 0) + " " + TRC$(38, 1) + " " + TRC$(38, 2) + ", " + TRC$(38, 3); HALF$(TotalRecords(38, 1))
Print

Color 14: Print "MOST FIELD GOALS, BOTH TEAMS, ONE HALF": Color 15
Print TotalRecords(39, 0); "- "; TRC$(39, 0); " "; TRC$(39, 1); " "; TRC$(39, 2); ", "; TRC$(39, 3); HALF$(TotalRecords(39, 1))
Print

Color 14: Print "MOST FIELD GOALS, ONE QUARTER": Color 15
Print TotalRecords(40, 0); "- "; TRC$(40, 0) + " " + TRC$(40, 1) + " " + TRC$(40, 2) + ", " + TRC$(40, 3); QTR$(TotalRecords(40, 1))
Print

Color 14: Print "MOST FIELD GOALS, BOTH TEAMS, ONE QUARTER": Color 15
Print TotalRecords(41, 0); "- "; TRC$(41, 0); " "; TRC$(41, 1); " "; TRC$(41, 2); ", "; TRC$(41, 3); QTR$(TotalRecords(41, 1))
Print

Color 14: Print "MOST FIELD GOAL ATTEMPTS, GAME": Color 15
Print TotalRecords(42, 0); "- "; TRC$(42, 0) + " " + TRC$(42, 1) + " " + TRC$(42, 2) + ", " + TRC$(42, 3)
Print

Color 14: Print "FEWEST FIELD GOAL ATTEMPTS, GAME": Color 15
Print TotalRecords(43, 0); "- "; TRC$(43, 0) + " " + TRC$(43, 1) + " " + TRC$(43, 2) + ", " + TRC$(43, 3)
Print

Color 14: Print "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(44, 0); "- "; TRC$(44, 0);
Print Using " (###) "; TotalRecords(44, 1);: Print TRC$(44, 1) + " ";
Print TRC$(44, 2);: Print Using " (###)"; TotalRecords(44, 2);: Print ", " + TRC$(44, 3)
Print

Color 14: Print "FEWEST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(45, 0); "- "; TRC$(45, 0);
Print Using " (###) "; TotalRecords(45, 1);: Print TRC$(45, 1) + " ";
Print TRC$(45, 2);: Print Using " (###)"; TotalRecords(45, 2);: Print ", " + TRC$(45, 3);
I$ = GetKeyPress$

Cls: Color 12
Print "                            TEAM RECORDS"
Print

Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE HALF": Color 15
Print TotalRecords(46, 0); "- "; TRC$(46, 0) + " " + TRC$(46, 1) + " " + TRC$(46, 2) + ", " + TRC$(46, 3); HALF$(TotalRecords(46, 1))
Print

Color 14: Print "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE HALF": Color 15
Print TotalRecords(47, 0); "- "; TRC$(47, 0); " "; TRC$(47, 1); " "; TRC$(47, 2); ", "; TRC$(47, 3); HALF$(TotalRecords(47, 1))
Print

Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE QUARTER": Color 15
Print TotalRecords(48, 0); "- "; TRC$(48, 0) + " " + TRC$(48, 1) + " " + TRC$(48, 2) + ", " + TRC$(48, 3); QTR$(TotalRecords(48, 1))
Print

Color 14: Print "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE QUARTER": Color 15
Print TotalRecords(49, 0); "- "; TRC$(49, 0); " "; TRC$(49, 1); " "; TRC$(49, 2); ", "; TRC$(49, 3); QTR$(TotalRecords(49, 1))
Print

Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME": Color 15
If TotalRecords(50, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print TotalRecords(50, 0); "- "; TRC$(50, 0) + " " + TRC$(50, 1) + " " + TRC$(50, 2) + ", " + TRC$(50, 3)
End If
Print

Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, BOTH TEAMS, GAME": Color 15
If TotalRecords(51, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print TotalRecords(51, 0); "- "; TRC$(51, 0);
    Print Using " (###) "; TotalRecords(51, 1);: Print TRC$(51, 1) + " ";
    Print TRC$(51, 2);: Print Using " (###)"; TotalRecords(51, 2);: Print ", " + TRC$(51, 3)
End If
Print

Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME": Color 15
If TotalRecords(52, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print TotalRecords(52, 0); "- "; TRC$(52, 0) + " " + TRC$(52, 1) + " " + TRC$(52, 2) + ", " + TRC$(52, 3)
End If
Print
I$ = GetKeyPress$

Cls: Color 12
Print "                            TEAM RECORDS"
Print

Color 14: Print "MOST THREE-POINT FIELD GOALS, GAME": Color 15
Print TotalRecords(53, 0); "- "; TRC$(53, 0) + " " + TRC$(53, 1) + " " + TRC$(53, 2) + ", " + TRC$(53, 3)
Print

Color 14: Print "MOST THREE-POINT FIELD GOALS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(54, 0); "- "; TRC$(54, 0);
Print Using " (###) "; TotalRecords(54, 1);: Print TRC$(54, 1) + " ";
Print TRC$(54, 2);: Print Using " (###)"; TotalRecords(54, 2);: Print ", " + TRC$(54, 3)
Print

Color 14: Print "MOST THREE-POINT FIELD GOALS, ONE HALF": Color 15
Print TotalRecords(55, 0); "- "; TRC$(55, 0) + " " + TRC$(55, 1) + " " + TRC$(55, 2) + ", " + TRC$(55, 3); HALF$(TotalRecords(55, 1))
Print

Color 14: Print "MOST THREE-POINT FIELD GOALS, ONE QUARTER": Color 15
Print TotalRecords(56, 0); "- "; TRC$(56, 0) + " " + TRC$(56, 1) + " " + TRC$(56, 2) + ", " + TRC$(56, 3); QTR$(TotalRecords(56, 1))
Print

Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME": Color 15
Print TotalRecords(57, 0); "- "; TRC$(57, 0) + " " + TRC$(57, 1) + " " + TRC$(57, 2) + ", " + TRC$(57, 3)
Print

Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(58, 0); "- "; TRC$(58, 0);
Print Using " (###) "; TotalRecords(58, 1);: Print TRC$(58, 1) + " ";
Print TRC$(58, 2);: Print Using " (###)"; TotalRecords(58, 2);: Print ", " + TRC$(58, 3)
Print

Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF": Color 15
Print TotalRecords(59, 0); "- "; TRC$(59, 0) + " " + TRC$(59, 1) + " " + TRC$(59, 2) + ", " + TRC$(59, 3); HALF$(TotalRecords(59, 1))
I$ = GetKeyPress$

Cls: Color 12
Print "                            TEAM RECORDS"

Color 14: Print "MOST FREE THROWS MADE, NO MISSES, GAME": Color 15
If TotalRecords(60, 0) = 0 Then
    Print "NO RECORD AT THIS TIME"
Else
    Print TotalRecords(60, 0); "- "; TRC$(60, 0) + " " + TRC$(60, 1) + " " + TRC$(60, 2) + ", " + TRC$(60, 3)
End If
Print

Color 14: Print "LOWEST FREE THROW PERCENTAGE, GAME": Color 15
Print Using ".### "; TotalRecords(61, 0);: Print "- "; TRC$(61, 0) + " " + TRC$(61, 1) + " " + TRC$(61, 2) + ", " + TRC$(61, 3);: Print Using "###/###"; TotalRecords(61, 1); TotalRecords(61, 2)
Print

Color 14: Print "HIGHEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME": Color 15
Print Using ".### "; TotalRecords(62, 0);: Print "- "; TRC$(62, 0);
Print Using " (.###) "; TotalRecords(62, 1);: Print TRC$(62, 1) + " ";
Print TRC$(62, 2);: Print Using " (.###)"; TotalRecords(62, 2);: Print ", " + TRC$(62, 3)
Print

Color 14: Print "LOWEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME": Color 15
Print Using ".### "; TotalRecords(63, 0);: Print "- "; TRC$(63, 0);
Print Using " (.###) "; TotalRecords(63, 1);: Print TRC$(63, 1) + " ";
Print TRC$(63, 2);: Print Using " (.###)"; TotalRecords(63, 2);: Print ", " + TRC$(63, 3)
Print

Color 14: Print "MOST FREE THROWS MADE, GAME": Color 15
Print TotalRecords(64, 0); "- "; TRC$(64, 0) + " " + TRC$(64, 1) + " " + TRC$(64, 2) + ", " + TRC$(64, 3)
Print

Color 14: Print "FEWEST FREE THROWS MADE, GAME": Color 15
Print TotalRecords(65, 0); "- "; TRC$(65, 0) + " " + TRC$(65, 1) + " " + TRC$(65, 2) + ", " + TRC$(65, 3)
Print

Color 14: Print "MOST FREE THROWS MADE, BOTH TEAMS, GAME": Color 15
Print TotalRecords(66, 0); "- "; TRC$(66, 0);
Print Using " (###) "; TotalRecords(66, 1);: Print TRC$(66, 1) + " ";
Print TRC$(66, 2);: Print Using " (###)"; TotalRecords(66, 2);: Print ", " + TRC$(66, 3)
Print

Color 14: Print "FEWEST FREE THROWS MADE, BOTH TEAMS, GAME": Color 15
Print TotalRecords(67, 0); "- "; TRC$(67, 0);
Print Using " (###) "; TotalRecords(67, 1);: Print TRC$(67, 1) + " ";
Print TRC$(67, 2);: Print Using " (###)"; TotalRecords(67, 2);: Print ", " + TRC$(67, 3);
I$ = GetKeyPress$

Cls: Color 12
Print "                            TEAM RECORDS"

Color 14: Print "MOST FREE THROWS MADE, ONE HALF": Color 15
Print TotalRecords(68, 0); "- "; TRC$(68, 0) + " " + TRC$(68, 1) + " " + TRC$(68, 2) + ", " + TRC$(68, 3); HALF$(TotalRecords(68, 1))
Print

Color 14: Print "MOST FREE THROWS MADE, ONE QUARTER": Color 15
Print TotalRecords(70, 0); "- "; TRC$(70, 0) + " " + TRC$(70, 1) + " " + TRC$(70, 2) + ", " + TRC$(70, 3); QTR$(TotalRecords(70, 1))
Print

Color 14: Print "MOST FREE THROW ATTEMPTS, GAME": Color 15
Print TotalRecords(72, 0); "- "; TRC$(72, 0) + " " + TRC$(72, 1) + " " + TRC$(72, 2) + ", " + TRC$(72, 3)
Print

Color 14: Print "FEWEST FREE THROW ATTEMPTS, GAME": Color 15
Print TotalRecords(73, 0); "- "; TRC$(73, 0) + " " + TRC$(73, 1) + " " + TRC$(73, 2) + ", " + TRC$(73, 3)
Print

Color 14: Print "MOST FREE THROW ATTEMPTS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(74, 0); "- "; TRC$(74, 0);
Print Using " (###) "; TotalRecords(74, 1);: Print TRC$(74, 1) + " ";
Print TRC$(74, 2);: Print Using " (###)"; TotalRecords(74, 2);: Print ", " + TRC$(74, 3)
Print

Color 14: Print "FEWEST FREE THROW ATTEMPTS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(75, 0); "- "; TRC$(75, 0);
Print Using " (###) "; TotalRecords(75, 1);: Print TRC$(75, 1) + " ";
Print TRC$(75, 2);: Print Using " (###)"; TotalRecords(75, 2);: Print ", " + TRC$(75, 3)
Print

Color 14: Print "MOST FREE THROW ATTEMPTS, ONE HALF": Color 15
Print TotalRecords(76, 0); "- "; TRC$(76, 0) + " " + TRC$(76, 1) + " " + TRC$(76, 2) + ", " + TRC$(76, 3); HALF$(TotalRecords(76, 1))
Print

Color 14: Print "MOST FREE THROW ATTEMPTS, ONE QUARTER": Color 15
Print TotalRecords(78, 0); "- "; TRC$(78, 0) + " " + TRC$(78, 1) + " " + TRC$(78, 2) + ", " + TRC$(78, 3); QTR$(TotalRecords(78, 1));
I$ = GetKeyPress$

Cls: Color 12
Print "                            TEAM RECORDS"
Print
Print
Color 14: Print "MOST REBOUNDS, GAME": Color 15
Print TotalRecords(80, 0); "- "; TRC$(80, 0) + " " + TRC$(80, 1) + " " + TRC$(80, 2) + ", " + TRC$(80, 3)
Print

Color 14: Print "FEWEST REBOUNDS, GAME": Color 15
Print TotalRecords(81, 0); "- "; TRC$(81, 0) + " " + TRC$(81, 1) + " " + TRC$(81, 2) + ", " + TRC$(81, 3)
Print

Color 14: Print "MOST REBOUNDS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(82, 0); "- "; TRC$(82, 0);
Print Using " (###) "; TotalRecords(82, 1);: Print TRC$(82, 1) + " ";
Print TRC$(82, 2);: Print Using " (###)"; TotalRecords(82, 2);: Print ", " + TRC$(82, 3)
Print

Color 14: Print "FEWEST REBOUNDS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(83, 0); "- "; TRC$(83, 0);
Print Using " (###) "; TotalRecords(83, 1);: Print TRC$(83, 1) + " ";
Print TRC$(83, 2);: Print Using " (###)"; TotalRecords(83, 2);: Print ", " + TRC$(83, 3)
Print

Color 14: Print "MOST REBOUNDS, ONE HALF": Color 15
Print TotalRecords(84, 0); "- "; TRC$(84, 0) + " " + TRC$(84, 1) + " " + TRC$(84, 2) + ", " + TRC$(84, 3); HALF$(TotalRecords(84, 1))
Print

Color 14: Print "MOST REBOUNDS, ONE QUARTER": Color 15
Print TotalRecords(85, 0); "- "; TRC$(85, 0) + " " + TRC$(85, 1) + " " + TRC$(85, 2) + ", " + TRC$(85, 3); QTR$(TotalRecords(85, 1))
Print
I$ = GetKeyPress$

Cls: Color 12
Print "                            TEAM RECORDS"
Print
Print
Color 14: Print "MOST OFFENSIVE REBOUNDS, GAME": Color 15
Print TotalRecords(86, 0); "- "; TRC$(86, 0) + " " + TRC$(86, 1) + " " + TRC$(86, 2) + ", " + TRC$(86, 3)
Print

Color 14: Print "MOST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(88, 0); "- "; TRC$(88, 0);
Print Using " (###) "; TotalRecords(88, 1);: Print TRC$(88, 1) + " ";
Print TRC$(88, 2);: Print Using " (###)"; TotalRecords(88, 2);: Print ", " + TRC$(88, 3)
Print

Color 14: Print "FEWEST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(89, 0); "- "; TRC$(89, 0);
Print Using " (###) "; TotalRecords(89, 1);: Print TRC$(89, 1) + " ";
Print TRC$(89, 2);: Print Using " (###)"; TotalRecords(89, 2);: Print ", " + TRC$(89, 3)
Print

Color 14: Print "MOST OFFENSIVE REBOUNDS, ONE HALF": Color 15
Print TotalRecords(90, 0); "- "; TRC$(90, 0) + " " + TRC$(90, 1) + " " + TRC$(90, 2) + ", " + TRC$(90, 3); HALF$(TotalRecords(90, 1))
Print
I$ = GetKeyPress$

Cls: Color 12
Print "                            TEAM RECORDS"
Print
Color 14: Print "MOST ASSISTS, GAME": Color 15
Print TotalRecords(96, 0); "- "; TRC$(96, 0) + " " + TRC$(96, 1) + " " + TRC$(96, 2) + ", " + TRC$(96, 3)
Print

Color 14: Print "FEWEST ASSISTS, GAME": Color 15
Print TotalRecords(97, 0); "- "; TRC$(97, 0) + " " + TRC$(97, 1) + " " + TRC$(97, 2) + ", " + TRC$(97, 3)
Print

Color 14: Print "MOST ASSISTS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(98, 0); "- "; TRC$(98, 0);
Print Using " (###) "; TotalRecords(98, 1);: Print TRC$(98, 1) + " ";
Print TRC$(98, 2);: Print Using " (###)"; TotalRecords(98, 2);: Print ", " + TRC$(98, 3)
Print

Color 14: Print "FEWEST ASSISTS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(99, 0); "- "; TRC$(99, 0);
Print Using " (###) "; TotalRecords(99, 1);: Print TRC$(99, 1) + " ";
Print TRC$(99, 2);: Print Using " (###)"; TotalRecords(99, 2);: Print ", " + TRC$(99, 3)
Print

Color 14: Print "MOST ASSISTS, ONE HALF": Color 15
Print TotalRecords(100, 0); "- "; TRC$(100, 0) + " " + TRC$(100, 1) + " " + TRC$(100, 2) + ", " + TRC$(100, 3); HALF$(TotalRecords(100, 1))
Print

Color 14: Print "MOST ASSISTS, ONE QUARTER": Color 15
Print TotalRecords(102, 0); "- "; TRC$(102, 0) + " " + TRC$(102, 1) + " " + TRC$(102, 2) + ", " + TRC$(102, 3); QTR$(TotalRecords(102, 1))
Print

I$ = GetKeyPress$

Cls: Color 12
Print "                            TEAM RECORDS"
Color 14: Print "MOST PERSONAL FOULS, GAME": Color 15
Print TotalRecords(104, 0); "- "; TRC$(104, 0) + " " + TRC$(104, 1) + " " + TRC$(104, 2) + ", " + TRC$(104, 3)
Print

Color 14: Print "FEWEST PERSONAL FOULS, GAME": Color 15
Print TotalRecords(105, 0); "- "; TRC$(105, 0) + " " + TRC$(105, 1) + " " + TRC$(105, 2) + ", " + TRC$(105, 3)
Print

Color 14: Print "MOST PERSONAL FOULS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(106, 0); "- "; TRC$(106, 0);
Print Using " (###) "; TotalRecords(106, 1);: Print TRC$(106, 1) + " ";
Print TRC$(106, 2);: Print Using " (###)"; TotalRecords(106, 2);: Print ", " + TRC$(106, 3)
Print

Color 14: Print "FEWEST PERSONAL FOULS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(107, 0); "- "; TRC$(107, 0);
Print Using " (###) "; TotalRecords(107, 1);: Print TRC$(107, 1) + " ";
Print TRC$(107, 2);: Print Using " (###)"; TotalRecords(107, 2);: Print ", " + TRC$(107, 3)
Print

Color 14: Print "MOST PERSONAL FOULS, ONE HALF": Color 15
Print TotalRecords(108, 0); "- "; TRC$(108, 0) + " " + TRC$(108, 1) + " " + TRC$(108, 2) + ", " + TRC$(108, 3); HALF$(TotalRecords(108, 1))
Print

Color 14: Print "MOST PERSONAL FOULS, BOTH TEAMS, ONE HALF": Color 15
Print TotalRecords(109, 0); "- "; TRC$(109, 0); " "; TRC$(109, 1); " "; TRC$(109, 2); ", "; TRC$(109, 3); HALF$(TotalRecords(109, 1))
Print

Color 14: Print "MOST PERSONAL FOULS, ONE QUARTER": Color 15
Print TotalRecords(110, 0); "- "; TRC$(110, 0) + " " + TRC$(110, 1) + " " + TRC$(110, 2) + ", " + TRC$(110, 3); QTR$(TotalRecords(110, 1))
Print

Color 14: Print "MOST PERSONAL FOULS, BOTH TEAMS, ONE QUARTER": Color 15
Print TotalRecords(111, 0); "- "; TRC$(111, 0); " "; TRC$(111, 1); " "; TRC$(111, 2); ", "; TRC$(111, 3); QTR$(TotalRecords(111, 1));

I$ = GetKeyPress$

Cls: Color 12
Print "                            TEAM RECORDS"
Print
Color 14: Print "MOST STEALS, GAME": Color 15
Print TotalRecords(112, 0); "- "; TRC$(112, 0) + " " + TRC$(112, 1) + " " + TRC$(112, 2) + ", " + TRC$(112, 3)
Print

Color 14: Print "MOST STEALS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(114, 0); "- "; TRC$(114, 0);
Print Using " (###) "; TotalRecords(114, 1);: Print TRC$(114, 1) + " ";
Print TRC$(114, 2);: Print Using " (###)"; TotalRecords(114, 2);: Print ", " + TRC$(114, 3)
Print

Color 14: Print "MOST STEALS, ONE HALF": Color 15
Print TotalRecords(116, 0); "- "; TRC$(116, 0) + " " + TRC$(116, 1) + " " + TRC$(116, 2) + ", " + TRC$(116, 3); HALF$(TotalRecords(116, 1))
Print

Color 14: Print "MOST STEALS, ONE QUARTER": Color 15
Print TotalRecords(117, 0); "- "; TRC$(117, 0) + " " + TRC$(117, 1) + " " + TRC$(117, 2) + ", " + TRC$(117, 3); QTR$(TotalRecords(117, 1))
Print

Color 14: Print "MOST BLOCKS, GAME": Color 15
Print TotalRecords(118, 0); "- "; TRC$(118, 0) + " " + TRC$(118, 1) + " " + TRC$(118, 2) + ", " + TRC$(118, 3)
Print

Color 14: Print "MOST BLOCKS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(119, 0); "- "; TRC$(119, 0);
Print Using " (###) "; TotalRecords(119, 1);: Print TRC$(119, 1) + " ";
Print TRC$(119, 2);: Print Using " (###)"; TotalRecords(119, 2);: Print ", " + TRC$(119, 3)
Print
I$ = GetKeyPress$

Cls: Color 12
Print "                            TEAM RECORDS"
Print
Color 14: Print "MOST TURNOVERS, GAME": Color 15
Print TotalRecords(122, 0); "- "; TRC$(122, 0) + " " + TRC$(122, 1) + " " + TRC$(122, 2) + ", " + TRC$(122, 3)
Print

Color 14: Print "FEWEST TURNOVERS, GAME": Color 15
Print TotalRecords(123, 0); "- "; TRC$(123, 0) + " " + TRC$(123, 1) + " " + TRC$(123, 2) + ", " + TRC$(123, 3)
Print

Color 14: Print "MOST TURNOVERS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(124, 0); "- "; TRC$(124, 0);
Print Using " (###) "; TotalRecords(124, 1);: Print TRC$(124, 1) + " ";
Print TRC$(124, 2);: Print Using " (###)"; TotalRecords(124, 2);: Print ", " + TRC$(124, 3)
Print

Color 14: Print "FEWEST TURNOVERS, BOTH TEAMS, GAME": Color 15
Print TotalRecords(125, 0); "- "; TRC$(125, 0);
Print Using " (###) "; TotalRecords(125, 1);: Print TRC$(125, 1) + " ";
Print TRC$(125, 2);: Print Using " (###)"; TotalRecords(125, 2);: Print ", " + TRC$(125, 3)
Print

I$ = GetKeyPress$
GoTo 300

200 'PRINT ROUTINE
Open "RECORDS" For Output As #2
Print #2, "                            INDIVIDUAL RECORDS"
Print #2,
Print #2, "MOST POINTS, GAME"
Print #2, allRecords(0, 0); "- "; RC$(0, 0); ", "; RC$(0, 1) + " " + RC$(0, 2) + " " + RC$(0, 3) + ", " + RC$(0, 4)
Print #2,
Print #2, "MOST POINTS, ONE HALF"
Print #2, allRecords(1, 0); "- "; RC$(1, 0); ", "; RC$(1, 1) + " " + RC$(1, 2) + " " + RC$(1, 3) + ", " + RC$(1, 4); HALF$(allRecords(1, 2))
Print #2,
Print #2, "MOST POINTS, ONE QUARTER"
Print #2, allRecords(2, 0); "- "; RC$(2, 0); ", "; RC$(2, 1) + " " + RC$(2, 2) + " " + RC$(2, 3) + ", " + RC$(2, 4); QTR$(allRecords(2, 2))
Print #2,
Print #2, "MOST POINTS, OVERTIME PERIOD"
If allRecords(3, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(3, 0); "- "; RC$(3, 0); ", "; RC$(3, 1) + " " + RC$(3, 2) + " " + RC$(3, 3) + ", " + RC$(3, 4)
End If
Print #2,
Print #2, "HIGHEST FIELD GOAL PERCENTAGE, GAME (MINIMUM 15 FIELD GOALS)"
If allRecords(4, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
    GoTo 210
End If
If allRecords(4, 0) = 1 Then
    Print #2, "1.000 ";
Else
    Print #2, Using " .### "; allRecords(4, 0);
End If
Print #2, "- "; RC$(4, 0); ", "; RC$(4, 1) + " " + RC$(4, 2) + " " + RC$(4, 3) + ", " + RC$(4, 4);
Print #2, Using " (##/##)"; allRecords(4, 1); allRecords(4, 2)

210 Print #2,
Print #2, "MOST FIELD GOALS, NO MISSES, GAME"
If allRecords(5, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(5, 0); "- "; RC$(5, 0); ", "; RC$(5, 1) + " " + RC$(5, 2) + " " + RC$(5, 3) + ", " + RC$(5, 4)
End If
Print #2,
Print #2, "MOST FIELD GOAL ATTEMPTS, NONE MADE, GAME"
If allRecords(6, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(6, 0); "- "; RC$(6, 0); ", "; RC$(6, 1) + " " + RC$(6, 2) + " " + RC$(6, 3) + ", " + RC$(6, 4)
End If
Print #2,
Print #2, "MOST FIELD GOALS, GAME"
Print #2, allRecords(7, 0); "- "; RC$(7, 0); ", "; RC$(7, 1) + " " + RC$(7, 2) + " " + RC$(7, 3) + ", " + RC$(7, 4)
Print #2,
Print #2, "MOST FIELD GOALS, ONE HALF"
Print #2, allRecords(8, 0); "- "; RC$(8, 0); ", "; RC$(8, 1) + " " + RC$(8, 2) + " " + RC$(8, 3) + ", " + RC$(8, 4); HALF$(allRecords(8, 2))
Print #2,
Print #2, "MOST FIELD GOALS, ONE QUARTER"
Print #2, allRecords(9, 0); "- "; RC$(9, 0); ", "; RC$(9, 1) + " " + RC$(9, 2) + " " + RC$(9, 3) + ", " + RC$(9, 4); QTR$(allRecords(9, 2))
Print #2,
Print #2, "MOST FIELD GOALS, OVERTIME"
If allRecords(10, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(10, 0); "- "; RC$(10, 0); ", "; RC$(10, 1) + " " + RC$(10, 2) + " " + RC$(10, 3) + ", " + RC$(10, 4)
End If
Print #2,
Print #2, "MOST FIELD GOAL ATTEMPTS, GAME"
Print #2, allRecords(11, 0); "- "; RC$(11, 0); ", "; RC$(11, 1) + " " + RC$(11, 2) + " " + RC$(11, 3) + ", " + RC$(11, 4)
Print #2,
Print #2, "MOST FIELD GOAL ATTEMPTS, ONE HALF"
Print #2, allRecords(12, 0); "- "; RC$(12, 0); ", "; RC$(12, 1) + " " + RC$(12, 2) + " " + RC$(12, 3) + ", " + RC$(12, 4); HALF$(allRecords(12, 2))
Print #2,
Print #2, "MOST FIELD GOAL ATTEMPTS, ONE QUARTER"
Print #2, allRecords(13, 0); "- "; RC$(13, 0); ", "; RC$(13, 1) + " " + RC$(13, 2) + " " + RC$(13, 3) + ", " + RC$(13, 4); QTR$(allRecords(13, 2))
Print #2,
Print #2, "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
If allRecords(15, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(15, 0); "- "; RC$(15, 0); ", "; RC$(15, 1) + " " + RC$(15, 2) + " " + RC$(15, 3) + ", " + RC$(15, 4)
End If
Print #2,
Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
If allRecords(16, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(16, 0); "- "; RC$(16, 0); ", "; RC$(16, 1) + " " + RC$(16, 2) + " " + RC$(16, 3) + ", " + RC$(16, 4)
End If
Print #2,
Print #2, "MOST THREE-POINT FIELD GOALS, GAME"
If allRecords(17, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(17, 0); "- "; RC$(17, 0); ", "; RC$(17, 1) + " " + RC$(17, 2) + " " + RC$(17, 3) + ", " + RC$(17, 4)
End If
Print #2,
Print #2, "MOST THREE-POINT FIELD GOALS MADE, ONE HALF"
If allRecords(18, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(18, 0); "- "; RC$(18, 0); ", "; RC$(18, 1) + " " + RC$(18, 2) + " " + RC$(18, 3) + ", " + RC$(18, 4); HALF$(allRecords(18, 2))
End If
Print #2,
Print #2, "MOST THREE-POINT FIELD GOALS MADE, ONE QUARTER"
If allRecords(19, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(19, 0); "- "; RC$(19, 0); ", "; RC$(19, 1) + " " + RC$(19, 2) + " " + RC$(19, 3) + ", " + RC$(19, 4); QTR$(allRecords(19, 2))
End If
Print #2,
Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
If allRecords(21, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(21, 0); "- "; RC$(21, 0); ", "; RC$(21, 1) + " " + RC$(21, 2) + " " + RC$(21, 3) + ", " + RC$(21, 4)
End If
Print #2,
Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
If allRecords(22, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(22, 0); "- "; RC$(22, 0); ", "; RC$(22, 1) + " " + RC$(22, 2) + " " + RC$(22, 3) + ", " + RC$(22, 4); HALF$(allRecords(22, 2))
End If
Print #2,
    
Print #2, "MOST FREE THROWS MADE, NO MISSES, GAME"
If allRecords(23, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(23, 0); "- "; RC$(23, 0); ", "; RC$(23, 1) + " " + RC$(23, 2) + " " + RC$(23, 3) + ", " + RC$(23, 4)
End If
Print #2,
Print #2, "MOST FREE THROW ATTEMPTS, NONE MADE, GAME"
If allRecords(24, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(24, 0); "- "; RC$(24, 0); ", "; RC$(24, 1) + " " + RC$(24, 2) + " " + RC$(24, 3) + ", " + RC$(24, 4)
End If
Print #2,
Print #2, "MOST FREE THROWS MADE, GAME"
If allRecords(25, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(25, 0); "- "; RC$(25, 0); ", "; RC$(25, 1) + " " + RC$(25, 2) + " " + RC$(25, 3) + ", " + RC$(25, 4)
End If
Print #2,
Print #2, "MOST FREE THROWS MADE, ONE HALF"
If allRecords(26, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(26, 0); "- "; RC$(26, 0); ", "; RC$(26, 1) + " " + RC$(26, 2) + " " + RC$(26, 3) + ", " + RC$(26, 4); HALF$(allRecords(26, 2))
End If
Print #2,
Print #2, "MOST FREE THROWS MADE, ONE QUARTER"
If allRecords(27, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(27, 0); "- "; RC$(27, 0); ", "; RC$(27, 1) + " " + RC$(27, 2) + " " + RC$(27, 3) + ", " + RC$(27, 4); QTR$(allRecords(27, 2))
End If
Print #2,
Print #2, "MOST FREE THROW ATTEMPTS, GAME"
If allRecords(28, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(28, 0); "- "; RC$(28, 0); ", "; RC$(28, 1) + " " + RC$(28, 2) + " " + RC$(28, 3) + ", " + RC$(28, 4)
End If
Print #2,
Print #2, "MOST FREE THROW ATTEMPTS, ONE HALF"
If allRecords(29, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(29, 0); "- "; RC$(29, 0); ", "; RC$(29, 1) + " " + RC$(29, 2) + " " + RC$(29, 3) + ", " + RC$(29, 4); HALF$(allRecords(29, 2))
End If
Print #2,
Print #2, "MOST FREE THROW ATTEMPTS, ONE QUARTER"
If allRecords(30, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME";
Else
    Print #2, allRecords(30, 0); "- "; RC$(30, 0); ", "; RC$(30, 1) + " " + RC$(30, 2) + " " + RC$(30, 3) + ", " + RC$(30, 4); QTR$(allRecords(30, 2))
End If
Print #2,
Print #2, "MOST REBOUNDS, GAME"
If allRecords(31, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(31, 0); "- "; RC$(31, 0); ", "; RC$(31, 1) + " " + RC$(31, 2) + " " + RC$(31, 3) + ", " + RC$(31, 4)
End If
Print #2,
Print #2, "MOST REBOUNDS, ONE HALF"
If allRecords(32, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(32, 0); "- "; RC$(32, 0); ", "; RC$(32, 1) + " " + RC$(32, 2) + " " + RC$(32, 3) + ", " + RC$(32, 4); HALF$(allRecords(32, 2))
End If
Print #2,
Print #2, "MOST REBOUNDS, ONE QUARTER"
If allRecords(33, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(33, 0); "- "; RC$(33, 0); ", "; RC$(33, 1) + " " + RC$(33, 2) + " " + RC$(33, 3) + ", " + RC$(33, 4); QTR$(allRecords(33, 2))
End If
Print #2,
Print #2, "MOST OFFENSIVE REBOUNDS, GAME"
If allRecords(34, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(34, 0); "- "; RC$(34, 0); ", "; RC$(34, 1) + " " + RC$(34, 2) + " " + RC$(34, 3) + ", " + RC$(34, 4)
End If
Print #2,
Print #2, "MOST OFFENSIVE REBOUNDS, ONE HALF"
If allRecords(35, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(35, 0); "- "; RC$(35, 0); ", "; RC$(35, 1) + " " + RC$(35, 2) + " " + RC$(35, 3) + ", " + RC$(35, 4); HALF$(allRecords(35, 2))
End If
Print #2,
Print #2, "MOST OFFENSIVE REBOUNDS, ONE QUARTER"
If allRecords(36, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME";
Else
    Print #2, allRecords(36, 0); "- "; RC$(36, 0); ", "; RC$(36, 1) + " " + RC$(36, 2) + " " + RC$(36, 3) + ", " + RC$(36, 4); QTR$(allRecords(36, 2))
End If
Print #2,
Print #2, "MOST DEFENSIVE REBOUNDS, GAME"
If allRecords(37, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(37, 0); "- "; RC$(37, 0); ", "; RC$(37, 1) + " " + RC$(37, 2) + " " + RC$(37, 3) + ", " + RC$(37, 4)
End If
Print #2,
Print #2, "MOST DEFENSIVE REBOUNDS, ONE HALF"
If allRecords(38, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(38, 0); "- "; RC$(38, 0); ", "; RC$(38, 1) + " " + RC$(38, 2) + " " + RC$(38, 3) + ", " + RC$(38, 4); HALF$(allRecords(38, 2))
End If
Print #2,
Print #2, "MOST DEFENSIVE REBOUNDS, ONE QUARTER"
If allRecords(39, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(39, 0); "- "; RC$(39, 0); ", "; RC$(39, 1) + " " + RC$(39, 2) + " " + RC$(39, 3) + ", " + RC$(39, 4); QTR$(allRecords(39, 2))
End If
Print #2,
Print #2, "MOST ASSISTS, GAME"
If allRecords(40, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(40, 0); "- "; RC$(40, 0); ", "; RC$(40, 1) + " " + RC$(40, 2) + " " + RC$(40, 3) + ", " + RC$(40, 4)
End If
Print #2,
Print #2, "MOST ASSISTS, ONE HALF"
If allRecords(41, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(41, 0); "- "; RC$(41, 0); ", "; RC$(41, 1) + " " + RC$(41, 2) + " " + RC$(41, 3) + ", " + RC$(41, 4); HALF$(allRecords(41, 2))
End If
Print #2,
Print #2, "MOST ASSISTS, ONE QUARTER"
If allRecords(42, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(42, 0); "- "; RC$(42, 0); ", "; RC$(42, 1) + " " + RC$(42, 2) + " " + RC$(42, 3) + ", " + RC$(42, 4); QTR$(allRecords(42, 2))
End If
Print #2,
Print #2, "MOST STEALS, GAME"
If allRecords(43, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(43, 0); "- "; RC$(43, 0); ", "; RC$(43, 1) + " " + RC$(43, 2) + " " + RC$(43, 3) + ", " + RC$(43, 4)
End If
Print #2,
Print #2, "MOST STEALS, ONE HALF"
If allRecords(44, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(44, 0); "- "; RC$(44, 0); ", "; RC$(44, 1) + " " + RC$(44, 2) + " " + RC$(44, 3) + ", " + RC$(44, 4); HALF$(allRecords(44, 2))
End If
Print #2,
Print #2, "MOST STEALS, ONE QUARTER"
If allRecords(45, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(45, 0); "- "; RC$(45, 0); ", "; RC$(45, 1) + " " + RC$(45, 2) + " " + RC$(45, 3) + ", " + RC$(45, 4); QTR$(allRecords(45, 2))
End If
Print #2,
Print #2, "MOST BLOCKS, GAME"
If allRecords(46, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(46, 0); "- "; RC$(46, 0); ", "; RC$(46, 1) + " " + RC$(46, 2) + " " + RC$(46, 3) + ", " + RC$(46, 4)
End If
Print #2,
Print #2, "MOST BLOCKS, ONE HALF"
If allRecords(47, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, allRecords(47, 0); "- "; RC$(47, 0); ", "; RC$(47, 1) + " " + RC$(47, 2) + " " + RC$(47, 3) + ", " + RC$(47, 4); HALF$(allRecords(47, 2))
End If
Print #2,
Print #2, "MOST BLOCKS, ONE QUARTER"
If allRecords(48, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME";
Else
    Print #2, allRecords(48, 0); "- "; RC$(48, 0); ", "; RC$(48, 1) + " " + RC$(48, 2) + " " + RC$(48, 3) + ", " + RC$(48, 4); QTR$(allRecords(48, 2));
End If

Print #2, Chr$(12)

Print #2, "                            TEAM RECORDS"
Print #2,
Print #2, "MOST OVERTIME PERIODS"
If TotalRecords(0, 0) = 0 Then
    Print #2, "NO RECORD AT THIS TIME"
Else
    Print #2, TotalRecords(0, 0); "- "; TRC$(0, 1);
    Print #2, Using " (###) AT "; TotalRecords(0, 1);
    Print #2, TRC$(0, 2);: Print #2, Using " (###)"; TotalRecords(0, 2);: Print #2, ", " + TRC$(0, 3)
End If
Print #2,
Print #2, "MOST POINTS, GAME"
Print #2, TotalRecords(1, 0); "- "; TRC$(1, 0) + " " + TRC$(1, 1) + " " + TRC$(1, 2) + ", " + TRC$(1, 3)
Print #2,
Print #2, "FEWEST POINTS, GAME"
Print #2, TotalRecords(2, 0); "- "; TRC$(2, 0) + " " + TRC$(2, 1) + " " + TRC$(2, 2) + ", " + TRC$(2, 3)
Print #2,
Print #2, "MOST POINTS, BOTH TEAMS, GAME"
Print #2, TotalRecords(3, 0); "- "; TRC$(3, 0);
Print #2, Using " (###) "; TotalRecords(3, 1);: Print #2, TRC$(3, 1) + " ";
Print #2, TRC$(3, 2);: Print #2, Using " (###)"; TotalRecords(3, 2);: Print #2, ", " + TRC$(3, 3)
Print #2,
Print #2, "FEWEST POINTS, BOTH TEAMS, GAME"
Print #2, TotalRecords(4, 0); "- "; TRC$(4, 0);
Print #2, Using " (###) "; TotalRecords(4, 1);: Print #2, TRC$(4, 1) + " ";
Print #2, TRC$(4, 2);: Print #2, Using " (###)"; TotalRecords(4, 2);: Print #2, ", " + TRC$(4, 3)
Print #2,
Print #2, "MOST POINTS, FIRST HALF"
Print #2, TotalRecords(6, 0); "- "; TRC$(6, 0) + " " + TRC$(6, 1) + " " + TRC$(6, 2) + ", " + TRC$(6, 3)
Print #2,
Print #2, "FEWEST POINTS, FIRST HALF"
Print #2, TotalRecords(7, 0); "- "; TRC$(7, 0) + " " + TRC$(7, 1) + " " + TRC$(7, 2) + ", " + TRC$(7, 3)
Print #2,
    
Print #2, "MOST POINTS, BOTH TEAMS, FIRST HALF"
Print #2, TotalRecords(8, 0); "- "; TRC$(8, 0);
Print #2, Using " (###) "; TotalRecords(8, 1);: Print #2, TRC$(8, 1) + " ";
Print #2, TRC$(8, 2);: Print #2, Using " (###)"; TotalRecords(8, 2);: Print #2, ", " + TRC$(8, 3)
Print #2,
Print #2, "FEWEST POINTS, BOTH TEAMS, FIRST HALF"
Print #2, TotalRecords(9, 0); "- "; TRC$(9, 0);
Print #2, Using " (###) "; TotalRecords(9, 1);: Print #2, TRC$(9, 1) + " ";
Print #2, TRC$(9, 2);: Print #2, Using " (###)"; TotalRecords(9, 2);: Print #2, ", " + TRC$(9, 3)
Print #2,
Print #2, "MOST POINTS, SECOND HALF"
Print #2, TotalRecords(10, 0); "- "; TRC$(10, 0) + " " + TRC$(10, 1) + " " + TRC$(10, 2) + ", " + TRC$(10, 3)
Print #2,
Print #2, "FEWEST POINTS, SECOND HALF"
Print #2, TotalRecords(11, 0); "- "; TRC$(11, 0) + " " + TRC$(11, 1) + " " + TRC$(11, 2) + ", " + TRC$(11, 3)
Print #2,
Print #2, "MOST POINTS, BOTH TEAMS, SECOND HALF"
Print #2, TotalRecords(12, 0); "- "; TRC$(12, 0);
Print #2, Using " (###) "; TotalRecords(12, 1);: Print #2, TRC$(12, 1) + " ";
Print #2, TRC$(12, 2);: Print #2, Using " (###)"; TotalRecords(12, 2);: Print #2, ", " + TRC$(12, 3)
Print #2,
Print #2, "FEWEST POINTS, BOTH TEAMS, SECOND HALF"
Print #2, TotalRecords(13, 0); "- "; TRC$(13, 0);
Print #2, Using " (###) "; TotalRecords(13, 1);: Print #2, TRC$(13, 1) + " ";
Print #2, TRC$(13, 2);: Print #2, Using " (###)"; TotalRecords(13, 2);: Print #2, ", " + TRC$(13, 3)
Print #2,
    
Print #2, "MOST POINTS, FIRST QUARTER"
Print #2, TotalRecords(14, 0); "- "; TRC$(14, 0) + " " + TRC$(14, 1) + " " + TRC$(14, 2) + ", " + TRC$(14, 3)
Print #2,
Print #2, "FEWEST POINTS, FIRST QUARTER"
Print #2, TotalRecords(15, 0); "- "; TRC$(15, 0) + " " + TRC$(15, 1) + " " + TRC$(15, 2) + ", " + TRC$(15, 3)
Print #2,
Print #2, "MOST POINTS, BOTH TEAMS, FIRST QUARTER"
Print #2, TotalRecords(16, 0); "- "; TRC$(16, 0);
Print #2, Using " (###) "; TotalRecords(16, 1);: Print #2, TRC$(16, 1) + " ";
Print #2, TRC$(16, 2);: Print #2, Using " (###)"; TotalRecords(16, 2);: Print #2, ", " + TRC$(16, 3)
Print #2,
Print #2, "FEWEST POINTS, BOTH TEAMS, FIRST QUARTER"
Print #2, TotalRecords(17, 0); "- "; TRC$(17, 0);
Print #2, Using " (###) "; TotalRecords(17, 1);: Print #2, TRC$(17, 1) + " ";
Print #2, TRC$(17, 2);: Print #2, Using " (###)"; TotalRecords(17, 2);: Print #2, ", " + TRC$(17, 3)
Print #2,
Print #2, "MOST POINTS, SECOND QUARTER"
Print #2, TotalRecords(18, 0); "- "; TRC$(18, 0) + " " + TRC$(18, 1) + " " + TRC$(18, 2) + ", " + TRC$(18, 3)
Print #2,
Print #2, "FEWEST POINTS, SECOND QUARTER"
Print #2, TotalRecords(19, 0); "- "; TRC$(19, 0) + " " + TRC$(19, 1) + " " + TRC$(19, 2) + ", " + TRC$(19, 3)
Print #2,
Print #2, "MOST POINTS, BOTH TEAMS, SECOND QUARTER"
Print #2, TotalRecords(20, 0); "- "; TRC$(20, 0);
Print #2, Using " (###) "; TotalRecords(20, 1);: Print #2, TRC$(20, 1) + " ";
Print #2, TRC$(20, 2);: Print #2, Using " (###)"; TotalRecords(20, 2);: Print #2, ", " + TRC$(20, 3)
Print #2,
Print #2, "FEWEST POINTS, BOTH TEAMS, SECOND QUARTER"
Print #2, TotalRecords(21, 0); "- "; TRC$(21, 0);
Print #2, Using " (###) "; TotalRecords(21, 1);: Print #2, TRC$(21, 1) + " ";
Print #2, TRC$(21, 2);: Print #2, Using " (###)"; TotalRecords(21, 2);: Print #2, ", " + TRC$(21, 3)
Print #2,
Print #2, "MOST POINTS, THIRD QUARTER"
Print #2, TotalRecords(22, 0); "- "; TRC$(22, 0) + " " + TRC$(22, 1) + " " + TRC$(22, 2) + ", " + TRC$(22, 3)
Print #2,
Print #2, "FEWEST POINTS, THIRD QUARTER"
Print #2, TotalRecords(23, 0); "- "; TRC$(23, 0) + " " + TRC$(23, 1) + " " + TRC$(23, 2) + ", " + TRC$(23, 3)
Print #2,
Print #2, "MOST POINTS, BOTH TEAMS, THIRD QUARTER"
Print #2, TotalRecords(24, 0); "- "; TRC$(24, 0);
Print #2, Using " (###) "; TotalRecords(24, 1);: Print #2, TRC$(24, 1) + " ";
Print #2, TRC$(24, 2);: Print #2, Using " (###)"; TotalRecords(24, 2);: Print #2, ", " + TRC$(24, 3)
Print #2,
Print #2, "FEWEST POINTS, BOTH TEAMS, THIRD QUARTER"
Print #2, TotalRecords(25, 0); "- "; TRC$(25, 0);
Print #2, Using " (###) "; TotalRecords(25, 1);: Print #2, TRC$(25, 1) + " ";
Print #2, TRC$(25, 2);: Print #2, Using " (###)"; TotalRecords(25, 2);: Print #2, ", " + TRC$(25, 3)
Print #2,
Print #2, "MOST POINTS, FOURTH QUARTER"
Print #2, TotalRecords(26, 0); "- "; TRC$(26, 0) + " " + TRC$(26, 1) + " " + TRC$(26, 2) + ", " + TRC$(26, 3)
Print #2,
Print #2, "FEWEST POINTS, FOURTH QUARTER"
Print #2, TotalRecords(27, 0); "- "; TRC$(27, 0) + " " + TRC$(27, 1) + " " + TRC$(27, 2) + ", " + TRC$(27, 3)
Print #2,
Print #2, "MOST POINTS, BOTH TEAMS, FOURTH QUARTER"
Print #2, TotalRecords(28, 0); "- "; TRC$(28, 0);
Print #2, Using " (###) "; TotalRecords(28, 1);: Print #2, TRC$(28, 1) + " ";
Print #2, TRC$(28, 2);: Print #2, Using " (###)"; TotalRecords(28, 2);: Print #2, ", " + TRC$(28, 3)
Print #2,
Print #2, "FEWEST POINTS, BOTH TEAMS, FOURTH QUARTER"
Print #2, TotalRecords(29, 0); "- "; TRC$(29, 0);
Print #2, Using " (###) "; TotalRecords(29, 1);: Print #2, TRC$(29, 1) + " ";
Print #2, TRC$(29, 2);: Print #2, Using " (###)"; TotalRecords(29, 2);: Print #2, ", " + TRC$(29, 3)
Print #2,
Print #2, "HIGHEST FIELD GOAL PERCENTAGE, GAME"
Print #2, Using ".### "; TotalRecords(30, 0);: Print #2, "- "; TRC$(30, 0) + " " + TRC$(30, 1) + " " + TRC$(30, 2) + ", " + TRC$(30, 3);: Print #2, Using "###/###"; TotalRecords(30, 1); TotalRecords(30, 2)
Print #2,
Print #2, "LOWEST FIELD GOAL PERCENTAGE, GAME"
Print #2, Using ".### "; TotalRecords(31, 0);: Print #2, "- "; TRC$(31, 0) + " " + TRC$(31, 1) + " " + TRC$(31, 2) + ", " + TRC$(31, 3);: Print #2, Using "###/###"; TotalRecords(31, 1); TotalRecords(31, 2)
PRINT #2,
    PRINT #2, "HIGHEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
PRINT #2, USING ".### "; TotalRecords(32, 0); : PRINT #2, "- "; TRC$(32, 0);
PRINT #2, USING " (.###) "; TotalRecords(32, 1); : PRINT #2, TRC$(32, 1) + " ";
PRINT #2, TRC$(32, 2); : PRINT #2, USING " (.###)"; TotalRecords(32, 2); : PRINT #2, ", " + TRC$(32, 3)
PRINT #2,
    PRINT #2, "LOWEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
PRINT #2, USING ".### "; TotalRecords(33, 0); : PRINT #2, "- "; TRC$(33, 0);
PRINT #2, USING " (.###) "; TotalRecords(33, 1); : PRINT #2, TRC$(33, 1) + " ";
PRINT #2, TRC$(33, 2); : PRINT #2, USING " (.###)"; TotalRecords(33, 2); : PRINT #2, ", " + TRC$(33, 3)
PRINT #2,
    PRINT #2, "MOST FIELD GOALS, GAME"
PRINT #2, TotalRecords(34, 0); "- "; TRC$(34, 0) + " " + TRC$(34, 1) + " " + TRC$(34, 2) + ", " + TRC$(34, 3)
PRINT #2,
    PRINT #2, "FEWEST FIELD GOALS, GAME"
PRINT #2, TotalRecords(35, 0); "- "; TRC$(35, 0) + " " + TRC$(35, 1) + " " + TRC$(35, 2) + ", " + TRC$(35, 3)
PRINT #2,
    PRINT #2, "MOST FIELD GOALS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(36, 0); "- "; TRC$(36, 0);
PRINT #2, USING " (###) "; TotalRecords(36, 1); : PRINT #2, TRC$(36, 1) + " ";
PRINT #2, TRC$(36, 2); : PRINT #2, USING " (###)"; TotalRecords(36, 2); : PRINT #2, ", " + TRC$(36, 3)
PRINT #2,
    PRINT #2, "FEWEST FIELD GOALS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(37, 0); "- "; TRC$(37, 0);
PRINT #2, USING " (###) "; TotalRecords(37, 1); : PRINT #2, TRC$(37, 1) + " ";
PRINT #2, TRC$(37, 2); : PRINT #2, USING " (###)"; TotalRecords(37, 2); : PRINT #2, ", " + TRC$(37, 3)
    PRINT #2,
    PRINT #2, "MOST FIELD GOALS, ONE HALF"
PRINT #2, TotalRecords(38, 0); "- "; TRC$(38, 0) + " " + TRC$(38, 1) + " " + TRC$(38, 2) + ", " + TRC$(38, 3); HALF$(TotalRecords(38, 1))
PRINT #2,
    PRINT #2, "MOST FIELD GOALS, BOTH TEAMS, ONE HALF"
PRINT #2, TotalRecords(39, 0); "- "; TRC$(39, 0); " "; TRC$(39, 1); " "; TRC$(39, 2); ", "; TRC$(39, 3); HALF$(TotalRecords(39, 1))
PRINT #2,
    PRINT #2, "MOST FIELD GOALS, ONE QUARTER"
PRINT #2, TotalRecords(40, 0); "- "; TRC$(40, 0) + " " + TRC$(40, 1) + " " + TRC$(40, 2) + ", " + TRC$(40, 3); QTR$(TotalRecords(40, 1))
PRINT #2,
    PRINT #2, "MOST FIELD GOALS, BOTH TEAMS, ONE QUARTER"
PRINT #2, TotalRecords(41, 0); "- "; TRC$(41, 0); " "; TRC$(41, 1); " "; TRC$(41, 2); ", "; TRC$(41, 3); QTR$(TotalRecords(41, 1))
PRINT #2,
    PRINT #2, "MOST FIELD GOAL ATTEMPTS, GAME"
PRINT #2, TotalRecords(42, 0); "- "; TRC$(42, 0) + " " + TRC$(42, 1) + " " + TRC$(42, 2) + ", " + TRC$(42, 3)
PRINT #2,
    PRINT #2, "FEWEST FIELD GOAL ATTEMPTS, GAME"
PRINT #2, TotalRecords(43, 0); "- "; TRC$(43, 0) + " " + TRC$(43, 1) + " " + TRC$(43, 2) + ", " + TRC$(43, 3)
PRINT #2,
    PRINT #2, "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(44, 0); "- "; TRC$(44, 0);
PRINT #2, USING " (###) "; TotalRecords(44, 1); : PRINT #2, TRC$(44, 1) + " ";
PRINT #2, TRC$(44, 2); : PRINT #2, USING " (###)"; TotalRecords(44, 2); : PRINT #2, ", " + TRC$(44, 3)
PRINT #2,
    PRINT #2, "FEWEST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(45, 0); "- "; TRC$(45, 0);
PRINT #2, USING " (###) "; TotalRecords(45, 1); : PRINT #2, TRC$(45, 1) + " ";
PRINT #2, TRC$(45, 2); : PRINT #2, USING " (###)"; TotalRecords(45, 2); : PRINT #2, ", " + TRC$(45, 3)
    PRINT #2,
    PRINT #2, "MOST FIELD GOAL ATTEMPTS, ONE HALF"
PRINT #2, TotalRecords(46, 0); "- "; TRC$(46, 0) + " " + TRC$(46, 1) + " " + TRC$(46, 2) + ", " + TRC$(46, 3); HALF$(TotalRecords(46, 1))
PRINT #2,
    PRINT #2, "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE HALF"
PRINT #2, TotalRecords(47, 0); "- "; TRC$(47, 0); " "; TRC$(47, 1); " "; TRC$(47, 2); ", "; TRC$(47, 3); HALF$(TotalRecords(47, 1))
PRINT #2,
    PRINT #2, "MOST FIELD GOAL ATTEMPTS, ONE QUARTER"
PRINT #2, TotalRecords(48, 0); "- "; TRC$(48, 0) + " " + TRC$(48, 1) + " " + TRC$(48, 2) + ", " + TRC$(48, 3); QTR$(TotalRecords(48, 1))
PRINT #2,
    PRINT #2, "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE QUARTER"
PRINT #2, TotalRecords(49, 0); "- "; TRC$(49, 0); " "; TRC$(49, 1); " "; TRC$(49, 2); ", "; TRC$(49, 3); QTR$(TotalRecords(49, 1))
PRINT #2,
    PRINT #2, "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
IF TotalRecords(50, 0) = 0 THEN
PRINT #2, "NO RECORD AT THIS TIME"
ELSE
PRINT #2, TotalRecords(50, 0); "- "; TRC$(50, 0) + " " + TRC$(50, 1) + " " + TRC$(50, 2) + ", " + TRC$(50, 3)
END IF
PRINT #2,
    PRINT #2, "MOST THREE-POINT FIELD GOALS, NO MISSES, BOTH TEAMS, GAME"
IF TotalRecords(51, 0) = 0 THEN
PRINT #2, "NO RECORD AT THIS TIME"
ELSE
PRINT #2, TotalRecords(51, 0); "- "; TRC$(51, 0);
PRINT #2, USING " (###) "; TotalRecords(51, 1); : PRINT #2, TRC$(51, 1) + " ";
PRINT #2, TRC$(51, 2); : PRINT #2, USING " (###)"; TotalRecords(51, 2); : PRINT #2, ", " + TRC$(51, 3)
END IF
PRINT #2,
    PRINT #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
IF TotalRecords(52, 0) = 0 THEN
PRINT #2, "NO RECORD AT THIS TIME"
ELSE
PRINT #2, TotalRecords(52, 0); "- "; TRC$(52, 0) + " " + TRC$(52, 1) + " " + TRC$(52, 2) + ", " + TRC$(52, 3)
END IF
PRINT #2,
    PRINT #2,
    PRINT #2, "MOST THREE-POINT FIELD GOALS, GAME"
PRINT #2, TotalRecords(53, 0); "- "; TRC$(53, 0) + " " + TRC$(53, 1) + " " + TRC$(53, 2) + ", " + TRC$(53, 3)
PRINT #2,
    PRINT #2, "MOST THREE-POINT FIELD GOALS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(54, 0); "- "; TRC$(54, 0);
PRINT #2, USING " (###) "; TotalRecords(54, 1); : PRINT #2, TRC$(54, 1) + " ";
PRINT #2, TRC$(54, 2); : PRINT #2, USING " (###)"; TotalRecords(54, 2); : PRINT #2, ", " + TRC$(54, 3)
PRINT #2,
    PRINT #2, "MOST THREE-POINT FIELD GOALS, ONE HALF"
PRINT #2, TotalRecords(55, 0); "- "; TRC$(55, 0) + " " + TRC$(55, 1) + " " + TRC$(55, 2) + ", " + TRC$(55, 3); HALF$(TotalRecords(55, 1))
PRINT #2,
    PRINT #2, "MOST THREE-POINT FIELD GOALS, ONE QUARTER"
PRINT #2, TotalRecords(56, 0); "- "; TRC$(56, 0) + " " + TRC$(56, 1) + " " + TRC$(56, 2) + ", " + TRC$(56, 3); QTR$(TotalRecords(56, 1))
PRINT #2,
    PRINT #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
PRINT #2, TotalRecords(57, 0); "- "; TRC$(57, 0) + " " + TRC$(57, 1) + " " + TRC$(57, 2) + ", " + TRC$(57, 3)
PRINT #2,
    PRINT #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(58, 0); "- "; TRC$(58, 0);
PRINT #2, USING " (###) "; TotalRecords(58, 1); : PRINT #2, TRC$(58, 1) + " ";
PRINT #2, TRC$(58, 2); : PRINT #2, USING " (###)"; TotalRecords(58, 2); : PRINT #2, ", " + TRC$(58, 3)
PRINT #2,
    PRINT #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
PRINT #2, TotalRecords(59, 0); "- "; TRC$(59, 0) + " " + TRC$(59, 1) + " " + TRC$(59, 2) + ", " + TRC$(59, 3); HALF$(TotalRecords(59, 1))
    PRINT #2,
    PRINT #2, "MOST FREE THROWS MADE, NO MISSES, GAME"
IF TotalRecords(60, 0) = 0 THEN
PRINT #2, "NO RECORD AT THIS TIME"
ELSE
PRINT #2, TotalRecords(60, 0); "- "; TRC$(60, 0) + " " + TRC$(60, 1) + " " + TRC$(60, 2) + ", " + TRC$(60, 3)
END IF
PRINT #2,
    PRINT #2, "LOWEST FREE THROW PERCENTAGE, GAME"
PRINT #2, USING ".### "; TotalRecords(61, 0); : PRINT #2, "- "; TRC$(61, 0) + " " + TRC$(61, 1) + " " + TRC$(61, 2) + ", " + TRC$(61, 3); : PRINT #2, USING "###/###"; TotalRecords(61, 1); TotalRecords(61, 2)
PRINT #2,
    PRINT #2, "HIGHEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
PRINT #2, USING ".### "; TotalRecords(62, 0); : PRINT #2, "- "; TRC$(62, 0);
PRINT #2, USING " (.###) "; TotalRecords(62, 1); : PRINT #2, TRC$(62, 1) + " ";
PRINT #2, TRC$(62, 2); : PRINT #2, USING " (.###)"; TotalRecords(62, 2); : PRINT #2, ", " + TRC$(62, 3)
PRINT #2,
    PRINT #2, "LOWEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
PRINT #2, USING ".### "; TotalRecords(63, 0); : PRINT #2, "- "; TRC$(63, 0);
PRINT #2, USING " (.###) "; TotalRecords(63, 1); : PRINT #2, TRC$(63, 1) + " ";
PRINT #2, TRC$(63, 2); : PRINT #2, USING " (.###)"; TotalRecords(63, 2); : PRINT #2, ", " + TRC$(63, 3)
PRINT #2,
    PRINT #2, "MOST FREE THROWS MADE, GAME"
PRINT #2, TotalRecords(64, 0); "- "; TRC$(64, 0) + " " + TRC$(64, 1) + " " + TRC$(64, 2) + ", " + TRC$(64, 3)
PRINT #2,
    PRINT #2, "FEWEST FREE THROWS MADE, GAME"
PRINT #2, TotalRecords(65, 0); "- "; TRC$(65, 0) + " " + TRC$(65, 1) + " " + TRC$(65, 2) + ", " + TRC$(65, 3)
PRINT #2,
    PRINT #2, "MOST FREE THROWS MADE, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(66, 0); "- "; TRC$(66, 0);
PRINT #2, USING " (###) "; TotalRecords(66, 1); : PRINT #2, TRC$(66, 1) + " ";
PRINT #2, TRC$(66, 2); : PRINT #2, USING " (###)"; TotalRecords(66, 2); : PRINT #2, ", " + TRC$(66, 3)
PRINT #2,
    PRINT #2, "FEWEST FREE THROWS MADE, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(67, 0); "- "; TRC$(67, 0);
PRINT #2, USING " (###) "; TotalRecords(67, 1); : PRINT #2, TRC$(67, 1) + " ";
PRINT #2, TRC$(67, 2); : PRINT #2, USING " (###)"; TotalRecords(67, 2); : PRINT #2, ", " + TRC$(67, 3)
    PRINT #2,
    PRINT #2, "MOST FREE THROWS MADE, ONE HALF"
PRINT #2, TotalRecords(68, 0); "- "; TRC$(68, 0) + " " + TRC$(68, 1) + " " + TRC$(68, 2) + ", " + TRC$(68, 3); HALF$(TotalRecords(68, 1))
PRINT #2,
    PRINT #2, "MOST FREE THROWS MADE, ONE QUARTER"
PRINT #2, TotalRecords(70, 0); "- "; TRC$(70, 0) + " " + TRC$(70, 1) + " " + TRC$(70, 2) + ", " + TRC$(70, 3); QTR$(TotalRecords(70, 1))
PRINT #2,
    PRINT #2, "MOST FREE THROW ATTEMPTS, GAME"
PRINT #2, TotalRecords(72, 0); "- "; TRC$(72, 0) + " " + TRC$(72, 1) + " " + TRC$(72, 2) + ", " + TRC$(72, 3)
PRINT #2,
    PRINT #2, "FEWEST FREE THROW ATTEMPTS, GAME"
PRINT #2, TotalRecords(73, 0); "- "; TRC$(73, 0) + " " + TRC$(73, 1) + " " + TRC$(73, 2) + ", " + TRC$(73, 3)
PRINT #2,
    PRINT #2, "MOST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(74, 0); "- "; TRC$(74, 0);
PRINT #2, USING " (###) "; TotalRecords(74, 1); : PRINT #2, TRC$(74, 1) + " ";
PRINT #2, TRC$(74, 2); : PRINT #2, USING " (###)"; TotalRecords(74, 2); : PRINT #2, ", " + TRC$(74, 3)
PRINT #2,
    PRINT #2, "FEWEST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(75, 0); "- "; TRC$(75, 0);
PRINT #2, USING " (###) "; TotalRecords(75, 1); : PRINT #2, TRC$(75, 1) + " ";
PRINT #2, TRC$(75, 2); : PRINT #2, USING " (###)"; TotalRecords(75, 2); : PRINT #2, ", " + TRC$(75, 3)
PRINT #2,
    PRINT #2, "MOST FREE THROW ATTEMPTS, ONE HALF"
PRINT #2, TotalRecords(76, 0); "- "; TRC$(76, 0) + " " + TRC$(76, 1) + " " + TRC$(76, 2) + ", " + TRC$(76, 3); HALF$(TotalRecords(76, 1))
PRINT #2,
    PRINT #2, "MOST FREE THROW ATTEMPTS, ONE QUARTER"
PRINT #2, TotalRecords(78, 0); "- "; TRC$(78, 0) + " " + TRC$(78, 1) + " " + TRC$(78, 2) + ", " + TRC$(78, 3); QTR$(TotalRecords(78, 1))
    PRINT #2,
PRINT #2, "MOST REBOUNDS, GAME"
PRINT #2, TotalRecords(80, 0); "- "; TRC$(80, 0) + " " + TRC$(80, 1) + " " + TRC$(80, 2) + ", " + TRC$(80, 3)
PRINT #2,
    PRINT #2, "FEWEST REBOUNDS, GAME"
PRINT #2, TotalRecords(81, 0); "- "; TRC$(81, 0) + " " + TRC$(81, 1) + " " + TRC$(81, 2) + ", " + TRC$(81, 3)
PRINT #2,
    PRINT #2, "MOST REBOUNDS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(82, 0); "- "; TRC$(82, 0);
PRINT #2, USING " (###) "; TotalRecords(82, 1); : PRINT #2, TRC$(82, 1) + " ";
PRINT #2, TRC$(82, 2); : PRINT #2, USING " (###)"; TotalRecords(82, 2); : PRINT #2, ", " + TRC$(82, 3)
PRINT #2,
    PRINT #2, "FEWEST REBOUNDS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(83, 0); "- "; TRC$(83, 0);
PRINT #2, USING " (###) "; TotalRecords(83, 1); : PRINT #2, TRC$(83, 1) + " ";
PRINT #2, TRC$(83, 2); : PRINT #2, USING " (###)"; TotalRecords(83, 2); : PRINT #2, ", " + TRC$(83, 3)
PRINT #2,
    PRINT #2, "MOST REBOUNDS, ONE HALF"
PRINT #2, TotalRecords(84, 0); "- "; TRC$(84, 0) + " " + TRC$(84, 1) + " " + TRC$(84, 2) + ", " + TRC$(84, 3); HALF$(TotalRecords(84, 1))
PRINT #2,
    PRINT #2, "MOST REBOUNDS, ONE QUARTER"
PRINT #2, TotalRecords(85, 0); "- "; TRC$(85, 0) + " " + TRC$(85, 1) + " " + TRC$(85, 2) + ", " + TRC$(85, 3); QTR$(TotalRecords(85, 1))
PRINT #2,
    PRINT #2,
PRINT #2, "MOST OFFENSIVE REBOUNDS, GAME"
PRINT #2, TotalRecords(86, 0); "- "; TRC$(86, 0) + " " + TRC$(86, 1) + " " + TRC$(86, 2) + ", " + TRC$(86, 3)
PRINT #2,
    PRINT #2, "MOST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(88, 0); "- "; TRC$(88, 0);
PRINT #2, USING " (###) "; TotalRecords(88, 1); : PRINT #2, TRC$(88, 1) + " ";
PRINT #2, TRC$(88, 2); : PRINT #2, USING " (###)"; TotalRecords(88, 2); : PRINT #2, ", " + TRC$(88, 3)
PRINT #2,
    PRINT #2, "FEWEST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(89, 0); "- "; TRC$(89, 0);
PRINT #2, USING " (###) "; TotalRecords(89, 1); : PRINT #2, TRC$(89, 1) + " ";
PRINT #2, TRC$(89, 2); : PRINT #2, USING " (###)"; TotalRecords(89, 2); : PRINT #2, ", " + TRC$(89, 3)
PRINT #2,
    PRINT #2, "MOST OFFENSIVE REBOUNDS, ONE HALF"
PRINT #2, TotalRecords(90, 0); "- "; TRC$(90, 0) + " " + TRC$(90, 1) + " " + TRC$(90, 2) + ", " + TRC$(90, 3); HALF$(TotalRecords(90, 1))
PRINT #2,
    PRINT #2, "MOST ASSISTS, GAME"
PRINT #2, TotalRecords(96, 0); "- "; TRC$(96, 0) + " " + TRC$(96, 1) + " " + TRC$(96, 2) + ", " + TRC$(96, 3)
PRINT #2,
    PRINT #2, "FEWEST ASSISTS, GAME"
PRINT #2, TotalRecords(97, 0); "- "; TRC$(97, 0) + " " + TRC$(97, 1) + " " + TRC$(97, 2) + ", " + TRC$(97, 3)
PRINT #2,
    PRINT #2, "MOST ASSISTS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(98, 0); "- "; TRC$(98, 0);
PRINT #2, USING " (###) "; TotalRecords(98, 1); : PRINT #2, TRC$(98, 1) + " ";
PRINT #2, TRC$(98, 2); : PRINT #2, USING " (###)"; TotalRecords(98, 2); : PRINT #2, ", " + TRC$(98, 3)
PRINT #2,
    PRINT #2, "FEWEST ASSISTS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(99, 0); "- "; TRC$(99, 0);
PRINT #2, USING " (###) "; TotalRecords(99, 1); : PRINT #2, TRC$(99, 1) + " ";
PRINT #2, TRC$(99, 2); : PRINT #2, USING " (###)"; TotalRecords(99, 2); : PRINT #2, ", " + TRC$(99, 3)
PRINT #2,
    PRINT #2, "MOST ASSISTS, ONE HALF"
PRINT #2, TotalRecords(100, 0); "- "; TRC$(100, 0) + " " + TRC$(100, 1) + " " + TRC$(100, 2) + ", " + TRC$(100, 3); HALF$(TotalRecords(100, 1))
PRINT #2,
    PRINT #2, "MOST ASSISTS, ONE QUARTER"
PRINT #2, TotalRecords(102, 0); "- "; TRC$(102, 0) + " " + TRC$(102, 1) + " " + TRC$(102, 2) + ", " + TRC$(102, 3); QTR$(TotalRecords(102, 1))
PRINT #2,
    PRINT #2, "MOST PERSONAL FOULS, GAME"
PRINT #2, TotalRecords(104, 0); "- "; TRC$(104, 0) + " " + TRC$(104, 1) + " " + TRC$(104, 2) + ", " + TRC$(104, 3)
PRINT #2,
    PRINT #2, "FEWEST PERSONAL FOULS, GAME"
PRINT #2, TotalRecords(105, 0); "- "; TRC$(105, 0) + " " + TRC$(105, 1) + " " + TRC$(105, 2) + ", " + TRC$(105, 3)
PRINT #2,
    PRINT #2, "MOST PERSONAL FOULS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(106, 0); "- "; TRC$(106, 0);
PRINT #2, USING " (###) "; TotalRecords(106, 1); : PRINT #2, TRC$(106, 1) + " ";
PRINT #2, TRC$(106, 2); : PRINT #2, USING " (###)"; TotalRecords(106, 2); : PRINT #2, ", " + TRC$(106, 3)
PRINT #2,
    PRINT #2, "FEWEST PERSONAL FOULS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(107, 0); "- "; TRC$(107, 0);
PRINT #2, USING " (###) "; TotalRecords(107, 1); : PRINT #2, TRC$(107, 1) + " ";
PRINT #2, TRC$(107, 2); : PRINT #2, USING " (###)"; TotalRecords(107, 2); : PRINT #2, ", " + TRC$(107, 3)
PRINT #2,
    PRINT #2, "MOST PERSONAL FOULS, ONE HALF"
PRINT #2, TotalRecords(108, 0); "- "; TRC$(108, 0) + " " + TRC$(108, 1) + " " + TRC$(108, 2) + ", " + TRC$(108, 3); HALF$(TotalRecords(108, 1))
PRINT #2,
    PRINT #2, "MOST PERSONAL FOULS, BOTH TEAMS, ONE HALF"
PRINT #2, TotalRecords(109, 0); "- "; TRC$(109, 0); " "; TRC$(109, 1); " "; TRC$(109, 2); ", "; TRC$(109, 3); HALF$(TotalRecords(109, 1))
PRINT #2,
    PRINT #2, "MOST PERSONAL FOULS, ONE QUARTER"
PRINT #2, TotalRecords(110, 0); "- "; TRC$(110, 0) + " " + TRC$(110, 1) + " " + TRC$(110, 2) + ", " + TRC$(110, 3); QTR$(TotalRecords(110, 1))
PRINT #2,
    PRINT #2, "MOST PERSONAL FOULS, BOTH TEAMS, ONE QUARTER"
PRINT #2, TotalRecords(111, 0); "- "; TRC$(111, 0); " "; TRC$(111, 1); " "; TRC$(111, 2); ", "; TRC$(111, 3); QTR$(TotalRecords(111, 1));
    
PRINT #2,
PRINT #2, "MOST STEALS, GAME"
PRINT #2, TotalRecords(112, 0); "- "; TRC$(112, 0) + " " + TRC$(112, 1) + " " + TRC$(112, 2) + ", " + TRC$(112, 3)
PRINT #2,
    PRINT #2, "MOST STEALS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(114, 0); "- "; TRC$(114, 0);
PRINT #2, USING " (###) "; TotalRecords(114, 1); : PRINT #2, TRC$(114, 1) + " ";
PRINT #2, TRC$(114, 2); : PRINT #2, USING " (###)"; TotalRecords(114, 2); : PRINT #2, ", " + TRC$(114, 3)
PRINT #2,
    PRINT #2, "MOST STEALS, ONE HALF"
PRINT #2, TotalRecords(116, 0); "- "; TRC$(116, 0) + " " + TRC$(116, 1) + " " + TRC$(116, 2) + ", " + TRC$(116, 3); HALF$(TotalRecords(116, 1))
PRINT #2,
    PRINT #2, "MOST STEALS, ONE QUARTER"
PRINT #2, TotalRecords(117, 0); "- "; TRC$(117, 0) + " " + TRC$(117, 1) + " " + TRC$(117, 2) + ", " + TRC$(117, 3); QTR$(TotalRecords(117, 1))
PRINT #2,
    PRINT #2, "MOST BLOCKS, GAME"
PRINT #2, TotalRecords(118, 0); "- "; TRC$(118, 0) + " " + TRC$(118, 1) + " " + TRC$(118, 2) + ", " + TRC$(118, 3)
PRINT #2,
    PRINT #2, "MOST BLOCKS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(119, 0); "- "; TRC$(119, 0);
PRINT #2, USING " (###) "; TotalRecords(119, 1); : PRINT #2, TRC$(119, 1) + " ";
PRINT #2, TRC$(119, 2); : PRINT #2, USING " (###)"; TotalRecords(119, 2); : PRINT #2, ", " + TRC$(119, 3)
PRINT #2,
    PRINT #2, "MOST TURNOVERS, GAME"
PRINT #2, TotalRecords(122, 0); "- "; TRC$(122, 0) + " " + TRC$(122, 1) + " " + TRC$(122, 2) + ", " + TRC$(122, 3)
PRINT #2,
    PRINT #2, "FEWEST TURNOVERS, GAME"
PRINT #2, TotalRecords(123, 0); "- "; TRC$(123, 0) + " " + TRC$(123, 1) + " " + TRC$(123, 2) + ", " + TRC$(123, 3)
PRINT #2,
    PRINT #2, "MOST TURNOVERS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(124, 0); "- "; TRC$(124, 0);
PRINT #2, USING " (###) "; TotalRecords(124, 1); : PRINT #2, TRC$(124, 1) + " ";
PRINT #2, TRC$(124, 2); : PRINT #2, USING " (###)"; TotalRecords(124, 2); : PRINT #2, ", " + TRC$(124, 3)
PRINT #2,
    PRINT #2, "FEWEST TURNOVERS, BOTH TEAMS, GAME"
PRINT #2, TotalRecords(125, 0); "- "; TRC$(125, 0);
PRINT #2, USING " (###) "; TotalRecords(125, 1); : PRINT #2, TRC$(125, 1) + " ";
PRINT #2, TRC$(125, 2); : PRINT #2, USING " (###)"; TotalRecords(125, 2); : PRINT #2, ", " + TRC$(125, 3)
PRINT #2, CHR$(12)
CLOSE 2
      OPEN "RECORDS" FOR INPUT AS #2
    DO WHILE NOT EOF(2)
    LINE INPUT #2, X$
    LPRINT X$
    LOOP
CLOSE 2

300 'Call REC

END SUB
