Sub RECORDS (menuChoice%)

    Shared RC$(), recordsTRC$()
    Shared allRecords!(), TotalRecords!()
    Shared HALF$(), QTR$()

    _Title "Basketball: The Pro Game - View Leaders"

    HALF$(1) = " (1ST HALF)": HALF$(2) = " (2ND HALF)"
    QTR$(1) = " (1ST QTR)"
    QTR$(2) = " (2ND QTR)"
    QTR$(3) = " (3RD QTR)"
    QTR$(4) = " (4TH QTR)"

    Open diskPaths$(1) + "RECORDS.GAM" For Input As #1

    For I = 0 To 48
        For I1 = 0 To 2
            Input #1, allRecords!(I, I1)
        Next
        For I1 = 0 To 4
            Input #1, RC$(I, I1)
        Next
    Next

    For I = 0 To 125
        For I1 = 0 To 2
            Input #1, TotalRecords!(I, I1)
        Next
        For I1 = 0 To 3
            Input #1, recordsTRC$(I, I1)
        Next
    Next
    Close 1

    If menuChoice% = 2 Then

        Call WriteRecords

    Else

        Color 15, 1

        Cls

        Color 12: Print "                            INDIVIDUAL RECORDS"
        Print
        Color 14: Print "MOST POINTS, GAME"
        Color 15
        Print allRecords!(0, 0); "- "; RC$(0, 0); ", "; RC$(0, 1) + " " + RC$(0, 2) + " " + RC$(0, 3) + ", " + RC$(0, 4)
        Print

        Color 14: Print "MOST POINTS, ONE HALF"
        Color 15
        Print allRecords!(1, 0); "- "; RC$(1, 0); ", "; RC$(1, 1) + " " + RC$(1, 2) + " " + RC$(1, 3) + ", " + RC$(1, 4); HALF$(allRecords!(1, 2))
        Print

        Color 14: Print "MOST POINTS, ONE QUARTER"
        Color 15
        Print allRecords!(2, 0); "- "; RC$(2, 0); ", "; RC$(2, 1) + " " + RC$(2, 2) + " " + RC$(2, 3) + ", " + RC$(2, 4); QTR$(allRecords!(2, 2))
        Print

        Color 14: Print "MOST POINTS, OVERTIME PERIOD"
        Color 15
        If allRecords!(3, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(3, 0); "- "; RC$(3, 0); ", "; RC$(3, 1) + " " + RC$(3, 2) + " " + RC$(3, 3) + ", " + RC$(3, 4)
        End If
        Print

        Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, GAME (MINIMUM 15 FIELD GOALS)"
        Color 15

        If allRecords!(4, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            If allRecords!(4, 0) = 1 Then
                Print "1.000 ";
            Else
                Print Using " .### "; allRecords!(4, 0);
            End If
            Print "- "; RC$(4, 0); ", "; RC$(4, 1) + " " + RC$(4, 2) + " " + RC$(4, 3) + ", " + RC$(4, 4);
            Print Using " (##/##)"; allRecords!(4, 1); allRecords!(4, 2)
        End If

        Print

        Color 14
        Print "MOST FIELD GOALS, NO MISSES, GAME"
        Color 15

        If allRecords!(5, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(5, 0); "- "; RC$(5, 0); ", "; RC$(5, 1) + " " + RC$(5, 2) + " " + RC$(5, 3) + ", " + RC$(5, 4)
        End If

        Print

        Color 14
        Print "MOST FIELD GOAL ATTEMPTS, NONE MADE, GAME"
        Color 15

        If allRecords!(6, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(6, 0); "- "; RC$(6, 0); ", "; RC$(6, 1) + " " + RC$(6, 2) + " " + RC$(6, 3) + ", " + RC$(6, 4)
        End If
        I$ = GetKeyPress$

        Cls
        Color 12: Print "                            INDIVIDUAL RECORDS"
        Print
        Color 14: Print "MOST FIELD GOALS, GAME"
        Color 15
        Print allRecords!(7, 0); "- "; RC$(7, 0); ", "; RC$(7, 1) + " " + RC$(7, 2) + " " + RC$(7, 3) + ", " + RC$(7, 4)
        Print

        Color 14: Print "MOST FIELD GOALS, ONE HALF"
        Color 15
        Print allRecords!(8, 0); "- "; RC$(8, 0); ", "; RC$(8, 1) + " " + RC$(8, 2) + " " + RC$(8, 3) + ", " + RC$(8, 4); HALF$(allRecords!(8, 2))
        Print

        Color 14: Print "MOST FIELD GOALS, ONE QUARTER"
        Color 15
        Print allRecords!(9, 0); "- "; RC$(9, 0); ", "; RC$(9, 1) + " " + RC$(9, 2) + " " + RC$(9, 3) + ", " + RC$(9, 4); QTR$(allRecords!(9, 2))
        Print

        Color 14: Print "MOST FIELD GOALS, OVERTIME"
        Color 15
        If allRecords!(10, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(10, 0); "- "; RC$(10, 0); ", "; RC$(10, 1) + " " + RC$(10, 2) + " " + RC$(10, 3) + ", " + RC$(10, 4)
        End If
        Print

        Color 14: Print "MOST FIELD GOAL ATTEMPTS, GAME"
        Color 15
        Print allRecords!(11, 0); "- "; RC$(11, 0); ", "; RC$(11, 1) + " " + RC$(11, 2) + " " + RC$(11, 3) + ", " + RC$(11, 4)
        Print

        Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE HALF"
        Color 15
        Print allRecords!(12, 0); "- "; RC$(12, 0); ", "; RC$(12, 1) + " " + RC$(12, 2) + " " + RC$(12, 3) + ", " + RC$(12, 4); HALF$(allRecords!(12, 2))
        Print

        Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE QUARTER"
        Color 15
        Print allRecords!(13, 0); "- "; RC$(13, 0); ", "; RC$(13, 1) + " " + RC$(13, 2) + " " + RC$(13, 3) + ", " + RC$(13, 4); QTR$(allRecords!(13, 2))
        Print
        I$ = GetKeyPress$

        Cls
        Color 12: Print "                            INDIVIDUAL RECORDS"
        Print
        Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
        Color 15
        If allRecords!(15, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(15, 0); "- "; RC$(15, 0); ", "; RC$(15, 1) + " " + RC$(15, 2) + " " + RC$(15, 3) + ", " + RC$(15, 4)
        End If
        Print

        Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
        Color 15
        If allRecords!(16, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(16, 0); "- "; RC$(16, 0); ", "; RC$(16, 1) + " " + RC$(16, 2) + " " + RC$(16, 3) + ", " + RC$(16, 4)
        End If
        Print

        Color 14: Print "MOST THREE-POINT FIELD GOALS, GAME"
        Color 15
        If allRecords!(17, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(17, 0); "- "; RC$(17, 0); ", "; RC$(17, 1) + " " + RC$(17, 2) + " " + RC$(17, 3) + ", " + RC$(17, 4)
        End If
        Print

        Color 14: Print "MOST THREE-POINT FIELD GOALS MADE, ONE HALF"
        Color 15
        If allRecords!(18, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(18, 0); "- "; RC$(18, 0); ", "; RC$(18, 1) + " " + RC$(18, 2) + " " + RC$(18, 3) + ", " + RC$(18, 4); HALF$(allRecords!(18, 2))
        End If
        Print

        Color 14: Print "MOST THREE-POINT FIELD GOALS MADE, ONE QUARTER"
        Color 15
        If allRecords!(19, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(19, 0); "- "; RC$(19, 0); ", "; RC$(19, 1) + " " + RC$(19, 2) + " " + RC$(19, 3) + ", " + RC$(19, 4); QTR$(allRecords!(19, 2))
        End If
        Print

        Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
        Color 15
        If allRecords!(21, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(21, 0); "- "; RC$(21, 0); ", "; RC$(21, 1) + " " + RC$(21, 2) + " " + RC$(21, 3) + ", " + RC$(21, 4)
        End If
        Print

        Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
        Color 15
        If allRecords!(22, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(22, 0); "- "; RC$(22, 0); ", "; RC$(22, 1) + " " + RC$(22, 2) + " " + RC$(22, 3) + ", " + RC$(22, 4); HALF$(allRecords!(22, 2))
        End If
        Print

        I$ = GetKeyPress$

        Cls
        Color 12: Print "                            INDIVIDUAL RECORDS"

        Color 14: Print "MOST FREE THROWS MADE, NO MISSES, GAME"
        Color 15

        If allRecords!(23, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(23, 0); "- "; RC$(23, 0); ", "; RC$(23, 1) + " " + RC$(23, 2) + " " + RC$(23, 3) + ", " + RC$(23, 4)
        End If

        Print

        Color 14: Print "MOST FREE THROW ATTEMPTS, NONE MADE, GAME"
        Color 15

        If allRecords!(24, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(24, 0); "- "; RC$(24, 0); ", "; RC$(24, 1) + " " + RC$(24, 2) + " " + RC$(24, 3) + ", " + RC$(24, 4)
        End If

        Print

        Color 14: Print "MOST FREE THROWS MADE, GAME"
        Color 15

        If allRecords!(25, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(25, 0); "- "; RC$(25, 0); ", "; RC$(25, 1) + " " + RC$(25, 2) + " " + RC$(25, 3) + ", " + RC$(25, 4)
        End If

        Print

        Color 14: Print "MOST FREE THROWS MADE, ONE HALF"
        Color 15

        If allRecords!(26, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(26, 0); "- "; RC$(26, 0); ", "; RC$(26, 1) + " " + RC$(26, 2) + " " + RC$(26, 3) + ", " + RC$(26, 4); HALF$(allRecords!(26, 2))
        End If

        Print

        Color 14: Print "MOST FREE THROWS MADE, ONE QUARTER"
        Color 15

        If allRecords!(27, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(27, 0); "- "; RC$(27, 0); ", "; RC$(27, 1) + " " + RC$(27, 2) + " " + RC$(27, 3) + ", " + RC$(27, 4); QTR$(allRecords!(27, 2))
        End If

        Print

        Color 14: Print "MOST FREE THROW ATTEMPTS, GAME"
        Color 15

        If allRecords!(28, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(28, 0); "- "; RC$(28, 0); ", "; RC$(28, 1) + " " + RC$(28, 2) + " " + RC$(28, 3) + ", " + RC$(28, 4)
        End If

        Print

        Color 14: Print "MOST FREE THROW ATTEMPTS, ONE HALF"
        Color 15

        If allRecords!(29, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(29, 0); "- "; RC$(29, 0); ", "; RC$(29, 1) + " " + RC$(29, 2) + " " + RC$(29, 3) + ", " + RC$(29, 4); HALF$(allRecords!(29, 2))
        End If

        Print

        Color 14: Print "MOST FREE THROW ATTEMPTS, ONE QUARTER"
        Color 15

        If allRecords!(30, 0) = 0 Then
            Print "NO RECORD AT THIS TIME";
        Else
            Print allRecords!(30, 0); "- "; RC$(30, 0); ", "; RC$(30, 1) + " " + RC$(30, 2) + " " + RC$(30, 3) + ", " + RC$(30, 4); QTR$(allRecords!(30, 2));
        End If

        I$ = GetKeyPress$

        Cls

        Color 12
        Print "                            INDIVIDUAL RECORDS"
        Print
        Color 14: Print "MOST REBOUNDS, GAME"
        Color 15

        If allRecords!(31, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(31, 0); "- "; RC$(31, 0); ", "; RC$(31, 1) + " " + RC$(31, 2) + " " + RC$(31, 3) + ", " + RC$(31, 4)
        End If
        Print

        Color 14: Print "MOST REBOUNDS, ONE HALF"

        Color 15

        If allRecords!(32, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(32, 0); "- "; RC$(32, 0); ", "; RC$(32, 1) + " " + RC$(32, 2) + " " + RC$(32, 3) + ", " + RC$(32, 4); HALF$(allRecords!(32, 2))
        End If

        Print

        Color 14: Print "MOST REBOUNDS, ONE QUARTER"
        Color 15

        If allRecords!(33, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(33, 0); "- "; RC$(33, 0); ", "; RC$(33, 1) + " " + RC$(33, 2) + " " + RC$(33, 3) + ", " + RC$(33, 4); QTR$(allRecords!(33, 2))
        End If

        Print

        Color 14: Print "MOST OFFENSIVE REBOUNDS, GAME"
        Color 15

        If allRecords!(34, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(34, 0); "- "; RC$(34, 0); ", "; RC$(34, 1) + " " + RC$(34, 2) + " " + RC$(34, 3) + ", " + RC$(34, 4)
        End If

        Print

        Color 14: Print "MOST OFFENSIVE REBOUNDS, ONE HALF"
        Color 15

        If allRecords!(35, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(35, 0); "- "; RC$(35, 0); ", "; RC$(35, 1) + " " + RC$(35, 2) + " " + RC$(35, 3) + ", " + RC$(35, 4); HALF$(allRecords!(35, 2))
        End If

        Print

        Color 14: Print "MOST OFFENSIVE REBOUNDS, ONE QUARTER"
        Color 15

        If allRecords!(36, 0) = 0 Then
            Print "NO RECORD AT THIS TIME";
        Else
            Print allRecords!(36, 0); "- "; RC$(36, 0); ", "; RC$(36, 1) + " " + RC$(36, 2) + " " + RC$(36, 3) + ", " + RC$(36, 4); QTR$(allRecords!(36, 2));
        End If

        I$ = GetKeyPress$

        Cls

        Color 12
        Print "                            INDIVIDUAL RECORDS"
        Print
        Color 14: Print "MOST DEFENSIVE REBOUNDS, GAME"
        Color 15

        If allRecords!(37, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(37, 0); "- "; RC$(37, 0); ", "; RC$(37, 1) + " " + RC$(37, 2) + " " + RC$(37, 3) + ", " + RC$(37, 4)
        End If

        Print

        Color 14: Print "MOST DEFENSIVE REBOUNDS, ONE HALF"
        Color 15

        If allRecords!(38, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(38, 0); "- "; RC$(38, 0); ", "; RC$(38, 1) + " " + RC$(38, 2) + " " + RC$(38, 3) + ", " + RC$(38, 4); HALF$(allRecords!(38, 2))
        End If
        Print

        Color 14: Print "MOST DEFENSIVE REBOUNDS, ONE QUARTER"
        Color 15

        If allRecords!(39, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(39, 0); "- "; RC$(39, 0); ", "; RC$(39, 1) + " " + RC$(39, 2) + " " + RC$(39, 3) + ", " + RC$(39, 4); QTR$(allRecords!(39, 2))
        End If

        Print

        Color 14: Print "MOST ASSISTS, GAME"
        Color 15

        If allRecords!(40, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(40, 0); "- "; RC$(40, 0); ", "; RC$(40, 1) + " " + RC$(40, 2) + " " + RC$(40, 3) + ", " + RC$(40, 4)
        End If

        Print

        Color 14: Print "MOST ASSISTS, ONE HALF"
        Color 15

        If allRecords!(41, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(41, 0); "- "; RC$(41, 0); ", "; RC$(41, 1) + " " + RC$(41, 2) + " " + RC$(41, 3) + ", " + RC$(41, 4); HALF$(allRecords!(41, 2))
        End If

        Print

        Color 14: Print "MOST ASSISTS, ONE QUARTER"

        Color 15

        If allRecords!(42, 0) = 0 Then
            Print "NO RECORD AT THIS TIME";
        Else
            Print allRecords!(42, 0); "- "; RC$(42, 0); ", "; RC$(42, 1) + " " + RC$(42, 2) + " " + RC$(42, 3) + ", " + RC$(42, 4); QTR$(allRecords!(42, 2));
        End If
        I$ = GetKeyPress$

        Cls

        Color 12
        Print "                            INDIVIDUAL RECORDS"
        Print
        Color 14: Print "MOST STEALS, GAME"
        Color 15

        If allRecords!(43, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(43, 0); "- "; RC$(43, 0); ", "; RC$(43, 1) + " " + RC$(43, 2) + " " + RC$(43, 3) + ", " + RC$(43, 4)
        End If

        Print

        Color 14: Print "MOST STEALS, ONE HALF"
        Color 15

        If allRecords!(44, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(44, 0); "- "; RC$(44, 0); ", "; RC$(44, 1) + " " + RC$(44, 2) + " " + RC$(44, 3) + ", " + RC$(44, 4); HALF$(allRecords!(44, 2))
        End If

        Print

        Color 14: Print "MOST STEALS, ONE QUARTER"
        Color 15

        If allRecords!(45, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(45, 0); "- "; RC$(45, 0); ", "; RC$(45, 1) + " " + RC$(45, 2) + " " + RC$(45, 3) + ", " + RC$(45, 4); QTR$(allRecords!(45, 2))
        End If

        Print

        Color 14: Print "MOST BLOCKS, GAME"
        Color 15

        If allRecords!(46, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(46, 0); "- "; RC$(46, 0); ", "; RC$(46, 1) + " " + RC$(46, 2) + " " + RC$(46, 3) + ", " + RC$(46, 4)
        End If

        Print

        Color 14: Print "MOST BLOCKS, ONE HALF"
        Color 15

        If allRecords!(47, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print allRecords!(47, 0); "- "; RC$(47, 0); ", "; RC$(47, 1) + " " + RC$(47, 2) + " " + RC$(47, 3) + ", " + RC$(47, 4); HALF$(allRecords!(47, 2))
        End If

        Print

        Color 14: Print "MOST BLOCKS, ONE QUARTER"
        Color 15

        If allRecords!(48, 0) = 0 Then
            Print "NO RECORD AT THIS TIME";
        Else
            Print allRecords!(48, 0); "- "; RC$(48, 0); ", "; RC$(48, 1) + " " + RC$(48, 2) + " " + RC$(48, 3) + ", " + RC$(48, 4); QTR$(allRecords!(48, 2));
        End If

        I$ = GetKeyPress$

        Cls

        Color 12
        Print "                            TEAM RECORDS"
        Print
        Color 14: Print "MOST OVERTIME PERIODS"
        Color 15

        If TotalRecords!(0, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print TotalRecords!(0, 0); "- "; recordsTRC$(0, 1);
            Print Using " (###) AT "; TotalRecords!(0, 1);
            Print recordsTRC$(0, 2);: Print Using " (###)"; TotalRecords!(0, 2);: Print ", " + recordsTRC$(0, 3)
        End If

        Print

        Color 14: Print "MOST POINTS, GAME"
        Color 15
        Print TotalRecords!(1, 0); "- "; recordsTRC$(1, 0) + " " + recordsTRC$(1, 1) + " " + recordsTRC$(1, 2) + ", " + recordsTRC$(1, 3)
        Print

        Color 14: Print "FEWEST POINTS, GAME"
        Color 15
        Print TotalRecords!(2, 0); "- "; recordsTRC$(2, 0) + " " + recordsTRC$(2, 1) + " " + recordsTRC$(2, 2) + ", " + recordsTRC$(2, 3)
        Print

        Color 14: Print "MOST POINTS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(3, 0); "- "; recordsTRC$(3, 0);
        Print Using " (###) "; TotalRecords!(3, 1);: Print recordsTRC$(3, 1) + " ";
        Print recordsTRC$(3, 2);: Print Using " (###)"; TotalRecords!(3, 2);: Print ", " + recordsTRC$(3, 3)
        Print

        Color 14: Print "FEWEST POINTS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(4, 0); "- "; recordsTRC$(4, 0);
        Print Using " (###) "; TotalRecords!(4, 1);: Print recordsTRC$(4, 1) + " ";
        Print recordsTRC$(4, 2);: Print Using " (###)"; TotalRecords!(4, 2);: Print ", " + recordsTRC$(4, 3)
        Print

        Color 14: Print "MOST POINTS, FIRST HALF"
        Color 15
        Print TotalRecords!(6, 0); "- "; recordsTRC$(6, 0) + " " + recordsTRC$(6, 1) + " " + recordsTRC$(6, 2) + ", " + recordsTRC$(6, 3)
        Print

        Color 14: Print "FEWEST POINTS, FIRST HALF"
        Color 15
        Print TotalRecords!(7, 0); "- "; recordsTRC$(7, 0) + " " + recordsTRC$(7, 1) + " " + recordsTRC$(7, 2) + ", " + recordsTRC$(7, 3)
        Print

        I$ = GetKeyPress$

        Cls

        Color 12
        Print "                            TEAM RECORDS"
        Print

        Color 14: Print "MOST POINTS, BOTH TEAMS, FIRST HALF"
        Color 15
        Print TotalRecords!(8, 0); "- "; recordsTRC$(8, 0);
        Print Using " (###) "; TotalRecords!(8, 1);: Print recordsTRC$(8, 1) + " ";
        Print recordsTRC$(8, 2);: Print Using " (###)"; TotalRecords!(8, 2);: Print ", " + recordsTRC$(8, 3)
        Print

        Color 14: Print "FEWEST POINTS, BOTH TEAMS, FIRST HALF"
        Color 15
        Print TotalRecords!(9, 0); "- "; recordsTRC$(9, 0);
        Print Using " (###) "; TotalRecords!(9, 1);: Print recordsTRC$(9, 1) + " ";
        Print recordsTRC$(9, 2);: Print Using " (###)"; TotalRecords!(9, 2);: Print ", " + recordsTRC$(9, 3)
        Print

        Color 14: Print "MOST POINTS, SECOND HALF"
        Color 15
        Print TotalRecords!(10, 0); "- "; recordsTRC$(10, 0) + " " + recordsTRC$(10, 1) + " " + recordsTRC$(10, 2) + ", " + recordsTRC$(10, 3)
        Print

        Color 14: Print "FEWEST POINTS, SECOND HALF"
        Color 15
        Print TotalRecords!(11, 0); "- "; recordsTRC$(11, 0) + " " + recordsTRC$(11, 1) + " " + recordsTRC$(11, 2) + ", " + recordsTRC$(11, 3)
        Print

        Color 14: Print "MOST POINTS, BOTH TEAMS, SECOND HALF"
        Color 15
        Print TotalRecords!(12, 0); "- "; recordsTRC$(12, 0);
        Print Using " (###) "; TotalRecords!(12, 1);: Print recordsTRC$(12, 1) + " ";
        Print recordsTRC$(12, 2);: Print Using " (###)"; TotalRecords!(12, 2);: Print ", " + recordsTRC$(12, 3)
        Print

        Color 14: Print "FEWEST POINTS, BOTH TEAMS, SECOND HALF"
        Color 15
        Print TotalRecords!(13, 0); "- "; recordsTRC$(13, 0);
        Print Using " (###) "; TotalRecords!(13, 1);: Print recordsTRC$(13, 1) + " ";
        Print recordsTRC$(13, 2);: Print Using " (###)"; TotalRecords!(13, 2);: Print ", " + recordsTRC$(13, 3)
        Print

        I$ = GetKeyPress$

        Cls
        Color 12
        Print "                            TEAM RECORDS"

        Color 14: Print "MOST POINTS, FIRST QUARTER"
        Color 15
        Print TotalRecords!(14, 0); "- "; recordsTRC$(14, 0) + " " + recordsTRC$(14, 1) + " " + recordsTRC$(14, 2) + ", " + recordsTRC$(14, 3)
        Print

        Color 14: Print "FEWEST POINTS, FIRST QUARTER"
        Color 15
        Print TotalRecords!(15, 0); "- "; recordsTRC$(15, 0) + " " + recordsTRC$(15, 1) + " " + recordsTRC$(15, 2) + ", " + recordsTRC$(15, 3)
        Print

        Color 14: Print "MOST POINTS, BOTH TEAMS, FIRST QUARTER"
        Color 15
        Print TotalRecords!(16, 0); "- "; recordsTRC$(16, 0);
        Print Using " (###) "; TotalRecords!(16, 1);: Print recordsTRC$(16, 1) + " ";
        Print recordsTRC$(16, 2);: Print Using " (###)"; TotalRecords!(16, 2);: Print ", " + recordsTRC$(16, 3)
        Print

        Color 14: Print "FEWEST POINTS, BOTH TEAMS, FIRST QUARTER"
        Color 15
        Print TotalRecords!(17, 0); "- "; recordsTRC$(17, 0);
        Print Using " (###) "; TotalRecords!(17, 1);: Print recordsTRC$(17, 1) + " ";
        Print recordsTRC$(17, 2);: Print Using " (###)"; TotalRecords!(17, 2);: Print ", " + recordsTRC$(17, 3)
        Print

        Color 14: Print "MOST POINTS, SECOND QUARTER"
        Color 15
        Print TotalRecords!(18, 0); "- "; recordsTRC$(18, 0) + " " + recordsTRC$(18, 1) + " " + recordsTRC$(18, 2) + ", " + recordsTRC$(18, 3)
        Print

        Color 14: Print "FEWEST POINTS, SECOND QUARTER"
        Color 15
        Print TotalRecords!(19, 0); "- "; recordsTRC$(19, 0) + " " + recordsTRC$(19, 1) + " " + recordsTRC$(19, 2) + ", " + recordsTRC$(19, 3)
        Print

        Color 14: Print "MOST POINTS, BOTH TEAMS, SECOND QUARTER"
        Color 15
        Print TotalRecords!(20, 0); "- "; recordsTRC$(20, 0);
        Print Using " (###) "; TotalRecords!(20, 1);: Print recordsTRC$(20, 1) + " ";
        Print recordsTRC$(20, 2);: Print Using " (###)"; TotalRecords!(20, 2);: Print ", " + recordsTRC$(20, 3)
        Print

        Color 14: Print "FEWEST POINTS, BOTH TEAMS, SECOND QUARTER"
        Color 15
        Print TotalRecords!(21, 0); "- "; recordsTRC$(21, 0);
        Print Using " (###) "; TotalRecords!(21, 1);: Print recordsTRC$(21, 1) + " ";
        Print recordsTRC$(21, 2);: Print Using " (###)"; TotalRecords!(21, 2);: Print ", " + recordsTRC$(21, 3);
        I$ = GetKeyPress$

        Cls
        Color 12
        Print "                            TEAM RECORDS"

        Color 14: Print "MOST POINTS, THIRD QUARTER"
        Color 15
        Print TotalRecords!(22, 0); "- "; recordsTRC$(22, 0) + " " + recordsTRC$(22, 1) + " " + recordsTRC$(22, 2) + ", " + recordsTRC$(22, 3)
        Print

        Color 14: Print "FEWEST POINTS, THIRD QUARTER"
        Color 15
        Print TotalRecords!(23, 0); "- "; recordsTRC$(23, 0) + " " + recordsTRC$(23, 1) + " " + recordsTRC$(23, 2) + ", " + recordsTRC$(23, 3)
        Print

        Color 14: Print "MOST POINTS, BOTH TEAMS, THIRD QUARTER"
        Color 15
        Print TotalRecords!(24, 0); "- "; recordsTRC$(24, 0);
        Print Using " (###) "; TotalRecords!(24, 1);: Print recordsTRC$(24, 1) + " ";
        Print recordsTRC$(24, 2);: Print Using " (###)"; TotalRecords!(24, 2);: Print ", " + recordsTRC$(24, 3)
        Print

        Color 14: Print "FEWEST POINTS, BOTH TEAMS, THIRD QUARTER"
        Color 15
        Print TotalRecords!(25, 0); "- "; recordsTRC$(25, 0);
        Print Using " (###) "; TotalRecords!(25, 1);: Print recordsTRC$(25, 1) + " ";
        Print recordsTRC$(25, 2);: Print Using " (###)"; TotalRecords!(25, 2);: Print ", " + recordsTRC$(25, 3)
        Print

        Color 14: Print "MOST POINTS, FOURTH QUARTER"
        Color 15
        Print TotalRecords!(26, 0); "- "; recordsTRC$(26, 0) + " " + recordsTRC$(26, 1) + " " + recordsTRC$(26, 2) + ", " + recordsTRC$(26, 3)
        Print

        Color 14: Print "FEWEST POINTS, FOURTH QUARTER"
        Color 15
        Print TotalRecords!(27, 0); "- "; recordsTRC$(27, 0) + " " + recordsTRC$(27, 1) + " " + recordsTRC$(27, 2) + ", " + recordsTRC$(27, 3)
        Print

        Color 14: Print "MOST POINTS, BOTH TEAMS, FOURTH QUARTER"
        Color 15
        Print TotalRecords!(28, 0); "- "; recordsTRC$(28, 0);
        Print Using " (###) "; TotalRecords!(28, 1);: Print recordsTRC$(28, 1) + " ";
        Print recordsTRC$(28, 2);: Print Using " (###)"; TotalRecords!(28, 2);: Print ", " + recordsTRC$(28, 3)
        Print

        Color 14: Print "FEWEST POINTS, BOTH TEAMS, FOURTH QUARTER"
        Color 15
        Print TotalRecords!(29, 0); "- "; recordsTRC$(29, 0);
        Print Using " (###) "; TotalRecords!(29, 1);: Print recordsTRC$(29, 1) + " ";
        Print recordsTRC$(29, 2);: Print Using " (###)"; TotalRecords!(29, 2);: Print ", " + recordsTRC$(29, 3);

        I$ = GetKeyPress$

        Cls
        Color 12
        Print "                            TEAM RECORDS"

        Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, GAME"
        Color 15
        Print Using ".### "; TotalRecords!(30, 0);: Print "- "; recordsTRC$(30, 0) + " " + recordsTRC$(30, 1) + " " + recordsTRC$(30, 2) + ", " + recordsTRC$(30, 3);: Print Using "###/###"; TotalRecords!(30, 1); TotalRecords!(30, 2)
        Print

        Color 14: Print "LOWEST FIELD GOAL PERCENTAGE, GAME"
        Color 15
        Print Using ".### "; TotalRecords!(31, 0);: Print "- "; recordsTRC$(31, 0) + " " + recordsTRC$(31, 1) + " " + recordsTRC$(31, 2) + ", " + recordsTRC$(31, 3);: Print Using "###/###"; TotalRecords!(31, 1); TotalRecords!(31, 2)
        Print

        Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
        Color 15
        Print Using ".### "; TotalRecords!(32, 0);: Print "- "; recordsTRC$(32, 0);
        Print Using " (.###) "; TotalRecords!(32, 1);: Print recordsTRC$(32, 1) + " ";
        Print recordsTRC$(32, 2);: Print Using " (.###)"; TotalRecords!(32, 2);: Print ", " + recordsTRC$(32, 3)
        Print

        Color 14: Print "LOWEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
        Color 15
        Print Using ".### "; TotalRecords!(33, 0);: Print "- "; recordsTRC$(33, 0);
        Print Using " (.###) "; TotalRecords!(33, 1);: Print recordsTRC$(33, 1) + " ";
        Print recordsTRC$(33, 2);: Print Using " (.###)"; TotalRecords!(33, 2);: Print ", " + recordsTRC$(33, 3)
        Print

        Color 14: Print "MOST FIELD GOALS, GAME"
        Color 15
        Print TotalRecords!(34, 0); "- "; recordsTRC$(34, 0) + " " + recordsTRC$(34, 1) + " " + recordsTRC$(34, 2) + ", " + recordsTRC$(34, 3)
        Print

        Color 14: Print "FEWEST FIELD GOALS, GAME"
        Color 15
        Print TotalRecords!(35, 0); "- "; recordsTRC$(35, 0) + " " + recordsTRC$(35, 1) + " " + recordsTRC$(35, 2) + ", " + recordsTRC$(35, 3)
        Print

        Color 14: Print "MOST FIELD GOALS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(36, 0); "- "; recordsTRC$(36, 0);
        Print Using " (###) "; TotalRecords!(36, 1);: Print recordsTRC$(36, 1) + " ";
        Print recordsTRC$(36, 2);: Print Using " (###)"; TotalRecords!(36, 2);: Print ", " + recordsTRC$(36, 3)
        Print

        Color 14: Print "FEWEST FIELD GOALS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(37, 0); "- "; recordsTRC$(37, 0);
        Print Using " (###) "; TotalRecords!(37, 1);: Print recordsTRC$(37, 1) + " ";
        Print recordsTRC$(37, 2);: Print Using " (###)"; TotalRecords!(37, 2);: Print ", " + recordsTRC$(37, 3);

        I$ = GetKeyPress$

        Cls

        Color 12
        Print "                            TEAM RECORDS"

        Color 14: Print "MOST FIELD GOALS, ONE HALF"
        Color 15
        Print TotalRecords!(38, 0); "- "; recordsTRC$(38, 0) + " " + recordsTRC$(38, 1) + " " + recordsTRC$(38, 2) + ", " + recordsTRC$(38, 3); HALF$(TotalRecords!(38, 1))
        Print

        Color 14: Print "MOST FIELD GOALS, BOTH TEAMS, ONE HALF"
        Color 15
        Print TotalRecords!(39, 0); "- "; recordsTRC$(39, 0); " "; recordsTRC$(39, 1); " "; recordsTRC$(39, 2); ", "; recordsTRC$(39, 3); HALF$(TotalRecords!(39, 1))
        Print

        Color 14: Print "MOST FIELD GOALS, ONE QUARTER"
        Color 15
        Print TotalRecords!(40, 0); "- "; recordsTRC$(40, 0) + " " + recordsTRC$(40, 1) + " " + recordsTRC$(40, 2) + ", " + recordsTRC$(40, 3); QTR$(TotalRecords!(40, 1))
        Print

        Color 14: Print "MOST FIELD GOALS, BOTH TEAMS, ONE QUARTER"
        Color 15
        Print TotalRecords!(41, 0); "- "; recordsTRC$(41, 0); " "; recordsTRC$(41, 1); " "; recordsTRC$(41, 2); ", "; recordsTRC$(41, 3); QTR$(TotalRecords!(41, 1))
        Print

        Color 14: Print "MOST FIELD GOAL ATTEMPTS, GAME"
        Color 15
        Print TotalRecords!(42, 0); "- "; recordsTRC$(42, 0) + " " + recordsTRC$(42, 1) + " " + recordsTRC$(42, 2) + ", " + recordsTRC$(42, 3)
        Print

        Color 14: Print "FEWEST FIELD GOAL ATTEMPTS, GAME"
        Color 15
        Print TotalRecords!(43, 0); "- "; recordsTRC$(43, 0) + " " + recordsTRC$(43, 1) + " " + recordsTRC$(43, 2) + ", " + recordsTRC$(43, 3)
        Print

        Color 14: Print "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(44, 0); "- "; recordsTRC$(44, 0);
        Print Using " (###) "; TotalRecords!(44, 1);: Print recordsTRC$(44, 1) + " ";
        Print recordsTRC$(44, 2);: Print Using " (###)"; TotalRecords!(44, 2);: Print ", " + recordsTRC$(44, 3)
        Print

        Color 14: Print "FEWEST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(45, 0); "- "; recordsTRC$(45, 0);
        Print Using " (###) "; TotalRecords!(45, 1);: Print recordsTRC$(45, 1) + " ";
        Print recordsTRC$(45, 2);: Print Using " (###)"; TotalRecords!(45, 2);: Print ", " + recordsTRC$(45, 3);

        I$ = GetKeyPress$

        Cls
        Color 12
        Print "                            TEAM RECORDS"
        Print

        Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE HALF"
        Color 15
        Print TotalRecords!(46, 0); "- "; recordsTRC$(46, 0) + " " + recordsTRC$(46, 1) + " " + recordsTRC$(46, 2) + ", " + recordsTRC$(46, 3); HALF$(TotalRecords!(46, 1))
        Print

        Color 14: Print "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE HALF"
        Color 15
        Print TotalRecords!(47, 0); "- "; recordsTRC$(47, 0); " "; recordsTRC$(47, 1); " "; recordsTRC$(47, 2); ", "; recordsTRC$(47, 3); HALF$(TotalRecords!(47, 1))
        Print

        Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE QUARTER"
        Color 15
        Print TotalRecords!(48, 0); "- "; recordsTRC$(48, 0) + " " + recordsTRC$(48, 1) + " " + recordsTRC$(48, 2) + ", " + recordsTRC$(48, 3); QTR$(TotalRecords!(48, 1))
        Print

        Color 14: Print "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE QUARTER"
        Color 15
        Print TotalRecords!(49, 0); "- "; recordsTRC$(49, 0); " "; recordsTRC$(49, 1); " "; recordsTRC$(49, 2); ", "; recordsTRC$(49, 3); QTR$(TotalRecords!(49, 1))
        Print

        Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
        Color 15

        If TotalRecords!(50, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print TotalRecords!(50, 0); "- "; recordsTRC$(50, 0) + " " + recordsTRC$(50, 1) + " " + recordsTRC$(50, 2) + ", " + recordsTRC$(50, 3)
        End If

        Print

        Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, BOTH TEAMS, GAME"
        Color 15

        If TotalRecords!(51, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print TotalRecords!(51, 0); "- "; recordsTRC$(51, 0);
            Print Using " (###) "; TotalRecords!(51, 1);: Print recordsTRC$(51, 1) + " ";
            Print recordsTRC$(51, 2);: Print Using " (###)"; TotalRecords!(51, 2);: Print ", " + recordsTRC$(51, 3)
        End If

        Print

        Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
        Color 15

        If TotalRecords!(52, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print TotalRecords!(52, 0); "- "; recordsTRC$(52, 0) + " " + recordsTRC$(52, 1) + " " + recordsTRC$(52, 2) + ", " + recordsTRC$(52, 3)
        End If

        Print

        I$ = GetKeyPress$

        Cls

        Color 12
        Print "                            TEAM RECORDS"
        Print

        Color 14: Print "MOST THREE-POINT FIELD GOALS, GAME"
        Color 15
        Print TotalRecords!(53, 0); "- "; recordsTRC$(53, 0) + " " + recordsTRC$(53, 1) + " " + recordsTRC$(53, 2) + ", " + recordsTRC$(53, 3)
        Print

        Color 14: Print "MOST THREE-POINT FIELD GOALS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(54, 0); "- "; recordsTRC$(54, 0);
        Print Using " (###) "; TotalRecords!(54, 1);: Print recordsTRC$(54, 1) + " ";
        Print recordsTRC$(54, 2);: Print Using " (###)"; TotalRecords!(54, 2);: Print ", " + recordsTRC$(54, 3)
        Print

        Color 14: Print "MOST THREE-POINT FIELD GOALS, ONE HALF"
        Color 15
        Print TotalRecords!(55, 0); "- "; recordsTRC$(55, 0) + " " + recordsTRC$(55, 1) + " " + recordsTRC$(55, 2) + ", " + recordsTRC$(55, 3); HALF$(TotalRecords!(55, 1))
        Print

        Color 14: Print "MOST THREE-POINT FIELD GOALS, ONE QUARTER"
        Color 15
        Print TotalRecords!(56, 0); "- "; recordsTRC$(56, 0) + " " + recordsTRC$(56, 1) + " " + recordsTRC$(56, 2) + ", " + recordsTRC$(56, 3); QTR$(TotalRecords!(56, 1))
        Print

        Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
        Color 15
        Print TotalRecords!(57, 0); "- "; recordsTRC$(57, 0) + " " + recordsTRC$(57, 1) + " " + recordsTRC$(57, 2) + ", " + recordsTRC$(57, 3)
        Print

        Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(58, 0); "- "; recordsTRC$(58, 0);
        Print Using " (###) "; TotalRecords!(58, 1);: Print recordsTRC$(58, 1) + " ";
        Print recordsTRC$(58, 2);: Print Using " (###)"; TotalRecords!(58, 2);: Print ", " + recordsTRC$(58, 3)
        Print

        Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
        Color 15
        Print TotalRecords!(59, 0); "- "; recordsTRC$(59, 0) + " " + recordsTRC$(59, 1) + " " + recordsTRC$(59, 2) + ", " + recordsTRC$(59, 3); HALF$(TotalRecords!(59, 1))
        I$ = GetKeyPress$

        Cls

        Color 12
        Print "                            TEAM RECORDS"

        Color 14: Print "MOST FREE THROWS MADE, NO MISSES, GAME"
        Color 15
        If TotalRecords!(60, 0) = 0 Then
            Print "NO RECORD AT THIS TIME"
        Else
            Print TotalRecords!(60, 0); "- "; recordsTRC$(60, 0) + " " + recordsTRC$(60, 1) + " " + recordsTRC$(60, 2) + ", " + recordsTRC$(60, 3)
        End If
        Print

        Color 14: Print "LOWEST FREE THROW PERCENTAGE, GAME"
        Color 15
        Print Using ".### "; TotalRecords!(61, 0);: Print "- "; recordsTRC$(61, 0) + " " + recordsTRC$(61, 1) + " " + recordsTRC$(61, 2) + ", " + recordsTRC$(61, 3);: Print Using "###/###"; TotalRecords!(61, 1); TotalRecords!(61, 2)
        Print

        Color 14: Print "HIGHEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
        Color 15
        Print Using ".### "; TotalRecords!(62, 0);: Print "- "; recordsTRC$(62, 0);
        Print Using " (.###) "; TotalRecords!(62, 1);: Print recordsTRC$(62, 1) + " ";
        Print recordsTRC$(62, 2);: Print Using " (.###)"; TotalRecords!(62, 2);: Print ", " + recordsTRC$(62, 3)
        Print

        Color 14: Print "LOWEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
        Color 15
        Print Using ".### "; TotalRecords!(63, 0);: Print "- "; recordsTRC$(63, 0);
        Print Using " (.###) "; TotalRecords!(63, 1);: Print recordsTRC$(63, 1) + " ";
        Print recordsTRC$(63, 2);: Print Using " (.###)"; TotalRecords!(63, 2);: Print ", " + recordsTRC$(63, 3)
        Print

        Color 14: Print "MOST FREE THROWS MADE, GAME"
        Color 15
        Print TotalRecords!(64, 0); "- "; recordsTRC$(64, 0) + " " + recordsTRC$(64, 1) + " " + recordsTRC$(64, 2) + ", " + recordsTRC$(64, 3)
        Print

        Color 14: Print "FEWEST FREE THROWS MADE, GAME"
        Color 15
        Print TotalRecords!(65, 0); "- "; recordsTRC$(65, 0) + " " + recordsTRC$(65, 1) + " " + recordsTRC$(65, 2) + ", " + recordsTRC$(65, 3)
        Print

        Color 14: Print "MOST FREE THROWS MADE, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(66, 0); "- "; recordsTRC$(66, 0);
        Print Using " (###) "; TotalRecords!(66, 1);: Print recordsTRC$(66, 1) + " ";
        Print recordsTRC$(66, 2);: Print Using " (###)"; TotalRecords!(66, 2);: Print ", " + recordsTRC$(66, 3)
        Print

        Color 14: Print "FEWEST FREE THROWS MADE, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(67, 0); "- "; recordsTRC$(67, 0);
        Print Using " (###) "; TotalRecords!(67, 1);: Print recordsTRC$(67, 1) + " ";
        Print recordsTRC$(67, 2);: Print Using " (###)"; TotalRecords!(67, 2);: Print ", " + recordsTRC$(67, 3);

        I$ = GetKeyPress$

        Cls

        Color 12
        Print "                            TEAM RECORDS"

        Color 14: Print "MOST FREE THROWS MADE, ONE HALF"
        Color 15
        Print TotalRecords!(68, 0); "- "; recordsTRC$(68, 0) + " " + recordsTRC$(68, 1) + " " + recordsTRC$(68, 2) + ", " + recordsTRC$(68, 3); HALF$(TotalRecords!(68, 1))
        Print

        Color 14: Print "MOST FREE THROWS MADE, ONE QUARTER"
        Color 15
        Print TotalRecords!(70, 0); "- "; recordsTRC$(70, 0) + " " + recordsTRC$(70, 1) + " " + recordsTRC$(70, 2) + ", " + recordsTRC$(70, 3); QTR$(TotalRecords!(70, 1))
        Print

        Color 14: Print "MOST FREE THROW ATTEMPTS, GAME"
        Color 15
        Print TotalRecords!(72, 0); "- "; recordsTRC$(72, 0) + " " + recordsTRC$(72, 1) + " " + recordsTRC$(72, 2) + ", " + recordsTRC$(72, 3)
        Print

        Color 14: Print "FEWEST FREE THROW ATTEMPTS, GAME"
        Color 15
        Print TotalRecords!(73, 0); "- "; recordsTRC$(73, 0) + " " + recordsTRC$(73, 1) + " " + recordsTRC$(73, 2) + ", " + recordsTRC$(73, 3)
        Print

        Color 14: Print "MOST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(74, 0); "- "; recordsTRC$(74, 0);
        Print Using " (###) "; TotalRecords!(74, 1);: Print recordsTRC$(74, 1) + " ";
        Print recordsTRC$(74, 2);: Print Using " (###)"; TotalRecords!(74, 2);: Print ", " + recordsTRC$(74, 3)
        Print

        Color 14: Print "FEWEST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(75, 0); "- "; recordsTRC$(75, 0);
        Print Using " (###) "; TotalRecords!(75, 1);: Print recordsTRC$(75, 1) + " ";
        Print recordsTRC$(75, 2);: Print Using " (###)"; TotalRecords!(75, 2);: Print ", " + recordsTRC$(75, 3)
        Print

        Color 14: Print "MOST FREE THROW ATTEMPTS, ONE HALF"
        Color 15
        Print TotalRecords!(76, 0); "- "; recordsTRC$(76, 0) + " " + recordsTRC$(76, 1) + " " + recordsTRC$(76, 2) + ", " + recordsTRC$(76, 3); HALF$(TotalRecords!(76, 1))
        Print

        Color 14: Print "MOST FREE THROW ATTEMPTS, ONE QUARTER"
        Color 15
        Print TotalRecords!(78, 0); "- "; recordsTRC$(78, 0) + " " + recordsTRC$(78, 1) + " " + recordsTRC$(78, 2) + ", " + recordsTRC$(78, 3); QTR$(TotalRecords!(78, 1));
        I$ = GetKeyPress$

        Cls

        Color 12
        Print "                            TEAM RECORDS"
        Print
        Print
        Color 14: Print "MOST REBOUNDS, GAME"
        Color 15
        Print TotalRecords!(80, 0); "- "; recordsTRC$(80, 0) + " " + recordsTRC$(80, 1) + " " + recordsTRC$(80, 2) + ", " + recordsTRC$(80, 3)
        Print

        Color 14: Print "FEWEST REBOUNDS, GAME"
        Color 15
        Print TotalRecords!(81, 0); "- "; recordsTRC$(81, 0) + " " + recordsTRC$(81, 1) + " " + recordsTRC$(81, 2) + ", " + recordsTRC$(81, 3)
        Print

        Color 14: Print "MOST REBOUNDS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(82, 0); "- "; recordsTRC$(82, 0);
        Print Using " (###) "; TotalRecords!(82, 1);: Print recordsTRC$(82, 1) + " ";
        Print recordsTRC$(82, 2);: Print Using " (###)"; TotalRecords!(82, 2);: Print ", " + recordsTRC$(82, 3)
        Print

        Color 14: Print "FEWEST REBOUNDS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(83, 0); "- "; recordsTRC$(83, 0);
        Print Using " (###) "; TotalRecords!(83, 1);: Print recordsTRC$(83, 1) + " ";
        Print recordsTRC$(83, 2);: Print Using " (###)"; TotalRecords!(83, 2);: Print ", " + recordsTRC$(83, 3)
        Print

        Color 14: Print "MOST REBOUNDS, ONE HALF"
        Color 15
        Print TotalRecords!(84, 0); "- "; recordsTRC$(84, 0) + " " + recordsTRC$(84, 1) + " " + recordsTRC$(84, 2) + ", " + recordsTRC$(84, 3); HALF$(TotalRecords!(84, 1))
        Print

        Color 14: Print "MOST REBOUNDS, ONE QUARTER"
        Color 15
        Print TotalRecords!(85, 0); "- "; recordsTRC$(85, 0) + " " + recordsTRC$(85, 1) + " " + recordsTRC$(85, 2) + ", " + recordsTRC$(85, 3); QTR$(TotalRecords!(85, 1))
        Print
        I$ = GetKeyPress$

        Cls

        Color 12
        Print "                            TEAM RECORDS"
        Print
        Print
        Color 14: Print "MOST OFFENSIVE REBOUNDS, GAME"
        Color 15
        Print TotalRecords!(86, 0); "- "; recordsTRC$(86, 0) + " " + recordsTRC$(86, 1) + " " + recordsTRC$(86, 2) + ", " + recordsTRC$(86, 3)
        Print

        Color 14: Print "MOST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(88, 0); "- "; recordsTRC$(88, 0);
        Print Using " (###) "; TotalRecords!(88, 1);: Print recordsTRC$(88, 1) + " ";
        Print recordsTRC$(88, 2);: Print Using " (###)"; TotalRecords!(88, 2);: Print ", " + recordsTRC$(88, 3)
        Print

        Color 14: Print "FEWEST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(89, 0); "- "; recordsTRC$(89, 0);
        Print Using " (###) "; TotalRecords!(89, 1);: Print recordsTRC$(89, 1) + " ";
        Print recordsTRC$(89, 2);: Print Using " (###)"; TotalRecords!(89, 2);: Print ", " + recordsTRC$(89, 3)
        Print

        Color 14: Print "MOST OFFENSIVE REBOUNDS, ONE HALF"
        Color 15
        Print TotalRecords!(90, 0); "- "; recordsTRC$(90, 0) + " " + recordsTRC$(90, 1) + " " + recordsTRC$(90, 2) + ", " + recordsTRC$(90, 3); HALF$(TotalRecords!(90, 1))
        Print
        I$ = GetKeyPress$

        Cls

        Color 12
        Print "                            TEAM RECORDS"
        Print
        Color 14: Print "MOST ASSISTS, GAME"
        Color 15
        Print TotalRecords!(96, 0); "- "; recordsTRC$(96, 0) + " " + recordsTRC$(96, 1) + " " + recordsTRC$(96, 2) + ", " + recordsTRC$(96, 3)
        Print

        Color 14: Print "FEWEST ASSISTS, GAME"
        Color 15
        Print TotalRecords!(97, 0); "- "; recordsTRC$(97, 0) + " " + recordsTRC$(97, 1) + " " + recordsTRC$(97, 2) + ", " + recordsTRC$(97, 3)
        Print

        Color 14: Print "MOST ASSISTS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(98, 0); "- "; recordsTRC$(98, 0);
        Print Using " (###) "; TotalRecords!(98, 1);: Print recordsTRC$(98, 1) + " ";
        Print recordsTRC$(98, 2);: Print Using " (###)"; TotalRecords!(98, 2);: Print ", " + recordsTRC$(98, 3)
        Print

        Color 14: Print "FEWEST ASSISTS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(99, 0); "- "; recordsTRC$(99, 0);
        Print Using " (###) "; TotalRecords!(99, 1);: Print recordsTRC$(99, 1) + " ";
        Print recordsTRC$(99, 2);: Print Using " (###)"; TotalRecords!(99, 2);: Print ", " + recordsTRC$(99, 3)
        Print

        Color 14: Print "MOST ASSISTS, ONE HALF"
        Color 15
        Print TotalRecords!(100, 0); "- "; recordsTRC$(100, 0) + " " + recordsTRC$(100, 1) + " " + recordsTRC$(100, 2) + ", " + recordsTRC$(100, 3); HALF$(TotalRecords!(100, 1))
        Print

        Color 14: Print "MOST ASSISTS, ONE QUARTER"
        Color 15
        Print TotalRecords!(102, 0); "- "; recordsTRC$(102, 0) + " " + recordsTRC$(102, 1) + " " + recordsTRC$(102, 2) + ", " + recordsTRC$(102, 3); QTR$(TotalRecords!(102, 1))
        Print

        I$ = GetKeyPress$

        Cls
        Color 12
        Print "                            TEAM RECORDS"
        Color 14: Print "MOST PERSONAL FOULS, GAME"
        Color 15
        Print TotalRecords!(104, 0); "- "; recordsTRC$(104, 0) + " " + recordsTRC$(104, 1) + " " + recordsTRC$(104, 2) + ", " + recordsTRC$(104, 3)
        Print

        Color 14: Print "FEWEST PERSONAL FOULS, GAME"
        Color 15
        Print TotalRecords!(105, 0); "- "; recordsTRC$(105, 0) + " " + recordsTRC$(105, 1) + " " + recordsTRC$(105, 2) + ", " + recordsTRC$(105, 3)
        Print

        Color 14: Print "MOST PERSONAL FOULS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(106, 0); "- "; recordsTRC$(106, 0);
        Print Using " (###) "; TotalRecords!(106, 1);: Print recordsTRC$(106, 1) + " ";
        Print recordsTRC$(106, 2);: Print Using " (###)"; TotalRecords!(106, 2);: Print ", " + recordsTRC$(106, 3)
        Print

        Color 14: Print "FEWEST PERSONAL FOULS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(107, 0); "- "; recordsTRC$(107, 0);
        Print Using " (###) "; TotalRecords!(107, 1);: Print recordsTRC$(107, 1) + " ";
        Print recordsTRC$(107, 2);: Print Using " (###)"; TotalRecords!(107, 2);: Print ", " + recordsTRC$(107, 3)
        Print

        Color 14: Print "MOST PERSONAL FOULS, ONE HALF"
        Color 15
        Print TotalRecords!(108, 0); "- "; recordsTRC$(108, 0) + " " + recordsTRC$(108, 1) + " " + recordsTRC$(108, 2) + ", " + recordsTRC$(108, 3); HALF$(TotalRecords!(108, 1))
        Print

        Color 14: Print "MOST PERSONAL FOULS, BOTH TEAMS, ONE HALF"
        Color 15
        Print TotalRecords!(109, 0); "- "; recordsTRC$(109, 0); " "; recordsTRC$(109, 1); " "; recordsTRC$(109, 2); ", "; recordsTRC$(109, 3); HALF$(TotalRecords!(109, 1))
        Print

        Color 14: Print "MOST PERSONAL FOULS, ONE QUARTER"
        Color 15
        Print TotalRecords!(110, 0); "- "; recordsTRC$(110, 0) + " " + recordsTRC$(110, 1) + " " + recordsTRC$(110, 2) + ", " + recordsTRC$(110, 3); QTR$(TotalRecords!(110, 1))
        Print

        Color 14: Print "MOST PERSONAL FOULS, BOTH TEAMS, ONE QUARTER"
        Color 15
        Print TotalRecords!(111, 0); "- "; recordsTRC$(111, 0); " "; recordsTRC$(111, 1); " "; recordsTRC$(111, 2); ", "; recordsTRC$(111, 3); QTR$(TotalRecords!(111, 1));

        I$ = GetKeyPress$

        Cls
        Color 12
        Print "                            TEAM RECORDS"
        Print
        Color 14: Print "MOST STEALS, GAME"
        Color 15
        Print TotalRecords!(112, 0); "- "; recordsTRC$(112, 0) + " " + recordsTRC$(112, 1) + " " + recordsTRC$(112, 2) + ", " + recordsTRC$(112, 3)
        Print

        Color 14: Print "MOST STEALS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(114, 0); "- "; recordsTRC$(114, 0);
        Print Using " (###) "; TotalRecords!(114, 1);: Print recordsTRC$(114, 1) + " ";
        Print recordsTRC$(114, 2);: Print Using " (###)"; TotalRecords!(114, 2);: Print ", " + recordsTRC$(114, 3)
        Print

        Color 14: Print "MOST STEALS, ONE HALF"
        Color 15
        Print TotalRecords!(116, 0); "- "; recordsTRC$(116, 0) + " " + recordsTRC$(116, 1) + " " + recordsTRC$(116, 2) + ", " + recordsTRC$(116, 3); HALF$(TotalRecords!(116, 1))
        Print

        Color 14: Print "MOST STEALS, ONE QUARTER"
        Color 15
        Print TotalRecords!(117, 0); "- "; recordsTRC$(117, 0) + " " + recordsTRC$(117, 1) + " " + recordsTRC$(117, 2) + ", " + recordsTRC$(117, 3); QTR$(TotalRecords!(117, 1))
        Print

        Color 14: Print "MOST BLOCKS, GAME"
        Color 15
        Print TotalRecords!(118, 0); "- "; recordsTRC$(118, 0) + " " + recordsTRC$(118, 1) + " " + recordsTRC$(118, 2) + ", " + recordsTRC$(118, 3)
        Print

        Color 14: Print "MOST BLOCKS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(119, 0); "- "; recordsTRC$(119, 0);
        Print Using " (###) "; TotalRecords!(119, 1);: Print recordsTRC$(119, 1) + " ";
        Print recordsTRC$(119, 2);: Print Using " (###)"; TotalRecords!(119, 2);: Print ", " + recordsTRC$(119, 3)
        Print
        I$ = GetKeyPress$

        Cls
        Color 12
        Print "                            TEAM RECORDS"
        Print
        Color 14: Print "MOST TURNOVERS, GAME"
        Color 15
        Print TotalRecords!(122, 0); "- "; recordsTRC$(122, 0) + " " + recordsTRC$(122, 1) + " " + recordsTRC$(122, 2) + ", " + recordsTRC$(122, 3)
        Print

        Color 14: Print "FEWEST TURNOVERS, GAME"
        Color 15
        Print TotalRecords!(123, 0); "- "; recordsTRC$(123, 0) + " " + recordsTRC$(123, 1) + " " + recordsTRC$(123, 2) + ", " + recordsTRC$(123, 3)
        Print

        Color 14: Print "MOST TURNOVERS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(124, 0); "- "; recordsTRC$(124, 0);
        Print Using " (###) "; TotalRecords!(124, 1);: Print recordsTRC$(124, 1) + " ";
        Print recordsTRC$(124, 2);: Print Using " (###)"; TotalRecords!(124, 2);: Print ", " + recordsTRC$(124, 3)
        Print

        Color 14: Print "FEWEST TURNOVERS, BOTH TEAMS, GAME"
        Color 15
        Print TotalRecords!(125, 0); "- "; recordsTRC$(125, 0);
        Print Using " (###) "; TotalRecords!(125, 1);: Print recordsTRC$(125, 1) + " ";
        Print recordsTRC$(125, 2);: Print Using " (###)"; TotalRecords!(125, 2);: Print ", " + recordsTRC$(125, 3)
        Print

        I$ = GetKeyPress$

    End If

End Sub

'----------------------------------------
'         WriteRecords Subroutine
'----------------------------------------
'...explanation...
Sub WriteRecords ()

    Shared RC$(), recordsTRC$()
    Shared allRecords!(), TotalRecords!()
    Shared HALF$(), QTR$()

    Color 0, 15

    'PRINT ROUTINE
    Open "RECORDS" For Output As #2
    Print #2, "                            INDIVIDUAL RECORDS"
    Print #2,
    Print #2, "MOST POINTS, GAME"
    Print #2, allRecords!(0, 0); "- "; RC$(0, 0); ", "; RC$(0, 1) + " " + RC$(0, 2) + " " + RC$(0, 3) + ", " + RC$(0, 4)
    Print #2,
    Print #2, "MOST POINTS, ONE HALF"
    Print #2, allRecords!(1, 0); "- "; RC$(1, 0); ", "; RC$(1, 1) + " " + RC$(1, 2) + " " + RC$(1, 3) + ", " + RC$(1, 4); HALF$(allRecords!(1, 2))
    Print #2,
    Print #2, "MOST POINTS, ONE QUARTER"
    Print #2, allRecords!(2, 0); "- "; RC$(2, 0); ", "; RC$(2, 1) + " " + RC$(2, 2) + " " + RC$(2, 3) + ", " + RC$(2, 4); QTR$(allRecords!(2, 2))
    Print #2,
    Print #2, "MOST POINTS, OVERTIME PERIOD"

    If allRecords!(3, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(3, 0); "- "; RC$(3, 0); ", "; RC$(3, 1) + " " + RC$(3, 2) + " " + RC$(3, 3) + ", " + RC$(3, 4)
    End If

    Print #2,
    Print #2, "HIGHEST FIELD GOAL PERCENTAGE, GAME (MINIMUM 15 FIELD GOALS)"

    If allRecords!(4, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        If allRecords!(4, 0) = 1 Then
            Print #2, "1.000 ";
        Else
            Print #2, Using " .### "; allRecords!(4, 0);
        End If
        Print #2, "- "; RC$(4, 0); ", "; RC$(4, 1) + " " + RC$(4, 2) + " " + RC$(4, 3) + ", " + RC$(4, 4);
        Print #2, Using " (##/##)"; allRecords!(4, 1); allRecords!(4, 2)
    End If

    Print #2,
    Print #2, "MOST FIELD GOALS, NO MISSES, GAME"

    If allRecords!(5, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(5, 0); "- "; RC$(5, 0); ", "; RC$(5, 1) + " " + RC$(5, 2) + " " + RC$(5, 3) + ", " + RC$(5, 4)
    End If

    Print #2,
    Print #2, "MOST FIELD GOAL ATTEMPTS, NONE MADE, GAME"

    If allRecords!(6, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(6, 0); "- "; RC$(6, 0); ", "; RC$(6, 1) + " " + RC$(6, 2) + " " + RC$(6, 3) + ", " + RC$(6, 4)
    End If

    Print #2,
    Print #2, "MOST FIELD GOALS, GAME"
    Print #2, allRecords!(7, 0); "- "; RC$(7, 0); ", "; RC$(7, 1) + " " + RC$(7, 2) + " " + RC$(7, 3) + ", " + RC$(7, 4)
    Print #2,
    Print #2, "MOST FIELD GOALS, ONE HALF"
    Print #2, allRecords!(8, 0); "- "; RC$(8, 0); ", "; RC$(8, 1) + " " + RC$(8, 2) + " " + RC$(8, 3) + ", " + RC$(8, 4); HALF$(allRecords!(8, 2))
    Print #2,
    Print #2, "MOST FIELD GOALS, ONE QUARTER"
    Print #2, allRecords!(9, 0); "- "; RC$(9, 0); ", "; RC$(9, 1) + " " + RC$(9, 2) + " " + RC$(9, 3) + ", " + RC$(9, 4); QTR$(allRecords!(9, 2))
    Print #2,
    Print #2, "MOST FIELD GOALS, OVERTIME"

    If allRecords!(10, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(10, 0); "- "; RC$(10, 0); ", "; RC$(10, 1) + " " + RC$(10, 2) + " " + RC$(10, 3) + ", " + RC$(10, 4)
    End If

    Print #2,
    Print #2, "MOST FIELD GOAL ATTEMPTS, GAME"
    Print #2, allRecords!(11, 0); "- "; RC$(11, 0); ", "; RC$(11, 1) + " " + RC$(11, 2) + " " + RC$(11, 3) + ", " + RC$(11, 4)
    Print #2,
    Print #2, "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Print #2, allRecords!(12, 0); "- "; RC$(12, 0); ", "; RC$(12, 1) + " " + RC$(12, 2) + " " + RC$(12, 3) + ", " + RC$(12, 4); HALF$(allRecords!(12, 2))
    Print #2,
    Print #2, "MOST FIELD GOAL ATTEMPTS, ONE QUARTER"
    Print #2, allRecords!(13, 0); "- "; RC$(13, 0); ", "; RC$(13, 1) + " " + RC$(13, 2) + " " + RC$(13, 3) + ", " + RC$(13, 4); QTR$(allRecords!(13, 2))
    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"

    If allRecords!(15, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(15, 0); "- "; RC$(15, 0); ", "; RC$(15, 1) + " " + RC$(15, 2) + " " + RC$(15, 3) + ", " + RC$(15, 4)
    End If

    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"

    If allRecords!(16, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(16, 0); "- "; RC$(16, 0); ", "; RC$(16, 1) + " " + RC$(16, 2) + " " + RC$(16, 3) + ", " + RC$(16, 4)
    End If

    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOALS, GAME"

    If allRecords!(17, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(17, 0); "- "; RC$(17, 0); ", "; RC$(17, 1) + " " + RC$(17, 2) + " " + RC$(17, 3) + ", " + RC$(17, 4)
    End If

    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOALS MADE, ONE HALF"

    If allRecords!(18, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(18, 0); "- "; RC$(18, 0); ", "; RC$(18, 1) + " " + RC$(18, 2) + " " + RC$(18, 3) + ", " + RC$(18, 4); HALF$(allRecords!(18, 2))
    End If

    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOALS MADE, ONE QUARTER"

    If allRecords!(19, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(19, 0); "- "; RC$(19, 0); ", "; RC$(19, 1) + " " + RC$(19, 2) + " " + RC$(19, 3) + ", " + RC$(19, 4); QTR$(allRecords!(19, 2))
    End If

    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"

    If allRecords!(21, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(21, 0); "- "; RC$(21, 0); ", "; RC$(21, 1) + " " + RC$(21, 2) + " " + RC$(21, 3) + ", " + RC$(21, 4)
    End If

    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"

    If allRecords!(22, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(22, 0); "- "; RC$(22, 0); ", "; RC$(22, 1) + " " + RC$(22, 2) + " " + RC$(22, 3) + ", " + RC$(22, 4); HALF$(allRecords!(22, 2))
    End If

    Print #2,
    Print #2, "MOST FREE THROWS MADE, NO MISSES, GAME"

    If allRecords!(23, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(23, 0); "- "; RC$(23, 0); ", "; RC$(23, 1) + " " + RC$(23, 2) + " " + RC$(23, 3) + ", " + RC$(23, 4)
    End If

    Print #2,
    Print #2, "MOST FREE THROW ATTEMPTS, NONE MADE, GAME"

    If allRecords!(24, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(24, 0); "- "; RC$(24, 0); ", "; RC$(24, 1) + " " + RC$(24, 2) + " " + RC$(24, 3) + ", " + RC$(24, 4)
    End If

    Print #2,
    Print #2, "MOST FREE THROWS MADE, GAME"

    If allRecords!(25, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(25, 0); "- "; RC$(25, 0); ", "; RC$(25, 1) + " " + RC$(25, 2) + " " + RC$(25, 3) + ", " + RC$(25, 4)
    End If

    Print #2,
    Print #2, "MOST FREE THROWS MADE, ONE HALF"

    If allRecords!(26, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(26, 0); "- "; RC$(26, 0); ", "; RC$(26, 1) + " " + RC$(26, 2) + " " + RC$(26, 3) + ", " + RC$(26, 4); HALF$(allRecords!(26, 2))
    End If

    Print #2,
    Print #2, "MOST FREE THROWS MADE, ONE QUARTER"

    If allRecords!(27, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(27, 0); "- "; RC$(27, 0); ", "; RC$(27, 1) + " " + RC$(27, 2) + " " + RC$(27, 3) + ", " + RC$(27, 4); QTR$(allRecords!(27, 2))
    End If

    Print #2,
    Print #2, "MOST FREE THROW ATTEMPTS, GAME"

    If allRecords!(28, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(28, 0); "- "; RC$(28, 0); ", "; RC$(28, 1) + " " + RC$(28, 2) + " " + RC$(28, 3) + ", " + RC$(28, 4)
    End If

    Print #2,
    Print #2, "MOST FREE THROW ATTEMPTS, ONE HALF"

    If allRecords!(29, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(29, 0); "- "; RC$(29, 0); ", "; RC$(29, 1) + " " + RC$(29, 2) + " " + RC$(29, 3) + ", " + RC$(29, 4); HALF$(allRecords!(29, 2))
    End If

    Print #2,
    Print #2, "MOST FREE THROW ATTEMPTS, ONE QUARTER"

    If allRecords!(30, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME";
    Else
        Print #2, allRecords!(30, 0); "- "; RC$(30, 0); ", "; RC$(30, 1) + " " + RC$(30, 2) + " " + RC$(30, 3) + ", " + RC$(30, 4); QTR$(allRecords!(30, 2))
    End If

    Print #2,
    Print #2, "MOST REBOUNDS, GAME"

    If allRecords!(31, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(31, 0); "- "; RC$(31, 0); ", "; RC$(31, 1) + " " + RC$(31, 2) + " " + RC$(31, 3) + ", " + RC$(31, 4)
    End If

    Print #2,
    Print #2, "MOST REBOUNDS, ONE HALF"

    If allRecords!(32, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(32, 0); "- "; RC$(32, 0); ", "; RC$(32, 1) + " " + RC$(32, 2) + " " + RC$(32, 3) + ", " + RC$(32, 4); HALF$(allRecords!(32, 2))
    End If

    Print #2,
    Print #2, "MOST REBOUNDS, ONE QUARTER"

    If allRecords!(33, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(33, 0); "- "; RC$(33, 0); ", "; RC$(33, 1) + " " + RC$(33, 2) + " " + RC$(33, 3) + ", " + RC$(33, 4); QTR$(allRecords!(33, 2))
    End If

    Print #2,
    Print #2, "MOST OFFENSIVE REBOUNDS, GAME"

    If allRecords!(34, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(34, 0); "- "; RC$(34, 0); ", "; RC$(34, 1) + " " + RC$(34, 2) + " " + RC$(34, 3) + ", " + RC$(34, 4)
    End If

    Print #2,
    Print #2, "MOST OFFENSIVE REBOUNDS, ONE HALF"
    If allRecords!(35, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(35, 0); "- "; RC$(35, 0); ", "; RC$(35, 1) + " " + RC$(35, 2) + " " + RC$(35, 3) + ", " + RC$(35, 4); HALF$(allRecords!(35, 2))
    End If

    Print #2,
    Print #2, "MOST OFFENSIVE REBOUNDS, ONE QUARTER"

    If allRecords!(36, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME";
    Else
        Print #2, allRecords!(36, 0); "- "; RC$(36, 0); ", "; RC$(36, 1) + " " + RC$(36, 2) + " " + RC$(36, 3) + ", " + RC$(36, 4); QTR$(allRecords!(36, 2))
    End If

    Print #2,
    Print #2, "MOST DEFENSIVE REBOUNDS, GAME"

    If allRecords!(37, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(37, 0); "- "; RC$(37, 0); ", "; RC$(37, 1) + " " + RC$(37, 2) + " " + RC$(37, 3) + ", " + RC$(37, 4)
    End If

    Print #2,
    Print #2, "MOST DEFENSIVE REBOUNDS, ONE HALF"

    If allRecords!(38, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(38, 0); "- "; RC$(38, 0); ", "; RC$(38, 1) + " " + RC$(38, 2) + " " + RC$(38, 3) + ", " + RC$(38, 4); HALF$(allRecords!(38, 2))
    End If

    Print #2,
    Print #2, "MOST DEFENSIVE REBOUNDS, ONE QUARTER"

    If allRecords!(39, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(39, 0); "- "; RC$(39, 0); ", "; RC$(39, 1) + " " + RC$(39, 2) + " " + RC$(39, 3) + ", " + RC$(39, 4); QTR$(allRecords!(39, 2))
    End If

    Print #2,
    Print #2, "MOST ASSISTS, GAME"

    If allRecords!(40, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(40, 0); "- "; RC$(40, 0); ", "; RC$(40, 1) + " " + RC$(40, 2) + " " + RC$(40, 3) + ", " + RC$(40, 4)
    End If

    Print #2,
    Print #2, "MOST ASSISTS, ONE HALF"

    If allRecords!(41, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(41, 0); "- "; RC$(41, 0); ", "; RC$(41, 1) + " " + RC$(41, 2) + " " + RC$(41, 3) + ", " + RC$(41, 4); HALF$(allRecords!(41, 2))
    End If

    Print #2,
    Print #2, "MOST ASSISTS, ONE QUARTER"

    If allRecords!(42, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(42, 0); "- "; RC$(42, 0); ", "; RC$(42, 1) + " " + RC$(42, 2) + " " + RC$(42, 3) + ", " + RC$(42, 4); QTR$(allRecords!(42, 2))
    End If

    Print #2,
    Print #2, "MOST STEALS, GAME"

    If allRecords!(43, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(43, 0); "- "; RC$(43, 0); ", "; RC$(43, 1) + " " + RC$(43, 2) + " " + RC$(43, 3) + ", " + RC$(43, 4)
    End If

    Print #2,
    Print #2, "MOST STEALS, ONE HALF"

    If allRecords!(44, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(44, 0); "- "; RC$(44, 0); ", "; RC$(44, 1) + " " + RC$(44, 2) + " " + RC$(44, 3) + ", " + RC$(44, 4); HALF$(allRecords!(44, 2))
    End If

    Print #2,
    Print #2, "MOST STEALS, ONE QUARTER"

    If allRecords!(45, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(45, 0); "- "; RC$(45, 0); ", "; RC$(45, 1) + " " + RC$(45, 2) + " " + RC$(45, 3) + ", " + RC$(45, 4); QTR$(allRecords!(45, 2))
    End If

    Print #2,
    Print #2, "MOST BLOCKS, GAME"

    If allRecords!(46, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(46, 0); "- "; RC$(46, 0); ", "; RC$(46, 1) + " " + RC$(46, 2) + " " + RC$(46, 3) + ", " + RC$(46, 4)
    End If

    Print #2,
    Print #2, "MOST BLOCKS, ONE HALF"

    If allRecords!(47, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, allRecords!(47, 0); "- "; RC$(47, 0); ", "; RC$(47, 1) + " " + RC$(47, 2) + " " + RC$(47, 3) + ", " + RC$(47, 4); HALF$(allRecords!(47, 2))
    End If

    Print #2,
    Print #2, "MOST BLOCKS, ONE QUARTER"

    If allRecords!(48, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME";
    Else
        Print #2, allRecords!(48, 0); "- "; RC$(48, 0); ", "; RC$(48, 1) + " " + RC$(48, 2) + " " + RC$(48, 3) + ", " + RC$(48, 4); QTR$(allRecords!(48, 2));
    End If

    Print #2, Chr$(12)

    Print #2, "                            TEAM RECORDS"
    Print #2,
    Print #2, "MOST OVERTIME PERIODS"

    If TotalRecords!(0, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, TotalRecords!(0, 0); "- "; recordsTRC$(0, 1);
        Print #2, Using " (###) AT "; TotalRecords!(0, 1);
        Print #2, recordsTRC$(0, 2);: Print #2, Using " (###)"; TotalRecords!(0, 2);: Print #2, ", " + recordsTRC$(0, 3)
    End If

    Print #2,
    Print #2, "MOST POINTS, GAME"
    Print #2, TotalRecords!(1, 0); "- "; recordsTRC$(1, 0) + " " + recordsTRC$(1, 1) + " " + recordsTRC$(1, 2) + ", " + recordsTRC$(1, 3)
    Print #2,
    Print #2, "FEWEST POINTS, GAME"
    Print #2, TotalRecords!(2, 0); "- "; recordsTRC$(2, 0) + " " + recordsTRC$(2, 1) + " " + recordsTRC$(2, 2) + ", " + recordsTRC$(2, 3)
    Print #2,
    Print #2, "MOST POINTS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(3, 0); "- "; recordsTRC$(3, 0);
    Print #2, Using " (###) "; TotalRecords!(3, 1);: Print #2, recordsTRC$(3, 1) + " ";
    Print #2, recordsTRC$(3, 2);: Print #2, Using " (###)"; TotalRecords!(3, 2);: Print #2, ", " + recordsTRC$(3, 3)
    Print #2,
    Print #2, "FEWEST POINTS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(4, 0); "- "; recordsTRC$(4, 0);
    Print #2, Using " (###) "; TotalRecords!(4, 1);: Print #2, recordsTRC$(4, 1) + " ";
    Print #2, recordsTRC$(4, 2);: Print #2, Using " (###)"; TotalRecords!(4, 2);: Print #2, ", " + recordsTRC$(4, 3)
    Print #2,
    Print #2, "MOST POINTS, FIRST HALF"
    Print #2, TotalRecords!(6, 0); "- "; recordsTRC$(6, 0) + " " + recordsTRC$(6, 1) + " " + recordsTRC$(6, 2) + ", " + recordsTRC$(6, 3)
    Print #2,
    Print #2, "FEWEST POINTS, FIRST HALF"
    Print #2, TotalRecords!(7, 0); "- "; recordsTRC$(7, 0) + " " + recordsTRC$(7, 1) + " " + recordsTRC$(7, 2) + ", " + recordsTRC$(7, 3)
    Print #2,

    Print #2, "MOST POINTS, BOTH TEAMS, FIRST HALF"
    Print #2, TotalRecords!(8, 0); "- "; recordsTRC$(8, 0);
    Print #2, Using " (###) "; TotalRecords!(8, 1);: Print #2, recordsTRC$(8, 1) + " ";
    Print #2, recordsTRC$(8, 2);: Print #2, Using " (###)"; TotalRecords!(8, 2);: Print #2, ", " + recordsTRC$(8, 3)
    Print #2,
    Print #2, "FEWEST POINTS, BOTH TEAMS, FIRST HALF"
    Print #2, TotalRecords!(9, 0); "- "; recordsTRC$(9, 0);
    Print #2, Using " (###) "; TotalRecords!(9, 1);: Print #2, recordsTRC$(9, 1) + " ";
    Print #2, recordsTRC$(9, 2);: Print #2, Using " (###)"; TotalRecords!(9, 2);: Print #2, ", " + recordsTRC$(9, 3)
    Print #2,
    Print #2, "MOST POINTS, SECOND HALF"
    Print #2, TotalRecords!(10, 0); "- "; recordsTRC$(10, 0) + " " + recordsTRC$(10, 1) + " " + recordsTRC$(10, 2) + ", " + recordsTRC$(10, 3)
    Print #2,
    Print #2, "FEWEST POINTS, SECOND HALF"
    Print #2, TotalRecords!(11, 0); "- "; recordsTRC$(11, 0) + " " + recordsTRC$(11, 1) + " " + recordsTRC$(11, 2) + ", " + recordsTRC$(11, 3)
    Print #2,
    Print #2, "MOST POINTS, BOTH TEAMS, SECOND HALF"
    Print #2, TotalRecords!(12, 0); "- "; recordsTRC$(12, 0);
    Print #2, Using " (###) "; TotalRecords!(12, 1);: Print #2, recordsTRC$(12, 1) + " ";
    Print #2, recordsTRC$(12, 2);: Print #2, Using " (###)"; TotalRecords!(12, 2);: Print #2, ", " + recordsTRC$(12, 3)
    Print #2,
    Print #2, "FEWEST POINTS, BOTH TEAMS, SECOND HALF"
    Print #2, TotalRecords!(13, 0); "- "; recordsTRC$(13, 0);
    Print #2, Using " (###) "; TotalRecords!(13, 1);: Print #2, recordsTRC$(13, 1) + " ";
    Print #2, recordsTRC$(13, 2);: Print #2, Using " (###)"; TotalRecords!(13, 2);: Print #2, ", " + recordsTRC$(13, 3)
    Print #2,

    Print #2, "MOST POINTS, FIRST QUARTER"
    Print #2, TotalRecords!(14, 0); "- "; recordsTRC$(14, 0) + " " + recordsTRC$(14, 1) + " " + recordsTRC$(14, 2) + ", " + recordsTRC$(14, 3)
    Print #2,
    Print #2, "FEWEST POINTS, FIRST QUARTER"
    Print #2, TotalRecords!(15, 0); "- "; recordsTRC$(15, 0) + " " + recordsTRC$(15, 1) + " " + recordsTRC$(15, 2) + ", " + recordsTRC$(15, 3)
    Print #2,
    Print #2, "MOST POINTS, BOTH TEAMS, FIRST QUARTER"
    Print #2, TotalRecords!(16, 0); "- "; recordsTRC$(16, 0);
    Print #2, Using " (###) "; TotalRecords!(16, 1);: Print #2, recordsTRC$(16, 1) + " ";
    Print #2, recordsTRC$(16, 2);: Print #2, Using " (###)"; TotalRecords!(16, 2);: Print #2, ", " + recordsTRC$(16, 3)
    Print #2,
    Print #2, "FEWEST POINTS, BOTH TEAMS, FIRST QUARTER"
    Print #2, TotalRecords!(17, 0); "- "; recordsTRC$(17, 0);
    Print #2, Using " (###) "; TotalRecords!(17, 1);: Print #2, recordsTRC$(17, 1) + " ";
    Print #2, recordsTRC$(17, 2);: Print #2, Using " (###)"; TotalRecords!(17, 2);: Print #2, ", " + recordsTRC$(17, 3)
    Print #2,
    Print #2, "MOST POINTS, SECOND QUARTER"
    Print #2, TotalRecords!(18, 0); "- "; recordsTRC$(18, 0) + " " + recordsTRC$(18, 1) + " " + recordsTRC$(18, 2) + ", " + recordsTRC$(18, 3)
    Print #2,
    Print #2, "FEWEST POINTS, SECOND QUARTER"
    Print #2, TotalRecords!(19, 0); "- "; recordsTRC$(19, 0) + " " + recordsTRC$(19, 1) + " " + recordsTRC$(19, 2) + ", " + recordsTRC$(19, 3)
    Print #2,
    Print #2, "MOST POINTS, BOTH TEAMS, SECOND QUARTER"
    Print #2, TotalRecords!(20, 0); "- "; recordsTRC$(20, 0);
    Print #2, Using " (###) "; TotalRecords!(20, 1);: Print #2, recordsTRC$(20, 1) + " ";
    Print #2, recordsTRC$(20, 2);: Print #2, Using " (###)"; TotalRecords!(20, 2);: Print #2, ", " + recordsTRC$(20, 3)
    Print #2,
    Print #2, "FEWEST POINTS, BOTH TEAMS, SECOND QUARTER"
    Print #2, TotalRecords!(21, 0); "- "; recordsTRC$(21, 0);
    Print #2, Using " (###) "; TotalRecords!(21, 1);: Print #2, recordsTRC$(21, 1) + " ";
    Print #2, recordsTRC$(21, 2);: Print #2, Using " (###)"; TotalRecords!(21, 2);: Print #2, ", " + recordsTRC$(21, 3)
    Print #2,
    Print #2, "MOST POINTS, THIRD QUARTER"
    Print #2, TotalRecords!(22, 0); "- "; recordsTRC$(22, 0) + " " + recordsTRC$(22, 1) + " " + recordsTRC$(22, 2) + ", " + recordsTRC$(22, 3)
    Print #2,
    Print #2, "FEWEST POINTS, THIRD QUARTER"
    Print #2, TotalRecords!(23, 0); "- "; recordsTRC$(23, 0) + " " + recordsTRC$(23, 1) + " " + recordsTRC$(23, 2) + ", " + recordsTRC$(23, 3)
    Print #2,
    Print #2, "MOST POINTS, BOTH TEAMS, THIRD QUARTER"
    Print #2, TotalRecords!(24, 0); "- "; recordsTRC$(24, 0);
    Print #2, Using " (###) "; TotalRecords!(24, 1);: Print #2, recordsTRC$(24, 1) + " ";
    Print #2, recordsTRC$(24, 2);: Print #2, Using " (###)"; TotalRecords!(24, 2);: Print #2, ", " + recordsTRC$(24, 3)
    Print #2,
    Print #2, "FEWEST POINTS, BOTH TEAMS, THIRD QUARTER"
    Print #2, TotalRecords!(25, 0); "- "; recordsTRC$(25, 0);
    Print #2, Using " (###) "; TotalRecords!(25, 1);: Print #2, recordsTRC$(25, 1) + " ";
    Print #2, recordsTRC$(25, 2);: Print #2, Using " (###)"; TotalRecords!(25, 2);: Print #2, ", " + recordsTRC$(25, 3)
    Print #2,
    Print #2, "MOST POINTS, FOURTH QUARTER"
    Print #2, TotalRecords!(26, 0); "- "; recordsTRC$(26, 0) + " " + recordsTRC$(26, 1) + " " + recordsTRC$(26, 2) + ", " + recordsTRC$(26, 3)
    Print #2,
    Print #2, "FEWEST POINTS, FOURTH QUARTER"
    Print #2, TotalRecords!(27, 0); "- "; recordsTRC$(27, 0) + " " + recordsTRC$(27, 1) + " " + recordsTRC$(27, 2) + ", " + recordsTRC$(27, 3)
    Print #2,
    Print #2, "MOST POINTS, BOTH TEAMS, FOURTH QUARTER"
    Print #2, TotalRecords!(28, 0); "- "; recordsTRC$(28, 0);
    Print #2, Using " (###) "; TotalRecords!(28, 1);: Print #2, recordsTRC$(28, 1) + " ";
    Print #2, recordsTRC$(28, 2);: Print #2, Using " (###)"; TotalRecords!(28, 2);: Print #2, ", " + recordsTRC$(28, 3)
    Print #2,
    Print #2, "FEWEST POINTS, BOTH TEAMS, FOURTH QUARTER"
    Print #2, TotalRecords!(29, 0); "- "; recordsTRC$(29, 0);
    Print #2, Using " (###) "; TotalRecords!(29, 1);: Print #2, recordsTRC$(29, 1) + " ";
    Print #2, recordsTRC$(29, 2);: Print #2, Using " (###)"; TotalRecords!(29, 2);: Print #2, ", " + recordsTRC$(29, 3)
    Print #2,
    Print #2, "HIGHEST FIELD GOAL PERCENTAGE, GAME"
    Print #2, Using ".### "; TotalRecords!(30, 0);: Print #2, "- "; recordsTRC$(30, 0) + " " + recordsTRC$(30, 1) + " " + recordsTRC$(30, 2) + ", " + recordsTRC$(30, 3);: Print #2, Using "###/###"; TotalRecords!(30, 1); TotalRecords!(30, 2)
    Print #2,
    Print #2, "LOWEST FIELD GOAL PERCENTAGE, GAME"
    Print #2, Using ".### "; TotalRecords!(31, 0);: Print #2, "- "; recordsTRC$(31, 0) + " " + recordsTRC$(31, 1) + " " + recordsTRC$(31, 2) + ", " + recordsTRC$(31, 3);: Print #2, Using "###/###"; TotalRecords!(31, 1); TotalRecords!(31, 2)
    Print #2,
    Print #2, "HIGHEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Print #2, Using ".### "; TotalRecords!(32, 0);: Print #2, "- "; recordsTRC$(32, 0);
    Print #2, Using " (.###) "; TotalRecords!(32, 1);: Print #2, recordsTRC$(32, 1) + " ";
    Print #2, recordsTRC$(32, 2);: Print #2, Using " (.###)"; TotalRecords!(32, 2);: Print #2, ", " + recordsTRC$(32, 3)
    Print #2,
    Print #2, "LOWEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Print #2, Using ".### "; TotalRecords!(33, 0);: Print #2, "- "; recordsTRC$(33, 0);
    Print #2, Using " (.###) "; TotalRecords!(33, 1);: Print #2, recordsTRC$(33, 1) + " ";
    Print #2, recordsTRC$(33, 2);: Print #2, Using " (.###)"; TotalRecords!(33, 2);: Print #2, ", " + recordsTRC$(33, 3)
    Print #2,
    Print #2, "MOST FIELD GOALS, GAME"
    Print #2, TotalRecords!(34, 0); "- "; recordsTRC$(34, 0) + " " + recordsTRC$(34, 1) + " " + recordsTRC$(34, 2) + ", " + recordsTRC$(34, 3)
    Print #2,
    Print #2, "FEWEST FIELD GOALS, GAME"
    Print #2, TotalRecords!(35, 0); "- "; recordsTRC$(35, 0) + " " + recordsTRC$(35, 1) + " " + recordsTRC$(35, 2) + ", " + recordsTRC$(35, 3)
    Print #2,
    Print #2, "MOST FIELD GOALS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(36, 0); "- "; recordsTRC$(36, 0);
    Print #2, Using " (###) "; TotalRecords!(36, 1);: Print #2, recordsTRC$(36, 1) + " ";
    Print #2, recordsTRC$(36, 2);: Print #2, Using " (###)"; TotalRecords!(36, 2);: Print #2, ", " + recordsTRC$(36, 3)
    Print #2,
    Print #2, "FEWEST FIELD GOALS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(37, 0); "- "; recordsTRC$(37, 0);
    Print #2, Using " (###) "; TotalRecords!(37, 1);: Print #2, recordsTRC$(37, 1) + " ";
    Print #2, recordsTRC$(37, 2);: Print #2, Using " (###)"; TotalRecords!(37, 2);: Print #2, ", " + recordsTRC$(37, 3)
    Print #2,
    Print #2, "MOST FIELD GOALS, ONE HALF"
    Print #2, TotalRecords!(38, 0); "- "; recordsTRC$(38, 0) + " " + recordsTRC$(38, 1) + " " + recordsTRC$(38, 2) + ", " + recordsTRC$(38, 3); HALF$(TotalRecords!(38, 1))
    Print #2,
    Print #2, "MOST FIELD GOALS, BOTH TEAMS, ONE HALF"
    Print #2, TotalRecords!(39, 0); "- "; recordsTRC$(39, 0); " "; recordsTRC$(39, 1); " "; recordsTRC$(39, 2); ", "; recordsTRC$(39, 3); HALF$(TotalRecords!(39, 1))
    Print #2,
    Print #2, "MOST FIELD GOALS, ONE QUARTER"
    Print #2, TotalRecords!(40, 0); "- "; recordsTRC$(40, 0) + " " + recordsTRC$(40, 1) + " " + recordsTRC$(40, 2) + ", " + recordsTRC$(40, 3); QTR$(TotalRecords!(40, 1))
    Print #2,
    Print #2, "MOST FIELD GOALS, BOTH TEAMS, ONE QUARTER"
    Print #2, TotalRecords!(41, 0); "- "; recordsTRC$(41, 0); " "; recordsTRC$(41, 1); " "; recordsTRC$(41, 2); ", "; recordsTRC$(41, 3); QTR$(TotalRecords!(41, 1))
    Print #2,
    Print #2, "MOST FIELD GOAL ATTEMPTS, GAME"
    Print #2, TotalRecords!(42, 0); "- "; recordsTRC$(42, 0) + " " + recordsTRC$(42, 1) + " " + recordsTRC$(42, 2) + ", " + recordsTRC$(42, 3)
    Print #2,
    Print #2, "FEWEST FIELD GOAL ATTEMPTS, GAME"
    Print #2, TotalRecords!(43, 0); "- "; recordsTRC$(43, 0) + " " + recordsTRC$(43, 1) + " " + recordsTRC$(43, 2) + ", " + recordsTRC$(43, 3)
    Print #2,
    Print #2, "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(44, 0); "- "; recordsTRC$(44, 0);
    Print #2, Using " (###) "; TotalRecords!(44, 1);: Print #2, recordsTRC$(44, 1) + " ";
    Print #2, recordsTRC$(44, 2);: Print #2, Using " (###)"; TotalRecords!(44, 2);: Print #2, ", " + recordsTRC$(44, 3)
    Print #2,
    Print #2, "FEWEST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(45, 0); "- "; recordsTRC$(45, 0);
    Print #2, Using " (###) "; TotalRecords!(45, 1);: Print #2, recordsTRC$(45, 1) + " ";
    Print #2, recordsTRC$(45, 2);: Print #2, Using " (###)"; TotalRecords!(45, 2);: Print #2, ", " + recordsTRC$(45, 3)
    Print #2,
    Print #2, "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Print #2, TotalRecords!(46, 0); "- "; recordsTRC$(46, 0) + " " + recordsTRC$(46, 1) + " " + recordsTRC$(46, 2) + ", " + recordsTRC$(46, 3); HALF$(TotalRecords!(46, 1))
    Print #2,
    Print #2, "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE HALF"
    Print #2, TotalRecords!(47, 0); "- "; recordsTRC$(47, 0); " "; recordsTRC$(47, 1); " "; recordsTRC$(47, 2); ", "; recordsTRC$(47, 3); HALF$(TotalRecords!(47, 1))
    Print #2,
    Print #2, "MOST FIELD GOAL ATTEMPTS, ONE QUARTER"
    Print #2, TotalRecords!(48, 0); "- "; recordsTRC$(48, 0) + " " + recordsTRC$(48, 1) + " " + recordsTRC$(48, 2) + ", " + recordsTRC$(48, 3); QTR$(TotalRecords!(48, 1))
    Print #2,
    Print #2, "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE QUARTER"
    Print #2, TotalRecords!(49, 0); "- "; recordsTRC$(49, 0); " "; recordsTRC$(49, 1); " "; recordsTRC$(49, 2); ", "; recordsTRC$(49, 3); QTR$(TotalRecords!(49, 1))
    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    If TotalRecords!(50, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, TotalRecords!(50, 0); "- "; recordsTRC$(50, 0) + " " + recordsTRC$(50, 1) + " " + recordsTRC$(50, 2) + ", " + recordsTRC$(50, 3)
    End If
    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOALS, NO MISSES, BOTH TEAMS, GAME"
    If TotalRecords!(51, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, TotalRecords!(51, 0); "- "; recordsTRC$(51, 0);
        Print #2, Using " (###) "; TotalRecords!(51, 1);: Print #2, recordsTRC$(51, 1) + " ";
        Print #2, recordsTRC$(51, 2);: Print #2, Using " (###)"; TotalRecords!(51, 2);: Print #2, ", " + recordsTRC$(51, 3)
    End If
    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    If TotalRecords!(52, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, TotalRecords!(52, 0); "- "; recordsTRC$(52, 0) + " " + recordsTRC$(52, 1) + " " + recordsTRC$(52, 2) + ", " + recordsTRC$(52, 3)
    End If
    Print #2,
    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOALS, GAME"
    Print #2, TotalRecords!(53, 0); "- "; recordsTRC$(53, 0) + " " + recordsTRC$(53, 1) + " " + recordsTRC$(53, 2) + ", " + recordsTRC$(53, 3)
    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOALS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(54, 0); "- "; recordsTRC$(54, 0);
    Print #2, Using " (###) "; TotalRecords!(54, 1);: Print #2, recordsTRC$(54, 1) + " ";
    Print #2, recordsTRC$(54, 2);: Print #2, Using " (###)"; TotalRecords!(54, 2);: Print #2, ", " + recordsTRC$(54, 3)
    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOALS, ONE HALF"
    Print #2, TotalRecords!(55, 0); "- "; recordsTRC$(55, 0) + " " + recordsTRC$(55, 1) + " " + recordsTRC$(55, 2) + ", " + recordsTRC$(55, 3); HALF$(TotalRecords!(55, 1))
    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOALS, ONE QUARTER"
    Print #2, TotalRecords!(56, 0); "- "; recordsTRC$(56, 0) + " " + recordsTRC$(56, 1) + " " + recordsTRC$(56, 2) + ", " + recordsTRC$(56, 3); QTR$(TotalRecords!(56, 1))
    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    Print #2, TotalRecords!(57, 0); "- "; recordsTRC$(57, 0) + " " + recordsTRC$(57, 1) + " " + recordsTRC$(57, 2) + ", " + recordsTRC$(57, 3)
    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(58, 0); "- "; recordsTRC$(58, 0);
    Print #2, Using " (###) "; TotalRecords!(58, 1);: Print #2, recordsTRC$(58, 1) + " ";
    Print #2, recordsTRC$(58, 2);: Print #2, Using " (###)"; TotalRecords!(58, 2);: Print #2, ", " + recordsTRC$(58, 3)
    Print #2,
    Print #2, "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    Print #2, TotalRecords!(59, 0); "- "; recordsTRC$(59, 0) + " " + recordsTRC$(59, 1) + " " + recordsTRC$(59, 2) + ", " + recordsTRC$(59, 3); HALF$(TotalRecords!(59, 1))
    Print #2,
    Print #2, "MOST FREE THROWS MADE, NO MISSES, GAME"
    If TotalRecords!(60, 0) = 0 Then
        Print #2, "NO RECORD AT THIS TIME"
    Else
        Print #2, TotalRecords!(60, 0); "- "; recordsTRC$(60, 0) + " " + recordsTRC$(60, 1) + " " + recordsTRC$(60, 2) + ", " + recordsTRC$(60, 3)
    End If
    Print #2,
    Print #2, "LOWEST FREE THROW PERCENTAGE, GAME"
    Print #2, Using ".### "; TotalRecords!(61, 0);: Print #2, "- "; recordsTRC$(61, 0) + " " + recordsTRC$(61, 1) + " " + recordsTRC$(61, 2) + ", " + recordsTRC$(61, 3);: Print #2, Using "###/###"; TotalRecords!(61, 1); TotalRecords!(61, 2)
    Print #2,
    Print #2, "HIGHEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Print #2, Using ".### "; TotalRecords!(62, 0);: Print #2, "- "; recordsTRC$(62, 0);
    Print #2, Using " (.###) "; TotalRecords!(62, 1);: Print #2, recordsTRC$(62, 1) + " ";
    Print #2, recordsTRC$(62, 2);: Print #2, Using " (.###)"; TotalRecords!(62, 2);: Print #2, ", " + recordsTRC$(62, 3)
    Print #2,
    Print #2, "LOWEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Print #2, Using ".### "; TotalRecords!(63, 0);: Print #2, "- "; recordsTRC$(63, 0);
    Print #2, Using " (.###) "; TotalRecords!(63, 1);: Print #2, recordsTRC$(63, 1) + " ";
    Print #2, recordsTRC$(63, 2);: Print #2, Using " (.###)"; TotalRecords!(63, 2);: Print #2, ", " + recordsTRC$(63, 3)
    Print #2,
    Print #2, "MOST FREE THROWS MADE, GAME"
    Print #2, TotalRecords!(64, 0); "- "; recordsTRC$(64, 0) + " " + recordsTRC$(64, 1) + " " + recordsTRC$(64, 2) + ", " + recordsTRC$(64, 3)
    Print #2,
    Print #2, "FEWEST FREE THROWS MADE, GAME"
    Print #2, TotalRecords!(65, 0); "- "; recordsTRC$(65, 0) + " " + recordsTRC$(65, 1) + " " + recordsTRC$(65, 2) + ", " + recordsTRC$(65, 3)
    Print #2,
    Print #2, "MOST FREE THROWS MADE, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(66, 0); "- "; recordsTRC$(66, 0);
    Print #2, Using " (###) "; TotalRecords!(66, 1);: Print #2, recordsTRC$(66, 1) + " ";
    Print #2, recordsTRC$(66, 2);: Print #2, Using " (###)"; TotalRecords!(66, 2);: Print #2, ", " + recordsTRC$(66, 3)
    Print #2,
    Print #2, "FEWEST FREE THROWS MADE, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(67, 0); "- "; recordsTRC$(67, 0);
    Print #2, Using " (###) "; TotalRecords!(67, 1);: Print #2, recordsTRC$(67, 1) + " ";
    Print #2, recordsTRC$(67, 2);: Print #2, Using " (###)"; TotalRecords!(67, 2);: Print #2, ", " + recordsTRC$(67, 3)
    Print #2,
    Print #2, "MOST FREE THROWS MADE, ONE HALF"
    Print #2, TotalRecords!(68, 0); "- "; recordsTRC$(68, 0) + " " + recordsTRC$(68, 1) + " " + recordsTRC$(68, 2) + ", " + recordsTRC$(68, 3); HALF$(TotalRecords!(68, 1))
    Print #2,
    Print #2, "MOST FREE THROWS MADE, ONE QUARTER"
    Print #2, TotalRecords!(70, 0); "- "; recordsTRC$(70, 0) + " " + recordsTRC$(70, 1) + " " + recordsTRC$(70, 2) + ", " + recordsTRC$(70, 3); QTR$(TotalRecords!(70, 1))
    Print #2,
    Print #2, "MOST FREE THROW ATTEMPTS, GAME"
    Print #2, TotalRecords!(72, 0); "- "; recordsTRC$(72, 0) + " " + recordsTRC$(72, 1) + " " + recordsTRC$(72, 2) + ", " + recordsTRC$(72, 3)
    Print #2,
    Print #2, "FEWEST FREE THROW ATTEMPTS, GAME"
    Print #2, TotalRecords!(73, 0); "- "; recordsTRC$(73, 0) + " " + recordsTRC$(73, 1) + " " + recordsTRC$(73, 2) + ", " + recordsTRC$(73, 3)
    Print #2,
    Print #2, "MOST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(74, 0); "- "; recordsTRC$(74, 0);
    Print #2, Using " (###) "; TotalRecords!(74, 1);: Print #2, recordsTRC$(74, 1) + " ";
    Print #2, recordsTRC$(74, 2);: Print #2, Using " (###)"; TotalRecords!(74, 2);: Print #2, ", " + recordsTRC$(74, 3)
    Print #2,
    Print #2, "FEWEST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(75, 0); "- "; recordsTRC$(75, 0);
    Print #2, Using " (###) "; TotalRecords!(75, 1);: Print #2, recordsTRC$(75, 1) + " ";
    Print #2, recordsTRC$(75, 2);: Print #2, Using " (###)"; TotalRecords!(75, 2);: Print #2, ", " + recordsTRC$(75, 3)
    Print #2,
    Print #2, "MOST FREE THROW ATTEMPTS, ONE HALF"
    Print #2, TotalRecords!(76, 0); "- "; recordsTRC$(76, 0) + " " + recordsTRC$(76, 1) + " " + recordsTRC$(76, 2) + ", " + recordsTRC$(76, 3); HALF$(TotalRecords!(76, 1))
    Print #2,
    Print #2, "MOST FREE THROW ATTEMPTS, ONE QUARTER"
    Print #2, TotalRecords!(78, 0); "- "; recordsTRC$(78, 0) + " " + recordsTRC$(78, 1) + " " + recordsTRC$(78, 2) + ", " + recordsTRC$(78, 3); QTR$(TotalRecords!(78, 1))
    Print #2,
    Print #2, "MOST REBOUNDS, GAME"
    Print #2, TotalRecords!(80, 0); "- "; recordsTRC$(80, 0) + " " + recordsTRC$(80, 1) + " " + recordsTRC$(80, 2) + ", " + recordsTRC$(80, 3)
    Print #2,
    Print #2, "FEWEST REBOUNDS, GAME"
    Print #2, TotalRecords!(81, 0); "- "; recordsTRC$(81, 0) + " " + recordsTRC$(81, 1) + " " + recordsTRC$(81, 2) + ", " + recordsTRC$(81, 3)
    Print #2,
    Print #2, "MOST REBOUNDS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(82, 0); "- "; recordsTRC$(82, 0);
    Print #2, Using " (###) "; TotalRecords!(82, 1);: Print #2, recordsTRC$(82, 1) + " ";
    Print #2, recordsTRC$(82, 2);: Print #2, Using " (###)"; TotalRecords!(82, 2);: Print #2, ", " + recordsTRC$(82, 3)
    Print #2,
    Print #2, "FEWEST REBOUNDS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(83, 0); "- "; recordsTRC$(83, 0);
    Print #2, Using " (###) "; TotalRecords!(83, 1);: Print #2, recordsTRC$(83, 1) + " ";
    Print #2, recordsTRC$(83, 2);: Print #2, Using " (###)"; TotalRecords!(83, 2);: Print #2, ", " + recordsTRC$(83, 3)
    Print #2,
    Print #2, "MOST REBOUNDS, ONE HALF"
    Print #2, TotalRecords!(84, 0); "- "; recordsTRC$(84, 0) + " " + recordsTRC$(84, 1) + " " + recordsTRC$(84, 2) + ", " + recordsTRC$(84, 3); HALF$(TotalRecords!(84, 1))
    Print #2,
    Print #2, "MOST REBOUNDS, ONE QUARTER"
    Print #2, TotalRecords!(85, 0); "- "; recordsTRC$(85, 0) + " " + recordsTRC$(85, 1) + " " + recordsTRC$(85, 2) + ", " + recordsTRC$(85, 3); QTR$(TotalRecords!(85, 1))
    Print #2,
    Print #2,
    Print #2, "MOST OFFENSIVE REBOUNDS, GAME"
    Print #2, TotalRecords!(86, 0); "- "; recordsTRC$(86, 0) + " " + recordsTRC$(86, 1) + " " + recordsTRC$(86, 2) + ", " + recordsTRC$(86, 3)
    Print #2,
    Print #2, "MOST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(88, 0); "- "; recordsTRC$(88, 0);
    Print #2, Using " (###) "; TotalRecords!(88, 1);: Print #2, recordsTRC$(88, 1) + " ";
    Print #2, recordsTRC$(88, 2);: Print #2, Using " (###)"; TotalRecords!(88, 2);: Print #2, ", " + recordsTRC$(88, 3)
    Print #2,
    Print #2, "FEWEST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(89, 0); "- "; recordsTRC$(89, 0);
    Print #2, Using " (###) "; TotalRecords!(89, 1);: Print #2, recordsTRC$(89, 1) + " ";
    Print #2, recordsTRC$(89, 2);: Print #2, Using " (###)"; TotalRecords!(89, 2);: Print #2, ", " + recordsTRC$(89, 3)
    Print #2,
    Print #2, "MOST OFFENSIVE REBOUNDS, ONE HALF"
    Print #2, TotalRecords!(90, 0); "- "; recordsTRC$(90, 0) + " " + recordsTRC$(90, 1) + " " + recordsTRC$(90, 2) + ", " + recordsTRC$(90, 3); HALF$(TotalRecords!(90, 1))
    Print #2,
    Print #2, "MOST ASSISTS, GAME"
    Print #2, TotalRecords!(96, 0); "- "; recordsTRC$(96, 0) + " " + recordsTRC$(96, 1) + " " + recordsTRC$(96, 2) + ", " + recordsTRC$(96, 3)
    Print #2,
    Print #2, "FEWEST ASSISTS, GAME"
    Print #2, TotalRecords!(97, 0); "- "; recordsTRC$(97, 0) + " " + recordsTRC$(97, 1) + " " + recordsTRC$(97, 2) + ", " + recordsTRC$(97, 3)
    Print #2,
    Print #2, "MOST ASSISTS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(98, 0); "- "; recordsTRC$(98, 0);
    Print #2, Using " (###) "; TotalRecords!(98, 1);: Print #2, recordsTRC$(98, 1) + " ";
    Print #2, recordsTRC$(98, 2);: Print #2, Using " (###)"; TotalRecords!(98, 2);: Print #2, ", " + recordsTRC$(98, 3)
    Print #2,
    Print #2, "FEWEST ASSISTS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(99, 0); "- "; recordsTRC$(99, 0);
    Print #2, Using " (###) "; TotalRecords!(99, 1);: Print #2, recordsTRC$(99, 1) + " ";
    Print #2, recordsTRC$(99, 2);: Print #2, Using " (###)"; TotalRecords!(99, 2);: Print #2, ", " + recordsTRC$(99, 3)
    Print #2,
    Print #2, "MOST ASSISTS, ONE HALF"
    Print #2, TotalRecords!(100, 0); "- "; recordsTRC$(100, 0) + " " + recordsTRC$(100, 1) + " " + recordsTRC$(100, 2) + ", " + recordsTRC$(100, 3); HALF$(TotalRecords!(100, 1))
    Print #2,
    Print #2, "MOST ASSISTS, ONE QUARTER"
    Print #2, TotalRecords!(102, 0); "- "; recordsTRC$(102, 0) + " " + recordsTRC$(102, 1) + " " + recordsTRC$(102, 2) + ", " + recordsTRC$(102, 3); QTR$(TotalRecords!(102, 1))
    Print #2,
    Print #2, "MOST PERSONAL FOULS, GAME"
    Print #2, TotalRecords!(104, 0); "- "; recordsTRC$(104, 0) + " " + recordsTRC$(104, 1) + " " + recordsTRC$(104, 2) + ", " + recordsTRC$(104, 3)
    Print #2,
    Print #2, "FEWEST PERSONAL FOULS, GAME"
    Print #2, TotalRecords!(105, 0); "- "; recordsTRC$(105, 0) + " " + recordsTRC$(105, 1) + " " + recordsTRC$(105, 2) + ", " + recordsTRC$(105, 3)
    Print #2,
    Print #2, "MOST PERSONAL FOULS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(106, 0); "- "; recordsTRC$(106, 0);
    Print #2, Using " (###) "; TotalRecords!(106, 1);: Print #2, recordsTRC$(106, 1) + " ";
    Print #2, recordsTRC$(106, 2);: Print #2, Using " (###)"; TotalRecords!(106, 2);: Print #2, ", " + recordsTRC$(106, 3)
    Print #2,
    Print #2, "FEWEST PERSONAL FOULS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(107, 0); "- "; recordsTRC$(107, 0);
    Print #2, Using " (###) "; TotalRecords!(107, 1);: Print #2, recordsTRC$(107, 1) + " ";
    Print #2, recordsTRC$(107, 2);: Print #2, Using " (###)"; TotalRecords!(107, 2);: Print #2, ", " + recordsTRC$(107, 3)
    Print #2,
    Print #2, "MOST PERSONAL FOULS, ONE HALF"
    Print #2, TotalRecords!(108, 0); "- "; recordsTRC$(108, 0) + " " + recordsTRC$(108, 1) + " " + recordsTRC$(108, 2) + ", " + recordsTRC$(108, 3); HALF$(TotalRecords!(108, 1))
    Print #2,
    Print #2, "MOST PERSONAL FOULS, BOTH TEAMS, ONE HALF"
    Print #2, TotalRecords!(109, 0); "- "; recordsTRC$(109, 0); " "; recordsTRC$(109, 1); " "; recordsTRC$(109, 2); ", "; recordsTRC$(109, 3); HALF$(TotalRecords!(109, 1))
    Print #2,
    Print #2, "MOST PERSONAL FOULS, ONE QUARTER"
    Print #2, TotalRecords!(110, 0); "- "; recordsTRC$(110, 0) + " " + recordsTRC$(110, 1) + " " + recordsTRC$(110, 2) + ", " + recordsTRC$(110, 3); QTR$(TotalRecords!(110, 1))
    Print #2,
    Print #2, "MOST PERSONAL FOULS, BOTH TEAMS, ONE QUARTER"
    Print #2, TotalRecords!(111, 0); "- "; recordsTRC$(111, 0); " "; recordsTRC$(111, 1); " "; recordsTRC$(111, 2); ", "; recordsTRC$(111, 3); QTR$(TotalRecords!(111, 1));

    Print #2,
    Print #2, "MOST STEALS, GAME"
    Print #2, TotalRecords!(112, 0); "- "; recordsTRC$(112, 0) + " " + recordsTRC$(112, 1) + " " + recordsTRC$(112, 2) + ", " + recordsTRC$(112, 3)
    Print #2,
    Print #2, "MOST STEALS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(114, 0); "- "; recordsTRC$(114, 0);
    Print #2, Using " (###) "; TotalRecords!(114, 1);: Print #2, recordsTRC$(114, 1) + " ";
    Print #2, recordsTRC$(114, 2);: Print #2, Using " (###)"; TotalRecords!(114, 2);: Print #2, ", " + recordsTRC$(114, 3)
    Print #2,
    Print #2, "MOST STEALS, ONE HALF"
    Print #2, TotalRecords!(116, 0); "- "; recordsTRC$(116, 0) + " " + recordsTRC$(116, 1) + " " + recordsTRC$(116, 2) + ", " + recordsTRC$(116, 3); HALF$(TotalRecords!(116, 1))
    Print #2,
    Print #2, "MOST STEALS, ONE QUARTER"
    Print #2, TotalRecords!(117, 0); "- "; recordsTRC$(117, 0) + " " + recordsTRC$(117, 1) + " " + recordsTRC$(117, 2) + ", " + recordsTRC$(117, 3); QTR$(TotalRecords!(117, 1))
    Print #2,
    Print #2, "MOST BLOCKS, GAME"
    Print #2, TotalRecords!(118, 0); "- "; recordsTRC$(118, 0) + " " + recordsTRC$(118, 1) + " " + recordsTRC$(118, 2) + ", " + recordsTRC$(118, 3)
    Print #2,
    Print #2, "MOST BLOCKS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(119, 0); "- "; recordsTRC$(119, 0);
    Print #2, Using " (###) "; TotalRecords!(119, 1);: Print #2, recordsTRC$(119, 1) + " ";
    Print #2, recordsTRC$(119, 2);: Print #2, Using " (###)"; TotalRecords!(119, 2);: Print #2, ", " + recordsTRC$(119, 3)
    Print #2,
    Print #2, "MOST TURNOVERS, GAME"
    Print #2, TotalRecords!(122, 0); "- "; recordsTRC$(122, 0) + " " + recordsTRC$(122, 1) + " " + recordsTRC$(122, 2) + ", " + recordsTRC$(122, 3)
    Print #2,
    Print #2, "FEWEST TURNOVERS, GAME"
    Print #2, TotalRecords!(123, 0); "- "; recordsTRC$(123, 0) + " " + recordsTRC$(123, 1) + " " + recordsTRC$(123, 2) + ", " + recordsTRC$(123, 3)
    Print #2,
    Print #2, "MOST TURNOVERS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(124, 0); "- "; recordsTRC$(124, 0);
    Print #2, Using " (###) "; TotalRecords!(124, 1);: Print #2, recordsTRC$(124, 1) + " ";
    Print #2, recordsTRC$(124, 2);: Print #2, Using " (###)"; TotalRecords!(124, 2);: Print #2, ", " + recordsTRC$(124, 3)
    Print #2,
    Print #2, "FEWEST TURNOVERS, BOTH TEAMS, GAME"
    Print #2, TotalRecords!(125, 0); "- "; recordsTRC$(125, 0);
    Print #2, Using " (###) "; TotalRecords!(125, 1);: Print #2, recordsTRC$(125, 1) + " ";
    Print #2, recordsTRC$(125, 2);: Print #2, Using " (###)"; TotalRecords!(125, 2);: Print #2, ", " + recordsTRC$(125, 3)
    Close 2

    Open "RECORDS" For Input As #2
    Do While Not EOF(2)
        Line Input #2, X$
        LPrint X$
    Loop

    LPrint Chr$(12)
    	
	Close 2

End Sub
