'----------------------------------------
'         GameRecords Subroutine
'----------------------------------------
'This subroutine reads in the record file
'and gives the user an option of either
'viewing or printing the records.
Sub GameRecords (menuChoice)

    _Title "Basketball: The Pro Game - Game Records"

    Shared allRecords!(), teamRecords!()

    Shared indRecCategory$(), teamRecCategory$()

    If Not _FileExists (diskPaths$(1) + "RECORDS.GAM") Then

        MsgText$ = "UNABLE TO LOCATE RECORDS PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)
    
    Else

        Open diskPaths$(1) + "RECORDS.GAM" For Input As #1

        For I = 0 To 48
            For I1 = 0 To 2
                Input #1, allRecords!(I, I1)
            Next
            For I1 = 0 To 4
                Input #1, indRecCategory$(I, I1)
            Next
        Next

        For I = 0 To 125
            For I1 = 0 To 2
                Input #1, teamRecords!(I, I1)
            Next
            For I1 = 0 To 3
                Input #1, teamRecCategory$(I, I1)
            Next
        Next
        Close #1

        Select Case menuChoice

            Case 1:
                Call ViewGameRecords

            Case 2:
                Call PrintGameRecords

        End Select

    End If
    
End Sub


'----------------------------------------
'       ViewGameRecords Subroutine
'----------------------------------------
'This subroutine formats the records For
'output to screen and displays them.
Sub ViewGameRecords

    Shared allRecords!(), teamRecords!()

    Shared indRecCategory$(), teamRecCategory$()

    Dim HALF$(2), QTR$(4)

    HALF$(1) = " (1ST HALF)"
    HALF$(2) = " (2ND HALF)"
    QTR$(1) = " (1ST QTR)"
    QTR$(2) = " (2ND QTR)"
    QTR$(3) = " (3RD QTR)"
    QTR$(4) = " (4TH QTR)"

    Color 15, 1

    Cls
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST POINTS, GAME"
    Color 15
    Print allRecords!(0, 0); "- "; indRecCategory$(0, 0); ", "; indRecCategory$(0, 1) + " " + indRecCategory$(0, 2) + " " + indRecCategory$(0, 3) + ", " + indRecCategory$(0, 4)
    Print

    Color 14: Print "MOST POINTS, ONE HALF"
    Color 15
    Print allRecords!(1, 0); "- "; indRecCategory$(1, 0); ", "; indRecCategory$(1, 1) + " " + indRecCategory$(1, 2) + " " + indRecCategory$(1, 3) + ", " + indRecCategory$(1, 4); HALF$(allRecords!(1, 2))
    Print

    Color 14: Print "MOST POINTS, ONE QUARTER"
    Color 15
    Print allRecords!(2, 0); "- "; indRecCategory$(2, 0); ", "; indRecCategory$(2, 1) + " " + indRecCategory$(2, 2) + " " + indRecCategory$(2, 3) + ", " + indRecCategory$(2, 4); QTR$(allRecords!(2, 2))
    Print

    Color 14: Print "MOST POINTS, OVERTIME PERIOD"
    Color 15
    If allRecords!(3, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(3, 0); "- "; indRecCategory$(3, 0); ", "; indRecCategory$(3, 1) + " " + indRecCategory$(3, 2) + " " + indRecCategory$(3, 3) + ", " + indRecCategory$(3, 4)
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
        Print "- "; indRecCategory$(4, 0); ", "; indRecCategory$(4, 1) + " " + indRecCategory$(4, 2) + " " + indRecCategory$(4, 3) + ", " + indRecCategory$(4, 4);
        Print Using " (##/##)"; allRecords!(4, 1); allRecords!(4, 2)
    End If
    Print

    Color 14
    Print "MOST FIELD GOALS, NO MISSES, GAME"
    Color 15
    If allRecords!(5, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(5, 0); "- "; indRecCategory$(5, 0); ", "; indRecCategory$(5, 1) + " " + indRecCategory$(5, 2) + " " + indRecCategory$(5, 3) + ", " + indRecCategory$(5, 4)
    End If
    Print

    Color 14
    Print "MOST FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    Color 15
    If allRecords!(6, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(6, 0); "- "; indRecCategory$(6, 0); ", "; indRecCategory$(6, 1) + " " + indRecCategory$(6, 2) + " " + indRecCategory$(6, 3) + ", " + indRecCategory$(6, 4)
    End If

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST FIELD GOALS, GAME"
    Color 15
    Print allRecords!(7, 0); "- "; indRecCategory$(7, 0); ", "; indRecCategory$(7, 1) + " " + indRecCategory$(7, 2) + " " + indRecCategory$(7, 3) + ", " + indRecCategory$(7, 4)
    Print

    Color 14: Print "MOST FIELD GOALS, ONE HALF"
    Color 15
    Print allRecords!(8, 0); "- "; indRecCategory$(8, 0); ", "; indRecCategory$(8, 1) + " " + indRecCategory$(8, 2) + " " + indRecCategory$(8, 3) + ", " + indRecCategory$(8, 4); HALF$(allRecords!(8, 2))
    Print

    Color 14: Print "MOST FIELD GOALS, ONE QUARTER"
    Color 15
    Print allRecords!(9, 0); "- "; indRecCategory$(9, 0); ", "; indRecCategory$(9, 1) + " " + indRecCategory$(9, 2) + " " + indRecCategory$(9, 3) + ", " + indRecCategory$(9, 4); QTR$(allRecords!(9, 2))
    Print

    Color 14: Print "MOST FIELD GOALS, OVERTIME"
    Color 15
    If allRecords!(10, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(10, 0); "- "; indRecCategory$(10, 0); ", "; indRecCategory$(10, 1) + " " + indRecCategory$(10, 2) + " " + indRecCategory$(10, 3) + ", " + indRecCategory$(10, 4)
    End If
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, GAME"
    Color 15
    Print allRecords!(11, 0); "- "; indRecCategory$(11, 0); ", "; indRecCategory$(11, 1) + " " + indRecCategory$(11, 2) + " " + indRecCategory$(11, 3) + ", " + indRecCategory$(11, 4)
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Color 15
    Print allRecords!(12, 0); "- "; indRecCategory$(12, 0); ", "; indRecCategory$(12, 1) + " " + indRecCategory$(12, 2) + " " + indRecCategory$(12, 3) + ", " + indRecCategory$(12, 4); HALF$(allRecords!(12, 2))
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE QUARTER"
    Color 15
    Print allRecords!(13, 0); "- "; indRecCategory$(13, 0); ", "; indRecCategory$(13, 1) + " " + indRecCategory$(13, 2) + " " + indRecCategory$(13, 3) + ", " + indRecCategory$(13, 4); QTR$(allRecords!(13, 2))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    Color 15
    If allRecords!(15, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(15, 0); "- "; indRecCategory$(15, 0); ", "; indRecCategory$(15, 1) + " " + indRecCategory$(15, 2) + " " + indRecCategory$(15, 3) + ", " + indRecCategory$(15, 4)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    Color 15
    If allRecords!(16, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(16, 0); "- "; indRecCategory$(16, 0); ", "; indRecCategory$(16, 1) + " " + indRecCategory$(16, 2) + " " + indRecCategory$(16, 3) + ", " + indRecCategory$(16, 4)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, GAME"
    Color 15
    If allRecords!(17, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(17, 0); "- "; indRecCategory$(17, 0); ", "; indRecCategory$(17, 1) + " " + indRecCategory$(17, 2) + " " + indRecCategory$(17, 3) + ", " + indRecCategory$(17, 4)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS MADE, ONE HALF"
    Color 15
    If allRecords!(18, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(18, 0); "- "; indRecCategory$(18, 0); ", "; indRecCategory$(18, 1) + " " + indRecCategory$(18, 2) + " " + indRecCategory$(18, 3) + ", " + indRecCategory$(18, 4); HALF$(allRecords!(18, 2))
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS MADE, ONE QUARTER"
    Color 15
    If allRecords!(19, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(19, 0); "- "; indRecCategory$(19, 0); ", "; indRecCategory$(19, 1) + " " + indRecCategory$(19, 2) + " " + indRecCategory$(19, 3) + ", " + indRecCategory$(19, 4); QTR$(allRecords!(19, 2))
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    Color 15
    If allRecords!(21, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(21, 0); "- "; indRecCategory$(21, 0); ", "; indRecCategory$(21, 1) + " " + indRecCategory$(21, 2) + " " + indRecCategory$(21, 3) + ", " + indRecCategory$(21, 4)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    Color 15
    If allRecords!(22, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(22, 0); "- "; indRecCategory$(22, 0); ", "; indRecCategory$(22, 1) + " " + indRecCategory$(22, 2) + " " + indRecCategory$(22, 3) + ", " + indRecCategory$(22, 4); HALF$(allRecords!(22, 2))
    End If
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls

    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST FREE THROWS MADE, NO MISSES, GAME"
    Color 15
    If allRecords!(23, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(23, 0); "- "; indRecCategory$(23, 0); ", "; indRecCategory$(23, 1) + " " + indRecCategory$(23, 2) + " " + indRecCategory$(23, 3) + ", " + indRecCategory$(23, 4)
    End If
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, NONE MADE, GAME"
    Color 15
    If allRecords!(24, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(24, 0); "- "; indRecCategory$(24, 0); ", "; indRecCategory$(24, 1) + " " + indRecCategory$(24, 2) + " " + indRecCategory$(24, 3) + ", " + indRecCategory$(24, 4)
    End If
    Print

    Color 14: Print "MOST FREE THROWS MADE, GAME"
    Color 15
    If allRecords!(25, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(25, 0); "- "; indRecCategory$(25, 0); ", "; indRecCategory$(25, 1) + " " + indRecCategory$(25, 2) + " " + indRecCategory$(25, 3) + ", " + indRecCategory$(25, 4)
    End If
    Print

    Color 14: Print "MOST FREE THROWS MADE, ONE HALF"
    Color 15

    If allRecords!(26, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(26, 0); "- "; indRecCategory$(26, 0); ", "; indRecCategory$(26, 1) + " " + indRecCategory$(26, 2) + " " + indRecCategory$(26, 3) + ", " + indRecCategory$(26, 4); HALF$(allRecords!(26, 2))
    End If
    Print

    Color 14: Print "MOST FREE THROWS MADE, ONE QUARTER"
    Color 15

    If allRecords!(27, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(27, 0); "- "; indRecCategory$(27, 0); ", "; indRecCategory$(27, 1) + " " + indRecCategory$(27, 2) + " " + indRecCategory$(27, 3) + ", " + indRecCategory$(27, 4); QTR$(allRecords!(27, 2))
    End If
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, GAME"
    Color 15
    If allRecords!(28, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(28, 0); "- "; indRecCategory$(28, 0); ", "; indRecCategory$(28, 1) + " " + indRecCategory$(28, 2) + " " + indRecCategory$(28, 3) + ", " + indRecCategory$(28, 4)
    End If
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, ONE HALF"
    Color 15
    If allRecords!(29, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(29, 0); "- "; indRecCategory$(29, 0); ", "; indRecCategory$(29, 1) + " " + indRecCategory$(29, 2) + " " + indRecCategory$(29, 3) + ", " + indRecCategory$(29, 4); HALF$(allRecords!(29, 2))
    End If
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, ONE QUARTER"
    Color 15
    If allRecords!(30, 0) = 0 Then
        Print "NO RECORD AT THIS TIME";
    Else
        Print allRecords!(30, 0); "- "; indRecCategory$(30, 0); ", "; indRecCategory$(30, 1) + " " + indRecCategory$(30, 2) + " " + indRecCategory$(30, 3) + ", " + indRecCategory$(30, 4); QTR$(allRecords!(30, 2));
    End If

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST REBOUNDS, GAME"
    Color 15
    If allRecords!(31, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(31, 0); "- "; indRecCategory$(31, 0); ", "; indRecCategory$(31, 1) + " " + indRecCategory$(31, 2) + " " + indRecCategory$(31, 3) + ", " + indRecCategory$(31, 4)
    End If
    Print

    Color 14: Print "MOST REBOUNDS, ONE HALF"
    Color 15
    If allRecords!(32, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(32, 0); "- "; indRecCategory$(32, 0); ", "; indRecCategory$(32, 1) + " " + indRecCategory$(32, 2) + " " + indRecCategory$(32, 3) + ", " + indRecCategory$(32, 4); HALF$(allRecords!(32, 2))
    End If
    Print

    Color 14: Print "MOST REBOUNDS, ONE QUARTER"
    Color 15
    If allRecords!(33, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(33, 0); "- "; indRecCategory$(33, 0); ", "; indRecCategory$(33, 1) + " " + indRecCategory$(33, 2) + " " + indRecCategory$(33, 3) + ", " + indRecCategory$(33, 4); QTR$(allRecords!(33, 2))
    End If
    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, GAME"
    Color 15

    If allRecords!(34, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(34, 0); "- "; indRecCategory$(34, 0); ", "; indRecCategory$(34, 1) + " " + indRecCategory$(34, 2) + " " + indRecCategory$(34, 3) + ", " + indRecCategory$(34, 4)
    End If

    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, ONE HALF"
    Color 15
    If allRecords!(35, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(35, 0); "- "; indRecCategory$(35, 0); ", "; indRecCategory$(35, 1) + " " + indRecCategory$(35, 2) + " " + indRecCategory$(35, 3) + ", " + indRecCategory$(35, 4); HALF$(allRecords!(35, 2))
    End If
    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, ONE QUARTER"
    Color 15
    If allRecords!(36, 0) = 0 Then
        Print "NO RECORD AT THIS TIME";
    Else
        Print allRecords!(36, 0); "- "; indRecCategory$(36, 0); ", "; indRecCategory$(36, 1) + " " + indRecCategory$(36, 2) + " " + indRecCategory$(36, 3) + ", " + indRecCategory$(36, 4); QTR$(allRecords!(36, 2));
    End If

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST DEFENSIVE REBOUNDS, GAME"
    Color 15
    If allRecords!(37, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(37, 0); "- "; indRecCategory$(37, 0); ", "; indRecCategory$(37, 1) + " " + indRecCategory$(37, 2) + " " + indRecCategory$(37, 3) + ", " + indRecCategory$(37, 4)
    End If
    Print

    Color 14: Print "MOST DEFENSIVE REBOUNDS, ONE HALF"
    Color 15
    If allRecords!(38, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(38, 0); "- "; indRecCategory$(38, 0); ", "; indRecCategory$(38, 1) + " " + indRecCategory$(38, 2) + " " + indRecCategory$(38, 3) + ", " + indRecCategory$(38, 4); HALF$(allRecords!(38, 2))
    End If
    Print

    Color 14: Print "MOST DEFENSIVE REBOUNDS, ONE QUARTER"
    Color 15
    If allRecords!(39, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(39, 0); "- "; indRecCategory$(39, 0); ", "; indRecCategory$(39, 1) + " " + indRecCategory$(39, 2) + " " + indRecCategory$(39, 3) + ", " + indRecCategory$(39, 4); QTR$(allRecords!(39, 2))
    End If
    Print

    Color 14: Print "MOST ASSISTS, GAME"
    Color 15
    If allRecords!(40, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(40, 0); "- "; indRecCategory$(40, 0); ", "; indRecCategory$(40, 1) + " " + indRecCategory$(40, 2) + " " + indRecCategory$(40, 3) + ", " + indRecCategory$(40, 4)
    End If
    Print

    Color 14: Print "MOST ASSISTS, ONE HALF"
    Color 15
    If allRecords!(41, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(41, 0); "- "; indRecCategory$(41, 0); ", "; indRecCategory$(41, 1) + " " + indRecCategory$(41, 2) + " " + indRecCategory$(41, 3) + ", " + indRecCategory$(41, 4); HALF$(allRecords!(41, 2))
    End If
    Print

    Color 14: Print "MOST ASSISTS, ONE QUARTER"
    Color 15
    If allRecords!(42, 0) = 0 Then
        Print "NO RECORD AT THIS TIME";
    Else
        Print allRecords!(42, 0); "- "; indRecCategory$(42, 0); ", "; indRecCategory$(42, 1) + " " + indRecCategory$(42, 2) + " " + indRecCategory$(42, 3) + ", " + indRecCategory$(42, 4); QTR$(allRecords!(42, 2));
    End If

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST STEALS, GAME"
    Color 15
    If allRecords!(43, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(43, 0); "- "; indRecCategory$(43, 0); ", "; indRecCategory$(43, 1) + " " + indRecCategory$(43, 2) + " " + indRecCategory$(43, 3) + ", " + indRecCategory$(43, 4)
    End If
    Print

    Color 14: Print "MOST STEALS, ONE HALF"
    Color 15
    If allRecords!(44, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(44, 0); "- "; indRecCategory$(44, 0); ", "; indRecCategory$(44, 1) + " " + indRecCategory$(44, 2) + " " + indRecCategory$(44, 3) + ", " + indRecCategory$(44, 4); HALF$(allRecords!(44, 2))
    End If
    Print

    Color 14: Print "MOST STEALS, ONE QUARTER"
    Color 15
    If allRecords!(45, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(45, 0); "- "; indRecCategory$(45, 0); ", "; indRecCategory$(45, 1) + " " + indRecCategory$(45, 2) + " " + indRecCategory$(45, 3) + ", " + indRecCategory$(45, 4); QTR$(allRecords!(45, 2))
    End If
    Print

    Color 14: Print "MOST BLOCKS, GAME"
    Color 15
    If allRecords!(46, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(46, 0); "- "; indRecCategory$(46, 0); ", "; indRecCategory$(46, 1) + " " + indRecCategory$(46, 2) + " " + indRecCategory$(46, 3) + ", " + indRecCategory$(46, 4)
    End If
    Print

    Color 14: Print "MOST BLOCKS, ONE HALF"
    Color 15
    If allRecords!(47, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print allRecords!(47, 0); "- "; indRecCategory$(47, 0); ", "; indRecCategory$(47, 1) + " " + indRecCategory$(47, 2) + " " + indRecCategory$(47, 3) + ", " + indRecCategory$(47, 4); HALF$(allRecords!(47, 2))
    End If
    Print

    Color 14: Print "MOST BLOCKS, ONE QUARTER"
    Color 15
    If allRecords!(48, 0) = 0 Then
        Print "NO RECORD AT THIS TIME";
    Else
        Print allRecords!(48, 0); "- "; indRecCategory$(48, 0); ", "; indRecCategory$(48, 1) + " " + indRecCategory$(48, 2) + " " + indRecCategory$(48, 3) + ", " + indRecCategory$(48, 4); QTR$(allRecords!(48, 2));
    End If

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST OVERTIME PERIODS"
    Color 15
    If teamRecords!(0, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRecords!(0, 0); "- "; teamRecCategory$(0, 1);
        Print Using " (###) AT "; teamRecords!(0, 1);
        Print teamRecCategory$(0, 2);: Print Using " (###)"; teamRecords!(0, 2);: Print ", " + teamRecCategory$(0, 3)
    End If
    Print

    Color 14: Print "MOST POINTS, GAME"
    Color 15
    Print teamRecords!(1, 0); "- "; teamRecCategory$(1, 0) + " " + teamRecCategory$(1, 1) + " " + teamRecCategory$(1, 2) + ", " + teamRecCategory$(1, 3)
    Print

    Color 14: Print "FEWEST POINTS, GAME"
    Color 15
    Print teamRecords!(2, 0); "- "; teamRecCategory$(2, 0) + " " + teamRecCategory$(2, 1) + " " + teamRecCategory$(2, 2) + ", " + teamRecCategory$(2, 3)
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(3, 0); "- "; teamRecCategory$(3, 0);
    Print Using " (###) "; teamRecords!(3, 1);: Print teamRecCategory$(3, 1) + " ";
    Print teamRecCategory$(3, 2);: Print Using " (###)"; teamRecords!(3, 2);: Print ", " + teamRecCategory$(3, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(4, 0); "- "; teamRecCategory$(4, 0);
    Print Using " (###) "; teamRecords!(4, 1);: Print teamRecCategory$(4, 1) + " ";
    Print teamRecCategory$(4, 2);: Print Using " (###)"; teamRecords!(4, 2);: Print ", " + teamRecCategory$(4, 3)
    Print

    Color 14: Print "MOST POINTS, FIRST HALF"
    Color 15
    Print teamRecords!(6, 0); "- "; teamRecCategory$(6, 0) + " " + teamRecCategory$(6, 1) + " " + teamRecCategory$(6, 2) + ", " + teamRecCategory$(6, 3)
    Print

    Color 14: Print "FEWEST POINTS, FIRST HALF"
    Color 15
    Print teamRecords!(7, 0); "- "; teamRecCategory$(7, 0) + " " + teamRecCategory$(7, 1) + " " + teamRecCategory$(7, 2) + ", " + teamRecCategory$(7, 3)
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST POINTS, BOTH TEAMS, FIRST HALF"
    Color 15
    Print teamRecords!(8, 0); "- "; teamRecCategory$(8, 0);
    Print Using " (###) "; teamRecords!(8, 1);: Print teamRecCategory$(8, 1) + " ";
    Print teamRecCategory$(8, 2);: Print Using " (###)"; teamRecords!(8, 2);: Print ", " + teamRecCategory$(8, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, FIRST HALF"
    Color 15
    Print teamRecords!(9, 0); "- "; teamRecCategory$(9, 0);
    Print Using " (###) "; teamRecords!(9, 1);: Print teamRecCategory$(9, 1) + " ";
    Print teamRecCategory$(9, 2);: Print Using " (###)"; teamRecords!(9, 2);: Print ", " + teamRecCategory$(9, 3)
    Print

    Color 14: Print "MOST POINTS, SECOND HALF"
    Color 15
    Print teamRecords!(10, 0); "- "; teamRecCategory$(10, 0) + " " + teamRecCategory$(10, 1) + " " + teamRecCategory$(10, 2) + ", " + teamRecCategory$(10, 3)
    Print

    Color 14: Print "FEWEST POINTS, SECOND HALF"
    Color 15
    Print teamRecords!(11, 0); "- "; teamRecCategory$(11, 0) + " " + teamRecCategory$(11, 1) + " " + teamRecCategory$(11, 2) + ", " + teamRecCategory$(11, 3)
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, SECOND HALF"
    Color 15
    Print teamRecords!(12, 0); "- "; teamRecCategory$(12, 0);
    Print Using " (###) "; teamRecords!(12, 1);: Print teamRecCategory$(12, 1) + " ";
    Print teamRecCategory$(12, 2);: Print Using " (###)"; teamRecords!(12, 2);: Print ", " + teamRecCategory$(12, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, SECOND HALF"
    Color 15
    Print teamRecords!(13, 0); "- "; teamRecCategory$(13, 0);
    Print Using " (###) "; teamRecords!(13, 1);: Print teamRecCategory$(13, 1) + " ";
    Print teamRecCategory$(13, 2);: Print Using " (###)"; teamRecords!(13, 2);: Print ", " + teamRecCategory$(13, 3)
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST POINTS, FIRST QUARTER"
    Color 15
    Print teamRecords!(14, 0); "- "; teamRecCategory$(14, 0) + " " + teamRecCategory$(14, 1) + " " + teamRecCategory$(14, 2) + ", " + teamRecCategory$(14, 3)
    Print

    Color 14: Print "FEWEST POINTS, FIRST QUARTER"
    Color 15
    Print teamRecords!(15, 0); "- "; teamRecCategory$(15, 0) + " " + teamRecCategory$(15, 1) + " " + teamRecCategory$(15, 2) + ", " + teamRecCategory$(15, 3)
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, FIRST QUARTER"
    Color 15
    Print teamRecords!(16, 0); "- "; teamRecCategory$(16, 0);
    Print Using " (###) "; teamRecords!(16, 1);: Print teamRecCategory$(16, 1) + " ";
    Print teamRecCategory$(16, 2);: Print Using " (###)"; teamRecords!(16, 2);: Print ", " + teamRecCategory$(16, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, FIRST QUARTER"
    Color 15
    Print teamRecords!(17, 0); "- "; teamRecCategory$(17, 0);
    Print Using " (###) "; teamRecords!(17, 1);: Print teamRecCategory$(17, 1) + " ";
    Print teamRecCategory$(17, 2);: Print Using " (###)"; teamRecords!(17, 2);: Print ", " + teamRecCategory$(17, 3)
    Print

    Color 14: Print "MOST POINTS, SECOND QUARTER"
    Color 15
    Print teamRecords!(18, 0); "- "; teamRecCategory$(18, 0) + " " + teamRecCategory$(18, 1) + " " + teamRecCategory$(18, 2) + ", " + teamRecCategory$(18, 3)
    Print

    Color 14: Print "FEWEST POINTS, SECOND QUARTER"
    Color 15
    Print teamRecords!(19, 0); "- "; teamRecCategory$(19, 0) + " " + teamRecCategory$(19, 1) + " " + teamRecCategory$(19, 2) + ", " + teamRecCategory$(19, 3)
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, SECOND QUARTER"
    Color 15
    Print teamRecords!(20, 0); "- "; teamRecCategory$(20, 0);
    Print Using " (###) "; teamRecords!(20, 1);: Print teamRecCategory$(20, 1) + " ";
    Print teamRecCategory$(20, 2);: Print Using " (###)"; teamRecords!(20, 2);: Print ", " + teamRecCategory$(20, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, SECOND QUARTER"
    Color 15
    Print teamRecords!(21, 0); "- "; teamRecCategory$(21, 0);
    Print Using " (###) "; teamRecords!(21, 1);: Print teamRecCategory$(21, 1) + " ";
    Print teamRecCategory$(21, 2);: Print Using " (###)"; teamRecords!(21, 2);: Print ", " + teamRecCategory$(21, 3);

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST POINTS, THIRD QUARTER"
    Color 15
    Print teamRecords!(22, 0); "- "; teamRecCategory$(22, 0) + " " + teamRecCategory$(22, 1) + " " + teamRecCategory$(22, 2) + ", " + teamRecCategory$(22, 3)
    Print

    Color 14: Print "FEWEST POINTS, THIRD QUARTER"
    Color 15
    Print teamRecords!(23, 0); "- "; teamRecCategory$(23, 0) + " " + teamRecCategory$(23, 1) + " " + teamRecCategory$(23, 2) + ", " + teamRecCategory$(23, 3)
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, THIRD QUARTER"
    Color 15
    Print teamRecords!(24, 0); "- "; teamRecCategory$(24, 0);
    Print Using " (###) "; teamRecords!(24, 1);: Print teamRecCategory$(24, 1) + " ";
    Print teamRecCategory$(24, 2);: Print Using " (###)"; teamRecords!(24, 2);: Print ", " + teamRecCategory$(24, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, THIRD QUARTER"
    Color 15
    Print teamRecords!(25, 0); "- "; teamRecCategory$(25, 0);
    Print Using " (###) "; teamRecords!(25, 1);: Print teamRecCategory$(25, 1) + " ";
    Print teamRecCategory$(25, 2);: Print Using " (###)"; teamRecords!(25, 2);: Print ", " + teamRecCategory$(25, 3)
    Print

    Color 14: Print "MOST POINTS, FOURTH QUARTER"
    Color 15
    Print teamRecords!(26, 0); "- "; teamRecCategory$(26, 0) + " " + teamRecCategory$(26, 1) + " " + teamRecCategory$(26, 2) + ", " + teamRecCategory$(26, 3)
    Print

    Color 14: Print "FEWEST POINTS, FOURTH QUARTER"
    Color 15
    Print teamRecords!(27, 0); "- "; teamRecCategory$(27, 0) + " " + teamRecCategory$(27, 1) + " " + teamRecCategory$(27, 2) + ", " + teamRecCategory$(27, 3)
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, FOURTH QUARTER"
    Color 15
    Print teamRecords!(28, 0); "- "; teamRecCategory$(28, 0);
    Print Using " (###) "; teamRecords!(28, 1);: Print teamRecCategory$(28, 1) + " ";
    Print teamRecCategory$(28, 2);: Print Using " (###)"; teamRecords!(28, 2);: Print ", " + teamRecCategory$(28, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, FOURTH QUARTER"
    Color 15
    Print teamRecords!(29, 0); "- "; teamRecCategory$(29, 0);
    Print Using " (###) "; teamRecords!(29, 1);: Print teamRecCategory$(29, 1) + " ";
    Print teamRecCategory$(29, 2);: Print Using " (###)"; teamRecords!(29, 2);: Print ", " + teamRecCategory$(29, 3);

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, GAME"
    Color 15
    Print Using ".### "; teamRecords!(30, 0);: Print "- "; teamRecCategory$(30, 0) + " " + teamRecCategory$(30, 1) + " " + teamRecCategory$(30, 2) + ", " + teamRecCategory$(30, 3);: Print Using "###/###"; teamRecords!(30, 1); teamRecords!(30, 2)
    Print

    Color 14: Print "LOWEST FIELD GOAL PERCENTAGE, GAME"
    Color 15
    Print Using ".### "; teamRecords!(31, 0);: Print "- "; teamRecCategory$(31, 0) + " " + teamRecCategory$(31, 1) + " " + teamRecCategory$(31, 2) + ", " + teamRecCategory$(31, 3);: Print Using "###/###"; teamRecords!(31, 1); teamRecords!(31, 2)
    Print

    Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Color 15
    Print Using ".### "; teamRecords!(32, 0);: Print "- "; teamRecCategory$(32, 0);
    Print Using " (.###) "; teamRecords!(32, 1);: Print teamRecCategory$(32, 1) + " ";
    Print teamRecCategory$(32, 2);: Print Using " (.###)"; teamRecords!(32, 2);: Print ", " + teamRecCategory$(32, 3)
    Print

    Color 14: Print "LOWEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Color 15
    Print Using ".### "; teamRecords!(33, 0);: Print "- "; teamRecCategory$(33, 0);
    Print Using " (.###) "; teamRecords!(33, 1);: Print teamRecCategory$(33, 1) + " ";
    Print teamRecCategory$(33, 2);: Print Using " (.###)"; teamRecords!(33, 2);: Print ", " + teamRecCategory$(33, 3)
    Print

    Color 14: Print "MOST FIELD GOALS, GAME"
    Color 15
    Print teamRecords!(34, 0); "- "; teamRecCategory$(34, 0) + " " + teamRecCategory$(34, 1) + " " + teamRecCategory$(34, 2) + ", " + teamRecCategory$(34, 3)
    Print

    Color 14: Print "FEWEST FIELD GOALS, GAME"
    Color 15
    Print teamRecords!(35, 0); "- "; teamRecCategory$(35, 0) + " " + teamRecCategory$(35, 1) + " " + teamRecCategory$(35, 2) + ", " + teamRecCategory$(35, 3)
    Print

    Color 14: Print "MOST FIELD GOALS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(36, 0); "- "; teamRecCategory$(36, 0);
    Print Using " (###) "; teamRecords!(36, 1);: Print teamRecCategory$(36, 1) + " ";
    Print teamRecCategory$(36, 2);: Print Using " (###)"; teamRecords!(36, 2);: Print ", " + teamRecCategory$(36, 3)
    Print

    Color 14: Print "FEWEST FIELD GOALS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(37, 0); "- "; teamRecCategory$(37, 0);
    Print Using " (###) "; teamRecords!(37, 1);: Print teamRecCategory$(37, 1) + " ";
    Print teamRecCategory$(37, 2);: Print Using " (###)"; teamRecords!(37, 2);: Print ", " + teamRecCategory$(37, 3);

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST FIELD GOALS, ONE HALF"
    Color 15
    Print teamRecords!(38, 0); "- "; teamRecCategory$(38, 0) + " " + teamRecCategory$(38, 1) + " " + teamRecCategory$(38, 2) + ", " + teamRecCategory$(38, 3); HALF$(teamRecords!(38, 1))
    Print

    Color 14: Print "MOST FIELD GOALS, BOTH TEAMS, ONE HALF"
    Color 15
    Print teamRecords!(39, 0); "- "; teamRecCategory$(39, 0); " "; teamRecCategory$(39, 1); " "; teamRecCategory$(39, 2); ", "; teamRecCategory$(39, 3); HALF$(teamRecords!(39, 1))
    Print

    Color 14: Print "MOST FIELD GOALS, ONE QUARTER"
    Color 15
    Print teamRecords!(40, 0); "- "; teamRecCategory$(40, 0) + " " + teamRecCategory$(40, 1) + " " + teamRecCategory$(40, 2) + ", " + teamRecCategory$(40, 3); QTR$(teamRecords!(40, 1))
    Print

    Color 14: Print "MOST FIELD GOALS, BOTH TEAMS, ONE QUARTER"
    Color 15
    Print teamRecords!(41, 0); "- "; teamRecCategory$(41, 0); " "; teamRecCategory$(41, 1); " "; teamRecCategory$(41, 2); ", "; teamRecCategory$(41, 3); QTR$(teamRecords!(41, 1))
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, GAME"
    Color 15
    Print teamRecords!(42, 0); "- "; teamRecCategory$(42, 0) + " " + teamRecCategory$(42, 1) + " " + teamRecCategory$(42, 2) + ", " + teamRecCategory$(42, 3)
    Print

    Color 14: Print "FEWEST FIELD GOAL ATTEMPTS, GAME"
    Color 15
    Print teamRecords!(43, 0); "- "; teamRecCategory$(43, 0) + " " + teamRecCategory$(43, 1) + " " + teamRecCategory$(43, 2) + ", " + teamRecCategory$(43, 3)
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(44, 0); "- "; teamRecCategory$(44, 0);
    Print Using " (###) "; teamRecords!(44, 1);: Print teamRecCategory$(44, 1) + " ";
    Print teamRecCategory$(44, 2);: Print Using " (###)"; teamRecords!(44, 2);: Print ", " + teamRecCategory$(44, 3)
    Print

    Color 14: Print "FEWEST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(45, 0); "- "; teamRecCategory$(45, 0);
    Print Using " (###) "; teamRecords!(45, 1);: Print teamRecCategory$(45, 1) + " ";
    Print teamRecCategory$(45, 2);: Print Using " (###)"; teamRecords!(45, 2);: Print ", " + teamRecCategory$(45, 3);

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Color 15
    Print teamRecords!(46, 0); "- "; teamRecCategory$(46, 0) + " " + teamRecCategory$(46, 1) + " " + teamRecCategory$(46, 2) + ", " + teamRecCategory$(46, 3); HALF$(teamRecords!(46, 1))
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE HALF"
    Color 15
    Print teamRecords!(47, 0); "- "; teamRecCategory$(47, 0); " "; teamRecCategory$(47, 1); " "; teamRecCategory$(47, 2); ", "; teamRecCategory$(47, 3); HALF$(teamRecords!(47, 1))
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE QUARTER"
    Color 15
    Print teamRecords!(48, 0); "- "; teamRecCategory$(48, 0) + " " + teamRecCategory$(48, 1) + " " + teamRecCategory$(48, 2) + ", " + teamRecCategory$(48, 3); QTR$(teamRecords!(48, 1))
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE QUARTER"
    Color 15
    Print teamRecords!(49, 0); "- "; teamRecCategory$(49, 0); " "; teamRecCategory$(49, 1); " "; teamRecCategory$(49, 2); ", "; teamRecCategory$(49, 3); QTR$(teamRecords!(49, 1))
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    Color 15
    If teamRecords!(50, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRecords!(50, 0); "- "; teamRecCategory$(50, 0) + " " + teamRecCategory$(50, 1) + " " + teamRecCategory$(50, 2) + ", " + teamRecCategory$(50, 3)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, BOTH TEAMS, GAME"
    Color 15
    If teamRecords!(51, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRecords!(51, 0); "- "; teamRecCategory$(51, 0);
        Print Using " (###) "; teamRecords!(51, 1);: Print teamRecCategory$(51, 1) + " ";
        Print teamRecCategory$(51, 2);: Print Using " (###)"; teamRecords!(51, 2);: Print ", " + teamRecCategory$(51, 3)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    Color 15
    If teamRecords!(52, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRecords!(52, 0); "- "; teamRecCategory$(52, 0) + " " + teamRecCategory$(52, 1) + " " + teamRecCategory$(52, 2) + ", " + teamRecCategory$(52, 3)
    End If
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST THREE-POINT FIELD GOALS, GAME"
    Color 15
    Print teamRecords!(53, 0); "- "; teamRecCategory$(53, 0) + " " + teamRecCategory$(53, 1) + " " + teamRecCategory$(53, 2) + ", " + teamRecCategory$(53, 3)
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(54, 0); "- "; teamRecCategory$(54, 0);
    Print Using " (###) "; teamRecords!(54, 1);: Print teamRecCategory$(54, 1) + " ";
    Print teamRecCategory$(54, 2);: Print Using " (###)"; teamRecords!(54, 2);: Print ", " + teamRecCategory$(54, 3)
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, ONE HALF"
    Color 15
    Print teamRecords!(55, 0); "- "; teamRecCategory$(55, 0) + " " + teamRecCategory$(55, 1) + " " + teamRecCategory$(55, 2) + ", " + teamRecCategory$(55, 3); HALF$(teamRecords!(55, 1))
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, ONE QUARTER"
    Color 15
    Print teamRecords!(56, 0); "- "; teamRecCategory$(56, 0) + " " + teamRecCategory$(56, 1) + " " + teamRecCategory$(56, 2) + ", " + teamRecCategory$(56, 3); QTR$(teamRecords!(56, 1))
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    Color 15
    Print teamRecords!(57, 0); "- "; teamRecCategory$(57, 0) + " " + teamRecCategory$(57, 1) + " " + teamRecCategory$(57, 2) + ", " + teamRecCategory$(57, 3)
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(58, 0); "- "; teamRecCategory$(58, 0);
    Print Using " (###) "; teamRecords!(58, 1);: Print teamRecCategory$(58, 1) + " ";
    Print teamRecCategory$(58, 2);: Print Using " (###)"; teamRecords!(58, 2);: Print ", " + teamRecCategory$(58, 3)
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    Color 15
    Print teamRecords!(59, 0); "- "; teamRecCategory$(59, 0) + " " + teamRecCategory$(59, 1) + " " + teamRecCategory$(59, 2) + ", " + teamRecCategory$(59, 3); HALF$(teamRecords!(59, 1))

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST FREE THROWS MADE, NO MISSES, GAME"
    Color 15
    If teamRecords!(60, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRecords!(60, 0); "- "; teamRecCategory$(60, 0) + " " + teamRecCategory$(60, 1) + " " + teamRecCategory$(60, 2) + ", " + teamRecCategory$(60, 3)
    End If
    Print

    Color 14: Print "LOWEST FREE THROW PERCENTAGE, GAME"
    Color 15
    Print Using ".### "; teamRecords!(61, 0);: Print "- "; teamRecCategory$(61, 0) + " " + teamRecCategory$(61, 1) + " " + teamRecCategory$(61, 2) + ", " + teamRecCategory$(61, 3);: Print Using "###/###"; teamRecords!(61, 1); teamRecords!(61, 2)
    Print

    Color 14: Print "HIGHEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Color 15
    Print Using ".### "; teamRecords!(62, 0);: Print "- "; teamRecCategory$(62, 0);
    Print Using " (.###) "; teamRecords!(62, 1);: Print teamRecCategory$(62, 1) + " ";
    Print teamRecCategory$(62, 2);: Print Using " (.###)"; teamRecords!(62, 2);: Print ", " + teamRecCategory$(62, 3)
    Print

    Color 14: Print "LOWEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Color 15
    Print Using ".### "; teamRecords!(63, 0);: Print "- "; teamRecCategory$(63, 0);
    Print Using " (.###) "; teamRecords!(63, 1);: Print teamRecCategory$(63, 1) + " ";
    Print teamRecCategory$(63, 2);: Print Using " (.###)"; teamRecords!(63, 2);: Print ", " + teamRecCategory$(63, 3)
    Print

    Color 14: Print "MOST FREE THROWS MADE, GAME"
    Color 15
    Print teamRecords!(64, 0); "- "; teamRecCategory$(64, 0) + " " + teamRecCategory$(64, 1) + " " + teamRecCategory$(64, 2) + ", " + teamRecCategory$(64, 3)
    Print

    Color 14: Print "FEWEST FREE THROWS MADE, GAME"
    Color 15
    Print teamRecords!(65, 0); "- "; teamRecCategory$(65, 0) + " " + teamRecCategory$(65, 1) + " " + teamRecCategory$(65, 2) + ", " + teamRecCategory$(65, 3)
    Print

    Color 14: Print "MOST FREE THROWS MADE, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(66, 0); "- "; teamRecCategory$(66, 0);
    Print Using " (###) "; teamRecords!(66, 1);: Print teamRecCategory$(66, 1) + " ";
    Print teamRecCategory$(66, 2);: Print Using " (###)"; teamRecords!(66, 2);: Print ", " + teamRecCategory$(66, 3)
    Print

    Color 14: Print "FEWEST FREE THROWS MADE, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(67, 0); "- "; teamRecCategory$(67, 0);
    Print Using " (###) "; teamRecords!(67, 1);: Print teamRecCategory$(67, 1) + " ";
    Print teamRecCategory$(67, 2);: Print Using " (###)"; teamRecords!(67, 2);: Print ", " + teamRecCategory$(67, 3);

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST FREE THROWS MADE, ONE HALF"
    Color 15
    Print teamRecords!(68, 0); "- "; teamRecCategory$(68, 0) + " " + teamRecCategory$(68, 1) + " " + teamRecCategory$(68, 2) + ", " + teamRecCategory$(68, 3); HALF$(teamRecords!(68, 1))
    Print

    Color 14: Print "MOST FREE THROWS MADE, ONE QUARTER"
    Color 15
    Print teamRecords!(70, 0); "- "; teamRecCategory$(70, 0) + " " + teamRecCategory$(70, 1) + " " + teamRecCategory$(70, 2) + ", " + teamRecCategory$(70, 3); QTR$(teamRecords!(70, 1))
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, GAME"
    Color 15
    Print teamRecords!(72, 0); "- "; teamRecCategory$(72, 0) + " " + teamRecCategory$(72, 1) + " " + teamRecCategory$(72, 2) + ", " + teamRecCategory$(72, 3)
    Print

    Color 14: Print "FEWEST FREE THROW ATTEMPTS, GAME"
    Color 15
    Print teamRecords!(73, 0); "- "; teamRecCategory$(73, 0) + " " + teamRecCategory$(73, 1) + " " + teamRecCategory$(73, 2) + ", " + teamRecCategory$(73, 3)
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(74, 0); "- "; teamRecCategory$(74, 0);
    Print Using " (###) "; teamRecords!(74, 1);: Print teamRecCategory$(74, 1) + " ";
    Print teamRecCategory$(74, 2);: Print Using " (###)"; teamRecords!(74, 2);: Print ", " + teamRecCategory$(74, 3)
    Print

    Color 14: Print "FEWEST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(75, 0); "- "; teamRecCategory$(75, 0);
    Print Using " (###) "; teamRecords!(75, 1);: Print teamRecCategory$(75, 1) + " ";
    Print teamRecCategory$(75, 2);: Print Using " (###)"; teamRecords!(75, 2);: Print ", " + teamRecCategory$(75, 3)
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, ONE HALF"
    Color 15
    Print teamRecords!(76, 0); "- "; teamRecCategory$(76, 0) + " " + teamRecCategory$(76, 1) + " " + teamRecCategory$(76, 2) + ", " + teamRecCategory$(76, 3); HALF$(teamRecords!(76, 1))
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, ONE QUARTER"
    Color 15
    Print teamRecords!(78, 0); "- "; teamRecCategory$(78, 0) + " " + teamRecCategory$(78, 1) + " " + teamRecCategory$(78, 2) + ", " + teamRecCategory$(78, 3); QTR$(teamRecords!(78, 1));

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST REBOUNDS, GAME"
    Color 15
    Print teamRecords!(80, 0); "- "; teamRecCategory$(80, 0) + " " + teamRecCategory$(80, 1) + " " + teamRecCategory$(80, 2) + ", " + teamRecCategory$(80, 3)
    Print

    Color 14: Print "FEWEST REBOUNDS, GAME"
    Color 15
    Print teamRecords!(81, 0); "- "; teamRecCategory$(81, 0) + " " + teamRecCategory$(81, 1) + " " + teamRecCategory$(81, 2) + ", " + teamRecCategory$(81, 3)
    Print

    Color 14: Print "MOST REBOUNDS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(82, 0); "- "; teamRecCategory$(82, 0);
    Print Using " (###) "; teamRecords!(82, 1);: Print teamRecCategory$(82, 1) + " ";
    Print teamRecCategory$(82, 2);: Print Using " (###)"; teamRecords!(82, 2);: Print ", " + teamRecCategory$(82, 3)
    Print

    Color 14: Print "FEWEST REBOUNDS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(83, 0); "- "; teamRecCategory$(83, 0);
    Print Using " (###) "; teamRecords!(83, 1);: Print teamRecCategory$(83, 1) + " ";
    Print teamRecCategory$(83, 2);: Print Using " (###)"; teamRecords!(83, 2);: Print ", " + teamRecCategory$(83, 3)
    Print

    Color 14: Print "MOST REBOUNDS, ONE HALF"
    Color 15
    Print teamRecords!(84, 0); "- "; teamRecCategory$(84, 0) + " " + teamRecCategory$(84, 1) + " " + teamRecCategory$(84, 2) + ", " + teamRecCategory$(84, 3); HALF$(teamRecords!(84, 1))
    Print

    Color 14: Print "MOST REBOUNDS, ONE QUARTER"
    Color 15
    Print teamRecords!(85, 0); "- "; teamRecCategory$(85, 0) + " " + teamRecCategory$(85, 1) + " " + teamRecCategory$(85, 2) + ", " + teamRecCategory$(85, 3); QTR$(teamRecords!(85, 1))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST OFFENSIVE REBOUNDS, GAME"
    Color 15
    Print teamRecords!(86, 0); "- "; teamRecCategory$(86, 0) + " " + teamRecCategory$(86, 1) + " " + teamRecCategory$(86, 2) + ", " + teamRecCategory$(86, 3)
    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(88, 0); "- "; teamRecCategory$(88, 0);
    Print Using " (###) "; teamRecords!(88, 1);: Print teamRecCategory$(88, 1) + " ";
    Print teamRecCategory$(88, 2);: Print Using " (###)"; teamRecords!(88, 2);: Print ", " + teamRecCategory$(88, 3)
    Print

    Color 14: Print "FEWEST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(89, 0); "- "; teamRecCategory$(89, 0);
    Print Using " (###) "; teamRecords!(89, 1);: Print teamRecCategory$(89, 1) + " ";
    Print teamRecCategory$(89, 2);: Print Using " (###)"; teamRecords!(89, 2);: Print ", " + teamRecCategory$(89, 3)
    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, ONE HALF"
    Color 15
    Print teamRecords!(90, 0); "- "; teamRecCategory$(90, 0) + " " + teamRecCategory$(90, 1) + " " + teamRecCategory$(90, 2) + ", " + teamRecCategory$(90, 3); HALF$(teamRecords!(90, 1))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST ASSISTS, GAME"
    Color 15
    Print teamRecords!(96, 0); "- "; teamRecCategory$(96, 0) + " " + teamRecCategory$(96, 1) + " " + teamRecCategory$(96, 2) + ", " + teamRecCategory$(96, 3)
    Print

    Color 14: Print "FEWEST ASSISTS, GAME"
    Color 15
    Print teamRecords!(97, 0); "- "; teamRecCategory$(97, 0) + " " + teamRecCategory$(97, 1) + " " + teamRecCategory$(97, 2) + ", " + teamRecCategory$(97, 3)
    Print

    Color 14: Print "MOST ASSISTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(98, 0); "- "; teamRecCategory$(98, 0);
    Print Using " (###) "; teamRecords!(98, 1);: Print teamRecCategory$(98, 1) + " ";
    Print teamRecCategory$(98, 2);: Print Using " (###)"; teamRecords!(98, 2);: Print ", " + teamRecCategory$(98, 3)
    Print

    Color 14: Print "FEWEST ASSISTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(99, 0); "- "; teamRecCategory$(99, 0);
    Print Using " (###) "; teamRecords!(99, 1);: Print teamRecCategory$(99, 1) + " ";
    Print teamRecCategory$(99, 2);: Print Using " (###)"; teamRecords!(99, 2);: Print ", " + teamRecCategory$(99, 3)
    Print

    Color 14: Print "MOST ASSISTS, ONE HALF"
    Color 15
    Print teamRecords!(100, 0); "- "; teamRecCategory$(100, 0) + " " + teamRecCategory$(100, 1) + " " + teamRecCategory$(100, 2) + ", " + teamRecCategory$(100, 3); HALF$(teamRecords!(100, 1))
    Print

    Color 14: Print "MOST ASSISTS, ONE QUARTER"
    Color 15
    Print teamRecords!(102, 0); "- "; teamRecCategory$(102, 0) + " " + teamRecCategory$(102, 1) + " " + teamRecCategory$(102, 2) + ", " + teamRecCategory$(102, 3); QTR$(teamRecords!(102, 1))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST PERSONAL FOULS, GAME"
    Color 15
    Print teamRecords!(104, 0); "- "; teamRecCategory$(104, 0) + " " + teamRecCategory$(104, 1) + " " + teamRecCategory$(104, 2) + ", " + teamRecCategory$(104, 3)
    Print

    Color 14: Print "FEWEST PERSONAL FOULS, GAME"
    Color 15
    Print teamRecords!(105, 0); "- "; teamRecCategory$(105, 0) + " " + teamRecCategory$(105, 1) + " " + teamRecCategory$(105, 2) + ", " + teamRecCategory$(105, 3)
    Print

    Color 14: Print "MOST PERSONAL FOULS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(106, 0); "- "; teamRecCategory$(106, 0);
    Print Using " (###) "; teamRecords!(106, 1);: Print teamRecCategory$(106, 1) + " ";
    Print teamRecCategory$(106, 2);: Print Using " (###)"; teamRecords!(106, 2);: Print ", " + teamRecCategory$(106, 3)
    Print

    Color 14: Print "FEWEST PERSONAL FOULS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(107, 0); "- "; teamRecCategory$(107, 0);
    Print Using " (###) "; teamRecords!(107, 1);: Print teamRecCategory$(107, 1) + " ";
    Print teamRecCategory$(107, 2);: Print Using " (###)"; teamRecords!(107, 2);: Print ", " + teamRecCategory$(107, 3)
    Print

    Color 14: Print "MOST PERSONAL FOULS, ONE HALF"
    Color 15
    Print teamRecords!(108, 0); "- "; teamRecCategory$(108, 0) + " " + teamRecCategory$(108, 1) + " " + teamRecCategory$(108, 2) + ", " + teamRecCategory$(108, 3); HALF$(teamRecords!(108, 1))
    Print

    Color 14: Print "MOST PERSONAL FOULS, BOTH TEAMS, ONE HALF"
    Color 15
    Print teamRecords!(109, 0); "- "; teamRecCategory$(109, 0); " "; teamRecCategory$(109, 1); " "; teamRecCategory$(109, 2); ", "; teamRecCategory$(109, 3); HALF$(teamRecords!(109, 1))
    Print

    Color 14: Print "MOST PERSONAL FOULS, ONE QUARTER"
    Color 15
    Print teamRecords!(110, 0); "- "; teamRecCategory$(110, 0) + " " + teamRecCategory$(110, 1) + " " + teamRecCategory$(110, 2) + ", " + teamRecCategory$(110, 3); QTR$(teamRecords!(110, 1))
    Print

    Color 14: Print "MOST PERSONAL FOULS, BOTH TEAMS, ONE QUARTER"
    Color 15
    Print teamRecords!(111, 0); "- "; teamRecCategory$(111, 0); " "; teamRecCategory$(111, 1); " "; teamRecCategory$(111, 2); ", "; teamRecCategory$(111, 3); QTR$(teamRecords!(111, 1));

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST STEALS, GAME"
    Color 15
    Print teamRecords!(112, 0); "- "; teamRecCategory$(112, 0) + " " + teamRecCategory$(112, 1) + " " + teamRecCategory$(112, 2) + ", " + teamRecCategory$(112, 3)
    Print

    Color 14: Print "MOST STEALS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(114, 0); "- "; teamRecCategory$(114, 0);
    Print Using " (###) "; teamRecords!(114, 1);: Print teamRecCategory$(114, 1) + " ";
    Print teamRecCategory$(114, 2);: Print Using " (###)"; teamRecords!(114, 2);: Print ", " + teamRecCategory$(114, 3)
    Print

    Color 14: Print "MOST STEALS, ONE HALF"
    Color 15
    Print teamRecords!(116, 0); "- "; teamRecCategory$(116, 0) + " " + teamRecCategory$(116, 1) + " " + teamRecCategory$(116, 2) + ", " + teamRecCategory$(116, 3); HALF$(teamRecords!(116, 1))
    Print

    Color 14: Print "MOST STEALS, ONE QUARTER"
    Color 15
    Print teamRecords!(117, 0); "- "; teamRecCategory$(117, 0) + " " + teamRecCategory$(117, 1) + " " + teamRecCategory$(117, 2) + ", " + teamRecCategory$(117, 3); QTR$(teamRecords!(117, 1))
    Print

    Color 14: Print "MOST BLOCKS, GAME"
    Color 15
    Print teamRecords!(118, 0); "- "; teamRecCategory$(118, 0) + " " + teamRecCategory$(118, 1) + " " + teamRecCategory$(118, 2) + ", " + teamRecCategory$(118, 3)
    Print

    Color 14: Print "MOST BLOCKS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(119, 0); "- "; teamRecCategory$(119, 0);
    Print Using " (###) "; teamRecords!(119, 1);: Print teamRecCategory$(119, 1) + " ";
    Print teamRecCategory$(119, 2);: Print Using " (###)"; teamRecords!(119, 2);: Print ", " + teamRecCategory$(119, 3)
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST TURNOVERS, GAME"
    Color 15
    Print teamRecords!(122, 0); "- "; teamRecCategory$(122, 0) + " " + teamRecCategory$(122, 1) + " " + teamRecCategory$(122, 2) + ", " + teamRecCategory$(122, 3)
    Print

    Color 14: Print "FEWEST TURNOVERS, GAME"
    Color 15
    Print teamRecords!(123, 0); "- "; teamRecCategory$(123, 0) + " " + teamRecCategory$(123, 1) + " " + teamRecCategory$(123, 2) + ", " + teamRecCategory$(123, 3)
    Print

    Color 14: Print "MOST TURNOVERS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(124, 0); "- "; teamRecCategory$(124, 0);
    Print Using " (###) "; teamRecords!(124, 1);: Print teamRecCategory$(124, 1) + " ";
    Print teamRecCategory$(124, 2);: Print Using " (###)"; teamRecords!(124, 2);: Print ", " + teamRecCategory$(124, 3)
    Print

    Color 14: Print "FEWEST TURNOVERS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(125, 0); "- "; teamRecCategory$(125, 0);
    Print Using " (###) "; teamRecords!(125, 1);: Print teamRecCategory$(125, 1) + " ";
    Print teamRecCategory$(125, 2);: Print Using " (###)"; teamRecords!(125, 2);: Print ", " + teamRecCategory$(125, 3)
    Print

    I$ = GetKeyPress$

End Sub

'----------------------------------------
'      PrintGameRecords Subroutine
'----------------------------------------
'This subroutine formats and prints the 
'records on the default printer
Sub PrintGameRecords ()

    Shared indRecCategory$(), teamRecCategory$()
    Shared allRecords!(), teamRecords!()

    Dim HALF$(2), QTR$(4)

    HALF$(1) = " (1ST HALF)"
    HALF$(2) = " (2ND HALF)"
    QTR$(1) = " (1ST QTR)"
    QTR$(2) = " (2ND QTR)"
    QTR$(3) = " (3RD QTR)"
    QTR$(4) = " (4TH QTR)"

    Color 0, 15

    'PRINT ROUTINE
    Open "RECORDS" For Output As #1
    Print #1, "                            INDIVIDUAL RECORDS"
    Print #1,

    Print #1, "MOST POINTS, GAME"
    Print #1, allRecords!(0, 0); "- "; indRecCategory$(0, 0); ", "; indRecCategory$(0, 1) + " " + indRecCategory$(0, 2) + " " + indRecCategory$(0, 3) + ", " + indRecCategory$(0, 4)
    Print #1,

    Print #1, "MOST POINTS, ONE HALF"
    Print #1, allRecords!(1, 0); "- "; indRecCategory$(1, 0); ", "; indRecCategory$(1, 1) + " " + indRecCategory$(1, 2) + " " + indRecCategory$(1, 3) + ", " + indRecCategory$(1, 4); HALF$(allRecords!(1, 2))
    Print #1,

    Print #1, "MOST POINTS, ONE QUARTER"
    Print #1, allRecords!(2, 0); "- "; indRecCategory$(2, 0); ", "; indRecCategory$(2, 1) + " " + indRecCategory$(2, 2) + " " + indRecCategory$(2, 3) + ", " + indRecCategory$(2, 4); QTR$(allRecords!(2, 2))
    Print #1,

    Print #1, "MOST POINTS, OVERTIME PERIOD"
    If allRecords!(3, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(3, 0); "- "; indRecCategory$(3, 0); ", "; indRecCategory$(3, 1) + " " + indRecCategory$(3, 2) + " " + indRecCategory$(3, 3) + ", " + indRecCategory$(3, 4)
    End If
    Print #1,

    Print #1, "HIGHEST FIELD GOAL PERCENTAGE, GAME (MINIMUM 15 FIELD GOALS)"
    If allRecords!(4, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        If allRecords!(4, 0) = 1 Then
            Print #1, "1.000 ";
        Else
            Print #1, Using " .### "; allRecords!(4, 0);
        End If

        Print #1, "- "; indRecCategory$(4, 0); ", "; indRecCategory$(4, 1) + " " + indRecCategory$(4, 2) + " " + indRecCategory$(4, 3) + ", " + indRecCategory$(4, 4);
        Print #1, Using " (##/##)"; allRecords!(4, 1); allRecords!(4, 2)
    End If
    Print #1,

    Print #1, "MOST FIELD GOALS, NO MISSES, GAME"
    If allRecords!(5, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(5, 0); "- "; indRecCategory$(5, 0); ", "; indRecCategory$(5, 1) + " " + indRecCategory$(5, 2) + " " + indRecCategory$(5, 3) + ", " + indRecCategory$(5, 4)
    End If
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    If allRecords!(6, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(6, 0); "- "; indRecCategory$(6, 0); ", "; indRecCategory$(6, 1) + " " + indRecCategory$(6, 2) + " " + indRecCategory$(6, 3) + ", " + indRecCategory$(6, 4)
    End If
    Print #1,

    Print #1, "MOST FIELD GOALS, GAME"
    Print #1, allRecords!(7, 0); "- "; indRecCategory$(7, 0); ", "; indRecCategory$(7, 1) + " " + indRecCategory$(7, 2) + " " + indRecCategory$(7, 3) + ", " + indRecCategory$(7, 4)
    Print #1,

    Print #1, "MOST FIELD GOALS, ONE HALF"
    Print #1, allRecords!(8, 0); "- "; indRecCategory$(8, 0); ", "; indRecCategory$(8, 1) + " " + indRecCategory$(8, 2) + " " + indRecCategory$(8, 3) + ", " + indRecCategory$(8, 4); HALF$(allRecords!(8, 2))
    Print #1,

    Print #1, "MOST FIELD GOALS, ONE QUARTER"
    Print #1, allRecords!(9, 0); "- "; indRecCategory$(9, 0); ", "; indRecCategory$(9, 1) + " " + indRecCategory$(9, 2) + " " + indRecCategory$(9, 3) + ", " + indRecCategory$(9, 4); QTR$(allRecords!(9, 2))
    Print #1,

    Print #1, "MOST FIELD GOALS, OVERTIME"
    If allRecords!(10, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(10, 0); "- "; indRecCategory$(10, 0); ", "; indRecCategory$(10, 1) + " " + indRecCategory$(10, 2) + " " + indRecCategory$(10, 3) + ", " + indRecCategory$(10, 4)
    End If
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, GAME"
    Print #1, allRecords!(11, 0); "- "; indRecCategory$(11, 0); ", "; indRecCategory$(11, 1) + " " + indRecCategory$(11, 2) + " " + indRecCategory$(11, 3) + ", " + indRecCategory$(11, 4)
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Print #1, allRecords!(12, 0); "- "; indRecCategory$(12, 0); ", "; indRecCategory$(12, 1) + " " + indRecCategory$(12, 2) + " " + indRecCategory$(12, 3) + ", " + indRecCategory$(12, 4); HALF$(allRecords!(12, 2))
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, ONE QUARTER"
    Print #1, allRecords!(13, 0); "- "; indRecCategory$(13, 0); ", "; indRecCategory$(13, 1) + " " + indRecCategory$(13, 2) + " " + indRecCategory$(13, 3) + ", " + indRecCategory$(13, 4); QTR$(allRecords!(13, 2))
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    If allRecords!(15, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(15, 0); "- "; indRecCategory$(15, 0); ", "; indRecCategory$(15, 1) + " " + indRecCategory$(15, 2) + " " + indRecCategory$(15, 3) + ", " + indRecCategory$(15, 4)
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    If allRecords!(16, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(16, 0); "- "; indRecCategory$(16, 0); ", "; indRecCategory$(16, 1) + " " + indRecCategory$(16, 2) + " " + indRecCategory$(16, 3) + ", " + indRecCategory$(16, 4)
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, GAME"
    If allRecords!(17, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(17, 0); "- "; indRecCategory$(17, 0); ", "; indRecCategory$(17, 1) + " " + indRecCategory$(17, 2) + " " + indRecCategory$(17, 3) + ", " + indRecCategory$(17, 4)
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS MADE, ONE HALF"
    If allRecords!(18, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(18, 0); "- "; indRecCategory$(18, 0); ", "; indRecCategory$(18, 1) + " " + indRecCategory$(18, 2) + " " + indRecCategory$(18, 3) + ", " + indRecCategory$(18, 4); HALF$(allRecords!(18, 2))
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS MADE, ONE QUARTER"
    If allRecords!(19, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(19, 0); "- "; indRecCategory$(19, 0); ", "; indRecCategory$(19, 1) + " " + indRecCategory$(19, 2) + " " + indRecCategory$(19, 3) + ", " + indRecCategory$(19, 4); QTR$(allRecords!(19, 2))
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    If allRecords!(21, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(21, 0); "- "; indRecCategory$(21, 0); ", "; indRecCategory$(21, 1) + " " + indRecCategory$(21, 2) + " " + indRecCategory$(21, 3) + ", " + indRecCategory$(21, 4)
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    If allRecords!(22, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(22, 0); "- "; indRecCategory$(22, 0); ", "; indRecCategory$(22, 1) + " " + indRecCategory$(22, 2) + " " + indRecCategory$(22, 3) + ", " + indRecCategory$(22, 4); HALF$(allRecords!(22, 2))
    End If
    Print #1,

    Print #1, "MOST FREE THROWS MADE, NO MISSES, GAME"
    If allRecords!(23, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(23, 0); "- "; indRecCategory$(23, 0); ", "; indRecCategory$(23, 1) + " " + indRecCategory$(23, 2) + " " + indRecCategory$(23, 3) + ", " + indRecCategory$(23, 4)
    End If
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, NONE MADE, GAME"
    If allRecords!(24, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(24, 0); "- "; indRecCategory$(24, 0); ", "; indRecCategory$(24, 1) + " " + indRecCategory$(24, 2) + " " + indRecCategory$(24, 3) + ", " + indRecCategory$(24, 4)
    End If
    Print #1,

    Print #1, "MOST FREE THROWS MADE, GAME"
    If allRecords!(25, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(25, 0); "- "; indRecCategory$(25, 0); ", "; indRecCategory$(25, 1) + " " + indRecCategory$(25, 2) + " " + indRecCategory$(25, 3) + ", " + indRecCategory$(25, 4)
    End If
    Print #1,

    Print #1, "MOST FREE THROWS MADE, ONE HALF"
    If allRecords!(26, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(26, 0); "- "; indRecCategory$(26, 0); ", "; indRecCategory$(26, 1) + " " + indRecCategory$(26, 2) + " " + indRecCategory$(26, 3) + ", " + indRecCategory$(26, 4); HALF$(allRecords!(26, 2))
    End If
    Print #1,

    Print #1, "MOST FREE THROWS MADE, ONE QUARTER"
    If allRecords!(27, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(27, 0); "- "; indRecCategory$(27, 0); ", "; indRecCategory$(27, 1) + " " + indRecCategory$(27, 2) + " " + indRecCategory$(27, 3) + ", " + indRecCategory$(27, 4); QTR$(allRecords!(27, 2))
    End If
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, GAME"
    If allRecords!(28, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(28, 0); "- "; indRecCategory$(28, 0); ", "; indRecCategory$(28, 1) + " " + indRecCategory$(28, 2) + " " + indRecCategory$(28, 3) + ", " + indRecCategory$(28, 4)
    End If
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, ONE HALF"
    If allRecords!(29, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(29, 0); "- "; indRecCategory$(29, 0); ", "; indRecCategory$(29, 1) + " " + indRecCategory$(29, 2) + " " + indRecCategory$(29, 3) + ", " + indRecCategory$(29, 4); HALF$(allRecords!(29, 2))
    End If
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, ONE QUARTER"
    If allRecords!(30, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME";
    Else
        Print #1, allRecords!(30, 0); "- "; indRecCategory$(30, 0); ", "; indRecCategory$(30, 1) + " " + indRecCategory$(30, 2) + " " + indRecCategory$(30, 3) + ", " + indRecCategory$(30, 4); QTR$(allRecords!(30, 2))
    End If
    Print #1,

    Print #1, "MOST REBOUNDS, GAME"
    If allRecords!(31, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(31, 0); "- "; indRecCategory$(31, 0); ", "; indRecCategory$(31, 1) + " " + indRecCategory$(31, 2) + " " + indRecCategory$(31, 3) + ", " + indRecCategory$(31, 4)
    End If
    Print #1,

    Print #1, "MOST REBOUNDS, ONE HALF"
    If allRecords!(32, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(32, 0); "- "; indRecCategory$(32, 0); ", "; indRecCategory$(32, 1) + " " + indRecCategory$(32, 2) + " " + indRecCategory$(32, 3) + ", " + indRecCategory$(32, 4); HALF$(allRecords!(32, 2))
    End If
    Print #1,

    Print #1, "MOST REBOUNDS, ONE QUARTER"
    If allRecords!(33, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(33, 0); "- "; indRecCategory$(33, 0); ", "; indRecCategory$(33, 1) + " " + indRecCategory$(33, 2) + " " + indRecCategory$(33, 3) + ", " + indRecCategory$(33, 4); QTR$(allRecords!(33, 2))
    End If
    Print #1,

    Print #1, "MOST OFFENSIVE REBOUNDS, GAME"
    If allRecords!(34, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(34, 0); "- "; indRecCategory$(34, 0); ", "; indRecCategory$(34, 1) + " " + indRecCategory$(34, 2) + " " + indRecCategory$(34, 3) + ", " + indRecCategory$(34, 4)
    End If
    Print #1,

    Print #1, "MOST OFFENSIVE REBOUNDS, ONE HALF"
    If allRecords!(35, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(35, 0); "- "; indRecCategory$(35, 0); ", "; indRecCategory$(35, 1) + " " + indRecCategory$(35, 2) + " " + indRecCategory$(35, 3) + ", " + indRecCategory$(35, 4); HALF$(allRecords!(35, 2))
    End If
    Print #1,

    Print #1, "MOST OFFENSIVE REBOUNDS, ONE QUARTER"
    If allRecords!(36, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME";
    Else
        Print #1, allRecords!(36, 0); "- "; indRecCategory$(36, 0); ", "; indRecCategory$(36, 1) + " " + indRecCategory$(36, 2) + " " + indRecCategory$(36, 3) + ", " + indRecCategory$(36, 4); QTR$(allRecords!(36, 2))
    End If
    Print #1,

    Print #1, "MOST DEFENSIVE REBOUNDS, GAME"
    If allRecords!(37, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(37, 0); "- "; indRecCategory$(37, 0); ", "; indRecCategory$(37, 1) + " " + indRecCategory$(37, 2) + " " + indRecCategory$(37, 3) + ", " + indRecCategory$(37, 4)
    End If
    Print #1,

    Print #1, "MOST DEFENSIVE REBOUNDS, ONE HALF"
    If allRecords!(38, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(38, 0); "- "; indRecCategory$(38, 0); ", "; indRecCategory$(38, 1) + " " + indRecCategory$(38, 2) + " " + indRecCategory$(38, 3) + ", " + indRecCategory$(38, 4); HALF$(allRecords!(38, 2))
    End If
    Print #1,

    Print #1, "MOST DEFENSIVE REBOUNDS, ONE QUARTER"
    If allRecords!(39, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(39, 0); "- "; indRecCategory$(39, 0); ", "; indRecCategory$(39, 1) + " " + indRecCategory$(39, 2) + " " + indRecCategory$(39, 3) + ", " + indRecCategory$(39, 4); QTR$(allRecords!(39, 2))
    End If
    Print #1,

    Print #1, "MOST ASSISTS, GAME"
    If allRecords!(40, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(40, 0); "- "; indRecCategory$(40, 0); ", "; indRecCategory$(40, 1) + " " + indRecCategory$(40, 2) + " " + indRecCategory$(40, 3) + ", " + indRecCategory$(40, 4)
    End If
    Print #1,

    Print #1, "MOST ASSISTS, ONE HALF"
    If allRecords!(41, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(41, 0); "- "; indRecCategory$(41, 0); ", "; indRecCategory$(41, 1) + " " + indRecCategory$(41, 2) + " " + indRecCategory$(41, 3) + ", " + indRecCategory$(41, 4); HALF$(allRecords!(41, 2))
    End If
    Print #1,

    Print #1, "MOST ASSISTS, ONE QUARTER"
    If allRecords!(42, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(42, 0); "- "; indRecCategory$(42, 0); ", "; indRecCategory$(42, 1) + " " + indRecCategory$(42, 2) + " " + indRecCategory$(42, 3) + ", " + indRecCategory$(42, 4); QTR$(allRecords!(42, 2))
    End If
    Print #1,

    Print #1, "MOST STEALS, GAME"
    If allRecords!(43, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(43, 0); "- "; indRecCategory$(43, 0); ", "; indRecCategory$(43, 1) + " " + indRecCategory$(43, 2) + " " + indRecCategory$(43, 3) + ", " + indRecCategory$(43, 4)
    End If
    Print #1,

    Print #1, "MOST STEALS, ONE HALF"
    If allRecords!(44, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(44, 0); "- "; indRecCategory$(44, 0); ", "; indRecCategory$(44, 1) + " " + indRecCategory$(44, 2) + " " + indRecCategory$(44, 3) + ", " + indRecCategory$(44, 4); HALF$(allRecords!(44, 2))
    End If
    Print #1,

    Print #1, "MOST STEALS, ONE QUARTER"
    If allRecords!(45, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(45, 0); "- "; indRecCategory$(45, 0); ", "; indRecCategory$(45, 1) + " " + indRecCategory$(45, 2) + " " + indRecCategory$(45, 3) + ", " + indRecCategory$(45, 4); QTR$(allRecords!(45, 2))
    End If
    Print #1,

    Print #1, "MOST BLOCKS, GAME"
    If allRecords!(46, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(46, 0); "- "; indRecCategory$(46, 0); ", "; indRecCategory$(46, 1) + " " + indRecCategory$(46, 2) + " " + indRecCategory$(46, 3) + ", " + indRecCategory$(46, 4)
    End If
    Print #1,

    Print #1, "MOST BLOCKS, ONE HALF"
    If allRecords!(47, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, allRecords!(47, 0); "- "; indRecCategory$(47, 0); ", "; indRecCategory$(47, 1) + " " + indRecCategory$(47, 2) + " " + indRecCategory$(47, 3) + ", " + indRecCategory$(47, 4); HALF$(allRecords!(47, 2))
    End If
    Print #1,

    Print #1, "MOST BLOCKS, ONE QUARTER"
    If allRecords!(48, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME";
    Else
        Print #1, allRecords!(48, 0); "- "; indRecCategory$(48, 0); ", "; indRecCategory$(48, 1) + " " + indRecCategory$(48, 2) + " " + indRecCategory$(48, 3) + ", " + indRecCategory$(48, 4); QTR$(allRecords!(48, 2));
    End If
    Print #1, Chr$(12)

    Print #1, "                            TEAM RECORDS"
    Print #1,

    Print #1, "MOST OVERTIME PERIODS"
    If teamRecords!(0, 0) = 0 Then
    Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRecords!(0, 0); "- "; teamRecCategory$(0, 1);
        Print #1, Using " (###) AT "; teamRecords!(0, 1);
        Print #1, teamRecCategory$(0, 2);: Print #1, Using " (###)"; teamRecords!(0, 2);: Print #1, ", " + teamRecCategory$(0, 3)
    End If
    Print #1,

    Print #1, "MOST POINTS, GAME"
    Print #1, teamRecords!(1, 0); "- "; teamRecCategory$(1, 0) + " " + teamRecCategory$(1, 1) + " " + teamRecCategory$(1, 2) + ", " + teamRecCategory$(1, 3)
    Print #1,

    Print #1, "FEWEST POINTS, GAME"
    Print #1, teamRecords!(2, 0); "- "; teamRecCategory$(2, 0) + " " + teamRecCategory$(2, 1) + " " + teamRecCategory$(2, 2) + ", " + teamRecCategory$(2, 3)
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(3, 0); "- "; teamRecCategory$(3, 0);
    Print #1, Using " (###) "; teamRecords!(3, 1);: Print #1, teamRecCategory$(3, 1) + " ";
    Print #1, teamRecCategory$(3, 2);: Print #1, Using " (###)"; teamRecords!(3, 2);: Print #1, ", " + teamRecCategory$(3, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(4, 0); "- "; teamRecCategory$(4, 0);
    Print #1, Using " (###) "; teamRecords!(4, 1);: Print #1, teamRecCategory$(4, 1) + " ";
    Print #1, teamRecCategory$(4, 2);: Print #1, Using " (###)"; teamRecords!(4, 2);: Print #1, ", " + teamRecCategory$(4, 3)
    Print #1,

    Print #1, "MOST POINTS, FIRST HALF"
    Print #1, teamRecords!(6, 0); "- "; teamRecCategory$(6, 0) + " " + teamRecCategory$(6, 1) + " " + teamRecCategory$(6, 2) + ", " + teamRecCategory$(6, 3)
    Print #1,

    Print #1, "FEWEST POINTS, FIRST HALF"
    Print #1, teamRecords!(7, 0); "- "; teamRecCategory$(7, 0) + " " + teamRecCategory$(7, 1) + " " + teamRecCategory$(7, 2) + ", " + teamRecCategory$(7, 3)
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, FIRST HALF"
    Print #1, teamRecords!(8, 0); "- "; teamRecCategory$(8, 0);
    Print #1, Using " (###) "; teamRecords!(8, 1);: Print #1, teamRecCategory$(8, 1) + " ";
    Print #1, teamRecCategory$(8, 2);: Print #1, Using " (###)"; teamRecords!(8, 2);: Print #1, ", " + teamRecCategory$(8, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, FIRST HALF"
    Print #1, teamRecords!(9, 0); "- "; teamRecCategory$(9, 0);
    Print #1, Using " (###) "; teamRecords!(9, 1);: Print #1, teamRecCategory$(9, 1) + " ";
    Print #1, teamRecCategory$(9, 2);: Print #1, Using " (###)"; teamRecords!(9, 2);: Print #1, ", " + teamRecCategory$(9, 3)
    Print #1,

    Print #1, "MOST POINTS, SECOND HALF"
    Print #1, teamRecords!(10, 0); "- "; teamRecCategory$(10, 0) + " " + teamRecCategory$(10, 1) + " " + teamRecCategory$(10, 2) + ", " + teamRecCategory$(10, 3)
    Print #1,

    Print #1, "FEWEST POINTS, SECOND HALF"
    Print #1, teamRecords!(11, 0); "- "; teamRecCategory$(11, 0) + " " + teamRecCategory$(11, 1) + " " + teamRecCategory$(11, 2) + ", " + teamRecCategory$(11, 3)
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, SECOND HALF"
    Print #1, teamRecords!(12, 0); "- "; teamRecCategory$(12, 0);
    Print #1, Using " (###) "; teamRecords!(12, 1);: Print #1, teamRecCategory$(12, 1) + " ";
    Print #1, teamRecCategory$(12, 2);: Print #1, Using " (###)"; teamRecords!(12, 2);: Print #1, ", " + teamRecCategory$(12, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, SECOND HALF"
    Print #1, teamRecords!(13, 0); "- "; teamRecCategory$(13, 0);
    Print #1, Using " (###) "; teamRecords!(13, 1);: Print #1, teamRecCategory$(13, 1) + " ";
    Print #1, teamRecCategory$(13, 2);: Print #1, Using " (###)"; teamRecords!(13, 2);: Print #1, ", " + teamRecCategory$(13, 3)
    Print #1,

    Print #1, "MOST POINTS, FIRST QUARTER"
    Print #1, teamRecords!(14, 0); "- "; teamRecCategory$(14, 0) + " " + teamRecCategory$(14, 1) + " " + teamRecCategory$(14, 2) + ", " + teamRecCategory$(14, 3)
    Print #1,

    Print #1, "FEWEST POINTS, FIRST QUARTER"
    Print #1, teamRecords!(15, 0); "- "; teamRecCategory$(15, 0) + " " + teamRecCategory$(15, 1) + " " + teamRecCategory$(15, 2) + ", " + teamRecCategory$(15, 3)
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, FIRST QUARTER"
    Print #1, teamRecords!(16, 0); "- "; teamRecCategory$(16, 0);
    Print #1, Using " (###) "; teamRecords!(16, 1);: Print #1, teamRecCategory$(16, 1) + " ";
    Print #1, teamRecCategory$(16, 2);: Print #1, Using " (###)"; teamRecords!(16, 2);: Print #1, ", " + teamRecCategory$(16, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, FIRST QUARTER"
    Print #1, teamRecords!(17, 0); "- "; teamRecCategory$(17, 0);
    Print #1, Using " (###) "; teamRecords!(17, 1);: Print #1, teamRecCategory$(17, 1) + " ";
    Print #1, teamRecCategory$(17, 2);: Print #1, Using " (###)"; teamRecords!(17, 2);: Print #1, ", " + teamRecCategory$(17, 3)
    Print #1,

    Print #1, "MOST POINTS, SECOND QUARTER"
    Print #1, teamRecords!(18, 0); "- "; teamRecCategory$(18, 0) + " " + teamRecCategory$(18, 1) + " " + teamRecCategory$(18, 2) + ", " + teamRecCategory$(18, 3)
    Print #1,

    Print #1, "FEWEST POINTS, SECOND QUARTER"
    Print #1, teamRecords!(19, 0); "- "; teamRecCategory$(19, 0) + " " + teamRecCategory$(19, 1) + " " + teamRecCategory$(19, 2) + ", " + teamRecCategory$(19, 3)
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, SECOND QUARTER"
    Print #1, teamRecords!(20, 0); "- "; teamRecCategory$(20, 0);
    Print #1, Using " (###) "; teamRecords!(20, 1);: Print #1, teamRecCategory$(20, 1) + " ";
    Print #1, teamRecCategory$(20, 2);: Print #1, Using " (###)"; teamRecords!(20, 2);: Print #1, ", " + teamRecCategory$(20, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, SECOND QUARTER"
    Print #1, teamRecords!(21, 0); "- "; teamRecCategory$(21, 0);
    Print #1, Using " (###) "; teamRecords!(21, 1);: Print #1, teamRecCategory$(21, 1) + " ";
    Print #1, teamRecCategory$(21, 2);: Print #1, Using " (###)"; teamRecords!(21, 2);: Print #1, ", " + teamRecCategory$(21, 3)
    Print #1,

    Print #1, "MOST POINTS, THIRD QUARTER"
    Print #1, teamRecords!(22, 0); "- "; teamRecCategory$(22, 0) + " " + teamRecCategory$(22, 1) + " " + teamRecCategory$(22, 2) + ", " + teamRecCategory$(22, 3)
    Print #1,

    Print #1, "FEWEST POINTS, THIRD QUARTER"
    Print #1, teamRecords!(23, 0); "- "; teamRecCategory$(23, 0) + " " + teamRecCategory$(23, 1) + " " + teamRecCategory$(23, 2) + ", " + teamRecCategory$(23, 3)
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, THIRD QUARTER"
    Print #1, teamRecords!(24, 0); "- "; teamRecCategory$(24, 0);
    Print #1, Using " (###) "; teamRecords!(24, 1);: Print #1, teamRecCategory$(24, 1) + " ";
    Print #1, teamRecCategory$(24, 2);: Print #1, Using " (###)"; teamRecords!(24, 2);: Print #1, ", " + teamRecCategory$(24, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, THIRD QUARTER"
    Print #1, teamRecords!(25, 0); "- "; teamRecCategory$(25, 0);
    Print #1, Using " (###) "; teamRecords!(25, 1);: Print #1, teamRecCategory$(25, 1) + " ";
    Print #1, teamRecCategory$(25, 2);: Print #1, Using " (###)"; teamRecords!(25, 2);: Print #1, ", " + teamRecCategory$(25, 3)
    Print #1,

    Print #1, "MOST POINTS, FOURTH QUARTER"
    Print #1, teamRecords!(26, 0); "- "; teamRecCategory$(26, 0) + " " + teamRecCategory$(26, 1) + " " + teamRecCategory$(26, 2) + ", " + teamRecCategory$(26, 3)
    Print #1,

    Print #1, "FEWEST POINTS, FOURTH QUARTER"
    Print #1, teamRecords!(27, 0); "- "; teamRecCategory$(27, 0) + " " + teamRecCategory$(27, 1) + " " + teamRecCategory$(27, 2) + ", " + teamRecCategory$(27, 3)
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, FOURTH QUARTER"
    Print #1, teamRecords!(28, 0); "- "; teamRecCategory$(28, 0);
    Print #1, Using " (###) "; teamRecords!(28, 1);: Print #1, teamRecCategory$(28, 1) + " ";
    Print #1, teamRecCategory$(28, 2);: Print #1, Using " (###)"; teamRecords!(28, 2);: Print #1, ", " + teamRecCategory$(28, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, FOURTH QUARTER"
    Print #1, teamRecords!(29, 0); "- "; teamRecCategory$(29, 0);
    Print #1, Using " (###) "; teamRecords!(29, 1);: Print #1, teamRecCategory$(29, 1) + " ";
    Print #1, teamRecCategory$(29, 2);: Print #1, Using " (###)"; teamRecords!(29, 2);: Print #1, ", " + teamRecCategory$(29, 3)
    Print #1,

    Print #1, "HIGHEST FIELD GOAL PERCENTAGE, GAME"
    Print #1, Using ".### "; teamRecords!(30, 0);: Print #1, "- "; teamRecCategory$(30, 0) + " " + teamRecCategory$(30, 1) + " " + teamRecCategory$(30, 2) + ", " + teamRecCategory$(30, 3);: Print #1, Using "###/###"; teamRecords!(30, 1); teamRecords!(30, 2)
    Print #1,

    Print #1, "LOWEST FIELD GOAL PERCENTAGE, GAME"
    Print #1, Using ".### "; teamRecords!(31, 0);: Print #1, "- "; teamRecCategory$(31, 0) + " " + teamRecCategory$(31, 1) + " " + teamRecCategory$(31, 2) + ", " + teamRecCategory$(31, 3);: Print #1, Using "###/###"; teamRecords!(31, 1); teamRecords!(31, 2)
    Print #1,

    Print #1, "HIGHEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Print #1, Using ".### "; teamRecords!(32, 0);: Print #1, "- "; teamRecCategory$(32, 0);
    Print #1, Using " (.###) "; teamRecords!(32, 1);: Print #1, teamRecCategory$(32, 1) + " ";
    Print #1, teamRecCategory$(32, 2);: Print #1, Using " (.###)"; teamRecords!(32, 2);: Print #1, ", " + teamRecCategory$(32, 3)
    Print #1,

    Print #1, "LOWEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Print #1, Using ".### "; teamRecords!(33, 0);: Print #1, "- "; teamRecCategory$(33, 0);
    Print #1, Using " (.###) "; teamRecords!(33, 1);: Print #1, teamRecCategory$(33, 1) + " ";
    Print #1, teamRecCategory$(33, 2);: Print #1, Using " (.###)"; teamRecords!(33, 2);: Print #1, ", " + teamRecCategory$(33, 3)
    Print #1,

    Print #1, "MOST FIELD GOALS, GAME"
    Print #1, teamRecords!(34, 0); "- "; teamRecCategory$(34, 0) + " " + teamRecCategory$(34, 1) + " " + teamRecCategory$(34, 2) + ", " + teamRecCategory$(34, 3)
    Print #1,

    Print #1, "FEWEST FIELD GOALS, GAME"
    Print #1, teamRecords!(35, 0); "- "; teamRecCategory$(35, 0) + " " + teamRecCategory$(35, 1) + " " + teamRecCategory$(35, 2) + ", " + teamRecCategory$(35, 3)
    Print #1,

    Print #1, "MOST FIELD GOALS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(36, 0); "- "; teamRecCategory$(36, 0);
    Print #1, Using " (###) "; teamRecords!(36, 1);: Print #1, teamRecCategory$(36, 1) + " ";
    Print #1, teamRecCategory$(36, 2);: Print #1, Using " (###)"; teamRecords!(36, 2);: Print #1, ", " + teamRecCategory$(36, 3)
    Print #1,

    Print #1, "FEWEST FIELD GOALS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(37, 0); "- "; teamRecCategory$(37, 0);
    Print #1, Using " (###) "; teamRecords!(37, 1);: Print #1, teamRecCategory$(37, 1) + " ";
    Print #1, teamRecCategory$(37, 2);: Print #1, Using " (###)"; teamRecords!(37, 2);: Print #1, ", " + teamRecCategory$(37, 3)
    Print #1,

    Print #1, "MOST FIELD GOALS, ONE HALF"
    Print #1, teamRecords!(38, 0); "- "; teamRecCategory$(38, 0) + " " + teamRecCategory$(38, 1) + " " + teamRecCategory$(38, 2) + ", " + teamRecCategory$(38, 3); HALF$(teamRecords!(38, 1))
    Print #1,

    Print #1, "MOST FIELD GOALS, BOTH TEAMS, ONE HALF"
    Print #1, teamRecords!(39, 0); "- "; teamRecCategory$(39, 0); " "; teamRecCategory$(39, 1); " "; teamRecCategory$(39, 2); ", "; teamRecCategory$(39, 3); HALF$(teamRecords!(39, 1))
    Print #1,

    Print #1, "MOST FIELD GOALS, ONE QUARTER"
    Print #1, teamRecords!(40, 0); "- "; teamRecCategory$(40, 0) + " " + teamRecCategory$(40, 1) + " " + teamRecCategory$(40, 2) + ", " + teamRecCategory$(40, 3); QTR$(teamRecords!(40, 1))
    Print #1,

    Print #1, "MOST FIELD GOALS, BOTH TEAMS, ONE QUARTER"
    Print #1, teamRecords!(41, 0); "- "; teamRecCategory$(41, 0); " "; teamRecCategory$(41, 1); " "; teamRecCategory$(41, 2); ", "; teamRecCategory$(41, 3); QTR$(teamRecords!(41, 1))
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, GAME"
    Print #1, teamRecords!(42, 0); "- "; teamRecCategory$(42, 0) + " " + teamRecCategory$(42, 1) + " " + teamRecCategory$(42, 2) + ", " + teamRecCategory$(42, 3)
    Print #1,

    Print #1, "FEWEST FIELD GOAL ATTEMPTS, GAME"
    Print #1, teamRecords!(43, 0); "- "; teamRecCategory$(43, 0) + " " + teamRecCategory$(43, 1) + " " + teamRecCategory$(43, 2) + ", " + teamRecCategory$(43, 3)
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(44, 0); "- "; teamRecCategory$(44, 0);
    Print #1, Using " (###) "; teamRecords!(44, 1);: Print #1, teamRecCategory$(44, 1) + " ";
    Print #1, teamRecCategory$(44, 2);: Print #1, Using " (###)"; teamRecords!(44, 2);: Print #1, ", " + teamRecCategory$(44, 3)
    Print #1,

    Print #1, "FEWEST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(45, 0); "- "; teamRecCategory$(45, 0);
    Print #1, Using " (###) "; teamRecords!(45, 1);: Print #1, teamRecCategory$(45, 1) + " ";
    Print #1, teamRecCategory$(45, 2);: Print #1, Using " (###)"; teamRecords!(45, 2);: Print #1, ", " + teamRecCategory$(45, 3)
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Print #1, teamRecords!(46, 0); "- "; teamRecCategory$(46, 0) + " " + teamRecCategory$(46, 1) + " " + teamRecCategory$(46, 2) + ", " + teamRecCategory$(46, 3); HALF$(teamRecords!(46, 1))
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE HALF"
    Print #1, teamRecords!(47, 0); "- "; teamRecCategory$(47, 0); " "; teamRecCategory$(47, 1); " "; teamRecCategory$(47, 2); ", "; teamRecCategory$(47, 3); HALF$(teamRecords!(47, 1))
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, ONE QUARTER"
    Print #1, teamRecords!(48, 0); "- "; teamRecCategory$(48, 0) + " " + teamRecCategory$(48, 1) + " " + teamRecCategory$(48, 2) + ", " + teamRecCategory$(48, 3); QTR$(teamRecords!(48, 1))
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE QUARTER"
    Print #1, teamRecords!(49, 0); "- "; teamRecCategory$(49, 0); " "; teamRecCategory$(49, 1); " "; teamRecCategory$(49, 2); ", "; teamRecCategory$(49, 3); QTR$(teamRecords!(49, 1))
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    If teamRecords!(50, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRecords!(50, 0); "- "; teamRecCategory$(50, 0) + " " + teamRecCategory$(50, 1) + " " + teamRecCategory$(50, 2) + ", " + teamRecCategory$(50, 3)
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, NO MISSES, BOTH TEAMS, GAME"
    If teamRecords!(51, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRecords!(51, 0); "- "; teamRecCategory$(51, 0);
        Print #1, Using " (###) "; teamRecords!(51, 1);: Print #1, teamRecCategory$(51, 1) + " ";
        Print #1, teamRecCategory$(51, 2);: Print #1, Using " (###)"; teamRecords!(51, 2);: Print #1, ", " + teamRecCategory$(51, 3)
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    If teamRecords!(52, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRecords!(52, 0); "- "; teamRecCategory$(52, 0) + " " + teamRecCategory$(52, 1) + " " + teamRecCategory$(52, 2) + ", " + teamRecCategory$(52, 3)
    End If
    Print #1,
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, GAME"
    Print #1, teamRecords!(53, 0); "- "; teamRecCategory$(53, 0) + " " + teamRecCategory$(53, 1) + " " + teamRecCategory$(53, 2) + ", " + teamRecCategory$(53, 3)
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(54, 0); "- "; teamRecCategory$(54, 0);
    Print #1, Using " (###) "; teamRecords!(54, 1);: Print #1, teamRecCategory$(54, 1) + " ";
    Print #1, teamRecCategory$(54, 2);: Print #1, Using " (###)"; teamRecords!(54, 2);: Print #1, ", " + teamRecCategory$(54, 3)
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, ONE HALF"
    Print #1, teamRecords!(55, 0); "- "; teamRecCategory$(55, 0) + " " + teamRecCategory$(55, 1) + " " + teamRecCategory$(55, 2) + ", " + teamRecCategory$(55, 3); HALF$(teamRecords!(55, 1))
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, ONE QUARTER"
    Print #1, teamRecords!(56, 0); "- "; teamRecCategory$(56, 0) + " " + teamRecCategory$(56, 1) + " " + teamRecCategory$(56, 2) + ", " + teamRecCategory$(56, 3); QTR$(teamRecords!(56, 1))
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    Print #1, teamRecords!(57, 0); "- "; teamRecCategory$(57, 0) + " " + teamRecCategory$(57, 1) + " " + teamRecCategory$(57, 2) + ", " + teamRecCategory$(57, 3)
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(58, 0); "- "; teamRecCategory$(58, 0);
    Print #1, Using " (###) "; teamRecords!(58, 1);: Print #1, teamRecCategory$(58, 1) + " ";
    Print #1, teamRecCategory$(58, 2);: Print #1, Using " (###)"; teamRecords!(58, 2);: Print #1, ", " + teamRecCategory$(58, 3)
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    Print #1, teamRecords!(59, 0); "- "; teamRecCategory$(59, 0) + " " + teamRecCategory$(59, 1) + " " + teamRecCategory$(59, 2) + ", " + teamRecCategory$(59, 3); HALF$(teamRecords!(59, 1))
    Print #1,

    Print #1, "MOST FREE THROWS MADE, NO MISSES, GAME"
    If teamRecords!(60, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRecords!(60, 0); "- "; teamRecCategory$(60, 0) + " " + teamRecCategory$(60, 1) + " " + teamRecCategory$(60, 2) + ", " + teamRecCategory$(60, 3)
    End If
    Print #1,

    Print #1, "LOWEST FREE THROW PERCENTAGE, GAME"
    Print #1, Using ".### "; teamRecords!(61, 0);: Print #1, "- "; teamRecCategory$(61, 0) + " " + teamRecCategory$(61, 1) + " " + teamRecCategory$(61, 2) + ", " + teamRecCategory$(61, 3);: Print #1, Using "###/###"; teamRecords!(61, 1); teamRecords!(61, 2)
    Print #1,

    Print #1, "HIGHEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Print #1, Using ".### "; teamRecords!(62, 0);: Print #1, "- "; teamRecCategory$(62, 0);
    Print #1, Using " (.###) "; teamRecords!(62, 1);: Print #1, teamRecCategory$(62, 1) + " ";
    Print #1, teamRecCategory$(62, 2);: Print #1, Using " (.###)"; teamRecords!(62, 2);: Print #1, ", " + teamRecCategory$(62, 3)
    Print #1,

    Print #1, "LOWEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Print #1, Using ".### "; teamRecords!(63, 0);: Print #1, "- "; teamRecCategory$(63, 0);
    Print #1, Using " (.###) "; teamRecords!(63, 1);: Print #1, teamRecCategory$(63, 1) + " ";
    Print #1, teamRecCategory$(63, 2);: Print #1, Using " (.###)"; teamRecords!(63, 2);: Print #1, ", " + teamRecCategory$(63, 3)
    Print #1,

    Print #1, "MOST FREE THROWS MADE, GAME"
    Print #1, teamRecords!(64, 0); "- "; teamRecCategory$(64, 0) + " " + teamRecCategory$(64, 1) + " " + teamRecCategory$(64, 2) + ", " + teamRecCategory$(64, 3)
    Print #1,

    Print #1, "FEWEST FREE THROWS MADE, GAME"
    Print #1, teamRecords!(65, 0); "- "; teamRecCategory$(65, 0) + " " + teamRecCategory$(65, 1) + " " + teamRecCategory$(65, 2) + ", " + teamRecCategory$(65, 3)
    Print #1,

    Print #1, "MOST FREE THROWS MADE, BOTH TEAMS, GAME"
    Print #1, teamRecords!(66, 0); "- "; teamRecCategory$(66, 0);
    Print #1, Using " (###) "; teamRecords!(66, 1);: Print #1, teamRecCategory$(66, 1) + " ";
    Print #1, teamRecCategory$(66, 2);: Print #1, Using " (###)"; teamRecords!(66, 2);: Print #1, ", " + teamRecCategory$(66, 3)
    Print #1,

    Print #1, "FEWEST FREE THROWS MADE, BOTH TEAMS, GAME"
    Print #1, teamRecords!(67, 0); "- "; teamRecCategory$(67, 0);
    Print #1, Using " (###) "; teamRecords!(67, 1);: Print #1, teamRecCategory$(67, 1) + " ";
    Print #1, teamRecCategory$(67, 2);: Print #1, Using " (###)"; teamRecords!(67, 2);: Print #1, ", " + teamRecCategory$(67, 3)
    Print #1,

    Print #1, "MOST FREE THROWS MADE, ONE HALF"
    Print #1, teamRecords!(68, 0); "- "; teamRecCategory$(68, 0) + " " + teamRecCategory$(68, 1) + " " + teamRecCategory$(68, 2) + ", " + teamRecCategory$(68, 3); HALF$(teamRecords!(68, 1))
    Print #1,

    Print #1, "MOST FREE THROWS MADE, ONE QUARTER"
    Print #1, teamRecords!(70, 0); "- "; teamRecCategory$(70, 0) + " " + teamRecCategory$(70, 1) + " " + teamRecCategory$(70, 2) + ", " + teamRecCategory$(70, 3); QTR$(teamRecords!(70, 1))
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, GAME"
    Print #1, teamRecords!(72, 0); "- "; teamRecCategory$(72, 0) + " " + teamRecCategory$(72, 1) + " " + teamRecCategory$(72, 2) + ", " + teamRecCategory$(72, 3)
    Print #1,

    Print #1, "FEWEST FREE THROW ATTEMPTS, GAME"
    Print #1, teamRecords!(73, 0); "- "; teamRecCategory$(73, 0) + " " + teamRecCategory$(73, 1) + " " + teamRecCategory$(73, 2) + ", " + teamRecCategory$(73, 3)
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(74, 0); "- "; teamRecCategory$(74, 0);
    Print #1, Using " (###) "; teamRecords!(74, 1);: Print #1, teamRecCategory$(74, 1) + " ";
    Print #1, teamRecCategory$(74, 2);: Print #1, Using " (###)"; teamRecords!(74, 2);: Print #1, ", " + teamRecCategory$(74, 3)
    Print #1,

    Print #1, "FEWEST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(75, 0); "- "; teamRecCategory$(75, 0);
    Print #1, Using " (###) "; teamRecords!(75, 1);: Print #1, teamRecCategory$(75, 1) + " ";
    Print #1, teamRecCategory$(75, 2);: Print #1, Using " (###)"; teamRecords!(75, 2);: Print #1, ", " + teamRecCategory$(75, 3)
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, ONE HALF"
    Print #1, teamRecords!(76, 0); "- "; teamRecCategory$(76, 0) + " " + teamRecCategory$(76, 1) + " " + teamRecCategory$(76, 2) + ", " + teamRecCategory$(76, 3); HALF$(teamRecords!(76, 1))
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, ONE QUARTER"
    Print #1, teamRecords!(78, 0); "- "; teamRecCategory$(78, 0) + " " + teamRecCategory$(78, 1) + " " + teamRecCategory$(78, 2) + ", " + teamRecCategory$(78, 3); QTR$(teamRecords!(78, 1))
    Print #1,

    Print #1, "MOST REBOUNDS, GAME"
    Print #1, teamRecords!(80, 0); "- "; teamRecCategory$(80, 0) + " " + teamRecCategory$(80, 1) + " " + teamRecCategory$(80, 2) + ", " + teamRecCategory$(80, 3)
    Print #1,

    Print #1, "FEWEST REBOUNDS, GAME"
    Print #1, teamRecords!(81, 0); "- "; teamRecCategory$(81, 0) + " " + teamRecCategory$(81, 1) + " " + teamRecCategory$(81, 2) + ", " + teamRecCategory$(81, 3)
    Print #1,

    Print #1, "MOST REBOUNDS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(82, 0); "- "; teamRecCategory$(82, 0);
    Print #1, Using " (###) "; teamRecords!(82, 1);: Print #1, teamRecCategory$(82, 1) + " ";
    Print #1, teamRecCategory$(82, 2);: Print #1, Using " (###)"; teamRecords!(82, 2);: Print #1, ", " + teamRecCategory$(82, 3)
    Print #1,

    Print #1, "FEWEST REBOUNDS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(83, 0); "- "; teamRecCategory$(83, 0);
    Print #1, Using " (###) "; teamRecords!(83, 1);: Print #1, teamRecCategory$(83, 1) + " ";
    Print #1, teamRecCategory$(83, 2);: Print #1, Using " (###)"; teamRecords!(83, 2);: Print #1, ", " + teamRecCategory$(83, 3)
    Print #1,

    Print #1, "MOST REBOUNDS, ONE HALF"
    Print #1, teamRecords!(84, 0); "- "; teamRecCategory$(84, 0) + " " + teamRecCategory$(84, 1) + " " + teamRecCategory$(84, 2) + ", " + teamRecCategory$(84, 3); HALF$(teamRecords!(84, 1))
    Print #1,

    Print #1, "MOST REBOUNDS, ONE QUARTER"
    Print #1, teamRecords!(85, 0); "- "; teamRecCategory$(85, 0) + " " + teamRecCategory$(85, 1) + " " + teamRecCategory$(85, 2) + ", " + teamRecCategory$(85, 3); QTR$(teamRecords!(85, 1))
    Print #1,
    Print #1,

    Print #1, "MOST OFFENSIVE REBOUNDS, GAME"
    Print #1, teamRecords!(86, 0); "- "; teamRecCategory$(86, 0) + " " + teamRecCategory$(86, 1) + " " + teamRecCategory$(86, 2) + ", " + teamRecCategory$(86, 3)
    Print #1,

    Print #1, "MOST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(88, 0); "- "; teamRecCategory$(88, 0);
    Print #1, Using " (###) "; teamRecords!(88, 1);: Print #1, teamRecCategory$(88, 1) + " ";
    Print #1, teamRecCategory$(88, 2);: Print #1, Using " (###)"; teamRecords!(88, 2);: Print #1, ", " + teamRecCategory$(88, 3)
    Print #1,

    Print #1, "FEWEST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(89, 0); "- "; teamRecCategory$(89, 0);
    Print #1, Using " (###) "; teamRecords!(89, 1);: Print #1, teamRecCategory$(89, 1) + " ";
    Print #1, teamRecCategory$(89, 2);: Print #1, Using " (###)"; teamRecords!(89, 2);: Print #1, ", " + teamRecCategory$(89, 3)
    Print #1,

    Print #1, "MOST OFFENSIVE REBOUNDS, ONE HALF"
    Print #1, teamRecords!(90, 0); "- "; teamRecCategory$(90, 0) + " " + teamRecCategory$(90, 1) + " " + teamRecCategory$(90, 2) + ", " + teamRecCategory$(90, 3); HALF$(teamRecords!(90, 1))
    Print #1,

    Print #1, "MOST ASSISTS, GAME"
    Print #1, teamRecords!(96, 0); "- "; teamRecCategory$(96, 0) + " " + teamRecCategory$(96, 1) + " " + teamRecCategory$(96, 2) + ", " + teamRecCategory$(96, 3)
    Print #1,

    Print #1, "FEWEST ASSISTS, GAME"
    Print #1, teamRecords!(97, 0); "- "; teamRecCategory$(97, 0) + " " + teamRecCategory$(97, 1) + " " + teamRecCategory$(97, 2) + ", " + teamRecCategory$(97, 3)
    Print #1,

    Print #1, "MOST ASSISTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(98, 0); "- "; teamRecCategory$(98, 0);
    Print #1, Using " (###) "; teamRecords!(98, 1);: Print #1, teamRecCategory$(98, 1) + " ";
    Print #1, teamRecCategory$(98, 2);: Print #1, Using " (###)"; teamRecords!(98, 2);: Print #1, ", " + teamRecCategory$(98, 3)
    Print #1,

    Print #1, "FEWEST ASSISTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(99, 0); "- "; teamRecCategory$(99, 0);
    Print #1, Using " (###) "; teamRecords!(99, 1);: Print #1, teamRecCategory$(99, 1) + " ";
    Print #1, teamRecCategory$(99, 2);: Print #1, Using " (###)"; teamRecords!(99, 2);: Print #1, ", " + teamRecCategory$(99, 3)
    Print #1,

    Print #1, "MOST ASSISTS, ONE HALF"
    Print #1, teamRecords!(100, 0); "- "; teamRecCategory$(100, 0) + " " + teamRecCategory$(100, 1) + " " + teamRecCategory$(100, 2) + ", " + teamRecCategory$(100, 3); HALF$(teamRecords!(100, 1))
    Print #1,

    Print #1, "MOST ASSISTS, ONE QUARTER"
    Print #1, teamRecords!(102, 0); "- "; teamRecCategory$(102, 0) + " " + teamRecCategory$(102, 1) + " " + teamRecCategory$(102, 2) + ", " + teamRecCategory$(102, 3); QTR$(teamRecords!(102, 1))
    Print #1,

    Print #1, "MOST PERSONAL FOULS, GAME"
    Print #1, teamRecords!(104, 0); "- "; teamRecCategory$(104, 0) + " " + teamRecCategory$(104, 1) + " " + teamRecCategory$(104, 2) + ", " + teamRecCategory$(104, 3)
    Print #1,

    Print #1, "FEWEST PERSONAL FOULS, GAME"
    Print #1, teamRecords!(105, 0); "- "; teamRecCategory$(105, 0) + " " + teamRecCategory$(105, 1) + " " + teamRecCategory$(105, 2) + ", " + teamRecCategory$(105, 3)
    Print #1,

    Print #1, "MOST PERSONAL FOULS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(106, 0); "- "; teamRecCategory$(106, 0);
    Print #1, Using " (###) "; teamRecords!(106, 1);: Print #1, teamRecCategory$(106, 1) + " ";
    Print #1, teamRecCategory$(106, 2);: Print #1, Using " (###)"; teamRecords!(106, 2);: Print #1, ", " + teamRecCategory$(106, 3)
    Print #1,

    Print #1, "FEWEST PERSONAL FOULS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(107, 0); "- "; teamRecCategory$(107, 0);
    Print #1, Using " (###) "; teamRecords!(107, 1);: Print #1, teamRecCategory$(107, 1) + " ";
    Print #1, teamRecCategory$(107, 2);: Print #1, Using " (###)"; teamRecords!(107, 2);: Print #1, ", " + teamRecCategory$(107, 3)
    Print #1,

    Print #1, "MOST PERSONAL FOULS, ONE HALF"
    Print #1, teamRecords!(108, 0); "- "; teamRecCategory$(108, 0) + " " + teamRecCategory$(108, 1) + " " + teamRecCategory$(108, 2) + ", " + teamRecCategory$(108, 3); HALF$(teamRecords!(108, 1))
    Print #1,

    Print #1, "MOST PERSONAL FOULS, BOTH TEAMS, ONE HALF"
    Print #1, teamRecords!(109, 0); "- "; teamRecCategory$(109, 0); " "; teamRecCategory$(109, 1); " "; teamRecCategory$(109, 2); ", "; teamRecCategory$(109, 3); HALF$(teamRecords!(109, 1))
    Print #1,

    Print #1, "MOST PERSONAL FOULS, ONE QUARTER"
    Print #1, teamRecords!(110, 0); "- "; teamRecCategory$(110, 0) + " " + teamRecCategory$(110, 1) + " " + teamRecCategory$(110, 2) + ", " + teamRecCategory$(110, 3); QTR$(teamRecords!(110, 1))
    Print #1,

    Print #1, "MOST PERSONAL FOULS, BOTH TEAMS, ONE QUARTER"
    Print #1, teamRecords!(111, 0); "- "; teamRecCategory$(111, 0); " "; teamRecCategory$(111, 1); " "; teamRecCategory$(111, 2); ", "; teamRecCategory$(111, 3); QTR$(teamRecords!(111, 1));

    Print #1,

    Print #1, "MOST STEALS, GAME"
    Print #1, teamRecords!(112, 0); "- "; teamRecCategory$(112, 0) + " " + teamRecCategory$(112, 1) + " " + teamRecCategory$(112, 2) + ", " + teamRecCategory$(112, 3)
    Print #1,

    Print #1, "MOST STEALS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(114, 0); "- "; teamRecCategory$(114, 0);
    Print #1, Using " (###) "; teamRecords!(114, 1);: Print #1, teamRecCategory$(114, 1) + " ";
    Print #1, teamRecCategory$(114, 2);: Print #1, Using " (###)"; teamRecords!(114, 2);: Print #1, ", " + teamRecCategory$(114, 3)
    Print #1,

    Print #1, "MOST STEALS, ONE HALF"
    Print #1, teamRecords!(116, 0); "- "; teamRecCategory$(116, 0) + " " + teamRecCategory$(116, 1) + " " + teamRecCategory$(116, 2) + ", " + teamRecCategory$(116, 3); HALF$(teamRecords!(116, 1))
    Print #1,

    Print #1, "MOST STEALS, ONE QUARTER"
    Print #1, teamRecords!(117, 0); "- "; teamRecCategory$(117, 0) + " " + teamRecCategory$(117, 1) + " " + teamRecCategory$(117, 2) + ", " + teamRecCategory$(117, 3); QTR$(teamRecords!(117, 1))
    Print #1,

    Print #1, "MOST BLOCKS, GAME"
    Print #1, teamRecords!(118, 0); "- "; teamRecCategory$(118, 0) + " " + teamRecCategory$(118, 1) + " " + teamRecCategory$(118, 2) + ", " + teamRecCategory$(118, 3)
    Print #1,

    Print #1, "MOST BLOCKS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(119, 0); "- "; teamRecCategory$(119, 0);
    Print #1, Using " (###) "; teamRecords!(119, 1);: Print #1, teamRecCategory$(119, 1) + " ";
    Print #1, teamRecCategory$(119, 2);: Print #1, Using " (###)"; teamRecords!(119, 2);: Print #1, ", " + teamRecCategory$(119, 3)
    Print #1,

    Print #1, "MOST TURNOVERS, GAME"
    Print #1, teamRecords!(122, 0); "- "; teamRecCategory$(122, 0) + " " + teamRecCategory$(122, 1) + " " + teamRecCategory$(122, 2) + ", " + teamRecCategory$(122, 3)
    Print #1,

    Print #1, "FEWEST TURNOVERS, GAME"
    Print #1, teamRecords!(123, 0); "- "; teamRecCategory$(123, 0) + " " + teamRecCategory$(123, 1) + " " + teamRecCategory$(123, 2) + ", " + teamRecCategory$(123, 3)
    Print #1,

    Print #1, "MOST TURNOVERS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(124, 0); "- "; teamRecCategory$(124, 0);
    Print #1, Using " (###) "; teamRecords!(124, 1);: Print #1, teamRecCategory$(124, 1) + " ";
    Print #1, teamRecCategory$(124, 2);: Print #1, Using " (###)"; teamRecords!(124, 2);: Print #1, ", " + teamRecCategory$(124, 3)
    Print #1,

    Print #1, "FEWEST TURNOVERS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(125, 0); "- "; teamRecCategory$(125, 0);
    Print #1, Using " (###) "; teamRecords!(125, 1);: Print #1, teamRecCategory$(125, 1) + " ";
    Print #1, teamRecCategory$(125, 2);: Print #1, Using " (###)"; teamRecords!(125, 2);: Print #1, ", " + teamRecCategory$(125, 3)

    Close #1

    Open "RECORDS" For Input As #2

    Do While Not EOF(2)
        Line Input #2, X$
        LPrint X$
    Loop

    LPrint Chr$(12)

    Close #2

End Sub

