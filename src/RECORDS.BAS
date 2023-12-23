'----------------------------------------
'         GameRecords Subroutine
'----------------------------------------
'This subroutine reads in the record file
'and gives the user an option of either
'viewing or printing the records.
Sub GameRecords (menuChoice)

    _Title "Basketball: The Pro Game - Game Records"

    Shared indRecords!(), teamRecords!()

    Shared indRecDesc$(), teamRecDesc$()

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
                Input #1, indRecords!(I, I1)
            Next

            For I1 = 0 To 4
                Input #1, indRecDesc$(I, I1)
            Next
        Next

        For I = 0 To 125
            For I1 = 0 To 2
                Input #1, teamRecords!(I, I1)
            Next
            For I1 = 0 To 3
                Input #1, teamRecDesc$(I, I1)
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

    Shared indRecords!(), teamRecords!()
    Shared indRecDesc$(), teamRecDesc$()

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
    Print indRecords!(0, 0); "- "; indRecDesc$(0, 0); ", "; indRecDesc$(0, 1) + " " + indRecDesc$(0, 2) + " " + indRecDesc$(0, 3) + ", " + indRecDesc$(0, 4)
    Print

    Color 14: Print "MOST POINTS, ONE HALF"
    Color 15
    Print indRecords!(1, 0); "- "; indRecDesc$(1, 0); ", "; indRecDesc$(1, 1) + " " + indRecDesc$(1, 2) + " " + indRecDesc$(1, 3) + ", " + indRecDesc$(1, 4); HALF$(indRecords!(1, 2))
    Print

    Color 14: Print "MOST POINTS, ONE QUARTER"
    Color 15
    Print indRecords!(2, 0); "- "; indRecDesc$(2, 0); ", "; indRecDesc$(2, 1) + " " + indRecDesc$(2, 2) + " " + indRecDesc$(2, 3) + ", " + indRecDesc$(2, 4); QTR$(indRecords!(2, 2))
    Print

    Color 14: Print "MOST POINTS, OVERTIME PERIOD"
    Color 15
    If indRecords!(3, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(3, 0); "- "; indRecDesc$(3, 0); ", "; indRecDesc$(3, 1) + " " + indRecDesc$(3, 2) + " " + indRecDesc$(3, 3) + ", " + indRecDesc$(3, 4)
    End If
    Print

    Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, GAME (MINIMUM 15 FIELD GOALS)"
    Color 15
    If indRecords!(4, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        If indRecords!(4, 0) = 1 Then
            Print "1.000 ";
        Else
            Print Using " .### "; indRecords!(4, 0);
        End If
        Print "- "; indRecDesc$(4, 0); ", "; indRecDesc$(4, 1) + " " + indRecDesc$(4, 2) + " " + indRecDesc$(4, 3) + ", " + indRecDesc$(4, 4);
        Print Using " (##/##)"; indRecords!(4, 1); indRecords!(4, 2)
    End If
    Print

    Color 14
    Print "MOST FIELD GOALS, NO MISSES, GAME"
    Color 15
    If indRecords!(5, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(5, 0); "- "; indRecDesc$(5, 0); ", "; indRecDesc$(5, 1) + " " + indRecDesc$(5, 2) + " " + indRecDesc$(5, 3) + ", " + indRecDesc$(5, 4)
    End If
    Print

    Color 14
    Print "MOST FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    Color 15
    If indRecords!(6, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(6, 0); "- "; indRecDesc$(6, 0); ", "; indRecDesc$(6, 1) + " " + indRecDesc$(6, 2) + " " + indRecDesc$(6, 3) + ", " + indRecDesc$(6, 4)
    End If

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST FIELD GOALS, GAME"
    Color 15
    Print indRecords!(7, 0); "- "; indRecDesc$(7, 0); ", "; indRecDesc$(7, 1) + " " + indRecDesc$(7, 2) + " " + indRecDesc$(7, 3) + ", " + indRecDesc$(7, 4)
    Print

    Color 14: Print "MOST FIELD GOALS, ONE HALF"
    Color 15
    Print indRecords!(8, 0); "- "; indRecDesc$(8, 0); ", "; indRecDesc$(8, 1) + " " + indRecDesc$(8, 2) + " " + indRecDesc$(8, 3) + ", " + indRecDesc$(8, 4); HALF$(indRecords!(8, 2))
    Print

    Color 14: Print "MOST FIELD GOALS, ONE QUARTER"
    Color 15
    Print indRecords!(9, 0); "- "; indRecDesc$(9, 0); ", "; indRecDesc$(9, 1) + " " + indRecDesc$(9, 2) + " " + indRecDesc$(9, 3) + ", " + indRecDesc$(9, 4); QTR$(indRecords!(9, 2))
    Print

    Color 14: Print "MOST FIELD GOALS, OVERTIME"
    Color 15
    If indRecords!(10, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(10, 0); "- "; indRecDesc$(10, 0); ", "; indRecDesc$(10, 1) + " " + indRecDesc$(10, 2) + " " + indRecDesc$(10, 3) + ", " + indRecDesc$(10, 4)
    End If
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, GAME"
    Color 15
    Print indRecords!(11, 0); "- "; indRecDesc$(11, 0); ", "; indRecDesc$(11, 1) + " " + indRecDesc$(11, 2) + " " + indRecDesc$(11, 3) + ", " + indRecDesc$(11, 4)
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Color 15
    Print indRecords!(12, 0); "- "; indRecDesc$(12, 0); ", "; indRecDesc$(12, 1) + " " + indRecDesc$(12, 2) + " " + indRecDesc$(12, 3) + ", " + indRecDesc$(12, 4); HALF$(indRecords!(12, 2))
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE QUARTER"
    Color 15
    Print indRecords!(13, 0); "- "; indRecDesc$(13, 0); ", "; indRecDesc$(13, 1) + " " + indRecDesc$(13, 2) + " " + indRecDesc$(13, 3) + ", " + indRecDesc$(13, 4); QTR$(indRecords!(13, 2))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    Color 15
    If indRecords!(15, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(15, 0); "- "; indRecDesc$(15, 0); ", "; indRecDesc$(15, 1) + " " + indRecDesc$(15, 2) + " " + indRecDesc$(15, 3) + ", " + indRecDesc$(15, 4)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    Color 15
    If indRecords!(16, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(16, 0); "- "; indRecDesc$(16, 0); ", "; indRecDesc$(16, 1) + " " + indRecDesc$(16, 2) + " " + indRecDesc$(16, 3) + ", " + indRecDesc$(16, 4)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, GAME"
    Color 15
    If indRecords!(17, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(17, 0); "- "; indRecDesc$(17, 0); ", "; indRecDesc$(17, 1) + " " + indRecDesc$(17, 2) + " " + indRecDesc$(17, 3) + ", " + indRecDesc$(17, 4)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS MADE, ONE HALF"
    Color 15
    If indRecords!(18, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(18, 0); "- "; indRecDesc$(18, 0); ", "; indRecDesc$(18, 1) + " " + indRecDesc$(18, 2) + " " + indRecDesc$(18, 3) + ", " + indRecDesc$(18, 4); HALF$(indRecords!(18, 2))
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS MADE, ONE QUARTER"
    Color 15
    If indRecords!(19, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(19, 0); "- "; indRecDesc$(19, 0); ", "; indRecDesc$(19, 1) + " " + indRecDesc$(19, 2) + " " + indRecDesc$(19, 3) + ", " + indRecDesc$(19, 4); QTR$(indRecords!(19, 2))
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    Color 15
    If indRecords!(21, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(21, 0); "- "; indRecDesc$(21, 0); ", "; indRecDesc$(21, 1) + " " + indRecDesc$(21, 2) + " " + indRecDesc$(21, 3) + ", " + indRecDesc$(21, 4)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    Color 15
    If indRecords!(22, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(22, 0); "- "; indRecDesc$(22, 0); ", "; indRecDesc$(22, 1) + " " + indRecDesc$(22, 2) + " " + indRecDesc$(22, 3) + ", " + indRecDesc$(22, 4); HALF$(indRecords!(22, 2))
    End If
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls

    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST FREE THROWS MADE, NO MISSES, GAME"
    Color 15
    If indRecords!(23, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(23, 0); "- "; indRecDesc$(23, 0); ", "; indRecDesc$(23, 1) + " " + indRecDesc$(23, 2) + " " + indRecDesc$(23, 3) + ", " + indRecDesc$(23, 4)
    End If
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, NONE MADE, GAME"
    Color 15
    If indRecords!(24, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(24, 0); "- "; indRecDesc$(24, 0); ", "; indRecDesc$(24, 1) + " " + indRecDesc$(24, 2) + " " + indRecDesc$(24, 3) + ", " + indRecDesc$(24, 4)
    End If
    Print

    Color 14: Print "MOST FREE THROWS MADE, GAME"
    Color 15
    If indRecords!(25, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(25, 0); "- "; indRecDesc$(25, 0); ", "; indRecDesc$(25, 1) + " " + indRecDesc$(25, 2) + " " + indRecDesc$(25, 3) + ", " + indRecDesc$(25, 4)
    End If
    Print

    Color 14: Print "MOST FREE THROWS MADE, ONE HALF"
    Color 15

    If indRecords!(26, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(26, 0); "- "; indRecDesc$(26, 0); ", "; indRecDesc$(26, 1) + " " + indRecDesc$(26, 2) + " " + indRecDesc$(26, 3) + ", " + indRecDesc$(26, 4); HALF$(indRecords!(26, 2))
    End If
    Print

    Color 14: Print "MOST FREE THROWS MADE, ONE QUARTER"
    Color 15

    If indRecords!(27, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(27, 0); "- "; indRecDesc$(27, 0); ", "; indRecDesc$(27, 1) + " " + indRecDesc$(27, 2) + " " + indRecDesc$(27, 3) + ", " + indRecDesc$(27, 4); QTR$(indRecords!(27, 2))
    End If
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, GAME"
    Color 15
    If indRecords!(28, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(28, 0); "- "; indRecDesc$(28, 0); ", "; indRecDesc$(28, 1) + " " + indRecDesc$(28, 2) + " " + indRecDesc$(28, 3) + ", " + indRecDesc$(28, 4)
    End If
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, ONE HALF"
    Color 15
    If indRecords!(29, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(29, 0); "- "; indRecDesc$(29, 0); ", "; indRecDesc$(29, 1) + " " + indRecDesc$(29, 2) + " " + indRecDesc$(29, 3) + ", " + indRecDesc$(29, 4); HALF$(indRecords!(29, 2))
    End If
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, ONE QUARTER"
    Color 15
    If indRecords!(30, 0) = 0 Then
        Print "NO RECORD AT THIS TIME";
    Else
        Print indRecords!(30, 0); "- "; indRecDesc$(30, 0); ", "; indRecDesc$(30, 1) + " " + indRecDesc$(30, 2) + " " + indRecDesc$(30, 3) + ", " + indRecDesc$(30, 4); QTR$(indRecords!(30, 2));
    End If

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST REBOUNDS, GAME"
    Color 15
    If indRecords!(31, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(31, 0); "- "; indRecDesc$(31, 0); ", "; indRecDesc$(31, 1) + " " + indRecDesc$(31, 2) + " " + indRecDesc$(31, 3) + ", " + indRecDesc$(31, 4)
    End If
    Print

    Color 14: Print "MOST REBOUNDS, ONE HALF"
    Color 15
    If indRecords!(32, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(32, 0); "- "; indRecDesc$(32, 0); ", "; indRecDesc$(32, 1) + " " + indRecDesc$(32, 2) + " " + indRecDesc$(32, 3) + ", " + indRecDesc$(32, 4); HALF$(indRecords!(32, 2))
    End If
    Print

    Color 14: Print "MOST REBOUNDS, ONE QUARTER"
    Color 15
    If indRecords!(33, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(33, 0); "- "; indRecDesc$(33, 0); ", "; indRecDesc$(33, 1) + " " + indRecDesc$(33, 2) + " " + indRecDesc$(33, 3) + ", " + indRecDesc$(33, 4); QTR$(indRecords!(33, 2))
    End If
    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, GAME"
    Color 15

    If indRecords!(34, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(34, 0); "- "; indRecDesc$(34, 0); ", "; indRecDesc$(34, 1) + " " + indRecDesc$(34, 2) + " " + indRecDesc$(34, 3) + ", " + indRecDesc$(34, 4)
    End If

    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, ONE HALF"
    Color 15
    If indRecords!(35, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(35, 0); "- "; indRecDesc$(35, 0); ", "; indRecDesc$(35, 1) + " " + indRecDesc$(35, 2) + " " + indRecDesc$(35, 3) + ", " + indRecDesc$(35, 4); HALF$(indRecords!(35, 2))
    End If
    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, ONE QUARTER"
    Color 15
    If indRecords!(36, 0) = 0 Then
        Print "NO RECORD AT THIS TIME";
    Else
        Print indRecords!(36, 0); "- "; indRecDesc$(36, 0); ", "; indRecDesc$(36, 1) + " " + indRecDesc$(36, 2) + " " + indRecDesc$(36, 3) + ", " + indRecDesc$(36, 4); QTR$(indRecords!(36, 2));
    End If

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST DEFENSIVE REBOUNDS, GAME"
    Color 15
    If indRecords!(37, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(37, 0); "- "; indRecDesc$(37, 0); ", "; indRecDesc$(37, 1) + " " + indRecDesc$(37, 2) + " " + indRecDesc$(37, 3) + ", " + indRecDesc$(37, 4)
    End If
    Print

    Color 14: Print "MOST DEFENSIVE REBOUNDS, ONE HALF"
    Color 15
    If indRecords!(38, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(38, 0); "- "; indRecDesc$(38, 0); ", "; indRecDesc$(38, 1) + " " + indRecDesc$(38, 2) + " " + indRecDesc$(38, 3) + ", " + indRecDesc$(38, 4); HALF$(indRecords!(38, 2))
    End If
    Print

    Color 14: Print "MOST DEFENSIVE REBOUNDS, ONE QUARTER"
    Color 15
    If indRecords!(39, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(39, 0); "- "; indRecDesc$(39, 0); ", "; indRecDesc$(39, 1) + " " + indRecDesc$(39, 2) + " " + indRecDesc$(39, 3) + ", " + indRecDesc$(39, 4); QTR$(indRecords!(39, 2))
    End If
    Print

    Color 14: Print "MOST ASSISTS, GAME"
    Color 15
    If indRecords!(40, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(40, 0); "- "; indRecDesc$(40, 0); ", "; indRecDesc$(40, 1) + " " + indRecDesc$(40, 2) + " " + indRecDesc$(40, 3) + ", " + indRecDesc$(40, 4)
    End If
    Print

    Color 14: Print "MOST ASSISTS, ONE HALF"
    Color 15
    If indRecords!(41, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(41, 0); "- "; indRecDesc$(41, 0); ", "; indRecDesc$(41, 1) + " " + indRecDesc$(41, 2) + " " + indRecDesc$(41, 3) + ", " + indRecDesc$(41, 4); HALF$(indRecords!(41, 2))
    End If
    Print

    Color 14: Print "MOST ASSISTS, ONE QUARTER"
    Color 15
    If indRecords!(42, 0) = 0 Then
        Print "NO RECORD AT THIS TIME";
    Else
        Print indRecords!(42, 0); "- "; indRecDesc$(42, 0); ", "; indRecDesc$(42, 1) + " " + indRecDesc$(42, 2) + " " + indRecDesc$(42, 3) + ", " + indRecDesc$(42, 4); QTR$(indRecords!(42, 2));
    End If

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(31); "INDIVIDUAL RECORDS"

    Color 14: Print "MOST STEALS, GAME"
    Color 15
    If indRecords!(43, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(43, 0); "- "; indRecDesc$(43, 0); ", "; indRecDesc$(43, 1) + " " + indRecDesc$(43, 2) + " " + indRecDesc$(43, 3) + ", " + indRecDesc$(43, 4)
    End If
    Print

    Color 14: Print "MOST STEALS, ONE HALF"
    Color 15
    If indRecords!(44, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(44, 0); "- "; indRecDesc$(44, 0); ", "; indRecDesc$(44, 1) + " " + indRecDesc$(44, 2) + " " + indRecDesc$(44, 3) + ", " + indRecDesc$(44, 4); HALF$(indRecords!(44, 2))
    End If
    Print

    Color 14: Print "MOST STEALS, ONE QUARTER"
    Color 15
    If indRecords!(45, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(45, 0); "- "; indRecDesc$(45, 0); ", "; indRecDesc$(45, 1) + " " + indRecDesc$(45, 2) + " " + indRecDesc$(45, 3) + ", " + indRecDesc$(45, 4); QTR$(indRecords!(45, 2))
    End If
    Print

    Color 14: Print "MOST BLOCKS, GAME"
    Color 15
    If indRecords!(46, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(46, 0); "- "; indRecDesc$(46, 0); ", "; indRecDesc$(46, 1) + " " + indRecDesc$(46, 2) + " " + indRecDesc$(46, 3) + ", " + indRecDesc$(46, 4)
    End If
    Print

    Color 14: Print "MOST BLOCKS, ONE HALF"
    Color 15
    If indRecords!(47, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print indRecords!(47, 0); "- "; indRecDesc$(47, 0); ", "; indRecDesc$(47, 1) + " " + indRecDesc$(47, 2) + " " + indRecDesc$(47, 3) + ", " + indRecDesc$(47, 4); HALF$(indRecords!(47, 2))
    End If
    Print

    Color 14: Print "MOST BLOCKS, ONE QUARTER"
    Color 15
    If indRecords!(48, 0) = 0 Then
        Print "NO RECORD AT THIS TIME";
    Else
        Print indRecords!(48, 0); "- "; indRecDesc$(48, 0); ", "; indRecDesc$(48, 1) + " " + indRecDesc$(48, 2) + " " + indRecDesc$(48, 3) + ", " + indRecDesc$(48, 4); QTR$(indRecords!(48, 2));
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
        Print teamRecords!(0, 0); "- "; teamRecDesc$(0, 1);
        Print Using " (###) AT "; teamRecords!(0, 1);
        Print teamRecDesc$(0, 2);: Print Using " (###)"; teamRecords!(0, 2);: Print ", " + teamRecDesc$(0, 3)
    End If
    Print

    Color 14: Print "MOST POINTS, GAME"
    Color 15
    Print teamRecords!(1, 0); "- "; teamRecDesc$(1, 0) + " " + teamRecDesc$(1, 1) + " " + teamRecDesc$(1, 2) + ", " + teamRecDesc$(1, 3)
    Print

    Color 14: Print "FEWEST POINTS, GAME"
    Color 15
    Print teamRecords!(2, 0); "- "; teamRecDesc$(2, 0) + " " + teamRecDesc$(2, 1) + " " + teamRecDesc$(2, 2) + ", " + teamRecDesc$(2, 3)
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(3, 0); "- "; teamRecDesc$(3, 0);
    Print Using " (###) "; teamRecords!(3, 1);: Print teamRecDesc$(3, 1) + " ";
    Print teamRecDesc$(3, 2);: Print Using " (###)"; teamRecords!(3, 2);: Print ", " + teamRecDesc$(3, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(4, 0); "- "; teamRecDesc$(4, 0);
    Print Using " (###) "; teamRecords!(4, 1);: Print teamRecDesc$(4, 1) + " ";
    Print teamRecDesc$(4, 2);: Print Using " (###)"; teamRecords!(4, 2);: Print ", " + teamRecDesc$(4, 3)
    Print

    Color 14: Print "MOST POINTS, FIRST HALF"
    Color 15
    Print teamRecords!(6, 0); "- "; teamRecDesc$(6, 0) + " " + teamRecDesc$(6, 1) + " " + teamRecDesc$(6, 2) + ", " + teamRecDesc$(6, 3)
    Print

    Color 14: Print "FEWEST POINTS, FIRST HALF"
    Color 15
    Print teamRecords!(7, 0); "- "; teamRecDesc$(7, 0) + " " + teamRecDesc$(7, 1) + " " + teamRecDesc$(7, 2) + ", " + teamRecDesc$(7, 3)
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST POINTS, BOTH TEAMS, FIRST HALF"
    Color 15
    Print teamRecords!(8, 0); "- "; teamRecDesc$(8, 0);
    Print Using " (###) "; teamRecords!(8, 1);: Print teamRecDesc$(8, 1) + " ";
    Print teamRecDesc$(8, 2);: Print Using " (###)"; teamRecords!(8, 2);: Print ", " + teamRecDesc$(8, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, FIRST HALF"
    Color 15
    Print teamRecords!(9, 0); "- "; teamRecDesc$(9, 0);
    Print Using " (###) "; teamRecords!(9, 1);: Print teamRecDesc$(9, 1) + " ";
    Print teamRecDesc$(9, 2);: Print Using " (###)"; teamRecords!(9, 2);: Print ", " + teamRecDesc$(9, 3)
    Print

    Color 14: Print "MOST POINTS, SECOND HALF"
    Color 15
    Print teamRecords!(10, 0); "- "; teamRecDesc$(10, 0) + " " + teamRecDesc$(10, 1) + " " + teamRecDesc$(10, 2) + ", " + teamRecDesc$(10, 3)
    Print

    Color 14: Print "FEWEST POINTS, SECOND HALF"
    Color 15
    Print teamRecords!(11, 0); "- "; teamRecDesc$(11, 0) + " " + teamRecDesc$(11, 1) + " " + teamRecDesc$(11, 2) + ", " + teamRecDesc$(11, 3)
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, SECOND HALF"
    Color 15
    Print teamRecords!(12, 0); "- "; teamRecDesc$(12, 0);
    Print Using " (###) "; teamRecords!(12, 1);: Print teamRecDesc$(12, 1) + " ";
    Print teamRecDesc$(12, 2);: Print Using " (###)"; teamRecords!(12, 2);: Print ", " + teamRecDesc$(12, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, SECOND HALF"
    Color 15
    Print teamRecords!(13, 0); "- "; teamRecDesc$(13, 0);
    Print Using " (###) "; teamRecords!(13, 1);: Print teamRecDesc$(13, 1) + " ";
    Print teamRecDesc$(13, 2);: Print Using " (###)"; teamRecords!(13, 2);: Print ", " + teamRecDesc$(13, 3)
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST POINTS, FIRST QUARTER"
    Color 15
    Print teamRecords!(14, 0); "- "; teamRecDesc$(14, 0) + " " + teamRecDesc$(14, 1) + " " + teamRecDesc$(14, 2) + ", " + teamRecDesc$(14, 3)
    Print

    Color 14: Print "FEWEST POINTS, FIRST QUARTER"
    Color 15
    Print teamRecords!(15, 0); "- "; teamRecDesc$(15, 0) + " " + teamRecDesc$(15, 1) + " " + teamRecDesc$(15, 2) + ", " + teamRecDesc$(15, 3)
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, FIRST QUARTER"
    Color 15
    Print teamRecords!(16, 0); "- "; teamRecDesc$(16, 0);
    Print Using " (###) "; teamRecords!(16, 1);: Print teamRecDesc$(16, 1) + " ";
    Print teamRecDesc$(16, 2);: Print Using " (###)"; teamRecords!(16, 2);: Print ", " + teamRecDesc$(16, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, FIRST QUARTER"
    Color 15
    Print teamRecords!(17, 0); "- "; teamRecDesc$(17, 0);
    Print Using " (###) "; teamRecords!(17, 1);: Print teamRecDesc$(17, 1) + " ";
    Print teamRecDesc$(17, 2);: Print Using " (###)"; teamRecords!(17, 2);: Print ", " + teamRecDesc$(17, 3)
    Print

    Color 14: Print "MOST POINTS, SECOND QUARTER"
    Color 15
    Print teamRecords!(18, 0); "- "; teamRecDesc$(18, 0) + " " + teamRecDesc$(18, 1) + " " + teamRecDesc$(18, 2) + ", " + teamRecDesc$(18, 3)
    Print

    Color 14: Print "FEWEST POINTS, SECOND QUARTER"
    Color 15
    Print teamRecords!(19, 0); "- "; teamRecDesc$(19, 0) + " " + teamRecDesc$(19, 1) + " " + teamRecDesc$(19, 2) + ", " + teamRecDesc$(19, 3)
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, SECOND QUARTER"
    Color 15
    Print teamRecords!(20, 0); "- "; teamRecDesc$(20, 0);
    Print Using " (###) "; teamRecords!(20, 1);: Print teamRecDesc$(20, 1) + " ";
    Print teamRecDesc$(20, 2);: Print Using " (###)"; teamRecords!(20, 2);: Print ", " + teamRecDesc$(20, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, SECOND QUARTER"
    Color 15
    Print teamRecords!(21, 0); "- "; teamRecDesc$(21, 0);
    Print Using " (###) "; teamRecords!(21, 1);: Print teamRecDesc$(21, 1) + " ";
    Print teamRecDesc$(21, 2);: Print Using " (###)"; teamRecords!(21, 2);: Print ", " + teamRecDesc$(21, 3);

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST POINTS, THIRD QUARTER"
    Color 15
    Print teamRecords!(22, 0); "- "; teamRecDesc$(22, 0) + " " + teamRecDesc$(22, 1) + " " + teamRecDesc$(22, 2) + ", " + teamRecDesc$(22, 3)
    Print

    Color 14: Print "FEWEST POINTS, THIRD QUARTER"
    Color 15
    Print teamRecords!(23, 0); "- "; teamRecDesc$(23, 0) + " " + teamRecDesc$(23, 1) + " " + teamRecDesc$(23, 2) + ", " + teamRecDesc$(23, 3)
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, THIRD QUARTER"
    Color 15
    Print teamRecords!(24, 0); "- "; teamRecDesc$(24, 0);
    Print Using " (###) "; teamRecords!(24, 1);: Print teamRecDesc$(24, 1) + " ";
    Print teamRecDesc$(24, 2);: Print Using " (###)"; teamRecords!(24, 2);: Print ", " + teamRecDesc$(24, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, THIRD QUARTER"
    Color 15
    Print teamRecords!(25, 0); "- "; teamRecDesc$(25, 0);
    Print Using " (###) "; teamRecords!(25, 1);: Print teamRecDesc$(25, 1) + " ";
    Print teamRecDesc$(25, 2);: Print Using " (###)"; teamRecords!(25, 2);: Print ", " + teamRecDesc$(25, 3)
    Print

    Color 14: Print "MOST POINTS, FOURTH QUARTER"
    Color 15
    Print teamRecords!(26, 0); "- "; teamRecDesc$(26, 0) + " " + teamRecDesc$(26, 1) + " " + teamRecDesc$(26, 2) + ", " + teamRecDesc$(26, 3)
    Print

    Color 14: Print "FEWEST POINTS, FOURTH QUARTER"
    Color 15
    Print teamRecords!(27, 0); "- "; teamRecDesc$(27, 0) + " " + teamRecDesc$(27, 1) + " " + teamRecDesc$(27, 2) + ", " + teamRecDesc$(27, 3)
    Print

    Color 14: Print "MOST POINTS, BOTH TEAMS, FOURTH QUARTER"
    Color 15
    Print teamRecords!(28, 0); "- "; teamRecDesc$(28, 0);
    Print Using " (###) "; teamRecords!(28, 1);: Print teamRecDesc$(28, 1) + " ";
    Print teamRecDesc$(28, 2);: Print Using " (###)"; teamRecords!(28, 2);: Print ", " + teamRecDesc$(28, 3)
    Print

    Color 14: Print "FEWEST POINTS, BOTH TEAMS, FOURTH QUARTER"
    Color 15
    Print teamRecords!(29, 0); "- "; teamRecDesc$(29, 0);
    Print Using " (###) "; teamRecords!(29, 1);: Print teamRecDesc$(29, 1) + " ";
    Print teamRecDesc$(29, 2);: Print Using " (###)"; teamRecords!(29, 2);: Print ", " + teamRecDesc$(29, 3);

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, GAME"
    Color 15
    Print Using ".### "; teamRecords!(30, 0);: Print "- "; teamRecDesc$(30, 0) + " " + teamRecDesc$(30, 1) + " " + teamRecDesc$(30, 2) + ", " + teamRecDesc$(30, 3);: Print Using "###/###"; teamRecords!(30, 1); teamRecords!(30, 2)
    Print

    Color 14: Print "LOWEST FIELD GOAL PERCENTAGE, GAME"
    Color 15
    Print Using ".### "; teamRecords!(31, 0);: Print "- "; teamRecDesc$(31, 0) + " " + teamRecDesc$(31, 1) + " " + teamRecDesc$(31, 2) + ", " + teamRecDesc$(31, 3);: Print Using "###/###"; teamRecords!(31, 1); teamRecords!(31, 2)
    Print

    Color 14: Print "HIGHEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Color 15
    Print Using ".### "; teamRecords!(32, 0);: Print "- "; teamRecDesc$(32, 0);
    Print Using " (.###) "; teamRecords!(32, 1);: Print teamRecDesc$(32, 1) + " ";
    Print teamRecDesc$(32, 2);: Print Using " (.###)"; teamRecords!(32, 2);: Print ", " + teamRecDesc$(32, 3)
    Print

    Color 14: Print "LOWEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Color 15
    Print Using ".### "; teamRecords!(33, 0);: Print "- "; teamRecDesc$(33, 0);
    Print Using " (.###) "; teamRecords!(33, 1);: Print teamRecDesc$(33, 1) + " ";
    Print teamRecDesc$(33, 2);: Print Using " (.###)"; teamRecords!(33, 2);: Print ", " + teamRecDesc$(33, 3)
    Print

    Color 14: Print "MOST FIELD GOALS, GAME"
    Color 15
    Print teamRecords!(34, 0); "- "; teamRecDesc$(34, 0) + " " + teamRecDesc$(34, 1) + " " + teamRecDesc$(34, 2) + ", " + teamRecDesc$(34, 3)
    Print

    Color 14: Print "FEWEST FIELD GOALS, GAME"
    Color 15
    Print teamRecords!(35, 0); "- "; teamRecDesc$(35, 0) + " " + teamRecDesc$(35, 1) + " " + teamRecDesc$(35, 2) + ", " + teamRecDesc$(35, 3)
    Print

    Color 14: Print "MOST FIELD GOALS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(36, 0); "- "; teamRecDesc$(36, 0);
    Print Using " (###) "; teamRecords!(36, 1);: Print teamRecDesc$(36, 1) + " ";
    Print teamRecDesc$(36, 2);: Print Using " (###)"; teamRecords!(36, 2);: Print ", " + teamRecDesc$(36, 3)
    Print

    Color 14: Print "FEWEST FIELD GOALS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(37, 0); "- "; teamRecDesc$(37, 0);
    Print Using " (###) "; teamRecords!(37, 1);: Print teamRecDesc$(37, 1) + " ";
    Print teamRecDesc$(37, 2);: Print Using " (###)"; teamRecords!(37, 2);: Print ", " + teamRecDesc$(37, 3);

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST FIELD GOALS, ONE HALF"
    Color 15
    Print teamRecords!(38, 0); "- "; teamRecDesc$(38, 0) + " " + teamRecDesc$(38, 1) + " " + teamRecDesc$(38, 2) + ", " + teamRecDesc$(38, 3); HALF$(teamRecords!(38, 1))
    Print

    Color 14: Print "MOST FIELD GOALS, BOTH TEAMS, ONE HALF"
    Color 15
    Print teamRecords!(39, 0); "- "; teamRecDesc$(39, 0); " "; teamRecDesc$(39, 1); " "; teamRecDesc$(39, 2); ", "; teamRecDesc$(39, 3); HALF$(teamRecords!(39, 1))
    Print

    Color 14: Print "MOST FIELD GOALS, ONE QUARTER"
    Color 15
    Print teamRecords!(40, 0); "- "; teamRecDesc$(40, 0) + " " + teamRecDesc$(40, 1) + " " + teamRecDesc$(40, 2) + ", " + teamRecDesc$(40, 3); QTR$(teamRecords!(40, 1))
    Print

    Color 14: Print "MOST FIELD GOALS, BOTH TEAMS, ONE QUARTER"
    Color 15
    Print teamRecords!(41, 0); "- "; teamRecDesc$(41, 0); " "; teamRecDesc$(41, 1); " "; teamRecDesc$(41, 2); ", "; teamRecDesc$(41, 3); QTR$(teamRecords!(41, 1))
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, GAME"
    Color 15
    Print teamRecords!(42, 0); "- "; teamRecDesc$(42, 0) + " " + teamRecDesc$(42, 1) + " " + teamRecDesc$(42, 2) + ", " + teamRecDesc$(42, 3)
    Print

    Color 14: Print "FEWEST FIELD GOAL ATTEMPTS, GAME"
    Color 15
    Print teamRecords!(43, 0); "- "; teamRecDesc$(43, 0) + " " + teamRecDesc$(43, 1) + " " + teamRecDesc$(43, 2) + ", " + teamRecDesc$(43, 3)
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(44, 0); "- "; teamRecDesc$(44, 0);
    Print Using " (###) "; teamRecords!(44, 1);: Print teamRecDesc$(44, 1) + " ";
    Print teamRecDesc$(44, 2);: Print Using " (###)"; teamRecords!(44, 2);: Print ", " + teamRecDesc$(44, 3)
    Print

    Color 14: Print "FEWEST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(45, 0); "- "; teamRecDesc$(45, 0);
    Print Using " (###) "; teamRecords!(45, 1);: Print teamRecDesc$(45, 1) + " ";
    Print teamRecDesc$(45, 2);: Print Using " (###)"; teamRecords!(45, 2);: Print ", " + teamRecDesc$(45, 3);

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Color 15
    Print teamRecords!(46, 0); "- "; teamRecDesc$(46, 0) + " " + teamRecDesc$(46, 1) + " " + teamRecDesc$(46, 2) + ", " + teamRecDesc$(46, 3); HALF$(teamRecords!(46, 1))
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE HALF"
    Color 15
    Print teamRecords!(47, 0); "- "; teamRecDesc$(47, 0); " "; teamRecDesc$(47, 1); " "; teamRecDesc$(47, 2); ", "; teamRecDesc$(47, 3); HALF$(teamRecords!(47, 1))
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, ONE QUARTER"
    Color 15
    Print teamRecords!(48, 0); "- "; teamRecDesc$(48, 0) + " " + teamRecDesc$(48, 1) + " " + teamRecDesc$(48, 2) + ", " + teamRecDesc$(48, 3); QTR$(teamRecords!(48, 1))
    Print

    Color 14: Print "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE QUARTER"
    Color 15
    Print teamRecords!(49, 0); "- "; teamRecDesc$(49, 0); " "; teamRecDesc$(49, 1); " "; teamRecDesc$(49, 2); ", "; teamRecDesc$(49, 3); QTR$(teamRecords!(49, 1))
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    Color 15
    If teamRecords!(50, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRecords!(50, 0); "- "; teamRecDesc$(50, 0) + " " + teamRecDesc$(50, 1) + " " + teamRecDesc$(50, 2) + ", " + teamRecDesc$(50, 3)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, NO MISSES, BOTH TEAMS, GAME"
    Color 15
    If teamRecords!(51, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRecords!(51, 0); "- "; teamRecDesc$(51, 0);
        Print Using " (###) "; teamRecords!(51, 1);: Print teamRecDesc$(51, 1) + " ";
        Print teamRecDesc$(51, 2);: Print Using " (###)"; teamRecords!(51, 2);: Print ", " + teamRecDesc$(51, 3)
    End If
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    Color 15
    If teamRecords!(52, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRecords!(52, 0); "- "; teamRecDesc$(52, 0) + " " + teamRecDesc$(52, 1) + " " + teamRecDesc$(52, 2) + ", " + teamRecDesc$(52, 3)
    End If
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST THREE-POINT FIELD GOALS, GAME"
    Color 15
    Print teamRecords!(53, 0); "- "; teamRecDesc$(53, 0) + " " + teamRecDesc$(53, 1) + " " + teamRecDesc$(53, 2) + ", " + teamRecDesc$(53, 3)
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(54, 0); "- "; teamRecDesc$(54, 0);
    Print Using " (###) "; teamRecords!(54, 1);: Print teamRecDesc$(54, 1) + " ";
    Print teamRecDesc$(54, 2);: Print Using " (###)"; teamRecords!(54, 2);: Print ", " + teamRecDesc$(54, 3)
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, ONE HALF"
    Color 15
    Print teamRecords!(55, 0); "- "; teamRecDesc$(55, 0) + " " + teamRecDesc$(55, 1) + " " + teamRecDesc$(55, 2) + ", " + teamRecDesc$(55, 3); HALF$(teamRecords!(55, 1))
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOALS, ONE QUARTER"
    Color 15
    Print teamRecords!(56, 0); "- "; teamRecDesc$(56, 0) + " " + teamRecDesc$(56, 1) + " " + teamRecDesc$(56, 2) + ", " + teamRecDesc$(56, 3); QTR$(teamRecords!(56, 1))
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    Color 15
    Print teamRecords!(57, 0); "- "; teamRecDesc$(57, 0) + " " + teamRecDesc$(57, 1) + " " + teamRecDesc$(57, 2) + ", " + teamRecDesc$(57, 3)
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(58, 0); "- "; teamRecDesc$(58, 0);
    Print Using " (###) "; teamRecords!(58, 1);: Print teamRecDesc$(58, 1) + " ";
    Print teamRecDesc$(58, 2);: Print Using " (###)"; teamRecords!(58, 2);: Print ", " + teamRecDesc$(58, 3)
    Print

    Color 14: Print "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    Color 15
    Print teamRecords!(59, 0); "- "; teamRecDesc$(59, 0) + " " + teamRecDesc$(59, 1) + " " + teamRecDesc$(59, 2) + ", " + teamRecDesc$(59, 3); HALF$(teamRecords!(59, 1))

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST FREE THROWS MADE, NO MISSES, GAME"
    Color 15
    If teamRecords!(60, 0) = 0 Then
        Print "NO RECORD AT THIS TIME"
    Else
        Print teamRecords!(60, 0); "- "; teamRecDesc$(60, 0) + " " + teamRecDesc$(60, 1) + " " + teamRecDesc$(60, 2) + ", " + teamRecDesc$(60, 3)
    End If
    Print

    Color 14: Print "LOWEST FREE THROW PERCENTAGE, GAME"
    Color 15
    Print Using ".### "; teamRecords!(61, 0);: Print "- "; teamRecDesc$(61, 0) + " " + teamRecDesc$(61, 1) + " " + teamRecDesc$(61, 2) + ", " + teamRecDesc$(61, 3);: Print Using "###/###"; teamRecords!(61, 1); teamRecords!(61, 2)
    Print

    Color 14: Print "HIGHEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Color 15
    Print Using ".### "; teamRecords!(62, 0);: Print "- "; teamRecDesc$(62, 0);
    Print Using " (.###) "; teamRecords!(62, 1);: Print teamRecDesc$(62, 1) + " ";
    Print teamRecDesc$(62, 2);: Print Using " (.###)"; teamRecords!(62, 2);: Print ", " + teamRecDesc$(62, 3)
    Print

    Color 14: Print "LOWEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Color 15
    Print Using ".### "; teamRecords!(63, 0);: Print "- "; teamRecDesc$(63, 0);
    Print Using " (.###) "; teamRecords!(63, 1);: Print teamRecDesc$(63, 1) + " ";
    Print teamRecDesc$(63, 2);: Print Using " (.###)"; teamRecords!(63, 2);: Print ", " + teamRecDesc$(63, 3)
    Print

    Color 14: Print "MOST FREE THROWS MADE, GAME"
    Color 15
    Print teamRecords!(64, 0); "- "; teamRecDesc$(64, 0) + " " + teamRecDesc$(64, 1) + " " + teamRecDesc$(64, 2) + ", " + teamRecDesc$(64, 3)
    Print

    Color 14: Print "FEWEST FREE THROWS MADE, GAME"
    Color 15
    Print teamRecords!(65, 0); "- "; teamRecDesc$(65, 0) + " " + teamRecDesc$(65, 1) + " " + teamRecDesc$(65, 2) + ", " + teamRecDesc$(65, 3)
    Print

    Color 14: Print "MOST FREE THROWS MADE, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(66, 0); "- "; teamRecDesc$(66, 0);
    Print Using " (###) "; teamRecords!(66, 1);: Print teamRecDesc$(66, 1) + " ";
    Print teamRecDesc$(66, 2);: Print Using " (###)"; teamRecords!(66, 2);: Print ", " + teamRecDesc$(66, 3)
    Print

    Color 14: Print "FEWEST FREE THROWS MADE, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(67, 0); "- "; teamRecDesc$(67, 0);
    Print Using " (###) "; teamRecords!(67, 1);: Print teamRecDesc$(67, 1) + " ";
    Print teamRecDesc$(67, 2);: Print Using " (###)"; teamRecords!(67, 2);: Print ", " + teamRecDesc$(67, 3);

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST FREE THROWS MADE, ONE HALF"
    Color 15
    Print teamRecords!(68, 0); "- "; teamRecDesc$(68, 0) + " " + teamRecDesc$(68, 1) + " " + teamRecDesc$(68, 2) + ", " + teamRecDesc$(68, 3); HALF$(teamRecords!(68, 1))
    Print

    Color 14: Print "MOST FREE THROWS MADE, ONE QUARTER"
    Color 15
    Print teamRecords!(70, 0); "- "; teamRecDesc$(70, 0) + " " + teamRecDesc$(70, 1) + " " + teamRecDesc$(70, 2) + ", " + teamRecDesc$(70, 3); QTR$(teamRecords!(70, 1))
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, GAME"
    Color 15
    Print teamRecords!(72, 0); "- "; teamRecDesc$(72, 0) + " " + teamRecDesc$(72, 1) + " " + teamRecDesc$(72, 2) + ", " + teamRecDesc$(72, 3)
    Print

    Color 14: Print "FEWEST FREE THROW ATTEMPTS, GAME"
    Color 15
    Print teamRecords!(73, 0); "- "; teamRecDesc$(73, 0) + " " + teamRecDesc$(73, 1) + " " + teamRecDesc$(73, 2) + ", " + teamRecDesc$(73, 3)
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(74, 0); "- "; teamRecDesc$(74, 0);
    Print Using " (###) "; teamRecords!(74, 1);: Print teamRecDesc$(74, 1) + " ";
    Print teamRecDesc$(74, 2);: Print Using " (###)"; teamRecords!(74, 2);: Print ", " + teamRecDesc$(74, 3)
    Print

    Color 14: Print "FEWEST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(75, 0); "- "; teamRecDesc$(75, 0);
    Print Using " (###) "; teamRecords!(75, 1);: Print teamRecDesc$(75, 1) + " ";
    Print teamRecDesc$(75, 2);: Print Using " (###)"; teamRecords!(75, 2);: Print ", " + teamRecDesc$(75, 3)
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, ONE HALF"
    Color 15
    Print teamRecords!(76, 0); "- "; teamRecDesc$(76, 0) + " " + teamRecDesc$(76, 1) + " " + teamRecDesc$(76, 2) + ", " + teamRecDesc$(76, 3); HALF$(teamRecords!(76, 1))
    Print

    Color 14: Print "MOST FREE THROW ATTEMPTS, ONE QUARTER"
    Color 15
    Print teamRecords!(78, 0); "- "; teamRecDesc$(78, 0) + " " + teamRecDesc$(78, 1) + " " + teamRecDesc$(78, 2) + ", " + teamRecDesc$(78, 3); QTR$(teamRecords!(78, 1));

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST REBOUNDS, GAME"
    Color 15
    Print teamRecords!(80, 0); "- "; teamRecDesc$(80, 0) + " " + teamRecDesc$(80, 1) + " " + teamRecDesc$(80, 2) + ", " + teamRecDesc$(80, 3)
    Print

    Color 14: Print "FEWEST REBOUNDS, GAME"
    Color 15
    Print teamRecords!(81, 0); "- "; teamRecDesc$(81, 0) + " " + teamRecDesc$(81, 1) + " " + teamRecDesc$(81, 2) + ", " + teamRecDesc$(81, 3)
    Print

    Color 14: Print "MOST REBOUNDS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(82, 0); "- "; teamRecDesc$(82, 0);
    Print Using " (###) "; teamRecords!(82, 1);: Print teamRecDesc$(82, 1) + " ";
    Print teamRecDesc$(82, 2);: Print Using " (###)"; teamRecords!(82, 2);: Print ", " + teamRecDesc$(82, 3)
    Print

    Color 14: Print "FEWEST REBOUNDS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(83, 0); "- "; teamRecDesc$(83, 0);
    Print Using " (###) "; teamRecords!(83, 1);: Print teamRecDesc$(83, 1) + " ";
    Print teamRecDesc$(83, 2);: Print Using " (###)"; teamRecords!(83, 2);: Print ", " + teamRecDesc$(83, 3)
    Print

    Color 14: Print "MOST REBOUNDS, ONE HALF"
    Color 15
    Print teamRecords!(84, 0); "- "; teamRecDesc$(84, 0) + " " + teamRecDesc$(84, 1) + " " + teamRecDesc$(84, 2) + ", " + teamRecDesc$(84, 3); HALF$(teamRecords!(84, 1))
    Print

    Color 14: Print "MOST REBOUNDS, ONE QUARTER"
    Color 15
    Print teamRecords!(85, 0); "- "; teamRecDesc$(85, 0) + " " + teamRecDesc$(85, 1) + " " + teamRecDesc$(85, 2) + ", " + teamRecDesc$(85, 3); QTR$(teamRecords!(85, 1))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST OFFENSIVE REBOUNDS, GAME"
    Color 15
    Print teamRecords!(86, 0); "- "; teamRecDesc$(86, 0) + " " + teamRecDesc$(86, 1) + " " + teamRecDesc$(86, 2) + ", " + teamRecDesc$(86, 3)
    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(88, 0); "- "; teamRecDesc$(88, 0);
    Print Using " (###) "; teamRecords!(88, 1);: Print teamRecDesc$(88, 1) + " ";
    Print teamRecDesc$(88, 2);: Print Using " (###)"; teamRecords!(88, 2);: Print ", " + teamRecDesc$(88, 3)
    Print

    Color 14: Print "FEWEST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(89, 0); "- "; teamRecDesc$(89, 0);
    Print Using " (###) "; teamRecords!(89, 1);: Print teamRecDesc$(89, 1) + " ";
    Print teamRecDesc$(89, 2);: Print Using " (###)"; teamRecords!(89, 2);: Print ", " + teamRecDesc$(89, 3)
    Print

    Color 14: Print "MOST OFFENSIVE REBOUNDS, ONE HALF"
    Color 15
    Print teamRecords!(90, 0); "- "; teamRecDesc$(90, 0) + " " + teamRecDesc$(90, 1) + " " + teamRecDesc$(90, 2) + ", " + teamRecDesc$(90, 3); HALF$(teamRecords!(90, 1))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST ASSISTS, GAME"
    Color 15
    Print teamRecords!(96, 0); "- "; teamRecDesc$(96, 0) + " " + teamRecDesc$(96, 1) + " " + teamRecDesc$(96, 2) + ", " + teamRecDesc$(96, 3)
    Print

    Color 14: Print "FEWEST ASSISTS, GAME"
    Color 15
    Print teamRecords!(97, 0); "- "; teamRecDesc$(97, 0) + " " + teamRecDesc$(97, 1) + " " + teamRecDesc$(97, 2) + ", " + teamRecDesc$(97, 3)
    Print

    Color 14: Print "MOST ASSISTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(98, 0); "- "; teamRecDesc$(98, 0);
    Print Using " (###) "; teamRecords!(98, 1);: Print teamRecDesc$(98, 1) + " ";
    Print teamRecDesc$(98, 2);: Print Using " (###)"; teamRecords!(98, 2);: Print ", " + teamRecDesc$(98, 3)
    Print

    Color 14: Print "FEWEST ASSISTS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(99, 0); "- "; teamRecDesc$(99, 0);
    Print Using " (###) "; teamRecords!(99, 1);: Print teamRecDesc$(99, 1) + " ";
    Print teamRecDesc$(99, 2);: Print Using " (###)"; teamRecords!(99, 2);: Print ", " + teamRecDesc$(99, 3)
    Print

    Color 14: Print "MOST ASSISTS, ONE HALF"
    Color 15
    Print teamRecords!(100, 0); "- "; teamRecDesc$(100, 0) + " " + teamRecDesc$(100, 1) + " " + teamRecDesc$(100, 2) + ", " + teamRecDesc$(100, 3); HALF$(teamRecords!(100, 1))
    Print

    Color 14: Print "MOST ASSISTS, ONE QUARTER"
    Color 15
    Print teamRecords!(102, 0); "- "; teamRecDesc$(102, 0) + " " + teamRecDesc$(102, 1) + " " + teamRecDesc$(102, 2) + ", " + teamRecDesc$(102, 3); QTR$(teamRecords!(102, 1))
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST PERSONAL FOULS, GAME"
    Color 15
    Print teamRecords!(104, 0); "- "; teamRecDesc$(104, 0) + " " + teamRecDesc$(104, 1) + " " + teamRecDesc$(104, 2) + ", " + teamRecDesc$(104, 3)
    Print

    Color 14: Print "FEWEST PERSONAL FOULS, GAME"
    Color 15
    Print teamRecords!(105, 0); "- "; teamRecDesc$(105, 0) + " " + teamRecDesc$(105, 1) + " " + teamRecDesc$(105, 2) + ", " + teamRecDesc$(105, 3)
    Print

    Color 14: Print "MOST PERSONAL FOULS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(106, 0); "- "; teamRecDesc$(106, 0);
    Print Using " (###) "; teamRecords!(106, 1);: Print teamRecDesc$(106, 1) + " ";
    Print teamRecDesc$(106, 2);: Print Using " (###)"; teamRecords!(106, 2);: Print ", " + teamRecDesc$(106, 3)
    Print

    Color 14: Print "FEWEST PERSONAL FOULS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(107, 0); "- "; teamRecDesc$(107, 0);
    Print Using " (###) "; teamRecords!(107, 1);: Print teamRecDesc$(107, 1) + " ";
    Print teamRecDesc$(107, 2);: Print Using " (###)"; teamRecords!(107, 2);: Print ", " + teamRecDesc$(107, 3)
    Print

    Color 14: Print "MOST PERSONAL FOULS, ONE HALF"
    Color 15
    Print teamRecords!(108, 0); "- "; teamRecDesc$(108, 0) + " " + teamRecDesc$(108, 1) + " " + teamRecDesc$(108, 2) + ", " + teamRecDesc$(108, 3); HALF$(teamRecords!(108, 1))
    Print

    Color 14: Print "MOST PERSONAL FOULS, BOTH TEAMS, ONE HALF"
    Color 15
    Print teamRecords!(109, 0); "- "; teamRecDesc$(109, 0); " "; teamRecDesc$(109, 1); " "; teamRecDesc$(109, 2); ", "; teamRecDesc$(109, 3); HALF$(teamRecords!(109, 1))
    Print

    Color 14: Print "MOST PERSONAL FOULS, ONE QUARTER"
    Color 15
    Print teamRecords!(110, 0); "- "; teamRecDesc$(110, 0) + " " + teamRecDesc$(110, 1) + " " + teamRecDesc$(110, 2) + ", " + teamRecDesc$(110, 3); QTR$(teamRecords!(110, 1))
    Print

    Color 14: Print "MOST PERSONAL FOULS, BOTH TEAMS, ONE QUARTER"
    Color 15
    Print teamRecords!(111, 0); "- "; teamRecDesc$(111, 0); " "; teamRecDesc$(111, 1); " "; teamRecDesc$(111, 2); ", "; teamRecDesc$(111, 3); QTR$(teamRecords!(111, 1));

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST STEALS, GAME"
    Color 15
    Print teamRecords!(112, 0); "- "; teamRecDesc$(112, 0) + " " + teamRecDesc$(112, 1) + " " + teamRecDesc$(112, 2) + ", " + teamRecDesc$(112, 3)
    Print

    Color 14: Print "MOST STEALS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(114, 0); "- "; teamRecDesc$(114, 0);
    Print Using " (###) "; teamRecords!(114, 1);: Print teamRecDesc$(114, 1) + " ";
    Print teamRecDesc$(114, 2);: Print Using " (###)"; teamRecords!(114, 2);: Print ", " + teamRecDesc$(114, 3)
    Print

    Color 14: Print "MOST STEALS, ONE HALF"
    Color 15
    Print teamRecords!(116, 0); "- "; teamRecDesc$(116, 0) + " " + teamRecDesc$(116, 1) + " " + teamRecDesc$(116, 2) + ", " + teamRecDesc$(116, 3); HALF$(teamRecords!(116, 1))
    Print

    Color 14: Print "MOST STEALS, ONE QUARTER"
    Color 15
    Print teamRecords!(117, 0); "- "; teamRecDesc$(117, 0) + " " + teamRecDesc$(117, 1) + " " + teamRecDesc$(117, 2) + ", " + teamRecDesc$(117, 3); QTR$(teamRecords!(117, 1))
    Print

    Color 14: Print "MOST BLOCKS, GAME"
    Color 15
    Print teamRecords!(118, 0); "- "; teamRecDesc$(118, 0) + " " + teamRecDesc$(118, 1) + " " + teamRecDesc$(118, 2) + ", " + teamRecDesc$(118, 3)
    Print

    Color 14: Print "MOST BLOCKS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(119, 0); "- "; teamRecDesc$(119, 0);
    Print Using " (###) "; teamRecords!(119, 1);: Print teamRecDesc$(119, 1) + " ";
    Print teamRecDesc$(119, 2);: Print Using " (###)"; teamRecords!(119, 2);: Print ", " + teamRecDesc$(119, 3)
    Print

    I$ = GetKeyPress$
    If I$ = Chr$(27) Then Exit Sub

    Cls
    Color 12: Print Tab(34); "TEAM RECORDS"

    Color 14: Print "MOST TURNOVERS, GAME"
    Color 15
    Print teamRecords!(122, 0); "- "; teamRecDesc$(122, 0) + " " + teamRecDesc$(122, 1) + " " + teamRecDesc$(122, 2) + ", " + teamRecDesc$(122, 3)
    Print

    Color 14: Print "FEWEST TURNOVERS, GAME"
    Color 15
    Print teamRecords!(123, 0); "- "; teamRecDesc$(123, 0) + " " + teamRecDesc$(123, 1) + " " + teamRecDesc$(123, 2) + ", " + teamRecDesc$(123, 3)
    Print

    Color 14: Print "MOST TURNOVERS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(124, 0); "- "; teamRecDesc$(124, 0);
    Print Using " (###) "; teamRecords!(124, 1);: Print teamRecDesc$(124, 1) + " ";
    Print teamRecDesc$(124, 2);: Print Using " (###)"; teamRecords!(124, 2);: Print ", " + teamRecDesc$(124, 3)
    Print

    Color 14: Print "FEWEST TURNOVERS, BOTH TEAMS, GAME"
    Color 15
    Print teamRecords!(125, 0); "- "; teamRecDesc$(125, 0);
    Print Using " (###) "; teamRecords!(125, 1);: Print teamRecDesc$(125, 1) + " ";
    Print teamRecDesc$(125, 2);: Print Using " (###)"; teamRecords!(125, 2);: Print ", " + teamRecDesc$(125, 3)
    Print

    I$ = GetKeyPress$

End Sub

'----------------------------------------
'      PrintGameRecords Subroutine
'----------------------------------------
'This subroutine formats and prints the 
'records on the default printer
Sub PrintGameRecords ()

    Shared indRecDesc$(), teamRecDesc$()
    Shared indRecords!(), teamRecords!()

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
    Print #1, indRecords!(0, 0); "- "; indRecDesc$(0, 0); ", "; indRecDesc$(0, 1) + " " + indRecDesc$(0, 2) + " " + indRecDesc$(0, 3) + ", " + indRecDesc$(0, 4)
    Print #1,

    Print #1, "MOST POINTS, ONE HALF"
    Print #1, indRecords!(1, 0); "- "; indRecDesc$(1, 0); ", "; indRecDesc$(1, 1) + " " + indRecDesc$(1, 2) + " " + indRecDesc$(1, 3) + ", " + indRecDesc$(1, 4); HALF$(indRecords!(1, 2))
    Print #1,

    Print #1, "MOST POINTS, ONE QUARTER"
    Print #1, indRecords!(2, 0); "- "; indRecDesc$(2, 0); ", "; indRecDesc$(2, 1) + " " + indRecDesc$(2, 2) + " " + indRecDesc$(2, 3) + ", " + indRecDesc$(2, 4); QTR$(indRecords!(2, 2))
    Print #1,

    Print #1, "MOST POINTS, OVERTIME PERIOD"
    If indRecords!(3, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(3, 0); "- "; indRecDesc$(3, 0); ", "; indRecDesc$(3, 1) + " " + indRecDesc$(3, 2) + " " + indRecDesc$(3, 3) + ", " + indRecDesc$(3, 4)
    End If
    Print #1,

    Print #1, "HIGHEST FIELD GOAL PERCENTAGE, GAME (MINIMUM 15 FIELD GOALS)"
    If indRecords!(4, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        If indRecords!(4, 0) = 1 Then
            Print #1, "1.000 ";
        Else
            Print #1, Using " .### "; indRecords!(4, 0);
        End If

        Print #1, "- "; indRecDesc$(4, 0); ", "; indRecDesc$(4, 1) + " " + indRecDesc$(4, 2) + " " + indRecDesc$(4, 3) + ", " + indRecDesc$(4, 4);
        Print #1, Using " (##/##)"; indRecords!(4, 1); indRecords!(4, 2)
    End If
    Print #1,

    Print #1, "MOST FIELD GOALS, NO MISSES, GAME"
    If indRecords!(5, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(5, 0); "- "; indRecDesc$(5, 0); ", "; indRecDesc$(5, 1) + " " + indRecDesc$(5, 2) + " " + indRecDesc$(5, 3) + ", " + indRecDesc$(5, 4)
    End If
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    If indRecords!(6, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(6, 0); "- "; indRecDesc$(6, 0); ", "; indRecDesc$(6, 1) + " " + indRecDesc$(6, 2) + " " + indRecDesc$(6, 3) + ", " + indRecDesc$(6, 4)
    End If
    Print #1,

    Print #1, "MOST FIELD GOALS, GAME"
    Print #1, indRecords!(7, 0); "- "; indRecDesc$(7, 0); ", "; indRecDesc$(7, 1) + " " + indRecDesc$(7, 2) + " " + indRecDesc$(7, 3) + ", " + indRecDesc$(7, 4)
    Print #1,

    Print #1, "MOST FIELD GOALS, ONE HALF"
    Print #1, indRecords!(8, 0); "- "; indRecDesc$(8, 0); ", "; indRecDesc$(8, 1) + " " + indRecDesc$(8, 2) + " " + indRecDesc$(8, 3) + ", " + indRecDesc$(8, 4); HALF$(indRecords!(8, 2))
    Print #1,

    Print #1, "MOST FIELD GOALS, ONE QUARTER"
    Print #1, indRecords!(9, 0); "- "; indRecDesc$(9, 0); ", "; indRecDesc$(9, 1) + " " + indRecDesc$(9, 2) + " " + indRecDesc$(9, 3) + ", " + indRecDesc$(9, 4); QTR$(indRecords!(9, 2))
    Print #1,

    Print #1, "MOST FIELD GOALS, OVERTIME"
    If indRecords!(10, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(10, 0); "- "; indRecDesc$(10, 0); ", "; indRecDesc$(10, 1) + " " + indRecDesc$(10, 2) + " " + indRecDesc$(10, 3) + ", " + indRecDesc$(10, 4)
    End If
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, GAME"
    Print #1, indRecords!(11, 0); "- "; indRecDesc$(11, 0); ", "; indRecDesc$(11, 1) + " " + indRecDesc$(11, 2) + " " + indRecDesc$(11, 3) + ", " + indRecDesc$(11, 4)
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Print #1, indRecords!(12, 0); "- "; indRecDesc$(12, 0); ", "; indRecDesc$(12, 1) + " " + indRecDesc$(12, 2) + " " + indRecDesc$(12, 3) + ", " + indRecDesc$(12, 4); HALF$(indRecords!(12, 2))
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, ONE QUARTER"
    Print #1, indRecords!(13, 0); "- "; indRecDesc$(13, 0); ", "; indRecDesc$(13, 1) + " " + indRecDesc$(13, 2) + " " + indRecDesc$(13, 3) + ", " + indRecDesc$(13, 4); QTR$(indRecords!(13, 2))
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    If indRecords!(15, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(15, 0); "- "; indRecDesc$(15, 0); ", "; indRecDesc$(15, 1) + " " + indRecDesc$(15, 2) + " " + indRecDesc$(15, 3) + ", " + indRecDesc$(15, 4)
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    If indRecords!(16, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(16, 0); "- "; indRecDesc$(16, 0); ", "; indRecDesc$(16, 1) + " " + indRecDesc$(16, 2) + " " + indRecDesc$(16, 3) + ", " + indRecDesc$(16, 4)
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, GAME"
    If indRecords!(17, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(17, 0); "- "; indRecDesc$(17, 0); ", "; indRecDesc$(17, 1) + " " + indRecDesc$(17, 2) + " " + indRecDesc$(17, 3) + ", " + indRecDesc$(17, 4)
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS MADE, ONE HALF"
    If indRecords!(18, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(18, 0); "- "; indRecDesc$(18, 0); ", "; indRecDesc$(18, 1) + " " + indRecDesc$(18, 2) + " " + indRecDesc$(18, 3) + ", " + indRecDesc$(18, 4); HALF$(indRecords!(18, 2))
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS MADE, ONE QUARTER"
    If indRecords!(19, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(19, 0); "- "; indRecDesc$(19, 0); ", "; indRecDesc$(19, 1) + " " + indRecDesc$(19, 2) + " " + indRecDesc$(19, 3) + ", " + indRecDesc$(19, 4); QTR$(indRecords!(19, 2))
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    If indRecords!(21, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(21, 0); "- "; indRecDesc$(21, 0); ", "; indRecDesc$(21, 1) + " " + indRecDesc$(21, 2) + " " + indRecDesc$(21, 3) + ", " + indRecDesc$(21, 4)
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    If indRecords!(22, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(22, 0); "- "; indRecDesc$(22, 0); ", "; indRecDesc$(22, 1) + " " + indRecDesc$(22, 2) + " " + indRecDesc$(22, 3) + ", " + indRecDesc$(22, 4); HALF$(indRecords!(22, 2))
    End If
    Print #1,

    Print #1, "MOST FREE THROWS MADE, NO MISSES, GAME"
    If indRecords!(23, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(23, 0); "- "; indRecDesc$(23, 0); ", "; indRecDesc$(23, 1) + " " + indRecDesc$(23, 2) + " " + indRecDesc$(23, 3) + ", " + indRecDesc$(23, 4)
    End If
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, NONE MADE, GAME"
    If indRecords!(24, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(24, 0); "- "; indRecDesc$(24, 0); ", "; indRecDesc$(24, 1) + " " + indRecDesc$(24, 2) + " " + indRecDesc$(24, 3) + ", " + indRecDesc$(24, 4)
    End If
    Print #1,

    Print #1, "MOST FREE THROWS MADE, GAME"
    If indRecords!(25, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(25, 0); "- "; indRecDesc$(25, 0); ", "; indRecDesc$(25, 1) + " " + indRecDesc$(25, 2) + " " + indRecDesc$(25, 3) + ", " + indRecDesc$(25, 4)
    End If
    Print #1,

    Print #1, "MOST FREE THROWS MADE, ONE HALF"
    If indRecords!(26, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(26, 0); "- "; indRecDesc$(26, 0); ", "; indRecDesc$(26, 1) + " " + indRecDesc$(26, 2) + " " + indRecDesc$(26, 3) + ", " + indRecDesc$(26, 4); HALF$(indRecords!(26, 2))
    End If
    Print #1,

    Print #1, "MOST FREE THROWS MADE, ONE QUARTER"
    If indRecords!(27, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(27, 0); "- "; indRecDesc$(27, 0); ", "; indRecDesc$(27, 1) + " " + indRecDesc$(27, 2) + " " + indRecDesc$(27, 3) + ", " + indRecDesc$(27, 4); QTR$(indRecords!(27, 2))
    End If
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, GAME"
    If indRecords!(28, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(28, 0); "- "; indRecDesc$(28, 0); ", "; indRecDesc$(28, 1) + " " + indRecDesc$(28, 2) + " " + indRecDesc$(28, 3) + ", " + indRecDesc$(28, 4)
    End If
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, ONE HALF"
    If indRecords!(29, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(29, 0); "- "; indRecDesc$(29, 0); ", "; indRecDesc$(29, 1) + " " + indRecDesc$(29, 2) + " " + indRecDesc$(29, 3) + ", " + indRecDesc$(29, 4); HALF$(indRecords!(29, 2))
    End If
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, ONE QUARTER"
    If indRecords!(30, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME";
    Else
        Print #1, indRecords!(30, 0); "- "; indRecDesc$(30, 0); ", "; indRecDesc$(30, 1) + " " + indRecDesc$(30, 2) + " " + indRecDesc$(30, 3) + ", " + indRecDesc$(30, 4); QTR$(indRecords!(30, 2))
    End If
    Print #1,

    Print #1, "MOST REBOUNDS, GAME"
    If indRecords!(31, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(31, 0); "- "; indRecDesc$(31, 0); ", "; indRecDesc$(31, 1) + " " + indRecDesc$(31, 2) + " " + indRecDesc$(31, 3) + ", " + indRecDesc$(31, 4)
    End If
    Print #1,

    Print #1, "MOST REBOUNDS, ONE HALF"
    If indRecords!(32, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(32, 0); "- "; indRecDesc$(32, 0); ", "; indRecDesc$(32, 1) + " " + indRecDesc$(32, 2) + " " + indRecDesc$(32, 3) + ", " + indRecDesc$(32, 4); HALF$(indRecords!(32, 2))
    End If
    Print #1,

    Print #1, "MOST REBOUNDS, ONE QUARTER"
    If indRecords!(33, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(33, 0); "- "; indRecDesc$(33, 0); ", "; indRecDesc$(33, 1) + " " + indRecDesc$(33, 2) + " " + indRecDesc$(33, 3) + ", " + indRecDesc$(33, 4); QTR$(indRecords!(33, 2))
    End If
    Print #1,

    Print #1, "MOST OFFENSIVE REBOUNDS, GAME"
    If indRecords!(34, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(34, 0); "- "; indRecDesc$(34, 0); ", "; indRecDesc$(34, 1) + " " + indRecDesc$(34, 2) + " " + indRecDesc$(34, 3) + ", " + indRecDesc$(34, 4)
    End If
    Print #1,

    Print #1, "MOST OFFENSIVE REBOUNDS, ONE HALF"
    If indRecords!(35, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(35, 0); "- "; indRecDesc$(35, 0); ", "; indRecDesc$(35, 1) + " " + indRecDesc$(35, 2) + " " + indRecDesc$(35, 3) + ", " + indRecDesc$(35, 4); HALF$(indRecords!(35, 2))
    End If
    Print #1,

    Print #1, "MOST OFFENSIVE REBOUNDS, ONE QUARTER"
    If indRecords!(36, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME";
    Else
        Print #1, indRecords!(36, 0); "- "; indRecDesc$(36, 0); ", "; indRecDesc$(36, 1) + " " + indRecDesc$(36, 2) + " " + indRecDesc$(36, 3) + ", " + indRecDesc$(36, 4); QTR$(indRecords!(36, 2))
    End If
    Print #1,

    Print #1, "MOST DEFENSIVE REBOUNDS, GAME"
    If indRecords!(37, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(37, 0); "- "; indRecDesc$(37, 0); ", "; indRecDesc$(37, 1) + " " + indRecDesc$(37, 2) + " " + indRecDesc$(37, 3) + ", " + indRecDesc$(37, 4)
    End If
    Print #1,

    Print #1, "MOST DEFENSIVE REBOUNDS, ONE HALF"
    If indRecords!(38, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(38, 0); "- "; indRecDesc$(38, 0); ", "; indRecDesc$(38, 1) + " " + indRecDesc$(38, 2) + " " + indRecDesc$(38, 3) + ", " + indRecDesc$(38, 4); HALF$(indRecords!(38, 2))
    End If
    Print #1,

    Print #1, "MOST DEFENSIVE REBOUNDS, ONE QUARTER"
    If indRecords!(39, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(39, 0); "- "; indRecDesc$(39, 0); ", "; indRecDesc$(39, 1) + " " + indRecDesc$(39, 2) + " " + indRecDesc$(39, 3) + ", " + indRecDesc$(39, 4); QTR$(indRecords!(39, 2))
    End If
    Print #1,

    Print #1, "MOST ASSISTS, GAME"
    If indRecords!(40, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(40, 0); "- "; indRecDesc$(40, 0); ", "; indRecDesc$(40, 1) + " " + indRecDesc$(40, 2) + " " + indRecDesc$(40, 3) + ", " + indRecDesc$(40, 4)
    End If
    Print #1,

    Print #1, "MOST ASSISTS, ONE HALF"
    If indRecords!(41, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(41, 0); "- "; indRecDesc$(41, 0); ", "; indRecDesc$(41, 1) + " " + indRecDesc$(41, 2) + " " + indRecDesc$(41, 3) + ", " + indRecDesc$(41, 4); HALF$(indRecords!(41, 2))
    End If
    Print #1,

    Print #1, "MOST ASSISTS, ONE QUARTER"
    If indRecords!(42, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(42, 0); "- "; indRecDesc$(42, 0); ", "; indRecDesc$(42, 1) + " " + indRecDesc$(42, 2) + " " + indRecDesc$(42, 3) + ", " + indRecDesc$(42, 4); QTR$(indRecords!(42, 2))
    End If
    Print #1,

    Print #1, "MOST STEALS, GAME"
    If indRecords!(43, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(43, 0); "- "; indRecDesc$(43, 0); ", "; indRecDesc$(43, 1) + " " + indRecDesc$(43, 2) + " " + indRecDesc$(43, 3) + ", " + indRecDesc$(43, 4)
    End If
    Print #1,

    Print #1, "MOST STEALS, ONE HALF"
    If indRecords!(44, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(44, 0); "- "; indRecDesc$(44, 0); ", "; indRecDesc$(44, 1) + " " + indRecDesc$(44, 2) + " " + indRecDesc$(44, 3) + ", " + indRecDesc$(44, 4); HALF$(indRecords!(44, 2))
    End If
    Print #1,

    Print #1, "MOST STEALS, ONE QUARTER"
    If indRecords!(45, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(45, 0); "- "; indRecDesc$(45, 0); ", "; indRecDesc$(45, 1) + " " + indRecDesc$(45, 2) + " " + indRecDesc$(45, 3) + ", " + indRecDesc$(45, 4); QTR$(indRecords!(45, 2))
    End If
    Print #1,

    Print #1, "MOST BLOCKS, GAME"
    If indRecords!(46, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(46, 0); "- "; indRecDesc$(46, 0); ", "; indRecDesc$(46, 1) + " " + indRecDesc$(46, 2) + " " + indRecDesc$(46, 3) + ", " + indRecDesc$(46, 4)
    End If
    Print #1,

    Print #1, "MOST BLOCKS, ONE HALF"
    If indRecords!(47, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, indRecords!(47, 0); "- "; indRecDesc$(47, 0); ", "; indRecDesc$(47, 1) + " " + indRecDesc$(47, 2) + " " + indRecDesc$(47, 3) + ", " + indRecDesc$(47, 4); HALF$(indRecords!(47, 2))
    End If
    Print #1,

    Print #1, "MOST BLOCKS, ONE QUARTER"
    If indRecords!(48, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME";
    Else
        Print #1, indRecords!(48, 0); "- "; indRecDesc$(48, 0); ", "; indRecDesc$(48, 1) + " " + indRecDesc$(48, 2) + " " + indRecDesc$(48, 3) + ", " + indRecDesc$(48, 4); QTR$(indRecords!(48, 2));
    End If
    Print #1, Chr$(12)

    Print #1, "                            TEAM RECORDS"
    Print #1,

    Print #1, "MOST OVERTIME PERIODS"
    If teamRecords!(0, 0) = 0 Then
    Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRecords!(0, 0); "- "; teamRecDesc$(0, 1);
        Print #1, Using " (###) AT "; teamRecords!(0, 1);
        Print #1, teamRecDesc$(0, 2);: Print #1, Using " (###)"; teamRecords!(0, 2);: Print #1, ", " + teamRecDesc$(0, 3)
    End If
    Print #1,

    Print #1, "MOST POINTS, GAME"
    Print #1, teamRecords!(1, 0); "- "; teamRecDesc$(1, 0) + " " + teamRecDesc$(1, 1) + " " + teamRecDesc$(1, 2) + ", " + teamRecDesc$(1, 3)
    Print #1,

    Print #1, "FEWEST POINTS, GAME"
    Print #1, teamRecords!(2, 0); "- "; teamRecDesc$(2, 0) + " " + teamRecDesc$(2, 1) + " " + teamRecDesc$(2, 2) + ", " + teamRecDesc$(2, 3)
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(3, 0); "- "; teamRecDesc$(3, 0);
    Print #1, Using " (###) "; teamRecords!(3, 1);: Print #1, teamRecDesc$(3, 1) + " ";
    Print #1, teamRecDesc$(3, 2);: Print #1, Using " (###)"; teamRecords!(3, 2);: Print #1, ", " + teamRecDesc$(3, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(4, 0); "- "; teamRecDesc$(4, 0);
    Print #1, Using " (###) "; teamRecords!(4, 1);: Print #1, teamRecDesc$(4, 1) + " ";
    Print #1, teamRecDesc$(4, 2);: Print #1, Using " (###)"; teamRecords!(4, 2);: Print #1, ", " + teamRecDesc$(4, 3)
    Print #1,

    Print #1, "MOST POINTS, FIRST HALF"
    Print #1, teamRecords!(6, 0); "- "; teamRecDesc$(6, 0) + " " + teamRecDesc$(6, 1) + " " + teamRecDesc$(6, 2) + ", " + teamRecDesc$(6, 3)
    Print #1,

    Print #1, "FEWEST POINTS, FIRST HALF"
    Print #1, teamRecords!(7, 0); "- "; teamRecDesc$(7, 0) + " " + teamRecDesc$(7, 1) + " " + teamRecDesc$(7, 2) + ", " + teamRecDesc$(7, 3)
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, FIRST HALF"
    Print #1, teamRecords!(8, 0); "- "; teamRecDesc$(8, 0);
    Print #1, Using " (###) "; teamRecords!(8, 1);: Print #1, teamRecDesc$(8, 1) + " ";
    Print #1, teamRecDesc$(8, 2);: Print #1, Using " (###)"; teamRecords!(8, 2);: Print #1, ", " + teamRecDesc$(8, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, FIRST HALF"
    Print #1, teamRecords!(9, 0); "- "; teamRecDesc$(9, 0);
    Print #1, Using " (###) "; teamRecords!(9, 1);: Print #1, teamRecDesc$(9, 1) + " ";
    Print #1, teamRecDesc$(9, 2);: Print #1, Using " (###)"; teamRecords!(9, 2);: Print #1, ", " + teamRecDesc$(9, 3)
    Print #1,

    Print #1, "MOST POINTS, SECOND HALF"
    Print #1, teamRecords!(10, 0); "- "; teamRecDesc$(10, 0) + " " + teamRecDesc$(10, 1) + " " + teamRecDesc$(10, 2) + ", " + teamRecDesc$(10, 3)
    Print #1,

    Print #1, "FEWEST POINTS, SECOND HALF"
    Print #1, teamRecords!(11, 0); "- "; teamRecDesc$(11, 0) + " " + teamRecDesc$(11, 1) + " " + teamRecDesc$(11, 2) + ", " + teamRecDesc$(11, 3)
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, SECOND HALF"
    Print #1, teamRecords!(12, 0); "- "; teamRecDesc$(12, 0);
    Print #1, Using " (###) "; teamRecords!(12, 1);: Print #1, teamRecDesc$(12, 1) + " ";
    Print #1, teamRecDesc$(12, 2);: Print #1, Using " (###)"; teamRecords!(12, 2);: Print #1, ", " + teamRecDesc$(12, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, SECOND HALF"
    Print #1, teamRecords!(13, 0); "- "; teamRecDesc$(13, 0);
    Print #1, Using " (###) "; teamRecords!(13, 1);: Print #1, teamRecDesc$(13, 1) + " ";
    Print #1, teamRecDesc$(13, 2);: Print #1, Using " (###)"; teamRecords!(13, 2);: Print #1, ", " + teamRecDesc$(13, 3)
    Print #1,

    Print #1, "MOST POINTS, FIRST QUARTER"
    Print #1, teamRecords!(14, 0); "- "; teamRecDesc$(14, 0) + " " + teamRecDesc$(14, 1) + " " + teamRecDesc$(14, 2) + ", " + teamRecDesc$(14, 3)
    Print #1,

    Print #1, "FEWEST POINTS, FIRST QUARTER"
    Print #1, teamRecords!(15, 0); "- "; teamRecDesc$(15, 0) + " " + teamRecDesc$(15, 1) + " " + teamRecDesc$(15, 2) + ", " + teamRecDesc$(15, 3)
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, FIRST QUARTER"
    Print #1, teamRecords!(16, 0); "- "; teamRecDesc$(16, 0);
    Print #1, Using " (###) "; teamRecords!(16, 1);: Print #1, teamRecDesc$(16, 1) + " ";
    Print #1, teamRecDesc$(16, 2);: Print #1, Using " (###)"; teamRecords!(16, 2);: Print #1, ", " + teamRecDesc$(16, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, FIRST QUARTER"
    Print #1, teamRecords!(17, 0); "- "; teamRecDesc$(17, 0);
    Print #1, Using " (###) "; teamRecords!(17, 1);: Print #1, teamRecDesc$(17, 1) + " ";
    Print #1, teamRecDesc$(17, 2);: Print #1, Using " (###)"; teamRecords!(17, 2);: Print #1, ", " + teamRecDesc$(17, 3)
    Print #1,

    Print #1, "MOST POINTS, SECOND QUARTER"
    Print #1, teamRecords!(18, 0); "- "; teamRecDesc$(18, 0) + " " + teamRecDesc$(18, 1) + " " + teamRecDesc$(18, 2) + ", " + teamRecDesc$(18, 3)
    Print #1,

    Print #1, "FEWEST POINTS, SECOND QUARTER"
    Print #1, teamRecords!(19, 0); "- "; teamRecDesc$(19, 0) + " " + teamRecDesc$(19, 1) + " " + teamRecDesc$(19, 2) + ", " + teamRecDesc$(19, 3)
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, SECOND QUARTER"
    Print #1, teamRecords!(20, 0); "- "; teamRecDesc$(20, 0);
    Print #1, Using " (###) "; teamRecords!(20, 1);: Print #1, teamRecDesc$(20, 1) + " ";
    Print #1, teamRecDesc$(20, 2);: Print #1, Using " (###)"; teamRecords!(20, 2);: Print #1, ", " + teamRecDesc$(20, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, SECOND QUARTER"
    Print #1, teamRecords!(21, 0); "- "; teamRecDesc$(21, 0);
    Print #1, Using " (###) "; teamRecords!(21, 1);: Print #1, teamRecDesc$(21, 1) + " ";
    Print #1, teamRecDesc$(21, 2);: Print #1, Using " (###)"; teamRecords!(21, 2);: Print #1, ", " + teamRecDesc$(21, 3)
    Print #1,

    Print #1, "MOST POINTS, THIRD QUARTER"
    Print #1, teamRecords!(22, 0); "- "; teamRecDesc$(22, 0) + " " + teamRecDesc$(22, 1) + " " + teamRecDesc$(22, 2) + ", " + teamRecDesc$(22, 3)
    Print #1,

    Print #1, "FEWEST POINTS, THIRD QUARTER"
    Print #1, teamRecords!(23, 0); "- "; teamRecDesc$(23, 0) + " " + teamRecDesc$(23, 1) + " " + teamRecDesc$(23, 2) + ", " + teamRecDesc$(23, 3)
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, THIRD QUARTER"
    Print #1, teamRecords!(24, 0); "- "; teamRecDesc$(24, 0);
    Print #1, Using " (###) "; teamRecords!(24, 1);: Print #1, teamRecDesc$(24, 1) + " ";
    Print #1, teamRecDesc$(24, 2);: Print #1, Using " (###)"; teamRecords!(24, 2);: Print #1, ", " + teamRecDesc$(24, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, THIRD QUARTER"
    Print #1, teamRecords!(25, 0); "- "; teamRecDesc$(25, 0);
    Print #1, Using " (###) "; teamRecords!(25, 1);: Print #1, teamRecDesc$(25, 1) + " ";
    Print #1, teamRecDesc$(25, 2);: Print #1, Using " (###)"; teamRecords!(25, 2);: Print #1, ", " + teamRecDesc$(25, 3)
    Print #1,

    Print #1, "MOST POINTS, FOURTH QUARTER"
    Print #1, teamRecords!(26, 0); "- "; teamRecDesc$(26, 0) + " " + teamRecDesc$(26, 1) + " " + teamRecDesc$(26, 2) + ", " + teamRecDesc$(26, 3)
    Print #1,

    Print #1, "FEWEST POINTS, FOURTH QUARTER"
    Print #1, teamRecords!(27, 0); "- "; teamRecDesc$(27, 0) + " " + teamRecDesc$(27, 1) + " " + teamRecDesc$(27, 2) + ", " + teamRecDesc$(27, 3)
    Print #1,

    Print #1, "MOST POINTS, BOTH TEAMS, FOURTH QUARTER"
    Print #1, teamRecords!(28, 0); "- "; teamRecDesc$(28, 0);
    Print #1, Using " (###) "; teamRecords!(28, 1);: Print #1, teamRecDesc$(28, 1) + " ";
    Print #1, teamRecDesc$(28, 2);: Print #1, Using " (###)"; teamRecords!(28, 2);: Print #1, ", " + teamRecDesc$(28, 3)
    Print #1,

    Print #1, "FEWEST POINTS, BOTH TEAMS, FOURTH QUARTER"
    Print #1, teamRecords!(29, 0); "- "; teamRecDesc$(29, 0);
    Print #1, Using " (###) "; teamRecords!(29, 1);: Print #1, teamRecDesc$(29, 1) + " ";
    Print #1, teamRecDesc$(29, 2);: Print #1, Using " (###)"; teamRecords!(29, 2);: Print #1, ", " + teamRecDesc$(29, 3)
    Print #1,

    Print #1, "HIGHEST FIELD GOAL PERCENTAGE, GAME"
    Print #1, Using ".### "; teamRecords!(30, 0);: Print #1, "- "; teamRecDesc$(30, 0) + " " + teamRecDesc$(30, 1) + " " + teamRecDesc$(30, 2) + ", " + teamRecDesc$(30, 3);: Print #1, Using "###/###"; teamRecords!(30, 1); teamRecords!(30, 2)
    Print #1,

    Print #1, "LOWEST FIELD GOAL PERCENTAGE, GAME"
    Print #1, Using ".### "; teamRecords!(31, 0);: Print #1, "- "; teamRecDesc$(31, 0) + " " + teamRecDesc$(31, 1) + " " + teamRecDesc$(31, 2) + ", " + teamRecDesc$(31, 3);: Print #1, Using "###/###"; teamRecords!(31, 1); teamRecords!(31, 2)
    Print #1,

    Print #1, "HIGHEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Print #1, Using ".### "; teamRecords!(32, 0);: Print #1, "- "; teamRecDesc$(32, 0);
    Print #1, Using " (.###) "; teamRecords!(32, 1);: Print #1, teamRecDesc$(32, 1) + " ";
    Print #1, teamRecDesc$(32, 2);: Print #1, Using " (.###)"; teamRecords!(32, 2);: Print #1, ", " + teamRecDesc$(32, 3)
    Print #1,

    Print #1, "LOWEST FIELD GOAL PERCENTAGE, BOTH TEAMS, GAME"
    Print #1, Using ".### "; teamRecords!(33, 0);: Print #1, "- "; teamRecDesc$(33, 0);
    Print #1, Using " (.###) "; teamRecords!(33, 1);: Print #1, teamRecDesc$(33, 1) + " ";
    Print #1, teamRecDesc$(33, 2);: Print #1, Using " (.###)"; teamRecords!(33, 2);: Print #1, ", " + teamRecDesc$(33, 3)
    Print #1,

    Print #1, "MOST FIELD GOALS, GAME"
    Print #1, teamRecords!(34, 0); "- "; teamRecDesc$(34, 0) + " " + teamRecDesc$(34, 1) + " " + teamRecDesc$(34, 2) + ", " + teamRecDesc$(34, 3)
    Print #1,

    Print #1, "FEWEST FIELD GOALS, GAME"
    Print #1, teamRecords!(35, 0); "- "; teamRecDesc$(35, 0) + " " + teamRecDesc$(35, 1) + " " + teamRecDesc$(35, 2) + ", " + teamRecDesc$(35, 3)
    Print #1,

    Print #1, "MOST FIELD GOALS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(36, 0); "- "; teamRecDesc$(36, 0);
    Print #1, Using " (###) "; teamRecords!(36, 1);: Print #1, teamRecDesc$(36, 1) + " ";
    Print #1, teamRecDesc$(36, 2);: Print #1, Using " (###)"; teamRecords!(36, 2);: Print #1, ", " + teamRecDesc$(36, 3)
    Print #1,

    Print #1, "FEWEST FIELD GOALS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(37, 0); "- "; teamRecDesc$(37, 0);
    Print #1, Using " (###) "; teamRecords!(37, 1);: Print #1, teamRecDesc$(37, 1) + " ";
    Print #1, teamRecDesc$(37, 2);: Print #1, Using " (###)"; teamRecords!(37, 2);: Print #1, ", " + teamRecDesc$(37, 3)
    Print #1,

    Print #1, "MOST FIELD GOALS, ONE HALF"
    Print #1, teamRecords!(38, 0); "- "; teamRecDesc$(38, 0) + " " + teamRecDesc$(38, 1) + " " + teamRecDesc$(38, 2) + ", " + teamRecDesc$(38, 3); HALF$(teamRecords!(38, 1))
    Print #1,

    Print #1, "MOST FIELD GOALS, BOTH TEAMS, ONE HALF"
    Print #1, teamRecords!(39, 0); "- "; teamRecDesc$(39, 0); " "; teamRecDesc$(39, 1); " "; teamRecDesc$(39, 2); ", "; teamRecDesc$(39, 3); HALF$(teamRecords!(39, 1))
    Print #1,

    Print #1, "MOST FIELD GOALS, ONE QUARTER"
    Print #1, teamRecords!(40, 0); "- "; teamRecDesc$(40, 0) + " " + teamRecDesc$(40, 1) + " " + teamRecDesc$(40, 2) + ", " + teamRecDesc$(40, 3); QTR$(teamRecords!(40, 1))
    Print #1,

    Print #1, "MOST FIELD GOALS, BOTH TEAMS, ONE QUARTER"
    Print #1, teamRecords!(41, 0); "- "; teamRecDesc$(41, 0); " "; teamRecDesc$(41, 1); " "; teamRecDesc$(41, 2); ", "; teamRecDesc$(41, 3); QTR$(teamRecords!(41, 1))
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, GAME"
    Print #1, teamRecords!(42, 0); "- "; teamRecDesc$(42, 0) + " " + teamRecDesc$(42, 1) + " " + teamRecDesc$(42, 2) + ", " + teamRecDesc$(42, 3)
    Print #1,

    Print #1, "FEWEST FIELD GOAL ATTEMPTS, GAME"
    Print #1, teamRecords!(43, 0); "- "; teamRecDesc$(43, 0) + " " + teamRecDesc$(43, 1) + " " + teamRecDesc$(43, 2) + ", " + teamRecDesc$(43, 3)
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(44, 0); "- "; teamRecDesc$(44, 0);
    Print #1, Using " (###) "; teamRecords!(44, 1);: Print #1, teamRecDesc$(44, 1) + " ";
    Print #1, teamRecDesc$(44, 2);: Print #1, Using " (###)"; teamRecords!(44, 2);: Print #1, ", " + teamRecDesc$(44, 3)
    Print #1,

    Print #1, "FEWEST FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(45, 0); "- "; teamRecDesc$(45, 0);
    Print #1, Using " (###) "; teamRecords!(45, 1);: Print #1, teamRecDesc$(45, 1) + " ";
    Print #1, teamRecDesc$(45, 2);: Print #1, Using " (###)"; teamRecords!(45, 2);: Print #1, ", " + teamRecDesc$(45, 3)
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, ONE HALF"
    Print #1, teamRecords!(46, 0); "- "; teamRecDesc$(46, 0) + " " + teamRecDesc$(46, 1) + " " + teamRecDesc$(46, 2) + ", " + teamRecDesc$(46, 3); HALF$(teamRecords!(46, 1))
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE HALF"
    Print #1, teamRecords!(47, 0); "- "; teamRecDesc$(47, 0); " "; teamRecDesc$(47, 1); " "; teamRecDesc$(47, 2); ", "; teamRecDesc$(47, 3); HALF$(teamRecords!(47, 1))
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, ONE QUARTER"
    Print #1, teamRecords!(48, 0); "- "; teamRecDesc$(48, 0) + " " + teamRecDesc$(48, 1) + " " + teamRecDesc$(48, 2) + ", " + teamRecDesc$(48, 3); QTR$(teamRecords!(48, 1))
    Print #1,

    Print #1, "MOST FIELD GOAL ATTEMPTS, BOTH TEAMS, ONE QUARTER"
    Print #1, teamRecords!(49, 0); "- "; teamRecDesc$(49, 0); " "; teamRecDesc$(49, 1); " "; teamRecDesc$(49, 2); ", "; teamRecDesc$(49, 3); QTR$(teamRecords!(49, 1))
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, NO MISSES, GAME"
    If teamRecords!(50, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRecords!(50, 0); "- "; teamRecDesc$(50, 0) + " " + teamRecDesc$(50, 1) + " " + teamRecDesc$(50, 2) + ", " + teamRecDesc$(50, 3)
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, NO MISSES, BOTH TEAMS, GAME"
    If teamRecords!(51, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRecords!(51, 0); "- "; teamRecDesc$(51, 0);
        Print #1, Using " (###) "; teamRecords!(51, 1);: Print #1, teamRecDesc$(51, 1) + " ";
        Print #1, teamRecDesc$(51, 2);: Print #1, Using " (###)"; teamRecords!(51, 2);: Print #1, ", " + teamRecDesc$(51, 3)
    End If
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, NONE MADE, GAME"
    If teamRecords!(52, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRecords!(52, 0); "- "; teamRecDesc$(52, 0) + " " + teamRecDesc$(52, 1) + " " + teamRecDesc$(52, 2) + ", " + teamRecDesc$(52, 3)
    End If
    Print #1,
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, GAME"
    Print #1, teamRecords!(53, 0); "- "; teamRecDesc$(53, 0) + " " + teamRecDesc$(53, 1) + " " + teamRecDesc$(53, 2) + ", " + teamRecDesc$(53, 3)
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(54, 0); "- "; teamRecDesc$(54, 0);
    Print #1, Using " (###) "; teamRecords!(54, 1);: Print #1, teamRecDesc$(54, 1) + " ";
    Print #1, teamRecDesc$(54, 2);: Print #1, Using " (###)"; teamRecords!(54, 2);: Print #1, ", " + teamRecDesc$(54, 3)
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, ONE HALF"
    Print #1, teamRecords!(55, 0); "- "; teamRecDesc$(55, 0) + " " + teamRecDesc$(55, 1) + " " + teamRecDesc$(55, 2) + ", " + teamRecDesc$(55, 3); HALF$(teamRecords!(55, 1))
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOALS, ONE QUARTER"
    Print #1, teamRecords!(56, 0); "- "; teamRecDesc$(56, 0) + " " + teamRecDesc$(56, 1) + " " + teamRecDesc$(56, 2) + ", " + teamRecDesc$(56, 3); QTR$(teamRecords!(56, 1))
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, GAME"
    Print #1, teamRecords!(57, 0); "- "; teamRecDesc$(57, 0) + " " + teamRecDesc$(57, 1) + " " + teamRecDesc$(57, 2) + ", " + teamRecDesc$(57, 3)
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(58, 0); "- "; teamRecDesc$(58, 0);
    Print #1, Using " (###) "; teamRecords!(58, 1);: Print #1, teamRecDesc$(58, 1) + " ";
    Print #1, teamRecDesc$(58, 2);: Print #1, Using " (###)"; teamRecords!(58, 2);: Print #1, ", " + teamRecDesc$(58, 3)
    Print #1,

    Print #1, "MOST THREE-POINT FIELD GOAL ATTEMPTS, ONE HALF"
    Print #1, teamRecords!(59, 0); "- "; teamRecDesc$(59, 0) + " " + teamRecDesc$(59, 1) + " " + teamRecDesc$(59, 2) + ", " + teamRecDesc$(59, 3); HALF$(teamRecords!(59, 1))
    Print #1,

    Print #1, "MOST FREE THROWS MADE, NO MISSES, GAME"
    If teamRecords!(60, 0) = 0 Then
        Print #1, "NO RECORD AT THIS TIME"
    Else
        Print #1, teamRecords!(60, 0); "- "; teamRecDesc$(60, 0) + " " + teamRecDesc$(60, 1) + " " + teamRecDesc$(60, 2) + ", " + teamRecDesc$(60, 3)
    End If
    Print #1,

    Print #1, "LOWEST FREE THROW PERCENTAGE, GAME"
    Print #1, Using ".### "; teamRecords!(61, 0);: Print #1, "- "; teamRecDesc$(61, 0) + " " + teamRecDesc$(61, 1) + " " + teamRecDesc$(61, 2) + ", " + teamRecDesc$(61, 3);: Print #1, Using "###/###"; teamRecords!(61, 1); teamRecords!(61, 2)
    Print #1,

    Print #1, "HIGHEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Print #1, Using ".### "; teamRecords!(62, 0);: Print #1, "- "; teamRecDesc$(62, 0);
    Print #1, Using " (.###) "; teamRecords!(62, 1);: Print #1, teamRecDesc$(62, 1) + " ";
    Print #1, teamRecDesc$(62, 2);: Print #1, Using " (.###)"; teamRecords!(62, 2);: Print #1, ", " + teamRecDesc$(62, 3)
    Print #1,

    Print #1, "LOWEST FREE THROW PERCENTAGE, BOTH TEAMS, GAME"
    Print #1, Using ".### "; teamRecords!(63, 0);: Print #1, "- "; teamRecDesc$(63, 0);
    Print #1, Using " (.###) "; teamRecords!(63, 1);: Print #1, teamRecDesc$(63, 1) + " ";
    Print #1, teamRecDesc$(63, 2);: Print #1, Using " (.###)"; teamRecords!(63, 2);: Print #1, ", " + teamRecDesc$(63, 3)
    Print #1,

    Print #1, "MOST FREE THROWS MADE, GAME"
    Print #1, teamRecords!(64, 0); "- "; teamRecDesc$(64, 0) + " " + teamRecDesc$(64, 1) + " " + teamRecDesc$(64, 2) + ", " + teamRecDesc$(64, 3)
    Print #1,

    Print #1, "FEWEST FREE THROWS MADE, GAME"
    Print #1, teamRecords!(65, 0); "- "; teamRecDesc$(65, 0) + " " + teamRecDesc$(65, 1) + " " + teamRecDesc$(65, 2) + ", " + teamRecDesc$(65, 3)
    Print #1,

    Print #1, "MOST FREE THROWS MADE, BOTH TEAMS, GAME"
    Print #1, teamRecords!(66, 0); "- "; teamRecDesc$(66, 0);
    Print #1, Using " (###) "; teamRecords!(66, 1);: Print #1, teamRecDesc$(66, 1) + " ";
    Print #1, teamRecDesc$(66, 2);: Print #1, Using " (###)"; teamRecords!(66, 2);: Print #1, ", " + teamRecDesc$(66, 3)
    Print #1,

    Print #1, "FEWEST FREE THROWS MADE, BOTH TEAMS, GAME"
    Print #1, teamRecords!(67, 0); "- "; teamRecDesc$(67, 0);
    Print #1, Using " (###) "; teamRecords!(67, 1);: Print #1, teamRecDesc$(67, 1) + " ";
    Print #1, teamRecDesc$(67, 2);: Print #1, Using " (###)"; teamRecords!(67, 2);: Print #1, ", " + teamRecDesc$(67, 3)
    Print #1,

    Print #1, "MOST FREE THROWS MADE, ONE HALF"
    Print #1, teamRecords!(68, 0); "- "; teamRecDesc$(68, 0) + " " + teamRecDesc$(68, 1) + " " + teamRecDesc$(68, 2) + ", " + teamRecDesc$(68, 3); HALF$(teamRecords!(68, 1))
    Print #1,

    Print #1, "MOST FREE THROWS MADE, ONE QUARTER"
    Print #1, teamRecords!(70, 0); "- "; teamRecDesc$(70, 0) + " " + teamRecDesc$(70, 1) + " " + teamRecDesc$(70, 2) + ", " + teamRecDesc$(70, 3); QTR$(teamRecords!(70, 1))
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, GAME"
    Print #1, teamRecords!(72, 0); "- "; teamRecDesc$(72, 0) + " " + teamRecDesc$(72, 1) + " " + teamRecDesc$(72, 2) + ", " + teamRecDesc$(72, 3)
    Print #1,

    Print #1, "FEWEST FREE THROW ATTEMPTS, GAME"
    Print #1, teamRecords!(73, 0); "- "; teamRecDesc$(73, 0) + " " + teamRecDesc$(73, 1) + " " + teamRecDesc$(73, 2) + ", " + teamRecDesc$(73, 3)
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(74, 0); "- "; teamRecDesc$(74, 0);
    Print #1, Using " (###) "; teamRecords!(74, 1);: Print #1, teamRecDesc$(74, 1) + " ";
    Print #1, teamRecDesc$(74, 2);: Print #1, Using " (###)"; teamRecords!(74, 2);: Print #1, ", " + teamRecDesc$(74, 3)
    Print #1,

    Print #1, "FEWEST FREE THROW ATTEMPTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(75, 0); "- "; teamRecDesc$(75, 0);
    Print #1, Using " (###) "; teamRecords!(75, 1);: Print #1, teamRecDesc$(75, 1) + " ";
    Print #1, teamRecDesc$(75, 2);: Print #1, Using " (###)"; teamRecords!(75, 2);: Print #1, ", " + teamRecDesc$(75, 3)
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, ONE HALF"
    Print #1, teamRecords!(76, 0); "- "; teamRecDesc$(76, 0) + " " + teamRecDesc$(76, 1) + " " + teamRecDesc$(76, 2) + ", " + teamRecDesc$(76, 3); HALF$(teamRecords!(76, 1))
    Print #1,

    Print #1, "MOST FREE THROW ATTEMPTS, ONE QUARTER"
    Print #1, teamRecords!(78, 0); "- "; teamRecDesc$(78, 0) + " " + teamRecDesc$(78, 1) + " " + teamRecDesc$(78, 2) + ", " + teamRecDesc$(78, 3); QTR$(teamRecords!(78, 1))
    Print #1,

    Print #1, "MOST REBOUNDS, GAME"
    Print #1, teamRecords!(80, 0); "- "; teamRecDesc$(80, 0) + " " + teamRecDesc$(80, 1) + " " + teamRecDesc$(80, 2) + ", " + teamRecDesc$(80, 3)
    Print #1,

    Print #1, "FEWEST REBOUNDS, GAME"
    Print #1, teamRecords!(81, 0); "- "; teamRecDesc$(81, 0) + " " + teamRecDesc$(81, 1) + " " + teamRecDesc$(81, 2) + ", " + teamRecDesc$(81, 3)
    Print #1,

    Print #1, "MOST REBOUNDS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(82, 0); "- "; teamRecDesc$(82, 0);
    Print #1, Using " (###) "; teamRecords!(82, 1);: Print #1, teamRecDesc$(82, 1) + " ";
    Print #1, teamRecDesc$(82, 2);: Print #1, Using " (###)"; teamRecords!(82, 2);: Print #1, ", " + teamRecDesc$(82, 3)
    Print #1,

    Print #1, "FEWEST REBOUNDS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(83, 0); "- "; teamRecDesc$(83, 0);
    Print #1, Using " (###) "; teamRecords!(83, 1);: Print #1, teamRecDesc$(83, 1) + " ";
    Print #1, teamRecDesc$(83, 2);: Print #1, Using " (###)"; teamRecords!(83, 2);: Print #1, ", " + teamRecDesc$(83, 3)
    Print #1,

    Print #1, "MOST REBOUNDS, ONE HALF"
    Print #1, teamRecords!(84, 0); "- "; teamRecDesc$(84, 0) + " " + teamRecDesc$(84, 1) + " " + teamRecDesc$(84, 2) + ", " + teamRecDesc$(84, 3); HALF$(teamRecords!(84, 1))
    Print #1,

    Print #1, "MOST REBOUNDS, ONE QUARTER"
    Print #1, teamRecords!(85, 0); "- "; teamRecDesc$(85, 0) + " " + teamRecDesc$(85, 1) + " " + teamRecDesc$(85, 2) + ", " + teamRecDesc$(85, 3); QTR$(teamRecords!(85, 1))
    Print #1,
    Print #1,

    Print #1, "MOST OFFENSIVE REBOUNDS, GAME"
    Print #1, teamRecords!(86, 0); "- "; teamRecDesc$(86, 0) + " " + teamRecDesc$(86, 1) + " " + teamRecDesc$(86, 2) + ", " + teamRecDesc$(86, 3)
    Print #1,

    Print #1, "MOST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(88, 0); "- "; teamRecDesc$(88, 0);
    Print #1, Using " (###) "; teamRecords!(88, 1);: Print #1, teamRecDesc$(88, 1) + " ";
    Print #1, teamRecDesc$(88, 2);: Print #1, Using " (###)"; teamRecords!(88, 2);: Print #1, ", " + teamRecDesc$(88, 3)
    Print #1,

    Print #1, "FEWEST OFFENSIVE REBOUNDS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(89, 0); "- "; teamRecDesc$(89, 0);
    Print #1, Using " (###) "; teamRecords!(89, 1);: Print #1, teamRecDesc$(89, 1) + " ";
    Print #1, teamRecDesc$(89, 2);: Print #1, Using " (###)"; teamRecords!(89, 2);: Print #1, ", " + teamRecDesc$(89, 3)
    Print #1,

    Print #1, "MOST OFFENSIVE REBOUNDS, ONE HALF"
    Print #1, teamRecords!(90, 0); "- "; teamRecDesc$(90, 0) + " " + teamRecDesc$(90, 1) + " " + teamRecDesc$(90, 2) + ", " + teamRecDesc$(90, 3); HALF$(teamRecords!(90, 1))
    Print #1,

    Print #1, "MOST ASSISTS, GAME"
    Print #1, teamRecords!(96, 0); "- "; teamRecDesc$(96, 0) + " " + teamRecDesc$(96, 1) + " " + teamRecDesc$(96, 2) + ", " + teamRecDesc$(96, 3)
    Print #1,

    Print #1, "FEWEST ASSISTS, GAME"
    Print #1, teamRecords!(97, 0); "- "; teamRecDesc$(97, 0) + " " + teamRecDesc$(97, 1) + " " + teamRecDesc$(97, 2) + ", " + teamRecDesc$(97, 3)
    Print #1,

    Print #1, "MOST ASSISTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(98, 0); "- "; teamRecDesc$(98, 0);
    Print #1, Using " (###) "; teamRecords!(98, 1);: Print #1, teamRecDesc$(98, 1) + " ";
    Print #1, teamRecDesc$(98, 2);: Print #1, Using " (###)"; teamRecords!(98, 2);: Print #1, ", " + teamRecDesc$(98, 3)
    Print #1,

    Print #1, "FEWEST ASSISTS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(99, 0); "- "; teamRecDesc$(99, 0);
    Print #1, Using " (###) "; teamRecords!(99, 1);: Print #1, teamRecDesc$(99, 1) + " ";
    Print #1, teamRecDesc$(99, 2);: Print #1, Using " (###)"; teamRecords!(99, 2);: Print #1, ", " + teamRecDesc$(99, 3)
    Print #1,

    Print #1, "MOST ASSISTS, ONE HALF"
    Print #1, teamRecords!(100, 0); "- "; teamRecDesc$(100, 0) + " " + teamRecDesc$(100, 1) + " " + teamRecDesc$(100, 2) + ", " + teamRecDesc$(100, 3); HALF$(teamRecords!(100, 1))
    Print #1,

    Print #1, "MOST ASSISTS, ONE QUARTER"
    Print #1, teamRecords!(102, 0); "- "; teamRecDesc$(102, 0) + " " + teamRecDesc$(102, 1) + " " + teamRecDesc$(102, 2) + ", " + teamRecDesc$(102, 3); QTR$(teamRecords!(102, 1))
    Print #1,

    Print #1, "MOST PERSONAL FOULS, GAME"
    Print #1, teamRecords!(104, 0); "- "; teamRecDesc$(104, 0) + " " + teamRecDesc$(104, 1) + " " + teamRecDesc$(104, 2) + ", " + teamRecDesc$(104, 3)
    Print #1,

    Print #1, "FEWEST PERSONAL FOULS, GAME"
    Print #1, teamRecords!(105, 0); "- "; teamRecDesc$(105, 0) + " " + teamRecDesc$(105, 1) + " " + teamRecDesc$(105, 2) + ", " + teamRecDesc$(105, 3)
    Print #1,

    Print #1, "MOST PERSONAL FOULS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(106, 0); "- "; teamRecDesc$(106, 0);
    Print #1, Using " (###) "; teamRecords!(106, 1);: Print #1, teamRecDesc$(106, 1) + " ";
    Print #1, teamRecDesc$(106, 2);: Print #1, Using " (###)"; teamRecords!(106, 2);: Print #1, ", " + teamRecDesc$(106, 3)
    Print #1,

    Print #1, "FEWEST PERSONAL FOULS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(107, 0); "- "; teamRecDesc$(107, 0);
    Print #1, Using " (###) "; teamRecords!(107, 1);: Print #1, teamRecDesc$(107, 1) + " ";
    Print #1, teamRecDesc$(107, 2);: Print #1, Using " (###)"; teamRecords!(107, 2);: Print #1, ", " + teamRecDesc$(107, 3)
    Print #1,

    Print #1, "MOST PERSONAL FOULS, ONE HALF"
    Print #1, teamRecords!(108, 0); "- "; teamRecDesc$(108, 0) + " " + teamRecDesc$(108, 1) + " " + teamRecDesc$(108, 2) + ", " + teamRecDesc$(108, 3); HALF$(teamRecords!(108, 1))
    Print #1,

    Print #1, "MOST PERSONAL FOULS, BOTH TEAMS, ONE HALF"
    Print #1, teamRecords!(109, 0); "- "; teamRecDesc$(109, 0); " "; teamRecDesc$(109, 1); " "; teamRecDesc$(109, 2); ", "; teamRecDesc$(109, 3); HALF$(teamRecords!(109, 1))
    Print #1,

    Print #1, "MOST PERSONAL FOULS, ONE QUARTER"
    Print #1, teamRecords!(110, 0); "- "; teamRecDesc$(110, 0) + " " + teamRecDesc$(110, 1) + " " + teamRecDesc$(110, 2) + ", " + teamRecDesc$(110, 3); QTR$(teamRecords!(110, 1))
    Print #1,

    Print #1, "MOST PERSONAL FOULS, BOTH TEAMS, ONE QUARTER"
    Print #1, teamRecords!(111, 0); "- "; teamRecDesc$(111, 0); " "; teamRecDesc$(111, 1); " "; teamRecDesc$(111, 2); ", "; teamRecDesc$(111, 3); QTR$(teamRecords!(111, 1));

    Print #1,

    Print #1, "MOST STEALS, GAME"
    Print #1, teamRecords!(112, 0); "- "; teamRecDesc$(112, 0) + " " + teamRecDesc$(112, 1) + " " + teamRecDesc$(112, 2) + ", " + teamRecDesc$(112, 3)
    Print #1,

    Print #1, "MOST STEALS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(114, 0); "- "; teamRecDesc$(114, 0);
    Print #1, Using " (###) "; teamRecords!(114, 1);: Print #1, teamRecDesc$(114, 1) + " ";
    Print #1, teamRecDesc$(114, 2);: Print #1, Using " (###)"; teamRecords!(114, 2);: Print #1, ", " + teamRecDesc$(114, 3)
    Print #1,

    Print #1, "MOST STEALS, ONE HALF"
    Print #1, teamRecords!(116, 0); "- "; teamRecDesc$(116, 0) + " " + teamRecDesc$(116, 1) + " " + teamRecDesc$(116, 2) + ", " + teamRecDesc$(116, 3); HALF$(teamRecords!(116, 1))
    Print #1,

    Print #1, "MOST STEALS, ONE QUARTER"
    Print #1, teamRecords!(117, 0); "- "; teamRecDesc$(117, 0) + " " + teamRecDesc$(117, 1) + " " + teamRecDesc$(117, 2) + ", " + teamRecDesc$(117, 3); QTR$(teamRecords!(117, 1))
    Print #1,

    Print #1, "MOST BLOCKS, GAME"
    Print #1, teamRecords!(118, 0); "- "; teamRecDesc$(118, 0) + " " + teamRecDesc$(118, 1) + " " + teamRecDesc$(118, 2) + ", " + teamRecDesc$(118, 3)
    Print #1,

    Print #1, "MOST BLOCKS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(119, 0); "- "; teamRecDesc$(119, 0);
    Print #1, Using " (###) "; teamRecords!(119, 1);: Print #1, teamRecDesc$(119, 1) + " ";
    Print #1, teamRecDesc$(119, 2);: Print #1, Using " (###)"; teamRecords!(119, 2);: Print #1, ", " + teamRecDesc$(119, 3)
    Print #1,

    Print #1, "MOST TURNOVERS, GAME"
    Print #1, teamRecords!(122, 0); "- "; teamRecDesc$(122, 0) + " " + teamRecDesc$(122, 1) + " " + teamRecDesc$(122, 2) + ", " + teamRecDesc$(122, 3)
    Print #1,

    Print #1, "FEWEST TURNOVERS, GAME"
    Print #1, teamRecords!(123, 0); "- "; teamRecDesc$(123, 0) + " " + teamRecDesc$(123, 1) + " " + teamRecDesc$(123, 2) + ", " + teamRecDesc$(123, 3)
    Print #1,

    Print #1, "MOST TURNOVERS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(124, 0); "- "; teamRecDesc$(124, 0);
    Print #1, Using " (###) "; teamRecords!(124, 1);: Print #1, teamRecDesc$(124, 1) + " ";
    Print #1, teamRecDesc$(124, 2);: Print #1, Using " (###)"; teamRecords!(124, 2);: Print #1, ", " + teamRecDesc$(124, 3)
    Print #1,

    Print #1, "FEWEST TURNOVERS, BOTH TEAMS, GAME"
    Print #1, teamRecords!(125, 0); "- "; teamRecDesc$(125, 0);
    Print #1, Using " (###) "; teamRecords!(125, 1);: Print #1, teamRecDesc$(125, 1) + " ";
    Print #1, teamRecDesc$(125, 2);: Print #1, Using " (###)"; teamRecords!(125, 2);: Print #1, ", " + teamRecDesc$(125, 3)

    Close #1

    Open "RECORDS" For Input As #2

    Do While Not EOF(2)
        Line Input #2, X$
        LPrint X$
    Loop

    LPrint Chr$(12)

    Close #2

End Sub

