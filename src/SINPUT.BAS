Sub EditStats ()

    _Title "Basketball: The Pro Game - Stats Input"

    Shared compareZ!(), compareZ1!()
    Shared compareW0!(), compareW1!()
    
    Dim inputA$(14)
    Dim Z2(14, 14)

    Call ChooseTeam (YN$, NTMS, teamNbr, team$)
	statFile$ = nameStatFile$(team$)

    If Not _FileExists(diskPaths$(1) + statFile$) Then

        Locate 10, 8
        Call CSMsgBox("NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Open diskPaths$(1) + statFile$ For Input As #1

        Input #1, A$

        For I = 0 To 14: 

            Input #1, inputA$(I)

            For J = 0 To 13: 
                Input #1, Z2(I, J)
            Next J

            Input #1, compareW0!(I)

            Input #1, compareW1!(I)

        Next I

        For L = 0 To 15: 
            Input #1, compareZ!(L)
        Next L

        For L = 0 To 15: 
            Input #1, compareZ1!(L)
        Next L

        Close 1

        '---------------
        ' Team Stats
        '---------------
        Color 15, 0
        Cls

        X$ = "OFFENSIVE TEAM STATS"
        Call TITLE_STAT_HEADER(X$)

        Locate 3, 1
        Print "1. STAT FILE NAME "; statFile$
        Print "2. TM MINUTES ";
        Print Using "####"; Int(compareZ!(0) / 60)
        Print "3. TM FGA "; compareZ!(1)
        Print "4. TM FGM "; compareZ!(2)
        Print "5. TM 3FGA "; compareZ!(14)
        Print "6. TM 3FGM "; compareZ!(13)
        Print "7. TM FTA "; compareZ!(3)
        Print "8. TM FTM "; compareZ!(4)
        Print "9. TM OFF RB  "; compareZ!(12)
        Print "10. TM TOT RB "; compareZ!(5)
        Print "11. TM FOULS "; compareZ!(6)
        Print "12. TM STEALS "; compareZ!(7)
        Print "13. TM ASSISTS "; compareZ!(8)
        Print "14. TM BLOCKS "; compareZ!(9)
        Print "15. TM TURNOVERS "; compareZ!(11)
        Print "16. TM POINTS  "; compareZ!(10)

        Do

            Do
                Locate 22, 1
                Input "# TO CHANGE (0 TO CONTINUE) "; B9
            Loop Until B9 >= 0 And B9 <= 16

            Select Case B9
                
                Case 1:
                    Locate 3, 1: Input "1. STAT FILE NAME "; statFile$
                
                Case 2:
                    Locate 4, 1: Input "2. TM MINUTES "; compareZ!(0)
                    compareZ!(0) = compareZ!(0) * 60
                
                Case 3:
                    Locate 5, 1: Input "3. TM FGA "; compareZ!(1)
                
                Case 4:
                    Locate 6, 1: Input "4. TM FGM "; compareZ!(2)
                
                Case 5:
                    Locate 7, 1: Input "5. TM 3FGA "; compareZ!(14)
                
                Case 6:
                    Locate 8, 1: Input "6. TM 3FGM "; compareZ!(13)
            
                Case 7:
                    Locate 9, 1: Input "7. TM FTA "; compareZ!(3)

                Case 8:
                    Locate 10, 1: Input "8. TM FTM "; compareZ!(4)

                Case 9:
                    Locate 11, 1: Input "9. TM OFF RB  "; compareZ!(12)

                Case 10:
                    Locate 12, 1: Input "10. TM TOT RB "; compareZ!(5)

                Case 11:
                    Locate 13, 1: Input "11. TM FOULS "; compareZ!(6)

                Case 12:
                    Locate 14, 1: Input "12. TM STEALS "; compareZ!(7)

                Case 13:
                    Locate 15, 1: Input "13. TM ASSISTS "; compareZ!(8)

                Case 14:
                    Locate 16, 1: Input "14. TM BLOCKS "; compareZ!(9)

                Case 15:
                    Locate 17, 1: Input "15. TM TURNOVERS "; compareZ!(11)

                Case 16:
                    Locate 18, 1: Input "16. TM POINTS  "; compareZ!(10)

            End Select

        Loop Until B9 = 0

        '---------------
        ' Defense Stats
        '---------------
        Cls
        X$ = "DEFENSIVE TEAM STATS"
        Call TITLE_STAT_HEADER(X$)

        Locate 3, 1
        Print "1. OPP FGA "; compareZ1!(1)
        Print "2. OPP FGM "; compareZ1!(2)
        Print "3. OPP 3FGA "; compareZ1!(14)
        Print "4. OPP 3FGM "; compareZ1!(13)
        Print "5. OPP FTA "; compareZ1!(3)
        Print "6. OPP FTM "; compareZ1!(4)
        Print "7. OPP OFF RB  "; compareZ1!(12)
        Print "8. OPP TOT RB "; compareZ1!(5)
        Print "9. OPP FOULS "; compareZ1!(6)
        Print "10. OPP STEALS "; compareZ1!(7)
        Print "11. OPP ASSISTS "; compareZ1!(8)
        Print "12. OPP BLOCKS "; compareZ1!(9)
        Print "13. OPP TURNOVERS "; compareZ1!(11)
        Print "14. OPP POINTS  "; compareZ1!(10)

        Do

            Do
                Locate 22, 1
                Input "# TO CHANGE (0 TO CONTINUE) "; B9
            Loop Until B9 >= 0 And B9 <= 14

            Select Case B9

                Case 1
                    Locate 3, 1: Input "1. OPP FGA "; compareZ1!(1)

                Case 2
                    Locate 4, 1: Input "2. OPP FGM "; compareZ1!(2)

                Case 3
                    Locate 5, 1: Input "3. OPP 3FGA "; compareZ1!(14)

                Case 4
                    Locate 6, 1: Input "4. OPP 3FGM "; compareZ1!(13)

                Case 5
                    Locate 7, 1: Input "5. OPP FTA "; compareZ1!(3)

                Case 6
                    Locate 8, 1: Input "6. OPP FTM "; compareZ1!(4)

                Case 7
                    Locate 9, 1: Input "7. OPP OFF RB  "; compareZ1!(12)

                Case 8
                    Locate 10, 1: Input "8. OPP TOT RB "; compareZ1!(5)

                Case 9
                    Locate 11, 1: Input "9. OPP FOULS "; compareZ1!(6)

                Case 10
                    Locate 12, 1: Input "10. OPP STEALS "; compareZ1!(7)

                Case 11
                    Locate 13, 1: Input "11. OPP ASSISTS "; compareZ1!(8)

                Case 12
                    Locate 14, 1: Input "12. OPP BLOCKS "; compareZ1!(9)

                Case 13
                    Locate 15, 1: Input "13. OPP TURNOVERS "; compareZ1!(11)

                Case 14
                    Locate 16, 1: Input "14. OPP POINTS  "; compareZ1!(10)
            
            End Select

        Loop Until B9 = 0

        
        '---------------
        ' Player Stats
        '---------------
        Do

            Cls
            X$ = "CHANGE PLAYERS"
            Call TITLE_STAT_HEADER(X$)

            For I = 0 To 13
                Color 7
                If I / 2 = Int(I / 2) Then Color 15
                Print (I + 1);
                Print " "; inputA$(I)
            Next I
 
            Print

            Do
                Input "# TO CHANGE (1-13) (0 TO CONTINUE) "; I
            Loop Until I >= 0 And I <= 13

            If I > 0 Then

                I = I - 1

                Cls

                Locate 2, 1: PRINT "PLAYER - " + Str$(I + 1)
                Locate 3, 1
                Print "1. NAME "; inputA$(I)
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
                Print "15. 3FGA "; compareW1!(I)
                Print "16. 3FGM "; compareW0!(I)
                Print "17. HI-GAME (PTS) "; Z2(I, 13)

                Do

                    Do
                        Locate 22, 1
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 17

                    Select Case B9

                        Case 1
                            Locate 3, 1: Input "1. NAME "; inputA$(I)

                        Case 2
                            Locate 4, 1: Input "2. GAMES "; Z2(I, 0)

                        Case 3
                            Locate 5, 1: Input "3. MINUTES "; Z2(I, 10)
                            Z2(I, 10) = Z2(I, 10) * 60

                        Case 4
                            Locate 6, 1: Input "4. FGA "; Z2(I, 1)

                        Case 5
                            Locate 7, 1: Input "5. FGM "; Z2(I, 2)

                        Case 6
                            Locate 8, 1: Input "6. FTA "; Z2(I, 3)

                        Case 7
                            Locate 9, 1: Input "7. FTM "; Z2(I, 4)

                        Case 8
                            Locate 10, 1: Input "8. OFF REB "; Z2(I, 12)

                        Case 9
                            Locate 11, 1: Input "9. TOT REB  "; Z2(I, 5)

                        Case 10
                            Locate 12, 1: Input "10. FOULS "; Z2(I, 6)

                        Case 11
                            Locate 13, 1: Input "11. STEALS "; Z2(I, 7)

                        Case 12
                            Locate 14, 1: Input "12. ASSISTS "; Z2(I, 8)

                        Case 13
                            Locate 15, 1: Input "13. BLOCKS "; Z2(I, 9)

                        Case 14
                            Locate 16, 1: Input "14. TURNOVERS "; Z2(I, 11)

                        Case 15
                            Locate 17, 1: Input "15. 3FGA "; compareW1!(I)

                        Case 16
                            Locate 18, 1: Input "16. 3FGM "; compareW0!(I)

                        Case 17
                            Locate 19, 1: Input "17. HI-GAME (PTS) "; Z2(I, 13)

                    End Select

                Loop Until B9 = 0

            End If

        Loop Until I = 0


        '---------------
        ' Save Edits
        '---------------        
        Open diskPaths$(1) + statFile$ For Output As #1

        Print #1, A$

        For I = 0 To 14: 
            Print #1, PrintA$(I)

            For J = 0 To 13: 
                Print #1, Z2(I, J)
            Next J

            Print #1, compareW0!(I)

            Print #1, compareW1!(I)

        Next I

        For L = 0 To 15: 
            Print #1, compareZ!(L)
        Next L

        For L = 0 To 15: 
            Print #1, compareZ1!(L)
        Next L

        Close 1

    End If

End Sub


'----------------------------------------
'        GetTeam_EditStats Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_EditStats (T1$, YN$, D)

    Shared teamRatings%()

    Do

        Color 15, 0
        Cls

        JJ = 7

        Locate 6, 8: Color 15, 4

        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32

        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 27: Input NY$

        If NY$ = "" And YN$ <> "" Then
            'YN$ = NY$
            Call SelectTeam_EditStats(YN$, NTMS, D)
        Else
            YN$ = RTrim$(NY$)
            Erase teamNames$
            NTMS = 0

            Open diskPaths$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031
            fileLength& = LOF(1)
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

            For I1 = 1 To fileLength& / 1031
                Get #1, I1
                CK% = CVI(Q$(9))
                If CK% = 999 Then
                    NTMS = NTMS + 1
                    teamNames$(NTMS) = Q$(0)
                    teamIndex%(NTMS) = I1
                End If
            Next

            Close 1

            Call SelectTeam_EditStats(YN$, NTMS, D)

        End If

        T1$ = RTrim$(Q$(0))

        For I = 0 To 34:
            teamRatings%(I) = CVI(Q$(I + 10))
        Next

        Close 1

        Locate 2, 33: Color 31: Print "LOADING..."

        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33
            Color teamRatings%(33), teamRatings%(34): Print T1$

            Do
                Color 15, 4
                Locate 3, 33: Print "ANY CHANGE (YN)";
                A$ = GetKeyPress$
            Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

        End If

    Loop Until UCase$(A$) = "N"

End Sub


'----------------------------------------
'      SelectTeam_EditStats Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_EditStats (yearNumber$, numberTeams, selectedIndex)

    Selection% = 1
    Count% = numberTeams
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
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        selectedIndex = teamIndex%(Selection%)

        teamYear$ = RTrim$(yearNumber$)
        Open diskPaths$(0) + "PROBBTMS." + teamYear$ For Random As #1 Len = 1031

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

        Get #1, selectedIndex
    Else
        BO% = 1
    End If

End Sub
