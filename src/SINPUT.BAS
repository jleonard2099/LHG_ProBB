'----------------------------------------
'          EditStats Subroutine
'----------------------------------------
'This subroutine loads in stats for a TEAM
'and gives the user an interface for making
'edits to the stats.
Sub EditStats ()

    _Title "Basketball: The Pro Game - Stats Input"

    Shared statsZ!(), statsZ1!()
    Shared statsW0!(), statsW1!()

    Dim statPlyrNames$(14)
    Dim inputZ2!(0 To 14, 0 To 13)

    teamNbr = 0
    Call ChooseTeam(YN$, NTMS, teamNbr, team$)
    statFile$ = nameStatFile$(team$)

    If Not _FileExists(diskPaths$(1) + statFile$) Then

        MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Open diskPaths$(1) + statFile$ For Input As #1

        Input #1, teamName$

        For I = 0 To 14:

            Input #1, statPlyrNames$(I)

            For J = 0 To 13:
                Input #1, inputZ2!(I, J)
            Next J

            Input #1, statsW0!(I)
            Input #1, statsW1!(I)

        Next I

        For L = 0 To 15:
            Input #1, statsZ!(L)
        Next L

        For L = 0 To 15:
            Input #1, statsZ1!(L)
        Next L

        Close #1

        '---------------
        ' Team Stats
        '---------------
        Color 15, 0
        Cls

        X$ = "OFFENSIVE TEAM STATS"
        Call TITLE_STAT_HEADER(X$)

        Locate 3, 1
        Print "1. STAT FILE NAME  "; statFile$
        Print "2. TM MINUTES ";
        Print Using "#####"; Int(statsZ!(0) / 60)
        Print "3. TM FGA "; statsZ!(1)
        Print "4. TM FGM "; statsZ!(2)
        Print "5. TM 3FGA "; statsZ!(14)
        Print "6. TM 3FGM "; statsZ!(13)
        Print "7. TM FTA "; statsZ!(3)
        Print "8. TM FTM "; statsZ!(4)
        Print "9. TM OFF RB "; statsZ!(12)
        Print "10. TM TOT RB "; statsZ!(5)
        Print "11. TM FOULS "; statsZ!(6)
        Print "12. TM STEALS "; statsZ!(7)
        Print "13. TM ASSISTS "; statsZ!(8)
        Print "14. TM BLOCKS "; statsZ!(9)
        Print "15. TM TURNOVERS "; statsZ!(11)
        Print "16. TM POINTS "; statsZ!(10)

        Do

            Do
                Locate 22, 1
                Input "# TO CHANGE (0 TO CONTINUE)"; B9
            Loop Until B9 >= 0 And B9 <= 16

            Select Case B9

                Case 1:
                    Locate 3, 1: Input "1. STAT FILE NAME "; statFile$
                    Locate 3, 19: Print " "

                Case 2:
                    Locate 4, 15: Print Space$(10)
                    Locate 4, 1: Input "2. TM MINUTES "; statsZ!(0)
                    Locate 4, 15: Print " "
                    statsZ!(0) = statsZ!(0) * 60

                Case 3:
                    Locate 5, 12: Print Space$(10)
                    Locate 5, 1: Input "3. TM FGA "; statsZ!(1)
                    Locate 5, 12: Print " "

                Case 4:
                    Locate 6, 12: Print Space$(10)
                    Locate 6, 1: Input "4. TM FGM "; statsZ!(2)
                    Locate 6, 12: Print " "

                Case 5:
                    Locate 7, 13: Print Space$(10)
                    Locate 7, 1: Input "5. TM 3FGA "; statsZ!(14)
                    Locate 7, 13: Print " "

                Case 6:
                    Locate 8, 13: Print Space$(10)
                    Locate 8, 1: Input "6. TM 3FGM "; statsZ!(13)
                    Locate 8, 13: Print " "

                Case 7:
                    Locate 9, 12: Print Space$(10)
                    Locate 9, 1: Input "7. TM FTA "; statsZ!(3)
                    Locate 9, 12: Print " "

                Case 8:
                    Locate 10, 12: Print Space$(10)
                    Locate 10, 1: Input "8. TM FTM "; statsZ!(4)
                    Locate 10, 12: Print " "

                Case 9:
                    Locate 11, 15: Print Space$(10)
                    Locate 11, 1: Input "9. TM OFF RB "; statsZ!(12)
                    Locate 11, 15: Print " "

                Case 10:
                    Locate 12, 15: Print Space$(10)
                    Locate 12, 1: Input "10. TM TOT RB "; statsZ!(5)
                    Locate 12, 15: Print " "

                Case 11:
                    Locate 13, 15: Print Space$(10)
                    Locate 13, 1: Input "11. TM FOULS "; statsZ!(6)
                    Locate 13, 15: Print " "

                Case 12:
                    Locate 14, 16: Print Space$(10)
                    Locate 14, 1: Input "12. TM STEALS "; statsZ!(7)
                    Locate 14, 16: Print " "

                Case 13:
                    Locate 15, 17: Print Space$(10)
                    Locate 15, 1: Input "13. TM ASSISTS "; statsZ!(8)
                    Locate 15, 17: Print " "

                Case 14:
                    Locate 16, 16: Print Space$(10)
                    Locate 16, 1: Input "14. TM BLOCKS "; statsZ!(9)
                    Locate 16, 16: Print " "

                Case 15:
                    Locate 17, 19: Print Space$(10)
                    Locate 17, 1: Input "15. TM TURNOVERS "; statsZ!(11)
                    Locate 17, 19: Print " "

                Case 16:
                    Locate 18, 16: Print Space$(10)
                    Locate 18, 1: Input "16. TM POINTS "; statsZ!(10)
                    Locate 18, 16: Print " "

            End Select

            Locate 22, 29
            Print Space$(10)

        Loop Until B9 = 0

        '---------------
        ' Defense Stats
        '---------------
        Cls
        X$ = "DEFENSIVE TEAM STATS"
        Call TITLE_STAT_HEADER(X$)

        Locate 3, 1
        Print "1. OPP FGA "; statsZ1!(1)
        Print "2. OPP FGM "; statsZ1!(2)
        Print "3. OPP 3FGA "; statsZ1!(14)
        Print "4. OPP 3FGM "; statsZ1!(13)
        Print "5. OPP FTA "; statsZ1!(3)
        Print "6. OPP FTM "; statsZ1!(4)
        Print "7. OPP OFF RB "; statsZ1!(12)
        Print "8. OPP TOT RB "; statsZ1!(5)
        Print "9. OPP FOULS "; statsZ1!(6)
        Print "10. OPP STEALS "; statsZ1!(7)
        Print "11. OPP ASSISTS "; statsZ1!(8)
        Print "12. OPP BLOCKS "; statsZ1!(9)
        Print "13. OPP TURNOVERS "; statsZ1!(11)
        Print "14. OPP POINTS "; statsZ1!(10)

        Do

            Do
                Locate 22, 1
                Input "# TO CHANGE (0 TO CONTINUE)"; B9
            Loop Until B9 >= 0 And B9 <= 14

            Select Case B9

                Case 1
                    Locate 3, 12: Print Space$(10)
                    Locate 3, 1: Input "1. OPP FGA "; statsZ1!(1)
                    Locate 3, 12: Print " "

                Case 2
                    Locate 4, 12: Print Space$(10)
                    Locate 4, 1: Input "2. OPP FGM "; statsZ1!(2)
                    Locate 4, 12: Print " "

                Case 3
                    Locate 5, 13: Print Space$(10)
                    Locate 5, 1: Input "3. OPP 3FGA "; statsZ1!(14)
                    Locate 5, 13: Print " "

                Case 4
                    Locate 6, 13: Print Space$(10)
                    Locate 6, 1: Input "4. OPP 3FGM "; statsZ1!(13)
                    Locate 6, 13: Print " "

                Case 5
                    Locate 7, 12: Print Space$(10)
                    Locate 7, 1: Input "5. OPP FTA "; statsZ1!(3)
                    Locate 7, 12: Print " "

                Case 6
                    Locate 8, 12: Print Space$(10)
                    Locate 8, 1: Input "6. OPP FTM "; statsZ1!(4)
                    Locate 8, 12: Print " "

                Case 7
                    Locate 9, 15: Print Space$(10)
                    Locate 9, 1: Input "7. OPP OFF RB "; statsZ1!(12)
                    Locate 9, 15: Print " "

                Case 8
                    Locate 10, 15: Print Space$(10)
                    Locate 10, 1: Input "8. OPP TOT RB "; statsZ1!(5)
                    Locate 10, 15: Print " "

                Case 9
                    Locate 11, 14: Print Space$(10)
                    Locate 11, 1: Input "9. OPP FOULS "; statsZ1!(6)
                    Locate 11, 14: Print " "

                Case 10
                    Locate 12, 16: Print Space$(10)
                    Locate 12, 1: Input "10. OPP STEALS "; statsZ1!(7)
                    Locate 12, 16: Print " "

                Case 11
                    Locate 13, 17: Print Space$(10)
                    Locate 13, 1: Input "11. OPP ASSISTS "; statsZ1!(8)
                    Locate 13, 17: Print " "

                Case 12
                    Locate 14, 16: Print Space$(10)
                    Locate 14, 1: Input "12. OPP BLOCKS "; statsZ1!(9)
                    Locate 14, 16: Print " "

                Case 13
                    Locate 15, 19: Print Space$(10)
                    Locate 15, 1: Input "13. OPP TURNOVERS "; statsZ1!(11)
                    Locate 15, 19: Print " "

                Case 14
                    Locate 16, 16: Print Space$(10)
                    Locate 16, 1: Input "14. OPP POINTS "; statsZ1!(10)
                    Locate 16, 16: Print " "

            End Select

            Locate 22, 29
            Print Space$(10)

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
                If (I Mod 2) = 0 Then Color 15
                Print (I + 1);
                Print " "; statPlyrNames$(I)
            Next I

            Print

            Do
                Input "# TO CHANGE (1-13) (0 TO CONTINUE)"; I
            Loop Until I >= 0 And I <= 13

            If I > 0 Then

                I = I - 1

                Cls
                Call TITLE_STAT_HEADER(X$)

                Locate 2, 1: Print "PLAYER - " + Str$(I + 1)
                Locate 3, 1
                Print "1. NAME  "; statPlyrNames$(I)
                Print "2. GAMES "; inputZ2!(I, 0)
                Print "3. MINUTES "; Int(inputZ2!(I, 10) / 60 + .5)
                Print "4. FGA "; inputZ2!(I, 1)
                Print "5. FGM "; inputZ2!(I, 2); ""
                Print "6. FTA "; inputZ2!(I, 3)
                Print "7. FTM "; inputZ2!(I, 4)
                Print "8. OFF REB "; inputZ2!(I, 12)
                Print "9. TOT REB "; inputZ2!(I, 5)
                Print "10. FOULS "; inputZ2!(I, 6)
                Print "11. STEALS "; inputZ2!(I, 7)
                Print "12. ASSISTS "; inputZ2!(I, 8)
                Print "13. BLOCKS "; inputZ2!(I, 9)
                Print "14. TURNOVERS "; inputZ2!(I, 11)
                Print "15. 3FGA "; statsW1!(I)
                Print "16. 3FGM "; statsW0!(I)
                Print "17. HI-GAME (PTS) "; inputZ2!(I, 13)

                Do

                    Do
                        Locate 22, 1
                        Input "# TO CHANGE (0 TO CONTINUE)"; B9
                    Loop Until B9 >= 0 And B9 <= 17

                    Select Case B9

                        Case 1
                            Locate 3, 1: Input "1. NAME "; statPlyrNames$(I)
                            Locate 3, 9: Print " "

                        Case 2
                            Locate 4, 10: Print Space$(10)
                            Locate 4, 1: Input "2. GAMES "; inputZ2!(I, 0)
                            Locate 4, 10: Print " "

                        Case 3
                            Locate 5, 12: Print Space$(10)
                            Locate 5, 1: Input "3. MINUTES "; inputZ2!(I, 10)
                            Locate 5, 12: Print " "
                            inputZ2!(I, 10) = inputZ2!(I, 10) * 60

                        Case 4
                            Locate 6, 8: Print Space$(10)
                            Locate 6, 1: Input "4. FGA "; inputZ2!(I, 1)
                            Locate 6, 8: Print " "

                        Case 5
                            Locate 7, 8: Print Space$(10)
                            Locate 7, 1: Input "5. FGM "; inputZ2!(I, 2)
                            Locate 7, 8: Print " "

                        Case 6
                            Locate 8, 8: Print Space$(10)
                            Locate 8, 1: Input "6. FTA "; inputZ2!(I, 3)
                            Locate 8, 8: Print " "

                        Case 7
                            Locate 9, 8: Print Space$(10)
                            Locate 9, 1: Input "7. FTM "; inputZ2!(I, 4)
                            Locate 9, 8: Print " "

                        Case 8
                            Locate 10, 12: Print Space$(10)
                            Locate 10, 1: Input "8. OFF REB "; inputZ2!(I, 12)
                            Locate 10, 12: Print " "

                        Case 9
                            Locate 11, 12: Print Space$(10)
                            Locate 11, 1: Input "9. TOT REB "; inputZ2!(I, 5)
                            Locate 11, 12: Print " "
                        Case 10

                        Case 10
                            Locate 12, 11: Print Space$(10)
                            Locate 12, 1: Input "10. FOULS "; inputZ2!(I, 6)
                            Locate 12, 11: Print " "

                        Case 11
                            Locate 13, 12: Print Space$(10)
                            Locate 13, 1: Input "11. STEALS "; inputZ2!(I, 7)
                            Locate 13, 12: Print " "

                        Case 12
                            Locate 14, 13: Print Space$(10)
                            Locate 14, 1: Input "12. ASSISTS "; inputZ2!(I, 8)
                            Locate 14, 13: Print " "

                        Case 13
                            Locate 15, 12: Print Space$(10)
                            Locate 15, 1: Input "13. BLOCKS "; inputZ2!(I, 9)
                            Locate 15, 12: Print " "

                        Case 14
                            Locate 16, 15: Print Space$(10)
                            Locate 16, 1: Input "14. TURNOVERS "; inputZ2!(I, 11)
                            Locate 16, 15: Print " "

                        Case 15
                            Locate 17, 10: Print Space$(10)
                            Locate 17, 1: Input "15. 3FGA "; statsW1!(I)
                            Locate 17, 10: Print " "

                        Case 16
                            Locate 18, 10: Print Space$(10)
                            Locate 18, 1: Input "16. 3FGM "; statsW0!(I)
                            Locate 18, 10: Print " "

                        Case 17
                            Locate 19, 19: Print Space$(10)
                            Locate 19, 1: Input "17. HI-GAME (PTS) "; inputZ2!(I, 13)
                            Locate 19, 19: Print " "

                    End Select

                    Locate 22, 29
                    Print Space$(10)

                Loop Until B9 = 0

            End If

        Loop Until I = 0


        '---------------
        ' Save Edits
        '---------------
        Open diskPaths$(1) + statFile$ For Output As #2

        Print #2, teamName$

        For I = 0 To 14:
            Print #2, statPlyrNames$(I)

            For J = 0 To 13:
                Print #2, inputZ2!(I, J)
            Next J

            Print #2, statsW0!(I)

            Print #2, statsW1!(I)

        Next I

        For L = 0 To 15:
            Print #2, statsZ!(L)
        Next L

        For L = 0 To 15:
            Print #2, statsZ1!(L)
        Next L

        Close #2

    End If

End Sub

