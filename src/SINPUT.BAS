'----------------------------------------
'          EditStats Subroutine
'----------------------------------------
'This subroutine loads in stats for a TEAM
'and gives the user an interface for making
'edits to the stats.
Sub EditStats ()

    _Title "Basketball: The Pro Game - Stats Input"

    Shared statsZ0!(), statsZ1!()
    Shared stats3FGM!(), stats3FGA!()

    Dim statPlyrNames$(14)
    Dim plyrRat_INPUT!(14, 13)

    teamNbr = 0
    Call ChooseTeam(YN$, NTMS, teamNbr, team$)
    statFile$ = nameStatFile$(team$)

    If Not _FileExists(diskPaths$(1) + statFile$) Then

        MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
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
                Input #1, plyrRat_INPUT!(I, J)
            Next J

            Input #1, stats3FGM!(I)
            Input #1, stats3FGA!(I)

        Next I

        For L = 0 To 15:
            Input #1, statsZ0!(L)
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
        Print Using "#####"; Int(statsZ0!(0) / 60)
        Print "3. TM FGA "; statsZ0!(1)
        Print "4. TM FGM "; statsZ0!(2)
        Print "5. TM 3FGA "; statsZ0!(14)
        Print "6. TM 3FGM "; statsZ0!(13)
        Print "7. TM FTA "; statsZ0!(3)
        Print "8. TM FTM "; statsZ0!(4)
        Print "9. TM OFF RB "; statsZ0!(12)
        Print "10. TM TOT RB "; statsZ0!(5)
        Print "11. TM FOULS "; statsZ0!(6)
        Print "12. TM STEALS "; statsZ0!(7)
        Print "13. TM ASSISTS "; statsZ0!(8)
        Print "14. TM BLOCKS "; statsZ0!(9)
        Print "15. TM TURNOVERS "; statsZ0!(11)
        Print "16. TM POINTS "; statsZ0!(10)

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
                    Locate 4, 1: Input "2. TM MINUTES "; statsZ0!(0)
                    Locate 4, 15: Print " "
                    statsZ0!(0) = statsZ0!(0) * 60

                Case 3:
                    Locate 5, 12: Print Space$(10)
                    Locate 5, 1: Input "3. TM FGA "; statsZ0!(1)
                    Locate 5, 12: Print " "

                Case 4:
                    Locate 6, 12: Print Space$(10)
                    Locate 6, 1: Input "4. TM FGM "; statsZ0!(2)
                    Locate 6, 12: Print " "

                Case 5:
                    Locate 7, 13: Print Space$(10)
                    Locate 7, 1: Input "5. TM 3FGA "; statsZ0!(14)
                    Locate 7, 13: Print " "

                Case 6:
                    Locate 8, 13: Print Space$(10)
                    Locate 8, 1: Input "6. TM 3FGM "; statsZ0!(13)
                    Locate 8, 13: Print " "

                Case 7:
                    Locate 9, 12: Print Space$(10)
                    Locate 9, 1: Input "7. TM FTA "; statsZ0!(3)
                    Locate 9, 12: Print " "

                Case 8:
                    Locate 10, 12: Print Space$(10)
                    Locate 10, 1: Input "8. TM FTM "; statsZ0!(4)
                    Locate 10, 12: Print " "

                Case 9:
                    Locate 11, 15: Print Space$(10)
                    Locate 11, 1: Input "9. TM OFF RB "; statsZ0!(12)
                    Locate 11, 15: Print " "

                Case 10:
                    Locate 12, 15: Print Space$(10)
                    Locate 12, 1: Input "10. TM TOT RB "; statsZ0!(5)
                    Locate 12, 15: Print " "

                Case 11:
                    Locate 13, 15: Print Space$(10)
                    Locate 13, 1: Input "11. TM FOULS "; statsZ0!(6)
                    Locate 13, 15: Print " "

                Case 12:
                    Locate 14, 16: Print Space$(10)
                    Locate 14, 1: Input "12. TM STEALS "; statsZ0!(7)
                    Locate 14, 16: Print " "

                Case 13:
                    Locate 15, 17: Print Space$(10)
                    Locate 15, 1: Input "13. TM ASSISTS "; statsZ0!(8)
                    Locate 15, 17: Print " "

                Case 14:
                    Locate 16, 16: Print Space$(10)
                    Locate 16, 1: Input "14. TM BLOCKS "; statsZ0!(9)
                    Locate 16, 16: Print " "

                Case 15:
                    Locate 17, 19: Print Space$(10)
                    Locate 17, 1: Input "15. TM TURNOVERS "; statsZ0!(11)
                    Locate 17, 19: Print " "

                Case 16:
                    Locate 18, 16: Print Space$(10)
                    Locate 18, 1: Input "16. TM POINTS "; statsZ0!(10)
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
                Print "2. GAMES "; plyrRat_INPUT!(I, 0)
                Print "3. MINUTES "; Int(plyrRat_INPUT!(I, 10) / 60 + .5)
                Print "4. FGA "; plyrRat_INPUT!(I, 1)
                Print "5. FGM "; plyrRat_INPUT!(I, 2); ""
                Print "6. FTA "; plyrRat_INPUT!(I, 3)
                Print "7. FTM "; plyrRat_INPUT!(I, 4)
                Print "8. OFF REB "; plyrRat_INPUT!(I, 12)
                Print "9. TOT REB "; plyrRat_INPUT!(I, 5)
                Print "10. FOULS "; plyrRat_INPUT!(I, 6)
                Print "11. STEALS "; plyrRat_INPUT!(I, 7)
                Print "12. ASSISTS "; plyrRat_INPUT!(I, 8)
                Print "13. BLOCKS "; plyrRat_INPUT!(I, 9)
                Print "14. TURNOVERS "; plyrRat_INPUT!(I, 11)
                Print "15. 3FGA "; stats3FGA!(I)
                Print "16. 3FGM "; stats3FGM!(I)
                Print "17. HI-GAME (PTS) "; plyrRat_INPUT!(I, 13)

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
                            Locate 4, 1: Input "2. GAMES "; plyrRat_INPUT!(I, 0)
                            Locate 4, 10: Print " "

                        Case 3
                            Locate 5, 12: Print Space$(10)
                            Locate 5, 1: Input "3. MINUTES "; plyrRat_INPUT!(I, 10)
                            Locate 5, 12: Print " "
                            plyrRat_INPUT!(I, 10) = plyrRat_INPUT!(I, 10) * 60

                        Case 4
                            Locate 6, 8: Print Space$(10)
                            Locate 6, 1: Input "4. FGA "; plyrRat_INPUT!(I, 1)
                            Locate 6, 8: Print " "

                        Case 5
                            Locate 7, 8: Print Space$(10)
                            Locate 7, 1: Input "5. FGM "; plyrRat_INPUT!(I, 2)
                            Locate 7, 8: Print " "

                        Case 6
                            Locate 8, 8: Print Space$(10)
                            Locate 8, 1: Input "6. FTA "; plyrRat_INPUT!(I, 3)
                            Locate 8, 8: Print " "

                        Case 7
                            Locate 9, 8: Print Space$(10)
                            Locate 9, 1: Input "7. FTM "; plyrRat_INPUT!(I, 4)
                            Locate 9, 8: Print " "

                        Case 8
                            Locate 10, 12: Print Space$(10)
                            Locate 10, 1: Input "8. OFF REB "; plyrRat_INPUT!(I, 12)
                            Locate 10, 12: Print " "

                        Case 9
                            Locate 11, 12: Print Space$(10)
                            Locate 11, 1: Input "9. TOT REB "; plyrRat_INPUT!(I, 5)
                            Locate 11, 12: Print " "
                        Case 10

                        Case 10
                            Locate 12, 11: Print Space$(10)
                            Locate 12, 1: Input "10. FOULS "; plyrRat_INPUT!(I, 6)
                            Locate 12, 11: Print " "

                        Case 11
                            Locate 13, 12: Print Space$(10)
                            Locate 13, 1: Input "11. STEALS "; plyrRat_INPUT!(I, 7)
                            Locate 13, 12: Print " "

                        Case 12
                            Locate 14, 13: Print Space$(10)
                            Locate 14, 1: Input "12. ASSISTS "; plyrRat_INPUT!(I, 8)
                            Locate 14, 13: Print " "

                        Case 13
                            Locate 15, 12: Print Space$(10)
                            Locate 15, 1: Input "13. BLOCKS "; plyrRat_INPUT!(I, 9)
                            Locate 15, 12: Print " "

                        Case 14
                            Locate 16, 15: Print Space$(10)
                            Locate 16, 1: Input "14. TURNOVERS "; plyrRat_INPUT!(I, 11)
                            Locate 16, 15: Print " "

                        Case 15
                            Locate 17, 10: Print Space$(10)
                            Locate 17, 1: Input "15. 3FGA "; stats3FGA!(I)
                            Locate 17, 10: Print " "

                        Case 16
                            Locate 18, 10: Print Space$(10)
                            Locate 18, 1: Input "16. 3FGM "; stats3FGM!(I)
                            Locate 18, 10: Print " "

                        Case 17
                            Locate 19, 19: Print Space$(10)
                            Locate 19, 1: Input "17. HI-GAME (PTS) "; plyrRat_INPUT!(I, 13)
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
                Print #2, plyrRat_INPUT!(I, J)
            Next J

            Print #2, stats3FGM!(I)

            Print #2, stats3FGA!(I)

        Next I

        For L = 0 To 15:
            Print #2, statsZ0!(L)
        Next L

        For L = 0 To 15:
            Print #2, statsZ1!(L)
        Next L

        Close #2

    End If

End Sub

