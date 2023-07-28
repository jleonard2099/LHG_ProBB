Sub GPMIN ()

    ReDim menuChoices$(1 To 2)

    _Title "Basketball: The Pro Game - Adjust Game Time"

    Shared playerNames$(), teamInfo$()
    Shared leagueRatings%(), playerRatings%(), teamRatings%()

    ' Postions (Guard / Forward / Center)
    Dim P$(0 To 2)

    P$(0) = "G"
    P$(1) = "F"
    P$(2) = "C"

    Call ChooseTeam(YN$, NTMS, teamNbr, team$)

    If teamNbr <> 0 Then

        Color 15, 0

        Do

            Color , 0
            Cls
            Color teamRatings%(33), teamRatings%(34): Print teamInfo$(0)
            Color 15, 0
            Color 12
            Print "   PLAYERS                 ACTUAL           ADJUSTED"
            Color 14
            Print "                          GP   MPG         GP    MPG"
            Color 15

            gamesPlayed = 0
            MIN = 0

            For I = 0 To 14

                If playerRatings%(I, 16) >= 0 Or playerRatings%(I, 17) >= 0 Then
                    playerRatings%(I, 16) = playerRatings%(I, 0) * -1
                    playerRatings%(I, 17) = playerRatings%(I, 1) * -1
                End If

                gamesPlayed = gamesPlayed + playerRatings%(I, 0)
                MIN = MIN + playerRatings%(I, 17) * -1

                If Left$(playerNames$(I, 0), 3) <> "XXX" Then
                    Color 14: Print Using "## "; I;
                    Color 15: Print playerNames$(I, 0); " "; playerNames$(I, 1); Tab(27);

                    actualMPG! = playerRatings%(I, 1) / playerRatings%(I, 0)
                    adjustedMPG! = playerRatings%(I, 17) / playerRatings%(I, 16)
                    adjustedGP! = playerRatings%(I, 16) * -1

                    Print Using "##"; playerRatings%(I, 0);
                    Print "  ";
                    Print Using "##.#"; actualMPG!;
                    Print "         ";
                    Print Using "##"; adjustedGP!;
                    Print "   ";
                    Print Using "##.#"; adjustedMPG!
                End If
            Next

            Color 14: Print "15 ";
            Color 15: Print "END AND SAVE CHANGES"
            Print
            Print "TEAM MIN/G SHOULD BE APPROXIMATELY 240, OVERTIME WILL INCREASE THIS FIGURE"
            teamTotalMins! = MIN / teamRatings%(0)
            Print Using "TEAM MIN/G = ###.#"; teamTotalMins!

            Do
                Locate 23, 1
                Input "ENTER SELECTION "; B1

                If B1 < 15 Then
                    Locate 23, 1: Print Space$(38);
                    Locate 23, 1: Print playerNames$(B1, 0); " ";
                    Input "GAMES PLAYED "; gamesPlayed

                    Locate 23, 1: Print Space$(38);
                    Locate 23, 1: Print playerNames$(B1, 0); " ";
                    Input "MINUTES PER GAME "; minutesPerGame

                    playerRatings%(B1, 16) = gamesPlayed * -1
                    playerRatings%(B1, 17) = minutesPerGame * gamesPlayed * -1

                Else

                    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

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

                    LSet Q$(0) = teamInfo$(0)
                    LSet Q$(1) = teamInfo$(1)

                    For I = 0 To 6:
                        LSet Q$(I + 2) = MKI$(leagueRatings%(I))
                    Next

                    LSet Q$(9) = MKI$(999)

                    For I = 0 To 34:
                        LSet Q$(I + 10) = MKI$(teamRatings%(I))
                    Next

                    For I = 0 To 14
                        LSet Q$(22 * I + 45) = playerNames$(I, 0)
                        LSet Q$(22 * I + 46) = playerNames$(I, 1)
                        For I1 = 0 To 19
                            LSet Q$(22 * I + 47 + I1) = MKI$(playerRatings%(I, I1))
                        Next
                    Next

                    LSet Q$(375) = teamInfo$(2)
                    LSet Q$(376) = teamInfo$(3)
                    LSet Q$(377) = teamInfo$(4)

                    Put #1, teamNbr

                    Close 1

                End If

            Loop Until B1 >= 0 And B1 <= 15

        Loop Until B1 = 15

    End If 'Done checking for ESC

End Sub
