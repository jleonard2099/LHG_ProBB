'----------------------------------------
'            MERGE Subroutine
'----------------------------------------
'This subroutine gives the user options
'for aligning / merging stat files
Sub MERGE ()

    _Title "Basketball: The Pro Game - Merge Stats"

    ReDim menuChoices$(1 To 3)

    Do

        Color , 0
        Cls

        Call TITLE_MERGE

        menuChoices$(1) = "MERGE STATISTICAL FILES"
        menuChoices$(2) = "ALIGN STATISTICAL FILES"
        menuChoices$(3) = "EXIT TO MAIN MENU      "
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1: 
                    Call MergeStats

                Case 2: 
                    Call ALIGN

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'          MergeStats Subroutine
'----------------------------------------
'This subroutine determines the parameters
'for the road and existing stat files that
'can be merged. It then reads in all stat
'files, checks for player sequence, and then
'calls a supporting routine to actually
'perform the merge
Sub MergeStats ()

    Dim statPlyrName_Road$(0 To 14)

    Shared nbrGamesCurr, nbrGamesRoad

    Shared gameAttendance&(), gameAtt_Road&()

    Shared teamScore(), teamScore_Road()
    Shared oppScore(), oppScore_Road()

    Shared ARD!(), playerRatings!()
    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()
    Shared W0RD!(), W1RD!()
    Shared ZRD!(), ZRD1!()

    'Shared gameSite$(), gameSite_Road$()
    Shared locIndicator$(), locIndicator_Road$()
    Shared oppName$(), oppName_Road$()
    Shared statPlyrNames$()

    Do

        Call GetMergePaths(pathCurrentStats$, pathRoadStats$, I$)

        If UCase$(I$) = "N" And I$ <> Chr$(27) Then

            teamNbr = 0
            Call ChooseTeam (YN$, NTMS, teamNbr, team$)
            statFile$ = nameStatFile$(team$)

            Color 15, 0
            Cls

            Locate 10, 15: Print "ROAD STATS ARE ON DRIVE "; pathRoadStats$
            Locate , 15: Print "MASTER STATS ARE ON DRIVE "; pathCurrentStats$
            Locate , 15: Print "TEAM TO MERGE STATS FOR IS "; team$

            Do
                Locate 14, 15: Print "ANY CHANGE? (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            If UCase$(I$) = "N" Then

                If _FileExists(pathRoadStats$ + statFile$) Then

                    Open pathRoadStats$ + statFile$ For Input As #1

                    Input #1, B$

                    For I = 0 To 14

                        Input #1, statPlyrName_Road$(I)

                        For J = 0 To 13
                            Input #1, ARD!(I, J)
                        Next

                        Input #1, W0RD!(I)
                        Input #1, W1RD!(I)

                    Next

                    For L = 0 To 15: 
                        Input #1, ZRD!(L)
                    Next
                    
                    For L = 0 To 15: 
                        Input #1, ZRD1!(L)
                    Next

                    Close #1

                    Open pathRoadStats$ + statFile$ + "S" For Input As #2

                    Input #2, nbrGamesRoad

                    For I = 1 to MAX_SCHED_STATS: 
                        Input #2, teamScore_Road(I)
                    Next I
                    
                    For I = 1 to MAX_SCHED_STATS: 
                        Input #2, oppName_Road$(I)
                    Next I
                    
                    For I = 1 to MAX_SCHED_STATS: 
                        Input #2, oppScore_Road(I)
                    Next I
                    
                    For I = 1 to MAX_SCHED_STATS: 
                        Input #2, locIndicator_Road$(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #2, gameAtt_Road&(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #2, gameSite_Road$(I)
                    Next I

                    Close #2

                    For J = 21 To 23
                        Locate J
                        Print Space$(64)
                    Next

                    If _FileExists(pathCurrentStats$ + statFile$) Then

                        Open pathCurrentStats$ + statFile$ For Input As #3

                        Input #3, B$

                        For I = 0 To 14:

                            Input #3, statPlyrNames$(I)

                            For J = 0 To 13: 
                                Input #3, playerRatings!(I, J)
                            Next

                            Input #3, statsW0!(I)
                            Input #3, statsW1!(I)

                        Next

                        For L = 0 To 15: 
                            Input #3, statsZ!(L)
                        Next

                        For L = 0 To 15: 
                            Input #3, statsZ1!(L)
                        Next

                        Close #3

                        Open pathCurrentStats$ + statFile$ + "S" For Input As #4

                        Input #4, nbrGamesCurr

                        For I = 1 to MAX_SCHED_STATS: 
                            Input #4, teamScore(I)
                        Next I
                        
                        For I = 1 to MAX_SCHED_STATS: 
                            Input #4, oppName$(I)
                        Next I
                        
                        For I = 1 to MAX_SCHED_STATS: 
                            Input #4, oppScore(I)
                        Next I
                        
                        For I = 1 to MAX_SCHED_STATS: 
                            Input #4, locIndicator$(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, gameAttendance&(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, gameSite$(I)
                        Next I

                        Close #4

                        SEQ = 0

                        For K = 0 To 14
                            If UCase$(statPlyrName_Road$(K)) <> UCase$(statPlyrNames$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        If SEQ = 0 Then

                            Call DoTheMerge(pathCurrentStats$, statFile$, B$, nbrGamesCurr, nbrGamesRoad)

                            For J = 13 To 23
                                Locate J
                                Print Space$(65)
                            Next

                        Else

                            Cls
                            Color 14
                            Locate 5, 27: Print "PLAYERS NOT IN SEQUENCE !!"
                            Color 15
                            Locate 7, 20: Print "STATS WILL BE DISTORTED UNLESS THE ROAD"
                            Locate , 19: Print "FILE LISTS THE PLAYERS IN THE SAME SEQUENCE"
                            Locate , 24: Print "AS YOUR CURRENT PLAYER STAT FILE."
                            Color 12
                            Locate 11, 24: Print "YOU MUST REALIGN ROAD STAT FILE OR"
                            Locate , 31: Print "ENTER STATS MANUALLY."
                            Color 13
                            Locate 15, 29: Line Input "HIT ENTER TO GO TO MENU"; DUMMY$

                        End If 'Done checking sequence

                    Else

                        Cls
                        Color 7
                        Locate 2, 10: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                        Print
                        Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

                        Do
                            Locate , 10
                            Print "TRY AGAIN? (Y/N) "
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                        If UCase$(I$) = "Y" Then
                            Print
                            Print "HIT ENTER TO RE-ENTER INFORMATION"
                            I$ = GetKeyPress$
                        End If

                    End If 'Done checking if home file exists

                Else

                    Cls
                    Locate 2, 10
                    Color 7: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                    Print
                    Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

                    Do
                        Locate , 10
                        Print "TRY AGAIN? (Y/N) "
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    If UCase$(I$) = "Y" Then
                        Print
                        Print "HIT ENTER TO RE-ENTER INFORMATION"
                        I$ = GetKeyPress$
                    End If

                End If 'Done checking if road file exists

            End If 'Done checking if ready to proceed

        End If 'Done checking for ESC

    Loop Until DUMMY$ <> "" Or UCase$(J$) = "N" Or UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       DoTheMerge Subroutine
'----------------------------------------
'This subroutine literally adds the stat
'values together from each file and then
'overwrites the current file with the
'newly aggregated results.
Sub DoTheMerge (pathCurrentStats$, statFile$, B$, nbrGamesCurr, nbrGamesRoad)

    Shared nbrGamesCurr, nbrGamesRoad

    Shared gameAttendance&(), gameAtt_Road&()

    Shared oppScore(), oppScore_Road() 
    Shared teamScore(), teamScore_Road()

    Shared ARD!(), playerRatings!()

    Shared statsW0!(), statsW1!()
    Shared statsZ!(), statsZ1!()
    Shared W0RD!(), W1RD!()
    Shared ZRD!(), ZRD1!()

    Shared gameSite$(), gameSite_Road$()
    Shared locIndicator$(), locIndicator_Road$()
    Shared oppName$(), oppName_Road$()
    Shared statPlyrNames$()

    For K = 1 To nbrGamesRoad
        nbrGamesCurr = nbrGamesCurr + 1
        teamScore(nbrGamesCurr) = teamScore_Road(K)
        oppName$(nbrGamesCurr) = oppName_Road$(K)
        oppScore(nbrGamesCurr) = oppScore_Road(K)
        locIndicator$(nbrGamesCurr) = locIndicator_Road$(K)
        gameAttendance&(nbrGamesCurr) = gameAtt_Road&(nbrGamesCurr)
        gameSite$(nbrGamesCurr) = gameSite_Road$(nbrGamesCurr)
    Next

    Color 12
    Locate 18, 20: Print "MERGING STAT FILE AND SCHEDULE RESULTS . . . ."

    For I = 0 To 14

        For J = 0 To 12
            playerRatings!(I, J) = playerRatings!(I, J) + ARD!(I, J)
        Next J

        statsW0!(I) = statsW0!(I) + W0RD!(I)
        statsW1!(I) = statsW1!(I) + W1RD!(I)

        If playerRatings!(I, 13) < ARD!(I, 13) Then
            playerRatings!(I, 13) = ARD!(I, 13)
        Else
            playerRatings!(I, 13) = playerRatings!(I, 13)
        End If

    Next I

    For L = 0 To 14
        statsZ!(L) = statsZ!(L) + ZRD!(L)
        statsZ1!(L) = statsZ1!(L) + ZRD1!(L)
    Next

    Open pathCurrentStats$ + statFile$ For Output As #1

    Write #1, B$

    For I = 0 To 14:

        Write #1, statPlyrNames$(I)

        For J = 0 To 13: 
            Write #1, playerRatings!(I, J)
        Next

        Write #1, statsW0!(I)
        Write #1, statsW1!(I)

    Next

    For L = 0 To 15: 
        Write #1, statsZ!(L)
    Next
    
    For L = 0 To 15: 
        Write #1, statsZ1!(L)
    Next

    Close #1

    Open pathCurrentStats$ + statFile$ + "S" For Output As #2

    Write #2, nbrGamesCurr

    For I = 1 to MAX_SCHED_STATS: 
        Write #2, teamScore(I)
    Next I

    For I = 1 to MAX_SCHED_STATS: 
        Write #2, oppName$(I)
    Next I

    For I = 1 to MAX_SCHED_STATS: 
        Write #2, oppScore(I)
    Next I

    For I = 1 to MAX_SCHED_STATS: 
        Write #2, locIndicator$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Write #2, gameAttendance&(I)
    Next I
    
    For I = 1 To MAX_SCHED_STATS: 
        Write #2, gameSite$(I)
    Next I

    Close #2

    Y = 0

    Color 12
    Locate 20, 20: Print "RESULTS SUCCESSFULLY MERGED!!"

End Sub


'----------------------------------------
'       GetMergePaths Subroutine
'----------------------------------------
'This subroutine assists the user with the
'entry of the separate paths to be used 
'for the merge / align process.
Sub GetMergePaths (pathCurrentStats$, pathRoadStats$, promptResponse$)

    Shared opSysType$

    Do
        Color 12, 0
        Cls

        Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO MERGE/ALIGN"
        Color 15
        Select Case opSysType$
            Case "[WINDOWS]":
                Locate 4, 5: Print "EXAMPLE - A:"
                Locate , 5: Print "          C:\HAFFNER"
                Locate , 5: Print "          C:\HAFFNER\1960"
                Locate , 5: Print "          D:\PBASKET"
            Case "[LINUX]":
                Locate 4, 5: Print "EXAMPLE - /mnt/floppy"
                Locate , 5: Print "           /usr/bin/haffner"
                Locate , 5: Print "           /usr/bin/haffner/1960"
                Locate , 5: Print "           /home/haffner/pbasket"
            Case "[MAC]", "[MACOSX]":
                Locate 4, 5: Print "EXAMPLE - /Volumes/floppy"
                Locate , 5: Print "           /Applications/haffner"
                Locate , 5: Print "           /Applications/haffner/1960"
                Locate , 5: Print "           /Users/haffner/pbasket"
        End Select

        For J = 10 To 13
            Locate J
            Print Space$(70)
        Next

        Color 10
        Locate 10, 5
        Line Input "ROAD STATS:     "; pathRoadStats$
        Locate , 5
        Locate , 5
        Line Input "CURRENT STATS:  "; pathCurrentStats$

        Color 14
        Locate 13, 5: Print "ANY CHANGE? (Y/N)"
        promptResponse$ = GetKeyPress$

    Loop Until UCase$(promptResponse$) = "N" Or promptResponse$ = Chr$(27)

    'Adjust path terminator for OS type
    Select Case opSysType$
        Case "[WINDOWS]":
            If Not (Right$(pathCurrentStats$, 1) = "\") Then
                pathCurrentStats$ = pathCurrentStats$ + "\"
            End If

            If Not (Right$(pathRoadStats$, 1) = "\") Then
                pathRoadStats$ = pathRoadStats$ + "\"
            End If

        Case "[MAC]", "[MACOSX]", "[LINUX]":
            If Not (Right$(pathCurrentStats$, 1) = "/") Then
                pathCurrentStats$ = pathCurrentStats$ + "/"
            End If

            If Not (Right$(pathRoadStats$, 1) = "/") Then
                pathRoadStats$ = pathRoadStats$ + "/"
            End If
    End Select

End Sub
