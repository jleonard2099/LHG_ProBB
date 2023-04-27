Sub CAREER ()

    ReDim menuChoices$(1 To 6)

    _Title "Basketball: The Pro Game - Career Stats"

    Do

        Color 15, 0

        Cls

        Call TITLE
        'Erase menuChoices$
        menuChoices$(1) = "CREATE AND MERGE INITIAL STAT FILE"
        menuChoices$(2) = "MERGE / UPDATE CAREER FILES       "
        menuChoices$(3) = "VIEW CAREER FILE                  "
        menuChoices$(4) = "PRINT CAREER FILE                 "
        menuChoices$(5) = "VIEW / PRINT CAREER RECORDS       "
        menuChoices$(6) = "EXIT TO MAIN MENU                 "
        Selection% = 1
        Count% = 6
        Start% = 1
        ExitCode% = 6
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 14, 59, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call CreateCareerFile
                Case 2: Call MergeCareerFiles

                    '-- These are basically doing the same thing, it's redundant
                    '-- just combine it into a View/Print option
                Case 3 To 4: Call CARVIEW
                Case 5: Call ViewCareerRecords
                Case 6: Call Main_Menu(1)
            End Select
        End If

    Loop Until Selection% = 6 Or ExitCode% = 27

End Sub

'----------------------------------------
'     CreateCareerFile Subroutine
'----------------------------------------
'... explain here ...
Sub CreateCareerFile ()

    Dim initA!(62, 15)
    Dim initA$(62)
    Dim careerW0!(62), careerW1!(62)

    Shared AR$()
    Shared ars!(), compareZ!(), compareZ1!()
    Shared W0S!(), W1S!()

    TMR% = 945

    NB = -1

    Cls
    Color 14
    Locate 5, 27: Print " - - - C A U T I O N - - -"
    Color 15
    Locate 8, 18: Print "THIS OPTION SHOULD ONLY BE USED WHEN CREATING"
    Locate , 18: Print "AND MERGING THE INITIAL STAT FILE.  IF YOU ARE"
    Locate , 18: Print "ADDING AN ADDITIONAL YEAR TO AN EXISTING FILE,"
    Locate , 18: Print "            USE OPTION #2."
    Print
    Print
    Print
    Color 12
    Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"
    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

    If UCase$(I$) = "Y" Then

        Do
            Cls
            For J = 15 To 18: Locate J: Print Space$(68)
            Next
            Color 11: Locate 15, 5: Input "ENTER NAME OF CAREER STAT FILE (MAX. - 8 CHARACTERS)"; careerFile$
            'This will open the team and load the data
            Call GetTeam(ti)

            B$ = A$

            'ti = team index of chosen team
            '0 means no team was chosen
            If ti <> 0 Then

                For XX = 4 To 11: If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
                Next XX

                For XX = 12 To 14: If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
                Next XX

                QQ = 8

                For XX = 11 To 4 Step -1
                    If Not (Mid$(B$, XX, 1) = "_") Then
                        Exit For
                    End If
                Next XX

                Z1$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)

                Cls
                Locate 8, 25: Input "ENTER YEAR TO ADD (EX. - 1961)"; Y

                Do
                    Locate 9, 25: Print "ANY CHANGE (YN)"
                    J$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                If UCase$(J$) = "N" Then

                    Locate , 5: Print "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"
                    K$ = GetKeyPress$
                    If UCase$(K$) = "Y" Then finalUpdate% = 1
                    If UCase$(K$) = "N" Then finalUpdate% = 0

                    If Not _FileExists(diskPaths$(1) + Z1$) Then
                        Cls
                        Color 7: Print "UNABLE TO LOCATE STAT FILE . . . PLEASE RE-ENTER"
                        Print
                        Line Input "HIT ENTER TO CONTINUE"; DUMMY$
                    Else
                        test$ = Z1$
                        'Read in the stat file Z1$ to arrays
                        Open diskPaths$(1) + Z1$ For Input As #1
                        Input #1, B$
                        For I = 0 To 14:
                            Input #1, initA$(I)
                            For J = 0 To 13: Input #1, initA!(I, J): Next
                            Input #1, careerW0!(I)
                            Input #1, careerW1!(I)
                            TMR% = TMR% - 1
                            Locate 21, 38: Color 5: Print Using "####"; TMR%
                        Next
                        For L = 0 To 15: Input #1, compareZ!(L): Next
                        For L = 0 To 15: Input #1, compareZ1!(L): Next
                        Close #1

                        For I = 0 To 61
                            N = 1
                            If initA$(I) = "END" Or initA$(I) = "XXX" Or initA$(I) = "" Then
                                initA$(I) = ""
                            Else
                                NB = NB + 1
                            End If

                            408 For J = 0 To 13
                                ars!(N, I, J) = initA!(I, J)
                            Next J
                            ars!(N, I, 14) = 1
                            ars!(N, I, 15) = Y
                            W0S!(N, I) = careerW0!(I)
                            W1S!(N, I) = careerW1!(I)
                        Next I

                        For I = 0 To 61: AR$(I) = initA$(I): Next
                        If finalUpdate% = 1 Then Call ReadCareerLeaders(careerFile$)

                        Locate 20, 20: Color 15: Print "MERGING CAREER STATS . . . . PLEASE WAIT"

                        Open diskPaths$(1) + careerFile$ + ".CAR" For Output As #2
                        Write #2, NB, B$
                        For N = 1 To 15
                            For I = 0 To 61
                                Write #2, AR$(I)
                                For J = 0 To 15: Write #2, ars!(N, I, J): Next J
                                Write #2, W0S!(N, I)
                                Write #2, W1S!(N, I)
                                TMR% = TMR% - 1
                                Locate 21, 38: Color 5: Print Using "####"; TMR%
                            Next I
                        Next N
                        Close #2

                    End If 'Done checking for stat file

                End If 'Done checking for changes

            End If 'Done checking if team was chosen

        Loop Until UCase$(I$) = "N" Or UCase$(J$) = "N"

    End If 'Done checking if user is ready

    'Call CAREER

End Sub

'----------------------------------------
'     MergeCareerFiles Subroutine
'----------------------------------------
'... explain here ...
Sub MergeCareerFiles ()

    Color 14, 0
    Cls

    Locate 5, 27: Print "- - - C A U T I O N - - -"

    Color 15

    Locate 8, 1: Print "THIS OPTION SHOULD ONLY BE USED WHEN ADDING YEARS TO AN EXISTING STAT"
    Print "FILE. IF YOU ARE CREATING A NEW CAREER FILE, USE THE CREATE OPTION."
    Print
    Print "IF YOU ARE ADDING AN ADDITIONAL CAREER YEAR, MAKE SURE THAT ANY UNUSED PLAYER"
    Print "SLOTS (XXX, END, ETC.) ARE AT THE END OF THE SEASON STAT LIST BEFORE YOU MERGE."
    Print
    Print "ALSO MAKE SURE THAT THE PLAYERS NAMES IN THE FILE YOU ARE ADDING, MATCH"
    Print "THE EXACT SPELLING OF THE NAME OF THE SAME PLAYER IN THE CAREER FILE FOR THE"
    Print "PLAYERS THAT HAD PLAYED IN PREVIOUS YEARS.  EXAMPLE - IF BILLY WILLIAMS IS"
    Print "LISTED AS 'WILLIAMS' IN THE CAREER FILE AND THE SEASON STATS HAD HIM AS"
    Print "'B.WILLIAMS', HE SHOULD BE CHANGED TO 'WILLIAMS' IN THE SEASON FILE BEFORE"
    Print "MERGING."
    Print
    Print

    Color 12
    Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"
    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y" Or I$ = Chr$(27)

    If UCase$(I$) = "Y" Then

        Spec$ = diskPaths$(1) + "*.CAR"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then
            Locate , 10
            Call CSMsgBox("NO CAREER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
            Exit Sub
        End If

        ReDim careerFiles$(0 To Count%)
        For X = 1 To Count%
            careerFiles$(X) = Space$(12)
        Next
        careerFiles$(0) = Spec$
        Call ReadFile(careerFiles$())

        Do
            Cls
            Color , 1
            Cls
            Locate , 28
            Color 14
            Print "SELECT FILE TO UPDATE": Color 15
            Selection% = 1
            Start% = 1
            FileFGColr% = 15: FileBGColr% = 1
            HiLiteFGColr% = 0: HiLiteBGColr% = 7
            Rows% = 20
            Columns% = 4
            Spacing% = 6
            Row% = 4
            Column% = 1
            Locate 25, 1
            Color 14
            Print "ENTER";: Color 15
            Print "-SELECTS FILE";

            _MouseShow "DEFAULT"
            MAMenu careerFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
            _MouseHide

            If ExitCode% <> 27 Then
                Color 15, 1
                targetFile$ = RTrim$(careerFiles$(Selection%))
                Locate 2, 33: Print targetFile$

                Do
                    Locate 3, 33: Print "ANY CHANGE (YN)";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                Color 15, 0

            End If

        Loop Until UCase$(I$) = "N" Or ExitCode% = 27

        If ExitCode% <> 27 Then

            careerFile$ = Left$(targetFile$, Len(targetFile$) - 4)

            'This will open the team and load the data
            Call GetTeam(ti)
            B$ = A$

            For XX = 4 To 11: If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
            Next XX

            For XX = 12 To 14: If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
            Next XX
            QQ = 8

            For XX = 11 To 4 Step -1
                If Not (Mid$(B$, XX, 1) = "_") Then
                    Exit For
                End If
            Next XX

            Z1$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)

            Call AddYear(careerFile$, Z1$)

        End If

    End If

End Sub

'----------------------------------------
'           AddYear Subroutine
'----------------------------------------
'... explain here ...
Sub AddYear (selectedFile$, statFile$)

    Dim initA$(62)

    Dim mergeA!(15, 62)
    Dim careerW0!(62), careerW1!(62)

    Shared AR$()

    Shared AL!()
    Shared ars!(), compareZ!(), compareZ1!()
    Shared W0S!(), W1S!()

    'needs to be a constant???
    TMR1% = 1875

    Cls

    Do
        Locate 8, 25: Input "ENTER YEAR TO ADD (EX. - 1961)"; statYear
        Locate 9, 25: Print "ANY CHANGE (YN)"
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    If UCase$(I$) = "N" Then

        Locate , 5: Print "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"
        I$ = GetKeyPress$

        If UCase$(I$) = "Y" Then finalUpdate% = 1 Else finalUpdate% = 0

        Locate 20, 5: Color 15: Print "MERGING CAREER STATS AND UPDATING CAREER LEADER FILE . . . . PLEASE WAIT"

        Open diskPaths$(1) + selectedFile$ + ".CAR" For Input As #2
        Input #2, NB, B$

        For N = 1 To 15
            For I = 0 To 61
                Input #2, AR$(I)
                For J = 0 To 15: Input #2, ars!(N, I, J): Next J
                Input #2, W0S!(N, I)
                Input #2, W1S!(N, I)
                TMR1% = TMR1% - 1
                Locate 21, 38: Color 5: Print Using "####"; TMR1%
            Next I
        Next N
        Close #2

        Locate 20, 20: Color 15: Print "MERGING CAREER STATS . . . . PLEASE WAIT"

        If _FileExists(diskPaths$(1) + statFile$) Then

            Open diskPaths$(1) + statFile$ For Input As #1
            Input #1, B$

            For I = 0 To 14
                Input #1, initA$(I)
                For J = 0 To 13: Input #1, mergeA!(I, J): Next
                Input #1, careerW0!(I)
                Input #1, careerW1!(I)
                TMR1% = TMR1% - 1
                Locate 21, 38: Color 5: Print Using "####"; TMR1%
            Next

            For L = 0 To 15: Input #1, compareZ!(L): Next
            For L = 0 To 15: Input #1, compareZ1!(L): Next
            Close #1

            For I = 0 To 14
                RR = 0
                CHK = 0

                For J = 0 To 61
                    If AR$(J) = initA$(I) Then
                        For W = 1 To 15
                            If ars!(W, J, 15) = statYear Then
                                1412 CHK = CHK + 1
                            End If
                        Next W

                        If CHK <= 0 Then
                            N = ars!(1, J, 14) + ars!(2, J, 14) + ars!(3, J, 14) + ars!(4, J, 14)
                            N = N + ars!(5, J, 14) + ars!(6, J, 14) + ars!(7, J, 14) + ars!(8, J, 14)
                            N = N + ars!(9, J, 14) + ars!(10, J, 14) + ars!(11, J, 14)
                            N = N + ars!(12, J, 14) + ars!(13, J, 14) + ars!(14, J, 14) + ars!(15, J, 14) + 1
                        Else
                            N = ars!(1, J, 14) + ars!(2, J, 14) + ars!(3, J, 14) + ars!(4, J, 14)
                            N = N + ars!(5, J, 14) + ars!(6, J, 14) + ars!(7, J, 14) + ars!(8, J, 14)
                            N = N + ars!(9, J, 14) + ars!(10, J, 14) + ars!(11, J, 14)
                            N = N + ars!(12, J, 14) + ars!(13, J, 14) + ars!(14, J, 14) + ars!(15, J, 14)
                        End If

                        For K = 0 To 13
                            ars!(N, J, K) = mergeA!(I, K)
                        Next K
                        ars!(N, J, 14) = 1
                        ars!(N, J, 15) = statYear
                        W0S!(N, J) = careerW0!(J)
                        W1S!(N, J) = careerW1!(J)
                    Else
                        RR = RR + 1
                        If RR = 62 Then
                            If Not (initA$(I) = "END" Or initA$(I) = "XXX" Or initA$(I) = "") Then

                                NB = NB + 1
                                AR$(NB) = initA$(I)
                                N = 1

                                For K = 0 To 13: ars!(N, NB, K) = mergeA!(I, K): Next K

                                ars!(N, NB, 14) = 1
                                ars!(N, NB, 15) = statYear
                                W0S!(N, NB) = careerW0!(NB)
                                W1S!(N, NB) = careerW1!(NB)
                            End If
                        End If

                    End If

                Next J

            Next I

            If finalUpdate% = 1 Then Call ReadCareerLeaders(selectedFile$)

            Locate 20, 20: Color 15: Print "MERGING CAREER STATS . . . . PLEASE WAIT"

            Open diskPaths$(1) + selectedFile$ + ".CAR" For Output As #2
            Write #2, NB, B$
            For N = 1 To 15
                For I = 0 To 61: Write #2, AR$(I)
                    For J = 0 To 15: Write #2, ars!(N, I, J): Next J
                    Write #2, W0S!(N, I)
                    Write #2, W1S!(N, I)
                    TMR1% = TMR1% - 1
                    Locate 21, 38: Color 5: Print Using "####"; TMR1%
                Next I
            Next N
            Close #2

        Else
            Cls
            Color 7: Print "UNABLE TO LOCATE STAT FILE . . . PLEASE RE-ENTER"
            Print
            Line Input "HIT ENTER TO CONTINUE"; DUMMY$

        End If 'Done checking file exists

    End If 'Done checking for user continue

End Sub

'----------------------------------------
'     ReadCareerFile Subroutine
'----------------------------------------
'... explain here ...
Sub ReadCareerLeaders (careerFile$)

    Dim BL!(62), BLYR!(21)
    Dim initA$(62)
    Dim BL$(0 To 20)

    Shared AR$()
    Shared ars!(), W0S!(), W1S!()

    If _FileExists(diskPaths$(1) + careerFile$ + ".CLD") Then
        Open diskPaths$(1) + careerFile$ + ".CLD" For Input As #1
        For I = 0 To 20
            Input #1, BLYR!(I), BL$(I), BL!(I)
        Next
        Close #1
    End If

    For I = 0 To 61

        If AR$(I) <> "" And AR$(I) <> "XXX" And AR$(I) <> "END" Then

            For J = 1 To 15
                If ars!(J, I, 15) <> 0 Then
                    If ars!(J, I, 1) > BL!(0) Then
                        BL!(0) = ars!(J, I, 1)
                        BL$(0) = AR$(I)
                        BLYR!(0) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 2) > BL!(1) Then
                        BL!(1) = ars!(J, I, 2): BL$(1) = AR$(I): BLYR!(1) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 1) <> 0 And ars!(J, I, 1) / ars!(J, I, 0) >= 3.65 Then
                        If ars!(J, I, 2) / ars!(J, I, 1) > BL!(2) Then
                            BL!(2) = ars!(J, I, 2) / ars!(J, I, 1): BL$(2) = AR$(I): BLYR!(2) = ars!(J, I, 15)
                        End If
                    End If

                    If ars!(J, I, 4) > BL!(3) Then
                        BL!(3) = ars!(J, I, 4): BL$(3) = AR$(I): BLYR!(3) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 3) > BL!(4) Then
                        BL!(4) = ars!(J, I, 3): BL$(4) = AR$(I): BLYR!(4) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 3) <> 0 And ars!(J, I, 3) / ars!(J, I, 0) >= 1.5 Then
                        If ars!(J, I, 4) / ars!(J, I, 3) > BL!(5) Then
                            BL!(5) = ars!(J, I, 4) / ars!(J, I, 3): BL$(5) = AR$(I): BLYR!(5) = ars!(J, I, 15)
                        End If
                    End If

                    If W0S!(J, I) > BL!(6) Then
                        BL!(6) = W0S!(J, I): BL$(6) = AR$(I): BLYR!(6) = ars!(J, I, 15)
                    End If

                    If W1S!(J, I) > BL!(7) Then
                        BL!(7) = W1S!(J, I): BL$(7) = AR$(I): BLYR!(7) = ars!(J, I, 15)
                    End If

                    If W1S!(J, I) <> 0 And W1S!(J, I) / ars!(J, I, 0) >= 0.3 Then
                        If W0S!(J, I) / W1S!(J, I) > BL!(8) Then
                            BL!(8) = W0S!(J, I) / W1S!(J, I): BL$(8) = AR$(I): BLYR!(8) = ars!(J, I, 15)
                        End If
                    End If

                    If ars!(J, I, 2) * 2 + ars!(J, I, 4) + W0S!(J, I) > BL!(9) Then
                        BL!(9) = ars!(J, I, 2) * 2 + ars!(J, I, 4) + W0S!(J, I): BL$(9) = AR$(I): BLYR!(9) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 0) <> 0 Then
                        If (ars!(J, I, 2) * 2 + ars!(J, I, 4) + W0S!(J, I)) / ars!(J, I, 0) > BL!(10) Then
                            BL!(10) = (ars!(J, I, 2) * 2 + ars!(J, I, 4) + W0S!(J, I)) / ars!(J, I, 0): BL$(10) = AR$(I): BLYR!(10) = ars!(J, I, 15)
                        End If
                    End If

                    If ars!(J, I, 12) > BL!(11) Then
                        BL!(11) = ars!(J, I, 12): BL$(11) = AR$(I): BLYR!(11) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 5) - ars!(J, I, 12) > BL!(12) Then
                        BL!(12) = ars!(J, I, 5) - ars!(J, I, 12): BL$(12) = AR$(I): BLYR!(12) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 5) > BL!(13) Then
                        BL!(13) = ars!(J, I, 5): BL$(13) = AR$(I): BLYR!(13) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 0) <> 0 Then
                        If ars!(J, I, 5) / ars!(J, I, 0) > BL!(14) Then
                            BL!(14) = ars!(J, I, 5) / ars!(J, I, 0): BL$(14) = AR$(I): BLYR!(14) = ars!(J, I, 15)
                        End If
                    End If

                    If ars!(J, I, 6) > BL!(15) Then
                        BL!(15) = ars!(J, I, 6): BL$(15) = AR$(I): BLYR!(15) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 8) > BL!(16) Then
                        BL!(16) = ars!(J, I, 8): BL$(16) = AR$(I): BLYR!(16) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 9) > BL!(17) Then
                        BL!(17) = ars!(J, I, 9): BL$(17) = AR$(I): BLYR!(17) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 7) > BL!(18) Then
                        BL!(18) = ars!(J, I, 7): BL$(18) = AR$(I): BLYR!(18) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 11) > BL!(19) Then
                        BL!(19) = ars!(J, I, 11): BL$(19) = AR$(I): BLYR!(19) = ars!(J, I, 15)
                    End If

                    If ars!(J, I, 13) > BL!(20) Then
                        BL!(20) = ars!(J, I, 13): BL$(20) = AR$(I): BLYR!(20) = ars!(J, I, 15)
                    End If

                End If

            Next J

        End If

    Next I

    Open diskPaths$(1) + careerFile$ + ".CLD" For Output As #1
    For I = 0 To 20
        Write #1, BLYR!(I), BL$(I), BL!(I)
    Next
    Close #1

End Sub

'----------------------------------------
'     ViewCareerRecords Subroutine
'----------------------------------------
'... explain here ...
Sub ViewCareerRecords ()

    Dim BL!(62), BLYR!(21)
    Dim BL$(0 To 20)

    Do
        'Display list of files...

        Spec$ = diskPaths$(1) + "*.CLD"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then
            Locate , 10
            Call CSMsgBox("NO CAREER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            ReDim careerLeaderFiles$(0 To Count%)
            For X = 1 To Count%
                careerLeaderFiles$(X) = Space$(12)
            Next
            careerLeaderFiles$(0) = Spec$
            Call ReadFile(careerLeaderFiles$())

            Do
                Color , 1
                Cls

                Locate , 28
                Color 14
                Print "SELECT FILE TO VIEW": Color 15
                Selection% = 1
                Start% = 1
                FileFGColr% = 15: FileBGColr% = 1
                HiLiteFGColr% = 0: HiLiteBGColr% = 7
                Rows% = 20
                Columns% = 4
                Spacing% = 6
                Row% = 4
                Column% = 1
                Locate 25, 1
                Color 14
                Print "ENTER";: Color 15
                Print "-SELECTS FILE";

                _MouseShow "DEFAULT"
                MAMenu careerLeaderFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then
                    Color 15, 1
                    targetFile$ = careerLeaderFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate 3, 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then

                targetFile$ = RTrim$(targetFile$)
                ZZ$ = Left$(targetFile$, Len(targetFile$) - 4)

                Open diskPaths$(1) + ZZ$ + ".CLD" For Input As #1
                For I = 0 To 20
                    Input #1, BLYR!(I), BL$(I), BL!(I)
                Next
                Close #1

                Do

                    Color 15, 0
                    Cls

                    Do
                        Locate 15, 10
                        Color 12: Print "V";: Color 7: Print "iew or ";
                        Color 12: Print "P";: Color 7: Print "rint"
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "P" Or UCase$(I$) = "V"

                    Select Case UCase$(I$)
                        Case "V":
                            Do
                                Cls
                                Color 0, 15

                                Color 12: Print "CATEGORY                     RECORD           PLAYER              YEAR"
                                Print
                                'BL!() = Record Val ; BL$() = Record Holder; BLYR!() = RecordYear
                                Color 3: Print "FIELD GOALS MADE";: Color 7: Print Tab(33); Using "####"; BL!(0);: Print Tab(47); BL$(0);: Print Tab(66); BLYR!(0)
                                Color 3: Print "FIELD GOAL ATTEMPTS";: Color 7: Print Tab(33); Using "####"; BL!(1);: Print Tab(47); BL$(1);: Print Tab(66); BLYR!(1)
                                Color 3: Print "FIELD GOAL PCT.";: Color 7: Print Tab(32); Using "#.###"; BL!(2);: Print Tab(47); BL$(2);: Print Tab(66); BLYR!(2)
                                Color 3: Print "FREE THROWS MADE";: Color 7: Print Tab(33); Using "####"; BL!(3);: Print Tab(47); BL$(3);: Print Tab(66); BLYR!(3)
                                Color 3: Print "FREE THROW ATTEMPTS";: Color 7: Print Tab(33); Using "####"; BL!(4);: Print Tab(47); BL$(4);: Print Tab(66); BLYR!(4)
                                Color 3: Print "FREE THROW PCT.";: Color 7: Print Tab(32); Using "#.###"; BL!(5);: Print Tab(47); BL$(5);: Print Tab(66); BLYR!(5)
                                Color 3: Print "3 PT. FIELD GOALS MADE";: Color 7: Print Tab(33); Using "####"; BL!(6);: Print Tab(47); BL$(6);: Print Tab(66); BLYR!(6)
                                Color 3: Print "3 PT. FILED GOAL ATTEMPTS";: Color 7: Print Tab(33); Using "####"; BL!(7);: Print Tab(47); BL$(7);: Print Tab(66); BLYR!(7)
                                Color 3: Print "3 PT. FIELD GOAL PCT.";: Color 7: Print Tab(32); Using "#.###"; BL!(8);: Print Tab(47); BL$(8);: Print Tab(66); BLYR!(8)
                                Color 3: Print "POINTS";: Color 7: Print Tab(32); Using "#####"; BL!(9);: Print Tab(47); BL$(9);: Print Tab(66); BLYR!(9)
                                Color 3: Print "POINTS PER GAME";: Color 7: Print Tab(32); Using "###.#"; BL!(10);: Print Tab(47); BL$(10);: Print Tab(66); BLYR!(10)
                                Color 3: Print "OFFENSIVE REBOUNDS";: Color 7: Print Tab(32); Using "#####"; BL!(11);: Print Tab(47); BL$(11);: Print Tab(66); BLYR!(11)
                                Color 3: Print "DEFENSIVE REBOUNDS";: Color 7: Print Tab(32); Using "#####"; BL!(12);: Print Tab(47); BL$(12);: Print Tab(66); BLYR!(12)
                                Color 3: Print "TOTAL REBOUNDS";: Color 7: Print Tab(32); Using "#####"; BL!(13);: Print Tab(47); BL$(13);: Print Tab(66); BLYR!(13)
                                Color 3: Print "REBOUNDS PER GAME";: Color 7: Print Tab(32); Using "###.#"; BL!(14);: Print Tab(47); BL$(14);: Print Tab(66); BLYR!(14)
                                Color 15: Locate 22: Print "HIT ANY KEY TO CONTINUE"
                                A$ = GetKeyPress$

                                Cls
                                Color 12: Print "CATEGORY                     RECORD           PLAYER              YEAR"
                                Print
                                Color 3: Print "PERSONAL FOULS";: Color 7: Print Tab(33); Using "####"; BL!(15);: Print Tab(47); BL$(15);: Print Tab(66); BLYR!(15)
                                Color 3: Print "ASSISTS";: Color 7: Print Tab(32); Using "#####"; BL!(16);: Print Tab(47); BL$(16);: Print Tab(66); BLYR!(16)
                                Color 3: Print "SHOTS BLOCKED";: Color 7: Print Tab(33); Using "####"; BL!(17);: Print Tab(47); BL$(17);: Print Tab(66); BLYR!(17)
                                Color 3: Print "STEALS";: Color 7: Print Tab(32); Using "#####"; BL!(18);: Print Tab(47); BL$(18);: Print Tab(66); BLYR!(18)
                                Color 3: Print "TURNOVERS";: Color 7: Print Tab(32); Using "#####"; BL!(19);: Print Tab(47); BL$(19);: Print Tab(66); BLYR!(19)
                                Color 3: Print "HI-GAME (POINTS)";: Color 7: Print Tab(33); Using "####"; BL!(20);: Print Tab(47); BL$(20);: Print Tab(66); BLYR!(20)

                                Color 15

                                Do
                                    Locate 22: Print "VIEW AGAIN (Y/N)"
                                    A$ = GetKeyPress$
                                Loop Until UCase$(A$) = "N" Or UCase$(A$) = "Y"

                            Loop Until UCase$(A$) = "N"

                        Case "P":

                            LPrint "CATEGORY                     RECORD           PLAYER              YEAR"
                            LPrint
                            LPrint "FIELD GOALS MADE";: LPrint Tab(33); Using "####"; BL!(0);: LPrint Tab(47); BL$(0);: LPrint Tab(66); BLYR!(0)
                            LPrint "FIELD GOAL ATTEMPTS";: LPrint Tab(33); Using "####"; BL!(1);: LPrint Tab(47); BL$(1);: LPrint Tab(66); BLYR!(1)
                            LPrint "FIELD GOAL PCT.";: LPrint Tab(32); Using "#.###"; BL!(2);: LPrint Tab(47); BL$(2);: LPrint Tab(66); BLYR!(2)
                            LPrint "FREE THROWS MADE";: LPrint Tab(33); Using "####"; BL!(3);: LPrint Tab(47); BL$(3);: LPrint Tab(66); BLYR!(3)
                            LPrint "FREE THROW ATTEMPTS";: LPrint Tab(33); Using "####"; BL!(4);: LPrint Tab(47); BL$(4);: LPrint Tab(66); BLYR!(4)
                            LPrint "FREE THROW PCT.";: LPrint Tab(32); Using "#.###"; BL!(5);: LPrint Tab(47); BL$(5);: LPrint Tab(66); BLYR!(5)
                            LPrint "3 PT. FIELD GOALS MADE";: LPrint Tab(33); Using "####"; BL!(6);: LPrint Tab(47); BL$(6);: LPrint Tab(66); BLYR!(6)
                            LPrint "3 PT. FILED GOAL ATTEMPTS";: LPrint Tab(33); Using "####"; BL!(7);: LPrint Tab(47); BL$(7);: LPrint Tab(66); BLYR!(7)
                            LPrint "3 PT. FIELD GOAL PCT.";: LPrint Tab(32); Using "#.###"; BL!(8);: LPrint Tab(47); BL$(8);: LPrint Tab(66); BLYR!(8)
                            LPrint "POINTS";: LPrint Tab(32); Using "#####"; BL!(9);: LPrint Tab(47); BL$(9);: LPrint Tab(66); BLYR!(9)
                            LPrint "POINTS PER GAME";: LPrint Tab(32); Using "###.#"; BL!(10);: LPrint Tab(47); BL$(10);: LPrint Tab(66); BLYR!(10)
                            LPrint "OFFENSIVE REBOUNDS";: LPrint Tab(32); Using "#####"; BL!(11);: LPrint Tab(47); BL$(11);: LPrint Tab(66); BLYR!(11)
                            LPrint "DEFENSIVE REBOUNDS";: LPrint Tab(32); Using "#####"; BL!(12);: LPrint Tab(47); BL$(12);: LPrint Tab(66); BLYR!(12)
                            LPrint "TOTAL REBOUNDS";: LPrint Tab(32); Using "#####"; BL!(13);: LPrint Tab(47); BL$(13);: LPrint Tab(66); BLYR!(13)
                            LPrint "REBOUNDS PER GAME";: LPrint Tab(32); Using "###.#"; BL!(14);: LPrint Tab(47); BL$(14);: LPrint Tab(66); BLYR!(14)
                            LPrint "PERSONAL FOULS";: LPrint Tab(33); Using "####"; BL!(15);: LPrint Tab(47); BL$(15);: LPrint Tab(66); BLYR!(15)
                            LPrint "ASSISTS";: LPrint Tab(32); Using "#####"; BL!(16);: LPrint Tab(47); BL$(16);: LPrint Tab(66); BLYR!(16)
                            LPrint "SHOTS BLOCKED";: LPrint Tab(33); Using "####"; BL!(17);: LPrint Tab(47); BL$(17);: LPrint Tab(66); BLYR!(17)
                            LPrint "STEALS";: LPrint Tab(32); Using "#####"; BL!(18);: LPrint Tab(47); BL$(18);: LPrint Tab(66); BLYR!(18)
                            LPrint "TURNOVERS";: LPrint Tab(32); Using "#####"; BL!(19);: LPrint Tab(47); BL$(19);: LPrint Tab(66); BLYR!(19)
                            LPrint "HI-GAME (POINTS)";: LPrint Tab(33); Using "####"; BL!(20);: LPrint Tab(47); BL$(20);: LPrint Tab(66); BLYR!(20)
                            LPrint Chr$(12)

                    End Select

                Loop Until UCase$(I$) = "V" Or UCase$(I$) = "P" Or I$ = Chr$(27)

            End If 'Done checking for ESC

        End If 'Done checking for CLD files

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0

End Sub

'----------------------------------------
'           GetTeam Subroutine
'----------------------------------------
'... explain here ...
Sub GetTeam (teamIDX)

    'Do
    Color 15, 0
    Cls
    JJ = 6
    SN = 1: Locate 6, 8: Color 15, 4
    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
    Locate 7 + JJ, 8
    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

    Locate 7, 32
    Locate , 12: Print "ENTER DISK ID: ": Print
    Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
    Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
    Print
    Locate , 12: Print "ESC TO RETURN TO MAIN MENU"

    I$ = GetKeyPress$
    If I$ <> Chr$(27) Then
        If I$ = Chr$(13) Then
            Call LoadTeam_CAREER(YN$, NTMS, teamIDX)
            If BO% = 1 Then BO% = 0 ': Exit Sub
        Else

            Locate 7, 27: Print I$
            Locate 7, 28: Line Input NY$
            NY$ = I$ + NY$
            Color 15, 0

            If NY$ = "" And YN$ <> "" Then
                'NY$ = YN$
                Call LoadTeam_CAREER(YN$, NTMS, teamIDX)
                If BO% = 1 Then BO% = 0 ': Exit Sub
            Else
                YN$ = NY$
                Erase teamNames$

                NTMS = 0
                Open diskPaths$(0) + "PROBBTMS." + YN$ For Random As #1 Len = 1031
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
                    If CK% = 999 Then
                        NTMS = NTMS + 1
                        teamNames$(NTMS) = Q$(0)
                        teamIndex%(NTMS) = I1
                    End If
                Next
                Close 1

                Call LoadTeam_CAREER(YN$, NTMS, teamIDX)

            End If 'End checking if NY$ = ""

        End If 'End processing Disk ID

    End If 'Done checking for ESC

    ' Loop Until UCase$(I$) = "N" Or ExitCode% = 27

    Color 15, 0

End Sub

'----------------------------------------
'       LoadTeam_CAREER Subroutine
'----------------------------------------
'...explanation...
Sub LoadTeam_CAREER (yearNumber$, numberTeams, teamNumber)

    Dim careerT%(0 To 34)

    Do
        1102 Selection% = 1
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

        If ExitCode% = 27 Then Exit Sub

        teamNumber = teamIndex%(Selection%)

        Open diskPaths$(0) + "PROBBTMS." + yearNumber$ For Random As #1 Len = 1031
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

        Get #1, teamNumber

        For I = 0 To 34: careerT%(I) = CVI(Q$(I + 10)): Next
        Close 1

        A$ = RTrim$(Q$(0))
        B$ = A$

        Locate 2, 33
        Color careerT%(33), careerT%(34): Print A$

        Color 15, 4

        Do
            Locate 3, 33: Print "ANY CHANGE (YN)";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Loop Until UCase$(I$) = "N"

End Sub
