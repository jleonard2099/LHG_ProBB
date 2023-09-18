'----------------------------------------
'         RecordsMenu Subroutine
'----------------------------------------
'This subroutine gives the user a number of 
'menu selections for viewing, updating and
'printing records
Sub RecordsMenu ()

    _Title "Basketball: The Pro Game - Analyze Records"

    ReDim menuChoices$(1 To 7)

    Shared teamRecords$(), indRecords$()
    Shared BRC$(), TRC$(), TRC1$()
    Shared BRC!(), TRC!(), TRC1!()

    If Not _FileExists(diskPaths$(1) + "RECORDS.PBB") Then

        MsgText$ = "UNABLE TO LOCATE RECORDS PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Open diskPaths$(1) + "RECORDS.PBB" For Input As #1

        For I = 1 To 25
            Input #1, BRC!(I)
            Input #1, BRC$(I, 0)
            Input #1, BRC$(I, 1)
        Next
        For I = 1 To 21
            Input #1, TRC!(I)
            Input #1, TRC$(I)
            If TRC!(I) = 19999 Then TRC!(I) = 0: TRC$(I) = ""
        Next
        For I = 1 To 21
            Input #1, TRC1!(I)
            Input #1, TRC1$(I)
            If TRC1!(I) = 19999 Then TRC1!(I) = 0: TRC1$(I) = ""
        Next
        Close #1

        Do

            Call TITLE

            menuChoices$(1) = "VIEW SEASON RECORDS      "
            menuChoices$(2) = "PRINT SEASON RECORDS     "
            menuChoices$(3) = "DELETE SEASON RECORDS    "
            menuChoices$(4) = "VIEW SINGLE GAME RECORDS "
            menuChoices$(5) = "PRINT SINGLE GAME RECORDS"
            menuChoices$(6) = "DELETE GAME RECORDS      "
            menuChoices$(7) = "RETURN TO GAME MENU      "
            Selection% = 1
            Count% = 7
            Start% = 1
            ExitCode% = 7
            FileFGColor% = 15: FileBGColor% = 1
            HiLiteFGColor% = 0: HiLiteBGColor% = 7
            Spacing% = 1
            Row% = 8
            Column% = 25

            Call box0(7, 24, 15, 50, 2, 15, 1)
            _MouseShow "DEFAULT"
            MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
            _MouseHide

            If ExitCode% <> 27 Then

                Select Case Selection%

                    Case 1:
                        Call ViewSeasonRecords

                    Case 2:
                        Call PrintSeasonRecords

                    Case 3:
                        If _FileExists(diskPaths$(1) + "RECORDS.PBB") Then Kill diskPaths$(1) + "RECORDS.PBB"

                    Case 4:
                        Call GameRecords(1)

                    Case 5:
                        Call GameRecords(2)

                    Case 6:
                        If _FileExists(diskPaths$(1) + "RECORDS.GAM") Then Kill diskPaths$(1) + "RECORDS.GAM"

                End Select

            End If

        Loop Until Selection% = 7 Or ExitCode% = 27

    End If

End Sub

'----------------------------------------
'     ViewSeasonRecords Subroutine
'----------------------------------------
'This subroutine assists the usere with
'viewing the records on the default display
Sub ViewSeasonRecords ()

    Shared teamRecords$(), indRecords$()
    Shared BRC$(), TRC$(), TRC1$()
    Shared BRC!(), TRC!(), TRC1!()

    Color 0, 0
    Cls

    J = 20

    Locate 1, 1

    Color 15, 4
    Print " " + Chr$(214); String$(73, 196); "� ";

    For II = 1 To J
        Color 15, 4
        Locate 1 + II, 1
        Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
        Color 7, 4: Print String$(2, 177)
    Next

    Color 15, 4
    Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
    Color 7, 4: Print String$(2, 177)
    Locate , 3: Print String$(77, 177)

    Color 15
    Locate 2, 23
    Print "******* INDIVIDUAL PLAYER RECORDS *******"
    Print
    Locate , 4: Print "CATEGORY                PLAYER           TEAM            RECORD"

    Color 7
    'Games Played
    Locate , 4: Print indRecords$(1); Tab(28); Left$(BRC$(1, 0), 14); Tab(42); BRC$(1, 1); Tab(62); Using "####"; BRC!(1)
    'Field Goal Attempts
    Locate , 4: Print indRecords$(2); Tab(28); Left$(BRC$(2, 0), 14); Tab(42); BRC$(2, 1); Tab(62); Using "####"; BRC!(2)
    'Field Goals Made
    Locate , 4: Print indRecords$(3); Tab(28); Left$(BRC$(3, 0), 14); Tab(42); BRC$(3, 1); Tab(62); Using "####"; BRC!(3)
    'Field Goal %
    Locate , 4: Print indRecords$(4); Tab(28); Left$(BRC$(4, 0), 14); Tab(42); BRC$(4, 1); Tab(62); Using "##.#"; BRC!(4) * 100
    '3 Field Goal Attempts
    Locate , 4: Print indRecords$(5); Tab(28); Left$(BRC$(5, 0), 14); Tab(42); BRC$(5, 1); Tab(62); Using "####"; BRC!(5)
    '3 Field Goal Made
    Locate , 4: Print indRecords$(6); Tab(28); Left$(BRC$(6, 0), 14); Tab(42); BRC$(6, 1); Tab(62); Using "####"; BRC!(6)
    '3 Field Goal %
    Locate , 4: Print indRecords$(7); Tab(28); Left$(BRC$(7, 0), 14); Tab(42); BRC$(7, 1); Tab(62); Using "##.#"; BRC!(7) * 100
    'Free Throw Attempts
    Locate , 4: Print indRecords$(8); Tab(28); Left$(BRC$(8, 0), 14); Tab(42); BRC$(8, 1); Tab(62); Using "####"; BRC!(8)
    'Free Throws Made
    Locate , 4: Print indRecords$(9); Tab(28); Left$(BRC$(9, 0), 14); Tab(42); BRC$(9, 1); Tab(62); Using "####"; BRC!(9)
    'Free Throw %
    Locate , 4: Print indRecords$(10); Tab(28); Left$(BRC$(10, 0), 14); Tab(42); BRC$(10, 1); Tab(62); Using "##.#"; BRC!(10) * 100
    'Minutes Played
    Locate , 4: Print indRecords$(11); Tab(28); Left$(BRC$(11, 0), 14); Tab(42); BRC$(11, 1); Tab(62); Using "####"; BRC!(11)
    'Points
    Locate , 4: Print indRecords$(12); Tab(28); Left$(BRC$(12, 0), 14); Tab(42); BRC$(12, 1); Tab(62); Using "####"; BRC!(12)
    'Scoring Average
    Locate , 4: Print indRecords$(13); Tab(28); Left$(BRC$(13, 0), 14); Tab(42); BRC$(13, 1); Tab(62); Using "####"; BRC!(13)

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    For I = 4 To 21: Locate I, 4
        Print Space$(72)
    Next

    Color 0, 0
    Cls

    J = 20

    Locate 1, 1

    Color 15, 4
    Print " " + Chr$(214); String$(73, 196); "� ";

    For II = 1 To J
        Color 15, 4
        Locate 1 + II, 1
        Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
        Color 7, 4: Print String$(2, 177)
    Next

    Color 15, 4
    Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
    Color 7, 4: Print String$(2, 177)
    Locate , 3: Print String$(77, 177)

    Color 15
    Locate 2, 23
    Print "******* INDIVIDUAL PLAYER RECORDS *******"
    Print
    Locate , 4: Print "CATEGORY                PLAYER           TEAM            RECORD"

    Color 7

    Locate , 4: Print indRecords$(14); Tab(28); Left$(BRC$(14, 0), 14); Tab(42); BRC$(14, 1); Tab(62); Using "####"; BRC!(14)
    Locate , 4: Print indRecords$(15); Tab(28); Left$(BRC$(15, 0), 14); Tab(42); BRC$(15, 1); Tab(62); Using "####"; BRC!(15)
    Locate , 4: Print indRecords$(16); Tab(28); Left$(BRC$(16, 0), 14); Tab(42); BRC$(16, 1); Tab(62); Using "####"; BRC!(16)
    Locate , 4: Print indRecords$(17); Tab(28); Left$(BRC$(17, 0), 14); Tab(42); BRC$(17, 1); Tab(62); Using "####"; BRC!(17)
    Locate , 4: Print indRecords$(18); Tab(28); Left$(BRC$(18, 0), 14); Tab(42); BRC$(18, 1); Tab(62); Using "##.#"; BRC!(18)
    Locate , 4: Print indRecords$(19); Tab(28); Left$(BRC$(19, 0), 14); Tab(42); BRC$(19, 1); Tab(62); Using "####"; BRC!(19)
    Locate , 4: Print indRecords$(20); Tab(28); Left$(BRC$(20, 0), 14); Tab(42); BRC$(20, 1); Tab(62); Using "####"; BRC!(20)
    Locate , 4: Print indRecords$(21); Tab(28); Left$(BRC$(21, 0), 14); Tab(42); BRC$(21, 1); Tab(62); Using "####"; BRC!(21)
    Locate , 4: Print indRecords$(22); Tab(28); Left$(BRC$(22, 0), 14); Tab(42); BRC$(22, 1); Tab(62); Using "####"; BRC!(22)
    Locate , 4: Print indRecords$(23); Tab(28); Left$(BRC$(23, 0), 14); Tab(42); BRC$(23, 1); Tab(62); Using "####"; BRC!(23)
    Locate , 4: Print indRecords$(24); Tab(28); Left$(BRC$(24, 0), 14); Tab(42); BRC$(24, 1); Tab(62); Using "##.#"; BRC!(24)
    Locate , 4: Print indRecords$(25); Tab(28); Left$(BRC$(25, 0), 14); Tab(42); BRC$(25, 1); Tab(62); Using "####"; BRC!(25)

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    For I = 4 To 21: Locate I, 4
        Print Space$(72)
    Next

    Color 0, 0
    Cls

    J = 20

    Locate 1, 1

    Color 15, 1
    Print " " + Chr$(214); String$(73, 196); "� ";

    For II = 1 To J
        Color 15, 1
        Locate 1 + II, 1
        Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
        Color 7, 1: Print String$(2, 177)
    Next

    Color 15, 1
    Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
    Color 7, 1: Print String$(2, 177)
    Locate , 3: Print String$(77, 177)

    Color 15
    Locate 2, 23
    Print "*******  OFFENSIVE TEAM RECORDS *******"
    Print
    Locate , 4: Print "CATEGORY                        TEAM             RECORD            "

    Color 14
    'Field Goal Attempts
    Locate , 4: Print teamRecords$(1); Tab(33); TRC$(1); Tab(53); Using "####"; TRC!(1)
    'Field Goals Made
    Locate , 4: Print teamRecords$(2); Tab(33); TRC$(2); Tab(53); Using "####"; TRC!(2)
    'Field Goal %
    Locate , 4: Print teamRecords$(3); Tab(33); TRC$(3); Tab(53); Using "##.#"; TRC!(3) * 100
    '3 Field Goal Attempts
    Locate , 4: Print teamRecords$(4); Tab(33); TRC$(4); Tab(53); Using "####"; TRC!(4)
    '3 Field Goals Made
    Locate , 4: Print teamRecords$(5); Tab(33); TRC$(5); Tab(53); Using "####"; TRC!(5)
    '3 Field Goal %
    Locate , 4: Print teamRecords$(6); Tab(33); TRC$(6); Tab(53); Using "##.#"; TRC!(6) * 100
    'Free Throw Attempts
    Locate , 4: Print teamRecords$(7); Tab(33); TRC$(7); Tab(53); Using "####"; TRC!(7)
    'Free Throws Made
    Locate , 4: Print teamRecords$(8); Tab(33); TRC$(8); Tab(53); Using "####"; TRC!(8)
    'Free Throw %
    Locate , 4: Print teamRecords$(9); Tab(33); TRC$(9); Tab(53); Using "##.#"; TRC!(9) * 100

    Locate , 4: Print teamRecords$(10); Tab(33); TRC$(10); Tab(53); Using "####"; TRC!(10)
    Locate , 4: Print teamRecords$(11); Tab(33); TRC$(11); Tab(53); Using "####"; TRC!(11)
    Locate , 4: Print teamRecords$(12); Tab(33); TRC$(12); Tab(53); Using "####"; TRC!(12)
    Locate , 4: Print teamRecords$(13); Tab(33); TRC$(13); Tab(53); Using "##.#"; TRC!(13)
    Locate , 4: Print teamRecords$(14); Tab(33); TRC$(14); Tab(53); Using "####"; TRC!(14)
    Locate , 4: Print teamRecords$(15); Tab(33); TRC$(15); Tab(53); Using "####"; TRC!(15)

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    For I = 4 To 21
        Locate I, 4
        Print Space$(72)
    Next

    Color 0, 0
    Cls

    J = 20

    Locate 1, 1

    Color 15, 1
    Print " " + Chr$(214); String$(73, 196); "� ";

    For II = 1 To J
        Color 15, 1
        Locate 1 + II, 1
        Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
        Color 7, 1: Print String$(2, 177)
    Next

    Color 15, 1
    Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
    Color 7, 1: Print String$(2, 177)
    Locate , 3: Print String$(77, 177)

    Color 15
    Locate 2, 23: Print "*******  OFFENSIVE TEAM RECORDS *******"
    Print
    Locate , 4: Print "CATEGORY                        TEAM             RECORD            "

    Color 14
    Locate , 4: Print teamRecords$(16); Tab(33); TRC$(16); Tab(53); Using "####"; TRC!(16)
    Locate , 4: Print teamRecords$(17); Tab(33); TRC$(17); Tab(53); Using "####"; TRC!(17)
    Locate , 4: Print teamRecords$(18); Tab(33); TRC$(18); Tab(53); Using "####"; TRC!(18)
    Locate , 4: Print teamRecords$(19); Tab(33); TRC$(19); Tab(52); Using "#####"; TRC!(19)
    Locate , 4: Print teamRecords$(20); Tab(33); TRC$(20); Tab(52); Using "###.#"; TRC!(20)
    Locate , 4: Print "WINS"; Tab(33); TRC$(21); Tab(53); Using "####"; TRC!(21)
    Color 7

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    For I = 4 To 21
        Locate I, 4
        Print Space$(72)
    Next

    Color 0, 0
    Cls

    J = 20

    Locate 1, 1

    Color 15, 4
    Print " " + Chr$(214); String$(73, 196); "� ";

    For II = 1 To J
        Color 15, 4
        Locate 1 + II, 1
        Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
        Color 7, 4: Print String$(2, 177)
    Next

    Color 15, 4
    Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
    Color 7, 4: Print String$(2, 177)
    Locate , 3: Print String$(77, 177)

    Color 14
    Locate 2, 23
    Print "*******  DEFENSIVE TEAM RECORDS *******"
    Print
    Locate , 4: Print "CATEGORY                        TEAM             RECORD            "

    Color 15
    'Field Goal Attempts
    Locate , 4: Print teamRecords$(1); Tab(33); TRC1$(1); Tab(53); Using "####"; TRC1!(1)
    'Field Goals Made
    Locate , 4: Print teamRecords$(2); Tab(33); TRC1$(2); Tab(53); Using "####"; TRC1!(2)
    'Field Goal %
    Locate , 4: Print teamRecords$(3); Tab(33); TRC1$(3); Tab(53); Using "##.#"; TRC1!(3) * 100
    '3 Field Goal Attempts
    Locate , 4: Print teamRecords$(4); Tab(33); TRC1$(4); Tab(53); Using "####"; TRC1!(4)
    '3 Field Goals Made
    Locate , 4: Print teamRecords$(5); Tab(33); TRC1$(5); Tab(53); Using "####"; TRC1!(5)
    '3 Field Goal %
    Locate , 4: Print teamRecords$(6); Tab(33); TRC1$(6); Tab(53); Using "##.#"; TRC1!(6) * 100
    'Free Throw Attempts
    Locate , 4: Print teamRecords$(7); Tab(33); TRC1$(7); Tab(53); Using "####"; TRC1!(7)
    'Free Throws Made
    Locate , 4: Print teamRecords$(8); Tab(33); TRC1$(8); Tab(53); Using "####"; TRC1!(8)
    'Free Throw %
    Locate , 4: Print teamRecords$(9); Tab(33); TRC1$(9); Tab(53); Using "##.#"; TRC1!(9) * 100

    Locate , 4: Print teamRecords$(10); Tab(33); TRC1$(10); Tab(53); Using "####"; TRC1!(10)
    Locate , 4: Print teamRecords$(11); Tab(33); TRC1$(11); Tab(53); Using "####"; TRC1!(11)
    Locate , 4: Print teamRecords$(12); Tab(33); TRC1$(12); Tab(53); Using "####"; TRC1!(12)
    Locate , 4: Print teamRecords$(13); Tab(33); TRC1$(13); Tab(53); Using "##.#"; TRC1!(13)
    Locate , 4: Print teamRecords$(14); Tab(33); TRC1$(14); Tab(53); Using "####"; TRC1!(14)
    Locate , 4: Print teamRecords$(15); Tab(33); TRC1$(15); Tab(53); Using "####"; TRC1!(15)

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    For I = 4 To 21: Locate I, 4
        Print Space$(72)
    Next

    Color 0, 0
    Cls

    J = 20

    Locate 1, 1

    Color 15, 4
    Print " " + Chr$(214); String$(73, 196); "� ";

    For II = 1 To J
        Color 15, 4
        Locate 1 + II, 1
        Print " " + Chr$(186); String$(73, 32); Chr$(186) + " ";
        Color 7, 4: Print String$(2, 177)
    Next

    Color 15, 4
    Locate 2 + J, 1: Print " " + Chr$(211); String$(73, 196); Chr$(189) + " ";
    Color 7, 4: Print String$(2, 177)
    Locate , 3: Print String$(77, 177)

    Color 14
    Locate 2, 23: Print "*******  DEFENSIVE TEAM RECORDS *******"
    Print
    Locate , 4: Print "CATEGORY                        TEAM             RECORD            "

    Color 15
    Locate , 4: Print teamRecords$(16); Tab(33); TRC1$(16); Tab(53); Using "####"; TRC1!(16)
    Locate , 4: Print teamRecords$(17); Tab(33); TRC1$(17); Tab(53); Using "####"; TRC1!(17)
    Locate , 4: Print teamRecords$(18); Tab(33); TRC1$(18); Tab(53); Using "####"; TRC1!(18)
    Locate , 4: Print teamRecords$(19); Tab(33); TRC1$(19); Tab(52); Using "#####"; TRC1!(19)
    Locate , 4: Print teamRecords$(20); Tab(33); TRC1$(20); Tab(52); Using "###.#"; TRC1!(20)
    Locate , 4: Print "LOSSES"; Tab(33); TRC1$(21); Tab(53); Using "####"; TRC1!(21)
    Color 7

    Color 15
    Locate 22, 30: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    For I = 4 To 21: Locate I, 4
        Print Space$(72)
    Next

End Sub

'----------------------------------------
'     PrintSeasonRecords Subroutine
'----------------------------------------
'This subroutine assists the usere with
'viewing the records on the default printer
Sub PrintSeasonRecords ()

    Shared teamRecords$(), indRecords$()
    Shared BRC$(), TRC$(), TRC1$()
    Shared BRC!(), TRC!(), TRC1!()

    Color 0, 15

    LPrint Tab(23); "******* INDIVIDUAL PLAYER RECORDS *******"
    LPrint
    LPrint "CATEGORY                PLAYER           TEAM            RECORD"
    LPrint indRecords$(1); Tab(28); BRC$(1, 0); Tab(42); BRC$(1, 1); Tab(62); Using "####"; BRC!(1)
    LPrint indRecords$(2); Tab(28); BRC$(2, 0); Tab(42); BRC$(2, 1); Tab(62); Using "####"; BRC!(2)
    LPrint indRecords$(3); Tab(28); BRC$(3, 0); Tab(42); BRC$(3, 1); Tab(62); Using "####"; BRC!(3)
    LPrint indRecords$(4); Tab(28); BRC$(4, 0); Tab(42); BRC$(4, 1); Tab(62); Using "##.#"; BRC!(4) * 100
    LPrint indRecords$(5); Tab(28); BRC$(5, 0); Tab(42); BRC$(5, 1); Tab(62); Using "####"; BRC!(5)
    LPrint indRecords$(6); Tab(28); BRC$(6, 0); Tab(42); BRC$(6, 1); Tab(62); Using "####"; BRC!(6)
    LPrint indRecords$(7); Tab(28); BRC$(7, 0); Tab(42); BRC$(7, 1); Tab(62); Using "##.#"; BRC!(7) * 100
    LPrint indRecords$(8); Tab(28); BRC$(8, 0); Tab(42); BRC$(8, 1); Tab(62); Using "####"; BRC!(8)
    LPrint indRecords$(9); Tab(28); BRC$(9, 0); Tab(42); BRC$(9, 1); Tab(62); Using "####"; BRC!(9)
    LPrint indRecords$(10); Tab(28); BRC$(10, 0); Tab(42); BRC$(10, 1); Tab(62); Using "##.#"; BRC!(10) * 100
    LPrint indRecords$(11); Tab(28); BRC$(11, 0); Tab(42); BRC$(11, 1); Tab(62); Using "####"; BRC!(11)
    LPrint indRecords$(12); Tab(28); BRC$(12, 0); Tab(42); BRC$(12, 1); Tab(62); Using "####"; BRC!(12)
    LPrint indRecords$(13); Tab(28); BRC$(13, 0); Tab(42); BRC$(13, 1); Tab(62); Using "####"; BRC!(13)
    LPrint indRecords$(14); Tab(28); BRC$(14, 0); Tab(42); BRC$(14, 1); Tab(62); Using "####"; BRC!(14)
    LPrint indRecords$(15); Tab(28); BRC$(15, 0); Tab(42); BRC$(15, 1); Tab(62); Using "####"; BRC!(15)
    LPrint indRecords$(16); Tab(28); BRC$(16, 0); Tab(42); BRC$(16, 1); Tab(62); Using "####"; BRC!(16)
    LPrint indRecords$(17); Tab(28); BRC$(17, 0); Tab(42); BRC$(17, 1); Tab(62); Using "####"; BRC!(17)
    LPrint indRecords$(18); Tab(28); BRC$(18, 0); Tab(42); BRC$(18, 1); Tab(62); Using "##.#"; BRC!(18)
    LPrint indRecords$(19); Tab(28); BRC$(19, 0); Tab(42); BRC$(19, 1); Tab(62); Using "####"; BRC!(19)
    LPrint indRecords$(20); Tab(28); BRC$(20, 0); Tab(42); BRC$(20, 1); Tab(62); Using "####"; BRC!(20)
    LPrint indRecords$(21); Tab(28); BRC$(21, 0); Tab(42); BRC$(21, 1); Tab(62); Using "####"; BRC!(21)
    LPrint indRecords$(22); Tab(28); BRC$(22, 0); Tab(42); BRC$(22, 1); Tab(62); Using "####"; BRC!(22)
    LPrint indRecords$(23); Tab(28); BRC$(23, 0); Tab(42); BRC$(23, 1); Tab(62); Using "####"; BRC!(23)
    LPrint indRecords$(24); Tab(28); BRC$(24, 0); Tab(42); BRC$(24, 1); Tab(62); Using "##.#"; BRC!(24)
    LPrint indRecords$(25); Tab(28); BRC$(25, 0); Tab(42); BRC$(25, 1); Tab(62); Using "####"; BRC!(25)
    LPrint
    LPrint Tab(23); "*******  OFFENSIVE TEAM RECORDS *******"
    LPrint
    LPrint "CATEGORY                        TEAM             RECORD            "
    LPrint teamRecords$(1); Tab(33); TRC$(1); Tab(53); Using "####"; TRC!(1)
    LPrint teamRecords$(2); Tab(33); TRC$(2); Tab(53); Using "####"; TRC!(2)
    LPrint teamRecords$(3); Tab(33); TRC$(3); Tab(53); Using "##.#"; TRC!(3) * 100
    LPrint teamRecords$(4); Tab(33); TRC$(4); Tab(53); Using "####"; TRC!(4)
    LPrint teamRecords$(5); Tab(33); TRC$(5); Tab(53); Using "####"; TRC!(5)
    LPrint teamRecords$(6); Tab(33); TRC$(6); Tab(53); Using "##.#"; TRC!(6) * 100
    LPrint teamRecords$(7); Tab(33); TRC$(7); Tab(53); Using "####"; TRC!(7)
    LPrint teamRecords$(8); Tab(33); TRC$(8); Tab(53); Using "####"; TRC!(8)
    LPrint teamRecords$(9); Tab(33); TRC$(9); Tab(53); Using "##.#"; TRC!(9) * 100
    LPrint teamRecords$(10); Tab(33); TRC$(10); Tab(53); Using "####"; TRC!(10)
    LPrint teamRecords$(11); Tab(33); TRC$(11); Tab(53); Using "####"; TRC!(11)
    LPrint teamRecords$(12); Tab(33); TRC$(12); Tab(53); Using "####"; TRC!(12)
    LPrint teamRecords$(13); Tab(33); TRC$(13); Tab(53); Using "##.#"; TRC!(13)
    LPrint teamRecords$(14); Tab(33); TRC$(14); Tab(53); Using "####"; TRC!(14)
    LPrint teamRecords$(15); Tab(33); TRC$(15); Tab(53); Using "####"; TRC!(15)
    LPrint teamRecords$(16); Tab(33); TRC$(16); Tab(53); Using "####"; TRC!(16)
    LPrint teamRecords$(17); Tab(33); TRC$(17); Tab(53); Using "####"; TRC!(17)
    LPrint teamRecords$(18); Tab(33); TRC$(18); Tab(53); Using "####"; TRC!(18)
    LPrint teamRecords$(19); Tab(33); TRC$(19); Tab(52); Using "#####"; TRC!(19)
    LPrint teamRecords$(20); Tab(33); TRC$(20); Tab(52); Using "###.#"; TRC!(20)
    LPrint "WINS"; Tab(33); TRC$(21); Tab(53); Using "####"; TRC!(21)
    LPrint
    LPrint Tab(23); "*******  DEFENSIVE TEAM RECORDS *******"
    LPrint
    LPrint "CATEGORY                        TEAM             RECORD            "
    LPrint teamRecords$(1); Tab(33); TRC1$(1); Tab(53); Using "####"; TRC1!(1)
    LPrint teamRecords$(2); Tab(33); TRC1$(2); Tab(53); Using "####"; TRC1!(2)
    LPrint teamRecords$(3); Tab(33); TRC1$(3); Tab(53); Using "##.#"; TRC1!(3) * 100
    LPrint teamRecords$(4); Tab(33); TRC1$(4); Tab(53); Using "####"; TRC1!(4)
    LPrint teamRecords$(5); Tab(33); TRC1$(5); Tab(53); Using "####"; TRC1!(5)
    LPrint teamRecords$(6); Tab(33); TRC1$(6); Tab(53); Using "##.#"; TRC1!(6) * 100
    LPrint teamRecords$(7); Tab(33); TRC1$(7); Tab(53); Using "####"; TRC1!(7)
    LPrint teamRecords$(8); Tab(33); TRC1$(8); Tab(53); Using "####"; TRC1!(8)
    LPrint teamRecords$(9); Tab(33); TRC1$(9); Tab(53); Using "##.#"; TRC1!(9) * 100
    LPrint teamRecords$(10); Tab(33); TRC1$(10); Tab(53); Using "####"; TRC1!(10)
    LPrint teamRecords$(11); Tab(33); TRC1$(11); Tab(53); Using "####"; TRC1!(11)
    LPrint teamRecords$(12); Tab(33); TRC1$(12); Tab(53); Using "####"; TRC1!(12)
    LPrint teamRecords$(13); Tab(33); TRC1$(13); Tab(53); Using "##.#"; TRC1!(13)
    LPrint teamRecords$(14); Tab(33); TRC1$(14); Tab(53); Using "####"; TRC1!(14)
    LPrint teamRecords$(15); Tab(33); TRC1$(15); Tab(53); Using "####"; TRC1!(15)
    LPrint teamRecords$(16); Tab(33); TRC1$(16); Tab(53); Using "####"; TRC1!(16)
    LPrint teamRecords$(17); Tab(33); TRC1$(17); Tab(53); Using "####"; TRC1!(17)
    LPrint teamRecords$(18); Tab(33); TRC1$(18); Tab(53); Using "####"; TRC1!(18)
    LPrint teamRecords$(19); Tab(33); TRC1$(19); Tab(52); Using "#####"; TRC1!(19)
    LPrint teamRecords$(20); Tab(33); TRC1$(20); Tab(52); Using "###.#"; TRC1!(20)
    LPrint "LOSSES"; Tab(33); TRC1$(21); Tab(53); Using "####"; TRC1!(21)

    LPrint Chr$(12)

End Sub

