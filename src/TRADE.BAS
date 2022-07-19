Sub TRADE ()

    ReDim menuChoices$(1 To 2)

    Color 3, 0

    _Title "Basketball: The Pro Game - Trade Players"

    Do
    
        99 Call TITLE
        'Erase menuChoices$
        menuChoices$(1) = "TRADE PLAYERS           "
        menuChoices$(2) = "RETURN TO PRO GAME MENU "
        SELECTION% = 1
        Count% = 2
        Start% = 1
        EXITCODE% = 2
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 10, 49, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0
        If Not (EXITCODE% = 27) Then
            If SELECTION% = 1 Then Call TradePlayers
        End If
    
    Loop Until SELECTION% = 2 Or EXITCODE% = 27

End Sub



'----------------------------------------
'         TradePlayers Subroutine
'----------------------------------------
'... explain here ...
Sub TradePlayers ()
    Dim A1$(1), B1$(2, 14)
    Dim F$(1), NY$(1), YN$(1)
    Dim S(1), ZZtrade(2, 15), ZZ1trade(2, 15)

    Shared B1(), tradeW0(), tradeW1()
    Shared teamSettings%(), tradeB%(), tradeL%()
    Shared SA$(), tradeAA$(), tradeB$()

    Shared tradeT()

    Do
        J = 0
        Color 15, 0: Cls
        JJ = 7
        Locate 6, 8: Color 15, 4
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
        Locate , 12: Print "ENTER DISK ID FOR TEAM A: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 38: Input NY$(J)
        If NY$(J) = "" Then 
            Call SelectTeam_TRADE (J,NT, YN$())
        Else
            YN$(J) = NY$(J)
            Erase teamNames$

            NT = 0
            Open diskPaths$(0) + "PROBBTMS." + YN$(J) For Random As #1 Len = 1031
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
                    NT = NT + 1
                    teamNames$(NT) = Q$(0): teamIndex%(NT) = I1
                End If
            Next
            Close 1

            TS% = INT((NT - 1) / 80)

            Call SelectTeam_TRADE (J,NT, YN$())
        End If
        
        If BO% = 1 Then 
            BO% = 0
        Else
            Locate 2, 33
			Color teamSettings%(J, 33), teamSettings%(J, 34): Print tradeAA$(J, 0)
			Color 15, 4
            Locate , 33: Print "ANY CHANGE (YN)";
            I$ = GetKeyPress$
        End If
    Loop Until UCase$(I$) = "N"
    
    Color 15, 0
    S(0) = 0
    JJ = 1

    Locate 6, 8: Color teamSettings%(J, 33), teamSettings%(J, 34)
    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
    For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next
    Locate 7 + JJ, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
    U$ = "DOES THIS TEAM HAVE SAVED STATS (YN)"
    Locate , (80 - Len(U$)) / 2
    Print U$
    Color 15, 0
    I$ = GetKeyPress$
	
    If UCase$(I$) = "Y" Then 
        '2145 GoSub 100
        For XX = 4 To 11: If Mid$(SA$(J), XX, 1) = " " Or Mid$(SA$(J), XX, 1) = "/" Or Mid$(SA$(J), XX, 1) = "." Then Mid$(SA$(J), XX, 1) = "_"
        Next XX
        For XX = 12 To 14: If Mid$(SA$(J), XX, 2) = "ST" Then Mid$(SA$(J), 9, 1) = "_": Mid$(SA$(J), 10, 2) = "ST"
        Next XX

        QQ = 8
        For XX = 11 To 4 Step -1
            If Not (Mid$(SA$(J), XX, 1) = "_") Then 

                YR$ = Left$(SA$(J), 2)
                F$(J) = Mid$(SA$(J), 4, QQ) + "." + YR$

                If _FileExists(diskPaths$(1) + F$(J)) Then
                    Open diskPaths$(1) + F$(J) For Input As #1
                    Input #1, A1$(J)
                    For I = 0 To 14: Input #1, B1$(J, I)
                        For I1 = 0 To 13: Input #1, B1(J, I, I1): Next I1
                    Input #1, tradeW0(J, I): Input #1, tradeW1(J, I): Next I

                    For L = 0 To 15: Input #1, ZZtrade(J, L): Next L
                    For L = 0 To 15: Input #1, ZZ1trade(J, L): Next L
                    Close 1
                Else
                    S(J) = 1
                End If

            Else
                QQ = XX - 4
            End If

        Next XX
    Else
        S(0) = 1
    End If
    
    Do
        2150 J = 1
        Color 15, 0: Cls
        JJ = 7
        Locate 6, 8: Color 15, 4
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
        Locate , 12: Print "ENTER DISK ID FOR TEAM B: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 38: Input NY$(J)
        If NY$(J) = "" Then 
            Call SelectTeam_TRADE (J,NT, YN$())
        Else
            YN$(J) = NY$(J)
            Erase teamNames$
            NT = 0
            Open diskPaths$(0) + "PROBBTMS." + YN$(J) For Random As #1 Len = 1031
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
                    NT = NT + 1
                    teamNames$(NT) = Q$(0): teamIndex%(NT) = I1
                End If
            Next
            Close 1

            TS% = INT((NT - 1) / 80)

            Call SelectTeam_TRADE (J,NT, YN$())
        End If
        
        If BO% = 1 Then 
            BO% = 0
        Else
            Locate 2, 33
            Color teamSettings%(J, 33), teamSettings%(J, 34): Print tradeAA$(J, 0): Color 15, 4
            Locate , 33: Print "ANY CHANGE (YN)";
            I$ = GetKeyPress$
        End If
        
    Loop Until UCase$(I$) = "N"

    Color 15, 0
    S(1) = 0
    JJ = 1

    Locate 6, 8: Color teamSettings%(J, 33), teamSettings%(J, 34)
    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
    For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next
    Locate 7 + JJ, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
    U$ = "DOES THIS TEAM HAVE SAVED STATS (YN)"
    Locate , (80 - Len(U$)) / 2
    Print U$
    Color 15, 0
    I$ = GetKeyPress$

    If UCase$(I$) = "N" Then 
        S(1) = 1
    Else
        For XX = 4 To 11
			If Mid$(SA$(J), XX, 1) = " " Or Mid$(SA$(J), XX, 1) = "/" Or Mid$(SA$(J), XX, 1) = "." Then Mid$(SA$(J), XX, 1) = "_"
        Next XX
        For XX = 12 To 14
			If Mid$(SA$(J), XX, 2) = "ST" Then Mid$(SA$(J), 9, 1) = "_": Mid$(SA$(J), 10, 2) = "ST"
        Next XX

        QQ = 8
        For XX = 11 To 4 Step -1
            If Not (Mid$(SA$(J), XX, 1) = "_") Then 

                YR$ = Left$(SA$(J), 2)
                F$(J) = Mid$(SA$(J), 4, QQ) + "." + YR$

                If _FileExists(diskPaths$(1) + F$(J)) Then
                    Open diskPaths$(1) + F$(J) For Input As #1
                    Input #1, A1$(J)
                    For I = 0 To 14: Input #1, B1$(J, I)
                        For I1 = 0 To 13: Input #1, B1(J, I, I1): Next I1
                    Input #1, tradeW0(J, I): Input #1, tradeW1(J, I): Next I

                    For L = 0 To 15: Input #1, ZZtrade(J, L): Next L
                    For L = 0 To 15: Input #1, ZZ1trade(J, L): Next L
                    Close 1
                Else
                    S(J) = 1
                End If

            Else
                QQ = XX - 4
            End If

        Next XX
    End If
    
    'Ready to trade
    Do

        200 Cls
        Color 15: Print "T";: Color 3: Print "RADE PLAYERS": Print
        Color 15: Print "N";: Color 3: Print "O MORE TRADES"
        I$ = GetKeyPress$

        If UCase$(I$) <> "N" Then

            If S(0) <> 1 Or S(1) <> 1 Then
                Cls
                Print
                Print
                Color 14: Print "(T)";: Color 15: Print " TRADE PLAYERS' COMPILED STATS"
                Color 14: Print: Print "(Z)";: Color 15: Print " ZERO PLAYERS' COMPILED STATS"
                I$ = GetKeyPress$
                If UCase$(I$) = "T" Or UCase$(I$) = "Z" Then
                    If UCase$(I$) = "T" Then W1 = 1
                    If UCase$(I$) = "Z" Then W1 = 0
                End If
            End If
            
            Cls
            Locate 3, 1
            Color teamSettings%(0, 33), teamSettings%(0, 34): Print tradeAA$(0, 0);
            Locate , 20
            Color teamSettings%(1, 33), teamSettings%(1, 34): Print tradeAA$(1, 0)
            Color 3, 0

            For I = 0 To 14
                Locate I + 4, 1
                Print I; " "; tradeB$(0, I, 0); " "; tradeB$(0, I, 1);
                Locate , 20
                Print I; " "; tradeB$(1, I, 0); " "; tradeB$(1, I, 1)
            Next I

            Do
                
                Do
                    Locate 19, 1: Print tradeAA$(0, 0); " PLAYER: ";
                    Input I
				'is this accurate?
                Loop Until I >= 0 And I <= 14
                
                Color teamSettings%(0, 33), teamSettings%(0, 34): Print tradeB$(0, I, 0): Color 3, 0

                Do
                    Locate 21, 1: Print tradeAA$(1, 0); " PLAYER: ";
                    Input I1
				'is this accurate?
                Loop Until I1 >= 0 And I1 <= 14

                Color teamSettings%(1, 33), teamSettings%(1, 34): Print tradeB$(1, I1, 0): Color 3, 0
                Locate 23, 1: Print "ANY CHANGE (YN) ";
                I$ = GetKeyPress$

            Loop Until UCase$(I$) = "N"

            Call PrintTradeSummary
            
            Locate 23, 1: Print "HIT ANY KEY TO TRADE";
            I$ = GetKeyPress$

            For I2 = 0 To 1: Swap tradeB$(0, I, I2), tradeB$(1, I1, I2): Next
            For I2 = 0 To 19: Swap tradeB%(0, I, I2), tradeB%(1, I1, I2): Next I2
            Swap B1$(0, I), B1$(1, I1)
            
            If S(0) = 1 Or S(1) = 1 Or W1 = 0 Then 
                240 For I2 = 0 To 13: B1(0, I, I2) = 0: B1(1, I1, I2) = 0: Next
                tradeW0(1, I1) = 0: tradeW1(1, I1) = 0: tradeW0(0, I) = 0: tradeW1(0, I) = 0
            Else
                For I2 = 0 To 13: Swap B1(0, I, I2), B1(1, I1, I2): Next I2
                Swap tradeW0(0, I), tradeW0(1, I1)
                Swap tradeW1(0, I), tradeW1(1, I1)
            End If
            
            Call PrintTradeSummary
            
            Locate 23, 1: Print "HIT ANY KEY TO CONTINUE";
            I$ = GetKeyPress$       
            
        Else
			'No more trades
		
            For J = 0 To 1
                Open diskPaths$(0) + "PROBBTMS." + YN$(J) For Random As #1 Len = 1031
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
                LSet Q$(0) = tradeAA$(J, 0)
				LSet Q$(1) = tradeAA$(J, 1)
				For I = 0 To 6: 
					LSet Q$(I + 2) = MKI$(tradeL%(J, I))
				Next
                LSet Q$(9) = MKI$(999)
                For I = 0 To 34: LSet Q$(I + 10) = MKI$(teamSettings%(J, I)): Next
                For I = 0 To 14
                    LSet Q$(22 * I + 45) = tradeB$(J, I, 0)
                    LSet Q$(22 * I + 46) = tradeB$(J, I, 1)
                    For I1 = 0 To 19
                        LSet Q$(22 * I + 47 + I1) = MKI$(tradeB%(J, I, I1))
					Next
				Next
                LSet Q$(375) = tradeAA$(J, 2)
				LSet Q$(376) = tradeAA$(J, 3)
				LSet Q$(377) = tradeAA$(J, 4)
                Put #1, tradeT(J): Close 1
            Next J

            For J = 0 To 1
                If S(J) <> 1 Then
                    Cls
                    Open diskPaths$(1) + F$(J) For Output As #1
                    Print #1, A1$(J): For I = 0 To 14: Print #1, B1$(J, I): For I1 = 0 To 13: Print #1, B1(J, I, I1): Next I1: Print #1, tradeW0(J, I): Print #1, tradeW1(J, I): Next I
                    For L = 0 To 15: Print #1, ZZtrade(J, L): Next L
                    For L = 0 To 15: Print #1, ZZ1trade(J, L): Next L
                    Close 1
                End If
            Next J
        
		'Exit Sub
        End If
    
    Loop Until UCase$(I$) = "N"
    
End Sub


'----------------------------------------
'     SelectTeam_TRADE Subroutine
'----------------------------------------
'... explain here ...
Sub SelectTeam_TRADE (J, numberTeams, YN$())

    shared tradeT()
    Shared SA$(), tradeAA$(), tradeB$()
	Shared teamSettings%()
	Shared tradeB%(), tradeL%()

    SELECTION% = 1
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
    MAMenu teamNames$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

	If EXITCODE% = 27 Then BO% = 1: Exit Sub
		
    tradeT(J) = teamIndex%(SELECTION%)

    Open diskPaths$(0) + "PROBBTMS." + YN$(J) For Random As #1 Len = 1031

    Field #1, 15 As Q$(0), 15 As Q$(1)
    For J0 = 0 To 6
        Field #1, 30 + J0 * 2 As X$, 2 As Q$(J0 + 2)
    Next
    Field #1, 44 As X$, 2 As Q$(9)
    For J0 = 0 To 34
        Field #1, 46 + J0 * 2 As X$, 2 As Q$(J0 + 10)
    Next
    For J0 = 0 To 14
        Field #1, 116 + J0 * 57 As X$, 15 As Q$(J0 * 22 + 45), 2 As Q$(J0 * 22 + 46)
        For J1 = 0 To 19
            Field #1, 133 + J0 * 57 + J1 * 2 As X$, 2 As Q$(J0 * 22 + 47 + J1)
        Next
    Next

    Field #1, 971 As X$, 5 As Q$(375), 15 As Q$(376), 40 As Q$(377)

    Get #1, tradeT(J)

    tradeAA$(J, 0) = Q$(0): tradeAA$(J, 1) = Q$(1)

    For I = 0 To 6
		tradeL%(J, I) = CVI(Q$(I + 2))
	Next

    9002 If Right$(tradeAA$(J, 0), 1) = " " Then tradeAA$(J, 0) = Left$(tradeAA$(J, 0), Len(tradeAA$(J, 0)) - 1): GoTo 9002

    For I = 0 To 34
		teamSettings%(J, I) = CVI(Q$(I + 10))
	Next

    For I = 0 To 14
        tradeB$(J, I, 0) = Q$(22 * I + 45)

        9004 If Right$(tradeB$(J, I, 0), 1) = " " Then tradeB$(J, I, 0) = Left$(tradeB$(J, I, 0), Len(tradeB$(J, I, 0)) - 1): GoTo 9004
        tradeB$(J, I, 1) = Q$(22 * I + 46)

        9005 If Right$(tradeB$(J, I, 1), 1) = " " Then tradeB$(J, I, 1) = Left$(tradeB$(J, I, 1), Len(tradeB$(J, I, 1)) - 1): GoTo 9005

        For I1 = 0 To 19
            tradeB%(J, I, I1) = CVI(Q$(22 * I + 47 + I1))
        Next
    Next
    tradeAA$(J, 2) = Q$(375)
	tradeAA$(J, 3) = Q$(376)
	tradeAA$(J, 4) = Q$(377)
    Close 1

    SA$(J) = tradeAA$(J, 0)

    9006 If Right$(tradeAA$(J, 2), 1) = " " Then tradeAA$(J, 2) = Left$(tradeAA$(J, 2), Len(tradeAA$(J, 2)) - 1): GoTo 9006

    9007 If Right$(tradeAA$(J, 3), 1) = " " Then tradeAA$(J, 3) = Left$(tradeAA$(J, 3), Len(tradeAA$(J, 3)) - 1): GoTo 9007

    9008 If Right$(tradeAA$(J, 4), 1) = " " Then tradeAA$(J, 4) = Left$(tradeAA$(J, 4), Len(tradeAA$(J, 4)) - 1): GoTo 9008

End Sub



'----------------------------------------
'         PrintTradeSummary Subroutine
'----------------------------------------
'Prints a summary of stats and players
'that will be swapped as a result of
'the trade that is being proposed
Sub PrintTradeSummary ()

    Shared B1(), tradeW0(), tradeW1()
    Shared teamSettings%(), tradeB%()
    Shared tradeAA$(), tradeB$()

    Cls
    Locate 3, 1: Color teamSettings%(0, 33), teamSettings%(0, 34)
    Locate , 10: Print tradeAA$(0, 0);
    Locate , 50: Color teamSettings%(1, 33), teamSettings%(1, 34): Print tradeAA$(1, 0)
    Color 3, 0: Print
    Locate , 10: Print tradeB$(0, I, 0); " "; tradeB$(0, I, 1); Tab(50); tradeB$(1, I1, 0); " "; tradeB$(1, I1, 1)
    Locate , 5: Print Using "  ##   GAMES     ##"; tradeB%(0, I, 0); B1(0, I, 0)
    Locate , 5: Print Using "####  MINUTES  ####"; tradeB%(0, I, 1); B1(0, I, 10) / 60
    Locate , 5: Print Using "####    FGA    ####"; tradeB%(0, I, 2); B1(0, I, 1)
    Locate , 5: Print Using "####    FGM    ####"; tradeB%(0, I, 3); B1(0, I, 2)
    Locate , 5: Print Using "####    FTA    ####"; tradeB%(0, I, 4); B1(0, I, 3)
    Locate , 5: Print Using "####    FTM    ####"; tradeB%(0, I, 5); B1(0, I, 4)
    Locate , 5: Print Using "####  OFF REB  ####"; tradeB%(0, I, 6); B1(0, I, 12)
    Locate , 5: Print Using "####  TOT REB  ####"; tradeB%(0, I, 6) + tradeB%(0, I, 7); B1(0, I, 5)
    Locate , 5: Print Using "####   FOULS   ####"; tradeB%(0, I, 9); B1(0, I, 6)
    Locate , 5: Print Using "####  STEALS   ####"; tradeB%(0, I, 11); B1(0, I, 7)
    Locate , 5: Print Using "####  ASSISTS  ####"; tradeB%(0, I, 8); B1(0, I, 8)
    Locate , 5: Print Using "####  BLOCKS   ####"; tradeB%(0, I, 12); B1(0, I, 9)
    Locate , 5: Print Using "     TURNOVERS ####"; B1(0, I, 11)
    Locate , 5: Print Using "####   3FGA    ####"; tradeB%(0, I, 13); tradeW1(0, I)
    Locate , 5: Print Using "####   3FGM    ####"; tradeB%(0, I, 14); tradeW0(0, I)
    Locate , 5: Print Using "  ##  HI GAME    ##"; tradeB%(0, I, 15); B1(0, I, 13)
    Locate 6, 45: Print Using "  ##   GAMES     ##"; tradeB%(1, I1, 0); B1(1, I1, 0)
    Locate , 45: Print Using "####  MINUTES  ####"; tradeB%(1, I1, 1); B1(1, I1, 10) / 60
    Locate , 45: Print Using "####    FGA    ####"; tradeB%(1, I1, 2); B1(1, I1, 1)
    Locate , 45: Print Using "####    FGM    ####"; tradeB%(1, I1, 3); B1(1, I1, 2)
    Locate , 45: Print Using "####    FTA    ####"; tradeB%(1, I1, 4); B1(1, I1, 3)
    Locate , 45: Print Using "####    FTM    ####"; tradeB%(1, I1, 5); B1(1, I1, 4)
    Locate , 45: Print Using "####  OFF REB  ####"; tradeB%(1, I1, 6); B1(1, I1, 12)
    Locate , 45: Print Using "####  TOT REB  ####"; tradeB%(1, I1, 6) + tradeB%(1, I1, 7); B1(1, I1, 5)
    Locate , 45: Print Using "####   FOULS   ####"; tradeB%(1, I1, 9); B1(1, I1, 6)
    Locate , 45: Print Using "####  STEALS   ####"; tradeB%(1, I1, 11); B1(1, I1, 7)
    Locate , 45: Print Using "####  ASSISTS  ####"; tradeB%(1, I1, 8); B1(1, I1, 8)
    Locate , 45: Print Using "####  BLOCKS   ####"; tradeB%(1, I1, 12); B1(1, I1, 9)
    Locate , 45: Print Using "     TURNOVERS ####"; B1(1, I1, 11)
    Locate , 45: Print Using "####   3FGA    ####"; tradeB%(1, I1, 13); tradeW1(1, I1)
    Locate , 45: Print Using "####   3FGM    ####"; tradeB%(1, I1, 14); tradeW0(1, I1)
    Locate , 45: Print Using "  ##  HI GAME    ##"; tradeB%(1, I1, 15); B1(1, I1, 13)

End Sub
