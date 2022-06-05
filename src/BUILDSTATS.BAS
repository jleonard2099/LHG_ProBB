'------------------------------
'     BUILDSTATS Subroutine
'------------------------------
' This creates an .LDS file for your league
Sub BUILDSTATS (statFile$)
    Dim buildA$(15), AL$(600)
    Dim TMA$(600), TT$(40, 15), TT1$(40, 15)

    'Dim A(15, 14)
    Dim AL(600, 14)
    Dim GM(40), GM1(40), TT(40, 15), TT1(40, 15)
    Dim W0(15), W1(15), Z(15), Z1(15)

    Dim W0L(600), W1L(600)
    ReDim GMA(600), TYP(600)

	Shared YY

    11 For KK = 1 To 40: YY = YY + 1
        If Z1$(KK) = "*" Then 60

        10 Print "CONVERTING STAT FILE OF "; Z1$(KK)
        If Not _FileExists(D$(1) + Z1$(KK)) Then 60

        Open D$(1) + Z1$(KK) For Input As #1
        Input #1, B$
        For I = 0 To 14
            Input #1, buildA$(I)
            For J = 0 To 13
                Input #1, A(I, J)
            Next
            Input #1, W0(I)
            Input #1, W1(I)
        Next

        'This currently causes an overflow
        'Are the stat files not getting created correctly??
        For L = 0 To 15: Input #1, Z(L): Next
        For L = 0 To 15: Input #1, Z1(L): Next
        Close #1

        Open D$(1) + Z1$(KK) + "S" For Input As #1
        Input #1, Y
        Close #1

        TS = TS + 1: For I = 0 To 14: TT(TS, I) = Z(I): TT$(TS, I) = B$: GM(TS) = Y: Next
        TTS = TTS + 1: For I = 0 To 14: TT1(TTS, I) = Z1(I): TT1$(TTS, I) = B$: GM1(TTS) = Y: Next
        For I = 0 To 14
            If buildA$(I) = "XXX" Then 35

            33 JB = JB + 1
            AL$(JB) = buildA$(I)
            TBN = TBN + 1
            TMA$(TBN) = B$
            GMA(JB) = Y
            K1 = 0
            For I1 = 0 To 13
                K1 = K1 + 1
                AL(JB, K1) = A(I, I1)
            35 Next I1
            W0L(JB) = W0(I): W1L(JB) = W1(I)
        Next I

    60 Next KK
    Print "SAVING LEADER FILES . . . . PLEASE WAIT"

    Open D$(1) + statFile$ + ".LDS" For Output As #1
    Write #1, JB, TS, TTS
    For I = 1 To 600: Write #1, AL$(I), TMA$(I), GMA(I): Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR
    For I1 = 1 To 14: Write #1, AL(I, I1): Next I1: Write #1, W0L(I): Write #1, W1L(I): Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Next I
    For I = 1 To 40: For I1 = 0 To 14: Write #1, TT(I, I1), TT$(I, I1): Next: Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Write #1, GM(I): Next
    For I = 1 To 40: For I1 = 0 To 14: Write #1, TT1(I, I1), TT1$(I, I1): Next: Color 14: Locate 18, 45: TMR = TMR - 1: Print Using "#####"; TMR: Write #1, GM1(I): Next
    Close #1
End Sub
