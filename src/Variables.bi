' Long INT vs. Single
' 4 bytes either way
' https://wiki.qb64.dev/qb64wiki/index.php/Variable_Types

Dim menuChoices$(1 To 8)
Dim YY%
ReDim AL(600, 14)

'----------------------------------------
' Used across more than one source file
'----------------------------------------
Dim Shared A(15, 14)

Dim Shared diskPaths$(3), Q$(0 To 377)
Dim Shared teamNames$(MAX_TEAMS), Z1$(40)
Dim Shared teamIndex%(MAX_TEAMS)
Dim Shared DirCount%
Dim Shared scheduleT!(0 To 34)

'File size
Dim Shared Y As Long

Dim Shared BO%
Dim Shared A$, YN$, Z1$
Dim Shared TMR%, TMR1%

Dim Shared DT$, TM$


'----------------------------------------
' Used across BINPUT.BAS routines
'----------------------------------------
Dim inputB%(0 To 14, 0 To 19), inputT%(35), inputL%(0 To 6)
Dim inputB$(15, 1), inputT$(4)

'----------------------------------------
' Used across CAREER.BAS routines
'----------------------------------------
Dim ars!(15, 62, 15), W0S!(15, 62), W1S!(15, 62)
Dim AR$(62)

'----------------------------------------
' Used across COMPARE.BAS routines
'----------------------------------------
Dim compareA$(15), compareAA$(15, 1), compareB$(14)
Dim compareT$(0), comparePS$(15)

Dim compareB%(15, 19), compareL%(6), compareT%(34)
Dim compareW0!(15), compareW1!(15)
Dim compareZ!(0 To 15), compareZ1!(0 To 15)

'----------------------------------------
' Used across CREATE.BAS routines
'----------------------------------------
Dim intZ1%(40)

'----------------------------------------
' Used across DRAFT.BAS routines
'----------------------------------------
Dim draftB%(0 To 1, 0 To 14, 0 To 19), draftL%(0 To 1, 0 To 6)
Dim draftLG%(0 To 52, 0 To 5), draftLT%(0 To 14, 0 To 34)
Dim draftT%(0 To 1, 0 To 34)
Dim draftAA$(1, 4), draftB$(1, 14, 1), draftYears$(1 To 52), draftYN$(0 To 1)

'----------------------------------------
' Used across GPMIN.BAS routines
'----------------------------------------
Dim adjustB$(15, 1), adjustT$(0 To 4)
Dim adjustB%(0 To 14, 19), adjustL%(0 To 6), adjustT%(0 To 34)

'----------------------------------------
' Used across HD2HD.BAS routines
'----------------------------------------
Dim Shared QQ$(1)
'--> could be converted to Dim / Shared split
Dim hd2hdT(34)
Dim hd2hdAL(50), AW(50), HL(50), HW(50), R1(50), R2(50), R3(50), R4(50)
Dim TAL(40), TAW(40), THL(40), THW(40)
Dim TR1(40), TR2(40), TR3(40), TR4(40)

'----------------------------------------
' Used across LOOKY.BAS,
'   LEAGCMPL.BAS routines
'----------------------------------------
Dim categories$(0 To 25), H$(0 To 100), lookyA$(0 To 14), O$(0 To 100)
Dim values!(0 To 14, 0 To 24), W0!(0 To 14), W1!(0 To 14)
Dim O%(0 To 100)

'----------------------------------------
' Used across LEAGCMPL.BAS,
'   SEELEAG.BAS routines
'----------------------------------------
Dim DL!(40, 20), LD!(250, 2), S!(14, 26)
Dim O1!(40), O2!(40), OL!(40, 20)
Dim P!(40), Z!(260), Z1!(260)

Dim DL$(40), N$(14), Z2$(260), Z3$(260)
Dim H1$(40), OL$(40), leagT$(40)

Dim DT!(21), OT!(21)

'----------------------------------------
' Used across NEWLDR.BAS routines
'----------------------------------------
Dim TP$(0 To 31), TP1$(0 To 38), TYP!(600)
Dim TPP$(32), TMM$(32)
Dim printOrView%

'----------------------------------------
' Used across REC.BAS routines
'----------------------------------------
Dim teamRecords$(20), indRecords$(25)
Dim BRC$(25, 1), TRC$(21), TRC1$(21)
Dim BRC!(25), TRC!(21), TRC1!(21)

'----------------------------------------
' Used across RECORDS.BAS routines
'----------------------------------------
Dim RC$(50, 4), recordsTRC$(125, 3)
Dim allRecords!(50, 2), TotalRecords!(125, 2)
Dim HALF$(2), QTR$(4)

'----------------------------------------
' Used across SCHEDULE.BAS routines
'----------------------------------------
Dim BS%, NS%
Dim N$
Dim scheduleAP%(1), homeScore%(MAX_GAMES), visitingScore%(MAX_GAMES)

'NG = Number of Games; not sure about the 18
Dim NG%(MAX_GAMES, 18)

ReDim scheduleH$(0 to 20), scheduleV$(0 to 20)
Dim scheduleYN$(MAX_GAMES, 1)

'----------------------------------------
' Used across TRADE.BAS routines
'----------------------------------------
Dim B1!(2, 14, 14), tradeT!(1)
Dim tradeW0!(2, 14), tradeW1!(2, 14)
Dim tradeAA$(1, 6), tradeB$(0 To 1, 0 To 14, 0 To 1)
Dim SA$(1)
Dim teamSettings%(1, 34)
Dim tradeB%(0 To 1, 0 To 14, 0 To 19), tradeL%(0 To 1, 0 To 6)

'----------------------------------------
' Used across Game Routines
'----------------------------------------
