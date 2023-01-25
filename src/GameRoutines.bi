
DefInt A-Z

Declare Sub TeamSelect_PROHOOP (teamIndicator)
Declare Sub PlayStyle ()
Declare Sub PlayItSafe ()
Declare Sub CheckZeroTime ()
Declare Sub PlayingTime ()
Declare Sub SomeCalcs ()
Declare Sub CreditAssists ()
Declare Sub ReboundRoutine1 ()
Declare Sub ReboundRoutine2 ()
Declare Sub Choices ()
Declare Sub Substitutions ()
Declare Sub Fouls (skipRandom%)
Declare Sub LastShot ()
Declare Sub ZeroCheck ()
Declare Sub BoxToScreen ()
Declare Sub STATCONVERT (Q, QQ)
Declare Sub LOADSTATS ()
Declare Sub BOGUS (X)
Declare Sub BOX2FILE ()
Declare Sub BOX2PRN ()
Declare Sub BOX2SPC (NM$)
Declare Sub COMPILESTATS ()
Declare Sub DISABLEPLAYERS ()
Declare Sub DRAWBOX (I)
Declare Sub GETATEAM (I1, YN$)
Declare Sub NEWSCREEN ()
Declare Sub QUARTERTOTAL ()
Declare Sub RATINGTABLES ()
Declare Sub SAVESTATFILES ()
Declare Sub SCOUTREPORT ()
Declare Sub SETUPRATINGS ()
Declare Sub TICKERUPDATE ()
Declare Sub ADJFOUL ()
Declare Sub BADFT ()
Declare Sub CHANGESTRATEGY (I$)
Declare Sub COMPUTERCOACH ()
Declare Sub COMPUTERSUBS ()
Declare Sub DELAY ()
Declare Sub EVENTS (Count, SPECIAL)
Declare Sub FRAMEBOX ()
Declare Sub MADEFT ()
Declare Sub MAKEPBP ()
Declare Sub MISSFT ()
Declare Sub MOREDELAY ()
Declare Sub ROSTER ()
Declare Sub SELECTPBP ()
Declare Sub TEAMMENU (BO%, YN$)
Declare Sub THREEFT ()
Declare Sub TICKER ()
Declare Sub WINDEX ()
Declare Sub RECORDSCHECK ()
Declare Sub UPDATESCREEN ()

'-------------------------
' Variable declarations
'-------------------------
Dim Shared AD, AP, B, CT, D, F3, H, JJ, JY, N, NTMS
Dim Shared P, P7, P9, PO, QQ, QR, SC, T, T1, TE, TMT
Dim Shared X, X1, X8, U5, VG, VH

Dim Shared B1!(1, 4), C1(1, 14)
Dim Shared D1(1), D2(1), D8(6, 6), F1(14), G4(14), G5(14)
Dim Shared P2(1), P4(5), P5(5), P7(1), QQ(1, 8, 14, 14), QR(1, 7, 14)
Dim Shared R3(1), RB(0 To 10), S!(1, 14), S3(14), SC(1)
Dim Shared T(1, 34), T0(1), T1(1), TF(1)
Dim Shared V9(14), VG(9), VH(9), W0!(1, 14), W1!(1, 14), W2(1, 14), W3(1, 14)
Dim Shared X$(4), X7(1), Z5(1), Z6(1), ZF(1), ZZ(2, 14)

Dim Shared pbpDelay!

Dim Shared A!(1, 14, 25), M9!(1), REC!(50, 2), Z!(15), Z1!(15), Z2!(14, 14)

Dim Shared A1$, B1$, C1$, D$, D1$, E1$, F1$, FF$, G1$, H1$, O$
Dim Shared P7$, PB$, PS$, TS$, U$, Y2$, Z3$

Dim Shared AA$(1, 14, 1), ABB$(3), A1$(3), B$(3), C$(3), CM$(1), defenseStyles$(5), FR$(1), FT$(1)
Dim Shared H$(100), HO$(100), LC$(2), offenseStyles$(2), diskPaths$(1), PO$(1), P7$(1), PB$(1), PS$(4)
Dim Shared Q$(380), R$(14), RC$(50, 4), S$(3), SC$(1), SX$(32, 1)
Dim Shared TS$(1), U5$(3), teamNames$(500), Y$(1), YN$(3)

Dim Shared CF%, CM%, DK%, EG%, FB%, FR%, FT%, GF%, HH%, HT%
Dim Shared NB%, NL%, NM%, OX%, OY%, PB%, PT%, PZ%, RT%
Dim Shared TS%, TTS%, X4%, XY%

Dim Shared AP%(2), B%(1, 14, 19), CF%(1, 9), E%(13)
Dim Shared F%(1, 9), F5%(1, 4), F7%(1, 9), G9%(1), GF%(2, 9)
Dim Shared HT%(100), L%(1, 6), NG%(18)
Dim Shared N0%(2, 2, 4), OX%(2), OY%(2), O%(100), O0%(1)
Dim Shared PF%(1), PFA%(33), ST%(32), SX%(32, 1, 14)
Dim Shared TOA%(33), TOF%(1), teamIndex%(500), W%(1, 14, 1)

Dim Shared fileLength&, actualAttendance&
