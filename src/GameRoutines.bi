
DefInt A-Z
DECLARE SUB ADJFOUL ()
DECLARE SUB BADFT ()
DECLARE SUB BOGUS (X)
DECLARE SUB BOX2FILE ()
DECLARE SUB BOX2PRN ()
DECLARE SUB BOX2SPC (NM$)
DECLARE SUB CHANGESTRATEGY (I$)
DECLARE SUB COMPILESTATS ()
DECLARE SUB COMPUTERCOACH ()
DECLARE SUB COMPUTERSUBS ()
DECLARE SUB DELAY ()
DECLARE SUB DISABLEPLAYERS ()
DECLARE SUB DRAWBOX (I)
DECLARE SUB EVENTS (Count, SPECIAL)
DECLARE SUB FRAMEBOX ()
DECLARE SUB GETATEAM (I1, YN$)
DECLARE SUB MADEFT ()
DECLARE SUB MAKEPBP ()
DECLARE SUB MISSFT ()
DECLARE SUB MOREDELAY ()
DECLARE SUB NEWSCREEN ()
DECLARE SUB QUARTERTOTAL ()
DECLARE SUB RATINGTABLES ()
DECLARE SUB RECORDSCHECK ()
DECLARE SUB ROSTER ()
DECLARE SUB SAVESTATFILES ()
DECLARE SUB SCOUTREPORT ()
DECLARE SUB SELECTPBP ()
DECLARE SUB SETUPRATINGS ()
DECLARE SUB TEAMMENU (BO%, YN$)
DECLARE SUB THREEFT ()
DECLARE SUB TICKER ()
DECLARE SUB TICKERUPDATE ()
DECLARE SUB UPDATESCREEN ()
DECLARE SUB WINDEX ()


Dim Shared AD, AP, B, CT, D, F3, H, JJ, JY, N, NTMS
Dim Shared P, P7, P9, PO, QQ, QR, SC, T, T1, TE, TMT
Dim Shared X, X1, X8, U5, VG, VH

Dim Shared A(1, 14, 25) As Single, B1(1, 4), C1(1, 14)
Dim Shared D1(1), D2(1), D8(6, 6), F1(14), G4(14), G5(14)
Dim Shared P2(1), P4(5), P5(5), P7(1), QQ(1, 8, 14, 14), QR(1, 7, 14)
Dim Shared R3(1), RB(0 To 10), S(1, 14), S3(14), SC(1)
Dim Shared T(1, 34), T0(1), T1(1), TF(1)
Dim Shared V9(14), VG(9), VH(9), W0(1, 14), W1(1, 14), W2(1, 14), W3(1, 14)
Dim Shared X$(4), X7(1), Z5(1), Z6(1), ZF(1), ZZ(2, 14)


Dim Shared displayPause As Single, FTF As Single, GP As Single
Dim Shared M9(1) As Single, MPG As Single, REC(50, 2) As Single
Dim Shared TG As Single, Z(15) As Single, Z1(15) As Single, Z2(14, 14) As Single

Dim Shared A1$, B1$, C1$, D$, D1$, E1$, F1$, FF$, G1$, H1$, O$
Dim Shared P7$, PB$, PS$, TS$, U$, Y2$, Z3$

Dim Shared AA$(1, 14, 1), B$(3, 5), CM$(1), D$(5), FR$(1), FT$(1)
Dim Shared H$(100), HO$(100), LC$(2), O$(2), P$(1), PO$(1), P7$(1), PB$(1), PS$(4)
Dim Shared Q$(380), R$(14), RC$(50, 4), SC$(1), SX$(32, 1)
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
