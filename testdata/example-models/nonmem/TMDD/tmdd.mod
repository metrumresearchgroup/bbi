$PROB RUN# 1 FULL TMDD MODEL EXAMPLE
$INPUT C ID AMT TIME DV EVID CMT L2
;DV is in molar concentration (pM)
;AMT is in molar amount (pmoles)
$DATA TMDD.csv IGNORE=C

$SUBROUTINE ADVAN6 TRANS1 TOL=4

$MODEL
 COMP(Central,DEFDOSE,DEFOBS) 
 COMP(Target) 
 COMP(Complex) 
 COMP(Periph)

$PK
CL  = THETA(1)*EXP(ETA(1)) ;clearance (L/hr)
VC  = THETA(2)*EXP(ETA(2)) ;central volume for free drug (L)
;assume VC is also the same for free target and drug-target complex
Q   = THETA(3)  ;intercompartmental clearance (L/hr)
VP  = THETA(4)  ;peripheral volume for free drug (L)

K   = CL/VC   ;elimination rate constant for free drug (hr-1)
K12 = Q/VC    ;central->peripheral rate constant for free drug (hr-1)
K21 = Q/VP    ;peripheral->central rate constant for free drug (hr-1)

BASE = THETA(5)*EXP(ETA(3))  ;baseline concentration of free target (pM)
KINT = THETA(6)  ;elimination rate constant for drug-target complex (hr-1)
KDEG = THETA(7)  ;elimination rate constant for free target (hr-1)
KON  = THETA(8)  ;binding rate constant (pM-1 hr-1)
KOFF = THETA(9)  ;dissociation rate constant (hr-1)

KSYN = BASE*KDEG ;synthesis rate constant for free target (pM/hr)
A_0(2)  = BASE   ;initialization of free target compartment by conc (pM)

$DES
DADT(1) = -K*A(1)-KON*A(2)*A(1) + KOFF*A(3)*VC-K12*A(1)+K21*A(4)    ;Free drug AMOUNT (pmoles) central comp                                               
DADT(2) = KSYN - KDEG*A(2) - KON*A(2)*A(1)/VC + KOFF*A(3)  ;Free target CONCENTRATION (pM)
DADT(3) = KON*A(2)*A(1)/VC-(KOFF+KINT)*A(3)                ;Drug-Target complex CONCENTRATION (pM)
DADT(4) = K12*A(1)-K21*A(4)                                ;Free drug AMOUNT (pmoles) peripheral comp

$ERROR
CALLFL=0

FDRUG=A(1)/VC ;free drug concentration (pM)
FTARG=A(2)    ;free target concentration (pM)
FCOMP=A(3)    ;complex concentration (pM)

IF(CMT.EQ.1) IPRED = FDRUG
IF(CMT.EQ.2) IPRED = FTARG
IF(CMT.EQ.3) IPRED = FCOMP

IF(CMT.EQ.1) Y=IPRED*(1+ERR(1))
IF(CMT.EQ.2) Y=IPRED*(1+ERR(2))
IF(CMT.EQ.3) Y=IPRED*(1+ERR(3))

$THETA
(0, 0.2)    ; 1 CL (L/hr)
(0, 2.0)    ; 2 VC (L)
(0, 0.1)    ; 3 Q (L/hr)
(0, 2.0)    ; 4 VP (L)
(0, 60)     ; 5 BASE (pM)
(0, 1.5)     ; 6 KINT (hr-1)
(0, 0.07)    ; 7 KDEG (hr-1)     
0.15 FIX    ; 8 KON  (pM-1 hr-1) 
10.5 FIX   ; 9 KOFF (hr-1)

$OMEGA BLOCK (2)
0.06          ;CL BSV
0.01 0.06   ;VC BSV

$OMEGA 0.06   ;BASE BSV

$SIGMA BLOCK (3)
0.02   ;PROP FREE DRUG
0.006 0.02   ;PROP FREE TARGET
0.006 0.006 0.02   ;PROP DRUG-TARGET COMPLEX

;MSFI=../1.msf

$ESTIMATION METHOD=1 NOABORT MAXEVAL=9999 PRINT=10 MSFO=../1.msf
$COVARIANCE
$TABLE ID TIME EVID IPRED CWRES CMT NOPRINT ONEHEADER FILE=../1.tab
$TABLE ID CL VC Q VP K K12 K21 BASE KINT KDEG KON KOFF KSYN ETA1
ETA2 ETA3 NOPRINT ONEHEADER FILE=../1par.tab

;END







