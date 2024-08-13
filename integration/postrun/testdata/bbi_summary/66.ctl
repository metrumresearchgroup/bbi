; THIS IS A BOOTSTRAPPED RUN FROM THE BBR EXAMPLE PROJECT
; it is included because it had a PRDERR that cause the shrinkage file to NOT be created
$PROBLEM BS-66 106-104 + COV-effects(CRCL, AGE) on CL

$INPUT C ID NUM OID SUBJ TIME SEQ CMT EVID AMT DV AGE WT CRCL ALB BMI AAG
       SCR AST ALT HT CP TAFD TAD LDOS MDV BLQ PHASE

$DATA ../data/66.csv IGNORE=(C='C', BLQ=1)

$SUBROUTINE ADVAN4 TRANS4

$PK
 
;log transformed PK parms
 
V2WT = LOG(WT/70)
CLWT = LOG(WT/70)*0.75
CLCR = LOG(CRCL/90)*THETA(6)
CLAGE = LOG(AGE/35)*THETA(7)
V3WT = LOG(WT/70)
QWT  = LOG(WT/70)*0.75
CLALB = LOG(ALB/4.5)*THETA(8)


KA   = EXP(THETA(1)+ETA(1))
V2   = EXP(THETA(2)+V2WT+ETA(2))
CL   = EXP(THETA(3)+CLWT+CLCR+CLAGE+CLALB+ETA(3))
V3   = EXP(THETA(4)+V3WT)
Q    = EXP(THETA(5)+QWT) 

S2 = V2/1000 ; dose in mcg, conc in mcg/mL

$ERROR
IPRED = F 
Y=IPRED*(1+EPS(1))

$THETA  ; log values
(0.5)   ;  1 KA (1/hr) - 1.5
(3.5)   ;  2 V2 (L) - 60
(1)     ;  3 CL (L/hr) - 3.5
(4)     ;  4 V3 (L) - 70
(2)     ;  5 Q  (L/hr) - 4
(1)     ;  6 CLCR~CL ()
(1)     ;  7 AGE~CL ()
(0.5)   ;  8 ALB~CL ()

$OMEGA BLOCK(3)
0.2   ;ETA(KA)
0.01 0.2   ;ETA(V2)
0.01 0.01 0.2   ;ETA(CL)

$SIGMA
0.05     ; 1 pro error

$EST MAXEVAL=9999 METHOD=1 INTER SIGL=6 NSIG=3 PRINT=1