;Model Desc: Base population model w. covariance step
;Project Name: chapter5
;Project ID: NO PROJECT DESCRIPTION

$PROB RUN# 402 POPULATION DATA
$INPUT C ID TIME DV AMT
$DATA ../402.csv IGNORE=C
$SUBROUTINES  ADVAN3 TRANS4

$PK
  TVV1=THETA(1)
  V1=TVV1*EXP(ETA(1))

  TVCL=THETA(2)
  CL=TVCL*EXP(ETA(2))

  TVV2=THETA(3)
  V2=TVV2*EXP(ETA(3))

  TVQ=THETA(4)
  Q=TVQ*EXP(ETA(4))

  S1=V1
$ERROR
  Y=F*(1+EPS(1))

$THETA  
(0,9.8) ;[V1]
(0,3.7) ;[CL]
(0,8.6) ;[V2]
(0,31)  ;[Q]

$OMEGA
    0.02 ; [P]
    0.02 ; [P]
    0.02 ; [P]
    0.02 ; [P]

$SIGMA
    0.02 ; [P]

$ESTIMATION METHOD=1 MAXEVAL=9999 INTER PRINT=5 FORMAT=,1PE13.6
$COV 
$TABLE ID TIME DV IPRE=CIPRED AMT CL V1 Q V2 ETA1 ETA2 ETA3 ETA4 
          CWRES IRES=CIRES IWRE=CIWRES NPD NPDE ESAMPLE=1000          
          NOPRINT FILE=402.tab FORMAT=s1PE12.5
