      MODULE NMPRD4P
      USE SIZES, ONLY: DPSIZE
      USE NMPRD4,ONLY: VRBL
      IMPLICIT NONE
      SAVE
      REAL(KIND=DPSIZE), DIMENSION (:),POINTER ::COM
      REAL(KIND=DPSIZE), POINTER ::POP_CL,POP_V1,POP_Q,POP_V2
      REAL(KIND=DPSIZE), POINTER ::COV_CL_AGE,COV_V1_WT,RUV_PROP
      REAL(KIND=DPSIZE), POINTER ::RUV_ADD,COV_CL_CLCR,ETA_CL,ETA_V1
      REAL(KIND=DPSIZE), POINTER ::ETA_Q,ETA_V2,LOGTWT,LOGTAGE
      REAL(KIND=DPSIZE), POINTER ::LOGTCLCR,MU_1,CL,MU_2,V1,MU_3,Q
      REAL(KIND=DPSIZE), POINTER ::MU_4,V2,CENTRAL_DES,PERIPHERAL_DES
      REAL(KIND=DPSIZE), POINTER ::CC_DES,CENTRAL,PERIPHERAL,CC,IPRED
      REAL(KIND=DPSIZE), POINTER ::W,Y,IRES,IWRES,A000042,A000044
      REAL(KIND=DPSIZE), POINTER ::A000055,A000056,A000057,A000058
      REAL(KIND=DPSIZE), POINTER ::A000059,A000060,A000061,A000062
      REAL(KIND=DPSIZE), POINTER ::E000020,E000021,F000106,F000108
      REAL(KIND=DPSIZE), POINTER ::F000107,F000105,E000046,E000045
      REAL(KIND=DPSIZE), POINTER ::F000168,F000166,F000167,D000001
      REAL(KIND=DPSIZE), POINTER ::D000002,D000003,D000004,D000005
      REAL(KIND=DPSIZE), POINTER ::D000006,D000007,D000008,D000009
      REAL(KIND=DPSIZE), POINTER ::D000010,D000011,D000012,D000013
      REAL(KIND=DPSIZE), POINTER ::D000014,D000132,D000129,D000131
      REAL(KIND=DPSIZE), POINTER ::D000130,C000031,D000136,D000133
      REAL(KIND=DPSIZE), POINTER ::D000135,D000134,D000140,D000137
      REAL(KIND=DPSIZE), POINTER ::D000139,D000138,D000148,D000145
      REAL(KIND=DPSIZE), POINTER ::D000147,D000146
      CONTAINS
      SUBROUTINE ASSOCNMPRD4
      COM=>VRBL
      POP_CL=>COM(000001);POP_V1=>COM(000002);POP_Q=>COM(000003)
      POP_V2=>COM(000004);COV_CL_AGE=>COM(000005)
      COV_V1_WT=>COM(000006);RUV_PROP=>COM(000007)
      RUV_ADD=>COM(000008);COV_CL_CLCR=>COM(000009)
      ETA_CL=>COM(000010);ETA_V1=>COM(000011);ETA_Q=>COM(000012)
      ETA_V2=>COM(000013);LOGTWT=>COM(000014);LOGTAGE=>COM(000015)
      LOGTCLCR=>COM(000016);MU_1=>COM(000017);CL=>COM(000018)
      MU_2=>COM(000019);V1=>COM(000020);MU_3=>COM(000021)
      Q=>COM(000022);MU_4=>COM(000023);V2=>COM(000024)
      CENTRAL_DES=>COM(000025);PERIPHERAL_DES=>COM(000026)
      CC_DES=>COM(000027);CENTRAL=>COM(000028);PERIPHERAL=>COM(000029)
      CC=>COM(000030);IPRED=>COM(000031);W=>COM(000032);Y=>COM(000033)
      IRES=>COM(000034);IWRES=>COM(000035);A000042=>COM(000036)
      A000044=>COM(000037);A000055=>COM(000038);A000056=>COM(000039)
      A000057=>COM(000040);A000058=>COM(000041);A000059=>COM(000042)
      A000060=>COM(000043);A000061=>COM(000044);A000062=>COM(000045)
      E000020=>COM(000046);E000021=>COM(000047);F000106=>COM(000048)
      F000108=>COM(000049);F000107=>COM(000050);F000105=>COM(000051)
      E000046=>COM(000052);E000045=>COM(000053);F000168=>COM(000054)
      F000166=>COM(000055);F000167=>COM(000056);D000001=>COM(000057)
      D000002=>COM(000058);D000003=>COM(000059);D000004=>COM(000060)
      D000005=>COM(000061);D000006=>COM(000062);D000007=>COM(000063)
      D000008=>COM(000064);D000009=>COM(000065);D000010=>COM(000066)
      D000011=>COM(000067);D000012=>COM(000068);D000013=>COM(000069)
      D000014=>COM(000070);D000132=>COM(000071);D000129=>COM(000072)
      D000131=>COM(000073);D000130=>COM(000074);C000031=>COM(000075)
      D000136=>COM(000076);D000133=>COM(000077);D000135=>COM(000078)
      D000134=>COM(000079);D000140=>COM(000080);D000137=>COM(000081)
      D000139=>COM(000082);D000138=>COM(000083);D000148=>COM(000084)
      D000145=>COM(000085);D000147=>COM(000086);D000146=>COM(000087)
      END SUBROUTINE ASSOCNMPRD4
      END MODULE NMPRD4P
      SUBROUTINE MODEL (IDNO,NCM,NPAR,IR,IATT,LINK)                           
      USE PRMOD_CHAR, ONLY: NAME                                              
      USE SIZES,     ONLY: DPSIZE,ISIZE,SD
      USE PRDIMS,    ONLY: GPRD,HPRD,GERD,HERD,GPKD
      INTEGER(KIND=ISIZE) :: IDNO,NCM,NPAR,IR,IATT,LINK,I,J                   
      DIMENSION :: IATT(IR,*),LINK(IR,*)                                      
      SAVE
      INTEGER(KIND=ISIZE), DIMENSION (2,7) :: MOD
      CHARACTER(LEN=SD), DIMENSION(2) :: CMOD
      DATA (MOD(I,  1),I=  1,  2)/&
      1,1 /
      DATA (MOD(I,  2),I=  1,  2)/&
      1,1 /
      DATA (MOD(I,  3),I=  1,  2)/&
      1,1 /
      DATA (MOD(I,  4),I=  1,  2)/&
      1,0 /
      DATA (MOD(I,  5),I=  1,  2)/&
      1,0 /
      DATA (MOD(I,  6),I=  1,  2)/&
      0,0 /
      DATA (MOD(I,  7),I=  1,  2)/&
      0,0 /
      DATA (CMOD(I),I=  1,  2) &
      /'COMP1','COMP2'/
      FORALL (I=1:2) NAME(I)=CMOD(I)
      FORALL (I=1:2,J=1:7) IATT(I,J)=MOD(I,J)
      IDNO=9999                                                               
      NCM=  2
      NPAR=004
      RETURN
      END
      SUBROUTINE PK(ICALL,IDEF,THETA,IREV,EVTREC,NVNT,INDXS,IRGG,GG,NETAS)      
      USE NMPRD4P
      USE SIZES,     ONLY: DPSIZE,ISIZE
      USE PRDIMS,    ONLY: GPRD,HPRD,GERD,HERD,GPKD
      USE NMBAYES_REAL,    ONLY: PRIORINFO
      USE PRDATA, ONLY: MXSTEP=>MXSTP01
      USE NMPRD_REAL,ONLY: ETA,EPS                                            
      USE NMPRD_INT, ONLY: MSEC=>ISECDER,MFIRST=>IFRSTDER,COMACT,COMSAV,IFIRSTEM
      USE NMPRD_INT, ONLY: MDVRES,ETASXI,NPDE_MODE,NOFIRSTDERCODE
      USE NMPRD_REAL, ONLY: DV_LOQ,CDF_L,DV_LAQ,CDF_LA
      USE NMPRD_INT, ONLY: IQUIT
      USE PROCM_INT, ONLY: NEWIND=>PNEWIF                                       
      USE PROCM_INT, ONLY: A_0FLG
      USE PRMOD_REAL,ONLY: A_0,DA_0,D2A_0
      USE NMBAYES_REAL, ONLY: LDF                                             
      IMPLICIT REAL(KIND=DPSIZE) (A-Z)                                          
      REAL(KIND=DPSIZE) :: EVTREC                                               
      SAVE
      INTEGER(KIND=ISIZE) :: FIRSTEM
      INTEGER(KIND=ISIZE) :: ICALL,IDEF,IREV,NVNT,INDXS,IRGG,NETAS              
      DIMENSION :: IDEF(7,*),THETA(*),EVTREC(IREV,*),INDXS(*),GG(IRGG,GPKD+1,*) 
      FIRSTEM=IFIRSTEM
      IF (ICALL <= 1) THEN                                                      
      CALL ASSOCNMPRD4
      IDEF(   1,0001)=  -9
      IDEF(   1,0002)=  -1
      IDEF(   1,0003)=   1
      IDEF(   1,0004)=   0
      IDEF(   2,0003)=   0
      IDEF(   2,0004)=   0
      CALL GETETA(ETA)                                                          
      IF (IQUIT == 1) RETURN                                                    
      RETURN                                                                    
      ENDIF                                                                     
      IF (NEWIND /= 2) THEN
      IF (ICALL == 4) THEN
      CALL SIMETA(ETA)
      ELSE
      CALL GETETA(ETA)
      ENDIF
      IF (IQUIT == 1) RETURN
      ENDIF
 !  level            0
      AGE=EVTREC(NVNT,008)
      WT=EVTREC(NVNT,009)
      CLCR=EVTREC(NVNT,010)
      POP_CL=THETA(001) 
      POP_V1=THETA(002) 
      POP_Q=THETA(003) 
      POP_V2=THETA(004) 
      COV_CL_AGE=THETA(005) 
      COV_V1_WT=THETA(006) 
      RUV_PROP=THETA(007) 
      RUV_ADD=THETA(008) 
      COV_CL_CLCR=THETA(009) 
      ETA_CL=ETA(001) 
      ETA_V1=ETA(002) 
      ETA_Q=ETA(003) 
      ETA_V2=ETA(004) 
      B000001=WT/70.D0 
      B000003=DLOG(B000001) 
      LOGTWT=B000003 
      B000004=AGE/35.D0 
      B000006=DLOG(B000004) 
      LOGTAGE=B000006 
      B000007=CLCR/83.D0 
      B000009=DLOG(B000007) 
      LOGTCLCR=B000009 
      B000010=DLOG(POP_CL) 
      MU_1=B000010+COV_CL_AGE*LOGTAGE+COV_CL_CLCR*LOGTCLCR 
      B000011=MU_1+ETA(001) 
      B000012=DEXP(B000011) 
      CL=B000012 
      B000013=DLOG(POP_V1) 
      MU_2=B000013+COV_V1_WT*LOGTWT 
      B000014=MU_2+ETA(002) 
      B000015=DEXP(B000014) 
      V1=B000015 
      MU_3=DLOG(POP_Q) 
      B000016=MU_3+ETA(003) 
      B000017=DEXP(B000016) 
      Q=B000017 
      MU_4=DLOG(POP_V2) 
      B000018=MU_4+ETA(004) 
      B000019=DEXP(B000018) 
      V2=B000019 
      IF(A_0FLG == 1)THEN 
      A_0(1)=0.D0 
      ENDIF 
      IF(A_0FLG == 1)THEN 
      A_0(2)=0.D0 
      ENDIF 
      P000001=V1 
      P000002=Q 
      P000003=V2 
      P000004=CL 
      IF (FIRSTEM == 1) THEN
!                      A000042 = DERIVATIVE OF V1 W.R.T. ETA(002)
      A000042=B000015 
!                      A000055 = DERIVATIVE OF P000001 W.R.T. ETA(002)
      A000055=A000042 
!                      A000057 = DERIVATIVE OF P000002 W.R.T. ETA(003)
      A000057=B000017 
!                      A000059 = DERIVATIVE OF P000003 W.R.T. ETA(004)
      A000059=B000019 
!                      A000061 = DERIVATIVE OF P000004 W.R.T. ETA(001)
      A000061=B000012 
      GG(0001,1,1)=P000001
      GG(0001,0003,1)=A000055
      GG(0002,1,1)=P000002
      GG(0002,0004,1)=A000057
      GG(0003,1,1)=P000003
      GG(0003,0005,1)=A000059
      GG(0004,1,1)=P000004
      GG(0004,0002,1)=A000061
      ELSE
      GG(0001,1,1)=P000001
      GG(0002,1,1)=P000002
      GG(0003,1,1)=P000003
      GG(0004,1,1)=P000004
      ENDIF
      IF (MSEC == 1) THEN
!                      A000044 = DERIVATIVE OF A000042 W.R.T. ETA(002)
      A000044=B000015 
!                      A000056 = DERIVATIVE OF A000055 W.R.T. ETA(002)
      A000056=A000044 
!                      A000058 = DERIVATIVE OF A000057 W.R.T. ETA(003)
      A000058=B000017 
!                      A000060 = DERIVATIVE OF A000059 W.R.T. ETA(004)
      A000060=B000019 
!                      A000062 = DERIVATIVE OF A000061 W.R.T. ETA(001)
      A000062=B000012 
      GG(0001,0003,0003)=A000056
      GG(0002,0004,0004)=A000058
      GG(0003,0005,0005)=A000060
      GG(0004,0002,0002)=A000062
      ENDIF
      RETURN
      END
      SUBROUTINE ERROR (ICALL,IDEF,THETA,IREV,EVTREC,NVNT,INDXS,F,G,HH)       
      USE NMPRD4P
      USE SIZES,     ONLY: DPSIZE,ISIZE
      USE PRDIMS,    ONLY: GPRD,HPRD,GERD,HERD,GPKD
      USE NMPRD_REAL,ONLY: ETA,EPS                                            
      USE NMPRD_INT, ONLY: MSEC=>ISECDER,MFIRST=>IFRSTDER,IQUIT,IFIRSTEM
      USE NMPRD_INT, ONLY: MDVRES,ETASXI,NPDE_MODE,NOFIRSTDERCODE
      USE NMPRD_REAL, ONLY: DV_LOQ,CDF_L,DV_LAQ,CDF_LA
      USE NMPRD_INT, ONLY: NEWL2
      USE PROCM_INT, ONLY: NEWIND=>PNEWIF                                       
      USE PROCM_REAL,ONLY: TSTATE
      USE PROCM_REAL,ONLY: A=>AMNT,DAETA,D2AETA
      IMPLICIT REAL(KIND=DPSIZE) (A-Z)                                        
      REAL(KIND=DPSIZE) :: EVTREC                                             
      SAVE
      INTEGER(KIND=ISIZE) :: ICALL,IDEF,IREV,NVNT,INDXS                       
      DIMENSION :: IDEF(*),THETA(*),EVTREC(IREV,*),INDXS(*)                   
      REAL(KIND=DPSIZE) :: G(GERD,*),HH(HERD,*)                               
      INTEGER(KIND=ISIZE) :: FIRSTEM
      FIRSTEM=IFIRSTEM
      IF (ICALL <= 1) THEN                                                    
      CALL ASSOCNMPRD4
      IDEF(2)=-1
      IDEF(3)=001
      RETURN
      ENDIF
      IF (ICALL == 4) THEN
      IF (NEWL2 == 1) THEN
      CALL SIMEPS(EPS)
      IF (IQUIT == 1) RETURN
      ENDIF
      ENDIF
 !  level            0
      DV=EVTREC(NVNT,003)
      CENTRAL=A(1) 
      PERIPHERAL=A(2) 
      B000001=CENTRAL/V1 
      CC=B000001 
      IPRED=CC 
      B000006=RUV_ADD*RUV_ADD 
      B000007=RUV_PROP*RUV_PROP*IPRED*IPRED 
      B000008=B000006+B000007 
      B000009=DSQRT(B000008) 
      W=B000009 
      Y=IPRED+W*EPS(001) 
!                      C000031 = DERIVATIVE OF Y W.R.T. EPS(001)
      C000031=W 
      IRES=DV-IPRED 
      IWRES=IRES/W 
      IF (FIRSTEM == 1) THEN !1
      B000002=1.D0/V1 
!                      D000083 = DERIVATIVE OF B000001 W.R.T. ETA(001)
      D000083=B000002*DAETA(001,001) 
!                      D000084 = DERIVATIVE OF B000001 W.R.T. ETA(002)
      D000084=B000002*DAETA(001,002) 
!                      D000085 = DERIVATIVE OF B000001 W.R.T. ETA(003)
      D000085=B000002*DAETA(001,003) 
!                      D000086 = DERIVATIVE OF B000001 W.R.T. ETA(004)
      D000086=B000002*DAETA(001,004) 
      B000003=-CENTRAL/V1/V1 
!                      D000087 = DERIVATIVE OF B000001 W.R.T. ETA(002)
      D000087=B000003*A000042+D000084 
      B000010=RUV_PROP*RUV_PROP*IPRED 
!                      D000101 = DERIVATIVE OF B000007 W.R.T. ETA(002)
      D000101=B000010*D000087 
!                      D000102 = DERIVATIVE OF B000007 W.R.T. ETA(004)
      D000102=B000010*D000086 
!                      D000103 = DERIVATIVE OF B000007 W.R.T. ETA(003)
      D000103=B000010*D000085 
!                      D000104 = DERIVATIVE OF B000007 W.R.T. ETA(001)
      D000104=B000010*D000083 
      B000011=RUV_PROP*RUV_PROP*IPRED 
!                      D000105 = DERIVATIVE OF B000007 W.R.T. ETA(002)
      D000105=B000011*D000087+D000101 
!                      D000106 = DERIVATIVE OF B000007 W.R.T. ETA(004)
      D000106=B000011*D000086+D000102 
!                      D000107 = DERIVATIVE OF B000007 W.R.T. ETA(003)
      D000107=B000011*D000085+D000103 
!                      D000108 = DERIVATIVE OF B000007 W.R.T. ETA(001)
      D000108=B000011*D000083+D000104 
      B000015=.5D0/B000009 
!                      D000117 = DERIVATIVE OF B000009 W.R.T. ETA(002)
      D000117=B000015*D000105 
!                      D000118 = DERIVATIVE OF B000009 W.R.T. ETA(004)
      D000118=B000015*D000106 
!                      D000119 = DERIVATIVE OF B000009 W.R.T. ETA(003)
      D000119=B000015*D000107 
!                      D000120 = DERIVATIVE OF B000009 W.R.T. ETA(001)
      D000120=B000015*D000108 
!                      D000125 = DERIVATIVE OF Y W.R.T. ETA(002)
      D000125=D000087 
!                      D000126 = DERIVATIVE OF Y W.R.T. ETA(004)
      D000126=D000086 
!                      D000127 = DERIVATIVE OF Y W.R.T. ETA(003)
      D000127=D000085 
!                      D000128 = DERIVATIVE OF Y W.R.T. ETA(001)
      D000128=D000083 
!                      D000129 = DERIVATIVE OF Y W.R.T. ETA(002)
      D000129=EPS(001)*D000117+D000125 
!                      D000130 = DERIVATIVE OF Y W.R.T. ETA(004)
      D000130=EPS(001)*D000118+D000126 
!                      D000131 = DERIVATIVE OF Y W.R.T. ETA(003)
      D000131=EPS(001)*D000119+D000127 
!                      D000132 = DERIVATIVE OF Y W.R.T. ETA(001)
      D000132=EPS(001)*D000120+D000128 
!                      D000133 = DERIVATIVE OF C000031 W.R.T. ETA(002)
      D000133=D000117 
!                      D000134 = DERIVATIVE OF C000031 W.R.T. ETA(004)
      D000134=D000118 
!                      D000135 = DERIVATIVE OF C000031 W.R.T. ETA(003)
      D000135=D000119 
!                      D000136 = DERIVATIVE OF C000031 W.R.T. ETA(001)
      D000136=D000120 
!                      D000137 = DERIVATIVE OF IRES W.R.T. ETA(002)
      D000137=-D000087 
!                      D000138 = DERIVATIVE OF IRES W.R.T. ETA(004)
      D000138=-D000086 
!                      D000139 = DERIVATIVE OF IRES W.R.T. ETA(003)
      D000139=-D000085 
!                      D000140 = DERIVATIVE OF IRES W.R.T. ETA(001)
      D000140=-D000083 
      B000016=1.D0/W 
!                      D000141 = DERIVATIVE OF IWRES W.R.T. ETA(001)
      D000141=B000016*D000140 
!                      D000142 = DERIVATIVE OF IWRES W.R.T. ETA(003)
      D000142=B000016*D000139 
!                      D000143 = DERIVATIVE OF IWRES W.R.T. ETA(004)
      D000143=B000016*D000138 
!                      D000144 = DERIVATIVE OF IWRES W.R.T. ETA(002)
      D000144=B000016*D000137 
      B000017=-IRES/W/W 
!                      D000145 = DERIVATIVE OF IWRES W.R.T. ETA(002)
      D000145=B000017*D000117+D000144 
!                      D000146 = DERIVATIVE OF IWRES W.R.T. ETA(004)
      D000146=B000017*D000118+D000143 
!                      D000147 = DERIVATIVE OF IWRES W.R.T. ETA(003)
      D000147=B000017*D000119+D000142 
!                      D000148 = DERIVATIVE OF IWRES W.R.T. ETA(001)
      D000148=B000017*D000120+D000141 
      G(001,1)=D000132
      G(002,1)=D000129
      G(003,1)=D000131
      G(004,1)=D000130
      ENDIF !1
      HH(001,1)=C000031
      IF (FIRSTEM == 1) THEN !2
      HH(001,002)=D000136
      HH(001,003)=D000133
      HH(001,004)=D000135
      HH(001,005)=D000134
      ENDIF !2
      F=Y
      RETURN
      END
      SUBROUTINE TOL(NRD,ANRD,NRDC,ANRDC)
      USE SIZES,     ONLY: ISIZE
      INTEGER(KIND=ISIZE) :: NRD(0:*), ANRD(0:*), NRDC(0:*), ANRDC(0:*)
      NRD(1)=9 
      RETURN
      END
      SUBROUTINE DES (A,P,T,DADT,IR,DA,DP,DT)                                 
      USE NMPRD4P
      USE SIZES,     ONLY: DPSIZE,ISIZE
      USE PRDIMS,    ONLY: GPRD,HPRD,GERD,HERD,GPKD
      USE NMPRD_INT, ONLY: IERPRD,IERPRDU,NETEXT,IQUIT                        
      USE NMPRD_CHAR,ONLY: ETEXT                                              
      USE NMPRD_INT, ONLY: MSEC=>ISECDER,MFIRST=>IFRSTDER,IFIRSTEM,IFIRSTEMJAC
      USE PRCOM_INT, ONLY: MITER
      USE NMPRD_INT, ONLY: MDVRES,ETASXI,NPDE_MODE,NOFIRSTDERCODE
      USE NMPRD_REAL, ONLY: DV_LOQ,CDF_L,DV_LAQ,CDF_LA
      USE PRMOD_INT, ONLY: ICALL=>ICALLD,IDEFD,IDEFA
      IMPLICIT REAL(KIND=DPSIZE) (A-Z)                                        
      SAVE
      INTEGER(KIND=ISIZE) :: IR                                               
      DIMENSION :: A(*),P(*),DADT(*),DA(IR,*),DP(IR,*),DT(*)                  
      INTEGER(KIND=ISIZE) :: FIRSTEM,IFIRSTEMJACIN
      IF(MITER==1.OR.MITER==4) IFIRSTEM=1
      FIRSTEM=IFIRSTEM
      IFIRSTEMJACIN=IFIRSTEMJAC
      IF(NOFIRSTDERCODE/=1) THEN
      IFIRSTEMJAC=FIRSTEM
      ELSE
      IFIRSTEMJAC=0
      ENDIF
      IF(IFIRSTEMJACIN==-2) RETURN
      IF (ICALL == 1) THEN
      CALL ASSOCNMPRD4
      IDEFD(1)=  0
      IDEFD(2)=0
      DA(   1,1)=0000014280
      DA(   2,1)=0000014399
      DA(   3,1)=0000028441
      DA(   4,1)=0000028560
      DA(   5,1)=0000014311
      DA(   6,1)=0000028472
      DA(   7,1)=0000014432
      DA(   8,1)=0000028593
      DA(   9,1)=0000000000
      DP(   1,1)=0000014280
      DP(   2,1)=0000014399
      DP(   3,1)=0000014518
      DP(   4,1)=0000014637
      DP(   5,1)=0000028441
      DP(   6,1)=0000028560
      DP(   7,1)=0000028679
      DP(   8,1)=0000014281
      DP(   9,1)=0000014400
      DP(  10,1)=0000014638
      DP(  11,1)=0000028442
      DP(  12,1)=0000028561
      DP(  13,1)=0000014282
      DP(  14,1)=0000014520
      DP(  15,1)=0000028443
      DP(  16,1)=0000028681
      DP(  17,1)=0000014402
      DP(  18,1)=0000014521
      DP(  19,1)=0000028563
      DP(  20,1)=0000028682
      DP(  21,1)=0000014284
      DP(  22,1)=0000000000
      DT(   1)=0000000000
      RETURN
      ENDIF
 !  level            0
 !  level            0
      CENTRAL_DES=A(1) 
      PERIPHERAL_DES=A(2) 
      B000001=CENTRAL_DES/P(001) 
      CC_DES=B000001 
      B000007=-P(002)*CENTRAL_DES 
      B000008=P(002)*PERIPHERAL_DES 
      B000009=B000007/P(001) 
      B000010=B000008/P(003) 
      B000011=P(004)*CENTRAL_DES 
      B000012=B000009+B000010 
      B000013=B000011/P(001) 
      B000014=B000012-B000013 
      DADT(1)=B000014 
      B000042=P(002)*CENTRAL_DES 
      B000043=P(002)*PERIPHERAL_DES 
      B000044=B000042/P(001) 
      B000045=B000043/P(003) 
      B000046=B000044-B000045 
      DADT(2)=B000046 
      IF (FIRSTEM == 1) THEN ! 1
      B000002=1.D0/P(001) 
      B000003=-CENTRAL_DES/P(001)/P(001) 
!                      E000009 = DERIVATIVE OF B000007 W.R.T. A(001)
      E000009=-P(002) 
      B000015=1.D0/P(001) 
!                      E000011 = DERIVATIVE OF B000009 W.R.T. A(001)
      E000011=B000015*E000009 
      B000016=1.D0/P(003) 
!                      E000012 = DERIVATIVE OF B000010 W.R.T. A(002)
      E000012=B000016*P(002) 
      B000017=1.D0/P(001) 
!                      E000016 = DERIVATIVE OF B000013 W.R.T. A(001)
      E000016=B000017*P(004) 
!                      E000019 = DERIVATIVE OF B000014 W.R.T. A(001)
      E000019=-E000016+E000011 
!                      E000020 = DERIVATIVE OF DADT(1) W.R.T. A(001)
      E000020=E000019 
!                      E000021 = DERIVATIVE OF DADT(1) W.R.T. A(002)
      E000021=E000012 
!                      F000087 = DERIVATIVE OF B000007 W.R.T. P(002)
      F000087=-CENTRAL_DES 
      B000020=1.D0/P(001) 
!                      F000089 = DERIVATIVE OF B000009 W.R.T. P(002)
      F000089=B000020*F000087 
      B000021=-B000007/P(001)/P(001) 
      B000022=1.D0/P(003) 
!                      F000091 = DERIVATIVE OF B000010 W.R.T. P(002)
      F000091=B000022*PERIPHERAL_DES 
      B000023=-B000008/P(003)/P(003) 
!                      F000097 = DERIVATIVE OF B000012 W.R.T. P(002)
      F000097=F000091+F000089 
      B000025=1.D0/P(001) 
!                      F000098 = DERIVATIVE OF B000013 W.R.T. P(004)
      F000098=B000025*CENTRAL_DES 
      B000026=-B000011/P(001)/P(001) 
!                      F000103 = DERIVATIVE OF B000014 W.R.T. P(001)
      F000103=-B000026+B000021 
!                      F000104 = DERIVATIVE OF B000014 W.R.T. P(004)
      F000104=-F000098 
!                      F000105 = DERIVATIVE OF DADT(1) W.R.T. P(004)
      F000105=F000104 
!                      F000106 = DERIVATIVE OF DADT(1) W.R.T. P(001)
      F000106=F000103 
!                      F000107 = DERIVATIVE OF DADT(1) W.R.T. P(003)
      F000107=B000023 
!                      F000108 = DERIVATIVE OF DADT(1) W.R.T. P(002)
      F000108=F000097 
      B000047=1.D0/P(001) 
!                      E000041 = DERIVATIVE OF B000044 W.R.T. A(001)
      E000041=B000047*P(002) 
      B000048=1.D0/P(003) 
!                      E000042 = DERIVATIVE OF B000045 W.R.T. A(002)
      E000042=B000048*P(002) 
!                      E000044 = DERIVATIVE OF B000046 W.R.T. A(002)
      E000044=-E000042 
!                      E000045 = DERIVATIVE OF DADT(2) W.R.T. A(002)
      E000045=E000044 
!                      E000046 = DERIVATIVE OF DADT(2) W.R.T. A(001)
      E000046=E000041 
      B000051=1.D0/P(001) 
!                      F000158 = DERIVATIVE OF B000044 W.R.T. P(002)
      F000158=B000051*CENTRAL_DES 
      B000052=-B000042/P(001)/P(001) 
      B000053=1.D0/P(003) 
!                      F000160 = DERIVATIVE OF B000045 W.R.T. P(002)
      F000160=B000053*PERIPHERAL_DES 
      B000054=-B000043/P(003)/P(003) 
!                      F000164 = DERIVATIVE OF B000046 W.R.T. P(003)
      F000164=-B000054 
!                      F000165 = DERIVATIVE OF B000046 W.R.T. P(002)
      F000165=-F000160+F000158 
!                      F000166 = DERIVATIVE OF DADT(2) W.R.T. P(002)
      F000166=F000165 
!                      F000167 = DERIVATIVE OF DADT(2) W.R.T. P(003)
      F000167=F000164 
!                      F000168 = DERIVATIVE OF DADT(2) W.R.T. P(001)
      F000168=B000052 
      ENDIF !1
      IF (MSEC == 1) THEN 
      B000005=CENTRAL_DES/P(001)/P(001)/P(001) 
      B000006=CENTRAL_DES/P(001)/P(001)/P(001) 
!                      F000084 = DERIVATIVE OF B000003 W.R.T. P(001)
      F000084=B000006+B000005 
!                      F000085 = DERIVATIVE OF F000081 W.R.T. P(001)
      F000085=F000084 
!                      F000086 = DERIVATIVE OF F000082 W.R.T. P(001)
      F000086=F000085 
      B000027=-1.D0/P(001)/P(001) 
!                      F000110 = DERIVATIVE OF F000089 W.R.T. P(001)
      F000110=F000087*B000027 
      B000028=-1.D0/P(001)/P(001) 
!                      F000111 = DERIVATIVE OF B000021 W.R.T. P(002)
      F000111=B000028*F000087 
      B000029=B000007/P(001)/P(001)/P(001) 
      B000030=B000007/P(001)/P(001)/P(001) 
!                      F000113 = DERIVATIVE OF B000021 W.R.T. P(001)
      F000113=B000030+B000029 
!                      F000114 = DERIVATIVE OF F000090 W.R.T. P(001)
      F000114=F000113 
!                      F000115 = DERIVATIVE OF F000090 W.R.T. P(002)
      F000115=F000111 
      B000031=-1.D0/P(003)/P(003) 
!                      F000117 = DERIVATIVE OF F000091 W.R.T. P(003)
      F000117=PERIPHERAL_DES*B000031 
      B000032=-1.D0/P(003)/P(003) 
!                      F000118 = DERIVATIVE OF B000023 W.R.T. P(002)
      F000118=B000032*PERIPHERAL_DES 
      B000033=B000008/P(003)/P(003)/P(003) 
      B000034=B000008/P(003)/P(003)/P(003) 
!                      F000120 = DERIVATIVE OF B000023 W.R.T. P(003)
      F000120=B000034+B000033 
!                      F000121 = DERIVATIVE OF F000092 W.R.T. P(003)
      F000121=F000120 
!                      F000122 = DERIVATIVE OF F000092 W.R.T. P(002)
      F000122=F000118 
!                      F000123 = DERIVATIVE OF F000094 W.R.T. P(002)
      F000123=F000115 
!                      F000124 = DERIVATIVE OF F000094 W.R.T. P(001)
      F000124=F000114 
!                      F000125 = DERIVATIVE OF F000095 W.R.T. P(001)
      F000125=F000110 
!                      F000126 = DERIVATIVE OF F000096 W.R.T. P(002)
      F000126=F000122 
!                      F000127 = DERIVATIVE OF F000096 W.R.T. P(003)
      F000127=F000121 
!                      F000128 = DERIVATIVE OF F000097 W.R.T. P(003)
      F000128=F000117 
!                      F000129 = DERIVATIVE OF F000097 W.R.T. P(001)
      F000129=F000125 
      B000035=-1.D0/P(001)/P(001) 
!                      F000131 = DERIVATIVE OF F000098 W.R.T. P(001)
      F000131=CENTRAL_DES*B000035 
      B000036=-1.D0/P(001)/P(001) 
!                      F000132 = DERIVATIVE OF B000026 W.R.T. P(004)
      F000132=B000036*CENTRAL_DES 
      B000037=B000011/P(001)/P(001)/P(001) 
      B000038=B000011/P(001)/P(001)/P(001) 
!                      F000134 = DERIVATIVE OF B000026 W.R.T. P(001)
      F000134=B000038+B000037 
!                      F000135 = DERIVATIVE OF F000099 W.R.T. P(001)
      F000135=F000134 
!                      F000136 = DERIVATIVE OF F000099 W.R.T. P(004)
      F000136=F000132 
!                      F000137 = DERIVATIVE OF F000100 W.R.T. P(001)
      F000137=F000129 
!                      F000138 = DERIVATIVE OF F000100 W.R.T. P(003)
      F000138=F000128 
!                      F000139 = DERIVATIVE OF F000101 W.R.T. P(003)
      F000139=F000127 
!                      F000140 = DERIVATIVE OF F000101 W.R.T. P(002)
      F000140=F000126 
!                      F000141 = DERIVATIVE OF F000102 W.R.T. P(001)
      F000141=F000124 
!                      F000142 = DERIVATIVE OF F000102 W.R.T. P(002)
      F000142=F000123 
!                      F000143 = DERIVATIVE OF F000103 W.R.T. P(004)
      F000143=-F000136 
!                      F000144 = DERIVATIVE OF F000103 W.R.T. P(001)
      F000144=-F000135 
!                      F000145 = DERIVATIVE OF F000103 W.R.T. P(002)
      F000145=F000142 
!                      F000146 = DERIVATIVE OF F000103 W.R.T. P(001)
      F000146=F000141+F000144 
!                      F000147 = DERIVATIVE OF F000104 W.R.T. P(001)
      F000147=-F000131 
!                      F000148 = DERIVATIVE OF F000105 W.R.T. P(001)
      F000148=F000147 
!                      F000149 = DERIVATIVE OF F000106 W.R.T. P(001)
      F000149=F000146 
!                      F000150 = DERIVATIVE OF F000106 W.R.T. P(002)
      F000150=F000145 
!                      F000151 = DERIVATIVE OF F000106 W.R.T. P(004)
      F000151=F000143 
!                      F000152 = DERIVATIVE OF F000107 W.R.T. P(002)
      F000152=F000140 
!                      F000153 = DERIVATIVE OF F000107 W.R.T. P(003)
      F000153=F000139 
!                      F000154 = DERIVATIVE OF F000108 W.R.T. P(003)
      F000154=F000138 
!                      F000155 = DERIVATIVE OF F000108 W.R.T. P(001)
      F000155=F000137 
      B000039=-1.D0/P(001)/P(001) 
!                      E000024 = DERIVATIVE OF B000021 W.R.T. A(001)
      E000024=B000039*E000009 
!                      E000025 = DERIVATIVE OF F000090 W.R.T. A(001)
      E000025=E000024 
      B000040=-1.D0/P(003)/P(003) 
!                      E000026 = DERIVATIVE OF B000023 W.R.T. A(002)
      E000026=B000040*P(002) 
!                      E000027 = DERIVATIVE OF F000092 W.R.T. A(002)
      E000027=E000026 
!                      E000029 = DERIVATIVE OF F000094 W.R.T. A(001)
      E000029=E000025 
!                      E000030 = DERIVATIVE OF F000096 W.R.T. A(002)
      E000030=E000027 
      B000041=-1.D0/P(001)/P(001) 
!                      E000031 = DERIVATIVE OF B000026 W.R.T. A(001)
      E000031=B000041*P(004) 
!                      E000032 = DERIVATIVE OF F000099 W.R.T. A(001)
      E000032=E000031 
!                      E000033 = DERIVATIVE OF F000101 W.R.T. A(002)
      E000033=E000030 
!                      E000034 = DERIVATIVE OF F000102 W.R.T. A(001)
      E000034=E000029 
!                      E000035 = DERIVATIVE OF F000103 W.R.T. A(001)
      E000035=-E000032 
!                      E000036 = DERIVATIVE OF F000103 W.R.T. A(001)
      E000036=E000034+E000035 
!                      E000037 = DERIVATIVE OF F000106 W.R.T. A(001)
      E000037=E000036 
!                      E000038 = DERIVATIVE OF F000107 W.R.T. A(002)
      E000038=E000033 
      B000055=-1.D0/P(001)/P(001) 
!                      F000170 = DERIVATIVE OF F000158 W.R.T. P(001)
      F000170=CENTRAL_DES*B000055 
      B000056=-1.D0/P(001)/P(001) 
!                      F000171 = DERIVATIVE OF B000052 W.R.T. P(002)
      F000171=B000056*CENTRAL_DES 
      B000057=B000042/P(001)/P(001)/P(001) 
      B000058=B000042/P(001)/P(001)/P(001) 
!                      F000173 = DERIVATIVE OF B000052 W.R.T. P(001)
      F000173=B000058+B000057 
!                      F000174 = DERIVATIVE OF F000159 W.R.T. P(001)
      F000174=F000173 
!                      F000175 = DERIVATIVE OF F000159 W.R.T. P(002)
      F000175=F000171 
      B000059=-1.D0/P(003)/P(003) 
!                      F000177 = DERIVATIVE OF F000160 W.R.T. P(003)
      F000177=PERIPHERAL_DES*B000059 
      B000060=-1.D0/P(003)/P(003) 
!                      F000178 = DERIVATIVE OF B000054 W.R.T. P(002)
      F000178=B000060*PERIPHERAL_DES 
      B000061=B000043/P(003)/P(003)/P(003) 
      B000062=B000043/P(003)/P(003)/P(003) 
!                      F000180 = DERIVATIVE OF B000054 W.R.T. P(003)
      F000180=B000062+B000061 
!                      F000181 = DERIVATIVE OF F000161 W.R.T. P(003)
      F000181=F000180 
!                      F000182 = DERIVATIVE OF F000161 W.R.T. P(002)
      F000182=F000178 
!                      F000183 = DERIVATIVE OF F000162 W.R.T. P(002)
      F000183=F000175 
!                      F000184 = DERIVATIVE OF F000162 W.R.T. P(001)
      F000184=F000174 
!                      F000185 = DERIVATIVE OF F000163 W.R.T. P(001)
      F000185=F000170 
!                      F000186 = DERIVATIVE OF F000164 W.R.T. P(002)
      F000186=-F000182 
!                      F000187 = DERIVATIVE OF F000164 W.R.T. P(003)
      F000187=-F000181 
!                      F000188 = DERIVATIVE OF F000165 W.R.T. P(003)
      F000188=-F000177 
!                      F000189 = DERIVATIVE OF F000165 W.R.T. P(001)
      F000189=F000185 
!                      F000190 = DERIVATIVE OF F000166 W.R.T. P(001)
      F000190=F000189 
!                      F000191 = DERIVATIVE OF F000166 W.R.T. P(003)
      F000191=F000188 
!                      F000192 = DERIVATIVE OF F000167 W.R.T. P(003)
      F000192=F000187 
!                      F000193 = DERIVATIVE OF F000167 W.R.T. P(002)
      F000193=F000186 
!                      F000194 = DERIVATIVE OF F000168 W.R.T. P(001)
      F000194=F000184 
!                      F000195 = DERIVATIVE OF F000168 W.R.T. P(002)
      F000195=F000183 
      B000063=-1.D0/P(001)/P(001) 
!                      E000049 = DERIVATIVE OF B000052 W.R.T. A(001)
      E000049=B000063*P(002) 
!                      E000050 = DERIVATIVE OF F000159 W.R.T. A(001)
      E000050=E000049 
      B000064=-1.D0/P(003)/P(003) 
!                      E000051 = DERIVATIVE OF B000054 W.R.T. A(002)
      E000051=B000064*P(002) 
!                      E000052 = DERIVATIVE OF F000161 W.R.T. A(002)
      E000052=E000051 
!                      E000053 = DERIVATIVE OF F000162 W.R.T. A(001)
      E000053=E000050 
!                      E000054 = DERIVATIVE OF F000164 W.R.T. A(002)
      E000054=-E000052 
!                      E000055 = DERIVATIVE OF F000167 W.R.T. A(002)
      E000055=E000054 
!                      E000056 = DERIVATIVE OF F000168 W.R.T. A(001)
      E000056=E000053 
      ENDIF !msec
      IF (FIRSTEM == 1) THEN !2
      DA(   1,1)=E000020
      DA(   2,1)=E000021
      DA(   3,1)=E000046
      DA(   4,1)=E000045
      DP(   1,1)=F000106
      DP(   2,1)=F000108
      DP(   3,1)=F000107
      DP(   4,1)=F000105
      DP(   5,1)=F000168
      DP(   6,1)=F000166
      DP(   7,1)=F000167
      ENDIF !2
      IF (MSEC == 1) THEN
      DA(   5,1)=E000037
      DA(   6,1)=E000056
      DA(   7,1)=E000038
      DA(   8,1)=E000055
      DP(   8,1)=F000149
      DP(   9,1)=F000155
      DP(  10,1)=F000148
      DP(  11,1)=F000194
      DP(  12,1)=F000190
      DP(  13,1)=F000150
      DP(  14,1)=F000152
      DP(  15,1)=F000195
      DP(  16,1)=F000193
      DP(  17,1)=F000154
      DP(  18,1)=F000153
      DP(  19,1)=F000191
      DP(  20,1)=F000192
      DP(  21,1)=F000151
      ENDIF
      RETURN
      END
      SUBROUTINE FSIZESR(NAME_FSIZES,F_SIZES)
      USE SIZES, ONLY: ISIZE
      INTEGER(KIND=ISIZE), DIMENSION(*) :: F_SIZES
      CHARACTER(LEN=*),    DIMENSION(*) :: NAME_FSIZES
      NAME_FSIZES(01)='LTH'; F_SIZES(01)=9
      NAME_FSIZES(02)='LVR'; F_SIZES(02)=5
      NAME_FSIZES(03)='LVR2'; F_SIZES(03)=0
      NAME_FSIZES(04)='LPAR'; F_SIZES(04)=20
      NAME_FSIZES(05)='LPAR3'; F_SIZES(05)=0
      NAME_FSIZES(06)='NO'; F_SIZES(06)=0
      NAME_FSIZES(07)='MMX'; F_SIZES(07)=1
      NAME_FSIZES(08)='LNP4'; F_SIZES(08)=0
      NAME_FSIZES(09)='LSUPP'; F_SIZES(09)=1
      NAME_FSIZES(10)='LIM7'; F_SIZES(10)=0
      NAME_FSIZES(11)='LWS3'; F_SIZES(11)=0
      NAME_FSIZES(12)='MAXIDS'; F_SIZES(12)=79
      NAME_FSIZES(13)='LIM1'; F_SIZES(13)=0
      NAME_FSIZES(14)='LIM2'; F_SIZES(14)=0
      NAME_FSIZES(15)='LIM3'; F_SIZES(15)=0
      NAME_FSIZES(16)='LIM4'; F_SIZES(16)=0
      NAME_FSIZES(17)='LIM5'; F_SIZES(17)=0
      NAME_FSIZES(18)='LIM6'; F_SIZES(18)=0
      NAME_FSIZES(19)='LIM8'; F_SIZES(19)=0
      NAME_FSIZES(20)='LIM10'; F_SIZES(20)=0
      NAME_FSIZES(21)='LIM11'; F_SIZES(21)=0
      NAME_FSIZES(22)='LIM13'; F_SIZES(22)=0
      NAME_FSIZES(23)='LIM15'; F_SIZES(23)=0
      NAME_FSIZES(24)='LIM16'; F_SIZES(24)=0
      NAME_FSIZES(25)='MAXRECID'; F_SIZES(25)=0
      NAME_FSIZES(26)='PC'; F_SIZES(26)=0
      NAME_FSIZES(27)='PCT'; F_SIZES(27)=1
      NAME_FSIZES(28)='PIR'; F_SIZES(28)=22
      NAME_FSIZES(29)='PD'; F_SIZES(29)=10
      NAME_FSIZES(30)='PAL'; F_SIZES(30)=0
      NAME_FSIZES(31)='MAXFCN'; F_SIZES(31)=0
      NAME_FSIZES(32)='MAXIC'; F_SIZES(32)=0
      NAME_FSIZES(33)='PG'; F_SIZES(33)=0
      NAME_FSIZES(34)='NPOPMIXMAX'; F_SIZES(34)=0
      NAME_FSIZES(35)='MAXOMEG'; F_SIZES(35)=4
      NAME_FSIZES(36)='MAXPTHETA'; F_SIZES(36)=10
      NAME_FSIZES(37)='MAXITER'; F_SIZES(37)=20
      NAME_FSIZES(38)='ISAMPLEMAX'; F_SIZES(38)=0
      NAME_FSIZES(39)='DIMTMP'; F_SIZES(39)=0
      NAME_FSIZES(40)='DIMCNS'; F_SIZES(40)=0
      NAME_FSIZES(41)='DIMNEW'; F_SIZES(41)=0
      NAME_FSIZES(42)='PDT'; F_SIZES(42)=17
      NAME_FSIZES(43)='LADD_MAX'; F_SIZES(43)=0
      NAME_FSIZES(44)='MAXSIDL'; F_SIZES(44)=0
      NAME_FSIZES(45)='NTT'; F_SIZES(45)=9
      NAME_FSIZES(46)='NOMEG'; F_SIZES(46)=4
      NAME_FSIZES(47)='NSIGM'; F_SIZES(47)=1
      NAME_FSIZES(48)='PPDT'; F_SIZES(48)=15
      RETURN
      END SUBROUTINE FSIZESR
      SUBROUTINE MUMODEL2(THETA,MU_,ICALL,IDEF,NEWIND,&
      EVTREC,DATREC,IREV,NVNT,INDXS,F,G,H,IRGG,GG,NETAS)
      USE NMPRD4P
      USE SIZES,     ONLY: DPSIZE,ISIZE
      USE PRDIMS,    ONLY: GPRD,HPRD,GERD,HERD,GPKD
      USE NMBAYES_REAL,    ONLY: PRIORINFO
      USE PRDATA, ONLY: MXSTEP=>MXSTP01
      USE NMPRD_REAL,ONLY: ETA,EPS
      USE NMPRD_INT, ONLY: MSEC=>ISECDER,MFIRST=>IFRSTDER,COMACT,COMSAV,IFIRSTEM
      USE NMPRD_INT, ONLY: MDVRES,ETASXI,NPDE_MODE,NOFIRSTDERCODE
      USE NMPRD_REAL, ONLY: DV_LOQ,CDF_L,DV_LAQ,CDF_LA
      USE NMPRD_INT, ONLY: IQUIT
      USE PROCM_INT, ONLY: A_0FLG
      USE PRMOD_REAL,ONLY: A_0,DA_0,D2A_0
      USE NMBAYES_REAL, ONLY: LDF
      IMPLICIT REAL(KIND=DPSIZE) (A-Z)
      REAL(KIND=DPSIZE)   :: MU_(*)
      INTEGER NEWIND
      REAL(KIND=DPSIZE) :: EVTREC
      SAVE
      INTEGER(KIND=ISIZE) :: FIRSTEM
      INTEGER(KIND=ISIZE) :: ICALL,IDEF,IREV,NVNT,INDXS,IRGG,NETAS
      DIMENSION :: IDEF(7,*),THETA(*),EVTREC(IREV,*),INDXS(*),GG(IRGG,GPKD+1,*)
      FIRSTEM=IFIRSTEM
      IF (ICALL <= 1) THEN
      CALL ASSOCNMPRD4
      IDEF(   1,0001)=  -9
      IDEF(   1,0002)=  -1
      IDEF(   1,0003)=   1
      IDEF(   1,0004)=   0
      IDEF(   2,0003)=   0
      IDEF(   2,0004)=   0
      CALL GETETA(ETA)
      IF (IQUIT == 1) RETURN
      RETURN
      ENDIF
      IF (NEWIND /= 2) THEN
      IF (ICALL == 4) THEN
      CALL SIMETA(ETA)
      ELSE
      CALL GETETA(ETA)
      ENDIF
      IF (IQUIT == 1) RETURN
      ENDIF
 !  level            0
      AGE=EVTREC(NVNT,008)
      WT=EVTREC(NVNT,009)
      CLCR=EVTREC(NVNT,010)
      POP_CL=THETA(001)
      POP_V1=THETA(002)
      POP_Q=THETA(003)
      POP_V2=THETA(004)
      COV_CL_AGE=THETA(005)
      COV_V1_WT=THETA(006)
      RUV_PROP=THETA(007)
      RUV_ADD=THETA(008)
      COV_CL_CLCR=THETA(009)
      ETA_CL=ETA(001)
      ETA_V1=ETA(002)
      ETA_Q=ETA(003)
      ETA_V2=ETA(004)
      B000001=WT/70.D0
      B000003=DLOG(B000001)
      LOGTWT=B000003
      B000004=AGE/35.D0
      B000006=DLOG(B000004)
      LOGTAGE=B000006
      B000007=CLCR/83.D0
      B000009=DLOG(B000007)
      LOGTCLCR=B000009
      B000010=DLOG(POP_CL)
      MU_1=B000010+COV_CL_AGE*LOGTAGE+COV_CL_CLCR*LOGTCLCR
      MU_(001)=MU_1
      B000011=MU_1+ETA(001)
      B000012=DEXP(B000011)
      CL=B000012
      B000013=DLOG(POP_V1)
      MU_2=B000013+COV_V1_WT*LOGTWT
      MU_(002)=MU_2
      B000014=MU_2+ETA(002)
      B000015=DEXP(B000014)
      V1=B000015
      MU_3=DLOG(POP_Q)
      MU_(003)=MU_3
      B000016=MU_3+ETA(003)
      B000017=DEXP(B000016)
      Q=B000017
      MU_4=DLOG(POP_V2)
      MU_(004)=MU_4
       RETURN
      B000018=MU_4+ETA(004)
      B000019=DEXP(B000018)
      V2=B000019
      IF(A_0FLG == 1)THEN
      A_0(1)=0.D0
      ENDIF
      IF(A_0FLG == 1)THEN
      A_0(2)=0.D0
      ENDIF
      P000001=V1
      P000002=Q
      P000003=V2
      P000004=CL
      IF (FIRSTEM == 1) THEN
!                      A000042 = DERIVATIVE OF V1 W.R.T. ETA(002)
      A000042=B000015
!                      A000055 = DERIVATIVE OF P000001 W.R.T. ETA(002)
      A000055=A000042
!                      A000057 = DERIVATIVE OF P000002 W.R.T. ETA(003)
      A000057=B000017
!                      A000059 = DERIVATIVE OF P000003 W.R.T. ETA(004)
      A000059=B000019
!                      A000061 = DERIVATIVE OF P000004 W.R.T. ETA(001)
      A000061=B000012
      GG(0001,1,1)=P000001
      GG(0001,0003,1)=A000055
      GG(0002,1,1)=P000002
      GG(0002,0004,1)=A000057
      GG(0003,1,1)=P000003
      GG(0003,0005,1)=A000059
      GG(0004,1,1)=P000004
      GG(0004,0002,1)=A000061
      ELSE
      GG(0001,1,1)=P000001
      GG(0002,1,1)=P000002
      GG(0003,1,1)=P000003
      GG(0004,1,1)=P000004
      ENDIF
      IF (MSEC == 1) THEN
!                      A000044 = DERIVATIVE OF A000042 W.R.T. ETA(002)
      A000044=B000015
!                      A000056 = DERIVATIVE OF A000055 W.R.T. ETA(002)
      A000056=A000044
!                      A000058 = DERIVATIVE OF A000057 W.R.T. ETA(003)
      A000058=B000017
!                      A000060 = DERIVATIVE OF A000059 W.R.T. ETA(004)
      A000060=B000019
!                      A000062 = DERIVATIVE OF A000061 W.R.T. ETA(001)
      A000062=B000012
      GG(0001,0003,0003)=A000056
      GG(0002,0004,0004)=A000058
      GG(0003,0005,0005)=A000060
      GG(0004,0002,0002)=A000062
      ENDIF
      RETURN
      END