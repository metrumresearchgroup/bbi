!*********************************COPYRIGHT******************************************
!                                                                                   !
!       THE NONMEM SYSTEM MAY BE DISTRIBUTED ONLY BY ICON DEVELOPMENT               !
!       SOLUTIONS.                                                                  !
!                                                                                   !
!       COPYRIGHT BY ICON DEVELOPMENT SOLUTIONS                                     !
!       2009-2017 ALL RIGHTS RESERVED.                                              !
!                                                                                   !
!       DO NOT ATTEMPT TO MODIFY CODE WITHOUT FIRST CONSULTING WITH                 !
!       ICON DEVELOPMENT SOLUTIONS.                                                 !
!                                                                                   !
!************************************************************************************
!
!-----------------------------HISTORY------------------------------------------------
! VERSION     : NONMEM VII
! AUTHOR      : ALISON J. BOECKMANN
! CREATED ON  : JUN/1984
! LANGUAGE    : FORTRAN 90/95
! LAST UPDATE : MAY/1990 - SECOND DERIVATIVES AND DERIVATIVES W.R.T. DT
!               JUL/2008 - COMMON BLOCKS REPLACED WITH MODULES
!               NOV/2008 - INTRODUCED HEADER INFORMATIONS AND RESTRUCTURED AS PER
!                          THE NONMEM STANDARDS
!               FEB/2009 - MOVED DATA STATEMENTS TO PRDATA.F90 FILE
!               APR/2009 - INTRODUCED ERROR CHECK FOR FILE OPERATION
!               FEB/2010 - CHANGED SIZES TO PRSIZES
!               FEB/2011 - INTEGRATED 7.2BETA5.8B MODIFICATIONS
!
!----------------------------- ADVAN4.F90 -------------------------------------------
!
! SUBROUTINE ADVAN(ICALL)
!
! DESCRIPTION : ADVAN4 is a routine in PREDPP's library which implements the kinetic
!               equations for the Two Compartment Linear Model with First Order
!               Absorption. Bi-exponential model with urine compartment and with absorption
!
! ARGUMENTS   : ICALL
!               IN     - ICALL
!                        ICALL - Values are 0,1,2 or 3. ICALL values 0,1,2,3 from NONMEM
!                                are unchanged. ICALL values >= 4 are converted to 2 for
!                                this routine.
!               OUT    - NONE
!               IN OUT - NONE
!
! CALLED BY   : FCN2   - FCN2 copies Y to the state vector and calls ADVAN to advance the 
!                        system (SS6 already set up infusion array and DT). It inputs the
!                        SS dose over the advance interval. It then computes the 
!                        difference between the initial state vector and the post-advance
!                        state vector for multiple doses-interfaces with ADVAN.
!               SADVAN - Stands for Supervisor ADVAN.  This is an interface.
!                        PREDPP contains a library of routines, called ADVAN routines,
!                        which implement specific kinetic models. Exactly one ADVAN
!                        routine must be selected for each NONMEM / PREDPP run.
!                        Its function is to "ADVANCE" the kinetic system from one
!                        state time point to the next
!               SS6    - General routine for all models
!               SS9    - General routine for all models
!               SS13   - General routine for all models
!
! CALLS       : ERRORMSGS - Writes error messages to a file JUNIT and sets IQUIT to 1 indicating
!                           that NONMEM has to quit for non-super problems. And for super problems
!                           calculation continues with next sub problem.
!
! ALGORITHM   : - IF (ICALL == 2) Normal Entry; Else
!                 - Initialization call
!                 - Set MODEL definition variables
!                 - Define the compartments
!                 - Define PK parameters
!                 - If output errors, skip to label 502 - call ERRORMSGS
!                 - RETURN
!               - Normal Entry
!                 - Check for errors in basic PK parameters
!                 - Compute for common terms and expressions
!                 - Save old compartments and start computations
!
!                 - Loop on ALPHA,BETA: RT(I)=ALPHA, RT(J)=BETA within loop
!                   - Compute denominators for fractions
!                   - Compute central compartment
!                   - Case (TERMBR= 1,2,3,4); executes different blocks of the code
!                   - Now compute the peripheral compartment
!                   - Case (TERMBR= 1,2,3,4); executes different blocks of the code
!                 - End of loop ALPHA,BETA
!                 - Process absorption compartment
!
!                 - Calculate derivatives
!                 - J loop: 1,NPETAS  ! Derivatives
!                   - Compute for common terms and expressions
!                   - Save old derivatives
!                   - Loop on ALPHA, BETA
!                     - Compute central compartment
!                     - Case (TERMBR= 1,2,3,4); executes different blocks of the code
!                     - Compute periperal compartment
!                     - Case (TERMBR= 1,2,3,4); executes different blocks of the code
!                   - End loop on ALPHA, BETA
!                   - Compute Absorption compartment
!                   - Save first derivatives
!                   - Compute second derivatives
!                   - K loop: 1,J
!                     - Compute for common terms and expressions
!                     - Save old derivatives
!                     - Loop on ALPHA, BETA
!                       - Compute central compartment
!                       - Case (TERMBR= 1,2,3,4); executes different blocks of the code
!                       - Compute periperal compartment
!                       - Case (TERMBR= 1,2,3,4); executes different blocks of the code
!                     - End of loop ALPHA, BETA
!                     - Process Absorption compartment
!                   - K Loop end
!                 - J Loop end
!
! MODULES USED: PRSIZES,PRDATA,NMPRD_INT,PRCOM_INT,PRMOD_INT,PROCM_INT,PRCOM_REAL,
!               PROCM_REAL,PRMOD_REAL,NMPRD_CHAR,PRMOD_CHAR,PRCOM_LOG,NM_INTERFACE
!
! CONTAINS    : NONE
!
! LOCAL'S     : A10,A10E,A10E1,A10E2,A12,A12E,A12E1,A12E2,A21,A21E,A21E1,A21E2,A21R,
!               A21RE,A21RE1,A21RE2,AA,AADD,AADDE,AADDE1,AADDE2,AAE,AAE1,AAE2,BATCE,
!               BATCE1,BATCE2,BATEC,BATEP,BATPE,BATPE1,BATPE2,CKA,CKAE,CKAE2,CKC,CKCE,
!               CKCE2,CKP,CKPE,CKPE2,D10,D10E,D10E1,D10E2,DA,DADFE,DADFE1,DADFE2,DADIF,
!               DADS,DADSE,DADSE1,DADSE2,DAE,DAE1,DAE2,DDFAE,DDFAE1,DDFAE2,DDIF,DDIFA,
!               DDIFE,DDIFE1,DDIFE2,DEDFE,DEDFE1,DEDFE2,DEDIF,DR,DRE,DRE1,DRE2,EX,EXA,
!               EXAE,EXAE1,EXAE2,EXD,EXDA,EXDAE,EXDAE1,EXDAE2,EXDE,EXDE1,EXDE2,EXE,
!               EXE1,EXE2,FA,FAE,FAE1,FAE2,FC,FCC,FCCE,FCCE1,FCCE2,FCE,FCE1,FCE2,FP,
!               FPE,FPE1,FPE2,FPP,FPPE,FPPE1,FPPE2,G,G2,GC,GCE,GCE1,GCE2,GE,GE2,GG,
!               GP,GPE,GPE1,GPE2,GR,GRE,GRE1,GRE2,HC,HCE,HCE1,HCE2,HP,HPE,HPE1,HPE2,
!               I,IOUT,ITEMP,J,K,N,OLDA,OLDAE,OLDAE1,OLDAE2,P,PE,PE2,PPE,RT,RTE,RTE1,
!               RTE2,S,SE,SE1,SE2,T1,T1E,T1E1,T1E2,T2,T2E,T2E1,T2E2,T3,T3E,T3E1,T3E2,
!               T4,T4E,T4E1,T4E2,T5,T5E,T5E1,T5E2,T6,T6E,T6E1,T6E2,TC,TCE,TCE1,TCE2,
!               TDEL,TDELE,TDELE2,TEMP,TEMPE,TEMPE1,TEMPE2,TERMBR,TOTR,TOTRE,TOTRE1,
!               TOTRE2,TP,TPE,TPE1,TPE2,U,UE,UE1,UE2,Y,YE,YE1,YE2,Z,ZE,ZE1,ZE2
!
!---------------------------- END OF HEADER -----------------------------------------
!
      SUBROUTINE ADVAN(ICALL)
!
      USE PRSIZES,      ONLY: ISIZE,DPSIZE,PE
!      
      USE PRDATA,       ONLY: IDENT_A4,KSV_A4,KINST_A4,KTURN_A4
! INTEGER
      USE NMPRD_INT,    ONLY: IERPRD
      USE PRCOM_INT,    ONLY: ITSC,BETA,ADVID,IDC,IDO,ISV,IINST,ITURN,SV,ISPEC,NPETAS,  &
                              LOGUNT,NC,NBP
      USE PRMOD_INT,    ONLY: IKE,IK12,IK21,IV,KP,KC,IKA,KA
      USE PROCM_INT,    ONLY: IDXETA
! REAL
      USE PRCOM_REAL,   ONLY: D2ADTE,I2DEA,I2REA,R2E,ADTE,G3,DT,IRA,IREA,IDA,IDEA,R,RE, &
                              ZERO,ONE
      USE PROCM_REAL,   ONLY: AMNT,DAETA,D2AETA
      USE PRMOD_REAL,   ONLY: TWO,FOUR
! CHARACTER
      USE NMPRD_CHAR,   ONLY: ETEXT
      USE PRMOD_CHAR,   ONLY: NAME
! LOGICAL
      USE PRCOM_LOG,    ONLY: NOETAS,SECOND
! INTERFACE
      USE NM_INTERFACE, ONLY: ERRORMSGS
!
      IMPLICIT NONE
!
      INTEGER(KIND=ISIZE), INTENT(IN) :: ICALL
!      
      SAVE
!
!------------------------------------------------------------------------------------
!     COMMON /NMPRD1/ IERPRD,NETEXT
!     COMMON /NMPRD2/ ETEXT(3)
!     INTEGER IERPRD,NETEXT
!     CHARACTER*132 ETEXT
!     INTEGER ICALL
!     COMMON /PRCOM0/ NP,NBP,YFORM
!     COMMON /PRCOM0/ MAXKF,IFORM
!     COMMON /PRCOM0/ IDC,IDO,MAXIC,ISV,IINST,ITURN
!     COMMON /PRCOM0/ JTIME,JCONT,JEVENT,JAMT,JRATE,JSS,JDELTA
!     COMMON /PRCOM0/ JCOMPT,JCOMPF,JERROR,SSC,KREC,JMORE,JDUM
!     COMMON /PRCOM1/ NOETAS,SECOND
!     COMMON /PRCOM2/ IBF,IRR,IS,ID,ITSC,IFR,ILAG
!     COMMON /PRCOM3/ ITRANS,IRGG,IREV,NPETAS,NPEPS
!     COMMON /PRCOM4/ G3,HH,DELTA,DT,DTE
!     COMMON /PRCOM4/ YMULT,ZERO,ONE,XR,XD,TSTART,DTSTAR
!     COMMON /PRCOM4/ DDELTA,D2DELT,ADTE,D2ADTE
!     COMMON /PRCOM5/ ISPEC,DCTR,BETA,DD
!     COMMON /PRCOM5/ IP,IPOOL,IHEAD,INEXT,IBACK,SV
!     COMMON /PRCOM6/ IA,IAA,IAEA,IRA,IREA,IDA,IDEA,R,RE
!     COMMON /PRCOM6/ RHO,RHOE,SDEL,SDELE,SSA,SSAE,SSR,SSRE
!     COMMON /PRCOM6/ SAMT,SDEL1
!     COMMON /PRCOM6/ I2AEA,I2REA,I2DEA,R2E,D2DTE,D2TSTA
!     COMMON /PRCOM7/ ADVID,SSID
!     COMMON /PRCOMN/ LOGUNT,NC
!     COMMON /PRNAME/ NAME
!     CHARACTER*8 NAME(PC)
!     DOUBLE PRECISION DELTA,G3,HH
!     DOUBLE PRECISION SDELE,RHOE,SSAE,SSRE,YMULT,ZERO,XR,XD
!     DOUBLE PRECISION ONE,TSTART,DTSTAR(PE)
!     DOUBLE PRECISION DDELTA(PE),D2DELT(PE,PE),ADTE(PE),D2ADTE(PE,PE)
!     DOUBLE PRECISION IA(90),IAA(90),IAEA(90,PE),IRA(90),IDA(90)
!     DOUBLE PRECISION IREA(90,PE),IDEA(90,PE),R(PC),RE(PC,PE)
!     DOUBLE PRECISION I2REA(90,PE,PE),I2DEA(90,PE,PE),I2AEA(90,PE,PE)
!     DOUBLE PRECISION R2E(PC,PE,PE),D2DTE(PE,PE)
!     DOUBLE PRECISION D2TSTA(PE,PE)
!     DOUBLE PRECISION DT,DTE(PE),RHO,SDEL,SSA,SSR
!     DOUBLE PRECISION SAMT,SDEL1
!     DIMENSION SDELE(PE),RHOE(PE),SSAE(PE),SSRE(PE)
!     DIMENSION G3(PG+1,PE+1,PE+1),HH(PE,PE)
!     INTEGER LOGUNT,IRGG,IREV,ITRANS,NPETAS,NPEPS
!     INTEGER JCONT,JTIME,JEVENT,JAMT,JRATE,JSS,JDELTA
!     INTEGER JCOMPT,JCOMPF,JERROR
!     INTEGER NC,IDC,IDO,NP,NBP,SSC,KREC,JMORE,JDUM
!     INTEGER ISV(PC),IBF(PC),IRR(PC),SV(PC)
!     INTEGER IINST(PC),ITURN(PC),ITSC,IFR,ILAG(PC),IS(PC),ID(PC)
!     INTEGER ADVID,SSID,MAXKF,IFORM(PG+1),YFORM,MAXIC
!     INTEGER BETA(90),IPOOL(90),IP,IHEAD,INEXT(90),IBACK(90)
!     INTEGER ISPEC,DD(90),DCTR
!     LOGICAL NOETAS,SECOND
! VARIABLES USED TO ADVANCE BY DT
! FOR INFUSIONS INTO PRIMARY COMPARTMENT WHICH CONTINUE PAST DT:
! R(IDC)=INFUSION RATE INTO PRIMARY COMPARTMENT
! RE(IDC,K)=DERIVATIVE OF RATE WRT ETA(K)
! IF AN INFUSION INTO PRIMARY COMPT. IS TERMINATING, ISPEC>0 AND:
! IRA(ISPEC)=ITS INFUSION RATE
! IREA(ISPEC,K)=DERIVATIVE OF IRA WRT ETA(K)
! IDA(ISPEC)=ITS DURATION
! IDEA(ISPEC,K)=DERIVATIVE OF IDEA WRT ETA(K)
! BETA(ISPEC)=ITS COMPARTMENT NUMBER
!     COMMON /PRBI/ TWO,FOUR,IKE,IK12,IK21,IV,KP,KC
!     COMMON /PRBI/ IKA,KA
!     INTEGER IOUT ! KP,KC,IOUT,IKE,IK12,IK21,IV
!     INTEGER KA,IKA
!     DOUBLE PRECISION TWO,FOUR
!     COMMON /PROCM4/ A,DAETA,D2AETA
!     DOUBLE PRECISION A,DAETA,D2AETA
!     DIMENSION A(PC),DAETA(PC,PE),D2AETA(PC,PE,PE)
! FOR LVOUT FEATURE
!     COMMON /PROCM5/ NACTIV,M(0:PE)
!     INTEGER NACTIV,M
!
!------------------------------------------------------------------------------------
!
! Local Variables
!
      INTEGER(KIND=ISIZE) :: IOUT,I,J,K,TERMBR,N,ITEMP
!
      REAL(KIND=DPSIZE)   :: A10,A10E,A10E1(PE),A10E2,A12,A12E,A12E1(PE),A12E2,A21,A21E,  &
                             A21E1(PE),A21E2,A21R(2),A21RE(2),A21RE1(PE,2),A21RE2(2),AA,  &
                             AADD(2),AADDE(2),AADDE1(PE,2),AADDE2(2),AAE,AAE1(PE),AAE2,   &
                             BATCE,BATCE1(PE),BATCE2,BATEC,BATEP,BATPE,BATPE1(PE),BATPE2, &   
                             CKA,CKAE,CKAE2,CKC,CKCE,CKCE2,CKP,CKPE,CKPE2,D10,D10E1(PE),  &
                             D10E,D10E2,DA,DADFE(2),DADFE1(PE,2),DADFE2(2),DADIF(2),DAE,  &
                             DADS(2),DADSE(2),DADSE1(PE,2),DADSE2(2),DAE1(PE),DDFAE(2),   &
                             DAE2,DDFAE1(PE,2),DDFAE2(2),DDIF(2),DDIFA(2),DDIFE(2),DR(2), &
                             DDIFE1(PE,2),DDIFE2(2),DEDFE(2),DEDFE1(PE,2),DEDIF(2),DRE(2),&
                             DEDFE2(2),DRE1(PE,2),DRE2(2),EX(2),EXA,EXAE,EXAE1(PE),EXAE2, &
                             EXD(2),EXDA,EXDAE,EXDAE1(PE),EXDAE2,EXDE(2),EXDE1(PE,2),FAE, &
                             EXDE2(2),EXE(2),EXE1(PE,2),EXE2(2),FAE1(PE),FA,FAE2,FC(2),   &
                             FCC,FCCE,FCCE1(PE),FCCE2,FCE(2),FCE1(PE,2),FCE2(2),FP(2),    &
                             FPE(2),FPE1(PE,2),FPE2(2),FPP,FPPE,FPPE1(PE),FPPE2,G,G2,     &
                             GC(2),GCE(2),GCE1(PE,2),GCE2(2),GE,GE2,GG,GP(2),GPE(2),      &
                             GPE1(PE,2),GPE2(2),GR(2),GRE(2),GRE1(PE,2),GRE2(2),HC(2),    &
                             HCE(2),HCE1(PE,2),HCE2(2),HP(2),HPE(2),HPE1(PE,2),HPE2(2),   &
                             OLDA(4),OLDAE(4),OLDAE1(PE,4),OLDAE2(4),P,PE2,PPE,RT(2),     &
                             RTE(2),RTE1(PE,2),RTE2(2),S,SE,SE1(PE),SE2,T1(2),T1E(2),     &
                             T1E1(PE,2),T1E2(2),T2(2),T2E(2),T2E1(PE,2),T2E2(2),T3(2),    &
                             T3E(2),T3E1(PE,2),T3E2(2),T4(2),T4E(2),T4E1(PE,2),T4E2(2),   &
                             T5(2),T5E(2),T5E2(2),T5E1(PE,2),T6(2),T6E(2),T6E1(PE,2),     &
                             TC(2),T6E2(2),TCE(2),TCE1(PE,2),TCE2(2),TDEL,TDELE,TDELE2,   &
                             TEMP,TEMPE,TEMPE1(PE),TEMPE2,TOTR,TOTRE,TOTRE1(PE),TOTRE2,   &
                             TP(2),TPE(2),TPE1(PE,2),TPE2(2),U,UE,UE1(PE),UE2,Y,YE,       &
                             YE1(PE),YE2,Z,ZE,ZE1(PE),ZE2
!
      G2(I,J,K)=G3(I,IDXETA(J)+1,IDXETA(K)+1) ! Statement function
      GG(I,J)=G3(I,IDXETA(J-1)+1,1)
!
      IF (ICALL /= 2) THEN   ! Initialization call
!
! Set MODEL definition variables
        KA=1;  IDC=KA;  ADVID=4 
        KC=2;  IDO=KC;  NBP=5
        KP=3;  IOUT=4;  NC=4 
!
! Define the compartments
        FORALL (J=1:4)
          ISV(J)  =KSV_A4(J)
          IINST(J)=KINST_A4(J)
          ITURN(J)=KTURN_A4(J)
          NAME(J) =IDENT_A4(J)
        END FORALL
!
! Define PK parameters
        IKE=1; IK12=2; IV=4; 
        IKA=5; IK21=3;
!
        WRITE (LOGUNT,10,ERR=502)
        WRITE (LOGUNT,15,ERR=502) NBP
        WRITE (LOGUNT,20,ERR=502) IKE,IK12,IK21,IKA
        TWO=2.0D0
        FOUR=4.0D0
        GO TO 999
      END IF  
!
! Normal Entry
      A10=GG(ITSC,1)*GG(IKE,1)
      A12=GG(ITSC,1)*GG(IK12,1)
      A21=GG(ITSC,1)*GG(IK21,1)
      AA =GG(ITSC,1)*GG(IKA,1)
!
! Check for errors in basic PK parameters
      IF (GG(ITSC,1) <= ZERO)  THEN
        IERPRD=1
        ETEXT(2)='PK PARAMETER FOR TIME SCALE IS NON-POSITIVE'
        GO TO 999
      END IF
      IF (A10 <= ZERO) THEN
        IERPRD=1
        ETEXT(2)='PK PARAMETER FOR K IS NON-POSITIVE'
        GO TO 999
      END IF
      IF (A12 <= ZERO) THEN
        IERPRD=1
        ETEXT(2)='PK PARAMETER FOR K23 IS NON-POSITIVE'
        GO TO 999
      END IF
      IF (A21 <= ZERO) THEN
        IERPRD=1
        ETEXT(2)='PK PARAMETER FOR K32 IS NON-POSITIVE'
        GO TO 999
      END IF
      IF (SV(KA) /= 0) THEN
        IF (AA <= ZERO) THEN
          IERPRD=1
          ETEXT(2)='PK PARAMETER FOR KA IS NON-POSITIVE'
          GO TO 999
        END IF
      END IF
!
      D10=ONE/A10
      G=A12+A10
      Z=A12/A21
      Y=G/A21
      S=A10+A12+A21
      P=A21*A10
      TEMP=SQRT(S*S-FOUR*P)
      RT(1)=(S+TEMP)/TWO
      RT(2)=(S-TEMP)/TWO
!
      IF (RT(2) == ZERO) THEN
        IERPRD=1
        ETEXT(1)='A ROOT OF THE CHARACTERISTIC EQUATION IS ZERO BECAUSE'
        ETEXT(2)='K*K32 IS MUCH SMALLER THAN  (K+K23+K32)**2.'
        ETEXT(3)='PERHAPS K OR K32 IS VERY SMALL, OR K23 IS VERY LARGE.'
        GO TO 999
      END IF
!
      TERMBR=4
      IF (ISPEC /= 0) THEN 
        IF (BETA(ISPEC) == KA) TERMBR=1
        IF (BETA(ISPEC) == KC) TERMBR=2
        IF (BETA(ISPEC) == KP) TERMBR=3
      END IF  
!
! Common terms and expressions
      DO I=1,2
        DR(I)=ONE/RT(I)
        EX(I)=EXP(-DT*RT(I))
        IF (ISPEC /= 0) EXD(I)=EXP(-(DT-IDA(ISPEC))*RT(I))
        A21R(I)=A21-RT(I)
        GR(I)=G-RT(I)
      END DO
!
! Save old compartments and start computations
      FORALL(I=1:4) OLDA(I)=AMNT(I)
      TOTR=R(KP)+R(KC)+R(KA)
      CKC=ZERO
      CKP=ZERO
      IF (TOTR /= ZERO) THEN 
        CKC=(R(KP)+R(KC))*D10
        CKP=(R(KP)*Y+R(KC)*Z)*D10
      END IF
!
      N=3
      DO I=1,2  ! Loop on ALPHA,BETA: RT(I)=ALPHA, RT(J)=BETA within loop
        N=N-1
!
! Denominators for fractions
        DDIF(I)=ONE/(RT(N)-RT(I))
        DEDIF(I)=D10*DDIF(I)
        DADIF(I)=DR(I)*DDIF(I)
        DADS(I)=DADIF(I)*DDIF(I)
        AADD(I)=ZERO
!
        IF (SV(KA) /= 0) THEN 
          IF (AA-RT(I) == ZERO) THEN
            IERPRD=1
            ETEXT(1)='A ROOT OF THE CHARACTERISTIC EQUATION EQUALS KA'
            GO TO 999
          END IF
          DDIFA(I)=ONE/(AA-RT(I))
          AADD(I)=AA*DDIFA(I)
        END IF
!
!  The central compartment
        GC(I)=A21R(I)*DDIF(I)*EX(I)
        FC(I)=GC(I)*AADD(I)
        HC(I)=A21*DDIF(I)*EX(I)
!
        IF (TOTR /= ZERO) THEN 
          T1(I)=R(KC)*A21R(I)+R(KP)*A21+R(KA)*A21R(I)*AADD(I)
          CKC=CKC-T1(I)*DADIF(I)*EX(I)
        END IF
!          
        ITEMP=0              ! Temp variable introduced to remove GOTO
        SELECT CASE (TERMBR)
        CASE (1)
          T2(I)=-(A21R(I)*A21R(I)+A12*A21)*AADD(I)
          T3(I)=A21R(I)+A12+A21R(I)*A10*DDIFA(I)
        CASE (2)
          T2(I)=-(A21R(I)*A21R(I)+A12*A21)
          T3(I)=A12+A21R(I)
        CASE (3)
          T2(I)=-(A21R(I)*A21+GR(I)*A21)
          T3(I)=G+A21R(I)
        CASE (4)
          ITEMP=1
        END SELECT
!
        IF (ITEMP == 0) THEN
          TC(I)=T2(I)*DADS(I)*EX(I)+T3(I)*DEDIF(I)*EXD(I)
          CKC=CKC+IRA(ISPEC)*TC(I)
        END IF  
!
! Now the peripheral compartment
        GP(I)=A12*DDIF(I)*EX(I)
        FP(I)=GP(I)*AADD(I)
        HP(I)=GR(I)*DDIF(I)*EX(I)
!
        IF (TOTR /= ZERO) THEN 
          T4(I)=R(KC)*A12+R(KP)*GR(I)+R(KA)*A12*AADD(I)
          CKP=CKP-T4(I)*DADIF(I)*EX(I)
        END IF
!  
        SELECT CASE (TERMBR)
          CASE (1)
            T5(I)=-A12*(A21R(I)+GR(I))*AADD(I)
            T6(I)=A12+GR(I)*Z+A12*A10*DDIFA(I)
          CASE (2)
            T5(I)=-(GR(I)+A21R(I))*A12
            T6(I)=A12+Z*GR(I)
          CASE (3)
            T5(I)=-(GR(I)*GR(I)+A12*A21)
            T6(I)=A12+Y*GR(I)
          CASE (4)
            CYCLE
        END SELECT
!
        TP(I)=T5(I)*DADS(I)*EX(I)+T6(I)*DEDIF(I)*EXD(I)
        CKP=CKP+IRA(ISPEC)*TP(I)
!
      END DO
!
! Absorption compartment
      IF (SV(KA) /= 0) THEN
        EXA=EXP(-DT*AA)
        DA=ONE/AA; FA=EXA; CKA=ZERO
        BATEC=DDIFA(1)*DDIFA(2)*(A21-AA)
        BATEP=DDIFA(1)*DDIFA(2)*A12
        FCC=AA*BATEC*EXA
        FPP=AA*BATEP*EXA
!
        IF (R(KA) /= ZERO) THEN 
          CKA=R(KA)*(ONE-EXA)*DA
          CKC=CKC+R(KA)*(D10-BATEC*EXA)
          CKP=CKP+R(KA)*(Z*D10-BATEP*EXA)
        END IF
!
        IF (TERMBR == 1) THEN 
          TDEL=(DT-IDA(ISPEC))*AA
          EXDA=EXP(-TDEL)
          U=IRA(ISPEC)*(EXDA-EXA)
          CKA=CKA+U*DA
          CKC=CKC+U*BATEC
          CKP=CKP+U*BATEP
        END IF  
      END IF  
!
      AMNT(KC)=OLDA(KC)*(GC(1)+GC(2))+OLDA(KP)*(HC(1)+HC(2))+CKC
      AMNT(KP)=OLDA(KC)*(GP(1)+GP(2))+OLDA(KP)*(HP(1)+HP(2))+CKP
!
      IF (SV(KA) /= 0) THEN 
        AMNT(KC)=AMNT(KC)+OLDA(KA)*(FCC+FC(1)+FC(2))
        AMNT(KP)=AMNT(KP)+OLDA(KA)*(FPP+FP(1)+FP(2))
        AMNT(KA)=OLDA(KA)*FA+CKA
      END IF  
!
      IF (SV(IOUT) /= 0) THEN 
        AMNT(IOUT) = OLDA(IOUT)+OLDA(KC)+OLDA(KP)+OLDA(KA)-AMNT(KC)-AMNT(KP) &
                    -AMNT(KA)+TOTR*DT
        IF (ISPEC /= 0) AMNT(IOUT)=AMNT(IOUT)+IRA(ISPEC)*IDA(ISPEC)
      END IF
!  
      IF (NOETAS) GO TO 999
!
! Derivatives
      DO J=1,NPETAS          ! Loop over NPETAS
        A10E=GG(ITSC,1)*GG(IKE,J+1)+GG(ITSC,J+1)*GG(IKE,1)
        A12E=GG(ITSC,1)*GG(IK12,J+1)+GG(ITSC,J+1)*GG(IK12,1)
        A21E=GG(ITSC,1)*GG(IK21,J+1)+GG(ITSC,J+1)*GG(IK21,1)
        AAE =GG(ITSC,1)*GG(IKA,J+1)+GG(ITSC,J+1)*GG(IKA,1)
        D10E=-D10*A10E/A10
        GE=A12E+A10E
        ZE=(A12E-Z*A21E)/A21
        YE=(GE-Y*A21E)/A21
        SE=A10E+A12E+A21E
        PPE=A21*A10E+A21E*A10
        TEMPE=ONE/(TWO*TEMP)*(TWO*S*SE-FOUR*PPE)
        RTE(1)=(SE+TEMPE)/TWO
        RTE(2)=(SE-TEMPE)/TWO
!
! Common terms and expressions
        DO I=1,2
          DRE(I) = -DR(I)/RT(I)*RTE(I)
          EXE(I) = -EX(I)*DT*RTE(I)-EX(I)*ADTE(J)*RT(I)
          IF (ISPEC /= 0) EXDE(I) = -EXD(I)*((DT-IDA(ISPEC))*RTE(I) &
                                    +RT(I)*(ADTE(J)-IDEA(ISPEC,J)))
          A21RE(I)=A21E-RTE(I)
          GRE(I)=GE-RTE(I)
        END DO
!
! Save old derivatives
        FORALL (I=1:4) OLDAE(I)=DAETA(I,J)
        TOTRE=RE(KA,J)+RE(KC,J)+RE(KP,J)
        CKCE=ZERO
        CKPE=ZERO
        IF (TOTR /= ZERO) THEN 
          CKCE = (R(KP)+R(KC))*D10E+(RE(KP,J)+RE(KC,J))*D10
          CKPE = (R(KP)*Y+R(KC)*Z)*D10E +(R(KP)*YE+RE(KP,J)*Y       &
                 +R(KC)*ZE+RE(KC,J)*Z)*D10
        END IF
!
        N=3
        DO I=1,2             ! Loop on ALPHA, BETA
          N=N-1
          DDIFE(I) =-(RTE(N)-RTE(I))*DDIF(I)*DDIF(I)
          DEDFE(I) = D10*DDIFE(I)+D10E*DDIF(I)
          DADFE(I) = DR(I)*DDIFE(I)+DRE(I)*DDIF(I)
          DADSE(I) = DADIF(I)*DDIFE(I)+DADFE(I)*DDIF(I)
          AADDE(I) = ZERO
          IF (SV(KA) /= 0) THEN
            DDFAE(I) =-DDIFA(I)*DDIFA(I)*(AAE-RTE(I))
            AADDE(I) = AAE*DDIFA(I)+AA*DDFAE(I)
          END IF  
!
! Central compartment
          GCE(I) = A21R(I)*DDIF(I)*EXE(I)+A21R(I)*DDIFE(I)*EX(I)              &
                  +A21RE(I)*DDIF(I)*EX(I)
          FCE(I) = GCE(I)*AADD(I)+GC(I)*AADDE(I)
          HCE(I) = A21*DDIF(I)*EXE(I)+A21*DDIFE(I)*EX(I)+A21E*DDIF(I)*EX(I)
!
          IF (TOTR /= ZERO) THEN 
            T1E(I) = R(KC)*A21RE(I)+RE(KC,J)*A21R(I)+R(KP)*A21E+RE(KP,J)*A21  &
                    +R(KA)*A21R(I)*AADDE(I)+RE(KA,J)*A21R(I)*AADD(I)          &
                    +R(KA)*A21RE(I)*AADD(I)
            CKCE = CKCE-T1(I)*DADIF(I)*EXE(I)-T1(I)*DADFE(I)*EX(I)            &
                  -T1E(I)*DADIF(I)*EX(I)
          END IF  
!
          ITEMP=0
          SELECT CASE (TERMBR)
          CASE (1)
            T2E(I) =-(A21R(I)*A21R(I)+A12*A21)*AADDE(I)-(TWO*A21R(I)*A21RE(I) &
                    +A12*A21E+A12E*A21)*AADD(I)
            T3E(I) = A21RE(I)+A12E+A21R(I)*A10*DDFAE(I)+A21R(I)*A10E*DDIFA(I) &
                    +A21RE(I)*A10*DDIFA(I)
          CASE (2)
            T2E(I) =-(TWO*A21R(I)*A21RE(I)+A12*A21E+A12E*A21)
            T3E(I) = A12E+A21RE(I)
          CASE (3)
            T2E(I) =-(A21R(I)*A21E+A21RE(I)*A21+GR(I)*A21E+GRE(I)*A21)
            T3E(I) = GE+A21RE(I)
          CASE (4)
            ITEMP=1
          END SELECT
!
          IF (ITEMP == 0) THEN
            TCE(I) = T2(I)*DADS(I)*EXE(I)+T2(I)*DADSE(I)*EX(I)      &
                    +T2E(I)*DADS(I)*EX(I)+T3(I)*DEDIF(I)*EXDE(I)    &
                    +T3(I)*DEDFE(I)*EXD(I)+T3E(I)*DEDIF(I)*EXD(I)   
            CKCE = CKCE+IREA(ISPEC,J)*TC(I)+IRA(ISPEC)*TCE(I)
          END IF  
!
! Periperal compartment
          GPE(I) = A12*DDIF(I)*EXE(I)+A12*DDIFE(I)*EX(I)            &
                  +A12E*DDIF(I)*EX(I)
          FPE(I) = GP(I)*AADDE(I)+GPE(I)*AADD(I)
          HPE(I) = GR(I)*DDIF(I)*EXE(I)+GR(I)*DDIFE(I)*EX(I)        &
                  +GRE(I)*DDIF(I)*EX(I)
!
          IF (TOTR /= ZERO) THEN 
            T4E(I) = R(KC)*A12E+RE(KC,J)*A12+R(KP)*GRE(I)           &
                    +RE(KP,J)*GR(I)+R(KA)*A12*AADDE(I)              &
                    +R(KA)*A12E*AADD(I)+RE(KA,J)*A12*AADD(I)
            CKPE = CKPE-T4(I)*DADIF(I)*EXE(I)-T4(I)*DADFE(I)*EX(I)  &
                  -T4E(I)*DADIF(I)*EX(I)
          END IF
!        
          SELECT CASE (TERMBR)
          CASE (1)
            T5E(I) =-A12*(A21R(I)+GR(I))*AADDE(I)-A12*(A21RE(I)     &
                     +GRE(I))*AADD(I)-A12E*(A21R(I)+GR(I))*AADD(I)
            T6E(I) = A12E+GR(I)*ZE+GRE(I)*Z+A12*A10*DDFAE(I)        &
                    +(A12*A10E+A12E*A10)*DDIFA(I)
          CASE (2)
            T5E(I) =-(GR(I)+A21R(I))*A12E-(GRE(I)+A21RE(I))*A12
            T6E(I) = A12E+Z*GRE(I)+ZE*GR(I)
          CASE (3)
            T5E(I) =-(TWO*GR(I)*GRE(I)+A12*A21E+A12E*A21)
            T6E(I) = A12E+Y*GRE(I)+YE*GR(I)
          CASE (4)
            CYCLE 
          END SELECT
!
          TPE(I) = T5(I)*DADS(I)*EXE(I)+T5(I)*DADSE(I)*EX(I)        &
                  +T5E(I)*DADS(I)*EX(I)+T6(I)*DEDIF(I)*EXDE(I)      &
                  +T6(I)*DEDFE(I)*EXD(I)+T6E(I)*DEDIF(I)*EXD(I)     
          CKPE = CKPE+IREA(ISPEC,J)*TP(I)+IRA(ISPEC)*TPE(I)
        END DO
!
! Absorption compartment
        IF (SV(KA) /= 0) THEN 
          EXAE=-EXA*(DT*AAE+ADTE(J)*AA)
          DAE=-DA*DA*AAE
          FAE=EXAE
          CKAE=ZERO
          BATCE = DDIFA(1)*DDIFA(2)*(A21E-AAE)+(DDIFA(1)*DDFAE(2)   &
                 +DDFAE(1)*DDIFA(2))*(A21-AA)
          BATPE = DDIFA(1)*DDIFA(2)*A12E +(DDIFA(1)*DDFAE(2)        &
                 +DDFAE(1)*DDIFA(2))*A12
          FCCE  = AA*BATEC*EXAE+AA*BATCE*EXA+AAE*BATEC*EXA
          FPPE  = AA*BATEP*EXAE+AA*BATPE*EXA+AAE*BATEP*EXA
!
          IF (R(KA) /= ZERO) THEN 
            CKAE = R(KA)*(ONE-EXA)*DAE-R(KA)*EXAE*DA+RE(KA,J)*(ONE-EXA)*DA
            CKCE = CKCE+R(KA)*(D10E-BATEC*EXAE-BATCE*EXA)                   &
                  +RE(KA,J)*(D10-BATEC*EXA)
            CKPE = CKPE+R(KA)*(Z*D10E+ZE*D10-BATEP*EXAE-BATPE*EXA)          &    
                  +RE(KA,J)*(Z*D10-BATEP*EXA)
          END IF
!      
          IF (TERMBR == 1) THEN 
            TDELE = (ADTE(J)-IDEA(ISPEC,J))*AA+(DT-IDA(ISPEC))*AAE
            EXDAE = -EXDA*TDELE
            UE    = IRA(ISPEC)*(EXDAE-EXAE)+IREA(ISPEC,J)*(EXDA-EXA)
            CKAE  = CKAE+U*DAE+UE*DA
            CKCE  = CKCE+U*BATCE+UE*BATEC
            CKPE  = CKPE+U*BATPE+UE*BATEP
          END IF  
        END IF
!  
        DAETA(KC,J) = OLDA(KC)*(GCE(1)+GCE(2))+OLDAE(KC)*(GC(1)+GC(2))      &
                     +OLDA(KP)*(HCE(1)+HCE(2))+OLDAE(KP)*(HC(1)+HC(2))+CKCE
        DAETA(KP,J) = OLDA(KC)*(GPE(1)+GPE(2))+OLDAE(KC)*(GP(1)+GP(2))      &
                     +OLDA(KP)*(HPE(1)+HPE(2))+OLDAE(KP)*(HP(1)+HP(2))+CKPE
!
        IF (SV(KA) /= 0) THEN 
          DAETA(KC,J) = DAETA(KC,J)+OLDA(KA)*(FCCE+FCE(1)+FCE(2))           &
                       +OLDAE(KA)*(FCC+FC(1)+FC(2))
          DAETA(KP,J) = DAETA(KP,J)+OLDA(KA)*(FPPE+FPE(1)+FPE(2))           &
                       +OLDAE(KA)*(FPP+FP(1)+FP(2))
          DAETA(KA,J) = OLDA(KA)*FAE+OLDAE(KA)*FA+CKAE
        END IF  
!
        IF (SV(IOUT) /= 0) THEN 
          DAETA(IOUT,J) = OLDAE(IOUT)+OLDAE(KC)+OLDAE(KP)+OLDAE(KA)-DAETA(KA,J) &
                         -DAETA(KC,J)-DAETA(KP,J)+TOTRE*DT+TOTR*ADTE(J)
          IF (ISPEC /= 0) DAETA(IOUT,J) = DAETA(IOUT,J)+IRA(ISPEC)*IDEA(ISPEC,J)&
                                         +IREA(ISPEC,J)*IDA(ISPEC)
        END IF
!  
        IF (.NOT. SECOND) CYCLE
!
! Second derivatives & first save first derivatives
        A10E1(J)=A10E; ZE1(J)=ZE; AAE1(J)=AAE
        A12E1(J)=A12E; YE1(J)=YE; D10E1(J)=D10E
        A21E1(J)=A21E; SE1(J)=SE; TEMPE1(J)=TEMPE
!
        DO I=1,2
          RTE1(J,I)=RTE(I)
          DRE1(J,I)=DRE(I)
          EXE1(J,I)=EXE(I)
          IF (ISPEC /= 0) EXDE1(J,I)=EXDE(I)
          A21RE1(J,I)=A21RE(I)
          GRE1(J,I)=GRE(I)
        END DO
!
        FORALL(I=1:4) OLDAE1(J,I)=OLDAE(I)
        TOTRE1(J)=TOTRE
!
        DO I=1,2
          DADSE1(J,I)=DADSE(I); DDIFE1(J,I)=DDIFE(I); TCE1(J,I)=TCE(I)
          DADFE1(J,I)=DADFE(I); DEDFE1(J,I)=DEDFE(I); TPE1(J,I)=TPE(I)
!          
          IF (SV(KA) /= 0) THEN
            DDFAE1(J,I)=DDFAE(I)
            AADDE1(J,I)=AADDE(I)
          END IF
          GCE1(J,I)=GCE(I)
          FCE1(J,I)=FCE(I)
          HCE1(J,I)=HCE(I)
          IF (TOTR /= ZERO) THEN
            T1E1(J,I)=T1E(I)
            T4E1(J,I)=T4E(I)
          END IF
          IF (TERMBR /= 4) THEN
            T2E1(J,I)=T2E(I); T5E1(J,I)=T5E(I)
            T3E1(J,I)=T3E(I); T6E1(J,I)=T6E(I)
          END IF
          GPE1(J,I)=GPE(I); FPE1(J,I)=FPE(I); HPE1(J,I)=HPE(I)
        END DO
!
        IF (SV(KA) /= 0) THEN
          EXAE1(J)=EXAE
          DAE1(J)=DAE; BATCE1(J)=BATCE; FCCE1(J)=FCCE
          FAE1(J)=FAE; BATPE1(J)=BATPE; FPPE1(J)=FPPE
          IF (TERMBR == 1) THEN
            EXDAE1(J)=EXDAE
            UE1(J)=UE
          END IF
        END IF 
! End of saving first derivatives
!
        DO K=1,J
          A10E2 = GG(ITSC,1)*G2(IKE,J,K)+GG(ITSC,J+1)*GG(IKE,K+1)   &
                 +G2(ITSC,J,K)*GG(IKE,1)+GG(ITSC,K+1)*GG(IKE,J+1)
          A12E2 = GG(ITSC,1)*G2(IK12,J,K)+GG(ITSC,J+1)*GG(IK12,K+1) &
                 +G2(ITSC,J,K)*GG(IK12,1)+GG(ITSC,K+1)*GG(IK12,J+1)
          A21E2 = GG(ITSC,1)*G2(IK21,J,K)+GG(ITSC,J+1)*GG(IK21,K+1) &
                 +G2(ITSC,J,K)*GG(IK21,1)+GG(ITSC,K+1)*GG(IK21,J+1)
          AAE2  = GG(ITSC,1)*G2(IKA,J,K)+GG(ITSC,J+1)*GG(IKA,K+1)   &
                 +G2(ITSC,J,K)*GG(IKA,1)+GG(ITSC,K+1)*GG(IKA,J+1)
          D10E2 = (-D10E*A10E1(K)-D10*A10E2-D10E1(K)*A10E)/A10
          GE2 = A12E2+A10E2
          ZE2 = (A12E2-ZE*A21E1(K)-Z*A21E2 -ZE1(K)*A21E)/A21
          YE2 = (GE2-YE*A21E1(K)-Y*A21E2 -YE1(K)*A21E)/A21
          SE2 = A10E2+A12E2+A21E2
          PE2 = A21*A10E2+A21E1(K)*A10E+A21E2*A10+A21E*A10E1(K)
          TEMPE2 = (TWO*SE*SE1(K)+TWO*S*SE2-FOUR*PE2)/(TWO*TEMP)    &
                   -TEMPE1(K)/(TWO*TEMP)*TWO*TEMPE
          RTE2(1) = (SE2+TEMPE2)/TWO
          RTE2(2) = (SE2-TEMPE2)/TWO
!
! Common terms and expressions
          DO I=1,2
            DRE2(I) =(-DR(I)*RTE2(I)-DRE(I)*RTE1(K,I)-DRE1(K,I)*RTE(I))/RT(I)
            EXE2(I) = -EXE(I)*DT*RTE1(K,I)-EX(I)*ADTE(J)*RTE1(K,I)          &
                      -EX(I)*DT*RTE2(I)-EXE(I)*ADTE(K)*RT(I)                &
                      -EX(I)*D2ADTE(J,K)*RT(I)-EX(I)*ADTE(K)*RTE(I)
!
            IF (ISPEC /= 0) THEN
              EXDE2(I) = -EXDE1(K,I)*((DT-IDA(ISPEC))*RTE(I)+RT(I)*(ADTE(J) &
                         -IDEA(ISPEC,J)))-EXD(I)*((ADTE(K)                  &
                         -IDEA(ISPEC,K))*RTE(I)+RTE1(K,I)*(ADTE(J)          &
                         -IDEA(ISPEC,J)))-EXD(I)*((DT-IDA(ISPEC))*RTE2(I)   &
                         +RT(I)*(D2ADTE(J,K)-I2DEA(ISPEC,J,K)))
            END IF
!
            A21RE2(I)=A21E2-RTE2(I)
            GRE2(I)=GE2-RTE2(I)
          END DO
!
! Save old derivatives
          FORALL(I=1:4) OLDAE2(I)=D2AETA(I,J,K)
          TOTRE2 = R2E(KA,J,K)+R2E(KC,J,K)+R2E(KP,J,K)
          CKCE2=ZERO
          CKPE2=ZERO
!
          IF (TOTR /= ZERO) THEN 
            CKCE2 = (RE(KP,K)+RE(KC,K))*D10E+(R(KP)+R(KC))*D10E2    &
                   +(R2E(KP,J,K)+R2E(KC,J,K))*D10+(RE(KP,J)         &
                   +RE(KC,J))*D10E1(K)
            CKPE2 = (RE(KP,K)*Y+R(KP)*YE1(K)+RE(KC,K)*Z             &
                    +R(KC)*ZE1(K))*D10E+(R(KP)*Y+R(KC)*Z)*D10E2     &
                    +(RE(KP,K)*YE+R2E(KP,J,K)*Y+RE(KC,K)*ZE         &
                    +R2E(KC,J,K)*Z)*D10+(R(KP)*YE2+RE(KP,J)*YE1(K)  &
                    +R(KC)*ZE2+RE(KC,J)*ZE1(K))*D10+(R(KP)*YE       &
                    +RE(KP,J)*Y+R(KC)*ZE+RE(KC,J)*Z)*D10E1(K)
          END IF        
!
          N=3
          DO I=1,2           ! Loop on ALPHA, BETA
            N=N-1
            DDIFE2(I) =-(RTE2(N)-RTE2(I))*DDIF(I)*DDIF(I)           &
                       -TWO*(RTE1(K,N)-RTE1(K,I))*DDIFE(I)*DDIF(I)
            DEDFE2(I) = D10E*DDIFE1(K,I)+D10*DDIFE2(I)+D10E2*DDIF(I)&
                       +D10E1(K)*DDIFE(I)
            DADFE2(I) = DRE(I)*DDIFE1(K,I)+DR(I)*DDIFE2(I)          &
                       +DRE2(I)*DDIF(I)+DRE1(K,I)*DDIFE(I)
            DADSE2(I) = DADFE(I)*DDIFE1(K,I)+DADIF(I)*DDIFE2(I)     &
                       +DADFE2(I)*DDIF(I)+DADFE1(K,I)*DDIFE(I)
            AADDE2(I) = ZERO
!
            IF (SV(KA) /= 0) THEN 
              DDFAE2(I) =-TWO*DDIFA(I)*DDFAE1(K,I)*(AAE-RTE(I))     &
                         -DDIFA(I)*DDIFA(I)*(AAE2-RTE2(I))
              AADDE2(I) = AAE2*DDIFA(I)+AAE*DDFAE1(K,I)             &
                         +AAE1(K)*DDFAE(I)+AA*DDFAE2(I)
            END IF  
!
! Central compartment
            GCE2(I) = A21RE1(K,I)*DDIF(I)*EXE(I)+A21R(I)*DDIFE1(K,I)*EXE(I) &
                      +A21R(I)*DDIF(I)*EXE2(I)+A21RE1(K,I)*DDIFE(I)*EX(I)   &
                      +A21R(I)*DDIFE2(I)*EX(I)+A21R(I)*DDIFE(I)*EXE1(K,I)   &
                      +A21RE2(I)*DDIF(I)*EX(I)+A21RE(I)*DDIFE1(K,I)*EX(I)   &
                      +A21RE(I)*DDIF(I)*EXE1(K,I)
            FCE2(I) = GCE2(I)*AADD(I)+GCE(I)*AADDE1(K,I)+GCE1(K,I)*AADDE(I) &
                     +GC(I)*AADDE2(I)
            HCE2(I) = A21E1(K)*DDIF(I)*EXE(I)+A21*DDIFE1(K,I)*EXE(I)        &
                     +A21*DDIF(I)*EXE2(I)+A21E1(K)*DDIFE(I)*EX(I)           &
                     +A21*DDIFE2(I)*EX(I)+A21*DDIFE(I)*EXE1(K,I)            &
                     +A21E2*DDIF(I)*EX(I)+A21E*DDIFE1(K,I)*EX(I)            &
                     +A21E*DDIF(I)*EXE1(K,I)
!
            IF (TOTR /= ZERO) THEN 
              T1E2(I) = RE(KC,K)*A21RE(I)+R(KC)*A21RE2(I)+R2E(KC,J,K)*A21R(I)       &
                       +RE(KC,J)*A21RE1(K,I)+RE(KP,K)*A21E+R(KP)*A21E2              &
                       +R2E(KP,J,K)*A21+RE(KP,J)*A21E1(K)+RE(KA,K)*A21R(I)*AADDE(I) &
                       +R(KA)*A21RE1(K,I)*AADDE(I)+R(KA)*A21R(I)*AADDE2(I)          &
                       +R2E(KA,J,K)*A21R(I)*AADD(I)+RE(KA,J)*A21RE1(K,I)*AADD(I)    &
                       +RE(KA,J)*A21R(I)*AADDE1(K,I)+RE(KA,K)*A21RE(I)*AADD(I)      &
                       +R(KA)*A21RE2(I)*AADD(I)+R(KA)*A21RE(I)*AADDE1(K,I)
              CKCE2   = CKCE2-T1E1(K,I)*DADIF(I)*EXE(I)-T1(I)*DADFE1(K,I)*EXE(I)    &
                       -T1(I)*DADIF(I)*EXE2(I)-T1E1(K,I)*DADFE(I)*EX(I)             &
                       -T1(I)*DADFE2(I)*EX(I)-T1(I)*DADFE(I)*EXE1(K,I)              &
                       -T1E2(I)*DADIF(I)*EX(I)-T1E(I)*DADFE1(K,I)*EX(I)             &
                       -T1E(I)*DADIF(I)*EXE1(K,I)
            END IF        
!
            ITEMP=0
            SELECT CASE (TERMBR)
            CASE (1)
              T2E2(I) =-(A21R(I)*A21R(I)+A12*A21)*AADDE2(I)                         &
                       -(A21RE1(K,I)*A21R(I)+A12E1(K)*A21)*AADDE(I)                 &
                       -(A21R(I)*A21RE1(K,I)+A12*A21E1(K))*AADDE(I)                 &
                       -(TWO*A21R(I)*A21RE(I)+A12*A21E+A12E*A21)*AADDE1(K,I)        &
                       -(TWO*A21RE1(K,I)*A21RE(I)+A12E1(K)*A21E+A12E2*A21)*AADD(I)  &
                       -(TWO*A21R(I)*A21RE2(I)+A12*A21E2+A12E*A21E1(K))*AADD(I)
              T3E2(I) = A21RE2(I)+A12E2+A21RE1(K,I)*A10*DDFAE(I)                    &
                       +A21R(I)*A10E1(K)*DDFAE(I)+A21R(I)*A10*DDFAE2(I)             &
                       +A21RE1(K,I)*A10E*DDIFA(I)+A21RE2(I)*A10*DDIFA(I)            &
                       +A21R(I)*A10E2*DDIFA(I)+A21RE(I)*A10E1(K)*DDIFA(I)           &
                       +A21R(I)*A10E*DDFAE1(K,I)+A21RE(I)*A10*DDFAE1(K,I)
            CASE (2)
              T2E2(I) =-TWO*A21RE1(K,I)*A21RE(I)-TWO*A21R(I)*A21RE2(I)              &
                       -A12E1(K)*A21E-A12*A21E2-A12E2*A21-A12E*A21E1(K)
              T3E2(I) = A12E2+A21RE2(I)
            CASE (3)
              T2E2(I) =-A21RE1(K,I)*A21E-A21R(I)*A21E2-A21RE2(I)*A21                &
                       -A21RE(I)*A21E1(K)-GRE1(K,I)*A21E-GR(I)*A21E2-GRE2(I)*A21    &
                       -GRE(I)*A21E1(K)
              T3E2(I) = GE2+A21RE2(I)
            CASE (4)
              ITEMP=1
            END SELECT
!
            IF (ITEMP == 0) THEN 
              TCE2(I) = T2E1(K,I)*DADS(I)*EXE(I)+T2(I)*DADSE1(K,I)*EXE(I)     &
                       +T2(I)*DADS(I)*EXE2(I)+T2E1(K,I)*DADSE(I)*EX(I)        &
                       +T2(I)*DADSE2(I)*EX(I)+T2(I)*DADSE(I)*EXE1(K,I)        &
                       +T2E2(I)*DADS(I)*EX(I)+T2E(I)*DADSE1(K,I)*EX(I)        &
                       +T2E(I)*DADS(I)*EXE1(K,I)+T3E1(K,I)*DEDIF(I)*EXDE(I)   &
                       +T3(I)*DEDFE1(K,I)*EXDE(I)+T3(I)*DEDIF(I)*EXDE2(I)     &
                       +T3E1(K,I)*DEDFE(I)*EXD(I)+T3(I)*DEDFE2(I)*EXD(I)      &
                       +T3(I)*DEDFE(I)*EXDE1(K,I)+T3E2(I)*DEDIF(I)*EXD(I)     &
                       +T3E(I)*DEDFE1(K,I)*EXD(I)+T3E(I)*DEDIF(I)*EXDE1(K,I)
!
              CKCE2 = CKCE2+I2REA(ISPEC,J,K)*TC(I)+IREA(ISPEC,J)*TCE1(K,I)    &
                     +IREA(ISPEC,K)*TCE(I)+IRA(ISPEC)*TCE2(I)
            END IF       
!
! Periperal compartment
            GPE2(I)= A12E1(K)*DDIF(I)*EXE(I)+A12*DDIFE1(K,I)*EXE(I)           &
                    +A12*DDIF(I)*EXE2(I)+A12E1(K)*DDIFE(I)*EX(I)              &
                    +A12*DDIFE2(I)*EX(I)+A12*DDIFE(I)*EXE1(K,I)               &
                    +A12E2*DDIF(I)*EX(I)+A12E*DDIFE1(K,I)*EX(I)               &
                    +A12E*DDIF(I)*EXE1(K,I)
            FPE2(I) = GPE1(K,I)*AADDE(I)+ GP(I)*AADDE2(I)+GPE2(I)*AADD(I)     &
                     +GPE(I)*AADDE1(K,I)
            HPE2(I) = GRE1(K,I)*DDIF(I)*EXE(I)+GR(I)*DDIFE1(K,I)*EXE(I)       &
                     +GR(I)*DDIF(I)*EXE2(I)+GRE1(K,I)*DDIFE(I)*EX(I)          &
                     +GR(I)*DDIFE2(I)*EX(I)+GR(I)*DDIFE(I)*EXE1(K,I)          &
                     +GRE2(I)*DDIF(I)*EX(I)+GRE(I)*DDIFE1(K,I)*EX(I)          &
                     +GRE(I)*DDIF(I)*EXE1(K,I)
!
            IF (TOTR /= ZERO) THEN 
              T4E2(I) = RE(KC,K)*A12E+R(KC)*A12E2+R2E(KC,J,K)*A12             &
                       +RE(KC,J)*A12E1(K)+RE(KP,K)*GRE(I)+R(KP)*GRE2(I)       &
                       +R2E(KP,J,K)*GR(I)+RE(KP,J)*GRE1(K,I)                  &
                       +RE(KA,K)*A12*AADDE(I)+R(KA)*A12E1(K)*AADDE(I)         &
                       +R(KA)*A12*AADDE2(I)+RE(KA,K)*A12E*AADD(I)             &
                       +R(KA)*A12E2*AADD(I)+R(KA)*A12E*AADDE1(K,I)            &
                       +R2E(KA,J,K)*A12*AADD(I)+RE(KA,J)*A12E1(K)*AADD(I)     &
                       +RE(KA,J)*A12*AADDE1(K,I)
              CKPE2 = CKPE2-T4E1(K,I)*DADIF(I)*EXE(I)-T4(I)*DADFE1(K,I)*EXE(I)&
                     -T4(I)*DADIF(I)*EXE2(I)-T4E1(K,I)*DADFE(I)*EX(I)         &
                     -T4(I)*DADFE2(I)*EX(I)-T4(I)*DADFE(I)*EXE1(K,I)          &
                     -T4E2(I)*DADIF(I)*EX(I)-T4E(I)*DADFE1(K,I)*EX(I)         &
                     -T4E(I)*DADIF(I)*EXE1(K,I)
            END IF
!        
            SELECT CASE ( TERMBR )
            CASE (1)
              T5E2(I) =-A12E1(K)*(A21R(I)+GR(I))*AADDE(I)                     &
                       -A12*(A21RE1(K,I)+GRE1(K,I))*AADDE(I)                  &
                       -A12*(A21R(I)+GR(I))*AADDE2(I)-A12E1(K)*(A21RE(I)      &
                       +GRE(I))*AADD(I)-A12*(A21RE2(I)+GRE2(I))*AADD(I)       &
                       -A12*(A21RE(I)+GRE(I))*AADDE1(K,I)-A12E2*(A21R(I)      &
                       +GR(I))*AADD(I)-A12E*(A21RE1(K,I)+GRE1(K,I))*AADD(I)   &
                       -A12E*(A21R(I)+GR(I))*AADDE1(K,I)
              T6E2(I) = A12E2+GRE1(K,I)*ZE+GR(I)*ZE2+GRE2(I)*Z+GRE(I)*ZE1(K)  &
                       +A12E1(K)*A10*DDFAE(I)+A12*A10E1(K)*DDFAE(I)           &
                       +A12*A10*DDFAE2(I)+(A12*A10E+A12E*A10)*DDFAE1(K,I)     &
                       +(A12E1(K)*A10E+A12E2*A10)*DDIFA(I)+(A12*A10E2         &
                       +A12E*A10E1(K))*DDIFA(I)
            CASE (2)
              T5E2(I) =-(GRE1(K,I)+A21RE1(K,I))*A12E-(GR(I)+A21R(I))*A12E2    &
                       -(GRE2(I)+A21RE2(I))*A12-(GRE(I)+A21RE(I))*A12E1(K)
              T6E2(I) = A12E2+ZE1(K)*GRE(I)+Z*GRE2(I)+ZE2*GR(I)+ZE*GRE1(K,I)
            CASE (3)
              T5E2(I) =-(TWO*GRE1(K,I)*GRE(I)+A12E1(K)*A21E+A12E2*A21)        &
                       -(TWO*GR(I)*GRE2(I)+A12*A21E2+A12E*A21E1(K))
              T6E2(I) = A12E2+Y*GRE2(I)+YE1(K)*GRE(I)+YE2*GR(I)+YE*GRE1(K,I)
            CASE (4)
              CYCLE 
            END SELECT
!
            TPE2(I) = T5E1(K,I)*DADS(I)*EXE(I)+T5(I)*DADSE1(K,I)*EXE(I)       &
                     +T5(I)*DADS(I)*EXE2(I)+T5E1(K,I)*DADSE(I)*EX(I)          &
                     +T5(I)*DADSE2(I)*EX(I)+T5(I)*DADSE(I)*EXE1(K,I)          &
                     +T5E2(I)*DADS(I)*EX(I)+T5E(I)*DADSE1(K,I)*EX(I)          &
                     +T5E(I)*DADS(I)*EXE1(K,I)+T6E1(K,I)*DEDIF(I)*EXDE(I)     &
                     +T6(I)*DEDFE1(K,I)*EXDE(I)+T6(I)*DEDIF(I)*EXDE2(I)       &
                     +T6E1(K,I)*DEDFE(I)*EXD(I)+T6(I)*DEDFE2(I)*EXD(I)        &
                     +T6(I)*DEDFE(I)*EXDE1(K,I)+T6E2(I)*DEDIF(I)*EXD(I)       &
                     +T6E(I)*DEDFE1(K,I)*EXD(I)+T6E(I)*DEDIF(I)*EXDE1(K,I)
!
            CKPE2 = CKPE2+I2REA(ISPEC,J,K)*TP(I)+IREA(ISPEC,J)*TPE1(K,I)      &
                   +IREA(ISPEC,K)*TPE(I)+IRA(ISPEC)*TPE2(I)
!
          END DO             ! End of loop on ALPHA, BETA
!
          IF (SV(KA) /= 0) THEN 
!
! Absorption compartment
            EXAE2 = -EXAE1(K)*(DT*AAE+ADTE(J)*AA) -EXA*(ADTE(K)*AAE+D2ADTE(J,K)*AA) &
                    -EXA*(DT*AAE2+ADTE(J)*AAE1(K))
            DAE2  = -DA*DA*AAE2-TWO*DA*DAE1(K)*AAE
            FAE2  = EXAE2
            CKAE2 = ZERO
            BATCE2 = DDFAE1(K,1)*DDIFA(2)*(A21E-AAE)+DDIFA(1)*DDFAE1(K,2)*(A21E-AAE)&
                    +DDIFA(1)*DDIFA(2)*(A21E2-AAE2)+(DDFAE1(K,1)*DDFAE(2)           &
                    +DDFAE2(1)*DDIFA(2))*(A21-AA)+(DDIFA(1)*DDFAE2(2)               &
                    +DDFAE(1)*DDFAE1(K,2))*(A21-AA)+(DDIFA(1)*DDFAE(2)              &
                    +DDFAE(1)*DDIFA(2))*(A21E1(K)-AAE1(K))
            BATPE2 = DDFAE1(K,1)*DDIFA(2)*A12E+DDIFA(1)*DDFAE1(K,2)*A12E            &
                    +DDIFA(1)*DDIFA(2)*A12E2 +(DDFAE1(K,1)*DDFAE(2)                 &
                    +DDFAE2(1)*DDIFA(2))*A12+(DDIFA(1)*DDFAE2(2)                    &
                    +DDFAE(1)*DDFAE1(K,2))*A12+(DDIFA(1)*DDFAE(2)                   &
                    +DDFAE(1)*DDIFA(2))*A12E1(K)
            FCCE2  = AAE1(K)*BATEC*EXAE+AA*BATCE1(K)*EXAE+AA*BATEC*EXAE2            &
                    +AAE1(K)*BATCE*EXA+AA*BATCE2*EXA+AA*BATCE*EXAE1(K)              &
                    +AAE2*BATEC*EXA+AAE*BATCE1(K)*EXA+AAE*BATEC*EXAE1(K)
            FPPE2  = AAE1(K)*BATEP*EXAE+AA*BATPE1(K)*EXAE+AA*BATEP*EXAE2            &
                    +AAE1(K)*BATPE*EXA+AA*BATPE2*EXA+AA*BATPE*EXAE1(K)              &
                    +AAE2*BATEP*EXA+AAE*BATPE1(K)*EXA+AAE*BATEP*EXAE1(K)
!
            IF (R(KA) /= ZERO) THEN 
              CKAE2 = RE(KA,K)*(ONE-EXA)*DAE-R(KA)*EXAE1(K)*DAE                     &
                     +R(KA)*(ONE-EXA)*DAE2-RE(KA,K)*EXAE*DA-R(KA)*EXAE2*DA          &
                     -R(KA)*EXAE*DAE1(K)+R2E(KA,J,K)*(ONE-EXA)*DA                   &
                     -RE(KA,J)*EXAE1(K)*DA+RE(KA,J)*(ONE-EXA)*DAE1(K)
              CKCE2 = CKCE2+RE(KA,K)*(D10E-BATEC*EXAE-BATCE*EXA)                    &
                     +R(KA)*(D10E2-BATCE1(K)*EXAE-BATCE2*EXA -BATEC*EXAE2           &
                     -BATCE*EXAE1(K))+R2E(KA,J,K)*(D10-BATEC*EXA)+RE(KA,J)*(D10E1(K)&
                     -BATCE1(K)*EXA-BATEC*EXAE1(K))
              CKPE2 = CKPE2+RE(KA,K)*(Z*D10E+ZE*D10-BATEP*EXAE-BATPE*EXA)           &
                     +R(KA)*(ZE1(K)*D10E+ZE2*D10-BATPE1(K)*EXAE-BATPE2*EXA)         &
                     +R(KA)*(Z*D10E2+ZE*D10E1(K)-BATEP*EXAE2-BATPE*EXAE1(K))        &
                     +R2E(KA,J,K)*(Z*D10-BATEP*EXA) +RE(KA,J)*(ZE1(K)*D10           &
                     -BATPE1(K)*EXA)+RE(KA,J)*(Z*D10E1(K)-BATEP*EXAE1(K))
            END IF
!      
            IF (TERMBR == 1) THEN 
              TDELE2 = (D2ADTE(J,K)-I2DEA(ISPEC,J,K))*AA+(ADTE(J)                   &
                      -IDEA(ISPEC,J))*AAE1(K)+(ADTE(K)-IDEA(ISPEC,K))*AAE           &
                      +(DT-IDA(ISPEC))*AAE2                 
              EXDAE2 =-EXDAE1(K)*TDELE-EXDA*TDELE2
              UE2    = IREA(ISPEC,K)*(EXDAE-EXAE)+I2REA(ISPEC,J,K)*(EXDA-EXA)       &
                      +IRA(ISPEC)*(EXDAE2-EXAE2)+IREA(ISPEC,J)*(EXDAE1(K)-EXAE1(K))
              CKAE2 = CKAE2+UE1(K)*DAE+UE2*DA+U*DAE2+UE*DAE1(K)
              CKCE2 = CKCE2+UE1(K)*BATCE+UE2*BATEC+U*BATCE2+UE*BATCE1(K)
              CKPE2 = CKPE2+UE1(K)*BATPE+UE2*BATEP+U*BATPE2+UE*BATPE1(K)
            END IF  
          END IF  
!
          D2AETA(KC,J,K) = OLDAE1(K,KC)*(GCE(1)+GCE(2))                             &
                          +OLDA(KC)*(GCE2(1)+GCE2(2))+OLDAE2(KC)*(GC(1)+GC(2))      &
                          +OLDAE(KC)*(GCE1(K,1)+GCE1(K,2))+OLDAE1(K,KP)*(HCE(1)     &
                          +HCE(2))+OLDA(KP)*(HCE2(1)+HCE2(2))+OLDAE2(KP)*(HC(1)     &
                          +HC(2))+OLDAE(KP)*(HCE1(K,1)+HCE1(K,2))+CKCE2 
          D2AETA(KP,J,K)= OLDAE1(K,KC)*(GPE(1)+GPE(2))+OLDA(KC)*(GPE2(1)+GPE2(2))   &
                          +OLDAE2(KC)*(GP(1)+GP(2))+OLDAE(KC)*(GPE1(K,1)+GPE1(K,2)) &
                          +OLDAE1(K,KP)*(HPE(1)+HPE(2))+OLDA(KP)*(HPE2(1)+HPE2(2))  &
                          +OLDAE2(KP)*(HP(1)+HP(2))+OLDAE(KP)*(HPE1(K,1)+HPE1(K,2)) &
                          +CKPE2
!
          IF (SV(KA) /= 0) THEN 
            D2AETA(KC,J,K) = D2AETA(KC,J,K)+OLDAE1(K,KA)*(FCCE+FCE(1)+FCE(2))       &
                            +OLDA(KA)*(FCCE2+FCE2(1)+FCE2(2))+OLDAE2(KA)*(FCC+FC(1) &
                            +FC(2))+OLDAE(KA)*(FCCE1(K)+FCE1(K,1)+FCE1(K,2))
            D2AETA(KP,J,K) = D2AETA(KP,J,K)+OLDAE1(K,KA)*(FPPE+FPE(1)+FPE(2))       &
                            +OLDA(KA)*(FPPE2+FPE2(1)+FPE2(2))+OLDAE2(KA)*(FPP+FP(1) &
                            +FP(2))+OLDAE(KA)*(FPPE1(K)+FPE1(K,1)+FPE1(K,2))
            D2AETA(KA,J,K) = OLDAE1(K,KA)*FAE+OLDA(KA)*FAE2+OLDAE2(KA)*FA           &
                            +OLDAE(KA)*FAE1(K)+CKAE2
          END IF  
!
          IF (SV(IOUT) /= 0) THEN 
            D2AETA(IOUT,J,K) = OLDAE2(IOUT)+OLDAE2(KC)+OLDAE2(KP)+OLDAE2(KA)        &
                              -D2AETA(KA,J,K)-D2AETA(KC,J,K)-D2AETA(KP,J,K)         &
                              +TOTRE2*DT+TOTRE1(K)*ADTE(J)+TOTRE*ADTE(K)            &
                              +TOTR*D2ADTE(J,K)
            IF (ISPEC /= 0) THEN 
              D2AETA(IOUT,J,K) = D2AETA(IOUT,J,K)+IREA(ISPEC,K)*IDEA(ISPEC,J)       &
                                +I2REA(ISPEC,J,K)*IDA(ISPEC)+IRA(ISPEC)             &
                                *I2DEA(ISPEC,J,K)+IREA(ISPEC,J)*IDEA(ISPEC,K)
            END IF    
          END IF   
        END DO
      END DO ! End of loop over NPETAS
!
   10 FORMAT (' TWO COMPARTMENT MODEL WITH FIRST-ORDER ABSORPTION (ADVAN4)')
   15 FORMAT ('0MAXIMUM NO. OF BASIC PK PARAMETERS:',I4)
   20 FORMAT ('0BASIC PK PARAMETERS (AFTER TRANSLATION):',                          &
             /'   BASIC PK PARAMETER NO.',I3,': ELIMINATION RATE (K)',              &
             /'   BASIC PK PARAMETER NO.',I3,': CENTRAL-TO-PERIPH. RATE (K23)',     &
             /'   BASIC PK PARAMETER NO.',I3,': PERIPH.-TO-CENTRAL RATE (K32)',     &
             /'   BASIC PK PARAMETER NO.',I3,': ABSORPTION RATE (KA)')
!
  999 RETURN
!
  502 CALL ERRORMSGS(502,FILENAME='OUTPUT')
!
      END SUBROUTINE ADVAN
