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
! CREATED ON  : FEB/1984
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
!----------------------------- ADVAN2.F90 -------------------------------------------
!
! SUBROUTINE ADVAN(ICALL)
!
! DESCRIPTION : ADVAN2 is a routine in PREDPP’s library which implements the kinetic
!               equations for the One Compartment Linear Model with First Order
!               Absorption. Opens 1 compartment model with absorption and with urine
!               compartment
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
! ALGORITHM   : - If(ICALL=2) Normal Entry; Else
!                 - Set MODEL definition variables
!                 - Internally, central compartment is no. 2, drug depot is no. 1
!                 - Define PK parameters
!                 - If error in output, call ERRORMSGS at label 502 and RETURN
!                 - If SECOND is true, initialize variables and RETURN
!               - Normal Entry
!                 - Initialize variables
!                 - Process for CENTRAL compartment
!                 - Check for errors in basic PK parameters
!                   - If(PK parameter for time scale or rate constant is not positive)
!                     RETURN ERROR code 1
!                 - Process for ABSORPTION compartment
!                 - Check for errors in basic PK parameters
!                   - If(PK parameter for time scale or KA is not positive, or KA=K)
!                     RETURN ERROR code 1
!                 - Loop (J) over 1,NPETAS
!                   - Calculate first derivatives for central compartment
!                   - Calculate first derivatives for absorption compartment
!                   - From absorption compartment to central compartment BATEMAN
!                     function's BETA
!                   - Save first derivatives
!                   - Loop (K) over 1,J
!                       - Calculate second derivatives for central compartment
!                       - Calculate second derivatives for absorption compartment
!                   - End loop (K)
!                 - End loop (J)
!
! MODULES USED: PRSIZES,PRDATA,NMPRD_INT,PRCOM_INT,PRMOD_INT,PROCM_INT,PRCOM_REAL,
!               PROCM_REAL,NMPRD_CHAR,PRMOD_CHAR,PRCOM_LOG,NM_INTERFACE
!
! CONTAINS    : NONE
!
! LOCAL'S     : A1,A2,BATE,D2A1,D2A2,D2BATE,D2DB,D2DEL1,D2DEL2,D2DR,D2DS,D2ETD1,
!               D2ETD2,D2EX1,D2EX2,D2K1T,D2K2T,D2OLD1,D2OLD2,D2OME1,D2OME2,D2SDT,
!               D2SRR,D2T1,D2T12,D2T2,D2TDEL,D2TR1,D2TR12,D2TR2,D2TS1,D2TS12,D2TS2,
!               D2U1,D2U12,D2U2,D2V1,D2V12,D2V2,DA1,DA1E,DA2,DA2E,DB,DBATE,DBATEE,
!               DDB,DDBE,DDEL1,DDEL1E,DDEL2,DDEL2E,DDR,DDRE,DDS,DDSE,DEL1,DEL2,DETD1,
!               DETD2,DEX1,DEX1E,DEX2,DEX2E,DK1T,DK1TE,DK2T,DK2TE,DOLD1,DOLD1E,DOLD2,
!               DOLD2E,DOME1,DOME1E,DOME2,DOME2E,DR,DS,DSDT,DSRR,DSRRE,DT1,DT12,DT2,
!               DTDEL,DTDELE,DTR1,DTR12,DTR2,DTS1,DTS12,DTS2,DU1,DU12,DU12E,DU1E,
!               DU2,DU2E,DV1,DV12,DV12E,DV1E,DV2,DV2E,ETD1,ETD2,EX1,EX2,G2,GG,I,J,
!               JC,K,K1T,K2T,OLD1,OLD2,OME1,OME2,PE,SDT,SRR,T1,T12,T2,TDEL,TR1,
!               TR12,TR2,TS1,TS12,TS2,U1,U12,U2,V1,V12,V2
!
!---------------------------- END OF HEADER -----------------------------------------
!
      SUBROUTINE ADVAN(ICALL)
!
      USE PRSIZES,      ONLY: ISIZE,DPSIZE,PE
!      
      USE PRDATA,       ONLY: IDENT_A2
! INTEGER
      USE NMPRD_INT,    ONLY: IERPRD
      USE PRCOM_INT,    ONLY: ITSC,BETA,ADVID,IDC,IDO,ISV,IINST,ITURN,SV,ISPEC,NPETAS,  &
                              LOGUNT,NC,NBP
      USE PRMOD_INT,    ONLY: IOUT,IV,IKE,IKA
      USE PROCM_INT,    ONLY: IDXETA
! REAL
      USE PRCOM_REAL,   ONLY: D2ADTE,I2DEA,I2REA,R2E,ADTE,G3,DT,IRA,IREA,IDA,IDEA,R,RE, &
                              ZERO,ONE
      USE PROCM_REAL,   ONLY: AMNT,DAETA,D2AETA
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
!     COMMON /PRNAME/ NAME
!     COMMON /PRCOMN/ LOGUNT,NC
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
! FOR INFUSIONS WHICH CONTINUE PAST DT:
! R(IC)=INFUSION RATE INTO COMPARTMENT IC
! RE(IC,K)=DERIVATIVE OF RATE WRT ETA(K)
! IF AN INFUSION IS TERMINATING, ISPEC>0 AND:
! IRA(ISPEC)=ITS INFUSION RATE
! IREA(ISPEC,K)=DERIVATIVE OF IRA WRT ETA(K)
! IDA(ISPEC)=ITS DURATION
! IDEA(ISPEC,K)=DERIVATIVE OF IDEA WRT ETA(K)
! BETA(ISPEC)=ITS COMPARTMENT NUMBER
!     COMMON /PRMON3/ IOUT,IV,IKE,IKA
!     COMMON /PROCM4/ A,DAETA,D2AETA
!     DOUBLE PRECISION A,DAETA,D2AETA
!     DIMENSION A(PC),DAETA(PC,PE),D2AETA(PC,PE,PE)
! MAP ACTIVE ETAS: M(K) IS "REAL" INDEX OF K-TH. ACTIVE ETA
!     COMMON /PROCM5/ NACTIV,M(0:PE)
!     INTEGER NACTIV,M
! 
!------------------------------------------------------------------------------------
!
! Local Variables
!
      INTEGER(KIND=ISIZE) :: I,J,K,JC
!
      REAL(KIND=DPSIZE)   :: A1,A2,BATE,D2A1,D2A2,D2BATE,D2DB,D2DEL1,D2DEL2,D2DR,D2DS,   &
                             D2ETD1,D2ETD2,D2EX1,D2EX2,D2K1T,D2K2T,D2OLD1,D2OLD2,D2OME1, &
                             D2OME2,D2SDT,D2SRR,D2T1,D2T12,D2T2,D2TDEL,D2TR1,D2TR12,     &
                             D2TR2,D2TS1,D2TS12,D2TS2,D2U1,D2U12,D2U2,D2V1,D2V12,D2V2,   &
                             DA1,DA1E(PE),DA2,DA2E(PE),DB,DBATE,DBATEE(PE),DDB,DDBE(PE), &
                             DDEL1,DDEL1E(PE),DDEL2,DDEL2E(PE),DDR,DDRE(PE),DDS,DDSE(PE),&
                             DEL1,DEL2,DETD1,DETD2,DEX1,DEX1E(PE),DEX2,DEX2E(PE),DK1T,   &
                             DK1TE(PE),DK2T,DK2TE(PE),DOLD1,DOLD1E(PE),DOLD2,DOLD2E(PE), &
                             DOME1,DOME1E(PE),DOME2,DOME2E(PE),DR,DS,DSDT,DSRR,DSRRE(PE),&
                             DT1,DT12,DT2,DTDEL,DTDELE(PE),DTR1,DTR12,DTR2,DTS1,DTS12,   &
                             DTS2,DU1,DU12,DU12E(PE),DU1E(PE),DU2,DU2E(PE),DV1,DV12,     &
                             DV12E(PE),DV1E(PE),DV2,DV2E(PE),ETD1,ETD2,EX1,EX2,G2,GG,    &
                             K1T,K2T,OLD1,OLD2,OME1,OME2,SDT,SRR,T1,T12,T2,TDEL,TR1,TR12,&
                             TR2,TS1,TS12,TS2,U1,U12,U2,V1,V12,V2
!
      G2(I,J,K)=G3(I,IDXETA(J)+1,IDXETA(K)+1) ! Statement function
      GG(I,J)=G3(I,IDXETA(J-1)+1,1)
!
      IF (ICALL /= 2) THEN   ! Initialization call
        ADVID=2; NC=3        ! Set MODEL definition variables 
!
! Internally, central compartment is No. 2, drug depot is No. 1
        IDC=1; ISV(1)=0; IINST(1)=1; ITURN(1)=1; NAME(1)=IDENT_A2(1)
        IDO=2; ISV(2)=1; IINST(2)=1; ITURN(2)=0; NAME(2)=IDENT_A2(2)
        NBP=3; ISV(3)=0; IINST(3)=0; ITURN(3)=1; NAME(3)=IDENT_A2(3)
        IOUT=3
!
        IKE=1; IV=2; IKA=3   ! Define PK parameters
        WRITE (LOGUNT,10,ERR=502)
        WRITE (LOGUNT,12,ERR=502) NBP
        WRITE (LOGUNT,20,ERR=502) IKE,IKA
        WRITE (LOGUNT,35,ERR=502)
        IF (SECOND) THEN
          DU1=ZERO; DOME1=ZERO; DDEL1=ZERO; DK1T=ZERO; DU12=ZERO; DV1=ZERO
          DU2=ZERO; DOME2=ZERO; DDEL2=ZERO; DEX1=ZERO; DV12=ZERO; DV2=ZERO
          DA1=ZERO; DBATE=ZERO; DSRR=ZERO;  DDB=ZERO;  DDR=ZERO;  DDS=ZERO           
! Nous: following statement is defined twice; hence commented
!         DDEL2=ZERO 
        END IF
        GO TO 999
      END IF
!
! Normal Entry
      JC=0; SDT=ZERO; TDEL=ZERO
!
      IF (ISPEC /= 0) THEN
        JC=BETA(ISPEC)
        SDT=IDA(ISPEC)
        TDEL=DT-SDT
      END IF
!
! Central compartment
      A2=GG(ITSC,1)*GG(IKE,1)
!
      IF (A2 <= ZERO) THEN   ! Check for errors in basic PK parameters
        IERPRD=1
        IF (GG(ITSC,1) <= ZERO) THEN
          ETEXT(2)='PK PARAMETER FOR TIME SCALE IS NON-POSITIVE'
        ELSE
          ETEXT(2)='PK PARAMETER FOR K IS NON-POSITIVE'
        END IF
        GO TO 999
      END IF
!
      K2T=-A2*DT
      EX2=EXP(K2T)
      OLD2=AMNT(2)
      T2=OLD2*EX2
      AMNT(2)=T2
!
      IF (R(2) /= ZERO) THEN 
        U2=R(2)/A2
        OME2=ONE-EX2
        TR2=U2*OME2
        AMNT(2)=AMNT(2)+TR2
      END IF  
!     
      IF (JC == 2) THEN 
        V2=IRA(ISPEC)/A2
        ETD2=EXP(-A2*TDEL)
        DEL2=ETD2-EX2
        TS2=V2*DEL2
        AMNT(2)=AMNT(2)+TS2
      END IF  
!
! Absorption compartment
      OLD1=0.0
      IF (SV(1) /= 0) THEN 
        A1=GG(ITSC,1)*GG(IKA,1)
!
        IF (A1 <= ZERO) THEN   ! Check for errors in basic PK parameters
          IERPRD=1
          IF (GG(ITSC,1) <= ZERO) THEN
            ETEXT(2)='PK PARAMETER FOR TIME SCALE IS NON-POSITIVE'
          ELSE
            ETEXT(2)='PK PARAMETER FOR KA IS NON-POSITIVE'
          END IF
          GO TO 999
        END IF
!
        IF (A1 == A2) THEN
          IERPRD=1
          ETEXT(2)='PK PARAMETERS FOR KA AND K ARE EQUAL'
          GO TO 999
        END IF
!
        K1T=-A1*DT
        EX1=EXP(K1T)
        OLD1=AMNT(1)
        T1=OLD1*EX1
        AMNT(1)=T1
!
        IF (R(1) /= ZERO) THEN
          U1=R(1)/A1
          OME1=ONE-EX1
          TR1=U1*OME1
          AMNT(1)=AMNT(1)+TR1
        END IF  
!
        IF (JC == 1) THEN 
          V1=IRA(ISPEC)/A1
          ETD1=EXP(-A1*TDEL)
          DEL1=ETD1-EX1
          TS1=V1*DEL1
          AMNT(1)=AMNT(1)+TS1
        END IF  
!
! From absorption compartment to central compartment
! BATEMAN Function's BETA
        BATE=GG(IKA,1)/(GG(IKA,1)-GG(IKE,1))
        T12=OLD1*EX2
        DB=T12-T1
        AMNT(2)=AMNT(2)+BATE*DB
!
        IF (R(1) /= ZERO) THEN
          U12=R(1)/A2
          OME2=ONE-EX2
          TR12=U12*OME2
          DR=TR12-TR1
          AMNT(2)=AMNT(2)+BATE*DR
        END IF  
!
        IF (JC == 1) THEN 
          V12=IRA(ISPEC)/A2
          ETD2=EXP(-A2*TDEL)
          DEL2=ETD2-EX2
          TS12=V12*DEL2
          DS=TS12-TS1
          AMNT(2)=AMNT(2)+BATE*DS
        END IF  
      END IF  
!
      IF (SV(IOUT) /= 0) THEN
        SRR=R(1)+R(2)
        AMNT(IOUT)=AMNT(IOUT)+OLD1+OLD2-AMNT(1)-AMNT(2)+SRR*DT
        IF (ISPEC /= 0) AMNT(IOUT)=AMNT(IOUT)+IDA(ISPEC)*IRA(ISPEC)
      END IF
!
      IF (NOETAS) GO TO 999
!      
      DO J=1,NPETAS
        DSDT=ZERO
        DTDEL=ZERO
        IF (ISPEC /= 0) THEN
          DSDT=IDEA(ISPEC,J)
          DTDEL=ADTE(J)-DSDT
        END IF
        DOLD1=DAETA(1,J)
        DOLD2=DAETA(2,J)
!
! First derivatives for central compartment
        DA2=GG(ITSC,1)*GG(IKE,J+1)+GG(ITSC,J+1)*GG(IKE,1)
        DK2T=-A2*ADTE(J)-DA2*DT
        DEX2=EX2*DK2T
        DT2=OLD2*DEX2+DOLD2*EX2
        DAETA(2,J)=DT2
!
        IF (R(2) /= ZERO) THEN 
          DU2=(RE(2,J)-U2*DA2)/A2
          DOME2=-DEX2
          DTR2=U2*DOME2+DU2*OME2
          DAETA(2,J)=DAETA(2,J)+DTR2
        END IF  
!
        IF (JC == 2) THEN 
          DV2=(IREA(ISPEC,J)-V2*DA2)/A2
          DETD2=ETD2*(-A2*DTDEL-DA2*TDEL)
          DDEL2=DETD2-DEX2
          DTS2=V2*DDEL2+DV2*DEL2
          DAETA(2,J)=DAETA(2,J)+DTS2
        END IF 
! 
! First derivatives for absorption compartment
        IF (SV(1) /= 0) THEN 
          DA1=GG(ITSC,1)*GG(IKA,J+1)+GG(ITSC,J+1)*GG(IKA,1)
          DK1T=-A1*ADTE(J)-DA1*DT
          DEX1=EX1*DK1T
          DT1=OLD1*DEX1+DOLD1*EX1
          DAETA(1,J)=DT1
!
          IF (R(1) /= ZERO) THEN 
            DU1=(RE(1,J)-U1*DA1)/A1
            DOME1=-DEX1
            DTR1=U1*DOME1+DU1*OME1
            DAETA(1,J)=DAETA(1,J)+DTR1
          END IF  
!
          IF (JC == 1) THEN 
            DV1=(IREA(ISPEC,J)-V1*DA1)/A1
            DETD1=ETD1*(-A1*DTDEL-DA1*TDEL)
            DDEL1=DETD1-DEX1
            DTS1=V1*DDEL1+DV1*DEL1
            DAETA(1,J)=DAETA(1,J)+DTS1
          END IF 
!
! From absorption compartment to central compartment
! BATEMAN function's BETA
          DBATE=(GG(IKA,J+1)-BATE*(GG(IKA,J+1)-GG(IKE,J+1)))/(GG(IKA,1)-GG(IKE,1))
          DT12=DOLD1*EX2+OLD1*DEX2
          DDB=DT12-DT1
          DAETA(2,J)=DAETA(2,J)+DBATE*DB+BATE*DDB
!
          IF (R(1) /= ZERO) THEN 
            DU12=(RE(1,J)-U12*DA2)/A2
            DOME2=-DEX2
            DTR12=DU12*OME2+U12*DOME2
            DDR=DTR12-DTR1
            DAETA(2,J)=DAETA(2,J)+DBATE*DR+BATE*DDR
          END IF  
!
          IF (JC == 1) THEN 
            DV12=(IREA(ISPEC,J)-V12*DA2)/A2
            DETD2=ETD2*(-A2*DTDEL-DA2*TDEL)
            DDEL2=DETD2-DEX2
            DTS12=V12*DDEL2+DV12*DEL2
            DDS=DTS12-DTS1
            DAETA(2,J)=DAETA(2,J)+DBATE*DS+BATE*DDS
          END IF  
        END IF  
!
        IF (SV(IOUT) /= 0) THEN
          DSRR=RE(1,J)+RE(2,J)
          DAETA(IOUT,J)= DAETA(IOUT,J)+DOLD1+DOLD2-DAETA(1,J)-DAETA(2,J)+DSRR*DT &
                        +SRR*ADTE(J)
          IF (ISPEC /= 0) THEN
            DAETA(IOUT,J) = DAETA(IOUT,J)+IDEA(ISPEC,J)*IRA(ISPEC)               &
                           +IDA(ISPEC)*IREA(ISPEC,J)
          END IF
        END IF
!
        IF (.NOT. SECOND) CYCLE
!
! Second derivatives & first save first derivatives
        DOLD1E(J)=DOLD1;  DK1TE(J)=DK1T;   DA1E(J)=DA1
        DOLD2E(J)=DOLD2;  DK2TE(J)=DK2T;   DA2E(J)=DA2 
        DDEL1E(J)=DDEL1;  DEX1E(J)=DEX1;   DU1E(J)=DU1 
        DDEL2E(J)=DDEL2;  DEX2E(J)=DEX2;   DV1E(J)=DV1 
        DOME1E(J)=DOME1;  DU12E(J)=DU12;   DU2E(J)=DU2     
        DOME2E(J)=DOME2;  DV12E(J)=DV12;   DV2E(J)=DV2           
        DTDELE(J)=DTDEL;  DSRRE(J)=DSRR;   DDSE(J)=DDS  
        DDBE(J)=DDB;      DBATEE(J)=DBATE; DDRE(J)=DDR    
!
! Nous: Below statements were defined twice; hence commented
!        DDEL2E(J)=DDEL2  
!        DOME2E(J)=DOME2  
!
! End saving first derivatives
        DO K=1,J
          IF (ISPEC /= 0) THEN
            D2SDT=I2DEA(ISPEC,J,K)
            D2TDEL=D2ADTE(J,K)-D2SDT
          END IF
          D2OLD1=D2AETA(1,J,K)
          D2OLD2=D2AETA(2,J,K)
!
! Second derivatives for central compartment
          D2A2  = GG(ITSC,J+1)*GG(IKE,K+1)+GG(ITSC,1)*G2(IKE,J,K)                &
                 +GG(ITSC,K+1)*GG(IKE,J+1)+G2(ITSC,J,K)*GG(IKE,1)
          D2K2T = -DA2*ADTE(K)-A2*D2ADTE(J,K)-DA2E(K)*ADTE(J)-DT*D2A2
          D2EX2 = EX2*D2K2T+DEX2*DK2TE(K)
          D2T2  = DOLD2*DEX2E(K)+OLD2*D2EX2+D2OLD2*EX2+DOLD2E(K)*DEX2
          D2AETA(2,J,K) = D2T2
!
          IF (R(2) /= ZERO) THEN 
            D2U2   = (R2E(2,J,K)-U2*D2A2-DU2*DA2E(K)-DU2E(K)*DA2)/A2
            D2OME2 = -D2EX2
            D2TR2  = U2*D2OME2+DOME2E(K)*DU2+OME2*D2U2+DOME2*DU2E(K)
            D2AETA(2,J,K)=D2AETA(2,J,K)+D2TR2
          END IF  
!
          IF (JC == 2) THEN 
            D2V2   = (I2REA(ISPEC,J,K)-V2*D2A2-DV2*DA2E(K)-DV2E(K)*DA2)/A2
            D2ETD2 = ETD2*(-DA2*DTDELE(K)-A2*D2TDEL-DA2E(K)*DTDEL-TDEL*D2A2)     &
                    +DETD2*(-A2*DTDELE(K)-DA2E(K)*TDEL)
            D2DEL2 = D2ETD2-D2EX2
            D2TS2  = DV2*DDEL2E(K)+V2*D2DEL2+D2V2*DEL2+DV2E(K)*DDEL2
            D2AETA(2,J,K)=D2AETA(2,J,K)+D2TS2
          END IF  
!
! Second derivatives for absorption compartment
          IF (SV(1) /= 0) THEN
            D2A1  = GG(ITSC,J+1)*GG(IKA,K+1)+GG(ITSC,1)*G2(IKA,J,K)              &
                   +GG(ITSC,K+1)*GG(IKA,J+1)+G2(ITSC,J,K)*GG(IKA,1)
            D2K1T =-DA1*ADTE(K)-A1*D2ADTE(J,K)-DA1E(K)*ADTE(J)-DT*D2A1
            D2EX1 = EX1*D2K1T+DEX1*DK1TE(K)
            D2T1  = DOLD1*DEX1E(K)+OLD1*D2EX1+D2OLD1*EX1+DOLD1E(K)*DEX1
            D2AETA(1,J,K)=D2T1
!
            IF (R(1) /= ZERO) THEN 
              D2U1  = (R2E(1,J,K)-U1*D2A1-DU1*DA1E(K)-DU1E(K)*DA1)/A1
              D2OME1= -D2EX1
              D2TR1 = U1*D2OME1+DOME1E(K)*DU1+OME1*D2U1+DOME1*DU1E(K)
              D2AETA(1,J,K) = D2AETA(1,J,K)+D2TR1
            END IF  
!
            IF (JC == 1) THEN
              D2V1   = (I2REA(ISPEC,J,K)-V1*D2A1-DV1*DA1E(K)-DV1E(K)*DA1)/A1
              D2ETD1 = ETD1*(-DA1*DTDELE(K)-A1*D2TDEL-DA1E(K)*DTDEL-TDEL*D2A1)   &
                      +DETD1*(-A1*DTDELE(K)-DA1E(K)*TDEL)
              D2DEL1 = D2ETD1-D2EX1
              D2TS1  = DV1*DDEL1E(K)+V1*D2DEL1+D2V1*DEL1+DV1E(K)*DDEL1
              D2AETA(1,J,K) = D2AETA(1,J,K)+D2TS1
            END IF
          END IF  
!
! From absorption compartment to central compartment
! BATEMAN function's BETA
          IF (SV(1) /= 0) THEN
            D2BATE = (G2(IKA,J,K)-BATE*(G2(IKA,J,K)-G2(IKE,J,K))                 &
                    -DBATE*(GG(IKA,K+1)-GG(IKE,K+1))-DBATEE(K)*(GG(IKA,J+1)      &
                    -GG(IKE,J+1)))/(GG(IKA,1)-GG(IKE,1))
            D2T12  = D2OLD1*EX2+DOLD1E(K)*DEX2+DOLD1*DEX2E(K)+OLD1*D2EX2
            D2DB   = D2T12-D2T1
            D2AETA(2,J,K) = D2AETA(2,J,K)+D2BATE*DB+DBATEE(K)*DDB+DBATE*DDBE(K)  &
                           +BATE*D2DB
!
            IF (R(1) /= ZERO) THEN 
              D2U12  = (R2E(1,J,K)-DU12*DA2E(K)-U12*D2A2-DU12E(K)*DA2)/A2
              D2OME2 = -D2EX2
              D2TR12 = D2U12*OME2+DU12E(K)*DOME2+DU12*DOME2E(K)+U12*D2OME2
              D2DR   = D2TR12-D2TR1
              D2AETA(2,J,K) = D2AETA(2,J,K)+D2BATE*DR+DBATEE(K)*DDR+DBATE        &
                             *DDRE(K)+BATE*D2DR
            END IF  
!
            IF (JC == 1) THEN 
              D2V12  = (I2REA(ISPEC,J,K)-DV12*DA2E(K)-V12*D2A2-DV12E(K)*DA2)/A2
              D2ETD2 = ETD2*(-DA2*DTDELE(K)-A2*D2TDEL-DA2E(K)*DTDEL-TDEL*D2A2)   &
                      +DETD2*(-A2*DTDELE(K)-DA2E(K)*TDEL)
              D2DEL2 = D2ETD2-D2EX2
              D2TS12 = DV12*DDEL2E(K)+V12*D2DEL2+D2V12*DEL2+DV12E(K)*DDEL2
              D2DS   = D2TS12-D2TS1
              D2AETA(2,J,K) = D2AETA(2,J,K)+D2BATE*DS+DBATEE(K)*DDS              &
                             +DBATE*DDSE(K)+BATE*D2DS
            END IF
          END IF  
!
          IF (SV(IOUT) /= 0) THEN
            D2SRR = R2E(1,J,K)+R2E(2,J,K)
            D2AETA(IOUT,J,K) = D2AETA(IOUT,J,K)+D2OLD1+D2OLD2-D2AETA(1,J,K)      &
                              -D2AETA(2,J,K)+D2SRR*DT+DSRRE(K)*ADTE(J)           &
                              +DSRR*ADTE(K)+SRR*D2ADTE(J,K)
            IF (ISPEC /= 0) THEN
              D2AETA(IOUT,J,K) = D2AETA(IOUT,J,K)+I2DEA(ISPEC,J,K)*IRA(ISPEC)    &
                                +IDEA(ISPEC,K)*IREA(ISPEC,J)+IDEA(ISPEC,J)       &
                                *IREA(ISPEC,K)+IDA(ISPEC)*I2REA(ISPEC,J,K)
            END IF
          END IF
        END DO
      END DO
!
   10 FORMAT (' ONE COMPARTMENT MODEL WITH FIRST-ORDER ABSORPTION (ADVAN2)')
   12 FORMAT ('0MAXIMUM NO. OF BASIC PK PARAMETERS:',I4)
   20 FORMAT ('0BASIC PK PARAMETERS (AFTER TRANSLATION):',                       &
             /'   ELIMINATION RATE (K) IS BASIC PK PARAMETER NO.:',I3,           &
             /'   ABSORPTION RATE (KA) IS BASIC PK PARAMETER NO.:',I3)           
   35 FORMAT (' ')
!
  999 RETURN
!
  502 CALL ERRORMSGS(502,FILENAME='OUTPUT')
!
      END SUBROUTINE ADVAN
