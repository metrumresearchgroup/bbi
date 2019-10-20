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
! LAST UPDATE : JUN/1990 - SECOND DERIVATIVES AND DERIVATIVES W.R.T. DT - AJB
!               JUL/2008 - COMMON BLOCKS REPLACED WITH MODULES
!               NOV/2008 - INTRODUCED HEADER INFORMATIONS AND RESTRUCTURED AS PER
!                          THE NONMEM STANDARDS
!               FEB/2009 - MOVED DATA STATEMENTS TO PRDATA.F90 FILE
!               APR/2009 - INTRODUCED ERROR CHECK FOR FILE OPERATION
!               FEB/2010 - CHANGED SIZES TO PRSIZES
!               FEB/2011 - INTEGRATED 7.2BETA5.8B MODIFICATIONS
!
!----------------------------- ADVAN1.F90 -------------------------------------------
!
! SUBROUTINE ADVAN(ICALL)
!
! DESCRIPTION :ADVAN1 is a routine in PREDPP's library which implements kinetic
!              equations for One Compartment Linear Model.
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
! ALGORITHM   : - IF (ICALL == 2) Normal Entry; ELSE
!                 - Initialize variables
!                 - Set MODEL definition variables
!                 - Internally, central compartment is no. 1
!                 - Define PK parameters
!                 - RETURN
!               - Normal entry
!                 - Perform central compartment calculations
!                 - Check for errors in basic PK parameters
!                 - J loop: 1,NPETAS
!                   - Calculate first derivatives for central compartment
!                   - Save first derivatives
!                   - K loop: 1,J
!                     - Calculate second derivatives for central compartment
!                   - K loop end
!                 - J loop end
!
! MODULES USED: PRSIZES,PRDATA,NMPRD_INT,PRCOM_INT,PRMOD_INT,PROCM_INT,PRCOM_REAL,
!               PROCM_REAL,NMPRD_CHAR,PRMOD_CHAR,PRCOM_LOG,NM_INTERFACE
!
! CONTAINS    : NONE
!
! LOCAL'S     : A2,D2A2,D2DEL2,D2ETD2,D2EX2,D2K2T,D2OLD1,D2OME2,D2SDT,D2T2,D2TDEL,
!               D2TR2,D2TS2,D2U2,D2V2,DA2,DA2E,DDEL2,DDEL2E,DEL2,DETD2,DEX2,DEX2E,
!               DK2T,DK2TE,DOLD1,DOLD1E,DOME2,DOME2E,DSDT,DT2,DTDEL,DTDELE,DTR2,DTS2,
!               DU2,DU2E,DV2,DV2E,ETD2,EX2,G2,GG,I,IOUT,J,K,K2T,OLD1,OME2,PE,SDT,T2,
!               TDEL,TR2,TS2,U2,V2
!
!---------------------------- END OF HEADER -----------------------------------------
!
      SUBROUTINE ADVAN(ICALL)
!
      USE PRSIZES,      ONLY: ISIZE,DPSIZE,PE
!
      USE PRDATA,       ONLY: IDENT
! INTEGER
      USE NMPRD_INT,    ONLY: IERPRD
      USE PRCOM_INT,    ONLY: ADVID,ITSC,IDC,IDO,ISV,IINST,ITURN,SV,ISPEC,NPETAS,LOGUNT,&
                              NC,NBP
      USE PRMOD_INT,    ONLY: IKE,IV
      USE PROCM_INT,    ONLY: IDXETA
! REAL
      USE PRCOM_REAL,   ONLY: ADTE,D2ADTE,G3,DT,I2REA,I2DEA,R2E,IRA,IREA,IDA,IDEA,R,RE, &
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
!     COMMON /PRMONO/ IKE,IV
!     COMMON /PROCM4/ A,DAETA,D2AETA
!     DOUBLE PRECISION A,DAETA,D2AETA
!     DIMENSION A(PC),DAETA(PC,PE),D2AETA(PC,PE,PE)
! FOR LVOUT FEATURE
!     COMMON /PROCM5/ NACTIV,M(0:PE)
!     INTEGER NACTIV,M
!------------------------------------------------------------------------------------
!
! Local Variables
!
      INTEGER(KIND=ISIZE):: I,IOUT,J,K
!      
      REAL(KIND=DPSIZE)  :: SDT,DSDT,D2SDT,A2,OLD1,T2,DA2,DOLD1,D2A2,D2OLD1,EX2,U2,  &
                            K2T,DK2T,D2K2T,DEX2,DT2,DU2,D2EX2,D2T2,D2U2,OME2,TR2,    &
                            DOME2,DTR2,D2OME2,D2TR2,V2,DV2,D2V2,ETD2,DEL2,DETD2,     &
                            DDEL2,D2ETD2,D2DEL2,TS2,DTS2,D2TS2,TDEL,DTDEL,D2TDEL,    &
                            DTDELE(PE),DA2E(PE),DK2TE(PE),DEX2E(PE),DU2E(PE),        &
                            DV2E(PE),DOME2E(PE),DDEL2E(PE),DOLD1E(PE),G2,GG
!
      G2(I,J,K)=G3(I,IDXETA(J)+1,IDXETA(K)+1) ! Statement function
      GG(I,J)=G3(I,IDXETA(J-1)+1,1)
!
      IF (ICALL /= 2) THEN  ! Initialization call
        NC=2;  ADVID=1      ! Set MODEL definition variables & internally, central compartment is no. 1
        IDC=1; ISV(1)=1; IINST(1)=1; NAME(1)=IDENT(1); ITURN(1)=0; IOUT=2
        IDO=1; ISV(2)=0; IINST(2)=0; NAME(2)=IDENT(2); ITURN(2)=1; NBP=2   
!        
        IKE=1; IV=2         ! Define PK parameters  
!        
        WRITE (LOGUNT,10,ERR=502)
        WRITE (LOGUNT,12,ERR=502) NBP
        WRITE (LOGUNT,20,ERR=502) IKE
        WRITE (LOGUNT,35,ERR=502)
        IF (SECOND) THEN
          DU2=ZERO; DOME2=ZERO
          DV2=ZERO; DDEL2=ZERO
        END IF
        GO TO 999
      END IF
!
      SDT=ZERO; TDEL=ZERO     ! Normal entry
      IF (ISPEC /= 0) THEN
        SDT=IDA(ISPEC); TDEL=DT-SDT
      END IF
!      
      A2=GG(ITSC,1)*GG(IKE,1) ! Central compartment
      IF (A2 <= ZERO) THEN    ! Check for errors in basic PK parameters
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
      OLD1=AMNT(1)
      T2=OLD1*EX2
      AMNT(1)=T2
!
      IF (R(1) /= ZERO) THEN
        U2=R(1)/A2
        OME2=ONE-EX2
        TR2=U2*OME2
        AMNT(1)=AMNT(1)+TR2
      END IF  
      IF (ISPEC /= 0) THEN
        V2=IRA(ISPEC)/A2
        ETD2=EXP(-A2*TDEL)
        DEL2=ETD2-EX2
        TS2=V2*DEL2
        AMNT(1)=AMNT(1)+TS2
      END IF  
 !
      IF (SV(IOUT) /= 0) THEN
        AMNT(IOUT)=AMNT(IOUT)+OLD1-AMNT(1)+R(1)*DT
        IF (ISPEC /= 0) AMNT(IOUT)=AMNT(IOUT)+IDA(ISPEC)*IRA(ISPEC)
      END IF
!
      IF (.NOT. NOETAS) THEN 
        DO J=1,NPETAS
          DSDT=ZERO; DTDEL=ZERO
          IF (ISPEC /= 0) THEN
            DSDT=IDEA(ISPEC,J)
            DTDEL=ADTE(J)-DSDT
          END IF
          DOLD1=DAETA(1,J)    ! First derivatives for central compartment
          DA2=GG(ITSC,1)*GG(IKE,J+1)+GG(ITSC,J+1)*GG(IKE,1)
          DK2T=-A2*ADTE(J)-DA2*DT
          DEX2=EX2*DK2T
          DT2=OLD1*DEX2+DOLD1*EX2
          DAETA(1,J)=DT2
          IF (R(1) /= ZERO) THEN
            DU2=(RE(1,J)-U2*DA2)/A2
            DOME2=-DEX2
            DTR2=U2*DOME2+DU2*OME2
            DAETA(1,J)=DAETA(1,J)+DTR2
          END IF  
          IF (ISPEC /= 0) THEN 
            DV2=(IREA(ISPEC,J)-V2*DA2)/A2
            DETD2=ETD2*(-A2*DTDEL-DA2*TDEL)
            DDEL2=DETD2-DEX2
            DTS2=V2*DDEL2+DV2*DEL2
            DAETA(1,J)=DAETA(1,J)+DTS2
          END IF
 !
          IF (SV(IOUT) /= 0) THEN
            DAETA(IOUT,J) = DAETA(IOUT,J)+DOLD1-DAETA(1,J)+RE(1,J)*DT+R(1)*ADTE(J)
            IF (ISPEC /= 0) THEN
              DAETA(IOUT,J) = DAETA(IOUT,J)+IDEA(ISPEC,J)*IRA(ISPEC)+IDA(ISPEC)*     &
                              IREA(ISPEC,J)
            END IF
          END IF
          IF (.NOT. SECOND) CYCLE  
!          
! Second derivatives; Save first derivatives
          DA2E(J)=DA2; DTDELE(J)=DTDEL; DK2TE(J)=DK2T; DOME2E(J)=DOME2
          DU2E(J)=DU2; DOLD1E(J)=DOLD1; DEX2E(J)=DEX2; DDEL2E(J)=DDEL2 
          DV2E(J)=DV2
! Below commented code is repeated in VI 2.0; check it
!         DOME2E(J)=DOME2
!         DDEL2E(J)=DDEL2
      
          DO K=1,J
            IF (ISPEC /= 0) THEN
              D2SDT=I2DEA(ISPEC,J,K)
              D2TDEL=D2ADTE(J,K)-D2SDT
            END IF
            D2OLD1= D2AETA(1,J,K) ! Second derivatives for central compartment
            D2A2  = GG(ITSC,J+1)*GG(IKE,K+1)+GG(ITSC,1)*G2(IKE,J,K)+                 &
                    GG(ITSC,K+1)*GG(IKE,J+1)+G2(ITSC,J,K)*GG(IKE,1)
            D2K2T = -DA2*ADTE(K)-A2*D2ADTE(J,K)-DA2E(K)*ADTE(J)-DT*D2A2
            D2EX2 = EX2*D2K2T+DEX2*DK2TE(K)
            D2T2  = DOLD1*DEX2E(K)+OLD1*D2EX2+D2OLD1*EX2+DOLD1E(K)*DEX2
            D2AETA(1,J,K)=D2T2
            IF (R(1) /= ZERO) THEN
              D2U2   = (R2E(1,J,K)-U2*D2A2-DU2*DA2E(K)-DU2E(K)*DA2)/A2
              D2OME2 = -D2EX2
              D2TR2  = U2*D2OME2+DOME2E(K)*DU2+OME2*D2U2+DOME2*DU2E(K)
              D2AETA(1,J,K)=D2AETA(1,J,K)+D2TR2
            END IF  
            IF (ISPEC /= 0) THEN
              D2V2   = (I2REA(ISPEC,J,K)-V2*D2A2-DV2*DA2E(K)-DV2E(K)*DA2)/A2
              D2ETD2 = ETD2*(-DA2*DTDELE(K)-A2*D2TDEL-DA2E(K)*DTDEL-TDEL*            &
                       D2A2)+DETD2*(-A2*DTDELE(K)-DA2E(K)*TDEL)
              D2DEL2 = D2ETD2-D2EX2
              D2TS2  = DV2*DDEL2E(K)+V2*D2DEL2+D2V2*DEL2+DV2E(K)*DDEL2
              D2AETA(1,J,K)=D2AETA(1,J,K)+D2TS2
            END IF  
            IF (SV(IOUT) /= 0) THEN
              D2AETA(IOUT,J,K) = D2AETA(IOUT,J,K)+D2OLD1-D2AETA(1,J,K)+R2E(1,J,K)*   &
                                 DT+RE(1,K)*ADTE(J)+RE(1,J)*ADTE(K)+R(1)*D2ADTE(J,K)
              IF (ISPEC /= 0) THEN
                D2AETA(IOUT,J,K) = D2AETA(IOUT,J,K)+I2DEA(ISPEC,J,K)*IRA(ISPEC)+     &
                                   IDEA(ISPEC,K)*IREA(ISPEC,J)+IDEA(ISPEC,J)*        &
                                   IREA(ISPEC,K)+IDA(ISPEC)*I2REA(ISPEC,J,K)
              END IF
            END IF
          END DO
        END DO
      END IF  
!
   10 FORMAT (' ONE COMPARTMENT MODEL (ADVAN1)')
   12 FORMAT ('0MAXIMUM NO. OF BASIC PK PARAMETERS:',I4)
   20 FORMAT ('0BASIC PK PARAMETERS (AFTER TRANSLATION):',                           &
             /'   ELIMINATION RATE (K) IS BASIC PK PARAMETER NO.:',I3)
   35 FORMAT (' ')
!
  999 RETURN
!
  502 CALL ERRORMSGS(502,FILENAME='OUTPUT')
!
      END SUBROUTINE ADVAN
