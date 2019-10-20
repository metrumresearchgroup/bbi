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
! CREATED ON  : AUG/1995
! LANGUAGE    : FORTRAN 90/95
! LAST UPDATE : JUL/2008 - COMMON BLOCKS REPLACED WITH MODULES
!               NOV/2008 - INTRODUCED HEADER INFORMATIONS AND RESTRUCTURED AS PER
!                          THE NONMEM STANDARDS
!               FEB/2009 - MOVED DATA STATEMENTS TO PRDATA.F90 FILE
!               APR/2009 - INTRODUCED ERROR CHECK FOR FILE OPERATION
!               FEB/2011 - INTEGRATED 7.2BETA5.8B MODIFICATIONS
!
!----------------------------- TRANS4.F90 -------------------------------------------
! SUBROUTINE TRANS(ITRANS,IRGG,GG,NPETAS)
!
! DESCRIPTION : Translates CL,V1,Q2,V2,Q3,V3 to K,K12,K21,K13,K31. Translator ID=9994.
!
! ARGUMENTS   : ITRANS,IRGG,GG,NPETAS
!               IN     - IRGG,NPETAS
!                        IRGG   - Row dimension of GG array
!                        NPETAS - Not used
!               OUT    - NONE
!               IN OUT - ITRANS,GG
!                        ITRANS - = 0 Model initialization
!                                 = 2 Normal entry
!                        GG     - Array of pharmacokinetic parameters and derivatives
!                                 w.r.t ETAs after creation by subr. PK and optional
!                                 exponentiation by PRED
!                                 GG(I,1)=Ith PK parameter
!                                 GG(I,J)=Deriv. of Ith PK parameter wrt (J-1)st ETA
!
! CALLED BY   : PRED      - This is PREDPP main program. Provides prediction, partial 
!                           derivatives of the statistical model with respect to ETA
!                           and EPSILON random variables and stores them in the G
!                           and H arguments of the PRED routine.
!               PREDI     - Initialization-Finalization routine of PRED
!
! CALLS       : ERRORMSGS - Writes error messages to a file JUNIT and sets IQUIT to 1 indicating
!                           that NONMEM has to quit for non-super problems. And for super problems
!                           calculation continues with next sub problem.
!
! ALGORITHM   : - If (ITRANS <= 0) then Initialization entry; Else Normal entry
!                 - ITRANS=9996; RETURN
!               - Normal entry. 
!                 - Check for errors in TRANS parameters
!                 - Update GG
!               - RETURN
!
! MODULES USED: PRSIZES,PRDATA,NMPRD_INT,PRCOM_INT,PROCM_INT,NMPRD_CHAR,PRCOM_LOG,
!               NM_INTERFACE,PRDIMS
!
! CONTAINS    : NONE
!      
! LOCAL'S     : DV,DV2,DVE,I,J,JP,K,KP,NA,NP,PE,V,V2
!
!---------------------------- END OF HEADER -----------------------------------------
!
      SUBROUTINE TRANS(ITRANS,IRGG,GG,NPETAS) 
!      
      USE PRSIZES,      ONLY: ISIZE,DPSIZE,PE
      USE PRDATA,       ONLY: PK
! INTEGER
      USE NMPRD_INT,    ONLY: IERPRD
      USE PRCOM_INT,    ONLY: ADVID,LOGUNT
      USE PROCM_INT,    ONLY: NACTIV,IDXETA
      USE PRDIMS,       ONLY: GTRD
! CHARACTER
      USE NMPRD_CHAR,   ONLY: ETEXT
! LOGICAL
      USE PRCOM_LOG,    ONLY: SECOND 
! INTERFACE
      USE NM_INTERFACE, ONLY: ERRORMSGS
!
      IMPLICIT NONE
!     
      INTEGER(KIND=ISIZE), INTENT(IN)     :: IRGG,NPETAS 
      INTEGER(KIND=ISIZE), INTENT(IN OUT) :: ITRANS
      REAL(KIND=DPSIZE),   INTENT(IN OUT) :: GG(IRGG,GTRD+1,GTRD+1) 
!      
      SAVE
!      
!------------------------------------------------------------------------------------
!     COMMON /NMPRD1/ IERPRD,NETEXT
!     COMMON /NMPRD2/ ETEXT(3)
!     INTEGER IERPRD,NETEXT
!     CHARACTER*132 ETEXT
!     COMMON /PROCM5/ NACTIV,M(0:PE)
!     INTEGER NACTIV,M
!     COMMON /PRCOM7/ ADVID,SSID
!     INTEGER ADVID,SSID
!     COMMON /PRCOMN/ LOGUNT,NC
!     COMMON /PRCOM1/ NOETAS,SECOND
!     LOGICAL NOETAS,SECOND 
!------------------------------------------------------------------------------------
!    
! Local Variables    
!   
      INTEGER(KIND=ISIZE) :: I,J,JP,K,KP,NA,NP
!
      REAL(KIND=DPSIZE)   :: DV,DVE(PE),DV2,V,V2
!      
      IF (ITRANS <= 0) THEN             ! Initialization entry
        SELECT CASE (ADVID)
        CASE (3) 
          WRITE (LOGUNT,100,ERR=502)
          NP=4; NA=1
        CASE (4) 
          WRITE (LOGUNT,101,ERR=502)
          NP=4; NA=2
        CASE (11) 
          WRITE (LOGUNT,102,ERR=502)
          NP=6; NA=3
        CASE (12) 
          WRITE (LOGUNT,103,ERR=502)
          NP=6; NA=4
        END SELECT
        ITRANS=9996; GO TO 999
      END IF
!     
      DO I=1,NP                         ! Normal entry
        IF (GG(I,1,1) <= 0.0D0) THEN   
          IF (GG(I,1,1) == 0.0D0) THEN  ! Check for errors in TRANS parameters
            ETEXT(1)='ERROR IN TRANS4 ROUTINE: '//PK(I,NA)//' IS ZERO'
          ELSE
            ETEXT(1)='ERROR IN TRANS4 ROUTINE: '//PK(I,NA)//' IS NEGATIVE'
          END IF
          IERPRD=1; GO TO 999
        END IF
      END DO
!      
      V = GG(2,1,1)                     ! Update GG
      V2= GG(4,1,1)
      GG(1,1,1) = GG(1,1,1)/V
      GG(2,1,1) = GG(3,1,1)/V
      GG(3,1,1) = GG(3,1,1)/V2
!      
      IF (NA > 2) THEN
        GG(4,1,1) = GG(5,1,1)/V
        GG(5,1,1) = GG(5,1,1)/GG(6,1,1)
      END IF
!      
      IF (NACTIV /= 0) THEN  
        DO K=1,NACTIV
          KP = IDXETA(K)+1
          DV = GG(2,KP,1)
          GG(1,KP,1) = (GG(1,KP,1)-GG(1,1,1)*DV)/V
          GG(2,KP,1) = (GG(3,KP,1)-GG(2,1,1)*DV)/V
          GG(3,KP,1) = (GG(3,KP,1)-GG(3,1,1)*GG(4,KP,1))/V2
          IF (NA > 2) THEN
            GG(4,KP,1) = (GG(5,KP,1)-GG(4,1,1)*DV)/V
            GG(5,KP,1) = (GG(5,KP,1)-GG(5,1,1)*GG(6,KP,1))/GG(6,1,1)
          ELSE
            DVE(K) = DV
          END IF
        END DO
        IF (SECOND) THEN
          DO K=1,NACTIV
            KP=IDXETA(K)+1
            DO J=K,NACTIV
              JP = IDXETA(J)+1
              DV2= GG(2,JP,KP)
              GG(1,JP,KP) = (GG(1,JP,KP)-GG(1,1,1)*DV2 -GG(1,JP,1)*DVE(K)-GG(1,KP,1)  &
                           *DVE(J))/V
              GG(2,JP,KP) = (GG(3,JP,KP)-GG(2,1,1)*DV2 -GG(2,JP,1)*DVE(K)-GG(2,KP,1)  &
                           *DVE(J))/V
              GG(3,JP,KP) = (GG(3,JP,KP)-GG(3,1,1)*GG(4,JP,KP)-GG(3,JP,1)*GG(4,KP,1)  &
                           -GG(3,KP,1)*GG(4,JP,1))/V2
            END DO
          END DO
        END IF
      END IF
!      
  100 FORMAT (' TRANSLATOR WILL CONVERT PARAMETERS '/' CL, V1, Q, V2 TO K, K12, K21 (TRANS4)')
  101 FORMAT (' TRANSLATOR WILL CONVERT PARAMETERS '/' CL, V2, Q, V3 TO K, K23, K32 (TRANS4)')
  102 FORMAT (' TRANSLATOR WILL CONVERT PARAMETERS '/' CL, V1, Q2, V2, Q3, V3 TO K, K12, K21, K13, K31 (TRANS4)')
  103 FORMAT (' TRANSLATOR WILL CONVERT PARAMETERS '/' CL, V2, Q3, V3, Q4, V4 TO K, K23, K32, K24, K42 (TRANS4)')      
!      
  999 RETURN 
!  
  502 CALL ERRORMSGS(502,FILENAME='OUTPUT')
!
      END SUBROUTINE TRANS
