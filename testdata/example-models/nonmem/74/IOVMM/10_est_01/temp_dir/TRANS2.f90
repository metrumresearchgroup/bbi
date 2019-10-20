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
! CREATED ON  : DEC/1983
! LANGUAGE    : FORTRAN 90/95
! LAST UPDATE : MAY/1990 - SECOND DERIVATIVES
!               JUL/1995 - LVOUT FEATURE. NPETAS IS THE ORIGINAL (TRUE) NUMBER
!               JUL/2008 - COMMON BLOCKS REPLACED WITH MODULES
!               NOV/2008 - INTRODUCED HEADER INFORMATIONS AND RESTRUCTURED AS PER
!                          THE NONMEM STANDARDS
!               APR/2009 - INTRODUCED ERROR CHECK FOR FILE OPERATION
!               FEB/2011 - INTEGRATED 7.2BETA5.8B MODIFICATIONS
!
!----------------------------- TRANS2.F90 -------------------------------------------
! SUBROUTINE TRANS(ITRANS,IRGG,GG,NPETAS)
!
! DESCRIPTION : Translates CL,V to K. Translator ID=9998.
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
!                 - ITRANS=9998; RETURN
!               - Normal entry. 
!                 - Check for errors in TRANS parameters
!                 - Update GG
!               - RETURN
!
! MODULES USED: PRSIZES,NMPRD_INT,PRCOM_INT,PROCM_INT,NMPRD_CHAR,PRCOM_LOG,NM_INTERFACE,PRDIMS
!
! CONTAINS    : NONE
!
! LOCAL'S     : J,JP,K,KP,PE
!
!---------------------------- END OF HEADER -----------------------------------------
!
      SUBROUTINE TRANS(ITRANS,IRGG,GG,NPETAS) 
!      
      USE PRSIZES,      ONLY: ISIZE,DPSIZE
! INTEGER
      USE NMPRD_INT,    ONLY: IERPRD
      USE PRCOM_INT,    ONLY: LOGUNT
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
!     INTEGER ITRANS,IRGG,NPETAS ! LOGUNT,NC
!     COMMON /PROCM5/ NACTIV,M(0:PE)
!     INTEGER NACTIV,M
!     COMMON /PRCOMN/ LOGUNT,NC
!     COMMON /PRCOM1/ NOETAS,SECOND
!     LOGICAL NOETAS,SECOND
!------------------------------------------------------------------------------------
!
! Local Variables           
!
      INTEGER(KIND=ISIZE) :: J,JP,K,KP
!
      IF (ITRANS <= 0) THEN             ! Initialization entry
        WRITE (LOGUNT,100,ERR=502)
        ITRANS=9998; GO TO 999
      END IF
!      
      IF (GG(2,1,1) <= 0.0D0) THEN      ! Normal entry
        IF (GG(2,1,1) == 0.0D0) THEN    ! Check for errors in TRANS parameters
          ETEXT(1)='ERROR IN TRANS2 ROUTINE: V IS ZERO'
        ELSE
          ETEXT(1)='ERROR IN TRANS2 ROUTINE: V IS NEGATIVE'
        END IF
        IERPRD=1; GO TO 999
      END IF
!      
      IF (GG(1,1,1) <= 0.0D0) THEN
        IF (GG(1,1,1) == 0.0D0) THEN    ! Check for errors in TRANS parameters
          ETEXT(1)='ERROR IN TRANS2 ROUTINE: CL IS ZERO'
        ELSE
          ETEXT(1)='ERROR IN TRANS2 ROUTINE: CL IS NEGATIVE'
        END IF
        IERPRD=1; GO TO 999
      END IF
!     
      GG(1,1,1)=GG(1,1,1)/GG(2,1,1)     ! Update GG 
! 
      IF (NACTIV /= 0) THEN     
        DO K=1,NACTIV
          KP=IDXETA(K)+1
          GG(1,KP,1)=(GG(1,KP,1)-GG(1,1,1)*GG(2,KP,1))/GG(2,1,1)
        END DO
        IF (SECOND) THEN
          DO K=1,NACTIV
            KP=IDXETA(K)+1
            DO J=K,NACTIV
              JP=IDXETA(J)+1
              GG(1,JP,KP) = (GG(1,JP,KP)-GG(1,1,1)*GG(2,JP,KP)-GG(1,JP,1)*GG(2,KP,1) &
                           -GG(1,KP,1)*GG(2,JP,1))/GG(2,1,1)
            END DO
          END DO
        END IF
      END IF
!      
  100 FORMAT (' TRANSLATOR WILL CONVERT PARAMETERS '/' CLEARANCE (CL) AND VOLUME (V) TO K (TRANS2)') 
!      
  999 RETURN 
! 
  502 CALL ERRORMSGS(502,FILENAME='OUTPUT')
!
      END SUBROUTINE TRANS
