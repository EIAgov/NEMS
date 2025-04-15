!-*- f90 -*-
!******************************************************************
!*    Subroutine RELAX
!*    PW2 USE UNADJUSTED PRICES FOR RELAXATION, TOO
!*    Heuristic routine to limit or reset certain model outputs
!*    between iterations to speed overall convergence.
!******************************************************************

      SUBROUTINE RELAX

      IMPLICIT NONE

      include 'parametr'
      include 'emmparm'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'uefdout'
      include 'ngtdmout'
      include 'ngtdmrep'
      include 'cdsparms'
      include 'coalemm'
      include 'coalprc'
      include 'pqchar'
      include 'intout'
      include 'macout'
      include 'pmmrpt'
      include 'emission'
      include 'uso2grp'
      include 'ogsmout'
      include 'wrenew'
      include 'pmmout'

      INTEGER IR         ! SUBSCRIPT INDEX FOR REGION
      INTEGER IV         ! SUBSCRIPT INDEX FOR VARIABLE
      INTEGER IX         ! readjustment subscript

! +++ Reset PRICES and QUANTITIES to points 2/3 between previous and current iteration.
      DO IR=1,MNUMCR-2
         DO IV = 1,MNUMP
            MPRC(IR,CURIYR,IV)=PREVP(IR,CURIYR,IV)+ &
                RLXP(CURITR,IV)*(MPRC(IR,CURIYR,IV)-PREVP(IR,CURIYR,IV))
         ENDDO
         DO IV = 1,MNUMQ
            MQTY(IR,CURIYR,IV)=PREVQ(IR,CURIYR,IV)+ &
                RLXQ(CURITR,IV)*(MQTY(IR,CURIYR,IV)-PREVQ(IR,CURIYR,IV))
         ENDDO
      ENDDO
      DO IR=1,NNGEM
         DO IV = 1,3
            MUPRC(IR,CURIYR,IV)=PREVUP(IR,CURIYR,IV)+ &
                RLXUP(CURITR,IV)*(MUPRC(IR,CURIYR,IV)-PREVUP(IR,CURIYR,IV))
            MUQTY(IR,CURIYR,IV)=PREVUQ(IR,CURIYR,IV)+ &
                RLXUQ(CURITR,IV)*(MUQTY(IR,CURIYR,IV)-PREVUQ(IR,CURIYR,IV))
         ENDDO
         DO IV = 1,4
            IF (IV.LE.2) THEN
              IX = 1
            ELSE
              IX = 2
            ENDIF
            SMUPRC(IR,CURIYR,IV)=PREVUP(IR,CURIYR,IV+3)+ &
                RLXUP(CURITR,IX)*(SMUPRC(IR,CURIYR,IV)-PREVUP(IR,CURIYR,IV+3))
            SMUQTY(IR,CURIYR,IV)=PREVUQ(IR,CURIYR,IV+3)+ &
                RLXUQ(CURITR,IX)*(SMUQTY(IR,CURIYR,IV)-PREVUQ(IR,CURIYR,IV+3))
         ENDDO
         DO IV = 1,3
            SGASPRICE(IR,CURIYR,IV)=PREVGASP(IR,CURIYR,IV)+ &
                RLXGASP(CURITR,IV)*(SGASPRICE(IR,CURIYR,IV)-PREVGASP(IR,CURIYR,IV))
            SGASQUANT(IR,CURIYR,IV)=PREVGASQ(IR,CURIYR,IV)+ &
                RLXGASQ(CURITR,IV)*(SGASQUANT(IR,CURIYR,IV)-PREVGASQ(IR,CURIYR,IV))
         ENDDO
      ENDDO

!  New coal regions
      DO IR=1,NDRGG
         PCLELCDR(1,IR,CURIYR)=PREVCPS(1,IR,CURIYR)+ &
                RLXCPS(CURITR)*(PCLELCDR(1,IR,CURIYR)-PREVCPS(1,IR,CURIYR))
         PCLELCDR(2,IR,CURIYR)=PREVCPS(2,IR,CURIYR)+ &
                RLXCPS(CURITR)*(PCLELCDR(2,IR,CURIYR)-PREVCPS(2,IR,CURIYR))
         DO IV = 1,NCLUT1
            QCLCLNR(IR,CURIYR,IV)=PREVCQS(IR,CURIYR,IV)+ &
                RLXCQS(CURITR,IV)*(QCLCLNR(IR,CURIYR,IV)-PREVCQS(IR,CURIYR,IV))
         ENDDO
      ENDDO
      DO IV = 1,MNOTH
         CONVOTH(2,IV,CURIYR)=CONVOTH(1,IV,CURIYR)+ &
            RLXOTH(CURITR,IV)*(CONVOTH(2,IV,CURIYR)-CONVOTH(1,IV,CURIYR))
      ENDDO

!  Coal Supply Curce Productive Capacity
      DO IR=1,MX_NCOALS
         XCL_PCAP(IR,CURIYR)=PREVPCAP(IR,CURIYR)+ &
             RLXPCAP(CURITR,1)*(XCL_PCAP(IR,CURIYR)-PREVPCAP(IR,CURIYR))
      ENDDO

!  Coal generation unit utilization factors
      DO IR=1,MX_UNITS
         EMM_CL_CF(IR,CURIYR) = PREV_CL_CF(IR,CURIYR)+ &
             RLXPCAP(CURITR,2) * (EMM_CL_CF(IR,CURIYR) - PREV_CL_CF(IR,CURIYR))
         EMM_CL_BTUs(IR,CURIYR) = PREV_CL_BTUs(IR,CURIYR)+ &
             RLXPCAP(CURITR,3) * (EMM_CL_BTUs(IR,CURIYR) - PREV_CL_BTUs(IR,CURIYR))
      ENDDO

!     SINCE WE RELAX THE OTHERS, WE NEED TO COPY THEM BACK

      MC_GDPR(CURIYR)         = CONVOTH(2,1,CURIYR)
      IT_WOP(CURIYR,1)        = CONVOTH(2,2,CURIYR)
      RFQICRD(MNUMPR,CURIYR)  = CONVOTH(2,3,CURIYR)
      RFPQIPRDT(MNUMPR,CURIYR,2) = CONVOTH(2,4,CURIYR)
      EMETAX(1,CURIYR)        = CONVOTH(2,5,CURIYR)
      EMELPSO2(CURIYR,1)      = CONVOTH(2,6,CURIYR)
      ECP_PHG(1,CURIYR)       = CONVOTH(2,7,CURIYR)
      OGWPRNG(MNUMOR,CURIYR)  = CONVOTH(2,8,CURIYR)
      OGCNPPRD(1,CURIYR)      = CONVOTH(2,9,CURIYR)
      OGCNPPRD(2,CURIYR)      = CONVOTH(2,10,CURIYR)
      GLBCRDDMD(CURIYR)       = CONVOTH(2,43,CURIYR)
      ECP_PSO2(0:10,CURIYR,1) = CONVOTH(2,44:54,CURIYR)
      ECP_PSO2(0:10,CURIYR,2) = CONVOTH(2,55:65,CURIYR)


! Comment out the following, as it would only mess things up
!     PBMET(1:9,CURIYR,1)   = CONVOTH(2,21:29,CURIYR)
!     PBMET(MNUMCR,CURIYR,1)= CONVOTH(2,30,CURIYR)
!     QBMET(1:9,CURIYR,1)   = CONVOTH(2,31:39,CURIYR)
!     QBMET(MNUMCR,CURIYR,1)= CONVOTH(2,40,CURIYR)

! Recalculate national totals for PBMET and QBMET to ensure they add up
!     DO IV=1,NUMETHQ
!     QBMET(MNUMCR,CURIYR,IV) = sum(QBMET(1:9,CURIYR,IV))
!     PBMET(MNUMCR,CURIYR,IV) = 0.0
!     DO IR=1,9
!        IF (QBMET(MNUMCR,CURIYR,IV) .NE. 0.0) &
!        PBMET(MNUMCR,CURIYR,IV) = PBMET(MNUMCR,CURIYR,IV) + &
!            QBMET(IR,CURIYR,IV) * PBMET(IR,CURIYR,IV) / QBMET(MNUMCR,CURIYR,IV)
!     ENDDO
!     ENDDO

      RETURN
      END
      
      
!* ******************************************************************
!*  Subroutine CVTEST(IMODEL)
!*
!*  Sets convergence flags for the module: CNVTST(IMODEL,CURIYR) in
!*  CVTEST.  CVTEST computes absolute and percentage changes for the
!*  key convergence variables, and sets CNVTST(IMODEL,CURIYR) to 1
!*  if changes are less than the required tolerance.
!*
!*  Inputs:
!*    IMODEL ARGUMENT Index of Model just executed
!*    CURIYR (NCNTRL) Current year being executed
!*    FRCTOL (NCNTRL) Minimum fractional convergence tolerance
!*    ABSTOL (NCNTRL) Minimum absolute convergence tolerance
!*    MQTY   (PQ)     Vector of QUANTITY products, current iteration
!*    MPRC   (PQ)     Vector of PRICE products, current iteration
!*    PREVQ  /PREV/   Vector of QUANTITY products, previous iteration
!*    PREVP  /PREV/   Vector of PRICE products, previous iteration
!*    ALSO, 3 NGMM  NG PRICE VARIABLES AT THE NERC REGION LEVEL
!*          3 EMM   NG QUANT VARIABLES AT THE NERC REGION LEVEL
!*         12 CMM COAL PRICE VARIABLES AT THE COAL REGION LEVEL
!*         15 EMM COAL QUANT VARIABLES AT THE COAL REGION LEVEL
!*
!*  QUANTITY CONVERGENCE TESTS:
!*     THE ABSOLUTE CHANGE MUST EXCEED A MINIMUM, AND THE RELATIVE
!*     CHANGE, COMPUTED AS:  (ABSOLUTE CHANGE) / ((CURRENT+PREVIOUS)/2)
!*     MUST EXCEED MINIMUM.
!*  PRICE CONVERGENCE TESTS:
!*     SAME AS QUANTITY TEST, BUT ALSO THE CORRESPONDING QUANTITY FOR
!*     THE PRICE MUST EXCEED A MINIMUM.  CURRENTLY, THE MINIMUM
!*     USED IS THE SAME MINIMUM USED FOR THE QUANTITY CONVERGENCE TEST
!*  Outputs:
!*    CNVTST(IMODEL,CURIYR)   Flag set to 0 if changes exceed minimum
!*                            tolerance.
!*    NOW USES ADJUSTED PRICES PW2
!*******************************************************************
      SUBROUTINE CVTEST(IMODEL)
      IMPLICIT NONE

      INTEGER IMODEL
      include 'parametr'
      include 'emmparm'
      include 'cdsparms'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'uefdout'
      include 'ngtdmout'
      include 'coalemm'
      include 'coalprc'
      include 'pqchar'
      include 'uso2grp'

      REAL FRCCHG                      ! Holds fractional change
      REAL ABSCHG                      ! Holds absolute change
      INTEGER IR                       ! Subscript index for REGION
      INTEGER IV                       ! Subscript index for VARIABLE
      INTEGER IQ                       ! Subscript index for Q VARIABLE
      INTEGER IY                       ! Subscript index for YEAR (IY=CURIYR)
      INTEGER NCHECK                   ! Flag for tolerance
      CHARACTER*9 VARNAM
      INTEGER NUMQAS,NUMQ_AS,NUMPAS,NUMP_AS,PQMATCH
      EXTERNAL NUMQ_AS,NUMP_AS,PQMATCH,RTOVALUE
      INTEGER MNDBGCV,RTOVALUE
      REAL TRIGGER
      LOGICAL DONEYET/.FALSE./
      NCHECK=1
      IY=CURIYR

! DEBUG CODE TO TRIGGER CVTESTS.
! GET RUNTIME PARAMETER MNDBGCV TO SEE IF DEBUG CODE SHOULD EXECUTE.
! PERTURB VALUES ON EVEN ITERATIONS TO TRIGGER NONCONVERGENCE
      IF(.NOT. DONEYET) MNDBGCV=RTOVALUE('MNDBGCV ',0)
      DONEYET=.TRUE.
      IF(MNDBGCV.EQ.1) THEN
        IF(CURITR.EQ.((CURITR/2)*2)) THEN
          TRIGGER=1.11
        ELSE
          TRIGGER=1/1.11
        ENDIF
        IR=7
        DO IV = 1,MNUMP
          NUMPAS=NUMP_AS(IV,MNUMP)
          IF(NUMPAS.EQ.0) THEN
            PREVP(IR,IY,IV)=PREVP(IR,IY,IV)*TRIGGER
          ENDIF
        ENDDO
        DO IV = 1,MNUMQ
          NUMQAS=NUMQ_AS(IV,MNUMQ)
          IF(NUMQAS.EQ.0) THEN
            PREVQ(IR,IY,IV)=PREVQ(IR,IY,IV)*TRIGGER
          ENDIF
        ENDDO
        DO IV = 1,7
          PREVUP(IR,IY,IV)=PREVUP(IR,IY,IV)*TRIGGER
        ENDDO
        DO IV = 1,7
          PREVUQ(IR,IY,IV)=PREVUQ(IR,IY,IV)*TRIGGER
        ENDDO
        PREVCPS(1,IR,IY)=PREVCPS(1,IR,IY)*TRIGGER
        PREVCPS(2,IR,IY)=PREVCPS(2,IR,IY)*TRIGGER
        DO IV = 1,NCLUT1
          PREVCQS(IR,IY,IV)=PREVCQS(IR,IY,IV)*TRIGGER
        ENDDO
        IR=11
        DO IV = 1,MNOTH
          CONVOTH(2,IV,IY)=CONVOTH(2,IV,IY)*TRIGGER
        ENDDO
        PREVPCAP(IR,IY)=PREVPCAP(IR,IY)*TRIGGER
        PREV_CL_CF(IR,IY)=PREV_CL_CF(IR,IY)*TRIGGER
        PREV_CL_BTUs(IR,IY)=PREV_CL_BTUs(IR,IY)*TRIGGER
      ENDIF
! END DEBUG CODE

! NOTE: NCHECK GETS SET TO 0 IN MNPRTCV IF ANY VARIABLE FAILS CONVERGENCE TESTS.
      DO IR=1,MNUMCR-2  ! 9 census divisons.  skip 10 (unused) and 11 (national total)
        DO IV = 1,MNUMP
          NUMPAS=NUMP_AS(IV,MNUMP)
          IF(NUMPAS.EQ.0) THEN
            CALL MNCHKCV(MPRC(IR,IY,IV),PREVP(IR,IY,IV),FRCCHG,ABSCHG)
            IQ=PQMATCH(IV,MNUMQ,MNUMP)     ! Get matching QUANTITY product
            IF(FRCCHG.GE.MNPCNV(1,IV).AND.ABSCHG.GE.MNPCNV(2,IV) .AND.  & ! CNV TEST
                   (MQTY(IR,IY,IQ).GE.MNQCNV(2,IQ)                      & ! CNV TEST
               .OR.PREVQ(IR,IY,IQ).GE.MNQCNV(2,IQ))  ) THEN               ! CNV TEST
               VARNAM=MPVARS(IV)//'   '
               CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                   MPRC(IR,IY,IV),PREVP(IR,IY,IV),FRCCHG,MNPCNV(1,IV), &
                   VARNAM,SUBR_NAMES(IMODEL),'&&&P')
            ENDIF
          ENDIF
        ENDDO
        DO IV = 1,MNUMQ
          NUMQAS=NUMQ_AS(IV,MNUMQ)
          IF(NUMQAS.EQ.0) THEN
            CALL MNCHKCV(MQTY(IR,IY,IV),PREVQ(IR,IY,IV),FRCCHG,ABSCHG)
            IF(FRCCHG.GE.MNQCNV(1,IV).AND.ABSCHG.GE.MNQCNV(2,IV))THEN ! CNV TEST
              VARNAM=MQVARS(IV)//'   '
              CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                  MQTY(IR,IY,IV),PREVQ(IR,IY,IV),FRCCHG,MNQCNV(1,IV), &
                  VARNAM,SUBR_NAMES(IMODEL),'&&&Q')
            ENDIF
          ENDIF
        ENDDO
      ENDDO

      DO IR=1,NNGEM
        DO IV = 1,3
          CALL MNCHKCV(MUPRC(IR,IY,IV),PREVUP(IR,IY,IV),FRCCHG,ABSCHG)
          IF (FRCCHG.GE.MNUPCNV(1,IV).AND.ABSCHG.GE.MNUPCNV(2,IV) .AND. & ! CNV TEST
             (MUQTY(IR,IY,IV).GE.MNUQCNV(2,IV)                          & ! CNV TEST
         .OR.PREVUQ(IR,IY,IV).GE.MNUQCNV(2,IV))       )THEN               ! CNV TEST
             VARNAM=MUPVARS(IV)
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 MUPRC(IR,IY,IV),PREVUP(IR,IY,IV),FRCCHG,MNUPCNV(1,IV), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&P')
          ENDIF
        ENDDO
        DO IV = 1,3
          CALL MNCHKCV(MUQTY(IR,IY,IV),PREVUQ(IR,IY,IV),FRCCHG,ABSCHG)
          IF(FRCCHG.GE.MNUQCNV(1,IV).AND.ABSCHG.GE.MNUQCNV(2,IV))THEN ! CNV TEST
             VARNAM=MUQVARS(IV)
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 MUQTY(IR,IY,IV),PREVUQ(IR,IY,IV),FRCCHG,MNUQCNV(1,IV), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&Q')
          ENDIF
        ENDDO
        DO IV = 1,3
          CALL MNCHKCV(SGASPRICE(IR,IY,IV),PREVGASP(IR,IY,IV),FRCCHG,ABSCHG)
          IF (FRCCHG.GE.MNGASPCNV(1,IV).AND.ABSCHG.GE.MNGASPCNV(2,IV) .AND. & ! CNV TEST
             (SGASQUANT(IR,IY,IV).GE.MNGASQCNV(2,IV)                          & ! CNV TEST
         .OR.PREVGASQ(IR,IY,IV).GE.MNGASQCNV(2,IV))       )THEN               ! CNV TEST
             VARNAM=SGASPVARS(IV)
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 SGASPRICE(IR,IY,IV),PREVGASP(IR,IY,IV),FRCCHG,MNGASPCNV(1,IV), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&P')
          ENDIF
        ENDDO
        DO IV = 1,3
          CALL MNCHKCV(SGASQUANT(IR,IY,IV),PREVGASQ(IR,IY,IV),FRCCHG,ABSCHG)
          IF(FRCCHG.GE.MNGASQCNV(1,IV).AND.ABSCHG.GE.MNGASQCNV(2,IV))THEN ! CNV TEST
             VARNAM=SGASQVARS(IV)
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 SGASQUANT(IR,IY,IV),PREVGASQ(IR,IY,IV),FRCCHG,MNGASQCNV(1,IV), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&Q')
          ENDIF
        ENDDO
      ENDDO

!   Added check for new coal/utility prices and quantities
      DO IR=1,NDRGG
        DO IV = 1,2
          CALL MNCHKCV(PCLELCDR(IV,IR,IY),PREVCPS(IV,IR,IY),FRCCHG,ABSCHG)
          IF(FRCCHG.GE.MNCPCNVS(1).AND.ABSCHG.GE.MNCPCNVS(2)   ) THEN ! CNV TEST
!           .AND. (QCLCLNR(IR,IY,IV).GE.MNCQCNVS(2,IV)              & ! CNV TEST
!            .OR. PREVCQS(IR,IY,IV).GE.MNCQCNVS(2,IV))         )THEN  ! CNV TEST
               VARNAM=MCPVARSS(IV)
               CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                   PCLELCDR(IV,IR,IY),PREVCPS(IV,IR,IY),FRCCHG,MNCPCNVS(1), &
                   VARNAM,SUBR_NAMES(IMODEL),'&&&P')
          ENDIF
        ENDDO
        DO IV = 1,NCLUT1
          CALL MNCHKCV(QCLCLNR(IR,IY,IV),PREVCQS(IR,IY,IV),FRCCHG,ABSCHG)
          IF(FRCCHG.GE.MNCQCNVS(1,IV).AND.ABSCHG.GE.MNCQCNVS(2,IV))THEN ! CNV TEST
             VARNAM=MCQVARSS(IV)//'  '
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 QCLCLNR(IR,IY,IV),PREVCQS(IR,IY,IV),FRCCHG,MNCQCNVS(1,IV), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&Q')
          ENDIF
        ENDDO
      ENDDO
      IR=11
      IR=11
      DO IV = 1,MNOTH
         CALL MNCHKCV(CONVOTH(2,IV,IY),CONVOTH(1,IV,IY),FRCCHG,ABSCHG)
         IF(FRCCHG.GE.MNOCNV(1,IV).AND.ABSCHG.GE.MNOCNV(2,IV))THEN   ! CNV TEST
             VARNAM=MOVARS(IV)
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 CONVOTH(2,IV,IY),CONVOTH(1,IV,IY),FRCCHG,MNOCNV(1,IV), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&O')
         ENDIF
      ENDDO

!     Added check for new coal productive capacity

      DO IR=1,MX_NCOALS
        CALL MNCHKCV(XCL_PCAP(IR,IY),PREVPCAP(IR,IY),FRCCHG,ABSCHG)
        IF(FRCCHG.GE.MNZCNV(1,1).AND.ABSCHG.GE.MNZCNV(2,1))  THEN  ! CNV TEST
             VARNAM=MZVARS(1)//' '
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 XCL_PCAP(IR,IY),PREVPCAP(IR,IY),FRCCHG,MNZCNV(1,1), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&Z')
        ENDIF
      ENDDO

!     Added check for coal generation unit utilization factors

      DO IR=1,MX_UNITS
        CALL MNCHKCV(EMM_CL_CF(IR,IY),PREV_CL_CF(IR,IY),FRCCHG,ABSCHG)
        IF(FRCCHG.GE.MNZCNV(1,2).AND.ABSCHG.GE.MNZCNV(2,2))  THEN  ! CNV TEST
             VARNAM=MZVARS(2)//' '
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 EMM_CL_CF(IR,IY),PREV_CL_CF(IR,IY),FRCCHG,MNZCNV(1,2), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&Z')
        ENDIF
        CALL MNCHKCV(EMM_CL_BTUs(IR,IY),PREV_CL_BTUs(IR,IY),FRCCHG,ABSCHG)
        IF(FRCCHG.GE.MNZCNV(1,3).AND.ABSCHG.GE.MNZCNV(2,3))  THEN  ! CNV TEST
             VARNAM=MZVARS(3)//' '
             CALL MNPRTCV(NCHECK,IY,CURITR,IR, &
                 EMM_CL_BTUs(IR,IY),PREV_CL_BTUs(IR,IY),FRCCHG,MNZCNV(1,3), &
                 VARNAM,SUBR_NAMES(IMODEL),'&&&Z')
        ENDIF
      ENDDO

      IF (NCHECK.EQ.1) THEN
          CNVTST(IMODEL,CURIYR)=1
      ELSE
          CNVTST(IMODEL,CURIYR)=0
      ENDIF
      RETURN
      END
!******************************************************************
!******************************************************************
      SUBROUTINE MNCHKCV(CUR,OLD,FRCCHG,ABSCHG)
      IMPLICIT NONE
! SETS FRACTIONAL AND ABSOLUTE CHANGE VARIABLES, GIVEN CURRENT AND PREVIOUS VALUES.

! CALLED BY CVTEST

      REAL CUR    ! CURRENT VALUE              (INPUT)
      REAL OLD    ! PREVIOUS VALUE             (INPUT)
      REAL FRCCHG ! FRACTIONAL RELATIVE CHANGE (OUTPUT)
      REAL ABSCHG ! ABSOLUTE VALUE OF CHANGE   (OUTPUT)
      REAL DENOM  ! DENOMINATOR FOR RELATIVE CHANGE (LOCAL)
      FRCCHG=0.0
      ABSCHG=ABS(CUR-OLD)
      DENOM=(CUR+OLD)/2.
      IF (DENOM.NE.0.0) FRCCHG = ABSCHG/DENOM
      RETURN
      END
!******************************************************************
      SUBROUTINE MNPRTCV(NCHECK,IY,IT,IR,CUR,OLD,FRCCHG,LIMIT,VARNAM,SUBNAM,TYPSTR)
      IMPLICIT NONE

! PRINTS MESSAGE FOR VARIABLES NOT MEETING CONVERGENCE CRITERIA.  THE
! MESSAGE DISPLAYS THE SUBROUTINE, VARIABLE NAME, CURRENT AND PRIOR VALUES,
! PERCENTAGE CHANGE, TOLERANCE,
! ITERATION COUNT, REGION, YEAR.  THE ROUTINE ALSO CALLS ROUTINE TO
! PUT THE ENTRY INTO THE SORTED SUMMARY LIST OF VARIABLES NOT CONVERGED.

! CALLED BY CVTEST
      INTEGER NCHECK  ! FLAG FOR WHETHER HEADING HAS BEEN PRINTED FOR GROUP
      INTEGER IY      ! NEMS YEAR INDEX
      INTEGER IT      ! NEMS ITERATION
      INTEGER IR      ! REGION INDEX
      REAL    CUR     ! CURRENT VALUE
      REAL    OLD     ! PREVIOUS VALUE
      REAL    FRCCHG  ! FRACTIONAL RELATIVE CHANGE
      REAL    LIMIT   ! TOLERANCE LIMIT ON FRACTIONAL RELATIVE CHANGE
      CHARACTER*9 VARNAM  ! VARIABLE NAME
      CHARACTER*6 SUBNAM  ! SHORT SUBROUTINE NAME FOR NEMS ROUTINE
      CHARACTER*4 TYPSTR  ! SEARCH STRING ('&&&Q', '&&&P' OR '&&&O')

      IF(NCHECK.EQ.1 .AND. IT.GE.3) WRITE(*,'(A)') ' &&&   MODULE' // &
           '  VARIABLE  YR IT RG   CURRENT  PREVIOUS %CHANGE %TOLER'
      NCHECK=0
!		write(6,'(A)') '[INTEGRATION] main.f MNPRTCV (2658) - 6'
!      IF(IT .GE. 3 .AND. TYPSTR .NE. '&&&Z') WRITE(*,'(A,3I3,2F10.3,F7.1,F7.1,F9.3)') ' ' // &
!             TYPSTR //': '// SUBNAM // '  ' // VARNAM,IY,IT,IR,CUR,OLD,FRCCHG*100.,LIMIT*100.

      CALL SORTRPT(FRCCHG,IR,OLD,CUR,VARNAM)

      RETURN
      END
      
!******************************************************************
!*    FILL ARRAYS CONTAINING debugging information for a run.  THE
!*    ROUTINES MAINTAINS A LIST, SORTED BY PERCENTAGE CHANGE, OF THE
!*    THE TOP n VARIABLES FAILING TO CONVERGE.
!******************************************************************
      SUBROUTINE SORTRPT(FRCCHG,IR,PREVV,MRC,MVARS)
      IMPLICIT NONE
      include 'parametr'
      include 'maindbg'

      REAL PREVV,MRC,FRCCHG
      INTEGER IR
      CHARACTER*(*) MVARS

      INTEGER I,IDX,TMP1,TMP2
      I = 1
      IDX = 0
      DO WHILE ((I .LE. MNDBGVARS) .AND. (IDX .EQ. 0))
        IF (FRCCHG .GT. DTBAC(I,3)) IDX = I
        I = I + 1
      ENDDO
      IF (IDX .GT. 0) THEN
        DO I = (MNDBGVARS-1), IDX , -1
          DTBAC(I + 1,1) = DTBAC(I,1)
          DTBAC(I + 1,2) = DTBAC(I,2)
          DTBAC(I + 1,3) = DTBAC(I,3)
          DTREG(I + 1) = DTREG(I)
          DTVAR(I + 1) = DTVAR(I)
        ENDDO
        DTBAC(IDX,1) = PREVV
        DTBAC(IDX,2) = MRC
        DTBAC(IDX,3) = FRCCHG
        DTREG(IDX)   = IR
        DTVAR(IDX)   = MVARS
      ENDIF
      RETURN
      END
      
!******************************************************************************
!*    Function PQMATCH(IPRICE,MNUMQ,MNUMP)
!*
!*    Returns the subscript of the corresponding quantity-variable for any
!*    PRICE subscript.  Checks for changes in price/quantity variable dimen-
!*    sions using MNUMQ and MNUMP.
!******************************************************************************
      FUNCTION PQMATCH(IPRICE,MNUMQ,MNUMP)
      IMPLICIT NONE

      INTEGER PQMATCH        ! Returns product code subscript for MQTY
      INTEGER NP             ! Dimension of map -- MUST MATCH MNUMP
      PARAMETER(NP=84)
      INTEGER MAP(NP)        ! Mapping of PRICE products to QUANTITY products
      INTEGER IPRICE         ! Product code subscript for MPRC
      INTEGER MNUMQ,MNUMP    ! Dimensions of MQTY and MPRC to insure against
                             ! changes.

      DATA MAP/ &
         1,  2,  3,  4,  7,          & ! PELRS,PELCM,PELTR,PELIN,PELAS,
         8,  9, 10, 11, 13, 15,      & ! PGFRS,PGFCM,PGFTR,PGFIN,PGFEL,PGFAS,
        16, 17, 18, 19, 21, 23,      & ! PGIRS,PGICM,PGITR,PGIIN,PGIEL,PGIAS,
        24, 25, 26, 27, 29, 31,      & ! PNGRS,PNGCM,PNGTR,PNGIN,PNGEL,PNGAS,
        32, 33,                      & ! PGPTR,PLPIN,
        34, 35, 36, 38, 39, 41, 42,  & ! PCLRS,PCLCM,PCLIN,PCLEL,PCLSN,PCLAS,PMCIN,
        43, 44, 45, 46, 47,          & ! PMGCM,PMGTR,PMGIN,PMGAS,PJFTR,
        48, 49, 50, 51, 53, 54,      & ! PDSRS,PDSCM,PDSTR,PDSIN,PDSEL,PDSAS,
        55, 56, 57, 58,              & ! PKSRS,PKSCM,PKSIN,PKSAS,
        59, 60, 61, 62, 64,          & ! PLGRS,PLGCM,PLGTR,PLGIN,PLGAS,
        65, 66, 67, 69, 70,          & ! PRLCM,PRLTR,PRLIN,PRLEL,PRLAS,
        71, 72, 73,                  & ! PRHTR,PRHEL,PRHAS,
        74, 75, 76, 78, 79,          & ! PRSCM,PRSTR,PRSIN,PRSEL,PRSAS,
        80, 87,                      & ! PPFIN,PASIN,
        88, 89, 91,                  & ! POTTR,POTIN,POTAS,
        92, 93, 94, 95, 96, 97, 98,  & ! PTPRS,PTPCM,PTPTR,PTPIN,PTPRF,PTPEL,PTPAS,
        99,100,102,103,              & ! PMETR,PETTR,PHYTR,PUREL
       155,156,157/                    ! PH1TR,PH2TR,PH3TR

      PQMATCH=MAP(IPRICE)
      IF (NP.NE.MNUMP) THEN
         WRITE(*,*) ' PARAMETER MNUMP (DIMENSION OF MAIN PRICE ARRAY MPRC) HAS BEEN CHANGED'
         WRITE(*,*) ' AND IS INCOMPATIBLE WITH FUNCTION PQMATCH IN MAIN.'
      ENDIF

      IF (PQMATCH.GT.MNUMQ) THEN
         WRITE(*,*) ' PARAMETER MNUMQ (DIMENSION OF MAIN QTY ARRAY MQTY) HAS BEEN CHANGED'
         WRITE(*,*) ' AND IS INCOMPATIBLE WITH FUNCTION PQMATCH IN MAIN.'
      ENDIF

      RETURN
      END
      subroutine GETIRUN(run_char)
      use dfport
      implicit none
      character*(*) run_char
      call getenv("n",run_char)
      return
      end
      subroutine GETMPS(mps)
      use dfport
      implicit none
      character*50 mps
      call getenv("MPS",mps)
      return
      end
      
!******************************************************************************
!*    Function NUMQ_AS(IQUAN,MNUMQ)
!*
!*    The function NUMQ_AS(IPRICE) returns the number of component quantities
!*    for the MNUMQ array.  Quantity arrays with names not ending in "AS"
!*    (for Across Sector) have 0 as the number of components.  It is currently
!*    (5/7/92) being used to skip these "AS" series for the convergence test.
!******************************************************************************
      FUNCTION NUMQ_AS(IQUAN,MNUMQ)
      IMPLICIT NONE

      INTEGER NUMQ_AS
      INTEGER MNUMQ,IQUAN
      INTEGER NQ
      PARAMETER (NQ=160)
      INTEGER NUMQAS(NQ)

      DATA NUMQAS/ &
            0, 0, 0, 0, 0, 0, 6,        & ! QELRS,QELCM,QELTR,QELIN,QELRF,QELHM,QELAS,
           -1,-1,-1, 0,-1, 0,-1, 7,     & ! QGFRS,QGFCM,QGFTR,QGFIN,QGFRF,QGFEL,QGFHM,QGFAS,
           -1,-1,-1, 0,-1, 0,-1, 7,     & ! QGIRS,QGICM,QGITR,QGIIN,QGIRF,QGIEL,QGIHM,QGIAS,
            0, 0, 0, 0, 0, 0, 0, 7,     & ! QNGRS,QNGCM,QNGTR,QNGIN,QNGRF,QNGEL,QNGHM,QNGAS,
            0, 0,                       & ! QGPTR,QLPIN,
            0, 0, 0, 0, 0, 0, 0, 7,     & ! QCLRS,QCLCM,QCLIN,QCLRF,QCLEL,QCLSN,QCLHM,QCLAS,
            0,                          & ! QMCIN,
            0, 0, 0, 3,                 & ! QMGCM,QMGTR,QMGIN,QMGAS,
            0,                          & ! QJFTR,
            0, 0, 0, 0, 0, 0, 6,        & ! QDSRS,QDSCM,QDSTR,QDSIN,QDSRF,QDSEL,QDSAS,
            0, 0, 0, 3,                 & ! QKSRS,QKSCM,QKSIN,QKSAS,
            0, 0, 0, 0, 0, 5,           & ! QLGRS,QLGCM,QLGTR,QLGIN,QLGRF,QLGAS,
            0, 0, 0, 0, 0, 5,           & ! QRLCM,QRLTR,QRLIN,QRLRF,QRLEL,QRLAS,
            0, 0, 2,                    & ! QRHTR,QRHEL,QRHAS,
           -1,-1,-1,-1,-1, 5,           & ! QRSCM,QRSTR,QRSIN,QRSRF,QRSEL,QRSAS,
            0, 0, 0,                    & ! QPFIN,QSGIN,QSGRF,
            0, 0, 0, 3,                 & ! QPCIN,QPCRF,QPCEL,QPCAS,
            0,                          & ! QASIN,
            0, 0, 0, 3,                 & ! QOTTR,QOTIN,QOTRF,QOTAS,
           -1,-1,-1,-1,-1,-1, 6,        & ! QTPRS,QTPCM,QTPTR,QTPIN,QTPRF,QTPEL,QTPAS,
            0, 0, 0, 0, 0, 0,           & ! QMETR,QETTR,QETHM,QHYTR,QUREL,QURHM,
            0, 0, 2,                    & ! QHOIN,QHOEL,QHOAS,
            0, 0, 0, 3,                 & ! QGERS,QGEIN,QGEEL,QGEAS,
            0, 0, 0, 0, 0, 0, 0, 7,     & ! QBMRS,QBMCM,QBMIN,QBMRF,QBMEL,QBMSN,QBMHM,QBMAS,
            0, 0, 2,                    & ! QMSIN,QMSEL,QMSAS,
            0, 0, 0, 0, 4,              & ! QSTRS,QSTCM,QSTIN,QSTEL,QSTAS,
            0, 0, 0, 0, 4,              & ! QPVRS,QPVCM,QPVIN,QPVEL,QPVAS,
            0, 0, 2,                    & ! QWIIN,QWIEL,QWIAS,
           -1,-1,-1,-1,-1,-1,-1, 7,     & ! QTRRS,QTRCM,QTRTR,QTRIN,QTREL,QTRSN,QTRHM,QTRAS,
            0, 0,                       & ! QEIEL,QCIIN,
           -1,-1,-1,-1,-1,-1,-1,-1, 8,  & ! QTSRS,QTSCM,QTSTR,QTSIN,QTSRF,QTSEL,QTSSN,QTSHM,QTSAS
            0, 0, 0, 0, 0, 0/             ! QH1TR,QH2TR,QH3TR,QH2IN,QH2INPF,QH2INHP

      IF (NQ.NE.MNUMQ) THEN
         WRITE(*,*) ' PARAMETER MNUMQ (DIMENSION OF MAIN QUANTITY ARRAY MPRC) HAS BEEN CHANGED'
         WRITE(*,*) ' AND IS INCOMPATIBLE WITH FUNCTION NUMQ_AS IN MAIN.'
      ENDIF

      NUMQ_AS=NUMQAS(IQUAN)

      RETURN
      END
      
!******************************************************************************
!*    Function NUMP_AS(IPRICE,MNUMP)
!*
!*    The function NUMP_AS(IPRICE) returns the number of component prices for
!*    any of the MNUMP price arrays.  Prices arrays with names not ending in
!*    "AS" (for Across Sector) have 0 as the number of component prices. For
!*    the "AS" arrays, NUMP_AS(IV,MNUMP) returns NUMPAS, the number of prices
!*    defined for product IV. These NUMPAS component prices arrays are stored
!*    as the NUMPAS consecutive arrays before IV, or from (IV-NUMPAS) to (IV-1).
!******************************************************************************
      FUNCTION NUMP_AS(IPRICE,MNUMP)
      IMPLICIT NONE

      INTEGER NUMP_AS
      INTEGER MNUMP,IPRICE
      INTEGER NP
      PARAMETER (NP=84)
      INTEGER NUMPAS(NP)

!  THE NUMPAS VARIABLE IS A KEY FOR BOTH CONVERGENCE TESTING AND
!     SUMMING OVER THE SECTORS INTO THE 'AS' VARIABLES AS FOLLOWS:

!           VALUE   EFFECT
!            -1     NO CONVERGENCE TESTING OR SUMMING
!             0     NO SUMMING
!             N(>0) SUM PREVIOUS N VARIABLES;  NO CONVERGENCE TESTING

      DATA NUMPAS/ &
            0, 0, 0, 0, 4,       & ! PELRS,PELCM,PELTR,PELIN,PELAS,
           -1,-1,-1, 0, 0, 5,    & ! PGFRS,PGFCM,PGFTR,PGFIN,PGFEL,PGFAS,
           -1,-1,-1, 0, 0, 5,    & ! PGIRS,PGICM,PGITR,PGIIN,PGIEL,PGIAS,
            0, 0, 0, 0, 0, 5,    & ! PNGRS,PNGCM,PNGTR,PNGIN,PNGEL,PNGAS,
            0, 0,                & ! PGPTR,PLPIN,
            0, 0, 0, 0, 0, 5,    & ! PCLRS,PCLCM,PCLIN,PCLEL,PCLSN,PCLAS,
            0,                   & ! PMCIN,
            0, 0, 0, 3,          & ! PMGCM,PMGTR,PMGIN,PMGAS,
            0,                   & ! PJFTR,
            0, 0, 0, 0, 0, 5,    & ! PDSRS,PDSCM,PDSTR,PDSIN,PDSEL,PDSAS,
            0, 0, 0, 3,          & ! PKSRS,PKSCM,PKSIN,PKSAS,
            0, 0, 0, 0, 4,       & ! PLGRS,PLGCM,PLGTR,PLGIN,PLGAS,
            0, 0, 0, 0, 4,       & ! PRLCM,PRLTR,PRLIN,PRLEL,PRLAS,
            0, 0, 2,             & ! PRHTR,PRHEL,PRHAS,
            0, 0, 0, 0, 4,       & ! PRSCM,PRSTR,PRSIN,PRSEL,PRSAS,
            0, 0,                & ! PPFIN,PASIN,
            0, 0, 2,             & ! POTTR,POTIN,POTAS,
           -1,-1,-1,-1,-1,-1, 6, & ! PTPRS,PTPCM,PTPTR,PTPIN,PTPRF,PTPEL,PTPAS
            0, 0, 0, 0,          & ! PMETR,PETTR,PHYTR,PUREL
           -1,-1,-1/               ! PH1TR,PH2TR,PH3TR

      IF (NP.NE.MNUMP) THEN
         WRITE(*,*) ' PARAMETER MNUMP (DIMENSION OF MAIN PRICE ARRAY MPRC) HAS BEEN CHANGED'
         WRITE(*,*) ' AND IS INCOMPATIBLE WITH FUNCTION NUMP_AS IN MAIN.'
      ENDIF

      NUMP_AS=NUMPAS(IPRICE)
      RETURN
      END