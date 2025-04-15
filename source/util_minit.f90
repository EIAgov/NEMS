!-*- f90 -*-
!*******************************************************************
!*    Subroutine NINIT
!*
!*    Initialize global QUANTITY and PRICE variables to zero
!*******************************************************************
      SUBROUTINE NINIT
      IMPLICIT NONE

      include 'parametr'
      include 'ncntrl'
      include 'qblk'
      include 'mpblk'
      include 'converge' ! for oscor and CVSCOREHIST
      integer i,j

!WRITE(*,*)"start NINIT()"
      CALL ZEROR(MQARRY,MQSIZE)
      CALL ZEROR(MPARRY,MPSIZE)

      RETURN
      END
!*****************************************************************
!*    Subroutine ZEROR(X,N)
!*
!*    Zeroes array X with adjustable dimension size N
!*****************************************************************
      SUBROUTINE ZEROR(X,N)
      IMPLICIT NONE

      INTEGER I,N
      REAL X(N)

!WRITE(*,*)"NINIT(): in ZEROR(),N=",N
      DO 10 I=1,N
         X(I)=0.
10    CONTINUE
      RETURN
      END
!*****************************************************************
!==========================================================================
!******************************************************************
!     ROUTINE TO STORE AN ITERATION'S WORTH OF CONVERGENCE VARIABLE
!     VALUES TO DIRECT ACCESS DEBUG FILE.  FILE IS LATER USED BY
!     ROUTINE MAINDBGRPT.  THIS ROUTINE CALLS WRMNPQIT TO DO WORK
!******************************************************************
      SUBROUTINE STMNPQIT
      IMPLICIT NONE
      include 'parametr'
      include 'emmparm'
      include 'cdsparms'
      include 'ncntrl'
      include 'qblk'
      include 'ampblk'
      include 'uefdout'
      include 'angtdm'
      include 'coalemm'
      include 'acoalprc'
      include 'pqchar'
      include 'uso2grp'
      include 'maindbg'  ! <-- working with these variables primarily

      INTEGER IR        ! Subscript index for REGION
      INTEGER IV        ! Subscript index for VARIABLE
      INTEGER IY        ! Subscript index for YEAR (IY=CURIYR)
      INTEGER IREC      ! HOLDS RECORD NUMBER FOR D/A FILE
      INTEGER FLAGWR    ! FLAG TO TELL SUBROUTINE TO WRITE BLOCK ON LAST CALL
      INTEGER NUMQAS,NUMQ_AS,NUMPAS,NUMP_AS,ILAST
      EXTERNAL NUMQ_AS,NUMP_AS

!WRITE(*,*)"start STMNPQIT()"

! SET POINTERS TO FIRST RECORD FOR EACH YEAR GROUP
      IY=CURIYR
      DTCURBLK=0  ! INIT THE # OF THE CURRENT BLOCK OF RECORDS IN MEMORY
      IF(CURITR.EQ.1.AND.CURIYR.EQ.FIRSYR) THEN
        DTSTART(CURIYR)=1
      ELSEIF(CURITR.EQ.1.AND.CURIYR.GT.FIRSYR) THEN
        DTSTART(CURIYR)=DTSTART(CURIYR-1)+DTNVAR
      ENDIF

!WRITE(*,*)"STMNPQIT():, MNUMP=",MNUMP

      DTNVAR=0  ! INITIALIZE COUNTER FOR NUMBER OF CONV VARIABLES OR RECORDS
      FLAGWR=0  ! TURNOFF FLAG THAT TELLS WRMNPQIT TO WRITE A BLOCK
! PROCESS PRICE VARIABLES, THEN QTY VARIABLES, THEN OTHER VARIABLES, LOOPING THROUGH REGIONS
      DO IV = 1,MNUMP
! SEE IF THE VARIABLE IS A TOTAL VARIABLE OR NOT--WE SKIP TOTAL VARIABLES
!WRITE(*,*)"STMNPQIT():, in loop, before NUMPAS=NUMP_AS(IV,MNUMP),NUMPAS=",NUMPAS
        NUMPAS=NUMP_AS(IV,MNUMP)
!WRITE(*,*)"STMNPQIT():, in loop, after NUMPAS=NUMP_AS(IV,MNUMP),IV=,NUMPAS=",IV,NUMPAS
!WRITE(*,*) NUMPAS
        IF (NUMPAS.EQ.0) THEN
!WRITE(*,*)"STMNPQIT():, NUMPAS=0 before call WRMNPQIT(MPVARS(IV),[IY,CURIYR,CURITR,FIRSYR,FLAGWR]=",IY,CURIYR,CURITR,FIRSYR,FLAGWR
          DO IR=1,MNUMCR-2
            CALL WRMNPQIT(MPVARS(IV),MPRC(IR,IY,IV),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
          ENDDO
        ENDIF
      ENDDO
! GO THROUGH LIST OF QUANTITIES--DO SAME THING
!WRITE(*,*)"STMNPQIT():, MNUMQ=",MNUMQ
      DO IV = 1,MNUMQ
!WRITE(*,*)"STMNPQIT():, in loop, before (b)NUMQAS=NUMP_AS(IV,MNUMP),NUMQAS=",NUMQAS
        NUMQAS=NUMQ_AS(IV,MNUMQ)
!WRITE(*,*)"STMNPQIT():, in loop, after (b)NUMQAS=NUMP_AS(IV,MNUMP),IV=,NUMPAS=",IV,NUMPAS
!WRITE(*,*) NUMQAS
        IF (NUMQAS.EQ.0) THEN
!WRITE(*,*)"STMNPQIT():, NUMQAS=0 before call (b)WRMNPQIT(MQVARS(IV),[CURIYR,CURITR,FIRSYR,FLAGWR]=",CURIYR,CURITR,FIRSYR,FLAGWR
          DO IR=1,MNUMCR-2
            CALL WRMNPQIT(MQVARS(IV),MQTY(IR,IY,IV),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
          ENDDO
        ENDIF
      ENDDO

!WRITE(*,*)"------STMNPQIT():, NNGEM------",NNGEM
! GO THROUGH LIST OF PRICES OF NATURAL GAS, COAL TO UTILITIES
      DO IV = 1,3
          DO IR=1,NNGEM
            CALL WRMNPQIT(MUPVARS(IV),MUPRC(IR,IY,IV),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
          ENDDO
      ENDDO
      DO IV = 1,3
          DO IR=1,NNGEM
            CALL WRMNPQIT(SGASPVARS(IV),SGASPRICE(IR,IY,IV),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
          ENDDO
      ENDDO
!  Coal prices
      DO IV = 1,2
          DO IR=1,NDRGG
            CALL WRMNPQIT(MCPVARSS(IV),PCLELCDR(IV,IR,IY),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
          ENDDO
      ENDDO
! GO THROUGH LIST OF QUANTITIES OF NATURAL GAS, COAL FROM UTILITIES
      DO IV = 1,3
          DO IR=1,NNGEM
            CALL WRMNPQIT(MUQVARS(IV),MUQTY(IR,IY,IV),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
          ENDDO
      ENDDO
      DO IV = 1,3
          DO IR=1,NNGEM
            CALL WRMNPQIT(SGASQVARS(IV),SGASQUANT(IR,IY,IV),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
          ENDDO
      ENDDO
! Coal quantities
      DO IV = 1,NCLUT1
          DO IR=1,NDRGG
            CALL WRMNPQIT(MCQVARSS(IV),QCLCLNR(IR,IY,IV),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
          ENDDO
      ENDDO
! GO THROUGH LIST OF OTHER CONVERGENCE VARIABLES AND DO SAME THING ON LAST CALL, SET FLAG
      FLAGWR=0
      IR=11
      DO IV = 1,MNOTH
        IF(IV.EQ.MNOTH) FLAGWR=1
        CALL WRMNPQIT(MOVARS(IV),CONVOTH(2,IV,IY),IR,CURIYR,CURITR,FIRSYR,FLAGWR)
      ENDDO
!WRITE(*,*)"STMNPQIT():, MNOTH=",MNOTH
!WRITE(*,*)"STMNPQIT():, if (DTNVAR.EQ.MXDTNVAR),DTNVAR=,MXDTNVAR=",DTNVAR,MXDTNVAR
      IF(DTNVAR.EQ.MXDTNVAR) WRITE(6,*) 'IN STMNPQIT, DTNVAR>MXDTNVAR'
      IF(DTNVAR.EQ.MXDTNVAR) WRITE(6,*) 'INCREASE MXDTNVAR IN /MAINDBG/'
99    RETURN
      END

!******************************************************************
!*    ROUTINE TO STORE/RETRIEVE BLOCKS OF CONVERGENCE VARIABLES
!     ON DIRECT ACCESS DEBUG FILE.  CALL BY STMNPQIT
!******************************************************************
      SUBROUTINE WRMNPQIT(CVAR,QVAR,IR,CURIYR,CURITR,FIRSYR,FLAGWR)
      IMPLICIT NONE
! INPUT ARGUMENTS:
!    CVAR   -- NAME OF VARIABLE TO STORE ON DATA FILE
!    QVAR   -- VALUE OF VARIABLE
!    IR     -- REGION NUMBER (1-11)
!    CURIYR -- NEMS YEAR INDEX
!    CURITR -- NEMS ITERATION COUNT
!    FLAGWR -- INTEGER FLAG (1=YES) TO SAY WHETHER TO WRITE OUT
!              A GROUP OF 100 ITEMS ON THE FINAL CALL
      include 'parametr'
      include 'maindbg'
      CHARACTER*1 AREG(23)/'1','2','3','4','5','6','7','8','9','0', &
       'A','B','C','D','E','F','G','H','I','J','K','L','M'/
      CHARACTER*2 AYEAR(MNUMYR)/ &
       '90','91','92','93','94','95','96','97','98','99', &
       '00','01','02','03','04','05','06','07','08','09', &
       '10','11','12','13','14','15','16','17','18','19', &
       '20','21','22','23','24','25','26','27','28','29', &
       '30','31','32','33','34','35','36','37','38','39', &
       '40','41','42','43','44','45','46','47','48','49', &
       '50'/
      CHARACTER*(*) CVAR
      CHARACTER*12 DTTMP  ! HOLDS KEY READ FROM D/A FILE
      REAL*4 QVAR
      INTEGER IR,CURIYR,CURITR,FIRSYR,IREC,IPOS,IBLK,I,J,FLAGWR
!WRITE(*,*)"in STMNPQIT():, WRMNPQIT() starts"
      IF(DTNVAR.LT.MXDTNVAR) DTNVAR=DTNVAR+1
! CONSTRUCT KEY FOR DIRECT ACCESS FILE. KEY CONSISTS OF VAR NAME, REGION, YEAR
      DTNAM=CVAR           ! VAR NAME
      DTAREG=AREG(IR)      ! REGION
      DTYEAR=AYEAR(CURIYR) ! YEAR
      DTTMP=DTNAM//DTAREG//DTYEAR
! ASSIGN RECORD NUMBER FOR EACH KEY--FOR USE AFTER SORTING TO GO TO PARTICULAR RECORD
      DTOFFSET(DTNVAR)=DTNVAR
      DTKEYS(DTNVAR)=DTTMP
! IREC: logical record or item--one for each variable in each region, year
! IBLK: actual file record or block, as records are accessed in blocks of 100
! IPOS: relative position (from 1 to 100) within the block of the logical records
      IREC=DTSTART(CURIYR)+DTOFFSET(DTNVAR)-1
      IBLK=1+(IREC-1)/100
      IPOS=IREC-(IBLK-1)*100
! Read values from previous iterations.  The current record may not be at the
! last written because the reporting step reads records out-of-sequence.  If
! this is the first iteration of a year, make sure that the current record is
! the last record from the previous year.
      IF(DTCURBLK.NE.IBLK) THEN
        IF(CURITR.GT.1.OR. (DTNVAR.EQ.1.AND.IREC.GT.1.AND.IPOS.NE.1)) THEN
! IF BLOCK OF DATA NOT ALREADY IN MEMORY, LOAD IT IN
          READ(IMNPQIT,REC=IBLK,ERR=99) (DTKEY(I),(DTPQIT(J,I),J=1,MNDBGITRS),I=1,100)
          DTCURBLK=IBLK
        ENDIF
      ENDIF
      IF(DTTMP.NE.DTKEY(IPOS).AND.CURITR.GT.1) THEN
        WRITE(6,*) ' MAIN DEBUG KEY MISMATCH, EXPECTED KEY=',DTTMP
        WRITE(6,*) ' BUY DTKEY(IPOS)=',DTKEY(IPOS), &
            ' ON RECORD BLOCK:',IBLK,', POSITION=',IPOS,', ITEM NUMBER=',IREC
      ENDIF
! INITIALIZE ARRAY OF VALUES ACROSS ITERATIONS IF ON FIRST ITERATION
      IF(CURITR.EQ.1) CALL ZEROR(DTPQIT(1,IPOS),MNDBGITRS)
! ASSIGN CURRENT ITERATION'S VALUE
      DTKEY(IPOS)=DTTMP
      DTPQIT(min(mndbgitrs,CURITR),IPOS)=QVAR
! REWRITE BLOCK OF 100 ITEMS IF ON THE 100TH OR FLAG IS SET.
      IF(IPOS.EQ.100.OR.FLAGWR.EQ.1) THEN
         IF(IPOS.LT.100)THEN
           DO I=IPOS+1,100
             DTKEY(I)=' '
             DO J=1,MNDBGITRS
               DTPQIT(J,I)=0.
             ENDDO
           ENDDO
         ENDIF
         WRITE(IMNPQIT,REC=IBLK,ERR=99) (DTKEY(I),(DTPQIT(J,I),J=1,MNDBGITRS),I=1,100)
        DTCURBLK=IBLK
      ENDIF
99    RETURN
      END
!==============STMNPQIT() calls NUMP_AS() and util_cvtest_relax.f90 also defines NUMP_AS() already. 
! If util_cvtest_relax.f90 converts to Python, the following NUMP_AS() needs to be enabled=======================
! !******************************************************************************
! !*    Function NUMP_AS(IPRICE,MNUMP)
! !*
! !*    The function NUMP_AS(IPRICE) returns the number of component prices for
! !*    any of the MNUMP price arrays.  Prices arrays with names not ending in
! !*    "AS" (for Across Sector) have 0 as the number of component prices. For
! !*    the "AS" arrays, NUMP_AS(IV,MNUMP) returns NUMPAS, the number of prices
! !*    defined for product IV. These NUMPAS component prices arrays are stored
! !*    as the NUMPAS consecutive arrays before IV, or from (IV-NUMPAS) to (IV-1).
! !******************************************************************************
      ! FUNCTION NUMP_AS(IPRICE,MNUMP)
      ! IMPLICIT NONE

      ! INTEGER NUMP_AS
      ! INTEGER MNUMP,IPRICE
      ! INTEGER NP
      ! PARAMETER (NP=84)
      ! INTEGER NUMPAS(NP)

! !  THE NUMPAS VARIABLE IS A KEY FOR BOTH CONVERGENCE TESTING AND
! !     SUMMING OVER THE SECTORS INTO THE 'AS' VARIABLES AS FOLLOWS:

! !           VALUE   EFFECT
! !            -1     NO CONVERGENCE TESTING OR SUMMING
! !             0     NO SUMMING
! !             N(>0) SUM PREVIOUS N VARIABLES;  NO CONVERGENCE TESTING

! WRITE(*,*)"------STMNPQIT():, start NUMP_AS()------"

      ! DATA NUMPAS/ &
            ! 0, 0, 0, 0, 4,       & ! PELRS,PELCM,PELTR,PELIN,PELAS,
           ! -1,-1,-1, 0, 0, 5,    & ! PGFRS,PGFCM,PGFTR,PGFIN,PGFEL,PGFAS,
           ! -1,-1,-1, 0, 0, 5,    & ! PGIRS,PGICM,PGITR,PGIIN,PGIEL,PGIAS,
            ! 0, 0, 0, 0, 0, 5,    & ! PNGRS,PNGCM,PNGTR,PNGIN,PNGEL,PNGAS,
            ! 0, 0,                & ! PGPTR,PLPIN,
            ! 0, 0, 0, 0, 0, 5,    & ! PCLRS,PCLCM,PCLIN,PCLEL,PCLSN,PCLAS,
            ! 0,                   & ! PMCIN,
            ! 0, 0, 0, 3,          & ! PMGCM,PMGTR,PMGIN,PMGAS,
            ! 0,                   & ! PJFTR,
            ! 0, 0, 0, 0, 0, 5,    & ! PDSRS,PDSCM,PDSTR,PDSIN,PDSEL,PDSAS,
            ! 0, 0, 0, 3,          & ! PKSRS,PKSCM,PKSIN,PKSAS,
            ! 0, 0, 0, 0, 4,       & ! PLGRS,PLGCM,PLGTR,PLGIN,PLGAS,
            ! 0, 0, 0, 0, 4,       & ! PRLCM,PRLTR,PRLIN,PRLEL,PRLAS,
            ! 0, 0, 2,             & ! PRHTR,PRHEL,PRHAS,
            ! 0, 0, 0, 0, 4,       & ! PRSCM,PRSTR,PRSIN,PRSEL,PRSAS,
            ! 0, 0,                & ! PPFIN,PASIN,
            ! 0, 0, 2,             & ! POTTR,POTIN,POTAS,
           ! -1,-1,-1,-1,-1,-1, 6, & ! PTPRS,PTPCM,PTPTR,PTPIN,PTPRF,PTPEL,PTPAS
            ! 0, 0, 0, 0,          & ! PMETR,PETTR,PHYTR,PUREL
           ! -1,-1,-1/               ! PH1TR,PH2TR,PH3TR

! WRITE(*,*)"------STMNPQIT():,NUMP_AS()------(NP.NE.MNUMP)=",NP,MNUMP
      ! IF (NP.NE.MNUMP) THEN
         ! WRITE(*,*) ' PARAMETER MNUMP (DIMENSION OF MAIN PRICE ARRAY MPRC) HAS BEEN CHANGED'
         ! WRITE(*,*) ' AND IS INCOMPATIBLE WITH FUNCTION NUMP_AS IN MAIN.'
      ! ENDIF

      ! NUMP_AS=NUMPAS(IPRICE)
      ! RETURN
      ! END

!===================================================================================================================
!*******************************************************************
!*    Subroutine RESETOSW(OPT_NAME,NEW_VALUE)
!*
!*    Resets the value for a run-time option integer switch from the value read from the "moreopt" file
!*******************************************************************
      SUBROUTINE RESETOSW(OPT_NAME,NEW_VALUE)
!This resets the value for a run-time option integer switch from the value read from the "moreopt" file
      IMPLICIT NONE
      INTEGER I,NEW_VALUE
      INTEGER MAXRTOPTS
      PARAMETER (MAXRTOPTS=230)
      CHARACTER*8 RTOPTS(MAXRTOPTS),OPT_NAME,RESTOFLINE(MAXRTOPTS)*80
      INTEGER RTOPTSV(MAXRTOPTS),NUMRTOPTS
      COMMON /RTOOPTIONS/ RTOPTS,RTOPTSV,RESTOFLINE,NUMRTOPTS

      DO I=1,NUMRTOPTS
         IF (RTOPTS(I) .EQ. OPT_NAME) THEN
             RTOPTSV(I) = NEW_VALUE
             WRITE(6,'(" Setting option ",A8," to ",I2)') RTOPTS(I),RTOPTSV(I)
             exit
         ENDIF
      ENDDO

      END

!*******************************************************************
!*    Subroutine mnclose
!*
!*    closes all files
!*******************************************************************
      subroutine mnclose
      implicit none
! closes all files
      integer low/1/,high/999/
      integer i
      logical opn
      do i=low,high
         if (i .ne. 6 .and. i .ne. 5) then
           inquire(unit=i,opened=opn)
           if (opn) close(i)
         endif
      enddo
      end
!*******************************************************************

!============================NROTHR and INITIALIZE_API=================================

!*******************************************************************
!*    Subroutine INITIALIZE_API
!*
! Initialize_API reads the table assigning Btu conversion factors to API gravities from:
!   Thermal Properties of Petroleum Products    November 9, 1929
!  input file is apitable.txt   (comma-delimited)
!  this is for function API_TO_BTU to look through and interpolate from
!*******************************************************************
      SUBROUTINE INITIALIZE_API
      IMPLICIT NONE

      include 'apiblk'

      CHARACTER*80 LINE
      LOGICAL STILL_COMMENTS, NOT_END_OF_DATA
      REAL API_HOLD, BTU_PER_GAL_HOLD
      INTEGER I, API_UNIT

      CHARACTER*18 API_FILENAME/'APITABLE          '/
      LOGICAL NEW/.FALSE./
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR

      STILL_COMMENTS = .TRUE.
      NOT_END_OF_DATA = .TRUE.
      API_GRAV=0.0
      BTU_PER_GAL=0.0
      API_UNIT=FILE_MGR('O',API_FILENAME,NEW)
!  read comments at top of file ending with line that has an @ in column 1
      DO WHILE (STILL_COMMENTS)
         READ (API_UNIT,'(A80)') LINE
         IF (LINE(1:1) .EQ. '@') STILL_COMMENTS = .FALSE.
      ENDDO
!  and now read numbers in comma-delimited format (can read list-directed)
      API_COUNT=0
      DO WHILE (NOT_END_OF_DATA)
         READ (API_UNIT,*) API_HOLD, BTU_PER_GAL_HOLD
         IF (API_HOLD .NE. 9999) THEN
             API_COUNT=API_COUNT+1
             API_GRAV(API_COUNT) = API_HOLD
             BTU_PER_GAL(API_COUNT) = BTU_PER_GAL_HOLD
         ELSE
             NOT_END_OF_DATA = .FALSE.
         ENDIF
         IF (API_COUNT .GT. MAX_API_LINES) THEN
             NOT_END_OF_DATA = .FALSE.
!   "eg" command should flag "Encountered" in the following write
             WRITE(6,'(" Encountered possibly early end of read:  MAX_API_LINES exceeded.")')
         ENDIF
      ENDDO
      API_UNIT=FILE_MGR('C',API_FILENAME,NEW)
      DO I=1,API_COUNT
         MM_BTU_PER_BBL = BTU_PER_GAL(I) * 42 / 1000000.
      ENDDO
      RETURN
      END