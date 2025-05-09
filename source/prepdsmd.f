! $Header: m:/default/source/RCS/prepdsmd.f,v 1.2 1999/12/19 19:27:47 DSA Exp $
      PROGRAM LSRDBMGR
!  modified 6/30/95 to use FILE_MGR to open files
!  modified 7/26/95 to cycle thru pgm twice, first to create DAF,
!                                            then to append DAF
!                       i.e. control file is now obsolete

!****************** DESCRIPTION OF THE PROGRAM/SUBPROGRAM **********************
! THIS PROGRAM READS THE HELM-PC FORMAT LSR FILES AND
! CREATES A DIRECT ACCESS LOAD SHAPE REPRESENTATION DATA BASE FILE.
! IT RUNS SEPARATELY FROM LDSM AS PREPROCESSOR OF THE DATA.
! THE INPUT FILES ARE AS FOLLOWS:
! UNIT=IMSG2 - FILE WITH MESSAGES GENERATED BY THE PROGRAM
! UNIT=IOCN2 - CONTROL FILE
!             CONTAINS TWO FLAGS YES/NO, FALSE/TRUE, 0/1:
!             1) ARE ALL LSRS IDENTICAL, EXCLUDING ONLY NUMERICAL VALUES?
!             2) CREAT AN ENTIRELY NEW VERSION OF DAF-LSR-DB(OR APPEND OLD ONE)?
! UNIT=IOLSR2 - FILE WITH HELM-PC FORMAT LSR LIBRARY
! UNIT=IODAF2 - DIRECT ACCESS FILE WITH LSR DATABASE (DAF-LSR-DB)
!-------------------------------------------------------------------------------
!     WRITTEN BY ADAM KRECZKO, ICF RESOURCES, INC., PHONE: (703)-934-3353
!*******************************************************************************
      IMPLICIT NONE
!****************** TYPING, DECLARING AND INITIALIZING CONSTANT PARAMENTERS ****
      INCLUDE 'lsrmgpar' !<< PARAMETER DECLARATIONS
!****************** TYPING AND DECLARING VARIABLES *****************************
      INTEGER*1 FRST_PASS
      INTEGER*2 I,J,L,K,M,K1 ! UNIVERSAL COUNTERS
      INTEGER*2 ONE ! 1 REPRESENTED AS INTEGER*2
      INTEGER*2 NUMCO(MAXNP) ! NUMBER OF SEASON/DAY-TYPE COMBINATIONS
      INTEGER*2 NUMPROF   ! NUMBER OF LOAD PROFILES IN THE LSR
      LOGICAL*1 NORMALIZYORN    ! NORMALIZATION FLAG
      LOGICAL*1 DTRATIOSYORN    ! DAY-TYPE RATIOS PRESENCE FLAG
      LOGICAL*1 MOALLOFAYORN    ! MONTHLY ALLOCATION FACTORS PRESENCE FLAG
      LOGICAL*1 LSRIDENT        ! ALL LSRS IDENTICAL ?
      LOGICAL*1 CREATEDAF       ! CREATE NEW DAF-LSR-DB OR APPEND THE OLD ONE?
      CHARACTER*1 LSRTYP  ! ONE CHARACTER DEFINITION OF THE LSR TYPE
      CHARACTER*8  LSRNAME      ! 8-CHARACTER LSR NAME
      CHARACTER*12 SEADTCO(MAXNP,MAXSDT,2) ! SEASON/DAYTYPE COMBINATIONS SPEC.
      CHARACTER*80 LINE        ! INTERNAL FILE WITH A CURRENT LINE
      REAL*8 HOURLOAD(MAXNP,24)    ! HOURLY LOADS
      REAL*8 DAYTYPRATIO(MAXSEA,MAXDTP) ! DAY-TYPE ALLOCATION RATIOS
      REAL*8 MONALLOFACT(MAXMON)      ! MOTHLY ALLOCATON FACTORS
      EXTERNAL LSRFIRNB
      INTEGER*2 LSRFIRNB ! FUNCTION RETURNING FIRST NON BLANK COLUMN NUMBER
      EXTERNAL LSRTORF
      LOGICAL*1 LSRTORF ! FUNCTION TRANSLATING YES OR NO INTO TRUE OR FALSE
      LOGICAL*1 PROFINDFLAG     ! FLAG SHOWING IF THE PROFINDEX MATRIX IS READY
      EXTERNAL LSRCOMPS
      LOGICAL*1 LSRCOMPS  ! FUNCTION USED TO COMPARE STRINGS
      EXTERNAL LSRSKIPC   ! SUBROUTINE WHICH SKIPS COMMENT LINES ON DATA FILES
      INTEGER*2 J1 ! CURRENT PROFILE NUMBER
      INTEGER*2 I1 ! CURRENT HOUR, CURRENT MONTH
      INTEGER*4 DAFRN,II ! DAF-LSR-DB RECORD NUMBER
      REAL*8 TOTAL  ! 24 HOUR SUM OF LOAD IN A PROFILE
      REAL*8 TOTMON ! TOTAL WEIGHT FOR A MONTH
      REAL*8 ENDFAC(MAXMON,MAXDTP) ! FRACTION OF LOAD FOR MONTH AND DAYTYP
      REAL*4 DISTLO(24,MAXDAY,MAXMON) ! DISTRIBUTION OF ANNUAL LOAD OVER HOURS
      REAL*8 RATIO ! CURRENT ENDFAC VALUE
      INTEGER*2 PROFINDEX(MAXSEA,MAXDTP) ! INDEXES OF PROFILES
      INTEGER*4 IOSLSR ! ERROR FLAG FOR HELMPC LSRS FILE

! variables to call FILE_MGR
      CHARACTER*18 FILENM/'FILELIST'/
      INTEGER ISTATUS
      LOGICAL NEW/.FALSE./
      INTEGER FILE_MGR
      EXTERNAL FILE_MGR

      INTEGER IMSG2,IOCN2,IOLSR2,IODAF2,IOCAL2

!****************** COMMON AND EQUIVALENCE *************************************
      INCLUDE 'lsrcaldr' !<< PARAMETER DECLARATIONS
!****************** INITIALIZING VARIABLES *************************************

! call FILE_MGR first time to open up and read the list of files
      ISTATUS=FILE_MGR('I',FILENM,NEW)

! 1st pass
      CREATEDAF=.TRUE.
      LSRIDENT =.TRUE.
      FRST_PASS=1

! Loop starts here
1     CONTINUE

      ONE=1
      DO I=1,MAXSEA
        DO J=1,MAXDTP
          DAYTYPRATIO(I,J)=0.0
        ENDDO
      ENDDO
      DO I=1,MAXMON
        MONALLOFACT(I)=0.0
      ENDDO
      DO I=1,MAXNP
        DO J=1,MAXSDT
          DO K=1,2
            SEADTCO(I,J,K)=' '
          ENDDO
        ENDDO
      ENDDO
      DTRATIOSYORN=.FALSE.
      MOALLOFAYORN=.FALSE.
!****************** BODY OF THE PROGRAM/SUBPROGRAM *****************************
! open message file and control file with file_mgr
         IF(FRST_PASS .EQ.1) THEN
      NEW = .TRUE.
      FILENM = 'LDSMMSG'
      IMSG2 = FILE_MGR('O',FILENM,NEW)
         ENDIF

      WRITE(6,*)'LSRIDENT: ',LSRIDENT
      write(6,*)'CREATEDAF: ',CREATEDAF

      IF(CREATEDAF) THEN

!      open calendar file, new ldsmdaf, and initial lsr input file
          NEW = .FALSE.
          FILENM = 'LDSMCAL'
          IOCAL2 = FILE_MGR('O',FILENM,NEW)

          NEW = .TRUE.
          FILENM = 'LDSMTMP'
          IODAF2 = FILE_MGR('O',FILENM,NEW)

          NEW = .FALSE.
          FILENM = 'ALL1LSR'
          IOLSR2 = FILE_MGR('O',FILENM,NEW)


        CALL LSRRDCAL (IOCAL2,IMSG2)
         DAFRN=1
      ELSE

!      open existing ldsmdaf, and additional lsr input file to be appended
         NEW = .FALSE.
         FILENM = 'LDSMTMP'
         IODAF2 = FILE_MGR('O',FILENM,NEW)

         NEW = .FALSE.
         FILENM = 'RELDLSR'
         IOLSR2 = FILE_MGR('O',FILENM,NEW)


        READ(IODAF2,REC=1)DAFRN,NMONTH,(NODAYS(I),I=1,NMONTH), &
        ((WEIGHT(J,K),J=1,NODAYS(K)),K=1,NMONTH),NODAYT,NOSEA, &
        (MONTYP(I),I=1,NMONTH),((IDAYTQ(I,J),I=1,NODAYS(J)),J=1,NMONTH), &
        ((JDAYTP(I,J),I=1,NODAYS(J)),J=1,NMONTH),(SENAME(I),I=1,NOSEA), &
        (DTNAME(I),I=1,NODAYT),(MONAME(i),i=1,NMONTH)
      ENDIF
! DO UNTIL END OF FILE OR ERROR IS ENCOUNTERED
      IOSLSR=0
      DO WHILE (IOSLSR .EQ. 0)
! IT IS ASSUMED THAT THE NAME OF LSR IS SPECIFIED
! ON THE SECOND LINE OF LSR FILE COLUMNS 2734
        READ(IOLSR2,*,END=999,IOSTAT=IOSLSR)  ! SKIP LINE
        READ(IOLSR2,'(T27,A8)')LSRNAME
        CALL LSRSKIPC(IOLSR2,IMSG2)
! IDENTIFY LSR TYPE
        READ(IOLSR2,'(A80)')LINE
        M=LSRFIRNB(LINE,ONE)
        LSRTYP=LINE(M:M)
        IF (LSRTYP.NE.'D'.AND.LSRTYP.NE.'d') GOTO 997
! READ NORMALIZATION FLAG
        CALL LSRSKIPC(IOLSR2,IMSG2)
        READ(IOLSR2,'(A80)')LINE
        NORMALIZYORN=LSRTORF(LINE)
        CALL LSRSKIPC(IOLSR2,IMSG2)
        READ(IOLSR2,*)NUMPROF
        IF (NUMPROF.GT.MAXNP) GOTO 996
! I - CURRENT PROFILE
! READ DEFINITION OF A PROFILE
        DO I=1,NUMPROF
          CALL LSRSKIPC(IOLSR2,IMSG2)
          READ(IOLSR2,*)NUMCO(I)
! J - CURRENT SESON/DAY-TYPE COMBINATION
! READ DEFINTION OF SEASON-DATYPE COMBINATIONS FOR A CURRENT PROFILE
          DO J=1,NUMCO(I)
            CALL LSRSKIPC(IOLSR2,IMSG2)
            READ(IOLSR2,'(A80)')LINE
            K=LSRFIRNB(LINE,ONE)
! SEASON DEFINTION FIRST
            K1=1
            DO WHILE (LINE(K:K).NE.' ')
              SEADTCO(I,J,1)(K1:K1)=LINE(K:K)
              K=K+1
              K1=K1+1
            ENDDO
            K=LSRFIRNB(LINE,K)
! DAYTYPE DEFINITION NEXT
            K1=1
            DO WHILE (LINE(K:K).NE.' ')
              SEADTCO(I,J,2)(K1:K1)=LINE(K:K)
              K=K+1
              K1=K1+1
            ENDDO
          ENDDO
! READ HOURLY LOAD PROFILE
          CALL LSRSKIPC(IOLSR2,IMSG2)
          READ(IOLSR2,*)(HOURLOAD(I,K),K=1,24)
        ENDDO   ! END OF DEFININTION OF A PROFILE
        IF (NORMALIZYORN) THEN
! READ DAY-TYPE RATIOS
          CALL LSRSKIPC(IOLSR2,IMSG2)
          READ(IOLSR2,'(A80)')LINE
          DTRATIOSYORN=LSRTORF(LINE)
          IF(DTRATIOSYORN) THEN
            CALL LSRSKIPC(IOLSR2,IMSG2)
            READ(IOLSR2,*)((DAYTYPRATIO(I,J),J=1,NODAYT),I=1,NOSEA)
          ENDIF
! READ MONTHLY ALLOCATION FACTORS
          CALL LSRSKIPC(IOLSR2,IMSG2)
          READ(IOLSR2,'(A80)')LINE
          MOALLOFAYORN=LSRTORF(LINE)
          IF(MOALLOFAYORN) THEN
            CALL LSRSKIPC(IOLSR2,IMSG2)
            READ(IOLSR2,*)(MONALLOFACT(I),I=1,NMONTH)
          ENDIF
        ENDIF
!
!  CALCULATE PERCENTAGE OF ENERGY FOR EACH HOUR
!
        DO J1=1,NUMPROF
          TOTAL=0.0
          DO I1=1,24
            TOTAL=TOTAL+HOURLOAD(J1,I1)
          ENDDO
          IF (TOTAL .EQ. 0)  GO TO 150
          DO I1=1,24
            HOURLOAD(J1,I1)=HOURLOAD(J1,I1)/TOTAL
          ENDDO
150       CONTINUE
        ENDDO
        PROFINDFLAG=.FALSE.
        IF(LSRIDENT) THEN
          IF(PROFINDFLAG) GOTO 888
        ENDIF
        PROFINDFLAG=.TRUE.
        DO I=1,NOSEA
          DO J=1,NODAYT
            PROFINDEX(I,J)=0
          ENDDO
        ENDDO
!*** CREATE AN INDEX MATRIX OF 24-HOUR PROFILES
        DO I=1,NUMPROF
          DO J=1,NUMCO(I)
            DO K=1,NOSEA
              IF(LSRCOMPS(SENAME(K),SEADTCO(I,J,1))) THEN
                DO L=1,NODAYT
                  IF(LSRCOMPS(DTNAME(L),SEADTCO(I,J,2))) THEN
                    IF (PROFINDEX(K,L).NE.0) GOTO 9971
                    PROFINDEX(K,L)=I
                  ENDIF
                ENDDO
              ENDIF
            ENDDO
          ENDDO
        ENDDO
        DO K=1,NOSEA
          DO L=1,NODAYT
            IF (PROFINDEX(K,L).EQ.0) GOTO 9961
          ENDDO
        ENDDO
888     CONTINUE
!***  CALCULATE ENDFAC, THE ALLOCATION OF ANNUAL TO DAILY USE.
        TOTAL=0.0
        DO I1=1,NMONTH
          TOTAL=TOTAL+MONALLOFACT(I1)
        ENDDO
        DO I1=1,NMONTH
          MONALLOFACT(I1)=MONALLOFACT(I1)/TOTAL
        ENDDO
        DO K=1,NMONTH
          TOTMON=0.0
          DO L=1,NODAYT
            TOTMON=TOTMON+IDAYTQ(L,K)*DAYTYPRATIO(MONTYP(K),L)
          ENDDO
          DO L=1,NODAYT
            ENDFAC(K,L)=MONALLOFACT(K)*DAYTYPRATIO(MONTYP(K),L)/TOTMON
          ENDDO
        ENDDO
!****  DO THE HOURLY ALLOCATION
        DO K=1,NMONTH
          DO L=1,NODAYS(K)
            RATIO=ENDFAC(K,JDAYTP(L,K))
            DO M=1,24
              DISTLO(M,L,K)= &
              HOURLOAD(PROFINDEX(MONTYP(K),JDAYTP(L,K)),M)*RATIO
            ENDDO
          ENDDO
        ENDDO
! WRITE A RECORD ON THE DIRECT ACCESS FILE
        DAFRN=DAFRN+1
        WRITE(IODAF2,REC=DAFRN)LSRNAME, &
        (((DISTLO(M,L,K),M=1,24),L=1,NODAYS(K)),K=1,NMONTH)
      ENDDO  ! END OF PROCESSING OF CURRENT LSR
      WRITE(IMSG2,*)'<))) UNIDENTIFIED ERROR IN LSRDBMGM SUBROUTINE'
999   WRITE(IODAF2,REC=1)DAFRN,NMONTH,(NODAYS(I),I=1,NMONTH), &
        ((WEIGHT(J,K),J=1,NODAYS(K)),K=1,NMONTH),NODAYT,NOSEA, &
        (MONTYP(I),I=1,NMONTH),((IDAYTQ(I,J),I=1,NODAYS(J)),J=1,NMONTH), &
        ((JDAYTP(I,J),I=1,NODAYS(J)),J=1,NMONTH),(SENAME(I),I=1,NOSEA), &
        (DTNAME(I),I=1,NODAYT),(MONAME(i),i=1,NMONTH)
!****************** TERMINATION OF THE PROGRAM/SUBPROGRAM **********************
      WRITE(IMSG2,*)'<))) DAF-LSR-DB SUCCESSFULLY CREATED'
      WRITE(IMSG2,*)'<))) CURRENT LIST OF LSRS ON DAF-LSR-DB'
      WRITE(IMSG2,*)'        REC# LSR NAME'
      DO II=2,DAFRN
        READ(IODAF2,REC=II)LSRNAME
          WRITE(IMSG2,*)II,' ',LSRNAME
        ENDDO
      CLOSE(IOLSR2)
      CLOSE(IODAF2)
         IF(FRST_PASS .EQ.1) THEN
              FRST_PASS = 0
              CREATEDAF = .FALSE.
              GOTO 1
         ENDIF
      CLOSE(IMSG2)
      WRITE(6,*) ' DSM Preprocessor terminating normally'
      STOP
9971  WRITE(IMSG2,*)'<))) ERROR DURING CREATION OF PROFINDEX MATRIX'
      WRITE(IMSG2,*)'   MORE THAN ONE PROFILE ASSIGNED TO COMABINATION:'
      WRITE(IMSG2,*)'     SEASON: ',SENAME(K),' DAY-TYPE: ',DTNAME(L)
      WRITE(IMSG2,*)'     LSR NAME: ',LSRNAME
      WRITE(IMSG2,*)'<))) PROGRAM TERMINATED'
      WRITE(6,*) ' DSM Preprocessor run failed - check LDSMMSG file'
      STOP
9961  WRITE(IMSG2,*)'<))) ERROR DURING CREATION OF PROFINDEX MATRIX'
      WRITE(IMSG2,*)'     NO PROFILE ASSIGNED TO COMBINATION:'
      WRITE(IMSG2,*)'     SEASON: ',SENAME(K),' DAY-TYPE: ',DTNAME(L)
      WRITE(IMSG2,*)'     LSR NAME: ',LSRNAME
      WRITE(IMSG2,*)'<))) PROGRAM TERMINATED'
      WRITE(6,*) ' DSM Preprocessor run failed - check LDSMMSG file'
      STOP
998   WRITE(IMSG2,*)'<))) ERROR ON FILE WITH HELMPC FORMAT LSRS.'
      WRITE(IMSG2,*)'<))) REASON:',IOSLSR,' PROGRAM TERMINATED'
      WRITE(6,*) ' DSM Preprocessor run failed - check LDSMMSG file'
      STOP
993   WRITE(IMSG2,*)'<))) ERROR ON CONTROL FILE.'
      WRITE(IMSG2,*)'<))) PROGRAM TERMINATED'
      WRITE(6,*) ' DSM Preprocessor run failed - check LDSMMSG file'
      STOP
997   WRITE(IMSG2,*)'<))) LSR: ',LSRNAME,' TYPE SPEC.: ',LSRTYP
      WRITE(IMSG2,*)'<))) PROGRAM TERMINATED'
      WRITE(6,*) ' DSM Preprocessor run failed - check LDSMMSG file'
      STOP
996   WRITE(IMSG2,*)'<))) WRONG NUMBER OF PROFILES: ',NUMPROF
      WRITE(IMSG2,*)'<))) PROGRAM TERMINATED'
      WRITE(6,*) ' DSM Preprocessor run failed - check LDSMMSG file'
      STOP
994   WRITE(IMSG2,*)'<))) ERROR OPENING DIRECT ACCESS FILE'
      WRITE(IMSG2,*)'<))) PROGRAM TERMINATED'
      WRITE(6,*) ' DSM Preprocessor run failed - check LDSMMSG file'
      STOP
      END
       SUBROUTINE LSRRDCAL (IOCAL2,IMSG2)
!****************** DESCRIPTION OF THE PROGRAM/SUBPROGRAM **********************
! THIS SUBROUTINE READS ONE HELM-PC FORMAT CALENDAR FILE TO BE USED WITH
! ALL LSR FILES USED FOR CREATION OF DAF-LSR-DB.
!-------------------------------------------------------------------------------
!     WRITTEN BY ADAM KRECZKO, ICF RESOURCES, INC., PHONE: (703)-934-3353
!*******************************************************************************
      IMPLICIT NONE
!****************** TYPING, DECLARING AND INITIALIZING CONSTANT PARAMENTERS ****
      INCLUDE 'lsrmgpar' !<< PARAMETER DECLARATIONS
!****************** TYPING AND DECLARING VARIABLES *****************************
      INTEGER IOCAL2,IMSG2
      EXTERNAL LSRFIRNB
      INTEGER*2 LSRFIRNB ! FUNCTION TO FIND POSITION OF FIRST NON-BLANC CH.
      CHARACTER*82 BUFFER ! CHARACTER BUFFER
      CHARACTER*13 BUFFER1 ! CHARACTER BUFFER
      EXTERNAL LSRCOMPS
      LOGICAL*1 LSRCOMPS ! FUNCTION TO COMPARE STRINGS
      INTEGER*2 L,L1,K,J,J1,I,M ! UNIVERSAL COUNTERS
      INTEGER NDAYYEAR ! NUMBER OF DAYS IN A CALENDAR YEAR
!****************** COMMON AND EQUIVALENCE *************************************
      INCLUDE 'lsrcaldr' !<< CALENDAR VARIABLES
!****************** INITIALIZING VARIABLES *************************************
      NDAYYEAR=0
      BUFFER=' '
      BUFFER1=' '
      DO I=1,MAXMON
        MONAME(I)=' '
      ENDDO
      DO I=1,MAXSEA
        SENAME(I)=' '
      ENDDO
      DO I=1,MAXDTP
        DTNAME(I)=' '
      ENDDO
!****************** BODY OF THE PROGRAM/SUBPROGRAM *****************************
! OPEN CALENDAR FILE
!      OPEN(UNIT=IOCAL2,FILE='y90dt361.cal',ERR=993)
! READ THE CALENDAR FILE
      CALL LSRSKIPC(IOCAL2,IMSG2)
       READ(IOCAL2,'(A80)')BUFFER
       WRITE(IMSG2,*)'FROM BUFFER: ',BUFFER
       BACKSPACE IOCAL2
      READ(IOCAL2,*)NMONTH
      CALL LSRSKIPC(IOCAL2,IMSG2)
      DO I=1,NMONTH
        CALL LSRSKIPC(IOCAL2,IMSG2)
        READ(IOCAL2,'(A80)')BUFFER
        L=1
        L1=L
        L=LSRFIRNB(BUFFER,L1)
!*** READ MONTH NAME
        K=1
        DO WHILE (BUFFER(L:L).NE.' ' .AND. L.LE.80)
          MONAME(I)(K:K)=BUFFER(L:L)
          L=L+1
          K=K+1
        ENDDO
        L1=L
        L=LSRFIRNB(BUFFER,L1)
!*** READ SEASON NAME
        BUFFER1=' '
        K=1
        DO WHILE (BUFFER(L:L).NE.' ' .AND. L.LE.80)
          BUFFER1(K:K)=BUFFER(L:L)
          L=L+1
          K=K+1
        ENDDO
!*** IDENTIFY SEASON TYPE FOR A MONTH AND ADD NEW ONE IF DOESN'T EXIST YET
        K=1
        DO WHILE (SENAME(K).NE. ' ')
          IF(LSRCOMPS(BUFFER1,SENAME(K))) GOTO 700
          K=K+1
          IF(K.GT.MAXSEA) GOTO 998
        ENDDO
        SENAME(K)=BUFFER1
        NOSEA=K
700     MONTYP(I)=K
!*** READ NUMBER OF DAYS IN A MONTH
        DO K=1,L
          BUFFER(K:K)=' '
        ENDDO
        READ(BUFFER,'(40X,I2)')NODAYS(I)
        NDAYYEAR=NDAYYEAR+NODAYS(I)
      ENDDO
      CALL LSRSKIPC(IOCAL2,IMSG2)
      READ(IOCAL2,*)NODAYT
      DO I=1,NODAYT
        CALL LSRSKIPC(IOCAL2,IMSG2)
        READ(IOCAL2,'(A80)')BUFFER
        L=1
        K=1
        L1=L
        L=LSRFIRNB(BUFFER,L1)
        DO WHILE (BUFFER(L:L).NE.' ' .AND. L.LE.80)
          DTNAME(I)(K:K)=BUFFER(L:L)
          L=L+1
          K=K+1
        ENDDO
      ENDDO
      DO I=1,NMONTH
        J=1
        DO WHILE (J .LE. NODAYS(I))
          CALL LSRSKIPC(IOCAL2,IMSG2)
          READ(IOCAL2,'(A80)',END=999)BUFFER
          L=1
          DO WHILE(L .LE. 80)
            DO WHILE(BUFFER(L:L).EQ.' ' .AND. L .LE. 80)
              L=L+1
            ENDDO
            BUFFER1=' '
            K=1
            DO WHILE(BUFFER(L:L).NE.' ' .AND. L .LE. 80)
              BUFFER1(K:K)=BUFFER(L:L)
              K=K+1
              L=L+1
            ENDDO
            IF (K.GT.1) THEN
              DO M=1,NODAYT
                IF(LSRCOMPS(BUFFER1,DTNAME(M))) GOTO 500
              ENDDO
              GOTO 999
500           JDAYTP(J,I)=M
              J=J+1
            ENDIF
          ENDDO
        ENDDO
      ENDDO
      IF (NDAYYEAR.LT.365) THEN
        DO I=1,NMONTH
          CALL LSRSKIPC(IOCAL2,IMSG2)
          READ(IOCAL2,*)(WEIGHT(J,I),J=1,NODAYS(I))
        ENDDO
      ELSE
        DO I=1,NMONTH
          DO J=1,NODAYS(I)
            WEIGHT(J,I)=1
          ENDDO
        ENDDO
      ENDIF
      DO I=1,NMONTH
        DO J1=1,NODAYT
          IDAYTQ(J1,I)=0
        ENDDO
        DO J=1,NODAYS(I)
          IDAYTQ(JDAYTP(J,I),I)=IDAYTQ(JDAYTP(J,I),I)+WEIGHT(J,I)
        ENDDO
      ENDDO
      CLOSE(IOCAL2)
!****************** TERMINATION OF THE PROGRAM/SUBPROGRAM **********************
      RETURN
999   WRITE(IMSG2,*)'<))) ERROR WHILE EXECUTING SUBROUTINE LSRRDCAL'
      WRITE(IMSG2,*)'     INVALID DAY-TYPE: ',BUFFER1,' ENCOUNTERED'
      WRITE(IMSG2,*)'     PROGRAM EXECUTION TERMINATED'
998   WRITE(IMSG2,*)'<))) ERROR WHILE EXECUTING SUBROUTINE LSRRDCAL'
      WRITE(IMSG2,*)'     TOO MANY SEASONS DETECTED, MAXSEA=',MAXSEA
      WRITE(IMSG2,*)'     PROGRAM EXECUTION TERMINATED'
993   WRITE(IMSG2,*)'<))) ERROR WHILE EXECUTING SUBROUTINE LSRRDCAL'
      WRITE(IMSG2,*)'     UNABLE TO OPEN THE CALENDAR FILE'
      STOP
      END
      LOGICAL FUNCTION LSRCOMPS(STR1,STR2)
!****************** DESCRIPTION OF THE PROGRAM/SUBPROGRAM **********************
! THIS FUNCTION COMPARES TWO STRINGS. IF ANY OF THE TWO STRINGS IS INCLUDED IN
! IN THE OTHER, THE VALUE IS .TRUE., OTHERWISE .FALSE.
!-------------------------------------------------------------------------------
!     DEVELOPED BY ADAM KRECZKO, ICF RESOURCES, INC., PHONE: (703)-934-3353
!*******************************************************************************
      IMPLICIT NONE
!****************** TYPING, DECLARING AND INITIALIZING CONSTANT PARAMENTERS ****
!****************** TYPING AND DECLARING VARIABLES *****************************
      CHARACTER*(*) STR1 ! STRING 1
      CHARACTER*(*) STR2 ! STRING 2
!****************** COMMON AND EQUIVALENCE *************************************
!****************** INITIALIZING VARIABLES *************************************
      LSRCOMPS=.TRUE.
!****************** BODY OF THE PROGRAM/SUBPROGRAM *****************************
      LSRCOMPS=.FALSE.
      IF(LEN(STR1).GT.LEN(STR2)) THEN
        IF(INDEX(STR1,STR2).EQ.1) LSRCOMPS=.TRUE.
      ELSE
        IF(INDEX(STR2,STR1).EQ.1) LSRCOMPS=.TRUE.
      ENDIF
!****************** TERMINATION OF THE PROGRAM/SUBPROGRAM **********************
      RETURN
      END
      INTEGER FUNCTION LSRFIRNB(STRTOPROC,FIRSTCOL)
!****************** DESCRIPTION OF THE PROGRAM/SUBPROGRAM **********************
!* THIS FUNCTION RETURNS POSITION OF THE FIRST NON-BLANK CHARACTER IN THE
!* STRING STRTOPROC
!-------------------------------------------------------------------------------
!     DEVELOPED BY ADAM KRECZKO, ICF RESOURCES, INC., PHONE: (703)-934-3353
!******************************************************************************
      IMPLICIT NONE
!****************** TYPING AND DECLARING VARIABLES *****************************
      INCLUDE 'lsrmgpar' !<< PARAMETER DECLARATIONS
      CHARACTER*80 STRTOPROC  ! STRING TO BE PROCESSED
      INTEGER*2 FIRSTCOL      ! STARTING COLUMN
      INTEGER*2 I           ! TEMPORARY VARIABLE
!****************** BODY OF THE PROGRAM/SUBPROGRAM *****************************
      I=FIRSTCOL
      DO WHILE (STRTOPROC(I:I) .EQ. ' ')
        I=I+1
      ENDDO
      LSRFIRNB=I
!****************** TERMINATION OF THE PROGRAM/SUBPROGRAM **********************
      RETURN
      END
       SUBROUTINE LSRSKIPC(FILE,IMSG2)
!****************** DESCRIPTION OF THE PROGRAM/SUBPROGRAM **********************
! THIS SUBROUTINE READS A DATA LINE AND SKIPS THE COMMENTS
! THEN IT POSITIONS THE NEXT READ AT THE NEXT LINE OF REAL DATA
!-------------------------------------------------------------------------------
!     DEVELOPED BY ADAM KRECZKO, ICF RESOURCES, INC., PHONE: (703)-934-3428
!*******************************************************************************
       IMPLICIT NONE
!****************** TYPING, DECLARING AND INITIALIZING CONSTANT PARAMENTERS ****
!****************** COMMON AND EQUIVALENCE *************************************
       INCLUDE 'lsrmgpar' !<< PARAMETER DECLARATIONS
!****************** TYPING AND DECLARING VARIABLES *****************************
       INTEGER*4 FILE,IMSG2   ! NAME OF THE FILE,unit of message file
       CHARACTER*1 LINE ! CURRENT CHARACTER
       CHARACTER*1 STAR ! CHARACTER VARIABLE REPRESENTING A COMMENT SYMBOL
!****************** INITIALIZING VARIABLES *************************************
       DATA STAR/'*'/
       LINE=STAR
!****************** BODY OF THE PROGRAM/SUBPROGRAM *****************************
       DO WHILE(LINE.EQ.STAR)
         READ(FILE,'(A1)',END=20)LINE
       ENDDO
       BACKSPACE FILE
!****************** TERMINATION OF THE PROGRAM/SUBPROGRAM **********************
       RETURN
   20  WRITE(IMSG2,*)'<))) UNEXPECTED EOF - READING FILE: ',FILE
       WRITE(IMSG2,*)'<))) PROGRAM TERMINATED BY SUBROUTINE DSMSKIPC'
       STOP
       END
      LOGICAL FUNCTION LSRTORF(STRING)
!****************** DESCRIPTION OF THE PROGRAM/SUBPROGRAM **********************
! THIS FUNCTION RETURNS LOGICAL FALSE IF A CHARACTER IS N,N,F,N,0 AND
! LOGICAL TRUE OTHERWISE
!-------------------------------------------------------------------------------
!     DEVELOPED BY ADAM KRECZKO, ICF RESOURCES, INC., PHONE: (703)-934-3353
!******************************************************************************
      IMPLICIT NONE
      INCLUDE 'lsrmgpar' !<< PARAMETER DECLARATIONS
!****************** TYPING AND DECLARING VARIABLES *****************************
      CHARACTER*(*) STRING
!****************** BODY OF THE PROGRAM/SUBPROGRAM *****************************
      IF    (INDEX(STRING,'N')+INDEX(STRING,'n')+INDEX(STRING,'F') &
            +INDEX(STRING,'f')+INDEX(STRING,'0').NE.0) THEN
        LSRTORF=.FALSE.
      ELSE
        LSRTORF=.TRUE.
      ENDIF
!****************** TERMINATION OF THE PROGRAM/SUBPROGRAM **********************
      RETURN
      END
