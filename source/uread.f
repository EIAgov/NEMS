! $Header: m:/default/source/RCS/uread.f,v 1.23 2019/06/27 11:49:16 xj2 Exp $
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!     GENUTIL SUBMODULE
!
!     CONTAINS SUBROUTINES USED TO READ DATA FROM EXTERNAL FLAT FILES
!     INTO THE EMM COMMON BLOCKS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!*******************************************************************
!     NAME: RD$TBL
!     PURPOSE:
!     READS A TABLE OF DATA FROM A SEQUENTIAL INPUT FILE
!     INTO AN INTERNAL CHARACTER BUFFER WITH 132 BYTE DATA LINES.
!     THE HEADER ROW HAS AN 12 CHAR UNIQUE NAME STARTING IN COL 1.
!     +++++++++++++
!     THE HEADER MUST BE AT LEAST 12 CHAR, OR IT MIGHT NOT READ CORRECTLY
!     +++++++++++++
!     THIS IS FOLLOWED BY NROW IDENTICALLY FORMATTED DATA ROWS
!     COMMENT LINES MUST BEGIN IN COL 1 WITH AN # AND MAY BE ANYWHERE
!     AND ARE IGNORED AS DATA.
!     HEADER LINES MUST BEGIN IN COL 1 WITH AN $ AND BEGIN A NEW
!     "DATA TABLE".
!     VARIABLES READ FROM FILE MUST BE 4 BYTE INT, 4 OR 8 BYTE REAL,
!     OR A CHARACTER STRING (OF ANY LENGTH)
!     DATA COLUMNS MUST BE FULLY FILLED IN (I.E., NO ASSUMED 0, 0.00
!     OR ' '); IF NOT FILLED IN, IT IS NOT READ INTO MODEL.
!     FOR MULTI-COLUMN TABLES, ALL COLUMNS SPECIFIED MUST HAVE DATA VALUES
!     OR AN "END" LABEL CAN INDICATE THE END OF THE RECORD, SO THAT MULTI-COLUMN
!     TABLES THAT WOULD HAVE 0, 0.0 OR '' IN THE TRAILING COLUMNS AT THE END OF
!     THE RECORD NEED NOT HAVE THIS SUPERFLUOUS DATA SPECIFIED.
!     AUTHOR: WAYNE COOPER
!     MODIFICATION LOG:
!     CREATED 2/1/95
!     MSG PARAMETER ADDED AND END LABEL SUPPORTED 6/13/95
!*******************************************************************
      FUNCTION RD$TBL(INFIL,BLKLBL,NLINES,DEBUG,MSGFIL)
      IMPLICIT NONE
      include 'emmbuf'                                           !//mf//
      include 'parametr'
      include 'ncntrl'
      INTEGER*4     INFIL                            ! INPUT FILE UNIT #
      INTEGER*4     MSGFIL                         ! MESSAGE FILE UNIT #
      INTEGER*4     DEBUG                              ! IF =0, NO DEBUG
                             ! IF >0, IS THE DEBUG UNIT #
      INTEGER*4     NLINES        ! NUMBER OF LINES IN DATA TABLE TO USE
      INTEGER*4     CROW               ! CURRENT ROW INDEX IN DATA TABLE
      INTEGER*4     RD$TBL                                 ! RETURN CODE
      INTEGER*4     NSKIP            ! # LINES SKIPPED IN PREVIOUS TABLE
      INTEGER*4     I,J,TEST                                   ! INDICES
      LOGICAL EQHEAD           ! CURRENT DATA TABLE HEAD IS SAME AS CALL
      LOGICAL EXTRA              ! EXTRA DATA EXISTS FROM PREVIOUS TABLE
      CHARACTER*12  BLKLBL            ! DATA HEADER FOR TABLE TO READ IN
      CHARACTER*12  OBLKLAB                      ! PREVIOUS TABLE HEADER
      CHARACTER*240  LINE                       ! CURRENT INPUT DATA LINE
!
      STDERR = 6                          ! SET FOR ALL USE OF FUNCTIONS
      MSG = MSGFIL
!
!     CHECK PREVIOUS USE OF BUFFER FOR REMAINING UNUSED DATA
!
      IF (TABROWS .GT. 0) THEN           ! IGNORE FIRST CALL TO FUNCTION
         DO I = 1, TABROWS
            EXTRA = .FALSE.
            DO J = 1, LEN(INTAB(I))
               TEST = INDEX('                          !',INTAB(I)(J:J))
               IF (TEST .EQ. 2) THEN
                  GOTO 20               ! EXIT IF FIRST NON-BLANK IS "!"
               ELSE IF (TEST .EQ. 0) THEN
                  EXTRA = .TRUE.
                  GOTO 20                          ! PRINT OUT NON-BLANK
               END IF
            END DO
20          IF (EXTRA) THEN
               WRITE(MSG,'(3A)') ' *** WARNING: NON-BLANK CHARS IN TABLE: ',BLKLAB, &
                                 ' NOT READ TO MODEL VARS ***'
               WRITE(MSG,400) INTAB(I)
            END IF
         END DO
      END IF
!
!     ASSIGN GLOBAL VARIABLES
!
      INFLL = INFIL
      DBUG = DEBUG
      DBUGFIL = DEBUG
      TABROWS = NLINES
      OBLKLAB = BLKLAB
      BLKLAB = BLKLBL
      COL = 0
      DO I = 1, NLN
         INTAB(I) = ' '
      END DO
      LINE = '            '
      NSKIP = 0
      EQHEAD = .FALSE.
!
      IF (DBUG .GT. 0 .AND. PRTDBGE .EQ. 4) &
          WRITE(DBUGFIL,'(2A)') ' *** BEGIN RD$TBL FOR TABLE: ',BLKLAB
!
!     READ UNTIL SPECIFIC DATA HEADER IS FOUND
!
      DO WHILE ( .NOT. EQHEAD)
         READ(UNIT = INFLL,FMT = 500,END = 1000) LINE
         IF (LINE(1:12) .EQ. BLKLAB) THEN                 ! VALID HEADER
            EQHEAD = .TRUE.
         ELSEIF (LINE(1:1).NE.'%' .AND. LINE(1:1).NE.'#') THEN ! DATA LINE
            NSKIP = NSKIP + 1
            IF (DBUG .GT. 0 .AND. &
             PRTDBGE .EQ. 4) WRITE(DBUGFIL,300) LINE
            ELSEIF (LINE(1:1).EQ.'%') THEN              ! INVALID HEADER
               WRITE(MSG,'(5A)') ' *** ERR: TABLE HEAD IN FILE SKIPPED:',LINE(1:12),':',BLKLAB,':***'
            IF (DBUG .GT. 0 .AND. PRTDBGE .EQ. 4)WRITE(MSG,200) LINE
            ENDIF
         END DO
      IF (NSKIP .GT. 0) THEN
         WRITE(MSG,'(A,I4,2A)') ' *** WARNING: ',NSKIP,' ROWS SKIPPED FROM PREVIOUS TABLE: ',OBLKLAB
      END IF
!
!     VALIDATE PASSED PARAMETERS AFTER READING HEADER LINE
!
      IF (TABROWS .EQ. 0) THEN
         WRITE(MSG,'(3A)') ' *** WARNING: NUM ROWS IN RD$TBL FOR TABLE: ', &
                           BLKLAB,' IS 0: NO DATA TO READ ***'
         RD$TBL = 1
         RETURN
      ELSEIF (TABROWS .GT. NLN) THEN
         WRITE(STDERR,'(3A)') ' *** ERR: NUM ROWS SPECIFIED IN RD$TBL FOR TABLE: ', &
                      BLKLAB,' EXCEEDS MAXIMUM ALLOWED IN BUFFER: EXIT ***'
         RD$TBL = 104
         RETURN
      END IF
!
!     READ IN SPECIFIC DATA TABLE
!
      DO CROW = 1,TABROWS
!        SKIP AND DON'T COUNT COMMENT LINES
10       READ(UNIT = INFLL,FMT = 500,END = 1000) LINE
         IF (LINE(1:1).EQ.'%') THEN
            WRITE(STDERR,'(3A)') ' *** ERR: TOO FEW LINES IN TABLE ',BLKLAB,' ***'
            BACKSPACE (UNIT = INFLL)
            RD$TBL = 102
            RETURN
         ELSEIF (LINE(1:1).EQ.'#') THEN
            GOTO 10
         ELSE
            INTAB(CROW) = LINE
         END IF
      END DO
      WRITE(MSG,'(3A)') ' *** TABLE: ',BLKLAB,' READ CORRECTLY ***'
      RD$TBL = 1
      RETURN
1000  WRITE(STDERR,'(3A)') ' *** ERR: PREMATURE EOF IN TABLE: ',BLKLAB,' ***'
      RD$TBL = 103
200   FORMAT('*** BLOCK HEAD NOT USED: ',A240)
300   FORMAT('*** SKIPPED DATA LINE  : ',A240)
400   FORMAT('*** EXTRA DATA IN BUF  : ',A240)
500   FORMAT(A240)
      RETURN
      END
!
!*******************************************************************
!     NAME: RD$I1
!     PURPOSE:  READ A 1 DIMENSIONAL INTEGER*4 VARIABLE
!     READ A 1ST DATA COLUMN FROM A DATA BUFFER PREVIOUSLY
!     FILLED BY RD$TBL. AFTER COLUMN IS READ, DROP DATA FROM
!     BUFFER SO NEXT CALL READS NEXT COLUMN
!     EXIT IF DATA ERR ARE ENCOUNTERED
!     AUTHOR: WAYNE COOPER
!*******************************************************************
      FUNCTION RD$I1(VAR,ROWLB,NDIM1)
      IMPLICIT NONE
      include 'emmbuf'                                           !//mf//
      include 'parametr'
      include 'ncntrl'
      INTEGER*4 ROWLB      ! 1ST ELE IN ARRAY FILLED BY 1ST ELE IN TABLE
      INTEGER*4 NDIM1                  ! NUMBER OF ELEMENTS IN VAR ARRAY
      INTEGER*4 RD$I1                                      ! RETURN CODE
      INTEGER*4 INDX1,INDX2,TEST,IND,ROW                       ! INDICES
      INTEGER*4 VAR(NDIM1)            ! VARIABLE FILLED FROM DATA COLUMN
      CHARACTER*10 TESTSTR           ! VALID CHARACTERS IN INTEGER FIELD
      LOGICAL LASTCOL                  ! =1 IF LAST COLUMN IN DATA TABLE
      DATA TESTSTR /'0123456789'/
!
      RD$I1 = 1
      COL = COL + 1
      IF ((ROWLB + TABROWS - 1) .GT. NDIM1) THEN
         WRITE(STDERR,'(3A,I4)') ' *** ERR: NUM ROWS EXCEEDS SIZE OF ARRAY IN TABLE: ', &
                                 BLKLAB,' COL: ',COL
         RD$I1 = 103
         RETURN
      END IF
      DO ROW = 1,TABROWS
         LASTCOL = .FALSE.
         INDX1 = 9999
         DO IND = 1,LEN(TESTSTR)                 ! FIND FIRST VALID CHAR
            TEST = INDEX(INTAB(ROW),TESTSTR(IND:IND))
            IF (TEST .GT. 0) INDX1 = MIN(INDX1,TEST)
            END DO
         IF (INDX1 .EQ. 9999) THEN          ! NO INTEGER FIELD: SKIP ROW
!           ROW TERMINATES WITH "END" LABEL BEFORE ARRAY END: SKIP ROW
            TEST = INDEX(INTAB(ROW),' END')
            IF (TEST .GT. 0) THEN
               WRITE(MSG,'(3A,I4,A,I4)') ' ***WARNING: DATA ROW HAS END LABEL BEF ARRAY END FOR: ', &
                                         BLKLAB,' ROW: ',ROW, ' COL: ',COL
               RD$I1 = 104
               INTAB(ROW)(TEST+1:TEST+3) = '   '  ! NULL OUT "END" LABEL TO CLEAR BUFFER
               GOTO 10
            END IF
           WRITE(STDERR,'(3A,I4,A,I4)') ' *** ERR: COLUMN DOES NOT EXIST IN TABLE: ', &
                                        BLKLAB,' ROW: ',ROW,' COL: ',COL
            RD$I1 = 101
            GOTO 10
         END IF
         INDX2 = INDEX(INTAB(ROW)(INDX1:),' ')
         IF (INDX2 .EQ. 0) THEN
            INDX2 = LEN(INTAB(ROW))
            LASTCOL = .TRUE.
         END IF
         INDX2 = INDX2 + INDX1 - 2
         DO IND = INDX1,INDX2                    ! CHECK FOR VALID FIELD
            TEST = INDEX(TESTSTR,INTAB(ROW)(IND:IND))
            IF (TEST .EQ. 0) THEN                ! EXIT IF INVALID FIELD
               WRITE(STDERR,'(3A,I4,A,I4)') ' *** ERR:NON-INTEGER VALUE IN TABLE: ', &
                                            BLKLAB,' ROW: ',ROW,' COL: ',COL
               RD$I1 = 102
               GOTO 10
            END IF
         END DO
         READ(INTAB(ROW)(INDX1:INDX2), * ) VAR(ROW + ROWLB - 1) ! READ VAL
         IF ( .NOT. LASTCOL) &
             INTAB(ROW) = INTAB(ROW)(INDX2 + 1:)   ! DROP LEADING COLUMN
         IF (DBUG .GT. 0 .AND. PRTDBGE .EQ. 4) WRITE(DBUGFIL,100) &
             BLKLAB,COL,ROW + ROWLB - 1,VAR(ROW + ROWLB - 1)
10    CONTINUE
      END DO
100   FORMAT(1X,' TABLE: ',A12,' COL: ',I3,' ROW: ',I3,' VAL: ',I10)
      RETURN
      END
!
!*******************************************************************
!     NAME: RD$R1
!     PURPOSE:  READ A 1 DIMENSIONAL REAL*4 VARIABLE
!     READ A 1ST DATA COLUMN FROM A DATA BUFFER PREVIOUSLY
!     FILLED BY RD$TBL. AFTER COLUMN IS READ, DROP DATA FROM
!     BUFFER SO NEXT CALL READS NEXT COLUMN
!     EXIT IF DATA ERR ARE ENCOUNTERED
!     AUTHOR: WAYNE COOPER
!*******************************************************************
      FUNCTION RD$R1(VAR,ROWLB,NDIM1)
      IMPLICIT NONE
      include 'emmbuf'                                           !//mf//
      include 'parametr'
      include 'ncntrl'
      INTEGER*4 ROWLB      ! 1ST ELE IN ARRAY FILLED BY 1ST ELE IN TABLE
      INTEGER*4 NDIM1                  ! NUMBER OF ELEMENTS IN VAR ARRAY
      INTEGER*4 RD$R1                                      ! RETURN CODE
      INTEGER*4 INDX1,INDX2,TEST,IND,ROW                       ! INDICES
      REAL*4 VAR(NDIM1)               ! VARIABLE FILLED FROM DATA COLUMN
      CHARACTER*13 TESTSTR             ! VALID CHARACTERS FOR REAL FIELD
      LOGICAL LASTCOL                  ! =1 IF LAST COLUMN OF DATA TABLE
      DATA TESTSTR /'+-.0123456789'/
!
      RD$R1 = 1
      COL = COL + 1
      IF ((ROWLB + TABROWS - 1) .GT. NDIM1) THEN
         WRITE(STDERR,'(3A,I4)') ' *** ERR: NUM ROWS EXCEEDS SIZE OF ARRAY IN TABLE: ', &
                      BLKLAB,' COL: ',COL
         RD$R1 = 103
         RETURN
      END IF
!
      DO ROW = 1,TABROWS
         LASTCOL = .FALSE.
         INDX1 = 9999
         DO IND = 1,LEN(TESTSTR)                 ! FIND FIRST VALID CHAR
            TEST = INDEX(INTAB(ROW),TESTSTR(IND:IND))
            IF (TEST .GT. 0) INDX1 = MIN(INDX1,TEST)
            END DO
         IF (INDX1 .EQ. 9999) THEN          ! NO INTEGER FIELD: SKIP ROW
!           ROW TERMINATES WITH "END" LABEL BEFORE ARRAY END: SKIP ROW
            TEST = INDEX(INTAB(ROW),' END')
            IF (TEST .GT. 0) THEN
               WRITE(MSG,'(3A,I4,A,I4)') ' ***WARNING: DATA ROW HAS END LABEL BEF ARRAY END FOR: ', &
                            BLKLAB,' ROW: ',ROW,' COL: ',COL
               INTAB(ROW)(TEST+1:TEST+3) = '   '  ! NULL OUT "END" LABEL TO CLEAR BUFFER
               RD$R1 = 104
               GOTO 10
            END IF
            WRITE(STDERR,'(3A,I4,A,I4)') ' *** ERR: COLUMN DOES NOT EXIST IN TABLE: ',BLKLAB,' ROW: ',ROW,' COL: ',COL
            RD$R1 = 101
            GOTO 10
         END IF
         INDX2 = INDEX(INTAB(ROW)(INDX1:),' ')
         IF (INDX2 .EQ. 0) THEN
            INDX2 = LEN(INTAB(ROW))
            LASTCOL = .TRUE.
         END IF
         INDX2 = INDX2 + INDX1 - 2
         DO IND = INDX1,INDX2        ! CHECK BYTES OF FIELD FOR VALIDITY
            TEST = INDEX(TESTSTR,INTAB(ROW)(IND:IND))
            IF (TEST .EQ. 0) THEN           ! CHECK IF COLUMN IS INVALID
               WRITE(STDERR,'(3A,I4,A,I4)') ' *** ERR: NON-REAL VALUE IN TABLE: ', &
                                             BLKLAB,' ROW: ',ROW,' COL: ',COL
               RD$R1 = 102
               GOTO 10
            END IF
         END DO
         READ(INTAB(ROW)(INDX1:INDX2), * ) VAR(ROW + ROWLB - 1) ! READ VAL
         IF ( .NOT. LASTCOL) &
             INTAB(ROW) = INTAB(ROW)(INDX2 + 1:)   ! DROP LEADING COLUMN
         IF (DBUG .GT. 0 .AND. PRTDBGE .EQ. 4) WRITE(DBUGFIL,100) &
             BLKLAB,COL,ROW + ROWLB - 1,VAR(ROW + ROWLB - 1)
10    CONTINUE
      END DO
100   FORMAT(1X,' TABLE: ',A12,' COL: ',I3,' ROW: ',I3,' VAL: ',G12.5)
      RETURN
      END
!
!*******************************************************************
!     NAME: RD$R81
!     PURPOSE:  READ A 1 DIMENSIONAL REAL*8 VARIABLE
!     READ A 1ST DATA COLUMN FROM A DATA BUFFER PREVIOUSLY
!     FILLED BY RD$TBL. AFTER COLUMN IS READ, DROP DATA FROM
!     BUFFER SO NEXT CALL READS NEXT COLUMN
!     EXIT IF DATA ERR ARE ENCOUNTERED
!     AUTHOR: WAYNE COOPER
!*******************************************************************
      FUNCTION RD$R81(VAR,ROWLB,NDIM1)
      IMPLICIT NONE
      include 'emmbuf'                                           !//mf//
      include 'parametr'
      include 'ncntrl'
      INTEGER*4 ROWLB      ! 1ST ELE IN ARRAY FILLED BY 1ST ELE IN TABLE
      INTEGER*4 NDIM1                  ! NUMBER OF ELEMENTS IN VAR ARRAY
      INTEGER*4 RD$R81                                     ! RETURN CODE
      INTEGER*4 INDX1,INDX2,TEST,IND,ROW                       ! INDICES
      REAL*8 VAR(NDIM1)               ! VARIABLE FILLED FROM DATA COLUMN
      CHARACTER*13 TESTSTR             ! VALID CHARACTERS FOR REAL FIELD
      LOGICAL LASTCOL                  ! =1 IF LAST COLUMN OF DATA TABLE
      DATA TESTSTR /'+-.0123456789'/
!
      RD$R81 = 1
      COL = COL + 1
      IF ((ROWLB + TABROWS - 1) .GT. NDIM1) THEN
         WRITE(STDERR,'(3A,I4)') ' *** ERR: NUM ROWS EXCEEDS SIZE OF ARRAY IN TABLE: ', &
                                 BLKLAB,' COL: ',COL
         RD$R81 = 103
         RETURN
      END IF
      DO ROW = 1,TABROWS
         LASTCOL = .FALSE.
         INDX1 = 9999
         DO IND = 1,LEN(TESTSTR)                 ! FIND FIRST VALID CHAR
            TEST = INDEX(INTAB(ROW),TESTSTR(IND:IND))
            IF (TEST .GT. 0) INDX1 = MIN(INDX1,TEST)
            END DO
         IF (INDX1 .EQ. 9999) THEN          ! NO INTEGER FIELD: SKIP ROW
!           ROW TERMINATES WITH "END" LABEL BEFORE ARRAY END: SKIP ROW
            TEST = INDEX(INTAB(ROW),' END')
            IF (TEST .GT. 0) THEN
               WRITE(MSG,'(3A,I4,A,I4)') ' ***WARNING: DATA ROW HAS END LABEL BEF ARRAY END FOR: ', &
                         BLKLAB,' ROW: ',ROW,' COL: ',COL
               INTAB(ROW)(TEST+1:TEST+3) = '   '  ! NULL OUT "END" LABEL TO CLEAR BUFFER
               RD$R81 = 104
               GOTO 10
            END IF
            WRITE(STDERR,'(3A,I4,A,I4)') ' *** ERR: COLUMN DOES NOT EXIST IN TABLE: ', &
                         BLKLAB,' ROW: ',ROW,' COL: ',COL
            RD$R81 = 101
            GOTO 10
         END IF
         INDX2 = INDEX(INTAB(ROW)(INDX1:),' ')
         IF (INDX2 .EQ. 0) THEN
            INDX2 = LEN(INTAB(ROW))
            LASTCOL = .TRUE.
         END IF
         INDX2 = INDX2 + INDX1 - 2
         DO IND = INDX1,INDX2        ! CHECK BYTES OF FIELD FOR VALIDITY
            TEST = INDEX(TESTSTR,INTAB(ROW)(IND:IND))
            IF (TEST .EQ. 0) THEN           ! CHECK IF COLUMN IS INVALID
               WRITE(STDERR,'(3A,I4,A,I4)') ' *** ERR: NON-REAL VALUE IN TABLE: ', &
                            BLKLAB,' ROW: ',ROW,' COL: ',COL
               RD$R81 = 102
               GOTO 10
            END IF
         END DO
         READ(INTAB(ROW)(INDX1:INDX2), * ) VAR(ROW + ROWLB - 1) ! READ VAL
         IF ( .NOT. LASTCOL) &
             INTAB(ROW) = INTAB(ROW)(INDX2 + 1:)   ! DROP LEADING COLUMN
         IF (DBUG .GT. 0 .AND. PRTDBGE .EQ. 4) WRITE(DBUGFIL,100) &
             BLKLAB,COL,ROW + ROWLB - 1,VAR(ROW + ROWLB - 1)
10    CONTINUE
      END DO
      RETURN
100   FORMAT(1X,'TABLE: ',A12,' COL: ',I3,' ROW: ',I3,' VAL: ',G12.5)
      END
!
!*******************************************************************
!     NAME: RD$C1
!     PURPOSE:  READ A 1 DIMENSIONAL CHARACTER* VARIABLE
!     READ A 1ST DATA COLUMN FROM A DATA BUFFER PREVIOUSLY
!     FILLED BY RD$TBL. AFTER COLUMN IS READ, DROP DATA FROM
!     BUFFER SO NEXT CALL READS NEXT COLUMN
!     EXIT IF DATA ERR ARE ENCOUNTERED
!     CHARACTER STRING IN DATA MUST BE ENCLOSED BY ' '.
!     CHARACTER STRINGS MAY NOT HAVE EMBEDDED QUOTES.
!     AUTHOR: WAYNE COOPER
!*******************************************************************
      FUNCTION RD$C1(VAR,ROWLB,NDIM1)
      IMPLICIT NONE
      include 'emmbuf'                                           !//mf//
      include 'parametr'
      include 'ncntrl'
      INTEGER*4 ROWLB      ! 1ST ELE IN ARRAY FILLED BY 1ST ELE IN TABLE
      INTEGER*4 NDIM1                  ! NUMBER OF ELEMENTS IN VAR ARRAY
      INTEGER*4 RD$C1                                      ! RETURN CODE
      INTEGER*4 INDX1,INDX2,ROW,TEST                           ! INDICES
      CHARACTER*(*) VAR(NDIM1)        ! VARIABLE FILLED FROM DATA COLUMN
      LOGICAL LASTCOL                  ! =1 IF LAST COLUMN OF DATA TABLE
!
      RD$C1 = 1
      COL = COL + 1
      IF ((ROWLB + TABROWS - 1) .GT. NDIM1) THEN
         WRITE(STDERR,'(3A,I4)') ' *** ERR: NUM ROWS EXCEEDS SIZE OF ARRAY IN TABLE: ', &
                      BLKLAB,' COL: ',COL
         RD$C1 = 103
         RETURN
      END IF
      DO ROW = 1,TABROWS
         LASTCOL = .FALSE.
         INDX1 = INDEX(INTAB(ROW),'''')
         IF (INDX1 .EQ. 0) THEN            ! NO OPENING QUOTES: SKIP ROW
!           ROW TERMINATES WITH "END" LABEL BEFORE ARRAY END: SKIP ROW
            TEST = INDEX(INTAB(ROW),' END')
            IF (TEST .GT. 0) THEN
               WRITE(MSG,'(3A,I4,A,I4)') ' ***WARNING: DATA ROW HAS END LABEL BEF ARRAY END FOR: ', &
                            BLKLAB,' ROW: ',ROW,' COL: ',COL
               INTAB(ROW)(TEST+1:TEST+3) = '   '  ! NULL OUT "END" LABEL TO CLEAR BUFFER
               RD$C1 = 104
               GOTO 10
            END IF
           WRITE(STDERR,'(3A,I4,A,I4)') ' *** ERR: NO OPENING QUOTES IN DATA FIELD IN TABLE: ', &
                        BLKLAB,' ROW: ',ROW,' COL: ',COL
            RD$C1 = 101
            GOTO 10
         END IF
         INDX2 = INDEX(INTAB(ROW)(INDX1+1:),'''')
         IF (INDX2 .EQ. 0) THEN            ! NO CLOSING QUOTES: SKIP ROW
           WRITE(STDERR,'(3A,I4,A,I4)') ' *** ERR: NO CLOSING QUOTES IN DATA FIELD IN TABLE: ', &
                        BLKLAB,' ROW: ',ROW,' COL: ',COL
            RD$C1 = 102
            GOTO 10
         END IF
         INDX2 = INDX2 + INDX1
         READ(INTAB(ROW)(INDX1:INDX2), * ) VAR(ROW + ROWLB - 1) ! READ VALUE
         IF ((INDX2 + 1) .LE. LEN(INTAB(ROW)))       & ! UNLESS LAST COLUMN
             INTAB(ROW) = INTAB(ROW)(INDX2 + 1:)   ! DROP LEADING COLUMN
         IF (DBUG .GT. 0 .AND. PRTDBGE .EQ. 4) WRITE(DBUGFIL,100) &
             BLKLAB,COL,ROW + ROWLB - 1,VAR(ROW + ROWLB - 1)
10    CONTINUE
      END DO
100   FORMAT(1X,'TABLE: ',A12,' COL: ',I3,' ROW: ',I3,' VAL: ',A20)
      RETURN
      END
!
!*******************************************************************
!     NAME: RD$I2
!     PURPOSE:  READ A 2 DIMENSIONAL INTEGER*4 VARIABLE
!     READ NCOL DATA COLUMNS FROM A DATA BUFFER PREVIOUSLY
!     FILLED BY RD$TBL. AFTER COLUMN IS READ, DROP DATA FROM
!     BUFFER SO NEXT CALL READS NEXT COLUMN
!     EXIT IF DATA ERR ARE ENCOUNTERED
!     AUTHOR: WAYNE COOPER
!*******************************************************************
      FUNCTION RD$I2(VAR,ROWLB,COLLB,NCOL,NDIM1,NDIM2)
      IMPLICIT NONE
      include 'emmbuf'                                           !//mf//
      include 'parametr'
      include 'ncntrl'
      INTEGER*4 CCOL               ! CURRENT COLUMN INDEX IN DATA BUFFER
      INTEGER*4 COLLB            ! 1ST COLUMN DATA IS READ INTO IN ARRAY
      INTEGER*4 NCOL                       ! NUMBER OF DATA COLUMNS USED
      INTEGER*4 INDX                                   ! INDEX INTO TEMP
      INTEGER*4 NDIM1,NDIM2        ! NUMBER OF COLS AND ROWS IN VARIABLE
      INTEGER*4 RD$I1                                     ! FUNCT CALLED
      INTEGER*4 RD$I2                                      ! RETURN CODE
      INTEGER*4 RET_CD                                     ! RETURN CODE
      INTEGER*4 ROW                   ! CURRENT ROW INDEX IN DATA BUFFER
      INTEGER*4 ROWLB               ! 1ST ROW DATA IS READ INTO IN ARRAY
      INTEGER*4 TEMP(NLN)                         ! TEMP COLUMN VARIABLE
      INTEGER*4 VAR(NDIM1,NDIM2)            ! VARIABLE DATA IS READ INTO
!
!     CHECK PASSED PARAMETERS
!
      IF (NCOL .EQ. 0) THEN
         WRITE(MSG,'(3A)') ' *** WARNING: NUM COLUMNS IN RD$I2 FOR TABLE: ', &
                           BLKLAB,' IS 0: NO DATA TO READ ***'
         RD$I2 = 1
         RETURN
      END IF
      IF ((ROWLB + TABROWS - 1 .GT. NDIM1) .OR. &
       (COLLB + NCOL - 1 .GT. NDIM2)) THEN
         WRITE(STDERR,'(3A)') ' *** ERR: DATA SIZE IN RD$I2 FOR TABLE: ', &
                                      BLKLAB,' EXCEEDS ARRAY BOUNDS: EXIT '
         RD$I2 = 201
         RETURN
      END IF
!
!     READ EACH DATA COLUMN INTO VARIABLE COLUMN
!
      DO CCOL = COLLB, COLLB + NCOL - 1
         RET_CD = RD$I1(TEMP,1,NLN)
         IF (RET_CD .GT. 1) THEN                      ! EXIT IF READ ERR
            RD$I2 = RET_CD
            IF (DBUG .GT. 0 .AND. PRTDBGE .EQ. 4) &
                WRITE(DBUGFIL,'(3A,I4)') ' *** END RD$I2 FOR TABLE: ',BLKLAB,' COL: ',COL
            RETURN
         END IF
         INDX = 0
         DO ROW = ROWLB, TABROWS + ROWLB - 1
            INDX = INDX + 1
            VAR(ROW,CCOL) = TEMP(INDX)
         END DO
      END DO
100   FORMAT(1X,' TABLE: ',A12,' COL: ',I3,' ROW: ',I3,' VAL: ',I10)
      RD$I2 = 1
      RETURN
      END
!
!*******************************************************************
!     NAME: RD$R2
!     PURPOSE:  READ A 2 DIMENSIONAL REAL*4 VARIABLE
!     READ NCOL DATA COLUMNS FROM A DATA BUFFER PREVIOUSLY
!     FILLED BY RD$TBL. AFTER COLUMN IS READ, DROP DATA FROM
!     BUFFER SO NEXT CALL READS NEXT COLUMN
!     EXIT IF DATA ERRS ARE ENCOUNTERED
!     AUTHOR: WAYNE COOPER
!*******************************************************************
      FUNCTION RD$R2(VAR,ROWLB,COLLB,NCOL,NDIM1,NDIM2)
      IMPLICIT NONE
      include 'emmbuf'                                           !//mf//
      include 'parametr'
      include 'ncntrl'
      INTEGER*4 CCOL               ! CURRENT COLUMN INDEX IN DATA BUFFER
      INTEGER*4 COLLB            ! 1ST COLUMN DATA IS READ INTO IN ARRAY
      INTEGER*4 INDX                                   ! INDEX INTO TEMP
      INTEGER*4 NCOL                       ! NUMBER OF DATA COLUMNS USED
      INTEGER*4 NDIM1,NDIM2     ! NUMBER OF COLUMNS AND ROWS IN VARIABLE
      INTEGER*4 RD$R1                                     ! FUNCT CALLED
      INTEGER*4 RD$R2                                      ! RETURN CODE
      INTEGER*4 RET_CD                                     ! RETURN CODE
      INTEGER*4 ROW                   ! CURRENT ROW INDEX IN DATA BUFFER
      INTEGER*4 ROWLB               ! 1ST ROW DATA IS READ INTO IN ARRAY
      REAL*4 TEMP(NLN)                            ! TEMP COLUMN VARIABLE
      REAL*4 VAR(NDIM1,NDIM2)               ! VARIABLE DATA IS READ INTO
!
!     CHECK PASSED PARAMETERS
!
      IF (NCOL .EQ. 0) THEN
         WRITE(MSG,'(3A)') ' *** WARNING: NUM COLUMNS IN RD$R2 FOR TABLE: ', &
                           BLKLAB,' IS 0: NO DATA TO READ ***'
         RD$R2 = 1
         RETURN
      END IF
      IF ((ROWLB + TABROWS - 1 .GT. NDIM1) .OR. &
       (COLLB + NCOL - 1 .GT. NDIM2)) THEN
         WRITE(STDERR,'(3A)') ' *** ERR: DATA SIZE IN RD$R2 FOR TABLE: ', &
                              BLKLAB,' EXCEEDS ARRAY BOUNDS: EXIT '
         RD$R2 = 201
         RETURN
      END IF
!
!     READ DATA COLUMN INTO VARIABLE COLUMN
!
      DO CCOL = COLLB, COLLB + NCOL - 1
         RET_CD = RD$R1(TEMP,1,NLN)
         IF (RET_CD .GT. 1) THEN                      ! EXIT IF READ ERR
            RD$R2 = RET_CD
            IF (DBUG .GT. 0 .AND. PRTDBGE .EQ. 4) &
                WRITE(DBUGFIL,'(3A,I4)') ' *** END RD$R2 FOR TABLE: ',BLKLAB,' COL: ',COL
            RETURN
         END IF
         INDX = 0
         DO ROW = ROWLB, TABROWS + ROWLB - 1
            INDX = INDX + 1
            VAR(ROW,CCOL) = TEMP(INDX)
         END DO
      END DO
      RD$R2 = 1
100   FORMAT(1X,' TABLE: ',A12,' COL: ',I3,' ROW: ',I3,' VAL: ',G12.5)
      RETURN
      END
!
!*******************************************************************
!     NAME: RD$R82
!     PURPOSE:  READ A 2 DIMENSIONAL REAL*8 VARIABLE
!     READ NCOL DATA COLUMNS FROM A DATA BUFFER PREVIOUSLY
!     FILLED BY RD$TBL. AFTER COLUMN IS READ, DROP DATA FROM
!     BUFFER SO NEXT CALL READS NEXT COLUMN
!     EXIT IF DATA ERRS ARE ENCOUNTERED
!     AUTHOR: WAYNE COOPER
!*******************************************************************
      FUNCTION RD$R82(VAR,ROWLB,COLLB,NCOL,NDIM1,NDIM2)
      IMPLICIT NONE
      include 'emmbuf'                                           !//mf//
      include 'parametr'
      include 'ncntrl'
      INTEGER*4 CCOL               ! CURRENT COLUMN INDEX IN DATA BUFFER
      INTEGER*4 COLLB            ! 1ST COLUMN DATA IS READ INTO IN ARRAY
      INTEGER*4 NCOL                       ! NUMBER OF DATA COLUMNS USED
      INTEGER*4 INDX                                   ! INDEX INTO TEMP
      INTEGER*4 NDIM1,NDIM2        ! NUMBER OF ROWS AND COLS IN VARIABLE
      INTEGER*4 RD$R81                                    ! FUNCT CALLED
      INTEGER*4 RD$R82                                     ! RETURN CODE
      INTEGER*4 RET_CD                                     ! RETURN CODE
      INTEGER*4 ROW                   ! CURRENT ROW INDEX IN DATA BUFFER
      INTEGER*4 ROWLB               ! 1ST ROW DATA IS READ INTO IN ARRAY
      REAL*8 TEMP(NLN)                            ! TEMP COLUMN VARIABLE
      REAL*8 VAR(NDIM1,NDIM2)               ! VARIABLE DATA IS READ INTO
!
!     VALIDATE PASSED PARAMETERS
!
      IF (NCOL .EQ. 0) THEN
         WRITE(MSG,'(3A)') ' *** WARNING: # COLUMNS IN RD$R82 FOR TABLE: ', &
                           BLKLAB,' IS 0: NO DATA TO READ ***'
         RD$R82 = 1
         RETURN
      END IF
      IF ((ROWLB + TABROWS - 1 .GT. NDIM1) .OR. &
       (COLLB + NCOL - 1 .GT. NDIM2)) THEN
         WRITE(STDERR,'(3A)') ' *** ERR: TABLE SIZE IN RD$R82 FOR TABLE: ', &
                              BLKLAB,' EXCEEDS ARRAY BOUNDS: EXIT '
         RD$R82 = 201
         RETURN
      END IF
!
!     READ DATA COLUMN INTO VARIABLE COLUMN
!
      DO CCOL = COLLB, COLLB + NCOL - 1
         RET_CD = RD$R81(TEMP,1,NLN)
         IF (RET_CD .GT. 1) THEN                      ! EXIT IF READ ERR
            RD$R82 = RET_CD
            IF (DBUG .GT. 0 .AND. PRTDBGE .EQ. 4) &
                WRITE(DBUGFIL,'(3A,I4)') ' ** END RD$R82 FOR TABLE: ',BLKLAB,' COL: ',COL
            RETURN
         END IF
         INDX = 0
         DO ROW = ROWLB, TABROWS + ROWLB - 1
            INDX = INDX + 1
            VAR(ROW,CCOL) = TEMP(INDX)
         END DO
      END DO
      RD$R82 = 1
100   FORMAT(1X,' TABLE: ',A12,' COL: ',I3,' ROW: ',I3,' VAL: ',G12.5)
      RETURN
      END
!
!*******************************************************************
!     NAME: RD$C2
!     PURPOSE:  READ A 2 DIMENSIONAL CHARACTER*(*) VARIABLE
!     READ NCOL DATA COLUMNS FROM A DATA BUFFER PREVIOUSLY
!     FILLED BY RD$TBL. AFTER COLUMN IS READ, DROP DATA FROM
!     BUFFER SO NEXT CALL READS NEXT COLUMN
!     EXIT IF DATA ERRS ARE ENCOUNTERED
!     AUTHOR: WAYNE COOPER
!*******************************************************************
      FUNCTION RD$C2(VAR,ROWLB,COLLB,NCOL,NDIM1,NDIM2)
      IMPLICIT NONE
      include 'emmbuf'                                           !//mf//
      include 'parametr'
      include 'ncntrl'
      INTEGER*4 CCOL               ! CURRENT COLUMN INDEX IN DATA BUFFER
      INTEGER*4 COLLB            ! 1ST COLUMN DATA IS READ INTO IN ARRAY
      INTEGER*4 NCOL                       ! NUMBER OF DATA COLUMNS USED
      INTEGER*4 INDX                                     ! INDEX ON TEMP
      INTEGER*4 NDIM1,NDIM2        ! NUMBER OF COLS AND ROWS IN VARIABLE
      INTEGER*4 RD$C1                                     ! FUNCT CALLED
      INTEGER*4 RD$C2                                      ! RETURN CODE
      INTEGER*4 RET_CD                                     ! RETURN CODE
      INTEGER*4 ROW                   ! CURRENT ROW INDEX IN DATA BUFFER
      INTEGER*4 ROWLB               ! 1ST ROW DATA IS READ INTO IN ARRAY
      CHARACTER*240 TEMP(NLN)                     ! TEMP COLUMN VARIABLE
      CHARACTER*(*) VAR(NDIM1,NDIM2)        ! VARIABLE DATA IS READ INTO
!
!     CHECK PARAMETERS PASSED IN
!
      IF (NCOL .EQ. 0) THEN
         WRITE(MSG,'(3A)') ' *** WARNING: NUM COLUMNS IN RD$C2 FOR TABLE: ', &
                           BLKLAB,' IS 0: NO DATA TO READ ***'
         RD$C2 = 1
         RETURN
      END IF
      IF ((ROWLB + TABROWS - 1 .GT. NDIM1) .OR. &
       (COLLB + NCOL - 1 .GT. NDIM2)) THEN
         WRITE(STDERR,'(3A)') ' *** ERR: DATA SIZE IN RD$C2 FOR TABLE: ', &
                              BLKLAB,' EXCEEDS ARRAY BOUNDS: EXIT'
         RD$C2 = 201
         RETURN
      END IF
!
!     READ DATA COLUMN INTO VARIABLE COLUMN
!
      DO CCOL = COLLB, COLLB + NCOL - 1
         RET_CD = RD$C1(TEMP,1,NLN)
         IF (RET_CD .GT. 1) THEN                      ! EXIT IF READ ERR
            RD$C2 = RET_CD
            IF (DBUG .GT. 0 .AND. PRTDBGE .EQ. 4) &
                WRITE(DBUGFIL,'(3A,I4)') ' ***END RD$C2 FOR TABLE: ',BLKLAB,' COL: ',COL
            RETURN
         END IF
         INDX = 0
         DO ROW = ROWLB, TABROWS + ROWLB - 1
            INDX = INDX + 1
            VAR(ROW,CCOL) = TEMP(INDX)
         END DO
      END DO
      RD$C2 = 1
100   FORMAT(1X,' TABLE: ',A12,' COL: ',I3,' ROW: ',I3,' VAL: ',A20)
      RETURN
      END
!
!*******************************************************************
!     NAME: RD$I3
!     PURPOSE:  READ A 3 DIMENSIONAL INTEGER*4 VARIABLE
!     READ NCOL DATA COLUMNS FROM A DATA BUFFER PREVIOUSLY
!     FILLED BY RD$TBL. AFTER COLUMN IS READ, DROP DATA FROM
!     BUFFER SO NEXT CALL READS NEXT COLUMN
!     EXIT IF DATA ERRS ARE ENCOUNTERED
!     *** UNLIKE RD$I1 AND RD$I2, DATA TABLE FILLS ARRAY IN RD$I3
!     ***                       , DATA TABLE CANNOT BE ROTATED TO ARRAY
!     *** DIMENSIONS 1 AND 2 ARE IN THE ROW DIMENSION OF THE DATA TABLE
!     AUTHOR: WAYNE COOPER
!*******************************************************************
      FUNCTION RD$I3(VAR,COLLB,NCOL,NDIM1,NDIM2,NDIM3)
      IMPLICIT NONE
      include 'emmbuf'                                           !//mf//
      include 'parametr'
      include 'ncntrl'
      INTEGER*4 DIM1, DIM2, DIM3         ! CURRENT PLANE, ROW, COL INDEX
      INTEGER*4 INDX       ! INDEX IN COLUMN TO BE LOADED INTO 3-D ARRAY
      INTEGER*4 COLLB            ! 1ST COLUMN IN ARRAY DATA IS READ INTO
      INTEGER*4 NCOL            ! NUMBER OF DATA COLUMNS READ INTO ARRAY
      INTEGER*4 NDIM1, NDIM2, NDIM3     ! # PLANES, ROWS, COLUMNS IN VAR
      INTEGER*4 RD$I3                                      ! RETURN CODE
      INTEGER*4 RD$I1                                     ! CALLED FUNCT
      INTEGER*4 RET_CD                          ! RETURN CODE FROM FUNCT
      INTEGER*4 TEMP(NLN)                  ! TEMPORARY DATA COLUMN ARRAY
      INTEGER*4 VAR(NDIM1,NDIM2,NDIM3)         ! ARRAY DATA IS READ INTO
!
!     CHECK PARAMETERS PASSED IN
!
      IF (NCOL .EQ. 0) THEN
         WRITE(MSG,'(3A)') ' *** WARNING: NUM COLUMNS IN RD$I3 FOR TABLE: ', &
                           BLKLAB,' IS 0; NO DATA TO READ ***'
         RD$I3 = 1
         RETURN
      END IF
      IF ((TABROWS .GT. NDIM1 * NDIM2) .OR. &
       (COLLB + NCOL - 1 .GT. NDIM3)) THEN
         WRITE(STDERR,'(3A)') ' *** ERR: DATA SIZE IN RD$I3 FOR TABLE: ', &
                              BLKLAB,' EXCEEDS ARRAY BOUNDS: EXIT '
         RD$I3 = 201
         RETURN
      END IF
!
!     READ ONE DATA TABLE COLUMN AT A TIME INTO VARIABLE
!
      DO DIM3 = COLLB, COLLB + NCOL - 1
         RET_CD = RD$I1(TEMP,1,NLN)
         IF (RET_CD .GT. 1) THEN                    ! EXIT IF READ ERROR
            RD$I3 = RET_CD
            IF (DBUG .GT. 0 .AND. PRTDBGE .EQ. 4) &
                WRITE(DBUGFIL,'(3A,I4)') ' *** END RD$I3 FOR TABLE: ',BLKLAB,' COL: ',COL
            RETURN
         ELSE
            INDX = 0
            DO DIM1 = 1, NDIM1
               DO DIM2 = 1, NDIM2
                  INDX = INDX + 1
                  VAR(DIM1,DIM2,DIM3) = TEMP(INDX)
               END DO
            END DO
         END IF
      END DO
      RD$I3 = 1
      RETURN
100   FORMAT(1X,' TABLE: ',A12,' DIM1: ',I3,' DIM2: ',I3,' DIM3: ',I3,' VAL: ',I10)
      END
!
!*******************************************************************
!     NAME: RD$R3
!     PURPOSE:  READ A 3 DIMENSIONAL REAL*4 VARIABLE
!     READ NCOL DATA COLUMNS FROM A DATA BUFFER PREVIOUSLY
!     FILLED BY RD$TBL. AFTER COLUMN IS READ, DROP DATA FROM
!     BUFFER SO NEXT CALL READS NEXT COLUMN
!     EXIT IF DATA ERRS ARE ENCOUNTERED
!     *** UNLIKE RD$R1 AND RD$R2, DATA TABLE FILLS ARRAY IN RD$R3
!     ***                       , DATA TABLE CANNOT BE ROTATED TO ARRAY
!     *** DIMENSIONS 1 AND 2 ARE IN THE ROW DIMENSION OF THE DATA TABLE
!     AUTHOR: WAYNE COOPER
!*******************************************************************
      FUNCTION RD$R3(VAR,COLLB,NCOL,NDIM1,NDIM2,NDIM3)
      IMPLICIT NONE
      include 'emmbuf'                                           !//mf//
      include 'parametr'
      include 'ncntrl'
      INTEGER*4 DIM1, DIM2, DIM3         ! CURRENT PLANE, ROW, COL INDEX
      INTEGER*4 COLLB            ! 1ST COLUMN IN ARRAY DATA IS READ INTO
      INTEGER*4 INDX             ! INDEX IN COLUMN TO LOAD INTO VARUABLE
      INTEGER*4 NCOL     ! NUMBER OF COLUMNS OF DATA TABLE USED IN ARRAY
      INTEGER*4 NDIM1, NDIM2, NDIM3     ! # PLANES, ROWS, COLUMNS IN VAR
      INTEGER*4 RD$R3                                      ! RETURN CODE
      INTEGER*4 RD$R1                                     ! CALLED FUNCT
      INTEGER*4 RET_CD                          ! RETURN CODE FROM FUNCT
      REAL*4 TEMP(NLN)                     ! TEMPORARY DATA COLUMN ARRAY
      REAL*4 VAR(NDIM1,NDIM2,NDIM3)            ! ARRAY DATA IS READ INTO
!
!     CHECK PARAMETERS PASSED IN
!
      IF (NCOL .EQ. 0) THEN
         WRITE(MSG,'(3A)') ' *** WARNING: NUM COLUMNS IN RD$R3 FOR TABLE: ', &
                           BLKLAB,' IS 0: NO DATA TO READ ***'
         RD$R3 = 1
         RETURN
      END IF
      IF ((TABROWS .GT. NDIM1 * NDIM2) .OR. &
       (COLLB + NCOL - 1 .GT. NDIM3)) THEN
         WRITE(STDERR,'(3A)') ' *** ERR: DATA SIZE IN RD$R3 FOR TABLE: ', &
                              BLKLAB,' EXCEEDS ARRAY BOUNDS: EXIT'
         RD$R3 = 201
         RETURN
      END IF
!
!     READ ONE DATA TABLE COLUMN AT A TIME INTO VARIABLE
!
      DO DIM3 = COLLB, COLLB + NCOL - 1
         RET_CD = RD$R1(TEMP,1,NLN)
         IF (RET_CD .GT. 1) THEN                    ! EXIT IF READ ERROR
            RD$R3 = RET_CD
            IF (DBUG .GT. 0 .AND. PRTDBGE .EQ. 4) &
                WRITE(DBUGFIL,'(3A,I4)') ' *** END RD$R3 FOR TABLE: ',BLKLAB,' COL: ',COL
            RETURN
         ELSE
            INDX = 0
            DO DIM1 = 1, NDIM1
               DO DIM2 = 1, NDIM2
                  INDX = INDX + 1
                  VAR(DIM1,DIM2,DIM3) = TEMP(INDX)
               END DO
            END DO
         END IF
      END DO
      RD$R3 = 1
      RETURN
100   FORMAT(1X,' TABLE: ',A12,' DIM1: ',I3,' DIM2: ',I3,' DIM3: ',I3,' VAL: ',G12.5)
      END
!
!*******************************************************************
!     NAME: CHK$PRF
!     PURPOSE:  CHECK A 2 DIMENSIONAL PROFILE ARRAY
!     IF (ERR)
!     IF SUM ACROSS CHOSEN DIMENSIONS .NE. A SPECIFIED
!     VALUE, PRINT ERROR MESSAGE TO UNIT ERRFL
!     ELSEIF (.NOT.ERR)
!     PRINT SUMS ACROSS CHOSEN DIMENSIONS TO UNIT ERRFL
!     AUTHOR: WAYNE COOPER
!*******************************************************************
      FUNCTION CHK$PRF(VAR,UB,VARNAME,NDIM1,NDIM2,VAL,ERR,ERRFL)
!
      IMPLICIT NONE
      INTEGER*4  CHK$PRF               ! RETURN CODE FROM FUNCTION, 1=OK
      INTEGER*4  CHK$RSUM                              ! CALLED FUNCTION
      INTEGER*4  ERRFL         ! UNIT # WHERE ERROR MESSAGES ARE SENT TO
      INTEGER*4  NDIM1,NDIM2              ! DIMENSION OF INPUT VARIABLES
      INTEGER*4  MAXDIM2                                 ! MAXIMUM NDIM1
      INTEGER*4  RCODE                        ! RETURN CODE FOR CHK$RSUM
      INTEGER*4  I,J                             ! INDICES OF DIM1, DIM2
      PARAMETER (MAXDIM2 = 500)
      REAL*4     VAR(NDIM1,NDIM2)                     ! PROFILE VARIABLE
      INTEGER*4  UB(NDIM1)                            ! PROFILE VARIABLE
      REAL*4     VAL                        ! VALUE TO CHECK SUM AGAINST
      REAL*4     TEST(MAXDIM2)                            ! 1 ROW OF VAR
      INTEGER*4  LIM2                                    ! 1 VALUE OF UB
      LOGICAL ERR                                  ! .TRUE., PRINT ERROR
                             ! .ELSE., PRINT SUM
      CHARACTER*(*) VARNAME                                ! NAME OF VAR
!
!     CHECK INPUT PARAMETERS
!
      CHK$PRF = 1
      IF (NDIM2 .GT. MAXDIM2) THEN
         WRITE (ERRFL,'(A)') ' *** CHK$PRF ERR: NDIM2 .GT. MAXDIM2'
         CHK$PRF = 101
         RETURN
      END IF
!
!     SPLIT VAR AND UB TO USE CHK$RSUM GENERIC FUNCTION
!
      DO I = 1, NDIM1
         LIM2 = UB(I)
         DO J = 1, NDIM2
            TEST(J) = VAR(I,J)
         END DO
         RCODE = &
          CHK$RSUM(TEST,VARNAME,.TRUE.,.FALSE.,.FALSE.,1,LIM2,1,1,MAXDIM2,1,VAL,ERR,ERRFL)
         CHK$PRF = MAX(CHK$PRF,RCODE)
      END DO
!
      RETURN
      END
!
!*******************************************************************
!     NAME: CHK$RSUM
!     PURPOSE:  CHECK A 1 TO 3 DIMENSIONAL REAL*4 VARIABLE
!     IF (ERR)
!     IF SUM ACROSS CHOSEN DIMENSIONS .NE. A SPECIFIED
!     VALUE, PRINT ERROR MESSAGE TO UNIT ERRFL
!     ELSEIF (.NOT.ERR)
!     PRINT SUMS ACROSS CHOSEN DIMENSIONS TO UNIT ERRFL
!     AUTHOR: WAYNE COOPER
!*******************************************************************
      FUNCTION CHK$RSUM(VAR,VARNAME,CHK1,CHK2,CHK3,UB1,UB2,UB3, &
       NDIM1,NDIM2,NDIM3,VAL,ERR,ERRFL)
!
      IMPLICIT NONE
      INTEGER*4  CHK$RSUM                            ! RETURN CODE, 1=OK
      INTEGER*4  ERRFL                             ! UNIT # FOR MESSAGES
      INTEGER*4  NDIM1,NDIM2,NDIM3                   ! DIMENSIONS OF VAR
      INTEGER*4  UB1,UB2,UB3              ! MAX INDEX IN EACH DIM TO USE
      INTEGER*4  I,J,K                          ! INDICES FOR THE 3 DIMS
      INTEGER*4  MAXDIM1,MAXDIM2               ! MAX SIZE OF TEMP ARRAYS
      PARAMETER (MAXDIM1 = 500)
      PARAMETER (MAXDIM2 = 500)
!
      REAL*4     VAL                        ! VALUE TO CHECK SUM AGAINST
      REAL*4     VAR(NDIM1,NDIM2,NDIM3)         ! VARIABLE BEING CHECKED
      REAL*4     TEST(MAXDIM1,MAXDIM2)              ! SUMMATION VARIABLE
      REAL*4     TOL                                         ! TOLERANCE
      PARAMETER (TOL = 1.0E-06)
      CHARACTER*(*) VARNAME                                ! NAME OF VAR
      LOGICAL CHK1,CHK2,CHK3
                                         ! FLAGS TO USE DIM:
                                         !  0=SUM ACROSS DIM,
                                         !  1=SUM FOR EACH VALUE IN DIM
      LOGICAL ERR             ! .TRUE. = CHECK ERR, .FALSE. = PRINT ONLY
!
!     VALIDATE INPUT PARAMETERS
!
      CHK$RSUM = 1
!
      IF ((UB1 .LT. 0) .OR. (UB1 .GT. NDIM1)) THEN
         WRITE (ERRFL,'(2A)') ' ** CHK$RSUM ERR: UB1 NOT VALID FOR ',VARNAME
         CHK$RSUM = 101
         RETURN
      ELSEIF ((UB2 .LT. 0) .OR. (UB2 .GT. NDIM2)) THEN
         WRITE (ERRFL,'(2A)') ' ** CHK$RSUM ERR: UB2 NOT VALID FOR ',VARNAME
         CHK$RSUM = 102
         RETURN
      ELSEIF ((UB3 .LT. 0) .OR. (UB3 .GT. NDIM3)) THEN
         WRITE (ERRFL,'(2A)') ' ** CHK$RSUM ERR: UB3 NOT VALID FOR ',VARNAME
         CHK$RSUM = 103
         RETURN
      ELSEIF (CHK1 .AND. CHK2 .AND. CHK3) THEN
         WRITE (ERRFL,'(2A)') ' ** CHK$RSUM ERR: NO DIM TO SUM OVER FOR ',VARNAME
         CHK$RSUM = 104
         RETURN
      ENDIF
!
!     SUM OVER I,J,K CHECK
!
      IF (( .NOT. CHK1) .AND. ( .NOT. CHK2) .AND. ( .NOT. CHK3)) THEN
         TEST(1,1) = 0
         DO I = 1, UB1
            DO J = 1, UB2
               DO K = 1, UB3
                  TEST(1,1) = TEST(1,1) + VAR(I,J,K)
               END DO
            END DO
         END DO
         IF (ERR) THEN
            IF ((TEST(1,1) .GT. (VAL + TOL)) .OR. (TEST(1,1) .LT. (VAL - TOL))) THEN
            WRITE(ERRFL,'(3A,F12.4,A,F12.4,A)') ' ** CHK$RSUM ERR: ',VARNAME,' HAS SUM OF ', &
                         TEST(1,1),' .NE. ',VAL,' ***'
            CHK$RSUM = 101
         END IF
      ELSE
         WRITE(ERRFL,'(3A,F12.4)') ' ** CHK$RSUM: ',VARNAME,' HAS SUM OF ',TEST(1,1)
      END IF
!
!     SUM OVER J,K CHECK
!
      ELSE IF (CHK1 .AND. ( .NOT. CHK2) .AND. ( .NOT. CHK3)) THEN
      IF (UB1 .GT. MAXDIM1) THEN
         WRITE(ERRFL,'(2A)') ' ** CHK$RSUM ERR: UB1 .GT. MAXDIM1 FOR ',VARNAME
         CHK$RSUM = 301
         RETURN
      END IF
      DO I = 1, UB1
         TEST(I,1) = 0
         DO J = 1, UB2
            DO K = 1, UB3
               TEST(I,1) = TEST(I,1) + VAR(I,J,K)
            END DO
         END DO
         IF (ERR) THEN
            IF ((TEST(I,1) .GT. (VAL + TOL)) .OR. &
                (TEST(I,1) .LT. (VAL - TOL))) THEN
            WRITE(ERRFL,'(3A,F12.4,A,F12.4,A,I4)') ' ** CHK$RSUM ERR: ',VARNAME, &
                              ' HAS SUM OF ',TEST(I,1),' .NE. ',VAL,' FOR DIM1=',I
            CHK$RSUM = 101
         END IF
      ELSE
         WRITE(ERRFL,'(3A,F12.4,A,I4)') ' ** CHK$RSUM: ',VARNAME,' HAS SUM OF ',TEST(I,1),' FOR DIM1=',I
      END IF
      END DO
!
!  SUM OVER I,K CHECK
!
      ELSE IF (( .NOT. CHK1) .AND. CHK2 .AND. ( .NOT. CHK3)) THEN
        IF (UB2 .GT. MAXDIM1) THEN
          WRITE(ERRFL,'(2A)') ' ** CHK$RSUM ERR: UB2 .GT. MAXDIM1 FOR ',VARNAME
          CHK$RSUM = 301
          RETURN
        END IF
        DO J = 1, UB2
        TEST(J,1) = 0
        DO I = 1, UB1
           DO K = 1, UB3
              TEST(J,1) = TEST(J,1) + VAR(I,J,K)
           END DO
        END DO
        IF (ERR) THEN
           IF ((TEST(J,1) .GT. (VAL + TOL)) .OR. &
               (TEST(J,1) .LT. (VAL - TOL))) THEN
           WRITE(ERRFL,'(3A,F12.4,A,F12.4,A,I4)') ' ** CHK$RSUM ERR: ',VARNAME, &
                       ' HAS SUM OF ',TEST(J,1),' .NE. ',VAL,' FOR DIM2=',J
           CHK$RSUM = 101
        END IF
      ELSE
        WRITE(ERRFL,'(3A,F12.4,A,I4)') ' ** CHK$RSUM: ',VARNAME,' HAS SUM OF ',TEST(J,1),' FOR DIM2=',J
      END IF
      END DO
!
!SUM OVER I,J CHECK
!
      ELSE IF (( .NOT. CHK1) .AND. ( .NOT. CHK2) .AND. CHK3) THEN
      IF (UB3 .GT. MAXDIM1) THEN
        WRITE(ERRFL,'(2A)') ' ** CHK$RSUM ERR: UB3 .GT. MAXDIM1 FOR ',VARNAME
        CHK$RSUM = 301
        RETURN
      END IF
      DO K = 1, UB3
      TEST(K,1) = 0
      DO I = 1, UB1
      DO J = 1, UB2
         TEST(K,1) = TEST(K,1) + VAR(I,J,K)
      END DO
      END DO
      IF (ERR) THEN
      IF ((TEST(K,1) .GT. (VAL + TOL)) .OR. &
          (TEST(K,1) .LT. (VAL - TOL))) THEN
        WRITE(ERRFL,'(3A,F12.4,A,F12.4,A,I4)') ' ** CHK$RSUM ERR: ',VARNAME, &
                    ' HAS SUM OF ',TEST(K,1),' .NE. ',VAL,' FOR DIM3=',I
        CHK$RSUM = 101
      END IF
      ELSE
      WRITE(ERRFL,'(3A,F12.4,A,I4)') ' ** CHK$RSUM: ',VARNAME,' HAS SUM OF ',TEST(K,1),' FOR DIM3=',I
      END IF
      END DO
!
!SUM OVER K CHECK
!
      ELSE IF (CHK1 .AND. CHK2 .AND. ( .NOT. CHK3)) THEN
      IF ((UB1 .GT. MAXDIM1) .OR. (UB2 .GT. MAXDIM2)) THEN
      WRITE(ERRFL,'(2A)') ' ** CHK$RSUM ERR:  UB1 .GT. MAXDIM1 .OR. UB2 .GT. MAXDIM2 FOR ',VARNAME
      CHK$RSUM = 301
      RETURN
      END IF
      DO I = 1, UB1
      DO J = 1, UB2
      TEST(I,J) = 0
      DO K = 1, UB3
      TEST(I,J) = TEST(I,J) + VAR(I,J,K)
      END DO
      IF (ERR) THEN
      IF ((TEST(I,J) .GT. (VAL + TOL)) .OR. &
          (TEST(I,J) .LT. (VAL - TOL))) THEN
      WRITE(ERRFL,'(3A,F12.4,A,F12.4,A,I4,A,I4)') ' ** CHK$RSUM ERR: ',VARNAME, &
                  ' HAS SUM OF ',TEST(I,J),' .NE. ',VAL,' FOR DIM1=',I,' DIM2=',J
      CHK$RSUM = 101
      END IF
      ELSE
      WRITE(ERRFL,'(3A,F12.4,A,I4,A,I4)') ' ** CHK$RSUM: ',VARNAME, &
                  ' HAS SUM OF ',TEST(I,J),' FOR DIM1=',I,' DIM2=',J
      END IF
      END DO
      END DO
!
!SUM OVER J CHECK
!
      ELSE IF (CHK1 .AND. ( .NOT. CHK2) .AND. CHK3) THEN
      IF ((UB1 .GT. MAXDIM1) .OR. (UB3 .GT. MAXDIM2)) THEN
      WRITE(ERRFL,'(2A)') ' ** CHK$RSUM ERR:  UB1 .GT. MAXDIM1 .OR. UB3 .GT. MAXDIM2 FOR ',VARNAME
      CHK$RSUM = 301
      RETURN
      END IF
      DO I = 1, UB1
      DO K = 1, UB3
      TEST(I,K) = 0
      DO J = 1, UB2
      TEST(I,K) = TEST(I,K) + VAR(I,J,K)
      END DO
      IF (ERR) THEN
      IF ((TEST(I,K) .GT. (VAL + TOL)) .OR. &
       (TEST(I,K) .LT. (VAL - TOL))) THEN
      WRITE(ERRFL,'(3A,F12.4,A,F12.4,A,I4,A,I4)') ' ** CHK$RSUM ERR: ',VARNAME, &
                  ' HAS SUM OF ',TEST(I,K),' .NE. ',VAL,' FOR DIM1=',I,' DIM3=',K
      CHK$RSUM = 101
      END IF
      ELSE
      WRITE(ERRFL,'(3A,F12.4,A,I4,A,I4)') ' ** CHK$RSUM: ',VARNAME, &
                  ' HAS SUM OF ',TEST(I,K),' FOR DIM1=',I,' DIM3=',K
      END IF
      END DO
      END DO
!
!SUM OVER I CHECK
!
      ELSE IF (( .NOT. CHK1) .AND. CHK2 .AND. CHK3) THEN
      IF ((UB2 .GT. MAXDIM1) .OR. (UB3 .GT. MAXDIM2)) THEN
      WRITE(ERRFL,'(2A)') ' ** CHK$RSUM ERR:  UB2 .GT. MAXDIM1 .OR. UB3 .GT. MAXDIM2 FOR ',VARNAME
      CHK$RSUM = 301
      RETURN
      END IF
      DO J = 1, UB2
      DO K = 1, UB3
      TEST(J,K) = 0
      DO I = 1, UB1
      TEST(J,K) = TEST(J,K) + VAR(I,J,K)
      END DO
      IF (ERR) THEN
      IF ((TEST(J,K) .GT. (VAL + TOL)) .OR. (TEST(J,K) .LT. (VAL - TOL))) THEN
      WRITE(ERRFL,'(3A,F12.4,A,F12.4,A,I4,A,I4)') ' ** CHK$RSUM ERR: ',VARNAME, &
                  ' HAS SUM OF ',TEST(J,K),' .NE. ',VAL,' FOR DIM2=',J,' DIM3=',K
      CHK$RSUM = 101
      END IF
      ELSE
      WRITE(ERRFL,'(3A,F12.4,A,I4,A,I4)') ' ** CHK$RSUM: ',VARNAME, &
                  ' HAS SUM OF ',TEST(J,K),' FOR DIM2=',J,' DIM3=',K
      END IF
      END DO
      END DO
      END IF            ! FOR ALL VALID COMBINATIONS OF CHK1, CHK2, CHK3
      RETURN
      END
!
!*******************************************************************
!     NAME: GET_INDX
!     PURPOSE:  CONVERT A CHARACTER CODE INTO ITS CORRESPONDING INDEX
!     MODIFICATION LOG: CREATED 6/27/95 WCW
!
!*******************************************************************
      FUNCTION GET_INDX(NVAR,IARR,CODE,L_IARR,SORTED,UF_MSG,UF_DBG)
!
      IMPLICIT NONE
!
      INTEGER*4 FST,MID,LST        ! FIRST, MIDDLE, LAST INDEX OF SEARCH
      INTEGER*4 I                                           ! LOOP INDEX
      INTEGER*4 L_IARR                           ! LENGTH OF INDEX ARRAY
      INTEGER*4 UF_MSG,UF_DBG            ! UNIT # FOR MSG and DBG Switch
      INTEGER*4 GET_INDX                   ! INDEX CORRESPONDING TO CODE
      LOGICAL SORTED             ! IF =1 IARR IS SORTED ALPHANUMERICALLY
                                 !    =0 IARR IS NOT SORTED
      CHARACTER*(*) CODE                         ! CODE TO FIND INDEX OF
      CHARACTER*(*) NVAR                           ! NAME OF INDEX ARRAY
      CHARACTER*(*) IARR(L_IARR)        ! INDEX ARRAY TO SEARCH FOR CODE
!
!     IF SORTED, USE BISECTION SEARCH
!
      GET_INDX = 1
      IF (SORTED) THEN
         FST = 1
         LST = L_IARR
         DO WHILE (LST .GT. (FST + 1))
            MID = (INT(REAL(LST - FST + 1) + 0.001) / 2.0) + FST
            IF (CODE .GT. IARR(MID)) THEN
               FST = MID
            ELSE IF (CODE .LT. IARR(MID)) THEN
               LST = MID
            ELSE
               GET_INDX = MID
               RETURN
            END IF
         END DO
         IF (CODE .EQ. IARR(FST)) THEN
            GET_INDX = FST
            RETURN
         ELSE IF (CODE .EQ. IARR(LST)) THEN
            GET_INDX = LST
            RETURN
         END IF
      END IF
!
!     OTHERWISE USE EXHAUSTIVE SEARCH
!
      DO I = 1, L_IARR
         IF (CODE .EQ. IARR(I)) THEN
            GET_INDX = I
            RETURN
         END IF
      END DO
      IF (UF_DBG .GT. 0) THEN
         WRITE(UF_MSG,'(4A)') ' ** GET_INDX ERROR: ',CODE,' NOT FOUND IN VARIABLE ',NVAR
      END IF
      RETURN
      END
!
      SUBROUTINE GET_EMF_AND_ACI(NMAX,N,CHOICE,OUT,HR,IN,PRCNT,LIM,GRAM,HG,EMF_P,F_FGD,F_SCR,A,B,C,D,EMF_M,Allowed,EMF_T,ACI,MACT_SW,EMF_MAX)
!
      IMPLICIT NONE

      INCLUDE 'parametr'
      INCLUDE 'emmparm'
      INCLUDE 'emission'
!
      INTEGER*4 NMAX
      REAL*8 OUT,HR,IN,PRCNT,LIM,GRAM,HG
      REAL*8 EMF_P,F_FGD,F_SCR,A,B,C,D,EMF_M
      REAL*8 EMF_T(NMAX),ACI(NMAX)
      REAL*8 EMF_PRCNT,EMF_IN,EMF_OUT,EMF_GRAM,EMF_MAX
      INTEGER*4 N,CHOICE,N2,I_ACI
      INTEGER*4 Allowed(NMAX),SV_P_ALLOW,MACT_SW
!
      EMF_T = EMF_P
      ACI = 0.0
      SV_P_ALLOW = Allowed(1)
      Allowed = 0
      Allowed(1) = SV_P_ALLOW
!
      IF (OUT .GT. 0.0 .OR. IN .GT. 0.0 .OR. PRCNT .GT. 0.0) THEN
         MACT_SW = 1
      ELSE
         MACT_SW = 0
      END IF
!
      IF (PRCNT .GT. 0.0) THEN
         EMF_PRCNT = PRCNT
      ELSE
         EMF_PRCNT = 1.0
      END IF
!
      IF (IN .GT. 0.0) THEN
         EMF_IN = MIN(1.0 , IN / HG)
      ELSE
         EMF_IN = 1.0
      END IF
!
      IF (OUT .GT. 0.0) THEN
         EMF_OUT = MIN(1.0 , 1000.0 * OUT / HR / HG)
      ELSE
         EMF_OUT = 1.0
      END IF
!
!     Skip Steps If No CAMR and Just Specified Required EMF, If Achievable
      IF (USW_CAMR .LE. 0)THEN
         EMF_MAX = MIN(EMF_P , EMF_PRCNT)
         IF (CHOICE .EQ. 1) THEN
            EMF_MAX = MIN(EMF_MAX , EMF_IN)
         ELSE IF (CHOICE .EQ. 2) THEN
            EMF_MAX = MIN(EMF_MAX , EMF_OUT)
         ELSE IF (CHOICE .EQ. 3) THEN
            EMF_MAX = MAX(MIN(EMF_MAX , EMF_IN) , MIN(EMF_MAX , EMF_OUT))
         END IF
!
         IF (MACT_SW .EQ. 1 .OR. LIM .LT. 100.0 .OR. GRAM .GT. 0.0) THEN
            IF (MACT_SW .EQ. 1 .AND. EMF_MAX .GT. EMF_M .AND. EMF_MAX .LT. EMF_P) THEN
               N2 = N - 1
            ELSE
               N2 = N
            END IF
            DO I_ACI = 2 , N + 1
               IF (MACT_SW .EQ. 1 .AND. EMF_MAX .GT. EMF_M .AND. EMF_MAX .LT. EMF_P .AND. I_ACI .EQ. N+1) THEN
                  EMF_T(I_ACI) = EMF_MAX
               ELSE
                  EMF_T(I_ACI) = EMF_P - (I_ACI - 1) * ((EMF_P - EMF_M) / N2)
               END IF
               Allowed(I_ACI) = 1
               ACI(I_ACI) = 1000.0 * 1000.0 * 1000.0 * 0.000454 * C * 2.2 / 12000.0 * MAX(0.0 , (A / (D - 100.0 + 100.0 * EMF_T(I_ACI) / (F_FGD * F_SCR)) - B))
            END DO
         END IF
      ELSE
!        If No CAMR but Other HG Standard, Specify Just One ACI Step for Standard Unless Min EMF is Insufficient
         IF (EMF_MAX .LT. EMF_P)THEN
            IF (Allowed(1) .GT. 0)Allowed(1) = 2
            IF (EMF_MAX .GE. EMF_M)THEN
               DO I_ACI = 2 , 2
                  Allowed(I_ACI) = 1
                  EMF_T(I_ACI) = EMF_MAX
                  ACI(I_ACI) = 1000.0 * 1000.0 * 1000.0 * 0.000454 * C * 2.2 / 12000.0 * MAX(0.0 , (A / (D - 100.0 + 100.0 * EMF_T(I_ACI) / (F_FGD * F_SCR)) - B))
               END DO
            END IF
         END IF
      END IF
!
      RETURN
      END
!
!     EPINCST CALCULATES THE INSTALLED COST OF A GENERATING UNIT.
!
      SUBROUTINE EPINCST(NYR,XPH,FPH,LCP,CLT,PLT,CYR,FYR,OVRCST,PROFILE,CAPESC,GNPF,INTR,DEBT_F,ROR,TXBOOK,BVBOOK)
!
      IMPLICIT NONE
!
      INTEGER*4 NYR                              ! NUMBER OF MODEL YEARS
      INTEGER*4 XPH                    ! # OF YEARS IN EXPLICIT PLANNING HORIZON
      INTEGER*4 FPH                    ! # OF YEARS IN FULL PLANNING HORIZON
      INTEGER*4 CYR                                 ! CURRENT MODEL YEAR
      INTEGER*4 FYR                    ! FIRST YEAR IN CONSTRUCTION CYCLE
      INTEGER*4 CLT                             ! CONSTRUCTION LEAD TIME -- ACTUAL
      INTEGER*4 PLT                             ! CONSTRUCTION LEAD TIME -- PLANNING
      INTEGER*4 LCP                       ! # OF YEARS IN CONSTR PROFILE
      REAL*8 OVRCST                                    ! OVERNIGHT COSTS
      REAL*4 PROFILE(LCP)                ! ANNUAL EXPENDITURE PERCENTAGE
      REAL*4 CAPESC(NYR+XPH)                   ! CAPITAL ESCALATION/REAL
      REAL*4 GNPF(NYR+FPH)                           ! GENERAL INFLATION
      REAL*4 INTR                                       ! INTEREST COSTS
      REAL*4 DEBT_F                        ! CONSTRUCTION DEBT FINANCING
      REAL*4 ROR                                        ! RATE OF RETURN
!
!     RESULTS:
      REAL*8 TXBOOK                          ! TAX BASIS INSTALLED COSTS
      REAL*8 BVBOOK                         ! BOOK VALUE INSTALLED COSTS
      REAL*8 RVALUE,LVALUE,UVALUE
      REAL*8 YCWP                      ! ANNUAL CONSTRUCTION WORK IN PROGRESS
      INTEGER*4 I,JYR,KYR
!
      TXBOOK = DBLE(0.0)
      BVBOOK = DBLE(0.0)
      DO I = 1 , CLT
         JYR = CYR + FYR + I - 2
         KYR = JYR + PLT - CLT
         KYR = MAX(KYR,1)
         YCWP = OVRCST * PROFILE(I) * CAPESC(KYR) * GNPF(KYR)
         TXBOOK = TXBOOK + YCWP + (BVBOOK + (0.5 * YCWP)) * INTR * DEBT_F
         BVBOOK = BVBOOK + YCWP + (BVBOOK + (0.5 * YCWP)) * ROR
      END DO
!
      RETURN
      END
!
      SUBROUTINE EPNBLD(ROR,DR,RATIO,PE,EL,TL,T,CL,NBLDP1)
!
      IMPLICIT NONE
!
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'ecpcntl'
      include 'taxdat'
      REAL*8 ROR,DR,PE,T,NBLDP1,RATIO
      INTEGER EL,TL,CL
!
!     FUNCTIONS DECLARED
!
      REAL*8 PVV,CRF,PVBV
!
!     TEMPORARY VARIABLES
!
      REAL*8 D(75),PVRB
!
      REAL*8 U,X,Y,Z,UTIL
      INTEGER I,J,K
!
!     CALCULATE UTILITY CAPITAL CHARGE RATE
!     ASSUMES:
!     AT COC = DISCOUNT RATE (DR)
!     TR=MARGINAL TAX RATE
!     PE=(CE*CF+PE*PF)/ROR=PORTION OF ROR TAXABLE
!
!     CALCULATE PRESENT VALUE OF RATE BASE
!
!     FIRST CALCULATE PV OF ACCUM TAX DEPR
!
      IF (TL .EQ. 5) I = 1
      IF (TL .EQ. 7) I = 2
      IF (TL .EQ. 10) I = 3
      IF (TL .EQ. 15) I = 4
      IF (TL .EQ. 20) I = 5
      X = 0.0
      U = 0.0
      DO J = 1,21
!        X = TAXDEPR(J,I)
         X = TAXDEPRN(J,I,MIN(CURIYR + UPPLYR(WIPV),MNUMYR))
         D(J) = U                 ! NOTE THIS STORES THE ACCUMULATED
         U = U + X                ! ANNUAL VALUES AT BEGINING OF YEAR
      END DO                      ! YEAR 1 EQUALS ZERO
!
      K = TL + 1
      IF (CL .GT. K) THEN
         DO I = K + 1,CL
            D(I) = 1.0
         END DO
      ENDIF
!
      J = CL
      Z = PVV(D,J,CL,DR)
!
!     CALUCULATE PV OF BV OF SL DEPR
!
      Y = PVBV(EL,CL,DR)
!
      X = 1/CRF(DR,CL)
      PVRB = X - Y - (Z - Y) * T * RATIO
!
!     NOW CALCULATE THE REST OF THE CAPITAL CHARGE
!
      X = PVRB * ROR
      Y=(1/REAL(EL))/CRF(DR,CL)
      Z = PVRB * PE * ROR * T/(1 - T)
      UTIL = X + Y + Z
      NBLDP1 = UTIL * CRF(DR,CL)
!
      RETURN
      END
!
!     EPCNBLD CALCULATES THE CAPACITY PAYMENT DUE FOR
!     PURCHASED POWER UNDER SPECIFIED CONDITIONS
!
      SUBROUTINE EPCNBLD(DF2,RATIO,ROEPRM,INTPRM,UTROE,UTINT,TR,TL,CL,DL2,CAP)
!
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'ecpcntl'
      include 'taxdat'
      REAL*8 UTROE                                         ! UTILITY ROE
      REAL*8 UTINT                               ! UTILITY INTEREST RATE
      REAL*8 INTPRM                          ! EWG INTEREST RATE PREMIUM
      REAL*8 ROEPRM                             ! EWG EQUITY ROR PREMIUM
      REAL*8 DF2                                     ! EWG DEBT FRACTION
      INTEGER DL2                  ! EWG DEBT LOAN LIFE <= CONTRACT LIFE
      INTEGER CL                                     ! EWG CONTRACT LIFE
      INTEGER TL                   ! TAX LIFE ASSOCIATED WITH TECHNOLOGY
      REAL*8 TR                       ! MARGINAL FEDERAL INCOME TAX RATE
      REAL*8 RATIO                                      ! TX TO BV RATIO
!
!     KEY LOCAL VARIABLES ARE:
!
      REAL*8 EWGROE            ! EWG REQUIRED ROE (UTILITY PLUS PREMIUM)
      REAL*8 EWGINT           ! EWG REQUIRED INTEREST (UTILITY + PREMIUM)
      REAL*8 EFRAC2                                ! EWG EQUITY FRACTION
!
      REAL*8 DPP                                ! DEBT PRINCIPAL PAYMENT
      REAL*8 PVDPP                       ! PV OF DEBT PRINCIPAL PAYMENTS
      REAL*8 PVDEBT                   ! PV OF OUTSTANDING DEBT PRINCIPAL
      REAL*8 PVINT                             ! PV OF INTEREST PAYMENTS
      REAL*8 PVTB                              ! PV OF TAX DEPR BENEFITS
      REAL*8 CAP                                           ! RESULT !!!!
!
!
!     THE OUTPUT OF THE ROUTINE IS THE FRACTION OF THE INSTALLED COST
!     REQUIRED FOR RECOVERY EACH YEAR DURING THE CONTRACT LIFE TO YIELD
!     THE TARGET EWG ROE AS AN IRR TO THE PROJECT OWNER (I.E.,CAP)
!
      REAL*8 CRF                     ! CAPITAL RECOVERY FACTOR  FUNCTION
      REAL*8 PVV                     ! PRESENT VALUE OF VECTOR  FUNCTION
      REAL*8 RVALUE,LVALUE,UVALUE
!
      EWGROE = ROEPRM + UTROE
      EWGINT = INTPRM + UTINT
      EFRAC2 = 1 - DF2
      X=REAL(DL2)
      IF (DL2 .GT. CL) THEN
         WRITE(18,'(A,I4,I4)')'DEBT LIFE GT CONTRACT LIFE ',DL2,CL
         DL2 = CL
      ENDIF
!
      DPP = 1/X * DF2
      PVDPP = DPP/CRF(EWGROE,DL2)
      PVDEBT = (X/EWGROE * (1 - ((1 - (1 + EWGROE) ** ( - X))/(X * EWGROE))) / X) * DF2
      PVINT = PVDEBT * EWGINT
!
      IF (TL .EQ. 5) I = 1
      IF (TL .EQ. 7) I = 2
      IF (TL .EQ. 10) I = 3
      IF (TL .EQ. 15) I = 4
      IF (TL .EQ. 20) I = 5
!
      N = 21
      M = TL
!     PVTB = PVV(TAXDEPR(1,I),N,M,EWGROE) * RATIO
      PVTB = PVV(TAXDEPRN(1,I,MIN(CURIYR + UPPLYR(WIPV),MNUMYR)),N,M,EWGROE) * RATIO
!
      CAP = (EFRAC2/(1 - TR) + PVINT - (TR/(1 - TR) * PVTB) + PVDPP / (1 - TR))
      CAP = CAP * CRF(EWGROE,CL)
!
      RETURN
      END
!
!     FUNCTION TO CALCULATE THE PRESENT VALUE OF A VECTOR OF NUMBERS
!
!     D(I) VECTOR OF NUMBERS TO BE PRESENT VALUED
!     XIR DISCOUNT RATE
!     ASSUMES VECTOR OF UP TO N NUMBERS
!     ASSUMES NO. OF PERIODS TO DISCOUNT IS M
!
      FUNCTION PVV(D,N,M,XIR)
!
      IMPLICIT NONE
!
      INTEGER N,M,I
      REAL*8 PVV
      REAL*8 D(N)
      REAL*8 XIR
      PVV = 0.0
!
      DO I = 1,M
         PVV = PVV + D(I)/((1 + XIR) ** I)
      END DO
      END
!
!     PRESENT WORTH FACTOR FUNCTION
!     CALCULATES A FACTOR THAT EQUATES IN PRESENT VALUE TERMS
!     A VALUE I YEARS IN THE FUTURE AT X DISCOUNT RATE
!
      FUNCTION PWF(XIR,I)
!
      IMPLICIT NONE
!
      REAL*8 PWF
      REAL*8 XIR
      INTEGER I
      PWF = (1 + XIR) ** ( - I)
      END
!
!     CRF IS A FUNCTION THAT RETURNS THE ANNUITY FACTOR
!     ASSOCIATED WITH A INPUT INTEREST RATE (DECIMAL FRACTION)
!     AND TERM (INTERGER YEARS)
!
      FUNCTION CRF(XIR,I)
!
      IMPLICIT NONE
!
      REAL*8 CRF
      REAL*8 XIR
      INTEGER I
      CRF = XIR/(1 - (1 + XIR) ** ( - I))
      END
!
!     FUNCTION TO CALCULATE THE PRESENT VALUE OF THE BOOK VALUE
!     OUTSTANDING: USED IN THE UTILITY PRESENT VALUE OF RATE BASE
!     CALCULATION
!     INPUTS: ECONOMIC LIFE (LIFE), NOYRS (YEARS FOR THE CALCULATION),
!     DISCOUNT RATE (DR)
!
      FUNCTION PVBV(LIFE,NOYRS,DR)
!
      IMPLICIT NONE
!
      INTEGER LIFE,NOYRS
      REAL*8 PVBV
      REAL*8 DR
!
      PVBV=(1-((1+DR)**(-NOYRS))*(DR*NOYRS+1))/((DR**2)*REAL(LIFE))
      END
!
      FUNCTION CRF2(E,DR,I)
!
      IMPLICIT NONE
!
!     FUNCTION TO CALCULATE PRESENT VALUE OF AN ANNUITY
!     ESCALATING AT RATE E, DISCOUNTED AT RATE DR, OVER
!     I NUMBER OF YEARS
!
      REAL*8 CRF2
      REAL*8 E,DR
      INTEGER I
      CRF2 = (DR - E)/(1 - (((1 + E)/(1 + DR)) ** (I)))
      END
!
      SUBROUTINE EPA$TRANRULE
!
!     THIS SUBROUTINE OVERWRITES VARIABLES AFFECTED BY THE
!     CHANGE FROM CAIR TO THE TRANSPORT RULE, IF APPROPRIATE
!
      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'ecp_coal'
      include 'cdsparms'
      include 'emission'
      include 'csapr'
      include 'dsmdimen'
      include 'dsmtoefd'
      include 'dsmtfecp'
      include 'emmemis'
!
      COMMON /CAIREMIS/ CSO2_SHR_BY_CLRG,CSO2_SHR_BY_OLRG,  &
                        CEMRFSA,CEMRFNA,CSO2_SHR_ALW_GRP,CEMELBNK
      REAL*4 CEMRFSA(MNUMYR,MX_SO2_TRAN)
      REAL*4 CEMRFNA(NOX_D_GRP,MNUMYR)
      REAL*4 CSO2_SHR_BY_CLRG(NDREG,MX_SO2_GRP)
      REAL*4 CSO2_SHR_BY_OLRG(MNUMCR,MX_SO2_GRP)
      REAL*4 CSO2_SHR_ALW_GRP(MNUMYR,MX_SO2_GRP)
      REAL*4 CEMELBNK(MX_SO2_GRP)
!
      INTEGER IRG,IYR,SO2,NOX,IST,IMO,ISP
!
!     SO2 INPUTS
!
      IF ((CURIYR + UHBSYR) .EQ. (TRANRULE1 - UPSCLT))THEN
        DO IYR = TRANRULE1 - UHBSYR , MNUMYR
           DO SO2 = 1 , MX_SO2_GRP
              CEMRFSA(IYR,SO2) = EMRFSA(IYR,SO2)
              EMRFSA(IYR,SO2) = TEMRFSA(IYR,SO2)
           END DO
        END DO
!       DO IYR = TRANRULE1 - UHBSYR , TRANRULE2 - 1 - UHBSYR
!          SO2_SHR_ALW_GRP(IYR,1) = 1.0
!          SO2_SHR_ALW_GRP(IYR,2) = 1.0
!       END DO
        DO IYR = TRANRULE1 - UHBSYR, MNUMYR
           CSO2_SHR_ALW_GRP(IYR,1) = SO2_SHR_ALW_GRP(IYR,1)
           CSO2_SHR_ALW_GRP(IYR,2) = SO2_SHR_ALW_GRP(IYR,2)
           SO2_SHR_ALW_GRP(IYR,1) = 0.0
           SO2_SHR_ALW_GRP(IYR,2) = 0.0
        END DO
      END IF
      IF ((CURIYR + UHBSYR) .EQ. TRANRULE1)THEN
        DO SO2 = 1 , MX_SO2_GRP
           CEMELBNK(SO2) = EMELBNK(CURIYR - 1,SO2)
           EMELBNK(CURIYR - 1,SO2) = 0.00
        END DO
        DO IRG = 1 , NDREG
           CSO2_SHR_BY_CLRG(IRG,1) = SO2_SHR_BY_CLRG(IRG,1)
           CSO2_SHR_BY_CLRG(IRG,2) = SO2_SHR_BY_CLRG(IRG,2)
           SO2_SHR_BY_CLRG(IRG,1) = TSO2_SHR_BY_CLRG(IRG,1)
           SO2_SHR_BY_CLRG(IRG,2) = TSO2_SHR_BY_CLRG(IRG,2)
!          SO2_SHR_BY_CLRG(IRG,1) = TSO2_SHR_BY_TRAN(IRG,1,1)
!          SO2_SHR_BY_CLRG(IRG,2) = TSO2_SHR_BY_TRAN(IRG,2,1)
        END DO
        DO IRG = 1 , MNUMCR - 1
           CSO2_SHR_BY_OLRG(IRG,1) = SO2_SHR_BY_OLRG(IRG,1)
           CSO2_SHR_BY_OLRG(IRG,2) = SO2_SHR_BY_OLRG(IRG,2)
           SO2_SHR_BY_OLRG(IRG,1) = TSO2_SHR_BY_OLRG(IRG,1)
           SO2_SHR_BY_OLRG(IRG,2) = TSO2_SHR_BY_OLRG(IRG,2)
        END DO
      END IF
!     IF ((CURIYR + UHBSYR) .EQ. TRANRULE2)THEN
!       DO IRG = 1 , NDREG
!          SO2_SHR_BY_CLRG(IRG,1) = TSO2_SHR_BY_TRAN(IRG,1,2)
!          SO2_SHR_BY_CLRG(IRG,2) = TSO2_SHR_BY_TRAN(IRG,2,2)
!       END DO
!     END IF
!       DO IRG = 1 , NDREG
!     write(6,2323) curiyr+1989,IRG,  &
!          SO2_SHR_BY_CLRG(IRG,1) , SO2_SHR_BY_CLRG(IRG,2),  &
!         TSO2_SHR_BY_CLRG(IRG,1) ,TSO2_SHR_BY_CLRG(IRG,2),  &
!         TSO2_SHR_BY_TRAN(IRG,1,1) ,TSO2_SHR_BY_TRAN(IRG,2,1),  &
!         TSO2_SHR_BY_TRAN(IRG,1,2) ,TSO2_SHR_BY_TRAN(IRG,2,2)
!2323 format(1h ,'!so2shr',i4,i3,8f10.4)
!       END DO
!     IF ((CURIYR + UHBSYR) .EQ. TRANRULE1)THEN
!        DO IST = 1 , TSO2_NST
!           TSO2_CSH_BY_ST(IST) = TSO2_CS1_BY_ST(IST)
!        END DO
!     END IF
!     IF ((CURIYR + UHBSYR) .EQ. TRANRULE2)THEN
!        DO IST = 1 , TSO2_NST
!           TSO2_CSH_BY_ST(IST) = TSO2_CS2_BY_ST(IST)
!        END DO
!     END IF
!     RESET VALUES FROM CSAPR TO CAIR IF APPROPRIATE
      IF ((CURIYR + UHBSYR) .EQ. UYR_RSCAIR)THEN
        IF (TRANRULE1 .GT. 0 .AND. TRANRULE1 .LT. 9990)THEN
           DO IYR = (TRANRULE1 - UHBSYR) - 1 , (UYR_RSCAIR - UHBSYR) - 1
              EMELBNK(IYR,1) = CEMELBNK(1)
              EMELBNK(IYR,2) = CEMELBNK(2)
!     write(6,1234) curiyr+1989,iyr,iyr + 1989,emelbnk(iyr,1)
!1234 format(1h ,'!cairbnk',i4,i5,i5,f10.3)
           END DO
        END IF
        TRANRULE1 = 9999
        TRANRULE2 = 9999
        TSO2_YR_BY_CLRG = 9999
        TSO2_VR_BY_CLRG = 9999
        DO IYR = UYR_RSCAIR - UHBSYR , MNUMYR
           DO SO2 = 1 , MX_SO2_GRP
              EMRFSA(IYR,SO2) = CEMRFSA(IYR,SO2)
           END DO
           SO2_SHR_ALW_GRP(IYR,1) = CSO2_SHR_ALW_GRP(IYR,1)
           SO2_SHR_ALW_GRP(IYR,2) = CSO2_SHR_ALW_GRP(IYR,2)
        END DO
        DO IRG = 1 , NDREG
           SO2_SHR_BY_CLRG(IRG,1) = CSO2_SHR_BY_CLRG(IRG,1)
           SO2_SHR_BY_CLRG(IRG,2) = CSO2_SHR_BY_CLRG(IRG,2)
        END DO
        DO IRG = 1 , MNUMCR - 1
           SO2_SHR_BY_OLRG(IRG,1) = CSO2_SHR_BY_OLRG(IRG,1)
           SO2_SHR_BY_OLRG(IRG,2) = CSO2_SHR_BY_OLRG(IRG,2)
        END DO
      END IF
!
!     NOX INPUTS
!
      IF ((CURIYR + UHBSYR) .EQ. (TRANRULE1 - UPSCLT))THEN
         NOX_TRDYR = TNOX_TRDYR
         DO NOX = 1 , TNOX_GRP
            NOX_RG(NOX) = TNOX_RG(NOX)
            NOX_NST(NOX) = TNOX_NST(NOX)
            DO IST = 1 , NOX_NST(NOX)
               NOX_ST(IST,NOX) = TNOX_ST(IST,NOX)
               DO IYR = TRANRULE1 - UHBSYR , MNUMYR
                  NOXBYST(IST,NOX,IYR) = TNOXBYST(IST,NOX,IYR)
                  IF (IST .EQ. 1)THEN
                     NOXBYGRP(NOX,IYR) = TNOXBYGRP(NOX,IYR)
                     EMRFNA(NOX,IYR) = TEMRFNA(NOX,IYR)
                  END IF
               END DO
            END DO
!     print *,'!noxbyr',curiyr+1989,nox,nox_rg(nox),nox_nst(nox),noxbygrp(nox,curiyr)
            DO IMO = 1 , 12
               NOX_HRS(IMO,NOX) = TNOX_HRS(IMO,NOX)
            END DO
            DO ISP = 1 , EFDns
               NOX_EFD(ISP,NOX) = TNOX_EFD(ISP,NOX)
            END DO
            DO ISP = 1 , ECPns
               NOX_ECP(ISP,NOX) = TNOX_ECP(ISP,NOX)
            END DO
            DO IRG = 1 , MX_HG_GRP
               NOX_SHR_BY_CLRG(IRG,NOX) = TNOX_SHR_BY_CLRG(IRG,NOX)
            END DO
         END DO
      END IF
!     write(6,1900) curiyr+1989,(curiyr+uhbsyr),(tranrule1-upsclt),nox_grp,tnox_grp
!1900 format(1h ,'!noxgrp0',5i5)
!     write(6,2000) curiyr+1989,nox_trdyr,(nox_rg(nox),nox=1,nox_grp),(nox_nst(nox),nox=1,nox_grp),  &
!                   (noxbygrp(nox,curiyr),nox=1,nox_grp),(emrfna(nox,curiyr),nox=1,nox_grp)
!2000 format(1h ,'!noxgrpi',i4,i5,3a3,3i5,6f10.3)
!     DO NOX = 1 , NOX_GRP
!     write(6,2100) curiyr+1989,(nox_hrs(imo,nox),imo=1,12)
!2100 format(1h ,'!noxgrph',i4,12f8.1)
!     write(6,2200) curiyr+1989,(NOX_EFD(isp,nox),isp=1,EFDns),(NOX_ECP(isp,nox),isp=1,ECPns)
!2200 format(1h ,'!noxgrpe',i4,<EFDns>f8.1,<ECPns>f8.1)
!     write(6,2300) curiyr+1989,(nox_shr_by_clrg(irg,nox),irg=1,mx_hg_grp)
!2300 format(1h ,'!noxgrps',i4,16f6.3)
!     END DO
      RETURN
      END
!
      SUBROUTINE EPA$TRANSHR(REG,PLT,SO2,SO2SHR)
!
!     THIS SUBROUTINE OVERWRITES VARIABLES AFFECTED BY THE
!     CHANGE FROM CAIR TO THE TRANSPORT RULE, IF APPROPRIATE
!
      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'emission'
      include 'csapr'
      include 'cdsparms'
      include 'emmemis'
!
      INTEGER REG,PLT,SO2
      REAL*4 SO2SHR
!
!     SO2 SHARES BY BTU WEIGHTS
!
!     SO2SHR = TBTU_SHR_BY_CLRG(REG,PLT,SO2)
      SO2SHR = TSO2_SHR_BY_CLRG(REG,SO2)

      RETURN
      END
      SUBROUTINE EPA$DSIRMV
!
!     THIS SUBROUTINE PROVIDES THE SO2 REMOVAL RATES
!     FOR DSI IN THE AIR TOXICS RULE, IF APPROPRIATE
!
      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'
      include 'cdsparms'
      include 'uso2grp'
      include 'coalemm'

      INTEGER*4  COL,PLT

!     DO COL = 1 , NDRGG
!        DO PLT = 1 , WIPC - 1
!           IF (ECP_SCRUB(PLT,CURIYR) .EQ. 2)THEN
!              IF (UPLNTCD(PLT)(1:1) .EQ. 'B')THEN
!                 RCLCLNR(COL,CURIYR,PLT) = UPDSISEF(1)
!              ELSE
!                 RCLCLNR(COL,CURIYR,PLT) = UPDSISEF(2)
!              END IF
!           END IF
!        END DO
!     END DO
!     IF ((CURIYR + UHBSYR) .EQ. UDSI_YR)THEN
         DO PLT = 1 , WIPC - 1
            IF (ECP_SCRUB(PLT,CURIYR) .EQ. 2)THEN
!              IF (UPLNTCD(PLT)(1:1) .EQ. 'B')THEN
                  UPPSEF(PLT) = UPDSISEF(1)
!              ELSE
!                 UPPSEF(PLT) = UPDSISEF(2)
!              END IF
            END IF
         END DO
!     END IF

      RETURN
      END
!
      SUBROUTINE EPA$DSICST(PLT,DSICAP,DSIOVR,DSIFOM,DSIVOM,DSISEF)
!
!     THIS SUBROUTINE PROVIDES THE SO2 REMOVAL RATES
!     FOR DSI IN THE AIR TOXICS RULE, IF APPROPRIATE
!
      IMPLICIT NONE
      include 'parametr'
      include 'ncntrl'
      include 'emmparm'
      include 'control'
      include 'ecpcntl'

      INTEGER*4  PLT,TYP,CAT,NCAT,ICAT
      REAL*8 DSICAP,DSIOVR,DSIFOM,DSIVOM,DSISEF,FRAC
!
!     IF ESP THEN FABRIC FILTER REQUIRED SO SAME COSTS USED FOR BH OR ESP
!
!     IF (UPLNTCD(PLT)(1:1) .EQ. 'B')THEN
         TYP = 1
!     ELSE
!        TYP = 2
!     END IF
!
!     CHECK FOR CAPACITY CATEGORY
!
      NCAT = 7
      DO CAT = 1 , NCAT
         IF (DSICAP .GE. DBLE(UPDSICAP(TYP,CAT)))THEN
            ICAT = CAT
         END IF
      END DO

      IF (ICAT .LE. 0 .OR. ICAT .GT. 7) THEN
         WRITE(6,4317) CURIRUN, CURIYR+1989, PLT, TYP, NCAT, ICAT, DSICAP, (UPDSICAP(TYP,CAT),CAT=1,7)
 4317    FORMAT(1X,"EPA_DSICST_OOPS",6(":",I4),8(":",F21.6))
      END IF

      FRAC = (DSICAP - DBLE(UPDSICAP(TYP,ICAT))) /  &
             (DBLE(UPDSICAP(TYP,ICAT + 1) - UPDSICAP(TYP,ICAT)))
      DSIOVR = DBLE(UPDSIOVR(TYP,ICAT)) + FRAC *  &
               (DBLE(UPDSIOVR(TYP,ICAT + 1) - UPDSIOVR(TYP,ICAT)))
      DSIFOM = DBLE(UPDSIFOM(TYP,ICAT)) + FRAC *  &
               (DBLE(UPDSIFOM(TYP,ICAT + 1) - UPDSIFOM(TYP,ICAT)))
      DSIVOM = DBLE(UPDSIVOM(TYP))
      DSISEF = DBLE(UPDSISEF(TYP))

      RETURN
      END
