C              ::: GBAGNDA2.FOR 12-01-92 :::
C
C DATES:  8-14-92...Changed BMINRO to return basket
C         9-23-92...Changed BNONSP using ZVALUE
C                   Added BRLIST, used instead of BROLST
C         9-24-92...Removed DOUBLE PRECISION ZVALUE from BNONSP
C        11-23-92...Fixed bug in BRLIST
C
C This contains the following GETMAT subroutines.
C
C     BFTRI......forward triangularization
C     BBTRI......backward triangularization
C     BFRONT.....assign to front
C     BREAR......assign to rear
C     BKERNL.....assign within kernel
C     BMINRO.....get min row count
C     BROLST.....get list of unassigned columns intersecting row
C     BCOLST.....get list of unassigned rows intersecting column
C     BRLIST.....get unassigned column intersecting each row in list
C     BUKADD.....add row to bucket
C     BUKDEL.....delete row from bucket
C     BSPACE.....check space in INDEX and VALUE arrays.
C     BNONSP.....put alpha vector for nonspike into alpha region
C
C See GALGRTHM.DOC and GETDATA.DOC for details.
C
      SUBROUTINE BFTRI(*)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This executes forward triangularization, seeking to assign row
C singletons to the front.
C ...Alternate return is SYSERR.
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C CALLS  BFRONT, BRLIST
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
C LOOP OVER ROWS WITH COUNT = 1 (1ST BUCKET)
10    CONTINUE
        IF( INDEX(BUCKET+1).EQ.0 )RETURN
        CALL BMINRO(COUNT)
C NOW WE HAVE A LIST OF ROWS WHOSE COUNTS = 1 (JUST A BASKET, NOT ALL)
        CALL BRLIST(*1300)
C LOOP OVER BASKET
        NUMBER = INDEX(IFREE)
        DO 500 I=1,NUMBER
           ROW = INDEX( IFREE+I )
           COL = INDEX( IFREE+NUMBER+I )
           IF( COL.EQ.0 )GOTO 500
C LOCATE COL
           DO 100 CPOSN=FRONT,REAR
              C = INDEX( BCPOSN+CPOSN )
              IF( C.EQ.COL )GOTO 190
100        CONTINUE
C COL IS NO LONGER UNASSIGNED...ROW BECAME NULL
           GOTO 500
190        CONTINUE
C ASSIGN TO FRONT
           CALL BFRONT(ROW,CPOSN,1,*1300)
500     CONTINUE
      GOTO 10
C
C ERROR RETURN
1300  CONTINUE
      IF( SWDBG ) PRINT *,' AT 1300 IN BFTRI'
      RETURN 1
C
C ** BFTRI ENDS HERE
      END
      SUBROUTINE BBTRI(*)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This executes backward triangularization, seeking to assign column
C singletons to the rear.
C ...Alternate return is SYSERR.
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C LOCAL
      LOGICAL*1  SW
C CALLS  BREAR, BCOLST
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
      IF( FRONT.GT.REAR )RETURN
10    CONTINUE
      SW = .FALSE.
C          :...INDICATES NO COLUMN SINGLETON FOUND
C
C ::: LOOP OVER BASIC COLUMNS :::
C (DO NOT USED BECAUSE REAR CHANGES WHEN CALL BREAR)
      CPOSN = FRONT
100   CONTINUE
         IF( INDEX(BCSTAT+CPOSN).NE.1 )GOTO 190
C UNASSIGNED COLUMN IN POSITION CPOSN HAS COUNT = 1
C ...GET ITS (UNASSIGNED) ROW
         CALL BCOLST(CPOSN,1)
         ROW = INDEX( IFREE+1 )
C ASSIGN TO REAR
         CALL BREAR(ROW,CPOSN,1,*1300)
         SW = .TRUE.
190   CONTINUE
C INCREMENT CPOSN
      IF( CPOSN.LT.REAR )THEN
         CPOSN = CPOSN+1
         GOTO 100
      ENDIF
C ::: END LOOP FROM FRONT TO REAR :::
C
      IF( SW ) GOTO 10
      RETURN
C ERROR RETURN
1300  CONTINUE
      IF(SWDBG)THEN
         PRINT *,' ...AT 1300 IN BBTRI'
      ENDIF
      RETURN 1
C
C ** BBTRI ENDS HERE
      END
      SUBROUTINE BFRONT(ROW,CPOSN,COUNT,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This assigns column in position CPOSN with pivot ROW at front.
C Caller passes ROW COUNT (avoids work and needed for bucket deletion).
C ...Alternate return is SYSERR.
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C CALLS  BCOLST, BROLST
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
C SET COL AND ITS NEW COUNT (AFTER ASSIGNING ROW)
      COL    = INDEX(BCPOSN + CPOSN)
      CCOUNT = INDEX(BCSTAT + CPOSN) - 1
C REMOVE ROW FROM BUCKET (IF IN ONE)
      CALL BUKDEL(ROW,COUNT)
C SWITCH CPOSN WITH FRONT
      INDEX(BCPOSN + CPOSN) = INDEX(BCPOSN + FRONT)
      INDEX(BCSTAT + CPOSN) = INDEX(BCSTAT + FRONT)
      INDEX(BCPOSN + FRONT) = COL
      INDEX(BCSTAT + FRONT) = ROW
C SET ROW STATS
      INDEX(IPIVOT + ROW)   = COL
      INDEX(IRSTAT + ROW)   = -INDEX( IRSTAT + ROW)
C ADVANCE FRONT
      FRONT = FRONT + 1
      IF( CCOUNT.GT.0 )THEN
C UPDATE UNASSIGNED ROWS INTERSECTING COL
         I1 = INONZ + 2*BASE(ICINFO + COL-1)
         I2 = INONZ + 2*BASE(ICINFO + COL) - 1
C
         DO 100 I=I1,I2,2
            R = INDEX(I)
            IF( INDEX( IRSTAT + R ).LT.0 )GOTO 100
C  ROW R IS NOT ASSIGNED...SET ITS COUNT (RCOUNT)
            RCOUNT = INDEX( IPIVOT + R )
C  ...REMOVE FROM BUCKET (IF IN ONE)
            CALL BUKDEL(R,RCOUNT)
C  ...DECREMENT ITS COUNT
            RCOUNT = RCOUNT-1
            IF( RCOUNT.EQ.0 )THEN
C    ROW R HAS BECOME NULL...MARK EARLIEST INSERTION
               INDEX(IPIVOT + R) = -FRONT
            ELSE
               INDEX(IPIVOT + R) = RCOUNT
C    ADD R TO BUCKET
               CALL BUKADD(R,RCOUNT)
            ENDIF
            CCOUNT = CCOUNT-1
            IF( CCOUNT.LE.0 )GOTO 110
100      CONTINUE
      ENDIF
C
110   CONTINUE
C FINISHED WITH ROW AND COL
      IF( COUNT.EQ.1 )RETURN
C
C ROW COUNT > 1...FORCE OTHER UNASSIGNED COLUMNS INTERSECTING ROW
C                 TO BE SPIKE
      RCOUNT = COUNT - 1
C                    :...ROW'S COUNT HAS DECREASED BY 1
      CALL BROLST(ROW,RCOUNT)
      NUMBER = INDEX( IFREE )
C
      NBSPIK = NBSPIK + NUMBER
C INDEX(IFREE+i) CONTAINS i-TH COLUMN IN ROW'S LIST (i=1,...,NUMBER)
C SAVE IFREE
      TEMP   = IFREE
C SET CCOUNT = MAX COL COUNT (SINCE WE DO NOT KNOW COL COUNTS)
      CCOUNT = NROWS
C
      DO 200 IFREE=TEMP+NUMBER,TEMP+1,-1
C            :...OK TO USE INDEX(IFREE) AFTER COL POSN (C) IS SET
C                BECAUSE WE'RE GOING BOTTOM-UP
         C = INDEX( IFREE )
C COL IN POSN C IS SPIKE (NO PIVOT ROW ASSIGNED)
         CALL BREAR(0,C,CCOUNT,*1300)
200   CONTINUE
C
C RESTORE IFREE
      IFREE = TEMP
      RETURN
C
C ERROR RETURN
1300  CONTINUE
      IF( SWDBG )PRINT *,' AT 1300 IN BFRONT'
      RETURN 1
C
C ** BFRONT ENDS HERE
      END
      SUBROUTINE BREAR(ROW,CPOSN,COUNT,*)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This assigns column in position CPOSN to rear with pivot ROW.
C ROW = 0 means column is put into rear as a spike, without pivot row.
C Caller passes column COUNT to avoid extra work (does not get used
C in this version).
C ...Alternate return is SYSERR.
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C CALLS  BROLST BUKDEL
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
C SET COL AND ITS STATUS IN POSITION CPOSN
      COL    = INDEX( BCPOSN + CPOSN )
C SWITCH WITH REAR
      INDEX(BCPOSN + CPOSN) = INDEX(BCPOSN + REAR)
      INDEX(BCSTAT + CPOSN) = INDEX(BCSTAT + REAR)
      INDEX(BCPOSN + REAR ) = COL
      IF( ROW.GT.0 )THEN
C ROW = PIVOT ASSIGNMENT
         RCOUNT = INDEX( IPIVOT + ROW   )
C REMOVE ROW FROM BUCKET (IF IN ONE)
         CALL BUKDEL(ROW,RCOUNT)
         INDEX( BCSTAT + REAR )  = ROW
         INDEX( IPIVOT + ROW  )  = COL
         INDEX( IRSTAT + ROW  )  = -INDEX( IRSTAT + ROW )
         REAR = REAR - 1
C DECREMENT COUNTS OF UNASSIGNED COLUMNS INTERSECTING ROW
         RCOUNT = RCOUNT - 1
C                        :...SUBTRACT THE COL JUST ASSIGNED
         CALL BROLST(ROW,RCOUNT)
         NUMBER = INDEX( IFREE )
         IF( NUMBER.GT.0 )THEN
            DO 100 I=IFREE+1,IFREE+NUMBER
               C = INDEX(I)
C  COLUMN C INTERSECTS ROW AND IS UNASSIGNED...DECREMENT ITS COUNT
               INDEX( BCSTAT+C ) = INDEX( BCSTAT+C ) - 1
100         CONTINUE
         ENDIF
      ELSE
C NO PIVOT ASSIGNMENT (COL IS SPIKE)...ENTER EARLIEST INSERTION (FRONT)
         INDEX( BCSTAT + REAR ) = FRONT
         REAR = REAR - 1
      ENDIF
C
      IF( COUNT.EQ.1 )RETURN
C
C IF WE GET HERE, CALLER HAS ASKED THAT COL BE MADE SPIKE AT REAR.
C WE ALREADY PUT COL AT REAR WITH STAT = FRONT (EARLIEST INSERTION).
C NOW WE MUST DECREMENT COUNTS OF COL'S UNASSIGNED ROWS.
C
      RCOUNT = NROWS
      POSN   = REAR+1
      CALL BCOLST(POSN,RCOUNT)
      NUMBER = INDEX( IFREE )
      IF( NUMBER.EQ.0 )RETURN
C NOW INDEX( IFREE+i ) IS AN UNASSIGNED ROW THAT INTERSECTS COL
C (i=1,...,NUMBER)
C
      DO 150 I=IFREE+1,IFREE+NUMBER
         R = INDEX(I)
         RCOUNT = INDEX( IPIVOT+R )
C REMOVE FROM BUCKET (IF IN ONE)
         CALL BUKDEL(R,RCOUNT)
C DECREMENT ROW (R) COUNT
         RCOUNT = RCOUNT - 1
         IF( RCOUNT.EQ.0 )THEN
C    ROW R HAS BECOME NULL...MARK EARLIEST INSERTION
            INDEX( IPIVOT+R ) = -FRONT
         ELSE
            INDEX( IPIVOT+R ) = RCOUNT
C    ADD TO BUCKET (MAYBE)
            CALL BUKADD(R,RCOUNT)
         ENDIF
150   CONTINUE
C
      RETURN
C
C ** BREAR ENDS HERE
      END
      SUBROUTINE BKERNL(*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This assigns from within kernel...for this version we shall assign
C row with min count to front and one of its columns as nonspike.
C ...Alternate return is error (basis singular) or SYSERR.
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C CALLS  BFTRI, BFRONT, BROLST, BMINRO
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
      IF( SWDBG )THEN
         PRINT *,' ENTERED BKERNL WITH FRONT,REAR =',FRONT,REAR
         CALL GDEBUG
      ENDIF
10    CONTINUE
      IF( FRONT.GT.REAR )RETURN
C GET MIN ROW COUNT (LIST OF ROWS PUT INTO INDEX(FREE+1),...)
      CALL BMINRO(COUNT)
      NUMBER = INDEX( IFREE )
      IF( NUMBER.EQ.0 )THEN
C ALL UNASSIGNED ROWS ARE NULL AMONG UNASSIGNED COLS
C ...MAKE REMAINING COLS SPIKE
         DO 50 I=FRONT,REAR
50       INDEX(BCSTAT+I) = FRONT
         NBSPIK = NBSPIK + REAR-FRONT+1
         FRONT = REAR+1
         RETURN
      ENDIF
C ROWS WITH MIN COUNT ARE IN INDEX(IFREE+i) FOR i=1,...,NUMBER
C SAVE IFREE:
      IFREE0= IFREE
C SEE IF WE HAVE ENOUGH SPACE TO TALLY ROWS
      IFRECO= IFREE + NUMBER + 1
      IFRERO= IFRECO+ COUNT  + 1
      MAX   = IFRERO+ REAR-FRONT+1
      IF( COUNT.GT.5 .AND. MAX.LE.PMXIND )GOTO 190
C EITHER COL HAS <= 5 NONZEROES, OR WE DON'T HAVE ENOUGH SPACE TO TALLY,
C SO ASSIGN ONE TO FRONT AS NONSPIKE HAVING GREATEST PIVOT VALUE
      VMAX = 0.
      IFREE = IFRECO
      DO 100 I=1,NUMBER
         R = INDEX( IFREE0+I )
         CALL BROLST(R,COUNT)
         NUMCOL = INDEX(IFREE)
         DO 75 J=1,NUMCOL
            C   = INDEX( IFREE+J )
            COL = INDEX( BCPOSN+C )
            CALL GETAIJ(R,COL,V)
            VABS = ABS( V )
            IF( VABS.GT.VMAX )THEN
               VMAX = VABS
               ROW   = R
               CPOSN = C
            ENDIF
75       CONTINUE
100   CONTINUE
      GOTO 900
C     ========
190   CONTINUE
C     ========
C WE HAVE ENOUGH SPACE TO TALLY THE NUMBER OF FRONT ASSIGNMENTS TO
C PICK ONE THAT REDUCES THE MOST ROW COUNTS FOR SUBSEQUENT ASSIGNMENT
C (WITH HOPE OF MORE TRIANGULARIZATION)
C
C WE SHALL USE INDEX AS FOLLOWS
C            INDEX
C            =====
C IFREE0---> NUMBER OF ROWS WITH MIN COUNT (TO BE TALLIED)
C            ROW 1
C            ...
C            ROW NUMBER
C IFRECO---> FREE SPACE USED BY BROLST TO FETCH ADJACENT COLS (POSNS)
C             (MAX # RESERVED = COUNT)
C IFRERO---> FREE SPACE USED BY BCOLST TO FETCH ADJACENT UNASSIGNED ROWS
C             (MAX # RESERVED = REAR-FRONT+1)
C
C INITIALIZE MAX NUMBER OF NONZEROES TO BE TALLIED (NZMAX)
      NZMAX = 0
      IMAX  = 1
C ::: LOOP OVER ROW CONTENDERS :::
      DO 300 I=1,NUMBER
         ROW = INDEX( IFREE0+I )
         IFREE = IFRECO
         CALL BROLST(ROW,COUNT)
C NOW INDEX( IFRECO+j)=j-TH COL (POSN) INTERSECTING ROW, FOR j=1,...,NC
         IFREE = IFRERO
C INITIALIZE NUMBER OF NONZEROES IN COLUMNS (NUMNZ) AND THE ONE
C WITH MAX LENGTH (CNZMAX)
         NUMNZ = 0
         CNZMAX= 0
C LOOP OVER COLUMNS INTERSECTING ROW TO TALLY THEIR ROW COUNTS
         DO 200 J=1,COUNT
            COLPSN = INDEX( IFRECO+J )
            COLCNT = INDEX(BCSTAT+COLPSN)
            NUMNZ = NUMNZ + COLCNT
            IF( CNZMAX.LT.COLCNT )THEN
               CNZMAX = COLCNT
               CROMAX = COLPSN
            ENDIF
200      CONTINUE
C NOW NUMNZ = NUMBER OF NONZEROES IN COLUMNS THAT WOULD BE ASSIGNED
C IF ROW IS ASSIGNED (WITH THE DENSEST OF THOSE COLUMNS, CROMAX)
C           = ROW COUNT DECREMENTATION
C ...WE SEEK ROW WITH MAX VALUE OF NUMNZ
         IF( NUMNZ.GT.NZMAX )THEN
            NZMAX = NUMNZ
            IMAX  = I
            COLMAX= CROMAX
         ENDIF
300   CONTINUE
C
C NOW INDEX( IFREE0+IMAX ) = ROW THAT WILL CAUSE MOST REDUCTION IN
C NEW ROW COUNTS IF ASSIGNED TO FRONT.  ITS DENSEST COLUMN IS COLMAX,
C WHICH WE ASSIGN TO MINIMIZE ARITHMETIC THAT WILL BE DONE FOR ALPHA
C CREATION (IE, ALPHA OF NON-SPIKE IS ORIGINAL COLUMN).
C
      ROW   = INDEX( IFREE0+IMAX )
      CPOSN = COLMAX
C
900   CONTINUE
C RESTORE IFREE
      IFREE = IFREE0
C ASSIGN COLUMN IN CPOSN TO FRONT WITH PIVOT ROW
      CALL BFRONT(ROW,CPOSN,COUNT,*1313)
      CALL BFTRI(*1313)
      GOTO 10
C
C ** SYSERR
1313  CONTINUE
      IF( SWDBG )THEN
         PRINT *,' AT 1313 IN BKERNL'
         CALL GDEBUG
      ENDIF
      RETURN 1
C
C ** BKERNL ENDS HERE
      END
      SUBROUTINE BMINRO(COUNT)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This determines unassigned rows of min positive count.  The list of
C rows having this min count is put into INDEX( IFREE+1 ),..., and the
C number in this list is put into INDEX(IFREE).
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
      PARAMETER (BASKET=10)
C                :...MAX NUMBER ROWS RETURNED (IF COUNT > 1)
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
      IF( SWDBG )THEN
         PRINT *,' ENTERED BMINRO'
         PAUSE = PAUSE-1
         IF(PAUSE.LE.0)CALL GDEBUG
      ENDIF
C INITIALIZE NUMBER OF ROWS HAVING MIN COUNT
      NUMBER = 0
C
C LOOK FOR BUCKET
      DO 100 COUNT=1,NBUCKT
         IF( INDEX( BUCKET+COUNT ).EQ.0 )GOTO 100
C ROWS IN BUCKET HAVE MIN COUNT...TRAVERSE LIST TO COPY
         ROW = INDEX( BUCKET+COUNT )
         IF( COUNT.EQ.1 )THEN
            MAXNUM = (PMXIND-IFREE-NROWS)/2
         ELSE
            MAXNUM = BASKET
         ENDIF
50       CONTINUE
           NUMBER = NUMBER+1
           INDEX(IFREE + NUMBER) = ROW
           IF( NUMBER.EQ.MAXNUM )GOTO 900
           ROW = INDEX( BLSUCC+ROW )
         IF( ROW )900,900,50
100   CONTINUE
C
900   CONTINUE
      INDEX(IFREE) = NUMBER
      RETURN
C
C ** BMINRO ENDS HERE
      END
      SUBROUTINE BROLST(ROW,COUNT)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This obtains list of positions of unassigned columns intersecting
C row, put into INDEX(IFREE+1),... with number put into INDEX(IFREE).
C Caller passes (max) ROW COUNT to avoid extra work, especially if
C COUNT = 1.
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C LOCAL
      CHARACTER*1 CHAR
C CALLS  GETSGN
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
C INITIALIZE NUMBER
      NUMBER = 0
      IF( FRONT.GT.REAR )GOTO 900
C  LOOP OVER POSITIONS BETWEEN FRONT AND REAR (UNASSIGNED)
C  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO 100 POSN=FRONT,REAR
         COL = INDEX(BCPOSN + POSN)
         CALL GETSGN(ROW,COL,CHAR)
         IF( CHAR.EQ.'-' .OR. CHAR.EQ.'+' )THEN
            NUMBER = NUMBER + 1
            INDEX(IFREE + NUMBER) = POSN
            IF( NUMBER.GE.COUNT )GOTO 900
         ENDIF
100   CONTINUE
C
900   CONTINUE
      INDEX(IFREE) = NUMBER
      RETURN
C
C ** BROLST ENDS HERE
      END
      SUBROUTINE BCOLST(CPOSN,COUNT)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This obtains list of unassigned rows intersecting the column in CPOSN
C ...put into INDEX( IFREE+1 ),... with number put into INDEX(IFREE).
C Caller passes max column COUNT to avoid extra work.
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
C GET COLUMN NUMBER (COL)
      COL = INDEX(BCPOSN + CPOSN)
C INITIALIZE NUMBER OF INTERSECTING ROWS
      NUMBER = 0
C SETUP LOOP OVER COL
      I1 = INONZ + 2*BASE(ICINFO + COL-1)
      I2 = INONZ + 2*BASE(ICINFO + COL) - 1
C
      DO 100 I=I1,I2,2
         ROW = INDEX(I)
         IF(INDEX( IRSTAT+ROW ).GT.0)THEN
            NUMBER = NUMBER + 1
            INDEX( IFREE+NUMBER ) = ROW
            IF( NUMBER.GE.COUNT )GOTO 900
         ENDIF
100   CONTINUE
C
900   INDEX(IFREE) = NUMBER
      RETURN
C
C ** BCOLST ENDS HERE
      END
      SUBROUTINE BRLIST(*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This gets the (unique) unassigned column for each row in
C INDEX(IFREE+1),...,INDEX(IFREE+NUMBER), where NUMBER = INDEX(IFREE).
C The column positions are put into INDEX(IFREE+NUMBER+1),...,
C INDEX(IFREE + 2*NUMBER).
C
C ...Alternate return is not enough space, or some fatal error.
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C LOCAL
      CHARACTER*1  CHAR
      CHARACTER*16 RNAME
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
      NUMBER = INDEX(IFREE)
      IF( NUMBER.LE.0 )RETURN
      IF( IFREE+2*NUMBER .GT. PMXIND )THEN
         PRINT *,' Sorry, not enough INDEX space...need',
     1           IFREE+2*NUMBER,'...Max =',PMXIND
         RETURN 1
      ENDIF
C
C FOR EACH ROW IN LIST,
C LOOP OVER POSITIONS BETWEEN FRONT AND REAR (UNASSIGNED COLS)
C
      DO 200 I=1,NUMBER
         ROW = INDEX(IFREE+I)
         IF( INDEX(IRSTAT+ROW) .LE. 0 .OR.
     1       INDEX(IPIVOT+ROW) .LE. 0 )THEN
C ROW IS NOT ASSIGNED OR HAS 0 COUNT...RETURN COL=0
            INDEX(IFREE+NUMBER + I) = 0
            GOTO 200
         ENDIF
C ROW IS NOT ASSIGNED AND HAS POSITIVE COUNT...LOOK FOR ITS COL
         DO 100 POSN=FRONT,REAR
            COL = INDEX(BCPOSN + POSN)
            CALL GETSGN(ROW,COL,CHAR)
            IF( CHAR.EQ.'-' .OR. CHAR.EQ.'+' )THEN
               INDEX(IFREE+NUMBER + I) = COL
               GOTO 200
            ENDIF
100      CONTINUE
C WE SHOULD NOT REACH HERE BECAUSE ROW SHOULD HAVE COL
         GOTO 1300
200   CONTINUE
      RETURN
C
C NOW EACH (SINGLETON) ROW HAS ITS COL IDENTIFIED (IE, EACH ROW HAS
C ONE COL, THOUGH THE SAME COL COULD BE THE ONE FOR MORE THAN ONE
C ROW).  WE STORE COL, RATHER THAN POSN, BECAUSE THE POSITION MIGHT
C CHANGE WHEN A ROW IS ASSIGNED (TO THE FRONT).
C
1300  CONTINUE
      PRINT *,' ** SYSERR BRLIST...I =',I
      PRINT *,' COUNT OF ROW',ROW,':'//NAME(IRNAME+ROW),'=',
     1          INDEX(IPIVOT+ROW)
      IF( SWDBG )THEN
         PRINT *,' FRONT,REAR =',FRONT,REAR
         READ(*,'(A1)')CHAR
         IF( CHAR.NE.' ')RETURN
         PAUSE = 22
         DO 1301 POSN=1,NROWS-NBLGL
            COL = INDEX(BCPOSN + POSN)
            CALL GETSGN(ROW,COL,CHAR)
            IF(CHAR.EQ.' ')GOTO 1301
            IF( PAUSE.LE.0 )THEN
               READ(*,'(A1)')CHAR
               IF( CHAR.NE.' ')RETURN
               PAUSE = 23
            ENDIF
            IF( POSN.LT.FRONT .OR. POSN.GT.REAR )THEN
               ROW = INDEX(BCSTAT+POSN)
               RNAME = NAME(IRNAME+ROW)
            ELSE
               RNAME = 'UNASSIGNED'
            ENDIF
            PRINT *,POSN,COL,':',NAME(ICNAME+COL),CHAR//' '//RNAME
            PAUSE = PAUSE-1
1301     CONTINUE
      ENDIF
      RETURN 1
C
C ** BRLIST ENDS HERE
      END
      SUBROUTINE BUKADD(ROW,COUNT)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This adds ROW to COUNT bucket.
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
      IF( COUNT.LE.0 )RETURN
      SUCC = INDEX( BUCKET + COUNT )
      INDEX( BLSUCC+ROW   ) = SUCC
      INDEX( BLPRED+ROW   ) = 0
      IF( SUCC.GT.0 ) INDEX( BLPRED+SUCC  ) = ROW
C ROW BECOMES NEW ENTRY
      INDEX( BUCKET+COUNT ) = ROW
C
      RETURN
C
C ** BUKADD ENDS HERE
      END
      SUBROUTINE BUKDEL(ROW,COUNT)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This deletes ROW from COUNT bucket.
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
      IF( COUNT.LE.0 )RETURN
      PRED = INDEX( BLPRED + ROW )
      SUCC = INDEX( BLSUCC + ROW )
      IF( SUCC.GT.0 )INDEX( BLPRED+SUCC ) = PRED
      IF( PRED.GT.0 ) THEN
         INDEX( BLSUCC+PRED ) = SUCC
      ELSE
C ROW WAS ENTRY INTO BUCKET
         INDEX( BUCKET+COUNT ) = SUCC
      ENDIF
C
      RETURN
C
C ** BUKDEL ENDS HERE
      END
      SUBROUTINE BSPACE(NEED,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This checks there is enough INDEX and VALUE space to store NEED.
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C LOCAL
      LOGICAL*1  SW
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
      SW = .FALSE.
      SPACE = NBLST+NEED
      IF( IRBLST+SPACE.GT.MAXIND )THEN
         PRINT *,' ** Not enough INDEX space...need at least',
     1           IRBLST+SPACE+PMXIND-MAXIND
         SW = .TRUE.
      ENDIF
      IF( IVBLST+2*SPACE.GT.MAXVAL )THEN
         PRINT *,' ** Not enough VALUE space...need at least',
     1           IVBLST+2*SPACE+PMXVAL-MAXVAL
         SW = .TRUE.
      ENDIF
      IF( SW )THEN
CC         PRINT *,' BASIS NOT SETUP'
         IF( SWDBG )THEN
            PRINT *,' IN BSPACE, NBLST,NEED =',NBLST,NEED
            CALL GDEBUG
         ENDIF
         RETURN 1
      ENDIF
C
      RETURN
C
C ** BSPACE ENDS HERE
      END
      SUBROUTINE BNONSP(ROW,COL,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This puts alpha vector of nonspike COL with pivot ROW into alpha
C region. (Caller checked space).
C ...Alternate return is SYSERR.
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
      INDEX(IPIVOT + ROW) = COL
C STORE 2 RECORDS
      INDEX (IRBLST + NBLST+1) = -ROW
      ZVALUE(IZBLST + NBLST+1) =  0.
      NBLST = NBLST+2
      INDEX (IRBLST + NBLST)   = -COL
C  LOOP OVER COL'S NONZEROES TO GET PIVOT (STORED IN 2ND VALUE)
      I1 = INONZ + 2*BASE(ICINFO + COL-1)
      NZ = BASE(ICINFO+COL) - BASE(ICINFO+COL-1)
      I2 = I1 + 2*NZ - 1
C
      DO 100 I=I1,I2,2
         IF( INDEX(I).NE.ROW )GOTO 100
         CALL GETVAL(V,I+1)
         ZVALUE(IZBLST + NBLST) = -V
C                                 :...STORED ALPHA = -COEF
         RETURN
100   CONTINUE
      PRINT *,' ** SYSERR IN BNONSP...',ROW,':',NAME(IRNAME+ROW),
     1       COL,':',NAME(ICNAME+COL)
      RETURN 1
C
C ** BNONSP ENDS HERE
      END
