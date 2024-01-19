C              ::: GBAGNDA1.FOR  8-13-95 :::
C
C Earlier dates deleted.
C    3-12-93...Advanced FRONT (for GDEBUG) at end of BAGNDA
C    8-02-95...Removed above change
C
C This contains the following GETMAT subroutines.
C
C       BAGNDA.....agenda algorithm (primary link to outside callers)
C       BASET......initialize basis agenda algorithm
C       BASTAT.....basis statistics (another link to outside callers)
C       GSPIKE.....add|delete spikes|nonspikes to|from submatrix
C                   (another link to outside callers)
C       BASPIK.....insert spike into alpha region
C       BSUPD8.....update stats after spike insertion into alpha region
C
C See GALGRTHM.DOC and GETDATA.DOC for details.
C
      SUBROUTINE BAGNDA(ALPHA,NALPHA,SWMSG,FREQMS,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This initializes alpha region for basis in memory.
C Caller must give ALPHA, whose length = NALPHA.  This may not be needed
C (ie., if basis triangularizes), but if it is, we require NALPHA >= NROWS.
C
      DOUBLE PRECISION ALPHA(NALPHA)
      LOGICAL*1   SWMSG
      INTEGER     FREQMS
C                 :...MESSAGE FREQUENCY DURING ALGORITHM
C ADDED 3-12-93:   STBASC SET = 1 IF SUCCESSFUL
C                               2 IF FAILS BY SPIKE INSERTION
C                               3 IF FAILS OTHERWISE
C                  RCODE = 2 IF STBASC = 2 UPON ENTRANCE
C
C LOCAL
      CHARACTER*1 CHAR
C SET SPIKE PIVOT TOLERANCE
      REAL       TOLPIV
      PARAMETER (TOLPIV=1.0E-15)
C
C CALLS  GALPHA, GETVAL, GFTRAN, BASET, BKERNL, BFTRI, BBTRI
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
      IF( STBASC.EQ.2 )THEN
         RCODE = 2
         RETURN
      ENDIF
C INITIALIZE
      CALL BASET(*1313)
      VPIVOT = VTOLAB
C PIVOT TOLERANCE FOR NON-SPIKES = ABSOLUTE ZERO (SET BY USER)
C ...VTOLAB >= NON-ZEROES IN MATRIX
      IF( NBPOSN.EQ.0 )GOTO 900
C                   :...BASIS IS ALL LOGICAL
C INITIALIZE AGENDA
      FRONT = 1
      REAR  = NBPOSN
      IF( (SWMSG .AND. NBPOSN.GT.1 000) .OR. SWDBG )
     1   PRINT *,' Beginning triangularization'
C FORWARD TRIANGULARIZATION
      CALL BFTRI(*1313)
      NBFTRI = FRONT - 1
      IF( (SWMSG .AND. NBPOSN-NBFTRI.GT.500) .OR. SWDBG )
     1   PRINT *,NBFTRI,' structurals in FTRI'
C BACKWARD TRIANGULARIZATION
      CALL BBTRI(*1313)
      NBBTRI = NBPOSN - REAR
      NBKRNL = NBPOSN - NBFTRI - NBBTRI
C     :        :        :        :...NUMBER IN BACK TRIANGLE
C     :        :        :            (NOT COUNTING BASIC LOGICALS)
C     :        :        :...NUMBER IN FORWARD TRIANGLE
C     :        :...NUMBER OF BASIC STRUCTURALS
C     :...SIZE OF KERNEL
C INITIAL TRIANGULARIZATION COMPLETE...NBKRNL=0 IFF BASIS TRIANGULARIZES
      IF( (SWMSG .AND. NBKRNL.GT.500) .OR. SWDBG )THEN
         WRITE(*,11) NBLGL,NBPOSN,NBFTRI,NBBTRI
11       FORMAT(' Phase 1 complete...basis has',I5,' logicals and',
     1          I7,' structurals:'/
     2          5X,I7,' in forward triangle (FTRI)'/
     3          5X,I7,' in back    triangle (BTRI)'/
     X         )
      ENDIF
      IF( NBKRNL.GT.0 )THEN
C BASIS DOES NOT TRIANGULARIZE
         IF( NALPHA.LT.NROWS )THEN
            PRINT *,' ** SYSERR...CALLER PASSED NALPHA =',NALPHA,
     1              ' < NROWS =',NROWS
            GOTO 1313
         ENDIF
         CALL BKERNL(*1313)
         IF( (SWMSG .AND. NBKRNL.GT.500) .OR. SWDBG )THEN
            WRITE(*,12) NBKRNL,NBSPIK
12          FORMAT(5X,I7,' in kernel...',I6,' spikes')
         ENDIF
         IF( SWMSG )PRINT *,' Transforming basis...please wait.'
      ENDIF
C ======================= AGENDA COMPLETE ============================
C RELOCATE AGENDA LISTS TO MAKE ROOM FOR (PERMANENT) ALPHA REGION
C (WE PUT AT FRONT OF INDEX BEFORE TO LEAVE ROOM (IFREE) FOR
C  GETTING INTERSECTING ROWS/COLS)
      BCP0   = BCPOSN
      BCS0   = BCSTAT
      BCPOSN = MAXIND - NBPOSN
      BCSTAT = BCPOSN - NBPOSN
      MAXIND = BCSTAT - 1
      CALL BSPACE(2*NBPOSN,*1313)
C
      DO 150 I=1,NBPOSN
         INDEX( BCPOSN+I ) = INDEX( BCP0+I )
         INDEX( BCSTAT+I ) = INDEX( BCS0+I )
150   CONTINUE
C
C SET FRONT AND REAR TO MARK LAST AND FIRST SPIKE, RESP.
C
      REAR  = NBFTRI + NBKRNL
      FRONT = REAR   - NBSPIK
C INDEX( BCPOSN|BCSTAT + FRONT+i) = INFO OF i-TH SPIKE
C
C           |\
C           | \
C           |  \ _______
C           |   |    |  |
C           |   |    |  |
C           |   |    |  |
C           |   |____|__|
C           |   :    :  :\
C           |   :    :  : \
C           |___:____:__:__\
C               :    :  :   :...NBPOSN
C               :    :  :...REAR    (FIRST SPIKE) = END OF KERNEL
C               :    :......FRONT+1 (LAST SPIKE)
C               :...NBFTRI+1 (BEGINNING OF KERNEL)
C
C SET LISTS AND FORM ALPHA VECTORS
C SET BASE POINTERS OF ALPHA REGION:
C  IRBLST===>INDEX, IVBLST===>VALUE, IZBLST===>ZVALUE
      IRBLST = IPIVOT + NROWS + 1
      IVBLST = IVFREE   + 1
      IZBLST = IVBLST/2 + 1
C ...FIRST ALPHA RECORD IS DUMMY (NEEDED FOR BTRAN)
      INDEX(IRBLST) = 0
      ZVALUE(IZBLST) = VINF
C NOW WE HAVE:          INDEX
C                         0    <--- IRBLST (BEGIN ALPHA REGION)
C                        ...
C                              <--- MAXIND (END OF FREE SPACE)
C                              <--- BCSTAT (BEGIN STAT IN AGENDA)
C                        ...
C                              <--- BCPOSN (BEGIN COL  IN AGENDA)
C                        ...
C                              <--- REAR (JUST BEFORE BTRI)
C                        ...
C                        ...   <--- PMXIND = TRUE END OF INDEX
C INITIALIZE
C ...NUMBER OF COLUMNS REMAINING TO BE INSERTED
      REMAIN = NBPOSN
C ...MESSAGE FREQUENCY
      FREQCH = FREQMS
C ...SOME DEBUG STATS
      VPMIN  = 1.
      SPIKIN = 0
C
C    LOOP OVER BASIC LISTS
C
      IF( NBFTRI.GT.0 )THEN
C FIRST, THE FORWARD TRIANGLE (NONSPIKES)
         DO 200 I=1,NBFTRI
            ROW = INDEX( BCSTAT+I )
            COL = INDEX( BCPOSN+I )
            CALL BNONSP(ROW,COL,*1300)
            IF( SWMSG )THEN
               REMAIN = REMAIN-1
               FREQCH = FREQCH-1
               IF( FREQCH.LE.0 )THEN
                  PRINT *,' Column ',NAME(ICNAME+COL),
     1                  ' in FTRI pivoting on row ',NAME(IRNAME+ROW)
                  PRINT *,REMAIN,' columns remain...Please wait'
                  FREQCH = FREQMS
               ENDIF
            ENDIF
200      CONTINUE
      ENDIF
C
      IF( NBKRNL.EQ.0 )GOTO 800
C SET PIVOT TOLERANCE FOR SPIKES = MAX{VTOLAB,TOLPIV}
      IF( TOLPIV.GT.VTOLAB )THEN
         VPIVOT = TOLPIV
      ELSE
         VPIVOT = VTOLAB
      ENDIF
C
      IF( SWMSG .AND. NBFTRI.GT.FREQMS )
     1   PRINT *,NBFTRI,' FTRI NONSPIKES INSERTED'
C
C SECOND, THE KERNEL
C
      DO 600 I=NBFTRI+1,FRONT
C  PUT NONSPIKE
         ROW = INDEX( BCSTAT+I )
         COL = INDEX( BCPOSN+I )
         CALL BNONSP(ROW,COL,*1300)
         IF( SWMSG )THEN
            REMAIN = REMAIN-1
            FREQCH = FREQCH-1
            IF( FREQCH.LE.0 )THEN
               PRINT *,' Column ',NAME(ICNAME+COL),
     1               ' (nonspike) pivoting on row ',NAME(IRNAME+ROW)
               PRINT *,REMAIN,' columns remain...',NBSPIK-SPIKIN,
     1                        ' spikes'
               IF( NBSPIK.GT.SPIKIN )
     1            PRINT *,' ...Fill-in so far =',NBFILL
               FREQCH = FREQMS
            ENDIF
         ENDIF
C
300      CONTINUE
         IF( FRONT.GE.REAR )GOTO 600
C  SEE IF NEXT SPIKE CAN BE INSERTED
         STAT = INDEX( BCSTAT+REAR )
         IF( STAT.GT.I )GOTO 600
C ATTEMPT TO INSERT SPIKE
         COL  = INDEX( BCPOSN+REAR )
         POSN = I
         CALL BASPIK(ALPHA,NALPHA,COL,VPIVOT,POSN,ROW,VPVAL,*1300)
         IF( ROW.GT.0 )THEN
C INSERTION SUCCESSFUL
            REAR   = REAR-1
            IF( SWMSG )CALL BSUPD8
     1         (ROW,COL,VPVAL,VPMIN,SPIKIN,REMAIN,FREQCH,FREQMS)
            GOTO 300
         ENDIF
C INSERTION ATTEMPT UNSUCCESSFUL...BASPIK TELLS US THAT WE SHOULD
C WAIT UNTIL THIS COL IS IN POSN (> I)
         IF( NBSPIK-SPIKIN.GT.1 )THEN
C    SLIDE IT DOWN THE AGENDA TO KEEP POSITION ORDER
            DO 450 I1=REAR-1,FRONT+1,-1
               STAT = INDEX( BCSTAT+I1 )
               IF( STAT.GE.POSN )GOTO 460
C    ...SLIDE COL AT I1 DOWN (TOWARDS REAR)
               INDEX( BCSTAT+I1 + 1) = STAT
               INDEX( BCPOSN+I1 + 1) = INDEX( BCPOSN+I1 )
450         CONTINUE
            I1 = FRONT
460         CONTINUE
C NOW PUT COL AT VACATED POSITION (AT I1+1)
            INDEX( BCPOSN+I1 + 1 ) = COL
            INDEX( BCSTAT+I1 + 1 ) = POSN
            GOTO 300
         ENDIF
C JUST CHANGE COL'S STAT (EARLIEST POSITION INCREASED TO POSN)
         INDEX( BCSTAT+REAR ) = POSN
C NEXT KERNEL NONSPIKE
600   CONTINUE
C
      IF( FRONT.LT.REAR )THEN
C SPIKES REMAIN TO BE INSERTED
C REDUCE PIVOT TOLERANCE = MIN{TOLPIV,VTOLAB}
         IF( TOLPIV.LT.VPIVOT ) VPIVOT = TOLPIV
         IF( SWDBG )THEN
            PRINT *,' AFTER 600, FRONT,REAR=',FRONT,REAR
            PRINT *,' ...BCPOSN,BCSTAT,SPIKIN=',BCPOSN,BCSTAT,SPIKIN
            CALL GDEBUG
         ENDIF
         DO 690 I=REAR,FRONT+1,-1
            COL = INDEX( BCPOSN+I )
            POSN = I
            CALL BASPIK(ALPHA,NALPHA,COL,VPIVOT,POSN,ROW,VPVAL,*1300)
            IF( ROW.EQ.0 )GOTO 1200
C SPIKE INSERTED...UPDATE
            REAR   = REAR-1
            IF( SWMSG )CALL BSUPD8
     1         (ROW,COL,VPVAL,VPMIN,SPIKIN,REMAIN,FREQCH,FREQMS)
690      CONTINUE
C OK, ALL SPIKES ARE IN
      ENDIF
C END OF KERNEL (AND FTRI)
C
      IF( NBBTRI.GT.0 )THEN
C FINALLY, THE BACK TRIANGLE
C RESTORE PIVOT TOLERANCE TO VTOLAB
         VPIVOT = VTOLAB
         DO 700 I=NBFTRI+NBKRNL+1,NBPOSN
            ROW = INDEX( BCSTAT+I )
            COL = INDEX( BCPOSN+I )
            CALL BNONSP(ROW,COL,*1300)
            IF( SWMSG )THEN
               FREQCH = FREQCH-1
               IF( FREQCH.LE.0 )THEN
                  PRINT *,' Column ',NAME(ICNAME+COL),
     1                  ' in BTRI pivoting on row ',NAME(IRNAME+ROW)
                  PRINT *,' ...Please wait'
                  FREQCH = FREQMS
               ENDIF
            ENDIF
700      CONTINUE
      ENDIF
C ====================== DONE ========================
C
C    SUCCESSFULL COMPLETION
800   CONTINUE
      STBASC = 1
C SET FREE SPACE POINTERS
      IFREE  = IRBLST +   NBLST + 1
      IVFREE = IVBLST + 2*NBLST + 1
C ADVANCE FRONT PAST REAR (FOR GDEBUG)
CC      FRONT  = REAR+1 ...REMOVED 8-13-5 (NEEDS TO BE = FOR PIVOT)
C RESTORE INDEX LENGTH
      MAXIND = PMXIND
C
900   CONTINUE
C
      RETURN
C     ======
C
1200  CONTINUE
C COULD NOT INSERT ALL SPIKES...BASIS SETUP FAILS
      N = NBSPIK-SPIKIN
      IF( N.NE.REAR-FRONT )THEN
         PRINT *,' ** SYSERR IN BAGNDA...',NBSPIK,' -',SPIKIN,' =',N
         PRINT *,'                     <>',REAR  ,' -',FRONT,' =',
     1              REAR-FRONT
         GOTO 1300
      ENDIF
      STBASC = 2
      COL = INDEX( BCPOSN+REAR )
      PRINT *,' There are',N,' (spike) columns that cannot be in'//
     1        ' the basis...eg, ',NAME(ICNAME + COL)
      IF( SWDBG )THEN
         PRINT *,' FRONT,REAR =',FRONT,REAR
         LINE = 5
         DO 1213 I=REAR,REAR-N-1,-1
            COL = INDEX( BCPOSN+I)
            PRINT *,I,':',NAME( ICNAME+COL ),INDEX( BCSTAT+I )
            IF( LINE.GT.21 )THEN
               READ(*,'(A1)')CHAR
               IF( CHAR.NE.' ')GOTO 1300
               LINE = 0
            ENDIF
1213     CONTINUE
      ENDIF
C FALL THRU TO ERROR RETURN
C
C ** ERROR RETURNS
1300  CONTINUE
      IF( SWDBG )THEN
         PRINT *,' AT 1300 IN BAGNDA (BEFORE RESTORING FREE SPACE)'
         CALL GDEBUG
      ENDIF
C RESTORE FREE VALUE SPACE
      IVFREE = IVBLST
1313  RCODE = 1
C RESTORE FREE INDEX SPACE
      IFREE  = IPIVOT
      IPIVOT = 0
C ...AND LENGTH OF INDEX
      MAXIND = PMXIND
C
      IF( STBASC.NE.2 )STBASC = 3
      RETURN
C
C ** BAGNDA ENDS HERE
      END
      SUBROUTINE BASET(*)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This initializes basis agenda algorithm
C ...Alternate return is error...the caller must reset IFREE and IPIVOT
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C LOCAL
      INTEGER*2 INT2
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
C SET IPIVOT LOCATION (PERMANENT LIST)
      IPIVOT = IFREE
      IFREE  = IPIVOT + NROWS + 1
C INITIALIZE ALPHA REGION POINTER (FOR GDEBUG ONLY)
      IRBLST = 0
      IF( IFREE.GT.MAXIND )GOTO 1300
C INITIALIZE COUNTS AND OTHER POINTERS/OFFSETS
C ...NUMBER IN FORWARD TRIANGLE
      NBFTRI = 0
C ...NUMBER IN BACKWARD TRIANGLE
      NBBTRI = 0
C ...NUMBER IN KERNEL
      NBKRNL = 0
C ...NUMBER OF SPIKES
      NBSPIK = 0
C ...FILL-IN (NUMBER OF NEW NONZEROES)
      NBFILL = 0
C ...LENGTH OF ALPHA REGION
      NBLST  = 0
C ...NUMBER OF BUCKETS (WILL = LONGEST NONBASIC ROW IN BASIS)
      NBUCKT = 0
C
C PUT BASIC LOGICALS IN REAR
      NBPOSN = NROWS
C
      DO 100 ROW=1,NROWS
         IF( INDEX( IRSTAT+ROW ).LT.0 )
     1       INDEX( IRSTAT+ROW ) = -INDEX( IRSTAT+ROW )
         IF( INDEX( IRSTAT+ROW ).GE.3 )THEN
C ROW IS BASIC (STAT = 3 OR 4)
                         INT2 = -ROW
C                             :...This is because MS compiler has bug
            INDEX( IPIVOT+ROW ) = INT2
            INDEX( IRSTAT+ROW ) = -INDEX( IRSTAT+ROW )
C  ...STAT < 0 USED THROUGHOUT TO FLAG ASSIGNED ROW
            NBPOSN = NBPOSN - 1
         ELSE
C ROW IS NONBASIC (STAT = 1 OR 2)
            INDEX( IPIVOT+ROW ) = 0
C  ...THIS HOLDS ROW COUNT DURING AGENDA CONSTRUCTION (INTIALIZED = 0)
         ENDIF
100   CONTINUE
C
C NOW NBPOSN = NUMBER OF BASIC STRUCTURALS, SO NUMBER OF BASIC LOGICALS
C MUST BE THE OTHERS:
      NBLGL = NROWS - NBPOSN
C
C ALLOCATE MEMORY FOR BASIS LISTS (SEE GETBASIS.DOC)
      BCSTAT = IFREE  + NROWS
      BCPOSN = BCSTAT + NBPOSN
      IFREE  = BCPOSN + NBPOSN + 1
      IF( IFREE+NROWS.GE.MAXIND )GOTO 1300
C
C PUT BASIC COLUMNS INTO POSITION LIST
      FRONT  = 0
      NBASNZ = 0
C
      DO 500 COL=1,NCOLS
         IF( INDEX( ICSTAT+COL ).LT.0 )
     1       INDEX( ICSTAT+COL ) = -INDEX( ICSTAT+COL )
         IF( INDEX( ICSTAT+COL ).LT.3 )GOTO 500
         IF( FRONT.EQ.NBPOSN )THEN
            PRINT *,' Basis has too many structural activities'
            GOTO 1390
         ENDIF
C  PUT COL INTO FRONT POSITION
         FRONT = FRONT + 1
         INDEX( BCPOSN + FRONT ) = COL
C  SET COL COUNT AND UPDATE ROW COUNTS
         NZ = BASE(ICINFO + COL) - BASE(ICINFO + COL-1)
         I1 = INONZ + 2*BASE(ICINFO + COL-1)
         I2 = I1 + 2*NZ - 1
         NBASNZ = NBASNZ + NZ
         COUNT = 0
C
         DO 300 I=I1,I2,2
            ROW = INDEX(I)
            IF( INDEX( IRSTAT+ROW ).LT.0 )GOTO 300
C   ROW IS NOT ASSIGNED...ADD TO COLUMN COUNT
            COUNT = COUNT + 1
C   ...AND TO ROW COUNT
            RCOUNT = INDEX( IPIVOT+ROW ) + 1
            INDEX( IPIVOT+ROW ) = RCOUNT
            IF( RCOUNT.GT.NBUCKT )NBUCKT = RCOUNT
300      CONTINUE
C
         IF( COUNT.EQ.0 )THEN
            PRINT *,' ** NULL COLUMN IN BASIS: ',NAME(ICNAME + COL)
            GOTO 1390
         ENDIF
         INDEX( BCSTAT + FRONT ) = COUNT
500   CONTINUE
C
      IF( FRONT.LT.NBPOSN )THEN
         PRINT *,FRONT,' is insufficient number of basic structurals'
         GOTO 1390
      ENDIF
C
C NOW INDEX(IPIVOT+i) = COUNT OF ROW i, IF NOT ASSIGNED
C ...FORM BUCKETS (NBUCKT = LONGEST ROW COUNT)
      BUCKET = IFREE
      BLSUCC = BUCKET + NBUCKT
      BLPRED = BLSUCC + NROWS
      IFREE  = BLPRED + NROWS + 1
      IF( IFREE+NROWS.GT.MAXIND )GOTO 1300
C INITIALIZE BUCKETS TO BE EMPTY
      DO 750 I=1,NBUCKT
750   INDEX( BUCKET+I ) = 0
C
      DO 800 ROW=1,NROWS
         IF( INDEX( IRSTAT+ROW ).LT.0 )GOTO 800
C ROW IS NOT ASSIGNED (IE, NOT BASIC)
         COUNT = INDEX( IPIVOT+ROW )
         IF( COUNT.EQ.0 )THEN
            PRINT *,' ** NULL ROW IN BASIS: ',NAME(IRNAME+ROW)
            GOTO 1390
         ENDIF
         CALL BUKADD(ROW,COUNT)
800   CONTINUE
C
C                    Summary of BASET outcome
C                    ~~~~~~~~~~~~~~~~~~~~~~~~
C  1. The number of basic variables is verified to = number of rows.
C  2. All logical basics are assigned to the rear.  This is done by
C     setting status < 0 (as in submatrix setting for other functions)
C     and setting INDEX( IPIVOT+ROW ) = -ROW...see GETBVP, which
C     interprets this as logical pivot.
C  3. The number of structurals remaining to be assigned is NBPOSN (passed
C     in common...see DCGETMAT), and two lists of indexes, each of length
C     NBPOSN, are initialized:
C       INDEX( BCPOSN+i ) = column in i-th position (initially arbitrary)
C       INDEX( BCSTAT+i ) = associated count/status
C        ...while this column is unassigned (ie, FRONT <= i <= REAR),
C           this is the count;  once assigned, this is status:
C              status > 0 means the index is the pivot row,
C                         and the column is nonspike;
C              status < 0 means the index is the position at which the
C                         column was forced to be spike (its earliest
C                         insertion position).
C
C  4. Buckets are setup, where INDEX(BUCKET+b) points to row with
C     count = b (else, 0).  Rows with count b (<= NBUCKT) are linked,
C     where INDEX(BLSUCC+i) = successor link of row i and
C           INDEX(BLPRED+i) = predecessor link or row i.
C
      IF( SWDBG )THEN
         PRINT *,' END OF BASET'
         CALL GDEBUG
      ENDIF
      RETURN
C
C ALL OVERFLOW EXITS COME HERE
1300  PRINT *,' Sorry, not enough INDEX space to setup basis',
     1        '...need at least',IFREE
C ALL ABORTIONS COME HERE
1390  IFREE = IPIVOT
      IF( SWDBG )CALL GDEBUG
      RETURN 1
C
C ** BASET ENDS HERE
      END
      SUBROUTINE BASTAT(NFTRI,NBTRI,NLGL,NKRNL,NSPIK,NZORG,NFILL,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This gets basis statistics.
C
C    NFTRI = Size of Forward Triangle
C    NBTRI = Size of Backward Triangle, except logicals
C    NLGL  = Number of basic logicals
C    NKRNL = Size of Kernel
C    NSPIK = Number of spikes in kernel
C    NZORG = Number of original nonzeroes in basis factors
C    NFILL = Fill-in = Number of new nonzeroes in basis factors
C
C ...RCODE = 0 IFF ALL IS WELL
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
      IF( IPIVOT.LE.0 )THEN
         PRINT *,' ** SYSERR IN BASTAT...',IPIVOT
         RCODE = 13
         RETURN
      ENDIF
C COPY STATS FOR CALLER
      NFTRI = NBFTRI
      NBTRI = NBBTRI
      NLGL  = NBLGL
      NKRNL = NBKRNL
      NSPIK = NBSPIK
      NZORG = NBASNZ
      NFILL = NBFILL
      RCODE = -PIVNUM
      RETURN
C
C ** BASTAT ENDS HERE
      END
      SUBROUTINE GSPIKE(WHAT,SWADD,NUMBER)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This adds|deletes spikes|nonspikes to|from submatrix.
C     SWADD = TRUE for Add;  FALSE for Delete
C     WHAT := SPIKE | NONSPIKE | FTRI | BTRI | KERNEL
C                                :..................:...Added 10-3-92
C Returned:  NUMBER = Number added or deleted.
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C IT IS PRESUMED CALLER HAS CHECKED THAT BASIS IS SETUP.
C
      CHARACTER*(*) WHAT
      LOGICAL*1     SWADD
C LOCAL
      LOGICAL*1     SW
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
C INITIALIZE
      NUMBER = 0
      POSN   = 0
C        :...ALPHA NUMBER TO KNOW WHAT SEGMENT WE'RE IN
      BBTRI1 = NBFTRI + NBKRNL + 1
C        :...FIRST COL IN BTRI = NUMBER IN FTRI + NUMBER IN KERNEL + 1
C
C  ::: LOOP OVER ALPHA-FILE (FORWARD DIRECTION) :::
      I = 1
100   CONTINUE
      IF( I.GT.NBLST )RETURN
         ROW = INDEX(IRBLST + I)
         IF( ROW.GT.0 )GOTO 190
C BEGIN NEW ALPHA
         POSN = POSN+1
         IF( DABS( ZVALUE(IZBLST+I) ) .LE. 0. )THEN
C ALPHA IS NONSPIKE...NEXT RECORD POINTS TO ORIGINAL COLUMN
            I = I+1
            IF( WHAT.EQ.'SPIKE' )GOTO 190
            IF( WHAT.EQ.'BTRI'   .AND. POSN.LT.BBTRI1 )GOTO 190
            IF( WHAT.EQ.'FTRI'   .AND. POSN.GT.NBFTRI )RETURN
            IF( WHAT.EQ.'KERNEL' .AND. POSN.LE.NBFTRI )GOTO 190
            IF( WHAT.EQ.'KERNEL' .AND. POSN.GE.BBTRI1 )RETURN
C AT THIS POINT, WHAT = NONSPIKE | FTRI | KERNEL | BTRI, AND
C  IF WHAT = FTRI,   WE ARE IN FTRI   SEGMENT (POSN <= NBFTRI)
C  IF WHAT = BTRI,   WE ARE IN BTRI   SEGMENT (POSN >= BBTRI1)
C  IF WHAT = KERNEL, WE ARE IN KERNEL SEGMENT (NBFTRI < POSN < BBTRI1)
            COL = -INDEX(IRBLST + I)
C COL WILL BE PROCESSED (ADDED OR DELETED) WHEN WE FALL THRU
         ELSE IF( WHAT.EQ.'SPIKE' .OR. WHAT.EQ.'KERNEL' )THEN
C NEW ALPHA IS SPIKE (AND MATCHES WHAT)...GET COL FROM PIVOT ROW
            COL = INDEX(IPIVOT - ROW)
         ELSE
C DO NOT PROCESS
            GOTO 190
         ENDIF
C
C AT THIS POINT COL IS IN BASIS WITH TYPE MATCHING WHAT
C ================== PROCESS COL ======================
         CALL GETMAP('COL ',COL,SW)
C SW = SWADD MEANS NO OPERATION NEEDED
C    (COL IS ALEADY IN OR OUT OF SUBMATRIX, AS SUPPOSED TO BE)
C SW <> SWADD MEANS COL MUST BE ADDED (SWADD=T) OR DELETED (SWADD=F)
         SW = (SW.AND.SWADD ).OR. .NOT.(SW.OR.SWADD)
         IF( .NOT.SW )THEN
            CALL GPUTMP('COL ',COL,SWADD)
            NUMBER = NUMBER+1
         ENDIF
C BOTTOM OF LOOP
190   CONTINUE
      I = I+1
      GOTO 100
C
C ** GSPIKE ENDS HERE
      END
      SUBROUTINE BASPIK(ALPHA,NALPHA,COL,VPIVOT,POSN,ROW,VPVAL,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This attempts to insert spike COL into alpha region at POSN.
C      VPIVOT = Pivot tolerance.
C Normal return means we got pivot ROW (spike stored in alpha region).
C      ROW   = 0 means no pivot row found (not added to alpha region),
C                in which case POSN might increase to indicate a new
C                earliest insertion.
C      VPVAL = Pivot value (ROW > 0).
C ...Alternate return means not enough space (fatal).
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
      DOUBLE PRECISION ALPHA(NALPHA)
C
C CALLS  GALPHA, GFTRAN, BSPACE
C BASIS POINTERS AND STATS USED (FROM BASE IN DCGETMAT):
C       IRBLST = INDEX BASE FOR ALPHA REGION
C       IVBLST = VALUE BASE FOR ALPHA REGION (FOR SINGLE PRECISION)
C       IZBLST = VALUE BASE FOR ALPHA REGION (FOR DOUBLE PRECISION)
C       NBLST  = LENGTH OF ALPHA REGION
C       NBFILL = FILL-IN
C NBFILL AND NBLST ARE UPDATED (IF SUCCESSFUL)
C CALLER IS RESPONSIBLE FOR OTHER UPDATES (EG, REAR)
C
      PARAMETER (ABIT=5)
C                :...WAIT TIME FOR NEXT ATTEMPT
C                    (MUST BE AT LEAST 1...SEE BELOW)
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
C FTRAN COL
      CALL GALPHA('COL ',COL,ALPHA,NALPHA)
      CALL GFTRAN(ALPHA,NALPHA)
C  NOW -ALPHA IS OUR VECTOR TO BE STORED
      IR = IRBLST + NBLST
      IZ = IZBLST + NBLST
      NZADD = 0
      IMAX  = 0
      VMAX  = 0.
      IPOSN = NROWS
      RPOSN = 0
C CHECK SPACE FOR WORST CASE (100% DENSE ALPHA)
      CALL BSPACE(NROWS,*1300)
C
C STORE WHILE LOOKING FOR UNASSIGNED ROW WITH LARGEST VALUE
      DO 400 ROW=1,NROWS
         ZALPHA = -ALPHA(ROW)
         VABS = DABS(ZALPHA)
         IF( VABS.LE.VPIVOT ) GOTO 400
C ALPHA-VALUE IS ACCEPTABLE...ADD TO ALPHA REGION
         NZADD = NZADD + 1
         INDEX (IR + NZADD) = ROW
         ZVALUE(IZ + NZADD) = ZALPHA
         IF( INDEX( IRSTAT+ROW ).GT.0 .AND. VABS.GT.VMAX )THEN
C ROW IS UNASSIGNED AND HAS GREATER PIVOT VALUE
            ROWPSN = -INDEX( IPIVOT+ROW )
            IF( ROWPSN.GT.0 )THEN
C ...SEE IF ROW CAN BE INSERTED AT THIS POSITION
               IF( POSN.GE.ROWPSN )THEN
C ...IT CAN
                  IMAX = NZADD
                  VMAX = VABS
               ELSE IF( ROWPSN.LT.IPOSN )THEN
C ...IT CANNOT, AND THIS IS THE EARLIEST AMONG ROW CANDIDATES
                  IPOSN = ROWPSN
                  RPOSN = ROW
               ENDIF
            ENDIF
         ENDIF
400   CONTINUE
C
      IF( IMAX.EQ.0 )THEN
C NO PIVOT ROW FOUND THAT COULD BE INSERTED AT POSN
         ROW = 0
         IF( RPOSN.GT.0 )THEN
C THIS MEANS WE FOUND A PIVOT, BUT ITS ROW (RPOSN) COULD NOT BE
C INSERTED YET
             POSN = IPOSN
         ELSE
C THIS MEANS WE DID NOT FIND A PIVOT...TELL CALLER TO WAIT A BIT
             POSN = POSN + ABIT
         ENDIF
         RETURN
      ENDIF
C
C WE HAVE A PIVOT ROW
      ROW    = INDEX (IR+IMAX)
      ZPIVOT = ZVALUE(IZ+IMAX)
      VPVAL  = ZPIVOT
C PUT 1ST WITH SIGN TAG (SWITCH PIVOT WITH 1ST)
      INDEX (IR+IMAX) = INDEX (IR+1)
      ZVALUE(IZ+IMAX) = ZVALUE(IZ+1)
      INDEX (IR+1) = -ROW
      ZVALUE(IZ+1) = ZPIVOT
C PUT ROW STAT (ASSIGNED)
      INDEX( IRSTAT+ROW ) = -INDEX( IRSTAT+ROW )
C ...AND COL PIVOT ON ROW
      INDEX( IPIVOT+ROW ) = COL
C UPDATE FILL-IN (NBFILL)
      NZ = BASE(ICINFO + COL) - BASE(ICINFO + COL-1)
      NBFILL = NBFILL + NZADD - NZ
C UPDATE LENGTH OF BASIS ALPHA REGION (NBLST)
      NBLST  = NBLST  + NZADD
C
      RETURN
C
C FATAL ERROR
1300  RETURN 1
C
C ** BASPIK ENDS HERE
      END
      SUBROUTINE BSUPD8(ROW,COL,VPVAL,
C     =================
     1                  VPMIN,SPIKIN,REMAIN,FREQCH,FREQMS)
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This updates stats after spike COL insertion into alpha region,
C pivoting on ROW with pivot value = VPVAL.
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C       VPMIN  = Min abs pivot value
C       SPIKIN = Number of spikes inserted so far
C       REMAIN = Number of columns remaining to be entered
C       FREQCH = Message frequency check
C       FREQMS = Message frequency (user set)
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
      SPIKIN = SPIKIN+1
      IF( ABS( VPVAL ) .LT. VPMIN )VPMIN = ABS( VPVAL )
      REMAIN = REMAIN - 1
      FREQCH = FREQCH - 1
      IF( FREQCH.LE.0 )THEN
         PRINT *,' Column ',NAME(ICNAME+COL),
     1           ' (spike) pivoting on row ',NAME(IRNAME+ROW)
         PRINT *,REMAIN,' columns remain...'//
     1                  'Fill-in so far =',NBFILL
         FREQCH = FREQMS
         IF( SWDBG )THEN
            PRINT *,' VPMIN =',VPMIN,' SPIKIN =',SPIKIN
            PAUSE = PAUSE-3
            IF(PAUSE.LE.0)CALL GDEBUG
         ENDIF
      ENDIF
      RETURN
C
C ** BSUPD8 ENDS HERE
      END
