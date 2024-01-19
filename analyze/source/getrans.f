C             ::: GETRANS.FOR  8-20-95 :::
C
C Earlier dates deleted
C    2-12-95...Added GVERFY
C    5-20-95...Modified GVERFY
C    5-25-95...Added base check to GVERFY (Caller must check)
C              Put rows and cols with discrepancies in submat.
C    7-30-95...Replaced some infeas tests in GBREFR and GVERFY (CC 8-2)
C              Moved GVERFY to GETIOPCK (8-8-95) ===> GETBRVER
C    8-09-95...Moved GBREFR and GBRDIF to GETBRVER
C    8-09-95...Moved GSETST here
C
C This contains the following GETMAT subroutines.
C
C     GFTRAN...forward transform to solve Bx = ALPHA
C     GBTRAN...backward transform to solve pB = ALPHA
C     GALPHA...form ALPHA vector for row/col
C     GBALPH...add alpha vector at end of alpha region
C                (supports FIX in BASIS CHECK)
C     GSETST...sets solution status
C
      SUBROUTINE GFTRAN(ALPHA,NALPHA)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This solves Bx = ALPHA
C
      DOUBLE PRECISION ALPHA(NALPHA)
      PARAMETER (ZERO=1.0D-18)
C                :...Special absolute tolerance for alpha purification
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
C NOTE: ALPHA DOES NOT CHANGE IF BASIS IS IDENTITY SINCE THEN NBLST=0
C
C  ::: LOOP OVER ALPHA-FILE (FORWARD DIRECTION) :::
      I = 1
100   CONTINUE
      IF( I.GT.NBLST )RETURN
         ROW = INDEX(IRBLST + I)
         IF( ROW.GT.0 )THEN
C           ==========
C CONTINUATION OF SPIKE ALPHA
            ALPHA(ROW) = ALPHA(ROW) - ZVALUE(IZBLST+I)*ZPIVOT
C PURIFY ALPHA
            IF( DABS( ALPHA(ROW) ).LE.ZERO ) ALPHA(ROW) = 0.
         ELSE IF( ROW.LT.0 )THEN
C                ==========
C BEGIN NEW ALPHA
            ROW = -ROW
            ZPIVOT = ZVALUE(IZBLST+I)
            IF( DABS( ZPIVOT ).LE.ZERO )THEN
C ALPHA IS NONSPIKE...NEXT RECORD GIVES PIVOT VALUE AND POINTS
C                      TO ORIGINAL COLUMN
               I = I+1
               COL    = -INDEX(IRBLST + I)
               ZPIVOT = ALPHA(ROW) / ZVALUE(IZBLST+I)
               ALPHA(ROW) = ZPIVOT
C PIVOT SET...LOOP OVER OTHER NON-ZEROES OF COL TO COMPLETE FTRAN STEP
               I1 = INONZ + 2*BASE(ICINFO + COL-1)
               NZ = BASE(ICINFO + COL) - BASE(ICINFO + COL-1)
               I2 = I1 + 2*NZ - 1
C        LOOP OVER COL'S ORIGINAL NONZEROES
               DO 50 INZ=I1,I2,2
                  R = INDEX(INZ)
                  IF( R.EQ.ROW )GOTO 50
C    ROW (R) IS NOT THE PIVOT ROW, SO PERFORM TRANSFORMATION
                  CALL GETVAL(V,INZ+1)
                  ZALPHA = -V
C                          :...ALPHA = -COEF
                  ALPHA(R) = ALPHA(R) - ZALPHA*ZPIVOT
C    PURIFY ALPHA (SET = 0 IF IT'S NEARLY 0)
                  IF( DABS( ALPHA(R) ).LE.ZERO ) ALPHA(R) = 0.
50             CONTINUE
            ELSE
C NEW ALPHA IS SPIKE...OPERATE IN-LINE
               ZPIVOT = ALPHA(ROW) / ZPIVOT
               ALPHA(ROW) = ZPIVOT
            ENDIF
         ENDIF
C BOTTOM OF LOOP (NEXT ALPHA)
190   CONTINUE
      I = I+1
      GOTO 100
C
C ** GFTRAN ENDS HERE
      END
      SUBROUTINE GBTRAN(ALPHA,NALPHA)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This solves yB = ALPHA
C
      DOUBLE PRECISION ALPHA(NALPHA)
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
C NOTE: ALPHA DOES NOT CHANGE IF BASIS IS IDENTITY SINCE THEN NBLST=0
C
      ZSUM = 0.0
C
C  ::: LOOP OVER ALPHA-FILE (FROM BACK TO FRONT) :::
      I = NBLST
100   CONTINUE
      IF( I.LE.0 )RETURN
         ROW = INDEX(IRBLST + I)
         IF( ROW.LT.0 )THEN
C          ==========
C WE MUST LOOK AT NEXT VALUE TO SEE IF THIS IS A NONSPIKE
            IF( DABS( ZVALUE(IZBLST + I-1) ).LE.0. )THEN
C ...IT IS:       INDEX   ZVALUE
C                  -Row    0              <---- I-1
C                  -Col    Pivot Value    <---- I  (ROW = -Col)
C
               COL = -ROW
               ZPIVOT = ZVALUE(IZBLST+I)
               I = I-1
               ROW = -INDEX(IRBLST + I)
C  COL PIVOTS ON ROW WITH PIVOT VALUE = ZPIVOT
               I1 = INONZ + 2*BASE(ICINFO + COL-1)
               NZ = BASE(ICINFO + COL) - BASE(ICINFO + COL-1)
               I2 = I1 + 2*NZ - 1
C       LOOP OVER COL'S ORIGINAL NONZEROES FOR BRTAN STEP
               DO 50 INZ=I1,I2,2
                  R = INDEX(INZ)
                  IF( R.EQ.ROW )GOTO 50
C    ROW (R) IS NOT THE PIVOT ROW, SO PERFORM TRANSFORMATION
                  CALL GETVAL(V,INZ+1)
                  ZALPHA = -V
C                          :...= -STORED ALPHA
                  ZSUM = ZSUM + ZALPHA*ALPHA(R)
50             CONTINUE
C NOW THE LAST STEP OF BTRAN FOR THIS NONSPIKE
               ALPHA(ROW) = ( ALPHA(ROW) - ZSUM ) / ZPIVOT
            ELSE
C ...WE ARE AT A SPIKE, SO THIS IS THE PIVOT ENTRY
C   (1ST RECORD OF ALPHA VECTOR)
C ...PROCEED WITH NORMAL LOGIC (IN-LINE) TO COMPLETE BTRAN STEP
               ROW = -ROW
               ALPHA(ROW) = ( ALPHA(ROW) - ZSUM ) / ZVALUE(IZBLST+I)
            ENDIF
C IN EITHER CASE, INITIALIZE ZSUM FOR NEXT BTRAN STEP
            ZSUM = 0.0
         ELSE IF( ROW.GT.0 )THEN
C                ==========
C PERFORM SUMMATION FOR IN-LINE SPIKE VALUE (NOT PIVOT)
            ZSUM  = ZSUM + ZVALUE(IZBLST+I)*ALPHA(ROW)
         ENDIF
C BOTTOM OF LOOP
190   CONTINUE
      I = I-1
      GOTO 100
C
C ** GBTRAN ENDS HERE
      END
      SUBROUTINE GALPHA(ROWCOL,NUMBER,ALPHA,NALPHA)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This initializes ALPHA for row/col NUMBER
C
      CHARACTER*4      ROWCOL
      DOUBLE PRECISION ALPHA(NALPHA)
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C INITIALIZE ALPHA=0
      DO 10 I=1,NROWS
10    ALPHA(I) = 0.0
C
      IF(ROWCOL.EQ.'ROW ')THEN
         ALPHA(NUMBER) = 1.0
         RETURN
      ENDIF
C
C UNPACK COLUMN'S NONZEROES
C
      I1 = INONZ + 2*BASE(ICINFO + NUMBER-1)
      I2 = INONZ + 2*BASE(ICINFO + NUMBER) - 1
C
      IF(I2.LE.I1)RETURN
      DO 100 I=I1,I2,2
         ROW = INDEX(I)
         CALL GETVAL(V,I+1)
         ALPHA(ROW) = V
100   CONTINUE
C
      RETURN
C
C ** GALPHA ENDS HERE
      END
      SUBROUTINE GBALPH(ALPHA,NALPHA,PIVROW,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This adds ALPHA to end of alpha region (for FIX).
C    PIVROW = pivot row
C ...RCODE = 0 iff all is well;  else, not enough space or bad pivot.
C
      DOUBLE PRECISION ALPHA(NALPHA)
      PARAMETER (ZERO=1.0D-18)
C                :...Special absolute tolerance for alpha purification
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      CALL BSPACE(NROWS,*1300)
      IF( ALPHA(PIVROW).LE.ZERO ) GOTO 1300
      NBLST = NBLST + 1
      INDEX ( IRBLST+NBLST ) = -PIVROW
      ZVALUE( IZBLST+NBLST ) = ALPHA(PIVROW)
C
      DO 100 I=1,NROWS
         IF( I.EQ.PIVROW .OR. DABS( ALPHA(I) ) .LE. ZERO )GOTO 100
         NBLST = NBLST + 1
         INDEX ( IRBLST+NBLST ) = I
         ZVALUE( IZBLST+NBLST ) = ALPHA(I)
100   CONTINUE
C
      RETURN
C
1300  CONTINUE
      RCODE = 1
      RETURN
C
C ** GBALPH ENDS HERE
      END
      SUBROUTINE GSETST(VAPINF,VAXDIF,ZA,NZA, STATUS,ROWCOL,NUMBER,VP)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This sets solution STATUS (SOLST unchanged).
C If STATUS='FEASIBLE' upon entrance, this tests for unboundedness.
C
      DOUBLE PRECISION ZA(NZA)
      CHARACTER*16     STATUS
      CHARACTER*4      ROWCOL
C Input:
C    VAPINF = Absolute tolerance for dual-infeasible price
C    VAXDIF = Absolute tolerance for primal level
C    ZA used for FTRAN (if necessary)...NZA MUST BE >= NROWS
C Output:
C    STATUS = solution status (or * if syserr)
C    ROWCOL NUMBER = ROW|COL number if STATUS = UNBOUNDED
C LOCAL
      CHARACTER*1 STAT
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF( NZA.LT.NROWS )THEN
         PRINT *,' ** SYSERR IN GSETST...',NZA,' < ',NROWS
         STATUS = '*'
         RETURN
      ENDIF
      IF( STATUS.EQ.'FEASIBLE' )GOTO 500
      STATUS = 'OPTIMAL '
      DO 100 I=1,NROWS
         CALL GETSOL('ROW ',I,VX,VP,STAT,STNUM)
         IF( STAT.EQ.'I' )THEN
            STATUS = 'INFEASIBLE'
            RETURN
         ENDIF
         IF( (STAT.EQ.'L' .AND. -OPT*VP.LT.-VAPINF) .OR.
     1       (STAT.EQ.'U' .AND. -OPT*VP.GT. VAPINF) )
     2      STATUS = 'FEASIBLE'
100   CONTINUE
C
      DO 200 I=1,NCOLS
         CALL GETSOL('COL ',I,VX,VP,STAT,STNUM)
         IF( STAT.EQ.'I' )THEN
            STATUS = 'INFEASIBLE'
            RETURN
         ENDIF
         IF( (STAT.EQ.'L' .AND. -OPT*VP.LT.-VAPINF) .OR.
     1       (STAT.EQ.'U' .AND. -OPT*VP.GT. VAPINF) )
     2      STATUS = 'FEASIBLE'
200   CONTINUE
C
      IF( STATUS.EQ.'OPTIMAL' )RETURN
C
500   CONTINUE
C ===== STATUS = FEASIBLE =====
      IF( IPIVOT.LE.0 )RETURN
C BASIS IS SETUP...CHECK IF UNBOUNDED
      DO 1000 I=1,NROWS
         CALL GETSOL('ROW ',I,VX,VP,STAT,STNUM)
         IF( STAT.EQ.'B' )GOTO 1000
         IF( (STAT.EQ.'L' .AND. -OPT*VP.GE. VAPINF) .OR.
     1       (STAT.EQ.'U' .AND. -OPT*VP.LE.-VAPINF) )GOTO 1000
C ROW I HAS NON-OPTIMAL PRICE
         CALL GETBND('ROW ',I,VL,VU)
         IF( (STAT.EQ.'L' .AND. VU.LT. VINF) .OR.
     1       (STAT.EQ.'U' .AND. VL.GT.-VINF) )GOTO 1000
C ...ROW DOES NOT BLOCK ITSELF
C GET RATES TO SEE IF THERE IS BLOCKAGE
         CALL GALPHA('ROW ',I,ZA,NZA)
         CALL GFTRAN(ZA,NZA)
C
         DO 800 ROW=1,NROWS
            VRATE = ZA(ROW)
            IF( ABS(VRATE).LE.VAXDIF )GOTO 800
            CALL GETBVP(ROW,ROWCOL,NUMBER)
            CALL GETBND(ROWCOL,NUMBER,VL,VU)
            IF( (VRATE.GT.0. .AND. VU.LT. VINF) .OR.
     1          (VRATE.LT.0. .AND. VL.GT.-VINF) )GOTO 1000
800      CONTINUE
C -NO BLOCKAGE...
         STATUS = 'UNBOUNDED'
         ROWCOL = 'ROW '
         NUMBER = I
         RETURN
1000  CONTINUE
C IF WE GET HERE, THIS MEANS ROWS WITH NON-OPTIMAL PRICE ARE BLOCKED
C
      DO 1500 I=1,NCOLS
         CALL GETSOL('COL ',I,VX,VP,STAT,STNUM)
         IF( STAT.EQ.'B' )GOTO 1500
         IF( (STAT.EQ.'L' .AND. -OPT*VP.GE. VAPINF) .OR.
     1       (STAT.EQ.'U' .AND. -OPT*VP.LE.-VAPINF) )GOTO 1500
C COL I HAS NON-OPTIMAL PRICE
         CALL GETBND('COL ',I,VL,VU)
         IF( (STAT.EQ.'L' .AND. VU.LT. VINF) .OR.
     1       (STAT.EQ.'U' .AND. VL.GT.-VINF) )GOTO 1500
C ...COL DOES NOT BLOCK ITSELF
C GET RATES TO SEE IF THERE IS BLOCKAGE
         CALL GALPHA('COL ',I,ZA,NZA)
         CALL GFTRAN(ZA,NZA)
C
         DO 1400 ROW=1,NROWS
            VRATE = ZA(ROW)
            IF( ABS(VRATE).LE.VAXDIF )GOTO 1400
            CALL GETBVP(ROW,ROWCOL,NUMBER)
            CALL GETBND(ROWCOL,NUMBER,VL,VU)
            IF( (VRATE.GT.0. .AND. VU.LT. VINF) .OR.
     1          (VRATE.LT.0. .AND. VL.GT.-VINF) )GOTO 1500
1400     CONTINUE
C -NO BLOCKAGE...
         STATUS = 'UNBOUNDED'
         ROWCOL = 'COL '
         NUMBER = I
         RETURN
1500  CONTINUE
C
      RETURN
C
C ** GSETST ENDS HERE
      END
