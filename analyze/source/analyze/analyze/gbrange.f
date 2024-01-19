C                  ::: GBRANGE.FOR  8-22-95 :::
C
C Earlier dates deleted
C    6-24-94...Fixed bug in GPIVOT
C    8-30-94...Moved BASCLR to GETSET
C    7-29-95...Added number and tolerances to calling args of GPIVOT
C    8-15-95...Declared TOLAXI...in GPIVOT
C              Rearranged some things in GBUNDO and GBPHST
C
C This contains routines for redundancy testing.
C     GBARNG...computes ranges of basic variables
C     GBRNGV...gives range and identity of basic variable
C              (used only after GBARNG is done)
C     GPIVOT...pivots to new basis
C     GBUNDO...undo pivot(s)
C     GBPHST...gives pivot history
C     GPVST0...puts|sets solution status before pivot|after undo
C
      SUBROUTINE GBARNG(RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This computes ranges of basic variables, allowing nonbasics to
C be at either of their bound values.
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
      COMMON/IBRNGV/IMIN,IMAX
C LOCAL
      CHARACTER*1  CHAR
      CHARACTER*4  ROWCOL
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C SETUP MEMORY
      IV0   = IVFREE
      IYL   = IVFREE
      IYU   = IYL + NROWS
      IVFREE= IYU + NROWS + 1
      IZFREE= IVFREE/2 + 1
      IMIN  = IZFREE
      IMAX  = IZFREE + NROWS
      IALPHA= IMAX + NROWS
      IVFREE= 2*(IALPHA + NROWS) + 1
      IF(IVFREE.GT.MAXVAL)THEN
         PRINT *,' Sorry, not enough value space...need',IVFREE
         RCODE = 1
         GOTO 9000
      ENDIF
      NALPHA = NROWS
C
C NOTE:  IMIN, IMAX, IALPHA POINTERS ARE FOR DOUBLE PRECISION
C     ZVALUE(IMIN+i) = MIN VALUE OF BASIC VARIABLE i
C     ZVALUE(IMAX+i) = MAX VALUE OF BASIC VARIABLE i
C     ZVALUE(IALPHA+1,...) = VECTOR FOR FTRAN OF NONBASIC COLUMN
C ==============================================================
C  INITIAL PASS TO REDUCE INFINITE BOUNDS OF NONBASICS
C   (USING ORIGINAL MATRIX)
C
C USE ROW BOUND SPACE FOR ACCUMULATION
      DO 10 I=1,NROWS
         VALUE(IYL+I)=0.
         VALUE(IYU+I)=0.
10    CONTINUE
C
      I1 = INONZ
C  ::: LOOP OVER COLUMNS TO ACCUMULATE ROW RANGES :::
      DO 50 J=1,NCOLS
         NZ = BASE(ICINFO + J) - BASE(ICINFO + J-1)
         IF(NZ.EQ.0)GOTO 50
         IF(INDEX(ICSTAT+J).GT.0)GOTO 40
C COLUMN (J) IS IN SUBMATRIX AND NOT NULL
         I2 = I1 + 2*NZ - 1
         CALL GETBND('COL ',J,VLO,VUP)
C
         DO 25 I=I1,I2,2
            ROW = INDEX(I)
            CALL GETVAL(V,I+1)
            IF(V.LT.0.)THEN
C SWITCH BOUNDS
               VL = -VUP
               VU = -VLO
               V = -V
            ELSE
               VL = VLO
               VU = VUP
            ENDIF
C NOW V > 0 AND V*[VL,VU] = RANGE OF TERM FOR ROW
            IF(VL.GT.-VINF)THEN
               VALUE(IYL+ROW) = VALUE(IYL+ROW) + VL*V
            ELSE
               VALUE(IYL+ROW) = -VINF
            ENDIF
            IF(VU.LT. VINF)THEN
               VALUE(IYU+ROW) = VALUE(IYU+ROW) + VU*V
            ELSE
               VALUE(IYU+ROW) =  VINF
            ENDIF
25       CONTINUE
C ADVANCE NONZERO POINTER
40       I1 = I2+1
C NEXT COLUMN
50    CONTINUE
C   ::: END LOOP OVER COLUMNS :::
C
C LOOP OVER ROWS TO PUT STRONGER OF ORIGINAL AND INFERRED BOUNDS
      DO 75 I=1,NROWS
         CALL GETBND('ROW ',I,VL,VU)
         IF(VL.GT.VALUE(IYL+I)) VALUE(IYL+I)=VL
         IF(VU.LT.VALUE(IYU+I)) VALUE(IYU+I)=VU
75    CONTINUE
C
C NOW VALUE(IYL+i) HAS THE STRONGER OF THE ORIGINAL AND REDUCED BOUND
C     VALUE(IYU+i) "
C
C   ============= END INITIAL BOUND REDUCTION ==============
C
C INITIALIZE ACCUMULATION
      DO 100 I=IMIN+1,IMAX+NROWS
100   ZVALUE(I)=0.
C
C LOOP OVER NONBASIC VARIABLES TO ACCUMULATE RANGES
C
C FIRST, THE ROWS
      ROWCOL = 'ROW '
      ISTAT  = IRSTAT
      NUMBER = NROWS
      ZSIGN  = -1.
C
200   CONTINUE
C
C   ::: MAIN LOOP OVER ROWS/COLUMNS TO ACCUMULATE RANGES :::
C       ...SKIPPING THE BASICS AND THOSE NOT IN SUBMATRIX
      DO 500 K=1,NUMBER
         STAT = INDEX(ISTAT+K)
         IF(STAT.GT.0)GOTO 500
C ...ROW/COL IS IN SUBMATRIX...LOOK AT STATUS
         CHAR = CHARST(-STAT)
         IF(CHAR.NE.'L'.AND.CHAR.NE.'U')GOTO 500
C STATUS IS L OR U (IE, NONBASIC)
C ...FTRAN ITS COLUMN
         CALL GALPHA(ROWCOL,K,ZVALUE(IALPHA+1),NALPHA)
         CALL GFTRAN(ZVALUE(IALPHA+1),NALPHA)
C ...GET ITS BOUNDS
         IF(ROWCOL.EQ.'ROW ')THEN
C USE REDUCED BOUNDS FOR NONBASIC ROW
            VLO = VALUE(IYL+K)
            VUP = VALUE(IYU+K)
         ELSE
C USE ORIGINAL BOUNDS FOR NONBASIC COLUMN
            CALL GETBND(ROWCOL,K,VLO,VUP)
         ENDIF
C
C LOOP OVER ALPHA'S TO ACCUMULATE RANGE
         DO 300 I=1,NROWS
            V = ZSIGN*ZVALUE(IALPHA+I)
C               :....NEGATIVE SIGN FOR ROW NONBASIC
C
C NOW FOR THE EQUATONS:  u(i)=M(i,.)w
C                u(i) = this basic level
C                w    = this nonbasic variable
C              M(i,*) = rate of substitution (V)
C
C THE EXTREMES ARE FROM w BEING AT ITS APPROPRIATE BOUND,
C DEPENDING UPON THE SIGN OF THE RATE (V)
C
            IF(V.LE.-VTOLAB)THEN
C RATE < 0...SWITCH BOUNDS
               VL = -VUP
               VU = -VLO
               V  = -V
            ELSE IF(V.GE.VTOLAB)THEN
C RATE > 0
               VL = VLO
               VU = VUP
            ELSE
C RATE = 0
               GOTO 300
            ENDIF
C  ...NOW V = ABS(RATE) AND RANGE OF NONBASIC TERM IS (V*VL,V*VU)
            IF(VL.GT.-VINF)THEN
               ZVALUE(IMIN+I) = ZVALUE(IMIN+I) + V*VL
            ELSE
               ZVALUE(IMIN+I) = -VINF
            ENDIF
            IF(VU.LT. VINF)THEN
               ZVALUE(IMAX+I) = ZVALUE(IMAX+I) + V*VU
            ELSE
               ZVALUE(IMAX+I) = VINF
            ENDIF
300      CONTINUE
C      ::: END LOOP OVER ALPHA :::
C NEXT NONBASIC
500   CONTINUE
C  ::: END LOOP OVER NONBASIC ROW/COLUMNS IN SUBMATRIX :::
C
      IF(ROWCOL.EQ.'COL ')GOTO 9000
C NOW THE COLUMNS
        ROWCOL = 'COL '
        ISTAT  = ICSTAT
        NUMBER = NCOLS
        ZSIGN  = 1.
      GOTO 200
C
9000  CONTINUE
C
C RESTORE FREE VALUE SPACE (IVFREE)
      IVFREE = IV0
      RETURN
C
C ** GBARNG ENDS HERE
      END
      SUBROUTINE GBRNGV(I,ROWCOL,NUMBER,VMIN,VMAX)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
C This gives the identity (ROWCOL NUMBER) of the I-th basic variable
C and its range (VMIN, VMAX) computed from GBARNG.
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C NOTE:  THIS MUST BE CALLED JUST AFTER GBARNG (AT LEAST BEFORE
C        ANY OTHER ROUTINE THAT USES FREE VALUE SPACE) BECAUSE
C        THE POINTER TO FREE VALUE SPACE (IVFREE) HAS BEEN RESTORED,
C        SO THE RANGES WILL BE LOST.
C
      CHARACTER*4 ROWCOL
      COMMON/IBRNGV/IMIN,IMAX
C :::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
C GET PIVOT ON ROW (I)
      CALL GETBVP(I,ROWCOL,NUMBER)
      VMIN = ZVALUE(IMIN+I)
      VMAX = ZVALUE(IMAX+I)
      RETURN
C
C ** GBRNGV ENDS HERE
      END
      SUBROUTINE GPIVOT(RCIN,NUMIN,RCOUT,NUMOUT,ALPHA,NALPHA,
     1                  TOLPIV,TOL0,TOLAXI,TOLRXI,NPIVOT, *)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This pivots nonbasic row|col (RCIN NUMIN) into basis, in exchange
C for basic row|col (RCOUT NUMOUT).
C If NUMOUT = 0 (doesn't matter what RCOUT is), caller wants
C automatic selection: 1st basic driven to a bound.  If none, NUMOUT=0
C upon return (pivot not performed, but no error message is given here);
C otherwise, (RCOUT NUMOUT) is set here for caller.
C
C CALLER IS RESPONSIBLE FOR INTEGRITY OF IN AND OUT VARS.
C CALLER MUST REFRESH DUAL PRICES (LEVELS SHOULD BE OK).
C ===============================
C SOLUTION STATUS (SOLST) COULD CHANGE BETWEEN FEASIBLE AND INFEASIBLE.
C Other args:
C   ALPHA  = array to perform FTRAN (double precision)
C   NALPHA = length of ALPHA...must be >= NROWS
C   TOLPIV = tolerance for pivot element to be acceptable
C   TOL0   = tolerance to add ALPHA as a nonzero
C   TOLAXI = absolute tolerance for X to be infeasible
C   TOLRXI = relative tolerance for X to be infeasible
C   NPIVOT = number of pivots (from history)
C ...Alternate return is fatal error (pivot not performed).
C
      CHARACTER*(*) RCIN, RCOUT
      INTEGER       NUMIN,NUMOUT,NALPHA,NPIVOT
      DOUBLE PRECISION ALPHA(NALPHA)
      REAL          TOLPIV,TOL0,TOLAXI,TOLRXI
C LOCAL
      CHARACTER*1   STAT,STATIN,STATOT,STOLD,STNEW
      CHARACTER*16  CNAME
      CHARACTER*4   ROWCOL
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
      IF( PIVNUM.GE.PIVMAX )THEN
         PRINT *,' ** MAX NUMBER OF PIVOTS REACHED'//
     1           ' (USE UNDO OR CLEAR)'
         GOTO 1390
      ENDIF
      IF( RCIN.NE.'COL ')THEN
         PRINT *,' ** ENTERING VARIABLE MUST BE A COLUMN'
         GOTO 1390
      ENDIF
C CHECK THAT IN VAR IS NONBASIC AND GET DIRECTION OF CHANGE
      CALL GETBND(RCIN,NUMIN,VLIN,VUIN)
      CALL GETSOL(RCIN,NUMIN,VXIN,VPIN,STATIN,STNUM)
      IF( STATIN.EQ.'L' )THEN
         INDIR = 1
         VIN = VLIN
      ELSE IF( STATIN.EQ.'U' )THEN
         INDIR = -1
         VIN = VUIN
      ELSE
         CALL GETNAM(RCIN,NUMIN,CNAME)
         PRINT *,' ** COLUMN '//CNAME(:NAMELN)//' IS ALREADY BASIC'
         GOTO 1390
      ENDIF
C FTRAN THE NONBASIC VAR
      IF( NALPHA.LT.NROWS )THEN
         PRINT *,' ** SYSERR IN GPIVOT...NALPHA =',NALPHA,' <',NROWS
         GOTO 1390
      ENDIF
      CALL GALPHA(RCIN,NUMIN,ALPHA,NALPHA)
      CALL GFTRAN(ALPHA,NALPHA)
C
C AT THIS POINT, WE HAVE FOR INCOMING (NONBASIC) VAR:
C       RCIN NUMIN = {ROW |COL } NUMBER
C       STATIN     = CURRENT STATUS (L OR U)
C       INDIR      = DIRECTION OF CHANGE (1 OR -1)
C       VLIN,VUIN  = CURRENT BOUNDS (OF IN VARIABLE)
C       VIN        = CURRENT LEVEL (VLIN OR VUIN)
C       VPIN       = CURRENT PRICE
C       ALPHA      = RATES OF SUBSTITUTION
      IF( NUMOUT.GT.0 )GOTO 400
C ==================== AUTOMATIC SELECTION ====================
C FIND BASIC THAT'S DRIVEN TO BOUND 1ST
C NOTE:  FOR AUTOMATIC SELECTION, RANGE OF THE IN VAR IS ALSO A LIMIT
      IF( INDIR.GT.0 )THEN
         IF( VUIN.GE.VINF )THEN
            ZTHETA = VINF
         ELSE
            ZTHETA = VUIN - VLIN
         ENDIF
      ELSE
         IF( VLIN.LE.-VINF )THEN
            ZTHETA = VINF
         ELSE
            ZTHETA = VUIN - VLIN
         ENDIF
      ENDIF
C ZTHETA = RANGE OF IN VAR BEFORE IT HITS OTHER BOUND
C   ::: LOOP OVER BASICS :::
      DO 100 I=1,NROWS
         VRATE = ALPHA(I)*INDIR
         CALL GETBVP(I,ROWCOL,NUMBER)
         CALL GETBND(ROWCOL,NUMBER,VL,VU)
         CALL GETSOL(ROWCOL,NUMBER,VX,VP,STOLD,STNUM)
         IF( VRATE.GT.TOLPIV )THEN
C BASIC INCREASES
            DIR = 1
            IF( STOLD.EQ.'I' )THEN
               IF( VX.GT.VU )GOTO 100
C INFEASIBLE BASIC IS DRIVEN TOWARDS ITS LOWER BOUND
               Z = (VL - VX)/VRATE
               STAT = 'L'
            ELSE
C FEASIBLE   BASIC IS DRIVEN TOWARDS ITS UPPER BOUND
               IF( VU.GE.VINF )GOTO 100
               Z = (VU - VX)/VRATE
               STAT = 'U'
            ENDIF
         ELSE IF( VRATE.LT.-TOLPIV )THEN
C BASIC DECREASES
            DIR = -1
            IF( STOLD.EQ.'I' )THEN
               IF( VX.LT.VL )GOTO 100
C INFEASIBLE BASIC IS DRIVEN TOWARDS ITS UPPER BOUND
               Z = (VU - VX)/VRATE
               STAT = 'U'
            ELSE
C FEASIBLE   BASIC IS DRIVEN TOWARDS ITS LOWER BOUND
               IF( VL.LE.-VINF )GOTO 100
               Z = (VL - VX)/VRATE
               STAT = 'L'
            ENDIF
         ELSE
C PIVOT NOT ACCEPTABLE
            GOTO 100
         ENDIF
C NOW Z = ABSOLUTE CHANGE IN LEVEL OF NONBASIC AT WHICH I-TH BASIC
C         IS DRIVEN TO BOUND
         IF( Z.GE.ZTHETA )GOTO 100
C THIS BASIC BLOCKS THE NONBASIC
         NUMOUT = NUMBER
         PIVROW = I
         RCOUT  = ROWCOL
         ZTHETA = Z
         OUTDIR = DIR
         STATOT = STOLD
         STNEW  = STAT
         VXOUT  = VX
         VLOUT  = VL
         VUOUT  = VU
         IF( Z.LE.0. )GOTO 500
C NEXT BASIC
100   CONTINUE
      IF( NUMOUT.EQ.0 )RETURN
      GOTO 500
C ================= END AUTOMATIC SELECTION ==================
400   CONTINUE
C CALLER HAS SPECIFIED OUT VAR...FIND ITS PIVOT ROW
      ROWCOL = RCOUT
      CALL GETPIV(PIVROW,ROWCOL,NUMOUT)
      IF( PIVROW.EQ.0 )THEN
         CALL GETNAM(ROWCOL,NUMOUT,CNAME)
         PRINT *,' ** '//ROWCOL//CNAME(:NAMELN)//' IS NOT BASIC'
         GOTO 1390
      ENDIF
      VPIVOT = ALPHA(PIVROW)
C GET OUT VAR DIRECTION OF CHANGE, RELATIVE TO INDIR (SAME OR OPPOSITE)
      IF( VPIVOT.GT.TOLPIV )THEN
         OUTDIR = INDIR
      ELSE IF( VPIVOT.LT.-TOLPIV )THEN
         OUTDIR = -INDIR
      ELSE
         GOTO 1310
      ENDIF
C FIND BLOCKING BOUND OF OUT VAR (DRIVEN BY IN VAR)
      CALL GETBND(RCOUT,NUMOUT,VLOUT,VUOUT)
      CALL GETSOL(RCOUT,NUMOUT,VXOUT,VPOUT,STATOT,STNUM)
C IS PIVOT DEGENERATE?
      IF( ABS(VXOUT).LE.VTOLAB .AND. ABS(VIN).LE.VTOLAB )THEN
C -YES, IS IT A BOUND?
         IF( ABS(VLOUT).LE.VTOLAB )THEN
            STNEW = 'L'
         ELSE IF( ABS(VUOUT).LE.VTOLAB )THEN
            STNEW = 'U'
         ELSE
            GOTO 450
         ENDIF
C -YES, DRIVING DIRECTION DOES NOT MATTER...LET PIVOT OCCUR
         VOUT = 0.
         GOTO 490
      ENDIF
450   CONTINUE
      IF( (STATOT.EQ.'B' .AND.OUTDIR.EQ. 1 ) .OR.
     1    (VXOUT.GT.VUOUT.AND.OUTDIR.EQ.-1 ) )THEN
C ...OUT VAR DRIVEN TO VU (MUST BE FINITE)
         IF( VUOUT.GE.VINF )GOTO 1320
         STNEW = 'U'
         VOUT  = VUOUT
      ELSE IF( (STATOT.EQ.'B' .AND.OUTDIR.EQ.-1 ) .OR.
     1         (VXOUT.LT.VLOUT.AND.OUTDIR.EQ. 1 ) )THEN
C ...OUT VAR DRIVEN TO VL (MUST BE FINITE)
         IF( VLOUT.LE.-VINF )GOTO 1320
         STNEW = 'L'
         VOUT  = VLOUT
      ENDIF
C
490   CONTINUE
      ZTHETA = (VOUT - VXOUT)/VPIVOT
      IF( ZTHETA.LT.0 )ZTHETA = -ZTHETA
C ============ GOT OUT VAR AND PIVOT ROW =============
500   CONTINUE
C ========== WE HAVE XB = X + VPIVOT*ëXN =============
C                    :        :      :...IN VAR CHANGE
C                    :        :...RATE OF SUBSTITUTION
C                    :...OUT VAR MOVES OUTDIR TO REACH VOUT (VL OR VU)
C     ==============================
C     ZTHETA = |ëXN| = |(XB - X)/VPIVOT|
C                        :    :...CURRENT LEVEL (VXOUT)
C                        :...NEW LEVEL (VOUT)
C     ==============================
C AT THIS POINT, WE HAVE FOR OUTGOING BASIC VAR:
C     RCOUT NUMOUT = {ROW |COL } NUMBER
C     PIVROW       = PIVOT ROW
C     OUTDIR       = DIRECTION (1 OR -1)
C     STATOT       = OLD STATUS (B OR I)
C     STNEW        = NEW STATUS (U OR L)
C     VLOUT, VUOUT = OUT VAR BOUNDS
C     VXOUT        = OLD LEVEL
C     VOUT         = NEW LEVEL (VUOUT OR VLOUT)
      V = ZTHETA
      VPIVOT = ALPHA(PIVROW)
      IF( V.GE.VINF.OR.ABS(VPIVOT).LT.TOLPIV )THEN
         PRINT *,' ** SYSERR GPIVOT...VTHETA,VPIVOT =',V,VPIVOT
         GOTO 1390
      ENDIF
C SET IN VAR CHANGE (MAGNITUDE * DIRECTION)
      ZTHETA = ZTHETA*INDIR
C SAVE POINTERS IN CASE WE ENCOUNTER ERROR
      IFREE0 = IFREE
      IVFRE0 = IVFREE
      IRBLS0 = IRBLST
      IVBLS0 = IVBLST
      IZBLS0 = IZBLST
      NBLST0 = NBLST
C PUT CURRENT SOLUTION STATUS INTO NEW PIVOT
      CALL GPVST0('PUT ',PIVNUM+1)
C =============== ADD ALPHA-VECTOR ===============
      IF( NBLST.EQ.0 )THEN
C NO ALPHA REGION (LOGICAL BASIS)...CREATE ONE
         IRBLST = IPIVOT + NROWS + 1
         IVBLST = IVFREE   + 1
         IZBLST = IVBLST/2 + 1
C FIRST ALPHA RECORD IS DUMMY (NEEDED FOR BTRAN)
         INDEX(IRBLST) = 0
         ZVALUE(IZBLST) = VINF
C FIRST ALPHA IS NONSPIKE
         CALL BNONSP(PIVROW,NUMIN,*1300)
         GOTO 900
      ENDIF
C
C WE WANT TO ADD ALPHA AT REAR (AS SPIKE)
C CHECK SPACE FOR WORST CASE (100% DENSE ALPHA)
      CALL BSPACE(NROWS,*1300)
      NBLST = NBLST + 1
C PUT PIVOT FIRST (TAG ROW INDEX WITH -)
      INDEX (IRBLST + NBLST) = -PIVROW
      ZVALUE(IZBLST + NBLST) = -ALPHA(PIVROW)
C                              :...STORING -ALPHA
900   CONTINUE
C
      ZTOL0 = TOL0
      IF( SOLST.EQ.'INFEASIBLE' )SOLST = 'FEASIBLE'
C ::: LOOP TO PUT OTHER ALPHA'S AND UPDATE PRIMAL :::
      DO 1000 I=1,NROWS
         IF( I.EQ.PIVROW .OR. DABS(ALPHA(I)).LT.ZTOL0 )GOTO 1000
C ALPHA-VALUE IS ACCEPTABLE TO ADD
         IF( NBLST0.GT.0 )THEN
C ADD TO ALPHA REGION
            NBLST = NBLST + 1
            INDEX (IRBLST + NBLST) = I
            ZVALUE(IZBLST + NBLST) = -ALPHA(I)
C                                    :...STORING -ALPHA
         ENDIF
C (NBLST0 = 0 MEANS WE ALREADY ADDED ALPHA AS NONSPIKE;
C  WE ENTERED THIS LOOP JUST TO UPDATE BASIC LEVELS)
         CALL GETBVP(I,ROWCOL,NUMBER)
         CALL GETSOL(ROWCOL,NUMBER,VX,VP,STAT,STNUM)
C UPDATE BASIC LEVEL = CURRENT + RATE*(CHANGE IN XN)
                  VX = VX      + ALPHA(I)*ZTHETA
         CALL GETBND(ROWCOL,NUMBER,VL,VU)
         IF( ( VX.LT.VL - TOLAXI - TOLRXI*ABS(VL) ) .OR.
     1       ( VX.GT.VU + TOLAXI + TOLRXI*ABS(VU) ) )THEN
            STAT = 'I'
            SOLST = 'INFEASIBLE'
         ELSE
            STAT = 'B'
         ENDIF
         CALL GPTSOL(ROWCOL,NUMBER,STAT,VX)
1000  CONTINUE
C ::: END LOOP ... ALPHA ENTERED AND BASIC LEVELS UPDATED :::
C
C UPDATE PIVOT ID FOR ROW
      INDEX( IPIVOT+PIVROW ) = NUMIN
C CHANGE STATUS AND VALUES OF IN AND OUT VARS
      VIN = VIN + ZTHETA
      IF( ( VIN.LT.VLIN - TOLAXI - TOLRXI*ABS(VLIN) ) .OR.
     1    ( VIN.GT.VUIN + TOLAXI + TOLRXI*ABS(VUIN) ) )THEN
         STAT  = 'I'
         SOLST = 'INFEASIBLE'
      ELSE
         STAT  = 'B'
      ENDIF
      CALL GPTSOL(RCIN,NUMIN,STAT,VIN)
      VPOUT = -VPIN/VPIVOT
      CALL GPTSOL(RCOUT,NUMOUT,STNEW,VPOUT)
C ADVANCE FREE SPACE POINTERS
      IFREE  = IRBLST +   NBLST + 1
      IVFREE = IVBLST + 2*NBLST + 1
C ADD TO PIVOT HISTORY
      PIVNUM = PIVNUM + 1
C ...LET CALLER KNOW NUMBER
      NPIVOT = PIVNUM
C ONLY COLUMN ENTERS (MAY CHANGE LATER)
      PIVIN (PIVNUM) = NUMIN
      PIVSTI(PIVNUM) = STATIN.EQ.'L'
      PIVSTO(PIVNUM) = STATOT.EQ.'B'
C USE SIGN TO DISTINGUISH ROW (-) FROM COL (+)
      IF( RCOUT.EQ.'ROW ')THEN
         PIVOUT(PIVNUM) = -NUMOUT
      ELSE
         PIVOUT(PIVNUM) =  NUMOUT
      ENDIF
C NBLST BEFORE PIVOT (TO REMOVE ALPHA WITH UNDO)
      PIVNBL(PIVNUM) = NBLST0
C
C SUCCESSFUL PIVOT COMES HERE
      IF( SWDBG )PRINT *,' PIVOT SUCCESSFULL...SOLST=',SOLST
      RETURN
C
C ERROR AFTER CHANGING BASIS POINTERS AND STATS
1300  CONTINUE
C RESTORE POINTERS
      IFREE  = IFREE0
      IVFREE = IVFRE0
      IRBLST = IRBLS0
      IVBLST = IVBLS0
      IZBLST = IZBLS0
      NBLST  = NBLST0
C REMOVE PIVOT FROM HISTORY
      PIVNUM = PIVNUM - 1
      GOTO 1390
1310  PRINT *,' ** PIVOT INDADMISSIBLE: ELEMENT TOO SMALL...',
     1        'TOLERANCE =',TOLPIV
      GOTO 1380
1320  PRINT *,' ** PIVOT INADMISSIBLE: DIRECTION INCOMPATABLE'
1380  PRINT *,' ...RATE OF SUBSTITUTION =',VPIVOT
C ALL ERROR RETURNS COME HERE
1390  CONTINUE
      IF( SWDBG )THEN
         CALL GETNAM(RCIN ,NUMIN ,CNAME)
         PRINT *,' ',RCIN ,CNAME(:NAMELN),' ST=',STATIN,' X=',VIN  ,
     1           ' DIR',INDIR
         CALL GETNAM(RCOUT,NUMOUT,CNAME)
         PRINT *,' ',RCOUT,CNAME(:NAMELN),' ST=',STATOT,' X=',VXOUT,
     1           ' DIR',OUTDIR
      ENDIF
      RETURN 1
C
C ** GPIVOT ENDS HERE
      END
      SUBROUTINE GBUNDO(NUMBER)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This reverses last NUMBER of pivots.
C       NUMBER >= PIVNUM ===> reverse all (restore to original basis).
C This restores only statuses and alpha's.
C CALLER MUST REFRESH PRIMAL AND DUAL VALUES.
C
C LOCAL
      CHARACTER*1  STAT
      CHARACTER*4  ROWCOL
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
      IF( NUMBER.GT.PIVNUM )THEN
         NUNDO = PIVNUM
      ELSE
         NUNDO = NUMBER
      ENDIF
      IF( NUNDO.EQ.0 )RETURN
      V0 = 0.
C ::: LOOP TO RESTORE PIVOT STATUSES :::
      DO 1000 PIVOT=PIVNUM,PIVNUM-NUNDO+1,-1
C RESTORE PIVOT ROW ASSIGNMENT
         OUTVAR = PIVOUT(PIVOT)
         INVAR  = PIVIN (PIVOT)
         CALL GETPIV(PIVROW,'COL ',INVAR)
CDEBUG
         IF( PIVROW.EQ.0 )THEN
            PRINT *,' ** SYSERR GBUNDO...PIVROW=0',PIVOT
            RETURN
         ENDIF
CDEBUG
         INDEX(IPIVOT + PIVROW) = OUTVAR
C RESTORE STATUSES OF IN AND OUT VARS
         IF( PIVSTI(PIVOT) )THEN
            STAT = 'L'
         ELSE
            STAT = 'U'
         ENDIF
         CALL GPTSOL('COL ',INVAR,STAT,V0)
C NOTE: V0 DOESN'T MATTER SINCE CALLER MUST REFRESH ANYWAY TO GET VALS
         IF( OUTVAR.GT.0 )THEN
            ROWCOL = 'COL '
         ELSE
            ROWCOL = 'ROW '
            OUTVAR = -OUTVAR
         ENDIF
         IF( PIVSTO(PIVOT) )THEN
            STAT = 'B'
         ELSE
            STAT = 'I'
         ENDIF
         CALL GPTSOL(ROWCOL,OUTVAR,STAT,V0)
1000  CONTINUE
C
      PIVNUM = PIVNUM-NUNDO
C NOW PIVNUM+1 = IS PIVOT OF LAST UNDO
C REMOVE ALPHA'S
      NBLST  = PIVNBL(PIVNUM+1)
C RESTORE SOLUTION STATUS
      CALL GPVST0('SET ',PIVNUM+1)
C
      RETURN
C
C ** GBUNDO ENDS HERE
      END
      SUBROUTINE GBPHST(RCIN,RCOUT,SOLST0,NUMBER)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This returns pivot info for NUMBER (1 transaction) for
C       NUMBER > PIVNUM ===> NUMBER truncated to PIVNUM
C RCIN & RCOUT of form:
C   stat {ROW|COL} name
C    :  :.........:.....1 space delimiter
C    :...1 char
C
      CHARACTER*(*) RCIN,RCOUT
C                :...MUST BE >= 6+NAMELN
      CHARACTER*(*) SOLST0
C                :...SHOULD BE 16
C LOCAL
      CHARACTER*1  STAT
      CHARACTER*4  ROWCOL
      CHARACTER*16 CNAME
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
      IF( NUMBER.GT.PIVNUM )NUMBER = PIVNUM
      IF( PIVNUM.EQ.0 )RETURN
      INVAR  = PIVIN(NUMBER)
      CALL GETNAM('COL ',INVAR,CNAME)
      IF( PIVSTI(NUMBER) )THEN
         STAT = 'L'
      ELSE
         STAT = 'U'
      ENDIF
      RCIN = STAT//' COL '//CNAME(:NAMELN)
C
      OUTVAR = PIVOUT(NUMBER)
      IF( OUTVAR.GT.0 )THEN
         ROWCOL = 'COL '
      ELSE
         ROWCOL = 'ROW '
         OUTVAR = -OUTVAR
      ENDIF
      CALL GETNAM(ROWCOL,OUTVAR,CNAME)
      IF( PIVSTO(NUMBER) )THEN
         STAT = 'B'
      ELSE
         STAT = 'I'
      ENDIF
      RCOUT = STAT//' '//ROWCOL//CNAME(:NAMELN)
C
      SOLST0 = 'GET '
      CALL GPVST0(SOLST0,NUMBER)
C
      RETURN
C
C ** GBPHST ENDS HERE
      END
      SUBROUTINE GPVST0(WHAT,NUMBER)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
      CHARACTER*(*) WHAT
C
C       WHAT = PUT ===> Put solution status into PIVST0(NUMBER)
C            = SET ===> Set solution status from PIVST0(NUMBER)
C            = GET ===> Get solution status from PIVST0(NUMBER)
C LOCAL
      CHARACTER*16 LCLST(0:4)
      DATA         LCLST/'Unknown',
     1       'OPTIMAL','FEASIBLE','INFEASIBLE','UNBOUNDED'/
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      IF( WHAT.EQ.'PUT ')THEN
         IF( SOLST.EQ.'OPTIMAL' )THEN
            PIVST0(NUMBER) = 1
         ELSE IF( SOLST.EQ.'FEASIBLE' )THEN
            PIVST0(NUMBER) = 2
         ELSE IF( SOLST.EQ.'INFEASIBLE' )THEN
            PIVST0(NUMBER) = 3
         ELSE IF( SOLST.EQ.'UNBOUNDED' )THEN
            PIVST0(NUMBER) = 4
         ELSE
            PIVST0(NUMBER) = 0
         ENDIF
         IF( STCOMP.EQ.1 )PIVST0(NUMBER) = -PIVST0(NUMBER)
      ELSE
         ST = PIVST0(NUMBER)
         IF( ST.LT.0 )THEN
            ST = -ST
            STCOMP = 1
         ENDIF
         IF( WHAT.EQ.'SET ')THEN
            SOLST = LCLST(ST)
         ELSE
            WHAT = LCLST(ST)
         ENDIF
      ENDIF
C
      RETURN
C
C ** GPVST0 ENDS HERE
      END
