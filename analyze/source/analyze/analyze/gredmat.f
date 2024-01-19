C                ::: GREDMAT.FOR  8-16-95 :::
C
C Earlier dates deleted
C       2-13-95...Checked for nonzero V from GETVAL (CC 5-25)
C       8-02-95...Fixed overwrite bug in REDMAT (CC 8-02)
C       8-14-95...Moved REDEBG to GREDUCE
C
C   REDMAT...Overwrites submat bounds with reduced values.
C   REDBND...Overwrites a bound.
C   REDFIX...Fixes a value (purifies)
C   REDINF...Clean infinite bounds
C   REDGET...Gets reduced bound (for outside caller)
C   REDRPT...Report results
C   GZPROD...Inner product (double precision)
C   REDTRK...Track row or column (for debug)
C
      SUBROUTINE REDMAT(IV0)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
      INCLUDE 'DCREDUCE.'
CITSO      INCLUDE (DCREDUCE)
CI$$INSERT DCREDUCE
C
C This overwrites the bounds with the reduced values (of submat).
C
C       IV0 = ORIGINAL VALUE OF IVFREE (BEFORE REDUCE), WHICH IS
C             WHERE THE VALUE POOL EXTENDS
C
C LOCAL
      CHARACTER*1 CHAR
      LOGICAL*1   SW
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF( IV0.NE.IXL )THEN
         PRINT *,' ** SYSERR IN REDMAT...',IV0,IXL
         PRINT *,' ...CANNOT OVERWRITE BOUNDS'
         RETURN
      ENDIF
C BECAUSE EVERY COLUMN BOUND MIGHT REQUIRE A NEW ENTRY INTO VALUE POOL,
C WE MUST COPY LOWER BOUNDS FIRST, THEN UPPER BOUNDS.
C (THIS WILL CHANGE IN FUTURE UPDATE OF DATA STRUCTURE.)
      DO 100 I=1,NCOLS
         CALL GETMAP('COL ',I,SW)
         IF( .NOT.SW )GOTO 100
         VNEW = VALUE(IXL+I)
CC 8-2-95 DO NOT OVERWRITE IF STAT=L AND VNEW=-*
         CALL GETST('COL ',I,CHAR,N)
         IF( CHAR.EQ.'L' .AND. VNEW.LE.-VINF )GOTO 100
         CALL GETVAL(VOLD,ICLBND+I)
         CALL REDBND(VNEW,VOLD,ICLBND+I,IV0)
100   CONTINUE
      DO 150 I=1,NCOLS
         CALL GETMAP('COL ',I,SW)
         IF( .NOT.SW )GOTO 150
         VNEW = VALUE(IXU+I)
CC 8-2-95 DO NOT OVERWRITE IF STAT=U AND VNEW=*
         CALL GETST('COL ',I,CHAR,N)
         IF( CHAR.EQ.'U' .AND. VNEW.GE.VINF )GOTO 150
         CALL GETVAL(VOLD,ICUBND+I)
         CALL REDBND(VNEW,VOLD,ICUBND+I,IV0)
150   CONTINUE
C
      DO 200 I=1,NROWS
         CALL GETMAP('ROW ',I,SW)
         IF( .NOT.SW )GOTO 200
         VNEW = VALUE(IYL+I)
CC 8-2-95 DO NOT OVERWRITE IF STAT=L AND VNEW=-*
         CALL GETST('ROW ',I,CHAR,N)
         IF( CHAR.EQ.'L' .AND. VNEW.LE.-VINF )GOTO 200
         CALL GETVAL(VOLD,IRLBND+I)
         CALL REDBND(VNEW,VOLD,IRLBND+I,IV0)
200   CONTINUE
      DO 250 I=1,NROWS
         CALL GETMAP('ROW ',I,SW)
         IF( .NOT.SW )GOTO 250
         VNEW = VALUE(IYU+I)
CC 8-2-95 DO NOT OVERWRITE IF STAT=U AND VNEW=*
         CALL GETST('ROW ',I,CHAR,N)
         IF( CHAR.EQ.'U' .AND. VNEW.GE.VINF )GOTO 250
         CALL GETVAL(VOLD,IRUBND+I)
         CALL REDBND(VNEW,VOLD,IRUBND+I,IV0)
250   CONTINUE
C
      RETURN
C
C ** REDMAT ENDS HERE
      END
      SUBROUTINE REDBND(VNEW,VOLD,IPOINT,IV0)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
      INCLUDE 'DCREDUCE.'
CITSO      INCLUDE (DCREDUCE)
CI$$INSERT DCREDUCE
C
C This overwrites a bound (VOLD) with VNEW, put into INDEX(IPOINT).
C If new value must be added to pool, this is at IV0, which is
C then incremented.  (CALLER IS RESPONSIBLE FOR IV0 < MAXVAL)
C
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF( ABS(VOLD-VNEW) .LE. VAXDIF + VRXDIF* ABS(VOLD) )RETURN
C VNEW <> VOLD ...OVERWRITE
      IF( VNEW.LT.-VTAFIX )THEN
C VNEW < 0
         IF( VNEW.LE.-VINF )THEN
C ...VNEW = -VINF = -VALUE(INF)
            INDEX(IPOINT) = -INF
         ELSE IF( ABS(VNEW+1.) .LE. VTOLAB )THEN
C ...VNEW = -1 = -VALUE(1)
            INDEX(IPOINT) = -1
         ELSE
C ...ADD VNEW TO VALUE POOL (ADVANCE IV0)  CC 8-02 IV0 ADVANCED 1ST
            IV0 = IV0 + 1
            INDEX(IPOINT) = -IV0
            VALUE(IV0)    = -VNEW
         ENDIF
      ELSE IF( VNEW.GT.VTAFIX )THEN
C VNEW > 0
         IF( VNEW.GE. VINF )THEN
C ...VNEW = VINF = VALUE(INF)
            INDEX(IPOINT) = INF
         ELSE IF( ABS(VNEW-1.) .LE. VTOLAB )THEN
C ...VNEW = 1 = VALUE(1)
            INDEX(IPOINT) = 1
         ELSE
C ...ADD VNEW TO POOL (ADVANCE IV0)  CC 8-02 IV0 ADVANCED 1ST
            IV0 = IV0 + 1
            INDEX(IPOINT) = IV0
            VALUE(IV0)    = VNEW
         ENDIF
      ELSE
C VNEW = 0
         INDEX(IPOINT) = 0
      ENDIF
C
C INCREMENT BOUND CHANGE FOR EDIT INFO
      EDRED = EDRED + 1
      RETURN
C
C ** REDBND ENDS HERE
      END
      SUBROUTINE REDFIX(VX,J)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
      INCLUDE 'DCREDUCE.'
CITSO      INCLUDE (DCREDUCE)
CI$$INSERT DCREDUCE
C
C This fixes column J = VX (for reduce).  Preference is given to 0, then
C to its original bounds, if within tolerance.  Third preference is
C given to integer (if within tolerance).  Otherwise, VX is unchanged.
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      VABS = ABS(VX)
      IF( VABS.LT.VTAFIX )THEN
         VX = 0.
         GOTO 900
      ENDIF
      VTOL = VTAFIX + VTRFIX*VABS
      CALL GETBND('COL ',J,VL,VU)
      IF(      ABS(VX-VL).LE.VTOL )THEN
         VX = VL
      ELSE IF( ABS(VX-VU).LE.VTOL )THEN
         VX = VU
      ELSE
C LOOK FOR AN INTEGER WITHIN TOLERANCE
         INT = VX
         V   = FLOAT(INT)
         IF(    ABS(VX-V).LE.VTOL )THEN
            VX = V
         ELSE
            V = FLOAT(INT+1)
            IF(  ABS(VX-V).LE.VTOL )VX = V
         ENDIF
      ENDIF
C
900   CONTINUE
C FIX = VX
      VALUE(IXL+J) = VX
      VALUE(IXU+J) = VX
      RETURN
C
C ** REDFIX ENDS HERE
      END
      SUBROUTINE REDINF
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
      INCLUDE 'DCREDUCE.'
CITSO      INCLUDE (DCREDUCE)
CI$$INSERT DCREDUCE
C
C This cleans infinite bounds.
C ::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      DO 100 I=1,NROWS
         IF( VALUE(IYL+I).LE.-VTSUM )VALUE(IYL+I) = -VINF
         IF( VALUE(IYU+I).GE. VTSUM )VALUE(IYU+I) =  VINF
100   CONTINUE
C
      DO 200 I=1,NCOLS
         IF( VALUE(IXL+I).LE.-VTSUM )VALUE(IXL+I) = -VINF
         IF( VALUE(IXU+I).GE. VTSUM )VALUE(IXU+I) =  VINF
200   CONTINUE
C
      RETURN
C
C ** REDINF ENDS HERE
      END
      SUBROUTINE REDGET(ROWCOL,NUMBER,VXL,VXU,VPL,VPU)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
      INCLUDE 'DCREDUCE.'
CITSO      INCLUDE (DCREDUCE)
CI$$INSERT DCREDUCE
C
C This returns reduced bounds (VL, VU) for ROWCOL NUMBER
C
      CHARACTER*(*) ROWCOL
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      IF( NUMBER.LE.0 )GOTO 1300
      IF( ROWCOL.EQ.'ROW ' )THEN
         IF( NUMBER.GT.NROWS )GOTO 1300
         VXL = VALUE(IYL+NUMBER)
         VXU = VALUE(IYU+NUMBER)
         VPL = VALUE(IPL+NUMBER)
         VPU = VALUE(IPU+NUMBER)
      ELSE IF( ROWCOL.EQ.'COL ' )THEN
         IF( NUMBER.GT.NCOLS )GOTO 1300
         VXL = VALUE(IXL+NUMBER)
         VXU = VALUE(IXU+NUMBER)
C  COMPUTE REDUCED COST RANGE
         VPL = 0.0
         VPU = 0.0
         NZ = BASE(ICINFO + NUMBER) - BASE(ICINFO + NUMBER-1)
         IF( NZ.EQ.0 )RETURN
C      LOOP OVER ROWS
         I1 = INONZ + 2*BASE(ICINFO + NUMBER-1)
         I2 = I1 + 2*NZ - 1
         DO 300 I=I1,I2,2
            ROW = INDEX(I)
            VPLROW = VALUE(IPL+ROW)
            VPUROW = VALUE(IPU+ROW)
            IF( INDEX(IRSTAT + ROW).GT.0 )GOTO 300
C ROW IS IN SUBMATRIX
            CALL GETVAL(V,I+1)
            IF( V.GT.VTOLAB )THEN
               IF( VPL.GT.-VINFX .AND. VPUROW.LT.VINFX )THEN
                  VPL = VPL - V*VPUROW
               ELSE
                  VPL = - VINF
               ENDIF
               IF( VPU.LT.VINFX .AND. VPLROW.GT.-VINFX )THEN
                  VPU = VPU - V*VPLROW
               ELSE
                  VPU = VINF
               ENDIF
            ELSE IF( V.LT.-VTOLAB )THEN
               IF( VPL.GT.-VINFX .AND. VPLROW.GT.-VINFX )THEN
                  VPL = VPL - V*VPLROW
               ELSE
                  VPL = - VINF
               ENDIF
               IF( VPU.LT.VINFX .AND. VPUROW.LT.VINFX )THEN
                  VPU = VPU - V*VPUROW
               ELSE
                  VPU = VINF
               ENDIF
            ELSE
               GOTO 300
            ENDIF
            IF( VPL.LE.-VINF .AND. VPU.GE.VINF )RETURN
300      CONTINUE
C   ::: END LOOP OVER ROWS :::
      ELSE
         GOTO 1300
      ENDIF
C
      RETURN
C
C ERROR RETURN
1300  PRINT *,' ** SYSERR REDGET...',ROWCOL,NUMBER,'...PLEASE REPORT'
      RETURN
C
C ** REDGET ENDS HERE
      END
      SUBROUTINE REDRPT(RPTBND,RPTSUM,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
      INCLUDE 'DCREDUCE.'
CITSO      INCLUDE (DCREDUCE)
CI$$INSERT DCREDUCE
C
C This writes REDUCE report to OUTPUT.
C
      LOGICAL*1 RPTBND,RPTSUM
C LOCAL
      CHARACTER*8 RSTAT,ROWCOL
      LOGICAL*1   SWHEAD
C ::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C  ROWS SECTION
      SWHEAD = .FALSE.
      RFREED = 0
      RFORCE = 0
      RWEAKN = 0
      REDUCE = 0
C
      DO 9100 I=1,NROWS
         IF(INDEX(IRSTAT + I).GT.0)GOTO 9100
         VYMIN = VALUE(IYMIN + I)
         IF(VYMIN.LE.-VINFX)VYMIN = -VINF
         VYMAX = VALUE(IYMAX + I)
         IF(VYMAX.GE. VINFX)VYMAX =  VINF
C
         RSTAT = ' '
         IF(INDEX(IFREE + I).NE.0)THEN
                         RSTAT = 'Forcing'
                         RFORCE = RFORCE + 1
         ELSE
            LO = INDEX(IRLBND + I)
            UP = INDEX(IRUBND + I)
            VPL = VALUE(IPL + I)
            VPU = VALUE(IPU + I)
            IF(    (LO.EQ.-INF .AND. UP.NE.INF.AND.VPL.GE.0.0)
     1        .OR. (LO.NE.-INF .AND. UP.EQ.INF.AND.VPU.LE.0.0)
     2        .OR. (LO.NE.-INF .AND. UP.NE.INF.AND.VPL.GE.VPU)
     3        )THEN
                         RSTAT = 'Freed'
                         RFREED = RFREED + 1
            ELSE IF( (LO.NE.-INF.AND.UP.NE.INF)  .AND.
     1               (VPL.GE.0.0.OR.VPU.LE.0.0) )THEN
                         RSTAT = 'Weakened'
                         RWEAKN = RWEAKN + 1
            ELSE IF( (LO.NE.-INF .AND. VPU.LT. VINFX) .OR.
     1               (UP.NE. INF .AND. VPL.GT.-VINFX))THEN
                         RSTAT = 'Reduced'
                         REDUCE = REDUCE + 1
            ENDIF
         ENDIF
C
         IF( RSTAT.EQ.' ' .OR. .NOT.RPTBND )GOTO 9100
C     WRITE REDUCTION (FIRST PRINT HEADING IF THIS IS FIRST ROW)
C
         IF( .NOT.SWHEAD )THEN
            ROWCOL = ' Row '
            WRITE(OUTPUT,9010,ERR=13000)ROWCOL
9010        FORMAT(/A8/' Name',T18,'Min Level',T32,'Max Level',T47,
     1                 'Min Price',T61,'Max Price' /' ',78('-') )
            SWHEAD = .TRUE.
         ENDIF
         WRITE(OUTPUT,9101,ERR=13000)NAME(IRNAME + I),
     1         VYMIN,VYMAX,VALUE(IPL + I),VALUE(IPU + I),RSTAT
9101     FORMAT(1X,A16,1X,G12.5,1X,G12.5,1X,G12.5,1X,G12.5,2X,A8)
9100  CONTINUE
C
      IF( SWHEAD.AND.RPTBND )THEN
         ROWCOL = 'rows.'
         WRITE(OUTPUT,9102,ERR=13000)ROWCOL
9102     FORMAT(' ...No change in other ',A8)
      ENDIF
C
C  COLUMNS SECTION
C
      SWHEAD = .FALSE.
      CFIXED = 0
      CFORCE = 0
      CREDUC = 0
C
      DO 9500 J=1,NCOLS
         IF(INDEX(ICSTAT + J).GT.0)GOTO 9500
C  SET COMPUTED BOUNDS
         VXL = VALUE(IXL + J)
         VXU = VALUE(IXU + J)
C  COMPUTE REDUCED COST RANGE
         VDMIN = 0.0
         VDMAX = 0.0
         RSTAT = 'Null'
         NZ = BASE(ICINFO + J) - BASE(ICINFO + J-1)
         IF(NZ.EQ.0)GOTO 9400
C      LOOP OVER ROWS
         I1 = INONZ + 2*BASE(ICINFO + J-1)
         I2 = I1 + 2*NZ - 1
C
         DO 9300 I=I1,I2,2
            ROW = INDEX(I)
            IF(INDEX(IRSTAT + ROW).GT.0)GOTO 9300
            RSTAT = ' '
            CALL GETVAL(V,I+1)
            IF( V.LT.-VTOLAB )THEN
CC 5-25-95           =======
                VDMIN = VDMIN - V*VALUE(IPL + ROW)
                VDMAX = VDMAX - V*VALUE(IPU + ROW)
            ELSE IF( V.GT.VTOLAB )THEN
CC 5-25-95       =====================   NOTE THAT V=0 DOES NOTHING
                VDMIN = VDMIN - V*VALUE(IPU + ROW)
                VDMAX = VDMAX - V*VALUE(IPL + ROW)
            ENDIF
9300     CONTINUE
C     ::: End LOOP OVER ROWS :::
C
         IF(VDMIN.LE.-VINFX)VDMIN = -VINF
         IF(VDMAX.GE. VINFX)VDMAX =  VINF
C
         IF(RSTAT.EQ.'Null')GOTO 9400
C
C    TEST FOR REDUCE STATUS (RSTAT)
         CALL GETVAL(VL,ICLBND + J)
         CALL GETVAL(VU,ICUBND + J)
         IF(VXL.LE.-VINFX .AND. VDMIN.GE.0.0 .OR.
     1      VXU.GE. VINFX .AND. VDMAX.LE.0.0)THEN
                  RSTAT = 'Forcing'
                  CFORCE = CFORCE + 1
         ELSE IF(VXL.GE.VXU .AND. VL.LT.VU)THEN
                  RSTAT = 'Fixed'
                  CFIXED= CFIXED + 1
         ELSE IF(VXL.GT.VL + VTOLAB .OR. VXU.LT.VU - VTOLAB)THEN
                  RSTAT = 'Reduced'
                  CREDUC = CREDUC + 1
         ELSE
                  GOTO 9500
         ENDIF
C
9400     CONTINUE
         IF( .NOT.RPTBND )GOTO 9500
C     WRITE REDUCED COLUMN (FIRST PRINT HEADING IF THIS IS FIRST ONE)
C
         IF( .NOT.SWHEAD )THEN
            ROWCOL = ' Column '
            WRITE(OUTPUT,9010,ERR=13000)ROWCOL
            SWHEAD = .TRUE.
         ENDIF
         WRITE(OUTPUT,9101,ERR=13000)NAME(ICNAME + J),
     1          VXL,VXU,VDMIN,VDMAX,RSTAT
9500  CONTINUE
C
      IF( .NOT.RPTBND )GOTO 9900
      IF( SWHEAD )THEN
         ROWCOL = 'columns.'
         WRITE(OUTPUT,9102,ERR=13000)ROWCOL
      ENDIF
      WRITE(OUTPUT,9810,ERR=13000)
9810  FORMAT(/5X,'*** End REDUCE Bound Reduction Report ***')
C
C   ALL NORMAL RETURNS COME HERE
9900  CONTINUE
      IF( .NOT.RPTSUM )RETURN
      WRITE(OUTPUT,9901,ERR=13000)RFREED,RFORCE,REDUCE,RWEAKN,
     1      CFIXED,CFORCE,CREDUC
      IF( OUTPUT.NE.TTYOUT )WRITE(*,9901)RFREED,RFORCE,REDUCE,RWEAKN,
     1      CFIXED,CFORCE,CREDUC
9901  FORMAT(/10X,'Summary of Reductions'/10X,22('-')
     1       /'    Rows: ',I6,' freed',I6,' forcing',I6,' reduced',
     *                                               I5,' weakened'
     2       /' Columns: ',I6,' fixed',I6,' forcing',I6,' reduced'/)
      RETURN
C
C I/O ERROR RETURN
13000 RETURN 1
C
C ** REDRPT ENDS HERE
      END
      SUBROUTINE GZPROD(ZVEC,NZVEC,COLUMN,ZPROD)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C Get ZPROD = ZVEC*A(COLUMN) (same as GRPDCT, except in double precision)
C
      DIMENSION ZVEC(NZVEC)
C MUST HAVE NZVEC >= NROWS
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C INITIALIZE
      ZPROD = 0.0
      I1 = INONZ + 2*BASE(ICINFO+COLUMN-1)
      I2 = INONZ + 2*BASE(ICINFO+COLUMN) - 1
C
      DO 100 I=I1,I2,2
           ROW = INDEX(I)
           CALL GETVAL(V,I+1)
           ZPROD = ZPROD + V*ZVEC(ROW)
100   CONTINUE
C
      RETURN
C
C ** GZPROD ENDS HERE
      END
      SUBROUTINE REDTRK(CLIST,LAST,NOTE,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
      INCLUDE 'DCREDUCE.'
CITSO      INCLUDE (DCREDUCE)
CI$$INSERT DCREDUCE
C
C This sees if CLIST(1:LAST) contains reference to row ROWDBG or column
C COLDBG If so, output is CLIST and NOTE.
C ...If debug is entered, alternate return is abort.
C
      CHARACTER*128 CLIST
      CHARACTER*(*) NOTE
C LOCAL
      CHARACTER*16  RCNAME
      CHARACTER*1   CHAR
      CHARACTER*8   ROWKEY(3),COLKEY(6)
      DATA ROWKEY/'row     ','Row    ','ROW    '/
      DATA COLKEY/'column  ','Column  ','COLUMN ',
     1            'activity','Activity','ACTIVITY'/
C ::::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      IF( ROWDBG.GT.0 )THEN
         DO 100 K=1,3
           FIRST = 1
50         CONTINUE
              CALL FLOOKF(CLIST,FIRST,LAST,ROWKEY(K),NUMBER)
              IF( NUMBER.EQ.0 )GOTO 100
              CALL FTOKEN(CLIST,FIRST,LAST,RCNAME, 8,CHAR)
              CALL FTOKEN(CLIST,FIRST,LAST,RCNAME,16,CHAR)
              IF( RCNAME.NE.' ' )THEN
                 CALL GETNUM('ROW ',RCNAME,NUMBER)
                 IF( NUMBER.EQ.ROWDBG )GOTO 500
              ENDIF
            GOTO 50
100      CONTINUE
      ENDIF
C
      IF( COLDBG.GT.0 )THEN
         DO 200 K=1,6
           FIRST = 1
150        CONTINUE
              CALL FLOOKF(CLIST,FIRST,LAST,COLKEY(K),NUMBER)
              IF( NUMBER.EQ.0 )GOTO 200
              FIRST = NUMBER
              CALL FTOKEN(CLIST,FIRST,LAST,RCNAME, 8,CHAR)
              CALL FTOKEN(CLIST,FIRST,LAST,RCNAME,16,CHAR)
              IF( RCNAME.NE.' ' )THEN
                 CALL GETNUM('COL ',RCNAME,NUMBER)
                 IF( NUMBER.EQ.COLDBG )GOTO 500
              ENDIF
            GOTO 150
200      CONTINUE
      ENDIF
      RETURN
C
500   CONTINUE
C ROW | COL FOUND ... PRINT LINE+NOTE
      PRINT *,' ',CLIST(:LAST)
      PRINT *,' ',NOTE
      PAUSE = PAUSE-2
      IF( PAUSE.LE.0 )THEN
         CALL SYSDBG
      ELSE
         CALL REDEBG
      ENDIF
      PRINT *,' ABORT (N/Y)? '
      READ(*,'(A1)')CHAR
      IF( CHAR.EQ.'Y' )RETURN 1
      RETURN
C
C ** REDTRK ENDS HERE
      END
