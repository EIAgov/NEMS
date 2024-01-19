C               ::: GREDBIN.FOR  8-24-95 :::
C
C Earlier dates deleted
C       8-29-93...Loosened test (VT =);  allow VCOEF=0 (CC 5-25)
C       5-25-95...Restructured (8-14)
C       8-17-95...Removed CDEBUG
C
C This contains REDUCE routines pertaining to binary variables.
C
C     REDBIN...Binary tests (and second column loop).
C     REDLGL...Implication tests
C     REDTXT...Text output
C
      SUBROUTINE REDBIN(CLIST,NAME01,NAMINT,*,*)
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
C Loop over columns a second time (after REDROW) to extremize levels
C for each row, and to apply binary test to 0-1 variables (NAME01)
C    First alternate return is for infeasibility.
C    Second alternate return is from FTEXT (could be i/o error).
C
      CHARACTER*128 CLIST
      CHARACTER*(*) NAME01,NAMINT
C
C LOCAL
       CHARACTER*16  NEWLO,NEWUP,CNAME
       CHARACTER*32  PREFIX,NOTE
       LOGICAL*1     SW
C ::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      NOTE = 'REDBIN'
C FORMAT FOR LOG ENTRIES
1     FORMAT(1X,A79)
C  ::: LOOP OVER COLUMNS :::
      DO 3500 J=1,NCOLS
         IF(INDEX(ICSTAT + J).GT.0)GOTO 3500
C COLUMN IS IN SUBMATRIX...SET CURRENT BOUNDS FOR TESTS
         VL = VALUE(IXL + J)
         VU = VALUE(IXU + J)
         IF( VL.GE.VU )GOTO 3500
         NZ = BASE(ICINFO + J) - BASE(ICINFO + J-1)
         IF( NZ.EQ.0 )GOTO 3500
C COLUMN IS NOT FIXED AND HAS A NONZERO...PREPARE TO LOOP OVER ROWS
         I1 = INONZ + 2*BASE(ICINFO + J-1)
         I2 = I1 + 2*NZ - 1
         CNAME = NAME(ICNAME + J)
         PREFIX = 'Activity '//CNAME
         CPOINT= 11 + NAMELN
         NEWLO = ' '
         NEWUP = ' '
C
C  ::::::::: LOOP OVER NONZEROES ::::::::::::
         DO 3200 I=I1,I2,2
            ROW = INDEX(I)
            IF( INDEX(IRSTAT+ROW).GT.0 )GOTO 3200
C    ROW IS IN SUBMATRIX...GET COEFFICIENT
            CALL GETVAL(V,I+1)
            IF( ABS(V).LE.VTOL0C )GOTO 3200
C    ...COEF (V) IS NOT ZERO
C    WAS ROW MARKED FORCING?
            IF( INDEX(IFREE + ROW).LT.0 )THEN
C    -YES, FORCE X AT ITS EXTREME VALUE FOR YMIN
               IF( V.LT.0. )THEN
                  VX = VU
               ELSE
                  VX = VL
               ENDIF
               GOTO 3150
            ELSE IF( INDEX(IFREE + ROW).GT.0 )THEN
C    -YES, FORCE X AT ITS EXTREME VALUE FOR YMAX
               IF( V.GT.0. )THEN
                  VX = VU
               ELSE
                  VX = VL
               ENDIF
               GOTO 3150
            ENDIF
C    -NO, PREPARE TO TEST FOR TIGHTENING BOUNDS
            VUNEW = VU
            VLNEW = VL
C COMPUTE BOUNDS IMPLIED BY ROW:
C  We have   YL <= ax <= YU   and we define  f = a - Ve(j) for V=a(j)
C                                         (i.e., 0 in j-th coordinate)
C  Then, let X=x(j), so we have:
C           YL - max{fx} <= VX <= YU - min{fx}
C The row accumulation set YMAX = max{ax} and YMIN = min{ax}, so we can
C use these (carefully, owing to error) by netting out the extreme values
C of x(j), depending on the sign of V.
C
C FOR V > 0,    YL - (YMAX - V*U) <= V*X <= YU - (YMIN - V*L);
C FOR V < 0,    YL - (YMAX - V*L) <= V*X <= YU - (YMIN - V*U),
C  where  L <= X <= U (current bounds on X).
C
C DIVIDING BY V, WE OBTAIN NEW BOUNDS, POSSIBLY TIGHTER, AS FOLLOWS.
C FOR V > 0,   U + (YL - YMAX)/V <= X <= L + (YU - YMIN)/V
C     ===> LNEW = U + (YL - YMAX)/V and UNEW = L + (YU - YMIN)/V
C FOR V < 0,   L + (YL - YMAX)/V >= X >= U + (YU - YMIN)/V
C     ===> LNEW = U + (YU - YMIN)/V and UNEW = L + (YL - YMAX)/V
C CAUTION:  THESE BOUNDS ARE NOT RELIABLE IF SOME VALUES ARE VERY LARGE.
C SO WE'LL USE LNEW-T AND UNEW+T TO BE CONSERVATIVE.
            VT = 1.0E-4
            IF( VALUE(IYU  +ROW).LT. VINF  .AND.
     1          VALUE(IYMIN+ROW).GT.-VINFX )THEN
               V1 = VALUE(IYU + ROW) - VALUE(IYMIN + ROW)
            ELSE
               V1 = VINF
            ENDIF
            IF( VALUE(IYL  +ROW).GT.-VINF  .AND.
     1          VALUE(IYMAX+ROW).LT. VINFX )THEN
               V2 = VALUE(IYL + ROW) - VALUE(IYMAX + ROW)
            ELSE
               V2 = -VINF
            ENDIF
            IF( V.GT.0. )THEN
               IF( V1.LT. VINFX.AND.V1.GE. VTOLAB )VUNEW = VL + V1/V+VT
               IF( V2.GT.-VINFX.AND.V2.LE.-VTOLAB )VLNEW = VU + V2/V-VT
            ELSE
               IF( V1.LT. VINFX.AND.V1.GE. VTOLAB )VLNEW = VU + V1/V-VT
               IF( V2.GT.-VINFX.AND.V2.LE.-VTOLAB )VUNEW = VL + V2/V+VT
            ENDIF
C ===== TEST FOR BOUND TIGHTENING =====
            IF( VUNEW.LT.VU-VTOLAB .AND. VUNEW.LT.VINFX )THEN
C  WE REDUCED UPPER BOUND
C  (BUT VUNEW > VL NECESSARILY, SO NO NEED TO CHECK INFEASIBILITY)
               VU = VUNEW
               NEWUP = NAME(IRNAME + ROW)
            ENDIF
C
            IF( VLNEW.GT.VL+VTOLAB .AND. VLNEW.GT.-VINFX )THEN
C  WE INCREASED LOWER BOUND
C  (BUT VLNEW < VU NECESSARILY, SO NO NEED TO CHECK INFEASIBILITY)
                VL = VLNEW
                NEWLO = NAME(IRNAME + ROW)
            ENDIF
C
            VLUABS = ABS(VL) + ABS(VU)
            IF( ABS(VU-VL) .GT. VTAFIX + VTRFIX*VLUABS )GOTO 3200
C BOUNDS GOT CLOSE ENOUGH TO FIX ACTIVITY
C ...FIRST, SEE IF AN ORIGINAL BOUND IS CLOSE
C   ...TRY LOWER BOUND (COMPARE WITH VU)
            CALL GETVAL(V0,ICLBND+J)
            VLUABS = ABS(V0) + ABS(VU)
            IF( ABS(VU-V0) .LE. VTAFIX + VTRFIX*VLUABS )THEN
C BINGO...FIX X AT ITS ORIGINAL LOWER BOUND
               VX = V0
               GOTO 3150
            ENDIF
C   ...TRY UPPER BOUND (COMPARE WITH VL)
            CALL GETVAL(V0,ICUBND+J)
            VLUABS = ABS(V0) + ABS(VL)
            IF( ABS(V0-VL) .LE. VTAFIX + VTRFIX*VLUABS )THEN
C BINGO...FIX X AT ITS ORIGINAL UPPER BOUND
               VX = V0
               GOTO 3150
            ENDIF
C SEE IF LEVEL IS NEAR INTEGER (NEITHER ORGINAL BOUND IS CLOSE)
            V0 = (VL + VU)/2.
            INTGR = V0 + VTOLAB
            VX = INTGR
C SET FIXED VALUE (VX) TO MIDPOINT (V0) IF NOT CLOSE ENOUGH TO INTEGER
            VLUABS = ABS(V0) + ABS(VL)
            IF( ABS(V0-VX) .GT. VTAFIX + VTRFIX*VLUABS )VX=V0
C
3150        CONTINUE
C =FIX LEVEL OF COLUMN J = VX
            CALL REDFIX(VX,J)
            SWRED = .TRUE.
            CLIST = PREFIX
            CLIST(CPOINT:) = 'forced ='
            CALL FR2C12(CLIST(CPOINT+9:),VX)
            CLIST(CPOINT+22:) = 'by row '//NAME(IRNAME+ROW)
            IF( FREQ.EQ.1 )THEN
               CALL REDTXT(CLIST,*13000)
               FREQ = FREQ0
            ELSE IF( FREQ.GT.1 )THEN
               FREQ = FREQ-1
            ENDIF
            IF( SWRDBG )THEN
               NOTE(8:) = '3150'
               CALL FSLEN(CLIST,128,LAST)
               CALL REDTRK(CLIST,LAST,NOTE,*13000)
            ENDIF
C
C  ...AND GO FOR NEXT COLUMN
            GOTO 3500
C   NEXT ROW WITH NONZERO COEFFICIENT
3200     CONTINUE
C ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C END OF ROW LOOP...SEE IF ANY BOUND HAS CHANGED
         IF(NEWUP.NE.' ')THEN
            SWRED = .TRUE.
            CLIST = PREFIX
            CLIST(CPOINT:)='Upper bound decreased to'
            CALL FR2C12(CLIST(CPOINT+25:),VU)
            CLIST(CPOINT+39:) = 'by row '//NEWUP
            IF( FREQ.EQ.1 )THEN
               CALL REDTXT(CLIST,*13000)
               FREQ = FREQ0
            ELSE IF( FREQ.GT.1 )THEN
               FREQ = FREQ-1
            ENDIF
            IF( SWRDBG )THEN
               NOTE(8:) = '3150'
               CALL FSLEN(CLIST,128,LAST)
               CALL REDTRK(CLIST,LAST,NOTE,*13000)
            ENDIF
            VALUE(IXU + J) = VU
         ENDIF
C
         IF( NEWLO.NE.' ' )THEN
            SWRED = .TRUE.
            CLIST = PREFIX
            CLIST(CPOINT:) = 'Lower bound increased to'
            CALL FR2C12(CLIST(CPOINT+25:),VL)
            CLIST(CPOINT+39:) = 'by row '//NEWLO
            IF( FREQ.EQ.1 )THEN
               CALL REDTXT(CLIST,*13000)
               FREQ = FREQ0
            ELSE IF( FREQ.GT.1 )THEN
               FREQ = FREQ-1
            ENDIF
            IF( SWRDBG )THEN
               NOTE(8:) = '> 3200'
               CALL FSLEN(CLIST,128,LAST)
               CALL REDTRK(CLIST,LAST,NOTE,*13000)
            ENDIF
            VALUE(IXL + J) = VL
         ENDIF
C ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         IF(NAME01.EQ.' ')GOTO 3400
C SEE IF COLUMN IS BINARY
         IF(INDEX(ICLBND+J).NE.0.OR.INDEX(ICUBND+J).NE.1)GOTO 3400
C ITS BOUNDS ARE 0-1, SO CHECK ITS NAME (MUST MATCH NAME01)
            CALL FMATCH(NAME01,CNAME,'*',SW)
            IF(.NOT.SW)GOTO 3400
C COLUMN IS BINARY, SO TEST BOUNDS
            IF(VL.GT.VTOLAB .AND. VU.LT.1.0-VTOLAB)THEN
               CLIST = ' Logical infeasibility detected with binary'
     1               //' activity '//CNAME(:NAMELN)//'...XL ='
               CALL FSLEN(CLIST,128,LAST)
               CALL FR2C12(CLIST(LAST+2:),VL)
               CLIST(LAST+15:) = 'and XU ='
               CALL FR2C12(CLIST(LAST+25:),VU)
               CALL REDTXT(CLIST,*13000)
               RETURN 1
            ENDIF
            CLIST = PREFIX
            IF(VL.GT.0)THEN
               SWRED = .TRUE.
               CLIST(CPOINT:) = ' (binary) fixed = 1'
               IF( FREQ.EQ.1 )THEN
                  CALL REDTXT(CLIST,*13000)
                  FREQ = FREQ0
               ELSE IF( FREQ.GT.1 )THEN
                  FREQ = FREQ-1
               ENDIF
               IF( SWRDBG )THEN
                  NOTE(8:) = '> 3200.'
                  CALL FSLEN(CLIST,128,LAST)
                  CALL REDTRK(CLIST,LAST,NOTE,*13000)
               ENDIF
               VALUE(IXL + J) = 1.
            ELSE IF(VU.LT.1)THEN
               SWRED = .TRUE.
               CLIST(CPOINT:) = ' (binary) fixed = 0'
               IF( FREQ.EQ.1 )THEN
                  CALL REDTXT(CLIST,*13000)
                  FREQ = FREQ0
               ELSE IF( FREQ.GT.1 )THEN
                  FREQ = FREQ-1
               ENDIF
               IF( SWRDBG )THEN
                  NOTE(8:) = '> 3200.'
                  CALL FSLEN(CLIST,128,LAST)
                  CALL REDTRK(CLIST,LAST,NOTE,*13000)
               ENDIF
               VALUE(IXU + J) = 0.
            ENDIF
            GOTO 3490
C ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
3400     CONTINUE
         IF(NAMINT.EQ.' ')GOTO 3500
C SEE IF COLUMN IS INTEGER
         CALL FMATCH(NAMINT,CNAME,'*',SW)
         IF(.NOT.SW)GOTO 3500
         CLIST = PREFIX
C COLUMN IS INTEGER...TRUNCATE BOUNDS
         IF(VL.GT.-32000.)THEN
            INTVAL=VL-VTOLAB
            IF(FLOAT(INTVAL).GT.VL)THEN
C ...LOWER BOUND IS INCREASED TO NEXT INTEGER VALUE
               VL = FLOAT(INTVAL)
               CLIST(CPOINT:) = ' Integer restriction '
     1                       // 'increased lower bound to'
               CALL FSLEN(CLIST,128,LAST)
               CALL FR2C12(CLIST(LAST+2:),VL)
               SWRED = .TRUE.
               IF( FREQ.EQ.1 )THEN
                  CALL REDTXT(CLIST,*13000)
                  FREQ = FREQ0
               ELSE IF( FREQ.GT.1 )THEN
                  FREQ = FREQ-1
               ENDIF
               IF( SWRDBG )THEN
                  NOTE(8:) = '> 3400'
                  CALL FSLEN(CLIST,128,LAST)
                  CALL REDTRK(CLIST,LAST,NOTE,*13000)
               ENDIF
               VALUE(IXL+J) = VL
            ENDIF
         ENDIF
C
         IF(VU.LT.32000.)THEN
            INTVAL=VU+VTOLAB
            IF(FLOAT(INTVAL).LT.VU)THEN
C ...UPPER BOUND IS DECREASED TO NEXT INTEGER VALUE
               VU = FLOAT(INTVAL)
               CLIST = PREFIX
               CLIST(CPOINT:) = ' Integer restriction decreased'
     1            //' upper bound to'
               CALL FSLEN(CLIST,128,LAST)
               CALL FR2C12(CLIST(LAST+2:),VU)
               SWRED = .TRUE.
               IF( FREQ.EQ.1 )THEN
                  CALL REDTXT(CLIST,*13000)
                  FREQ = FREQ0
               ELSE IF( FREQ.GT.1 )THEN
                  FREQ = FREQ-1
               ENDIF
               IF( SWRDBG )THEN
                  NOTE(8:) = '> 3400.'
                  CALL FSLEN(CLIST,128,LAST)
                  CALL REDTRK(CLIST,LAST,NOTE,*13000)
               ENDIF
               VALUE(IXU+J) = VU
            ENDIF
         ENDIF
C TEST FOR FEASIBILITY (MAY HAVE INCREASED LOWER BOUND ABOVE ITS UPPER BOUND)
         IF(VL.GT.VU)THEN
            CLIST = ' !Integer infeasibility detected with '//CNAME
            CALL REDTXT(CLIST,*13000)
            IF( OUTPUT.NE.TTYOUT )WRITE(*,1)CLIST
            RETURN 1
         ENDIF
C  PREPARE FOR NEXT COLUMN
3490     IF(SWRDBG.AND.PAUSE.LE.0)CALL GDEBUG
C  NEXT COLUMN
C  ~~~~~~~~~~~
3500  CONTINUE
C ~~~~~~~~~~~~~
      IF(SWRDBG)THEN
         PRINT *,' .....READY TO LEAVE REDBIN WITH SWRED=',SWRED
         PAUSE = PAUSE - 1
         IF(PAUSE.LE.0)CALL GDEBUG
      ENDIF
      RETURN
C
13000 IF(SWRDBG)PRINT *,' FROM REDBIN...'
      RETURN 2
C
C ** REDBIN ENDS HERE
      END
      SUBROUTINE REDLGL(CLIST,JLIST,VLIST,MAXJV,NAME01,NAMINT,NUMLGL,
     1                  RCODE)
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
C This produces implications for binary (0-1) variables.
C
      CHARACTER*128 CLIST
      INTEGER       JLIST(MAXJV)
      REAL          VLIST(MAXJV)
C                         :...>= NROWS
      CHARACTER*(*) NAME01,NAMINT
C
C  NAME01 = Name mask for binary activities
C  NAMINT = Name mask for integer activities
C  NUMLGL = NUMBER OF LOGICAL IMPLICATIONS WRITTEN (TO OUTPUT)
C For example, if there is a degree-2 inequality
C (representing IF...THEN), the setting of one variable's value will
C affect the other's.  If we have x - y <= 0, then x=1===>y=1
C (contrapositively, y=0===>x=0).
C ...See REDUCE.DOC for further information.
C ::::::::::::::::::::::::::::::: NOTE ::::::::::::::::::::::::::::::
C : USING COLUMN BOUNDS IN VALUE(IXL), VALUE(IXU) AND ROW BOUNDS IN :
C :     VALUE(IYL), VALUE(IYU)....REDUCE MUST HAVE BEEN CALLED      :
C :     FOR AT LEAST 1 ITERATION, SO THE REDUCE DATA ARE SET        :
C :.................................................................:
C We shall use the REDUCE COMMON, so caller must have set pointers,
C etc.  It is best to call this after at least 1 iteration of main
C REDUCE, which includes stronger tests for binaries.
C
C LOCAL
      LOGICAL*1     SW
      CHARACTER*1   CHAR
      CHARACTER*16  RNAME,CNAME
      CHARACTER*64  PRED
C :::::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::::
C INITIALIZE
      NUMLGL = 0
      SWRED = .FALSE.
C             :...INITIALIZES WHETHER WE FIX ANY
      CLIST = 'BEGIN LOGICAL IMPLICATIONS TEST'
      CALL FCENTR(CLIST,FIRST,LAST)
      CALL REDTXT(CLIST,*1300)
C
C  ::: TOP OF MAIN LOOP OVER ROWS :::
C
      DO 900 ROW=1,NROWS
         CALL GETMAP('ROW ',ROW,SW)
         IF(.NOT.SW)GOTO 900
C SET ROW'S BOUNDS
         VL = VALUE(IYL+ROW)
         VU = VALUE(IYU+ROW)
         IF(VL.LE.-VINF .AND. VU.GE.VINF)GOTO 900
C ROW IS NOT FREE...GET ITS NONZEROES
         CALL GETROW(ROW,JLIST,VLIST,MAXJV,NZ,RCODE)
         IF(NZ.EQ.0)GOTO 900
C ROW IS IN SUBMATRIX AND NOT NULL
         IF(RCODE.NE.0)THEN
C SKIP THIS ROW (NOT ENOUGH SPACE IN LISTS)
            RCODE=0
            GOTO 900
         ENDIF
C INITIALIZE TALLIES (DO NOT TRUST VALUE(IYMIN), VALUE(IYMAX))
         VMIN = 0.
         VMAX = 0.
         N01  = 0
         CALL GETNAM('ROW ',ROW,RNAME)
C
C  ::: LOOP OVER NONZEROES :::
         DO 500 K=1,NZ
            COL = JLIST(K)
            CALL GETMAP('COL ',COL,SW)
            IF(.NOT.SW)GOTO 500
C COLUMN (COL) IS IN SUBMATRIX...SET ITS BOUNDS
            VLC = VALUE(IXL+COL)
            VUC = VALUE(IXU+COL)
C ...AND COEFFICIENT
            VAL = VLIST(K)
            IF(VLC.GE.VUC)THEN
C ACTIVITY LEVEL IS FIXED...JUST TALLY
               VMIN = VMIN + VAL*VLC
               VMAX = VMAX + VAL*VLC
               GOTO 500
            ENDIF
C
            IF(ABS(VLC).GT.VTOLAB .OR. ABS(VUC-1.).GT.VTOLAB)
C THIS ROW HAS A NON 0-1 VARIABLE...GO TO NEXT ROW
     1         GOTO 900
C
C MAKE SURE 0-1 IS BINARY (NAME MUST MATCH)
            CALL GETNAM('COL ',COL,CNAME)
            SW = NAME01.EQ.' '
            IF(.NOT.SW)CALL FMATCH(NAME01,CNAME,'*',SW)
            IF(.NOT.SW)THEN
C ...ALLOW INTEGER ACTIVITY SINCE BOUNDS ARE 0,1
               IF(NAMINT.NE.' ')CALL FMATCH(NAMINT,CNAME,'*',SW)
               IF(.NOT.SW)GOTO 900
            ENDIF
C
C  NOW COL IS A BINARY (0-1) VARIABLE WHOSE VALUE IS NOT FIXED
C
C GET BINARY'S BEST 0-1 SETTING FOR VMIN AND VMAX
            IF(VAL.LT.0.)THEN
C  SET X=0 FOR VMAX AND X=1 FOR VMIN
               VMIN = VMIN + VAL
            ELSE
C  SET X=0 FOR VMIN AND X=1 FOR VMAX
               VMAX = VMAX + VAL
            ENDIF
C FINALLY, PUT BINARY IN LIST (OVERWRITE FROM GETROW)
            N01 = N01+1
            JLIST(N01) = COL
            VLIST(N01) = VAL
C NEXT COLUMN WITH NONZERO IN ROW
500      CONTINUE
C    ::: END LOOP OVER NONZEROES OF ROW :::
C
         IF(N01.LE.1)GOTO 900
C
C  ::: LOOP OVER BINARY VARIABLES TO TEST IMPLICATIONS OF 0,1 :::
         DO 700 K=1,N01-1
            COL = JLIST(K)
            VAL = VLIST(K)
            IF(VAL.LT.0.)THEN
C WE SET X=1 IN VMIN AND X=0 IN VMAX, SO ADJUST ONLY THOSE
               VMIN0 = VMIN - VAL
               VMAX0 = VMAX
               VMIN1 = VMIN
               VMAX1 = VMAX + VAL
            ELSE
C WE SET X=0 IN VMIN AND X=1 IN VMAX, SO ADJUST ONLY THOSE
               VMIN0 = VMIN
               VMAX0 = VMAX - VAL
               VMIN1 = VMIN + VAL
               VMAX1 = VMAX
            ENDIF
C
C NOW VMIN0=VMIN: COL=0,  VMIN1=VMIN: COL=1,
C     VMAX0=VMAX: COL=0,  VMAX1=VMAX: COL=1
C
C USE SW TO INDICATE IF WE FIX COL
            IF(VMIN0.GT.VU .OR. VMAX0.LT.VL)THEN
C COL MUST = 1 (INFEASIBLE TO SET =0)
               CLIST = '* Column '//CNAME(:NAMELN)//' fixed=1 by row '
     1                 //RNAME
               CALL REDTXT(CLIST,*1300)
               VALUE(IXL+COL)=1.
               SWRED = .TRUE.
               SW    = .TRUE.
            ELSE
               SW = .FALSE.
            ENDIF
            IF(VMIN1.GT.VU .OR. VMAX1.LT.VL)THEN
C COL MUST = 0
               CLIST = '* Column '//CNAME(:NAMELN)//' fixed=0 by row '
     1                 //RNAME
               CALL REDTXT(CLIST,*1300)
               VALUE(IXU+COL) = 0.
               VMIN = VMIN0
               VMAX = VMAX0
C
               IF(SW)THEN
C OH, OH, WE ALREADY FIXED IT = 0
                  CALL GETROW(ROW,JLIST,VLIST,MAXJV,NZ,RCODE)
                  CLIST=' ** LOGICAL INFEASIBILITY DETECTED WITH ROW '
     1            //RNAME
                  CALL REDTXT(CLIST,*1300)
                  CLIST = ' COLUMN'
                  CLIST(20:)='COEFFICIENT'
                  CLIST(36:)='LO BOUND'
                  CLIST(51:)='UP BOUND'
                  CALL REDTXT(CLIST,*1300)
                  DO 711 KKK=1,64
711               CLIST(KKK:KKK)='-'
                  CALL REDTXT(CLIST,*1300)
C
                  DO 713 KKK=1,NZ
                     COL = JLIST(KKK)
                     CALL GETNAM('COL ',COL,CLIST)
                     CALL FR2C12(CLIST(20:),VLIST(KKK))
                     CALL FR2C12(CLIST(35:),VALUE(IXL+COL))
                     CALL FR2C12(CLIST(50:),VALUE(IXU+COL))
                     CALL REDTXT(CLIST,*1300)
713               CONTINUE
C
                  DO 714 KKK=1,64
714               CLIST(KKK:KKK)='-'
                  CALL REDTXT(CLIST,*1300)
C
                  CLIST = '       ROW RANGE:'
                  CALL FR2C12(CLIST(20:),VL)
                  CLIST(35:) = 'TO'
                  CALL FR2C12(CLIST(40:),VU)
                  CALL REDTXT(CLIST,*1300)
C
                  CLIST = '  COMPUTED RANGE:'
                  CALL FR2C12(CLIST(20:),VMIN)
                  CLIST(35:) = 'TO'
                  CALL FR2C12(CLIST(40:),VMAX)
                  CALL REDTXT(CLIST,*1300)
                  RCODE = -1
                  RETURN
               ENDIF
C
C UPDATE FIXED VALUES OF VMIN, VMAX
               VMIN = VMIN1
               VMAX = VMAX1
               SWRED = .TRUE.
C ...AND GO TO NEXT NONZERO
               GOTO 700
            ENDIF
C
C         TEST FOR IMPLICATIONS OF COL=0,1
C
            IF(VMIN0.LT.VU .AND. VMAX0.GT.VL .AND.
     1         VMIN1.LT.VU .AND. VMAX1.GT.VL)GOTO 700
C
C ONE OF THE ABOVE YIELDS IMPLICATION(S):
C
C     VMIN0 >= VU ..... 0 ---> VMIN values of others
C     VMAX0 <= VL ..... 0 ---> VMAX values of others
C     VMIN1 >= VU ..... 1 ---> VMIN values of others
C     VMAX1 >= VL ..... 1 ---> VMAX values of others
C
            CLIST = '** FROM ROW '//RNAME
            CALL REDTXT(CLIST,*1300)
C
C USE SW TO DISTINGUISH VMIN (TRUE) OR VMAX (FALSE)
C
            IF(VMIN0.GE.VU)THEN
               CHAR = '0'
               SW = .TRUE.
            ELSE IF(VMAX0.LE.VL)THEN
               CHAR = '0'
               SW = .FALSE.
            ELSE IF(VMIN1.GE.VU)THEN
               CHAR = '1'
               SW = .TRUE.
            ELSE
               CHAR = '1'
               SW = .FALSE.
            ENDIF
C
C NOW CHAR IS VALUE OF PREDECESSOR AND SW INDICATES OTHER EXTREMES
C
            CALL GETNAM('COL ',COL,CNAME)
            PRED = ' IF '//CNAME(:NAMELN)//' = '//CHAR//' THEN'
            CALL FSLEN(PRED,50,PMTAB)
            PMTAB= PMTAB+2
C               :....BEGINS CONSEQUENT FIELD IN IMPLICATION
C
C GO BACK OVER NONZEROES TO GET CONSEQUENCES
            DO 600 KK=K+1,N01
               SUCCOL = JLIST(KK)
               IF(VALUE(IXL+SUCCOL).GE.VALUE(IXU+SUCCOL))GOTO 600
C BINARY (SUCCOL) IS NOT FIXED, SO SET ITS CONSEQUENCE
               VALSUC = VLIST(KK)
C
               IF(VALSUC.LT.0.)THEN
C SUCCESSOR WAS SET = 1 IN VMIN AND = 0 IN VMAX
                  IF(SW)THEN
                     CHAR = '1'
                  ELSE
                     CHAR = '0'
                  ENDIF
               ELSE
C SUCCESSOR WAS SET = 0 IN VMIN AND = 1 IN VMAX
                  IF(SW)THEN
                     CHAR = '0'
                  ELSE
                     CHAR = '1'
                  ENDIF
               ENDIF
C
               CLIST = PRED
C NOW SUCCESSOR OF IMPLICATION
               CALL GETNAM('COL ',SUCCOL,CLIST(PMTAB:))
               CLIST(PMTAB+NAMELN+1:) = '= '//CHAR
               CALL REDTXT(CLIST,*1300)
               NUMLGL = NUMLGL+1
600         CONTINUE
C     ::: END (INNER-MOST) LOOP OF CONSEQUENCES OF PRED COLUMN :::
C
C NEXT NONZERO OF ROW
700      CONTINUE
C     ::: END LOOP OVER BINARIES IN ROW :::
C
C NEXT ROW
900   CONTINUE
C   ::: END MAIN LOOP OVER ROWS :::
C
C LET CALLER GIVE MESSAGE AND CLOSE OUTPUT
      SW = SWRED
      RETURN
C
C ERROR RETURNS
1300  CONTINUE
      PRINT *,' ** I/O ERROR ON OUTPUT FILE'
      RCODE = 10
      RETURN
C
C ** REDLGL ENDS HERE
      END
      SUBROUTINE REDTXT(CLIST,*)
C     =================
C
C This sends CLIST to OUTPUT, calling FTEXT.
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*128 CTEMP
C  :::::::::::::::::: BEGIN ::::::::::::::
      CTEMP = CLIST
      CALL FSLEN(CTEMP,128,LAST)
      LINE = 1
      CALL FTEXT(CTEMP,LAST,1,0,LINE,'CLEAR ',*13)
      RETURN
13    RETURN 1
C
C ** REDTXT ENDS HERE
      END
