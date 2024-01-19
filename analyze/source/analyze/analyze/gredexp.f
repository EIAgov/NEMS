C               ::: GREDEXP.FOR  8-18-95 :::
C
C Earlier dates deleted
C        2-04-94...Added call to REDTRK
C        5-15-95...Restructured (8-14-95)
C
C This contains REDUCE routine REDEXP...expensive tests.
C
      SUBROUTINE REDEXP(CLIST,ZALPHA,NALPHA,*,*)
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
C This applies more expensive reduction tests (not iterative).
C    First  alternate return is for infeasibility.
C    Second alternate return is for i/o error.
C Tests are:  Row degree
C             Univariate column extremes
C             Ranging basic variables (if basis is not logical)
C
      CHARACTER*128    CLIST
      DOUBLE PRECISION ZALPHA(NALPHA)
C LOCAL
      CHARACTER*64  NOTE
      CHARACTER*16  CNAME
      CHARACTER*12  STRVAL
      CHARACTER*4   ROWCOL,STR4
      CHARACTER*1   CHAR
      LOGICAL*1     SW
C ::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      CALL FCLOCK(TIME0)
1     FORMAT(1X,A79)
      NOTE = 'REDEXP'
      SWRED = .FALSE.
      IV0   = IVFREE
      CALL REDINF
C SET MYOPIC ROW RANGES (DO NOT TRUST CALLER TO HAVE DONE THIS)
      DO 100 I=1,NROWS
         VALUE(IYMIN+I) = 0.
         VALUE(IYMAX+I) = 0.
100   CONTINUE
C
      DO 300 J=1,NCOLS
         IF( INDEX(ICSTAT+J).GT.0 )GOTO 300
         VL = VALUE(IXL+J)
         VU = VALUE(IXU+J)
         I1 = INONZ + 2*BASE(ICINFO + J-1)
         I2 = INONZ + 2*BASE(ICINFO + J) - 1
         DO 200 I=I1,I2,2
            ROW = INDEX(I)
            CALL GETVAL(VCOEF,I+1)
            IF( ABS(VCOEF).LT.VTOL0C )GOTO 200
            IF( VCOEF.GT.0. )THEN
               IF( VALUE(IYMIN+ROW).GT.-VINF .AND. VL.GT.-VINF )THEN
                  VALUE(IYMIN+ROW) = VALUE(IYMIN+ROW) + VCOEF*VL
               ELSE
                  VALUE(IYMIN+ROW) = -VINF
               ENDIF
               IF( VALUE(IYMAX+ROW).LT.VINF .AND. VU.LT.VINF )THEN
                  VALUE(IYMAX+ROW) = VALUE(IYMAX+ROW) + VCOEF*VU
               ELSE
                  VALUE(IYMAX+ROW) = VINF
               ENDIF
            ELSE
               IF( VALUE(IYMIN+ROW).GT.-VINF .AND. VU.LT.VINF )THEN
                  VALUE(IYMIN+ROW) = VALUE(IYMIN+ROW) + VCOEF*VU
               ELSE
                  VALUE(IYMIN+ROW) = -VINF
               ENDIF
               IF( VALUE(IYMAX+ROW).LT.VINF .AND. VL.GT.-VINF )THEN
                  VALUE(IYMAX+ROW) = VALUE(IYMAX+ROW) + VCOEF*VL
               ELSE
                  VALUE(IYMAX+ROW) = VINF
               ENDIF
            ENDIF
200      CONTINUE
300   CONTINUE
C =====================================================
C ROW DEGREE TEST...LOOK FOR ROWS WITH 1 OR 2 NONZEROES
C =====================================================
      IF( IVFREE+4.GT.MAXVAL )GOTO 2000
C                :...USING 4 WORDS IN VALUE (EQUIVALENCED TO I4VAL)
      CLIST = 'Performing row degree test'
      IF( SWMSG .OR. SWRDBG )THEN
         IF( OUTPUT.NE.TTYOUT )WRITE(*,1,ERR=13000)CLIST
         CALL REDTXT(CLIST,*13000)
      ENDIF
      CALL FCLOCK(TIME)
      ELAPSE = TIME-TIME0
      IF( ELAPSE.GE.MAXTIM )CALL FLTIME(ELAPSE,MAXTIM,*9000)
C
C  ::: LOOP OVER ROWS :::
      DO 1000 ROW=1,NROWS
         IF( INDEX(IRSTAT+ROW).GT.0 )GOTO 1000
         CALL FCLOCK(TIME)
         ELAPSE = TIME-TIME0
         IF( ELAPSE.GT.MAXTIM )THEN
            PRINT *,' Looking at row ',NAME(IRNAME+ROW)
            CALL FLTIME(ELAPSE,MAXTIM,*9000)
         ENDIF
         CALL GETROW(ROW,I4VAL(IVFREE+1),VALUE(IVFREE+3),2,NZ,RCODE)
         IF( RCODE.GT.0 )THEN
C > 2 NONZEROES IN SUBMATRIX
            RCODE = 0
            GOTO 1000
         ENDIF
         IF( NZ.EQ.0 )GOTO 1000
C ROW IS IN SUBMATRIX AND HAS 1 OR 2 NONZEROES
         VLROW = VALUE(IYL+ROW)
         VUROW = VALUE(IYU+ROW)
         COL   = I4VAL(IVFREE+1)
         VCOEF = VALUE(IVFREE+3)
         CALL GETBND('COL ',COL,VL,VU)
         IF( VL.GE.VU )THEN
            IF( NZ.EQ.1 )GOTO 1000
C COL IS FIXED...ADJUST ROW BOUNDS AND LOOK AT 2ND COL AS ONLY COL
            IF( VLROW.GT.-VINF )VLROW = VLROW - VCOEF*VL
            IF( VUROW.LT. VINF )VUROW = VUROW - VCOEF*VL
            COL   = I4VAL(IVFREE+2)
            VCOEF = VALUE(IVFREE+4)
            CALL GETBND('COL ',COL,VL,VU)
         ELSE IF( NZ.EQ.2 )THEN
            GOTO 600
         ENDIF
500      CONTINUE
C ONE COL...VLROW <= VCOEF*X <= VUROW AND VL <= X <= VU
         VLO = VL
         VUP = VU
         IF( VCOEF.GT.VTOL0C )THEN
C VCOEF > 0 ===> VLROW/VCOEF <= X <= VUROW/VCOEF
            IF( VUROW.LT. VINF )VUP = VUROW/VCOEF
            IF( VLROW.GT.-VINF )VLO = VLROW/VCOEF
         ELSE IF( VCOEF.LT.-VTOL0C )THEN
C VCOEF < 0 ===> VLROW/VCOEF >= X >= VUROW/VCOEF
            IF( VLROW.GT.-VINF )VUP = VLROW/VCOEF
            IF( VUROW.LT. VINF )VLO = VUROW/VCOEF
         ELSE
            GOTO 1000
         ENDIF
         CNAME = NAME(ICNAME+COL)
         IF( VUP.LE.VLO + VTAFIX + VTRFIX*ABS(VLO) )THEN
C COL FIXED BY ROW
            NRFORC = NRFORC + 1
            VX = (VLO + VUP)/2.
            CALL REDFIX(VX,COL)
            CALL FR2CLJ(STRVAL,VX,L)
            CLIST = ' Activity '//CNAME(:NAMELN)//' fixed = '//
     1              STRVAL//' by row '//NAME(IRNAME+ROW)
            IF( FREQ.EQ.1 )THEN
               CALL REDTXT(CLIST,*13000)
               FREQ = FREQ0
            ELSE IF( FREQ.GT.1 )THEN
               FREQ = FREQ-1
            ENDIF
            IF( SWRDBG )THEN
               NOTE(8:) = '> 500'
               CALL FSLEN(CLIST,128,LAST)
               CALL REDTRK(CLIST,LAST,NOTE,*13000)
            ENDIF
         ELSE
            IF( VUP.LT.VU - VTOLAB )THEN
               VALUE(IXU+COL) = VUP
               CALL FR2CLJ(STRVAL,VUP,L)
               CLIST = ' Activity '//CNAME(:NAMELN)//
     1                 ' upper bound decreased to '//STRVAL//
     2                 ' by row '//NAME(IRNAME+ROW)
               IF( FREQ.EQ.1 )THEN
                  CALL REDTXT(CLIST,*13000)
                  FREQ = FREQ0
               ELSE IF( FREQ.GT.1 )THEN
                  FREQ = FREQ-1
               ENDIF
               IF( SWRDBG )THEN
                  NOTE(8:) = '>> 500'
                  CALL FSLEN(CLIST,128,LAST)
                  CALL REDTRK(CLIST,LAST,NOTE,*13000)
               ENDIF
            ENDIF
            IF( VLO.GT.VL + VTOLAB )THEN
               VALUE(IXL+COL) = VLO
               CALL FR2CLJ(STRVAL,VLO,L)
               CLIST = ' Activity '//CNAME(:NAMELN)//
     1              ' lower bound increased to '//STRVAL//
     2              ' by row '//NAME(IRNAME+ROW)
               IF( FREQ.EQ.1 )THEN
                  CALL REDTXT(CLIST,*13000)
                  FREQ = FREQ0
               ELSE IF( FREQ.GT.1 )THEN
                  FREQ = FREQ-1
               ENDIF
               IF( SWRDBG )THEN
                  NOTE(8:) = '>>> 500'
                  CALL FSLEN(CLIST,128,LAST)
                  CALL REDTRK(CLIST,LAST,NOTE,*13000)
               ENDIF
            ENDIF
         ENDIF
         GOTO 1000
C        =========
600      CONTINUE
C 2 COLS...SEE IF 2ND ONE IS FIXED
         COL2   = I4VAL(IVFREE+2)
         VCOEF2 = VALUE(IVFREE+4)
         IF( ABS(VCOEF2).LT.VTOLAB )GOTO 500
         CALL GETBND('COL ',COL2,VL2,VU2)
         IF( VL2.GE.VU2 )THEN
C ...IT IS, SO ADJUST ROW BOUNDS AND XFER TO ONE COL
            IF( VLROW.GT.-VINF )VLROW = VLROW - VCOEF*VL2
            IF( VUROW.LT. VINF )VUROW = VUROW - VCOEF*VL2
            GOTO 500
         ENDIF
C NEITHER OF THE 2 COLS IS FIXED, AND VLROW <= V1*X1 + V2*X2 <= VUROW
         IF( VCOEF*VCOEF2.GT.0. )GOTO 1000
C V1 AND V2 ARE OF OPPOSITE SIGNS...FOR SIMPLICITY, MAKE V1 > 0
         IF( VCOEF2.GT.0. )THEN
            J      = COL
            COL    = COL2
            COL2   = J
            V      = VL
            VL     = VL2
            VL2    = V
            V      = VU
            VU     = VU2
            VU2    = V
            V1     = VCOEF2
            V2     = -VCOEF
         ELSE
            V1     = VCOEF
            V2     = -VCOEF2
         ENDIF
C NOW WE HAVE VLROW <= V1*X1 - V2*X2 <= VUROW (Vi > 0)
C             VLi <= Xi <= VUi                (i=1,2)
C (NOTE: VL,VU = VL1,VU1)
         IF( VLROW.GT.-VINF .AND. VL2.GT.-VINF )THEN
            VLO = (VLROW + V2*VL2)/V1
            IF( VLO.GT.VL + VTOLAB )VL = VLO
         ENDIF
         IF( VUROW.LT.VINF .AND. VU2.LT.VINF )THEN
            VUP = (VUROW + V2*VU2)/V1
            IF( VUP.LT.VU - VTOLAB )VU = VUP
         ENDIF
         IF( VLROW.GT.-VINF .AND. VU.LT.VINF )THEN
            VUP = (V1*VU - VLROW)/V2
            IF( VUP.LT.VU2 - VTOLAB )VU2 = VUP
         ENDIF
         IF( VUROW.LT.VINF .AND. VL.GT.-VINF )THEN
            VLO = (V1*VL - VUROW)/V2
            IF( VLO.GT.VL2 + VTOLAB )VL2 = VLO
         ENDIF
         IF( VL .GT.VALUE(IXL+COL ) .OR. VU .LT.VALUE(IXU+COL ) .OR.
     1       VL2.GT.VALUE(IXL+COL2) .OR. VU2.LT.VALUE(IXU+COL2) )THEN
C AT LEAST ONE BOUND WAS TIGHTENED
            SWRED = .TRUE.
            IF( VL.GE.VU + VTAFIX + VTRFIX*ABS(VL) )THEN
               VX = (VL+VU)/2
               CALL REDFIX(VX,COL)
            ELSE
               VALUE(IXL+COL) = VL
               VALUE(IXU+COL) = VU
            ENDIF
            IF( VL2.GE.VU2 + VTAFIX + VTRFIX*ABS(VL2) )THEN
               VX = (VL2+VU2)/2
               CALL REDFIX(VX,COL2)
            ELSE
               VALUE(IXL+COL2) = VL2
               VALUE(IXU+COL2) = VU2
            ENDIF
            CNAME = NAME(IRNAME+ROW)
            CLIST = ' Row '//CNAME(:NAMELN)//':'
            CNAME = NAME(ICNAME+COL)
            IF( VLROW.GE.VUROW .OR. VLROW.LT.-VINF .OR.
     1                              VUROW.GT. VINF )THEN
               IF( ABS(V1-1).LE.VTOLAB )THEN
                  CLIST(NAMELN+8:) = CNAME
               ELSE
                  CALL FR2CLJ(STRVAL,V1,L)
                  CLIST(NAMELN+8:) = STRVAL(:L)//' '//CNAME
               ENDIF
               CALL FSLEN(CLIST,60,LC)
               IF( ABS(V2-1).LE.VTOLAB )THEN
                  CLIST(LC+3:) = '-'
               ELSE
                  CALL FR2CLJ(STRVAL,V2,L)
                  CLIST(LC+3:) = '- '//STRVAL
               ENDIF
               CALL FSLEN(CLIST,80,LC)
               CNAME = NAME(ICNAME+COL2)
               CLIST(LC+2:) = ' '//CNAME
               CALL FR2CLJ(STRVAL,VUROW,L)
               CALL FSLEN(CLIST,90,L)
               IF( VLROW.GE.VUROW )THEN
                  CLIST(L+2:) = '= '//STRVAL
               ELSE IF( VLROW.LE.-VINF )THEN
                  CLIST(L+2:) = '<= '//STRVAL
               ELSE
                  CLIST(L+2:) = '>= '//STRVAL
               ENDIF
            ELSE
C ROW RANGE
               CALL FR2CLJ(STRVAL,VLROW,L)
               CLIST(NAMELN+8:) = STRVAL(:L)//' <='
               CALL FSLEN(CLIST,40,LC)
               IF( ABS(V1-1).LE.VTOLAB )THEN
                  CLIST(LC+2:) = CNAME
               ELSE
                  CALL FR2CLJ(STRVAL,V1,L)
                  CLIST(LC+2:) = STRVAL(:L)//' '//CNAME
               ENDIF
               CALL FSLEN(CLIST,90,LC)
               IF( ABS(V2-1).LE.VTOLAB )THEN
                  CLIST(LC+3:) = '-'
               ELSE
                  CALL FR2CLJ(STRVAL,V2,L)
                  CALL FSLEN(CLIST,60,LC)
                  CLIST(LC+3:) = '- '//STRVAL
               ENDIF
               CALL FSLEN(CLIST,80,LC)
               CNAME = NAME(ICNAME+COL2)
               CLIST(LC+2:) = ' '//CNAME
               CALL FR2CLJ(STRVAL,VUROW,L)
               CALL FSLEN(CLIST,100,L)
               CLIST(L+2:) = '<= '//STRVAL
            ENDIF
            IF( FREQ.EQ.1 )THEN
               CALL REDTXT(CLIST,*13000)
               FREQ = FREQ0
            ELSE IF( FREQ.GT.1 )THEN
               FREQ = FREQ-1
            ENDIF
            IF( SWRDBG )THEN
               NOTE(8:) = '> ROW RANGE'
               CALL FSLEN(CLIST,128,LAST)
               CALL REDTRK(CLIST,LAST,NOTE,*13000)
            ENDIF
            CLIST = '   implies strengthening:'
            CALL FSLEN(CLIST,25,LC)
            LC = LC+2
            CNAME = NAME(ICNAME+COL)
            CALL FR2CLJ(STRVAL,VL,L)
            CALL FSLEN(CNAME,16,L)
            SW = .FALSE.
            IF( VL.GE.VU )THEN
               CLIST(LC+2:) = CNAME(:L)//' ='
               CALL FSLEN(CLIST,50,LC)
               CALL FR2CLJ(STRVAL,VL,L)
               CLIST(LC+2:) = STRVAL
               SW = .TRUE.
            ELSE
               CALL GETBND('COL ',COL,VLO,VUP)
               IF( VL.GT.VLO )THEN
                  CALL FR2CLJ(STRVAL,VL,L)
                  CLIST(LC+2:) = STRVAL(:L)//' <= '//CNAME
                  CALL FSLEN(CLIST,80,LC)
                  SW = .TRUE.
               ENDIF
               IF( VU.LT.VUP )THEN
                  CALL FR2CLJ(STRVAL,VU,L)
                  CLIST(LC+2:) = ' <= '//STRVAL(:L)
                  CALL FSLEN(CLIST,90,LC)
                  IF( VL.LT.VLO )CLIST(LC+2:) = CNAME
                  SW = .TRUE.
               ENDIF
            ENDIF
            CALL FSLEN(CLIST,80,LC)
            CNAME = NAME(ICNAME+COL2)
            CALL FR2CLJ(STRVAL,VL2,L)
            CALL FSLEN(CNAME,16,L)
            IF( VL2.GE.VU2 )THEN
               IF( SW )THEN
                  CLIST(LC+3:) = 'and'
                  LC = LC + 7
               ENDIF
               CLIST(LC+2:) = CNAME(:L)//' ='
               CALL FSLEN(CLIST,80,LC)
               CALL FR2CLJ(STRVAL,VL2,L)
               CLIST(LC+2:) = STRVAL
            ELSE
               CALL GETBND('COL ',COL2,VLO,VUP)
               IF( VL2.GT.VLO )THEN
                  IF( SW )THEN
                     CLIST(LC+3:) = 'and'
                     LC = LC + 7
                     SW = .FALSE.
                  ENDIF
                  CALL FR2CLJ(STRVAL,VL2,L)
                  CLIST(LC+2:) = STRVAL(:L)//' <= '//CNAME
                  CALL FSLEN(CLIST,80,LC)
               ENDIF
               IF( VU2.LT.VUP )THEN
                  IF( SW )THEN
                     CLIST(LC+3:) = 'and'
                     LC = LC + 7
                  ENDIF
                  CALL FR2CLJ(STRVAL,VU2,L)
                  CLIST(LC+2:) = ' <= '//STRVAL(:L)
                  CALL FSLEN(CLIST,100,LC)
                  IF( VL2.GT.VLO )CLIST(LC+2:) = CNAME
               ENDIF
            ENDIF
            IF( FREQ.EQ.1 )THEN
               CALL REDTXT(CLIST,*13000)
               FREQ = FREQ0
            ELSE IF( FREQ.GT.1 )THEN
               FREQ = FREQ-1
            ENDIF
            IF( SWRDBG )THEN
               NOTE(8:) = '> 600'
               CALL FSLEN(CLIST,128,LAST)
               CALL REDTRK(CLIST,LAST,NOTE,*13000)
            ENDIF
         ENDIF
1000  CONTINUE
C
2000  CONTINUE
C ======================================
C UNIVARIATE EXTREME TEST
C ======================================
      IF( SWMSG .OR. SWRDBG )THEN
         CLIST = 'Performing univariate extreme test'
         IF( OUTPUT.NE.TTYOUT )WRITE(*,1,ERR=13000)CLIST
         CALL REDTXT(CLIST,*13000)
      ENDIF
      CALL FCLOCK(TIME)
      ELAPSE = TIME-TIME0
      IF( ELAPSE.GE.MAXTIM )CALL FLTIME(ELAPSE,MAXTIM,*9000)
C ================================================================
C TEST:  SUPPOSE COLUMN APPEARS IN ROW WITH COEFFICIENT V.  THEN,
C     V > 0:  (YL-YMAX)/V + XU <= X <= (YU-YMIN)/V + XL
C     V < 0:  (YL-YMAX)/V + XL >= X >= (YU-YMIN)/V + XU
C THESE GIVE POTENTIALLY NEW BOUNDS ON X.
C ================================================================
C
C  ::: LOOP OVER COLUMNS :::
      DO 2900 COL=1,NCOLS
         IF( INDEX(ICSTAT+COL).GT.0 )GOTO 2900
         VL = VALUE(IXL+COL)
         VU = VALUE(IXU+COL)
         IF( VU.GE.VL )GOTO 2900
C COL IS IN SUBMATRIX AND NOT FIXED
         CNAME = NAME(ICNAME+COL)
         CLIST = ' Activity '//CNAME
         CPOINT= NAMELN + 12
C
         CALL FCLOCK(TIME)
         ELAPSE = TIME-TIME0
         IF( ELAPSE.GT.MAXTIM )THEN
            PRINT *,' Analyzing column ',CNAME
            CALL FLTIME(ELAPSE,MAXTIM,*9000)
         ENDIF
C
         ROWLO = 0
         ROWUP = 0
C
C   ::: LOOP OVER ROWS OF COL :::
         I1 = INONZ + 2*BASE(ICINFO + COL - 1)
         I2 = INONZ + 2*BASE(ICINFO + COL) - 1
         DO 1500 I=I1,I2,2
            ROW = INDEX(I)
            IF( INDEX(IRSTAT+ROW).GT.0 )GOTO 1500
            VLROW = VALUE(IYL+ROW)
            VUROW = VALUE(IYU+ROW)
            IF( VLROW.LE.-VINFX .AND. VUROW.GE.VINFX )GOTO 1500
C ROW IS IN SUBMATRIX AND NOT FREE
            VMIN = VALUE(IYMIN+ROW)
            VMAX = VALUE(IYMAX+ROW)
            CALL GETVAL(VCOEF,I+1)
            IF( VCOEF.GT.VTOL0C )THEN
C VCOEF > 0
               IF( VLROW.GT.-VINFX .AND. VMAX.LT.VINFX )THEN
                  VLO = (VLROW - VMAX)/VCOEF + VU
               ELSE
                  VLO = VL
               ENDIF
               IF( VUROW.LT.VINFX .AND. VMIN.GT.-VINFX )THEN
                  VUP = (VUROW - VMIN)/VCOEF + VL
               ELSE
                  VUP = VU
               ENDIF
            ELSE IF( VCOEF.LT.-VTOL0C )THEN
C VCOEF < 0
               IF( VUROW.LT.VINFX .AND. VMIN.GT.-VINFX )THEN
                  VLO = (VUROW - VMIN)/VCOEF + VU
               ELSE
                  VLO = VL
               ENDIF
               IF( VLROW.GT.-VINFX .AND. VMAX.LT.VINFX )THEN
                  VUP = (VLROW - VMAX)/VCOEF + VL
               ELSE
                  VUP = VU
               ENDIF
            ELSE
C VCOEF = 0
               GOTO 1500
            ENDIF
C WE REQUIRE  VLO <= X(COL) <= VUP  ... TEST FOR TIGHTENING
            IF( VLO.GT.VL + VTOLAB )THEN
               VL = VLO
               ROWLO = ROW
            ENDIF
            IF( VUP.LT.VU - VTOLAB )THEN
               VU = VUP
               ROWUP = ROW
            ENDIF
C ...TEST FEASIBILITY
            IF( VU.LT.VL - VTAINF - VTRINF*ABS(VL) )GOTO 2913
1500     CONTINUE
C   ::: END LOOP OVER ROWS :::
C
         IF( ROWLO.EQ.0 .AND. ROWUP.EQ.0 )GOTO 2900
C AT LEAST ONE BOUND WAS TIGHTENED
         SWRED = .TRUE.
C TEST IF COL BECAME FIXED
         IF( VU-VL.LE.VTAFIX + VTRFIX*ABS(VU) )THEN
C YES, FIX IT (AND REPORT)
            VX = (VL + VU)/2.
            CALL REDFIX(VX,COL)
            CALL FR2CLJ(STRVAL,VX,L)
            CLIST = '  Column '//CNAME(:NAMELN)//' fixed ='//STRVAL
            CALL FSLEN(CLIST,70,L)
            IF( ROWLO.GT.0 )THEN
               IF( ROWUP.EQ.0 .OR. ROWUP.EQ.ROWLO )THEN
C ...BY SAME ROW (ROWLO=ROWUP)
                  CLIST(L+2:) = 'by row '//NAME(IRNAME+ROWLO)
               ELSE
C ...BY DIFFERENT ROWS
                  CLIST(L+2:) = 'by rows '//NAME(IRNAME+ROWLO)
                  CALL FSLEN(CLIST,100,L)
                  CLIST(L+2:) = 'and '//NAME(IRNAME+ROWUP)
               ENDIF
            ELSE
                CLIST(L+2:) = 'by row '//NAME(IRNAME+ROWUP)
            ENDIF
            IF( FREQ.EQ.1 )THEN
               CALL REDTXT(CLIST,*13000)
               FREQ = FREQ0
            ELSE IF( FREQ.GT.1 )THEN
               FREQ = FREQ-1
            ENDIF
            IF( SWRDBG )THEN
               NOTE(8:) = '> 1500'
               CALL FSLEN(CLIST,128,LAST)
               CALL REDTRK(CLIST,LAST,NOTE,*13000)
            ENDIF
         ELSE
C NO, DO BOUND REDUCTION(S)
            IF( VL.GT.VALUE(IXL+COL) .AND. VL.GT.-VINFX )THEN
               VALUE(IXL + COL) = VL
               CLIST(CPOINT:) = 'lower bound ='
               CALL FR2C12(CLIST(CPOINT+14:),VL)
               CLIST(CPOINT+27:)=' by row '//NAME(IRNAME+ROWLO)
               IF( FREQ.EQ.1 )THEN
                  CALL REDTXT(CLIST,*13000)
                  FREQ = FREQ0
               ELSE IF( FREQ.GT.1 )THEN
                  FREQ = FREQ-1
               ENDIF
               IF( SWRDBG )THEN
                  NOTE(8:) = '>> 1500'
                  CALL FSLEN(CLIST,128,LAST)
                  CALL REDTRK(CLIST,LAST,NOTE,*13000)
               ENDIF
            ENDIF
            IF( VU.LT.VALUE(IXU+COL) .AND. VU.LT.VINFX )THEN
               VALUE(IXU + COL) = VU
               CLIST(CPOINT:) = 'upper bound ='
               CALL FR2C12(CLIST(CPOINT+14:),VU)
               CLIST(CPOINT+27:)=' by row '//NAME(IRNAME+ROWUP)
               IF( FREQ.EQ.1 )THEN
                  CALL REDTXT(CLIST,*13000)
                  FREQ = FREQ0
               ELSE IF( FREQ.GT.1 )THEN
                  FREQ = FREQ-1
               ENDIF
               IF( SWRDBG )THEN
                  NOTE(8:) = '>> 1500'
                  CALL FSLEN(CLIST,128,LAST)
                  CALL REDTRK(CLIST,LAST,NOTE,*13000)
               ENDIF
            ENDIF
         ENDIF
C NEXT COL
2900  CONTINUE
C     =========
      GOTO 3000
C     =========
C INFEASIBILITY DETECTED
2913  CONTINUE
C FROM BRANCH:  VLO  <=  X(COL)  <=  VUP
C               VL    = MAX[VL,VLO] > VU = MIN[VU,VUP]
C               ROW   = ROW AT DETECTION
C              [VMIN,  VMAX ] = MYOPIC ROW RANGE (INCLUDING COL)
C              [VLROW, VUROW] = REQUIRED ROW RANGE
C               VCOEF = COEFFICIENT = A(ROW,COL)
C
      CLIST = '! Primal infeasibility detected with'//
     1        ' activity '//NAME(ICNAME+COL)
      CALL FSLEN(CLIST,90,L)
      CLIST(L+2:) = 'by row '//NAME(IRNAME+ROW)
      IF( OUTPUT.NE.TTYOUT )WRITE(*,1,ERR=13000)CLIST
      CALL REDTXT(CLIST,*13000)
C
      IF( VUP.LT.VL )THEN
         CHAR = '<'
         VX = VUP
         IF( V.GT.0. )THEN
            STR4 = 'MIN'
         ELSE
            STR4 = 'MAX'
         ENDIF
      ELSE
         CHAR = '>'
         VX = VLO
         IF( V.GT.0. )THEN
            STR4 = 'MAX'
         ELSE
            STR4 = 'MIN'
         ENDIF
      ENDIF
C STR4 IDENTIFIES WHETHER WE WANT TO RE-CREATE VMAX OR VMIN
C CHAR IDENTIFES WHICH NEW BOUND (VLO OR VUP) IMPLIES INFEASIBILITY
C VX = STRENGTHENED BOUND THAT CAUSED INFEASIBILITY (VLO OR VUP)
      WRITE(OUTPUT,2914,ERR=13000)CHAR,VX
2914  FORMAT(' Here is how row implies X ',A2,G15.8 //5X,
     2      'Activity',T25,'Coefficient',T42,'Relevant',T57,'Term'
     3     /                      T42,'bound',T52,'(Coef * bound)')
C GIVE TABLE OF RELEVANT BOUNDS AND COEFS
C (RE-CREATE VMIN OR VMAX WITHOUT COL)
      VSUM = 0.
      NZERO = 0
      DO 2950 J=1,NCOLS
         IF( INDEX(ICSTAT+J).GT.0 .OR. J.EQ.COL )GOTO 2950
         CALL GETAIJ(ROW,J,V)
         IF( ABS(V).LE.VTOL0C )GOTO 2950
C COLUMN J <> COL IS IN SUBMATRIX AND COEF IN ROW = V <> 0
         CALL GETBND('COL ',J,VL,VU)
         IF( (V.GT.0. .AND. STR4.EQ.'MIN' ) .OR.
     1       (V.LT.0. .AND. STR4.EQ.'MAX' ) )THEN
C COLUMN J = VL IN ROW'S EXTREME
            CHAR = 'L'
            VX = VALUE(IXL+J)
            IF( ABS(VX).LE.VTOLAB .AND. VX.LE.VL )THEN
               NZERO = NZERO + 1
               GOTO 2950
            ENDIF
         ELSE
C COLUMN J = VU IN ROW'S EXTREME
            CHAR = 'U'
            VX = VALUE(IXU+J)
            IF( ABS(VX).LE.VTOLAB .AND. VX.GE.VU )THEN
               NZERO = NZERO + 1
               GOTO 2950
            ENDIF
         ENDIF
C VX = EXTREME BOUND VALUE (VL OR VU) <> ORIGINAL 0 BOUND VALUE
         VTERM = V*VX
         VSUM = VSUM + VTERM
         WRITE(OUTPUT,2951,ERR=13000)NAME(ICNAME+J),V,CHAR,VX,VTERM
2951     FORMAT(5X,A16,T25,G15.8,1X,A1,'=',G15.8,T52,G16.9)
2950  CONTINUE
C
      IF( (VCOEF.GT.0. .AND. STR4.EQ.'MAX') .OR.
     1    (VCOEF.LT.0. .AND. STR4.EQ.'MIN') )THEN
         CHAR = '>'
         V = VLROW
         STR4 = '>'
      ELSE
         CHAR = '<'
         V = VUROW
         STR4 = '<'
      ENDIF
      VTERM  = V - VSUM
      VX = VTERM/VCOEF
      WRITE(OUTPUT,2961,ERR=13000)VSUM,CHAR,V,STR4,VTERM,VCOEF,VX
2961  FORMAT(T47,'Sum =',G16.9 / ' ...Row ',A1,' ',G15.8,
     1           ' ===> X ',A2,'(',G19.9,')/(',G15.8,') = ',G16.9)
      IF( NZERO.GT.0 )WRITE(OUTPUT,2962,ERR=13000)NZERO
2962  FORMAT(' ...',I6,' original zero bound values are not listed.')
C
      RETURN 1
C
3000  CONTINUE
C =========================================
C  BASIC RANGE TEST (USING REDUCED BOUNDS)
C =========================================
      IF( SOLNAM.EQ.'logical' .OR. IPIVOT.EQ.0 )GOTO 9000
      IF( SWMSG .OR. SWRDBG )THEN
         CLIST = 'Performing myopic range test'
         IF( OUTPUT.NE.TTYOUT )WRITE(*,1,ERR=13010)CLIST
         CALL REDTXT(CLIST,*13010)
      ENDIF
      CALL FCLOCK(TIME)
      ELAPSE = TIME-TIME0
      IF( ELAPSE.GE.MAXTIM )CALL FLTIME(ELAPSE,MAXTIM,*9000)
C
      CALL REDINF
C USING VALUE(IYMIN) AND VALUE(IYMAX) = MYOPIC RANGE
C
C INITIALIZE MYOPIC RANGE
      DO 4190 I=1,NROWS
         VALUE(IYMIN+I) = 0.
         VALUE(IYMAX+I) = 0.
4190  CONTINUE
C
      NALPHA = NROWS
C LOOP OVER NONBASIC VARIABLES TO ACCUMULATE RANGES
C
C FIRST, THE ROWS
      ROWCOL = 'ROW '
      ISTAT  = IRSTAT
      NUMBER = NROWS
      ILO    = IYL
      IUP    = IYU
      ZSIGN  = -1.
C
4500  CONTINUE
C
C ::: LOOP OVER ROWS/COLUMNS TO ACCUMULATE MYOPIC RANGES :::
C     (IN SUBMATRIX)
      DO 4700 K=1,NUMBER
         STAT = INDEX(ISTAT+K)
         IF( STAT.GT.0 )GOTO 4700
C ROW/COL IS IN SUBMATRIX...LOOK AT STATUS
         CHAR = CHARST(-STAT)
         IF( CHAR.NE.'L'.AND.CHAR.NE.'U' )GOTO 4700
C ...STATUS IS L OR U (IE, NONBASIC)
         CALL FCLOCK(TIME)
         ELAPSE = TIME-TIME0
         IF( ELAPSE.GT.MAXTIM )THEN
            CALL GETNAM(ROWCOL,K,CNAME)
            PRINT *,' Analyzing ',ROWCOL,CNAME
            CALL FLTIME(ELAPSE,MAXTIM,*9000)
         ENDIF
C
C FTRAN ITS COLUMN
         CALL GALPHA(ROWCOL,K,ZALPHA,NALPHA)
         CALL GFTRAN(ZALPHA,NALPHA)
C USE REDUCED BOUNDS
         VLO = VALUE(ILO+K)
         VUP = VALUE(IUP+K)
C
C LOOP OVER ALPHA'S TO ACCUMULATE RANGE
         DO 4600 I=1,NROWS
            CALL GETBVP(I,STR4,NUM)
            CALL GETMAP(STR4,NUM,SW)
            IF( .NOT.SW )GOTO 4600
C BASIC ROW/COL (STR4) NUM IN SUBMATRIX...SET ITS RATE (V)
            V = ZSIGN*ZALPHA(I)
C               :....NEGATIVE SIGN FOR ROW
C THE EXTREMES ARE FROM NONBASIC LEVEL BEING AT ITS APPROPRIATE BOUND,
C DEPENDING UPON THE SIGN OF THE RATE (V)
            IF( V.LE.-VTOLAB )THEN
C RATE < 0...SWITCH BOUNDS
               VL = -VUP
               VU = -VLO
               V  = -V
            ELSE IF( V.GE.VTOLAB )THEN
C RATE > 0
               VL = VLO
               VU = VUP
            ELSE
C RATE = 0
               GOTO 4600
            ENDIF
C  ...NOW V = ABS(RATE) AND RANGE OF NONBASIC TERM IS (V*VL,V*VU)
            IF( VL.GT.-VINF )THEN
               IF( VALUE(IYMIN+I).GT.-VINF )
     1            VALUE(IYMIN+I) = VALUE(IYMIN+I) + V*VL
            ELSE
               VALUE(IYMIN+I) = -VINF
            ENDIF
            IF( VU.LT. VINF )THEN
               IF( VALUE(IYMAX+I).LT. VINF )
     1            VALUE(IYMAX+I) = VALUE(IYMAX+I) + V*VU
            ELSE
               VALUE(IYMAX+I) = VINF
            ENDIF
4600     CONTINUE
C      ::: END LOOP OVER ALPHA :::
C NEXT NONBASIC
4700  CONTINUE
C  ::: END LOOP OVER NONBASIC ROW/COLUMNS IN SUBMATRIX :::
C
      IF( ROWCOL.EQ.'COL ' )GOTO 4900
C NOW THE COLUMNS
        ROWCOL = 'COL '
        ISTAT  = ICSTAT
        NUMBER = NCOLS
        ILO    = IXL
        IUP    = IXU
        ZSIGN  = 1.
      GOTO 4500
C
4900  CONTINUE
C NOW VALUE(IYMIN+i) <= LEVEL OF i-TH BASIC <= VALUE(IYMAX+i)
      DO 5900 I=1,NROWS
         CALL GETBVP(I,ROWCOL,NUMBER)
         CALL GETMAP(ROWCOL,NUMBER,SW)
         IF( .NOT.SW )GOTO 5900
         CALL FCLOCK(TIME)
         ELAPSE = TIME-TIME0
         IF( ELAPSE.GT.MAXTIM )THEN
            CALL GETNAM(ROWCOL,NUMBER,CNAME)
            PRINT *,' Analyzing (basic) ',ROWCOL,CNAME
            CALL FLTIME(ELAPSE,MAXTIM,*9000)
         ENDIF
C
         VMIN = VALUE(IYMIN+I)
         VMAX = VALUE(IYMAX+I)
         IF( VMIN.LE.-VINF .AND. VMAX.GE.VINF )GOTO 5900
C MYOPIC RANGE = [VMIN, VMAX]
         IF( ROWCOL.EQ.'ROW ' )THEN
C BASIC ROW
            VL = VALUE(IYL+NUMBER)
            VU = VALUE(IYU+NUMBER)
            CNAME = NAME(IRNAME+NUMBER)
         ELSE
C BASIC COLUMN
            VL = VALUE(IXL+NUMBER)
            VU = VALUE(IXU+NUMBER)
            CNAME = NAME(ICNAME+NUMBER)
         ENDIF
C REQUIRED RANGE = [VL, VU]
         IF( VL.LE.-VINF .AND. VU.GE.VINF )GOTO 5900
C TEST FEASIBILITY
C ================
         IF( VMIN-VU.GT. VTAINF + VTRINF*ABS(VMIN)  .OR.
     1       VMAX-VL.LT.-VTAINF - VTRINF*ABS(VMAX) )THEN
            WRITE(OUTPUT,5201,ERR=13010)ROWCOL,CNAME
5201        FORMAT(' ! Primal infeasibility detected with basic ',
     1                A4,A16)
            IF( OUTPUT.NE.TTYOUT )WRITE(*,5201)ROWCOL,CNAME
            SWRED = VMAX-VL .LT.-VTAINF - VTRINF*ABS(VMAX)
C               :..DISTINGUISH WHICH BOUND IS VIOLATED
C                  (WE WON'T NEED SWRED ANYMORE)
            IF( SWRED )THEN
               WRITE(OUTPUT,5202,ERR=13010)ROWCOL,VMAX,VL
5202           FORMAT(' MAX ',A4,'VALUE =',G15.8,' < ',G15.8,
     1                ' = LO BOUND')
            ELSE
               WRITE(OUTPUT,5203,ERR=13010)ROWCOL,VMIN,VU
5203           FORMAT(' MIN ',A4,'VALUE =',G15.8,' > ',G15.8,
     1                ' = UP BOUND')
            ENDIF
C ...RE-CREATE THE RATES, GIVING TABLE AS IN COEFFICIENTS
C    (EXCEPT WE COMPUTE THE RATES DIFFERENTLY, WHICH IS AN ERROR CHECK)
            WRITE(OUTPUT,5205,ERR=13010)
5205        FORMAT(' Here are the relevant values:'/
     1      ' Nonbasic',           T35,'Relevant',T53,'Value of Term'
     2     /' Activity',T18,'Rate',T35,'Bound',   T53,'(Rate * Bound)'
     X            )
C ...BTRAN THE PIVOT ROW
            CALL GALPHA('ROW ',I,ZALPHA,NALPHA)
            CALL GBTRAN(ZALPHA,NALPHA)
C
C   NOW ZALPHA = -e(i)B-1, SO THE INNNER PRODUCT ZALPHA*A(j) EQUALS THE
C   RATE, -e(i)B-1A(j), AND ZALPHA*e(k) = -e(i)B-1e(k) = -RATE OF Y(k).
C   (NOTE:  DO NOT CALL GPRDCT, BECAUSE VECTOR HERE IS ZALPHA.)
C           BUT VECTOR IN GPRDCT IS SINGLE PRECISION.)
C
            ZSUM = 0.
            NZERO= 0
C ...PUT ACTIVITIES FIRST IN TABLE
            DO 5300 COL=1,NCOLS
               CALL GETMAP('COL ',COL,SW)
               IF( .NOT.SW )GOTO 5300
               CALL GETST('COL ',COL,CHAR,DUMMY)
               IF( CHAR.NE.'L' .AND. CHAR.NE.'U' )GOTO 5300
               CALL GZPROD(ZALPHA,NALPHA,COL,ZRATE)
               VRATE = ZRATE
               IF( ABS(VRATE).LE.VTOLAB )GOTO 5300
C ...RATE = VRATE <> 0
               IF( SWRED )THEN
C    VMAX < VL
                  IF( VRATE.GT.0. )THEN
                     VX = VALUE(IXU+COL)
                     CHAR = 'U'
                  ELSE
                     VX = VALUE(IXL+COL)
                     CHAR = 'L'
                  ENDIF
               ELSE
C    VMIN > VU
                  IF( VRATE.GT.0.)THEN
                     VX = VALUE(IXL+COL)
                     CHAR = 'L'
                  ELSE
                     VX = VALUE(IXU+COL)
                     CHAR = 'U'
                  ENDIF
               ENDIF
               IF( ABS(VX).LE.VTOLAB )THEN
C VX = 0...SEE IF = ORIGINAL BOUND (IF SO, DO NOT ENTER INTO TABLE)
                  CALL GETBND('COL ',COL,VL,VU)
C          VL <= VX <= VU IN GENERAL, SO TEST EQUALITY:
                  IF( CHAR.EQ.'L' )THEN
                     SW = (VX.LE.VL)
                  ELSE
                     SW = (VX.GE.VU)
                  ENDIF
                  IF( SW )THEN
                     NZERO = NZERO+1
                     GOTO 5300
                  ENDIF
               ENDIF
               Z = VRATE*VX
               V = Z
               ZSUM = ZSUM + Z
               WRITE(OUTPUT,5209,ERR=13010)NAME(ICNAME+COL),VRATE,
     1                               CHAR,VX,V
5209           FORMAT(1X,A16,1X,G15.8,T35,A1,'=',G15.8,T53,G16.9)
5300        CONTINUE
C
C PUT ROW VARIABLES SECOND IN TABLE
            DO 5350 ROW=1,NROWS
               CALL GETMAP('ROW ',ROW,SW)
               IF( .NOT.SW )GOTO 5350
               CALL GETST('ROW ',ROW,CHAR,DUMMY)
               IF( CHAR.NE.'L' .AND. CHAR.NE.'U' )GOTO 5350
               VRATE = -ZALPHA(ROW)
               IF( ABS(VRATE).LE.VTOLAB )GOTO 5350
C ...RATE = VRATE <> 0
               IF( SWRED )THEN
C    VMAX < VL
                  IF( VRATE.GT.0. )THEN
                     VX = VALUE(IYU+ROW)
                     CHAR = 'U'
                  ELSE
                     VX = VALUE(IYL+ROW)
                     CHAR = 'L'
                  ENDIF
               ELSE
C    VMIN > VU
                  IF( VRATE.GT.0.)THEN
                     VX = VALUE(IYL+ROW)
                     CHAR = 'L'
                  ELSE
                     VX = VALUE(IYU+ROW)
                     CHAR = 'U'
                  ENDIF
               ENDIF
               IF( ABS(VX).LE.VTOLAB )THEN
                  CALL GETBND('ROW ',ROW,VL,VU)
                  IF( CHAR.EQ.'L' )THEN
                     SW = (VX.LE.VL)
                  ELSE
                     SW = (VX.GE.VU)
                  ENDIF
                  IF( SW )THEN
                     NZERO = NZERO+1
                     GOTO 5350
                  ENDIF
               ENDIF
               Z = VRATE*VX
               V = Z
               ZSUM = ZSUM + Z
               WRITE(OUTPUT,5209,ERR=13010)NAME(IRNAME+ROW),VRATE,
     1                               CHAR,VX,V
5350        CONTINUE
C
            WRITE(OUTPUT,'(T48,5HSum = ,G16.9)',ERR=13010)ZSUM
            IF( NZERO.GT.0 )WRITE(OUTPUT,5351,ERR=13010)NZERO
5351        FORMAT(' ...',I6,
     1             ' original zero bound values are not listed.')
C RESTORE IVFREE BEFORE RETURNING
            IVFREE = IV0
            RETURN 1
         ENDIF
C
C TEST IF FORCING
C ===============
         IF( VMIN.GE.VU + VTOLAB )THEN
C ...BASIC VAR FORCES NONBASICS TO THEIR VMIN EXTREMES
            STR4 = 'MIN'
         ELSE IF( VMAX.LE.VL - VTOLAB )THEN
C ...BASIC VAR FORCES NONBASICS TO THEIR VMAX EXTREMES
            STR4 = 'MAX'
         ELSE
            GOTO 5900
         ENDIF
C
         NFIX = 0
C ...BTRAN THE PIVOT ROW
         CALL GALPHA('ROW ',I,ZALPHA,NALPHA)
         CALL GBTRAN(ZALPHA,NALPHA)
C ...RE-CREATE RATES TO FIX NONBASIC COLUMNS AT THEIR EXTREMES
         DO 5600 COL=1,NCOLS
            CALL GETMAP('COL ',COL,SW)
            IF( .NOT.SW )GOTO 5600
            CALL GETST('COL ',COL,CHAR,DUMMY)
            IF( CHAR.NE.'L' .AND. CHAR.NE.'U' )GOTO 5600
C ...COL IS NONBASIC AND IN SUBMATRIX
            VLO = VALUE(IXL+COL)
            VUP = VALUE(IXU+COL)
            IF( VLO.GE.VUP )GOTO 5600
C ...COL IS PRESENTLY NOT FIXED
            CALL GZPROD(ZALPHA,NALPHA,COL,ZRATE)
            VRATE = ZRATE
            IF( ABS(VRATE).LE.VTOLAB )GOTO 5600
C ...RATE = VRATE <> 0
            VLO = VRATE*VLO
            VUP = VRATE*VUP
            IF( VLO.GT.VUP )THEN
               V   = VLO
               VLO = VUP
               VUP = V
            ENDIF
C ...TERM RANGE = [VLO,VUP]
            IF( STR4.EQ.'MIN' )THEN
               VX = VLO
            ELSE
               VX = VUP
            ENDIF
            CALL REDFIX(VX,COL)
            IF( NFIX.EQ.0 )THEN
               CLIST = 'Basic '//ROWCOL//CNAME(:NAMELN)//' forces '
     1          //'nonbasics to their '//STR4//'(reduced) extremes'
               IF( FREQ.EQ.1 )THEN
                  CALL REDTXT(CLIST,*13000)
                  FREQ = FREQ0
               ELSE IF( FREQ.GT.1 )THEN
                  FREQ = FREQ-1
               ENDIF
               IF( SWRDBG )THEN
                  NOTE(8:) = '> 5351'
                  CALL FSLEN(CLIST,128,LAST)
                  CALL REDTRK(CLIST,LAST,NOTE,*13010)
               ENDIF
            ENDIF
            CALL FR2CLJ(STRVAL,VX,L)
            CNAME = NAME(ICNAME+COL)
            CLIST = '...Nonbasic column '//CNAME(:NAMELN)
     1            //'fixed = '//STRVAL
            IF( FREQ.EQ.1 )THEN
               CALL REDTXT(CLIST,*13000)
               FREQ = FREQ0
            ELSE IF( FREQ.GT.1 )THEN
               FREQ = FREQ-1
            ENDIF
            IF( SWRDBG )THEN
               NOTE(8:) = '5351-5600'
               CALL FSLEN(CLIST,128,LAST)
               CALL REDTRK(CLIST,LAST,NOTE,*13000)
            ENDIF
            NFIX = NFIX + 1
5600     CONTINUE
C
         IF( ROWCOL.EQ.'COL ' .AND. VL.LT.VU )THEN
C ...FIX BASIC COL AT FORCED LEVEL (NOT ALREADY FIXED)
            IF( STR4.EQ.'MIN' )THEN
               VX = VMIN
            ELSE
               VX = VMAX
            ENDIF
            CALL REDFIX(VX,NUMBER)
            NFIX = NFIX + 1
            CNAME = NAME(ICNAME+NUMBER)
            CALL FR2CLJ(STRVAL,VX,L)
            CLIST = 'Basic column '//CNAME(:NAMELN)
     1           // ' fixed = '//STRVAL//'...forced'
            IF( FREQ.EQ.1 )THEN
               CALL REDTXT(CLIST,*13000)
               FREQ = FREQ0
            ELSE IF( FREQ.GT.1 )THEN
               FREQ = FREQ-1
            ENDIF
            IF( SWRDBG )THEN
               NOTE(8:) = '< 5600'
               CALL FSLEN(CLIST,128,LAST)
               CALL REDTRK(CLIST,LAST,NOTE,*13000)
            ENDIF
            CLIST = 'Basic column '//CNAME(:NAMELN)
     1           // ' fixed = '//STRVAL
            IF( FREQ.EQ.1 )THEN
               CALL REDTXT(CLIST,*13000)
               FREQ = FREQ0
            ELSE IF( FREQ.GT.1 )THEN
               FREQ = FREQ-1
            ENDIF
            IF( SWRDBG )THEN
               NOTE(8:) = '< 5600'
               CALL FSLEN(CLIST,128,LAST)
               CALL REDTRK(CLIST,LAST,NOTE,*13000)
            ENDIF
         ENDIF
         SWRED = (NFIX.GT.0)
         IF( SWRED )GOTO 9000
C                :...NO ITERATION...WE STOP WITH ONE REDUCTION TO
C                                   RETURN TO CHEAPER TESTS
C NEXT BASIC ROW/COL VAR
5900  CONTINUE
C ====== END BASIC RANGING ======
C
C NORMAL RETURN
9000  CONTINUE
C RESTORE IVFREE
      IVFREE = IV0
      RETURN
C
C ERROR RETURN BEFORE IVFREE IS CHANGED
13000 CONTINUE
      RETURN 2
C ERROR RETURN AFTER IVFREE IS CHANGED
13010 IVFREE = IV0
      RETURN 2
C
C ** REDEXP ENDS HERE
      END
