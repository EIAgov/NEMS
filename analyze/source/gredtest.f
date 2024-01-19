C             ::: GREDTEST.FOR  8-17-95 :::
C
C Created 8-14-95, taking REDCOL and REDROW from REDUCE.FOR
C
C This contains REDUCE basic test routines.
C
C     REDCOL.....Column tests
C     REDROW.....Row tests
C
      SUBROUTINE REDCOL(CLIST,*,*)
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
C This applies dual tests to each column and accumulates row ranges.
C    First alternate return is for infeasibility.
C    Second alternate return is for i/o error.
C
      CHARACTER*128 CLIST
C LOCAL
      LOGICAL*1     SW
      CHARACTER*1   CHAR
      CHARACTER*8   RSTAT
      CHARACTER*64  NOTE
C ::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
1     FORMAT(1X,A79)
      NOTE = 'REDCOL'
C
      DO 2900 J=1,NCOLS
         IF(INDEX(ICSTAT + J).GT.0)GOTO 2900
C NEGATIVE COLUMN STATUS MEANS COLUMN J IS IN SUBMATRIX (ELSE IGNORED)
         NZ = BASE(ICINFO + J) - BASE(ICINFO + J-1)
         IF(NZ.EQ.0)GOTO 2900
         I1 = INONZ + 2*BASE(ICINFO + J-1)
         I2 = I1 + 2*NZ - 1
         CLIST = 'Activity '//NAME(ICNAME+J)
         CPOINT= NAMELN + 11
C SET CURRENT BOUNDS FOR TESTS
         VL = VALUE(IXL + J)
         VU = VALUE(IXU + J)
C PREPARE TO COMPUTE REDUCED COST RANGE
         VDMIN = 0.0
         VDMAX = 0.0
C
C  LOOP OVER NONZEROES OF COLUMN J TO ACCUMULATE AND TEST
C  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         DO 1500 I=I1,I2,2
            ROW = INDEX(I)
            IF(INDEX(IRSTAT + ROW).GT.0)GOTO 1500
C ...ROW IS IN SUBMATRIX...GET VALUE OF NONZERO
            CALL GETVAL(V,I+1)
            IF( SWRDBG .AND. ROW.EQ.ROWDBG )THEN
               PRINT *,' REDCOL 1500...ROW ',NAME(IRNAME+ROW),V
               PAUSE = PAUSE-1
               IF( SWDBG .AND. PAUSE.LE.0 )CALL GDEBUG
            ENDIF
C  DETERMINE EXTREME VALUES CONTRIBUTING TO ROW RANGE AND REDUCED COST
            IF( V.LT.-VTOL0C )THEN
               VYMIN = VU
               VYMAX = VL
               VSIGN = -1.0
               IF( VDMIN.GT.-VTSUM .AND. VALUE(IPL+ROW).GT.-VINFX )THEN
                  VDMIN = VDMIN - V*VALUE(IPL + ROW)
               ELSE
                  VDMIN = -VINF
               ENDIF
               IF( VDMAX.LT.VTSUM .AND. VALUE(IPU+ROW).LT.VINFX )THEN
                  VDMAX = VDMAX - V*VALUE(IPU + ROW)
               ELSE
                  VDMAX = VINF
               ENDIF
            ELSE IF( V.GT.VTOL0C )THEN
               VYMIN = VL
               VYMAX = VU
               VSIGN = 1.0
               IF( VDMIN.GT.-VTSUM .AND. VALUE(IPU+ROW).LT.VINFX )THEN
                  VDMIN = VDMIN - V*VALUE(IPU + ROW)
               ELSE
                  VDMIN = -VINF
               ENDIF
               IF( VDMAX.LT.VTSUM .AND. VALUE(IPL+ROW).GT.-VINFX )THEN
                  VDMAX = VDMAX - V*VALUE(IPL + ROW)
               ELSE
                  VDMAX = VINF
               ENDIF
            ELSE
               GOTO 1500
            ENDIF
            IF( SWRDBG .AND. ROW.EQ.ROWDBG )THEN
               PRINT *,' VYMIN,VYMAX,VSIGN =',VYMIN,VYMAX,VSIGN
               PAUSE = PAUSE-1
               IF( SWDBG .AND. PAUSE.LE.0 )THEN
                  CALL GDEBUG
                  PRINT *,' ABORT (N/Y)? '
                  READ(*,'(A1)')CHAR
                  IF( CHAR.EQ.'Y' )GOTO 13000
               ENDIF
            ENDIF
            IF( VL.GE.VU )GOTO 1400
C    (SKIP FORCING TEST IF COLUMN IS ALREADY FIXED)
C
C    WAS ROW MARKED FORCING?
            IF(INDEX(IFREE + ROW).LT.0)THEN
C    -YES, FORCE X AT ITS EXTREME VALUE FOR YMIN
                VX = VYMIN
            ELSE IF(INDEX(IFREE + ROW).GT.0)THEN
C    -YES, FORCE X AT ITS EXTREME VALUE FOR YMAX
                VX = VYMAX
            ELSE
C    -NO
                GOTO 1400
            ENDIF
500         CONTINUE
C =FIX COLUMN J = VX
            CALL REDFIX(VX,J)
            VYMIN = VX
            VYMAX = VX
            VL = VX
            VU = VX
            SWRED = .TRUE.
            CLIST(CPOINT:)='forced = '
            CALL FR2C12(CLIST(CPOINT+9:),VX)
            CLIST(CPOINT+22:)=' by row '//NAME(IRNAME+ROW)
            IF( FREQ.EQ.1 )THEN
               CALL REDTXT(CLIST,*13000)
               FREQ = FREQ0
            ELSE IF( FREQ.GT.1 )THEN
               FREQ = FREQ-1
            ENDIF
            IF( SWRDBG )THEN
               NOTE(8:) = '500'
               CALL FSLEN(CLIST,128,LAST)
               CALL REDTRK(CLIST,LAST,NOTE,*13000)
            ENDIF
C
1400        CONTINUE
C    TALLY ROW RANGE
            IF( SWRDBG .AND. ROW.EQ.ROWDBG )THEN
               PRINT *,' AT 1400, TALLY ROW ',NAME(IRNAME+ROW)
               PRINT *,' V,VINF,VINFX =',V,VINF,VINFX
               VVYMIN = V*VYMIN
               WRITE(*,'(21H YMIN,VYMIN,VYMIN*V =,3G20.12)')
     1                  VALUE(IYMIN+ROW),VYMIN,VVYMIN
               PAUSE = PAUSE-4
            ENDIF
            IF( VALUE(IYMIN+ROW).GT.-VINF  .AND.
     1          VYMIN           .GT.-VTSUM .AND.
     2          VYMIN*V         .GT.-VINFX )THEN
               VALUE(IYMIN+ROW) = VALUE(IYMIN+ROW) + V*VYMIN
               IF( SWRDBG .AND. ROW.EQ.ROWDBG )WRITE(*,
     1            '(14H ...NOW YMIN =,G20.12)' )VALUE(IYMIN+ROW)
            ELSE
               VALUE(IYMIN+ROW) =-VINF
            ENDIF
            IF( VALUE(IYMAX+ROW).LT. VINF  .AND.
     1          VYMAX           .LT. VTSUM .AND.
     2          VYMAX*V         .LT. VINFX )THEN
               VALUE(IYMAX+ROW) = VALUE(IYMAX+ROW) + VYMAX*V
            ELSE
               VALUE(IYMAX+ROW) = VINF
            ENDIF
            IF( SWRDBG .AND. ROW.EQ.ROWDBG )THEN
               WRITE(*,'(12H YMIN,YMAX =,2G20.12)')
     1                  VALUE(IYMIN+ROW),VALUE(IYMAX+ROW)
               PAUSE = PAUSE-1
               IF( SWDBG .AND. PAUSE.LE.0 )THEN
                  CALL GDEBUG
                  PRINT *,' ABORT (N/Y)? '
                  READ(*,'(A1)')CHAR
                  IF( CHAR.EQ.'Y' )GOTO 13000
               ENDIF
            ENDIF
1500     CONTINUE
C ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C END LOOP OVER NONZEROES OF COLUMN J
C ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
C SET REQUIRED REDUCED COST RANGE (VDL,VDU)
C
         IF( (VL.LE.-VINF .AND. OPT.EQ.OPTMIN) .OR.
     1       (VU.GE. VINF .AND. OPT.EQ.OPTMAX) )THEN
            VDU = 0.0
         ELSE
            VDU = VINF
         ENDIF
         IF( (VU.GE. VINF .AND. OPT.EQ.OPTMIN) .OR.
     1       (VL.LE.-VINF .AND. OPT.EQ.OPTMAX) )THEN
            VDL = 0.0
         ELSE
            VDL = -VINF
         ENDIF
C NOW TEST DUAL INFEASIBILITY
         IF( VDMAX.LT.VDL - VTAINF - VTRINF*ABS(VDL) .OR.
     1       VDMIN.GT.VDU + VTAINF + VTRINF*ABS(VDMIN) )THEN
C !DUAL INFEASIBLE
            WRITE(OUTPUT,1501,ERR=13000)NAME(ICNAME+J),
     1        VDL,VDU,VDMIN,VDMAX
            IF(OUTPUT.NE.TTYOUT)WRITE(TTYOUT,1501)NAME(ICNAME+J),
     1           VDL,VDU,VDMIN,VDMAX
1501        FORMAT(' !Dual infeasibility detected with activity ',A16
     1            /' Required reduced cost range: ',G15.8,' to ',G15.8
     2            /' Computed reduced cost range: ',G15.8,' to ',G15.8)
            RETURN 1
         ENDIF
C
         SW = .FALSE.
         IF( VDMAX.LE.VDL + VTAFIX + VTRFIX*ABS(VDL) )THEN
C =FORCES PRICES OF ADJACENT ROWS TO DMAX EXTREMES (recompute)
            DO 1700 I=I1,I2,2
               ROW = INDEX(I)
               IF( INDEX(IRSTAT+ROW).GT.0 .OR.
     1              VALUE(IPL+ROW).GE.VALUE(IPU+ROW) )GOTO 1700
C ROW IS IN SUBMATRIX AND PRICE IS NOT FIXED
               IF(INDEX(I+1).LT.0)THEN
                  VP = VALUE(IPU + ROW)
                  VALUE(IPL + ROW) = VP
               ELSE
                  VP = VALUE(IPL + ROW)
                  VALUE(IPU + ROW) = VP
               ENDIF
C VP = ROW PRICE BOUND THAT YIELDS MAX REDUCED COST (VDMAX)
               SW = .TRUE.
               CLIST(CPOINT:) = 'forces price of row '//
     1                          NAME(IRNAME+ROW)
               K = CPOINT + 22 + NAMELN
               CLIST(K:) = '='
               CALL FR2C12(CLIST(K+2:),VP)
               IF( FREQ.EQ.1 )THEN
                  CALL REDTXT(CLIST,*13000)
                  FREQ = FREQ0
               ELSE IF( FREQ.GT.1 )THEN
                  FREQ = FREQ-1
               ENDIF
               IF( SWRDBG )THEN
                  NOTE(8:) = '< 1700'
                  CALL FSLEN(CLIST,128,LAST)
                  CALL REDTRK(CLIST,LAST,NOTE,*13000)
               ENDIF
1700        CONTINUE
          ENDIF
C
         IF( VDMIN.GE.VDU - VTAFIX - VTRFIX*ABS(VDMIN) )THEN
C =FORCES PRICES OF ADJACENT ROWS TO DMIN EXTREMES (recompute)
            DO 1750 I=I1,I2,2
               ROW = INDEX(I)
               IF( INDEX(IRSTAT+ROW).GT.0 .OR.
     1             VALUE(IPL+ROW).GE.VALUE(IPU+ROW) )GOTO 1750
C ROW IS IN SUBMATRIX AND PRICE IS NOT FIXED
               IF(INDEX(I+1).LT.0)THEN
                  VP = VALUE(IPL + ROW)
                  VALUE(IPU + ROW) = VP
               ELSE
                  VP = VALUE(IPU + ROW)
                  VALUE(IPL + ROW) = VP
               ENDIF
C VP = ROW PRICE BOUND THAT YIELDS MIN REDUCED COST (VDMIN)
               SW = .TRUE.
               CLIST(CPOINT:) = 'forces price of row '
     1                         //NAME(IRNAME+ROW)
               K = CPOINT + 22 + NAMELN
               CLIST(K:) = '='
               CALL FR2C12(CLIST(K+2:),VP)
               IF( FREQ.EQ.1 )THEN
                  CALL REDTXT(CLIST,*13000)
                  FREQ = FREQ0
               ELSE IF( FREQ.GT.1 )THEN
                  FREQ = FREQ-1
               ENDIF
               IF( SWRDBG )THEN
                  NOTE(8:) = '< 1750'
                  CALL FSLEN(CLIST,128,LAST)
                  CALL REDTRK(CLIST,LAST,NOTE,*13000)
               ENDIF
1750        CONTINUE
         ENDIF
C
         IF( SW )THEN
            SWRED = .TRUE.
            NCFORC = NCFORC + 1
            IF( SWRDBG )THEN
               NOTE(8:) = '1750'
               CALL FSLEN(CLIST,128,LAST)
               CALL REDTRK(CLIST,LAST,NOTE,*13000)
            ENDIF
            GOTO 2900
         ENDIF
C
         IF( VL.GE.VU )GOTO 2000
C TEST IF ACTIVITY MUST BE AT BOUND FOR OPTIMALITY
         IF( VDMAX.LE.-VTPINF )THEN
C D <= 0...X=L OR U AT OPTIMALITY (DEPENDS ON OPT)
            IF( OPT.EQ.OPTMIN )THEN
               V = VU
            ELSE
               V = VL
            ENDIF
            CALL REDFIX(V,J)
            RSTAT = 'negative'
         ELSE IF( VDMIN.GE.VTPINF )THEN
C D >= 0...X=U OR L AT OPTIMALITY (DEPENDS ON OPT)
            IF( OPT.EQ.OPTMIN )THEN
               V = VL
            ELSE
               V = VU
            ENDIF
            CALL REDFIX(V,J)
            RSTAT = 'positive'
         ELSE
            GOTO 2000
         ENDIF
         SWRED = .TRUE.
C
         CLIST(CPOINT:) = 'forced = '
         CALL FR2C12(CLIST(CPOINT+10:),V)
         CLIST(CPOINT+23:)='by '//RSTAT(:8)//' price'
         IF( FREQ.EQ.1 )THEN
            CALL REDTXT(CLIST,*13000)
            FREQ = FREQ0
         ELSE IF( FREQ.GT.1 )THEN
            FREQ = FREQ-1
         ENDIF
         IF( SWRDBG )THEN
            NOTE(8:) = '< 2000'
            CALL FSLEN(CLIST,128,LAST)
            CALL REDTRK(CLIST,LAST,NOTE,*13000)
         ENDIF
         GOTO 2900
C   ============================
2000     CONTINUE
C   ============================
C DUAL TESTS, USING REQUIRED SIGN OF REDUCED COST
         IF( VDL.LT.0. .AND. VDU.GT.0. )GOTO 2900
C COLUMN (J) REQUIRES D >= 0 (= VDL) AND/OR D <= 0 (= VDU)
         DO 2500 I=I1,I2,2
            ROW = INDEX(I)
            VPL = VALUE(IPL + ROW)
            VPU = VALUE(IPU + ROW)
            IF( SWRDBG .AND. ROW.EQ.ROWDBG )
     1        PRINT *,' REDCOL 2500...ROW ',NAME(IRNAME+ROW),VPL,VPU
            IF( INDEX(IRSTAT+ROW).GT.0 .OR. VPL.GE.VPU )GOTO 2500
C ROW IS IN SUBMATRIX AND PRICE IS NOT FIXED
            CALL GETVAL(V,I+1)
            IF( SWRDBG .AND. ROW.EQ.ROWDBG )THEN
               PRINT *,' ...V=',V
               PAUSE = PAUSE-2
               IF( SWDBG .AND. PAUSE.LE.0 )THEN
                  CALL GDEBUG
                  PRINT *,' ABORT (N/Y)? '
                  READ(*,'(A1)')CHAR
                  IF( CHAR.EQ.'Y' )GOTO 13000
               ENDIF
            ENDIF
            IF( V.GT.VTOL0C )THEN
              IF( VDMAX.LT. VTSUM .AND. VDL.GE.0. )VPU = VDMAX/V + VPL
              IF( VDMIN.GT.-VTSUM .AND. VDU.LE.0. )VPL = VDMIN/V + VPU
            ELSE IF( V.LT.-VTOL0C )THEN
              IF( VDMAX.LT. VTSUM .AND. VDL.GE.0. )VPL = VDMAX/V + VPL
              IF( VDMIN.GT.-VTSUM .AND. VDU.LE.0. )VPU = VDMIN/V + VPU
            ELSE
               GOTO 2500
            ENDIF
            IF( VPL.LT.VALUE(IPL+ROW) )VPL = VALUE(IPL+ROW)
            IF( VPU.GT.VALUE(IPU+ROW) )VPU = VALUE(IPU+ROW)
            IF( VPL.LE.VALUE(IPL+ROW) - VAPDIF - VRPDIF*ABS(VPL) .AND.
     1          VPU.GE.VALUE(IPU+ROW) + VAPDIF + VRPDIF*ABS(VPU)
     2        )GOTO 2500
C AT LEAST ONE PRICE BOUND WAS TIGHTENED
            IF( VPL.GT.VPU + VTPINF )THEN
               WRITE(OUTPUT,2501,ERR=13000)NAME(ICNAME+J)
2501           FORMAT(' !Dual infeasibility implied by column ',A16)
               IF( OUTPUT.NE.TTYOUT )WRITE(*,2501)NAME(ICNAME+J)
               WRITE(OUTPUT,2502,ERR=13000)NAME(IRNAME+ROW),VPL,VPU
2502           FORMAT(' Prices of row ',A16,' were reduced to: ',
     1                'P >= ',G13.7,' and P <= ',G13.7)
               RETURN 1
            ENDIF
            IF( VPL.GE.VPU + VTAFIX + VTRFIX*ABS(VPL) )THEN
               V = (VPL+VPU)/2.
               IF( ABS(V).LE.VTOLAB )V = 0.
               VALUE(IPL+ROW) = V
               VALUE(IPU+ROW) = V
               SWRED = .TRUE.
               CLIST(CPOINT:) = 'forces price of row '//
     1                          NAME(IRNAME+ROW)
               L = CPOINT + 22 + NAMELN
               CLIST(L:) = '= '
               CALL FR2C12(CLIST(L+2:),V)
               IF( VDL.GE.VDU )THEN
                  CLIST(L+15:) = 'by reduced cost = 0'
               ELSE IF( VDL.GE.0. )THEN
                  CLIST(L+15:) = 'by reduced cost > 0'
               ELSE
                  CLIST(L+15:) = 'by reduced cost < 0'
               ENDIF
               IF( FREQ.EQ.1 )THEN
                  CALL REDTXT(CLIST,*13000)
                  FREQ = FREQ0
               ELSE IF( FREQ.GT.1 )THEN
                  FREQ = FREQ-1
               ENDIF
               IF( SWRDBG )THEN
                  NOTE(8:) = '> 2502'
                  CALL FSLEN(CLIST,128,LAST)
                  CALL REDTRK(CLIST,LAST,NOTE,*13000)
               ENDIF
            ELSE
               CLIST(CPOINT:) = 'implies price of row '//
     1                           NAME(IRNAME+ROW)
               L = CPOINT + 23 + NAMELN
               IF( VPL.GT.VALUE(IPL+ROW) + VTOLAB )THEN
                  VALUE(IPL+ROW) = VPL
                  SWRED = .TRUE.
                  CLIST(L:) = '>='
                  CALL FR2C12(CLIST(L+2:),VPL)
                  IF( FREQ.EQ.1 )THEN
                     CALL REDTXT(CLIST,*13000)
                     FREQ = FREQ0
                  ELSE IF( FREQ.GT.1 )THEN
                     FREQ = FREQ-1
                  ENDIF
                  IF( SWRDBG )THEN
                     NOTE(8:) = '>> 2502'
                     CALL FSLEN(CLIST,128,LAST)
                     CALL REDTRK(CLIST,LAST,NOTE,*13000)
                  ENDIF
               ENDIF
               IF( VPU.LT.VALUE(IPU+ROW) - VTOLAB )THEN
                  VALUE(IPU+ROW) = VPU
                  SWRED = .TRUE.
                  CLIST(L:) = '<='
                  CALL FR2C12(CLIST(L+2:),VPU)
                  IF( FREQ.EQ.1 )THEN
                     CALL REDTXT(CLIST,*13000)
                     FREQ = FREQ0
                  ELSE IF( FREQ.GT.1 )THEN
                     FREQ = FREQ-1
                  ENDIF
                  IF( SWRDBG )THEN
                     NOTE(8:) = '>>> 2502'
                     CALL FSLEN(CLIST,128,LAST)
                     CALL REDTRK(CLIST,LAST,NOTE,*13000)
                  ENDIF
               ENDIF
            ENDIF
2500     CONTINUE
C _________________________
C  READY FOR NEXT COLUMN
C ~~~~~~~~~~~~~~~~~~~~~~~~~
2900  CONTINUE
C
      IF( SWRDBG )THEN
         PRINT *,' END OF REDCOL'
         CALL REDEBG
         PRINT *,' ABORT (N/Y)? '
         READ(*,'(A1)')CHAR
         IF( CHAR.EQ.'Y' )RETURN 2
      ENDIF
      RETURN
C
13000 CONTINUE
      IF( SWRDBG )PRINT *,' FROM REDCOL...'
      RETURN 2
C
C ** REDCOL ENDS HERE
      END
      SUBROUTINE REDROW(CLIST,*,*)
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
C Test rows (using primal ranges set by REDCOL)
C    First alternate return is for infeasibility.
C    Second alternate return is for i/o error.
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*64  NOTE
      CHARACTER*1   CHAR
C ::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      NOTE = 'REDROW'
C ::: LOOP OVER ROWS :::
      DO 3000 I=1,NROWS
         IF( I.EQ.ROWDBG )THEN
            PRINT *,' ROW ',NAME(IRNAME+I),'...REDROW'
            CALL GDEBUG
         ENDIF
         IF( (INDEX(IFREE+I).NE.0 .OR. INDEX(IRSTAT+I).GT.0) .OR.
C                           :                         :...NOT IN
C                           :...FORCING                  SUBMATRIX
     1       (VALUE(IYL+I).LE.-VINF .AND. VALUE(IYU+I).GE.VINF)
C                               :...FREE...:
     2     )GOTO 3000
C ROW I IS NOT FORCING AND IN SUBMATRIX AND NOT FREE
         VL = VALUE(IYL + I)
         VU = VALUE(IYU + I)
         VYMIN = VALUE(IYMIN + I)
         VYMAX = VALUE(IYMAX + I)
         IF( SWRDBG .AND. I.EQ.ROWDBG )THEN
            PRINT *,VL,VU,VYMIN,VYMAX
            PAUSE = PAUSE-1
         ENDIF
         IF( VYMIN.GT.VU + VTAINF + VTRINF*ABS(VU) .OR.
     1       VYMAX.LT.VL - VTAINF - VTRINF*ABS(VL) )THEN
C !PRIMAL INFEASIBLE
            WRITE(OUTPUT,2001,ERR=13000)NAME(IRNAME+I)
            IF( OUTPUT.NE.TTYOUT )WRITE(*,2001)NAME(IRNAME+I)
2001        FORMAT(' !Primal infeasibility detected with row ',A16)
            IF( VYMIN.GT.VU + VTAINF + VTRINF*ABS(VU) )THEN
               CLIST = 'MIN'
               WRITE(OUTPUT,2002,ERR=13000)VYMIN,VU
2002           FORMAT(' MIN ROW VALUE =',G15.8,' > ',G15.8,
     1                ' = UP BOUND')
            ELSE
               CLIST = 'MAX'
               WRITE(OUTPUT,2003,ERR=13000)VYMAX,VL
2003           FORMAT(' MAX ROW VALUE =',G15.8,' < ',G15.8,
     1                ' = LO BOUND')
            ENDIF
            RCODE = 0
            MAX = NROWS
C USE MYOPIC ROW RANGE SPACE TO GET COLUMNS IN ROW I
            CALL GETROW(I,I4VAL(IYMIN+1),VALUE(IYMAX+1),MAX,NZ,RCODE)
            IF( NZ.EQ.0 )THEN
               WRITE(OUTPUT,2010,ERR=13000)
2010           FORMAT(' ...Row is null (in submatrix)')
               RETURN 1
            ENDIF
C   GIVE TABLE OF RELEVANT BOUNDS IN THIS EQUATION
            WRITE(OUTPUT,2011,ERR=13000)
2011        FORMAT(' Here is how this was inferred'/
     1  /                           T31,'Relevant',T46,'Value of Term'
     2  /' Activity',T17,'Coefficient',T31,'Bound',T46,'(Coef * Bound)'
     X             )
            VSUM = 0.
            NZERO = 0
            DO 2100 J=1,NZ
               COL   = I4VAL(IYMIN+J)
               VCOEF = VALUE(IYMAX+J)
               CALL GETBND('COL ',COL,VL,VU)
               IF( (CLIST.EQ.'MIN' .AND. VCOEF.GT.0.) .OR.
     1             (CLIST.EQ.'MAX' .AND. VCOEF.LT.0.) )THEN
                  VX = VALUE(IXL + COL)
                  IF( ABS(VX).LE.VTOLAB .AND. VX.LE.VL )THEN
                     NZERO = NZERO + 1
                     GOTO 2100
                  ENDIF
                  CHAR = 'L'
               ELSE
                  VX = VALUE(IXU + COL)
                  IF( ABS(VX).LE.VTOLAB .AND. VX.GE.VU )THEN
                     NZERO = NZERO + 1
                     GOTO 2100
                  ENDIF
                  CHAR = 'U'
               ENDIF
               V = VCOEF*VX
               VSUM = VSUM + V
               WRITE(OUTPUT,2020,ERR=13000)NAME(ICNAME+COL),
     1                                       VCOEF,CHAR,VX,V
2020           FORMAT(1X,A16,1X,G12.5,T31,A1,'=',G12.5,T47,G13.5)
2100        CONTINUE
            IF( RCODE.NE.0 )THEN
               WRITE(OUTPUT,2101,ERR=13000)
2101           FORMAT(' ...There are more, but not enough space to',
     1                     ' list them.')
            ELSE
               WRITE(OUTPUT,2102,ERR=13000)VSUM
2102           FORMAT(T42,'Sum =',G13.5)
               IF( NZERO.GT.0 )WRITE(OUTPUT,2103,ERR=13000)NZERO
2103           FORMAT(' ...',I6,
     1                ' original zero bound values not listed.')
            ENDIF
            RETURN 1
         ENDIF
C
         CHAR = ' '
         CLIST = 'Row '//NAME(IRNAME+I)
C
         IF(VYMIN.GE.VU - VTOLAB)THEN
C =ROW I FORCES ADJACENT ACTIVITIES TO YMIN VALUES
             INDEX(IFREE + I) = -PASS
             CHAR = 'L'
             V = VYMIN
             CLIST(NAMELN+6:) =
     1           'forces adjacent activity levels to their extremes'
         ELSE IF(VYMAX.LE.VL + VTOLAB)THEN
C =ROW I FORCES ADJACENT ACTIVITIES TO YMAX VALUES
             INDEX(IFREE + I) = PASS
             CHAR = 'U'
             V = VYMAX
             CLIST(NAMELN+6:) =
     1           'forces adjacent activity levels to their extremes'
         ELSE
C    TEST FOR PRIMAL REDUNDANCY
            IF(VYMAX.LE.VU .AND. VALUE(IPL+I).LT.0.0
     1                     .AND. I.NE.OBJNUM)THEN
C ?ROW I HAS REDUNDANT UPPER BOUND...IE, IS NEVER BINDING
                CLIST(NAMELN+6:) = 'upper bound is never binding'
                CLIST(NAMELN+34:)= '...price >= 0'
                CHAR='R'
                V = VU
                VALUE(IPL + I) = 0.0
            ENDIF
            IF(VYMIN.GE.VL .AND. VALUE(IPU+I).GT.0.0
     1                     .AND. I.NE.OBJNUM)THEN
C ?ROW I HAS REDUNDANT LOWER BOUND...IE, IS NEVER BINDING
                CLIST(NAMELN+6:) = 'lower bound is never binding'
                CLIST(NAMELN+34:)= '...price <= 0'
                CHAR = 'R'
                V = VL
                VALUE(IPU + I) = 0.0
            ENDIF
         ENDIF
         IF(CHAR.EQ.' ')GOTO 3000
C
         SWRED = .TRUE.
         IF( FREQ.EQ.1 )THEN
            CALL REDTXT(CLIST,*13000)
            FREQ = FREQ0
         ELSE IF( FREQ.GT.1 )THEN
            FREQ = FREQ-1
         ENDIF
         IF( SWRDBG )THEN
            NOTE(8:) = '2103'
            CALL FSLEN(CLIST,128,LAST)
            CALL REDTRK(CLIST,LAST,NOTE,*13000)
         ENDIF
         IF( CHAR.EQ.'L' .OR. CHAR.EQ.'U' )THEN
            NRFORC = NRFORC + 1
            VALUE(IPL + I) = 0.
            VALUE(IPU + I) = 0.
         ENDIF
C NEXT ROW (I)
3000  CONTINUE
C
      IF( SWRDBG )THEN
         PRINT *,' END OF REDROW'
         CALL REDEBG
         PRINT *,' ABORT (N/Y)? '
         READ(*,'(A1)')CHAR
         IF( CHAR.EQ.'Y' )RETURN 2
      ENDIF
      RETURN
C
13000 IF(SWRDBG)PRINT *,' FROM REDROW...'
      RETURN 2
C
C ** REDROW ENDS HERE
      END
