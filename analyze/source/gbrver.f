C             ::: GBRVER.FOR  8-24-95 :::
C
C Created 8-9-95
C       8-15-95...Row loop needed to have I.NE.OBJNUM in GVERFY
C       8-20-95...Added VTOL0C to GVERFY and GBREFR args
C                 Removed VRPINF from GBREFR args
C                 Separated accumulations into + and -
C
C This contains the following GETMAT subroutines.
C
C   GVERFY...verifies solution values (need not be basic)
C   GBREFR...refreshes primal and/or dual values
C   GBRDIF...computes difference between old and new values
C   GPRDIF...prints line of discrepancy
C
      SUBROUTINE GVERFY(SWBASE,SWVALS, ZY,MAXZY,CLIST,
     1    SWMSG,FREQ0,MAXTIM,
     2    VAXDIF,VRXDIF,VAPDIF,VRPDIF,VAXINF,VRXINF,VAPINF, VTOL0C,
     3    NPRIML,VYAVG,VYMAX,RNAME,  NDUAL,VDAVG,VDMAX,CNAME,
     4    NEWST, RC,RCNAME,RCKEY,VRC,VRCDIF, *)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This verifies primal and dual solution values.
C  ...Alternate return is SYSERR or IO error
C
      LOGICAL*1         SWBASE,SWVALS,SWMSG
      CHARACTER*4                         RC
      CHARACTER*16      RNAME,CNAME,NEWST,RCNAME
      CHARACTER*1                         RCKEY
      DOUBLE PRECISION  ZY(MAXZY)
      CHARACTER*128     CLIST
C
C Input:
C  SWBASE = T means verify base
C  SWVALS = T means verify values
C  ZY used locally...MAXZY MUST BE >= NROWS
C  CLIST is used locally for output
C  SWMSG  = Message switch
C  FREQ0  = Output frequency
C  MAXTIM = Elapsed number of seconds to ask user for continuation
C  VAXDIF = Absolute tolerance to declare levels different
C  VRXDIF = Relative tolerance to declare levels different
C  VAPDIF = Absolute tolerance to declare prices different
C  VRPDIF = Relative tolerance to declare prices different
C  VAXINF = Absolute tolerance to declare level infeasible
C  VRXINF = Relative tolerance to declare level infeasible
C  VAPINF = Absolute tolerance to declare price infeasible
C  VTOL0C = Tolerance for coefficient to be nonzero
C
C Output if base values not ok:
C  NEWST  = 'BASE CHANGED'
C
C Output if base values ok:
C  NPRIML = Number of rows whose primal level (Y) has discrepancy
C  VYAVG  = Average of primal solution discrepancy magnitudes
C  VYMAX  = Worst discrepancy in primal solution values (Y-YNEW)
C  RNAME  = Name of row with worst primal solution discrepancy (VYMAX)
C  NDUAL  = Number of columns whose dual value (D) has discrepancy
C  VDAVG  = Average of dual solution discrepancy magnitudes
C  VDMAX  = Worst discrepancy in dual solution values (D-DNEW)
C  CNAME  = Name of col with worst dual solution discrepancy (VDMAX)
C  NEWST  = New solution status (SOLST unchanged in any case)
C  RC     = ROW or COL associated with RCNAME
C  RCNAME = Row or column name associated with NEWST
C  RCKEY  = Rim key associated with RCNAME (X, Y, P, or D)
C  VRC    = Value associated with RCNAME (that caused status)
C  VRCDIF = Difference in VRC value (may be 0)
C  ...Alternate return is SYSERR
C
C SUBMATRIX WILL BE CLEARED IF WE GET PAST BASE...WILL ADD DISCREPANTS
C CALLER MUST SET SUBMATRIX AFTER RETURN (IF NEWST <> 'BASE CHANGED')
C
C LOCAL
      CHARACTER*1 CHAR,STAT
C ::::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      NEWST = ' '
      IF( .NOT.SWBASE )GOTO 90
      PRINT *,' VERIFYING BASE'
C FIRST, VERIFY SOME SETTINGS (MIGHT HAVE BEEN CORRUPTED)
C OBJ TEST (CAN COME BACK TO HERE, IF USER SETS OBJ FROM PROMPT)
10    CONTINUE
        RNAME = OBJNAM
        NC    = OBJNUM
C MUST TEMPORARILY CHANGE OBJNAM TO FORCE GETNUM TO LOOK IT UP
        OBJNAM = ' '
        CALL GETNUM('ROW ',RNAME,NR)
        OBJNAM = RNAME
        IF( NC.GT.0 )THEN
           CNAME = NAME(IRNAME+NC)
        ELSE
           CNAME = ' '
        ENDIF
C NR = NUMBER OF OBJNAM (= RNAME);  NC = ROW OBJNUM, WHOSE NAME = CNAME
C TO VERIFY, MUST HAVE NR=NC > 0 (IE, NUMBERS AGREE WITH NAMES)
C AND ROW TYPE MUST BE FREE
        CHAR = ' '
        IF( NR.GT.0 )CALL GETYPE(NR,CHAR)
        IF( RNAME.EQ.CNAME .AND. CHAR.EQ.'N' )GOTO 29
C
        NEWST = 'BASE CHANGED'
        PRINT *,' OBJ ROW CORRUPTED...'
C WILL FIRST SEEK CHANGE (IF ONLY 1 CHOICE)
        IF( CHAR.NE.'N' )GOTO 11
C OBJNAM IS A POSSIBILITY (IT'S A FREE ROW AND WAS NAMED)
C ...SEE IF OBJNUM IS ALSO A POSSIBILITY
        CHAR = ' '
        IF( NC.GT.0 )CALL GETYPE(NC,CHAR)
        IF( CHAR.EQ.'N' )GOTO 15
C OBJNUM IS NOT A POSSIBILITY, SO OBJNAM IS ONLY CHOICE
        OBJNUM = NR
        GOTO 29
11      CONTINUE
C OBJNAM IS NOT A POSSIBILITY...SEE IF OBJNUM IS
        IF( NC.GT.0 )CALL GETYPE(NC,CHAR)
        IF( CHAR.NE.'N' )GOTO 20
C OBJNUM IS A POSSIBILITY, AND IS ONLY CHOICE
        OBJNAM = CNAME
        GOTO 29
15      CONTINUE
C WE GET HERE IFF BOTH OBJNAM AND OBJNUM ARE POSSIBILITIES
        PRINT *,' ...COULD BE 1>',RNAME
        PRINT *,'          OR 2>',CNAME
        PRINT *,'  ENTER 1 OR 2 (* TO REJECT BOTH) '
        CALL FGTCHR('12*',' ',CHAR)
        IF( CHAR.EQ.'1' )THEN
           OBJNUM = NR
        ELSE IF( CHAR.EQ.'2' )THEN
           OBJNAM = CNAME
        ELSE
           GOTO 20
        ENDIF
        PRINT *,' ...SET OBJ = ',OBJNAM
        GOTO 29
20      CONTINUE
C WE GET HERE IFF NEITHER OBJNAM NOR OBJNUM IS RIGHT
        OBJNUM = 0
        PRINT *,' ENTER OBJ NAME (NOTHING IF YOU DO NOT KNOW) '
        READ(*,'(A79)')CLIST
        IF( CLIST.EQ.' ' )THEN
C USER DID NOT ENTER OBJ NAME
           PRINT *,' LOOK AT FREE ROWS (WITH DISPLAY) AND RETURN',
     1             ' IF YOU CAN SPECIFY OBJ ROW'
           RETURN
        ENDIF
        F = 1
        L = 79
        CALL FTOKEN(CLIST,F,L,RNAME,16,CHAR)
        CALL GETNUM('ROW ',RNAME,NR)
        IF( NR.EQ.0 )THEN
           PRINT *,' ** ROW ',RNAME,' NOT FOUND'
           GOTO 20
        ENDIF
        CALL GETYPE(NR,CHAR)
        IF( CHAR.NE.'N' )THEN
           PRINT *,' ** ROW ',RNAME,' IS NOT A FREE ROW'
           GOTO 20
        ENDIF
        OBJNAM = RNAME
        OBJNUM = NR
29    CONTINUE
C WE GET HERE IFF OBJ IS RESOLVED OR WAS NO PROBLEM
C CHECK ROW WITH MAX LENGTH (CANNOT BE FREE ROW)
      NUMBER = RMAXLN
      CALL GETYPE(NUMBER,CHAR)
      IF( CHAR.EQ.'N' )THEN
         NEWST = 'BASE CHANGED'
         RNAME = NAME(IRNAME+NUMBER)
         PRINT *,' ROW WITH MAX COUNT IS FREE ROW: ',RNAME(:NAMELN)
         PRINT *,' ...THIS SHOULD NOT BE...WILL RE-COMPUTE HERE'
         NZMAX = 0
         DO 50 I=1,NROWS
            CALL GETYPE(I,CHAR)
            IF( CHAR.EQ.'N' )GOTO 50
            NZ = INDEX(IRINFO+I)
            IF( NZ.GT.NZMAX )THEN
               RMAXLN = I
               NZMAX = NZ
            ENDIF
50       CONTINUE
         RNAME = NAME(IRNAME+RMAXLN)
         PRINT *,' NON-FREE ROW WITH MAX COUNT IS ',
     1            RNAME(:NAMELN),' =',NZMAX
      ENDIF
C SOLUTION STATUS
      IF( SOLST.NE.'INFEASIBLE' .AND. SOLST.NE.'FEASIBLE' .AND.
     1    SOLST.NE.'OPTIMAL '   .AND. SOLST.NE.'UNBOUNDED' )THEN
         PRINT *,' SOLUTION STATUS = ',SOLST,'...WILL SET NOW'
         NEWST = ' '
         CALL GSETST(VAPINF,VAXDIF,ZY,MAXZY, NEWST,RC,I,VP)
         SOLST = NEWST
         NEWST = 'BASE CHANGED'
         IF( SWMSG )THEN
            CLIST = ' ...SOLUTION STATUS = '//SOLST
            CALL FSLEN(CLIST,80,LAST)
            PRINT *,CLIST(:LAST)
         ENDIF
      ENDIF
      IF( NEWST.EQ.'BASE CHANGED' )RETURN
      IF( SWMSG )PRINT *,' BASE VERIFIED'
C ====================== END BASE CHECK ==========================
90    CONTINUE
      IF( .NOT.SWVALS )RETURN
      IF( MAXZY.LT.NROWS )RETURN 1
C =================== VERIFY SOLUTION VALUES =====================
C IF THERE'S ENOUGH SPACE, WE'LL ACCUMULATE + AND - SEPARATELY
      INEG = IVFREE/2 + 1
      IF( 2*(INEG+NROWS+1).GE.MAXVAL )INEG = 0
C ...INEG = 0 IF NOT ENOUGH SPACE;  ELSE, INEG POINTS TO ZVALUE
C INITIALIZE
      CALL FCLOCK(TIME0)
      CALL GMPCLR
      DO 100 I=1,NROWS
         ZY(I) = 0.
         IF( INEG.GT.0 )ZVALUE(INEG+I)=0.
100   CONTINUE
      NPRIML = 0
      VYAVG  = 0.
      VYMAX  = 0.
      RNAME  = ' '
      NDUAL  = 0
      VDAVG  = 0.
      VDMAX  = 0.
      CNAME  = ' '
      NEWST  = 'OPTIMAL'
      RCNAME = ' '
      FREQ = FREQ0
C
      IF( SWMSG )PRINT *,' VERIFYING COLUMNS'
      IF( FREQ0.GT.0 )THEN
C WRITE HEADER TO OUTPUT
         CLIST = 'Column Reduced Costs'
         CALL GPRDIF(-1,FREQ0,CLIST,VP,VNEW,VDIF,*1300)
      ENDIF
      RC = 'COL '
      I1 = INONZ + 2*BASE(ICINFO)
C
C ::: LOOP OVER COLS TO ACCUMULATE Y AND VERIFY D :::
      DO 900 J=1,NCOLS
         CALL FCLOCK(TIME)
         ELAPSE = TIME - TIME0
         IF( ELAPSE.GT.MAXTIM )THEN
            PRINT *,J,' COLUMNS CHECKED',NDUAL,' HAVE DISCREPANCY'
            CALL FLTIME(ELAPSE,MAXTIM,*9000)
            TIME0 = TIME
         ENDIF
         NZ = BASE(ICINFO + J) - BASE(ICINFO + J-1)
         I2 = I1 + 2*NZ - 1
         CALL GETSOL('COL ',J,VX,VP,STAT,STNUM)
         ZPOS = 0.
         ZNEG = 0.
         IF( NZ.GT.0 )THEN
C    ::: LOOP OVER COL J'S NONZEROES :::
            DO 500 I=I1,I2,2
               CALL GETVAL(V,I+1)
               IF( ABS(V).LT.VTOL0C )GOTO 500
               ROW = INDEX(I)
               VNEW = V*VX
               IF( VX.LT.-VTOL0C .AND. INEG.GT.0 )THEN
                  ZVALUE(INEG+ROW) = ZVALUE(INEG+ROW) + VNEW
               ELSE
                  ZY(ROW) = ZY(ROW) + VNEW
               ENDIF
               CALL GETSOL('ROW ',ROW,VY,VPROW,CHAR,STNUM)
               VNEW = -VPROW*V
               IF( VPROW.GT.0. )THEN
                  ZPOS = ZPOS + VNEW
               ELSE
                  ZNEG = ZNEG + VNEW
               ENDIF
500         CONTINUE
         ENDIF
         ZP = ZPOS + ZNEG
C GET NEW DUAL VALUE AND DIFFERENCE (VDIF)
         CALL GBRDIF(VAPDIF,VRPDIF,VP,ZP,VNEW,VDIF,*510)
         GOTO 550
510      CONTINUE
C DISCREPANCY
         CLIST = '  '//NAME(ICNAME+J)
         CALL GPRDIF(FREQ,FREQ0,CLIST,VP,VNEW,VDIF,*1300)
         CALL GPUTMP('COL ',J,SWVALS)
         NDUAL = NDUAL+1
         VDAVG = VDAVG + ABS(VDIF)
         IF( ABS(VDIF).GT.ABS(VDMAX) )THEN
            VDMAX = VDIF
            CNAME = NAME(ICNAME+J)
         ENDIF
         VP = VNEW
550      CONTINUE
C WE GET HERE WHETHER OR NOT DISCREPANCY
         IF( NEWST.EQ.'INFEASIBLE' )GOTO 890
C DETERMINE SOLUTION STATUS
         CALL GETBND('COL ',J,VL,VU)
         IF( VX.LT.VL - VAXINF - VRXINF*ABS(VL) .OR.
     1       VX.GT.VU + VAXINF + VRXINF*ABS(VU) )THEN
C PRIMAL IS INFEASIBLE
            RCNAME= NAME(ICNAME+J)
            RCKEY = 'X'
            VRC   = VX
            VRCDIF= 0.
         ELSE
            IF( (STAT.NE.'L' .AND. -OPT*VP.GT. VAPINF) .OR.
     1          (STAT.NE.'U' .AND. -OPT*VP.LT.-VAPINF) )THEN
C DUAL IS INFEASIBLE
               IF( NEWST.EQ.'OPTIMAL' .OR. ABS(VDIF).GT.ABS(VRCDIF)
     1           )THEN
                  RCNAME = NAME(ICNAME+J)
                  RCKEY  = 'D'
                  VRC    = VP
                  VRCDIF = VDIF
               ENDIF
               NEWST = 'FEASIBLE'
            ENDIF
         ENDIF
890      CONTINUE
C ADVANCE TO NEXT COL
         I1 = I2+1
900   CONTINUE
C
      IF( NDUAL.GT.1 )VDAVG = VDAVG/NDUAL
C
      IF( SWMSG )PRINT *,' VERIFYING ROWS'
      IF( FREQ0.GT.0 )THEN
         CALL GPRDIF(-1,FREQ0,' ',VP,VNEW,VDIF,*1300)
         CLIST = 'Row Levels'
         CALL GPRDIF(-1,FREQ0,CLIST,VP,VNEW,VDIF,*1300)
      ENDIF
C GET ROW DIFFERENCES AND UPDATE STATUS (NEWST)
      DO 1900 I=1,NROWS
         CALL FCLOCK(TIME)
         ELAPSE = TIME - TIME0
         IF( ELAPSE.GT.MAXTIM )THEN
            PRINT *,I,' ROWS CHECKED',NPRIML,' HAVE DISCREPANCY'
            CALL FLTIME(ELAPSE,MAXTIM,*9000)
            TIME0 = TIME
         ENDIF
         CALL GETSOL('ROW ',I,VY,VP,STAT,STNUM)
         IF( INEG.GT.0 )ZY(I) = ZY(I) + ZVALUE(INEG+I)
         CALL GBRDIF(VAXDIF,VRXDIF,VY,ZY(I),VNEW,VDIF,*1510)
         GOTO 1550
1510     CONTINUE
C DISCREPANCY
         CLIST = '  '//NAME(IRNAME+I)
         CALL GPRDIF(FREQ,FREQ0,CLIST,VY,VNEW,VDIF,*1300)
         CALL GPUTMP('ROW ',I,SWVALS)
         NPRIML = NPRIML+1
         VYAVG  = VYAVG + ABS(VDIF)
         IF( ABS(VDIF).GT.ABS(VYMAX) )THEN
            VYMAX = VDIF
            RNAME = NAME(IRNAME+I)
         ENDIF
         VY = VNEW
1550     CONTINUE
C WE GET HERE WHETHER OR NOT DISCREPANCY
C DETERMINE SOLUTION STATUS
         CALL GETBND('ROW ',I,VL,VU)
         IF( VY.LT.VL - VAXINF - VRXINF*ABS(VL)  .OR.
     1       VY.GT.VU + VAXINF + VRXINF*ABS(VU) )THEN
C PRIMAL IS INFEASIBLE
            IF( NEWST.NE.'INFEASIBLE' .OR. ABS(VDIF).GT.ABS(VRCDIF)
     1        )THEN
               RCNAME = NAME(IRNAME+I)
               RC     = 'ROW '
               RCKEY  = 'Y'
               VRC    = VY
               VRCDIF = VDIF
            ENDIF
            NEWST = 'INFEASIBLE'
         ELSE IF( (NEWST.EQ.'OPTIMAL') .AND. (I.NE.OBJNUM) .AND.
     1       (    (STAT.NE.'L' .AND. -OPT*VP.GT. VAPINF)
     2        .OR.(STAT.NE.'U' .AND. -OPT*VP.LT.-VAPINF)
     3       )  )THEN
C DUAL IS INFEASIBLE
            NEWST  = 'FEASIBLE'
            RCNAME = NAME(IRNAME+I)
            RC     = 'ROW '
            RCKEY  = 'P'
            VRC    = VP
            VRCDIF = 0.
         ENDIF
1900  CONTINUE
C
      IF( FREQ0.GT.0 )CALL GPRDIF(-1,FREQ0,' ',VP,VNEW,VDIF,*1300)
      IF( NPRIML.GT.1 )VYAVG = VYAVG/NPRIML
C
9000  RETURN
C
1300  RETURN 1
C
C ** GVERFY ENDS HERE
      END
      SUBROUTINE GBREFR(SWREPL,SWPRML,SWDUAL,ZXY,MAXLST,CLIST,
     1    SWMSG,FREQ0,MAXTIM,
     2    VAXDIF,VRXDIF,VAPDIF,VRPDIF,VAXINF,VRXINF,VAPINF, VTOL0C,
     3    RNUMP,RAVGP,RMAXP,RNAMEP, CNUMP,CAVGP,CMAXP,CNAMEP,
     4    RNUMD,RAVGD,RMAXD,RNAMED, CNUMD,CAVGD,CMAXD,CNAMED,
     5    CNUMB,CAVGB,CMAXB,CNAMEB,
     6    NEWST, RC,RCNAME,RCKEY,VRC,VRCDIF, *)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C ...NEED DCFLIP FOR OUTPUT FILE
C
C This refreshes primal and/or dual values for basis (presumed setup).
C CALLER IS RESPONSIBLE FOR CLEARING SUBMATRIX AND SETTING UPON RETURN
C (if SWREPL=FALSE).
C
      LOGICAL*1      SWREPL,SWPRML,SWDUAL,SWMSG
      DOUBLE PRECISION  ZXY(MAXLST)
      CHARACTER*128  CLIST
      INTEGER        FREQ0, MAXTIM
      INTEGER        RNUMP, CNUMP, RNUMD, CNUMD, CNUMB
      REAL           RAVGP, CAVGP, RAVGD, CAVGD, CAVGB
      REAL           RMAXP, CMAXP, RMAXD, CMAXD, CMAXB
      CHARACTER*16   RNAMEP,CNAMEP,RNAMED,CNAMED,CNAMEB,NEWST,RCNAME
      CHARACTER*4    RC
      CHARACTER*1    RCKEY
C
C Input:
C  SWREPL = T means replace values and do not change submatrix
C         = F means do not replace values, but put violations in submat
C  SWPRML = T means refresh primal values
C  SWDUAL = T means refresh dual values
C  ZXY used locally...MAXLST MUST BE >= NROWS
C  CLIST is used locally for output
C  SWMSG  = Message switch
C  FREQ0  = Output frequency
C  MAXTIM = Elapsed number of seconds to ask user for continuation
C  VAXDIF = Absolute tolerance to declare basic levels different
C  VRXDIF = Relative tolerance to declare basic levels different
C  VAPDIF = Absolute tolerance to declare nonbasic prices different
C  VRPDIF = Relative tolerance to declare nonbasic prices different
C  VAXINF = Absolute tolerance to declare basic level infeasible
C  VRXINF = Relative tolerance to declare basic level infeasible
C  VAPINF = Absolute tolerance to declare nonbasic price infeasible
C  VTOL0C = Tolerance for coefficient to be nonzero
C
C Output:
C  RNUMP  = Number of rows with primal discrepancy
C  RAVGP  = Average of magnitudes of primal row discrepancies
C           (= 0 if RNUMP=0)
C  RMAXP  = Worst primal row discrepancy
C  RNAMEP = Name of row with worst primal discrepancy
C  CNUMP  = Number of cols with primal discrepancy
C  CAVGP  = Average of magnitudes of primal col discrepancies
C           (= 0 if CNUMP=0)
C  CMAXP  = Worst primal col discrepancy
C  CNAMEP = Name of col with worst primal discrepancy
C  RNUMD  = Number of rows with dual discrepancy
C  RAVGD  = Average of magnitudes of dual row discrepancies
C           (= 0 if RNUMD=0)
C  RMAXD  = Worst dual row discrepancy
C  RNAMED = Name of row with worst dual discrepancy
C  CNUMD  = Number of cols with dual discrepancy
C  CAVGD  = Average of magnitudes of dual col discrepancies
C           (= 0 if CNUMD=0)
C  CMAXD  = Worst dual col discrepancy
C  CNAMED = Name of col with worst dual discrepancy
C  CNUMB  = Number of basic cols with dual discrepancy
C  CAVGB  = Average of magnitudes of dual basic col discrepancies
C           (= 0 if CNUMB=0)
C  CMAXB  = Worst dual basic col discrepancy
C  CNAMEB = Name of basic col with worst dual discrepancy
C  NEWST  = New solution status (SOLST changed iff SWREPL is True)
C  RC     = ROW or COL associated with RCNAME
C  RCNAME = Row or column name associated with NEWST
C  RCKEY  = Rim key associated with RCNAME (X, Y, P, or D)
C  VRC    = Value associated with RCNAME (that caused status)
C  VRCDIF = Difference in VRC value (may be 0)
C ...Alternate return is SYSERR.
C LOCAL
      CHARACTER*1  STAT
      CHARACTER*4  ROWCOL
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      IF( MAXLST.LT.NROWS )THEN
         PRINT *,' ** SYSERR GBREFR...MAXLST =',MAXLST,' <',NROWS
         RETURN 1
      ENDIF
      CALL FCLOCK(TIME0)
C INITIALIZE TALLIES
      RNUMP  = 0
      RAVGP  = 0.
      RMAXP  = 0.
      RNAMEP = ' '
      CNUMP  = 0
      CAVGP  = 0.
      CMAXP  = 0.
      CNAMEP = ' '
      RNUMD  = 0
      RAVGD  = 0.
      RMAXD  = 0.
      RNAMED = ' '
      CNUMD  = 0
      CAVGD  = 0.
      CMAXD  = 0.
      CNAMED = ' '
      CNUMB  = 0
      CAVGB  = 0.
      CMAXB  = 0.
      CNAMEB = ' '
      NEWST  = ' '
      RCNAME = ' '
      FREQ   = FREQ0
C IF THERE'S ENOUGH SPACE, WE'LL ACCUMULATE + AND - SEPARATELY
      INEG = IVFREE/2 + 1
      IV0  = IVFREE
      IF( 2*(INEG+NROWS+1).LT.MAXVAL )THEN
C OK, INEG POINTS TO ZVALUE AND WE'LL (TEMPORARILY) ADVANCE IVFREE
         IVFREE = IVFREE + 2*NROWS + 2
      ELSE
C NOT ENOUGH SPACE
         INEG = 0
      ENDIF
C === FROM THIS POINT, MUST RETURN VIA 9000 TO RESTORE IVFREE ===
      IF( .NOT.SWPRML )GOTO 1000
C ========== REFRESH PRIMAL VALUES ==========
      IF( SWMSG )PRINT *,' REFRESHING PRIMAL VALUES'
      IF( FREQ0.GT.0 )THEN
C WRITE HEADER TO OUTPUT
         CLIST = 'Primal Levels'
         CALL GPRDIF(-1,FREQ0,CLIST,VP,VNEW,VDIF,*1300)
      ENDIF
C PRESUME SOLUTION IS FEASIBLE
      NEWST  = 'FEASIBLE'
C FORM RHS = [Y_N - NX_N   -N*X_N]
C             ==========   ======
C             TIGHT ROWS   SURPLUS ROWS
      DO 100 I=1,NROWS
         ZXY(I) = 0.
         IF( INEG.GT.0 )ZVALUE(INEG+I) = 0.
         CALL GETSOL('ROW ',I,VX,VP,STAT,STNUM)
         IF( STAT.EQ.'U' .OR. STAT.EQ.'L' )THEN
            IF( VX.LT.0. .AND. INEG.GT.0 )THEN
               ZVALUE(INEG+I) = VX
            ELSE
               ZXY(I) = VX
            ENDIF
         ENDIF
100   CONTINUE
C NOW ZXY = [Y_N  0]   (IN COMMENTS, ZXY INCLUDES ZVALUE IF INEG > 0)
C
C  ::: LOOP OVER NONBASIC COLS TO COMPLETE RHS INITIALIZATION :::
      DO 300 J=1,NCOLS
         CALL GETSOL('COL ',J,VX,VP,STAT,STNUM)
         IF( STAT.EQ.'B' .OR. STAT.EQ.'I' .OR. ABS(VX).LT.VTOLAB
     1     )GOTO 300
C COLUMN J IS NONBASIC AT NONZERO LEVEL
         I1 = INONZ + 2*BASE(ICINFO + J-1)
         I2 = INONZ + 2*BASE(ICINFO + J) - 1
C      ::: SUBTRACT N*X_N :::
         DO 200 I=I1,I2,2
            CALL GETVAL(V,I+1)
            IF( ABS(V).LT.VTOL0C )GOTO 200
            ROW = INDEX(I)
            V = -V*VX
            IF( V.LT.0. .AND. INEG.GT.0 )THEN
               ZVALUE(INEG+ROW) = ZVALUE(INEG+ROW) + V
            ELSE
               ZXY(ROW) = ZXY(ROW) + V
            ENDIF
200      CONTINUE
300   CONTINUE
C
      IF( INEG.GT.0 )THEN
C COMBINE POS + NEG (= ZXY + ZVALUE)
         DO 350 I=1,NROWS
350      ZXY(I) = ZXY(I) + ZVALUE(INEG+I)
      ENDIF
C
C NOW ZXY=[Y_N - NX_N  -N*X_N], SO FTRAN(ZXY) = BASIC LEVELS
      CALL GFTRAN(ZXY,NROWS)
C NOW ZXY(I) = -LEVEL OF I-TH BASIC
C              :...GFTRAN RETURNS -FTRAN(ZXY) (= RATE)
C
C ::: LOOP OVER BASICS :::
      DO 500 I=1,NROWS
         CALL FCLOCK(TIME)
         ELAPSE = TIME - TIME0
         IF( ELAPSE.GT.MAXTIM )THEN
            N = RNUMP+CNUMP
            PRINT *,J,' BASICS CHECKED',N,' HAVE DISCREPANCY'
            CALL FLTIME(ELAPSE,MAXTIM,*9000)
            TIME0 = TIME
         ENDIF
         CALL GETBVP(I,ROWCOL,J)
C [ROW]COL J PIVOTS ON ROW I, SO X[Y](J) = -ZXY(I)
         CALL GETSOL(ROWCOL,J,VX,VP,STAT,STNUM)
         CALL GBRDIF(VAXDIF,VRXDIF,VX,-ZXY(I),VZ,VDIF,*410)
         GOTO 490
410      CONTINUE
C DISCREPANCY
         IF( ROWCOL.EQ.'ROW ' )THEN
            CLIST = NAME(IRNAME+J)//' Y'
         ELSE
            CLIST = NAME(ICNAME+J)//' X'
         ENDIF
         CALL GPRDIF(FREQ,FREQ0,CLIST,VX,VZ,VDIF,*1300)
C SET STAT
         CALL GETBND(ROWCOL,J,VL,VU)
         IF( VZ.LT.VL - VAXINF - VRXINF*ABS(VL) .OR.
     1       VZ.GT.VU + VAXINF + VRXINF*ABS(VU) )THEN
            STAT = 'I'
         ELSE
            STAT = 'B'
         ENDIF
         IF( SWREPL )THEN
C ...REPLACE VALUE
            CALL GPTSOL(ROWCOL,J,STAT,VZ)
         ELSE
C ...ADD TO SUBMAT
            CALL GPUTMP(ROWCOL,J,SWPRML)
         ENDIF
C UPDATE STATS
         IF( ROWCOL.EQ.'ROW ' )THEN
            RNUMP = RNUMP + 1
            RAVGP = RAVGP + ABS(VDIF)
            IF( ABS(VDIF).GT.ABS(RMAXP) )THEN
               RMAXP  = VDIF
               RNAMEP = NAME(IRNAME+J)
            ENDIF
         ELSE
            CNUMP = CNUMP + 1
            CAVGP = CAVGP + ABS(VDIF)
            IF( ABS(VDIF).GT.ABS(CMAXP) )THEN
               CMAXP  = VDIF
               CNAMEP = NAME(ICNAME+J)
            ENDIF
         ENDIF
         VX = VZ
490      CONTINUE
C WE GET HERE WHETHER OR NOT NEW VALUE IS DISCREPANCY
         IF( STAT.EQ.'I' )THEN
            IF( NEWST.NE.'INFEASIBLE'  .OR.
     1          ABS(VDIF).GT.ABS(VRCDIF) )THEN
               IF( ROWCOL.EQ.'ROW ' )THEN
                  RCNAME = NAME(IRNAME+J)
                  RCKEY  = 'Y'
               ELSE
                  RCNAME = NAME(ICNAME+J)
                  RCKEY  = 'X'
               ENDIF
               RC     = ROWCOL
               VRC    = VX
               VRCDIF = VDIF
            ENDIF
            NEWST  = 'INFEASIBLE'
         ENDIF
500   CONTINUE
      IF( FREQ0.GT.0 )CALL GPRDIF(-1,FREQ0,' ',VX,VZ,VDIF,*1300)
C =========== END OF PRIMAL REFRESH ============
1000  CONTINUE
      IF( .NOT.SWDUAL )THEN
C NO DUAL REFRESH...IF FEASIBLE, PRESUME STAT UNCHANGED
         IF( NEWST.EQ.'FEASIBLE' .OR. NEWST.EQ.' ' )NEWST=SOLST
         GOTO 9000
      ENDIF
      IF( SWMSG )PRINT *,' REFRESHING DUAL PRICES'
      IF( FREQ0.GT.0 )THEN
C WRITE HEADER TO OUTPUT
         CLIST = 'Dual Prices'
         CALL GPRDIF(-1,FREQ0,CLIST,VP,VNEW,VDIF,*1300)
      ENDIF
      IF( NEWST.EQ.' ' )THEN
C PRIMAL NOT REFRESHED...USE CURRENT STATUS FOR FEASIBILITY
         IF( SOLST.EQ.'INFEASIBLE' )THEN
            NEWST = SOLST
         ELSE
            NEWST = 'OPTIMAL'
         ENDIF
      ELSE IF( NEWST.NE.'INFEASIBLE' )THEN
         NEWST = 'OPTIMAL'
      ENDIF
C NOW NEWST = INFEASIBLE | OPTIMAL
C                          :...PRESUMED UNTIL VIOLATION FOUND
C ========== REFRESH DUAL VALUES ==========
C FORM PRICE VECTOR
      DO 1100 I=1,NROWS
         ZXY(I) = 0.
         CALL GETBVP(I,ROWCOL,J)
         IF( NEWST.EQ.'INFEASIBLE' )THEN
C PHASE 1
            CALL GETSOL(ROWCOL,J,VX,VP,STAT,STNUM)
            IF( STAT.EQ.'I' )THEN
               CALL GETBND(ROWCOL,J,VL,VU)
               IF( VX.LT.VL )THEN
                  ZXY(I) = 1.
               ELSE IF( VX.GT.VU )THEN
                  ZXY(I) = -1.
               ENDIF
            ENDIF
         ELSE
C PHASE 2
            IF( ROWCOL.EQ.'COL ')THEN
               CALL GETRIM(ROWCOL,J,VL,VU,VC,NZ)
               ZXY(I) = -VC
            ENDIF
         ENDIF
1100  CONTINUE
C
C NOW ZXY = -BASIC COST COEFFICIENTS (EITHER PHASE), SO
C           :...GBTRAN RETURNS -BTRAN(ZXY)
      CALL GBTRAN(ZXY,NROWS)
      IF( NEWST.NE.'INFEASIBLE' )ZXY(OBJNUM) = -1.
C NOW ZXY(i) = i-TH ROW PRICE (EITHER PHASE), AND D = -ZXY*A
C
C ::: LOOP OVER ROWS TO COMPARE COMPUTED PRICES WITH RESIDENT ONES :::
      DO 1500 I=1,NROWS
         CALL FCLOCK(TIME)
         ELAPSE = TIME - TIME0
         IF( ELAPSE.GT.MAXTIM )THEN
            PRINT *,I,' ROWS CHECKED',RNUMD,' HAVE DISCREPANCY'
            CALL FLTIME(ELAPSE,MAXTIM,*9000)
            TIME0 = TIME
         ENDIF
         CALL GETSOL('ROW ',I,VX,VP,STAT,STNUM)
         IF( STAT.EQ.'B' .OR. STAT.EQ.'I' )GOTO 1500
C ROW I IS NONBASIC (TIGHT)...SOLUTION VALUE = PRICE
         CALL GBRDIF(VAPDIF,VRPDIF,VP,ZXY(I),VZ,VDIF,*1410)
         GOTO 1490
1410     CONTINUE
C DISCREPANCY
         CLIST = NAME(IRNAME+I)//' P'
         CALL GPRDIF(FREQ,FREQ0,CLIST,VP,VZ,VDIF,*1300)
         IF( SWREPL )THEN
C ...REPLACE VALUE
            CALL GPTSOL('ROW ',I,STAT,VZ)
         ELSE
C ...PUT INTO SUBMATRIX
            CALL GPUTMP('ROW ',I,SWDUAL)
         ENDIF
C UPDATE STATS
         RNUMD = RNUMD + 1
         RAVGD = RAVGD + ABS(VDIF)
         IF( ABS(VDIF).GT.ABS(RMAXD) )THEN
            RMAXD  = VDIF
            RNAMED = NAME(IRNAME+I)
         ENDIF
         VP = VZ
1490     CONTINUE
C WE GET HERE WHETHER OR NOT NEW VALUE IS DISCREPANCY
         IF( NEWST.EQ.'INFEASIBLE' )GOTO 1500
         IF( ( STAT.EQ.'L'.AND.-OPT*VP.LT.-VAPINF ).OR.
     1       ( STAT.EQ.'U'.AND.-OPT*VP.GT. VAPINF )
     2     )THEN
C NOT DUAL FEASIBLE
            IF( NEWST.EQ.'OPTIMAL'      .OR.
     1          ABS(VDIF).GT.ABS(VRCDIF) )THEN
               RCNAME = NAME(IRNAME+I)
               RC     = 'ROW '
               RCKEY  = 'P'
               VRC    = VP
               VRCDIF = VDIF
            ENDIF
            NEWST = 'FEASIBLE'
         ENDIF
1500  CONTINUE
C
C NOW REFRESH NONBASIC COLUMN REDUCED COSTS (AND TEST OPTIMALITY)
C ...ALSO TEST BASIC REDUCED COSTS (THEORETICALLY 0)
      DO 1700 J=1,NCOLS
         CALL FCLOCK(TIME)
         ELAPSE = TIME - TIME0
         IF( ELAPSE.GT.MAXTIM )THEN
            PRINT *,J,' COLUMNS CHECKED',CNUMD,' HAVE DISCREPANCY'
            CALL FLTIME(ELAPSE,MAXTIM,*9000)
            TIME0 = TIME
         ENDIF
         CALL GETSOL('COL ',J,VX,VP,STAT,STNUM)
C COMPUTE REDUCED COST (ZJ) EVEN IF BASIC
         ZPOS = 0.
         ZNEG = 0.
         IF( STAT.EQ.'I' )THEN
C SET = VP (= 1 OR -1)...REST OF ACCUMULATION SHOULD BE 0
            IF( VP.GT.0. )THEN
               ZPOS = VP
            ELSE
               ZNEG = VP
            ENDIF
         ENDIF
         I1 = INONZ + 2*BASE(ICINFO + J-1)
         I2 = INONZ + 2*BASE(ICINFO + J) - 1
         DO 1600 I=I1,I2,2
            CALL GETVAL(V,I+1)
            IF( ABS(V).LT.VTOL0C )GOTO 1600
            ROW = INDEX(I)
            ZJ = - ZXY(ROW)*V
            IF( ZJ.GT.0. )THEN
               ZPOS  = ZPOS + ZJ
            ELSE
               ZNEG  = ZNEG + ZJ
            ENDIF
1600     CONTINUE
         ZJ = ZPOS + ZNEG
C NOW ZJ = REDUCED COST OF COL J (EITHER PHASE)
         CALL GBRDIF(VAPDIF,VRPDIF,VP,ZJ,VZ,VDIF,*1610)
         GOTO 1690
1610     CONTINUE
C DISCREPANCY
         IF( STAT.EQ.'B' .OR. STAT.EQ.'I' )THEN
            CLIST = NAME(ICNAME+J)//' '//STAT
         ELSE
            CLIST = NAME(ICNAME+J)//' D'
         ENDIF
         CALL GPRDIF(FREQ,FREQ0,CLIST,VP,VZ,VDIF,*1300)
         IF( STAT.EQ.'L'.OR.STAT.EQ.'U' )THEN
C NONBASIC
            IF( SWREPL )THEN
C ...REPLACE VALUE
               CALL GPTSOL('COL ',J,STAT,VZ)
            ELSE
C ...PUT INTO SUBMATRIX (STAT=BI TOO)
               CALL GPUTMP('COL ',J,SWDUAL)
            ENDIF
C ...UPDATE STATS
            CNUMD = CNUMD + 1
            CAVGD = CAVGD + ABS(VDIF)
            IF( ABS(VDIF).GT.ABS(CMAXD) )THEN
               CMAXD  = VDIF
               CNAMED = NAME(ICNAME+J)
            ENDIF
         ELSE
C BASIC...CANNOT REPLACE...JUST TALLY
            CNUMB = CNUMB + 1
            CAVGB = CAVGB + ABS(VDIF)
            IF( ABS(VDIF).GT.ABS(CMAXB) )THEN
               CMAXB  = VDIF
               CNAMEB = NAME(ICNAME+J)
            ENDIF
         ENDIF
         VP = VZ
1690     CONTINUE
C WE GET HERE WHETHER OR NOT NEW VALUE IS DISCREPANCY
         IF( NEWST.EQ.'INFEASIBLE' )GOTO 1700
         IF( ( STAT.EQ.'L'.AND.-OPT*VP.LT.-VAPINF )  .OR.
     1       ( STAT.EQ.'U'.AND.-OPT*VP.GT. VAPINF ) )THEN
C NOT DUAL FEASIBLE
            IF( NEWST.EQ.'OPTIMAL'      .OR.
     1          ABS(VDIF).GT.ABS(VRCDIF) )THEN
               RCNAME = NAME(ICNAME+J)
               RC     = 'COL '
               RCKEY  = 'D'
               VRC    = VP
               VRCDIF = VDIF
            ENDIF
            NEWST = 'FEASIBLE'
         ENDIF
1700  CONTINUE
C =========== END OF DUAL REFRESH ============
      IF( FREQ0.GT.0 )CALL GPRDIF(-1,FREQ0,' ',VX,VZ,VDIF,*1300)
      IF( NEWST.EQ.'FEASIBLE' )THEN
C TEST FOR UNBOUNDEDNESS
         CALL GSETST(VAPINF,VAXDIF,ZXY,MAXLST,NEWST,RC,NUMBER,VRC)
         IF( NEWST.EQ.'UNBOUNDED' )THEN
            IF( RC.EQ.'ROW ' )THEN
               RCNAME = NAME(IRNAME+NUMBER)
               RCKEY  = 'P'
            ELSE
               RCNAME = NAME(ICNAME+NUMBER)
               RCKEY  = 'D'
            ENDIF
            VRCDIF = 0.
         ENDIF
      ENDIF
C
C ALL RETURNS AFTER STARTING REFRESH MUST COME HERE TO RESTORE IVFREE
9000  CONTINUE
      IVFREE = IV0
C COMPUTE AVERAGES (FROM SUMS)
      IF( RNUMP.GT.1 )RAVGP = RAVGP/RNUMP
      IF( RNUMD.GT.1 )RAVGD = RAVGD/RNUMD
      IF( CNUMP.GT.1 )CAVGP = CAVGP/CNUMP
      IF( CNUMD.GT.1 )CAVGD = CAVGD/CNUMD
      IF( CNUMB.GT.1 )CAVGB = CAVGB/CNUMB
C
      IF( .NOT.SWREPL )RETURN
      SOLST = NEWST
CDEBUG
        IF( SWDBG )PRINT *,' GBREFR SET SOLST=',SOLST
CDEBUG
C UPDATE EDITS PERFORMED ONLY IF NOT ALREADY COUNTED
      IF( EDREFR.EQ.0 )EDREFR = RNUMP + RNUMD + CNUMP + CNUMD
C (CALLER MUST STORE NEW SOLUTION STATUS, AND CALL GETSTP)
      RETURN
C
C ERROR FROM GPRDIF
1300  CONTINUE
C RESTORE IVFREE
      IVFREE = IV0
      RETURN 1
C
C ** GBREFR ENDS HERE
      END
      SUBROUTINE GBRDIF(TOLABS,TOLREL,VOLD,ZNEW,VNEW,VDIF,*)
C     =================
C
C This computes VNEW and VDIF from ZNEW and VOLD
C (VOLD and ZNEW unchanged).
C ...Alternate return is if VDIF is significant:
C       |VDIF| > TOLABS + TOLREL * |VOLD|
C
      DOUBLE PRECISION ZOLD,ZNEW,ZDIF
      REAL             TOLABS,TOLREL,VOLD,VNEW,VDIF
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::::
      VNEW = ZNEW
      ZOLD = VOLD
      ZDIF = ZNEW - ZOLD
      VDIF = ZDIF
      IF( ABS(VDIF) .GT. TOLABS + TOLREL*ABS(VOLD) )RETURN 1
      RETURN
C
C ** GBRDIF ENDS HERE
      END
      SUBROUTINE GPRDIF(FREQ,FREQ0,STRING,VOLD,VNEW,VDIF,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C ...NEED DCFLIP FOR OUTPUT FILE
C
C This prints a discrepancy to FILE...Alternate return is IO error.
C
C FREQ < 1 means write header (STRING) or trailer (if STRING is null)
C FREQ = 0 means no writing at all
C  THE ABOVE 2 CASES DO NOT CHANGE FREQ, SO IT CAN BE PASSED AS CONSTANT
C FREQ = 1 means write discrepancy, and set FREQ=FREQ0
C FREQ > 1 means do not write, but decrement FREQ
C  ALL OTHER ARGUMENTS ARE INPUT AND NOT CHANGED
C
      CHARACTER*(*) STRING
C LOCAL
      CHARACTER*64  STR64
C ::::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::::
      IF( FREQ.EQ.0 )RETURN
      IF( FREQ.LT.0 )THEN
         IF( STRING.EQ.' ' )THEN
C WRITE TRAILER
            WRITE(OUTPUT,9,ERR=1300)
9           FORMAT(1X,77('='))
         ELSE
C WRITE HEADER
            STR64 = STRING
            CALL FSLEN(STR64,64,LS)
            IF( LS.LT.56 )THEN
               STR64 = ' '
               TAB = 28-LS/2
               DO 10 I=1,TAB-1
10             STR64(I:I) = '='
               STR64(TAB+1:) = STRING
               DO 20 I=TAB+LS+2,58
20             STR64(I:I) = '='
            ENDIF
            WRITE(OUTPUT,1,ERR=1300)STR64
1           FORMAT(/20X,A58
     1             /22X,'Resident',T41,'Computed',T60,'Discrepancy')
            WRITE(OUTPUT,9,ERR=1300)
         ENDIF
      ELSE IF( FREQ.EQ.1 )THEN
C WRITE DISCREPANCY
         WRITE(OUTPUT,6,ERR=1300)STRING,VOLD,VNEW,VDIF
6        FORMAT(1X,A18,1X, G18.10,1X, G18.10,1X, G19.11)
         FREQ = FREQ0
      ELSE IF( FREQ.GT.1 )THEN
         FREQ = FREQ-1
      ENDIF
      RETURN
C
1300  PRINT *,' ** IO ERROR WRITING TO OUTPUT FILE',OUTPUT
      RETURN 1
C
C ** GPRDIF ENDS HERE
      END
