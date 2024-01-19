C              ::: GREDUCE.FOR  8-17-95 :::
C
C Earlier dates deleted
C    5-29-95...Added null and singleton test up front
C    5-30-95...Cosmetic changes in REDCOL
C    7-21-95...Added MAXTIM to args of REDUCE
C              Calls FCLOCK for time interrupt
C    7-30-95...Moved REDCOL and REDROW to GREDTEST.FOR
C              Added tolerances to calling args of REDUCE and
C     8-15-95...Moved REDEBG here
C
C This contains REDUCE routine, using GETMAT data structure.
C
C    REDSET...Sets params and reports (MUST BE CALLED BEFORE REDUCE).
C    REDUCE...Successive bound reduction of primal and dual bounds.
C    REDEBG...Reduce debug
C
C See also ALGRTHMS.DOC for details of reduction tests.
C
      SUBROUTINE REDSET(CLIST,
C FOR COMMON
     1    UFILE,UITER,USWMSG,UFROUT,UTIME,
     2    UAXDIF,URXDIF,UAPDIF,URPDIF,UAXINF,URXINF,UAPINF,
     3    UAXFIX,URXFIX,UTOL0C,UTOL0A,UTOL0P,
C NOT FOR COMMON
     4    NAME01,NAMINT,LGLTST,PHASE,SWMAT,RPTBND,RPTSUM, *,*)
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
C This reports user's settings.
C ...First alternate return is fatal error (eg, IO); 2nd is user abort.
C
      CHARACTER*128    CLIST
C
C Args copied to reduce common (REDCOM) in DCREDUCE:
C Arg    REDCOM
C ====== ======
C UFILE  OUTPUT unit number that will contain all output...presumed open
C               (messages also go to screen)
C UITER  MAXITR max number of iterations (passes) in Phase 1
C USWMSG SWMSG  Message switch
C UFROUT FREQ0  Output frequency
C UTIME  MAXTIM Elapsed number of seconds to ask user for continuation
C UAXDIF VAXDIF Absolute tolerance to declare levels different
C URXDIF VRXDIF Relative tolerance to declare levels different
C UAPDIF VAPDIF Absolute tolerance to declare prices different
C URPDIF VRPDIF Relative tolerance to declare prices different
C UAXINF VTAINF Absolute tolerance to declare level infeasible
C URXINF VTRINF Relative tolerance to declare level infeasible
C UAPINF VTPINF Absolute tolerance to declare price infeasible
C UAXFIX VTAFIX Absolute tolerance to fix a variable (declaring XU=XL)
C URXFIX VTRFIX Relative tolerance to fix a variable (declaring XU=XL)
C UTOL0C VTOL0C Tolerance for coeff to be nonzero
C UTOL0A VTOL0A Tolerance for alpha to be nonzero
C UTOL0P VTOL0P Tolerance for pivot to be acceptable
C
C Args used locally
C NAME01  Name mask for 0-1 columns (blank means none)
C NAMINT  Name mask for integer columns (blank means none)
C LGLTST  = True means perform logical implications test
C PHASE   = 1 for standard successive bounding;
C         = 2 some expensive tests;  > 2 all tests
C SWMAT   = True means overwrite the submatrix with the reduced bounds
C RPTBND  = True means write final bounds
C RPTSUM  = True means write summary
C ====================================================================
      INTEGER     UFILE,UITER,UFROUT,UTIME
      LOGICAL*1   USWMSG
      REAL        UAXDIF,URXDIF,UAPDIF,URPDIF,UAXINF,URXINF,UAPINF,
     1            UAXFIX,URXFIX,UTOL0C,UTOL0A,UTOL0P
      CHARACTER*(*) NAME01,NAMINT
      INTEGER       PHASE
      LOGICAL*1     LGLTST,SWMAT,RPTBND,RPTSUM
C LOCAL
      CHARACTER*64  UNDERL
      CHARACTER*8   STR8
      CHARACTER*1   CHAR
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      TTYOUT = 6
CI$      TTYOUT = 1
C CHECK INDEX SPACE
C ...INDEX(IFREE + I) is used as scratch to signify that row I is forcing
      IF( IFREE + NROWS.GT.MAXIND )THEN
         PRINT *,' Sorry, not enough index space for REDUCE'
         RETURN 1
      ENDIF
C COPY USER SPECS TO REDUCE COMMON
      OUTPUT = UFILE
      MAXITR = UITER
      SWMSG  = USWMSG
      FREQ0  = UFROUT
      FREQ   = FREQ0
      MAXTIM = UTIME
      VAXDIF = UAXDIF
      VRXDIF = URXDIF
      VAPDIF = UAPDIF
      VRPDIF = URPDIF
      VTAINF = UAXINF
      VTRINF = URXINF
      VTPINF = UAPINF
      VTAFIX = UAXFIX
      VTRFIX = URXFIX
      VTOL0C = UTOL0C
      VTOL0A = UTOL0A
      VTOL0P = UTOL0P
C SET INFINITIES FOR ACCUMULATIONS (ALSO IN COMMON)
      VINFX = VINF/10000.
      VTSUM = 1./VTOLAB
      IF( VTSUM.GT.VINFX )VTSUM = VINFX
      IF( VTSUM.GT.1.0E9 )VTSUM = 1.0E9
C PREPARE TO REPORT SETTINGS
      DO 10 I=1,64
10    UNDERL(I:I) = '='
C
      CLIST = UNDERL(:23)//' REDUCE Settings '//UNDERL(:23)
      WRITE(OUTPUT,1,ERR=13000)CLIST
1     FORMAT(1X,A70)
C
      IF( MAXITR.EQ.0 )THEN
         CLIST = 'MAXITER = 0...only preliminary tests performed.'
      ELSE
         STR8 = ' '
         CALL FI2C(STR8,MAXITR,L)
         CLIST = 'MAXITER = '//STR8(L:8)//', PHASE ='
         CALL FSLEN(CLIST,60,L)
         L = L+2
         IF( PHASE.EQ.1 )THEN
            CLIST(L:) = '1...only basic tests performed'
         ELSE IF( PHASE.EQ.2 )THEN
            CLIST(L:) = '2...some expensive tests performed'
         ELSE
            CLIST(L:) = '3...all tests performed'
         ENDIF
         WRITE(OUTPUT,1,ERR=13000)CLIST
         IF( NAME01.EQ.' ' )THEN
            CLIST = 'No binary variable mask'
         ELSE
            CLIST = 'Binary variable mask: '//NAME01
         ENDIF
         IF( NAMINT.EQ.' ' )THEN
            CLIST(40:) = 'No Integer variable mask'
         ELSE
            CLIST(40:) = 'Integer variable mask: '//NAMINT
         ENDIF
      ENDIF
      WRITE(OUTPUT,1,ERR=13000)CLIST
      CLIST = ' '
      IF( LGLTST )THEN
         CLIST = 'Perform logical implications test'
      ELSE
         CLIST = 'Will not perform logical implications test'
      ENDIF
      WRITE(OUTPUT,1,ERR=13000)CLIST
      CLIST = ' '
C
      STR8 = ' '
      CALL FI2C(STR8,MAXTIM,L)
      CLIST = 'TIME interrupt = '//STR8(L:)
      CALL FI2C(STR8,FREQ0,L)
      CLIST(40:) = 'FREQUENCY = '//STR8(L:)
      WRITE(OUTPUT,1,ERR=13000)CLIST
      CLIST = ' '
C
      CLIST(23:) = UNDERL(:5)//' Tolerances '//UNDERL(:5)
      WRITE(OUTPUT,1,ERR=13000)CLIST
      CLIST(23:) = ' Absolute'
      CLIST(34:) = ' Relative'
      WRITE(OUTPUT,1,ERR=13000)CLIST
      CLIST(23:) = UNDERL(:22)
      WRITE(OUTPUT,1,ERR=13000)CLIST
      WRITE(OUTPUT,101,ERR=13000)VAXDIF,VRXDIF,VAPDIF,VRPDIF,
     1                   VTAINF,VTRINF,VTPINF, VTAFIX,VTRFIX,
     2                   VTOL0C,VTOL0A,VTOL0P
101   FORMAT(
     1    ' Primal comparison',   T24,G10.4 ,T36,G10.4  /
     2    ' Dual comparison',     T24,G10.4 ,T36,G10.4  /
     3    ' Primal infeasibility',T24,G10.4 ,T36,G10.4  /
     4    ' Dual infeasibility',  T24,G10.4             /
     5    ' Fix (primal) value',  T24,G10.4 ,T36,G10.4  /
     6    ' Coefficient nonzero', T24,G10.4             /
     7    ' Alpha nonzero',       T24,G10.4             /
     8    ' Pivot acceptable',    T24,G10.4             /
     9      )
C
      CLIST = UNDERL(:28)//' Output '//UNDERL(:28)
      WRITE(OUTPUT,1,ERR=13000)CLIST
      STR8 = ' '
      CALL FI2C(STR8,FREQ0,L)
      CLIST = 'Reduction frequency = '//STR8(L:)
      IF( RPTBND )THEN
         CLIST = 'Bound report: Yes'
      ELSE
         CLIST = 'Bound report: No'
      ENDIF
      IF( RPTSUM )THEN
         CLIST(24:) = 'Summary: Yes'
      ELSE
         CLIST(24:) = 'Summary: No'
      ENDIF
      IF( SWMAT )THEN
         CLIST(40:) = 'Will overwrite bounds'
      ELSE
         CLIST(40:) = 'Will not overwrite bounds'
      ENDIF
      WRITE(OUTPUT,1,ERR=13000)CLIST
      CLIST = UNDERL
      WRITE(OUTPUT,1,ERR=13000)CLIST
C
      IF( OUTPUT.NE.TTYOUT )RETURN
      IF( FREQ0.GT.0 .AND. FREQ0.LT.999 )
     1   PRINT *,' Warning: there is no scrolling...reductions',
     2           ' will flash by screen.'
      PRINT *,' Continue? '
      CALL FGTCHR('YN','Y',CHAR)
      IF( CHAR.EQ.'Y' )RETURN
      RETURN 2
C
13000 PRINT *,' ** IO ERROR WRITING OUTPUT FILE'
      RETURN 1
C
C ** REDSET ENDS HERE
      END
      SUBROUTINE REDUCE(ZALPHA,NALPHA,CLIST,
     1    NAME01,NAMINT,PHASE,SWMAT,RPTBND,RPTSUM,RCODE)
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
C This is the main routine to apply successive reduction to submatrix.
C
C ZALPHA  general purpose double precision array
C NALPHA  length of ZALPHA (>= NROWS, or else not used)
C CLIST   general purpose character list that routines can use for
C         output or messages...presumed to be 128
C
      DOUBLE PRECISION ZALPHA(NALPHA)
      CHARACTER*128    CLIST
      CHARACTER*(*)    NAME01,NAMINT
      INTEGER          PHASE
      LOGICAL*1        SWMAT,RPTBND,RPTSUM
C LOCAL
      CHARACTER*32  NOTE
      CHARACTER*16  RNAME,CNAME
      CHARACTER*12  STRVAL
      CHARACTER*1   CHAR
      LOGICAL*1     RPTANY,RPTRED
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      RPTRED = FREQ0.GT.0
      CALL FCLOCK(TIME0)
      NOTE = 'REDUCE'
C SET POINTERS INTO VALUE ARRAY
      IXL    = IVFREE
      IXU    = IXL + NCOLS
      IYL    = IXU + NCOLS
      IYU    = IYL + NROWS
      IPL    = IYU + NROWS
      IPU    = IPL + NROWS
      IYMIN  = IPU + NROWS
      IYMAX  = IYMIN + NROWS
      IVFREE = IYMAX + NROWS
      IV0    = IVFREE
C     =============== RETURNS FROM THIS POINT MUST GO TO 9999
      IF( SWRDBG )CALL GDEBUG
C
      IF( IVFREE.GT.MAXVAL )THEN
         I = IVFREE - MAXVAL
         PRINT *,' Sorry, not enough value space (need',I,' more)'
         RCODE = 1
         GOTO 9999
      ENDIF
C
C INITIALIZE COLUMN BOUNDS
      DO 200 J=1,NCOLS
         IF(INDEX(ICSTAT+J).GT.0)GOTO 200
         CALL GETVAL(VALUE(IXL + J),ICLBND + J)
         CALL GETVAL(VALUE(IXU + J),ICUBND + J)
200   CONTINUE
C
C INITIALIZE ROW BOUNDS AND PRICE RANGE FOR ROWS IN SUBMATRIX
      DO 800 I=1,NROWS
C FIRST, INITIALIZE RANGE ACCUMULATORS
         VALUE(IYMIN + I) = 0.0
         VALUE(IYMAX + I) = 0.0
         INDEX(IFREE + I) = 0
         CALL GETYPE(I,CHAR)
C IF ROW I IS NOT IN SUBMATRIX, TREAT AS FREE ROW
         IF( INDEX(IRSTAT+I).GT.0 .OR. CHAR.EQ.'N' )GOTO 750
C ROW I IS IN SUBMATRIX AND NOT FREE
         RNAME = NAME ( IRNAME+I)
         NZ    = INDEX( IRINFO+I)
         CALL GETVAL(VL,IRLBND+I)
         CALL GETVAL(VU,IRUBND+I)
         NOTE(8:) = ' '
         IF( NZ.EQ.0 )GOTO 700
C                     ======== ROW IS NULL IN ENTIRE LP
         IF( NZ.GT.1 )GOTO 790
C ROW HAS 1 NONZERO...FIND IT
         DO 590 J=1,NCOLS
            I1=INONZ + 2*BASE(ICINFO+J-1)
            I2=INONZ + 2*BASE(ICINFO+J) - 1
            DO 525 IJ=I1,I2,2
               ROW = INDEX(IJ)
               IF( ROW.EQ.I )GOTO 600
525         CONTINUE
590      CONTINUE
         PRINT *,' ** SYSERR IN REDUCE...',NAME(IRNAME+I),NZ
         GOTO 13013
C
600      CONTINUE
C COLUMN J HAS THE NONZERO IN ROW I
         CALL GETVAL(V,IJ+1)
         IF( ABS(V).LE.VTOLAB )GOTO 700
C ...COEFFICIENT = V <> 0
         IF( INDEX(ICSTAT+J).GT.0 )THEN
C ...COLUMN NOT IN SUBMATRIX...TREAT ROW I AS NULL (JUST IN SUBMATRIX)
            NOTE(8:) = '(in submatrix)'
            GOTO 700
         ENDIF
C ...COLUMN IS IN SUBMATRIX, SO USE ROW TO MODIFY COLUMN'S BOUNDS
         IF( V.EQ.-1. )THEN
            STRVAL = '-'
         ELSE IF( V.EQ.1. )THEN
            STRVAL = ' '
         ELSE
            CALL FR2CLJ(STRVAL,V,L)
         ENDIF
         CLIST = 'Freed singleton row '//RNAME(:NAMELN)//' = '//STRVAL
         CALL FSLEN(CLIST,128,LC)
         CLIST(LC+2:) = NAME(ICNAME+J)
         LC = LC+3+NAMELN
         IF( CHAR.EQ.'E' )THEN
            CALL FR2CLJ(STRVAL,VL,L)
            CLIST(LC:) = ' = '//STRVAL
            LC = LC+13
         ELSE
            IF( CHAR.EQ.'L' .OR. CHAR.EQ.'R' )THEN
               CALL FR2CLJ(STRVAL,VL,L)
               CLIST(LC:) = '>= '//STRVAL
               LC = LC+13
            ENDIF
            IF( CHAR.EQ.'U' .OR. CHAR.EQ.'R' )THEN
               CALL FR2CLJ(STRVAL,VU,L)
               IF( CHAR.EQ.'R' )THEN
                  CLIST(LC:) = 'and'
                  LC = LC+4
               ENDIF
               CLIST(LC:) = '>= '//STRVAL
            ENDIF
         ENDIF
         IF( RPTRED )CALL REDTXT(CLIST,*13000)
         IF( SWRDBG )THEN
            CALL FSLEN(CLIST,128,L)
            CALL REDTRK(CLIST,L,NOTE,*13000)
         ENDIF
C
         CALL GETBND('COL ',J,VLJ,VUJ)
         CNAME = NAME(ICNAME+J)
         IF( V.LT.0. )THEN
            V = -V
            VT= -VL
            VL= -VU
            VU = VT
         ENDIF
C NOW WE HAVE VL <= V*X(J) <= VU WITH V > 0.
         IF( VL.GT.-VINF .AND. VL/V.GT.VLJ+VTOLAB )THEN
            VX = VL/V
            VALUE(IXL + J) = VX
            CALL FR2CLJ(STRVAL,VX,L)
            CLIST = '...Activity '//CNAME(:NAMELN)//
     1              ' Lower bound increased to '//STRVAL
            IF( RPTRED )CALL REDTXT(CLIST,*13000)
            IF( SWRDBG )THEN
               CALL FSLEN(CLIST,128,L)
               CALL REDTRK(CLIST,L,NOTE,*13000)
            ENDIF
         ENDIF
         IF( VU.LT.VINF .AND. VU/V.LT.VUJ-VTOLAB )THEN
            VX = VU/V
            VALUE(IXU + J) = VX
            CALL FR2CLJ(STRVAL,VX,L)
            CLIST = '...Activity '//CNAME(:NAMELN)//
     1              ' Upper bound decreased to '//STRVAL
            IF( RPTRED )CALL REDTXT(CLIST,*13000)
            IF( SWRDBG )THEN
               CALL FSLEN(CLIST,128,L)
               CALL REDTRK(CLIST,L,NOTE,*13000)
            ENDIF
         ENDIF
         VL = VALUE(IXL+J)
         VU = VALUE(IXU+J)
         VABS = ABS(VL) + ABS(VU)
         IF( VL-VU.GE.VTAINF + VTRINF*VABS )THEN
            CLIST = '...THIS IS INFEASIBLE'
            CALL REDTXT(CLIST,*13000)
            GOTO 9100
         ENDIF
         IF( VL-VU.GE. -(VTAFIX + VTRFIX*VABS) )THEN
            VX = VL
            CALL REDFIX(VX,J)
            IF( RPTRED )THEN
               CALL FR2CLJ(STRVAL,VX,L)
               CLIST = '...This forces column fixed at '//STRVAL
               CALL REDTXT(CLIST,*13000)
               IF( SWRDBG )THEN
                  CALL FSLEN(CLIST,128,L)
                  CALL REDTRK(CLIST,L,NOTE,*13000)
               ENDIF
            ENDIF
         ENDIF
         GOTO 750
C ========================================================
700      CONTINUE
C ===== ROW IS NULL [NOTE SAYS IF JUST IN SUBMATRIX] =====
         CLIST = 'Row '//RNAME(:NAMELN)//' is null '//NOTE(8:)
         CALL FSLEN(CLIST,128,L)
         CLIST(L+1:) = '.'
         IF( RPTRED )CALL REDTXT(CLIST,*13000)
         IF( SWRDBG )CALL REDTRK(CLIST,L,NOTE,*13000)
         IF( VL.GT.VTOLAB .OR. VU.LT.-VTOLAB )THEN
            CLIST = '...INFEASIBLE (See Bounds)'
            CALL REDTXT(CLIST,*13000)
            GOTO 9100
         ENDIF
750      CONTINUE
C ===== FREE ROW =====
         VALUE(IYL + I) = -VINF
         VALUE(IYU + I) =  VINF
         VALUE(IPL + I) =  0.
         VALUE(IPU + I) =  0.
         GOTO 800
C        ======== (NEXT ROW)
790      CONTINUE
C ===== ROW HAS MORE THAN 1 NONZERO ... SET BOUNDS AND PRICES ====
         VALUE(IYL + I) = VL
         VALUE(IYU + I) = VU
         IF(VL.LE.-VINF)THEN
            VALUE(IPU + I) = 0.0
         ELSE
            VALUE(IPU + I) = VINF
         ENDIF
         IF(VU.GE.VINF)THEN
            VALUE(IPL + I) = 0.0
         ELSE
            VALUE(IPL + I) = -VINF
         ENDIF
800   CONTINUE
C
      IF( MAXITR.EQ.0 )GOTO 9000
C ======== PREPARE FOR PASSES ========
C OVERIDE OBJ PRICE IF IN SUBMATRIX
      IF( INDEX(IRSTAT + OBJNUM).LT.0 )THEN
         VOPT = -OPT*OPTMIN
C       (VOPT = -1 if OPT=OPTMIN and +1 if OPT=OPTMAX...This treats
C        reduced computation and price ranges for minimization.)
         VALUE(IPL + OBJNUM) = VOPT
         VALUE(IPU + OBJNUM) = VOPT
      ENDIF
C INITIALIZE STATS
      PASS   = 0
      MXPASS = MAXITR
      NRFORC = 0
      NCFORC = 0
      RPTANY = (FREQ0.GT.0) .OR. RPTSUM .OR. RPTBND
C WRITE HEADER (IF ANYTHING IS TO BE REPORTED)
      IF( RPTANY )WRITE(OUTPUT,901,ERR=13000)PRBNAM
901   FORMAT(/5X,'REDUCE LOG FOR ',A16)
C     .~~~~~~~~~~~~~~~~~~~.
C     : BEGIN REDUCE PASS :
C     :...................:
1000  CONTINUE
      PASS = PASS + 1
      IF( RPTANY )THEN
         PRINT *,' REDUCE PASS',PASS
         IF( OUTPUT.NE.TTYOUT )WRITE(OUTPUT,1001,ERR=13000)PASS
1001     FORMAT(/' REDUCE PASS',I5/' ================')
      ENDIF
C
      CALL FCLOCK(TIME)
      ELAPSE = TIME-TIME0
      IF( ELAPSE.GE.MAXTIM )CALL FLTIME(ELAPSE,MAXTIM,*9000)
      SWRED = .FALSE.
C   After PASS, SWRED = TRUE means a reduction occurred.
C   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF( PASS.GT.MXPASS )THEN
         WRITE(OUTPUT,1002,ERR=13000)MXPASS
1002     FORMAT(' ...MAX NUMBER OF PASSES REACHED...',I5)
         IF( OUTPUT.NE.TTYOUT )WRITE(*,1002)MXPASS
      ELSE
C  _________________________________________________________
C   LOOP OVER COLUMNS TO ACCUMULATE ROW RANGES AND TEST DUAL
          CALL REDCOL(CLIST,*9100,*13000)
C  _________________________________________________________
C   LOOP OVER ROWS TO TEST PRIMAL RANGES
          CALL REDROW(CLIST,*9100,*13000)
C  _________________________________________________________
C   LOOP OVER COLUMNS AGAIN TO EXTREMIZE LEVELS FOR EACH ROW
C     AND APPLY 0-1 & INTEGER TESTS
          CALL REDBIN(CLIST,NAME01,NAMINT,*9100,*13000)
          IF( (PHASE.GE.2) .AND. (PASS.GE.MXPASS.OR..NOT.SWRED) )THEN
C  _________________________________________________________
C  TRY EXPENSIVE TESTS
             IF( RPTANY .OR. SWMSG )
     1          PRINT *,' Performing expensive tests...please wait'
             CALL REDEXP(CLIST,ZALPHA,NALPHA,*9100,*13000)
             IF( PHASE.GE.3 )
     1          CALL REDEX2(CLIST,ZALPHA,NALPHA,*9100,*13000)
          ENDIF
C  _________________________________________________________
      ENDIF
C
      IF( SWRED .AND. PASS.LT.MXPASS )THEN
C REDUCTION OCCURRED AND WE ARE NOT AT MAX PASS...PREPARE FOR NEW PASS
         DO 4000 I=1,NROWS
            VALUE(IYMIN + I) = 0.0
            VALUE(IYMAX + I) = 0.0
            IF(VALUE(IYL + I).LE.-VINFX)VALUE(IYL + I) = -VINF
            IF(VALUE(IYU + I).GE. VINFX)VALUE(IYU + I) =  VINF
4000     CONTINUE
         GOTO 1000
      ENDIF
C     .~~~~~~~~~~~~~~~~~~~~~~~~.
C     : END OF BOUND REDUCTION :
C     :........................:
C INFERRED BOUNDS:  VALUE(IYL+i) <= Y(i) <= VALUE(IYU+i)
C                   VALUE(IPL+i) <= P(i) <= VALUE(IPU+i)
C                   VALUE(IXL+j) <= X(j) <= VALUE(IXU+j)
C  MORE TO BE ADDED LATER:
C      PATH/CYCLE REDUCTION
C      COVERING ANALYSIS
C      AGGREGATE BOUNDS
C
9000  CONTINUE
C   NORMAL RETURNS COME HERE
C   ~~~~~~~~~~~~~~~~~~~~~~~~
C CLEAN INFINITE BOUNDS
      CALL REDINF
      CALL REDRPT(RPTBND,RPTSUM,*13000)
      IF( SWMAT )CALL REDMAT(IV0)
C                =========== MUST BE DONE LAST BECAUSE IV0 WILL DEFINE
C                       NEW END OF VALUE POOL, CHANGING IVFREE AT 9999
      GOTO 9999
C
C INFEASIBLE RETURNS COME HERE
9100  CONTINUE
      RCODE = -1
C
9999  CONTINUE
C    ALL RETURNS COME HERE AFTER MEMORY IS SET
C    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C RESTORE IVFREE
      IVFREE = IV0
      RETURN
C
13000 PRINT *,' ** IO ERROR WRITING OUTPUT FILE'
13013 RCODE = 1
      GOTO 9999
C
C ** REDUCE ENDS HERE
      END
      SUBROUTINE REDEBG
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
C Debug for REDUCE (called by GDEBUG).
C
C LOCAL
      CHARACTER*1  CHAR
      CHARACTER*16 STR16
      LOGICAL*1    SW
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      PRINT *,' REDEBG ENTERED...8-02-95'
5     CONTINUE
      PRINT *,' Numbers, Row, Col, Debug '
      READ(*,'(A1)')CHAR
      IF( CHAR.EQ.' ' )RETURN
      IF( CHAR.EQ.'N' )THEN
         PRINT *,' PASS=',PASS,' MXPASS=',MXPASS,' TTYOUT=',TTYOUT
         PRINT *,' VAXDIF=',VAXDIF,' VRXDIF=',VRXDIF,' VAPDIF=',VAPDIF
         PRINT *,' VTAFIX=',VTAFIX,' VTRFIX=',VTRFIX,' VRPDIF=',VRPDIF
         PRINT *,' VTAINF=',VTAINF,' VTRINF=',VTRINF,' VTPINF=',VTPINF
         PRINT *,' VTOL0C=',VTOL0C,' VTOL0A=',VTOL0A,' VTOL0P=',VTOL0P
         PRINT *,' VTSUM =',VTSUM, ' VINFX =',VINFX, '  SWRED=',SWRED
         PRINT *,' NRFORC=',NRFORC,' NCFORC=',NCFORC
         PRINT *,' IXL=',  IXL,  ' IXU=',  IXU,
     1           ' IYL  =',IYL,  ' IYU  =',IYU
         PRINT *,' IPL=',  IPL,  ' IPU=',  IPU,
     1           ' IYMIN=',IYMIN,' IYMAX=',IYMAX
      ELSE IF( CHAR.EQ.'R' )THEN
10       CONTINUE
           PRINT *,' Name: '
           READ(*,'(A16)')STR16
           IF( STR16.EQ.' ' )GOTO 5
           CALL GETNUM('ROW ',STR16,N)
           IF( N.EQ.0 )THEN
              PRINT *,' ** ',STR16,' NOT FOUND'
           ELSE
              PRINT *,' ROW ',STR16,N
              PRINT *,' YMIN,YMAX =',VALUE(IYMIN+N),VALUE(IYMAX+N)
              PRINT *,' YL  ,YU   =',VALUE(IYL  +N),VALUE(IYU  +N)
              PRINT *,' PL  ,PU   =',VALUE(IPL  +N),VALUE(IPU  +N)
              PRINT *,' IFREE+ROW =',INDEX(IFREE+N)
              PRINT *,' SEE NONZEROES (Y/N)? '
              READ(*,'(A1)')CHAR
              IF( CHAR.EQ.'Y'.OR.CHAR.EQ.' ' )THEN
                 NOTSUB = 0
                 I1=INONZ + 2*BASE(ICINFO)
                 WRITE(*,'(2X,6HCOLUMN,T35,4HCOEF,T50,2HXL,T65,2HXU)')
                 LINE = 1
                 DO 100 J=1,NCOLS
                    NZ = BASE(ICINFO + J) - BASE(ICINFO + J-1)
                    IF( NZ.EQ.0 )GOTO 100
                    I2 = I1 + 2*NZ - 1
                    CALL GETMAP('COL ',J,SW)
                    IF( .NOT.SW )THEN
                       NOTSUB = NOTSUB+1
                       GOTO 60
                    ENDIF
                    DO 50 I=I1,I2,2
                       ROW = INDEX(I)
                       IF( ROW.NE.N )GOTO 50
                       CALL GETVAL(VCOEF,I+1)
                       STR16 = NAME(ICNAME+J)
                       IF( LINE.GT.20 )THEN
                          PRINT *,' CONTINUE...'
                          READ(*,'(A1)')CHAR
                          IF( CHAR.EQ.'N' )GOTO 10
                          LINE = 0
                       ENDIF
                       PRINT *,J,' ',STR16(:NAMELN),VCOEF,
     1                         VALUE(IXL+J),VALUE(IXU+J)
                       LINE = LINE+1
                       GOTO 60
50                  CONTINUE
C NEXT COL
60                  CONTINUE
                    I1 = I2+1
100              CONTINUE
                 PRINT *,NOTSUB,' COLS NOT IN SUBMATRIX'
              ENDIF
           ENDIF
           GOTO 10
      ELSE IF( CHAR.EQ.'C' )THEN
20       CONTINUE
           PRINT *,' Name: '
           READ(*,'(A16)')STR16
           IF( STR16.EQ.' ' )GOTO 5
           CALL GETNUM('COL ',STR16,N)
           IF( N.EQ.0 )THEN
              PRINT *,' ** ',STR16,' NOT FOUND'
           ELSE
              PRINT *,' COL ',STR16,N
              PRINT *,' XL,XU =',VALUE(IXL+N),VALUE(IXU+N)
              CALL GETBND('COL ',N,VL,VU)
              PRINT *,' VL,VU =',VL,VU
           ENDIF
           PRINT *,' SEE NONZEROES (Y/N)? '
           READ(*,'(A1)')CHAR
           IF( CHAR.EQ.'Y'.OR.CHAR.EQ.' ' )THEN
              NOTSUB = 0
              I1=INONZ + 2*BASE(ICINFO+N-1)
              I2=INONZ + 2*BASE(ICINFO+N) - 1
              DO 150 I=I1,I2,2
                 ROW = INDEX(I)
                 CALL GETMAP('ROW ',ROW,SW)
                 IF( .NOT.SW )THEN
                    NOTSUB = NOTSUB+1
                    GOTO 150
                 ENDIF
                 CALL GETVAL(VCOEF,I+1)
                 STR16 = NAME(IRNAME+ROW)
                 IF( LINE.GT.20 )THEN
                    PRINT *,' CONTINUE...'
                    READ(*,'(A1)')CHAR
                    IF( CHAR.EQ.'N' )GOTO 20
                    LINE = 0
                 ENDIF
                 PRINT *,ROW,' ',STR16(:NAMELN),VCOEF
                 LINE = LINE+1
150           CONTINUE
           ENDIF
           PRINT *,NOTSUB,' ROWS NOT IN SUBMATRIX'
           GOTO 20
      ELSE IF( CHAR.EQ.'D' )THEN
90       CONTINUE
           PRINT *,' Currently, ROWDBG=',NAME(IRNAME+ROWDBG)
           PRINT *,' Enter / for null'
           READ(*,'(A16)') STR16
           IF( STR16.NE.' ' )THEN
              IF( STR16.EQ.'/' )THEN
                 ROWDBG = 0
              ELSE
                 CALL GETNUM('ROW ',STR16,ROWDBG)
                 IF( ROWDBG.EQ.0 )
     1               PRINT *,' ** ',STR16,' NOT FOUND'
              ENDIF
              GOTO 90
           ENDIF
91       CONTINUE
           PRINT *,' Currently, COLDBG=',NAME(ICNAME+COLDBG)
           PRINT *,' Enter / for null'
           READ(*,'(A16)') STR16
           IF( STR16.NE.' ' )THEN
              IF( STR16.NE.'/' )THEN
                 CALL GETNUM('COL ',STR16,COLDBG)
                 IF( COLDBG.EQ.0 )
     1               PRINT *,' ** ',STR16,' NOT FOUND'
              ELSE
                 COLDBG = 0
              ENDIF
              GOTO 91
           ENDIF
C
         PRINT *,' Currently, SWRDBG=',SWRDBG
         CALL FPAUSE(PAUSE0)
         PAUSE = PAUSE0
         SWRDBG = PAUSE0.GT.0
         PRINT *,' Now SWRDBG=',SWRDBG
         IF( .NOT.SWRDBG )PAUSE = -PAUSE0
      ELSE
         PRINT *,' ?',CHAR
      ENDIF
      GOTO 5
C
C ** REDEBG ENDS HERE
      END
