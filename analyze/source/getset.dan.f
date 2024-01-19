C                 ::: GETSET.FOR  8-20-95 :::
C
C Earlier dates deleted.
C       2-12-95...Added tol/inf check in GNEWLP
C       5-14-95...Added GPUTBS (for LPRENAME)
C       7-04-95...Removed GPUTBS & Moved BASCLR to here
C       7-29-95...Added tolerances in GSETSL (CC 8-2-95)
C       8-02-95...Added GSETST
C       8-12-95...Moved GSETST to GETRANS (calls GALPHA and GFTRAN)
C
C This contains the following GETMAT subroutines.
C
C   GINITL...initializes data (called once by host)
C   GNEWLP...initializes for new LP
C   GETBAS...gives base names/stats
C   GETEDS...gives ED* values in BASE
C   GSETNM...sets names of obj, rhs, range, bound
C   GSETOP...sets sense of optimization
C   GADDNZ...adds nonzero
C   GSETSL...initializes solution values to all-logical basis
C   GPUTSL...puts solution into INDEX and VALUE arrays
C   BASCLR...clears basis (restores space)
C   GMPCLR...clears maps
C   GETMAP...gets map of row/col
C   GPUTMP...puts map of row/col
C   GMAPNZ...removes null rows/columns from submatrix
C
C See GETMAT.DOC for details about data structures.
C
      SUBROUTINE GINITL(FILDAT,FILPCK,VERSN)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This initializes data in COMMON blocks.
C
C             MUST BE CALLED BEFORE ANY OTHER GETMAT ROUTINE!
C             ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C FILE UNITS PASSED
      INTEGER*4 FILDAT,FILPCK
C
C FILDAT is for data files, used as DATFIL
C FILPCK is UNFORMATTED, used as PCKFIL and for scratch
C
C VERSION (returned to caller)
      CHARACTER*(*) VERSN
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      VERSN = '8/93'
C DEBUG PARAMS
      SWDBG  = .FALSE.
      SWRDBG = .FALSE.
      PAUSE0 = 0
      PAUSE = 0
C CONSTANTS
      VTOLAB = 1.0E-10
      VTOLRE = 1.0E-18
      VINF   = 1.0E+20
C DIMENSION LIMITS
      MAXNAM = PMXNAM
      MAXIND = PMXIND
      MAXVAL = PMXVAL
      write(6,*) 'PMXNAM,PMXIND,PMXVAL=',PMXNAM,PMXIND,PMXVAL
      write(6,*) 'MAXNAM,MAXIND,MAXVAL=',MAXNAM,MAXIND,MAXVAL
      EBASE2 = 89
C              :...end of BASE, except for MAX values
      ENDBAS = 99
C              :.....end of BASE
      ENDIND = ENDBAS
C SET DEFAULT NAME LENGTH
      NAMELN = 8
C ...ROW NAME BASE
      IRNAME = 0
C ...ROW INFO BASE
      IRINFO = ENDIND
C INITIALIZE NAMERC TO HAVE BLANKS IN 0-TH NAME
      DO 10 I=-16,0
10    NAMERC(I)=' '
C SET MAX NUMBER OF ROWS AND COLUMNS (MAXRC)
      MAXRC  = MAXNAM/NAMELN
C SET MAX NUMBER OF DISTINCT VALUES (MAXVPL)
C  Value pool limited by less than half VALUE array...see GETDATA.DOC.
      MAXVPL = MAXVAL/2 - MAXRC - 257
      write(6,*) 'MAXRC,MAXVAL/2,MAXVPL',maxrc,maxval/2,maxvpl
C              ================   :
C              changed 8-30-93    :...space for list heads
C              to allow full word links in GETIOMAT (for > 64K cols)
C  Note that while value pool is being constructed, the bottom 2*MAXRC
C  words are reserved (+ 257 for list heads).  The remaining VALUE
C  space becomes MAXVAL - 2*MAXRC, but half of this is for value pool
C  links, so the maximum number of distinct values is (MAXVAL-2*MAXRC)/2
C  - 257, which is equal to the above.  See GETDATA.DOC.
      IF( MAXVPL.LT.100 )THEN
         PRINT *,' ** SYSERR IN GINITL...MAXVPL =',MAXVPL
         PRINT *,' ...PLEASE REPORT'
         STOP
      ENDIF
C
C SENSE OF OPTIMIZATION
      OPTMIN  = -1
      OPTMAX  =  1
C SPECIAL CHARACTERS
      GMASK  = '*'
      CHARST(1) = 'L'
      CHARST(2) = 'U'
      CHARST(3) = 'B'
      CHARST(4) = 'I'
C ...Solution STATUS codes:
C        L = nonbasic at Lower bound
C        U = nonbasic at Upper bound
C        B = Basic and feasible
C        I = Infeasible (and basic)
C SET FILES
      DATFIL = FILDAT
      PCKFIL = FILPCK
C
      CALL GNEWLP
      RETURN
C
C ** GINITL ENDS HERE
      END
      SUBROUTINE GNEWLP
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This initializes for new LP (called before reading LP).
C                                     ~~~~~~
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
C FOR SOME UNKNOWN REASON TOLERANCES AND INFINITY MIGHT NOT HAVE BEEN
C SET (SHOULD HAVE BEEN IN GINITL).  HERE IS A CHECK.
      IF( VTOLAB.LE.0. .OR. VTOLAB.GT.1. .OR.
     1    VTOLRE.LE.0. .OR. VTOLRE.GT.1. .OR.
     2    VINF  .LE.1. )THEN
         IF( SWDBG )THEN
            PRINT *,' GNEWLP DETECTED BAD TOL/INF:'
            CALL GDEBUG
            PRINT *,' ...CHANGING THEM NOW'
         ENDIF
         VTOLAB = 1.0E-10
         VTOLRE = 1.0E-12
         VINF   = 1.0E+20
         IF( SWDBG )CALL GDEBUG
      ENDIF
C CHECK MAX NUMBER OF ROWS + COLUMNS (NAMELN MAY HAVE CHANGED)
      MAX = MAXNAM/NAMELN - 1
      IF(MAXRC.GT.MAX)MAXRC = MAX
C
C VALUE POOL (enter 0, 1 and infinity).
      VALUE(0) = 0.0
      NVALS= 0
      HEAD = MAXVPL + 1
C ...SET HEAD POINTERS = 0 (using secondary hash with 256 lists)
      DO 25 I=HEAD,HEAD+255
25    I4VAL(I) = 0
C ENTER 1.0 INTO VALUE POOL RIGHT AWAY (MAKE IT FIRST IN ITS LIST)
      V = 1.0
      CALL GETVID(V,ID)
      IF(ID.NE.1)THEN
          PRINT *,' ** VALUE INITIALIZATION ERROR...',V,ID
          STOP
      ENDIF
C WE NOW HAVE VALUE(1)=1...PUT INFINITY INTO VALUE POOL
      V = VINF
      CALL GETVID(V,ID)
      IF(ID.NE.2 .OR. NVALS.NE.2)THEN
          PRINT *,' ** VALUE INITIALIZATION ERROR...',V,ID,NVALS
          STOP
      ENDIF
      INF = 2
C  ::::::::::::::: END VALUE POOL INITIALIZATION :::::::::::::
C
      IFREE  = ENDIND + 1
C  NOTE:  IFREE points to free space at end of INDEX, which may
C         change;  however, ENDIND is constant end of base.
      IVFREE = 3
C
C SENSE OF OPTIMIZATION
      OPT     = OPTMIN
C SET STATS
      NROWS  = 0
      NCOLS  = 0
      NONZER = 0
      NONES  = 0
      STCOMP = 0
      STBASC = 0
C
      NRFREE = 0
      NCFIX  = 0
      OBJNUM = 0
      RMAXLN = 0
      CMAXLN = 0
C
C EDIT STATS
      DO 79 I=71,77
79    BASE(I) = 0
C SET NONZERO POINTER TO BOTTOM OF INDEX
C (FOR GETMAT, WHICH INSERTS FROM BOTTOM UP)
      INONZ  = MAXIND
C
C SET NAMES (NO LP IF NO PROBLEM NAME (PRBNAM))
      PRBNAM = ' '
      OBJNAM = ' '
C ...OPTIONAL
      RHSNAM = 'none'
      BNDNAM = 'none'
      RNGNAM = 'none'
      SOLNAM = 'unknown'
C
C NO BASIS FACTORIZATION (DEFER UNTIL COMMAND NEEDS IT)
      IPIVOT = 0
      PIVNUM = 0
      RETURN
C
C ** GNEWLP ENDS HERE
      END
      SUBROUTINE GETBAS(PRBNM,OBJNM,RHSNM,RNGNM,BNDNM,STATNM,SOLNM,
C     =================
     1  NMROWS,NMCOLS,NMNZ,NMVALS,NMONES,NMOBJ,
     2  NMFREE,NMFIXD,NM MAXR,NM MAXC,NAMLEN,VINFTY,VTOLA,VTOLR)
C      ==================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This returns base names/stats.
C
        CHARACTER*(*) PRBNM,OBJNM,RHSNM,RNGNM,BNDNM,STATNM,SOLNM
C
C NAMES
C       PRBNM  = problem name
C       OBJNM  = objective row name
C       RHSNM  = right-hand side name ('none' if none)
C       RNGNM  = range name ('none' if none)
C       BNDNM  = bound name ('none' if none)
C       STATNM = solution status:
C                OPTIMAL | FEASIBLE | INFEASIBLE | UNBOUNDED | unknown
C       SOLNM  = solver name
C NUMBERS
C       NMROWS  = number of rows
C       NMCOLS  = number of columns
C       NMNZ    = number of nonzeroes
C       NMVALS  = number of distinct (positive) values
C       NMONES  = number of 1's (+ or -)
C       NMOBJ   = number of objective row (OBJNUM)
C       NMFREE  = number of free rows
C       NMFIXD  = number of fixed columns
C       NMMAXR  = index number of longest row, except OBJ
C       NMMAXC  = index number of longest column
C       NAMLEN  = max length of row/column name (2 to 16)
C REALS
C       VINFTY  = infinity (VINF)
C       VTOLA   = absolute tolerance (VTOLAB)
C       VTOLR   = relative tolerance (VTOLRE)
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      PRBNM  = PRBNAM
      OBJNM  = OBJNAM
      RHSNM  = RHSNAM
      RNGNM  = RNGNAM
      BNDNM  = BNDNAM
      STATNM = SOLST
      SOLNM  = SOLNAM
C
      NMROWS = NROWS
      NMCOLS = NCOLS
      NMNZ   = NONZER
      NMVALS = NVALS
      NMONES = NONES
      NMOBJ  = OBJNUM
C
      NMFREE = NRFREE
      NMFIXD = NCFIX
      NMMAXR = RMAXLN
      NMMAXC = CMAXLN
      NAMLEN = NAMELN
C
      VINFTY = VINF
      VTOLA  = VTOLAB
      VTOLR  = VTOLRE
C
      RETURN
C
C ** GETBAS ENDS HERE
      END
      SUBROUTINE GETEDS(EFREE,EFIX,EROUND,EREFR,EPIVOT,EREDUC,EREN)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets ED* values from base.
C
      INTEGER   EFREE,EFIX,EROUND,EREFR,EPIVOT,EREDUC
      LOGICAL*1 EREN(4)
C
C     EFREE   Number of bounds freed
C     EFIX    Number of bounds fixed
C     EROUND  Number of bounds changed by integer rounding
C     EREFR   Number of primal and dual values changed by BASIS REFRESH
C     EPIVOT  Number of pivots performed on basis
C     EREDUC  Number of bounds overwritten by REDUCE
C     EREN    Byte map for what header names were changed:
C             1:PROBLEM  2:RHS  3:RANGESET  4:BOUNDSET
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      EFREE  = EDFREE
      EFIX   = EDFIX
      EROUND = EDROND
      EREFR  = EDREFR
      EPIVOT = PIVNUM
      EREDUC = EDRED
      DO 10 I=1,4
10    EREN(I) = EDREN(I)
C
      RETURN
C
C ** GETEDS ENDS HERE
      END
      SUBROUTINE GSETNM(OPTNM,OBJNM,RHSNM,RNGNM,BNDNM,MAXNL)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This sets names (for GETMAT to read in)
C
C       OPTNM = 'MINIMIZE' or 'MAXIMIZE' or ' '
C       OBJNM = objective row (must be N-type)
C       RHSNM = right-hand side set
C       RNGNM = range set
C       BNDNM = bound set
C       MAXNL = max name length
C
      CHARACTER*(*) OPTNM,OBJNM,RHSNM,RNGNM,BNDNM
C
C If no change in a xxxNM is wanted, it is entered as null (' ')
C Otherwise, the name passed is set into COMMON/GNAME/
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF(OBJNM.NE.' ')OBJNAM = OBJNM
      IF(RHSNM.NE.' ')RHSNAM = RHSNM
      IF(RNGNM.NE.' ')RNGNAM = RNGNM
      IF(BNDNM.NE.' ')BNDNAM = BNDNM
C Now set OPT
      CALL GSETOP(OPTNM)
C Finally, max name length
      IF(MAXNL.LT.2)THEN
         NAMELN = 2
      ELSE IF(MAXNL.GT.16)THEN
         NAMELN = 16
      ELSE
         NAMELN = MAXNL
      ENDIF
      RETURN
C
C ** GSETNM ENDS HERE
      END
      SUBROUTINE GSETOP(OPTNM)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This sets sense of optimization.
C
      CHARACTER*(*) OPTNM
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF( OPTNM(1:3).EQ.'MAX' )THEN
         OPT = OPTMAX
      ELSE IF( OPTNM(1:3).EQ.'MIN' )THEN
         OPT = OPTMIN
      ELSE
         PRINT *,' Warning: ',OPTNM,
     1           ' not recognized sense of optimization'
      ENDIF
      RETURN
C
C ** GSETOP ENDS HERE
      END
      SUBROUTINE GADDNZ(RNAME,V,VTZERO,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This adds nonzero value (V) for column NCOLS in row named RNAME.
C       VTOLZ = zero tolerance
C
C RCODE (return code) = 0  means all is well
C                     = 1  means RNAME not found
C                     = 2  means out of space
C                     =-1  means V too small to add
C                         i.e., ABS(V) < VTOLZ
C
      CHARACTER*(*) RNAME
C
C CALLS GETVID, GETNUM
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
C LOOKUP ROW
      CALL GETNUM('ROW ',RNAME,NUMBER)
      IF(NUMBER.EQ.0)THEN
           PRINT *,' Row ',RNAME,' not recognized'
           RCODE=1
           RETURN
      ENDIF
C
C GET VALUE ID (ADD TO POOL)
      IF(V.LT.-VTZERO)THEN
           SIGN = -1
           V = -V
      ELSE IF(V.GT. VTZERO)THEN
           SIGN = 1
      ELSE
C COEFFICIENT TOO SMALL...DO NOT ADD
           RCODE = -1
           RETURN
      ENDIF
C TEST IF = 1
      IF(ABS(V-1.0).LE.VTOLAB)THEN
           ID = 1
           NONES = NONES+1
      ELSE
           CALL GETVID(V,ID)
           IF(ID.EQ.0)THEN
              PRINT *,' Sorry, not enough VALUE space'
              RCODE = 2
              RETURN
           ENDIF
      ENDIF
C
C NOW VALUE(ID) = ABS(V)
C
C NONZERO INDEXES ARE GROWN UPWARD FROM BOTTOM OF INDEX
C ...INONZ IS RUNNING POINTER, DECREMENTED BY 2 FOR EACH NONZERO
C
C CHECK OVERFLOW
       IF(2*(IFREE + NCOLS + 1).GE.INONZ)THEN
           PRINT *,' Sorry, ',NONZER,' are too many nonzeroes'
           RCODE = 2
           RETURN
       ENDIF
C OK, PUT ROW INDEX (NUMBER)
      INDEX(INONZ) = NUMBER
C PUT SIGNED VALUE INDEX
      INDEX(INONZ-1) = ID*SIGN
C ADVANCE POINTER
      INONZ = INONZ-2
C ...AND NONZERO COUNT
      NONZER = NONZER+1
C
C INCREMENT ROW & COLUMN (NCOLS) COUNTS
      INDEX(IRINFO+NUMBER)=INDEX(IRINFO+NUMBER)+1
      BASE(ICINFO+NCOLS) =BASE(ICINFO+NCOLS) +1
C
      RETURN
C
C ** GADDNZ ENDS HERE
      END
      SUBROUTINE GSETSL(SWRATE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This sets solution to all-logical basis
C (Assumes OPT is set correctly for status setting.)
C
      LOGICAL*1 SWRATE
C ...SWRATE = TRUE iff enough space to support pivot info
C
C SUCCESSFULL RETURN SETS STCOMP=3 (UNKNOWN) AND STBASC=1 (BASIC)
C
C CALLS  GPRDCT, GETBND, GETVAL
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
C RE-SET POINTER FOR LIST OF BASIC VARIABLES (NO RATE FOR LOGICAL BASIS)
      IPIVOT = 0
      SWRATE = .FALSE.
C
C SET ROW STATUS AND INITIALIZE SOLUTION VALUE
C
      DO 100 I=1,NROWS
           INDEX(IRSTAT+I) = 3
           VALUE(IRSOLV+I) = 0.0
100   CONTINUE
C
C LOOP OVER COLUMNS TO SET STATUS AND ACCUMULATE y=Ax
C
      SOLST = 'OPTIMAL'
      I1 = INONZ
C
      DO 200 J=1,NCOLS
        I2 = INONZ + 2*BASE(ICINFO+J) - 1
C       :::::::::::::::::::::::::::::::::::::::
C        The nonzero pointers are I1,I2.  We
C        shall first set status to finite bound
C        (L or U) arbitrarily;  if column J is
C        is free (L = -inf and U = +inf), we shall
C        make x = 0 with status=Basic, giving an
C        overdetermined "basis."  Also, to test
C        unboundedness we shall determine the
C        qualitative max change:
C
C         If L, SIGN = 1, MOVEDN = 0, MOVEUP = 0 | 1
C         If U, SIGN =-1, MOVEDN = 0 | 1, MOVEUP = 0
C         If B, SIGN = 0, MOVEDN = 1, MOVEUP = 1
C
C       The SIGN is the direction of movement, allowing free variables
C       to move up or down.  MOVEUP and MOVEDN qualitatively determine
C       limits, where 0 means finite limit and 1 means unlimited.
C
C        After we complete the loop through column
C        J's nonzeroes, we shall test its reduced
C        cost sign (for phase 2), which is simply
C        its input cost, adjusted for sense of
C        optimization (OPT).  If it is not optimal,
C        we set SOLST = feasible unless we already
C        have SOLST = unbounded.  If the reduced
C        cost is not optimal, we test for unboundedness
C        using MOVEUP and MOVEDN.
C
C        The net result is that column J will either
C        determine SOLST = unbounded, or it will
C        change SOLST from optimal to feasible, or
C        it will leave SOLST unchanged.  After we
C        complete the column loop, we shall test feasibility
C        so that SOLST will be the correct status of this
C        solution.
C       ::::::::::::::::::::::::::::::::::::::::::::::::::::
C
           CALL GETVAL(VL,ICLBND+J)
           CALL GETVAL(VU,ICUBND+J)
C
           IF(VL.GT.-VINF)THEN
C            SET X = L
               INDEX(ICSTAT+J) = 1
               VX = VL
C          ...SET DIRECTION/LIMIT
               SIGN = 1
               MOVEDN = 0
               IF(VU.LT.VINF)THEN
                  MOVEUP = 1
               ELSE
                  MOVEUP = 0
               ENDIF
C
           ELSE IF(VU.LT.VINF)THEN
C            SET X = U
               INDEX(ICSTAT+J) = 2
               VX = VU
C          ...SET DIRECTION/LIMIT
               SIGN = -1
               MOVEUP = 0
               MOVEDN = 1
C
           ELSE
C            SET X = 0 BUT STAT = BASIC
               INDEX(ICSTAT+J) = 3
               VX = 0.0
               SIGN = 0
               MOVEUP = 1
               MOVEDN = 1
           ENDIF
C
C          INITIALIZE REDUCED COST = 0
           VALUE(ICSOLV+J) = 0.0
C
           IF(I2.LE.I1)GOTO 175
C
C LOOP OVER COLUMN J'S NONZEROES TO ACCUMULATE y AND TO SET DJ=OBJ
C
           DO 150 I=I1,I2,2
              ROW = INDEX(I)
              CALL GETVAL(V,I+1)
              IF(ROW.EQ.OBJNUM)VALUE(ICSOLV+J) = V
              VALUE(IRSOLV+ROW) = VALUE(IRSOLV+ROW) + V*VX
C         ...UPDATE MOVEMENT
C
              IF(MOVEUP.EQ.1)THEN
                 IF(V.GT.0.0)THEN
                    IF(INDEX(IRUBND+ROW).NE.INF)MOVEUP = 0
                 ELSE IF(V.LT.0.0)THEN
                    IF(INDEX(IRLBND+ROW).NE.-INF)MOVEUP = 0
                 ENDIF
              ELSE IF(MOVEDN.EQ.1)THEN
                 IF(V.GT.0.0)THEN
                    IF(INDEX(IRLBND+ROW).NE.-INF)MOVEDN = 0
                 ELSE IF(V.LT.0.0)THEN
                    IF(INDEX(IRUBND+ROW).NE.INF)MOVEDN = 0
                 ENDIF
              ENDIF
C
C NOW MOVEUP = 0 IFF UPWARD MOVEMENT IS LIMITED (ELSE, 1);
C     MOVEDN = 0 IFF DOWNWARD MOVEMENT IS LIMITED (ELSE, 1).
C
150        CONTINUE
C
           I1=I2 + 1
C
C CHECK OPTIMALITY OF REDUCED COST
C
175        CONTINUE
CC 8-2-95
           VDJ = VALUE(ICSOLV+J) * OPTMIN*OPT
C NOW SIGN*VDJ > 0 IFF OPTIMAL     ==========  =1 IF MIN, -1 IF MAX
           IF( SIGN*VDJ.GT.-VTOLAB )GOTO 200
CC =======
C        ACTIVITY IS NOT OPTIMAL...TEST FOR UNBOUNDEDNESS
             IF(MOVEUP+MOVEDN.GT.0)SOLST = 'UNBOUNDED'
             IF(SOLST.EQ.'OPTIMAL')SOLST = 'FEASIBLE'
C _____________
200   CONTINUE
C~~~~~~~~~~~~~~
C LOOP OVER ROWS TO CHECK FEASIBILITY
      DO 300 NUMBER=1,NROWS
         CALL GETBND('ROW ',NUMBER,VL,VU)
         VY = VALUE(IRSOLV+NUMBER)
              IF( VY.LT.VL - VTOLAB - VTOLRE*ABS(VL) )THEN
            VALUE(IVFREE+NUMBER) = -1.0
         ELSE IF( VY.GT.VU + VTOLAB + VTOLRE*ABS(VU) )THEN
            VALUE(IVFREE+NUMBER) = 1.0
         ELSE
            VALUE(IVFREE+NUMBER) = 0.0
            GOTO 300
         ENDIF
           SOLST = 'INFEASIBLE'
           INDEX(IRSTAT+NUMBER) = 4
300   CONTINUE
C
      IF(SOLST.EQ.'INFEASIBLE')THEN
C LP IS INFEASIBLE...ADJUST REDUCED COSTS OF COLUMNS FOR PHASE 1
C
         DO 500 COLUMN=1,NCOLS
500      CALL GPRDCT(VALUE(IVFREE+1),COLUMN,VALUE(ICSOLV+COLUMN))
C
      ENDIF
C
      SOLNAM = 'logical'
      STCOMP = 0
      STBASC = 1
      RETURN
C
C ** GSETSL ENDS HERE
      END
      SUBROUTINE GPUTSL(ISTAT,ISOLV,STAT,VX,VD,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C PUT SOLUTION STAT (B,L,U, or I) AND VALUE (VX=LEVEL, VD=DUAL PRICE)
C
      CHARACTER*(*) STAT
C LOCAL
      CHARACTER*1  CHAR
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF(ISOLV.GT.PMXVAL)THEN
         PRINT *,' ** SYSERR...GPUTSL...ISOLV =',ISOLV,
     1                                  ' > PMXVAL =',PMXVAL
         RCODE = 13
         RETURN
      ENDIF
      IF(ISTAT.GT.PMXIND)THEN
         PRINT *,' ** SYSERR...GPUTSL...ISTAT =',ISTAT,
     1                                  ' > PMXIND =',PMXIND
         RCODE = 13
         RETURN
      ENDIF
      CHAR = STAT
      IF( CHAR.EQ.'F' )CHAR = 'L'
      IF( SOLST.EQ.'OPTIMAL '.AND.(CHAR.EQ.'L'.OR.CHAR.EQ.'U') )THEN
C SEE IF COL
         COL = ISTAT - ICSTAT
         IF( COL.GE.1 .AND. COL.LE.NCOLS )THEN
C ...YES, GET ITS BOUNDS
            CALL GETBND('COL ',COL,VL,VU)
            IF( VL.GE.VU )THEN
C ...COL IS FIXED (VL=VU)...CHECK ITS PRICE
C    (CALLER MIGHT HAVE WRONG STAT)
               IF( VD*OPT.LT.0. )THEN
                  CHAR = 'L'
               ELSE IF( VD*OPT.GT.0. )THEN
                  CHAR = 'U'
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF( CHAR.EQ.'L' )THEN
           INDEX(ISTAT) = 1
           VALUE(ISOLV) = VD
      ELSE IF( CHAR.EQ.'U' )THEN
           INDEX(ISTAT) = 2
           VALUE(ISOLV) = VD
      ELSE IF( CHAR.EQ.'B' )THEN
           INDEX(ISTAT) = 3
           VALUE(ISOLV) = VX
      ELSE IF( CHAR.EQ.'I' )THEN
           INDEX(ISTAT) = 4
           VALUE(ISOLV) = VX
      ELSE
           RCODE = 1
      ENDIF
      RETURN
C
C ** GPUTSL ENDS HERE
      END
      SUBROUTINE BASCLR
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This clears basis (removes alpha's and restores space).
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::::
      IF( IPIVOT.EQ.0 )RETURN
      IFREE  = IPIVOT + 1
      IVFREE = IVBLST + 1
      IRBLST = 0
      IVBLST = 0
      IZBLST = 0
      IPIVOT = 0
      PIVNUM = 0
C
C ** BASCLR ENDS HERE
      END
C
C     .~~~~~~~~~~~~~~~.
C     : MAPS Routines :
C     :...............:
C
      SUBROUTINE GMPCLR
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This clears maps
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      DO 100 I=IRSTAT+1,IRSTAT+NROWS
           IF(INDEX(I).LT.0)INDEX(I) = -INDEX(I)
100   CONTINUE
C
      DO 150 I=ICSTAT+1,ICSTAT+NCOLS
           IF(INDEX(I).LT.0)INDEX(I) = -INDEX(I)
150   CONTINUE
C
      RETURN
C
C ** GMPCLR ENDS HERE
      END
      SUBROUTINE GETMAP(ROWCOL,NUMBER,SW)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets map for row/column NUMBER...SW = T if map bit is on
C
      LOGICAL*1     SW
      CHARACTER*(*) ROWCOL
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF(ROWCOL.EQ.'ROW ')THEN
           SW = INDEX(IRSTAT + NUMBER).LT.0
      ELSE IF(ROWCOL.EQ.'COL ')THEN
           SW = INDEX(ICSTAT + NUMBER).LT.0
      ELSE
           PRINT *,' ** SYSERR GMP',ROWCOL,'...please report'
      ENDIF
C
      RETURN
C
C ** GETMAP ENDS HERE
      END
      SUBROUTINE GPUTMP(ROWCOL,NUMBER,SW)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This puts map for row/column NUMBER
C ...SW=T if map bit is to be on
C
      LOGICAL*1 SW
      CHARACTER*4 ROWCOL
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF(ROWCOL.EQ.'ROW ')THEN
           I = IRSTAT+NUMBER
      ELSE IF(ROWCOL.EQ.'COL ')THEN
           I = ICSTAT+NUMBER
      ENDIF
C
      IF(SW)THEN
           IF(INDEX(I).GT.0)INDEX(I) = -INDEX(I)
      ELSE
           IF(INDEX(I).LT.0)INDEX(I) = -INDEX(I)
      ENDIF
C
      RETURN
C
C ** GPUTMP ENDS HERE
      END
      SUBROUTINE GMAPNZ(NRSUB,ROW1,NCSUB,COL1,NZSUB)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This removes null rows/columns from submatrix and resets:
C
C    NRSUB = NUMBER OF ROWS IN SUBMATRIX
C    ROW1  = FIRST ROW NUMBER IN SUBMATRIX
C    NCSUB = NUMBER OF COLUMNS IN SUBMATRIX
C    COL1  = FIRST COLUMN NUMBER IN SUBMATRIX
C    NZSUB = NUMBER OF NONZEROES IN SUBMATRIX
C
      INTEGER*4 I,I1,I2
      LOGICAL*1 SW
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      BITON = 2**14
      NZSUB = 0
      IF(COL1.LE.0)COL1=1
      IF(ROW1.LE.0)ROW1=1
C
C LOOP OVER COLUMNS
C
      NEW1 = COL1
      DO 500 COLUMN=COL1,NCOLS
           IF(INDEX(ICSTAT+COLUMN).GT.0)GOTO 500
C     COLUMN IS IN SUBMATRIX...TEST IF NULL OVER ROWS IN SUBMATRIX
C
           I1 = INONZ + 2*BASE(ICINFO+COLUMN-1)
           NZ = BASE(ICINFO+COLUMN) - BASE(ICINFO+COLUMN-1)
           IF(NZ.EQ.0)GOTO 400
           I2 = I1 + 2*NZ - 1
C
C          INITIALIZE SW (FALSE MEANS NULL)
           SW = .FALSE.
C
C          LOOP OVER ROWS
C
           DO 300 I=I1,I2,2
             ROW = INDEX(I)
             IF(INDEX(IRSTAT+ROW).GT.0)GOTO 300
C       ...ROW IS IN SUBMATRIX
             NZSUB = NZSUB + 1
             IF(-INDEX(IRSTAT+ROW).LT.BITON)
     1        INDEX(IRSTAT+ROW)=INDEX(IRSTAT+ROW) - BITON
C
C          (BITON SHOWS ROW HAD COLUMN INTERSECTION)
             SW = .TRUE.
300        CONTINUE
C
           IF(SW)GOTO 500
C
C          COLUMN IS NULL...REMOVE FROM SUBMATRIX
C
400        INDEX(ICSTAT+COLUMN) = -INDEX(ICSTAT+COLUMN)
           NCSUB = NCSUB - 1
           IF(COLUMN.EQ.NEW1)NEW1=COLUMN+1
500   CONTINUE
C
      IF(NEW1.LE.NCOLS)COL1 = NEW1
      NEW1 = ROW1
C
C NOW LOOP OVER ROWS TO RETAIN ONLY NONNULL ONES
C
      DO 600 ROW=ROW1,NROWS
           IF(INDEX(IRSTAT+ROW).GT.0)GOTO 600
           IF(-INDEX(IRSTAT+ROW).GT.BITON)THEN
C          ..ROW IS NOT NULL...RESTORE MAP (IE, TURN OFF BIT)
             INDEX(IRSTAT+ROW) = INDEX(IRSTAT+ROW) + BITON
           ELSE
C          ...REMOVE NULL ROW
             INDEX(IRSTAT+ROW) = -INDEX(IRSTAT+ROW)
             NRSUB = NRSUB - 1
             IF(ROW.EQ.NEW1)NEW1=ROW+1
           ENDIF
600   CONTINUE
C
      IF(NEW1.LE.NROWS)ROW1=NEW1
C
      RETURN
C
C ** GMAPNZ ENDS HERE
      END
