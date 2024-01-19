C             ::: GETEDIT.FOR  8-02-95 :::
C
C Earlier dates deleted
C       2-12-95...Added ROWDBG and COLDBG to REDCOM in GDEBUG
C       5-14-95...Added call REDEBG in GDEBUG
C       5-26-95...Added some things to GDEBUG + GPRBAS
C
C This contains the following GETMAT routines.
C
C   GETFRE...Free bound/limit on column/row
C   GETFIX...Fix level of column/row
C   GROUND...Round level of column
C   GTVNEW...Get value ID for new value (cannot use GETVID)
C   GRENAM...Rename something (eg, probname)
C   GPTSOL...Put solution value(s)
C            CALLED BY OUTSIDER (LIKE ANALYZE)
C            USE GPUTSL (IN GETSET.FOR) WITHIN GETMAT (LIKE GETIOSOL)
C   GDEBUG...Debug routine
C   GPRBAS...Writes BASE for GDEBUG (ADDED 7-29-95)
C
      SUBROUTINE GETFRE(WHAT,ROWCOL,NUMBER,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This frees bound/limit on ROWCOL NUMBER in submatrix.
C
      CHARACTER*(*) ROWCOL,WHAT
C
C       WHAT := LOWER | UPPER | BOTH
C
C       RCODE = -1 IF LOWER IS REVISED AND STAT = L
C             =  1 IF UPPER IS REVISED AND STAT = U
C              IF RCODE <> 0 UPON ENTRANCE, REVISION DOES NOT OCCUR
C LOCAL
      CHARACTER*1 STAT
      LOGICAL*1   SWOK,SWFIX,SWFREE
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
      IF( ROWCOL.EQ.'ROW ' )THEN
         LO = IRLBND + NUMBER
         UP = IRUBND + NUMBER
         SWFREE= (INDEX(LO).EQ.-INF) .AND. (INDEX(UP).EQ.INF)
         IF( SWFREE )THEN
C ROW IS ALREADY FREE
            RCODE = 0
            RETURN
         ENDIF
         SWFIX = .FALSE.
         ISTAT = IRSTAT + NUMBER
         ISOLV = IRSOLV + NUMBER
      ELSE
         LO = ICLBND + NUMBER
         UP = ICUBND + NUMBER
         SWFIX = INDEX(LO).EQ.INDEX(UP)
         SWFREE= .TRUE.
         ISTAT = ICSTAT + NUMBER
         ISOLV = ICSOLV + NUMBER
      ENDIF
      CALL GETST(ROWCOL,NUMBER,STAT,STNUM)
      SWOK = .TRUE.
      IF( WHAT.NE.'LOWER' .AND. INDEX(UP).NE.INF )THEN
C FREE UPPER
         IF( STAT.EQ.'U' )THEN
            IF( RCODE.EQ.0 )THEN
C CHANGE SOLUTION STAT TO BASIC AND SET VALUE = VUP
               CALL GETVAL(V,UP)
               CALL GPUTSL(ISTAT,ISOLV,'B',V,0.,RCODE)
C PUT INTO SUBMATRIX
               INDEX(ISTAT) = -INDEX(ISTAT)
            ENDIF
            SWOK = .FALSE.
            RCODE = 1
         ENDIF
C FREE UPPER BOUND/LIMIT
         INDEX(UP) = INF
         EDFREE = EDFREE+1
      ENDIF
      IF( WHAT.NE.'UPPER' .AND. INDEX(LO).NE.-INF )THEN
         IF( STAT.EQ.'L' )THEN
            IF( RCODE.EQ.0 )THEN
C CHANGE SOLUTION STAT TO BASIC AND SET VALUE = VLO
               CALL GETVAL(V,LO)
               CALL GPUTSL(ISTAT,ISOLV,'B',V,0.,RCODE)
C PUT INTO SUBMATRIX
               INDEX(ISTAT) = -INDEX(ISTAT)
            ENDIF
            SWOK = .FALSE.
            RCODE = -1
         ENDIF
C FREE LOWER BOUND/LIMIT
         INDEX(LO) = -INF
         EDFREE = EDFREE+1
      ENDIF
C
      IF( SWOK )RCODE = 0
      IF( SWFIX .AND. INDEX(LO).NE.INDEX(UP) )NCFIX = NCFIX - 1
      IF( .NOT.SWFREE .AND. INDEX(LO).EQ.-INF .AND.
     1                      INDEX(UP).EQ. INF )NRFREE = NRFREE + 1
      RETURN
C
C ** GETFRE ENDS HERE
      END
      SUBROUTINE GETFIX(ROWCOL,NUMBER,V,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This fixes level of ROWCOL NUMBER = V (bounds change).
C ...RCODE > 0 IF FATAL ERROR (NO REVISION)
C STATUS UPDATED FOR BASICS (I TO B OR B TO I), BUT
C CALLER IS RESPONSIBLE FOR REFRESHING SOLUTION.
C
      CHARACTER*(*) ROWCOL
C LOCAL
      CHARACTER*1  STAT
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
C GET CURRENT BOUNDS (MIGHT BE NO CHANGE)
      CALL GETBND(ROWCOL,NUMBER,VL,VU)
      IF( VL.GE.VU .AND. ABS(VL-V).LE.VTOLAB )RETURN
C EITHER VL < VU (ROWCOL NUMBER NOT FIXED)
C OR     VL=VU <> V (ROWCOL NUMBER FIXED AT DIFFERENT VALUE)
      CALL GTVNEW(V,ID,RCODE)
      IF( RCODE.NE.0 )RETURN
      IF( ROWCOL.EQ.'ROW ' )THEN
         LO = IRLBND + NUMBER
         UP = IRUBND + NUMBER
         IF( INDEX(LO).EQ.-INF.AND.INDEX(UP).EQ.INF )NRFREE = NRFREE-1
      ELSE
         LO = ICLBND + NUMBER
         UP = ICUBND + NUMBER
         IF( INDEX(LO).NE.INDEX(UP) )THEN
C ADD TO NUMBER OF FIXED COLS
            NCFIX = NCFIX + 1
            IF( BNDNAM.EQ.'none' )BNDNAM = 'FIX_EDIT'
         ENDIF
      ENDIF
C
      INDEX(UP) = ID
      INDEX(LO) = ID
      CALL GETSOL(ROWCOL,NUMBER,VX,VP,STAT,STNUM)
      EDFIX = EDFIX + 1
      IF( STAT.EQ.'L' .OR. STAT.EQ.'U' )THEN
         VX = V
      ELSE
C PUT STAT (MAYBE CHANGED BETWEEN B AND I)
         IF( ABS(VX-V).LE.VTOLAB )THEN
            STAT = 'B'
         ELSE
            STAT = 'I'
         ENDIF
      ENDIF
      CALL GPTSOL(ROWCOL,NUMBER,STAT,VX)
C
      RETURN
C
C ** GETFIX ENDS HERE
      END
      SUBROUTINE GROUND(COLUMN,CHBND,VBND,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This rounds level of COLUMN to nearest integer.
C Upon RETURN,
C   VBND = ROUND(X)
C  CHBND = L if COLUMN lower bound changed = VBND
C  CHBND = U if COLUMN upper bound changed = VBND
C  CHBND = blank if no change
C RCODE > 0 IF FATAL ERROR (NO REVISION)
C CALLER IS RESPONSIBLE FOR REFRESHING BASIS.
      CHARACTER*1 CHBND
C LOCAL
      CHARACTER*1 CSTAT
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
      CHBND = ' '
      CALL GETSOL('COL ',COLUMN,VX,VP,CSTAT,STNUM)
      INT = VX + .5
      VBND= FLOAT(INT)
      IF( CSTAT.EQ.'L'.OR.CSTAT.EQ.'U' )RETURN
      IF( VBND.LT.VX-VTOLAB )THEN
C ROUND DOWN BY SETTING XU=VBND
         CSTAT = 'U'
      ELSE IF( VBND.GT.VX+VTOLAB )THEN
C ROUND UP BY SETTING XL=VBND
         CSTAT = 'L'
      ELSE
         RETURN
      ENDIF
C COL IS BASIC AND X IS NOT INTEGER
      CALL GTVNEW(VBND,ID,RCODE)
      IF( RCODE.NE.0 )RETURN
C CHANGE BOUND OF COLUMN
      CHBND = CSTAT
      EDROND = EDROND + 1
      IF( CSTAT.EQ.'L' )THEN
         INDEX(ICLBND+COLUMN) = ID
      ELSE
         INDEX(ICUBND+COLUMN) = ID
      ENDIF
      CALL GPTSOL('COL ',COLUMN,'I',VX)
C MAKE SURE THERE'S A BOUND SET
      IF( BNDNAM.EQ.'none' )BNDNAM = 'ROUNDED'
C SEE IF COLUMN BECAME FIXED (IF SO, UPDATE NUMBER)
C NOTE:  COLUMN WAS NOT FIXED BEFORE SINCE IT IS BASIC AND NOT INTEGER
      IF( INDEX(ICLBND+COLUMN).EQ.INDEX(ICUBND+COLUMN) )NCFIX=NCFIX+1
C
      RETURN
C
C ** GROUND ENDS HERE
      END
      SUBROUTINE GTVNEW(V,ID,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets value ID for V (added to pool, if necessary).
C ...RCODE > 0 IF NOT IN POOL AND NOT ENOUGH SPACE TO ADD.
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
      IF( V.LT.-VTOLAB )THEN
         VNEW = -V
         SIGN = -1
      ELSE IF( V.GT.VTOLAB )THEN
         VNEW = V
         SIGN = 1
      ELSE
         ID = 0
         RETURN
      ENDIF
C
      DO 100 I=1,NVALS
         IF( ABS(VNEW-VALUE(I)).LE.VTOLAB+VTOLRE*(VNEW+VALUE(I)) )THEN
            ID = SIGN*I
            RETURN
         ENDIF
100   CONTINUE
C
C VALUE NOT IN POOL
      IF( IVFREE.GE.PMXVAL )THEN
         RCODE = 1
         RETURN
      ENDIF
C ADD VALUE
      ID = SIGN*IVFREE
      VALUE(IVFREE) = VNEW
      IVFREE = IVFREE+1
C
      RETURN
C
C ** GTVNEW ENDS HERE
      END
      SUBROUTINE GRENAM(PRBNM,RHSNM,RNGNM,BNDNM,SOLNM)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This renames:
C     PRBNM  = problem name
C     RHSNM  = right-hand side name
C     RNGNM  = range name
C     BNDNM  = bound name
C     SOLNM  = solver name
C
        CHARACTER*(*) PRBNM,RHSNM,RNGNM,BNDNM,SOLNM
C ::::::::::::::::::::::::::: BEGIN :::::::::::::::::::::::::::::
      IF( PRBNAM .NE. PRBNM )EDREN(1) = .TRUE.
      IF( RHSNAM .NE. RHSNM )EDREN(2) = .TRUE.
      IF( RNGNAM .NE. RNGNM )EDREN(3) = .TRUE.
      IF( BNDNAM .NE. BNDNM )EDREN(4) = .TRUE.
      PRBNAM = PRBNM
      RHSNAM = RHSNM
      RNGNAM = RNGNM
      BNDNAM = BNDNM
      SOLNAM = SOLNM
      RETURN
C
C ** GRENAM ENDS HERE
      END
      SUBROUTINE GPTSOL(ROWCOL,NUMBER,STAT,VAL)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This puts solution STAT and VALue in ROWCOL NUMBER.
C
      CHARACTER*(*) ROWCOL,STAT
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF( STAT.EQ.'L' )THEN
         STNUM = 1
      ELSE IF( STAT.EQ.'U' )THEN
         STNUM = 2
      ELSE IF( STAT.EQ.'B' )THEN
         STNUM = 3
      ELSE
         STNUM = 4
      ENDIF
      IF( ROWCOL.EQ.'ROW ' )THEN
         VALUE(IRSOLV+NUMBER) = VAL
         IF( INDEX(IRSTAT+NUMBER).GT.0 )THEN
            INDEX(IRSTAT+NUMBER) = STNUM
         ELSE
            INDEX(IRSTAT+NUMBER) = -STNUM
         ENDIF
      ELSE
         VALUE(ICSOLV+NUMBER) = VAL
         IF( INDEX(ICSTAT+NUMBER).GT.0 )THEN
            INDEX(ICSTAT+NUMBER) = STNUM
         ELSE
            INDEX(ICSTAT+NUMBER) = -STNUM
         ENDIF
      ENDIF
C UPDATE SOLUTION STATS
      IF( STAT.EQ.'I' )SOLST = 'INFEASIBLE'
      IF( SOLST.EQ.'INFEASIBLE' )THEN
         STCOMP = 2
         RETURN
      ENDIF
C SOLUTION STATUS IS NOT INFEASIBLE...GET BOUNDS
      CALL GETBND(ROWCOL,NUMBER,VL,VU)
      IF( STAT.EQ.'B' )THEN
C CHECK COMPLEMENTARITY
         IF( VAL.LE.VL .OR. VAL.GE.VU )STCOMP = 2
      ELSE
C CHECK COMPLEMENTARITY
         IF( ABS(VAL).LE.VTOLAB )STCOMP = 2
         IF( SOLST.EQ.'OPTIMAL' )THEN
C CHECK OPTIMALITY
            VP = -OPT*VAL
            IF( STAT.EQ.'U' )VP = -VP
            IF( VP.GT.VTOLAB )SOLST = 'FEASIBLE'
         ENDIF
      ENDIF
C
      RETURN
C
C ** GPTSOL ENDS HERE
      END
      SUBROUTINE GDEBUG
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This is debug routine entered from SYSDBG (or called by a GETMAT routine).
C
C LOCAL
      CHARACTER*16  STR16
      CHARACTER*8   ARRAY
      CHARACTER*4   ROWCOL
      CHARACTER*1   CHAR
      CHARACTER*128 CLIST
      PARAMETER              (LCLMXR=100)
      DOUBLE PRECISION ZLOCAL(LCLMXR)
C CALLS  FVRNG, FPAUSE
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
       IF(PAUSE.LT.0)THEN
          PRINT *,' ...PRESS RETURN'
          READ(*,1)CHAR
       ENDIF
1      FORMAT(A1)
       PRINT *,' GETMAT DEBUG 2/94'
       PAUSE = PAUSE0
C
5      CONTINUE
         PRINT *,' Base, Index, Value, Name, Reduce, Header,',
     1           ' bAsis, Tran, Pivot'
         PRINT *,' roW, Col, Debug '
         READ(*,1)CHAR
C
         IF(CHAR.EQ.' ')RETURN
         IF(CHAR.EQ.'H')THEN
            PRINT *,' DATFIL,PCKFIL=',DATFIL,PCKFIL
            PRINT *,' VTOLAB,VTOLRE=',VTOLAB,VTOLRE
            PRINT *,' PRBNAM=',PRBNAM,' OBJNAM=',OBJNAM,
     1              ' RHSNAM=',RHSNAM
            PRINT *,' RNGNAM=',RNGNAM,' BNDNAM=',BNDNAM,
     1              ' SOLST=',SOLST
            PRINT *,' SOLNAM=',SOLNAM,' NAMELN=',NAMELN
            GOTO 5
         ENDIF
         RCODE=0
         IF(CHAR.EQ.'N')THEN
                ARRAY = 'NAME'
                BRANCH = 1
                MAX = MAXNAM
         ELSE IF(CHAR.EQ.'I')THEN
                ARRAY = 'INDEX'
                BRANCH = 2
                PRINT *,' MAXIND =',MAXIND,' PMXMAX =',PMXIND
                MAX = PMXIND
         ELSE IF(CHAR.EQ.'B')THEN
                ARRAY = 'BASE'
                BRANCH = 3
                MAX = PMXBAS
         ELSE IF(CHAR.EQ.'V')THEN
                PRINT *,' Real, Integer, Z '
                READ(*,1)CHAR
                IF(CHAR.EQ.'R')THEN
                        ARRAY = 'VALUE'
                        MAX    = MAXVAL
                        BRANCH = 4
                ELSE IF(CHAR.EQ.'I')THEN
                        ARRAY = 'I4VAL'
                        MAX    = MAXVAL
                        BRANCH = 6
                ELSE IF(CHAR.EQ.'Z')THEN
                        ARRAY  = 'ZVALUE'
                        MAX    = PMXVAL/2
                        BRANCH = 7
                ELSE
                        PRINT *,' ?',CHAR
                        GOTO 5
                ENDIF
         ELSE IF(CHAR.EQ.'R')THEN
                CALL REDEBG
                GOTO 5
         ELSE IF(CHAR.EQ.'A')THEN
                PRINT *,' Stats, Alpha, aGenda, Buckets, sPikes, Find '
                READ(*,1)CHAR
                IF( CHAR.EQ.' ' )GOTO 5
                IF(CHAR.EQ.'A')THEN
                   ARRAY = 'B-ALPHA'
                   BRANCH= 8
                   MAX   = NBLST
                ELSE IF(CHAR.EQ.'S')THEN
                   ARRAY = 'B-STATS'
                   BRANCH= 9
                   MAX   = NROWS
                ELSE IF(CHAR.EQ.'G')THEN
                   ARRAY = 'B-AGENDA'
                   BRANCH= 10
                   MAX   = NBPOSN
                ELSE IF(CHAR.EQ.'B')THEN
                   ARRAY = 'BUCKETS'
                   BRANCH= 12
                   MAX   = NBUCKT
                ELSE IF(CHAR.EQ.'P')THEN
                   ARRAY = 'B-SPIKES'
                   BRANCH= 13
                   MAX   = NBSPIK
                ELSE IF(CHAR.EQ.'F')THEN
71                 CONTINUE
                   PRINT *,' Column name: '
                   READ(*,'(A16)')STR16
                   IF( STR16.EQ.' ' )GOTO 5
                   CALL GETNUM('COL ',STR16,NUMBER)
                   IF( NUMBER.EQ.0 )THEN
                      PRINT *,' ',STR16,' NOT A COLUMN'
                      CALL FC2I(STR16,16,NUMBER,RCODE)
                      IF( RCODE.NE.0 )THEN
                         RCODE = 0
                         GOTO 71
                      ENDIF
                      CALL GETNAM('COL ',NUMBER,STR16)
                      IF( STR16.EQ.' ' )GOTO 71
                   ENDIF
                   CALL GETST('COL ',NUMBER,CHAR,STNUM)
                   IF( CHAR.NE.'B' .AND. CHAR.NE.'I' )THEN
                      PRINT *,' ',STR16,' NOT BASIC...STAT=',CHAR
                      GOTO 71
                   ENDIF
                   PRINT *,' ...SCANNING BCPOSN FOR ',NUMBER,':',STR16
                   DO 75 I=1,NBPOSN
                      COL = INDEX(BCPOSN+I)
                      IF( COL.EQ.NUMBER )THEN
                         IF( I.LT.FRONT .OR. I.GT.REAR )THEN
                            ROW   = INDEX(BCSTAT+I)
                            STR16 = NAME(IRNAME+ROW)
                         ELSE
                            STR16 = 'UNASSIGNED'
                         ENDIF
                         PRINT *,' AT POSN',I,' ',STR16
                         GOTO 71
                      ENDIF
75                 CONTINUE
                   PRINT *,' ...SCANNING IPIVOT'
                   DO 76 PIVROW=1,NROWS
                      N = INDEX(IPIVOT+PIVROW)
                      IF( N.LT.0 )N=-N
                      IF( N.EQ.NUMBER )THEN
                        STR16 = NAME(IRNAME+PIVROW)
                        PRINT *,'    PIVOT ROW =',PIVROW,':',STR16
                        GOTO 77
                      ENDIF
76                 CONTINUE
                   GOTO 79
77                 CONTINUE
                   IF( NBLST.GT.0 )THEN
                      PRINT *,' ...SCANNING ALPHA'
                      CHAR  = ' '
                      APOSN = 0
                      DO 78 I=1,NBLST
                         IF( INDEX(IRBLST+I).GT.0 )GOTO 78
                         IF( CHAR.NE.' ' )THEN
                            CHAR = ' '
                            GOTO 78
                         ENDIF
                         N = -INDEX(IRBLST+I)
                         APOSN = APOSN+1
                         IF( N.EQ.PIVROW )THEN
                            PRINT *,'    ALPHA STARTS AT',I,
     1                              '  AGENDA POSN =',APOSN
                            GOTO 71
                         ENDIF
                         VABS = DABS( ZVALUE(IZBLST+I) )
                         IF( VABS.LE.0. )CHAR='N'
78                    CONTINUE
                   ENDIF
79                 PRINT *,' ** '//STR16//' NOT FOUND'
                ELSE
                   GOTO 5
                ENDIF
         ELSE IF(CHAR.EQ.'T')THEN
                PRINT *,' Btran, Ftran '
                READ(*,1)CHAR
                IF(CHAR.EQ.'B')THEN
                   ARRAY = 'BTRAN'
                   BRANCH= 11
                   MAX   = NROWS
                ELSE IF(CHAR.EQ.'F')THEN
                   ARRAY = 'FTRAN'
                   BRANCH= 11
                   MAX   = NCOLS
                ELSE
                   GOTO 5
                ENDIF
         ELSE IF(CHAR.EQ.'P')THEN
             PRINT *,' PIVMAX =',PIVMAX,' PIVNUM =',PIVNUM
             WRITE(*,'(
     1  7H PIVIN ,7H PIVOUT,7H PIVNBL,7H PIVST0,7H PIVSTI,7H PIVSTO)')
                DO 1010 I=1,PIVNUM+1
                   WRITE(*,'( 4(I5,2X),2X,L1,5X,L1 )')
     1     PIVIN(I),PIVOUT(I),PIVNBL(I),PIVST0(I),PIVSTI(I),PIVSTO(I)
1010            CONTINUE
                GOTO 5
         ELSE IF(CHAR.EQ.'D')THEN
                PRINT *,' Currently, SWDBG=',SWDBG
                CALL FPAUSE(PAUSE0)
                PAUSE = PAUSE0
                SWDBG = PAUSE0.GT.0
                PRINT *,' ...PAUSE=',PAUSE,'...Now SWDBG=',SWDBG
                RETURN
         ELSE
                IF( CHAR.EQ.'W' .OR. CHAR.EQ.'C' )GOTO 1000
                PRINT *,' ?',CHAR
                GOTO 5
         ENDIF
C
100      PRINT *,' ',ARRAY,' MAX = ',MAX
         PRINT *,' Enter range (v[/v])'
         READ(*,101)CLIST
101      FORMAT(A128)
         IF( CLIST.EQ.' ' )GOTO 5
         FIRST = 1
         CALL FSLEN(CLIST,40,LAST)
         N=0
102      CONTINUE
         CALL FVRNG(CLIST,FIRST,LAST,VL,VU,VINF,CHAR,RCODE)
         IF( RCODE.NE.0 )THEN
            IF( N.GT.0 )GOTO 5
            CALL FSINIT
            N = 1
            RCODE = 0
            FIRST = 1
            GOTO 102
         ENDIF
         IF( VL.LT.0.   ) VL = 0.
         IF( VL.LT.1. .AND. ARRAY.NE.'VALUE' ) VL = 1.
         IF( VU.GE.VINF ) VU = MAX
         I1 = VL+.1
         I2 = VU+.1
         IF( I2.GT.MAX )I2 = MAX
         IF( I1.GT.I2  )I1 = I2
         LINE = 0
C
         GOTO(110,120,130,140,150,160,170,180,190,200,300,400,500
     X       ),BRANCH
C
110      CONTINUE
         PRINT *,' Actual or Functional '
         READ(*,1)CHAR
         IF(CHAR.EQ.'F')GOTO 115
C ...PRINT ACTUAL ARRAY VALUES (NAMERC IS CHAR*1)
         WRITE(*,1111)NAMELN
1111     FORMAT('   USING NAME LENGTH (NAMELN) =',I2)
         DO 112 I=I1,I2,NAMELN
            WRITE(*,111)I,(NAMERC(II),II=I,I+NAMELN-1)
111         FORMAT(I7,':',T10,64A1/T10,6(9X,':'))
112      CONTINUE
         GOTO 100
115      CONTINUE
C ...PRINT NAME FUNCTIONAL VALUE (CHAR*16)
         DO 118 I=I1,I2,3
            WRITE(*,116)(K,NAME(K),K=I,I+2)
116         FORMAT(3(2X,I7,':',A16))
118      CONTINUE
         GOTO 100
C
120      WRITE(*,121)(I,INDEX(I),I=I1,I2)
121      FORMAT(5(1X,I6,'=',I8))
         GOTO 100
C
130      CONTINUE
CC 7-29-95
         CALL GPRBAS(I1,I2)
         GOTO 100
C
140      WRITE(*,141)(I,VALUE(I),I=I1,I2)
141      FORMAT( 1X,I7,'=',G20.12, 5X, I7,'=',G20.12)
         GOTO 100
C
150      PRINT *,' ** SHOULD NOT GET HERE (I2VAL REMOVED 9-3-93)'
         GOTO 100
C
160      WRITE(*,121)(I,I4VAL(I),I=I1,I2)
         GOTO 100
C
170      WRITE(*,171)(I,ZVALUE(I),I=I1,I2)
171      FORMAT(2(1X,I7,'=',D25.12))
         GOTO 100
C
180      CONTINUE
         WRITE(*,'(2X,20HOFFSET: INDEX  VALUE)')
         STR16 = ' '
         DO 189 I=I1,I2
            IF( I.EQ.NBLST )THEN
               STR16 = '= NBLST'
            ELSE IF( I.EQ.NBASNZ )THEN
               STR16 = '= NBASNZ'
            ELSE IF( INDEX(IRBLST+I).LT.0 )THEN
               ROW = -INDEX(IRBLST+I)
               IF( ZVALUE(IZBLST+I).EQ.0. )THEN
                  STR16 = 'NONSPIKE'
               ELSE IF( STR16.EQ.'NONSPIKE' )THEN
                  STR16 = NAME(ICNAME+ROW)
               ELSE IF( ZVALUE(IZBLST+I-1).NE.0. )THEN
                  ROW   = INDEX(IPIVOT+ROW)
                  STR16 = NAME(ICNAME+ROW)
               ENDIF
            ELSE
               STR16 = ' '
            ENDIF
            WRITE(*,181) I,INDEX(IRBLST+I),ZVALUE(IZBLST+I),STR16
181         FORMAT(1X,I7,':',I6,           D19.9,  1X,     A16)
189      CONTINUE
         GOTO 100
C
190      CONTINUE
         WRITE(*,'( 1X,14HIRSTAT,IPIVOT= ,2I8 )')IRSTAT,IPIVOT
         WRITE(*,'( 5X,3HROW,T27,5HRSTAT ,3X,5HPIVOT )')
         DO 199 I=I1,I2
            STAT= INDEX(IRSTAT+I)
            COL = INDEX(IPIVOT+I)
            IF( COL.GT.0.AND.(STAT.LT.0.OR.IRBLST.GT.0) )THEN
               STR16 = NAME(ICNAME+COL)
            ELSE IF( STAT.GT.0.AND.(COL.LT.0.OR.IRBLST.EQ.0) )THEN
               STR16 = 'UNASSIGNED'
            ELSE
               STR16 = ' '
            ENDIF
            WRITE(*,191) I,NAME(IRNAME+I),STAT,COL,STR16
191         FORMAT(1X, I7,':',A16,       2(1X,I7),1X,A16)
199      CONTINUE
         GOTO 100
C
200      CONTINUE
         IF( IRBLST.EQ.0 )GOTO 250
         WRITE(*,201)IRBLST,IZBLST
201      FORMAT(' GIVING AGENDA FROM IRBLST,IZBLST =',2I7 /
     1          ' POSN',T8,'BLST LOC',T17,'ROW',T34,'COL',T52,'PIVOT')
         LINE = LINE+2
         I = I1
         N = I1
         B1= 1
205      CONTINUE
C SKIP TO NEXT ALPHA
         B = B1
210      CONTINUE
           IF( B.GT.NBLST )THEN
              PRINT *,' END OF BLIST'
              GOTO 100
           ENDIF
           IF( INDEX(IRBLST+B).LT.0 )THEN
              ROW = -INDEX(IRBLST+B)
              Z   = ZVALUE(IZBLST+B)
              IF( Z.EQ.0. )THEN
                 B = B+1
                 Z = ZVALUE(IZBLST+B)
                 CLIST = 'NONSPIKE'
              ELSE
                 CLIST = 'SPIKE'
              ENDIF
              N = N-1
              IF(N.EQ.0)GOTO 211
           ENDIF
           B = B+1
         GOTO 210
C
211        CONTINUE
C N=0 MEANS WE REACHED POSN=I
           COL =  INDEX(IPIVOT+ROW)
           IF( COL.LT.0 )COL = 0
           IF( I.LE.NBFTRI )THEN
              CLIST = 'FTRI'
           ELSE IF( I.GE.NBFTRI+NBKRNL+1 )THEN
              CLIST = 'BTRI'
           ENDIF
           WRITE(*,202)I,B, NAME(IRNAME+ROW),NAME(ICNAME+COL),Z,CLIST
C                           |17                               |51
202        FORMAT(1X,I5,1X,I7,T17,2(A16,1X),             D20.12,1X,A4)
           IF( I.GT.I2 )GOTO 100
           I = I+1
           N = 1
           B1= B+1
         GOTO 205
C
250      CONTINUE
         PRINT *,' AGENDA NOT SET...GIVING AGENDA FROM ',
     1           'BCPOSN,BCSTAT=',BCPOSN,BCSTAT
         LINE = LINE+1
         WRITE(*,'( 8X,4HSTAT,T45,3HCOL )')
         DO 290 I=I1,I2
            COL   = INDEX(BCPOSN + I)
            STAT  = INDEX(BCSTAT + I)
            STR16 = ' '
            IF( I.EQ.FRONT.AND.FRONT.LE.REAR )THEN
               CHAR = 'F'
            ELSE IF( I.EQ.REAR.AND.FRONT.LE.REAR  )THEN
               CHAR = 'R'
            ELSE IF( I.LT.FRONT.OR.I.GT.REAR )THEN
               IF( I.LE.NBFTRI )THEN
                  STR16 = 'FTRI'
               ELSE IF( I.GT.NBPOSN-NBBTRI )THEN
                  STR16 = 'BTRI'
               ELSE IF( I.GT.REAR )THEN
                  CHAR  = 'S'
               ELSE
                  STR16 = 'NONSPIKE'
               ENDIF
            ELSE
               CHAR = 'U'
            ENDIF
            IF( STR16.EQ.' ' )THEN
               CALL FI2C(STR16,STAT,F)
               CLIST = STR16(F:8)
               IF( CHAR.EQ.'S' )THEN
                  STR16 = 'SPIKE'
               ELSE IF( CHAR.EQ.'F' )THEN
                  STR16 = 'FRONT'
                  IF( FRONT.EQ.REAR )STR16(6:) = '=R'
               ELSE IF( CHAR.EQ.'R' )THEN
                  STR16 = 'REAR'
               ELSE
                  STR16 = 'UNASSIGN'
               ENDIF
            ELSE
               CLIST = NAME(IRNAME+STAT)
            ENDIF
            CLIST(NAMELN+2:) = NAME(ICNAME+COL)
            IF( LINE.GE.20 )THEN
               LINE = 0
               READ(*,1)CHAR
               IF(CHAR.NE.' ')GOTO 100
            ENDIF
            WRITE(*,291)I,CLIST,COL,STR16
291         FORMAT(I7,':',A35,':',I7,2X,A16)
            LINE = LINE+1
290      CONTINUE
         GOTO 100
C
300      CONTINUE
         IF( NROWS.GT.LCLMXR )THEN
            PRINT *,' ** LCLMXR =',LCLMXR,' < NROWS =',NROWS
            GOTO 5
         ENDIF
C
         DO 350 I=I1,I2
            IF( ARRAY.EQ.'BTRAN' )THEN
               CALL GALPHA('ROW ',I,ZLOCAL,LCLMXR)
               CALL GBTRAN(ZLOCAL,LCLMXR)
               STR16 = NAME(IRNAME+I)
            ELSE
               CALL GALPHA('COL ',I,ZLOCAL,LCLMXR)
               CALL GFTRAN(ZLOCAL,LCLMXR)
               STR16 = NAME(ICNAME+I)
            ENDIF
            CLIST = ':'
            DO 320 ROW=1,NROWS
               IF( ZLOCAL(ROW).EQ.0. )GOTO 320
               IF( LINE.GE.20 )THEN
                  LINE = 0
                  READ(*,1)CHAR
                  IF(CHAR.NE.' ')GOTO 100
               ENDIF
               CLIST(2:) = NAME(IRNAME+ROW)
               COL = INDEX(IPIVOT+ROW)
               IF( COL.GT.0 )CLIST(15:) = NAME(ICNAME+COL)
               WRITE(*,301)STR16,CLIST,ZLOCAL(ROW)
301            FORMAT(1X,  A16,  A40, D19.9)
               STR16 = ' '
               LINE  = LINE+1
320         CONTINUE
350      CONTINUE
         GOTO 100
C
400      CONTINUE
         IF( BUCKET.EQ.0.OR.IRBLST.GT.0 )THEN
            PRINT *,' NO BUCKET'
            GOTO 5
         ENDIF
         WRITE(*,'(1X,7HBUCKET=,I7,2X,7HBLSUCC=,I7,2X,7HBLPRED=,I7)')
     1          BUCKET,BLSUCC,BLPRED
         WRITE(*,'(9X,3HROW,T26,4HSUCC,T34,4HPRED)')
         LINE = 3
         DO 450 I=I1,I2
            ROW = INDEX(BUCKET+I)
            IF( ROW.EQ.0 )THEN
               WRITE(*,'(I7,7H: EMPTY)') I
               LINE = LINE+1
               GOTO 450
            ENDIF
410         CONTINUE
              IF( LINE.GE.21 )THEN
                 READ(*,'(A1)')CHAR
                 IF( CHAR.NE.' ' )GOTO 100
                 LINE = 0
              ENDIF
              STR16 = NAME ( IRNAME+ROW )
              PRED = INDEX( BLPRED+ROW )
              SUCC = INDEX( BLSUCC+ROW )
              WRITE(*,'( I7,1H:,A16,2(1X,I7) )' )I,STR16,SUCC,PRED
              LINE = LINE+1
              IF( SUCC.EQ.0 )GOTO 450
              ROW = SUCC
            GOTO 410
450      CONTINUE
         GOTO 100
C
500      CONTINUE
         IF( IRBLST.EQ.0 )THEN
            PRINT *,' SPIKES BEGIN AT REAR+1 =',REAR+1,' AND END AT',
     1              ' NBPOSN-NBBTRI =',NBPOSN-NBBTRI
            GOTO 5
         ENDIF
C SPIKES ARE LOCATED FROM REAR TO
550      CONTINUE
         PRINT *,' SPIKES AT REAR =',REAR,' TO FRONT+1 =',FRONT+1
         I1 = REAR - I1+1
         I2 = REAR - I2+1
         IF( I1.GT.REAR   )I1 = REAR
         IF( I1.LE.FRONT  )I1 = FRONT+1
         IF( I2.LE.FRONT  )I2 = FRONT+1
         IF( I2.GT.I1     )I2 = I1
         DO 590 I=I1,I2,-1
            COL = INDEX( BCPOSN+I )
            PRINT *,I,':',COL,':',NAME(ICNAME+COL),INDEX(BCSTAT+I)
590      CONTINUE
         GOTO 100
C
1000  CONTINUE
      IF( CHAR.EQ.'W' )THEN
         ROWCOL = 'ROW '
      ELSE
         ROWCOL = 'COL '
      ENDIF
1100  CONTINUE
        PRINT *,' ',ROWCOL,'Name: '
        READ(*,'(A16)')STR16
        IF( STR16.EQ.' ' )GOTO 5
        CALL GETNUM(ROWCOL,STR16,NUMBER)
        PRINT *,' ',ROWCOL,STR16,NUMBER
        IF( NUMBER.EQ.0 )GOTO 1100
        CALL GETRIM(ROWCOL,NUMBER,VL,VU,VC,NZ)
        PRINT *,' VL,VU,VC =', VL,VU,VC
        CALL GETSOL(ROWCOL,NUMBER,VX,VP,CHAR,STNUM)
        PRINT *,NZ,'=NZ, STAT=',CHAR,' VX,VP =', VX,VP
      GOTO 1100
C
C ** GDEBUG ENDS HERE
      END
      SUBROUTINE GPRBAS(I1,I2)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This is called only by GDEBUG to obtain values in BASE
C  I1 <= 0 ===> GIVE DICTIONARY OF SECTIONS
C  I1  > 0 ===> GIVE BASE VALUES DEFINED IN I1-I2
C
C LOCAL
      CHARACTER*1  CHAR
      CHARACTER*6  BASNAM(99)
      DATA         BASNAM/99*' '/
C ::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF( I1.LE.0 )THEN
         PRINT *,' DCGETMAT BASE VALUES'
         PRINT *,'  1- 9 SPECIAL MARKERS'
         PRINT *,' 10-29 STATS'
         PRINT *,' 30-49 POINTERS FOR MATRIX & SOLUTION'
         PRINT *,' 50-70 BASIS INFO'
         PRINT *,' 71-79 EDIT VALUES'
         PRINT *,' 80-89 FILE UNITS'
         PRINT *,' 90-99 DIMENSION LIMITS'
         RETURN
      ENDIF
CC 7-29-95 TAKEN FROM GETDATA.DOC
      BASNAM( 1) = 'IFREE '
      BASNAM( 2) = 'ENDIND'
      BASNAM( 3) = 'IVFREE'
      BASNAM( 4) = 'ENDBAS'
      BASNAM( 5) = 'ENDNAM'
      BASNAM( 6) = 'INF   '
C
      BASNAM(10) = 'NROWS '
      BASNAM(11) = 'NCOLS '
      BASNAM(12) = 'NONZER'
      BASNAM(13) = 'NVALS '
      BASNAM(14) = 'NONES '
      BASNAM(15) = 'NRFREE'
      BASNAM(16) = 'NCFIX '
      BASNAM(17) = 'NAMELN'
      BASNAM(18) = 'OBJNUM'
      BASNAM(19) = 'RMAXLN'
      BASNAM(20) = 'CMAXLN'
      BASNAM(21) = 'OPT   '
      BASNAM(22) = 'OPTMIN'
      BASNAM(23) = 'OPTMAX'
      BASNAM(24) = 'STCOMP'
      BASNAM(25) = 'STBASC'
C
      BASNAM(30) = 'IRNAME'
      BASNAM(31) = 'ICNAME'
      BASNAM(32) = 'IRSOLV'
      BASNAM(33) = 'ICSOLV'
      BASNAM(34) = 'IRLBND'
      BASNAM(35) = 'ICLBND'
      BASNAM(36) = 'IRUBND'
      BASNAM(37) = 'ICUBND'
      BASNAM(38) = 'IRSTAT'
      BASNAM(39) = 'ICSTAT'
      BASNAM(40) = 'IRINFO'
      BASNAM(41) = 'ICINFO'
      BASNAM(42) = 'INONZ '
C
      BASNAM(50) = 'IPIVOT'
      BASNAM(51) = 'IRBLST'
      BASNAM(52) = 'IVBLST'
      BASNAM(53) = 'NBLST '
      BASNAM(54) = 'NBASNZ'
      BASNAM(55) = 'NBFILL'
      BASNAM(56) = 'NBFTRI'
      BASNAM(57) = 'NBBTRI'
      BASNAM(58) = 'NBSPIK'
      BASNAM(59) = 'NBLGL '
      BASNAM(60) = 'NBKRNL'
      BASNAM(61) = 'NBPOSN'
      BASNAM(62) = 'BCPOSN'
      BASNAM(63) = 'BCSTAT'
      BASNAM(64) = 'FRONT '
      BASNAM(65) = 'REAR  '
      BASNAM(66) = 'BUCKET'
      BASNAM(67) = 'BUCKLO'
      BASNAM(68) = 'BLINK '
      BASNAM(69) = 'NBUCKT'
      BASNAM(70) = 'IZBLST'
C
      BASNAM(80) = 'DATFIL'
      BASNAM(81) = 'PCKFIL'
C
      BASNAM(90) = 'EBASE2'
      BASNAM(91) = 'MAXNAM'
      BASNAM(92) = 'MAXIND'
      BASNAM(93) = 'MAXVAL'
      BASNAM(94) = 'MAXRC '
      BASNAM(95) = 'MAXVPL'
C
      LINE = 0
      DO 900 I=I1,I2,4
         N = I+3
         IF( N.GT.I2 )N=I2
         LINE = LINE+1
         IF( LINE.GT.21 )THEN
            PRINT *,' Continue (Y/N)? '
            READ(*,'(A1)')CHAR
            IF( CHAR.EQ.'N' )RETURN
            LINE = 0
         ENDIF
         WRITE(*,'( 4(1X,I2,1H:,A6,1H=,I7) )')
     1                 (K,BASNAM(K),BASE(K),K=I,N)
900   CONTINUE
      RETURN
C
C ** GPRBAS ENDS HERE
      END
