C             ::: GUPCKGEN.FOR  8-20-95 :::
C
C Earlier dates deleted.
C       5-15-94...Added REDEBG (7-13-95)
C       7-13-95...Added SWREPL to calling args of GBREFR
C       8-05-95...Removed CALL GBREFR (Will refresh in ANALYZE)
C       8-17-95...Put 1st part of GUPCKW to GUPCKB (created) to
C                 have all setup done (incl. basis) before user calls
C                 GUPCKW to write the packed file
C
C THIS IS A STAND-ALONE UTILITY TO CREATE A PACKED FILE FROM LP IN CORE.
C (BYPASSES NEED TO CREATE MATRIX + SOLUTION FILES FOR ANALYZE.)
C LINK WITH GUPCKLIB.
C
C     GUPCK0....Initialize memory...MUST BE CALLED FIRST
C     GUPCKR....Enter rows
C     GUPCKC....Enter cols
C     GUPCKB....Finish setup, incl basis
C     GUPCKW....Write packed file...MUST BE CALLED LAST
C     GUCHKS....Check solution for row or col
C     GUPCKV....Enter nonzero into pool
C     GUPCKD....Print routine (used for debugging)
C     GUCHK.....Check input to GUPCK0
C     REDEBG....Dummy routine
C
C IN EACH CASE, THERE IS AN INTEGER-VALUED RETURN CODE (RCODE).
C RCODE = 0 IFF ALL IS WELL;  ELSE, ERROR IS FATAL.
C
      SUBROUTINE GUPCK0(PRNAME,OBNAME,RHNAME,RGNAME,BDNAME,OPNAME,
     1                 SVNAME,STNAME, LENGTH,ROWS,COLS,NZ,
     2                 VINFTY,VTOLA,VTOLR,FILE,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This is the initial call to the GUPCKx routines, which bypass the need
C for reading matrix and solution file.  It is also more efficient than
C reading an LP file with LPRENAME.
C
      CHARACTER*(*) PRNAME,OBNAME,RHNAME,RGNAME,BDNAME,SVNAME,
     1              OPNAME,STNAME
      INTEGER       ROWS,COLS,NZ,RCODE
      REAL          VINFTY,VTOLA,VTOLR
C Names (stored as 16-char, and is case-sensitive):
C     PRNAME = problem name (required)
C     OBNAME = objective row name (required)
C     RHNAME = right-hand side name (required)
C     RGNAME = range name ('none' if none)
C     BDNAME = bound name ('none' if none)
C     SVNAME = solver name (eg, OML)
C     STNAME = solution status:
C              OPTIMAL | FEASIBLE | INFEASIBLE | UNBOUNDED | unknown
C     OPNAME = sense of opt:  MINIMIZE | MAXIMIZE
C Numbers:
C     LENGTH = max name length of rows and cols (<= 16, and usually 8)
C     ROWS   = number of rows (> 0)
C     COLS   = number of columns (> 0)
C     NZ     = number of nonzeroes (>= 0)
C            = 0 MEANS DON'T KNOW (GUPCKC WILL COUNT)
C     VINFTY = infinity (usually 1.0E+20)
C     VTOLA  = absolute tolerance
C     VTOLR  = relative tolerance
C     FILE   = output file unit
C              CALLER IS RESPONSIBLE FOR OPENING BEFORE CALLING GUPCKW
C
      COMMON/GUNUM/LLINK,RLINK,TOP,BOTTOM,FILOUT,NZREMN
      CHARACTER*16 FNAME,LNAME,RCNAME
      COMMON/GUCHR/FNAME,LNAME,RCNAME
C
      CHARACTER*4  VERSN
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
C CHECK INPUT
      CALL GUCHK('PRNAME',PRNAME,*1300)
      CALL GUCHK('OBNAME',OBNAME,*1300)
      CALL GUCHK('RHNAME',RHNAME,*1300)
      CALL GUCHK('RGNAME',RGNAME,*1300)
      CALL GUCHK('BDNAME',BDNAME,*1300)
      CALL GUCHK('OPNAME',OPNAME,*1300)
      CALL GUCHK('SVNAME',SVNAME,*1300)
      CALL GUCHK('STNAME',STNAME,*1300)
      IF( LENGTH.LT.1 .OR. LENGTH.GT.16 )THEN
         PRINT *,' ** LENGTH=',LENGTH,'...MUST BE 1 TO 16'
         GOTO 1300
      ENDIF
      IF( ROWS.LE.0 .OR. COLS.LE.0 )THEN
         PRINT *,' ** ROWS,COLS=',ROWS,COLS,'...MUST BE > 0'
         GOTO 1300
      ENDIF
      IF( NZ.LT.0 )THEN
         PRINT *,' ** NONZEROES=',NZ,'...MUST BE >= 0'
         GOTO 1300
      ENDIF
      IF( ROWS+COLS .GT. PMXNAM/LENGTH )THEN
         PRINT *,' ** TOO MANY ROWS+COLS...ROWS=',ROWS,' COLS=',COLS
         PRINT *,' ...MUST BE <',PMXNAM/LENGTH,' WITH LENGTH=',LENGTH
         GOTO 1300
      ENDIF
      IF( FILE.LE.0 )THEN
         PRINT *,' ** FILE=',FILE,'...MUST BE > 0'
         GOTO 1300
      ENDIF
      IF( VINFTY.LT.0. .OR. VTOLA.LT.0. .OR. VTOLR.LT.0. )THEN
         PRINT *,' ** VINFTY,VTOLA,VTOLR=',VINFTY,VTOLA,VTOLR,
     1           '...MUST BE > 0'
         GOTO 1300
      ENDIF
      IF( VINFTY.LT.1.0E+10 .OR. VTOLA.GT.1. .OR. VTOLR.GT.1. )THEN
         PRINT *,' Warning: VINFTY,VTOLA,VTOLR=',VINFTY,VTOLA,VTOLR
      ENDIF
      RCODE = 0
C
      FILOUT = FILE
C GINITL WANTS DATA FILE AND PACKED FILE
      FILDAT = 10
      FILPCK = 11
C BUT DO NOT USE CALLER'S FILE (PCKFIL WILL BE USED AS SCRATCH)
      IF( FILDAT.EQ.FILE )FILDAT = 12
      IF( FILPCK.EQ.FILE )FILPCK = 12
      CALL GINITL(FILDAT,FILPCK,VERSN)
C NOW PCKFIL=FILPCK AND DATFIL=FILDAT IN DCGETMAT COMMON
C AND MEMORY HAS BEEN INITIALIZED (GINITL CALLED GNEWLP)
C CHECK MEMORY, USING MINIMAL SAFE DIMENSIONS
      NUMRC = ROWS + COLS
      IF( NUMRC.GT.MAXRC )THEN
         PRINT *,' ** TOO MANY ROWS + COLS:',
     1           ROWS,' +',COLS,' >',MAXRC
         RCODE = 1
         RETURN
      ENDIF
      IF( NZ.GT.0 )THEN
         NZL = NZ
      ELSE
C CALLER DOESN'T KNOW NZ...USE LOWER BOUND OF 2 PER COLUMN
         NZL = 2*COLS
      ENDIF
      MIN = 6*ROWS + 7*COLS + 2*NZL + 500
      IF( MIN.GE.PMXIND )THEN
         PRINT *,' ** NOT ENOUGH INDEX SPACE...NEED',MIN
         RCODE = 1
         RETURN
      ENDIF
      MIN = 7*ROWS + 6*COLS + 2*NZL + .01*ROWS*ROWS + 400
      IF( MIN.GE.PMXVAL )THEN
         PRINT *,' ** NOT ENOUGH VALUE SPACE...NEED',MIN
         RCODE = 1
         RETURN
      ENDIF
C OK, SET NAMES
      PRBNAM = PRNAME
      OBJNAM = OBNAME
      RHSNAM = RHNAME
      IF( RGNAME.EQ.' ' )THEN
         RNGNAM = 'none'
      ELSE
         RNGNAM = RGNAME
      ENDIF
      IF( BDNAME.EQ.' ' )THEN
         BNDNAM = 'none'
      ELSE
         BNDNAM = BDNAME
      ENDIF
      SOLNAM = SVNAME
      SOLST  = STNAME
      CALL GSETOP(OPNAME)
C SET NUMBERS
      NAMELN = LENGTH
      NROWS  = ROWS
      NCOLS  = COLS
      NONZER = NZ
      VINF   = VINFTY
      VALUE(INF) = VINF
      VTOLAB = VTOLA
      VTOLRE = VTOLR
C SET POINTERS
      IRNAME = 0
      IRINFO = IFREE
      IRLBND = IRINFO + NROWS
      IRUBND = IRLBND + NROWS
      IRSTAT = IRUBND + NROWS
C
      ICNAME = IRNAME + NROWS + 1
      ICINFO = IRSTAT + NROWS + 1
      INDEX(ICINFO) = 0
      ICLBND = ICINFO + NCOLS + 1
      ICUBND = ICLBND + NCOLS
      ICSTAT = ICUBND + NCOLS
      INONZ  = ICSTAT + NCOLS + 1
C CHECK SPACE
      MIN = INONZ + 2*NZL
      IF( MIN.GE.PMXIND )THEN
         PRINT *,' ** NOT ENOUGH INDEX SPACE...NEED AT LEAST',MIN
         RCODE = 1
         PRBNAM = ' '
         RETURN
      ENDIF
      IFREE = PMXIND - NCOLS - 1
      CALL GPTNAM(IRNAME,' ')
      CALL GPTNAM(ICNAME,' ')
      MAXVAL = PMXVAL - 4*NUMRC
C     :                 :...2 PER ROW AND COL USED DURING CONSTRUCTION
C     :                   + 2 FOR SOLUTION VALUES
C     :....THIS IS LIMIT FOR VALUE POOL
C THE FOLLOWING LINKS ARE JUST DURING CONSTRUCTION
C (PASSED IN COMMON/GUNUM/ FOR GUPCKR AND GUPCKC)
      LLINK  = MAXVAL
      RLINK  = LLINK  + NUMRC
      IRSOLV = RLINK  + NUMRC
      ICSOLV = IRSOLV + NROWS
C NOTE:  SOLUTION VALUES ARE AT BOTTOM OF VALUE ARRAY NOW, BUT THIS
C        WILL BE MOVED UP AFTER VALUE POOL IS FINISHED.
C
C             VALUE | I4VAL
C       .______________________.
C       |                      | <=== MAXVAL = LLINK
C       |----------------------|
C       | i-th Left link       | <=== LLINK + i \
C       |----------------------|                 | WILL BE FREE
C       | i-th Right link      | <=== RLINK + i /
C       |----------------------| <=== IRSOLV      \
C       | Row i solution value | <=== IRSOLV + i   \
C       |----------------------|                    | WILL MOVE UP
C       | Col j solution value | <=== ICSOLV + j   /
C       |______________________| <=== PMXVAL      /
C                                     = ICSOLV + NCOLS
C
C ========== WE MUST GET HERE IF NORMAL RETURN ==========
C INITIALIZE ASCII (NEEDED FOR FSCASE AND FC2*)
      CALL FSINIT
      RETURN
C
1300  CONTINUE
      PRINT *,' ********************************************'
      PRINT *,' * INPUT FAILURE IN GUPCK0...YOU MUST ABORT *'
      PRINT *,' ********************************************'
      RCODE = 13
      RETURN
C
C ** GUPCK0 ENDS HERE
      END
      SUBROUTINE GUPCKR(RNAME,NR,VLO,VUP,VX,VP,STAT,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This enters a row and its rim.
C
      CHARACTER*(*) RNAME
      INTEGER       NR
      REAL          VLO,VUP,VX,VP
      CHARACTER*(*) STAT
      INTEGER       RCODE
C   RNAME = Name of row
C   NR    = Row number | 0 to init | > NROWS to finish
C The following are relevant only if 1 <= NR <= NROWS
C   VLO = Lower bound of row
C   VUP = Upper bound of row
C   VX  = Level of row in solution (= Ax, not the slack or surplus)
C   VP  = Dual price of row in solution
C         ...CHECK SIGN FOR THEORETICALLY CORRECT VALUE
C   STAT  = Solution status: B | I | L | U
C FIRST, CALL GPCKR WITH NR = 0...THIS WILL SETUP MEMORY STUFF.
C LAST,  CALL GPCKR WITH NR > NROWS...THIS WILL WRAP UP ROWS SECTION.
C
      COMMON/GUNUM/LLINK,RLINK,TOP,BOTTOM,FILOUT,NZREMN
      CHARACTER*16 FNAME,LNAME,RCNAME
      COMMON/GUCHR/FNAME,LNAME,RCNAME
      CHARACTER*1  CHAR, RC0
      COMMON/GUTERM/RC0
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      RCODE = 0
C SEE GETMAT ROWS SECTION FOR COMMENTS
      IF( NR.EQ.0 )THEN
C INITALIZE
         I4VAL(LLINK) = 0
         I4VAL(RLINK) = 0
         FNAME = '~'
         LNAME = ' '
         TOP   = 0
         BOTTOM= 0
C  REMEMBER RNAME FOR LAST CALL
         RC0 = RNAME
         IF( RC0.NE.' ' )THEN
            PRINT *,' ROW INITIALIZATION'
            CALL GUPCKD(RC0,*1399)
         ENDIF
         RETURN
      ENDIF
      IF( NR.GT.NROWS )GOTO 1000
C COPY NAME TO BE SURE WE HAVE IT IN 16 CHAR STRING
      RCNAME = RNAME
      CALL GUCHKS('ROW ',RCNAME,VLO,VUP,VX,VP,STAT,RCODE)
      IF( RCODE.NE.0 )GOTO 1399
C
C ========== PUT ROW ===========
C LOCATE ROW IN TREE (AND TEST FOR DUPLICATE)
C IS NEW NAME LAST?
      IF( RCNAME.GT.LNAME )THEN
C YES, INSERT TO RIGHT OF BOTTOM (AND UPDATE LNAME)
         LNAME = RCNAME
         I4VAL(RLINK + NR)     = I4VAL(RLINK + BOTTOM)
         I4VAL(RLINK + BOTTOM) = NR
         BOTTOM = NR
         IF( NR.EQ.1 )THEN
            FNAME = RCNAME
            TOP   = 1
         ENDIF
         GOTO 180
      ENDIF
C IS NEW NAME FIRST?
      IF( RCNAME.LT.FNAME )THEN
C YES, INSERT TO LEFT OF TOP (AND UPDATE FNAME)
         FNAME = RCNAME
         I4VAL(LLINK + TOP) = NR
         I4VAL(RLINK + NR)  = -TOP
         TOP   = NR
         IF( NROWS.EQ.1 )THEN
            LNAME = RCNAME
            BOTTOM= 1
         ENDIF
         GOTO 180
      ENDIF
C NOT FIRST OR LAST
      PARENT = 0
C LOOP TO LOCATE AND INSERT
150   CONTINUE
      IF( RCNAME.LT.NAME(IRNAME + PARENT) )THEN
C LOOK LEFT
          LOC = I4VAL(LLINK + PARENT)
          IF( LOC.GT.0 )THEN
C ...MOVE LEFT
             PARENT = LOC
             GOTO 150
          ELSE
C ...INSERT LEFT
             I4VAL(LLINK + PARENT) = NR
             I4VAL(RLINK + NR)     = -PARENT
          ENDIF
      ELSE IF( RCNAME.GT.NAME(IRNAME + PARENT) )THEN
C LOOK RIGHT
          LOC = I4VAL(RLINK + PARENT)
          IF( LOC.GT.0 )THEN
C ...MOVE RIGHT
             PARENT = LOC
             GOTO 150
          ELSE
C ...INSERT RIGHT
             I4VAL(RLINK + NR)     = I4VAL(RLINK + PARENT)
             I4VAL(RLINK + PARENT) = NR
          ENDIF
      ELSE
          PRINT *,' ** DUPLICATE ROW: ',RCNAME
          RCODE = 1
          RETURN
      ENDIF
C
C FINISH PUTTING ROW IN TREE
180   CONTINUE
      CALL GUPCKV(LO,VLO,*1310)
      CALL GUPCKV(UP,VUP,*1310)
      INDEX(IRLBND+NR) = LO
      INDEX(IRUBND+NR) = UP
C COPY STAT TO BE SURE IT'S IN 1 CHAR STRING
      CHAR = STAT
      CALL GPUTSL(IRSTAT+NR,IRSOLV+NR,CHAR,VX,VP,RCODE)
      IF( RCODE.NE.0 )GOTO 1399
      INDEX(IRINFO+NR) = 0
      CALL GPTNAM(IRNAME+NR,RCNAME)
      I4VAL(LLINK + NR) = 0
C TALLY
      IF( LO.EQ.-INF .AND. UP.EQ.INF )NRFREE = NRFREE + 1
      RETURN
C
1000  CONTINUE
C ======= WRAP UP =======
      IF( STAT.NE.'E' .AND. STAT.NE.'e' .AND. STAT.NE.' ' )THEN
         PRINT *,' AT BEGINNING OF GUPCKR TERMINATION'
         CALL GUPCKD(RC0,*1399)
      ENDIF
C
C  WRITE SORTED ROW LIST TO PCKFIL
C  --------------------------------
      CLOSE(PCKFIL)
C UNIX AND XENIX NEED RECORD LENGTHS (RECL) FOR UNFORMATTED FILES
CIXNX      OPEN(PCKFIL,STATUS='SCRATCH',FORM='UNFORMATTED',ERR=1350,
CIXNX     1     RECL=2048)
CIXNX      GOTO 222
CIEIAC   FILEINF NEEDED FOR SCRATCH FILE TO PROVIDE ENOUGH SPACE
CIEIA      CALL FILEINF(RCODE,'TRK',50,'SECOND',100,'RECFM','VBS',
CIEIA     1             'LRECL',-1,'BLKSIZE',32760)
      OPEN(PCKFIL,STATUS='SCRATCH',FORM='UNFORMATTED',ERR=1350)
222   REWIND(PCKFIL)
C
C     TRAVERSE ROWS INORDER
C
      ROW = I4VAL(LLINK)
      IF( ROW.EQ.0 )GOTO 1950
C   MOVE DOWN LEFT LEG OF TREE
1910  CONTINUE
      IF( I4VAL(LLINK+ROW).GT.0 )THEN
          ROW = I4VAL(LLINK+ROW)
          GOTO 1910
      ENDIF
C     WRITE ROW
C     =========
1920  CONTINUE
      RCNAME = NAME(IRNAME+ROW)
      WRITE(PCKFIL,ERR=1356)RCNAME,VALUE(IRSOLV+ROW),
     1      INDEX(IRSTAT+ROW),INDEX(IRLBND+ROW),INDEX(IRUBND+ROW)
C
C GET INORDER SUCCESSOR OF ROW
1950  CONTINUE
      IF( I4VAL(RLINK+ROW).LT.0 )THEN
          ROW = -I4VAL(RLINK+ROW)
          GOTO 1920
      ELSE IF( I4VAL(RLINK+ROW).GT.0 )THEN
          ROW = I4VAL(RLINK+ROW)
          GOTO 1910
      ENDIF
C NOW WE HAVE WRITTEN LAST ROW...RIGHT LINK = 0
C SET POINTERS
C WRITE A DUMMY RECORD TO AVOID EOF
      WRITE(PCKFIL,ERR=1356)OBJNAM
C
C READ ROWS BACK (SORTED)
      REWIND(PCKFIL,ERR=1357)
      OBJNUM = 0
C
      DO 1989 ROW=1,NROWS
         READ(PCKFIL,ERR=1355,END=1359)RCNAME,VALUE(IRSOLV+ROW),
     1      INDEX(IRSTAT+ROW),INDEX(IRLBND+ROW),INDEX(IRUBND+ROW)
         CALL GPTNAM(IRNAME+ROW,RCNAME)
         IF( RCNAME.EQ.OBJNAM )OBJNUM = ROW
1989  CONTINUE
      IF( OBJNUM.EQ.0 )THEN
         PRINT *,' ** OBJ ROW ',OBJNAM(:NAMELN),' NOT FOUND'
         GOTO 1390
      ENDIF
C
C NOW ROWS ARE IN NAME SORT ORDER WITH THEIR BOUNDS AND SOLUTION
C ...ROW COUNTS (IRINFO) WILL BE UPDATED AS WE GET COLS (GUPCKC).
C
      IF( STAT.EQ.'E' .OR. STAT.EQ.'e' .OR. STAT.EQ.'*' )THEN
         PRINT *,' AT END OF GUPCKR TERMINATION'
         CALL GUPCKD(RC0,*1399)
      ENDIF
      RETURN
C
C ERROR RETURNS
1310  PRINT *,' ** NOT ENOUGH VALUE SPACE'
      GOTO 1399
1350  PRINT *,' ** ERROR OPENING UNIT',PCKFIL,' AS SCRATCH FILE'
      GOTO 1399
1355  PRINT *,' ** ERROR READING SCRATCH FILE...UNIT',PCKFIL
      GOTO 1390
1356  PRINT *,' ** ERROR WRITING SCRATCH FILE...UNIT',PCKFIL
      GOTO 1390
1357  PRINT *,' ** ERROR REWINDING SCRATCH FILE...UNIT',PCKFIL
      GOTO 1390
1359  PRINT *,' ** PREMATURE END OF SCRATCH FILE...UNIT',PCKFIL
1390  CLOSE( PCKFIL )
1399  CONTINUE
      IF( RCODE.LE.0 )RCODE = 12
      PRINT *,' ...ABORTING GUPCKR.  ENTERING VALUES:'
      PRINT *,' RNAME=',RNAME,' NR =',NR
      PRINT *,'   VLO,VUP =',VLO,VUP
      PRINT *,'   STAT=',STAT,' VX,VP =',VX,VP
      RETURN
C
C ** GUPCKR ENDS HERE
      END
      SUBROUTINE GUPCKC(CNAME,NC,NZ,RNAME,VCOEF,VLO,VUP,
     1                  VX,VP,STAT,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This receives column (CNAME) and its info.
C
      CHARACTER*(*) CNAME
      INTEGER       NC,NZ
      CHARACTER*(*) RNAME(NZ)
      REAL          VCOEF(NZ)
      REAL          VLO,VUP,VX,VP
      CHARACTER*(*) STAT
      INTEGER       RCODE
C
C   CNAME = Name of column
C   NC    = Col number | 0 to init | > NCOLS to finish
C The following are relevant only if 1 <= NC <= NCOLS
C   NZ    = Number of nonzeroes in column (MUST BE > 0)
C   RNAME(i) = Name of i-th nonzero row
C   VCOEF(i) = Value of i-th nonzero coefficient
C   VLO   = Lower bound of column
C   VUP   = Upper bound of column
C   VX    = Level of column in solution
C   VP    = Dual price of column in solution
C   STAT  = Solution status: B | I | L | U
C FIRST, CALL GUPCKC WITH NC = 0...THIS WILL SETUP MEMORY STUFF.
C LAST,  CALL GUPCKC WITH NC > NCOLS...THIS WILL WRAP UP COLS.
C
      COMMON/GUNUM/LLINK,RLINK,TOP,BOTTOM,FILOUT,NZREMN
      CHARACTER*16 FNAME,LNAME,RCNAME,STR16
      COMMON/GUCHR/FNAME,LNAME,RCNAME
      CHARACTER*1  CHAR, RC0
      COMMON/GUTERM/RC0
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
C INONZ AND IFREE WERE SET IN GUPCK0:
C                  __________________________________
C   INONZ     --->| 2 per nonzero                    |
C                 |__________________________________|
C   IFREE     --->|                                  |
C   IFREE + j --->| pointer to nonzeroes of column j |
C                 |__________________________________|
C   IFREE + NCOLS = PMXIND-1
C
      RCODE = 0
C SEE GETMAT COLUMNS SECTION FOR COMMENTS
      IF( NC.EQ.0 )THEN
C INITIALIZE
         I4VAL(LLINK) = 0
         I4VAL(RLINK) = 0
         FNAME = '~'
         LNAME = ' '
         TOP   = 0
         BOTTOM= 0
         NZREMN= NONZER
C During COLUMNS:                   INDEX
C                         .-------------------------------------------.
C      INDEX(IFREE+j)===> | pointer to first nonzero pair of column j |
C and
C      INDEX(ICINFO+j) = Number of nonzeroes in column j
C
C Thus, we initialize:
         INDEX(IFREE) = INONZ
         INDEX(ICINFO)= 0
C  REMEMBER CNAME AND STAT FOR LAST CALL
         RC0 = CNAME
         IF( RC0.NE.' ' )THEN
            PRINT *,' COLUMN INITIALIZATION'
            CALL GUPCKD(RC0,*1399)
         ENDIF
         RETURN
      ENDIF
      IF( NC.GT.NCOLS )GOTO 1000
C COPY NAME TO BE SURE WE HAVE IT IN 16 CHAR STRING
      RCNAME = CNAME
      CALL GUCHKS('COL ',RCNAME,VLO,VUP,VX,VP,STAT,RCODE)
      IF( RCODE.NE.0 )GOTO 1399
C
      IF( NZ.LE.0 )GOTO 1330
      IF( NZREMN-NZ.LT.0 .AND. NONZER.GT.0 )GOTO 1331
      NZREMN = NZREMN - NZ
C
C LOCATE COLUMN IN TREE (AND TEST FOR DUPLICATE)
C IS NEW NAME LAST?
      IF( RCNAME.GT.LNAME )THEN
C YES, INSERT TO RIGHT OF BOTTOM (AND UPDATE LNAME)
         LNAME = RCNAME
         I4VAL(RLINK + NC) = I4VAL(RLINK + BOTTOM)
         I4VAL(RLINK + BOTTOM) = NC
         BOTTOM = NC
         IF( NC.EQ.1 )THEN
            FNAME = RCNAME
            TOP   = 1
         ENDIF
         GOTO 280
      ENDIF
C IS NEW NAME FIRST?
      IF( RCNAME.LT.FNAME )THEN
C YES, INSERT TO LEFT OF TOP (AND UPDATE FNAME)
         FNAME = RCNAME
         I4VAL(LLINK + TOP) = NC
         I4VAL(RLINK + NC)  = -TOP
         TOP   = NC
         IF( NC.EQ.1 )THEN
            LNAME = RCNAME
            BOTTOM= 1
         ENDIF
         GOTO 280
      ENDIF
C NEITHER FIRST NOR LAST...SEARCH
      PARENT = 0
250   CONTINUE
        IF( RCNAME.LT.NAME(ICNAME+PARENT) )THEN
C LOOK LEFT
           IF( I4VAL(LLINK+PARENT).GT.0 )THEN
C MOVE LEFT
              PARENT = I4VAL(LLINK+PARENT)
              GOTO 250
           ELSE
C INSERT LEFT
              I4VAL(LLINK+PARENT)= NC
              I4VAL(RLINK+NC)    = -PARENT
           ENDIF
        ELSE
           IF( I4VAL(RLINK+PARENT).GT.0 )THEN
C MOVE RIGHT
              PARENT = I4VAL(RLINK+PARENT)
              GOTO 250
           ELSE
C INSERT RIGHT
              I4VAL(RLINK+NC)    = I4VAL(RLINK+PARENT)
              I4VAL(RLINK+PARENT)= NC
           ENDIF
        ENDIF
280   CONTINUE
C FINISH PUTTING COL IN TREE
      CALL GUPCKV(LO,VLO,*1310)
      CALL GUPCKV(UP,VUP,*1310)
      INDEX(ICLBND+NC) = LO
      INDEX(ICUBND+NC) = UP
C COPY STAT TO BE SURE IT'S IN 1 CHAR STRING
      CHAR = STAT
      CALL GPUTSL(ICSTAT+NC,ICSOLV+NC,CHAR,VX,VP,RCODE)
      IF( RCODE.NE.0 )GOTO 1399
      LOCNZ = INDEX(IFREE+NC-1) + 2*INDEX(ICINFO+NC-1)
      IF( INONZ+LOCNZ.GE.IFREE )GOTO 1340
      INDEX(IFREE +NC) = LOCNZ
      INDEX(ICINFO+NC) = NZ
C PUT NONZEROES
      DO 300 I=1,NZ
         STR16 = RNAME(I)
         CALL GETNUM('ROW ',STR16,ROW)
         IF( ROW.EQ.0 )GOTO 1320
         CALL GUPCKV(ID,VCOEF(I),*1310)
         INDEX(LOCNZ  ) = ROW
         INDEX(LOCNZ+1) = ID
         LOCNZ = LOCNZ + 2
         IF( ID.EQ.1 .OR. ID.EQ.-1 )NONES = NONES+1
         INDEX(IRINFO+ROW) = INDEX(IRINFO+ROW) + 1
300   CONTINUE
C
      CALL GPTNAM(ICNAME+NC,RCNAME)
      I4VAL(LLINK +NC) = 0
C TALLY
      IF( LO.EQ.UP )NCFIX = NCFIX + 1
      RETURN
C
1000  CONTINUE
C ============ WRAP UP ============
      IF( NONZER.GT.0 )THEN
C CALLER HAD PASSED NUMBER OF NONZEROES...CHECK
         IF( NZREMN.NE.0 )GOTO 1335
      ELSE
C CALLER DIDN'T KNOW (PASSED NZ=0 IN GUPCK0)...SET NUMBER
         NONZER = -NZREMN
      ENDIF
      IF( STAT.NE.'E' .AND. STAT.NE.'e' .AND. STAT.NE.' ' )THEN
         PRINT *,' AT BEGINNING OF GUPCKC TERMINATION'
         CALL GUPCKD(RC0,*1399)
      ENDIF
      CLOSE( PCKFIL )
C UNIX AND XENIX NEED RECORD LENGTHS (RECL) FOR UNFORMATTED FILES
CIXNX      OPEN(PCKFIL,STATUS='SCRATCH',FORM='UNFORMATTED',ERR=1350,
CIXNX     1     RECL=2048)
CIXNX      GOTO 222
CIEIAC   FILEINF NEEDED FOR SCRATCH FILE TO PROVIDE ENOUGH SPACE
CIEIA      CALL FILEINF(RCODE,'TRK',50,'SECOND',100,'RECFM','VBS',
CIEIA     1             'LRECL',-1,'BLKSIZE',32760)
C TRAVERSE COLUMN TREE INORDER (VISIT = WRITE TO PCKFIL)
      OPEN(PCKFIL,STATUS='SCRATCH',FORM='UNFORMATTED',ERR=1350)
222   REWIND( PCKFIL )
      COL = I4VAL(LLINK)
      IF( COL.EQ.0 )GOTO 2950
C MOVE DOWN LEFT LEG
2910  CONTINUE
      IF( I4VAL(LLINK+COL).GT.0 )THEN
         COL = I4VAL(LLINK+COL)
         GOTO 2910
      ENDIF
C === VISIT COLUMN (WRITE TO PCKFIL) ===
2920  CONTINUE
      NZCOL = INDEX(ICINFO+COL)
      WRITE(PCKFIL,ERR=1356)NAME(ICNAME+COL),VALUE(ICSOLV+COL),NZCOL,
     1        INDEX(ICSTAT+COL),INDEX(ICLBND+COL),INDEX(ICUBND+COL)
C ...WRITE NONZEROES
      I1 = INDEX(IFREE +COL)
      I2 = I1 + 2*NZCOL - 1
      WRITE(PCKFIL,ERR=1356)(INDEX(I),I=I1,I2)
C GET INORDER SUCCESSOR OF COLUMN
2950  IF( I4VAL(RLINK+COL).LT.0 )THEN
C MOVE TO RIGHT THREAD, AND VISIT
         COL = -I4VAL(RLINK+COL)
         GOTO 2920
      ELSE IF( I4VAL(RLINK+COL).GT.0 )THEN
C MOVE TO RIGHT, THEN DOWN LEFT LEG
         COL = I4VAL(RLINK+COL)
         GOTO 2910
      ENDIF
C ::: TRAVERSAL COMPLETE ...RIGHT LINK = 0 :::
C
C WRITE A DUMMY RECORD TO AVOID EOF
      WRITE(PCKFIL,ERR=1356)NZCOL
C
C READ BACK SORTED COLUMN LIST AND SET INDEX POINTERS INTO INFO
      REWIND(PCKFIL,ERR=1357)
      I1 = INONZ
C
      DO 2980 COL=1,NCOLS
         READ(PCKFIL,ERR=1355,END=1359)RCNAME,VALUE(ICSOLV+COL),NZCOL,
     1        INDEX(ICSTAT+COL),INDEX(ICLBND+COL),INDEX(ICUBND+COL)
         I2 = I1 + 2*NZCOL - 1
         READ(PCKFIL,ERR=1355,END=1359)(INDEX(I),I=I1,I2)
         I1 = I2 + 1
         CALL GPTNAM(ICNAME+COL,RCNAME)
         INDEX(ICINFO+COL) = INDEX(ICINFO+COL-1) + NZCOL
2980  CONTINUE
C
      IF( STAT.EQ.'E' .OR. STAT.EQ.'e' .OR. STAT.EQ.'*' )THEN
         PRINT *,' AT END OF GUPCKC TERMINATION'
         CALL GUPCKD(RC0,*1399)
      ENDIF
C
C FINALLY, WRAP UP DATA STRUCTURE AND SETUP BASIS
      CALL GUPCKB(RCODE)
      RETURN
C
C ERROR RETURNS
1310  PRINT *,' ** NOT ENOUGH VALUE SPACE'
      GOTO 1399
1320  PRINT *,' ** ROW ',RNAME(I)(:NAMELN),' NOT FOUND'
      GOTO 1399
1330  PRINT *,' ** COL ',CNAME(:NAMELN),' NONZEROES =',NZ
      PRINT *,' ...MUST BE POSITIVE'
      GOTO 1399
1331  PRINT *,' ** COL ',CNAME(:NAMELN),' NONZEROES =',NZ
      PRINT *,' THIS MAKES TOTAL NONZEROES > ',NONZER,
     1      ' (DECLARED IN GUPCK0)'
      GOTO 1399
1335  PRINT *,' ** NONZERO COUNT DISCREPENCY...REMAIN =',NZREMN
      PRINT *,NONZER,' DECLARED IN GUPCK0'
      GOTO 1399
1340  PRINT *,' ** OUT OF INDEX SPACE...NONZER,NZREMN=',NONZER,NZREMN
      GOTO 1399
1350  PRINT *,' ** ERROR OPENING UNIT',PCKFIL,' AS SCRATCH FILE'
      GOTO 1399
1355  PRINT *,' ** ERROR READING SCRATCH FILE...UNIT',PCKFIL
      GOTO 1390
1356  PRINT *,' ** ERROR WRITING SCRATCH FILE...UNIT',PCKFIL
      GOTO 1390
1357  PRINT *,' ** ERROR REWINDING SCRATCH FILE...UNIT',PCKFIL
      GOTO 1390
1359  PRINT *,' ** PREMATURE END OF SCRATCH FILE...UNIT',PCKFIL
      GOTO 1390
1390  CLOSE( PCKFIL )
1399  CONTINUE
      IF( RCODE.LE.0 )RCODE = 12
      PRINT *,' ...ABORTING GUPCKC.  ENTERING VALUES:'
      PRINT *,' CNAME=',CNAME,' NC,NZ =',NC,NZ
      PRINT *,'   VLO,VUP =',VLO,VUP
      PRINT *,'   STAT=',STAT,' VX,VP =',VX,VP
      IF( NZ.GT.0 )THEN
         PRINT *,'   RNAME               VCOEF'
         DO 13991 I=1,NZ
13991    PRINT *,'   ',RNAME(I),VCOEF(I)
      ENDIF
      RETURN
C
C ** GUPCKC ENDS HERE
      END
      SUBROUTINE GUPCKB(RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This ends packed file setup and sets up the basis.
C ...RCODE = -1 ===> COULD NOT SETUP BASIS (NONFATAL)
C          >  0 ===> FATAL ERROR
C
      COMMON/GUNUM/LLINK,RLINK,TOP,BOTTOM,FILOUT,NZREMN
C
C PASSED TO BASIS AGENDA:
C                              /~~~ MAX # ROWS FROM DCGETMAT
      DOUBLE PRECISION ZALPHA(PMXR)
C  MESSAGE SWITCH
      LOGICAL*1        SWMSG
                       SWMSG = .TRUE.
C  MESSAGE FREQUENCY
CLAHEY                 FREQMS = 100
                FREQMS = 500
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      RCODE = 0
C MOVE UP SOLUTION VALUES
      DO 100 I=1,NROWS+NCOLS
100   VALUE(NVALS+I) = VALUE(IRSOLV+I)
      IRSOLV = NVALS
      ICSOLV = IRSOLV + NROWS
      IVFREE = ICSOLV + NCOLS + 1
      MAXVAL = PMXVAL
      ENDNAM = (ICNAME + NCOLS)*NAMELN
      IFREE  = INONZ + 2*NONZER + 1
C GET ROW INFO
      RMAXLN = 0
      MAX    = 0
      DO 200 I=1,NROWS
         IF(   (INDEX(IRLBND+I).NE.-INF .OR. INDEX(IRUBND+I).NE.INF)
     1    .AND.(MAX.LT.INDEX(IRINFO+I)) )THEN
C NOT A FREE ROW...UPDATE MAX LENGTH
            MAX = INDEX(IRINFO+I)
            RMAXLN = I
         ENDIF
200   CONTINUE
      IF( RMAXLN.EQ.0 )THEN
         PRINT *,' ** SYSERR...RMAXLN = 0'
         GOTO 1390
      ENDIF
C GET COL INFO
      CMAXLN = 0
      MAX = 0
      DO 300 I=1,NCOLS
         NZ = INDEX(ICINFO+I) - INDEX(ICINFO+I-1)
         IF( MAX.LT.NZ )THEN
C UPDATE MAX LENGTH
            MAX = NZ
            CMAXLN = I
         ENDIF
300   CONTINUE
      IF( CMAXLN.EQ.0 )THEN
         PRINT *,' ** SYSERR...CMAXLN = 0'
         GOTO 1390
      ENDIF
C ============= END PACKED FILE SETUP IN CORE ==============
      CLOSE( PCKFIL )
C ================= NOW TRY TO SETUP BASIS =====================
      NZ = NROWS
      CALL BAGNDA(ZALPHA,NZ,SWMSG,FREQMS,RCODE)
      IF( RCODE.EQ.0 )THEN
         IF( SWMSG )PRINT *,' Basis is setup'
C CALLER IS RESPONSIBLE FOR REFRESHING BASIS (AS IN OSLANAL)
      ELSE
C BASIS NOT SETUP
         IF( SWMSG )PRINT *,' Warning: Basis could not be setup'
C ...LET CALLER KNOW
         RCODE = -1
      ENDIF
C
      RETURN
C
C ERROR RETURN
1390  CONTINUE
      IF( RCODE.EQ.0 )RCODE = 13
      CLOSE( FILOUT )
      RETURN
C
C ** GUPCKB ENDS HERE
      END
      SUBROUTINE GUPCKW(SWMSG,SWPCK,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This writes packed file to FILOUT, presumed open.
C
      COMMON/GUNUM/LLINK,RLINK,TOP,BOTTOM,FILOUT,NZREMN
C
      LOGICAL*1     SWMSG,SWPCK
C                       :     :...TRUE iff packed (unformatted)
C                       :         FALSE if formatted
C                       :...message switch
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF( SWPCK )THEN
C SAVE ORIGINAL PACKED FILE UNIT (PCKFIL)
         PCKSAV = PCKFIL
C SET PCKFIL TO USER'S FILE (OPENED BY CALLER)
         PCKFIL = FILOUT
C WRITE PACKED FILE
         CALL GWRPCK(RCODE)
C RESTORE ORIGINAL PACKED FILE UNIT
         PCKFIL = PCKSAV
         IF( RCODE.EQ.0 .AND. SWMSG )PRINT *,' Packed file written'
      ELSE
C SAVE ORIGINAL DATA FILE UNIT (DATFIL)
         DATSAV = DATFIL
C SET DATFIL TO USER'S FILE (OPENED BY CALLER)
         DATFIL = FILOUT
C WRITE FORMATTED PACKED FILE
         CALL GWRFPK(RCODE)
         DATFIL = DATSAV
         IF( RCODE.EQ.0 .AND. SWMSG )
     1      PRINT *,' Formatted packed file written'
      ENDIF
C
      RETURN
C
C ** GUPCKW ENDS HERE
      END
      SUBROUTINE GUCHKS(ROWCOL,RCNAME,VL,VU,VX,VP,STAT,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This checks solution values for ROWCOL RCNAME.
C
      CHARACTER*(*) ROWCOL
      CHARACTER*16  RCNAME
      REAL          VL,VU,VX,VP
      CHARACTER*1   STAT
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF( STAT.EQ.'L' )THEN
         IF( ABS(VX-VL).GT.VTOLAB )GOTO 1300
      ELSE IF( STAT.EQ.'U' )THEN
         IF( ABS(VX-VU).GT.VTOLAB )GOTO 1300
      ELSE IF( STAT.EQ.'B' )THEN
         IF( VX.GT.VU+VTOLAB .OR. VX.LT.VL-VTOLAB )GOTO 1300
      ELSE IF( STAT.EQ.'I' )THEN
         IF( VX.LE.VU+VTOLAB .AND. VX.GE.VL-VTOLAB )GOTO 1300
      ELSE
C CALLER WANTS THIS TO SET ITS STATUS
         IF( VX.GT.VU+VTOLAB .OR. VX.LT.VL-VTOLAB )THEN
            STAT = 'I'
         ELSE IF( VX.GT.VL .AND. VX.LT.VU )THEN
            STAT = 'B'
         ELSE IF( (VX.LE.VL) .AND.
     1            (SOLST.NE.'OPTIMAL' .OR. VP*OPT .LT. 0) )THEN
            STAT = 'L'
         ELSE IF( (VX.GE.VU) .AND.
     1            (SOLST.NE.'OPTIMAL' .OR. VP*OPT .GT. 0) )THEN
            STAT = 'U'
         ELSE IF( ABS(VP).LT.VTOLAB )THEN
            STAT = 'B'
         ELSE
            GOTO 1300
         ENDIF
      ENDIF
C
C CHECK THIS STAT AGAINST LP STATUS (SOLST)
      IF( SOLST.EQ.'INFEASIBLE'.OR. SOLST.EQ.'UNBOUNDED' )RETURN
C SOLST = OPTIMAL | FEASIBLE
      IF( STAT.EQ.'I' )GOTO 1300
C STAT = B | L | U
      IF( ROWCOL.EQ.'ROW ' .AND. RCNAME.EQ.OBJNAM )THEN
         IF( STAT.NE.'B' )GOTO 1300
      ELSE
C NOT OBJ ROW...CHECK PRICE
         IF( STAT.EQ.'B' .AND. ABS(VP).GT.VTOLAB )GOTO 1300
         IF( VL.GE.VU )THEN
C FIXED ROWCOL...ADJUST STAT ACCORDING TO PRICE (IF NECESSARY)
            IF( STAT.EQ.'L' .AND. VP*OPT.GT.0. )THEN
               PRINT *,' CHANGING STAT OF ',ROWCOL(:3),' ',
     1                 RCNAME(:NAMELN),' FROM L TO U'
               STAT = 'U'
            ELSE IF( STAT.EQ.'U' .AND. VP*OPT.LT.0. )THEN
               PRINT *,' CHANGING STAT OF ',ROWCOL(:3),' ',
     1                 RCNAME(:NAMELN),' FROM U TO L'
               STAT = 'L'
            ENDIF
         ELSE IF( SOLST.EQ.'OPTIMAL' )THEN
            IF( STAT.EQ.'L' .AND. VP*OPT.GT.0. )GOTO 1300
            IF( STAT.EQ.'U' .AND. VP*OPT.LT.0. )GOTO 1300
         ENDIF
      ENDIF
C
      RETURN
C
C ERROR RETURN
1300  CONTINUE
      PRINT *,' ** ERROR WITH ',ROWCOL(:3),' ',RCNAME(:NAMELN),
     1        ' SOLUTION...STAT=',STAT
      PRINT *,'     VL, VU =',VL,VU
      PRINT *,'     VX, VP =',VX,VP
      PRINT *,' LP SOLUTION STATUS = ',SOLST
      RCODE = 1
      RETURN
C
C ** GUCHKS ENDS HERE
      END
      SUBROUTINE GUPCKV(ID,V,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This gets value pool ID for V.
C ...Alternate return means out of space to add V to pool (fatal).
C
      REAL     V
      INTEGER  ID
C
C CALLS GETVID
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF( V.GT.VTOLAB )THEN
         VABS = V
         SIGN = 1
      ELSE IF( V.LT.-VTOLAB )THEN
         VABS = -V
         SIGN = -1
      ELSE
         ID = 0
         RETURN
      ENDIF
      IF( VABS.GE.VINF )THEN
         ID = SIGN*INF
         RETURN
      ENDIF
      IF( ABS(VABS-1.).LE.VTOLAB )THEN
         ID = SIGN
         RETURN
      ENDIF
      CALL GETVID(VABS,ID)
      IF( ID.EQ.0 )RETURN 1
      ID = SIGN*ID
      RETURN
C
C ** GUPCKV ENDS HERE
      END
      SUBROUTINE GUPCKD(WHAT,*)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCGETMAT.'
CITSO      INCLUDE (DCGETMAT)
CI$$INSERT DCGETMAT
C
C This prints base and/or memory values according to:
C       WHAT := B | b | M | m | *
C (See GUPCK.DOC)  Alternate return means user wants to stop.
C
      CHARACTER*(*) WHAT
C
      COMMON/GUNUM/LLINK,RLINK,TOP,BOTTOM,FILOUT,NZREMN
      CHARACTER*16 FNAME,LNAME,RCNAME
      COMMON/GUCHR/FNAME,LNAME,RCNAME
      CHARACTER*1 CHAR
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      CHAR = WHAT
      IF( CHAR.EQ.'B' .OR. CHAR.EQ.'b' .OR. CHAR.EQ.'*' )THEN
C PRINT BASE VALUES
         PRINT *,' PRBNAM=',PRBNAM,' OBJNAM=',OBJNAM,' NAMELN=',NAMELN
         PRINT *,' RHSNAM=',RHSNAM,' RNGNAM=',RNGNAM,' BNDNAM=',BNDNAM
         PRINT *,' SOLNAM=',SOLNAM,' SOLST =',SOLST ,' OPT   =',OPT
         PRINT *,' NROWS =',NROWS ,' NCOLS =',NCOLS ,' NONZER=',NONZER
         PRINT *,' NVALS =',NVALS ,' NONES =',NONES ,' MAXVAL=',MAXVAL
         PRINT *,' VINF  =',VINF  ,' VTOLAB=',VTOLAB,' VTOLRE=',VTOLRE
      ENDIF
C
      IF( CHAR.EQ.'M' .OR. CHAR.EQ.'m' .OR. CHAR.EQ.'*' )THEN
C PRINT MEMORY VALUES
         IF( CHAR.EQ.'*' )PRINT *,' '
         PRINT *,' IRNAME=',IRNAME,' IRINFO=',IRINFO,' IRSTAT=',IRSTAT
         PRINT *,' IRLBND=',IRLBND,' IRUBND=',IRUBND,' IRSOLV=',IRSOLV
         PRINT *,' ICNAME=',ICNAME,' ICINFO=',ICINFO,' ICSTAT=',ICSTAT
         PRINT *,' ICLBND=',ICLBND,' ICUBND=',ICUBND,' ICSOLV=',ICSOLV
         PRINT *,' INONZ =',INONZ ,' IFREE =',IFREE ,' IVFREE=',IVFREE
         PRINT *,' MAXVAL=',MAXVAL,' LLINK =',LLINK ,' RLINK =',RLINK
      ENDIF
900   CONTINUE
      PRINT *,' Enter nothing to continue (any non-blank to stop) '
      READ( *,'(A1)' )CHAR
      IF( CHAR.EQ.' ' )RETURN
      IF( CHAR.EQ.'D'.OR.CHAR.EQ.'d'.OR.CHAR.EQ.',' )THEN
         CALL GDEBUG
         GOTO 900
      ENDIF
      RETURN 1
C
C ** GUPCKD ENDS HERE
      END
      SUBROUTINE GUCHK(STRNAM,STR16,*)
C     ================
C
C This checks STRNAM = STR16 to make sure it is not blank and does not
C contain embedded delimiters.
C
       CHARACTER*(*) STRNAM
       CHARACTER*16  STR16
C
C LOCAL
       CHARACTER*32  STR32
       CHARACTER*1   DELIM(5)
       DATA DELIM/' ','=',',','(',')'/
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF( STR16.EQ.' ' )THEN
         PRINT *,' ** ',STRNAM,' IS BLANK'
         RETURN 1
      ENDIF
      CALL FSLEN(STR16,16,L)
      DO 10 I=1,L
         DO 5 K=1,5
           IF( STR16(I:I).EQ.DELIM(K) )GOTO 1300
5        CONTINUE
10    CONTINUE
C OK
      RETURN
C
1300  CONTINUE
      STR32 = '** '//STRNAM
      CALL FSLEN(STR32,32,L)
      STR32(L+1:) = '='//STR16
      PRINT *,' ',STR32
      STR32 = ' '
      STR32(L+1+I:) = ':'
      PRINT *,' ',STR32(:L+1+I),'...DELIMITER NOT ALLOWED'
      RETURN 1
C
C ** GUCHK ENDS HERE
      END
      SUBROUTINE REDEBG
C     =================
C This is dummy to replace the real one not in this collection.
      END
