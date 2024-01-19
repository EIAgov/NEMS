C              ::: AREADIN.FOR  8-22-95 :::
C
C Earlier dates deleted
C     2-12-95...Added NAMELN to CALL GETMAT in ARDMAT
C     7-17-95...Removed OB1 in ARDSOL (CC 7-20)
C     7-20-95...Relocated Edits info in ASUMRY
C     7-24-95...Changed edit output slightly in ASUMRY
C     7-29-95...Added obj value to ASUMRY (unless INFEAS)
C     8-11-95...Changed VTZERO to VTOL0C in ARDMAT
C
C This file contains the following subroutines for ANALYZE
C
C    AREAD....executes READIN (parses for type)
C    ANEWLP...initializes modules (called after reading a new LP)
C    ARDMAT...executes READIN MATRIX
C    ARDPCK...executes READIN PACKED
C    ARDSYN...executes READIN SYNTAX
C    ARDIIS...executes READIN IIS
C    ASUMRY...writes summary of LP in memory
C    AWRITE...parses to write packed file
C    ARDSOL...parses to read solution file
C
      SUBROUTINE AREAD(CLIST,FIRST,LAST,RCODE)
C     ================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This executes the ANALYZE command:
C
C  READIN {MATRIX | PACKED | SYNTAX | IIS} [filename [options]]
C
C The job of AREAD is to parse CLIST(FIRST:LAST) and pass to file
C  type-specific routines.
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*8   OPTION(4)
      CHARACTER*64  FILNAM
      CHARACTER*1   CHAR
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C GET TYPE (MPS)
C
      OPTION(1)='MATRIX'
      OPTION(2)='PACKED'
      OPTION(3)='SYNTAX'
      OPTION(4)='IIS'
      NUMBER = 4
      RCODE = -1
C    (SETTING RCODE NE 0 MAKES OPTION MANDATORY)
C
      CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
      IF(RCODE.NE.0)RETURN
C GET filename
      CALL FTOKEN(CLIST,FIRST,LAST,FILNAM,NAMLEN,CHAR)
      IF(FILNAM.EQ.' ')THEN
           IF(PRBNAM.NE.' '.AND.NUMBER.GE.3)THEN
                FILNAM = PRBNAM
           ELSE
                PRINT *,' ** MISSING FILENAME'
                RCODE = -1
                RETURN
           ENDIF
      ENDIF
C
C BRANCH ON OPTION NUMBER...1=MATRIX, 2=PACKED, 3=SYNTAX, 4=IIS
      IF(NUMBER.EQ.1)THEN
         CALL ARDMAT(FILNAM,CLIST,FIRST,LAST,RCODE)
      ELSE IF(NUMBER.EQ.2)THEN
         CALL ARDPCK(FILNAM,CLIST,FIRST,LAST,RCODE)
      ELSE IF(NUMBER.EQ.3)THEN
         CALL ARDSYN(FILNAM,RCODE)
         RETURN
      ELSE
         CALL ARDIIS(FILNAM,RCODE)
         RETURN
      ENDIF
C MATRIX (1) OR PACKED (2) IS NEW LP...CHECK STUFF
      IF(RCODE.NE.0)RETURN
C OK, INITIALIZE SYNTAX FILE
      SYNAM  = ' (none)'
C ...AND NEW LP
      CALL ANEWLP
      RETURN
C
C ** AREAD ENDS HERE
      END
      SUBROUTINE ANEWLP
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This initializes modules (after new LP readin).
C                           ~~~~~
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C INITIALIZE SUBMATRIX
      CALL GMPCLR
      NRCSUB(1) = 0
      NRCSUB(2) = 0
      RCSUB1(1) = 0
      RCSUB1(2) = 0
      NZSUB     = 0
C INITIALIZE BLOCKS
      CALL BLINIT
C INITIALIZE EXPLAIN
      CALL EXINIT
C INITIALIZE GRAPH
      CALL GRAPH0
C BRING IN BASE
      CALL GETBAS(PRBNAM,OBJNAM,RHSNAM,RNGNAM,BNDNAM,SOLST,SOLNAM,
     2  NROWS,NCOLS,NONZER,NVALS,NONES,OBJNUM,
     3  NRFREE,NCFIX,MAXRLN,MAXCLN,NAMELN,VINF,VTOLAB,VTOLRE)
C INITIALIZE SCHEMA
      CALL SCHCLR(0,0,*900)
C
900   RETURN
C
C ** ANEWLP ENDS HERE
      END
      SUBROUTINE ARDMAT(FILNAM,CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This reads matrix file (using GETMAT)...
C      Matrix options : {MINIMIZE | MAXIMIZE}  obj_name,
C            RHS = rhs_name, RANGE = range_name,  BOUND = bound_name
C            LEN_NAME = number, ZERO_TOL = value
C
      CHARACTER*128 CLIST
      CHARACTER*64  FILNAM
C LOCAL
      CHARACTER*8   OPTION(8)
      CHARACTER*16  RNAME,CNAME
      CHARACTER*1   CHAR
      LOGICAL       EXIST
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C  INITIALIZE OPTIONS
C     OPTNAM = 'MINIMIZE' or 'MAXIMIZE'
C        (according to user's switch setting)
C NAMES
      PRBNAM = ' '
      OBJNAM = ' '
      RHSNAM = ' '
      RNGNAM = ' '
      BNDNAM = ' '
C LENGTH OF ROW/COL NAME
      NAMELN = 8
C ZERO TOLERANCE
      CALL AKEYS0
      VTZERO = VTOL0C
C
C PARSE OPTIONS
      OPTION(1)='MINIMIZE'
      OPTION(2)='MAXIMIZE'
      OPTION(3)='RHS     '
      OPTION(4)='RANGE   '
      OPTION(5)='BOUND   '
      OPTION(6)='LEN_NAME'
      OPTION(7)='ZERO_TOL'
C
C   TOP OF OPTION LOOP
10    NUMBER = 7
        CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
        IF(RCODE.NE.0)RETURN
        IF(NUMBER.EQ.0)GOTO 50
C
C  OPTION (NUMBER) MUST HAVE NAME OR VALUE
        CALL FTOKEN(CLIST,FIRST,LAST,CNAME,16,CHAR)
        IF(NUMBER.EQ.6)THEN
C  ...INTEGER NAME LENGTH (NAMELN) = # CHARS IN ROW/COL NAMES
           CALL FC2I(CNAME,16,INT,RCODE)
           IF(RCODE.NE.0)RETURN
           IF(INT.LT.1.OR.INT.GT.10)THEN
              PRINT *,' CANNOT HAVE NAME LENGTH=',INT,
     1                '...MUST BE 1 TO 10'
              RCODE = -1
              RETURN
           ENDIF
           NAMELN = INT
        ELSE IF(NUMBER.EQ.7)THEN
C  ...REAL ZERO TOLERANCE
           CALL FC2R(CNAME,VTZERO,RCODE)
           IF(RCODE.NE.0)RETURN
           IF( VTZERO.LT.0. )THEN
              PRINT *,' ** ZERO TOLERANCE CANNOT BE NEGATIVE'
              RCODE = -1
              RETURN
           ENDIF
        ELSE
C BASE SETTING IS CHAR TYPE (CNAME REQUIRED)
           IF(CNAME.EQ.' ')THEN
              PRINT *,' ** NAME EXPECTED AFTER '//OPTION(NUMBER)
              RCODE = -1
              RETURN
           ENDIF
           RNAME = OPTION(NUMBER)
           IF(RNAME.EQ.'MINIMIZE'.OR.RNAME.EQ.'MAXIMIZE')THEN
               OPTNAM = RNAME
               OBJNAM = CNAME
           ELSE IF(RNAME.EQ.'RHS   ')THEN
               RHSNAM = CNAME
           ELSE IF(RNAME.EQ.'RANGE ')THEN
               RNGNAM = CNAME
           ELSE IF(RNAME.EQ.'BOUND ')THEN
               BNDNAM = CNAME
           ENDIF
        ENDIF
      GOTO 10
C ===== END OF OPTION LOOP =====
50    CONTINUE
C SET NAMES FOR GETMAT
      CALL GSETNM(OPTNAM,OBJNAM,RHSNAM,RNGNAM,BNDNAM,NAMELN)
C
C PREPARE TO READ MATRIX FILE
      MATNAM = FILNAM
      CALL FSETNM('MATRIX  ',MATNAM,BEGEXT,RCODE)
      IF(RCODE.NE.0)RETURN
      CALL FINQUR(MATNAM,IOSTAT,EXIST,*1313)
      IF( .NOT.EXIST )THEN
         CALL FSLEN(MATNAM,64,L)
         PRINT *,' ** FILE '//MATNAM(:L)//' DOES NOT EXIST'
         RCODE=1
         RETURN
      ENDIF
C
      CALL FLOPEN(DATFIL,MATNAM,'FORMATTED','OLD',*1390)
C READ MATRIX FILE
      CALL GETMAT(SWMSG,NAMELN,VTZERO,PMXROW,PMXCOL,RCODE)
CC 7-16-95              ======
      IF( RCODE.NE.0 )RETURN
C INITIALIZE ANALYZE COMPONENTS
      CALL ANEWLP
C INITIALIZE SOLUTION
      CALL GSETSL(SWRATE)
      SOLNAM = 'logical'
      IF(SWMSG)PRINT *,' Initialized solution to all logical basis.'
      RETURN
C
1313  CONTINUE
      CALL FSLEN(MATNAM,64,L)
      PRINT *,' ** IO ERROR ATTEMPTING TO INQUIRE ABOUT '//MATNAM(:L)
1390  RCODE=1
      PRBNAM = ' '
C
      RETURN
C
C ** ARDMAT ENDS HERE
      END
      SUBROUTINE ARDPCK(FILNAM,CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This reads packed file (using GRDPCK or GRDFPK)...
C
C      Pack option:  FORMAT
C
      CHARACTER*128 CLIST
      CHARACTER*64  FILNAM
C LOCAL
      CHARACTER*16  RNAME
      CHARACTER*1   CHAR
      LOGICAL*1     SW
      LOGICAL       EXIST
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C INITIALIZE MATRIX NAME (NULL MEANS NO LP IN MEMORY)
      MATNAM = ' '
C
C SEE IF FORMATTED
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,8,CHAR)
      SW = (RNAME.NE.' ')
      PCKNAM = FILNAM
      IF(SW)THEN
          CALL FMATCH(RNAME,'FORMAT',' ',SW)
          IF(.NOT.SW)THEN
               PRINT *,'** CANNOT RECOGNIZE '//RNAME
               RCODE = -1
               RETURN
          ENDIF
          CALL FSETNM('FORMAT  ',PCKNAM,BEGEXT,RCODE)
      ELSE
          CALL FSETNM('PACKED  ',PCKNAM,BEGEXT,RCODE)
      ENDIF
      IF(RCODE.NE.0)RETURN
C
C ...SW = TRUE IFF FORMATTED
C
      CALL FINQUR(PCKNAM,IOSTAT,EXIST,*1313)
      IF(.NOT.EXIST)THEN
         CALL FSLEN(PCKNAM,64,L)
         PRINT *,' ** FILE '//PCKNAM(:L)//' DOES NOT EXIST'
         RCODE = 1
         RETURN
      ENDIF
C
      IF(SW)THEN
         CALL FLOPEN(DATFIL,PCKNAM,'FORMATTED','UNKNOWN',*1313)
         CALL GRDFPK(SWRATE,RCODE)
      ELSE
C UNIX AND XENIX NEED RECORD LENGTH (RECL) FOR UNFORMATTED FILE
CIXNX           OPEN(PCKFIL,FILE=PCKNAM,FORM='UNFORMATTED',
CIXNX     1          STATUS='UNKNOWN',RECL=2048,ERR=1313)
CIXNX           GOTO 111
CIXNXC LEAVING XNX OPEN, RATHER THAN FLOPEN, BECAUSE IT WORKS
           CALL FLOPEN(PCKFIL,PCKNAM,'UNFORMATTED','UNKNOWN',*1313)
111        CONTINUE
           CALL GRDPCK(SWRATE,RCODE)
      ENDIF
C
      IF(RCODE.EQ.0)THEN
         CALL ANEWLP
         IF(SWMSG)PRINT *,' Packed LP read from '//PCKNAM
      ELSE
         PRBNAM = ' '
      ENDIF
      RETURN
C
1313  CONTINUE
      RCODE = 1
      PRBNAM = ' '
C
      RETURN
C
C ** ARDPCK ENDS HERE
      END
      SUBROUTINE ARDSYN(FILNAM,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This reads syntax file (FILNAM).
C
      CHARACTER*64  FILNAM
C LOCAL
      CHARACTER*128 CLIST
      LOGICAL       EXIST
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      SYNAM = FILNAM
      CALL FSETNM('SYNTAX  ',SYNAM,BEGEXT,RCODE)
      IF(RCODE.NE.0)RETURN
C
      CALL FINQUR(SYNAM,IOSTAT,EXIST,*1313)
      IF(EXIST)THEN
         CALL FLOPEN(DATFIL,SYNAM,'FORMATTED','OLD',*1313)
         CALL EXREAD(RCODE)
         IF(RCODE.NE.0)GOTO 1314
      ELSE
         IF( .NOT.SWRULE )THEN
            CALL FSLEN(SYNAM,64,L)
            PRINT *,' ** FILE '//SYNAM(:L)//' DOES NOT EXIST'
            RCODE = 1
         ENDIF
         GOTO 1314
      ENDIF
C
      IF(SWMSG)PRINT *,' Syntax read from '//SYNAM
C CLEAR SCHEMA (IF WAS SET BY ANOTHER SYNTAX FILE)
      CLIST = 'CLEAR'
      FIRST = 1
      LAST  = 5
      CALL ASCHMA(CLIST,FIRST,LAST,RCODE)
      RETURN
C
1313  CONTINUE
      RCODE = 1
1314  SYNAM = ' (none)'
      SWEXPL = .FALSE.
C
      RETURN
C
C ** ARDSYN ENDS HERE
      END
      SUBROUTINE ARDIIS(FILNAM,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This reads IIS file (FILNAM).
C
      CHARACTER*64  FILNAM
C LOCAL
      CHARACTER*128 CLIST
      CHARACTER*16  RNAME
      CHARACTER*1   CHAR
      LOGICAL       EXIST
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C PREPARE FILESPEC
      CALL FSETNM('IIS     ',FILNAM,BEGEXT,RCODE)
      IF(RCODE.NE.0)RETURN
C
      CALL FINQUR(FILNAM,IOSTAT,EXIST,*1310)
      IF(EXIST)THEN
         CALL FLOPEN(DATFIL,FILNAM,'FORMATTED','OLD',*1399)
      ELSE
         CALL FSLEN(FILNAM,64,L)
         PRINT *,' ** FILE ',FILNAM(:L),' DOES NOT EXIST'
         RCODE = 1
         RETURN
      ENDIF
C
C READ IIS FILE UNTIL 'BEGIN COLUMNS' is found
10    CALL FLRDLN(CLIST,DATFIL,RCODE)
      IF(RCODE)1390,20,1399
20    IF(CLIST(:13).NE.'BEGIN COLUMNS')GOTO 10
      IF( SWMSG .AND. .NOT.SWRULE )PRINT *,' ',CLIST(:60)
C
C CREATE COLUMN BLOCKS
      RNAME = 'IIS.LO'
      CALL BLKNEW('COL ',RNAME,BCLO,RCODE)
      IF(RCODE.NE.0)RETURN
      RNAME = 'IIS.UP'
      CALL BLKNEW('COL ',RNAME,BCUP,RCODE)
      IF(RCODE.NE.0)RETURN
      RNAME = 'IIS.LU'
      CALL BLKNEW('COL ',RNAME,BCLU,RCODE)
      IF(RCODE.NE.0)RETURN
      NLO = 0
      NUP = 0
      NLU = 0
C
C COLUMNS SECTION
100   CALL FLRDLN(CLIST,DATFIL,RCODE)
      IF(RCODE)1390,120,1399
C
120   CONTINUE
      IF(CLIST(:11).EQ.'END COLUMNS')GOTO 490
      FIRST = 1
      LAST = 79
C GET COLUMN
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,NAMELN,CHAR)
      CALL GETNUM('COL ',RNAME,NUMBER)
      IF(NUMBER.EQ.0)GOTO 1330
C GET TYPE OF BOUND
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,6,CHAR)
C ADD COLUMN TO BLOCK
      IF(RNAME.EQ.'lower')THEN
C ...BCLO
         CALL BLKADD('COL ',NUMBER,BCLO)
         NLO = NLO+1
      ELSE IF(RNAME.EQ.'upper')THEN
C ...BCUP
         CALL BLKADD('COL ',NUMBER,BCUP)
         NUP = NUP+1
      ELSE IF(RNAME.EQ.'both' .OR. RNAME.EQ.'fixed')THEN
C ...BCLU
         CALL BLKADD('COL ',NUMBER,BCLU)
         NLU = NLU+1
      ELSE
         GOTO 1330
      ENDIF
C RETURN TO TOP OF COLUMN LOOP
      GOTO 100
C
490   CONTINUE
C :::::: END COLUMNS SECTION ::::::::
C
      IF( SWMSG .AND. .NOT.SWRULE )
     1   PRINT *,' IIS has',NLO+NUP+NLU,' column bounds'
C CREATE ROW BLOCKS
      RNAME = 'IIS.LO'
      CALL BLKNEW('ROW ',RNAME,BRLO,RCODE)
      IF(RCODE.NE.0)RETURN
      RNAME = 'IIS.UP'
      CALL BLKNEW('ROW ',RNAME,BRUP,RCODE)
      IF(RCODE.NE.0)RETURN
      RNAME = 'IIS.LU'
      CALL BLKNEW('ROW ',RNAME,BRLU,RCODE)
      IF(RCODE.NE.0)RETURN
      NLO = 0
      NUP = 0
      NLU = 0
      CALL FLRDLN(CLIST,DATFIL,RCODE)
      IF(RCODE)1390,495,1399
495   CONTINUE
      IF( SWMSG .AND. .NOT.SWRULE )PRINT *,' ',CLIST(:60)
      IF(CLIST(:10).NE.'BEGIN ROWS')GOTO 1350
C
C ROWS SECTION
500   CALL FLRDLN(CLIST,DATFIL,RCODE)
      IF(RCODE)1390,520,1399
520   CONTINUE
      IF(CLIST(:8).EQ.'END ROWS')GOTO 890
C GET ROW
      FIRST = 1
      LAST  = 79
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,NAMELN,CHAR)
      CALL GETNUM('ROW ',RNAME,NUMBER)
      IF(NUMBER.EQ.0)GOTO 1330
C GET BOUND TYPE
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,6,CHAR)
C ADD TO BLOCK
      IF(RNAME.EQ.'lower')THEN
         CALL BLKADD('ROW ',NUMBER,BRLO)
         NLO = NLO+1
      ELSE IF(RNAME.EQ.'upper')THEN
         CALL BLKADD('ROW ',NUMBER,BRUP)
         NUP = NUP+1
      ELSE IF(RNAME.EQ.'both' .OR. RNAME.EQ.'fixed')THEN
         CALL BLKADD('ROW ',NUMBER,BRLU)
         NLU = NLU+1
      ELSE
         GOTO 1330
      ENDIF
C RETURN TO TOP OF ROWS LOOP
      GOTO 500
C
890   CONTINUE
C :::::: END ROWS SECTION ::::::::
C
      IF( SWMSG .AND. .NOT.SWRULE )
     1   PRINT *,' IIS has',NLO+NUP+NLU,' rows'
C
900   RCODE = 0
      IF( SWMSG .AND. .NOT.SWRULE )THEN
         CALL FSLEN(FILNAM,64,L)
         PRINT *,' IIS read from '//FILNAM(:L)//'...See blocks'
      ENDIF
      CLOSE(DATFIL)
      CALL BLKCLR
      RETURN
C
C ERROR RETURNS
1310  PRINT *,' ** IO ERROR INQUIRING ABOUT '//FILNAM
      GOTO 1399
1330  PRINT *,' ** '//RNAME(:NAMELN)//' not recognized'
      GOTO 1399
1350  PRINT *,' ** BEGIN ROWS expected'
      GOTO 1399
1390  PRINT *,' ** PREMATURE END OF FILE REACHED'
1399  RCODE = 1
      RETURN
C
C ** ARDIIS ENDS HERE
      END
      SUBROUTINE ASUMRY
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCFLIP
C          DCFLIP NEEDED FOR SCRWTH
C
C This writes summary of LP in memory to OUTPUT (using FTEXT).
C
C LOCAL
      LOGICAL*1     EREN(4)
      CHARACTER*128 CLIST
      CHARACTER*64  STR64
      CHARACTER*16  RNAME,CNAME
      CHARACTER*1   CHAR
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      LINE = 0
C PROBLEM NAMES
      CLIST = 'Problem '//PRBNAM
      CALL FSLEN(CLIST,30,LAST)
      CLIST(LAST+2:) = 'is to'
      CALL FSLEN(CLIST,80,TAB)
      TAB = TAB+1
      CLIST(TAB+1:) = OPTNAM(:9)//OBJNAM
      CALL FSLEN(CLIST,80,LAST)
      IF( SOLST.EQ.'INFEASIBLE' )THEN
         CLIST(LAST+1:) = '.'
      ELSE
         CALL GETSOL('ROW ',OBJNUM,V,VD,CHAR,LS)
         STR64 = ' '
         CALL FR2CLJ(STR64,V,LS)
         IF( LAST+21.GE.SCRWTH )THEN
            CALL FSLEN(CLIST,128,LAST)
            CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
            LAST = TAB
         ENDIF
         CLIST(LAST+1:) = '...Level='//STR64
      ENDIF
      CALL FSLEN(CLIST,128,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C
      CLIST = 'RHS='//RHSNAM
      CALL FSLEN(CLIST,30,LAST)
      CLIST(LAST+3:) = 'RANGE='//RNGNAM
      CALL FSLEN(CLIST,80,LAST)
      CLIST(LAST+3:) = 'BOUND='//BNDNAM
      CALL FSLEN(CLIST,80,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C FILES
      IF(MATNAM.EQ.' ')THEN
           CLIST = 'Source file: '//PCKNAM
      ELSE
           CLIST = 'Source file: '//MATNAM
      ENDIF
      CALL FSLEN(CLIST,120,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C EDITS
      CALL GETEDS(EFREE,EFIX,EROUND,EREFR,EPIVOT,EREDUC,EREN)
C ...SKIP IF THERE WERE NONE
      IF( EFREE.GT.0 .OR. EFIX  .GT.0 .OR. EROUND.GT.0 .OR.
     1    EREFR.GT.0 .OR. EPIVOT.GT.0 .OR. EREDUC.GT.0 .OR.
     2    EREN(1) .OR. EREN(2) .OR. EREN(3) .OR. EREN(4)
     3  )THEN
C SOMETHING WAS CHANGED
         CLIST = '  ...Edits performed:'
         CALL FSLEN(CLIST,64,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
         MARGIN = 5
C NAMES
         CLIST = 'Renamed:'
         LAST = 9
         IF( EREN(1) )THEN
            CLIST(LAST+2:) = 'PROBLEM,'
            CALL FSLEN(CLIST,128,LAST)
         ENDIF
         IF( EREN(2) )THEN
            CLIST(LAST+2:) = ' RHS,'
            CALL FSLEN(CLIST,128,LAST)
         ENDIF
         IF( EREN(3) )THEN
            CLIST(LAST+2:) = ' RANGE,'
            CALL FSLEN(CLIST,128,LAST)
         ENDIF
         IF( EREN(4) )THEN
            CLIST(LAST+2:) = ' BOUND,'
            CALL FSLEN(CLIST,128,LAST)
         ENDIF
         IF( CLIST(LAST:LAST).EQ.',' )THEN
            CLIST(LAST:LAST) = '.'
            CALL FTEXT(CLIST,LAST,MARGIN,0,LINE,'CLEAR ',*900)
         ENDIF
C BOUNDS
         IF( EFREE.GT.0 )THEN
            IF( EFREE.EQ.1 )THEN
               CLIST = '1 Bound Freed'
            ELSE
               CALL FI2C(RNAME,EFREE,FR)
               CLIST = RNAME(FR:8)//' Bounds Freed'
            ENDIF
            CALL FSLEN(CLIST,64,LAST)
            CALL FTEXT(CLIST,LAST,MARGIN,0,LINE,'CLEAR ',*900)
         ENDIF
         IF( EFIX.GT.0 )THEN
            IF( EFIX.EQ.1 )THEN
               CLIST = '1 Bound Fixed'
            ELSE
               CALL FI2C(RNAME,EFIX,FR)
               CLIST = RNAME(FR:8)//' Bounds Fixed'
            ENDIF
            CALL FSLEN(CLIST,64,LAST)
            CALL FTEXT(CLIST,LAST,MARGIN,0,LINE,'CLEAR ',*900)
         ENDIF
         IF( EROUND.GT.0 )THEN
            IF( EROUND.EQ.1 )THEN
               CLIST = '1 Bound Changed by Integer Rounding'
            ELSE
               CALL FI2C(RNAME,EROUND,FR)
               CLIST = RNAME(FR:8)//
     1               ' Bounds Changed by Integer Rounding'
            ENDIF
            CALL FSLEN(CLIST,128,LAST)
            CALL FTEXT(CLIST,LAST,MARGIN,0,LINE,'CLEAR ',*900)
         ENDIF
         IF( EREDUC.GT.0 )THEN
            IF( EREDUC.EQ.1 )THEN
               CLIST = '1 Bound Overwritten by REDUCE'
            ELSE
               CALL FI2C(RNAME,EREDUC,FR)
               CLIST = RNAME(FR:8)//' Bounds Overwritten by REDUCE'
            ENDIF
            CALL FSLEN(CLIST,128,LAST)
            CALL FTEXT(CLIST,LAST,MARGIN,0,LINE,'CLEAR ',*900)
         ENDIF
C SOLUTION VALUES
         IF( EREFR.GT.0 )THEN
            IF( EREFR.EQ.1 )THEN
               CLIST = '1 Solution Value Changed by Basis Refresh'
            ELSE
               CALL FI2C(RNAME,EREFR,FR)
               CLIST = RNAME(FR:8)//
     1               ' Solution Values Changed by Basis Refresh'
            ENDIF
            CALL FSLEN(CLIST,128,LAST)
            CALL FTEXT(CLIST,LAST,MARGIN,0,LINE,'CLEAR ',*900)
         ENDIF
C BASIS
         IF( EPIVOT.GT.0 )THEN
            IF( EPIVOT.EQ.1 )THEN
               CLIST = 'Basis Changed by 1 Pivot'
            ELSE
               CALL FI2C(RNAME,EPIVOT,FR)
               CLIST = 'Basis Changed by '//RNAME(FR:8)//' Pivots'
            ENDIF
            CALL FSLEN(CLIST,128,LAST)
            CALL FTEXT(CLIST,LAST,MARGIN,0,LINE,'CLEAR ',*900)
         ENDIF
      ENDIF
      IF(SYNAM(1:1).NE.' ')THEN
         CLIST = 'Syntax file: '//SYNAM
         CALL FSLEN(CLIST,80,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
      ENDIF
C SOLUTION
      CLIST = 'Solver: '//SOLNAM
      CALL FSLEN(CLIST,30,LAST)
      STAT = LAST+5
      CLIST(STAT:) = 'Solution Status: '//SOLST
      CALL FSLEN(CLIST,80,LAST)
      IF( LAST.GE.SCRWTH )THEN
         STR64 = CLIST(STAT:)
         CLIST(STAT:) = ' '
         CALL FSLEN(CLIST,60,LAST)
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
         CLIST = STR64
         STAT = 1
         CALL FSLEN(CLIST,60,LAST)
      ENDIF
C SOLUTION TYPE (BASIC / STRICTLY COMPLEMENTARY)
      CALL GETSTP(RNAME,CNAME)
      IF( RNAME(1:1).EQ.'B' )THEN
         RNAME = 'Basic'
      ELSE IF( RNAME(1:1).EQ.'N' )THEN
         RNAME = 'Nonbasic'
      ELSE
         RNAME = ' '
      ENDIF
      STR64 = 'Type: '//RNAME
      CALL FSLEN(STR64,40,LS)
      IF( CNAME(1:1).EQ.'S' )THEN
         IF( RNAME.NE.' ')THEN
            STR64(LS+2:) = 'and'
            LS = LS+4
         ENDIF
         STR64(LS+2:) = 'Strictly Complementary'
         CALL FSLEN(STR64,60,LS)
      ELSE IF( CNAME(1:1).EQ.'N' )THEN
         IF( RNAME.NE.' ')THEN
            STR64(LS+2:) = 'and'
            LS = LS+4
         ENDIF
         STR64(LS+2:) = 'Not Strictly Complementary'
         CALL FSLEN(STR64,60,LS)
      ELSE IF( RNAME.EQ.' ' )THEN
         STR64(LS+2:) = 'Unknown'
      ENDIF
      IF( LAST+LS+3.GE.SCRWTH )THEN
         CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
         CLIST(STAT:) = '           '//STR64
         CALL FSLEN(CLIST,80,LAST)
         IF( LAST.GE.SCRWTH )CLIST = 'Solution '//STR64
      ELSE
         CLIST(LAST+3:) = STR64(:LS)
      ENDIF
      CALL FSLEN(CLIST,127,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C STATISTICS
      CLIST = 'LP Statistics:'
      CALL FSLEN(CLIST,30,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C
      V  = NROWS*NCOLS
      VD = 100.0*NONZER/V
      V  = NONZER
      VE = 100.0*NVALS/V
      VC = 100.0*NONES/V
C
      CNAME = ' '
      RNAME = ' '
      CALL FI2C(CNAME,NROWS,FC)
      CALL FI2C(RNAME,NRFREE,FR)
      CLIST = CNAME(FC:8)//' Rows ('//RNAME(FR:8)//' free) and'
      CALL FSLEN(CLIST,80,LAST)
      CALL FI2C(CNAME,NCOLS,FC)
      CALL FI2C(RNAME,NCFIX,FR)
      CLIST(LAST+2:) = CNAME(FC:8)//' Columns ('
     1               //RNAME(FR:8)//' fixed)'
      CALL FSLEN(CLIST,120,LAST)
      CALL FTEXT(CLIST,LAST,5,0,LINE,'CLEAR ',*900)
C
      CALL FI2C(CNAME,NONZER,FC)
      CALL FI2C(RNAME,NVALS,FR)
      CLIST = CNAME(FC:8)//' Nonzeroes ('//RNAME(FR:8)//' distinct'
      CALL FSLEN(CLIST,120,LAST)
      CALL FI2C(CNAME,NONES,FC)
      CLIST(LAST+1:) = '... '//CNAME(FC:8)//' ones).'
      CALL FSLEN(CLIST,120,LAST)
      CALL FTEXT(CLIST,LAST,5,0,LINE,'CLEAR ',*900)
C
      CALL FR2C(CNAME,VD)
      CNAME(13:)='%'
      LC = 13
      CALL FSQUEZ(CNAME,LC)
      CALL FR2C(RNAME,VE)
      RNAME(13:)='%'
      LR = 13
      CALL FSQUEZ(RNAME,LR)
      CLIST = 'Density = '//CNAME(:LC)
     1    //'  Entropy = '//RNAME(:LR)//'  Unity ='
      CNAME = ' '
      CALL FR2C(CNAME,VC)
      CNAME(13:)='%'
      LC = 13
      CALL FSQUEZ(CNAME,LC)
      CALL FSLEN(CLIST,120,LAST)
      CLIST(LAST+2:) = CNAME
      CALL FSLEN(CLIST,120,LAST)
      CALL FTEXT(CLIST,LAST,5,0,LINE,'CLEAR ',*900)
C EXTREME ROW AND COLUMN
      CALL GETNAM('ROW ',MAXRLN,RNAME)
      CALL GETRIM('ROW ',MAXRLN,VL,VU,VC,NZROW)
      CNAME = ' '
      CALL FI2C(CNAME,NZROW,FC)
      CLIST = 'Row    '//RNAME(:NAMELN)//' has the most nonzeroes = '
     1        //CNAME(FC:8)//'.'
      CALL FSLEN(CLIST,120,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C
      CALL GETNAM('COL ',MAXCLN,CNAME)
      CALL GETRIM('COL ',MAXCLN,VL,VU,VC,NZCOL)
      CALL FI2C(RNAME,NZCOL,FR)
      CLIST = 'Column '//CNAME(:NAMELN)//' has the most nonzeroes = '
     1        //RNAME(FR:8)//'.'
      CALL FSLEN(CLIST,120,LAST)
      CALL FTEXT(CLIST,LAST,1,0,LINE,'CLEAR ',*900)
C
900   RETURN
C
C ** ASUMRY ENDS HERE
      END
      SUBROUTINE AWRITE(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This writes packed file, PCKFIL or DATFIL
C
C SYNTAX:  WRITEOUT [filespec [FORMAT]]
C
C    Default filename = PRBNAM
C ...PACKED [FORMAT] extension and prefix from _SETUP
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*64  FILNAM
      CHARACTER*16  RNAME
      CHARACTER*1   CHAR
      LOGICAL*1     SW
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
C See if filename specified
      CALL FTOKEN(CLIST,FIRST,LAST,FILNAM,NAMLEN,CHAR)
      IF(FILNAM.EQ.' ')THEN
           FILNAM = PRBNAM
           SW = .FALSE.
      ELSE
           IF(FILNAM.EQ.'*')FILNAM = PRBNAM
           CALL FTOKEN(CLIST,FIRST,LAST,RNAME,8,CHAR)
           SW = (RNAME.NE.' ')
           IF(SW)THEN
                CALL FMATCH(RNAME,'FORMAT',' ',SW)
                IF(.NOT.SW)THEN
                   PRINT *,' ** CANNOT RECOGNIZE '//RNAME
                   RCODE = -1
                   RETURN
                ENDIF
           ENDIF
      ENDIF
C
C NOW SW = TRUE IFF FORMAT
C
      PCKNAM = FILNAM
      IF(SW)THEN
         CALL FSETNM('FORMAT  ',PCKNAM,BEGEXT,RCODE)
         IF(RCODE.NE.0)RETURN
         CALL FILOUT(PCKNAM,RCODE)
         IF( RCODE.NE.0 )RETURN
         CALL FLOPEN(DATFIL,PCKNAM,'FORMATTED','UNKNOWN',*1390)
         CALL GWRFPK(RCODE)
      ELSE
         CALL FSETNM('PACKED  ',PCKNAM,BEGEXT,RCODE)
         IF( RCODE.NE.0 )RETURN
         CALL FILOUT(PCKNAM,RCODE)
         IF( RCODE.NE.0 )RETURN
C UNIX AND XENIX NEED RECORD LENGTH (RECL) FOR UNFORMATTED FILE
CIXNX         OPEN(PCKFIL,FILE=PCKNAM,FORM='UNFORMATTED',
CIXNX     1        STATUS='UNKNOWN',RECL=2048,ERR=1300)
CIXNXC  LEAVING OPEN, RATHER THAN FLOPEN, BECAUSE IT WORKS
CIXNX         GOTO 111
         CALL FLOPEN(PCKFIL,PCKNAM,'UNFORMATTED','UNKNOWN',*1390)
111      CONTINUE
         CALL GWRPCK(RCODE)
      ENDIF
C
      IF( RCODE.EQ.0 .AND. SWMSG )
     1   PRINT *,' Packed LP written to '//PCKNAM
      RETURN
C
CIXNX1300  PRINT *,' ** IO ERROR ATTEMPTING TO OPEN ',FILNAM
1390  RCODE = 1
      RETURN
C
C ** AWRITE ENDS HERE
      END
      SUBROUTINE ARDSOL(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This reads solution file.
C   SYNTAX:  SOLUTION {MINOS | OSL | MPS} [filespec]
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*64  FILNAM
      CHARACTER*16  RNAME
      CHARACTER*1   CHAR
      LOGICAL       EXIST
      CHARACTER*8   OPTION(5)
C :::::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::
      OPTION(1) = 'MINOS'
      OPTION(2) = 'OB1'
      OPTION(3) = 'OSL'
      OPTION(4) = 'MPS'
CC 7-20-95      OPTION(5) = 'XMP'
C GET OPTION
      RCODE = -1
CC 7-2-95      NUMBER = 5
      NUMBER = 4
      CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
      IF(RCODE.NE.0)RETURN
C
      RNAME = OPTION(NUMBER)
CC
        IF( RNAME.EQ.'OB1 ' )THEN
           PRINT *,' Sorry, OB1 no longer supported...contact author'
           RCODE = 1
           RETURN
        ENDIF
CC
CC      IF( RNAME.EQ.'XMP' )THEN
CC         IF(SWMSG)PRINT *,
CC     1      ' Warning: XMP solution presumed to come from OB1'
CC         RNAME = 'OB1'
CC      ENDIF
C
C Parse CLIST(FIRST:LAST) to obtain filename
      CALL FTOKEN(CLIST,FIRST,LAST,FILNAM,NAMLEN,CHAR)
      IF(FILNAM.EQ.' '.OR.FILNAM.EQ.'*')FILNAM = PRBNAM
      SOLNAM = FILNAM
      CALL FSETNM(RNAME,SOLNAM,BEGEXT,RCODE)
      IF(RCODE.NE.0)RETURN
C
      CALL FINQUR(SOLNAM,IOSTAT,EXIST,*1313)
      IF(.NOT.EXIST)THEN
         RCODE = 1
         CALL FSLEN(SOLNAM,64,L)
         PRINT *,' ** FILE '//SOLNAM(:L)//' DOES NOT EXIST'
         SOLNAM = 'UNKNOWN'
         RETURN
      ENDIF
C
      IF(RNAME.NE.'MPS')THEN
C SOLUTION FILE IS FORMATTED
         CALL FLOPEN(DATFIL,SOLNAM,'FORMATTED','OLD',*1390)
      ELSE
C MPS SOLUTION FILE IS UNFORMATTED
C UNIX AND XENIX NEED RECORD LENGTH (RECL) FOR UNFORMATTED FILE
CIXNX           OPEN(PCKFIL,FILE=PCKNAM,FORM='UNFORMATTED',
CIXNX     1          STATUS='OLD',RECL=2048,ERR=1300)
CIXNX           GOTO 111
         CALL FLOPEN(PCKFIL,SOLNAM,'UNFORMATTED','OLD',*1390)
111      CONTINUE
      ENDIF
      CALL GRDSOL(SWMSG,RNAME,RCODE)
C
      IF(RCODE.NE.0)THEN
         CALL GSETSL(SWRATE)
         SOLNAM = 'logical'
         PRINT *,' ...Solution initialized to all-logical basis.'
      ELSE
         CALL GETBAS(PRBNAM,OBJNAM,RHSNAM,RNGNAM,BNDNAM,SOLST,SOLNAM,
     1        NROWS,NCOLS,NONZER,NVALS,NONES,OBJNUM,NRFREE,
     2        NCFIX,MAXRLN,MAXCLN,NAMELN,VINF,VTOLAB,VTOLRE)
C
         SWRATE = .FALSE.
         CALL GMPCLR
         CALL ASETSB('ROW ')
         CALL ASETSB('COL ')
         IF(SWMSG) PRINT *,' Submatrix has been cleared.'
      ENDIF
C
      IF( SOLST.NE.'INFEASIBLE' .OR. RNAME.NE.'OSL')RETURN
C OSL DOES NOT PROVIDE PHASE 1 PRICES
      PRINT *,' Warning:  Phase 1 prices not provided by OSL'
      PRINT *,' Use BASIS REFRESH DUAL to set'
C
      RETURN
C
CIXNX1300  PRINT *,' ** I/O ERROR OPENING ',SOLNAM
1390  RCODE = 1
      RETURN
1313  PRINT *,' ** I/O ERROR INQUIRING ABOUT '//SOLNAM
      RCODE = 1
      RETURN
C
C ** ARDSOL ENDS HERE
      END
