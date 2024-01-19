C              ::: ASUBMAT.FOR  6-30-95 :::
C
C LAST DATE:  Earlier dates deleted
C             4-25-93...Added DUAL option to SUBMAT WRITE
C             5-27-93...Allowed NAMELN > 10 in SUBMAT WRITE
C
C This contains the following subroutines for ANALYZE
C
C    ASBMAT.....initialization for submatrix management
C                ...MUST BE CALLED BY SYSIN
C    ASETKY.....sets key value
C    ASBSET.....manages submatrix map (ADD|DELETE|REVERSE|SAVE)
C    AGETRC.....gets ROW|COLUMN specification
C    ACONDN.....parses conditional
C    ARIMKY.....gets number of rimkey from character
C    ANXTRC.....gets next qualified row/column
C    ASBNEW.....creates new row/column submatrix from latest conditional
C    ASETSB.....sets stats for submatrix (parsing completed)
C    ASBDBG.....debug routine
C
      SUBROUTINE ASBMAT
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This initializes submatrix...MUST BE CALLED BY SYSIN
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      NKEYS = 5
C     ROWS               COLUMNS
C     ----------------   ------------------
      RIMKEY(1,1) = 'L'
                           RIMKEY(2,1) = 'L'
      RIMKEY(1,2) = 'U'
                           RIMKEY(2,2) = 'U'
      RIMKEY(1,3) = 'C'
                           RIMKEY(2,3) = 'C'
      RIMKEY(1,4) = 'Y'
                           RIMKEY(2,4) = 'X'
      RIMKEY(1,5) = 'P'
                           RIMKEY(2,5) = 'D'
C
C
      NOPLST = 6
      OPLIST(1) = 'LE'
      OPLIST(2) = 'LT'
      OPLIST(3) = 'EQ'
      OPLIST(4) = 'NE'
      OPLIST(5) = 'GT'
      OPLIST(6) = 'GE'
C
      MAXOPS = 8
      VERSUB = '4.25'
      RETURN
C
C ** ASBMAT ENDS HERE
      END
      SUBROUTINE ASETKY(KEY,VL,VU,VC,VX,VP,V)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This sets rim KEY value V
C
      CHARACTER*1 KEY
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF(KEY.EQ.'L')THEN
            V = VL
      ELSE IF(KEY.EQ.'U')THEN
            V = VU
      ELSE IF(KEY.EQ.'C'.OR.KEY.EQ.'S')THEN
            V = VC
      ELSE IF(KEY.EQ.'X'.OR.KEY.EQ.'Y')THEN
            V = VX
      ELSE
            V = VP
      ENDIF
      RETURN
C
C ** ASETKY ENDS HERE
      END
      SUBROUTINE ASBSET(CLIST,FIRST,LAST,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCFLIP.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCFLIP)
CI$$INSERT DCANAL
CI$$INSERT DCFLIP
C
C This parses CLIST(FIRST:LAST) to set submatrix...
C
C Syntax: [ROW | COLUMN  conditional
C        | *
C        | subcommand
C        | SET
C        | WRITE [filename] [//DUAL]
C        | CLEAR]
C
C      Default filename for WRITE is problem name (PRBNAM)
C        ...complete file name is filename.MAT
C      Formats are:   MPS (default), SMMS
C
      CHARACTER*128 CLIST
C LOCAL
      CHARACTER*64  FILNAM
      CHARACTER*16  RNAME,CNAME
      CHARACTER*4   ROWCOL
      CHARACTER*1   CHAR
      LOGICAL*1     SW
CITSO      LOGICAL       EXIST
C
      CHARACTER*8   OPTION(9),FORMAT
      LOGICAL*1     SUBCOM
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
C SAVE FIRST FOR POSSIBLE ERROR MESSAGE
      F = FIRST
C GET SUBMAT OPTION
      OPTION(1) = 'ROW    '
      OPTION(2) = 'COLUMN '
      OPTION(3) = '*      '
      OPTION(4) = 'WRITE  '
      OPTION(5) = 'ADD    '
      OPTION(6) = 'DELETE '
      OPTION(7) = 'CLEAR  '
      OPTION(8) = 'SAVE   '
      OPTION(9) = 'SET    '
      NUMBER = 9
      RCODE = 0
      SUBCOM = .TRUE.
      CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
      IF(RCODE.NE.0)RETURN
C
      IF(NUMBER.EQ.0)GOTO 8999
C
      GOTO(8010,8010,8020,8060,8100,8100,8990,8100,8900),NUMBER
8010  CONTINUE
C ROW | COLUMN
C ...DIRECT SETTING...USE ADDRIM WITH OUTPUT SUPPRESSED
       FIRST = F
       CALL ADDRIM(CLIST,FIRST,LAST,0,RCODE)
       RETURN
8020  CONTINUE
C *
C ...SET SUBMATRIX = ENTIRE MATRIX
       FIRST= 2
       LAST = 6
       CLIST = ' ROW *'
       CALL ADDRIM(CLIST,FIRST,LAST,0,RCODE)
       FIRST = 2
       CLIST = ' COL *'
       CALL ADDRIM(CLIST,FIRST,LAST,0,RCODE)
       IF(RCODE.NE.0)RETURN
       GOTO 9000
C
8060  CONTINUE
C WRITE [filename] [//{PRIMAL | DUAL}]
      IF( NAMELN.GT.10 .AND. .NOT.SWRULE )THEN
         PRINT *,' Warning:  This will be non-standard since',
     1           ' name length > 10'
         LINE = SCRLEN+1
         CALL FPRMPT(LINE,*1390)
      ENDIF
      SW = SWMSG .AND. .NOT.SWRULE
C ...SEE IF OPTION SPECIFIED
      CALL FLOOK(CLIST,FIRST,LAST,'//',I)
      IF( I.GT.0 )THEN
         F = I+2
         OPTION(1) = 'PRIMAL'
         OPTION(2) = 'DUAL  '
         NUMBER = 2
         RCODE = -1
         CALL FOPTN(CLIST,F,LAST,OPTION,NUMBER,RCODE)
         IF( RCODE.NE.0 )RETURN
         IF( NUMBER.EQ.2 )FORMAT = 'DUAL'
         LAST = I-1
      ELSE
         FORMAT = 'PRIMAL'
      ENDIF
C ...SET filename
      CALL FTOKEN(CLIST,FIRST,LAST,FILNAM,NAMLEN,CHAR)
      IF( FILNAM.EQ.' ' .OR. FILNAM.EQ.'*' )FILNAM = PRBNAM
C ...GET FULLY QUALIFIED NAME
      CALL FSETNM('MATRIX',FILNAM,BEGEXT,RCODE)
      IF( RCODE.NE.0 )RETURN
      CALL FILOUT(FILNAM,RCODE)
      IF( RCODE.NE.0 )RETURN
C
C OK, WRITE (SUB-)MATRIX FILE
      CALL FLOPEN(DATFIL,FILNAM,'FORMATTED','UNKNOWN',*1390)
      IF( FORMAT.EQ.'PRIMAL' )THEN
         CALL GWRSUB(SW,DATFIL,RCODE)
      ELSE
         CALL GWRDUL(SW,DATFIL,PMXROW,ROWLST,VALLST,RCODE)
      ENDIF
      IF( SW .AND. RCODE.EQ.0 )THEN
         CALL FSLEN(FORMAT,8,L)
         CLIST = 'SUBMATRIX '//FORMAT(:L)//' WRITTEN TO '//FILNAM
         CALL FSLEN(CLIST,100,L)
         PRINT *,' ',CLIST(:L)
      ENDIF
      RETURN
C
8100  CONTINUE
C ADD | DELETE | SAVE (don't enter Sub-Command level)
        SUBCOM = .FALSE.
        NUMBER = NUMBER-4
C ...NOW NUMBER=1 IF ADD, NUMBER=2 IF DELETE AND NUMBER=4 IF SAVE
        IF(NUMBER.LT.4)THEN
           GOTO 50
        ELSE
           GOTO 1000
        ENDIF
C
C SET
8900    SUBCOM = .TRUE.
        GOTO 9000
C :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
8990   CONTINUE
C CLEAR
        CALL GMPCLR
        CALL ASETSB('ROW ')
        CALL ASETSB('COL ')
C DO NOT ENTER SUB-COMMAND LEVEL IF INPUT IS NOT TERMINAL OR IF
C A RULE FILE IS ACTIVE.
        SUBCOM = SWRULE .OR. INPUT.NE.TTYIN
        IF(SUBCOM)GOTO 9000
C            :....AT 9000 THIS WILL CAUSE SUBMATRIX TO BE SET
C                 (IE, REMOVAL OF NULL ROWS AND COLUMNS)
        SUBCOM = .TRUE.
C              :...THIS IS TRUE MEANING OF SUBCOM
C               ...WE ARE AT SUB-COMMAND LEVEL
C ...NOW ENTER SUBMAT COMMAND LEVEL
8999  CONTINUE
C :::::::::::::::::: HERE IS SUBMAT COMMAND LEVEL ::::::::::::::::::
      IF(SWMSG)THEN
         CLIST = ' '
         LAST = 1
         FIRST= 2
         FILE = OUTPUT
C USE COUNT TO GIVE ROWS, COLS, NONZEROES
         CALL ACOUNT(CLIST,FIRST,LAST,FILE,RCODE)
      ENDIF
C
C SET COMMAND OPTIONS
C
      OPTION(1)='ADD'
      OPTION(2)='DELETE'
      OPTION(3)='REVERSE'
      OPTION(4)='SAVE'
C
      IF(INPUT.NE.TTYIN)GOTO 20
      GOTO 10
5        PRINT *,' SUBMAT subcommands:'
         PRINT *,'    For delineation:'
         PRINT *,' ADD | DELETE | REVERSE  {ROW|COL} [conditional]'
         PRINT *,'    For later EXECUTE:  SAVE [filespec]'
         PRINT *,' ...Enter RETURN to return to main command level'
10    CONTINUE
      IF(.NOT.SUBCOM)GOTO 9000
      PRINT *,' SUBMAT (enter ? for menu)...'
C
C GET SUBCOMMAND OPTION
C
20    CONTINUE
      RCODE = 0
21    READ(INPUT,25,END=9000)CLIST
25    FORMAT(A128)
      IF(CLIST(1:1).EQ.'*')GOTO 21
      IF(CLIST.EQ.'?')GOTO 5
C
      FIRST = 1
      CALL FSLEN(CLIST,128,LAST)
C PARSE OPTION
      NUMBER=4
      RCODE = 0
      CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
      IF(RCODE.NE.0)THEN
           PRINT *,' RE-ENTER'
           GOTO 5
      ENDIF
      IF(NUMBER.EQ.0)GOTO 9000
C
      IF(NUMBER.EQ.4)GOTO 1000
C
50    CONTINUE
C ADD | DELETE | REVERSE
C
C    GET ROW|COLUMN
           RCODE = -1
           CALL AGETRC(CLIST,FIRST,LAST,ROWCOL,RCODE)
           IF(RCODE.NE.0)THEN
             IF(INPUT.NE.TTYIN)THEN
                CLOSE(INPUT)
                INPUT = TTYIN
                IF(SWMSG)PRINT *,' ...Back to terminal input'
             ENDIF
             GOTO 10
           ENDIF
C
C    PARSE CONDITIONAL
          CALL ACONDN(CLIST,FIRST,LAST,ROWCOL,RNAME,RCNUM,RCODE)
          IF(RCODE.NE.0)GOTO 10
C    ...RCNUM=FIRST QUALIFIED NUMBER
C
          IF(RCNUM.EQ.0)GOTO 10
C
C    SET SW FOR SUBCOMMAND
          IF(NUMBER.EQ.1)THEN
C    ADD
               SW=.TRUE.
          ELSE IF(NUMBER.EQ.2)THEN
C    DELETE
               SW=.FALSE.
C    REVERSE...MUST CALL GETMAP IN LOOP OVER QUALIFIED MEMBERS
          ENDIF
C
C    ::: LOOP TO SET QUALIFIED MEMBERS :::
C
           RCNUM = RCNUM - 1
100      CONTINUE
C    FIND (NEXT) QUALIFIED ROW/COLUMN
C
           CALL ANXTRC(ROWCOL,RCNUM,RNAME)
           IF(RCNUM.EQ.0)GOTO 10
C
           IF(NUMBER.EQ.3)THEN
C    ...REVERSE NEEDS TO
             CALL GETMAP(ROWCOL,RCNUM,SW)
             SW = .NOT.SW
           ENDIF
C
           CALL GPUTMP(ROWCOL,RCNUM,SW)
         GOTO 100
C
C
C  SAVE...GET filename (optional)
C
1000   CALL FTOKEN(CLIST,FIRST,LAST,FILNAM,NAMLEN,CHAR)
       IF(FILNAM.EQ.' ')FILNAM=PRBNAM
C  ...ADD .SUB EXTENSION
       CALL FSETNM('SUBMAT  ',FILNAM,BEGEXT,RCODE)
       IF(RCODE.NE.0)RETURN
C
CXTSO       CALL FINQUR(FILNAM,IOSTAT,EXIST,*1313)
CXTSO       IF(.NOT.EXIST)GOTO 1313
CIEIA       CALL FINQUR(FILNAM,IOSTAT,EXIST,*1313)
CIEIA       IF(.NOT.EXIST)GOTO 1313
C
       CALL FLOPEN(DATFIL,FILNAM,'FORMATTED','UNKNOWN',*1390)
C
C SAVE AS EXECUTABLE FILE
      WRITE(DATFIL,1001,ERR=1313)PRBNAM,FILNAM
1001  FORMAT('$    SUBMATRIX OF ',A16,' FROM ',A64/
     1       '$ This will MERGE with your current submatrix'/
     2       '_PAUSE'/'* ROWS')
C
      CNAME = 'ROW'
C   ::: LOOP OVER ROWS :::
      DO 1050 NUMBER=1,NROWS
          CALL GETMAP('ROW ',NUMBER,SW)
          IF(.NOT.SW)GOTO 1050
          CALL GETNAM('ROW ',NUMBER,RNAME)
          WRITE(DATFIL,1010,ERR=1313)CNAME,RNAME
1010      FORMAT(' SUBMAT ADD ',A4,A16)
1050  CONTINUE
C    ::: END LOOP OVER ROWS :::
C
      WRITE(DATFIL,1051,ERR=1313)
1051  FORMAT('* COLUMNS')
C
      CNAME = 'COL'
C    ::: LOOP OVER COLUMNS :::
      DO 1080 NUMBER=1,NCOLS
          CALL GETMAP('COL ',NUMBER,SW)
          IF(.NOT.SW)GOTO 1080
          CALL GETNAM('COL ',NUMBER,RNAME)
          WRITE(DATFIL,1010,ERR=1313)CNAME,RNAME
1080  CONTINUE
C    ::: END LOOP OVER COLUMNS :::
C
      WRITE(DATFIL,1090,ERR=1313)
1090  FORMAT('*'/' RETURN QUIETLY')
C
C
      CLOSE(DATFIL)
      IF(SWMSG)THEN
         CALL FSLEN(FILNAM,64,L)
         PRINT *,' To load submatrix use EXECUTE '//FILNAM(:L)
      ENDIF
      GOTO 10
C
C END SUBMAT
C
9000  CONTINUE
C    FOR CMS WE MUST CLOSE TERMINAL INPUT FILE ON EOF
      CALL ASETSB('ROW ')
      CALL ASETSB('COL ')
      IF(SUBCOM)
C CLEAR NULL ROWS AND COLUMNS
     1   CALL GMAPNZ(NRCSUB(1),RCSUB1(1),NRCSUB(2),RCSUB1(2),NZSUB)
C
      RETURN
C
1313  CONTINUE
      PRINT *,' ** IO ERROR ON '//FILNAM
1390  CLOSE(DATFIL)
      RCODE = 13
      GOTO 9000
C
C ** ASBSET ENDS HERE
      END
      SUBROUTINE AGETRC(CLIST,FIRST,LAST,ROWCOL,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This sets ROWCOL from CLIST(FIRST:LAST)
C
C NOTE:  RCODE NE 0 UPON ENTRANCE MAKES SPEC MANDATORY (AS IN FOPTN)
C
      CHARACTER*128 CLIST
      CHARACTER*4   ROWCOL
C LOCAL
      CHARACTER*8   OPTION(2)
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      OPTION(1) = 'ROW '
      OPTION(2) = 'COLUMN'
      NUMBER = 2
      CALL FOPTN(CLIST,FIRST,LAST,OPTION,NUMBER,RCODE)
      IF(RCODE.NE.0)RETURN
      IF(NUMBER-1)10,20,30
10    ROWCOL = ' '
      RETURN
20    ROWCOL = 'ROW '
      SWDISP = .FALSE.
      RETURN
30    ROWCOL = 'COL '
      SWDISP = .TRUE.
      RETURN
C
C ** AGETRC ENDS HERE
      END
      SUBROUTINE ACONDN(CLIST,FIRST,LAST,ROWCOL,CNAME,RCNUM,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This parses conditonal from CLIST(FIRST:LAST)
C
C    CNAME = name_mask
C     (results are in COMMON arrays)
C
C    RCODE = 0 means all is well (else, syntax error)
C
      CHARACTER*128 CLIST
      CHARACTER*4   ROWCOL
      CHARACTER*16  CNAME
C LOCAL
      CHARACTER*16  RNAME
      CHARACTER*1   CHAR
      LOGICAL*1     SW
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
C FIRST GET NAME
      CALL FTOKEN(CLIST,FIRST,LAST,CNAME,16,CHAR)
      IF(CNAME.EQ.' ')RETURN
C    (No conditional means leave submatrix alone)
C
C GET FIRST NAME MATCH
C
      CALL GETFNM(ROWCOL,CNAME,RCNUM)
      IF(RCNUM.EQ.0)THEN
         IF( .NOT.SWRULE)PRINT *,' NO ',ROWCOL,'MATCHES ',CNAME
           RETURN
C          (RCNUM = 0 means no match, so conditional is irrelevant)
      ENDIF
C
C PARSE conditional
C
      IF(ROWCOL.EQ.'ROW ')THEN
           K=1
      ELSE IF(ROWCOL.EQ.'COL ')THEN
           K=2
      ELSE
           PRINT *,' ** SYSERR ACNDN',ROWCOL,'...PLEASE REPORT'
           RCODE = 13
           RETURN
      ENDIF
C
C FIRST, INITIALIZE VALUE RANGES
      DO 100 I=1,NKEYS
           VLO(I)=-VINF
           VUP(I)= VINF
100   CONTINUE
C
C INITIALIZE NONZERO LENGTH RANGE
C
      NZLO = 0
      NZUP = NROWS + NCOLS
C INITIALIZE RELATIONAL OPERATORS
      NOPKEY = 0
C
C INITIALIZE STATUS MAPS
      DO 120 I=1,4
120   SOLMAP(I) = .TRUE.
C
      IF(CHAR.EQ.',')RETURN
C        (Parse stops at comma)
C
150   CONTINUE
C
C GET RELATION(S):
C   [STATUS {LUBI}] [rimkey op rimkey] [rimkey=value[/value] [NONZERO=v/[v]]
C
C FIRST OPERAND IS STATUS, NONZERO, or some rimkey
C
      CALL FTOKEN(CLIST,FIRST,LAST,RNAME,8,CHAR)
      IF(RNAME.EQ.' ')RETURN
C
      CALL FMATCH(RNAME,'STATUS',' ',SW)
      IF(SW)THEN
C STATUS SPECIFIED...FIRST CLEAR SOLMAP
C          STATUS = {LUBI}  (at least one)
           DO 200 I=1,4
200        SOLMAP(I)=.FALSE.
C
           CALL FTOKEN(CLIST,FIRST,LAST,RNAME,8,CHAR)
C
           DO 220 I=1,4
             IF(RNAME(I:I).EQ.'L')THEN
                             SOLMAP(1)=.TRUE.
             ELSE IF(RNAME(I:I).EQ.'U')THEN
                             SOLMAP(2)=.TRUE.
             ELSE IF(RNAME(I:I).EQ.'B')THEN
                             SOLMAP(3)=.TRUE.
             ELSE IF(RNAME(I:I).EQ.'I')THEN
                             SOLMAP(4)=.TRUE.
             ELSE IF(RNAME(I:I).NE.' ')THEN
                 PRINT *,' ** Status ',RNAME(I:I),' not recognized'
                 RCODE = -1
                 RETURN
             ENDIF
220        CONTINUE
C
           GOTO 900
      ENDIF
C
C Since RNAME ne STATUS, RNAME = rimkey | NONZERO (nonzero length)
C
      CALL FMATCH(RNAME,'NONZERO',' ',SW)
      IF(SW)THEN
C  ...NONZERO = v[/v]...GET VALUE RANGE
           VINFI = 32000.
           CALL FVRNG(CLIST,FIRST,LAST,VL,VU,VINFI,CHAR,RCODE)
           IF(RCODE.NE.0)RETURN
             NZLO = VL + .1
             NZUP = VU + .1
           GOTO 900
      ENDIF
C
C Now RNAME must be rimkey (rimkey op rimkey | rimkey = v[/v])
C
           SW = SWDISP
           SWDISP = ROWCOL.EQ.'COL '
C  TSO CANNOT USE ASSIGNMENT FOR LOGICAL*1 (VSFORTRAN BUG)
CXTSO           IF(ROWCOL.EQ.'ROW ')THEN
CXTSO              SWDISP = .FALSE.
CXTSO           ELSE IF(ROWCOL.EQ.'COL ')THEN
CXTSO              SWDISP = .TRUE.
CXTSO           ELSE
CXTSO              PRINT *,' ** SYSERR ACONDN ',ROWCOL,'...PLEASE REPORT'
CXTSO              RCODE = 13
CXTSO              RETURN
CXTSO           ENDIF
      CALL ARIMKY(RNAME,NUMBER,RCODE)
           SWDISP = SW
      IF(RCODE.NE.0)RETURN
C  ...NUMBER IDENTIFIES RIMKEY OF RNAME
      IF(CHAR.EQ.'=')THEN
C     VALUE RANGE SYNTAX:  rimkey = v[/v]
C
         CALL FVRNG(CLIST,FIRST,LAST,VLO(NUMBER),VUP(NUMBER),VINF,
     1              CHAR,RCODE)
         IF(RCODE.NE.0)RETURN
C THIS CONTINUE IS FOR VS FORTRAN
CICMS         CONTINUE
      ELSE
C    OP SYNTAX:  rimkey op rimkey
C
           IF(NOPKEY.EQ.MAXOPS)THEN
             PRINT *,' Sorry, too many conditions (limit =',MAXOPS,')'
             RCODE = -1
             RETURN
           ENDIF
C
           NOPKEY = NOPKEY + 1
           KEY1(NOPKEY) = RIMKEY(K,NUMBER)
C
C    GET OPERATOR
C
           NUMBER = NOPLST
           RCODE = -1
           CALL FOPTN(CLIST,FIRST,LAST,OPLIST,NUMBER,RCODE)
           IF(RCODE.NE.0)RETURN
C
           OPKEY(NOPKEY) = OPLIST(NUMBER)(:2)
C
C    GET KEY2
C
           CALL FTOKEN(CLIST,FIRST,LAST,RNAME,16,CHAR)
           CALL ARIMKY(RNAME,NUMBER,RCODE)
           IF(RCODE.NE.0)RETURN
           KEY2(NOPKEY) = RIMKEY(K,NUMBER)
C
      ENDIF
C
C MORE?
C
900   IF(CHAR.EQ.',')RETURN
C -MAYBE
      GOTO 150
C
C ** ACIBDB ENDS HERE
      END
      SUBROUTINE ARIMKY(RNAME,NUMBER,RCODE)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This gets rimkey NUMBER from RNAME
C
      CHARACTER*16  RNAME
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF(RNAME(2:).NE.' ')GOTO 13
      IF(SWDISP)THEN
           K=2
      ELSE
           K=1
      ENDIF
C
      DO 10 NUMBER=1,NKEYS
           IF(RIMKEY(K,NUMBER).EQ.RNAME(1:1))RETURN
10    CONTINUE
C
13    RCODE = -1
      PRINT *,' Rimkey expected...',
     1    (RIMKEY(K,NUMBER),' ',NUMBER=1,NKEYS)
C
      RETURN
C ** ARIMKY ENDS HERE
      END
      SUBROUTINE ANXTRC(ROWCOL,NUMBER,RNAME)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This gets next row/column after NUMBER that qualifies (conditions are
C in COMMON).
C
C ...NUMBER = 0 if no (more) qualify
C
      CHARACTER*4   ROWCOL
      CHARACTER*16  RNAME
C LOCAL
      CHARACTER*16  CNAME
      CHARACTER*1   STAT
      LOGICAL*1     SW
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      FIRST = NUMBER + 1
      IF(ROWCOL.EQ.'ROW ')THEN
           LAST  = NROWS
      ELSE
           LAST  = NCOLS
      ENDIF
      IF(FIRST.GT.LAST)GO TO 900
C
C LOOP TO TEST QUALIFICATIONS
C
      DO 500 NUMBER=FIRST,LAST
C    TEST NAME
           CALL GETNAM(ROWCOL,NUMBER,CNAME)
           CALL FMATCH(RNAME,CNAME,'*',SW)
           IF(.NOT.SW)GOTO 500
C
C    NAME MATCHES...TEST RIM
           CALL GETRIM(ROWCOL,NUMBER,VL,VU,VC,NZ)
           IF(  VL.LT.VLO(1)-VTOLAB .OR. VL.GT.VUP(1)+VTOLAB
     1     .OR. VU.LT.VLO(2)-VTOLAB .OR. VU.GT.VUP(2)+VTOLAB
     2     .OR. VC.LT.VLO(3)-VTOLAB .OR. VC.GT.VUP(3)+VTOLAB
     3     .OR. NZ.LT.NZLO.OR.NZ.GT.NZUP)GOTO 500
C
C     RIM WITHIN RANGES...TEST SOLUTION
           CALL GETSOL(ROWCOL,NUMBER,VX,VP,STAT,STNUM)
           IF(  VX.LT.VLO(4)-VTOLAB.OR.VX.GT.VUP(4)+VTOLAB
     1     .OR. VP.LT.VLO(5)-VTOLAB.OR.VP.GT.VUP(5)+VTOLAB
     2     .OR.  .NOT.SOLMAP(STNUM))GOTO 500
C
C    SOLUTION WITHIN RANGES...TEST RELATIONAL CONDITIONS
C
           IF(NOPKEY.EQ.0)RETURN
C    UNLIKE RANGE TESTS, RELATIONALS ARE EXACT (IE, NO TOLERANCE)
C    BECAUSE TYPICALLY THEY ARE SET BY STATUS RATHER THAN COMPUTED
C    ...IT IS POSSIBLE WE SHALL CHANGE THIS
C
           DO 400 K=1,NOPKEY
C
             CALL ASETKY(KEY1(K),VL,VU,VC,VX,VP,V1)
             CALL ASETKY(KEY2(K),VL,VU,VC,VX,VP,V2)
             IF(OPKEY(K).EQ.'LT')THEN
               IF(V1.GE.V2)GOTO 500
             ELSE IF(OPKEY(K).EQ.'LE')THEN
               IF(V1.GT.V2)GOTO 500
             ELSE IF(OPKEY(K).EQ.'EQ')THEN
               IF(V1.GT.V2.OR.V1.LT.V2)GOTO 500
             ELSE IF(OPKEY(K).EQ.'NE')THEN
               IF(V1.LE.V2.AND.V1.GE.V2)GOTO 500
             ELSE IF(OPKEY(K).EQ.'GT')THEN
               IF(V1.LE.V2)GOTO 500
             ELSE
               IF(V1.LT.V2)GOTO 500
             ENDIF
400        CONTINUE
C
C ALL TESTS PASS...ROW/COLUMN QUALIFIES
C
           RETURN
C
500   CONTINUE
C
C NO (MORE) QUALIFIED
900   NUMBER = 0
C
      RETURN
C ** ANXTRC ENDS HERE
      END
      SUBROUTINE ASBNEW(ROWCOL,RNAME,FIRST)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This creates new row/column (ROWCOL) submatrix (used for inline spec)
C
C ...RNAME = name_mask ...FIRST = first name match
C (conditions are in common...processed by ANXTRC)
C
      CHARACTER*4   ROWCOL
      CHARACTER*16  RNAME
C LOCAL
      LOGICAL*1     SW
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF(ROWCOL.EQ.'ROW ')THEN
           N = NROWS
           RCSUB1(1) = FIRST
      ELSE
           N = NCOLS
           RCSUB1(2) = FIRST
      ENDIF
C
C  CLEAR MAPS
C
      SW = .FALSE.
      DO 10 I=1,N
10    CALL GPUTMP(ROWCOL,I,SW)
C
C NOW PREPARE TO SET QUALIFIED MEMBERS
      SW = .TRUE.
C
C    ::: LOOP OVER QUALIFIED MEMBERS :::
C
      NUMBER = FIRST - 1
100   CONTINUE
           CALL ANXTRC(ROWCOL,NUMBER,RNAME)
           IF(NUMBER.EQ.0)THEN
                 CALL ASETSB(ROWCOL)
                 RETURN
           ELSE
                 CALL GPUTMP(ROWCOL,NUMBER,SW)
                 GOTO 100
           ENDIF
C
C ** ASBNEW ENDS HERE
      END
      SUBROUTINE ASETSB(ROWCOL)
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
      INCLUDE 'DCRULE.'
CITSO      INCLUDE (DCANAL)
CITSO      INCLUDE (DCRULE)
CI$$INSERT DCANAL
CI$$INSERT DCRULE
C
C This sets submatrix statistics for row/column
C
      CHARACTER*4   ROWCOL
C LOCAL
      CHARACTER*1   STAT
      LOGICAL*1     SW
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      IF(ROWCOL.EQ.'ROW ')THEN
           K = 1
           NUMBER = NROWS
      ELSE IF(ROWCOL.EQ.'COL ')THEN
           K = 2
           NUMBER = NCOLS
      ENDIF
C
C INITIALIZE STATS
C
      NRCSUB(K) = 0
      NZSUB   = 0
      SWSBNZ = .FALSE.
C
C   This switch says that row or column submatrix has changed, so
C   the nonzero count (NZSUB) is not valid for submatrix.  This
C   becomes valid upon calling GMAPNZ, which ACOUNT does.  The value
C   of NZSUB computed here is just for the rows over all columns (or
C   columns over all rows).
C
      DO 50 I=1,NKEYS
           VSUMSB(K,I) = 0.0
           VAVGSB(K,I) = 0.0
           VMINSB(K,I) =  VINF
           VMAXSB(K,I) = -VINF
50    CONTINUE
C
      DO 500 I=NUMBER,1,-1
           CALL GETMAP(ROWCOL,I,SW)
           IF(.NOT.SW)GOTO 500
C
           RCSUB1(K) = I
           NRCSUB(K) = NRCSUB(K) + 1
           CALL GETRIM(ROWCOL,I,VL,VU,VC,NZ)
           CALL GETSOL(ROWCOL,I,VX,VP,STAT,STNUM)
           NZSUB = NZSUB + NZ
C
C      UPDATE VALUES
C
           IF(VL.GT.-VINF)THEN
              VSUMSB(K,1) = VSUMSB(K,1) + VL
              IF(VL.LT.VMINSB(K,1))VMINSB(K,1)=VL
              IF(VL.GT.VMAXSB(K,1))VMAXSB(K,1)=VL
           ENDIF
C
           IF(VU.LT. VINF)THEN
              VSUMSB(K,2) = VSUMSB(K,2) + VU
              IF(VU.LT.VMINSB(K,2))VMINSB(K,2)=VU
              IF(VU.GT.VMAXSB(K,2))VMAXSB(K,2)=VU
           ENDIF
C
           VSUMSB(K,3) = VSUMSB(K,3) + VC
           IF(VC.LT.VMINSB(K,3))VMINSB(K,3)=VC
           IF(VC.GT.VMAXSB(K,3))VMAXSB(K,3)=VC
C
           VSUMSB(K,4) = VSUMSB(K,4) + VX
           IF(VX.LT.VMINSB(K,4))VMINSB(K,4)=VX
           IF(VX.GT.VMAXSB(K,4))VMAXSB(K,4)=VX
C
           VSUMSB(K,5) = VSUMSB(K,5) + VP
           IF(VP.LT.VMINSB(K,5))VMINSB(K,5)=VP
           IF(VP.GT.VMAXSB(K,5))VMAXSB(K,5)=VP
C
500   CONTINUE
      IF(NRCSUB(K).EQ.0)THEN
         VN = 1.
      ELSE
         VN = NRCSUB(K)
      ENDIF
C
C    SET AVERAGES
C
      DO 600 I=1,NKEYS
600   VAVGSB(K,I) = VSUMSB(K,I)/VN
C
      IF(SWRULE)THEN
         IF(ROWCOL.EQ.'ROW ')RULROW = RCSUB1(1)
         IF(ROWCOL.EQ.'COL ')RULCOL = RCSUB1(2)
      ENDIF
C
      RETURN
C
C ** ASETSB ENDS HERE
      END
      SUBROUTINE ASBDBG
C     =================
      IMPLICIT INTEGER (A-U), DOUBLE PRECISION (Z)
C
      INCLUDE 'DCANAL.'
CITSO      INCLUDE (DCANAL)
CI$$INSERT DCANAL
C
C This is Submat Debug (entered from SYSDBG)
C :::::::::::::::::::::::::: BEGIN ::::::::::::::::::::::::::::::::
      PRINT *,' SUBMAT DEBUG...NKEYS=',NKEYS,' ...VLO,VUP...'
      DO 10 K=1,NKEYS
10    PRINT *,VLO(K),VUP(K)
C
      PRINT *,' RCSUB1=',RCSUB1(1),RCSUB1(2)
      PRINT *,' NRCSUB=',NRCSUB(1),NRCSUB(2)
      PRINT *,' NZSUB=',NZSUB,' NZLO=',NZLO,' NZUP=',NZUP
      PRINT *,' SWSBNZ=',SWSBNZ,' NOPLST=',NOPLST
      LINE = NKEYS + 5
C
      IF(NOPLST.GT.0)THEN
         PRINT *,' OPLIST: ',(OPLIST(N),N=1,NOPLST)
         LINE = LINE+1
      ENDIF
      PRINT *,' NOPKEY=',NOPKEY
      LINE = LINE+1
      IF(NOPKEY.GT.0)THEN
         DO 20 N=1,NOPKEY
            CALL FPRMPT(LINE,*900)
            PRINT *,N,': ',KEY1(N),' ',OPKEY(N),' ',KEY2(N)
20       CONTINUE
      ENDIF
      CALL FPRMPT(LINE,*900)
      PRINT *,' SOLMAP=',(SOLMAP(I),I=1,4)
C
900   RETURN
C
C ** ASBDBG ENDS HERE
      END
